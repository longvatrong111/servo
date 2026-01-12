/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Layout construction code that is shared between modern layout modes (Flexbox and CSS Grid)

use std::borrow::Cow;
use std::sync::LazyLock;

use rayon::iter::{IntoParallelIterator, ParallelIterator};
use servo_arc::Arc as ServoArc;
use style::selector_parser::PseudoElement;

use crate::PropagatedBoxTreeData;
use crate::context::LayoutContext;
use crate::dom::{BoxSlot, LayoutBox};
use crate::dom_traversal::{Contents, NodeAndStyleInfo, TraversalHandler};
use crate::flow::inline::SharedInlineStyles;
use crate::flow::inline::construct::InlineFormattingContextBuilder;
use crate::flow::{BlockContainer, BlockFormattingContext};
use crate::formatting_contexts::{
    IndependentFormattingContext, IndependentFormattingContextContents,
};
use crate::layout_box_base::LayoutBoxBase;
use crate::style_ext::{ComputedValuesExt, DisplayGeneratingBox};

/// A builder used for both flex and grid containers.
pub(crate) struct ModernContainerBuilder<'a, 'dom> {
    context: &'a LayoutContext<'a>,
    info: &'a NodeAndStyleInfo<'dom>,
    propagated_data: PropagatedBoxTreeData,
    contiguous_text_runs: Vec<ModernContainerTextRun<'dom>>,
    /// To be run in parallel with rayon in `finish`
    jobs: Vec<ModernContainerJob<'dom>>,
    has_text_runs: bool,

    /// A stack of `display: contents` styles currently in scope.
    ///
    /// This matters for modern layout modes (flex/grid) because `display: contents` elements do
    /// not generate boxes but still provide styling for their children. `<slot>` behaves like
    /// `display: contents` for box tree construction, so without this, slotted text can inherit
    /// from the wrong ancestor in flex/grid containers.
    display_contents_shared_styles: Vec<SharedInlineStyles>,
}

enum ModernContainerJob<'dom> {
    ElementOrPseudoElement {
        info: NodeAndStyleInfo<'dom>,
        display: DisplayGeneratingBox,
        contents: Contents,
        box_slot: BoxSlot<'dom>,
    },
    TextRuns(Vec<ModernContainerTextRun<'dom>>),
}

impl<'dom> ModernContainerJob<'dom> {
    fn finish(
        self,
        builder: &ModernContainerBuilder,
        anonymous_info: &LazyLock<NodeAndStyleInfo<'dom>, impl FnOnce() -> NodeAndStyleInfo<'dom>>,
    ) -> Option<ModernItem<'dom>> {
        match self {
            ModernContainerJob::TextRuns(runs) => {
                fn shared_inline_styles_ptr_eq(
                    lhs: &SharedInlineStyles,
                    rhs: &SharedInlineStyles,
                ) -> bool {
                    let lhs_style = lhs.style.borrow();
                    let rhs_style = rhs.style.borrow();
                    let lhs_selected = lhs.selected.borrow();
                    let rhs_selected = rhs.selected.borrow();
                    ServoArc::ptr_eq(&*lhs_style, &*rhs_style) &&
                        ServoArc::ptr_eq(&*lhs_selected, &*rhs_selected)
                }

                let mut inline_formatting_context_builder =
                    InlineFormattingContextBuilder::new(builder.info);
                let mut active_display_contents: Vec<SharedInlineStyles> = Vec::new();

                for flex_text_run in runs.into_iter() {
                    let desired = &flex_text_run.display_contents_shared_styles;
                    let common_prefix_len = active_display_contents
                        .iter()
                        .zip(desired.iter())
                        .take_while(|(lhs, rhs)| shared_inline_styles_ptr_eq(lhs, rhs))
                        .count();

                    while active_display_contents.len() > common_prefix_len {
                        inline_formatting_context_builder.leave_display_contents();
                        active_display_contents.pop();
                    }

                    for styles in desired.iter().skip(common_prefix_len).cloned() {
                        inline_formatting_context_builder.enter_display_contents(styles.clone());
                        active_display_contents.push(styles);
                    }

                    inline_formatting_context_builder
                        .push_text(flex_text_run.text, &flex_text_run.info);
                }

                let inline_formatting_context = inline_formatting_context_builder.finish(
                    builder.context,
                    true,  /* has_first_formatted_line */
                    false, /* is_single_line_text_box */
                    builder.info.style.to_bidi_level(),
                )?;

                let block_formatting_context = BlockFormattingContext::from_block_container(
                    BlockContainer::InlineFormattingContext(inline_formatting_context),
                );
                let info: &NodeAndStyleInfo = anonymous_info;
                let formatting_context = IndependentFormattingContext::new(
                    LayoutBoxBase::new(info.into(), info.style.clone()),
                    IndependentFormattingContextContents::Flow(block_formatting_context),
                );

                Some(ModernItem {
                    kind: ModernItemKind::InFlow(formatting_context),
                    order: 0,
                    box_slot: None,
                })
            },
            ModernContainerJob::ElementOrPseudoElement {
                info,
                display,
                contents,
                box_slot,
            } => {
                let is_abspos = info.style.get_box().position.is_absolutely_positioned();
                let order = if is_abspos {
                    0
                } else {
                    info.style.clone_order()
                };

                if let Some(layout_box) = box_slot
                    .take_layout_box_if_undamaged(info.damage)
                    .and_then(|layout_box| match &layout_box {
                        LayoutBox::FlexLevel(_) | LayoutBox::TaffyItemBox(_) => Some(layout_box),
                        _ => None,
                    })
                {
                    return Some(ModernItem {
                        kind: ModernItemKind::ReusedBox(layout_box),
                        order,
                        box_slot: Some(box_slot),
                    });
                }

                // Text decorations are not propagated to any out-of-flow descendants. In addition,
                // absolutes don't affect the size of ancestors so it is fine to allow descendent
                // tables to resolve percentage columns.
                let propagated_data = match is_abspos {
                    false => builder.propagated_data,
                    true => PropagatedBoxTreeData::default(),
                };

                let formatting_context = IndependentFormattingContext::construct(
                    builder.context,
                    &info,
                    display.display_inside(),
                    contents,
                    propagated_data,
                );

                let kind = if is_abspos {
                    ModernItemKind::OutOfFlow(formatting_context)
                } else {
                    ModernItemKind::InFlow(formatting_context)
                };
                Some(ModernItem {
                    kind,
                    order,
                    box_slot: Some(box_slot),
                })
            },
        }
    }
}

struct ModernContainerTextRun<'dom> {
    info: NodeAndStyleInfo<'dom>,
    text: Cow<'dom, str>,
    display_contents_shared_styles: Vec<SharedInlineStyles>,
}

impl ModernContainerTextRun<'_> {
    /// <https://drafts.csswg.org/css-text/#white-space>
    fn is_only_document_white_space(&self) -> bool {
        // FIXME: is this the right definition? See
        // https://github.com/w3c/csswg-drafts/issues/5146
        // https://github.com/w3c/csswg-drafts/issues/5147
        self.text
            .bytes()
            .all(|byte| matches!(byte, b' ' | b'\n' | b'\t'))
    }
}

pub(crate) enum ModernItemKind {
    InFlow(IndependentFormattingContext),
    OutOfFlow(IndependentFormattingContext),
    ReusedBox(LayoutBox),
}

pub(crate) struct ModernItem<'dom> {
    pub kind: ModernItemKind,
    pub order: i32,
    pub box_slot: Option<BoxSlot<'dom>>,
}

impl<'dom> TraversalHandler<'dom> for ModernContainerBuilder<'_, 'dom> {
    fn handle_text(&mut self, info: &NodeAndStyleInfo<'dom>, text: Cow<'dom, str>) {
        self.contiguous_text_runs.push(ModernContainerTextRun {
            info: info.clone(),
            text,
            display_contents_shared_styles: self.display_contents_shared_styles.clone(),
        })
    }

    fn enter_display_contents(&mut self, styles: SharedInlineStyles) {
        self.display_contents_shared_styles.push(styles);
    }

    fn leave_display_contents(&mut self) {
        self.display_contents_shared_styles.pop();
    }

    /// Or pseudo-element
    fn handle_element(
        &mut self,
        info: &NodeAndStyleInfo<'dom>,
        display: DisplayGeneratingBox,
        contents: Contents,
        box_slot: BoxSlot<'dom>,
    ) {
        self.wrap_any_text_in_anonymous_block_container();

        self.jobs.push(ModernContainerJob::ElementOrPseudoElement {
            info: info.clone(),
            display,
            contents,
            box_slot,
        })
    }
}

impl<'a, 'dom> ModernContainerBuilder<'a, 'dom> {
    pub fn new(
        context: &'a LayoutContext<'a>,
        info: &'a NodeAndStyleInfo<'dom>,
        propagated_data: PropagatedBoxTreeData,
    ) -> Self {
        ModernContainerBuilder {
            context,
            info,
            propagated_data: propagated_data.disallowing_percentage_table_columns(),
            contiguous_text_runs: Vec::new(),
            jobs: Vec::new(),
            has_text_runs: false,
            display_contents_shared_styles: Vec::new(),
        }
    }

    fn wrap_any_text_in_anonymous_block_container(&mut self) {
        let runs = std::mem::take(&mut self.contiguous_text_runs);
        if runs
            .iter()
            .all(ModernContainerTextRun::is_only_document_white_space)
        {
            // There is no text run, or they all only contain document white space characters
        } else {
            self.jobs.push(ModernContainerJob::TextRuns(runs));
            self.has_text_runs = true;
        }
    }

    pub(crate) fn finish(mut self) -> Vec<ModernItem<'dom>> {
        self.wrap_any_text_in_anonymous_block_container();

        let anonymous_info = LazyLock::new(|| {
            self.info
                .with_pseudo_element(self.context, PseudoElement::ServoAnonymousBox)
                .expect("Should always be able to construct info for anonymous boxes.")
        });

        let jobs = std::mem::take(&mut self.jobs);
        let mut children: Vec<_> = if self.context.use_rayon {
            jobs.into_par_iter()
                .filter_map(|job| job.finish(&self, &anonymous_info))
                .collect()
        } else {
            jobs.into_iter()
                .filter_map(|job| job.finish(&self, &anonymous_info))
                .collect()
        };

        // https://drafts.csswg.org/css-flexbox/#order-modified-document-order
        children.sort_by_key(|child| child.order);

        children
    }
}

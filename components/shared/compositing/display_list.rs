/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

//! Defines data structures which are consumed by the Compositor.

use std::cell::Cell;
use std::collections::HashMap;

use base::id::ScrollTreeNodeId;
use base::print_tree::PrintTree;
use bitflags::bitflags;
use embedder_traits::{Cursor, ViewportDetails};
use euclid::{SideOffsets2D, Transform3D};
use malloc_size_of_derive::MallocSizeOf;
use serde::{Deserialize, Serialize};
use style::values::specified::Overflow;
use webrender_api::units::{
    LayoutPixel, LayoutPoint, LayoutRect, LayoutSize, LayoutTransform, LayoutVector2D,
};
use webrender_api::{
    Epoch, ExternalScrollId, PipelineId, ReferenceFrameKind, ScrollLocation, SpatialId,
    StickyOffsetBounds, TransformStyle,
};

/// A scroll type, describing whether what kind of action originated this scroll request.
/// This is a bitflag as it is also used to track what kinds of [`ScrollType`]s scroll
/// nodes are sensitive to.
#[derive(Clone, Copy, Debug, Deserialize, MallocSizeOf, PartialEq, Serialize)]
pub struct ScrollType(u8);

bitflags! {
    impl ScrollType: u8 {
        /// This node can be scrolled by input events or an input event originated this
        /// scroll.
        const InputEvents = 1 << 0;
        /// This node can be scrolled by script events or script originated this scroll.
        const Script = 1 << 1;
    }
}

/// Convert [Overflow] to [ScrollSensitivity].
impl From<Overflow> for ScrollType {
    fn from(overflow: Overflow) -> Self {
        match overflow {
            Overflow::Hidden => ScrollType::Script,
            Overflow::Scroll | Overflow::Auto => ScrollType::Script | ScrollType::InputEvents,
            Overflow::Visible | Overflow::Clip => ScrollType::empty(),
        }
    }
}

/// The [ScrollSensitivity] of particular node in the vertical and horizontal axes.
#[derive(Clone, Copy, Debug, Deserialize, MallocSizeOf, PartialEq, Serialize)]
pub struct AxesScrollSensitivity {
    pub x: ScrollType,
    pub y: ScrollType,
}

/// Information that Servo keeps alongside WebRender display items
/// in order to add more context to hit test results.
#[derive(Debug, Deserialize, PartialEq, Serialize)]
pub struct HitTestInfo {
    /// The id of the node of this hit test item.
    pub node: u64,

    /// The cursor of this node's hit test item.
    pub cursor: Option<Cursor>,

    /// The id of the [ScrollTree] associated with this hit test item.
    pub scroll_tree_node: ScrollTreeNodeId,
}

#[derive(Debug, Deserialize, Serialize)]
pub enum SpatialTreeNodeInfo {
    ReferenceFrame(ReferenceFrameNodeInfo),
    Scroll(ScrollableNodeInfo),
    Sticky(StickyNodeInfo),
}

#[derive(Debug, Deserialize, Serialize)]
pub struct StickyNodeInfo {
    pub frame_rect: LayoutRect,
    pub margins: SideOffsets2D<Option<f32>, LayoutPixel>,
    pub vertical_offset_bounds: StickyOffsetBounds,
    pub horizontal_offset_bounds: StickyOffsetBounds,
}

impl StickyNodeInfo {
    /// Calculate the sticky offset for this [`StickyNodeInfo`] given information about
    /// sticky positioning from its ancestors.
    ///
    /// This is originally taken from WebRender `SpatialTree` implementation.
    fn calculate_sticky_offset(&self, ancestor_sticky_info: &AncestorStickyInfo) -> LayoutVector2D {
        let viewport_scroll_offset = &ancestor_sticky_info.nearest_scrolling_ancestor_offset;
        let viewport_rect = &ancestor_sticky_info.nearest_scrolling_ancestor_viewport;

        if self.margins.top.is_none() &&
            self.margins.bottom.is_none() &&
            self.margins.left.is_none() &&
            self.margins.right.is_none()
        {
            return LayoutVector2D::zero();
        }

        // The viewport and margins of the item establishes the maximum amount that it can
        // be offset in order to keep it on screen. Since we care about the relationship
        // between the scrolled content and unscrolled viewport we adjust the viewport's
        // position by the scroll offset in order to work with their relative positions on the
        // page.
        let mut sticky_rect = self.frame_rect.translate(*viewport_scroll_offset);

        let mut sticky_offset = LayoutVector2D::zero();
        if let Some(margin) = self.margins.top {
            let top_viewport_edge = viewport_rect.min.y + margin;
            if sticky_rect.min.y < top_viewport_edge {
                // If the sticky rect is positioned above the top edge of the viewport (plus margin)
                // we move it down so that it is fully inside the viewport.
                sticky_offset.y = top_viewport_edge - sticky_rect.min.y;
            }
        }

        // If we don't have a sticky-top offset (sticky_offset.y == 0) then we check for
        // handling the bottom margin case. Note that the "don't have a sticky-top offset"
        // case includes the case where we *had* a sticky-top offset but we reduced it to
        // zero in the above block.
        if sticky_offset.y <= 0.0 {
            if let Some(margin) = self.margins.bottom {
                // If sticky_offset.y is nonzero that means we must have set it
                // in the sticky-top handling code above, so this item must have
                // both top and bottom sticky margins. We adjust the item's rect
                // by the top-sticky offset, and then combine any offset from
                // the bottom-sticky calculation into sticky_offset below.
                sticky_rect.min.y += sticky_offset.y;
                sticky_rect.max.y += sticky_offset.y;

                // Same as the above case, but inverted for bottom-sticky items. Here
                // we adjust items upwards, resulting in a negative sticky_offset.y,
                // or reduce the already-present upward adjustment, resulting in a positive
                // sticky_offset.y.
                let bottom_viewport_edge = viewport_rect.max.y - margin;
                if sticky_rect.max.y > bottom_viewport_edge {
                    sticky_offset.y += bottom_viewport_edge - sticky_rect.max.y;
                }
            }
        }

        // Same as above, but for the x-axis.
        if let Some(margin) = self.margins.left {
            let left_viewport_edge = viewport_rect.min.x + margin;
            if sticky_rect.min.x < left_viewport_edge {
                sticky_offset.x = left_viewport_edge - sticky_rect.min.x;
            }
        }

        if sticky_offset.x <= 0.0 {
            if let Some(margin) = self.margins.right {
                sticky_rect.min.x += sticky_offset.x;
                sticky_rect.max.x += sticky_offset.x;
                let right_viewport_edge = viewport_rect.max.x - margin;
                if sticky_rect.max.x > right_viewport_edge {
                    sticky_offset.x += right_viewport_edge - sticky_rect.max.x;
                }
            }
        }

        // The total "sticky offset" and the extra amount we computed as a result of
        // scrolling, stored in sticky_offset needs to be clamped to the provided bounds.
        let clamp =
            |value: f32, bounds: &StickyOffsetBounds| (value).max(bounds.min).min(bounds.max);
        sticky_offset.y = clamp(sticky_offset.y, &self.vertical_offset_bounds);
        sticky_offset.x = clamp(sticky_offset.x, &self.horizontal_offset_bounds);

        sticky_offset
    }
}

#[derive(Debug, Deserialize, Serialize)]
pub struct ReferenceFrameNodeInfo {
    pub origin: LayoutPoint,
    /// Origin of this frame relative to the document for bounding box queries.
    pub frame_origin_for_query: LayoutPoint,
    pub transform_style: TransformStyle,
    pub transform: LayoutTransform,
    pub kind: ReferenceFrameKind,
}

/// Data stored for nodes in the [ScrollTree] that actually scroll,
/// as opposed to reference frames and sticky nodes which do not.
#[derive(Debug, Deserialize, Serialize)]
pub struct ScrollableNodeInfo {
    /// The external scroll id of this node, used to track
    /// it between successive re-layouts.
    pub external_id: ExternalScrollId,

    /// The content rectangle for this scroll node;
    pub content_rect: LayoutRect,

    /// The clip rectange for this scroll node.
    pub clip_rect: LayoutRect,

    /// Whether this `ScrollableNode` is sensitive to input events.
    pub scroll_sensitivity: AxesScrollSensitivity,

    /// The current offset of this scroll node.
    pub offset: LayoutVector2D,

    /// Whether or not the scroll offset of this node has changed and it needs it's
    /// cached transformations invalidated.
    pub offset_changed: Cell<bool>,
}

impl ScrollableNodeInfo {
    fn scroll_to_offset(
        &mut self,
        new_offset: LayoutVector2D,
        context: ScrollType,
    ) -> Option<LayoutVector2D> {
        if !self.scroll_sensitivity.x.contains(context) &&
            !self.scroll_sensitivity.y.contains(context)
        {
            return None;
        }

        let scrollable_size = self.scrollable_size();
        let original_layer_scroll_offset = self.offset;

        if scrollable_size.width > 0. && self.scroll_sensitivity.x.contains(context) {
            self.offset.x = new_offset.x.clamp(0.0, scrollable_size.width);
        }

        if scrollable_size.height > 0. && self.scroll_sensitivity.y.contains(context) {
            self.offset.y = new_offset.y.clamp(0.0, scrollable_size.height);
        }

        if self.offset != original_layer_scroll_offset {
            self.offset_changed.set(true);
            Some(self.offset)
        } else {
            None
        }
    }

    fn scroll_to_webrender_location(
        &mut self,
        scroll_location: ScrollLocation,
        context: ScrollType,
    ) -> Option<LayoutVector2D> {
        if !self.scroll_sensitivity.x.contains(context) &&
            !self.scroll_sensitivity.y.contains(context)
        {
            return None;
        }

        let delta = match scroll_location {
            ScrollLocation::Delta(delta) => delta,
            ScrollLocation::Start => {
                if self.offset.y.round() <= 0.0 {
                    // Nothing to do on this layer.
                    return None;
                }

                self.offset.y = 0.0;
                self.offset_changed.set(true);
                return Some(self.offset);
            },
            ScrollLocation::End => {
                let end_pos = self.scrollable_size().height;
                if self.offset.y.round() >= end_pos {
                    // Nothing to do on this layer.
                    return None;
                }

                self.offset.y = end_pos;
                self.offset_changed.set(true);
                return Some(self.offset);
            },
        };

        self.scroll_to_offset(self.offset + delta, context)
    }
}

impl ScrollableNodeInfo {
    fn scrollable_size(&self) -> LayoutSize {
        self.content_rect.size() - self.clip_rect.size()
    }
}

/// A cached of transforms of a particular [`ScrollTree`] node in both directions:
/// mapping from node-relative points to root-relative points and vice-versa.
///
/// Potential ideas for improvement:
///  - Test optimizing simple translations to avoid having to do full matrix
///    multiplication when transforms are not involved.
#[derive(Clone, Copy, Debug, Default, Deserialize, Serialize)]
pub struct ScrollTreeNodeTransformationCache {
    node_to_root_transform: LayoutTransform,
}

#[derive(Default)]
struct AncestorStickyInfo {
    nearest_scrolling_ancestor_offset: LayoutVector2D,
    nearest_scrolling_ancestor_viewport: LayoutRect,
}

#[derive(Debug, Deserialize, Serialize)]
/// A node in a tree of scroll nodes. This may either be a scrollable
/// node which responds to scroll events or a non-scrollable one.
pub struct ScrollTreeNode {
    /// The index of the parent of this node in the tree. If this is
    /// None then this is the root node.
    pub parent: Option<ScrollTreeNodeId>,

    /// The children of this [`ScrollTreeNode`].
    pub children: Vec<ScrollTreeNodeId>,

    /// The WebRender id, which is filled in when this tree is serialiezd
    /// into a WebRender display list.
    pub webrender_id: Option<SpatialId>,

    /// Specific information about this node, depending on whether it is a scroll node
    /// or a reference frame.
    pub info: SpatialTreeNodeInfo,

    /// Cached transformation information that's used to do things like hit testing
    /// and viewport bounding box calculation.
    transformation_cache: Cell<Option<ScrollTreeNodeTransformationCache>>,
}

impl ScrollTreeNode {
    /// Get the WebRender [`SpatialId`] for the given [`ScrollNodeId`]. This will
    /// panic if [`ScrollTree::build_display_list`] has not been called yet.
    pub fn webrender_id(&self) -> SpatialId {
        self.webrender_id
            .expect("Should have called ScrollTree::build_display_list before querying SpatialId")
    }

    /// Get the external id of this node.
    pub fn external_id(&self) -> Option<ExternalScrollId> {
        match self.info {
            SpatialTreeNodeInfo::Scroll(ref info) => Some(info.external_id),
            _ => None,
        }
    }

    /// Get the offset id of this node if it applies.
    pub fn offset(&self) -> Option<LayoutVector2D> {
        match self.info {
            SpatialTreeNodeInfo::Scroll(ref info) => Some(info.offset),
            _ => None,
        }
    }

    /// Scroll this node given a WebRender ScrollLocation. Returns a tuple that can
    /// be used to scroll an individual WebRender scroll frame if the operation
    /// actually changed an offset.
    fn scroll(
        &mut self,
        scroll_location: ScrollLocation,
        context: ScrollType,
    ) -> Option<(ExternalScrollId, LayoutVector2D)> {
        let SpatialTreeNodeInfo::Scroll(ref mut info) = self.info else {
            return None;
        };

        info.scroll_to_webrender_location(scroll_location, context)
            .map(|location| (info.external_id, location))
    }

    pub fn debug_print(&self, print_tree: &mut PrintTree, node_index: usize) {
        match &self.info {
            SpatialTreeNodeInfo::ReferenceFrame(info) => {
                print_tree.new_level(format!(
                    "Reference Frame({node_index}): webrender_id={:?}\
                        \norigin: {:?}\
                        \ntransform_style: {:?}\
                        \ntransform: {:?}\
                        \nkind: {:?}",
                    self.webrender_id, info.origin, info.transform_style, info.transform, info.kind,
                ));
            },
            SpatialTreeNodeInfo::Scroll(info) => {
                print_tree.new_level(format!(
                    "Scroll Frame({node_index}): webrender_id={:?}\
                        \nexternal_id: {:?}\
                        \ncontent_rect: {:?}\
                        \nclip_rect: {:?}\
                        \nscroll_sensitivity: {:?}\
                        \noffset: {:?}",
                    self.webrender_id,
                    info.external_id,
                    info.content_rect,
                    info.clip_rect,
                    info.scroll_sensitivity,
                    info.offset,
                ));
            },
            SpatialTreeNodeInfo::Sticky(info) => {
                print_tree.new_level(format!(
                    "Sticky Frame({node_index}): webrender_id={:?}\
                        \nframe_rect: {:?}\
                        \nmargins: {:?}\
                        \nhorizontal_offset_bounds: {:?}\
                        \nvertical_offset_bounds: {:?}",
                    self.webrender_id,
                    info.frame_rect,
                    info.margins,
                    info.horizontal_offset_bounds,
                    info.vertical_offset_bounds,
                ));
            },
        };
    }

    fn invalidate_cached_transforms(&self, scroll_tree: &ScrollTree, ancestors_invalid: bool) {
        let node_invalid = match &self.info {
            SpatialTreeNodeInfo::Scroll(info) => info.offset_changed.take(),
            _ => false,
        };

        let invalid = node_invalid || ancestors_invalid;
        if invalid {
            self.transformation_cache.set(None);
        }

        for child_id in &self.children {
            scroll_tree
                .get_node(child_id)
                .invalidate_cached_transforms(scroll_tree, invalid);
        }
    }
}

/// A tree of spatial nodes, which mirrors the spatial nodes in the WebRender
/// display list, except these are used to scrolling in the compositor so that
/// new offsets can be sent to WebRender.
#[derive(Debug, Default, Deserialize, Serialize)]
pub struct ScrollTree {
    /// A list of compositor-side scroll nodes that describe the tree
    /// of WebRender spatial nodes, used by the compositor to scroll the
    /// contents of the display list.
    pub nodes: Vec<ScrollTreeNode>,
}

impl ScrollTree {
    /// Add a scroll node to this ScrollTree returning the id of the new node.
    pub fn add_scroll_tree_node(
        &mut self,
        parent: Option<&ScrollTreeNodeId>,
        info: SpatialTreeNodeInfo,
    ) -> ScrollTreeNodeId {
        self.nodes.push(ScrollTreeNode {
            parent: parent.cloned(),
            children: Vec::new(),
            webrender_id: None,
            info,
            transformation_cache: Cell::default(),
        });

        let new_node_id = ScrollTreeNodeId {
            index: self.nodes.len() - 1,
        };

        if let Some(parent_id) = parent {
            self.get_node_mut(parent_id).children.push(new_node_id);
        }

        new_node_id
    }

    /// Once WebRender display list construction is complete for this [`ScrollTree`], update
    /// the mapping of nodes to WebRender [`SpatialId`]s.
    pub fn update_mapping(&mut self, mapping: Vec<SpatialId>) {
        for (spatial_id, node) in mapping.into_iter().zip(self.nodes.iter_mut()) {
            node.webrender_id = Some(spatial_id);
        }
    }

    /// Get a mutable reference to the node with the given index.
    pub fn get_node_mut(&mut self, id: &ScrollTreeNodeId) -> &mut ScrollTreeNode {
        &mut self.nodes[id.index]
    }

    /// Get an immutable reference to the node with the given index.
    pub fn get_node(&self, id: &ScrollTreeNodeId) -> &ScrollTreeNode {
        &self.nodes[id.index]
    }

    /// Get the WebRender [`SpatialId`] for the given [`ScrollNodeId`]. This will
    /// panic if [`ScrollTree::build_display_list`] has not been called yet.
    pub fn webrender_id(&self, id: &ScrollTreeNodeId) -> SpatialId {
        self.get_node(id).webrender_id()
    }

    pub fn scroll_node_or_ancestor_inner(
        &mut self,
        scroll_node_id: &ScrollTreeNodeId,
        scroll_location: ScrollLocation,
        context: ScrollType,
    ) -> Option<(ExternalScrollId, LayoutVector2D)> {
        let parent = {
            let node = &mut self.get_node_mut(scroll_node_id);
            let result = node.scroll(scroll_location, context);
            if result.is_some() {
                return result;
            }
            node.parent
        };

        parent.and_then(|parent| {
            self.scroll_node_or_ancestor_inner(&parent, scroll_location, context)
        })
    }

    /// Scroll the given scroll node on this scroll tree. If the node cannot be scrolled,
    /// because it isn't a scrollable node or it's already scrolled to the maximum scroll
    /// extent, try to scroll an ancestor of this node. Returns the node scrolled and the
    /// new offset if a scroll was performed, otherwise returns None.
    pub fn scroll_node_or_ancestor(
        &mut self,
        scroll_node_id: &ScrollTreeNodeId,
        scroll_location: ScrollLocation,
        context: ScrollType,
    ) -> Option<(ExternalScrollId, LayoutVector2D)> {
        let result = self.scroll_node_or_ancestor_inner(scroll_node_id, scroll_location, context);
        if result.is_some() {
            self.invalidate_cached_transforms();
        }
        result
    }

    /// Given an [`ExternalScrollId`] and an offset, update the scroll offset of the scroll node
    /// with the given id.
    pub fn set_scroll_offset_for_node_with_external_scroll_id(
        &mut self,
        external_scroll_id: ExternalScrollId,
        offset: LayoutVector2D,
        context: ScrollType,
    ) -> Option<LayoutVector2D> {
        let result = self.nodes.iter_mut().find_map(|node| match node.info {
            SpatialTreeNodeInfo::Scroll(ref mut scroll_info)
                if scroll_info.external_id == external_scroll_id =>
            {
                scroll_info.scroll_to_offset(offset, context)
            },
            _ => None,
        });

        if result.is_some() {
            self.invalidate_cached_transforms();
        }

        result
    }

    /// Given a set of all scroll offsets coming from the Servo renderer, update all of the offsets
    /// for nodes that actually exist in this tree.
    pub fn set_all_scroll_offsets(&mut self, offsets: &HashMap<ExternalScrollId, LayoutVector2D>) {
        for node in self.nodes.iter_mut() {
            if let SpatialTreeNodeInfo::Scroll(ref mut scroll_info) = node.info {
                if let Some(offset) = offsets.get(&scroll_info.external_id) {
                    scroll_info.scroll_to_offset(*offset, ScrollType::Script);
                }
            }
        }

        self.invalidate_cached_transforms();
    }

    /// Set the offsets of all scrolling nodes in this tree to 0.
    pub fn reset_all_scroll_offsets(&mut self) {
        for node in self.nodes.iter_mut() {
            if let SpatialTreeNodeInfo::Scroll(ref mut scroll_info) = node.info {
                scroll_info.scroll_to_offset(LayoutVector2D::zero(), ScrollType::Script);
            }
        }

        self.invalidate_cached_transforms();
    }

    /// Collect all of the scroll offsets of the scrolling nodes of this tree into a
    /// [`HashMap`] which can be applied to another tree.
    pub fn scroll_offsets(&self) -> HashMap<ExternalScrollId, LayoutVector2D> {
        HashMap::from_iter(self.nodes.iter().filter_map(|node| match node.info {
            SpatialTreeNodeInfo::Scroll(ref scroll_info) => {
                Some((scroll_info.external_id, scroll_info.offset))
            },
            _ => None,
        }))
    }

    /// Get the scroll offset for the given [`ExternalScrollId`] or `None` if that node cannot
    /// be found in the tree.
    pub fn scroll_offset(&self, id: ExternalScrollId) -> Option<LayoutVector2D> {
        self.nodes.iter().find_map(|node| match node.info {
            SpatialTreeNodeInfo::Scroll(ref info) if info.external_id == id => Some(info.offset),
            _ => None,
        })
    }

    /// Traverse a scroll node to its root to calculate the transform.
    pub fn cumulative_node_to_root_transform(&self, node_id: &ScrollTreeNodeId) -> LayoutTransform {
        let node = self.get_node(node_id);
        if let Some(cached_transforms) = node.transformation_cache.get() {
            return cached_transforms.node_to_root_transform;
        }

        let (transforms, _) = self.cumulative_node_transform_inner(node);
        node.transformation_cache.set(Some(transforms));
        transforms.node_to_root_transform
    }

    /// Traverse a scroll node to its root to calculate the transform.
    fn cumulative_node_transform_inner(
        &self,
        node: &ScrollTreeNode,
    ) -> (ScrollTreeNodeTransformationCache, AncestorStickyInfo) {
        let (parent_transforms, mut sticky_info) = match node.parent {
            Some(parent_id) => self.cumulative_node_transform_inner(self.get_node(&parent_id)),
            None => (Default::default(), Default::default()),
        };

        let change_basis =
            |transform: &Transform3D<f32, LayoutPixel, LayoutPixel>, x: f32, y: f32| {
                let pre_translation = Transform3D::translation(x, y, 0.0);
                let post_translation = Transform3D::translation(-x, -y, 0.0);
                post_translation.then(transform).then(&pre_translation)
            };

        let node_to_parent_transform = match &node.info {
            SpatialTreeNodeInfo::ReferenceFrame(info) => {
                // To apply a transformation we need to make sure the rectangle's
                // coordinate space is the same as reference frame's coordinate space.
                let node_to_parent_transform = change_basis(
                    &info.transform,
                    info.frame_origin_for_query.x,
                    info.frame_origin_for_query.y,
                );

                sticky_info.nearest_scrolling_ancestor_viewport = sticky_info
                    .nearest_scrolling_ancestor_viewport
                    .translate(-info.origin.to_vector());

                node_to_parent_transform
            },
            SpatialTreeNodeInfo::Scroll(info) => {
                sticky_info.nearest_scrolling_ancestor_viewport = info.clip_rect;
                sticky_info.nearest_scrolling_ancestor_offset = -info.offset;

                Transform3D::translation(-info.offset.x, -info.offset.y, 0.0)
            },

            SpatialTreeNodeInfo::Sticky(info) => {
                let offset = info.calculate_sticky_offset(&sticky_info);
                sticky_info.nearest_scrolling_ancestor_offset += offset;
                Transform3D::translation(offset.x, offset.y, 0.0)
            },
        };

        let node_to_root_transform =
            node_to_parent_transform.then(&parent_transforms.node_to_root_transform);
        let transforms = ScrollTreeNodeTransformationCache {
            node_to_root_transform,
        };
        (transforms, sticky_info)
    }

    fn invalidate_cached_transforms(&self) {
        let Some(root_node) = self.nodes.first() else {
            return;
        };
        root_node.invalidate_cached_transforms(self, false /* ancestors_invalid */);
    }
}

/// In order to pretty print the [ScrollTree] structure, we are converting
/// the node list inside the tree to be a adjacency list. The adjacency list
/// then is used for the [ScrollTree::debug_print_traversal] of the tree.
///
/// This preprocessing helps decouples print logic a lot from its construction.
type AdjacencyListForPrint = Vec<Vec<ScrollTreeNodeId>>;

/// Implementation of [ScrollTree] that is related to debugging.
// FIXME: probably we could have a universal trait for this. Especially for
//        structures that utilizes PrintTree.
impl ScrollTree {
    fn nodes_in_adjacency_list(&self) -> AdjacencyListForPrint {
        let mut adjacency_list: AdjacencyListForPrint = vec![Default::default(); self.nodes.len()];

        for (node_index, node) in self.nodes.iter().enumerate() {
            let current_id = ScrollTreeNodeId { index: node_index };
            if let Some(parent_id) = node.parent {
                adjacency_list[parent_id.index].push(current_id);
            }
        }

        adjacency_list
    }

    fn debug_print_traversal(
        &self,
        print_tree: &mut PrintTree,
        current_id: &ScrollTreeNodeId,
        adjacency_list: &[Vec<ScrollTreeNodeId>],
    ) {
        for node_id in &adjacency_list[current_id.index] {
            self.nodes[node_id.index].debug_print(print_tree, node_id.index);
            self.debug_print_traversal(print_tree, node_id, adjacency_list);
        }
        print_tree.end_level();
    }

    /// Print the [ScrollTree]. Particularly, we are printing the node in
    /// preorder traversal. The order of the nodes will depends of the
    /// index of a node in the [ScrollTree] which corresponds to the
    /// declarations of the nodes.
    // TODO(stevennovaryo): add information about which fragment that
    //                      defines this node.
    pub fn debug_print(&self) {
        let mut print_tree = PrintTree::new("Scroll Tree".to_owned());

        let adj_list = self.nodes_in_adjacency_list();
        let root_id = ScrollTreeNodeId { index: 0 };

        self.nodes[root_id.index].debug_print(&mut print_tree, root_id.index);
        self.debug_print_traversal(&mut print_tree, &root_id, &adj_list);
        print_tree.end_level();
    }
}

/// A data structure which stores compositor-side information about
/// display lists sent to the compositor.
#[derive(Debug, Deserialize, Serialize)]
pub struct CompositorDisplayListInfo {
    /// The WebRender [PipelineId] of this display list.
    pub pipeline_id: PipelineId,

    /// The [`ViewportDetails`] that describe the viewport in the script/layout thread at
    /// the time this display list was created.
    pub viewport_details: ViewportDetails,

    /// The size of this display list's content.
    pub content_size: LayoutSize,

    /// The epoch of the display list.
    pub epoch: Epoch,

    /// An array of `HitTestInfo` which is used to store information
    /// to assist the compositor to take various actions (set the cursor,
    /// scroll without layout) using a WebRender hit test result.
    pub hit_test_info: Vec<HitTestInfo>,

    /// A ScrollTree used by the compositor to scroll the contents of the
    /// display list.
    pub scroll_tree: ScrollTree,

    /// The `ScrollTreeNodeId` of the root reference frame of this info's scroll
    /// tree.
    pub root_reference_frame_id: ScrollTreeNodeId,

    /// The `ScrollTreeNodeId` of the topmost scrolling frame of this info's scroll
    /// tree.
    pub root_scroll_node_id: ScrollTreeNodeId,

    /// Contentful paint i.e. whether the display list contains items of type
    /// text, image, non-white canvas or SVG). Used by metrics.
    /// See <https://w3c.github.io/paint-timing/#first-contentful-paint>.
    pub is_contentful: bool,

    /// Whether the first layout or a subsequent (incremental) layout triggered this
    /// display list creation.
    pub first_reflow: bool,
}

impl CompositorDisplayListInfo {
    /// Create a new CompositorDisplayListInfo with the root reference frame
    /// and scroll frame already added to the scroll tree.
    pub fn new(
        viewport_details: ViewportDetails,
        content_size: LayoutSize,
        pipeline_id: PipelineId,
        epoch: Epoch,
        viewport_scroll_sensitivity: AxesScrollSensitivity,
        first_reflow: bool,
    ) -> Self {
        let mut scroll_tree = ScrollTree::default();
        let root_reference_frame_id = scroll_tree.add_scroll_tree_node(
            None,
            SpatialTreeNodeInfo::ReferenceFrame(ReferenceFrameNodeInfo {
                origin: Default::default(),
                frame_origin_for_query: Default::default(),
                transform_style: TransformStyle::Flat,
                transform: LayoutTransform::identity(),
                kind: ReferenceFrameKind::default(),
            }),
        );
        let root_scroll_node_id = scroll_tree.add_scroll_tree_node(
            Some(&root_reference_frame_id),
            SpatialTreeNodeInfo::Scroll(ScrollableNodeInfo {
                external_id: ExternalScrollId(0, pipeline_id),
                content_rect: LayoutRect::from_origin_and_size(LayoutPoint::zero(), content_size),
                clip_rect: LayoutRect::from_origin_and_size(
                    LayoutPoint::zero(),
                    viewport_details.layout_size(),
                ),
                scroll_sensitivity: viewport_scroll_sensitivity,
                offset: LayoutVector2D::zero(),
                offset_changed: Cell::new(false),
            }),
        );

        CompositorDisplayListInfo {
            pipeline_id,
            viewport_details,
            content_size,
            epoch,
            hit_test_info: Default::default(),
            scroll_tree,
            root_reference_frame_id,
            root_scroll_node_id,
            is_contentful: false,
            first_reflow,
        }
    }

    /// Add or re-use a duplicate HitTestInfo entry in this `CompositorHitTestInfo`
    /// and return the index.
    pub fn add_hit_test_info(
        &mut self,
        node: u64,
        cursor: Option<Cursor>,
        scroll_tree_node: ScrollTreeNodeId,
    ) -> usize {
        let hit_test_info = HitTestInfo {
            node,
            cursor,
            scroll_tree_node,
        };

        if let Some(last) = self.hit_test_info.last() {
            if hit_test_info == *last {
                return self.hit_test_info.len() - 1;
            }
        }

        self.hit_test_info.push(hit_test_info);
        self.hit_test_info.len() - 1
    }
}

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use std::cell::{Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::collections::hash_map::Entry;
use std::path::PathBuf;
use std::rc::Rc;

use crossbeam_channel::Receiver;
use euclid::Vector2D;
use keyboard_types::{Key, Modifiers, ShortcutMatcher};
use log::{error, info};
use servo::base::id::WebViewId;
use servo::config::pref;
use servo::ipc_channel::ipc::IpcSender;
use servo::webrender_api::ScrollLocation;
use servo::webrender_api::units::{DeviceIntPoint, DeviceIntSize};
use servo::{
    AllowOrDenyRequest, AuthenticationRequest, FilterPattern, FocusId, FormControl,
    GamepadHapticEffectType, KeyboardEvent, LoadStatus, PermissionRequest, Servo, ServoDelegate,
    ServoError, SimpleDialog, TraversalId, WebDriverCommandMsg, WebDriverJSResult,
    WebDriverJSValue, WebDriverLoadStatus, WebDriverUserPrompt, WebView, WebViewBuilder,
    WebViewDelegate,
};
use url::Url;

use super::app::PumpResult;
use super::dialog::Dialog;
use super::gamepad::GamepadSupport;
use super::keyutils::CMD_OR_CONTROL;
use super::window_trait::{LINE_HEIGHT, LINE_WIDTH, WindowPortsMethods};
use crate::output_image::save_output_image_if_necessary;
use crate::prefs::ServoShellPreferences;

pub(crate) enum AppState {
    Initializing,
    Running(Rc<RunningAppState>),
    ShuttingDown,
}

/// A collection of [`IpcSender`]s that are used to asynchronously communicate
/// to a WebDriver server with information about application state.
#[derive(Clone, Default)]
struct WebDriverSenders {
    pub load_status_senders: HashMap<WebViewId, IpcSender<WebDriverLoadStatus>>,
    pub script_evaluation_interrupt_sender: Option<IpcSender<WebDriverJSResult>>,
    pub pending_traversals: HashMap<TraversalId, IpcSender<WebDriverLoadStatus>>,
    pub pending_focus: HashMap<FocusId, IpcSender<bool>>,
}

pub(crate) struct RunningAppState {
    /// A handle to the Servo instance of the [`RunningAppState`]. This is not stored inside
    /// `inner` so that we can keep a reference to Servo in order to spin the event loop,
    /// which will in turn call delegates doing a mutable borrow on `inner`.
    servo: Servo,
    /// The preferences for this run of servoshell. This is not mutable, so doesn't need to
    /// be stored inside the [`RunningAppStateInner`].
    servoshell_preferences: ServoShellPreferences,
    /// A [`Receiver`] for receiving commands from a running WebDriver server, if WebDriver
    /// was enabled.
    webdriver_receiver: Option<Receiver<WebDriverCommandMsg>>,
    webdriver_senders: RefCell<WebDriverSenders>,
    inner: RefCell<RunningAppStateInner>,
}

pub struct RunningAppStateInner {
    /// List of top-level browsing contexts.
    /// Modified by EmbedderMsg::WebViewOpened and EmbedderMsg::WebViewClosed,
    /// and we exit if it ever becomes empty.
    webviews: HashMap<WebViewId, WebView>,

    /// The order in which the webviews were created.
    creation_order: Vec<WebViewId>,

    /// The webview that is currently focused.
    /// Modified by EmbedderMsg::WebViewFocused and EmbedderMsg::WebViewBlurred.
    focused_webview_id: Option<WebViewId>,

    /// The current set of open dialogs.
    dialogs: HashMap<WebViewId, Vec<Dialog>>,

    /// A handle to the Window that Servo is rendering in -- either headed or headless.
    window: Rc<dyn WindowPortsMethods>,

    /// Gamepad support, which may be `None` if it failed to initialize.
    gamepad_support: Option<GamepadSupport>,

    /// Whether or not the application interface needs to be updated.
    need_update: bool,

    /// Whether or not Servo needs to repaint its display. Currently this is global
    /// because every `WebView` shares a `RenderingContext`.
    need_repaint: bool,
}

impl Drop for RunningAppState {
    fn drop(&mut self) {
        self.servo.deinit();
    }
}

impl RunningAppState {
    pub fn new(
        servo: Servo,
        window: Rc<dyn WindowPortsMethods>,
        servoshell_preferences: ServoShellPreferences,
        webdriver_receiver: Option<Receiver<WebDriverCommandMsg>>,
    ) -> RunningAppState {
        servo.set_delegate(Rc::new(ServoShellServoDelegate));
        RunningAppState {
            servo,
            servoshell_preferences,
            webdriver_receiver,
            webdriver_senders: RefCell::default(),
            inner: RefCell::new(RunningAppStateInner {
                webviews: HashMap::default(),
                creation_order: Default::default(),
                focused_webview_id: None,
                dialogs: Default::default(),
                window,
                gamepad_support: GamepadSupport::maybe_new(),
                need_update: false,
                need_repaint: false,
            }),
        }
    }

    pub(crate) fn create_and_focus_toplevel_webview(self: &Rc<Self>, url: Url) {
        let webview = self.create_toplevel_webview(url);
        webview.focus();
        webview.raise_to_top(true);
    }

    pub(crate) fn create_toplevel_webview(self: &Rc<Self>, url: Url) -> WebView {
        let webview = WebViewBuilder::new(self.servo())
            .url(url)
            .hidpi_scale_factor(self.inner().window.hidpi_scale_factor())
            .delegate(self.clone())
            .build();

        webview.notify_theme_change(self.inner().window.theme());
        self.add(webview.clone());
        webview
    }

    pub(crate) fn inner(&self) -> Ref<RunningAppStateInner> {
        self.inner.borrow()
    }

    pub(crate) fn inner_mut(&self) -> RefMut<RunningAppStateInner> {
        self.inner.borrow_mut()
    }

    pub(crate) fn servo(&self) -> &Servo {
        &self.servo
    }

    pub(crate) fn webdriver_receiver(&self) -> Option<&Receiver<WebDriverCommandMsg>> {
        self.webdriver_receiver.as_ref()
    }

    pub(crate) fn hidpi_scale_factor_changed(&self) {
        let inner = self.inner();
        let new_scale_factor = inner.window.hidpi_scale_factor();
        for webview in inner.webviews.values() {
            webview.set_hidpi_scale_factor(new_scale_factor);
        }
    }

    /// Repaint the Servo view is necessary, returning true if anything was actually
    /// painted or false otherwise. Something may not be painted if Servo is waiting
    /// for a stable image to paint.
    pub(crate) fn repaint_servo_if_necessary(&self) {
        if !self.inner().need_repaint {
            return;
        }
        let Some(webview) = self.focused_webview() else {
            return;
        };
        if !webview.paint() {
            return;
        }

        // This needs to be done before presenting(), because `ReneringContext::read_to_image` reads
        // from the back buffer.
        save_output_image_if_necessary(
            &self.servoshell_preferences,
            &self.inner().window.rendering_context(),
        );

        let mut inner_mut = self.inner_mut();
        inner_mut.window.rendering_context().present();
        inner_mut.need_repaint = false;

        if self.servoshell_preferences.exit_after_stable_image {
            self.servo().start_shutting_down();
        }
    }

    /// Spins the internal application event loop.
    ///
    /// - Notifies Servo about incoming gamepad events
    /// - Spin the Servo event loop, which will run the compositor and trigger delegate methods.
    pub(crate) fn pump_event_loop(&self) -> PumpResult {
        if pref!(dom_gamepad_enabled) {
            self.handle_gamepad_events();
        }

        if !self.servo().spin_event_loop() {
            return PumpResult::Shutdown;
        }

        // Delegate handlers may have asked us to present or update compositor contents.
        // Currently, egui-file-dialog dialogs need to be constantly redrawn or animations aren't fluid.
        let need_window_redraw = self.inner().need_repaint || self.has_active_dialog();
        let need_update = std::mem::replace(&mut self.inner_mut().need_update, false);

        PumpResult::Continue {
            need_update,
            need_window_redraw,
        }
    }

    pub(crate) fn add(&self, webview: WebView) {
        self.inner_mut().creation_order.push(webview.id());
        self.inner_mut().webviews.insert(webview.id(), webview);
    }

    pub(crate) fn shutdown(&self) {
        self.inner_mut().webviews.clear();
    }

    pub(crate) fn for_each_active_dialog(&self, callback: impl Fn(&mut Dialog) -> bool) {
        let last_created_webview_id = self.inner().creation_order.last().cloned();
        let Some(webview_id) = self
            .focused_webview()
            .as_ref()
            .map(WebView::id)
            .or(last_created_webview_id)
        else {
            return;
        };

        if let Some(dialogs) = self.inner_mut().dialogs.get_mut(&webview_id) {
            dialogs.retain_mut(callback);
        }
    }

    pub fn close_webview(&self, webview_id: WebViewId) {
        // This can happen because we can trigger a close with a UI action and then get the
        // close event from Servo later.
        let mut inner = self.inner_mut();
        if !inner.webviews.contains_key(&webview_id) {
            return;
        }

        inner.webviews.retain(|&id, _| id != webview_id);
        inner.creation_order.retain(|&id| id != webview_id);
        inner.dialogs.remove(&webview_id);
        if Some(webview_id) == inner.focused_webview_id {
            inner.focused_webview_id = None;
        }

        let last_created = inner
            .creation_order
            .last()
            .and_then(|id| inner.webviews.get(id));

        match last_created {
            Some(last_created_webview) => {
                last_created_webview.focus();
            },
            None if self.servoshell_preferences.webdriver_port.is_none() => {
                self.servo.start_shutting_down()
            },
            None => {
                // For WebDriver, don't shut down when last webview closed
                // https://github.com/servo/servo/issues/37408
            },
        }
    }

    pub fn focused_webview(&self) -> Option<WebView> {
        self.inner()
            .focused_webview_id
            .and_then(|id| self.inner().webviews.get(&id).cloned())
    }

    // Returns the webviews in the creation order.
    pub fn webviews(&self) -> Vec<(WebViewId, WebView)> {
        let inner = self.inner();
        inner
            .creation_order
            .iter()
            .map(|id| (*id, inner.webviews.get(id).unwrap().clone()))
            .collect()
    }

    pub fn webview_by_id(&self, id: WebViewId) -> Option<WebView> {
        self.inner().webviews.get(&id).cloned()
    }

    pub fn handle_gamepad_events(&self) {
        let Some(active_webview) = self.focused_webview() else {
            return;
        };
        if let Some(gamepad_support) = self.inner_mut().gamepad_support.as_mut() {
            gamepad_support.handle_gamepad_events(active_webview);
        }
    }

    pub(crate) fn focus_webview_by_index(&self, index: usize) {
        if let Some((_, webview)) = self.webviews().get(index) {
            webview.focus();
        }
    }

    fn add_dialog(&self, webview: servo::WebView, dialog: Dialog) {
        let mut inner_mut = self.inner_mut();
        inner_mut
            .dialogs
            .entry(webview.id())
            .or_default()
            .push(dialog);
        inner_mut.need_update = true;
    }

    pub(crate) fn has_active_dialog(&self) -> bool {
        let last_created_webview_id = self.inner().creation_order.last().cloned();
        let Some(webview_id) = self
            .focused_webview()
            .as_ref()
            .map(WebView::id)
            .or(last_created_webview_id)
        else {
            return false;
        };

        let inner = self.inner();
        inner
            .dialogs
            .get(&webview_id)
            .is_some_and(|dialogs| !dialogs.is_empty())
    }

    pub(crate) fn webview_has_active_dialog(&self, webview_id: WebViewId) -> bool {
        self.inner()
            .dialogs
            .get(&webview_id)
            .is_some_and(|dialogs| !dialogs.is_empty())
    }

    pub(crate) fn get_current_active_dialog_webdriver_type(
        &self,
        webview_id: WebViewId,
    ) -> Option<WebDriverUserPrompt> {
        self.inner()
            .dialogs
            .get(&webview_id)
            .and_then(|dialogs| dialogs.last())
            .map(|dialog| dialog.webdriver_diaglog_type())
    }

    pub(crate) fn accept_active_dialogs(&self, webview_id: WebViewId) {
        if let Some(dialogs) = self.inner_mut().dialogs.get_mut(&webview_id) {
            dialogs.drain(..).for_each(|dialog| {
                dialog.accept();
            });
        }
    }

    pub(crate) fn dismiss_active_dialogs(&self, webview_id: WebViewId) {
        if let Some(dialogs) = self.inner_mut().dialogs.get_mut(&webview_id) {
            dialogs.drain(..).for_each(|dialog| {
                dialog.dismiss();
            });
        }
    }

    pub(crate) fn alert_text_of_newest_dialog(&self, webview_id: WebViewId) -> Option<String> {
        self.inner()
            .dialogs
            .get(&webview_id)
            .and_then(|dialogs| dialogs.last())
            .and_then(|dialog| dialog.message())
    }

    pub(crate) fn set_alert_text_of_newest_dialog(&self, webview_id: WebViewId, text: String) {
        if let Some(dialogs) = self.inner_mut().dialogs.get_mut(&webview_id) {
            if let Some(dialog) = dialogs.last_mut() {
                dialog.set_message(text);
            }
        }
    }

    pub(crate) fn get_focused_webview_index(&self) -> Option<usize> {
        let focused_id = self.inner().focused_webview_id?;
        self.webviews()
            .iter()
            .position(|webview| webview.0 == focused_id)
    }

    /// Handle servoshell key bindings that may have been prevented by the page in the focused webview.
    fn handle_overridable_key_bindings(&self, webview: ::servo::WebView, event: KeyboardEvent) {
        let origin = webview.rect().min.ceil().to_i32();
        ShortcutMatcher::from_event(event.event)
            .shortcut(CMD_OR_CONTROL, '=', || {
                webview.set_zoom(1.1);
            })
            .shortcut(CMD_OR_CONTROL, '+', || {
                webview.set_zoom(1.1);
            })
            .shortcut(CMD_OR_CONTROL, '-', || {
                webview.set_zoom(1.0 / 1.1);
            })
            .shortcut(CMD_OR_CONTROL, '0', || {
                webview.reset_zoom();
            })
            .shortcut(Modifiers::empty(), Key::PageDown, || {
                let scroll_location = ScrollLocation::Delta(Vector2D::new(
                    0.0,
                    self.inner().window.page_height() - 2.0 * LINE_HEIGHT,
                ));
                webview.notify_scroll_event(scroll_location, origin);
            })
            .shortcut(Modifiers::empty(), Key::PageUp, || {
                let scroll_location = ScrollLocation::Delta(Vector2D::new(
                    0.0,
                    -self.inner().window.page_height() + 2.0 * LINE_HEIGHT,
                ));
                webview.notify_scroll_event(scroll_location, origin);
            })
            .shortcut(Modifiers::empty(), Key::Home, || {
                webview.notify_scroll_event(ScrollLocation::Start, origin);
            })
            .shortcut(Modifiers::empty(), Key::End, || {
                webview.notify_scroll_event(ScrollLocation::End, origin);
            })
            .shortcut(Modifiers::empty(), Key::ArrowUp, || {
                let location = ScrollLocation::Delta(Vector2D::new(0.0, -1.0 * LINE_HEIGHT));
                webview.notify_scroll_event(location, origin);
            })
            .shortcut(Modifiers::empty(), Key::ArrowDown, || {
                let location = ScrollLocation::Delta(Vector2D::new(0.0, 1.0 * LINE_HEIGHT));
                webview.notify_scroll_event(location, origin);
            })
            .shortcut(Modifiers::empty(), Key::ArrowLeft, || {
                let location = ScrollLocation::Delta(Vector2D::new(-LINE_WIDTH, 0.0));
                webview.notify_scroll_event(location, origin);
            })
            .shortcut(Modifiers::empty(), Key::ArrowRight, || {
                let location = ScrollLocation::Delta(Vector2D::new(LINE_WIDTH, 0.0));
                webview.notify_scroll_event(location, origin);
            });
    }

    pub(crate) fn set_pending_focus(&self, focus_id: FocusId, sender: IpcSender<bool>) {
        self.webdriver_senders
            .borrow_mut()
            .pending_focus
            .insert(focus_id, sender);
    }

    pub(crate) fn set_pending_traversal(
        &self,
        traversal_id: TraversalId,
        sender: IpcSender<WebDriverLoadStatus>,
    ) {
        self.webdriver_senders
            .borrow_mut()
            .pending_traversals
            .insert(traversal_id, sender);
    }

    pub(crate) fn set_load_status_sender(
        &self,
        webview_id: WebViewId,
        sender: IpcSender<WebDriverLoadStatus>,
    ) {
        self.webdriver_senders
            .borrow_mut()
            .load_status_senders
            .insert(webview_id, sender);
    }

    pub(crate) fn set_script_command_interrupt_sender(
        &self,
        sender: Option<IpcSender<WebDriverJSResult>>,
    ) {
        self.webdriver_senders
            .borrow_mut()
            .script_evaluation_interrupt_sender = sender;
    }

    /// Interrupt any ongoing WebDriver-based script evaluation.
    ///
    /// From <https://w3c.github.io/webdriver/#dfn-execute-a-function-body>:
    /// > The rules to execute a function body are as follows. The algorithm returns
    /// > an ECMAScript completion record.
    /// >
    /// > If at any point during the algorithm a user prompt appears, immediately return
    /// > Completion { Type: normal, Value: null, Target: empty }, but continue to run the
    /// >  other steps of this algorithm in parallel.
    fn interrupt_webdriver_script_evaluation(&self) {
        if let Some(sender) = &self
            .webdriver_senders
            .borrow()
            .script_evaluation_interrupt_sender
        {
            sender.send(Ok(WebDriverJSValue::Null)).unwrap_or_else(|err| {
                info!("Notify dialog appear failed. Maybe the channel to webdriver is closed: {err}");
            });
        }
    }

    pub(crate) fn remove_load_status_sender(&self, webview_id: WebViewId) {
        self.webdriver_senders
            .borrow_mut()
            .load_status_senders
            .remove(&webview_id);
    }
}

struct ServoShellServoDelegate;
impl ServoDelegate for ServoShellServoDelegate {
    fn notify_devtools_server_started(&self, _servo: &Servo, port: u16, _token: String) {
        info!("Devtools Server running on port {port}");
    }

    fn request_devtools_connection(&self, _servo: &Servo, request: AllowOrDenyRequest) {
        request.allow();
    }

    fn notify_error(&self, _servo: &Servo, error: ServoError) {
        error!("Saw Servo error: {error:?}!");
    }
}

impl WebViewDelegate for RunningAppState {
    fn screen_geometry(&self, _webview: WebView) -> Option<servo::ScreenGeometry> {
        Some(self.inner().window.screen_geometry())
    }

    fn notify_status_text_changed(&self, _webview: servo::WebView, _status: Option<String>) {
        self.inner_mut().need_update = true;
    }

    fn notify_page_title_changed(&self, webview: servo::WebView, title: Option<String>) {
        if webview.focused() {
            let window_title = format!("{} - Servo", title.clone().unwrap_or_default());
            self.inner().window.set_title(&window_title);
            self.inner_mut().need_update = true;
        }
    }

    fn notify_traversal_complete(&self, _webview: servo::WebView, traversal_id: TraversalId) {
        let mut webdriver_state = self.webdriver_senders.borrow_mut();
        if let Entry::Occupied(entry) = webdriver_state.pending_traversals.entry(traversal_id) {
            let sender = entry.remove();
            let _ = sender.send(WebDriverLoadStatus::Complete);
        }
    }

    fn request_move_to(&self, _: servo::WebView, new_position: DeviceIntPoint) {
        self.inner().window.set_position(new_position);
    }

    fn request_resize_to(&self, webview: servo::WebView, requested_outer_size: DeviceIntSize) {
        // We need to update compositor's view later as we not sure about resizing result.
        self.inner()
            .window
            .request_resize(&webview, requested_outer_size);
    }

    fn show_simple_dialog(&self, webview: servo::WebView, dialog: SimpleDialog) {
        self.interrupt_webdriver_script_evaluation();

        // Dialogs block the page load, so need need to notify WebDriver
        let webview_id = webview.id();
        if let Some(sender) = self
            .webdriver_senders
            .borrow_mut()
            .load_status_senders
            .get(&webview_id)
        {
            let _ = sender.send(WebDriverLoadStatus::Blocked);
        };

        if self.servoshell_preferences.headless &&
            self.servoshell_preferences.webdriver_port.is_none()
        {
            // TODO: Avoid copying this from the default trait impl?
            // Return the DOM-specified default value for when we **cannot show simple dialogs**.
            let _ = match dialog {
                SimpleDialog::Alert {
                    response_sender, ..
                } => response_sender.send(Default::default()),
                SimpleDialog::Confirm {
                    response_sender, ..
                } => response_sender.send(Default::default()),
                SimpleDialog::Prompt {
                    response_sender, ..
                } => response_sender.send(Default::default()),
            };
            return;
        }
        let dialog = Dialog::new_simple_dialog(dialog);
        self.add_dialog(webview, dialog);
    }

    fn request_authentication(
        &self,
        webview: WebView,
        authentication_request: AuthenticationRequest,
    ) {
        if self.servoshell_preferences.headless &&
            self.servoshell_preferences.webdriver_port.is_none()
        {
            return;
        }

        self.add_dialog(
            webview,
            Dialog::new_authentication_dialog(authentication_request),
        );
    }

    fn request_open_auxiliary_webview(
        &self,
        parent_webview: servo::WebView,
    ) -> Option<servo::WebView> {
        let webview = WebViewBuilder::new_auxiliary(&self.servo)
            .hidpi_scale_factor(self.inner().window.hidpi_scale_factor())
            .delegate(parent_webview.delegate())
            .build();

        webview.notify_theme_change(self.inner().window.theme());
        // When WebDriver is enabled, do not focus and raise the WebView to the top,
        // as that is what the specification expects. Otherwise, we would like `window.open()`
        // to create a new foreground tab
        if self.servoshell_preferences.webdriver_port.is_none() {
            webview.focus();
            webview.raise_to_top(true);
        }
        self.add(webview.clone());
        Some(webview)
    }

    fn notify_closed(&self, webview: servo::WebView) {
        self.close_webview(webview.id());
    }

    fn notify_focus_complete(&self, webview: servo::WebView, focus_id: FocusId) {
        let mut webdriver_state = self.webdriver_senders.borrow_mut();
        if let Entry::Occupied(entry) = webdriver_state.pending_focus.entry(focus_id) {
            let sender = entry.remove();
            let _ = sender.send(webview.focused());
        }
    }

    fn notify_focus_changed(&self, webview: servo::WebView, focused: bool) {
        let mut inner_mut = self.inner_mut();
        if focused {
            webview.show(true);
            inner_mut.need_update = true;
            inner_mut.focused_webview_id = Some(webview.id());
        } else if inner_mut.focused_webview_id == Some(webview.id()) {
            inner_mut.focused_webview_id = None;
        }
    }

    fn notify_keyboard_event(&self, webview: servo::WebView, keyboard_event: KeyboardEvent) {
        self.handle_overridable_key_bindings(webview, keyboard_event);
    }

    fn notify_cursor_changed(&self, _webview: servo::WebView, cursor: servo::Cursor) {
        self.inner().window.set_cursor(cursor);
    }

    fn notify_load_status_changed(&self, webview: servo::WebView, status: LoadStatus) {
        self.inner_mut().need_update = true;

        if status == LoadStatus::Complete {
            if let Some(sender) = self
                .webdriver_senders
                .borrow_mut()
                .load_status_senders
                .remove(&webview.id())
            {
                let _ = sender.send(WebDriverLoadStatus::Complete);
            }
        }
    }

    fn notify_fullscreen_state_changed(&self, _webview: servo::WebView, fullscreen_state: bool) {
        self.inner().window.set_fullscreen(fullscreen_state);
    }

    fn show_bluetooth_device_dialog(
        &self,
        webview: servo::WebView,
        devices: Vec<String>,
        response_sender: IpcSender<Option<String>>,
    ) {
        self.add_dialog(
            webview,
            Dialog::new_device_selection_dialog(devices, response_sender),
        );
    }

    fn show_file_selection_dialog(
        &self,
        webview: servo::WebView,
        filter_pattern: Vec<FilterPattern>,
        allow_select_mutiple: bool,
        response_sender: IpcSender<Option<Vec<PathBuf>>>,
    ) {
        let file_dialog =
            Dialog::new_file_dialog(allow_select_mutiple, response_sender, filter_pattern);
        self.add_dialog(webview, file_dialog);
    }

    fn request_permission(&self, webview: servo::WebView, permission_request: PermissionRequest) {
        if self.servoshell_preferences.headless &&
            self.servoshell_preferences.webdriver_port.is_none()
        {
            permission_request.deny();
            return;
        }

        let permission_dialog = Dialog::new_permission_request_dialog(permission_request);
        self.add_dialog(webview, permission_dialog);
    }

    fn notify_new_frame_ready(&self, _webview: servo::WebView) {
        self.inner_mut().need_repaint = true;
    }

    fn play_gamepad_haptic_effect(
        &self,
        _webview: servo::WebView,
        index: usize,
        effect_type: GamepadHapticEffectType,
        effect_complete_sender: IpcSender<bool>,
    ) {
        match self.inner_mut().gamepad_support.as_mut() {
            Some(gamepad_support) => {
                gamepad_support.play_haptic_effect(index, effect_type, effect_complete_sender);
            },
            None => {
                let _ = effect_complete_sender.send(false);
            },
        }
    }

    fn stop_gamepad_haptic_effect(
        &self,
        _webview: servo::WebView,
        index: usize,
        haptic_stop_sender: IpcSender<bool>,
    ) {
        let stopped = match self.inner_mut().gamepad_support.as_mut() {
            Some(gamepad_support) => gamepad_support.stop_haptic_effect(index),
            None => false,
        };
        let _ = haptic_stop_sender.send(stopped);
    }
    fn show_ime(
        &self,
        _webview: WebView,
        input_type: servo::InputMethodType,
        text: Option<(String, i32)>,
        multiline: bool,
        position: servo::webrender_api::units::DeviceIntRect,
    ) {
        self.inner()
            .window
            .show_ime(input_type, text, multiline, position);
    }

    fn hide_ime(&self, _webview: WebView) {
        self.inner().window.hide_ime();
    }

    fn show_form_control(&self, webview: WebView, form_control: FormControl) {
        if self.servoshell_preferences.headless &&
            self.servoshell_preferences.webdriver_port.is_none()
        {
            return;
        }

        match form_control {
            FormControl::SelectElement(prompt) => {
                // FIXME: Reading the toolbar height is needed here to properly position the select dialog.
                // But if the toolbar height changes while the dialog is open then the position won't be updated
                let offset = self.inner().window.toolbar_height();
                self.add_dialog(webview, Dialog::new_select_element_dialog(prompt, offset));
            },
            FormControl::ColorPicker(color_picker) => {
                // FIXME: Reading the toolbar height is needed here to properly position the select dialog.
                // But if the toolbar height changes while the dialog is open then the position won't be updated
                let offset = self.inner().window.toolbar_height();
                self.add_dialog(
                    webview,
                    Dialog::new_color_picker_dialog(color_picker, offset),
                );
            },
        }
    }
}

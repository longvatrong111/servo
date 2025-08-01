/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use std::sync::Arc;
use std::sync::atomic::AtomicBool;
use std::thread::{self, JoinHandle};
use std::time::{Duration, Instant};

use base::id::PipelineId;
use constellation_traits::{
    ScopeThings, ServiceWorkerMsg, WorkerGlobalScopeInit, WorkerScriptLoadOrigin,
};
use crossbeam_channel::{Receiver, Sender, after, unbounded};
use devtools_traits::DevtoolScriptControlMsg;
use dom_struct::dom_struct;
use ipc_channel::ipc::{IpcReceiver, IpcSender};
use ipc_channel::router::ROUTER;
use js::jsapi::{JS_AddInterruptCallback, JSContext};
use js::jsval::UndefinedValue;
use net_traits::request::{
    CredentialsMode, Destination, InsecureRequestsPolicy, ParserMetadata, Referrer, RequestBuilder,
};
use net_traits::{CustomResponseMediator, IpcSend};
use servo_config::pref;
use servo_rand::random;
use servo_url::ServoUrl;
use style::thread_state::{self, ThreadState};

use crate::devtools;
use crate::dom::abstractworker::WorkerScriptMsg;
use crate::dom::abstractworkerglobalscope::{WorkerEventLoopMethods, run_worker_event_loop};
use crate::dom::bindings::codegen::Bindings::ServiceWorkerGlobalScopeBinding;
use crate::dom::bindings::codegen::Bindings::ServiceWorkerGlobalScopeBinding::ServiceWorkerGlobalScopeMethods;
use crate::dom::bindings::codegen::Bindings::WorkerBinding::WorkerType;
use crate::dom::bindings::inheritance::Castable;
use crate::dom::bindings::root::{DomRoot, RootCollection, ThreadLocalStackRoots};
use crate::dom::bindings::str::DOMString;
use crate::dom::bindings::structuredclone;
use crate::dom::bindings::trace::CustomTraceable;
use crate::dom::bindings::utils::define_all_exposed_interfaces;
use crate::dom::csp::Violation;
use crate::dom::dedicatedworkerglobalscope::AutoWorkerReset;
use crate::dom::event::Event;
use crate::dom::eventtarget::EventTarget;
use crate::dom::extendableevent::ExtendableEvent;
use crate::dom::extendablemessageevent::ExtendableMessageEvent;
use crate::dom::globalscope::GlobalScope;
#[cfg(feature = "webgpu")]
use crate::dom::webgpu::identityhub::IdentityHub;
use crate::dom::worker::TrustedWorkerAddress;
use crate::dom::workerglobalscope::WorkerGlobalScope;
use crate::fetch::{CspViolationsProcessor, load_whole_resource};
use crate::messaging::{CommonScriptMsg, ScriptEventLoopSender};
use crate::realms::{AlreadyInRealm, InRealm, enter_realm};
use crate::script_runtime::{CanGc, JSContext as SafeJSContext, Runtime, ThreadSafeJSContext};
use crate::task_queue::{QueuedTask, QueuedTaskConversion, TaskQueue};
use crate::task_source::TaskSourceName;

/// Messages used to control service worker event loop
pub(crate) enum ServiceWorkerScriptMsg {
    /// Message common to all workers
    CommonWorker(WorkerScriptMsg),
    /// Message to request a custom response by the service worker
    Response(CustomResponseMediator),
    /// Wake-up call from the task queue.
    WakeUp,
}

impl QueuedTaskConversion for ServiceWorkerScriptMsg {
    fn task_source_name(&self) -> Option<&TaskSourceName> {
        let script_msg = match self {
            ServiceWorkerScriptMsg::CommonWorker(WorkerScriptMsg::Common(script_msg)) => script_msg,
            _ => return None,
        };
        match script_msg {
            CommonScriptMsg::Task(_category, _boxed, _pipeline_id, task_source) => {
                Some(task_source)
            },
            _ => None,
        }
    }

    fn pipeline_id(&self) -> Option<PipelineId> {
        // Workers always return None, since the pipeline_id is only used to check for document activity,
        // and this check does not apply to worker event-loops.
        None
    }

    fn into_queued_task(self) -> Option<QueuedTask> {
        let script_msg = match self {
            ServiceWorkerScriptMsg::CommonWorker(WorkerScriptMsg::Common(script_msg)) => script_msg,
            _ => return None,
        };
        let (category, boxed, pipeline_id, task_source) = match script_msg {
            CommonScriptMsg::Task(category, boxed, pipeline_id, task_source) => {
                (category, boxed, pipeline_id, task_source)
            },
            _ => return None,
        };
        Some((None, category, boxed, pipeline_id, task_source))
    }

    fn from_queued_task(queued_task: QueuedTask) -> Self {
        let (_worker, category, boxed, pipeline_id, task_source) = queued_task;
        let script_msg = CommonScriptMsg::Task(category, boxed, pipeline_id, task_source);
        ServiceWorkerScriptMsg::CommonWorker(WorkerScriptMsg::Common(script_msg))
    }

    fn inactive_msg() -> Self {
        // Inactive is only relevant in the context of a browsing-context event-loop.
        panic!("Workers should never receive messages marked as inactive");
    }

    fn wake_up_msg() -> Self {
        ServiceWorkerScriptMsg::WakeUp
    }

    fn is_wake_up(&self) -> bool {
        matches!(self, ServiceWorkerScriptMsg::WakeUp)
    }
}

/// Messages sent from the owning registration.
pub(crate) enum ServiceWorkerControlMsg {
    /// Shutdown.
    Exit,
}

pub(crate) enum MixedMessage {
    ServiceWorker(ServiceWorkerScriptMsg),
    Devtools(DevtoolScriptControlMsg),
    Control(ServiceWorkerControlMsg),
    Timer,
}

struct ServiceWorkerCspProcessor {}

impl CspViolationsProcessor for ServiceWorkerCspProcessor {
    fn process_csp_violations(&self, _violations: Vec<Violation>) {}
}

#[dom_struct]
pub(crate) struct ServiceWorkerGlobalScope {
    workerglobalscope: WorkerGlobalScope,

    #[ignore_malloc_size_of = "Defined in std"]
    #[no_trace]
    task_queue: TaskQueue<ServiceWorkerScriptMsg>,

    own_sender: Sender<ServiceWorkerScriptMsg>,

    /// A port on which a single "time-out" message can be received,
    /// indicating the sw should stop running,
    /// while still draining the task-queue
    // and running all enqueued, and not cancelled, tasks.
    #[ignore_malloc_size_of = "Defined in std"]
    #[no_trace]
    time_out_port: Receiver<Instant>,

    #[ignore_malloc_size_of = "Defined in std"]
    #[no_trace]
    swmanager_sender: IpcSender<ServiceWorkerMsg>,

    #[no_trace]
    scope_url: ServoUrl,

    /// A receiver of control messages,
    /// currently only used to signal shutdown.
    #[ignore_malloc_size_of = "Channels are hard"]
    #[no_trace]
    control_receiver: Receiver<ServiceWorkerControlMsg>,
}

impl WorkerEventLoopMethods for ServiceWorkerGlobalScope {
    type WorkerMsg = ServiceWorkerScriptMsg;
    type ControlMsg = ServiceWorkerControlMsg;
    type Event = MixedMessage;

    fn task_queue(&self) -> &TaskQueue<ServiceWorkerScriptMsg> {
        &self.task_queue
    }

    fn handle_event(&self, event: MixedMessage, can_gc: CanGc) -> bool {
        self.handle_mixed_message(event, can_gc)
    }

    fn handle_worker_post_event(&self, _worker: &TrustedWorkerAddress) -> Option<AutoWorkerReset> {
        None
    }

    fn from_control_msg(msg: ServiceWorkerControlMsg) -> MixedMessage {
        MixedMessage::Control(msg)
    }

    fn from_worker_msg(msg: ServiceWorkerScriptMsg) -> MixedMessage {
        MixedMessage::ServiceWorker(msg)
    }

    fn from_devtools_msg(msg: DevtoolScriptControlMsg) -> MixedMessage {
        MixedMessage::Devtools(msg)
    }

    fn from_timer_msg() -> MixedMessage {
        MixedMessage::Timer
    }

    fn control_receiver(&self) -> &Receiver<ServiceWorkerControlMsg> {
        &self.control_receiver
    }
}

impl ServiceWorkerGlobalScope {
    #[allow(clippy::too_many_arguments)]
    fn new_inherited(
        init: WorkerGlobalScopeInit,
        worker_url: ServoUrl,
        from_devtools_receiver: Receiver<DevtoolScriptControlMsg>,
        runtime: Runtime,
        own_sender: Sender<ServiceWorkerScriptMsg>,
        receiver: Receiver<ServiceWorkerScriptMsg>,
        time_out_port: Receiver<Instant>,
        swmanager_sender: IpcSender<ServiceWorkerMsg>,
        scope_url: ServoUrl,
        control_receiver: Receiver<ServiceWorkerControlMsg>,
        closing: Arc<AtomicBool>,
    ) -> ServiceWorkerGlobalScope {
        ServiceWorkerGlobalScope {
            workerglobalscope: WorkerGlobalScope::new_inherited(
                init,
                DOMString::new(),
                WorkerType::Classic, // FIXME(cybai): Should be provided from `Run Service Worker`
                worker_url,
                runtime,
                from_devtools_receiver,
                closing,
                #[cfg(feature = "webgpu")]
                Arc::new(IdentityHub::default()),
                InsecureRequestsPolicy::DoNotUpgrade, // FIXME: investigate what environment this value comes from for
                                                      // service workers.
            ),
            task_queue: TaskQueue::new(receiver, own_sender.clone()),
            own_sender,
            time_out_port,
            swmanager_sender,
            scope_url,
            control_receiver,
        }
    }

    #[allow(unsafe_code, clippy::too_many_arguments)]
    pub(crate) fn new(
        init: WorkerGlobalScopeInit,
        worker_url: ServoUrl,
        from_devtools_receiver: Receiver<DevtoolScriptControlMsg>,
        runtime: Runtime,
        own_sender: Sender<ServiceWorkerScriptMsg>,
        receiver: Receiver<ServiceWorkerScriptMsg>,
        time_out_port: Receiver<Instant>,
        swmanager_sender: IpcSender<ServiceWorkerMsg>,
        scope_url: ServoUrl,
        control_receiver: Receiver<ServiceWorkerControlMsg>,
        closing: Arc<AtomicBool>,
    ) -> DomRoot<ServiceWorkerGlobalScope> {
        let cx = runtime.cx();
        let scope = Box::new(ServiceWorkerGlobalScope::new_inherited(
            init,
            worker_url,
            from_devtools_receiver,
            runtime,
            own_sender,
            receiver,
            time_out_port,
            swmanager_sender,
            scope_url,
            control_receiver,
            closing,
        ));
        unsafe {
            ServiceWorkerGlobalScopeBinding::Wrap::<crate::DomTypeHolder>(
                SafeJSContext::from_ptr(cx),
                scope,
            )
        }
    }

    /// <https://w3c.github.io/ServiceWorker/#run-service-worker-algorithm>
    #[allow(unsafe_code, clippy::too_many_arguments)]
    pub(crate) fn run_serviceworker_scope(
        scope_things: ScopeThings,
        own_sender: Sender<ServiceWorkerScriptMsg>,
        receiver: Receiver<ServiceWorkerScriptMsg>,
        devtools_receiver: IpcReceiver<DevtoolScriptControlMsg>,
        swmanager_sender: IpcSender<ServiceWorkerMsg>,
        scope_url: ServoUrl,
        control_receiver: Receiver<ServiceWorkerControlMsg>,
        context_sender: Sender<ThreadSafeJSContext>,
        closing: Arc<AtomicBool>,
    ) -> JoinHandle<()> {
        let ScopeThings {
            script_url,
            init,
            worker_load_origin,
            ..
        } = scope_things;

        let serialized_worker_url = script_url.to_string();
        let origin = scope_url.origin();
        thread::Builder::new()
            .name(format!("SW:{}", script_url.debug_compact()))
            .spawn(move || {
                thread_state::initialize(ThreadState::SCRIPT | ThreadState::IN_WORKER);
                let runtime = Runtime::new(None);
                let context_for_interrupt = runtime.thread_safe_js_context();
                let _ = context_sender.send(context_for_interrupt);

                let roots = RootCollection::new();
                let _stack_roots = ThreadLocalStackRoots::new(&roots);

                let WorkerScriptLoadOrigin {
                    referrer_url,
                    referrer_policy,
                    pipeline_id,
                } = worker_load_origin;

                // Service workers are time limited
                // https://w3c.github.io/ServiceWorker/#service-worker-lifetime
                let sw_lifetime_timeout = pref!(dom_serviceworker_timeout_seconds) as u64;
                let time_out_port = after(Duration::new(sw_lifetime_timeout, 0));

                let (devtools_mpsc_chan, devtools_mpsc_port) = unbounded();
                ROUTER
                    .route_ipc_receiver_to_crossbeam_sender(devtools_receiver, devtools_mpsc_chan);

                let resource_threads_sender = init.resource_threads.sender();
                let global = ServiceWorkerGlobalScope::new(
                    init,
                    script_url.clone(),
                    devtools_mpsc_port,
                    runtime,
                    own_sender,
                    receiver,
                    time_out_port,
                    swmanager_sender,
                    scope_url,
                    control_receiver,
                    closing,
                );

                let scope = global.upcast::<WorkerGlobalScope>();

                let referrer = referrer_url
                    .map(Referrer::ReferrerUrl)
                    .unwrap_or_else(|| global.upcast::<GlobalScope>().get_referrer());

                let request = RequestBuilder::new(None, script_url, referrer)
                    .destination(Destination::ServiceWorker)
                    .credentials_mode(CredentialsMode::Include)
                    .parser_metadata(ParserMetadata::NotParserInserted)
                    .use_url_credentials(true)
                    .pipeline_id(Some(pipeline_id))
                    .referrer_policy(referrer_policy)
                    .insecure_requests_policy(scope.insecure_requests_policy())
                    .origin(origin);

                let (_url, source) = match load_whole_resource(
                    request,
                    &resource_threads_sender,
                    global.upcast(),
                    &ServiceWorkerCspProcessor {},
                    CanGc::note(),
                ) {
                    Err(_) => {
                        println!("error loading script {}", serialized_worker_url);
                        scope.clear_js_runtime();
                        return;
                    },
                    Ok((metadata, bytes)) => {
                        (metadata.final_url, String::from_utf8(bytes).unwrap())
                    },
                };

                unsafe {
                    // Handle interrupt requests
                    JS_AddInterruptCallback(*scope.get_cx(), Some(interrupt_callback));
                }

                {
                    // TODO: use AutoWorkerReset as in dedicated worker?
                    let realm = enter_realm(scope);
                    define_all_exposed_interfaces(
                        scope.upcast(),
                        InRealm::entered(&realm),
                        CanGc::note(),
                    );
                    scope.execute_script(DOMString::from(source), CanGc::note());
                    global.dispatch_activate(CanGc::note(), InRealm::entered(&realm));
                }

                let reporter_name = format!("service-worker-reporter-{}", random::<u64>());
                scope
                    .upcast::<GlobalScope>()
                    .mem_profiler_chan()
                    .run_with_memory_reporting(
                        || {
                            // Step 18, Run the responsible event loop specified
                            // by inside settings until it is destroyed.
                            // The worker processing model remains on this step
                            // until the event loop is destroyed,
                            // which happens after the closing flag is set to true,
                            // or until the worker has run beyond its allocated time.
                            while !scope.is_closing() && !global.has_timed_out() {
                                run_worker_event_loop(&*global, None, CanGc::note());
                            }
                        },
                        reporter_name,
                        global.event_loop_sender(),
                        CommonScriptMsg::CollectReports,
                    );

                scope.clear_js_runtime();
            })
            .expect("Thread spawning failed")
    }

    fn handle_mixed_message(&self, msg: MixedMessage, can_gc: CanGc) -> bool {
        match msg {
            MixedMessage::Devtools(msg) => match msg {
                DevtoolScriptControlMsg::EvaluateJS(_pipe_id, string, sender) => {
                    devtools::handle_evaluate_js(self.upcast(), string, sender, can_gc)
                },
                DevtoolScriptControlMsg::WantsLiveNotifications(_pipe_id, bool_val) => {
                    devtools::handle_wants_live_notifications(self.upcast(), bool_val)
                },
                _ => debug!("got an unusable devtools control message inside the worker!"),
            },
            MixedMessage::ServiceWorker(msg) => {
                self.handle_script_event(msg, can_gc);
            },
            MixedMessage::Control(ServiceWorkerControlMsg::Exit) => {
                return false;
            },
            MixedMessage::Timer => {},
        }
        true
    }

    fn has_timed_out(&self) -> bool {
        // TODO: https://w3c.github.io/ServiceWorker/#service-worker-lifetime
        false
    }

    fn handle_script_event(&self, msg: ServiceWorkerScriptMsg, can_gc: CanGc) {
        use self::ServiceWorkerScriptMsg::*;

        match msg {
            CommonWorker(WorkerScriptMsg::DOMMessage { data, .. }) => {
                let scope = self.upcast::<WorkerGlobalScope>();
                let target = self.upcast();
                let _ac = enter_realm(scope);
                rooted!(in(*scope.get_cx()) let mut message = UndefinedValue());
                if let Ok(ports) =
                    structuredclone::read(scope.upcast(), *data, message.handle_mut())
                {
                    ExtendableMessageEvent::dispatch_jsval(
                        target,
                        scope.upcast(),
                        message.handle(),
                        ports,
                        can_gc,
                    );
                } else {
                    ExtendableMessageEvent::dispatch_error(target, scope.upcast(), can_gc);
                }
            },
            CommonWorker(WorkerScriptMsg::Common(msg)) => {
                self.upcast::<WorkerGlobalScope>().process_event(msg);
            },
            Response(mediator) => {
                // TODO XXXcreativcoder This will eventually use a FetchEvent interface to fire event
                // when we have the Request and Response dom api's implemented
                // https://w3c.github.io/ServiceWorker/#fetchevent-interface
                self.upcast::<EventTarget>()
                    .fire_event(atom!("fetch"), can_gc);
                let _ = mediator.response_chan.send(None);
            },
            WakeUp => {},
        }
    }

    pub(crate) fn event_loop_sender(&self) -> ScriptEventLoopSender {
        ScriptEventLoopSender::ServiceWorker(self.own_sender.clone())
    }

    fn dispatch_activate(&self, can_gc: CanGc, _realm: InRealm) {
        let event = ExtendableEvent::new(self, atom!("activate"), false, false, can_gc);
        let event = (*event).upcast::<Event>();
        self.upcast::<EventTarget>().dispatch_event(event, can_gc);
    }
}

#[allow(unsafe_code)]
unsafe extern "C" fn interrupt_callback(cx: *mut JSContext) -> bool {
    let in_realm_proof = AlreadyInRealm::assert_for_cx(SafeJSContext::from_ptr(cx));
    let global = GlobalScope::from_context(cx, InRealm::Already(&in_realm_proof));
    let worker =
        DomRoot::downcast::<WorkerGlobalScope>(global).expect("global is not a worker scope");
    assert!(worker.is::<ServiceWorkerGlobalScope>());

    // A false response causes the script to terminate
    !worker.is_closing()
}

impl ServiceWorkerGlobalScopeMethods<crate::DomTypeHolder> for ServiceWorkerGlobalScope {
    // https://w3c.github.io/ServiceWorker/#dom-serviceworkerglobalscope-onmessage
    event_handler!(message, GetOnmessage, SetOnmessage);

    // https://w3c.github.io/ServiceWorker/#dom-serviceworkerglobalscope-onmessageerror
    event_handler!(messageerror, GetOnmessageerror, SetOnmessageerror);
}

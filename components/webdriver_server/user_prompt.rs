/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use std::collections::HashMap;

use base::id::WebViewId;
use embedder_traits::{WebDriverCommandMsg, WebDriverUserPromptAction};
use ipc_channel::ipc;
use serde_json::{Map, Value};
use webdriver::error::{ErrorStatus, WebDriverError, WebDriverResult};
use webdriver::response::{ValueResponse, WebDriverResponse};

use crate::{Handler, wait_for_script_response};

const KNOWN_PROMPT_HANDLERS: [&str; 5] = [
    "dismiss",
    "accept",
    "dismiss and notify",
    "accept and notify",
    "ignore",
];

const VALID_PROMPT_TYPES: [&str; 6] = [
    "alert",
    "beforeUnload",
    "confirm",
    "default",
    "file",
    "prompt",
];

/// <https://w3c.github.io/webdriver/#dfn-prompt-handler-configuration>
pub(crate) struct PromptHandlerConfiguration {
    handler: String,
    notify: bool,
}

pub(crate) type UserPromptHandler = HashMap<String, PromptHandlerConfiguration>;

/// <https://w3c.github.io/webdriver/#dfn-deserialize-as-an-unhandled-prompt-behavior>
pub(crate) fn deserialize_unhandled_prompt_behaviour(
    value_param: Value,
) -> Result<UserPromptHandler, WebDriverError> {
    // Step 2-5.
    let (value, is_string_value) = match value_param {
        Value::Object(map) => (map, false),
        Value::String(..) => {
            let mut map = Map::new();
            map.insert("fallbackDefault".to_string(), value_param);
            (map, true)
        },
        _ => {
            return Err(WebDriverError::new(
                ErrorStatus::InvalidArgument,
                "Expected an object or a string for unhandled prompt behavior.",
            ));
        },
    };

    // Step 6. Let user prompt handler be a new empty map.
    let mut user_prompt_handler = UserPromptHandler::new();

    // Step 7. For each key-value pair in value:
    for (prompt_type, handler) in value {
        // Step 7.1. If `is_string_value` is false and prompt type is not one of
        // the valid prompt types, return error with error code invalid argument.
        if !is_string_value && !VALID_PROMPT_TYPES.contains(&prompt_type.as_str()) {
            return Err(WebDriverError::new(
                ErrorStatus::InvalidArgument,
                format!("Invalid prompt type: {}", prompt_type),
            ));
        }

        // Step 7.2. If known prompt handlers does not contain an entry with
        // handler key `handler` return error with error code invalid argument.
        if !KNOWN_PROMPT_HANDLERS.contains(&handler.to_string().as_str()) {
            return Err(WebDriverError::new(
                ErrorStatus::InvalidArgument,
                format!("Unknown prompt handler: {}", handler),
            ));
        }

        // Step 7.3 - 7.6.
        let (handler, notify) = match handler.to_string().as_str() {
            "accept and notify" => ("accept", true),
            "dismiss and notify" => ("dismiss", true),
            "ignore" => ("ignore", false),
            "accept" => ("accept", false),
            "dismiss" => ("dismiss", false),
            _ => unreachable!(),
        };

        // Step 7.7 - 7.8.
        user_prompt_handler.insert(
            prompt_type,
            PromptHandlerConfiguration {
                handler: handler.to_string(),
                notify,
            },
        );
    }

    Ok(user_prompt_handler)
}

pub(crate) fn default_unhandled_prompt_behavior() -> String {
    "dismiss and notify".to_string()
}

fn get_user_prompt_handler(
    user_prompt_handler: &UserPromptHandler,
    prompt_type: &str,
) {
    let before_unload_res = PromptHandlerConfiguration {
        handler: "accept".to_string(),
        notify: false,
    };

    let default_res = PromptHandlerConfiguration {
        handler: "dismiss".to_string(),
        notify: true,
    };

    let x = user_prompt_handler.get(prompt_type).or_else(|| {
        user_prompt_handler
            .get("default")
            .or_else(|| user_prompt_handler.get("fallbackDefault"))
    });
}

impl Handler {
    /// <https://w3c.github.io/webdriver/#dismiss-alert>
    pub(crate) fn handle_dismiss_alert(&mut self) -> WebDriverResult<WebDriverResponse> {
        // Step 1. If session's current top-level browsing context is no longer open,
        // return error with error code no such window.
        self.verify_top_level_browsing_context_is_open(self.session()?.webview_id)?;

        // Step 3. Dismiss the current user prompt.
        let (sender, receiver) = ipc::channel().unwrap();
        self.send_message_to_embedder(WebDriverCommandMsg::HandleUserPrompt(
            self.verified_webview_id(),
            WebDriverUserPromptAction::Dismiss,
            sender,
        ))?;

        match wait_for_script_response(receiver)? {
            // Step 2. If the current user prompt is null, return error with error code no such alert.
            Err(()) => Err(WebDriverError::new(
                ErrorStatus::NoSuchAlert,
                "No user prompt is currently active.",
            )),
            // Step 4. Return success with data null.
            Ok(()) => Ok(WebDriverResponse::Void),
        }
    }

    /// <https://w3c.github.io/webdriver/#accept-alert>
    pub(crate) fn handle_accept_alert(&mut self) -> WebDriverResult<WebDriverResponse> {
        // Step 1. If session's current top-level browsing context is no longer open,
        // return error with error code no such window.
        self.verify_top_level_browsing_context_is_open(self.session()?.webview_id)?;

        // Step 3. Accept the current user prompt.
        let (sender, receiver) = ipc::channel().unwrap();
        self.send_message_to_embedder(WebDriverCommandMsg::HandleUserPrompt(
            self.verified_webview_id(),
            WebDriverUserPromptAction::Accept,
            sender,
        ))?;

        match wait_for_script_response(receiver)? {
            // Step 2. If the current user prompt is null, return error with error code no such alert.
            Err(()) => Err(WebDriverError::new(
                ErrorStatus::NoSuchAlert,
                "No user prompt is currently active.",
            )),
            // Step 4. Return success with data null.
            Ok(()) => Ok(WebDriverResponse::Void),
        }
    }

    pub(crate) fn handle_get_alert_text(&mut self) -> WebDriverResult<WebDriverResponse> {
        // Step 1. If session's current top-level browsing context is no longer open,
        // return error with error code no such window.
        self.verify_top_level_browsing_context_is_open(self.session()?.webview_id)?;

        let (sender, receiver) = ipc::channel().unwrap();
        self.send_message_to_embedder(WebDriverCommandMsg::GetAlertText(
            self.verified_webview_id(),
            sender,
        ))?;

        match wait_for_script_response(receiver)? {
            // Step 2. If the current user prompt is null, return error with error code no such alert.
            Err(()) => Err(WebDriverError::new(
                ErrorStatus::NoSuchAlert,
                "No user prompt is currently active.",
            )),
            // Step 3. Let message be the text message associated with the current user prompt
            // or otherwise be null
            // Step 4. Return success with data message.
            Ok(message) => Ok(WebDriverResponse::Generic(ValueResponse(
                serde_json::to_value(message).map_err(|e| {
                    WebDriverError::new(
                        ErrorStatus::UnknownError,
                        format!("Failed to serialize alert text: {}", e),
                    )
                })?,
            ))),
        }
    }

    /// <https://w3c.github.io/webdriver/#dfn-handle-any-user-prompts>
    pub(crate) fn handle_any_user_prompt(&self, webview_id: WebViewId) {
        let (sender, _) = ipc::channel().unwrap();

        let user_prompt_handler = &self.session().unwrap().user_prompt_handler;
        let prompt_type = VALID_PROMPT_TYPES[0];

        // let handle_action = match

        let _ = self.send_message_to_embedder(WebDriverCommandMsg::HandleUserPrompt(
            webview_id,
            WebDriverUserPromptAction::Accept, // Default action
            sender,
        ));
    }
}

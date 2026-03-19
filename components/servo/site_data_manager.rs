/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use bitflags::bitflags;
use cookie::Cookie;
use log::warn;
use net_traits::pub_domains::registered_domain_name;
use net_traits::{CookieSource, ResourceThreads, SiteDescriptor};
use rustc_hash::FxHashMap;
use servo_url::ServoUrl;
use storage_traits::StorageThreads;
use storage_traits::webstorage_thread::{OriginDescriptor, WebStorageType};
use time::Duration as TimeDuration;

// ── CookieInfo ───────────────────────────────────────────────────────────────

/// A snapshot of a single HTTP cookie, suitable for inspection and modification
/// by embedder applications.
///
/// Build one with [`CookieInfo::new`], then set the optional attributes before
/// passing the value to [`SiteDataManager::set_cookie`].
///
/// # Example
///
/// ```rust,ignore
/// let cookie = CookieInfo::new("session", "abc123")
///     .with_domain("example.com")
///     .with_path("/")
///     .with_secure(true)
///     .with_max_age_secs(3600);
///
/// webview.set_cookie(&url, cookie);
/// ```
#[derive(Clone, Debug, PartialEq)]
pub struct CookieInfo {
    /// Cookie name.
    pub name: String,
    /// Cookie value.
    pub value: String,
    /// Domain scope, e.g. `"example.com"`. `None` = use the URL's host.
    pub domain: Option<String>,
    /// URL path scope, e.g. `"/"`. `None` = use the URL's path.
    pub path: Option<String>,
    /// Cookie must only be sent over HTTPS.
    pub secure: bool,
    /// Cookie is inaccessible from JavaScript.
    pub http_only: bool,
    /// `SameSite` attribute: `"Strict"`, `"Lax"`, or `"None"`.
    /// `None` here means the attribute was not set.
    pub same_site: Option<String>,
    /// Maximum lifetime in seconds. Negative = expired immediately.
    /// `None` means the cookie is a session cookie.
    pub max_age_secs: Option<i64>,
}

impl CookieInfo {
    /// Create a new session cookie with `name` and `value`.
    /// All other attributes default to their most permissive values.
    pub fn new(name: impl Into<String>, value: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            value: value.into(),
            domain: None,
            path: None,
            secure: false,
            http_only: false,
            same_site: None,
            max_age_secs: None,
        }
    }

    /// Set the domain scope.
    pub fn with_domain(mut self, domain: impl Into<String>) -> Self {
        self.domain = Some(domain.into());
        self
    }

    /// Set the path scope.
    pub fn with_path(mut self, path: impl Into<String>) -> Self {
        self.path = Some(path.into());
        self
    }

    /// Require HTTPS for transmission.
    pub fn with_secure(mut self, secure: bool) -> Self {
        self.secure = secure;
        self
    }

    /// Hide the cookie from JavaScript.
    pub fn with_http_only(mut self, http_only: bool) -> Self {
        self.http_only = http_only;
        self
    }

    /// Set the `SameSite` attribute (`"Strict"`, `"Lax"`, or `"None"`).
    pub fn with_same_site(mut self, policy: impl Into<String>) -> Self {
        self.same_site = Some(policy.into());
        self
    }

    /// Set the maximum lifetime in seconds.
    pub fn with_max_age_secs(mut self, secs: i64) -> Self {
        self.max_age_secs = Some(secs);
        self
    }
}

impl From<Cookie<'static>> for CookieInfo {
    fn from(c: Cookie<'static>) -> Self {
        CookieInfo {
            name: c.name().to_owned(),
            value: c.value().to_owned(),
            domain: c.domain().map(str::to_owned),
            path: c.path().map(str::to_owned),
            secure: c.secure().unwrap_or(false),
            http_only: c.http_only().unwrap_or(false),
            same_site: c.same_site().map(|ss| ss.to_string()),
            max_age_secs: c.max_age().map(|d| d.whole_seconds()),
        }
    }
}

impl From<CookieInfo> for Cookie<'static> {
    fn from(info: CookieInfo) -> Cookie<'static> {
        let mut b = Cookie::build((info.name, info.value));
        if let Some(d) = info.domain {
            b = b.domain(d);
        }
        if let Some(p) = info.path {
            b = b.path(p);
        }
        b = b.secure(info.secure).http_only(info.http_only);
        if let Some(ss) = info.same_site.as_deref() {
            let policy = match ss {
                "Strict" => cookie::SameSite::Strict,
                "None" => cookie::SameSite::None,
                _ => cookie::SameSite::Lax,
            };
            b = b.same_site(policy);
        }
        if let Some(secs) = info.max_age_secs {
            b = b.max_age(TimeDuration::seconds(secs));
        }
        b.build()
    }
}

// ── StorageType ───────────────────────────────────────────────────────────────

bitflags! {
    /// Identifies categories of site data associated with a site.
    ///
    /// This type is used by `SiteDataManager` to query, describe, and manage
    /// different kinds of data stored by the user agent for a given site.
    ///
    /// Additional storage categories (e.g. IndexedDB) may be added in the
    /// future.
    #[derive(Clone, Copy, Debug, PartialEq)]
    pub struct StorageType: u8 {
        /// Corresponds to the HTTP cookies:
        /// <https://developer.mozilla.org/en-US/docs/Web/HTTP/Guides/Cookies>
        const Cookies = 1 << 0;

        /// Corresponds to the `localStorage` Web API:
        /// <https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage>
        const Local   = 1 << 1;

        /// Corresponds to the `sessionStorage` Web API:
        /// <https://developer.mozilla.org/en-US/docs/Web/API/Window/sessionStorage>
        const Session = 1 << 2;
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SiteData {
    name: String,
    storage_types: StorageType,
}

impl SiteData {
    pub fn new(name: impl Into<String>, storage_types: StorageType) -> SiteData {
        SiteData {
            name: name.into(),
            storage_types,
        }
    }

    pub fn name(&self) -> String {
        self.name.clone()
    }

    pub fn storage_types(&self) -> StorageType {
        self.storage_types
    }
}

/// Provides APIs for inspecting and managing site data.
///
/// `SiteDataManager` exposes information about data that is conceptually
/// associated with a site (equivalent to an eTLD+1), such as web exposed
/// storage mechanisms like `localStorage` and `sessionStorage`.
///
/// The manager can be used by embedders to list sites with stored data.
/// Support for site scoped management operations (e.g. clearing data for a
/// specific site) will be added in the future.
///
/// Note: Network layer state (such as the HTTP cache) is intentionally not
/// handled by `SiteDataManager`. That functionality lives in `NetworkManager`.
#[derive(Clone)]
pub struct SiteDataManager {
    public_resource_threads: ResourceThreads,
    private_resource_threads: ResourceThreads,
    public_storage_threads: StorageThreads,
    private_storage_threads: StorageThreads,
}

impl SiteDataManager {
    pub(crate) fn new(
        public_resource_threads: ResourceThreads,
        private_resource_threads: ResourceThreads,
        public_storage_threads: StorageThreads,
        private_storage_threads: StorageThreads,
    ) -> Self {
        Self {
            public_resource_threads,
            private_resource_threads,
            public_storage_threads,
            private_storage_threads,
        }
    }

    /// Return a list of sites that have associated site data.
    ///
    /// The returned list is filtered by the provided `storage_types` bitflags.
    /// Each [`SiteData`] entry represents a site (equivalent to an eTLD+1)
    /// and indicates which kinds of storage data are present for it (e.g.
    /// localStorage, sessionStorage).
    ///
    /// The returned list is sorted by site name.
    ///
    /// Both public and private storage are included in the result.
    pub fn site_data(&self, storage_types: StorageType) -> Vec<SiteData> {
        let mut all_sites: FxHashMap<String, StorageType> = FxHashMap::default();

        let mut add_sites = |sites: Vec<SiteDescriptor>, storage_type: StorageType| {
            for site in sites {
                all_sites
                    .entry(site.name)
                    .and_modify(|storage_types| *storage_types |= storage_type)
                    .or_insert(storage_type);
            }
        };

        if storage_types.contains(StorageType::Cookies) {
            let public_cookies = self.public_resource_threads.cookies();
            add_sites(public_cookies, StorageType::Cookies);

            let private_cookies = self.private_resource_threads.cookies();
            add_sites(private_cookies, StorageType::Cookies);
        }

        let mut add_origins = |origins: Vec<OriginDescriptor>, storage_type: StorageType| {
            for origin in origins {
                let url =
                    ServoUrl::parse(&origin.name).expect("Should always be able to parse origins.");

                let Some(domain) = registered_domain_name(&url) else {
                    warn!("Failed to get a registered domain name for: {url}.");
                    continue;
                };
                let domain = domain.to_string();

                all_sites
                    .entry(domain)
                    .and_modify(|storage_types| *storage_types |= storage_type)
                    .or_insert(storage_type);
            }
        };

        if storage_types.contains(StorageType::Local) {
            let public_origins = self
                .public_storage_threads
                .webstorage_origins(WebStorageType::Local);
            add_origins(public_origins, StorageType::Local);

            let private_origins = self
                .private_storage_threads
                .webstorage_origins(WebStorageType::Local);
            add_origins(private_origins, StorageType::Local);
        }

        if storage_types.contains(StorageType::Session) {
            let public_origins = self
                .public_storage_threads
                .webstorage_origins(WebStorageType::Session);
            add_origins(public_origins, StorageType::Session);

            let private_origins = self
                .private_storage_threads
                .webstorage_origins(WebStorageType::Session);
            add_origins(private_origins, StorageType::Session);
        }

        let mut result: Vec<SiteData> = all_sites
            .into_iter()
            .map(|(name, storage_types)| SiteData::new(name, storage_types))
            .collect();

        result.sort_by_key(SiteData::name);

        result
    }

    /// Clear site data for the given sites.
    ///
    /// The clearing is restricted to the provided `storage_types` bitflags.
    /// Both public and private browsing data are affected.
    pub fn clear_site_data(&self, sites: &[&str], storage_types: StorageType) {
        if storage_types.contains(StorageType::Cookies) {
            self.public_resource_threads.clear_cookies_for_sites(sites);
            self.private_resource_threads.clear_cookies_for_sites(sites);
        }

        if storage_types.contains(StorageType::Local) {
            self.public_storage_threads
                .clear_webstorage_for_sites(WebStorageType::Local, sites);
            self.private_storage_threads
                .clear_webstorage_for_sites(WebStorageType::Local, sites);
        }

        if storage_types.contains(StorageType::Session) {
            self.public_storage_threads
                .clear_webstorage_for_sites(WebStorageType::Session, sites);
            self.private_storage_threads
                .clear_webstorage_for_sites(WebStorageType::Session, sites);
        }
    }

    pub fn clear_cookies(&self) {
        self.public_resource_threads.clear_cookies();
        self.private_resource_threads.clear_cookies();
    }

    // ── Per-URL cookie access ─────────────────────────────────────────────────

    /// Return all cookies applicable to `url` from the public browsing store.
    ///
    /// **Blocks** the calling thread until the networking thread replies.
    pub fn get_cookies(&self, url: &ServoUrl) -> Vec<CookieInfo> {
        self.public_resource_threads
            .get_cookies_for_url(url.clone(), CookieSource::HTTP)
            .into_iter()
            .map(CookieInfo::from)
            .collect()
    }

    /// Look up cookies for `url` without blocking the calling thread.
    ///
    /// The `callback` is invoked on a **background thread** once the networking
    /// thread has replied. Use a channel or similar primitive if you need to
    /// bring the result back to the embedder thread.
    pub fn get_cookies_async(
        &self,
        url: ServoUrl,
        callback: impl FnOnce(Vec<CookieInfo>) + Send + 'static,
    ) {
        let threads = self.public_resource_threads.clone();
        std::thread::spawn(move || {
            let cookies = threads
                .get_cookies_for_url(url, CookieSource::HTTP)
                .into_iter()
                .map(CookieInfo::from)
                .collect();
            callback(cookies);
        });
    }

    /// Store a cookie for `url` in the public browsing store. Fire-and-forget.
    pub fn set_cookie(&self, url: &ServoUrl, cookie: CookieInfo) {
        self.public_resource_threads
            .set_cookie(url.clone(), Cookie::from(cookie), CookieSource::HTTP);
    }

    /// Store a cookie for `url` without blocking the calling thread.
    ///
    /// The message is sent on a **background thread**. The optional `callback`
    /// is invoked once the send has completed, which is useful for sequencing
    /// follow-up work (e.g. navigating after setting a session cookie).
    ///
    /// # Example
    ///
    /// ```rust,ignore
    /// let (tx, rx) = std::sync::mpsc::channel();
    /// site_data_manager.set_cookie_async(&url, cookie, move || { let _ = tx.send(()); });
    /// rx.recv().unwrap(); // wait for the cookie to be queued
    /// ```
    pub fn set_cookie_async(
        &self,
        url: ServoUrl,
        cookie: CookieInfo,
        callback: impl FnOnce() + Send + 'static,
    ) {
        let threads = self.public_resource_threads.clone();
        std::thread::spawn(move || {
            threads.set_cookie(url, Cookie::from(cookie), CookieSource::HTTP);
            callback();
        });
    }

    /// Delete the cookie named `name` for `url` from the public browsing store.
    /// Fire-and-forget.
    pub fn delete_cookie(&self, url: &ServoUrl, name: &str) {
        self.public_resource_threads
            .delete_cookie(url.clone(), name.to_owned());
    }
}

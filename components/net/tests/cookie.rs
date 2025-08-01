/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/. */

use std::time::{Duration, SystemTime};

use net::cookie::ServoCookie;
use net::cookie_storage::CookieStorage;
use net_traits::CookieSource;
use servo_url::ServoUrl;
use time::macros::datetime;

#[test]
fn test_domain_match() {
    assert!(ServoCookie::domain_match("foo.com", "foo.com"));
    assert!(ServoCookie::domain_match("bar.foo.com", "foo.com"));
    assert!(ServoCookie::domain_match("baz.bar.foo.com", "foo.com"));

    assert!(!ServoCookie::domain_match("bar.foo.com", "bar.com"));
    assert!(!ServoCookie::domain_match("bar.com", "baz.bar.com"));
    assert!(!ServoCookie::domain_match("foo.com", "bar.com"));

    assert!(!ServoCookie::domain_match("bar.com", "bbar.com"));
    assert!(ServoCookie::domain_match("235.132.2.3", "235.132.2.3"));
    assert!(!ServoCookie::domain_match("235.132.2.3", "1.1.1.1"));
    assert!(!ServoCookie::domain_match("235.132.2.3", ".2.3"));
}

#[test]
fn test_path_match() {
    assert!(ServoCookie::path_match("/", "/"));
    assert!(ServoCookie::path_match("/index.html", "/"));
    assert!(ServoCookie::path_match("/w/index.html", "/"));
    assert!(ServoCookie::path_match("/w/index.html", "/w/index.html"));
    assert!(ServoCookie::path_match("/w/index.html", "/w/"));
    assert!(ServoCookie::path_match("/w/index.html", "/w"));

    assert!(!ServoCookie::path_match("/", "/w/"));
    assert!(!ServoCookie::path_match("/a", "/w/"));
    assert!(!ServoCookie::path_match("/", "/w"));
    assert!(!ServoCookie::path_match("/w/index.html", "/w/index"));
    assert!(!ServoCookie::path_match("/windex.html", "/w/"));
    assert!(!ServoCookie::path_match("/windex.html", "/w"));
}

#[test]
fn test_default_path() {
    assert_eq!(&*ServoCookie::default_path("/foo/bar/baz/"), "/foo/bar/baz");
    assert_eq!(&*ServoCookie::default_path("/foo/bar/baz"), "/foo/bar");
    assert_eq!(&*ServoCookie::default_path("/foo/"), "/foo");
    assert_eq!(&*ServoCookie::default_path("/foo"), "/");
    assert_eq!(&*ServoCookie::default_path("/"), "/");
    assert_eq!(&*ServoCookie::default_path(""), "/");
    assert_eq!(&*ServoCookie::default_path("foo"), "/");
}

#[test]
fn fn_cookie_constructor() {
    use net_traits::CookieSource;

    let url = &ServoUrl::parse("http://example.com/foo").unwrap();

    let gov_url = &ServoUrl::parse("http://gov.ac/foo").unwrap();
    // cookie name/value test
    assert!(cookie::Cookie::parse(" baz ").is_err());
    assert!(cookie::Cookie::parse(" = bar  ").is_err());
    assert!(cookie::Cookie::parse(" baz = ").is_ok());

    // cookie domains test
    let cookie = cookie::Cookie::parse(" baz = bar; Domain =  ").unwrap();
    assert!(ServoCookie::new_wrapped(cookie.clone(), url, CookieSource::HTTP).is_some());
    let cookie = ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).unwrap();
    assert_eq!(&**cookie.cookie.domain().as_ref().unwrap(), "example.com");

    // cookie public domains test
    let cookie = cookie::Cookie::parse(" baz = bar; Domain =  gov.ac").unwrap();
    assert!(ServoCookie::new_wrapped(cookie.clone(), url, CookieSource::HTTP).is_none());
    assert!(ServoCookie::new_wrapped(cookie, gov_url, CookieSource::HTTP).is_some());

    // cookie domain matching test
    let cookie = cookie::Cookie::parse(" baz = bar ; Secure; Domain = bazample.com").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let cookie = cookie::Cookie::parse(" baz = bar ; Secure; Path = /foo/bar/").unwrap();
    assert!(
        ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none(),
        "Cookie with \"Secure\" attribute from non-secure source should be rejected"
    );

    let cookie = cookie::Cookie::parse(" baz = bar ; HttpOnly").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::NonHTTP).is_none());

    let secure_url = &ServoUrl::parse("https://example.com/foo").unwrap();
    let cookie = cookie::Cookie::parse(" baz = bar ; Secure; Path = /foo/bar/").unwrap();
    let cookie = ServoCookie::new_wrapped(cookie, secure_url, CookieSource::HTTP).unwrap();
    assert_eq!(cookie.cookie.value(), "bar");
    assert_eq!(cookie.cookie.name(), "baz");
    assert!(cookie.cookie.secure().unwrap_or(false));
    assert_eq!(&cookie.cookie.path().as_ref().unwrap()[..], "/foo/bar/");
    assert_eq!(&cookie.cookie.domain().as_ref().unwrap()[..], "example.com");
    assert!(cookie.host_only);

    let u = &ServoUrl::parse("http://example.com/foobar").unwrap();
    let cookie = cookie::Cookie::parse("foobar=value;path=/").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, u, CookieSource::HTTP).is_some());

    let cookie = cookie::Cookie::parse("foo=bar; max-age=99999999999999999999999999999").unwrap();
    let cookie = ServoCookie::new_wrapped(cookie, u, CookieSource::HTTP).unwrap();
    assert!(
        cookie
            .expiry_time
            .is_some_and(|exp| exp < SystemTime::now() + Duration::from_secs(401 * 24 * 60 * 60))
    );
}

#[test]
fn test_cookie_secure_prefix() {
    let url = &ServoUrl::parse("https://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Secure-SID=12345").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let url = &ServoUrl::parse("http://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Secure-SID=12345; Secure").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let url = &ServoUrl::parse("https://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Secure-SID=12345; Secure").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_some());

    let url = &ServoUrl::parse("https://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Secure-SID=12345; Domain=example.com").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let url = &ServoUrl::parse("http://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Secure-SID=12345; Secure; Domain=example.com").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let url = &ServoUrl::parse("https://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Secure-SID=12345; Secure; Domain=example.com").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_some());
}

#[test]
fn test_cookie_host_prefix() {
    let url = &ServoUrl::parse("https://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Host-SID=12345").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let url = &ServoUrl::parse("http://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Host-SID=12345; Secure").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let url = &ServoUrl::parse("https://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Host-SID=12345; Secure").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let url = &ServoUrl::parse("https://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Host-SID=12345; Domain=example.com").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let url = &ServoUrl::parse("https://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Host-SID=12345; Domain=example.com; Path=/").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let url = &ServoUrl::parse("http://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Host-SID=12345; Secure; Domain=example.com").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let url = &ServoUrl::parse("https://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Host-SID=12345; Secure; Domain=example.com").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let url = &ServoUrl::parse("https://example.com").unwrap();
    let cookie =
        cookie::Cookie::parse("__Host-SID=12345; Secure; Domain=example.com; Path=/").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_none());

    let url = &ServoUrl::parse("https://example.com").unwrap();
    let cookie = cookie::Cookie::parse("__Host-SID=12345; Secure; Path=/").unwrap();
    assert!(ServoCookie::new_wrapped(cookie, url, CookieSource::HTTP).is_some());
}

fn delay_to_ensure_different_timestamp() {
    use std::thread;
    use std::time::Duration;

    // time::now()'s resolution on some platforms isn't granular enought to ensure
    // that two back-to-back calls to Cookie::new_wrapped generate different timestamps .
    thread::sleep(Duration::from_millis(500));
}

#[test]
fn test_sort_order() {
    use std::cmp::Ordering;

    let url = &ServoUrl::parse("http://example.com/foo").unwrap();
    let a_wrapped = cookie::Cookie::parse("baz=bar; Path=/foo/bar/").unwrap();
    let a = ServoCookie::new_wrapped(a_wrapped.clone(), url, CookieSource::HTTP).unwrap();
    delay_to_ensure_different_timestamp();
    let a_prime = ServoCookie::new_wrapped(a_wrapped, url, CookieSource::HTTP).unwrap();
    let b = cookie::Cookie::parse("baz=bar;Path=/foo/bar/baz/").unwrap();
    let b = ServoCookie::new_wrapped(b, url, CookieSource::HTTP).unwrap();

    assert!(b.cookie.path().as_ref().unwrap().len() > a.cookie.path().as_ref().unwrap().len());
    assert_eq!(CookieStorage::cookie_comparator(&a, &b), Ordering::Greater);
    assert_eq!(CookieStorage::cookie_comparator(&b, &a), Ordering::Less);
    assert_eq!(
        CookieStorage::cookie_comparator(&a, &a_prime),
        Ordering::Less
    );
    assert_eq!(
        CookieStorage::cookie_comparator(&a_prime, &a),
        Ordering::Greater
    );
    assert_eq!(CookieStorage::cookie_comparator(&a, &a), Ordering::Equal);
}

fn add_cookie_to_storage(storage: &mut CookieStorage, url: &ServoUrl, cookie_str: &str) {
    let source = CookieSource::HTTP;
    let cookie = cookie::Cookie::parse(cookie_str.to_owned()).unwrap();
    let cookie = ServoCookie::new_wrapped(cookie, url, source).unwrap();
    storage.push(cookie, url, source);
}

#[test]
fn test_insecure_cookies_cannot_evict_secure_cookie() {
    let mut storage = CookieStorage::new(5);
    let secure_url = ServoUrl::parse("https://home.example.org:8888/cookie-parser?0001").unwrap();
    let source = CookieSource::HTTP;
    let mut cookies = Vec::new();

    cookies.push(cookie::Cookie::parse("foo=bar; Secure; Domain=home.example.org").unwrap());
    cookies.push(cookie::Cookie::parse("foo2=bar; Secure; Domain=.example.org").unwrap());
    cookies.push(cookie::Cookie::parse("foo3=bar; Secure; Path=/foo").unwrap());
    cookies.push(cookie::Cookie::parse("foo4=bar; Secure; Path=/foo/bar").unwrap());

    for bare_cookie in cookies {
        let cookie = ServoCookie::new_wrapped(bare_cookie, &secure_url, source).unwrap();
        storage.push(cookie, &secure_url, source);
    }

    let insecure_url = ServoUrl::parse("http://home.example.org:8888/cookie-parser?0001").unwrap();

    add_cookie_to_storage(
        &mut storage,
        &insecure_url,
        "foo=value; Domain=home.example.org",
    );
    add_cookie_to_storage(
        &mut storage,
        &insecure_url,
        "foo2=value; Domain=.example.org",
    );
    add_cookie_to_storage(&mut storage, &insecure_url, "foo3=value; Path=/foo/bar");
    add_cookie_to_storage(&mut storage, &insecure_url, "foo4=value; Path=/foo");

    let source = CookieSource::HTTP;
    assert_eq!(
        storage.cookies_for_url(&secure_url, source).unwrap(),
        "foo=bar; foo2=bar"
    );

    let url =
        ServoUrl::parse("https://home.example.org:8888/foo/cookie-parser-result?0001").unwrap();
    let source = CookieSource::HTTP;
    assert_eq!(
        storage.cookies_for_url(&url, source).unwrap(),
        "foo3=bar; foo4=value; foo=bar; foo2=bar"
    );

    let url =
        ServoUrl::parse("https://home.example.org:8888/foo/bar/cookie-parser-result?0001").unwrap();
    let source = CookieSource::HTTP;
    assert_eq!(
        storage.cookies_for_url(&url, source).unwrap(),
        "foo4=bar; foo3=bar; foo4=value; foo=bar; foo2=bar"
    );
}

#[test]
fn test_secure_cookies_eviction() {
    let mut storage = CookieStorage::new(5);
    let url = ServoUrl::parse("https://home.example.org:8888/cookie-parser?0001").unwrap();
    let source = CookieSource::HTTP;
    let mut cookies = Vec::new();

    cookies.push(cookie::Cookie::parse("foo=bar; Secure; Domain=home.example.org").unwrap());
    cookies.push(cookie::Cookie::parse("foo2=bar; Secure; Domain=.example.org").unwrap());
    cookies.push(cookie::Cookie::parse("foo3=bar; Secure; Path=/foo").unwrap());
    cookies.push(cookie::Cookie::parse("foo4=bar; Secure; Path=/foo/bar").unwrap());

    for bare_cookie in cookies {
        let cookie = ServoCookie::new_wrapped(bare_cookie, &url, source).unwrap();
        storage.push(cookie, &url, source);
    }

    add_cookie_to_storage(&mut storage, &url, "foo=value; Domain=home.example.org");
    add_cookie_to_storage(&mut storage, &url, "foo2=value; Domain=.example.org");
    add_cookie_to_storage(&mut storage, &url, "foo3=value; Path=/foo/bar");
    add_cookie_to_storage(&mut storage, &url, "foo4=value; Path=/foo");

    let source = CookieSource::HTTP;
    assert_eq!(storage.cookies_for_url(&url, source).unwrap(), "foo2=value");

    let url =
        ServoUrl::parse("https://home.example.org:8888/foo/cookie-parser-result?0001").unwrap();
    let source = CookieSource::HTTP;
    assert_eq!(
        storage.cookies_for_url(&url, source).unwrap(),
        "foo3=bar; foo4=value; foo2=value"
    );

    let url =
        ServoUrl::parse("https://home.example.org:8888/foo/bar/cookie-parser-result?0001").unwrap();
    let source = CookieSource::HTTP;
    assert_eq!(
        storage.cookies_for_url(&url, source).unwrap(),
        "foo4=bar; foo3=value; foo3=bar; foo4=value; foo2=value"
    );
}

#[test]
fn test_secure_cookies_eviction_non_http_source() {
    let mut storage = CookieStorage::new(5);
    let url = ServoUrl::parse("https://home.example.org:8888/cookie-parser?0001").unwrap();
    let source = CookieSource::NonHTTP;
    let mut cookies = Vec::new();

    cookies.push(cookie::Cookie::parse("foo=bar; Secure; Domain=home.example.org").unwrap());
    cookies.push(cookie::Cookie::parse("foo2=bar; Secure; Domain=.example.org").unwrap());
    cookies.push(cookie::Cookie::parse("foo3=bar; Secure; Path=/foo").unwrap());
    cookies.push(cookie::Cookie::parse("foo4=bar; Secure; Path=/foo/bar").unwrap());

    for bare_cookie in cookies {
        let cookie = ServoCookie::new_wrapped(bare_cookie, &url, source).unwrap();
        storage.push(cookie, &url, source);
    }

    add_cookie_to_storage(&mut storage, &url, "foo=value; Domain=home.example.org");
    add_cookie_to_storage(&mut storage, &url, "foo2=value; Domain=.example.org");
    add_cookie_to_storage(&mut storage, &url, "foo3=value; Path=/foo/bar");
    add_cookie_to_storage(&mut storage, &url, "foo4=value; Path=/foo");

    let source = CookieSource::HTTP;
    assert_eq!(storage.cookies_for_url(&url, source).unwrap(), "foo2=value");

    let url =
        ServoUrl::parse("https://home.example.org:8888/foo/cookie-parser-result?0001").unwrap();
    let source = CookieSource::HTTP;
    assert_eq!(
        storage.cookies_for_url(&url, source).unwrap(),
        "foo3=bar; foo4=value; foo2=value"
    );

    let url =
        ServoUrl::parse("https://home.example.org:8888/foo/bar/cookie-parser-result?0001").unwrap();
    let source = CookieSource::HTTP;
    assert_eq!(
        storage.cookies_for_url(&url, source).unwrap(),
        "foo4=bar; foo3=value; foo3=bar; foo4=value; foo2=value"
    );
}

fn add_retrieve_cookies(
    set_location: &str,
    set_cookies: &[String],
    final_location: &str,
) -> String {
    let mut storage = CookieStorage::new(5);
    let url = ServoUrl::parse(set_location).unwrap();
    let source = CookieSource::HTTP;

    // Add all cookies to the store
    for str_cookie in set_cookies {
        let cookie = ServoCookie::from_cookie_string(str_cookie.to_owned(), &url, source).unwrap();
        storage.push(cookie, &url, source);
    }

    // Get cookies for the test location
    let url = ServoUrl::parse(final_location).unwrap();
    storage
        .cookies_for_url(&url, source)
        .unwrap_or("".to_string())
}

#[test]
fn test_cookie_eviction_expired() {
    let mut vec = Vec::new();
    for i in 1..6 {
        let st = format!(
            "extra{}=bar; Secure; expires=Sun, 18-Apr-2000 21:06:29 GMT",
            i
        );
        vec.push(st);
    }
    vec.push("foo=bar; Secure; expires=Sun, 18-Apr-2027 21:06:29 GMT".to_owned());
    let r = add_retrieve_cookies(
        "https://home.example.org:8888/cookie-parser?0001",
        &vec,
        "https://home.example.org:8888/cookie-parser-result?0001",
    );
    assert_eq!(&r, "foo=bar");
}

#[test]
fn test_cookie_eviction_all_secure_one_nonsecure() {
    let mut vec = Vec::new();
    for i in 1..5 {
        let st = format!(
            "extra{}=bar; Secure; expires=Sun, 18-Apr-2026 21:06:29 GMT",
            i
        );
        vec.push(st);
    }
    vec.push("foo=bar; expires=Sun, 18-Apr-2026 21:06:29 GMT".to_owned());
    vec.push("foo2=bar; Secure; expires=Sun, 18-Apr-2028 21:06:29 GMT".to_owned());
    let r = add_retrieve_cookies(
        "https://home.example.org:8888/cookie-parser?0001",
        &vec,
        "https://home.example.org:8888/cookie-parser-result?0001",
    );
    assert_eq!(
        &r,
        "extra1=bar; extra2=bar; extra3=bar; extra4=bar; foo2=bar"
    );
}

#[test]
fn test_cookie_eviction_all_secure_new_nonsecure() {
    let mut vec = Vec::new();
    for i in 1..6 {
        let st = format!(
            "extra{}=bar; Secure; expires=Sun, 18-Apr-2026 21:06:29 GMT",
            i
        );
        vec.push(st);
    }
    vec.push("foo=bar; expires=Sun, 18-Apr-2077 21:06:29 GMT".to_owned());
    let r = add_retrieve_cookies(
        "https://home.example.org:8888/cookie-parser?0001",
        &vec,
        "https://home.example.org:8888/cookie-parser-result?0001",
    );
    assert_eq!(
        &r,
        "extra1=bar; extra2=bar; extra3=bar; extra4=bar; extra5=bar"
    );
}

#[test]
fn test_cookie_eviction_all_nonsecure_new_secure() {
    let mut vec = Vec::new();
    for i in 1..6 {
        let st = format!("extra{}=bar; expires=Sun, 18-Apr-2026 21:06:29 GMT", i);
        vec.push(st);
    }
    vec.push("foo=bar; Secure; expires=Sun, 18-Apr-2077 21:06:29 GMT".to_owned());
    let r = add_retrieve_cookies(
        "https://home.example.org:8888/cookie-parser?0001",
        &vec,
        "https://home.example.org:8888/cookie-parser-result?0001",
    );
    assert_eq!(
        &r,
        "extra2=bar; extra3=bar; extra4=bar; extra5=bar; foo=bar"
    );
}

#[test]
fn test_cookie_eviction_all_nonsecure_new_nonsecure() {
    let mut vec = Vec::new();
    for i in 1..6 {
        let st = format!("extra{}=bar; expires=Sun, 18-Apr-2026 21:06:29 GMT", i);
        vec.push(st);
    }
    vec.push("foo=bar; expires=Sun, 18-Apr-2077 21:06:29 GMT".to_owned());
    let r = add_retrieve_cookies(
        "https://home.example.org:8888/cookie-parser?0001",
        &vec,
        "https://home.example.org:8888/cookie-parser-result?0001",
    );
    assert_eq!(
        &r,
        "extra2=bar; extra3=bar; extra4=bar; extra5=bar; foo=bar"
    );
}

#[test]
fn test_parse_date() {
    assert_eq!(
        ServoCookie::parse_date("26 Jun 2024 15:35:10 GMT"), // without day of week
        Some(datetime!(2024-06-26 15:35:10).assume_utc())
    );
    assert_eq!(
        ServoCookie::parse_date("26-Jun-2024 15:35:10 GMT"), // dashed
        Some(datetime!(2024-06-26 15:35:10).assume_utc())
    );
    assert_eq!(
        ServoCookie::parse_date("26 Jun 2024 15:35:10"), // no GMT
        Some(datetime!(2024-06-26 15:35:10).assume_utc())
    );
    assert_eq!(
        ServoCookie::parse_date("26 Jun 24 15:35:10 GMT"), // 2-digit year
        Some(datetime!(2024-06-26 15:35:10).assume_utc())
    );
    assert_eq!(
        ServoCookie::parse_date("26 jun 2024 15:35:10 gmt"), // Lowercase
        Some(datetime!(2024-06-26 15:35:10).assume_utc())
    );
}

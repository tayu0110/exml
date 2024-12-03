// Copyright of the original code is the following.
// --------
// Summary: library of generic URI related routines
// Description: library of generic URI related routines
//              Implements RFC 2396
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// uri.c: set of generic URI related routines
//
// Reference: RFCs 3986, 2732 and 2373
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{borrow::Cow, string::FromUtf8Error};

use url::Url;

/// Computes he final URI of the reference done by checking that
/// the given URI is valid, and building the final URI using the
/// base URI. This is processed according to section 5.2 of the
/// RFC 2396
///
/// 5.2. Resolving Relative References to Absolute Form
///
/// Returns a new URI string or `None` if an error occurs.
#[doc(alias = "xmlBuildURI")]
pub fn build_uri<'a>(mut uri: impl Iterator<Item = Cow<'a, str>>, base: &str) -> Option<String> {
    if let Ok(base) = Url::parse(base) {
        let res = uri.try_fold(base, |base, uri| base.join(uri.as_ref()).ok())?;
        Some(res.as_str().to_owned())
    } else if let Ok(base) = Url::from_directory_path(base) {
        let res = uri.try_fold(base, |base, uri| base.join(uri.as_ref()).ok())?;
        Some(res.path().to_owned())
    } else {
        let root = Url::from_directory_path("/").ok()?;
        let base = root.join(base).ok()?;
        let res = uri.try_fold(base, |base, uri| base.join(uri.as_ref()).ok())?;
        Some(res.path()[1..].to_owned())
    }
}

/// Unescaping routine, but does not check that the string is an URI.
/// The output is a direct unsigned char translation of %XX values (no encoding)
///
/// Note that the length of the result can only be smaller or same size as the input string.
///
/// If escaped content is not found, return the original `url` wrapped `Ok(Cow::Borrowed)`.  
/// If escaped content is found, return unescaped content wrapped `Ok(Cow::Owned)`.  
/// If Invalid UTF-8 sequence is found while unescaping, return `Err`.
///
/// # Examples
/// ```rust
/// use std::borrow::Cow;
///
/// use exml::uri::unescape_url;
///
/// assert_eq!(
///     unescape_url("%F0%9F%91%BE%20Exterminate%21"),
///     Ok(Cow::<str>::Owned("👾 Exterminate!".to_owned()))
/// );
/// assert_eq!(unescape_url("Hello, World!"), Ok(Cow::<str>::Borrowed("Hello, World!")));
/// ```
pub fn unescape_url(mut url: &str) -> Result<Cow<'_, str>, FromUtf8Error> {
    let mut consumed = 0;
    let mut keep = 0;
    let mut owned = None;
    let hex2dec = |hex: u8| -> u8 {
        match hex {
            b'0'..=b'9' => hex - b'0',
            b'a'..=b'f' => hex - b'a' + 10,
            b'A'..=b'F' => hex - b'A' + 10,
            _ => unreachable!(),
        }
    };
    let orig = url;
    while let Some((non_escaped, maybe_escaped)) = url.split_once('%') {
        keep += non_escaped.len();
        match maybe_escaped.as_bytes() {
            &[hi, lo, ..] if hi.is_ascii_hexdigit() && lo.is_ascii_hexdigit() => {
                let owned = owned.get_or_insert_with(|| Vec::with_capacity(orig.len()));
                owned.extend_from_slice(orig[consumed..keep].as_bytes());
                let (hi, lo) = (hex2dec(hi), hex2dec(lo));
                owned.push((hi << 4) | lo);
                url = &maybe_escaped[2..];
                // the length of '%' + `hi` + `lo`
                keep += 3;
                consumed = keep;
            }
            _ => {
                // the length of '%'
                keep += 1;
                url = maybe_escaped;
            }
        }
    }

    if let Some(mut owned) = owned {
        owned.extend_from_slice(url.as_bytes());
        String::from_utf8(owned).map(Cow::<str>::Owned)
    } else {
        Ok(Cow::Borrowed(url))
    }
}

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

use std::{borrow::Cow, fmt::Display, string::FromUtf8Error};

fn is_reserved(c: u8) -> bool {
    c == b';'
        || c == b'/'
        || c == b'?'
        || c == b':'
        || c == b'@'
        || c == b'&'
        || c == b'='
        || c == b'+'
        || c == b'$'
        || c == b','
        || c == b'['
        || c == b']'
}

fn is_mark(c: u8) -> bool {
    c == b'-'
        || c == b'_'
        || c == b'.'
        || c == b'!'
        || c == b'~'
        || c == b'*'
        || c == b'\''
        || c == b'('
        || c == b')'
}

fn is_unreserved(c: u8) -> bool {
    c.is_ascii_alphanumeric() || is_mark(c)
}

fn to_hexdigit(c: u8) -> char {
    assert_eq!(c & 0xF0, 0);
    (if c < 10 { c + b'0' } else { c - 10 + b'A' }) as char
}

// `pct-encoded   = "%" HEXDIG HEXDIG`
fn starts_with_pct_encoded(p: &str) -> bool {
    let p = p.as_bytes();
    p.len() >= 3 && p[0] == b'%' && p[1].is_ascii_hexdigit() && p[2].is_ascii_hexdigit()
}

// `unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"`
fn starts_with_unreserved(p: &str) -> bool {
    !p.is_empty() && {
        let np = p.as_bytes()[0];
        p.starts_with(|p: char| p.is_ascii_alphanumeric())
            || np == b'-'
            || np == b'.'
            || np == b'_'
            || np == b'~'
    }
}

// `sub-delims    = "!" / "$" / "&" / "'" / "(" / ")" / "*" / "+" / "," / ";" / "="`
fn starts_with_sub_delims(p: &str) -> bool {
    p.starts_with(['!', '$', '&', '(', ')', '*', '+', ',', ';', '=', '\''])
}

// `pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"`
fn starts_with_pchar(p: &str) -> bool {
    !p.is_empty() && {
        let np = p.as_bytes()[0];
        starts_with_unreserved(p)
            || starts_with_pct_encoded(p)
            || starts_with_sub_delims(p)
            || np == b':'
            || np == b'@'
    }
}

// `unwise = "{" | "}" | "|" | "\" | "^" | "`"`
fn starts_with_unwise(p: &str) -> bool {
    p.starts_with(['{', '}', '|', '\\', '^', '[', ']', '`'])
}

const PORT_EMPTY_SERVER: Option<u16> = Some(0);

/// A parsed URI reference. This is a struct containing the various fields
/// as described in RFC 2396 but separated for further processing.
///
/// # Note
/// Query is a deprecated field which is incorrectly unescaped.
/// query_raw takes precedence over query if the former is set.
/// See: http://mail.gnome.org/archives/xml/2007-April/thread.html#00127
#[doc(alias = "xmlURI")]
#[derive(Debug, Default)]
pub struct XmlURI {
    pub(crate) scheme: Option<Cow<'static, str>>, /* the URI scheme */
    pub(crate) opaque: Option<Cow<'static, str>>, /* opaque part */
    pub(crate) authority: Option<Cow<'static, str>>, /* the authority part */
    pub(crate) server: Option<Cow<'static, str>>, /* the server part */
    pub(crate) user: Option<Cow<'static, str>>,   /* the user part */
    pub(crate) port: Option<u16>,                 /* the port number */
    pub path: Option<Cow<'static, str>>,          /* the path string */
    pub(crate) query: Option<Cow<'static, str>>, /* the query string (deprecated - use with caution) */
    pub(crate) fragment: Option<Cow<'static, str>>, /* the fragment identifier */
    pub(crate) cleanup: i32,                     /* parsing potentially unclean URI */
    pub(crate) query_raw: Option<Cow<'static, str>>, /* the query string (as it appears in the URI) */
}

impl XmlURI {
    /// Simply creates an empty xmlURI
    ///
    /// Returns the new structure
    #[doc(alias = "xmlCreateURI")]
    pub fn new() -> Self {
        Self::default()
    }

    /// Save the URI as an escaped string
    ///
    /// Returns a new string
    #[doc(alias = "xmlSaveUri")]
    pub fn save(&self) -> String {
        let mut ret = String::new();
        if let Some(scheme) = self.scheme.as_deref() {
            ret.push_str(scheme);
            ret.push(':');
        }
        if let Some(opaque) = self.opaque.as_deref() {
            for p in opaque.bytes() {
                if is_reserved(p) || is_unreserved(p) {
                    ret.push(p as char);
                } else {
                    let hi = p >> 4;
                    let lo = p & 0xF;
                    ret.push('%');
                    ret.push(to_hexdigit(hi));
                    ret.push(to_hexdigit(lo));
                }
            }
        } else {
            if self.server.is_some() || self.port.is_some() {
                ret.push_str("//");
                if let Some(user) = self.user.as_deref() {
                    for p in user.bytes() {
                        if is_unreserved(p)
                            || p == b';'
                            || p == b':'
                            || p == b'&'
                            || p == b'='
                            || p == b'+'
                            || p == b'$'
                            || p == b','
                        {
                            ret.push(p as char);
                        } else {
                            let hi = p >> 4;
                            let lo = p & 0xF;
                            ret.push('%');
                            ret.push(to_hexdigit(hi));
                            ret.push(to_hexdigit(lo));
                        }
                    }
                    ret.push('@');
                }
                if let Some(server) = self.server.as_deref() {
                    ret.push_str(server);
                }
                if let Some(port) = self.port.filter(|&p| p > 0) {
                    ret.push_str(format!(":{port}").as_str());
                }
            } else if let Some(authority) = self.authority.as_deref() {
                ret.push_str("//");
                for p in authority.bytes() {
                    if is_unreserved(p)
                        || p == b'$'
                        || p == b','
                        || p == b';'
                        || p == b':'
                        || p == b'@'
                        || p == b'&'
                        || p == b'='
                        || p == b'+'
                    {
                        ret.push(p as char);
                    } else {
                        let hi = p >> 4;
                        let lo = p & 0xF;
                        ret.push('%');
                        ret.push(to_hexdigit(hi));
                        ret.push(to_hexdigit(lo));
                    }
                }
            }
            if let Some(mut path) = self.path.as_deref() {
                // the colon in file:///d: should not be escaped or
                // Windows accesses fail later.
                if self
                    .scheme
                    .as_deref()
                    .filter(|&scheme| {
                        let p = path.as_bytes();
                        p.len() >= 3
                            && p[0] == b'/'
                            && p[1].is_ascii_alphabetic()
                            && p[2] == b':'
                            && scheme == "file"
                    })
                    .is_some()
                {
                    ret.push_str(&path[..3]);
                    path = &path[3..];
                }
                for p in path.bytes() {
                    if is_unreserved(p)
                        || p == b'/'
                        || p == b';'
                        || p == b'@'
                        || p == b'&'
                        || p == b'='
                        || p == b'+'
                        || p == b'$'
                        || p == b','
                    {
                        ret.push(p as char);
                    } else {
                        let hi = p >> 4;
                        let lo = p & 0xF;
                        ret.push('%');
                        ret.push(to_hexdigit(hi));
                        ret.push(to_hexdigit(lo));
                    }
                }
            }
            if let Some(query) = self.query_raw.as_deref() {
                ret.push('?');
                ret.push_str(query);
            } else if let Some(query) = self.query.as_deref() {
                ret.push('?');
                for p in query.bytes() {
                    if is_unreserved(p) || is_reserved(p) {
                        ret.push(p as char);
                    } else {
                        let hi = p >> 4;
                        let lo = p & 0xF;
                        ret.push('%');
                        ret.push(to_hexdigit(hi));
                        ret.push(to_hexdigit(lo));
                    }
                }
            }
        }
        if let Some(fragment) = self.fragment.as_deref() {
            ret.push('#');
            for p in fragment.bytes() {
                if is_unreserved(p) || is_reserved(p) {
                    ret.push(p as char);
                } else {
                    let hi = p >> 4;
                    let lo = p & 0xF;
                    ret.push('%');
                    ret.push(to_hexdigit(hi));
                    ret.push(to_hexdigit(lo));
                }
            }
        }
        ret
    }

    /// Parse an URI scheme
    ///
    /// ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986Scheme")]
    fn parse3986_scheme<'a>(&mut self, mut s: &'a str) -> Result<&'a str, i32> {
        let orig = s;
        if s.starts_with(|c: char| !c.is_ascii_alphabetic()) {
            return Err(2);
        }
        s = s.trim_start_matches(|c: char| {
            c.is_ascii_alphanumeric() || c == '+' || c == '-' || c == '.'
        });
        self.scheme = Some(orig[..orig.len() - s.len()].to_owned().into());
        Ok(s)
    }

    /// Parse an user information part and fills in the appropriate fields
    /// of the @uri structure
    ///
    /// userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986Userinfo")]
    fn parse3986_userinfo<'a>(&mut self, s: &'a str) -> Result<&'a str, i32> {
        let mut cur = s;
        while starts_with_unreserved(cur)
            || starts_with_pct_encoded(cur)
            || starts_with_sub_delims(cur)
            || cur.starts_with(':')
        {
            cur = if cur.starts_with('%') {
                &cur[3..]
            } else {
                &cur[1..]
            };
        }
        if !cur.starts_with('@') {
            return Err(1);
        }
        self.user = if self.cleanup & 2 != 0 {
            Some(s[..s.len() - cur.len()].to_owned().into())
        } else {
            unescape_url(&s[..s.len() - cur.len()])
                .ok()
                .map(|u| u.into_owned().into())
        };
        Ok(cur)
    }

    /// Parse an host part and fills in the appropriate fields
    /// of the @uri structure
    ///
    /// ```ignore
    /// host          = IP-literal / IPv4address / reg-name
    /// IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"
    /// IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet
    /// reg-name      = *( unreserved / pct-encoded / sub-delims )
    /// ```
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986Host")]
    fn parse3986_host<'a>(&mut self, s: &'a str) -> Result<&'a str, i32> {
        let mut cur = s;

        // IPv6 and future addressing scheme are enclosed between brackets
        if let Some(rem) = cur.strip_prefix('[') {
            let (_, rem) = rem.split_once(']').ok_or(1)?;
            cur = rem;
        } else {
            // try to parse an IPv4
            fn parse_ipv4(mut s: &str) -> Result<&str, i32> {
                s = parse3986_dec_octet(s)?;
                for _ in 0..3 {
                    s = s.strip_prefix('.').ok_or(1)?;
                    s = parse3986_dec_octet(s)?;
                }
                Ok(s)
            }
            if let Ok(rem) = parse_ipv4(cur) {
                cur = rem;
            } else {
                // then this should be a hostname which can be empty
                while starts_with_unreserved(cur)
                    || starts_with_pct_encoded(cur)
                    || starts_with_sub_delims(cur)
                {
                    if cur.starts_with('%') {
                        cur = &cur[3..];
                    } else {
                        cur = &cur[1..];
                    }
                }
            }
        }
        // found:
        self.authority = None;
        self.server = None;
        if cur.len() != s.len() {
            self.server = if self.cleanup & 2 != 0 {
                Some(s[..s.len() - cur.len()].to_owned().into())
            } else {
                unescape_url(&s[..s.len() - cur.len()])
                    .ok()
                    .map(|u| u.into_owned().into())
            };
        }
        Ok(cur)
    }

    /// Parse a port part and fills in the appropriate fields of the @uri structure
    ///
    /// `port          = *DIGIT`
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986Port")]
    fn parse3986_port<'a>(&mut self, s: &'a str) -> Result<&'a str, i32> {
        if let Some((port, _)) = s
            .split_once(|c: char| !c.is_ascii_digit())
            .filter(|p| !p.0.is_empty())
        {
            self.port = Some(port.parse::<u16>().map_err(|_| 1)?);
            Ok(&s[port.len()..])
        } else if let Ok(port) = s.parse::<u16>() {
            self.port = Some(port);
            Ok("")
        } else {
            Err(1)
        }
    }

    /// Parse an authority part and fills in the appropriate fields
    /// of the @uri structure
    ///
    /// `authority     = [ userinfo "@" ] host [ ":" port ]`
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986Authority")]
    fn parse3986_authority<'a>(&mut self, mut s: &'a str) -> Result<&'a str, i32> {
        // try to parse an userinfo and check for the trailing @
        if let Some(rem) = self
            .parse3986_userinfo(s)
            .ok()
            .filter(|s| s.starts_with('@'))
        {
            s = &rem[1..];
        }
        s = self.parse3986_host(s)?;
        if let Some(rem) = s.strip_prefix(':') {
            s = self.parse3986_port(rem)?;
        }
        Ok(s)
    }

    /// Parse an path absolute or empty and fills in the appropriate fields of the @uri structure
    ///
    /// `path-abempty  = *( "/" segment )`
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986PathAbEmpty")]
    fn parse3986_path_ab_empty<'a>(&mut self, mut s: &'a str) -> Result<&'a str, i32> {
        let orig = s;
        while let Some(rem) = s.strip_prefix('/') {
            s = parse3986_segment(rem, 0, true)?;
        }
        self.path = None;
        if s.len() != orig.len() {
            if self.cleanup & 2 != 0 {
                self.path = Some(orig[..orig.len() - s.len()].to_owned().into());
            } else {
                self.path = unescape_url(&orig[..orig.len() - s.len()])
                    .ok()
                    .map(|u| u.into_owned().into())
            }
        }
        Ok(s)
    }

    /// Parse an path absolute and fills in the appropriate fields of the @uri structure
    ///
    /// `path-absolute = "/" [ segment-nz *( "/" segment ) ]`
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986PathAbsolute")]
    fn parse3986_path_absolute<'a>(&mut self, mut s: &'a str) -> Result<&'a str, i32> {
        let orig = s;
        s = s.strip_prefix('/').ok_or(1)?;
        if let Ok(rem) = parse3986_segment(s, 0, false) {
            s = rem;
            while let Some(rem) = s.strip_prefix('/') {
                s = parse3986_segment(rem, 0, true)?;
            }
        }
        self.path = None;
        if orig.len() != s.len() {
            if self.cleanup & 2 != 0 {
                self.path = Some(orig[..orig.len() - s.len()].to_owned().into());
            } else {
                self.path = unescape_url(&orig[..orig.len() - s.len()])
                    .ok()
                    .map(|u| u.into_owned().into())
            }
        }
        Ok(s)
    }

    /// Parse an path without root and fills in the appropriate fields
    /// of the @uri structure
    ///
    /// `path-rootless = segment-nz *( "/" segment )`
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986PathRootless")]
    fn parse3986_path_rootless<'a>(&mut self, mut s: &'a str) -> Result<&'a str, i32> {
        let orig = s;
        s = parse3986_segment(s, 0, false)?;
        while let Some(rem) = s.strip_prefix('/') {
            s = parse3986_segment(rem, 0, true)?;
        }
        self.path = None;
        if orig.len() != s.len() {
            if self.cleanup & 2 != 0 {
                self.path = Some(orig[..orig.len() - s.len()].to_owned().into());
            } else {
                self.path = unescape_url(&orig[..orig.len() - s.len()])
                    .ok()
                    .map(|u| u.into_owned().into())
            }
        }
        Ok(s)
    }

    /// Parse an hierarchical part and fills in the appropriate fields
    /// of the @uri structure
    ///
    /// ```ignore
    /// hier-part     = "//" authority path-abempty
    ///                / path-absolute
    ///                / path-rootless
    ///                / path-empty
    /// ```
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986HierPart")]
    fn parse3986_hier_part<'a>(&mut self, mut s: &'a str) -> Result<&'a str, i32> {
        if let Some(rem) = s.strip_prefix("//") {
            s = self.parse3986_authority(rem)?;
            // An empty server is marked with a special URI value.
            if self.server.is_none() && self.port.is_none() {
                self.port = PORT_EMPTY_SERVER;
            }
            s = self.parse3986_path_ab_empty(s)?;
        } else if s.starts_with('/') {
            s = self.parse3986_path_absolute(s)?;
        } else if starts_with_pchar(s) {
            s = self.parse3986_path_rootless(s)?;
        } else {
            /* path-empty is effectively empty */
            self.path = None;
        }
        Ok(s)
    }

    /// Parse the query part of an URI
    ///
    /// `query = *uric`
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986Query")]
    fn parse3986_query<'a>(&mut self, mut s: &'a str) -> Result<&'a str, i32> {
        let orig = s;
        while starts_with_pchar(s)
            || s.starts_with(['/', '?'])
            || (self.cleanup & 1 != 0 && starts_with_unwise(s))
        {
            if s.starts_with('%') {
                s = &s[3..];
            } else {
                s = &s[1..];
            }
        }
        if self.cleanup & 2 != 0 {
            self.query = Some(orig[..orig.len() - s.len()].to_owned().into());
        } else {
            self.query = unescape_url(&orig[..orig.len() - s.len()])
                .ok()
                .map(|u| u.into_owned().into())
        }

        // Save the raw bytes of the query as well.
        // See: http://mail.gnome.org/archives/xml/2007-April/thread.html#00114
        self.query_raw = Some(orig[..orig.len() - s.len()].to_owned().into());
        Ok(s)
    }

    /// Parse the query part of an URI
    ///
    /// `fragment      = *( pchar / "/" / "?" )`
    ///
    /// # Note
    /// The strict syntax as defined by 3986 does not allow '[' and ']'
    /// in the fragment identifier but this is used very broadly for
    /// xpointer scheme selection, so we are allowing it here to not break
    /// for example all the DocBook processing chains.
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986Fragment")]
    fn parse3986_fragment<'a>(&mut self, mut s: &'a str) -> Result<&'a str, i32> {
        let orig = s;
        while starts_with_pchar(s)
            || s.starts_with(['/', '?', '[', ']'])
            || (self.cleanup & 1 != 0 && starts_with_unwise(s))
        {
            if s.starts_with('%') {
                s = &s[3..];
            } else {
                s = &s[1..];
            }
        }
        if self.cleanup & 2 != 0 {
            self.fragment = Some(orig[..orig.len() - s.len()].to_owned().into());
        } else {
            self.fragment = unescape_url(&orig[..orig.len() - s.len()])
                .ok()
                .map(|u| u.into_owned().into());
        }
        Ok(s)
    }

    /// Parse an URI string and fills in the appropriate fields of the @uri structure
    ///
    /// `scheme ":" hier-part [ "?" query ] [ "#" fragment ]`
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986URI")]
    fn parse3986_uri<'a>(&mut self, mut s: &'a str) -> Result<&'a str, i32> {
        s = self.parse3986_scheme(s)?;
        s = s.strip_prefix(':').ok_or(1)?;
        s = self.parse3986_hier_part(s)?;
        if let Some(rem) = s.strip_prefix('?') {
            s = self.parse3986_query(rem)?;
        }
        if let Some(rem) = s.strip_prefix('#') {
            s = self.parse3986_fragment(rem)?;
        }
        if !s.is_empty() {
            *self = Self::default();
            Err(1)
        } else {
            Ok(s)
        }
    }

    /// Parse an path which is not a scheme and fills in the appropriate fields of the @uri structure
    ///
    /// `path-noscheme = segment-nz-nc *( "/" segment )`
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986PathNoScheme")]
    fn parse3986_path_no_scheme<'a>(&mut self, mut s: &'a str) -> Result<&'a str, i32> {
        let orig = s;
        s = parse3986_segment(s, b':', false)?;
        while let Some(rem) = s.strip_prefix('/') {
            s = parse3986_segment(rem, 0, true)?;
        }
        self.path = None;
        if orig.len() != s.len() {
            if self.cleanup & 2 != 0 {
                self.path = Some(orig[..orig.len() - s.len()].to_owned().into());
            } else {
                self.path = unescape_url(&orig[..orig.len() - s.len()])
                    .ok()
                    .map(|u| u.into_owned().into())
            }
        }
        Ok(s)
    }

    /// Parse an URI string and fills in the appropriate fields
    /// of the @uri structure
    ///
    /// ```ignore
    /// relative-refe  = relative-part [ "?" query ] [ "#" fragment ]
    /// relative-part = "//" authority path-abempty
    ///               / path-absolute
    ///               / path-noscheme
    ///               / path-empty
    /// ```
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986RelativeRef")]
    fn parse3986_relative_ref<'a>(&mut self, mut s: &'a str) -> Result<&'a str, i32> {
        if let Some(rem) = s.strip_prefix("//") {
            s = self.parse3986_authority(rem)?;
            s = self.parse3986_path_ab_empty(s)?;
        } else if s.starts_with('/') {
            s = self.parse3986_path_absolute(s)?;
        } else if starts_with_pchar(s) {
            s = self.parse3986_path_no_scheme(s)?;
        } else {
            // path-empty is effectively empty
            self.path = None;
        }

        if let Some(rem) = s.strip_prefix('?') {
            s = self.parse3986_query(rem)?;
        }
        if let Some(rem) = s.strip_prefix('#') {
            s = self.parse3986_fragment(rem)?;
        }
        if !s.is_empty() {
            *self = Self::default();
            Err(1)
        } else {
            Ok(s)
        }
    }

    /// Parse an URI reference string and fills in the appropriate fields of the @uri structure
    ///
    /// `URI-reference = URI / relative-refe`
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParse3986URIReference")]
    fn parse3986_uri_reference(&mut self, s: &str) -> Result<(), i32> {
        *self = Self::default();

        // Try first to parse absolute refs, then fallback to relative if it fails.
        if self.parse3986_uri(s).is_err() {
            *self = Self::default();
            if let Err(code) = self.parse3986_relative_ref(s) {
                *self = Self::default();
                return Err(code);
            }
        }
        Ok(())
    }

    /// Parse an URI reference string based on RFC 3986 and fills in the
    /// appropriate fields of the @uri structure
    ///
    /// `URI-reference = URI / relative-refe`
    ///
    /// Returns 0 or the error code
    #[doc(alias = "xmlParseURIReference")]
    pub fn parse_uri_reference(&mut self, s: &str) -> Result<(), i32> {
        self.parse3986_uri_reference(s)
    }
}

impl Display for XmlURI {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.save())
    }
}

///```ignore
/// dec-octet     = DIGIT                 ; 0-9
///              / %x31-39 DIGIT         ; 10-99
///              / "1" 2DIGIT            ; 100-199
///              / "2" %x30-34 DIGIT     ; 200-249
///              / "25" %x30-35          ; 250-255
/// ```
///
/// Skip a dec-octet.
///
/// Returns 0 if found and skipped, 1 otherwise
#[doc(alias = "xmlParse3986DecOctet")]
fn parse3986_dec_octet(s: &str) -> Result<&str, i32> {
    let mut t = s[..3.min(s.len())].split_terminator(|c: char| !c.is_ascii_digit());
    let t = t.next().filter(|t| !t.is_empty()).ok_or(1)?;
    let u = t.parse::<u16>().unwrap();
    if u < 256 {
        Ok(&s[t.len()..])
    } else {
        Err(1)
    }
}

/// Parse a segment and fills in the appropriate fields
/// of the @uri structure
///
/// ```ignore
/// segment       = *pchar
/// segment-nz    = 1*pchar
/// segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
///               ; non-zero-length segment without any colon ":"
/// ```
///
/// Returns 0 or the error code
#[doc(alias = "xmlParse3986Segment")]
fn parse3986_segment(mut s: &str, forbid: u8, empty: bool) -> Result<&str, i32> {
    if !starts_with_pchar(s) {
        if empty {
            return Ok(s);
        }
        return Err(1);
    }
    while starts_with_pchar(s) && !s.as_bytes().starts_with(&[forbid]) {
        if s.starts_with('%') {
            s = &s[3..];
        } else {
            s = &s[1..];
        }
    }
    Ok(s)
}

/// Computes he final URI of the reference done by checking that
/// the given URI is valid, and building the final URI using the
/// base URI. This is processed according to section 5.2 of the
/// RFC 2396
///
/// 5.2. Resolving Relative References to Absolute Form
///
/// Returns a new URI string or `None` if an error occurs.
#[doc(alias = "xmlBuildURI")]
pub fn build_uri(uri: &str, base: &str) -> Option<String> {
    let mut val = None;
    let mut refe = None;

    // 1) The URI reference is parsed into the potential four components and
    //    fragment identifier, as described in Section 4.3.
    //
    //    NOTE that a completely empty URI is treated by modern browsers
    //    as a reference to "." rather than as a synonym for the current URI.
    //    Should we do that here?
    if !uri.is_empty() {
        let mut r = XmlURI::new();
        r.parse_uri_reference(uri).ok()?;
        refe = Some(r);
    }
    if refe.as_ref().map_or(false, |r| r.scheme.is_some()) {
        // The URI is absolute don't modify.
        return Some(uri.to_owned());
    }
    let mut bas = XmlURI::new();
    if bas.parse_uri_reference(base).is_err() {
        if let Some(refe) = refe.as_ref() {
            val = Some(refe.save());
        }
        return val;
    }
    let Some(refe) = refe else {
        // the base fragment must be ignored
        bas.fragment = None;
        return Some(bas.save());
    };

    // 2) If the path component is empty and the scheme, authority, and
    //    query components are undefined, then it is a reference to the
    //    current document and we are done.  Otherwise, the reference URI's
    //    query and fragment components are defined as found (or not found)
    //    within the URI reference and not inherited from the base URI.
    //
    //    NOTE that in modern browsers, the parsing differs from the above
    //    in the following aspect:  the query component is allowed to be
    //    defined while still treating this as a reference to the current
    //    document.
    let mut res = XmlURI::new();
    'step_7: {
        if refe.scheme.is_none()
            && refe.path.is_none()
            && (refe.authority.is_none() && refe.server.is_none() && refe.port.is_none())
        {
            res.scheme = bas.scheme.clone();
            if bas.authority.is_some() {
                res.authority = bas.authority.clone();
            } else {
                res.server = bas.server.clone();
                res.user = bas.user.clone();
                res.port = bas.port;
            }
            res.path = bas.path.clone();
            if refe.query_raw.is_some() {
                res.query_raw = refe.query_raw.clone();
            } else if refe.query.is_some() {
                res.query = refe.query.clone();
            } else if bas.query_raw.is_some() {
                res.query_raw = bas.query_raw.clone();
            } else if bas.query.is_some() {
                res.query = bas.query.clone();
            }
            res.fragment = refe.fragment.clone();
            break 'step_7;
        }

        // 3) If the scheme component is defined, indicating that the reference
        //    starts with a scheme name, then the reference is interpreted as an
        //    absolute URI and we are done.  Otherwise, the reference URI's
        //    scheme is inherited from the base URI's scheme component.
        if refe.scheme.is_some() {
            return Some(refe.save());
        }
        if bas.scheme.is_some() {
            res.scheme = bas.scheme.clone();
        }

        if refe.query_raw.is_some() {
            res.query_raw = refe.query_raw.clone();
        } else if refe.query.is_some() {
            res.query = refe.query.clone();
        }
        if refe.fragment.is_some() {
            res.fragment = refe.fragment.clone();
        }

        // 4) If the authority component is defined, then the reference is a
        //    network-path and we skip to step 7.  Otherwise, the reference
        //    URI's authority is inherited from the base URI's authority
        //    component, which will also be undefined if the URI scheme does not
        //    use an authority component.
        if refe.authority.is_some() || refe.server.is_some() || refe.port.is_some() {
            if refe.authority.is_some() {
                res.authority = refe.authority.clone();
            } else {
                if refe.server.is_some() {
                    res.server = refe.server.clone();
                }
                if refe.user.is_some() {
                    res.user = refe.user.clone();
                }
                res.port = refe.port;
            }
            if refe.path.is_some() {
                res.path = refe.path.clone();
            }
            break 'step_7;
        }
        if bas.authority.is_some() {
            res.authority = bas.authority.clone();
        } else if bas.server.is_some() || bas.port.is_some() {
            if bas.server.is_some() {
                res.server = bas.server.clone();
            }
            if bas.user.is_some() {
                res.user = bas.user.clone();
            }
            res.port = bas.port;
        }

        // 5) If the path component begins with a slash character ("/"), then
        //    the reference is an absolute-path and we skip to step 7.
        if let Some(path) = refe.path.as_deref().filter(|p| p.starts_with('/')) {
            res.path = Some(path.to_owned().into());
            break 'step_7;
        }

        // 6) If this step is reached, then we are resolving a relative-path reference.
        //    The relative path needs to be merged with the base URI's path.
        //    Although there are many ways to do this, we will
        //    describe a simple method using a separate string buffer.
        let mut path = String::new();

        // a) All but the last segment of the base URI's path component is copied to the buffer.
        //    In other words, any characters after the
        //    last (right-most) slash character, if any, are excluded.
        if let Some(bas_path) = bas.path.as_deref() {
            if let Some(pos) = bas_path.rfind('/') {
                path.push_str(&bas_path[..pos + 1]);
            }
        }

        // b) The reference's path component is appended to the buffer string.
        if let Some(ref_path) = refe.path.as_deref().filter(|p| !p.is_empty()) {
            // Ensure the path includes a '/'
            if path.is_empty() && (bas.server.is_some() || bas.port.is_some()) {
                path.push('/');
            }
            path.push_str(ref_path);
        }

        // Steps c) to h) are really path normalization steps
        match normalize_uri_path(&path) {
            Cow::Owned(path) => res.path = Some(Cow::Owned(path)),
            Cow::Borrowed(_) => res.path = Some(Cow::Owned(path)),
        }
    }

    // step_7:

    // 7) The resulting URI components, including any inherited from the base URI,
    //    are recombined to give the absolute form of the URI reference.
    Some(res.save())
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
#[doc(alias = "xmlURIUnescapeString")]
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

/// Applies the 5 normalization steps to a path string--that is, RFC 2396
/// Section 5.2, steps 6.c through 6.g.
///
/// Returns normalized path string.  
/// If `path` is not modified, no extended memories are allocated.
#[doc(alias = "xmlNormalizeURIPath")]
pub fn normalize_uri_path(path: &str) -> Cow<'_, str> {
    // Skip all initial "/" chars.  We want to get to the beginning of the
    // first non-empty segment.
    let Some(pos) = path.find(|c: char| c != '/') else {
        return Cow::Borrowed(path);
    };
    let (head, path) = path.split_at(pos);
    let mut splited = path.split('/');
    let last = splited.next_back().unwrap();
    let mut segments = vec![];
    for seg in splited.filter(|&seg| seg != "." && !seg.is_empty()) {
        if seg == ".." {
            if segments.is_empty() || segments.last().copied() == Some("..") {
                segments.push(seg);
            } else {
                segments.pop();
            }
        } else {
            segments.push(seg);
        }
    }
    if last == ".." && segments.last().copied() != Some("..") {
        segments.pop();
        segments.push("");
    } else if last != "." {
        segments.push(last);
    } else if last == "." {
        segments.push("");
    }
    let mut path = head.to_owned();
    if !segments.is_empty() {
        path.push_str(segments[0]);
        for seg in segments.into_iter().skip(1) {
            path.push('/');
            path.push_str(seg);
        }
    }
    Cow::Owned(path)
}

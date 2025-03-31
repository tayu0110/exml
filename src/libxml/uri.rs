//! Provide methods and data structures for handling URIs.  
//! This module is based on `libxml/uri.h`, `uri.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

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

use std::{ffi::c_char, mem::size_of, ptr::null_mut};

use libc::{INT_MAX, memset, strlen};

use crate::error::{__xml_raise_error, XmlErrorDomain, XmlErrorLevel, XmlParserErrors};

use super::{
    globals::{xml_free, xml_malloc, xml_malloc_atomic},
    xmlstring::{XmlChar, xml_strndup},
};

pub type XmlURIPtr = *mut XmlURI;
/// A parsed URI reference. This is a struct containing the various fields
/// as described in RFC 2396 but separated for further processing.
///
/// # Note
/// Query is a deprecated field which is incorrectly unescaped.
/// query_raw takes precedence over query if the former is set.  
/// See: <http://mail.gnome.org/archives/xml/2007-April/thread.html#00127>
#[doc(alias = "xmlURI")]
#[repr(C)]
pub struct XmlURI {
    pub(crate) scheme: *mut c_char,    /* the URI scheme */
    pub(crate) opaque: *mut c_char,    /* opaque part */
    pub(crate) authority: *mut c_char, /* the authority part */
    pub(crate) server: *mut c_char,    /* the server part */
    pub(crate) user: *mut c_char,      /* the user part */
    pub(crate) port: i32,              /* the port number */
    pub path: *mut c_char,             /* the path string */
    pub(crate) query: *mut c_char,     /* the query string (deprecated - use with caution) */
    pub(crate) fragment: *mut c_char,  /* the fragment identifier */
    pub(crate) cleanup: i32,           /* parsing potentially unclean URI */
    pub(crate) query_raw: *mut c_char, /* the query string (as it appears in the URI) */
}

const PORT_EMPTY: i32 = 0;
const PORT_EMPTY_SERVER: i32 = -1;

fn xml_uri_err_memory(extra: &str) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        None,
        XmlErrorDomain::XmlFromURI,
        XmlParserErrors::XmlErrNoMemory,
        XmlErrorLevel::XmlErrFatal,
        None,
        0,
        Some(extra.to_owned().into()),
        None,
        None,
        0,
        0,
        "Memory allocation failed : {}\n",
        extra
    );
}

/// Simply creates an empty xmlURI
///
/// Returns the new structure or NULL in case of error
#[doc(alias = "xmlCreateURI")]
pub unsafe extern "C" fn xml_create_uri() -> XmlURIPtr {
    unsafe {
        let ret: XmlURIPtr = xml_malloc(size_of::<XmlURI>()) as XmlURIPtr;
        if ret.is_null() {
            xml_uri_err_memory("creating URI structure\n");
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlURI>());
        (*ret).port = PORT_EMPTY;
        ret
    }
}

/// Make sure the xmlURI struct is free of content
#[doc(alias = "xmlCleanURI")]
unsafe extern "C" fn xml_clean_uri(uri: XmlURIPtr) {
    unsafe {
        if uri.is_null() {
            return;
        }

        if !(*uri).scheme.is_null() {
            xml_free((*uri).scheme as _);
        }
        (*uri).scheme = null_mut();
        if !(*uri).server.is_null() {
            xml_free((*uri).server as _);
        }
        (*uri).server = null_mut();
        if !(*uri).user.is_null() {
            xml_free((*uri).user as _);
        }
        (*uri).user = null_mut();
        if !(*uri).path.is_null() {
            xml_free((*uri).path as _);
        }
        (*uri).path = null_mut();
        if !(*uri).fragment.is_null() {
            xml_free((*uri).fragment as _);
        }
        (*uri).fragment = null_mut();
        if !(*uri).opaque.is_null() {
            xml_free((*uri).opaque as _);
        }
        (*uri).opaque = null_mut();
        if !(*uri).authority.is_null() {
            xml_free((*uri).authority as _);
        }
        (*uri).authority = null_mut();
        if !(*uri).query.is_null() {
            xml_free((*uri).query as _);
        }
        (*uri).query = null_mut();
        if !(*uri).query_raw.is_null() {
            xml_free((*uri).query_raw as _);
        }
        (*uri).query_raw = null_mut();
    }
}

macro_rules! ISA_ALPHA {
    ($p:expr) => {
        (*$p >= b'a' as i8 && *$p <= b'z' as i8) || (*$p >= b'A' as i8 && *$p <= b'Z' as i8)
    };
}

macro_rules! ISA_DIGIT {
    ($p:expr) => {
        *$p >= b'0' as i8 && *$p <= b'9' as i8
    };
}

macro_rules! STRNDUP {
    ($s:expr, $n:expr) => {
        xml_strndup($s as *const XmlChar, $n) as *mut c_char
    };
}

/// Parse an URI scheme
///
/// ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
///
/// Returns 0 or the error code
#[doc(alias = "xmlParse3986Scheme")]
unsafe extern "C" fn xml_parse3986_scheme(uri: XmlURIPtr, str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char;

        if str.is_null() {
            return -1;
        }

        cur = *str;
        if !ISA_ALPHA!(cur) {
            return 2;
        }
        cur = cur.add(1);
        while ISA_ALPHA!(cur)
            || ISA_DIGIT!(cur)
            || *cur == b'+' as _
            || *cur == b'-' as _
            || *cur == b'.' as _
        {
            cur = cur.add(1);
        }
        if !uri.is_null() {
            if !(*uri).scheme.is_null() {
                xml_free((*uri).scheme as _);
            }
            (*uri).scheme = STRNDUP!(*str, cur.offset_from(*str) as _);
        }
        *str = cur;
        0
    }
}

/*
 *    unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
 */
macro_rules! ISA_UNRESERVED {
    ($p:expr) => {
        ISA_ALPHA!($p)
            || ISA_DIGIT!($p)
            || *$p == b'-' as i8
            || *$p == b'.' as i8
            || *$p == b'_' as i8
            || *$p == b'~' as i8
    };
}

macro_rules! ISA_HEXDIG {
    ($p:expr) => {
        ISA_DIGIT!($p)
            || (*$p >= b'a' as i8 && *$p <= b'f' as i8)
            || (*$p >= b'A' as i8 && *$p <= b'F' as i8)
    };
}

/*
 *    sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
 *                     / "*" / "+" / "," / ";" / "="
 */
macro_rules! ISA_SUB_DELIM {
	($p:expr) => {
		*$p == b'!' as i8 || *$p == b'$' as i8 || *$p == b'&' as i8 ||
		*$p == b'(' as i8 || *$p == b')' as i8 || *$p == b'*' as i8 ||
		*$p == b'+' as i8 || *$p == b',' as i8 || *$p == b';' as i8 ||
		*$p == b'=' as i8 || *$p == b'\'' as i8
	}
}

/*
 *    pct-encoded   = "%" HEXDIG HEXDIG
 */
macro_rules! ISA_PCT_ENCODED {
    ($p:expr) => {
        *$p == b'%' as i8 && ISA_HEXDIG!($p.add(1)) && ISA_HEXDIG!($p.add(2))
    };
}

/*
 *    pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"
 */
macro_rules! ISA_PCHAR {
    ($p:expr) => {
        ISA_UNRESERVED!($p)
            || ISA_PCT_ENCODED!($p)
            || ISA_SUB_DELIM!($p)
            || *$p == b':' as i8
            || *$p == b'@' as i8
    };
}

/*
 * Skip to next pointer c_char, handle escaped sequences
 */

macro_rules! NEXT {
    ($p:expr) => {
        if *$p == b'%' as i8 {
            $p = $p.add(3);
        } else {
            $p = $p.add(1);
        }
    };
}

/// Parse an user information part and fills in the appropriate fields
/// of the @uri structure
///
/// userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )
///
/// Returns 0 or the error code
#[doc(alias = "xmlParse3986Userinfo")]
unsafe extern "C" fn xml_parse3986_userinfo(uri: XmlURIPtr, str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char;

        cur = *str;
        while ISA_UNRESERVED!(cur)
            || ISA_PCT_ENCODED!(cur)
            || ISA_SUB_DELIM!(cur)
            || *cur == b':' as i8
        {
            NEXT!(cur);
        }
        if *cur == b'@' as i8 {
            if !uri.is_null() {
                if !(*uri).user.is_null() {
                    xml_free((*uri).user as _);
                }
                if (*uri).cleanup & 2 != 0 {
                    (*uri).user = STRNDUP!(*str, cur.offset_from(*str) as _);
                } else {
                    (*uri).user =
                        xml_uri_unescape_string(*str, cur.offset_from(*str) as _, null_mut());
                }
            }
            *str = cur;
            return 0;
        }
        1
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
unsafe extern "C" fn xml_parse3986_dec_octet(str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char = *str;

        if !ISA_DIGIT!(cur) {
            return 1;
        }
        if !ISA_DIGIT!(cur.add(1)) {
            cur = cur.add(1);
        } else if *cur != b'0' as i8 && ISA_DIGIT!(cur.add(1)) && !ISA_DIGIT!(cur.add(2)) {
            cur = cur.add(2);
        } else if (*cur == b'1' as i8 && ISA_DIGIT!(cur.add(1)) && ISA_DIGIT!(cur.add(2)))
            || (*cur == b'2' as i8
                && *cur.add(1) >= b'0' as i8
                && *cur.add(1) <= b'4' as i8
                && ISA_DIGIT!(cur.add(2)))
            || (*cur == b'2' as i8
                && *cur.add(1) == b'5' as i8
                && *cur.add(2) >= b'0' as i8
                && *cur.add(1) <= b'5' as i8)
        {
            cur = cur.add(3);
        } else {
            return 1;
        }
        *str = cur;
        0
    }
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
unsafe extern "C" fn xml_parse3986_host(uri: XmlURIPtr, str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char = *str;

        let host: *const c_char = cur;
        /*
         * IPv6 and future addressing scheme are enclosed between brackets
         */
        'found: {
            if *cur == b'[' as i8 {
                cur = cur.add(1);
                while *cur != b']' as i8 && *cur != 0 {
                    cur = cur.add(1);
                }
                if *cur != b']' as i8 {
                    return 1;
                }
                cur = cur.add(1);
                break 'found;
            }
            /*
             * try to parse an IPv4
             */
            if ISA_DIGIT!(cur) {
                'not_ipv4: {
                    if xml_parse3986_dec_octet(&raw mut cur) != 0 {
                        break 'not_ipv4;
                    }
                    if *cur != b'.' as i8 {
                        break 'not_ipv4;
                    }
                    cur = cur.add(1);
                    if xml_parse3986_dec_octet(&raw mut cur) != 0 {
                        break 'not_ipv4;
                    }
                    if *cur != b'.' as i8 {
                        break 'not_ipv4;
                    }
                    if xml_parse3986_dec_octet(&raw mut cur) != 0 {
                        break 'not_ipv4;
                    }
                    if *cur != b'.' as i8 {
                        break 'not_ipv4;
                    }
                    if xml_parse3986_dec_octet(&raw mut cur) != 0 {
                        break 'not_ipv4;
                    }
                    break 'found;
                }
                // not_ipv4:
                cur = *str;
            }
            /*
             * then this should be a hostname which can be empty
             */
            while ISA_UNRESERVED!(cur) || ISA_PCT_ENCODED!(cur) || ISA_SUB_DELIM!(cur) {
                NEXT!(cur);
            }
        }
        // found:
        if !uri.is_null() {
            if !(*uri).authority.is_null() {
                xml_free((*uri).authority as _);
            }
            (*uri).authority = null_mut();
            if !(*uri).server.is_null() {
                xml_free((*uri).server as _);
            }
            if cur != host {
                if (*uri).cleanup & 2 != 0 {
                    (*uri).server = STRNDUP!(host, cur.offset_from(host) as _);
                } else {
                    (*uri).server =
                        xml_uri_unescape_string(host, cur.offset_from(host) as _, null_mut());
                }
            } else {
                (*uri).server = null_mut();
            }
        }
        *str = cur;
        0
    }
}

/// Parse a port part and fills in the appropriate fields of the @uri structure
///
/// `port          = *DIGIT`
///
/// Returns 0 or the error code
#[doc(alias = "xmlParse3986Port")]
unsafe extern "C" fn xml_parse3986_port(uri: XmlURIPtr, str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char = *str;
        let mut port: i32 = 0;

        if ISA_DIGIT!(cur) {
            while ISA_DIGIT!(cur) {
                let digit: i32 = *cur as i32 - b'0' as i32;

                if port > INT_MAX / 10 {
                    return 1;
                }
                port *= 10;
                if port > INT_MAX - digit {
                    return 1;
                }
                port += digit;

                cur = cur.add(1);
            }
            if !uri.is_null() {
                (*uri).port = port;
            }
            *str = cur;
            return 0;
        }
        1
    }
}

/// Parse an authority part and fills in the appropriate fields
/// of the @uri structure
///
/// `authority     = [ userinfo "@" ] host [ ":" port ]`
///
/// Returns 0 or the error code
#[doc(alias = "xmlParse3986Authority")]
unsafe extern "C" fn xml_parse3986_authority(uri: XmlURIPtr, str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char;
        let mut ret: i32;

        cur = *str;
        /*
         * try to parse an userinfo and check for the trailing @
         */
        ret = xml_parse3986_userinfo(uri, &raw mut cur);
        if ret != 0 || *cur != b'@' as i8 {
            cur = *str;
        } else {
            cur = cur.add(1);
        }
        ret = xml_parse3986_host(uri, &raw mut cur);
        if ret != 0 {
            return ret;
        }
        if *cur == b':' as i8 {
            cur = cur.add(1);
            ret = xml_parse3986_port(uri, &raw mut cur);
            if ret != 0 {
                return ret;
            }
        }
        *str = cur;
        0
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
unsafe extern "C" fn xml_parse3986_segment(
    str: *mut *const c_char,
    forbid: c_char,
    empty: i32,
) -> i32 {
    unsafe {
        let mut cur: *const c_char;

        cur = *str;
        if !ISA_PCHAR!(cur) {
            if empty != 0 {
                return 0;
            }
            return 1;
        }
        while ISA_PCHAR!(cur) && *cur != forbid {
            NEXT!(cur);
        }
        *str = cur;
        0
    }
}

/// Parse an path absolute or empty and fills in the appropriate fields of the @uri structure
///
/// `path-abempty  = *( "/" segment )`
///
/// Returns 0 or the error code
#[doc(alias = "xmlParse3986PathAbEmpty")]
unsafe extern "C" fn xml_parse3986_path_ab_empty(uri: XmlURIPtr, str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char;
        let mut ret: i32;

        cur = *str;

        while *cur == b'/' as i8 {
            cur = cur.add(1);
            ret = xml_parse3986_segment(&raw mut cur, 0, 1);
            if ret != 0 {
                return ret;
            }
        }
        if !uri.is_null() {
            if !(*uri).path.is_null() {
                xml_free((*uri).path as _);
            }
            if *str != cur {
                if (*uri).cleanup & 2 != 0 {
                    (*uri).path = STRNDUP!(*str, cur.offset_from(*str) as _);
                } else {
                    (*uri).path =
                        xml_uri_unescape_string(*str, cur.offset_from(*str) as _, null_mut());
                }
            } else {
                (*uri).path = null_mut();
            }
        }
        *str = cur;
        0
    }
}

/// Parse an path absolute and fills in the appropriate fields of the @uri structure
///
/// `path-absolute = "/" [ segment-nz *( "/" segment ) ]`
///
/// Returns 0 or the error code
#[doc(alias = "xmlParse3986PathAbsolute")]
unsafe extern "C" fn xml_parse3986_path_absolute(uri: XmlURIPtr, str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char;
        let mut ret: i32;

        cur = *str;

        if *cur != b'/' as i8 {
            return 1;
        }
        cur = cur.add(1);
        ret = xml_parse3986_segment(&raw mut cur, 0, 0);
        if ret == 0 {
            while *cur == b'/' as i8 {
                cur = cur.add(1);
                ret = xml_parse3986_segment(&raw mut cur, 0, 1);
                if ret != 0 {
                    return ret;
                }
            }
        }
        if !uri.is_null() {
            if !(*uri).path.is_null() {
                xml_free((*uri).path as _);
            }
            if cur != *str {
                if (*uri).cleanup & 2 != 0 {
                    (*uri).path = STRNDUP!(*str, cur.offset_from(*str) as _);
                } else {
                    (*uri).path =
                        xml_uri_unescape_string(*str, cur.offset_from(*str) as _, null_mut());
                }
            } else {
                (*uri).path = null_mut();
            }
        }
        *str = cur;
        0
    }
}

/// Parse an path without root and fills in the appropriate fields
/// of the @uri structure
///
/// `path-rootless = segment-nz *( "/" segment )`
///
/// Returns 0 or the error code
#[doc(alias = "xmlParse3986PathRootless")]
unsafe extern "C" fn xml_parse3986_path_rootless(uri: XmlURIPtr, str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char;
        let mut ret: i32;

        cur = *str;

        ret = xml_parse3986_segment(&raw mut cur, 0, 0);
        if ret != 0 {
            return ret;
        }
        while *cur == b'/' as i8 {
            cur = cur.add(1);
            ret = xml_parse3986_segment(&raw mut cur, 0, 1);
            if ret != 0 {
                return ret;
            }
        }
        if !uri.is_null() {
            if !(*uri).path.is_null() {
                xml_free((*uri).path as _);
            }
            if cur != *str {
                if (*uri).cleanup & 2 != 0 {
                    (*uri).path = STRNDUP!(*str, cur.offset_from(*str) as _);
                } else {
                    (*uri).path =
                        xml_uri_unescape_string(*str, cur.offset_from(*str) as _, null_mut());
                }
            } else {
                (*uri).path = null_mut();
            }
        }
        *str = cur;
        0
    }
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
unsafe extern "C" fn xml_parse3986_hier_part(uri: XmlURIPtr, str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char;
        let mut ret: i32;

        cur = *str;

        if *cur == b'/' as i8 && *cur.add(1) == b'/' as i8 {
            cur = cur.add(2);
            ret = xml_parse3986_authority(uri, &raw mut cur);
            if ret != 0 {
                return ret;
            }
            /*
             * An empty server is marked with a special URI value.
             */
            if (*uri).server.is_null() && (*uri).port == PORT_EMPTY {
                (*uri).port = PORT_EMPTY_SERVER;
            }
            ret = xml_parse3986_path_ab_empty(uri, &raw mut cur);
            if ret != 0 {
                return ret;
            }
            *str = cur;
            return 0;
        } else if *cur == b'/' as i8 {
            ret = xml_parse3986_path_absolute(uri, &raw mut cur);
            if ret != 0 {
                return ret;
            }
        } else if ISA_PCHAR!(cur) {
            ret = xml_parse3986_path_rootless(uri, &raw mut cur);
            if ret != 0 {
                return ret;
            }
        } else {
            /* path-empty is effectively empty */
            if !uri.is_null() {
                if !(*uri).path.is_null() {
                    xml_free((*uri).path as _);
                }
                (*uri).path = null_mut();
            }
        }
        *str = cur;
        0
    }
}

/*
 * unwise = "{" | "}" | "|" | "\" | "^" | "`"
 */
macro_rules! IS_UNWISE {
    ($p:expr) => {
        *$p == b'{' as i8
            || *$p == b'}' as i8
            || *$p == b'|' as i8
            || *$p == b'\\' as i8
            || *$p == b'^' as i8
            || *$p == b'[' as i8
            || *$p == b']' as i8
            || *$p == b'`' as i8
    };
}

/// Parse the query part of an URI
///
/// `query = *uric`
///
/// Returns 0 or the error code
#[doc(alias = "xmlParse3986Query")]
unsafe extern "C" fn xml_parse3986_query(uri: XmlURIPtr, str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char;

        if str.is_null() {
            return -1;
        }

        cur = *str;

        while ISA_PCHAR!(cur)
            || *cur == b'/' as i8
            || *cur == b'?' as i8
            || (!uri.is_null() && (*uri).cleanup & 1 != 0 && IS_UNWISE!(cur))
        {
            NEXT!(cur);
        }
        if !uri.is_null() {
            if !(*uri).query.is_null() {
                xml_free((*uri).query as _);
            }
            if (*uri).cleanup & 2 != 0 {
                (*uri).query = STRNDUP!(*str, cur.offset_from(*str) as _);
            } else {
                (*uri).query =
                    xml_uri_unescape_string(*str, cur.offset_from(*str) as _, null_mut());
            }

            /* Save the raw bytes of the query as well.
             * See: http://mail.gnome.org/archives/xml/2007-April/thread.html#00114
             */
            if !(*uri).query_raw.is_null() {
                xml_free((*uri).query_raw as _);
            }
            (*uri).query_raw = STRNDUP!(*str, cur.offset_from(*str) as _);
        }
        *str = cur;
        0
    }
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
unsafe extern "C" fn xml_parse3986_fragment(uri: XmlURIPtr, str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char;

        if str.is_null() {
            return -1;
        }

        cur = *str;

        while ISA_PCHAR!(cur)
            || *cur == b'/' as i8
            || *cur == b'?' as i8
            || *cur == b'[' as i8
            || *cur == b']' as i8
            || (!uri.is_null() && (*uri).cleanup & 1 != 0 && IS_UNWISE!(cur))
        {
            NEXT!(cur);
        }
        if !uri.is_null() {
            if !(*uri).fragment.is_null() {
                xml_free((*uri).fragment as _);
            }
            if (*uri).cleanup & 2 != 0 {
                (*uri).fragment = STRNDUP!(*str, cur.offset_from(*str) as _);
            } else {
                (*uri).fragment =
                    xml_uri_unescape_string(*str, cur.offset_from(*str) as _, null_mut());
            }
        }
        *str = cur;
        0
    }
}

/// Parse an URI string and fills in the appropriate fields of the @uri structure
///
/// `scheme ":" hier-part [ "?" query ] [ "#" fragment ]`
///
/// Returns 0 or the error code
#[doc(alias = "xmlParse3986URI")]
unsafe extern "C" fn xml_parse3986_uri(uri: XmlURIPtr, mut str: *const c_char) -> i32 {
    unsafe {
        let mut ret: i32;

        ret = xml_parse3986_scheme(uri, &raw mut str);
        if ret != 0 {
            return ret;
        }
        if *str != b':' as i8 {
            return 1;
        }
        str = str.add(1);
        ret = xml_parse3986_hier_part(uri, &raw mut str);
        if ret != 0 {
            return ret;
        }
        if *str == b'?' as i8 {
            str = str.add(1);
            ret = xml_parse3986_query(uri, &raw mut str);
            if ret != 0 {
                return ret;
            }
        }
        if *str == b'#' as i8 {
            str = str.add(1);
            ret = xml_parse3986_fragment(uri, &raw mut str);
            if ret != 0 {
                return ret;
            }
        }
        if *str != 0 {
            xml_clean_uri(uri);
            return 1;
        }
        0
    }
}

/// Parse an path which is not a scheme and fills in the appropriate fields of the @uri structure
///
/// `path-noscheme = segment-nz-nc *( "/" segment )`
///
/// Returns 0 or the error code
#[doc(alias = "xmlParse3986PathNoScheme")]
unsafe extern "C" fn xml_parse3986_path_no_scheme(uri: XmlURIPtr, str: *mut *const c_char) -> i32 {
    unsafe {
        let mut cur: *const c_char;
        let mut ret: i32;

        cur = *str;

        ret = xml_parse3986_segment(&raw mut cur, b':' as i8, 0);
        if ret != 0 {
            return ret;
        }
        while *cur == b'/' as i8 {
            cur = cur.add(1);
            ret = xml_parse3986_segment(&raw mut cur, 0, 1);
            if ret != 0 {
                return ret;
            }
        }
        if !uri.is_null() {
            if !(*uri).path.is_null() {
                xml_free((*uri).path as _);
            }
            if cur != *str {
                if (*uri).cleanup & 2 != 0 {
                    (*uri).path = STRNDUP!(*str, cur.offset_from(*str) as _);
                } else {
                    (*uri).path =
                        xml_uri_unescape_string(*str, cur.offset_from(*str) as _, null_mut());
                }
            } else {
                (*uri).path = null_mut();
            }
        }
        *str = cur;
        0
    }
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
unsafe extern "C" fn xml_parse3986_relative_ref(uri: XmlURIPtr, mut str: *const c_char) -> i32 {
    unsafe {
        let mut ret: i32;

        if *str == b'/' as i8 && *str.add(1) == b'/' as i8 {
            str = str.add(2);
            ret = xml_parse3986_authority(uri, &raw mut str);
            if ret != 0 {
                return ret;
            }
            ret = xml_parse3986_path_ab_empty(uri, &raw mut str);
            if ret != 0 {
                return ret;
            }
        } else if *str == b'/' as i8 {
            ret = xml_parse3986_path_absolute(uri, &raw mut str);
            if ret != 0 {
                return ret;
            }
        } else if ISA_PCHAR!(str) {
            ret = xml_parse3986_path_no_scheme(uri, &raw mut str);
            if ret != 0 {
                return ret;
            }
        } else {
            /* path-empty is effectively empty */
            if !uri.is_null() {
                if !(*uri).path.is_null() {
                    xml_free((*uri).path as _);
                }
                (*uri).path = null_mut();
            }
        }

        if *str == b'?' as i8 {
            str = str.add(1);
            ret = xml_parse3986_query(uri, &raw mut str);
            if ret != 0 {
                return ret;
            }
        }
        if *str == b'#' as i8 {
            str = str.add(1);
            ret = xml_parse3986_fragment(uri, &raw mut str);
            if ret != 0 {
                return ret;
            }
        }
        if *str != 0 {
            xml_clean_uri(uri);
            return 1;
        }
        0
    }
}

/// Parse an URI reference string and fills in the appropriate fields of the @uri structure
///
/// `URI-reference = URI / relative-refe`
///
/// Returns 0 or the error code
#[doc(alias = "xmlParse3986URIReference")]
unsafe extern "C" fn xml_parse3986_uri_reference(uri: XmlURIPtr, str: *const c_char) -> i32 {
    unsafe {
        let mut ret: i32;

        if str.is_null() {
            return -1;
        }
        xml_clean_uri(uri);

        /*
         * Try first to parse absolute refs, then fallback to relative if
         * it fails.
         */
        ret = xml_parse3986_uri(uri, str);
        if ret != 0 {
            xml_clean_uri(uri);
            ret = xml_parse3986_relative_ref(uri, str);
            if ret != 0 {
                xml_clean_uri(uri);
                return ret;
            }
        }
        0
    }
}

/// Parse an URI but allows to keep intact the original fragments.
///
/// `URI-reference = URI / relative-refe`
///
/// Returns a newly built xmlURIPtr or NULL in case of error
#[doc(alias = "xmlParseURIRaw")]
pub unsafe extern "C" fn xml_parse_uri_raw(str: *const c_char, raw: i32) -> XmlURIPtr {
    unsafe {
        let ret: i32;

        if str.is_null() {
            return null_mut();
        }
        let uri: XmlURIPtr = xml_create_uri();
        if !uri.is_null() {
            if raw != 0 {
                (*uri).cleanup |= 2;
            }
            ret = xml_parse_uri_reference(uri, str);
            if ret != 0 {
                xml_free_uri(uri);
                return null_mut();
            }
        }
        uri
    }
}

/// Parse an URI reference string based on RFC 3986 and fills in the
/// appropriate fields of the @uri structure
///
/// `URI-reference = URI / relative-refe`
///
/// Returns 0 or the error code
#[doc(alias = "xmlParseURIReference")]
pub unsafe extern "C" fn xml_parse_uri_reference(uri: XmlURIPtr, str: *const c_char) -> i32 {
    unsafe { xml_parse3986_uri_reference(uri, str) }
}

unsafe extern "C" fn is_hex(c: c_char) -> i32 {
    if (c >= b'0' as i8 && c <= b'9' as i8)
        || (c >= b'a' as i8 && c <= b'f' as i8)
        || (c >= b'A' as i8 && c <= b'F' as i8)
    {
        return 1;
    }
    0
}

/// Unescaping routine, but does not check that the string is an URI. The
/// output is a direct unsigned char translation of %XX values (no encoding)
/// Note that the length of the result can only be smaller or same size as
/// the input string.
///
/// Returns a copy of the string, but unescaped, will return NULL only in case of error
#[doc(alias = "xmlURIUnescapeString")]
pub unsafe extern "C" fn xml_uri_unescape_string(
    str: *const c_char,
    mut len: i32,
    target: *mut c_char,
) -> *mut c_char {
    unsafe {
        let ret: *mut c_char;
        let mut out: *mut c_char;
        let mut input: *const c_char;

        if str.is_null() {
            return null_mut();
        }
        if len <= 0 {
            len = strlen(str) as _;
        }
        if len < 0 {
            return null_mut();
        }

        if target.is_null() {
            ret = xml_malloc_atomic(len as usize + 1) as *mut c_char;
            if ret.is_null() {
                xml_uri_err_memory("unescaping URI value\n");
                return null_mut();
            }
        } else {
            ret = target;
        }
        input = str;
        out = ret;
        while len > 0 {
            if len > 2
                && *input == b'%' as i8
                && is_hex(*input.add(1)) != 0
                && is_hex(*input.add(2)) != 0
            {
                let mut c: i32 = 0;
                input = input.add(1);
                if *input >= b'0' as i8 && *input <= b'9' as i8 {
                    c = (*input - b'0' as i8) as i32;
                } else if *input >= b'a' as i8 && *input <= b'f' as i8 {
                    c = (*input - b'a' as i8) as i32 + 10;
                } else if *input >= b'A' as i8 && *input <= b'F' as i8 {
                    c = (*input - b'A' as i8) as i32 + 10;
                }
                input = input.add(1);
                if *input >= b'0' as i8 && *input <= b'9' as i8 {
                    c = c * 16 + (*input - b'0' as i8) as i32;
                } else if *input >= b'a' as i8 && *input <= b'f' as i8 {
                    c = c * 16 + (*input - b'a' as i8) as i32 + 10;
                } else if *input >= b'A' as i8 && *input <= b'F' as i8 {
                    c = c * 16 + (*input - b'A' as i8) as i32 + 10;
                }
                input = input.add(1);
                len -= 3;
                /* Explicit sign change */
                *out = c as c_char;
                out = out.add(1);
            } else {
                *out = *input;
                out = out.add(1);
                input = input.add(1);
                len -= 1;
            }
        }
        *out = 0;
        ret
    }
}

/// Free up the xmlURI struct
#[doc(alias = "xmlFreeURI")]
pub unsafe extern "C" fn xml_free_uri(uri: XmlURIPtr) {
    unsafe {
        if uri.is_null() {
            return;
        }

        if !(*uri).scheme.is_null() {
            xml_free((*uri).scheme as _);
        }
        if !(*uri).server.is_null() {
            xml_free((*uri).server as _);
        }
        if !(*uri).user.is_null() {
            xml_free((*uri).user as _);
        }
        if !(*uri).path.is_null() {
            xml_free((*uri).path as _);
        }
        if !(*uri).fragment.is_null() {
            xml_free((*uri).fragment as _);
        }
        if !(*uri).opaque.is_null() {
            xml_free((*uri).opaque as _);
        }
        if !(*uri).authority.is_null() {
            xml_free((*uri).authority as _);
        }
        if !(*uri).query.is_null() {
            xml_free((*uri).query as _);
        }
        if !(*uri).query_raw.is_null() {
            xml_free((*uri).query_raw as _);
        }
        xml_free(uri as _);
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_parse_urireference() {
        unsafe {
            let mut leaks = 0;

            for n_uri in 0..GEN_NB_XML_URIPTR {
                for n_str in 0..GEN_NB_CONST_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let uri = gen_xml_uriptr(n_uri, 0);
                    let str = gen_const_char_ptr(n_str, 1);

                    let ret_val = xml_parse_uri_reference(uri, str);
                    desret_int(ret_val);
                    des_xml_uriptr(n_uri, uri, 0);
                    des_const_char_ptr(n_str, str, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlParseURIReference",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlParseURIReference()"
                        );
                        eprint!(" {}", n_uri);
                        eprintln!(" {}", n_str);
                    }
                }
            }
        }
    }
}

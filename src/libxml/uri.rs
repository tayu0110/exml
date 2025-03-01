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

use libc::{INT_MAX, memcpy, memset, snprintf, strlen};

use crate::error::__xml_raise_error;

use super::{
    globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
    xmlstring::{
        XmlChar, xml_str_equal, xml_strchr, xml_strcmp, xml_strdup, xml_strlen, xml_strndup,
    },
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

unsafe fn xml_uri_err_memory(extra: &str) {
    unsafe {
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

/// Expresses the URI of the reference in terms relative to the base.  
/// Some examples of this operation include:
/// ```ignore
///    base = "http://site1.com/docs/book1.html"
///    URI input                        URI returned
///    docs/pic1.gif                    pic1.gif
///    docs/img/pic1.gif                img/pic1.gif
///    img/pic1.gif                     ../img/pic1.gif
///    http://site1.com/docs/pic1.gif   pic1.gif
///    http://site2.com/docs/pic1.gif   http://site2.com/docs/pic1.gif
///
///    base = "docs/book1.html"
///
///    URI input                        URI returned
///    docs/pic1.gif                    pic1.gif
///    docs/img/pic1.gif                img/pic1.gif
///    img/pic1.gif                     ../img/pic1.gif
///    http://site1.com/docs/pic1.gif   http://site1.com/docs/pic1.gif
/// ```
///
/// # Note
/// if the URI reference is really weird or complicated, it may be
/// worthwhile to first convert it into a "nice" one by calling
/// xmlBuildURI (using 'base') before calling this routine,
/// since this routine (for reasonable efficiency) assumes URI has
/// already been through some validation.
///
/// Returns a new URI string (to be freed by the caller) or NULL in case error.
#[doc(alias = "xmlBuildRelativeURI")]
pub unsafe extern "C" fn xml_build_relative_uri(
    uri: *const XmlChar,
    base: *const XmlChar,
) -> *mut XmlChar {
    unsafe {
        let mut val: *mut XmlChar = null_mut();
        let mut ret: i32;
        let mut ix: i32;
        let mut nbslash: i32 = 0;
        let len: i32;
        let mut bas: XmlURIPtr = null_mut();
        let mut bptr: *mut XmlChar;
        let uptr: *mut XmlChar;
        let mut vptr: *mut XmlChar;
        let mut remove_path: i32 = 0;

        if uri.is_null() || *uri == 0 {
            return null_mut();
        }

        /*
         * First parse URI into a standard form
         */
        let refe: XmlURIPtr = xml_create_uri();
        if refe.is_null() {
            return null_mut();
        }
        'done: {
            /* If URI not already in "relative" form */
            if *uri.add(0) != b'.' {
                ret = xml_parse_uri_reference(refe as _, uri as _);
                if ret != 0 {
                    break 'done; /* Error in URI, return NULL */
                }
            } else {
                (*refe).path = xml_strdup(uri as _) as *mut c_char;
            }

            /*
             * Next parse base into the same standard form
             */
            if base.is_null() || *base == 0 {
                val = xml_strdup(uri);
                break 'done;
            }
            bas = xml_create_uri();
            if bas.is_null() {
                break 'done;
            }
            if *base.add(0) != b'.' {
                ret = xml_parse_uri_reference(bas, base as *const c_char);
                if ret != 0 {
                    break 'done; /* Error in base, return NULL */
                }
            } else {
                (*bas).path = xml_strdup(base) as *mut c_char;
            }

            /*
             * If the scheme / server on the URI differs from the base,
             * just return the URI
             */
            if !(*refe).scheme.is_null()
                && ((*bas).scheme.is_null()
                    || xml_strcmp(
                        (*bas).scheme as *mut XmlChar,
                        (*refe).scheme as *mut XmlChar,
                    ) != 0
                    || xml_strcmp(
                        (*bas).server as *mut XmlChar,
                        (*refe).server as *mut XmlChar,
                    ) != 0
                    || (*bas).port != (*refe).port)
            {
                val = xml_strdup(uri);
                break 'done;
            }
            if xml_str_equal((*bas).path as *mut XmlChar, (*refe).path as *mut XmlChar) {
                val = xml_strdup(c"".as_ptr() as _);
                break 'done;
            }
            if (*bas).path.is_null() {
                val = xml_strdup((*refe).path as *mut XmlChar);
                break 'done;
            }
            if (*refe).path.is_null() {
                (*refe).path = c"/".as_ptr() as _;
                remove_path = 1;
            }

            /*
             * At this point (at last!) we can compare the two paths
             *
             * First we take care of the special case where either of the
             * two path components may be missing (bug 316224)
             */
            bptr = (*bas).path as *mut XmlChar;
            {
                let mut rptr: *mut XmlChar = (*refe).path as *mut XmlChar;
                let mut pos: i32 = 0;

                /*
                 * Next we compare the two strings and find where they first differ
                 */
                if *rptr == b'.' && *rptr.add(1) == b'/' {
                    rptr = rptr.add(2);
                }
                if *bptr == b'.' && *bptr.add(1) == b'/' {
                    bptr = bptr.add(2);
                } else if *bptr == b'/' && *rptr != b'/' {
                    bptr = bptr.add(1);
                }
                while *bptr.add(pos as usize) == *rptr.add(pos as usize)
                    && *bptr.add(pos as usize) != 0
                {
                    pos += 1;
                }

                if *bptr.add(pos as usize) == *rptr.add(pos as usize) {
                    val = xml_strdup(c"".as_ptr() as _);
                    break 'done;
                }

                /*
                 * In URI, "back up" to the last '/' encountered.  This will be the
                 * beginning of the "unique" suffix of URI
                 */
                ix = pos;
                while ix > 0 {
                    if *rptr.add(ix as usize - 1) == b'/' {
                        break;
                    }
                    ix -= 1;
                }
                uptr = rptr.add(ix as usize) as *mut XmlChar;

                /*
                 * In base, count the number of '/' from the differing point
                 */
                while *bptr.add(ix as usize) != 0 {
                    if *bptr.add(ix as usize) == b'/' {
                        nbslash += 1;
                    }

                    ix += 1;
                }

                /*
                 * e.g: URI="foo/" base="foo/bar" -> "./"
                 */
                if nbslash == 0 && *uptr.add(0) == 0 {
                    val = xml_strdup(c"./".as_ptr() as _);
                    break 'done;
                }

                len = xml_strlen(uptr as _) + 1;
            }

            if nbslash == 0 {
                if !uptr.is_null() {
                    /* exception characters from xmlSaveUri */
                    val = xml_uri_escape_str(uptr, c"/;&=+$,".as_ptr() as _);
                }
                break 'done;
            }

            /*
             * Allocate just enough space for the returned string -
             * length of the remainder of the URI, plus enough space
             * for the "../" groups, plus one for the terminator
             */
            val = xml_malloc(len as usize + 3 * nbslash as usize) as *mut XmlChar;
            if val.is_null() {
                xml_uri_err_memory("building relative URI\n");
                break 'done;
            }
            vptr = val;
            /*
             * Put in as many "../" as needed
             */
            while nbslash > 0 {
                *vptr = b'.';
                vptr = vptr.add(1);
                *vptr = b'.';
                vptr = vptr.add(1);
                *vptr = b'/';
                vptr = vptr.add(1);
                nbslash -= 1;
            }
            /*
             * Finish up with the end of the URI
             */
            if !uptr.is_null() {
                if vptr > val && len > 0 && *uptr.add(0) == b'/' && *vptr.sub(1) == b'/' {
                    memcpy(vptr as _, uptr.add(1) as _, len as usize - 1);
                    *vptr.add(len as usize - 2) = 0;
                } else {
                    memcpy(vptr as _, uptr as _, len as usize);
                    *vptr.add(len as usize - 1) = 0;
                }
            } else {
                *vptr.add(len as usize - 1) = 0;
            }

            /* escape the freshly-built path */
            vptr = val;
            /* exception characters from xmlSaveUri */
            val = xml_uri_escape_str(vptr, c"/;&=+$,".as_ptr() as _);
            xml_free(vptr as _);
        }

        // done:
        /*
         * Free the working variables
         */
        if remove_path != 0 {
            (*refe).path = null_mut();
        }
        if !refe.is_null() {
            xml_free_uri(refe);
        }
        if !bas.is_null() {
            xml_free_uri(bas);
        }

        val
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

/// Parse an URI based on RFC 3986
///
/// `URI-reference = [ absoluteURI | relativeURI ] [ "#" fragment ]`
///
/// Returns a newly built xmlURIPtr or NULL in case of error
#[doc(alias = "xmlParseURI")]
pub unsafe extern "C" fn xml_parse_uri(str: *const c_char) -> XmlURIPtr {
    unsafe {
        let ret: i32;

        if str.is_null() {
            return null_mut();
        }
        let uri: XmlURIPtr = xml_create_uri();
        if !uri.is_null() {
            ret = xml_parse3986_uri_reference(uri, str);
            if ret != 0 {
                xml_free_uri(uri);
                return null_mut();
            }
        }
        uri
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

/*
 * lowalpha = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" |
 *            "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" |
 *            "u" | "v" | "w" | "x" | "y" | "z"
 */
macro_rules! IS_LOWALPHA {
    ($x:expr) => {
        $x >= b'a' as i8 && $x <= b'z' as i8
    };
}

/*
 * upalpha = "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" |
 *           "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" |
 *           "U" | "V" | "W" | "X" | "Y" | "Z"
 */
macro_rules! IS_UPALPHA {
    ($x:expr) => {
        $x >= b'A' as i8 && $x <= b'Z' as i8
    };
}

/*
 * Old rule from 2396 used in legacy handling code
 * alpha    = lowalpha | upalpha
 */
macro_rules! IS_ALPHA {
    ($x:expr) => {
        IS_LOWALPHA!($x) || IS_UPALPHA!($x)
    };
}

/*
 * digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
 */
macro_rules! IS_DIGIT {
    ($x:expr) => {
        $x >= b'0' as i8 && $x <= b'9' as i8
    };
}

/*
 * alphanum = alpha | digit
 */
macro_rules! IS_ALPHANUM {
    ($x:expr) => {
        IS_ALPHA!($x) || IS_DIGIT!($x)
    };
}

/*
 * reserved = ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | "," |
 *            "[" | "]"
 */
macro_rules! IS_RESERVED {
	($x:expr) => {
		$x == b';' as i8 || $x == b'/' as i8 || $x == b'?' as i8 || $x == b':' as i8 ||
		$x == b'@' as i8 || $x == b'&' as i8 || $x == b'=' as i8 || $x == b'+' as i8 ||
		$x == b'$' as i8 || $x == b',' as i8 || $x == b'[' as i8 || $x == b']' as i8
	}
}

/*
 * mark = "-" | "_" | "." | "!" | "~" | "*" | "'" | "(" | ")"
 */
macro_rules! IS_MARK {
    ($x:expr) => {
        $x == b'-' as i8
            || $x == b'_' as i8
            || $x == b'.' as i8
            || $x == b'!' as i8
            || $x == b'~' as i8
            || $x == b'*' as i8
            || $x == b'\'' as i8
            || $x == b'(' as i8
            || $x == b')' as i8
    };
}

/*
 * unreserved = alphanum | mark
 */
macro_rules! IS_UNRESERVED {
    ($x:expr) => {
        IS_ALPHANUM!($x) || IS_MARK!($x)
    };
}

/// The definition of the URI regexp in the above RFC has no size limit
/// In practice they are usually relatively short except for the
/// data URI scheme as defined in RFC 2397. Even for data URI the usual
/// maximum size before hitting random practical limits is around 64 KB
/// and 4KB is usually a maximum admitted limit for proper operations.
/// The value below is more a security limit than anything else and
/// really should never be hit by 'normal' operations
/// Set to 1 MByte in 2012, this is only enforced on output
const MAX_URI_LENGTH: usize = 1024 * 1024;

/// Function to handle properly a reallocation when saving an URI
/// Also imposes some limit on the length of an URI string output
#[doc(alias = "xmlSaveUriRealloc")]
unsafe extern "C" fn xml_save_uri_realloc(ret: *mut XmlChar, max: *mut i32) -> *mut XmlChar {
    unsafe {
        if *max > MAX_URI_LENGTH as i32 {
            xml_uri_err_memory("reaching arbitrary MAX_URI_LENGTH limit\n");
            return null_mut();
        }
        let tmp: i32 = *max * 2;
        let temp: *mut XmlChar = xml_realloc(ret as _, tmp as usize + 1) as *mut XmlChar;
        if temp.is_null() {
            xml_uri_err_memory("saving URI\n");
            return null_mut();
        }
        *max = tmp;
        temp
    }
}

/// Save the URI as an escaped string
///
/// Returns a new string (to be deallocated by caller)
#[doc(alias = "xmlSaveUri")]
pub unsafe extern "C" fn xml_save_uri(uri: XmlURIPtr) -> *mut XmlChar {
    unsafe {
        let mut ret: *mut XmlChar;
        let mut temp: *mut XmlChar;
        let mut p: *const c_char;
        let mut len: i32;
        let mut max: i32;

        if uri.is_null() {
            return null_mut();
        }

        max = 80;
        ret = xml_malloc_atomic(max as usize + 1) as *mut XmlChar;
        if ret.is_null() {
            xml_uri_err_memory("saving URI\n");
            return null_mut();
        }
        len = 0;

        'mem_error: {
            if !(*uri).scheme.is_null() {
                p = (*uri).scheme;
                while *p != 0 {
                    if len >= max {
                        temp = xml_save_uri_realloc(ret, &raw mut max);
                        if temp.is_null() {
                            break 'mem_error;
                        }
                        ret = temp;
                    }
                    *ret.add(len as usize) = *p as _;
                    len += 1;
                    p = p.add(1);
                }
                if len >= max {
                    temp = xml_save_uri_realloc(ret, &raw mut max);
                    if temp.is_null() {
                        break 'mem_error;
                    }
                    ret = temp;
                }
                *ret.add(len as usize) = b':';
                len += 1;
            }
            if !(*uri).opaque.is_null() {
                p = (*uri).opaque;
                while *p != 0 {
                    if len + 3 >= max {
                        temp = xml_save_uri_realloc(ret, &raw mut max);
                        if temp.is_null() {
                            break 'mem_error;
                        }
                        ret = temp;
                    }
                    if IS_RESERVED!(*p) || IS_UNRESERVED!(*p) {
                        *ret.add(len as usize) = *p as _;
                        len += 1;
                        p = p.add(1);
                    } else {
                        let val: i32 = *(p as *mut u8) as i32;
                        p = p.add(1);
                        let hi: i32 = val / 0x10;
                        let lo: i32 = val % 0x10;
                        *ret.add(len as usize) = b'%';
                        len += 1;
                        *ret.add(len as usize) = hi as u8 + if hi > 9 { b'A' - 10 } else { b'0' };
                        len += 1;
                        *ret.add(len as usize) = lo as u8 + if lo > 9 { b'A' - 10 } else { b'0' };
                        len += 1;
                    }
                }
            } else {
                if !(*uri).server.is_null() || (*uri).port != PORT_EMPTY {
                    if len + 3 >= max {
                        temp = xml_save_uri_realloc(ret, &raw mut max);
                        if temp.is_null() {
                            break 'mem_error;
                        }
                        ret = temp;
                    }
                    *ret.add(len as usize) = b'/';
                    len += 1;
                    *ret.add(len as usize) = b'/';
                    len += 1;
                    if !(*uri).user.is_null() {
                        p = (*uri).user;
                        while *p != 0 {
                            if len + 3 >= max {
                                temp = xml_save_uri_realloc(ret, &raw mut max);
                                if temp.is_null() {
                                    break 'mem_error;
                                }
                                ret = temp;
                            }
                            if IS_UNRESERVED!(*p)
                                || *p == b';' as i8
                                || *p == b':' as i8
                                || *p == b'&' as i8
                                || *p == b'=' as i8
                                || *p == b'+' as i8
                                || *p == b'$' as i8
                                || *p == b',' as i8
                            {
                                *ret.add(len as usize) = *p as _;
                                len += 1;
                                p = p.add(1);
                            } else {
                                let val: i32 = *(p as *mut u8) as i32;
                                p = p.add(1);
                                let hi: i32 = val / 0x10;
                                let lo: i32 = val % 0x10;
                                *ret.add(len as usize) = b'%';
                                len += 1;
                                *ret.add(len as usize) =
                                    hi as u8 + if hi > 9 { b'A' - 10 } else { b'0' };
                                len += 1;
                                *ret.add(len as usize) =
                                    lo as u8 + if lo > 9 { b'A' - 10 } else { b'0' };
                                len += 1;
                            }
                        }
                        if len + 3 >= max {
                            temp = xml_save_uri_realloc(ret, &raw mut max);
                            if temp.is_null() {
                                break 'mem_error;
                            }
                            ret = temp;
                        }
                        *ret.add(len as usize) = b'@';
                        len += 1;
                    }
                    if !(*uri).server.is_null() {
                        p = (*uri).server;
                        while *p != 0 {
                            if len >= max {
                                temp = xml_save_uri_realloc(ret, &raw mut max);
                                if temp.is_null() {
                                    break 'mem_error;
                                }
                                ret = temp;
                            }
                            /* TODO: escaping? */
                            *ret.add(len as usize) = *p as XmlChar;
                            len += 1;
                            p = p.add(1);
                        }
                    }
                    if (*uri).port > 0 {
                        if len + 10 >= max {
                            temp = xml_save_uri_realloc(ret, &raw mut max);
                            if temp.is_null() {
                                break 'mem_error;
                            }
                            ret = temp;
                        }
                        len += snprintf(
                            ret.add(len as usize) as _,
                            max as usize - len as usize,
                            c":%d".as_ptr() as _,
                            (*uri).port,
                        );
                    }
                } else if !(*uri).authority.is_null() {
                    if len + 3 >= max {
                        temp = xml_save_uri_realloc(ret, &raw mut max);
                        if temp.is_null() {
                            break 'mem_error;
                        }
                        ret = temp;
                    }
                    *ret.add(len as usize) = b'/';
                    len += 1;
                    *ret.add(len as usize) = b'/';
                    len += 1;
                    p = (*uri).authority;
                    while *p != 0 {
                        if len + 3 >= max {
                            temp = xml_save_uri_realloc(ret, &raw mut max);
                            if temp.is_null() {
                                break 'mem_error;
                            }
                            ret = temp;
                        }
                        if IS_UNRESERVED!(*p)
                            || *p == b'$' as i8
                            || *p == b',' as i8
                            || *p == b';' as i8
                            || *p == b':' as i8
                            || *p == b'@' as i8
                            || *p == b'&' as i8
                            || *p == b'=' as i8
                            || *p == b'+' as i8
                        {
                            *ret.add(len as usize) = *p as _;
                            len += 1;
                            p = p.add(1);
                        } else {
                            let val: i32 = *(p as *mut u8) as i32;
                            p = p.add(1);
                            let hi: i32 = val / 0x10;
                            let lo: i32 = val % 0x10;
                            *ret.add(len as usize) = b'%';
                            len += 1;
                            *ret.add(len as usize) =
                                hi as u8 + if hi > 9 { b'A' - 10 } else { b'0' };
                            len += 1;
                            *ret.add(len as usize) =
                                lo as u8 + if lo > 9 { b'A' - 10 } else { b'0' };
                            len += 1;
                        }
                    }
                } else if !(*uri).scheme.is_null() && len + 3 >= max {
                    temp = xml_save_uri_realloc(ret, &raw mut max);
                    if temp.is_null() {
                        break 'mem_error;
                    }
                    ret = temp;
                }
                if !(*uri).path.is_null() {
                    p = (*uri).path;
                    /*
                     * the colon in file:///d: should not be escaped or
                     * Windows accesses fail later.
                     */
                    if !(*uri).scheme.is_null()
                        && *p.add(0) == b'/' as i8
                        && ((*p.add(1) >= b'a' as i8 && *p.add(1) <= b'z' as i8)
                            || (*p.add(1) >= b'A' as i8 && *p.add(1) <= b'Z' as i8))
                        && *p.add(2) == b':' as i8
                        && xml_str_equal((*uri).scheme as _, c"file".as_ptr() as _)
                    {
                        if len + 3 >= max {
                            temp = xml_save_uri_realloc(ret, &raw mut max);
                            if temp.is_null() {
                                break 'mem_error;
                            }
                            ret = temp;
                        }
                        *ret.add(len as usize) = *p as _;
                        len += 1;
                        p = p.add(1);
                        *ret.add(len as usize) = *p as _;
                        len += 1;
                        p = p.add(1);
                        *ret.add(len as usize) = *p as _;
                        len += 1;
                        p = p.add(1);
                    }
                    while *p != 0 {
                        if len + 3 >= max {
                            temp = xml_save_uri_realloc(ret, &raw mut max);
                            if temp.is_null() {
                                break 'mem_error;
                            }
                            ret = temp;
                        }
                        if IS_UNRESERVED!(*p)
                            || *p == b'/' as i8
                            || *p == b';' as i8
                            || *p == b'@' as i8
                            || *p == b'&' as i8
                            || *p == b'=' as i8
                            || *p == b'+' as i8
                            || *p == b'$' as i8
                            || *p == b',' as i8
                        {
                            *ret.add(len as usize) = *p as _;
                            len += 1;
                            p = p.add(1);
                        } else {
                            let val: i32 = *(p as *mut u8) as i32;
                            p = p.add(1);
                            let hi: i32 = val / 0x10;
                            let lo: i32 = val % 0x10;
                            *ret.add(len as usize) = b'%';
                            len += 1;
                            *ret.add(len as usize) =
                                hi as u8 + if hi > 9 { b'A' - 10 } else { b'0' };
                            len += 1;
                            *ret.add(len as usize) =
                                lo as u8 + if lo > 9 { b'A' - 10 } else { b'0' };
                            len += 1;
                        }
                    }
                }
                if !(*uri).query_raw.is_null() {
                    if len + 1 >= max {
                        temp = xml_save_uri_realloc(ret, &raw mut max);
                        if temp.is_null() {
                            break 'mem_error;
                        }
                        ret = temp;
                    }
                    *ret.add(len as usize) = b'?';
                    len += 1;
                    p = (*uri).query_raw;
                    while *p != 0 {
                        if len + 1 >= max {
                            temp = xml_save_uri_realloc(ret, &raw mut max);
                            if temp.is_null() {
                                break 'mem_error;
                            }
                            ret = temp;
                        }
                        *ret.add(len as usize) = *p as _;
                        len += 1;
                        p = p.add(1);
                    }
                } else if !(*uri).query.is_null() {
                    if len + 3 >= max {
                        temp = xml_save_uri_realloc(ret, &raw mut max);
                        if temp.is_null() {
                            break 'mem_error;
                        }
                        ret = temp;
                    }
                    *ret.add(len as usize) = b'?';
                    len += 1;
                    p = (*uri).query;
                    while *p != 0 {
                        if len + 3 >= max {
                            temp = xml_save_uri_realloc(ret, &raw mut max);
                            if temp.is_null() {
                                break 'mem_error;
                            }
                            ret = temp;
                        }
                        if IS_UNRESERVED!(*p) || IS_RESERVED!(*p) {
                            *ret.add(len as usize) = *p as _;
                            len += 1;
                            p = p.add(1);
                        } else {
                            let val: i32 = *(p as *mut u8) as i32;
                            p = p.add(1);
                            let hi: i32 = val / 0x10;
                            let lo: i32 = val % 0x10;
                            *ret.add(len as usize) = b'%';
                            len += 1;
                            *ret.add(len as usize) =
                                hi as u8 + if hi > 9 { b'A' - 10 } else { b'0' };
                            len += 1;
                            *ret.add(len as usize) =
                                lo as u8 + if lo > 9 { b'A' - 10 } else { b'0' };
                            len += 1;
                        }
                    }
                }
            }
            if !(*uri).fragment.is_null() {
                if len + 3 >= max {
                    temp = xml_save_uri_realloc(ret, &raw mut max);
                    if temp.is_null() {
                        break 'mem_error;
                    }
                    ret = temp;
                }
                *ret.add(len as usize) = b'#';
                len += 1;
                p = (*uri).fragment;
                while *p != 0 {
                    if len + 3 >= max {
                        temp = xml_save_uri_realloc(ret, &raw mut max);
                        if temp.is_null() {
                            break 'mem_error;
                        }
                        ret = temp;
                    }
                    if IS_UNRESERVED!(*p) || IS_RESERVED!(*p) {
                        *ret.add(len as usize) = *p as _;
                        len += 1;
                        p = p.add(1);
                    } else {
                        let val: i32 = *(p as *mut u8) as i32;
                        p = p.add(1);
                        let hi: i32 = val / 0x10;
                        let lo: i32 = val % 0x10;
                        *ret.add(len as usize) = b'%';
                        len += 1;
                        *ret.add(len as usize) = hi as u8 + if hi > 9 { b'A' - 10 } else { b'0' };
                        len += 1;
                        *ret.add(len as usize) = lo as u8 + if lo > 9 { b'A' - 10 } else { b'0' };
                        len += 1;
                    }
                }
            }
            if len >= max {
                temp = xml_save_uri_realloc(ret, &raw mut max);
                if temp.is_null() {
                    break 'mem_error;
                }
                ret = temp;
            }
            *ret.add(len as usize) = 0;
            return ret;
        }

        // mem_error:
        xml_free(ret as _);
        null_mut()
    }
}

/// This routine escapes a string to hex, ignoring reserved characters
/// (a-z, A-Z, 0-9, "@-_.!~*'()") and the characters in the exception list.
///
/// Returns a new escaped string or NULL in case of error.
#[doc(alias = "xmlURIEscapeStr")]
pub unsafe extern "C" fn xml_uri_escape_str(
    str: *const XmlChar,
    list: *const XmlChar,
) -> *mut XmlChar {
    unsafe {
        let mut ret: *mut XmlChar;
        let mut ch: XmlChar;
        let mut temp: *mut XmlChar;
        let mut input: *const XmlChar;
        let mut len: i32;
        let mut out: i32;

        if str.is_null() {
            return null_mut();
        }
        if *str.add(0) == 0 {
            return xml_strdup(str);
        }
        len = xml_strlen(str);
        if len <= 0 {
            return null_mut();
        }

        len += 20;
        ret = xml_malloc_atomic(len as usize) as *mut XmlChar;
        if ret.is_null() {
            xml_uri_err_memory("escaping URI value\n");
            return null_mut();
        }
        input = str as *const XmlChar;
        out = 0;
        while *input != 0 {
            if len - out <= 3 {
                temp = xml_save_uri_realloc(ret, &raw mut len);
                if temp.is_null() {
                    xml_uri_err_memory("escaping URI value\n");
                    xml_free(ret as _);
                    return null_mut();
                }
                ret = temp;
            }

            ch = *input;

            if ch != b'@' && !IS_UNRESERVED!(ch as i8) && xml_strchr(list, ch).is_null() {
                let mut val: u8;
                *ret.add(out as usize) = b'%';
                out += 1;
                val = ch >> 4;
                if val <= 9 {
                    *ret.add(out as usize) = b'0' + val;
                    out += 1;
                } else {
                    *ret.add(out as usize) = b'A' + val - 0xA;
                    out += 1;
                }
                val = ch & 0xF;
                if val <= 9 {
                    *ret.add(out as usize) = b'0' + val;
                    out += 1;
                } else {
                    *ret.add(out as usize) = b'A' + val - 0xA;
                    out += 1;
                }
                input = input.add(1);
            } else {
                *ret.add(out as usize) = *input;
                out += 1;
                input = input.add(1);
            }
        }
        *ret.add(out as usize) = 0;
        ret
    }
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
    fn test_xml_build_relative_uri() {
        unsafe {
            let mut leaks = 0;

            for n_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_base in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let uri = gen_const_xml_char_ptr(n_uri, 0);
                    let base = gen_const_xml_char_ptr(n_base, 1);

                    let ret_val = xml_build_relative_uri(uri as *const XmlChar, base);
                    desret_xml_char_ptr(ret_val);
                    des_const_xml_char_ptr(n_uri, uri, 0);
                    des_const_xml_char_ptr(n_base, base, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlBuildRelativeURI",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlBuildRelativeURI()"
                        );
                        eprint!(" {}", n_uri);
                        eprintln!(" {}", n_base);
                    }
                }
            }
        }
    }

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

    #[test]
    fn test_xml_save_uri() {
        unsafe {
            let mut leaks = 0;

            for n_uri in 0..GEN_NB_XML_URIPTR {
                let mem_base = xml_mem_blocks();
                let uri = gen_xml_uriptr(n_uri, 0);

                let ret_val = xml_save_uri(uri);
                desret_xml_char_ptr(ret_val);
                des_xml_uriptr(n_uri, uri, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSaveUri",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveUri()");
                    eprintln!(" {}", n_uri);
                }
            }
        }
    }

    #[test]
    fn test_xml_uriescape_str() {
        unsafe {
            let mut leaks = 0;

            for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_list in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let str = gen_const_xml_char_ptr(n_str, 0);
                    let list = gen_const_xml_char_ptr(n_list, 1);

                    let ret_val = xml_uri_escape_str(str as *const XmlChar, list);
                    desret_xml_char_ptr(ret_val);
                    des_const_xml_char_ptr(n_str, str, 0);
                    des_const_xml_char_ptr(n_list, list, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlURIEscapeStr",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlURIEscapeStr()");
                        eprint!(" {}", n_str);
                        eprintln!(" {}", n_list);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_uriunescape_string() {

        /* missing type support */
    }
}

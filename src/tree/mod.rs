//! Provide methods and data structures for tree manipulation.  
//! This module is based on `libxml/tree.h`, `tree.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: interfaces for tree manipulation
// Description: this module describes the structures found in an tree resulting
//              from an XML or HTML parsing, as well as the API provided for
//              various processing on that tree
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// tree.c : implementation of access function for an XML tree.
//
// References:
//   XHTML 1.0 W3C REC: http://www.w3.org/TR/2002/REC-xhtml1-20020801/
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

mod attribute;
mod document;
mod dom_wrapper;
mod dtd;
#[cfg(feature = "libxml_output")]
mod dump;
mod entities;
mod generic;
mod id;
mod namespace;
mod node;

use std::{
    any::type_name,
    borrow::Cow,
    ffi::{CStr, CString},
    fmt::Display,
    ptr::null_mut,
    sync::atomic::{AtomicBool, AtomicI32, Ordering},
};

use libc::{memcpy, strlen};

use crate::{
    error::{__xml_simple_error, __xml_simple_oom_error, XmlErrorDomain, XmlParserErrors},
    libxml::{
        chvalid::xml_is_blank_char,
        globals::{
            xml_deregister_node_default_value, xml_free, xml_malloc_atomic,
            xml_register_node_default_value,
        },
        parser_internals::{XML_STRING_COMMENT, XML_STRING_TEXT, XML_STRING_TEXT_NOENC},
        xmlstring::{XmlChar, xml_str_equal, xml_strdup, xml_strncat, xml_strndup},
    },
};

pub use attribute::*;
pub use document::*;
pub use dom_wrapper::*;
pub use dtd::*;
pub use entities::*;
pub use generic::*;
pub use id::*;
pub use namespace::*;
pub use node::*;

/// default buffer size 4000.
pub const BASE_BUFFER_SIZE: usize = 4096;

/// A buffer allocation scheme can be defined to either match exactly the
/// need or double it's allocated size each time it is found too small.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlBufferAllocationScheme {
    XmlBufferAllocDoubleit,  /* double each time one need to grow */
    XmlBufferAllocExact,     /* grow only to the minimal size */
    XmlBufferAllocImmutable, /* immutable buffer, deprecated */
    XmlBufferAllocIo,        /* special allocation scheme used for I/O */
    XmlBufferAllocHybrid,    /* exact up to a threshold, and doubleit thereafter */
    XmlBufferAllocBounded,   /* limit the upper size of the buffer */
}

impl TryFrom<i32> for XmlBufferAllocationScheme {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XmlBufferAllocDoubleit as i32 {
            Ok(Self::XmlBufferAllocDoubleit)
        } else if value == Self::XmlBufferAllocExact as i32 {
            Ok(Self::XmlBufferAllocExact)
        } else if value == Self::XmlBufferAllocImmutable as i32 {
            Ok(Self::XmlBufferAllocImmutable)
        } else if value == Self::XmlBufferAllocIo as i32 {
            Ok(Self::XmlBufferAllocIo)
        } else if value == Self::XmlBufferAllocHybrid as i32 {
            Ok(Self::XmlBufferAllocHybrid)
        } else if value == Self::XmlBufferAllocBounded as i32 {
            Ok(Self::XmlBufferAllocBounded)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

pub(crate) static __XML_REGISTER_CALLBACKS: AtomicI32 = AtomicI32::new(0);

/// This is the namespace for the special xml: prefix predefined in the
/// XML Namespace specification.
pub const XML_XML_NAMESPACE: &CStr = c"http://www.w3.org/XML/1998/namespace";

/// This is the name for the special xml:id attribute
pub const XML_XML_ID: *const XmlChar = c"xml:id".as_ptr() as _;

/// The different element types carried by an XML tree.
///
/// # NOTE
/// This is synchronized with DOM Level1 values.  
/// See <http://www.w3.org/TR/REC-DOM-Level-1/>
///
/// Actually this had diverged a bit, and now XML_DOCUMENT_TYPE_NODE should
/// be deprecated to use an XML_DTD_NODE.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlElementType {
    #[default]
    XmlInvalidNode = 0, // for std::mem::zeroed(). This is invalid value.
    XmlElementNode = 1,
    XmlAttributeNode = 2,
    XmlTextNode = 3,
    XmlCDATASectionNode = 4,
    XmlEntityRefNode = 5,
    XmlEntityNode = 6,
    XmlPINode = 7,
    XmlCommentNode = 8,
    XmlDocumentNode = 9,
    XmlDocumentTypeNode = 10,
    XmlDocumentFragNode = 11,
    XmlNotationNode = 12,
    XmlHTMLDocumentNode = 13,
    XmlDTDNode = 14,
    XmlElementDecl = 15,
    XmlAttributeDecl = 16,
    XmlEntityDecl = 17,
    XmlNamespaceDecl = 18,
    XmlXIncludeStart = 19,
    XmlXIncludeEnd = 20,
}

impl TryFrom<i32> for XmlElementType {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XmlElementNode as i32 {
            Ok(Self::XmlElementNode)
        } else if value == Self::XmlAttributeNode as i32 {
            Ok(Self::XmlAttributeNode)
        } else if value == Self::XmlTextNode as i32 {
            Ok(Self::XmlTextNode)
        } else if value == Self::XmlCDATASectionNode as i32 {
            Ok(Self::XmlCDATASectionNode)
        } else if value == Self::XmlEntityRefNode as i32 {
            Ok(Self::XmlEntityRefNode)
        } else if value == Self::XmlEntityNode as i32 {
            Ok(Self::XmlEntityNode)
        } else if value == Self::XmlPINode as i32 {
            Ok(Self::XmlPINode)
        } else if value == Self::XmlCommentNode as i32 {
            Ok(Self::XmlCommentNode)
        } else if value == Self::XmlDocumentNode as i32 {
            Ok(Self::XmlDocumentNode)
        } else if value == Self::XmlDocumentTypeNode as i32 {
            Ok(Self::XmlDocumentTypeNode)
        } else if value == Self::XmlDocumentFragNode as i32 {
            Ok(Self::XmlDocumentFragNode)
        } else if value == Self::XmlNotationNode as i32 {
            Ok(Self::XmlNotationNode)
        } else if value == Self::XmlHTMLDocumentNode as i32 {
            Ok(Self::XmlHTMLDocumentNode)
        } else if value == Self::XmlDTDNode as i32 {
            Ok(Self::XmlDTDNode)
        } else if value == Self::XmlElementDecl as i32 {
            Ok(Self::XmlElementDecl)
        } else if value == Self::XmlAttributeDecl as i32 {
            Ok(Self::XmlAttributeDecl)
        } else if value == Self::XmlEntityDecl as i32 {
            Ok(Self::XmlEntityDecl)
        } else if value == Self::XmlNamespaceDecl as i32 {
            Ok(Self::XmlNamespaceDecl)
        } else if value == Self::XmlXIncludeStart as i32 {
            Ok(Self::XmlXIncludeStart)
        } else if value == Self::XmlXIncludeEnd as i32 {
            Ok(Self::XmlXIncludeEnd)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

/// A DTD Attribute type definition.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlAttributeType {
    XmlAttributeCDATA = 1,
    XmlAttributeID,
    XmlAttributeIDREF,
    XmlAttributeIDREFS,
    XmlAttributeEntity,
    XmlAttributeEntities,
    XmlAttributeNmtoken,
    XmlAttributeNmtokens,
    XmlAttributeEnumeration,
    XmlAttributeNotation,
}

impl TryFrom<i32> for XmlAttributeType {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XmlAttributeCDATA as i32 {
            Ok(Self::XmlAttributeCDATA)
        } else if value == Self::XmlAttributeID as i32 {
            Ok(Self::XmlAttributeID)
        } else if value == Self::XmlAttributeIDREF as i32 {
            Ok(Self::XmlAttributeIDREF)
        } else if value == Self::XmlAttributeIDREFS as i32 {
            Ok(Self::XmlAttributeIDREFS)
        } else if value == Self::XmlAttributeEntity as i32 {
            Ok(Self::XmlAttributeEntity)
        } else if value == Self::XmlAttributeEntities as i32 {
            Ok(Self::XmlAttributeEntities)
        } else if value == Self::XmlAttributeNmtoken as i32 {
            Ok(Self::XmlAttributeNmtoken)
        } else if value == Self::XmlAttributeNmtokens as i32 {
            Ok(Self::XmlAttributeNmtokens)
        } else if value == Self::XmlAttributeEnumeration as i32 {
            Ok(Self::XmlAttributeEnumeration)
        } else if value == Self::XmlAttributeNotation as i32 {
            Ok(Self::XmlAttributeNotation)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

/// A DTD Attribute default definition.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlAttributeDefault {
    XmlAttributeNone = 1,
    XmlAttributeRequired,
    XmlAttributeImplied,
    XmlAttributeFixed,
}

impl TryFrom<i32> for XmlAttributeDefault {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XmlAttributeNone as i32 {
            Ok(Self::XmlAttributeNone)
        } else if value == Self::XmlAttributeRequired as i32 {
            Ok(Self::XmlAttributeRequired)
        } else if value == Self::XmlAttributeImplied as i32 {
            Ok(Self::XmlAttributeImplied)
        } else if value == Self::XmlAttributeFixed as i32 {
            Ok(Self::XmlAttributeFixed)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

/// Possible definitions of element content types.
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum XmlElementContentType {
    XmlElementContentPCDATA = 1,
    XmlElementContentElement,
    XmlElementContentSeq,
    XmlElementContentOr,
}

/// Possible definitions of element content occurrences.
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum XmlElementContentOccur {
    XmlElementContentOnce = 1,
    XmlElementContentOpt,
    XmlElementContentMult,
    XmlElementContentPlus,
}

/// An XML Element content as stored after parsing an element definition in a DTD.
pub type XmlElementContentPtr = *mut XmlElementContent;
#[repr(C)]
pub struct XmlElementContent {
    pub(crate) typ: XmlElementContentType, /* PCDATA, ELEMENT, SEQ or OR */
    pub(crate) ocur: XmlElementContentOccur, /* ONCE, OPT, MULT or PLUS */
    pub(crate) name: *const XmlChar,       /* Element name */
    pub(crate) c1: *mut XmlElementContent, /* first child */
    pub(crate) c2: *mut XmlElementContent, /* second child */
    pub(crate) parent: *mut XmlElementContent, /* parent */
    pub(crate) prefix: *const XmlChar,     /* Namespace prefix */
}

/// The different possibilities for an element content type.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlElementTypeVal {
    XmlElementTypeUndefined = 0,
    XmlElementTypeEmpty = 1,
    XmlElementTypeAny,
    XmlElementTypeMixed,
    XmlElementTypeElement,
}

impl TryFrom<i32> for XmlElementTypeVal {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XmlElementTypeUndefined as i32 {
            Ok(Self::XmlElementTypeUndefined)
        } else if value == Self::XmlElementTypeEmpty as i32 {
            Ok(Self::XmlElementTypeEmpty)
        } else if value == Self::XmlElementTypeAny as i32 {
            Ok(Self::XmlElementTypeAny)
        } else if value == Self::XmlElementTypeMixed as i32 {
            Ok(Self::XmlElementTypeMixed)
        } else if value == Self::XmlElementTypeElement as i32 {
            Ok(Self::XmlElementTypeElement)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

/// A namespace declaration node.
pub(crate) const XML_LOCAL_NAMESPACE: XmlElementType = XmlElementType::XmlNamespaceDecl;
pub type XmlNsType = XmlElementType;

/// Set of properties of the document as found by the parser.
///
/// Some of them are linked to similarly named `xmlParserOption`.
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlDocProperties {
    XmlDocWellformed = 1 << 0, /* document is XML well formed */
    XmlDocNsvalid = 1 << 1,    /* document is Namespace valid */
    XmlDocOld10 = 1 << 2,      /* parsed with old XML-1.0 parser */
    XmlDocDTDValid = 1 << 3,   /* DTD validation was successful */
    XmlDocXInclude = 1 << 4,   /* XInclude substitution was done */
    XmlDocUserbuilt = 1 << 5,  /* Document was built using the API
                               and not by parsing an instance */
    XmlDocInternal = 1 << 6, /* built for internal processing */
    XmlDocHTML = 1 << 7,     /* parsed or built HTML document */
}

#[derive(Debug, Clone)]
pub struct InvalidNodePointerCastError {
    from: XmlElementType,
    to: &'static str,
}

impl Display for InvalidNodePointerCastError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Invalid casting from {:?} to {}", self.from, self.to)
    }
}

macro_rules! CUR_SCHAR {
    ($s:expr, $l:expr) => {
        $crate::libxml::parser_internals::xml_string_current_char(
            std::ptr::null_mut(),
            $s,
            std::ptr::addr_of_mut!($l),
        )
    };
}

/// Check that a value conforms to the lexical space of NCName
///
/// Returns `Ok` if this validates, `Err` otherwise.
#[doc(alias = "xmlValidateNCName")]
#[cfg(any(
    feature = "libxml_tree",
    feature = "xpath",
    feature = "schema",
    feature = "html",
    feature = "sax1",
    feature = "libxml_writer",
))]
pub fn validate_ncname<const ALLOW_SPACE: bool>(value: &str) -> Result<(), &'static str> {
    use crate::libxml::{
        chvalid::{xml_is_blank_char, xml_is_combining, xml_is_digit, xml_is_extender},
        parser_internals::xml_is_letter,
    };

    let mut cur = value;

    // First quick algorithm for ASCII range
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c| xml_is_blank_char(c as u32));
    }
    if cur.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
        cur = cur[1..].trim_start_matches(|c: char| {
            c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.'
        });
        if ALLOW_SPACE {
            cur = cur.trim_start_matches(|c| xml_is_blank_char(c as u32));
        }

        if cur.is_empty() {
            return Ok(());
        }
    }

    // try_complex:
    // Second check for chars outside the ASCII range
    let mut cur = value;
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| xml_is_blank_char(c as u32));
    }
    let Some(mut cur) = cur.strip_prefix(|c: char| xml_is_letter(c as u32) || c == '_') else {
        return Err("Invalid NCName");
    };
    cur = cur.trim_start_matches(|c: char| {
        xml_is_letter(c as u32)
            || xml_is_digit(c as u32)
            || c == '.'
            || c == '-'
            || c == '_'
            || xml_is_combining(c as u32)
            || xml_is_extender(c as u32)
    });
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| xml_is_blank_char(c as u32));
    }

    if cur.is_empty() {
        Ok(())
    } else {
        Err("Invalid NCName")
    }
}

/// Check that a value conforms to the lexical space of QName
///
/// Returns `Ok` if this validates, `Err` otherwise.
#[doc(alias = "xmlValidateQName")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub fn validate_qname<const ALLOW_SPACE: bool>(value: &str) -> Result<(), &'static str> {
    use crate::libxml::{
        chvalid::{xml_is_blank_char, xml_is_combining, xml_is_digit, xml_is_extender},
        parser_internals::xml_is_letter,
    };

    let mut cur = value;

    // First quick algorithm for ASCII range
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| xml_is_blank_char(c as u32));
    }

    if cur.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
        'try_complex: {
            cur = cur[1..].trim_start_matches(|c: char| {
                c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.'
            });

            if let Some(local) = cur.strip_prefix(':') {
                let Some(rem) = local.strip_prefix(|c: char| c.is_ascii_alphabetic() || c == '_')
                else {
                    break 'try_complex;
                };
                cur = rem.trim_start_matches(|c: char| {
                    c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.'
                });
            }
            if ALLOW_SPACE {
                cur = cur.trim_start_matches(|c: char| xml_is_blank_char(c as u32));
            }
            if cur.is_empty() {
                return Ok(());
            }
        }
    }

    // try_complex:
    // Second check for chars outside the ASCII range
    let mut cur = value;
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| xml_is_blank_char(c as u32));
    }
    let Some(mut cur) = cur.strip_prefix(|c: char| xml_is_letter(c as u32) || c == '_') else {
        return Err("Invalid QName");
    };
    cur = cur.trim_start_matches(|c: char| {
        xml_is_letter(c as u32)
            || xml_is_digit(c as u32)
            || c == '.'
            || c == '-'
            || c == '_'
            || xml_is_combining(c as u32)
            || xml_is_extender(c as u32)
    });
    if let Some(local) = cur.strip_prefix(':') {
        let Some(rem) = local.strip_prefix(|c: char| xml_is_letter(c as u32) || c == '_') else {
            return Err("Invalid QName");
        };
        cur = rem.trim_start_matches(|c: char| {
            xml_is_letter(c as u32)
                || xml_is_digit(c as u32)
                || c == '.'
                || c == '-'
                || c == '_'
                || xml_is_combining(c as u32)
                || xml_is_extender(c as u32)
        });
    }
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| xml_is_blank_char(c as u32));
    }
    if cur.is_empty() {
        Ok(())
    } else {
        Err("Invalid QName")
    }
}

/// Check that a value conforms to the lexical space of Name
///
/// Returns `Ok` if this validates, `Err` otherwise.
#[doc(alias = "xmlValidateName")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub fn validate_name<const ALLOW_SPACE: bool>(value: &str) -> Result<(), &'static str> {
    use crate::libxml::{
        chvalid::{xml_is_blank_char, xml_is_combining, xml_is_digit, xml_is_extender},
        parser_internals::xml_is_letter,
    };

    let mut cur = value;

    // First quick algorithm for ASCII range
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| xml_is_blank_char(c as u32));
    }
    if cur.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_' || c == ':') {
        cur = cur[1..].trim_start_matches(|c: char| {
            c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == '.' || c == ':'
        });
        if ALLOW_SPACE {
            cur = cur.trim_start_matches(|c: char| xml_is_blank_char(c as u32));
        }
        if cur.is_empty() {
            return Ok(());
        }
    }

    // try_complex:
    // Second check for chars outside the ASCII range
    let mut cur = value;
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| xml_is_blank_char(c as u32));
    }
    let Some(mut cur) = cur.strip_prefix(|c: char| xml_is_letter(c as u32) || c == '_' || c == ':')
    else {
        return Err("Invalid Name");
    };
    cur = cur.trim_start_matches(|c: char| {
        xml_is_letter(c as u32)
            || xml_is_digit(c as u32)
            || c == '.'
            || c == ':'
            || c == '-'
            || c == '_'
            || xml_is_combining(c as u32)
            || xml_is_extender(c as u32)
    });
    if ALLOW_SPACE {
        cur = cur.trim_start_matches(|c: char| xml_is_blank_char(c as u32));
    }
    if cur.is_empty() {
        Ok(())
    } else {
        Err("Invalid Name")
    }
}

/// Check that a value conforms to the lexical space of NMToken
///
/// Returns 0 if this validates, a positive error code number otherwise
/// and -1 in case of internal or API error.
#[doc(alias = "xmlValidateNMToken")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub unsafe fn xml_validate_nmtoken(value: *const XmlChar, space: i32) -> i32 {
    unsafe {
        use crate::libxml::{
            chvalid::{xml_is_blank_char, xml_is_combining, xml_is_digit, xml_is_extender},
            parser_internals::xml_is_letter,
        };

        let mut cur: *const XmlChar = value;
        let mut c: i32;
        let mut l: i32 = 0;

        if value.is_null() {
            return -1;
        }
        // First quick algorithm for ASCII range
        if space != 0 {
            while xml_is_blank_char(*cur as u32) {
                cur = cur.add(1);
            }
        }
        if (*cur >= b'a' && *cur <= b'z')
            || (*cur >= b'A' && *cur <= b'Z')
            || (*cur >= b'0' && *cur <= b'9')
            || *cur == b'_'
            || *cur == b'-'
            || *cur == b'.'
            || *cur == b':'
        {
            cur = cur.add(1);
            while (*cur >= b'a' && *cur <= b'z')
                || (*cur >= b'A' && *cur <= b'Z')
                || (*cur >= b'0' && *cur <= b'9')
                || *cur == b'_'
                || *cur == b'-'
                || *cur == b'.'
                || *cur == b':'
            {
                cur = cur.add(1);
            }
            if space != 0 {
                while xml_is_blank_char(*cur as u32) {
                    cur = cur.add(1);
                }
            }
            if *cur == 0 {
                return 0;
            }
        } else {
            // goto try_complex;
        }

        // try_complex:
        // Second check for chars outside the ASCII range
        cur = value;
        c = CUR_SCHAR!(cur, l);
        if space != 0 {
            while xml_is_blank_char(c as u32) {
                cur = cur.add(l as usize);
                c = CUR_SCHAR!(cur, l);
            }
        }
        if !(xml_is_letter(c as u32)
            || xml_is_digit(c as u32)
            || c == b'.' as i32
            || c == b':' as i32
            || c == b'-' as i32
            || c == b'_' as i32
            || xml_is_combining(c as u32)
            || xml_is_extender(c as u32))
        {
            return 1;
        }
        cur = cur.add(l as usize);
        c = CUR_SCHAR!(cur, l);
        while xml_is_letter(c as u32)
            || xml_is_digit(c as u32)
            || c == b'.' as i32
            || c == b':' as i32
            || c == b'-' as i32
            || c == b'_' as i32
            || xml_is_combining(c as u32)
            || xml_is_extender(c as u32)
        {
            cur = cur.add(l as usize);
            c = CUR_SCHAR!(cur, l);
        }
        if space != 0 {
            while xml_is_blank_char(c as u32) {
                cur = cur.add(l as usize);
                c = CUR_SCHAR!(cur, l);
            }
        }
        if c != 0 {
            return 1;
        }
        0
    }
}

/// Handle an out of memory condition
#[doc(alias = "xmlTreeErrMemory")]
unsafe fn xml_tree_err_memory(extra: &str) {
    unsafe {
        __xml_simple_oom_error(XmlErrorDomain::XmlFromTree, None, Some(extra));
    }
}

/// Builds the QName @prefix:@ncname in @memory if there is enough space
/// and prefix is not NULL nor empty, otherwise allocate a new string.
/// If prefix is NULL or empty it returns ncname.
///
/// Returns the new string which must be freed by the caller if different from
/// @memory and @ncname or NULL in case of error
#[doc(alias = "xmlBuildQName")]
pub unsafe fn xml_build_qname(
    ncname: *const XmlChar,
    prefix: *const XmlChar,
    memory: *mut XmlChar,
    len: i32,
) -> *mut XmlChar {
    unsafe {
        let ret: *mut XmlChar;

        if ncname.is_null() {
            return null_mut();
        }
        if prefix.is_null() {
            return ncname as _;
        }

        let lenn: i32 = strlen(ncname as _) as _;
        let lenp: i32 = strlen(prefix as _) as _;

        if memory.is_null() || len < lenn + lenp + 2 {
            ret = xml_malloc_atomic(lenn as usize + lenp as usize + 2) as _;
            if ret.is_null() {
                xml_tree_err_memory("building QName");
                return null_mut();
            }
        } else {
            ret = memory;
        }
        memcpy(ret.add(0) as _, prefix as _, lenp as usize);
        *ret.add(lenp as usize) = b':';
        memcpy(ret.add(lenp as usize + 1) as _, ncname as _, lenn as usize);
        *ret.add(lenn as usize + lenp as usize + 1) = 0;
        ret as _
    }
}

/// Parse an XML qualified name string
///
/// ```text
/// [NS 5] QName ::= (Prefix ':')? LocalPart
///
/// [NS 6] Prefix ::= NCName
///
/// [NS 7] LocalPart ::= NCName
/// ```
///
/// Returns NULL if the name doesn't have a prefix. Otherwise, returns the
/// local part, and prefix is updated to get the Prefix. Both the return value
/// and the prefix must be freed by the caller.
#[doc(alias = "xmlSplitQName2")]
pub unsafe fn xml_split_qname2(name: *const XmlChar, prefix: *mut *mut XmlChar) -> *mut XmlChar {
    unsafe {
        let mut len: i32 = 0;

        if prefix.is_null() {
            return null_mut();
        }
        *prefix = null_mut();
        if name.is_null() {
            return null_mut();
        }

        // nasty but valid
        if *name.add(0) == b':' {
            return null_mut();
        }

        // we are not trying to validate but just to cut, and yes it will
        // work even if this is as set of UTF-8 encoded chars
        while *name.add(len as usize) != 0 && *name.add(len as usize) != b':' {
            len += 1;
        }

        if *name.add(len as usize) == 0 {
            return null_mut();
        }

        *prefix = xml_strndup(name, len) as _;
        if (*prefix).is_null() {
            xml_tree_err_memory("QName split");
            return null_mut();
        }
        let ret: *mut XmlChar = xml_strdup(name.add(len as usize + 1) as _) as _;
        if ret.is_null() {
            xml_tree_err_memory("QName split");
            if !(*prefix).is_null() {
                xml_free(*prefix as _);
                *prefix = null_mut();
            }
            return null_mut();
        }

        ret
    }
}

/// Rarse an XML qualified name string,i
///
/// Returns NULL if it is not a Qualified Name, otherwise, update len
/// with the length in byte of the prefix and return a pointer
/// to the start of the name without the prefix
#[doc(alias = "xmlSplitQName3")]
pub unsafe fn xml_split_qname3(name: *const XmlChar, len: *mut i32) -> *const XmlChar {
    unsafe {
        let mut l: i32 = 0;

        if name.is_null() {
            return null_mut();
        }
        if len.is_null() {
            return null_mut();
        }

        // nasty but valid
        if *name.add(0) == b':' {
            return null_mut();
        }

        // we are not trying to validate but just to cut, and yes it will
        // work even if this is as set of UTF-8 encoded chars
        while *name.add(l as usize) != 0 && *name.add(l as usize) != b':' {
            l += 1;
        }

        if *name.add(l as usize) == 0 {
            return null_mut();
        }

        *len = l;

        name.add(l as usize + 1)
    }
}

/// This function tries to locate a namespace definition in a tree
/// ancestors, or create a new namespace definition node similar to
/// @ns trying to reuse the same prefix. However if the given prefix is
/// null (default namespace) or reused within the subtree defined by
/// @tree or on one of its ancestors then a new prefix is generated.
/// Returns the (new) namespace definition or null_mut() in case of error
#[doc(alias = "xmlNewReconciledNs")]
unsafe fn xml_new_reconciled_ns(
    doc: Option<XmlDocPtr>,
    mut tree: XmlNodePtr,
    ns: XmlNsPtr,
) -> Option<XmlNsPtr> {
    unsafe {
        let mut counter: i32 = 1;

        // let tree = tree?;
        if !matches!(tree.element_type(), XmlElementType::XmlElementNode) {
            return None;
        }
        if !matches!(ns.typ, XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        // Search an existing namespace definition inherited.
        if let Some(def) = tree.search_ns_by_href(doc, ns.href().as_deref().unwrap()) {
            return Some(def);
        }

        // Find a close prefix which is not already in use.
        // Let's strip namespace prefixes longer than 20 chars !
        let prefix = ns.prefix().unwrap_or(Cow::Borrowed("default"));
        let mut def = tree.search_ns(doc, Some(&prefix));
        while def.is_some() {
            if counter > 1000 {
                return None;
            }
            let prefix = format!("{prefix}{counter}");
            counter += 1;
            def = tree.search_ns(doc, Some(&prefix));
        }

        // OK, now we are ready to create a new one.
        xml_new_ns(Some(tree), ns.href, Some(&prefix))
    }
}

// NOTE about the CopyNode operations !
//
// They are split into external and internal parts for one
// tricky reason: namespaces. Doing a direct copy of a node
// say RPM:Copyright without changing the namespace pointer to
// something else can produce stale links. One way to do it is
// to keep a reference counter but this doesn't work as soon
// as one moves the element or the subtree out of the scope of
// the existing namespace. The actual solution seems to be to add
// a copy of the namespace at the top of the copied tree if
// not available in the subtree.
// Hence two functions, the public front-end call the inner ones
// The argument "recursive" normally indicates a recursive copy
// of the node with values 0 (no) and 1 (yes).  For XInclude,
// however, we allow a value of 2 to indicate copy properties and
// namespace info, but don't recurse on children.
pub(crate) unsafe fn xml_static_copy_node(
    node: XmlGenericNodePtr,
    doc: Option<XmlDocPtr>,
    parent: Option<XmlGenericNodePtr>,
    extended: i32,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        // if node.is_null() {
        //     return null_mut();
        // }
        match node.element_type() {
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlElementNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {}
            XmlElementType::XmlAttributeNode => {
                let attr = XmlAttrPtr::try_from(node).unwrap();
                return xml_copy_prop_internal(
                    doc,
                    parent.and_then(|p| XmlNodePtr::try_from(p).ok()),
                    attr,
                )
                .map(|attr| attr.into());
            }
            XmlElementType::XmlNamespaceDecl => {
                let ns = XmlNsPtr::try_from(node).unwrap();
                return xml_copy_namespace_list(Some(ns)).map(|ns| ns.into());
            }

            #[cfg(feature = "libxml_tree")]
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                let doc = XmlDocPtr::try_from(node).unwrap();
                return xml_copy_doc(doc, extended).map(|doc| doc.into());
            }
            #[cfg(not(feature = "libxml_tree"))]
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                return None;
            }
            XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl => {
                return None;
            }
            _ => unreachable!(),
        }

        let mut node = XmlNodePtr::try_from(node).unwrap();

        // Allocate a new node and fill the fields.
        let Some(mut ret) = XmlNodePtr::new(XmlNode {
            typ: node.element_type(),
            doc,
            parent,
            ..Default::default()
        }) else {
            xml_tree_err_memory("copying node");
            return None;
        };
        if node.name == XML_STRING_TEXT.as_ptr() as _ {
            ret.name = XML_STRING_TEXT.as_ptr() as _;
        } else if node.name == XML_STRING_TEXT_NOENC.as_ptr() as _ {
            ret.name = XML_STRING_TEXT_NOENC.as_ptr() as _;
        } else if node.name == XML_STRING_COMMENT.as_ptr() as _ {
            ret.name = XML_STRING_COMMENT.as_ptr() as _;
        } else if !node.name.is_null() {
            ret.name = xml_strdup(node.name);
        }
        if !matches!(node.element_type(), XmlElementType::XmlElementNode)
            && !node.content.is_null()
            && !matches!(node.element_type(), XmlElementType::XmlEntityRefNode)
            && !matches!(node.element_type(), XmlElementType::XmlXIncludeEnd)
            && !matches!(node.element_type(), XmlElementType::XmlXIncludeStart)
        {
            ret.content = xml_strdup(node.content);
        } else if matches!(node.element_type(), XmlElementType::XmlElementNode) {
            ret.line = node.line;
        }
        if let Some(mut parent) = parent {
            // this is a tricky part for the node register thing:
            // in case ret does get coalesced in xmlAddChild
            // the deregister-node callback is called; so we register ret now already
            if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
            // && xmlRegisterNodeDefaultValue.is_some()
            {
                xml_register_node_default_value(ret.into());
            }

            // Note that since (*ret).parent is already set, xmlAddChild will
            // return early and not actually insert the node. It will only
            // coalesce text nodes and unnecessarily call xmlSetTreeDoc.
            // Assuming that the subtree to be copied always has its text
            // nodes coalesced, the somewhat confusing call to xmlAddChild
            // could be removed.
            let tmp = parent.add_child(ret.into());
            /* node could have coalesced */
            if tmp != Some(ret.into()) {
                return tmp;
            }
        }

        if extended == 0 {
            //  goto out;
            // if parent != null_mut() we already registered the node above
            if parent.is_none() && __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
            // && xmlRegisterNodeDefaultValue.is_some()
            {
                xml_register_node_default_value(ret.into());
            }

            return Some(ret.into());
        }
        if matches!(
            node.element_type(),
            XmlElementType::XmlElementNode | XmlElementType::XmlXIncludeStart
        ) && node.ns_def.is_some()
        {
            ret.ns_def = xml_copy_namespace_list(node.ns_def);
        }

        if let Some(node_ns) = node.ns {
            let prefix = node_ns.prefix();
            if let Some(ns) = ret.search_ns(doc, prefix.as_deref()) {
                // reference the existing namespace definition in our own tree.
                ret.ns = Some(ns);
            } else {
                // Humm, we are copying an element whose namespace is defined
                // out of the new tree scope. Search it in the original tree
                // and add it at the top of the new tree
                let node_doc = node.doc;
                if let Some(ns) = node.search_ns(node_doc, prefix.as_deref()) {
                    let mut root = XmlGenericNodePtr::from(ret);

                    while let Some(parent) = root.parent() {
                        root = parent;
                    }
                    ret.ns = xml_new_ns(
                        Some(XmlNodePtr::try_from(root).unwrap()),
                        ns.href,
                        ns.prefix().as_deref(),
                    );
                } else {
                    ret.ns = xml_new_reconciled_ns(doc, ret, node_ns);
                }
            }
        }
        if (matches!(node.element_type(), XmlElementType::XmlElementNode)
            || matches!(node.element_type(), XmlElementType::XmlXIncludeStart))
            && node.properties.is_some()
        {
            ret.properties = xml_copy_prop_list(Some(ret), node.properties);
        }
        if matches!(node.element_type(), XmlElementType::XmlEntityRefNode) {
            if doc.is_none_or(|doc| node.doc != Some(doc)) {
                // The copied node will go into a separate document, so
                // to avoid dangling references to the ENTITY_DECL node
                // we cannot keep the reference. Try to find it in the
                // target document.
                let children = xml_get_doc_entity(doc, &ret.name().unwrap());
                ret.set_children(children.map(|children| children.into()));
            } else {
                ret.set_children(node.children());
            }
            let children = ret.children();
            ret.set_last(children);
        } else if let Some(children) = node.children().filter(|_| extended != 2) {
            let mut cur = Some(children);
            let mut insert = XmlGenericNodePtr::from(ret);
            while let Some(mut now) = cur {
                let Some(mut copy) = xml_static_copy_node(now, doc, Some(insert), 2) else {
                    xml_free_node(ret);
                    return None;
                };

                // Check for coalesced text nodes
                if insert.last() != Some(copy) {
                    if let Some(mut last) = insert.last() {
                        copy.set_prev(Some(last));
                        last.set_next(Some(copy));
                    } else {
                        insert.set_children(Some(copy));
                    }
                    insert.set_last(Some(copy));
                }

                if let Some(children) = now
                    .children()
                    .filter(|_| !matches!(now.element_type(), XmlElementType::XmlEntityRefNode))
                {
                    cur = Some(children);
                    insert = copy;
                    continue;
                }

                cur = loop {
                    if let Some(next) = now.next() {
                        break Some(next);
                    }

                    now = now.parent().unwrap();
                    if now == node.into() {
                        break None;
                    }
                    insert = insert.parent().unwrap();
                };
            }
        }

        //  out:
        /* if parent != null_mut() we already registered the node above */
        if parent.is_none() && __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        // && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(ret.into());
        }

        Some(ret.into())
    }
}

pub(crate) unsafe fn xml_static_copy_node_list(
    mut node: Option<XmlGenericNodePtr>,
    doc: Option<XmlDocPtr>,
    parent: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        let mut ret = None;
        let mut p = None;

        while let Some(cur_node) = node {
            #[cfg(feature = "libxml_tree")]
            let q = if matches!(cur_node.element_type(), XmlElementType::XmlDTDNode) {
                let Some(mut doc) = doc else {
                    node = cur_node.next();
                    continue;
                };
                if let Some(int_subset) = doc.int_subset {
                    let q = int_subset;
                    parent.unwrap().add_child(q.into());
                    Some(q.into())
                } else {
                    let Some(mut new) = xml_copy_dtd(XmlDtdPtr::try_from(cur_node).unwrap()) else {
                        // goto error;
                        xml_free_node_list(ret);
                        return None;
                    };
                    new.doc = Some(doc);
                    new.set_parent(parent);
                    doc.int_subset = Some(new);
                    parent.unwrap().add_child(new.into());
                    Some(new.into())
                }
            } else {
                xml_static_copy_node(cur_node, doc, parent, 1)
            };
            #[cfg(not(feature = "libxml_tree"))]
            let q = xml_static_copy_node(node, doc, parent, 1);
            let Some(mut q) = q else {
                // goto error;
                xml_free_node_list(ret);
                return None;
            };
            if ret.is_none() {
                q.set_prev(None);
                ret = Some(q);
                p = Some(q);
            } else if let Some(mut pr) = p.filter(|&p| p != q) {
                // the test is required if xmlStaticCopyNode coalesced 2 text nodes
                pr.set_next(Some(q));
                q.set_prev(Some(pr));
                p = Some(q);
            }
            node = cur_node.next();
        }
        ret
        // error:
        //     xmlFreeNodeList(ret);
        //     return null_mut();
    }
}

/// Do a copy of the dtd.
///
/// Returns: a new #xmlDtdPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyDtd")]
#[cfg(feature = "libxml_tree")]
pub unsafe fn xml_copy_dtd(dtd: XmlDtdPtr) -> Option<XmlDtdPtr> {
    unsafe {
        use crate::libxml::valid::{
            xml_copy_attribute_table, xml_copy_notation_table, xml_get_dtd_qelement_desc,
        };

        let mut ret = xml_new_dtd(
            None,
            dtd.name().as_deref(),
            dtd.external_id.as_deref(),
            dtd.system_id.as_deref(),
        )?;
        if let Some(entities) = dtd.entities {
            ret.entities = xml_copy_entities_table(entities);
        }
        if let Some(table) = dtd.notations.as_deref() {
            let new = xml_copy_notation_table(table);
            ret.notations = Some(Box::new(new));
        }
        if let Some(table) = dtd.elements.as_ref() {
            ret.elements =
                Some(table.clone_with(|data, _| {
                    xml_copy_element(*data).expect("Failed to copy element")
                }));
        }
        if let Some(table) = dtd.attributes {
            ret.attributes = xml_copy_attribute_table(table);
        }
        if let Some(pentities) = dtd.pentities {
            ret.pentities = xml_copy_entities_table(pentities);
        }

        let mut cur = dtd.children;
        let mut p: Option<XmlGenericNodePtr> = None;
        while let Some(cur_node) = cur {
            let mut q = None;

            if let Ok(tmp) = XmlEntityPtr::try_from(cur_node) {
                match tmp.etype {
                    XmlEntityType::XmlInternalGeneralEntity
                    | XmlEntityType::XmlExternalGeneralParsedEntity
                    | XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                        q = ret
                            .get_entity(tmp.name().as_deref().unwrap())
                            .map(XmlGenericNodePtr::from);
                    }
                    XmlEntityType::XmlInternalParameterEntity
                    | XmlEntityType::XmlExternalParameterEntity => {
                        q = ret
                            .get_parameter_entity(tmp.name().as_deref().unwrap())
                            .map(XmlGenericNodePtr::from);
                    }
                    XmlEntityType::XmlInternalPredefinedEntity => {}
                    _ => unreachable!(),
                }
            } else if let Ok(tmp) = XmlElementPtr::try_from(cur_node) {
                q = xml_get_dtd_qelement_desc(
                    Some(ret),
                    tmp.name().as_deref().unwrap(),
                    tmp.prefix.as_deref(),
                )
                .map(XmlGenericNodePtr::from);
            } else if let Ok(tmp) = XmlAttributePtr::try_from(cur_node) {
                q = ret
                    .get_qattr_desc(
                        tmp.elem.as_deref().unwrap(),
                        tmp.name().as_deref().unwrap(),
                        tmp.prefix.as_deref(),
                    )
                    .map(XmlGenericNodePtr::from);
            } else if matches!(cur_node.element_type(), XmlElementType::XmlCommentNode) {
                q = xml_copy_node(cur_node, 0);
            }

            let Some(mut q) = q else {
                cur = cur_node.next();
                continue;
            };

            if let Some(mut p) = p {
                p.set_next(Some(q));
            } else {
                ret.children = Some(q);
            }

            q.set_prev(p);
            q.set_parent(Some(ret.into()));
            q.set_next(None);
            ret.last = Some(q);
            p = Some(q);
            cur = cur_node.next();
        }

        Some(ret)
    }
}

/// Creation of a new node element within a document. @ns and @content
/// are optional (null_mut()).
/// NOTE: @content is supposed to be a piece of XML CDATA, so it allow entities
///       references, but XML special chars need to be escaped first by using
///       xmlEncodeEntitiesReentrant(). Use xmlNewDocRawNode() if you don't
///       need entities support.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocNode")]
pub unsafe fn xml_new_doc_node(
    doc: Option<XmlDocPtr>,
    ns: Option<XmlNsPtr>,
    name: &str,
    content: *const XmlChar,
) -> Option<XmlNodePtr> {
    unsafe {
        let name = CString::new(name).unwrap();
        let cur = xml_new_node(ns, name.as_ptr() as *const u8);
        if let Some(mut cur) = cur {
            cur.doc = doc;
            if !content.is_null() {
                cur.set_children(
                    doc.and_then(|doc| doc.get_node_list(content).map(|node| node.into())),
                );
                if let Some(mut ulccur) = cur.children() {
                    while let Some(next) = ulccur.next() {
                        ulccur.set_parent(Some(cur.into()));
                        ulccur = next;
                    }
                    (*ulccur).set_parent(Some(cur.into()));
                    cur.set_last(Some(ulccur));
                } else {
                    cur.set_last(None);
                }
            }
        }

        cur
    }
}

/// Creation of a new node element within a document. @ns and @content
/// are optional (null_mut()).
/// NOTE: @content is supposed to be a piece of XML CDATA, so it allow entities
///       references, but XML special chars need to be escaped first by using
///       xmlEncodeEntitiesReentrant(). Use xmlNewDocRawNode() if you don't
///       need entities support.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocNodeEatName")]
pub unsafe fn xml_new_doc_node_eat_name(
    doc: Option<XmlDocPtr>,
    ns: Option<XmlNsPtr>,
    name: *mut XmlChar,
    content: *const XmlChar,
) -> Option<XmlNodePtr> {
    unsafe {
        let cur = xml_new_node_eat_name(ns, name);
        if let Some(mut cur) = cur {
            cur.doc = doc;
            if !content.is_null() {
                cur.set_children(
                    doc.and_then(|doc| doc.get_node_list(content).map(|node| node.into())),
                );
                if let Some(mut ulccur) = cur.children() {
                    while let Some(next) = ulccur.next() {
                        ulccur.set_parent(Some(cur.into()));
                        ulccur = next;
                    }
                    (*ulccur).set_parent(Some(cur.into()));
                    cur.set_last(Some(ulccur));
                } else {
                    cur.set_last(None);
                }
            }
        } else {
            // if name don't come from the doc dictionary free it here
            if !name.is_null() {
                xml_free(name as _);
            }
        }
        cur
    }
}

/// Creation of a new node element. @ns is optional (null_mut()).
///
/// Use of this function is DISCOURAGED in favor of xmlNewDocNode.
///
/// Returns a pointer to the new node object. Uses xml_strdup() to make copy of @name.
#[doc(alias = "xmlNewNode")]
pub unsafe fn xml_new_node(ns: Option<XmlNsPtr>, name: *const XmlChar) -> Option<XmlNodePtr> {
    unsafe {
        if name.is_null() {
            return None;
        }

        // Allocate a new node and fill the fields.
        let Some(cur) = XmlNodePtr::new(XmlNode {
            typ: XmlElementType::XmlElementNode,
            name: xml_strdup(name),
            ns,
            ..Default::default()
        }) else {
            xml_tree_err_memory("building node");
            return None;
        };

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        //  && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

/// Creation of a new node element. @ns is optional (null_mut()).
///
/// Use of this function is DISCOURAGED in favor of xmlNewDocNodeEatName.
///
/// Returns a pointer to the new node object, with pointer @name as
/// new node's name. Use xmlNewNode() if a copy of @name string is
/// is needed as new node's name.
#[doc(alias = "xmlNewNodeEatName")]
pub unsafe fn xml_new_node_eat_name(
    ns: Option<XmlNsPtr>,
    name: *mut XmlChar,
) -> Option<XmlNodePtr> {
    unsafe {
        if name.is_null() {
            return None;
        }

        // Allocate a new node and fill the fields.
        let Some(cur) = XmlNodePtr::new(XmlNode {
            typ: XmlElementType::XmlElementNode,
            name,
            ns,
            ..Default::default()
        }) else {
            xml_tree_err_memory("building node");
            // we can't check here that name comes from the doc dictionary
            return None;
        };

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        //  && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

/// Creation of a new child element, added at the end of @parent children list.
/// @ns and @content parameters are optional (NULL). If @ns is NULL, the newly
/// created element inherits the namespace of @parent. If @content is non NULL,
/// a child list containing the TEXTs and ENTITY_REFs node will be created.
/// NOTE: @content is supposed to be a piece of XML CDATA, so it allows entity
///       references. XML special chars must be escaped first by using
///       xmlEncodeEntitiesReentrant(), or xmlNewTextChild() should be used.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewChild")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub unsafe fn xml_new_child(
    mut parent: XmlGenericNodePtr,
    ns: Option<XmlNsPtr>,
    name: &str,
    content: *const XmlChar,
) -> Option<XmlNodePtr> {
    unsafe {
        // Allocate a new node
        let mut cur = if let Some(node) = XmlNodePtr::try_from(parent)
            .ok()
            .filter(|node| node.element_type() == XmlElementType::XmlElementNode)
        {
            if ns.is_none() {
                xml_new_doc_node(node.doc, node.ns, name, content)
            } else {
                xml_new_doc_node(node.doc, ns, name, content)
            }
        } else if let Ok(parent) = XmlDocPtr::try_from(parent) {
            if ns.is_none() {
                xml_new_doc_node(Some(parent), None, name, content)
            } else {
                xml_new_doc_node(Some(parent), ns, name, content)
            }
        } else if matches!(parent.element_type(), XmlElementType::XmlDocumentFragNode) {
            xml_new_doc_node(parent.document(), ns, name, content)
        } else {
            None
        }?;

        // add the new element at the end of the children list.
        cur.typ = XmlElementType::XmlElementNode;
        cur.set_parent(Some(parent));
        cur.doc = parent.document();
        if parent.children().is_none() {
            parent.set_children(Some(cur.into()));
            parent.set_last(Some(cur.into()));
        } else {
            let mut prev = parent.last().unwrap();
            prev.set_next(Some(cur.into()));
            cur.prev = Some(prev);
            parent.set_last(Some(cur.into()));
        }

        Some(cur)
    }
}

/// Creation of a new text node within a document.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocText")]
pub unsafe fn xml_new_doc_text(
    doc: Option<XmlDocPtr>,
    content: *const XmlChar,
) -> Option<XmlNodePtr> {
    unsafe {
        let cur = xml_new_text(content);
        if let Some(mut cur) = cur {
            cur.doc = doc;
        }
        cur
    }
}

/// Creation of a new text node.
///
/// Use of this function is DISCOURAGED in favor of xmlNewDocText.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewText")]
pub unsafe fn xml_new_text(content: *const XmlChar) -> Option<XmlNodePtr> {
    unsafe {
        // Allocate a new node and fill the fields.
        let Some(mut cur) = XmlNodePtr::new(XmlNode {
            typ: XmlElementType::XmlTextNode,
            name: XML_STRING_TEXT.as_ptr() as _,
            ..Default::default()
        }) else {
            xml_tree_err_memory("building text");
            return None;
        };
        if !content.is_null() {
            cur.content = xml_strdup(content);
        }

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        //  && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

/// Creation of a processing instruction element.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocPI")]
pub unsafe fn xml_new_doc_pi(
    doc: Option<XmlDocPtr>,
    name: &str,
    content: Option<&str>,
) -> Option<XmlNodePtr> {
    unsafe {
        // Allocate a new node and fill the fields.
        let Some(mut cur) = XmlNodePtr::new(XmlNode {
            typ: XmlElementType::XmlPINode,
            name: xml_strndup(name.as_ptr(), name.len() as i32),
            ..Default::default()
        }) else {
            xml_tree_err_memory("building PI");
            return None;
        };
        if let Some(content) = content {
            let content = CString::new(content).unwrap();
            cur.content = xml_strdup(content.as_ptr() as *const u8);
        }
        cur.doc = doc;

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        //  && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

/// Creation of a processing instruction element.
///
/// Use of this function is DISCOURAGED in favor of xmlNewDocPI.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewPI")]
pub unsafe fn xml_new_pi(name: &str, content: Option<&str>) -> Option<XmlNodePtr> {
    unsafe { xml_new_doc_pi(None, name, content) }
}

/// Creation of a new text node with an extra content length parameter. The
/// text node pertain to a given document.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocTextLen")]
pub unsafe fn xml_new_doc_text_len(
    doc: Option<XmlDocPtr>,
    content: *const XmlChar,
    len: i32,
) -> Option<XmlNodePtr> {
    unsafe {
        let cur = xml_new_text_len(content, len);
        if let Some(mut cur) = cur {
            cur.doc = doc;
        }
        cur
    }
}

/// Use of this function is DISCOURAGED in favor of xmlNewDocTextLen.
///
/// Creation of a new text node with an extra parameter for the content's length
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewTextLen")]
pub unsafe fn xml_new_text_len(content: *const XmlChar, len: i32) -> Option<XmlNodePtr> {
    unsafe {
        // Allocate a new node and fill the fields.
        let Some(mut cur) = XmlNodePtr::new(XmlNode {
            typ: XmlElementType::XmlTextNode,
            name: XML_STRING_TEXT.as_ptr() as _,
            ..Default::default()
        }) else {
            xml_tree_err_memory("building text");
            return None;
        };
        if !content.is_null() {
            cur.content = xml_strndup(content, len);
        }

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        //  && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

/// Creation of a new node containing a comment within a document.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocComment")]
pub unsafe fn xml_new_doc_comment(doc: Option<XmlDocPtr>, content: &str) -> Option<XmlNodePtr> {
    unsafe {
        let cur = xml_new_comment(content);
        if let Some(mut cur) = cur {
            cur.doc = doc;
        }
        cur
    }
}

/// Use of this function is DISCOURAGED in favor of xmlNewDocComment.
///
/// Creation of a new node containing a comment.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewComment")]
pub unsafe fn xml_new_comment(content: &str) -> Option<XmlNodePtr> {
    unsafe {
        // Allocate a new node and fill the fields.
        let Some(cur) = XmlNodePtr::new(XmlNode {
            typ: XmlElementType::XmlCommentNode,
            name: XML_STRING_COMMENT.as_ptr() as _,
            content: xml_strndup(content.as_ptr(), content.len() as i32),
            ..Default::default()
        }) else {
            xml_tree_err_memory("building comment");
            return None;
        };

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        //  && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

/// Creation of a new node containing a CDATA block.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewCDataBlock")]
pub unsafe fn xml_new_cdata_block(doc: Option<XmlDocPtr>, content: &str) -> Option<XmlNodePtr> {
    unsafe {
        // Allocate a new node and fill the fields.
        let Some(cur) = XmlNodePtr::new(XmlNode {
            typ: XmlElementType::XmlCDATASectionNode,
            doc,
            content: xml_strndup(content.as_ptr(), content.len() as i32),
            ..Default::default()
        }) else {
            xml_tree_err_memory("building CDATA");
            return None;
        };

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        //  && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

/// Creation of a new character reference node.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewCharRef")]
pub unsafe fn xml_new_char_ref(doc: Option<XmlDocPtr>, name: &str) -> Option<XmlNodePtr> {
    unsafe {
        // Allocate a new node and fill the fields.
        let Some(mut cur) = XmlNodePtr::new(XmlNode {
            typ: XmlElementType::XmlEntityRefNode,
            doc,
            ..Default::default()
        }) else {
            xml_tree_err_memory("building character reference");
            return None;
        };
        if let Some(name) = name.strip_prefix('&') {
            let len = name.len();
            if let Some(name) = name.strip_suffix(';') {
                cur.name = xml_strndup(name.as_ptr(), len as i32 - 1);
            } else {
                cur.name = xml_strndup(name.as_ptr(), len as i32);
            }
        } else {
            let name = CString::new(name).unwrap();
            cur.name = xml_strdup(name.as_ptr() as *const u8);
        }

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        //  && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

/// Creation of a new reference node.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewReference")]
pub unsafe fn xml_new_reference(doc: Option<XmlDocPtr>, name: &str) -> Option<XmlNodePtr> {
    unsafe {
        // Allocate a new node and fill the fields.
        let Some(mut cur) = XmlNodePtr::new(XmlNode {
            typ: XmlElementType::XmlEntityRefNode,
            doc,
            ..Default::default()
        }) else {
            xml_tree_err_memory("building reference");
            return None;
        };
        if let Some(name) = name.strip_prefix('&') {
            let len = name.len();
            if let Some(name) = name.strip_suffix(';') {
                cur.name = xml_strndup(name.as_ptr(), len as i32 - 1);
            } else {
                cur.name = xml_strndup(name.as_ptr(), len as i32);
            }
        } else {
            let name = CString::new(name).unwrap();
            cur.name = xml_strdup(name.as_ptr() as *const u8);
        }

        let ent = xml_get_doc_entity(doc, &cur.name().unwrap());
        if let Some(ent) = ent {
            cur.content = ent.content;
            // The parent pointer in entity is a DTD pointer and thus is NOT
            // updated.  Not sure if this is 100% correct.
            //  -George
            cur.set_children(Some(ent.into()));
            cur.set_last(Some(ent.into()));
        }

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        //  && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

/// Do a copy of the node.
///
/// Returns: a new #XmlNodePtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyNode")]
pub unsafe fn xml_copy_node(node: XmlGenericNodePtr, extended: i32) -> Option<XmlGenericNodePtr> {
    unsafe { xml_static_copy_node(node, None, None, extended) }
}

/// Do a copy of the node to a given document.
///
/// Returns: a new #XmlNodePtr, or null_mut() in case of error.
#[doc(alias = "xmlDocCopyNode")]
pub unsafe fn xml_doc_copy_node(
    node: XmlGenericNodePtr,
    doc: Option<XmlDocPtr>,
    extended: i32,
) -> Option<XmlGenericNodePtr> {
    unsafe { xml_static_copy_node(node, doc, None, extended) }
}

/// Do a recursive copy of the node list.
///
/// Returns: a new #XmlNodePtr, or null_mut() in case of error.
#[doc(alias = "xmlDocCopyNodeList")]
pub unsafe fn xml_doc_copy_node_list(
    doc: Option<XmlDocPtr>,
    node: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe { xml_static_copy_node_list(node, doc, None) }
}

/// Do a recursive copy of the node list.
/// Use xmlDocCopyNodeList() if possible to ensure string interning.
///
/// Returns: a new #XmlNodePtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyNodeList")]
pub unsafe fn xml_copy_node_list(node: Option<XmlGenericNodePtr>) -> Option<XmlGenericNodePtr> {
    unsafe { xml_static_copy_node_list(node, None, None) }
}

/// Creation of a new child element, added at the end of @parent children list.
/// @ns and @content parameters are optional (null_mut()). If @ns is null_mut(), the newly
/// created element inherits the namespace of @parent. If @content is non null_mut(),
/// a child TEXT node will be created containing the string @content.
/// NOTE: Use xmlNewChild() if @content will contain entities that need to be
/// preserved. Use this function, xmlNewTextChild(), if you need to ensure that
/// reserved XML chars that might appear in @content, such as the ampersand,
/// greater-than or less-than signs, are automatically replaced by their XML
/// escaped entity representations.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewTextChild")]
#[cfg(feature = "libxml_tree")]
pub unsafe fn xml_new_text_child(
    mut parent: XmlGenericNodePtr,
    ns: Option<XmlNsPtr>,
    name: &str,
    content: *const XmlChar,
) -> Option<XmlNodePtr> {
    unsafe {
        // Allocate a new node
        let mut cur = if let Some(parent) = XmlNodePtr::try_from(parent)
            .ok()
            .filter(|parent| matches!(parent.element_type(), XmlElementType::XmlElementNode))
        {
            if ns.is_none() {
                xml_new_doc_raw_node(parent.doc, parent.ns, name, content)
            } else {
                xml_new_doc_raw_node(parent.doc, ns, name, content)
            }
        } else if let Ok(parent) = XmlDocPtr::try_from(parent) {
            if ns.is_none() {
                xml_new_doc_raw_node(Some(parent), None, name, content)
            } else {
                xml_new_doc_raw_node(Some(parent), ns, name, content)
            }
        } else if matches!(parent.element_type(), XmlElementType::XmlDocumentFragNode) {
            xml_new_doc_raw_node(parent.document(), ns, name, content)
        } else {
            None
        }?;

        // add the new element at the end of the children list.
        cur.typ = XmlElementType::XmlElementNode;
        cur.set_parent(Some(parent));
        cur.doc = parent.document();
        if parent.children().is_none() {
            parent.set_children(Some(cur.into()));
            parent.set_last(Some(cur.into()));
        } else {
            let mut prev = parent.last().unwrap();
            prev.set_next(Some(cur.into()));
            cur.prev = Some(prev);
            parent.set_last(Some(cur.into()));
        }

        Some(cur)
    }
}

/// Creation of a new node element within a document. @ns and @content
/// are optional (null_mut()).
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocRawNode")]
#[cfg(feature = "libxml_tree")]
pub unsafe fn xml_new_doc_raw_node(
    doc: Option<XmlDocPtr>,
    ns: Option<XmlNsPtr>,
    name: &str,
    content: *const XmlChar,
) -> Option<XmlNodePtr> {
    unsafe {
        let cur = xml_new_doc_node(doc, ns, name, null_mut());
        if let Some(mut cur) = cur {
            cur.doc = doc;
            if !content.is_null() {
                cur.set_children(xml_new_doc_text(doc, content).map(|node| node.into()));
                if let Some(mut ulccur) = cur.children() {
                    while let Some(next) = ulccur.next() {
                        ulccur.set_parent(Some(cur.into()));
                        ulccur = next;
                    }
                    (*ulccur).set_parent(Some(cur.into()));
                    cur.set_last(Some(ulccur));
                } else {
                    cur.set_last(None);
                }
            }
        }
        cur
    }
}

/// Creation of a new Fragment node.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocFragment")]
#[cfg(feature = "libxml_tree")]
pub unsafe fn xml_new_doc_fragment(doc: Option<XmlDocPtr>) -> Option<XmlNodePtr> {
    unsafe {
        // Allocate a new DocumentFragment node and fill the fields.
        let Some(cur) = XmlNodePtr::new(XmlNode {
            typ: XmlElementType::XmlDocumentFragNode,
            doc,
            ..Default::default()
        }) else {
            xml_tree_err_memory("building fragment");
            return None;
        };

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        //  && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

/// Unlink the old node from its current context, prune the new one
/// at the same place. If @cur was already inserted in a document it is
/// first unlinked from its existing context.
///
/// See the note regarding namespaces in xmlAddChild.
///
/// Returns the @old node
#[doc(alias = "xmlReplaceNode")]
#[cfg(any(feature = "libxml_tree", feature = "libxml_writer"))]
pub unsafe fn xml_replace_node(
    mut old: XmlGenericNodePtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if Some(old) == cur {
            return None;
        }
        if matches!(old.element_type(), XmlElementType::XmlNamespaceDecl) || old.parent().is_none()
        {
            return None;
        }
        let Some(mut cur) =
            cur.filter(|cur| !matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl))
        else {
            old.unlink();
            return Some(old);
        };
        if cur == old {
            return Some(old);
        }
        if matches!(old.element_type(), XmlElementType::XmlAttributeNode)
            && !matches!(cur.element_type(), XmlElementType::XmlAttributeNode)
        {
            return Some(old);
        }
        if matches!(cur.element_type(), XmlElementType::XmlAttributeNode)
            && !matches!(old.element_type(), XmlElementType::XmlAttributeNode)
        {
            return Some(old);
        }
        cur.unlink();
        cur.set_doc(old.document());
        cur.set_parent(old.parent());
        cur.set_next(old.next());
        if let Some(mut next) = cur.next() {
            next.set_prev(Some(cur));
        }
        cur.set_prev(old.prev());
        if let Some(mut prev) = cur.prev() {
            prev.set_next(Some(cur));
        }
        if let Some(mut parent) = cur.parent() {
            if matches!(cur.element_type(), XmlElementType::XmlAttributeNode) {
                let mut parent = XmlNodePtr::try_from(parent).unwrap();
                if parent.properties.map(|prop| prop.into()) == Some(old) {
                    parent.properties = Some(XmlAttrPtr::try_from(cur).unwrap());
                }
            } else {
                if parent.children() == Some(old) {
                    parent.set_children(Some(cur));
                }
                if parent.last() == Some(old) {
                    parent.set_last(Some(cur));
                }
            }
        }
        old.set_next(None);
        old.set_prev(None);
        old.set_parent(None);
        Some(old)
    }
}

/// Merge two text nodes into one
/// Returns the first text node augmented
#[doc(alias = "xmlTextMerge")]
pub unsafe fn xml_text_merge(
    first: Option<XmlNodePtr>,
    second: Option<XmlNodePtr>,
) -> Option<XmlNodePtr> {
    unsafe {
        match (first, second) {
            (Some(mut first), Some(mut second))
                if first.element_type() == XmlElementType::XmlTextNode
                    && second.element_type() == XmlElementType::XmlTextNode =>
            {
                if first.name != second.name {
                    return Some(first);
                }
                first.add_content(second.content);
                second.unlink();
                xml_free_node(second);
                Some(first)
            }
            (Some(first), Some(_)) => Some(first),
            (Some(text), None) | (None, Some(text)) => Some(text),
            (None, None) => None,
        }
    }
}

/// Concat the given string at the end of the existing node content
///
/// Returns -1 in case of error, 0 otherwise
#[doc(alias = "xmlTextConcat")]
pub unsafe fn xml_text_concat(mut node: XmlNodePtr, content: &str) -> i32 {
    unsafe {
        // if node.is_null() {
        //     return -1;
        // }

        if !matches!(node.element_type(), XmlElementType::XmlTextNode)
            && !matches!(node.element_type(), XmlElementType::XmlCDATASectionNode)
            && !matches!(node.element_type(), XmlElementType::XmlCommentNode)
            && !matches!(node.element_type(), XmlElementType::XmlPINode)
        {
            return -1;
        }
        // need to check if content is currently in the dictionary
        node.content = xml_strncat(node.content, content.as_ptr(), content.len() as i32);
        node.properties = None;
        if node.content.is_null() {
            return -1;
        }
        0
    }
}

/// Free a node and all its siblings, this is a recursive behaviour, all
/// the children are freed too.
#[doc(alias = "xmlFreeNodeList")]
pub unsafe fn xml_free_node_list(cur: Option<impl Into<XmlGenericNodePtr>>) {
    unsafe {
        let mut depth: usize = 0;

        let Some(mut cur) = cur.map(|cur| cur.into()) else {
            return;
        };
        if let Ok(ns) = XmlNsPtr::try_from(cur) {
            xml_free_ns_list(ns);
            return;
        }
        loop {
            // This conditional statement cannot be replaced by `let Some`.
            // Calling `children()` when `cur` type is `XmlEntityRefNode` will not work correctly.
            // Cause unknown for now...
            while !matches!(
                cur.element_type(),
                XmlElementType::XmlDocumentNode
                    | XmlElementType::XmlHTMLDocumentNode
                    | XmlElementType::XmlDTDNode
                    | XmlElementType::XmlEntityRefNode
            ) && cur.children().is_some()
            {
                cur = cur.children().unwrap();
                depth += 1;
            }

            let next = cur.next();
            let parent = cur.parent();
            if let Ok(doc) = XmlDocPtr::try_from(cur) {
                xml_free_doc(doc);
            } else if !matches!(cur.element_type(), XmlElementType::XmlDTDNode) {
                let cur = XmlNodePtr::try_from(cur).unwrap();
                if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
                // && xmlDeregisterNodeDefaultValue.is_some()
                {
                    xml_deregister_node_default_value(cur.into());
                }

                if (matches!(cur.element_type(), XmlElementType::XmlElementNode)
                    || matches!(cur.element_type(), XmlElementType::XmlXIncludeStart)
                    || matches!(cur.element_type(), XmlElementType::XmlXIncludeEnd))
                    && cur.properties.is_some()
                {
                    xml_free_prop_list(cur.properties);
                }
                if !matches!(cur.element_type(), XmlElementType::XmlElementNode)
                    && !matches!(cur.element_type(), XmlElementType::XmlXIncludeStart)
                    && !matches!(cur.element_type(), XmlElementType::XmlXIncludeEnd)
                    && !matches!(cur.element_type(), XmlElementType::XmlEntityRefNode)
                    && !cur.content.is_null()
                {
                    xml_free(cur.content as _);
                }
                if matches!(cur.element_type(), XmlElementType::XmlElementNode)
                    || matches!(cur.element_type(), XmlElementType::XmlXIncludeStart)
                    || matches!(cur.element_type(), XmlElementType::XmlXIncludeEnd)
                {
                    if let Some(ns_def) = cur.ns_def {
                        xml_free_ns_list(ns_def);
                    }
                }

                // When a node is a text node or a comment, it uses a global static
                // variable for the name of the node.
                // Otherwise the node name might come from the document's
                // dictionary
                if !cur.name.is_null()
                    && !matches!(cur.element_type(), XmlElementType::XmlTextNode)
                    && !matches!(cur.element_type(), XmlElementType::XmlCommentNode)
                {
                    xml_free(cur.name as _);
                }
                cur.free();
            }

            if let Some(next) = next {
                cur = next;
            } else {
                if depth == 0 {
                    break;
                }
                let Some(parent) = parent else {
                    break;
                };
                depth -= 1;
                cur = parent;
                cur.set_children(None);
            }
        }
    }
}

/// Free a node, this is a recursive behaviour, all the children are freed too.
/// This doesn't unlink the child from the list, use xmlUnlinkNode() first.
#[doc(alias = "xmlFreeNode")]
pub unsafe fn xml_free_node(cur: impl Into<XmlGenericNodePtr>) {
    unsafe {
        let cur = cur.into();
        // use xmlFreeDtd for DTD nodes
        if let Ok(dtd) = XmlDtdPtr::try_from(cur) {
            xml_free_dtd(dtd);
            return;
        }
        if let Ok(ns) = XmlNsPtr::try_from(cur) {
            xml_free_ns(ns);
            return;
        }
        if let Ok(attr) = XmlAttrPtr::try_from(cur) {
            xml_free_prop(attr);
            return;
        }

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        // && xmlDeregisterNodeDefaultValue.is_some()
        {
            xml_deregister_node_default_value(cur);
        }

        if let Ok(mut ent) = XmlEntityPtr::try_from(cur) {
            let system_id = ent.system_id;
            if !system_id.is_null() {
                xml_free(system_id as _);
            }
            ent.system_id = null_mut();
            let external_id = ent.external_id;
            if !external_id.is_null() {
                xml_free(external_id as _);
            }
            ent.external_id = null_mut();
        }
        if let Some(children) = cur
            .children()
            .filter(|_| !matches!(cur.element_type(), XmlElementType::XmlEntityRefNode))
        {
            xml_free_node_list(Some(children));
        }

        if let Ok(mut cur) = XmlNodePtr::try_from(cur) {
            if matches!(
                cur.element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlXIncludeStart
                    | XmlElementType::XmlXIncludeEnd
            ) {
                if cur.properties.is_some() {
                    xml_free_prop_list(cur.properties);
                }
                if let Some(ns_def) = cur.ns_def.take() {
                    xml_free_ns_list(ns_def);
                }
            } else if !cur.content.is_null()
                && !matches!(cur.element_type(), XmlElementType::XmlEntityRefNode)
                && !cur.content.is_null()
            {
                xml_free(cur.content as _);
            }

            // When a node is a text node or a comment, it uses a global static
            // variable for the name of the node.
            // Otherwise the node name might come from the document's dictionary
            if !cur.name.is_null()
                && !matches!(cur.element_type(), XmlElementType::XmlTextNode)
                && !matches!(cur.element_type(), XmlElementType::XmlCommentNode)
            {
                xml_free(cur.name as _);
            }

            cur.free();
        } else if let Ok(cur) = XmlEntityPtr::try_from(cur) {
            let content = cur.content;
            if !content.is_null() {
                xml_free(content as _);
            }
            // When a node is a text node or a comment, it uses a global static
            // variable for the name of the node.
            // Otherwise the node name might come from the document's dictionary
            let name = cur.name;
            if !name.is_null() {
                xml_free(name as _);
            }
        } else {
            // Does this pattern occur ???
            todo!()
        }
    }
}

/// Verify that the given namespace held on @ancestor is still in scope on node.
///
/// Returns 1 if true, 0 if false and -1 in case of error.
#[doc(alias = "xmlNsInScope")]
unsafe fn xml_ns_in_scope(
    _doc: Option<XmlDocPtr>,
    mut node: Option<XmlGenericNodePtr>,
    ancestor: Option<XmlGenericNodePtr>,
    prefix: *const XmlChar,
) -> i32 {
    unsafe {
        while let Some(cur_node) = node.filter(|&node| Some(node) != ancestor) {
            if matches!(
                cur_node.element_type(),
                XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode
                    | XmlElementType::XmlEntityDecl
            ) {
                return -1;
            }
            if let Some(cur_node) = XmlNodePtr::try_from(cur_node).ok().filter(|cur_node| {
                matches!(cur_node.element_type(), XmlElementType::XmlElementNode)
            }) {
                let mut tst = cur_node.ns_def;
                while let Some(now) = tst {
                    if now.prefix().is_none() && prefix.is_null() {
                        return 0;
                    }
                    if now.prefix().is_some()
                        && now.prefix()
                            == (!prefix.is_null())
                                .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                    {
                        return 0;
                    }
                    tst = now.next;
                }
            }
            node = cur_node.parent();
        }
        if node != ancestor {
            return -1;
        }
        1
    }
}

/// Do a copy of the namespace.
///
/// Returns: a new #xmlNsPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyNamespace")]
pub unsafe fn xml_copy_namespace(cur: Option<XmlNsPtr>) -> Option<XmlNsPtr> {
    unsafe {
        let cur = cur?;
        match cur.element_type() {
            XML_LOCAL_NAMESPACE => xml_new_ns(None, cur.href, cur.prefix().as_deref()),
            _ => None,
        }
    }
}

/// Do a copy of an namespace list.
///
/// Returns: a new #xmlNsPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyNamespaceList")]
pub unsafe fn xml_copy_namespace_list(mut cur: Option<XmlNsPtr>) -> Option<XmlNsPtr> {
    unsafe {
        let mut ret = None::<XmlNsPtr>;
        let mut p = None::<XmlNsPtr>;

        while let Some(now) = cur {
            let Some(q) = xml_copy_namespace(Some(now)) else {
                if let Some(ret) = ret {
                    xml_free_ns_list(ret);
                }
                return None;
            };
            if let Some(mut l) = p {
                l.next = Some(q);
                p = Some(q);
            } else {
                ret = Some(q);
                p = ret;
            }
            cur = now.next;
        }
        ret
    }
}

static XML_CHECK_DTD: AtomicBool = AtomicBool::new(true);

/// Handle an out of memory condition
#[doc(alias = "xmlTreeErr")]
unsafe fn xml_tree_err(
    code: XmlParserErrors,
    node: Option<XmlGenericNodePtr>,
    extra: Option<&str>,
) {
    unsafe {
        let msg: Cow<'static, str> = match code {
            XmlParserErrors::XmlTreeInvalidHex => "invalid hexadecimal character value\n".into(),
            XmlParserErrors::XmlTreeInvalidDec => "invalid decimal character value\n".into(),
            XmlParserErrors::XmlTreeUnterminatedEntity => format!(
                "unterminated entity reference {}\n",
                extra.expect("Internal Error")
            )
            .into(),
            XmlParserErrors::XmlTreeNotUTF8 => "string is not in UTF-8\n".into(),
            _ => "unexpected error number\n".into(),
        };
        __xml_simple_error!(XmlErrorDomain::XmlFromTree, code, node, &msg);
    }
}

const XHTML_STRICT_PUBLIC_ID: &str = "-//W3C//DTD XHTML 1.0 Strict//EN";
const XHTML_STRICT_SYSTEM_ID: &str = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd";
const XHTML_FRAME_PUBLIC_ID: &str = "-//W3C//DTD XHTML 1.0 Frameset//EN";
const XHTML_FRAME_SYSTEM_ID: &str = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd";
const XHTML_TRANS_PUBLIC_ID: &str = "-//W3C//DTD XHTML 1.0 Transitional//EN";
const XHTML_TRANS_SYSTEM_ID: &str = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd";

/// Try to find if the document correspond to an XHTML DTD.
#[doc(alias = "xmlIsXHTML")]
pub fn is_xhtml(system_id: Option<&str>, public_id: Option<&str>) -> bool {
    public_id.is_some_and(|s| {
        s == XHTML_STRICT_PUBLIC_ID || s == XHTML_FRAME_PUBLIC_ID || s == XHTML_TRANS_PUBLIC_ID
    }) || system_id.is_some_and(|s| {
        s == XHTML_STRICT_SYSTEM_ID || s == XHTML_FRAME_SYSTEM_ID || s == XHTML_TRANS_SYSTEM_ID
    })
}

static XML_COMPRESS_MODE: AtomicI32 = AtomicI32::new(0);

/// Get the default compression mode used, ZLIB based.
///
/// Returns 0 (uncompressed) to 9 (max compression)
#[doc(alias = "xmlGetCompressMode")]
pub fn get_compress_mode() -> i32 {
    XML_COMPRESS_MODE.load(Ordering::Acquire)
}

/// Set the default compression mode used, ZLIB based.
///
/// Correct values: 0 (uncompressed) to 9 (max compression)
#[doc(alias = "xmlSetCompressMode")]
pub fn set_compress_mode(mode: i32) {
    if mode < 0 {
        XML_COMPRESS_MODE.store(0, Ordering::Release);
    } else if mode > 9 {
        XML_COMPRESS_MODE.store(9, Ordering::Release);
    } else {
        XML_COMPRESS_MODE.store(mode, Ordering::Release);
    }
}

macro_rules! IS_STR_XML {
    ($str:expr) => {
        !$str.is_null()
            && *$str.add(0) == b'x'
            && *$str.add(1) == b'm'
            && *$str.add(2) == b'l'
            && *$str.add(3) == 0
    };
}

/// Searches for a ns-decl with the given prefix in @nsList.
///
/// Returns the ns-decl if found, null_mut() if not found and on API errors.
#[doc(alias = "xmlTreeLookupNsListByPrefix")]
unsafe fn xml_tree_nslist_lookup_by_prefix(
    ns_list: Option<XmlNsPtr>,
    prefix: *const XmlChar,
) -> Option<XmlNsPtr> {
    unsafe {
        let mut ns = ns_list;
        while let Some(now) = ns {
            if prefix == now.prefix || xml_str_equal(prefix, now.prefix) {
                return ns;
            }
            ns = now.next;
        }
        None
    }
}

/// Dynamically searches for a ns-declaration which matches
/// the given @nsName in the ancestor-or-self axis of @node.
///
/// Returns 1 if a ns-decl was found, 0 if not and -1 on API and internal errors.
#[doc(alias = "xmlSearchNsByPrefixStrict")]
unsafe fn xml_search_ns_by_prefix_strict(
    mut doc: XmlDocPtr,
    node: XmlGenericNodePtr,
    prefix: *const XmlChar,
    mut ret_ns: Option<&mut Option<XmlNsPtr>>,
) -> i32 {
    unsafe {
        if matches!(node.element_type(), XmlElementType::XmlNamespaceDecl) {
            return -1;
        }

        if let Some(ret_ns) = ret_ns.as_deref_mut() {
            *ret_ns = None;
        }
        if IS_STR_XML!(prefix) {
            if let Some(ret_ns) = ret_ns {
                *ret_ns = doc.ensure_xmldecl();
                if ret_ns.is_none() {
                    return -1;
                }
            }
            return 1;
        }
        let mut cur = Some(node);
        while let Some(cur_node) =
            cur.filter(|&cur| cur.document().map(|doc| doc.into()) != Some(cur))
        {
            if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                let mut ns = cur_node.ns_def;
                while let Some(now) = ns {
                    if prefix == now.prefix || xml_str_equal(prefix, now.prefix) {
                        // Disabled namespaces, e.g. xmlns:abc="".
                        if now.href.is_null() {
                            return 0;
                        }
                        if let Some(ret_ns) = ret_ns {
                            *ret_ns = ns;
                        }
                        return 1;
                    }
                    ns = now.next;
                }
            } else if (matches!(cur_node.element_type(), XmlElementType::XmlEntityNode)
                || matches!(cur_node.element_type(), XmlElementType::XmlEntityDecl))
            {
                return 0;
            }
            cur = cur_node.parent();
        }
        0
    }
}

/// Dynamically searches for a ns-declaration which matches
/// the given @nsName in the ancestor-or-self axis of @node.
///
/// Returns 1 if a ns-decl was found, 0 if not and -1 on API and internal errors.
#[doc(alias = "xmlSearchNsByNamespaceStrict")]
unsafe fn xml_search_ns_by_namespace_strict(
    mut doc: XmlDocPtr,
    node: XmlGenericNodePtr,
    ns_name: *const XmlChar,
    ret_ns: &mut Option<XmlNsPtr>,
    prefixed: i32,
) -> i32 {
    unsafe {
        if ns_name.is_null() {
            return -1;
        }
        // if node.is_null() {
        //     return -1;
        // }
        if matches!(node.element_type(), XmlElementType::XmlNamespaceDecl) {
            return -1;
        }

        *ret_ns = None;
        if xml_str_equal(ns_name, XML_XML_NAMESPACE.as_ptr() as _) {
            *ret_ns = doc.ensure_xmldecl();
            if ret_ns.is_none() {
                return -1;
            }
            return 1;
        }
        let mut cur = Some(node);
        let mut prev: Option<XmlNodePtr> = None;
        let mut out = None;
        while let Some(cur_node) =
            cur.filter(|&cur| Some(cur) != cur.document().map(|doc| doc.into()))
        {
            if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                let cur = XmlNodePtr::try_from(cur_node).unwrap();
                if cur.ns_def.is_some() {
                    let mut ns = cur.ns_def;
                    while let Some(now) = ns {
                        if prefixed != 0 && now.prefix().is_none() {
                            ns = now.next;
                            continue;
                        }
                        if let Some(prev) = prev {
                            // Check the last level of ns-decls for a
                            // shadowing prefix.
                            let mut prevns = prev.ns_def;
                            while let Some(pns) = prevns {
                                if pns.prefix() == now.prefix()
                                    || (pns.prefix().is_some()
                                        && now.prefix().is_some()
                                        && pns.prefix() == now.prefix())
                                {
                                    // Shadowed.
                                    break;
                                }
                                prevns = pns.next;
                            }
                            if prevns.is_some() {
                                ns = now.next;
                                continue;
                            }
                        }
                        // Ns-name comparison.
                        if ns_name == now.href || xml_str_equal(ns_name, now.href) {
                            // At this point the prefix can only be shadowed,
                            // if we are the the (at least) 3rd level of ns-decls.
                            if out.is_some() {
                                let ret: i32 = xml_ns_in_scope(
                                    Some(doc),
                                    Some(node),
                                    prev.map(|prev| prev.into()),
                                    now.prefix,
                                );
                                if ret < 0 {
                                    return -1;
                                }
                                // TODO: Should we try to find a matching ns-name
                                // only once? This here keeps on searching.
                                // I think we should try further since, there might
                                // be an other matching ns-decl with an unshadowed
                                // prefix.
                                if ret == 0 {
                                    ns = now.next;
                                    continue;
                                }
                            }
                            *ret_ns = ns;
                            return 1;
                        }
                    }
                    out = prev;
                    prev = Some(cur);
                }
            } else if (matches!(cur_node.element_type(), XmlElementType::XmlEntityNode)
                || matches!(cur_node.element_type(), XmlElementType::XmlEntityDecl))
            {
                return 0;
            }
            cur = cur_node.parent();
        }
        0
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        globals::reset_last_error,
        libxml::{xmlmemory::xml_mem_blocks, xmlstring::xml_strlen},
        test_util::*,
    };

    use super::*;

    #[test]
    fn test_xml_build_qname() {
        unsafe {
            let mut leaks = 0;
            for n_ncname in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_memory in 0..GEN_NB_XML_CHAR_PTR {
                        for n_len in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let ncname = gen_const_xml_char_ptr(n_ncname, 0);
                            let prefix = gen_const_xml_char_ptr(n_prefix, 1);
                            let memory = gen_xml_char_ptr(n_memory, 2);
                            let mut len = gen_int(n_len, 3);
                            if !prefix.is_null() && len > xml_strlen(prefix) {
                                len = 0;
                            }

                            let ret_val =
                                xml_build_qname(ncname as *const XmlChar, prefix, memory, len);
                            if !ret_val.is_null()
                                && ret_val != ncname as _
                                && ret_val != prefix as _
                                && ret_val != memory
                            {
                                xml_free(ret_val as _);
                            }
                            let ret_val = null_mut();
                            desret_xml_char_ptr(ret_val);
                            des_const_xml_char_ptr(n_ncname, ncname, 0);
                            des_const_xml_char_ptr(n_prefix, prefix, 1);
                            des_xml_char_ptr(n_memory, memory, 2);
                            des_int(n_len, len, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlBuildQName",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlBuildQName()");
                                eprint!(" {}", n_ncname);
                                eprint!(" {}", n_prefix);
                                eprint!(" {}", n_memory);
                                eprintln!(" {}", n_len);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_get_compress_mode() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            let ret_val = get_compress_mode();
            desret_int(ret_val);
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlGetCompressMode",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlGetCompressMode()"
                );
            }
        }
    }

    #[test]
    fn test_xml_split_qname2() {
        unsafe {
            let mut leaks = 0;

            for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_prefix in 0..GEN_NB_XML_CHAR_PTR_PTR {
                    let mem_base = xml_mem_blocks();
                    let name = gen_const_xml_char_ptr(n_name, 0);
                    let prefix = gen_xml_char_ptr_ptr(n_prefix, 1);

                    let ret_val = xml_split_qname2(name as *const XmlChar, prefix);
                    desret_xml_char_ptr(ret_val);
                    des_const_xml_char_ptr(n_name, name, 0);
                    des_xml_char_ptr_ptr(n_prefix, prefix, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSplitQName2",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlSplitQName2()");
                        eprint!(" {}", n_name);
                        eprintln!(" {}", n_prefix);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_split_qname3() {
        unsafe {
            let mut leaks = 0;

            for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_len in 0..GEN_NB_INT_PTR {
                    let mem_base = xml_mem_blocks();
                    let name = gen_const_xml_char_ptr(n_name, 0);
                    let len = gen_int_ptr(n_len, 1);

                    let ret_val = xml_split_qname3(name as *const XmlChar, len);
                    desret_const_xml_char_ptr(ret_val);
                    des_const_xml_char_ptr(n_name, name, 0);
                    des_int_ptr(n_len, len, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSplitQName3",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlSplitQName3()");
                        eprint!(" {}", n_name);
                        eprintln!(" {}", n_len);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_nmtoken() {
        #[cfg(any(feature = "libxml_tree", feature = "schema"))]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "libxml_tree")]
            {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_space in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let value = gen_const_xml_char_ptr(n_value, 0);
                        let space = gen_int(n_space, 1);

                        let ret_val = xml_validate_nmtoken(value as *const XmlChar, space);
                        desret_int(ret_val);
                        des_const_xml_char_ptr(n_value, value, 0);
                        des_int(n_space, space, 1);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlValidateNMToken",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlValidateNMToken()"
                            );
                            eprint!(" {}", n_value);
                            eprintln!(" {}", n_space);
                        }
                    }
                }
            }
        }
    }
}

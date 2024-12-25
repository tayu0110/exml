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
mod id;
mod namespace;
mod node;

use std::{
    any::type_name,
    borrow::Cow,
    ffi::{c_char, CStr, CString},
    mem::size_of,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::{AtomicBool, AtomicI32, Ordering},
};

use libc::{memcpy, memset, snprintf, strlen};

pub(crate) use crate::buf::libxml_api::*;
use crate::{
    error::{XmlErrorDomain, XmlParserErrors, __xml_simple_error, __xml_simple_oom_error},
    libxml::{
        chvalid::xml_is_blank_char,
        dict::{xml_dict_free, xml_dict_lookup, xml_dict_owns, XmlDictPtr},
        entities::{xml_free_entities_table, xml_get_doc_entity, XmlEntityPtr, XmlEntityType},
        globals::{
            xml_deregister_node_default_value, xml_free, xml_malloc, xml_malloc_atomic,
            xml_realloc, xml_register_node_default_value,
        },
        parser_internals::{XML_STRING_COMMENT, XML_STRING_TEXT, XML_STRING_TEXT_NOENC},
        valid::{
            xml_add_id, xml_free_attribute_table, xml_free_element_table, xml_free_id_table,
            xml_free_notation_table, xml_free_ref_table, xml_is_id, xml_remove_id,
            XmlElementTablePtr, XmlNotationTablePtr,
        },
        xmlstring::{
            xml_str_equal, xml_strdup, xml_strncat, xml_strncat_new, xml_strndup, XmlChar,
        },
    },
};

pub use attribute::*;
pub use document::*;
pub use dom_wrapper::*;
pub use dtd::*;
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

pub type XmlNsMapItemPtr = *mut XmlNsMapItem;
#[repr(C)]
pub struct XmlNsMapItem {
    next: XmlNsMapItemPtr,
    prev: XmlNsMapItemPtr,
    old_ns: XmlNsPtr,  /* old ns decl reference */
    new_ns: XmlNsPtr,  /* new ns decl reference */
    shadow_depth: i32, /* Shadowed at this depth */
    /// depth:
    /// `>= 0` == @node's ns-decls
    /// `-1`   == @parent's ns-decls
    /// `-2`   == the (*doc).oldNs XML ns-decl
    /// `-3`   == the (*doc).oldNs storage ns-decls
    /// `-4`   == ns-decls provided via custom ns-handling
    depth: i32,
}

pub type XmlNsMapPtr = *mut XmlNsMap;
#[repr(C)]
pub struct XmlNsMap {
    first: XmlNsMapItemPtr,
    last: XmlNsMapItemPtr,
    pool: XmlNsMapItemPtr,
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
/// Returns 0 if this validates, a positive error code number otherwise
/// and -1 in case of internal or API error.
#[doc(alias = "xmlValidateNCName")]
#[cfg(any(
    feature = "libxml_tree",
    feature = "xpath",
    feature = "schema",
    feature = "html",
    feature = "sax1",
    feature = "libxml_writer",
    feature = "libxml_legacy"
))]
pub unsafe extern "C" fn xml_validate_ncname(value: *const XmlChar, space: i32) -> i32 {
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

    /*
     * First quick algorithm for ASCII range
     */
    if space != 0 {
        while xml_is_blank_char(*cur as u32) {
            cur = cur.add(1);
        }
    }
    if (*cur >= b'a' && *cur <= b'z') || (*cur >= b'A' && *cur <= b'Z') || *cur == b'_' {
        cur = cur.add(1);

        while (*cur >= b'a' && *cur <= b'z')
            || (*cur >= b'A' && *cur <= b'Z')
            || (*cur >= b'0' && *cur <= b'9')
            || *cur == b'_'
            || *cur == b'-'
            || *cur == b'.'
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
    }
    // else {
    //     goto try_complex;
    // }

    // try_complex:
    /*
     * Second check for chars outside the ASCII range
     */
    cur = value;
    c = CUR_SCHAR!(cur, l);
    if space != 0 {
        while xml_is_blank_char(c as u32) {
            cur = cur.add(l as usize);
            c = CUR_SCHAR!(cur, l);
        }
    }
    if !xml_is_letter(c as u32) && c != b'_' as i32 {
        return 1;
    }
    cur = cur.add(l as usize);
    c = CUR_SCHAR!(cur, l);
    while xml_is_letter(c as u32)
        || xml_is_digit(c as u32)
        || c == b'.' as i32
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

/// Check that a value conforms to the lexical space of QName
///
/// Returns 0 if this validates, a positive error code number otherwise
/// and -1 in case of internal or API error.
#[doc(alias = "xmlValidateQName")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub unsafe extern "C" fn xml_validate_qname(value: *const XmlChar, space: i32) -> i32 {
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
    /*
     * First quick algorithm for ASCII range
     */
    if space != 0 {
        while xml_is_blank_char(*cur as u32) {
            cur = cur.add(1);
        }
    }

    'try_complex: {
        if (*cur >= b'a' && *cur <= b'z') || (*cur >= b'A' && *cur <= b'Z') || *cur == b'_' {
            cur = cur.add(1);
        } else {
            break 'try_complex;
        }
        while (*cur >= b'a' && *cur <= b'z')
            || (*cur >= b'A' && *cur <= b'Z')
            || (*cur >= b'0' && *cur <= b'9')
            || *cur == b'_'
            || *cur == b'-'
            || *cur == b'.'
        {
            cur = cur.add(1);
        }
        if *cur == b':' {
            cur = cur.add(1);
            if (*cur >= b'a' && *cur <= b'z') || (*cur >= b'A' && *cur <= b'Z') || *cur == b'_' {
                cur = cur.add(1);
            } else {
                break 'try_complex;
            }
            while (*cur >= b'a' && *cur <= b'z')
                || (*cur >= b'A' && *cur <= b'Z')
                || (*cur >= b'0' && *cur <= b'9')
                || *cur == b'_'
                || *cur == b'-'
                || *cur == b'.'
            {
                cur = cur.add(1);
            }
        }
        if space != 0 {
            while xml_is_blank_char(*cur as u32) {
                cur = cur.add(1);
            }
        }
        if *cur == 0 {
            return 0;
        }
    }

    // try_complex:
    /*
     * Second check for chars outside the ASCII range
     */
    cur = value;
    c = CUR_SCHAR!(cur, l);
    if space != 0 {
        while xml_is_blank_char(c as u32) {
            cur = cur.add(l as usize);
            c = CUR_SCHAR!(cur, l);
        }
    }
    if !xml_is_letter(c as u32) && c != b'_' as i32 {
        return 1;
    }
    cur = cur.add(l as usize);
    c = CUR_SCHAR!(cur, l);
    while xml_is_letter(c as u32)
        || xml_is_digit(c as u32)
        || c == b'.' as i32
        || c == b'-' as i32
        || c == b'_' as i32
        || xml_is_combining(c as u32)
        || xml_is_extender(c as u32)
    {
        cur = cur.add(l as usize);
        c = CUR_SCHAR!(cur, l);
    }
    if c == b':' as i32 {
        cur = cur.add(l as usize);
        c = CUR_SCHAR!(cur, l);
        if !xml_is_letter(c as u32) && c != b'_' as i32 {
            return 1;
        }
        cur = cur.add(l as usize);
        c = CUR_SCHAR!(cur, l);
        while xml_is_letter(c as u32)
            || xml_is_digit(c as u32)
            || c == b'.' as i32
            || c == b'-' as i32
            || c == b'_' as i32
            || xml_is_combining(c as u32)
            || xml_is_extender(c as u32)
        {
            cur = cur.add(l as usize);
            c = CUR_SCHAR!(cur, l);
        }
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

/// Check that a value conforms to the lexical space of Name
///
/// Returns 0 if this validates, a positive error code number otherwise
/// and -1 in case of internal or API error.
#[doc(alias = "xmlValidateName")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub unsafe extern "C" fn xml_validate_name(value: *const XmlChar, space: i32) -> i32 {
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
    /*
     * First quick algorithm for ASCII range
     */
    if space != 0 {
        while xml_is_blank_char(*cur as u32) {
            cur = cur.add(1);
        }
    }
    if (*cur >= b'a' && *cur <= b'z')
        || (*cur >= b'A' && *cur <= b'Z')
        || *cur == b'_'
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
    /*
     * Second check for chars outside the ASCII range
     */
    cur = value;
    c = CUR_SCHAR!(cur, l);
    if space != 0 {
        while xml_is_blank_char(c as u32) {
            cur = cur.add(l as usize);
            c = CUR_SCHAR!(cur, l);
        }
    }
    if !xml_is_letter(c as u32) && c != b'_' as i32 && c != b':' as i32 {
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

/// Check that a value conforms to the lexical space of NMToken
///
/// Returns 0 if this validates, a positive error code number otherwise
/// and -1 in case of internal or API error.
#[doc(alias = "xmlValidateNMToken")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub unsafe extern "C" fn xml_validate_nmtoken(value: *const XmlChar, space: i32) -> i32 {
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
    /*
     * First quick algorithm for ASCII range
     */
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
    /*
     * Second check for chars outside the ASCII range
     */
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

/// Handle an out of memory condition
#[doc(alias = "xmlTreeErrMemory")]
unsafe fn xml_tree_err_memory(extra: &str) {
    __xml_simple_oom_error(XmlErrorDomain::XmlFromTree, null_mut(), Some(extra));
}

/// Builds the QName @prefix:@ncname in @memory if there is enough space
/// and prefix is not NULL nor empty, otherwise allocate a new string.
/// If prefix is NULL or empty it returns ncname.
///
/// Returns the new string which must be freed by the caller if different from
/// @memory and @ncname or NULL in case of error
#[doc(alias = "xmlBuildQName")]
pub unsafe extern "C" fn xml_build_qname(
    ncname: *const XmlChar,
    prefix: *const XmlChar,
    memory: *mut XmlChar,
    len: i32,
) -> *mut XmlChar {
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

/// Parse an XML qualified name string
///
/// ```ignore
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
pub unsafe extern "C" fn xml_split_qname2(
    name: *const XmlChar,
    prefix: *mut *mut XmlChar,
) -> *mut XmlChar {
    let mut len: i32 = 0;

    if prefix.is_null() {
        return null_mut();
    }
    *prefix = null_mut();
    if name.is_null() {
        return null_mut();
    }

    /* nasty but valid */
    if *name.add(0) == b':' {
        return null_mut();
    }

    /*
     * we are not trying to validate but just to cut, and yes it will
     * work even if this is as set of UTF-8 encoded chars
     */
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

/// Parse an XML qualified name string
///
/// ```ignore
/// [NS 5] QName ::= (Prefix ':')? LocalPart
///
/// [NS 6] Prefix ::= NCName
///
/// [NS 7] LocalPart ::= NCName
/// ```
///
/// Returns `None` if the name doesn't have a prefix.
/// Otherwise, returns `Some((Prefix, LocalPart))`.
///
/// # Note:
/// This function does not perform validation.
#[doc(alias = "xmlSplitQName2")]
pub fn split_qname2(name: &str) -> Option<(&str, &str)> {
    // nasty but valid
    if name.starts_with(':') {
        return None;
    }

    // we are not trying to validate but just to cut, and yes it will
    // work even if this is as set of UTF-8 encoded chars
    name.split_once(':')
}

/// Rarse an XML qualified name string,i
///
/// Returns NULL if it is not a Qualified Name, otherwise, update len
/// with the length in byte of the prefix and return a pointer
/// to the start of the name without the prefix
#[doc(alias = "xmlSplitQName3")]
pub unsafe extern "C" fn xml_split_qname3(name: *const XmlChar, len: *mut i32) -> *const XmlChar {
    let mut l: i32 = 0;

    if name.is_null() {
        return null_mut();
    }
    if len.is_null() {
        return null_mut();
    }

    /* nasty but valid */
    if *name.add(0) == b':' {
        return null_mut();
    }

    /*
     * we are not trying to validate but just to cut, and yes it will
     * work even if this is as set of UTF-8 encoded chars
     */
    while *name.add(l as usize) != 0 && *name.add(l as usize) != b':' {
        l += 1;
    }

    if *name.add(l as usize) == 0 {
        return null_mut();
    }

    *len = l;

    name.add(l as usize + 1)
}

/// Free a string if it is not owned by the "dict" dictionary in the
/// current scope
macro_rules! DICT_FREE {
    ($dict:expr, $str:expr) => {
        if !$str.is_null()
            && ($dict.is_null() || crate::libxml::dict::xml_dict_owns($dict, $str as _) == 0)
        {
            xml_free($str as _);
        }
    };
}

/// Free a DTD structure.
#[doc(alias = "xmlFreeDtd")]
pub unsafe extern "C" fn xml_free_dtd(cur: XmlDtdPtr) {
    let mut dict: XmlDictPtr = null_mut();

    if cur.is_null() {
        return;
    }
    if !(*cur).doc.is_null() {
        dict = (*(*cur).doc).dict;
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    // && xmlDeregisterNodeDefaultValue.is_some()
    {
        xml_deregister_node_default_value(cur as _);
    }

    if let Some(children) = (*cur).children() {
        /*
         * Cleanup all nodes which are not part of the specific lists
         * of notations, elements, attributes and entities.
         */
        let mut c = Some(children);
        while let Some(mut now) = c {
            let next = now.next;
            if !matches!(
                now.element_type(),
                XmlElementType::XmlNotationNode
                    | XmlElementType::XmlElementDecl
                    | XmlElementType::XmlAttributeDecl
                    | XmlElementType::XmlEntityDecl
            ) {
                now.unlink();
                xml_free_node(now.as_ptr());
            }
            c = next;
        }
    }
    DICT_FREE!(dict, (*cur).name);
    (*cur).system_id = None;
    (*cur).external_id = None;
    /* TODO !!! */
    if !(*cur).notations.is_null() {
        xml_free_notation_table((*cur).notations as XmlNotationTablePtr);
    }

    if !(*cur).elements.is_null() {
        xml_free_element_table((*cur).elements as XmlElementTablePtr);
    }
    if let Some(table) = (*cur).attributes.take().map(|t| t.into_inner()) {
        xml_free_attribute_table(table);
    }
    if let Some(entities) = (*cur).entities.take() {
        xml_free_entities_table(entities);
    }
    if let Some(pentities) = (*cur).pentities.take() {
        xml_free_entities_table(pentities);
    }

    xml_free(cur as _);
}

/// Free up all the structures used by a document, tree included.
#[doc(alias = "xmlFreeDoc")]
pub unsafe extern "C" fn xml_free_doc(cur: XmlDocPtr) {
    let mut ext_subset: XmlDtdPtr;
    let mut dict: XmlDictPtr = null_mut();

    if cur.is_null() {
        return;
    }

    if !cur.is_null() {
        dict = (*cur).dict;
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    // && xmlDeregisterNodeDefaultValue.is_some()
    {
        xml_deregister_node_default_value(cur as _);
    }

    /*
     * Do this before freeing the children list to avoid ID lookups
     */
    if !(*cur).ids.is_null() {
        xml_free_id_table((*cur).ids as _);
    }
    (*cur).ids = null_mut();
    if !(*cur).refs.is_null() {
        xml_free_ref_table((*cur).refs as _);
    }
    (*cur).refs = null_mut();
    ext_subset = (*cur).ext_subset;
    let int_subset: XmlDtdPtr = (*cur).int_subset;
    if int_subset == ext_subset {
        ext_subset = null_mut();
    }
    if !ext_subset.is_null() {
        (*(*cur).ext_subset).unlink();
        (*cur).ext_subset = null_mut();
        xml_free_dtd(ext_subset);
    }
    if !int_subset.is_null() {
        (*(*cur).int_subset).unlink();
        (*cur).int_subset = null_mut();
        xml_free_dtd(int_subset);
    }

    if let Some(children) = (*cur).children() {
        xml_free_node_list(children.as_ptr());
    }
    if !(*cur).old_ns.is_null() {
        xml_free_ns_list((*cur).old_ns);
    }

    (*cur).version = None;
    DICT_FREE!(dict, (*cur).name);
    (*cur).encoding = None;
    (*cur).url = None;
    xml_free(cur as _);
    if !dict.is_null() {
        xml_dict_free(dict);
    }
}

unsafe fn xml_new_prop_internal(
    node: XmlNodePtr,
    ns: XmlNsPtr,
    name: *const XmlChar,
    value: *const XmlChar,
    eatname: i32,
) -> XmlAttrPtr {
    let mut doc: XmlDocPtr = null_mut();

    if !node.is_null() && !matches!((*node).element_type(), XmlElementType::XmlElementNode) {
        if eatname == 1
            && ((*node).doc.is_null()
                || (*(*node).doc).dict.is_null()
                || xml_dict_owns((*(*node).doc).dict, name) == 0)
        {
            xml_free(name as _);
        }
        return null_mut();
    }

    /*
     * Allocate a new property and fill the fields.
     */
    let cur: XmlAttrPtr = xml_malloc(size_of::<XmlAttr>()) as _;
    if cur.is_null() {
        if eatname == 1
            && (node.is_null()
                || (*node).doc.is_null()
                || (*(*node).doc).dict.is_null()
                || xml_dict_owns((*(*node).doc).dict, name) == 0)
        {
            xml_free(name as _);
        }
        xml_tree_err_memory("building attribute");
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlAttr>());
    (*cur).typ = XmlElementType::XmlAttributeNode;

    (*cur).parent = NodePtr::from_ptr(node);
    if !node.is_null() {
        doc = (*node).doc;
        (*cur).doc = doc;
    }
    (*cur).ns = ns;

    if eatname == 0 {
        if !doc.is_null() && !(*doc).dict.is_null() {
            (*cur).name = xml_dict_lookup((*doc).dict, name, -1);
        } else {
            (*cur).name = xml_strdup(name);
        }
    } else {
        (*cur).name = name;
    }

    if !value.is_null() {
        (*cur).children = NodePtr::from_ptr(xml_new_doc_text(doc, value));
        (*cur).set_last(None);
        let mut tmp = (*cur).children;
        while let Some(mut now) = tmp {
            now.set_parent(NodePtr::from_ptr(cur as *mut XmlNode));
            if now.next.is_none() {
                (*cur).last = Some(now);
            }
            tmp = now.next;
        }
    }

    /*
     * Add it at the end to preserve parsing order ...
     */
    if !node.is_null() {
        if (*node).properties.is_null() {
            (*node).properties = cur;
        } else {
            let mut prev: XmlAttrPtr = (*node).properties;

            while !(*prev).next.is_null() {
                prev = (*prev).next;
            }
            (*prev).next = cur;
            (*cur).prev = prev;
        }
    }

    if !value.is_null() && !node.is_null() && (xml_is_id((*node).doc, node, cur) == 1) {
        xml_add_id(null_mut(), (*node).doc, value, cur);
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/// Free a property and all its siblings, all the children are freed too.
#[doc(alias = "xmlFreePropList")]
pub unsafe extern "C" fn xml_free_prop_list(mut cur: XmlAttrPtr) {
    let mut next: XmlAttrPtr;
    if cur.is_null() {
        return;
    }
    while !cur.is_null() {
        next = (*cur).next;
        xml_free_prop(cur);
        cur = next;
    }
}

/// Free one attribute, all the content is freed too
#[doc(alias = "xmlFreeProp")]
pub unsafe extern "C" fn xml_free_prop(cur: XmlAttrPtr) {
    let mut dict: XmlDictPtr = null_mut();
    if cur.is_null() {
        return;
    }

    if !(*cur).doc.is_null() {
        dict = (*(*cur).doc).dict;
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    // && xmlDeregisterNodeDefaultValue.is_some()
    {
        xml_deregister_node_default_value(cur as _);
    }

    /* Check for ID removal -> leading to invalid references ! */
    if !(*cur).doc.is_null() && matches!((*cur).atype, Some(XmlAttributeType::XmlAttributeID)) {
        xml_remove_id((*cur).doc, cur);
    }
    if let Some(children) = (*cur).children() {
        xml_free_node_list(children.as_ptr());
    }
    DICT_FREE!(dict, (*cur).name);
    xml_free(cur as _);
}

/// This function tries to locate a namespace definition in a tree
/// ancestors, or create a new namespace definition node similar to
/// @ns trying to reuse the same prefix. However if the given prefix is
/// null (default namespace) or reused within the subtree defined by
/// @tree or on one of its ancestors then a new prefix is generated.
/// Returns the (new) namespace definition or null_mut() in case of error
#[doc(alias = "xmlNewReconciledNs")]
unsafe extern "C" fn xml_new_reconciled_ns(
    doc: XmlDocPtr,
    tree: XmlNodePtr,
    ns: XmlNsPtr,
) -> XmlNsPtr {
    let mut def: XmlNsPtr;
    let mut counter: i32 = 1;

    if tree.is_null() || !matches!((*tree).element_type(), XmlElementType::XmlElementNode) {
        return null_mut();
    }
    if ns.is_null() || !matches!((*ns).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    /*
     * Search an existing namespace definition inherited.
     */
    def = (*tree).search_ns_by_href(
        doc,
        CStr::from_ptr((*ns).href as *const i8)
            .to_string_lossy()
            .as_ref(),
    );
    if !def.is_null() {
        return def;
    }

    /*
     * Find a close prefix which is not already in use.
     * Let's strip namespace prefixes longer than 20 chars !
     */
    let prefix = if (*ns).prefix.is_null() {
        Cow::Borrowed("default")
    } else {
        Cow::Owned(format!(
            "{}",
            CStr::from_ptr((*ns).prefix as *const c_char).to_string_lossy()
        ))
    };

    def = (*tree).search_ns(doc, Some(&prefix));
    while !def.is_null() {
        if counter > 1000 {
            return null_mut();
        }
        let prefix = if (*ns).prefix.is_null() {
            format!("default{counter}")
        } else {
            format!(
                "{}{counter}",
                CStr::from_ptr((*ns).prefix as *const i8).to_string_lossy()
            )
        };
        counter += 1;
        def = (*tree).search_ns(doc, Some(&prefix));
    }

    /*
     * OK, now we are ready to create a new one.
     */
    def = xml_new_ns(tree, (*ns).href, prefix.as_ptr());
    def
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
    node: XmlNodePtr,
    doc: XmlDocPtr,
    parent: XmlNodePtr,
    extended: i32,
) -> XmlNodePtr {
    if node.is_null() {
        return null_mut();
    }
    match (*node).element_type() {
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
            return xml_copy_prop_internal(doc, parent, node as _) as _;
        }
        XmlElementType::XmlNamespaceDecl => {
            return xml_copy_namespace_list(node as _) as _;
        }

        XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
            #[cfg(feature = "libxml_tree")]
            {
                return xml_copy_doc(node as _, extended) as _;
            }
        }
        XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlDTDNode
        | XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl => {
            return null_mut();
        }
        _ => unreachable!(),
    }

    // Allocate a new node and fill the fields.
    let ret: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if ret.is_null() {
        xml_tree_err_memory("copying node");
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlNode::default());
    (*ret).typ = (*node).element_type();

    (*ret).doc = doc;
    (*ret).set_parent(NodePtr::from_ptr(parent));
    if (*node).name == XML_STRING_TEXT.as_ptr() as _ {
        (*ret).name = XML_STRING_TEXT.as_ptr() as _;
    } else if (*node).name == XML_STRING_TEXT_NOENC.as_ptr() as _ {
        (*ret).name = XML_STRING_TEXT_NOENC.as_ptr() as _;
    } else if (*node).name == XML_STRING_COMMENT.as_ptr() as _ {
        (*ret).name = XML_STRING_COMMENT.as_ptr() as _;
    } else if !(*node).name.is_null() {
        if !doc.is_null() && !(*doc).dict.is_null() {
            (*ret).name = xml_dict_lookup((*doc).dict, (*node).name, -1);
        } else {
            (*ret).name = xml_strdup((*node).name);
        }
    }
    if !matches!((*node).element_type(), XmlElementType::XmlElementNode)
        && !(*node).content.is_null()
        && !matches!((*node).element_type(), XmlElementType::XmlEntityRefNode)
        && !matches!((*node).element_type(), XmlElementType::XmlXIncludeEnd)
        && !matches!((*node).element_type(), XmlElementType::XmlXIncludeStart)
    {
        (*ret).content = xml_strdup((*node).content);
    } else if matches!((*node).element_type(), XmlElementType::XmlElementNode) {
        (*ret).line = (*node).line;
    }
    if !parent.is_null() {
        /*
         * this is a tricky part for the node register thing:
         * in case ret does get coalesced in xmlAddChild
         * the deregister-node callback is called; so we register ret now already
         */
        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        // && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(ret as _);
        }

        /*
         * Note that since (*ret).parent is already set, xmlAddChild will
         * return early and not actually insert the node. It will only
         * coalesce text nodes and unnecessarily call xmlSetTreeDoc.
         * Assuming that the subtree to be copied always has its text
         * nodes coalesced, the somewhat confusing call to xmlAddChild
         * could be removed.
         */
        let tmp: XmlNodePtr = (*parent).add_child(ret);
        /* node could have coalesced */
        if tmp != ret {
            return tmp;
        }
    }

    if extended == 0 {
        //  goto out;
        /* if parent != null_mut() we already registered the node above */
        if parent.is_null() && __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        // && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(ret as _);
        }

        return ret;
    }
    if matches!(
        (*node).element_type(),
        XmlElementType::XmlElementNode | XmlElementType::XmlXIncludeStart
    ) && !(*node).ns_def.is_null()
    {
        (*ret).ns_def = xml_copy_namespace_list((*node).ns_def);
    }

    if !(*node).ns.is_null() {
        let prefix = (*(*node).ns).prefix;
        let prefix =
            (!prefix.is_null()).then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy());
        let mut ns = (*ret).search_ns(doc, prefix.as_deref());
        if ns.is_null() {
            /*
             * Humm, we are copying an element whose namespace is defined
             * out of the new tree scope. Search it in the original tree
             * and add it at the top of the new tree
             */
            ns = (*node).search_ns((*node).doc, prefix.as_deref());
            if !ns.is_null() {
                let mut root: XmlNodePtr = ret;

                while let Some(parent) = (*root).parent() {
                    root = parent.as_ptr();
                }
                (*ret).ns = xml_new_ns(root, (*ns).href, (*ns).prefix);
            } else {
                (*ret).ns = xml_new_reconciled_ns(doc, ret, (*node).ns);
            }
        } else {
            /*
             * reference the existing namespace definition in our own tree.
             */
            (*ret).ns = ns;
        }
    }
    if (matches!((*node).element_type(), XmlElementType::XmlElementNode)
        || matches!((*node).element_type(), XmlElementType::XmlXIncludeStart))
        && !(*node).properties.is_null()
    {
        (*ret).properties = xml_copy_prop_list(ret, (*node).properties);
    }
    if matches!((*node).element_type(), XmlElementType::XmlEntityRefNode) {
        if doc.is_null() || (*node).doc != doc {
            // The copied node will go into a separate document, so
            // to avoid dangling references to the ENTITY_DECL node
            // we cannot keep the reference. Try to find it in the
            // target document.
            (*ret).set_children(NodePtr::from_ptr(
                xml_get_doc_entity(doc, &(*ret).name().unwrap()) as *mut XmlNode,
            ));
        } else {
            (*ret).set_children((*node).children());
        }
        (*ret).set_last((*ret).children());
    } else if let Some(children) = (*node).children().filter(|_| extended != 2) {
        let mut cur = children.as_ptr();
        let mut insert = ret;
        while !cur.is_null() {
            let copy: XmlNodePtr = xml_static_copy_node(cur, doc, insert, 2);
            if copy.is_null() {
                xml_free_node(ret);
                return null_mut();
            }

            /* Check for coalesced text nodes */
            if (*insert).last() != NodePtr::from_ptr(copy) {
                if let Some(mut last) = (*insert).last() {
                    (*copy).prev = Some(last);
                    last.next = NodePtr::from_ptr(copy);
                } else {
                    (*insert).set_children(NodePtr::from_ptr(copy));
                }
                (*insert).set_last(NodePtr::from_ptr(copy));
            }

            if let Some(children) = (*cur)
                .children()
                .filter(|_| !matches!((*cur).element_type(), XmlElementType::XmlEntityRefNode))
            {
                cur = children.as_ptr();
                insert = copy;
                continue;
            }

            loop {
                if let Some(next) = (*cur).next {
                    cur = next.as_ptr();
                    break;
                }

                cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                insert = (*insert).parent().map_or(null_mut(), |p| p.as_ptr());
                if cur == node {
                    cur = null_mut();
                    break;
                }
            }
        }
    }

    //  out:
    /* if parent != null_mut() we already registered the node above */
    if parent.is_null() && __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    // && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(ret as _);
    }

    ret
}

pub(crate) unsafe fn xml_static_copy_node_list(
    mut node: XmlNodePtr,
    doc: XmlDocPtr,
    parent: XmlNodePtr,
) -> XmlNodePtr {
    let mut ret: XmlNodePtr = null_mut();
    let mut p: XmlNodePtr = null_mut();
    let mut q: XmlNodePtr;

    while !node.is_null() {
        #[cfg(feature = "libxml_tree")]
        {
            if matches!((*node).element_type(), XmlElementType::XmlDTDNode) {
                if doc.is_null() {
                    node = (*node).next.map_or(null_mut(), |c| c.as_ptr());
                    continue;
                }
                if (*doc).int_subset.is_null() {
                    q = xml_copy_dtd(node as _) as _;
                    if q.is_null() {
                        // goto error;
                        xml_free_node_list(ret);
                        return null_mut();
                    }
                    (*q).doc = doc;
                    (*q).set_parent(NodePtr::from_ptr(parent));
                    (*doc).int_subset = q as _;
                    (*parent).add_child(q);
                } else {
                    q = (*doc).int_subset as _;
                    (*parent).add_child(q);
                }
            } else {
                q = xml_static_copy_node(node, doc, parent, 1);
            }
        }
        #[cfg(not(feature = "libxml_tree"))]
        {
            q = xml_static_copy_node(node, doc, parent, 1);
        }
        if q.is_null() {
            // goto error;
            xml_free_node_list(ret);
            return null_mut();
        }
        if ret.is_null() {
            (*q).prev = None;
            ret = q;
            p = q;
        } else if p != q {
            /* the test is required if xmlStaticCopyNode coalesced 2 text nodes */
            (*p).next = NodePtr::from_ptr(q);
            (*q).prev = NodePtr::from_ptr(p);
            p = q;
        }
        node = (*node).next.map_or(null_mut(), |c| c.as_ptr());
    }
    ret
    // error:
    //     xmlFreeNodeList(ret);
    //     return null_mut();
}

unsafe fn xml_copy_prop_internal(
    doc: XmlDocPtr,
    target: XmlNodePtr,
    cur: XmlAttrPtr,
) -> XmlAttrPtr {
    let ret: XmlAttrPtr;

    if cur.is_null() {
        return null_mut();
    }
    if !target.is_null() && !matches!((*target).element_type(), XmlElementType::XmlElementNode) {
        return null_mut();
    }
    if !target.is_null() {
        ret = xml_new_doc_prop((*target).doc, (*cur).name, null_mut());
    } else if !doc.is_null() {
        ret = xml_new_doc_prop(doc, (*cur).name, null_mut());
    } else if let Some(parent) = (*cur).parent() {
        ret = xml_new_doc_prop(parent.doc, (*cur).name, null_mut());
    } else if let Some(children) = (*cur).children() {
        ret = xml_new_doc_prop(children.doc, (*cur).name, null_mut());
    } else {
        ret = xml_new_doc_prop(null_mut(), (*cur).name, null_mut());
    }
    if ret.is_null() {
        return null_mut();
    }
    (*ret).parent = NodePtr::from_ptr(target);

    if !(*cur).ns.is_null() && !target.is_null() {
        let prefix = (*(*cur).ns).prefix;
        let prefix =
            (!prefix.is_null()).then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy());
        let mut ns = (*target).search_ns((*target).doc, prefix.as_deref());
        if ns.is_null() {
            // Humm, we are copying an element whose namespace is defined
            // out of the new tree scope. Search it in the original tree
            // and add it at the top of the new tree
            ns = (*cur)
                .parent
                .unwrap()
                .search_ns((*cur).doc, prefix.as_deref());
            if !ns.is_null() {
                let mut root: XmlNodePtr = target;
                let mut pred: XmlNodePtr = null_mut();

                while let Some(parent) = (*root).parent() {
                    pred = root;
                    root = parent.as_ptr();
                }
                if root == (*target).doc as _ {
                    // correct possibly cycling above the document elt
                    root = pred;
                }
                (*ret).ns = xml_new_ns(root, (*ns).href, (*ns).prefix);
            }
        } else {
            // we have to find something appropriate here since
            // we can't be sure, that the namespace we found is identified
            // by the prefix
            if xml_str_equal((*ns).href, (*(*cur).ns).href) {
                // this is the nice case
                (*ret).ns = ns;
            } else {
                // we are in trouble: we need a new reconciled namespace.
                // This is expensive
                (*ret).ns = xml_new_reconciled_ns((*target).doc, target, (*cur).ns);
            }
        }
    } else {
        (*ret).ns = null_mut();
    }

    if let Some(children) = (*cur).children() {
        (*ret).children = NodePtr::from_ptr(xml_static_copy_node_list(
            children.as_ptr(),
            (*ret).doc,
            ret as _,
        ));
        (*ret).last = None;
        let mut tmp = (*ret).children;
        while let Some(now) = tmp {
            /* (*tmp).parent = ret; */
            if now.next.is_none() {
                (*ret).last = Some(now);
            }
            tmp = now.next;
        }
    }
    // Try to handle IDs
    if !target.is_null()
        && !cur.is_null()
        && !(*target).doc.is_null()
        && !(*cur).doc.is_null()
        && !(*(*cur).doc).ids.is_null()
        && (*cur)
            .parent
            .filter(|p| xml_is_id((*cur).doc, p.as_ptr(), cur) != 0)
            .is_some()
    {
        let children = (*cur).children;
        if let Some(id) = children.and_then(|c| c.get_string((*cur).doc, 1)) {
            let id = CString::new(id).unwrap();
            xml_add_id(null_mut(), (*target).doc, id.as_ptr() as *const u8, ret);
        }
    }
    ret
}

/// Do a copy of the attribute.
///
/// Returns: a new #xmlAttrPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyProp")]
pub unsafe fn xml_copy_prop(target: XmlNodePtr, cur: XmlAttrPtr) -> XmlAttrPtr {
    xml_copy_prop_internal(null_mut(), target, cur)
}

/// Do a copy of an attribute list.
///
/// Returns: a new #xmlAttrPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyPropList")]
pub unsafe fn xml_copy_prop_list(target: XmlNodePtr, mut cur: XmlAttrPtr) -> XmlAttrPtr {
    let mut ret: XmlAttrPtr = null_mut();
    let mut p: XmlAttrPtr = null_mut();
    let mut q: XmlAttrPtr;

    if !target.is_null() && !matches!((*target).element_type(), XmlElementType::XmlElementNode) {
        return null_mut();
    }
    while !cur.is_null() {
        q = xml_copy_prop(target, cur);
        if q.is_null() {
            xml_free_prop_list(ret);
            return null_mut();
        }
        if p.is_null() {
            ret = q;
            p = q;
        } else {
            (*p).next = q;
            (*q).prev = p;
            p = q;
        }
        cur = (*cur).next;
    }
    ret
}

/// Do a copy of the dtd.
///
/// Returns: a new #xmlDtdPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyDtd")]
#[cfg(feature = "libxml_tree")]
pub unsafe fn xml_copy_dtd(dtd: XmlDtdPtr) -> XmlDtdPtr {
    use std::ffi::CString;

    use crate::libxml::{
        entities::xml_copy_entities_table,
        valid::{
            xml_copy_attribute_table, xml_copy_element_table, xml_copy_notation_table,
            xml_get_dtd_qelement_desc,
        },
    };

    let mut p: XmlNodePtr = null_mut();
    let mut q: XmlNodePtr;

    if dtd.is_null() {
        return null_mut();
    }
    let ret: XmlDtdPtr = xml_new_dtd(
        null_mut(),
        (*dtd).name().as_deref(),
        (*dtd).external_id.as_deref(),
        (*dtd).system_id.as_deref(),
    );
    if ret.is_null() {
        return null_mut();
    }
    if let Some(entities) = (*dtd).entities {
        (*ret).entities = xml_copy_entities_table(entities);
    }
    if !(*dtd).notations.is_null() {
        (*ret).notations = xml_copy_notation_table((*dtd).notations as XmlNotationTablePtr) as _;
    }
    if !(*dtd).elements.is_null() {
        (*ret).elements = xml_copy_element_table((*dtd).elements as XmlElementTablePtr) as _;
    }
    if let Some(table) = (*dtd).attributes {
        (*ret).attributes = xml_copy_attribute_table(table);
    }
    if let Some(pentities) = (*dtd).pentities {
        (*ret).pentities = xml_copy_entities_table(pentities);
    }

    let mut cur = (*dtd).children.map_or(null_mut(), |c| c.as_ptr());
    while !cur.is_null() {
        q = null_mut();

        if matches!((*cur).element_type(), XmlElementType::XmlEntityDecl) {
            let tmp: XmlEntityPtr = cur as _;
            match (*tmp).etype {
                XmlEntityType::XmlInternalGeneralEntity
                | XmlEntityType::XmlExternalGeneralParsedEntity
                | XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                    q = (*ret).get_entity((*tmp).name.load(Ordering::Relaxed)) as _;
                }
                XmlEntityType::XmlInternalParameterEntity
                | XmlEntityType::XmlExternalParameterEntity => {
                    q = (*ret).get_parameter_entity((*tmp).name.load(Ordering::Relaxed)) as _;
                }
                XmlEntityType::XmlInternalPredefinedEntity => {}
                _ => unreachable!(),
            }
        } else if matches!((*cur).element_type(), XmlElementType::XmlElementDecl) {
            let tmp: XmlElementPtr = cur as _;
            let prefix = (*tmp).prefix.as_deref().map(|p| CString::new(p).unwrap());
            let name = (*tmp).name().map(|n| CString::new(n.as_ref()).unwrap());
            q = xml_get_dtd_qelement_desc(
                ret,
                name.as_ref().map_or(null(), |n| n.as_ptr() as *const u8),
                prefix
                    .as_ref()
                    .map_or(null_mut(), |p| p.as_ptr() as *const u8),
            ) as _;
        } else if matches!((*cur).element_type(), XmlElementType::XmlAttributeDecl) {
            let tmp: XmlAttributePtr = cur as _;
            q = (*ret).get_qattr_desc(
                (*tmp).elem.as_deref().unwrap(),
                (*tmp).name().as_deref().unwrap(),
                (*tmp).prefix.as_deref(),
            ) as _;
        } else if matches!((*cur).element_type(), XmlElementType::XmlCommentNode) {
            q = xml_copy_node(cur, 0);
        }

        if q.is_null() {
            cur = (*cur).next.map_or(null_mut(), |c| c.as_ptr());
            continue;
        }

        if p.is_null() {
            (*ret).children = NodePtr::from_ptr(q);
        } else {
            (*p).next = NodePtr::from_ptr(q);
        }

        (*q).prev = NodePtr::from_ptr(p);
        (*q).set_parent(NodePtr::from_ptr(ret as *mut XmlNode));
        (*q).next = None;
        (*ret).last = NodePtr::from_ptr(q);
        p = q;
        cur = (*cur).next.map_or(null_mut(), |c| c.as_ptr());
    }

    ret
}

/// Do a copy of the document info. If recursive, the content tree will
/// be copied too as well as DTD, namespaces and entities.
///
/// Returns: a new #xmlDocPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyDoc")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub unsafe extern "C" fn xml_copy_doc(doc: XmlDocPtr, recursive: i32) -> XmlDocPtr {
    use crate::libxml::globals::xml_mem_strdup;

    if doc.is_null() {
        return null_mut();
    }
    let ret: XmlDocPtr = xml_new_doc((*doc).version.as_deref());
    if ret.is_null() {
        return null_mut();
    }
    (*ret).typ = (*doc).typ;
    if !(*doc).name.is_null() {
        (*ret).name = xml_mem_strdup((*doc).name as _) as _;
    }
    (*ret).encoding = (*doc).encoding.clone();
    if let Some(url) = (*doc).url.as_deref() {
        (*ret).url = Some(url.to_owned());
    }
    (*ret).charset = (*doc).charset;
    (*ret).compression = (*doc).compression;
    (*ret).standalone = (*doc).standalone;
    if recursive == 0 {
        return ret;
    }

    (*ret).last = None;
    (*ret).children = None;
    #[cfg(feature = "libxml_tree")]
    {
        if !(*doc).int_subset.is_null() {
            (*ret).int_subset = xml_copy_dtd((*doc).int_subset);
            if (*ret).int_subset.is_null() {
                xml_free_doc(ret);
                return null_mut();
            }
            (*((*ret).int_subset as *mut XmlNode)).set_doc(ret);
            (*(*ret).int_subset).parent = ret;
        }
    }
    if !(*doc).old_ns.is_null() {
        (*ret).old_ns = xml_copy_namespace_list((*doc).old_ns);
    }
    if let Some(children) = (*doc).children {
        (*ret).children =
            NodePtr::from_ptr(xml_static_copy_node_list(children.as_ptr(), ret, ret as _));
        (*ret).last = None;
        let mut tmp = (*ret).children;
        while let Some(now) = tmp {
            if now.next.is_none() {
                (*ret).last = Some(now);
            }
            tmp = now.next;
        }
    }
    ret
}

macro_rules! UPDATE_LAST_CHILD_AND_PARENT {
    ($n:expr) => {
        if !$n.is_null() {
            if let Some(mut ulccur) = (*$n).children() {
                while let Some(next) = ulccur.next {
                    ulccur.set_parent(NodePtr::from_ptr($n));
                    ulccur = next;
                }
                (*ulccur).set_parent(NodePtr::from_ptr($n));
                (*$n).set_last(Some(ulccur));
            } else {
                (*$n).set_last(None);
            }
        }
    };
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
pub unsafe extern "C" fn xml_new_doc_node(
    doc: XmlDocPtr,
    ns: XmlNsPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> XmlNodePtr {
    let cur = if !doc.is_null() && !(*doc).dict.is_null() {
        xml_new_node_eat_name(ns, xml_dict_lookup((*doc).dict, name, -1) as _)
    } else {
        xml_new_node(ns, name)
    };
    if !cur.is_null() {
        (*cur).doc = doc;
        if !content.is_null() {
            (*cur).set_children(
                (!doc.is_null())
                    .then(|| NodePtr::from_ptr((*doc).get_node_list(content)))
                    .flatten(),
            );
            UPDATE_LAST_CHILD_AND_PARENT!(cur)
        }
    }

    cur
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
pub unsafe extern "C" fn xml_new_doc_node_eat_name(
    doc: XmlDocPtr,
    ns: XmlNsPtr,
    name: *mut XmlChar,
    content: *const XmlChar,
) -> XmlNodePtr {
    let cur: XmlNodePtr = xml_new_node_eat_name(ns, name);
    if !cur.is_null() {
        (*cur).doc = doc;
        if !content.is_null() {
            (*cur).set_children(
                (!doc.is_null())
                    .then(|| NodePtr::from_ptr((*doc).get_node_list(content)))
                    .flatten(),
            );
            UPDATE_LAST_CHILD_AND_PARENT!(cur)
        }
    } else {
        /* if name don't come from the doc dictionary free it here */
        if !name.is_null()
            && (doc.is_null() || (*doc).dict.is_null() || xml_dict_owns((*doc).dict, name) == 0)
        {
            xml_free(name as _);
        }
    }
    cur
}

/// Creation of a new node element. @ns is optional (null_mut()).
///
/// Use of this function is DISCOURAGED in favor of xmlNewDocNode.
///
/// Returns a pointer to the new node object. Uses xml_strdup() to make copy of @name.
#[doc(alias = "xmlNewNode")]
pub unsafe extern "C" fn xml_new_node(ns: XmlNsPtr, name: *const XmlChar) -> XmlNodePtr {
    if name.is_null() {
        return null_mut();
    }

    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building node");
        return null_mut();
    }
    std::ptr::write(&mut *cur, XmlNode::default());
    (*cur).typ = XmlElementType::XmlElementNode;

    (*cur).name = xml_strdup(name);
    (*cur).ns = ns;

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/// Creation of a new node element. @ns is optional (null_mut()).
///
/// Use of this function is DISCOURAGED in favor of xmlNewDocNodeEatName.
///
/// Returns a pointer to the new node object, with pointer @name as
/// new node's name. Use xmlNewNode() if a copy of @name string is
/// is needed as new node's name.
#[doc(alias = "xmlNewNodeEatName")]
pub unsafe extern "C" fn xml_new_node_eat_name(ns: XmlNsPtr, name: *mut XmlChar) -> XmlNodePtr {
    if name.is_null() {
        return null_mut();
    }

    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building node");
        /* we can't check here that name comes from the doc dictionary */
        return null_mut();
    }
    std::ptr::write(&mut *cur, XmlNode::default());
    (*cur).typ = XmlElementType::XmlElementNode;

    (*cur).name = name;
    (*cur).ns = ns;

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/// Creation of a new child element, added at the end of @parent children list.
/// @ns and @content parameters are optional (null_mut()). If @ns is null_mut(), the newly
/// created element inherits the namespace of @parent. If @content is non null_mut(),
/// a child list containing the TEXTs and ENTITY_REFs node will be created.
/// NOTE: @content is supposed to be a piece of XML CDATA, so it allows entity
///       references. XML special chars must be escaped first by using
///       xmlEncodeEntitiesReentrant(), or xmlNewTextChild() should be used.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewChild")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub unsafe extern "C" fn xml_new_child(
    parent: XmlNodePtr,
    ns: XmlNsPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> XmlNodePtr {
    let cur: XmlNodePtr;
    let prev: XmlNodePtr;

    if parent.is_null() {
        return null_mut();
    }

    if name.is_null() {
        return null_mut();
    }

    // Allocate a new node
    if matches!((*parent).element_type(), XmlElementType::XmlElementNode) {
        if ns.is_null() {
            cur = xml_new_doc_node((*parent).doc, (*parent).ns, name, content);
        } else {
            cur = xml_new_doc_node((*parent).doc, ns, name, content);
        }
    } else if (matches!((*parent).element_type(), XmlElementType::XmlDocumentNode)
        || matches!(
            (*parent).element_type(),
            XmlElementType::XmlHTMLDocumentNode
        ))
    {
        if ns.is_null() {
            cur = xml_new_doc_node(parent as _, null_mut(), name, content);
        } else {
            cur = xml_new_doc_node(parent as _, ns, name, content);
        }
    } else if matches!(
        (*parent).element_type(),
        XmlElementType::XmlDocumentFragNode
    ) {
        cur = xml_new_doc_node((*parent).doc, ns, name, content);
    } else {
        return null_mut();
    }
    if cur.is_null() {
        return null_mut();
    }

    // add the new element at the end of the children list.
    (*cur).typ = XmlElementType::XmlElementNode;
    (*cur).set_parent(NodePtr::from_ptr(parent));
    (*cur).doc = (*parent).doc;
    if (*parent).children().is_none() {
        (*parent).set_children(NodePtr::from_ptr(cur));
        (*parent).set_last(NodePtr::from_ptr(cur));
    } else {
        prev = (*parent).last().map_or(null_mut(), |l| l.as_ptr());
        (*prev).next = NodePtr::from_ptr(cur);
        (*cur).prev = NodePtr::from_ptr(prev);
        (*parent).set_last(NodePtr::from_ptr(cur));
    }

    cur
}

/// Creation of a new text node within a document.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocText")]
pub unsafe extern "C" fn xml_new_doc_text(
    doc: *const XmlDoc,
    content: *const XmlChar,
) -> XmlNodePtr {
    let cur: XmlNodePtr = xml_new_text(content);
    if !cur.is_null() {
        (*cur).doc = doc as _;
    }
    cur
}

/// Creation of a new text node.
///
/// Use of this function is DISCOURAGED in favor of xmlNewDocText.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewText")]
pub unsafe extern "C" fn xml_new_text(content: *const XmlChar) -> XmlNodePtr {
    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building text");
        return null_mut();
    }
    std::ptr::write(&mut *cur, XmlNode::default());
    (*cur).typ = XmlElementType::XmlTextNode;

    (*cur).name = XML_STRING_TEXT.as_ptr() as _;
    if !content.is_null() {
        (*cur).content = xml_strdup(content);
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/// Creation of a processing instruction element.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocPI")]
pub unsafe extern "C" fn xml_new_doc_pi(
    doc: XmlDocPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> XmlNodePtr {
    if name.is_null() {
        return null_mut();
    }

    // Allocate a new node and fill the fields.
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building PI");
        return null_mut();
    }
    std::ptr::write(&mut *cur, XmlNode::default());
    (*cur).typ = XmlElementType::XmlPINode;

    if !doc.is_null() && !(*doc).dict.is_null() {
        (*cur).name = xml_dict_lookup((*doc).dict, name, -1);
    } else {
        (*cur).name = xml_strdup(name);
    }
    if !content.is_null() {
        (*cur).content = xml_strdup(content);
    }
    (*cur).doc = doc;

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/// Creation of a processing instruction element.
///
/// Use of this function is DISCOURAGED in favor of xmlNewDocPI.
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewPI")]
pub unsafe extern "C" fn xml_new_pi(name: *const XmlChar, content: *const XmlChar) -> XmlNodePtr {
    xml_new_doc_pi(null_mut(), name, content)
}

/// Creation of a new text node with an extra content length parameter. The
/// text node pertain to a given document.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocTextLen")]
pub unsafe extern "C" fn xml_new_doc_text_len(
    doc: XmlDocPtr,
    content: *const XmlChar,
    len: i32,
) -> XmlNodePtr {
    let cur: XmlNodePtr = xml_new_text_len(content, len);
    if !cur.is_null() {
        (*cur).doc = doc;
    }
    cur
}

/// Use of this function is DISCOURAGED in favor of xmlNewDocTextLen.
///
/// Creation of a new text node with an extra parameter for the content's length
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewTextLen")]
pub unsafe extern "C" fn xml_new_text_len(content: *const XmlChar, len: i32) -> XmlNodePtr {
    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building text");
        return null_mut();
    }
    std::ptr::write(&mut *cur, XmlNode::default());
    (*cur).typ = XmlElementType::XmlTextNode;

    (*cur).name = XML_STRING_TEXT.as_ptr() as _;
    if !content.is_null() {
        (*cur).content = xml_strndup(content, len);
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/// Creation of a new node containing a comment within a document.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocComment")]
pub unsafe extern "C" fn xml_new_doc_comment(
    doc: XmlDocPtr,
    content: *const XmlChar,
) -> XmlNodePtr {
    let cur: XmlNodePtr = xml_new_comment(content);
    if !cur.is_null() {
        (*cur).doc = doc;
    }
    cur
}

/// Use of this function is DISCOURAGED in favor of xmlNewDocComment.
///
/// Creation of a new node containing a comment.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewComment")]
pub unsafe extern "C" fn xml_new_comment(content: *const XmlChar) -> XmlNodePtr {
    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building comment");
        return null_mut();
    }
    std::ptr::write(&mut *cur, XmlNode::default());
    (*cur).typ = XmlElementType::XmlCommentNode;

    (*cur).name = XML_STRING_COMMENT.as_ptr() as _;
    if !content.is_null() {
        (*cur).content = xml_strdup(content);
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/// Creation of a new node containing a CDATA block.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewCDataBlock")]
pub unsafe extern "C" fn xml_new_cdata_block(
    doc: XmlDocPtr,
    content: *const XmlChar,
    len: i32,
) -> XmlNodePtr {
    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building CDATA");
        return null_mut();
    }
    std::ptr::write(&mut *cur, XmlNode::default());
    (*cur).typ = XmlElementType::XmlCDATASectionNode;
    (*cur).doc = doc;

    if !content.is_null() {
        (*cur).content = xml_strndup(content, len);
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/// Creation of a new character reference node.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewCharRef")]
pub unsafe fn xml_new_char_ref(doc: XmlDocPtr, name: &str) -> XmlNodePtr {
    // Allocate a new node and fill the fields.
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building character reference");
        return null_mut();
    }
    std::ptr::write(&mut *cur, XmlNode::default());
    (*cur).typ = XmlElementType::XmlEntityRefNode;

    (*cur).doc = doc;
    if let Some(name) = name.strip_prefix('&') {
        let len = name.len();
        if let Some(name) = name.strip_suffix(';') {
            (*cur).name = xml_strndup(name.as_ptr(), len as i32 - 1);
        } else {
            (*cur).name = xml_strndup(name.as_ptr(), len as i32);
        }
    } else {
        let name = CString::new(name).unwrap();
        (*cur).name = xml_strdup(name.as_ptr() as *const u8);
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/// Creation of a new reference node.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewReference")]
pub unsafe fn xml_new_reference(doc: *const XmlDoc, name: &str) -> XmlNodePtr {
    // Allocate a new node and fill the fields.
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building reference");
        return null_mut();
    }
    std::ptr::write(&mut *cur, XmlNode::default());
    (*cur).typ = XmlElementType::XmlEntityRefNode;

    (*cur).doc = doc as _;
    if let Some(name) = name.strip_prefix('&') {
        let len = name.len();
        if let Some(name) = name.strip_suffix(';') {
            (*cur).name = xml_strndup(name.as_ptr(), len as i32 - 1);
        } else {
            (*cur).name = xml_strndup(name.as_ptr(), len as i32);
        }
    } else {
        let name = CString::new(name).unwrap();
        (*cur).name = xml_strdup(name.as_ptr() as *const u8);
    }

    let ent: XmlEntityPtr = xml_get_doc_entity(doc, &(*cur).name().unwrap());
    if !ent.is_null() {
        (*cur).content = (*ent).content.load(Ordering::Acquire);
        // The parent pointer in entity is a DTD pointer and thus is NOT
        // updated.  Not sure if this is 100% correct.
        //  -George
        (*cur).set_children(NodePtr::from_ptr(ent as *mut XmlNode));
        (*cur).set_last(NodePtr::from_ptr(ent as *mut XmlNode));
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/// Do a copy of the node.
///
/// Returns: a new #XmlNodePtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyNode")]
pub unsafe extern "C" fn xml_copy_node(node: XmlNodePtr, extended: i32) -> XmlNodePtr {
    let ret: XmlNodePtr = xml_static_copy_node(node, null_mut(), null_mut(), extended);
    ret
}

/// Do a copy of the node to a given document.
///
/// Returns: a new #XmlNodePtr, or null_mut() in case of error.
#[doc(alias = "xmlDocCopyNode")]
pub unsafe extern "C" fn xml_doc_copy_node(
    node: XmlNodePtr,
    doc: XmlDocPtr,
    extended: i32,
) -> XmlNodePtr {
    let ret: XmlNodePtr = xml_static_copy_node(node, doc, null_mut(), extended);
    ret
}

/// Do a recursive copy of the node list.
///
/// Returns: a new #XmlNodePtr, or null_mut() in case of error.
#[doc(alias = "xmlDocCopyNodeList")]
pub unsafe extern "C" fn xml_doc_copy_node_list(doc: XmlDocPtr, node: XmlNodePtr) -> XmlNodePtr {
    let ret: XmlNodePtr = xml_static_copy_node_list(node, doc, null_mut());
    ret
}

/// Do a recursive copy of the node list.
/// Use xmlDocCopyNodeList() if possible to ensure string interning.
///
/// Returns: a new #XmlNodePtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyNodeList")]
pub unsafe extern "C" fn xml_copy_node_list(node: XmlNodePtr) -> XmlNodePtr {
    let ret: XmlNodePtr = xml_static_copy_node_list(node, null_mut(), null_mut());
    ret
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
pub unsafe extern "C" fn xml_new_text_child(
    parent: XmlNodePtr,
    ns: XmlNsPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> XmlNodePtr {
    let cur: XmlNodePtr;
    let prev: XmlNodePtr;

    if parent.is_null() {
        return null_mut();
    }

    if name.is_null() {
        return null_mut();
    }

    /*
     * Allocate a new node
     */
    if matches!((*parent).element_type(), XmlElementType::XmlElementNode) {
        if ns.is_null() {
            cur = xml_new_doc_raw_node((*parent).doc, (*parent).ns, name, content);
        } else {
            cur = xml_new_doc_raw_node((*parent).doc, ns, name, content);
        }
    } else if (matches!((*parent).element_type(), XmlElementType::XmlDocumentNode)
        || matches!(
            (*parent).element_type(),
            XmlElementType::XmlHTMLDocumentNode
        ))
    {
        if ns.is_null() {
            cur = xml_new_doc_raw_node(parent as _, null_mut(), name, content);
        } else {
            cur = xml_new_doc_raw_node(parent as _, ns, name, content);
        }
    } else if matches!(
        (*parent).element_type(),
        XmlElementType::XmlDocumentFragNode
    ) {
        cur = xml_new_doc_raw_node((*parent).doc, ns, name, content);
    } else {
        return null_mut();
    }
    if cur.is_null() {
        return null_mut();
    }

    // add the new element at the end of the children list.
    (*cur).typ = XmlElementType::XmlElementNode;
    (*cur).set_parent(NodePtr::from_ptr(parent));
    (*cur).doc = (*parent).doc;
    if (*parent).children().is_none() {
        (*parent).set_children(NodePtr::from_ptr(cur));
        (*parent).set_last(NodePtr::from_ptr(cur));
    } else {
        prev = (*parent).last().map_or(null_mut(), |l| l.as_ptr());
        (*prev).next = NodePtr::from_ptr(cur);
        (*cur).prev = NodePtr::from_ptr(prev);
        (*parent).set_last(NodePtr::from_ptr(cur));
    }

    cur
}

/// Creation of a new node element within a document. @ns and @content
/// are optional (null_mut()).
///
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocRawNode")]
#[cfg(feature = "libxml_tree")]
pub unsafe extern "C" fn xml_new_doc_raw_node(
    doc: XmlDocPtr,
    ns: XmlNsPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> XmlNodePtr {
    let cur: XmlNodePtr = xml_new_doc_node(doc, ns, name, null_mut());
    if !cur.is_null() {
        (*cur).doc = doc;
        if !content.is_null() {
            (*cur).set_children(NodePtr::from_ptr(xml_new_doc_text(doc, content)));
            UPDATE_LAST_CHILD_AND_PARENT!(cur)
        }
    }
    cur
}

/// Creation of a new Fragment node.
/// Returns a pointer to the new node object.
#[doc(alias = "xmlNewDocFragment")]
#[cfg(feature = "libxml_tree")]
pub unsafe extern "C" fn xml_new_doc_fragment(doc: XmlDocPtr) -> XmlNodePtr {
    // Allocate a new DocumentFragment node and fill the fields.
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building fragment");
        return null_mut();
    }
    std::ptr::write(&mut *cur, XmlNode::default());
    (*cur).typ = XmlElementType::XmlDocumentFragNode;

    (*cur).doc = doc;

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
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
pub unsafe extern "C" fn xml_replace_node(old: XmlNodePtr, cur: XmlNodePtr) -> XmlNodePtr {
    if old == cur {
        return null_mut();
    }
    if old.is_null()
        || matches!((*old).element_type(), XmlElementType::XmlNamespaceDecl)
        || (*old).parent().is_none()
    {
        return null_mut();
    }
    if cur.is_null() || matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
        (*old).unlink();
        return old;
    }
    if cur == old {
        return old;
    }
    if (matches!((*old).element_type(), XmlElementType::XmlAttributeNode)
        && !matches!((*cur).element_type(), XmlElementType::XmlAttributeNode))
    {
        return old;
    }
    if (matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
        && !matches!((*old).element_type(), XmlElementType::XmlAttributeNode))
    {
        return old;
    }
    (*cur).unlink();
    (*cur).set_doc((*old).doc);
    (*cur).set_parent((*old).parent());
    (*cur).next = (*old).next;
    if let Some(mut next) = (*cur).next {
        next.prev = NodePtr::from_ptr(cur);
    }
    (*cur).prev = (*old).prev;
    if let Some(mut prev) = (*cur).prev {
        prev.next = NodePtr::from_ptr(cur);
    }
    if let Some(mut parent) = (*cur).parent() {
        if matches!((*cur).element_type(), XmlElementType::XmlAttributeNode) {
            if parent.properties == old as _ {
                parent.properties = cur as _;
            }
        } else {
            if parent.children() == NodePtr::from_ptr(old) {
                parent.set_children(NodePtr::from_ptr(cur));
            }
            if parent.last() == NodePtr::from_ptr(old) {
                parent.set_last(NodePtr::from_ptr(cur));
            }
        }
    }
    (*old).next = None;
    (*old).prev = None;
    (*old).set_parent(None);
    old
}

/// Merge two text nodes into one
/// Returns the first text node augmented
#[doc(alias = "xmlTextMerge")]
pub unsafe extern "C" fn xml_text_merge(first: XmlNodePtr, second: XmlNodePtr) -> XmlNodePtr {
    if first.is_null() {
        return second;
    }
    if second.is_null() {
        return first;
    }
    if !matches!((*first).element_type(), XmlElementType::XmlTextNode) {
        return first;
    }
    if !matches!((*second).element_type(), XmlElementType::XmlTextNode) {
        return first;
    }
    if (*second).name != (*first).name {
        return first;
    }
    (*first).add_content((*second).content);
    (*second).unlink();
    xml_free_node(second);
    first
}

/// Concat the given string at the end of the existing node content
///
/// Returns -1 in case of error, 0 otherwise
#[doc(alias = "xmlTextConcat")]
pub unsafe extern "C" fn xml_text_concat(
    node: XmlNodePtr,
    content: *const XmlChar,
    len: i32,
) -> i32 {
    if node.is_null() {
        return -1;
    }

    if !matches!((*node).element_type(), XmlElementType::XmlTextNode)
        && !matches!((*node).element_type(), XmlElementType::XmlCDATASectionNode)
        && !matches!((*node).element_type(), XmlElementType::XmlCommentNode)
        && !matches!((*node).element_type(), XmlElementType::XmlPINode)
    {
        return -1;
    }
    /* need to check if content is currently in the dictionary */
    if (*node).content == addr_of_mut!((*node).properties) as _
        || (!(*node).doc.is_null()
            && !(*(*node).doc).dict.is_null()
            && xml_dict_owns((*(*node).doc).dict, (*node).content) != 0)
    {
        (*node).content = xml_strncat_new((*node).content, content, len);
    } else {
        (*node).content = xml_strncat((*node).content, content, len);
    }
    (*node).properties = null_mut();
    if (*node).content.is_null() {
        return -1;
    }
    0
}

/// Free a node and all its siblings, this is a recursive behaviour, all
/// the children are freed too.
#[doc(alias = "xmlFreeNodeList")]
pub unsafe extern "C" fn xml_free_node_list(mut cur: XmlNodePtr) {
    let mut parent: XmlNodePtr;
    let mut dict: XmlDictPtr = null_mut();
    let mut depth: usize = 0;

    if cur.is_null() {
        return;
    }
    if matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
        xml_free_ns_list(cur as _);
        return;
    }
    if !(*cur).doc.is_null() {
        dict = (*(*cur).doc).dict;
    }
    loop {
        while let Some(children) = (*cur).children().filter(|_| {
            !matches!(
                (*cur).element_type(),
                XmlElementType::XmlDocumentNode
                    | XmlElementType::XmlHTMLDocumentNode
                    | XmlElementType::XmlDTDNode
                    | XmlElementType::XmlEntityRefNode
            )
        }) {
            cur = children.as_ptr();
            depth += 1;
        }

        let next = (*cur).next.map_or(null_mut(), |c| c.as_ptr());
        parent = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        if matches!((*cur).element_type(), XmlElementType::XmlDocumentNode)
            || matches!((*cur).element_type(), XmlElementType::XmlHTMLDocumentNode)
        {
            xml_free_doc(cur as _);
        } else if !matches!((*cur).element_type(), XmlElementType::XmlDTDNode) {
            if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
            // && xmlDeregisterNodeDefaultValue.is_some()
            {
                xml_deregister_node_default_value(cur as _);
            }

            if (matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                || matches!((*cur).element_type(), XmlElementType::XmlXIncludeStart)
                || matches!((*cur).element_type(), XmlElementType::XmlXIncludeEnd))
                && !(*cur).properties.is_null()
            {
                xml_free_prop_list((*cur).properties);
            }
            if !matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                && !matches!((*cur).element_type(), XmlElementType::XmlXIncludeStart)
                && !matches!((*cur).element_type(), XmlElementType::XmlXIncludeEnd)
                && !matches!((*cur).element_type(), XmlElementType::XmlEntityRefNode)
                && ((*cur).content != addr_of_mut!((*cur).properties) as _)
            {
                DICT_FREE!(dict, (*cur).content)
            }
            if (matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                || matches!((*cur).element_type(), XmlElementType::XmlXIncludeStart)
                || matches!((*cur).element_type(), XmlElementType::XmlXIncludeEnd))
                && !(*cur).ns_def.is_null()
            {
                xml_free_ns_list((*cur).ns_def);
            }

            // When a node is a text node or a comment, it uses a global static
            // variable for the name of the node.
            // Otherwise the node name might come from the document's
            // dictionary
            if !(*cur).name.is_null()
                && !matches!((*cur).element_type(), XmlElementType::XmlTextNode)
                && !matches!((*cur).element_type(), XmlElementType::XmlCommentNode)
            {
                DICT_FREE!(dict, (*cur).name)
            }
            xml_free(cur as _);
        }

        if !next.is_null() {
            cur = next;
        } else {
            if depth == 0 || parent.is_null() {
                break;
            }
            depth -= 1;
            cur = parent;
            (*cur).set_children(None);
        }
    }
}

/// Free a node, this is a recursive behaviour, all the children are freed too.
/// This doesn't unlink the child from the list, use xmlUnlinkNode() first.
#[doc(alias = "xmlFreeNode")]
pub unsafe extern "C" fn xml_free_node(cur: XmlNodePtr) {
    let mut dict: XmlDictPtr = null_mut();

    if cur.is_null() {
        return;
    }

    /* use xmlFreeDtd for DTD nodes */
    if matches!((*cur).element_type(), XmlElementType::XmlDTDNode) {
        xml_free_dtd(cur as _);
        return;
    }
    if matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
        xml_free_ns(cur as _);
        return;
    }
    if matches!((*cur).element_type(), XmlElementType::XmlAttributeNode) {
        xml_free_prop(cur as _);
        return;
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    // && xmlDeregisterNodeDefaultValue.is_some()
    {
        xml_deregister_node_default_value(cur as _);
    }

    if !(*cur).doc.is_null() {
        dict = (*(*cur).doc).dict;
    }

    if matches!((*cur).element_type(), XmlElementType::XmlEntityDecl) {
        let ent: XmlEntityPtr = cur as _;
        DICT_FREE!(dict, (*ent).system_id.load(Ordering::Relaxed));
        (*ent).system_id.store(null_mut(), Ordering::Relaxed);
        DICT_FREE!(dict, (*ent).external_id.load(Ordering::Relaxed));
        (*ent).external_id.store(null_mut(), Ordering::Relaxed);
    }
    if let Some(children) = (*cur)
        .children()
        .filter(|_| !matches!((*cur).element_type(), XmlElementType::XmlEntityRefNode))
    {
        xml_free_node_list(children.as_ptr());
    }

    if matches!(
        (*cur).element_type(),
        XmlElementType::XmlElementNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd
    ) {
        if !(*cur).properties.is_null() {
            xml_free_prop_list((*cur).properties);
        }
        if !(*cur).ns_def.is_null() {
            xml_free_ns_list((*cur).ns_def);
        }
    } else if !(*cur).content.is_null()
        && !matches!((*cur).element_type(), XmlElementType::XmlEntityRefNode)
        && ((*cur).content != addr_of_mut!((*cur).properties) as _)
    {
        DICT_FREE!(dict, (*cur).content)
    }

    /*
     * When a node is a text node or a comment, it uses a global static
     * variable for the name of the node.
     * Otherwise the node name might come from the document's dictionary
     */
    if !(*cur).name.is_null()
        && !matches!((*cur).element_type(), XmlElementType::XmlTextNode)
        && !matches!((*cur).element_type(), XmlElementType::XmlCommentNode)
    {
        DICT_FREE!(dict, (*cur).name)
    }

    xml_free(cur as _);
}

unsafe extern "C" fn copy_string_for_new_dict_if_needed(
    old_dict: XmlDictPtr,
    new_dict: XmlDictPtr,
    old_value: *const XmlChar,
) -> *const XmlChar {
    let mut new_value: *const XmlChar = old_value;
    if !old_value.is_null() {
        let old_dict_owns_old_value: i32 =
            (!old_dict.is_null() && xml_dict_owns(old_dict, old_value) == 1) as i32;
        if old_dict_owns_old_value != 0 {
            if !new_dict.is_null() {
                new_value = xml_dict_lookup(new_dict, old_value, -1);
            } else {
                new_value = xml_strdup(old_value);
            }
        }
    }
    new_value
}

/// Verify that the given namespace held on @ancestor is still in scope on node.
///
/// Returns 1 if true, 0 if false and -1 in case of error.
#[doc(alias = "xmlNsInScope")]
unsafe extern "C" fn xml_ns_in_scope(
    _doc: XmlDocPtr,
    mut node: XmlNodePtr,
    ancestor: XmlNodePtr,
    prefix: *const XmlChar,
) -> i32 {
    let mut tst: XmlNsPtr;

    while !node.is_null() && node != ancestor {
        if matches!(
            (*node).element_type(),
            XmlElementType::XmlEntityRefNode
                | XmlElementType::XmlEntityNode
                | XmlElementType::XmlEntityDecl
        ) {
            return -1;
        }
        if matches!((*node).element_type(), XmlElementType::XmlElementNode) {
            tst = (*node).ns_def;
            while !tst.is_null() {
                if (*tst).prefix.is_null() && prefix.is_null() {
                    return 0;
                }
                if !(*tst).prefix.is_null()
                    && !prefix.is_null()
                    && xml_str_equal((*tst).prefix, prefix)
                {
                    return 0;
                }
                tst = (*tst).next;
            }
        }
        node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
    }
    if node != ancestor {
        return -1;
    }
    1
}

/// Do a copy of the namespace.
///
/// Returns: a new #xmlNsPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyNamespace")]
pub unsafe extern "C" fn xml_copy_namespace(cur: XmlNsPtr) -> XmlNsPtr {
    if cur.is_null() {
        return null_mut();
    }

    match (*cur).element_type() {
        XML_LOCAL_NAMESPACE => xml_new_ns(null_mut(), (*cur).href, (*cur).prefix),
        _ => null_mut(),
    }
}

/// Do a copy of an namespace list.
///
/// Returns: a new #xmlNsPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyNamespaceList")]
pub unsafe extern "C" fn xml_copy_namespace_list(mut cur: XmlNsPtr) -> XmlNsPtr {
    let mut ret: XmlNsPtr = null_mut();
    let mut p: XmlNsPtr = null_mut();
    let mut q: XmlNsPtr;

    while !cur.is_null() {
        q = xml_copy_namespace(cur);
        if q.is_null() {
            xml_free_ns_list(ret);
            return null_mut();
        }
        if p.is_null() {
            ret = q;
            p = ret;
        } else {
            (*p).next = q;
            p = q;
        }
        cur = (*cur).next;
    }
    ret
}

static XML_CHECK_DTD: AtomicBool = AtomicBool::new(true);

/// Handle an out of memory condition
#[doc(alias = "xmlTreeErr")]
unsafe fn xml_tree_err(code: XmlParserErrors, node: XmlNodePtr, extra: Option<&str>) {
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

const XHTML_STRICT_PUBLIC_ID: &str = "-//W3C//DTD XHTML 1.0 Strict//EN";
const XHTML_STRICT_SYSTEM_ID: &str = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd";
const XHTML_FRAME_PUBLIC_ID: &str = "-//W3C//DTD XHTML 1.0 Frameset//EN";
const XHTML_FRAME_SYSTEM_ID: &str = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd";
const XHTML_TRANS_PUBLIC_ID: &str = "-//W3C//DTD XHTML 1.0 Transitional//EN";
const XHTML_TRANS_SYSTEM_ID: &str = "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd";

/// Try to find if the document correspond to an XHTML DTD.
#[doc(alias = "xmlIsXHTML")]
pub fn is_xhtml(system_id: Option<&str>, public_id: Option<&str>) -> bool {
    public_id.map_or(false, |s| {
        s == XHTML_STRICT_PUBLIC_ID || s == XHTML_FRAME_PUBLIC_ID || s == XHTML_TRANS_PUBLIC_ID
    }) || system_id.map_or(false, |s| {
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

/// Allocates and initializes a new DOM-wrapper context.
///
/// Returns the xmlDOMWrapCtxtPtr or null_mut() in case of an internal error.
#[doc(alias = "xmlDOMWrapNewCtxt")]
pub unsafe extern "C" fn xml_dom_wrap_new_ctxt() -> XmlDOMWrapCtxtPtr {
    let ret: XmlDOMWrapCtxtPtr = xml_malloc(size_of::<XmlDOMWrapCtxt>()) as _;
    if ret.is_null() {
        xml_tree_err_memory("allocating DOM-wrapper context");
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlDOMWrapCtxt>());
    ret
}

/// Frees the ns-map
#[doc(alias = "xmlDOMWrapNsMapFree")]
unsafe extern "C" fn xml_dom_wrap_ns_map_free(nsmap: XmlNsMapPtr) {
    let mut cur: XmlNsMapItemPtr;
    let mut tmp: XmlNsMapItemPtr;

    if nsmap.is_null() {
        return;
    }
    cur = (*nsmap).pool;
    while !cur.is_null() {
        tmp = cur;
        cur = (*cur).next;
        xml_free(tmp as _);
    }
    cur = (*nsmap).first;
    while !cur.is_null() {
        tmp = cur;
        cur = (*cur).next;
        xml_free(tmp as _);
    }
    xml_free(nsmap as _);
}

/// Frees the DOM-wrapper context.
#[doc(alias = "xmlDOMWrapFreeCtxt")]
pub unsafe extern "C" fn xml_dom_wrap_free_ctxt(ctxt: XmlDOMWrapCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    if !(*ctxt).namespace_map.is_null() {
        xml_dom_wrap_ns_map_free((*ctxt).namespace_map as _);
    }
    /*
     * TODO: Store the namespace map in the context.
     */
    xml_free(ctxt as _);
}

#[repr(C)]
enum XmlDomreconcileNsoptions {
    XmlDomReconnsRemoveredund = 1 << 0,
}

macro_rules! XML_NSMAP_NOTEMPTY {
    ($m:expr) => {
        !$m.is_null() && !(*$m).first.is_null()
    };
}

macro_rules! XML_NSMAP_FOREACH {
    ($m:expr, $i:expr, $b:block) => {
        let mut __initialized = false;
        $i = (*$m).first;
        while {
            if !__initialized {
                __initialized = true;
            } else {
                $i = (*$i).next;
            }
            !$i.is_null()
        } {
            $b;
        }
    };
}

macro_rules! XML_NSMAP_POP {
    ($m:expr, $i:expr) => {
        $i = (*$m).last;
        (*$m).last = (*$i).prev;
        if (*$m).last.is_null() {
            (*$m).first = null_mut();
        } else {
            (*(*$m).last).next = null_mut();
        }
        (*$i).next = (*$m).pool;
        (*$m).pool = $i;
    };
}

// XML_TREE_ADOPT_STR: If we have a dest-dict, put @str in the dict;
// otherwise copy it, when it was in the source-dict.
macro_rules! XML_TREE_ADOPT_STR {
    ($str:expr, $adoptStr:expr, $sourceDoc:expr, $destDoc:expr) => {
        if $adoptStr != 0 && !$str.is_null() {
            if !(*$destDoc).dict.is_null() {
                let old: *const XmlChar = $str;
                $str = xml_dict_lookup((*$destDoc).dict, $str, -1);
                if $sourceDoc.is_null()
                    || (*$sourceDoc).dict.is_null()
                    || xml_dict_owns((*$sourceDoc).dict, old) == 0
                {
                    xml_free(old as _);
                }
            } else if !$sourceDoc.is_null()
                && !(*$sourceDoc).dict.is_null()
                && xml_dict_owns((*$sourceDoc).dict, $str) != 0
            {
                $str = xml_strdup($str);
            }
        }
    };
}

// XML_TREE_ADOPT_STR_2: If @str was in the source-dict, then
// put it in dest-dict or copy it.
macro_rules! XML_TREE_ADOPT_STR_2 {
    ($str:expr, $adoptStr:expr, $sourceDoc:expr, $destDoc:expr, $cur:expr) => {
        if $adoptStr != 0
            && !$str.is_null()
            && !$sourceDoc.is_null()
            && !(*$sourceDoc).dict.is_null()
            && xml_dict_owns((*$sourceDoc).dict, (*$cur).content) != 0
        {
            if !(*$destDoc).dict.is_null() {
                (*$cur).content = xml_dict_lookup((*$destDoc).dict, (*$cur).content, -1) as _;
            } else {
                (*$cur).content = xml_strdup((*$cur).content);
            }
        }
    };
}

const XML_TREE_NSMAP_PARENT: i32 = -1;
const XML_TREE_NSMAP_XML: i32 = -2;
const XML_TREE_NSMAP_DOC: i32 = -3;
const XML_TREE_NSMAP_CUSTOM: i32 = -4;

/// Adds an ns-mapping item.
#[doc(alias = "xmlDOMWrapNsMapAddItem")]
unsafe extern "C" fn xml_dom_wrap_ns_map_add_item(
    nsmap: *mut XmlNsMapPtr,
    position: i32,
    old_ns: XmlNsPtr,
    new_ns: XmlNsPtr,
    depth: i32,
) -> XmlNsMapItemPtr {
    let ret: XmlNsMapItemPtr;
    let mut map: XmlNsMapPtr;

    if nsmap.is_null() {
        return null_mut();
    }
    if position != -1 && position != 0 {
        return null_mut();
    }
    map = *nsmap;

    if map.is_null() {
        /*
        	* Create the ns-map.
        	*/
        map = xml_malloc(size_of::<XmlNsMap>()) as _;
        if map.is_null() {
            xml_tree_err_memory("allocating namespace map");
            return null_mut();
        }
        memset(map as _, 0, size_of::<XmlNsMap>());
        *nsmap = map;
    }

    if !(*map).pool.is_null() {
        /*
        	* Reuse an item from the pool.
        	*/
        ret = (*map).pool;
        (*map).pool = (*ret).next;
        memset(ret as _, 0, size_of::<XmlNsMapItem>());
    } else {
        /*
        	* Create a new item.
        	*/
        ret = xml_malloc(size_of::<XmlNsMapItem>()) as _;
        if ret.is_null() {
            xml_tree_err_memory("allocating namespace map item");
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlNsMapItem>());
    }

    if (*map).first.is_null() {
        /*
        	* First ever.
        	*/
        (*map).first = ret;
        (*map).last = ret;
    } else if position == -1 {
        /*
        	* Append.
        	*/
        (*ret).prev = (*map).last;
        (*(*map).last).next = ret;
        (*map).last = ret;
    } else if position == 0 {
        /*
        	* Set on first position.
        	*/
        (*(*map).first).prev = ret;
        (*ret).next = (*map).first;
        (*map).first = ret;
    }

    (*ret).old_ns = old_ns;
    (*ret).new_ns = new_ns;
    (*ret).shadow_depth = -1;
    (*ret).depth = depth;
    ret
}

/// Puts in-scope namespaces into the ns-map.
///
/// Returns 0 on success, -1 on API or internal errors.
#[doc(alias = "xmlDOMWrapNSNormGatherInScopeNs")]
unsafe extern "C" fn xml_dom_wrap_ns_norm_gather_in_scope_ns(
    map: *mut XmlNsMapPtr,
    node: XmlNodePtr,
) -> i32 {
    let mut cur: XmlNodePtr;
    let mut ns: XmlNsPtr;
    let mut mi: XmlNsMapItemPtr;
    let mut shadowed: i32;

    if map.is_null() || !(*map).is_null() {
        return -1;
    }
    if node.is_null() || matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl) {
        return -1;
    }
    /*
     * Get in-scope ns-decls of @parent.
     */
    cur = node;
    while !cur.is_null() && cur != (*cur).doc as _ {
        if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
            && !(*cur).ns_def.is_null()
        {
            ns = (*cur).ns_def;
            loop {
                shadowed = 0;
                if XML_NSMAP_NOTEMPTY!(*map) {
                    /*
                    	* Skip shadowed prefixes.
                    	*/
                    XML_NSMAP_FOREACH!(*map, mi, {
                        if (*ns).prefix == (*(*mi).new_ns).prefix
                            || xml_str_equal((*ns).prefix, (*(*mi).new_ns).prefix)
                        {
                            shadowed = 1;
                            break;
                        }
                    });
                }
                /*
                 * Insert mapping.
                 */
                mi = xml_dom_wrap_ns_map_add_item(map, 0, null_mut(), ns, XML_TREE_NSMAP_PARENT);
                if mi.is_null() {
                    return -1;
                }
                if shadowed != 0 {
                    (*mi).shadow_depth = 0;
                }
                ns = (*ns).next;

                if ns.is_null() {
                    break;
                }
            }
        }
        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
    }
    0
}

/// For internal use. Adds a ns-decl mapping.
///
/// Returns 0 on success, -1 on internal errors.
#[doc(alias = "xmlDOMWrapNSNormAddNsMapItem2")]
unsafe extern "C" fn xml_dom_wrap_ns_norm_add_ns_map_item2(
    list: *mut *mut XmlNsPtr,
    size: *mut i32,
    number: *mut i32,
    old_ns: XmlNsPtr,
    new_ns: XmlNsPtr,
) -> i32 {
    if !(*list).is_null() {
        *list = xml_malloc(6 * size_of::<XmlNsPtr>()) as _;
        if !(*list).is_null() {
            xml_tree_err_memory("alloc ns map item");
            return -1;
        }
        *size = 3;
        *number = 0;
    } else if *number >= *size {
        *size *= 2;
        *list = xml_realloc(*list as _, (*size) as usize * 2 * size_of::<XmlNsPtr>()) as _;
        if !(*list).is_null() {
            xml_tree_err_memory("realloc ns map item");
            return -1;
        }
    }
    *(*list).add(2 * (*number) as usize) = old_ns;
    *(*list).add(2 * (*number) as usize + 1) = new_ns;
    (*number) += 1;
    0
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

/// Creates or reuses an xmlNs struct on (*doc).oldNs with
/// the given prefix and namespace name.
///
/// Returns the acquired ns struct or null_mut() in case of an API or internal error.
#[doc(alias = "xmlDOMWrapStoreNs")]
unsafe extern "C" fn xml_dom_wrap_store_ns(
    doc: XmlDocPtr,
    ns_name: *const XmlChar,
    prefix: *const XmlChar,
) -> XmlNsPtr {
    let mut ns: XmlNsPtr;

    if doc.is_null() {
        return null_mut();
    }
    ns = (*doc).ensure_xmldecl();
    if ns.is_null() {
        return null_mut();
    }
    if !(*ns).next.is_null() {
        /* Reuse. */
        ns = (*ns).next;
        while !ns.is_null() {
            if (((*ns).prefix == prefix as _) || xml_str_equal((*ns).prefix, prefix))
                && xml_str_equal((*ns).href, ns_name)
            {
                return ns;
            }
            if (*ns).next.is_null() {
                break;
            }
            ns = (*ns).next;
        }
    }
    /* Create. */
    if !ns.is_null() {
        (*ns).next = xml_new_ns(null_mut(), ns_name, prefix);
        return (*ns).next;
    }
    null_mut()
}

/// Searches for a ns-decl with the given prefix in @nsList.
///
/// Returns the ns-decl if found, null_mut() if not found and on API errors.
#[doc(alias = "xmlTreeLookupNsListByPrefix")]
unsafe extern "C" fn xml_tree_nslist_lookup_by_prefix(
    ns_list: XmlNsPtr,
    prefix: *const XmlChar,
) -> XmlNsPtr {
    if ns_list.is_null() {
        return null_mut();
    }
    {
        let mut ns: XmlNsPtr;
        ns = ns_list;
        loop {
            if prefix == (*ns).prefix || xml_str_equal(prefix, (*ns).prefix) {
                return ns;
            }
            ns = (*ns).next;

            if ns.is_null() {
                break;
            }
        }
    }
    null_mut()
}

/// Dynamically searches for a ns-declaration which matches
/// the given @nsName in the ancestor-or-self axis of @node.
///
/// Returns 1 if a ns-decl was found, 0 if not and -1 on API and internal errors.
#[doc(alias = "xmlSearchNsByPrefixStrict")]
unsafe extern "C" fn xml_search_ns_by_prefix_strict(
    doc: XmlDocPtr,
    node: XmlNodePtr,
    prefix: *const XmlChar,
    ret_ns: *mut XmlNsPtr,
) -> i32 {
    let mut cur: XmlNodePtr;
    let mut ns: XmlNsPtr;

    if doc.is_null()
        || node.is_null()
        || matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
    {
        return -1;
    }

    if !ret_ns.is_null() {
        *ret_ns = null_mut();
    }
    if IS_STR_XML!(prefix) {
        if !ret_ns.is_null() {
            *ret_ns = (*doc).ensure_xmldecl();
            if (*ret_ns).is_null() {
                return -1;
            }
        }
        return 1;
    }
    cur = node;
    loop {
        if matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
            if !(*cur).ns_def.is_null() {
                ns = (*cur).ns_def;
                loop {
                    if prefix == (*ns).prefix || xml_str_equal(prefix, (*ns).prefix) {
                        /*
                        	* Disabled namespaces, e.g. xmlns:abc="".
                        	*/
                        if (*ns).href.is_null() {
                            return 0;
                        }
                        if !ret_ns.is_null() {
                            *ret_ns = ns;
                        }
                        return 1;
                    }
                    ns = (*ns).next;

                    if ns.is_null() {
                        break;
                    }
                }
            }
        } else if (matches!((*cur).element_type(), XmlElementType::XmlEntityNode)
            || matches!((*cur).element_type(), XmlElementType::XmlEntityDecl))
        {
            return 0;
        }
        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());

        if cur.is_null() || (*cur).doc == cur as _ {
            break;
        }
    }
    0
}

/// Declares a new namespace on @elem. It tries to use the
/// given @prefix; if a ns-decl with the given prefix is already existent
/// on @elem, it will generate an other prefix.
///
/// Returns 1 if a ns-decl was found, 0 if not and -1 on API and internal errors.
#[doc(alias = "xmlDOMWrapNSNormDeclareNsForced")]
unsafe extern "C" fn xml_dom_wrap_nsnorm_declare_ns_forced(
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    ns_name: *const XmlChar,
    prefix: *const XmlChar,
    check_shadow: i32,
) -> XmlNsPtr {
    let ret: XmlNsPtr;
    let mut buf: [c_char; 50] = [0; 50];
    let mut pref: *const XmlChar;
    let mut counter: i32 = 0;

    if doc.is_null()
        || elem.is_null()
        || !matches!((*elem).element_type(), XmlElementType::XmlElementNode)
    {
        return null_mut();
    }
    // Create a ns-decl on @anchor.
    pref = prefix;
    loop {
        // Lookup whether the prefix is unused in elem's ns-decls.
        if !(*elem).ns_def.is_null()
            && !xml_tree_nslist_lookup_by_prefix((*elem).ns_def, pref).is_null()
        {
            // goto ns_next_prefix;
        } else {
            // Does it shadow ancestor ns-decls?
            if check_shadow != 0
                && (*elem)
                    .parent()
                    .filter(|p| {
                        p.doc != p.as_ptr() as *mut XmlDoc
                            && xml_search_ns_by_prefix_strict(doc, p.as_ptr(), pref, null_mut())
                                == 1
                    })
                    .is_some()
            {
                // goto ns_next_prefix;
            } else {
                ret = xml_new_ns(null_mut(), ns_name, pref);
                if ret.is_null() {
                    return null_mut();
                }
                if (*elem).ns_def.is_null() {
                    (*elem).ns_def = ret;
                } else {
                    let mut ns2: XmlNsPtr = (*elem).ns_def;
                    while !(*ns2).next.is_null() {
                        ns2 = (*ns2).next;
                    }
                    (*ns2).next = ret;
                }
                return ret;
            }
        }
        // ns_next_prefix:
        counter += 1;
        if counter > 1000 {
            return null_mut();
        }
        if prefix.is_null() {
            snprintf(
                buf.as_mut_ptr() as _,
                buf.len(),
                c"ns_%d".as_ptr() as _,
                counter,
            );
        } else {
            snprintf(
                buf.as_mut_ptr() as _,
                buf.len(),
                c"%.30s_%d".as_ptr() as _,
                prefix,
                counter,
            );
        }
        pref = buf.as_ptr() as _;
    }
}

/// Searches for a matching ns-name in the ns-decls of @nsMap, if not
/// found it will either declare it on @elem, or store it in (*doc).oldNs.
/// If a new ns-decl needs to be declared on @elem, it tries to use the
/// @(*ns).prefix for it, if this prefix is already in use on @elem, it will
/// change the prefix or the new ns-decl.
///
/// Returns 0 if succeeded, -1 otherwise and on API/internal errors.
#[doc(alias = "xmlDOMWrapNSNormAcquireNormalizedNs")]
unsafe extern "C" fn xml_dom_wrap_ns_norm_acquire_normalized_ns(
    doc: XmlDocPtr,
    elem: XmlNodePtr,
    ns: XmlNsPtr,
    ret_ns: *mut XmlNsPtr,
    ns_map: *mut XmlNsMapPtr,
    depth: i32,
    ancestors_only: i32,
    prefixed: i32,
) -> i32 {
    let mut mi: XmlNsMapItemPtr;

    if doc.is_null() || ns.is_null() || ret_ns.is_null() || ns_map.is_null() {
        return -1;
    }

    *ret_ns = null_mut();
    /*
     * Handle XML namespace.
     */
    if IS_STR_XML!((*ns).prefix) {
        /*
        	* Insert XML namespace mapping.
        	*/
        *ret_ns = (*doc).ensure_xmldecl();
        if (*ret_ns).is_null() {
            return -1;
        }
        return 0;
    }
    /*
     * If the search should be done in ancestors only and no
     * @elem (the first ancestor) was specified, then skip the search.
     */
    if XML_NSMAP_NOTEMPTY!(*ns_map) && !(ancestors_only != 0 && elem.is_null()) {
        /*
        	* Try to find an equal ns-name in in-scope ns-decls.
        	*/
        XML_NSMAP_FOREACH!(*ns_map, mi, {
            if ((*mi).depth >= XML_TREE_NSMAP_PARENT) &&
		        /*
		        * ancestorsOnly: This should be turned on to gain speed,
		        * if one knows that the branch itself was already
		        * ns-wellformed and no stale references existed.
		        * I.e. it searches in the ancestor axis only.
		        */
		        (ancestors_only == 0 || (*mi).depth == XML_TREE_NSMAP_PARENT) &&
		        /* Skip shadowed prefixes. */
		        (*mi).shadow_depth == -1 &&
		        /* Skip xmlns="" or xmlns:foo="". */
		        (!(*(*mi).new_ns).href.is_null() &&
                *(*(*mi).new_ns).href.add(0) != 0) &&
		        /* Ensure a prefix if wanted. */
		        (prefixed == 0 || !(*(*mi).new_ns).prefix.is_null()) &&
		        /* Equal ns name */
		        ((*(*mi).new_ns).href == (*ns).href ||
                xml_str_equal((*(*mi).new_ns).href, (*ns).href) )
            {
                /* Set the mapping. */
                (*mi).old_ns = ns;
                *ret_ns = (*mi).new_ns;
                return 0;
            }
        });
    }
    /*
     * No luck, the namespace is out of scope or shadowed.
     */
    if elem.is_null() {
        /*
        	* Store ns-decls in "oldNs" of the document-node.
        	*/
        let tmpns: XmlNsPtr = xml_dom_wrap_store_ns(doc, (*ns).href, (*ns).prefix);
        if tmpns.is_null() {
            return -1;
        }
        /*
        	* Insert mapping.
        	*/
        if xml_dom_wrap_ns_map_add_item(ns_map, -1, ns, tmpns, XML_TREE_NSMAP_DOC).is_null() {
            xml_free_ns(tmpns);
            return -1;
        }
        *ret_ns = tmpns;
    } else {
        let tmpns: XmlNsPtr =
            xml_dom_wrap_nsnorm_declare_ns_forced(doc, elem, (*ns).href, (*ns).prefix, 0);
        if tmpns.is_null() {
            return -1;
        }

        if !(*ns_map).is_null() {
            /*
             * Does it shadow ancestor ns-decls?
             */
            XML_NSMAP_FOREACH!(*ns_map, mi, {
                if ((*mi).depth < depth)
                    && (*mi).shadow_depth == -1
                    && ((*ns).prefix == (*(*mi).new_ns).prefix
                        || xml_str_equal((*ns).prefix, (*(*mi).new_ns).prefix))
                {
                    /*
                     * Shadows.
                     */
                    (*mi).shadow_depth = depth;
                    break;
                }
            });
        }
        if xml_dom_wrap_ns_map_add_item(ns_map, -1, ns, tmpns, depth).is_null() {
            xml_free_ns(tmpns);
            return -1;
        }
        *ret_ns = tmpns;
    }
    0
}

/// Ensures that ns-references point to ns-decls hold on element-nodes.
/// Ensures that the tree is namespace wellformed by creating additional
/// ns-decls where needed. Note that, since prefixes of already existent
/// ns-decls can be shadowed by this process, it could break QNames in
/// attribute values or element content.
///
/// NOTE: This function was not intensively tested.
///
/// Returns 0 if succeeded, -1 otherwise and on API/internal errors.
#[doc(alias = "xmlDOMWrapReconcileNamespaces")]
pub unsafe extern "C" fn xml_dom_wrap_reconcile_namespaces(
    _ctxt: XmlDOMWrapCtxtPtr,
    elem: XmlNodePtr,
    options: i32,
) -> i32 {
    let mut depth: i32 = -1;
    let mut adoptns: i32;
    let mut parnsdone: i32 = 0;
    let mut ns: XmlNsPtr = null_mut();
    let mut prevns: XmlNsPtr;
    let mut cur: XmlNodePtr;
    let mut cur_elem: XmlNodePtr = null_mut();
    let mut ns_map: XmlNsMapPtr = null_mut();
    let mut mi: XmlNsMapItemPtr;
    /* @ancestorsOnly should be set by an option flag. */
    let ancestors_only: i32 = 0;
    let opt_remove_redundant_ns: i32 =
        if options & XmlDomreconcileNsoptions::XmlDomReconnsRemoveredund as i32 != 0 {
            1
        } else {
            0
        };
    let mut list_redund: *mut XmlNsPtr = null_mut();
    let mut size_redund: i32 = 0;
    let mut nb_redund: i32 = 0;

    if elem.is_null()
        || (*elem).doc.is_null()
        || !matches!((*elem).element_type(), XmlElementType::XmlElementNode)
    {
        return -1;
    }

    let ret;
    let doc: XmlDocPtr = (*elem).doc;
    cur = elem;
    'exit: {
        'internal_error: {
            'main: while {
                match (*cur).element_type() {
                    ty @ XmlElementType::XmlElementNode | ty @ XmlElementType::XmlAttributeNode => {
                        if matches!(ty, XmlElementType::XmlElementNode) {
                            adoptns = 1;
                            cur_elem = cur;
                            depth += 1;
                            // Namespace declarations.
                            if !(*cur).ns_def.is_null() {
                                prevns = null_mut();
                                ns = (*cur).ns_def;
                                'b: while !ns.is_null() {
                                    if parnsdone == 0 {
                                        if let Some(parent) = (*elem)
                                            .parent()
                                            .filter(|p| p.doc != p.as_ptr() as *mut XmlDoc)
                                        {
                                            // Gather ancestor in-scope ns-decls.
                                            if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                                addr_of_mut!(ns_map),
                                                parent.as_ptr(),
                                            ) == -1
                                            {
                                                break 'internal_error;
                                            }
                                        }
                                        parnsdone = 1;
                                    }

                                    // Lookup the ns ancestor-axis for equal ns-decls in scope.
                                    if opt_remove_redundant_ns != 0 && XML_NSMAP_NOTEMPTY!(ns_map) {
                                        XML_NSMAP_FOREACH!(ns_map, mi, {
                                            if (*mi).depth >= XML_TREE_NSMAP_PARENT
                                                && (*mi).shadow_depth == -1
                                                && ((*ns).prefix == (*(*mi).new_ns).prefix
                                                    || xml_str_equal(
                                                        (*ns).prefix,
                                                        (*(*mi).new_ns).prefix,
                                                    ))
                                                && ((*ns).href == (*(*mi).new_ns).href
                                                    || xml_str_equal(
                                                        (*ns).href,
                                                        (*(*mi).new_ns).href,
                                                    ))
                                            {
                                                // A redundant ns-decl was found.
                                                // Add it to the list of redundant ns-decls.
                                                if xml_dom_wrap_ns_norm_add_ns_map_item2(
                                                    addr_of_mut!(list_redund),
                                                    addr_of_mut!(size_redund),
                                                    addr_of_mut!(nb_redund),
                                                    ns,
                                                    (*mi).new_ns,
                                                ) == -1
                                                {
                                                    break 'internal_error;
                                                }
                                                // Remove the ns-decl from the element-node.
                                                if !prevns.is_null() {
                                                    (*prevns).next = (*ns).next;
                                                } else {
                                                    (*cur).ns_def = (*ns).next;
                                                }
                                                // goto next_ns_decl;
                                                ns = (*ns).next;
                                                continue 'b;
                                            }
                                        });
                                    }

                                    /*
                                     * Skip ns-references handling if the referenced
                                     * ns-decl is declared on the same element.
                                     */
                                    if !(*cur).ns.is_null() && adoptns != 0 && (*cur).ns == ns {
                                        adoptns = 0;
                                    }
                                    /*
                                     * Does it shadow any ns-decl?
                                     */
                                    if XML_NSMAP_NOTEMPTY!(ns_map) {
                                        XML_NSMAP_FOREACH!(ns_map, mi, {
                                            if (*mi).depth >= XML_TREE_NSMAP_PARENT
                                                && (*mi).shadow_depth == -1
                                                && ((*ns).prefix == (*(*mi).new_ns).prefix
                                                    || xml_str_equal(
                                                        (*ns).prefix,
                                                        (*(*mi).new_ns).prefix,
                                                    ))
                                            {
                                                (*mi).shadow_depth = depth;
                                            }
                                        });
                                    }
                                    /*
                                     * Push mapping.
                                     */
                                    if xml_dom_wrap_ns_map_add_item(
                                        addr_of_mut!(ns_map),
                                        -1,
                                        ns,
                                        ns,
                                        depth,
                                    )
                                    .is_null()
                                    {
                                        break 'internal_error;
                                    }

                                    prevns = ns;
                                    // next_ns_decl:
                                    ns = (*ns).next;
                                }
                            }
                            if adoptns == 0 {
                                // goto ns_end;
                                if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                                    && !(*cur).properties.is_null()
                                {
                                    /*
                                     * Process attributes.
                                     */
                                    cur = (*cur).properties as _;
                                    if cur.is_null() {
                                        break 'main;
                                    }
                                    continue 'main;
                                }
                            }
                        }

                        /* No ns, no fun. */
                        if (*cur).ns.is_null() {
                            // goto ns_end;
                            if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                                && !(*cur).properties.is_null()
                            {
                                /*
                                 * Process attributes.
                                 */
                                cur = (*cur).properties as _;
                                if cur.is_null() {
                                    break 'main;
                                }
                                continue 'main;
                            }
                        }

                        if parnsdone == 0 {
                            if (*elem)
                                .parent()
                                .filter(|p| {
                                    p.doc != p.as_ptr() as *mut XmlDoc
                                        && xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                            addr_of_mut!(ns_map),
                                            p.as_ptr(),
                                        ) == -1
                                })
                                .is_some()
                            {
                                break 'internal_error;
                            }
                            parnsdone = 1;
                        }
                        /*
                         * Adjust the reference if this was a redundant ns-decl.
                         */
                        if !list_redund.is_null() {
                            for (_, j) in (0..nb_redund).zip((0..).step_by(2)) {
                                if (*cur).ns == *list_redund.add(j) {
                                    (*cur).ns = *list_redund.add(j + 1);
                                    break;
                                }
                            }
                        }
                        /*
                         * Adopt ns-references.
                         */
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            /*
                             * Search for a mapping.
                             */
                            XML_NSMAP_FOREACH!(ns_map, mi, {
                                if (*mi).shadow_depth == -1 && ((*cur).ns == (*mi).old_ns) {
                                    (*cur).ns = (*mi).new_ns;
                                    // goto ns_end;
                                    if matches!(
                                        (*cur).element_type(),
                                        XmlElementType::XmlElementNode
                                    ) && !(*cur).properties.is_null()
                                    {
                                        /*
                                         * Process attributes.
                                         */
                                        cur = (*cur).properties as _;
                                        if cur.is_null() {
                                            break 'main;
                                        }
                                        continue 'main;
                                    }
                                }
                            });
                        }
                        /*
                         * Acquire a normalized ns-decl and add it to the map.
                         */
                        if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                            doc,
                            cur_elem,
                            (*cur).ns,
                            addr_of_mut!(ns),
                            addr_of_mut!(ns_map),
                            depth,
                            ancestors_only,
                            matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
                                as i32,
                        ) == -1
                        {
                            break 'internal_error;
                        }
                        (*cur).ns = ns;

                        // ns_end:
                        if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                            && !(*cur).properties.is_null()
                        {
                            /*
                             * Process attributes.
                             */
                            cur = (*cur).properties as _;
                            if cur.is_null() {
                                break 'main;
                            }
                            continue 'main;
                        }
                    }
                    _ => {
                        // goto next_sibling;
                        'next_sibling: loop {
                            if cur == elem {
                                break 'main;
                            }
                            if matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
                                if XML_NSMAP_NOTEMPTY!(ns_map) {
                                    /*
                                     * Pop mappings.
                                     */
                                    while !(*ns_map).last.is_null()
                                        && (*(*ns_map).last).depth >= depth
                                    {
                                        XML_NSMAP_POP!(ns_map, mi);
                                    }
                                    /*
                                     * Unshadow.
                                     */
                                    XML_NSMAP_FOREACH!(ns_map, mi, {
                                        if (*mi).shadow_depth >= depth {
                                            (*mi).shadow_depth = -1;
                                        }
                                    });
                                }
                                depth -= 1;
                            }
                            if let Some(next) = (*cur).next {
                                cur = next.as_ptr();
                            } else {
                                if matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
                                {
                                    cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                                    // goto into_content;
                                    break 'next_sibling;
                                }
                                cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                                // goto next_sibling;
                                continue 'next_sibling;
                            }

                            if cur.is_null() {
                                break 'main;
                            }
                            continue 'main;
                        }
                    }
                }
                // into_content:
                'into_content: loop {
                    if let Some(children) = (*cur)
                        .children()
                        .filter(|_| matches!((*cur).element_type(), XmlElementType::XmlElementNode))
                    {
                        /*
                         * Process content of element-nodes only.
                         */
                        cur = children.as_ptr();
                        continue;
                    }
                    // next_sibling:
                    'next_sibling: loop {
                        if cur == elem {
                            break 'main;
                        }
                        if matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
                            if XML_NSMAP_NOTEMPTY!(ns_map) {
                                /*
                                 * Pop mappings.
                                 */
                                while !(*ns_map).last.is_null() && (*(*ns_map).last).depth >= depth
                                {
                                    XML_NSMAP_POP!(ns_map, mi);
                                }
                                /*
                                 * Unshadow.
                                 */
                                XML_NSMAP_FOREACH!(ns_map, mi, {
                                    if (*mi).shadow_depth >= depth {
                                        (*mi).shadow_depth = -1;
                                    }
                                });
                            }
                            depth -= 1;
                        }
                        if let Some(next) = (*cur).next {
                            cur = next.as_ptr();
                        } else {
                            if matches!((*cur).element_type(), XmlElementType::XmlAttributeNode) {
                                cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                                // goto into_content;
                                continue 'into_content;
                            }
                            cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                            // goto next_sibling;
                            continue 'next_sibling;
                        }
                        break 'next_sibling;
                    }

                    break 'into_content;
                }

                !cur.is_null()
            } {}

            ret = 0;
            break 'exit;
        }
        // internal_error:
        ret = -1;
    }
    // exit:
    if !list_redund.is_null() {
        for (_, j) in (0..nb_redund).zip((0..).step_by(2)) {
            xml_free_ns(*list_redund.add(j as usize));
        }
        xml_free(list_redund as _);
    }
    if !ns_map.is_null() {
        xml_dom_wrap_ns_map_free(ns_map);
    }
    ret
}

/// Ensures that ns-references point to @destDoc: either to
/// elements->nsDef entries if @destParent is given, or to
/// @(*destDoc).oldNs otherwise.
/// If @destParent is given, it ensures that the tree is namespace
/// wellformed by creating additional ns-decls where needed.
/// Note that, since prefixes of already existent ns-decls can be
/// shadowed by this process, it could break QNames in attribute
/// values or element content.
///
/// NOTE: This function was not intensively tested.
///
/// Returns 0 if succeeded, -1 otherwise and on API/internal errors.
#[doc(alias = "xmlDOMWrapAdoptBranch")]
unsafe extern "C" fn xml_dom_wrap_adopt_branch(
    ctxt: XmlDOMWrapCtxtPtr,
    source_doc: XmlDocPtr,
    node: XmlNodePtr,
    dest_doc: XmlDocPtr,
    dest_parent: XmlNodePtr,
    _options: i32,
) -> i32 {
    let mut ret: i32 = 0;
    let mut cur: XmlNodePtr;
    let mut cur_elem: XmlNodePtr = null_mut();
    let mut ns_map: XmlNsMapPtr = null_mut();
    let mut mi: XmlNsMapItemPtr;
    let mut ns: XmlNsPtr = null_mut();
    let mut depth: i32 = -1;
    /* gather @parent's ns-decls. */
    let mut parnsdone: i32;
    /* @ancestorsOnly should be set per option. */
    let ancestors_only: i32 = 0;

    /*
     * Optimize string adoption for equal or none dicts.
     */
    let adopt_str = if !source_doc.is_null() && (*source_doc).dict == (*dest_doc).dict {
        0
    } else {
        1
    };

    /*
     * Get the ns-map from the context if available.
     */
    if !ctxt.is_null() {
        ns_map = (*ctxt).namespace_map as _;
    }
    /*
     * Disable search for ns-decls in the parent-axis of the
     * destination element, if:
     * 1) there's no destination parent
     * 2) custom ns-reference handling is used
     */
    if dest_parent.is_null() || (!ctxt.is_null() && (*ctxt).get_ns_for_node_func.is_some()) {
        parnsdone = 1;
    } else {
        parnsdone = 0;
    }

    'exit: {
        'internal_error: {
            cur = node;
            if !cur.is_null() && matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
                break 'internal_error;
            }

            'main: while !cur.is_null() {
                let mut leave_node = false;

                /*
                 * Paranoid source-doc sanity check.
                 */
                if (*cur).doc != source_doc {
                    /*
                     * We'll assume XIncluded nodes if the doc differs.
                     * TODO: Do we need to reconciliate XIncluded nodes?
                     * This here skips XIncluded nodes and tries to handle
                     * broken sequences.
                     */
                    if (*cur).next.is_some() {
                        let mut next = (*cur).next;
                        while let Some(now) = next.filter(|now| {
                            !matches!(now.element_type(), XmlElementType::XmlXIncludeEnd)
                                && now.doc != (*node).doc
                        }) {
                            cur = now.as_ptr();
                            next = now.next;
                        }

                        if (*cur).doc != (*node).doc {
                            // goto leave_node;
                            leave_node = true;
                        }
                    } else {
                        // goto leave_node;
                        leave_node = true;
                    }
                }

                if !leave_node {
                    (*cur).doc = dest_doc;
                    match (*cur).element_type() {
                        XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
                            /*
                             * TODO
                             */
                            return -1;
                        }
                        ty @ XmlElementType::XmlElementNode
                        | ty @ XmlElementType::XmlAttributeNode => {
                            if matches!(ty, XmlElementType::XmlElementNode) {
                                cur_elem = cur;
                                depth += 1;
                                /*
                                 * Namespace declarations.
                                 * - (*ns).href and (*ns).prefix are never in the dict, so
                                 *   we need not move the values over to the destination dict.
                                 * - Note that for custom handling of ns-references,
                                 *   the ns-decls need not be stored in the ns-map,
                                 *   since they won't be referenced by (*node).ns.
                                 */
                                if !(*cur).ns_def.is_null()
                                    && (ctxt.is_null() || (*ctxt).get_ns_for_node_func.is_none())
                                {
                                    if parnsdone == 0 {
                                        /*
                                         * Gather @parent's in-scope ns-decls.
                                         */
                                        if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                            addr_of_mut!(ns_map),
                                            dest_parent,
                                        ) == -1
                                        {
                                            break 'internal_error;
                                        }

                                        parnsdone = 1;
                                    }
                                    ns = (*cur).ns_def;
                                    while !ns.is_null() {
                                        /*
                                         * NOTE: (*ns).prefix and (*ns).href are never in the dict.
                                         * XML_TREE_ADOPT_STR((*ns).prefix)
                                         * XML_TREE_ADOPT_STR((*ns).href)
                                         */
                                        /*
                                         * Does it shadow any ns-decl?
                                         */
                                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                                            XML_NSMAP_FOREACH!(ns_map, mi, {
                                                if (*mi).depth >= XML_TREE_NSMAP_PARENT
                                                    && (*mi).shadow_depth == -1
                                                    && ((*ns).prefix == (*(*mi).new_ns).prefix
                                                        || xml_str_equal(
                                                            (*ns).prefix,
                                                            (*(*mi).new_ns).prefix,
                                                        ))
                                                {
                                                    (*mi).shadow_depth = depth;
                                                }
                                            });
                                        }
                                        /*
                                         * Push mapping.
                                         */
                                        if xml_dom_wrap_ns_map_add_item(
                                            addr_of_mut!(ns_map),
                                            -1,
                                            ns,
                                            ns,
                                            depth,
                                        )
                                        .is_null()
                                        {
                                            break 'internal_error;
                                        }
                                        ns = (*ns).next;
                                    }
                                }
                            }
                            /* No namespace, no fun. */
                            if (*cur).ns.is_null() {
                                // goto ns_end;
                            } else {
                                if parnsdone == 0 {
                                    if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                        addr_of_mut!(ns_map),
                                        dest_parent,
                                    ) == -1
                                    {
                                        break 'internal_error;
                                    }
                                    parnsdone = 1;
                                }
                                /*
                                 * Adopt ns-references.
                                 */
                                let mut ns_end = false;
                                if XML_NSMAP_NOTEMPTY!(ns_map) {
                                    /*
                                     * Search for a mapping.
                                     */
                                    XML_NSMAP_FOREACH!(ns_map, mi, {
                                        if (*mi).shadow_depth == -1 && ((*cur).ns == (*mi).old_ns) {
                                            (*cur).ns = (*mi).new_ns;
                                            // goto ns_end;
                                            ns_end = true;
                                            break;
                                        }
                                    });
                                }

                                if !ns_end {
                                    /*
                                     * No matching namespace in scope. We need a new one.
                                     */
                                    if !ctxt.is_null() && (*ctxt).get_ns_for_node_func.is_some() {
                                        /*
                                         * User-defined behaviour.
                                         */
                                        ns = ((*ctxt).get_ns_for_node_func.unwrap())(
                                            ctxt,
                                            cur,
                                            (*(*cur).ns).href,
                                            (*(*cur).ns).prefix,
                                        );
                                        /*
                                         * Insert mapping if ns is available; it's the users fault
                                         * if not.
                                         */
                                        if xml_dom_wrap_ns_map_add_item(
                                            addr_of_mut!(ns_map),
                                            -1,
                                            (*cur).ns,
                                            ns,
                                            XML_TREE_NSMAP_CUSTOM,
                                        )
                                        .is_null()
                                        {
                                            break 'internal_error;
                                        }
                                        (*cur).ns = ns;
                                    } else {
                                        /*
                                         * Acquire a normalized ns-decl and add it to the map.
                                         */
                                        if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                                            dest_doc,
                                            /* ns-decls on curElem or on (*destDoc).oldNs */
                                            if !dest_parent.is_null() {
                                                cur_elem
                                            } else {
                                                null_mut()
                                            },
                                            (*cur).ns,
                                            addr_of_mut!(ns),
                                            addr_of_mut!(ns_map),
                                            depth,
                                            ancestors_only,
                                            /* ns-decls must be prefixed for attributes. */
                                            matches!(
                                                (*cur).element_type(),
                                                XmlElementType::XmlAttributeNode
                                            ) as i32,
                                        ) == -1
                                        {
                                            break 'internal_error;
                                        }
                                        (*cur).ns = ns;
                                    }
                                }
                            }
                            // ns_end:
                            // Further node properties.
                            // TODO: Is this all?
                            XML_TREE_ADOPT_STR!((*cur).name, adopt_str, source_doc, dest_doc);
                            if matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
                                (*cur).psvi = null_mut();
                                (*cur).line = 0;
                                (*cur).extra = 0;
                                // Walk attributes.
                                if !(*cur).properties.is_null() {
                                    // Process first attribute node.
                                    cur = (*cur).properties as _;
                                    continue;
                                }
                            } else {
                                // Attributes.
                                if !source_doc.is_null()
                                    && matches!(
                                        (*cur).as_attribute_node().unwrap().as_ref().atype,
                                        Some(XmlAttributeType::XmlAttributeID)
                                    )
                                {
                                    xml_remove_id(source_doc, cur as _);
                                }
                                (*cur).as_attribute_node().unwrap().as_mut().atype = None;
                                (*cur).as_attribute_node().unwrap().as_mut().psvi = null_mut();
                            }
                        }
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {
                            // This puts the content in the dest dict, only if
                            // it was previously in the source dict.
                            XML_TREE_ADOPT_STR_2!(
                                (*cur).content,
                                adopt_str,
                                source_doc,
                                dest_doc,
                                cur
                            );
                            // goto leave_node;
                            leave_node = true;
                        }
                        XmlElementType::XmlEntityRefNode => {
                            // Remove reference to the entity-node.
                            (*cur).content = null_mut();
                            (*cur).set_children(None);
                            (*cur).set_last(None);
                            if !(*dest_doc).int_subset.is_null()
                                || !(*dest_doc).ext_subset.is_null()
                            {
                                // Assign new entity-node if available.
                                let ent: XmlEntityPtr =
                                    xml_get_doc_entity(dest_doc, &(*cur).name().unwrap());
                                if !ent.is_null() {
                                    (*cur).content = (*ent).content.load(Ordering::Relaxed);
                                    (*cur).set_children(NodePtr::from_ptr(ent as *mut XmlNode));
                                    (*cur).set_last(NodePtr::from_ptr(ent as *mut XmlNode));
                                }
                            }
                            // goto leave_node;
                            leave_node = true;
                        }
                        XmlElementType::XmlPINode => {
                            XML_TREE_ADOPT_STR!((*cur).name, adopt_str, source_doc, dest_doc);
                            XML_TREE_ADOPT_STR_2!(
                                (*cur).content,
                                adopt_str,
                                source_doc,
                                dest_doc,
                                cur
                            );
                        }
                        XmlElementType::XmlCommentNode => {}
                        _ => {
                            break 'internal_error;
                        }
                    }

                    if !leave_node {
                        // Walk the tree.
                        if let Some(children) = (*cur).children() {
                            cur = children.as_ptr();
                            continue;
                        }
                    }
                }

                // leave_node:
                'leave_node: loop {
                    if cur == node {
                        break 'main;
                    }
                    if matches!(
                        (*cur).element_type(),
                        XmlElementType::XmlElementNode
                            | XmlElementType::XmlXIncludeStart
                            | XmlElementType::XmlXIncludeEnd
                    ) {
                        // TODO: Do we expect nsDefs on xmlElementType::XML_XINCLUDE_START?
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            // Pop mappings.
                            while !(*ns_map).last.is_null() && (*(*ns_map).last).depth >= depth {
                                XML_NSMAP_POP!(ns_map, mi);
                            }
                            // Unshadow.
                            XML_NSMAP_FOREACH!(ns_map, mi, {
                                if (*mi).shadow_depth >= depth {
                                    (*mi).shadow_depth = -1;
                                }
                            });
                        }
                        depth -= 1;
                    }
                    if let Some(next) = (*cur).next {
                        cur = next.as_ptr();
                    } else if let Some(children) = (*cur).parent().and_then(|p| {
                        p.children().filter(|_| {
                            matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
                        })
                    }) {
                        cur = children.as_ptr();
                    } else {
                        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                        // goto leave_node;
                        continue 'leave_node;
                    }

                    break;
                }
            }
            break 'exit;
        }
        // internal_error:
        ret = -1;
    }

    // exit:
    // Cleanup.
    if !ns_map.is_null() {
        if !ctxt.is_null() && (*ctxt).namespace_map == ns_map as _ {
            // Just cleanup the map but don't free.
            if !(*ns_map).first.is_null() {
                if !(*ns_map).pool.is_null() {
                    (*(*ns_map).last).next = (*ns_map).pool;
                }
                (*ns_map).pool = (*ns_map).first;
                (*ns_map).first = null_mut();
            }
        } else {
            xml_dom_wrap_ns_map_free(ns_map);
        }
    }
    ret
}

/// Dynamically searches for a ns-declaration which matches
/// the given @nsName in the ancestor-or-self axis of @node.
///
/// Returns 1 if a ns-decl was found, 0 if not and -1 on API and internal errors.
#[doc(alias = "xmlSearchNsByNamespaceStrict")]
unsafe extern "C" fn xml_search_ns_by_namespace_strict(
    doc: XmlDocPtr,
    node: XmlNodePtr,
    ns_name: *const XmlChar,
    ret_ns: *mut XmlNsPtr,
    prefixed: i32,
) -> i32 {
    let mut cur: XmlNodePtr;
    let mut prev: XmlNodePtr = null_mut();
    let mut out: XmlNodePtr = null_mut();
    let mut ns: XmlNsPtr;
    let mut prevns: XmlNsPtr;

    if doc.is_null() || ns_name.is_null() || ret_ns.is_null() {
        return -1;
    }
    if node.is_null() || matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl) {
        return -1;
    }

    *ret_ns = null_mut();
    if xml_str_equal(ns_name, XML_XML_NAMESPACE.as_ptr() as _) {
        *ret_ns = (*doc).ensure_xmldecl();
        if (*ret_ns).is_null() {
            return -1;
        }
        return 1;
    }
    cur = node;
    loop {
        if matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
            if !(*cur).ns_def.is_null() {
                ns = (*cur).ns_def;
                while !ns.is_null() {
                    if prefixed != 0 && (*ns).prefix.is_null() {
                        ns = (*ns).next;
                        continue;
                    }
                    if !prev.is_null() {
                        /*
                         * Check the last level of ns-decls for a
                         * shadowing prefix.
                         */
                        prevns = (*prev).ns_def;
                        loop {
                            if (*prevns).prefix == (*ns).prefix
                                || (!(*prevns).prefix.is_null()
                                    && !(*ns).prefix.is_null()
                                    && xml_str_equal((*prevns).prefix, (*ns).prefix))
                            {
                                /*
                                 * Shadowed.
                                 */
                                break;
                            }
                            prevns = (*prevns).next;

                            if prevns.is_null() {
                                break;
                            }
                        }
                        if !prevns.is_null() {
                            ns = (*ns).next;
                            continue;
                        }
                    }
                    /*
                     * Ns-name comparison.
                     */
                    if ns_name == (*ns).href || xml_str_equal(ns_name, (*ns).href) {
                        /*
                         * At this point the prefix can only be shadowed,
                         * if we are the the (at least) 3rd level of
                         * ns-decls.
                         */
                        if !out.is_null() {
                            let ret: i32 = xml_ns_in_scope(doc, node, prev, (*ns).prefix);
                            if ret < 0 {
                                return -1;
                            }
                            /*
                             * TODO: Should we try to find a matching ns-name
                             * only once? This here keeps on searching.
                             * I think we should try further since, there might
                             * be an other matching ns-decl with an unshadowed
                             * prefix.
                             */
                            if ret == 0 {
                                ns = (*ns).next;
                                continue;
                            }
                        }
                        *ret_ns = ns;
                        return 1;
                    }
                }
                out = prev;
                prev = cur;
            }
        } else if (matches!((*cur).element_type(), XmlElementType::XmlEntityNode)
            || matches!((*cur).element_type(), XmlElementType::XmlEntityDecl))
        {
            return 0;
        }
        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());

        if cur.is_null() || (*cur).doc == cur as _ {
            break;
        }
    }
    0
}

/// @attr is adopted by @destDoc.
/// Ensures that ns-references point to @destDoc: either to
/// elements->nsDef entries if @destParent is given, or to
/// @(*destDoc).oldNs otherwise.
///
/// Returns 0 if succeeded, -1 otherwise and on API/internal errors.
#[doc(alias = "xmlDOMWrapAdoptAttr")]
unsafe extern "C" fn xml_dom_wrap_adopt_attr(
    ctxt: XmlDOMWrapCtxtPtr,
    source_doc: XmlDocPtr,
    attr: XmlAttrPtr,
    dest_doc: XmlDocPtr,
    dest_parent: XmlNodePtr,
    _options: i32,
) -> i32 {
    let mut cur: XmlNodePtr;
    let adopt_str: i32 = 1;

    if !attr.is_null() || dest_doc.is_null() {
        return -1;
    }

    (*attr).doc = dest_doc;
    if !(*attr).ns.is_null() {
        let mut ns: XmlNsPtr = null_mut();

        if !ctxt.is_null() { /* TODO: User defined. */ }
        /* XML Namespace. */
        if IS_STR_XML!((*(*attr).ns).prefix) {
            ns = (*dest_doc).ensure_xmldecl();
        } else if dest_parent.is_null() {
            /*
             * Store in @(*destDoc).oldNs.
             */
            ns = xml_dom_wrap_store_ns(dest_doc, (*(*attr).ns).href, (*(*attr).ns).prefix);
        } else {
            /*
             * Declare on @destParent.
             */
            if xml_search_ns_by_namespace_strict(
                dest_doc,
                dest_parent,
                (*(*attr).ns).href,
                addr_of_mut!(ns),
                1,
            ) == -1
            {
                // goto internal_error;
                return -1;
            }
            if ns.is_null() {
                ns = xml_dom_wrap_nsnorm_declare_ns_forced(
                    dest_doc,
                    dest_parent,
                    (*(*attr).ns).href,
                    (*(*attr).ns).prefix,
                    1,
                );
            }
        }
        if ns.is_null() {
            // goto internal_error;
            return -1;
        }
        (*attr).ns = ns;
    }

    XML_TREE_ADOPT_STR!((*attr).name, adopt_str, source_doc, dest_doc);
    (*attr).atype = None;
    (*attr).psvi = null_mut();
    /*
     * Walk content.
     */
    let Some(children) = (*attr).children else {
        return 0;
    };
    cur = children.as_ptr();
    if !cur.is_null() && matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
        // goto internal_error;
        return -1;
    }
    while !cur.is_null() {
        (*cur).doc = dest_doc;
        match (*cur).element_type() {
            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {
                XML_TREE_ADOPT_STR_2!((*cur).content, adopt_str, source_doc, dest_doc, cur);
            }
            XmlElementType::XmlEntityRefNode => {
                // Remove reference to the entity-node.
                (*cur).content = null_mut();
                (*cur).set_children(None);
                (*cur).set_last(None);
                if !(*dest_doc).int_subset.is_null() || !(*dest_doc).ext_subset.is_null() {
                    // Assign new entity-node if available.
                    let ent: XmlEntityPtr = xml_get_doc_entity(dest_doc, &(*cur).name().unwrap());
                    if !ent.is_null() {
                        (*cur).content = (*ent).content.load(Ordering::Relaxed);
                        (*cur).set_children(NodePtr::from_ptr(ent as *mut XmlNode));
                        (*cur).set_last(NodePtr::from_ptr(ent as *mut XmlNode));
                    }
                }
            }
            _ => {}
        }
        if let Some(children) = (*cur).children() {
            cur = children.as_ptr();
            continue;
        }
        // next_sibling:
        'next_sibling: loop {
            if cur == attr as _ {
                break;
            }
            if let Some(next) = (*cur).next {
                cur = next.as_ptr();
            } else {
                cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                // goto next_sibling;
                continue 'next_sibling;
            }

            break;
        }
    }
    0
    // internal_error:
    //     return -1;
}

/// References of out-of scope ns-decls are remapped to point to @destDoc:
/// 1) If @destParent is given, then nsDef entries on element-nodes are used
/// 2) If *no* @destParent is given, then @(*destDoc).oldNs entries are used
///    This is the case when you have an unlinked node and just want to move it
///    to the context of
///
/// If @destParent is given, it ensures that the tree is namespace
/// wellformed by creating additional ns-decls where needed.
/// Note that, since prefixes of already existent ns-decls can be
/// shadowed by this process, it could break QNames in attribute
/// values or element content.
/// NOTE: This function was not intensively tested.
///
/// Returns 0 if the operation succeeded,
///         1 if a node of unsupported type was given,
///         2 if a node of not yet supported type was given and
///         -1 on API/internal errors.
#[doc(alias = "xmlDOMWrapAdoptNode")]
pub unsafe extern "C" fn xml_dom_wrap_adopt_node(
    ctxt: XmlDOMWrapCtxtPtr,
    mut source_doc: XmlDocPtr,
    node: XmlNodePtr,
    dest_doc: XmlDocPtr,
    dest_parent: XmlNodePtr,
    options: i32,
) -> i32 {
    if node.is_null()
        || matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
        || dest_doc.is_null()
        || (!dest_parent.is_null() && (*dest_parent).doc != dest_doc)
    {
        return -1;
    }
    /*
     * Check (*node).doc sanity.
     */
    if !(*node).doc.is_null() && !source_doc.is_null() && (*node).doc != source_doc {
        /*
        	* Might be an XIncluded node.
        	*/
        return -1;
    }
    if source_doc.is_null() {
        source_doc = (*node).doc;
    }
    if source_doc == dest_doc {
        return -1;
    }
    match (*node).element_type() {
        XmlElementType::XmlElementNode
        | XmlElementType::XmlAttributeNode
        | XmlElementType::XmlTextNode
        | XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCommentNode => {}
        XmlElementType::XmlDocumentFragNode => {
            /* TODO: Support document-fragment-nodes. */
            return 2;
        }
        _ => {
            return 1;
        }
    }
    // Unlink only if @node was not already added to @destParent.
    if (*node)
        .parent()
        .filter(|p| dest_parent != p.as_ptr())
        .is_some()
    {
        (*node).unlink();
    }

    if matches!((*node).element_type(), XmlElementType::XmlElementNode) {
        return xml_dom_wrap_adopt_branch(ctxt, source_doc, node, dest_doc, dest_parent, options);
    } else if matches!((*node).element_type(), XmlElementType::XmlAttributeNode) {
        return xml_dom_wrap_adopt_attr(
            ctxt,
            source_doc,
            node as _,
            dest_doc,
            dest_parent,
            options,
        );
    } else {
        let cur: XmlNodePtr = node;
        let mut adopt_str: i32 = 1;

        (*cur).doc = dest_doc;
        // Optimize string adoption.
        if !source_doc.is_null() && (*source_doc).dict == (*dest_doc).dict {
            adopt_str = 0;
        }
        match (*node).element_type() {
            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {
                XML_TREE_ADOPT_STR_2!((*node).content, adopt_str, source_doc, dest_doc, cur);
            }
            XmlElementType::XmlEntityRefNode => {
                // Remove reference to the entity-node.
                (*node).content = null_mut();
                (*node).set_children(None);
                (*node).set_last(None);
                if !(*dest_doc).int_subset.is_null() || !(*dest_doc).ext_subset.is_null() {
                    // Assign new entity-node if available.
                    let ent: XmlEntityPtr = xml_get_doc_entity(dest_doc, &(*node).name().unwrap());
                    if !ent.is_null() {
                        (*node).content = (*ent).content.load(Ordering::Relaxed);
                        (*node).set_children(NodePtr::from_ptr(ent as *mut XmlNode));
                        (*node).set_last(NodePtr::from_ptr(ent as *mut XmlNode));
                    }
                }
                XML_TREE_ADOPT_STR!((*node).name, adopt_str, source_doc, dest_doc);
            }
            XmlElementType::XmlPINode => {
                XML_TREE_ADOPT_STR!((*node).name, adopt_str, source_doc, dest_doc);
                XML_TREE_ADOPT_STR_2!((*node).content, adopt_str, source_doc, dest_doc, cur);
            }
            _ => {}
        }
    }
    0
}

/// Unlinks the given node from its owner.
/// This will substitute ns-references to (*node).nsDef for
/// ns-references to (*doc).oldNs, thus ensuring the removed
/// branch to be autark wrt ns-references.
///
/// NOTE: This function was not intensively tested.
///
/// Returns 0 on success, 1 if the node is not supported,
///         -1 on API and internal errors.
#[doc(alias = "xmlDOMWrapRemoveNode")]
pub unsafe extern "C" fn xml_dom_wrap_remove_node(
    ctxt: XmlDOMWrapCtxtPtr,
    doc: XmlDocPtr,
    mut node: XmlNodePtr,
    _options: i32,
) -> i32 {
    let mut list: *mut XmlNsPtr = null_mut();
    let mut size_list: i32 = 0;
    let mut nb_list: i32 = 0;
    let mut ns: XmlNsPtr;

    if node.is_null() || doc.is_null() || (*node).doc != doc {
        return -1;
    }

    /* TODO: 0 or -1 ? */
    if (*node).parent().is_none() {
        return 0;
    }

    match (*node).element_type() {
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCommentNode => {
            (*node).unlink();
            return 0;
        }
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {}
        _ => {
            return 1;
        }
    }
    (*node).unlink();
    /*
     * Save out-of-scope ns-references in (*doc).oldNs.
     */
    'main: loop {
        match (*node).element_type() {
            ty @ XmlElementType::XmlElementNode | ty @ XmlElementType::XmlAttributeNode => {
                if matches!(ty, XmlElementType::XmlElementNode)
                    && (ctxt.is_null() && !(*node).ns_def.is_null())
                {
                    ns = (*node).ns_def;
                    while {
                        if xml_dom_wrap_ns_norm_add_ns_map_item2(
                            addr_of_mut!(list),
                            addr_of_mut!(size_list),
                            addr_of_mut!(nb_list),
                            ns,
                            ns,
                        ) == -1
                        {
                            // goto internal_error;
                            if !list.is_null() {
                                xml_free(list as _);
                            }
                            return -1;
                        }
                        ns = (*ns).next;
                        !ns.is_null()
                    } {}
                }

                if !(*node).ns.is_null() {
                    /*
                     * Find a mapping.
                     */
                    if !list.is_null() {
                        for (_, j) in (0..nb_list).zip((0..).step_by(2)) {
                            if (*node).ns == *list.add(j) {
                                (*node).ns = *list.add(j + 1);
                                // goto next_node;
                                if let Some(children) = (*node).children().filter(|_| {
                                    matches!((*node).element_type(), XmlElementType::XmlElementNode)
                                }) {
                                    node = children.as_ptr();
                                    continue 'main;
                                }
                                // next_sibling:
                                'next_sibling: loop {
                                    if node.is_null() {
                                        break;
                                    }
                                    if let Some(next) = (*node).next {
                                        node = next.as_ptr();
                                    } else {
                                        node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
                                        // goto next_sibling;
                                        continue 'next_sibling;
                                    }

                                    if node.is_null() {
                                        break 'main;
                                    }

                                    continue 'main;
                                }
                            }
                        }
                    }
                    ns = null_mut();
                    if !ctxt.is_null() {
                        /*
                         * User defined.
                         */
                    } else {
                        /*
                         * Add to doc's oldNs.
                         */
                        ns = xml_dom_wrap_store_ns(doc, (*(*node).ns).href, (*(*node).ns).prefix);
                        if ns.is_null() {
                            // goto internal_error;
                            if !list.is_null() {
                                xml_free(list as _);
                            }
                            return -1;
                        }
                    }
                    if !ns.is_null() {
                        /*
                         * Add mapping.
                         */
                        if xml_dom_wrap_ns_norm_add_ns_map_item2(
                            addr_of_mut!(list),
                            addr_of_mut!(size_list),
                            addr_of_mut!(nb_list),
                            (*node).ns,
                            ns,
                        ) == -1
                        {
                            // goto internal_error;
                            if !list.is_null() {
                                xml_free(list as _);
                            }
                            return -1;
                        }
                    }
                    (*node).ns = ns;
                }
                if matches!((*node).element_type(), XmlElementType::XmlElementNode)
                    && !(*node).properties.is_null()
                {
                    node = (*node).properties as _;
                    continue;
                }
            }
            _ => {
                // goto next_sibling;
                'next_sibling: loop {
                    if node.is_null() {
                        break;
                    }
                    if let Some(next) = (*node).next {
                        node = next.as_ptr();
                    } else {
                        node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
                        // goto next_sibling;
                        continue 'next_sibling;
                    }

                    if node.is_null() {
                        break 'main;
                    }
                    continue 'main;
                }
            }
        }
        // next_node:
        if let Some(children) = (*node)
            .children()
            .filter(|_| matches!((*node).element_type(), XmlElementType::XmlElementNode))
        {
            node = children.as_ptr();
            continue;
        }
        // next_sibling:
        'next_sibling: loop {
            if node.is_null() {
                break;
            }
            if let Some(next) = (*node).next {
                node = next.as_ptr();
            } else {
                node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
                // goto next_sibling;
                continue 'next_sibling;
            }
        }

        if node.is_null() {
            break;
        }
    }

    if !list.is_null() {
        xml_free(list as _);
    }
    0

    // internal_error:
    // if !list.is_null() {
    // 	xml_free(list as _);
    // }
    // return -1;
}

/// Copy a string using a "dict" dictionary in the current scope, if available.
macro_rules! DICT_COPY {
    ($dict:expr, $str:expr, $cpy:expr) => {
        if !$str.is_null() {
            if !$dict.is_null() {
                if xml_dict_owns($dict, $str) != 0 {
                    $cpy = $str;
                } else {
                    $cpy = xml_dict_lookup($dict, $str, -1) as _;
                }
            } else {
                $cpy = xml_strdup($str);
            }
        }
    };
}

/// Copy a string using a "dict" dictionary in the current scope, if available.
macro_rules! DICT_CONST_COPY {
    ($dict:expr, $str:expr, $cpy:expr) => {
        if !$str.is_null() {
            if !$dict.is_null() {
                if xml_dict_owns($dict, $str) != 0 {
                    $cpy = $str;
                } else {
                    $cpy = xml_dict_lookup($dict, $str, -1);
                }
            } else {
                $cpy = xml_strdup($str);
            }
        }
    };
}

/// References of out-of scope ns-decls are remapped to point to @destDoc:
/// 1) If @destParent is given, then nsDef entries on element-nodes are used
/// 2) If *no* @destParent is given, then @(*destDoc).oldNs entries are used.
///    This is the case when you don't know already where the cloned branch
///    will be added to.
///
/// If @destParent is given, it ensures that the tree is namespace
/// wellformed by creating additional ns-decls where needed.
/// Note that, since prefixes of already existent ns-decls can be
/// shadowed by this process, it could break QNames in attribute
/// values or element content.
/// TODO:
///   1) What to do with XInclude? Currently this returns an error for XInclude.
///
/// Returns 0 if the operation succeeded,
///         1 if a node of unsupported (or not yet supported) type was given,
///         -1 on API/internal errors.
#[doc(alias = "xmlDOMWrapCloneNode")]
pub unsafe extern "C" fn xml_dom_wrap_clone_node(
    ctxt: XmlDOMWrapCtxtPtr,
    mut source_doc: XmlDocPtr,
    node: XmlNodePtr,
    res_node: *mut XmlNodePtr,
    dest_doc: XmlDocPtr,
    dest_parent: XmlNodePtr,
    deep: i32,
    _options: i32,
) -> i32 {
    let mut ret: i32 = 0;
    let mut cur: XmlNodePtr;
    let mut cur_elem: XmlNodePtr = null_mut();
    let mut ns_map: XmlNsMapPtr = null_mut();
    let mut mi: XmlNsMapItemPtr;
    let mut ns: XmlNsPtr = null_mut();
    let mut depth: i32 = -1;
    /* let adoptStr: i32 = 1; */
    /* gather @parent's ns-decls. */
    let mut parnsdone: i32 = 0;
    /*
     * @ancestorsOnly:
     * TODO: @ancestorsOnly should be set per option.
     *
     */
    let ancestors_only: i32 = 0;
    let mut result_clone: XmlNodePtr = null_mut();
    let mut clone: XmlNodePtr;
    let mut parent_clone: XmlNodePtr = null_mut();
    let mut prev_clone: XmlNodePtr = null_mut();
    let mut clone_ns: XmlNsPtr;
    let mut clone_ns_def_slot: *mut XmlNsPtr;
    /* The destination dict */

    if node.is_null() || res_node.is_null() || dest_doc.is_null() {
        return -1;
    }
    /*
     * TODO: Initially we support only element-nodes.
     */
    if !matches!((*node).element_type(), XmlElementType::XmlElementNode) {
        return 1;
    }
    /*
     * Check (*node).doc sanity.
     */
    if !(*node).doc.is_null() && !source_doc.is_null() && (*node).doc != source_doc {
        /*
        	* Might be an XIncluded node.
        	*/
        return -1;
    }
    if source_doc.is_null() {
        source_doc = (*node).doc;
    }
    if source_doc.is_null() {
        return -1;
    }

    let dict: XmlDictPtr = (*dest_doc).dict;
    /*
     * Reuse the namespace map of the context.
     */
    if !ctxt.is_null() {
        ns_map = (*ctxt).namespace_map as _;
    }

    *res_node = null_mut();

    cur = node;
    if !cur.is_null() && matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
        return -1;
    }

    'exit: {
        'internal_error: {
            'main: while !cur.is_null() {
                if (*cur).doc != source_doc {
                    /*
                     * We'll assume XIncluded nodes if the doc differs.
                     * TODO: Do we need to reconciliate XIncluded nodes?
                     * TODO: This here returns -1 in this case.
                     */
                    break 'internal_error;
                }
                /*
                 * Create a new node.
                 */
                match (*cur).element_type() {
                    XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
                        /*
                         * TODO: What to do with XInclude?
                         */
                        break 'internal_error;
                    }
                    XmlElementType::XmlElementNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlCDATASectionNode
                    | XmlElementType::XmlCommentNode
                    | XmlElementType::XmlPINode
                    | XmlElementType::XmlDocumentFragNode
                    | XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode => {
                        // Nodes of xmlNode structure.
                        clone = xml_malloc(size_of::<XmlNode>()) as _;
                        if clone.is_null() {
                            xml_tree_err_memory("xmlDOMWrapCloneNode(): allocating a node");
                            break 'internal_error;
                        }
                        std::ptr::write(&mut *clone, XmlNode::default());
                        // Set hierarchical links.
                        if !result_clone.is_null() {
                            (*clone).set_parent(NodePtr::from_ptr(parent_clone));
                            if !prev_clone.is_null() {
                                (*prev_clone).next = NodePtr::from_ptr(clone);
                                (*clone).prev = NodePtr::from_ptr(prev_clone);
                            } else {
                                (*parent_clone).set_children(NodePtr::from_ptr(clone));
                            }
                        } else {
                            result_clone = clone;
                        }
                    }
                    XmlElementType::XmlAttributeNode => {
                        // Attributes (xmlAttr).
                        // Use xmlRealloc to avoid -Warray-bounds warning
                        clone = xml_realloc(null_mut(), size_of::<XmlAttr>()) as _;
                        if clone.is_null() {
                            xml_tree_err_memory("xmlDOMWrapCloneNode(): allocating an attr-node");
                            break 'internal_error;
                        }
                        memset(clone as _, 0, size_of::<XmlAttr>());
                        // Set hierarchical links.
                        // TODO: Change this to add to the end of attributes.
                        if !result_clone.is_null() {
                            (*clone).set_parent(NodePtr::from_ptr(parent_clone));
                            if !prev_clone.is_null() {
                                (*prev_clone).next = NodePtr::from_ptr(clone);
                                (*clone).prev = NodePtr::from_ptr(prev_clone);
                            } else {
                                (*parent_clone).properties = clone as _;
                            }
                        } else {
                            result_clone = clone;
                        }
                    }
                    _ => {
                        // TODO QUESTION: Any other nodes expected?
                        break 'internal_error;
                    }
                }

                (*clone).typ = (*cur).element_type();
                (*clone).doc = dest_doc;

                // Clone the name of the node if any.
                if (*cur).name == XML_STRING_TEXT.as_ptr() as _ {
                    (*clone).name = XML_STRING_TEXT.as_ptr() as _;
                } else if (*cur).name == XML_STRING_TEXT_NOENC.as_ptr() as _ {
                    // NOTE: Although xmlStringTextNoenc is never assigned to a node
                    //   in tree.c, it might be set in Libxslt via
                    //   "xsl:disable-output-escaping".
                    (*clone).name = XML_STRING_TEXT_NOENC.as_ptr() as _;
                } else if (*cur).name == XML_STRING_COMMENT.as_ptr() as _ {
                    (*clone).name = XML_STRING_COMMENT.as_ptr() as _;
                } else if !(*cur).name.is_null() {
                    DICT_CONST_COPY!(dict, (*cur).name, (*clone).name);
                }

                let mut leave_node = false;
                match (*cur).element_type() {
                    XmlElementType::XmlXIncludeStart | XmlElementType::XmlXIncludeEnd => {
                        // TODO
                        return -1;
                    }
                    XmlElementType::XmlElementNode => {
                        cur_elem = cur;
                        depth += 1;
                        // Namespace declarations.
                        if !(*cur).ns_def.is_null() {
                            if parnsdone == 0 {
                                if !dest_parent.is_null() && ctxt.is_null() {
                                    // Gather @parent's in-scope ns-decls.
                                    if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                        addr_of_mut!(ns_map),
                                        dest_parent,
                                    ) == -1
                                    {
                                        break 'internal_error;
                                    }
                                }
                                parnsdone = 1;
                            }
                            // Clone namespace declarations.
                            clone_ns_def_slot = addr_of_mut!((*clone).ns_def);
                            ns = (*cur).ns_def;
                            while !ns.is_null() {
                                // Create a new xmlNs.
                                clone_ns = xml_malloc(size_of::<XmlNs>()) as _;
                                if clone_ns.is_null() {
                                    xml_tree_err_memory(
                                        "xmlDOMWrapCloneNode(): allocating namespace",
                                    );
                                    return -1;
                                }
                                memset(clone_ns as _, 0, size_of::<XmlNs>());
                                (*clone_ns).typ = XML_LOCAL_NAMESPACE;

                                if !(*ns).href.is_null() {
                                    (*clone_ns).href = xml_strdup((*ns).href);
                                }
                                if !(*ns).prefix.is_null() {
                                    (*clone_ns).prefix = xml_strdup((*ns).prefix);
                                }

                                *clone_ns_def_slot = clone_ns;
                                let mut p = (*clone_ns).next;
                                clone_ns_def_slot = addr_of_mut!(p);

                                // Note that for custom handling of ns-references,
                                // the ns-decls need not be stored in the ns-map,
                                // since they won't be referenced by (*node).ns.
                                if ctxt.is_null() || (*ctxt).get_ns_for_node_func.is_none() {
                                    // Does it shadow any ns-decl?
                                    if XML_NSMAP_NOTEMPTY!(ns_map) {
                                        XML_NSMAP_FOREACH!(ns_map, mi, {
                                            if ((*mi).depth >= XML_TREE_NSMAP_PARENT)
                                                && (*mi).shadow_depth == -1
                                                && ((*ns).prefix == (*(*mi).new_ns).prefix
                                                    || xml_str_equal(
                                                        (*ns).prefix,
                                                        (*(*mi).new_ns).prefix,
                                                    ))
                                            {
                                                /*
                                                 * Mark as shadowed at the current
                                                 * depth.
                                                 */
                                                (*mi).shadow_depth = depth;
                                            }
                                        });
                                    }
                                    // Push mapping.
                                    if xml_dom_wrap_ns_map_add_item(
                                        addr_of_mut!(ns_map),
                                        -1,
                                        ns,
                                        clone_ns,
                                        depth,
                                    )
                                    .is_null()
                                    {
                                        break 'internal_error;
                                    }
                                }
                                ns = (*ns).next;
                            }
                        }
                        /* (*cur).ns will be processed further down. */
                    }
                    XmlElementType::XmlAttributeNode => {
                        /* IDs will be processed further down. */
                        /* (*cur).ns will be processed further down. */
                    }
                    XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode => {
                        /*
                         * Note that this will also cover the values of attributes.
                         */
                        DICT_COPY!(dict, (*cur).content, (*clone).content);
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlEntityNode => {
                        /* TODO: What to do here? */
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlEntityRefNode => {
                        if source_doc != dest_doc {
                            if !(*dest_doc).int_subset.is_null()
                                || !(*dest_doc).ext_subset.is_null()
                            {
                                // Different doc: Assign new entity-node if available.
                                let ent: XmlEntityPtr =
                                    xml_get_doc_entity(dest_doc, &(*cur).name().unwrap());
                                if !ent.is_null() {
                                    (*clone).content = (*ent).content.load(Ordering::Relaxed);
                                    (*clone).set_children(NodePtr::from_ptr(ent as *mut XmlNode));
                                    (*clone).set_last(NodePtr::from_ptr(ent as *mut XmlNode));
                                }
                            }
                        } else {
                            // Same doc: Use the current node's entity declaration and value.
                            (*clone).content = (*cur).content;
                            (*clone).set_children((*cur).children());
                            (*clone).set_last((*cur).last());
                        }
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlPINode => {
                        DICT_COPY!(dict, (*cur).content, (*clone).content);
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlCommentNode => {
                        DICT_COPY!(dict, (*cur).content, (*clone).content);
                        // goto leave_node;
                        leave_node = true;
                    }
                    _ => {
                        break 'internal_error;
                    }
                }

                if !leave_node {
                    if (*cur).ns.is_null() {
                        // goto end_ns_reference;
                    } else {
                        /* handle_ns_reference: */
                        /*
                         ** The following will take care of references to ns-decls ********
                         ** and is intended only for element- and attribute-nodes.
                         **
                         */
                        if parnsdone == 0 {
                            if (!dest_parent.is_null() && ctxt.is_null())
                                && (xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                    addr_of_mut!(ns_map),
                                    dest_parent,
                                ) == -1)
                            {
                                break 'internal_error;
                            }
                            parnsdone = 1;
                        }
                        /*
                         * Adopt ns-references.
                         */
                        let mut end_ns_reference = false;
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            /*
                             * Search for a mapping.
                             */
                            XML_NSMAP_FOREACH!(ns_map, mi, {
                                if (*mi).shadow_depth == -1 && ((*cur).ns == (*mi).old_ns) {
                                    /*
                                     * This is the nice case: a mapping was found.
                                     */
                                    (*clone).ns = (*mi).new_ns;
                                    // goto end_ns_reference;
                                    end_ns_reference = true;
                                    break;
                                }
                            });
                        }

                        if !end_ns_reference {
                            /*
                             * No matching namespace in scope. We need a new one.
                             */
                            if !ctxt.is_null() && (*ctxt).get_ns_for_node_func.is_some() {
                                /*
                                 * User-defined behaviour.
                                 */
                                ns = ((*ctxt).get_ns_for_node_func.unwrap())(
                                    ctxt,
                                    cur,
                                    (*(*cur).ns).href,
                                    (*(*cur).ns).prefix,
                                );
                                /*
                                 * Add user's mapping.
                                 */
                                if xml_dom_wrap_ns_map_add_item(
                                    addr_of_mut!(ns_map),
                                    -1,
                                    (*cur).ns,
                                    ns,
                                    XML_TREE_NSMAP_CUSTOM,
                                )
                                .is_null()
                                {
                                    break 'internal_error;
                                }
                                (*clone).ns = ns;
                            } else {
                                /*
                                 * Acquire a normalized ns-decl and add it to the map.
                                 */
                                if xml_dom_wrap_ns_norm_acquire_normalized_ns(
                                    dest_doc,
                                    /* ns-decls on curElem or on (*destDoc).oldNs */
                                    if !dest_parent.is_null() {
                                        cur_elem
                                    } else {
                                        null_mut()
                                    },
                                    (*cur).ns,
                                    addr_of_mut!(ns),
                                    addr_of_mut!(ns_map),
                                    depth,
                                    /* if we need to search only in the ancestor-axis */
                                    ancestors_only,
                                    /* ns-decls must be prefixed for attributes. */
                                    matches!(
                                        (*cur).element_type(),
                                        XmlElementType::XmlAttributeNode
                                    ) as i32,
                                ) == -1
                                {
                                    break 'internal_error;
                                }
                                (*clone).ns = ns;
                            }
                        }
                    }

                    // end_ns_reference:

                    // Some post-processing.
                    //
                    // Handle ID attributes.
                    if matches!((*clone).element_type(), XmlElementType::XmlAttributeNode)
                        && (*clone).parent().is_some()
                        && xml_is_id(dest_doc, (*clone).parent().unwrap().as_ptr(), clone as _) != 0
                    {
                        let children = (*cur).children();
                        if let Some(id_val) = children.and_then(|c| c.get_string((*cur).doc, 1)) {
                            let id_val = CString::new(id_val).unwrap();
                            if xml_add_id(
                                null_mut(),
                                dest_doc,
                                id_val.as_ptr() as *const u8,
                                cur as _,
                            )
                            .is_null()
                            {
                                // TODO: error message.
                                break 'internal_error;
                            }
                        }
                    }
                    /*
                     **
                     ** The following will traverse the tree **************************
                     **
                     *
                     * Walk the element's attributes before descending into child-nodes.
                     */
                    if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                        && !(*cur).properties.is_null()
                    {
                        prev_clone = null_mut();
                        parent_clone = clone;
                        cur = (*cur).properties as _;
                        continue 'main;
                    }
                    // into_content:
                    // Descend into child-nodes.
                    if let Some(children) = (*cur).children().filter(|_| {
                        deep != 0
                            || matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
                    }) {
                        prev_clone = null_mut();
                        parent_clone = clone;
                        cur = children.as_ptr();
                        continue 'main;
                    }
                }

                // leave_node:
                'leave_node: loop {
                    /*
                     * At this point we are done with the node, its content
                     * and an element-nodes's attribute-nodes.
                     */
                    if cur == node {
                        break 'main;
                    }
                    if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                        || matches!((*cur).element_type(), XmlElementType::XmlXIncludeStart)
                        || matches!((*cur).element_type(), XmlElementType::XmlXIncludeEnd)
                    {
                        // TODO: Do we expect nsDefs on xmlElementType::XML_XINCLUDE_START?
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            // Pop mappings.
                            while !(*ns_map).last.is_null() && (*(*ns_map).last).depth >= depth {
                                XML_NSMAP_POP!(ns_map, mi);
                            }
                            // Unshadow.
                            XML_NSMAP_FOREACH!(ns_map, mi, {
                                if (*mi).shadow_depth >= depth {
                                    (*mi).shadow_depth = -1;
                                }
                            });
                        }
                        depth -= 1;
                    }
                    if let Some(next) = (*cur).next {
                        prev_clone = clone;
                        cur = next.as_ptr();
                    } else if !matches!((*cur).element_type(), XmlElementType::XmlAttributeNode) {
                        // Set (*clone).last.
                        if let Some(mut parent) = (*clone).parent() {
                            parent.set_last(NodePtr::from_ptr(clone));
                        }
                        clone = (*clone).parent().map_or(null_mut(), |p| p.as_ptr());
                        if !clone.is_null() {
                            parent_clone = (*clone).parent().map_or(null_mut(), |p| p.as_ptr());
                        }
                        // Process parent --> next;
                        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                        // goto leave_node;
                        continue 'leave_node;
                    } else {
                        // This is for attributes only.
                        clone = (*clone).parent().map_or(null_mut(), |p| p.as_ptr());
                        parent_clone = (*clone).parent().map_or(null_mut(), |p| p.as_ptr());
                        // Process parent-element --> children.
                        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                        // goto into_content;
                        // Descend into child-nodes.
                        if let Some(children) = (*cur).children().filter(|_| {
                            deep != 0
                                || matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
                        }) {
                            prev_clone = null_mut();
                            parent_clone = clone;
                            cur = children.as_ptr();
                            continue 'main;
                        }
                        continue 'leave_node;
                    }

                    break;
                }
            }
            break 'exit;
        }

        // internal_error:
        ret = -1;
    }
    // exit:
    /*
     * Cleanup.
     */
    if !ns_map.is_null() {
        if !ctxt.is_null() && (*ctxt).namespace_map == ns_map as _ {
            /*
             * Just cleanup the map but don't free.
             */
            if !(*ns_map).first.is_null() {
                if !(*ns_map).pool.is_null() {
                    (*(*ns_map).last).next = (*ns_map).pool;
                }
                (*ns_map).pool = (*ns_map).first;
                (*ns_map).first = null_mut();
            }
        } else {
            xml_dom_wrap_ns_map_free(ns_map);
        }
    }
    /*
     * TODO: Should we try a cleanup of the cloned node in case of a
     * fatal error?
     */
    *res_node = result_clone;
    ret
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
    fn test_xml_copy_doc() {
        #[cfg(any(feature = "libxml_tree", feature = "schema"))]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_recursive in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let recursive = gen_int(n_recursive, 1);

                    let ret_val = xml_copy_doc(doc, recursive);
                    desret_xml_doc_ptr(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_int(n_recursive, recursive, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCopyDoc",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlCopyDoc()");
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_recursive);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_copy_dtd() {
        #[cfg(feature = "libxml_tree")]
        unsafe {
            let mut leaks = 0;

            for n_dtd in 0..GEN_NB_XML_DTD_PTR {
                let mem_base = xml_mem_blocks();
                let dtd = gen_xml_dtd_ptr(n_dtd, 0);

                let ret_val = xml_copy_dtd(dtd);
                desret_xml_dtd_ptr(ret_val);
                des_xml_dtd_ptr(n_dtd, dtd, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCopyDtd",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlCopyDtd()");
                    eprintln!(" {}", n_dtd);
                }
            }
        }
    }

    #[test]
    fn test_xml_copy_namespace() {
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_XML_NS_PTR {
                let mem_base = xml_mem_blocks();
                let cur = gen_xml_ns_ptr(n_cur, 0);

                let ret_val = xml_copy_namespace(cur);
                if !ret_val.is_null() {
                    xml_free_ns(ret_val);
                }
                desret_xml_ns_ptr(ret_val);
                des_xml_ns_ptr(n_cur, cur, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCopyNamespace",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlCopyNamespace()");
                    eprintln!(" {}", n_cur);
                }
            }
        }
    }

    #[test]
    fn test_xml_copy_namespace_list() {
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_XML_NS_PTR {
                let mem_base = xml_mem_blocks();
                let cur = gen_xml_ns_ptr(n_cur, 0);

                let ret_val = xml_copy_namespace_list(cur);
                if !ret_val.is_null() {
                    xml_free_ns_list(ret_val);
                }
                desret_xml_ns_ptr(ret_val);
                des_xml_ns_ptr(n_cur, cur, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCopyNamespaceList",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlCopyNamespaceList()"
                    );
                    eprintln!(" {}", n_cur);
                }
            }
        }
    }

    #[test]
    fn test_xml_copy_node() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_extended in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let node = gen_xml_node_ptr(n_node, 0);
                    let extended = gen_int(n_extended, 1);

                    let ret_val = xml_copy_node(node, extended);
                    desret_xml_node_ptr(ret_val);
                    des_xml_node_ptr(n_node, node, 0);
                    des_int(n_extended, extended, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCopyNode",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlCopyNode()");
                        eprint!(" {}", n_node);
                        eprintln!(" {}", n_extended);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_copy_node_list() {
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let node = gen_xml_node_ptr(n_node, 0);

                let ret_val = xml_copy_node_list(node);
                desret_xml_node_ptr(ret_val);
                des_xml_node_ptr(n_node, node, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCopyNodeList",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlCopyNodeList()");
                    eprintln!(" {}", n_node);
                }
            }
        }
    }

    #[test]
    fn test_xml_copy_prop() {
        unsafe {
            let mut leaks = 0;
            for n_target in 0..GEN_NB_XML_NODE_PTR {
                for n_cur in 0..GEN_NB_XML_ATTR_PTR {
                    let mem_base = xml_mem_blocks();
                    let target = gen_xml_node_ptr(n_target, 0);
                    let cur = gen_xml_attr_ptr(n_cur, 1);

                    let ret_val = xml_copy_prop(target, cur);
                    desret_xml_attr_ptr(ret_val);
                    des_xml_node_ptr(n_target, target, 0);
                    des_xml_attr_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCopyProp",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlCopyProp()");
                        eprint!(" {}", n_target);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_copy_prop_list() {
        unsafe {
            let mut leaks = 0;
            for n_target in 0..GEN_NB_XML_NODE_PTR {
                for n_cur in 0..GEN_NB_XML_ATTR_PTR {
                    let mem_base = xml_mem_blocks();
                    let target = gen_xml_node_ptr(n_target, 0);
                    let cur = gen_xml_attr_ptr(n_cur, 1);

                    let ret_val = xml_copy_prop_list(target, cur);
                    desret_xml_attr_ptr(ret_val);
                    des_xml_node_ptr(n_target, target, 0);
                    des_xml_attr_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCopyPropList",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlCopyPropList()");
                        eprint!(" {}", n_target);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_domwrap_clone_node() {
        unsafe {
            let mut leaks = 0;
            for n_ctxt in 0..GEN_NB_XML_DOMWRAP_CTXT_PTR {
                for n_source_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_res_node in 0..GEN_NB_XML_NODE_PTR_PTR {
                            for n_dest_doc in 0..GEN_NB_XML_DOC_PTR {
                                for n_dest_parent in 0..GEN_NB_XML_NODE_PTR {
                                    for n_deep in 0..GEN_NB_INT {
                                        for n_options in 0..GEN_NB_INT {
                                            let mem_base = xml_mem_blocks();
                                            let ctxt = gen_xml_domwrap_ctxt_ptr(n_ctxt, 0);
                                            let source_doc = gen_xml_doc_ptr(n_source_doc, 1);
                                            let node = gen_xml_node_ptr(n_node, 2);
                                            let res_node = gen_xml_node_ptr_ptr(n_res_node, 3);
                                            let dest_doc = gen_xml_doc_ptr(n_dest_doc, 4);
                                            let dest_parent = gen_xml_node_ptr(n_dest_parent, 5);
                                            let deep = gen_int(n_deep, 6);
                                            let options = gen_int(n_options, 7);

                                            let ret_val = xml_dom_wrap_clone_node(
                                                ctxt,
                                                source_doc,
                                                node,
                                                res_node,
                                                dest_doc,
                                                dest_parent,
                                                deep,
                                                options,
                                            );
                                            desret_int(ret_val);
                                            des_xml_domwrap_ctxt_ptr(n_ctxt, ctxt, 0);
                                            des_xml_doc_ptr(n_source_doc, source_doc, 1);
                                            des_xml_node_ptr(n_node, node, 2);
                                            des_xml_node_ptr_ptr(n_res_node, res_node, 3);
                                            des_xml_doc_ptr(n_dest_doc, dest_doc, 4);
                                            des_xml_node_ptr(n_dest_parent, dest_parent, 5);
                                            des_int(n_deep, deep, 6);
                                            des_int(n_options, options, 7);
                                            reset_last_error();
                                            if mem_base != xml_mem_blocks() {
                                                leaks += 1;
                                                eprint!("Leak of {} blocks found in xmlDOMWrapCloneNode", xml_mem_blocks() - mem_base);
                                                assert!(leaks == 0, "{leaks} Leaks are found in xmlDOMWrapCloneNode()");
                                                eprint!(" {}", n_ctxt);
                                                eprint!(" {}", n_source_doc);
                                                eprint!(" {}", n_node);
                                                eprint!(" {}", n_res_node);
                                                eprint!(" {}", n_dest_doc);
                                                eprint!(" {}", n_dest_parent);
                                                eprint!(" {}", n_deep);
                                                eprintln!(" {}", n_options);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_domwrap_new_ctxt() {

        /* missing type support */
    }

    #[test]
    fn test_xml_domwrap_reconcile_namespaces() {
        unsafe {
            let mut leaks = 0;
            for n_ctxt in 0..GEN_NB_XML_DOMWRAP_CTXT_PTR {
                for n_elem in 0..GEN_NB_XML_NODE_PTR {
                    for n_options in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_domwrap_ctxt_ptr(n_ctxt, 0);
                        let elem = gen_xml_node_ptr(n_elem, 1);
                        let options = gen_int(n_options, 2);

                        let ret_val = xml_dom_wrap_reconcile_namespaces(ctxt, elem, options);
                        desret_int(ret_val);
                        des_xml_domwrap_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_node_ptr(n_elem, elem, 1);
                        des_int(n_options, options, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDOMWrapReconcileNamespaces",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlDOMWrapReconcileNamespaces()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_elem);
                            eprintln!(" {}", n_options);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_domwrap_remove_node() {
        unsafe {
            let mut leaks = 0;
            for n_ctxt in 0..GEN_NB_XML_DOMWRAP_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_options in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_domwrap_ctxt_ptr(n_ctxt, 0);
                            let doc = gen_xml_doc_ptr(n_doc, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let options = gen_int(n_options, 3);

                            let ret_val = xml_dom_wrap_remove_node(ctxt, doc, node, options);
                            desret_int(ret_val);
                            des_xml_domwrap_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_xml_doc_ptr(n_doc, doc, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_int(n_options, options, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlDOMWrapRemoveNode",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlDOMWrapRemoveNode()"
                                );
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_doc);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_options);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_doc_copy_node() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_extended in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let node = gen_xml_node_ptr(n_node, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let extended = gen_int(n_extended, 2);

                        let ret_val = xml_doc_copy_node(node, doc, extended);
                        desret_xml_node_ptr(ret_val);
                        des_xml_node_ptr(n_node, node, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_int(n_extended, extended, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDocCopyNode",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlDocCopyNode()");
                            eprint!(" {}", n_node);
                            eprint!(" {}", n_doc);
                            eprintln!(" {}", n_extended);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_doc_copy_node_list() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let node = gen_xml_node_ptr(n_node, 1);

                    let ret_val = xml_doc_copy_node_list(doc, node);
                    desret_xml_node_ptr(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_xml_node_ptr(n_node, node, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDocCopyNodeList",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDocCopyNodeList()"
                        );
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_node);
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
    fn test_xml_new_cdata_block() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_xml_doc_ptr(n_doc, 0);
                        let content = gen_const_xml_char_ptr(n_content, 1);
                        let mut len = gen_int(n_len, 2);
                        if !content.is_null() && len > xml_strlen(content) {
                            len = 0;
                        }

                        let ret_val = xml_new_cdata_block(doc, content, len);
                        desret_xml_node_ptr(ret_val);
                        des_xml_doc_ptr(n_doc, doc, 0);
                        des_const_xml_char_ptr(n_content, content, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNewCDataBlock",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlNewCDataBlock()");
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_content);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_child() {
        #[cfg(any(feature = "libxml_tree", feature = "schema"))]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "libxml_tree")]
            {
                for n_parent in 0..GEN_NB_XML_NODE_PTR {
                    for n_ns in 0..GEN_NB_XML_NS_PTR {
                        for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                let mem_base = xml_mem_blocks();
                                let parent = gen_xml_node_ptr(n_parent, 0);
                                let ns = gen_xml_ns_ptr(n_ns, 1);
                                let name = gen_const_xml_char_ptr(n_name, 2);
                                let content = gen_const_xml_char_ptr(n_content, 3);

                                let ret_val = xml_new_child(parent, ns, name, content);
                                desret_xml_node_ptr(ret_val);
                                des_xml_node_ptr(n_parent, parent, 0);
                                des_xml_ns_ptr(n_ns, ns, 1);
                                des_const_xml_char_ptr(n_name, name, 2);
                                des_const_xml_char_ptr(n_content, content, 3);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlNewChild",
                                        xml_mem_blocks() - mem_base
                                    );
                                    assert!(leaks == 0, "{leaks} Leaks are found in xmlNewChild()");
                                    eprint!(" {}", n_parent);
                                    eprint!(" {}", n_ns);
                                    eprint!(" {}", n_name);
                                    eprintln!(" {}", n_content);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_comment() {
        unsafe {
            let mut leaks = 0;
            for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let content = gen_const_xml_char_ptr(n_content, 0);

                let ret_val = xml_new_comment(content as *const XmlChar);
                desret_xml_node_ptr(ret_val);
                des_const_xml_char_ptr(n_content, content, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNewComment",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlNewComment()");
                    eprintln!(" {}", n_content);
                }
            }
        }
    }

    #[test]
    fn test_xml_new_doc_comment() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    let ret_val = xml_new_doc_comment(doc, content);
                    desret_xml_node_ptr(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_char_ptr(n_content, content, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewDocComment",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNewDocComment()");
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_content);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_doc_fragment() {
        #[cfg(feature = "libxml_tree")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                let mem_base = xml_mem_blocks();
                let doc = gen_xml_doc_ptr(n_doc, 0);

                let ret_val = xml_new_doc_fragment(doc);
                desret_xml_node_ptr(ret_val);
                des_xml_doc_ptr(n_doc, doc, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNewDocFragment",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlNewDocFragment()");
                    eprintln!(" {}", n_doc);
                }
            }
        }
    }

    #[test]
    fn test_xml_new_doc_node() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_ns in 0..GEN_NB_XML_NS_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let doc = gen_xml_doc_ptr(n_doc, 0);
                            let ns = gen_xml_ns_ptr(n_ns, 1);
                            let name = gen_const_xml_char_ptr(n_name, 2);
                            let content = gen_const_xml_char_ptr(n_content, 3);

                            let ret_val = xml_new_doc_node(doc, ns, name, content);
                            desret_xml_node_ptr(ret_val);
                            des_xml_doc_ptr(n_doc, doc, 0);
                            des_xml_ns_ptr(n_ns, ns, 1);
                            des_const_xml_char_ptr(n_name, name, 2);
                            des_const_xml_char_ptr(n_content, content, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlNewDocNode",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlNewDocNode()");
                                eprint!(" {}", n_doc);
                                eprint!(" {}", n_ns);
                                eprint!(" {}", n_name);
                                eprintln!(" {}", n_content);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_doc_node_eat_name() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_ns in 0..GEN_NB_XML_NS_PTR {
                    for n_name in 0..GEN_NB_EATEN_NAME {
                        for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let doc = gen_xml_doc_ptr(n_doc, 0);
                            let ns = gen_xml_ns_ptr(n_ns, 1);
                            let name = gen_eaten_name(n_name, 2);
                            let content = gen_const_xml_char_ptr(n_content, 3);

                            let ret_val = xml_new_doc_node_eat_name(doc, ns, name, content);
                            desret_xml_node_ptr(ret_val);
                            des_xml_doc_ptr(n_doc, doc, 0);
                            des_xml_ns_ptr(n_ns, ns, 1);
                            des_eaten_name(n_name, name, 2);
                            des_const_xml_char_ptr(n_content, content, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlNewDocNodeEatName",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlNewDocNodeEatName()"
                                );
                                eprint!(" {}", n_doc);
                                eprint!(" {}", n_ns);
                                eprint!(" {}", n_name);
                                eprintln!(" {}", n_content);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_doc_pi() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_xml_doc_ptr(n_doc, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let content = gen_const_xml_char_ptr(n_content, 2);

                        let ret_val = xml_new_doc_pi(doc, name, content);
                        desret_xml_node_ptr(ret_val);
                        des_xml_doc_ptr(n_doc, doc, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_content, content, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNewDocPI",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlNewDocPI()");
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_content);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_doc_prop() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_xml_doc_ptr(n_doc, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let value = gen_const_xml_char_ptr(n_value, 2);

                        let ret_val = xml_new_doc_prop(doc, name, value);
                        desret_xml_attr_ptr(ret_val);
                        des_xml_doc_ptr(n_doc, doc, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_value, value, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNewDocProp",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlNewDocProp()");
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_value);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_doc_raw_node() {
        #[cfg(feature = "libxml_tree")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_ns in 0..GEN_NB_XML_NS_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let doc = gen_xml_doc_ptr(n_doc, 0);
                            let ns = gen_xml_ns_ptr(n_ns, 1);
                            let name = gen_const_xml_char_ptr(n_name, 2);
                            let content = gen_const_xml_char_ptr(n_content, 3);

                            let ret_val = xml_new_doc_raw_node(doc, ns, name, content);
                            desret_xml_node_ptr(ret_val);
                            des_xml_doc_ptr(n_doc, doc, 0);
                            des_xml_ns_ptr(n_ns, ns, 1);
                            des_const_xml_char_ptr(n_name, name, 2);
                            des_const_xml_char_ptr(n_content, content, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlNewDocRawNode",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlNewDocRawNode()"
                                );
                                eprint!(" {}", n_doc);
                                eprint!(" {}", n_ns);
                                eprint!(" {}", n_name);
                                eprintln!(" {}", n_content);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_doc_text() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_CONST_XML_DOC_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_const_xml_doc_ptr(n_doc, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    let ret_val = xml_new_doc_text(doc, content);
                    desret_xml_node_ptr(ret_val);
                    des_const_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_char_ptr(n_content, content, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewDocText",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNewDocText()");
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_content);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_doc_text_len() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_xml_doc_ptr(n_doc, 0);
                        let content = gen_const_xml_char_ptr(n_content, 1);
                        let mut len = gen_int(n_len, 2);
                        if !content.is_null() && len > xml_strlen(content) {
                            len = 0;
                        }

                        let ret_val = xml_new_doc_text_len(doc, content, len);
                        desret_xml_node_ptr(ret_val);
                        des_xml_doc_ptr(n_doc, doc, 0);
                        des_const_xml_char_ptr(n_content, content, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNewDocTextLen",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlNewDocTextLen()");
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_content);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_node() {
        unsafe {
            let mut leaks = 0;
            for n_ns in 0..GEN_NB_XML_NS_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let ns = gen_xml_ns_ptr(n_ns, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_new_node(ns, name);
                    desret_xml_node_ptr(ret_val);
                    des_xml_ns_ptr(n_ns, ns, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewNode",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNewNode()");
                        eprint!(" {}", n_ns);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_node_eat_name() {
        unsafe {
            let mut leaks = 0;
            for n_ns in 0..GEN_NB_XML_NS_PTR {
                for n_name in 0..GEN_NB_EATEN_NAME {
                    let mem_base = xml_mem_blocks();
                    let ns = gen_xml_ns_ptr(n_ns, 0);
                    let name = gen_eaten_name(n_name, 1);

                    let ret_val = xml_new_node_eat_name(ns, name);
                    desret_xml_node_ptr(ret_val);
                    des_xml_ns_ptr(n_ns, ns, 0);
                    des_eaten_name(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewNodeEatName",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNewNodeEatName()");
                        eprint!(" {}", n_ns);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_ns() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_href in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let node = gen_xml_node_ptr(n_node, 0);
                        let href = gen_const_xml_char_ptr(n_href, 1);
                        let prefix = gen_const_xml_char_ptr(n_prefix, 2);

                        let ret_val = xml_new_ns(node, href, prefix);
                        if node.is_null() && !ret_val.is_null() {
                            xml_free_ns(ret_val);
                        }
                        desret_xml_ns_ptr(ret_val);
                        des_xml_node_ptr(n_node, node, 0);
                        des_const_xml_char_ptr(n_href, href, 1);
                        des_const_xml_char_ptr(n_prefix, prefix, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNewNs",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlNewNs()");
                            eprint!(" {}", n_node);
                            eprint!(" {}", n_href);
                            eprintln!(" {}", n_prefix);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_ns_prop() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_ns in 0..GEN_NB_XML_NS_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let node = gen_xml_node_ptr(n_node, 0);
                            let ns = gen_xml_ns_ptr(n_ns, 1);
                            let name = gen_const_xml_char_ptr(n_name, 2);
                            let value = gen_const_xml_char_ptr(n_value, 3);

                            let ret_val = xml_new_ns_prop(node, ns, name, value);
                            desret_xml_attr_ptr(ret_val);
                            des_xml_node_ptr(n_node, node, 0);
                            des_xml_ns_ptr(n_ns, ns, 1);
                            des_const_xml_char_ptr(n_name, name, 2);
                            des_const_xml_char_ptr(n_value, value, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlNewNsProp",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlNewNsProp()");
                                eprint!(" {}", n_node);
                                eprint!(" {}", n_ns);
                                eprint!(" {}", n_name);
                                eprintln!(" {}", n_value);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_ns_prop_eat_name() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_ns in 0..GEN_NB_XML_NS_PTR {
                    for n_name in 0..GEN_NB_EATEN_NAME {
                        for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let node = gen_xml_node_ptr(n_node, 0);
                            let ns = gen_xml_ns_ptr(n_ns, 1);
                            let name = gen_eaten_name(n_name, 2);
                            let value = gen_const_xml_char_ptr(n_value, 3);

                            let ret_val = xml_new_ns_prop_eat_name(node, ns, name, value);
                            desret_xml_attr_ptr(ret_val);
                            des_xml_node_ptr(n_node, node, 0);
                            des_xml_ns_ptr(n_ns, ns, 1);
                            des_eaten_name(n_name, name, 2);
                            des_const_xml_char_ptr(n_value, value, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlNewNsPropEatName",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlNewNsPropEatName()"
                                );
                                eprint!(" {}", n_node);
                                eprint!(" {}", n_ns);
                                eprint!(" {}", n_name);
                                eprintln!(" {}", n_value);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_pi() {
        unsafe {
            let mut leaks = 0;
            for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let name = gen_const_xml_char_ptr(n_name, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    let ret_val = xml_new_pi(name as *const XmlChar, content);
                    desret_xml_node_ptr(ret_val);
                    des_const_xml_char_ptr(n_name, name, 0);
                    des_const_xml_char_ptr(n_content, content, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewPI",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNewPI()");
                        eprint!(" {}", n_name);
                        eprintln!(" {}", n_content);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_prop() {
        #[cfg(any(feature = "libxml_tree", feature = "html", feature = "schema"))]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "libxml_tree")]
            {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let node = gen_xml_node_ptr(n_node, 0);
                            let name = gen_const_xml_char_ptr(n_name, 1);
                            let value = gen_const_xml_char_ptr(n_value, 2);

                            let ret_val = xml_new_prop(node, name, value);
                            desret_xml_attr_ptr(ret_val);
                            des_xml_node_ptr(n_node, node, 0);
                            des_const_xml_char_ptr(n_name, name, 1);
                            des_const_xml_char_ptr(n_value, value, 2);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlNewProp",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlNewProp()");
                                eprint!(" {}", n_node);
                                eprint!(" {}", n_name);
                                eprintln!(" {}", n_value);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_text() {
        unsafe {
            let mut leaks = 0;
            for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let content = gen_const_xml_char_ptr(n_content, 0);

                let ret_val = xml_new_text(content as *const XmlChar);
                desret_xml_node_ptr(ret_val);
                des_const_xml_char_ptr(n_content, content, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNewText",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlNewText()");
                    eprintln!(" {}", n_content);
                }
            }
        }
    }

    #[test]
    fn test_xml_new_text_child() {
        #[cfg(feature = "libxml_tree")]
        unsafe {
            let mut leaks = 0;

            for n_parent in 0..GEN_NB_XML_NODE_PTR {
                for n_ns in 0..GEN_NB_XML_NS_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let parent = gen_xml_node_ptr(n_parent, 0);
                            let ns = gen_xml_ns_ptr(n_ns, 1);
                            let name = gen_const_xml_char_ptr(n_name, 2);
                            let content = gen_const_xml_char_ptr(n_content, 3);

                            let ret_val = xml_new_text_child(parent, ns, name, content);
                            desret_xml_node_ptr(ret_val);
                            des_xml_node_ptr(n_parent, parent, 0);
                            des_xml_ns_ptr(n_ns, ns, 1);
                            des_const_xml_char_ptr(n_name, name, 2);
                            des_const_xml_char_ptr(n_content, content, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlNewTextChild",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlNewTextChild()");
                                eprint!(" {}", n_parent);
                                eprint!(" {}", n_ns);
                                eprint!(" {}", n_name);
                                eprintln!(" {}", n_content);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_text_len() {
        unsafe {
            let mut leaks = 0;
            for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_len in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let content = gen_const_xml_char_ptr(n_content, 0);
                    let mut len = gen_int(n_len, 1);
                    if !content.is_null() && len > xml_strlen(content) {
                        len = 0;
                    }

                    let ret_val = xml_new_text_len(content as *const XmlChar, len);
                    desret_xml_node_ptr(ret_val);
                    des_const_xml_char_ptr(n_content, content, 0);
                    des_int(n_len, len, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewTextLen",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNewTextLen()");
                        eprint!(" {}", n_content);
                        eprintln!(" {}", n_len);
                    }
                }
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
    fn test_xml_text_concat() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let node = gen_xml_node_ptr(n_node, 0);
                        let content = gen_const_xml_char_ptr(n_content, 1);
                        let mut len = gen_int(n_len, 2);
                        if !content.is_null() && len > xml_strlen(content) {
                            len = 0;
                        }

                        let ret_val = xml_text_concat(node, content, len);
                        desret_int(ret_val);
                        des_xml_node_ptr(n_node, node, 0);
                        des_const_xml_char_ptr(n_content, content, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlTextConcat",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlTextConcat()");
                            eprint!(" {}", n_node);
                            eprint!(" {}", n_content);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_ncname() {
        #[cfg(any(
            feature = "libxml_tree",
            feature = "xpath",
            feature = "schema",
            feature = "libxml_debug"
        ))]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "libxml_tree")]
            {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_space in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let value = gen_const_xml_char_ptr(n_value, 0);
                        let space = gen_int(n_space, 1);

                        let ret_val = xml_validate_ncname(value as *const XmlChar, space);
                        desret_int(ret_val);
                        des_const_xml_char_ptr(n_value, value, 0);
                        des_int(n_space, space, 1);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlValidateNCName",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlValidateNCName()");
                            eprint!(" {}", n_value);
                            eprintln!(" {}", n_space);
                        }
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

    #[test]
    fn test_xml_validate_name() {
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

                        let ret_val = xml_validate_name(value as *const XmlChar, space);
                        desret_int(ret_val);
                        des_const_xml_char_ptr(n_value, value, 0);
                        des_int(n_space, space, 1);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlValidateName",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlValidateName()");
                            eprint!(" {}", n_value);
                            eprintln!(" {}", n_space);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_qname() {
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

                        let ret_val = xml_validate_qname(value as *const XmlChar, space);
                        desret_int(ret_val);
                        des_const_xml_char_ptr(n_value, value, 0);
                        des_int(n_space, space, 1);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlValidateQName",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlValidateQName()");
                            eprint!(" {}", n_value);
                            eprintln!(" {}", n_space);
                        }
                    }
                }
            }
        }
    }
}

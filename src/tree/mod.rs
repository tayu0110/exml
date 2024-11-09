//! Provide methods and data structures for tree manipulation.  
//! This module is based on `libxml/tree.h`, `tree.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

mod attribute;
mod document;
mod dtd;
#[cfg(feature = "output")]
mod dump;
mod id;
mod namespace;
mod node;

use std::{
    any::type_name,
    ffi::{c_char, CStr},
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::{AtomicI32, Ordering},
};

use libc::{memcpy, memset, snprintf, strlen};

pub(crate) use crate::buf::libxml_api::*;
use crate::{
    error::{XmlErrorDomain, XmlParserErrors, __xml_simple_error},
    libxml::{
        chvalid::xml_is_blank_char,
        dict::{xml_dict_free, xml_dict_lookup, xml_dict_owns, XmlDictPtr},
        entities::{
            xml_encode_entities_reentrant, xml_free_entities_table, xml_get_doc_entity,
            XmlEntityPtr, XmlEntityType,
        },
        globals::{
            xml_deregister_node_default_value, xml_free, xml_malloc, xml_malloc_atomic,
            xml_realloc, xml_register_node_default_value,
        },
        parser_internals::{
            xml_copy_char_multi_byte, XML_STRING_COMMENT, XML_STRING_TEXT, XML_STRING_TEXT_NOENC,
        },
        valid::{
            xml_add_id, xml_free_attribute_table, xml_free_element_table, xml_free_notation_table,
            xml_is_id, XmlAttributeTablePtr, XmlElementTablePtr, XmlNotationTablePtr,
        },
        valid::{xml_free_id_table, xml_free_ref_table, xml_get_dtd_qattr_desc, xml_remove_id},
        xmlstring::{
            xml_str_equal, xml_strcat, xml_strdup, xml_strlen, xml_strncat, xml_strncat_new,
            xml_strndup, XmlChar,
        },
    },
    private::{
        buf::{
            xml_buf_add, xml_buf_cat, xml_buf_create, xml_buf_create_size, xml_buf_detach,
            xml_buf_free, xml_buf_is_empty, xml_buf_set_allocation_scheme,
        },
        entities::{xml_encode_attribute_entities, XML_ENT_EXPANDING, XML_ENT_PARSED},
    },
};

pub use attribute::*;
pub use document::*;
pub use dtd::*;
pub use id::*;
pub use namespace::*;
pub use node::*;

/**
 * BASE_BUFFER_SIZE:
 *
 * default buffer size 4000.
 */
pub const BASE_BUFFER_SIZE: usize = 4096;

/**
 *    xmlBufferAllocationScheme:
 *
 * A buffer allocation scheme can be defined to either match exactly the
 * need or double it's allocated size each time it is found too small.
 */
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

/**
 * XML_XML_NAMESPACE:
 *
 * This is the namespace for the special xml: prefix predefined in the
 * XML Namespace specification.
 */
pub const XML_XML_NAMESPACE: &CStr = c"http://www.w3.org/XML/1998/namespace";

/**
 * XML_XML_ID:
 *
 * This is the name for the special xml:id attribute
 */
pub const XML_XML_ID: *const XmlChar = c"xml:id".as_ptr() as _;

/*
 * The different element types carried by an XML tree.
 *
 * NOTE: This is synchronized with DOM Level1 values
 *       See http://www.w3.org/TR/REC-DOM-Level-1/
 *
 * Actually this had diverged a bit, and now XML_DOCUMENT_TYPE_NODE should
 * be deprecated to use an XML_DTD_NODE.
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlElementType {
    #[default]
    XmlInvalidNode = 0, // for std::mem::zeroed(). This is invalid value.
    XmlElementNode = 1,
    XmlAttributeNode = 2,
    XmlTextNode = 3,
    XmlCdataSectionNode = 4,
    XmlEntityRefNode = 5,
    XmlEntityNode = 6,
    XmlPiNode = 7,
    XmlCommentNode = 8,
    XmlDocumentNode = 9,
    XmlDocumentTypeNode = 10,
    XmlDocumentFragNode = 11,
    XmlNotationNode = 12,
    XmlHtmlDocumentNode = 13,
    XmlDtdNode = 14,
    XmlElementDecl = 15,
    XmlAttributeDecl = 16,
    XmlEntityDecl = 17,
    XmlNamespaceDecl = 18,
    XmlXincludeStart = 19,
    XmlXincludeEnd = 20,
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
        } else if value == Self::XmlCdataSectionNode as i32 {
            Ok(Self::XmlCdataSectionNode)
        } else if value == Self::XmlEntityRefNode as i32 {
            Ok(Self::XmlEntityRefNode)
        } else if value == Self::XmlEntityNode as i32 {
            Ok(Self::XmlEntityNode)
        } else if value == Self::XmlPiNode as i32 {
            Ok(Self::XmlPiNode)
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
        } else if value == Self::XmlHtmlDocumentNode as i32 {
            Ok(Self::XmlHtmlDocumentNode)
        } else if value == Self::XmlDtdNode as i32 {
            Ok(Self::XmlDtdNode)
        } else if value == Self::XmlElementDecl as i32 {
            Ok(Self::XmlElementDecl)
        } else if value == Self::XmlAttributeDecl as i32 {
            Ok(Self::XmlAttributeDecl)
        } else if value == Self::XmlEntityDecl as i32 {
            Ok(Self::XmlEntityDecl)
        } else if value == Self::XmlNamespaceDecl as i32 {
            Ok(Self::XmlNamespaceDecl)
        } else if value == Self::XmlXincludeStart as i32 {
            Ok(Self::XmlXincludeStart)
        } else if value == Self::XmlXincludeEnd as i32 {
            Ok(Self::XmlXincludeEnd)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

/**
 * xmlAttributeType:
 *
 * A DTD Attribute type definition.
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlAttributeType {
    XmlAttributeCdata = 1,
    XmlAttributeId,
    XmlAttributeIdref,
    XmlAttributeIdrefs,
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
        if value == Self::XmlAttributeCdata as i32 {
            Ok(Self::XmlAttributeCdata)
        } else if value == Self::XmlAttributeId as i32 {
            Ok(Self::XmlAttributeId)
        } else if value == Self::XmlAttributeIdref as i32 {
            Ok(Self::XmlAttributeIdref)
        } else if value == Self::XmlAttributeIdrefs as i32 {
            Ok(Self::XmlAttributeIdrefs)
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

/**
 * xmlAttributeDefault:
 *
 * A DTD Attribute default definition.
 */

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

/**
 * xmlElementContentType:
 *
 * Possible definitions of element content types.
 */
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum XmlElementContentType {
    XmlElementContentPcdata = 1,
    XmlElementContentElement,
    XmlElementContentSeq,
    XmlElementContentOr,
}

/**
 * xmlElementContentOccur:
 *
 * Possible definitions of element content occurrences.
 */
#[repr(C)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum XmlElementContentOccur {
    XmlElementContentOnce = 1,
    XmlElementContentOpt,
    XmlElementContentMult,
    XmlElementContentPlus,
}

/**
 * xmlElementContent:
 *
 * An XML Element content as stored after parsing an element definition
 * in a DTD.
 */
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

/**
 * xmlElementTypeVal:
 *
 * The different possibilities for an element content type.
 */

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

/**
 * XML_LOCAL_NAMESPACE:
 *
 * A namespace declaration node.
 */
pub(crate) const XML_LOCAL_NAMESPACE: XmlElementType = XmlElementType::XmlNamespaceDecl;
pub type XmlNsType = XmlElementType;

/**
 * xmlDocProperty
 *
 * Set of properties of the document as found by the parser
 * Some of them are linked to similarly named xmlParserOption
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlDocProperties {
    XmlDocWellformed = 1 << 0, /* document is XML well formed */
    XmlDocNsvalid = 1 << 1,    /* document is Namespace valid */
    XmlDocOld10 = 1 << 2,      /* parsed with old XML-1.0 parser */
    XmlDocDtdvalid = 1 << 3,   /* DTD validation was successful */
    XmlDocXinclude = 1 << 4,   /* XInclude substitution was done */
    XmlDocUserbuilt = 1 << 5,  /* Document was built using the API
                               and not by parsing an instance */
    XmlDocInternal = 1 << 6, /* built for internal processing */
    XmlDocHtml = 1 << 7,     /* parsed or built HTML document */
}

/**
 * xmlDOMWrapAcquireNsFunction:
 * @ctxt:  a DOM wrapper context
 * @node:  the context node (element or attribute)
 * @nsName:  the requested namespace name
 * @nsPrefix:  the requested namespace prefix
 *
 * A function called to acquire namespaces (xmlNs) from the wrapper.
 *
 * Returns an xmlNsPtr or NULL in case of an error.
 */
pub type XmlDOMWrapAcquireNsFunction = unsafe extern "C" fn(
    ctxt: XmlDOMWrapCtxtPtr,
    node: XmlNodePtr,
    nsName: *const XmlChar,
    nsPrefix: *const XmlChar,
) -> XmlNsPtr;

/**
 * xmlDOMWrapCtxt:
 *
 * Context for DOM wrapper-operations.
 */
pub type XmlDOMWrapCtxtPtr = *mut XmlDOMWrapCtxt;
#[repr(C)]
pub struct XmlDOMWrapCtxt {
    _private: *mut c_void,
    /*
     * The type of this context, just in case we need specialized
     * contexts in the future.
     */
    typ: i32,
    /*
     * Internal namespace map used for various operations.
     */
    namespace_map: *mut c_void,
    /*
     * Use this one to acquire an xmlNsPtr intended for node->ns.
     * (Note that this is not intended for elem->nsDef).
     */
    get_ns_for_node_func: Option<XmlDOMWrapAcquireNsFunction>,
}

pub type XmlNsMapItemPtr = *mut XmlNsMapItem;
#[repr(C)]
pub struct XmlNsMapItem {
    next: XmlNsMapItemPtr,
    prev: XmlNsMapItemPtr,
    old_ns: XmlNsPtr,  /* old ns decl reference */
    new_ns: XmlNsPtr,  /* new ns decl reference */
    shadow_depth: i32, /* Shadowed at this depth */
    /*
     * depth:
     * >= 0 == @node's ns-decls
     * -1   == @parent's ns-decls
     * -2   == the (*doc).oldNs XML ns-decl
     * -3   == the (*doc).oldNs storage ns-decls
     * -4   == ns-decls provided via custom ns-handling
     */
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

/*
 * Some helper functions
 */
/**
 * xmlValidateNCName:
 * @value: the value to check
 * @space: allow spaces in front and end of the string
 *
 * Check that a value conforms to the lexical space of NCName
 *
 * Returns 0 if this validates, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
#[cfg(any(
    feature = "tree",
    feature = "xpath",
    feature = "schema",
    feature = "html",
    feature = "sax1",
    feature = "writer",
    feature = "legacy"
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

/**
 * xmlValidateQName:
 * @value: the value to check
 * @space: allow spaces in front and end of the string
 *
 * Check that a value conforms to the lexical space of QName
 *
 * Returns 0 if this validates, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
#[cfg(any(feature = "tree", feature = "schema"))]
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

/**
 * xmlValidateName:
 * @value: the value to check
 * @space: allow spaces in front and end of the string
 *
 * Check that a value conforms to the lexical space of Name
 *
 * Returns 0 if this validates, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
#[cfg(any(feature = "tree", feature = "schema"))]
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

/**
 * xmlValidateNMToken:
 * @value: the value to check
 * @space: allow spaces in front and end of the string
 *
 * Check that a value conforms to the lexical space of NMToken
 *
 * Returns 0 if this validates, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
#[cfg(any(feature = "tree", feature = "schema"))]
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

/**
 * xmlTreeErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
unsafe extern "C" fn xml_tree_err_memory(extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromTree,
        XmlParserErrors::XmlErrNoMemory,
        null_mut(),
        null(),
        extra,
    );
}

/**
 * xmlBuildQName:
 * @ncname:  the Name
 * @prefix:  the prefix
 * @memory:  preallocated memory
 * @len:  preallocated memory length
 *
 * Builds the QName @prefix:@ncname in @memory if there is enough space
 * and prefix is not NULL nor empty, otherwise allocate a new string.
 * If prefix is NULL or empty it returns ncname.
 *
 * Returns the new string which must be freed by the caller if different from
 *         @memory and @ncname or NULL in case of error
 */
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
            xml_tree_err_memory(c"building QName".as_ptr() as _);
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

/**
 * xmlSplitQName2:
 * @name:  the full QName
 * @prefix:  a XmlChar **
 *
 * parse an XML qualified name string
 *
 * [NS 5] QName ::= (Prefix ':')? LocalPart
 *
 * [NS 6] Prefix ::= NCName
 *
 * [NS 7] LocalPart ::= NCName
 *
 * Returns NULL if the name doesn't have a prefix. Otherwise, returns the
 * local part, and prefix is updated to get the Prefix. Both the return value
 * and the prefix must be freed by the caller.
 */
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
        xml_tree_err_memory(c"QName split".as_ptr() as _);
        return null_mut();
    }
    let ret: *mut XmlChar = xml_strdup(name.add(len as usize + 1) as _) as _;
    if ret.is_null() {
        xml_tree_err_memory(c"QName split".as_ptr() as _);
        if !(*prefix).is_null() {
            xml_free(*prefix as _);
            *prefix = null_mut();
        }
        return null_mut();
    }

    ret
}

/**
 * xmlSplitQName3:
 * @name:  the full QName
 * @len: an int *
 *
 * parse an XML qualified name string,i
 *
 * returns NULL if it is not a Qualified Name, otherwise, update len
 *         with the length in byte of the prefix and return a pointer
 *         to the start of the name without the prefix
 */
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

/*
 * Creating/freeing new structures.
 */

/**
 * DICT_FREE:
 * @str:  a string
 *
 * Free a string if it is not owned by the "dict" dictionary in the
 * current scope
 */
macro_rules! DICT_FREE {
    ($dict:expr, $str:expr) => {
        if !$str.is_null()
            && ($dict.is_null() || crate::libxml::dict::xml_dict_owns($dict, $str as _) == 0)
        {
            xml_free($str as _);
        }
    };
}

/**
 * xmlFreeDtd:
 * @cur:  the DTD structure to free up
 *
 * Free a DTD structure.
 */
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

    if !(*cur).children.is_null() {
        let mut next: XmlNodePtr;
        let mut c: XmlNodePtr = (*cur).children;

        /*
         * Cleanup all nodes which are not part of the specific lists
         * of notations, elements, attributes and entities.
         */
        while !c.is_null() {
            next = (*c).next;
            if !matches!(
                (*c).typ,
                XmlElementType::XmlNotationNode
                    | XmlElementType::XmlElementDecl
                    | XmlElementType::XmlAttributeDecl
                    | XmlElementType::XmlEntityDecl
            ) {
                (*c).unlink();
                xml_free_node(c);
            }
            c = next;
        }
    }
    DICT_FREE!(dict, (*cur).name);
    DICT_FREE!(dict, (*cur).system_id);
    DICT_FREE!(dict, (*cur).external_id);
    /* TODO !!! */
    if !(*cur).notations.is_null() {
        xml_free_notation_table((*cur).notations as XmlNotationTablePtr);
    }

    if !(*cur).elements.is_null() {
        xml_free_element_table((*cur).elements as XmlElementTablePtr);
    }
    if !(*cur).attributes.is_null() {
        xml_free_attribute_table((*cur).attributes as XmlAttributeTablePtr);
    }
    if !(*cur).entities.is_null() {
        xml_free_entities_table((*cur).entities as _);
    }
    if !(*cur).pentities.is_null() {
        xml_free_entities_table((*cur).pentities as _);
    }

    xml_free(cur as _);
}

/**
 * xmlFreeDoc:
 * @cur:  pointer to the document
 *
 * Free up all the structures used by a document, tree included.
 */
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

    if !(*cur).children.is_null() {
        xml_free_node_list((*cur).children);
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

    if !node.is_null() && !matches!((*node).typ, XmlElementType::XmlElementNode) {
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
        xml_tree_err_memory(c"building attribute".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlAttr>());
    (*cur).typ = XmlElementType::XmlAttributeNode;

    (*cur).parent = node;
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
        let mut tmp: XmlNodePtr;

        (*cur).children = xml_new_doc_text(doc, value);
        (*cur).last = null_mut();
        tmp = (*cur).children;
        while !tmp.is_null() {
            (*tmp).parent = cur as _;
            if (*tmp).next.is_null() {
                (*cur).last = tmp;
            }
            tmp = (*tmp).next;
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

/**
 * xmlFreePropList:
 * @cur:  the first property in the list
 *
 * Free a property and all its siblings, all the children are freed too.
 */
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

/**
 * xmlFreeProp:
 * @cur:  an attribute
 *
 * Free one attribute, all the content is freed too
 */
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
    if !(*cur).doc.is_null() && matches!((*cur).atype, Some(XmlAttributeType::XmlAttributeId)) {
        xml_remove_id((*cur).doc, cur);
    }
    if !(*cur).children.is_null() {
        xml_free_node_list((*cur).children);
    }
    DICT_FREE!(dict, (*cur).name);
    xml_free(cur as _);
}

/**
 * xmlNewReconciledNs:
 * @doc:  the document
 * @tree:  a node expected to hold the new namespace
 * @ns:  the original namespace
 *
 * This function tries to locate a namespace definition in a tree
 * ancestors, or create a new namespace definition node similar to
 * @ns trying to reuse the same prefix. However if the given prefix is
 * null (default namespace) or reused within the subtree defined by
 * @tree or on one of its ancestors then a new prefix is generated.
 * Returns the (new) namespace definition or null_mut() in case of error
 */
unsafe extern "C" fn xml_new_reconciled_ns(
    doc: XmlDocPtr,
    tree: XmlNodePtr,
    ns: XmlNsPtr,
) -> XmlNsPtr {
    let mut def: XmlNsPtr;
    let mut prefix: [XmlChar; 50] = [0; 50];
    let mut counter: i32 = 1;

    if tree.is_null() || !matches!((*tree).typ, XmlElementType::XmlElementNode) {
        return null_mut();
    }
    if ns.is_null() || !matches!((*ns).typ, Some(XmlElementType::XmlNamespaceDecl)) {
        return null_mut();
    }
    /*
     * Search an existing namespace definition inherited.
     */
    def = xml_search_ns_by_href(doc, tree, (*ns).href.load(Ordering::Relaxed));
    if !def.is_null() {
        return def;
    }

    /*
     * Find a close prefix which is not already in use.
     * Let's strip namespace prefixes longer than 20 chars !
     */
    if (*ns).prefix.load(Ordering::Relaxed).is_null() {
        snprintf(
            prefix.as_mut_ptr() as _,
            prefix.len(),
            c"default".as_ptr() as _,
        );
    } else {
        snprintf(
            prefix.as_mut_ptr() as _,
            prefix.len(),
            c"%.20s".as_ptr() as _,
            (*ns).prefix.load(Ordering::Relaxed) as *const c_char,
        );
    }

    def = xml_search_ns(doc, tree, prefix.as_ptr() as _);
    while !def.is_null() {
        if counter > 1000 {
            return null_mut();
        }
        if (*ns).prefix.load(Ordering::Relaxed).is_null() {
            snprintf(
                prefix.as_mut_ptr() as _,
                prefix.len(),
                c"default%d".as_ptr() as _,
                counter,
            );
            counter += 1;
        } else {
            snprintf(
                prefix.as_mut_ptr() as _,
                prefix.len(),
                c"%.20s%d".as_ptr() as _,
                (*ns).prefix.load(Ordering::Relaxed) as *const c_char,
                counter,
            );
            counter += 1;
        }
        def = xml_search_ns(doc, tree, prefix.as_ptr() as _);
    }

    /*
     * OK, now we are ready to create a new one.
     */
    def = xml_new_ns(tree, (*ns).href.load(Ordering::Relaxed), prefix.as_ptr());
    def
}

/*
 * NOTE about the CopyNode operations !
 *
 * They are split into external and internal parts for one
 * tricky reason: namespaces. Doing a direct copy of a node
 * say RPM:Copyright without changing the namespace pointer to
 * something else can produce stale links. One way to do it is
 * to keep a reference counter but this doesn't work as soon
 * as one moves the element or the subtree out of the scope of
 * the existing namespace. The actual solution seems to be to add
 * a copy of the namespace at the top of the copied tree if
 * not available in the subtree.
 * Hence two functions, the public front-end call the inner ones
 * The argument "recursive" normally indicates a recursive copy
 * of the node with values 0 (no) and 1 (yes).  For XInclude,
 * however, we allow a value of 2 to indicate copy properties and
 * namespace info, but don't recurse on children.
 */
pub(crate) unsafe extern "C" fn xml_static_copy_node(
    node: XmlNodePtr,
    doc: XmlDocPtr,
    parent: XmlNodePtr,
    extended: i32,
) -> XmlNodePtr {
    if node.is_null() {
        return null_mut();
    }
    match (*node).typ {
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlElementNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => {}
        XmlElementType::XmlAttributeNode => {
            return xml_copy_prop_internal(doc, parent, node as _) as _;
        }
        XmlElementType::XmlNamespaceDecl => {
            return xml_copy_namespace_list(node as _) as _;
        }

        XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode => {
            #[cfg(feature = "tree")]
            {
                return xml_copy_doc(node as _, extended) as _;
            }
        }
        XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl => {
            return null_mut();
        }
        _ => unreachable!(),
    }

    /*
     * Allocate a new node and fill the fields.
     */
    let ret: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if ret.is_null() {
        xml_tree_err_memory(c"copying node".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlNode>());
    (*ret).typ = (*node).typ;

    (*ret).doc = doc;
    (*ret).parent = parent;
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
    if !matches!((*node).typ, XmlElementType::XmlElementNode)
        && !(*node).content.is_null()
        && !matches!((*node).typ, XmlElementType::XmlEntityRefNode)
        && !matches!((*node).typ, XmlElementType::XmlXincludeEnd)
        && !matches!((*node).typ, XmlElementType::XmlXincludeStart)
    {
        (*ret).content = xml_strdup((*node).content);
    } else if matches!((*node).typ, XmlElementType::XmlElementNode) {
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
    if (matches!((*node).typ, XmlElementType::XmlElementNode)
        || matches!((*node).typ, XmlElementType::XmlXincludeStart))
        && !(*node).ns_def.is_null()
    {
        (*ret).ns_def = xml_copy_namespace_list((*node).ns_def);
    }

    if !(*node).ns.is_null() {
        let mut ns: XmlNsPtr;

        ns = xml_search_ns(doc, ret, (*(*node).ns).prefix.load(Ordering::Relaxed));
        if ns.is_null() {
            /*
             * Humm, we are copying an element whose namespace is defined
             * out of the new tree scope. Search it in the original tree
             * and add it at the top of the new tree
             */
            ns = xml_search_ns(
                (*node).doc,
                node,
                (*(*node).ns).prefix.load(Ordering::Relaxed),
            );
            if !ns.is_null() {
                let mut root: XmlNodePtr = ret;

                while !(*root).parent.is_null() {
                    root = (*root).parent;
                }
                (*ret).ns = xml_new_ns(
                    root,
                    (*ns).href.load(Ordering::Relaxed),
                    (*ns).prefix.load(Ordering::Relaxed),
                );
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
    if (matches!((*node).typ, XmlElementType::XmlElementNode)
        || matches!((*node).typ, XmlElementType::XmlXincludeStart))
        && !(*node).properties.is_null()
    {
        (*ret).properties = xml_copy_prop_list(ret, (*node).properties);
    }
    if matches!((*node).typ, XmlElementType::XmlEntityRefNode) {
        if doc.is_null() || (*node).doc != doc {
            /*
             * The copied node will go into a separate document, so
             * to avoid dangling references to the ENTITY_DECL node
             * we cannot keep the reference. Try to find it in the
             * target document.
             */
            (*ret).children = xml_get_doc_entity(doc, (*ret).name) as _;
        } else {
            (*ret).children = (*node).children;
        }
        (*ret).last = (*ret).children;
    } else if !(*node).children.is_null() && extended != 2 {
        let mut cur: XmlNodePtr;
        let mut insert: XmlNodePtr;

        cur = (*node).children;
        insert = ret;
        while !cur.is_null() {
            let copy: XmlNodePtr = xml_static_copy_node(cur, doc, insert, 2);
            if copy.is_null() {
                xml_free_node(ret);
                return null_mut();
            }

            /* Check for coalesced text nodes */
            if (*insert).last != copy {
                if (*insert).last.is_null() {
                    (*insert).children = copy;
                } else {
                    (*copy).prev = (*insert).last;
                    (*(*insert).last).next = copy;
                }
                (*insert).last = copy;
            }

            if !matches!((*cur).typ, XmlElementType::XmlEntityRefNode) && !(*cur).children.is_null()
            {
                cur = (*cur).children;
                insert = copy;
                continue;
            }

            loop {
                if !(*cur).next.is_null() {
                    cur = (*cur).next;
                    break;
                }

                cur = (*cur).parent;
                insert = (*insert).parent;
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

pub(crate) unsafe extern "C" fn xml_static_copy_node_list(
    mut node: XmlNodePtr,
    doc: XmlDocPtr,
    parent: XmlNodePtr,
) -> XmlNodePtr {
    let mut ret: XmlNodePtr = null_mut();
    let mut p: XmlNodePtr = null_mut();
    let mut q: XmlNodePtr;

    while !node.is_null() {
        #[cfg(feature = "tree")]
        {
            if matches!((*node).typ, XmlElementType::XmlDtdNode) {
                if doc.is_null() {
                    node = (*node).next;
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
                    (*q).parent = parent;
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
        #[cfg(not(feature = "tree"))]
        {
            q = xml_static_copy_node(node, doc, parent, 1);
        }
        if q.is_null() {
            // goto error;
            xml_free_node_list(ret);
            return null_mut();
        }
        if ret.is_null() {
            (*q).prev = null_mut();
            ret = q;
            p = q;
        } else if p != q {
            /* the test is required if xmlStaticCopyNode coalesced 2 text nodes */
            (*p).next = q;
            (*q).prev = p;
            p = q;
        }
        node = (*node).next;
    }
    ret
    // error:
    //     xmlFreeNodeList(ret);
    //     return null_mut();
}

unsafe extern "C" fn xml_copy_prop_internal(
    doc: XmlDocPtr,
    target: XmlNodePtr,
    cur: XmlAttrPtr,
) -> XmlAttrPtr {
    let ret: XmlAttrPtr;

    if cur.is_null() {
        return null_mut();
    }
    if !target.is_null() && !matches!((*target).typ, XmlElementType::XmlElementNode) {
        return null_mut();
    }
    if !target.is_null() {
        ret = xml_new_doc_prop((*target).doc, (*cur).name, null_mut());
    } else if !doc.is_null() {
        ret = xml_new_doc_prop(doc, (*cur).name, null_mut());
    } else if !(*cur).parent.is_null() {
        ret = xml_new_doc_prop((*(*cur).parent).doc, (*cur).name, null_mut());
    } else if !(*cur).children.is_null() {
        ret = xml_new_doc_prop((*(*cur).children).doc, (*cur).name, null_mut());
    } else {
        ret = xml_new_doc_prop(null_mut(), (*cur).name, null_mut());
    }
    if ret.is_null() {
        return null_mut();
    }
    (*ret).parent = target;

    if !(*cur).ns.is_null() && !target.is_null() {
        let mut ns: XmlNsPtr;

        ns = xml_search_ns(
            (*target).doc,
            target,
            (*(*cur).ns).prefix.load(Ordering::Relaxed),
        );
        if ns.is_null() {
            /*
             * Humm, we are copying an element whose namespace is defined
             * out of the new tree scope. Search it in the original tree
             * and add it at the top of the new tree
             */
            ns = xml_search_ns(
                (*cur).doc,
                (*cur).parent,
                (*(*cur).ns).prefix.load(Ordering::Relaxed),
            );
            if !ns.is_null() {
                let mut root: XmlNodePtr = target;
                let mut pred: XmlNodePtr = null_mut();

                while !(*root).parent.is_null() {
                    pred = root;
                    root = (*root).parent;
                }
                if root == (*target).doc as _ {
                    /* correct possibly cycling above the document elt */
                    root = pred;
                }
                (*ret).ns = xml_new_ns(
                    root,
                    (*ns).href.load(Ordering::Relaxed),
                    (*ns).prefix.load(Ordering::Relaxed),
                );
            }
        } else {
            /*
             * we have to find something appropriate here since
             * we can't be sure, that the namespace we found is identified
             * by the prefix
             */
            if xml_str_equal(
                (*ns).href.load(Ordering::Relaxed),
                (*(*cur).ns).href.load(Ordering::Relaxed),
            ) {
                /* this is the nice case */
                (*ret).ns = ns;
            } else {
                /*
                 * we are in trouble: we need a new reconciled namespace.
                 * This is expensive
                 */
                (*ret).ns = xml_new_reconciled_ns((*target).doc, target, (*cur).ns);
            }
        }
    } else {
        (*ret).ns = null_mut();
    }

    if !(*cur).children.is_null() {
        let mut tmp: XmlNodePtr;

        (*ret).children = xml_static_copy_node_list((*cur).children, (*ret).doc, ret as _);
        (*ret).last = null_mut();
        tmp = (*ret).children;
        while !tmp.is_null() {
            /* (*tmp).parent = ret; */
            if (*tmp).next.is_null() {
                (*ret).last = tmp;
            }
            tmp = (*tmp).next;
        }
    }
    /*
     * Try to handle IDs
     */
    if !target.is_null()
        && !cur.is_null()
        && !(*target).doc.is_null()
        && !(*cur).doc.is_null()
        && !(*(*cur).doc).ids.is_null()
        && !(*cur).parent.is_null()
        && xml_is_id((*cur).doc, (*cur).parent, cur) != 0
    {
        let id: *mut XmlChar = xml_node_list_get_string((*cur).doc, (*cur).children, 1);
        if !id.is_null() {
            xml_add_id(null_mut(), (*target).doc, id, ret);
            xml_free(id as _);
        }
    }
    ret
}

/**
 * xmlCopyProp:
 * @target:  the element where the attribute will be grafted
 * @cur:  the attribute
 *
 * Do a copy of the attribute.
 *
 * Returns: a new #xmlAttrPtr, or null_mut() in case of error.
 */
pub unsafe extern "C" fn xml_copy_prop(target: XmlNodePtr, cur: XmlAttrPtr) -> XmlAttrPtr {
    xml_copy_prop_internal(null_mut(), target, cur)
}

/**
 * xmlCopyPropList:
 * @target:  the element where the attributes will be grafted
 * @cur:  the first attribute
 *
 * Do a copy of an attribute list.
 *
 * Returns: a new #xmlAttrPtr, or null_mut() in case of error.
 */
pub unsafe extern "C" fn xml_copy_prop_list(target: XmlNodePtr, mut cur: XmlAttrPtr) -> XmlAttrPtr {
    let mut ret: XmlAttrPtr = null_mut();
    let mut p: XmlAttrPtr = null_mut();
    let mut q: XmlAttrPtr;

    if !target.is_null() && !matches!((*target).typ, XmlElementType::XmlElementNode) {
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

/**
 * xmlCopyDtd:
 * @dtd:  the dtd
 *
 * Do a copy of the dtd.
 *
 * Returns: a new #xmlDtdPtr, or null_mut() in case of error.
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_copy_dtd(dtd: XmlDtdPtr) -> XmlDtdPtr {
    use crate::libxml::{
        entities::xml_copy_entities_table,
        valid::{
            xml_copy_attribute_table, xml_copy_element_table, xml_copy_notation_table,
            xml_get_dtd_qattr_desc, xml_get_dtd_qelement_desc,
        },
    };

    let mut cur: XmlNodePtr;
    let mut p: XmlNodePtr = null_mut();
    let mut q: XmlNodePtr;

    if dtd.is_null() {
        return null_mut();
    }
    let ret: XmlDtdPtr = xml_new_dtd(
        null_mut(),
        (*dtd).name,
        (*dtd).external_id,
        (*dtd).system_id,
    );
    if ret.is_null() {
        return null_mut();
    }
    if !(*dtd).entities.is_null() {
        (*ret).entities = xml_copy_entities_table((*dtd).entities as _) as _;
    }
    if !(*dtd).notations.is_null() {
        (*ret).notations = xml_copy_notation_table((*dtd).notations as XmlNotationTablePtr) as _;
    }
    if !(*dtd).elements.is_null() {
        (*ret).elements = xml_copy_element_table((*dtd).elements as XmlElementTablePtr) as _;
    }
    if !(*dtd).attributes.is_null() {
        (*ret).attributes =
            xml_copy_attribute_table((*dtd).attributes as XmlAttributeTablePtr) as _;
    }
    if !(*dtd).pentities.is_null() {
        (*ret).pentities = xml_copy_entities_table((*dtd).pentities as _) as _;
    }

    cur = (*dtd).children;
    while !cur.is_null() {
        q = null_mut();

        if matches!((*cur).typ, XmlElementType::XmlEntityDecl) {
            let tmp: XmlEntityPtr = cur as _;
            match (*tmp).etype {
                Some(XmlEntityType::XmlInternalGeneralEntity)
                | Some(XmlEntityType::XmlExternalGeneralParsedEntity)
                | Some(XmlEntityType::XmlExternalGeneralUnparsedEntity) => {
                    q = (*ret).get_entity((*tmp).name.load(Ordering::Relaxed)) as _;
                }
                Some(XmlEntityType::XmlInternalParameterEntity)
                | Some(XmlEntityType::XmlExternalParameterEntity) => {
                    q = (*ret).get_parameter_entity((*tmp).name.load(Ordering::Relaxed)) as _;
                }
                Some(XmlEntityType::XmlInternalPredefinedEntity) => {}
                _ => unreachable!(),
            }
        } else if matches!((*cur).typ, XmlElementType::XmlElementDecl) {
            let tmp: XmlElementPtr = cur as _;
            q = xml_get_dtd_qelement_desc(ret, (*tmp).name, (*tmp).prefix) as _;
        } else if matches!((*cur).typ, XmlElementType::XmlAttributeDecl) {
            let tmp: XmlAttributePtr = cur as _;
            q = xml_get_dtd_qattr_desc(ret, (*tmp).elem, (*tmp).name, (*tmp).prefix) as _;
        } else if matches!((*cur).typ, XmlElementType::XmlCommentNode) {
            q = xml_copy_node(cur, 0);
        }

        if q.is_null() {
            cur = (*cur).next;
            continue;
        }

        if p.is_null() {
            (*ret).children = q;
        } else {
            (*p).next = q;
        }

        (*q).prev = p;
        (*q).parent = ret as _;
        (*q).next = null_mut();
        (*ret).last = q;
        p = q;
        cur = (*cur).next;
    }

    ret
}

/**
 * xmlCopyDoc:
 * @doc:  the document
 * @recursive:  if not zero do a recursive copy.
 *
 * Do a copy of the document info. If recursive, the content tree will
 * be copied too as well as DTD, namespaces and entities.
 *
 * Returns: a new #xmlDocPtr, or null_mut() in case of error.
 */
#[cfg(any(feature = "tree", feature = "schema"))]
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

    (*ret).last = null_mut();
    (*ret).children = null_mut();
    #[cfg(feature = "tree")]
    {
        if !(*doc).int_subset.is_null() {
            (*ret).int_subset = xml_copy_dtd((*doc).int_subset);
            if (*ret).int_subset.is_null() {
                xml_free_doc(ret);
                return null_mut();
            }
            xml_set_tree_doc((*ret).int_subset as _, ret);
            (*(*ret).int_subset).parent = ret;
        }
    }
    if !(*doc).old_ns.is_null() {
        (*ret).old_ns = xml_copy_namespace_list((*doc).old_ns);
    }
    if !(*doc).children.is_null() {
        let mut tmp: XmlNodePtr;

        (*ret).children = xml_static_copy_node_list((*doc).children, ret, ret as _);
        (*ret).last = null_mut();
        tmp = (*ret).children;
        while !tmp.is_null() {
            if (*tmp).next.is_null() {
                (*ret).last = tmp;
            }
            tmp = (*tmp).next;
        }
    }
    ret
}

macro_rules! UPDATE_LAST_CHILD_AND_PARENT {
    ($n:expr) => {
        if !$n.is_null() {
            let mut ulccur: XmlNodePtr = (*$n).children;
            if ulccur.is_null() {
                (*$n).last = null_mut();
            } else {
                while !(*ulccur).next.is_null() {
                    (*ulccur).parent = $n;
                    ulccur = (*ulccur).next;
                }
                (*ulccur).parent = $n;
                (*$n).last = ulccur;
            }
        }
    };
}

/*
 * Creating new nodes.
 */
/**
 * xmlNewDocNode:
 * @doc:  the document
 * @ns:  namespace if any
 * @name:  the node name
 * @content:  the XML text content if any
 *
 * Creation of a new node element within a document. @ns and @content
 * are optional (null_mut()).
 * NOTE: @content is supposed to be a piece of XML CDATA, so it allow entities
 *       references, but XML special chars need to be escaped first by using
 *       xmlEncodeEntitiesReentrant(). Use xmlNewDocRawNode() if you don't
 *       need entities support.
 *
 * Returns a pointer to the new node object.
 */
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
            (*cur).children = xml_string_get_node_list(doc, content);
            UPDATE_LAST_CHILD_AND_PARENT!(cur)
        }
    }

    cur
}

/**
 * xmlNewDocNodeEatName:
 * @doc:  the document
 * @ns:  namespace if any
 * @name:  the node name
 * @content:  the XML text content if any
 *
 * Creation of a new node element within a document. @ns and @content
 * are optional (null_mut()).
 * NOTE: @content is supposed to be a piece of XML CDATA, so it allow entities
 *       references, but XML special chars need to be escaped first by using
 *       xmlEncodeEntitiesReentrant(). Use xmlNewDocRawNode() if you don't
 *       need entities support.
 *
 * Returns a pointer to the new node object.
 */
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
            (*cur).children = xml_string_get_node_list(doc, content);
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

/**
 * xmlNewNode:
 * @ns:  namespace if any
 * @name:  the node name
 *
 * Creation of a new node element. @ns is optional (null_mut()).
 *
 * Use of this function is DISCOURAGED in favor of xmlNewDocNode.
 *
 * Returns a pointer to the new node object. Uses xml_strdup() to make
 * copy of @name.
 */
pub unsafe extern "C" fn xml_new_node(ns: XmlNsPtr, name: *const XmlChar) -> XmlNodePtr {
    if name.is_null() {
        return null_mut();
    }

    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building node".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNode>());
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

/**
 * xmlNewNodeEatName:
 * @ns:  namespace if any
 * @name:  the node name
 *
 * Creation of a new node element. @ns is optional (null_mut()).
 *
 * Use of this function is DISCOURAGED in favor of xmlNewDocNodeEatName.
 *
 * Returns a pointer to the new node object, with pointer @name as
 * new node's name. Use xmlNewNode() if a copy of @name string is
 * is needed as new node's name.
 */
pub unsafe extern "C" fn xml_new_node_eat_name(ns: XmlNsPtr, name: *mut XmlChar) -> XmlNodePtr {
    if name.is_null() {
        return null_mut();
    }

    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building node".as_ptr() as _);
        /* we can't check here that name comes from the doc dictionary */
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNode>());
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

/**
 * xmlNewChild:
 * @parent:  the parent node
 * @ns:  a namespace if any
 * @name:  the name of the child
 * @content:  the XML content of the child if any.
 *
 * Creation of a new child element, added at the end of @parent children list.
 * @ns and @content parameters are optional (null_mut()). If @ns is null_mut(), the newly
 * created element inherits the namespace of @parent. If @content is non null_mut(),
 * a child list containing the TEXTs and ENTITY_REFs node will be created.
 * NOTE: @content is supposed to be a piece of XML CDATA, so it allows entity
 *       references. XML special chars must be escaped first by using
 *       xmlEncodeEntitiesReentrant(), or xmlNewTextChild() should be used.
 *
 * Returns a pointer to the new node object.
 */
#[cfg(any(feature = "tree", feature = "schema"))]
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

    /*
     * Allocate a new node
     */
    if matches!((*parent).typ, XmlElementType::XmlElementNode) {
        if ns.is_null() {
            cur = xml_new_doc_node((*parent).doc, (*parent).ns, name, content);
        } else {
            cur = xml_new_doc_node((*parent).doc, ns, name, content);
        }
    } else if (matches!((*parent).typ, XmlElementType::XmlDocumentNode)
        || matches!((*parent).typ, XmlElementType::XmlHtmlDocumentNode))
    {
        if ns.is_null() {
            cur = xml_new_doc_node(parent as _, null_mut(), name, content);
        } else {
            cur = xml_new_doc_node(parent as _, ns, name, content);
        }
    } else if matches!((*parent).typ, XmlElementType::XmlDocumentFragNode) {
        cur = xml_new_doc_node((*parent).doc, ns, name, content);
    } else {
        return null_mut();
    }
    if cur.is_null() {
        return null_mut();
    }

    /*
     * add the new element at the end of the children list.
     */
    (*cur).typ = XmlElementType::XmlElementNode;
    (*cur).parent = parent;
    (*cur).doc = (*parent).doc;
    if (*parent).children.is_null() {
        (*parent).children = cur;
        (*parent).last = cur;
    } else {
        prev = (*parent).last;
        (*prev).next = cur;
        (*cur).prev = prev;
        (*parent).last = cur;
    }

    cur
}

/**
 * xmlNewDocText:
 * @doc: the document
 * @content:  the text content
 *
 * Creation of a new text node within a document.
 * Returns a pointer to the new node object.
 */
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

/**
 * xmlNewText:
 * @content:  the text content
 *
 * Creation of a new text node.
 *
 * Use of this function is DISCOURAGED in favor of xmlNewDocText.
 *
 * Returns a pointer to the new node object.
 */
pub unsafe extern "C" fn xml_new_text(content: *const XmlChar) -> XmlNodePtr {
    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building text".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNode>());
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

/**
 * xmlNewDocPI:
 * @doc:  the target document
 * @name:  the processing instruction name
 * @content:  the PI content
 *
 * Creation of a processing instruction element.
 * Returns a pointer to the new node object.
 */
pub unsafe extern "C" fn xml_new_doc_pi(
    doc: XmlDocPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> XmlNodePtr {
    if name.is_null() {
        return null_mut();
    }

    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building PI".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNode>());
    (*cur).typ = XmlElementType::XmlPiNode;

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

/**
 * xmlNewPI:
 * @name:  the processing instruction name
 * @content:  the PI content
 *
 * Creation of a processing instruction element.
 *
 * Use of this function is DISCOURAGED in favor of xmlNewDocPI.
 *
 * Returns a pointer to the new node object.
 */
pub unsafe extern "C" fn xml_new_pi(name: *const XmlChar, content: *const XmlChar) -> XmlNodePtr {
    xml_new_doc_pi(null_mut(), name, content)
}

/**
 * xmlNewDocTextLen:
 * @doc: the document
 * @content:  the text content
 * @len:  the text len.
 *
 * Creation of a new text node with an extra content length parameter. The
 * text node pertain to a given document.
 * Returns a pointer to the new node object.
 */
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

/**
 * xmlNewTextLen:
 * @content:  the text content
 * @len:  the text len.
 *
 * Use of this function is DISCOURAGED in favor of xmlNewDocTextLen.
 *
 * Creation of a new text node with an extra parameter for the content's length
 * Returns a pointer to the new node object.
 */
pub unsafe extern "C" fn xml_new_text_len(content: *const XmlChar, len: i32) -> XmlNodePtr {
    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building text".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNode>());
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

/**
 * xmlNewDocComment:
 * @doc:  the document
 * @content:  the comment content
 *
 * Creation of a new node containing a comment within a document.
 * Returns a pointer to the new node object.
 */
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

/**
 * xmlNewComment:
 * @content:  the comment content
 *
 * Use of this function is DISCOURAGED in favor of xmlNewDocComment.
 *
 * Creation of a new node containing a comment.
 * Returns a pointer to the new node object.
 */
pub unsafe extern "C" fn xml_new_comment(content: *const XmlChar) -> XmlNodePtr {
    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building comment".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNode>());
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

/**
 * xmlNewCDataBlock:
 * @doc:  the document
 * @content:  the CDATA block content content
 * @len:  the length of the block
 *
 * Creation of a new node containing a CDATA block.
 * Returns a pointer to the new node object.
 */
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
        xml_tree_err_memory(c"building CDATA".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNode>());
    (*cur).typ = XmlElementType::XmlCdataSectionNode;
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

/**
 * xmlNewCharRef:
 * @doc: the document
 * @name:  the c_char ref string, starting with # or "&# ... ;"
 *
 * Creation of a new character reference node.
 * Returns a pointer to the new node object.
 */
pub unsafe extern "C" fn xml_new_char_ref(doc: XmlDocPtr, mut name: *const XmlChar) -> XmlNodePtr {
    if name.is_null() {
        return null_mut();
    }

    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building character reference".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNode>());
    (*cur).typ = XmlElementType::XmlEntityRefNode;

    (*cur).doc = doc;
    if *name.add(0) == b'&' {
        name = name.add(1);
        let len: i32 = xml_strlen(name);
        if *name.add(len as usize - 1) == b';' {
            (*cur).name = xml_strndup(name, len - 1);
        } else {
            (*cur).name = xml_strndup(name, len);
        }
    } else {
        (*cur).name = xml_strdup(name);
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/**
 * xmlNewReference:
 * @doc: the document
 * @name:  the reference name, or the reference string with & and ;
 *
 * Creation of a new reference node.
 * Returns a pointer to the new node object.
 */
pub unsafe extern "C" fn xml_new_reference(
    doc: *const XmlDoc,
    mut name: *const XmlChar,
) -> XmlNodePtr {
    if name.is_null() {
        return null_mut();
    }

    /*
     * Allocate a new node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building reference".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNode>());
    (*cur).typ = XmlElementType::XmlEntityRefNode;

    (*cur).doc = doc as _;
    if *name.add(0) == b'&' {
        name = name.add(1);
        let len: i32 = xml_strlen(name);
        if *name.add(len as usize - 1) == b';' {
            (*cur).name = xml_strndup(name, len - 1);
        } else {
            (*cur).name = xml_strndup(name, len);
        }
    } else {
        (*cur).name = xml_strdup(name);
    }

    let ent: XmlEntityPtr = xml_get_doc_entity(doc, (*cur).name);
    if !ent.is_null() {
        (*cur).content = (*ent).content.load(Ordering::Acquire);
        /*
         * The parent pointer in entity is a DTD pointer and thus is NOT
         * updated.  Not sure if this is 100% correct.
         *  -George
         */
        (*cur).children = ent as _;
        (*cur).last = ent as _;
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/**
 * xmlCopyNode:
 * @node:  the node
 * @extended:   if 1 do a recursive copy (properties, namespaces and children
 *            when applicable)
 *        if 2 copy properties and namespaces (when applicable)
 *
 * Do a copy of the node.
 *
 * Returns: a new #XmlNodePtr, or null_mut() in case of error.
 */
pub unsafe extern "C" fn xml_copy_node(node: XmlNodePtr, extended: i32) -> XmlNodePtr {
    let ret: XmlNodePtr = xml_static_copy_node(node, null_mut(), null_mut(), extended);
    ret
}

/**
 * xmlDocCopyNode:
 * @node:  the node
 * @doc:  the document
 * @extended:   if 1 do a recursive copy (properties, namespaces and children
 *            when applicable)
 *        if 2 copy properties and namespaces (when applicable)
 *
 * Do a copy of the node to a given document.
 *
 * Returns: a new #XmlNodePtr, or null_mut() in case of error.
 */
pub unsafe extern "C" fn xml_doc_copy_node(
    node: XmlNodePtr,
    doc: XmlDocPtr,
    extended: i32,
) -> XmlNodePtr {
    let ret: XmlNodePtr = xml_static_copy_node(node, doc, null_mut(), extended);
    ret
}

/**
 * xmlDocCopyNodeList:
 * @doc: the target document
 * @node:  the first node in the list.
 *
 * Do a recursive copy of the node list.
 *
 * Returns: a new #XmlNodePtr, or null_mut() in case of error.
 */
pub unsafe extern "C" fn xml_doc_copy_node_list(doc: XmlDocPtr, node: XmlNodePtr) -> XmlNodePtr {
    let ret: XmlNodePtr = xml_static_copy_node_list(node, doc, null_mut());
    ret
}

/**
 * xmlCopyNodeList:
 * @node:  the first node in the list.
 *
 * Do a recursive copy of the node list.
 * Use xmlDocCopyNodeList() if possible to ensure string interning.
 *
 * Returns: a new #XmlNodePtr, or null_mut() in case of error.
 */
pub unsafe extern "C" fn xml_copy_node_list(node: XmlNodePtr) -> XmlNodePtr {
    let ret: XmlNodePtr = xml_static_copy_node_list(node, null_mut(), null_mut());
    ret
}

/**
 * xmlNewTextChild:
 * @parent:  the parent node
 * @ns:  a namespace if any
 * @name:  the name of the child
 * @content:  the text content of the child if any.
 *
 * Creation of a new child element, added at the end of @parent children list.
 * @ns and @content parameters are optional (null_mut()). If @ns is null_mut(), the newly
 * created element inherits the namespace of @parent. If @content is non null_mut(),
 * a child TEXT node will be created containing the string @content.
 * NOTE: Use xmlNewChild() if @content will contain entities that need to be
 * preserved. Use this function, xmlNewTextChild(), if you need to ensure that
 * reserved XML chars that might appear in @content, such as the ampersand,
 * greater-than or less-than signs, are automatically replaced by their XML
 * escaped entity representations.
 *
 * Returns a pointer to the new node object.
 */
#[cfg(feature = "tree")]
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
    if matches!((*parent).typ, XmlElementType::XmlElementNode) {
        if ns.is_null() {
            cur = xml_new_doc_raw_node((*parent).doc, (*parent).ns, name, content);
        } else {
            cur = xml_new_doc_raw_node((*parent).doc, ns, name, content);
        }
    } else if (matches!((*parent).typ, XmlElementType::XmlDocumentNode)
        || matches!((*parent).typ, XmlElementType::XmlHtmlDocumentNode))
    {
        if ns.is_null() {
            cur = xml_new_doc_raw_node(parent as _, null_mut(), name, content);
        } else {
            cur = xml_new_doc_raw_node(parent as _, ns, name, content);
        }
    } else if matches!((*parent).typ, XmlElementType::XmlDocumentFragNode) {
        cur = xml_new_doc_raw_node((*parent).doc, ns, name, content);
    } else {
        return null_mut();
    }
    if cur.is_null() {
        return null_mut();
    }

    /*
     * add the new element at the end of the children list.
     */
    (*cur).typ = XmlElementType::XmlElementNode;
    (*cur).parent = parent;
    (*cur).doc = (*parent).doc;
    if (*parent).children.is_null() {
        (*parent).children = cur;
        (*parent).last = cur;
    } else {
        prev = (*parent).last;
        (*prev).next = cur;
        (*cur).prev = prev;
        (*parent).last = cur;
    }

    cur
}

/**
 * xmlNewDocRawNode:
 * @doc:  the document
 * @ns:  namespace if any
 * @name:  the node name
 * @content:  the text content if any
 *
 * Creation of a new node element within a document. @ns and @content
 * are optional (null_mut()).
 *
 * Returns a pointer to the new node object.
 */
#[cfg(feature = "tree")]
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
            (*cur).children = xml_new_doc_text(doc, content);
            UPDATE_LAST_CHILD_AND_PARENT!(cur)
        }
    }
    cur
}

/**
 * xmlNewDocFragment:
 * @doc:  the document owning the fragment
 *
 * Creation of a new Fragment node.
 * Returns a pointer to the new node object.
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_new_doc_fragment(doc: XmlDocPtr) -> XmlNodePtr {
    /*
     * Allocate a new DocumentFragment node and fill the fields.
     */
    let cur: XmlNodePtr = xml_malloc(size_of::<XmlNode>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building fragment".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNode>());
    (*cur).typ = XmlElementType::XmlDocumentFragNode;

    (*cur).doc = doc;

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/*
 * Changing the structure.
 */

/**
 * xmlReplaceNode:
 * @old:  the old node
 * @cur:  the node
 *
 * Unlink the old node from its current context, prune the new one
 * at the same place. If @cur was already inserted in a document it is
 * first unlinked from its existing context.
 *
 * See the note regarding namespaces in xmlAddChild.
 *
 * Returns the @old node
 */
#[cfg(any(feature = "tree", feature = "writer"))]
pub unsafe extern "C" fn xml_replace_node(old: XmlNodePtr, cur: XmlNodePtr) -> XmlNodePtr {
    if old == cur {
        return null_mut();
    }
    if old.is_null()
        || matches!((*old).typ, XmlElementType::XmlNamespaceDecl)
        || (*old).parent.is_null()
    {
        return null_mut();
    }
    if cur.is_null() || matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        (*old).unlink();
        return old;
    }
    if cur == old {
        return old;
    }
    if (matches!((*old).typ, XmlElementType::XmlAttributeNode)
        && !matches!((*cur).typ, XmlElementType::XmlAttributeNode))
    {
        return old;
    }
    if (matches!((*cur).typ, XmlElementType::XmlAttributeNode)
        && !matches!((*old).typ, XmlElementType::XmlAttributeNode))
    {
        return old;
    }
    (*cur).unlink();
    xml_set_tree_doc(cur, (*old).doc);
    (*cur).parent = (*old).parent;
    (*cur).next = (*old).next;
    if !(*cur).next.is_null() {
        (*(*cur).next).prev = cur;
    }
    (*cur).prev = (*old).prev;
    if !(*cur).prev.is_null() {
        (*(*cur).prev).next = cur;
    }
    if !(*cur).parent.is_null() {
        if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
            if (*(*cur).parent).properties == old as _ {
                (*(*cur).parent).properties = cur as _;
            }
        } else {
            if (*(*cur).parent).children == old {
                (*(*cur).parent).children = cur;
            }
            if (*(*cur).parent).last == old {
                (*(*cur).parent).last = cur;
            }
        }
    }
    (*old).next = null_mut();
    (*old).prev = null_mut();
    (*old).parent = null_mut();
    old
}

/**
 * xmlTextMerge:
 * @first:  the first text node
 * @second:  the second text node being merged
 *
 * Merge two text nodes into one
 * Returns the first text node augmented
 */
pub unsafe extern "C" fn xml_text_merge(first: XmlNodePtr, second: XmlNodePtr) -> XmlNodePtr {
    if first.is_null() {
        return second;
    }
    if second.is_null() {
        return first;
    }
    if !matches!((*first).typ, XmlElementType::XmlTextNode) {
        return first;
    }
    if !matches!((*second).typ, XmlElementType::XmlTextNode) {
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

/**
 * xmlTextConcat:
 * @node:  the node
 * @content:  the content
 * @len:  @content length
 *
 * Concat the given string at the end of the existing node content
 *
 * Returns -1 in case of error, 0 otherwise
 */
pub unsafe extern "C" fn xml_text_concat(
    node: XmlNodePtr,
    content: *const XmlChar,
    len: i32,
) -> i32 {
    if node.is_null() {
        return -1;
    }

    if !matches!((*node).typ, XmlElementType::XmlTextNode)
        && !matches!((*node).typ, XmlElementType::XmlCdataSectionNode)
        && !matches!((*node).typ, XmlElementType::XmlCommentNode)
        && !matches!((*node).typ, XmlElementType::XmlPiNode)
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

/**
 * xmlFreeNodeList:
 * @cur:  the first node in the list
 *
 * Free a node and all its siblings, this is a recursive behaviour, all
 * the children are freed too.
 */
pub unsafe extern "C" fn xml_free_node_list(mut cur: XmlNodePtr) {
    let mut next: XmlNodePtr;
    let mut parent: XmlNodePtr;
    let mut dict: XmlDictPtr = null_mut();
    let mut depth: usize = 0;

    if cur.is_null() {
        return;
    }
    if matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        xml_free_ns_list(cur as _);
        return;
    }
    if !(*cur).doc.is_null() {
        dict = (*(*cur).doc).dict;
    }
    loop {
        while !(*cur).children.is_null()
            && !matches!((*cur).typ, XmlElementType::XmlDocumentNode)
            && !matches!((*cur).typ, XmlElementType::XmlHtmlDocumentNode)
            && !matches!((*cur).typ, XmlElementType::XmlDtdNode)
            && !matches!((*cur).typ, XmlElementType::XmlEntityRefNode)
        {
            cur = (*cur).children;
            depth += 1;
        }

        next = (*cur).next;
        parent = (*cur).parent;
        if matches!((*cur).typ, XmlElementType::XmlDocumentNode)
            || matches!((*cur).typ, XmlElementType::XmlHtmlDocumentNode)
        {
            xml_free_doc(cur as _);
        } else if !matches!((*cur).typ, XmlElementType::XmlDtdNode) {
            if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
            // && xmlDeregisterNodeDefaultValue.is_some()
            {
                xml_deregister_node_default_value(cur as _);
            }

            if (matches!((*cur).typ, XmlElementType::XmlElementNode)
                || matches!((*cur).typ, XmlElementType::XmlXincludeStart)
                || matches!((*cur).typ, XmlElementType::XmlXincludeEnd))
                && !(*cur).properties.is_null()
            {
                xml_free_prop_list((*cur).properties);
            }
            if !matches!((*cur).typ, XmlElementType::XmlElementNode)
                && !matches!((*cur).typ, XmlElementType::XmlXincludeStart)
                && !matches!((*cur).typ, XmlElementType::XmlXincludeEnd)
                && !matches!((*cur).typ, XmlElementType::XmlEntityRefNode)
                && ((*cur).content != addr_of_mut!((*cur).properties) as _)
            {
                DICT_FREE!(dict, (*cur).content)
            }
            if (matches!((*cur).typ, XmlElementType::XmlElementNode)
                || matches!((*cur).typ, XmlElementType::XmlXincludeStart)
                || matches!((*cur).typ, XmlElementType::XmlXincludeEnd))
                && !(*cur).ns_def.is_null()
            {
                xml_free_ns_list((*cur).ns_def);
            }

            /*
             * When a node is a text node or a comment, it uses a global static
             * variable for the name of the node.
             * Otherwise the node name might come from the document's
             * dictionary
             */
            if !(*cur).name.is_null()
                && !matches!((*cur).typ, XmlElementType::XmlTextNode)
                && !matches!((*cur).typ, XmlElementType::XmlCommentNode)
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
            (*cur).children = null_mut();
        }
    }
}

/**
 * xmlFreeNode:
 * @cur:  the node
 *
 * Free a node, this is a recursive behaviour, all the children are freed too.
 * This doesn't unlink the child from the list, use xmlUnlinkNode() first.
 */
pub unsafe extern "C" fn xml_free_node(cur: XmlNodePtr) {
    let mut dict: XmlDictPtr = null_mut();

    if cur.is_null() {
        return;
    }

    /* use xmlFreeDtd for DTD nodes */
    if matches!((*cur).typ, XmlElementType::XmlDtdNode) {
        xml_free_dtd(cur as _);
        return;
    }
    if matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        xml_free_ns(cur as _);
        return;
    }
    if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
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

    if matches!((*cur).typ, XmlElementType::XmlEntityDecl) {
        let ent: XmlEntityPtr = cur as _;
        DICT_FREE!(dict, (*ent).system_id.load(Ordering::Relaxed));
        DICT_FREE!(dict, (*ent).external_id.load(Ordering::Relaxed));
    }
    if !(*cur).children.is_null() && !matches!((*cur).typ, XmlElementType::XmlEntityRefNode) {
        xml_free_node_list((*cur).children);
    }

    if matches!((*cur).typ, XmlElementType::XmlElementNode)
        || matches!((*cur).typ, XmlElementType::XmlXincludeStart)
        || matches!((*cur).typ, XmlElementType::XmlXincludeEnd)
    {
        if !(*cur).properties.is_null() {
            xml_free_prop_list((*cur).properties);
        }
        if !(*cur).ns_def.is_null() {
            xml_free_ns_list((*cur).ns_def);
        }
    } else if !(*cur).content.is_null()
        && !matches!((*cur).typ, XmlElementType::XmlEntityRefNode)
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
        && !matches!((*cur).typ, XmlElementType::XmlTextNode)
        && !matches!((*cur).typ, XmlElementType::XmlCommentNode)
    {
        DICT_FREE!(dict, (*cur).name)
    }

    xml_free(cur as _);
}

unsafe extern "C" fn _copy_string_for_new_dict_if_needed(
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

/**
 * xmlSetTreeDoc:
 * @tree:  the top element
 * @doc:  the document
 *
 * update all nodes under the tree to point to the right document
 */
pub unsafe extern "C" fn xml_set_tree_doc(tree: XmlNodePtr, doc: XmlDocPtr) {
    let mut prop: XmlAttrPtr;

    if tree.is_null() || ((*tree).typ == XmlElementType::XmlNamespaceDecl) {
        return;
    }
    if (*tree).doc != doc {
        let old_tree_dict: XmlDictPtr = if !(*tree).doc.is_null() {
            (*(*tree).doc).dict
        } else {
            null_mut()
        };
        let new_dict: XmlDictPtr = if !doc.is_null() {
            (*doc).dict
        } else {
            null_mut()
        };

        if matches!((*tree).typ, XmlElementType::XmlElementNode) {
            prop = (*tree).properties;
            while !prop.is_null() {
                if matches!((*prop).atype, Some(XmlAttributeType::XmlAttributeId)) {
                    xml_remove_id((*tree).doc, prop);
                }

                if (*prop).doc != doc {
                    let old_prop_dict: XmlDictPtr = if !(*prop).doc.is_null() {
                        (*(*prop).doc).dict
                    } else {
                        null_mut()
                    };
                    (*prop).name =
                        _copy_string_for_new_dict_if_needed(old_prop_dict, new_dict, (*prop).name);
                    (*prop).doc = doc;
                }
                xml_set_list_doc((*prop).children, doc);

                /*
                 * TODO: ID attributes should be also added to the new
                 * document, but this breaks things like xmlReplaceNode.
                 * The underlying problem is that xmlRemoveID is only called
                 * if a node is destroyed, not if it's unlinked.
                 */
                // #if 0
                //                 if (xmlIsID(doc, tree, prop)) {
                //                     XmlChar *idVal = xmlNodeListGetString(doc, (*prop).children,
                //                                                           1);
                //                     xmlAddID(null_mut(), doc, idVal, prop);
                //                 }
                // #endif

                prop = (*prop).next;
            }
        }
        if matches!((*tree).typ, XmlElementType::XmlEntityRefNode) {
            /*
             * Clear 'children' which points to the entity declaration
             * from the original document.
             */
            (*tree).children = null_mut();
        } else if !(*tree).children.is_null() {
            xml_set_list_doc((*tree).children, doc);
        }

        (*tree).name = _copy_string_for_new_dict_if_needed(old_tree_dict, new_dict, (*tree).name);
        (*tree).content =
            _copy_string_for_new_dict_if_needed(old_tree_dict, null_mut(), (*tree).content) as _;
        /* FIXME: (*tree).ns should be updated as in xmlStaticCopyNode(). */
        (*tree).doc = doc;
    }
}

/**
 * xmlSetListDoc:
 * @list:  the first element
 * @doc:  the document
 *
 * update all nodes in the list to point to the right document
 */
pub unsafe extern "C" fn xml_set_list_doc(list: XmlNodePtr, doc: XmlDocPtr) {
    let mut cur: XmlNodePtr;

    if list.is_null() || ((*list).typ == XmlElementType::XmlNamespaceDecl) {
        return;
    }
    cur = list;
    while !cur.is_null() {
        if (*cur).doc != doc {
            xml_set_tree_doc(cur, doc);
        }
        cur = (*cur).next;
    }
}

/*
* xmlTreeEnsureXMLDecl:
* @doc: the doc
*
* Ensures that there is an XML namespace declaration on the doc.
*
* Returns the XML ns-struct or null_mut() on API and internal errors.
*/
unsafe extern "C" fn xml_tree_ensure_xmldecl(doc: XmlDocPtr) -> XmlNsPtr {
    if doc.is_null() {
        return null_mut();
    }
    if !(*doc).old_ns.is_null() {
        return (*doc).old_ns;
    }
    {
        let ns: XmlNsPtr = xml_malloc(size_of::<XmlNs>()) as _;
        if ns.is_null() {
            xml_tree_err_memory(c"allocating the XML namespace".as_ptr() as _);
            return null_mut();
        }
        memset(ns as _, 0, size_of::<XmlNs>());
        (*ns).typ = Some(XML_LOCAL_NAMESPACE);
        (*ns).href.store(
            xml_strdup(XML_XML_NAMESPACE.as_ptr() as _) as _,
            Ordering::Relaxed,
        );
        (*ns)
            .prefix
            .store(xml_strdup(c"xml".as_ptr() as _) as _, Ordering::Relaxed);
        (*doc).old_ns = ns;
        ns
    }
}

/*
 * Namespaces.
 */
/**
 * xmlSearchNs:
 * @doc:  the document
 * @node:  the current node
 * @nameSpace:  the namespace prefix
 *
 * Search a Ns registered under a given name space for a document.
 * recurse on the parents until it finds the defined namespace
 * or return null_mut() otherwise.
 * @nameSpace can be null_mut(), this is a search for the default namespace.
 * We don't allow to cross entities boundaries. If you don't declare
 * the namespace within those you will be in troubles !!! A warning
 * is generated to cover this case.
 *
 * Returns the namespace pointer or null_mut().
 */
pub unsafe extern "C" fn xml_search_ns(
    mut doc: XmlDocPtr,
    mut node: XmlNodePtr,
    name_space: *const XmlChar,
) -> XmlNsPtr {
    let mut cur: XmlNsPtr;
    let orig: *const XmlNode = node;

    if node.is_null() || matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    if !name_space.is_null() && xml_str_equal(name_space, c"xml".as_ptr() as _) {
        if doc.is_null() && matches!((*node).typ, XmlElementType::XmlElementNode) {
            /*
             * The XML-1.0 namespace is normally held on the root
             * element. In this case exceptionally create it on the
             * node element.
             */
            cur = xml_malloc(size_of::<XmlNs>()) as _;
            if cur.is_null() {
                xml_tree_err_memory(c"searching namespace".as_ptr() as _);
                return null_mut();
            }
            memset(cur as _, 0, size_of::<XmlNs>());
            (*cur).typ = Some(XML_LOCAL_NAMESPACE);
            (*cur).href.store(
                xml_strdup(XML_XML_NAMESPACE.as_ptr() as _) as _,
                Ordering::Relaxed,
            );
            (*cur)
                .prefix
                .store(xml_strdup(c"xml".as_ptr() as _) as _, Ordering::Relaxed);
            (*cur).next.store((*node).ns_def as _, Ordering::Relaxed);
            (*node).ns_def = cur;
            return cur;
        }
        if doc.is_null() {
            doc = (*node).doc;
            if doc.is_null() {
                return null_mut();
            }
        }
        /*
        	* Return the XML namespace declaration held by the doc.
        	*/
        if (*doc).old_ns.is_null() {
            return xml_tree_ensure_xmldecl(doc) as _;
        } else {
            return (*doc).old_ns;
        }
    }
    while !node.is_null() {
        if (matches!((*node).typ, XmlElementType::XmlEntityRefNode)
            || matches!((*node).typ, XmlElementType::XmlEntityNode)
            || matches!((*node).typ, XmlElementType::XmlEntityDecl))
        {
            return null_mut();
        }
        if matches!((*node).typ, XmlElementType::XmlElementNode) {
            cur = (*node).ns_def;
            while !cur.is_null() {
                if (*cur).prefix.load(Ordering::Relaxed).is_null()
                    && name_space.is_null()
                    && !(*cur).href.load(Ordering::Relaxed).is_null()
                {
                    return cur;
                }
                if !(*cur).prefix.load(Ordering::Relaxed).is_null()
                    && !name_space.is_null()
                    && !(*cur).href.load(Ordering::Relaxed).is_null()
                    && xml_str_equal((*cur).prefix.load(Ordering::Relaxed), name_space)
                {
                    return cur;
                }
                cur = (*cur).next.load(Ordering::Relaxed);
            }
            if orig != node {
                cur = (*node).ns;
                if !cur.is_null() {
                    if (*cur).prefix.load(Ordering::Relaxed).is_null()
                        && name_space.is_null()
                        && !(*cur).href.load(Ordering::Relaxed).is_null()
                    {
                        return cur;
                    }
                    if !(*cur).prefix.load(Ordering::Relaxed).is_null()
                        && !name_space.is_null()
                        && !(*cur).href.load(Ordering::Relaxed).is_null()
                        && xml_str_equal((*cur).prefix.load(Ordering::Relaxed), name_space)
                    {
                        return cur;
                    }
                }
            }
        }
        node = (*node).parent;
    }
    null_mut()
}

/**
 * xmlNsInScope:
 * @doc:  the document
 * @node:  the current node
 * @ancestor:  the ancestor carrying the namespace
 * @prefix:  the namespace prefix
 *
 * Verify that the given namespace held on @ancestor is still in scope
 * on node.
 *
 * Returns 1 if true, 0 if false and -1 in case of error.
 */
unsafe extern "C" fn xml_ns_in_scope(
    _doc: XmlDocPtr,
    mut node: XmlNodePtr,
    ancestor: XmlNodePtr,
    prefix: *const XmlChar,
) -> i32 {
    let mut tst: XmlNsPtr;

    while !node.is_null() && node != ancestor {
        if matches!((*node).typ, XmlElementType::XmlEntityRefNode)
            || matches!((*node).typ, XmlElementType::XmlEntityNode)
            || matches!((*node).typ, XmlElementType::XmlEntityDecl)
        {
            return -1;
        }
        if matches!((*node).typ, XmlElementType::XmlElementNode) {
            tst = (*node).ns_def;
            while !tst.is_null() {
                if (*tst).prefix.load(Ordering::Relaxed).is_null() && prefix.is_null() {
                    return 0;
                }
                if !(*tst).prefix.load(Ordering::Relaxed).is_null()
                    && !prefix.is_null()
                    && xml_str_equal((*tst).prefix.load(Ordering::Relaxed), prefix)
                {
                    return 0;
                }
                tst = (*tst).next.load(Ordering::Relaxed);
            }
        }
        node = (*node).parent;
    }
    if node != ancestor {
        return -1;
    }
    1
}

/**
 * xmlSearchNsByHref:
 * @doc:  the document
 * @node:  the current node
 * @href:  the namespace value
 *
 * Search a Ns aliasing a given URI. Recurse on the parents until it finds
 * the defined namespace or return null_mut() otherwise.
 * Returns the namespace pointer or null_mut().
 */
pub unsafe extern "C" fn xml_search_ns_by_href(
    mut doc: XmlDocPtr,
    mut node: XmlNodePtr,
    href: *const XmlChar,
) -> XmlNsPtr {
    let mut cur: XmlNsPtr;
    let orig: XmlNodePtr = node;

    if node.is_null() || matches!((*node).typ, XmlElementType::XmlNamespaceDecl) || href.is_null() {
        return null_mut();
    }
    if xml_str_equal(href, XML_XML_NAMESPACE.as_ptr() as _) {
        /*
         * Only the document can hold the XML spec namespace.
         */
        if doc.is_null() && matches!((*node).typ, XmlElementType::XmlElementNode) {
            /*
             * The XML-1.0 namespace is normally held on the root
             * element. In this case exceptionally create it on the
             * node element.
             */
            cur = xml_malloc(size_of::<XmlNs>()) as _;
            if cur.is_null() {
                xml_tree_err_memory(c"searching namespace".as_ptr() as _);
                return null_mut();
            }
            memset(cur as _, 0, size_of::<XmlNs>());
            (*cur).typ = Some(XML_LOCAL_NAMESPACE);
            (*cur).href.store(
                xml_strdup(XML_XML_NAMESPACE.as_ptr() as _),
                Ordering::Relaxed,
            );
            (*cur)
                .prefix
                .store(xml_strdup(c"xml".as_ptr() as _), Ordering::Relaxed);
            (*cur).next.store((*node).ns_def, Ordering::Relaxed);
            (*node).ns_def = cur;
            return cur;
        }
        if doc.is_null() {
            doc = (*node).doc;
            if doc.is_null() {
                return null_mut();
            }
        }
        /*
        	* Return the XML namespace declaration held by the doc.
        	*/
        if (*doc).old_ns.is_null() {
            return xml_tree_ensure_xmldecl(doc);
        } else {
            return (*doc).old_ns;
        }
    }
    let is_attr: i32 = matches!((*node).typ, XmlElementType::XmlAttributeNode) as i32;
    while !node.is_null() {
        if matches!((*node).typ, XmlElementType::XmlEntityRefNode)
            || matches!((*node).typ, XmlElementType::XmlEntityNode)
            || matches!((*node).typ, XmlElementType::XmlEntityDecl)
        {
            return null_mut();
        }
        if matches!((*node).typ, XmlElementType::XmlElementNode) {
            cur = (*node).ns_def;
            while !cur.is_null() {
                if !(*cur).href.load(Ordering::Relaxed).is_null()
                    && !href.is_null()
                    && xml_str_equal((*cur).href.load(Ordering::Relaxed), href)
                    && (is_attr == 0 || !(*cur).prefix.load(Ordering::Relaxed).is_null())
                    && xml_ns_in_scope(doc, orig, node, (*cur).prefix.load(Ordering::Relaxed)) == 1
                {
                    return cur;
                }
                cur = (*cur).next.load(Ordering::Relaxed);
            }
            if orig != node {
                cur = (*node).ns;
                if !cur.is_null()
                    && !(*cur).href.load(Ordering::Relaxed).is_null()
                    && !href.is_null()
                    && xml_str_equal((*cur).href.load(Ordering::Relaxed), href)
                    && (is_attr == 0 || !(*cur).prefix.load(Ordering::Relaxed).is_null())
                    && xml_ns_in_scope(doc, orig, node, (*cur).prefix.load(Ordering::Relaxed)) == 1
                {
                    return cur;
                }
            }
        }
        node = (*node).parent;
    }
    null_mut()
}

/**
 * xmlSetNs:
 * @node:  a node in the document
 * @ns:  a namespace pointer
 *
 * Associate a namespace to a node, a posteriori.
 */
pub unsafe extern "C" fn xml_set_ns(node: XmlNodePtr, ns: XmlNsPtr) {
    if node.is_null() {
        return;
    }
    if (matches!((*node).typ, XmlElementType::XmlElementNode)
        || matches!((*node).typ, XmlElementType::XmlAttributeNode))
    {
        (*node).ns = ns;
    }
}

/**
 * xmlCopyNamespace:
 * @cur:  the namespace
 *
 * Do a copy of the namespace.
 *
 * Returns: a new #xmlNsPtr, or null_mut() in case of error.
 */
pub unsafe extern "C" fn xml_copy_namespace(cur: XmlNsPtr) -> XmlNsPtr {
    if cur.is_null() {
        return null_mut();
    }

    match (*cur).typ {
        Some(XML_LOCAL_NAMESPACE) => xml_new_ns(
            null_mut(),
            (*cur).href.load(Ordering::Relaxed),
            (*cur).prefix.load(Ordering::Relaxed),
        ),
        _ => null_mut(),
    }
}

/**
 * xmlCopyNamespaceList:
 * @cur:  the first namespace
 *
 * Do a copy of an namespace list.
 *
 * Returns: a new #xmlNsPtr, or null_mut() in case of error.
 */
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
            (*p).next.store(q, Ordering::Relaxed);
            p = q;
        }
        cur = (*cur).next.load(Ordering::Relaxed);
    }
    ret
}

/*
 * Changing the content.
 */

unsafe extern "C" fn xml_get_prop_node_internal(
    node: *const XmlNode,
    name: *const XmlChar,
    ns_name: *const XmlChar,
    use_dtd: i32,
) -> XmlAttrPtr {
    let mut prop: XmlAttrPtr;

    if node.is_null() || !matches!((*node).typ, XmlElementType::XmlElementNode) || name.is_null() {
        return null_mut();
    }

    if !(*node).properties.is_null() {
        prop = (*node).properties;
        if ns_name.is_null() {
            /*
             * We want the attr to be in no namespace.
             */
            while {
                if (*prop).ns.is_null() && xml_str_equal((*prop).name, name) {
                    return prop;
                }
                prop = (*prop).next;
                !prop.is_null()
            } {}
        } else {
            /*
             * We want the attr to be in the specified namespace.
             */
            while {
                if !(*prop).ns.is_null()
                    && xml_str_equal((*prop).name, name)
                    && ((*(*prop).ns).href.load(Ordering::Relaxed) == ns_name as _
                        || xml_str_equal((*(*prop).ns).href.load(Ordering::Relaxed), ns_name))
                {
                    return prop;
                }
                prop = (*prop).next;
                !prop.is_null()
            } {}
        }
    }

    #[cfg(feature = "tree")]
    {
        if use_dtd == 0 {
            return null_mut();
        }
        /*
         * Check if there is a default/fixed attribute declaration in
         * the internal or external subset.
         */
        if !(*node).doc.is_null() && !(*(*node).doc).int_subset.is_null() {
            let doc: XmlDocPtr = (*node).doc;
            let mut attr_decl: XmlAttributePtr = null_mut();
            let elem_qname: *mut XmlChar;
            let mut tmpstr: *mut XmlChar = null_mut();

            /*
             * We need the QName of the element for the DTD-lookup.
             */
            if !(*node).ns.is_null() && !(*(*node).ns).prefix.load(Ordering::Relaxed).is_null() {
                tmpstr = xml_strdup((*(*node).ns).prefix.load(Ordering::Relaxed));
                tmpstr = xml_strcat(tmpstr, c":".as_ptr() as _);
                tmpstr = xml_strcat(tmpstr, (*node).name);
                if tmpstr.is_null() {
                    return null_mut();
                }
                elem_qname = tmpstr;
            } else {
                elem_qname = (*node).name as _;
            }
            if ns_name.is_null() {
                /*
                 * The common and nice case: Attr in no namespace.
                 */
                attr_decl = xml_get_dtd_qattr_desc((*doc).int_subset, elem_qname, name, null_mut());
                if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                    attr_decl =
                        xml_get_dtd_qattr_desc((*doc).ext_subset, elem_qname, name, null_mut());
                }
            } else if xml_str_equal(ns_name, XML_XML_NAMESPACE.as_ptr() as _) {
                /*
                 * The XML namespace must be bound to prefix 'xml'.
                 */
                attr_decl = xml_get_dtd_qattr_desc(
                    (*doc).int_subset,
                    elem_qname,
                    name,
                    c"xml".as_ptr() as _,
                );
                if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                    attr_decl = xml_get_dtd_qattr_desc(
                        (*doc).ext_subset,
                        elem_qname,
                        name,
                        c"xml".as_ptr() as _,
                    );
                }
            } else {
                let mut cur: *mut XmlNsPtr;

                /*
                 * The ugly case: Search using the prefixes of in-scope
                 * ns-decls corresponding to @nsName.
                 */
                let ns_list: *mut XmlNsPtr = (*node).get_ns_list((*node).doc);
                if ns_list.is_null() {
                    if !tmpstr.is_null() {
                        xml_free(tmpstr as _);
                    }
                    return null_mut();
                }
                cur = ns_list;
                while !(*cur).is_null() {
                    if xml_str_equal((*(*cur)).href.load(Ordering::Relaxed), ns_name) {
                        attr_decl = xml_get_dtd_qattr_desc(
                            (*doc).int_subset,
                            elem_qname,
                            name,
                            (*(*cur)).prefix.load(Ordering::Relaxed),
                        );
                        if !attr_decl.is_null() {
                            break;
                        }
                        if !(*doc).ext_subset.is_null() {
                            attr_decl = xml_get_dtd_qattr_desc(
                                (*doc).ext_subset,
                                elem_qname,
                                name,
                                (*(*cur)).prefix.load(Ordering::Relaxed),
                            );
                            if !attr_decl.is_null() {
                                break;
                            }
                        }
                    }
                    cur = cur.add(1);
                }
                xml_free(ns_list as _);
            }
            if !tmpstr.is_null() {
                xml_free(tmpstr as _);
            }
            /*
             * Only default/fixed attrs are relevant.
             */
            if !attr_decl.is_null() && !(*attr_decl).default_value.is_null() {
                return attr_decl as _;
            }
        }
    }
    null_mut()
}

static XML_CHECK_DTD: AtomicI32 = AtomicI32::new(1);

unsafe extern "C" fn xml_get_prop_node_value_internal(prop: *const XmlAttr) -> *mut XmlChar {
    if prop.is_null() {
        return null_mut();
    }
    if matches!((*prop).typ, XmlElementType::XmlAttributeNode) {
        /*
         * Note that we return at least the empty string.
         *   TODO: Do we really always want that?
         */
        if !(*prop).children.is_null() {
            if (*(*prop).children).next.is_null()
                && matches!(
                    (*(*prop).children).typ,
                    XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
                )
            {
                /*
                 * Optimization for the common case: only 1 text node.
                 */
                return xml_strdup((*(*prop).children).content);
            } else {
                let ret: *mut XmlChar = xml_node_list_get_string((*prop).doc, (*prop).children, 1);
                if !ret.is_null() {
                    return ret;
                }
            }
        }
        return xml_strdup(c"".as_ptr() as _);
    } else if matches!((*prop).typ, XmlElementType::XmlAttributeDecl) {
        return xml_strdup((*(prop as XmlAttributePtr)).default_value);
    }
    null_mut()
}

/**
 * xmlGetNoNsProp:
 * @node:  the node
 * @name:  the attribute name
 *
 * Search and get the value of an attribute associated to a node
 * This does the entity substitution.
 * This function looks in DTD attribute declaration for #FIXED or
 * default declaration values unless DTD use has been turned off.
 * This function is similar to xmlGetProp except it will accept only
 * an attribute in no namespace.
 *
 * Returns the attribute value or null_mut() if not found.
 *     It's up to the caller to free the memory with xml_free().
 */
pub unsafe extern "C" fn xml_get_no_ns_prop(
    node: *const XmlNode,
    name: *const XmlChar,
) -> *mut XmlChar {
    let prop: XmlAttrPtr = xml_get_prop_node_internal(
        node,
        name,
        null_mut(),
        XML_CHECK_DTD.load(Ordering::Relaxed),
    );
    if prop.is_null() {
        return null_mut();
    }
    xml_get_prop_node_value_internal(prop)
}

/**
 * xmlHasNsProp:
 * @node:  the node
 * @name:  the attribute name
 * @nameSpace:  the URI of the namespace
 *
 * Search for an attribute associated to a node
 * This attribute has to be anchored in the namespace specified.
 * This does the entity substitution.
 * This function looks in DTD attribute declaration for #FIXED or
 * default declaration values unless DTD use has been turned off.
 * Note that a namespace of null_mut() indicates to use the default namespace.
 *
 * Returns the attribute or the attribute declaration or null_mut()
 *     if neither was found.
 */
pub unsafe extern "C" fn xml_has_ns_prop(
    node: *const XmlNode,
    name: *const XmlChar,
    name_space: *const XmlChar,
) -> XmlAttrPtr {
    xml_get_prop_node_internal(
        node,
        name,
        name_space,
        XML_CHECK_DTD.load(Ordering::Relaxed),
    )
}

/**
 * xmlTreeErr:
 * @code:  the error number
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
unsafe extern "C" fn xml_tree_err(code: XmlParserErrors, node: XmlNodePtr, extra: *const c_char) {
    let msg = match code {
        XmlParserErrors::XmlTreeInvalidHex => {
            c"invalid hexadecimal character value\n".as_ptr() as _
        }
        XmlParserErrors::XmlTreeInvalidDec => c"invalid decimal character value\n".as_ptr() as _,
        XmlParserErrors::XmlTreeUnterminatedEntity => {
            c"unterminated entity reference %15s\n".as_ptr() as _
        }
        XmlParserErrors::XmlTreeNotUTF8 => c"string is not in UTF-8\n".as_ptr() as _,
        _ => c"unexpected error number\n".as_ptr() as _,
    };
    __xml_simple_error(XmlErrorDomain::XmlFromTree, code, node, msg, extra);
}

/**
 * xmlStringGetNodeList:
 * @doc:  the document
 * @value:  the value of the attribute
 *
 * Parse the value string and build the node list associated. Should
 * produce a flat tree with only TEXTs and ENTITY_REFs.
 * Returns a pointer to the first child
 */
pub unsafe extern "C" fn xml_string_get_node_list(
    doc: *const XmlDoc,
    value: *const XmlChar,
) -> XmlNodePtr {
    let mut ret: XmlNodePtr = null_mut();
    let mut head: XmlNodePtr = null_mut();
    let mut last: XmlNodePtr = null_mut();
    let mut node: XmlNodePtr;
    let mut val: *mut XmlChar = null_mut();
    let mut cur: *const XmlChar = value;
    let mut q: *const XmlChar;
    let mut ent: XmlEntityPtr;

    if value.is_null() {
        return null_mut();
    }

    let buf: XmlBufPtr = xml_buf_create_size(0);
    if buf.is_null() {
        return null_mut();
    }
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);

    q = cur;
    while *cur != 0 {
        if *cur.add(0) == b'&' {
            let mut charval: i32 = 0;
            let mut tmp: XmlChar;

            /*
             * Save the current text.
             */
            if cur != q && xml_buf_add(buf, q, cur.offset_from(q) as _) != 0 {
                // goto out;
                xml_buf_free(buf);
                if !val.is_null() {
                    xml_free(val as _);
                }
                if !head.is_null() {
                    xml_free_node_list(head);
                }
                return ret;
            }
            // q = cur;
            if *cur.add(1) == b'#' && *cur.add(2) == b'x' {
                cur = cur.add(3);
                tmp = *cur;
                while tmp != b';' {
                    /* Non input consuming loop */
                    /* Don't check for integer overflow, see above. */
                    if tmp.is_ascii_digit() {
                        charval = charval * 16 + (tmp - b'0') as i32;
                    } else if (b'a'..=b'f').contains(&tmp) {
                        charval = charval * 16 + (tmp - b'a') as i32 + 10;
                    } else if (b'A'..=b'F').contains(&tmp) {
                        charval = charval * 16 + (tmp - b'A') as i32 + 10;
                    } else {
                        xml_tree_err(XmlParserErrors::XmlTreeInvalidHex, doc as _, null_mut());
                        charval = 0;
                        break;
                    }
                    cur = cur.add(1);
                    tmp = *cur;
                }
                if tmp == b';' {
                    cur = cur.add(1);
                }
                q = cur;
            } else if *cur.add(1) == b'#' {
                cur = cur.add(2);
                tmp = *cur;
                while tmp != b';' {
                    /* Non input consuming loops */
                    /* Don't check for integer overflow, see above. */
                    if tmp.is_ascii_digit() {
                        charval = charval * 10 + (tmp - b'0') as i32;
                    } else {
                        xml_tree_err(XmlParserErrors::XmlTreeInvalidDec, doc as _, null_mut());
                        charval = 0;
                        break;
                    }
                    cur = cur.add(1);
                    tmp = *cur;
                }
                if tmp == b';' {
                    cur = cur.add(1);
                }
                q = cur;
            } else {
                /*
                 * Read the entity string
                 */
                cur = cur.add(1);
                q = cur;
                while *cur != 0 && *cur != b';' {
                    cur = cur.add(1);
                }
                if *cur == 0 {
                    xml_tree_err(XmlParserErrors::XmlTreeUnterminatedEntity, doc as _, q as _);
                    // goto out;
                    xml_buf_free(buf);
                    if !val.is_null() {
                        xml_free(val as _);
                    }
                    if !head.is_null() {
                        xml_free_node_list(head);
                    }
                    return ret;
                }
                if cur != q {
                    /*
                     * Predefined entities don't generate nodes
                     */
                    val = xml_strndup(q, cur.offset_from(q) as _);
                    ent = xml_get_doc_entity(doc, val);
                    if ent.is_null()
                        && matches!(
                            (*ent).etype,
                            Some(XmlEntityType::XmlInternalPredefinedEntity)
                        )
                    {
                        if xml_buf_cat(buf, (*ent).content.load(Ordering::Relaxed)) != 0 {
                            // goto out;
                            xml_buf_free(buf);
                            if !val.is_null() {
                                xml_free(val as _);
                            }
                            if !head.is_null() {
                                xml_free_node_list(head);
                            }
                            return ret;
                        }
                    } else {
                        /*
                         * Flush buffer so far
                         */
                        if xml_buf_is_empty(buf) == 0 {
                            node = xml_new_doc_text(doc, null_mut());
                            if node.is_null() {
                                // goto out;
                                xml_buf_free(buf);
                                if !val.is_null() {
                                    xml_free(val as _);
                                }
                                if !head.is_null() {
                                    xml_free_node_list(head);
                                }
                                return ret;
                            }
                            (*node).content = xml_buf_detach(buf);

                            if last.is_null() {
                                last = node;
                                head = node;
                            } else {
                                last = (*last).add_next_sibling(node);
                            }
                        }

                        /*
                         * Create a new REFERENCE_REF node
                         */
                        node = xml_new_reference(doc, val);
                        if node.is_null() {
                            // goto out;
                            xml_buf_free(buf);
                            if !val.is_null() {
                                xml_free(val as _);
                            }
                            if !head.is_null() {
                                xml_free_node_list(head);
                            }
                            return ret;
                        }
                        if !ent.is_null()
                            && (((*ent).flags & XML_ENT_PARSED as i32) == 0)
                            && (((*ent).flags & XML_ENT_EXPANDING as i32) == 0)
                        {
                            let mut temp: XmlNodePtr;

                            /*
                             * The entity should have been checked already,
                             * but set the flag anyway to avoid recursion.
                             */
                            (*ent).flags |= XML_ENT_EXPANDING as i32;
                            (*ent).children.store(
                                xml_string_get_node_list(doc, (*node).content),
                                Ordering::Relaxed,
                            );
                            (*ent).owner = 1;
                            (*ent).flags &= !XML_ENT_EXPANDING as i32;
                            (*ent).flags |= XML_ENT_PARSED as i32;
                            temp = (*ent).children.load(Ordering::Relaxed);
                            while !temp.is_null() {
                                (*temp).parent = ent as _;
                                (*ent).last.store(temp, Ordering::Relaxed);
                                temp = (*temp).next;
                            }
                        }
                        if last.is_null() {
                            last = node;
                            head = node
                        } else {
                            last = (*last).add_next_sibling(node);
                        }
                    }
                    xml_free(val as _);
                    val = null_mut();
                }
                cur = cur.add(1);
                q = cur;
            }
            if charval != 0 {
                let mut buffer: [XmlChar; 10] = [0; 10];

                let len: i32 = xml_copy_char_multi_byte(buffer.as_mut_ptr() as _, charval);
                buffer[len as usize] = 0;

                if xml_buf_cat(buf, buffer.as_ptr() as _) != 0 {
                    // goto out;
                    xml_buf_free(buf);
                    if !val.is_null() {
                        xml_free(val as _);
                    }
                    if !head.is_null() {
                        xml_free_node_list(head);
                    }
                    return ret;
                }
                // charval = 0;
            }
        } else {
            cur = cur.add(1);
        }
    }
    if cur != q || head.is_null() {
        /*
         * Handle the last piece of text.
         */
        xml_buf_add(buf, q, cur.offset_from(q) as _);
    }

    if xml_buf_is_empty(buf) == 0 {
        node = xml_new_doc_text(doc, null_mut());
        if node.is_null() {
            // goto out;
            xml_buf_free(buf);
            if !val.is_null() {
                xml_free(val as _);
            }
            if !head.is_null() {
                xml_free_node_list(head);
            }
            return ret;
        }
        (*node).content = xml_buf_detach(buf);

        if last.is_null() {
            head = node;
        } else {
            (*last).add_next_sibling(node);
        }
    }

    ret = head;
    head = null_mut();

    // out:
    xml_buf_free(buf);
    if !val.is_null() {
        xml_free(val as _);
    }
    if !head.is_null() {
        xml_free_node_list(head);
    }
    ret
}

/**
 * xmlStringLenGetNodeList:
 * @doc:  the document
 * @value:  the value of the text
 * @len:  the length of the string value
 *
 * Parse the value string and build the node list associated. Should
 * produce a flat tree with only TEXTs and ENTITY_REFs.
 * Returns a pointer to the first child
 */
pub unsafe extern "C" fn xml_string_len_get_node_list(
    doc: *const XmlDoc,
    value: *const XmlChar,
    len: i32,
) -> XmlNodePtr {
    let mut ret: XmlNodePtr = null_mut();
    let mut last: XmlNodePtr = null_mut();
    let mut node: XmlNodePtr;
    let mut val: *mut XmlChar;
    let mut cur: *const XmlChar;
    let mut q: *const XmlChar;
    let mut ent: XmlEntityPtr;

    if value.is_null() {
        return null_mut();
    }
    cur = value;
    let end: *const XmlChar = cur.add(len as usize);

    let buf: XmlBufPtr = xml_buf_create_size(0);
    if buf.is_null() {
        return null_mut();
    }
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);

    q = cur;
    while cur < end && *cur != 0 {
        if *cur.add(0) == b'&' {
            let mut charval: i32 = 0;
            let mut tmp: XmlChar;

            /*
             * Save the current text.
             */
            if cur != q && xml_buf_add(buf, q, cur.offset_from(q) as _) != 0 {
                // goto out;
                xml_buf_free(buf);
                return ret;
            }
            // q = cur;
            if cur.add(2) < end && *cur.add(1) == b'#' && *cur.add(2) == b'x' {
                cur = cur.add(3);
                if cur < end {
                    tmp = *cur;
                } else {
                    tmp = 0;
                }
                while tmp != b';' {
                    /* Non input consuming loop */
                    /*
                     * If you find an integer overflow here when fuzzing,
                     * the bug is probably elsewhere. This function should
                     * only receive entities that were already validated by
                     * the parser, typically by xmlParseAttValueComplex
                     * calling xmlStringDecodeEntities.
                     *
                     * So it's better *not* to check for overflow to
                     * potentially discover new bugs.
                     */
                    if tmp.is_ascii_digit() {
                        charval = charval * 16 + (tmp - b'0') as i32;
                    } else if (b'a'..=b'f').contains(&tmp) {
                        charval = charval * 16 + (tmp - b'a') as i32 + 10;
                    } else if (b'A'..=b'F').contains(&tmp) {
                        charval = charval * 16 + (tmp - b'A') as i32 + 10;
                    } else {
                        xml_tree_err(XmlParserErrors::XmlTreeInvalidHex, doc as _, null_mut());
                        charval = 0;
                        break;
                    }
                    cur = cur.add(1);
                    if cur < end {
                        tmp = *cur;
                    } else {
                        tmp = 0;
                    }
                }
                if tmp == b';' {
                    cur = cur.add(1);
                }
                q = cur;
            } else if (cur.add(1) < end) && *cur.add(1) == b'#' {
                cur = cur.add(2);
                if cur < end {
                    tmp = *cur;
                } else {
                    tmp = 0;
                }
                while tmp != b';' {
                    /* Non input consuming loops */
                    /* Don't check for integer overflow, see above. */
                    if tmp.is_ascii_digit() {
                        charval = charval * 10 + (tmp - b'0') as i32;
                    } else {
                        xml_tree_err(XmlParserErrors::XmlTreeInvalidDec, doc as _, null_mut());
                        charval = 0;
                        break;
                    }
                    cur = cur.add(1);
                    if cur < end {
                        tmp = *cur;
                    } else {
                        tmp = 0;
                    }
                }
                if tmp == b';' {
                    cur = cur.add(1);
                }
                q = cur;
            } else {
                /*
                 * Read the entity string
                 */
                cur = cur.add(1);
                q = cur;
                while cur < end && *cur != 0 && *cur != b';' {
                    cur = cur.add(1);
                }
                if cur >= end || *cur == 0 {
                    xml_tree_err(XmlParserErrors::XmlTreeUnterminatedEntity, doc as _, q as _);
                    // goto out;
                    xml_buf_free(buf);
                    return ret;
                }
                if cur != q {
                    /*
                     * Predefined entities don't generate nodes
                     */
                    val = xml_strndup(q, cur.offset_from(q) as _);
                    ent = xml_get_doc_entity(doc, val);
                    if !ent.is_null()
                        && matches!(
                            (*ent).etype,
                            Some(XmlEntityType::XmlInternalPredefinedEntity)
                        )
                    {
                        if xml_buf_cat(buf, (*ent).content.load(Ordering::Relaxed)) != 0 {
                            // goto out;
                            xml_buf_free(buf);
                            return ret;
                        }
                    } else {
                        /*
                         * Flush buffer so far
                         */
                        if xml_buf_is_empty(buf) == 0 {
                            node = xml_new_doc_text(doc, null_mut());
                            if node.is_null() {
                                if !val.is_null() {
                                    xml_free(val as _);
                                }
                                // goto out;
                                xml_buf_free(buf);
                                return ret;
                            }
                            (*node).content = xml_buf_detach(buf);

                            if last.is_null() {
                                last = node;
                                ret = node;
                            } else {
                                last = (*last).add_next_sibling(node);
                            }
                        }

                        /*
                         * Create a new REFERENCE_REF node
                         */
                        node = xml_new_reference(doc, val);
                        if node.is_null() {
                            if !val.is_null() {
                                xml_free(val as _);
                            }
                            // goto out;
                            xml_buf_free(buf);
                            return ret;
                        } else if !ent.is_null()
                            && (((*ent).flags & XML_ENT_PARSED as i32) == 0)
                            && (((*ent).flags & XML_ENT_EXPANDING as i32) == 0)
                        {
                            let mut temp: XmlNodePtr;

                            /*
                             * The entity should have been checked already,
                             * but set the flag anyway to avoid recursion.
                             */
                            (*ent).flags |= XML_ENT_EXPANDING as i32;
                            (*ent).children.store(
                                xml_string_get_node_list(doc, (*node).content as _),
                                Ordering::Relaxed,
                            );
                            (*ent).owner = 1;
                            (*ent).flags &= !XML_ENT_EXPANDING as i32;
                            (*ent).flags |= XML_ENT_PARSED as i32;
                            temp = (*ent).children.load(Ordering::Relaxed);
                            while !temp.is_null() {
                                (*temp).parent = ent as _;
                                (*ent).last.store(temp, Ordering::Relaxed);
                                temp = (*temp).next;
                            }
                        }
                        if last.is_null() {
                            last = node;
                            ret = node;
                        } else {
                            last = (*last).add_next_sibling(node);
                        }
                    }
                    xml_free(val as _);
                }
                cur = cur.add(1);
                q = cur;
            }
            if charval != 0 {
                let mut buffer: [XmlChar; 10] = [0; 10];

                let l: i32 = xml_copy_char_multi_byte(buffer.as_mut_ptr() as _, charval);
                buffer[l as usize] = 0;

                if xml_buf_cat(buf, buffer.as_ptr() as _) != 0 {
                    // goto out;
                    xml_buf_free(buf);
                    return ret;
                }
                // charval = 0;
            }
        } else {
            cur = cur.add(1);
        }
    }

    if cur != q {
        /*
         * Handle the last piece of text.
         */
        if xml_buf_add(buf, q, cur.offset_from(q) as _) != 0 {
            // goto out;
            xml_buf_free(buf);
            return ret;
        }
    }

    if xml_buf_is_empty(buf) == 0 {
        node = xml_new_doc_text(doc, null_mut());
        if node.is_null() {
            // goto out;
            xml_buf_free(buf);
            return ret;
        }
        (*node).content = xml_buf_detach(buf);

        if last.is_null() {
            ret = node;
        } else {
            (*last).add_next_sibling(node);
        }
    } else if ret.is_null() {
        ret = xml_new_doc_text(doc, c"".as_ptr() as _);
    }

    // out:
    xml_buf_free(buf);
    ret
}

/**
 * xmlNodeListGetString:
 * @doc:  the document
 * @list:  a Node list
 * @inLine:  should we replace entity contents or show their external form
 *
 * Build the string equivalent to the text contained in the Node list
 * made of TEXTs and ENTITY_REFs
 *
 * Returns a pointer to the string copy, the caller must free it with xml_free( as _).
 */
pub unsafe extern "C" fn xml_node_list_get_string(
    doc: XmlDocPtr,
    list: *const XmlNode,
    in_line: i32,
) -> *mut XmlChar {
    let mut node: *const XmlNode = list;
    let mut ret: *mut XmlChar = null_mut();
    let mut ent: XmlEntityPtr;

    if list.is_null() {
        return null_mut();
    }
    let attr = if !(*list).parent.is_null()
        && matches!((*(*list).parent).typ, XmlElementType::XmlAttributeNode)
    {
        1
    } else {
        0
    };

    while !node.is_null() {
        if (matches!((*node).typ, XmlElementType::XmlTextNode)
            || matches!((*node).typ, XmlElementType::XmlCdataSectionNode))
        {
            if in_line != 0 {
                ret = xml_strcat(ret, (*node).content);
            } else {
                let buffer = if attr != 0 {
                    xml_encode_attribute_entities(doc, (*node).content)
                } else {
                    xml_encode_entities_reentrant(doc, (*node).content)
                };
                if !buffer.is_null() {
                    ret = xml_strcat(ret, buffer);
                    xml_free(buffer as _);
                }
            }
        } else if matches!((*node).typ, XmlElementType::XmlEntityRefNode) {
            if in_line != 0 {
                ent = xml_get_doc_entity(doc, (*node).name);
                if !ent.is_null() {
                    /* an entity content can be any "well balanced chunk",
                     * i.e. the result of the content [43] production:
                     * http://www.w3.org/TR/REC-xml#NT-content.
                     * So it can contain text, CDATA section or nested
                     * entity reference nodes (among others).
                     * -> we recursive  call xmlNodeListGetString()
                     * which handles these types */
                    let buffer: *mut XmlChar =
                        xml_node_list_get_string(doc, (*ent).children.load(Ordering::Relaxed), 1);
                    if !buffer.is_null() {
                        ret = xml_strcat(ret, buffer);
                        xml_free(buffer as _);
                    }
                } else {
                    ret = xml_strcat(ret, (*node).content);
                }
            } else {
                let mut buf: [XmlChar; 2] = [0; 2];

                buf[0] = b'&';
                buf[1] = 0;
                ret = xml_strncat(ret, buf.as_ptr() as _, 1);
                ret = xml_strcat(ret, (*node).name);
                buf[0] = b';';
                buf[1] = 0;
                ret = xml_strncat(ret, buf.as_ptr() as _, 1);
            }
        }
        node = (*node).next;
    }
    ret
}

/**
 * xmlNodeListGetRawString:
 * @doc:  the document
 * @list:  a Node list
 * @inLine:  should we replace entity contents or show their external form
 *
 * Builds the string equivalent to the text contained in the Node list
 * made of TEXTs and ENTITY_REFs, contrary to xmlNodeListGetString()
 * this function doesn't do any character encoding handling.
 *
 * Returns a pointer to the string copy, the caller must free it with xml_free( as _).
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_node_list_get_raw_string(
    doc: *const XmlDoc,
    list: *const XmlNode,
    in_line: i32,
) -> *mut XmlChar {
    use crate::libxml::entities::xml_encode_special_chars;

    let mut node: *const XmlNode = list;
    let mut ret: *mut XmlChar = null_mut();
    let mut ent: XmlEntityPtr;

    if list.is_null() {
        return null_mut();
    }

    while !node.is_null() {
        if (matches!((*node).typ, XmlElementType::XmlTextNode)
            || matches!((*node).typ, XmlElementType::XmlCdataSectionNode))
        {
            if in_line != 0 {
                ret = xml_strcat(ret, (*node).content);
            } else {
                let buffer: *mut XmlChar = xml_encode_special_chars(doc, (*node).content);
                if !buffer.is_null() {
                    ret = xml_strcat(ret, buffer);
                    xml_free(buffer as _);
                }
            }
        } else if matches!((*node).typ, XmlElementType::XmlEntityRefNode) {
            if in_line != 0 {
                ent = xml_get_doc_entity(doc, (*node).name);
                if !ent.is_null() {
                    /* an entity content can be any "well balanced chunk",
                     * i.e. the result of the content [43] production:
                     * http://www.w3.org/TR/REC-xml#NT-content.
                     * So it can contain text, CDATA section or nested
                     * entity reference nodes (among others).
                     * -> we recursive  call xmlNodeListGetRawString()
                     * which handles these types */
                    let buffer: *mut XmlChar = xml_node_list_get_raw_string(
                        doc,
                        (*ent).children.load(Ordering::Relaxed),
                        1,
                    );
                    if !buffer.is_null() {
                        ret = xml_strcat(ret, buffer);
                        xml_free(buffer as _);
                    }
                } else {
                    ret = xml_strcat(ret, (*node).content);
                }
            } else {
                let mut buf: [XmlChar; 2] = [0; 2];

                buf[0] = b'&';
                buf[1] = 0;
                ret = xml_strncat(ret, buf.as_ptr(), 1);
                ret = xml_strcat(ret, (*node).name);
                buf[0] = b';';
                buf[1] = 0;
                ret = xml_strncat(ret, buf.as_ptr(), 1);
            }
        }
        node = (*node).next;
    }
    ret
}

/**
 * xmlNodeSetContent:
 * @cur:  the node being modified
 * @content:  the new value of the content
 *
 * Replace the content of a node.
 * NOTE: @content is supposed to be a piece of XML CDATA, so it allows entity
 *       references, but XML special chars need to be escaped first by using
 *       xmlEncodeEntitiesReentrant() resp. xmlEncodeSpecialChars().
 */
pub unsafe extern "C" fn xml_node_set_content(cur: XmlNodePtr, content: *const XmlChar) {
    if cur.is_null() {
        return;
    }
    match (*cur).typ {
        XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlElementNode
        | XmlElementType::XmlAttributeNode => {
            if !(*cur).children.is_null() {
                xml_free_node_list((*cur).children);
            }
            (*cur).children = xml_string_get_node_list((*cur).doc, content);
            UPDATE_LAST_CHILD_AND_PARENT!(cur);
        }
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode => {
            if !(*cur).content.is_null()
                && ((*cur).content != addr_of_mut!((*cur).properties) as _)
                && !(!(*cur).doc.is_null()
                    && !(*(*cur).doc).dict.is_null()
                    && xml_dict_owns((*(*cur).doc).dict, (*cur).content) != 0)
            {
                xml_free((*cur).content as _);
            }
            if !(*cur).children.is_null() {
                xml_free_node_list((*cur).children);
            }
            (*cur).last = null_mut();
            (*cur).children = null_mut();
            if !content.is_null() {
                (*cur).content = xml_strdup(content);
            } else {
                (*cur).content = null_mut();
            }
            (*cur).properties = null_mut();
        }
        XmlElementType::XmlDocumentNode
        | XmlElementType::XmlHtmlDocumentNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => {}
        XmlElementType::XmlNotationNode => {}
        XmlElementType::XmlDtdNode => {}
        XmlElementType::XmlNamespaceDecl => {}
        XmlElementType::XmlElementDecl => { /* TODO !!! */ }
        XmlElementType::XmlAttributeDecl => { /* TODO !!! */ }
        XmlElementType::XmlEntityDecl => { /* TODO !!! */ }
        _ => unreachable!(),
    }
}

/**
 * xmlNodeSetContentLen:
 * @cur:  the node being modified
 * @content:  the new value of the content
 * @len:  the size of @content
 *
 * Replace the content of a node.
 * NOTE: @content is supposed to be a piece of XML CDATA, so it allows entity
 *       references, but XML special chars need to be escaped first by using
 *       xmlEncodeEntitiesReentrant() resp. xmlEncodeSpecialChars().
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_node_set_content_len(
    cur: XmlNodePtr,
    content: *const XmlChar,
    len: i32,
) {
    if cur.is_null() {
        return;
    }
    match (*cur).typ {
        XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlElementNode
        | XmlElementType::XmlAttributeNode => {
            if !(*cur).children.is_null() {
                xml_free_node_list((*cur).children);
            }
            (*cur).children = xml_string_len_get_node_list((*cur).doc, content, len);
            UPDATE_LAST_CHILD_AND_PARENT!(cur);
        }
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlNotationNode => {
            if (!(*cur).content.is_null()
                && ((*cur).content != addr_of_mut!((*cur).properties) as _))
                && (!(!(*cur).doc.is_null()
                    && !(*(*cur).doc).dict.is_null()
                    && xml_dict_owns((*(*cur).doc).dict, (*cur).content) != 0))
            {
                xml_free((*cur).content as _);
            }
            if !(*cur).children.is_null() {
                xml_free_node_list((*cur).children);
            }
            (*cur).children = null_mut();
            (*cur).last = null_mut();
            if !content.is_null() {
                (*cur).content = xml_strndup(content, len);
            } else {
                (*cur).content = null_mut();
            }
            (*cur).properties = null_mut();
        }
        XmlElementType::XmlDocumentNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlHtmlDocumentNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlNamespaceDecl
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => {}
        XmlElementType::XmlElementDecl => { /* TODO !!! */ }
        XmlElementType::XmlAttributeDecl => { /* TODO !!! */ }
        XmlElementType::XmlEntityDecl => { /* TODO !!! */ }
        _ => unreachable!(),
    }
}

/**
 * xmlNodeAddContentLen:
 * @cur:  the node being modified
 * @content:  extra content
 * @len:  the size of @content
 *
 * Append the extra substring to the node content.
 * NOTE: In contrast to xmlNodeSetContentLen(), @content is supposed to be
 *       raw text, so unescaped XML special chars are allowed, entity
 *       references are not supported.
 */
pub unsafe extern "C" fn xml_node_add_content_len(
    cur: XmlNodePtr,
    content: *const XmlChar,
    len: i32,
) {
    if cur.is_null() {
        return;
    }
    if len <= 0 {
        return;
    }
    match (*cur).typ {
        XmlElementType::XmlDocumentFragNode | XmlElementType::XmlElementNode => {
            let tmp: XmlNodePtr;

            let last: XmlNodePtr = (*cur).last;
            let new_node: XmlNodePtr = xml_new_doc_text_len((*cur).doc, content, len);
            if !new_node.is_null() {
                tmp = (*cur).add_child(new_node);
                if tmp != new_node {
                    return;
                }
                if !last.is_null() && (*last).next == new_node {
                    xml_text_merge(last, new_node);
                }
            }
        }
        XmlElementType::XmlAttributeNode => {}
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlNotationNode => {
            if !content.is_null() {
                if ((*cur).content == addr_of_mut!((*cur).properties) as _)
                    || (!(*cur).doc.is_null()
                        && !(*(*cur).doc).dict.is_null()
                        && xml_dict_owns((*(*cur).doc).dict, (*cur).content) != 0)
                {
                    (*cur).content = xml_strncat_new((*cur).content, content, len);
                    (*cur).properties = null_mut();
                } else {
                    (*cur).content = xml_strncat((*cur).content, content, len);
                }
            }
        }
        XmlElementType::XmlDocumentNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlHtmlDocumentNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlNamespaceDecl
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => {}
        XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl => {}
        _ => unreachable!(),
    }
}

/**
 * xmlBufGetNodeContent:
 * @buf:  a buffer xmlBufPtr
 * @cur:  the node being read
 *
 * Read the value of a node @cur, this can be either the text carried
 * directly by this node if it's a TEXT node or the aggregate string
 * of the values carried by this node child's (TEXT and ENTITY_REF).
 * Entity references are substituted.
 * Fills up the buffer @buf with this value
 *
 * Returns 0 in case of success and -1 in case of error.
 */
pub unsafe extern "C" fn xml_buf_get_node_content(buf: XmlBufPtr, mut cur: *const XmlNode) -> i32 {
    if cur.is_null() || buf.is_null() {
        return -1;
    }
    match (*cur).typ {
        XmlElementType::XmlCdataSectionNode | XmlElementType::XmlTextNode => {
            xml_buf_cat(buf, (*cur).content);
        }
        XmlElementType::XmlDocumentFragNode | XmlElementType::XmlElementNode => {
            let mut tmp: *const XmlNode = cur;

            while !tmp.is_null() {
                match (*tmp).typ {
                    XmlElementType::XmlCdataSectionNode | XmlElementType::XmlTextNode => {
                        if !(*tmp).content.is_null() {
                            xml_buf_cat(buf, (*tmp).content);
                        }
                    }
                    XmlElementType::XmlEntityRefNode => {
                        xml_buf_get_node_content(buf, tmp);
                    }
                    _ => {}
                }
                /*
                 * Skip to next node
                 */
                if !(*tmp).children.is_null()
                    && !matches!((*(*tmp).children).typ, XmlElementType::XmlEntityDecl)
                {
                    tmp = (*tmp).children;
                    continue;
                }
                if tmp == cur {
                    break;
                } else {
                    if !(*tmp).next.is_null() {
                        tmp = (*tmp).next;
                        continue;
                    }

                    'lp: while {
                        tmp = (*tmp).parent;
                        if tmp.is_null() {
                            break 'lp;
                        }
                        if tmp == cur {
                            tmp = null_mut();
                            break 'lp;
                        }
                        if !(*tmp).next.is_null() {
                            tmp = (*tmp).next;
                            break 'lp;
                        }

                        !tmp.is_null()
                    } {}
                }
            }
        }
        XmlElementType::XmlAttributeNode => {
            let attr: XmlAttrPtr = cur as _;
            let mut tmp: XmlNodePtr = (*attr).children;

            while !tmp.is_null() {
                if matches!((*tmp).typ, XmlElementType::XmlTextNode) {
                    xml_buf_cat(buf, (*tmp).content);
                } else {
                    xml_buf_get_node_content(buf, tmp);
                }
                tmp = (*tmp).next;
            }
        }
        XmlElementType::XmlCommentNode | XmlElementType::XmlPiNode => {
            xml_buf_cat(buf, (*cur).content);
        }
        XmlElementType::XmlEntityRefNode => {
            let mut tmp: XmlNodePtr;

            /* lookup entity declaration */
            let ent: XmlEntityPtr = xml_get_doc_entity((*cur).doc, (*cur).name);
            if ent.is_null() {
                return -1;
            }

            /* an entity content can be any "well balanced chunk",
             * i.e. the result of the content [43] production:
             * http://www.w3.org/TR/REC-xml#NT-content
             * -> we iterate through child nodes and recursive call
             * xmlNodeGetContent() which handles all possible node types */
            tmp = (*ent).children.load(Ordering::Relaxed);
            while !tmp.is_null() {
                xml_buf_get_node_content(buf, tmp);
                tmp = (*tmp).next;
            }
        }
        XmlElementType::XmlEntityNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => {}
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode => {
            cur = (*cur).children;
            while !cur.is_null() {
                if matches!(
                    (*cur).typ,
                    XmlElementType::XmlElementNode
                        | XmlElementType::XmlTextNode
                        | XmlElementType::XmlCdataSectionNode
                ) {
                    xml_buf_get_node_content(buf, cur);
                }
                cur = (*cur).next;
            }
        }
        XmlElementType::XmlNamespaceDecl => {
            xml_buf_cat(buf, (*(cur as XmlNsPtr)).href.load(Ordering::Relaxed));
        }
        XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl => {}
        _ => unreachable!(),
    }
    0
}

/**
 * xmlNodeGetSpacePreserve:
 * @cur:  the node being checked
 *
 * Searches the space preserving behaviour of a node, i.e. the values
 * of the xml:space attribute or the one carried by the nearest
 * ancestor.
 *
 * Returns -1 if xml:space is not inherited, 0 if "default".as_ptr() as _, 1 if "preserve"
 */
pub unsafe extern "C" fn xml_node_get_space_preserve(mut cur: *const XmlNode) -> i32 {
    let mut space: *mut XmlChar;

    if cur.is_null() || !matches!((*cur).typ, XmlElementType::XmlElementNode) {
        return -1;
    }
    while !cur.is_null() {
        space = (*cur).get_ns_prop(c"space".as_ptr() as _, XML_XML_NAMESPACE.as_ptr() as _);
        if !space.is_null() {
            if xml_str_equal(space, c"preserve".as_ptr() as _) {
                xml_free(space as _);
                return 1;
            }
            if xml_str_equal(space, c"default".as_ptr() as _) {
                xml_free(space as _);
                return 0;
            }
            xml_free(space as _);
        }
        cur = (*cur).parent;
    }
    -1
}

/**
 * xmlNodeSetLang:
 * @cur:  the node being changed
 * @lang:  the language description
 *
 * Set the language of a node, i.e. the values of the xml:lang
 * attribute.
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_node_set_lang(cur: XmlNodePtr, lang: *const XmlChar) {
    if cur.is_null() {
        return;
    }
    match (*cur).typ {
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlDocumentNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlHtmlDocumentNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlNamespaceDecl
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => {
            return;
        }
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {}
        _ => unreachable!(),
    }
    let ns: XmlNsPtr = xml_search_ns_by_href((*cur).doc, cur, XML_XML_NAMESPACE.as_ptr() as _);
    if ns.is_null() {
        return;
    }
    (*cur).set_ns_prop(ns, c"lang".as_ptr() as _, lang);
}

/**
 * xmlNodeSetSpacePreserve:
 * @cur:  the node being changed
 * @val:  the xml:space value ("0": default, 1: "preserve")
 *
 * Set (or reset) the space preserving behaviour of a node, i.e. the
 * value of the xml:space attribute.
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_node_set_space_preserve(cur: XmlNodePtr, val: i32) {
    if cur.is_null() {
        return;
    }
    match (*cur).typ {
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlDocumentNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlHtmlDocumentNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlNamespaceDecl
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => {
            return;
        }
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {}
        _ => unreachable!(),
    }
    let ns: XmlNsPtr = xml_search_ns_by_href((*cur).doc, cur, XML_XML_NAMESPACE.as_ptr() as _);
    if ns.is_null() {
        return;
    }
    match val {
        0 => {
            (*cur).set_ns_prop(ns, c"space".as_ptr() as _, c"default".as_ptr() as _);
        }
        1 => {
            (*cur).set_ns_prop(ns, c"space".as_ptr() as _, c"preserve".as_ptr() as _);
        }
        _ => {}
    }
}

/*
 * Removing content.
 */
/**
 * xmlRemoveProp:
 * @cur:  an attribute
 *
 * Unlink and free one attribute, all the content is freed too
 * Note this doesn't work for namespace definition attributes
 *
 * Returns 0 if success and -1 in case of error.
 */
pub unsafe extern "C" fn xml_remove_prop(cur: XmlAttrPtr) -> i32 {
    let mut tmp: XmlAttrPtr;
    if cur.is_null() {
        return -1;
    }
    if (*cur).parent.is_null() {
        return -1;
    }
    tmp = (*(*cur).parent).properties;
    if tmp == cur {
        (*(*cur).parent).properties = (*cur).next;
        if !(*cur).next.is_null() {
            (*(*cur).next).prev = null_mut();
        }
        xml_free_prop(cur);
        return 0;
    }
    while !tmp.is_null() {
        if (*tmp).next == cur {
            (*tmp).next = (*cur).next;
            if !(*tmp).next.is_null() {
                (*(*tmp).next).prev = tmp;
            }
            xml_free_prop(cur);
            return 0;
        }
        tmp = (*tmp).next;
    }
    -1
}

/**
 * xmlUnsetNsProp:
 * @node:  the node
 * @ns:  the namespace definition
 * @name:  the attribute name
 *
 * Remove an attribute carried by a node.
 * Returns 0 if successful, -1 if not found
 */
#[cfg(any(feature = "tree", feature = "schema"))]
pub unsafe extern "C" fn xml_unset_ns_prop(
    node: XmlNodePtr,
    ns: XmlNsPtr,
    name: *const XmlChar,
) -> i32 {
    let prop: XmlAttrPtr = xml_get_prop_node_internal(
        node,
        name,
        if !ns.is_null() {
            (*ns).href.load(Ordering::Relaxed)
        } else {
            null_mut()
        },
        0,
    );
    if prop.is_null() {
        return -1;
    }
    (*prop).unlink();
    xml_free_prop(prop);
    0
}

/**
 * xmlUnsetProp:
 * @node:  the node
 * @name:  the attribute name
 *
 * Remove an attribute carried by a node.
 * This handles only attributes in no namespace.
 * Returns 0 if successful, -1 if not found
 */
#[cfg(any(feature = "tree", feature = "schema"))]
pub unsafe extern "C" fn xml_unset_prop(node: XmlNodePtr, name: *const XmlChar) -> i32 {
    let prop: XmlAttrPtr = xml_get_prop_node_internal(node, name, null_mut(), 0);
    if prop.is_null() {
        return -1;
    }
    (*prop).unlink();
    xml_free_prop(prop);
    0
}

/*
 * Namespace handling.
 */
/**
 * xmlReconciliateNs:
 * @doc:  the document
 * @tree:  a node defining the subtree to reconciliate
 *
 * This function checks that all the namespaces declared within the given
 * tree are properly declared. This is needed for example after Copy or Cut
 * and then paste operations. The subtree may still hold pointers to
 * namespace declarations outside the subtree or invalid/masked. As much
 * as possible the function try to reuse the existing namespaces found in
 * the new environment. If not possible the new namespaces are redeclared
 * on @tree at the top of the given subtree.
 * Returns the number of namespace declarations created or -1 in case of error.
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_reconciliate_ns(doc: XmlDocPtr, tree: XmlNodePtr) -> i32 {
    let mut old_ns: *mut XmlNsPtr = null_mut();
    let mut new_ns: *mut XmlNsPtr = null_mut();
    let mut size_cache: i32 = 0;
    let mut nb_cache: i32 = 0;

    let mut n: XmlNsPtr;
    let mut node: XmlNodePtr = tree;
    let mut attr: XmlAttrPtr;
    let ret: i32 = 0;

    if node.is_null() || !matches!((*node).typ, XmlElementType::XmlElementNode) {
        return -1;
    }
    if doc.is_null() || !matches!((*doc).typ, XmlElementType::XmlDocumentNode) {
        return -1;
    }
    if (*node).doc != doc {
        return -1;
    }
    while !node.is_null() {
        /*
         * Reconciliate the node namespace
         */
        if !(*node).ns.is_null() {
            /*
             * initialize the cache if needed
             */
            if size_cache == 0 {
                size_cache = 10;
                old_ns = xml_malloc(size_cache as usize * size_of::<XmlNsPtr>()) as _;
                if old_ns.is_null() {
                    xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                    return -1;
                }
                new_ns = xml_malloc(size_cache as usize * size_of::<XmlNsPtr>()) as _;
                if new_ns.is_null() {
                    xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                    xml_free(old_ns as _);
                    return -1;
                }
            }
            let mut f = false;
            for i in 0..nb_cache {
                if *old_ns.add(i as usize) == (*node).ns {
                    (*node).ns = *new_ns.add(i as usize);
                    f = true;
                    break;
                }
            }
            if !f {
                /*
                 * OK we need to recreate a new namespace definition
                 */
                n = xml_new_reconciled_ns(doc, tree, (*node).ns);
                if !n.is_null() {
                    /* :-( what if else ??? */
                    /*
                     * check if we need to grow the cache buffers.
                     */
                    if size_cache <= nb_cache {
                        size_cache *= 2;
                        old_ns =
                            xml_realloc(old_ns as _, size_cache as usize * size_of::<XmlNsPtr>())
                                as _;
                        if old_ns.is_null() {
                            xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                            xml_free(new_ns as _);
                            return -1;
                        }
                        new_ns =
                            xml_realloc(new_ns as _, size_cache as usize * size_of::<XmlNsPtr>())
                                as _;
                        if new_ns.is_null() {
                            xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                            xml_free(old_ns as _);
                            return -1;
                        }
                    }
                    *new_ns.add(nb_cache as usize) = n;
                    *old_ns.add(nb_cache as usize) = (*node).ns;
                    nb_cache += 1;
                    (*node).ns = n;
                }
            }
        }
        /*
         * now check for namespace held by attributes on the node.
         */
        if matches!((*node).typ, XmlElementType::XmlElementNode) {
            attr = (*node).properties;
            while !attr.is_null() {
                if !(*attr).ns.is_null() {
                    /*
                     * initialize the cache if needed
                     */
                    if size_cache == 0 {
                        size_cache = 10;
                        old_ns = xml_malloc(size_cache as usize * size_of::<XmlNsPtr>()) as _;
                        if old_ns.is_null() {
                            xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                            return -1;
                        }
                        new_ns = xml_malloc(size_cache as usize * size_of::<XmlNsPtr>()) as _;
                        if new_ns.is_null() {
                            xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                            xml_free(old_ns as _);
                            return -1;
                        }
                    }
                    let mut f = false;
                    for i in 0..nb_cache {
                        if *old_ns.add(i as usize) == (*attr).ns {
                            (*attr).ns = *new_ns.add(i as usize);
                            f = true;
                            break;
                        }
                    }
                    if !f {
                        /*
                         * OK we need to recreate a new namespace definition
                         */
                        n = xml_new_reconciled_ns(doc, tree, (*attr).ns);
                        if !n.is_null() {
                            /* :-( what if else ??? */
                            /*
                             * check if we need to grow the cache buffers.
                             */
                            if size_cache <= nb_cache {
                                size_cache *= 2;
                                old_ns = xml_realloc(
                                    old_ns as _,
                                    size_cache as usize * size_of::<XmlNsPtr>(),
                                ) as _;
                                if old_ns.is_null() {
                                    xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                                    xml_free(new_ns as _);
                                    return -1;
                                }
                                new_ns = xml_realloc(
                                    new_ns as _,
                                    size_cache as usize * size_of::<XmlNsPtr>(),
                                ) as _;
                                if new_ns.is_null() {
                                    xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                                    xml_free(old_ns as _);
                                    return -1;
                                }
                            }
                            *new_ns.add(nb_cache as usize) = n;
                            *old_ns.add(nb_cache as usize) = (*attr).ns;
                            nb_cache += 1;
                            (*attr).ns = n;
                        }
                    }
                }
                attr = (*attr).next;
            }
        }

        /*
         * Browse the full subtree, deep first
         */
        if !(*node).children.is_null() && !matches!((*node).typ, XmlElementType::XmlEntityRefNode) {
            /* deep first */
            node = (*node).children;
        } else if node != tree && !(*node).next.is_null() {
            /* then siblings */
            node = (*node).next;
        } else if node != tree {
            /* go up to parents->next if needed */
            while node != tree {
                if !(*node).parent.is_null() {
                    node = (*node).parent;
                }
                if node != tree && !(*node).next.is_null() {
                    node = (*node).next;
                    break;
                }
                if (*node).parent.is_null() {
                    node = null_mut();
                    break;
                }
            }
            /* exit condition */
            if node == tree {
                node = null_mut();
            }
        } else {
            break;
        }
    }
    if !old_ns.is_null() {
        xml_free(old_ns as _);
    }
    if !new_ns.is_null() {
        xml_free(new_ns as _);
    }
    ret
}

/*
 * XHTML
 */
const XHTML_STRICT_PUBLIC_ID: &CStr = c"-//W3C//DTD XHTML 1.0 Strict//EN";
const XHTML_STRICT_SYSTEM_ID: &CStr = c"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd";
const XHTML_FRAME_PUBLIC_ID: &CStr = c"-//W3C//DTD XHTML 1.0 Frameset//EN";
const XHTML_FRAME_SYSTEM_ID: &CStr = c"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd";
const XHTML_TRANS_PUBLIC_ID: &CStr = c"-//W3C//DTD XHTML 1.0 Transitional//EN";
const XHTML_TRANS_SYSTEM_ID: &CStr = c"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd";

/**
 * xmlIsXHTML:
 * @systemID:  the system identifier
 * @publicID:  the public identifier
 *
 * Try to find if the document correspond to an XHTML DTD
 *
 * Returns 1 if true, 0 if not and -1 in case of error
 */
pub unsafe extern "C" fn xml_is_xhtml(system_id: *const XmlChar, public_id: *const XmlChar) -> i32 {
    if system_id.is_null() && public_id.is_null() {
        return -1;
    }
    if !public_id.is_null() {
        if xml_str_equal(public_id, XHTML_STRICT_PUBLIC_ID.as_ptr() as _) {
            return 1;
        }
        if xml_str_equal(public_id, XHTML_FRAME_PUBLIC_ID.as_ptr() as _) {
            return 1;
        }
        if xml_str_equal(public_id, XHTML_TRANS_PUBLIC_ID.as_ptr() as _) {
            return 1;
        }
    }
    if !system_id.is_null() {
        if xml_str_equal(system_id, XHTML_STRICT_SYSTEM_ID.as_ptr() as _) {
            return 1;
        }
        if xml_str_equal(system_id, XHTML_FRAME_SYSTEM_ID.as_ptr() as _) {
            return 1;
        }
        if xml_str_equal(system_id, XHTML_TRANS_SYSTEM_ID.as_ptr() as _) {
            return 1;
        }
    }
    0
}

/*
 * Compression.
 */
/**
 * xmlGetDocCompressMode:
 * @doc:  the document
 *
 * get the compression ratio for a document, ZLIB based
 * Returns 0 (uncompressed) to 9 (max compression)
 */
pub unsafe extern "C" fn xml_get_doc_compress_mode(doc: *const XmlDoc) -> i32 {
    if doc.is_null() {
        return -1;
    }
    (*doc).compression
}

/**
 * xmlSetDocCompressMode:
 * @doc:  the document
 * @mode:  the compression ratio
 *
 * set the compression ratio for a document, ZLIB based
 * Correct values: 0 (uncompressed) to 9 (max compression)
 */
pub unsafe extern "C" fn xml_set_doc_compress_mode(doc: XmlDocPtr, mode: i32) {
    if doc.is_null() {
        return;
    }
    if mode < 0 {
        (*doc).compression = 0;
    } else if mode > 9 {
        (*doc).compression = 9;
    } else {
        (*doc).compression = mode;
    }
}

/**
 * xmlGetCompressMode:
 *
 * get the default compression mode used, ZLIB based.
 * Returns 0 (uncompressed) to 9 (max compression)
 */
static XML_COMPRESS_MODE: AtomicI32 = AtomicI32::new(0);

pub unsafe extern "C" fn xml_get_compress_mode() -> i32 {
    XML_COMPRESS_MODE.load(Ordering::Acquire)
}

/**
 * xmlSetCompressMode:
 * @mode:  the compression ratio
 *
 * set the default compression mode used, ZLIB based
 * Correct values: 0 (uncompressed) to 9 (max compression)
 */
pub unsafe extern "C" fn xml_set_compress_mode(mode: i32) {
    if mode < 0 {
        XML_COMPRESS_MODE.store(0, Ordering::Release);
    } else if mode > 9 {
        XML_COMPRESS_MODE.store(9, Ordering::Release);
    } else {
        XML_COMPRESS_MODE.store(mode, Ordering::Release);
    }
}

/*
* DOM-wrapper helper functions.
*/
/*
* xmlDOMWrapNewCtxt:
*
* Allocates and initializes a new DOM-wrapper context.
*
* Returns the xmlDOMWrapCtxtPtr or null_mut() in case of an internal error.
*/
pub unsafe extern "C" fn xml_dom_wrap_new_ctxt() -> XmlDOMWrapCtxtPtr {
    let ret: XmlDOMWrapCtxtPtr = xml_malloc(size_of::<XmlDOMWrapCtxt>()) as _;
    if ret.is_null() {
        xml_tree_err_memory(c"allocating DOM-wrapper context".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlDOMWrapCtxt>());
    ret
}

/*
* xmlDOMWrapNsMapFree:
* @map: the ns-map
*
* Frees the ns-map
*/
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

/*
* xmlDOMWrapFreeCtxt:
* @ctxt: the DOM-wrapper context
*
* Frees the DOM-wrapper context.
*/
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

/*
* XML_TREE_ADOPT_STR: If we have a dest-dict, put @str in the dict;
* otherwise copy it, when it was in the source-dict.
*/
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

/*
* XML_TREE_ADOPT_STR_2: If @str was in the source-dict, then
* put it in dest-dict or copy it.
*/
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

/*
* xmlDOMWrapNsMapAddItem:
* @map: the ns-map
* @oldNs: the old ns-struct
* @newNs: the new ns-struct
* @depth: depth and ns-kind information
*
* Adds an ns-mapping item.
*/
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
            xml_tree_err_memory(c"allocating namespace map".as_ptr() as _);
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
            xml_tree_err_memory(c"allocating namespace map item".as_ptr() as _);
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

/*
*
* xmlDOMWrapNSNormGatherInScopeNs:
* @map: the namespace map
* @node: the node to start with
*
* Puts in-scope namespaces into the ns-map.
*
* Returns 0 on success, -1 on API or internal errors.
*/
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
    if node.is_null() || matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
        return -1;
    }
    /*
     * Get in-scope ns-decls of @parent.
     */
    cur = node;
    while !cur.is_null() && cur != (*cur).doc as _ {
        if matches!((*cur).typ, XmlElementType::XmlElementNode) && !(*cur).ns_def.is_null() {
            ns = (*cur).ns_def;
            loop {
                shadowed = 0;
                if XML_NSMAP_NOTEMPTY!(*map) {
                    /*
                    	* Skip shadowed prefixes.
                    	*/
                    XML_NSMAP_FOREACH!(*map, mi, {
                        if (*ns).prefix.load(Ordering::Relaxed)
                            == (*(*mi).new_ns).prefix.load(Ordering::Relaxed)
                            || xml_str_equal(
                                (*ns).prefix.load(Ordering::Relaxed),
                                (*(*mi).new_ns).prefix.load(Ordering::Relaxed),
                            )
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
                ns = (*ns).next.load(Ordering::Relaxed);

                if ns.is_null() {
                    break;
                }
            }
        }
        cur = (*cur).parent;
    }
    0
}

/*
* xmlDOMWrapNSNormAddNsMapItem2:
*
* For internal use. Adds a ns-decl mapping.
*
* Returns 0 on success, -1 on internal errors.
*/
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
            xml_tree_err_memory(c"alloc ns map item".as_ptr() as _);
            return -1;
        }
        *size = 3;
        *number = 0;
    } else if *number >= *size {
        *size *= 2;
        *list = xml_realloc(*list as _, (*size) as usize * 2 * size_of::<XmlNsPtr>()) as _;
        if !(*list).is_null() {
            xml_tree_err_memory(c"realloc ns map item".as_ptr() as _);
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

/*
* xmlDOMWrapStoreNs:
* @doc: the doc
* @nsName: the namespace name
* @prefix: the prefix
*
* Creates or reuses an xmlNs struct on (*doc).oldNs with
* the given prefix and namespace name.
*
* Returns the acquired ns struct or null_mut() in case of an API
*         or internal error.
*/
unsafe extern "C" fn xml_dom_wrap_store_ns(
    doc: XmlDocPtr,
    ns_name: *const XmlChar,
    prefix: *const XmlChar,
) -> XmlNsPtr {
    let mut ns: XmlNsPtr;

    if doc.is_null() {
        return null_mut();
    }
    ns = xml_tree_ensure_xmldecl(doc);
    if ns.is_null() {
        return null_mut();
    }
    if !(*ns).next.load(Ordering::Relaxed).is_null() {
        /* Reuse. */
        ns = (*ns).next.load(Ordering::Relaxed);
        while !ns.is_null() {
            if (((*ns).prefix.load(Ordering::Relaxed) == prefix as _)
                || xml_str_equal((*ns).prefix.load(Ordering::Relaxed), prefix))
                && xml_str_equal((*ns).href.load(Ordering::Relaxed), ns_name)
            {
                return ns;
            }
            if (*ns).next.load(Ordering::Relaxed).is_null() {
                break;
            }
            ns = (*ns).next.load(Ordering::Relaxed);
        }
    }
    /* Create. */
    if !ns.is_null() {
        (*ns)
            .next
            .store(xml_new_ns(null_mut(), ns_name, prefix), Ordering::Relaxed);
        return (*ns).next.load(Ordering::Relaxed);
    }
    null_mut()
}

/*
* xmlTreeLookupNsListByPrefix:
* @nsList: a list of ns-structs
* @prefix: the searched prefix
*
* Searches for a ns-decl with the given prefix in @nsList.
*
* Returns the ns-decl if found, null_mut() if not found and on
*         API errors.
*/
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
            if prefix == (*ns).prefix.load(Ordering::Relaxed)
                || xml_str_equal(prefix, (*ns).prefix.load(Ordering::Relaxed))
            {
                return ns;
            }
            ns = (*ns).next.load(Ordering::Relaxed);

            if ns.is_null() {
                break;
            }
        }
    }
    null_mut()
}

/*
* xmlSearchNsByPrefixStrict:
* @doc: the document
* @node: the start node
* @prefix: the searched namespace prefix
* @retNs: the resulting ns-decl
*
* Dynamically searches for a ns-declaration which matches
* the given @nsName in the ancestor-or-self axis of @node.
*
* Returns 1 if a ns-decl was found, 0 if not and -1 on API
*         and internal errors.
*/
unsafe extern "C" fn xml_search_ns_by_prefix_strict(
    doc: XmlDocPtr,
    node: XmlNodePtr,
    prefix: *const XmlChar,
    ret_ns: *mut XmlNsPtr,
) -> i32 {
    let mut cur: XmlNodePtr;
    let mut ns: XmlNsPtr;

    if doc.is_null() || node.is_null() || matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
        return -1;
    }

    if !ret_ns.is_null() {
        *ret_ns = null_mut();
    }
    if IS_STR_XML!(prefix) {
        if !ret_ns.is_null() {
            *ret_ns = xml_tree_ensure_xmldecl(doc);
            if (*ret_ns).is_null() {
                return -1;
            }
        }
        return 1;
    }
    cur = node;
    loop {
        if matches!((*cur).typ, XmlElementType::XmlElementNode) {
            if !(*cur).ns_def.is_null() {
                ns = (*cur).ns_def;
                loop {
                    if prefix == (*ns).prefix.load(Ordering::Relaxed)
                        || xml_str_equal(prefix, (*ns).prefix.load(Ordering::Relaxed))
                    {
                        /*
                        	* Disabled namespaces, e.g. xmlns:abc="".
                        	*/
                        if (*ns).href.load(Ordering::Relaxed).is_null() {
                            return 0;
                        }
                        if !ret_ns.is_null() {
                            *ret_ns = ns;
                        }
                        return 1;
                    }
                    ns = (*ns).next.load(Ordering::Relaxed);

                    if ns.is_null() {
                        break;
                    }
                }
            }
        } else if (matches!((*cur).typ, XmlElementType::XmlEntityNode)
            || matches!((*cur).typ, XmlElementType::XmlEntityDecl))
        {
            return 0;
        }
        cur = (*cur).parent;

        if cur.is_null() || (*cur).doc == cur as _ {
            break;
        }
    }
    0
}

/*
* xmlDOMWrapNSNormDeclareNsForced:
* @doc: the doc
* @elem: the element-node to declare on
* @nsName: the namespace-name of the ns-decl
* @prefix: the preferred prefix of the ns-decl
* @checkShadow: ensure that the new ns-decl doesn't shadow ancestor ns-decls
*
* Declares a new namespace on @elem. It tries to use the
* given @prefix; if a ns-decl with the given prefix is already existent
* on @elem, it will generate an other prefix.
*
* Returns 1 if a ns-decl was found, 0 if not and -1 on API
*         and internal errors.
*/
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

    if doc.is_null() || elem.is_null() || !matches!((*elem).typ, XmlElementType::XmlElementNode) {
        return null_mut();
    }
    /*
     * Create a ns-decl on @anchor.
     */
    pref = prefix;
    loop {
        /*
         * Lookup whether the prefix is unused in elem's ns-decls.
         */
        if !(*elem).ns_def.is_null()
            && !xml_tree_nslist_lookup_by_prefix((*elem).ns_def, pref).is_null()
        {
            // goto ns_next_prefix;
        } else {
            /*
             * Does it shadow ancestor ns-decls?
             */
            if check_shadow != 0
                && !(*elem).parent.is_null()
                && (*(*elem).parent).doc != (*elem).parent as _
                && xml_search_ns_by_prefix_strict(doc, (*elem).parent, pref, null_mut()) == 1
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
                    while !(*ns2).next.load(Ordering::Relaxed).is_null() {
                        ns2 = (*ns2).next.load(Ordering::Relaxed);
                    }
                    (*ns2).next.store(ret, Ordering::Relaxed);
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

/*
* xmlDOMWrapNSNormAcquireNormalizedNs:
* @doc: the doc
* @elem: the element-node to declare namespaces on
* @ns: the ns-struct to use for the search
* @retNs: the found/created ns-struct
* @nsMap: the ns-map
* @depth: the current tree depth
* @ancestorsOnly: search in ancestor ns-decls only
* @prefixed: if the searched ns-decl must have a prefix (for attributes)
*
* Searches for a matching ns-name in the ns-decls of @nsMap, if not
* found it will either declare it on @elem, or store it in (*doc).oldNs.
* If a new ns-decl needs to be declared on @elem, it tries to use the
* @(*ns).prefix for it, if this prefix is already in use on @elem, it will
* change the prefix or the new ns-decl.
*
* Returns 0 if succeeded, -1 otherwise and on API/internal errors.
*/
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
    if IS_STR_XML!((*ns).prefix.load(Ordering::Relaxed)) {
        /*
        	* Insert XML namespace mapping.
        	*/
        *ret_ns = xml_tree_ensure_xmldecl(doc);
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
		        (!(*(*mi).new_ns).href.load(Ordering::Relaxed).is_null() &&
                *(*(*mi).new_ns).href.load(Ordering::Relaxed).add(0) != 0) &&
		        /* Ensure a prefix if wanted. */
		        (prefixed == 0 || !(*(*mi).new_ns).prefix.load(Ordering::Relaxed).is_null()) &&
		        /* Equal ns name */
		        ((*(*mi).new_ns).href.load(Ordering::Relaxed) == (*ns).href.load(Ordering::Relaxed) ||
                xml_str_equal((*(*mi).new_ns).href.load(Ordering::Relaxed), (*ns).href.load(Ordering::Relaxed)) )
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
        let tmpns: XmlNsPtr = xml_dom_wrap_store_ns(
            doc,
            (*ns).href.load(Ordering::Relaxed),
            (*ns).prefix.load(Ordering::Relaxed),
        );
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
        let tmpns: XmlNsPtr = xml_dom_wrap_nsnorm_declare_ns_forced(
            doc,
            elem,
            (*ns).href.load(Ordering::Relaxed),
            (*ns).prefix.load(Ordering::Relaxed),
            0,
        );
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
                    && ((*ns).prefix.load(Ordering::Relaxed)
                        == (*(*mi).new_ns).prefix.load(Ordering::Relaxed)
                        || xml_str_equal(
                            (*ns).prefix.load(Ordering::Relaxed),
                            (*(*mi).new_ns).prefix.load(Ordering::Relaxed),
                        ))
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

/*
* xmlDOMWrapReconcileNamespaces:
* @ctxt: DOM wrapper context, unused at the moment
* @elem: the element-node
* @options: option flags
*
* Ensures that ns-references point to ns-decls hold on element-nodes.
* Ensures that the tree is namespace wellformed by creating additional
* ns-decls where needed. Note that, since prefixes of already existent
* ns-decls can be shadowed by this process, it could break QNames in
* attribute values or element content.
*
* NOTE: This function was not intensively tested.
*
* Returns 0 if succeeded, -1 otherwise and on API/internal errors.
*/
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
        || !matches!((*elem).typ, XmlElementType::XmlElementNode)
    {
        return -1;
    }

    let ret;
    let doc: XmlDocPtr = (*elem).doc;
    cur = elem;
    'exit: {
        'internal_error: {
            'main: while {
                match (*cur).typ {
                    ty @ XmlElementType::XmlElementNode | ty @ XmlElementType::XmlAttributeNode => {
                        if matches!(ty, XmlElementType::XmlElementNode) {
                            adoptns = 1;
                            cur_elem = cur;
                            depth += 1;
                            /*
                             * Namespace declarations.
                             */
                            if !(*cur).ns_def.is_null() {
                                prevns = null_mut();
                                ns = (*cur).ns_def;
                                'b: while !ns.is_null() {
                                    if parnsdone == 0 {
                                        if !(*elem).parent.is_null()
                                            && (*(*elem).parent).doc != (*elem).parent as _
                                        {
                                            /*
                                             * Gather ancestor in-scope ns-decls.
                                             */
                                            if xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                                addr_of_mut!(ns_map),
                                                (*elem).parent,
                                            ) == -1
                                            {
                                                break 'internal_error;
                                            }
                                        }
                                        parnsdone = 1;
                                    }

                                    /*
                                     * Lookup the ns ancestor-axis for equal ns-decls in scope.
                                     */
                                    if opt_remove_redundant_ns != 0 && XML_NSMAP_NOTEMPTY!(ns_map) {
                                        XML_NSMAP_FOREACH!(ns_map, mi, {
                                            if (*mi).depth >= XML_TREE_NSMAP_PARENT
                                                && (*mi).shadow_depth == -1
                                                && ((*ns).prefix.load(Ordering::Relaxed)
                                                    == (*(*mi).new_ns)
                                                        .prefix
                                                        .load(Ordering::Relaxed)
                                                    || xml_str_equal(
                                                        (*ns).prefix.load(Ordering::Relaxed),
                                                        (*(*mi).new_ns)
                                                            .prefix
                                                            .load(Ordering::Relaxed),
                                                    ))
                                                && ((*ns).href.load(Ordering::Relaxed)
                                                    == (*(*mi).new_ns).href.load(Ordering::Relaxed)
                                                    || xml_str_equal(
                                                        (*ns).href.load(Ordering::Relaxed),
                                                        (*(*mi).new_ns)
                                                            .href
                                                            .load(Ordering::Relaxed),
                                                    ))
                                            {
                                                /*
                                                 * A redundant ns-decl was found.
                                                 * Add it to the list of redundant ns-decls.
                                                 */
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
                                                /*
                                                 * Remove the ns-decl from the element-node.
                                                 */
                                                if !prevns.is_null() {
                                                    (*prevns).next.store(
                                                        (*ns).next.load(Ordering::Relaxed),
                                                        Ordering::Relaxed,
                                                    );
                                                } else {
                                                    (*cur).ns_def =
                                                        (*ns).next.load(Ordering::Relaxed);
                                                }
                                                // goto next_ns_decl;
                                                ns = (*ns).next.load(Ordering::Relaxed);
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
                                                && ((*ns).prefix.load(Ordering::Relaxed)
                                                    == (*(*mi).new_ns)
                                                        .prefix
                                                        .load(Ordering::Relaxed)
                                                    || xml_str_equal(
                                                        (*ns).prefix.load(Ordering::Relaxed),
                                                        (*(*mi).new_ns)
                                                            .prefix
                                                            .load(Ordering::Relaxed),
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
                                    ns = (*ns).next.load(Ordering::Relaxed);
                                }
                            }
                            if adoptns == 0 {
                                // goto ns_end;
                                if matches!((*cur).typ, XmlElementType::XmlElementNode)
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
                            if matches!((*cur).typ, XmlElementType::XmlElementNode)
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
                            if !(*elem).parent.is_null()
                                && (*(*elem).parent).doc != (*elem).parent as _
                                && xml_dom_wrap_ns_norm_gather_in_scope_ns(
                                    addr_of_mut!(ns_map),
                                    (*elem).parent,
                                ) == -1
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
                                    if matches!((*cur).typ, XmlElementType::XmlElementNode)
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
                            matches!((*cur).typ, XmlElementType::XmlAttributeNode) as i32,
                        ) == -1
                        {
                            break 'internal_error;
                        }
                        (*cur).ns = ns;

                        // ns_end:
                        if matches!((*cur).typ, XmlElementType::XmlElementNode)
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
                            if matches!((*cur).typ, XmlElementType::XmlElementNode) {
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
                            if !(*cur).next.is_null() {
                                cur = (*cur).next;
                            } else {
                                if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
                                    cur = (*cur).parent;
                                    // goto into_content;
                                    break 'next_sibling;
                                }
                                cur = (*cur).parent;
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
                    if matches!((*cur).typ, XmlElementType::XmlElementNode)
                        && !(*cur).children.is_null()
                    {
                        /*
                         * Process content of element-nodes only.
                         */
                        cur = (*cur).children;
                        continue;
                    }
                    // next_sibling:
                    'next_sibling: loop {
                        if cur == elem {
                            break 'main;
                        }
                        if matches!((*cur).typ, XmlElementType::XmlElementNode) {
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
                        if !(*cur).next.is_null() {
                            cur = (*cur).next;
                        } else {
                            if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
                                cur = (*cur).parent;
                                // goto into_content;
                                continue 'into_content;
                            }
                            cur = (*cur).parent;
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

/*
* xmlDOMWrapAdoptBranch:
* @ctxt: the optional context for custom processing
* @sourceDoc: the optional sourceDoc
* @node: the element-node to start with
* @destDoc: the destination doc for adoption
* @destParent: the optional new parent of @node in @destDoc
* @options: option flags
*
* Ensures that ns-references point to @destDoc: either to
* elements->nsDef entries if @destParent is given, or to
* @(*destDoc).oldNs otherwise.
* If @destParent is given, it ensures that the tree is namespace
* wellformed by creating additional ns-decls where needed.
* Note that, since prefixes of already existent ns-decls can be
* shadowed by this process, it could break QNames in attribute
* values or element content.
*
* NOTE: This function was not intensively tested.
*
* Returns 0 if succeeded, -1 otherwise and on API/internal errors.
*/
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
            if !cur.is_null() && matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
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
                    if (*cur).next.is_null() {
                        // goto leave_node;
                        leave_node = true;
                    } else {
                        'lp: while {
                            cur = (*cur).next;
                            if matches!((*cur).typ, XmlElementType::XmlXincludeEnd)
                                || (*cur).doc == (*node).doc
                            {
                                break 'lp;
                            }

                            !(*cur).next.is_null()
                        } {}

                        if (*cur).doc != (*node).doc {
                            // goto leave_node;
                            leave_node = true;
                        }
                    }
                }

                if !leave_node {
                    (*cur).doc = dest_doc;
                    match (*cur).typ {
                        XmlElementType::XmlXincludeStart | XmlElementType::XmlXincludeEnd => {
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
                                                    && ((*ns).prefix.load(Ordering::Relaxed)
                                                        == (*(*mi).new_ns)
                                                            .prefix
                                                            .load(Ordering::Relaxed)
                                                        || xml_str_equal(
                                                            (*ns).prefix.load(Ordering::Relaxed),
                                                            (*(*mi).new_ns)
                                                                .prefix
                                                                .load(Ordering::Relaxed),
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
                                        ns = (*ns).next.load(Ordering::Relaxed);
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
                                            (*(*cur).ns).href.load(Ordering::Relaxed),
                                            (*(*cur).ns).prefix.load(Ordering::Relaxed),
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
                                            matches!((*cur).typ, XmlElementType::XmlAttributeNode)
                                                as i32,
                                        ) == -1
                                        {
                                            break 'internal_error;
                                        }
                                        (*cur).ns = ns;
                                    }
                                }
                            }
                            // ns_end:
                            /*
                             * Further node properties.
                             * TODO: Is this all?
                             */
                            XML_TREE_ADOPT_STR!((*cur).name, adopt_str, source_doc, dest_doc);
                            if matches!((*cur).typ, XmlElementType::XmlElementNode) {
                                (*cur).psvi = null_mut();
                                (*cur).line = 0;
                                (*cur).extra = 0;
                                /*
                                 * Walk attributes.
                                 */
                                if !(*cur).properties.is_null() {
                                    /*
                                     * Process first attribute node.
                                     */
                                    cur = (*cur).properties as _;
                                    continue;
                                }
                            } else {
                                /*
                                 * Attributes.
                                 */
                                if !source_doc.is_null()
                                    && matches!(
                                        (*(cur as XmlAttrPtr)).atype,
                                        Some(XmlAttributeType::XmlAttributeId)
                                    )
                                {
                                    xml_remove_id(source_doc, cur as _);
                                }
                                (*(cur as XmlAttrPtr)).atype = None;
                                (*(cur as XmlAttrPtr)).psvi = null_mut();
                            }
                        }
                        XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode => {
                            /*
                             * This puts the content in the dest dict, only if
                             * it was previously in the source dict.
                             */
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
                            /*
                             * Remove reference to the entity-node.
                             */
                            (*cur).content = null_mut();
                            (*cur).children = null_mut();
                            (*cur).last = null_mut();
                            if !(*dest_doc).int_subset.is_null()
                                || !(*dest_doc).ext_subset.is_null()
                            {
                                /*
                                 * Assign new entity-node if available.
                                 */
                                let ent: XmlEntityPtr = xml_get_doc_entity(dest_doc, (*cur).name);
                                if !ent.is_null() {
                                    (*cur).content = (*ent).content.load(Ordering::Relaxed);
                                    (*cur).children = ent as _;
                                    (*cur).last = ent as _;
                                }
                            }
                            // goto leave_node;
                            leave_node = true;
                        }
                        XmlElementType::XmlPiNode => {
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
                        /*
                         * Walk the tree.
                         */
                        if !(*cur).children.is_null() {
                            cur = (*cur).children;
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
                        (*cur).typ,
                        XmlElementType::XmlElementNode
                            | XmlElementType::XmlXincludeStart
                            | XmlElementType::XmlXincludeEnd
                    ) {
                        /*
                         * TODO: Do we expect nsDefs on xmlElementType::XML_XINCLUDE_START?
                         */
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            /*
                             * Pop mappings.
                             */
                            while !(*ns_map).last.is_null() && (*(*ns_map).last).depth >= depth {
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
                    if !(*cur).next.is_null() {
                        cur = (*cur).next;
                    } else if matches!((*cur).typ, XmlElementType::XmlAttributeNode)
                        && !(*(*cur).parent).children.is_null()
                    {
                        cur = (*(*cur).parent).children;
                    } else {
                        cur = (*cur).parent;
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
    ret
}

/*
* xmlSearchNsByNamespaceStrict:
* @doc: the document
* @node: the start node
* @nsName: the searched namespace name
* @retNs: the resulting ns-decl
* @prefixed: if the found ns-decl must have a prefix (for attributes)
*
* Dynamically searches for a ns-declaration which matches
* the given @nsName in the ancestor-or-self axis of @node.
*
* Returns 1 if a ns-decl was found, 0 if not and -1 on API
*         and internal errors.
*/
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
    if node.is_null() || matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
        return -1;
    }

    *ret_ns = null_mut();
    if xml_str_equal(ns_name, XML_XML_NAMESPACE.as_ptr() as _) {
        *ret_ns = xml_tree_ensure_xmldecl(doc);
        if (*ret_ns).is_null() {
            return -1;
        }
        return 1;
    }
    cur = node;
    loop {
        if matches!((*cur).typ, XmlElementType::XmlElementNode) {
            if !(*cur).ns_def.is_null() {
                ns = (*cur).ns_def;
                while !ns.is_null() {
                    if prefixed != 0 && (*ns).prefix.load(Ordering::Relaxed).is_null() {
                        ns = (*ns).next.load(Ordering::Relaxed);
                        continue;
                    }
                    if !prev.is_null() {
                        /*
                         * Check the last level of ns-decls for a
                         * shadowing prefix.
                         */
                        prevns = (*prev).ns_def;
                        loop {
                            if (*prevns).prefix.load(Ordering::Relaxed)
                                == (*ns).prefix.load(Ordering::Relaxed)
                                || (!(*prevns).prefix.load(Ordering::Relaxed).is_null()
                                    && !(*ns).prefix.load(Ordering::Relaxed).is_null()
                                    && xml_str_equal(
                                        (*prevns).prefix.load(Ordering::Relaxed),
                                        (*ns).prefix.load(Ordering::Relaxed),
                                    ))
                            {
                                /*
                                 * Shadowed.
                                 */
                                break;
                            }
                            prevns = (*prevns).next.load(Ordering::Relaxed);

                            if prevns.is_null() {
                                break;
                            }
                        }
                        if !prevns.is_null() {
                            ns = (*ns).next.load(Ordering::Relaxed);
                            continue;
                        }
                    }
                    /*
                     * Ns-name comparison.
                     */
                    if ns_name == (*ns).href.load(Ordering::Relaxed)
                        || xml_str_equal(ns_name, (*ns).href.load(Ordering::Relaxed))
                    {
                        /*
                         * At this point the prefix can only be shadowed,
                         * if we are the the (at least) 3rd level of
                         * ns-decls.
                         */
                        if !out.is_null() {
                            let ret: i32 = xml_ns_in_scope(
                                doc,
                                node,
                                prev,
                                (*ns).prefix.load(Ordering::Relaxed),
                            );
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
                                ns = (*ns).next.load(Ordering::Relaxed);
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
        } else if (matches!((*cur).typ, XmlElementType::XmlEntityNode)
            || matches!((*cur).typ, XmlElementType::XmlEntityDecl))
        {
            return 0;
        }
        cur = (*cur).parent;

        if cur.is_null() || (*cur).doc == cur as _ {
            break;
        }
    }
    0
}

/*
* xmlDOMWrapAdoptAttr:
* @ctxt: the optional context for custom processing
* @sourceDoc: the optional source document of attr
* @attr: the attribute-node to be adopted
* @destDoc: the destination doc for adoption
* @destParent: the optional new parent of @attr in @destDoc
* @options: option flags
*
* @attr is adopted by @destDoc.
* Ensures that ns-references point to @destDoc: either to
* elements->nsDef entries if @destParent is given, or to
* @(*destDoc).oldNs otherwise.
*
* Returns 0 if succeeded, -1 otherwise and on API/internal errors.
*/
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
        if IS_STR_XML!((*(*attr).ns).prefix.load(Ordering::Relaxed)) {
            ns = xml_tree_ensure_xmldecl(dest_doc);
        } else if dest_parent.is_null() {
            /*
             * Store in @(*destDoc).oldNs.
             */
            ns = xml_dom_wrap_store_ns(
                dest_doc,
                (*(*attr).ns).href.load(Ordering::Relaxed),
                (*(*attr).ns).prefix.load(Ordering::Relaxed),
            );
        } else {
            /*
             * Declare on @destParent.
             */
            if xml_search_ns_by_namespace_strict(
                dest_doc,
                dest_parent,
                (*(*attr).ns).href.load(Ordering::Relaxed),
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
                    (*(*attr).ns).href.load(Ordering::Relaxed),
                    (*(*attr).ns).prefix.load(Ordering::Relaxed),
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
    if (*attr).children.is_null() {
        return 0;
    }
    cur = (*attr).children;
    if !cur.is_null() && matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        // goto internal_error;
        return -1;
    }
    while !cur.is_null() {
        (*cur).doc = dest_doc;
        match (*cur).typ {
            XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode => {
                XML_TREE_ADOPT_STR_2!((*cur).content, adopt_str, source_doc, dest_doc, cur);
            }
            XmlElementType::XmlEntityRefNode => {
                /*
                 * Remove reference to the entity-node.
                 */
                (*cur).content = null_mut();
                (*cur).children = null_mut();
                (*cur).last = null_mut();
                if !(*dest_doc).int_subset.is_null() || !(*dest_doc).ext_subset.is_null() {
                    /*
                     * Assign new entity-node if available.
                     */
                    let ent: XmlEntityPtr = xml_get_doc_entity(dest_doc, (*cur).name);
                    if !ent.is_null() {
                        (*cur).content = (*ent).content.load(Ordering::Relaxed);
                        (*cur).children = ent as _;
                        (*cur).last = ent as _;
                    }
                }
            }
            _ => {}
        }
        if !(*cur).children.is_null() {
            cur = (*cur).children;
            continue;
        }
        // next_sibling:
        'next_sibling: loop {
            if cur == attr as _ {
                break;
            }
            if !(*cur).next.is_null() {
                cur = (*cur).next;
            } else {
                cur = (*cur).parent;
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

/*
* xmlDOMWrapAdoptNode:
* @ctxt: the optional context for custom processing
* @sourceDoc: the optional sourceDoc
* @node: the node to start with
* @destDoc: the destination doc
* @destParent: the optional new parent of @node in @destDoc
* @options: option flags
*
* References of out-of scope ns-decls are remapped to point to @destDoc:
* 1) If @destParent is given, then nsDef entries on element-nodes are used
* 2) If *no* @destParent is given, then @(*destDoc).oldNs entries are used
*    This is the case when you have an unlinked node and just want to move it
*    to the context of
*
* If @destParent is given, it ensures that the tree is namespace
* wellformed by creating additional ns-decls where needed.
* Note that, since prefixes of already existent ns-decls can be
* shadowed by this process, it could break QNames in attribute
* values or element content.
* NOTE: This function was not intensively tested.
*
* Returns 0 if the operation succeeded,
*         1 if a node of unsupported type was given,
*         2 if a node of not yet supported type was given and
*         -1 on API/internal errors.
*/
pub unsafe extern "C" fn xml_dom_wrap_adopt_node(
    ctxt: XmlDOMWrapCtxtPtr,
    mut source_doc: XmlDocPtr,
    node: XmlNodePtr,
    dest_doc: XmlDocPtr,
    dest_parent: XmlNodePtr,
    options: i32,
) -> i32 {
    if node.is_null()
        || matches!((*node).typ, XmlElementType::XmlNamespaceDecl)
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
    match (*node).typ {
        XmlElementType::XmlElementNode
        | XmlElementType::XmlAttributeNode
        | XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode => {}
        XmlElementType::XmlDocumentFragNode => {
            /* TODO: Support document-fragment-nodes. */
            return 2;
        }
        _ => {
            return 1;
        }
    }
    /*
     * Unlink only if @node was not already added to @destParent.
     */
    if !(*node).parent.is_null() && dest_parent != (*node).parent {
        (*node).unlink();
    }

    if matches!((*node).typ, XmlElementType::XmlElementNode) {
        return xml_dom_wrap_adopt_branch(ctxt, source_doc, node, dest_doc, dest_parent, options);
    } else if matches!((*node).typ, XmlElementType::XmlAttributeNode) {
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
        /*
        	* Optimize string adoption.
        	*/
        if !source_doc.is_null() && (*source_doc).dict == (*dest_doc).dict {
            adopt_str = 0;
        }
        match (*node).typ {
            XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode => {
                XML_TREE_ADOPT_STR_2!((*node).content, adopt_str, source_doc, dest_doc, cur);
            }
            XmlElementType::XmlEntityRefNode => {
                /*
                 * Remove reference to the entity-node.
                 */
                (*node).content = null_mut();
                (*node).children = null_mut();
                (*node).last = null_mut();
                if !(*dest_doc).int_subset.is_null() || !(*dest_doc).ext_subset.is_null() {
                    /*
                     * Assign new entity-node if available.
                     */
                    let ent: XmlEntityPtr = xml_get_doc_entity(dest_doc, (*node).name);
                    if !ent.is_null() {
                        (*node).content = (*ent).content.load(Ordering::Relaxed);
                        (*node).children = ent as _;
                        (*node).last = ent as _;
                    }
                }
                XML_TREE_ADOPT_STR!((*node).name, adopt_str, source_doc, dest_doc);
            }
            XmlElementType::XmlPiNode => {
                XML_TREE_ADOPT_STR!((*node).name, adopt_str, source_doc, dest_doc);
                XML_TREE_ADOPT_STR_2!((*node).content, adopt_str, source_doc, dest_doc, cur);
            }
            _ => {}
        }
    }
    0
}

/*
* xmlDOMWrapRemoveNode:
* @ctxt: a DOM wrapper context
* @doc: the doc
* @node: the node to be removed.
* @options: set of options, unused at the moment
*
* Unlinks the given node from its owner.
* This will substitute ns-references to (*node).nsDef for
* ns-references to (*doc).oldNs, thus ensuring the removed
* branch to be autark wrt ns-references.
*
* NOTE: This function was not intensively tested.
*
* Returns 0 on success, 1 if the node is not supported,
*         -1 on API and internal errors.
*/
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
    if (*node).parent.is_null() {
        return 0;
    }

    match (*node).typ {
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlPiNode
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
        match (*node).typ {
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
                        ns = (*ns).next.load(Ordering::Relaxed);
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
                                if (matches!((*node).typ, XmlElementType::XmlElementNode)
                                    && !(*node).children.is_null())
                                {
                                    node = (*node).children;
                                    continue 'main;
                                }
                                // next_sibling:
                                'next_sibling: loop {
                                    if node.is_null() {
                                        break;
                                    }
                                    if !(*node).next.is_null() {
                                        node = (*node).next;
                                    } else {
                                        node = (*node).parent;
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
                        ns = xml_dom_wrap_store_ns(
                            doc,
                            (*(*node).ns).href.load(Ordering::Relaxed),
                            (*(*node).ns).prefix.load(Ordering::Relaxed),
                        );
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
                if matches!((*node).typ, XmlElementType::XmlElementNode)
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
                    if !(*node).next.is_null() {
                        node = (*node).next;
                    } else {
                        node = (*node).parent;
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
        if matches!((*node).typ, XmlElementType::XmlElementNode) && !(*node).children.is_null() {
            node = (*node).children;
            continue;
        }
        // next_sibling:
        'next_sibling: loop {
            if node.is_null() {
                break;
            }
            if !(*node).next.is_null() {
                node = (*node).next;
            } else {
                node = (*node).parent;
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

/**
 * DICT_COPY:
 * @str:  a string
 *
 * Copy a string using a "dict" dictionary in the current scope,
 * if available.
 */
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

/**
 * DICT_CONST_COPY:
 * @str:  a string
 *
 * Copy a string using a "dict" dictionary in the current scope,
 * if available.
 */
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

/*
* xmlDOMWrapCloneNode:
* @ctxt: the optional context for custom processing
* @sourceDoc: the optional sourceDoc
* @node: the node to start with
* @resNode: the clone of the given @node
* @destDoc: the destination doc
* @destParent: the optional new parent of @node in @destDoc
* @deep: descend into child if set
* @options: option flags
*
* References of out-of scope ns-decls are remapped to point to @destDoc:
* 1) If @destParent is given, then nsDef entries on element-nodes are used
* 2) If *no* @destParent is given, then @(*destDoc).oldNs entries are used.
*    This is the case when you don't know already where the cloned branch
*    will be added to.
*
* If @destParent is given, it ensures that the tree is namespace
* wellformed by creating additional ns-decls where needed.
* Note that, since prefixes of already existent ns-decls can be
* shadowed by this process, it could break QNames in attribute
* values or element content.
* TODO:
*   1) What to do with XInclude? Currently this returns an error for XInclude.
*
* Returns 0 if the operation succeeded,
*         1 if a node of unsupported (or not yet supported) type was given,
*         -1 on API/internal errors.
*/
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
    if !matches!((*node).typ, XmlElementType::XmlElementNode) {
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
    if !cur.is_null() && matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
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
                match (*cur).typ {
                    XmlElementType::XmlXincludeStart | XmlElementType::XmlXincludeEnd => {
                        /*
                         * TODO: What to do with XInclude?
                         */
                        break 'internal_error;
                    }
                    XmlElementType::XmlElementNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlCdataSectionNode
                    | XmlElementType::XmlCommentNode
                    | XmlElementType::XmlPiNode
                    | XmlElementType::XmlDocumentFragNode
                    | XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode => {
                        /*
                         * Nodes of xmlNode structure.
                         */
                        clone = xml_malloc(size_of::<XmlNode>()) as _;
                        if clone.is_null() {
                            xml_tree_err_memory(
                                c"xmlDOMWrapCloneNode(): allocating a node".as_ptr() as _,
                            );
                            break 'internal_error;
                        }
                        memset(clone as _, 0, size_of::<XmlNode>());
                        /*
                         * Set hierarchical links.
                         */
                        if !result_clone.is_null() {
                            (*clone).parent = parent_clone;
                            if !prev_clone.is_null() {
                                (*prev_clone).next = clone;
                                (*clone).prev = prev_clone;
                            } else {
                                (*parent_clone).children = clone;
                            }
                        } else {
                            result_clone = clone;
                        }
                    }
                    XmlElementType::XmlAttributeNode => {
                        /*
                         * Attributes (xmlAttr).
                         */
                        /* Use xmlRealloc to avoid -Warray-bounds warning */
                        clone = xml_realloc(null_mut(), size_of::<XmlAttr>()) as _;
                        if clone.is_null() {
                            xml_tree_err_memory(
                                c"xmlDOMWrapCloneNode(): allocating an attr-node".as_ptr() as _,
                            );
                            break 'internal_error;
                        }
                        memset(clone as _, 0, size_of::<XmlAttr>());
                        /*
                         * Set hierarchical links.
                         * TODO: Change this to add to the end of attributes.
                         */
                        if !result_clone.is_null() {
                            (*clone).parent = parent_clone;
                            if !prev_clone.is_null() {
                                (*prev_clone).next = clone;
                                (*clone).prev = prev_clone;
                            } else {
                                (*parent_clone).properties = clone as _;
                            }
                        } else {
                            result_clone = clone;
                        }
                    }
                    _ => {
                        /*
                         * TODO QUESTION: Any other nodes expected?
                         */
                        break 'internal_error;
                    }
                }

                (*clone).typ = (*cur).typ;
                (*clone).doc = dest_doc;

                /*
                 * Clone the name of the node if any.
                 */
                if (*cur).name == XML_STRING_TEXT.as_ptr() as _ {
                    (*clone).name = XML_STRING_TEXT.as_ptr() as _;
                } else if (*cur).name == XML_STRING_TEXT_NOENC.as_ptr() as _ {
                    /*
                     * NOTE: Although xmlStringTextNoenc is never assigned to a node
                     *   in tree.c, it might be set in Libxslt via
                     *   "xsl:disable-output-escaping".
                     */
                    (*clone).name = XML_STRING_TEXT_NOENC.as_ptr() as _;
                } else if (*cur).name == XML_STRING_COMMENT.as_ptr() as _ {
                    (*clone).name = XML_STRING_COMMENT.as_ptr() as _;
                } else if !(*cur).name.is_null() {
                    DICT_CONST_COPY!(dict, (*cur).name, (*clone).name);
                }

                let mut leave_node = false;
                match (*cur).typ {
                    XmlElementType::XmlXincludeStart | XmlElementType::XmlXincludeEnd => {
                        /*
                         * TODO
                         */
                        return -1;
                    }
                    XmlElementType::XmlElementNode => {
                        cur_elem = cur;
                        depth += 1;
                        /*
                         * Namespace declarations.
                         */
                        if !(*cur).ns_def.is_null() {
                            if parnsdone == 0 {
                                if !dest_parent.is_null() && ctxt.is_null() {
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
                                }
                                parnsdone = 1;
                            }
                            /*
                             * Clone namespace declarations.
                             */
                            clone_ns_def_slot = addr_of_mut!((*clone).ns_def);
                            ns = (*cur).ns_def;
                            while !ns.is_null() {
                                /*
                                 * Create a new xmlNs.
                                 */
                                clone_ns = xml_malloc(size_of::<XmlNs>()) as _;
                                if clone_ns.is_null() {
                                    xml_tree_err_memory(
                                        c"xmlDOMWrapCloneNode(): allocating namespace".as_ptr()
                                            as _,
                                    );
                                    return -1;
                                }
                                memset(clone_ns as _, 0, size_of::<XmlNs>());
                                (*clone_ns).typ = Some(XML_LOCAL_NAMESPACE);

                                if !(*ns).href.load(Ordering::Relaxed).is_null() {
                                    (*clone_ns).href.store(
                                        xml_strdup((*ns).href.load(Ordering::Relaxed)),
                                        Ordering::Relaxed,
                                    );
                                }
                                if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
                                    (*clone_ns).prefix.store(
                                        xml_strdup((*ns).prefix.load(Ordering::Relaxed)),
                                        Ordering::Relaxed,
                                    );
                                }

                                *clone_ns_def_slot = clone_ns;
                                let mut p = (*clone_ns).next.load(Ordering::Relaxed);
                                clone_ns_def_slot = addr_of_mut!(p);

                                /*
                                 * Note that for custom handling of ns-references,
                                 * the ns-decls need not be stored in the ns-map,
                                 * since they won't be referenced by (*node).ns.
                                 */
                                if ctxt.is_null() || (*ctxt).get_ns_for_node_func.is_none() {
                                    /*
                                     * Does it shadow any ns-decl?
                                     */
                                    if XML_NSMAP_NOTEMPTY!(ns_map) {
                                        XML_NSMAP_FOREACH!(ns_map, mi, {
                                            if ((*mi).depth >= XML_TREE_NSMAP_PARENT)
                                                && (*mi).shadow_depth == -1
                                                && ((*ns).prefix.load(Ordering::Relaxed)
                                                    == (*(*mi).new_ns)
                                                        .prefix
                                                        .load(Ordering::Relaxed)
                                                    || xml_str_equal(
                                                        (*ns).prefix.load(Ordering::Relaxed),
                                                        (*(*mi).new_ns)
                                                            .prefix
                                                            .load(Ordering::Relaxed),
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
                                    /*
                                     * Push mapping.
                                     */
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
                                ns = (*ns).next.load(Ordering::Relaxed);
                            }
                        }
                        /* (*cur).ns will be processed further down. */
                    }
                    XmlElementType::XmlAttributeNode => {
                        /* IDs will be processed further down. */
                        /* (*cur).ns will be processed further down. */
                    }
                    XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode => {
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
                                /*
                                 * Different doc: Assign new entity-node if available.
                                 */
                                let ent: XmlEntityPtr = xml_get_doc_entity(dest_doc, (*cur).name);
                                if !ent.is_null() {
                                    (*clone).content = (*ent).content.load(Ordering::Relaxed);
                                    (*clone).children = ent as _;
                                    (*clone).last = ent as _;
                                }
                            }
                        } else {
                            /*
                             * Same doc: Use the current node's entity declaration
                             * and value.
                             */
                            (*clone).content = (*cur).content;
                            (*clone).children = (*cur).children;
                            (*clone).last = (*cur).last;
                        }
                        // goto leave_node;
                        leave_node = true;
                    }
                    XmlElementType::XmlPiNode => {
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
                                    (*(*cur).ns).href.load(Ordering::Relaxed),
                                    (*(*cur).ns).prefix.load(Ordering::Relaxed),
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
                                    matches!((*cur).typ, XmlElementType::XmlAttributeNode) as i32,
                                ) == -1
                                {
                                    break 'internal_error;
                                }
                                (*clone).ns = ns;
                            }
                        }
                    }

                    // end_ns_reference:

                    /*
                     * Some post-processing.
                     *
                     * Handle ID attributes.
                     */
                    if (matches!((*clone).typ, XmlElementType::XmlAttributeNode)
                        && !(*clone).parent.is_null())
                        && xml_is_id(dest_doc, (*clone).parent, clone as _) != 0
                    {
                        let id_val: *mut XmlChar =
                            xml_node_list_get_string((*cur).doc, (*cur).children, 1);
                        if !id_val.is_null() {
                            if xml_add_id(null_mut(), dest_doc, id_val, cur as _).is_null() {
                                /* TODO: error message. */
                                xml_free(id_val as _);
                                break 'internal_error;
                            }
                            xml_free(id_val as _);
                        }
                    }
                    /*
                     **
                     ** The following will traverse the tree **************************
                     **
                     *
                     * Walk the element's attributes before descending into child-nodes.
                     */
                    if matches!((*cur).typ, XmlElementType::XmlElementNode)
                        && !(*cur).properties.is_null()
                    {
                        prev_clone = null_mut();
                        parent_clone = clone;
                        cur = (*cur).properties as _;
                        continue 'main;
                    }
                    // into_content:
                    /*
                     * Descend into child-nodes.
                     */
                    if !(*cur).children.is_null()
                        && (deep != 0 || matches!((*cur).typ, XmlElementType::XmlAttributeNode))
                    {
                        prev_clone = null_mut();
                        parent_clone = clone;
                        cur = (*cur).children;
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
                    if matches!((*cur).typ, XmlElementType::XmlElementNode)
                        || matches!((*cur).typ, XmlElementType::XmlXincludeStart)
                        || matches!((*cur).typ, XmlElementType::XmlXincludeEnd)
                    {
                        /*
                         * TODO: Do we expect nsDefs on xmlElementType::XML_XINCLUDE_START?
                         */
                        if XML_NSMAP_NOTEMPTY!(ns_map) {
                            /*
                             * Pop mappings.
                             */
                            while !(*ns_map).last.is_null() && (*(*ns_map).last).depth >= depth {
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
                    if !(*cur).next.is_null() {
                        prev_clone = clone;
                        cur = (*cur).next;
                    } else if !matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
                        /*
                         * Set (*clone).last.
                         */
                        if !(*clone).parent.is_null() {
                            (*(*clone).parent).last = clone;
                        }
                        clone = (*clone).parent;
                        if !clone.is_null() {
                            parent_clone = (*clone).parent;
                        }
                        /*
                         * Process parent --> next;
                         */
                        cur = (*cur).parent;
                        // goto leave_node;
                        continue 'leave_node;
                    } else {
                        /* This is for attributes only. */
                        clone = (*clone).parent;
                        parent_clone = (*clone).parent;
                        /*
                         * Process parent-element --> children.
                         */
                        cur = (*cur).parent;
                        // goto into_content;
                        /*
                         * Descend into child-nodes.
                         */
                        if !(*cur).children.is_null()
                            && (deep != 0 || matches!((*cur).typ, XmlElementType::XmlAttributeNode))
                        {
                            prev_clone = null_mut();
                            parent_clone = clone;
                            cur = (*cur).children;
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

/*
 * 5 interfaces from DOM ElementTraversal, but different in entities
 * traversal.
 */
/**
 * xmlChildElementCount:
 * @parent: the parent node
 *
 * Finds the current number of child nodes of that element which are
 * element nodes.
 * Note the handling of entities references is different than in
 * the W3C DOM element traversal spec since we don't have back reference
 * from entities content to entities references.
 *
 * Returns the count of element child or 0 if not available
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_child_element_count(parent: XmlNodePtr) -> u64 {
    let mut ret: u64 = 0;
    let mut cur: XmlNodePtr;

    if parent.is_null() {
        return 0;
    }
    match (*parent).typ {
        XmlElementType::XmlElementNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlDocumentNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlHtmlDocumentNode => {
            cur = (*parent).children;
        }
        _ => {
            return 0;
        }
    }
    while !cur.is_null() {
        if matches!((*cur).typ, XmlElementType::XmlElementNode) {
            ret += 1;
        }
        cur = (*cur).next;
    }
    ret
}

/**
 * xmlNextElementSibling:
 * @node: the current node
 *
 * Finds the first closest next sibling of the node which is an
 * element node.
 * Note the handling of entities references is different than in
 * the W3C DOM element traversal spec since we don't have back reference
 * from entities content to entities references.
 *
 * Returns the next element sibling or null_mut() if not available
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_next_element_sibling(mut node: XmlNodePtr) -> XmlNodePtr {
    if node.is_null() {
        return null_mut();
    }
    match (*node).typ {
        XmlElementType::XmlElementNode
        | XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => {
            node = (*node).next;
        }
        _ => {
            return null_mut();
        }
    }
    while !node.is_null() {
        if matches!((*node).typ, XmlElementType::XmlElementNode) {
            return node;
        }
        node = (*node).next;
    }
    null_mut()
}

/**
 * xmlFirstElementChild:
 * @parent: the parent node
 *
 * Finds the first child node of that element which is a Element node
 * Note the handling of entities references is different than in
 * the W3C DOM element traversal spec since we don't have back reference
 * from entities content to entities references.
 *
 * Returns the first element child or null_mut() if not available
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_first_element_child(parent: XmlNodePtr) -> XmlNodePtr {
    let mut cur: XmlNodePtr;

    if parent.is_null() {
        return null_mut();
    }
    match (*parent).typ {
        XmlElementType::XmlElementNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlDocumentNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlHtmlDocumentNode => {
            cur = (*parent).children;
        }
        _ => {
            return null_mut();
        }
    }
    while !cur.is_null() {
        if matches!((*cur).typ, XmlElementType::XmlElementNode) {
            return cur;
        }
        cur = (*cur).next;
    }
    null_mut()
}

/**
 * xmlLastElementChild:
 * @parent: the parent node
 *
 * Finds the last child node of that element which is a Element node
 * Note the handling of entities references is different than in
 * the W3C DOM element traversal spec since we don't have back reference
 * from entities content to entities references.
 *
 * Returns the last element child or null_mut() if not available
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_last_element_child(parent: XmlNodePtr) -> XmlNodePtr {
    let mut cur: XmlNodePtr;

    if parent.is_null() {
        return null_mut();
    }
    match (*parent).typ {
        XmlElementType::XmlElementNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlDocumentNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlHtmlDocumentNode => {
            cur = (*parent).last;
        }
        _ => {
            return null_mut();
        }
    }
    while !cur.is_null() {
        if matches!((*cur).typ, XmlElementType::XmlElementNode) {
            return cur;
        }
        cur = (*cur).prev;
    }
    null_mut()
}

/**
 * xmlPreviousElementSibling:
 * @node: the current node
 *
 * Finds the first closest previous sibling of the node which is an
 * element node.
 * Note the handling of entities references is different than in
 * the W3C DOM element traversal spec since we don't have back reference
 * from entities content to entities references.
 *
 * Returns the previous element sibling or null_mut() if not available
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_previous_element_sibling(mut node: XmlNodePtr) -> XmlNodePtr {
    if node.is_null() {
        return null_mut();
    }
    match (*node).typ {
        XmlElementType::XmlElementNode
        | XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => {
            node = (*node).prev;
        }
        _ => {
            return null_mut();
        }
    }
    while !node.is_null() {
        if matches!((*node).typ, XmlElementType::XmlElementNode) {
            return node;
        }
        node = (*node).prev;
    }
    null_mut()
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

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
    fn test_xml_child_element_count() {
        #[cfg(feature = "tree")]
        unsafe {
            let mut leaks = 0;

            for n_parent in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let parent = gen_xml_node_ptr(n_parent, 0);

                let ret_val = xml_child_element_count(parent);
                desret_unsigned_long(ret_val);
                des_xml_node_ptr(n_parent, parent, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlChildElementCount",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlChildElementCount()"
                    );
                    eprintln!(" {}", n_parent);
                }
            }
        }
    }

    #[test]
    fn test_xml_copy_doc() {
        #[cfg(any(feature = "tree", feature = "schema"))]
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
        #[cfg(feature = "tree")]
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
    fn test_xml_create_int_subset() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_external_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_system_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let doc = gen_xml_doc_ptr(n_doc, 0);
                            let name = gen_const_xml_char_ptr(n_name, 1);
                            let external_id = gen_const_xml_char_ptr(n_external_id, 2);
                            let system_id = gen_const_xml_char_ptr(n_system_id, 3);

                            let ret_val = xml_create_int_subset(doc, name, external_id, system_id);
                            desret_xml_dtd_ptr(ret_val);
                            des_xml_doc_ptr(n_doc, doc, 0);
                            des_const_xml_char_ptr(n_name, name, 1);
                            des_const_xml_char_ptr(n_external_id, external_id, 2);
                            des_const_xml_char_ptr(n_system_id, system_id, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlCreateIntSubset",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlCreateIntSubset()"
                                );
                                eprint!(" {}", n_doc);
                                eprint!(" {}", n_name);
                                eprint!(" {}", n_external_id);
                                eprintln!(" {}", n_system_id);
                            }
                        }
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
    fn test_xml_first_element_child() {
        #[cfg(feature = "tree")]
        unsafe {
            let mut leaks = 0;

            for n_parent in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let parent = gen_xml_node_ptr(n_parent, 0);

                let ret_val = xml_first_element_child(parent);
                desret_xml_node_ptr(ret_val);
                des_xml_node_ptr(n_parent, parent, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlFirstElementChild",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlFirstElementChild()"
                    );
                    eprintln!(" {}", n_parent);
                }
            }
        }
    }

    #[test]
    fn test_xml_get_compress_mode() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            let ret_val = xml_get_compress_mode();
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
    fn test_xml_get_doc_compress_mode() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_CONST_XML_DOC_PTR {
                let mem_base = xml_mem_blocks();
                let doc = gen_const_xml_doc_ptr(n_doc, 0);

                let ret_val = xml_get_doc_compress_mode(doc);
                desret_int(ret_val);
                des_const_xml_doc_ptr(n_doc, doc, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlGetDocCompressMode",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlGetDocCompressMode()"
                    );
                    eprintln!(" {}", n_doc);
                }
            }
        }
    }

    #[test]
    fn test_xml_get_no_ns_prop() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_CONST_XML_NODE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let node = gen_const_xml_node_ptr(n_node, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_get_no_ns_prop(node, name);
                    desret_xml_char_ptr(ret_val);
                    des_const_xml_node_ptr(n_node, node, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlGetNoNsProp",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlGetNoNsProp()");
                        eprint!(" {}", n_node);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_has_ns_prop() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_CONST_XML_NODE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name_space in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let node = gen_const_xml_node_ptr(n_node, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let name_space = gen_const_xml_char_ptr(n_name_space, 2);

                        let ret_val = xml_has_ns_prop(node, name, name_space);
                        desret_xml_attr_ptr(ret_val);
                        des_const_xml_node_ptr(n_node, node, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_name_space, name_space, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlHasNsProp",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlHasNsProp()");
                            eprint!(" {}", n_node);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_name_space);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_is_xhtml() {
        unsafe {
            let mut leaks = 0;
            for n_system_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_public_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let system_id = gen_const_xml_char_ptr(n_system_id, 0);
                    let public_id = gen_const_xml_char_ptr(n_public_id, 1);

                    let ret_val = xml_is_xhtml(system_id as *const XmlChar, public_id);
                    desret_int(ret_val);
                    des_const_xml_char_ptr(n_system_id, system_id, 0);
                    des_const_xml_char_ptr(n_public_id, public_id, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlIsXHTML",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlIsXHTML()");
                        eprint!(" {}", n_system_id);
                        eprintln!(" {}", n_public_id);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_last_element_child() {
        #[cfg(feature = "tree")]
        unsafe {
            let mut leaks = 0;

            for n_parent in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let parent = gen_xml_node_ptr(n_parent, 0);

                let ret_val = xml_last_element_child(parent);
                desret_xml_node_ptr(ret_val);
                des_xml_node_ptr(n_parent, parent, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlLastElementChild",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlLastElementChild()"
                    );
                    eprintln!(" {}", n_parent);
                }
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
    fn test_xml_new_char_ref() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_new_char_ref(doc, name);
                    desret_xml_node_ptr(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewCharRef",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNewCharRef()");
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_child() {
        #[cfg(any(feature = "tree", feature = "schema"))]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "tree")]
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
        #[cfg(feature = "tree")]
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
        #[cfg(feature = "tree")]
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
    fn test_xml_new_dtd() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_external_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_system_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let doc = gen_xml_doc_ptr(n_doc, 0);
                            let name = gen_const_xml_char_ptr(n_name, 1);
                            let external_id = gen_const_xml_char_ptr(n_external_id, 2);
                            let system_id = gen_const_xml_char_ptr(n_system_id, 3);

                            let ret_val = xml_new_dtd(doc, name, external_id, system_id);
                            desret_xml_dtd_ptr(ret_val);
                            des_xml_doc_ptr(n_doc, doc, 0);
                            des_const_xml_char_ptr(n_name, name, 1);
                            des_const_xml_char_ptr(n_external_id, external_id, 2);
                            des_const_xml_char_ptr(n_system_id, system_id, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlNewDtd",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlNewDtd()");
                                eprint!(" {}", n_doc);
                                eprint!(" {}", n_name);
                                eprint!(" {}", n_external_id);
                                eprintln!(" {}", n_system_id);
                            }
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
        #[cfg(any(feature = "tree", feature = "html", feature = "schema"))]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "tree")]
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
    fn test_xml_new_reference() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_CONST_XML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_const_xml_doc_ptr(n_doc, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_new_reference(doc, name);
                    desret_xml_node_ptr(ret_val);
                    des_const_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewReference",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNewReference()");
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_name);
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
        #[cfg(feature = "tree")]
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
    fn test_xml_next_element_sibling() {
        #[cfg(feature = "tree")]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let node = gen_xml_node_ptr(n_node, 0);

                let ret_val = xml_next_element_sibling(node);
                desret_xml_node_ptr(ret_val);
                des_xml_node_ptr(n_node, node, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNextElementSibling",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlNextElementSibling()"
                    );
                    eprintln!(" {}", n_node);
                }
            }
        }
    }

    #[test]
    fn test_xml_node_add_content_len() {
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_XML_NODE_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let cur = gen_xml_node_ptr(n_cur, 0);
                        let content = gen_const_xml_char_ptr(n_content, 1);
                        let mut len = gen_int(n_len, 2);
                        if !content.is_null() && len > xml_strlen(content) {
                            len = 0;
                        }

                        xml_node_add_content_len(cur, content, len);
                        des_xml_node_ptr(n_cur, cur, 0);
                        des_const_xml_char_ptr(n_content, content, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNodeAddContentLen",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlNodeAddContentLen()"
                            );
                            eprint!(" {}", n_cur);
                            eprint!(" {}", n_content);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_node_get_space_preserve() {
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_CONST_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let cur = gen_const_xml_node_ptr(n_cur, 0);

                let ret_val = xml_node_get_space_preserve(cur);
                desret_int(ret_val);
                des_const_xml_node_ptr(n_cur, cur, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNodeGetSpacePreserve",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlNodeGetSpacePreserve()"
                    );
                    eprintln!(" {}", n_cur);
                }
            }
        }
    }

    #[test]
    fn test_xml_node_list_get_raw_string() {
        #[cfg(feature = "tree")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_CONST_XML_DOC_PTR {
                for n_list in 0..GEN_NB_CONST_XML_NODE_PTR {
                    for n_in_line in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_const_xml_doc_ptr(n_doc, 0);
                        let list = gen_const_xml_node_ptr(n_list, 1);
                        let in_line = gen_int(n_in_line, 2);

                        let ret_val = xml_node_list_get_raw_string(doc, list, in_line);
                        desret_xml_char_ptr(ret_val);
                        des_const_xml_doc_ptr(n_doc, doc, 0);
                        des_const_xml_node_ptr(n_list, list, 1);
                        des_int(n_in_line, in_line, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNodeListGetRawString",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlNodeListGetRawString()"
                            );
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_list);
                            eprintln!(" {}", n_in_line);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_node_list_get_string() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_list in 0..GEN_NB_CONST_XML_NODE_PTR {
                    for n_in_line in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_xml_doc_ptr(n_doc, 0);
                        let list = gen_const_xml_node_ptr(n_list, 1);
                        let in_line = gen_int(n_in_line, 2);

                        let ret_val = xml_node_list_get_string(doc, list, in_line);
                        desret_xml_char_ptr(ret_val);
                        des_xml_doc_ptr(n_doc, doc, 0);
                        des_const_xml_node_ptr(n_list, list, 1);
                        des_int(n_in_line, in_line, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNodeListGetString",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlNodeListGetString()"
                            );
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_list);
                            eprintln!(" {}", n_in_line);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_node_set_content() {
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_XML_NODE_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_ptr(n_cur, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    xml_node_set_content(cur, content);
                    des_xml_node_ptr(n_cur, cur, 0);
                    des_const_xml_char_ptr(n_content, content, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNodeSetContent",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNodeSetContent()");
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_content);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_node_set_content_len() {
        #[cfg(feature = "tree")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_NODE_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let cur = gen_xml_node_ptr(n_cur, 0);
                        let content = gen_const_xml_char_ptr(n_content, 1);
                        let mut len = gen_int(n_len, 2);
                        if !content.is_null() && len > xml_strlen(content) {
                            len = 0;
                        }

                        xml_node_set_content_len(cur, content, len);
                        des_xml_node_ptr(n_cur, cur, 0);
                        des_const_xml_char_ptr(n_content, content, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNodeSetContentLen",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlNodeSetContentLen()"
                            );
                            eprint!(" {}", n_cur);
                            eprint!(" {}", n_content);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_node_set_lang() {
        #[cfg(feature = "tree")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_NODE_PTR {
                for n_lang in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_ptr(n_cur, 0);
                    let lang = gen_const_xml_char_ptr(n_lang, 1);

                    xml_node_set_lang(cur, lang);
                    des_xml_node_ptr(n_cur, cur, 0);
                    des_const_xml_char_ptr(n_lang, lang, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNodeSetLang",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNodeSetLang()");
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_lang);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_node_set_space_preserve() {
        #[cfg(feature = "tree")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_NODE_PTR {
                for n_val in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_ptr(n_cur, 0);
                    let val = gen_int(n_val, 1);

                    xml_node_set_space_preserve(cur, val);
                    des_xml_node_ptr(n_cur, cur, 0);
                    des_int(n_val, val, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNodeSetSpacePreserve",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlNodeSetSpacePreserve()"
                        );
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_val);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_previous_element_sibling() {
        #[cfg(feature = "tree")]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let node = gen_xml_node_ptr(n_node, 0);

                let ret_val = xml_previous_element_sibling(node);
                desret_xml_node_ptr(ret_val);
                des_xml_node_ptr(n_node, node, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlPreviousElementSibling",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlPreviousElementSibling()"
                    );
                    eprintln!(" {}", n_node);
                }
            }
        }
    }

    #[test]
    fn test_xml_reconciliate_ns() {
        #[cfg(feature = "tree")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_tree in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let tree = gen_xml_node_ptr(n_tree, 1);

                    let ret_val = xml_reconciliate_ns(doc, tree);
                    desret_int(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_xml_node_ptr(n_tree, tree, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlReconciliateNs",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlReconciliateNs()");
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_tree);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_remove_prop() {
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_XML_ATTR_PTR {
                let mem_base = xml_mem_blocks();
                let mut cur = gen_xml_attr_ptr(n_cur, 0);

                let ret_val = xml_remove_prop(cur);
                cur = null_mut();
                desret_int(ret_val);
                des_xml_attr_ptr(n_cur, cur, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlRemoveProp",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlRemoveProp()");
                    eprintln!(" {}", n_cur);
                }
            }
        }
    }

    #[test]
    fn test_xml_search_ns() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    for n_name_space in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_xml_doc_ptr(n_doc, 0);
                        let node = gen_xml_node_ptr(n_node, 1);
                        let name_space = gen_const_xml_char_ptr(n_name_space, 2);

                        let ret_val = xml_search_ns(doc, node, name_space);
                        desret_xml_ns_ptr(ret_val);
                        des_xml_doc_ptr(n_doc, doc, 0);
                        des_xml_node_ptr(n_node, node, 1);
                        des_const_xml_char_ptr(n_name_space, name_space, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSearchNs",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlSearchNs()");
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_node);
                            eprintln!(" {}", n_name_space);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_search_ns_by_href() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    for n_href in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_xml_doc_ptr(n_doc, 0);
                        let node = gen_xml_node_ptr(n_node, 1);
                        let href = gen_const_xml_char_ptr(n_href, 2);

                        let ret_val = xml_search_ns_by_href(doc, node, href);
                        desret_xml_ns_ptr(ret_val);
                        des_xml_doc_ptr(n_doc, doc, 0);
                        des_xml_node_ptr(n_node, node, 1);
                        des_const_xml_char_ptr(n_href, href, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSearchNsByHref",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlSearchNsByHref()");
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_node);
                            eprintln!(" {}", n_href);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_set_compress_mode() {
        unsafe {
            let mut leaks = 0;

            for n_mode in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let mode = gen_int(n_mode, 0);

                xml_set_compress_mode(mode);
                des_int(n_mode, mode, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSetCompressMode",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlSetCompressMode()"
                    );
                    eprintln!(" {}", n_mode);
                }
            }
        }
    }

    #[test]
    fn test_xml_set_doc_compress_mode() {
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_mode in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let mode = gen_int(n_mode, 1);

                    xml_set_doc_compress_mode(doc, mode);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_int(n_mode, mode, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSetDocCompressMode",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlSetDocCompressMode()"
                        );
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_mode);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_set_ns() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_ns in 0..GEN_NB_XML_NS_PTR {
                    let mem_base = xml_mem_blocks();
                    let node = gen_xml_node_ptr(n_node, 0);
                    let ns = gen_xml_ns_ptr(n_ns, 1);

                    xml_set_ns(node, ns);
                    des_xml_node_ptr(n_node, node, 0);
                    des_xml_ns_ptr(n_ns, ns, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSetNs",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlSetNs()");
                        eprint!(" {}", n_node);
                        eprintln!(" {}", n_ns);
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
    fn test_xml_string_get_node_list() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_CONST_XML_DOC_PTR {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_const_xml_doc_ptr(n_doc, 0);
                    let value = gen_const_xml_char_ptr(n_value, 1);

                    let ret_val = xml_string_get_node_list(doc, value);
                    desret_xml_node_ptr(ret_val);
                    des_const_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_char_ptr(n_value, value, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlStringGetNodeList",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlStringGetNodeList()"
                        );
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_value);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_string_len_get_node_list() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_CONST_XML_DOC_PTR {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_const_xml_doc_ptr(n_doc, 0);
                        let value = gen_const_xml_char_ptr(n_value, 1);
                        let mut len = gen_int(n_len, 2);
                        if !value.is_null() && len > xml_strlen(value) {
                            len = 0;
                        }

                        let ret_val = xml_string_len_get_node_list(doc, value, len);
                        desret_xml_node_ptr(ret_val);
                        des_const_xml_doc_ptr(n_doc, doc, 0);
                        des_const_xml_char_ptr(n_value, value, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlStringLenGetNodeList",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlStringLenGetNodeList()"
                            );
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_value);
                            eprintln!(" {}", n_len);
                        }
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
    fn test_xml_unset_ns_prop() {
        #[cfg(any(feature = "tree", feature = "schema"))]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_ns in 0..GEN_NB_XML_NS_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let node = gen_xml_node_ptr(n_node, 0);
                        let ns = gen_xml_ns_ptr(n_ns, 1);
                        let name = gen_const_xml_char_ptr(n_name, 2);

                        let ret_val = xml_unset_ns_prop(node, ns, name);
                        desret_int(ret_val);
                        des_xml_node_ptr(n_node, node, 0);
                        des_xml_ns_ptr(n_ns, ns, 1);
                        des_const_xml_char_ptr(n_name, name, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlUnsetNsProp",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlUnsetNsProp()");
                            eprint!(" {}", n_node);
                            eprint!(" {}", n_ns);
                            eprintln!(" {}", n_name);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_unset_prop() {
        #[cfg(any(feature = "tree", feature = "schema"))]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let node = gen_xml_node_ptr(n_node, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_unset_prop(node, name);
                    desret_int(ret_val);
                    des_xml_node_ptr(n_node, node, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlUnsetProp",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlUnsetProp()");
                        eprint!(" {}", n_node);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_validate_ncname() {
        #[cfg(any(
            feature = "tree",
            feature = "xpath",
            feature = "schema",
            feature = "libxml_debug"
        ))]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "tree")]
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
        #[cfg(any(feature = "tree", feature = "schema"))]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "tree")]
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
        #[cfg(any(feature = "tree", feature = "schema"))]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "tree")]
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
        #[cfg(any(feature = "tree", feature = "schema"))]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "tree")]
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

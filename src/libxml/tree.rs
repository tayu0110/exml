//! Provide methods and data structures for tree manipulation.  
//! This module is based on `libxml/tree.h`, `tree.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    any::type_name,
    ffi::{c_char, c_int, c_long, c_uint, c_ulong, c_ushort, CStr},
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::{AtomicI32, AtomicPtr, Ordering},
};

use libc::{fwrite, memcpy, memmove, memset, ptrdiff_t, size_t, snprintf, strlen, FILE};

use crate::{
    libxml::{
        dict::xml_dict_free,
        entities::XmlEntityPtr,
        valid::{xml_free_id_table, xml_free_ref_table, xml_get_dtd_qattr_desc, xml_remove_id},
        xml_io::{XmlOutputBuffer, XmlOutputBufferPtr},
        xmlstring::XmlChar,
    },
    private::{
        buf::{
            xml_buf_add, xml_buf_back_to_buffer, xml_buf_cat, xml_buf_create, xml_buf_create_size,
            xml_buf_detach, xml_buf_free, xml_buf_from_buffer, xml_buf_is_empty,
            xml_buf_set_allocation_scheme,
        },
        entities::{xml_encode_attribute_entities, XML_ENT_EXPANDING, XML_ENT_PARSED},
        error::__xml_simple_error,
    },
    CHECK_COMPAT, IS_BLANK_CH, UPDATE_COMPAT,
};

use super::{
    dict::{xml_dict_lookup, xml_dict_owns, XmlDict, XmlDictPtr},
    encoding::XmlCharEncoding,
    entities::{
        xml_encode_entities_reentrant, xml_free_entities_table, xml_get_doc_entity,
        XmlEntitiesTablePtr, XmlEntityType,
    },
    globals::{
        xmlBufferAllocScheme, xmlDefaultBufferSize, xmlDeregisterNodeDefaultValue,
        xmlRegisterNodeDefaultValue, xml_free, xml_malloc, xml_malloc_atomic, xml_realloc,
    },
    hash::{xmlHashLookup, xmlHashRemoveEntry},
    parser_internals::{
        xml_copy_char_multi_byte, XML_STRING_COMMENT, XML_STRING_TEXT, XML_STRING_TEXT_NOENC,
    },
    uri::xml_build_uri,
    valid::{
        xml_add_id, xml_free_attribute_table, xml_free_element_table, xml_free_notation_table,
        xml_get_dtd_attr_desc, xml_is_id, XmlAttributeTablePtr, XmlElementTablePtr,
        XmlNotationTablePtr,
    },
    xmlerror::{XmlErrorDomain, XmlParserErrors},
    xmlregexp::XmlRegexpPtr,
    xmlstring::{
        xml_str_equal, xml_strcasecmp, xml_strcat, xml_strchr, xml_strdup, xml_strlen, xml_strncat,
        xml_strncat_new, xml_strncmp, xml_strndup,
    },
};

/**
 * BASE_BUFFER_SIZE:
 *
 * default buffer size 4000.
 */
pub const BASE_BUFFER_SIZE: usize = 4096;

/**
 * LIBXML_NAMESPACE_DICT:
 *
 * Defines experimental behaviour:
 * 1) xmlNs gets an additional field @context (a xmlDoc)
 * 2) when creating a tree, xmlNs->href is stored in the dict of xmlDoc.
 */
/* #define LIBXML_NAMESPACE_DICT */

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

/**
 * xmlBuffer:
 *
 * A buffer structure, this old construct is limited to 2GB and
 * is being deprecated, use API with xmlBuf instead
 */
pub type XmlBufferPtr = *mut XmlBuffer;
#[repr(C)]
pub struct XmlBuffer {
    pub content: *mut XmlChar,                   /* The buffer content UTF8 */
    pub using: c_uint,                           /* The buffer size used */
    pub(crate) size: c_uint,                     /* The buffer size */
    pub(crate) alloc: XmlBufferAllocationScheme, /* The realloc method */
    pub(crate) content_io: *mut XmlChar,         /* in IO mode we may have a different base */
}

/**
 * xmlBuf:
 *
 * A buffer structure, new one, the actual structure internals are not public
 */

#[repr(C)]
pub struct XmlBuf {
    pub(crate) content: *mut XmlChar, /* The buffer content UTF8 */
    pub(crate) compat_use: c_uint,    /* for binary compatibility */
    pub(crate) compat_size: c_uint,   /* for binary compatibility */
    pub(crate) alloc: XmlBufferAllocationScheme, /* The realloc method */
    pub(crate) content_io: *mut XmlChar, /* in IO mode we may have a different base */
    pub(crate) using: size_t,         /* The buffer size used */
    pub(crate) size: size_t,          /* The buffer size */
    pub(crate) buffer: XmlBufferPtr,  /* wrapper for an old buffer */
    pub(crate) error: c_int,          /* an error code if a failure occurred */
}

/**
 * xmlBufPtr:
 *
 * A pointer to a buffer structure, the actual structure internals are not
 * public
 */

pub type XmlBufPtr = *mut XmlBuf;

pub(crate) static __XML_REGISTER_CALLBACKS: AtomicI32 = AtomicI32::new(0);

/*
 * LIBXML2_NEW_BUFFER:
 *
 * Macro used to express that the API use the new buffers for
 * xmlParserInputBuffer and xmlOutputBuffer. The change was
 * introduced in 2.9.0.
 */
// #define LIBXML2_NEW_BUFFER

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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlElementType {
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

/** DOC_DISABLE */
/* For backward compatibility */
// const XML_DOCB_DOCUMENT_NODE: usize = 21;
/** DOC_ENABLE */

/**
 * xmlNotation:
 *
 * A DTD Notation definition.
 */
pub type XmlNotationPtr = *mut XmlNotation;
#[repr(C)]
pub struct XmlNotation {
    pub(crate) name: *const XmlChar,      /* Notation name */
    pub(crate) public_id: *const XmlChar, /* Public identifier, if any */
    pub(crate) system_id: *const XmlChar, /* System identifier, if any */
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
 * xmlEnumeration:
 *
 * List structure used when there is an enumeration in DTDs.
 */
pub type XmlEnumerationPtr = *mut XmlEnumeration;
#[repr(C)]
pub struct XmlEnumeration {
    pub(crate) next: *mut XmlEnumeration, /* next one */
    pub(crate) name: *const XmlChar,      /* Enumeration name */
}

/**
 * xmlAttribute:
 *
 * An Attribute declaration in a DTD.
 */
pub type XmlAttributePtr = *mut XmlAttribute;
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct XmlAttribute {
    pub(crate) _private: *mut c_void,  /* application data */
    pub(crate) typ: XmlElementType,    /* XML_ATTRIBUTE_DECL, must be second ! */
    pub(crate) name: *const XmlChar,   /* Attribute name */
    pub(crate) children: *mut XmlNode, /* NULL */
    pub(crate) last: *mut XmlNode,     /* NULL */
    pub(crate) parent: *mut XmlDtd,    /* -> DTD */
    pub(crate) next: *mut XmlNode,     /* next sibling link  */
    pub(crate) prev: *mut XmlNode,     /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,       /* the containing document */

    pub(crate) nexth: *mut XmlAttribute, /* next in hash table */
    pub(crate) atype: XmlAttributeType,  /* The attribute type */
    pub(crate) def: XmlAttributeDefault, /* the default */
    pub(crate) default_value: *const XmlChar, /* or the default value */
    pub(crate) tree: XmlEnumerationPtr,  /* or the enumeration tree if any */
    pub(crate) prefix: *const XmlChar,   /* the namespace prefix if any */
    pub(crate) elem: *const XmlChar,     /* Element holding the attribute */
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

// #include <libxml/xmlregexp.h>

/**
 * xmlElement:
 *
 * An XML Element declaration from a DTD.
 */
pub type XmlElementPtr = *mut XmlElement;
#[repr(C)]
pub struct XmlElement {
    pub(crate) _private: *mut c_void,  /* application data */
    pub(crate) typ: XmlElementType,    /* XML_ELEMENT_DECL, must be second ! */
    pub(crate) name: *const XmlChar,   /* Element name */
    pub(crate) children: *mut XmlNode, /* NULL */
    pub(crate) last: *mut XmlNode,     /* NULL */
    pub(crate) parent: *mut XmlDtd,    /* -> DTD */
    pub(crate) next: *mut XmlNode,     /* next sibling link  */
    pub(crate) prev: *mut XmlNode,     /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,       /* the containing document */

    pub(crate) etype: XmlElementTypeVal,      /* The type */
    pub(crate) content: XmlElementContentPtr, /* the allowed element content */
    pub(crate) attributes: XmlAttributePtr,   /* List of the declared attributes */
    pub(crate) prefix: *const XmlChar,        /* the namespace prefix if any */
    #[cfg(feature = "regexp")]
    pub(crate) cont_model: XmlRegexpPtr, /* the validating regexp */
    #[cfg(not(feature = "regexp"))]
    pub(crate) cont_model: *mut c_void,
}

/**
 * XML_LOCAL_NAMESPACE:
 *
 * A namespace declaration node.
 */
pub(crate) const XML_LOCAL_NAMESPACE: XmlElementType = XmlElementType::XmlNamespaceDecl;
pub type XmlNsType = XmlElementType;

/**
 * xmlNs:
 *
 * An XML namespace.
 * Note that prefix == NULL is valid, it defines the default namespace
 * within the subtree (until overridden).
 *
 * xmlNsType is unified with xmlElementType.
 */

pub type XmlNsPtr = *mut XmlNs;
#[repr(C)]
pub struct XmlNs {
    pub next: AtomicPtr<XmlNs>,             /* next Ns link for this node  */
    pub(crate) typ: Option<XmlNsType>,      /* global or local */
    pub href: AtomicPtr<XmlChar>,           /* URL for the namespace */
    pub prefix: AtomicPtr<XmlChar>,         /* prefix for the namespace */
    pub(crate) _private: AtomicPtr<c_void>, /* application data */
    pub(crate) context: AtomicPtr<XmlDoc>,  /* normally an xmlDoc */
}

/**
 * xmlDtd:
 *
 * An XML DTD, as defined by <!DOCTYPE ... There is actually one for
 * the internal subset and for the external subset.
 */
pub type XmlDtdPtr = *mut XmlDtd;
#[repr(C)]
pub struct XmlDtd {
    pub(crate) _private: *mut c_void,  /* application data */
    pub(crate) typ: XmlElementType,    /* XML_DTD_NODE, must be second ! */
    pub(crate) name: *const XmlChar,   /* Name of the DTD */
    pub(crate) children: *mut XmlNode, /* the value of the property link */
    pub(crate) last: *mut XmlNode,     /* last child link */
    pub(crate) parent: *mut XmlDoc,    /* child->parent link */
    pub(crate) next: *mut XmlNode,     /* next sibling link  */
    pub(crate) prev: *mut XmlNode,     /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,       /* the containing document */

    /* End of common part */
    pub(crate) notations: *mut c_void, /* Hash table for notations if any */
    pub(crate) elements: *mut c_void,  /* Hash table for elements if any */
    pub(crate) attributes: *mut c_void, /* Hash table for attributes if any */
    pub(crate) entities: *mut c_void,  /* Hash table for entities if any */
    pub(crate) external_id: *const XmlChar, /* External identifier for PUBLIC DTD */
    pub(crate) system_id: *const XmlChar, /* URI for a SYSTEM or PUBLIC DTD */
    pub(crate) pentities: *mut c_void, /* Hash table for param entities if any */
}

/**
 * xmlAttr:
 *
 * An attribute on an XML node.
 */
pub type XmlAttrPtr = *mut XmlAttr;
#[repr(C)]
pub struct XmlAttr {
    pub(crate) _private: *mut c_void,           /* application data */
    pub(crate) typ: XmlElementType,             /* XML_ATTRIBUTE_NODE, must be second ! */
    pub(crate) name: *const XmlChar,            /* the name of the property */
    pub(crate) children: *mut XmlNode,          /* the value of the property */
    pub(crate) last: *mut XmlNode,              /* NULL */
    pub(crate) parent: *mut XmlNode,            /* child->parent link */
    pub(crate) next: *mut XmlAttr,              /* next sibling link  */
    pub(crate) prev: *mut XmlAttr,              /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,                /* the containing document */
    pub(crate) ns: *mut XmlNs,                  /* pointer to the associated namespace */
    pub(crate) atype: Option<XmlAttributeType>, /* the attribute type if validating */
    pub(crate) psvi: *mut c_void,               /* for type/PSVI information */
}

/**
 * xmlID:
 *
 * An XML ID instance.
 */
pub type XmlIDPtr = *mut XmlID;
#[repr(C)]
pub struct XmlID {
    pub(crate) next: *mut XmlID,      /* next ID */
    pub(crate) value: *const XmlChar, /* The ID name */
    pub(crate) attr: XmlAttrPtr,      /* The attribute holding it */
    pub(crate) name: *const XmlChar,  /* The attribute if attr is not available */
    pub(crate) lineno: c_int,         /* The line number if attr is not available */
    pub(crate) doc: *mut XmlDoc,      /* The document holding the ID */
}

/**
 * xmlRef:
 *
 * An XML IDREF instance.
 */
pub type XmlRefPtr = *mut XmlRef;
#[repr(C)]
pub struct XmlRef {
    pub(crate) next: *mut XmlRef,     /* next Ref */
    pub(crate) value: *const XmlChar, /* The Ref name */
    pub(crate) attr: XmlAttrPtr,      /* The attribute holding it */
    pub(crate) name: *const XmlChar,  /* The attribute if attr is not available */
    pub(crate) lineno: c_int,         /* The line number if attr is not available */
}

/**
 * xmlNode:
 *
 * A node in an XML tree.
 */
pub type XmlNodePtr = *mut XmlNode;
#[repr(C)]
pub struct XmlNode {
    pub _private: *mut c_void,       /* application data */
    pub typ: XmlElementType,         /* type number, must be second ! */
    pub name: *const XmlChar,        /* the name of the node, or the entity */
    pub children: *mut XmlNode,      /* parent->childs link */
    pub last: *mut XmlNode,          /* last child link */
    pub(crate) parent: *mut XmlNode, /* child->parent link */
    pub next: *mut XmlNode,          /* next sibling link  */
    pub(crate) prev: *mut XmlNode,   /* previous sibling link  */
    pub doc: *mut XmlDoc,            /* the containing document */

    /* End of common part */
    pub(crate) ns: *mut XmlNs, /* pointer to the associated namespace */
    pub content: *mut XmlChar, /* the content */
    pub(crate) properties: *mut XmlAttr, /* properties list */
    pub ns_def: *mut XmlNs,    /* namespace definitions on this node */
    pub(crate) psvi: *mut c_void, /* for type/PSVI information */
    pub(crate) line: c_ushort, /* line number */
    pub(crate) extra: c_ushort, /* extra data for XPath/XSLT */
}

/**
 * XML_GET_CONTENT:
 *
 * Macro to extract the content pointer of a node.
 */
// macro_rules! XML_GET_CONTENT {
// 	( $( $n:tt )* ) => {
// 		($( $n )*->type == XML_ELEMENT_NODE ? NULL : $( $n )*->content)
// 	}
// }

/**
 * XML_GET_LINE:
 *
 * Macro to extract the line number of an element node.
 */
// macro_rules! XML_GET_LINE {
// 	( $( $n:tt )* ) => {
// 		(xmlGetLineNo$( $n )*)
// 	}
// }

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
 * xmlDoc:
 *
 * An XML document.
 */
pub type XmlDocPtr = *mut XmlDoc;
#[repr(C)]
pub struct XmlDoc {
    pub(crate) _private: *mut c_void, /* application data */
    pub(crate) typ: XmlElementType,   /* XML_DOCUMENT_NODE, must be second ! */
    pub(crate) name: *mut c_char,     /* name/filename/URI of the document */
    pub children: *mut XmlNode,       /* the document tree */
    pub(crate) last: *mut XmlNode,    /* last child link */
    pub(crate) parent: *mut XmlNode,  /* child->parent link */
    pub(crate) next: *mut XmlNode,    /* next sibling link  */
    pub(crate) prev: *mut XmlNode,    /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,      /* autoreference to itself */

    /* End of common part */
    pub(crate) compression: c_int, /* level of zlib compression */
    pub(crate) standalone: c_int,  /* standalone document (no external refs)
                                    1 if standalone="yes"
                                    0 if standalone="no"
                                   -1 if there is no XML declaration
                                   -2 if there is an XML declaration, but no
                                   standalone attribute was specified */
    pub int_subset: *mut XmlDtd,        /* the document internal subset */
    pub(crate) ext_subset: *mut XmlDtd, /* the document external subset */
    pub(crate) old_ns: *mut XmlNs,      /* Global namespace, the old way */
    pub(crate) version: *const XmlChar, /* the XML version string */
    pub(crate) encoding: *const XmlChar, /* external initial encoding, if any */
    pub(crate) ids: *mut c_void,        /* Hash table for ID attributes if any */
    pub(crate) refs: *mut c_void,       /* Hash table for IDREFs attributes if any */
    pub(crate) url: *const XmlChar,     /* The URI for that document */
    pub(crate) charset: c_int,          /* Internal flag for charset handling,
                                        actually an xmlCharEncoding */
    pub dict: *mut XmlDict,       /* dict used to allocate names or NULL */
    pub(crate) psvi: *mut c_void, /* for type/PSVI information */
    pub(crate) parse_flags: c_int, /* set of xmlParserOption used to parse the
                                  document */
    pub properties: c_int, /* set of xmlDocProperties for this document
                           set at the end of parsing */
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
    typ: c_int,
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
    old_ns: XmlNsPtr,    /* old ns decl reference */
    new_ns: XmlNsPtr,    /* new ns decl reference */
    shadow_depth: c_int, /* Shadowed at this depth */
    /*
     * depth:
     * >= 0 == @node's ns-decls
     * -1   == @parent's ns-decls
     * -2   == the (*doc).oldNs XML ns-decl
     * -3   == the (*doc).oldNs storage ns-decls
     * -4   == ns-decls provided via custom ns-handling
     */
    depth: c_int,
}

pub type XmlNsMapPtr = *mut XmlNsMap;
#[repr(C)]
pub struct XmlNsMap {
    first: XmlNsMapItemPtr,
    last: XmlNsMapItemPtr,
    pool: XmlNsMapItemPtr,
}

/**
 * xmlChildrenNode:
 *
 * Macro for compatibility naming layer with libxml1. Maps
 * to "children."
 */
// #ifndef xmlChildrenNode
// #define xmlChildrenNode children
// #endif

/**
 * xmlRootNode:
 *
 * Macro for compatibility naming layer with libxml1. Maps
 * to "children".
 */
// #ifndef xmlRootNode
// #define xmlRootNode children
// #endif

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
 * A few public routines for xmlBuf. As those are expected to be used
 * mostly internally the bulk of the routines are internal in buf.h
 */
/**
 * xmlBufContent:
 * @buf:  the buffer
 *
 * Function to extract the content of a buffer
 *
 * Returns the internal content
 */
pub unsafe extern "C" fn xml_buf_content(buf: *const XmlBuf) -> *mut XmlChar {
    if buf.is_null() || (*buf).error != 0 {
        return null_mut();
    }

    (*buf).content
}

/**
 * xmlBufEnd:
 * @buf:  the buffer
 *
 * Function to extract the end of the content of a buffer
 *
 * Returns the end of the internal content or NULL in case of error
 */
pub unsafe extern "C" fn xml_buf_end(buf: XmlBufPtr) -> *mut XmlChar {
    if buf.is_null() || (*buf).error != 0 {
        return null_mut();
    }
    CHECK_COMPAT!(buf);

    (*buf).content.add((*buf).using)
}

/**
 * xmlBufUse:
 * @buf:  the buffer
 *
 * Function to get the length of a buffer
 *
 * Returns the length of data in the internal content
 */
pub unsafe extern "C" fn xml_buf_use(buf: XmlBufPtr) -> size_t {
    if buf.is_null() || (*buf).error != 0 {
        return 0;
    }
    CHECK_COMPAT!(buf);

    (*buf).using
}
/**
 * xmlBufShrink:
 * @buf:  the buffer to dump
 * @len:  the number of XmlChar to remove
 *
 * Remove the beginning of an XML buffer.
 * NOTE that this routine behaviour differs from xmlBufferShrink()
 * as it will return 0 on error instead of -1 due to size_t being
 * used as the return type.
 *
 * Returns the number of byte removed or 0 in case of failure
 */
pub unsafe extern "C" fn xml_buf_shrink(buf: XmlBufPtr, len: size_t) -> size_t {
    if buf.is_null() || (*buf).error != 0 {
        return 0;
    }
    CHECK_COMPAT!(buf);
    if len == 0 {
        return 0;
    }
    if len > (*buf).using {
        return 0;
    }

    (*buf).using -= len;
    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
        && !(*buf).content_io.is_null()
    {
        /*
         * we just move the content pointer, but also make sure
         * the perceived buffer size has shrunk accordingly
         */
        (*buf).content = (*buf).content.add(len);
        (*buf).size -= len;

        /*
         * sometimes though it maybe be better to really shrink
         * on IO buffers
         */
        if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
            && !(*buf).content_io.is_null()
        {
            let start_buf: size_t = (*buf).content.offset_from((*buf).content_io) as _;
            if start_buf >= (*buf).size {
                memmove(
                    (*buf).content_io as _,
                    (*buf).content.add(0) as _,
                    (*buf).using,
                );
                (*buf).content = (*buf).content_io;
                *(*buf).content.add((*buf).using) = 0;
                (*buf).size += start_buf;
            }
        }
    } else {
        memmove(
            (*buf).content as _,
            (*buf).content.add(len) as _,
            (*buf).using,
        );
        *(*buf).content.add((*buf).using) = 0;
    }
    UPDATE_COMPAT!(buf);
    len
}

/*
 * Variables.
 */

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
pub unsafe extern "C" fn xml_validate_ncname(value: *const XmlChar, space: c_int) -> c_int {
    use crate::{IS_BLANK, IS_COMBINING, IS_DIGIT, IS_EXTENDER, IS_LETTER};

    let mut cur: *const XmlChar = value;
    let mut c: i32;
    let mut l: c_int = 0;

    if value.is_null() {
        return -1;
    }

    /*
     * First quick algorithm for ASCII range
     */
    if space != 0 {
        while IS_BLANK_CH!(*cur) {
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
            while IS_BLANK_CH!(*cur) {
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
        while IS_BLANK!(c) {
            cur = cur.add(l as usize);
            c = CUR_SCHAR!(cur, l);
        }
    }
    if !IS_LETTER!(c as u32) && c != b'_' as i32 {
        return 1;
    }
    cur = cur.add(l as usize);
    c = CUR_SCHAR!(cur, l);
    while IS_LETTER!(c as u32)
        || IS_DIGIT!(c as u32)
        || c == b'.' as i32
        || c == b'-' as i32
        || c == b'_' as i32
        || IS_COMBINING!(c as u32)
        || IS_EXTENDER!(c as u32)
    {
        cur = cur.add(l as usize);
        c = CUR_SCHAR!(cur, l);
    }
    if space != 0 {
        while IS_BLANK!(c) {
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
pub unsafe extern "C" fn xml_validate_qname(value: *const XmlChar, space: c_int) -> c_int {
    use crate::{IS_BLANK, IS_COMBINING, IS_DIGIT, IS_EXTENDER, IS_LETTER};

    let mut cur: *const XmlChar = value;
    let mut c: c_int;
    let mut l: c_int = 0;

    if value.is_null() {
        return -1;
    }
    /*
     * First quick algorithm for ASCII range
     */
    if space != 0 {
        while IS_BLANK_CH!(*cur) {
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
            while IS_BLANK_CH!(*cur) {
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
        while IS_BLANK!(c) {
            cur = cur.add(l as usize);
            c = CUR_SCHAR!(cur, l);
        }
    }
    if !IS_LETTER!(c as u32) && c != b'_' as i32 {
        return 1;
    }
    cur = cur.add(l as usize);
    c = CUR_SCHAR!(cur, l);
    while IS_LETTER!(c as u32)
        || IS_DIGIT!(c as u32)
        || c == b'.' as i32
        || c == b'-' as i32
        || c == b'_' as i32
        || IS_COMBINING!(c as u32)
        || IS_EXTENDER!(c as u32)
    {
        cur = cur.add(l as usize);
        c = CUR_SCHAR!(cur, l);
    }
    if c == b':' as i32 {
        cur = cur.add(l as usize);
        c = CUR_SCHAR!(cur, l);
        if !IS_LETTER!(c as u32) && c != b'_' as i32 {
            return 1;
        }
        cur = cur.add(l as usize);
        c = CUR_SCHAR!(cur, l);
        while IS_LETTER!(c as u32)
            || IS_DIGIT!(c as u32)
            || c == b'.' as i32
            || c == b'-' as i32
            || c == b'_' as i32
            || IS_COMBINING!(c as u32)
            || IS_EXTENDER!(c as u32)
        {
            cur = cur.add(l as usize);
            c = CUR_SCHAR!(cur, l);
        }
    }
    if space != 0 {
        while IS_BLANK!(c) {
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
pub unsafe extern "C" fn xml_validate_name(value: *const XmlChar, space: c_int) -> c_int {
    use crate::{IS_BLANK, IS_COMBINING, IS_DIGIT, IS_EXTENDER, IS_LETTER};

    let mut cur: *const XmlChar = value;
    let mut c: c_int;
    let mut l: c_int = 0;

    if value.is_null() {
        return -1;
    }
    /*
     * First quick algorithm for ASCII range
     */
    if space != 0 {
        while IS_BLANK_CH!(*cur) {
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
            while IS_BLANK_CH!(*cur) {
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
        while IS_BLANK!(c) {
            cur = cur.add(l as usize);
            c = CUR_SCHAR!(cur, l);
        }
    }
    if !IS_LETTER!(c as u32) && c != b'_' as i32 && c != b':' as i32 {
        return 1;
    }
    cur = cur.add(l as usize);
    c = CUR_SCHAR!(cur, l);
    while IS_LETTER!(c as u32)
        || IS_DIGIT!(c as u32)
        || c == b'.' as i32
        || c == b':' as i32
        || c == b'-' as i32
        || c == b'_' as i32
        || IS_COMBINING!(c as u32)
        || IS_EXTENDER!(c as u32)
    {
        cur = cur.add(l as usize);
        c = CUR_SCHAR!(cur, l);
    }
    if space != 0 {
        while IS_BLANK!(c) {
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
pub unsafe extern "C" fn xml_validate_nmtoken(value: *const XmlChar, space: c_int) -> c_int {
    use crate::{IS_BLANK, IS_COMBINING, IS_DIGIT, IS_EXTENDER, IS_LETTER};

    let mut cur: *const XmlChar = value;
    let mut c: c_int;
    let mut l: c_int = 0;

    if value.is_null() {
        return -1;
    }
    /*
     * First quick algorithm for ASCII range
     */
    if space != 0 {
        while IS_BLANK_CH!(*cur) {
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
            while IS_BLANK_CH!(*cur) {
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
        while IS_BLANK!(c) {
            cur = cur.add(l as usize);
            c = CUR_SCHAR!(cur, l);
        }
    }
    if !(IS_LETTER!(c as u32)
        || IS_DIGIT!(c as u32)
        || c == b'.' as i32
        || c == b':' as i32
        || c == b'-' as i32
        || c == b'_' as i32
        || IS_COMBINING!(c as u32)
        || IS_EXTENDER!(c as u32))
    {
        return 1;
    }
    cur = cur.add(l as usize);
    c = CUR_SCHAR!(cur, l);
    while IS_LETTER!(c as u32)
        || IS_DIGIT!(c as u32)
        || c == b'.' as i32
        || c == b':' as i32
        || c == b'-' as i32
        || c == b'_' as i32
        || IS_COMBINING!(c as u32)
        || IS_EXTENDER!(c as u32)
    {
        cur = cur.add(l as usize);
        c = CUR_SCHAR!(cur, l);
    }
    if space != 0 {
        while IS_BLANK!(c) {
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
        XmlErrorDomain::XmlFromTree as i32,
        XmlParserErrors::XmlErrNoMemory as i32,
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
    len: c_int,
) -> *mut XmlChar {
    let ret: *mut XmlChar;

    if ncname.is_null() {
        return null_mut();
    }
    if prefix.is_null() {
        return ncname as _;
    }

    let lenn: c_int = strlen(ncname as _) as _;
    let lenp: c_int = strlen(prefix as _) as _;

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
    let mut len: c_int = 0;

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
 * @len: an c_int *
 *
 * parse an XML qualified name string,i
 *
 * returns NULL if it is not a Qualified Name, otherwise, update len
 *         with the length in byte of the prefix and return a pointer
 *         to the start of the name without the prefix
 */
pub unsafe extern "C" fn xml_split_qname3(name: *const XmlChar, len: *mut c_int) -> *const XmlChar {
    let mut l: c_int = 0;

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
 * Handling Buffers, the old ones see @xmlBuf for the new ones.
 */

/**
 * xmlSetBufferAllocationScheme:
 * @scheme:  allocation method to use
 *
 * Set the buffer allocation method.  Types are
 * xmlBufferAllocationScheme::XML_BUFFER_ALLOC_EXACT - use exact sizes, keeps memory usage down
 * xmlBufferAllocationScheme::XML_BUFFER_ALLOC_DOUBLEIT - double buffer when extra needed,
 *                             improves performance
 */
pub unsafe extern "C" fn xml_set_buffer_allocation_scheme(scheme: XmlBufferAllocationScheme) {
    if matches!(
        scheme,
        XmlBufferAllocationScheme::XmlBufferAllocExact
            | XmlBufferAllocationScheme::XmlBufferAllocDoubleit
            | XmlBufferAllocationScheme::XmlBufferAllocHybrid
    ) {
        *xmlBufferAllocScheme() = scheme;
    }
}

/**
 * xmlGetBufferAllocationScheme:
 *
 * Types are
 * xmlBufferAllocationScheme::XML_BUFFER_ALLOC_EXACT - use exact sizes, keeps memory usage down
 * xmlBufferAllocationScheme::XML_BUFFER_ALLOC_DOUBLEIT - double buffer when extra needed,
 *                             improves performance
 * xmlBufferAllocationScheme::XML_BUFFER_ALLOC_HYBRID - use exact sizes on small strings to keep memory usage tight
 *                            in normal usage, and doubleit on large strings to avoid
 *                            pathological performance.
 *
 * Returns the current allocation scheme
 */
pub unsafe extern "C" fn xml_get_buffer_allocation_scheme() -> XmlBufferAllocationScheme {
    *xmlBufferAllocScheme()
}

/**
 * xmlBufferCreate:
 *
 * routine to create an XML buffer.
 * returns the new structure.
 */
pub unsafe extern "C" fn xml_buffer_create() -> XmlBufferPtr {
    let ret: XmlBufferPtr = xml_malloc(size_of::<XmlBuffer>()) as _;
    if ret.is_null() {
        xml_tree_err_memory(c"creating buffer".as_ptr() as _);
        return null_mut();
    }
    (*ret).using = 0;
    (*ret).size = *xmlDefaultBufferSize() as _;
    (*ret).alloc = *xmlBufferAllocScheme();
    (*ret).content = xml_malloc_atomic((*ret).size as usize) as _;
    if (*ret).content.is_null() {
        xml_tree_err_memory(c"creating buffer".as_ptr() as _);
        xml_free(ret as _);
        return null_mut();
    }
    *(*ret).content.add(0) = 0;
    (*ret).content_io = null_mut();
    ret
}

/**
 * xmlBufferCreateSize:
 * @size: initial size of buffer
 *
 * routine to create an XML buffer.
 * returns the new structure.
 */
pub unsafe extern "C" fn xml_buffer_create_size(size: size_t) -> XmlBufferPtr {
    if size >= u32::MAX as usize {
        return null_mut();
    }
    let ret: XmlBufferPtr = xml_malloc(size_of::<XmlBuffer>()) as _;
    if ret.is_null() {
        xml_tree_err_memory(c"creating buffer".as_ptr() as _);
        return null_mut();
    }
    (*ret).using = 0;
    (*ret).alloc = *xmlBufferAllocScheme();
    (*ret).size = if size != 0 { size as u32 + 1 } else { 0 }; /* +1 for ending null */
    if (*ret).size != 0 {
        (*ret).content = xml_malloc_atomic((*ret).size as usize) as _;
        if (*ret).content.is_null() {
            xml_tree_err_memory(c"creating buffer".as_ptr() as _);
            xml_free(ret as _);
            return null_mut();
        }
        *(*ret).content.add(0) = 0;
    } else {
        (*ret).content = null_mut();
    }
    (*ret).content_io = null_mut();
    ret
}

/**
 * xmlBufferCreateStatic:
 * @mem: the memory area
 * @size:  the size in byte
 *
 * Create an XML buffer initialized with bytes.
 */
pub unsafe extern "C" fn xml_buffer_create_static(mem: *mut c_void, size: size_t) -> XmlBufferPtr {
    let buf: XmlBufferPtr = xml_buffer_create_size(size);

    xml_buffer_add(buf, mem as _, size as _);
    buf
}

/**
 * xmlBufferResize:
 * @buf:  the buffer to resize
 * @size:  the desired size
 *
 * Resize a buffer to accommodate minimum size of @size.
 *
 * Returns  0 in case of problems, 1 otherwise
 */
pub unsafe extern "C" fn xml_buffer_resize(buf: XmlBufferPtr, size: c_uint) -> c_int {
    let mut new_size: c_uint;
    let rebuf: *mut XmlChar;
    let start_buf: size_t;

    if buf.is_null() {
        return 0;
    }

    /* Don't resize if we don't have to */
    if size < (*buf).size {
        return 1;
    }

    if size > u32::MAX - 10 {
        xml_tree_err_memory(c"growing buffer past UINT_MAX".as_ptr() as _);
        return 0;
    }

    /* figure out new size */
    match (*buf).alloc {
        XmlBufferAllocationScheme::XmlBufferAllocIo
        | XmlBufferAllocationScheme::XmlBufferAllocDoubleit => {
            /*take care of empty case*/
            if (*buf).size == 0 {
                new_size = if size > u32::MAX - 10 {
                    u32::MAX
                } else {
                    size + 10
                };
            } else {
                new_size = (*buf).size;
            }
            while size > new_size {
                if new_size > u32::MAX / 2 {
                    xml_tree_err_memory(c"growing buffer".as_ptr() as _);
                    return 0;
                }
                new_size *= 2;
            }
        }
        XmlBufferAllocationScheme::XmlBufferAllocExact => {
            new_size = if size > u32::MAX - 10 {
                u32::MAX
            } else {
                size + 10
            };
        }
        XmlBufferAllocationScheme::XmlBufferAllocHybrid => {
            if (*buf).using < BASE_BUFFER_SIZE as u32 {
                new_size = size;
            } else {
                new_size = (*buf).size;
                while size > new_size {
                    if new_size > u32::MAX / 2 {
                        xml_tree_err_memory(c"growing buffer".as_ptr() as _);
                        return 0;
                    }
                    new_size *= 2;
                }
            }
        }
        _ => {
            new_size = if size > u32::MAX - 10 {
                u32::MAX
            } else {
                size + 10
            };
        }
    }

    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
        && !(*buf).content_io.is_null()
    {
        start_buf = (*buf).content.offset_from((*buf).content_io) as _;

        if start_buf > new_size as usize {
            /* move data back to start */
            memmove(
                (*buf).content_io as _,
                (*buf).content as _,
                (*buf).using as usize,
            );
            (*buf).content = (*buf).content_io;
            *(*buf).content.add((*buf).using as usize) = 0;
            (*buf).size += start_buf as u32;
        } else {
            rebuf = xml_realloc((*buf).content_io as _, start_buf + new_size as usize) as _;
            if rebuf.is_null() {
                xml_tree_err_memory(c"growing buffer".as_ptr() as _);
                return 0;
            }
            (*buf).content_io = rebuf;
            (*buf).content = rebuf.add(start_buf as usize);
        }
    } else {
        if (*buf).content.is_null() {
            rebuf = xml_malloc_atomic(new_size as usize) as _;
            (*buf).using = 0;
            *rebuf.add((*buf).using as usize) = 0;
        } else if (*buf).size - (*buf).using < 100 {
            rebuf = xml_realloc((*buf).content as _, new_size as usize) as _;
        } else {
            /*
             * if we are reallocating a buffer far from being full, it's
             * better to make a new allocation and copy only the used range
             * and free the old one.
             */
            rebuf = xml_malloc_atomic(new_size as usize) as _;
            if !rebuf.is_null() {
                memcpy(rebuf as _, (*buf).content as _, (*buf).using as usize);
                xml_free((*buf).content as _);
                *rebuf.add((*buf).using as usize) = 0;
            }
        }
        if rebuf.is_null() {
            xml_tree_err_memory(c"growing buffer".as_ptr() as _);
            return 0;
        }
        (*buf).content = rebuf;
    }
    (*buf).size = new_size;

    1
}

/**
 * xmlBufferFree:
 * @buf:  the buffer to free
 *
 * Frees an XML buffer. It frees both the content and the structure which
 * encapsulate it.
 */
pub unsafe extern "C" fn xml_buffer_free(buf: XmlBufferPtr) {
    if buf.is_null() {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlBufferFree: buf.is_null()\n".as_ptr() as _);
        // #endif
        return;
    }

    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
        && !(*buf).content_io.is_null()
    {
        xml_free((*buf).content_io as _);
    } else if !(*buf).content.is_null() {
        xml_free((*buf).content as _);
    }
    xml_free(buf as _);
}

/**
 * xmlBufferDump:
 * @file:  the file output
 * @buf:  the buffer to dump
 *
 * Dumps an XML buffer to  a FILE *.
 * Returns the number of #XmlChar written
 */
pub unsafe extern "C" fn xml_buffer_dump(mut file: *mut FILE, buf: XmlBufferPtr) -> c_int {
    if buf.is_null() {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlBufferDump: buf.is_null()\n".as_ptr() as _);
        // #endif
        return 0;
    }
    if (*buf).content.is_null() {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlBufferDump: (*buf).content.is_null()\n".as_ptr() as _);
        // #endif
        return 0;
    }
    if file.is_null() {
        extern "C" {
            // Does it work ?????
            static stdout: *mut FILE;
        }
        file = stdout;
    }
    let ret: size_t = fwrite((*buf).content as _, 1, (*buf).using as _, file);
    if ret > i32::MAX as usize {
        i32::MAX
    } else {
        ret as _
    }
}

/**
 * xmlBufferAdd:
 * @buf:  the buffer to dump
 * @str:  the #XmlChar string
 * @len:  the number of #XmlChar to add
 *
 * Add a string range to an XML buffer. if len == -1, the length of
 * str is recomputed.
 *
 * Returns 0 successful, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
pub unsafe extern "C" fn xml_buffer_add(
    buf: XmlBufferPtr,
    str: *const XmlChar,
    mut len: c_int,
) -> c_int {
    let need_size: c_uint;

    if str.is_null() || buf.is_null() {
        return -1;
    }
    if len < -1 {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlBufferAdd: len < 0\n".as_ptr() as _);
        // #endif
        return -1;
    }
    if len == 0 {
        return 0;
    }

    if len < 0 {
        len = xml_strlen(str);
    }

    if len < 0 {
        return -1;
    }
    if len == 0 {
        return 0;
    }

    /* Note that both (*buf).size and (*buf).using can be zero here. */
    if len as u32 >= (*buf).size - (*buf).using {
        if len as u32 >= u32::MAX - (*buf).using {
            xml_tree_err_memory(c"growing buffer past UINT_MAX".as_ptr() as _);
            return XmlParserErrors::XmlErrNoMemory as i32;
        }
        need_size = (*buf).using + len as u32 + 1;
        if xml_buffer_resize(buf, need_size) == 0 {
            xml_tree_err_memory(c"growing buffer".as_ptr() as _);
            return XmlParserErrors::XmlErrNoMemory as i32;
        }
    }

    memmove(
        (*buf).content.add((*buf).using as usize) as _,
        str as _,
        len as usize,
    );
    (*buf).using += len as u32;
    *(*buf).content.add((*buf).using as usize) = 0;
    0
}

/**
 * xmlBufferAddHead:
 * @buf:  the buffer
 * @str:  the #XmlChar string
 * @len:  the number of #XmlChar to add
 *
 * Add a string range to the beginning of an XML buffer.
 * if len == -1, the length of @str is recomputed.
 *
 * Returns 0 successful, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
pub unsafe extern "C" fn xml_buffer_add_head(
    buf: XmlBufferPtr,
    str: *const XmlChar,
    mut len: c_int,
) -> c_int {
    let need_size: c_uint;

    if buf.is_null() {
        return -1;
    }
    if str.is_null() {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlBufferAddHead: str.is_null()\n".as_ptr() as _);
        // #endif
        return -1;
    }
    if len < -1 {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlBufferAddHead: len < 0\n".as_ptr() as _);
        // #endif
        return -1;
    }
    if len == 0 {
        return 0;
    }

    if len < 0 {
        len = xml_strlen(str);
    }

    if len <= 0 {
        return -1;
    }

    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
        && !(*buf).content_io.is_null()
    {
        let start_buf: size_t = (*buf).content.offset_from((*buf).content_io) as _;

        if start_buf > len as usize {
            /*
             * We can add it in the space previously shrunk
             */
            (*buf).content = (*buf).content.sub(len as usize);
            memmove((*buf).content.add(0) as _, str as _, len as _);
            (*buf).using += len as u32;
            (*buf).size += len as u32;
            *(*buf).content.add((*buf).using as usize) = 0;
            return 0;
        }
    }
    /* Note that both (*buf).size and (*buf).using can be zero here. */
    if len as u32 >= (*buf).size - (*buf).using {
        if len as u32 >= u32::MAX - (*buf).using {
            xml_tree_err_memory(c"growing buffer past UINT_MAX".as_ptr() as _);
            return -1;
        }
        need_size = (*buf).using + len as u32 + 1;
        if xml_buffer_resize(buf, need_size) == 0 {
            xml_tree_err_memory(c"growing buffer".as_ptr() as _);
            return XmlParserErrors::XmlErrNoMemory as i32;
        }
    }

    memmove(
        (*buf).content.add(len as usize) as _,
        (*buf).content.add(0) as _,
        (*buf).using as usize,
    );
    memmove((*buf).content.add(0) as _, str as _, len as usize);
    (*buf).using += len as u32;
    *(*buf).content.add((*buf).using as usize) = 0;
    0
}

/**
 * xmlBufferCat:
 * @buf:  the buffer to add to
 * @str:  the #XmlChar string
 *
 * Append a zero terminated string to an XML buffer.
 *
 * Returns 0 successful, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
pub unsafe extern "C" fn xml_buffer_cat(buf: XmlBufferPtr, str: *const XmlChar) -> c_int {
    if buf.is_null() {
        return -1;
    }
    if str.is_null() {
        return -1;
    }
    xml_buffer_add(buf, str, -1)
}

/**
 * xmlBufferCCat:
 * @buf:  the buffer to dump
 * @str:  the C c_char string
 *
 * Append a zero terminated C string to an XML buffer.
 *
 * Returns 0 successful, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
pub unsafe extern "C" fn xml_buffer_ccat(buf: XmlBufferPtr, str: *const c_char) -> c_int {
    xml_buffer_cat(buf, str as _)
}

/**
 * xmlBufferShrink:
 * @buf:  the buffer to dump
 * @len:  the number of XmlChar to remove
 *
 * Remove the beginning of an XML buffer.
 *
 * Returns the number of #XmlChar removed, or -1 in case of failure.
 */
pub unsafe extern "C" fn xml_buffer_shrink(buf: XmlBufferPtr, len: c_uint) -> c_int {
    if buf.is_null() {
        return -1;
    }
    if len == 0 {
        return 0;
    }
    if len > (*buf).using {
        return -1;
    }

    (*buf).using -= len;
    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
        && !(*buf).content_io.is_null()
    {
        /*
         * we just move the content pointer, but also make sure
         * the perceived buffer size has shrunk accordingly
         */
        (*buf).content = (*buf).content.add(len as usize);
        (*buf).size -= len;

        /*
         * sometimes though it maybe be better to really shrink
         * on IO buffers
         */
        if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
            && !(*buf).content_io.is_null()
        {
            let start_buf: size_t = (*buf).content.offset_from((*buf).content_io) as _;
            if start_buf >= (*buf).size as usize {
                memmove(
                    (*buf).content_io as _,
                    (*buf).content.add(0) as _,
                    (*buf).using as usize,
                );
                (*buf).content = (*buf).content_io;
                *(*buf).content.add((*buf).using as usize) = 0;
                (*buf).size += start_buf as u32;
            }
        }
    } else {
        memmove(
            (*buf).content as _,
            (*buf).content.add(len as usize) as _,
            (*buf).using as usize,
        );
        *(*buf).content.add((*buf).using as usize) = 0;
    }
    len as _
}

/**
 * xmlBufferGrow:
 * @buf:  the buffer
 * @len:  the minimum free size to allocate
 *
 * Grow the available space of an XML buffer.
 *
 * Returns the new available space or -1 in case of error
 */
pub unsafe extern "C" fn xml_buffer_grow(buf: XmlBufferPtr, len: c_uint) -> c_int {
    let mut size: c_uint;
    let newbuf: *mut XmlChar;

    if buf.is_null() {
        return -1;
    }

    if len < (*buf).size - (*buf).using {
        return 0;
    }
    if len >= u32::MAX - (*buf).using {
        xml_tree_err_memory(c"growing buffer past UINT_MAX".as_ptr() as _);
        return -1;
    }

    if (*buf).size > len {
        size = if (*buf).size > u32::MAX / 2 {
            u32::MAX
        } else {
            (*buf).size * 2
        };
    } else {
        size = (*buf).using + len;
        size = if size > u32::MAX - 100 {
            u32::MAX
        } else {
            size + 100
        };
    }

    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
        && !(*buf).content_io.is_null()
    {
        let start_buf: size_t = (*buf).content.offset_from((*buf).content_io) as _;

        newbuf = xml_realloc((*buf).content_io as _, start_buf + size as usize) as _;
        if newbuf.is_null() {
            xml_tree_err_memory(c"growing buffer".as_ptr() as _);
            return -1;
        }
        (*buf).content_io = newbuf;
        (*buf).content = newbuf.add(start_buf);
    } else {
        newbuf = xml_realloc((*buf).content as _, size as usize) as _;
        if newbuf.is_null() {
            xml_tree_err_memory(c"growing buffer".as_ptr() as _);
            return -1;
        }
        (*buf).content = newbuf;
    }
    (*buf).size = size;
    ((*buf).size - (*buf).using - 1) as i32
}

/**
 * xmlBufferEmpty:
 * @buf:  the buffer
 *
 * empty a buffer.
 */
pub unsafe extern "C" fn xml_buffer_empty(buf: XmlBufferPtr) {
    if buf.is_null() {
        return;
    }
    if (*buf).content.is_null() {
        return;
    }
    (*buf).using = 0;
    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
        && !(*buf).content_io.is_null()
    {
        let start_buf: size_t = (*buf).content.offset_from((*buf).content_io) as _;

        (*buf).size += start_buf as u32;
        (*buf).content = (*buf).content_io;
        *(*buf).content.add(0) = 0;
    } else {
        *(*buf).content.add(0) = 0;
    }
}

/**
 * xmlBufferContent:
 * @buf:  the buffer
 *
 * Function to extract the content of a buffer
 *
 * Returns the internal content
 */
pub unsafe extern "C" fn xml_buffer_content(buf: *const XmlBuffer) -> *const XmlChar {
    if buf.is_null() {
        return null_mut();
    }

    (*buf).content
}

/**
 * xmlBufferDetach:
 * @buf:  the buffer
 *
 * Remove the string contained in a buffer and gie it back to the
 * caller. The buffer is reset to an empty content.
 * This doesn't work with immutable buffers as they can't be reset.
 *
 * Returns the previous string contained by the buffer.
 */
pub unsafe extern "C" fn xml_buffer_detach(buf: XmlBufferPtr) -> *mut XmlChar {
    if buf.is_null() {
        return null_mut();
    }

    let ret: *mut XmlChar = (*buf).content;
    (*buf).content = null_mut();
    (*buf).size = 0;
    (*buf).using = 0;

    ret
}

/**
 * xmlBufferSetAllocationScheme:
 * @buf:  the buffer to tune
 * @scheme:  allocation scheme to use
 *
 * Sets the allocation scheme for this buffer
 */
pub unsafe extern "C" fn xml_buffer_set_allocation_scheme(
    buf: XmlBufferPtr,
    scheme: XmlBufferAllocationScheme,
) {
    if buf.is_null() {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlBufferSetAllocationScheme: buf.is_null()\n".as_ptr() as _);
        // #endif
        return;
    }
    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo) {
        return;
    }
    if matches!(
        scheme,
        XmlBufferAllocationScheme::XmlBufferAllocDoubleit
            | XmlBufferAllocationScheme::XmlBufferAllocExact
            | XmlBufferAllocationScheme::XmlBufferAllocHybrid
    ) {
        (*buf).alloc = scheme;
    }
}

/**
 * xmlBufferLength:
 * @buf:  the buffer
 *
 * Function to get the length of a buffer
 *
 * Returns the length of data in the internal content
 */
pub unsafe extern "C" fn xml_buffer_length(buf: *const XmlBuffer) -> c_int {
    if buf.is_null() {
        return 0;
    }

    (*buf).using as _
}

/*
 * Creating/freeing new structures.
 */
/**
 * xmlCreateIntSubset:
 * @doc:  the document pointer
 * @name:  the DTD name
 * @ExternalID:  the external (PUBLIC) ID
 * @SystemID:  the system ID
 *
 * Create the internal subset of a document
 * Returns a pointer to the new DTD structure
 */
pub unsafe extern "C" fn xml_create_int_subset(
    doc: XmlDocPtr,
    name: *const XmlChar,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) -> XmlDtdPtr {
    if !doc.is_null() && !xml_get_int_subset(doc).is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,

        //      "xmlCreateIntSubset(): document %s already have an internal subset\n",
        // 	    (*doc).name);
        // #endif
        return null_mut();
    }

    /*
     * Allocate a new DTD and fill the fields.
     */
    let cur: XmlDtdPtr = xml_malloc(size_of::<XmlDtd>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building internal subset".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlDtd>());
    (*cur).typ = XmlElementType::XmlDtdNode;

    if !name.is_null() {
        (*cur).name = xml_strdup(name);
        if (*cur).name.is_null() {
            xml_tree_err_memory(c"building internal subset".as_ptr() as _);
            xml_free(cur as _);
            return null_mut();
        }
    }
    if !external_id.is_null() {
        (*cur).external_id = xml_strdup(external_id);
        if (*cur).external_id.is_null() {
            xml_tree_err_memory(c"building internal subset".as_ptr() as _);
            if !(*cur).name.is_null() {
                xml_free((*cur).name as _);
            }
            xml_free(cur as _);
            return null_mut();
        }
    }
    if !system_id.is_null() {
        (*cur).system_id = xml_strdup(system_id);
        if (*cur).system_id.is_null() {
            xml_tree_err_memory(c"building internal subset".as_ptr() as _);
            if !(*cur).name.is_null() {
                xml_free((*cur).name as _);
            }
            if !(*cur).external_id.is_null() {
                xml_free((*cur).external_id as _);
            }
            xml_free(cur as _);
            return null_mut();
        }
    }
    if !doc.is_null() {
        (*doc).int_subset = cur;
        (*cur).parent = doc;
        (*cur).doc = doc;
        if (*doc).children.is_null() {
            (*doc).children = cur as _;
            (*doc).last = cur as _;
        } else if matches!((*doc).typ, XmlElementType::XmlHtmlDocumentNode) {
            let prev: XmlNodePtr = (*doc).children;
            (*prev).prev = cur as _;
            (*cur).next = prev;
            (*doc).children = cur as _;
        } else {
            let mut next: XmlNodePtr;

            next = (*doc).children;
            while !next.is_null() && !matches!((*next).typ, XmlElementType::XmlElementNode) {
                next = (*next).next;
            }
            if next.is_null() {
                (*cur).prev = (*doc).last;
                (*(*cur).prev).next = cur as _;
                (*cur).next = null_mut();
                (*doc).last = cur as _;
            } else {
                (*cur).next = next;
                (*cur).prev = (*next).prev;
                if (*cur).prev.is_null() {
                    (*doc).children = cur as _;
                } else {
                    (*(*cur).prev).next = cur as _;
                }
                (*next).prev = cur as _;
            }
        }
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xmlRegisterNodeDefaultValue(cur as _);
    }
    cur
}

/**
 * xmlNewDtd:
 * @doc:  the document pointer
 * @name:  the DTD name
 * @ExternalID:  the external ID
 * @SystemID:  the system ID
 *
 * Creation of a new DTD for the external subset. To create an
 * internal subset, use xmlCreateIntSubset().
 *
 * Returns a pointer to the new DTD structure
 */
pub unsafe extern "C" fn xml_new_dtd(
    doc: XmlDocPtr,
    name: *const XmlChar,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) -> XmlDtdPtr {
    if !doc.is_null() && !(*doc).ext_subset.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewDtd(%s): document %s already have a DTD %s\n",
        // 	    /* !!! */  name, (*doc).name,
        // 	    /* !!! */ (*(*doc).extSubset).name);
        // #endif
        return null_mut();
    }

    /*
     * Allocate a new DTD and fill the fields.
     */
    let cur: XmlDtdPtr = xml_malloc(size_of::<XmlDtd>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building DTD".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlDtd>());
    (*cur).typ = XmlElementType::XmlDtdNode;

    if !name.is_null() {
        (*cur).name = xml_strdup(name);
    }
    if !external_id.is_null() {
        (*cur).external_id = xml_strdup(external_id);
    }
    if !system_id.is_null() {
        (*cur).system_id = xml_strdup(system_id);
    }
    if !doc.is_null() {
        (*doc).ext_subset = cur;
    }
    (*cur).doc = doc;

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xmlRegisterNodeDefaultValue(cur as _);
    }
    cur
}

/**
 * xmlGetIntSubset:
 * @doc:  the document pointer
 *
 * Get the internal subset of a document
 * Returns a pointer to the DTD structure or null_mut() if not found
 */
pub unsafe extern "C" fn xml_get_int_subset(doc: *const XmlDoc) -> XmlDtdPtr {
    let mut cur: XmlNodePtr;

    if doc.is_null() {
        return null_mut();
    }
    cur = (*doc).children;
    while !cur.is_null() {
        if matches!((*cur).typ, XmlElementType::XmlDtdNode) {
            return cur as _;
        }
        cur = (*cur).next;
    }
    (*doc).int_subset
}

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
        xmlDeregisterNodeDefaultValue(cur as _);
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
                xml_unlink_node(c);
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
 * xmlNewGlobalNs:
 * @doc:  the document carrying the namespace
 * @href:  the URI associated
 * @prefix:  the prefix for the namespace
 *
 * Creation of a Namespace, the old way using PI and without scoping
 *   DEPRECATED !!!
 * Returns NULL this functionality had been removed
 */
#[deprecated]
#[cfg(feature = "legacy")]
pub unsafe extern "C" fn xmlNewGlobalNs(
    _doc: XmlDocPtr,
    _href: *const XmlChar,
    _prefix: *const XmlChar,
) -> XmlNsPtr {
    use std::sync::atomic::AtomicBool;

    use crate::{libxml::globals::xmlGenericErrorContext, xml_generic_error};

    static DEPRECATED: AtomicBool = AtomicBool::new(false);

    if !DEPRECATED.load(Ordering::Acquire) {
        xml_generic_error!(
            xmlGenericErrorContext(),
            c"xmlNewGlobalNs() deprecated function reached\n".as_ptr() as _
        );
        DEPRECATED.store(true, Ordering::Release);
    }
    null_mut()
}

/**
 * xmlNewNs:
 * @node:  the element carrying the namespace
 * @href:  the URI associated
 * @prefix:  the prefix for the namespace
 *
 * Creation of a new Namespace. This function will refuse to create
 * a namespace with a similar prefix than an existing one present on this
 * node.
 * Note that for a default namespace, @prefix should be null_mut().
 *
 * We use href==null_mut() in the case of an element creation where the namespace
 * was not defined.
 *
 * Returns a new namespace pointer or null_mut()
 */
pub unsafe extern "C" fn xml_new_ns(
    node: XmlNodePtr,
    href: *const XmlChar,
    prefix: *const XmlChar,
) -> XmlNsPtr {
    if !node.is_null() && !matches!((*node).typ, XmlElementType::XmlElementNode) {
        return null_mut();
    }

    if !prefix.is_null() && xml_str_equal(prefix, c"xml".as_ptr() as _) != 0 {
        /* xml namespace is predefined, no need to add it */
        if xml_str_equal(href, XML_XML_NAMESPACE.as_ptr() as _) != 0 {
            return null_mut();
        }

        /*
         * Problem, this is an attempt to bind xml prefix to a wrong
         * namespace, which breaks
         * Namespace constraint: Reserved Prefixes and Namespace Names
         * from XML namespace. But documents authors may not care in
         * their context so let's proceed.
         */
    }

    /*
     * Allocate a new Namespace and fill the fields.
     */
    let cur: XmlNsPtr = xml_malloc(size_of::<XmlNs>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building namespace".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNs>());
    (*cur).typ = Some(XML_LOCAL_NAMESPACE);

    if !href.is_null() {
        (*cur).href = AtomicPtr::new(xml_strdup(href));
    }
    if !prefix.is_null() {
        (*cur).prefix = AtomicPtr::new(xml_strdup(prefix));
    }

    /*
     * Add it at the end to preserve parsing order ...
     * and checks for existing use of the prefix
     */
    if !node.is_null() {
        if (*node).ns_def.is_null() {
            (*node).ns_def = cur;
        } else {
            let mut prev: XmlNsPtr = (*node).ns_def;

            if ((*prev).prefix.load(Ordering::Relaxed).is_null()
                && (*cur).prefix.load(Ordering::Relaxed).is_null())
                || xml_str_equal(
                    (*prev).prefix.load(Ordering::Relaxed),
                    (*cur).prefix.load(Ordering::Relaxed),
                ) != 0
            {
                xml_free_ns(cur);
                return null_mut();
            }
            while !(*prev).next.load(Ordering::Relaxed).is_null() {
                prev = (*prev).next.load(Ordering::Relaxed);
                if ((*prev).prefix.load(Ordering::Relaxed).is_null()
                    && (*cur).prefix.load(Ordering::Relaxed).is_null())
                    || xml_str_equal(
                        (*prev).prefix.load(Ordering::Relaxed),
                        (*cur).prefix.load(Ordering::Relaxed),
                    ) != 0
                {
                    xml_free_ns(cur);
                    return null_mut();
                }
            }
            (*prev).next = AtomicPtr::new(cur);
        }
    }
    cur
}

/**
 * xmlFreeNs:
 * @cur:  the namespace pointer
 *
 * Free up the structures associated to a namespace
 */
pub unsafe extern "C" fn xml_free_ns(cur: XmlNsPtr) {
    if cur.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlFreeNs : ns.is_null()\n".as_ptr() as _);
        // #endif
        return;
    }
    if !(*cur).href.load(Ordering::Relaxed).is_null() {
        xml_free((*cur).href.load(Ordering::Relaxed) as _);
    }
    if !(*cur).prefix.load(Ordering::Relaxed).is_null() {
        xml_free((*cur).prefix.load(Ordering::Relaxed) as _);
    }
    xml_free(cur as _);
}

/**
 * xmlFreeNsList:
 * @cur:  the first namespace pointer
 *
 * Free up all the structures associated to the chained namespaces.
 */
pub unsafe extern "C" fn xml_free_ns_list(mut cur: XmlNsPtr) {
    let mut next: XmlNsPtr;
    if cur.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlFreeNsList : ns.is_null()\n".as_ptr() as _);
        // #endif
        return;
    }
    while !cur.is_null() {
        next = (*cur).next.load(Ordering::Relaxed);
        xml_free_ns(cur);
        cur = next;
    }
}

/**
 * xmlNewDoc:
 * @version:  XmlChar string giving the version of XML "1.0"
 *
 * Creates a new XML document
 *
 * Returns a new document
 */
pub unsafe extern "C" fn xml_new_doc(mut version: *const XmlChar) -> XmlDocPtr {
    if version.is_null() {
        version = c"1.0".as_ptr() as _;
    }

    /*
     * Allocate a new document and fill the fields.
     */
    let cur: XmlDocPtr = xml_malloc(size_of::<XmlDoc>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building doc".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlDoc>());
    (*cur).typ = XmlElementType::XmlDocumentNode;

    (*cur).version = xml_strdup(version) as _;
    if (*cur).version.is_null() {
        xml_tree_err_memory(c"building doc".as_ptr() as _);
        xml_free(cur as _);
        return null_mut();
    }
    (*cur).standalone = -1;
    (*cur).compression = -1; /* not initialized */
    (*cur).doc = cur;
    (*cur).parse_flags = 0;
    (*cur).properties = XmlDocProperties::XmlDocUserbuilt as i32;
    /*
     * The in memory encoding is always UTF8
     * This field will never change and would
     * be obsolete if not for binary compatibility.
     */
    (*cur).charset = XmlCharEncoding::XmlCharEncodingUtf8 as i32;

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xmlRegisterNodeDefaultValue(cur as _);
    }
    cur
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
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlFreeDoc : document.is_null()\n".as_ptr() as _);
        // #endif
        return;
    }

    if !cur.is_null() {
        dict = (*cur).dict;
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    // && xmlDeregisterNodeDefaultValue.is_some()
    {
        xmlDeregisterNodeDefaultValue(cur as _);
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
        xml_unlink_node((*cur).ext_subset as _);
        (*cur).ext_subset = null_mut();
        xml_free_dtd(ext_subset);
    }
    if !int_subset.is_null() {
        xml_unlink_node((*cur).int_subset as _);
        (*cur).int_subset = null_mut();
        xml_free_dtd(int_subset);
    }

    if !(*cur).children.is_null() {
        xml_free_node_list((*cur).children);
    }
    if !(*cur).old_ns.is_null() {
        xml_free_ns_list((*cur).old_ns);
    }

    DICT_FREE!(dict, (*cur).version);
    DICT_FREE!(dict, (*cur).name);
    DICT_FREE!(dict, (*cur).encoding);
    DICT_FREE!(dict, (*cur).url);
    xml_free(cur as _);
    if !dict.is_null() {
        xml_dict_free(dict);
    }
}

/**
 * xmlNewDocProp:
 * @doc:  the document
 * @name:  the name of the attribute
 * @value:  the value of the attribute
 *
 * Create a new property carried by a document.
 * NOTE: @value is supposed to be a piece of XML CDATA, so it allows entity
 *       references, but XML special chars need to be escaped first by using
 *       xmlEncodeEntitiesReentrant(). Use xmlNewProp() if you don't need
 *       entities support.
 *
 * Returns a pointer to the attribute
 */
pub unsafe extern "C" fn xml_new_doc_prop(
    doc: XmlDocPtr,
    name: *const XmlChar,
    value: *const XmlChar,
) -> XmlAttrPtr {
    if name.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewDocProp : name.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    /*
     * Allocate a new property and fill the fields.
     */
    let cur: XmlAttrPtr = xml_malloc(size_of::<XmlAttr>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building attribute".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlAttr>());
    (*cur).typ = XmlElementType::XmlAttributeNode;

    if !doc.is_null() && !(*doc).dict.is_null() {
        (*cur).name = xml_dict_lookup((*doc).dict, name, -1);
    } else {
        (*cur).name = xml_strdup(name);
    }
    (*cur).doc = doc;
    if !value.is_null() {
        let mut tmp: XmlNodePtr;

        (*cur).children = xml_string_get_node_list(doc, value);
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

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xmlRegisterNodeDefaultValue(cur as _);
    }
    cur
}

unsafe extern "C" fn xml_new_prop_internal(
    node: XmlNodePtr,
    ns: XmlNsPtr,
    name: *const XmlChar,
    value: *const XmlChar,
    eatname: c_int,
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
        xmlRegisterNodeDefaultValue(cur as _);
    }
    cur
}

/**
 * xmlNewProp:
 * @node:  the holding node
 * @name:  the name of the attribute
 * @value:  the value of the attribute
 *
 * Create a new property carried by a node.
 * Returns a pointer to the attribute
 */
#[cfg(any(feature = "tree", feature = "html", feature = "schema"))]
pub unsafe extern "C" fn xml_new_prop(
    node: XmlNodePtr,
    name: *const XmlChar,
    value: *const XmlChar,
) -> XmlAttrPtr {
    if name.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewProp : name.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    xml_new_prop_internal(node, null_mut(), name, value, 0)
}

/**
 * xmlNewNsProp:
 * @node:  the holding node
 * @ns:  the namespace
 * @name:  the name of the attribute
 * @value:  the value of the attribute
 *
 * Create a new property tagged with a namespace and carried by a node.
 * Returns a pointer to the attribute
 */
pub unsafe extern "C" fn xml_new_ns_prop(
    node: XmlNodePtr,
    ns: XmlNsPtr,
    name: *const XmlChar,
    value: *const XmlChar,
) -> XmlAttrPtr {
    if name.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewNsProp : name.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    xml_new_prop_internal(node, ns, name, value, 0)
}

/**
 * xmlNewNsPropEatName:
 * @node:  the holding node
 * @ns:  the namespace
 * @name:  the name of the attribute
 * @value:  the value of the attribute
 *
 * Create a new property tagged with a namespace and carried by a node.
 * Returns a pointer to the attribute
 */
pub unsafe extern "C" fn xml_new_ns_prop_eat_name(
    node: XmlNodePtr,
    ns: XmlNsPtr,
    name: *mut XmlChar,
    value: *const XmlChar,
) -> XmlAttrPtr {
    if name.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewNsPropEatName : name.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    xml_new_prop_internal(node, ns, name, value, 1)
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
        xmlDeregisterNodeDefaultValue(cur as _);
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
    let mut counter: c_int = 1;

    if tree.is_null() || !matches!((*tree).typ, XmlElementType::XmlElementNode) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewReconciledNs : tree.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }
    if ns.is_null() || !matches!((*ns).typ, Some(XmlElementType::XmlNamespaceDecl)) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewReconciledNs : ns.is_null()\n".as_ptr() as _);
        // #endif
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
    extended: c_int,
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
            xmlRegisterNodeDefaultValue(ret as _);
        }

        /*
         * Note that since (*ret).parent is already set, xmlAddChild will
         * return early and not actually insert the node. It will only
         * coalesce text nodes and unnecessarily call xmlSetTreeDoc.
         * Assuming that the subtree to be copied always has its text
         * nodes coalesced, the somewhat confusing call to xmlAddChild
         * could be removed.
         */
        let tmp: XmlNodePtr = xml_add_child(parent, ret);
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
            xmlRegisterNodeDefaultValue(ret as _);
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
        xmlRegisterNodeDefaultValue(ret as _);
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
                    xml_add_child(parent, q);
                } else {
                    q = (*doc).int_subset as _;
                    xml_add_child(parent, q);
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
            ) != 0
            {
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
 * xmlGetEntityFromDtd:
 * @dtd:  A pointer to the DTD to search
 * @name:  The entity name
 *
 * Do an entity lookup in the DTD entity hash table and
 * return the corresponding entity, if found.
 *
 * Returns A pointer to the entity structure or null_mut() if not found.
 */
#[cfg(feature = "tree")]
unsafe extern "C" fn xml_get_entity_from_dtd(
    dtd: *const XmlDtd,
    name: *const XmlChar,
) -> XmlEntityPtr {
    let table: XmlEntitiesTablePtr;

    if !dtd.is_null() && !(*dtd).entities.is_null() {
        table = (*dtd).entities as _;
        return xmlHashLookup(table, name) as _;
        /* return(xmlGetEntityFromTable(table, name)); */
    }
    null_mut()
}

/**
 * xmlGetParameterEntityFromDtd:
 * @dtd:  A pointer to the DTD to search
 * @name:  The entity name
 *
 * Do an entity lookup in the DTD parameter entity hash table and
 * return the corresponding entity, if found.
 *
 * Returns A pointer to the entity structure or null_mut() if not found.
 */
#[cfg(feature = "tree")]
unsafe extern "C" fn xml_get_parameter_entity_from_dtd(
    dtd: *const XmlDtd,
    name: *const XmlChar,
) -> XmlEntityPtr {
    let table: XmlEntitiesTablePtr;

    if !dtd.is_null() && !(*dtd).pentities.is_null() {
        table = (*dtd).pentities as _;
        return xmlHashLookup(table, name) as _;
        /* return(xmlGetEntityFromTable(table, name)); */
    }
    null_mut()
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
                    q = xml_get_entity_from_dtd(ret, (*tmp).name.load(Ordering::Relaxed)) as _;
                }
                Some(XmlEntityType::XmlInternalParameterEntity)
                | Some(XmlEntityType::XmlExternalParameterEntity) => {
                    q = xml_get_parameter_entity_from_dtd(ret, (*tmp).name.load(Ordering::Relaxed))
                        as _;
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
pub unsafe extern "C" fn xml_copy_doc(doc: XmlDocPtr, recursive: c_int) -> XmlDocPtr {
    use super::globals::xml_mem_strdup;

    if doc.is_null() {
        return null_mut();
    }
    let ret: XmlDocPtr = xml_new_doc((*doc).version);
    if ret.is_null() {
        return null_mut();
    }
    (*ret).typ = (*doc).typ;
    if !(*doc).name.is_null() {
        (*ret).name = xml_mem_strdup((*doc).name as _) as _;
    }
    if !(*doc).encoding.is_null() {
        (*ret).encoding = xml_strdup((*doc).encoding);
    }
    if !(*doc).url.is_null() {
        (*ret).url = xml_strdup((*doc).url);
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
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewNode : name.is_null()\n".as_ptr() as _);
        // #endif
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
        xmlRegisterNodeDefaultValue(cur as _);
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
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewNode : name.is_null()\n".as_ptr() as _);
        // #endif
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
        xmlRegisterNodeDefaultValue(cur as _);
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
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewChild : parent.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    if name.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewChild : name.is_null()\n".as_ptr() as _);
        // #endif
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
        xmlRegisterNodeDefaultValue(cur as _);
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
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewPI : name.is_null()\n".as_ptr() as _);
        // #endif
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
        xmlRegisterNodeDefaultValue(cur as _);
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
    len: c_int,
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
pub unsafe extern "C" fn xml_new_text_len(content: *const XmlChar, len: c_int) -> XmlNodePtr {
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
        xmlRegisterNodeDefaultValue(cur as _);
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
        xmlRegisterNodeDefaultValue(cur as _);
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
    len: c_int,
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
        xmlRegisterNodeDefaultValue(cur as _);
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
        let len: c_int = xml_strlen(name);
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
        xmlRegisterNodeDefaultValue(cur as _);
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
        let len: c_int = xml_strlen(name);
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
        xmlRegisterNodeDefaultValue(cur as _);
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
pub unsafe extern "C" fn xml_copy_node(node: XmlNodePtr, extended: c_int) -> XmlNodePtr {
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
    extended: c_int,
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
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewTextChild : parent.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    if name.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNewTextChild : name.is_null()\n".as_ptr() as _);
        // #endif
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
        xmlRegisterNodeDefaultValue(cur as _);
    }
    cur
}

/**
 * xmlGetLineNoInternal:
 * @node: valid node
 * @depth: used to limit any risk of recursion
 *
 * Get line number of @node.
 * Try to override the limitation of lines being store in 16 bits ints
 *
 * Returns the line number if successful, -1 otherwise
 */
unsafe extern "C" fn xml_get_line_no_internal(node: *const XmlNode, depth: c_int) -> c_long {
    let mut result: c_long = -1;

    if depth >= 5 {
        return -1;
    }

    if node.is_null() {
        return result;
    }
    if (matches!((*node).typ, XmlElementType::XmlElementNode)
        || matches!((*node).typ, XmlElementType::XmlTextNode)
        || matches!((*node).typ, XmlElementType::XmlCommentNode)
        || matches!((*node).typ, XmlElementType::XmlPiNode))
    {
        if (*node).line == 65535 {
            if (matches!((*node).typ, XmlElementType::XmlTextNode) && !(*node).psvi.is_null()) {
                result = (*node).psvi as ptrdiff_t as c_long;
            } else if (matches!((*node).typ, XmlElementType::XmlElementNode)
                && !(*node).children.is_null())
            {
                result = xml_get_line_no_internal((*node).children, depth + 1);
            } else if !(*node).next.is_null() {
                result = xml_get_line_no_internal((*node).next, depth + 1);
            } else if !(*node).prev.is_null() {
                result = xml_get_line_no_internal((*node).prev, depth + 1);
            }
        }
        if (result == -1) || (result == 65535) {
            result = (*node).line as c_long;
        }
    } else if !(*node).prev.is_null()
        && (matches!((*(*node).prev).typ, XmlElementType::XmlElementNode)
            || matches!((*(*node).prev).typ, XmlElementType::XmlTextNode)
            || matches!((*(*node).prev).typ, XmlElementType::XmlCommentNode)
            || matches!((*(*node).prev).typ, XmlElementType::XmlPiNode))
    {
        result = xml_get_line_no_internal((*node).prev, depth + 1);
    } else if !(*node).parent.is_null()
        && matches!((*(*node).parent).typ, XmlElementType::XmlElementNode)
    {
        result = xml_get_line_no_internal((*node).parent, depth + 1);
    }

    result
}

/*
 * Navigating.
 */
/**
 * xmlGetLineNo:
 * @node: valid node
 *
 * Get line number of @node.
 * Try to override the limitation of lines being store in 16 bits ints
 * if XML_PARSE_BIG_LINES parser option was used
 *
 * Returns the line number if successful, -1 otherwise
 */
pub unsafe extern "C" fn xml_get_line_no(node: *const XmlNode) -> c_long {
    xml_get_line_no_internal(node, 0)
}

/**
 * xmlGetNodePath:
 * @node: a node
 *
 * Build a structure based Path for the given node
 *
 * Returns the new path or null_mut() in case of error. The caller must free
 *     the returned string
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_get_node_path(node: *const XmlNode) -> *mut XmlChar {
    let mut cur: *const XmlNode;
    let mut tmp: *const XmlNode;
    let mut next: *const XmlNode;
    let mut buffer: *mut XmlChar;
    let mut temp: *mut XmlChar;
    let mut buf_len: size_t;
    let mut buf: *mut XmlChar;
    let mut sep: *const c_char;
    let mut name: *const c_char;
    let mut nametemp: [c_char; 100] = [0; 100];
    let mut occur: c_int;
    let mut generic: c_int;

    if node.is_null() || matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }

    buf_len = 500;
    buffer = xml_malloc_atomic(buf_len) as _;
    if buffer.is_null() {
        xml_tree_err_memory(c"getting node path".as_ptr() as _);
        return null_mut();
    }
    buf = xml_malloc_atomic(buf_len) as _;
    if buf.is_null() {
        xml_tree_err_memory(c"getting node path".as_ptr() as _);
        xml_free(buffer as _);
        return null_mut();
    }

    *buffer.add(0) = 0;
    cur = node;
    loop {
        name = c"".as_ptr() as _;
        // sep = c"?".as_ptr() as _;
        occur = 0;
        if (matches!((*cur).typ, XmlElementType::XmlDocumentNode)
            || matches!((*cur).typ, XmlElementType::XmlHtmlDocumentNode))
        {
            if *buffer.add(0) == b'/' {
                break;
            }
            sep = c"/".as_ptr() as _;
            next = null_mut();
        } else if matches!((*cur).typ, XmlElementType::XmlElementNode) {
            generic = 0;
            sep = c"/".as_ptr() as _;
            name = (*cur).name as _;
            if !(*cur).ns.is_null() {
                if !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                    snprintf(
                        nametemp.as_mut_ptr() as _,
                        nametemp.len() - 1,
                        c"%s:%s".as_ptr() as _,
                        (*(*cur).ns).prefix.load(Ordering::Relaxed) as *const c_char,
                        (*cur).name,
                    );
                    *nametemp.last_mut().unwrap() = 0;
                    name = nametemp.as_ptr() as _;
                } else {
                    /*
                     * We cannot express named elements in the default
                     * namespace, so use "*".
                     */
                    generic = 1;
                    name = c"*".as_ptr() as _;
                }
            }
            next = (*cur).parent;

            /*
             * Thumbler index computation
             * TODO: the occurrence test seems bogus for namespaced names
             */
            tmp = (*cur).prev;
            while !tmp.is_null() {
                if matches!((*tmp).typ, XmlElementType::XmlElementNode)
                    && (generic != 0
                        || (xml_str_equal((*cur).name, (*tmp).name) != 0
                            && ((*tmp).ns == (*cur).ns
                                || (!(*tmp).ns.is_null()
                                    && !(*cur).ns.is_null()
                                    && xml_str_equal(
                                        (*(*cur).ns).prefix.load(Ordering::Relaxed),
                                        (*(*tmp).ns).prefix.load(Ordering::Relaxed),
                                    ) != 0))))
                {
                    occur += 1;
                }
                tmp = (*tmp).prev;
            }
            if occur == 0 {
                tmp = (*cur).next;
                while !tmp.is_null() && occur == 0 {
                    if matches!((*tmp).typ, XmlElementType::XmlElementNode)
                        && (generic != 0
                            || (xml_str_equal((*cur).name, (*tmp).name) != 0
                                && (((*tmp).ns == (*cur).ns)
                                    || (!(*tmp).ns.is_null()
                                        && !(*cur).ns.is_null()
                                        && (xml_str_equal(
                                            (*(*cur).ns).prefix.load(Ordering::Relaxed),
                                            (*(*tmp).ns).prefix.load(Ordering::Relaxed),
                                        ) != 0)))))
                    {
                        occur += 1;
                    }
                    tmp = (*tmp).next;
                }
                if occur != 0 {
                    occur = 1;
                }
            } else {
                occur += 1;
            }
        } else if matches!((*cur).typ, XmlElementType::XmlCommentNode) {
            sep = c"/".as_ptr() as _;
            name = c"comment()".as_ptr() as _;
            next = (*cur).parent;

            /*
             * Thumbler index computation
             */
            tmp = (*cur).prev;
            while !tmp.is_null() {
                if matches!((*tmp).typ, XmlElementType::XmlCommentNode) {
                    occur += 1;
                }
                tmp = (*tmp).prev;
            }
            if occur == 0 {
                tmp = (*cur).next;
                while !tmp.is_null() && occur == 0 {
                    if matches!((*tmp).typ, XmlElementType::XmlCommentNode) {
                        occur += 1;
                    }
                    tmp = (*tmp).next;
                }
                if occur != 0 {
                    occur = 1;
                }
            } else {
                occur += 1;
            }
        } else if (matches!((*cur).typ, XmlElementType::XmlTextNode)
            || matches!((*cur).typ, XmlElementType::XmlCdataSectionNode))
        {
            sep = c"/".as_ptr() as _;
            name = c"text()".as_ptr() as _;
            next = (*cur).parent;

            /*
             * Thumbler index computation
             */
            tmp = (*cur).prev;
            while !tmp.is_null() {
                if (matches!((*tmp).typ, XmlElementType::XmlTextNode)
                    || matches!((*tmp).typ, XmlElementType::XmlCdataSectionNode))
                {
                    occur += 1;
                }
                tmp = (*tmp).prev;
            }
            /*
             * Evaluate if this is the only text- or CDATA-section-node;
             * if yes, then we'll get "text()".as_ptr() as _, otherwise "text()[1]".
             */
            if occur == 0 {
                tmp = (*cur).next;
                while !tmp.is_null() {
                    if (matches!((*tmp).typ, XmlElementType::XmlTextNode)
                        || matches!((*tmp).typ, XmlElementType::XmlCdataSectionNode))
                    {
                        occur = 1;
                        break;
                    }
                    tmp = (*tmp).next;
                }
            } else {
                occur += 1;
            }
        } else if matches!((*cur).typ, XmlElementType::XmlPiNode) {
            sep = c"/".as_ptr() as _;
            snprintf(
                nametemp.as_mut_ptr() as _,
                nametemp.len() - 1,
                c"processing-instruction('%s')".as_ptr() as _,
                (*cur).name,
            );
            *nametemp.last_mut().unwrap() = 0;
            name = nametemp.as_ptr() as _;

            next = (*cur).parent;

            /*
             * Thumbler index computation
             */
            tmp = (*cur).prev;
            while !tmp.is_null() {
                if matches!((*tmp).typ, XmlElementType::XmlPiNode)
                    && xml_str_equal((*cur).name, (*tmp).name) != 0
                {
                    occur += 1;
                }
                tmp = (*tmp).prev;
            }
            if occur == 0 {
                tmp = (*cur).next;
                while !tmp.is_null() && occur == 0 {
                    if matches!((*tmp).typ, XmlElementType::XmlPiNode)
                        && xml_str_equal((*cur).name, (*tmp).name) != 0
                    {
                        occur += 1;
                    }
                    tmp = (*tmp).next;
                }
                if occur != 0 {
                    occur = 1;
                }
            } else {
                occur += 1;
            }
        } else if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
            sep = c"/@".as_ptr() as _;
            name = (*(cur as XmlAttrPtr)).name as _;
            if !(*cur).ns.is_null() {
                if !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                    snprintf(
                        nametemp.as_mut_ptr() as _,
                        nametemp.len() - 1,
                        c"%s:%s".as_ptr() as _,
                        (*(*cur).ns).prefix.load(Ordering::Relaxed) as *const c_char,
                        (*cur).name,
                    );
                } else {
                    snprintf(
                        nametemp.as_mut_ptr() as _,
                        nametemp.len() - 1,
                        c"%s".as_ptr() as _,
                        (*cur).name,
                    );
                }
                *nametemp.last_mut().unwrap() = 0;
                name = nametemp.as_ptr() as _;
            }
            next = (*(cur as XmlAttrPtr)).parent;
        } else {
            xml_free(buf as _);
            xml_free(buffer as _);
            return null_mut();
        }

        /*
         * Make sure there is enough room
         */
        if xml_strlen(buffer) as usize + nametemp.len() + 20 > buf_len {
            buf_len = 2 * buf_len + xml_strlen(buffer) as usize + nametemp.len() + 20;
            temp = xml_realloc(buffer as _, buf_len) as _;
            if temp.is_null() {
                xml_tree_err_memory(c"getting node path".as_ptr() as _);
                xml_free(buf as _);
                xml_free(buffer as _);
                return null_mut();
            }
            buffer = temp;
            temp = xml_realloc(buf as _, buf_len) as _;
            if temp.is_null() {
                xml_tree_err_memory(c"getting node path".as_ptr() as _);
                xml_free(buf as _);
                xml_free(buffer as _);
                return null_mut();
            }
            buf = temp;
        }
        if occur == 0 {
            snprintf(
                buf as _,
                buf_len,
                c"%s%s%s".as_ptr() as _,
                sep,
                name,
                buffer,
            );
        } else {
            snprintf(
                buf as _,
                buf_len,
                c"%s%s[%d]%s".as_ptr() as _,
                sep,
                name,
                occur,
                buffer,
            );
        }
        snprintf(buffer as _, buf_len, c"%s".as_ptr() as _, buf);
        cur = next;

        if cur.is_null() {
            break;
        }
    }
    xml_free(buf as _);
    buffer
}

/**
 * xmlDocGetRootElement:
 * @doc:  the document
 *
 * Get the root element of the document ((*doc).children is a list
 * containing possibly comments, PIs, etc ...).
 *
 * Returns the #XmlNodePtr for the root or null_mut()
 */
pub unsafe extern "C" fn xml_doc_get_root_element(doc: *const XmlDoc) -> XmlNodePtr {
    let mut ret: XmlNodePtr;

    if doc.is_null() {
        return null_mut();
    }
    ret = (*doc).children;
    while !ret.is_null() {
        if matches!((*ret).typ, XmlElementType::XmlElementNode) {
            return ret;
        }
        ret = (*ret).next;
    }
    ret
}

/**
 * xmlGetLastChild:
 * @parent:  the parent node
 *
 * Search the last child of a node.
 * Returns the last child or null_mut() if none.
 */
pub unsafe extern "C" fn xml_get_last_child(parent: *const XmlNode) -> XmlNodePtr {
    if parent.is_null() || matches!((*parent).typ, XmlElementType::XmlNamespaceDecl) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlGetLastChild : parent.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }
    (*parent).last
}

/**
 * xmlNodeIsText:
 * @node:  the node
 *
 * Is this node a Text node ?
 * Returns 1 yes, 0 no
 */
pub unsafe extern "C" fn xml_node_is_text(node: *const XmlNode) -> c_int {
    if node.is_null() {
        return 0;
    }

    if matches!((*node).typ, XmlElementType::XmlTextNode) {
        return 1;
    }
    0
}

/**
 * xmlIsBlankNode:
 * @node:  the node
 *
 * Checks whether this node is an empty or whitespace only
 * (and possibly ignorable) text-node.
 *
 * Returns 1 yes, 0 no
 */
pub unsafe extern "C" fn xml_is_blank_node(node: *const XmlNode) -> c_int {
    let mut cur: *const XmlChar;
    if node.is_null() {
        return 0;
    }

    if !matches!((*node).typ, XmlElementType::XmlTextNode)
        && !matches!((*node).typ, XmlElementType::XmlCdataSectionNode)
    {
        return 0;
    }
    if (*node).content.is_null() {
        return 1;
    }
    cur = (*node).content;
    while *cur != 0 {
        if !IS_BLANK_CH!(*cur) {
            return 0;
        }
        cur = cur.add(1);
    }

    1
}

/*
 * Changing the structure.
 */
/**
 * xmlDocSetRootElement:
 * @doc:  the document
 * @root:  the new document root element, if root is null_mut() no action is taken,
 *         to remove a node from a document use xmlUnlinkNode(root) instead.
 *
 * Set the root element of the document ((*doc).children is a list
 * containing possibly comments, PIs, etc ...).
 *
 * Returns the old root element if any was found, null_mut() if root was null_mut()
 */
#[cfg(any(feature = "tree", feature = "writer"))]
pub unsafe extern "C" fn xml_doc_set_root_element(doc: XmlDocPtr, root: XmlNodePtr) -> XmlNodePtr {
    let mut old: XmlNodePtr;

    if doc.is_null() {
        return null_mut();
    }
    if root.is_null() || matches!((*root).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    xml_unlink_node(root);
    xml_set_tree_doc(root, doc);
    (*root).parent = doc as _;
    old = (*doc).children;
    while !old.is_null() {
        if matches!((*old).typ, XmlElementType::XmlElementNode) {
            break;
        }
        old = (*old).next;
    }
    if old.is_null() {
        if (*doc).children.is_null() {
            (*doc).children = root;
            (*doc).last = root;
        } else {
            xml_add_sibling((*doc).children, root);
        }
    } else {
        xml_replace_node(old, root);
    }
    old
}

/**
 * xmlNodeSetName:
 * @cur:  the node being changed
 * @name:  the new tag name
 *
 * Set (or reset) the name of a node.
 */
#[cfg(feature = "tree")]
pub unsafe extern "C" fn xml_node_set_name(cur: XmlNodePtr, name: *const XmlChar) {
    let mut freeme: *const XmlChar = null_mut();

    if cur.is_null() {
        return;
    }
    if name.is_null() {
        return;
    }
    match (*cur).typ {
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlHtmlDocumentNode
        | XmlElementType::XmlNamespaceDecl
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => {
            return;
        }
        XmlElementType::XmlElementNode
        | XmlElementType::XmlAttributeNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlDocumentNode
        | XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl => {}
        _ => unreachable!(),
    }
    let doc: XmlDocPtr = (*cur).doc;
    let dict = if !doc.is_null() {
        (*doc).dict
    } else {
        null_mut()
    };
    if !dict.is_null() {
        if !(*cur).name.is_null() && xml_dict_owns(dict, (*cur).name) == 0 {
            freeme = (*cur).name;
        }
        (*cur).name = xml_dict_lookup(dict, name, -1);
    } else {
        if !(*cur).name.is_null() {
            freeme = (*cur).name;
        }
        (*cur).name = xml_strdup(name);
    }

    if !freeme.is_null() {
        xml_free(freeme as _);
    }
}

/**
 * xmlAddChild:
 * @parent:  the parent node
 * @cur:  the child node
 *
 * Add a new node to @parent, at the end of the child (or property) list
 * merging adjacent TEXT nodes (in which case @cur is freed)
 * If the new node is ATTRIBUTE, it is added into properties instead of children.
 * If there is an attribute with equal name, it is first destroyed.
 *
 * All tree manipulation functions can safely move nodes within a document.
 * But when moving nodes from one document to another, references to
 * namespaces in element or attribute nodes are NOT fixed. In this case,
 * you MUST call xmlReconciliateNs after the move operation to avoid
 * memory errors.
 *
 * Returns the child or null_mut() in case of error.
 */
pub unsafe extern "C" fn xml_add_child(parent: XmlNodePtr, cur: XmlNodePtr) -> XmlNodePtr {
    let mut prev: XmlNodePtr;

    if parent.is_null() || matches!((*parent).typ, XmlElementType::XmlNamespaceDecl) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddChild : parent.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    if cur.is_null() || matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddChild : child.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    if parent == cur {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddChild : parent == cur\n".as_ptr() as _);
        // #endif
        return null_mut();
    }
    /*
     * If cur is a TEXT node, merge its content with adjacent TEXT nodes
     * cur is then freed.
     */
    if matches!((*cur).typ, XmlElementType::XmlTextNode) {
        if matches!((*parent).typ, XmlElementType::XmlTextNode)
            && !(*parent).content.is_null()
            && (*parent).name == (*cur).name
        {
            xml_node_add_content(parent, (*cur).content);
            xml_free_node(cur);
            return parent;
        }
        if !(*parent).last.is_null()
            && matches!((*(*parent).last).typ, XmlElementType::XmlTextNode)
            && ((*(*parent).last).name == (*cur).name)
            && ((*parent).last != cur)
        {
            xml_node_add_content((*parent).last, (*cur).content);
            xml_free_node(cur);
            return (*parent).last;
        }
    }

    /*
     * add the new element at the end of the children list.
     */
    prev = (*cur).parent;
    (*cur).parent = parent;
    if (*cur).doc != (*parent).doc {
        xml_set_tree_doc(cur, (*parent).doc);
    }
    /* this check prevents a loop on tree-traversions if a developer
     * tries to add a node to its parent multiple times
     */
    if prev == parent {
        return cur;
    }

    /*
     * Coalescing
     */
    if matches!((*parent).typ, XmlElementType::XmlTextNode)
        && !(*parent).content.is_null()
        && parent != cur
    {
        xml_node_add_content(parent, (*cur).content);
        xml_free_node(cur);
        return parent;
    }
    if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
        if !matches!((*parent).typ, XmlElementType::XmlElementNode) {
            return null_mut();
        }
        if !(*parent).properties.is_null() {
            /* check if an attribute with the same name exists */

            let lastattr = if (*cur).ns.is_null() {
                xml_has_ns_prop(parent, (*cur).name, null_mut())
            } else {
                xml_has_ns_prop(
                    parent,
                    (*cur).name,
                    (*(*cur).ns).href.load(Ordering::Relaxed),
                )
            };
            if !lastattr.is_null()
                && lastattr != cur as _
                && !matches!((*lastattr).typ, XmlElementType::XmlAttributeDecl)
            {
                /* different instance, destroy it (attributes must be unique) */
                xml_unlink_node(lastattr as _);
                xml_free_prop(lastattr);
            }
            if lastattr == cur as _ {
                return cur;
            }
        }
        if (*parent).properties.is_null() {
            (*parent).properties = cur as _;
        } else {
            /* find the end */
            let mut lastattr: XmlAttrPtr = (*parent).properties;
            while !(*lastattr).next.is_null() {
                lastattr = (*lastattr).next;
            }
            (*lastattr).next = cur as _;
            (*(cur as XmlAttrPtr)).prev = lastattr;
        }
    } else if (*parent).children.is_null() {
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
 * xmlAddChildList:
 * @parent:  the parent node
 * @cur:  the first node in the list
 *
 * Add a list of node at the end of the child list of the parent
 * merging adjacent TEXT nodes (@cur may be freed)
 *
 * See the note regarding namespaces in xmlAddChild.
 *
 * Returns the last child or null_mut() in case of error.
 */
pub unsafe extern "C" fn xml_add_child_list(parent: XmlNodePtr, mut cur: XmlNodePtr) -> XmlNodePtr {
    let mut prev: XmlNodePtr;

    if parent.is_null() || matches!((*parent).typ, XmlElementType::XmlNamespaceDecl) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddChildList : parent.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    if cur.is_null() || matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddChildList : child.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    if !(*cur).doc.is_null() && !(*parent).doc.is_null() && (*cur).doc != (*parent).doc {
        // #ifdef DEBUG_TREE
        // 	xmlGenericError(xmlGenericErrorContext,
        // 		c"Elements moved to a different document\n".as_ptr() as _);
        // #endif
    }

    /*
     * add the first element at the end of the children list.
     */

    if (*parent).children.is_null() {
        (*parent).children = cur;
    } else {
        /*
         * If cur and (*parent).last both are TEXT nodes, then merge them.
         */
        if matches!((*cur).typ, XmlElementType::XmlTextNode)
            && matches!((*(*parent).last).typ, XmlElementType::XmlTextNode)
            && ((*cur).name == (*(*parent).last).name)
        {
            xml_node_add_content((*parent).last, (*cur).content);
            /*
             * if it's the only child, nothing more to be done.
             */
            if (*cur).next.is_null() {
                xml_free_node(cur);
                return (*parent).last;
            }
            prev = cur;
            cur = (*cur).next;
            xml_free_node(prev);
        }
        prev = (*parent).last;
        (*prev).next = cur;
        (*cur).prev = prev;
    }
    while !(*cur).next.is_null() {
        (*cur).parent = parent;
        if (*cur).doc != (*parent).doc {
            xml_set_tree_doc(cur, (*parent).doc);
        }
        cur = (*cur).next;
    }
    (*cur).parent = parent;
    /* the parent may not be linked to a doc ! */
    if (*cur).doc != (*parent).doc {
        xml_set_tree_doc(cur, (*parent).doc);
    }
    (*parent).last = cur;

    cur
}

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
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlReplaceNode : old.is_null() or without parent\n".as_ptr() as _);
        // #endif
        return null_mut();
    }
    if cur.is_null() || matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        xml_unlink_node(old);
        return old;
    }
    if cur == old {
        return old;
    }
    if (matches!((*old).typ, XmlElementType::XmlAttributeNode)
        && !matches!((*cur).typ, XmlElementType::XmlAttributeNode))
    {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlReplaceNode : Trying to replace attribute node with other node type\n".as_ptr() as _);
        // #endif
        return old;
    }
    if (matches!((*cur).typ, XmlElementType::XmlAttributeNode)
        && !matches!((*old).typ, XmlElementType::XmlAttributeNode))
    {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlReplaceNode : Trying to replace a non-attribute node with attribute node\n".as_ptr() as _);
        // #endif
        return old;
    }
    xml_unlink_node(cur);
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
 * xmlAddPropSibling:
 * @prev:  the attribute to which @prop is added after
 * @cur:   the base attribute passed to calling function
 * @prop:  the new attribute
 *
 * Add a new attribute after @prev using @cur as base attribute.
 * When inserting before @cur, @prev is passed as @(*cur).prev.
 * When inserting after @cur, @prev is passed as @cur.
 * If an existing attribute is found it is destroyed prior to adding @prop.
 *
 * See the note regarding namespaces in xmlAddChild.
 *
 * Returns the attribute being inserted or null_mut() in case of error.
 */
unsafe extern "C" fn xml_add_prop_sibling(
    prev: XmlNodePtr,
    cur: XmlNodePtr,
    prop: XmlNodePtr,
) -> XmlNodePtr {
    if cur.is_null()
        || !matches!((*cur).typ, XmlElementType::XmlAttributeNode)
        || prop.is_null()
        || !matches!((*prop).typ, XmlElementType::XmlAttributeNode)
        || (!prev.is_null() && !matches!((*prev).typ, XmlElementType::XmlAttributeNode))
    {
        return null_mut();
    }

    /* check if an attribute with the same name exists */
    let attr = if (*prop).ns.is_null() {
        xml_has_ns_prop((*cur).parent, (*prop).name, null_mut())
    } else {
        xml_has_ns_prop(
            (*cur).parent,
            (*prop).name,
            (*(*prop).ns).href.load(Ordering::Relaxed),
        )
    };

    if (*prop).doc != (*cur).doc {
        xml_set_tree_doc(prop, (*cur).doc);
    }
    (*prop).parent = (*cur).parent;
    (*prop).prev = prev;
    if !prev.is_null() {
        (*prop).next = (*prev).next;
        (*prev).next = prop;
        if !(*prop).next.is_null() {
            (*(*prop).next).prev = prop;
        }
    } else {
        (*prop).next = cur;
        (*cur).prev = prop;
    }
    if (*prop).prev.is_null() && !(*prop).parent.is_null() {
        (*(*prop).parent).properties = prop as _;
    }
    if !attr.is_null() && ((*attr).typ != XmlElementType::XmlAttributeDecl) {
        /* different instance, destroy it (attributes must be unique) */
        xml_remove_prop(attr);
    }
    prop
}

/**
 * xmlAddPrevSibling:
 * @cur:  the child node
 * @elem:  the new node
 *
 * Add a new node @elem as the previous sibling of @cur
 * merging adjacent TEXT nodes (@elem may be freed)
 * If the new node was already inserted in a document it is
 * first unlinked from its existing context.
 * If the new node is ATTRIBUTE, it is added into properties instead of children.
 * If there is an attribute with equal name, it is first destroyed.
 *
 * See the note regarding namespaces in xmlAddChild.
 *
 * Returns the new node or null_mut() in case of error.
 */
#[cfg(any(
    feature = "tree",
    feature = "html",
    feature = "schema",
    feature = "xinclude"
))]
pub unsafe extern "C" fn xml_add_prev_sibling(cur: XmlNodePtr, elem: XmlNodePtr) -> XmlNodePtr {
    if cur.is_null() || matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddPrevSibling : cur.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }
    if elem.is_null() || ((*elem).typ == XmlElementType::XmlNamespaceDecl) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddPrevSibling : elem.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    if cur == elem {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddPrevSibling : cur == elem\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    xml_unlink_node(elem);

    if matches!((*elem).typ, XmlElementType::XmlTextNode) {
        if matches!((*cur).typ, XmlElementType::XmlTextNode) {
            let mut tmp: *mut XmlChar;

            tmp = xml_strdup((*elem).content);
            tmp = xml_strcat(tmp, (*cur).content);
            xml_node_set_content(cur, tmp);
            xml_free(tmp as _);
            xml_free_node(elem);
            return cur;
        }
        if !(*cur).prev.is_null()
            && matches!((*(*cur).prev).typ, XmlElementType::XmlTextNode)
            && ((*cur).name == (*(*cur).prev).name)
        {
            xml_node_add_content((*cur).prev, (*elem).content);
            xml_free_node(elem);
            return (*cur).prev;
        }
    } else if matches!((*elem).typ, XmlElementType::XmlAttributeNode) {
        return xml_add_prop_sibling((*cur).prev, cur, elem);
    }

    if (*elem).doc != (*cur).doc {
        xml_set_tree_doc(elem, (*cur).doc);
    }
    (*elem).parent = (*cur).parent;
    (*elem).next = cur;
    (*elem).prev = (*cur).prev;
    (*cur).prev = elem;
    if !(*elem).prev.is_null() {
        (*(*elem).prev).next = elem;
    }
    if !(*elem).parent.is_null() && (*(*elem).parent).children == cur {
        (*(*elem).parent).children = elem;
    }
    elem
}

/**
 * xmlAddSibling:
 * @cur:  the child node
 * @elem:  the new node
 *
 * Add a new element @elem to the list of siblings of @cur
 * merging adjacent TEXT nodes (@elem may be freed)
 * If the new element was already inserted in a document it is
 * first unlinked from its existing context.
 *
 * See the note regarding namespaces in xmlAddChild.
 *
 * Returns the new element or null_mut() in case of error.
 */
pub unsafe extern "C" fn xml_add_sibling(mut cur: XmlNodePtr, elem: XmlNodePtr) -> XmlNodePtr {
    if cur.is_null() || matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddSibling : cur.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    if elem.is_null() || ((*elem).typ == XmlElementType::XmlNamespaceDecl) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddSibling : elem.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    if cur == elem {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddSibling : cur == elem\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    /*
     * Constant time is we can rely on the ->(*parent).last to find
     * the last sibling.
     */
    if !matches!((*cur).typ, XmlElementType::XmlAttributeNode)
        && !(*cur).parent.is_null()
        && !(*(*cur).parent).children.is_null()
        && !(*(*cur).parent).last.is_null()
        && (*(*(*cur).parent).last).next.is_null()
    {
        cur = (*(*cur).parent).last;
    } else {
        while !(*cur).next.is_null() {
            cur = (*cur).next;
        }
    }

    xml_unlink_node(elem);

    if (matches!((*cur).typ, XmlElementType::XmlTextNode)
        && matches!((*elem).typ, XmlElementType::XmlTextNode)
        && (*cur).name == (*elem).name)
    {
        xml_node_add_content(cur, (*elem).content);
        xml_free_node(elem);
        return cur;
    } else if matches!((*elem).typ, XmlElementType::XmlAttributeNode) {
        return xml_add_prop_sibling(cur, cur, elem);
    }

    if (*elem).doc != (*cur).doc {
        xml_set_tree_doc(elem, (*cur).doc);
    }
    let parent: XmlNodePtr = (*cur).parent;
    (*elem).prev = cur;
    (*elem).next = null_mut();
    (*elem).parent = parent;
    (*cur).next = elem;
    if !parent.is_null() {
        (*parent).last = elem;
    }

    elem
}

/**
 * xmlAddNextSibling:
 * @cur:  the child node
 * @elem:  the new node
 *
 * Add a new node @elem as the next sibling of @cur
 * If the new node was already inserted in a document it is
 * first unlinked from its existing context.
 * As a result of text merging @elem may be freed.
 * If the new node is ATTRIBUTE, it is added into properties instead of children.
 * If there is an attribute with equal name, it is first destroyed.
 *
 * See the note regarding namespaces in xmlAddChild.
 *
 * Returns the new node or null_mut() in case of error.
 */
pub unsafe extern "C" fn xml_add_next_sibling(cur: XmlNodePtr, elem: XmlNodePtr) -> XmlNodePtr {
    if cur.is_null() || matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddNextSibling : cur.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }
    if elem.is_null() || ((*elem).typ == XmlElementType::XmlNamespaceDecl) {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddNextSibling : elem.is_null()\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    if cur == elem {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlAddNextSibling : cur == elem\n".as_ptr() as _);
        // #endif
        return null_mut();
    }

    xml_unlink_node(elem);

    if matches!((*elem).typ, XmlElementType::XmlTextNode) {
        if matches!((*cur).typ, XmlElementType::XmlTextNode) {
            xml_node_add_content(cur, (*elem).content);
            xml_free_node(elem);
            return cur;
        }
        if !(*cur).next.is_null()
            && matches!((*(*cur).next).typ, XmlElementType::XmlTextNode)
            && (*cur).name == (*(*cur).next).name
        {
            let mut tmp: *mut XmlChar;

            tmp = xml_strdup((*elem).content);
            tmp = xml_strcat(tmp, (*(*cur).next).content);
            xml_node_set_content((*cur).next, tmp);
            xml_free(tmp as _);
            xml_free_node(elem);
            return (*cur).next;
        }
    } else if matches!((*elem).typ, XmlElementType::XmlAttributeNode) {
        return xml_add_prop_sibling(cur, cur, elem);
    }

    if (*elem).doc != (*cur).doc {
        xml_set_tree_doc(elem, (*cur).doc);
    }
    (*elem).parent = (*cur).parent;
    (*elem).prev = cur;
    (*elem).next = (*cur).next;
    (*cur).next = elem;
    if !(*elem).next.is_null() {
        (*(*elem).next).prev = elem;
    }
    if !(*elem).parent.is_null() && (*(*elem).parent).last == cur {
        (*(*elem).parent).last = elem;
    }
    elem
}

/**
 * xmlUnlinkNode:
 * @cur:  the node
 *
 * Unlink a node from it's current context, the node is not freed
 * If one need to free the node, use xmlFreeNode() routine after the
 * unlink to discard it.
 * Note that namespace nodes can't be unlinked as they do not have
 * pointer to their parent.
 */
pub unsafe extern "C" fn xml_unlink_node(cur: XmlNodePtr) {
    if cur.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlUnlinkNode : node.is_null()\n".as_ptr() as _);
        // #endif
        return;
    }
    if matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        return;
    }
    if matches!((*cur).typ, XmlElementType::XmlDtdNode) {
        let doc: XmlDocPtr = (*cur).doc;
        if !doc.is_null() {
            if (*doc).int_subset == cur as _ {
                (*doc).int_subset = null_mut();
            }
            if (*doc).ext_subset == cur as _ {
                (*doc).ext_subset = null_mut();
            }
        }
    }
    if matches!((*cur).typ, XmlElementType::XmlEntityDecl) {
        let doc: XmlDocPtr = (*cur).doc;
        if !doc.is_null() {
            if !(*doc).int_subset.is_null() {
                if xmlHashLookup((*(*doc).int_subset).entities as _, (*cur).name) == cur as _ {
                    xmlHashRemoveEntry((*(*doc).int_subset).entities as _, (*cur).name, None);
                }
                if xmlHashLookup((*(*doc).int_subset).pentities as _, (*cur).name) == cur as _ {
                    xmlHashRemoveEntry((*(*doc).int_subset).pentities as _, (*cur).name, None);
                }
            }
            if !(*doc).ext_subset.is_null() {
                if xmlHashLookup((*(*doc).ext_subset).entities as _, (*cur).name) == cur as _ {
                    xmlHashRemoveEntry((*(*doc).ext_subset).entities as _, (*cur).name, None);
                }
                if xmlHashLookup((*(*doc).ext_subset).pentities as _, (*cur).name) == cur as _ {
                    xmlHashRemoveEntry((*(*doc).ext_subset).pentities as _, (*cur).name, None);
                }
            }
        }
    }
    if !(*cur).parent.is_null() {
        let parent: XmlNodePtr = (*cur).parent;
        if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
            if (*parent).properties == cur as _ {
                (*parent).properties = (*(cur as XmlAttrPtr)).next;
            }
        } else {
            if (*parent).children == cur {
                (*parent).children = (*cur).next;
            }
            if (*parent).last == cur {
                (*parent).last = (*cur).prev;
            }
        }
        (*cur).parent = null_mut();
    }
    if !(*cur).next.is_null() {
        (*(*cur).next).prev = (*cur).prev;
    }
    if !(*cur).prev.is_null() {
        (*(*cur).prev).next = (*cur).next;
    }
    (*cur).next = null_mut();
    (*cur).prev = null_mut();
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
    xml_node_add_content(first, (*second).content);
    xml_unlink_node(second);
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
    len: c_int,
) -> c_int {
    if node.is_null() {
        return -1;
    }

    if !matches!((*node).typ, XmlElementType::XmlTextNode)
        && !matches!((*node).typ, XmlElementType::XmlCdataSectionNode)
        && !matches!((*node).typ, XmlElementType::XmlCommentNode)
        && !matches!((*node).typ, XmlElementType::XmlPiNode)
    {
        // #ifdef DEBUG_TREE
        // 	xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlTextConcat: node is not text nor CDATA\n".as_ptr() as _);
        // #endif
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
    let mut depth: size_t = 0;

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
                xmlDeregisterNodeDefaultValue(cur as _);
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
        xmlDeregisterNodeDefaultValue(cur as _);
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
        let old_dict_owns_old_value: c_int =
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
    if !name_space.is_null() && xml_str_equal(name_space, c"xml".as_ptr() as _) != 0 {
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
                    && xml_str_equal((*cur).prefix.load(Ordering::Relaxed), name_space) != 0
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
                        && xml_str_equal((*cur).prefix.load(Ordering::Relaxed), name_space) != 0
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
) -> c_int {
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
                    && xml_str_equal((*tst).prefix.load(Ordering::Relaxed), prefix) != 0
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
    if xml_str_equal(href, XML_XML_NAMESPACE.as_ptr() as _) != 0 {
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
    let is_attr: c_int = matches!((*node).typ, XmlElementType::XmlAttributeNode) as i32;
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
                    && xml_str_equal((*cur).href.load(Ordering::Relaxed), href) != 0
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
                    && xml_str_equal((*cur).href.load(Ordering::Relaxed), href) != 0
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
 * xmlGetNsList:
 * @doc:  the document
 * @node:  the current node
 *
 * Search all the namespace applying to a given element.
 * Returns an null_mut() terminated array of all the #xmlNsPtr found
 *         that need to be freed by the caller or null_mut() if no
 *         namespace if defined
 */
#[cfg(any(feature = "tree", feature = "xpath", feature = "schema"))]
pub unsafe extern "C" fn xml_get_ns_list(
    _doc: *const XmlDoc,
    mut node: *const XmlNode,
) -> *mut XmlNsPtr {
    let mut cur: XmlNsPtr;
    let mut ret: *mut XmlNsPtr = null_mut();
    let mut nbns: c_int = 0;
    let mut maxns: c_int = 0;

    if node.is_null() || matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }

    while !node.is_null() {
        if matches!((*node).typ, XmlElementType::XmlElementNode) {
            cur = (*node).ns_def;
            'b: while !cur.is_null() {
                for i in 0..nbns {
                    if ((*cur).prefix.load(Ordering::Relaxed)
                        == (*(*ret.add(i as usize))).prefix.load(Ordering::Relaxed))
                        || xml_str_equal(
                            (*cur).prefix.load(Ordering::Relaxed),
                            (*(*ret.add(i as usize))).prefix.load(Ordering::Relaxed),
                        ) != 0
                    {
                        cur = (*cur).next.load(Ordering::Relaxed);
                        continue 'b;
                    }
                }
                if nbns >= maxns {
                    maxns = if maxns != 0 { maxns * 2 } else { 10 };
                    let tmp: *mut XmlNsPtr =
                        xml_realloc(ret as _, (maxns as usize + 1) * size_of::<XmlNsPtr>()) as _;
                    if tmp.is_null() {
                        xml_tree_err_memory(c"getting namespace list".as_ptr() as _);
                        xml_free(ret as _);
                        return null_mut();
                    }
                    ret = tmp;
                }
                *ret.add(nbns as usize) = cur;
                nbns += 1;
                *ret.add(nbns as usize) = null_mut();
            }
        }
        node = (*node).parent;
    }
    ret
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
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlSetNs: node.is_null()\n".as_ptr() as _);
        // #endif
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
        _ => {
            // #ifdef DEBUG_TREE
            // 	    xmlGenericError(xmlGenericErrorContext(),
            // 		    "xmlCopyNamespace: invalid type %d\n".as_ptr() as _, (*cur).typ);
            // #endif
            null_mut()
        }
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
/**
 * xmlSetProp:
 * @node:  the node
 * @name:  the attribute name (a QName)
 * @value:  the attribute value
 *
 * Set (or reset) an attribute carried by a node.
 * If @name has a prefix, then the corresponding
 * namespace-binding will be used, if in scope; it is an
 * error it there's no such ns-binding for the prefix in
 * scope.
 * Returns the attribute pointer.
 *
 */
#[cfg(any(
    feature = "tree",
    feature = "xinclude",
    feature = "schema",
    feature = "html"
))]
pub unsafe extern "C" fn xml_set_prop(
    node: XmlNodePtr,
    name: *const XmlChar,
    value: *const XmlChar,
) -> XmlAttrPtr {
    let mut len: c_int = 0;

    if node.is_null() || name.is_null() || !matches!((*node).typ, XmlElementType::XmlElementNode) {
        return null_mut();
    }

    /*
     * handle QNames
     */
    let nqname: *const XmlChar = xml_split_qname3(name, addr_of_mut!(len));
    if !nqname.is_null() {
        let prefix: *mut XmlChar = xml_strndup(name, len);
        let ns: XmlNsPtr = xml_search_ns((*node).doc, node, prefix);
        if !prefix.is_null() {
            xml_free(prefix as _);
        }
        if !ns.is_null() {
            return xml_set_ns_prop(node, ns, nqname, value);
        }
    }
    xml_set_ns_prop(node, null_mut(), name, value)
}

unsafe extern "C" fn xml_get_prop_node_internal(
    node: *const XmlNode,
    name: *const XmlChar,
    ns_name: *const XmlChar,
    use_dtd: c_int,
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
                if (*prop).ns.is_null() && xml_str_equal((*prop).name, name) != 0 {
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
                    && xml_str_equal((*prop).name, name) != 0
                    && ((*(*prop).ns).href.load(Ordering::Relaxed) == ns_name as _
                        || xml_str_equal((*(*prop).ns).href.load(Ordering::Relaxed), ns_name) != 0)
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
            } else if xml_str_equal(ns_name, XML_XML_NAMESPACE.as_ptr() as _) != 0 {
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
                let ns_list: *mut XmlNsPtr = xml_get_ns_list((*node).doc, node);
                if ns_list.is_null() {
                    if !tmpstr.is_null() {
                        xml_free(tmpstr as _);
                    }
                    return null_mut();
                }
                cur = ns_list;
                while !(*cur).is_null() {
                    if xml_str_equal((*(*cur)).href.load(Ordering::Relaxed), ns_name) != 0 {
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

/**
 * xmlSetNsProp:
 * @node:  the node
 * @ns:  the namespace definition
 * @name:  the attribute name
 * @value:  the attribute value
 *
 * Set (or reset) an attribute carried by a node.
 * The ns structure must be in scope, this is not checked
 *
 * Returns the attribute pointer.
 */
#[cfg(any(
    feature = "tree",
    feature = "xinclude",
    feature = "schema",
    feature = "html"
))]
pub unsafe extern "C" fn xml_set_ns_prop(
    node: XmlNodePtr,
    ns: XmlNsPtr,
    name: *const XmlChar,
    value: *const XmlChar,
) -> XmlAttrPtr {
    if !ns.is_null() && (*ns).href.load(Ordering::Relaxed).is_null() {
        return null_mut();
    }
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
    if !prop.is_null() {
        /*
        	* Modify the attribute's value.
        	*/
        if matches!((*prop).atype, Some(XmlAttributeType::XmlAttributeId)) {
            xml_remove_id((*node).doc, prop);
            (*prop).atype = Some(XmlAttributeType::XmlAttributeId);
        }
        if !(*prop).children.is_null() {
            xml_free_node_list((*prop).children);
        }
        (*prop).children = null_mut();
        (*prop).last = null_mut();
        (*prop).ns = ns;
        if !value.is_null() {
            let mut tmp: XmlNodePtr;

            (*prop).children = xml_new_doc_text((*node).doc, value);
            (*prop).last = null_mut();
            tmp = (*prop).children;
            while !tmp.is_null() {
                (*tmp).parent = prop as _;
                if (*tmp).next.is_null() {
                    (*prop).last = tmp;
                }
                tmp = (*tmp).next;
            }
        }
        if matches!((*prop).atype, Some(XmlAttributeType::XmlAttributeId)) {
            xml_add_id(null_mut(), (*node).doc, value, prop);
        }
        return prop;
    }
    /*
     * No equal attr found; create a new one.
     */
    xml_new_prop_internal(node, ns, name, value, 0)
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
 *     It's up to the caller to free the memory with xml_free( as _).
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
 * xmlGetProp:
 * @node:  the node
 * @name:  the attribute name
 *
 * Search and get the value of an attribute associated to a node
 * This does the entity substitution.
 * This function looks in DTD attribute declaration for #FIXED or
 * default declaration values unless DTD use has been turned off.
 * NOTE: this function acts independently of namespaces associated
 *       to the attribute. Use xmlGetNsProp() or xmlGetNoNsProp()
 *       for namespace aware processing.
 *
 * Returns the attribute value or null_mut() if not found.
 *     It's up to the caller to free the memory with xml_free( as _).
 */
pub unsafe extern "C" fn xml_get_prop(node: *const XmlNode, name: *const XmlChar) -> *mut XmlChar {
    let prop: XmlAttrPtr = xml_has_prop(node, name);
    if prop.is_null() {
        return null_mut();
    }
    xml_get_prop_node_value_internal(prop)
}

/**
 * xmlHasProp:
 * @node:  the node
 * @name:  the attribute name
 *
 * Search an attribute associated to a node
 * This function also looks in DTD attribute declaration for #FIXED or
 * default declaration values unless DTD use has been turned off.
 *
 * Returns the attribute or the attribute declaration or null_mut() if
 *         neither was found.
 */
pub unsafe extern "C" fn xml_has_prop(node: *const XmlNode, name: *const XmlChar) -> XmlAttrPtr {
    let mut prop: XmlAttrPtr;

    if node.is_null() || !matches!((*node).typ, XmlElementType::XmlElementNode) || name.is_null() {
        return null_mut();
    }
    /*
     * Check on the properties attached to the node
     */
    prop = (*node).properties;
    while !prop.is_null() {
        if xml_str_equal((*prop).name, name) != 0 {
            return prop;
        }
        prop = (*prop).next;
    }
    if XML_CHECK_DTD.load(Ordering::Relaxed) == 0 {
        return null_mut();
    }

    /*
     * Check if there is a default declaration in the internal
     * or external subsets
     */
    let doc: XmlDocPtr = (*node).doc;
    if !doc.is_null() {
        let mut attr_decl: XmlAttributePtr;
        if !(*doc).int_subset.is_null() {
            attr_decl = xml_get_dtd_attr_desc((*doc).int_subset, (*node).name, name);
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl = xml_get_dtd_attr_desc((*doc).ext_subset, (*node).name, name);
            }
            if !attr_decl.is_null() && !(*attr_decl).default_value.is_null() {
                /* return attribute declaration only if a default value is given
                (that includes #FIXED declarations) */
                return attr_decl as _;
            }
        }
    }
    null_mut()
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
 * xmlGetNsProp:
 * @node:  the node
 * @name:  the attribute name
 * @nameSpace:  the URI of the namespace
 *
 * Search and get the value of an attribute associated to a node
 * This attribute has to be anchored in the namespace specified.
 * This does the entity substitution.
 * This function looks in DTD attribute declaration for #FIXED or
 * default declaration values unless DTD use has been turned off.
 *
 * Returns the attribute value or null_mut() if not found.
 *     It's up to the caller to free the memory with xml_free( as _).
 */
pub unsafe extern "C" fn xml_get_ns_prop(
    node: *const XmlNode,
    name: *const XmlChar,
    name_space: *const XmlChar,
) -> *mut XmlChar {
    let prop: XmlAttrPtr = xml_get_prop_node_internal(
        node,
        name,
        name_space,
        XML_CHECK_DTD.load(Ordering::Relaxed),
    );
    if prop.is_null() {
        return null_mut();
    }
    xml_get_prop_node_value_internal(prop)
}

/**
 * xmlTreeErr:
 * @code:  the error number
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
unsafe extern "C" fn xml_tree_err(code: c_int, node: XmlNodePtr, extra: *const c_char) {
    let msg = match XmlParserErrors::try_from(code) {
        Ok(XmlParserErrors::XmlTreeInvalidHex) => {
            c"invalid hexadecimal character value\n".as_ptr() as _
        }
        Ok(XmlParserErrors::XmlTreeInvalidDec) => {
            c"invalid decimal character value\n".as_ptr() as _
        }
        Ok(XmlParserErrors::XmlTreeUnterminatedEntity) => {
            c"unterminated entity reference %15s\n".as_ptr() as _
        }
        Ok(XmlParserErrors::XmlTreeNotUtf8) => c"string is not in UTF-8\n".as_ptr() as _,
        _ => c"unexpected error number\n".as_ptr() as _,
    };
    __xml_simple_error(XmlErrorDomain::XmlFromTree as i32, code, node, msg, extra);
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
            let mut charval: c_int = 0;
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
                    if (b'0'..=b'9').contains(&tmp) {
                        charval = charval * 16 + (tmp - b'0') as i32;
                    } else if (b'a'..=b'f').contains(&tmp) {
                        charval = charval * 16 + (tmp - b'a') as i32 + 10;
                    } else if (b'A'..=b'F').contains(&tmp) {
                        charval = charval * 16 + (tmp - b'A') as i32 + 10;
                    } else {
                        xml_tree_err(
                            XmlParserErrors::XmlTreeInvalidHex as i32,
                            doc as _,
                            null_mut(),
                        );
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
                    if (b'0'..=b'9').contains(&tmp) {
                        charval = charval * 10 + (tmp - b'0') as i32;
                    } else {
                        xml_tree_err(
                            XmlParserErrors::XmlTreeInvalidDec as i32,
                            doc as _,
                            null_mut(),
                        );
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
                    xml_tree_err(
                        XmlParserErrors::XmlTreeUnterminatedEntity as i32,
                        doc as _,
                        q as _,
                    );
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
                                last = xml_add_next_sibling(last, node);
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
                            last = xml_add_next_sibling(last, node);
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

                let len: c_int = xml_copy_char_multi_byte(buffer.as_mut_ptr() as _, charval);
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
            xml_add_next_sibling(last, node);
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
    len: c_int,
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
            let mut charval: c_int = 0;
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
                    if (b'0'..=b'9').contains(&tmp) {
                        charval = charval * 16 + (tmp - b'0') as i32;
                    } else if (b'a'..=b'f').contains(&tmp) {
                        charval = charval * 16 + (tmp - b'a') as i32 + 10;
                    } else if (b'A'..=b'F').contains(&tmp) {
                        charval = charval * 16 + (tmp - b'A') as i32 + 10;
                    } else {
                        xml_tree_err(
                            XmlParserErrors::XmlTreeInvalidHex as i32,
                            doc as _,
                            null_mut(),
                        );
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
                        xml_tree_err(
                            XmlParserErrors::XmlTreeInvalidDec as i32,
                            doc as _,
                            null_mut(),
                        );
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
                    xml_tree_err(
                        XmlParserErrors::XmlTreeUnterminatedEntity as i32,
                        doc as _,
                        q as _,
                    );
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
                                last = xml_add_next_sibling(last, node);
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
                            last = xml_add_next_sibling(last, node);
                        }
                    }
                    xml_free(val as _);
                }
                cur = cur.add(1);
                q = cur;
            }
            if charval != 0 {
                let mut buffer: [XmlChar; 10] = [0; 10];

                let l: c_int = xml_copy_char_multi_byte(buffer.as_mut_ptr() as _, charval);
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
            xml_add_next_sibling(last, node);
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
    in_line: c_int,
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
        // #if 0
        //         else {
        //             xmlGenericError(xmlGenericErrorContext,
        //                             "xmlGetNodeListString : invalid node type %d\n",
        //                             (*node).typ);
        //         }
        // #endif
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
    in_line: c_int,
) -> *mut XmlChar {
    use super::entities::xml_encode_special_chars;

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
        // #if 0
        //         else {
        //             xmlGenericError(xmlGenericErrorContext,
        //                             "xmlGetNodeListString : invalid node type %d\n",
        //                             (*node).typ);
        //         }
        // #endif
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
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNodeSetContent : node.is_null()\n".as_ptr() as _);
        // #endif
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
    len: c_int,
) {
    if cur.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNodeSetContentLen : node.is_null()\n".as_ptr() as _);
        // #endif
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
 * xmlNodeAddContent:
 * @cur:  the node being modified
 * @content:  extra content
 *
 * Append the extra substring to the node content.
 * NOTE: In contrast to xmlNodeSetContent(), @content is supposed to be
 *       raw text, so unescaped XML special chars are allowed, entity
 *       references are not supported.
 */
pub unsafe extern "C" fn xml_node_add_content(cur: XmlNodePtr, content: *const XmlChar) {
    if cur.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNodeAddContent : node.is_null()\n".as_ptr() as _);
        // #endif
        return;
    }
    if content.is_null() {
        return;
    }
    let len: c_int = xml_strlen(content);
    xml_node_add_content_len(cur, content, len);
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
    len: c_int,
) {
    if cur.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlNodeAddContentLen : node.is_null()\n".as_ptr() as _);
        // #endif
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
                tmp = xml_add_child(cur, new_node);
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
 * xmlNodeGetContent:
 * @cur:  the node being read
 *
 * Read the value of a node, this can be either the text carried
 * directly by this node if it's a TEXT node or the aggregate string
 * of the values carried by this node child's (TEXT and ENTITY_REF).
 * Entity references are substituted.
 * Returns a new #XmlChar * or null_mut() if no content is available.
 *     It's up to the caller to free the memory with xml_free( as _).
 */
pub unsafe extern "C" fn xml_node_get_content(cur: *const XmlNode) -> *mut XmlChar {
    if cur.is_null() {
        return null_mut();
    }
    match (*cur).typ {
        XmlElementType::XmlDocumentFragNode | XmlElementType::XmlElementNode => {
            let buf: XmlBufPtr = xml_buf_create_size(64);
            if buf.is_null() {
                return null_mut();
            }
            xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
            xml_buf_get_node_content(buf, cur);
            let ret: *mut XmlChar = xml_buf_detach(buf);
            xml_buf_free(buf);
            ret
        }
        XmlElementType::XmlAttributeNode => xml_get_prop_node_value_internal(cur as _),
        XmlElementType::XmlCommentNode | XmlElementType::XmlPiNode => {
            if !(*cur).content.is_null() {
                return xml_strdup((*cur).content);
            }
            null_mut()
        }
        XmlElementType::XmlEntityRefNode => {
            /* lookup entity declaration */
            let ent: XmlEntityPtr = xml_get_doc_entity((*cur).doc, (*cur).name);
            if ent.is_null() {
                return null_mut();
            }

            let buf: XmlBufPtr = xml_buf_create();
            if buf.is_null() {
                return null_mut();
            }
            xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);

            xml_buf_get_node_content(buf, cur);

            let ret: *mut XmlChar = xml_buf_detach(buf);
            xml_buf_free(buf);
            ret
        }
        XmlElementType::XmlEntityNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => null_mut(),
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode => {
            let buf: XmlBufPtr = xml_buf_create();
            if buf.is_null() {
                return null_mut();
            }
            xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);

            xml_buf_get_node_content(buf, cur);

            let ret: *mut XmlChar = xml_buf_detach(buf);
            xml_buf_free(buf);
            ret
        }
        XmlElementType::XmlNamespaceDecl => {
            let tmp: *mut XmlChar = xml_strdup((*(cur as XmlNsPtr)).href.load(Ordering::Relaxed));
            tmp
        }
        XmlElementType::XmlElementDecl => {
            /* TODO !!! */
            null_mut()
        }
        XmlElementType::XmlAttributeDecl => {
            /* TODO !!! */
            null_mut()
        }
        XmlElementType::XmlEntityDecl => {
            /* TODO !!! */
            null_mut()
        }
        XmlElementType::XmlCdataSectionNode | XmlElementType::XmlTextNode => {
            if !(*cur).content.is_null() {
                return xml_strdup((*cur).content);
            }
            null_mut()
        }
        _ => unreachable!(),
    }
    // return null_mut();
}

/**
 * xmlNodeBufGetContent:
 * @buffer:  a buffer
 * @cur:  the node being read
 *
 * Read the value of a node @cur, this can be either the text carried
 * directly by this node if it's a TEXT node or the aggregate string
 * of the values carried by this node child's (TEXT and ENTITY_REF).
 * Entity references are substituted.
 * Fills up the buffer @buffer with this value
 *
 * Returns 0 in case of success and -1 in case of error.
 */
pub unsafe extern "C" fn xml_node_buf_get_content(
    mut buffer: XmlBufferPtr,
    cur: *const XmlNode,
) -> c_int {
    if cur.is_null() || buffer.is_null() {
        return -1;
    }
    let buf: XmlBufPtr = xml_buf_from_buffer(buffer);
    let ret: c_int = xml_buf_get_node_content(buf, cur);
    buffer = xml_buf_back_to_buffer(buf);
    if ret < 0 || buffer.is_null() {
        return -1;
    }
    0
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
pub unsafe extern "C" fn xml_buf_get_node_content(
    buf: XmlBufPtr,
    mut cur: *const XmlNode,
) -> c_int {
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
 * xmlNodeGetLang:
 * @cur:  the node being checked
 *
 * Searches the language of a node, i.e. the values of the xml:lang
 * attribute or the one carried by the nearest ancestor.
 *
 * Returns a pointer to the lang value, or null_mut() if not found
 *     It's up to the caller to free the memory with xml_free( as _).
 */
pub unsafe extern "C" fn xml_node_get_lang(mut cur: *const XmlNode) -> *mut XmlChar {
    let mut lang: *mut XmlChar;

    if cur.is_null() || matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    while !cur.is_null() {
        lang = xml_get_ns_prop(cur, c"lang".as_ptr() as _, XML_XML_NAMESPACE.as_ptr() as _);
        if !lang.is_null() {
            return lang;
        }
        cur = (*cur).parent;
    }
    null_mut()
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
pub unsafe extern "C" fn xml_node_get_space_preserve(mut cur: *const XmlNode) -> c_int {
    let mut space: *mut XmlChar;

    if cur.is_null() || !matches!((*cur).typ, XmlElementType::XmlElementNode) {
        return -1;
    }
    while !cur.is_null() {
        space = xml_get_ns_prop(cur, c"space".as_ptr() as _, XML_XML_NAMESPACE.as_ptr() as _);
        if !space.is_null() {
            if xml_str_equal(space, c"preserve".as_ptr() as _) != 0 {
                xml_free(space as _);
                return 1;
            }
            if xml_str_equal(space, c"default".as_ptr() as _) != 0 {
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
    xml_set_ns_prop(cur, ns, c"lang".as_ptr() as _, lang);
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
pub unsafe extern "C" fn xml_node_set_space_preserve(cur: XmlNodePtr, val: c_int) {
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
            xml_set_ns_prop(cur, ns, c"space".as_ptr() as _, c"default".as_ptr() as _);
        }
        1 => {
            xml_set_ns_prop(cur, ns, c"space".as_ptr() as _, c"preserve".as_ptr() as _);
        }
        _ => {}
    }
}

/**
 * xmlNodeGetBase:
 * @doc:  the document the node pertains to
 * @cur:  the node being checked
 *
 * Searches for the BASE URL. The code should work on both XML
 * and HTML document even if base mechanisms are completely different.
 * It returns the base as defined in RFC 2396 sections
 * 5.1.1. Base URI within Document Content
 * and
 * 5.1.2. Base URI from the Encapsulating Entity
 * However it does not return the document base (5.1.3), use
 * (*doc).URL in this case
 *
 * Returns a pointer to the base URL, or null_mut() if not found
 *     It's up to the caller to free the memory with xml_free( as _).
 */
pub unsafe extern "C" fn xml_node_get_base(
    mut doc: *const XmlDoc,
    mut cur: *const XmlNode,
) -> *mut XmlChar {
    let mut oldbase: *mut XmlChar = null_mut();
    let mut base: *mut XmlChar;
    let mut newbase: *mut XmlChar;

    if cur.is_null() && doc.is_null() {
        return null_mut();
    }
    if !cur.is_null() && matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    if doc.is_null() {
        doc = (*cur).doc;
    }
    if !doc.is_null() && matches!((*doc).typ, XmlElementType::XmlHtmlDocumentNode) {
        cur = (*doc).children;
        while !cur.is_null() && !(*cur).name.is_null() {
            if !matches!((*cur).typ, XmlElementType::XmlElementNode) {
                cur = (*cur).next;
                continue;
            }
            if xml_strcasecmp((*cur).name, c"html".as_ptr() as _) == 0 {
                cur = (*cur).children;
                continue;
            }
            if xml_strcasecmp((*cur).name, c"head".as_ptr() as _) == 0 {
                cur = (*cur).children;
                continue;
            }
            if xml_strcasecmp((*cur).name, c"base".as_ptr() as _) == 0 {
                return xml_get_prop(cur, c"href".as_ptr() as _);
            }
            cur = (*cur).next;
        }
        return null_mut();
    }
    while !cur.is_null() {
        if matches!((*cur).typ, XmlElementType::XmlEntityDecl) {
            let ent: XmlEntityPtr = cur as _;
            return xml_strdup((*ent).uri.load(Ordering::Relaxed));
        }
        if matches!((*cur).typ, XmlElementType::XmlElementNode) {
            base = xml_get_ns_prop(cur, c"base".as_ptr() as _, XML_XML_NAMESPACE.as_ptr() as _);
            if !base.is_null() {
                if !oldbase.is_null() {
                    newbase = xml_build_uri(oldbase, base);
                    if !newbase.is_null() {
                        xml_free(oldbase as _);
                        xml_free(base as _);
                        oldbase = newbase;
                    } else {
                        xml_free(oldbase as _);
                        xml_free(base as _);
                        return null_mut();
                    }
                } else {
                    oldbase = base;
                }
                if xml_strncmp(oldbase, c"http://".as_ptr() as _, 7) == 0
                    || xml_strncmp(oldbase, c"ftp://".as_ptr() as _, 6) == 0
                    || xml_strncmp(oldbase, c"urn:".as_ptr() as _, 4) == 0
                {
                    return oldbase;
                }
            }
        }
        cur = (*cur).parent;
    }
    if !doc.is_null() && !(*doc).url.is_null() {
        if oldbase.is_null() {
            return xml_strdup((*doc).url);
        }
        newbase = xml_build_uri(oldbase, (*doc).url);
        xml_free(oldbase as _);
        return newbase;
    }
    oldbase
}

/**
 * xmlNodeSetBase:
 * @cur:  the node being changed
 * @uri:  the new base URI
 *
 * Set (or reset) the base URI of a node, i.e. the value of the
 * xml:base attribute.
 */
#[cfg(any(feature = "tree", feature = "xinclude"))]
pub unsafe extern "C" fn xml_node_set_base(cur: XmlNodePtr, uri: *const XmlChar) {
    use super::uri::xml_path_to_uri;

    if cur.is_null() {
        return;
    }
    match (*cur).typ {
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlNotationNode
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
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode => {
            let doc: XmlDocPtr = cur as _;

            if !(*doc).url.is_null() {
                xml_free((*doc).url as _);
            }
            if uri.is_null() {
                (*doc).url = null_mut();
            } else {
                (*doc).url = xml_path_to_uri(uri);
            }
            return;
        }
        _ => unreachable!(),
    }

    let ns: XmlNsPtr = xml_search_ns_by_href((*cur).doc, cur, XML_XML_NAMESPACE.as_ptr() as _);
    if ns.is_null() {
        return;
    }
    let fixed: *mut XmlChar = xml_path_to_uri(uri);
    if !fixed.is_null() {
        xml_set_ns_prop(cur, ns, c"base".as_ptr() as _, fixed);
        xml_free(fixed as _);
    } else {
        xml_set_ns_prop(cur, ns, c"base".as_ptr() as _, uri);
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
pub unsafe extern "C" fn xml_remove_prop(cur: XmlAttrPtr) -> c_int {
    let mut tmp: XmlAttrPtr;
    if cur.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlRemoveProp : cur.is_null()\n".as_ptr() as _);
        // #endif
        return -1;
    }
    if (*cur).parent.is_null() {
        // #ifdef DEBUG_TREE
        //         xmlGenericError(xmlGenericErrorContext,
        // 		c"xmlRemoveProp : (*cur).parent.is_null()\n".as_ptr() as _);
        // #endif
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
    // #ifdef DEBUG_TREE
    //     xmlGenericError(xmlGenericErrorContext,
    // 	    "xmlRemoveProp : attribute not owned by its node\n".as_ptr() as _);
    // #endif
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
) -> c_int {
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
    xml_unlink_node(prop as _);
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
pub unsafe extern "C" fn xml_unset_prop(node: XmlNodePtr, name: *const XmlChar) -> c_int {
    let prop: XmlAttrPtr = xml_get_prop_node_internal(node, name, null_mut(), 0);
    if prop.is_null() {
        return -1;
    }
    xml_unlink_node(prop as _);
    xml_free_prop(prop);
    0
}

/*
 * Internal, don't use.
 */
/**
 * xmlBufferWriteCHAR:
 * @buf:  the XML buffer
 * @string:  the string to add
 *
 * routine which manages and grows an output buffer. This one adds
 * xmlChars at the end of the buffer.
 */
pub unsafe extern "C" fn xml_buffer_write_xml_char(buf: XmlBufferPtr, string: *const XmlChar) {
    if buf.is_null() {
        return;
    }
    xml_buffer_cat(buf, string);
}

/**
 * xmlBufferWriteChar:
 * @buf:  the XML buffer output
 * @string:  the string to add
 *
 * routine which manage and grows an output buffer. This one add
 * C chars at the end of the array.
 */
pub unsafe extern "C" fn xml_buffer_write_char(buf: XmlBufferPtr, string: *const c_char) {
    if buf.is_null() {
        return;
    }
    xml_buffer_ccat(buf, string);
}

/**
 * xmlBufferWriteQuotedString:
 * @buf:  the XML buffer output
 * @string:  the string to add
 *
 * routine which manage and grows an output buffer. This one writes
 * a quoted or double quoted #XmlChar string, checking first if it holds
 * quote or double-quotes internally
 */
pub unsafe extern "C" fn xml_buffer_write_quoted_string(buf: XmlBufferPtr, string: *const XmlChar) {
    let mut cur: *const XmlChar;
    let mut base: *const XmlChar;
    if buf.is_null() {
        return;
    }
    if !xml_strchr(string, b'\"').is_null() {
        if !xml_strchr(string, b'\'').is_null() {
            // #ifdef DEBUG_BUFFER
            // 	    xmlGenericError(xmlGenericErrorContext(),
            //  "xmlBufferWriteQuotedString: string contains quote and double-quotes !\n".as_ptr() as _);
            // #endif
            xml_buffer_ccat(buf, c"\"".as_ptr() as _);
            base = string;
            cur = string;
            while *cur != 0 {
                if *cur == b'"' {
                    if base != cur {
                        xml_buffer_add(buf, base, cur.offset_from(base) as _);
                    }
                    xml_buffer_add(buf, c"&quot;".as_ptr() as _, 6);
                    cur = cur.add(1);
                    base = cur;
                } else {
                    cur = cur.add(1);
                }
            }
            if base != cur {
                xml_buffer_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buffer_ccat(buf, c"\"".as_ptr() as _);
        } else {
            xml_buffer_ccat(buf, c"\'".as_ptr() as _);
            xml_buffer_cat(buf, string);
            xml_buffer_ccat(buf, c"\'".as_ptr() as _);
        }
    } else {
        xml_buffer_ccat(buf, c"\"".as_ptr() as _);
        xml_buffer_cat(buf, string);
        xml_buffer_ccat(buf, c"\"".as_ptr() as _);
    }
}

/**
 * xmlAttrSerializeTxtContent:
 * @buf:  the XML buffer output
 * @doc:  the document
 * @attr: the attribute node
 * @string: the text content
 *
 * Serialize text attribute values to an xml simple buffer
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_attr_serialize_txt_content(
    buf: XmlBufferPtr,
    doc: XmlDocPtr,
    attr: XmlAttrPtr,
    string: *const XmlChar,
) {
    use crate::private::save::xml_buf_attr_serialize_txt_content;

    if buf.is_null() || string.is_null() {
        return;
    }
    let buffer: XmlBufPtr = xml_buf_from_buffer(buf);
    if buffer.is_null() {
        return;
    }
    xml_buf_attr_serialize_txt_content(buffer, doc, attr, string);
    xml_buf_back_to_buffer(buffer);
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
pub unsafe extern "C" fn xml_reconciliate_ns(doc: XmlDocPtr, tree: XmlNodePtr) -> c_int {
    let mut old_ns: *mut XmlNsPtr = null_mut();
    let mut new_ns: *mut XmlNsPtr = null_mut();
    let mut size_cache: c_int = 0;
    let mut nb_cache: c_int = 0;

    let mut n: XmlNsPtr;
    let mut node: XmlNodePtr = tree;
    let mut attr: XmlAttrPtr;
    let ret: c_int = 0;

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
 * Saving.
 */
/**
 * xmlDocDumpFormatMemory:
 * @cur:  the document
 * @mem:  OUT: the memory pointer
 * @size:  OUT: the memory length
 * @format:  should formatting spaces been added
 *
 *
 * Dump an XML document in memory and return the #XmlChar * and it's size.
 * It's up to the caller to free the memory with xml_free().
 * Note that @format = 1 provide node indenting only if xmlIndentTreeOutput = 1
 * or xmlKeepBlanksDefault(0) was called
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_doc_dump_format_memory(
    cur: XmlDocPtr,
    mem: *mut *mut XmlChar,
    size: *mut c_int,
    format: c_int,
) {
    xml_doc_dump_format_memory_enc(cur, mem, size, null_mut(), format);
}

/**
 * xmlDocDumpMemory:
 * @cur:  the document
 * @mem:  OUT: the memory pointer
 * @size:  OUT: the memory length
 *
 * Dump an XML document in memory and return the #XmlChar * and it's size
 * in bytes. It's up to the caller to free the memory with xml_free().
 * The resulting byte array is zero terminated, though the last 0 is not
 * included in the returned size.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_doc_dump_memory(
    cur: XmlDocPtr,
    mem: *mut *mut XmlChar,
    size: *mut c_int,
) {
    xml_doc_dump_format_memory_enc(cur, mem, size, null_mut(), 0);
}

/**
 * xmlDocDumpMemoryEnc:
 * @out_doc:  Document to generate XML text from
 * @doc_txt_ptr:  Memory pointer for allocated XML text
 * @doc_txt_len:  Length of the generated XML text
 * @txt_encoding:  Character encoding to use when generating XML text
 *
 * Dump the current DOM tree into memory using the character encoding specified
 * by the caller.  Note it is up to the caller of this function to free the
 * allocated memory with xml_free().
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_doc_dump_memory_enc(
    out_doc: XmlDocPtr,
    doc_txt_ptr: *mut *mut XmlChar,
    doc_txt_len: *mut c_int,
    txt_encoding: *const c_char,
) {
    xml_doc_dump_format_memory_enc(out_doc, doc_txt_ptr, doc_txt_len, txt_encoding, 0);
}

/**
 * xmlDocDumpFormatMemoryEnc:
 * @out_doc:  Document to generate XML text from
 * @doc_txt_ptr:  Memory pointer for allocated XML text
 * @doc_txt_len:  Length of the generated XML text
 * @txt_encoding:  Character encoding to use when generating XML text
 * @format:  should formatting spaces been added
 *
 * Dump the current DOM tree into memory using the character encoding specified
 * by the caller.  Note it is up to the caller of this function to free the
 * allocated memory with xml_free().
 * Note that @format = 1 provide node indenting only if xmlIndentTreeOutput = 1
 * or xmlKeepBlanksDefault(0) was called
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_doc_dump_format_memory_enc(
    out_doc: XmlDocPtr,
    doc_txt_ptr: *mut *mut XmlChar,
    mut doc_txt_len: *mut c_int,
    mut txt_encoding: *const c_char,
    format: c_int,
) {
    use std::mem::{size_of_val, zeroed};

    use crate::libxml::{
        encoding::{xmlCharEncCloseFunc, xmlFindCharEncodingHandler},
        xml_io::{
            xmlAllocOutputBuffer, xmlOutputBufferClose, xmlOutputBufferFlush, XmlOutputBufferPtr,
        },
        xmlsave::{
            xmlDocContentDumpOutput, xmlSaveCtxtInit, xml_save_err, xml_save_err_memory,
            XmlSaveCtxt, XmlSaveOption,
        },
    };

    use super::encoding::XmlCharEncodingHandlerPtr;

    let mut ctxt: XmlSaveCtxt = unsafe { zeroed() };
    let mut dummy: c_int = 0;

    let mut conv_hdlr: XmlCharEncodingHandlerPtr = null_mut();

    if doc_txt_len.is_null() {
        doc_txt_len = addr_of_mut!(dummy); /*  Continue, caller just won't get length */
    }

    if doc_txt_ptr.is_null() {
        *doc_txt_len = 0;
        return;
    }

    *doc_txt_ptr = null_mut();
    *doc_txt_len = 0;

    if out_doc.is_null() {
        /*  No document, no output  */
        return;
    }

    /*
     *  Validate the encoding value, if provided.
     *  This logic is copied from xmlSaveFileEnc.
     */

    if txt_encoding.is_null() {
        txt_encoding = (*out_doc).encoding as _;
    }
    if !txt_encoding.is_null() {
        conv_hdlr = xmlFindCharEncodingHandler(txt_encoding);
        if conv_hdlr.is_null() {
            xml_save_err(
                XmlParserErrors::XmlSaveUnknownEncoding as i32,
                out_doc as XmlNodePtr,
                txt_encoding,
            );
            return;
        }
    }

    let out_buff: XmlOutputBufferPtr = xmlAllocOutputBuffer(conv_hdlr);
    if out_buff.is_null() {
        xml_save_err_memory(c"creating buffer".as_ptr() as _);
        xmlCharEncCloseFunc(conv_hdlr);
        return;
    }

    memset(addr_of_mut!(ctxt) as _, 0, size_of_val(&ctxt));
    ctxt.buf = out_buff;
    ctxt.level = 0;
    ctxt.format = (format != 0) as i32;
    ctxt.encoding = txt_encoding as _;
    xmlSaveCtxtInit(addr_of_mut!(ctxt));
    ctxt.options |= XmlSaveOption::XmlSaveAsXml as i32;
    xmlDocContentDumpOutput(addr_of_mut!(ctxt), out_doc);
    xmlOutputBufferFlush(out_buff);
    if !(*out_buff).conv.is_null() {
        *doc_txt_len = xml_buf_use((*out_buff).conv) as _;
        *doc_txt_ptr = xml_strndup(xml_buf_content((*out_buff).conv), *doc_txt_len);
    } else {
        *doc_txt_len = xml_buf_use((*out_buff).buffer) as _;
        *doc_txt_ptr = xml_strndup(xml_buf_content((*out_buff).buffer), *doc_txt_len);
    }
    xmlOutputBufferClose(out_buff);

    if (*doc_txt_ptr).is_null() && *doc_txt_len > 0 {
        *doc_txt_len = 0;
        xml_save_err_memory(c"creating output".as_ptr() as _);
    }
}

/**
 * xmlDocFormatDump:
 * @f:  the FILE*
 * @cur:  the document
 * @format: should formatting spaces been added
 *
 * Dump an XML document to an open FILE.
 *
 * returns: the number of bytes written or -1 in case of failure.
 * Note that @format = 1 provide node indenting only if xmlIndentTreeOutput = 1
 * or xmlKeepBlanksDefault(0) was called
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_doc_format_dump(f: *mut FILE, cur: XmlDocPtr, format: c_int) -> c_int {
    use std::mem::{size_of_val, zeroed};

    use crate::libxml::{
        encoding::{xmlFindCharEncodingHandler, XmlCharEncodingHandlerPtr},
        xml_io::{xmlOutputBufferClose, xmlOutputBufferCreateFile, XmlOutputBufferPtr},
        xmlsave::{xmlDocContentDumpOutput, xmlSaveCtxtInit, XmlSaveOption},
    };

    use super::xmlsave::XmlSaveCtxt;

    let mut ctxt: XmlSaveCtxt = unsafe { zeroed() };

    let mut encoding: *const c_char;
    let mut handler: XmlCharEncodingHandlerPtr = null_mut();

    if cur.is_null() {
        // #ifdef DEBUG_TREE
        //         xml_generic_error!(xmlGenericErrorContext,
        // 		c"xmlDocDump : document.is_null()\n".as_ptr() as _);
        // #endif
        return -1;
    }
    encoding = (*cur).encoding as _;

    if !encoding.is_null() {
        handler = xmlFindCharEncodingHandler(encoding);
        if handler.is_null() {
            xml_free((*cur).encoding as _);
            (*cur).encoding = null_mut();
            encoding = null_mut();
        }
    }
    let buf: XmlOutputBufferPtr = xmlOutputBufferCreateFile(f, handler);
    if buf.is_null() {
        return -1;
    }
    memset(addr_of_mut!(ctxt) as _, 0, size_of_val(&ctxt));
    ctxt.buf = buf;
    ctxt.level = 0;
    ctxt.format = if format != 0 { 1 } else { 0 };
    ctxt.encoding = encoding as _;
    xmlSaveCtxtInit(addr_of_mut!(ctxt) as _);
    ctxt.options |= XmlSaveOption::XmlSaveAsXml as i32;
    xmlDocContentDumpOutput(addr_of_mut!(ctxt) as _, cur);

    let ret: c_int = xmlOutputBufferClose(buf);
    ret
}

/**
 * xmlDocDump:
 * @f:  the FILE*
 * @cur:  the document
 *
 * Dump an XML document to an open FILE.
 *
 * returns: the number of bytes written or -1 in case of failure.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_doc_dump(f: *mut FILE, cur: XmlDocPtr) -> c_int {
    xml_doc_format_dump(f, cur, 0)
}

/**
 * xmlElemDump:
 * @f:  the FILE * for the output
 * @doc:  the document
 * @cur:  the current node
 *
 * Dump an XML/HTML node, recursive behaviour, children are printed too.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_elem_dump(f: *mut FILE, doc: XmlDocPtr, cur: XmlNodePtr) {
    use crate::libxml::{
        htmltree::htmlNodeDumpOutput,
        parser::xml_init_parser,
        xml_io::{xmlOutputBufferClose, xmlOutputBufferCreateFile, XmlOutputBufferPtr},
    };

    xml_init_parser();

    if cur.is_null() {
        // #ifdef DEBUG_TREE
        //         xml_generic_error!(xmlGenericErrorContext,
        //                         c"xmlElemDump : cur.is_null()\n".as_ptr() as _);
        // #endif
        return;
    }
    // #ifdef DEBUG_TREE
    //     if doc.is_null() {
    //         xml_generic_error!(xmlGenericErrorContext,
    //                         c"xmlElemDump : doc.is_null()\n".as_ptr() as _);
    //     }
    // #endif

    let outbuf: XmlOutputBufferPtr = xmlOutputBufferCreateFile(f, null_mut());
    if outbuf.is_null() {
        return;
    }
    if !doc.is_null() && matches!((*doc).typ, XmlElementType::XmlHtmlDocumentNode) {
        #[cfg(feature = "html")]
        {
            htmlNodeDumpOutput(outbuf, doc, cur, null_mut());
        }
        #[cfg(not(feature = "html"))]
        {
            xmlSaveErr(
                XmlParserErrors::XmlErrInternalError,
                cur,
                c"HTML support not compiled in\n".as_ptr() as _,
            );
        }
    } else {
        xml_node_dump_output(outbuf, doc, cur, 0, 1, null_mut());
    }
    xmlOutputBufferClose(outbuf);
}

/**
 * xmlSaveFile:
 * @filename:  the filename (or URL)
 * @cur:  the document
 *
 * Dump an XML document to a file. Will use compression if
 * compiled in and enabled. If @filename is "-" the stdout file is
 * used.
 * returns: the number of bytes written or -1 in case of failure.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_save_file(filename: *const c_char, cur: XmlDocPtr) -> c_int {
    xml_save_format_file_enc(filename, cur, null_mut(), 0)
}

/**
 * xmlSaveFormatFile:
 * @filename:  the filename (or URL)
 * @cur:  the document
 * @format:  should formatting spaces been added
 *
 * Dump an XML document to a file. Will use compression if
 * compiled in and enabled. If @filename is "-" the stdout file is
 * used. If @format is set then the document will be indented on output.
 * Note that @format = 1 provide node indenting only if xmlIndentTreeOutput = 1
 * or xmlKeepBlanksDefault(0) was called
 *
 * returns: the number of bytes written or -1 in case of failure.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_save_format_file(
    filename: *const c_char,
    cur: XmlDocPtr,
    format: c_int,
) -> c_int {
    xml_save_format_file_enc(filename, cur, null_mut(), format)
}

/**
 * xmlBufNodeDump:
 * @buf:  the XML buffer output
 * @doc:  the document
 * @cur:  the current node
 * @level: the imbrication level for indenting
 * @format: is formatting allowed
 *
 * Dump an XML node, recursive behaviour,children are printed too.
 * Note that @format = 1 provide node indenting only if xmlIndentTreeOutput = 1
 * or xmlKeepBlanksDefault(0) was called
 *
 * Returns the number of bytes written to the buffer, in case of error 0
 *     is returned or @buf stores the error
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_buf_node_dump(
    buf: XmlBufPtr,
    doc: XmlDocPtr,
    cur: XmlNodePtr,
    level: c_int,
    format: c_int,
) -> size_t {
    use crate::{
        libxml::{xml_io::XmlOutputBufferPtr, xmlsave::xml_save_err_memory},
        private::buf::xml_buf_get_allocation_scheme,
    };

    use super::parser::xml_init_parser;

    xml_init_parser();

    if cur.is_null() {
        // #ifdef DEBUG_TREE
        //         xml_generic_error!(xmlGenericErrorContext,
        //                         c"xmlNodeDump : node.is_null()\n".as_ptr() as _);
        // #endif
        return usize::MAX;
    }
    if buf.is_null() {
        // #ifdef DEBUG_TREE
        //         xml_generic_error!(xmlGenericErrorContext,
        //                         c"xmlNodeDump : buf.is_null()\n".as_ptr() as _);
        // #endif
        return usize::MAX;
    }
    let outbuf: XmlOutputBufferPtr = xml_malloc(size_of::<XmlOutputBuffer>()) as _;
    if outbuf.is_null() {
        xml_save_err_memory(c"creating buffer".as_ptr() as _);
        return usize::MAX;
    }
    memset(outbuf as _, 0, size_of::<XmlOutputBuffer>());
    (*outbuf).buffer = buf;
    (*outbuf).encoder = null_mut();
    (*outbuf).writecallback = None;
    (*outbuf).closecallback = None;
    (*outbuf).context = null_mut();
    (*outbuf).written = 0;

    let using: size_t = xml_buf_use(buf);
    let oldalloc: c_int = xml_buf_get_allocation_scheme(buf);
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_node_dump_output(outbuf, doc, cur, level, format, null_mut());
    xml_buf_set_allocation_scheme(buf, oldalloc.try_into().unwrap());
    xml_free(outbuf as _);
    let ret: c_int = (xml_buf_use(buf) - using) as i32;
    ret as _
}

/**
 * xmlNodeDump:
 * @buf:  the XML buffer output
 * @doc:  the document
 * @cur:  the current node
 * @level: the imbrication level for indenting
 * @format: is formatting allowed
 *
 * Dump an XML node, recursive behaviour,children are printed too.
 * Note that @format = 1 provide node indenting only if xmlIndentTreeOutput = 1
 * or xmlKeepBlanksDefault(0) was called.
 * Since this is using xmlBuffer structures it is limited to 2GB and somehow
 * deprecated, use xmlNodeDumpOutput() instead.
 *
 * Returns the number of bytes written to the buffer or -1 in case of error
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_node_dump(
    buf: XmlBufferPtr,
    doc: XmlDocPtr,
    cur: XmlNodePtr,
    level: c_int,
    format: c_int,
) -> c_int {
    if buf.is_null() || cur.is_null() {
        return -1;
    }
    let buffer: XmlBufPtr = xml_buf_from_buffer(buf);
    if buffer.is_null() {
        return -1;
    }
    let ret: size_t = xml_buf_node_dump(buffer, doc, cur, level, format);
    xml_buf_back_to_buffer(buffer);
    if ret > i32::MAX as usize {
        return -1;
    }
    ret as _
}

/**
 * xmlSaveFileTo:
 * @buf:  an output I/O buffer
 * @cur:  the document
 * @encoding:  the encoding if any assuming the I/O layer handles the transcoding
 *
 * Dump an XML document to an I/O buffer.
 * Warning ! This call xmlOutputBufferClose() on buf which is not available
 * after this call.
 *
 * returns: the number of bytes written or -1 in case of failure.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_save_file_to(
    buf: XmlOutputBufferPtr,
    cur: XmlDocPtr,
    encoding: *const c_char,
) -> c_int {
    use std::mem::{size_of_val, zeroed};

    use crate::libxml::{
        xml_io::xmlOutputBufferClose,
        xmlsave::{xmlDocContentDumpOutput, xmlSaveCtxtInit, XmlSaveOption},
    };

    use super::xmlsave::XmlSaveCtxt;

    let mut ctxt: XmlSaveCtxt = unsafe { zeroed() };

    if buf.is_null() {
        return -1;
    }
    if cur.is_null() {
        xmlOutputBufferClose(buf);
        return -1;
    }
    memset(addr_of_mut!(ctxt) as _, 0, size_of_val(&ctxt));
    ctxt.buf = buf;
    ctxt.level = 0;
    ctxt.format = 0;
    ctxt.encoding = encoding as _;
    xmlSaveCtxtInit(addr_of_mut!(ctxt) as _);
    ctxt.options |= XmlSaveOption::XmlSaveAsXml as i32;
    xmlDocContentDumpOutput(addr_of_mut!(ctxt) as _, cur);
    let ret: c_int = xmlOutputBufferClose(buf);
    ret
}

/**
 * xmlSaveFormatFileTo:
 * @buf:  an output I/O buffer
 * @cur:  the document
 * @encoding:  the encoding if any assuming the I/O layer handles the transcoding
 * @format: should formatting spaces been added
 *
 * Dump an XML document to an I/O buffer.
 * Warning ! This call xmlOutputBufferClose() on buf which is not available
 * after this call.
 *
 * returns: the number of bytes written or -1 in case of failure.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_save_format_file_to(
    buf: XmlOutputBufferPtr,
    cur: XmlDocPtr,
    encoding: *const c_char,
    format: c_int,
) -> c_int {
    use std::mem::{size_of_val, zeroed};

    use crate::libxml::{
        xml_io::xmlOutputBufferClose,
        xmlsave::{xmlDocContentDumpOutput, xmlSaveCtxtInit, XmlSaveOption},
    };

    use super::xmlsave::XmlSaveCtxt;

    let mut ctxt: XmlSaveCtxt = unsafe { zeroed() };

    if buf.is_null() {
        return -1;
    }
    if cur.is_null()
        || (!matches!((*cur).typ, XmlElementType::XmlDocumentNode)
            && !matches!((*cur).typ, XmlElementType::XmlHtmlDocumentNode))
    {
        xmlOutputBufferClose(buf);
        return -1;
    }
    memset(addr_of_mut!(ctxt) as _, 0, size_of_val(&ctxt));
    ctxt.buf = buf;
    ctxt.level = 0;
    ctxt.format = if format != 0 { 1 } else { 0 };
    ctxt.encoding = encoding as _;
    xmlSaveCtxtInit(addr_of_mut!(ctxt) as _);
    ctxt.options |= XmlSaveOption::XmlSaveAsXml as i32;
    xmlDocContentDumpOutput(addr_of_mut!(ctxt) as _, cur);
    let ret: c_int = xmlOutputBufferClose(buf);
    ret
}

/**
 * xmlNodeDumpOutput:
 * @buf:  the XML buffer output
 * @doc:  the document
 * @cur:  the current node
 * @level: the imbrication level for indenting
 * @format: is formatting allowed
 * @encoding:  an optional encoding string
 *
 * Dump an XML node, recursive behaviour, children are printed too.
 * Note that @format = 1 provide node indenting only if xmlIndentTreeOutput = 1
 * or xmlKeepBlanksDefault(0) was called
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_node_dump_output(
    buf: XmlOutputBufferPtr,
    doc: XmlDocPtr,
    cur: XmlNodePtr,
    level: c_int,
    format: c_int,
    mut encoding: *const c_char,
) {
    use std::mem::{size_of_val, zeroed};

    use crate::libxml::{
        parser::xml_init_parser,
        xmlsave::{xhtmlNodeDumpOutput, xmlNodeDumpOutputInternal, xmlSaveCtxtInit, XmlSaveOption},
    };

    use super::xmlsave::XmlSaveCtxt;

    let mut ctxt: XmlSaveCtxt = unsafe { zeroed() };
    #[cfg(feature = "html")]
    let dtd: XmlDtdPtr;
    #[cfg(feature = "html")]
    let mut is_xhtml: c_int = 0;

    xml_init_parser();

    if buf.is_null() || cur.is_null() {
        return;
    }

    if encoding.is_null() {
        encoding = c"UTF-8".as_ptr() as _;
    }

    memset(addr_of_mut!(ctxt) as _, 0, size_of_val(&ctxt));
    ctxt.buf = buf;
    ctxt.level = level;
    ctxt.format = if format != 0 { 1 } else { 0 };
    ctxt.encoding = encoding as _;
    xmlSaveCtxtInit(addr_of_mut!(ctxt) as _);
    ctxt.options |= XmlSaveOption::XmlSaveAsXml as i32;

    #[cfg(feature = "html")]
    {
        dtd = xml_get_int_subset(doc);
        if !dtd.is_null() {
            is_xhtml = xml_is_xhtml((*dtd).system_id, (*dtd).external_id);
            if is_xhtml < 0 {
                is_xhtml = 0;
            }
        }

        if is_xhtml != 0 {
            xhtmlNodeDumpOutput(addr_of_mut!(ctxt) as _, cur);
        } else {
            xmlNodeDumpOutputInternal(addr_of_mut!(ctxt) as _, cur);
        }
    }
    #[cfg(not(feature = "html"))]
    {
        xmlNodeDumpOutputInternal(addr_of_mut!(ctxt) as _, cur);
    }
}

/**
 * xmlSaveFormatFileEnc:
 * @filename:  the filename or URL to output
 * @cur:  the document being saved
 * @encoding:  the name of the encoding to use or NULL.
 * @format:  should formatting spaces be added.
 *
 * Dump an XML document to a file or an URL.
 *
 * Returns the number of bytes written or -1 in case of error.
 * Note that @format = 1 provide node indenting only if xmlIndentTreeOutput = 1
 * or xmlKeepBlanksDefault(0) was called
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_save_format_file_enc(
    filename: *const c_char,
    cur: XmlDocPtr,
    mut encoding: *const c_char,
    format: c_int,
) -> c_int {
    use std::mem::{size_of_val, zeroed};

    use crate::libxml::{
        encoding::{xmlFindCharEncodingHandler, XmlCharEncodingHandlerPtr},
        xml_io::{xmlOutputBufferClose, xmlOutputBufferCreateFilename},
        xmlsave::{xmlDocContentDumpOutput, xmlSaveCtxtInit, XmlSaveOption},
    };

    use super::xmlsave::XmlSaveCtxt;

    let mut ctxt: XmlSaveCtxt = unsafe { zeroed() };

    let mut handler: XmlCharEncodingHandlerPtr = null_mut();

    if cur.is_null() {
        return -1;
    }

    if encoding.is_null() {
        encoding = (*cur).encoding as _;
    }

    if !encoding.is_null() {
        handler = xmlFindCharEncodingHandler(encoding);
        if handler.is_null() {
            return -1;
        }
    }

    // #ifdef LIBXML_ZLIB_ENABLED
    //     if ((*cur).compression < 0) (*cur).compression = xmlGetCompressMode();
    // #endif
    /*
     * save the content to a temp buffer.
     */
    let buf: XmlOutputBufferPtr =
        xmlOutputBufferCreateFilename(filename, handler, (*cur).compression);
    if buf.is_null() {
        return -1;
    }
    memset(addr_of_mut!(ctxt) as _, 0, size_of_val(&ctxt));
    ctxt.buf = buf;
    ctxt.level = 0;
    ctxt.format = if format != 0 { 1 } else { 0 };
    ctxt.encoding = encoding as _;
    xmlSaveCtxtInit(addr_of_mut!(ctxt) as _);
    ctxt.options |= XmlSaveOption::XmlSaveAsXml as i32;

    xmlDocContentDumpOutput(addr_of_mut!(ctxt) as _, cur);

    let ret: c_int = xmlOutputBufferClose(buf);
    ret
}

/**
 * xmlSaveFileEnc:
 * @filename:  the filename (or URL)
 * @cur:  the document
 * @encoding:  the name of an encoding (or NULL)
 *
 * Dump an XML document, converting it to the given encoding
 *
 * returns: the number of bytes written or -1 in case of failure.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn xml_save_file_enc(
    filename: *const c_char,
    cur: XmlDocPtr,
    encoding: *const c_char,
) -> c_int {
    xml_save_format_file_enc(filename, cur, encoding, 0)
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
pub unsafe extern "C" fn xml_is_xhtml(
    system_id: *const XmlChar,
    public_id: *const XmlChar,
) -> c_int {
    if system_id.is_null() && public_id.is_null() {
        return -1;
    }
    if !public_id.is_null() {
        if xml_str_equal(public_id, XHTML_STRICT_PUBLIC_ID.as_ptr() as _) != 0 {
            return 1;
        }
        if xml_str_equal(public_id, XHTML_FRAME_PUBLIC_ID.as_ptr() as _) != 0 {
            return 1;
        }
        if xml_str_equal(public_id, XHTML_TRANS_PUBLIC_ID.as_ptr() as _) != 0 {
            return 1;
        }
    }
    if !system_id.is_null() {
        if xml_str_equal(system_id, XHTML_STRICT_SYSTEM_ID.as_ptr() as _) != 0 {
            return 1;
        }
        if xml_str_equal(system_id, XHTML_FRAME_SYSTEM_ID.as_ptr() as _) != 0 {
            return 1;
        }
        if xml_str_equal(system_id, XHTML_TRANS_SYSTEM_ID.as_ptr() as _) != 0 {
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
pub unsafe extern "C" fn xml_get_doc_compress_mode(doc: *const XmlDoc) -> c_int {
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
pub unsafe extern "C" fn xml_set_doc_compress_mode(doc: XmlDocPtr, mode: c_int) {
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

pub unsafe extern "C" fn xml_get_compress_mode() -> c_int {
    XML_COMPRESS_MODE.load(Ordering::Acquire)
}

/**
 * xmlSetCompressMode:
 * @mode:  the compression ratio
 *
 * set the default compression mode used, ZLIB based
 * Correct values: 0 (uncompressed) to 9 (max compression)
 */
pub unsafe extern "C" fn xml_set_compress_mode(mode: c_int) {
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
    position: c_int,
    old_ns: XmlNsPtr,
    new_ns: XmlNsPtr,
    depth: c_int,
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
) -> c_int {
    let mut cur: XmlNodePtr;
    let mut ns: XmlNsPtr;
    let mut mi: XmlNsMapItemPtr;
    let mut shadowed: c_int;

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
                            ) != 0
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
    size: *mut c_int,
    number: *mut c_int,
    old_ns: XmlNsPtr,
    new_ns: XmlNsPtr,
) -> c_int {
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
                || xml_str_equal((*ns).prefix.load(Ordering::Relaxed), prefix) != 0)
                && xml_str_equal((*ns).href.load(Ordering::Relaxed), ns_name) != 0
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
                || xml_str_equal(prefix, (*ns).prefix.load(Ordering::Relaxed)) != 0
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
) -> c_int {
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
                        || xml_str_equal(prefix, (*ns).prefix.load(Ordering::Relaxed)) != 0
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
    check_shadow: c_int,
) -> XmlNsPtr {
    let ret: XmlNsPtr;
    let mut buf: [c_char; 50] = [0; 50];
    let mut pref: *const XmlChar;
    let mut counter: c_int = 0;

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
    depth: c_int,
    ancestors_only: c_int,
    prefixed: c_int,
) -> c_int {
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
                xml_str_equal((*(*mi).new_ns).href.load(Ordering::Relaxed), (*ns).href.load(Ordering::Relaxed)) != 0)
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
                        ) != 0)
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
    options: c_int,
) -> c_int {
    let mut depth: c_int = -1;
    let mut adoptns: c_int;
    let mut parnsdone: c_int = 0;
    let mut ns: XmlNsPtr = null_mut();
    let mut prevns: XmlNsPtr;
    let mut cur: XmlNodePtr;
    let mut cur_elem: XmlNodePtr = null_mut();
    let mut ns_map: XmlNsMapPtr = null_mut();
    let mut mi: XmlNsMapItemPtr;
    /* @ancestorsOnly should be set by an option flag. */
    let ancestors_only: c_int = 0;
    let opt_remove_redundant_ns: c_int =
        if options & XmlDomreconcileNsoptions::XmlDomReconnsRemoveredund as i32 != 0 {
            1
        } else {
            0
        };
    let mut list_redund: *mut XmlNsPtr = null_mut();
    let mut size_redund: c_int = 0;
    let mut nb_redund: c_int = 0;

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
                                                    ) != 0)
                                                && ((*ns).href.load(Ordering::Relaxed)
                                                    == (*(*mi).new_ns).href.load(Ordering::Relaxed)
                                                    || xml_str_equal(
                                                        (*ns).href.load(Ordering::Relaxed),
                                                        (*(*mi).new_ns)
                                                            .href
                                                            .load(Ordering::Relaxed),
                                                    ) != 0)
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
                                                    ) != 0)
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
                                if (*cur).ns == *list_redund.add(j as usize) {
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
    _options: c_int,
) -> c_int {
    let mut ret: c_int = 0;
    let mut cur: XmlNodePtr;
    let mut cur_elem: XmlNodePtr = null_mut();
    let mut ns_map: XmlNsMapPtr = null_mut();
    let mut mi: XmlNsMapItemPtr;
    let mut ns: XmlNsPtr = null_mut();
    let mut depth: c_int = -1;
    /* gather @parent's ns-decls. */
    let mut parnsdone: c_int;
    /* @ancestorsOnly should be set per option. */
    let ancestors_only: c_int = 0;

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
                                                        ) != 0)
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
    prefixed: c_int,
) -> c_int {
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
    if xml_str_equal(ns_name, XML_XML_NAMESPACE.as_ptr() as _) != 0 {
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
                                    ) != 0)
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
                        || xml_str_equal(ns_name, (*ns).href.load(Ordering::Relaxed)) != 0
                    {
                        /*
                         * At this point the prefix can only be shadowed,
                         * if we are the the (at least) 3rd level of
                         * ns-decls.
                         */
                        if !out.is_null() {
                            let ret: c_int = xml_ns_in_scope(
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
    _options: c_int,
) -> c_int {
    let mut cur: XmlNodePtr;
    let adopt_str: c_int = 1;

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
    options: c_int,
) -> c_int {
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
        xml_unlink_node(node);
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
        let mut adopt_str: c_int = 1;

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
    _options: c_int,
) -> c_int {
    let mut list: *mut XmlNsPtr = null_mut();
    let mut size_list: c_int = 0;
    let mut nb_list: c_int = 0;
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
            xml_unlink_node(node);
            return 0;
        }
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {}
        _ => {
            return 1;
        }
    }
    xml_unlink_node(node);
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
    deep: c_int,
    _options: c_int,
) -> c_int {
    let mut ret: c_int = 0;
    let mut cur: XmlNodePtr;
    let mut cur_elem: XmlNodePtr = null_mut();
    let mut ns_map: XmlNsMapPtr = null_mut();
    let mut mi: XmlNsMapItemPtr;
    let mut ns: XmlNsPtr = null_mut();
    let mut depth: c_int = -1;
    /* let adoptStr: c_int = 1; */
    /* gather @parent's ns-decls. */
    let mut parnsdone: c_int = 0;
    /*
     * @ancestorsOnly:
     * TODO: @ancestorsOnly should be set per option.
     *
     */
    let ancestors_only: c_int = 0;
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
                                                    ) != 0)
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
pub unsafe extern "C" fn xml_child_element_count(parent: XmlNodePtr) -> c_ulong {
    let mut ret: c_ulong = 0;
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
    use crate::{
        libxml::{xmlerror::xmlResetLastError, xmlmemory::xml_mem_blocks},
        test_util::*,
    };

    use super::*;

    #[test]
    fn test_xml_add_child() {
        unsafe {
            let mut leaks = 0;
            for n_parent in 0..GEN_NB_XML_NODE_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR_IN {
                    let mem_base = xml_mem_blocks();
                    let parent = gen_xml_node_ptr(n_parent, 0);
                    let mut cur = gen_xml_node_ptr_in(n_cur, 1);

                    let ret_val = xml_add_child(parent, cur);
                    if ret_val.is_null() {
                        xml_free_node(cur);
                        cur = null_mut();
                    }
                    desret_xml_node_ptr(ret_val);
                    des_xml_node_ptr(n_parent, parent, 0);
                    des_xml_node_ptr_in(n_cur, cur, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlAddChild",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlAddChild()");
                        eprint!(" {}", n_parent);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_add_child_list() {
        unsafe {
            let mut leaks = 0;
            for n_parent in 0..GEN_NB_XML_NODE_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR_IN {
                    let mem_base = xml_mem_blocks();
                    let parent = gen_xml_node_ptr(n_parent, 0);
                    let mut cur = gen_xml_node_ptr_in(n_cur, 1);

                    let ret_val = xml_add_child_list(parent, cur);
                    if ret_val.is_null() {
                        xml_free_node_list(cur);
                        cur = null_mut();
                    }
                    desret_xml_node_ptr(ret_val);
                    des_xml_node_ptr(n_parent, parent, 0);
                    des_xml_node_ptr_in(n_cur, cur, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlAddChildList",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlAddChildList()");
                        eprint!(" {}", n_parent);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_add_next_sibling() {
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_XML_NODE_PTR {
                for n_elem in 0..GEN_NB_XML_NODE_PTR_IN {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_ptr(n_cur, 0);
                    let mut elem = gen_xml_node_ptr_in(n_elem, 1);

                    let ret_val = xml_add_next_sibling(cur, elem);
                    if ret_val.is_null() {
                        xml_free_node(elem);
                        elem = null_mut();
                    }
                    desret_xml_node_ptr(ret_val);
                    des_xml_node_ptr(n_cur, cur, 0);
                    des_xml_node_ptr_in(n_elem, elem, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlAddNextSibling",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlAddNextSibling()");
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_elem);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_add_prev_sibling() {
        #[cfg(any(
            feature = "tree",
            feature = "html",
            feature = "schema",
            feature = "xinclude"
        ))]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_NODE_PTR {
                for n_elem in 0..GEN_NB_XML_NODE_PTR_IN {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_ptr(n_cur, 0);
                    let mut elem = gen_xml_node_ptr_in(n_elem, 1);

                    let ret_val = xml_add_prev_sibling(cur, elem);
                    if ret_val.is_null() {
                        xml_free_node(elem);
                        elem = null_mut();
                    }
                    desret_xml_node_ptr(ret_val);
                    des_xml_node_ptr(n_cur, cur, 0);
                    des_xml_node_ptr_in(n_elem, elem, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlAddPrevSibling",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlAddPrevSibling()");
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_elem);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_add_sibling() {
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_XML_NODE_PTR {
                for n_elem in 0..GEN_NB_XML_NODE_PTR_IN {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_ptr(n_cur, 0);
                    let mut elem = gen_xml_node_ptr_in(n_elem, 1);

                    let ret_val = xml_add_sibling(cur, elem);
                    if ret_val.is_null() {
                        xml_free_node(elem);
                        elem = null_mut();
                    }
                    desret_xml_node_ptr(ret_val);
                    des_xml_node_ptr(n_cur, cur, 0);
                    des_xml_node_ptr_in(n_elem, elem, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlAddSibling",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlAddSibling()");
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_elem);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_attr_serialize_txt_content() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_attr in 0..GEN_NB_XML_ATTR_PTR {
                        for n_string in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let buf = gen_xml_buffer_ptr(n_buf, 0);
                            let doc = gen_xml_doc_ptr(n_doc, 1);
                            let attr = gen_xml_attr_ptr(n_attr, 2);
                            let string = gen_const_xml_char_ptr(n_string, 3);

                            xml_attr_serialize_txt_content(buf, doc, attr, string);
                            des_xml_buffer_ptr(n_buf, buf, 0);
                            des_xml_doc_ptr(n_doc, doc, 1);
                            des_xml_attr_ptr(n_attr, attr, 2);
                            des_const_xml_char_ptr(n_string, string, 3);
                            xmlResetLastError();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlAttrSerializeTxtContent",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlAttrSerializeTxtContent()"
                                );
                                eprint!(" {}", n_buf);
                                eprint!(" {}", n_doc);
                                eprint!(" {}", n_attr);
                                eprintln!(" {}", n_string);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_buf_content() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_CONST_XML_BUF_PTR {
                let mem_base = xml_mem_blocks();
                let buf = gen_const_xml_buf_ptr(n_buf, 0);

                let ret_val = xml_buf_content(buf);
                desret_xml_char_ptr(ret_val);
                des_const_xml_buf_ptr(n_buf, buf, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlBufContent",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlBufContent()");
                    eprintln!(" {}", n_buf);
                }
            }
        }
    }

    #[test]
    fn test_xml_buf_end() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUF_PTR {
                let mem_base = xml_mem_blocks();
                let buf = gen_xml_buf_ptr(n_buf, 0);

                let ret_val = xml_buf_end(buf as _);
                desret_xml_char_ptr(ret_val);
                des_xml_buf_ptr(n_buf, buf, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlBufEnd",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlBufEnd()");
                    eprintln!(" {}", n_buf);
                }
            }
        }
    }

    #[test]
    fn test_xml_buf_get_node_content() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUF_PTR {
                for n_cur in 0..GEN_NB_CONST_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_xml_buf_ptr(n_buf, 0);
                    let cur = gen_const_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_buf_get_node_content(buf as _, cur);
                    desret_int(ret_val);
                    des_xml_buf_ptr(n_buf, buf, 0);
                    des_const_xml_node_ptr(n_cur, cur, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlBufGetNodeContent",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlBufGetNodeContent()"
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_buf_node_dump() {

        /* missing type support */
    }

    #[test]
    fn test_xml_buf_shrink() {

        /* missing type support */
    }

    #[test]
    fn test_xml_buf_use() {

        /* missing type support */
    }

    #[test]
    fn test_xml_buffer_add() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let buf = gen_xml_buffer_ptr(n_buf, 0);
                        let str = gen_const_xml_char_ptr(n_str, 1);
                        let mut len = gen_int(n_len, 2);
                        if !str.is_null() && len > xml_strlen(str) {
                            len = 0;
                        }

                        let ret_val = xml_buffer_add(buf, str, len);
                        desret_int(ret_val);
                        des_xml_buffer_ptr(n_buf, buf, 0);
                        des_const_xml_char_ptr(n_str, str, 1);
                        des_int(n_len, len, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlBufferAdd",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlBufferAdd()");
                            eprint!(" {}", n_buf);
                            eprint!(" {}", n_str);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_add_head() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let buf = gen_xml_buffer_ptr(n_buf, 0);
                        let str = gen_const_xml_char_ptr(n_str, 1);
                        let mut len = gen_int(n_len, 2);
                        if !str.is_null() && len > xml_strlen(str) {
                            len = 0;
                        }

                        let ret_val = xml_buffer_add_head(buf, str, len);
                        desret_int(ret_val);
                        des_xml_buffer_ptr(n_buf, buf, 0);
                        des_const_xml_char_ptr(n_str, str, 1);
                        des_int(n_len, len, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlBufferAddHead",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlBufferAddHead()");
                            eprint!(" {}", n_buf);
                            eprint!(" {}", n_str);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_ccat() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_str in 0..GEN_NB_CONST_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_xml_buffer_ptr(n_buf, 0);
                    let str = gen_const_char_ptr(n_str, 1);

                    let ret_val = xml_buffer_ccat(buf, str);
                    desret_int(ret_val);
                    des_xml_buffer_ptr(n_buf, buf, 0);
                    des_const_char_ptr(n_str, str, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlBufferCCat",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlBufferCCat()");
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_str);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_cat() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_xml_buffer_ptr(n_buf, 0);
                    let str = gen_const_xml_char_ptr(n_str, 1);

                    let ret_val = xml_buffer_cat(buf, str);
                    desret_int(ret_val);
                    des_xml_buffer_ptr(n_buf, buf, 0);
                    des_const_xml_char_ptr(n_str, str, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlBufferCat",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlBufferCat()");
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_str);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_content() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_CONST_XML_BUFFER_PTR {
                let mem_base = xml_mem_blocks();
                let buf = gen_const_xml_buffer_ptr(n_buf, 0);

                let ret_val = xml_buffer_content(buf);
                desret_const_xml_char_ptr(ret_val);
                des_const_xml_buffer_ptr(n_buf, buf, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlBufferContent",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlBufferContent()");
                    eprintln!(" {}", n_buf);
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_create() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            let ret_val = xml_buffer_create();
            desret_xml_buffer_ptr(ret_val);
            xmlResetLastError();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlBufferCreate",
                    xml_mem_blocks() - mem_base
                );
                assert!(leaks == 0, "{leaks} Leaks are found in xmlBufferCreate()");
            }
        }
    }

    #[test]
    fn test_xml_buffer_create_size() {

        /* missing type support */
    }

    #[test]
    fn test_xml_buffer_create_static() {

        /* missing type support */
    }

    #[test]
    fn test_xml_buffer_detach() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                let mem_base = xml_mem_blocks();
                let buf = gen_xml_buffer_ptr(n_buf, 0);

                let ret_val = xml_buffer_detach(buf);
                desret_xml_char_ptr(ret_val);
                des_xml_buffer_ptr(n_buf, buf, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlBufferDetach",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlBufferDetach()");
                    eprintln!(" {}", n_buf);
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_empty() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                let mem_base = xml_mem_blocks();
                let buf = gen_xml_buffer_ptr(n_buf, 0);

                xml_buffer_empty(buf);
                des_xml_buffer_ptr(n_buf, buf, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlBufferEmpty",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlBufferEmpty()");
                    eprintln!(" {}", n_buf);
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_grow() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_len in 0..GEN_NB_UNSIGNED_INT {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_xml_buffer_ptr(n_buf, 0);
                    let len = gen_unsigned_int(n_len, 1);

                    let ret_val = xml_buffer_grow(buf, len);
                    desret_int(ret_val);
                    des_xml_buffer_ptr(n_buf, buf, 0);
                    des_unsigned_int(n_len, len, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlBufferGrow",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlBufferGrow()");
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_len);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_length() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_CONST_XML_BUFFER_PTR {
                let mem_base = xml_mem_blocks();
                let buf = gen_const_xml_buffer_ptr(n_buf, 0);

                let ret_val = xml_buffer_length(buf);
                desret_int(ret_val);
                des_const_xml_buffer_ptr(n_buf, buf, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlBufferLength",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlBufferLength()");
                    eprintln!(" {}", n_buf);
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_resize() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_size in 0..GEN_NB_UNSIGNED_INT {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_xml_buffer_ptr(n_buf, 0);
                    let size = gen_unsigned_int(n_size, 1);

                    let ret_val = xml_buffer_resize(buf, size);
                    desret_int(ret_val);
                    des_xml_buffer_ptr(n_buf, buf, 0);
                    des_unsigned_int(n_size, size, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlBufferResize",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlBufferResize()");
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_size);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_set_allocation_scheme() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_scheme in 0..GEN_NB_XML_BUFFER_ALLOCATION_SCHEME {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_xml_buffer_ptr(n_buf, 0);
                    let scheme = gen_xml_buffer_allocation_scheme(n_scheme, 1);

                    xml_buffer_set_allocation_scheme(buf, scheme);
                    des_xml_buffer_ptr(n_buf, buf, 0);
                    des_xml_buffer_allocation_scheme(n_scheme, scheme, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlBufferSetAllocationScheme",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlBufferSetAllocationScheme()"
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_scheme);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_shrink() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_len in 0..GEN_NB_UNSIGNED_INT {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_xml_buffer_ptr(n_buf, 0);
                    let len = gen_unsigned_int(n_len, 1);

                    let ret_val = xml_buffer_shrink(buf, len);
                    desret_int(ret_val);
                    des_xml_buffer_ptr(n_buf, buf, 0);
                    des_unsigned_int(n_len, len, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlBufferShrink",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlBufferShrink()");
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_len);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_write_char() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_string in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_xml_buffer_ptr(n_buf, 0);
                    let string = gen_const_xml_char_ptr(n_string, 1);

                    xml_buffer_write_xml_char(buf, string);
                    des_xml_buffer_ptr(n_buf, buf, 0);
                    des_const_xml_char_ptr(n_string, string, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlBufferWriteCHAR",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlBufferWriteCHAR()"
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_string);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_write_ichar() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_string in 0..GEN_NB_CONST_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_xml_buffer_ptr(n_buf, 0);
                    let string = gen_const_char_ptr(n_string, 1);

                    xml_buffer_write_char(buf, string);
                    des_xml_buffer_ptr(n_buf, buf, 0);
                    des_const_char_ptr(n_string, string, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlBufferWriteChar",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlBufferWriteChar()"
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_string);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_buffer_write_quoted_string() {
        unsafe {
            let mut leaks = 0;
            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_string in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_xml_buffer_ptr(n_buf, 0);
                    let string = gen_const_xml_char_ptr(n_string, 1);

                    xml_buffer_write_quoted_string(buf, string);
                    des_xml_buffer_ptr(n_buf, buf, 0);
                    des_const_xml_char_ptr(n_string, string, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlBufferWriteQuotedString",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlBufferWriteQuotedString()"
                        );
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_string);
                    }
                }
            }
        }
    }

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
                            xmlResetLastError();
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
                xmlResetLastError();
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
                    xmlResetLastError();
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
                xmlResetLastError();
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
                xmlResetLastError();
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
                xmlResetLastError();
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
                    xmlResetLastError();
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
                xmlResetLastError();
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
                    xmlResetLastError();
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
                    xmlResetLastError();
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
                            xmlResetLastError();
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
    fn test_xml_domwrap_adopt_node() {
        unsafe {
            let mut leaks = 0;
            for n_ctxt in 0..GEN_NB_XML_DOMWRAP_CTXT_PTR {
                for n_source_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_dest_doc in 0..GEN_NB_XML_DOC_PTR {
                            for n_dest_parent in 0..GEN_NB_XML_NODE_PTR {
                                for n_options in 0..GEN_NB_INT {
                                    let mem_base = xml_mem_blocks();
                                    let ctxt = gen_xml_domwrap_ctxt_ptr(n_ctxt, 0);
                                    let source_doc = gen_xml_doc_ptr(n_source_doc, 1);
                                    let mut node = gen_xml_node_ptr(n_node, 2);
                                    let dest_doc = gen_xml_doc_ptr(n_dest_doc, 3);
                                    let dest_parent = gen_xml_node_ptr(n_dest_parent, 4);
                                    let options = gen_int(n_options, 5);

                                    let ret_val = xml_dom_wrap_adopt_node(
                                        ctxt,
                                        source_doc,
                                        node,
                                        dest_doc,
                                        dest_parent,
                                        options,
                                    );
                                    if !node.is_null() && (*node).parent.is_null() {
                                        xml_unlink_node(node);
                                        xml_free_node(node);
                                        node = null_mut();
                                    }
                                    desret_int(ret_val);
                                    des_xml_domwrap_ctxt_ptr(n_ctxt, ctxt, 0);
                                    des_xml_doc_ptr(n_source_doc, source_doc, 1);
                                    des_xml_node_ptr(n_node, node, 2);
                                    des_xml_doc_ptr(n_dest_doc, dest_doc, 3);
                                    des_xml_node_ptr(n_dest_parent, dest_parent, 4);
                                    des_int(n_options, options, 5);
                                    xmlResetLastError();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlDOMWrapAdoptNode",
                                            xml_mem_blocks() - mem_base
                                        );
                                        assert!(
                                            leaks == 0,
                                            "{leaks} Leaks are found in xmlDOMWrapAdoptNode()"
                                        );
                                        eprint!(" {}", n_ctxt);
                                        eprint!(" {}", n_source_doc);
                                        eprint!(" {}", n_node);
                                        eprint!(" {}", n_dest_doc);
                                        eprint!(" {}", n_dest_parent);
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
                                            xmlResetLastError();
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
                        xmlResetLastError();
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
                            xmlResetLastError();
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
                        xmlResetLastError();
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
                    xmlResetLastError();
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
    fn test_xml_doc_dump() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_f in 0..GEN_NB_FILE_PTR {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let f = gen_file_ptr(n_f, 0);
                    let cur = gen_xml_doc_ptr(n_cur, 1);

                    let ret_val = xml_doc_dump(f, cur);
                    desret_int(ret_val);
                    des_file_ptr(n_f, f, 0);
                    des_xml_doc_ptr(n_cur, cur, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDocDump",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlDocDump()");
                        eprint!(" {}", n_f);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_doc_dump_format_memory() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_DOC_PTR {
                for n_mem in 0..GEN_NB_XML_CHAR_PTR_PTR {
                    for n_size in 0..GEN_NB_INT_PTR {
                        for n_format in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let cur = gen_xml_doc_ptr(n_cur, 0);
                            let mem = gen_xml_char_ptr_ptr(n_mem, 1);
                            let size = gen_int_ptr(n_size, 2);
                            let format = gen_int(n_format, 3);

                            xml_doc_dump_format_memory(cur, mem, size, format);
                            des_xml_doc_ptr(n_cur, cur, 0);
                            des_xml_char_ptr_ptr(n_mem, mem, 1);
                            des_int_ptr(n_size, size, 2);
                            des_int(n_format, format, 3);
                            xmlResetLastError();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlDocDumpFormatMemory",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlDocDumpFormatMemory()"
                                );
                                eprint!(" {}", n_cur);
                                eprint!(" {}", n_mem);
                                eprint!(" {}", n_size);
                                eprintln!(" {}", n_format);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_doc_dump_format_memory_enc() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_out_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_doc_txt_ptr in 0..GEN_NB_XML_CHAR_PTR_PTR {
                    for n_doc_txt_len in 0..GEN_NB_INT_PTR {
                        for n_txt_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                            for n_format in 0..GEN_NB_INT {
                                let mem_base = xml_mem_blocks();
                                let out_doc = gen_xml_doc_ptr(n_out_doc, 0);
                                let doc_txt_ptr = gen_xml_char_ptr_ptr(n_doc_txt_ptr, 1);
                                let doc_txt_len = gen_int_ptr(n_doc_txt_len, 2);
                                let txt_encoding = gen_const_char_ptr(n_txt_encoding, 3);
                                let format = gen_int(n_format, 4);

                                xml_doc_dump_format_memory_enc(
                                    out_doc,
                                    doc_txt_ptr,
                                    doc_txt_len,
                                    txt_encoding,
                                    format,
                                );
                                des_xml_doc_ptr(n_out_doc, out_doc, 0);
                                des_xml_char_ptr_ptr(n_doc_txt_ptr, doc_txt_ptr, 1);
                                des_int_ptr(n_doc_txt_len, doc_txt_len, 2);
                                des_const_char_ptr(n_txt_encoding, txt_encoding, 3);
                                des_int(n_format, format, 4);
                                xmlResetLastError();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlDocDumpFormatMemoryEnc",
                                        xml_mem_blocks() - mem_base
                                    );
                                    assert!(
                                        leaks == 0,
                                        "{leaks} Leaks are found in xmlDocDumpFormatMemoryEnc()"
                                    );
                                    eprint!(" {}", n_out_doc);
                                    eprint!(" {}", n_doc_txt_ptr);
                                    eprint!(" {}", n_doc_txt_len);
                                    eprint!(" {}", n_txt_encoding);
                                    eprintln!(" {}", n_format);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_doc_dump_memory() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_DOC_PTR {
                for n_mem in 0..GEN_NB_XML_CHAR_PTR_PTR {
                    for n_size in 0..GEN_NB_INT_PTR {
                        let mem_base = xml_mem_blocks();
                        let cur = gen_xml_doc_ptr(n_cur, 0);
                        let mem = gen_xml_char_ptr_ptr(n_mem, 1);
                        let size = gen_int_ptr(n_size, 2);

                        xml_doc_dump_memory(cur, mem, size);
                        des_xml_doc_ptr(n_cur, cur, 0);
                        des_xml_char_ptr_ptr(n_mem, mem, 1);
                        des_int_ptr(n_size, size, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDocDumpMemory",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlDocDumpMemory()");
                            eprint!(" {}", n_cur);
                            eprint!(" {}", n_mem);
                            eprintln!(" {}", n_size);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_doc_dump_memory_enc() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_out_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_doc_txt_ptr in 0..GEN_NB_XML_CHAR_PTR_PTR {
                    for n_doc_txt_len in 0..GEN_NB_INT_PTR {
                        for n_txt_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let out_doc = gen_xml_doc_ptr(n_out_doc, 0);
                            let doc_txt_ptr = gen_xml_char_ptr_ptr(n_doc_txt_ptr, 1);
                            let doc_txt_len = gen_int_ptr(n_doc_txt_len, 2);
                            let txt_encoding = gen_const_char_ptr(n_txt_encoding, 3);

                            xml_doc_dump_memory_enc(
                                out_doc,
                                doc_txt_ptr,
                                doc_txt_len,
                                txt_encoding,
                            );
                            des_xml_doc_ptr(n_out_doc, out_doc, 0);
                            des_xml_char_ptr_ptr(n_doc_txt_ptr, doc_txt_ptr, 1);
                            des_int_ptr(n_doc_txt_len, doc_txt_len, 2);
                            des_const_char_ptr(n_txt_encoding, txt_encoding, 3);
                            xmlResetLastError();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlDocDumpMemoryEnc",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlDocDumpMemoryEnc()"
                                );
                                eprint!(" {}", n_out_doc);
                                eprint!(" {}", n_doc_txt_ptr);
                                eprint!(" {}", n_doc_txt_len);
                                eprintln!(" {}", n_txt_encoding);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_doc_format_dump() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_f in 0..GEN_NB_FILE_PTR {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    for n_format in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let f = gen_file_ptr(n_f, 0);
                        let cur = gen_xml_doc_ptr(n_cur, 1);
                        let format = gen_int(n_format, 2);

                        let ret_val = xml_doc_format_dump(f, cur, format);
                        desret_int(ret_val);
                        des_file_ptr(n_f, f, 0);
                        des_xml_doc_ptr(n_cur, cur, 1);
                        des_int(n_format, format, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDocFormatDump",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlDocFormatDump()");
                            eprint!(" {}", n_f);
                            eprint!(" {}", n_cur);
                            eprintln!(" {}", n_format);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_doc_get_root_element() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_CONST_XML_DOC_PTR {
                let mem_base = xml_mem_blocks();
                let doc = gen_const_xml_doc_ptr(n_doc, 0);

                let ret_val = xml_doc_get_root_element(doc);
                desret_xml_node_ptr(ret_val);
                des_const_xml_doc_ptr(n_doc, doc, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlDocGetRootElement",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlDocGetRootElement()"
                    );
                    eprintln!(" {}", n_doc);
                }
            }
        }
    }

    #[test]
    fn test_xml_doc_set_root_element() {
        #[cfg(any(feature = "tree", feature = "writer"))]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_root in 0..GEN_NB_XML_NODE_PTR_IN {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_xml_doc_ptr(n_doc, 0);
                    let mut root = gen_xml_node_ptr_in(n_root, 1);

                    let ret_val = xml_doc_set_root_element(doc, root);
                    if doc.is_null() {
                        xml_free_node(root);
                        root = null_mut();
                    }
                    desret_xml_node_ptr(ret_val);
                    des_xml_doc_ptr(n_doc, doc, 0);
                    des_xml_node_ptr_in(n_root, root, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDocSetRootElement",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDocSetRootElement()"
                        );
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_root);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_elem_dump() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_f in 0..GEN_NB_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_cur in 0..GEN_NB_XML_NODE_PTR {
                        let mem_base = xml_mem_blocks();
                        let f = gen_file_ptr(n_f, 0);
                        let doc = gen_xml_doc_ptr(n_doc, 1);
                        let cur = gen_xml_node_ptr(n_cur, 2);

                        xml_elem_dump(f, doc, cur);
                        des_file_ptr(n_f, f, 0);
                        des_xml_doc_ptr(n_doc, doc, 1);
                        des_xml_node_ptr(n_cur, cur, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlElemDump",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlElemDump()");
                            eprint!(" {}", n_f);
                            eprint!(" {}", n_doc);
                            eprintln!(" {}", n_cur);
                        }
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
                xmlResetLastError();
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
    fn test_xml_get_buffer_allocation_scheme() {
        unsafe {
            let mem_base = xml_mem_blocks();
            let mut leaks = 0;

            let ret_val = xml_get_buffer_allocation_scheme();
            desret_xml_buffer_allocation_scheme(ret_val);
            xmlResetLastError();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlGetBufferAllocationScheme",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlGetBufferAllocationScheme()"
                );
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
            xmlResetLastError();
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
                xmlResetLastError();
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
    fn test_xml_get_int_subset() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_CONST_XML_DOC_PTR {
                let mem_base = xml_mem_blocks();
                let doc = gen_const_xml_doc_ptr(n_doc, 0);

                let ret_val = xml_get_int_subset(doc);
                desret_xml_dtd_ptr(ret_val);
                des_const_xml_doc_ptr(n_doc, doc, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlGetIntSubset",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlGetIntSubset()");
                    eprintln!(" {}", n_doc);
                }
            }
        }
    }

    #[test]
    fn test_xml_get_last_child() {
        unsafe {
            let mut leaks = 0;
            for n_parent in 0..GEN_NB_CONST_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let parent = gen_const_xml_node_ptr(n_parent, 0);

                let ret_val = xml_get_last_child(parent);
                desret_xml_node_ptr(ret_val);
                des_const_xml_node_ptr(n_parent, parent, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlGetLastChild",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlGetLastChild()");
                    eprintln!(" {}", n_parent);
                }
            }
        }
    }

    #[test]
    fn test_xml_get_line_no() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_CONST_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let node = gen_const_xml_node_ptr(n_node, 0);

                let ret_val = xml_get_line_no(node);
                desret_long(ret_val);
                des_const_xml_node_ptr(n_node, node, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlGetLineNo",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlGetLineNo()");
                    eprintln!(" {}", n_node);
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
                    xmlResetLastError();
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
    fn test_xml_get_node_path() {
        #[cfg(any(feature = "tree", feature = "libxml_debug"))]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_CONST_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let node = gen_const_xml_node_ptr(n_node, 0);

                let ret_val = xml_get_node_path(node);
                desret_xml_char_ptr(ret_val);
                des_const_xml_node_ptr(n_node, node, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlGetNodePath",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlGetNodePath()");
                    eprintln!(" {}", n_node);
                }
            }
        }
    }

    #[test]
    fn test_xml_get_ns_list() {

        /* missing type support */
    }

    #[test]
    fn test_xml_get_ns_prop() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_CONST_XML_NODE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name_space in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let node = gen_const_xml_node_ptr(n_node, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let name_space = gen_const_xml_char_ptr(n_name_space, 2);

                        let ret_val = xml_get_ns_prop(node, name, name_space);
                        desret_xml_char_ptr(ret_val);
                        des_const_xml_node_ptr(n_node, node, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_name_space, name_space, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlGetNsProp",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlGetNsProp()");
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
    fn test_xml_get_prop() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_CONST_XML_NODE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let node = gen_const_xml_node_ptr(n_node, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_get_prop(node, name);
                    desret_xml_char_ptr(ret_val);
                    des_const_xml_node_ptr(n_node, node, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlGetProp",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlGetProp()");
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
                        xmlResetLastError();
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
    fn test_xml_has_prop() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_CONST_XML_NODE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let node = gen_const_xml_node_ptr(n_node, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_has_prop(node, name);
                    desret_xml_attr_ptr(ret_val);
                    des_const_xml_node_ptr(n_node, node, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlHasProp",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlHasProp()");
                        eprint!(" {}", n_node);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_is_blank_node() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_CONST_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let node = gen_const_xml_node_ptr(n_node, 0);

                let ret_val = xml_is_blank_node(node);
                desret_int(ret_val);
                des_const_xml_node_ptr(n_node, node, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlIsBlankNode",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlIsBlankNode()");
                    eprintln!(" {}", n_node);
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
                    xmlResetLastError();
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
                xmlResetLastError();
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
                        xmlResetLastError();
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
                    xmlResetLastError();
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
                                xmlResetLastError();
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
                xmlResetLastError();
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
    fn test_xml_new_doc() {
        unsafe {
            let mut leaks = 0;
            for n_version in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let version = gen_const_xml_char_ptr(n_version, 0);

                let ret_val = xml_new_doc(version as *const XmlChar);
                desret_xml_doc_ptr(ret_val);
                des_const_xml_char_ptr(n_version, version, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNewDoc",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlNewDoc()");
                    eprintln!(" {}", n_version);
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
                    xmlResetLastError();
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
                xmlResetLastError();
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
                            xmlResetLastError();
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
                            xmlResetLastError();
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
                        xmlResetLastError();
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
                        xmlResetLastError();
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
                            xmlResetLastError();
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
                    xmlResetLastError();
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
                        xmlResetLastError();
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
                            xmlResetLastError();
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
                    xmlResetLastError();
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
                    xmlResetLastError();
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
                        xmlResetLastError();
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
                            xmlResetLastError();
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
                            xmlResetLastError();
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
                    xmlResetLastError();
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
                            xmlResetLastError();
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
                    xmlResetLastError();
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
                xmlResetLastError();
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
                            xmlResetLastError();
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
                    xmlResetLastError();
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
                xmlResetLastError();
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
    fn test_xml_node_add_content() {
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_XML_NODE_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_ptr(n_cur, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    xml_node_add_content(cur, content);
                    des_xml_node_ptr(n_cur, cur, 0);
                    des_const_xml_char_ptr(n_content, content, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNodeAddContent",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNodeAddContent()");
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_content);
                    }
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
                        xmlResetLastError();
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
    fn test_xml_node_buf_get_content() {
        unsafe {
            let mut leaks = 0;
            for n_buffer in 0..GEN_NB_XML_BUFFER_PTR {
                for n_cur in 0..GEN_NB_CONST_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let buffer = gen_xml_buffer_ptr(n_buffer, 0);
                    let cur = gen_const_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_node_buf_get_content(buffer, cur);
                    desret_int(ret_val);
                    des_xml_buffer_ptr(n_buffer, buffer, 0);
                    des_const_xml_node_ptr(n_cur, cur, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNodeBufGetContent",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlNodeBufGetContent()"
                        );
                        eprint!(" {}", n_buffer);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_node_dump() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_cur in 0..GEN_NB_XML_NODE_PTR {
                        for n_level in 0..GEN_NB_INT {
                            for n_format in 0..GEN_NB_INT {
                                let mem_base = xml_mem_blocks();
                                let buf = gen_xml_buffer_ptr(n_buf, 0);
                                let doc = gen_xml_doc_ptr(n_doc, 1);
                                let cur = gen_xml_node_ptr(n_cur, 2);
                                let level = gen_int(n_level, 3);
                                let format = gen_int(n_format, 4);

                                let ret_val = xml_node_dump(buf, doc, cur, level, format);
                                desret_int(ret_val);
                                des_xml_buffer_ptr(n_buf, buf, 0);
                                des_xml_doc_ptr(n_doc, doc, 1);
                                des_xml_node_ptr(n_cur, cur, 2);
                                des_int(n_level, level, 3);
                                des_int(n_format, format, 4);
                                xmlResetLastError();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlNodeDump",
                                        xml_mem_blocks() - mem_base
                                    );
                                    assert!(leaks == 0, "{leaks} Leaks are found in xmlNodeDump()");
                                    eprint!(" {}", n_buf);
                                    eprint!(" {}", n_doc);
                                    eprint!(" {}", n_cur);
                                    eprint!(" {}", n_level);
                                    eprintln!(" {}", n_format);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_node_dump_output() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    for n_cur in 0..GEN_NB_XML_NODE_PTR {
                        for n_level in 0..GEN_NB_INT {
                            for n_format in 0..GEN_NB_INT {
                                for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                                    let mem_base = xml_mem_blocks();
                                    let buf = gen_xml_output_buffer_ptr(n_buf, 0);
                                    let doc = gen_xml_doc_ptr(n_doc, 1);
                                    let cur = gen_xml_node_ptr(n_cur, 2);
                                    let level = gen_int(n_level, 3);
                                    let format = gen_int(n_format, 4);
                                    let encoding = gen_const_char_ptr(n_encoding, 5);

                                    xml_node_dump_output(buf, doc, cur, level, format, encoding);
                                    des_xml_output_buffer_ptr(n_buf, buf, 0);
                                    des_xml_doc_ptr(n_doc, doc, 1);
                                    des_xml_node_ptr(n_cur, cur, 2);
                                    des_int(n_level, level, 3);
                                    des_int(n_format, format, 4);
                                    des_const_char_ptr(n_encoding, encoding, 5);
                                    xmlResetLastError();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlNodeDumpOutput",
                                            xml_mem_blocks() - mem_base
                                        );
                                        assert!(
                                            leaks == 0,
                                            "{leaks} Leaks are found in xmlNodeDumpOutput()"
                                        );
                                        eprint!(" {}", n_buf);
                                        eprint!(" {}", n_doc);
                                        eprint!(" {}", n_cur);
                                        eprint!(" {}", n_level);
                                        eprint!(" {}", n_format);
                                        eprintln!(" {}", n_encoding);
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
    fn test_xml_node_get_base() {
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_CONST_XML_DOC_PTR {
                for n_cur in 0..GEN_NB_CONST_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_const_xml_doc_ptr(n_doc, 0);
                    let cur = gen_const_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_node_get_base(doc, cur);
                    desret_xml_char_ptr(ret_val);
                    des_const_xml_doc_ptr(n_doc, doc, 0);
                    des_const_xml_node_ptr(n_cur, cur, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNodeGetBase",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNodeGetBase()");
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_node_get_content() {
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_CONST_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let cur = gen_const_xml_node_ptr(n_cur, 0);

                let ret_val = xml_node_get_content(cur);
                desret_xml_char_ptr(ret_val);
                des_const_xml_node_ptr(n_cur, cur, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNodeGetContent",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlNodeGetContent()");
                    eprintln!(" {}", n_cur);
                }
            }
        }
    }

    #[test]
    fn test_xml_node_get_lang() {
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_CONST_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let cur = gen_const_xml_node_ptr(n_cur, 0);

                let ret_val = xml_node_get_lang(cur);
                desret_xml_char_ptr(ret_val);
                des_const_xml_node_ptr(n_cur, cur, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNodeGetLang",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlNodeGetLang()");
                    eprintln!(" {}", n_cur);
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
                xmlResetLastError();
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
    fn test_xml_node_is_text() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_CONST_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let node = gen_const_xml_node_ptr(n_node, 0);

                let ret_val = xml_node_is_text(node);
                desret_int(ret_val);
                des_const_xml_node_ptr(n_node, node, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNodeIsText",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlNodeIsText()");
                    eprintln!(" {}", n_node);
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
                        xmlResetLastError();
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
                        xmlResetLastError();
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
    fn test_xml_node_set_base() {
        #[cfg(any(feature = "tree", feature = "xinclude"))]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_NODE_PTR {
                for n_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_ptr(n_cur, 0);
                    let uri = gen_const_xml_char_ptr(n_uri, 1);

                    xml_node_set_base(cur, uri);
                    des_xml_node_ptr(n_cur, cur, 0);
                    des_const_xml_char_ptr(n_uri, uri, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNodeSetBase",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNodeSetBase()");
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_uri);
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
                    xmlResetLastError();
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
                        xmlResetLastError();
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
                    xmlResetLastError();
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
    fn test_xml_node_set_name() {
        #[cfg(feature = "tree")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_NODE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_ptr(n_cur, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    xml_node_set_name(cur, name);
                    des_xml_node_ptr(n_cur, cur, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNodeSetName",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlNodeSetName()");
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_name);
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
                    xmlResetLastError();
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
                xmlResetLastError();
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
                    xmlResetLastError();
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
                xmlResetLastError();
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
    fn test_xml_replace_node() {
        #[cfg(any(feature = "tree", feature = "writer"))]
        unsafe {
            let mut leaks = 0;

            for n_old in 0..GEN_NB_XML_NODE_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR_IN {
                    let mem_base = xml_mem_blocks();
                    let mut old = gen_xml_node_ptr(n_old, 0);
                    let mut cur = gen_xml_node_ptr_in(n_cur, 1);

                    xml_replace_node(old, cur);
                    if !cur.is_null() {
                        xml_unlink_node(cur);
                        xml_free_node(cur);
                        cur = null_mut();
                    }
                    if !old.is_null() {
                        xml_unlink_node(old);
                        xml_free_node(old);
                        old = null_mut();
                    }
                    // desret_xml_node_ptr(ret_val);
                    des_xml_node_ptr(n_old, old, 0);
                    des_xml_node_ptr_in(n_cur, cur, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlReplaceNode",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlReplaceNode()");
                        eprint!(" {}", n_old);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_save_file() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEOUTPUT {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let filename = gen_fileoutput(n_filename, 0);
                    let cur = gen_xml_doc_ptr(n_cur, 1);

                    let ret_val = xml_save_file(filename, cur);
                    desret_int(ret_val);
                    des_fileoutput(n_filename, filename, 0);
                    des_xml_doc_ptr(n_cur, cur, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSaveFile",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveFile()");
                        eprint!(" {}", n_filename);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_save_file_enc() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEOUTPUT {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let filename = gen_fileoutput(n_filename, 0);
                        let cur = gen_xml_doc_ptr(n_cur, 1);
                        let encoding = gen_const_char_ptr(n_encoding, 2);

                        let ret_val = xml_save_file_enc(filename, cur, encoding);
                        desret_int(ret_val);
                        des_fileoutput(n_filename, filename, 0);
                        des_xml_doc_ptr(n_cur, cur, 1);
                        des_const_char_ptr(n_encoding, encoding, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSaveFileEnc",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveFileEnc()");
                            eprint!(" {}", n_filename);
                            eprint!(" {}", n_cur);
                            eprintln!(" {}", n_encoding);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_save_file_to() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let mut buf = gen_xml_output_buffer_ptr(n_buf, 0);
                        let cur = gen_xml_doc_ptr(n_cur, 1);
                        let encoding = gen_const_char_ptr(n_encoding, 2);

                        let ret_val = xml_save_file_to(buf, cur, encoding);
                        buf = null_mut();
                        desret_int(ret_val);
                        des_xml_output_buffer_ptr(n_buf, buf, 0);
                        des_xml_doc_ptr(n_cur, cur, 1);
                        des_const_char_ptr(n_encoding, encoding, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSaveFileTo",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveFileTo()");
                            eprint!(" {}", n_buf);
                            eprint!(" {}", n_cur);
                            eprintln!(" {}", n_encoding);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_save_format_file() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEOUTPUT {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    for n_format in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let filename = gen_fileoutput(n_filename, 0);
                        let cur = gen_xml_doc_ptr(n_cur, 1);
                        let format = gen_int(n_format, 2);

                        let ret_val = xml_save_format_file(filename, cur, format);
                        desret_int(ret_val);
                        des_fileoutput(n_filename, filename, 0);
                        des_xml_doc_ptr(n_cur, cur, 1);
                        des_int(n_format, format, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSaveFormatFile",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveFormatFile()");
                            eprint!(" {}", n_filename);
                            eprint!(" {}", n_cur);
                            eprintln!(" {}", n_format);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_save_format_file_enc() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEOUTPUT {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                        for n_format in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let filename = gen_fileoutput(n_filename, 0);
                            let cur = gen_xml_doc_ptr(n_cur, 1);
                            let encoding = gen_const_char_ptr(n_encoding, 2);
                            let format = gen_int(n_format, 3);

                            let ret_val = xml_save_format_file_enc(filename, cur, encoding, format);
                            desret_int(ret_val);
                            des_fileoutput(n_filename, filename, 0);
                            des_xml_doc_ptr(n_cur, cur, 1);
                            des_const_char_ptr(n_encoding, encoding, 2);
                            des_int(n_format, format, 3);
                            xmlResetLastError();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSaveFormatFileEnc",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlSaveFormatFileEnc()"
                                );
                                eprint!(" {}", n_filename);
                                eprint!(" {}", n_cur);
                                eprint!(" {}", n_encoding);
                                eprintln!(" {}", n_format);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_save_format_file_to() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
                for n_cur in 0..GEN_NB_XML_DOC_PTR {
                    for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                        for n_format in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let mut buf = gen_xml_output_buffer_ptr(n_buf, 0);
                            let cur = gen_xml_doc_ptr(n_cur, 1);
                            let encoding = gen_const_char_ptr(n_encoding, 2);
                            let format = gen_int(n_format, 3);

                            let ret_val = xml_save_format_file_to(buf, cur, encoding, format);
                            buf = null_mut();
                            desret_int(ret_val);
                            des_xml_output_buffer_ptr(n_buf, buf, 0);
                            des_xml_doc_ptr(n_cur, cur, 1);
                            des_const_char_ptr(n_encoding, encoding, 2);
                            des_int(n_format, format, 3);
                            xmlResetLastError();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSaveFormatFileTo",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlSaveFormatFileTo()"
                                );
                                eprint!(" {}", n_buf);
                                eprint!(" {}", n_cur);
                                eprint!(" {}", n_encoding);
                                eprintln!(" {}", n_format);
                            }
                        }
                    }
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
                        xmlResetLastError();
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
                        xmlResetLastError();
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
    fn test_xml_set_buffer_allocation_scheme() {
        unsafe {
            let mut leaks = 0;
            for n_scheme in 0..GEN_NB_XML_BUFFER_ALLOCATION_SCHEME {
                let mem_base = xml_mem_blocks();
                let scheme = gen_xml_buffer_allocation_scheme(n_scheme, 0);

                xml_set_buffer_allocation_scheme(scheme);
                des_xml_buffer_allocation_scheme(n_scheme, scheme, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSetBufferAllocationScheme",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlSetBufferAllocationScheme()"
                    );
                    eprintln!(" {}", n_scheme);
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
                xmlResetLastError();
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
                    xmlResetLastError();
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
                    xmlResetLastError();
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
    fn test_xml_set_ns_prop() {
        #[cfg(any(
            feature = "tree",
            feature = "xinclude",
            feature = "schema",
            feature = "html"
        ))]
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

                            let ret_val = xml_set_ns_prop(node, ns, name, value);
                            desret_xml_attr_ptr(ret_val);
                            des_xml_node_ptr(n_node, node, 0);
                            des_xml_ns_ptr(n_ns, ns, 1);
                            des_const_xml_char_ptr(n_name, name, 2);
                            des_const_xml_char_ptr(n_value, value, 3);
                            xmlResetLastError();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSetNsProp",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlSetNsProp()");
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
    fn test_xml_set_prop() {
        #[cfg(any(
            feature = "tree",
            feature = "xinclude",
            feature = "schema",
            feature = "html"
        ))]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let node = gen_xml_node_ptr(n_node, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let value = gen_const_xml_char_ptr(n_value, 2);

                        let ret_val = xml_set_prop(node, name, value);
                        desret_xml_attr_ptr(ret_val);
                        des_xml_node_ptr(n_node, node, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_value, value, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSetProp",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlSetProp()");
                            eprint!(" {}", n_node);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_value);
                        }
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
                    xmlResetLastError();
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
                    xmlResetLastError();
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
                    xmlResetLastError();
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
                        xmlResetLastError();
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
                        xmlResetLastError();
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
    fn test_xml_text_merge() {
        unsafe {
            let mut leaks = 0;

            for n_first in 0..GEN_NB_XML_NODE_PTR_IN {
                for n_second in 0..GEN_NB_XML_NODE_PTR_IN {
                    let mem_base = xml_mem_blocks();
                    let first = gen_xml_node_ptr_in(n_first, 0);
                    let mut second = gen_xml_node_ptr_in(n_second, 1);

                    let ret_val = xml_text_merge(first, second);
                    if !first.is_null() && (*first).typ != XmlElementType::XmlTextNode {
                        xml_unlink_node(second);
                        xml_free_node(second);
                        second = null_mut();
                    }
                    desret_xml_node_ptr(ret_val);
                    des_xml_node_ptr_in(n_first, first, 0);
                    des_xml_node_ptr_in(n_second, second, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextMerge",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlTextMerge()");
                        eprint!(" {}", n_first);
                        eprintln!(" {}", n_second);
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
                        xmlResetLastError();
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
                    xmlResetLastError();
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
                        xmlResetLastError();
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
                        xmlResetLastError();
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
                        xmlResetLastError();
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
                        xmlResetLastError();
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

use std::os::raw::c_void;

use crate::{
    libxml::{xmlregexp::XmlRegexpPtr, xmlstring::XmlChar},
    tree::{XmlDoc, XmlDtd, XmlElementContentPtr, XmlElementType, XmlElementTypeVal, XmlNode},
};

use super::XmlAttributePtr;

/// An XML Element declaration from a DTD.
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

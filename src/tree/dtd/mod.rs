mod attribute;
mod element;
mod enumeration;
mod notation;

use std::os::raw::c_void;

pub use attribute::*;
pub use element::*;
pub use enumeration::*;
pub use notation::*;

use crate::libxml::xmlstring::XmlChar;

use super::{XmlDoc, XmlElementType, XmlNode};

/// An XML DTD, as defined by <!DOCTYPE ... There is actually one for
/// the internal subset and for the external subset.
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

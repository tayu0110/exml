use std::os::raw::c_void;

use crate::libxml::xmlstring::XmlChar;

use super::{XmlAttr, XmlDoc, XmlElementType, XmlNs};

/// A node in an XML tree.
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
    pub(crate) line: u16,      /* line number */
    pub(crate) extra: u16,     /* extra data for XPath/XSLT */
}

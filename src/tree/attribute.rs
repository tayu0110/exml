use std::os::raw::c_void;

use crate::libxml::xmlstring::XmlChar;

use super::{NodeCommon, XmlAttributeType, XmlDoc, XmlElementType, XmlNode, XmlNs};

/// An attribute on an XML node.
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

impl NodeCommon for XmlAttr {
    fn document(&self) -> *mut XmlDoc {
        self.doc
    }
    fn element_type(&self) -> XmlElementType {
        self.typ
    }
    fn name(&self) -> *const u8 {
        self.name
    }
    fn children(&self) -> *mut XmlNode {
        self.children
    }
    fn set_children(&mut self, children: *mut XmlNode) {
        self.children = children
    }
    fn last(&self) -> *mut XmlNode {
        self.last
    }
    fn set_last(&mut self, last: *mut XmlNode) {
        self.last = last;
    }
    fn next(&self) -> *mut XmlNode {
        self.next as *mut XmlNode
    }
    fn set_next(&mut self, next: *mut XmlNode) {
        self.next = next as *mut XmlAttr;
    }
    fn prev(&self) -> *mut XmlNode {
        self.prev as *mut XmlNode
    }
    fn set_prev(&mut self, prev: *mut XmlNode) {
        self.prev = prev as *mut XmlAttr;
    }
    fn parent(&self) -> *mut XmlNode {
        self.parent
    }
    fn set_parent(&mut self, parent: *mut XmlNode) {
        self.parent = parent;
    }
}

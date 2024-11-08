use std::os::raw::c_void;

use crate::{
    libxml::xmlstring::XmlChar,
    tree::{
        NodeCommon, XmlAttributeDefault, XmlAttributeType, XmlDoc, XmlDtd, XmlElementType,
        XmlEnumerationPtr, XmlNode,
    },
};

/// An Attribute declaration in a DTD.
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

impl NodeCommon for XmlAttribute {
    fn document(&self) -> *mut XmlDoc {
        self.doc
    }
    fn element_type(&self) -> XmlElementType {
        self.typ
    }
    fn name(&self) -> *const u8 {
        self.name
    }
    fn next(&self) -> *mut XmlNode {
        self.next
    }
    fn set_next(&mut self, next: *mut XmlNode) {
        self.next = next;
    }
    fn prev(&self) -> *mut XmlNode {
        self.prev
    }
    fn set_prev(&mut self, prev: *mut XmlNode) {
        self.prev = prev;
    }
    fn parent(&self) -> *mut XmlNode {
        self.parent as *mut XmlNode
    }
    fn set_parent(&mut self, parent: *mut XmlNode) {
        self.parent = parent as *mut XmlDtd;
    }
}

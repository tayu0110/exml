use std::{os::raw::c_void, ptr::null_mut};

use crate::{
    libxml::xmlstring::XmlChar,
    tree::{
        NodeCommon, NodePtr, XmlAttributeDefault, XmlAttributeType, XmlDoc, XmlDtd, XmlElementType,
        XmlEnumerationPtr, XmlNode,
    },
};

/// An Attribute declaration in a DTD.
pub type XmlAttributePtr = *mut XmlAttribute;
#[repr(C)]
#[derive(Clone, Copy)]
pub struct XmlAttribute {
    pub(crate) _private: *mut c_void,     /* application data */
    pub(crate) typ: XmlElementType,       /* XML_ATTRIBUTE_DECL, must be second ! */
    pub(crate) name: *const XmlChar,      /* Attribute name */
    pub(crate) children: Option<NodePtr>, /* NULL */
    pub(crate) last: Option<NodePtr>,     /* NULL */
    pub(crate) parent: *mut XmlDtd,       /* -> DTD */
    pub(crate) next: Option<NodePtr>,     /* next sibling link  */
    pub(crate) prev: Option<NodePtr>,     /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,          /* the containing document */

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
    fn children(&self) -> Option<NodePtr> {
        self.children
    }
    fn set_children(&mut self, children: Option<NodePtr>) {
        self.children = children;
    }
    fn last(&self) -> Option<NodePtr> {
        self.last
    }
    fn set_last(&mut self, last: Option<NodePtr>) {
        self.last = last;
    }
    fn next(&self) -> Option<NodePtr> {
        self.next
    }
    fn set_next(&mut self, next: Option<NodePtr>) {
        self.next = next;
    }
    fn prev(&self) -> Option<NodePtr> {
        self.prev
    }
    fn set_prev(&mut self, prev: Option<NodePtr>) {
        self.prev = prev;
    }
    fn parent(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.parent as *mut XmlNode)
    }
    fn set_parent(&mut self, parent: Option<NodePtr>) {
        self.parent = parent.map_or(null_mut(), |p| p.as_ptr()) as *mut XmlDtd;
    }
}

use std::{os::raw::c_void, ptr::null_mut};

use crate::{
    libxml::{xmlregexp::XmlRegexpPtr, xmlstring::XmlChar},
    tree::{
        NodeCommon, NodePtr, XmlDoc, XmlDtd, XmlElementContentPtr, XmlElementType,
        XmlElementTypeVal, XmlNode,
    },
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

impl NodeCommon for XmlElement {
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
        NodePtr::from_ptr(self.children)
    }
    fn set_children(&mut self, children: Option<NodePtr>) {
        self.children = children.map_or(null_mut(), |c| c.as_ptr())
    }
    fn last(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.last)
    }
    fn set_last(&mut self, last: Option<NodePtr>) {
        self.last = last.map_or(null_mut(), |l| l.as_ptr());
    }
    fn next(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.next)
    }
    fn set_next(&mut self, next: Option<NodePtr>) {
        self.next = next.map_or(null_mut(), |n| n.as_ptr());
    }
    fn prev(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.prev)
    }
    fn set_prev(&mut self, prev: Option<NodePtr>) {
        self.prev = prev.map_or(null_mut(), |p| p.as_ptr());
    }
    fn parent(&self) -> Option<NodePtr> {
        NodePtr::from_ptr(self.parent as *mut XmlNode)
    }
    fn set_parent(&mut self, parent: Option<NodePtr>) {
        self.parent = parent.map_or(null_mut(), |p| p.as_ptr()) as *mut XmlDtd;
    }
}

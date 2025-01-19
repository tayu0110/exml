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

use std::{borrow::Cow, os::raw::c_void, ptr::null_mut};

use crate::{
    libxml::xmlregexp::XmlRegexpPtr,
    tree::{
        NodeCommon, NodePtr, XmlDoc, XmlDtd, XmlElementContentPtr, XmlElementType,
        XmlElementTypeVal, XmlNode,
    },
};

use super::XmlAttribute;

#[repr(C)]
pub struct XmlElement {
    pub(crate) _private: *mut c_void, /* application data */
    pub(crate) typ: XmlElementType,   /* XML_ELEMENT_DECL, must be second ! */
    // In current implementation, this field may be used as `XmlNode::name`,
    // so the size of string type must be equal to pointer size.
    // `Option<Box<T>>` is equal to pointer size by NULL pointer optimization if `T` is `Sized`.
    // ref: https://doc.rust-lang.org/std/option/index.html#representation
    #[allow(clippy::box_collection)]
    pub(crate) name: Option<Box<String>>, /* Element name */
    pub(crate) children: Option<NodePtr>, /* NULL */
    pub(crate) last: Option<NodePtr>,     /* NULL */
    pub(crate) parent: *mut XmlDtd,       /* -> DTD */
    pub(crate) next: Option<NodePtr>,     /* next sibling link  */
    pub(crate) prev: Option<NodePtr>,     /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,          /* the containing document */

    pub(crate) etype: XmlElementTypeVal,      /* The type */
    pub(crate) content: XmlElementContentPtr, /* the allowed element content */
    pub(crate) attributes: *mut XmlAttribute, /* List of the declared attributes */
    pub(crate) prefix: Option<String>,        /* the namespace prefix if any */
    #[cfg(feature = "libxml_regexp")]
    pub(crate) cont_model: XmlRegexpPtr, /* the validating regexp */
    #[cfg(not(feature = "libxml_regexp"))]
    pub(crate) cont_model: *mut c_void,
}

impl Default for XmlElement {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::XmlInvalidNode,
            name: None,
            children: None,
            last: None,
            parent: null_mut(),
            next: None,
            prev: None,
            doc: null_mut(),
            etype: XmlElementTypeVal::XmlElementTypeUndefined,
            content: null_mut(),
            attributes: null_mut(),
            prefix: None,
            cont_model: null_mut(),
        }
    }
}

impl NodeCommon for XmlElement {
    fn document(&self) -> *mut XmlDoc {
        self.doc
    }
    fn set_document(&mut self, doc: *mut XmlDoc) {
        self.doc = doc;
    }
    fn element_type(&self) -> XmlElementType {
        self.typ
    }
    fn name(&self) -> Option<Cow<'_, str>> {
        self.name.as_deref().map(|n| Cow::Borrowed(n.as_str()))
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

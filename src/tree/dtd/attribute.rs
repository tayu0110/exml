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

use std::{
    borrow::Cow,
    ffi::CStr,
    os::raw::c_void,
    ptr::{drop_in_place, null_mut},
};

use crate::{
    libxml::{globals::xml_free, xmlstring::XmlChar},
    tree::{
        NodeCommon, NodePtr, XmlAttributeDefault, XmlAttributeType, XmlDoc, XmlDtd, XmlElementType,
        XmlNode,
    },
};

use super::XmlEnumeration;

/// An Attribute declaration in a DTD.
pub type XmlAttributePtr = *mut XmlAttribute;
#[repr(C)]
#[derive(Clone)]
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
    pub(crate) tree: Option<Box<XmlEnumeration>>, /* or the enumeration tree if any */
    pub(crate) prefix: Option<String>,   /* the namespace prefix if any */
    pub(crate) elem: Option<String>,     /* Element holding the attribute */
}

impl Default for XmlAttribute {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::default(),
            name: null_mut(),
            children: None,
            last: None,
            parent: null_mut(),
            next: None,
            prev: None,
            doc: null_mut(),
            nexth: null_mut(),
            atype: XmlAttributeType::XmlAttributeCDATA,
            def: XmlAttributeDefault::XmlAttributeNone,
            default_value: null_mut(),
            tree: None,
            prefix: None,
            elem: None,
        }
    }
}

impl NodeCommon for XmlAttribute {
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
        (!self.name.is_null())
            .then(|| unsafe { CStr::from_ptr(self.name as *const i8).to_string_lossy() })
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

/// Deallocate the memory used by an attribute definition
#[doc(alias = "xmlFreeAttribute")]
pub(crate) unsafe fn xml_free_attribute(attr: XmlAttributePtr) {
    if attr.is_null() {
        return;
    }
    (*attr).unlink();
    (*attr).elem = None;
    if !(*attr).name.is_null() {
        xml_free((*attr).name as _);
    }
    if !(*attr).default_value.is_null() {
        xml_free((*attr).default_value as _);
    }
    (*attr).prefix = None;
    drop_in_place(attr);
    xml_free(attr as _);
}

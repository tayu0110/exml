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

mod attribute;
mod element;
mod enumeration;
mod notation;

use std::{
    borrow::Cow,
    ffi::{CStr, CString},
    os::raw::c_void,
    ptr::null_mut,
    sync::atomic::Ordering,
};

pub use attribute::*;
pub use element::*;
pub use enumeration::*;
use libc::memset;
pub use notation::*;

use crate::{
    hash::XmlHashTableRef,
    libxml::{
        entities::XmlEntityPtr,
        globals::{xml_free, xml_malloc, xml_register_node_default_value},
        xmlstring::{xml_strdup, XmlChar},
    },
    parser::split_qname2,
};

use super::{
    xml_tree_err_memory, NodeCommon, NodePtr, XmlDoc, XmlDocPtr, XmlElementType, XmlNode,
    __XML_REGISTER_CALLBACKS,
};

/// An XML DTD, as defined by <!DOCTYPE ... There is actually one for
/// the internal subset and for the external subset.
pub type XmlDtdPtr = *mut XmlDtd;
#[repr(C)]
pub struct XmlDtd {
    pub(crate) _private: *mut c_void,     /* application data */
    pub(crate) typ: XmlElementType,       /* XML_DTD_NODE, must be second ! */
    pub(crate) name: *const XmlChar,      /* Name of the DTD */
    pub(crate) children: Option<NodePtr>, /* the value of the property link */
    pub(crate) last: Option<NodePtr>,     /* last child link */
    pub(crate) parent: *mut XmlDoc,       /* child->parent link */
    pub(crate) next: Option<NodePtr>,     /* next sibling link  */
    pub(crate) prev: Option<NodePtr>,     /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,          /* the containing document */

    // End of common part
    pub(crate) notations: *mut c_void, /* Hash table for notations if any */
    pub(crate) elements: *mut c_void,  /* Hash table for elements if any */
    pub(crate) attributes: Option<XmlHashTableRef<'static, XmlAttributePtr>>, /* Hash table for attributes if any */
    pub(crate) entities: Option<XmlHashTableRef<'static, XmlEntityPtr>>, /* Hash table for entities if any */
    pub(crate) external_id: Option<String>, /* External identifier for PUBLIC DTD */
    pub(crate) system_id: Option<String>,   /* URI for a SYSTEM or PUBLIC DTD */
    pub(crate) pentities: Option<XmlHashTableRef<'static, XmlEntityPtr>>, /* Hash table for param entities if any */
}

impl XmlDtd {
    /// Do an entity lookup in the DTD entity hash table and
    /// return the corresponding entity, if found.
    ///
    /// Returns A pointer to the entity structure or null_mut() if not found.
    #[doc(alias = "xmlGetEntityFromDtd")]
    #[cfg(feature = "libxml_tree")]
    pub(super) unsafe fn get_entity(&self, name: *const XmlChar) -> XmlEntityPtr {
        if let Some(table) = self.entities {
            return table
                .lookup(CStr::from_ptr(name as *const i8))
                .map_or(null_mut(), |p| *p);
        }
        null_mut()
    }

    /// Do an entity lookup in the DTD parameter entity hash table and
    /// return the corresponding entity, if found.
    ///
    /// Returns A pointer to the entity structure or NULL if not found.
    #[doc(alias = "xmlGetParameterEntityFromDtd")]
    #[cfg(feature = "libxml_tree")]
    pub(super) unsafe fn get_parameter_entity(&self, name: *const XmlChar) -> XmlEntityPtr {
        if let Some(table) = self.pentities {
            return table
                .lookup(CStr::from_ptr(name as *const i8))
                .map_or(null_mut(), |p| *p);
        }
        null_mut()
    }

    /// Search the DTD for the description of this attribute on this element.
    ///
    /// returns the xmlAttributePtr if found or null_mut()
    #[doc(alias = "xmlGetDtdAttrDesc")]
    pub fn get_attr_desc(&self, elem: &str, name: &str) -> XmlAttributePtr {
        let Some(table) = self.attributes else {
            return null_mut();
        };

        if let Some((prefix, local)) = split_qname2(name) {
            table
                .lookup3(
                    CString::new(local).unwrap().as_c_str(),
                    Some(CString::new(prefix).unwrap().as_c_str()),
                    Some(CString::new(elem).unwrap().as_c_str()),
                )
                .copied()
                .unwrap_or(null_mut())
        } else {
            table
                .lookup3(
                    CString::new(name).unwrap().as_c_str(),
                    None,
                    Some(CString::new(elem).unwrap().as_c_str()),
                )
                .copied()
                .unwrap_or(null_mut())
        }
    }

    /// Search the DTD for the description of this qualified attribute on this element.
    ///
    /// returns the xmlAttributePtr if found or null_mut()
    #[doc(alias = "xmlGetDtdQAttrDesc")]
    pub fn get_qattr_desc(&self, elem: &str, name: &str, prefix: Option<&str>) -> XmlAttributePtr {
        let Some(table) = self.attributes else {
            return null_mut();
        };

        table
            .lookup3(
                CString::new(name).unwrap().as_c_str(),
                prefix.map(|p| CString::new(p).unwrap()).as_deref(),
                Some(CString::new(elem).unwrap().as_c_str()),
            )
            .copied()
            .unwrap_or(null_mut())
    }
}

impl Default for XmlDtd {
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
            notations: null_mut(),
            elements: null_mut(),
            attributes: None,
            entities: None,
            external_id: None,
            system_id: None,
            pentities: None,
        }
    }
}

impl NodeCommon for XmlDtd {
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
        self.parent = parent.map_or(null_mut(), |p| p.as_ptr()) as *mut XmlDoc;
    }
}

/// Create the internal subset of a document.  
/// Returns a pointer to the new DTD structure
#[doc(alias = "xmlCreateIntSubset")]
pub unsafe fn xml_create_int_subset(
    doc: XmlDocPtr,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) -> XmlDtdPtr {
    if !doc.is_null() && !(*doc).get_int_subset().is_null() {
        return null_mut();
    }

    // Allocate a new DTD and fill the fields.
    let cur: XmlDtdPtr = xml_malloc(size_of::<XmlDtd>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building internal subset");
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlDtd>());
    std::ptr::write(&mut *cur, XmlDtd::default());
    (*cur).typ = XmlElementType::XmlDTDNode;

    if let Some(name) = name {
        let name = CString::new(name).unwrap();
        (*cur).name = xml_strdup(name.as_ptr() as *const u8);
        if (*cur).name.is_null() {
            xml_tree_err_memory("building internal subset");
            xml_free(cur as _);
            return null_mut();
        }
    }
    (*cur).external_id = external_id.map(|e| e.to_owned());
    (*cur).system_id = system_id.map(|e| e.to_owned());
    if !doc.is_null() {
        (*doc).int_subset = cur;
        (*cur).parent = doc;
        (*cur).doc = doc;
        if let Some(children) = (*doc).children {
            if matches!((*doc).typ, XmlElementType::XmlHTMLDocumentNode) {
                let mut prev = children;
                prev.prev = NodePtr::from_ptr(cur as *mut XmlNode);
                (*cur).next = Some(prev);
                (*doc).children = NodePtr::from_ptr(cur as *mut XmlNode);
            } else {
                let mut next = Some(children);
                while let Some(now) =
                    next.filter(|n| !matches!(n.element_type(), XmlElementType::XmlElementNode))
                {
                    next = now.next;
                }
                if let Some(mut next) = next {
                    (*cur).next = Some(next);
                    (*cur).prev = next.prev;
                    if let Some(mut prev) = (*cur).prev {
                        prev.next = NodePtr::from_ptr(cur as *mut XmlNode);
                    } else {
                        (*doc).children = NodePtr::from_ptr(cur as *mut XmlNode);
                    }
                    next.prev = NodePtr::from_ptr(cur as *mut XmlNode);
                } else {
                    (*cur).prev = (*doc).last;
                    (*cur).prev.unwrap().next = NodePtr::from_ptr(cur as *mut XmlNode);
                    (*cur).next = None;
                    (*doc).last = NodePtr::from_ptr(cur as *mut XmlNode);
                }
            }
        } else {
            (*doc).children = NodePtr::from_ptr(cur as *mut XmlNode);
            (*doc).last = NodePtr::from_ptr(cur as *mut XmlNode);
        }
    }

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

/// Creation of a new DTD for the external subset.  
/// To create an internal subset, use xmlCreateIntSubset().
///
/// Returns a pointer to the new DTD structure
#[doc(alias = "xmlNewDtd")]
pub unsafe fn xml_new_dtd(
    doc: XmlDocPtr,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) -> XmlDtdPtr {
    if !doc.is_null() && !(*doc).ext_subset.is_null() {
        return null_mut();
    }

    // Allocate a new DTD and fill the fields.
    let cur: XmlDtdPtr = xml_malloc(size_of::<XmlDtd>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building DTD");
        return null_mut();
    }
    std::ptr::write(&mut *cur, XmlDtd::default());
    (*cur).typ = XmlElementType::XmlDTDNode;

    if let Some(name) = name {
        let name = CString::new(name).unwrap();
        (*cur).name = xml_strdup(name.as_ptr() as *const u8);
    }
    (*cur).external_id = external_id.map(|e| e.to_owned());
    (*cur).system_id = system_id.map(|s| s.to_owned());
    if !doc.is_null() {
        (*doc).ext_subset = cur;
    }
    (*cur).doc = doc;

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

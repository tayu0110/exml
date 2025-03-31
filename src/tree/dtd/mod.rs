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
    any::type_name,
    borrow::Cow,
    ops::{Deref, DerefMut},
    os::raw::c_void,
    ptr::{NonNull, null_mut},
    sync::atomic::Ordering,
};

use crate::{
    hash::{XmlHashTable, XmlHashTableRef},
    libxml::{
        globals::{xml_deregister_node_default_value, xml_register_node_default_value},
        valid::xml_free_attribute_table,
    },
    parser::split_qname2,
};

use super::{
    __XML_REGISTER_CALLBACKS, InvalidNodePointerCastError, NodeCommon, XmlDocPtr, XmlElementType,
    XmlEntityPtr, XmlGenericNodePtr, xml_free_entities_table, xml_free_node, xml_tree_err_memory,
};

pub use attribute::*;
pub use element::*;
pub use enumeration::*;
pub use notation::*;

#[repr(C)]
pub struct XmlDtd {
    pub _private: *mut c_void,                      /* application data */
    pub(crate) typ: XmlElementType,                 /* XML_DTD_NODE, must be second ! */
    pub(crate) name: Option<String>,                /* Name of the DTD */
    pub(crate) children: Option<XmlGenericNodePtr>, /* the value of the property link */
    pub(crate) last: Option<XmlGenericNodePtr>,     /* last child link */
    pub(crate) parent: Option<XmlDocPtr>,           /* child->parent link */
    pub(crate) next: Option<XmlGenericNodePtr>,     /* next sibling link  */
    pub(crate) prev: Option<XmlGenericNodePtr>,     /* previous sibling link  */
    pub(crate) doc: Option<XmlDocPtr>,              /* the containing document */

    // End of common part
    pub(crate) notations: Option<Box<XmlHashTable<'static, XmlNotation>>>, /* Hash table for notations if any */
    pub(crate) elements: Option<XmlHashTable<'static, XmlElementPtr>>, /* Hash table for elements if any */
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
    pub(super) fn get_entity(&self, name: &str) -> Option<XmlEntityPtr> {
        self.entities.as_deref()?.lookup(name).copied()
    }

    /// Do an entity lookup in the DTD parameter entity hash table and
    /// return the corresponding entity, if found.
    ///
    /// Returns A pointer to the entity structure or NULL if not found.
    #[doc(alias = "xmlGetParameterEntityFromDtd")]
    #[cfg(feature = "libxml_tree")]
    pub(super) fn get_parameter_entity(&self, name: &str) -> Option<XmlEntityPtr> {
        self.pentities.as_deref()?.lookup(name).copied()
    }

    /// Search the DTD for the description of this attribute on this element.
    ///
    /// returns the xmlAttributePtr if found or null_mut()
    #[doc(alias = "xmlGetDtdAttrDesc")]
    pub fn get_attr_desc(&self, elem: &str, name: &str) -> Option<XmlAttributePtr> {
        let table = self.attributes?;
        if let Some((prefix, local)) = split_qname2(name) {
            table.lookup3(local, Some(prefix), Some(elem)).copied()
        } else {
            table.lookup3(name, None, Some(elem)).copied()
        }
    }

    /// Search the DTD for the description of this qualified attribute on this element.
    ///
    /// returns the xmlAttributePtr if found or null_mut()
    #[doc(alias = "xmlGetDtdQAttrDesc")]
    pub fn get_qattr_desc(
        &self,
        elem: &str,
        name: &str,
        prefix: Option<&str>,
    ) -> Option<XmlAttributePtr> {
        self.attributes?.lookup3(name, prefix, Some(elem)).copied()
    }

    /// update all nodes under the tree to point to the right document
    #[doc(alias = "xmlSetTreeDoc")]
    pub unsafe fn set_doc(&mut self, doc: Option<XmlDocPtr>) {
        unsafe {
            if self.doc != doc {
                if let Some(children) = self.children() {
                    children.set_doc_all_sibling(doc);
                }

                // FIXME: self.ns should be updated as in xmlStaticCopyNode().
                self.doc = doc;
            }
        }
    }
}

impl Default for XmlDtd {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::default(),
            name: None,
            children: None,
            last: None,
            parent: None,
            next: None,
            prev: None,
            doc: None,
            notations: None,
            elements: None,
            attributes: None,
            entities: None,
            external_id: None,
            system_id: None,
            pentities: None,
        }
    }
}

impl NodeCommon for XmlDtd {
    fn document(&self) -> Option<XmlDocPtr> {
        self.doc
    }
    fn set_document(&mut self, doc: Option<XmlDocPtr>) {
        self.doc = doc;
    }
    fn element_type(&self) -> XmlElementType {
        self.typ
    }
    fn name(&self) -> Option<Cow<'_, str>> {
        self.name.as_deref().map(Cow::Borrowed)
    }
    fn children(&self) -> Option<XmlGenericNodePtr> {
        self.children
    }
    fn set_children(&mut self, children: Option<XmlGenericNodePtr>) {
        self.children = children
    }
    fn last(&self) -> Option<XmlGenericNodePtr> {
        self.last
    }
    fn set_last(&mut self, last: Option<XmlGenericNodePtr>) {
        self.last = last
    }
    fn next(&self) -> Option<XmlGenericNodePtr> {
        self.next
    }
    fn set_next(&mut self, next: Option<XmlGenericNodePtr>) {
        self.next = next
    }
    fn prev(&self) -> Option<XmlGenericNodePtr> {
        self.prev
    }
    fn set_prev(&mut self, prev: Option<XmlGenericNodePtr>) {
        self.prev = prev
    }
    fn parent(&self) -> Option<XmlGenericNodePtr> {
        self.parent.map(|node| node.into())
    }
    fn set_parent(&mut self, parent: Option<XmlGenericNodePtr>) {
        self.parent = parent.map(|ptr| XmlDocPtr::try_from(ptr).unwrap());
    }
}

/// Create the internal subset of a document.  
/// Returns a pointer to the new DTD structure
#[doc(alias = "xmlCreateIntSubset")]
pub unsafe fn xml_create_int_subset(
    doc: Option<XmlDocPtr>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) -> Option<XmlDtdPtr> {
    unsafe {
        if doc.is_some_and(|doc| doc.get_int_subset().is_some()) {
            return None;
        }

        // Allocate a new DTD and fill the fields.
        let Some(mut cur) = XmlDtdPtr::new(XmlDtd {
            typ: XmlElementType::XmlDTDNode,
            external_id: external_id.map(|e| e.to_owned()),
            system_id: system_id.map(|e| e.to_owned()),
            ..Default::default()
        }) else {
            xml_tree_err_memory("building internal subset");
            return None;
        };

        cur.name = name.map(|name| name.to_owned());
        if let Some(mut doc) = doc {
            doc.int_subset = Some(cur);
            cur.parent = Some(doc);
            cur.doc = Some(doc);
            if let Some(children) = doc.children() {
                if matches!(doc.typ, XmlElementType::XmlHTMLDocumentNode) {
                    let mut prev = children;
                    prev.set_prev(Some(cur.into()));
                    cur.set_next(Some(prev));
                    doc.set_children(Some(cur.into()));
                } else {
                    let mut next = Some(children);
                    while let Some(now) =
                        next.filter(|n| !matches!(n.element_type(), XmlElementType::XmlElementNode))
                    {
                        next = now.next();
                    }
                    if let Some(mut next) = next {
                        cur.set_next(Some(next));
                        cur.set_prev(next.prev());
                        if let Some(mut prev) = cur.prev() {
                            prev.set_next(Some(cur.into()));
                        } else {
                            doc.set_children(Some(cur.into()));
                        }
                        next.set_prev(Some(cur.into()));
                    } else {
                        cur.set_prev(doc.last());
                        cur.prev().unwrap().set_next(Some(cur.into()));
                        cur.set_next(None);
                        doc.set_last(Some(cur.into()));
                    }
                }
            } else {
                doc.children = Some(cur.into());
                doc.last = Some(cur.into());
            }
        }

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        //  && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

/// Creation of a new DTD for the external subset.  
/// To create an internal subset, use xmlCreateIntSubset().
///
/// Returns a pointer to the new DTD structure
#[doc(alias = "xmlNewDtd")]
pub unsafe fn xml_new_dtd(
    doc: Option<XmlDocPtr>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) -> Option<XmlDtdPtr> {
    unsafe {
        if doc.is_some_and(|doc| doc.ext_subset.is_some()) {
            return None;
        }

        // Allocate a new DTD and fill the fields.
        let Some(mut cur) = XmlDtdPtr::new(XmlDtd {
            typ: XmlElementType::XmlDTDNode,
            external_id: external_id.map(|e| e.to_owned()),
            system_id: system_id.map(|s| s.to_owned()),
            doc,
            ..Default::default()
        }) else {
            xml_tree_err_memory("building DTD");
            return None;
        };

        cur.name = name.map(|name| name.to_owned());
        if let Some(mut doc) = doc {
            doc.ext_subset = Some(cur);
        }

        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        //  && xmlRegisterNodeDefaultValue.is_some()
        {
            xml_register_node_default_value(cur.into());
        }
        Some(cur)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct XmlDtdPtr(NonNull<XmlDtd>);

impl XmlDtdPtr {
    /// Allocate new memory and create new `XmlDtdPtr` from an owned xml node.
    ///
    /// This method leaks allocated memory.  
    /// Users can use `free` method for deallocating memory.
    pub(crate) fn new(node: XmlDtd) -> Option<Self> {
        let boxed = Box::new(node);
        NonNull::new(Box::leak(boxed)).map(Self)
    }

    /// Create `XmlDtdPtr` from a raw pointer.  
    ///
    /// If `ptr` is a NULL pointer, return `Ok(None)`.  
    /// If `ptr` is a valid pointer of `XmlDtd`, return `Ok(Some(Self))`.  
    /// Otherwise, return `Err`.
    ///
    /// # Safety
    /// - `ptr` must be a pointer of types that is implemented `NodeCommon` at least.
    pub(crate) unsafe fn from_raw(
        ptr: *mut XmlDtd,
    ) -> Result<Option<Self>, InvalidNodePointerCastError> {
        unsafe {
            if ptr.is_null() {
                return Ok(None);
            }
            match (*ptr).element_type() {
                XmlElementType::XmlDTDNode | XmlElementType::XmlDocumentTypeNode => {
                    Ok(Some(Self(NonNull::new_unchecked(ptr))))
                }
                _ => Err(InvalidNodePointerCastError {
                    from: (*ptr).element_type(),
                    to: type_name::<Self>(),
                }),
            }
        }
    }

    // pub(crate) fn as_ptr(self) -> *mut XmlDtd {
    //     self.0.as_ptr()
    // }

    /// Deallocate memory.
    ///
    /// # Safety
    /// This method should be called only once.  
    /// If called more than twice, the behavior is undefined.
    pub(crate) unsafe fn free(self) {
        unsafe {
            let _ = *Box::from_raw(self.0.as_ptr());
        }
    }

    // /// Acquire the ownership of the inner value.
    // /// As a result, `self` will be invalid. `self` must not be used after performs this method.
    // ///
    // /// # Safety
    // /// This method should be called only once.
    // /// If called more than twice, the behavior is undefined.
    // pub(crate) unsafe fn into_inner(self) -> Box<XmlDtd> {
    //     unsafe { Box::from_raw(self.0.as_ptr()) }
    // }
}

impl Clone for XmlDtdPtr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for XmlDtdPtr {}

impl Deref for XmlDtdPtr {
    type Target = XmlDtd;
    fn deref(&self) -> &Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlDtd`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlDtdPtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlDtd`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_mut() }
    }
}

impl TryFrom<XmlGenericNodePtr> for XmlDtdPtr {
    type Error = InvalidNodePointerCastError;

    fn try_from(value: XmlGenericNodePtr) -> Result<Self, Self::Error> {
        match value.element_type() {
            XmlElementType::XmlDTDNode | XmlElementType::XmlDocumentTypeNode => {
                Ok(Self(value.0.cast()))
            }
            _ => Err(InvalidNodePointerCastError {
                from: value.element_type(),
                to: type_name::<Self>(),
            }),
        }
    }
}

impl From<XmlDtdPtr> for XmlGenericNodePtr {
    fn from(value: XmlDtdPtr) -> Self {
        Self(value.0 as NonNull<dyn NodeCommon>)
    }
}

impl From<XmlDtdPtr> for *mut XmlDtd {
    fn from(value: XmlDtdPtr) -> Self {
        value.0.as_ptr()
    }
}

/// Free a DTD structure.
#[doc(alias = "xmlFreeDtd")]
pub unsafe fn xml_free_dtd(mut cur: XmlDtdPtr) {
    unsafe {
        if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
        // && xmlDeregisterNodeDefaultValue.is_some()
        {
            xml_deregister_node_default_value(cur.into());
        }

        if let Some(children) = (*cur).children() {
            // Cleanup all nodes which are not part of the specific lists
            // of notations, elements, attributes and entities.
            let mut c = Some(children);
            while let Some(mut now) = c {
                let next = now.next();
                if !matches!(
                    now.element_type(),
                    XmlElementType::XmlNotationNode
                        | XmlElementType::XmlElementDecl
                        | XmlElementType::XmlAttributeDecl
                        | XmlElementType::XmlEntityDecl
                ) {
                    now.unlink();
                    xml_free_node(now);
                }
                c = next;
            }
        }
        cur.system_id = None;
        cur.external_id = None;
        // TODO !!!

        if let Some(table) = cur.elements.take() {
            table.scan(|data, _, _, _| xml_free_element(Some(*data)));
        }
        if let Some(table) = cur.attributes.take().map(|t| t.into_inner()) {
            xml_free_attribute_table(table);
        }
        if let Some(entities) = cur.entities.take() {
            xml_free_entities_table(entities);
        }
        if let Some(pentities) = cur.pentities.take() {
            xml_free_entities_table(pentities);
        }

        cur.free();
    }
}

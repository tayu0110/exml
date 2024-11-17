mod attribute;
mod element;
mod enumeration;
mod notation;

use std::{borrow::Cow, ffi::CStr, os::raw::c_void, ptr::null_mut, sync::atomic::Ordering};

pub use attribute::*;
pub use element::*;
pub use enumeration::*;
use libc::memset;
pub use notation::*;

use crate::libxml::{
    entities::XmlEntityPtr,
    globals::{xml_free, xml_malloc, xml_register_node_default_value},
    xmlstring::{xml_strdup, XmlChar},
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

    /* End of common part */
    pub(crate) notations: *mut c_void, /* Hash table for notations if any */
    pub(crate) elements: *mut c_void,  /* Hash table for elements if any */
    pub(crate) attributes: *mut c_void, /* Hash table for attributes if any */
    pub(crate) entities: *mut c_void,  /* Hash table for entities if any */
    pub(crate) external_id: Option<String>, /* External identifier for PUBLIC DTD */
    pub(crate) system_id: Option<String>, /* URI for a SYSTEM or PUBLIC DTD */
    pub(crate) pentities: *mut c_void, /* Hash table for param entities if any */
}

impl XmlDtd {
    /// Do an entity lookup in the DTD entity hash table and
    /// return the corresponding entity, if found.
    ///
    /// Returns A pointer to the entity structure or null_mut() if not found.
    #[doc(alias = "xmlGetEntityFromDtd")]
    #[cfg(feature = "tree")]
    pub(super) unsafe fn get_entity(&self, name: *const XmlChar) -> XmlEntityPtr {
        use std::ptr::null_mut;

        use crate::hash::xml_hash_lookup;

        if !self.entities.is_null() {
            let table = self.entities as _;
            return xml_hash_lookup(table, name) as _;
        }
        null_mut()
    }

    /// Do an entity lookup in the DTD parameter entity hash table and
    /// return the corresponding entity, if found.
    ///
    /// Returns A pointer to the entity structure or NULL if not found.
    #[doc(alias = "xmlGetParameterEntityFromDtd")]
    #[cfg(feature = "tree")]
    pub(super) unsafe fn get_parameter_entity(&self, name: *const XmlChar) -> XmlEntityPtr {
        use crate::{hash::xml_hash_lookup, libxml::entities::XmlEntitiesTablePtr};

        if !self.pentities.is_null() {
            let table = self.pentities as XmlEntitiesTablePtr;
            return xml_hash_lookup(table, name) as _;
            /* return(xmlGetEntityFromTable(table, name)); */
        }
        null_mut()
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
            attributes: null_mut(),
            entities: null_mut(),
            external_id: None,
            system_id: None,
            pentities: null_mut(),
        }
    }
}

impl NodeCommon for XmlDtd {
    fn document(&self) -> *mut XmlDoc {
        self.doc
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
    name: *const XmlChar,
    external_id: Option<&str>,
    system_id: Option<&str>,
) -> XmlDtdPtr {
    if !doc.is_null() && !(*doc).get_int_subset().is_null() {
        return null_mut();
    }

    /*
     * Allocate a new DTD and fill the fields.
     */
    let cur: XmlDtdPtr = xml_malloc(size_of::<XmlDtd>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building internal subset".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlDtd>());
    std::ptr::write(&mut *cur, XmlDtd::default());
    (*cur).typ = XmlElementType::XmlDTDNode;

    if !name.is_null() {
        (*cur).name = xml_strdup(name);
        if (*cur).name.is_null() {
            xml_tree_err_memory(c"building internal subset".as_ptr() as _);
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
                    next.filter(|n| !matches!(n.typ, XmlElementType::XmlElementNode))
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
    name: *const XmlChar,
    external_id: Option<&str>,
    system_id: Option<&str>,
) -> XmlDtdPtr {
    if !doc.is_null() && !(*doc).ext_subset.is_null() {
        return null_mut();
    }

    /*
     * Allocate a new DTD and fill the fields.
     */
    let cur: XmlDtdPtr = xml_malloc(size_of::<XmlDtd>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building DTD".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlDtd>());
    std::ptr::write(&mut *cur, XmlDtd::default());
    (*cur).typ = XmlElementType::XmlDTDNode;

    if !name.is_null() {
        (*cur).name = xml_strdup(name);
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

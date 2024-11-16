mod attribute;
mod element;
mod enumeration;
mod notation;

use std::{os::raw::c_void, ptr::null_mut, sync::atomic::Ordering};

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
    pub(crate) _private: *mut c_void,  /* application data */
    pub(crate) typ: XmlElementType,    /* XML_DTD_NODE, must be second ! */
    pub(crate) name: *const XmlChar,   /* Name of the DTD */
    pub(crate) children: *mut XmlNode, /* the value of the property link */
    pub(crate) last: *mut XmlNode,     /* last child link */
    pub(crate) parent: *mut XmlDoc,    /* child->parent link */
    pub(crate) next: *mut XmlNode,     /* next sibling link  */
    pub(crate) prev: *mut XmlNode,     /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,       /* the containing document */

    /* End of common part */
    pub(crate) notations: *mut c_void, /* Hash table for notations if any */
    pub(crate) elements: *mut c_void,  /* Hash table for elements if any */
    pub(crate) attributes: *mut c_void, /* Hash table for attributes if any */
    pub(crate) entities: *mut c_void,  /* Hash table for entities if any */
    pub(crate) external_id: *const XmlChar, /* External identifier for PUBLIC DTD */
    pub(crate) system_id: *const XmlChar, /* URI for a SYSTEM or PUBLIC DTD */
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

impl NodeCommon for XmlDtd {
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
        self.parent = parent.map_or(null_mut(), |p| p.as_ptr()) as *mut XmlDoc;
    }
}

/// Create the internal subset of a document.  
/// Returns a pointer to the new DTD structure
#[doc(alias = "xmlCreateIntSubset")]
pub unsafe fn xml_create_int_subset(
    doc: XmlDocPtr,
    name: *const XmlChar,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
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
    (*cur).typ = XmlElementType::XmlDTDNode;

    if !name.is_null() {
        (*cur).name = xml_strdup(name);
        if (*cur).name.is_null() {
            xml_tree_err_memory(c"building internal subset".as_ptr() as _);
            xml_free(cur as _);
            return null_mut();
        }
    }
    if !external_id.is_null() {
        (*cur).external_id = xml_strdup(external_id);
        if (*cur).external_id.is_null() {
            xml_tree_err_memory(c"building internal subset".as_ptr() as _);
            if !(*cur).name.is_null() {
                xml_free((*cur).name as _);
            }
            xml_free(cur as _);
            return null_mut();
        }
    }
    if !system_id.is_null() {
        (*cur).system_id = xml_strdup(system_id);
        if (*cur).system_id.is_null() {
            xml_tree_err_memory(c"building internal subset".as_ptr() as _);
            if !(*cur).name.is_null() {
                xml_free((*cur).name as _);
            }
            if !(*cur).external_id.is_null() {
                xml_free((*cur).external_id as _);
            }
            xml_free(cur as _);
            return null_mut();
        }
    }
    if !doc.is_null() {
        (*doc).int_subset = cur;
        (*cur).parent = doc;
        (*cur).doc = doc;
        if let Some(children) = (*doc).children {
            if matches!((*doc).typ, XmlElementType::XmlHTMLDocumentNode) {
                let mut prev = children;
                prev.prev = NodePtr::from_ptr(cur as *mut XmlNode);
                (*cur).next = prev.as_ptr();
                (*doc).children = NodePtr::from_ptr(cur as *mut XmlNode);
            } else {
                let mut next = Some(children);
                while let Some(now) =
                    next.filter(|n| !matches!(n.typ, XmlElementType::XmlElementNode))
                {
                    next = now.next;
                }
                if let Some(mut next) = next {
                    (*cur).next = next.as_ptr();
                    (*cur).prev = next.prev.map_or(null_mut(), |p| p.as_ptr());
                    if (*cur).prev.is_null() {
                        (*doc).children = NodePtr::from_ptr(cur as *mut XmlNode);
                    } else {
                        (*(*cur).prev).next = NodePtr::from_ptr(cur as *mut XmlNode);
                    }
                    next.prev = NodePtr::from_ptr(cur as *mut XmlNode);
                } else {
                    (*cur).prev = (*doc).last.map_or(null_mut(), |p| p.as_ptr());
                    (*(*cur).prev).next = NodePtr::from_ptr(cur as *mut XmlNode);
                    (*cur).next = null_mut();
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
    external_id: *const XmlChar,
    system_id: *const XmlChar,
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
    (*cur).typ = XmlElementType::XmlDTDNode;

    if !name.is_null() {
        (*cur).name = xml_strdup(name);
    }
    if !external_id.is_null() {
        (*cur).external_id = xml_strdup(external_id);
    }
    if !system_id.is_null() {
        (*cur).system_id = xml_strdup(system_id);
    }
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

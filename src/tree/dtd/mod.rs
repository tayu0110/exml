mod attribute;
mod element;
mod enumeration;
mod notation;

use std::{os::raw::c_void, ptr::null_mut};

pub use attribute::*;
pub use element::*;
pub use enumeration::*;
pub use notation::*;

use crate::libxml::{entities::XmlEntityPtr, xmlstring::XmlChar};

use super::{xml_set_tree_doc, NodeCommon, XmlDoc, XmlElementType, XmlNode, XmlNodePtr};

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
        self.parent = parent as *mut XmlDoc;
    }

    unsafe fn add_child(&mut self, cur: XmlNodePtr) -> XmlNodePtr {
        let mut prev: XmlNodePtr;

        if matches!(self.typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if cur.is_null() || matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if self as *mut Self as *mut XmlNode == cur {
            return null_mut();
        }

        /*
         * add the new element at the end of the children list.
         */
        prev = (*cur).parent;
        (*cur).parent = self as *mut Self as *mut XmlNode;
        if (*cur).doc != self.doc {
            xml_set_tree_doc(cur, self.doc);
        }
        /* this check prevents a loop on tree-traversions if a developer
         * tries to add a node to its parent multiple times
         */
        if prev == self as *mut Self as *mut XmlNode {
            return cur;
        }

        if self.children.is_null() {
            self.children = cur;
            self.last = cur;
        } else {
            prev = self.last;
            (*prev).next = cur;
            (*cur).prev = prev;
            self.last = cur;
        }
        cur
    }
}

use std::{os::raw::c_void, ptr::null_mut};

use crate::{dict::XmlDict, encoding::XmlCharEncoding};

use super::{NodeCommon, XmlDtd, XmlDtdPtr, XmlElementType, XmlNode, XmlNodePtr, XmlNs};

/// An XML document.
pub type XmlDocPtr = *mut XmlDoc;
#[repr(C)]
pub struct XmlDoc {
    pub(crate) _private: *mut c_void, /* application data */
    pub(crate) typ: XmlElementType,   /* XML_DOCUMENT_NODE, must be second ! */
    pub(crate) name: *mut i8,         /* name/filename/URI of the document */
    pub children: *mut XmlNode,       /* the document tree */
    pub(crate) last: *mut XmlNode,    /* last child link */
    pub(crate) parent: *mut XmlNode,  /* child->parent link */
    pub(crate) next: *mut XmlNode,    /* next sibling link  */
    pub(crate) prev: *mut XmlNode,    /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,      /* autoreference to itself */

    /* End of common part */
    pub(crate) compression: i32, /* level of zlib compression */
    pub(crate) standalone: i32,  /* standalone document (no external refs)
                                  1 if standalone="yes"
                                  0 if standalone="no"
                                 -1 if there is no XML declaration
                                 -2 if there is an XML declaration, but no
                                 standalone attribute was specified */
    pub int_subset: *mut XmlDtd,        /* the document internal subset */
    pub(crate) ext_subset: *mut XmlDtd, /* the document external subset */
    pub(crate) old_ns: *mut XmlNs,      /* Global namespace, the old way */
    pub(crate) version: Option<String>, /* the XML version string */
    pub(crate) encoding: Option<String>, /* external initial encoding, if any */
    pub(crate) ids: *mut c_void,        /* Hash table for ID attributes if any */
    pub(crate) refs: *mut c_void,       /* Hash table for IDREFs attributes if any */
    pub(crate) url: Option<String>,     /* The URI for that document */
    pub(crate) charset: XmlCharEncoding, /* Internal flag for charset handling,
                                        actually an xmlCharEncoding */
    pub dict: *mut XmlDict,       /* dict used to allocate names or NULL */
    pub(crate) psvi: *mut c_void, /* for type/PSVI information */
    pub(crate) parse_flags: i32,  /* set of xmlParserOption used to parse the
                                  document */
    pub properties: i32, /* set of xmlDocProperties for this document
                         set at the end of parsing */
}

impl XmlDoc {
    /// Get the internal subset of a document
    /// Returns a pointer to the DTD structure or null_mut() if not found
    #[doc(alias = "xmlGetIntSubset")]
    pub unsafe fn get_int_subset(&self) -> XmlDtdPtr {
        let mut cur: XmlNodePtr;

        cur = self.children;
        while !cur.is_null() {
            if matches!((*cur).typ, XmlElementType::XmlDtdNode) {
                return cur as _;
            }
            cur = (*cur).next;
        }
        self.int_subset
    }

    /// Get the root element of the document  
    /// (self.children is a list containing possibly comments, PIs, etc ...).
    ///
    /// Returns the `XmlNodePtr` for the root or NULL
    #[doc(alias = "xmlDocGetRootElement")]
    pub unsafe fn get_root_element(&self) -> XmlNodePtr {
        let mut ret = self.children;
        while !ret.is_null() {
            if matches!((*ret).typ, XmlElementType::XmlElementNode) {
                return ret;
            }
            ret = (*ret).next;
        }
        ret
    }

    /// Set the root element of the document.
    /// (self.children is a list containing possibly comments, PIs, etc ...).
    ///
    /// Returns the old root element if any was found, NULL if root was NULL
    #[doc(alias = "xmlDocSetRootElement")]
    #[cfg(any(feature = "tree", feature = "writer"))]
    pub unsafe fn set_root_element(&mut self, root: XmlNodePtr) -> XmlNodePtr {
        use crate::tree::{xml_replace_node, xml_set_tree_doc, NodeCommon};

        if root.is_null() || matches!((*root).typ, XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        (*root).unlink();
        xml_set_tree_doc(root, self);
        (*root).parent = self as *mut XmlDoc as *mut XmlNode;
        let mut old = self.children;
        while !old.is_null() {
            if matches!((*old).typ, XmlElementType::XmlElementNode) {
                break;
            }
            old = (*old).next;
        }
        if old.is_null() {
            if self.children.is_null() {
                self.children = root;
                self.last = root;
            } else {
                (*self.children).add_sibling(root);
            }
        } else {
            xml_replace_node(old, root);
        }
        old
    }
}

impl NodeCommon for XmlDoc {
    fn document(&self) -> *mut XmlDoc {
        self.doc
    }
    fn element_type(&self) -> XmlElementType {
        self.typ
    }
    fn name(&self) -> *const u8 {
        self.name as *const u8
    }
    fn children(&self) -> *mut XmlNode {
        self.children
    }
    fn set_children(&mut self, children: *mut XmlNode) {
        self.children = children
    }
    fn last(&self) -> *mut XmlNode {
        self.last
    }
    fn set_last(&mut self, last: *mut XmlNode) {
        self.last = last;
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
        self.parent
    }
    fn set_parent(&mut self, parent: *mut XmlNode) {
        self.parent = parent;
    }
}

impl Default for XmlDoc {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::default(),
            name: null_mut(),
            children: null_mut(),
            last: null_mut(),
            parent: null_mut(),
            next: null_mut(),
            prev: null_mut(),
            doc: null_mut(),
            compression: 0,
            standalone: 0,
            int_subset: null_mut(),
            ext_subset: null_mut(),
            old_ns: null_mut(),
            version: None,
            encoding: None,
            ids: null_mut(),
            refs: null_mut(),
            url: None,
            charset: XmlCharEncoding::None,
            dict: null_mut(),
            psvi: null_mut(),
            parse_flags: 0,
            properties: 0,
        }
    }
}

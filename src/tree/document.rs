use std::{os::raw::c_void, ptr::null_mut, sync::atomic::Ordering};

use libc::memset;

use crate::{
    dict::XmlDict,
    encoding::XmlCharEncoding,
    error::XmlParserErrors,
    libxml::{
        entities::{xml_get_doc_entity, XmlEntityPtr, XmlEntityType},
        globals::{xml_free, xml_malloc, xml_register_node_default_value},
        parser_internals::xml_copy_char_multi_byte,
        xmlstring::{xml_strdup, xml_strndup, XmlChar},
    },
};

use super::{
    xml_buf_add, xml_buf_cat, xml_buf_create_size, xml_buf_detach, xml_buf_free, xml_buf_is_empty,
    xml_buf_set_allocation_scheme, xml_free_node_list, xml_new_doc_text, xml_new_reference,
    xml_tree_err, xml_tree_err_memory, NodeCommon, XmlBufferAllocationScheme, XmlDocProperties,
    XmlDtd, XmlDtdPtr, XmlElementType, XmlNode, XmlNodePtr, XmlNs, XmlNsPtr, XML_ENT_EXPANDING,
    XML_ENT_PARSED, XML_LOCAL_NAMESPACE, XML_XML_NAMESPACE, __XML_REGISTER_CALLBACKS,
};

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
            if matches!((*cur).typ, XmlElementType::XmlDTDNode) {
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

    /// Get the compression ratio for a document, ZLIB based.
    ///
    /// Returns 0 (uncompressed) to 9 (max compression)
    #[doc(alias = "xmlGetDocCompressMode")]
    pub fn get_compress_mode(&self) -> i32 {
        self.compression
    }

    /// Parse the value string and build the node list associated.
    /// Should produce a flat tree with only TEXTs and ENTITY_REFs.
    /// Returns a pointer to the first child
    #[doc(alias = "xmlStringGetNodeList")]
    pub unsafe fn get_node_list(&self, value: *const XmlChar) -> XmlNodePtr {
        let mut ret: XmlNodePtr = null_mut();
        let mut head: XmlNodePtr = null_mut();
        let mut last: XmlNodePtr = null_mut();
        let mut node: XmlNodePtr;
        let mut val: *mut XmlChar = null_mut();
        let mut cur: *const XmlChar = value;
        let mut q: *const XmlChar;
        let mut ent: XmlEntityPtr;

        if value.is_null() {
            return null_mut();
        }

        let buf = xml_buf_create_size(0);
        if buf.is_null() {
            return null_mut();
        }
        xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);

        q = cur;
        while *cur != 0 {
            if *cur.add(0) == b'&' {
                let mut charval: i32 = 0;
                let mut tmp: XmlChar;

                /*
                 * Save the current text.
                 */
                if cur != q && xml_buf_add(buf, q, cur.offset_from(q) as _) != 0 {
                    // goto out;
                    xml_buf_free(buf);
                    if !val.is_null() {
                        xml_free(val as _);
                    }
                    if !head.is_null() {
                        xml_free_node_list(head);
                    }
                    return ret;
                }
                // q = cur;
                if *cur.add(1) == b'#' && *cur.add(2) == b'x' {
                    cur = cur.add(3);
                    tmp = *cur;
                    while tmp != b';' {
                        /* Non input consuming loop */
                        /* Don't check for integer overflow, see above. */
                        if tmp.is_ascii_digit() {
                            charval = charval * 16 + (tmp - b'0') as i32;
                        } else if (b'a'..=b'f').contains(&tmp) {
                            charval = charval * 16 + (tmp - b'a') as i32 + 10;
                        } else if (b'A'..=b'F').contains(&tmp) {
                            charval = charval * 16 + (tmp - b'A') as i32 + 10;
                        } else {
                            xml_tree_err(
                                XmlParserErrors::XmlTreeInvalidHex,
                                self as *const XmlDoc as _,
                                null_mut(),
                            );
                            charval = 0;
                            break;
                        }
                        cur = cur.add(1);
                        tmp = *cur;
                    }
                    if tmp == b';' {
                        cur = cur.add(1);
                    }
                    q = cur;
                } else if *cur.add(1) == b'#' {
                    cur = cur.add(2);
                    tmp = *cur;
                    while tmp != b';' {
                        /* Non input consuming loops */
                        /* Don't check for integer overflow, see above. */
                        if tmp.is_ascii_digit() {
                            charval = charval * 10 + (tmp - b'0') as i32;
                        } else {
                            xml_tree_err(
                                XmlParserErrors::XmlTreeInvalidDec,
                                self as *const XmlDoc as _,
                                null_mut(),
                            );
                            charval = 0;
                            break;
                        }
                        cur = cur.add(1);
                        tmp = *cur;
                    }
                    if tmp == b';' {
                        cur = cur.add(1);
                    }
                    q = cur;
                } else {
                    /*
                     * Read the entity string
                     */
                    cur = cur.add(1);
                    q = cur;
                    while *cur != 0 && *cur != b';' {
                        cur = cur.add(1);
                    }
                    if *cur == 0 {
                        xml_tree_err(
                            XmlParserErrors::XmlTreeUnterminatedEntity,
                            self as *const XmlDoc as _,
                            q as _,
                        );
                        // goto out;
                        xml_buf_free(buf);
                        if !val.is_null() {
                            xml_free(val as _);
                        }
                        if !head.is_null() {
                            xml_free_node_list(head);
                        }
                        return ret;
                    }
                    if cur != q {
                        /*
                         * Predefined entities don't generate nodes
                         */
                        val = xml_strndup(q, cur.offset_from(q) as _);
                        ent = xml_get_doc_entity(self, val);
                        if ent.is_null()
                            && matches!(
                                (*ent).etype,
                                Some(XmlEntityType::XmlInternalPredefinedEntity)
                            )
                        {
                            if xml_buf_cat(buf, (*ent).content.load(Ordering::Relaxed)) != 0 {
                                // goto out;
                                xml_buf_free(buf);
                                if !val.is_null() {
                                    xml_free(val as _);
                                }
                                if !head.is_null() {
                                    xml_free_node_list(head);
                                }
                                return ret;
                            }
                        } else {
                            /*
                             * Flush buffer so far
                             */
                            if xml_buf_is_empty(buf) == 0 {
                                node = xml_new_doc_text(self, null_mut());
                                if node.is_null() {
                                    // goto out;
                                    xml_buf_free(buf);
                                    if !val.is_null() {
                                        xml_free(val as _);
                                    }
                                    if !head.is_null() {
                                        xml_free_node_list(head);
                                    }
                                    return ret;
                                }
                                (*node).content = xml_buf_detach(buf);

                                if last.is_null() {
                                    last = node;
                                    head = node;
                                } else {
                                    last = (*last).add_next_sibling(node);
                                }
                            }

                            /*
                             * Create a new REFERENCE_REF node
                             */
                            node = xml_new_reference(self, val);
                            if node.is_null() {
                                // goto out;
                                xml_buf_free(buf);
                                if !val.is_null() {
                                    xml_free(val as _);
                                }
                                if !head.is_null() {
                                    xml_free_node_list(head);
                                }
                                return ret;
                            }
                            if !ent.is_null()
                                && (((*ent).flags & XML_ENT_PARSED as i32) == 0)
                                && (((*ent).flags & XML_ENT_EXPANDING as i32) == 0)
                            {
                                let mut temp: XmlNodePtr;

                                /*
                                 * The entity should have been checked already,
                                 * but set the flag anyway to avoid recursion.
                                 */
                                (*ent).flags |= XML_ENT_EXPANDING as i32;
                                (*ent)
                                    .children
                                    .store(self.get_node_list((*node).content), Ordering::Relaxed);
                                (*ent).owner = 1;
                                (*ent).flags &= !XML_ENT_EXPANDING as i32;
                                (*ent).flags |= XML_ENT_PARSED as i32;
                                temp = (*ent).children.load(Ordering::Relaxed);
                                while !temp.is_null() {
                                    (*temp).parent = ent as _;
                                    (*ent).last.store(temp, Ordering::Relaxed);
                                    temp = (*temp).next;
                                }
                            }
                            if last.is_null() {
                                last = node;
                                head = node
                            } else {
                                last = (*last).add_next_sibling(node);
                            }
                        }
                        xml_free(val as _);
                        val = null_mut();
                    }
                    cur = cur.add(1);
                    q = cur;
                }
                if charval != 0 {
                    let mut buffer: [XmlChar; 10] = [0; 10];

                    let len: i32 = xml_copy_char_multi_byte(buffer.as_mut_ptr() as _, charval);
                    buffer[len as usize] = 0;

                    if xml_buf_cat(buf, buffer.as_ptr() as _) != 0 {
                        // goto out;
                        xml_buf_free(buf);
                        if !val.is_null() {
                            xml_free(val as _);
                        }
                        if !head.is_null() {
                            xml_free_node_list(head);
                        }
                        return ret;
                    }
                    // charval = 0;
                }
            } else {
                cur = cur.add(1);
            }
        }
        if cur != q || head.is_null() {
            /*
             * Handle the last piece of text.
             */
            xml_buf_add(buf, q, cur.offset_from(q) as _);
        }

        if xml_buf_is_empty(buf) == 0 {
            node = xml_new_doc_text(self, null_mut());
            if node.is_null() {
                // goto out;
                xml_buf_free(buf);
                if !val.is_null() {
                    xml_free(val as _);
                }
                if !head.is_null() {
                    xml_free_node_list(head);
                }
                return ret;
            }
            (*node).content = xml_buf_detach(buf);

            if last.is_null() {
                head = node;
            } else {
                (*last).add_next_sibling(node);
            }
        }

        ret = head;
        head = null_mut();

        // out:
        xml_buf_free(buf);
        if !val.is_null() {
            xml_free(val as _);
        }
        if !head.is_null() {
            xml_free_node_list(head);
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

    /// Set the compression ratio for a document, ZLIB based.
    ///
    /// Correct values: 0 (uncompressed) to 9 (max compression)
    #[doc(alias = "xmlSetDocCompressMode")]
    pub fn set_compress_mode(&mut self, mode: i32) {
        if mode < 0 {
            self.compression = 0;
        } else if mode > 9 {
            self.compression = 9;
        } else {
            self.compression = mode;
        }
    }

    /// Ensures that there is an XML namespace declaration on the doc.
    ///
    /// Returns the XML ns-struct or null_mut() on API and internal errors.
    #[doc(alias = "xmlTreeEnsureXMLDecl")]
    pub(super) unsafe fn ensure_xmldecl(&mut self) -> XmlNsPtr {
        if !self.old_ns.is_null() {
            return self.old_ns;
        }
        {
            let ns = xml_malloc(size_of::<XmlNs>()) as XmlNsPtr;
            if ns.is_null() {
                xml_tree_err_memory(c"allocating the XML namespace".as_ptr() as _);
                return null_mut();
            }
            memset(ns as _, 0, size_of::<XmlNs>());
            (*ns).typ = Some(XML_LOCAL_NAMESPACE);
            (*ns).href.store(
                xml_strdup(XML_XML_NAMESPACE.as_ptr() as _) as _,
                Ordering::Relaxed,
            );
            (*ns)
                .prefix
                .store(xml_strdup(c"xml".as_ptr() as _) as _, Ordering::Relaxed);
            self.old_ns = ns;
            ns
        }
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

/// Creates a new XML document
///
/// Returns a new document
#[doc(alias = "xmlNewDoc")]
pub unsafe fn xml_new_doc(version: Option<&str>) -> XmlDocPtr {
    let version = version.unwrap_or("1.0");

    /*
     * Allocate a new document and fill the fields.
     */
    let cur: XmlDocPtr = xml_malloc(size_of::<XmlDoc>()) as _;
    if cur.is_null() {
        xml_tree_err_memory(c"building doc".as_ptr() as _);
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlDoc>());
    std::ptr::write(&mut *cur, XmlDoc::default());
    (*cur).typ = XmlElementType::XmlDocumentNode;

    (*cur).version = Some(version.to_owned());
    (*cur).standalone = -1;
    (*cur).compression = -1; /* not initialized */
    (*cur).doc = cur;
    (*cur).parse_flags = 0;
    (*cur).properties = XmlDocProperties::XmlDocUserbuilt as i32;
    /*
     * The in memory encoding is always UTF8
     * This field will never change and would
     * be obsolete if not for binary compatibility.
     */
    (*cur).charset = XmlCharEncoding::UTF8;

    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur as _);
    }
    cur
}

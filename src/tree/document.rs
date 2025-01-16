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

use std::{borrow::Cow, ffi::CStr, os::raw::c_void, ptr::null_mut, sync::atomic::Ordering};

use libc::memset;

use crate::{
    encoding::XmlCharEncoding,
    error::XmlParserErrors,
    hash::XmlHashTable,
    libxml::{
        globals::{xml_free, xml_malloc, xml_register_node_default_value},
        parser_internals::xml_copy_char_multi_byte,
        xmlstring::{xml_strdup, xml_strndup, XmlChar},
    },
};

use super::{
    xml_buf_add, xml_buf_cat, xml_buf_create_size, xml_buf_detach, xml_buf_free, xml_buf_is_empty,
    xml_buf_set_allocation_scheme, xml_free_node_list, xml_get_doc_entity, xml_new_doc_text,
    xml_new_reference, xml_tree_err, xml_tree_err_memory, NodeCommon, NodePtr,
    XmlBufferAllocationScheme, XmlDocProperties, XmlDtd, XmlDtdPtr, XmlElementType, XmlEntityPtr,
    XmlEntityType, XmlID, XmlNode, XmlNodePtr, XmlNs, XmlNsPtr, XML_ENT_EXPANDING, XML_ENT_PARSED,
    XML_LOCAL_NAMESPACE, XML_XML_NAMESPACE, __XML_REGISTER_CALLBACKS,
};

/// An XML document.
pub type XmlDocPtr = *mut XmlDoc;
#[repr(C)]
pub struct XmlDoc {
    pub(crate) _private: *mut c_void,   /* application data */
    pub(crate) typ: XmlElementType,     /* XML_DOCUMENT_NODE, must be second ! */
    pub(crate) name: *mut i8,           /* name/filename/URI of the document */
    pub children: Option<NodePtr>,      /* the document tree */
    pub(crate) last: Option<NodePtr>,   /* last child link */
    pub(crate) parent: Option<NodePtr>, /* child->parent link */
    pub(crate) next: Option<NodePtr>,   /* next sibling link  */
    pub(crate) prev: Option<NodePtr>,   /* previous sibling link  */
    pub(crate) doc: *mut XmlDoc,        /* autoreference to itself */

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
    pub(crate) ids: Option<Box<XmlHashTable<'static, *mut XmlID>>>, /* Hash table for ID attributes if any */
    pub(crate) refs: *mut c_void, /* Hash table for IDREFs attributes if any */
    pub(crate) url: Option<String>, /* The URI for that document */
    pub(crate) charset: XmlCharEncoding, /* Internal flag for charset handling,
                                  actually an xmlCharEncoding */
    // `dict` confuses me very much about the lifetime of the string...
    // I believe it is incompatible with the lifetime of Rust objects, so I removed it.
    // pub dict: *mut XmlDict,       /* dict used to allocate names or NULL */
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
        let mut cur = self.children();
        while let Some(now) = cur {
            if matches!(now.element_type(), XmlElementType::XmlDTDNode) {
                return now.as_ptr() as *mut XmlDtd;
            }
            cur = now.next();
        }
        self.int_subset
    }

    /// Get the root element of the document  
    /// (self.children is a list containing possibly comments, PIs, etc ...).
    ///
    /// Returns the `XmlNodePtr` for the root or NULL
    #[doc(alias = "xmlDocGetRootElement")]
    pub unsafe fn get_root_element(&self) -> XmlNodePtr {
        let mut ret = self.children();
        while let Some(now) = ret {
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                return now.as_ptr();
            }
            ret = now.next();
        }
        ret.map_or(null_mut(), |r| r.as_ptr())
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
    ///
    /// Returns a pointer to the first child.
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

                // Save the current text.
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
                                None,
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
                        // Non input consuming loops
                        // Don't check for integer overflow, see above.
                        if tmp.is_ascii_digit() {
                            charval = charval * 10 + (tmp - b'0') as i32;
                        } else {
                            xml_tree_err(
                                XmlParserErrors::XmlTreeInvalidDec,
                                self as *const XmlDoc as _,
                                None,
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
                    // Read the entity string
                    cur = cur.add(1);
                    q = cur;
                    while *cur != 0 && *cur != b';' {
                        cur = cur.add(1);
                    }
                    if *cur == 0 {
                        let q = (!q.is_null())
                            .then(|| CStr::from_ptr(q as *const i8).to_string_lossy());
                        xml_tree_err(
                            XmlParserErrors::XmlTreeUnterminatedEntity,
                            self as *const XmlDoc as _,
                            q.as_deref(),
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
                        // Predefined entities don't generate nodes
                        val = xml_strndup(q, cur.offset_from(q) as _);
                        ent = xml_get_doc_entity(
                            self,
                            CStr::from_ptr(val as *const i8).to_string_lossy().as_ref(),
                        );
                        if ent.is_null()
                            && matches!((*ent).etype, XmlEntityType::XmlInternalPredefinedEntity)
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
                            // Flush buffer so far
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

                            // Create a new REFERENCE_REF node
                            node = xml_new_reference(
                                self,
                                &CStr::from_ptr(val as *const i8).to_string_lossy(),
                            );
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
                                // The entity should have been checked already,
                                // but set the flag anyway to avoid recursion.
                                (*ent).flags |= XML_ENT_EXPANDING as i32;
                                (*ent).set_children(NodePtr::from_ptr(
                                    self.get_node_list((*node).content),
                                ));
                                (*ent).owner = 1;
                                (*ent).flags &= !XML_ENT_EXPANDING as i32;
                                (*ent).flags |= XML_ENT_PARSED as i32;
                                let mut temp = (*ent).children();
                                while let Some(mut now) = temp {
                                    now.set_parent(NodePtr::from_ptr(ent as *mut XmlNode));
                                    (*ent).set_last(Some(now));
                                    temp = now.next;
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
            // Handle the last piece of text.
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

    /// Parse the value string and build the node list associated.  
    /// Should produce a flat tree with only TEXTs and ENTITY_REFs.
    ///
    /// Returns a pointer to the first child.
    #[doc(alias = "xmlStringLenGetNodeList")]
    pub unsafe fn get_node_list_with_strlen(&self, value: *const XmlChar, len: i32) -> XmlNodePtr {
        let mut ret: XmlNodePtr = null_mut();
        let mut last: XmlNodePtr = null_mut();
        let mut node: XmlNodePtr;
        let mut val: *mut XmlChar;
        let mut cur: *const XmlChar;
        let mut q: *const XmlChar;
        let mut ent: XmlEntityPtr;

        if value.is_null() {
            return null_mut();
        }
        cur = value;
        let end: *const XmlChar = cur.add(len as usize);

        let buf = xml_buf_create_size(0);
        if buf.is_null() {
            return null_mut();
        }
        xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);

        q = cur;
        while cur < end && *cur != 0 {
            if *cur.add(0) == b'&' {
                let mut charval: i32 = 0;
                let mut tmp: XmlChar;

                // Save the current text.
                if cur != q && xml_buf_add(buf, q, cur.offset_from(q) as _) != 0 {
                    // goto out;
                    xml_buf_free(buf);
                    return ret;
                }
                // q = cur;
                if cur.add(2) < end && *cur.add(1) == b'#' && *cur.add(2) == b'x' {
                    cur = cur.add(3);
                    if cur < end {
                        tmp = *cur;
                    } else {
                        tmp = 0;
                    }
                    while tmp != b';' {
                        // Non input consuming loop

                        // If you find an integer overflow here when fuzzing,
                        // the bug is probably elsewhere. This function should
                        // only receive entities that were already validated by
                        // the parser, typically by xmlParseAttValueComplex
                        // calling xmlStringDecodeEntities.
                        //
                        // So it's better *not* to check for overflow to
                        // potentially discover new bugs.
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
                                None,
                            );
                            charval = 0;
                            break;
                        }
                        cur = cur.add(1);
                        if cur < end {
                            tmp = *cur;
                        } else {
                            tmp = 0;
                        }
                    }
                    if tmp == b';' {
                        cur = cur.add(1);
                    }
                    q = cur;
                } else if cur.add(1) < end && *cur.add(1) == b'#' {
                    cur = cur.add(2);
                    if cur < end {
                        tmp = *cur;
                    } else {
                        tmp = 0;
                    }
                    while tmp != b';' {
                        // Non input consuming loops
                        // Don't check for integer overflow, see above.
                        if tmp.is_ascii_digit() {
                            charval = charval * 10 + (tmp - b'0') as i32;
                        } else {
                            xml_tree_err(
                                XmlParserErrors::XmlTreeInvalidDec,
                                self as *const XmlDoc as _,
                                None,
                            );
                            charval = 0;
                            break;
                        }
                        cur = cur.add(1);
                        if cur < end {
                            tmp = *cur;
                        } else {
                            tmp = 0;
                        }
                    }
                    if tmp == b';' {
                        cur = cur.add(1);
                    }
                    q = cur;
                } else {
                    // Read the entity string
                    cur = cur.add(1);
                    q = cur;
                    while cur < end && *cur != 0 && *cur != b';' {
                        cur = cur.add(1);
                    }
                    if cur >= end || *cur == 0 {
                        let q = (!q.is_null())
                            .then(|| CStr::from_ptr(q as *const i8).to_string_lossy());
                        xml_tree_err(
                            XmlParserErrors::XmlTreeUnterminatedEntity,
                            self as *const XmlDoc as _,
                            q.as_deref(),
                        );
                        // goto out;
                        xml_buf_free(buf);
                        return ret;
                    }
                    if cur != q {
                        // Predefined entities don't generate nodes
                        val = xml_strndup(q, cur.offset_from(q) as _);
                        ent = xml_get_doc_entity(
                            self,
                            CStr::from_ptr(val as *const i8).to_string_lossy().as_ref(),
                        );
                        if !ent.is_null()
                            && matches!((*ent).etype, XmlEntityType::XmlInternalPredefinedEntity)
                        {
                            if xml_buf_cat(buf, (*ent).content.load(Ordering::Relaxed)) != 0 {
                                // goto out;
                                xml_buf_free(buf);
                                return ret;
                            }
                        } else {
                            // Flush buffer so far
                            if xml_buf_is_empty(buf) == 0 {
                                node = xml_new_doc_text(self, null_mut());
                                if node.is_null() {
                                    if !val.is_null() {
                                        xml_free(val as _);
                                    }
                                    // goto out;
                                    xml_buf_free(buf);
                                    return ret;
                                }
                                (*node).content = xml_buf_detach(buf);

                                if last.is_null() {
                                    last = node;
                                    ret = node;
                                } else {
                                    last = (*last).add_next_sibling(node);
                                }
                            }

                            // Create a new REFERENCE_REF node
                            node = xml_new_reference(
                                self,
                                &CStr::from_ptr(val as *const i8).to_string_lossy(),
                            );
                            if node.is_null() {
                                if !val.is_null() {
                                    xml_free(val as _);
                                }
                                // goto out;
                                xml_buf_free(buf);
                                return ret;
                            } else if !ent.is_null()
                                && (*ent).flags & XML_ENT_PARSED as i32 == 0
                                && (*ent).flags & XML_ENT_EXPANDING as i32 == 0
                            {
                                // The entity should have been checked already,
                                // but set the flag anyway to avoid recursion.
                                (*ent).flags |= XML_ENT_EXPANDING as i32;
                                (*ent).children.store(
                                    self.get_node_list((*node).content as _),
                                    Ordering::Relaxed,
                                );
                                (*ent).owner = 1;
                                (*ent).flags &= !XML_ENT_EXPANDING as i32;
                                (*ent).flags |= XML_ENT_PARSED as i32;
                                let mut temp = (*ent).children();
                                while let Some(mut now) = temp {
                                    now.set_parent(NodePtr::from_ptr(ent as *mut XmlNode));
                                    (*ent).set_last(Some(now));
                                    temp = now.next();
                                }
                            }
                            if last.is_null() {
                                last = node;
                                ret = node;
                            } else {
                                last = (*last).add_next_sibling(node);
                            }
                        }
                        xml_free(val as _);
                    }
                    cur = cur.add(1);
                    q = cur;
                }
                if charval != 0 {
                    let mut buffer: [XmlChar; 10] = [0; 10];

                    let l: i32 = xml_copy_char_multi_byte(buffer.as_mut_ptr() as _, charval);
                    buffer[l as usize] = 0;

                    if xml_buf_cat(buf, buffer.as_ptr() as _) != 0 {
                        // goto out;
                        xml_buf_free(buf);
                        return ret;
                    }
                    // charval = 0;
                }
            } else {
                cur = cur.add(1);
            }
        }

        if cur != q {
            // Handle the last piece of text.
            if xml_buf_add(buf, q, cur.offset_from(q) as _) != 0 {
                // goto out;
                xml_buf_free(buf);
                return ret;
            }
        }

        if xml_buf_is_empty(buf) == 0 {
            node = xml_new_doc_text(self, null_mut());
            if node.is_null() {
                // goto out;
                xml_buf_free(buf);
                return ret;
            }
            (*node).content = xml_buf_detach(buf);

            if last.is_null() {
                ret = node;
            } else {
                (*last).add_next_sibling(node);
            }
        } else if ret.is_null() {
            ret = xml_new_doc_text(self, c"".as_ptr() as _);
        }

        // out:
        xml_buf_free(buf);
        ret
    }

    /// Set the root element of the document.
    /// (self.children is a list containing possibly comments, PIs, etc ...).
    ///
    /// Returns the old root element if any was found, NULL if root was NULL
    #[doc(alias = "xmlDocSetRootElement")]
    #[cfg(any(feature = "libxml_tree", feature = "libxml_writer"))]
    pub unsafe fn set_root_element(&mut self, root: XmlNodePtr) -> XmlNodePtr {
        use crate::tree::{xml_replace_node, NodeCommon};

        if root.is_null() || matches!((*root).element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        (*root).unlink();
        (*root).set_doc(self);
        (*root).set_parent(NodePtr::from_ptr(self as *mut XmlDoc as *mut XmlNode));
        let mut old = self.children();
        while let Some(now) = old {
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                break;
            }
            old = now.next();
        }
        if let Some(old) = old {
            xml_replace_node(old.as_ptr(), root);
        } else if let Some(mut children) = self.children() {
            children.add_sibling(root);
        } else {
            self.set_children(NodePtr::from_ptr(root));
            self.set_last(NodePtr::from_ptr(root));
        }
        old.map_or(null_mut(), |o| o.as_ptr())
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
                xml_tree_err_memory("allocating the XML namespace");
                return null_mut();
            }
            memset(ns as _, 0, size_of::<XmlNs>());
            (*ns).typ = XML_LOCAL_NAMESPACE;
            (*ns).href = xml_strdup(XML_XML_NAMESPACE.as_ptr() as _);
            (*ns).prefix = xml_strdup(c"xml".as_ptr() as _);
            self.old_ns = ns;
            ns
        }
    }
}

impl NodeCommon for XmlDoc {
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
        self.children = children
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
        self.parent
    }
    fn set_parent(&mut self, parent: Option<NodePtr>) {
        self.parent = parent;
    }
}

impl Default for XmlDoc {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::default(),
            name: null_mut(),
            children: None,
            last: None,
            parent: None,
            next: None,
            prev: None,
            doc: null_mut(),
            compression: 0,
            standalone: 0,
            int_subset: null_mut(),
            ext_subset: null_mut(),
            old_ns: null_mut(),
            version: None,
            encoding: None,
            ids: None,
            refs: null_mut(),
            url: None,
            charset: XmlCharEncoding::None,
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

    // Allocate a new document and fill the fields.
    let cur: XmlDocPtr = xml_malloc(size_of::<XmlDoc>()) as _;
    if cur.is_null() {
        xml_tree_err_memory("building doc");
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

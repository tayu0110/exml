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
    any::type_name,
    borrow::Cow,
    collections::HashMap,
    ffi::{CStr, CString},
    ops::{Deref, DerefMut},
    os::raw::c_void,
    ptr::{null_mut, NonNull},
    slice::from_raw_parts,
    sync::atomic::Ordering,
};

use crate::{
    encoding::XmlCharEncoding,
    error::XmlParserErrors,
    hash::XmlHashTable,
    libxml::{
        globals::{xml_deregister_node_default_value, xml_free, xml_register_node_default_value},
        parser_internals::xml_copy_char_multi_byte,
        xmlstring::{xml_str_equal, xml_strdup, xml_strndup, XmlChar},
    },
    list::XmlList,
};

use super::{
    xml_free_dtd, xml_free_node_list, xml_free_ns_list, xml_get_doc_entity, xml_new_doc_text,
    xml_new_reference, xml_ns_in_scope, xml_tree_err, xml_tree_err_memory,
    InvalidNodePointerCastError, NodeCommon, NodePtr, XmlDocProperties, XmlDtdPtr, XmlElementType,
    XmlEntityType, XmlGenericNodePtr, XmlID, XmlNodePtr, XmlNs, XmlNsPtr, XmlRef,
    XML_ENT_EXPANDING, XML_ENT_PARSED, XML_LOCAL_NAMESPACE, XML_XML_NAMESPACE,
    __XML_REGISTER_CALLBACKS,
};

#[repr(C)]
pub struct XmlDoc {
    pub _private: *mut c_void,          /* application data */
    pub(crate) typ: XmlElementType,     /* XML_DOCUMENT_NODE, must be second ! */
    pub(crate) name: *mut i8,           /* name/filename/URI of the document */
    pub children: Option<NodePtr>,      /* the document tree */
    pub(crate) last: Option<NodePtr>,   /* last child link */
    pub(crate) parent: Option<NodePtr>, /* child->parent link */
    pub(crate) next: Option<NodePtr>,   /* next sibling link  */
    pub(crate) prev: Option<NodePtr>,   /* previous sibling link  */
    pub(crate) doc: Option<XmlDocPtr>,  /* autoreference to itself */

    /* End of common part */
    pub(crate) compression: i32, /* level of zlib compression */
    // standalone document (no external refs)
    //   1 if standalone="yes"
    //   0 if standalone="no"
    //  -1 if there is no XML declaration
    //  -2 if there is an XML declaration, but no
    //  standalone attribute was specified
    pub(crate) standalone: i32,
    // the document internal subset
    pub int_subset: Option<XmlDtdPtr>,
    // the document external subset
    pub(crate) ext_subset: Option<XmlDtdPtr>,
    // Global namespace, the old way
    pub(crate) old_ns: Option<XmlNsPtr>,
    // the XML version string
    pub(crate) version: Option<String>,
    // external initial encoding, if any
    pub(crate) encoding: Option<String>,
    // Hash table for ID attributes if any
    pub(crate) ids: Option<Box<XmlHashTable<'static, XmlID>>>,
    // Hash table for IDREFs attributes if any
    pub(crate) refs: Option<HashMap<String, XmlList<Box<XmlRef>>>>,
    // The URI for that document
    pub(crate) url: Option<String>,
    // Internal flag for charset handling, actually an xmlCharEncoding
    pub(crate) charset: XmlCharEncoding,
    // `dict` confuses me very much about the lifetime of the string...
    // I believe it is incompatible with the lifetime of Rust objects, so I removed it.
    // pub dict: *mut XmlDict,       /* dict used to allocate names or NULL */
    pub(crate) psvi: *mut c_void, /* for type/PSVI information */
    // set of xmlParserOption used to parse the document
    pub(crate) parse_flags: i32,
    // set of xmlDocProperties for this document
    // set at the end of parsing
    pub properties: i32,
}

impl XmlDoc {
    /// Search a Ns aliasing a given URI.
    /// Recurse on the parents until it finds the defined namespace or return NULL otherwise.
    ///
    /// Returns the namespace pointer or NULL.
    #[doc(alias = "xmlSearchNsByHref")]
    pub unsafe fn search_ns_by_href(
        &mut self,
        doc: Option<XmlDocPtr>,
        href: &str,
    ) -> Option<XmlNsPtr> {
        let orig = XmlGenericNodePtr::from_raw(self).unwrap();

        if href == XML_XML_NAMESPACE.to_str().unwrap() {
            let mut doc = doc.or(self.document())?;
            // Return the XML namespace declaration held by the doc.
            if doc.old_ns.is_none() {
                return doc.ensure_xmldecl();
            } else {
                return doc.old_ns;
            }
        }
        let mut node = Some(orig);
        while let Some(cur_node) = node {
            if matches!(
                cur_node.element_type(),
                XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode
                    | XmlElementType::XmlEntityDecl
            ) {
                return None;
            }
            if matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                let href = CString::new(href).unwrap();
                let mut cur = cur_node.ns_def;
                while let Some(now) = cur {
                    if !now.href.is_null()
                        && xml_str_equal(now.href, href.as_ptr() as *const u8)
                        && now.prefix().is_some()
                        && xml_ns_in_scope(doc, Some(orig), node, now.prefix) == 1
                    {
                        return Some(now);
                    }
                    cur = now.next;
                }
                if orig != cur_node.into() {
                    let cur = cur_node.ns;
                    if let Some(cur) = cur.filter(|cur| {
                        !cur.href.is_null()
                            && xml_str_equal(cur.href, href.as_ptr() as *const u8)
                            && (*cur).prefix().is_some()
                            && xml_ns_in_scope(doc, Some(orig), node, cur.prefix) == 1
                    }) {
                        return Some(cur);
                    }
                }
            }
            node = cur_node
                .parent()
                .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
        }
        None
    }

    /// Get the internal subset of a document
    /// Returns a pointer to the DTD structure or null_mut() if not found
    #[doc(alias = "xmlGetIntSubset")]
    pub unsafe fn get_int_subset(&self) -> Option<XmlDtdPtr> {
        let mut cur = self.children();
        while let Some(now) = cur {
            if matches!(now.element_type(), XmlElementType::XmlDTDNode) {
                return Some(XmlDtdPtr::try_from(now).unwrap());
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
    pub unsafe fn get_root_element(&self) -> Option<XmlNodePtr> {
        let mut ret = self.children();
        while let Some(now) = ret {
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                return XmlNodePtr::from_raw(now.as_ptr()).unwrap();
            }
            ret = now.next();
        }
        None
    }

    /// Get the compression ratio for a document, ZLIB based.
    ///
    /// Returns 0 (uncompressed) to 9 (max compression)
    #[doc(alias = "xmlGetDocCompressMode")]
    pub fn get_compress_mode(&self) -> i32 {
        self.compression
    }

    /// Read the value of a node, this can be either the text carried
    /// directly by this node if it's a TEXT node or the aggregate string
    /// of the values carried by this node child's (TEXT and ENTITY_REF).  
    ///
    /// Entity references are substituted.
    ///
    /// Returns a new #XmlChar * or null_mut() if no content is available.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlNodeGetContent")]
    pub unsafe fn get_content(&self) -> Option<String> {
        assert!(matches!(
            self.element_type(),
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
        ));
        let mut buf = String::new();
        self.get_content_to(&mut buf);
        Some(buf)
    }

    /// Read the value of a node `cur`, this can be either the text carried
    /// directly by this node if it's a TEXT node or the aggregate string
    /// of the values carried by this node child's (TEXT and ENTITY_REF).
    ///
    /// Entity references are substituted. Fills up the buffer `buf` with this value.
    ///
    /// Returns 0 in case of success and -1 in case of error.
    #[doc(alias = "xmlBufGetNodeContent")]
    pub unsafe fn get_content_to(&self, buf: &mut String) -> i32 {
        assert!(matches!(
            self.element_type(),
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
        ));
        let mut next = self.children();
        while let Some(cur) = next {
            if matches!(
                cur.element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlCDATASectionNode
            ) {
                cur.get_content_to(buf);
            }
            next = cur.next();
        }
        0
    }

    /// Parse the value string and build the node list associated.  
    /// Should produce a flat tree with only TEXTs and ENTITY_REFs.
    ///
    /// Returns a pointer to the first child.
    #[doc(alias = "xmlStringGetNodeList")]
    pub unsafe fn get_node_list(&self, value: *const XmlChar) -> Option<XmlNodePtr> {
        let mut ret = None;
        let mut head: Option<XmlNodePtr> = None;
        let mut last: Option<XmlNodePtr> = None;
        let mut val: *mut XmlChar = null_mut();
        let mut cur: *const XmlChar = value;
        let mut q: *const XmlChar;

        if value.is_null() {
            return None;
        }

        let mut buf = vec![];
        q = cur;
        while *cur != 0 {
            if *cur.add(0) == b'&' {
                let mut charval: i32 = 0;
                let mut tmp: XmlChar;

                // Save the current text.
                if cur != q {
                    buf.extend(from_raw_parts(q, cur.offset_from(q) as usize));
                    // goto out;
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
                                XmlGenericNodePtr::from_raw(self as *const XmlDoc as *mut XmlDoc),
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
                                XmlGenericNodePtr::from_raw(self as *const XmlDoc as *mut XmlDoc),
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
                            XmlGenericNodePtr::from_raw(self as *const XmlDoc as *mut XmlDoc),
                            q.as_deref(),
                        );
                        // goto out;
                        if !val.is_null() {
                            xml_free(val as _);
                        }
                        if let Some(head) = head {
                            xml_free_node_list(Some(head));
                        }
                        return ret;
                    }
                    if cur != q {
                        // Predefined entities don't generate nodes
                        val = xml_strndup(q, cur.offset_from(q) as _);
                        let ent = xml_get_doc_entity(
                            XmlDocPtr::from_raw(self as *const XmlDoc as _).unwrap(),
                            CStr::from_ptr(val as *const i8).to_string_lossy().as_ref(),
                        );
                        if let Some(ent) = ent.filter(|ent| {
                            matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
                        }) {
                            buf.extend(
                                CStr::from_ptr(ent.content.load(Ordering::Relaxed) as *const i8)
                                    .to_bytes(),
                            );
                        } else {
                            // Flush buffer so far
                            if !buf.is_empty() {
                                let Some(mut node) = xml_new_doc_text(
                                    XmlDocPtr::from_raw(self as *const XmlDoc as *mut XmlDoc)
                                        .unwrap(),
                                    null_mut(),
                                ) else {
                                    // goto out;
                                    if !val.is_null() {
                                        xml_free(val as _);
                                    }
                                    if let Some(head) = head {
                                        xml_free_node_list(Some(head));
                                    }
                                    return ret;
                                };
                                node.content = xml_strndup(buf.as_ptr(), buf.len() as i32);
                                buf.clear();

                                if let Some(mut l) = last {
                                    last = l
                                        .add_next_sibling(node.into())
                                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                                } else {
                                    last = Some(node);
                                    head = Some(node);
                                }
                            }

                            // Create a new REFERENCE_REF node
                            let Some(node) = xml_new_reference(
                                XmlDocPtr::from_raw(self as *const XmlDoc as _).unwrap(),
                                &CStr::from_ptr(val as *const i8).to_string_lossy(),
                            ) else {
                                // goto out;
                                if !val.is_null() {
                                    xml_free(val as _);
                                }
                                if let Some(head) = head {
                                    xml_free_node_list(Some(head));
                                }
                                return ret;
                            };
                            if let Some(mut ent) = ent.filter(|ent| {
                                ent.flags & XML_ENT_PARSED as i32 == 0
                                    && ent.flags & XML_ENT_EXPANDING as i32 == 0
                            }) {
                                // The entity should have been checked already,
                                // but set the flag anyway to avoid recursion.
                                ent.flags |= XML_ENT_EXPANDING as i32;
                                ent.set_children(
                                    self.get_node_list(node.content).map(|node| node.into()),
                                );
                                ent.owner = 1;
                                ent.flags &= !XML_ENT_EXPANDING as i32;
                                ent.flags |= XML_ENT_PARSED as i32;
                                let mut temp = ent.children();
                                while let Some(mut now) = temp {
                                    now.set_parent(Some(ent.into()));
                                    ent.set_last(Some(now));
                                    temp = now.next();
                                }
                            }
                            if let Some(mut l) = last {
                                last = l
                                    .add_next_sibling(node.into())
                                    .map(|node| XmlNodePtr::try_from(node).unwrap());
                            } else {
                                last = Some(node);
                                head = Some(node);
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
                    buf.extend_from_slice(&buffer[..len as usize]);
                    // charval = 0;
                }
            } else {
                cur = cur.add(1);
            }
        }
        if cur != q || head.is_none() {
            // Handle the last piece of text.
            buf.extend(from_raw_parts(q, cur.offset_from(q) as usize));
        }

        if !buf.is_empty() {
            let Some(mut node) = xml_new_doc_text(
                XmlDocPtr::from_raw(self as *const XmlDoc as *mut XmlDoc).unwrap(),
                null_mut(),
            ) else {
                // goto out;
                if !val.is_null() {
                    xml_free(val as _);
                }
                if let Some(head) = head {
                    xml_free_node_list(Some(head));
                }
                return ret;
            };
            node.content = xml_strndup(buf.as_ptr(), buf.len() as i32);
            buf.clear();

            if let Some(mut last) = last {
                (*last).add_next_sibling(node.into());
            } else {
                head = Some(node);
            }
        }

        ret = head;

        // out:
        if !val.is_null() {
            xml_free(val as _);
        }
        ret
    }

    /// Parse the value string and build the node list associated.  
    /// Should produce a flat tree with only TEXTs and ENTITY_REFs.
    ///
    /// Returns a pointer to the first child.
    #[doc(alias = "xmlStringLenGetNodeList")]
    pub unsafe fn get_node_list_with_strlen(
        &self,
        value: *const XmlChar,
        len: i32,
    ) -> Option<XmlNodePtr> {
        let mut ret = None;
        let mut last: Option<XmlNodePtr> = None;
        let mut val: *mut XmlChar;
        let mut cur: *const XmlChar;
        let mut q: *const XmlChar;

        if value.is_null() {
            return None;
        }
        cur = value;
        let end: *const XmlChar = cur.add(len as usize);

        let mut buf = vec![];
        q = cur;
        while cur < end && *cur != 0 {
            if *cur.add(0) == b'&' {
                let mut charval: i32 = 0;
                let mut tmp: XmlChar;

                // Save the current text.
                if cur != q {
                    buf.extend(from_raw_parts(q, cur.offset_from(q) as usize));
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
                                XmlGenericNodePtr::from_raw(self as *const XmlDoc as *mut XmlDoc),
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
                                XmlGenericNodePtr::from_raw(self as *const XmlDoc as *mut XmlDoc),
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
                            XmlGenericNodePtr::from_raw(self as *const XmlDoc as *mut XmlDoc),
                            q.as_deref(),
                        );
                        // goto out;
                        return ret;
                    }
                    if cur != q {
                        // Predefined entities don't generate nodes
                        val = xml_strndup(q, cur.offset_from(q) as _);
                        let ent = xml_get_doc_entity(
                            XmlDocPtr::from_raw(self as *const XmlDoc as _).unwrap(),
                            CStr::from_ptr(val as *const i8).to_string_lossy().as_ref(),
                        );
                        if let Some(ent) = ent.filter(|ent| {
                            matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
                        }) {
                            buf.extend(
                                CStr::from_ptr(ent.content.load(Ordering::Relaxed) as *const i8)
                                    .to_bytes(),
                            );
                        } else {
                            // Flush buffer so far
                            if !buf.is_empty() {
                                let Some(mut node) = xml_new_doc_text(
                                    XmlDocPtr::from_raw(self as *const XmlDoc as *mut XmlDoc)
                                        .unwrap(),
                                    null_mut(),
                                ) else {
                                    if !val.is_null() {
                                        xml_free(val as _);
                                    }
                                    // goto out;
                                    return ret;
                                };
                                node.content = xml_strndup(buf.as_ptr(), buf.len() as i32);
                                buf.clear();

                                if let Some(mut l) = last {
                                    last = l
                                        .add_next_sibling(node.into())
                                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                                } else {
                                    last = Some(node);
                                    ret = Some(node);
                                }
                            }

                            // Create a new REFERENCE_REF node
                            let Some(node) = xml_new_reference(
                                XmlDocPtr::from_raw(self as *const XmlDoc as _).unwrap(),
                                &CStr::from_ptr(val as *const i8).to_string_lossy(),
                            ) else {
                                if !val.is_null() {
                                    xml_free(val as _);
                                }
                                // goto out;
                                return ret;
                            };
                            if let Some(mut ent) = ent.filter(|ent| {
                                ent.flags & XML_ENT_PARSED as i32 == 0
                                    && ent.flags & XML_ENT_EXPANDING as i32 == 0
                            }) {
                                // The entity should have been checked already,
                                // but set the flag anyway to avoid recursion.
                                ent.flags |= XML_ENT_EXPANDING as i32;
                                ent.children.store(
                                    self.get_node_list(node.content as _)
                                        .map_or(null_mut(), |node| node.as_ptr()),
                                    Ordering::Relaxed,
                                );
                                ent.owner = 1;
                                ent.flags &= !XML_ENT_EXPANDING as i32;
                                ent.flags |= XML_ENT_PARSED as i32;
                                let mut temp = ent.children();
                                while let Some(mut now) = temp {
                                    now.set_parent(Some(ent.into()));
                                    ent.set_last(Some(now));
                                    temp = now.next();
                                }
                            }
                            if let Some(mut l) = last {
                                last = l
                                    .add_next_sibling(node.into())
                                    .map(|node| XmlNodePtr::try_from(node).unwrap());
                            } else {
                                last = Some(node);
                                ret = Some(node);
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
                    buf.extend_from_slice(&buffer[..l as usize]);
                    // charval = 0;
                }
            } else {
                cur = cur.add(1);
            }
        }

        if cur != q {
            // Handle the last piece of text.
            buf.extend(from_raw_parts(q, cur.offset_from(q) as usize));
        }

        if !buf.is_empty() {
            let Some(mut node) = xml_new_doc_text(
                XmlDocPtr::from_raw(self as *const XmlDoc as *mut XmlDoc).unwrap(),
                null_mut(),
            ) else {
                // goto out;
                return ret;
            };
            node.content = xml_strndup(buf.as_ptr(), buf.len() as i32);
            buf.clear();

            if let Some(mut last) = last {
                last.add_next_sibling(node.into());
            } else {
                ret = Some(node);
            }
        } else if ret.is_none() {
            ret = xml_new_doc_text(
                XmlDocPtr::from_raw(self as *const XmlDoc as *mut XmlDoc).unwrap(),
                c"".as_ptr() as _,
            );
        }

        // out:
        ret
    }

    /// Set (or reset) the base URI of a node, i.e. the value of the xml:base attribute.
    #[doc(alias = "xmlNodeSetBase")]
    #[cfg(any(feature = "libxml_tree", feature = "xinclude"))]
    pub unsafe fn set_base(&mut self, uri: Option<&str>) {
        use crate::uri::path_to_uri;

        let url = uri.map(path_to_uri);
        self.url = url.map(|url| url.into_owned());
    }

    /// Set the root element of the document.
    /// (self.children is a list containing possibly comments, PIs, etc ...).
    ///
    /// Returns the old root element if any was found, NULL if root was NULL
    #[doc(alias = "xmlDocSetRootElement")]
    #[cfg(any(feature = "libxml_tree", feature = "libxml_writer"))]
    pub unsafe fn set_root_element(&mut self, mut root: XmlNodePtr) -> Option<XmlNodePtr> {
        use crate::tree::{xml_replace_node, NodeCommon};

        (*root).unlink();
        (*root).set_doc(XmlDocPtr::from_raw(self).unwrap());
        (*root).set_parent(XmlGenericNodePtr::from_raw(self));
        let mut old = self
            .children()
            .and_then(|children| XmlGenericNodePtr::from_raw(children.as_ptr()));
        while let Some(now) = old {
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                break;
            }
            old = now
                .next()
                .and_then(|next| XmlGenericNodePtr::from_raw(next.as_ptr()));
        }
        if let Some(old) = old {
            xml_replace_node(
                XmlGenericNodePtr::from_raw(old.as_ptr()).unwrap(),
                Some(XmlGenericNodePtr::from(root)),
            );
        } else if let Some(children) = self.children() {
            children.add_sibling(XmlGenericNodePtr::from(root));
        } else {
            self.set_children(Some(root.into()));
            self.set_last(Some(root.into()));
        }
        old.map(|o| XmlNodePtr::try_from(o).unwrap())
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
    pub(super) unsafe fn ensure_xmldecl(&mut self) -> Option<XmlNsPtr> {
        if let Some(old_ns) = self.old_ns {
            return Some(old_ns);
        }
        let Some(ns) = XmlNsPtr::new(XmlNs {
            typ: XML_LOCAL_NAMESPACE,
            href: xml_strdup(XML_XML_NAMESPACE.as_ptr() as _),
            prefix: xml_strdup(c"xml".as_ptr() as _),
            ..Default::default()
        }) else {
            xml_tree_err_memory("allocating the XML namespace");
            return None;
        };
        self.old_ns = Some(ns);
        Some(ns)
    }
}

impl NodeCommon for XmlDoc {
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
        (!self.name.is_null())
            .then(|| unsafe { CStr::from_ptr(self.name as *const i8).to_string_lossy() })
    }
    fn children(&self) -> Option<XmlGenericNodePtr> {
        self.children
            .and_then(|children| XmlGenericNodePtr::from_raw(children.as_ptr()))
    }
    fn set_children(&mut self, children: Option<XmlGenericNodePtr>) {
        self.children = children.and_then(|node| NodePtr::from_ptr(node.as_ptr()));
    }
    fn last(&self) -> Option<XmlGenericNodePtr> {
        self.last
            .and_then(|last| XmlGenericNodePtr::from_raw(last.as_ptr()))
    }
    fn set_last(&mut self, last: Option<XmlGenericNodePtr>) {
        self.last = last.and_then(|node| NodePtr::from_ptr(node.as_ptr()));
    }
    fn next(&self) -> Option<XmlGenericNodePtr> {
        self.next
            .and_then(|next| XmlGenericNodePtr::from_raw(next.as_ptr()))
    }
    fn set_next(&mut self, next: Option<XmlGenericNodePtr>) {
        self.next = next.and_then(|node| NodePtr::from_ptr(node.as_ptr()));
    }
    fn prev(&self) -> Option<XmlGenericNodePtr> {
        self.prev
            .and_then(|prev| XmlGenericNodePtr::from_raw(prev.as_ptr()))
    }
    fn set_prev(&mut self, prev: Option<XmlGenericNodePtr>) {
        self.prev = prev.and_then(|node| NodePtr::from_ptr(node.as_ptr()));
    }
    fn parent(&self) -> Option<XmlGenericNodePtr> {
        self.parent
            .and_then(|parent| XmlGenericNodePtr::from_raw(parent.as_ptr()))
    }
    fn set_parent(&mut self, parent: Option<XmlGenericNodePtr>) {
        self.parent = parent.and_then(|node| NodePtr::from_ptr(node.as_ptr()));
    }
}

impl Default for XmlDoc {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::XmlDocumentNode,
            name: null_mut(),
            children: None,
            last: None,
            parent: None,
            next: None,
            prev: None,
            doc: None,
            compression: 0,
            standalone: 0,
            int_subset: None,
            ext_subset: None,
            old_ns: None,
            version: None,
            encoding: None,
            ids: None,
            refs: None,
            url: None,
            charset: XmlCharEncoding::None,
            psvi: null_mut(),
            parse_flags: 0,
            properties: 0,
        }
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct XmlDocPtr(NonNull<XmlDoc>);

impl XmlDocPtr {
    /// Allocate new memory and create new `XmlDocPtr` from an owned xml node.
    ///
    /// This method leaks allocated memory.  
    /// Users can use `free` method for deallocating memory.
    pub(crate) fn new(node: XmlDoc) -> Option<Self> {
        let boxed = Box::new(node);
        NonNull::new(Box::leak(boxed)).map(Self)
    }

    /// Create `XmlDocPtr` from a raw pointer.  
    ///
    /// If `ptr` is a NULL pointer, return `Ok(None)`.  
    /// If `ptr` is a valid pointer of `XmlDoc`, return `Ok(Some(Self))`.  
    /// Otherwise, return `Err`.
    ///
    /// # Safety
    /// - `ptr` must be a pointer of types that is implemented `NodeCommon` at least.
    ///
    /// # TODO
    /// - fix to private mathod
    pub unsafe fn from_raw(ptr: *mut XmlDoc) -> Result<Option<Self>, InvalidNodePointerCastError> {
        if ptr.is_null() {
            return Ok(None);
        }
        match (*ptr).element_type() {
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                Ok(Some(Self(NonNull::new_unchecked(ptr))))
            }
            _ => Err(InvalidNodePointerCastError {
                from: (*ptr).element_type(),
                to: type_name::<Self>(),
            }),
        }
    }

    pub(crate) fn as_ptr(self) -> *mut XmlDoc {
        self.0.as_ptr()
    }

    /// Deallocate memory.
    ///
    /// # Safety
    /// This method should be called only once.  
    /// If called more than twice, the behavior is undefined.
    pub(crate) unsafe fn free(self) {
        let _ = *Box::from_raw(self.0.as_ptr());
    }

    /// Acquire the ownership of the inner value.  
    /// As a result, `self` will be invalid. `self` must not be used after performs this method.
    ///
    /// # Safety
    /// This method should be called only once.  
    /// If called more than twice, the behavior is undefined.
    pub(crate) unsafe fn into_inner(self) -> Box<XmlDoc> {
        Box::from_raw(self.0.as_ptr())
    }
}

impl Clone for XmlDocPtr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for XmlDocPtr {}

impl Deref for XmlDocPtr {
    type Target = XmlDoc;
    fn deref(&self) -> &Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlDoc`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlDocPtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `*mut XmlDoc`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_mut() }
    }
}

impl TryFrom<XmlGenericNodePtr> for XmlDocPtr {
    type Error = InvalidNodePointerCastError;

    fn try_from(value: XmlGenericNodePtr) -> Result<Self, Self::Error> {
        match value.element_type() {
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                Ok(Self(value.0.cast()))
            }
            _ => Err(InvalidNodePointerCastError {
                from: value.element_type(),
                to: type_name::<Self>(),
            }),
        }
    }
}

impl From<XmlDocPtr> for XmlGenericNodePtr {
    fn from(value: XmlDocPtr) -> Self {
        Self(value.0 as NonNull<dyn NodeCommon>)
    }
}

impl From<XmlDocPtr> for *mut XmlDoc {
    fn from(value: XmlDocPtr) -> Self {
        value.0.as_ptr()
    }
}

/// Creates a new XML document
///
/// Returns a new document
#[doc(alias = "xmlNewDoc")]
pub unsafe fn xml_new_doc(version: Option<&str>) -> Option<XmlDocPtr> {
    let version = version.unwrap_or("1.0");

    // Allocate a new document and fill the fields.
    let Some(mut cur) = XmlDocPtr::new(XmlDoc {
        typ: XmlElementType::XmlDocumentNode,
        version: Some(version.to_owned()),
        standalone: -1,
        compression: -1, /* not initialized */
        parse_flags: 0,
        properties: XmlDocProperties::XmlDocUserbuilt as i32,
        // The in memory encoding is always UTF8
        // This field will never change and would
        // be obsolete if not for binary compatibility.
        charset: XmlCharEncoding::UTF8,
        ..Default::default()
    }) else {
        xml_tree_err_memory("building doc");
        return None;
    };
    cur.doc = Some(cur);
    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    //  && xmlRegisterNodeDefaultValue.is_some()
    {
        xml_register_node_default_value(cur.into());
    }
    Some(cur)
}

/// Do a copy of the document info. If recursive, the content tree will
/// be copied too as well as DTD, namespaces and entities.
///
/// Returns: a new #xmlDocPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyDoc")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub unsafe fn xml_copy_doc(doc: XmlDocPtr, recursive: i32) -> Option<XmlDocPtr> {
    use crate::{
        libxml::globals::xml_mem_strdup,
        tree::{xml_copy_dtd, xml_copy_namespace_list, xml_static_copy_node_list},
    };

    let mut ret = xml_new_doc(doc.version.as_deref())?;
    ret.typ = doc.typ;
    if !doc.name.is_null() {
        ret.name = xml_mem_strdup(doc.name as _) as _;
    }
    ret.encoding = doc.encoding.clone();
    if let Some(url) = doc.url.as_deref() {
        ret.url = Some(url.to_owned());
    }
    ret.charset = doc.charset;
    ret.compression = doc.compression;
    ret.standalone = doc.standalone;
    if recursive == 0 {
        return Some(ret);
    }

    ret.last = None;
    ret.children = None;
    #[cfg(feature = "libxml_tree")]
    if let Some(doc_int_subset) = doc.int_subset {
        ret.int_subset = xml_copy_dtd(doc_int_subset);
        let Some(mut ret_int_subset) = ret.int_subset else {
            xml_free_doc(ret);
            return None;
        };
        ret_int_subset.set_doc(Some(ret));
        ret_int_subset.parent = Some(ret);
    }
    if doc.old_ns.is_some() {
        ret.old_ns = xml_copy_namespace_list(doc.old_ns);
    }
    if let Some(children) = doc.children {
        ret.children = NodePtr::from_ptr(
            xml_static_copy_node_list(
                XmlGenericNodePtr::from_raw(children.as_ptr()),
                Some(ret),
                Some(ret.into()),
            )
            .map_or(null_mut(), |node| node.as_ptr()),
        );
        ret.last = None;
        let mut tmp = ret.children();
        while let Some(now) = tmp {
            if now.next().is_none() {
                ret.set_last(Some(now));
            }
            tmp = now.next();
        }
    }
    Some(ret)
}

/// Free up all the structures used by a document, tree included.
#[doc(alias = "xmlFreeDoc")]
pub unsafe fn xml_free_doc(mut cur: XmlDocPtr) {
    if __XML_REGISTER_CALLBACKS.load(Ordering::Relaxed) != 0
    // && xmlDeregisterNodeDefaultValue.is_some()
    {
        xml_deregister_node_default_value(cur.into());
    }

    // Do this before freeing the children list to avoid ID lookups
    cur.ids.take();
    cur.refs.take();
    let mut ext_subset = cur.ext_subset.take();
    let int_subset = cur.int_subset.take();
    if int_subset == ext_subset {
        ext_subset = None;
    }
    if let Some(mut ext_subset) = ext_subset {
        ext_subset.unlink();
        xml_free_dtd(ext_subset);
    }
    if let Some(mut int_subset) = int_subset {
        int_subset.unlink();
        xml_free_dtd(int_subset);
    }

    if let Some(children) = cur.children() {
        xml_free_node_list(Some(children));
    }
    if let Some(old_ns) = cur.old_ns.take() {
        xml_free_ns_list(old_ns);
    }

    cur.version = None;
    if !cur.name.is_null() {
        xml_free(cur.name as _);
    }
    cur.encoding = None;
    cur.url = None;
    cur.free();
}

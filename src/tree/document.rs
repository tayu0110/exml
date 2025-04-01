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
    ffi::CStr,
    ops::{Deref, DerefMut},
    os::raw::c_void,
    ptr::{NonNull, null_mut},
    sync::atomic::Ordering,
};

use crate::{
    encoding::XmlCharEncoding,
    error::XmlParserErrors,
    hash::XmlHashTable,
    libxml::globals::{xml_deregister_node_default_value, xml_register_node_default_value},
    list::XmlList,
};

use super::{
    __XML_REGISTER_CALLBACKS, InvalidNodePointerCastError, NodeCommon, XML_ENT_EXPANDING,
    XML_ENT_PARSED, XML_LOCAL_NAMESPACE, XML_XML_NAMESPACE, XmlDocProperties, XmlDtdPtr,
    XmlElementType, XmlEntityType, XmlGenericNodePtr, XmlID, XmlNodePtr, XmlNs, XmlNsPtr, XmlRef,
    xml_free_dtd, xml_free_node_list, xml_free_ns_list, xml_get_doc_entity, xml_new_doc_text,
    xml_new_reference, xml_ns_in_scope, xml_tree_err, xml_tree_err_memory,
};

#[repr(C)]
pub struct XmlDoc {
    pub _private: *mut c_void,                  /* application data */
    pub(crate) typ: XmlElementType,             /* XML_DOCUMENT_NODE, must be second ! */
    pub(crate) name: Cow<'static, str>,         /* name/filename/URI of the document */
    pub children: Option<XmlGenericNodePtr>,    /* the document tree */
    pub(crate) last: Option<XmlGenericNodePtr>, /* last child link */
    pub(crate) parent: Option<XmlDocPtr>,       /* child->parent link */
    pub(crate) next: Option<XmlDocPtr>,         /* next sibling link  */
    pub(crate) prev: Option<XmlDocPtr>,         /* previous sibling link  */
    pub(crate) doc: Option<XmlDocPtr>,          /* autoreference to itself */

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
    pub fn search_ns_by_href(&mut self, doc: Option<XmlDocPtr>, href: &str) -> Option<XmlNsPtr> {
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
                let mut cur = cur_node.ns_def;
                while let Some(now) = cur {
                    if now.href.as_deref().is_some_and(|h| h == href)
                        && now.prefix().is_some()
                        && xml_ns_in_scope(doc, Some(orig), node, now.prefix.as_deref()) == 1
                    {
                        return Some(now);
                    }
                    cur = now.next;
                }
                if orig != cur_node.into() {
                    let cur = cur_node.ns;
                    if let Some(cur) = cur.filter(|cur| {
                        cur.href.as_deref().is_some_and(|h| h == href)
                            && (*cur).prefix().is_some()
                            && xml_ns_in_scope(doc, Some(orig), node, cur.prefix.as_deref()) == 1
                    }) {
                        return Some(cur);
                    }
                }
            }
            node = cur_node.parent();
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
                return Some(XmlNodePtr::try_from(now).unwrap());
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
        unsafe {
            assert!(matches!(
                self.element_type(),
                XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
            ));
            let mut buf = String::new();
            self.get_content_to(&mut buf);
            Some(buf)
        }
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
        unsafe {
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
    }

    /// Parse the value string and build the node list associated.  
    /// Should produce a flat tree with only TEXTs and ENTITY_REFs.
    ///
    /// Returns a pointer to the first child.
    #[doc(alias = "xmlStringGetNodeList")]
    pub unsafe fn get_node_list(&self, value: &str) -> Option<XmlNodePtr> {
        unsafe {
            let mut head: Option<XmlNodePtr> = None;
            let mut last: Option<XmlNodePtr> = None;
            let mut buf = String::new();
            let mut cur = value;
            let mut q = cur;
            while !cur.is_empty() {
                if cur.starts_with('&') {
                    let mut charval = 0;

                    // Save the current text.
                    if cur.len() != q.len() {
                        let len = q.len() - cur.len();
                        buf.push_str(&q[..len]);
                    }

                    if cur[1..].starts_with("#x") {
                        cur = &cur[3..];
                        if let Some((value, rem)) = cur.split_once(';').and_then(|(head, tail)| {
                            u32::from_str_radix(head, 16).ok().map(|val| (val, tail))
                        }) {
                            charval = value;
                            cur = rem;
                        } else {
                            xml_tree_err(
                                XmlParserErrors::XmlTreeInvalidHex,
                                XmlGenericNodePtr::from_raw(self as *const XmlDoc as *mut XmlDoc),
                                None,
                            );
                            charval = 0;
                        };
                        q = cur;
                    } else if cur[1..].starts_with('#') {
                        cur = &cur[2..];
                        if let Some((value, rem)) = cur.split_once(';').and_then(|(head, tail)| {
                            head.parse::<u32>().ok().map(|val| (val, tail))
                        }) {
                            charval = value;
                            cur = rem;
                        } else {
                            xml_tree_err(
                                XmlParserErrors::XmlTreeInvalidDec,
                                XmlGenericNodePtr::from_raw(self as *const XmlDoc as *mut XmlDoc),
                                None,
                            );
                            charval = 0;
                        };
                        q = cur;
                    } else {
                        // Read the entity string
                        cur = &cur[1..];
                        q = cur;
                        let Some((entity, rem)) = cur.split_once(';') else {
                            xml_tree_err(
                                XmlParserErrors::XmlTreeUnterminatedEntity,
                                XmlGenericNodePtr::from_raw(self as *const XmlDoc as *mut XmlDoc),
                                Some(q),
                            );
                            if let Some(head) = head {
                                xml_free_node_list(Some(head));
                            }
                            return None;
                        };
                        if !entity.is_empty() {
                            // Predefined entities don't generate nodes
                            let ent = xml_get_doc_entity(
                                XmlDocPtr::from_raw(self as *const XmlDoc as _).unwrap(),
                                entity,
                            );
                            if let Some(ent) = ent.filter(|ent| {
                                matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
                            }) {
                                buf.push_str(
                                    &CStr::from_ptr(ent.content as *const i8).to_string_lossy(),
                                );
                            } else {
                                // Flush buffer so far
                                if !buf.is_empty() {
                                    let Some(mut node) = xml_new_doc_text(
                                        XmlDocPtr::from_raw(self as *const XmlDoc as *mut XmlDoc)
                                            .unwrap(),
                                        None,
                                    ) else {
                                        if let Some(head) = head {
                                            xml_free_node_list(Some(head));
                                        }
                                        return None;
                                    };
                                    node.content.get_or_insert_default().push_str(&buf);
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
                                    entity,
                                ) else {
                                    if let Some(head) = head {
                                        xml_free_node_list(Some(head));
                                    }
                                    return None;
                                };
                                if let Some(mut ent) = ent.filter(|ent| {
                                    ent.flags & XML_ENT_PARSED as i32 == 0
                                        && ent.flags & XML_ENT_EXPANDING as i32 == 0
                                }) {
                                    // The entity should have been checked already,
                                    // but set the flag anyway to avoid recursion.
                                    ent.flags |= XML_ENT_EXPANDING as i32;
                                    let content = node.content.as_deref().unwrap();
                                    ent.set_children(
                                        self.get_node_list(content).map(|node| node.into()),
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
                        }
                        cur = rem;
                        q = cur;
                    }
                    if charval != 0 {
                        if let Some(c) = char::from_u32(charval) {
                            buf.push(c);
                        }
                    }
                } else {
                    let c = cur.chars().next().unwrap();
                    cur = &cur[c.len_utf8()..];
                }
            }
            if cur != q || head.is_none() {
                // Handle the last piece of text.
                let len = q.len() - cur.len();
                buf.push_str(&q[..len]);
            }

            if !buf.is_empty() {
                let Some(mut node) = xml_new_doc_text(
                    XmlDocPtr::from_raw(self as *const XmlDoc as *mut XmlDoc).unwrap(),
                    None,
                ) else {
                    if let Some(head) = head {
                        xml_free_node_list(Some(head));
                    }
                    return None;
                };
                node.content = Some(buf);

                if let Some(mut last) = last {
                    last.add_next_sibling(node.into());
                } else {
                    head = Some(node);
                }
            }

            head
        }
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
        unsafe {
            use crate::tree::{NodeCommon, xml_replace_node};

            (*root).unlink();
            (*root).set_doc(XmlDocPtr::from_raw(self).unwrap());
            (*root).set_parent(XmlGenericNodePtr::from_raw(self));
            let mut old = self.children();
            while let Some(now) = old {
                if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                    break;
                }
                old = now.next();
            }
            if let Some(old) = old {
                xml_replace_node(old, Some(XmlGenericNodePtr::from(root)));
            } else if let Some(children) = self.children() {
                children.add_sibling(XmlGenericNodePtr::from(root));
            } else {
                self.set_children(Some(root.into()));
                self.set_last(Some(root.into()));
            }
            old.map(|o| XmlNodePtr::try_from(o).unwrap())
        }
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
    pub(super) fn ensure_xmldecl(&mut self) -> Option<XmlNsPtr> {
        if let Some(old_ns) = self.old_ns {
            return Some(old_ns);
        }
        let Some(ns) = XmlNsPtr::new(XmlNs {
            typ: XML_LOCAL_NAMESPACE,
            href: Some(XML_XML_NAMESPACE.to_str().unwrap().into()),
            prefix: Some("xml".into()),
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
        Some(Cow::Borrowed(self.name.as_ref()))
    }
    fn children(&self) -> Option<XmlGenericNodePtr> {
        self.children
    }
    fn set_children(&mut self, children: Option<XmlGenericNodePtr>) {
        self.children = children;
    }
    fn last(&self) -> Option<XmlGenericNodePtr> {
        self.last
    }
    fn set_last(&mut self, last: Option<XmlGenericNodePtr>) {
        self.last = last;
    }
    fn next(&self) -> Option<XmlGenericNodePtr> {
        self.next.map(|next| next.into())
    }
    fn set_next(&mut self, next: Option<XmlGenericNodePtr>) {
        self.next = next.map(|node| XmlDocPtr::try_from(node).unwrap())
    }
    fn prev(&self) -> Option<XmlGenericNodePtr> {
        self.prev.map(|prev| prev.into())
    }
    fn set_prev(&mut self, prev: Option<XmlGenericNodePtr>) {
        self.prev = prev.map(|node| XmlDocPtr::try_from(node).unwrap())
    }
    fn parent(&self) -> Option<XmlGenericNodePtr> {
        self.parent.map(|parent| parent.into())
    }
    fn set_parent(&mut self, parent: Option<XmlGenericNodePtr>) {
        self.parent = parent.map(|node| XmlDocPtr::try_from(node).unwrap());
    }
}

impl Default for XmlDoc {
    fn default() -> Self {
        Self {
            _private: null_mut(),
            typ: XmlElementType::XmlDocumentNode,
            name: "".into(),
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
        unsafe {
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
    }

    // pub(crate) fn as_ptr(self) -> *mut XmlDoc {
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
    // pub(crate) unsafe fn into_inner(self) -> Box<XmlDoc> {
    //     unsafe { Box::from_raw(self.0.as_ptr()) }
    // }
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
    unsafe {
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
}

/// Do a copy of the document info. If recursive, the content tree will
/// be copied too as well as DTD, namespaces and entities.
///
/// Returns: a new #xmlDocPtr, or null_mut() in case of error.
#[doc(alias = "xmlCopyDoc")]
#[cfg(any(feature = "libxml_tree", feature = "schema"))]
pub unsafe fn xml_copy_doc(doc: XmlDocPtr, recursive: i32) -> Option<XmlDocPtr> {
    unsafe {
        use crate::tree::{xml_copy_dtd, xml_copy_namespace_list, xml_static_copy_node_list};

        let mut ret = xml_new_doc(doc.version.as_deref())?;
        ret.typ = doc.typ;
        ret.name = doc.name.clone();
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
            ret.children = xml_static_copy_node_list(Some(children), Some(ret), Some(ret.into()));
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
}

/// Free up all the structures used by a document, tree included.
#[doc(alias = "xmlFreeDoc")]
pub unsafe fn xml_free_doc(mut cur: XmlDocPtr) {
    unsafe {
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

        cur.free();
    }
}

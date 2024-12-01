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
    borrow::Cow,
    ffi::{CStr, CString},
    ops::{Deref, DerefMut},
    os::raw::c_void,
    ptr::{null, null_mut, NonNull},
    sync::atomic::Ordering,
};

use libc::memset;

use crate::{
    dict::xml_dict_owns,
    libxml::{
        entities::{
            xml_encode_attribute_entities, xml_encode_entities_reentrant, xml_get_doc_entity,
            XmlEntity, XmlEntityPtr,
        },
        globals::{xml_free, xml_malloc},
        uri::xml_build_uri,
        valid::{xml_get_dtd_attr_desc, xml_get_dtd_qattr_desc, xml_remove_id},
        xmlstring::{
            xml_str_equal, xml_strcat, xml_strdup, xml_strlen, xml_strncat, xml_strncat_new,
            xml_strncmp, XmlChar,
        },
    },
    tree::{xml_free_node_list, XmlAttributePtr},
};

use super::{
    copy_string_for_new_dict_if_needed, xml_buf_cat, xml_buf_create, xml_buf_create_size,
    xml_buf_detach, xml_buf_free, xml_buf_set_allocation_scheme, xml_free_node, xml_free_prop,
    xml_is_blank_char, xml_new_doc_text_len, xml_ns_in_scope, xml_text_merge, xml_tree_err_memory,
    XmlAttr, XmlAttrPtr, XmlAttribute, XmlAttributeType, XmlBufPtr, XmlBufferAllocationScheme,
    XmlDoc, XmlDocPtr, XmlDtd, XmlElement, XmlElementType, XmlNs, XmlNsPtr, XML_CHECK_DTD,
    XML_LOCAL_NAMESPACE, XML_XML_NAMESPACE,
};

pub trait NodeCommon {
    fn element_type(&self) -> XmlElementType;
    fn name(&self) -> Option<Cow<'_, str>>;
    fn children(&self) -> Option<NodePtr>;
    fn set_children(&mut self, children: Option<NodePtr>);
    fn last(&self) -> Option<NodePtr>;
    fn set_last(&mut self, last: Option<NodePtr>);
    fn parent(&self) -> Option<NodePtr>;
    fn set_parent(&mut self, parent: Option<NodePtr>);
    fn next(&self) -> Option<NodePtr>;
    fn set_next(&mut self, next: Option<NodePtr>);
    fn prev(&self) -> Option<NodePtr>;
    fn set_prev(&mut self, prev: Option<NodePtr>);
    fn document(&self) -> *mut XmlDoc;
    fn set_document(&mut self, doc: *mut XmlDoc);

    fn as_node(&self) -> Option<NonNull<XmlNode>> {
        // TODO: Remove unneeded types
        match self.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => NonNull::new(self as *const Self as *mut XmlNode),
            _ => None,
        }
    }
    fn as_attribute_node(&self) -> Option<NonNull<XmlAttr>> {
        match self.element_type() {
            XmlElementType::XmlAttributeNode => NonNull::new(self as *const Self as *mut XmlAttr),
            _ => None,
        }
    }
    fn as_attribute_decl_node(&self) -> Option<NonNull<XmlAttribute>> {
        match self.element_type() {
            XmlElementType::XmlAttributeDecl => {
                NonNull::new(self as *const Self as *mut XmlAttribute)
            }
            _ => None,
        }
    }
    fn as_document_node(&self) -> Option<NonNull<XmlDoc>> {
        match self.element_type() {
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                NonNull::new(self as *const Self as *mut XmlDoc)
            }
            _ => None,
        }
    }
    fn as_dtd_node(&self) -> Option<NonNull<XmlDtd>> {
        match self.element_type() {
            XmlElementType::XmlDTDNode | XmlElementType::XmlDocumentTypeNode => {
                NonNull::new(self as *const Self as *mut XmlDtd)
            }
            _ => None,
        }
    }
    fn as_element_decl_node(&self) -> Option<NonNull<XmlElement>> {
        match self.element_type() {
            XmlElementType::XmlElementDecl => NonNull::new(self as *const Self as *mut XmlElement),
            _ => None,
        }
    }
    fn as_entity_decl_node(&self) -> Option<NonNull<XmlEntity>> {
        match self.element_type() {
            XmlElementType::XmlEntityDecl => NonNull::new(self as *const Self as *mut XmlEntity),
            _ => None,
        }
    }
    fn as_namespace_decl_node(&self) -> Option<NonNull<XmlNs>> {
        match self.element_type() {
            XmlElementType::XmlNamespaceDecl => NonNull::new(self as *const Self as *mut XmlNs),
            _ => None,
        }
    }

    /// Check whether this node is a Text node or not.
    #[doc(alias = "xmlNodeIsText")]
    fn is_text_node(&self) -> bool {
        matches!(self.element_type(), XmlElementType::XmlTextNode)
    }

    /// Searches for the BASE URL. The code should work on both XML
    /// and HTML document even if base mechanisms are completely different.  
    /// It returns the base as defined in RFC 2396 sections
    /// 5.1.1. Base URI within Document Content and  5.1.2. Base URI from the Encapsulating Entity.  
    /// However it does not return the document base (5.1.3), use `doc.url` in this case
    ///
    /// Returns a pointer to the base URL, or NULL if not found.  
    /// It's up to the caller to free the memory with `xml_free()`.
    #[doc(alias = "xmlNodeGetBase")]
    unsafe fn get_base(&self, mut doc: *const XmlDoc) -> *mut XmlChar {
        let mut oldbase: *mut XmlChar = null_mut();
        let mut base: *mut XmlChar;
        let mut newbase: *mut XmlChar;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        if doc.is_null() {
            doc = self.document();
        }
        let mut cur = NodePtr::from_ptr(self as *const Self as *mut XmlNode);
        if !doc.is_null() && matches!((*doc).element_type(), XmlElementType::XmlHTMLDocumentNode) {
            cur = (*doc).children();
            while let Some(now) = cur.filter(|cur| cur.name().is_some()) {
                if !matches!(now.element_type(), XmlElementType::XmlElementNode) {
                    cur = now.next();
                    continue;
                }
                let name = now.name().unwrap();
                if name.eq_ignore_ascii_case("html") {
                    cur = now.children();
                    continue;
                }
                if name.eq_ignore_ascii_case("head") {
                    cur = now.children();
                    continue;
                }
                if name.eq_ignore_ascii_case("base") {
                    return now.get_prop("href");
                }
                cur = now.next();
            }
            return null_mut();
        }
        while let Some(now) = cur {
            if let Some(ent) = now.as_entity_decl_node() {
                return xml_strdup(ent.as_ref().uri.load(Ordering::Relaxed));
            }
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                base = now.get_ns_prop("base", XML_XML_NAMESPACE.to_str().ok());
                if !base.is_null() {
                    if !oldbase.is_null() {
                        newbase = xml_build_uri(oldbase, base);
                        if !newbase.is_null() {
                            xml_free(oldbase as _);
                            xml_free(base as _);
                            oldbase = newbase;
                        } else {
                            xml_free(oldbase as _);
                            xml_free(base as _);
                            return null_mut();
                        }
                    } else {
                        oldbase = base;
                    }
                    if xml_strncmp(oldbase, c"http://".as_ptr() as _, 7) == 0
                        || xml_strncmp(oldbase, c"ftp://".as_ptr() as _, 6) == 0
                        || xml_strncmp(oldbase, c"urn:".as_ptr() as _, 4) == 0
                    {
                        return oldbase;
                    }
                }
            }
            cur = now.parent();
        }
        if !doc.is_null() && (*doc).url.is_some() {
            let url = CString::new((*doc).url.as_deref().unwrap()).unwrap();
            if oldbase.is_null() {
                return xml_strdup(url.as_ptr() as *const u8);
            }
            newbase = xml_build_uri(oldbase, url.as_ptr() as *const u8);
            xml_free(oldbase as _);
            return newbase;
        }
        oldbase
    }

    #[doc(hidden)]
    unsafe fn get_prop_node_value_internal(&self) -> *mut XmlChar {
        if matches!(self.element_type(), XmlElementType::XmlAttributeNode) {
            // Note that we return at least the empty string.
            // TODO: Do we really always want that?
            if let Some(children) = self.children() {
                if children.next().is_none()
                    && matches!(
                        children.element_type(),
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                    )
                {
                    // Optimization for the common case: only 1 text node.
                    return xml_strdup(children.content);
                } else {
                    let ret: *mut XmlChar = children.get_string(self.document(), 1);
                    if !ret.is_null() {
                        return ret;
                    }
                }
            }
            xml_strdup(c"".as_ptr() as _)
        } else if matches!(self.element_type(), XmlElementType::XmlAttributeDecl) {
            xml_strdup(
                self.as_attribute_decl_node()
                    .unwrap()
                    .as_ref()
                    .default_value,
            )
        } else {
            null_mut()
        }
    }

    /// Search the last child of a node.
    /// Returns the last child or null_mut() if none.
    #[doc(alias = "xmlGetLastChild")]
    fn get_last_child(&self) -> XmlNodePtr {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        self.last().map_or(null_mut(), |p| p.as_ptr())
    }

    /// Append the extra substring to the node content.
    ///
    /// # Note
    /// In contrast to xmlNodeSetContent(), @content is supposed to be raw text,
    /// so unescaped XML special chars are allowed, entity references are not supported.
    #[doc(alias = "xmlNodeAddContent")]
    unsafe fn add_content(&mut self, content: *const XmlChar) {
        if content.is_null() {
            return;
        }
        let len: i32 = xml_strlen(content);
        self.add_content_len(content, len);
    }

    /// Append the extra substring to the node content.
    ///
    /// # Note
    /// In contrast to xmlNodeSetContentLen(), `content` is supposed to be raw text,
    /// so unescaped XML special chars are allowed, entity references are not supported.
    #[doc(alias = "xmlNodeAddContentLen")]
    unsafe fn add_content_len(&mut self, content: *const XmlChar, len: i32) {
        if len <= 0 {
            return;
        }
        match self.element_type() {
            XmlElementType::XmlDocumentFragNode | XmlElementType::XmlElementNode => {
                let last = self.last();
                let new_node: XmlNodePtr = xml_new_doc_text_len(self.document(), content, len);
                if !new_node.is_null() {
                    let tmp = self.add_child(new_node);
                    if tmp != new_node {
                        return;
                    }
                    if let Some(last) = last.filter(|l| l.next() == NodePtr::from_ptr(new_node)) {
                        xml_text_merge(last.as_ptr(), new_node);
                    }
                }
            }
            XmlElementType::XmlAttributeNode => {}
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlNotationNode => {
                let mut node = self.as_node().unwrap();
                if !content.is_null() {
                    if node.as_ref().content == &raw mut node.as_mut().properties as _
                        || (!node.as_ref().document().is_null()
                            && !(*node.as_ref().document()).dict.is_null()
                            && xml_dict_owns(
                                (*node.as_ref().document()).dict,
                                node.as_ref().content,
                            ) != 0)
                    {
                        node.as_mut().content =
                            xml_strncat_new(node.as_ref().content, content, len);
                        node.as_mut().properties = null_mut();
                    } else {
                        node.as_mut().content = xml_strncat(node.as_ref().content, content, len);
                    }
                }
            }
            XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {}
            XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl => {}
            _ => unreachable!(),
        }
    }

    /// Add a new node to `self`, at the end of the child (or property) list
    /// merging adjacent TEXT nodes (in which case `cur` is freed)  
    /// If the new node is ATTRIBUTE, it is added into properties instead of children.  
    /// If there is an attribute with equal name, it is first destroyed.  
    ///
    /// All tree manipulation functions can safely move nodes within a document.  
    /// But when moving nodes from one document to another, references to
    /// namespaces in element or attribute nodes are NOT fixed. In this case,
    /// you MUST call xmlReconciliateNs after the move operation to avoid memory errors.
    ///
    /// Returns the child or NULL in case of error.
    #[doc(alias = "xmlAddChild")]
    unsafe fn add_child(&mut self, cur: XmlNodePtr) -> XmlNodePtr {
        let mut prev: XmlNodePtr;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if cur.is_null() || matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if self as *mut Self as *mut XmlNode == cur {
            return null_mut();
        }
        /*
         * If cur is a TEXT node, merge its content with adjacent TEXT nodes
         * cur is then freed.
         */
        if matches!((*cur).typ, XmlElementType::XmlTextNode) {
            if matches!(self.element_type(), XmlElementType::XmlTextNode)
                && !(*(self as *mut Self as *mut XmlNode)).content.is_null()
                && self.name().map_or(null(), |c| c.as_ptr()) == (*cur).name
            {
                (*(self as *mut Self as *mut XmlNode)).add_content((*cur).content);
                xml_free_node(cur);
                return self as *mut Self as *mut XmlNode;
            }
            if let Some(mut last) = self.last().filter(|l| {
                matches!(l.typ, XmlElementType::XmlTextNode)
                    && l.name == (*cur).name
                    && self.last() != NodePtr::from_ptr(cur)
            }) {
                last.add_content((*cur).content);
                xml_free_node(cur);
                return self.last().map_or(null_mut(), |l| l.as_ptr());
            }
        }

        /*
         * add the new element at the end of the children list.
         */
        prev = (*cur).parent.map_or(null_mut(), |p| p.as_ptr());
        (*cur).parent = NodePtr::from_ptr(self as *mut Self as *mut XmlNode);
        if (*cur).doc != self.document() {
            (*cur).set_doc(self.document());
        }
        /* this check prevents a loop on tree-traversions if a developer
         * tries to add a node to its parent multiple times
         */
        if prev == self as *mut Self as *mut XmlNode {
            return cur;
        }

        /*
         * Coalescing
         */
        if matches!(self.element_type(), XmlElementType::XmlTextNode)
            && !(*(self as *mut Self as *mut XmlNode)).content.is_null()
            && self as *mut Self as *mut XmlNode != cur
        {
            (*(self as *mut Self as *mut XmlNode)).add_content((*cur).content);
            xml_free_node(cur);
            return self as *mut Self as *mut XmlNode;
        }
        if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
            if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
                return null_mut();
            }
            if !(*(self as *mut Self as *mut XmlNode)).properties.is_null() {
                /* check if an attribute with the same name exists */

                let lastattr = if (*cur).ns.is_null() {
                    (*(self as *mut Self as *mut XmlNode)).has_ns_prop(
                        CStr::from_ptr((*cur).name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        None,
                    )
                } else {
                    let href = (*(*cur).ns).href;
                    (*(self as *mut Self as *mut XmlNode)).has_ns_prop(
                        CStr::from_ptr((*cur).name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        (!href.is_null())
                            .then(|| CStr::from_ptr(href as *const i8).to_string_lossy())
                            .as_deref(),
                    )
                };
                if !lastattr.is_null()
                    && lastattr != cur as _
                    && !matches!((*lastattr).typ, XmlElementType::XmlAttributeDecl)
                {
                    /* different instance, destroy it (attributes must be unique) */
                    (*lastattr).unlink();
                    xml_free_prop(lastattr);
                }
                if lastattr == cur as _ {
                    return cur;
                }
            }
            if (*(self as *mut Self as *mut XmlNode)).properties.is_null() {
                (*(self as *mut Self as *mut XmlNode)).properties = cur as _;
            } else {
                /* find the end */
                let mut lastattr = (*(self as *mut Self as *mut XmlNode)).properties;
                while !(*lastattr).next.is_null() {
                    lastattr = (*lastattr).next;
                }
                (*lastattr).next = cur as _;
                (*(cur as *mut XmlAttr)).prev = lastattr;
            }
        } else if self.children().is_none() {
            self.set_children(NodePtr::from_ptr(cur));
            self.set_last(NodePtr::from_ptr(cur));
        } else {
            prev = self.last().map_or(null_mut(), |l| l.as_ptr());
            (*prev).next = NodePtr::from_ptr(cur);
            (*cur).prev = NodePtr::from_ptr(prev);
            self.set_last(NodePtr::from_ptr(cur));
        }
        cur
    }

    /// Finds the first child node of that element which is a Element node.
    ///
    /// Note the handling of entities references is different than in
    /// the W3C DOM element traversal spec since we don't have back reference
    /// from entities content to entities references.
    ///
    /// Returns the first element child or NULL if not available.
    #[doc(alias = "xmlFirstElementChild")]
    #[cfg(feature = "libxml_tree")]
    fn first_element_child(&self) -> XmlNodePtr {
        let mut next = match self.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => self.children(),
            _ => {
                return null_mut();
            }
        };
        while let Some(cur) = next {
            if matches!(cur.element_type(), XmlElementType::XmlElementNode) {
                return cur.as_ptr();
            }
            next = cur.next();
        }
        null_mut()
    }

    /// Finds the last child node of that element which is a Element node.
    ///
    /// Note the handling of entities references is different than in
    /// the W3C DOM element traversal spec since we don't have back reference
    /// from entities content to entities references.
    ///
    /// Returns the last element child or NULL if not available.
    #[doc(alias = "xmlLastElementChild")]
    #[cfg(feature = "libxml_tree")]
    fn last_element_child(&self) -> XmlNodePtr {
        let mut cur = match self.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => self.last(),
            _ => {
                return null_mut();
            }
        };
        while let Some(now) = cur {
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                return now.as_ptr();
            }
            cur = now.prev();
        }
        null_mut()
    }

    /// Finds the current number of child nodes of that element which are element nodes.
    ///
    /// Note the handling of entities references is different than in
    /// the W3C DOM element traversal spec since we don't have back reference
    /// from entities content to entities references.
    ///
    /// Returns the count of element child or 0 if not available
    #[doc(alias = "xmlChildElementCount")]
    #[cfg(feature = "libxml_tree")]
    fn child_element_count(&self) -> u64 {
        let mut ret = 0;

        let mut next = match self.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => self.children(),
            _ => {
                return 0;
            }
        };
        while let Some(cur) = next {
            if matches!(cur.element_type(), XmlElementType::XmlElementNode) {
                ret += 1;
            }
            next = cur.next();
        }
        ret
    }

    /// Finds the first closest next sibling of the node which is an element node.
    ///
    /// Note the handling of entities references is different than in
    /// the W3C DOM element traversal spec since we don't have back reference
    /// from entities content to entities references.
    ///
    /// Returns the next element sibling or NULL if not available
    #[doc(alias = "xmlNextElementSibling")]
    #[cfg(feature = "libxml_tree")]
    fn next_element_sibling(&self) -> XmlNodePtr {
        let mut cur = match self.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => self.next(),
            _ => {
                return null_mut();
            }
        };
        while let Some(now) = cur {
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                return now.as_ptr();
            }
            cur = now.next();
        }
        null_mut()
    }

    /// Finds the first closest previous sibling of the node which is an element node.
    ///
    /// Note the handling of entities references is different than in
    /// the W3C DOM element traversal spec since we don't have back reference
    /// from entities content to entities references.
    ///
    /// Returns the previous element sibling or null_mut() if not available
    #[doc(alias = "xmlPreviousElementSibling")]
    #[cfg(feature = "libxml_tree")]
    fn previous_element_sibling(&self) -> XmlNodePtr {
        let mut node = match self.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => self.prev(),
            _ => {
                return null_mut();
            }
        };
        while let Some(now) = node {
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                return now.as_ptr();
            }
            node = now.prev();
        }
        null_mut()
    }

    /// Unlink a node from it's current context, the node is not freed.  
    /// If one need to free the node, use xmlFreeNode() routine after the unlink to discard it.  
    ///
    /// Note that namespace nodes can't be unlinked as they do not have pointer to their parent.
    #[doc(alias = "xmlUnlinkNode")]
    unsafe fn unlink(&mut self) {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return;
        }

        if matches!(self.element_type(), XmlElementType::XmlDTDNode) {
            let doc = self.document();
            if !doc.is_null() {
                if (*doc).int_subset == self as *mut Self as *mut XmlDtd {
                    (*doc).int_subset = null_mut();
                }
                if (*doc).ext_subset == self as *mut Self as *mut XmlDtd {
                    (*doc).ext_subset = null_mut();
                }
            }
        }
        if matches!(self.element_type(), XmlElementType::XmlEntityDecl) {
            let doc = self.document();
            let name = self.name().map(|n| CString::new(n.as_ref()).unwrap());
            if !doc.is_null() {
                if !(*doc).int_subset.is_null() {
                    if let (Some(mut table), Some(name)) =
                        ((*(*doc).int_subset).entities, name.as_deref())
                    {
                        if table.lookup(name).copied() == Some(self as *mut Self as XmlEntityPtr) {
                            table.remove_entry(name, |_, _| {});
                        }
                    }
                    if let (Some(mut table), Some(name)) =
                        ((*(*doc).int_subset).pentities, name.as_deref())
                    {
                        if table.lookup(name).copied() == Some(self as *mut Self as XmlEntityPtr) {
                            table.remove_entry(name, |_, _| {});
                        }
                    }
                }
                if !(*doc).ext_subset.is_null() {
                    if let (Some(mut table), Some(name)) =
                        ((*(*doc).ext_subset).entities, name.as_deref())
                    {
                        if table.lookup(name).copied() == Some(self as *mut Self as XmlEntityPtr) {
                            table.remove_entry(name, |_, _| {});
                        }
                    }
                    if let (Some(mut table), Some(name)) =
                        ((*(*doc).ext_subset).pentities, name.as_deref())
                    {
                        if table.lookup(name).copied() == Some(self as *mut Self as XmlEntityPtr) {
                            table.remove_entry(name, |_, _| {});
                        }
                    }
                }
            }
        }
        if let Some(mut parent) = self.parent() {
            if matches!(self.element_type(), XmlElementType::XmlAttributeNode) {
                if parent.properties == self as *mut Self as *mut XmlAttr {
                    parent.properties = (*(self as *mut Self as *mut XmlAttr)).next;
                }
            } else {
                if parent.children == NodePtr::from_ptr(self as *mut Self as *mut XmlNode) {
                    parent.children = self.next();
                }
                if parent.last == NodePtr::from_ptr(self as *mut Self as *mut XmlNode) {
                    parent.last = self.prev();
                }
            }
            self.set_parent(None);
        }
        if let Some(mut next) = self.next() {
            next.prev = self.prev();
        }
        if let Some(mut prev) = self.prev() {
            prev.next = self.next();
        }
        self.set_next(None);
        self.set_prev(None);
    }
}

/// A node in an XML tree.
pub type XmlNodePtr = *mut XmlNode;
#[repr(C)]
pub struct XmlNode {
    pub _private: *mut c_void,        /* application data */
    pub(crate) typ: XmlElementType,   /* type number, must be second ! */
    pub name: *const XmlChar,         /* the name of the node, or the entity */
    children: Option<NodePtr>,        /* parent->childs link */
    last: Option<NodePtr>,            /* last child link */
    parent: Option<NodePtr>,          /* child->parent link */
    pub next: Option<NodePtr>,        /* next sibling link  */
    pub(crate) prev: Option<NodePtr>, /* previous sibling link  */
    pub doc: *mut XmlDoc,             /* the containing document */

    /* End of common part */
    pub(crate) ns: *mut XmlNs, /* pointer to the associated namespace */
    pub content: *mut XmlChar, /* the content */
    pub(crate) properties: *mut XmlAttr, /* properties list */
    pub ns_def: *mut XmlNs,    /* namespace definitions on this node */
    pub(crate) psvi: *mut c_void, /* for type/PSVI information */
    pub(crate) line: u16,      /* line number */
    pub(crate) extra: u16,     /* extra data for XPath/XSLT */
}

impl XmlNode {
    /// Checks whether this node is an empty or whitespace only (and possibly ignorable) text-node.
    #[doc(alias = "xmlIsBlankNode")]
    pub unsafe fn is_blank_node(&self) -> bool {
        if !matches!(
            self.element_type(),
            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
        ) {
            return false;
        }
        if self.content.is_null() {
            return true;
        }
        let mut cur = self.content;
        while *cur != 0 {
            if !xml_is_blank_char(*cur as u32) {
                return false;
            }
            cur = cur.add(1);
        }

        true
    }

    /// Get line number of `self`.
    /// Try to override the limitation of lines being store in 16 bits ints
    ///
    /// Returns the line number if successful, -1 otherwise
    #[doc(alias = "xmlGetLineNoInternal")]
    unsafe fn get_line_no_internal(&self, depth: i32) -> i64 {
        let mut result: i64 = -1;

        if depth >= 5 {
            return -1;
        }

        if matches!(
            self.element_type(),
            XmlElementType::XmlElementNode
                | XmlElementType::XmlTextNode
                | XmlElementType::XmlCommentNode
                | XmlElementType::XmlPINode
        ) {
            if self.line == 65535 {
                if matches!(self.element_type(), XmlElementType::XmlTextNode)
                    && !self.psvi.is_null()
                {
                    result = self.psvi as isize as i64;
                } else if let Some(children) = self
                    .children()
                    .filter(|_| matches!(self.element_type(), XmlElementType::XmlElementNode))
                {
                    result = children.get_line_no_internal(depth + 1);
                } else if let Some(next) = self.next() {
                    result = next.get_line_no_internal(depth + 1);
                } else if let Some(prev) = self.prev() {
                    result = prev.get_line_no_internal(depth + 1);
                }
            }
            if result == -1 || result == 65535 {
                result = self.line as i64;
            }
        } else if let Some(prev) = self.prev().filter(|p| {
            matches!(
                p.element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlCommentNode
                    | XmlElementType::XmlPINode
            )
        }) {
            result = prev.get_line_no_internal(depth + 1);
        } else if let Some(parent) = self
            .parent()
            .filter(|p| matches!(p.element_type(), XmlElementType::XmlElementNode))
        {
            result = parent.get_line_no_internal(depth + 1);
        }

        result
    }

    /// Get line number of `self`.
    /// Try to override the limitation of lines being store in 16 bits ints
    /// if XML_PARSE_BIG_LINES parser option was used
    ///
    /// Returns the line number if successful, -1 otherwise
    #[doc(alias = "xmlGetLineNo")]
    pub unsafe fn get_line_no(&self) -> i64 {
        self.get_line_no_internal(0)
    }

    /// Build a structure based Path for the given node
    ///
    /// Returns the new path or `None` in case of error.  
    #[doc(alias = "xmlGetNodePath")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn get_node_path(&self) -> Option<String> {
        use crate::libxml::xmlstring::xml_str_equal;

        let mut occur: i32;
        let mut generic: i32;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        let mut buffer = String::with_capacity(500);
        let mut buf = String::with_capacity(500);
        let mut cur = NodePtr::from_ptr(self as *const XmlNode as *mut XmlNode);
        let mut sep: &str;
        while let Some(current) = cur {
            let mut name: Cow<'_, str> = "".into();
            occur = 0;
            let next = if matches!(
                current.element_type(),
                XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
            ) {
                if buffer.starts_with('/') {
                    break;
                }
                sep = "/";
                None
            } else if matches!(current.element_type(), XmlElementType::XmlElementNode) {
                generic = 0;
                sep = "/";
                if !current.ns.is_null() {
                    name = if !(*current.ns).prefix.is_null() {
                        Cow::Owned(format!(
                            "{}:{}",
                            CStr::from_ptr((*current.ns).prefix as *const i8).to_string_lossy(),
                            current.name().unwrap(),
                        ))
                    } else {
                        // We cannot express named elements in the default
                        // namespace, so use "*".
                        generic = 1;
                        "*".into()
                    };
                } else {
                    name = current.name().unwrap();
                }

                // Thumbler index computation
                // TODO: the occurrence test seems bogus for namespaced names
                let mut tmp = current.prev();
                while let Some(now) = tmp {
                    if matches!(now.element_type(), XmlElementType::XmlElementNode)
                        && (generic != 0
                            || (current.name() == now.name()
                                && (now.ns == current.ns
                                    || (!now.ns.is_null()
                                        && !current.ns.is_null()
                                        && xml_str_equal((*current.ns).prefix, (*now.ns).prefix)))))
                    {
                        occur += 1;
                    }
                    tmp = now.prev();
                }
                if occur == 0 {
                    let mut tmp = current.next();
                    while let Some(now) = tmp.filter(|_| occur == 0) {
                        if matches!(now.element_type(), XmlElementType::XmlElementNode)
                            && (generic != 0
                                || (current.name() == now.name()
                                    && (now.ns == current.ns
                                        || (!now.ns.is_null()
                                            && !current.ns.is_null()
                                            && (xml_str_equal(
                                                (*current.ns).prefix,
                                                (*now.ns).prefix,
                                            ))))))
                        {
                            occur += 1;
                        }
                        tmp = now.next();
                    }
                    if occur != 0 {
                        occur = 1;
                    }
                } else {
                    occur += 1;
                }
                current.parent()
            } else if matches!(current.element_type(), XmlElementType::XmlCommentNode) {
                sep = "/";
                name = "comment()".into();

                // Thumbler index computation
                let mut tmp = current.prev();
                while let Some(now) = tmp {
                    if matches!(now.element_type(), XmlElementType::XmlCommentNode) {
                        occur += 1;
                    }
                    tmp = now.prev();
                }
                if occur == 0 {
                    let mut tmp = current.next();
                    while let Some(now) = tmp.filter(|_| occur == 0) {
                        if matches!(now.element_type(), XmlElementType::XmlCommentNode) {
                            occur += 1;
                        }
                        tmp = now.next();
                    }
                    if occur != 0 {
                        occur = 1;
                    }
                } else {
                    occur += 1;
                }
                current.parent()
            } else if matches!(
                current.element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                sep = "/";
                name = "text()".into();

                // Thumbler index computation
                let mut tmp = current.prev();
                while let Some(now) = tmp {
                    if matches!(
                        now.element_type(),
                        XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                    ) {
                        occur += 1;
                    }
                    tmp = now.prev();
                }
                // Evaluate if this is the only text- or CDATA-section-node;
                // if yes, then we'll get "text()".as_ptr() as _, otherwise "text()[1]".
                if occur == 0 {
                    let mut tmp = current.next();
                    while let Some(now) = tmp {
                        if matches!(
                            now.element_type(),
                            XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
                        ) {
                            occur = 1;
                            break;
                        }
                        tmp = now.next();
                    }
                } else {
                    occur += 1;
                }
                current.parent()
            } else if matches!(current.element_type(), XmlElementType::XmlPINode) {
                sep = "/";
                name = Cow::Owned(format!(
                    "processing-instruction('{}')",
                    current.name().unwrap()
                ));

                // Thumbler index computation
                let mut tmp = current.prev();
                while let Some(now) = tmp {
                    if matches!(now.element_type(), XmlElementType::XmlPINode)
                        && current.name() == now.name()
                    {
                        occur += 1;
                    }
                    tmp = now.prev();
                }
                if occur == 0 {
                    let mut tmp = current.next();
                    while let Some(now) = tmp.filter(|_| occur == 0) {
                        if matches!(now.element_type(), XmlElementType::XmlPINode)
                            && current.name() == now.name()
                        {
                            occur += 1;
                        }
                        tmp = now.next();
                    }
                    if occur != 0 {
                        occur = 1;
                    }
                } else {
                    occur += 1;
                }
                current.parent()
            } else if matches!(current.element_type(), XmlElementType::XmlAttributeNode) {
                sep = "/@";
                if !current.ns.is_null() {
                    name = if !(*current.ns).prefix.is_null() {
                        format!(
                            "{}:{}",
                            CStr::from_ptr((*current.ns).prefix as *const i8).to_string_lossy(),
                            current.name().unwrap()
                        )
                        .into()
                    } else {
                        format!("{}", current.name().unwrap()).into()
                    };
                } else {
                    name = current
                        .as_attribute_node()
                        .unwrap()
                        .as_ref()
                        .name()
                        .unwrap();
                }
                current.as_attribute_node().unwrap().as_ref().parent()
            } else {
                return None;
            };

            {
                use std::fmt::Write as _;
                if occur == 0 {
                    write!(buf, "{sep}{name}{buffer}").ok();
                } else {
                    write!(buf, "{sep}{name}[{occur}]{buffer}").ok();
                }
            }
            (buffer, buf) = (buf, buffer);
            buf.clear();
            cur = next;
        }
        Some(buffer)
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
    pub unsafe fn get_content(&self) -> *mut XmlChar {
        match self.element_type() {
            XmlElementType::XmlDocumentFragNode | XmlElementType::XmlElementNode => {
                let buf = xml_buf_create_size(64);
                if buf.is_null() {
                    return null_mut();
                }
                xml_buf_set_allocation_scheme(
                    buf,
                    XmlBufferAllocationScheme::XmlBufferAllocDoubleit,
                );
                self.get_content_to(buf);
                let ret: *mut XmlChar = xml_buf_detach(buf);
                xml_buf_free(buf);
                ret
            }
            XmlElementType::XmlAttributeNode => {
                (*(self as *const XmlNode as *const XmlAttr)).get_prop_node_value_internal()
            }
            XmlElementType::XmlCommentNode | XmlElementType::XmlPINode => {
                if !self.content.is_null() {
                    return xml_strdup(self.content);
                }
                null_mut()
            }
            XmlElementType::XmlEntityRefNode => {
                /* lookup entity declaration */
                let ent: XmlEntityPtr = xml_get_doc_entity(self.doc, self.name);
                if ent.is_null() {
                    return null_mut();
                }

                let buf = xml_buf_create();
                if buf.is_null() {
                    return null_mut();
                }
                xml_buf_set_allocation_scheme(
                    buf,
                    XmlBufferAllocationScheme::XmlBufferAllocDoubleit,
                );

                self.get_content_to(buf);

                let ret: *mut XmlChar = xml_buf_detach(buf);
                xml_buf_free(buf);
                ret
            }
            XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => null_mut(),
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                let buf = xml_buf_create();
                if buf.is_null() {
                    return null_mut();
                }
                xml_buf_set_allocation_scheme(
                    buf,
                    XmlBufferAllocationScheme::XmlBufferAllocDoubleit,
                );

                self.get_content_to(buf);

                let ret: *mut XmlChar = xml_buf_detach(buf);
                xml_buf_free(buf);
                ret
            }
            XmlElementType::XmlNamespaceDecl => {
                let tmp: *mut XmlChar =
                    xml_strdup((*(self as *const XmlNode as *const XmlNs)).href);
                tmp
            }
            XmlElementType::XmlElementDecl => {
                /* TODO !!! */
                null_mut()
            }
            XmlElementType::XmlAttributeDecl => {
                /* TODO !!! */
                null_mut()
            }
            XmlElementType::XmlEntityDecl => {
                /* TODO !!! */
                null_mut()
            }
            XmlElementType::XmlCDATASectionNode | XmlElementType::XmlTextNode => {
                if !self.content.is_null() {
                    return xml_strdup(self.content);
                }
                null_mut()
            }
            _ => unreachable!(),
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
    pub unsafe fn get_content_to(&self, buf: XmlBufPtr) -> i32 {
        if buf.is_null() {
            return -1;
        }
        match self.element_type() {
            XmlElementType::XmlCDATASectionNode | XmlElementType::XmlTextNode => {
                xml_buf_cat(buf, self.content);
            }
            XmlElementType::XmlDocumentFragNode | XmlElementType::XmlElementNode => {
                let mut tmp: *const XmlNode = self;

                while !tmp.is_null() {
                    match (*tmp).element_type() {
                        XmlElementType::XmlCDATASectionNode | XmlElementType::XmlTextNode => {
                            if !(*tmp).content.is_null() {
                                xml_buf_cat(buf, (*tmp).content);
                            }
                        }
                        XmlElementType::XmlEntityRefNode => {
                            (*tmp).get_content_to(buf);
                        }
                        _ => {}
                    }
                    // Skip to next node
                    if let Some(children) = (*tmp).children().filter(|children| {
                        !matches!(children.element_type(), XmlElementType::XmlEntityDecl)
                    }) {
                        tmp = children.as_ptr();
                        continue;
                    }
                    if tmp == self {
                        break;
                    } else {
                        if let Some(next) = (*tmp).next() {
                            tmp = next.as_ptr();
                            continue;
                        }

                        'lp: while {
                            tmp = (*tmp).parent().map_or(null_mut(), |p| p.as_ptr());
                            if tmp.is_null() {
                                break 'lp;
                            }
                            if tmp == self {
                                tmp = null_mut();
                                break 'lp;
                            }
                            if let Some(next) = (*tmp).next() {
                                tmp = next.as_ptr();
                                break 'lp;
                            }

                            !tmp.is_null()
                        } {}
                    }
                }
            }
            XmlElementType::XmlAttributeNode => {
                let attr: XmlAttrPtr = self as *const XmlNode as _;
                let mut tmp = (*attr).children();

                while let Some(now) = tmp {
                    if matches!(now.element_type(), XmlElementType::XmlTextNode) {
                        xml_buf_cat(buf, now.content);
                    } else {
                        now.get_content_to(buf);
                    }
                    tmp = now.next;
                }
            }
            XmlElementType::XmlCommentNode | XmlElementType::XmlPINode => {
                xml_buf_cat(buf, self.content);
            }
            XmlElementType::XmlEntityRefNode => {
                // lookup entity declaration
                let ent: XmlEntityPtr = xml_get_doc_entity(self.document(), self.name);
                if ent.is_null() {
                    return -1;
                }

                // an entity content can be any "well balanced chunk",
                // i.e. the result of the content [43] production:
                // http://www.w3.org/TR/REC-xml#NT-content
                // -> we iterate through child nodes and recursive call
                // xmlNodeGetContent() which handles all possible node types
                let mut tmp = (*ent).children();
                while let Some(now) = tmp {
                    now.get_content_to(buf);
                    tmp = now.next();
                }
            }
            XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {}
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
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
            }
            XmlElementType::XmlNamespaceDecl => {
                xml_buf_cat(buf, self.as_namespace_decl_node().unwrap().as_ref().href);
            }
            XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl => {}
            _ => unreachable!(),
        }
        0
    }

    unsafe fn get_prop_node_internal(
        &self,
        name: &str,
        ns_name: Option<&str>,
        use_dtd: bool,
    ) -> XmlAttrPtr {
        let mut prop: XmlAttrPtr;

        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return null_mut();
        }

        let name = CString::new(name).unwrap();
        if !self.properties.is_null() {
            prop = self.properties;
            if let Some(ns_name) = ns_name {
                // We want the attr to be in the specified namespace.
                let ns_name = CString::new(ns_name).unwrap();
                while {
                    if !(*prop).ns.is_null()
                        && xml_str_equal((*prop).name, name.as_ptr() as *const u8)
                        && ((*(*prop).ns).href == ns_name.as_ptr() as _
                            || xml_str_equal((*(*prop).ns).href, ns_name.as_ptr() as *const u8))
                    {
                        return prop;
                    }
                    prop = (*prop)
                        .next()
                        .map_or(null_mut(), |p| p.as_ptr() as *mut XmlAttr);
                    !prop.is_null()
                } {}
            } else {
                // We want the attr to be in no namespace.
                while {
                    if (*prop).ns.is_null()
                        && xml_str_equal((*prop).name, name.as_ptr() as *const u8)
                    {
                        return prop;
                    }
                    prop = (*prop)
                        .next()
                        .map_or(null_mut(), |p| p.as_ptr() as *mut XmlAttr);
                    !prop.is_null()
                } {}
            }
        }

        #[cfg(feature = "libxml_tree")]
        {
            if !use_dtd {
                return null_mut();
            }
            // Check if there is a default/fixed attribute declaration in
            // the internal or external subset.
            if !self.doc.is_null() && !(*self.doc).int_subset.is_null() {
                let doc: XmlDocPtr = self.doc;
                let mut attr_decl: XmlAttributePtr = null_mut();
                let elem_qname: *mut XmlChar;
                let mut tmpstr: *mut XmlChar = null_mut();

                // We need the QName of the element for the DTD-lookup.
                if !self.ns.is_null() && !(*self.ns).prefix.is_null() {
                    tmpstr = xml_strdup((*self.ns).prefix);
                    tmpstr = xml_strcat(tmpstr, c":".as_ptr() as _);
                    tmpstr = xml_strcat(tmpstr, self.name);
                    if tmpstr.is_null() {
                        return null_mut();
                    }
                    elem_qname = tmpstr;
                } else {
                    elem_qname = self.name as _;
                }
                if let Some(ns_name) = ns_name {
                    if ns_name == XML_XML_NAMESPACE.to_string_lossy() {
                        // The XML namespace must be bound to prefix 'xml'.
                        attr_decl = xml_get_dtd_qattr_desc(
                            (*doc).int_subset,
                            elem_qname,
                            name.as_ptr() as *const u8,
                            c"xml".as_ptr() as _,
                        );
                        if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                            attr_decl = xml_get_dtd_qattr_desc(
                                (*doc).ext_subset,
                                elem_qname,
                                name.as_ptr() as *const u8,
                                c"xml".as_ptr() as _,
                            );
                        }
                    } else {
                        // The ugly case: Search using the prefixes of in-scope
                        // ns-decls corresponding to @nsName.
                        let ns_list: *mut XmlNsPtr = self.get_ns_list(self.doc);
                        if ns_list.is_null() {
                            if !tmpstr.is_null() {
                                xml_free(tmpstr as _);
                            }
                            return null_mut();
                        }
                        let mut cur = ns_list;
                        let ns_name = CString::new(ns_name).unwrap();
                        while !(*cur).is_null() {
                            if xml_str_equal((*(*cur)).href, ns_name.as_ptr() as *const u8) {
                                attr_decl = xml_get_dtd_qattr_desc(
                                    (*doc).int_subset,
                                    elem_qname,
                                    name.as_ptr() as *const u8,
                                    (*(*cur)).prefix,
                                );
                                if !attr_decl.is_null() {
                                    break;
                                }
                                if !(*doc).ext_subset.is_null() {
                                    attr_decl = xml_get_dtd_qattr_desc(
                                        (*doc).ext_subset,
                                        elem_qname,
                                        name.as_ptr() as *const u8,
                                        (*(*cur)).prefix,
                                    );
                                    if !attr_decl.is_null() {
                                        break;
                                    }
                                }
                            }
                            cur = cur.add(1);
                        }
                        xml_free(ns_list as _);
                    }
                } else {
                    /*
                     * The common and nice case: Attr in no namespace.
                     */
                    attr_decl = xml_get_dtd_qattr_desc(
                        (*doc).int_subset,
                        elem_qname,
                        name.as_ptr() as *const u8,
                        null_mut(),
                    );
                    if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                        attr_decl = xml_get_dtd_qattr_desc(
                            (*doc).ext_subset,
                            elem_qname,
                            name.as_ptr() as *const u8,
                            null_mut(),
                        );
                    }
                }
                if !tmpstr.is_null() {
                    xml_free(tmpstr as _);
                }
                /*
                 * Only default/fixed attrs are relevant.
                 */
                if !attr_decl.is_null() && !(*attr_decl).default_value.is_null() {
                    return attr_decl as _;
                }
            }
        }
        null_mut()
    }

    /// Search and get the value of an attribute associated to a node.  
    ///
    /// This does the entity substitution.
    ///
    /// This function looks in DTD attribute declaration for #FIXED or
    /// default declaration values unless DTD use has been turned off.
    ///
    /// # Note
    /// This function acts independently of namespaces associated to the attribute.  
    /// Use xmlGetNsProp() or xmlGetNoNsProp() for namespace aware processing.
    ///
    /// Returns the attribute value or NULL if not found.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlGetProp")]
    pub unsafe fn get_prop(&self, name: &str) -> *mut XmlChar {
        let prop = self.has_prop(name);
        if prop.is_null() {
            return null_mut();
        }
        (*prop).get_prop_node_value_internal()
    }

    /// Search and get the value of an attribute associated to a node.
    ///
    /// This attribute has to be anchored in the namespace specified.
    ///
    /// This does the entity substitution.
    ///
    /// This function looks in DTD attribute declaration for #FIXED or
    /// default declaration values unless DTD use has been turned off.
    ///
    /// Returns the attribute value or NULL if not found.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlGetNsProp")]
    pub unsafe fn get_ns_prop(&self, name: &str, name_space: Option<&str>) -> *mut XmlChar {
        let prop =
            self.get_prop_node_internal(name, name_space, XML_CHECK_DTD.load(Ordering::Relaxed));
        if prop.is_null() {
            return null_mut();
        }
        (*prop).get_prop_node_value_internal()
    }

    /// Search and get the value of an attribute associated to a node
    /// This does the entity substitution.
    ///
    /// This function looks in DTD attribute declaration for #FIXED or
    /// default declaration values unless DTD use has been turned off.  
    ///
    /// This function is similar to xmlGetProp except it will accept only
    /// an attribute in no namespace.
    ///
    /// Returns the attribute value or NULL if not found.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlGetNoNsProp")]
    pub unsafe fn get_no_ns_prop(&self, name: &str) -> *mut XmlChar {
        let prop = self.get_prop_node_internal(name, None, XML_CHECK_DTD.load(Ordering::Relaxed));
        if prop.is_null() {
            return null_mut();
        }
        (*prop).get_prop_node_value_internal()
    }

    /// Search all the namespace applying to a given element.
    ///
    /// Returns an NULL terminated array of all the `xmlNsPtr` found
    /// that need to be freed by the caller or NULL if no namespace if defined.
    #[doc(alias = "xmlGetNsList")]
    #[cfg(any(feature = "libxml_tree", feature = "xpath", feature = "schema"))]
    pub unsafe fn get_ns_list(&self, _doc: *const XmlDoc) -> *mut XmlNsPtr {
        use crate::{
            libxml::{globals::xml_realloc, xmlstring::xml_str_equal},
            tree::xml_tree_err_memory,
        };

        let mut cur: XmlNsPtr;
        let mut ret: *mut XmlNsPtr = null_mut();
        let mut nbns: i32 = 0;
        let mut maxns: i32 = 0;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        let mut node = self as *const XmlNode;
        while !node.is_null() {
            if matches!((*node).element_type(), XmlElementType::XmlElementNode) {
                cur = (*node).ns_def;
                'b: while !cur.is_null() {
                    for i in 0..nbns {
                        if ((*cur).prefix == (*(*ret.add(i as usize))).prefix)
                            || xml_str_equal((*cur).prefix, (*(*ret.add(i as usize))).prefix)
                        {
                            cur = (*cur).next;
                            continue 'b;
                        }
                    }
                    if nbns >= maxns {
                        maxns = if maxns != 0 { maxns * 2 } else { 10 };
                        let tmp: *mut XmlNsPtr =
                            xml_realloc(ret as _, (maxns as usize + 1) * size_of::<XmlNsPtr>())
                                as _;
                        if tmp.is_null() {
                            xml_tree_err_memory(c"getting namespace list".as_ptr() as _);
                            xml_free(ret as _);
                            return null_mut();
                        }
                        ret = tmp;
                    }
                    *ret.add(nbns as usize) = cur;
                    nbns += 1;
                    *ret.add(nbns as usize) = null_mut();
                }
            }
            node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
        }
        ret
    }

    /// Searches the language of a node, i.e. the values of the xml:lang
    /// attribute or the one carried by the nearest ancestor.
    ///
    /// Returns a pointer to the lang value, or null_mut() if not found.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlNodeGetLang")]
    pub unsafe fn get_lang(&self) -> *mut XmlChar {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        let mut cur = self as *const XmlNode;
        while !cur.is_null() {
            let lang = (*cur).get_ns_prop("lang", XML_XML_NAMESPACE.to_str().ok());
            if !lang.is_null() {
                return lang;
            }
            cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        }
        null_mut()
    }

    /// Searches the space preserving behaviour of a node, i.e. the values
    /// of the xml:space attribute or the one carried by the nearest ancestor.
    ///
    /// Returns -1 if xml:space is not inherited, 0 if "default", 1 if "preserve"
    #[doc(alias = "xmlNodeGetSpacePreserve")]
    pub unsafe fn get_space_preserve(&self) -> i32 {
        let mut space: *mut XmlChar;

        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return -1;
        }
        let mut cur = self as *const XmlNode;
        while !cur.is_null() {
            space = (*cur).get_ns_prop("space", XML_XML_NAMESPACE.to_str().ok());
            if !space.is_null() {
                if xml_str_equal(space, c"preserve".as_ptr() as _) {
                    xml_free(space as _);
                    return 1;
                }
                if xml_str_equal(space, c"default".as_ptr() as _) {
                    xml_free(space as _);
                    return 0;
                }
                xml_free(space as _);
            }
            cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        }
        -1
    }

    /// Build the string equivalent to the text contained in the Node list
    /// made of TEXTs and ENTITY_REFs.
    ///
    /// Returns a pointer to the string copy, the caller must free it with `xml_free()`.
    #[doc(alias = "xmlNodeListGetString")]
    pub unsafe fn get_string(&self, doc: XmlDocPtr, in_line: i32) -> *mut XmlChar {
        let mut node: *const XmlNode = self;
        let mut ret: *mut XmlChar = null_mut();
        let mut ent: XmlEntityPtr;

        let attr = self
            .parent()
            .filter(|p| matches!(p.element_type(), XmlElementType::XmlAttributeNode))
            .is_some();

        while !node.is_null() {
            if matches!(
                (*node).element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                if in_line != 0 {
                    ret = xml_strcat(ret, (*node).content);
                } else {
                    let buffer = if attr {
                        xml_encode_attribute_entities(doc, (*node).content)
                    } else {
                        xml_encode_entities_reentrant(doc, (*node).content)
                    };
                    if !buffer.is_null() {
                        ret = xml_strcat(ret, buffer);
                        xml_free(buffer as _);
                    }
                }
            } else if matches!((*node).element_type(), XmlElementType::XmlEntityRefNode) {
                if in_line != 0 {
                    ent = xml_get_doc_entity(doc, (*node).name);
                    if !ent.is_null() {
                        // an entity content can be any "well balanced chunk",
                        // i.e. the result of the content [43] production:
                        // http://www.w3.org/TR/REC-xml#NT-content.
                        // So it can contain text, CDATA section or nested
                        // entity reference nodes (among others).
                        // -> we recursive  call xmlNodeListGetString()
                        // which handles these types
                        let children = (*ent).children();
                        let buffer = if let Some(children) = children {
                            children.get_string(doc, 1)
                        } else {
                            null_mut()
                        };
                        if !buffer.is_null() {
                            ret = xml_strcat(ret, buffer);
                            xml_free(buffer as _);
                        }
                    } else {
                        ret = xml_strcat(ret, (*node).content);
                    }
                } else {
                    let mut buf: [XmlChar; 2] = [0; 2];

                    buf[0] = b'&';
                    buf[1] = 0;
                    ret = xml_strncat(ret, buf.as_ptr() as _, 1);
                    ret = xml_strcat(ret, (*node).name);
                    buf[0] = b';';
                    buf[1] = 0;
                    ret = xml_strncat(ret, buf.as_ptr() as _, 1);
                }
            }
            node = (*node).next().map_or(null_mut(), |n| n.as_ptr());
        }
        ret
    }

    /// Builds the string equivalent to the text contained in the Node list
    /// made of TEXTs and ENTITY_REFs, contrary to `xmlNodeListGetString()`
    /// this function doesn't do any character encoding handling.
    ///
    /// Returns a pointer to the string copy, the caller must free it with `xml_free()`.
    #[doc(alias = "xmlNodeListGetRawString")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn get_raw_string(&self, doc: *const XmlDoc, in_line: i32) -> *mut XmlChar {
        use crate::libxml::entities::xml_encode_special_chars;

        let mut node: *const XmlNode = self;
        let mut ret: *mut XmlChar = null_mut();
        let mut ent: XmlEntityPtr;

        while !node.is_null() {
            if matches!(
                (*node).element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                if in_line != 0 {
                    ret = xml_strcat(ret, (*node).content);
                } else {
                    let buffer: *mut XmlChar = xml_encode_special_chars(doc, (*node).content);
                    if !buffer.is_null() {
                        ret = xml_strcat(ret, buffer);
                        xml_free(buffer as _);
                    }
                }
            } else if matches!((*node).element_type(), XmlElementType::XmlEntityRefNode) {
                if in_line != 0 {
                    ent = xml_get_doc_entity(doc, (*node).name);
                    if !ent.is_null() {
                        // an entity content can be any "well balanced chunk",
                        // i.e. the result of the content [43] production:
                        // http://www.w3.org/TR/REC-xml#NT-content.
                        // So it can contain text, CDATA section or nested
                        // entity reference nodes (among others).
                        // -> we recursive  call xmlNodeListGetRawString()
                        // which handles these types
                        let children = (*ent).children();
                        let buffer = if let Some(children) = children {
                            children.get_raw_string(doc, 1)
                        } else {
                            null_mut()
                        };
                        if !buffer.is_null() {
                            ret = xml_strcat(ret, buffer);
                            xml_free(buffer as _);
                        }
                    } else {
                        ret = xml_strcat(ret, (*node).content);
                    }
                } else {
                    let mut buf: [XmlChar; 2] = [0; 2];

                    buf[0] = b'&';
                    buf[1] = 0;
                    ret = xml_strncat(ret, buf.as_ptr(), 1);
                    ret = xml_strcat(ret, (*node).name);
                    buf[0] = b';';
                    buf[1] = 0;
                    ret = xml_strncat(ret, buf.as_ptr(), 1);
                }
            }
            node = (*node).next().map_or(null_mut(), |n| n.as_ptr());
        }
        ret
    }

    /// Set (or reset) the name of a node.
    #[doc(alias = "xmlNodeSetName")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn set_name(&mut self, name: &str) {
        use crate::{
            dict::{xml_dict_lookup, xml_dict_owns},
            libxml::{globals::xml_free, xmlstring::xml_strdup},
        };

        match self.element_type() {
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {
                return;
            }
            XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl => {}
            _ => unreachable!(),
        }
        let doc = self.document();
        let dict = if !doc.is_null() {
            (*doc).dict
        } else {
            null_mut()
        };
        let mut freeme: *const XmlChar = null_mut();
        let name = CString::new(name).unwrap();
        if !dict.is_null() {
            if !self.name.is_null() && xml_dict_owns(dict, self.name) == 0 {
                freeme = self.name;
            }
            self.name = xml_dict_lookup(dict, name.as_ptr() as *const u8, -1);
        } else {
            if !self.name.is_null() {
                freeme = self.name;
            }
            self.name = xml_strdup(name.as_ptr() as *const u8);
        }

        if !freeme.is_null() {
            xml_free(freeme as _);
        }
    }

    /// Set (or reset) an attribute carried by a node.
    ///
    /// If `name` has a prefix, then the corresponding namespace-binding will be used,
    /// if in scope; it is an error it there's no such ns-binding for the prefix in scope.
    ///
    /// Returns the attribute pointer.
    #[doc(alias = "xmlSetProp")]
    #[cfg(any(
        feature = "libxml_tree",
        feature = "xinclude",
        feature = "schema",
        feature = "html"
    ))]
    pub unsafe fn set_prop(&mut self, name: &str, value: Option<&str>) -> XmlAttrPtr {
        use crate::libxml::xmlstring::xml_strndup;

        use super::xml_split_qname3;

        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return null_mut();
        }

        // handle QNames
        let mut len: i32 = 0;
        let cname = CString::new(name).unwrap();
        let nqname: *const XmlChar = xml_split_qname3(cname.as_ptr() as *const u8, &raw mut len);
        if !nqname.is_null() {
            let prefix: *mut XmlChar = xml_strndup(cname.as_ptr() as *const u8, len);
            let ns = self.search_ns(self.document(), Some(&name[..len as usize]));
            if !prefix.is_null() {
                xml_free(prefix as _);
            }
            if !ns.is_null() {
                return self.set_ns_prop(
                    ns,
                    CStr::from_ptr(nqname as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                    value,
                );
            }
        }
        self.set_ns_prop(null_mut(), name, value)
    }

    /// Remove an attribute carried by a node.  
    /// This handles only attributes in no namespace.
    ///
    /// Returns 0 if successful, -1 if not found
    #[doc(alias = "xmlUnsetProp")]
    #[cfg(any(feature = "libxml_tree", feature = "schema"))]
    pub unsafe fn unset_prop(&mut self, name: &str) -> i32 {
        let prop = self.get_prop_node_internal(name, None, false);
        if prop.is_null() {
            return -1;
        }
        (*prop).unlink();
        xml_free_prop(prop);
        0
    }

    /// Set (or reset) an attribute carried by a node.  
    /// The ns structure must be in scope, this is not checked.
    ///
    /// Returns the attribute pointer.
    #[doc(alias = "xmlSetNsProp")]
    #[cfg(any(
        feature = "libxml_tree",
        feature = "xinclude",
        feature = "schema",
        feature = "html"
    ))]
    pub unsafe fn set_ns_prop(
        &mut self,
        ns: XmlNsPtr,
        name: &str,
        value: Option<&str>,
    ) -> XmlAttrPtr {
        use std::ptr::null;

        use crate::{
            libxml::valid::{xml_add_id, xml_remove_id},
            tree::{xml_free_node_list, xml_new_doc_text, xml_new_prop_internal, XmlAttributeType},
        };

        if !ns.is_null() && (*ns).href.is_null() {
            return null_mut();
        }
        let href = if !ns.is_null() {
            (*ns).href as *const i8
        } else {
            null_mut()
        };
        let prop = self.get_prop_node_internal(
            name,
            (!href.is_null())
                .then(|| CStr::from_ptr(href).to_string_lossy())
                .as_deref(),
            false,
        );
        if !prop.is_null() {
            // Modify the attribute's value.
            if matches!((*prop).atype, Some(XmlAttributeType::XmlAttributeID)) {
                xml_remove_id(self.document(), prop);
                (*prop).atype = Some(XmlAttributeType::XmlAttributeID);
            }
            if let Some(children) = (*prop).children() {
                xml_free_node_list(children.as_ptr());
            }
            (*prop).set_children(None);
            (*prop).set_last(None);
            (*prop).ns = ns;
            if let Some(value) = value {
                let value = CString::new(value).unwrap();
                (*prop).set_children(NodePtr::from_ptr(xml_new_doc_text(
                    self.doc,
                    value.as_ptr() as *const u8,
                )));
                (*prop).set_last(None);
                let mut tmp = (*prop).children();
                while let Some(mut now) = tmp {
                    now.parent = NodePtr::from_ptr(prop as *mut XmlNode);
                    if now.next().is_none() {
                        (*prop).set_last(Some(now));
                    }
                    tmp = now.next();
                }
            }
            if matches!((*prop).atype, Some(XmlAttributeType::XmlAttributeID)) {
                let value = value.map(|v| CString::new(v).unwrap());
                xml_add_id(
                    null_mut(),
                    self.document(),
                    value.as_deref().map_or(null(), |v| v.as_ptr() as *const u8),
                    prop,
                );
            }
            return prop;
        }
        // No equal attr found; create a new one.
        let name = CString::new(name).unwrap();
        let value = value.map(|v| CString::new(v).unwrap());
        xml_new_prop_internal(
            self,
            ns,
            name.as_ptr() as *const u8,
            value.as_deref().map_or(null(), |v| v.as_ptr() as *const u8),
            0,
        )
    }

    /// Remove an attribute carried by a node.
    ///
    /// Returns 0 if successful, -1 if not found
    #[doc(alias = "xmlUnsetNsProp")]
    #[cfg(any(feature = "libxml_tree", feature = "schema"))]
    pub unsafe fn unset_ns_prop(&mut self, ns: XmlNsPtr, name: &str) -> i32 {
        let href = if !ns.is_null() {
            (*ns).href as *const i8
        } else {
            null_mut()
        };
        let prop = self.get_prop_node_internal(
            name,
            (!href.is_null())
                .then(|| CStr::from_ptr(href).to_string_lossy())
                .as_deref(),
            false,
        );
        if prop.is_null() {
            return -1;
        }
        (*prop).unlink();
        xml_free_prop(prop);
        0
    }

    /// Associate a namespace to a node, a posteriori.
    #[doc(alias = "xmlSetNs")]
    pub fn set_ns(&mut self, ns: XmlNsPtr) {
        if matches!(
            self.element_type(),
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode
        ) {
            self.ns = ns;
        }
    }

    /// Set (or reset) the base URI of a node, i.e. the value of the xml:base attribute.
    #[doc(alias = "xmlNodeSetBase")]
    #[cfg(any(feature = "libxml_tree", feature = "xinclude"))]
    pub unsafe fn set_base(&mut self, uri: Option<&str>) {
        use std::ffi::CStr;

        use crate::libxml::uri::xml_path_to_uri;

        match self.element_type() {
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlPINode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {
                return;
            }
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {}
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                let doc = self.as_document_node().unwrap().as_mut();

                doc.url = uri.and_then(|uri| {
                    let uri = CString::new(uri).unwrap();
                    let uri = xml_path_to_uri(uri.as_ptr() as *const u8);
                    (!uri.is_null()).then(|| {
                        let url = CStr::from_ptr(uri as *const i8)
                            .to_string_lossy()
                            .into_owned();
                        xml_free(uri as _);
                        url
                    })
                });
                return;
            }
            _ => unreachable!(),
        }

        let ns = self.search_ns_by_href(self.document(), XML_XML_NAMESPACE.to_str().unwrap());
        if ns.is_null() {
            return;
        }
        if let Some(uri) = uri {
            let curi = CString::new(uri).unwrap();
            let fixed: *mut XmlChar = xml_path_to_uri(curi.as_ptr() as *const u8);
            if !fixed.is_null() {
                self.set_ns_prop(
                    ns,
                    "base",
                    Some(
                        CStr::from_ptr(fixed as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    ),
                );
                xml_free(fixed as _);
            } else {
                self.set_ns_prop(ns, "base", Some(uri));
            }
        } else {
            self.set_ns_prop(ns, "base", None);
        }
    }

    /// Set the language of a node, i.e. the values of the xml:lang
    /// attribute.
    #[doc(alias = "xmlNodeSetLang")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn set_lang(&mut self, lang: Option<&str>) {
        match self.element_type() {
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlPINode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {
                return;
            }
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {}
            _ => unreachable!(),
        }
        let ns = self.search_ns_by_href(self.doc, XML_XML_NAMESPACE.to_str().unwrap());
        if ns.is_null() {
            return;
        }
        self.set_ns_prop(ns, "lang", lang);
    }

    /// Set (or reset) the space preserving behaviour of a node,   
    /// i.e. the value of the xml:space attribute.
    #[doc(alias = "xmlNodeSetSpacePreserve")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn set_space_preserve(&mut self, val: i32) {
        match self.element_type() {
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlPINode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {
                return;
            }
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {}
            _ => unreachable!(),
        }
        let ns = self.search_ns_by_href(self.doc, XML_XML_NAMESPACE.to_str().unwrap());
        if ns.is_null() {
            return;
        }
        match val {
            0 => {
                self.set_ns_prop(ns, "space", Some("default"));
            }
            1 => {
                self.set_ns_prop(ns, "space", Some("preserve"));
            }
            _ => {}
        }
    }

    /// Replace the content of a node.
    ///
    /// # Note
    /// `content` is supposed to be a piece of XML CDATA, so it allows entity references,
    /// but XML special chars need to be escaped first by using `xmlEncodeEntitiesReentrant()`
    /// resp. `xmlEncodeSpecialChars()`.
    #[doc(alias = "xmlNodeSetContent")]
    pub unsafe fn set_content(&mut self, content: *const XmlChar) {
        match self.element_type() {
            XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode => {
                if let Some(children) = self.children() {
                    xml_free_node_list(children.as_ptr());
                }
                self.children = (!self.document().is_null())
                    .then(|| NodePtr::from_ptr((*self.document()).get_node_list(content)))
                    .flatten();
                if let Some(mut ulccur) = self.children() {
                    while let Some(next) = ulccur.next() {
                        ulccur.set_parent(NodePtr::from_ptr(self as *mut XmlNode));
                        ulccur = next;
                    }
                    ulccur.set_parent(NodePtr::from_ptr(self as *mut XmlNode));
                    self.set_last(Some(ulccur));
                } else {
                    self.set_last(None);
                }
            }
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode => {
                if !self.content.is_null()
                    && (self.content != &raw mut self.properties as _)
                    && !(!self.document().is_null()
                        && !(*self.document()).dict.is_null()
                        && xml_dict_owns((*self.document()).dict, self.content) != 0)
                {
                    xml_free(self.content as _);
                }
                if let Some(children) = self.children() {
                    xml_free_node_list(children.as_ptr());
                }
                self.set_last(None);
                self.set_children(None);
                if !content.is_null() {
                    self.content = xml_strdup(content);
                } else {
                    self.content = null_mut();
                }
                self.properties = null_mut();
            }
            XmlElementType::XmlDocumentNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {}
            XmlElementType::XmlNotationNode => {}
            XmlElementType::XmlDTDNode => {}
            XmlElementType::XmlNamespaceDecl => {}
            XmlElementType::XmlElementDecl => { /* TODO !!! */ }
            XmlElementType::XmlAttributeDecl => { /* TODO !!! */ }
            XmlElementType::XmlEntityDecl => { /* TODO !!! */ }
            _ => unreachable!(),
        }
    }

    /// Replace the content of a node.
    ///
    /// # Note
    /// `content` is supposed to be a piece of XML CDATA, so it allows entity
    /// references, but XML special chars need to be escaped first by using
    /// xmlEncodeEntitiesReentrant() resp. xmlEncodeSpecialChars().
    #[doc(alias = "xmlNodeSetContentLen")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn set_content_len(&mut self, content: *const XmlChar, len: i32) {
        use crate::libxml::xmlstring::xml_strndup;

        use super::xml_free_node_list;

        match self.element_type() {
            XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode => {
                if let Some(children) = self.children() {
                    xml_free_node_list(children.as_ptr());
                }
                self.children = (!self.document().is_null())
                    .then(|| {
                        NodePtr::from_ptr(
                            (*self.document()).get_node_list_with_strlen(content, len),
                        )
                    })
                    .flatten();
                if let Some(mut ulccur) = self.children() {
                    while let Some(next) = ulccur.next() {
                        ulccur.set_parent(NodePtr::from_ptr(self as *mut XmlNode));
                        ulccur = next;
                    }
                    ulccur.set_parent(NodePtr::from_ptr(self as *mut XmlNode));
                    self.set_last(Some(ulccur));
                } else {
                    self.set_last(None);
                }
            }
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlNotationNode => {
                if (!self.content.is_null() && self.content != &raw mut self.properties as _)
                    && (!(!self.document().is_null()
                        && !(*self.document()).dict.is_null()
                        && xml_dict_owns((*self.document()).dict, self.content) != 0))
                {
                    xml_free(self.content as _);
                }
                if let Some(children) = self.children() {
                    xml_free_node_list(children.as_ptr());
                }
                self.set_children(None);
                self.set_last(None);
                if !content.is_null() {
                    self.content = xml_strndup(content, len);
                } else {
                    self.content = null_mut();
                }
                self.properties = null_mut();
            }
            XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlHTMLDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {}
            XmlElementType::XmlElementDecl => { /* TODO !!! */ }
            XmlElementType::XmlAttributeDecl => { /* TODO !!! */ }
            XmlElementType::XmlEntityDecl => { /* TODO !!! */ }
            _ => unreachable!(),
        }
    }

    /// update all nodes under the tree to point to the right document
    #[doc(alias = "xmlSetTreeDoc")]
    pub unsafe fn set_doc(&mut self, doc: XmlDocPtr) {
        let mut prop: XmlAttrPtr;

        if self.element_type() == XmlElementType::XmlNamespaceDecl {
            return;
        }
        if self.document() != doc {
            let old_tree_dict = if !self.document().is_null() {
                (*self.document()).dict
            } else {
                null_mut()
            };
            let new_dict = if !doc.is_null() {
                (*doc).dict
            } else {
                null_mut()
            };

            if matches!(self.element_type(), XmlElementType::XmlElementNode) {
                prop = self.properties;
                while !prop.is_null() {
                    if matches!((*prop).atype, Some(XmlAttributeType::XmlAttributeID)) {
                        xml_remove_id(self.document(), prop);
                    }

                    if (*prop).document() != doc {
                        let old_prop_dict = if !(*prop).doc.is_null() {
                            (*(*prop).document()).dict
                        } else {
                            null_mut()
                        };
                        (*prop).name = copy_string_for_new_dict_if_needed(
                            old_prop_dict,
                            new_dict,
                            (*prop).name,
                        );
                        (*prop).set_document(doc);
                    }
                    if let Some(mut children) = (*prop).children() {
                        children.set_doc_all_sibling(doc);
                    }

                    // TODO: ID attributes should be also added to the new
                    //       document, but this breaks things like xmlReplaceNode.
                    //       The underlying problem is that xmlRemoveID is only called
                    //       if a node is destroyed, not if it's unlinked.
                    // if (xmlIsID(doc, tree, prop)) {
                    //     XmlChar *idVal = xmlNodeListGetString(doc, (*prop).children,
                    //                                           1);
                    //     xmlAddID(null_mut(), doc, idVal, prop);
                    // }

                    prop = (*prop).next;
                }
            }
            if matches!(self.element_type(), XmlElementType::XmlEntityRefNode) {
                // Clear 'children' which points to the entity declaration
                // from the original document.
                self.set_children(None);
            } else if let Some(mut children) = self.children() {
                children.set_doc_all_sibling(doc);
            }

            self.name = copy_string_for_new_dict_if_needed(old_tree_dict, new_dict, self.name);
            self.content =
                copy_string_for_new_dict_if_needed(old_tree_dict, null_mut(), self.content) as _;
            /* FIXME: self.ns should be updated as in xmlStaticCopyNode(). */
            self.set_document(doc);
        }
    }

    /// update all nodes in the list to point to the right document
    #[doc(alias = "xmlSetListDoc")]
    pub unsafe fn set_doc_all_sibling(&mut self, doc: XmlDocPtr) {
        if self.element_type() == XmlElementType::XmlNamespaceDecl {
            return;
        }
        let mut cur = NodePtr::from_ptr(self as *mut XmlNode);
        while let Some(mut now) = cur {
            if now.document() != doc {
                now.set_doc(doc);
            }
            cur = now.next();
        }
    }

    /// Search an attribute associated to a node.  
    ///
    /// This function also looks in DTD attribute declaration for #FIXED or
    /// default declaration values unless DTD use has been turned off.
    ///
    /// Returns the attribute or the attribute declaration or NULL if neither was found.
    #[doc(alias = "xmlHasProp")]
    pub unsafe fn has_prop(&self, name: &str) -> XmlAttrPtr {
        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return null_mut();
        }
        // Check on the properties attached to the node
        let name = CString::new(name).unwrap();
        let mut prop = self.properties;
        while !prop.is_null() {
            if xml_str_equal((*prop).name, name.as_ptr() as *const u8) {
                return prop;
            }
            prop = (*prop).next;
        }
        if !XML_CHECK_DTD.load(Ordering::Relaxed) {
            return null_mut();
        }

        // Check if there is a default declaration in the internal or external subsets
        let doc = self.document();
        if !doc.is_null() && !(*doc).int_subset.is_null() {
            let mut attr_decl =
                xml_get_dtd_attr_desc((*doc).int_subset, self.name, name.as_ptr() as *const u8);
            if attr_decl.is_null() && !(*doc).ext_subset.is_null() {
                attr_decl =
                    xml_get_dtd_attr_desc((*doc).ext_subset, self.name, name.as_ptr() as *const u8);
            }
            if !attr_decl.is_null() && !(*attr_decl).default_value.is_null() {
                // return attribute declaration only if a default value is given
                // (that includes #FIXED declarations)
                return attr_decl as *mut XmlAttr;
            }
        }
        null_mut()
    }

    /// Search for an attribute associated to a node.
    ///
    /// This attribute has to be anchored in the namespace specified.  
    /// This does the entity substitution.  
    /// This function looks in DTD attribute declaration for #FIXED or
    /// default declaration values unless DTD use has been turned off.  
    ///
    /// Note that a namespace of NULL indicates to use the default namespace.
    ///
    /// Returns the attribute or the attribute declaration or NULL if neither was found.
    #[doc(alias = "xmlHasNsProp")]
    pub unsafe fn has_ns_prop(&self, name: &str, namespace: Option<&str>) -> XmlAttrPtr {
        self.get_prop_node_internal(name, namespace, XML_CHECK_DTD.load(Ordering::Relaxed))
    }

    /// Add a new element `elem` to the list of siblings of `self`
    /// merging adjacent TEXT nodes (`elem` may be freed)  
    /// If the new element was already inserted in a document
    /// it is first unlinked from its existing context.
    ///
    /// See the note regarding namespaces in xmlAddChild.
    ///
    /// Returns the new element or NULL in case of error.
    #[doc(alias = "xmlAddSibling")]
    pub unsafe fn add_sibling(&mut self, elem: XmlNodePtr) -> XmlNodePtr {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if elem.is_null() || ((*elem).element_type() == XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        let cur = self as *mut XmlNode;
        if cur == elem {
            return null_mut();
        }
        let mut cur = NodePtr::from_ptr(cur).unwrap();

        // Constant time is we can rely on the -> parent -> last to find the last sibling.
        if let Some(parent) = self.parent().filter(|p| {
            !matches!(self.element_type(), XmlElementType::XmlAttributeNode)
                && p.children().is_some()
                && p.last().filter(|l| l.next().is_none()).is_some()
        }) {
            cur = parent.last().unwrap();
        } else {
            while let Some(next) = cur.next() {
                cur = next;
            }
        }

        (*elem).unlink();

        if matches!((*cur).element_type(), XmlElementType::XmlTextNode)
            && matches!((*elem).element_type(), XmlElementType::XmlTextNode)
            && cur.name() == (*elem).name()
        {
            (*cur).add_content((*elem).content);
            xml_free_node(elem);
            return cur.as_ptr();
        } else if matches!((*elem).element_type(), XmlElementType::XmlAttributeNode) {
            return add_prop_sibling(cur.as_ptr(), cur.as_ptr(), elem);
        }

        if (*elem).document() != cur.document() {
            (*elem).set_doc((*cur).document());
        }
        let parent = cur.parent();
        (*elem).set_prev(Some(cur));
        (*elem).set_next(None);
        (*elem).set_parent(parent);
        (*cur).set_next(NodePtr::from_ptr(elem));
        if let Some(mut parent) = parent {
            parent.set_last(NodePtr::from_ptr(elem));
        }

        elem
    }

    /// Add a new node `elem` as the previous sibling of `self`
    /// merging adjacent TEXT nodes (`elem` may be freed)  
    /// If the new node was already inserted in a document it is
    /// first unlinked from its existing context.  
    /// If the new node is ATTRIBUTE, it is added into properties instead of children.  
    /// If there is an attribute with equal name, it is first destroyed.
    ///
    /// See the note regarding namespaces in xmlAddChild.
    ///
    /// Returns the new node or null_mut() in case of error.
    #[doc(alias = "xmlAddPrevSibling")]
    #[cfg(any(
        feature = "libxml_tree",
        feature = "html",
        feature = "schema",
        feature = "xinclude"
    ))]
    pub unsafe fn add_prev_sibling(&mut self, elem: XmlNodePtr) -> XmlNodePtr {
        use crate::libxml::{
            globals::xml_free,
            xmlstring::{xml_strcat, xml_strdup},
        };

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        if elem.is_null() || ((*elem).element_type() == XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if self as *mut XmlNode == elem {
            return null_mut();
        }

        (*elem).unlink();

        if matches!((*elem).element_type(), XmlElementType::XmlTextNode) {
            if matches!(self.element_type(), XmlElementType::XmlTextNode) {
                let mut tmp: *mut XmlChar;

                tmp = xml_strdup((*elem).content);
                tmp = xml_strcat(tmp, self.content);
                self.set_content(tmp);
                xml_free(tmp as _);
                xml_free_node(elem);
                return self as *mut XmlNode;
            }
            if let Some(mut prev) = self.prev().filter(|p| {
                matches!(p.element_type(), XmlElementType::XmlTextNode) && self.name == p.name
            }) {
                prev.add_content((*elem).content);
                xml_free_node(elem);
                return prev.as_ptr();
            }
        } else if matches!((*elem).element_type(), XmlElementType::XmlAttributeNode) {
            return add_prop_sibling(self.prev.map_or(null_mut(), |p| p.as_ptr()), self, elem);
        }

        if (*elem).document() != self.document() {
            (*elem).set_doc(self.document());
        }
        (*elem).set_parent(self.parent());
        (*elem).set_prev(self.prev());
        self.set_prev(NodePtr::from_ptr(elem));
        (*elem).next = Some(NodePtr::from(self));
        if let Some(mut prev) = (*elem).prev() {
            prev.set_next(NodePtr::from_ptr(elem));
        }
        if let Some(mut parent) = (*elem).parent().filter(|p| p.children() == (*elem).next()) {
            parent.set_children(NodePtr::from_ptr(elem));
        }
        elem
    }

    /// Add a new node `elem` as the next sibling of `self`.  
    /// If the new node was already inserted in a document it is
    /// first unlinked from its existing context.
    /// As a result of text merging `elem` may be freed.  
    /// If the new node is ATTRIBUTE, it is added into properties instead of children.  
    /// If there is an attribute with equal name, it is first destroyed.  
    ///
    /// See the note regarding namespaces in xmlAddChild.
    ///
    /// Returns the new node or NULL in case of error.
    #[doc(alias = "xmlAddNextSibling")]
    pub unsafe extern "C" fn add_next_sibling(&mut self, elem: XmlNodePtr) -> XmlNodePtr {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        if elem.is_null() || ((*elem).element_type() == XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if self as *mut XmlNode == elem {
            return null_mut();
        }

        (*elem).unlink();

        if matches!((*elem).element_type(), XmlElementType::XmlTextNode) {
            if matches!(self.element_type(), XmlElementType::XmlTextNode) {
                self.add_content((*elem).content);
                xml_free_node(elem);
                return self as *mut XmlNode;
            }
            if let Some(mut next) = self.next().filter(|next| {
                matches!(next.element_type(), XmlElementType::XmlTextNode) && self.name == next.name
            }) {
                let mut tmp = xml_strdup((*elem).content);
                tmp = xml_strcat(tmp, next.content);
                next.set_content(tmp);
                xml_free(tmp as _);
                xml_free_node(elem);
                return next.as_ptr();
            }
        } else if matches!((*elem).element_type(), XmlElementType::XmlAttributeNode) {
            return add_prop_sibling(self, self, elem);
        }

        if (*elem).document() != self.document() {
            (*elem).set_doc(self.document());
        }
        (*elem).set_parent(self.parent());
        (*elem).set_prev(NodePtr::from_ptr(self as *mut XmlNode));
        (*elem).set_next(self.next());
        self.next = NodePtr::from_ptr(elem);
        if let Some(mut next) = (*elem).next() {
            next.set_prev(NodePtr::from_ptr(elem));
        }
        if let Some(mut parent) = (*elem)
            .parent()
            .filter(|p| p.last() == NodePtr::from_ptr(self as *mut XmlNode))
        {
            parent.set_last(NodePtr::from_ptr(elem));
        }
        elem
    }

    /// Add a list of node at the end of the child list of the parent
    /// merging adjacent TEXT nodes (`cur` may be freed)
    ///
    /// See the note regarding namespaces in xmlAddChild.
    ///
    /// Returns the last child or NULL in case of error.
    #[doc(alias = "xmlAddChildList")]
    pub unsafe fn add_child_list(&mut self, mut cur: XmlNodePtr) -> XmlNodePtr {
        let mut prev: XmlNodePtr;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if cur.is_null() || matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        // add the first element at the end of the children list.
        if self.children().is_none() {
            self.set_children(NodePtr::from_ptr(cur));
        } else {
            // If cur and self.last both are TEXT nodes, then merge them.
            if matches!((*cur).element_type(), XmlElementType::XmlTextNode)
                && matches!(
                    self.last().unwrap().element_type(),
                    XmlElementType::XmlTextNode
                )
                && (*cur).name() == self.last.unwrap().name()
            {
                self.last().unwrap().add_content((*cur).content);
                // if it's the only child, nothing more to be done.
                let Some(next) = (*cur).next() else {
                    xml_free_node(cur);
                    return self.last().map_or(null_mut(), |l| l.as_ptr());
                };
                prev = cur;
                cur = next.as_ptr();
                xml_free_node(prev);
            }
            prev = self.last().map_or(null_mut(), |l| l.as_ptr());
            (*prev).set_next(NodePtr::from_ptr(cur));
            (*cur).set_prev(NodePtr::from_ptr(prev));
        }
        while let Some(next) = (*cur).next() {
            (*cur).set_parent(NodePtr::from_ptr(self as *mut XmlNode));
            if (*cur).document() != self.document() {
                (*cur).set_doc(self.document());
            }
            cur = next.as_ptr();
        }
        (*cur).set_parent(NodePtr::from_ptr(self as *mut XmlNode));
        // the parent may not be linked to a doc !
        if (*cur).document() != self.document() {
            (*cur).set_doc(self.document());
        }
        self.set_last(NodePtr::from_ptr(cur));

        cur
    }

    /// Search a Ns registered under a given name space for a document.  
    /// recurse on the parents until it finds the defined namespace or return NULL otherwise.  
    /// `namespace` can be NULL, this is a search for the default namespace.
    ///
    /// We don't allow to cross entities boundaries.  
    /// If you don't declare the namespace within those you will be in troubles !!!  
    /// A warning is generated to cover this case.
    ///
    /// Returns the namespace pointer or NULL.
    #[doc(alias = "xmlSearchNs")]
    pub unsafe fn search_ns(&mut self, mut doc: XmlDocPtr, namespace: Option<&str>) -> XmlNsPtr {
        let mut cur: XmlNsPtr;
        let orig: *const XmlNode = self;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        if namespace == Some("xml") {
            if doc.is_null() && matches!(self.element_type(), XmlElementType::XmlElementNode) {
                // The XML-1.0 namespace is normally held on the root element.
                // In this case exceptionally create it on the node element.
                cur = xml_malloc(size_of::<XmlNs>()) as _;
                if cur.is_null() {
                    xml_tree_err_memory(c"searching namespace".as_ptr() as _);
                    return null_mut();
                }
                memset(cur as _, 0, size_of::<XmlNs>());
                (*cur).typ = XML_LOCAL_NAMESPACE;
                (*cur).href = xml_strdup(XML_XML_NAMESPACE.as_ptr() as _);
                (*cur).prefix = xml_strdup(c"xml".as_ptr() as _);
                (*cur).next = self.ns_def;
                self.ns_def = cur;
                return cur;
            }
            if doc.is_null() {
                doc = self.document();
                if doc.is_null() {
                    return null_mut();
                }
            }
            // Return the XML namespace declaration held by the doc.
            if (*doc).old_ns.is_null() {
                return (*doc).ensure_xmldecl() as _;
            } else {
                return (*doc).old_ns;
            }
        }
        let mut node = self as *mut XmlNode;
        while !node.is_null() {
            if matches!(
                (*node).element_type(),
                XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode
                    | XmlElementType::XmlEntityDecl
            ) {
                return null_mut();
            }
            if matches!((*node).element_type(), XmlElementType::XmlElementNode) {
                cur = (*node).ns_def;
                while !cur.is_null() {
                    if (*cur).prefix.is_null() && namespace.is_none() && !(*cur).href.is_null() {
                        return cur;
                    }
                    if !(*cur).prefix.is_null()
                        && !(*cur).href.is_null()
                        && namespace
                            .filter(|&n| {
                                n == CStr::from_ptr((*cur).prefix as *const i8).to_string_lossy()
                            })
                            .is_some()
                    {
                        return cur;
                    }
                    cur = (*cur).next;
                }
                if orig != node {
                    cur = (*node).ns;
                    if !cur.is_null() {
                        if (*cur).prefix.is_null() && namespace.is_none() && !(*cur).href.is_null()
                        {
                            return cur;
                        }
                        if !(*cur).prefix.is_null()
                            && !(*cur).href.is_null()
                            && namespace
                                .filter(|&n| {
                                    n == CStr::from_ptr((*cur).prefix as *const i8)
                                        .to_string_lossy()
                                })
                                .is_some()
                        {
                            return cur;
                        }
                    }
                }
            }
            node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
        }
        null_mut()
    }

    /// Search a Ns aliasing a given URI.
    /// Recurse on the parents until it finds the defined namespace or return NULL otherwise.
    ///
    /// Returns the namespace pointer or NULL.
    #[doc(alias = "xmlSearchNsByHref")]
    pub unsafe fn search_ns_by_href(&mut self, mut doc: XmlDocPtr, href: &str) -> XmlNsPtr {
        let mut cur: XmlNsPtr;
        let orig: XmlNodePtr = self;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }
        if href == XML_XML_NAMESPACE.to_str().unwrap() {
            // Only the document can hold the XML spec namespace.
            if doc.is_null() && matches!(self.element_type(), XmlElementType::XmlElementNode) {
                // The XML-1.0 namespace is normally held on the root element.
                // In this case exceptionally create it on the node element.
                cur = xml_malloc(size_of::<XmlNs>()) as _;
                if cur.is_null() {
                    xml_tree_err_memory(c"searching namespace".as_ptr() as _);
                    return null_mut();
                }
                memset(cur as _, 0, size_of::<XmlNs>());
                (*cur).typ = XML_LOCAL_NAMESPACE;
                (*cur).href = xml_strdup(XML_XML_NAMESPACE.as_ptr() as _);
                (*cur).prefix = xml_strdup(c"xml".as_ptr() as _);
                (*cur).next = self.ns_def;
                self.ns_def = cur;
                return cur;
            }
            if doc.is_null() {
                doc = self.document();
                if doc.is_null() {
                    return null_mut();
                }
            }
            /*
            	* Return the XML namespace declaration held by the doc.
            	*/
            if (*doc).old_ns.is_null() {
                return (*doc).ensure_xmldecl();
            } else {
                return (*doc).old_ns;
            }
        }
        let is_attr = matches!(self.element_type(), XmlElementType::XmlAttributeNode);
        let mut node = self as *mut XmlNode;
        while !node.is_null() {
            if matches!(
                (*node).element_type(),
                XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode
                    | XmlElementType::XmlEntityDecl
            ) {
                return null_mut();
            }
            if matches!((*node).element_type(), XmlElementType::XmlElementNode) {
                let href = CString::new(href).unwrap();
                cur = (*node).ns_def;
                while !cur.is_null() {
                    if !(*cur).href.is_null()
                        && xml_str_equal((*cur).href, href.as_ptr() as *const u8)
                        && (!is_attr || !(*cur).prefix.is_null())
                        && xml_ns_in_scope(doc, orig, node, (*cur).prefix) == 1
                    {
                        return cur;
                    }
                    cur = (*cur).next;
                }
                if orig != node {
                    cur = (*node).ns;
                    if !cur.is_null()
                        && !(*cur).href.is_null()
                        && xml_str_equal((*cur).href, href.as_ptr() as *const u8)
                        && (!is_attr || !(*cur).prefix.is_null())
                        && xml_ns_in_scope(doc, orig, node, (*cur).prefix) == 1
                    {
                        return cur;
                    }
                }
            }
            node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
        }
        null_mut()
    }

    /// This function checks that all the namespaces declared within the given tree are properly declared.
    /// This is needed for example after Copy or Cut and then paste operations.
    ///
    /// The subtree may still hold pointers to namespace declarations outside the subtree or invalid/masked.
    ///
    /// As much as possible the function try to reuse the existing namespaces found in the new environment.
    /// If not possible the new namespaces are redeclared on `tree` at the top of the given subtree.
    ///
    /// Returns the number of namespace declarations created or -1 in case of error.
    #[doc(alias = "xmlReconciliateNs")]
    #[cfg(feature = "libxml_tree")]
    pub unsafe fn reconciliate_ns(&mut self, doc: XmlDocPtr) -> i32 {
        use crate::libxml::globals::xml_realloc;

        use super::xml_new_reconciled_ns;

        let mut old_ns: *mut XmlNsPtr = null_mut();
        let mut new_ns: *mut XmlNsPtr = null_mut();
        let mut size_cache: i32 = 0;
        let mut nb_cache: i32 = 0;
        let mut n: XmlNsPtr;
        let mut node: XmlNodePtr = self;
        let mut attr: XmlAttrPtr;
        let ret: i32 = 0;

        if !matches!((*node).element_type(), XmlElementType::XmlElementNode) {
            return -1;
        }
        if doc.is_null() || !matches!((*doc).element_type(), XmlElementType::XmlDocumentNode) {
            return -1;
        }
        if (*node).document() != doc {
            return -1;
        }
        while !node.is_null() {
            // Reconciliate the node namespace
            if !(*node).ns.is_null() {
                // initialize the cache if needed
                if size_cache == 0 {
                    size_cache = 10;
                    old_ns = xml_malloc(size_cache as usize * size_of::<XmlNsPtr>()) as _;
                    if old_ns.is_null() {
                        xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                        return -1;
                    }
                    new_ns = xml_malloc(size_cache as usize * size_of::<XmlNsPtr>()) as _;
                    if new_ns.is_null() {
                        xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                        xml_free(old_ns as _);
                        return -1;
                    }
                }
                let mut f = false;
                for i in 0..nb_cache {
                    if *old_ns.add(i as usize) == (*node).ns {
                        (*node).ns = *new_ns.add(i as usize);
                        f = true;
                        break;
                    }
                }
                if !f {
                    // OK we need to recreate a new namespace definition
                    n = xml_new_reconciled_ns(doc, self, (*node).ns);
                    if !n.is_null() {
                        // :-( what if else ???
                        // check if we need to grow the cache buffers.
                        if size_cache <= nb_cache {
                            size_cache *= 2;
                            old_ns = xml_realloc(
                                old_ns as _,
                                size_cache as usize * size_of::<XmlNsPtr>(),
                            ) as _;
                            if old_ns.is_null() {
                                xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                                xml_free(new_ns as _);
                                return -1;
                            }
                            new_ns = xml_realloc(
                                new_ns as _,
                                size_cache as usize * size_of::<XmlNsPtr>(),
                            ) as _;
                            if new_ns.is_null() {
                                xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                                xml_free(old_ns as _);
                                return -1;
                            }
                        }
                        *new_ns.add(nb_cache as usize) = n;
                        *old_ns.add(nb_cache as usize) = (*node).ns;
                        nb_cache += 1;
                        (*node).ns = n;
                    }
                }
            }
            // now check for namespace held by attributes on the node.
            if matches!((*node).typ, XmlElementType::XmlElementNode) {
                attr = (*node).properties;
                while !attr.is_null() {
                    if !(*attr).ns.is_null() {
                        // initialize the cache if needed
                        if size_cache == 0 {
                            size_cache = 10;
                            old_ns = xml_malloc(size_cache as usize * size_of::<XmlNsPtr>()) as _;
                            if old_ns.is_null() {
                                xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                                return -1;
                            }
                            new_ns = xml_malloc(size_cache as usize * size_of::<XmlNsPtr>()) as _;
                            if new_ns.is_null() {
                                xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                                xml_free(old_ns as _);
                                return -1;
                            }
                        }
                        let mut f = false;
                        for i in 0..nb_cache {
                            if *old_ns.add(i as usize) == (*attr).ns {
                                (*attr).ns = *new_ns.add(i as usize);
                                f = true;
                                break;
                            }
                        }
                        if !f {
                            // OK we need to recreate a new namespace definition
                            n = xml_new_reconciled_ns(doc, self, (*attr).ns);
                            if !n.is_null() {
                                // :-( what if else ???
                                // check if we need to grow the cache buffers.
                                if size_cache <= nb_cache {
                                    size_cache *= 2;
                                    old_ns = xml_realloc(
                                        old_ns as _,
                                        size_cache as usize * size_of::<XmlNsPtr>(),
                                    ) as _;
                                    if old_ns.is_null() {
                                        xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                                        xml_free(new_ns as _);
                                        return -1;
                                    }
                                    new_ns = xml_realloc(
                                        new_ns as _,
                                        size_cache as usize * size_of::<XmlNsPtr>(),
                                    ) as _;
                                    if new_ns.is_null() {
                                        xml_tree_err_memory(c"fixing namespaces".as_ptr() as _);
                                        xml_free(old_ns as _);
                                        return -1;
                                    }
                                }
                                *new_ns.add(nb_cache as usize) = n;
                                *old_ns.add(nb_cache as usize) = (*attr).ns;
                                nb_cache += 1;
                                (*attr).ns = n;
                            }
                        }
                    }
                    attr = (*attr).next;
                }
            }

            // Browse the full subtree, deep first
            if let Some(children) = (*node)
                .children()
                .filter(|_| !matches!((*node).element_type(), XmlElementType::XmlEntityRefNode))
            {
                // deep first
                node = children.as_ptr();
            } else if let Some(next) = (*node).next().filter(|_| node != self) {
                // then siblings
                node = next.as_ptr();
            } else if node != self {
                // go up to parents->next if needed
                while node != self {
                    if (*node).parent().is_some() {
                        node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
                    }
                    if let Some(next) = (*node).next().filter(|_| node != self) {
                        node = next.as_ptr();
                        break;
                    }
                    if (*node).parent().is_none() {
                        node = null_mut();
                        break;
                    }
                }
                // exit condition
                if node == self {
                    node = null_mut();
                }
            } else {
                break;
            }
        }
        if !old_ns.is_null() {
            xml_free(old_ns as _);
        }
        if !new_ns.is_null() {
            xml_free(new_ns as _);
        }
        ret
    }
}

impl NodeCommon for XmlNode {
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
        self.parent
    }
    fn set_parent(&mut self, parent: Option<NodePtr>) {
        self.parent = parent;
    }
}

pub struct NodePtr(NonNull<XmlNode>);

impl NodePtr {
    pub(crate) fn from_ptr(ptr: *mut XmlNode) -> Option<Self> {
        NonNull::new(ptr).map(Self)
    }

    pub fn as_ptr(&self) -> *mut XmlNode {
        self.0.as_ptr()
    }
}

impl Clone for NodePtr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for NodePtr {}

impl PartialEq for NodePtr {
    fn eq(&self, other: &Self) -> bool {
        self.0.as_ptr() == other.0.as_ptr()
    }
}

impl Deref for NodePtr {
    type Target = XmlNode;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for NodePtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

impl From<&XmlNode> for NodePtr {
    fn from(value: &XmlNode) -> Self {
        // Safety
        // `value` is a reference,
        // it is not NULL as long as the constraint of references is observed.
        unsafe {
            Self(NonNull::new_unchecked(
                value as *const XmlNode as *mut XmlNode,
            ))
        }
    }
}

impl From<&mut XmlNode> for NodePtr {
    fn from(value: &mut XmlNode) -> Self {
        // Safety
        // `value` is a reference,
        // it is not NULL as long as the constraint of references is observed.
        unsafe {
            Self(NonNull::new_unchecked(
                value as *const XmlNode as *mut XmlNode,
            ))
        }
    }
}

/// Add a new attribute after `prev` using `cur` as base attribute.  
/// When inserting before `cur`, `prev` is passed as `cur.prev`.  
/// When inserting after `cur`, `prev` is passed as `cur`.  
/// If an existing attribute is found it is destroyed prior to adding `prop`.  
///
/// See the note regarding namespaces in xmlAddChild.
///
/// Returns the attribute being inserted or NULL in case of error.
#[doc(alias = "xmlAddPropSibling")]
unsafe fn add_prop_sibling(prev: XmlNodePtr, cur: XmlNodePtr, prop: XmlNodePtr) -> XmlNodePtr {
    if cur.is_null()
        || !matches!((*cur).typ, XmlElementType::XmlAttributeNode)
        || prop.is_null()
        || !matches!((*prop).typ, XmlElementType::XmlAttributeNode)
        || (!prev.is_null() && !matches!((*prev).typ, XmlElementType::XmlAttributeNode))
    {
        return null_mut();
    }

    /* check if an attribute with the same name exists */
    let attr = if (*prop).ns.is_null() {
        (*cur).parent.expect("Internal Error").has_ns_prop(
            CStr::from_ptr((*prop).name as *const i8)
                .to_string_lossy()
                .as_ref(),
            None,
        )
    } else {
        let href = (*(*prop).ns).href;
        (*cur).parent.expect("Internal Error").has_ns_prop(
            CStr::from_ptr((*prop).name as *const i8)
                .to_string_lossy()
                .as_ref(),
            (!href.is_null())
                .then(|| CStr::from_ptr(href as *const i8).to_string_lossy())
                .as_deref(),
        )
    };

    if (*prop).doc != (*cur).doc {
        (*prop).set_doc((*cur).doc);
    }
    (*prop).parent = (*cur).parent;
    (*prop).prev = NodePtr::from_ptr(prev);
    if !prev.is_null() {
        (*prop).next = (*prev).next;
        (*prev).next = NodePtr::from_ptr(prop);
        if let Some(mut next) = (*prop).next {
            next.prev = NodePtr::from_ptr(prop);
        }
    } else {
        (*prop).next = NodePtr::from_ptr(cur);
        (*cur).prev = NodePtr::from_ptr(prop);
    }
    if let Some(mut parent) = (*prop).parent.filter(|_| (*prop).prev.is_none()) {
        parent.properties = prop as _;
    }
    if !attr.is_null() && ((*attr).typ != XmlElementType::XmlAttributeDecl) {
        /* different instance, destroy it (attributes must be unique) */
        (*attr).remove_prop();
    }
    prop
}

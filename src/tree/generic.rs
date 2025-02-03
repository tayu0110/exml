use std::{
    borrow::Cow,
    ffi::CStr,
    ops::{Deref, DerefMut},
    ptr::{null, null_mut, NonNull},
    sync::atomic::Ordering,
};

use crate::{
    libxml::xmlstring::{xml_strlen, xml_strncat, XmlChar},
    uri::build_uri,
};

use super::{
    xml_free_node, xml_free_prop, xml_new_doc_text_len, xml_text_merge, NodePtr, XmlAttr,
    XmlAttrPtr, XmlAttribute, XmlDoc, XmlDocPtr, XmlDtd, XmlDtdPtr, XmlElement, XmlElementType,
    XmlEntity, XmlNode, XmlNs, XML_XML_NAMESPACE,
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
    unsafe fn get_base(&self, mut doc: *const XmlDoc) -> Option<String> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
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
            return None;
        }
        let mut bases: Vec<String> = vec![];
        while let Some(now) = cur {
            if let Some(ent) = now.as_entity_decl_node() {
                let ret = ent.as_ref().uri.load(Ordering::Relaxed);
                return (!ret.is_null()).then(|| {
                    CStr::from_ptr(ret as *const i8)
                        .to_string_lossy()
                        .into_owned()
                });
            }
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                let base = now.get_ns_prop("base", XML_XML_NAMESPACE.to_str().ok());
                if let Some(base) = base {
                    if base.starts_with("http://")
                        || base.starts_with("ftp://")
                        || base.starts_with("urn:")
                    {
                        return bases
                            .into_iter()
                            .rev()
                            .try_fold(base, |s, v| build_uri(&v, &s));
                    }
                    bases.push(base);
                }
            }
            cur = now.parent();
        }
        if !doc.is_null() && (*doc).url.is_some() {
            let url = (*doc).url.as_deref().unwrap();
            if bases.is_empty() {
                return Some(url.to_owned());
            }
            let base = build_uri(&bases.pop().unwrap(), url)?;
            return bases
                .into_iter()
                .rev()
                .try_fold(base, |base, uri| build_uri(&uri, &base));
        }
        let base = bases.pop()?;
        bases
            .into_iter()
            .rev()
            .try_fold(base, |base, uri| build_uri(&uri, &base))
    }

    #[doc(hidden)]
    unsafe fn get_prop_node_value_internal(&self) -> Option<String> {
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
                    return (!children.content.is_null()).then(|| {
                        CStr::from_ptr(children.content as *const i8)
                            .to_string_lossy()
                            .into_owned()
                    });
                } else if let Some(ret) =
                    children.get_string(XmlDocPtr::from_raw(self.document()).unwrap(), 1)
                {
                    return Some(ret);
                }
            }
            Some("".to_owned())
        } else if matches!(self.element_type(), XmlElementType::XmlAttributeDecl) {
            let def = self
                .as_attribute_decl_node()
                .unwrap()
                .as_ref()
                .default_value;
            (!def.is_null()).then(|| {
                CStr::from_ptr(def as *const i8)
                    .to_string_lossy()
                    .into_owned()
            })
        } else {
            None
        }
    }

    /// Search the last child of a node.
    /// Returns the last child or null_mut() if none.
    #[doc(alias = "xmlGetLastChild")]
    fn get_last_child(&self) -> *mut XmlNode {
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
                let new_node: *mut XmlNode = xml_new_doc_text_len(
                    XmlDocPtr::from_raw(self.document()).unwrap(),
                    content,
                    len,
                );
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
                    node.as_mut().content = xml_strncat(node.as_ref().content, content, len);
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
    unsafe fn add_child(&mut self, cur: *mut XmlNode) -> *mut XmlNode {
        let mut prev: *mut XmlNode;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if cur.is_null() || matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if self as *mut Self as *mut XmlNode == cur {
            return null_mut();
        }
        // If cur is a TEXT node, merge its content with adjacent TEXT nodes cur is then freed.
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

        // add the new element at the end of the children list.
        prev = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        (*cur).set_parent(NodePtr::from_ptr(self as *mut Self as *mut XmlNode));
        if (*cur).doc != self.document() {
            (*cur).set_doc(XmlDocPtr::from_raw(self.document()).unwrap());
        }
        // this check prevents a loop on tree-traversions if a developer
        // tries to add a node to its parent multiple times
        if prev == self as *mut Self as *mut XmlNode {
            return cur;
        }

        // Coalescing
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
            if (*(self as *mut Self as *mut XmlNode)).properties.is_some() {
                // check if an attribute with the same name exists

                let lastattr = if let Some(ns) = (*cur).ns {
                    let href = ns.href;
                    (*(self as *mut Self as *mut XmlNode)).has_ns_prop(
                        CStr::from_ptr((*cur).name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        (!href.is_null())
                            .then(|| CStr::from_ptr(href as *const i8).to_string_lossy())
                            .as_deref(),
                    )
                } else {
                    (*(self as *mut Self as *mut XmlNode)).has_ns_prop(
                        CStr::from_ptr((*cur).name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        None,
                    )
                };
                if let Some(mut lastattr) = lastattr.and_then(|attr| attr.ok()).filter(|&attr| {
                    attr != XmlAttrPtr::from_raw(cur as *mut XmlAttr).unwrap().unwrap()
                }) {
                    // different instance, destroy it (attributes must be unique)
                    (*lastattr).unlink();
                    xml_free_prop(lastattr);
                }
                match lastattr {
                    Some(Ok(attr)) if attr.as_ptr() == cur as *mut XmlAttr => return cur,
                    Some(Err(attr)) if attr.as_ptr() == cur as *mut XmlAttribute => return cur,
                    _ => {}
                }
            }
            if let Some(mut lastattr) = (*(self as *mut Self as *mut XmlNode)).properties {
                // find the end
                while let Some(next) = lastattr.next {
                    lastattr = next;
                }
                lastattr.next = XmlAttrPtr::from_raw(cur as _).unwrap();
                (*(cur as *mut XmlAttr)).prev = Some(lastattr);
            } else {
                (*(self as *mut Self as *mut XmlNode)).properties =
                    XmlAttrPtr::from_raw(cur as _).unwrap();
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
    fn first_element_child(&self) -> *mut XmlNode {
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
    fn last_element_child(&self) -> *mut XmlNode {
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
    fn next_element_sibling(&self) -> *mut XmlNode {
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
    fn previous_element_sibling(&self) -> *mut XmlNode {
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
            let dtd = XmlDtdPtr::from_raw(self as *mut Self as *mut XmlDtd).unwrap();
            let doc = self.document();
            if !doc.is_null() {
                if (*doc).int_subset == dtd {
                    (*doc).int_subset = None;
                }
                if (*doc).ext_subset == dtd {
                    (*doc).ext_subset = None;
                }
            }
        }
        if matches!(self.element_type(), XmlElementType::XmlEntityDecl) {
            let doc = self.document();
            let name = self.name().map(|n| n.into_owned());
            if !doc.is_null() {
                if let Some(int_subset) = (*doc).int_subset {
                    if let (Some(mut table), Some(name)) = (int_subset.entities, name.as_deref()) {
                        if table.lookup(name).copied().map(|e| e.as_ptr())
                            == Some(self as *mut Self as *mut XmlEntity)
                        {
                            table.remove_entry(name, |_, _| {});
                        }
                    }
                    if let (Some(mut table), Some(name)) = (int_subset.pentities, name.as_deref()) {
                        if table.lookup(name).copied().map(|e| e.as_ptr())
                            == Some(self as *mut Self as *mut XmlEntity)
                        {
                            table.remove_entry(name, |_, _| {});
                        }
                    }
                }
                if let Some(ext_subset) = (*doc).ext_subset {
                    if let (Some(mut table), Some(name)) = (ext_subset.entities, name.as_deref()) {
                        if table.lookup(name).copied().map(|e| e.as_ptr())
                            == Some(self as *mut Self as *mut XmlEntity)
                        {
                            table.remove_entry(name, |_, _| {});
                        }
                    }
                    if let (Some(mut table), Some(name)) = (ext_subset.pentities, name.as_deref()) {
                        if table.lookup(name).copied().map(|e| e.as_ptr())
                            == Some(self as *mut Self as *mut XmlEntity)
                        {
                            table.remove_entry(name, |_, _| {});
                        }
                    }
                }
            }
        }
        if let Some(mut parent) = self.parent() {
            if matches!(self.element_type(), XmlElementType::XmlAttributeNode) {
                if parent.properties.map_or(null_mut(), |prop| prop.as_ptr())
                    == self as *mut Self as *mut XmlAttr
                {
                    parent.properties = (*(self as *mut Self as *mut XmlAttr)).next;
                }
            } else {
                if parent.children() == NodePtr::from_ptr(self as *mut Self as *mut XmlNode) {
                    parent.set_children(self.next());
                }
                if parent.last() == NodePtr::from_ptr(self as *mut Self as *mut XmlNode) {
                    parent.set_last(self.prev());
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

pub(crate) struct XmlGenericNodePtr(pub(super) NonNull<dyn NodeCommon>);

impl XmlGenericNodePtr {
    /// Allocate new memory and create new `XmlGenericNodePtr` from an owned xml node.
    ///
    /// This method leaks allocated memory.  
    /// Users can use `free` method for deallocating memory.
    pub(crate) fn new<T: NodeCommon + 'static>(node: T) -> Option<Self> {
        let boxed: Box<dyn NodeCommon> = Box::new(node);
        NonNull::new(Box::leak(boxed)).map(Self)
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
    pub(crate) unsafe fn into_inner(self) -> Box<dyn NodeCommon> {
        Box::from_raw(self.0.as_ptr())
    }
}

impl Clone for XmlGenericNodePtr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for XmlGenericNodePtr {}

impl Deref for XmlGenericNodePtr {
    type Target = dyn NodeCommon;
    fn deref(&self) -> &Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `XmlGenericNodePtr`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlGenericNodePtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // # Safety
        // I don't implement the pointer casting and addition/subtraction methods
        // and don't expose the inner `NonNull` for `XmlGenericNodePtr`.
        // Therefore, as long as the constructor is correctly implemented,
        // the pointer dereference is valid.
        unsafe { self.0.as_mut() }
    }
}

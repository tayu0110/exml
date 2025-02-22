use std::{
    borrow::Cow,
    ffi::CStr,
    ops::{Deref, DerefMut},
    ptr::{null_mut, NonNull},
    sync::atomic::Ordering,
};

use crate::{
    libxml::{
        valid::xml_remove_id,
        xmlstring::{xml_strdup, xml_strlen, xml_strncat, XmlChar},
    },
    uri::build_uri,
};

use super::{
    add_prop_sibling, xml_free_node, xml_free_prop, xml_new_doc_text_len, xml_text_merge,
    xml_tree_err_memory, NodePtr, XmlAttr, XmlAttrPtr, XmlAttribute, XmlAttributePtr,
    XmlAttributeType, XmlDocPtr, XmlDtd, XmlDtdPtr, XmlElementType, XmlEntity, XmlEntityPtr,
    XmlNode, XmlNodePtr, XmlNs, XmlNsPtr, XML_LOCAL_NAMESPACE, XML_XML_NAMESPACE,
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
    fn document(&self) -> Option<XmlDocPtr>;
    fn set_document(&mut self, doc: Option<XmlDocPtr>);

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
    unsafe fn get_base(&self, doc: Option<XmlDocPtr>) -> Option<String> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        let doc = doc.or(self.document());
        let mut cur = NodePtr::from_ptr(self as *const Self as *mut XmlNode);
        if let Some(doc) =
            doc.filter(|doc| matches!(doc.element_type(), XmlElementType::XmlHTMLDocumentNode))
        {
            cur = doc.children;
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
            if let Ok(ent) =
                XmlEntityPtr::try_from(XmlGenericNodePtr::from_raw(now.as_ptr()).unwrap())
            {
                let ret = ent.uri.load(Ordering::Relaxed);
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
        if let Some(url) = doc.as_deref().and_then(|doc| doc.url.as_deref()) {
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
                } else if let Some(ret) = children.get_string(self.document(), 1) {
                    return Some(ret);
                }
            }
            Some("".to_owned())
        } else if matches!(self.element_type(), XmlElementType::XmlAttributeDecl) {
            let attr_decl = XmlAttributePtr::from_raw(self as *const Self as *mut XmlAttribute)
                .unwrap()
                .unwrap();
            let def = attr_decl.default_value;
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
    fn get_last_child(&self) -> Option<XmlGenericNodePtr> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        self.last()
            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
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
                if let Some(new_node) = xml_new_doc_text_len(self.document(), content, len) {
                    let tmp = self.add_child(new_node.into());
                    if tmp != Some(new_node.into()) {
                        return;
                    }
                    if let Some(last) =
                        last.filter(|l| l.next() == NodePtr::from_ptr(new_node.as_ptr()))
                    {
                        xml_text_merge(
                            XmlNodePtr::from_raw(last.as_ptr()).unwrap(),
                            Some(new_node),
                        );
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
                let mut node = XmlNodePtr::try_from(
                    XmlGenericNodePtr::from_raw(self as *mut Self as *mut XmlNode).unwrap(),
                )
                .unwrap();
                if !content.is_null() {
                    node.content = xml_strncat(node.content, content, len);
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
    unsafe fn add_child(&mut self, mut cur: XmlGenericNodePtr) -> Option<XmlGenericNodePtr> {
        let mut cur_node = XmlGenericNodePtr::from_raw(self as *mut Self as *mut XmlNode).unwrap();

        if matches!(cur_node.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        if matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        if cur_node == cur {
            return None;
        }
        // If cur is a TEXT node, merge its content with adjacent TEXT nodes cur is then freed.
        if matches!(cur.element_type(), XmlElementType::XmlTextNode) {
            let cur = XmlNodePtr::try_from(cur).unwrap();
            if let Some(mut cur_node) = XmlNodePtr::try_from(cur_node).ok().filter(|cur_node| {
                matches!(cur_node.element_type(), XmlElementType::XmlTextNode)
                    && !cur_node.content.is_null()
                    && self.name() == cur.name()
            }) {
                cur_node.add_content(cur.content);
                xml_free_node(cur.as_ptr());
                return Some(cur_node.into());
            }
            if let Some(mut last) = self.last().filter(|l| {
                matches!(l.typ, XmlElementType::XmlTextNode)
                    && l.name == cur.name
                    && self
                        .last()
                        .and_then(|l| XmlGenericNodePtr::from_raw(l.as_ptr()))
                        != Some(cur.into())
            }) {
                last.add_content(cur.content);
                xml_free_node(cur.as_ptr());
                return self
                    .last()
                    .and_then(|l| XmlGenericNodePtr::from_raw(l.as_ptr()));
            }
        }

        // add the new element at the end of the children list.
        let prev = cur
            .parent()
            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
        cur.set_parent(NodePtr::from_ptr(cur_node.as_ptr()));
        if cur.document() != self.document() {
            cur.set_doc(self.document());
        }
        // this check prevents a loop on tree-traversions if a developer
        // tries to add a node to its parent multiple times
        if prev == Some(cur_node) {
            return Some(cur);
        }

        // Coalescing
        if let Some(mut cur_node) = XmlNodePtr::try_from(cur_node).ok().filter(|&cur_node| {
            matches!(cur_node.element_type(), XmlElementType::XmlTextNode)
                && !cur_node.content.is_null()
                && cur != cur_node.into()
        }) {
            cur_node.add_content(XmlNodePtr::try_from(cur).unwrap().content);
            xml_free_node(cur.as_ptr());
            return Some(cur_node.into());
        }
        if let Ok(mut cur) = XmlAttrPtr::try_from(cur) {
            if !matches!(cur_node.element_type(), XmlElementType::XmlElementNode) {
                return None;
            }
            let mut cur_node = XmlNodePtr::try_from(cur_node).unwrap();
            if cur_node.properties.is_some() {
                // check if an attribute with the same name exists

                let lastattr = if let Some(ns) = cur.ns {
                    let href = ns.href;
                    cur_node.has_ns_prop(
                        CStr::from_ptr(cur.name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        (!href.is_null())
                            .then(|| CStr::from_ptr(href as *const i8).to_string_lossy())
                            .as_deref(),
                    )
                } else {
                    cur_node.has_ns_prop(
                        CStr::from_ptr(cur.name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        None,
                    )
                };
                if let Some(mut lastattr) = lastattr
                    .and_then(|attr| attr.ok())
                    .filter(|&attr| attr != cur)
                {
                    // different instance, destroy it (attributes must be unique)
                    (*lastattr).unlink();
                    xml_free_prop(lastattr);
                }
                match lastattr {
                    Some(Ok(attr)) if attr == cur => return Some(cur.into()),
                    _ => {}
                }
            }
            if let Some(mut lastattr) = cur_node.properties {
                // find the end
                while let Some(next) = lastattr.next {
                    lastattr = next;
                }
                lastattr.next = Some(cur);
                cur.prev = Some(lastattr);
            } else {
                cur_node.properties = Some(cur);
            }
        } else if cur_node.children().is_none() {
            cur_node.set_children(NodePtr::from_ptr(cur.as_ptr()));
            cur_node.set_last(NodePtr::from_ptr(cur.as_ptr()));
        } else {
            let mut prev = cur_node
                .last()
                .and_then(|l| XmlGenericNodePtr::from_raw(l.as_ptr()))
                .unwrap();
            prev.set_next(NodePtr::from_ptr(cur.as_ptr()));
            cur.set_prev(NodePtr::from_ptr(prev.as_ptr()));
            cur_node.set_last(NodePtr::from_ptr(cur.as_ptr()));
        }
        Some(cur)
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
    fn first_element_child(&self) -> Option<XmlNodePtr> {
        let mut next = match self.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => self.children(),
            _ => {
                return None;
            }
        };
        while let Some(cur) = next {
            if matches!(cur.element_type(), XmlElementType::XmlElementNode) {
                unsafe {
                    return XmlNodePtr::from_raw(cur.as_ptr()).unwrap();
                }
            }
            next = cur.next();
        }
        None
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
    fn last_element_child(&self) -> Option<XmlNodePtr> {
        let mut cur = match self.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => self.last(),
            _ => {
                return None;
            }
        };
        while let Some(now) = cur {
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                unsafe {
                    return XmlNodePtr::from_raw(now.as_ptr()).unwrap();
                }
            }
            cur = now.prev();
        }
        None
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
    fn next_element_sibling(&self) -> Option<XmlNodePtr> {
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
                return None;
            }
        };
        while let Some(now) = cur {
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                unsafe {
                    return XmlNodePtr::from_raw(now.as_ptr()).unwrap();
                }
            }
            cur = now.next();
        }
        None
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
    fn previous_element_sibling(&self) -> Option<XmlNodePtr> {
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
                return None;
            }
        };
        while let Some(now) = node {
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                unsafe {
                    return XmlNodePtr::from_raw(now.as_ptr()).unwrap();
                }
            }
            node = now.prev();
        }
        None
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
            if let Some(mut doc) = self.document() {
                if doc.int_subset == dtd {
                    doc.int_subset = None;
                }
                if doc.ext_subset == dtd {
                    doc.ext_subset = None;
                }
            }
        }
        if matches!(self.element_type(), XmlElementType::XmlEntityDecl) {
            let name = self.name().map(|n| n.into_owned());
            if let Some(doc) = self.document() {
                if let Some(int_subset) = doc.int_subset {
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
                if let Some(ext_subset) = doc.ext_subset {
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

pub struct XmlGenericNodePtr(pub(super) NonNull<dyn NodeCommon>);

impl XmlGenericNodePtr {
    /// Allocate new memory and create new `XmlGenericNodePtr` from an owned xml node.
    ///
    /// This method leaks allocated memory.  
    /// Users can use `free` method for deallocating memory.
    pub(crate) fn new<T: NodeCommon + 'static>(node: T) -> Option<Self> {
        let boxed: Box<dyn NodeCommon> = Box::new(node);
        NonNull::new(Box::leak(boxed)).map(Self)
    }

    // Temporary workaround
    pub fn from_raw<T: NodeCommon>(ptr: *mut T) -> Option<Self> {
        NonNull::new(ptr as *mut dyn NodeCommon).map(Self)
    }

    // Temporary workaround
    pub fn as_ptr(self) -> *mut XmlNode {
        self.0.as_ptr() as *mut XmlNode
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
    pub unsafe fn search_ns(
        self,
        doc: Option<XmlDocPtr>,
        namespace: Option<&str>,
    ) -> Option<XmlNsPtr> {
        let orig = self;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        if namespace == Some("xml") {
            if doc.is_none() && matches!(self.element_type(), XmlElementType::XmlElementNode) {
                let mut node = XmlNodePtr::try_from(self).unwrap();
                // The XML-1.0 namespace is normally held on the root element.
                // In this case exceptionally create it on the node element.
                let Some(cur) = XmlNsPtr::new(XmlNs {
                    typ: XML_LOCAL_NAMESPACE,
                    href: xml_strdup(XML_XML_NAMESPACE.as_ptr() as _),
                    prefix: xml_strdup(c"xml".as_ptr() as _),
                    next: node.ns_def,
                    ..Default::default()
                }) else {
                    xml_tree_err_memory("searching namespace");
                    return None;
                };
                node.ns_def = Some(cur);
                return Some(cur);
            }
            let mut doc = doc.or(self.document())?;
            // Return the XML namespace declaration held by the doc.
            if doc.old_ns.is_none() {
                return doc.ensure_xmldecl();
            } else {
                return doc.old_ns;
            }
        }
        let mut node = Some(self);
        while let Some(cur_node) = node {
            if matches!(
                cur_node.element_type(),
                XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode
                    | XmlElementType::XmlEntityDecl
            ) {
                return None;
            }
            if let Some(cur_node) = XmlNodePtr::try_from(cur_node).ok().filter(|cur_node| {
                matches!(cur_node.element_type(), XmlElementType::XmlElementNode)
            }) {
                let mut cur = cur_node.ns_def;
                while let Some(now) = cur {
                    if now.prefix().is_none() && namespace.is_none() && !now.href.is_null() {
                        return Some(now);
                    }
                    if now.href().is_some()
                        && now.prefix().is_some()
                        && namespace == now.prefix().as_deref()
                    {
                        return Some(now);
                    }
                    cur = now.next;
                }
                if orig != cur_node.into() {
                    let cur = cur_node.ns;
                    if let Some(cur) = cur {
                        if cur.prefix().is_none() && namespace.is_none() && !cur.href.is_null() {
                            return Some(cur);
                        }
                        if cur.prefix().is_some()
                            && cur.href().is_some()
                            && namespace == cur.prefix().as_deref()
                        {
                            return Some(cur);
                        }
                    }
                }
            }
            node = cur_node
                .parent()
                .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
        }
        None
    }

    /// Search all the namespace applying to a given element.
    ///
    /// Returns an `Vec` of all the `xmlNsPtr` found.
    #[doc(alias = "xmlGetNsList")]
    #[cfg(any(feature = "libxml_tree", feature = "xpath", feature = "schema"))]
    pub unsafe fn get_ns_list(self, _doc: Option<XmlDocPtr>) -> Option<Vec<XmlNsPtr>> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        let mut ret: Vec<XmlNsPtr> = vec![];
        let mut node = Some(self);
        while let Some(now) = node {
            if let Some(node) = XmlNodePtr::try_from(now)
                .ok()
                .filter(|node| matches!(node.element_type(), XmlElementType::XmlElementNode))
            {
                let mut cur = node.ns_def;
                while let Some(now) = cur {
                    if ret.iter().all(|&ret| now.prefix() != (*ret).prefix()) {
                        ret.push(now);
                    }
                    cur = now.next
                }
            }
            node = now
                .parent()
                .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()));
        }
        Some(ret)
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
    pub unsafe fn get_content(self) -> Option<String> {
        if let Ok(ns) = XmlNsPtr::try_from(self) {
            ns.get_content()
        } else if let Ok(attr) = XmlAttrPtr::try_from(self) {
            attr.get_content()
        } else if let Ok(doc) = XmlDocPtr::try_from(self) {
            doc.get_content()
        } else if let Ok(node) = XmlNodePtr::try_from(self) {
            node.get_content()
        } else {
            None
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
    pub unsafe fn get_content_to(self, buf: &mut String) -> i32 {
        if let Ok(attr) = XmlAttrPtr::try_from(self) {
            attr.get_content_to(buf)
        } else if let Ok(doc) = XmlDocPtr::try_from(self) {
            doc.get_content_to(buf)
        } else if let Ok(ns) = XmlNsPtr::try_from(self) {
            ns.get_content_to(buf)
        } else if let Ok(node) = XmlNodePtr::try_from(self) {
            node.get_content_to(buf)
        } else {
            0
        }
    }

    /// Searches the language of a node, i.e. the values of the xml:lang
    /// attribute or the one carried by the nearest ancestor.
    ///
    /// Returns a pointer to the lang value, or null_mut() if not found.  
    /// It's up to the caller to free the memory with xml_free().
    #[doc(alias = "xmlNodeGetLang")]
    pub unsafe fn get_lang(self) -> Option<String> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        let mut cur = Some(self);
        while let Some(now) = cur {
            if let Some(now) = XmlNodePtr::try_from(now)
                .ok()
                .filter(|node| node.element_type() == XmlElementType::XmlElementNode)
            {
                let lang = now.get_ns_prop("lang", XML_XML_NAMESPACE.to_str().ok());
                if lang.is_some() {
                    return lang;
                }
            }
            cur = now
                .parent()
                .and_then(|now| XmlGenericNodePtr::from_raw(now.as_ptr()));
        }
        None
    }

    /// Get line number of `self`.
    /// Try to override the limitation of lines being store in 16 bits ints
    ///
    /// Returns the line number if successful, -1 otherwise
    #[doc(alias = "xmlGetLineNoInternal")]
    unsafe fn get_line_no_internal(self, depth: i32) -> i64 {
        let mut result = -1;

        if depth >= 5 {
            return -1;
        }

        if let Some(node) = XmlNodePtr::try_from(self).ok().filter(|node| {
            matches!(
                node.element_type(),
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlCommentNode
                    | XmlElementType::XmlPINode
            )
        }) {
            if node.line == 65535 {
                if matches!(node.element_type(), XmlElementType::XmlTextNode)
                    && !node.psvi.is_null()
                {
                    result = node.psvi as isize as i64;
                } else if let Some(children) = node
                    .children()
                    .filter(|_| matches!(node.element_type(), XmlElementType::XmlElementNode))
                    .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                {
                    result = children.get_line_no_internal(depth + 1);
                } else if let Some(next) = node
                    .next()
                    .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                {
                    result = next.get_line_no_internal(depth + 1);
                } else if let Some(prev) = node
                    .prev()
                    .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
                {
                    result = prev.get_line_no_internal(depth + 1);
                }
            }
            if result == -1 || result == 65535 {
                result = node.line as i64;
            }
        } else if let Some(prev) = self
            .prev()
            .filter(|p| {
                matches!(
                    p.element_type(),
                    XmlElementType::XmlElementNode
                        | XmlElementType::XmlTextNode
                        | XmlElementType::XmlCommentNode
                        | XmlElementType::XmlPINode
                )
            })
            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
        {
            result = prev.get_line_no_internal(depth + 1);
        } else if let Some(parent) = self
            .parent()
            .filter(|p| matches!(p.element_type(), XmlElementType::XmlElementNode))
            .and_then(|p| XmlGenericNodePtr::from_raw(p.as_ptr()))
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

    /// update all nodes under the tree to point to the right document
    #[doc(alias = "xmlSetTreeDoc")]
    pub unsafe fn set_doc(mut self, doc: Option<XmlDocPtr>) {
        if self.element_type() == XmlElementType::XmlNamespaceDecl {
            return;
        }
        if self.document() != doc {
            if let Some(node) = XmlNodePtr::try_from(self)
                .ok()
                .filter(|node| matches!(node.element_type(), XmlElementType::XmlElementNode))
            {
                let mut prop = node.properties;
                while let Some(mut now) = prop {
                    if matches!(now.atype, Some(XmlAttributeType::XmlAttributeID)) {
                        xml_remove_id(self.document().unwrap(), now);
                    }

                    if now.document() != doc {
                        now.set_document(doc);
                    }
                    if let Some(mut children) = now.children() {
                        children.set_doc_all_sibling(doc);
                    }

                    // TODO: ID attributes should be also added to the new
                    //       document, but this breaks things like xmlReplaceNode.
                    //       The underlying problem is that xmlRemoveID is only called
                    //       if a node is destroyed, not if it's unlinked.
                    // if (xmlIsID(doc, tree, prop)) {
                    //     XmlChar *idVal = xmlNodeListGetString(doc, now.children, 1);
                    //     xmlAddID(null_mut(), doc, idVal, prop);
                    // }

                    prop = now.next;
                }
            }
            if matches!(self.element_type(), XmlElementType::XmlEntityRefNode) {
                // Clear 'children' which points to the entity declaration
                // from the original document.
                self.set_children(None);
            } else if let Some(mut children) = self.children() {
                children.set_doc_all_sibling(doc);
            }

            // FIXME: self.ns should be updated as in xmlStaticCopyNode().
            self.set_document(doc);
        }
    }

    /// Set (or reset) the base URI of a node, i.e. the value of the xml:base attribute.
    #[doc(alias = "xmlNodeSetBase")]
    #[cfg(any(feature = "libxml_tree", feature = "xinclude"))]
    pub unsafe fn set_base(self, uri: Option<&str>) {
        if let Ok(mut node) = XmlNodePtr::try_from(self) {
            node.set_base(uri);
        } else if let Ok(mut attr) = XmlAttrPtr::try_from(self) {
            attr.set_base(uri);
        } else if let Ok(mut doc) = XmlDocPtr::try_from(self) {
            doc.set_base(uri);
        }
    }

    /// Add a list of node at the end of the child list of the parent
    /// merging adjacent TEXT nodes (`cur` may be freed)
    ///
    /// See the note regarding namespaces in xmlAddChild.
    ///
    /// Returns the last child or NULL in case of error.
    #[doc(alias = "xmlAddChildList")]
    pub unsafe fn add_child_list(
        mut self,
        mut cur: XmlGenericNodePtr,
    ) -> Option<XmlGenericNodePtr> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        if matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        // add the first element at the end of the children list.
        if self.children().is_none() {
            self.set_children(NodePtr::from_ptr(cur.as_ptr()));
        } else {
            // If cur and self.last both are TEXT nodes, then merge them.
            if let Some(node) = XmlNodePtr::try_from(cur).ok().filter(|cur| {
                cur.element_type() == XmlElementType::XmlTextNode
                    && matches!(
                        self.last().unwrap().element_type(),
                        XmlElementType::XmlTextNode
                    )
                    && cur.name() == self.last().unwrap().name()
            }) {
                self.last().unwrap().add_content(node.content);
                // if it's the only child, nothing more to be done.
                let Some(next) = node.next() else {
                    xml_free_node(node.as_ptr());
                    return self
                        .last()
                        .and_then(|l| XmlGenericNodePtr::from_raw(l.as_ptr()));
                };
                let prev = node;
                cur = XmlGenericNodePtr::from_raw(next.as_ptr()).unwrap();
                xml_free_node(prev.as_ptr());
            }
            let prev: *mut XmlNode = self.last().map_or(null_mut(), |l| l.as_ptr());
            (*prev).set_next(NodePtr::from_ptr(cur.as_ptr()));
            cur.set_prev(NodePtr::from_ptr(prev));
        }
        while let Some(next) = cur.next() {
            cur.set_parent(NodePtr::from_ptr(self.as_ptr()));
            if cur.document() != self.document() {
                cur.set_doc(self.document());
            }
            cur = XmlGenericNodePtr::from_raw(next.as_ptr()).unwrap();
        }
        cur.set_parent(NodePtr::from_ptr(self.as_ptr()));
        // the parent may not be linked to a doc !
        if cur.document() != self.document() {
            cur.set_doc(self.document());
        }
        self.set_last(NodePtr::from_ptr(cur.as_ptr()));

        Some(cur)
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
    pub unsafe fn add_sibling(self, mut elem: XmlGenericNodePtr) -> *mut XmlNode {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return null_mut();
        }

        if elem.element_type() == XmlElementType::XmlNamespaceDecl {
            return null_mut();
        }

        if self == elem {
            return null_mut();
        }

        let mut cur = self;
        // Constant time is we can rely on the -> parent -> last to find the last sibling.
        if let Some(last) = self
            .parent()
            .filter(|p| {
                !matches!(self.element_type(), XmlElementType::XmlAttributeNode)
                    && p.children().is_some()
            })
            .and_then(|p| p.last().filter(|l| l.next().is_none()))
        {
            cur = XmlGenericNodePtr::from_raw(last.as_ptr()).unwrap();
        } else {
            while let Some(next) = cur.next() {
                cur = XmlGenericNodePtr::from_raw(next.as_ptr()).unwrap();
            }
        }

        elem.unlink();

        if let Some((mut cur, elem)) = XmlNodePtr::try_from(cur)
            .ok()
            .filter(|cur| cur.element_type() == XmlElementType::XmlTextNode)
            .zip(
                XmlNodePtr::try_from(elem)
                    .ok()
                    .filter(|elem| elem.element_type() == XmlElementType::XmlTextNode),
            )
            .filter(|(cur, elem)| cur.name() == elem.name())
        {
            cur.add_content(elem.content);
            xml_free_node(elem.as_ptr());
            return cur.as_ptr();
        }
        if matches!(elem.element_type(), XmlElementType::XmlAttributeNode) {
            return add_prop_sibling(cur.as_ptr(), cur.as_ptr(), elem.as_ptr());
        }

        if elem.document() != cur.document() {
            elem.set_doc(cur.document());
        }
        let parent = cur.parent();
        elem.set_prev(NodePtr::from_ptr(cur.as_ptr()));
        elem.set_next(None);
        elem.set_parent(parent);
        cur.set_next(NodePtr::from_ptr(elem.as_ptr()));
        if let Some(mut parent) = parent {
            parent.set_last(NodePtr::from_ptr(elem.as_ptr()));
        }

        elem.as_ptr()
    }
}

impl Clone for XmlGenericNodePtr {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for XmlGenericNodePtr {}

impl PartialEq for XmlGenericNodePtr {
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl Eq for XmlGenericNodePtr {}

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

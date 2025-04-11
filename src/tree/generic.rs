use std::{
    borrow::Cow,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use crate::{libxml::valid::xml_remove_id, uri::build_uri};

use super::{
    XML_LOCAL_NAMESPACE, XML_XML_NAMESPACE, XmlAttr, XmlAttrPtr, XmlAttribute, XmlAttributePtr,
    XmlAttributeType, XmlDoc, XmlDocPtr, XmlDtd, XmlDtdPtr, XmlElement, XmlElementPtr,
    XmlElementType, XmlEntity, XmlEntityPtr, XmlNode, XmlNodePtr, XmlNs, XmlNsPtr,
    add_prop_sibling, xml_encode_attribute_entities, xml_encode_entities_reentrant, xml_free_node,
    xml_free_prop, xml_get_doc_entity, xml_new_doc_text, xml_text_merge, xml_tree_err_memory,
};

pub trait NodeCommon {
    fn element_type(&self) -> XmlElementType;
    fn name(&self) -> Option<Cow<'_, str>>;
    fn children(&self) -> Option<XmlGenericNodePtr>;
    fn set_children(&mut self, children: Option<XmlGenericNodePtr>);
    fn last(&self) -> Option<XmlGenericNodePtr>;
    fn set_last(&mut self, last: Option<XmlGenericNodePtr>);
    fn parent(&self) -> Option<XmlGenericNodePtr>;
    fn set_parent(&mut self, parent: Option<XmlGenericNodePtr>);
    fn next(&self) -> Option<XmlGenericNodePtr>;
    fn set_next(&mut self, next: Option<XmlGenericNodePtr>);
    fn prev(&self) -> Option<XmlGenericNodePtr>;
    fn set_prev(&mut self, prev: Option<XmlGenericNodePtr>);
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
    fn get_base(&self, doc: Option<XmlDocPtr>) -> Option<String> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        let doc = doc.or(self.document());
        let mut cur = XmlGenericNodePtr::from_raw(self as *const Self as *mut XmlNode);
        if let Some(doc) =
            doc.filter(|doc| matches!(doc.element_type(), XmlElementType::XmlHTMLDocumentNode))
        {
            cur = doc.children();
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
            if let Ok(ent) = XmlEntityPtr::try_from(now) {
                return ent.uri.as_deref().map(|uri| uri.to_owned());
            }
            if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                let base = now.get_ns_prop("base", Some(XML_XML_NAMESPACE));
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

    /// Search the last child of a node.
    /// Returns the last child or null_mut() if none.
    #[doc(alias = "xmlGetLastChild")]
    fn get_last_child(&self) -> Option<XmlGenericNodePtr> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        self.last()
    }

    /// Append the extra substring to the node content.
    ///
    /// # Note
    /// In contrast to xmlNodeSetContentLen(), `content` is supposed to be raw text,
    /// so unescaped XML special chars are allowed, entity references are not supported.
    #[doc(alias = "xmlNodeAddContent", alias = "xmlNodeAddContentLen")]
    unsafe fn add_content(&mut self, content: &str) {
        unsafe {
            if content.is_empty() {
                return;
            }
            match self.element_type() {
                XmlElementType::XmlDocumentFragNode | XmlElementType::XmlElementNode => {
                    let last = self.last();
                    if let Some(new_node) = xml_new_doc_text(self.document(), Some(content)) {
                        let tmp = self.add_child(new_node.into());
                        if tmp != Some(new_node.into()) {
                            return;
                        }
                        if let Some(last) = last.filter(|l| l.next() == Some(new_node.into())) {
                            xml_text_merge(
                                Some(XmlNodePtr::try_from(last).unwrap()),
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
                    let buf = node.content.get_or_insert_default();
                    buf.push_str(content);
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
        unsafe {
            let mut cur_node =
                XmlGenericNodePtr::from_raw(self as *mut Self as *mut XmlNode).unwrap();

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
                        && cur_node.content.is_some()
                        && self.name() == cur.name()
                }) {
                    let content = cur.content.as_deref().unwrap();
                    cur_node.add_content(content);
                    xml_free_node(cur);
                    return Some(cur_node.into());
                }
                if let Some(mut last) = self.last().filter(|l| {
                    matches!(l.element_type(), XmlElementType::XmlTextNode)
                        && l.name() == cur.name()
                        && self.last() != Some(cur.into())
                }) {
                    let content = cur.content.as_deref().unwrap();
                    last.add_content(content);
                    xml_free_node(cur);
                    return self.last();
                }
            }

            // add the new element at the end of the children list.
            let prev = cur.parent();
            cur.set_parent(Some(cur_node));
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
                    && cur_node.content.is_some()
                    && cur != XmlGenericNodePtr::from(cur_node)
            }) {
                let node = XmlNodePtr::try_from(cur).unwrap();
                let content = node.content.as_deref().unwrap();
                cur_node.add_content(content);
                xml_free_node(cur);
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
                        cur_node.has_ns_prop(&cur.name, ns.href.as_deref())
                    } else {
                        cur_node.has_ns_prop(&cur.name, None)
                    };
                    if let Some(mut lastattr) = lastattr
                        .and_then(|attr| attr.ok())
                        .filter(|&attr| attr != cur)
                    {
                        // different instance, destroy it (attributes must be unique)
                        lastattr.unlink();
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
                cur_node.set_children(Some(cur));
                cur_node.set_last(Some(cur));
            } else {
                let mut prev = cur_node.last().unwrap();
                prev.set_next(Some(cur));
                cur.set_prev(Some(prev));
                cur_node.set_last(Some(cur));
            }
            Some(cur)
        }
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
                return Some(XmlNodePtr::try_from(cur).unwrap());
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
                return Some(XmlNodePtr::try_from(now).unwrap());
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
                return Some(XmlNodePtr::try_from(now).unwrap());
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
                return Some(XmlNodePtr::try_from(now).unwrap());
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
    fn unlink(&mut self) {
        if let Some(mut parent) = self.parent() {
            if parent
                .children()
                .is_some_and(|par| std::ptr::addr_eq(par.as_ptr(), self))
            {
                parent.set_children(self.next());
            }
            if parent
                .last()
                .is_some_and(|last| std::ptr::addr_eq(last.as_ptr(), self))
            {
                parent.set_last(self.prev());
            }
            self.set_parent(None);
        }
        if let Some(mut next) = self.next() {
            next.set_prev(self.prev());
        }
        if let Some(mut prev) = self.prev() {
            prev.set_next(self.next());
        }
        self.set_next(None);
        self.set_prev(None);
    }
}

pub struct XmlGenericNodePtr(pub(super) NonNull<dyn NodeCommon>);

impl XmlGenericNodePtr {
    // /// Allocate new memory and create new `XmlGenericNodePtr` from an owned xml node.
    // ///
    // /// This method leaks allocated memory.
    // /// Users can use `free` method for deallocating memory.
    // pub(crate) fn new<T: NodeCommon + 'static>(node: T) -> Option<Self> {
    //     let boxed: Box<dyn NodeCommon> = Box::new(node);
    //     NonNull::new(Box::leak(boxed)).map(Self)
    // }

    // Temporary workaround
    pub fn from_raw<T: NodeCommon>(ptr: *mut T) -> Option<Self> {
        let res = NonNull::new(ptr as *mut dyn NodeCommon).map(Self)?;
        let res = XmlNodePtr::try_from(res)
            .map(|node| node.into())
            .or_else(|_| XmlDocPtr::try_from(res).map(|node| node.into()))
            .or_else(|_| XmlAttrPtr::try_from(res).map(|node| node.into()))
            .or_else(|_| XmlNsPtr::try_from(res).map(|node| node.into()))
            .or_else(|_| XmlEntityPtr::try_from(res).map(|node| node.into()))
            .or_else(|_| XmlDtdPtr::try_from(res).map(|node| node.into()))
            .or_else(|_| XmlAttributePtr::try_from(res).map(|node| node.into()))
            .or_else(|_| XmlElementPtr::try_from(res).map(|node| node.into()))
            .unwrap_or_else(|_| panic!("Unknown Node Type: {:?}", res.element_type()));
        Some(res)
    }

    // Temporary workaround
    pub fn as_ptr(self) -> *mut XmlNode {
        self.0.as_ptr() as *mut XmlNode
    }

    // /// Deallocate memory.
    // ///
    // /// # Safety
    // /// This method should be called only once.
    // /// If called more than twice, the behavior is undefined.
    // pub(crate) unsafe fn free(self) {
    //     unsafe {
    //         let _ = *Box::from_raw(self.0.as_ptr());
    //     }
    // }

    // /// Acquire the ownership of the inner value.
    // /// As a result, `self` will be invalid. `self` must not be used after performs this method.
    // ///
    // /// # Safety
    // /// This method should be called only once.
    // /// If called more than twice, the behavior is undefined.
    // pub(crate) unsafe fn into_inner(self) -> Box<dyn NodeCommon> {
    //     unsafe { Box::from_raw(self.0.as_ptr()) }
    // }

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
    pub fn search_ns(self, doc: Option<XmlDocPtr>, namespace: Option<&str>) -> Option<XmlNsPtr> {
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
                    href: Some(XML_XML_NAMESPACE.into()),
                    prefix: Some("xml".into()),
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
                    if now.prefix().is_none() && namespace.is_none() && now.href.is_some() {
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
                if orig != XmlGenericNodePtr::from(cur_node) {
                    let cur = cur_node.ns;
                    if let Some(cur) = cur {
                        if cur.prefix().is_none() && namespace.is_none() && cur.href.is_some() {
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
            node = cur_node.parent();
        }
        None
    }

    // fn get_prop_node_value_internal(&self) -> Option<String> {
    //     if let Ok(attr) = XmlAttrPtr::try_from(*self) {
    //         attr.get_prop_node_value_internal()
    //     } else if matches!(self.element_type(), XmlElementType::XmlAttributeDecl) {
    //         let attr_decl = XmlAttributePtr::try_from(*self).unwrap();
    //         attr_decl.default_value.as_deref().map(|def| def.to_owned())
    //     } else {
    //         None
    //     }
    // }

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
    pub fn get_prop(&self, name: &str) -> Option<String> {
        let node = XmlNodePtr::try_from(*self).ok()?;
        node.get_prop(name)
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
    pub fn get_ns_prop(&self, name: &str, name_space: Option<&str>) -> Option<String> {
        let node = XmlNodePtr::try_from(*self).ok()?;
        node.get_ns_prop(name, name_space)
    }

    /// Search all the namespace applying to a given element.
    ///
    /// Returns an `Vec` of all the `xmlNsPtr` found.
    #[doc(alias = "xmlGetNsList")]
    #[cfg(any(feature = "libxml_tree", feature = "xpath", feature = "schema"))]
    pub fn get_ns_list(self, _doc: Option<XmlDocPtr>) -> Option<Vec<XmlNsPtr>> {
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
            node = now.parent();
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
    pub fn get_content(self) -> Option<String> {
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
    pub fn get_content_to(self, buf: &mut String) -> i32 {
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
    pub fn get_lang(self) -> Option<String> {
        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        let mut cur = Some(self);
        while let Some(now) = cur {
            if let Some(now) = XmlNodePtr::try_from(now)
                .ok()
                .filter(|node| node.element_type() == XmlElementType::XmlElementNode)
            {
                let lang = now.get_ns_prop("lang", Some(XML_XML_NAMESPACE));
                if lang.is_some() {
                    return lang;
                }
            }
            cur = now.parent();
        }
        None
    }

    /// Get line number of `self`.
    /// Try to override the limitation of lines being store in 16 bits ints
    ///
    /// Returns the line number if successful, -1 otherwise
    #[doc(alias = "xmlGetLineNoInternal")]
    pub(super) fn get_line_no_internal(self, depth: i32) -> i64 {
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
                {
                    result = children.get_line_no_internal(depth + 1);
                } else if let Some(next) = node.next() {
                    result = next.get_line_no_internal(depth + 1);
                } else if let Some(prev) = node.prev() {
                    result = prev.get_line_no_internal(depth + 1);
                }
            }
            if result == -1 || result == 65535 {
                result = node.line as i64;
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
    pub fn get_line_no(&self) -> i64 {
        self.get_line_no_internal(0)
    }

    /// Build a structure based Path for the given node
    ///
    /// Returns the new path or `None` in case of error.  
    #[doc(alias = "xmlGetNodePath")]
    #[cfg(feature = "libxml_tree")]
    pub fn get_node_path(self) -> Option<String> {
        let mut occur: i32;
        let mut generic: i32;

        if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }

        let mut buffer = String::with_capacity(500);
        let mut buf = String::with_capacity(500);
        let mut cur = Some(self);
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
                let current = XmlNodePtr::try_from(current).unwrap();
                generic = 0;
                sep = "/";
                if let Some(ns) = current.ns {
                    name = if let Some(prefix) = ns.prefix() {
                        Cow::Owned(format!("{prefix}:{}", current.name().unwrap(),))
                    } else {
                        // We cannot express named elements in the default
                        // namespace, so use "*".
                        generic = 1;
                        "*".into()
                    };
                } else {
                    name = current.name().unwrap().into_owned().into();
                }

                // Thumbler index computation
                // TODO: the occurrence test seems bogus for namespaced names
                let mut tmp = current.prev();
                while let Some(now) = tmp {
                    if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                        let now = XmlNodePtr::try_from(now).unwrap();
                        if generic != 0
                            || (current.name() == now.name()
                                && (now.ns == current.ns
                                    || current
                                        .ns
                                        .zip(now.ns)
                                        .is_some_and(|(c, n)| c.prefix() == n.prefix())))
                        {
                            occur += 1;
                        }
                    }
                    tmp = now.prev();
                }
                if occur == 0 {
                    let mut tmp = current.next();
                    while let Some(now) = tmp.filter(|_| occur == 0) {
                        if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                            let now = XmlNodePtr::try_from(now).unwrap();
                            if generic != 0
                                || (current.name() == now.name()
                                    && (now.ns == current.ns
                                        || current
                                            .ns
                                            .zip(now.ns)
                                            .is_some_and(|(c, n)| c.prefix() == n.prefix())))
                            {
                                occur += 1;
                            }
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
                let attr = XmlAttrPtr::try_from(current).unwrap();
                sep = "/@";
                if let Some(ns) = attr.ns {
                    name = if let Some(prefix) = ns.prefix() {
                        format!("{prefix}:{}", attr.name().unwrap()).into()
                    } else {
                        format!("{}", attr.name().unwrap()).into()
                    };
                } else {
                    name = attr.name().unwrap().into_owned().into();
                }
                attr.parent()
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

    /// Build the string equivalent to the text contained in the Node list
    /// made of TEXTs and ENTITY_REFs.
    ///
    /// Returns a pointer to the string copy.
    #[doc(alias = "xmlNodeListGetString")]
    pub fn get_string(self, doc: Option<XmlDocPtr>, in_line: i32) -> Option<String> {
        let mut node = Some(self);
        let mut ret = None::<String>;

        let attr = self
            .parent()
            .filter(|p| matches!(p.element_type(), XmlElementType::XmlAttributeNode))
            .is_some();

        while let Some(cur_node) = node {
            if matches!(
                cur_node.element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                let content = cur_node.content.as_deref().unwrap();
                if in_line != 0 {
                    ret.get_or_insert_default().push_str(content);
                } else {
                    let buffer = if attr {
                        xml_encode_attribute_entities(doc, cur_node.content.as_deref().unwrap())
                    } else {
                        xml_encode_entities_reentrant(doc, cur_node.content.as_deref().unwrap())
                    };
                    ret.get_or_insert_default().push_str(&buffer);
                }
            } else if matches!(cur_node.element_type(), XmlElementType::XmlEntityRefNode) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                if in_line != 0 {
                    let ent = xml_get_doc_entity(doc, &cur_node.name().unwrap());
                    if let Some(ent) = ent {
                        // an entity content can be any "well balanced chunk",
                        // i.e. the result of the content [43] production:
                        // http://www.w3.org/TR/REC-xml#NT-content.
                        // So it can contain text, CDATA section or nested
                        // entity reference nodes (among others).
                        // -> we recursive  call xmlNodeListGetString()
                        // which handles these types
                        let children = ent.children();
                        if let Some(buffer) = children.and_then(|c| c.get_string(doc, 1)) {
                            ret.get_or_insert_default().push_str(&buffer);
                        }
                    } else {
                        let content = cur_node.content.as_deref().unwrap();
                        ret.get_or_insert_default().push_str(content);
                    }
                } else {
                    ret.get_or_insert_default()
                        .push_str(format!("&{};", cur_node.name).as_str());
                }
            }
            node = cur_node.next();
        }
        ret
    }

    /// Builds the string equivalent to the text contained in the Node list
    /// made of TEXTs and ENTITY_REFs, contrary to `xmlNodeListGetString()`
    /// this function doesn't do any character encoding handling.
    ///
    /// Returns a pointer to the string copy
    #[doc(alias = "xmlNodeListGetRawString")]
    #[cfg(feature = "libxml_tree")]
    pub fn get_raw_string(self, doc: Option<XmlDocPtr>, in_line: i32) -> Option<String> {
        use super::xml_encode_special_chars;

        let mut node = Some(self);
        let mut ret = None::<String>;

        while let Some(cur_node) = node {
            if matches!(
                cur_node.element_type(),
                XmlElementType::XmlTextNode | XmlElementType::XmlCDATASectionNode
            ) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                if in_line != 0 {
                    let content = cur_node.content.as_deref().unwrap();
                    ret.get_or_insert_default().push_str(content);
                } else {
                    let buffer =
                        xml_encode_special_chars(doc, cur_node.content.as_deref().unwrap());
                    ret.get_or_insert_default().push_str(&buffer);
                }
            } else if matches!(cur_node.element_type(), XmlElementType::XmlEntityRefNode) {
                let cur_node = XmlNodePtr::try_from(cur_node).unwrap();
                if in_line != 0 {
                    let ent = xml_get_doc_entity(doc, &cur_node.name().unwrap());
                    if let Some(ent) = ent {
                        // an entity content can be any "well balanced chunk",
                        // i.e. the result of the content [43] production:
                        // http://www.w3.org/TR/REC-xml#NT-content.
                        // So it can contain text, CDATA section or nested
                        // entity reference nodes (among others).
                        // -> we recursive  call xmlNodeListGetRawString()
                        // which handles these types
                        let children = ent.children();
                        let buffer = children.and_then(|c| c.get_raw_string(doc, 1));
                        if let Some(buffer) = buffer {
                            ret.get_or_insert_default().push_str(&buffer);
                        }
                    } else {
                        let content = cur_node.content.as_deref().unwrap();
                        ret.get_or_insert_default().push_str(content);
                    }
                } else {
                    ret.get_or_insert_default()
                        .push_str(format!("&{};", cur_node.name).as_str());
                }
            }
            node = cur_node.next();
        }
        ret
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
    pub fn set_prop(self, name: &str, value: Option<&str>) -> Option<XmlAttrPtr> {
        if !matches!(self.element_type(), XmlElementType::XmlElementNode) {
            return None;
        }
        let mut node = XmlNodePtr::try_from(self).unwrap();
        node.set_prop(name, value)
    }

    /// update all nodes under the tree to point to the right document
    #[doc(alias = "xmlSetTreeDoc")]
    pub fn set_doc(mut self, doc: Option<XmlDocPtr>) {
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
                    if let Some(children) = now.children() {
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
            } else if let Some(children) = self.children() {
                children.set_doc_all_sibling(doc);
            }

            // FIXME: self.ns should be updated as in xmlStaticCopyNode().
            self.set_document(doc);
        }
    }

    /// update all nodes in the list to point to the right document
    #[doc(alias = "xmlSetListDoc")]
    pub fn set_doc_all_sibling(self, doc: Option<XmlDocPtr>) {
        if self.element_type() == XmlElementType::XmlNamespaceDecl {
            return;
        }
        if self.document() != doc {
            self.set_doc(doc);
        }
        let mut cur = self.next();
        while let Some(now) = cur {
            if now.document() != doc {
                now.set_doc(doc);
            }
            cur = now.next();
        }
    }

    /// Set (or reset) the base URI of a node, i.e. the value of the xml:base attribute.
    #[doc(alias = "xmlNodeSetBase")]
    #[cfg(any(feature = "libxml_tree", feature = "xinclude"))]
    pub fn set_base(self, uri: Option<&str>) {
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
        unsafe {
            if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
                return None;
            }

            if matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl) {
                return None;
            }

            // add the first element at the end of the children list.
            if self.children().is_none() {
                self.set_children(Some(cur));
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
                    let content = node.content.as_deref().unwrap();
                    self.last().unwrap().add_content(content);
                    // if it's the only child, nothing more to be done.
                    let Some(next) = node.next() else {
                        xml_free_node(node);
                        return self.last();
                    };
                    let prev = node;
                    cur = next;
                    xml_free_node(prev);
                }
                let mut prev = self.last().unwrap();
                prev.set_next(Some(cur));
                cur.set_prev(Some(prev));
            }
            while let Some(next) = cur.next() {
                cur.set_parent(Some(self));
                if cur.document() != self.document() {
                    cur.set_doc(self.document());
                }
                cur = next;
            }
            cur.set_parent(Some(self));
            // the parent may not be linked to a doc !
            if cur.document() != self.document() {
                cur.set_doc(self.document());
            }
            self.set_last(Some(cur));

            Some(cur)
        }
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
    pub unsafe fn add_sibling(self, mut elem: XmlGenericNodePtr) -> Option<XmlGenericNodePtr> {
        unsafe {
            if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
                return None;
            }

            if elem.element_type() == XmlElementType::XmlNamespaceDecl {
                return None;
            }

            if self == elem {
                return None;
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
                cur = last;
            } else {
                while let Some(next) = cur.next() {
                    cur = next;
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
                let content = elem.content.as_deref().unwrap();
                cur.add_content(content);
                xml_free_node(elem);
                return Some(cur.into());
            }
            if matches!(elem.element_type(), XmlElementType::XmlAttributeNode) {
                return Some(
                    add_prop_sibling(
                        XmlAttrPtr::try_from(cur).ok(),
                        XmlAttrPtr::try_from(cur).unwrap(),
                        XmlAttrPtr::try_from(elem).unwrap(),
                    )
                    .into(),
                );
            }

            if elem.document() != cur.document() {
                elem.set_doc(cur.document());
            }
            let parent = cur.parent();
            elem.set_prev(Some(cur));
            elem.set_next(None);
            elem.set_parent(parent);
            cur.set_next(Some(elem));
            if let Some(mut parent) = parent {
                parent.set_last(Some(elem));
            }

            Some(elem)
        }
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
    pub unsafe fn add_prev_sibling(
        mut self,
        mut elem: XmlGenericNodePtr,
    ) -> Option<XmlGenericNodePtr> {
        unsafe {
            if matches!(self.element_type(), XmlElementType::XmlNamespaceDecl) {
                return None;
            }
            if elem.element_type() == XmlElementType::XmlNamespaceDecl {
                return None;
            }

            if self == elem {
                return None;
            }

            elem.unlink();

            if matches!(elem.element_type(), XmlElementType::XmlTextNode) {
                let mut elem = XmlNodePtr::try_from(elem).unwrap();
                if matches!(self.element_type(), XmlElementType::XmlTextNode) {
                    let mut node = XmlNodePtr::try_from(self).unwrap();
                    let tmp = elem.content.get_or_insert_default();
                    tmp.push_str(node.content.as_deref().unwrap());
                    node.set_content(tmp);
                    xml_free_node(elem);
                    return Some(self);
                }
                if let Some(mut prev) = self.prev().filter(|p| {
                    matches!(p.element_type(), XmlElementType::XmlTextNode)
                        && self.name() == p.name()
                }) {
                    let content = elem.content.as_deref().unwrap();
                    prev.add_content(content);
                    xml_free_node(elem);
                    return Some(prev);
                }
            } else if matches!(elem.element_type(), XmlElementType::XmlAttributeNode) {
                return Some(
                    add_prop_sibling(
                        self.prev().map(|p| XmlAttrPtr::try_from(p).unwrap()),
                        XmlAttrPtr::try_from(self).unwrap(),
                        XmlAttrPtr::try_from(elem).unwrap(),
                    )
                    .into(),
                );
            }

            if elem.document() != self.document() {
                elem.set_doc(self.document());
            }
            elem.set_parent(self.parent());
            elem.set_prev(self.prev());
            self.set_prev(Some(elem));
            elem.set_next(Some(self));
            if let Some(mut prev) = elem.prev() {
                prev.set_next(Some(elem));
            }
            if let Some(mut parent) = elem.parent().filter(|p| p.children() == elem.next()) {
                parent.set_children(Some(elem));
            }
            Some(elem)
        }
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
    pub unsafe fn add_next_sibling(
        mut self,
        mut elem: XmlGenericNodePtr,
    ) -> Option<XmlGenericNodePtr> {
        unsafe {
            if self.element_type() == XmlElementType::XmlNamespaceDecl {
                return None;
            }
            if elem.element_type() == XmlElementType::XmlNamespaceDecl {
                return None;
            }

            if self == elem {
                return None;
            }

            elem.unlink();

            if matches!(elem.element_type(), XmlElementType::XmlTextNode) {
                let mut elem = XmlNodePtr::try_from(elem).unwrap();
                if matches!(self.element_type(), XmlElementType::XmlTextNode) {
                    let content = elem.content.as_deref().unwrap();
                    self.add_content(content);
                    xml_free_node(elem);
                    return Some(self);
                }
                if let Some(mut next) = self
                    .next()
                    .filter(|next| {
                        matches!(next.element_type(), XmlElementType::XmlTextNode)
                            && self.name() == next.name()
                    })
                    .map(|next| XmlNodePtr::try_from(next).unwrap())
                {
                    let tmp = elem.content.get_or_insert_default();
                    tmp.push_str(next.content.as_deref().unwrap());
                    next.set_content(tmp);
                    xml_free_node(elem);
                    return Some(next.into());
                }
            } else if matches!(elem.element_type(), XmlElementType::XmlAttributeNode) {
                return Some(
                    add_prop_sibling(
                        XmlAttrPtr::try_from(self).ok(),
                        XmlAttrPtr::try_from(self).unwrap(),
                        XmlAttrPtr::try_from(elem).unwrap(),
                    )
                    .into(),
                );
            }

            if elem.document() != self.document() {
                elem.set_doc(self.document());
            }
            elem.set_parent(self.parent());
            elem.set_prev(Some(self));
            elem.set_next(self.next());
            self.set_next(Some(elem));
            if let Some(mut next) = elem.next() {
                next.set_prev(Some(elem));
            }
            if let Some(mut parent) = elem.parent().filter(|p| p.last() == Some(self)) {
                parent.set_last(Some(elem));
            }
            Some(elem)
        }
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

impl<N: NodeCommon> PartialEq<&mut N> for XmlGenericNodePtr {
    fn eq(&self, other: &&mut N) -> bool {
        let ptr = self.as_ptr();
        let other = other as *const &mut N as *mut &mut N;
        let other = unsafe { *other as *mut N as *mut XmlNode };
        std::ptr::eq(ptr, other)
    }
}

impl Deref for XmlGenericNodePtr {
    type Target = dyn NodeCommon;
    fn deref(&self) -> &Self::Target {
        unsafe {
            // Hmmm, there seems to be a bug somewhere that causes some cases
            // to be referenced as a different type than the actual type...
            // As a workaround, branch manually.
            let r#type = self.0.as_ref().element_type();
            match r#type {
                XmlElementType::XmlAttributeDecl => &*(self.0.as_ptr() as *mut XmlAttribute),
                XmlElementType::XmlAttributeNode => &*(self.0.as_ptr() as *mut XmlAttr),
                XmlElementType::XmlDTDNode | XmlElementType::XmlDocumentTypeNode => {
                    &*(self.0.as_ptr() as *mut XmlDtd)
                }
                XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                    &*(self.0.as_ptr() as *mut XmlDoc)
                }
                XmlElementType::XmlElementDecl => &*(self.0.as_ptr() as *mut XmlElement),
                XmlElementType::XmlEntityDecl => &*(self.0.as_ptr() as *mut XmlEntity),
                XmlElementType::XmlNamespaceDecl => &*(self.0.as_ptr() as *mut XmlNs),
                _ => &*(self.0.as_ptr() as *mut XmlNode),
            }
            // // # Safety
            // // I don't implement the pointer casting and addition/subtraction methods
            // // and don't expose the inner `NonNull` for `XmlGenericNodePtr`.
            // // Therefore, as long as the constructor is correctly implemented,
            // // the pointer dereference is valid.
            // self.0.as_ref()
        }
    }
}

impl DerefMut for XmlGenericNodePtr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe {
            let r#type = self.0.as_ref().element_type();
            self.0 = match r#type {
                XmlElementType::XmlAttributeDecl => {
                    NonNull::new_unchecked(self.0.as_ptr() as *mut XmlAttribute)
                }
                XmlElementType::XmlAttributeNode => {
                    NonNull::new_unchecked(self.0.as_ptr() as *mut XmlAttr)
                }
                XmlElementType::XmlDTDNode | XmlElementType::XmlDocumentTypeNode => {
                    NonNull::new_unchecked(self.0.as_ptr() as *mut XmlDtd)
                }
                XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                    NonNull::new_unchecked(self.0.as_ptr() as *mut XmlDoc)
                }
                XmlElementType::XmlElementDecl => {
                    NonNull::new_unchecked(self.0.as_ptr() as *mut XmlElement)
                }
                XmlElementType::XmlEntityDecl => {
                    NonNull::new_unchecked(self.0.as_ptr() as *mut XmlEntity)
                }
                XmlElementType::XmlNamespaceDecl => {
                    NonNull::new_unchecked(self.0.as_ptr() as *mut XmlNs)
                }
                _ => NonNull::new_unchecked(self.0.as_ptr() as *mut XmlNode),
            };
            // # Safety
            // I don't implement the pointer casting and addition/subtraction methods
            // and don't expose the inner `NonNull` for `XmlGenericNodePtr`.
            // Therefore, as long as the constructor is correctly implemented,
            // the pointer dereference is valid.
            self.0.as_mut()
        }
    }
}

use std::{
    cell::RefCell,
    mem::replace,
    rc::{Rc, Weak},
};

use crate::{
    dom::{
        check_no_modification_allowed_err, check_owner_document_sameness,
        named_node_map::{AttributeMap, NamedNodeMap},
        node_list::FilteredSubtreeElementsList,
    },
    parser::split_qname2,
    tree::{validate_name, validate_ncname},
};

use super::{
    DOMException, NodeType,
    attr::AttrRef,
    document::{DocumentRef, DocumentWeakRef},
    node::{Node, NodeConnection, NodeRef, NodeWeakRef},
};

/// Implementation of [Element](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-745549614)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
pub struct Element {
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    /// - `Document`
    /// - `DocumentFragment`
    /// - `EntityReference`
    /// - `Element`
    /// - `Entity`
    parent_node: Option<NodeWeakRef>,
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    /// - `Element`
    /// - `Text`
    /// - `Comment`
    /// - `ProcessingInstruction`
    /// - `CDATASection`
    /// - `EntityReference`
    first_child: Option<NodeRef>,
    last_child: Option<NodeRef>,
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    /// - `DocumentType`
    /// - `EntityReference`
    /// - `Element`
    /// - `ProcessingInstruction`
    /// - `Comment`
    /// - `Text`
    /// - `CDATASection`
    previous_sibling: Option<NodeWeakRef>,
    next_sibling: Option<NodeRef>,
    /// Implementation of `attributes` attribute.
    attributes: AttributeMap,
    pub(super) owner_document: DocumentWeakRef,

    /// Implementation of `tagName` for `Element`.
    /// as same as `nodeName` for `Node`.
    ///
    /// If `local_name` is `Some`, this field represents a QName.
    tag_name: Rc<str>,
    /// Implementation of `namespaceURI` for `Node`.
    namespace_uri: Option<Rc<str>>,
    /// Implementation of `prefix` for `Node`.
    prefix: Option<Rc<str>>,
    /// Implementation of `localName` for `Node`.
    local_name: Option<Rc<str>>,

    // 0: read-only ?
    // 1 - 15: unused
    flag: u16,
}

impl Element {
    /// Get `attributes` attribute of this element.
    pub fn attributes(&self) -> AttributeMap {
        self.attributes.clone()
    }

    /// Rename this element and replace namespaceURI with `ns_uri`.\
    /// This method does not validate the format of `qname` and `ns_uri`
    /// and namespace constraints.
    pub(super) fn rename(&mut self, qname: Rc<str>, ns_uri: Option<Rc<str>>) {
        self.namespace_uri = ns_uri;
        self.tag_name = qname.clone();
        if let Some((prefix, local)) = split_qname2(&qname) {
            self.prefix = Some(prefix.into());
            self.local_name = Some(local.into());
        } else {
            self.prefix = None;
            self.local_name = Some(qname);
        }
    }

    /// # Specification
    /// ```text
    /// ELEMENT_NODE
    ///     Specified attribute nodes of the source element are adopted. Default attributes
    ///     are discarded, though if the document being adopted into defines default attributes
    ///     for this element name, those are assigned. The descendants of the source element
    ///     are recursively adopted.
    /// ```
    fn adopted_to(&mut self, new_doc: DocumentRef) {
        self.owner_document = new_doc.downgrade();
        let mut children = self.first_child.clone();
        while let Some(mut child) = children {
            children = child.next_sibling();
            child.adopted_to(new_doc.clone());
        }

        // `AttrRef::adopted_to` sets its `specified` flag to `true`,
        // so we should retain the attributes before execute `AttrRef::adopted_to`.
        self.attributes.retain(|attr| attr.specified());
        self.attributes.set_owner_document(new_doc.clone());
        for i in 0..self.attributes.length() {
            let mut attr = self.attributes.item(i).unwrap();
            let elem = attr.owner_element();
            attr.adopted_to(new_doc.clone());
            // `AttrRef::adopted_to` sets its ownerElement to `None`,
            // so reset ownerElement manually.
            attr.set_owner_element(elem);
        }

        // TODO: set default attributes
    }
}

/// Wrapper of `Rc<RefCell<Element>>`.
#[derive(Clone)]
pub struct ElementRef(pub(super) Rc<RefCell<Element>>);

impl ElementRef {
    /// Create new [`ElementRef`].
    pub(super) fn new(doc: DocumentRef, tag_name: Rc<str>) -> Self {
        let ddoc = doc.downgrade();
        let mut new = Self(Rc::new(RefCell::new(Element {
            parent_node: None,
            first_child: None,
            last_child: None,
            previous_sibling: None,
            next_sibling: None,
            attributes: AttributeMap::new(ddoc.clone()),
            owner_document: ddoc,
            tag_name: tag_name.clone(),
            namespace_uri: None,
            prefix: None,
            local_name: None,
            flag: 0,
        })));
        new.0.borrow_mut().attributes.set_owner_element(new.clone());

        if let Some(def) = doc.get_default_attributes(&tag_name) {
            for attr in def {
                new.set_attribute_node(attr).ok();
            }
        }
        new
    }

    /// Create new [`ElementRef`] with namespace whose URI is `ns_uri`.
    pub(super) fn with_namespace(
        doc: DocumentRef,
        tag_name: Rc<str>,
        namespace_uri: Option<Rc<str>>,
    ) -> Self {
        let ddoc = doc.downgrade();
        let mut new = if let Some((prefix, local_name)) = split_qname2(&tag_name) {
            Self(Rc::new(RefCell::new(Element {
                parent_node: None,
                first_child: None,
                last_child: None,
                previous_sibling: None,
                next_sibling: None,
                attributes: AttributeMap::new(ddoc.clone()),
                owner_document: ddoc,
                tag_name: tag_name.clone(),
                namespace_uri,
                prefix: Some(prefix.into()),
                local_name: Some(local_name.into()),
                flag: 0,
            })))
        } else {
            Self(Rc::new(RefCell::new(Element {
                parent_node: None,
                first_child: None,
                last_child: None,
                previous_sibling: None,
                next_sibling: None,
                attributes: AttributeMap::new(ddoc.clone()),
                owner_document: ddoc,
                tag_name: tag_name.clone(),
                namespace_uri,
                prefix: None,
                local_name: Some(tag_name.clone()),
                flag: 0,
            })))
        };
        new.0.borrow_mut().attributes.set_owner_element(new.clone());

        if let Some(def) = doc.get_default_attributes(&tag_name) {
            for mut attr in def {
                // In DOM Level 2, even if the namespaceURI is NULL,
                // the prefix and localName must be set.
                // `Attr::rename` set them properly.
                attr.rename(attr.name(), None);
                new.set_attribute_node_ns(attr).ok();
            }
        }
        new
    }

    /// Get `tagName` attribute of this element.
    pub fn tag_name(&self) -> Rc<str> {
        self.0.borrow().tag_name.clone()
    }

    /// Get `attributes` attribute of this element.
    pub fn attributes(&self) -> AttributeMap {
        self.0.borrow().attributes()
    }

    /// Generate [`ElementWeakRef`] from `self`.
    pub fn downgrade(&self) -> ElementWeakRef {
        ElementWeakRef(Rc::downgrade(&self.0))
    }

    /// Implementation of [`getAttribute`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-666EE0F9) method.
    ///
    /// # Note
    /// As the DOM specification describes, this method returns an empty string if the attribute
    /// is not found. Therefore, the return value is not [`Option`].\
    /// You can use [`get_attribute_node`](crate::dom::element::ElementRef::get_attribute_node)
    /// to determine the existence of an attribute.
    ///
    /// # Specification
    /// ```text
    /// Retrieves an attribute value by name.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The name of the attribute to retrieve.
    ///
    /// Return Value
    ///     DOMString The Attr value as a string, or the empty string if that attribute
    ///               does not have a specified or default value.
    ///
    /// No Exceptions
    /// ```
    pub fn get_attribute(&self, name: &str) -> String {
        self.get_attribute_node(name)
            .and_then(|attr| attr.text_content())
            .unwrap_or_default()
    }
    /// Implementation of [`setAttribute`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-F68F082) method.
    ///
    /// # Specification
    /// ```text
    /// Adds a new attribute. If an attribute with that name is already present in the
    /// element, its value is changed to be that of the value parameter. This value is
    /// a simple string; it is not parsed as it is being set. So any markup (such as
    /// syntax to be recognized as an entity reference) is treated as literal text, and
    /// needs to be appropriately escaped by the implementation when it is written out.
    /// In order to assign an attribute value that contains entity references, the user
    /// must create an Attr node plus any Text and EntityReference nodes, build the
    /// appropriate subtree, and use setAttributeNode to assign it as the value of an
    /// attribute.
    /// To set an attribute with a qualified name and namespace URI,
    /// use the setAttributeNS method.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The name of the attribute to create or alter.
    ///     value of type DOMString
    ///         Value to set in string form.
    ///
    /// Exceptions
    ///     DOMException
    ///     INVALID_CHARACTER_ERR:       Raised if the specified name is not an XML name
    ///                                  according to the XML version in use specified in
    ///                                  the Document.xmlVersion attribute.
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///
    /// No Return Value
    /// ```
    pub fn set_attribute(
        &mut self,
        name: impl Into<Rc<str>>,
        value: impl Into<String>,
    ) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;

        let name: Rc<str> = name.into();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }

        let doc = self.owner_document().expect("Internal Error");
        let mut new = doc.create_attribute(name)?;
        let text = doc.create_text_node(value);
        new.append_child(text.into())?;
        self.set_attribute_node(new)?;
        Ok(())
    }
    /// Implementation of [`removeAttribute`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-6D6AC0F9) method.
    ///
    /// # Specification
    /// ```text
    /// Removes an attribute by name. If a default value for the removed attribute is
    /// defined in the DTD, a new attribute immediately appears with the default value
    /// as well as the corresponding namespace URI, local name, and prefix when applicable.
    /// The implementation may handle default values from other schemas similarly but
    /// applications should use Document.normalizeDocument() to guarantee this information
    /// is up-to-date.
    /// If no attribute with this name is found, this method has no effect.
    /// To remove an attribute by local name and namespace URI, use the removeAttributeNS method.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The name of the attribute to remove.
    ///
    /// Exceptions
    ///     DOMException
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///
    /// No Return Value
    /// ```
    pub fn remove_attribute(&mut self, name: &str) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;

        let mut attrs = self.attributes();
        let mut attr = attrs.remove_named_item(name)?;
        attr.set_owner_element(None);

        // If the owner Document has a default attribute,
        // insert it to this element.
        if let Some(def) = self
            .owner_document()
            .and_then(|doc| doc.get_default_attribute(&self.node_name(), name))
        {
            self.set_attribute_node(def)?;
        }

        Ok(())
    }

    /// Implementation of [`getAttributeNode`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-217A91B8) method.
    ///
    /// # Specification
    /// ```text
    /// Retrieves an attribute node by name.
    /// To retrieve an attribute node by qualified name and namespace URI,
    /// use the getAttributeNodeNS method.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The name (nodeName) of the attribute to retrieve.
    ///
    /// Return Value
    ///     Attr The Attr node with the specified name (nodeName) or null
    ///          if there is no such attribute.
    ///
    /// No Exceptions
    /// ```
    pub fn get_attribute_node(&self, name: &str) -> Option<AttrRef> {
        self.attributes().get_named_item(name)
    }
    /// Implementation of [`setAttributeNode`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-887236154) method.
    ///
    /// # Specification
    /// ```text
    /// Adds a new attribute node. If an attribute with that name (nodeName) is already
    /// present in the element, it is replaced by the new one. Replacing an attribute
    /// node by itself has no effect.
    /// To add a new attribute node with a qualified name and namespace URI,
    /// use the setAttributeNodeNS method.
    ///
    /// Parameters
    ///     newAttr of type Attr
    ///         The Attr node to add to the attribute list.
    ///
    /// Return Value
    ///     Attr If the newAttr attribute replaces an existing attribute, the replaced
    ///          Attr node is returned, otherwise null is returned.
    ///
    /// Exceptions
    ///     DOMException
    ///     WRONG_DOCUMENT_ERR:          Raised if newAttr was created from a different
    ///                                  document than the one that created the element.
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///     INUSE_ATTRIBUTE_ERR:         Raised if newAttr is already an attribute of
    ///                                  another Element object. The DOM user must explicitly
    ///                                  clone Attr nodes to re-use them in other elements.
    /// ```
    pub fn set_attribute_node(
        &mut self,
        mut new_attr: AttrRef,
    ) -> Result<Option<AttrRef>, DOMException> {
        check_no_modification_allowed_err(self)?;

        if !check_owner_document_sameness(self, &new_attr) {
            return Err(DOMException::WrongDocumentErr);
        }
        if let Some(owner) = new_attr.owner_element() {
            // Replacing an attribute node by itself has no effect.
            if self.is_same_node(&owner.into()) {
                return Ok(Some(new_attr));
            }
            return Err(DOMException::InuseAttributeErr);
        }

        let mut attrs = self.attributes();
        let res = attrs.set_named_item(new_attr.clone())?;
        new_attr.set_owner_element(Some(self.clone()));
        if self.is_read_only() {
            new_attr.set_read_only();
        }
        if self
            .owner_document()
            .and_then(|doc| doc.doctype())
            .and_then(|doctype| doctype.get_element_decl(&self.node_name()))
            .and_then(|elemdecl| elemdecl.get_attribute_decl(&new_attr.name()))
            .is_some_and(|attrdecl| attrdecl.is_id())
        {
            new_attr.set_is_id(true);
        }
        Ok(res)
    }
    /// Implementation of [`removeAttributeNode`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-D589198) method.
    ///
    /// # Specification
    /// ```text
    /// Removes the specified attribute node. If a default value for the removed Attr node
    /// is defined in the DTD, a new node immediately appears with the default value as well
    /// as the corresponding namespace URI, local name, and prefix when applicable.
    /// The implementation may handle default values from other schemas similarly but
    /// applications should use Document.normalizeDocument() to guarantee this information
    /// is up-to-date.
    ///
    /// Parameters
    ///     oldAttr of type Attr
    ///         The Attr node to remove from the attribute list.
    ///
    /// Return Value
    ///     Attr The Attr node that was removed.
    ///
    /// Exceptions
    ///     DOMException
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///     NOT_FOUND_ERR:               Raised if oldAttr is not an attribute of the element.
    /// ```
    pub fn remove_attribute_node(&mut self, old_attr: AttrRef) -> Result<AttrRef, DOMException> {
        check_no_modification_allowed_err(self)?;

        if !old_attr
            .owner_element()
            .is_some_and(|elem| self.is_same_node(&elem.into()))
        {
            return Err(DOMException::NotFoundErr);
        }
        let attr_name = old_attr.node_name();
        let mut attr = self.attributes().remove_named_item(attr_name.as_ref())?;
        attr.set_owner_element(None);

        // If the owner Document has a default attribute,
        // insert it to this element.
        if let Some(def) = self
            .owner_document()
            .and_then(|doc| doc.get_default_attribute(&self.node_name(), &attr_name))
        {
            self.set_attribute_node(def)?;
        }
        Ok(attr)
    }

    /// Implementation of [`getElementsByTagName`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1938918D) method.
    ///
    /// # Specification
    /// ```text
    /// Returns a NodeList of all descendant Elements with a given tag name, in document order.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The name of the tag to match on. The special value "*" matches all tags.
    ///
    /// Return Value
    ///     NodeList A list of matching Element nodes.
    ///
    /// No Exceptions
    /// ```
    pub fn get_elements_by_tag_name(&self, name: &str) -> FilteredSubtreeElementsList {
        if self.owner_document().is_some_and(|doc| doc.is_html()) {
            FilteredSubtreeElementsList::new(
                self.clone().into(),
                None,
                name.to_owned(),
                |elem, _, name| name == "*" || elem.node_name().eq_ignore_ascii_case(name),
            )
        } else {
            FilteredSubtreeElementsList::new(
                self.clone().into(),
                None,
                name.to_owned(),
                |elem, _, name| name == "*" || elem.node_name().as_ref() == name,
            )
        }
    }

    /// Implementation of [`getAttributeNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-ElGetAttrNS) method.
    ///
    /// # Note
    /// As the DOM specification describes, this method returns an empty string if the attribute
    /// is not found. Therefore, the return value is not [`Option`].\
    /// You can use [`get_attribute_node_ns`](crate::dom::element::ElementRef::get_attribute_node_ns)
    /// to determine the existence of an attribute.
    ///
    /// # Specification
    /// ```text
    /// Retrieves an attribute value by local name and namespace URI.
    /// Per [XML Namespaces], applications must use the value null as the namespaceURI
    /// parameter for methods if they wish to have no namespace.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the attribute to retrieve.
    ///     localName of type DOMString
    ///         The local name of the attribute to retrieve.
    ///
    /// Return Value
    ///     DOMString The Attr value as a string, or the empty string if that attribute
    ///               does not have a specified or default value.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_SUPPORTED_ERR: May be raised if the implementation does not support the
    ///                        feature "XML" and the language exposed through the Document
    ///                        does not support XML Namespaces (such as [HTML 4.01]).
    /// ```
    pub fn get_attribute_ns(
        &self,
        namespace_uri: Option<&str>,
        local_name: &str,
    ) -> Result<String, DOMException> {
        if self.owner_document().is_some_and(|doc| doc.is_html()) {
            return Err(DOMException::NotSupportedErr);
        }

        Ok(self
            .attributes()
            .get_named_item_ns(namespace_uri, local_name)?
            .and_then(|attr| attr.text_content())
            .unwrap_or_default())
    }
    /// Implementation of [`setAttributeNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-ElSetAttrNS) method.
    ///
    /// # Specification
    /// ```text
    /// Adds a new attribute. If an attribute with the same local name and namespace
    /// URI is already present on the element, its prefix is changed to be the prefix
    /// part of the qualifiedName, and its value is changed to be the value parameter.
    /// This value is a simple string; it is not parsed as it is being set. So any
    /// markup (such as syntax to be recognized as an entity reference) is treated as
    /// literal text, and needs to be appropriately escaped by the implementation when
    /// it is written out. In order to assign an attribute value that contains entity
    /// references, the user must create an Attr node plus any Text and EntityReference
    /// nodes, build the appropriate subtree, and use setAttributeNodeNS or
    /// setAttributeNode to assign it as the value of an attribute.
    /// Per [XML Namespaces], applications must use the value null as the namespaceURI
    /// parameter for methods if they wish to have no namespace.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the attribute to create or alter.
    ///     qualifiedName of type DOMString
    ///         The qualified name of the attribute to create or alter.
    ///     value of type DOMString
    ///         The value to set in string form.
    ///
    /// Exceptions
    ///     DOMException
    ///     INVALID_CHARACTER_ERR:       Raised if the specified qualified name is not
    ///                                  an XML name according to the XML version in use
    ///                                  specified in the Document.xmlVersion attribute.
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///     NAMESPACE_ERR:               Raised if the qualifiedName is malformed per the
    ///                                  Namespaces in XML specification, if the
    ///                                  qualifiedName has a prefix and the namespaceURI
    ///                                  is null, if the qualifiedName has a prefix that
    ///                                  is "xml" and the namespaceURI is different from
    ///                                  "http://www.w3.org/XML/1998/namespace", if the
    ///                                  qualifiedName or its prefix is "xmlns" and the
    ///                                  namespaceURI is different from
    ///                                  "http://www.w3.org/2000/xmlns/", or if the
    ///                                  namespaceURI is "http://www.w3.org/2000/xmlns/"
    ///                                  and neither the qualifiedName nor its prefix is
    ///                                  "xmlns".
    ///     NOT_SUPPORTED_ERR:           May be raised if the implementation does not
    ///                                  support the feature "XML" and the language exposed
    ///                                  through the Document does not support XML Namespaces
    ///                                  (such as [HTML 4.01]).
    ///
    /// No Return Value
    /// ```
    pub fn set_attribute_ns(
        &mut self,
        namespace_uri: Option<&str>,
        qualified_name: &str,
        value: impl Into<String>,
    ) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;

        let doc = self.owner_document().expect("Internal Error");
        if doc.is_html() {
            return Err(DOMException::NotSupportedErr);
        }

        // `INVALID_CHARACTER_ERR` and `NAMESPACE_ERR` are checked
        // by `DocumentRef::create_attribute_ns`.
        let mut attr = doc.create_attribute_ns(namespace_uri, qualified_name)?;
        let mut attrs = self.attributes();
        attrs.set_named_item_ns(attr.clone())?;
        let text = doc.create_text_node(value);
        attr.append_child(text.into())?;
        attr.set_owner_element(Some(self.clone()));
        if self.is_read_only() {
            attr.set_read_only();
        }
        if self
            .owner_document()
            .and_then(|doc| doc.doctype())
            .and_then(|doctype| doctype.get_element_decl(&self.node_name()))
            .and_then(|elemdecl| elemdecl.get_attribute_decl(&attr.name()))
            .is_some_and(|attrdecl| attrdecl.is_id())
        {
            attr.set_is_id(true);
        }

        Ok(())
    }
    /// Implementation of [`removeAttributeNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-ElRemAtNS) method.
    ///
    /// # Specification
    /// ```text
    /// Removes an attribute by local name and namespace URI. If a default value for the
    /// removed attribute is defined in the DTD, a new attribute immediately appears with
    /// the default value as well as the corresponding namespace URI, local name, and
    /// prefix when applicable. The implementation may handle default values from other
    /// schemas similarly but applications should use Document.normalizeDocument() to
    /// guarantee this information is up-to-date.
    /// If no attribute with this local name and namespace URI is found, this method has
    /// no effect.
    /// Per [XML Namespaces], applications must use the value null as the namespaceURI
    /// parameter for methods if they wish to have no namespace.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the attribute to remove.
    ///     localName of type DOMString
    ///         The local name of the attribute to remove.
    ///
    /// Exceptions
    ///     DOMException
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///     NOT_SUPPORTED_ERR:           May be raised if the implementation does not
    ///                                  support the feature "XML" and the language exposed
    ///                                  through the Document does not support XML Namespaces
    ///                                  (such as [HTML 4.01]).
    ///
    /// No Return Value
    /// ```
    pub fn remove_attribute_ns(
        &mut self,
        namespace_uri: Option<&str>,
        local_name: &str,
    ) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;

        if self.owner_document().is_some_and(|doc| doc.is_html()) {
            return Err(DOMException::NotSupportedErr);
        }
        let mut attrs = self.attributes();
        let mut attr = attrs.remove_named_item_ns(namespace_uri, local_name)?;
        attr.set_owner_element(None);
        // TODO: restore the default attribute if exists in DTD.
        Ok(())
    }

    /// Implementation of [`getAttributeNodeNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-ElGetAtNodeNS) method.
    ///
    /// # Specification
    /// ```text
    /// Retrieves an Attr node by local name and namespace URI.
    /// Per [XML Namespaces], applications must use the value null as the namespaceURI
    /// parameter for methods if they wish to have no namespace.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the attribute to retrieve.
    ///     localName of type DOMString
    ///         The local name of the attribute to retrieve.
    ///
    /// Return Value
    ///     Attr The Attr node with the specified attribute local name and namespace URI
    ///          or null if there is no such attribute.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_SUPPORTED_ERR: May be raised if the implementation does not support the
    ///                        feature "XML" and the language exposed through the Document
    ///                        does not support XML Namespaces (such as [HTML 4.01]).
    /// ```
    pub fn get_attribute_node_ns(
        &self,
        namespace_uri: Option<&str>,
        local_name: &str,
    ) -> Result<Option<AttrRef>, DOMException> {
        if self.owner_document().is_some_and(|doc| doc.is_html()) {
            return Err(DOMException::NotSupportedErr);
        }
        if let Some(attr) = self
            .attributes()
            .get_named_item_ns(namespace_uri, local_name)?
        {
            return Ok(Some(attr));
        }

        // fallback for the unspecified attribute.
        if let Some(prefix) = self.lookup_prefix(namespace_uri.unwrap_or_default()) {
            let qname = format!("{prefix}:{local_name}");
            if let Some(mut attr) = self
                .attributes()
                .get_named_item(&qname)
                .filter(|attr| !attr.specified())
            {
                // If found, we register the namespaceURI for the attribute.
                attr.rename(qname.into(), namespace_uri.map(|uri| uri.into()));
                self.attributes().set_named_item_ns(attr.clone());
                return Ok(Some(attr));
            }
        }
        Ok(None)
    }
    /// Implementation of [`setAttributeNodeNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-ElSetAtNodeNS) method.
    ///
    /// # Specification
    /// ```text
    /// Adds a new attribute. If an attribute with that local name and that namespace URI is
    /// already present in the element, it is replaced by the new one. Replacing an attribute
    /// node by itself has no effect.
    /// Per [XML Namespaces], applications must use the value null as the namespaceURI
    /// parameter for methods if they wish to have no namespace.
    ///
    /// Parameters
    ///     newAttr of type Attr
    ///         The Attr node to add to the attribute list.
    ///
    /// Return Value
    ///     Attr If the newAttr attribute replaces an existing attribute with the same local
    ///          name and namespace URI, the replaced Attr node is returned, otherwise null
    ///          is returned.
    ///
    /// Exceptions
    ///     DOMException
    ///     WRONG_DOCUMENT_ERR:          Raised if newAttr was created from a different
    ///                                  document than the one that created the element.
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///     INUSE_ATTRIBUTE_ERR:         Raised if newAttr is already an attribute of another
    ///                                  Element object. The DOM user must explicitly clone
    ///                                  Attr nodes to re-use them in other elements.
    ///     NOT_SUPPORTED_ERR:           May be raised if the implementation does not support
    ///                                  the feature "XML" and the language exposed through
    ///                                  the Document does not support XML Namespaces
    ///                                  (such as [HTML 4.01]).
    /// ```
    pub fn set_attribute_node_ns(
        &mut self,
        mut new_attr: AttrRef,
    ) -> Result<Option<AttrRef>, DOMException> {
        check_no_modification_allowed_err(self)?;

        if !check_owner_document_sameness(self, &new_attr) {
            return Err(DOMException::WrongDocumentErr);
        }
        if let Some(owner) = new_attr.owner_element() {
            // Replacing an attribute node by itself has no effect.
            if self.is_same_node(&owner.into()) {
                return Ok(Some(new_attr));
            }
            return Err(DOMException::InuseAttributeErr);
        }

        let mut attrs = self.attributes();
        let res = attrs.set_named_item_ns(new_attr.clone())?;
        new_attr.set_owner_element(Some(self.clone()));
        if self.is_read_only() {
            new_attr.set_read_only();
        }
        if self
            .owner_document()
            .and_then(|doc| doc.doctype())
            .and_then(|doctype| doctype.get_element_decl(&self.node_name()))
            .and_then(|elemdecl| elemdecl.get_attribute_decl(&new_attr.name()))
            .is_some_and(|attrdecl| attrdecl.is_id())
        {
            new_attr.set_is_id(true);
        }
        Ok(res)
    }

    /// Implementation of [`getElementsByTagNameNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-A6C90942) method.
    ///
    /// # Specification
    /// ```text
    /// Returns a NodeList of all the descendant Elements with a given local name and
    /// namespace URI in document order.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the elements to match on. The special value "*" matches
    ///         all namespaces.
    ///     localName of type DOMString
    ///         The local name of the elements to match on. The special value "*" matches
    ///         all local names.
    ///
    /// Return Value
    ///     NodeList A new NodeList object containing all the matched Elements.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_SUPPORTED_ERR: May be raised if the implementation does not support the
    ///                        feature "XML" and the language exposed through the Document
    ///                        does not support XML Namespaces (such as [HTML 4.01]).
    /// ```
    pub fn get_elements_by_tag_name_ns(
        &self,
        namespace_uri: Option<&str>,
        local_name: &str,
    ) -> Result<FilteredSubtreeElementsList, DOMException> {
        if self.owner_document().is_some_and(|doc| doc.is_html()) {
            return Err(DOMException::NotSupportedErr);
        }
        Ok(FilteredSubtreeElementsList::new(
            self.clone().into(),
            namespace_uri.map(|uri| uri.to_owned()),
            local_name.to_owned(),
            |elem, uri, local_name| {
                (local_name == "*"
                    || elem
                        .local_name()
                        .is_some_and(|ln| ln.as_ref() == local_name))
                    && (uri == Some("*")
                        || match (elem.namespace_uri(), uri) {
                            (Some(elem_ns), Some(uri)) if elem_ns.as_ref() == uri => true,
                            (None, None) => true,
                            _ => false,
                        })
            },
        ))
    }

    /// Implementation of [`hasAttribute`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-ElHasAttr) method.
    ///
    /// # Specification
    /// ```text
    /// Returns true when an attribute with a given name is specified on this element
    /// or has a default value, false otherwise.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The name of the attribute to look for.
    ///
    /// Return Value
    ///     boolean true if an attribute with the given name is specified on this element
    ///             or has a default value, false otherwise.
    ///
    /// No Exceptions
    /// ```
    pub fn has_attribute(&self, name: &str) -> bool {
        self.attributes().get_named_item(name).is_some()
    }
    /// Implementation of [`hasAttributeNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-ElHasAttrNS) method.
    ///
    /// # Specification
    /// ```text
    /// Returns true when an attribute with a given local name and namespace URI is specified
    /// on this element or has a default value, false otherwise.
    /// Per [XML Namespaces], applications must use the value null as the namespaceURI
    /// parameter for methods if they wish to have no namespace.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the attribute to look for.
    ///     localName of type DOMString
    ///         The local name of the attribute to look for.
    ///
    /// Return Value
    ///     boolean true if an attribute with the given local name and namespace URI is
    ///             specified or has a default value on this element, false otherwise.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_SUPPORTED_ERR: May be raised if the implementation does not support the
    ///                        feature "XML" and the language exposed through the Document
    ///                        does not support XML Namespaces (such as [HTML 4.01]).
    /// ```
    pub fn has_attribute_ns(
        &self,
        namespace_uri: Option<&str>,
        local_name: &str,
    ) -> Result<bool, DOMException> {
        self.attributes()
            .get_named_item_ns(namespace_uri, local_name)
            .map(|attr| attr.is_some())
    }

    /// Implementation of [`setIdAttribute`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-ElSetIdAttr) method.
    ///
    /// # Specification
    /// ```text
    /// If the parameter isId is true, this method declares the specified attribute to be a
    /// user-determined ID attribute. This affects the value of Attr.isId and the behavior of
    /// Document.getElementById, but does not change any schema that may be in use,
    /// in particular this does not affect the Attr.schemaTypeInfo of the specified Attr node.
    /// Use the value false for the parameter isId to undeclare an attribute for being a
    /// user-determined ID attribute.
    /// To specify an attribute by local name and namespace URI, use the setIdAttributeNS method.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The name of the attribute.
    ///     isId of type boolean
    ///         Whether the attribute is a of type ID.
    ///
    /// Exceptions
    ///     DOMException
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///     NOT_FOUND_ERR:               Raised if the specified node is not an attribute of
    ///                                  this element.
    ///
    /// No Return Value
    /// ```
    pub fn set_id_attribute(&mut self, name: &str, is_id: bool) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;

        let mut attr = self
            .get_attribute_node(name)
            .ok_or(DOMException::NotFoundErr)?;
        attr.set_is_id(is_id);
        Ok(())
    }
    /// Implementation of [`setIdAttributeNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-ElSetIdAttrNS) method.
    ///
    /// # Specification
    /// ```text
    /// If the parameter isId is true, this method declares the specified attribute to be a
    /// user-determined ID attribute. This affects the value of Attr.isId and the behavior of
    /// Document.getElementById, but does not change any schema that may be in use, in
    /// particular this does not affect the Attr.schemaTypeInfo of the specified Attr node.
    /// Use the value false for the parameter isId to undeclare an attribute for being a
    /// user-determined ID attribute.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the attribute.
    ///     localName of type DOMString
    ///         The local name of the attribute.
    ///     isId of type boolean
    ///         Whether the attribute is a of type ID.
    ///
    /// Exceptions
    ///     DOMException
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///     NOT_FOUND_ERR:               Raised if the specified node is not an attribute
    ///                                  of this element.
    ///
    /// No Return Value
    /// ```
    pub fn set_id_attribute_ns(
        &mut self,
        namespace_uri: Option<&str>,
        local_name: &str,
        is_id: bool,
    ) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;

        let mut attr = self
            .get_attribute_node_ns(namespace_uri, local_name)?
            .ok_or(DOMException::NotFoundErr)?;
        attr.set_is_id(is_id);
        Ok(())
    }

    /// Implementation of [`setIdAttributeNode`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-ElSetIdAttrNode) method.
    ///
    /// # Specification
    /// ```text
    /// If the parameter isId is true, this method declares the specified attribute to be a
    /// user-determined ID attribute. This affects the value of Attr.isId and the behavior of
    /// Document.getElementById, but does not change any schema that may be in use, in
    /// particular this does not affect the Attr.schemaTypeInfo of the specified Attr node.
    /// Use the value false for the parameter isId to undeclare an attribute for being a
    /// user-determined ID attribute.
    ///
    /// Parameters
    ///     idAttr of type Attr
    ///         The attribute node.
    ///     isId of type boolean
    ///         Whether the attribute is a of type ID.
    ///
    /// Exceptions
    ///     DOMException
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///     NOT_FOUND_ERR:               Raised if the specified node is not an attribute of
    ///                                  this element.
    ///
    /// No Return Value
    /// ```
    pub fn set_id_attribute_node(
        &mut self,
        mut id_attr: AttrRef,
        is_id: bool,
    ) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;

        if id_attr
            .owner_element()
            .is_none_or(|elem| !self.is_same_node(&NodeRef::Element(elem)))
        {
            return Err(DOMException::NotFoundErr);
        }
        id_attr.set_is_id(is_id);
        Ok(())
    }

    /// Rename this element and replace namespaceURI with `ns_uri`.\
    /// This method does not validate the format of `qname` and `ns_uri`
    /// and namespace constraints.
    pub(super) fn rename(&mut self, qname: Rc<str>, ns_uri: Option<Rc<str>>) {
        self.0.borrow_mut().rename(qname, ns_uri);
    }

    /// Implementation of [`lookupNamespacePrefix`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#namespaces-algorithms-lookupNamespacePrefixAlgo)
    pub(super) fn lookup_namespace_prefix(
        &self,
        namespace_uri: &str,
        orig: ElementRef,
    ) -> Option<Rc<str>> {
        if self.namespace_uri().as_deref() == Some(namespace_uri)
            && orig
                .lookup_namespace_uri(self.prefix().as_deref())
                .as_deref()
                == Some(namespace_uri)
        {
            return self.prefix();
        }

        let attrs = self.attributes();
        for i in 0..attrs.length() {
            let attr = attrs.item(i).unwrap();
            if attr.prefix().as_deref() == Some("xmlns")
                && attr.node_value().as_deref() == Some(namespace_uri)
                && orig
                    .lookup_namespace_uri(attr.local_name().as_deref())
                    .as_deref()
                    == Some(namespace_uri)
            {
                return attr.local_name();
            }
        }

        let mut ancestor = self.parent_node();
        while let Some(par) = ancestor {
            ancestor = par.parent_node();
            if let Some(element) = par.as_element() {
                return element.lookup_namespace_prefix(namespace_uri, orig);
            }
        }
        None
    }
}

impl Node for ElementRef {
    fn node_name(&self) -> Rc<str> {
        self.0.borrow().tag_name.clone()
    }

    fn node_value(&self) -> Option<Rc<str>> {
        None
    }

    fn set_node_value(&mut self, _: impl Into<String>) -> Result<(), DOMException> {
        Ok(())
    }

    fn node_type(&self) -> NodeType {
        NodeType::Element
    }

    fn parent_node(&self) -> Option<NodeRef> {
        self.0
            .borrow()
            .parent_node
            .as_ref()
            .and_then(|par| par.upgrade())
    }

    fn first_child(&self) -> Option<NodeRef> {
        self.0.borrow().first_child.clone()
    }

    fn last_child(&self) -> Option<NodeRef> {
        self.0.borrow().last_child.clone()
    }

    fn previous_sibling(&self) -> Option<NodeRef> {
        self.0
            .borrow()
            .previous_sibling
            .as_ref()
            .and_then(|prev| prev.upgrade())
    }

    fn next_sibling(&self) -> Option<NodeRef> {
        self.0.borrow().next_sibling.clone()
    }

    fn attributes(&self) -> Option<AttributeMap> {
        Some(self.0.borrow().attributes.clone())
    }

    fn owner_document(&self) -> Option<DocumentRef> {
        self.0.borrow().owner_document.upgrade()
    }

    fn clone_node(&self, deep: bool) -> NodeRef {
        let mut elem = ElementRef(Rc::new(RefCell::new(Element {
            parent_node: None,
            first_child: None,
            last_child: None,
            previous_sibling: None,
            next_sibling: None,
            attributes: AttributeMap::new(self.0.borrow().owner_document.clone()),
            owner_document: self.0.borrow().owner_document.clone(),
            tag_name: self.0.borrow().tag_name.clone(),
            namespace_uri: self.0.borrow().namespace_uri.clone(),
            prefix: self.0.borrow().prefix.clone(),
            local_name: self.0.borrow().local_name.clone(),
            flag: 0,
        })));
        elem.0
            .borrow_mut()
            .attributes
            .set_owner_element(elem.clone());

        let attrs = self.attributes();
        for i in 0..attrs.length() {
            let attr = attrs
                .item(i)
                .unwrap()
                .clone_node(true)
                .as_attribute()
                .unwrap();
            if elem.set_attribute_node_ns(attr.clone()).is_err() {
                elem.set_attribute_node(attr).expect("Internal Error");
            }
        }

        if deep {
            let mut children = self.first_child();
            while let Some(child) = children {
                children = child.next_sibling();
                elem.append_child(child.clone_node(true))
                    .expect("Internal Error");
            }
        }

        elem.into()
    }

    fn namespace_uri(&self) -> Option<Rc<str>> {
        self.0.borrow().namespace_uri.clone()
    }

    fn prefix(&self) -> Option<Rc<str>> {
        self.0.borrow().prefix.clone()
    }

    fn set_prefix(&mut self, prefix: Option<impl Into<Rc<str>>>) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;
        let Some(local_name) = self.local_name() else {
            // This should have been created with DOM Level 1 method.
            return Ok(());
        };

        let prefix: Option<Rc<str>> = prefix.map(|pre| pre.into());
        if self.prefix() == prefix {
            return Ok(());
        }
        if let Some(prefix) = prefix {
            if validate_ncname::<false>(&prefix).is_err() {
                return Err(DOMException::InvalidCharacterErr);
            }
            if self.namespace_uri().is_none() {
                return Err(DOMException::NamespaceErr);
            }
            self.0.borrow_mut().prefix = Some(prefix.clone());
            self.0.borrow_mut().tag_name = format!("{prefix}:{local_name}").into();
        } else {
            self.0.borrow_mut().prefix = None;
            self.0.borrow_mut().tag_name = local_name.clone();
        }
        Ok(())
    }

    fn local_name(&self) -> Option<Rc<str>> {
        self.0.borrow().local_name.clone()
    }

    fn is_same_node(&self, other: &NodeRef) -> bool {
        let NodeRef::Element(other) = other else {
            return false;
        };
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn lookup_prefix(&self, ns_uri: &str) -> Option<Rc<str>> {
        self.lookup_namespace_prefix(ns_uri, self.clone())
    }

    fn is_default_namespace(&self, ns_uri: &str) -> bool {
        if self.prefix().is_none() {
            return self
                .namespace_uri()
                .is_some_and(|uri| uri.as_ref() == ns_uri);
        };

        if let Some(attr) = self
            .get_attribute_node("xmlns")
            .or_else(|| self.get_attribute_node_ns(None, "xmlns").ok().flatten())
        {
            return attr.text_content().is_some_and(|attr| attr == ns_uri);
        }

        let mut ancestor = self.parent_node();
        while let Some(par) = ancestor {
            ancestor = par.parent_node();
            if let NodeRef::Element(elem) = par {
                return elem.is_default_namespace(ns_uri);
            }
        }
        false
    }

    fn lookup_namespace_uri(&self, prefix: Option<&str>) -> Option<Rc<str>> {
        if self.prefix().as_deref() == prefix {
            if let Some(namespace_uri) = self.namespace_uri() {
                return Some(namespace_uri);
            }
        }
        if let Some(attr) =
            prefix.and_then(|prefix| self.get_attribute_node(&format!("xmlns:{prefix}")))
        {
            return attr.node_value().filter(|value| !value.is_empty());
        } else if prefix.is_none_or(|prefix| prefix.is_empty()) {
            if let Some(attr) = self.get_attribute_node("xmlns") {
                return attr.node_value().filter(|value| !value.is_empty());
            }
        }
        let mut par = self.parent_node();
        while let Some(now) = par {
            if let Some(elem) = now.as_element() {
                return elem.lookup_namespace_uri(prefix);
            }
            par = now.parent_node();
        }
        None
    }

    fn is_read_only(&self) -> bool {
        self.0.borrow().flag & 0b01 != 0
    }
}

impl NodeConnection for ElementRef {
    fn set_parent_node(&mut self, new_parent: Option<NodeRef>) -> Option<NodeRef> {
        replace(
            &mut self.0.borrow_mut().parent_node,
            new_parent.map(|par| par.downgrade()),
        )
        .and_then(|old| old.upgrade())
    }

    fn set_first_child(&mut self, new_child: Option<NodeRef>) -> Option<NodeRef> {
        replace(&mut self.0.borrow_mut().first_child, new_child)
    }

    fn set_last_child(&mut self, new_child: Option<NodeRef>) -> Option<NodeRef> {
        replace(&mut self.0.borrow_mut().last_child, new_child)
    }

    fn set_previous_sibling(&mut self, new_sibling: Option<NodeRef>) -> Option<NodeRef> {
        replace(
            &mut self.0.borrow_mut().previous_sibling,
            new_sibling.map(|sib| sib.downgrade()),
        )
        .and_then(|old| old.upgrade())
    }

    fn set_next_sibling(&mut self, new_sibling: Option<NodeRef>) -> Option<NodeRef> {
        replace(&mut self.0.borrow_mut().next_sibling, new_sibling)
    }

    fn set_owner_document(&mut self, new_doc: DocumentRef) -> Option<DocumentRef> {
        replace(&mut self.0.borrow_mut().owner_document, new_doc.downgrade()).upgrade()
    }

    fn set_read_only(&mut self) {
        if !self.is_read_only() {
            self.0.borrow_mut().flag |= 0b01;
            let mut children = self.first_child();
            while let Some(mut child) = children {
                children = child.next_sibling();
                if !child.is_read_only() {
                    child.set_read_only();
                }
            }

            let attrs = self.attributes();
            let len = attrs.length();
            for i in 0..len {
                attrs.item(i).unwrap().set_read_only();
            }
        }
    }

    fn unset_read_only(&mut self) {
        if self.is_read_only() {
            self.0.borrow_mut().flag &= !0b01;
            let mut children = self.first_child();
            while let Some(mut child) = children {
                children = child.next_sibling();
                if child.is_read_only() {
                    child.unset_read_only();
                }
            }

            let attrs = self.attributes();
            let len = attrs.length();
            for i in 0..len {
                attrs.item(i).unwrap().unset_read_only();
            }
        }
    }

    fn adopted_to(&mut self, new_doc: DocumentRef) {
        self.0.borrow_mut().adopted_to(new_doc);
    }
}

impl From<ElementRef> for NodeRef {
    fn from(value: ElementRef) -> Self {
        NodeRef::Element(value)
    }
}

impl From<Rc<RefCell<Element>>> for ElementRef {
    fn from(value: Rc<RefCell<Element>>) -> Self {
        Self(value)
    }
}

/// Wrapper of `Weak<RefCell<Element>>`.
#[derive(Clone)]
pub struct ElementWeakRef(Weak<RefCell<Element>>);

impl ElementWeakRef {
    /// Generate [`ElementRef`] from `self`.  
    /// Success conditions are the same as for [`std::rc::Weak::upgrade`].
    pub fn upgrade(&self) -> Option<ElementRef> {
        self.0.upgrade().map(ElementRef)
    }
}

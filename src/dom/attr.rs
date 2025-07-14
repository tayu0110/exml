use std::{
    cell::RefCell,
    collections::HashMap,
    mem::replace,
    rc::{Rc, Weak},
    sync::Arc,
};

use crate::{
    dom::element::Element,
    parser::split_qname2,
    tree::{validate_name, validate_ncname},
};

use super::{
    DOMException, NodeType, XML_NS_NAMESPACE, XML_XML_NAMESPACE, check_no_modification_allowed_err,
    document::{Document, DocumentRef},
    element::ElementRef,
    node::{Node, NodeConnection, NodeRef},
    user_data::{DOMUserData, OperationType, UserDataHandler},
};

/// Implementation of [Attr](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-637646024)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
pub struct Attr {
    // [Interface Attr](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-637646024)
    // ```
    // Attr objects inherit the Node interface,
    // but since they are not actually child nodes of the element they describe,
    // the DOM does not consider them part of the document tree.
    // Thus, the Node attributes parentNode, previousSibling, and nextSibling have
    // a null value for Attr objects.
    // ```
    // parent_node: Option<NodeWeakRef>,
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)  
    /// - `Text`
    /// - `EntityReference`
    first_child: Option<NodeRef>,
    last_child: Option<NodeRef>,
    // previous_sibling: Option<NodeWeakRef>,
    // next_sibling: Option<NodeRef>,
    owner_document: Weak<RefCell<Document>>,

    /// Implementation of `name` attribute of `Attr`.  
    /// as same as `nodeName` for `Node`.
    name: Rc<str>,
    /// Implementation of `ownerElement` attribute of `Attr`.  
    owner_element: Weak<RefCell<Element>>,
    /// Implementation of `namespaceURI` for `Node`.
    namespace_uri: Option<Rc<str>>,
    /// Implementation of `prefix` for `Node`.
    prefix: Option<Rc<str>>,
    /// Implementation of `localName` for `Node`.
    local_name: Option<Rc<str>>,

    /// Implementation of `specified` attribute of `Attr`.
    specified: bool,
    /// Implementation of `isId` attribute of `Attr`.
    is_id: bool,

    user_data: Option<HashMap<String, (DOMUserData, Option<Arc<dyn UserDataHandler>>)>>,

    // 0      : read-only ?
    // 1 - 15 : unused
    flag: u16,
}

impl Attr {
    /// Implementation of `ownerDocument` attribute of `Attr`.  
    pub fn owner_document(&self) -> Option<DocumentRef> {
        self.owner_document.upgrade().map(DocumentRef)
    }

    /// Implementation of `ownerElement` attribute of `Attr`.  
    pub fn owner_element(&self) -> Option<ElementRef> {
        self.owner_element.upgrade().map(From::from)
    }

    /// Implementation of `specified` attribute of `Attr`.
    pub fn specified(&self) -> bool {
        self.specified
    }

    /// Implementation of `isId` attribute of `Attr`.
    pub fn is_id(&self) -> bool {
        self.is_id
    }

    pub(super) fn set_specified(&mut self, is_specified: bool) -> bool {
        replace(&mut self.specified, is_specified)
    }

    /// Set `isId` attribute.\
    /// Return an old flag.
    pub(super) fn set_is_id(&mut self, is_id: bool) -> bool {
        replace(&mut self.is_id, is_id)
    }

    /// Return `value` attribute of `Attr` interface.
    ///
    /// # Specification
    /// [value of type DOMString](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-221662474)
    /// ```text
    /// On retrieval, the value of the attribute is returned as a string.
    /// Character and general entity references are replaced with their values.
    /// See also the method getAttribute on the Element interface.
    /// ```
    pub fn value(&self) -> String {
        let mut buf = String::new();
        let mut children = self.first_child.clone();
        while let Some(child) = children {
            children = child.next_sibling();
            match child {
                NodeRef::EntityReference(entref) => {
                    if let Some(content) = entref.get_entity_ref_value() {
                        buf.push_str(&content);
                    }
                }
                other => {
                    if let Some(value) = other.node_value() {
                        buf.push_str(&value);
                    }
                }
            }
        }
        buf
    }

    /// Rename this element and replace namespaceURI with `ns_uri`.\
    /// This method does not validate the format of `qname` and `ns_uri`
    /// and namespace constraints.
    pub(super) fn rename(&mut self, qname: Rc<str>, ns_uri: Option<Rc<str>>) {
        self.namespace_uri = ns_uri;
        self.name = qname.clone();
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
    /// ATTRIBUTE_NODE
    ///     The ownerElement attribute is set to null and the specified flag is set
    ///     to true on the adopted Attr. The descendants of the source Attr are
    ///     recursively adopted.
    /// ```
    fn adopted_to(&mut self, new_doc: DocumentRef) {
        self.owner_element = Weak::new();
        self.specified = true;
        self.owner_document = Rc::downgrade(&new_doc.0);
        let mut children = self.first_child.clone();
        while let Some(mut child) = children {
            children = child.next_sibling();
            child.adopted_to(new_doc.clone());
        }
    }
}

/// Wrapper of `Rc<RefCell<Attr>>`.
#[derive(Clone)]
pub struct AttrRef(pub(super) Rc<RefCell<Attr>>, pub(super) DocumentRef);

impl AttrRef {
    /// Create new [`AttrRef`].
    pub(super) fn new(doc: DocumentRef, tag_name: Rc<str>) -> Self {
        Self(
            Rc::new(RefCell::new(Attr {
                first_child: None,
                last_child: None,
                owner_document: Rc::downgrade(&doc.0),
                name: tag_name,
                owner_element: Weak::new(),
                namespace_uri: None,
                prefix: None,
                local_name: None,
                specified: true,
                is_id: false,
                user_data: None,
                flag: 0,
            })),
            doc,
        )
    }

    /// Create new [`AttrRef`] with namespace whose URI is `ns_uri`.
    pub(super) fn with_namespace(doc: DocumentRef, tag_name: Rc<str>, ns_uri: Rc<str>) -> Self {
        let new = Self::new(doc, tag_name.clone());
        let mut elem = new.0.borrow_mut();
        elem.namespace_uri = Some(ns_uri);
        if let Some((prefix, local_name)) = split_qname2(&tag_name) {
            elem.prefix = Some(prefix.into());
            elem.local_name = Some(local_name.into());
        } else {
            elem.local_name = Some(tag_name);
        }
        drop(elem);
        new
    }

    /// Implementation of `ownerElement` attribute of `Attr`.  
    pub fn owner_element(&self) -> Option<ElementRef> {
        self.0.borrow().owner_element()
    }

    /// Set `isId` attribute.\
    /// Return an old flag.
    pub(super) fn set_owner_element(&mut self, elem: Option<ElementRef>) {
        self.0.borrow_mut().owner_element =
            elem.map(|elem| Rc::downgrade(&elem.0)).unwrap_or_default();
    }

    /// Implementation of `name` attribute of `Attr`.
    pub fn name(&self) -> Rc<str> {
        self.0.borrow().name.clone()
    }

    /// Implementation of `specified` attribute of `Attr`.
    pub fn specified(&self) -> bool {
        self.0.borrow().specified
    }

    /// Implementation of `value` attribute.
    ///
    /// # Specification
    /// ```text
    /// On retrieval, the value of the attribute is returned as a string. Character and
    /// general entity references are replaced with their values. See also the method
    /// getAttribute on the Element interface.
    /// Some specialized implementations, such as some [SVG 1.1] implementations, may do
    /// normalization automatically, even after mutation; in such case, the value on
    /// retrieval may differ from the value on setting.
    /// ```
    pub fn value(&self) -> String {
        self.0.borrow().value()
    }

    /// Implementation of `value` attribute.
    ///
    /// # Specification
    /// ```text
    /// On setting, this creates a Text node with the unparsed contents of the string,
    /// i.e. any characters that an XML processor would recognize as markup are instead
    /// treated as literal text. See also the method Element.setAttribute().
    /// Some specialized implementations, such as some [SVG 1.1] implementations, may do
    /// normalization automatically, even after mutation; in such case, the value on
    /// retrieval may differ from the value on setting.
    ///
    /// Exceptions on setting
    ///     DOMException
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
    /// ```
    pub fn set_value(&mut self, value: impl Into<String>) -> Result<(), DOMException> {
        let mut children = self.first_child();
        while let Some(mut child) = children {
            children = child.next_sibling();
            child.disconnect_parent_and_sibling();
        }
        self.append_child(
            self.owner_document()
                .ok_or(DOMException::WrongDocumentErr)?
                .create_text_node(value)
                .into(),
        )?;
        Ok(())
    }

    /// Implementation of `isId` attribute of `Attr`.
    pub fn is_id(&self) -> bool {
        self.0.borrow().is_id
    }

    pub(super) fn set_specified(&mut self, is_specified: bool) -> bool {
        self.0.borrow_mut().set_specified(is_specified)
    }

    pub(super) fn set_is_id(&mut self, is_id: bool) -> bool {
        self.0.borrow_mut().set_is_id(is_id)
    }

    /// Rename this element and replace namespaceURI with `ns_uri`.\
    /// This method does not validate the format of `qname` and `ns_uri`
    /// and namespace constraints.
    pub(super) fn rename(&mut self, qname: Rc<str>, ns_uri: Option<Rc<str>>) {
        self.0.borrow_mut().rename(qname, ns_uri);
    }
}

impl Node for AttrRef {
    fn node_name(&self) -> Rc<str> {
        self.0.borrow().name.clone()
    }

    fn node_value(&self) -> Option<Rc<str>> {
        Some(self.0.borrow().value().into())
    }

    fn set_node_value(&mut self, value: impl Into<String>) -> Result<(), DOMException> {
        self.set_value(value)?;
        Ok(())
    }

    fn node_type(&self) -> NodeType {
        NodeType::Attribute
    }

    fn first_child(&self) -> Option<NodeRef> {
        self.0.borrow().first_child.clone()
    }

    fn last_child(&self) -> Option<NodeRef> {
        self.0.borrow().last_child.clone()
    }

    fn owner_document(&self) -> Option<DocumentRef> {
        Some(self.1.clone())
    }

    fn clone_node(&self, _deep: bool) -> NodeRef {
        let attr = Attr {
            first_child: None,
            last_child: None,
            owner_document: self.0.borrow().owner_document.clone(),
            name: self.node_name().clone(),
            owner_element: Weak::new(),
            namespace_uri: self.namespace_uri().clone(),
            prefix: self.prefix().clone(),
            local_name: self.local_name().clone(),
            // The specification says,
            // "Cloning an Attr directly, as opposed to be cloned as part of an Element cloning
            // operation, returns a specified attribute (specified is true)."
            specified: true,
            is_id: self.is_id(),
            user_data: None,
            flag: 0,
        };
        let mut attr = AttrRef(Rc::new(RefCell::new(attr)), self.1.clone());
        let mut children = self.first_child();
        while let Some(child) = children {
            children = child.next_sibling();
            attr.append_child(child.clone_node(true))
                .expect("Internal Error");
        }

        self.handle_user_data(OperationType::NodeCloned, Some(attr.clone().into()));
        attr.into()
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
            return if prefix.is_some() {
                Err(DOMException::NamespaceErr)
            } else {
                Ok(())
            };
        };

        let prefix: Option<Rc<str>> = prefix.map(|pre| pre.into());
        if self.prefix() == prefix {
            return Ok(());
        }
        if let Some(prefix) = prefix {
            if validate_name::<false>(&prefix).is_err() {
                return Err(DOMException::InvalidCharacterErr);
            }
            if validate_ncname::<false>(&prefix).is_err() {
                return Err(DOMException::NamespaceErr);
            }
            if local_name.as_ref() == "xmlns" {
                return Err(DOMException::NamespaceErr);
            }
            if self.namespace_uri().is_none()
                || (prefix.as_ref() == "xml"
                    && self.namespace_uri().as_deref() != Some(XML_XML_NAMESPACE))
                || (prefix.as_ref() == "xmlns"
                    && self.namespace_uri().as_deref() != Some(XML_NS_NAMESPACE))
            {
                return Err(DOMException::NamespaceErr);
            }
            self.0.borrow_mut().prefix = Some(prefix.clone());
            self.0.borrow_mut().name = format!("{prefix}:{local_name}").into();
        } else {
            if local_name.as_ref() == "xmlns"
                && self.namespace_uri().as_deref() != Some(XML_NS_NAMESPACE)
            {
                return Err(DOMException::NamespaceErr);
            }
            self.0.borrow_mut().prefix = None;
            self.0.borrow_mut().name = local_name.clone();
        }
        Ok(())
    }

    fn local_name(&self) -> Option<Rc<str>> {
        self.0.borrow().local_name.clone()
    }

    fn is_same_node(&self, other: &NodeRef) -> bool {
        let NodeRef::Attribute(other) = other else {
            return false;
        };
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn lookup_prefix(&self, namespace_uri: &str) -> Option<Rc<str>> {
        self.owner_element()?.lookup_prefix(namespace_uri)
    }

    fn is_default_namespace(&self, namespace_uri: &str) -> bool {
        self.owner_element()
            .is_some_and(|elem| elem.is_default_namespace(namespace_uri))
    }

    fn lookup_namespace_uri(&self, prefix: Option<&str>) -> Option<Rc<str>> {
        self.owner_element()?.lookup_namespace_uri(prefix)
    }

    fn set_user_data(
        &mut self,
        key: impl Into<String>,
        data: DOMUserData,
        handler: Option<Arc<dyn UserDataHandler>>,
    ) -> Option<DOMUserData> {
        self.0
            .borrow_mut()
            .user_data
            .get_or_insert_default()
            .insert(key.into(), (data, handler))
            .map(|v| v.0)
    }

    fn get_user_data(&self, key: &str) -> Option<DOMUserData> {
        self.0
            .borrow()
            .user_data
            .as_ref()
            .and_then(|user_data| user_data.get(key))
            .map(|v| v.0.clone())
    }

    fn is_read_only(&self) -> bool {
        self.0.borrow().flag & 1 != 0
    }
}

impl NodeConnection for AttrRef {
    fn set_parent_node(&mut self, _: Option<NodeRef>) -> Option<NodeRef> {
        None
    }

    fn set_first_child(&mut self, new_child: Option<NodeRef>) -> Option<NodeRef> {
        replace(&mut self.0.borrow_mut().first_child, new_child)
    }

    fn set_last_child(&mut self, new_child: Option<NodeRef>) -> Option<NodeRef> {
        replace(&mut self.0.borrow_mut().last_child, new_child)
    }

    fn set_previous_sibling(&mut self, _: Option<NodeRef>) -> Option<NodeRef> {
        None
    }

    fn set_next_sibling(&mut self, _: Option<NodeRef>) -> Option<NodeRef> {
        None
    }

    fn set_owner_document(&mut self, new_doc: DocumentRef) -> Option<DocumentRef> {
        self.0.borrow_mut().owner_document = Rc::downgrade(&new_doc.0);
        Some(replace(&mut self.1, new_doc))
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
        }
    }

    fn adopted_to(&mut self, new_doc: DocumentRef) {
        self.0.borrow_mut().adopted_to(new_doc);

        if let Some(user_data) = self.0.borrow().user_data.as_ref() {
            for (key, value) in user_data.iter() {
                if let Some(handler) = value.1.clone() {
                    handler.handle(
                        OperationType::NodeAdopted,
                        key.as_str(),
                        value.0.clone(),
                        self.clone().into(),
                        None,
                    );
                }
            }
        }
    }

    fn handle_user_data(&self, operation: OperationType, dst: Option<NodeRef>) {
        if let Some(user_data) = self.0.borrow().user_data.as_ref() {
            for (key, value) in user_data.iter() {
                if let Some(handler) = value.1.clone() {
                    handler.handle(
                        operation,
                        key.as_str(),
                        value.0.clone(),
                        self.clone().into(),
                        dst.clone(),
                    );
                }
            }
        }
    }
}

impl From<AttrRef> for NodeRef {
    fn from(value: AttrRef) -> Self {
        NodeRef::Attribute(value)
    }
}

impl From<Rc<RefCell<Attr>>> for AttrRef {
    fn from(value: Rc<RefCell<Attr>>) -> Self {
        let doc = value.borrow().owner_document().unwrap();
        Self(value, doc)
    }
}

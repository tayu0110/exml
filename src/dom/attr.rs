use std::{
    cell::RefCell,
    mem::replace,
    rc::{Rc, Weak},
};

use crate::{dom::DOMException, parser::split_qname2};

use super::{
    NodeType,
    document::{DocumentRef, DocumentWeakRef},
    element::{ElementRef, ElementWeakRef},
    node::{Node, NodeConnection, NodeRef},
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
    owner_document: DocumentWeakRef,

    /// Implementation of `name` attribute of `Attr`.  
    /// as same as `nodeName` for `Node`.
    name: Rc<str>,
    /// Implementation of `ownerElement` attribute of `Attr`.  
    owner_element: Option<ElementWeakRef>,
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
}

impl Attr {
    /// Implementation of `ownerElement` attribute of `Attr`.  
    pub fn owner_element(&self) -> Option<ElementRef> {
        self.owner_element.as_ref().and_then(|elem| elem.upgrade())
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
    pub fn get_value(&self) -> String {
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
        self.owner_element = None;
        self.specified = true;
        self.owner_document = new_doc.downgrade();
        let mut children = self.first_child.clone();
        while let Some(mut child) = children {
            children = child.next_sibling();
            child.adopted_to(new_doc.clone());
        }
    }
}

/// Wrapper of `Rc<RefCell<Attr>>`.
#[derive(Clone)]
pub struct AttrRef(Rc<RefCell<Attr>>);

impl AttrRef {
    /// Create new [`AttrRef`].
    pub(super) fn new(doc: DocumentRef, tag_name: Rc<str>) -> Self {
        let doc = doc.downgrade();
        Self(Rc::new(RefCell::new(Attr {
            first_child: None,
            last_child: None,
            owner_document: doc,
            name: tag_name,
            owner_element: None,
            namespace_uri: None,
            prefix: None,
            local_name: None,
            specified: true,
            is_id: false,
        })))
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
        self.0.borrow_mut().owner_element = elem.map(|elem| elem.downgrade());
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
    pub fn get_value(&self) -> String {
        self.0.borrow().get_value()
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

    /// Generate [`AttrWeakRef`] from `self`.
    pub fn downgrade(&self) -> AttrWeakRef {
        AttrWeakRef(Rc::downgrade(&self.0))
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
        Some(self.0.borrow().get_value().into())
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
        self.0.borrow().owner_document.upgrade()
    }

    fn clone_node(&self, _deep: bool) -> NodeRef {
        let attr = Attr {
            first_child: None,
            last_child: None,
            owner_document: self.0.borrow().owner_document.clone(),
            name: self.node_name().clone(),
            owner_element: None,
            namespace_uri: self.namespace_uri().clone(),
            prefix: self.prefix().clone(),
            local_name: self.local_name().clone(),
            // The specification says,
            // "Cloning an Attr directly, as opposed to be cloned as part of an Element cloning
            // operation, returns a specified attribute (specified is true)."
            specified: true,
            is_id: self.is_id(),
        };
        let mut attr = AttrRef(Rc::new(RefCell::new(attr)));
        let mut children = self.first_child();
        while let Some(child) = children {
            children = child.next_sibling();
            attr.append_child(child.clone_node(true))
                .expect("Internal Error");
        }
        attr.into()
    }

    fn namespace_uri(&self) -> Option<Rc<str>> {
        self.0.borrow().namespace_uri.clone()
    }

    fn prefix(&self) -> Option<Rc<str>> {
        self.0.borrow().prefix.clone()
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

    fn lookup_prefix(&self, ns_uri: &str) -> Option<Rc<str>> {
        self.owner_element()?.lookup_prefix(ns_uri)
    }

    fn is_default_namespace(&self, ns_uri: &str) -> bool {
        self.owner_element()
            .is_some_and(|elem| elem.is_default_namespace(ns_uri))
    }

    fn lookup_namespace_uri(&self, prefix: &str) -> Option<Rc<str>> {
        self.owner_element()?.lookup_namespace_uri(prefix)
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
        replace(&mut self.0.borrow_mut().owner_document, new_doc.downgrade()).upgrade()
    }

    fn adopted_to(&mut self, new_doc: DocumentRef) {
        self.0.borrow_mut().adopted_to(new_doc);
    }
}

impl From<AttrRef> for NodeRef {
    fn from(value: AttrRef) -> Self {
        NodeRef::Attribute(value)
    }
}

/// Wrapper of `Weak<RefCell<Attr>>`.
#[derive(Clone)]
pub struct AttrWeakRef(Weak<RefCell<Attr>>);

impl AttrWeakRef {
    /// Generate [`AttrRef`] from `self`.  
    /// Success conditions are the same as for [`std::rc::Weak::upgrade`].
    pub fn upgrade(&self) -> Option<AttrRef> {
        self.0.upgrade().map(AttrRef)
    }
}

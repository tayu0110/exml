use std::{
    borrow::Cow,
    cell::RefCell,
    collections::{HashMap, hash_map::Entry},
    mem::replace,
    rc::{Rc, Weak},
};

use crate::dom::{
    element::{Element, ElementRef},
    entity::EntityRef,
    notation::NotationRef,
};

use super::{
    DOMException,
    attr::AttrRef,
    check_owner_document_sameness,
    document::{DocumentRef, DocumentWeakRef},
    node::Node,
};

pub trait NamedNodeMap {
    type Item: Node;

    /// Implementation of [`getNamedItem`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1074577549) method.
    ///
    /// # Specification
    /// ```text
    /// Retrieves a node specified by name.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The nodeName of a node to retrieve.
    ///
    /// Return Value
    ///     Node A Node (of any type) with the specified nodeName, or null if it does not
    ///          identify any node in this map.
    ///
    /// No Exceptions
    /// ```
    fn get_named_item(&self, name: &str) -> Option<Self::Item>;
    /// Implementation of [`setNamedItem`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1025163788) method.
    ///
    /// # Specification
    /// ```text
    /// Adds a node using its nodeName attribute. If a node with that name is already present
    /// in this map, it is replaced by the new one. Replacing a node by itself has no effect.
    /// As the nodeName attribute is used to derive the name which the node must be stored
    /// under, multiple nodes of certain types (those that have a "special" string value)
    /// cannot be stored as the names would clash. This is seen as preferable to allowing
    /// nodes to be aliased.
    ///
    /// Parameters
    ///     arg of type Node
    ///         A node to store in this map. The node will later be accessible using the value
    ///         of its nodeName attribute.
    ///
    /// Return Value
    ///     Node If the new Node replaces an existing node the replaced Node is returned,
    ///          otherwise null is returned.
    ///
    /// Exceptions
    ///     DOMException
    ///     WRONG_DOCUMENT_ERR:          Raised if arg was created from a different document
    ///                                  than the one that created this map.
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this map is readonly.
    ///     INUSE_ATTRIBUTE_ERR:         Raised if arg is an Attr that is already an attribute
    ///                                  of another Element object. The DOM user must
    ///                                  explicitly clone Attr nodes to re-use them in other
    ///                                  elements.
    ///     HIERARCHY_REQUEST_ERR:       Raised if an attempt is made to add a node doesn't
    ///                                  belong in this NamedNodeMap. Examples would include
    ///                                  trying to insert something other than an Attr node
    ///                                  into an Element's map of attributes, or a non-Entity
    ///                                  node into the DocumentType's map of Entities.
    /// ```
    fn set_named_item(&mut self, node: Self::Item) -> Result<Option<Self::Item>, DOMException>;
    /// Implementation of [`removeNamedItem`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-D58B193) method.
    ///
    /// # Specification
    /// ```text
    /// Removes a node specified by name. When this map contains the attributes attached to
    /// an element, if the removed attribute is known to have a default value, an attribute
    /// immediately appears containing the default value as well as the corresponding
    /// namespace URI, local name, and prefix when applicable.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The nodeName of the node to remove.
    ///
    /// Return Value
    ///     Node The node removed from this map if a node with such a name exists.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_FOUND_ERR:               Raised if there is no node named name in this map.
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this map is readonly.
    /// ```
    fn remove_named_item(&mut self, name: &str) -> Result<Self::Item, DOMException>;
    /// Implementation of [`item`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-349467F9) method.
    ///
    /// # Specification
    /// ```text
    /// Returns the indexth item in the map. If index is greater than or equal to the number
    /// of nodes in this map, this returns null.
    ///
    /// Parameters
    ///     index of type unsigned long
    ///         Index into this map.
    ///
    /// Return Value
    ///     Node The node at the indexth position in the map, or null if that is not
    ///          a valid index.
    ///
    /// No Exceptions
    /// ```
    fn item(&self, index: usize) -> Option<Self::Item>;
    /// Implementation of [`length`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-6D0FB19E) attribute.
    ///
    /// # Specification
    /// ```text
    /// length of type unsigned long, readonly
    ///     The number of nodes in this map. The range of valid child node indices is
    ///     0 to length-1 inclusive.
    /// ```
    fn length(&self) -> usize;

    /// Implementation of [`getNamedItemNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-getNamedItemNS) method.
    ///
    /// # Specification
    /// ```text
    /// Retrieves a node specified by local name and namespace URI.
    /// Per [XML Namespaces], applications must use the value null as the namespaceURI
    /// parameter for methods if they wish to have no namespace.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the node to retrieve.
    ///     localName of type DOMString
    ///         The local name of the node to retrieve.
    ///
    /// Return Value
    ///     Node A Node (of any type) with the specified local name and namespace URI,
    ///          or null if they do not identify any node in this map.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_SUPPORTED_ERR: May be raised if the implementation does not support the
    ///                        feature "XML" and the language exposed through the Document
    ///                        does not support XML Namespaces (such as [HTML 4.01]).
    /// ```
    fn get_named_item_ns(
        &self,
        namespace_uri: Option<&str>,
        local_name: &str,
    ) -> Result<Option<Self::Item>, DOMException>;
    /// Implementation of [`setNamedItemNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-setNamedItemNS) method.
    ///
    /// # Specification
    /// ```text
    /// Adds a node using its namespaceURI and localName. If a node with that namespace URI
    /// and that local name is already present in this map, it is replaced by the new one.
    /// Replacing a node by itself has no effect.
    /// Per [XML Namespaces], applications must use the value null as the namespaceURI
    /// parameter for methods if they wish to have no namespace.
    ///
    /// Parameters
    ///     arg of type Node
    ///         A node to store in this map. The node will later be accessible using the value
    ///         of its namespaceURI and localName attributes.
    ///
    /// Return Value
    ///     Node If the new Node replaces an existing node the replaced Node is returned,
    ///          otherwise null is returned.
    ///
    /// Exceptions
    ///     DOMException
    ///     WRONG_DOCUMENT_ERR:          Raised if arg was created from a different document
    ///                                  than the one that created this map.
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this map is readonly.
    ///     INUSE_ATTRIBUTE_ERR:         Raised if arg is an Attr that is already an attribute
    ///                                  of another Element object. The DOM user must
    ///                                  explicitly clone Attr nodes to re-use them in other
    ///                                  elements.
    ///     HIERARCHY_REQUEST_ERR:       Raised if an attempt is made to add a node doesn't
    ///                                  belong in this NamedNodeMap. Examples would include
    ///                                  trying to insert something other than an Attr node
    ///                                  into an Element's map of attributes, or a non-Entity
    ///                                  node into the DocumentType's map of Entities.
    ///     NOT_SUPPORTED_ERR:           May be raised if the implementation does not support
    ///                                  the feature "XML" and the language exposed through
    ///                                  the Document does not support XML Namespaces
    ///                                  (such as [HTML 4.01]).
    /// ```
    fn set_named_item_ns(&mut self, node: Self::Item) -> Result<Option<Self::Item>, DOMException>;
    /// Implementation of [`removeNamedItemNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-removeNamedItemNS) method.
    ///
    /// # Specification
    /// ```text
    /// Removes a node specified by local name and namespace URI. A removed attribute may be
    /// known to have a default value when this map contains the attributes attached to an
    /// element, as returned by the attributes attribute of the Node interface. If so, an
    /// attribute immediately appears containing the default value as well as the
    /// corresponding namespace URI, local name, and prefix when applicable.
    /// Per [XML Namespaces], applications must use the value null as the namespaceURI
    /// parameter for methods if they wish to have no namespace.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the node to remove.
    ///     localName of type DOMString
    ///         The local name of the node to remove.
    ///
    /// Return Value
    ///     Node The node removed from this map if a node with such a local name
    ///          and namespace URI exists.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_FOUND_ERR:               Raised if there is no node with the specified
    ///                                  namespaceURI and localName in this map.
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this map is readonly.
    ///     NOT_SUPPORTED_ERR:           May be raised if the implementation does not support
    ///                                  the feature "XML" and the language exposed through
    ///                                  the Document does not support XML Namespaces
    ///                                  (such as [HTML 4.01]).
    /// ```
    fn remove_named_item_ns(
        &mut self,
        namespace_uri: Option<&str>,
        local_name: &str,
    ) -> Result<Self::Item, DOMException>;
}

/// A [`NamedNodeMap`] for attributes of [`Element`](crate::dom::element::Element).
///
/// Since the data is shared by [`Rc`], [`clone`](Clone::clone) means shallow copy, not deep copy.
#[derive(Clone)]
pub struct AttributeMap {
    owner_document: DocumentWeakRef,
    owner_element: Weak<RefCell<Element>>,
    // (LocalPart, namespaceURI) : if inserted by Level 2 method
    // (Name, None)              : if inserted by Level 1 method
    index: Rc<RefCell<HashMap<(Cow<'static, str>, Option<Cow<'static, str>>), usize>>>,
    // For Name and QName
    index_fullname: Rc<RefCell<HashMap<String, usize>>>,
    data: Rc<RefCell<Vec<AttrRef>>>,
}

impl AttributeMap {
    /// Create new empty [`AttributeMap`]
    pub(super) fn new(doc: DocumentWeakRef) -> Self {
        Self {
            owner_document: doc,
            owner_element: Weak::new(),
            index: Rc::new(RefCell::new(HashMap::new())),
            index_fullname: Rc::new(RefCell::new(HashMap::new())),
            data: Rc::new(RefCell::new(vec![])),
        }
    }

    /// Check if this list is empty.\
    /// In other words, check `self.len() == 0` is satisfied.
    pub fn is_empty(&self) -> bool {
        self.length() == 0
    }

    /// Check if this map has `node` or not.
    ///
    /// This method checks the equality of the `node`.\
    /// If you need to check the sameness, please use [`AttributeMap::has_same_node`].
    pub fn has_equal_node(&self, node: &AttrRef) -> bool {
        self.index_fullname
            .borrow()
            .get(node.node_name().as_ref())
            .is_some_and(|&index| self.data.borrow()[index].is_equal_node(&node.clone().into()))
    }

    /// Check if this map owns `node` or not.
    ///
    /// This method checks the sameness of the `node`.\
    /// If you need to check the equality, please use [`AttributeMap::has_equal_node`].
    pub fn has_same_node(&self, node: &AttrRef) -> bool {
        self.index_fullname
            .borrow()
            .get(node.node_name().as_ref())
            .is_some_and(|&index| self.data.borrow()[index].is_same_node(&node.clone().into()))
    }

    /// Remove elements for which `f` returns `false`.\
    /// The relative order of the elements is preserved.
    ///
    /// # Constraint
    /// - `f` must be idempotent.
    pub fn retain(&mut self, f: impl Fn(&AttrRef) -> bool) {
        let mut remain = vec![];
        let mut new = vec![];
        for (i, data) in self.data.borrow().iter().enumerate() {
            if f(data) {
                new.push(data.clone());
                remain.push(i);
            }
        }
        *self.data.borrow_mut() = new;
        self.index.borrow_mut().retain(|_, index| {
            if let Ok(new) = remain.binary_search(&*index) {
                *index = new;
                true
            } else {
                false
            }
        });
        self.index_fullname.borrow_mut().retain(|_, index| {
            if let Ok(new) = remain.binary_search(&*index) {
                *index = new;
                true
            } else {
                false
            }
        });
    }

    /// If `node` exists in this map, return the index of `node`.\
    /// Otherwise return `None`.
    pub fn index_of(&self, node: &AttrRef) -> Option<usize> {
        self.index_fullname
            .borrow()
            .get(node.node_name().as_ref())
            .copied()
    }

    /// Get the owner Document.
    pub(super) fn owner_document(&self) -> Option<DocumentRef> {
        self.owner_document.upgrade()
    }

    /// Replace ownerDocument of this map.
    ///
    /// All data in this map won't modified.  
    /// If need, the caller should modify the ownerDocument of all data in this map additionally.
    pub(super) fn set_owner_document(&mut self, new_doc: DocumentRef) {
        self.owner_document = new_doc.downgrade();
    }

    pub(super) fn set_owner_element(&mut self, owner_element: ElementRef) {
        self.owner_element = Rc::downgrade(&owner_element.0);
    }
}

impl NamedNodeMap for AttributeMap {
    type Item = AttrRef;

    fn get_named_item(&self, name: &str) -> Option<Self::Item> {
        let &index = self.index_fullname.borrow().get(name)?;
        self.item(index)
    }
    fn set_named_item(&mut self, node: Self::Item) -> Result<Option<Self::Item>, DOMException> {
        if self
            .owner_document
            .upgrade()
            .is_none_or(|doc| !check_owner_document_sameness(&doc, &node))
        {
            return Err(DOMException::WrongDocumentErr);
        }

        if node.owner_element().is_some() {
            return Err(DOMException::InuseAttributeErr);
        }

        let name = node.node_name();
        match self.index_fullname.borrow_mut().entry(name.to_string()) {
            Entry::Occupied(entry) => {
                let &index = entry.get();
                Ok(Some(replace(&mut self.data.borrow_mut()[index], node)))
            }
            Entry::Vacant(entry) => {
                let index = self.data.borrow().len();
                entry.insert(index);
                self.data.borrow_mut().push(node);
                Ok(None)
            }
        }
    }
    fn remove_named_item(&mut self, name: &str) -> Result<Self::Item, DOMException> {
        let &index = self
            .index_fullname
            .borrow()
            .get(name)
            .ok_or(DOMException::NotFoundErr)?;
        if let Some(def) = self.owner_document().and_then(|doc| {
            doc.get_default_attribute(
                &ElementRef::from(self.owner_element.upgrade().unwrap()).node_name(),
                name,
            )
        }) {
            let res = replace(&mut self.data.borrow_mut()[index], def.clone());
            if let Some(local_name) = res.local_name() {
                if def.namespace_uri() != res.namespace_uri() {
                    self.index.borrow_mut().remove(&(
                        local_name.to_string().into(),
                        res.namespace_uri()
                            .as_deref()
                            .map(|uri| uri.to_owned().into()),
                    ));
                    self.index.borrow_mut().insert(
                        (
                            local_name.to_string().into(),
                            def.namespace_uri()
                                .as_deref()
                                .map(|uri| uri.to_owned().into()),
                        ),
                        index,
                    );
                }
            }
            Ok(res)
        } else {
            self.index_fullname.borrow_mut().remove(name);
            self.index_fullname
                .borrow_mut()
                .values_mut()
                .filter(|i| **i > index)
                .for_each(|i| *i -= 1);

            let res = self.data.borrow_mut().remove(index);
            if let Some(local_name) = res.local_name() {
                self.index.borrow_mut().remove(&(
                    local_name.to_string().into(),
                    res.namespace_uri()
                        .as_deref()
                        .map(|uri| uri.to_string().into()),
                ));
            }
            Ok(res)
        }
    }
    fn item(&self, index: usize) -> Option<Self::Item> {
        self.data.borrow().get(index).cloned()
    }
    fn length(&self) -> usize {
        self.data.borrow().len()
    }

    fn get_named_item_ns(
        &self,
        ns_uri: Option<&str>,
        local_name: &str,
    ) -> Result<Option<Self::Item>, DOMException> {
        Ok(self
            .index
            .borrow()
            .get(&(Cow::Borrowed(local_name), ns_uri.map(Cow::Borrowed)))
            .copied()
            .and_then(|index| self.item(index)))
    }
    fn set_named_item_ns(&mut self, node: Self::Item) -> Result<Option<Self::Item>, DOMException> {
        let Some(doc) = self
            .owner_document
            .upgrade()
            .filter(|doc| check_owner_document_sameness(doc, &node))
        else {
            return Err(DOMException::WrongDocumentErr);
        };
        if doc.is_html() {
            return Err(DOMException::NotSupportedErr);
        }

        if node.owner_element().is_some() {
            return Err(DOMException::InuseAttributeErr);
        }

        let ns_uri = node.namespace_uri();
        if let Some(local_name) = node.local_name() {
            if let Some(&index) = self.index.borrow().get(&(
                Cow::Borrowed(local_name.as_ref()),
                ns_uri.as_deref().map(Cow::Borrowed),
            )) {
                let res = replace(&mut self.data.borrow_mut()[index], node.clone());
                if res.node_name() != node.node_name() {
                    self.index_fullname
                        .borrow_mut()
                        .remove(&res.node_name().to_string());
                    self.index_fullname
                        .borrow_mut()
                        .insert(node.node_name().to_string(), index);
                }
                Ok(Some(res))
            } else {
                self.index.borrow_mut().insert(
                    (
                        local_name.to_string().into(),
                        ns_uri.as_deref().map(|uri| uri.to_owned().into()),
                    ),
                    self.data.borrow().len(),
                );
                self.index_fullname
                    .borrow_mut()
                    .insert(node.node_name().to_string(), self.data.borrow().len());
                self.data.borrow_mut().push(node);
                Ok(None)
            }
        } else {
            // Is this correct ??
            self.set_named_item(node)
        }
    }
    fn remove_named_item_ns(
        &mut self,
        ns_uri: Option<&str>,
        local_name: &str,
    ) -> Result<Self::Item, DOMException> {
        if self
            .owner_document
            .upgrade()
            .is_none_or(|doc| doc.is_html())
        {
            return Err(DOMException::NotSupportedErr);
        }
        let &index = self
            .index
            .borrow()
            .get(&(Cow::Borrowed(local_name), ns_uri.map(Cow::Borrowed)))
            .ok_or(DOMException::NotFoundErr)?;
        let attr = self.data.borrow()[index].clone();
        let attr_name = attr.node_name();
        let context_node = attr.owner_element().unwrap();
        if let Some(def) = self
            .owner_document()
            .and_then(|doc| doc.get_default_attribute_ns(context_node, &attr_name))
        {
            // Since the nodeName is the same, there should be no need to modify index_fullname.
            Ok(replace(&mut self.data.borrow_mut()[index], def))
        } else {
            self.index.borrow_mut().retain(|key, value| {
                if *value > index {
                    *value -= 1;
                }
                key.0.as_ref() != local_name || key.1.as_deref() != ns_uri
            });
            self.index_fullname.borrow_mut().retain(|key, value| {
                if *value > index {
                    *value -= 1;
                }
                key.as_str() != attr_name.as_ref()
            });
            Ok(self.data.borrow_mut().remove(index))
        }
    }
}

/// Since the data is shared by [`Rc`], [`clone`](Clone::clone) means shallow copy, not deep copy.
#[derive(Clone)]
pub(crate) struct DTDSubsetMap<N: Node> {
    owner_document: DocumentWeakRef,
    index: Rc<RefCell<HashMap<String, usize>>>,
    data: Rc<RefCell<Vec<N>>>,
}

impl<N: Node> DTDSubsetMap<N> {
    /// Create new empty [`DTDSubsetMap`]
    pub(super) fn new(doc: DocumentWeakRef) -> Self {
        Self {
            owner_document: doc,
            index: Rc::new(RefCell::new(HashMap::new())),
            data: Rc::new(RefCell::new(vec![])),
        }
    }

    /// Return the number of data in this map.
    pub fn len(&self) -> usize {
        self.data.borrow().len()
    }

    /// Check if this map is empty.\
    /// In other words, check `self.len() == 0` is satisfied.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Check if this map has `node` or not.
    ///
    /// This method checks the equality of the `node`.
    pub fn has_equal_node(&self, node: &N) -> bool {
        self.index
            .borrow()
            .get(node.node_name().as_ref())
            .is_some_and(|&index| self.data.borrow()[index].is_equal_node(&node.clone().into()))
    }

    /// Implementation of `getNamedItem` method.
    pub fn get_named_item(&self, name: &str) -> Option<N> {
        let &index = self.index.borrow().get(name)?;
        self.item(index)
    }

    /// Implementation of `setNamedItem` method.
    pub fn set_named_item(&mut self, node: N) -> Result<Option<N>, DOMException> {
        if !check_owner_document_sameness(&self.owner_document.upgrade().unwrap(), &node) {
            return Err(DOMException::WrongDocumentErr);
        }

        let name = node.node_name();
        match self.index.borrow_mut().entry(name.to_string()) {
            Entry::Occupied(entry) => {
                let &index = entry.get();
                Ok(Some(replace(&mut self.data.borrow_mut()[index], node)))
            }
            Entry::Vacant(entry) => {
                let index = self.data.borrow().len();
                entry.insert(index);
                self.data.borrow_mut().push(node);
                Ok(None)
            }
        }
    }

    /// Implementation of `item` method.
    pub fn item(&self, index: usize) -> Option<N> {
        self.data.borrow().get(index).cloned()
    }

    /// Get the owner Document.
    pub(super) fn owner_document(&self) -> Option<DocumentRef> {
        self.owner_document.upgrade()
    }

    /// Replace ownerDocument of this map.
    ///
    /// All data in this map won't modified.  
    /// If need, the caller should modify the ownerDocument of all data in this map additionally.
    pub(super) fn set_owner_document(&mut self, new_doc: DocumentRef) {
        self.owner_document = new_doc.downgrade();
    }
}

pub(crate) type SubsetEntityMap = DTDSubsetMap<EntityRef>;
pub(crate) type SubsetNotationMap = DTDSubsetMap<NotationRef>;

/// Since the data is shared by [`Rc`], [`clone`](Clone::clone) means shallow copy, not deep copy.
#[derive(Clone)]
pub struct DTDMap<N: Node> {
    internal_map: Option<DTDSubsetMap<N>>,
    external_map: Option<DTDSubsetMap<N>>,
}

pub type EntityMap = DTDMap<EntityRef>;
pub type NotationMap = DTDMap<NotationRef>;

impl<N: Node> DTDMap<N> {
    pub(super) fn new(
        internal_map: Option<DTDSubsetMap<N>>,
        external_map: Option<DTDSubsetMap<N>>,
    ) -> Self {
        Self {
            internal_map,
            external_map,
        }
    }
}

impl<N: Node> NamedNodeMap for DTDMap<N> {
    type Item = N;

    fn get_named_item(&self, name: &str) -> Option<Self::Item> {
        self.internal_map
            .as_ref()
            .and_then(|map| map.get_named_item(name))
            .or_else(|| {
                self.external_map
                    .as_ref()
                    .and_then(|map| map.get_named_item(name))
            })
    }
    fn set_named_item(&mut self, _node: Self::Item) -> Result<Option<Self::Item>, DOMException> {
        Err(DOMException::NoModificationAllowedErr)
    }
    fn remove_named_item(&mut self, _name: &str) -> Result<Self::Item, DOMException> {
        Err(DOMException::NoModificationAllowedErr)
    }
    fn item(&self, mut index: usize) -> Option<Self::Item> {
        if let Some(map) = self.internal_map.as_ref() {
            if index < map.len() {
                return map.item(index);
            } else {
                index -= map.len();
            }
        }
        self.external_map.as_ref().and_then(|map| map.item(index))
    }
    fn length(&self) -> usize {
        self.internal_map.as_ref().map_or(0, |map| map.len())
            + self.external_map.as_ref().map_or(0, |map| map.len())
    }

    fn get_named_item_ns(
        &self,
        _ns_uri: Option<&str>,
        _local_name: &str,
    ) -> Result<Option<Self::Item>, DOMException> {
        Err(DOMException::NotSupportedErr)
    }
    fn set_named_item_ns(&mut self, _node: Self::Item) -> Result<Option<Self::Item>, DOMException> {
        Err(DOMException::NotSupportedErr)
    }
    fn remove_named_item_ns(
        &mut self,
        _ns_uri: Option<&str>,
        _local_name: &str,
    ) -> Result<Self::Item, DOMException> {
        Err(DOMException::NotSupportedErr)
    }
}

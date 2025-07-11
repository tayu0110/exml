use std::{
    cell::RefCell,
    collections::HashMap,
    mem::replace,
    rc::{Rc, Weak},
    sync::Arc,
};

use crate::{
    dom::{
        attlistdecl::{AttType, AttlistDeclRef, DefaultDecl},
        document::Document,
        elementdecl::{ContentSpec, ElementDeclRef},
        entity::EntityType,
        named_node_map::{EntityMap, NotationMap, SubsetEntityMap, SubsetNotationMap},
        notation::NotationIdentifier,
        user_data::{DOMUserData, OperationType, UserDataHandler},
    },
    tree::{validate_name, validate_qname},
};

use super::{
    DOMException, NodeType,
    character_data::CommentRef,
    document::DocumentRef,
    entity::EntityRef,
    entity_reference::EntityReferenceRef,
    node::{Node, NodeConnection, NodeRef, NodeWeakRef},
    notation::NotationRef,
    pi::ProcessingInstructionRef,
};

/// Implementation of [DocumentType](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-412266927)
/// interface on [1.5 Extended Interfaces: XML Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-E067D597)
pub struct DocumentType {
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    /// - `Document`
    parent_node: Option<NodeWeakRef>,
    // /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    // /// - no children
    // first_child: Option<NodeRef>,
    // last_child: Option<NodeRef>,
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    /// - `Element`
    /// - `ProcessingInstruction`
    /// - `Comment`
    previous_sibling: Option<NodeWeakRef>,
    next_sibling: Option<NodeRef>,
    owner_document: Weak<RefCell<Document>>,

    /// Implementation of `name` for `DocumentType`.
    /// as same as `nodeName` for `Node`.
    name: Rc<str>,
    /// Implementation of `publicId` for `DocumentType`.
    public_id: Option<Rc<str>>,
    /// Implementation of `systemId` for `DocumentType`.
    system_id: Option<Rc<str>>,

    internal_subset: Option<InternalSubset>,
    external_subset: Option<ExternalSubset>,

    user_data: Option<HashMap<String, (DOMUserData, Option<Arc<dyn UserDataHandler>>)>>,
}

impl DocumentType {
    /// Return an entity that is specified by `name` if exists.
    pub fn get_entity(&self, name: &str) -> Option<EntityRef> {
        self.internal_subset
            .as_ref()
            .and_then(|int| int.get_entity(name))
            .or_else(|| {
                self.external_subset
                    .as_ref()
                    .and_then(|ext| ext.get_entity(name))
            })
    }

    /// Return an elementdecl that is specified by `name` if exists.
    pub fn get_element_decl(&self, name: &str) -> Option<ElementDeclRef> {
        self.internal_subset
            .as_ref()
            .and_then(|int| int.get_element_decl(name))
            .or_else(|| {
                self.external_subset
                    .as_ref()
                    .and_then(|ext| ext.get_element_decl(name))
            })
    }

    /// Add an entity.
    ///
    /// If `EXT` is true, `entity` is added to the external subset,
    /// otherwise, added to the internal subset.
    pub fn add_entity<const EXT: bool>(&mut self, entity: EntityRef) -> Result<(), DOMException> {
        let doc = self.owner_document();
        let name = self.name.clone();
        let public_id = self.public_id.clone();
        let system_id = self.system_id.clone();
        if EXT {
            self.external_subset
                .get_or_insert_with(|| DtdSubset::new(doc, name, public_id, system_id))
                .add_entity(entity)
        } else {
            self.internal_subset
                .get_or_insert_with(|| DtdSubset::new(doc, name, public_id, system_id))
                .add_entity(entity)
        }
    }

    /// Add a notation.
    ///
    /// If `EXT` is true, `entity` is added to the external subset,
    /// otherwise, added to the internal subset.
    pub fn add_notation<const EXT: bool>(
        &mut self,
        notation: NotationRef,
    ) -> Result<(), DOMException> {
        let doc = self.owner_document();
        let name = self.name.clone();
        let public_id = self.public_id.clone();
        let system_id = self.system_id.clone();
        if EXT {
            self.external_subset
                .get_or_insert_with(|| DtdSubset::new(doc, name, public_id, system_id))
                .add_notation(notation)
        } else {
            self.internal_subset
                .get_or_insert_with(|| DtdSubset::new(doc, name, public_id, system_id))
                .add_notation(notation)
        }
    }

    /// Add a elementdecl.
    ///
    /// If `EXT` is true, `element_decl` is added to the external subset,
    /// otherwise, added to the internal subset.
    pub fn add_element_decl<const EXT: bool>(
        &mut self,
        element_decl: ElementDeclRef,
    ) -> Result<(), DOMException> {
        let doc = self.owner_document();
        let name = self.name.clone();
        let public_id = self.public_id.clone();
        let system_id = self.system_id.clone();
        if EXT {
            self.external_subset
                .get_or_insert_with(|| DtdSubset::new(doc, name, public_id, system_id))
                .add_element_decl(element_decl)
        } else {
            self.internal_subset
                .get_or_insert_with(|| DtdSubset::new(doc, name, public_id, system_id))
                .add_element_decl(element_decl)
        }
    }

    /// Add a attlistdecl.
    ///
    /// If `EXT` is true, `attlist_decl` is added to the external subset,
    /// otherwise, added to the internal subset.
    pub fn add_attlist_decl<const EXT: bool>(
        &mut self,
        attlist_decl: AttlistDeclRef,
    ) -> Result<(), DOMException> {
        let doc = self.owner_document();
        let name = self.name.clone();
        let public_id = self.public_id.clone();
        let system_id = self.system_id.clone();
        if EXT {
            self.external_subset
                .get_or_insert_with(|| DtdSubset::new(doc, name, public_id, system_id))
                .add_attlist_decl(attlist_decl)
        } else {
            self.internal_subset
                .get_or_insert_with(|| DtdSubset::new(doc, name, public_id, system_id))
                .add_attlist_decl(attlist_decl)
        }
    }

    fn owner_document(&self) -> Option<DocumentRef> {
        self.owner_document.upgrade().map(From::from)
    }

    fn set_owner_document(&mut self, doc: DocumentRef) -> Option<DocumentRef> {
        if let Some(sub) = self.internal_subset.as_mut() {
            sub.set_owner_document(doc.clone());
        }
        if let Some(sub) = self.external_subset.as_mut() {
            sub.set_owner_document(doc.clone());
        }
        replace(&mut self.owner_document, Rc::downgrade(&doc.0))
            .upgrade()
            .map(From::from)
    }
}

/// Wrapper of `Rc<RefCell<DocumentType>>`.
#[derive(Clone)]
pub struct DocumentTypeRef(Rc<RefCell<DocumentType>>);

impl DocumentTypeRef {
    /// Implementation of [`createDocumentType`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Level-2-Core-DOM-createDocType)
    ///
    /// In the specification, this is implemented in [`DOMImplementation`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-102161490).
    ///
    /// ```text
    /// Creates an empty DocumentType node. Entity declarations and notations are not made
    /// available. Entity reference expansions and default attribute additions do not occur.
    ///
    /// Parameters
    ///     qualifiedName of type DOMString
    ///         The qualified name of the document type to be created.
    ///     publicId of type DOMString
    ///         The external subset public identifier.
    ///     systemId of type DOMString
    ///         The external subset system identifier.
    ///
    /// Return Value
    ///     DocumentType A new DocumentType node with Node.ownerDocument set to null.
    ///
    /// Exceptions
    ///     DOMException
    ///     INVALID_CHARACTER_ERR: Raised if the specified qualified name is not an XML name
    ///                            according to [XML 1.0].
    ///     NAMESPACE_ERR:         Raised if the qualifiedName is malformed.
    ///     NOT_SUPPORTED_ERR:     May be raised if the implementation does not support the
    ///                            feature "XML" and the language exposed through the Document
    ///                            does not support XML Namespaces (such as [HTML 4.01]).
    /// ```
    pub fn new(
        qualified_name: &str,
        public_id: Option<&str>,
        system_id: Option<&str>,
    ) -> Result<Self, DOMException> {
        if validate_name::<false>(qualified_name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }
        if validate_qname::<false>(qualified_name).is_err() {
            return Err(DOMException::NamespaceErr);
        }

        Ok(DocumentTypeRef(Rc::new(RefCell::new(DocumentType {
            parent_node: None,
            previous_sibling: None,
            next_sibling: None,
            owner_document: Weak::new(),
            name: qualified_name.into(),
            public_id: public_id.map(|pubid| pubid.into()),
            system_id: system_id.map(|systemid| systemid.into()),
            internal_subset: None,
            external_subset: None,
            user_data: None,
        }))))
    }

    /// Implementation of [`name`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1844763134) attribute.
    ///
    /// # Specification
    /// ```text
    /// name of type DOMString, readonly
    ///     The name of DTD; i.e., the name immediately following the DOCTYPE keyword.
    /// ```
    pub fn name(&self) -> Rc<str> {
        self.0.borrow().name.clone()
    }

    /// Implementation of [`entities`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1788794630) attribute.
    ///
    /// # Specification
    /// ```text
    /// entities of type NamedNodeMap, readonly
    ///     A NamedNodeMap containing the general entities, both external and internal,
    ///     declared in the DTD. Parameter entities are not contained. Duplicates are
    ///     discarded. For example in:
    ///     --------------------------------
    ///     <!DOCTYPE ex SYSTEM "ex.dtd" [
    ///       <!ENTITY foo "foo">
    ///       <!ENTITY bar "bar">
    ///       <!ENTITY bar "bar2">
    ///       <!ENTITY % baz "baz">
    ///     ]>
    ///     <ex/>
    ///     --------------------------------
    ///     the interface provides access to foo and the first declaration of bar but not
    ///     the second declaration of bar or baz. Every node in this map also implements
    ///     the Entity interface.
    ///     The DOM Level 2 does not support editing entities, therefore entities cannot
    ///     be altered in any way.
    /// ```
    pub fn entities(&self) -> EntityMap {
        EntityMap::new(
            self.0
                .borrow()
                .internal_subset
                .as_ref()
                .map(|sub| sub.entities.clone()),
            self.0
                .borrow()
                .external_subset
                .as_ref()
                .map(|sub| sub.entities.clone()),
        )
    }

    /// Implementation of [`notations`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-D46829EF) attribute.
    ///
    /// # Specification
    /// ```text
    /// notations of type NamedNodeMap, readonly
    ///     A NamedNodeMap containing the notations declared in the DTD. Duplicates are
    ///     discarded. Every node in this map also implements the Notation interface.
    ///     The DOM Level 2 does not support editing notations, therefore notations
    ///     cannot be altered in any way.
    /// ```
    pub fn notations(&self) -> NotationMap {
        NotationMap::new(
            self.0
                .borrow()
                .internal_subset
                .as_ref()
                .map(|sub| sub.notations.clone()),
            self.0
                .borrow()
                .external_subset
                .as_ref()
                .map(|sub| sub.notations.clone()),
        )
    }

    /// Implementation of [`publicId`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-Core-DocType-publicId) attribute.
    ///
    /// # Specification
    /// ```text
    /// publicId of type DOMString, readonly, introduced in DOM Level 2
    ///     The public identifier of the external subset.
    /// ```
    pub fn public_id(&self) -> Option<Rc<str>> {
        self.0.borrow().public_id.clone()
    }

    /// Implementation of [`systemId`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-Core-DocType-systemId) attribute.
    ///
    /// # Specification
    /// ```text
    /// systemId of type DOMString, readonly, introduced in DOM Level 2
    ///     The system identifier of the external subset. This may be an absolute URI or not.
    /// ```
    pub fn system_id(&self) -> Option<Rc<str>> {
        self.0.borrow().system_id.clone()
    }

    /// Generate [`DocumentTypeWeakRef`] from `self`.
    pub fn downgrade(&self) -> DocumentTypeWeakRef {
        DocumentTypeWeakRef(Rc::downgrade(&self.0))
    }

    /// Check if an internal subset exists.
    pub fn has_internal_subset(&self) -> bool {
        self.0.borrow().internal_subset.is_some()
    }

    /// Check if an external subset exists.
    pub fn has_external_subset(&self) -> bool {
        self.0.borrow().external_subset.is_some()
    }

    /// Create a new [`EntityRef`] that has `name` as the entity name.
    ///
    /// This is not a required method by the DOM specification.
    ///
    /// # Errors
    /// - If `name` is not a valid XML Name, return `Err(DOMException::InvalidCharacterErr)`
    pub fn create_entity(
        &self,
        name: impl Into<Rc<str>>,
        etype: EntityType,
    ) -> Result<EntityRef, DOMException> {
        let name: Rc<str> = name.into();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }
        Ok(EntityRef::new(self.owner_document(), name, etype))
    }

    /// Create a new [`NotationRef`] that has `name` as the notation name.
    ///
    /// This is not a required method by the DOM specification.
    ///
    /// # Errors
    /// - If `name` is not a valid XML Name, return `Err(DOMException::InvalidCharacterErr)`
    pub fn create_notation(
        &self,
        name: impl Into<Rc<str>>,
        id: NotationIdentifier,
    ) -> Result<NotationRef, DOMException> {
        let name: Rc<str> = name.into();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }
        Ok(NotationRef::new(self.owner_document(), name, id))
    }

    /// Create a new [`ElementDeclRef`] that has `name` as the element name.
    ///
    /// This is not a required method by the DOM specification.
    ///
    /// # Errors
    /// - If `name` is not a valid XML Name, return `Err(DOMException::InvalidCharacterErr)`
    pub fn create_element_decl(
        &self,
        name: impl Into<Rc<str>>,
        contentspec: ContentSpec,
    ) -> Result<ElementDeclRef, DOMException> {
        let name: Rc<str> = name.into();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }
        Ok(ElementDeclRef::new(
            self.owner_document(),
            name,
            contentspec,
        ))
    }

    /// Create a new [`AttlistDeclRef`] that has `elem_name` as the element name
    /// and `att_name` as the attribute name.
    ///
    /// This is not a required method by the DOM specification.
    ///
    /// # Errors
    /// - If either `elem_name` or `att_name` is not a valid XML Name,
    ///   return `Err(DOMException::InvalidCharacterErr)`
    pub fn create_attlist_decl(
        &self,
        elem_name: impl Into<Rc<str>>,
        att_name: impl Into<Rc<str>>,
        att_type: AttType,
        default_decl: DefaultDecl,
    ) -> Result<AttlistDeclRef, DOMException> {
        let elem_name: Rc<str> = elem_name.into();
        let att_name: Rc<str> = att_name.into();
        if validate_name::<false>(&elem_name).is_err() || validate_name::<false>(&att_name).is_err()
        {
            return Err(DOMException::InvalidCharacterErr);
        }
        Ok(AttlistDeclRef::new(
            self.owner_document(),
            att_name,
            att_type,
            default_decl,
            elem_name,
        ))
    }

    /// Return an entity that is specified by `name` if exists.
    pub fn get_entity(&self, name: &str) -> Option<EntityRef> {
        self.0.borrow().get_entity(name)
    }

    /// Return an elementdecl that is specified by `name` if exists.
    pub fn get_element_decl(&self, name: &str) -> Option<ElementDeclRef> {
        self.0.borrow().get_element_decl(name)
    }

    /// Add an entity.
    ///
    /// If `EXT` is true, `entity` is added to the external subset,
    /// otherwise, added to the internal subset.
    pub fn add_entity<const EXT: bool>(&mut self, entity: EntityRef) -> Result<(), DOMException> {
        self.0.borrow_mut().add_entity::<EXT>(entity)
    }

    /// Add a notation.
    ///
    /// If `EXT` is true, `entity` is added to the external subset,
    /// otherwise, added to the internal subset.
    pub fn add_notation<const EXT: bool>(
        &mut self,
        notation: NotationRef,
    ) -> Result<(), DOMException> {
        self.0.borrow_mut().add_notation::<EXT>(notation)
    }

    /// Add a elementdecl.
    ///
    /// If `EXT` is true, `element_decl` is added to the external subset,
    /// otherwise, added to the internal subset.
    pub fn add_element_decl<const EXT: bool>(
        &mut self,
        element_decl: ElementDeclRef,
    ) -> Result<(), DOMException> {
        self.0.borrow_mut().add_element_decl::<EXT>(element_decl)
    }

    /// Add a attlistdecl.
    ///
    /// If `EXT` is true, `attlist_decl` is added to the external subset,
    /// otherwise, added to the internal subset.
    pub fn add_attlist_decl<const EXT: bool>(
        &mut self,
        attlist_decl: AttlistDeclRef,
    ) -> Result<(), DOMException> {
        self.0.borrow_mut().add_attlist_decl::<EXT>(attlist_decl)
    }

    /// Compare positions of `l` and `r`.
    ///
    /// Return
    ///   - `None` if either `l` or `r` is not in this subset.
    ///   - `Some(Ordering::Less)` if `l` precedes `r`.
    ///   - `Some(Ordering::Equal)` if `l` is as same as `r`.
    ///   - `Some(Ordering::Greater)` if `l` follows `r`.
    pub(super) fn compare_decl_position(
        &self,
        l: &impl Node,
        r: &impl Node,
    ) -> Result<std::cmp::Ordering, (Option<usize>, Option<usize>)> {
        let ri = self
            .0
            .borrow()
            .internal_subset
            .as_ref()
            .map(|int| int.compare_decl_position(l, r));
        if let Some(Ok(cmp)) = ri {
            return Ok(cmp);
        }
        let re = self
            .0
            .borrow()
            .external_subset
            .as_ref()
            .map(|ext| ext.compare_decl_position(l, r));
        if let Some(Ok(cmp)) = re {
            return Ok(cmp);
        }

        // If they are included in different subsets,
        // the one included in the internal subset shall be considered as preceding.
        match (ri, re) {
            (Some(Err((Some(_), None))), Some(Err((None, Some(_))))) => {
                Ok(std::cmp::Ordering::Less)
            }
            (Some(Err((None, Some(_)))), Some(Err((Some(_), None)))) => {
                Ok(std::cmp::Ordering::Greater)
            }
            (Some(Err((Some(li), None))), _) | (_, Some(Err((Some(li), None)))) => {
                Err((Some(li), None))
            }
            (Some(Err((None, Some(ri)))), _) | (_, Some(Err((None, Some(ri))))) => {
                Err((None, Some(ri)))
            }
            _ => unreachable!(),
        }
    }
}

impl Node for DocumentTypeRef {
    fn node_name(&self) -> Rc<str> {
        self.0.borrow().name.clone()
    }

    fn node_value(&self) -> Option<Rc<str>> {
        None
    }

    fn set_node_value(&mut self, _: impl Into<String>) -> Result<(), DOMException> {
        Ok(())
    }

    fn node_type(&self) -> super::NodeType {
        NodeType::DocumentType
    }

    fn parent_node(&self) -> Option<NodeRef> {
        self.0
            .borrow()
            .parent_node
            .as_ref()
            .and_then(|par| par.upgrade())
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

    fn owner_document(&self) -> Option<DocumentRef> {
        self.0.borrow().owner_document()
    }

    fn clone_node(&self, deep: bool) -> NodeRef {
        let doctype = DocumentTypeRef(Rc::new(RefCell::new(DocumentType {
            parent_node: None,
            previous_sibling: None,
            next_sibling: None,
            owner_document: self.0.borrow().owner_document.clone(),
            name: self.0.borrow().name.clone(),
            public_id: self.0.borrow().public_id.clone(),
            system_id: self.0.borrow().system_id.clone(),
            internal_subset: self
                .0
                .borrow()
                .internal_subset
                .as_ref()
                .map(|sub| sub.clone_node(deep)),
            external_subset: self
                .0
                .borrow()
                .external_subset
                .as_ref()
                .map(|sub| sub.clone_node(deep)),
            user_data: None,
        })));

        self.handle_user_data(OperationType::NodeCloned, Some(doctype.clone().into()));
        doctype.into()
    }

    fn text_content(&self) -> Option<String> {
        None
    }

    fn set_text_content(&mut self, _text: impl Into<String>) -> Result<(), DOMException> {
        Ok(())
    }

    fn is_same_node(&self, other: &NodeRef) -> bool {
        let NodeRef::DocumentType(other) = other else {
            return false;
        };
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn lookup_prefix(&self, _ns_uri: &str) -> Option<Rc<str>> {
        None
    }

    fn is_default_namespace(&self, _ns_uri: &str) -> bool {
        false
    }

    fn lookup_namespace_uri(&self, _prefix: Option<&str>) -> Option<Rc<str>> {
        None
    }

    fn is_equal_node(&self, arg: &NodeRef) -> bool {
        if self.is_same_node(arg) {
            return true;
        }
        let NodeRef::DocumentType(other) = arg else {
            return false;
        };
        if self.node_name() != other.node_name()
            || self.public_id() != other.public_id()
            || self.system_id() != other.system_id()
            || self.0.borrow().internal_subset.is_some()
                != other.0.borrow().internal_subset.is_some()
            || self.0.borrow().external_subset.is_some()
                != other.0.borrow().external_subset.is_some()
        {
            return false;
        }

        if let Some((s, o)) = self
            .0
            .borrow()
            .internal_subset
            .as_ref()
            .zip(other.0.borrow().internal_subset.as_ref())
        {
            if !s.is_equal_node(o) {
                return false;
            }
        }
        if let Some((s, o)) = self
            .0
            .borrow()
            .external_subset
            .as_ref()
            .zip(other.0.borrow().external_subset.as_ref())
        {
            if !s.is_equal_node(o) {
                return false;
            }
        }

        true
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
        // DOM Level 3 doesn't support editing DocumentType nodes.
        // DocumentType nodes are read-only.
        true
    }
}

impl NodeConnection for DocumentTypeRef {
    fn set_parent_node(&mut self, new_parent: Option<NodeRef>) -> Option<NodeRef> {
        replace(
            &mut self.0.borrow_mut().parent_node,
            new_parent.map(|par| par.downgrade()),
        )
        .and_then(|old| old.upgrade())
    }

    fn set_first_child(&mut self, _: Option<NodeRef>) -> Option<NodeRef> {
        None
    }

    fn set_last_child(&mut self, _: Option<NodeRef>) -> Option<NodeRef> {
        None
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
        self.0.borrow_mut().set_owner_document(new_doc)
    }

    fn adopted_to(&mut self, _new_doc: DocumentRef) {
        // `DocumentType` nodes cannot be adopted.
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

impl From<DocumentTypeRef> for NodeRef {
    fn from(value: DocumentTypeRef) -> Self {
        NodeRef::DocumentType(value)
    }
}

/// Wrapper of `Weak<RefCell<DocumentType>>`.
#[derive(Clone)]
pub struct DocumentTypeWeakRef(Weak<RefCell<DocumentType>>);

impl DocumentTypeWeakRef {
    /// Generate [`DocumentTypeRef`] from `self`.  
    /// Success conditions are the same as for [`std::rc::Weak::upgrade`].
    pub fn upgrade(&self) -> Option<DocumentTypeRef> {
        self.0.upgrade().map(DocumentTypeRef)
    }
}

/// Internal Subset or External Subset.\
/// If `Ext` is `true`, this type specifies an External Subset,
/// otherwise an Internal Subset.
///
/// This type is not included in the DOM Spec, but is introduced for convenience.
pub struct DtdSubset<const EXT: bool> {
    first_child: Option<Rc<RefCell<MarkupDeclListEntry>>>,
    last_child: Option<Rc<RefCell<MarkupDeclListEntry>>>,

    // If this subset is the independent external subset,
    // this subset may have no owner documents.
    owner_document: Weak<RefCell<Document>>,

    /// Implementation of `name` for `DocumentType`.
    /// as same as `nodeName` for `Node`.
    name: Rc<str>,
    /// Implementation of `entities` for `DocumentType`.
    entities: SubsetEntityMap,
    /// Implementation of `notations` for `DocumentType`.
    notations: SubsetNotationMap,
    /// Implementation of `publicId` for `DocumentType`.
    public_id: Option<Rc<str>>,
    /// Implementation of `systemId` for `DocumentType`.
    system_id: Option<Rc<str>>,

    elements: HashMap<String, ElementDeclRef>,
}

pub type InternalSubset = DtdSubset<false>;
pub type ExternalSubset = DtdSubset<true>;

impl<const EXT: bool> DtdSubset<EXT> {
    fn new(
        doc: Option<DocumentRef>,
        name: impl Into<Rc<str>>,
        public_id: Option<impl Into<Rc<str>>>,
        system_id: Option<impl Into<Rc<str>>>,
    ) -> Self {
        let owner_document = doc.map(|doc| Rc::downgrade(&doc.0)).unwrap_or_default();
        Self {
            first_child: None,
            last_child: None,
            owner_document: owner_document.clone(),
            name: name.into(),
            entities: SubsetEntityMap::new(owner_document.clone()),
            notations: SubsetNotationMap::new(owner_document.clone()),
            public_id: public_id.map(|id| id.into()),
            system_id: system_id.map(|id| id.into()),
            elements: HashMap::new(),
        }
    }

    /// Return an entity that is specified by `name` if exists.
    pub fn get_entity(&self, name: &str) -> Option<EntityRef> {
        self.entities.get_named_item(name)
    }

    /// Return a notation that is specified by `name` if exists.
    pub fn get_notation(&self, name: &str) -> Option<NotationRef> {
        self.notations.get_named_item(name)
    }

    /// Return a elementdecl that is specified by `name` if exists.
    pub fn get_element_decl(&self, name: &str) -> Option<ElementDeclRef> {
        self.elements.get(name).cloned()
    }

    /// Check if this subset has `entity` or not.
    pub fn has_entity(&self, entity: &EntityRef) -> bool {
        self.entities.has_equal_node(entity)
    }

    /// Check if this subset has `notation` or not.
    pub fn has_notation(&self, notation: &NotationRef) -> bool {
        self.notations.has_equal_node(notation)
    }

    fn append_markup_decl(&mut self, data: MarkupDecl) {
        if let Some(last) = self.last_child.clone() {
            let entry = Rc::new(RefCell::new(MarkupDeclListEntry {
                previous: Some(Rc::downgrade(&last)),
                next: None,
                data,
            }));
            last.borrow_mut().next = Some(entry.clone());
            self.last_child = Some(entry);
        } else {
            let entry = Rc::new(RefCell::new(MarkupDeclListEntry {
                previous: None,
                next: None,
                data,
            }));
            self.first_child = Some(entry.clone());
            self.last_child = Some(entry);
        }
    }

    /// Add an entity `entity` to this subset.
    ///
    /// If this subset already has an entity with the same name as the name of the `entity`,
    /// `entity` is not added and this method returns [`Err`].  
    /// The name of the entity is also verified.
    pub fn add_entity(&mut self, entity: EntityRef) -> Result<(), DOMException> {
        let name = entity.node_name();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }
        if self.entities.get_named_item(&name).is_some() {
            // Is this correct ??
            return Err(DOMException::NoDataAllowedErr);
        }

        self.entities.set_named_item(entity.clone())?;
        let decl = MarkupDecl::EntityDecl(entity);
        self.append_markup_decl(decl);
        Ok(())
    }

    /// Add an notation `notation` to this subset.
    ///
    /// If this subset already has an notation with the same name as the name of the `notation`,
    /// `notation` is not added and this method returns [`Err`].  
    /// The name of the notation is also verified.
    pub fn add_notation(&mut self, notation: NotationRef) -> Result<(), DOMException> {
        let name = notation.node_name();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }
        if self.notations.get_named_item(&name).is_some() {
            // Is this correct ??
            return Err(DOMException::NoDataAllowedErr);
        }

        self.notations.set_named_item(notation.clone())?;
        let decl = MarkupDecl::NotationDecl(notation);
        self.append_markup_decl(decl);
        Ok(())
    }

    /// Add an elementdecl `element_decl` to this subset.
    ///
    /// If this subset already has an elementdecl with the same name as the name of the
    /// `element_decl`, `element_decl` is not added and this method returns [`Err`].  
    /// The name of the elementdecl is also verified.
    pub fn add_element_decl(&mut self, element_decl: ElementDeclRef) -> Result<(), DOMException> {
        let name = element_decl.name();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }
        if self.elements.contains_key(name.as_ref()) {
            // Is this correct ??
            return Err(DOMException::NoDataAllowedErr);
        }

        self.elements.insert(name.to_string(), element_decl.clone());
        let decl = MarkupDecl::ElementDecl(element_decl);
        self.append_markup_decl(decl);
        Ok(())
    }

    /// Add an attlistdecl `attlist_decl` to this subset.
    ///
    /// If the owner element of the `attlist_decl` is not found, this method returns [`Err`].  
    /// If this subset already has an attlistdecl with the same name as the name of the
    /// `attlist_decl`, `attlist_decl` is not added and this method returns [`Err`].  
    /// The name of the attlistdecl is also verified.
    pub fn add_attlist_decl(&mut self, attlist_decl: AttlistDeclRef) -> Result<(), DOMException> {
        let name = attlist_decl.name();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }

        let elem_name = attlist_decl.element_name();
        let mut elem = self
            .elements
            .get(elem_name.as_ref())
            .ok_or(DOMException::NotFoundErr)?
            .clone();
        elem.add_attlist_decl(attlist_decl.clone())?;
        let decl = MarkupDecl::AttlistDecl(attlist_decl);
        self.append_markup_decl(decl);
        Ok(())
    }

    fn set_owner_document(&mut self, doc: DocumentRef) -> Option<DocumentRef> {
        self.entities.set_owner_document(doc.clone());
        self.notations.set_owner_document(doc.clone());
        let mut children = self.first_child.clone();
        while let Some(child) = children {
            children = child.borrow().next.clone();
            match &mut child.borrow_mut().data {
                MarkupDecl::AttlistDecl(attlist) => attlist.set_owner_document(doc.clone()),
                MarkupDecl::Comment(comment) => comment.set_owner_document(doc.clone()),
                MarkupDecl::ElementDecl(element) => element.set_owner_document(doc.clone()),
                MarkupDecl::EntityDecl(entity) => entity.set_owner_document(doc.clone()),
                MarkupDecl::NotationDecl(notation) => notation.set_owner_document(doc.clone()),
                MarkupDecl::PEReference(entref) => entref.set_owner_document(doc.clone()),
                MarkupDecl::ProcessingInstruction(pi) => pi.set_owner_document(doc.clone()),
            };
        }
        replace(&mut self.owner_document, Rc::downgrade(&doc.0))
            .upgrade()
            .map(From::from)
    }

    pub fn clone_node(&self, deep: bool) -> Self {
        let mut subset = Self {
            first_child: None,
            last_child: None,
            owner_document: self.owner_document.clone(),
            name: self.name.clone(),
            entities: SubsetEntityMap::new(
                self.entities
                    .owner_document()
                    .map(|doc| Rc::downgrade(&doc.0))
                    .unwrap_or_default(),
            ),
            notations: SubsetNotationMap::new(
                self.notations
                    .owner_document()
                    .map(|doc| Rc::downgrade(&doc.0))
                    .unwrap_or_default(),
            ),
            public_id: self.public_id.clone(),
            system_id: self.system_id.clone(),
            elements: HashMap::new(),
        };

        if deep {
            for i in 0..self.entities.len() {
                let ent = self.entities.item(i).unwrap();
                let NodeRef::Entity(ent) = ent.clone_node(true) else {
                    unreachable!()
                };
                subset.add_entity(ent).unwrap();
            }

            for i in 0..self.notations.len() {
                let not = self.notations.item(i).unwrap();
                let NodeRef::Notation(not) = not.clone_node(true) else {
                    unreachable!()
                };
                subset.add_notation(not).unwrap();
            }

            for element_decl in self.elements.values() {
                let element_decl = element_decl.clone_node(true);
                subset.add_element_decl(element_decl).unwrap();
            }
        }

        subset
    }

    /// Compare positions of `l` and `r`.
    ///
    /// Return
    ///   - `None` if either `l` or `r` is not in this subset.
    ///   - `Some(Ordering::Less)` if `l` precedes `r`.
    ///   - `Some(Ordering::Equal)` if `l` is as same as `r`.
    ///   - `Some(Ordering::Greater)` if `l` follows `r`.
    pub(super) fn compare_decl_position(
        &self,
        l: &impl Node,
        r: &impl Node,
    ) -> Result<std::cmp::Ordering, (Option<usize>, Option<usize>)> {
        let l: NodeRef = l.clone().into();
        let r: NodeRef = r.clone().into();
        if l.is_same_node(&r) {
            return Ok(std::cmp::Ordering::Equal);
        }

        let mut children = self.first_child.clone();
        let mut li = None::<usize>;
        let mut ri = None::<usize>;
        let mut cur = 0;
        while let Some(child) = children {
            children = child.borrow().next.clone();

            match &child.borrow().data {
                MarkupDecl::AttlistDecl(_attlist) => {
                    todo!()
                }
                MarkupDecl::Comment(comment) => {
                    if comment.is_same_node(&l) {
                        li.get_or_insert(cur);
                    }
                    if comment.is_same_node(&r) {
                        ri.get_or_insert(cur);
                    }
                }
                MarkupDecl::ElementDecl(_element) => {
                    todo!()
                }
                MarkupDecl::EntityDecl(entity) => {
                    if entity.is_same_node(&l) {
                        li.get_or_insert(cur);
                    }
                    if entity.is_same_node(&r) {
                        ri.get_or_insert(cur);
                    }
                }
                MarkupDecl::NotationDecl(notation) => {
                    if notation.is_same_node(&l) {
                        li.get_or_insert(cur);
                    }
                    if notation.is_same_node(&r) {
                        ri.get_or_insert(cur);
                    }
                }
                MarkupDecl::PEReference(entref) => {
                    if entref.is_same_node(&l) {
                        li.get_or_insert(cur);
                    }
                    if entref.is_same_node(&r) {
                        ri.get_or_insert(cur);
                    }
                }
                MarkupDecl::ProcessingInstruction(pi) => {
                    if pi.is_same_node(&l) {
                        li.get_or_insert(cur);
                    }
                    if pi.is_same_node(&r) {
                        ri.get_or_insert(cur);
                    }
                }
            }

            if let Some((li, ri)) = li.zip(ri) {
                return Ok(li.cmp(&ri));
            }
            cur += 1;
        }

        Err((li, ri))
    }

    fn is_equal_node(&self, other: &Self) -> bool {
        if std::ptr::eq(self, other) {
            return true;
        }
        if self.name != other.name
            || self.public_id != other.public_id
            || self.system_id != other.system_id
        {
            return false;
        }

        let (latt, ratt) = (self.entities.clone(), other.entities.clone());
        if latt.len() != ratt.len() {
            return false;
        }
        let mut r = (0..ratt.len())
            .flat_map(|i| ratt.item(i))
            .collect::<Vec<_>>();
        for i in 0..latt.len() {
            let att = NodeRef::from(latt.item(i).unwrap());
            let Some(pos) = r.iter().position(|r| r.is_equal_node(&att)) else {
                return false;
            };
            r.swap_remove(pos);
        }

        let (latt, ratt) = (self.notations.clone(), other.notations.clone());
        if latt.len() != ratt.len() {
            return false;
        }
        let mut r = (0..ratt.len())
            .flat_map(|i| ratt.item(i))
            .collect::<Vec<_>>();
        for i in 0..latt.len() {
            let att = NodeRef::from(latt.item(i).unwrap());
            let Some(pos) = r.iter().position(|r| r.is_equal_node(&att)) else {
                return false;
            };
            r.swap_remove(pos);
        }

        true
    }
}

/// Markup declarations.
///
/// # Specification in XML Spec
/// ```text
/// [29] markupdecl ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment
/// ```
pub(crate) enum MarkupDecl {
    // TODO: implement ElementDecl
    ElementDecl(ElementDeclRef),
    // TODO: implement AttlistDecl
    AttlistDecl(AttlistDeclRef),
    EntityDecl(EntityRef),
    NotationDecl(NotationRef),
    ProcessingInstruction(ProcessingInstructionRef),
    Comment(CommentRef),
    // Actually, this is `DeclSep`, but I provide this for convenience.
    PEReference(EntityReferenceRef),
}

pub(crate) struct MarkupDeclListEntry {
    previous: Option<Weak<RefCell<MarkupDeclListEntry>>>,
    next: Option<Rc<RefCell<MarkupDeclListEntry>>>,
    data: MarkupDecl,
}

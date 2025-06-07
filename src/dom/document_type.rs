use std::{
    cell::{LazyCell, RefCell},
    mem::replace,
    rc::{Rc, Weak},
};

use crate::tree::{validate_name, validate_qname};

use super::{
    DOMException, NodeType,
    character_data::CommentRef,
    document::{DocumentRef, DocumentWeakRef},
    entity::EntityRef,
    entity_reference::EntityReferenceRef,
    named_node_map::NamedNodeMap,
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
    owner_document: Option<DocumentWeakRef>,

    /// Implementation of `name` for `DocumentType`.
    /// as same as `nodeName` for `Node`.
    name: Rc<str>,
    /// Implementation of `publicId` for `DocumentType`.
    public_id: Option<Rc<str>>,
    /// Implementation of `systemId` for `DocumentType`.
    system_id: Option<Rc<str>>,

    internal_subset: Option<InternalSubset>,
    external_subset: Option<ExternalSubset>,
}

impl DocumentType {
    /// Return an entity that is specified by `name` if exists.
    pub fn get_entity(&self, name: Rc<str>) -> Option<EntityRef> {
        self.internal_subset
            .as_ref()
            .and_then(|int| int.get_entity(name.clone()))
            .or_else(|| {
                self.external_subset
                    .as_ref()
                    .and_then(|ext| ext.get_entity(name))
            })
    }

    /// Add an entity.
    ///
    /// If `EXT` is true, `entity` is added to the external subset,
    /// otherwise, added to the internal subset.
    pub fn add_entity<const EXT: bool>(&mut self, entity: EntityRef) -> Result<(), DOMException> {
        if EXT {
            let subset = self.external_subset.get_or_insert_with(|| {
                DtdSubset::new(
                    self.owner_document.as_ref().and_then(|doc| doc.upgrade()),
                    self.name.clone(),
                    self.public_id.clone(),
                    self.system_id.clone(),
                )
            });
            subset.add_entity(entity)
        } else {
            let subset = self.internal_subset.get_or_insert_with(|| {
                DtdSubset::new(
                    self.owner_document.as_ref().and_then(|doc| doc.upgrade()),
                    self.name.clone(),
                    self.public_id.clone(),
                    self.system_id.clone(),
                )
            });
            subset.add_entity(entity)
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
        if EXT {
            let subset = self.external_subset.get_or_insert_with(|| {
                DtdSubset::new(
                    self.owner_document.as_ref().and_then(|doc| doc.upgrade()),
                    self.name.clone(),
                    self.public_id.clone(),
                    self.system_id.clone(),
                )
            });
            subset.add_notation(notation)
        } else {
            let subset = self.internal_subset.get_or_insert_with(|| {
                DtdSubset::new(
                    self.owner_document.as_ref().and_then(|doc| doc.upgrade()),
                    self.name.clone(),
                    self.public_id.clone(),
                    self.system_id.clone(),
                )
            });
            subset.add_notation(notation)
        }
    }

    fn set_owner_document(&mut self, doc: DocumentRef) -> Option<DocumentRef> {
        if let Some(sub) = self.internal_subset.as_mut() {
            sub.set_owner_document(doc.clone());
        }
        if let Some(sub) = self.external_subset.as_mut() {
            sub.set_owner_document(doc.clone());
        }
        replace(&mut self.owner_document, Some(doc.downgrade())).and_then(|doc| doc.upgrade())
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
        qname: &str,
        public_id: Option<&str>,
        system_id: Option<&str>,
    ) -> Result<Self, DOMException> {
        if validate_qname::<false>(qname).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }

        Ok(DocumentTypeRef(Rc::new(RefCell::new(DocumentType {
            parent_node: None,
            previous_sibling: None,
            next_sibling: None,
            owner_document: None,
            name: qname.into(),
            public_id: public_id.map(|pubid| pubid.into()),
            system_id: system_id.map(|systemid| systemid.into()),
            internal_subset: None,
            external_subset: None,
        }))))
    }

    /// Implementation of `publicId` attribute.
    pub fn public_id(&self) -> Option<Rc<str>> {
        self.0.borrow().public_id.clone()
    }

    /// Implementation of `systemId` attribute.
    pub fn system_id(&self) -> Option<Rc<str>> {
        self.0.borrow().system_id.clone()
    }

    /// Generate [`DocumentTypeWeakRef`] from `self`.
    pub fn downgrade(&self) -> DocumentTypeWeakRef {
        DocumentTypeWeakRef(Rc::downgrade(&self.0))
    }

    /// Create a new [`EntityRef`] that has `name` as the entity name.
    ///
    /// This is not a required method by the DOM specification.
    ///
    /// # Errors
    /// - If `name` is not a valid XML Name, return `Err(DOMException::InvalidCharacterErr)`
    pub fn create_entity(&self, name: impl Into<Rc<str>>) -> Result<EntityRef, DOMException> {
        let name: Rc<str> = name.into();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }
        Ok(EntityRef::new(self.owner_document(), name))
    }

    /// Create a new [`NotationRef`] that has `name` as the notation name.
    ///
    /// This is not a required method by the DOM specification.
    ///
    /// # Errors
    /// - If `name` is not a valid XML Name, return `Err(DOMException::InvalidCharacterErr)`
    pub fn create_notation(&self, name: impl Into<Rc<str>>) -> Result<NotationRef, DOMException> {
        let name: Rc<str> = name.into();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }
        Ok(NotationRef::new(self.owner_document(), name))
    }

    /// Return an entity that is specified by `name` if exists.
    pub fn get_entity(&self, name: Rc<str>) -> Option<EntityRef> {
        self.0.borrow().get_entity(name)
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
        self.0
            .borrow()
            .owner_document
            .as_ref()
            .and_then(|doc| doc.upgrade())
    }

    fn clone_node(&self, deep: bool) -> NodeRef {
        DocumentTypeRef(Rc::new(RefCell::new(DocumentType {
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
        })))
        .into()
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

    fn lookup_namespace_uri(&self, _prefix: &str) -> Option<Rc<str>> {
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
    owner_document: Option<DocumentWeakRef>,

    /// Implementation of `name` for `DocumentType`.
    /// as same as `nodeName` for `Node`.
    name: Rc<str>,
    /// Implementation of `entities` for `DocumentType`.
    entities: NamedNodeMap<EntityRef>,
    /// Implementation of `notations` for `DocumentType`.
    notations: NamedNodeMap<NotationRef>,
    /// Implementation of `publicId` for `DocumentType`.
    public_id: Option<Rc<str>>,
    /// Implementation of `systemId` for `DocumentType`.
    system_id: Option<Rc<str>>,
}

pub type InternalSubset = DtdSubset<false>;
pub type ExternalSubset = DtdSubset<true>;

thread_local! {
    static DUMMY_DOCUMENT: LazyCell<DocumentRef> = LazyCell::new(|| DocumentRef::new(None, None, None).unwrap());
}

impl<const EXT: bool> DtdSubset<EXT> {
    fn new(
        doc: Option<DocumentRef>,
        name: impl Into<Rc<str>>,
        public_id: Option<impl Into<Rc<str>>>,
        system_id: Option<impl Into<Rc<str>>>,
    ) -> Self {
        let ddoc = doc
            .clone()
            .unwrap_or(DUMMY_DOCUMENT.with(|doc| (**doc).clone()))
            .downgrade();
        Self {
            first_child: None,
            last_child: None,
            owner_document: doc.map(|doc| doc.downgrade()),
            name: name.into(),
            entities: NamedNodeMap::new(ddoc.clone()),
            notations: NamedNodeMap::new(ddoc),
            public_id: public_id.map(|id| id.into()),
            system_id: system_id.map(|id| id.into()),
        }
    }

    /// Return an entity that is specified by `name` if exists.
    pub fn get_entity(&self, name: Rc<str>) -> Option<EntityRef> {
        self.entities.get_named_item(name)
    }

    /// Return a notation that is specified by `name` if exists.
    pub fn get_notation(&self, name: Rc<str>) -> Option<NotationRef> {
        self.notations.get_named_item(name)
    }

    /// Check if this subset has `entity` or not.
    pub fn has_entity(&self, entity: &EntityRef) -> bool {
        self.entities.contains(entity)
    }

    /// Check if this subset has `notation` or not.
    pub fn has_notation(&self, notation: &NotationRef) -> bool {
        self.notations.contains(notation)
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
        if self.entities.get_named_item(name.clone()).is_some() {
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
        if self.notations.get_named_item(name.clone()).is_some() {
            // Is this correct ??
            return Err(DOMException::NoDataAllowedErr);
        }

        self.notations.set_named_item(notation.clone())?;
        let decl = MarkupDecl::NotationDecl(notation);
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
                MarkupDecl::AttlistDecl => {
                    todo!()
                }
                MarkupDecl::Comment(comment) => comment.set_owner_document(doc.clone()),
                MarkupDecl::ElementDecl => {
                    todo!()
                }
                MarkupDecl::EntityDecl(entity) => entity.set_owner_document(doc.clone()),
                MarkupDecl::NotationDecl(notation) => notation.set_owner_document(doc.clone()),
                MarkupDecl::PEReference(entref) => entref.set_owner_document(doc.clone()),
                MarkupDecl::ProcessingInstruction(pi) => pi.set_owner_document(doc.clone()),
            };
        }
        replace(&mut self.owner_document, Some(doc.downgrade())).and_then(|doc| doc.upgrade())
    }

    pub fn clone_node(&self, deep: bool) -> Self {
        let mut subset = Self {
            first_child: None,
            last_child: None,
            owner_document: self.owner_document.clone(),
            name: self.name.clone(),
            entities: NamedNodeMap::new(
                self.entities
                    .owner_document()
                    .unwrap_or(DUMMY_DOCUMENT.with(|doc| (**doc).clone()))
                    .downgrade(),
            ),
            notations: NamedNodeMap::new(
                self.notations
                    .owner_document()
                    .unwrap_or(DUMMY_DOCUMENT.with(|doc| (**doc).clone()))
                    .downgrade(),
            ),
            public_id: self.public_id.clone(),
            system_id: self.system_id.clone(),
        };

        if deep {
            for i in 0..self.entities.len() {
                let ent = self.entities.item(i).unwrap();
                let NodeRef::Entity(ent) = ent.clone_node(true) else {
                    unreachable!()
                };
                subset.add_entity(ent).expect("Internal Error");
            }

            for i in 0..self.notations.len() {
                let not = self.notations.item(i).unwrap();
                let NodeRef::Notation(not) = not.clone_node(true) else {
                    unreachable!()
                };
                subset.add_notation(not).expect("Internal Error");
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
                MarkupDecl::AttlistDecl => {
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
                MarkupDecl::ElementDecl => {
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
    ElementDecl,
    // TODO: implement AttlistDecl
    AttlistDecl,
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

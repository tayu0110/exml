use std::{
    cell::RefCell,
    mem::replace,
    rc::{Rc, Weak},
};

use super::{
    DOMException, NodeType,
    document::{DocumentRef, DocumentWeakRef},
    node::{Node, NodeConnection, NodeRef},
};

pub enum NotationIdentifier {
    ExternalID {
        public_id: Option<Rc<str>>,
        system_id: Rc<str>,
    },
    PublicID {
        public_id: Rc<str>,
    },
}

/// Implementation of [Notation](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-5431D1B9)
/// interface on [1.5 Extended Interfaces: XML Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-E067D597)
pub struct Notation {
    // /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    // /// - no parent
    // parent_node: Option<NodeWeakRef>,
    // /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    // /// - no children
    // first_child: Option<NodeRef>,
    // last_child: Option<NodeRef>,
    // /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    // /// - no sibling
    // previous_sibling: Option<NodeWeakRef>,
    // next_sibling: Option<NodeRef>,
    owner_document: Option<DocumentWeakRef>,
    /// Notation name. as same as `nodeName` for `Node`.
    name: Rc<str>,

    /// Implementation of `publicId` attribute.
    public_id: Option<Rc<str>>,
    /// Implementation of `systemId` attribute.
    system_id: Option<Rc<str>>,
}

/// Wrapper of `Rc<RefCell<Notation>>`.
#[derive(Clone)]
pub struct NotationRef(Rc<RefCell<Notation>>);

impl NotationRef {
    pub(crate) fn new(doc: Option<DocumentRef>, name: Rc<str>, id: NotationIdentifier) -> Self {
        let (public_id, system_id) = match id {
            NotationIdentifier::ExternalID {
                public_id,
                system_id,
            } => (public_id, Some(system_id)),
            NotationIdentifier::PublicID { public_id } => (Some(public_id), None),
        };
        Self(Rc::new(RefCell::new(Notation {
            owner_document: doc.map(|doc| doc.downgrade()),
            name,
            public_id,
            system_id,
        })))
    }

    /// Get `name` attribute of this notation.
    pub fn name(&self) -> Rc<str> {
        self.0.borrow().name.clone()
    }

    /// Implementation of [`publicId`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-54F2B4D0) attribute.
    ///
    /// # Specification
    /// ```text
    /// publicId of type DOMString, readonly
    ///     The public identifier of this notation. If the public identifier was not
    ///     specified, this is null.
    /// ```
    pub fn public_id(&self) -> Option<Rc<str>> {
        self.0.borrow().public_id.clone()
    }

    /// Set the new PublicID.
    ///
    /// Note that the SystemID that has already been set will be removed.
    pub fn set_public_id(&mut self, public_id: impl Into<Rc<str>>) {
        self.0.borrow_mut().system_id = None;
        self.0.borrow_mut().public_id = Some(public_id.into());
    }

    /// Implementation [`systemId`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-E8AAB1D0) attribute.
    ///
    /// # Specification
    /// ```text
    /// systemId of type DOMString, readonly
    ///     The system identifier of this notation. If the system identifier was not
    ///     specified, this is null. This may be an absolute URI or not.
    /// ```
    pub fn system_id(&self) -> Option<Rc<str>> {
        self.0.borrow().system_id.clone()
    }

    /// Set the new ExternalID.
    pub fn set_external_id(
        &mut self,
        public_id: Option<impl Into<Rc<str>>>,
        system_id: impl Into<Rc<str>>,
    ) {
        self.0.borrow_mut().public_id = public_id.map(|id| id.into());
        self.0.borrow_mut().system_id = Some(system_id.into());
    }

    /// Generate [`NotationWeakRef`] from `self`.
    pub fn downgrade(&self) -> NotationWeakRef {
        NotationWeakRef(Rc::downgrade(&self.0))
    }
}

impl Node for NotationRef {
    fn node_name(&self) -> Rc<str> {
        self.0.borrow().name.clone()
    }

    fn node_value(&self) -> Option<Rc<str>> {
        None
    }

    fn set_node_value(&mut self, _: impl Into<String>) -> Result<(), DOMException> {
        Ok(())
    }

    fn node_type(&self) -> NodeType {
        NodeType::Notation
    }

    fn owner_document(&self) -> Option<DocumentRef> {
        self.0
            .borrow()
            .owner_document
            .as_ref()
            .and_then(|doc| doc.upgrade())
    }

    fn clone_node(&self, _deep: bool) -> NodeRef {
        NotationRef(Rc::new(RefCell::new(Notation {
            owner_document: self.0.borrow().owner_document.clone(),
            name: self.0.borrow().name.clone(),
            public_id: self.0.borrow().public_id.clone(),
            system_id: self.0.borrow().system_id.clone(),
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
        let NodeRef::Notation(other) = other else {
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

    fn is_read_only(&self) -> bool {
        // The DOM Core does not support editing Notation nodes; they are therefore readonly.
        true
    }
}

impl NodeConnection for NotationRef {
    fn set_parent_node(&mut self, _: Option<NodeRef>) -> Option<NodeRef> {
        None
    }

    fn set_first_child(&mut self, _: Option<NodeRef>) -> Option<NodeRef> {
        None
    }

    fn set_last_child(&mut self, _: Option<NodeRef>) -> Option<NodeRef> {
        None
    }

    fn set_previous_sibling(&mut self, _: Option<NodeRef>) -> Option<NodeRef> {
        None
    }

    fn set_next_sibling(&mut self, _: Option<NodeRef>) -> Option<NodeRef> {
        None
    }

    fn set_owner_document(&mut self, new_doc: DocumentRef) -> Option<DocumentRef> {
        replace(
            &mut self.0.borrow_mut().owner_document,
            Some(new_doc.downgrade()),
        )
        .and_then(|doc| doc.upgrade())
    }

    fn adopted_to(&mut self, _new_doc: DocumentRef) {
        // `Notation` nodes cannot be adopted.
    }
}

impl From<NotationRef> for NodeRef {
    fn from(value: NotationRef) -> Self {
        NodeRef::Notation(value)
    }
}

/// Wrapper of `Weak<RefCell<Notation>>`.
#[derive(Clone)]
pub struct NotationWeakRef(Weak<RefCell<Notation>>);

impl NotationWeakRef {
    /// Generate [`NotationRef`] from `self`.  
    /// Success conditions are the same as for [`std::rc::Weak::upgrade`].
    pub fn upgrade(&self) -> Option<NotationRef> {
        self.0.upgrade().map(NotationRef)
    }
}

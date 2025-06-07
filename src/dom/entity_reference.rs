use std::{
    cell::RefCell,
    mem::replace,
    rc::{Rc, Weak},
};

use super::{
    NodeType,
    document::{DocumentRef, DocumentWeakRef},
    node::{Node, NodeConnection, NodeRef, NodeWeakRef},
};

/// Implementation of [EntityReference](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-11C98490)
/// interface on [1.5 Extended Interfaces: XML Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-E067D597)
pub struct EntityReference {
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    /// - `DocumentFragment`
    /// - `EntityReference`
    /// - `Element`
    /// - `Attr`
    /// - `Entity`
    parent_node: Option<NodeWeakRef>,
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)  
    /// - `Element`
    /// - `ProcessingInstruction`
    /// - `Comment`
    /// - `Text`
    /// - `CDATASection`
    /// - `EntityReference`
    first_child: Option<NodeRef>,
    last_child: Option<NodeRef>,
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    /// - `EntityReference`
    /// - `Element`
    /// - `ProcessingInstruction`
    /// - `Comment`
    /// - `Text`
    /// - `CDATASection`
    previous_sibling: Option<NodeWeakRef>,
    next_sibling: Option<NodeRef>,
    owner_document: DocumentWeakRef,

    /// Name of entity referenced. as same as `nodeName` for `Node`.
    name: Rc<str>,
}

impl EntityReference {
    /// Build and return the expanded entity value.
    ///
    /// All character and general parsed entities in the value are expanded.
    pub(crate) fn get_entity_ref_value(&self) -> Option<String> {
        let doc = self.owner_document.upgrade()?;
        let ent = doc.get_entity(self.name.clone())?;
        let mut children = ent.first_child();
        let mut buf = String::new();
        while let Some(child) = children {
            match &child {
                NodeRef::EntityReference(entref) => {
                    if let Some(content) = entref.get_entity_ref_value() {
                        buf.push_str(&content);
                    }
                }
                other => {
                    if let Some(content) = other.node_value() {
                        buf.push_str(&content);
                    }
                }
            }
            children = child.next_sibling();
        }
        Some(buf)
    }
}

/// Wrapper of `Rc<RefCell<EntityReference>>`.
#[derive(Clone)]
pub struct EntityReferenceRef(Rc<RefCell<EntityReference>>);

impl EntityReferenceRef {
    /// Create new [`EntityReferenceRef`] whose ownerDocument is `doc`.
    pub(super) fn from_doc(doc: DocumentRef, name: Rc<str>) -> Self {
        Self(Rc::new(RefCell::new(EntityReference {
            parent_node: None,
            first_child: None,
            last_child: None,
            previous_sibling: None,
            next_sibling: None,
            owner_document: doc.downgrade(),
            name,
        })))
    }

    pub(crate) fn get_entity_ref_value(&self) -> Option<String> {
        self.0.borrow().get_entity_ref_value()
    }

    /// Generate [`EntityReferenceWeakRef`] from `self`.
    pub fn downgrade(&self) -> EntityReferenceWeakRef {
        EntityReferenceWeakRef(Rc::downgrade(&self.0))
    }
}

impl Node for EntityReferenceRef {
    fn node_name(&self) -> Rc<str> {
        self.0.borrow().name.clone()
    }

    fn node_value(&self) -> Option<Rc<str>> {
        None
    }

    fn set_node_value(&mut self, _: impl Into<String>) -> Result<(), super::DOMException> {
        Ok(())
    }

    fn node_type(&self) -> NodeType {
        NodeType::EntityReference
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

    fn owner_document(&self) -> Option<DocumentRef> {
        self.0.borrow().owner_document.upgrade()
    }

    fn clone_node(&self, _deep: bool) -> NodeRef {
        let mut entref = EntityReferenceRef(Rc::new(RefCell::new(EntityReference {
            parent_node: None,
            first_child: None,
            last_child: None,
            previous_sibling: None,
            next_sibling: None,
            owner_document: self.0.borrow().owner_document.clone(),
            name: self.0.borrow().name.clone(),
        })));

        let mut children = self.first_child();
        while let Some(child) = children {
            children = child.next_sibling();
            entref.append_child(child.clone_node(true)).unwrap();
        }

        entref.into()
    }

    fn is_same_node(&self, other: &NodeRef) -> bool {
        let NodeRef::EntityReference(other) = other else {
            return false;
        };
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl NodeConnection for EntityReferenceRef {
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

    /// # Specification
    /// ```text
    /// ENTITY_REFERENCE_NODE
    ///     Only the EntityReference node itself is adopted, the descendants are
    ///     discarded, since the source and destination documents might have defined
    ///     the entity differently.
    ///     If the document being imported into provides a definition for this entity
    ///     name, its value is assigned.
    /// ```
    fn adopted_to(&mut self, mut new_doc: DocumentRef) {
        let mut children = self.set_first_child(None);
        while let Some(mut child) = children {
            child.set_parent_node(None);
            children = child.next_sibling();
        }
        self.set_last_child(None);
        self.0.borrow_mut().owner_document = new_doc.downgrade();

        // If new entity reference can be created successfully,
        // move its children to `self`.
        if let Ok(entref) = new_doc.create_entity_reference(self.node_name()) {
            self.set_first_child(entref.first_child());
            self.set_last_child(entref.last_child());
            let mut children = entref.first_child();
            while let Some(mut child) = children {
                child.set_parent_node(Some(self.clone().into()));
                children = child.next_sibling();
            }
        }
    }
}

impl From<EntityReferenceRef> for NodeRef {
    fn from(value: EntityReferenceRef) -> Self {
        NodeRef::EntityReference(value)
    }
}

/// Wrapper of `Weak<RefCell<EntityReference>>`.
#[derive(Clone)]
pub struct EntityReferenceWeakRef(Weak<RefCell<EntityReference>>);

impl EntityReferenceWeakRef {
    /// Generate [`EntityReferenceRef`] from `self`.  
    /// Success conditions are the same as for [`std::rc::Weak::upgrade`].
    pub fn upgrade(&self) -> Option<EntityReferenceRef> {
        self.0.upgrade().map(EntityReferenceRef)
    }
}

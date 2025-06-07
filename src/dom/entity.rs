use std::{
    cell::RefCell,
    mem::replace,
    rc::{Rc, Weak},
};

use crate::error::{__xml_simple_error, XmlErrorDomain, XmlParserErrors};

use super::{
    NodeType,
    document::{DocumentRef, DocumentWeakRef},
    node::{Node, NodeConnection, NodeRef},
};

/// Raise an error.
#[doc(alias = "xmlEntitiesErr")]
pub(super) fn xml_entities_err(code: XmlParserErrors, msg: &str) {
    __xml_simple_error!(XmlErrorDomain::XmlFromTree, code, None, msg);
}

/// Implementation of [Entity](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-527DCFF2)
/// interface on [1.5 Extended Interfaces: XML Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-E067D597)
pub struct Entity {
    // /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    // /// - no parent
    // parent_node: Option<NodeWeakRef>,
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)  
    /// - `Element`
    /// - `ProcessingInstruction`
    /// - `Comment`
    /// - `Text`
    /// - `CDATASection`
    /// - `EntityReference`
    first_child: Option<NodeRef>,
    last_child: Option<NodeRef>,
    // /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    // /// - no sibling
    // previous_sibling: Option<NodeWeakRef>,
    // next_sibling: Option<NodeRef>,
    owner_document: Option<DocumentWeakRef>,

    /// Entity name. as same as `nodeName` for `Node`.
    name: Rc<str>,
    /// Implementation of `publicId` attribute.
    public_id: Option<Rc<str>>,
    /// Implementation of `systemId` attribute.
    system_id: Option<Rc<str>>,
    /// Implementation of `notationName` attribute.
    notation_name: Option<Rc<str>>,
    /// Implementation of `inputEncoding` attribute.
    input_encoding: Option<Rc<str>>,
    /// Implementation of `xmlEncoding` attribute.
    xml_encoding: Option<Rc<str>>,
    /// Implementation of `xmlVersion` attribute.
    xml_version: Option<Rc<str>>,
}

/// Wrapper of `Rc<RefCell<Entity>>`.
#[derive(Clone)]
pub struct EntityRef(Rc<RefCell<Entity>>);

impl EntityRef {
    pub(crate) fn new(doc: Option<DocumentRef>, name: Rc<str>) -> Self {
        Self(Rc::new(RefCell::new(Entity {
            first_child: None,
            last_child: None,
            owner_document: doc.map(|doc| doc.downgrade()),
            name,
            public_id: None,
            system_id: None,
            notation_name: None,
            input_encoding: None,
            xml_encoding: None,
            xml_version: None,
        })))
    }

    /// Generate [`EntityWeakRef`] from `self`.
    pub fn downgrade(&self) -> EntityWeakRef {
        EntityWeakRef(Rc::downgrade(&self.0))
    }
}

impl Node for EntityRef {
    fn node_name(&self) -> Rc<str> {
        self.0.borrow().name.clone()
    }

    fn node_value(&self) -> Option<Rc<str>> {
        None
    }

    fn node_type(&self) -> NodeType {
        NodeType::Entity
    }

    fn first_child(&self) -> Option<NodeRef> {
        self.0.borrow().first_child.clone()
    }

    fn last_child(&self) -> Option<NodeRef> {
        self.0.borrow().last_child.clone()
    }

    fn owner_document(&self) -> Option<DocumentRef> {
        self.0
            .borrow()
            .owner_document
            .as_ref()
            .and_then(|doc| doc.upgrade())
    }

    fn clone_node(&self, deep: bool) -> NodeRef {
        let mut entity = EntityRef(Rc::new(RefCell::new(Entity {
            first_child: None,
            last_child: None,
            owner_document: self.0.borrow().owner_document.clone(),
            name: self.0.borrow().name.clone(),
            public_id: self.0.borrow().public_id.clone(),
            system_id: self.0.borrow().system_id.clone(),
            notation_name: self.0.borrow().notation_name.clone(),
            input_encoding: self.0.borrow().input_encoding.clone(),
            xml_encoding: self.0.borrow().xml_encoding.clone(),
            xml_version: self.0.borrow().xml_version.clone(),
        })));

        if deep {
            let mut children = self.first_child();
            while let Some(child) = children {
                children = child.next_sibling();
                entity
                    .append_child(child.clone_node(true))
                    .expect("Internal Error");
            }
        }
        entity.into()
    }

    fn is_same_node(&self, other: &NodeRef) -> bool {
        let NodeRef::Entity(other) = other else {
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
}

impl NodeConnection for EntityRef {
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
        replace(
            &mut self.0.borrow_mut().owner_document,
            Some(new_doc.downgrade()),
        )
        .and_then(|doc| doc.upgrade())
    }

    fn adopted_to(&mut self, _new_doc: DocumentRef) {
        // Entity nodes cannot be adopted.
    }
}

impl From<EntityRef> for NodeRef {
    fn from(value: EntityRef) -> Self {
        NodeRef::Entity(value)
    }
}

/// Wrapper of `Weak<RefCell<Entity>>`.
#[derive(Clone)]
pub struct EntityWeakRef(Weak<RefCell<Entity>>);

impl EntityWeakRef {
    /// Generate [`EntityRef`] from `self`.  
    /// Success conditions are the same as for [`std::rc::Weak::upgrade`].
    pub fn upgrade(&self) -> Option<EntityRef> {
        self.0.upgrade().map(EntityRef)
    }
}

use std::{
    cell::RefCell,
    collections::HashMap,
    mem::replace,
    rc::{Rc, Weak},
    sync::Arc,
};

use crate::error::{__xml_simple_error, XmlErrorDomain, XmlParserErrors};

use super::{
    NodeType,
    document::{Document, DocumentRef},
    node::{Node, NodeConnection, NodeRef},
    user_data::{DOMUserData, OperationType, UserDataHandler},
};

/// Raise an error.
#[doc(alias = "xmlEntitiesErr")]
pub(super) fn xml_entities_err(code: XmlParserErrors, msg: &str) {
    __xml_simple_error!(XmlErrorDomain::XmlFromTree, code, None, msg);
}

/// The different valid entity types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum EntityType {
    #[default]
    InternalGeneralEntity,
    InternalParameterEntity,
    InternalPredefinedEntity,
    ExternalGeneralParsedEntity,
    ExternalGeneralUnparsedEntity,
    ExternalParameterEntity,
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
    owner_document: Weak<RefCell<Document>>,

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

    user_data: Option<HashMap<String, (DOMUserData, Option<Arc<dyn UserDataHandler>>)>>,

    etype: EntityType,
}

/// Wrapper of `Rc<RefCell<Entity>>`.
#[derive(Clone)]
pub struct EntityRef(Rc<RefCell<Entity>>);

impl EntityRef {
    pub(crate) fn new(doc: Option<DocumentRef>, name: Rc<str>, etype: EntityType) -> Self {
        Self(Rc::new(RefCell::new(Entity {
            first_child: None,
            last_child: None,
            owner_document: doc.map(|doc| Rc::downgrade(&doc.0)).unwrap_or_default(),
            name,
            public_id: None,
            system_id: None,
            notation_name: None,
            input_encoding: None,
            xml_encoding: None,
            xml_version: None,
            user_data: None,
            etype,
        })))
    }

    /// Generate [`EntityWeakRef`] from `self`.
    pub fn downgrade(&self) -> EntityWeakRef {
        EntityWeakRef(Rc::downgrade(&self.0))
    }

    /// Get `name` attribute of this entity.
    pub fn name(&self) -> Rc<str> {
        self.0.borrow().name.clone()
    }

    /// Get `publicId` attribute of this entity.
    pub fn public_id(&self) -> Option<Rc<str>> {
        self.0.borrow().public_id.clone()
    }

    /// Get `systemId` attribute of this entity.
    pub fn system_id(&self) -> Option<Rc<str>> {
        self.0.borrow().system_id.clone()
    }

    /// Get `notationName` attribute of this entity.
    pub fn notation_name(&self) -> Option<Rc<str>> {
        self.0.borrow().notation_name.clone()
    }

    pub fn set_public_id(&mut self, public_id: Option<impl Into<Rc<str>>>) -> Option<Rc<str>> {
        replace(
            &mut self.0.borrow_mut().public_id,
            public_id.map(|id| id.into()),
        )
    }

    pub fn set_system_id(&mut self, system_id: Option<impl Into<Rc<str>>>) -> Option<Rc<str>> {
        replace(
            &mut self.0.borrow_mut().system_id,
            system_id.map(|id| id.into()),
        )
    }

    pub fn set_notation_name(
        &mut self,
        notation_name: Option<impl Into<Rc<str>>>,
    ) -> Option<Rc<str>> {
        replace(
            &mut self.0.borrow_mut().notation_name,
            notation_name.map(|name| name.into()),
        )
    }
}

impl Node for EntityRef {
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
        NodeType::Entity
    }

    fn first_child(&self) -> Option<NodeRef> {
        self.0.borrow().first_child.clone()
    }

    fn last_child(&self) -> Option<NodeRef> {
        self.0.borrow().last_child.clone()
    }

    fn owner_document(&self) -> Option<DocumentRef> {
        self.0.borrow().owner_document.upgrade().map(From::from)
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
            user_data: None,
            etype: self.0.borrow().etype,
        })));

        if deep {
            let mut read_only_check = false;
            if let Some(mut doc) = self.owner_document() {
                read_only_check = doc.is_enabled_read_only_check();
                doc.disable_read_only_check();
            }
            let mut children = self.first_child();
            while let Some(child) = children {
                children = child.next_sibling();
                entity
                    .append_child(child.clone_node(true))
                    .expect("Internal Error");
            }
            if read_only_check {
                self.owner_document().unwrap().enable_read_only_check();
            }
        }

        self.handle_user_data(OperationType::NodeCloned, Some(entity.clone().into()));
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

    fn lookup_namespace_uri(&self, _prefix: Option<&str>) -> Option<Rc<str>> {
        None
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
        // DOM Level 3 does not support editing Entity nodes <..snip..>
        // Entity nodes and all their descendants are readonly.
        true
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
            Rc::downgrade(&new_doc.0),
        )
        .upgrade()
        .map(From::from)
    }

    fn adopted_to(&mut self, _new_doc: DocumentRef) {
        // Entity nodes cannot be adopted.
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

use std::{
    cell::RefCell,
    mem::replace,
    rc::{Rc, Weak},
};

use super::{
    NodeType,
    document::{DocumentRef, DocumentWeakRef},
    node::{Node, NodeConnection, NodeRef},
};

/// Implementation of [DocumentFragment](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-B63ED1A3)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
pub struct DocumentFragment {
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
    owner_document: DocumentWeakRef,
}

impl DocumentFragment {
    fn adopted_to(&mut self, new_doc: DocumentRef) {
        self.owner_document = new_doc.downgrade();
        let mut children = self.first_child.clone();
        while let Some(mut child) = children {
            child.adopted_to(new_doc.clone());
            children = child.next_sibling();
        }
    }
}

/// Wrapper of `Rc<RefCell<DocumentFragment>>`.
#[derive(Clone)]
pub struct DocumentFragmentRef(Rc<RefCell<DocumentFragment>>);

impl DocumentFragmentRef {
    /// Generate [`DocumentFragmentWeakRef`] from `self`.
    pub fn downgrade(&self) -> DocumentFragmentWeakRef {
        DocumentFragmentWeakRef(Rc::downgrade(&self.0))
    }

    /// Create new [`DocumentFragmentRef`] whose ownerDocument is `doc`.
    pub(super) fn from_doc(doc: DocumentRef) -> Self {
        Self(Rc::new(RefCell::new(DocumentFragment {
            first_child: None,
            last_child: None,
            owner_document: doc.downgrade(),
        })))
    }
}

impl Node for DocumentFragmentRef {
    fn node_name(&self) -> Rc<str> {
        "#document-fragment".into()
    }

    fn node_value(&self) -> Option<Rc<str>> {
        None
    }

    fn set_node_value(&mut self, _: impl Into<String>) -> Result<(), super::DOMException> {
        Ok(())
    }

    fn node_type(&self) -> NodeType {
        NodeType::DocumentFragment
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

    fn clone_node(&self, deep: bool) -> NodeRef {
        let mut frag = DocumentFragmentRef(Rc::new(RefCell::new(DocumentFragment {
            first_child: None,
            last_child: None,
            owner_document: self.0.borrow().owner_document.clone(),
        })));

        if deep {
            let mut children = self.first_child();
            while let Some(child) = children {
                children = child.next_sibling();
                frag.append_child(child.clone_node(true))
                    .expect("Internal Error");
            }
        }
        frag.into()
    }

    fn is_same_node(&self, other: &NodeRef) -> bool {
        let NodeRef::DocumentFragment(other) = other else {
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

    fn is_read_only(&self) -> bool {
        false
    }
}

impl NodeConnection for DocumentFragmentRef {
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

impl From<DocumentFragmentRef> for NodeRef {
    fn from(value: DocumentFragmentRef) -> Self {
        NodeRef::DocumentFragment(value)
    }
}

/// Wrapper of `Weak<RefCell<DocumentFragment>>`.
#[derive(Clone)]
pub struct DocumentFragmentWeakRef(Weak<RefCell<DocumentFragment>>);

impl DocumentFragmentWeakRef {
    /// Generate [`DocumentFragmentRef`] from `self`.  
    /// Success conditions are the same as for [`std::rc::Weak::upgrade`].
    pub fn upgrade(&self) -> Option<DocumentFragmentRef> {
        self.0.upgrade().map(DocumentFragmentRef)
    }
}

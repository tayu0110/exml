use std::{
    cell::RefCell,
    mem::replace,
    rc::{Rc, Weak},
};

use crate::dom::DOMException;

use super::{
    NodeType,
    document::{DocumentRef, DocumentWeakRef},
    node::{Node, NodeConnection, NodeRef, NodeWeakRef},
};

/// Implementation of [ProcessingInstruction](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1004215813)
/// interface on [1.5 Extended Interfaces: XML Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-E067D597)
pub struct ProcessingInstruction {
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    /// - `Document`
    /// - `DocumentFragment`
    /// - `EntityReference`
    /// - `Element`
    /// - `Entity`
    parent_node: Option<NodeWeakRef>,
    // /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    // /// - no children
    // first_child: Option<NodeRef>,
    // last_child: Option<NodeRef>,
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    /// - `DocumentType`
    /// - `EntityReference`
    /// - `Element`
    /// - `ProcessingInstruction`
    /// - `Comment`
    /// - `Text`
    /// - `CDATASection`
    previous_sibling: Option<NodeWeakRef>,
    next_sibling: Option<NodeRef>,
    owner_document: DocumentWeakRef,
    /// Implementation of `target` attribute for `ProcessingInstruction`.
    /// as same as `nodeName` for `Node`.
    target: Rc<str>,
    /// Implementation of `data` attribute for `ProcessingInstruction`.
    data: Option<Rc<str>>,
}

/// Wrapper of `Rc<RefCell<ProcessingInstruction>>`.
#[derive(Clone)]
pub struct ProcessingInstructionRef(Rc<RefCell<ProcessingInstruction>>);

impl ProcessingInstructionRef {
    /// Generate [`ProcessingInstructionWeakRef`] from `self`.
    pub fn downgrade(&self) -> ProcessingInstructionWeakRef {
        ProcessingInstructionWeakRef(Rc::downgrade(&self.0))
    }

    /// Create new [`ProcessingInstructionRef`] whose ownerDocument is `doc`.
    ///
    /// This method does not validate `target`.
    pub(super) fn from_doc(doc: DocumentRef, target: Rc<str>, data: Option<Rc<str>>) -> Self {
        Self(Rc::new(RefCell::new(ProcessingInstruction {
            parent_node: None,
            previous_sibling: None,
            next_sibling: None,
            owner_document: doc.downgrade(),
            target,
            data,
        })))
    }

    /// Implementation of `data` attribute.
    ///
    /// # Specification
    /// ```text
    /// target of type DOMString, readonly
    ///     The target of this processing instruction. XML defines this as being the first
    ///     token following the markup that begins the processing instruction.
    /// ```
    pub fn target(&self) -> Rc<str> {
        self.0.borrow().target.clone()
    }

    /// Implementation of `data` attribute.
    ///
    /// # Specification
    /// ```text
    /// data of type DOMString
    ///     The content of this processing instruction. This is from the first non white
    ///     space character after the target to the character immediately preceding the ?>.
    pub fn data(&self) -> Option<Rc<str>> {
        self.0.borrow().data.clone()
    }

    /// Implementation of `data` attribute.
    ///
    /// # Specification
    /// ```text
    /// The content of this processing instruction. This is from the first non white space
    /// character after the target to the character immediately preceding the ?>.
    ///
    /// Exceptions on setting
    ///     DOMException
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
    /// ```
    pub fn set_data(&mut self, value: impl Into<Rc<str>>) -> Result<(), DOMException> {
        self.0.borrow_mut().data = Some(value.into());
        Ok(())
    }
}

impl Node for ProcessingInstructionRef {
    fn node_name(&self) -> Rc<str> {
        self.0.borrow().target.clone()
    }

    fn node_value(&self) -> Option<Rc<str>> {
        self.0.borrow().data.clone()
    }

    fn set_node_value(&mut self, value: impl Into<String>) -> Result<(), DOMException> {
        self.set_data(value.into())?;
        Ok(())
    }

    fn node_type(&self) -> NodeType {
        NodeType::ProcessingInstruction
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
        self.0.borrow().owner_document.upgrade()
    }

    fn clone_node(&self, _deep: bool) -> NodeRef {
        let pi = ProcessingInstruction {
            parent_node: None,
            previous_sibling: None,
            next_sibling: None,
            owner_document: self.0.borrow().owner_document.clone(),
            target: self.0.borrow().target.clone(),
            data: self.0.borrow().data.clone(),
        };

        ProcessingInstructionRef(Rc::new(RefCell::new(pi))).into()
    }

    fn text_content(&self) -> Option<String> {
        self.node_value().map(|value| value.to_string())
    }

    fn set_text_content(&mut self, text: impl Into<String>) -> Result<(), super::DOMException> {
        let text: String = text.into();
        self.0.borrow_mut().data = Some(text.into());
        Ok(())
    }

    fn is_same_node(&self, other: &NodeRef) -> bool {
        let NodeRef::ProcessingInstruction(other) = other else {
            return false;
        };
        Rc::ptr_eq(&self.0, &other.0)
    }
}

impl NodeConnection for ProcessingInstructionRef {
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
        replace(&mut self.0.borrow_mut().owner_document, new_doc.downgrade()).upgrade()
    }

    fn adopted_to(&mut self, new_doc: DocumentRef) {
        self.0.borrow_mut().owner_document = new_doc.downgrade();
    }
}

impl From<ProcessingInstructionRef> for NodeRef {
    fn from(value: ProcessingInstructionRef) -> Self {
        NodeRef::ProcessingInstruction(value)
    }
}

/// Wrapper of `Weak<RefCell<ProcessingInstruction>>`.
#[derive(Clone)]
pub struct ProcessingInstructionWeakRef(Weak<RefCell<ProcessingInstruction>>);

impl ProcessingInstructionWeakRef {
    /// Generate [`ProcessingInstructionRef`] from `self`.  
    /// Success conditions are the same as for [`std::rc::Weak::upgrade`].
    pub fn upgrade(&self) -> Option<ProcessingInstructionRef> {
        self.0.upgrade().map(ProcessingInstructionRef)
    }
}

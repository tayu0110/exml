//! Implement [Document Object Model (DOM) Level 3 Core](https://www.w3.org/TR/DOM-Level-3-Core/).
//!
//! I will try to maintain compatibility with the libxml2 tree as much as possible.  
//!
//! Currently, full implementation of the specification is not a goal.  
//! For example, I do not plan to provide a feature selection mechanism.
//!
//! # Note
//! - The iterators that walk through the nodes of the DOM are not implemented.\
//!   Nodes are not restricted to only one source, and each node can modify the whole DOM tree.
//!   Therefore, it is impossible to implement iterators that is not invalidated.

use node::{Node, NodeRef};

pub mod attlistdecl;
pub mod attr;
pub mod character_data;
pub mod document;
pub mod document_fragment;
pub mod document_type;
pub mod dom_implementation;
pub mod element;
pub mod elementdecl;
pub mod entity;
pub mod entity_reference;
pub mod name_list;
pub mod named_node_map;
pub mod node;
pub mod node_list;
pub mod notation;
pub mod pi;
pub mod user_data;

/// This is the namespace for the special xml: prefix predefined in the
/// XML Namespace specification.
pub const XML_XML_NAMESPACE: &str = "http://www.w3.org/XML/1998/namespace";
pub const XML_NS_NAMESPACE: &str = "http://www.w3.org/2000/xmlns/";

/// Implementation of [DOMException](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-17189187)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
///
/// Although named “Exception”,
/// it merely inherits its name from the specification and is in fact just an error code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DOMException {
    /// If index or size is negative, or greater than the allowed value.
    IndexSizeErr = 1,
    /// If the specified range of text does not fit into a DOMString.
    DOMStringSizeErr = 2,
    /// If any Node is inserted somewhere it doesn't belong.
    HierarchyRequestErr = 3,
    /// If a Node is used in a different document than the one that created it
    /// (that doesn't support it).
    WrongDocumentErr = 4,
    /// If an invalid or illegal character is specified, such as in an XML name.
    InvalidCharacterErr = 5,
    /// If data is specified for a Node which does not support data.
    NoDataAllowedErr = 6,
    /// If an attempt is made to modify an object where modifications are not allowed.
    NoModificationAllowedErr = 7,
    /// If an attempt is made to reference a Node in a context where it does not exist.
    NotFoundErr = 8,
    /// If the implementation does not support the requested type of object or operation.
    NotSupportedErr = 9,
    /// If an attempt is made to add an attribute that is already in use elsewhere.
    InuseAttributeErr = 10,
    /// If an attempt is made to use an object that is not, or is no longer, usable.
    InvalidStateErr = 11,
    /// If an invalid or illegal string is specified.
    SyntaxErr = 12,
    /// If an attempt is made to modify the type of the underlying object.
    InvalidModificationErr = 13,
    /// If an attempt is made to create or change an object in a way which is incorrect with
    /// regard to namespaces.
    NamespaceErr = 14,
    /// If a parameter or an operation is not supported by the underlying object.
    InvalidAccessErr = 15,
    /// If a call to a method such as insertBefore or removeChild would make the Node invalid
    /// with respect to "partial validity", this exception would be raised and the operation
    /// would not be done. This code is used in [DOM Level 3 Validation]. Refer to this
    /// specification for further information.
    ValidationErr = 16,
    /// If the type of an object is incompatible with the expected type of the parameter
    /// associated to the object.
    TypeMismatchErr = 17,
}

/// The two nodes are disconnected.\
/// Order between disconnected nodes is always implementation-specific.
const DOCUMENT_POSITION_DISCONNECTED: u16 = 0x01;
/// The second node precedes the reference node.
const DOCUMENT_POSITION_PRECEDING: u16 = 0x02;
/// The node follows the reference node.
const DOCUMENT_POSITION_FOLLOWING: u16 = 0x04;
/// The node contains the reference node.\
/// A node which contains is always preceding, too.
const DOCUMENT_POSITION_CONTAINS: u16 = 0x08;
/// The node is contained by the reference node.\
/// A node which is contained is always following, too.
const DOCUMENT_POSITION_CONTAINED_BY: u16 = 0x10;
/// The determination of preceding versus following is implementation-specific.
const DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC: u16 = 0x20;

/// Constants `DocumentPosition` in [Interface Node](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1950641247).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DocumentPosition(u16);

impl DocumentPosition {
    fn new() -> Self {
        DocumentPosition(0)
    }

    fn set_flag(mut self, flag: u16) -> Self {
        self.0 |= flag;
        self
    }

    fn unset_flag(mut self, flag: u16) -> Self {
        self.0 &= !flag;
        self
    }

    pub fn is_same_node(self) -> bool {
        self.0 == 0
    }

    pub fn is_disconnected(self) -> bool {
        self.0 & DOCUMENT_POSITION_DISCONNECTED != 0
    }

    pub fn is_preceding(self) -> bool {
        self.0 & DOCUMENT_POSITION_PRECEDING != 0
    }

    pub fn is_following(self) -> bool {
        self.0 & DOCUMENT_POSITION_FOLLOWING != 0
    }

    pub fn is_contains(self) -> bool {
        self.0 & DOCUMENT_POSITION_CONTAINS != 0
    }

    pub fn is_contained_by(self) -> bool {
        self.0 & DOCUMENT_POSITION_CONTAINED_BY != 0
    }

    pub fn is_implementation_specific(self) -> bool {
        self.0 & DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC != 0
    }
}

impl From<DocumentPosition> for u16 {
    fn from(value: DocumentPosition) -> Self {
        value.0
    }
}

/// Constants `NodeType` in [Interface Node](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1950641247).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NodeType {
    Element = 1,
    Attribute = 2,
    Text = 3,
    CDATASection = 4,
    EntityReference = 5,
    Entity = 6,
    ProcessingInstruction = 7,
    Comment = 8,
    Document = 9,
    DocumentType = 10,
    DocumentFragment = 11,
    Notation = 12,
}

/// Return `true` if `left` and `right` are allowed to be siblings.  
/// Otherwise, return `false`.
///
/// The following table is derived from the vertical constraints
/// and is not directly described in the specification.
///
/// | NodeType              | Description                                                                                   |
/// | :-------------------- | :-------------------------------------------------------------------------------------------- |
/// | Document              | no siblings                                                                                   |
/// | DocumentFragment      | no siblings                                                                                   |
/// | DocumentType          | Element, ProcessingInstruction, Comment                                                       |
/// | EntityReference       | Element, Text, Comment, ProcessingInstruction, CDATASection, EntityReference                  |
/// | Element               | DocumentType, Element, Text, Comment, ProcessingInstruction, CDATASection, EntityReference    |
/// | Attr                  | no siblings                                                                                   |
/// | ProcessingInstruction | DocumentType, Element, Text, Comment, ProcessingInstruction, CDATASection, EntityReference    |
/// | Comment               | DocumentType, Element, Text, Comment, ProcessingInstruction, CDATASection, EntityReference    |
/// | Text                  | Element, Text, Comment, ProcessingInstruction, CDATASection, EntityReference                  |
/// | CDATASection          | Element, Text, Comment, ProcessingInstruction, CDATASection, EntityReference                  |
/// | Entity                | no siblings                                                                                   |
/// | Notation              | no siblings                                                                                   |
fn check_horizontal_hierarchy(left: NodeType, right: NodeType) -> bool {
    use NodeType::*;
    match left {
        Element | ProcessingInstruction | Comment => matches!(
            right,
            DocumentType
                | Element
                | Text
                | Comment
                | ProcessingInstruction
                | CDATASection
                | EntityReference
        ),
        Text | CDATASection | EntityReference => matches!(
            right,
            Element | Text | Comment | ProcessingInstruction | CDATASection | EntityReference
        ),
        DocumentType => matches!(right, Element | ProcessingInstruction | Comment),
        _ => false,
    }
}

/// Return `true` if `parent` and `child` are allowed to be parent and child.  
/// Otherwise, return `false`.
///
/// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
///
/// | NodeType              | Description                                                                               |
/// | :-------------------- | :---------------------------------------------------------------------------------------- |
/// | Document              | Element (maximum of one), ProcessingInstruction, Comment, DocumentType (maximum of one)   |
/// | DocumentFragment      | Element, ProcessingInstruction, Comment, Text, CDATASection, EntityReference              |
/// | DocumentType          | no children                                                                               |
/// | EntityReference       | Element, ProcessingInstruction, Comment, Text, CDATASection, EntityReference              |
/// | Element               | Element, ProcessingInstruction, Comment, Text, CDATASection, EntityReference              |
/// | Attr                  | Text, EntityReference                                                                     |
/// | ProcessingInstruction | no children                                                                               |
/// | Comment               | no children                                                                               |
/// | Text                  | no children                                                                               |
/// | CDATASection          | no children                                                                               |
/// | Entity                | Element, ProcessingInstruction, Comment, Text, CDATASection, EntityReference              |
/// | Notation              | no children                                                                               |
fn check_vertical_hierarchy(parent: NodeType, child: NodeType) -> bool {
    use NodeType::*;
    match parent {
        Element | DocumentFragment | EntityReference | Entity => matches!(
            child,
            Element | Text | Comment | ProcessingInstruction | CDATASection | EntityReference
        ),
        Attribute => matches!(child, Text | EntityReference),
        Document => matches!(
            child,
            Element | ProcessingInstruction | Comment | DocumentType
        ),
        _ => false,
    }
}

/// Check if the nodes belong to the same document or not.
fn check_owner_document_sameness(l: &impl Node, r: &impl Node) -> bool {
    match (l.node_type(), r.node_type()) {
        (NodeType::DocumentType, NodeType::DocumentType) => {
            let ldoc = l.owner_document();
            let rdoc = r.owner_document();
            ldoc.is_some() == rdoc.is_some()
                && ldoc
                    .zip(rdoc)
                    .is_none_or(|(l, r)| l.is_same_node(&r.into()))
        }
        (NodeType::DocumentType, _) if l.owner_document().is_none() => true,
        (_, NodeType::DocumentType) if r.owner_document().is_none() => true,
        (NodeType::Document, NodeType::Document) => l.is_same_node(&r.clone().into()),
        (NodeType::Document, _) => r
            .owner_document()
            .is_some_and(|doc| l.is_same_node(&NodeRef::Document(doc))),
        (_, NodeType::Document) => l
            .owner_document()
            .is_some_and(|doc| r.is_same_node(&NodeRef::Document(doc))),
        _ => l
            .owner_document()
            .zip(r.owner_document())
            .is_some_and(|(l, r)| l.is_same_node(&node::NodeRef::Document(r))),
    }
}

fn check_no_modification_allowed_err(node: &impl Node) -> Result<(), DOMException> {
    if node.is_read_only()
        && node
            .owner_document()
            .is_some_and(|doc| doc.is_enabled_read_only_check())
    {
        Err(DOMException::NoModificationAllowedErr)
    } else {
        Ok(())
    }
}

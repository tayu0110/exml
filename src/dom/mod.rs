//! Implement [Document Object Model (DOM) Level 3 Core](https://www.w3.org/TR/DOM-Level-3-Core/).
//!
//! I will try to maintain compatibility with the libxml2 tree as much as possible.  
//!
//! Currently, full implementation of the specification is not a goal.  
//! For example, I do not plan to provide components equivalent to the `DOMImplementation` interface
//! and a feature selection mechanism.
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
pub mod element;
pub mod elementdecl;
pub mod entity;
pub mod entity_reference;
pub mod name_list;
pub mod named_node_map;
pub mod node;
pub mod notation;
pub mod pi;

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
    IndexSizeErr = 1,
    DOMStringSizeErr = 2,
    HierarchyRequestErr = 3,
    WrongDocumentErr = 4,
    InvalidCharacterErr = 5,
    NoDataAllowedErr = 6,
    NoModificationAllowedErr = 7,
    NotFoundErr = 8,
    NotSupportedErr = 9,
    InuseAttributeErr = 10,
    InvalidStateErr = 11,
    SyntaxErr = 12,
    InvalidModificationErr = 13,
    NamespaceErr = 14,
    InvalidAccessErr = 15,
    ValidationErr = 16,
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

#[cfg(test)]
mod dom_test_suite {
    use crate::dom::{
        DOMException,
        attlistdecl::{AttType, DefaultDecl},
        document::DocumentRef,
        document_type::DocumentTypeRef,
        elementdecl::{ContentSpec, ElementContent, ElementContentOccur},
        entity::EntityType,
        node::Node,
    };

    const STAFF_XML: &str =
        include_str!("../../resources/DOM-Test-Suite/tests/level1/core/files/staff.xml");

    fn staff_xml(_doc: &str) -> Result<DocumentRef, DOMException> {
        let doctype = DocumentTypeRef::new("staff", None, Some("staff.dtd")).unwrap();
        let mut doc = DocumentRef::new(None, Some("staff"), Some(doctype)).unwrap();

        let mut doctype = doc.doctype().unwrap();
        let mut ent = doctype
            .create_entity("ent1", EntityType::InternalGeneralEntity)
            .unwrap();
        ent.append_child(doc.create_text_node("es").into()).unwrap();
        doctype.add_entity::<false>(ent).unwrap();
        let mut ent = doctype
            .create_entity("ent2", EntityType::InternalGeneralEntity)
            .unwrap();
        ent.append_child(doc.create_text_node("1900 Dallas Road").into())
            .unwrap();
        doctype.add_entity::<false>(ent).unwrap();
        let mut ent = doctype
            .create_entity("ent3", EntityType::InternalGeneralEntity)
            .unwrap();
        ent.append_child(doc.create_text_node("Texas").into())
            .unwrap();
        doctype.add_entity::<false>(ent).unwrap();
        let mut ent = doctype
            .create_entity("ent4", EntityType::InternalGeneralEntity)
            .unwrap();
        let mut ent_element = doc.create_element("entElement").unwrap();
        ent_element.set_attribute("domestic", "Yes").unwrap();
        ent_element
            .append_child(doc.create_text_node("Element data").into())
            .unwrap();
        ent.append_child(ent_element.into()).unwrap();
        ent.append_child(
            doc.create_processing_instruction("PItarget", Some("PIdata"))
                .unwrap()
                .into(),
        )
        .unwrap();
        doctype.add_entity::<false>(ent).unwrap();
        let mut ent = doctype
            .create_entity("ent5", EntityType::ExternalGeneralUnparsedEntity)
            .unwrap();
        ent.set_public_id(Some("entityURI"));
        ent.set_system_id(Some("entityFile"));
        ent.set_notation_name(Some("notation1"));
        doctype.add_entity::<false>(ent).unwrap();
        assert!(
            doctype
                .add_entity::<false>(
                    doctype
                        .create_entity("ent1", EntityType::InternalGeneralEntity)
                        .unwrap()
                )
                .is_err()
        );
        doctype
            .add_notation::<false>(doctype.create_notation("notation1").unwrap())
            .unwrap();
        doctype
            .add_notation::<false>(doctype.create_notation("notation2").unwrap())
            .unwrap();

        // TODO: add external subset
        doctype
            .add_element_decl::<true>(
                doctype
                    .create_element_decl(
                        "employeeId",
                        ContentSpec::Mixed(ElementContent::new_pcdata(ElementContentOccur::Once)),
                    )
                    .unwrap(),
            )
            .unwrap();
        doctype
            .add_element_decl::<true>(
                doctype
                    .create_element_decl(
                        "name",
                        ContentSpec::Mixed(ElementContent::new_pcdata(ElementContentOccur::Once)),
                    )
                    .unwrap(),
            )
            .unwrap();
        doctype
            .add_element_decl::<true>(
                doctype
                    .create_element_decl(
                        "position",
                        ContentSpec::Mixed(ElementContent::new_pcdata(ElementContentOccur::Once)),
                    )
                    .unwrap(),
            )
            .unwrap();
        doctype
            .add_element_decl::<true>(
                doctype
                    .create_element_decl(
                        "salary",
                        ContentSpec::Mixed(ElementContent::new_pcdata(ElementContentOccur::Once)),
                    )
                    .unwrap(),
            )
            .unwrap();
        doctype
            .add_element_decl::<true>(
                doctype
                    .create_element_decl(
                        "address",
                        ContentSpec::Mixed(ElementContent::new_pcdata(ElementContentOccur::Once)),
                    )
                    .unwrap(),
            )
            .unwrap();
        doctype
            .add_element_decl::<true>(
                doctype
                    .create_element_decl(
                        "entElement",
                        ContentSpec::Mixed(ElementContent::new_pcdata(ElementContentOccur::Once)),
                    )
                    .unwrap(),
            )
            .unwrap();
        let or = ElementContent::new_or(ElementContentOccur::Mult);
        or.set_first_child(ElementContent::new_pcdata(ElementContentOccur::Once));
        or.set_second_child(ElementContent::new_element(
            ElementContentOccur::Once,
            "entElement",
        ));
        doctype
            .add_element_decl::<true>(
                doctype
                    .create_element_decl("gender", ContentSpec::Mixed(or))
                    .unwrap(),
            )
            .unwrap();
        let seq1 = ElementContent::new_seq(ElementContentOccur::Once);
        seq1.set_first_child(ElementContent::new_element(
            ElementContentOccur::Once,
            "employeeId",
        ));
        let seq2 = ElementContent::new_seq(ElementContentOccur::Once);
        seq2.set_first_child(ElementContent::new_element(
            ElementContentOccur::Once,
            "name",
        ));
        let seq3 = ElementContent::new_seq(ElementContentOccur::Once);
        seq3.set_first_child(ElementContent::new_element(
            ElementContentOccur::Once,
            "position",
        ));
        let seq4 = ElementContent::new_seq(ElementContentOccur::Once);
        seq4.set_first_child(ElementContent::new_element(
            ElementContentOccur::Once,
            "salary",
        ));
        let seq5 = ElementContent::new_seq(ElementContentOccur::Once);
        seq5.set_first_child(ElementContent::new_element(
            ElementContentOccur::Once,
            "gender",
        ));
        seq5.set_second_child(ElementContent::new_element(
            ElementContentOccur::Once,
            "address",
        ));
        seq4.set_second_child(seq5);
        seq3.set_second_child(seq4);
        seq2.set_second_child(seq3);
        seq1.set_second_child(seq2);
        doctype
            .add_element_decl::<true>(
                doctype
                    .create_element_decl("employee", ContentSpec::Children(seq1))
                    .unwrap(),
            )
            .unwrap();
        doctype
            .add_element_decl::<true>(
                doctype
                    .create_element_decl(
                        "staff",
                        ContentSpec::Children(ElementContent::new_element(
                            ElementContentOccur::Plus,
                            "employee",
                        )),
                    )
                    .unwrap(),
            )
            .unwrap();
        doctype
            .add_attlist_decl::<true>(
                doctype
                    .create_attlist_decl(
                        "entElement",
                        "attr1",
                        AttType::CDATA,
                        DefaultDecl::None("Attr".into()),
                    )
                    .unwrap(),
            )
            .unwrap();
        doctype
            .add_attlist_decl::<true>(
                doctype
                    .create_attlist_decl(
                        "address",
                        "domestic",
                        AttType::CDATA,
                        DefaultDecl::Implied,
                    )
                    .unwrap(),
            )
            .unwrap();
        doctype
            .add_attlist_decl::<true>(
                doctype
                    .create_attlist_decl(
                        "address",
                        "street",
                        AttType::CDATA,
                        DefaultDecl::None("Yes".into()),
                    )
                    .unwrap(),
            )
            .unwrap();
        doctype
            .add_attlist_decl::<true>(
                doctype
                    .create_attlist_decl(
                        "entElement",
                        "domestic",
                        AttType::CDATA,
                        DefaultDecl::None("MALE".into()),
                    )
                    .unwrap(),
            )
            .unwrap();

        let mut root = doc.document_element().unwrap();
        assert!(root.parent_node().is_some());
        let comment = doc.create_comment(" This is comment number 1.").into();
        doc.insert_before(comment, Some(root.clone().into()))
            .unwrap();

        let mut employee = root
            .append_child(doc.create_element("employee").unwrap().into())
            .unwrap();
        let mut employee_id = employee
            .append_child(doc.create_element("employeeId").unwrap().into())
            .unwrap();
        employee_id
            .append_child(doc.create_text_node("EMP0001").into())
            .unwrap();
        let mut name = employee
            .append_child(doc.create_element("name").unwrap().into())
            .unwrap();
        name.append_child(doc.create_text_node("Margaret Martin").into())
            .unwrap();
        let mut position = employee
            .append_child(doc.create_element("position").unwrap().into())
            .unwrap();
        position
            .append_child(doc.create_text_node("Accountant").into())
            .unwrap();
        let mut salary = employee
            .append_child(doc.create_element("salary").unwrap().into())
            .unwrap();
        salary
            .append_child(doc.create_text_node("56,000").into())
            .unwrap();
        let mut gender = employee
            .append_child(doc.create_element("gender").unwrap().into())
            .unwrap();
        gender
            .append_child(doc.create_text_node("Female").into())
            .unwrap();
        let mut address = employee
            .append_child(doc.create_element("address").unwrap().into())
            .unwrap()
            .as_element()
            .unwrap();
        address.append_child(
            doc.create_text_node("1230 North Ave. Dallas, Texas 98551")
                .into(),
        )?;
        address.set_attribute("domestic", "Yes").unwrap();

        let mut employee = root
            .append_child(doc.create_element("employee").unwrap().into())
            .unwrap();
        let mut employee_id = employee
            .append_child(doc.create_element("employeeId").unwrap().into())
            .unwrap();
        employee_id
            .append_child(doc.create_text_node("EMP0002").into())
            .unwrap();
        let mut name = employee
            .append_child(doc.create_element("name").unwrap().into())
            .unwrap();
        name.append_child(doc.create_text_node("Martha Raynolds").into())
            .unwrap();
        name.append_child(
            doc.create_cdata_section("This is a CDATASection with EntityReference number 2 &ent2;")
                .unwrap()
                .into(),
        )?;
        name.append_child(
            doc.create_cdata_section(
                "This is an adjacent CDATASection with a reference to a tab &tab;",
            )
            .unwrap()
            .into(),
        )?;
        let mut position = employee
            .append_child(doc.create_element("position").unwrap().into())
            .unwrap();
        position
            .append_child(doc.create_text_node("Secretary").into())
            .unwrap();
        let mut salary = employee
            .append_child(doc.create_element("salary").unwrap().into())
            .unwrap();
        salary
            .append_child(doc.create_text_node("35,000").into())
            .unwrap();
        let mut gender = employee
            .append_child(doc.create_element("gender").unwrap().into())
            .unwrap();
        gender
            .append_child(doc.create_text_node("Female").into())
            .unwrap();
        let mut address = employee
            .append_child(doc.create_element("address").unwrap().into())
            .unwrap()
            .as_element()
            .unwrap();
        address
            .append_child(doc.create_entity_reference("ent2").unwrap().into())
            .unwrap();
        address
            .append_child(doc.create_text_node(" Dallas, ").into())
            .unwrap();
        address
            .append_child(doc.create_entity_reference("ent3").unwrap().into())
            .unwrap();
        address
            .append_child(doc.create_text_node("\n 98554").into())
            .unwrap();
        address.set_attribute("domestic", "Yes").unwrap();
        address.set_attribute("street", "Yes").unwrap();

        let mut employee = root
            .append_child(doc.create_element("employee").unwrap().into())
            .unwrap();
        let mut employee_id = employee
            .append_child(doc.create_element("employeeId").unwrap().into())
            .unwrap();
        employee_id
            .append_child(doc.create_text_node("EMP0003").into())
            .unwrap();
        let mut name = employee
            .append_child(doc.create_element("name").unwrap().into())
            .unwrap();
        name.append_child(doc.create_text_node("Roger\n Jones").into())
            .unwrap();
        let mut position = employee
            .append_child(doc.create_element("position").unwrap().into())
            .unwrap();
        position
            .append_child(doc.create_text_node("Department Manager").into())
            .unwrap();
        let mut salary = employee
            .append_child(doc.create_element("salary").unwrap().into())
            .unwrap();
        salary
            .append_child(doc.create_text_node("100,000").into())
            .unwrap();
        let mut gender = employee
            .append_child(doc.create_element("gender").unwrap().into())
            .unwrap();
        gender
            .append_child(doc.create_entity_reference("ent4").unwrap().into())
            .unwrap();
        let mut address = employee
            .append_child(doc.create_element("address").unwrap().into())
            .unwrap()
            .as_element()
            .unwrap();
        address
            .append_child(doc.create_text_node("PO Box 27 Irving, texas 98553").into())
            .unwrap();
        address.set_attribute("domestic", "Yes").unwrap();
        address.set_attribute("street", "No").unwrap();

        let mut employee = root
            .append_child(doc.create_element("employee").unwrap().into())
            .unwrap();
        let mut employee_id = employee
            .append_child(doc.create_element("employeeId").unwrap().into())
            .unwrap();
        employee_id
            .append_child(doc.create_text_node("EMP0004").into())
            .unwrap();
        let mut name = employee
            .append_child(doc.create_element("name").unwrap().into())
            .unwrap();
        name.append_child(doc.create_text_node("Jeny Oconnor").into())
            .unwrap();
        let mut position = employee
            .append_child(doc.create_element("position").unwrap().into())
            .unwrap();
        position
            .append_child(doc.create_text_node("Personnel Director").into())
            .unwrap();
        let mut salary = employee
            .append_child(doc.create_element("salary").unwrap().into())
            .unwrap();
        salary
            .append_child(doc.create_text_node("95,000").into())
            .unwrap();
        let mut gender = employee
            .append_child(doc.create_element("gender").unwrap().into())
            .unwrap();
        gender
            .append_child(doc.create_text_node("Female").into())
            .unwrap();
        let mut address = employee
            .append_child(doc.create_element("address").unwrap().into())
            .unwrap()
            .as_element()
            .unwrap();
        address
            .append_child(doc.create_text_node("PO Box 27 Irving, texas 98553").into())
            .unwrap();
        address.set_attribute("domestic", "Yes")?;
        address.set_attribute("street", "Y")?;
        let mut street = address.get_attribute_node("street").unwrap();
        street
            .append_child(doc.create_entity_reference("ent1").unwrap().into())
            .unwrap();

        let mut employee = root
            .append_child(doc.create_element("employee").unwrap().into())
            .unwrap();
        let mut employee_id = employee
            .append_child(doc.create_element("employeeId").unwrap().into())
            .unwrap();
        employee_id
            .append_child(doc.create_text_node("EMP0005").into())
            .unwrap();
        let mut name = employee
            .append_child(doc.create_element("name").unwrap().into())
            .unwrap();
        name.append_child(doc.create_text_node("Robert Myers").into())
            .unwrap();
        let mut position = employee
            .append_child(doc.create_element("position").unwrap().into())
            .unwrap();
        position
            .append_child(doc.create_text_node("Computer Specialist").into())
            .unwrap();
        let mut salary = employee
            .append_child(doc.create_element("salary").unwrap().into())
            .unwrap();
        salary
            .append_child(doc.create_text_node("90,000").into())
            .unwrap();
        let mut gender = employee
            .append_child(doc.create_element("gender").unwrap().into())
            .unwrap();
        gender
            .append_child(doc.create_text_node("male").into())
            .unwrap();
        let mut address = employee
            .append_child(doc.create_element("address").unwrap().into())
            .unwrap()
            .as_element()
            .unwrap();
        address.append_child(
            doc.create_text_node("1821 Nordic. Road, Irving Texas 98558")
                .into(),
        )?;
        address.set_attribute("street", "Yyes").unwrap();

        Ok(doc)
    }

    mod level1 {
        mod core {
            use crate::dom::{
                dom_test_suite::{STAFF_XML, staff_xml},
                node::Node,
            };

            // ./resources/DOM-Test-Suite/tests/level1/core/attrcreatedocumentfragment.xml
            #[test]
            fn test_attrcreatedocumentfragment() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let mut doc_fragment = doc.create_document_fragment();
                let mut new_one = doc.create_element("newElement").unwrap();
                new_one.set_attribute("newdomestic", "Yes").unwrap();
                doc_fragment.append_child(new_one.into()).unwrap();
                let domestic_node = doc_fragment.first_child().unwrap();
                let domestic_attr = domestic_node.attributes().unwrap();
                let attrs = domestic_attr.item(0).unwrap();
                let attr_name = attrs.name();
                assert_eq!(attr_name, "newdomestic".into());
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrcreatetextnode2.xml
            #[test]
            fn test_attrcreatetextnode2() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let address_list = doc.get_elements_by_tag_name("address");
                let test_node = address_list[3].clone();
                let attributes = test_node.attributes();
                let mut street_attr = attributes.get_named_item("street".into()).unwrap();
                street_attr.set_node_value("Y&ent1;").unwrap();
                let value = street_attr.get_value();
                assert_eq!(value, "Y&ent1;");
                let value = street_attr.node_value().unwrap();
                assert_eq!(value, "Y&ent1;".into());
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrcreatetextnode.xml
            #[test]
            fn test_attrcreatetextnode() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let address_list = doc.get_elements_by_tag_name("address");
                let test_node = address_list[3].clone();
                let attributes = test_node.attributes();
                let mut street_attr = attributes.get_named_item("street".into()).unwrap();
                street_attr.set_value("Y&ent1;").unwrap();
                let value = street_attr.get_value();
                assert_eq!(value, "Y&ent1;");
                let value = street_attr.node_value().unwrap();
                assert_eq!(value, "Y&ent1;".into());
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrdefaultvalue.xml
            #[test]
            fn test_attrdefaultvalue() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let address_list = doc.get_elements_by_tag_name("address");
                let test_node = address_list[0].clone();
                let attributes = test_node.attributes();
                let stree_attr = attributes.get_named_item("street".into()).unwrap();
                let value = stree_attr.node_value().unwrap();
                assert_eq!(value, "Yes".into());
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attreffectivevalue.xml
            #[test]
            fn test_attreffectivevalue() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let address_list = doc.get_elements_by_tag_name("address");
                let test_node = address_list[0].clone();
                let attributes = test_node.attributes();
                let domestic_attr = attributes.get_named_item("domestic".into()).unwrap();
                let value = domestic_attr.node_value().unwrap();
                assert_eq!(value, "Yes".into());
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrentityreplacement.xml
            #[test]
            fn test_attrentityreplacement() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let address_list = doc.get_elements_by_tag_name("address");
                let test_node = address_list[3].clone();
                let attributes = test_node.attributes();
                let street_attr = attributes.get_named_item("street".into()).unwrap();
                let value = street_attr.get_value();
                assert_eq!(value, "Yes");
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrname.xml
            #[test]
            fn test_attrname() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let address_list = doc.get_elements_by_tag_name("address");
                let test_node = address_list[1].clone();
                let attributes = test_node.attributes();
                let street_attr = attributes.get_named_item("street".into()).unwrap();
                let name = street_attr.node_name();
                assert_eq!(name.as_ref(), "street");
                let name = street_attr.name();
                assert_eq!(name.as_ref(), "street");
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrnextsiblingnull.xml
            #[test]
            fn test_attrnextsiblingnull() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let address_list = doc.get_elements_by_tag_name("address");
                let test_node = address_list[0].clone();
                let attributes = test_node.attributes();
                let domestic_attr = attributes.get_named_item("domestic".into()).unwrap();
                let s = domestic_attr.next_sibling();
                assert!(s.is_none());
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrnotspecifiedvalue.xml
            #[test]
            fn test_attrnotspecifiedvalue() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let address_list = doc.get_elements_by_tag_name("address");
                let test_node = address_list[0].clone();
                let attributes = test_node.attributes();
                let street_attr = attributes.get_named_item("street".into()).unwrap();
                let state = street_attr.specified();
                assert!(!state);
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrparentnodenull.xml
            #[test]
            fn test_attrparentnodenull() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let address_list = doc.get_elements_by_tag_name("address");
                let test_node = address_list[0].clone();
                let attributes = test_node.attributes();
                let domestic_attr = attributes.get_named_item("domestic".into()).unwrap();
                let s = domestic_attr.parent_node();
                assert!(s.is_none());
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrprevioussiblingnull.xml
            #[test]
            fn test_attrprevioussiblingnull() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let address_list = doc.get_elements_by_tag_name("address");
                let test_node = address_list[0].clone();
                let attributes = test_node.attributes();
                let domestic_attr = attributes.get_named_item("domestic".into()).unwrap();
                let s = domestic_attr.previous_sibling();
                assert!(s.is_none());
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrremovechild1.xml
            #[test]
            fn test_attrremovechild1() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let ent_ref = doc.create_entity_reference("ent4").unwrap();
                let ent_element = ent_ref.first_child().unwrap().as_element().unwrap();
                let mut attr_node = ent_element.get_attribute_node("domestic").unwrap();
                let text_node = attr_node.first_child().unwrap();
                assert!(attr_node.remove_child(text_node).is_err());
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrreplacechild1.xml
            #[test]
            fn test_attrreplacechild1() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let ent_ref = doc.create_entity_reference("ent4").unwrap();
                let ent_element = ent_ref.first_child().unwrap().as_element().unwrap();
                let mut attr_node = ent_element.get_attribute_node("domestic").unwrap();
                let text_node = attr_node.first_child().unwrap();
                let new_child = doc.create_text_node("Yesterday");
                assert!(
                    attr_node
                        .replace_child(new_child.into(), text_node)
                        .is_err()
                );
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrsetvaluenomodificationallowederrEE.xml
            #[test]
            fn test_attrsetvaluenomodificationallowederr_ee() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let gender_list = doc.get_elements_by_tag_name("gender");
                let mut gender = gender_list[2].clone();
                let ent_ref = doc.create_entity_reference("ent4").unwrap();
                gender.append_child(ent_ref.clone().into()).unwrap();
                let ent_element = ent_ref.first_child().unwrap();
                let attr_list = ent_element.attributes().unwrap();
                let mut attr_node = attr_list.get_named_item("domestic".into()).unwrap();
                assert!(attr_node.set_value("newvalue").is_err());
                assert!(attr_node.set_node_value("newvalue2").is_err());
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrsetvaluenomodificationallowederr.xml
            #[test]
            fn test_attrsetvaluenomodificationallowederr() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let gender_list = doc.get_elements_by_tag_name("gender");
                let gender = gender_list[2].clone();
                let gen_list = gender.child_nodes();
                let r#gen = gen_list[0].clone();
                let g_list = r#gen.child_nodes();
                let g = g_list[0].clone();
                let attr_list = g.attributes().unwrap();
                let mut attr_node = attr_list.get_named_item("domestic".into()).unwrap();
                assert!(attr_node.set_value("newvalue").is_err());
                assert!(attr_node.set_node_value("newvalue2").is_err());
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrspecifiedvaluechanged.xml
            #[test]
            fn test_attrspecifiedvaluechanged() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let address_list = doc.get_elements_by_tag_name("address");
                let mut test_node = address_list[2].clone();
                test_node.set_attribute("street", "Yes");
                let attributes = test_node.attributes();
                let street_attr = attributes.get_named_item("street".into()).unwrap();
                let state = street_attr.specified();
                assert!(state);
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrspecifiedvalueremove.xml
            #[test]
            fn test_attrspecifiedvalueremove() {
                let doc = staff_xml(STAFF_XML).unwrap();
                let address_list = doc.get_elements_by_tag_name("address");
                let mut test_node = address_list[2].clone();
                test_node.remove_attribute("street".into()).unwrap();
                let attributes = test_node.attributes();
                let street_attr = attributes.get_named_item("street".into()).unwrap();
                let state = street_attr.specified();
                assert!(!state);
            }
            // ./resources/DOM-Test-Suite/tests/level1/core/attrspecifiedvalue.xml
            #[test]
            fn test_attrspecifiedvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/cdatasectiongetdata.xml
            #[test]
            fn test_cdatasectiongetdata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/cdatasectionnormalize.xml
            #[test]
            fn test_cdatasectionnormalize() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataappenddatagetdata.xml
            #[test]
            fn test_characterdataappenddatagetdata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataappenddatanomodificationallowederrEE.xml
            #[test]
            fn test_characterdataappenddatanomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataappenddatanomodificationallowederr.xml
            #[test]
            fn test_characterdataappenddatanomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataappenddata.xml
            #[test]
            fn test_characterdataappenddata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatadeletedatabegining.xml
            #[test]
            fn test_characterdatadeletedatabegining() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatadeletedataend.xml
            #[test]
            fn test_characterdatadeletedataend() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatadeletedataexceedslength.xml
            #[test]
            fn test_characterdatadeletedataexceedslength() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatadeletedatagetlengthanddata.xml
            #[test]
            fn test_characterdatadeletedatagetlengthanddata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatadeletedatamiddle.xml
            #[test]
            fn test_characterdatadeletedatamiddle() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatadeletedatanomodificationallowederrEE.xml
            #[test]
            fn test_characterdatadeletedatanomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatadeletedatanomodificationallowederr.xml
            #[test]
            fn test_characterdatadeletedatanomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatagetdata.xml
            #[test]
            fn test_characterdatagetdata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatagetlength.xml
            #[test]
            fn test_characterdatagetlength() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataindexsizeerrdeletedatacountnegative.xml
            #[test]
            fn test_characterdataindexsizeerrdeletedatacountnegative() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataindexsizeerrdeletedataoffsetgreater.xml
            #[test]
            fn test_characterdataindexsizeerrdeletedataoffsetgreater() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataindexsizeerrdeletedataoffsetnegative.xml
            #[test]
            fn test_characterdataindexsizeerrdeletedataoffsetnegative() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataindexsizeerrinsertdataoffsetgreater.xml
            #[test]
            fn test_characterdataindexsizeerrinsertdataoffsetgreater() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataindexsizeerrinsertdataoffsetnegative.xml
            #[test]
            fn test_characterdataindexsizeerrinsertdataoffsetnegative() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataindexsizeerrreplacedatacountnegative.xml
            #[test]
            fn test_characterdataindexsizeerrreplacedatacountnegative() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataindexsizeerrreplacedataoffsetgreater.xml
            #[test]
            fn test_characterdataindexsizeerrreplacedataoffsetgreater() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataindexsizeerrreplacedataoffsetnegative.xml
            #[test]
            fn test_characterdataindexsizeerrreplacedataoffsetnegative() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataindexsizeerrsubstringcountnegative.xml
            #[test]
            fn test_characterdataindexsizeerrsubstringcountnegative() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataindexsizeerrsubstringnegativeoffset.xml
            #[test]
            fn test_characterdataindexsizeerrsubstringnegativeoffset() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdataindexsizeerrsubstringoffsetgreater.xml
            #[test]
            fn test_characterdataindexsizeerrsubstringoffsetgreater() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatainsertdatabeginning.xml
            #[test]
            fn test_characterdatainsertdatabeginning() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatainsertdataend.xml
            #[test]
            fn test_characterdatainsertdataend() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatainsertdatamiddle.xml
            #[test]
            fn test_characterdatainsertdatamiddle() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatainsertdatanomodificationallowederrEE.xml
            #[test]
            fn test_characterdatainsertdatanomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatainsertdatanomodificationallowederr.xml
            #[test]
            fn test_characterdatainsertdatanomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatareplacedatabegining.xml
            #[test]
            fn test_characterdatareplacedatabegining() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatareplacedataend.xml
            #[test]
            fn test_characterdatareplacedataend() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatareplacedataexceedslengthofarg.xml
            #[test]
            fn test_characterdatareplacedataexceedslengthofarg() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatareplacedataexceedslengthofdata.xml
            #[test]
            fn test_characterdatareplacedataexceedslengthofdata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatareplacedatamiddle.xml
            #[test]
            fn test_characterdatareplacedatamiddle() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatareplacedatanomodificationallowederrEE.xml
            #[test]
            fn test_characterdatareplacedatanomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatareplacedatanomodificationallowederr.xml
            #[test]
            fn test_characterdatareplacedatanomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatasetdatanomodificationallowederrEE.xml
            #[test]
            fn test_characterdatasetdatanomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatasetdatanomodificationallowederr.xml
            #[test]
            fn test_characterdatasetdatanomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatasetnodevalue.xml
            #[test]
            fn test_characterdatasetnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatasubstringexceedsvalue.xml
            #[test]
            fn test_characterdatasubstringexceedsvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/characterdatasubstringvalue.xml
            #[test]
            fn test_characterdatasubstringvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/commentgetcomment.xml
            #[test]
            fn test_commentgetcomment() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentcreateattribute.xml
            #[test]
            fn test_documentcreateattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentcreatecdatasection.xml
            #[test]
            fn test_documentcreatecdatasection() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentcreatecomment.xml
            #[test]
            fn test_documentcreatecomment() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentcreatedocumentfragment.xml
            #[test]
            fn test_documentcreatedocumentfragment() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentcreateelementcasesensitive.xml
            #[test]
            fn test_documentcreateelementcasesensitive() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentcreateelementdefaultattr.xml
            #[test]
            fn test_documentcreateelementdefaultattr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentcreateelement.xml
            #[test]
            fn test_documentcreateelement() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentcreateentityreferenceknown.xml
            #[test]
            fn test_documentcreateentityreferenceknown() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentcreateentityreference.xml
            #[test]
            fn test_documentcreateentityreference() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentcreateprocessinginstruction.xml
            #[test]
            fn test_documentcreateprocessinginstruction() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentcreatetextnode.xml
            #[test]
            fn test_documentcreatetextnode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentgetdoctypenodtd.xml
            #[test]
            fn test_documentgetdoctypenodtd() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentgetdoctype.xml
            #[test]
            fn test_documentgetdoctype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentgetelementsbytagnamelength.xml
            #[test]
            fn test_documentgetelementsbytagnamelength() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentgetelementsbytagnametotallength.xml
            #[test]
            fn test_documentgetelementsbytagnametotallength() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentgetelementsbytagnamevalue.xml
            #[test]
            fn test_documentgetelementsbytagnamevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentgetimplementation.xml
            #[test]
            fn test_documentgetimplementation() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentgetrootnode.xml
            #[test]
            fn test_documentgetrootnode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentinvalidcharacterexceptioncreateattribute.xml
            #[test]
            fn test_documentinvalidcharacterexceptioncreateattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentinvalidcharacterexceptioncreateelement.xml
            #[test]
            fn test_documentinvalidcharacterexceptioncreateelement() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentinvalidcharacterexceptioncreateentref1.xml
            #[test]
            fn test_documentinvalidcharacterexceptioncreateentref1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentinvalidcharacterexceptioncreateentref.xml
            #[test]
            fn test_documentinvalidcharacterexceptioncreateentref() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentinvalidcharacterexceptioncreatepi1.xml
            #[test]
            fn test_documentinvalidcharacterexceptioncreatepi1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documentinvalidcharacterexceptioncreatepi.xml
            #[test]
            fn test_documentinvalidcharacterexceptioncreatepi() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documenttypegetdoctype.xml
            #[test]
            fn test_documenttypegetdoctype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documenttypegetentitieslength.xml
            #[test]
            fn test_documenttypegetentitieslength() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documenttypegetentitiestype.xml
            #[test]
            fn test_documenttypegetentitiestype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documenttypegetentities.xml
            #[test]
            fn test_documenttypegetentities() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documenttypegetnotationstype.xml
            #[test]
            fn test_documenttypegetnotationstype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/documenttypegetnotations.xml
            #[test]
            fn test_documenttypegetnotations() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/domimplementationfeaturenoversion.xml
            #[test]
            fn test_domimplementationfeaturenoversion() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/domimplementationfeaturenull.xml
            #[test]
            fn test_domimplementationfeaturenull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/domimplementationfeaturexml.xml
            #[test]
            fn test_domimplementationfeaturexml() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementaddnewattribute.xml
            #[test]
            fn test_elementaddnewattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementassociatedattribute.xml
            #[test]
            fn test_elementassociatedattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementchangeattributevalue.xml
            #[test]
            fn test_elementchangeattributevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementcreatenewattribute.xml
            #[test]
            fn test_elementcreatenewattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementgetattributenodenull.xml
            #[test]
            fn test_elementgetattributenodenull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementgetattributenode.xml
            #[test]
            fn test_elementgetattributenode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementgetelementempty.xml
            #[test]
            fn test_elementgetelementempty() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementgetelementsbytagnameaccessnodelist.xml
            #[test]
            fn test_elementgetelementsbytagnameaccessnodelist() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementgetelementsbytagnamenomatch.xml
            #[test]
            fn test_elementgetelementsbytagnamenomatch() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementgetelementsbytagnamespecialvalue.xml
            #[test]
            fn test_elementgetelementsbytagnamespecialvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementgetelementsbytagname.xml
            #[test]
            fn test_elementgetelementsbytagname() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementgettagname.xml
            #[test]
            fn test_elementgettagname() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementinuseattributeerr.xml
            #[test]
            fn test_elementinuseattributeerr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementinvalidcharacterexception.xml
            #[test]
            fn test_elementinvalidcharacterexception() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementnormalize.xml
            #[test]
            fn test_elementnormalize() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementnotfounderr.xml
            #[test]
            fn test_elementnotfounderr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementremoveattributeaftercreate.xml
            #[test]
            fn test_elementremoveattributeaftercreate() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementremoveattributenodenomodificationallowederrEE.xml
            #[test]
            fn test_elementremoveattributenodenomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementremoveattributenodenomodificationallowederr.xml
            #[test]
            fn test_elementremoveattributenodenomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementremoveattributenode.xml
            #[test]
            fn test_elementremoveattributenode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementremoveattributenomodificationallowederrEE.xml
            #[test]
            fn test_elementremoveattributenomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementremoveattributenomodificationallowederr.xml
            #[test]
            fn test_elementremoveattributenomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementremoveattributerestoredefaultvalue.xml
            #[test]
            fn test_elementremoveattributerestoredefaultvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementremoveattribute.xml
            #[test]
            fn test_elementremoveattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementreplaceattributewithself.xml
            #[test]
            fn test_elementreplaceattributewithself() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementreplaceexistingattributegevalue.xml
            #[test]
            fn test_elementreplaceexistingattributegevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementreplaceexistingattribute.xml
            #[test]
            fn test_elementreplaceexistingattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementretrieveallattributes.xml
            #[test]
            fn test_elementretrieveallattributes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementretrieveattrvalue.xml
            #[test]
            fn test_elementretrieveattrvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementretrievetagname.xml
            #[test]
            fn test_elementretrievetagname() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementsetattributenodenomodificationallowederrEE.xml
            #[test]
            fn test_elementsetattributenodenomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementsetattributenodenomodificationallowederr.xml
            #[test]
            fn test_elementsetattributenodenomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementsetattributenodenull.xml
            #[test]
            fn test_elementsetattributenodenull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementsetattributenomodificationallowederrEE.xml
            #[test]
            fn test_elementsetattributenomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementsetattributenomodificationallowederr.xml
            #[test]
            fn test_elementsetattributenomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/elementwrongdocumenterr.xml
            #[test]
            fn test_elementwrongdocumenterr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/entitygetentityname.xml
            #[test]
            fn test_entitygetentityname() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/entitygetpublicidnull.xml
            #[test]
            fn test_entitygetpublicidnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/entitygetpublicid.xml
            #[test]
            fn test_entitygetpublicid() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrappendchild1.xml
            #[test]
            fn test_hc_attrappendchild1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrappendchild2.xml
            #[test]
            fn test_hc_attrappendchild2() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrappendchild3.xml
            #[test]
            fn test_hc_attrappendchild3() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrappendchild4.xml
            #[test]
            fn test_hc_attrappendchild4() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrappendchild5.xml
            #[test]
            fn test_hc_attrappendchild5() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrappendchild6.xml
            #[test]
            fn test_hc_attrappendchild6() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrchildnodes1.xml
            #[test]
            fn test_hc_attrchildnodes1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrchildnodes2.xml
            #[test]
            fn test_hc_attrchildnodes2() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrclonenode1.xml
            #[test]
            fn test_hc_attrclonenode1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrcreatedocumentfragment.xml
            #[test]
            fn test_hc_attrcreatedocumentfragment() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrcreatetextnode2.xml
            #[test]
            fn test_hc_attrcreatetextnode2() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrcreatetextnode.xml
            #[test]
            fn test_hc_attrcreatetextnode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attreffectivevalue.xml
            #[test]
            fn test_hc_attreffectivevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrfirstchild.xml
            #[test]
            fn test_hc_attrfirstchild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrgetvalue1.xml
            #[test]
            fn test_hc_attrgetvalue1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrgetvalue2.xml
            #[test]
            fn test_hc_attrgetvalue2() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrhaschildnodes.xml
            #[test]
            fn test_hc_attrhaschildnodes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrinsertbefore1.xml
            #[test]
            fn test_hc_attrinsertbefore1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrinsertbefore2.xml
            #[test]
            fn test_hc_attrinsertbefore2() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrinsertbefore3.xml
            #[test]
            fn test_hc_attrinsertbefore3() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrinsertbefore4.xml
            #[test]
            fn test_hc_attrinsertbefore4() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrinsertbefore5.xml
            #[test]
            fn test_hc_attrinsertbefore5() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrinsertbefore6.xml
            #[test]
            fn test_hc_attrinsertbefore6() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrinsertbefore7.xml
            #[test]
            fn test_hc_attrinsertbefore7() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrlastchild.xml
            #[test]
            fn test_hc_attrlastchild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrname.xml
            #[test]
            fn test_hc_attrname() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrnextsiblingnull.xml
            #[test]
            fn test_hc_attrnextsiblingnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrnormalize.xml
            #[test]
            fn test_hc_attrnormalize() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrparentnodenull.xml
            #[test]
            fn test_hc_attrparentnodenull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrprevioussiblingnull.xml
            #[test]
            fn test_hc_attrprevioussiblingnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrremovechild1.xml
            #[test]
            fn test_hc_attrremovechild1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrremovechild2.xml
            #[test]
            fn test_hc_attrremovechild2() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrreplacechild1.xml
            #[test]
            fn test_hc_attrreplacechild1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrreplacechild2.xml
            #[test]
            fn test_hc_attrreplacechild2() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrsetvalue1.xml
            #[test]
            fn test_hc_attrsetvalue1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrsetvalue2.xml
            #[test]
            fn test_hc_attrsetvalue2() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrspecifiedvaluechanged.xml
            #[test]
            fn test_hc_attrspecifiedvaluechanged() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_attrspecifiedvalue.xml
            #[test]
            fn test_hc_attrspecifiedvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataappenddatagetdata.xml
            #[test]
            fn test_hc_characterdataappenddatagetdata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataappenddata.xml
            #[test]
            fn test_hc_characterdataappenddata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatadeletedatabegining.xml
            #[test]
            fn test_hc_characterdatadeletedatabegining() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatadeletedataend.xml
            #[test]
            fn test_hc_characterdatadeletedataend() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatadeletedataexceedslength.xml
            #[test]
            fn test_hc_characterdatadeletedataexceedslength() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatadeletedatagetlengthanddata.xml
            #[test]
            fn test_hc_characterdatadeletedatagetlengthanddata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatadeletedatamiddle.xml
            #[test]
            fn test_hc_characterdatadeletedatamiddle() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatagetdata.xml
            #[test]
            fn test_hc_characterdatagetdata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatagetlength.xml
            #[test]
            fn test_hc_characterdatagetlength() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataindexsizeerrdeletedatacountnegative.xml
            #[test]
            fn test_hc_characterdataindexsizeerrdeletedatacountnegative() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataindexsizeerrdeletedataoffsetgreater.xml
            #[test]
            fn test_hc_characterdataindexsizeerrdeletedataoffsetgreater() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataindexsizeerrdeletedataoffsetnegative.xml
            #[test]
            fn test_hc_characterdataindexsizeerrdeletedataoffsetnegative() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataindexsizeerrinsertdataoffsetgreater.xml
            #[test]
            fn test_hc_characterdataindexsizeerrinsertdataoffsetgreater() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataindexsizeerrinsertdataoffsetnegative.xml
            #[test]
            fn test_hc_characterdataindexsizeerrinsertdataoffsetnegative() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataindexsizeerrreplacedatacountnegative.xml
            #[test]
            fn test_hc_characterdataindexsizeerrreplacedatacountnegative() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataindexsizeerrreplacedataoffsetgreater.xml
            #[test]
            fn test_hc_characterdataindexsizeerrreplacedataoffsetgreater() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataindexsizeerrreplacedataoffsetnegative.xml
            #[test]
            fn test_hc_characterdataindexsizeerrreplacedataoffsetnegative() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataindexsizeerrsubstringcountnegative.xml
            #[test]
            fn test_hc_characterdataindexsizeerrsubstringcountnegative() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataindexsizeerrsubstringnegativeoffset.xml
            #[test]
            fn test_hc_characterdataindexsizeerrsubstringnegativeoffset() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdataindexsizeerrsubstringoffsetgreater.xml
            #[test]
            fn test_hc_characterdataindexsizeerrsubstringoffsetgreater() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatainsertdatabeginning.xml
            #[test]
            fn test_hc_characterdatainsertdatabeginning() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatainsertdataend.xml
            #[test]
            fn test_hc_characterdatainsertdataend() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatainsertdatamiddle.xml
            #[test]
            fn test_hc_characterdatainsertdatamiddle() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatareplacedatabegining.xml
            #[test]
            fn test_hc_characterdatareplacedatabegining() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatareplacedataend.xml
            #[test]
            fn test_hc_characterdatareplacedataend() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatareplacedataexceedslengthofarg.xml
            #[test]
            fn test_hc_characterdatareplacedataexceedslengthofarg() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatareplacedataexceedslengthofdata.xml
            #[test]
            fn test_hc_characterdatareplacedataexceedslengthofdata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatareplacedatamiddle.xml
            #[test]
            fn test_hc_characterdatareplacedatamiddle() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatasetnodevalue.xml
            #[test]
            fn test_hc_characterdatasetnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatasubstringexceedsvalue.xml
            #[test]
            fn test_hc_characterdatasubstringexceedsvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_characterdatasubstringvalue.xml
            #[test]
            fn test_hc_characterdatasubstringvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_commentgetcomment.xml
            #[test]
            fn test_hc_commentgetcomment() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentcreateattribute.xml
            #[test]
            fn test_hc_documentcreateattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentcreatecomment.xml
            #[test]
            fn test_hc_documentcreatecomment() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentcreatedocumentfragment.xml
            #[test]
            fn test_hc_documentcreatedocumentfragment() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentcreateelementcasesensitive.xml
            #[test]
            fn test_hc_documentcreateelementcasesensitive() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentcreateelement.xml
            #[test]
            fn test_hc_documentcreateelement() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentcreatetextnode.xml
            #[test]
            fn test_hc_documentcreatetextnode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentgetdoctype.xml
            #[test]
            fn test_hc_documentgetdoctype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentgetelementsbytagnamelength.xml
            #[test]
            fn test_hc_documentgetelementsbytagnamelength() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentgetelementsbytagnametotallength.xml
            #[test]
            fn test_hc_documentgetelementsbytagnametotallength() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentgetelementsbytagnamevalue.xml
            #[test]
            fn test_hc_documentgetelementsbytagnamevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentgetimplementation.xml
            #[test]
            fn test_hc_documentgetimplementation() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentgetrootnode.xml
            #[test]
            fn test_hc_documentgetrootnode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentinvalidcharacterexceptioncreateattribute1.xml
            #[test]
            fn test_hc_documentinvalidcharacterexceptioncreateattribute1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentinvalidcharacterexceptioncreateattribute.xml
            #[test]
            fn test_hc_documentinvalidcharacterexceptioncreateattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentinvalidcharacterexceptioncreateelement1.xml
            #[test]
            fn test_hc_documentinvalidcharacterexceptioncreateelement1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_documentinvalidcharacterexceptioncreateelement.xml
            #[test]
            fn test_hc_documentinvalidcharacterexceptioncreateelement() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_domimplementationfeaturenoversion.xml
            #[test]
            fn test_hc_domimplementationfeaturenoversion() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_domimplementationfeaturenull.xml
            #[test]
            fn test_hc_domimplementationfeaturenull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_domimplementationfeaturexml.xml
            #[test]
            fn test_hc_domimplementationfeaturexml() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementaddnewattribute.xml
            #[test]
            fn test_hc_elementaddnewattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementassociatedattribute.xml
            #[test]
            fn test_hc_elementassociatedattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementchangeattributevalue.xml
            #[test]
            fn test_hc_elementchangeattributevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementcreatenewattribute.xml
            #[test]
            fn test_hc_elementcreatenewattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementgetattributenodenull.xml
            #[test]
            fn test_hc_elementgetattributenodenull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementgetattributenode.xml
            #[test]
            fn test_hc_elementgetattributenode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementgetelementempty.xml
            #[test]
            fn test_hc_elementgetelementempty() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementgetelementsbytagnameaccessnodelist.xml
            #[test]
            fn test_hc_elementgetelementsbytagnameaccessnodelist() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementgetelementsbytagnamenomatch.xml
            #[test]
            fn test_hc_elementgetelementsbytagnamenomatch() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementgetelementsbytagnamespecialvalue.xml
            #[test]
            fn test_hc_elementgetelementsbytagnamespecialvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementgetelementsbytagname.xml
            #[test]
            fn test_hc_elementgetelementsbytagname() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementgettagname.xml
            #[test]
            fn test_hc_elementgettagname() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementinuseattributeerr.xml
            #[test]
            fn test_hc_elementinuseattributeerr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementinvalidcharacterexception1.xml
            #[test]
            fn test_hc_elementinvalidcharacterexception1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementinvalidcharacterexception.xml
            #[test]
            fn test_hc_elementinvalidcharacterexception() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementnormalize2.xml
            #[test]
            fn test_hc_elementnormalize2() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementnormalize.xml
            #[test]
            fn test_hc_elementnormalize() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementnotfounderr.xml
            #[test]
            fn test_hc_elementnotfounderr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementremoveattributeaftercreate.xml
            #[test]
            fn test_hc_elementremoveattributeaftercreate() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementremoveattributenode.xml
            #[test]
            fn test_hc_elementremoveattributenode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementremoveattribute.xml
            #[test]
            fn test_hc_elementremoveattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementreplaceattributewithself.xml
            #[test]
            fn test_hc_elementreplaceattributewithself() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementreplaceexistingattributegevalue.xml
            #[test]
            fn test_hc_elementreplaceexistingattributegevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementreplaceexistingattribute.xml
            #[test]
            fn test_hc_elementreplaceexistingattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementretrieveallattributes.xml
            #[test]
            fn test_hc_elementretrieveallattributes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementretrieveattrvalue.xml
            #[test]
            fn test_hc_elementretrieveattrvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementretrievetagname.xml
            #[test]
            fn test_hc_elementretrievetagname() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementsetattributenodenull.xml
            #[test]
            fn test_hc_elementsetattributenodenull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_elementwrongdocumenterr.xml
            #[test]
            fn test_hc_elementwrongdocumenterr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_entitiesremovenameditem1.xml
            #[test]
            fn test_hc_entitiesremovenameditem1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_entitiessetnameditem1.xml
            #[test]
            fn test_hc_entitiessetnameditem1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapchildnoderange.xml
            #[test]
            fn test_hc_namednodemapchildnoderange() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapgetnameditem.xml
            #[test]
            fn test_hc_namednodemapgetnameditem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapinuseattributeerr.xml
            #[test]
            fn test_hc_namednodemapinuseattributeerr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapnotfounderr.xml
            #[test]
            fn test_hc_namednodemapnotfounderr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapnumberofnodes.xml
            #[test]
            fn test_hc_namednodemapnumberofnodes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapremovenameditem.xml
            #[test]
            fn test_hc_namednodemapremovenameditem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapreturnattrnode.xml
            #[test]
            fn test_hc_namednodemapreturnattrnode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapreturnfirstitem.xml
            #[test]
            fn test_hc_namednodemapreturnfirstitem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapreturnlastitem.xml
            #[test]
            fn test_hc_namednodemapreturnlastitem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapreturnnull.xml
            #[test]
            fn test_hc_namednodemapreturnnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapsetnameditemreturnvalue.xml
            #[test]
            fn test_hc_namednodemapsetnameditemreturnvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapsetnameditemthatexists.xml
            #[test]
            fn test_hc_namednodemapsetnameditemthatexists() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapsetnameditemwithnewvalue.xml
            #[test]
            fn test_hc_namednodemapsetnameditemwithnewvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapsetnameditem.xml
            #[test]
            fn test_hc_namednodemapsetnameditem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_namednodemapwrongdocumenterr.xml
            #[test]
            fn test_hc_namednodemapwrongdocumenterr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeappendchildchildexists.xml
            #[test]
            fn test_hc_nodeappendchildchildexists() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeappendchilddocfragment.xml
            #[test]
            fn test_hc_nodeappendchilddocfragment() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeappendchildgetnodename.xml
            #[test]
            fn test_hc_nodeappendchildgetnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeappendchildinvalidnodetype.xml
            #[test]
            fn test_hc_nodeappendchildinvalidnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeappendchildnewchilddiffdocument.xml
            #[test]
            fn test_hc_nodeappendchildnewchilddiffdocument() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeappendchildnodeancestor.xml
            #[test]
            fn test_hc_nodeappendchildnodeancestor() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeappendchild.xml
            #[test]
            fn test_hc_nodeappendchild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeattributenodeattribute.xml
            #[test]
            fn test_hc_nodeattributenodeattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeattributenodename.xml
            #[test]
            fn test_hc_nodeattributenodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeattributenodetype.xml
            #[test]
            fn test_hc_nodeattributenodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeattributenodevalue.xml
            #[test]
            fn test_hc_nodeattributenodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodechildnodesappendchild.xml
            #[test]
            fn test_hc_nodechildnodesappendchild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodechildnodesempty.xml
            #[test]
            fn test_hc_nodechildnodesempty() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodechildnodes.xml
            #[test]
            fn test_hc_nodechildnodes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodecloneattributescopied.xml
            #[test]
            fn test_hc_nodecloneattributescopied() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeclonefalsenocopytext.xml
            #[test]
            fn test_hc_nodeclonefalsenocopytext() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeclonegetparentnull.xml
            #[test]
            fn test_hc_nodeclonegetparentnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeclonenodefalse.xml
            #[test]
            fn test_hc_nodeclonenodefalse() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeclonenodetrue.xml
            #[test]
            fn test_hc_nodeclonenodetrue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeclonetruecopytext.xml
            #[test]
            fn test_hc_nodeclonetruecopytext() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodecommentnodeattributes.xml
            #[test]
            fn test_hc_nodecommentnodeattributes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodecommentnodename.xml
            #[test]
            fn test_hc_nodecommentnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodecommentnodetype.xml
            #[test]
            fn test_hc_nodecommentnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodecommentnodevalue.xml
            #[test]
            fn test_hc_nodecommentnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodedocumentfragmentnodename.xml
            #[test]
            fn test_hc_nodedocumentfragmentnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodedocumentfragmentnodetype.xml
            #[test]
            fn test_hc_nodedocumentfragmentnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodedocumentfragmentnodevalue.xml
            #[test]
            fn test_hc_nodedocumentfragmentnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodedocumentnodeattribute.xml
            #[test]
            fn test_hc_nodedocumentnodeattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodedocumentnodename.xml
            #[test]
            fn test_hc_nodedocumentnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodedocumentnodetype.xml
            #[test]
            fn test_hc_nodedocumentnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodedocumentnodevalue.xml
            #[test]
            fn test_hc_nodedocumentnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeelementnodeattributes.xml
            #[test]
            fn test_hc_nodeelementnodeattributes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeelementnodename.xml
            #[test]
            fn test_hc_nodeelementnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeelementnodetype.xml
            #[test]
            fn test_hc_nodeelementnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeelementnodevalue.xml
            #[test]
            fn test_hc_nodeelementnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodegetfirstchildnull.xml
            #[test]
            fn test_hc_nodegetfirstchildnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodegetfirstchild.xml
            #[test]
            fn test_hc_nodegetfirstchild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodegetlastchildnull.xml
            #[test]
            fn test_hc_nodegetlastchildnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodegetlastchild.xml
            #[test]
            fn test_hc_nodegetlastchild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodegetnextsiblingnull.xml
            #[test]
            fn test_hc_nodegetnextsiblingnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodegetnextsibling.xml
            #[test]
            fn test_hc_nodegetnextsibling() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodegetownerdocumentnull.xml
            #[test]
            fn test_hc_nodegetownerdocumentnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodegetownerdocument.xml
            #[test]
            fn test_hc_nodegetownerdocument() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodegetprevioussiblingnull.xml
            #[test]
            fn test_hc_nodegetprevioussiblingnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodegetprevioussibling.xml
            #[test]
            fn test_hc_nodegetprevioussibling() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodehaschildnodesfalse.xml
            #[test]
            fn test_hc_nodehaschildnodesfalse() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodehaschildnodes.xml
            #[test]
            fn test_hc_nodehaschildnodes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeinsertbeforedocfragment.xml
            #[test]
            fn test_hc_nodeinsertbeforedocfragment() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeinsertbeforeinvalidnodetype.xml
            #[test]
            fn test_hc_nodeinsertbeforeinvalidnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeinsertbeforenewchilddiffdocument.xml
            #[test]
            fn test_hc_nodeinsertbeforenewchilddiffdocument() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeinsertbeforenewchildexists.xml
            #[test]
            fn test_hc_nodeinsertbeforenewchildexists() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeinsertbeforenodeancestor.xml
            #[test]
            fn test_hc_nodeinsertbeforenodeancestor() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeinsertbeforenodename.xml
            #[test]
            fn test_hc_nodeinsertbeforenodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeinsertbeforerefchildnonexistent.xml
            #[test]
            fn test_hc_nodeinsertbeforerefchildnonexistent() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeinsertbeforerefchildnull.xml
            #[test]
            fn test_hc_nodeinsertbeforerefchildnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeinsertbefore.xml
            #[test]
            fn test_hc_nodeinsertbefore() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodelistindexequalzero.xml
            #[test]
            fn test_hc_nodelistindexequalzero() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodelistindexgetlengthofemptylist.xml
            #[test]
            fn test_hc_nodelistindexgetlengthofemptylist() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodelistindexgetlength.xml
            #[test]
            fn test_hc_nodelistindexgetlength() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodelistindexnotzero.xml
            #[test]
            fn test_hc_nodelistindexnotzero() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodelistreturnfirstitem.xml
            #[test]
            fn test_hc_nodelistreturnfirstitem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodelistreturnlastitem.xml
            #[test]
            fn test_hc_nodelistreturnlastitem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodelisttraverselist.xml
            #[test]
            fn test_hc_nodelisttraverselist() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeparentnodenull.xml
            #[test]
            fn test_hc_nodeparentnodenull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodeparentnode.xml
            #[test]
            fn test_hc_nodeparentnode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_noderemovechildgetnodename.xml
            #[test]
            fn test_hc_noderemovechildgetnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_noderemovechildnode.xml
            #[test]
            fn test_hc_noderemovechildnode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_noderemovechildoldchildnonexistent.xml
            #[test]
            fn test_hc_noderemovechildoldchildnonexistent() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_noderemovechild.xml
            #[test]
            fn test_hc_noderemovechild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodereplacechildinvalidnodetype.xml
            #[test]
            fn test_hc_nodereplacechildinvalidnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodereplacechildnewchilddiffdocument.xml
            #[test]
            fn test_hc_nodereplacechildnewchilddiffdocument() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodereplacechildnewchildexists.xml
            #[test]
            fn test_hc_nodereplacechildnewchildexists() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodereplacechildnodeancestor.xml
            #[test]
            fn test_hc_nodereplacechildnodeancestor() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodereplacechildnodename.xml
            #[test]
            fn test_hc_nodereplacechildnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodereplacechildoldchildnonexistent.xml
            #[test]
            fn test_hc_nodereplacechildoldchildnonexistent() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodereplacechild.xml
            #[test]
            fn test_hc_nodereplacechild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodetextnodeattribute.xml
            #[test]
            fn test_hc_nodetextnodeattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodetextnodename.xml
            #[test]
            fn test_hc_nodetextnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodetextnodetype.xml
            #[test]
            fn test_hc_nodetextnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodetextnodevalue.xml
            #[test]
            fn test_hc_nodetextnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodevalue01.xml
            #[test]
            fn test_hc_nodevalue01() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodevalue02.xml
            #[test]
            fn test_hc_nodevalue02() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodevalue03.xml
            #[test]
            fn test_hc_nodevalue03() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodevalue04.xml
            #[test]
            fn test_hc_nodevalue04() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodevalue05.xml
            #[test]
            fn test_hc_nodevalue05() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodevalue06.xml
            #[test]
            fn test_hc_nodevalue06() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodevalue07.xml
            #[test]
            fn test_hc_nodevalue07() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_nodevalue08.xml
            #[test]
            fn test_hc_nodevalue08() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_notationsremovenameditem1.xml
            #[test]
            fn test_hc_notationsremovenameditem1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_notationssetnameditem1.xml
            #[test]
            fn test_hc_notationssetnameditem1() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_textindexsizeerrnegativeoffset.xml
            #[test]
            fn test_hc_textindexsizeerrnegativeoffset() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_textindexsizeerroffsetoutofbounds.xml
            #[test]
            fn test_hc_textindexsizeerroffsetoutofbounds() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_textparseintolistofelements.xml
            #[test]
            fn test_hc_textparseintolistofelements() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_textsplittextfour.xml
            #[test]
            fn test_hc_textsplittextfour() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_textsplittextone.xml
            #[test]
            fn test_hc_textsplittextone() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_textsplittextthree.xml
            #[test]
            fn test_hc_textsplittextthree() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_textsplittexttwo.xml
            #[test]
            fn test_hc_textsplittexttwo() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/hc_textwithnomarkup.xml
            #[test]
            fn test_hc_textwithnomarkup() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/metadata.xml
            #[test]
            fn test_metadata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapchildnoderange.xml
            #[test]
            fn test_namednodemapchildnoderange() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapgetnameditem.xml
            #[test]
            fn test_namednodemapgetnameditem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapinuseattributeerr.xml
            #[test]
            fn test_namednodemapinuseattributeerr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapnotfounderr.xml
            #[test]
            fn test_namednodemapnotfounderr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapnumberofnodes.xml
            #[test]
            fn test_namednodemapnumberofnodes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapremovenameditemgetvalue.xml
            #[test]
            fn test_namednodemapremovenameditemgetvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapremovenameditemreturnnodevalue.xml
            #[test]
            fn test_namednodemapremovenameditemreturnnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapremovenameditem.xml
            #[test]
            fn test_namednodemapremovenameditem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapreturnattrnode.xml
            #[test]
            fn test_namednodemapreturnattrnode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapreturnfirstitem.xml
            #[test]
            fn test_namednodemapreturnfirstitem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapreturnlastitem.xml
            #[test]
            fn test_namednodemapreturnlastitem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapreturnnull.xml
            #[test]
            fn test_namednodemapreturnnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapsetnameditemreturnvalue.xml
            #[test]
            fn test_namednodemapsetnameditemreturnvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapsetnameditemthatexists.xml
            #[test]
            fn test_namednodemapsetnameditemthatexists() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapsetnameditemwithnewvalue.xml
            #[test]
            fn test_namednodemapsetnameditemwithnewvalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapsetnameditem.xml
            #[test]
            fn test_namednodemapsetnameditem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/namednodemapwrongdocumenterr.xml
            #[test]
            fn test_namednodemapwrongdocumenterr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeappendchildchildexists.xml
            #[test]
            fn test_nodeappendchildchildexists() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeappendchilddocfragment.xml
            #[test]
            fn test_nodeappendchilddocfragment() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeappendchildgetnodename.xml
            #[test]
            fn test_nodeappendchildgetnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeappendchildinvalidnodetype.xml
            #[test]
            fn test_nodeappendchildinvalidnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeappendchildnewchilddiffdocument.xml
            #[test]
            fn test_nodeappendchildnewchilddiffdocument() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeappendchildnodeancestor.xml
            #[test]
            fn test_nodeappendchildnodeancestor() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeappendchildnomodificationallowederrEE.xml
            #[test]
            fn test_nodeappendchildnomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeappendchildnomodificationallowederr.xml
            #[test]
            fn test_nodeappendchildnomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeappendchild.xml
            #[test]
            fn test_nodeappendchild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeattributenodeattribute.xml
            #[test]
            fn test_nodeattributenodeattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeattributenodename.xml
            #[test]
            fn test_nodeattributenodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeattributenodetype.xml
            #[test]
            fn test_nodeattributenodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeattributenodevalue.xml
            #[test]
            fn test_nodeattributenodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodecdatasectionnodeattribute.xml
            #[test]
            fn test_nodecdatasectionnodeattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodecdatasectionnodename.xml
            #[test]
            fn test_nodecdatasectionnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodecdatasectionnodetype.xml
            #[test]
            fn test_nodecdatasectionnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodecdatasectionnodevalue.xml
            #[test]
            fn test_nodecdatasectionnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodechildnodesappendchild.xml
            #[test]
            fn test_nodechildnodesappendchild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodechildnodesempty.xml
            #[test]
            fn test_nodechildnodesempty() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodechildnodes.xml
            #[test]
            fn test_nodechildnodes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodecloneattributescopied.xml
            #[test]
            fn test_nodecloneattributescopied() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeclonefalsenocopytext.xml
            #[test]
            fn test_nodeclonefalsenocopytext() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeclonegetparentnull.xml
            #[test]
            fn test_nodeclonegetparentnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeclonenodefalse.xml
            #[test]
            fn test_nodeclonenodefalse() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeclonenodetrue.xml
            #[test]
            fn test_nodeclonenodetrue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeclonetruecopytext.xml
            #[test]
            fn test_nodeclonetruecopytext() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodecommentnodeattributes.xml
            #[test]
            fn test_nodecommentnodeattributes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodecommentnodename.xml
            #[test]
            fn test_nodecommentnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodecommentnodetype.xml
            #[test]
            fn test_nodecommentnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodecommentnodevalue.xml
            #[test]
            fn test_nodecommentnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodedocumentfragmentnodename.xml
            #[test]
            fn test_nodedocumentfragmentnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodedocumentfragmentnodetype.xml
            #[test]
            fn test_nodedocumentfragmentnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodedocumentfragmentnodevalue.xml
            #[test]
            fn test_nodedocumentfragmentnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodedocumentnodeattribute.xml
            #[test]
            fn test_nodedocumentnodeattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodedocumentnodename.xml
            #[test]
            fn test_nodedocumentnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodedocumentnodetype.xml
            #[test]
            fn test_nodedocumentnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodedocumentnodevalue.xml
            #[test]
            fn test_nodedocumentnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodedocumenttypenodename.xml
            #[test]
            fn test_nodedocumenttypenodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodedocumenttypenodetype.xml
            #[test]
            fn test_nodedocumenttypenodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodedocumenttypenodevalue.xml
            #[test]
            fn test_nodedocumenttypenodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeelementnodeattributes.xml
            #[test]
            fn test_nodeelementnodeattributes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeelementnodename.xml
            #[test]
            fn test_nodeelementnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeelementnodetype.xml
            #[test]
            fn test_nodeelementnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeelementnodevalue.xml
            #[test]
            fn test_nodeelementnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeentitynodeattributes.xml
            #[test]
            fn test_nodeentitynodeattributes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeentitynodename.xml
            #[test]
            fn test_nodeentitynodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeentitynodetype.xml
            #[test]
            fn test_nodeentitynodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeentitynodevalue.xml
            #[test]
            fn test_nodeentitynodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeentityreferencenodeattributes.xml
            #[test]
            fn test_nodeentityreferencenodeattributes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeentityreferencenodename.xml
            #[test]
            fn test_nodeentityreferencenodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeentityreferencenodetype.xml
            #[test]
            fn test_nodeentityreferencenodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeentityreferencenodevalue.xml
            #[test]
            fn test_nodeentityreferencenodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeentitysetnodevalue.xml
            #[test]
            fn test_nodeentitysetnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodegetfirstchildnull.xml
            #[test]
            fn test_nodegetfirstchildnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodegetfirstchild.xml
            #[test]
            fn test_nodegetfirstchild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodegetlastchildnull.xml
            #[test]
            fn test_nodegetlastchildnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodegetlastchild.xml
            #[test]
            fn test_nodegetlastchild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodegetnextsiblingnull.xml
            #[test]
            fn test_nodegetnextsiblingnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodegetnextsibling.xml
            #[test]
            fn test_nodegetnextsibling() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodegetownerdocumentnull.xml
            #[test]
            fn test_nodegetownerdocumentnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodegetownerdocument.xml
            #[test]
            fn test_nodegetownerdocument() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodegetprevioussiblingnull.xml
            #[test]
            fn test_nodegetprevioussiblingnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodegetprevioussibling.xml
            #[test]
            fn test_nodegetprevioussibling() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodehaschildnodesfalse.xml
            #[test]
            fn test_nodehaschildnodesfalse() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodehaschildnodes.xml
            #[test]
            fn test_nodehaschildnodes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeinsertbeforedocfragment.xml
            #[test]
            fn test_nodeinsertbeforedocfragment() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeinsertbeforeinvalidnodetype.xml
            #[test]
            fn test_nodeinsertbeforeinvalidnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeinsertbeforenewchilddiffdocument.xml
            #[test]
            fn test_nodeinsertbeforenewchilddiffdocument() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeinsertbeforenewchildexists.xml
            #[test]
            fn test_nodeinsertbeforenewchildexists() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeinsertbeforenodeancestor.xml
            #[test]
            fn test_nodeinsertbeforenodeancestor() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeinsertbeforenodename.xml
            #[test]
            fn test_nodeinsertbeforenodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeinsertbeforenomodificationallowederrEE.xml
            #[test]
            fn test_nodeinsertbeforenomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeinsertbeforenomodificationallowederr.xml
            #[test]
            fn test_nodeinsertbeforenomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeinsertbeforerefchildnonexistent.xml
            #[test]
            fn test_nodeinsertbeforerefchildnonexistent() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeinsertbeforerefchildnull.xml
            #[test]
            fn test_nodeinsertbeforerefchildnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeinsertbefore.xml
            #[test]
            fn test_nodeinsertbefore() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodelistindexequalzero.xml
            #[test]
            fn test_nodelistindexequalzero() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodelistindexgetlengthofemptylist.xml
            #[test]
            fn test_nodelistindexgetlengthofemptylist() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodelistindexgetlength.xml
            #[test]
            fn test_nodelistindexgetlength() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodelistindexnotzero.xml
            #[test]
            fn test_nodelistindexnotzero() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodelistreturnfirstitem.xml
            #[test]
            fn test_nodelistreturnfirstitem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodelistreturnlastitem.xml
            #[test]
            fn test_nodelistreturnlastitem() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodelisttraverselist.xml
            #[test]
            fn test_nodelisttraverselist() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodenotationnodeattributes.xml
            #[test]
            fn test_nodenotationnodeattributes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodenotationnodename.xml
            #[test]
            fn test_nodenotationnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodenotationnodetype.xml
            #[test]
            fn test_nodenotationnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodenotationnodevalue.xml
            #[test]
            fn test_nodenotationnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeparentnodenull.xml
            #[test]
            fn test_nodeparentnodenull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeparentnode.xml
            #[test]
            fn test_nodeparentnode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeprocessinginstructionnodeattributes.xml
            #[test]
            fn test_nodeprocessinginstructionnodeattributes() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeprocessinginstructionnodename.xml
            #[test]
            fn test_nodeprocessinginstructionnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeprocessinginstructionnodetype.xml
            #[test]
            fn test_nodeprocessinginstructionnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeprocessinginstructionnodevalue.xml
            #[test]
            fn test_nodeprocessinginstructionnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodeprocessinginstructionsetnodevalue.xml
            #[test]
            fn test_nodeprocessinginstructionsetnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/noderemovechildgetnodename.xml
            #[test]
            fn test_noderemovechildgetnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/noderemovechildnode.xml
            #[test]
            fn test_noderemovechildnode() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/noderemovechildnomodificationallowederrEE.xml
            #[test]
            fn test_noderemovechildnomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/noderemovechildnomodificationallowederr.xml
            #[test]
            fn test_noderemovechildnomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/noderemovechildoldchildnonexistent.xml
            #[test]
            fn test_noderemovechildoldchildnonexistent() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/noderemovechild.xml
            #[test]
            fn test_noderemovechild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodereplacechildinvalidnodetype.xml
            #[test]
            fn test_nodereplacechildinvalidnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodereplacechildnewchilddiffdocument.xml
            #[test]
            fn test_nodereplacechildnewchilddiffdocument() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodereplacechildnewchildexists.xml
            #[test]
            fn test_nodereplacechildnewchildexists() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodereplacechildnodeancestor.xml
            #[test]
            fn test_nodereplacechildnodeancestor() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodereplacechildnodename.xml
            #[test]
            fn test_nodereplacechildnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodereplacechildnomodificationallowederrEE.xml
            #[test]
            fn test_nodereplacechildnomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodereplacechildnomodificationallowederr.xml
            #[test]
            fn test_nodereplacechildnomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodereplacechildoldchildnonexistent.xml
            #[test]
            fn test_nodereplacechildoldchildnonexistent() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodereplacechild.xml
            #[test]
            fn test_nodereplacechild() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodesetnodevaluenomodificationallowederrEE.xml
            #[test]
            fn test_nodesetnodevaluenomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodesetnodevaluenomodificationallowederr.xml
            #[test]
            fn test_nodesetnodevaluenomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodetextnodeattribute.xml
            #[test]
            fn test_nodetextnodeattribute() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodetextnodename.xml
            #[test]
            fn test_nodetextnodename() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodetextnodetype.xml
            #[test]
            fn test_nodetextnodetype() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodetextnodevalue.xml
            #[test]
            fn test_nodetextnodevalue() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodevalue01.xml
            #[test]
            fn test_nodevalue01() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodevalue02.xml
            #[test]
            fn test_nodevalue02() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodevalue03.xml
            #[test]
            fn test_nodevalue03() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodevalue04.xml
            #[test]
            fn test_nodevalue04() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodevalue05.xml
            #[test]
            fn test_nodevalue05() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodevalue06.xml
            #[test]
            fn test_nodevalue06() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodevalue07.xml
            #[test]
            fn test_nodevalue07() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodevalue08.xml
            #[test]
            fn test_nodevalue08() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/nodevalue09.xml
            #[test]
            fn test_nodevalue09() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/notationgetnotationname.xml
            #[test]
            fn test_notationgetnotationname() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/notationgetpublicidnull.xml
            #[test]
            fn test_notationgetpublicidnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/notationgetpublicid.xml
            #[test]
            fn test_notationgetpublicid() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/notationgetsystemidnull.xml
            #[test]
            fn test_notationgetsystemidnull() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/notationgetsystemid.xml
            #[test]
            fn test_notationgetsystemid() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/processinginstructiongetdata.xml
            #[test]
            fn test_processinginstructiongetdata() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/processinginstructiongettarget.xml
            #[test]
            fn test_processinginstructiongettarget() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/processinginstructionsetdatanomodificationallowederrEE.xml
            #[test]
            fn test_processinginstructionsetdatanomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/processinginstructionsetdatanomodificationallowederr.xml
            #[test]
            fn test_processinginstructionsetdatanomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/textindexsizeerrnegativeoffset.xml
            #[test]
            fn test_textindexsizeerrnegativeoffset() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/textindexsizeerroffsetoutofbounds.xml
            #[test]
            fn test_textindexsizeerroffsetoutofbounds() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/textparseintolistofelements.xml
            #[test]
            fn test_textparseintolistofelements() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/textsplittextfour.xml
            #[test]
            fn test_textsplittextfour() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/textsplittextnomodificationallowederrEE.xml
            #[test]
            fn test_textsplittextnomodificationallowederr_ee() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/textsplittextnomodificationallowederr.xml
            #[test]
            fn test_textsplittextnomodificationallowederr() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/textsplittextone.xml
            #[test]
            fn test_textsplittextone() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/textsplittextthree.xml
            #[test]
            fn test_textsplittextthree() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/textsplittexttwo.xml
            #[test]
            fn test_textsplittexttwo() {}
            // ./resources/DOM-Test-Suite/tests/level1/core/textwithnomarkup.xml
            #[test]
            fn test_textwithnomarkup() {}
        }
    }
}

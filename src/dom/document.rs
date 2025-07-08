use std::{
    cell::RefCell,
    mem::replace,
    rc::{Rc, Weak},
    sync::Arc,
};

use crate::{
    chvalid::XmlCharValid,
    error::XmlParserErrors,
    parser::split_qname2,
    tree::{validate_name, validate_qname},
};

use super::{
    DOMException, NodeType, XML_NS_NAMESPACE, XML_XML_NAMESPACE,
    attlistdecl::DefaultDecl,
    attr::AttrRef,
    character_data::{CDATASectionRef, CommentRef, TextRef},
    check_owner_document_sameness, check_vertical_hierarchy,
    document_fragment::DocumentFragmentRef,
    document_type::DocumentTypeRef,
    dom_implementation::{DEFAULT_DOM_IMPLEMENTATION, DOMImplementation},
    element::ElementRef,
    entity::EntityType,
    entity::{EntityRef, xml_entities_err},
    entity_reference::EntityReferenceRef,
    named_node_map::NamedNodeMap,
    node::{Node, NodeConnection, NodeRef},
    node_list::FilteredSubtreeElementsList,
    pi::ProcessingInstructionRef,
};

/// Implementation of [Document](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-i-Document)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
pub struct Document {
    // /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    // /// - no parent
    // parent_node: Option<NodeWeakRef>,
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)  
    /// - `Element` (maximum of one)
    /// - `ProcessingInstruction`
    /// - `Comment`
    /// - `DocumentType` (maximum of one)
    first_child: Option<NodeRef>,
    last_child: Option<NodeRef>,
    // /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    // /// - no sibling
    // previous_sibling: Option<NodeWeakRef>,
    // next_sibling: Option<NodeRef>,
    /// Implementation of `doctype` attribute.  
    doctype: Option<DocumentTypeRef>,
    /// Implementation of `implementation` attribute.
    implementation: Arc<dyn DOMImplementation>,
    /// Implementation of `documentElement` attribute.
    document_element: Option<ElementRef>,
    /// Implementation of `documentURI` attribute.
    document_uri: Option<Rc<str>>,
    /// Implementation of `inputEncoding` attribute.
    input_encoding: Option<Rc<str>>,
    /// Implementation of `xmlEncoding` attribute.
    xml_encoding: Option<Rc<str>>,
    /// Implementation of `xmlStandalone` attribute.  
    /// In libxml2, this field us used as follows.
    ///
    /// | value | meaning                                                                |
    /// | ----- |:---------------------------------------------------------------------- |
    /// | `1`   | standalone="yes"                                                       |
    /// | `0`   | standalone="no"                                                        |
    /// | `-1`  | there is no XML declaration                                            |
    /// | `-2`  | there is an XML declaration, but no standalone attribute was specified |
    xml_standalone: i8,
    /// Implementation of `xmlVersion` attribute.
    xml_version: Option<Rc<str>>,

    /// HTML document or not
    html: bool,
    /// 0      : enable modification check if set
    /// 1 - 31 : unused
    flag: u32,

    // Predefined entities.
    //  0: lt
    //  1: gt
    //  2: amp
    //  3: apos
    //  4: quot
    predefined_entities: [EntityRef; 5],
}

impl Document {
    /// Enable check for read-only node.\
    /// As a result, editing of nodes specified as read-only in the DOM specification
    /// becomes impossible.
    pub fn enable_read_only_check(&mut self) {
        self.flag |= 0b01;
    }

    /// Disable check for read-only node.\
    /// It allows editing of nodes that are not editable in the DOM specification
    /// (e.g., DTD nodes).
    pub fn disable_read_only_check(&mut self) {
        self.flag &= !0b01;
    }

    /// Check if read-only check is enabled.
    pub fn is_enabled_read_only_check(&self) -> bool {
        self.flag & 0b01 != 0
    }

    /// Do an entity lookup in the document entity hash table and
    /// returns the corresponding entity, otherwise a lookup is done
    /// in the predefined entities too.
    ///
    /// If found, return found entity wrapped `Some`.  
    /// Otherwise, return `None`.
    #[doc(alias = "xmlGetDocEntity")]
    pub fn get_entity(&self, name: &str) -> Option<EntityRef> {
        if let Some(ent) = self
            .doctype
            .as_ref()
            .and_then(|doctype| doctype.get_entity(name))
        {
            return Some(ent);
        }

        match name {
            "lt" => Some(self.predefined_entities[0].clone()),
            "gt" => Some(self.predefined_entities[1].clone()),
            "amp" => Some(self.predefined_entities[2].clone()),
            "apos" => Some(self.predefined_entities[3].clone()),
            "quot" => Some(self.predefined_entities[4].clone()),
            _ => None,
        }
    }

    /// Do a global encoding of a string, replacing the predefined entities
    /// and non ASCII values with their entities and CharRef counterparts.
    /// Contrary to xmlEncodeEntities, this routine is reentrant, and result
    /// must be deallocated.
    ///
    /// Returns a newly allocated string with the substitution done.
    #[doc(alias = "xmlEncodeEntitiesInternal")]
    fn encode_entities_internal<const ATTR: bool>(&mut self, input: &str) -> String {
        // allocate an translation buffer.
        let mut cur = input;
        let mut out = String::new();
        while !cur.is_empty() {
            // By default one have to encode at least '<', '>', '"' and '&' !
            if cur.starts_with('<') {
                // Special handling of server side include in HTML attributes
                if self.html && ATTR && cur.starts_with("<!--") {
                    if let Some(pos) = cur.find("-->") {
                        out.push_str(&cur[..pos + 3]);
                        cur = &cur[pos + 3..];
                        continue;
                    }
                }
                out.push_str("&lt;");
            } else if cur.starts_with('>') {
                out.push_str("&gt;");
            } else if cur.starts_with('&') {
                // Special handling of &{...} construct from HTML 4, see
                // http://www.w3.org/TR/html401/appendix/notes.html#h-B.7.1
                if self.html && ATTR && cur[1..].starts_with('{') {
                    if let Some(pos) = cur.find('}') {
                        out.push_str(&cur[..pos + 1]);
                        cur = &cur[pos + 1..];
                        continue;
                    }
                }
                out.push_str("&amp;");
            } else if matches!(cur.as_bytes()[0], 0x20..0x80 | b'\n' | b'\t')
                || (self.html && cur.starts_with('\r'))
            {
                // default case, just copy !
                out.push(cur.as_bytes()[0] as char);
            } else if matches!(cur.as_bytes()[0], 0x80..) {
                if self.input_encoding.is_some() || self.html {
                    let c = cur.chars().next().unwrap();
                    out.push(c);
                    cur = &cur[c.len_utf8()..];
                    continue;
                } else {
                    let val = cur.chars().next().unwrap();
                    if !val.is_xml_char() {
                        xml_entities_err(
                            XmlParserErrors::XmlErrInvalidChar,
                            "xmlEncodeEntities: char out of range\n",
                        );
                        self.input_encoding = Some("ISO-8859-1".into());
                        out.push_str(format!("&#{}", cur.as_bytes()[0]).as_str());
                        cur = &cur[1..];
                        continue;
                    }
                    // We could do multiple things here. Just save as a c_char ref
                    out.push_str(format!("&#x{:X}", val as u32).as_str());
                    cur = &cur[val.len_utf8()..];
                    continue;
                }
            } else if cur.as_bytes()[0].is_xml_char() {
                out.push_str(format!("&#{};", cur.as_bytes()[0]).as_str());
            }
            cur = &cur[1..];
        }
        out
    }
}

/// Wrapper of `Rc<RefCell<Document>>`.
#[derive(Clone)]
pub struct DocumentRef(pub(super) Rc<RefCell<Document>>);

impl DocumentRef {
    /// Implementation of [`createDocument`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Level-2-Core-DOM-createDocument) method.
    ///
    /// In the specification, this is implemented in [`DOMImplementation`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-102161490).
    ///
    /// ```text
    /// Creates a DOM Document object of the specified type with its document element.
    /// Note that based on the DocumentType given to create the document, the implementation
    /// may instantiate specialized Document objects that support additional features than
    /// the "Core", such as "HTML" [DOM Level 2 HTML]. On the other hand, setting the
    /// DocumentType after the document was created makes this very unlikely to happen.
    /// Alternatively, specialized Document creation methods, such as createHTMLDocument
    /// [DOM Level 2 HTML], can be used to obtain specific types of Document objects.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the document element to create or null.
    ///     qualifiedName of type DOMString
    ///         The qualified name of the document element to be created or null.
    ///     doctype of type DocumentType
    ///         The type of document to be created or null.
    ///         When doctype is not null, its Node.ownerDocument attribute is set to the
    ///         document being created.
    ///
    /// Return Value
    ///     Document A new Document object with its document element. If the NamespaceURI,
    ///              qualifiedName, and doctype are null, the returned Document is empty with
    ///              no document element.
    ///
    /// Exceptions
    ///     DOMException
    ///     INVALID_CHARACTER_ERR: Raised if the specified qualified name is not an XML name
    ///                            according to [XML 1.0].
    ///     NAMESPACE_ERR:         Raised if the qualifiedName is malformed, if the
    ///                            qualifiedName has a prefix and the namespaceURI is null,
    ///                            or if the qualifiedName is null and the namespaceURI is
    ///                            different from null, or if the qualifiedName has a prefix
    ///                            that is "xml" and the namespaceURI is different from
    ///                            "http://www.w3.org/XML/1998/namespace" [XML Namespaces],
    ///                            or if the DOM implementation does not support the "XML"
    ///                            feature but a non-null namespace URI was provided, since
    ///                            namespaces were defined by XML.
    ///     WRONG_DOCUMENT_ERR:    Raised if doctype has already been used with a different
    ///                            document or was created from a different implementation.
    ///     NOT_SUPPORTED_ERR:     May be raised if the implementation does not support the
    ///                            feature "XML" and the language exposed through the Document
    ///                            does not support XML Namespaces (such as [HTML 4.01]).
    /// ```
    pub fn new(
        namespace_uri: Option<&str>,
        qualified_name: Option<&str>,
        doctype: Option<DocumentTypeRef>,
    ) -> Result<Self, DOMException> {
        if doctype
            .as_ref()
            .is_some_and(|doctype| doctype.owner_document().is_some())
        {
            return Err(DOMException::WrongDocumentErr);
        }

        let mut new = DocumentRef(Rc::new(RefCell::new(Document {
            first_child: None,
            last_child: None,
            doctype: None,
            implementation: (*DEFAULT_DOM_IMPLEMENTATION).clone(),
            document_element: None,
            document_uri: None,
            input_encoding: None,
            xml_encoding: None,
            xml_standalone: 0,
            xml_version: None,
            html: false,
            flag: 0,
            predefined_entities: [
                EntityRef::new(None, "lt".into(), EntityType::InternalPredefinedEntity),
                EntityRef::new(None, "gt".into(), EntityType::InternalPredefinedEntity),
                EntityRef::new(None, "amp".into(), EntityType::InternalPredefinedEntity),
                EntityRef::new(None, "apos".into(), EntityType::InternalPredefinedEntity),
                EntityRef::new(None, "quot".into(), EntityType::InternalPredefinedEntity),
            ],
        })));

        // TODO: check if the DTD specifies this document is HTML or not.
        if let Some(doctype) = doctype {
            new.append_child(doctype.into())?;
        }

        if let Some(qname) = qualified_name {
            if validate_name::<false>(qname).is_err() {
                return Err(DOMException::InvalidCharacterErr);
            }
            if validate_qname::<false>(qname).is_err() {
                return Err(DOMException::NamespaceErr);
            }

            let elem = if let Some((prefix, _)) = split_qname2(qname) {
                let Some(ns_uri) = namespace_uri else {
                    // ... if the qualifiedName has a prefix and the namespaceURI is null,
                    return Err(DOMException::NamespaceErr);
                };

                // ... or if the qualifiedName has a prefix that is "xml" and the namespaceURI
                // is different from "http://www.w3.org/XML/1998/namespace" [XML Namespaces],
                if prefix == "xml" && ns_uri != "http://www.w3.org/XML/1998/namespace" {
                    return Err(DOMException::NamespaceErr);
                }
                ElementRef::with_namespace(new.clone(), qname.into(), Some(ns_uri.into()))
            } else {
                ElementRef::new(new.clone(), qname.into())
            };

            new.append_child(elem.into())?;
        } else {
            // ... or if the qualifiedName is null and the namespaceURI is different from null
            if namespace_uri.is_some() {
                return Err(DOMException::NamespaceErr);
            }
        }

        // Setup predefined entities
        let predefined_entities = new.0.borrow().predefined_entities.clone();
        for (mut ent, text) in predefined_entities.iter().cloned().zip([
            new.create_text_node("<"),
            new.create_text_node(">"),
            new.create_text_node("&"),
            new.create_text_node("'"),
            new.create_text_node("\""),
        ]) {
            ent.set_owner_document(new.clone());
            ent.append_child(text.into())?;
        }

        Ok(new)
    }

    /// Implementation of [`createElement`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-2141741547) method.
    ///
    /// # Specification
    /// ```text
    /// Creates an element of the type specified. Note that the instance returned implements
    /// the Element interface, so attributes can be specified directly on the returned object.
    /// In addition, if there are known attributes with default values, Attr nodes
    /// representing them are automatically created and attached to the element.
    /// To create an element with a qualified name and namespace URI, use the createElementNS method.
    ///
    /// Parameters
    ///     tagName of type DOMString
    ///         The name of the element type to instantiate. For XML, this is case-sensitive,
    ///         otherwise it depends on the case-sensitivity of the markup language in use.
    ///         In that case, the name is mapped to the canonical form of that markup
    ///         by the DOM implementation.
    ///
    /// Return Value
    ///     Element A new Element object with the nodeName attribute set to tagName,
    ///             and localName, prefix, and namespaceURI set to null.
    ///
    /// Exceptions
    ///     DOMException
    ///     INVALID_CHARACTER_ERR: Raised if the specified name is not an XML name
    ///                            according to the XML version in use specified in
    ///                            the Document.xmlVersion attribute.
    /// ```
    pub fn create_element(&self, tag_name: impl Into<Rc<str>>) -> Result<ElementRef, DOMException> {
        let tag_name: Rc<str> = tag_name.into();
        if validate_name::<false>(&tag_name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }

        Ok(ElementRef::new(self.clone(), tag_name))
    }

    /// Implementation of [`createDocumentFragment`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-35CB04B5) method.
    ///
    /// # Specification
    /// ```text
    /// Creates an empty DocumentFragment object.
    ///
    /// Return Value
    ///     DocumentFragment A new DocumentFragment.
    ///
    /// No Parameters
    /// No Exceptions
    /// ```
    pub fn create_document_fragment(&self) -> DocumentFragmentRef {
        DocumentFragmentRef::from_doc(self.clone())
    }

    /// Implementation of [`createTextNode`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1975348127) method.
    ///
    /// # Specification
    /// ```text
    /// Creates a Text node given the specified string.
    ///
    /// Parameters
    ///     data of type DOMString
    ///         The data for the node.
    ///
    /// Return Value
    ///     Text The new Text object.
    ///
    /// No Exceptions
    /// ```
    pub fn create_text_node(&self, data: impl Into<String>) -> TextRef {
        TextRef::from_doc(self.clone(), data.into())
    }

    /// Implementation of [`createComment`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1334481328) method.
    ///
    /// # Specification
    /// ```text
    /// Creates a Comment node given the specified string.
    ///
    /// Parameters
    ///     data of type DOMString
    ///         The data for the node.
    ///
    /// Return Value
    ///     Comment The new Comment object.
    ///
    /// No Exceptions
    /// ```
    pub fn create_comment(&self, data: impl Into<String>) -> CommentRef {
        CommentRef::from_doc(self.clone(), data.into())
    }

    /// Implementation of [`createCDATASection`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-D26C0AF8) method.
    ///
    /// According to the specification, [`Err`] is returned if the document is an HTML document.
    ///
    /// # Specification
    /// ```text
    /// Creates a CDATASection node whose value is the specified string.
    ///
    /// Parameters
    ///     data of type DOMString
    ///         The data for the CDATASection contents.
    ///
    /// Return Value
    ///     CDATASection The new CDATASection object.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_SUPPORTED_ERR: Raised if this document is an HTML document.
    /// ```
    pub fn create_cdata_section(
        &self,
        data: impl Into<String>,
    ) -> Result<CDATASectionRef, DOMException> {
        if self.is_html() {
            Err(DOMException::NotSupportedErr)
        } else {
            Ok(CDATASectionRef::from_doc(self.clone(), data.into()))
        }
    }

    /// Implementation of [`createProcessingInstruction`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-135944439) method.
    ///
    /// # Specification
    /// ```text
    /// Creates a ProcessingInstruction node given the specified name and data strings.
    ///
    /// Parameters
    ///     target of type DOMString
    ///         The target part of the processing instruction.
    ///         Unlike Document.createElementNS or Document.createAttributeNS,
    ///         no namespace well-formed checking is done on the target name.
    ///         Applications should invoke Document.normalizeDocument()
    ///         with the parameter "namespaces" set to true in order to ensure
    ///         that the target name is namespace well-formed.
    ///     data of type DOMString
    ///         The data for the node.
    ///
    /// Return Value
    ///     ProcessingInstruction The new ProcessingInstruction object.
    ///
    /// Exceptions
    ///     DOMException
    ///     INVALID_CHARACTER_ERR: Raised if the specified target is not
    ///                            an XML name according to the XML version in use
    ///                            specified in the Document.xmlVersion attribute.
    ///     NOT_SUPPORTED_ERR:     Raised if this document is an HTML document.
    /// ```
    pub fn create_processing_instruction(
        &self,
        target: impl Into<Rc<str>>,
        data: Option<impl Into<Rc<str>>>,
    ) -> Result<ProcessingInstructionRef, DOMException> {
        if self.is_html() {
            return Err(DOMException::NotSupportedErr);
        }

        let target: Rc<str> = target.into();

        // [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
        if target.eq_ignore_ascii_case("xml") || validate_name::<false>(&target).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }

        Ok(ProcessingInstructionRef::from_doc(
            self.clone(),
            target,
            data.map(|data| data.into()),
        ))
    }

    /// Implementation of [`createAttribute`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1084891198) method.
    ///
    /// # Specification
    /// ```text
    /// Creates an Attr of the given name. Note that the Attr instance can then be set
    /// on an Element using the setAttributeNode method.
    /// To create an attribute with a qualified name and namespace URI, use the
    /// createAttributeNS method.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The name of the attribute.
    ///
    /// Return Value
    ///     Attr A new Attr object with the nodeName attribute set to name, and localName,
    ///          prefix, and namespaceURI set to null. The value of the attribute
    ///          is the empty string.
    ///
    /// Exceptions
    ///     DOMException
    ///     INVALID_CHARACTER_ERR: Raised if the specified name is not an XML name
    ///                            according to the XML version in use specified in
    ///                            the Document.xmlVersion attribute.
    /// ```
    pub fn create_attribute(&self, name: impl Into<Rc<str>>) -> Result<AttrRef, DOMException> {
        let name: Rc<str> = name.into();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }

        Ok(AttrRef::new(self.clone(), name))
    }

    /// Implementation of [`createEntityReference`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-392B75AE) method.
    ///
    /// # Specification
    /// ```text
    /// Creates an EntityReference object. In addition, if the referenced entity is known,
    /// the child list of the EntityReference node is made the same as that of
    /// the corresponding Entity node.
    /// Note: If any descendant of the Entity node has an unbound namespace prefix,
    /// the corresponding descendant of the created EntityReference node is also unbound;
    /// (its namespaceURI is null). The DOM Level 2 and 3 do not support any mechanism
    /// to resolve namespace prefixes in this case.
    ///
    /// Parameters
    ///     name of type DOMString
    ///         The name of the entity to reference.
    ///         Unlike Document.createElementNS or Document.createAttributeNS,
    ///         no namespace well-formed checking is done on the entity name.
    ///         Applications should invoke Document.normalizeDocument() with the parameter
    ///         "namespaces" set to true in order to ensure that the entity name is namespace
    ///         well-formed.
    ///
    /// Return Value
    ///     EntityReference The new EntityReference object.
    ///
    /// Exceptions
    ///     DOMException
    ///     INVALID_CHARACTER_ERR: Raised if the specified name is not an XML name according
    ///                            to the XML version in use specified in the
    ///                            Document.xmlVersion attribute.
    ///     NOT_SUPPORTED_ERR:     Raised if this document is an HTML document.
    /// ```
    pub fn create_entity_reference(
        &self,
        name: impl Into<Rc<str>>,
    ) -> Result<EntityReferenceRef, DOMException> {
        if self.is_html() {
            return Err(DOMException::NotSupportedErr);
        }
        let name: Rc<str> = name.into();
        if validate_name::<false>(&name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }
        let mut entref = EntityReferenceRef::from_doc(self.clone(), name);

        if let Some(entity) = self.get_entity(&entref.node_name()) {
            let read_only_check = self.is_enabled_read_only_check();
            self.0.borrow_mut().disable_read_only_check();
            let mut children = entity.first_child();
            while let Some(child) = children {
                children = child.next_sibling();
                entref.append_child(child.clone_node(true))?;
            }
            if read_only_check {
                self.0.borrow_mut().enable_read_only_check();
            }
        }
        Ok(entref)
    }

    /// Implementation of [`getElementsByTagName`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-A6C9094) method.
    ///
    /// # Specification
    /// ```text
    /// Returns a NodeList of all the Elements in document order with a given tag name
    /// and are contained in the document.
    ///
    /// Parameters
    ///     tagname of type DOMString
    ///         The name of the tag to match on. The special value "*" matches all tags.
    ///         For XML, the tagname parameter is case-sensitive, otherwise it depends
    ///         on the case-sensitivity of the markup language in use.
    ///
    /// Return Value
    ///     NodeList A new NodeList object containing all the matched Elements.
    ///
    /// No Exceptions
    /// ```
    pub fn get_elements_by_tag_name(&self, tag_name: &str) -> FilteredSubtreeElementsList {
        // If this document is HTML document, the tagname is case-insensitive.
        if self.is_html() {
            FilteredSubtreeElementsList::new(
                self.clone().into(),
                None,
                tag_name.to_owned(),
                |elem, _, name| name == "*" || elem.node_name().eq_ignore_ascii_case(name),
            )
        } else {
            FilteredSubtreeElementsList::new(
                self.clone().into(),
                None,
                tag_name.to_owned(),
                |elem, _, name| name == "*" || elem.node_name().as_ref() == name,
            )
        }
    }

    /// Implementation of [`importNode`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Core-Document-importNode) method.
    ///
    /// # Specification
    /// ```text
    /// Imports a node from another document to this document, without altering
    /// or removing the source node from the original document; this method creates
    /// a new copy of the source node. The returned node has no parent; (parentNode is null).
    /// For all nodes, importing a node creates a node object owned by the importing document,
    /// with attribute values identical to the source node's nodeName and nodeType,
    /// plus the attributes related to namespaces (prefix, localName, and namespaceURI).
    /// As in the cloneNode operation, the source node is not altered. User data associated
    /// to the imported node is not carried over. However, if any UserDataHandlers has been
    /// specified along with the associated data these handlers will be called with
    /// the appropriate parameters before this method returns.
    /// Additional information is copied as appropriate to the nodeType, attempting to mirror
    /// the behavior expected if a fragment of XML or HTML source was copied from one document
    /// to another, recognizing that the two documents may have different DTDs in the XML case.
    /// The following list describes the specifics for each type of node.
    ///
    /// <too long...please refer to the specification>
    ///
    /// Parameters
    ///     importedNode of type Node
    ///         The node to import.
    ///     deep of type boolean
    ///         If true, recursively import the subtree under the specified node; if false,
    ///         import only the node itself, as explained above. This has no effect on nodes
    ///         that cannot have any children, and on Attr, and EntityReference nodes.
    ///
    /// Return Value
    ///     Node The imported node that belongs to this Document.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_SUPPORTED_ERR:     Raised if the type of node being imported is not supported.
    ///     INVALID_CHARACTER_ERR: Raised if one of the imported names is not an XML name
    ///                            according to the XML version in use specified in the
    ///                            Document.xmlVersion attribute. This may happen when
    ///                            importing  an XML 1.1 [XML 1.1] element into an XML 1.0
    ///                            document, for instance.
    /// ```
    pub fn import_node(
        &mut self,
        imported_node: NodeRef,
        deep: bool,
    ) -> Result<NodeRef, DOMException> {
        match &imported_node {
            NodeRef::Attribute(attr) => {
                let mut new_attr = if attr.local_name().is_some() {
                    self.create_attribute_ns(
                        attr.namespace_uri().as_deref(),
                        attr.node_name().as_ref(),
                    )
                } else {
                    self.create_attribute(attr.node_name().as_ref())
                }?;
                let mut children = attr.first_child();
                while let Some(child) = children {
                    children = child.next_sibling();
                    new_attr.append_child(self.import_node(child, true)?)?;
                }
                Ok(new_attr.into())
            }
            NodeRef::DocumentFragment(frag) => {
                let mut new = self.create_document_fragment();
                if deep {
                    let mut children = frag.first_child();
                    while let Some(child) = children {
                        children = child.next_sibling();
                        new.append_child(self.import_node(child, true)?)?;
                    }
                }
                Ok(new.into())
            }
            NodeRef::Document(_) | NodeRef::DocumentType(_) => Err(DOMException::NotSupportedErr),
            NodeRef::Element(elem) => {
                let mut new = if elem.local_name().is_some() {
                    self.create_element_ns(
                        elem.namespace_uri().as_deref(),
                        elem.node_name().as_ref(),
                    )
                } else {
                    self.create_element(elem.node_name())
                }?;

                let attrs = elem.attributes();
                for i in 0..attrs.length() {
                    let attr = attrs.item(i).unwrap();
                    if attr.specified() {
                        let attr = self.import_node(attr.into(), deep)?.as_attribute().unwrap();
                        new.set_attribute_node_ns(attr.clone())
                            .or_else(|_| new.set_attribute_node(attr))?;
                    }
                }

                if deep {
                    let mut children = elem.first_child();
                    while let Some(child) = children {
                        children = child.next_sibling();
                        new.append_child(self.import_node(child, true)?)?;
                    }
                }
                Ok(new.into())
            }
            NodeRef::EntityReference(entref) => Ok(self
                .create_entity_reference(entref.node_name().as_ref())?
                .into()),
            node @ (NodeRef::Notation(_)
            | NodeRef::CDATASection(_)
            | NodeRef::Comment(_)
            | NodeRef::ProcessingInstruction(_)
            | NodeRef::Text(_)
            | NodeRef::Entity(_)) => {
                let mut new = node.clone_node(deep);
                new.set_owner_document(self.clone());

                let mut children = new.first_child();
                while let Some(mut child) = children {
                    let new_child = self.import_node(child.clone(), deep)?;
                    new.append_child(new_child);

                    if let Some(ch) = child.first_child() {
                        children = Some(ch);
                    } else if let Some(sib) = child.next_sibling() {
                        children = Some(sib);
                    } else {
                        children = None;
                        while let Some(par) =
                            child.parent_node().filter(|par| !new.is_same_node(par))
                        {
                            if let Some(sib) = par.next_sibling() {
                                children = Some(sib);
                                break;
                            }
                            child = par;
                        }
                    }
                }
                Ok(new)
            }
        }
    }

    /// Implementation of [`createElementNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-DocCrElNS) method.
    ///
    /// # Specification
    /// ```text
    /// Creates an element of the given qualified name and namespace URI.
    /// Per [XML Namespaces], applications must use the value null as the namespaceURI
    /// parameter for methods if they wish to have no namespace.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the element to create.
    ///     qualifiedName of type DOMString
    ///         The qualified name of the element type to instantiate.
    ///
    /// Return Value
    ///     Element A new Element object with the following attributes:
    ///
    /// Exceptions
    ///     DOMException
    ///     INVALID_CHARACTER_ERR: Raised if the specified qualifiedName is not an XML name
    ///                            according to the XML version in use specified in the
    ///                            Document.xmlVersion attribute.
    ///     NAMESPACE_ERR:         Raised if the qualifiedName is a malformed qualified name,
    ///                            if the qualifiedName has a prefix and the namespaceURI
    ///                            is null, or if the qualifiedName has a prefix that is "xml"
    ///                            and the namespaceURI is different from
    ///                            "http://www.w3.org/XML/1998/namespace" [XML Namespaces],
    ///                            or if the qualifiedName or its prefix is "xmlns" and the
    ///                            namespaceURI is different from
    ///                            "http://www.w3.org/2000/xmlns/", or if the namespaceURI
    ///                            is "http://www.w3.org/2000/xmlns/" and neither
    ///                            the qualifiedName nor its prefix is "xmlns".
    ///     NOT_SUPPORTED_ERR:     Always thrown if the current document does not support
    ///                            the "XML" feature, since namespaces were defined by XML.
    /// ```
    pub fn create_element_ns(
        &mut self,
        namespace_uri: Option<&str>,
        qualified_name: &str,
    ) -> Result<ElementRef, DOMException> {
        if validate_name::<false>(qualified_name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }
        if validate_qname::<false>(qualified_name).is_err() {
            return Err(DOMException::NamespaceErr);
        }

        match split_qname2(qualified_name) {
            Some((prefix, _)) => {
                // ... if the qualifiedName has a prefix and the namespaceURI is null, ...
                let Some(ns_uri) = namespace_uri else {
                    return Err(DOMException::NamespaceErr);
                };

                // ... if the qualifiedName has a prefix that is "xml"
                // and the namespaceURI is different from "http://www.w3.org/XML/1998/namespace" ...
                if prefix == "xml" && ns_uri != XML_XML_NAMESPACE {
                    return Err(DOMException::NamespaceErr);
                }

                // ... if the qualifiedName or its prefix is "xmlns"
                // and the namespaceURI is different from "http://www.w3.org/2000/xmlns/" ...
                if (prefix == "xmlns" || qualified_name == "xmlns") && ns_uri != XML_NS_NAMESPACE {
                    return Err(DOMException::NamespaceErr);
                }

                // ... if the namespaceURI is "http://www.w3.org/2000/xmlns/"
                // and neither the qualifiedName nor its prefix is "xmlns".
                if ns_uri == XML_NS_NAMESPACE && prefix != "xmlns" && qualified_name != "xmlns" {
                    return Err(DOMException::NamespaceErr);
                }

                Ok(ElementRef::with_namespace(
                    self.clone(),
                    qualified_name.into(),
                    Some(ns_uri.into()),
                ))
            }
            None => Ok(ElementRef::with_namespace(
                self.clone(),
                qualified_name.into(),
                namespace_uri.map(|uri| uri.into()),
            )),
        }
    }

    /// Implementation of [`createAttributeNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-DocCrAttrNS) method.
    ///
    /// # Specification
    /// ```text
    /// Creates an attribute of the given qualified name and namespace URI.
    /// Per [XML Namespaces], applications must use the value null as the namespaceURI
    /// parameter for methods if they wish to have no namespace.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the attribute to create.
    ///     qualifiedName of type DOMString
    ///         The qualified name of the attribute to instantiate.
    ///
    /// Return Value
    ///     Attr A new Attr object with the following attributes:
    ///          (ommited)
    ///
    /// Exceptions
    ///     DOMException
    ///     INVALID_CHARACTER_ERR: Raised if the specified qualifiedName is not an XML name
    ///                            according to the XML version in use specified in the
    ///                            Document.xmlVersion attribute.
    ///     NAMESPACE_ERR:         Raised if the qualifiedName is a malformed qualified name,
    ///                            if the qualifiedName has a prefix and the namespaceURI is
    ///                            null, if the qualifiedName has a prefix that is "xml" and
    ///                            the namespaceURI is different from
    ///                            "http://www.w3.org/XML/1998/namespace", if the qualifiedName
    ///                            or its prefix is "xmlns" and the namespaceURI is different
    ///                            from "http://www.w3.org/2000/xmlns/", or if the namespaceURI
    ///                            is "http://www.w3.org/2000/xmlns/" and neither the
    ///                            qualifiedName nor its prefix is "xmlns".
    ///     NOT_SUPPORTED_ERR:     Always thrown if the current document does not support the
    ///                            "XML" feature, since namespaces were defined by XML.
    /// ```
    pub fn create_attribute_ns(
        &self,
        namespace_uri: Option<&str>,
        qualified_name: &str,
    ) -> Result<AttrRef, DOMException> {
        if validate_name::<false>(qualified_name).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }
        if validate_qname::<false>(qualified_name).is_err() {
            return Err(DOMException::NamespaceErr);
        }

        match split_qname2(qualified_name) {
            Some((prefix, _)) => {
                // ... if the qualifiedName has a prefix and the namespaceURI is null, ...
                let Some(ns_uri) = namespace_uri else {
                    return Err(DOMException::NamespaceErr);
                };

                // ... if the qualifiedName has a prefix that is "xml"
                // and the namespaceURI is different from "http://www.w3.org/XML/1998/namespace" ...
                if prefix == "xml" && ns_uri != XML_XML_NAMESPACE {
                    return Err(DOMException::NamespaceErr);
                }

                // ... if the qualifiedName or its prefix is "xmlns"
                // and the namespaceURI is different from "http://www.w3.org/2000/xmlns/" ...
                if prefix == "xmlns" && ns_uri != XML_NS_NAMESPACE {
                    return Err(DOMException::NamespaceErr);
                }

                // ... if the namespaceURI is "http://www.w3.org/2000/xmlns/"
                // and neither the qualifiedName nor its prefix is "xmlns".
                if ns_uri == XML_NS_NAMESPACE && prefix != "xmlns" {
                    return Err(DOMException::NamespaceErr);
                }

                Ok(AttrRef::with_namespace(
                    self.clone(),
                    qualified_name.into(),
                    ns_uri.into(),
                ))
            }
            None => {
                // ... if the qualifiedName or its prefix is "xmlns"
                // and the namespaceURI is different from "http://www.w3.org/2000/xmlns/" ...
                if qualified_name == "xmlns" && namespace_uri != Some(XML_NS_NAMESPACE) {
                    return Err(DOMException::NamespaceErr);
                }

                // ... if the namespaceURI is "http://www.w3.org/2000/xmlns/"
                // and neither the qualifiedName nor its prefix is "xmlns".
                if namespace_uri == Some(XML_NS_NAMESPACE) && qualified_name != "xmlns" {
                    return Err(DOMException::NamespaceErr);
                }

                if let Some(ns_uri) = namespace_uri {
                    Ok(AttrRef::with_namespace(
                        self.clone(),
                        qualified_name.into(),
                        ns_uri.into(),
                    ))
                } else {
                    Ok(AttrRef::new(self.clone(), qualified_name.into()))
                }
            }
        }
    }

    /// Implementation of [`getElementsByTagNameNS`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-getElBTNNS) method.
    ///
    /// # Specification
    /// ```text
    /// Returns a NodeList of all the Elements with a given local name and namespace URI
    /// in document order.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI of the elements to match on. The special value "*" matches
    ///         all namespaces.
    ///     localName of type DOMString
    ///         The local name of the elements to match on. The special value "*" matches
    ///         all local names.
    ///
    /// Return Value
    ///     NodeList A new NodeList object containing all the matched Elements.
    ///
    /// No Exceptions
    /// ```
    pub fn get_elements_by_tag_name_ns(
        &self,
        namespace_uri: Option<&str>,
        local_name: &str,
    ) -> FilteredSubtreeElementsList {
        // If this document is HTML document, the tagname is case-insensitive.
        if self.is_html() {
            FilteredSubtreeElementsList::new(
                self.clone().into(),
                namespace_uri.map(|uri| uri.to_owned()),
                local_name.to_owned(),
                |elem, uri, local_name| {
                    (local_name == "*"
                        || match elem.local_name() {
                            Some(ln) => ln.eq_ignore_ascii_case(local_name),
                            None => elem.node_name().eq_ignore_ascii_case(local_name),
                        })
                        && (uri == Some("*")
                            || match (elem.namespace_uri(), uri) {
                                (Some(elem_ns), Some(uri)) if elem_ns.eq_ignore_ascii_case(uri) => {
                                    true
                                }
                                (None, None) => true,
                                _ => false,
                            })
                },
            )
        } else {
            FilteredSubtreeElementsList::new(
                self.clone().into(),
                namespace_uri.map(|uri| uri.to_owned()),
                local_name.to_owned(),
                |elem, uri, local_name| {
                    (local_name == "*"
                        || match elem.local_name() {
                            Some(ln) => ln.as_ref() == local_name,
                            None => elem.node_name().as_ref() == local_name,
                        })
                        && (uri == Some("*")
                            || match (elem.namespace_uri(), uri) {
                                (Some(elem_ns), Some(uri)) if elem_ns.as_ref() == uri => true,
                                (None, None) => true,
                                _ => false,
                            })
                },
            )
        }
    }

    /// Implementation of [`getElementById`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-getElBId) method.
    ///
    /// # Specification
    /// ```text
    /// Returns the Element that has an ID attribute with the given value.
    /// If no such element exists, this returns null. If more than one element has
    /// an ID attribute with that value, what is returned is undefined.
    /// The DOM implementation is expected to use the attribute Attr.isId to determine
    /// if an attribute is of type ID.
    ///
    /// Note: Attributes with the name "ID" or "id" are not of type ID unless so defined.
    ///
    /// Parameters
    ///     elementId of type DOMString
    ///         The unique id value for an element.
    ///
    /// Return Value
    ///     Element The matching element or null if there is none.
    ///
    /// No Exceptions
    /// ```
    pub fn get_element_by_id(&self, element_id: impl Into<Rc<str>>) -> Option<ElementRef> {
        let element_id: Rc<str> = element_id.into();
        let mut children = self.first_child();
        while let Some(child) = children {
            if let NodeRef::Element(elem) = &child {
                let attrs = elem.attributes();
                for i in 0..attrs.length() {
                    let attr = attrs.item(i).unwrap();
                    if attr.is_id() && attr.node_value().as_deref() == Some(&element_id) {
                        return Some(elem.clone());
                    }
                }
            }

            if let Some(first) = child.first_child() {
                children = Some(first);
            } else if let Some(next) = child.next_sibling() {
                children = Some(next);
            } else {
                children = child.parent_node();
                while let Some(par) = children {
                    if let Some(next) = par.next_sibling() {
                        children = Some(next);
                        break;
                    }
                    children = par.parent_node();
                }
            }
        }
        None
    }

    /// Implementation of [`adoptNode`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Document3-adoptNode) method.
    ///
    /// # Specification
    /// ```text
    /// Attempts to adopt a node from another document to this document. If supported,
    /// it changes the ownerDocument of the source node, its children, as well as the
    /// attached attribute nodes if there are any. If the source node has a parent it
    /// is first removed from the child list of its parent. This effectively allows moving
    /// a subtree from one document to another (unlike importNode() which create a copy
    /// of the source node instead of moving it). When it fails, applications should
    /// use Document.importNode() instead. Note that if the adopted node is already part
    /// of this document (i.e. the source and target document are the same), this method
    /// still has the effect of removing the source node from the child list of its parent,
    /// if any. The following list describes the specifics for each type of node.
    ///
    /// <too long...please refer to the specification>
    ///
    /// Note: Since it does not create new nodes unlike the Document.importNode() method,
    ///       this method does not raise an INVALID_CHARACTER_ERR exception, and applications
    ///       should use the Document.normalizeDocument() method to check if an imported
    ///       name is not an XML name according to the XML version in use.
    ///
    /// Parameters
    ///     source of type Node
    ///         The node to move into this document.
    ///
    /// Return Value
    ///     Node The adopted node, or null if this operation fails, such as when the source
    ///          node comes from a different implementation.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_SUPPORTED_ERR:           Raised if the source node is of type DOCUMENT,
    ///                                  DOCUMENT_TYPE.
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised when the source node is readonly.
    /// ```
    pub fn adopt_node(&mut self, mut source: NodeRef) -> Result<NodeRef, DOMException> {
        if matches!(
            source.node_type(),
            NodeType::Document | NodeType::DocumentType
        ) {
            return Err(DOMException::NotSupportedErr);
        }

        if let Some(mut parent) = source.parent_node() {
            parent.remove_child(source.clone())?;
        }

        if !check_owner_document_sameness(self, &source) {
            return Ok(source);
        }

        source.adopted_to(self.clone());
        Ok(source)
    }

    /// Implementation of [`normalizeDocument`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Document3-normalizeDocument) method.
    ///
    /// # Specification
    /// ```text
    /// This method acts as if the document was going through a save and load cycle,
    /// putting the document in a "normal" form. As a consequence, this method updates the
    /// replacement tree of EntityReference nodes and normalizes Text nodes, as defined in
    /// the method Node.normalize().
    /// Otherwise, the actual result depends on the features being set on the
    /// Document.domConfig object and governing what operations actually take place.
    /// Noticeably this method could also make the document namespace well-formed according
    /// to the algorithm described in Namespace Normalization, check the character
    /// normalization, remove the CDATASection nodes, etc. See DOMConfiguration for details.
    ///
    /// <..snip..>
    ///
    /// Mutation events, when supported, are generated to reflect the changes occurring
    /// onthe document.
    /// If errors occur during the invocation of this method, such as an attempt to update
    /// a read-only node or a Node.nodeName contains an invalid character according to
    /// the XML version in use, errors or warnings (DOMError.SEVERITY_ERROR or
    /// DOMError.SEVERITY_WARNING) will be reported using the DOMErrorHandler object
    /// associated with the "error-handler" parameter. Note this method might also report
    /// fatal errors (DOMError.SEVERITY_FATAL_ERROR) if an implementation cannot recover
    /// from an error.
    ///
    /// No Parameters
    /// No Return Value
    /// No Exceptions
    /// ```
    pub fn normalize_document(&mut self) {
        todo!()
    }

    /// Implementation of [`renameNode`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Document3-renameNode) method.
    ///
    /// # Specification
    /// ```text
    /// Rename an existing node of type ELEMENT_NODE or ATTRIBUTE_NODE.
    /// When possible this simply changes the name of the given node, otherwise this creates
    /// a new node with the specified name and replaces the existing node with the new node
    /// as described below.
    /// If simply changing the name of the given node is not possible, the following operations
    /// are performed: a new node is created, any registered event listener is registered on
    /// the new node, any user data attached to the old node is removed from that node, the old
    /// node is removed from its parent if it has one, the children are moved to the new node,
    /// if the renamed node is an Element its attributes are moved to the new node, the new
    /// node is inserted at the position the old node used to have in its parent's child nodes
    /// list if it has one, the user data that was attached to the old node is attached to
    /// the new node.
    /// When the node being renamed is an Element only the specified attributes are moved,
    /// default attributes originated from the DTD are updated according to the new element
    /// name.
    /// In addition, the implementation may update default attributes from other schemas.
    /// Applications should use Document.normalizeDocument() to guarantee these attributes are
    /// up-to-date.
    /// When the node being renamed is an Attr that is attached to an Element, the node is
    /// first removed from the Element attributes map. Then, once renamed, either by modifying
    /// the existing node or creating a new one as described above, it is put back.
    /// In addition,
    ///
    /// - a user data event NODE_RENAMED is fired,
    /// - when the implementation supports the feature "MutationNameEvents", each mutation
    ///   operation involved in this method fires the appropriate event, and in the end the
    ///   event {http://www.w3.org/2001/xml-events, DOMElementNameChanged}
    ///   or {http://www.w3.org/2001/xml-events, DOMAttributeNameChanged} is fired.
    ///
    /// Parameters
    ///     n of type Node
    ///         The node to rename.
    ///     namespaceURI of type DOMString
    ///         The new namespace URI.
    ///     qualifiedName of type DOMString
    ///         The new qualified name.
    ///
    /// Return Value
    ///     Node The renamed node. This is either the specified node or the new node that was
    ///          created to replace the specified node.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_SUPPORTED_ERR:     Raised when the type of the specified node is neither
    ///                            ELEMENT_NODE nor ATTRIBUTE_NODE, or if the implementation
    ///                            does not support the renaming of the document element.
    ///     INVALID_CHARACTER_ERR: Raised if the new qualified name is not an XML name
    ///                            according to the XML version in use specified in the
    ///                            Document.xmlVersion attribute.
    ///     WRONG_DOCUMENT_ERR:    Raised when the specified node was created from a different
    ///                            document than this document.
    ///     NAMESPACE_ERR:         Raised if the qualifiedName is a malformed qualified name,
    ///                            if the qualifiedName has a prefix and the namespaceURI is
    ///                            null, or if the qualifiedName has a prefix that is "xml" and
    ///                            the namespaceURI is different from
    ///                            "http://www.w3.org/XML/1998/namespace" [XML Namespaces].
    ///                            Also raised, when the node being renamed is an attribute,
    ///                            if the qualifiedName, or its prefix, is "xmlns" and the
    ///                            namespaceURI is different from
    ///                            "http://www.w3.org/2000/xmlns/".
    /// ```
    pub fn rename_node(
        &mut self,
        mut n: NodeRef,
        namespace_uri: Option<impl Into<Rc<str>>>,
        qualified_name: impl Into<Rc<str>>,
    ) -> Result<NodeRef, DOMException> {
        // TODO: handle HTML element and attribute
        if self.is_html() {
            return Err(DOMException::NotSupportedErr);
        }

        if !check_owner_document_sameness(self, &n) {
            return Err(DOMException::WrongDocumentErr);
        }

        let qname: Rc<str> = qualified_name.into();
        if validate_qname::<false>(&qname).is_err() {
            return Err(DOMException::InvalidCharacterErr);
        }

        let ns_uri: Option<Rc<str>> = namespace_uri.map(|uri| uri.into());
        match &mut n {
            NodeRef::Element(elem) => {
                match (split_qname2(&qname), ns_uri.as_deref()) {
                    (Some((pre, _)), Some(ns_uri)) => {
                        if pre == "xml" && ns_uri != XML_XML_NAMESPACE {
                            return Err(DOMException::NamespaceErr);
                        }
                    }
                    (Some(_), None) => {
                        return Err(DOMException::NamespaceErr);
                    }
                    _ => {}
                }
                let mut res = elem.clone();
                res.rename(qname, ns_uri);
                Ok(n)
            }
            NodeRef::Attribute(attr) => {
                let mut keep = None;
                if let Some(mut elem) = attr.owner_element() {
                    elem.remove_attribute_node(attr.clone())?;
                    keep = Some(elem);
                }

                match (split_qname2(&qname), ns_uri.as_deref()) {
                    (Some((pre, _)), Some(ns_uri)) => {
                        if pre == "xml" && ns_uri != XML_XML_NAMESPACE {
                            return Err(DOMException::NamespaceErr);
                        }
                        if pre == "xmlns" && ns_uri != XML_NS_NAMESPACE {
                            return Err(DOMException::NamespaceErr);
                        }
                    }
                    (Some(_), None) => {
                        return Err(DOMException::NamespaceErr);
                    }
                    (None, Some(ns_uri)) => {
                        if qname.as_ref() == "xmlns" && ns_uri != XML_NS_NAMESPACE {
                            return Err(DOMException::NamespaceErr);
                        }
                    }
                    (None, None) => {}
                }

                attr.rename(qname, ns_uri);
                if let Some(mut elem) = keep {
                    elem.set_attribute_node(attr.clone())?;
                }
                Ok(n)
            }
            _ => Err(DOMException::NotSupportedErr),
        }
    }

    /// Implementation of [`doctype`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-B63ED1A31) attribute.
    ///
    /// # Specification
    /// ```text
    /// doctype of type DocumentType, readonly, modified in DOM Level 3
    ///     The Document Type Declaration (see DocumentType) associated with this document.
    ///     For XML documents without a document type declaration this returns null.
    ///     For HTML documents, a DocumentType object may be returned, independently of the
    ///     presence or absence of document type declaration in the HTML document.
    ///     This provides direct access to the DocumentType node, child node of this Document.
    ///     This node can be set at document creation time and later changed through the use
    ///     of child nodes manipulation methods, such as Node.insertBefore,
    ///     or Node.replaceChild. Note, however, that while some implementations may
    ///     instantiate different types of Document objects supporting additional features
    ///     than the "Core", such as "HTML" [DOM Level 2 HTML], based on the DocumentType
    ///     specified at creation time, changing it afterwards is very unlikely to result
    ///     in a change of the features supported.
    /// ```
    pub fn doctype(&self) -> Option<DocumentTypeRef> {
        self.0.borrow().doctype.clone()
    }

    /// Implementation of [`implementation`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1B793EBA) attribute.
    ///
    /// # Specification
    /// ```text
    /// implementation of type DOMImplementation, readonly
    ///     The DOMImplementation object that handles this document. A DOM application may
    ///     use objects from multiple implementations.
    /// ```
    pub fn implementation(&self) -> Arc<dyn DOMImplementation> {
        self.0.borrow().implementation.clone()
    }

    /// Implementation of [`documentElement`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-87CD092) attribute.
    ///
    /// # Specification
    /// ```text
    /// documentElement of type Element, readonly
    ///     This is a convenience attribute that allows direct access to the child node
    ///     that is the document element of the document.
    /// ```
    pub fn document_element(&self) -> Option<ElementRef> {
        self.0.borrow().document_element.clone()
    }

    /// Implementation of [`inputEncoding`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Document3-inputEncoding) attribute.
    ///
    /// # Specification
    /// ```text
    /// inputEncoding of type DOMString, readonly, introduced in DOM Level 3
    ///     An attribute specifying the encoding used for this document at the time of
    ///     the parsing. This is null when it is not known, such as when the Document was
    ///     created in memory.
    /// ```
    pub fn input_encoding(&self) -> Option<Rc<str>> {
        self.0.borrow().input_encoding.clone()
    }

    /// Implementation of [`xmlEncoding`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Document3-encoding) attribute.
    ///
    /// # Specification
    /// ```text
    /// xmlEncoding of type DOMString, readonly, introduced in DOM Level 3
    ///     An attribute specifying, as part of the XML declaration, the encoding of
    ///     this document. This is null when unspecified or when it is not known, such as
    ///     when the Document was created in memory.
    /// ```
    pub fn xml_encoding(&self) -> Option<Rc<str>> {
        self.0.borrow().xml_encoding.clone()
    }

    /// Implementation of [`xmlStandalone`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Document3-standalone) attribute.
    ///
    /// # Specification
    /// ```text
    /// xmlStandalone of type boolean, introduced in DOM Level 3
    ///     An attribute specifying, as part of the XML declaration, whether this document
    ///     is standalone. This is false when unspecified.
    ///
    ///     Note: No verification is done on the value when setting this attribute.
    ///           Applications should use Document.normalizeDocument() with the "validate"
    ///           parameter to verify if the value matches the validity constraint for
    ///           standalone document declaration as defined in [XML 1.0].
    /// ```
    pub fn xml_standalone(&self) -> bool {
        self.0.borrow().xml_standalone == 1
    }

    /// Implementation of [`xmlStandalone`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Document3-standalone) attribute.
    ///
    /// # Specification
    /// ```text
    /// xmlStandalone of type boolean, introduced in DOM Level 3
    ///     An attribute specifying, as part of the XML declaration, whether this document
    ///     is standalone. This is false when unspecified.
    ///
    ///     Note: No verification is done on the value when setting this attribute.
    ///           Applications should use Document.normalizeDocument() with the "validate"
    ///           parameter to verify if the value matches the validity constraint for
    ///           standalone document declaration as defined in [XML 1.0].
    ///
    /// Exceptions on setting
    ///     DOMException
    ///     NOT_SUPPORTED_ERR: Raised if this document does not support the "XML" feature.
    /// ```
    pub fn set_xml_standalone(&mut self, standalone: bool) -> Result<(), DOMException> {
        if !self.implementation().has_feature("XML", None) {
            return Err(DOMException::NotSupportedErr);
        }
        self.0.borrow_mut().xml_standalone = standalone as i8;
        Ok(())
    }

    /// Implementation of [`xmlVersion`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Document3-version) attribute.
    ///
    /// # Specification
    /// ```text
    /// An attribute specifying, as part of the XML declaration, the version numberof
    /// this document. If there is no declaration and if this document supports the
    /// "XML" feature, the value is "1.0". If this document does not support the "XML" feature,
    /// the value is always null. Changing this attribute will affect methods that check
    /// for invalid characters in XML names. Application should invoke
    /// Document.normalizeDocument() in order to check for invalid characters in the Nodes
    /// that are already part of this Document.
    /// DOM applications may use the DOMImplementation.hasFeature(feature, version) method
    /// with parameter values "XMLVersion" and "1.0" (respectively) to determine if
    /// an implementation supports [XML 1.0]. DOM applications may use the same method
    /// with parameter values "XMLVersion" and "1.1" (respectively) to determine if
    /// an implementation supports [XML 1.1]. In both cases, in order to support XML,
    /// an implementation must also support the "XML" feature defined in this specification.
    /// Document objects supporting a version of the "XMLVersion" feature must not raise
    /// a NOT_SUPPORTED_ERR exception for the same version number when using Document.xmlVersion.
    /// ```
    pub fn xml_version(&self) -> Option<Rc<str>> {
        if let Some(version) = self.0.borrow().xml_version.clone() {
            Some(version)
        } else if self.implementation().has_feature("XML", None) {
            Some("1.0".into())
        } else {
            None
        }
    }

    /// Implementation of [`xmlVersion`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Document3-version) attribute.
    ///
    /// # Specification
    /// ```text
    /// An attribute specifying, as part of the XML declaration, the version numberof
    /// this document. If there is no declaration and if this document supports the
    /// "XML" feature, the value is "1.0". If this document does not support the "XML"
    /// feature, the value is always null. Changing this attribute will affect methods
    /// that check for invalid characters in XML names. Application should invoke
    /// Document.normalizeDocument() in order to check for invalid characters in the Nodes
    /// that are already part of this Document.
    /// DOM applications may use the DOMImplementation.hasFeature(feature, version) method
    /// with parameter values "XMLVersion" and "1.0" (respectively) to determine if
    /// an implementation supports [XML 1.0]. DOM applications may use the same method
    /// with parameter values "XMLVersion" and "1.1" (respectively) to determine if
    /// an implementation supports [XML 1.1]. In both cases, in order to support XML,
    /// an implementation must also support the "XML" feature defined in this specification.
    /// Document objects supporting a version of the "XMLVersion" feature must not raise
    /// a NOT_SUPPORTED_ERR exception for the same version number when using Document.xmlVersion.
    ///
    /// Exceptions on setting
    ///     DOMException
    ///     NOT_SUPPORTED_ERR: Raised if the version is set to a value that is not supported
    ///                        by this Document or if this document does not support the
    ///                        "XML" feature.
    /// ```
    pub fn set_xml_version(&mut self, xml_version: &str) -> Result<(), DOMException> {
        if !self.implementation().has_feature("XML", None)
            || !self
                .implementation()
                .has_feature("XMLVersion", Some(xml_version))
        {
            return Err(DOMException::NotSupportedErr);
        }
        self.0.borrow_mut().xml_version = Some(xml_version.into());
        Ok(())
    }

    /// Implementation of [`documentURI`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Document3-documentURI) attribute.
    ///
    /// # Specification
    /// ```text
    /// documentURI of type DOMString, introduced in DOM Level 3
    ///     The location of the document or null if undefined or if the Document was created
    ///     using DOMImplementation.createDocument. No lexical checking is performed when
    ///     setting this attribute; this could result in a null value returned when using
    ///     Node.baseURI.
    ///     Beware that when the Document supports the feature "HTML" [DOM Level 2 HTML],
    ///     the href attribute of the HTML BASE element takes precedence over this attribute
    ///     when computing Node.baseURI.
    /// ```
    pub fn document_uri(&self) -> Option<Rc<str>> {
        self.0.borrow().document_uri.clone()
    }

    /// Do an entity lookup in the document entity hash table and
    /// returns the corresponding entity, otherwise a lookup is done
    /// in the predefined entities too.
    ///
    /// If found, return found entity wrapped `Some`.  
    /// Otherwise, return `None`.
    #[doc(alias = "xmlGetDocEntity")]
    pub fn get_entity(&self, name: &str) -> Option<EntityRef> {
        self.0.borrow().get_entity(name)
    }

    pub(super) fn get_default_attribute(
        &self,
        elem_name: &str,
        attr_name: &str,
    ) -> Option<AttrRef> {
        let elemdecl = self.doctype()?.get_element_decl(elem_name)?;
        let attlistdecl = elemdecl.get_attribute_decl(attr_name)?;
        match attlistdecl.default_decl() {
            DefaultDecl::None(def) | DefaultDecl::Fixed(def) => {
                let mut attr = self.create_attribute(attr_name).ok()?;
                attr.set_value(def.as_ref()).ok()?;
                attr.set_specified(false);
                Some(attr)
            }
            _ => None,
        }
    }

    pub(super) fn get_default_attribute_ns(
        &self,
        context_node: ElementRef,
        attr_name: &str,
    ) -> Option<AttrRef> {
        let elemdecl = self
            .doctype()?
            .get_element_decl(&context_node.node_name())?;
        let attlistdecl = elemdecl.get_attribute_decl(attr_name)?;
        match attlistdecl.default_decl() {
            DefaultDecl::None(def) | DefaultDecl::Fixed(def) => {
                let mut attr = if let Some(namespace_uri) = split_qname2(attr_name)
                    .and_then(|(pre, _)| context_node.lookup_namespace_uri(Some(pre)))
                {
                    self.create_attribute_ns(Some(&namespace_uri), attr_name)
                        .ok()?
                } else {
                    self.create_attribute_ns(None, attr_name).ok()?
                };
                attr.set_value(def.as_ref()).ok()?;
                attr.set_specified(false);
                Some(attr)
            }
            _ => None,
        }
    }

    pub(super) fn get_default_attributes(&self, elem_name: &str) -> Option<Vec<AttrRef>> {
        let elemdecl = self.doctype()?.get_element_decl(elem_name)?;
        let mut res = vec![];
        for decl in elemdecl.get_attribute_decls() {
            match decl.default_decl() {
                DefaultDecl::None(def) | DefaultDecl::Fixed(def) => {
                    let mut attr = self.create_attribute(decl.name()).ok()?;
                    attr.set_value(def.as_ref()).ok()?;
                    attr.set_specified(false);

                    // Due to the requirements of the XML specification, attributes
                    // with default values are not ID attributes, so it is not necessary
                    // to check whether they should be set as ID attributes.
                    //
                    // 3.3.1 Attribute Types
                    // [Validity constraint: ID Attribute Default]
                    // An ID attribute MUST have a declared default of #IMPLIED or #REQUIRED.

                    res.push(attr);
                }
                _ => {}
            }
        }
        Some(res)
    }

    /// Check if this is HTML document.
    pub fn is_html(&self) -> bool {
        self.0.borrow().html
    }

    /// Enable check for read-only node.\
    /// As a result, editing of nodes specified as read-only in the DOM specification
    /// becomes impossible.
    pub fn enable_read_only_check(&mut self) {
        self.0.borrow_mut().enable_read_only_check();
    }

    /// Disable check for read-only node.\
    /// It allows editing of nodes that are not editable in the DOM specification
    /// (e.g., DTD nodes).
    pub fn disable_read_only_check(&mut self) {
        self.0.borrow_mut().disable_read_only_check();
    }

    /// Check if read-only check is enabled.
    pub fn is_enabled_read_only_check(&self) -> bool {
        self.0.borrow().is_enabled_read_only_check()
    }

    /// Generate [`DocumentWeakRef`] from `self`.
    pub fn downgrade(&self) -> DocumentWeakRef {
        DocumentWeakRef(Rc::downgrade(&self.0))
    }
}

impl Node for DocumentRef {
    fn node_name(&self) -> Rc<str> {
        "#document".into()
    }

    fn node_value(&self) -> Option<Rc<str>> {
        None
    }

    fn set_node_value(&mut self, _: impl Into<String>) -> Result<(), DOMException> {
        Ok(())
    }

    fn node_type(&self) -> NodeType {
        NodeType::Document
    }

    fn first_child(&self) -> Option<NodeRef> {
        self.0.borrow().first_child.clone()
    }

    fn last_child(&self) -> Option<NodeRef> {
        self.0.borrow().last_child.clone()
    }

    fn insert_before(
        &mut self,
        mut new_child: NodeRef,
        ref_child: Option<NodeRef>,
    ) -> Result<NodeRef, DOMException> {
        // In this implementation, if `new_child` and `ref_child` are same node,
        // do nothing and return `new_child`.
        if ref_child
            .as_ref()
            .is_some_and(|ref_child| new_child.is_same_node(ref_child))
        {
            return Ok(new_child);
        }

        // HIERARCHY_REQUEST_ERR: Raised if this node is of a type that does not allow children
        // of the type of the newChild node (..snip)
        if new_child.node_type() == NodeType::DocumentFragment {
            let mut children = new_child.first_child();
            while let Some(child) = children {
                children = child.next_sibling();
                if !check_vertical_hierarchy(self.node_type(), child.node_type()) {
                    return Err(DOMException::HierarchyRequestErr);
                }
                match child.node_type() {
                    NodeType::Element if self.document_element().is_some() => {
                        return Err(DOMException::HierarchyRequestErr);
                    }
                    NodeType::DocumentType if self.doctype().is_some() => {
                        return Err(DOMException::HierarchyRequestErr);
                    }
                    _ => {}
                }
            }
        } else if !check_vertical_hierarchy(self.node_type(), new_child.node_type()) {
            return Err(DOMException::HierarchyRequestErr);
        }
        // NOT_FOUND_ERR: Raised if refChild is not a child of this node.
        if ref_child.as_ref().is_some_and(|ref_child| {
            ref_child
                .parent_node()
                .is_none_or(|par| !self.is_same_node(&par))
        }) {
            return Err(DOMException::NotFoundErr);
        }
        // WRONG_DOCUMENT_ERR: Raised if newChild was created from a different document
        // than the one that created this node.
        if !check_owner_document_sameness(self, &new_child) {
            return Err(DOMException::WrongDocumentErr);
        }

        // HIERARCHY_REQUEST_ERR: Raised if (..snip..) this node is of type Document
        // and the DOM application attempts to insert a second DocumentType or Element node.
        match new_child.node_type() {
            NodeType::Element => {
                if self.document_element().is_some() {
                    return Err(DOMException::HierarchyRequestErr);
                }
                // If `new_child` is an Element that passes all checks, it is a document element.
                self.0.borrow_mut().document_element = new_child.as_element();
            }
            NodeType::DocumentType => {
                if self.doctype().is_some() {
                    return Err(DOMException::HierarchyRequestErr);
                }
                // The DocumentType that passed the check should have a dummy Document set,
                // so set the owner Document here to `self`.
                new_child.set_owner_document(self.clone());
                // and set `new_child` as the `doctype` of this document.
                self.0.borrow_mut().doctype = new_child.as_document_type();
            }
            _ => {}
        }

        if let Some(ref_child) = ref_child {
            match &mut new_child {
                // If newChild is a DocumentFragment object, all of its children are inserted,
                // in the same order, before refChild.
                NodeRef::DocumentFragment(frag) => {
                    let mut child = frag.first_child();
                    while let Some(mut now) = child {
                        child = now.next_sibling();
                        now.connect_as_previous_sibling(ref_child.clone());
                    }
                }
                other => {
                    other.connect_as_previous_sibling(ref_child);
                }
            }
        } else {
            let mut children = match new_child.clone() {
                NodeRef::DocumentFragment(frag) => frag.first_child(),
                mut other => {
                    other.disconnect_parent_and_sibling();
                    Some(other)
                }
            };
            while let Some(mut child) = children {
                children = child.next_sibling();
                child.set_parent_node(Some(self.clone().into()));
                let last = self.set_last_child(Some(child.clone()));
                if let Some(mut last) = last {
                    // If old last child is found,
                    // it is set as the previous sibling of `new_child`.
                    last.set_next_sibling(Some(child.clone()));
                    child.set_previous_sibling(Some(last));
                } else {
                    // Otherwise, `self` has no children.
                    // Therefore, I need to set `new_child` as also the first child of `self`.
                    self.set_first_child(Some(child.clone()));
                }
            }
        }

        Ok(new_child)
    }

    fn clone_node(&self, deep: bool) -> NodeRef {
        let doctype = self.doctype().map(|doctype| {
            let NodeRef::DocumentType(doctype) = doctype.clone_node(deep) else {
                unreachable!()
            };
            doctype
        });
        let document_element = self.document_element().map(|elem| {
            let NodeRef::Element(elem) = elem.clone_node(deep) else {
                unreachable!()
            };
            elem
        });
        let mut doc = DocumentRef(Rc::new(RefCell::new(Document {
            first_child: None,
            last_child: None,
            doctype,
            document_element,
            implementation: self.0.borrow().implementation.clone(),
            document_uri: self.0.borrow().document_uri.clone(),
            input_encoding: self.0.borrow().input_encoding.clone(),
            xml_encoding: self.0.borrow().xml_encoding.clone(),
            xml_standalone: self.0.borrow().xml_standalone,
            xml_version: self.0.borrow().xml_version.clone(),
            html: self.0.borrow().html,
            flag: self.0.borrow().flag,
            predefined_entities: self.0.borrow().predefined_entities.clone(),
        })));
        for (i, ent) in self
            .0
            .borrow()
            .predefined_entities
            .iter()
            .cloned()
            .enumerate()
        {
            let new = doc.import_node(ent.into(), true).expect("Internal Error");
            doc.0.borrow_mut().predefined_entities[i] = new.as_entity().unwrap();
        }

        if deep {
            let mut children = self.first_child();
            while let Some(child) = children {
                children = child.next_sibling();
                let new = doc.import_node(child, deep).expect("Internal Error");
                doc.append_child(new).expect("Internal Error");
            }
        }

        doc.into()
    }

    fn text_content(&self) -> Option<String> {
        None
    }

    fn set_text_content(&mut self, _text: impl Into<String>) -> Result<(), DOMException> {
        Ok(())
    }

    fn is_same_node(&self, other: &NodeRef) -> bool {
        let NodeRef::Document(other) = other else {
            return false;
        };
        Rc::ptr_eq(&self.0, &other.0)
    }

    fn lookup_prefix(&self, ns_uri: &str) -> Option<Rc<str>> {
        self.document_element()
            .and_then(|elem| elem.lookup_prefix(ns_uri))
    }

    fn is_default_namespace(&self, ns_uri: &str) -> bool {
        self.document_element()
            .map(|elem| elem.is_default_namespace(ns_uri))
            .unwrap_or(false)
    }

    fn lookup_namespace_uri(&self, prefix: Option<&str>) -> Option<Rc<str>> {
        self.document_element()?.lookup_namespace_uri(prefix)
    }

    fn is_read_only(&self) -> bool {
        false
    }
}

impl NodeConnection for DocumentRef {
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

    fn set_owner_document(&mut self, _: DocumentRef) -> Option<DocumentRef> {
        None
    }

    fn adopted_to(&mut self, _new_doc: DocumentRef) {
        // `Document` nodes cannot be adopted.
    }
}

impl From<DocumentRef> for NodeRef {
    fn from(value: DocumentRef) -> Self {
        NodeRef::Document(value)
    }
}

impl From<Rc<RefCell<Document>>> for DocumentRef {
    fn from(value: Rc<RefCell<Document>>) -> Self {
        Self(value)
    }
}

/// Wrapper of `Weak<RefCell<Document>>`.
#[derive(Clone)]
pub struct DocumentWeakRef(Weak<RefCell<Document>>);

impl DocumentWeakRef {
    /// Generate [`DocumentRef`] from `self`.  
    /// Success conditions are the same as for [`std::rc::Weak::upgrade`].
    pub fn upgrade(&self) -> Option<DocumentRef> {
        self.0.upgrade().map(DocumentRef)
    }
}

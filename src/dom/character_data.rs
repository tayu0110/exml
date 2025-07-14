use std::{
    cell::RefCell,
    collections::HashMap,
    mem::replace,
    ops::{Deref, DerefMut},
    rc::{Rc, Weak},
    sync::Arc,
};

use crate::chvalid::XmlCharValid;

use super::{
    DOMException, NodeType, check_no_modification_allowed_err,
    document::{Document, DocumentRef},
    node::{Node, NodeConnection, NodeRef, NodeWeakRef},
    user_data::{DOMUserData, OperationType, UserDataHandler},
};

/// Implementation of [CharacterData](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-FF21A306)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
///
/// Strings are encoded in UTF-8, not UTF-16.\
/// Therefore, errors related to string boundaries are subject to the constraints of UTF-8.
pub trait CharacterData: Node {
    /// Implementation of [`data`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-72AB8359) attribute.
    ///
    /// # Specification
    /// ```text
    /// The character data of the node that implements this interface. The DOM implementation
    /// may not put arbitrary limits on the amount of data that may be stored in a
    /// CharacterData node. However, implementation limits may mean that the entirety of a
    /// node's data may not fit into a single DOMString. In such cases, the user may call
    /// substringData to retrieve the data in appropriately sized pieces.
    ///
    /// Exceptions on retrieval
    ///     DOMException
    ///     DOMSTRING_SIZE_ERR: Raised when it would return more characters than fit in a
    ///                         DOMString variable on the implementation platform.
    /// ```
    fn data(&self) -> String;

    /// Implementation of [`data`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-72AB8359) attribute.
    ///
    /// # Specification
    /// ```text
    /// The character data of the node that implements this interface. The DOM implementation
    /// may not put arbitrary limits on the amount of data that may be stored in a
    /// CharacterData node. However, implementation limits may mean that the entirety of a
    /// node's data may not fit into a single DOMString. In such cases, the user may call
    /// substringData to retrieve the data in appropriately sized pieces.
    ///
    /// Exceptions on setting
    ///     DOMException
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
    /// ```
    fn set_data(&mut self, data: impl Into<String>) -> Result<(), DOMException>;

    /// Implementation of [`length`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-7D61178C) attribute.
    ///
    /// # Note
    /// Unlike the specification,
    /// this implementation returns **the number of bytes in a UTF-8 string**.
    ///
    /// # Specification
    /// ```text
    /// length of type unsigned long, readonly
    ///     The number of 16-bit units that are available through data
    ///     and the substringData method below. This may have the value zero,
    ///     i.e., CharacterData nodes may be empty.
    /// ```
    fn length(&self) -> usize {
        self.data().len()
    }

    /// Implementation of [`substringData`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-6531BCCF).
    ///
    /// # Note
    /// Unlike the specification, `offset` and `count` is **the number of bytes in a UTF-8 string**.
    ///
    /// # Specification
    /// ```text
    /// Extracts a range of data from the node.
    ///
    /// Parameters
    ///     offset of type unsigned long
    ///         Start offset of substring to extract.
    ///     count of type unsigned long
    ///         The number of 16-bit units to extract.
    ///
    /// Return Value
    ///     DOMString The specified substring. If the sum of offset and count exceeds the
    ///               length, then all 16-bit units to the end of the data are returned.
    ///
    /// Exceptions
    ///     DOMException
    ///     INDEX_SIZE_ERR:     Raised if the specified offset is negative or greater than
    ///                         the number of 16-bit units in data, or if the specified
    ///                         count is negative.
    ///     DOMSTRING_SIZE_ERR: Raised if the specified range of text does not fit into
    ///                         a DOMString.
    /// ```
    fn substring_data(&self, offset: usize, count: usize) -> Result<String, DOMException> {
        let data = self.data();
        let end = data.len().min(offset + count);
        if !data.is_char_boundary(offset) || !data.is_char_boundary(end) {
            return Err(DOMException::IndexSizeErr);
        }
        Ok(data[offset..end].to_owned())
    }

    /// Implementation of [`appendData`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-32791A2F) method.
    ///
    /// # Specification
    /// ```text
    /// Append the string to the end of the character data of the node. Upon success,
    /// data provides access to the concatenation of data and the DOMString specified.
    ///
    /// Parameters
    ///     arg of type DOMString
    ///         The DOMString to append.
    ///
    /// Exceptions
    ///     DOMException
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///
    /// No Return Value
    /// ```
    fn append_data(&mut self, arg: &str) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;
        let mut data = self.data();
        data.push_str(arg);
        self.set_data(data)
    }
    /// Implementation of [`insertData`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-3EDB695F) method.
    ///
    /// # Specification
    /// ```text
    /// Insert a string at the specified 16-bit unit offset.
    ///
    /// Parameters
    ///     offset of type unsigned long
    ///         The character offset at which to insert.
    ///     arg of type DOMString
    ///         The DOMString to insert.
    ///
    /// Exceptions
    ///     DOMException
    ///     INDEX_SIZE_ERR:              Raised if the specified offset is negative or greater
    ///                                  than the number of 16-bit units in data.
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///
    /// No Return Value
    /// ```
    fn insert_data(&mut self, offset: usize, arg: &str) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;
        let mut data = self.data();
        if !data.is_char_boundary(offset) {
            return Err(DOMException::IndexSizeErr);
        }
        data.insert_str(offset, arg);
        self.set_data(data)
    }
    /// Implementation of [`deleteData`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-7C603781) method.
    ///
    /// # Specification
    /// ```text
    /// Remove a range of 16-bit units from the node. Upon success, data and length reflect
    /// the change.
    ///
    /// Parameters
    ///     offset of type unsigned long
    ///         The offset from which to start removing.
    ///     count of type unsigned long
    ///         The number of 16-bit units to delete. If the sum of offset and count exceeds
    ///         length then all 16-bit units from offset to the end of the data are deleted.
    ///
    /// Exceptions
    ///     DOMException
    ///     INDEX_SIZE_ERR:              Raised if the specified offset is negative or greater
    ///                                  than the number of 16-bit units in data, or if the
    ///                                  specified count is negative.
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///
    /// No Return Value
    /// ```
    fn delete_data(&mut self, offset: usize, count: usize) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;
        let mut data = self.data();
        let end = data.len().min(offset + count);
        if !data.is_char_boundary(offset) || !data.is_char_boundary(end) {
            return Err(DOMException::IndexSizeErr);
        }
        data.drain(offset..end);
        self.set_data(data)
    }
    /// Implementation of [`replaceData`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-E5CBA7FB) method.
    ///
    /// # Specification
    /// ```text
    /// Replace the characters starting at the specified 16-bit unit offset with the
    /// specified string.
    ///
    /// Parameters
    ///     offset of type unsigned long
    ///         The offset from which to start replacing.
    ///     count of type unsigned long
    ///         The number of 16-bit units to replace. If the sum of offset and count exceeds
    ///         length, then all 16-bit units to the end of the data are replaced; (i.e., the
    ///         effect is the same as a remove method call with the same range, followed by
    ///         an append method invocation).
    ///     arg of type DOMString
    ///         The DOMString with which the range must be replaced.
    ///
    /// Exceptions
    ///     DOMException
    ///     INDEX_SIZE_ERR:              Raised if the specified offset is negative or greater
    ///                                  than the number of 16-bit units in data, or if the
    ///                                  specified count is negative.
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    ///
    /// No Return Value
    /// ```
    fn replace_data(&mut self, offset: usize, count: usize, arg: &str) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;
        let mut data = self.data();
        let end = data.len().min(offset + count);
        if !data.is_char_boundary(offset) || !data.is_char_boundary(end) {
            return Err(DOMException::IndexSizeErr);
        }
        data.replace_range(offset..end, arg);
        self.set_data(data)
    }
}

/// Implementation of [Text](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1312295772)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
///
/// Strings are encoded in UTF-8. Unlike the specification, methods that specify string
/// boundaries are constrained to be UTF-8 character boundaries.
pub struct Text {
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    /// - `DocumentFragment`
    /// - `EntityReference`
    /// - `Element`
    /// - `Attr`
    /// - `Entity`
    parent_node: Option<NodeWeakRef>,
    // /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    // /// - no children
    // first_child: Option<NodeRef>,
    // last_child: Option<NodeRef>,
    /// [1.1.1 The DOM Structure Model](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1590626202)
    /// - `EntityReference`
    /// - `Element`
    /// - `ProcessingInstruction`
    /// - `Comment`
    /// - `Text`
    /// - `CDATASection`
    previous_sibling: Option<NodeWeakRef>,
    next_sibling: Option<NodeRef>,
    owner_document: Weak<RefCell<Document>>,
    data: String,

    user_data: Option<HashMap<String, (DOMUserData, Option<Arc<dyn UserDataHandler>>)>>,

    // 0      : read-only ?
    // 1 - 15 : unused
    flag: u16,
}

impl Text {
    pub fn owner_document(&self) -> Option<DocumentRef> {
        self.owner_document.upgrade().map(From::from)
    }

    fn adopted_to(&mut self, new_doc: DocumentRef) {
        self.owner_document = Rc::downgrade(&new_doc.0);
    }
}

/// Wrapper of [`Rc<RefCell<Text>>`].
///
/// Strings are encoded in UTF-8. Unlike the specification, methods that specify string
/// boundaries are constrained to be UTF-8 character boundaries.
#[derive(Clone)]
pub struct TextRef(pub(super) Rc<RefCell<Text>>, pub(super) DocumentRef);

impl TextRef {
    /// Implementation of [`splitText`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-38853C1D) method.
    ///
    /// # Specification
    /// ```text
    /// Breaks this node into two nodes at the specified offset, keeping both in the tree as
    /// siblings. After being split, this node will contain all the content up to the offset
    /// point. A new node of the same type, which contains all the content at and after the
    /// offset point, is returned. If the original node had a parent node, the new node is
    /// inserted as the next sibling of the original node. When the offset is equal to the
    /// length of this node, the new node has no data.
    ///
    /// Parameters
    ///     offset of type unsigned long
    ///         The 16-bit unit offset at which to split, starting from 0.
    ///
    /// Return Value
    /// Text
    /// The new node, of the same type as this node.
    ///
    /// Exceptions
    /// DOMException
    /// INDEX_SIZE_ERR:              Raised if the specified offset is negative or greater
    ///                              than the number of 16-bit units in data.
    /// NO_MODIFICATION_ALLOWED_ERR: Raised if this node is readonly.
    /// ```
    pub fn split_text(&mut self, offset: usize) -> Result<TextRef, DOMException> {
        check_no_modification_allowed_err(self)?;

        if !self.0.borrow().data.is_char_boundary(offset) {
            return Err(DOMException::IndexSizeErr);
        }

        let back = self.0.borrow_mut().data.split_off(offset);
        let res = Self::from_doc(self.owner_document().expect("Internal Error"), back);
        if let Some(mut parent) = self.parent_node() {
            parent
                .insert_before(res.clone().into(), self.next_sibling())
                .ok();
        }
        Ok(res)
    }

    /// Implementation of [`replaceWholeText`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Text3-replaceWholeText) method.
    ///
    /// # Specification
    /// ```text
    /// Replaces the text of the current node and all logically-adjacent text nodes with the
    /// specified text. All logically-adjacent text nodes are removed including the current
    /// node unless it was the recipient of the replacement text.
    /// This method returns the node which received the replacement text. The returned node is:
    ///
    /// - null, when the replacement text is the empty string;
    /// - the current node, except when the current node is read-only;
    /// - a new Text node of the same type (Text or CDATASection) as the current node inserted
    ///   at the location of the replacement.
    ///
    ///
    /// For instance, in the above example calling replaceWholeText on the Text node that
    /// contains "bar" with "yo" in argument results in the following:
    ///
    /// Figure: barTextNode.replaceWholeText("yo") modifies the textual content of barTextNode with "yo" [SVG 1.0 version]
    ///
    /// Where the nodes to be removed are read-only descendants of an EntityReference, the
    /// EntityReference must be removed instead of the read-only nodes. If any EntityReference
    /// to be removed has descendants that are not EntityReference, Text, or CDATASection
    /// nodes, the replaceWholeText method must fail before performing any modification of the
    /// document, raising a DOMException with the code NO_MODIFICATION_ALLOWED_ERR.
    /// For instance, in the example below calling replaceWholeText on the Text node that
    /// contains "bar" fails, because the EntityReference node "ent" contains an Element node
    /// which cannot be removed.
    ///
    /// Figure: barTextNode.replaceWholeText("yo") raises a NO_MODIFICATION_ALLOWED_ERR DOMException [SVG 1.0 version]
    ///
    /// Parameters
    ///     content of type DOMString
    ///         The content of the replacing Text node.
    ///
    /// Return Value
    ///     Text The Text node created with the specified content.
    ///
    /// Exceptions
    ///     DOMException
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised if one of the Text nodes being replaced is
    ///                                  readonly.
    /// ```
    pub fn replace_whole_text(&mut self, content: &str) -> Result<Option<TextRef>, DOMException> {
        // reference:
        // https://github.com/apache/xerces2-j/blob/trunk/src/org/apache/xerces/dom/TextImpl.java
        fn has_text_only_children(node: Option<NodeRef>) -> bool {
            let Some(child) = node else {
                return false;
            };
            let mut child = child.first_child();
            while let Some(ch) = child {
                match ch.node_type() {
                    NodeType::EntityReference => {
                        if !has_text_only_children(Some(ch.clone())) {
                            return false;
                        }
                    }
                    NodeType::CDATASection | NodeType::Text => {}
                    _ => return false,
                }
                child = ch.next_sibling();
            }
            true
        }

        // reference:
        // https://github.com/apache/xerces2-j/blob/trunk/src/org/apache/xerces/dom/TextImpl.java
        fn can_modify_prev(node: NodeRef) -> bool {
            let mut text_last_child = false;

            let mut prev = node.previous_sibling();

            while let Some(pre) = prev {
                match pre.node_type() {
                    NodeType::EntityReference => {
                        let mut last_child = pre.last_child();
                        if last_child.is_none() {
                            return false;
                        }
                        while let Some(child) = last_child {
                            match child.node_type() {
                                NodeType::CDATASection | NodeType::Text => {
                                    text_last_child = true;
                                }
                                NodeType::EntityReference => {
                                    if !can_modify_prev(child.clone()) {
                                        return false;
                                    }
                                    text_last_child = true;
                                }
                                _ => return !text_last_child,
                            }
                            last_child = child.previous_sibling();
                        }
                    }
                    NodeType::CDATASection | NodeType::Text => {}
                    _ => return true,
                }
                prev = pre.previous_sibling();
            }
            true
        }

        // reference:
        // https://github.com/apache/xerces2-j/blob/trunk/src/org/apache/xerces/dom/TextImpl.java
        fn can_modify_next(node: NodeRef) -> bool {
            let mut text_first_child = false;

            let mut next = node.next_sibling();
            while let Some(nxt) = next {
                match nxt.node_type() {
                    NodeType::EntityReference => {
                        let mut first_child = nxt.first_child();
                        if first_child.is_none() {
                            return false;
                        }
                        while let Some(child) = first_child {
                            match child.node_type() {
                                NodeType::CDATASection | NodeType::Text => {
                                    text_first_child = true;
                                }
                                NodeType::EntityReference => {
                                    if !can_modify_next(child.clone()) {
                                        return false;
                                    }
                                    text_first_child = true;
                                }
                                _ => return !text_first_child,
                            }
                            first_child = child.next_sibling();
                        }
                    }
                    NodeType::CDATASection | NodeType::Text => {}
                    _ => return true,
                }
                next = nxt.next_sibling();
            }
            true
        }

        let parent = self.parent_node();
        if content.is_empty() {
            if let Some(mut parent) = parent {
                parent.remove_child(self.clone().into())?;
            }
            return Ok(None);
        }

        if !can_modify_prev(self.clone().into()) || !can_modify_next(self.clone().into()) {
            return Err(DOMException::NoModificationAllowedErr);
        }

        // TODO: handle read-only node
        self.set_data(content)?;
        let mut prev = self.previous_sibling();
        while let Some(mut pre) = prev {
            match pre.node_type() {
                NodeType::CDATASection | NodeType::Text => {
                    pre.disconnect_parent_and_sibling();
                    prev = self.previous_sibling();
                }
                NodeType::EntityReference if has_text_only_children(Some(pre.clone())) => {
                    pre.disconnect_parent_and_sibling();
                    prev = self.previous_sibling();
                }
                _ => break,
            }
        }

        let mut next = self.next_sibling();
        while let Some(mut nxt) = next {
            match nxt.node_type() {
                NodeType::CDATASection | NodeType::Text => {
                    nxt.disconnect_parent_and_sibling();
                    next = self.next_sibling();
                }
                NodeType::EntityReference if has_text_only_children(Some(nxt.clone())) => {
                    nxt.disconnect_parent_and_sibling();
                    next = self.next_sibling();
                }
                _ => break,
            }
        }
        Ok(Some(self.clone()))
    }

    /// Implementation of [`isElementContentWhitespace`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Text3-isElementContentWhitespace) attribute.
    ///
    /// # Specification
    /// ```text
    /// Returns whether this text node contains element content whitespace, often abusively
    /// called "ignorable whitespace". The text node is determined to contain whitespace in
    /// element content during the load of the document or if validation occurs while using
    /// Document.normalizeDocument().
    /// ```
    pub fn is_element_content_whitespace(&self) -> bool {
        self.0.borrow().data.chars().all(|c| c.is_xml_blank_char())
    }

    /// Implementation of [`wholeText`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Text3-wholeText) attribute.
    ///
    /// # Specification
    /// ```text
    /// Returns all text of Text nodes logically-adjacent text nodes to this node,
    /// concatenated in document order.
    /// For instance, in the example below wholeText on the Text node that contains "bar"
    /// returns "barfoo", while on the Text node that contains "foo" it returns "barfoo".
    ///
    /// Figure: barTextNode.wholeText value is "barfoo" [SVG 1.0 version]
    /// ```
    pub fn whole_text(&self) -> String {
        fn whole_text_forward(
            mut node: Option<NodeRef>,
            buf: &mut String,
            parent: Option<NodeRef>,
        ) -> bool {
            while let Some(cur) = node {
                match &cur {
                    NodeRef::EntityReference(entref) => {
                        if whole_text_forward(entref.first_child(), buf, Some(cur.clone())) {
                            return true;
                        }
                    }
                    NodeRef::CDATASection(cdata) => {
                        buf.push_str(&cdata.0.borrow().data);
                    }
                    NodeRef::Text(text) => {
                        buf.push_str(&text.0.borrow().data);
                    }
                    _ => return true,
                }
                node = cur.next_sibling();
            }
            if let Some(parent) = parent.filter(|par| par.node_type() == NodeType::EntityReference)
            {
                whole_text_forward(parent.next_sibling(), buf, parent.parent_node());
                return true;
            }
            false
        }

        fn whole_text_backward(
            mut node: Option<NodeRef>,
            buf: &mut String,
            parent: Option<NodeRef>,
        ) -> bool {
            while let Some(cur) = node {
                match &cur {
                    NodeRef::EntityReference(entref) => {
                        if whole_text_backward(entref.last_child(), buf, Some(cur.clone())) {
                            return true;
                        }
                    }
                    NodeRef::CDATASection(cdata) => {
                        buf.insert_str(0, &cdata.0.borrow().data);
                    }
                    NodeRef::Text(text) => {
                        buf.insert_str(0, &text.0.borrow().data);
                    }
                    _ => return true,
                }
                node = cur.previous_sibling();
            }
            if let Some(parent) = parent.filter(|par| par.node_type() == NodeType::EntityReference)
            {
                whole_text_backward(parent.previous_sibling(), buf, parent.parent_node());
                return true;
            }
            false
        }

        let mut buf = self.0.borrow().data.clone();
        whole_text_backward(self.previous_sibling(), &mut buf, self.parent_node());
        whole_text_forward(self.next_sibling(), &mut buf, self.parent_node());
        buf
    }

    /// Create new [`TextRef`] whose ownerDocument is `doc`.
    pub(super) fn from_doc(doc: DocumentRef, data: String) -> Self {
        Self(
            Rc::new(RefCell::new(Text {
                parent_node: None,
                previous_sibling: None,
                next_sibling: None,
                owner_document: Rc::downgrade(&doc.0),
                data,
                user_data: None,
                flag: 0,
            })),
            doc,
        )
    }
}

impl Node for TextRef {
    fn node_name(&self) -> Rc<str> {
        "#text".into()
    }

    fn node_value(&self) -> Option<Rc<str>> {
        Some(self.0.borrow().data.as_str().into())
    }

    fn set_node_value(&mut self, value: impl Into<String>) -> Result<(), DOMException> {
        self.set_data(value)?;
        Ok(())
    }

    fn node_type(&self) -> NodeType {
        NodeType::Text
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
        Some(self.1.clone())
    }

    fn clone_node(&self, _deep: bool) -> NodeRef {
        let text = TextRef(
            Rc::new(RefCell::new(Text {
                parent_node: None,
                previous_sibling: None,
                next_sibling: None,
                owner_document: self.0.borrow().owner_document.clone(),
                data: self.0.borrow().data.clone(),
                user_data: None,
                flag: 0,
            })),
            self.1.clone(),
        );

        self.handle_user_data(OperationType::NodeCloned, Some(text.clone().into()));
        text.into()
    }

    fn text_content(&self) -> Option<String> {
        self.node_value().map(|value| value.to_string())
    }

    fn set_text_content(&mut self, text: impl Into<String>) -> Result<(), DOMException> {
        self.0.borrow_mut().data = text.into();
        Ok(())
    }

    fn is_same_node(&self, other: &NodeRef) -> bool {
        let NodeRef::Text(other) = other else {
            return false;
        };
        Rc::ptr_eq(&self.0, &other.0)
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
        self.0.borrow().flag & 1 != 0
    }
}

impl CharacterData for TextRef {
    fn data(&self) -> String {
        self.0.borrow().data.clone()
    }

    fn set_data(&mut self, data: impl Into<String>) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;
        self.0.borrow_mut().data = data.into();
        Ok(())
    }
}

impl NodeConnection for TextRef {
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
        self.0.borrow_mut().owner_document = Rc::downgrade(&new_doc.0);
        Some(replace(&mut self.1, new_doc))
    }

    fn set_read_only(&mut self) {
        self.0.borrow_mut().flag |= 0b01;
    }

    fn unset_read_only(&mut self) {
        self.0.borrow_mut().flag &= !0b01;
    }

    fn adopted_to(&mut self, new_doc: DocumentRef) {
        self.0.borrow_mut().adopted_to(new_doc);

        if let Some(user_data) = self.0.borrow().user_data.as_ref() {
            for (key, value) in user_data.iter() {
                if let Some(handler) = value.1.clone() {
                    handler.handle(
                        OperationType::NodeAdopted,
                        key.as_str(),
                        value.0.clone(),
                        self.clone().into(),
                        None,
                    );
                }
            }
        }
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

impl From<TextRef> for NodeRef {
    fn from(value: TextRef) -> Self {
        NodeRef::Text(value)
    }
}

impl From<Rc<RefCell<Text>>> for TextRef {
    fn from(value: Rc<RefCell<Text>>) -> Self {
        let doc = value.borrow().owner_document().unwrap();
        Self(value, doc)
    }
}

/// Implementation of [Comment](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1728279322)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
///
/// Strings are encoded in UTF-8. Unlike the specification, methods that specify string
/// boundaries are constrained to be UTF-8 character boundaries.
pub struct Comment {
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
    owner_document: Weak<RefCell<Document>>,
    data: String,

    user_data: Option<HashMap<String, (DOMUserData, Option<Arc<dyn UserDataHandler>>)>>,

    // 0      : read-only ?
    // 1 - 15 : unused
    flag: u16,
}

impl Comment {
    pub fn owner_document(&self) -> Option<DocumentRef> {
        self.owner_document.upgrade().map(From::from)
    }

    fn adopted_to(&mut self, new_doc: DocumentRef) {
        self.owner_document = Rc::downgrade(&new_doc.0);
    }
}

/// Wrapper of `Rc<RefCell<Comment>>`.
///
/// Strings are encoded in UTF-8. Unlike the specification, methods that specify string
/// boundaries are constrained to be UTF-8 character boundaries.
#[derive(Clone)]
pub struct CommentRef(pub(super) Rc<RefCell<Comment>>, pub(super) DocumentRef);

impl CommentRef {
    /// Create new [`CommentRef`] whose ownerDocument is `doc`.
    pub(super) fn from_doc(doc: DocumentRef, data: String) -> Self {
        Self(
            Rc::new(RefCell::new(Comment {
                parent_node: None,
                previous_sibling: None,
                next_sibling: None,
                owner_document: Rc::downgrade(&doc.0),
                data,
                user_data: None,
                flag: 0,
            })),
            doc,
        )
    }
}

impl Node for CommentRef {
    fn node_name(&self) -> Rc<str> {
        "#comment".into()
    }

    fn node_value(&self) -> Option<Rc<str>> {
        Some(self.0.borrow().data.as_str().into())
    }

    fn set_node_value(&mut self, value: impl Into<String>) -> Result<(), DOMException> {
        self.set_data(value)?;
        Ok(())
    }

    fn node_type(&self) -> NodeType {
        NodeType::Comment
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
        Some(self.1.clone())
    }

    fn clone_node(&self, _deep: bool) -> NodeRef {
        let text = CommentRef(
            Rc::new(RefCell::new(Comment {
                parent_node: None,
                previous_sibling: None,
                next_sibling: None,
                owner_document: self.0.borrow().owner_document.clone(),
                data: self.0.borrow().data.clone(),
                user_data: None,
                flag: 0,
            })),
            self.1.clone(),
        );

        self.handle_user_data(OperationType::NodeCloned, Some(text.clone().into()));
        text.into()
    }

    fn text_content(&self) -> Option<String> {
        self.node_value().map(|value| value.to_string())
    }

    fn set_text_content(&mut self, text: impl Into<String>) -> Result<(), DOMException> {
        self.0.borrow_mut().data = text.into();
        Ok(())
    }

    fn is_same_node(&self, other: &NodeRef) -> bool {
        let NodeRef::Comment(other) = other else {
            return false;
        };
        Rc::ptr_eq(&self.0, &other.0)
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
        self.0.borrow().flag & 1 != 0
    }
}

impl NodeConnection for CommentRef {
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
        self.0.borrow_mut().owner_document = Rc::downgrade(&new_doc.0);
        Some(replace(&mut self.1, new_doc))
    }

    fn set_read_only(&mut self) {
        self.0.borrow_mut().flag |= 0b01;
    }

    fn unset_read_only(&mut self) {
        self.0.borrow_mut().flag &= !0b01;
    }

    fn adopted_to(&mut self, new_doc: DocumentRef) {
        self.0.borrow_mut().adopted_to(new_doc);

        if let Some(user_data) = self.0.borrow().user_data.as_ref() {
            for (key, value) in user_data.iter() {
                if let Some(handler) = value.1.clone() {
                    handler.handle(
                        OperationType::NodeAdopted,
                        key.as_str(),
                        value.0.clone(),
                        self.clone().into(),
                        None,
                    );
                }
            }
        }
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

impl CharacterData for CommentRef {
    fn data(&self) -> String {
        self.0.borrow().data.clone()
    }

    fn set_data(&mut self, data: impl Into<String>) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;
        self.0.borrow_mut().data = data.into();
        Ok(())
    }
}

impl From<CommentRef> for NodeRef {
    fn from(value: CommentRef) -> Self {
        NodeRef::Comment(value)
    }
}

impl From<Rc<RefCell<Comment>>> for CommentRef {
    fn from(value: Rc<RefCell<Comment>>) -> Self {
        let doc = value.borrow().owner_document().unwrap();
        Self(value, doc)
    }
}

/// Implementation of [CDATASection](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-667469212)
/// interface on [1.5 Extended Interfaces: XML Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-E067D597)
///
/// Strings are encoded in UTF-8. Unlike the specification, methods that specify string
/// boundaries are constrained to be UTF-8 character boundaries.
pub struct CDATASection(Text);

impl Deref for CDATASection {
    type Target = Text;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for CDATASection {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

/// Wrapper of `Rc<RefCell<CDATASection>>`.
///
/// Strings are encoded in UTF-8. Unlike the specification, methods that specify string
/// boundaries are constrained to be UTF-8 character boundaries.
#[derive(Clone)]
pub struct CDATASectionRef(pub(super) Rc<RefCell<CDATASection>>, pub(super) DocumentRef);

impl CDATASectionRef {
    /// Create new [`CDATASectionRef`] whose ownerDocument is `doc`.
    pub(super) fn from_doc(doc: DocumentRef, data: String) -> Self {
        Self(
            Rc::new(RefCell::new(CDATASection(Text {
                parent_node: None,
                previous_sibling: None,
                next_sibling: None,
                owner_document: Rc::downgrade(&doc.0),
                data,
                user_data: None,
                flag: 0,
            }))),
            doc,
        )
    }
}

impl Node for CDATASectionRef {
    fn node_name(&self) -> Rc<str> {
        "#cdata-section".into()
    }

    fn node_value(&self) -> Option<Rc<str>> {
        Some(self.0.borrow().data.as_str().into())
    }

    fn set_node_value(&mut self, value: impl Into<String>) -> Result<(), DOMException> {
        self.set_data(value)?;
        Ok(())
    }

    fn node_type(&self) -> NodeType {
        NodeType::CDATASection
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
        Some(self.1.clone())
    }

    fn clone_node(&self, _deep: bool) -> NodeRef {
        let text = CDATASectionRef(
            Rc::new(RefCell::new(CDATASection(Text {
                parent_node: None,
                previous_sibling: None,
                next_sibling: None,
                owner_document: self.0.borrow().owner_document.clone(),
                data: self.0.borrow().data.clone(),
                user_data: None,
                flag: 0,
            }))),
            self.1.clone(),
        );

        self.handle_user_data(OperationType::NodeCloned, Some(text.clone().into()));
        text.into()
    }

    fn text_content(&self) -> Option<String> {
        self.node_value().map(|value| value.to_string())
    }

    fn set_text_content(&mut self, text: impl Into<String>) -> Result<(), DOMException> {
        self.0.borrow_mut().data = text.into();
        Ok(())
    }

    fn is_same_node(&self, other: &NodeRef) -> bool {
        let NodeRef::CDATASection(other) = other else {
            return false;
        };
        Rc::ptr_eq(&self.0, &other.0)
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
        self.0.borrow().0.flag & 1 != 0
    }
}

impl NodeConnection for CDATASectionRef {
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
        self.0.borrow_mut().owner_document = Rc::downgrade(&new_doc.0);
        Some(replace(&mut self.1, new_doc))
    }

    fn set_read_only(&mut self) {
        self.0.borrow_mut().0.flag |= 0b01;
    }

    fn unset_read_only(&mut self) {
        self.0.borrow_mut().0.flag &= !0b01;
    }

    fn adopted_to(&mut self, new_doc: DocumentRef) {
        self.0.borrow_mut().adopted_to(new_doc);

        if let Some(user_data) = self.0.borrow().user_data.as_ref() {
            for (key, value) in user_data.iter() {
                if let Some(handler) = value.1.clone() {
                    handler.handle(
                        OperationType::NodeAdopted,
                        key.as_str(),
                        value.0.clone(),
                        self.clone().into(),
                        None,
                    );
                }
            }
        }
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

impl CharacterData for CDATASectionRef {
    fn data(&self) -> String {
        self.0.borrow().data.clone()
    }

    fn set_data(&mut self, data: impl Into<String>) -> Result<(), DOMException> {
        check_no_modification_allowed_err(self)?;
        self.0.borrow_mut().data = data.into();
        Ok(())
    }
}

impl From<CDATASectionRef> for NodeRef {
    fn from(value: CDATASectionRef) -> Self {
        NodeRef::CDATASection(value)
    }
}

impl From<Rc<RefCell<CDATASection>>> for CDATASectionRef {
    fn from(value: Rc<RefCell<CDATASection>>) -> Self {
        let doc = value.borrow().owner_document().unwrap();
        Self(value, doc)
    }
}

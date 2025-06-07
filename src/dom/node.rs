use std::rc::Rc;

use crate::{
    dom::{DOCUMENT_POSITION_DISCONNECTED, check_owner_document_sameness},
    tree::XML_XML_NAMESPACE,
    uri::{XmlURI, build_uri},
};

use super::{
    DOCUMENT_POSITION_CONTAINED_BY, DOCUMENT_POSITION_CONTAINS, DOCUMENT_POSITION_FOLLOWING,
    DOCUMENT_POSITION_PRECEDING, DOMException, DocumentPosition, NodeType,
    attr::{AttrRef, AttrWeakRef},
    character_data::{
        CDATASectionRef, CDATASectionWeakRef, CharacterData, CommentRef, CommentWeakRef, TextRef,
        TextWeakRef,
    },
    check_vertical_hierarchy,
    document::{DocumentRef, DocumentWeakRef},
    document_fragment::{DocumentFragmentRef, DocumentFragmentWeakRef},
    document_type::{DocumentTypeRef, DocumentTypeWeakRef},
    element::{ElementRef, ElementWeakRef},
    entity::{EntityRef, EntityWeakRef},
    entity_reference::{EntityReferenceRef, EntityReferenceWeakRef},
    named_node_map::NamedNodeMap,
    notation::{NotationRef, NotationWeakRef},
    pi::{ProcessingInstructionRef, ProcessingInstructionWeakRef},
};

/// Implementation of [Node](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1950641247)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
///
/// Actual node representations are implemented as `NodeRef` and `NodeWeakRef`.
#[allow(private_bounds)]
pub trait Node: NodeConnection {
    /// Implementation of `nodeName` attribute.
    fn node_name(&self) -> Rc<str>;
    /// Implementation of `nodeValue` attribute.
    fn node_value(&self) -> Option<Rc<str>>;

    /// Implementation of `nodeType` attribute.
    fn node_type(&self) -> NodeType;
    /// Implementation of `parentNode` attribute.
    fn parent_node(&self) -> Option<NodeRef> {
        None
    }
    /// Implementation of `childNodes` attribute.
    ///
    /// The specification requires that this method returns a `NodeList`,
    /// but this method returns [`Vec`].  
    fn child_nodes(&self) -> Vec<NodeRef> {
        let mut res = vec![];
        let mut children = self.first_child();
        while let Some(child) = children {
            children = child.next_sibling();
            res.push(child);
        }
        res
    }
    /// Implementation of `firstChild` attribute.
    fn first_child(&self) -> Option<NodeRef> {
        None
    }
    /// Implementation of `lastChild` attribute.
    fn last_child(&self) -> Option<NodeRef> {
        None
    }
    /// Implementation of `previousSibling` attribute.
    fn previous_sibling(&self) -> Option<NodeRef> {
        None
    }
    /// Implementation of `nextSibling` attribute.
    fn next_sibling(&self) -> Option<NodeRef> {
        None
    }
    /// Implementation of `attributes` attribute.
    fn attributes(&self) -> Option<NamedNodeMap<AttrRef>> {
        None
    }
    /// Implementation of `ownerDocument` attribute.
    fn owner_document(&self) -> Option<DocumentRef> {
        None
    }

    /// Implementation of [`insertBefore`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-952280727) method.
    ///
    /// If `new_child` and `ref_child` are same nodes,
    /// this method does nothing and return `Ok(new_child)`.
    ///
    /// # Specification
    /// ```text
    /// Inserts the node newChild before the existing child node refChild.
    /// If refChild is null, insert newChild at the end of the list of children.
    /// If newChild is a DocumentFragment object, all of its children are inserted,
    /// in the same order, before refChild.
    /// If the newChild is already in the tree, it is first removed.
    ///
    /// Note: Inserting a node before itself is implementation dependent.
    ///
    /// Parameters
    ///     newChild of type Node
    ///         The node to insert.
    ///     refChild of type Node
    ///         The reference node, i.e., the node before which the new node must be inserted.
    ///
    /// Return Value
    ///     Node The node being inserted.
    /// ```
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
        if !check_vertical_hierarchy(self.node_type(), new_child.node_type()) {
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

        // HIERARCHY_REQUEST_ERR: Raised if (..snip..) the node to insert is
        // one of this node's ancestors or this node itself, (..snip..)
        if self.is_same_node(&new_child) {
            return Err(DOMException::HierarchyRequestErr);
        }
        let mut par = self.parent_node();
        while let Some(cur) = par {
            par = cur.parent_node();
            if new_child.is_same_node(&cur) {
                return Err(DOMException::HierarchyRequestErr);
            }
        }

        // HIERARCHY_REQUEST_ERR: Raised if (..snip..) this node is of type Document
        // and the DOM application attempts to insert a second DocumentType or Element node.
        if self.node_type() == NodeType::Document
            && matches!(
                new_child.node_type(),
                NodeType::Element | NodeType::DocumentType
            )
        {
            let mut chilren = self.first_child();
            while let Some(child) = chilren {
                chilren = child.next_sibling();
                if child.node_type() == new_child.node_type() {
                    return Err(DOMException::HierarchyRequestErr);
                }
            }
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
                other => other.connect_as_previous_sibling(ref_child),
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
                child.set_parent_node(Some(<Self as Into<NodeRef>>::into(self.clone())));
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

    /// Implementation of [`replaceChild`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-785887307) method.
    ///
    /// # Specification
    /// ```text
    /// Replaces the child node oldChild with newChild in the list of children,
    /// and returns the oldChild node.  
    /// If newChild is a DocumentFragment object, oldChild is replaced by all of
    /// the DocumentFragment children, which are inserted in the same order.  
    /// If the newChild is already in the tree, it is first removed.
    ///
    /// Note: Replacing a node with itself is implementation dependent.
    ///
    /// Parameters
    ///     newChild of type Node
    ///         The new node to put in the child list.
    ///     oldChild of type Node
    ///         The node being replaced in the list.
    ///
    /// Return Value
    ///     Node The node replaced.
    /// ```
    fn replace_child(
        &mut self,
        new_child: NodeRef,
        mut old_child: NodeRef,
    ) -> Result<NodeRef, DOMException> {
        // In this implementation, if `new_child` and `ref_child` are same node,
        // do nothing and return `new_child`.
        if new_child.is_same_node(&old_child) {
            return Ok(new_child);
        }

        // HIERARCHY_REQUEST_ERR: Raised if this node is of a type that does not allow children
        // of the type of the newChild node (..snip)
        if !check_vertical_hierarchy(self.node_type(), new_child.node_type()) {
            return Err(DOMException::HierarchyRequestErr);
        }
        // NOT_FOUND_ERR: Raised if oldChild is not a child of this node.
        if old_child
            .parent_node()
            .is_none_or(|par| !self.is_same_node(&par))
        {
            return Err(DOMException::NotFoundErr);
        }
        let rdoc = self.owner_document();
        let ndoc = new_child.owner_document();
        // WRONG_DOCUMENT_ERR: Raised if newChild was created from a different document
        // than the one that created this node.
        if rdoc.is_some() != ndoc.is_some()
            || rdoc
                .zip(ndoc)
                .is_some_and(|(rd, nd)| !rd.is_same_node(&NodeRef::Document(nd)))
        {
            return Err(DOMException::WrongDocumentErr);
        }

        // HIERARCHY_REQUEST_ERR: Raised if (..snip..) the node to put in is
        // one of this node's ancestors or this node itself, (..snip..)
        if self.is_same_node(&new_child) {
            return Err(DOMException::HierarchyRequestErr);
        }
        let mut par = self.parent_node();
        while let Some(cur) = par {
            par = cur.parent_node();
            if new_child.is_same_node(&cur) {
                return Err(DOMException::HierarchyRequestErr);
            }
        }

        // HIERARCHY_REQUEST_ERR: Raised if (..snip..) this node is of type Document
        // and the result of the replacement operation would add
        // a second DocumentType or Element on the Document node.
        if self.node_type() == NodeType::Document
            && matches!(
                new_child.node_type(),
                NodeType::Element | NodeType::DocumentType
            )
        {
            let mut chilren = self.first_child();
            while let Some(child) = chilren {
                chilren = child.next_sibling();
                if child.node_type() == new_child.node_type() && !child.is_same_node(&old_child) {
                    return Err(DOMException::HierarchyRequestErr);
                }
            }
        }

        let mut children = match new_child {
            NodeRef::DocumentFragment(frag) => frag.first_child(),
            mut other => {
                other.disconnect_parent_and_sibling();
                Some(other)
            }
        };
        let par = old_child.set_parent_node(None);
        let prev = old_child.set_previous_sibling(None);
        let next = old_child.set_next_sibling(None);
        let prev = prev.or_else(|| {
            let mut child = children.clone()?;
            children = child.next_sibling();
            child.set_parent_node(par.clone());
            self.set_first_child(Some(child.clone()));
            Some(child)
        });
        if let Some(mut prev) = prev {
            while let Some(mut child) = children {
                children = child.next_sibling();
                child.set_parent_node(par.clone());
                child.set_previous_sibling(Some(prev.clone()));
                prev.set_next_sibling(Some(child.clone()));
                prev = child;
            }
            prev.set_next_sibling(next.clone());
            if let Some(mut next) = next {
                next.set_previous_sibling(Some(prev));
            }
        }
        Ok(old_child)
    }

    /// Implementation of [`removeChild`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1734834066) method.
    ///
    /// # Specification
    /// ```text
    /// Removes the child node indicated by oldChild from the list of children, and returns it.
    ///
    /// Parameters
    ///     oldChild of type Node
    ///         The node being removed.
    ///
    /// Return Value
    ///     Node The node removed.
    /// ```
    fn remove_child(&mut self, mut old_child: NodeRef) -> Result<NodeRef, DOMException> {
        // NOT_FOUND_ERR: Raised if oldChild is not a child of this node.
        if old_child
            .parent_node()
            .is_none_or(|par| !self.is_same_node(&par))
        {
            return Err(DOMException::NotFoundErr);
        }

        old_child.disconnect_parent_and_sibling();
        Ok(old_child)
    }

    /// Implementation of [`appendChild`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-184E7107) method.
    ///
    /// # Specification
    /// ```text
    /// Adds the node newChild to the end of the list of children of this node.
    /// If the newChild is already in the tree, it is first removed.
    ///
    /// Parameters
    ///     newChild of type Node
    ///         The node to add.
    ///         If it is a DocumentFragment object, the entire contents of
    ///         the document fragment are moved into the child list of this node
    ///
    /// Return Value
    ///     Node The node added.
    /// ```
    fn append_child(&mut self, new_child: NodeRef) -> Result<NodeRef, DOMException> {
        self.insert_before(new_child, None)
    }

    /// Implementation of `hasChildNodes` method.
    fn has_child_nodes(&self) -> bool {
        self.first_child().is_some()
    }

    /// Implementation of [`cloneNode`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-3A0ED0A4) method.
    ///
    /// # Specification
    /// ```text
    /// Returns a duplicate of this node, i.e., serves as a generic copy constructor for nodes.
    /// The duplicate node has no parent (parentNode is null) and no user data.
    /// User data associated to the imported node is not carried over.
    /// However, if any UserDataHandlers has been specified along with the associated data
    /// these handlers will be called with the appropriate parameters before this method returns.
    /// Cloning an Element copies all attributes and their values,
    /// including those generated by the XML processor to represent defaulted attributes,
    /// but this method does not copy any children it contains unless it is a deep clone.
    /// This includes text contained in an the Element since the text is contained
    /// in a child Text node. Cloning an Attr directly, as opposed to be cloned as part of
    /// an Element cloning operation, returns a specified attribute (specified is true).
    /// Cloning an Attr always clones its children, since they represent its value,
    /// no matter whether this is a deep clone or not.
    /// Cloning an EntityReference automatically constructs its subtree
    /// if a corresponding Entity is available, no matter whether this is a deep clone or not.
    /// Cloning any other type of node simply returns a copy of this node.
    /// Note that cloning an immutable subtree results in a mutable copy,
    /// but the children of an EntityReference clone are readonly.
    /// In addition, clones of unspecified Attr nodes are specified.
    /// And, cloning Document, DocumentType, Entity, and Notation nodes is implementation dependent.
    ///
    /// Parameters
    ///     deep of type boolean
    ///         If true, recursively clone the subtree under the specified node;
    ///         if false, clone only the node itself (and its attributes, if it is an Element).
    ///
    /// Return Value
    ///     Node The duplicate node.
    /// ```
    fn clone_node(&self, deep: bool) -> NodeRef;

    /// Implementation of [`normalize`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-normalize) method.
    ///
    /// # Specification
    /// ```text
    /// Puts all Text nodes in the full depth of the sub-tree underneath this Node, including
    /// attribute nodes, into a "normal" form where only structure (e.g., elements, comments,
    /// processing instructions, CDATA sections, and entity references) separates Text nodes,
    /// i.e., there are neither adjacent Text nodes nor empty Text nodes. This can be used to
    /// ensure that the DOM view of a document is the same as if it were saved and re-loaded,
    /// and is useful when operations (such as XPointer [XPointer] lookups) that depend on a
    /// particular document tree structure are to be used. If the parameter
    /// "normalize-characters" of the DOMConfiguration object attached to the
    /// Node.ownerDocument is true, this method will also fully normalize the characters of
    /// the Text nodes.
    ///
    /// Note: In cases where the document contains CDATASections, the normalize operation
    /// alone may not be sufficient, since XPointers do not differentiate between Text nodes
    /// and CDATASection nodes.
    ///
    /// No Parameters
    ///
    /// No Return Value
    ///
    /// No Exceptions
    /// ```
    fn normalize(&mut self) {
        let mut children = self.first_child();
        while let Some(child) = children {
            match child {
                NodeRef::Text(mut text) => {
                    children = text.next_sibling();
                    while let Some(next) = children {
                        match next {
                            NodeRef::Text(next) => {
                                text.append_data(next.data().as_str()).ok();
                                self.remove_child(next.into()).ok();
                                children = text.next_sibling();
                            }
                            next => {
                                children = next.next_sibling();
                                break;
                            }
                        }
                    }
                }
                NodeRef::Element(mut elem) => {
                    elem.normalize();
                    children = elem.next_sibling();
                }
                child => children = child.next_sibling(),
            }
        }

        if let Some(attrs) = self.attributes() {
            for i in 0..attrs.len() {
                let mut attr = attrs.item(i).unwrap();
                attr.normalize();
            }
        }
    }

    /// Implementation of [`namespaceURI`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-NodeNSname) attribute.
    ///
    /// # Specification
    /// ```text
    /// The namespace URI of this node, or null if it is unspecified (see XML Namespaces).
    /// This is not a computed value that is the result of a namespace lookup
    /// based on an examination of the namespace declarations in scope.
    /// It is merely the namespace URI given at creation time.
    /// For nodes of any type other than ELEMENT_NODE and ATTRIBUTE_NODE and
    /// nodes created with a DOM Level 1 method, such as Document.createElement(),
    /// this is always null.
    ///
    /// Note: Per the Namespaces in XML Specification [XML Namespaces] an attribute
    /// does not inherit its namespace from the element it is attached to.
    /// If an attribute is not explicitly given a namespace, it simply has no namespace.
    /// ```
    fn namespace_uri(&self) -> Option<Rc<str>> {
        None
    }

    /// Implementation of [`prefix`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-NodeNSPrefix) attribute.
    ///
    /// # Specification
    /// ```text
    /// The namespace prefix of this node, or null if it is unspecified.
    /// When it is defined to be null, setting it has no effect,
    /// including if the node is read-only.
    /// Note that setting this attribute, when permitted, changes the nodeName attribute,
    /// which holds the qualified name, as well as the tagName and name attributes
    /// of the Element and Attr interfaces, when applicable.
    /// Setting the prefix to null makes it unspecified, setting it to an empty string
    /// is implementation dependent.
    /// Note also that changing the prefix of an attribute that is known to
    /// have a default value, does not make a new attribute with the default value
    /// and the original prefix appear, since the namespaceURI and localName do not change.
    /// For nodes of any type other than ELEMENT_NODE and ATTRIBUTE_NODE and nodes
    /// created with a DOM Level 1 method, such as createElement from the Document interface,
    /// this is always null.
    /// ```
    fn prefix(&self) -> Option<Rc<str>> {
        None
    }

    /// Implementation of [`localName`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-NodeNSLocalN) attribute.
    ///
    /// # Specification
    /// ```text
    /// Returns the local part of the qualified name of this node.
    /// For nodes of any type other than ELEMENT_NODE and ATTRIBUTE_NODE
    /// and nodes created with a DOM Level 1 method, such as Document.createElement(),
    /// this is always null.
    /// ```
    fn local_name(&self) -> Option<Rc<str>> {
        None
    }

    /// Implementation of [`hasAttributes`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-NodeHasAttrs) method.
    ///
    /// # Specification
    /// ```text
    /// Returns whether this node (if it is an element) has any attributes.
    ///
    /// Return Value
    ///     boolean Returns true if this node has any attributes, false otherwise.
    /// ```
    fn has_attributes(&self) -> bool {
        self.attributes().is_some_and(|attr| !attr.is_empty())
    }

    /// Implementation of [`baseURI`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Node3-baseURI) attribute.
    ///
    /// # Specification
    /// ```text
    /// The absolute base URI of this node or null if the implementation wasn't able
    /// to obtain an absolute URI. This value is computed as described in Base URIs.
    /// However, when the Document supports the feature "HTML" [DOM Level 2 HTML],
    /// the base URI is computed using first the value of the href attribute
    /// of the HTML BASE element if any, and the value of the documentURI attribute
    /// from the Document interface otherwise.
    /// ```
    fn base_uri(&self) -> Option<String> {
        if let Some(doc) = self.owner_document().filter(|doc| doc.is_html()) {
            // when the Document supports the feature "HTML" [DOM Level 2 HTML],
            // the base URI is computed using first the value of the href attribute
            // of the HTML BASE element if any, and the value of the documentURI attribute
            // from the Document interface otherwise.
            let mut children = doc.first_child();
            while let Some(child) = children {
                children = child.next_sibling();
                if let NodeRef::Element(elem) = child {
                    let name = elem.node_name();
                    if name.eq_ignore_ascii_case("html") || name.eq_ignore_ascii_case("head") {
                        children = elem.first_child();
                        continue;
                    } else if name.eq_ignore_ascii_case("base") {
                        return elem.get_attribute("href".into());
                    }
                }
            }
            return None;
        }

        let mut bases = vec![];
        if let Some(base) = self
            .attributes()
            .and_then(|attrs| {
                attrs
                    .get_named_item_ns(Some(XML_XML_NAMESPACE.into()), "base".into())
                    .ok()
                    .flatten()
            })
            .and_then(|attr| attr.node_value())
        {
            let base = base.to_string();
            if XmlURI::parse(&base).is_some_and(|base| base.scheme.is_some()) {
                return Some(base);
            }
            bases.push(base);
        }
        let mut parents = self.parent_node();
        while let Some(parent) = parents {
            parents = parent.parent_node();

            if let NodeRef::Element(elem) = parent {
                if let Ok(Some(attr)) =
                    elem.get_attribute_ns(Some(XML_XML_NAMESPACE.into()), "base".into())
                {
                    if XmlURI::parse(&attr).is_some_and(|base| base.scheme.is_some()) {
                        return bases
                            .into_iter()
                            .rev()
                            .try_fold(attr, |base, uri| build_uri(&uri, &base));
                    }
                    bases.push(attr);
                }
            }
        }

        if let Some(url) = self.owner_document().and_then(|doc| doc.document_uri()) {
            bases.push(url.to_string());
        }

        let base = bases.pop()?;
        bases
            .into_iter()
            .rev()
            .try_fold(base, |base, uri| build_uri(&uri, &base))
    }

    /// Implementation of [`compareDocumentPosition`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Node3-compareDocumentPosition) method.
    ///
    /// # Specification
    /// ```text
    /// Compares the reference node, i.e. the node on which this method is being called,
    /// with a node, i.e. the one passed as a parameter, with regard to their position
    /// in the document and according to the document order.
    ///
    /// Parameters
    ///     other of type Node
    ///         The node to compare against the reference node.
    ///
    /// Return Value
    ///     unsigned short Returns how the node is positioned relatively to the reference
    ///                    node.
    ///
    /// Exceptions
    ///     DOMException
    ///     NOT_SUPPORTED_ERR: when the compared nodes are from different DOM implementations
    ///                        that do not coordinate to return consistent
    ///                        implementation-specific results.
    /// ```
    fn compare_document_position(&self, other: &NodeRef) -> DocumentPosition {
        // reference:
        // https://github.com/apache/xerces2-j/blob/trunk/src/org/apache/xerces/dom/NodeImpl.java

        // If the nodes are the same, no flags should be set
        if self.is_same_node(other) {
            return DocumentPosition::new();
        }

        // If from different documents, we know they are disconnected.
        if !check_owner_document_sameness(self, other) {
            return DocumentPosition::new().set_flag(DOCUMENT_POSITION_DISCONNECTED);
        }

        // get the respective Document owners.
        let this_owner_doc = if let NodeRef::Document(doc) = self.clone().into() {
            doc
        } else {
            self.owner_document().expect("Internal Error")
        };
        let other_owner_doc = if let NodeRef::Document(doc) = other {
            doc.clone()
        } else {
            other.owner_document().expect("Internal Erorr")
        };

        // Find the ancestor of each node, and the distance each node is from
        // its ancestor.
        // During this traversal, look for ancestor/descendent relationships
        // between the 2 nodes in question.
        // We do this now, so that we get this info correct for attribute nodes
        // and their children.

        let mut this_ancestor: NodeRef = self.clone().into();
        let mut this_depth = 0;
        while let Some(node) = this_ancestor.parent_node() {
            this_depth += 1;
            if this_ancestor.is_same_node(other) {
                // The other node is an ancestor of this one.
                return DocumentPosition::new()
                    .set_flag(DOCUMENT_POSITION_CONTAINS)
                    .set_flag(DOCUMENT_POSITION_PRECEDING);
            }
            this_ancestor = node;
        }

        let mut other_ancestor = other.clone();
        let mut other_depth = 0;
        while let Some(node) = other_ancestor.parent_node() {
            other_depth += 1;
            if self.is_same_node(&node) {
                // The other node is a descendent of the reference node.
                return DocumentPosition::new()
                    .set_flag(DOCUMENT_POSITION_CONTAINED_BY)
                    .set_flag(DOCUMENT_POSITION_FOLLOWING);
            }
            other_ancestor = node;
        }

        let mut this_node: NodeRef = self.clone().into();
        let mut other_node = other.clone();
        if let Some(doctype) = this_owner_doc.doctype() {
            match doctype.compare_decl_position(&this_node, &other_node) {
                Ok(cmp) => {
                    return match cmp {
                        std::cmp::Ordering::Less => {
                            DocumentPosition::new().set_flag(DOCUMENT_POSITION_FOLLOWING)
                        }
                        std::cmp::Ordering::Equal => DocumentPosition::new(),
                        std::cmp::Ordering::Greater => {
                            DocumentPosition::new().set_flag(DOCUMENT_POSITION_PRECEDING)
                        }
                    };
                }
                Err((Some(_), None)) => {
                    this_depth = doctype.parent_node().is_some() as i32;
                    this_node = doctype.into();
                    this_ancestor = this_owner_doc.clone().into();
                }
                Err((None, Some(_))) => {
                    other_depth = doctype.parent_node().is_some() as i32;
                    other_node = doctype.into();
                    other_ancestor = other_owner_doc.clone().into();
                }
                _ => {}
            }
        }
        let this_ancestor_type = this_ancestor.node_type();
        let other_ancestor_type = other_ancestor.node_type();

        // Special casing for ENTITY, NOTATION, DOCTYPE and ATTRIBUTES
        // LM:  should rewrite this.
        match this_ancestor_type {
            NodeType::Notation | NodeType::Entity => {
                let container = this_owner_doc.doctype();
                if container
                    .as_ref()
                    .is_some_and(|doctype| doctype.is_same_node(&other_ancestor))
                {
                    return DocumentPosition::new()
                        .set_flag(DOCUMENT_POSITION_CONTAINS)
                        .set_flag(DOCUMENT_POSITION_PRECEDING);
                }
                match other_ancestor_type {
                    NodeType::Notation | NodeType::Entity => {
                        // If neither `self` nor `other` is a Notation or Entity
                        // but the positional relationship cannot be determined,
                        // they should not belong to the same tree.
                        return DocumentPosition::new().set_flag(DOCUMENT_POSITION_DISCONNECTED);
                    }
                    _ => {}
                }
                this_node = this_owner_doc.clone().into();
                this_ancestor = this_owner_doc.clone().into();
            }
            NodeType::DocumentType => {
                return if this_owner_doc.is_same_node(&other_node) {
                    DocumentPosition::new()
                        .set_flag(DOCUMENT_POSITION_PRECEDING)
                        .set_flag(DOCUMENT_POSITION_CONTAINS)
                } else {
                    DocumentPosition::new().set_flag(DOCUMENT_POSITION_FOLLOWING)
                };
            }
            NodeType::Attribute => {
                let attr = this_ancestor.as_attribute().unwrap();
                let Some(elem) = attr.owner_element() else {
                    let mut ancestor = other.parent_node();
                    while let Some(par) = ancestor {
                        ancestor = par.parent_node();
                        if attr.is_same_node(&par) {
                            return DocumentPosition::new()
                                .set_flag(DOCUMENT_POSITION_FOLLOWING)
                                .set_flag(DOCUMENT_POSITION_CONTAINED_BY);
                        }
                    }
                    return DocumentPosition::new().set_flag(DOCUMENT_POSITION_DISCONNECTED);
                };
                if let NodeRef::Attribute(oattr) = &other_ancestor {
                    let Some(oelem) = oattr.owner_element() else {
                        return DocumentPosition::new().set_flag(DOCUMENT_POSITION_DISCONNECTED);
                    };
                    if oelem.is_same_node(&NodeRef::Element(elem.clone())) {
                        let attrs = elem.attributes();
                        return if attrs.index_of(&attr) < attrs.index_of(oattr) {
                            DocumentPosition::new().set_flag(DOCUMENT_POSITION_FOLLOWING)
                        } else {
                            DocumentPosition::new().set_flag(DOCUMENT_POSITION_PRECEDING)
                        };
                    }
                }

                // Now, find the ancestor of the element
                this_depth = 0;
                this_node = NodeRef::Element(elem);
                this_ancestor = this_node.clone();
                while let Some(node) = this_ancestor.parent_node() {
                    this_depth += 1;
                    if this_node.is_same_node(&other_node) {
                        // The other node is an ancestor of the owning element
                        return DocumentPosition::new()
                            .set_flag(DOCUMENT_POSITION_CONTAINS)
                            .set_flag(DOCUMENT_POSITION_PRECEDING);
                    }
                    this_ancestor = node;
                }
            }
            _ => {}
        }
        match other_ancestor_type {
            NodeType::Notation | NodeType::Entity => {
                let container = this_owner_doc.doctype();
                if container.is_some_and(|doctype| self.is_same_node(&doctype.into())) {
                    return DocumentPosition::new()
                        .set_flag(DOCUMENT_POSITION_CONTAINED_BY)
                        .set_flag(DOCUMENT_POSITION_FOLLOWING);
                }
                other_node = this_owner_doc.clone().into();
                other_ancestor = this_owner_doc.clone().into();
            }
            NodeType::DocumentType => {
                return if other_owner_doc.is_same_node(&this_node) {
                    DocumentPosition::new()
                        .set_flag(DOCUMENT_POSITION_FOLLOWING)
                        .set_flag(DOCUMENT_POSITION_CONTAINED_BY)
                } else {
                    DocumentPosition::new().set_flag(DOCUMENT_POSITION_PRECEDING)
                };
            }
            NodeType::Attribute => {
                other_depth = 0;
                let NodeRef::Attribute(oattr) = &other_ancestor else {
                    unimplemented!()
                };
                let Some(oelem) = oattr.owner_element() else {
                    return DocumentPosition::new().set_flag(DOCUMENT_POSITION_DISCONNECTED);
                };
                other_node = NodeRef::Element(oelem);
                other_ancestor = other_node.clone();
                while let Some(node) = other_ancestor.parent_node() {
                    other_depth += 1;
                    if other_ancestor.is_same_node(&this_node) {
                        // The other node is a descendent of the reference
                        // node's element
                        return DocumentPosition::new()
                            .set_flag(DOCUMENT_POSITION_FOLLOWING)
                            .set_flag(DOCUMENT_POSITION_CONTAINED_BY);
                    }
                    other_ancestor = node;
                }
            }
            _ => {}
        }

        // thisAncestor and otherAncestor must be the same at this point,
        // otherwise, the original nodes are disconnected
        if !this_ancestor.is_same_node(&other_ancestor) {
            return DocumentPosition::new().set_flag(DOCUMENT_POSITION_DISCONNECTED);
        }

        // Go up the parent chain of the deeper node, until we find a node
        // with the same depth as the shallower node

        if this_depth > other_depth {
            for _ in 0..this_depth - other_depth {
                this_node = this_node.parent_node().unwrap();
            }
            // Check if the node we have reached is in fact "otherNode". This can
            // happen in the case of attributes.  In this case, otherNode
            // "precedes" this.
            if this_node.is_same_node(&other_node) {
                return DocumentPosition::new().set_flag(DOCUMENT_POSITION_PRECEDING);
            }
        } else {
            for _ in 0..other_depth - this_depth {
                other_node = other_node.parent_node().unwrap();
            }
            // Check if the node we have reached is in fact "thisNode".  This can
            // happen in the case of attributes.  In this case, otherNode
            // "follows" this.
            if other_node.is_same_node(&this_node) {
                return DocumentPosition::new().set_flag(DOCUMENT_POSITION_FOLLOWING);
            }
        }

        // We now have nodes at the same depth in the tree.  Find a common
        // ancestor.
        while let Some((tp, op)) = this_node
            .parent_node()
            .zip(other_node.parent_node())
            .filter(|(t, p)| !t.is_same_node(p))
        {
            this_node = tp;
            other_node = op;
        }

        let parent = this_node.parent_node().unwrap();
        // At this point, thisNode and otherNode are direct children of
        // the common ancestor.
        // See whether thisNode or otherNode is the leftmost
        let mut children = parent.first_child();
        while let Some(child) = children {
            children = child.next_sibling();
            if child.is_same_node(&this_node) {
                return DocumentPosition::new().set_flag(DOCUMENT_POSITION_FOLLOWING);
            } else if child.is_same_node(&other_node) {
                return DocumentPosition::new().set_flag(DOCUMENT_POSITION_PRECEDING);
            }
        }
        // REVISIT:  shouldn't get here.   Should probably throw an  exception
        unreachable!()
    }

    /// Implementation of [`textContent`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Node3-textContent) attribute.
    ///
    /// # Specification
    /// ```text
    /// This attribute returns the text content of this node and its descendants. When it is
    /// defined to be null, setting it has no effect. On setting, any possible children this
    /// node may have are removed and, if it the new string is not empty or null, replaced by
    /// a single Text node containing the string this attribute is set to.
    /// On getting, no serialization is performed, the returned string does not contain any
    /// markup. No whitespace normalization is performed and the returned string does not
    /// contain the white spaces in element content (see the attribute
    /// Text.isElementContentWhitespace). Similarly, on setting, no parsing is performed
    /// either, the input string is taken as pure textual content.
    /// The string returned is made of the text content of this node depending on its type, as
    /// defined below:
    ///
    /// <omitted>
    ///
    /// Exceptions on retrieval
    ///     DOMException
    ///     DOMSTRING_SIZE_ERR: Raised when it would return more characters than fit in a
    ///                         DOMString variable on the implementation platform.
    /// ```
    fn text_content(&self) -> Option<String> {
        use NodeRef::*;

        let mut children = self.first_child();
        let mut res = String::new();
        while let Some(child) = children {
            children = child.next_sibling();

            match child {
                Comment(_) | ProcessingInstruction(_) => {}
                child => {
                    if let Some(value) = child.text_content().filter(|text| !text.is_empty()) {
                        res.push_str(&value);
                    }
                }
            }
        }
        Some(res)
    }

    /// Implementation of [`textContent`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Node3-textContent) attribute.
    ///
    /// # Specification
    /// ```text
    /// This attribute returns the text content of this node and its descendants. When it is
    /// defined to be null, setting it has no effect. On setting, any possible children this
    /// node may have are removed and, if it the new string is not empty or null, replaced by
    /// a single Text node containing the string this attribute is set to.
    /// On getting, no serialization is performed, the returned string does not contain any
    /// markup. No whitespace normalization is performed and the returned string does not
    /// contain the white spaces in element content (see the attribute
    /// Text.isElementContentWhitespace). Similarly, on setting, no parsing is performed
    /// either, the input string is taken as pure textual content.
    /// The string returned is made of the text content of this node depending on its type, as
    /// defined below:
    ///
    /// <omitted>
    ///
    /// Exceptions on setting
    ///     DOMException
    ///     NO_MODIFICATION_ALLOWED_ERR: Raised when the node is readonly.
    /// ```
    fn set_text_content(&mut self, text: impl Into<String>) -> Result<(), DOMException> {
        let mut children = self.first_child();
        while let Some(child) = children {
            children = child.next_sibling();
            self.remove_child(child).ok();
        }

        let mut doc = self.owner_document().expect("Internal Error");
        let text = doc.create_text_node(text);
        self.append_child(text.into()).ok();
        Ok(())
    }

    /// Implementation of [`isSameNode`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Node3-isSameNode) method.
    ///
    /// # Specification
    /// ```text
    /// Returns whether this node is the same node as the given one.
    /// This method provides a way to determine whether two Node references returned by the
    /// implementation reference the same object. When two Node references are references to
    /// the same object, even if through a proxy, the references may be used completely
    /// interchangeably, such that all attributes have the same values and calling the same
    /// DOM method on either reference always has exactly the same effect.
    ///
    /// Parameters
    ///     other of type Node
    ///         The node to test against.
    ///
    /// Return Value
    ///     boolean Returns true if the nodes are the same, false otherwise.
    ///
    /// No Exceptions
    /// ```
    fn is_same_node(&self, other: &NodeRef) -> bool;

    /// Implementation of [`lookupPrefix`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Node3-lookupNamespacePrefix) method.
    ///
    /// The implementation was based on
    /// [Appendix B.2 Namespace Prefix Lookup](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#namespaces-algorithms-lookupNamespacePrefixAlgo).
    ///
    /// # Specification
    /// ```text
    /// Look up the prefix associated to the given namespace URI, starting from this node.
    /// The default namespace declarations are ignored by this method.
    /// See Namespace Prefix Lookup for details on the algorithm used by this method.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI to look for.
    ///
    /// Return Value
    ///     DOMString Returns an associated namespace prefix if found or null if none is
    ///               found. If more than one prefix are associated to the namespace prefix,
    ///               the returned namespace prefix is implementation dependent.
    ///
    /// No Exceptions
    /// ```
    fn lookup_prefix(&self, ns_uri: &str) -> Option<Rc<str>> {
        let mut ancestor = self.parent_node();
        while let Some(par) = ancestor {
            ancestor = par.parent_node();
            if let NodeRef::Element(elem) = par {
                return elem.lookup_prefix(ns_uri);
            }
        }
        None
    }

    /// Implementation of [`isDefaultNamespace`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Node3-isDefaultNamespace) method.
    ///
    /// The implementation was based on
    /// [Appendix B.3 Default Namespace Lookup](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#namespaces-algorithms-isDefaultNamespaceAlgo).
    ///
    /// # Specification
    /// ```text
    /// This method checks if the specified namespaceURI is the default namespace or not.
    ///
    /// Parameters
    ///     namespaceURI of type DOMString
    ///         The namespace URI to look for.
    ///
    /// Return Value
    ///     boolean Returns true if the specified namespaceURI is the default namespace,
    ///             false otherwise.
    ///
    /// No Exceptions
    /// ```
    fn is_default_namespace(&self, ns_uri: &str) -> bool {
        let mut ancestor = self.parent_node();
        while let Some(par) = ancestor {
            ancestor = par.parent_node();
            if let NodeRef::Element(elem) = par {
                return elem.is_default_namespace(ns_uri);
            }
        }
        false
    }

    /// Implementation of [`lookupNamespaceURI`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Node3-lookupNamespaceURI) method.
    ///
    /// The implementation was based on
    /// [Appendix B.4 Namespace URI Lookup](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#namespaces-algorithms-lookupNamespaceURIAlgo).
    ///
    /// # Specification
    /// ```text
    /// Look up the namespace URI associated to the given prefix, starting from this node.
    /// See Namespace URI Lookup for details on the algorithm used by this method.
    ///
    /// Parameters
    ///     prefix of type DOMString
    ///         The prefix to look for. If this parameter is null, the method will return
    ///         the default namespace URI if any.
    ///
    /// Return Value
    ///     DOMString Returns the associated namespace URI or null if none is found.
    ///
    /// No Exceptions
    /// ```
    fn lookup_namespace_uri(&self, prefix: &str) -> Option<Rc<str>> {
        let mut ancestor = self.parent_node();
        while let Some(par) = ancestor {
            ancestor = par.parent_node();
            if let NodeRef::Element(elem) = par {
                return elem.lookup_namespace_uri(prefix);
            }
        }
        None
    }

    /// Implementation of [`isEqualNode`](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-Node3-isEqualNode) method.
    ///
    /// # Note
    /// Normalization can affect equality, but this method does not normalize nodes.\
    /// If need, the caller should normalize nodes before calling this method.
    ///
    /// # Specification
    /// ```text
    /// Tests whether two nodes are equal.
    /// This method tests for equality of nodes, not sameness
    /// (i.e., whether the two nodes are references to the same object)
    /// which can be tested with Node.isSameNode().
    /// All nodes that are the same will also be equal, though the reverse may not be true.
    /// Two nodes are equal if and only if the following conditions are satisfied:
    ///
    /// - The two nodes are of the same type.
    /// - The following string attributes are equal: nodeName, localName, namespaceURI,
    ///   prefix, nodeValue. This is: they are both null, or they have the same length
    ///   and are character for character identical.
    /// - The attributes NamedNodeMaps are equal.
    ///   This is: they are both null, or they have the same length and for each node
    ///   that exists in one map there is a node that exists in the other map and is equal,
    ///   although not necessarily at the same index.
    /// - The childNodes NodeLists are equal.
    ///   This is: they are both null, or they have the same length and contain equal nodes
    ///   at the same index. Note that normalization can affect equality;
    ///   to avoid this, nodes should be normalized before being compared.
    ///
    /// For two DocumentType nodes to be equal, the following conditions must also be satisfied:
    ///
    /// - The following string attributes are equal: publicId, systemId, internalSubset.
    /// - The entities NamedNodeMaps are equal.
    /// - The notations NamedNodeMaps are equal.
    ///
    /// On the other hand, the following do not affect equality: the ownerDocument,
    /// baseURI, and parentNode attributes, the specified attribute for Attr nodes,
    /// the schemaTypeInfo attribute for Attr and Element nodes,
    /// the Text.isElementContentWhitespace attribute for Text nodes, as well as any user data
    /// or event listeners registered on the nodes.
    ///
    /// Note: As a general rule, anything not mentioned in the description above
    ///       is not significant in consideration of equality checking.
    ///       Note that future versions of this specification may take into account
    ///       more attributes and implementations conform to this specification are expected
    ///       to be updated accordingly.
    ///
    /// Parameters
    ///     arg of type Node
    ///         The node to compare equality with.
    ///
    /// Return Value
    ///     boolean Returns true if the nodes are equal, false otherwise.
    /// ```
    fn is_equal_node(&self, arg: &NodeRef) -> bool {
        if self.is_same_node(arg) {
            return true;
        }
        if self.node_type() != arg.node_type()
            || self.node_name() != arg.node_name()
            || self.local_name() != arg.local_name()
            || self.namespace_uri() != arg.namespace_uri()
            || self.prefix() != arg.prefix()
            || self.node_value() != arg.node_value()
        {
            return false;
        }

        match (self.attributes(), arg.attributes()) {
            (Some(lattrs), Some(rattrs)) => {
                if lattrs.len() != rattrs.len() {
                    return false;
                }

                let mut r = vec![];
                for i in 0..rattrs.len() {
                    r.push(rattrs.item(i).unwrap());
                }
                // Simply scanning and deleting.
                // Not efficient, but probably sufficient.
                for i in 0..lattrs.len() {
                    let attr = NodeRef::Attribute(lattrs.item(i).unwrap());
                    let Some(pos) = r.iter().position(|r| r.is_equal_node(&attr)) else {
                        return false;
                    };
                    r.swap_remove(pos);
                }
            }
            (None, None) => {}
            _ => return false,
        }

        let lch = self.child_nodes();
        let rch = arg.child_nodes();
        if lch.len() != rch.len() || lch.into_iter().zip(rch).any(|(l, r)| !l.is_equal_node(&r)) {
            return false;
        }

        true
    }
}

/// A set of operations that change the adjacency of nodes.
///
/// None of these methods are exposed to the user
/// because they may cause inconsistencies in the tree constraints.
///
/// These method does not check the tree constraints.  
/// It is the responsibility of the user to maintain tree constraints.
pub(super) trait NodeConnection: Clone + Into<NodeRef> {
    // The setter method (set_xxx) simply sets the given node
    // and is not required to check the node's type constraints.

    /// Set new parent node.  
    /// Return old parent node if exists.
    fn set_parent_node(&mut self, new_parent: Option<NodeRef>) -> Option<NodeRef>;
    /// Set new first child node.  
    /// Return old first child node if exists.
    fn set_first_child(&mut self, new_child: Option<NodeRef>) -> Option<NodeRef>;
    /// Set new last child node.  
    /// Return old last child node if exists.
    fn set_last_child(&mut self, new_child: Option<NodeRef>) -> Option<NodeRef>;
    /// Set new previous sibling node.  
    /// Return old previous sibling node if exists.
    fn set_previous_sibling(&mut self, new_sibling: Option<NodeRef>) -> Option<NodeRef>;
    /// Set new next sibling node.  
    /// Return old next sibling node if exists.
    fn set_next_sibling(&mut self, new_sibling: Option<NodeRef>) -> Option<NodeRef>;
    /// Set new owner Document node.  
    /// Return old owner Document node if exists.
    fn set_owner_document(&mut self, new_doc: DocumentRef) -> Option<DocumentRef>;

    /// Replace ownerDocument of self and all elements of the subtree.
    ///
    /// The ownerDocument of siblings and ancestors must not be changed.
    fn adopted_to(&mut self, new_doc: DocumentRef);

    /// Connect `self` to the front of `next`.
    ///
    /// If `next` has a previous sibling, it is connected to the front of `self`.  
    /// Otherwise, `self` is set as a first child of the parent of `next`.
    ///
    /// If `self` already has a parent or siblings, these will be disconnected from `self`.
    ///
    /// # Note
    /// If `next` does not have a parent,
    /// the constraint that the root node is unique is broken.
    fn connect_as_previous_sibling(&mut self, mut next: NodeRef) {
        let prev = next.set_previous_sibling(None);
        let par = next.parent_node();
        let slf: NodeRef = self.clone().into();
        self.disconnect_parent_and_sibling();
        // new parent
        self.set_parent_node(par.clone());
        // self <-> next
        next.set_previous_sibling(Some(slf.clone()));
        self.set_next_sibling(Some(next));
        if let Some(mut prev) = prev {
            // prev <-> self
            prev.set_next_sibling(Some(slf));
            self.set_previous_sibling(Some(prev));
        } else if let Some(mut par) = par {
            // If a previous sibling does not exists,
            // `self` becomes the first child of its parent.
            par.set_first_child(Some(slf));
        }
    }
    /// Connect `self` to the back of `prev`.
    ///
    /// If `prev` has a next sibling, it is connected to the back of `self`.  
    /// Otherwise, `self` is set as a last child of the parent of `prev`.
    ///
    /// If `self` already has a parent or siblings, these will be disconnected from `self`.
    ///
    /// # Note
    /// If `prev` does not have a parent,
    /// the constraint that the root node is unique is broken.
    fn connect_as_next_sibling(&mut self, mut prev: NodeRef) {
        let next = prev.set_next_sibling(None);
        let par = prev.parent_node();
        let slf: NodeRef = self.clone().into();
        self.disconnect_parent_and_sibling();
        // new parent
        self.set_parent_node(par.clone());
        // prev <-> self
        prev.set_next_sibling(Some(slf.clone()));
        self.set_previous_sibling(Some(prev));
        if let Some(mut next) = next {
            // self <-> next
            next.set_previous_sibling(Some(slf));
            self.set_next_sibling(Some(next));
        } else if let Some(mut par) = par {
            // If a next sibling does not exists,
            // `self` becomes the last child of its parent.
            par.set_last_child(Some(slf));
        }
    }
    /// Disconnect the parent node of `self`.  
    /// If `self` has siblings, it is also disconnected from its siblings,
    /// and the siblings before and after it establish adjacency.
    ///
    /// As a result, `self` is the root node of the detached subtree.
    ///
    /// Return old parent node if exists.
    fn disconnect_parent_and_sibling(&mut self) -> Option<NodeRef> {
        let mut par = self.set_parent_node(None);
        let prev = self.set_previous_sibling(None);
        let next = self.set_next_sibling(None);
        match (prev, next) {
            (Some(mut prev), Some(mut next)) => {
                prev.set_next_sibling(Some(next.clone()));
                next.set_previous_sibling(Some(prev));
            }
            (Some(mut prev), None) => {
                prev.set_next_sibling(None);
                if let Some(par) = par.as_mut() {
                    par.set_last_child(Some(prev));
                }
            }
            (None, Some(mut next)) => {
                next.set_previous_sibling(None);
                if let Some(par) = par.as_mut() {
                    par.set_first_child(Some(next));
                }
            }
            (None, None) => {
                if let Some(par) = par.as_mut() {
                    par.set_first_child(None);
                    par.set_last_child(None);
                }
            }
        }
        par
    }
}

/// Implementation of [Node](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1950641247)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
#[derive(Clone)]
pub enum NodeRef {
    Element(ElementRef),
    Attribute(AttrRef),
    Text(TextRef),
    CDATASection(CDATASectionRef),
    EntityReference(EntityReferenceRef),
    Entity(EntityRef),
    ProcessingInstruction(ProcessingInstructionRef),
    Comment(CommentRef),
    Document(DocumentRef),
    DocumentType(DocumentTypeRef),
    DocumentFragment(DocumentFragmentRef),
    Notation(NotationRef),
}

impl NodeRef {
    /// Generate [`NodeWeakRef`] from `self`.
    pub fn downgrade(&self) -> NodeWeakRef {
        use NodeRef::*;
        match self {
            Element(node) => NodeWeakRef::Element(node.downgrade()),
            Attribute(node) => NodeWeakRef::Attribute(node.downgrade()),
            Text(node) => NodeWeakRef::Text(node.downgrade()),
            CDATASection(node) => NodeWeakRef::CDATASection(node.downgrade()),
            EntityReference(node) => NodeWeakRef::EntityReference(node.downgrade()),
            Entity(node) => NodeWeakRef::Entity(node.downgrade()),
            ProcessingInstruction(node) => NodeWeakRef::ProcessingInstruction(node.downgrade()),
            Comment(node) => NodeWeakRef::Comment(node.downgrade()),
            Document(node) => NodeWeakRef::Document(node.downgrade()),
            DocumentType(node) => NodeWeakRef::DocumentType(node.downgrade()),
            DocumentFragment(node) => NodeWeakRef::DocumentFragment(node.downgrade()),
            Notation(node) => NodeWeakRef::Notation(node.downgrade()),
        }
    }
}

macro_rules! impl_node_trait_to_noderef {
    (
        $(
            fn $( ($mut:tt) )? $fn:ident($( $arg_name:ident : $arg_type:ty ),*) -> $ret:ty
        ),*
    ) => {
        impl Node for NodeRef {
            $(
                fn $fn(& $( $mut )? self, $( $arg_name: $arg_type),* ) -> $ret {
                    match self {
                        NodeRef::Element(elem) => <ElementRef as Node>::$fn(elem, $( $arg_name ),* ),
                        NodeRef::Attribute(attr) => attr.$fn( $( $arg_name ),* ),
                        NodeRef::Text(text) => text.$fn( $( $arg_name ),* ),
                        NodeRef::CDATASection(cdata) => cdata.$fn( $( $arg_name ),* ),
                        NodeRef::EntityReference(entref) => entref.$fn( $( $arg_name ),* ),
                        NodeRef::Entity(ent) => ent.$fn( $( $arg_name ),* ),
                        NodeRef::ProcessingInstruction(pi) => pi.$fn( $( $arg_name ),* ),
                        NodeRef::Comment(comment) => comment.$fn( $( $arg_name ),* ),
                        NodeRef::Document(doc) => doc.$fn( $( $arg_name ),* ),
                        NodeRef::DocumentType(doctype) => doctype.$fn( $( $arg_name ),* ),
                        NodeRef::DocumentFragment(frag) => frag.$fn( $( $arg_name ),* ),
                        NodeRef::Notation(nota) => nota.$fn( $( $arg_name ),* ),
                    }
                }
            )*
        }
    };
}

impl_node_trait_to_noderef! {
    fn node_name() -> Rc<str>,
    fn node_value() -> Option<Rc<str>>,
    fn node_type() -> NodeType,
    fn parent_node() -> Option<NodeRef>,
    fn child_nodes() -> Vec<NodeRef>,
    fn first_child() -> Option<NodeRef>,
    fn last_child() -> Option<NodeRef>,
    fn previous_sibling() -> Option<NodeRef>,
    fn next_sibling() -> Option<NodeRef>,
    fn attributes() -> Option<NamedNodeMap<AttrRef>>,
    fn owner_document() -> Option<DocumentRef>,
    fn(mut) insert_before(new_child: NodeRef, ref_child: Option<NodeRef>) -> Result<NodeRef, DOMException>,
    fn(mut) replace_child(new_child: NodeRef, old_child: NodeRef) -> Result<NodeRef, DOMException>,
    fn(mut) remove_child(old_child: NodeRef) -> Result<NodeRef, DOMException>,
    fn(mut) append_child(new_child: NodeRef) -> Result<NodeRef, DOMException>,
    fn has_child_nodes() -> bool,
    fn clone_node(deep: bool) -> NodeRef,
    fn(mut) normalize() -> (),
    fn namespace_uri() -> Option<Rc<str>>,
    fn prefix() -> Option<Rc<str>>,
    fn local_name() -> Option<Rc<str>>,
    fn has_attributes() -> bool,
    fn base_uri() -> Option<String>,
    fn compare_document_position(other: &NodeRef) -> DocumentPosition,
    fn text_content() -> Option<String>,
    fn is_same_node(other: &NodeRef) -> bool,
    fn lookup_prefix(ns_uri: &str) -> Option<Rc<str>>,
    fn is_default_namespace(ns_uri: &str) -> bool,
    fn lookup_namespace_uri(prefix: &str) -> Option<Rc<str>>,
    fn is_equal_node(arg: &NodeRef) -> bool
}

macro_rules! impl_node_connection_to_noderef {
    (
        $(
            $fn:ident($( $arg_name:ident : $arg_type:ty ),*) -> $ret:ty
        ),*
    ) => {
        impl NodeConnection for NodeRef {
            $(
                fn $fn(&mut self, $( $arg_name: $arg_type),* ) -> $ret {
                    match self {
                        NodeRef::Element(elem) => elem.$fn( $( $arg_name ),* ),
                        NodeRef::Attribute(attr) => attr.$fn( $( $arg_name ),* ),
                        NodeRef::Text(text) => text.$fn( $( $arg_name ),* ),
                        NodeRef::CDATASection(cdata) => cdata.$fn( $( $arg_name ),* ),
                        NodeRef::EntityReference(entref) => entref.$fn( $( $arg_name ),* ),
                        NodeRef::Entity(ent) => ent.$fn( $( $arg_name ),* ),
                        NodeRef::ProcessingInstruction(pi) => pi.$fn( $( $arg_name ),* ),
                        NodeRef::Comment(comment) => comment.$fn( $( $arg_name ),* ),
                        NodeRef::Document(doc) => doc.$fn( $( $arg_name ),* ),
                        NodeRef::DocumentType(doctype) => doctype.$fn( $( $arg_name ),* ),
                        NodeRef::DocumentFragment(frag) => frag.$fn( $( $arg_name ),* ),
                        NodeRef::Notation(nota) => nota.$fn( $( $arg_name ),* ),
                    }
                }
            )*
        }
    };
}

impl_node_connection_to_noderef! {
    set_parent_node(new_parent: Option<NodeRef>) -> Option<NodeRef>,
    set_first_child(new_child: Option<NodeRef>) -> Option<NodeRef>,
    set_last_child(new_child: Option<NodeRef>) -> Option<NodeRef>,
    set_previous_sibling(new_sibling: Option<NodeRef>) -> Option<NodeRef>,
    set_next_sibling(new_sibling: Option<NodeRef>) -> Option<NodeRef>,
    set_owner_document(new_doc: DocumentRef) -> Option<DocumentRef>,
    adopted_to(new_doc: DocumentRef) -> (),
    connect_as_previous_sibling(next: NodeRef) -> (),
    connect_as_next_sibling(prev: NodeRef) -> (),
    disconnect_parent_and_sibling() -> Option<NodeRef>
}

macro_rules! impl_node_conversion {
    ( $( ( $fn:ident, $var:ident, $t:ty ) ),* ) => {
        impl NodeRef {
            $(
                pub fn $fn (&self) -> Option<$t> {
                    match self {
                        NodeRef:: $var (node) => Some(node.clone()),
                        _ => None
                    }
                }
            )*
        }
    };
}

impl_node_conversion! {
    ( as_element, Element, ElementRef ),
    ( as_attribute, Attribute, AttrRef ),
    ( as_text_node, Text, TextRef ),
    ( as_cdata_section, CDATASection, CDATASectionRef ),
    ( as_entity_reference, EntityReference, EntityReferenceRef ),
    ( as_processing_instruction, ProcessingInstruction, ProcessingInstructionRef ),
    ( as_comment, Comment, CommentRef ),
    ( as_document, Document, DocumentRef ),
    ( as_document_type, DocumentType, DocumentTypeRef ),
    ( as_document_fragment, DocumentFragment, DocumentFragmentRef ),
    ( as_notation, Notation, NotationRef )
}

/// Implementation of [Node](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-1950641247)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
#[derive(Clone)]
pub enum NodeWeakRef {
    Element(ElementWeakRef),
    Attribute(AttrWeakRef),
    Text(TextWeakRef),
    CDATASection(CDATASectionWeakRef),
    EntityReference(EntityReferenceWeakRef),
    Entity(EntityWeakRef),
    ProcessingInstruction(ProcessingInstructionWeakRef),
    Comment(CommentWeakRef),
    Document(DocumentWeakRef),
    DocumentType(DocumentTypeWeakRef),
    DocumentFragment(DocumentFragmentWeakRef),
    Notation(NotationWeakRef),
}

impl NodeWeakRef {
    /// Generate [`NodeRef`] from `self`.  
    /// Success conditions are the same as for [`std::rc::Weak::upgrade`].
    pub fn upgrade(&self) -> Option<NodeRef> {
        match self {
            NodeWeakRef::Element(node) => node.upgrade().map(NodeRef::Element),
            NodeWeakRef::Attribute(node) => node.upgrade().map(NodeRef::Attribute),
            NodeWeakRef::Text(node) => node.upgrade().map(NodeRef::Text),
            NodeWeakRef::CDATASection(node) => node.upgrade().map(NodeRef::CDATASection),
            NodeWeakRef::EntityReference(node) => node.upgrade().map(NodeRef::EntityReference),
            NodeWeakRef::Entity(node) => node.upgrade().map(NodeRef::Entity),
            NodeWeakRef::ProcessingInstruction(node) => {
                node.upgrade().map(NodeRef::ProcessingInstruction)
            }
            NodeWeakRef::Comment(node) => node.upgrade().map(NodeRef::Comment),
            NodeWeakRef::Document(node) => node.upgrade().map(NodeRef::Document),
            NodeWeakRef::DocumentType(node) => node.upgrade().map(NodeRef::DocumentType),
            NodeWeakRef::DocumentFragment(node) => node.upgrade().map(NodeRef::DocumentFragment),
            NodeWeakRef::Notation(node) => node.upgrade().map(NodeRef::Notation),
        }
    }
}

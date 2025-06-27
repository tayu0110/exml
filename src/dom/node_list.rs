use crate::dom::{
    element::ElementRef,
    node::{Node, NodeRef},
};

/// Implementation of [NodeList](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-536297177)
/// interface on [1.4 Fundamental Interfaces: Core Module](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-BBACDC08)
///
/// # Specification
/// ```text
/// The NodeList interface provides the abstraction of an ordered collection of nodes,
/// without defining or constraining how this collection is implemented. NodeList objects
/// in the DOM are live.
///
/// The items in the NodeList are accessible via an integral index, starting from 0.
/// ```
pub trait NodeList {
    type Output: Node;

    /// Implementation of [item](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-844377136) method.
    ///
    /// # Note
    /// There are no guarantees on the complexity of this method.
    ///
    /// # Specification
    /// ```text
    /// Returns the indexth item in the collection. If index is greater than or equal to the
    /// number of nodes in the list, this returns null.
    ///
    /// Parameters
    ///     index of type unsigned long
    ///         Index into the collection.
    ///
    /// Return Value
    ///     Node The node at the indexth position in the NodeList, or null if that is not
    ///          a valid index.
    /// ```
    fn item(&self, index: usize) -> Option<Self::Output>;
    /// Implementation of [length](https://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/DOM3-Core.html#core-ID-203510337) attribute.
    ///
    /// # Note
    /// There are no guarantees on the complexity of this method.
    ///
    /// # Specification
    /// ```text
    /// length of type unsigned long, readonly
    ///     The number of nodes in the list. The range of valid child node indices is 0
    ///     to length-1 inclusive.
    /// ```
    fn length(&self) -> usize;
}

/// A [NodeList] that [Node::child_nodes] returns.
///
/// In the current implementation, both [`item`](`ChildNodesList::item`)
/// and [`length`](`ChildNodesList::length`) are the linear complexity of the number of child nodes.
pub struct ChildNodesList<N: Node> {
    owner_node: N,
}

impl<N: Node> ChildNodesList<N> {
    pub(super) fn new(owner_node: N) -> Self {
        Self { owner_node }
    }
}

impl<N: Node> NodeList for ChildNodesList<N> {
    type Output = NodeRef;

    fn item(&self, index: usize) -> Option<Self::Output> {
        let mut children = self.owner_node.first_child();
        let mut now = 0;
        while let Some(child) = children {
            if now == index {
                return Some(child);
            }
            now += 1;
            children = child.next_sibling();
        }
        None
    }

    fn length(&self) -> usize {
        let mut children = self.owner_node.first_child();
        let mut cnt = 0;
        while let Some(child) = children {
            cnt += 1;
            children = child.next_sibling();
        }
        cnt
    }
}

pub struct FilteredSubtreeElementsList {
    root: NodeRef,
    namespace_uri: Option<String>,
    local_name: String,
    // fn(element, namespace_uri, local_name)
    eq: fn(ElementRef, Option<&str>, &str) -> bool,
}

impl FilteredSubtreeElementsList {
    pub(super) fn new(
        root: NodeRef,
        namespace_uri: Option<String>,
        local_name: String,
        eq: fn(ElementRef, Option<&str>, &str) -> bool,
    ) -> Self {
        Self {
            root,
            namespace_uri,
            local_name,
            eq,
        }
    }

    /// If an `index` element with `eq` returning `true` is found, wrap it with `Ok` and return it.
    /// Otherwise, wrap the found count with `Err` and return it.
    fn seek(
        &self,
        index: usize,
        eq: impl Fn(ElementRef, Option<&str>, &str) -> bool,
    ) -> Result<ElementRef, usize> {
        let mut descendant = self.root.first_child();
        let mut cnt = 0;
        while let Some(mut now) = descendant.filter(|dsc| !self.root.is_same_node(dsc)) {
            if let Some(elem) = now.as_element().filter(|elem| {
                eq(
                    elem.clone(),
                    self.namespace_uri.as_deref(),
                    &self.local_name,
                )
            }) {
                if cnt == index {
                    return Ok(elem);
                }
                cnt += 1;
            }

            if let Some(child) = now.first_child() {
                descendant = Some(child);
            } else if let Some(sibling) = now.next_sibling() {
                descendant = Some(sibling);
            } else {
                descendant = None;
                while let Some(par) = now
                    .parent_node()
                    .filter(|par| !par.is_same_node(&self.root))
                {
                    if let Some(sibling) = par.next_sibling() {
                        descendant = Some(sibling);
                        break;
                    }
                    now = par;
                }
            }
        }
        Err(cnt)
    }
}

impl NodeList for FilteredSubtreeElementsList {
    type Output = ElementRef;

    fn item(&self, index: usize) -> Option<Self::Output> {
        self.seek(index, self.eq).ok()
    }

    fn length(&self) -> usize {
        self.seek(usize::MAX, self.eq).err().unwrap_or(usize::MAX)
    }
}

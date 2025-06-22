use crate::dom::node::{Node, NodeRef};

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

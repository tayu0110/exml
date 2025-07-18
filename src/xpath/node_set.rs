use std::collections::HashSet;

use crate::{
    tree::{
        NodeCommon, XmlElementType, XmlGenericNodePtr, XmlNodePtr, XmlNsPtr, xml_free_node_list,
    },
    xpath::xml_xpath_node_set_free_ns,
};

use super::{
    XmlXPathObject, xml_xpath_cast_node_to_string, xml_xpath_cmp_nodes_ext, xml_xpath_err_memory,
    xml_xpath_new_node_set, xml_xpath_node_set_dup_ns,
};

// when evaluating an XPath expression nodesets are created and we
// arbitrary limit the maximum length of those node set. 10000000 is
// an insanely large value which should never be reached under normal
// circumstances, one would first need to construct an in memory tree
// with more than 10 millions nodes.
const XPATH_MAX_NODESET_LENGTH: usize = 10000000;

/// A node-set (an unordered collection of nodes without duplicates).
#[repr(C)]
#[derive(Clone, PartialEq, Default)]
pub struct XmlNodeSet {
    // array of nodes in no particular order
    pub node_tab: Vec<XmlGenericNodePtr>,
    // @@ with_ns to check whether namespace nodes should be looked at @@
}

impl XmlNodeSet {
    pub fn with_value(val: Option<XmlGenericNodePtr>) -> Option<Self> {
        let mut ret = Self::default();
        if let Some(val) = val {
            ret.node_tab = vec![];
            if let Ok(ns) = XmlNsPtr::try_from(val) {
                let ns_node =
                    xml_xpath_node_set_dup_ns(ns.node.or(ns.next.map(|next| next.into())), ns)?;

                ret.node_tab.push(ns_node);
            } else {
                ret.node_tab.push(val);
            }
        }
        Some(ret)
    }

    /// Checks whether @ns is empty or not.
    ///
    /// Returns %TRUE if @ns is an empty node-set.
    #[doc(alias = "xmlXPathNodeSetIsEmpty")]
    pub(crate) fn is_empty(&self) -> bool {
        self.node_tab.is_empty()
    }

    /// Implement a functionality similar to the DOM NodeList.length.
    ///
    /// Returns the number of nodes in the node-set.
    #[doc(alias = "xmlXPathNodeSetGetLength")]
    pub(crate) fn len(&self) -> usize {
        self.node_tab.len()
    }

    /// Implements a functionality similar to the DOM NodeList.item().
    ///
    /// Returns the xmlNodePtr at the given @index in @ns or NULL if
    /// @index is out of range (0 to length-1)
    #[doc(alias = "xmlXPathNodeSetItem")]
    pub(crate) fn get(&self, index: usize) -> Option<XmlGenericNodePtr> {
        self.node_tab.get(index).copied()
    }

    /// checks whether @cur contains @val
    ///
    /// Returns true (1) if @cur contains @val, false (0) otherwise
    #[doc(alias = "xmlXPathNodeSetContains")]
    pub fn contains(&self, val: Option<XmlGenericNodePtr>) -> bool {
        let Some(val) = val else {
            return false;
        };
        let table = &self.node_tab;
        if let Ok(ns1) = XmlNsPtr::try_from(val) {
            for &node in table {
                if let Ok(ns2) = XmlNsPtr::try_from(node) {
                    if ns1 == ns2 {
                        return true;
                    }
                    if ns1.node.is_some() && ns2.node == ns1.node && ns1.prefix() == ns2.prefix() {
                        return true;
                    }
                }
            }
        } else {
            for &node in table {
                if val == node {
                    return true;
                }
            }
        }
        false
    }

    /// Implements the EXSLT - Sets has-same-nodes function:
    ///    boolean set:has-same-node(node-set, node-set)
    ///
    /// Returns true (1) if @nodes1 shares any node with @nodes2, false (0) otherwise
    #[doc(alias = "xmlXPathHasSameNodes")]
    pub unsafe fn has_same_nodes(&self, other: &XmlNodeSet) -> bool {
        let t1 = &self.node_tab;
        let t2 = &other.node_tab;
        if t1.is_empty() || t2.is_empty() {
            return false;
        }
        t1.iter().any(|node| t2.contains(node))
    }

    /// Implements the EXSLT - Sets intersection() function:
    ///    node-set set:intersection (node-set, node-set)
    ///
    /// Returns a node set comprising the nodes that are within both the
    /// node sets passed as arguments
    #[doc(alias = "xmlXPathIntersection")]
    pub fn intersection(&self, other: &XmlNodeSet) -> Option<Box<XmlNodeSet>> {
        let mut ret = xml_xpath_node_set_create(None)?;
        let t1 = &self.node_tab;
        let t2 = &other.node_tab;
        if t1.is_empty() || t2.is_empty() {
            return Some(ret);
        }

        for &node in t1 {
            if t2.contains(&node) {
                // TODO: Propagate memory error.
                if ret.as_mut().add_unique(node) < 0 {
                    break;
                }
            }
        }
        Some(ret)
    }

    /// Free the NodeSet compound.
    ///
    /// If `free_actual_tree` is `true`, free the actual tree also.
    #[doc(alias = "xmlXPathFreeNodeSet", alias = "xmlXPathFreeValueTree")]
    pub(crate) fn cleanup(&mut self, free_actual_tree: bool) {
        unsafe {
            let table = &mut self.node_tab;
            while let Some(node) = table.pop() {
                if let Ok(ns) = XmlNsPtr::try_from(node) {
                    xml_xpath_node_set_free_ns(ns);
                } else if free_actual_tree {
                    xml_free_node_list(Some(node));
                }
            }
        }
    }

    /// Sort the node set in document order
    #[doc(alias = "xmlXPathNodeSetSort")]
    pub fn sort(&mut self) {
        // Use the old Shell's sort implementation to sort the node-set
        // Timsort ought to be quite faster
        let table = &mut self.node_tab;
        // TODO: Use `sort_unstable` of Rust standard library.
        //       When I tried to rewirte, it did not work fine
        //       because `xml_xpath_cmp_nodes_ext` does not satisfy "total order" constraint.
        let len = table.len();
        let mut incr = len;
        while {
            incr /= 2;
            incr > 0
        } {
            for i in incr..len {
                let mut j = i as i32 - incr as i32;
                while j >= 0 {
                    if xml_xpath_cmp_nodes_ext(table[j as usize], table[j as usize + incr])
                        .is_some_and(|f| f.is_gt())
                    {
                        table.swap(j as usize, j as usize + incr);
                        j -= incr as i32;
                    } else {
                        break;
                    }
                }
            }
        }
    }

    /// Removes an entry from an existing NodeSet list.
    #[doc(alias = "xmlXPathNodeSetRemove")]
    pub fn remove(&mut self, val: i32) {
        if val >= self.node_tab.len() as i32 {
            return;
        }
        if let Ok(ns) = XmlNsPtr::try_from(self.node_tab[val as usize]) {
            xml_xpath_node_set_free_ns(ns);
        }
        self.node_tab.remove(val as usize);
    }

    /// Removes an xmlNodePtr from an existing NodeSet
    #[doc(alias = "xmlXPathNodeSetDel")]
    pub fn delete(&mut self, val: XmlGenericNodePtr) {
        // find node in nodeTab
        let Some(pos) = self.node_tab.iter().position(|&node| node == val) else {
            return;
        };
        if let Ok(ns) = XmlNsPtr::try_from(self.node_tab[pos]) {
            xml_xpath_node_set_free_ns(ns);
        }
        self.node_tab.remove(pos);
    }

    /// Clears the list from temporary XPath objects (e.g. namespace nodes
    /// are feed) starting with the entry at @pos, but does *not* free the list
    /// itself. Sets the length of the list to @pos.
    #[doc(alias = "xmlXPathNodeSetClearFromPos")]
    pub(super) fn truncate(&mut self, new_len: usize, has_ns_nodes: bool) {
        if new_len >= self.node_tab.len() {
            return;
        }
        if has_ns_nodes {
            for &node in &self.node_tab[new_len..] {
                if let Ok(ns) = XmlNsPtr::try_from(node) {
                    xml_xpath_node_set_free_ns(ns);
                }
            }
        }
        self.node_tab.truncate(new_len);
    }

    /// Clears the list from all temporary XPath objects (e.g. namespace nodes are feed),
    /// but does *not* free the list itself. Sets the length of the list to 0.
    #[doc(alias = "xmlXPathNodeSetClear")]
    pub(super) fn clear(&mut self, has_ns_nodes: bool) {
        self.truncate(0, has_ns_nodes);
    }

    /// Add a new xmlNodePtr to an existing NodeSet
    ///
    /// Returns 0 in case of success, and -1 in case of error
    #[doc(alias = "xmlXPathNodeSetAdd")]
    pub fn add(&mut self, val: XmlGenericNodePtr) -> i32 {
        // @@ with_ns to check whether namespace nodes should be looked at @@
        // prevent duplicates
        for &node in &self.node_tab {
            if node == val {
                return 0;
            }
        }

        // grow the nodeTab if needed
        if self.node_tab.len() >= XPATH_MAX_NODESET_LENGTH {
            xml_xpath_err_memory(None, Some("growing nodeset hit limit\n"));
            return -1;
        }
        if let Ok(ns) = XmlNsPtr::try_from(val) {
            let Some(ns_node) =
                xml_xpath_node_set_dup_ns(ns.node.or(ns.next.map(|next| next.into())), ns)
            else {
                return -1;
            };

            self.node_tab.push(ns_node);
        } else {
            self.node_tab.push(val);
        }
        0
    }

    /// Add a new xmlNodePtr to an existing NodeSet, optimized version
    /// when we are sure the node is not already in the set.
    ///
    /// Returns 0 in case of success and -1 in case of failure
    #[doc(alias = "xmlXPathNodeSetAddUnique")]
    pub fn add_unique(&mut self, val: XmlGenericNodePtr) -> i32 {
        // @@ with_ns to check whether namespace nodes should be looked at @@
        // grow the nodeTab if needed
        if self.node_tab.len() >= XPATH_MAX_NODESET_LENGTH {
            xml_xpath_err_memory(None, Some("growing nodeset hit limit\n"));
            return -1;
        }
        if let Ok(ns) = XmlNsPtr::try_from(val) {
            let Some(ns_node) =
                xml_xpath_node_set_dup_ns(ns.node.or(ns.next.map(|next| next.into())), ns)
            else {
                return -1;
            };

            self.node_tab.push(ns_node);
        } else {
            self.node_tab.push(val);
        }
        0
    }

    /// Add a new namespace node to an existing NodeSet
    ///
    /// Returns 0 in case of success and -1 in case of error
    #[doc(alias = "xmlXPathNodeSetAddNs")]
    pub fn add_ns(&mut self, node: XmlNodePtr, ns: XmlNsPtr) -> i32 {
        if !matches!(ns.typ, XmlElementType::XmlNamespaceDecl)
            || !matches!(node.element_type(), XmlElementType::XmlElementNode)
        {
            return -1;
        }

        // @@ with_ns to check whether namespace nodes should be looked at @@
        // prevent duplicates
        for &cur_node in &self.node_tab {
            if XmlNsPtr::try_from(cur_node)
                .ok()
                .filter(|cur_node| cur_node.node == Some(node.into()))
                .filter(|cur_node| ns.prefix() == cur_node.prefix())
                .is_some()
            {
                return 0;
            }
        }

        // grow the nodeTab if needed
        if self.node_tab.len() >= XPATH_MAX_NODESET_LENGTH {
            xml_xpath_err_memory(None, Some("growing nodeset hit limit\n"));
            return -1;
        }
        let Some(ns_node) = xml_xpath_node_set_dup_ns(Some(node.into()), ns) else {
            return -1;
        };
        self.node_tab.push(ns_node);
        0
    }
}

/// Create a new xmlNodeSetPtr of type f64 and of value @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNodeSetCreate")]
pub fn xml_xpath_node_set_create(val: Option<XmlGenericNodePtr>) -> Option<Box<XmlNodeSet>> {
    let set = XmlNodeSet::with_value(val)?;
    Some(Box::new(set))
}

/// Free the NodeSet compound (not the actual nodes !).
#[doc(alias = "xmlXPathFreeNodeSet")]
pub fn xml_xpath_free_node_set(obj: Option<Box<XmlNodeSet>>) {
    if let Some(mut obj) = obj {
        obj.as_mut().cleanup(false);
    }
}

/// Free the NodeSet compound and the actual tree, this is different from xmlXPathFreeNodeSet()
#[doc(alias = "xmlXPathFreeValueTree")]
pub fn xml_xpath_free_value_tree(obj: Option<Box<XmlNodeSet>>) {
    if let Some(mut obj) = obj {
        obj.as_mut().cleanup(true);
    }
}

/// Implements the EXSLT - Sets difference() function:
///    node-set set:difference (node-set, node-set)
///
/// Returns the difference between the two node sets.
///
/// # Note
/// In original libxml, if `node2` is empty, return `nodes1`.  
/// However, this function returns the clone of `nodes1`.
#[doc(alias = "xmlXPathDifference")]
pub fn xml_xpath_difference(
    nodes1: Option<&XmlNodeSet>,
    nodes2: Option<&XmlNodeSet>,
) -> Option<Box<XmlNodeSet>> {
    let Some(nodes2) = nodes2.filter(|n| !n.is_empty()) else {
        return nodes1.cloned().map(Box::new);
    };

    // TODO: Check memory error.
    let mut ret = xml_xpath_node_set_create(None);
    let Some(nodes1) = nodes1 else {
        return ret;
    };
    if nodes1.is_empty() {
        return ret;
    }

    let l1 = nodes1.len();

    if let Some(ret) = ret.as_mut() {
        for i in 0..l1 {
            let cur = nodes1.get(i).unwrap();
            if !nodes2.contains(Some(cur)) {
                // TODO: Propagate memory error.
                if ret.add_unique(cur) < 0 {
                    break;
                }
            }
        }
    }
    ret
}

/// Implements the EXSLT - Sets distinct() function:
///    node-set set:distinct (node-set)
///
/// Returns a subset of the nodes contained in @nodes.
///
/// # Note
/// In original libxml, if `nodes` is empty, return `nodes`.  
/// However, this function returns the clone of `nodes`.
#[doc(alias = "xmlXPathDistinctSorted")]
pub fn xml_xpath_distinct_sorted(nodes: Option<&XmlNodeSet>) -> Option<Box<XmlNodeSet>> {
    let nodes = nodes?;
    let mut ret = xml_xpath_node_set_create(None)?;
    if nodes.is_empty() {
        return Some(ret);
    }

    let l = nodes.len();
    let mut hash = HashSet::new();
    for i in 0..l {
        let cur = nodes.get(i).unwrap();
        let strval = xml_xpath_cast_node_to_string(Some(cur));
        if hash.insert(strval) && ret.as_mut().add_unique(cur) < 0 {
            xml_xpath_free_node_set(Some(ret));
            return None;
        }
    }
    Some(ret)
}

/// Implements the EXSLT - Sets distinct() function:
///    node-set set:distinct (node-set)
/// @nodes is sorted by document order, then #exslSetsDistinctSorted
/// is called with the sorted node-set
///
/// Returns a subset of the nodes contained in @nodes.
///
/// # Note
/// In original libxml, if `nodes` is empty, return `nodes`.  
/// However, this function returns the clone of `nodes`.
#[doc(alias = "xmlXPathDistinct")]
pub fn xml_xpath_distinct(nodes: Option<&mut XmlNodeSet>) -> Option<Box<XmlNodeSet>> {
    let nodes = nodes?;
    if nodes.is_empty() {
        return Some(Box::new(nodes.clone()));
    }

    nodes.sort();
    xml_xpath_distinct_sorted(Some(nodes))
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
///
/// Returns the nodes in @nodes that precede @node in document order,
/// an empty node-set if @nodes doesn't contain @node
///
/// # Note
/// In original libxml, if `node` is NULL, return `nodes`.  
/// However, this function returns the clone of `nodes`.
#[doc(alias = "xmlXPathNodeLeadingSorted")]
pub fn xml_xpath_node_leading_sorted(
    nodes: Option<&XmlNodeSet>,
    node: Option<XmlGenericNodePtr>,
) -> Option<Box<XmlNodeSet>> {
    let Some(node) = node else {
        return nodes.cloned().map(Box::new);
    };
    let mut ret = xml_xpath_node_set_create(None)?;
    let Some(nodes) = nodes.filter(|n| !n.is_empty() && n.contains(Some(node))) else {
        return Some(ret);
    };

    let l = nodes.len();
    for i in 0..l {
        let cur = nodes.get(i).unwrap();
        if cur == node {
            break;
        }
        // TODO: Propagate memory error.
        if ret.add_unique(cur) < 0 {
            break;
        }
    }
    Some(ret)
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
///
/// Returns the nodes in @nodes1 that precede the first node in @nodes2 in document order,
/// an empty node-set if @nodes1 doesn't contain @nodes2
///
/// # Note
/// In original libxml, if `nodes2` is NULL, return `nodes1`.  
/// However, this function returns the clone of `nodes1`.
#[doc(alias = "xmlXPathLeadingSorted")]
pub fn xml_xpath_leading_sorted(
    nodes1: Option<&XmlNodeSet>,
    nodes2: Option<&XmlNodeSet>,
) -> Option<Box<XmlNodeSet>> {
    let Some(nodes2) = nodes2.filter(|n| !n.is_empty()) else {
        return nodes1.cloned().map(Box::new);
    };
    xml_xpath_node_leading_sorted(nodes1, nodes2.get(1))
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
/// @nodes is sorted by document order, then #exslSetsNodeLeadingSorted
/// is called.
///
/// Returns the nodes in @nodes that precede @node in document order,
/// an empty node-set if @nodes doesn't contain @node
///
/// # Note
/// In original libxml, if `nodes2` is NULL, return `nodes1`.  
/// However, this function returns the clone of `nodes1`.
#[doc(alias = "xmlXPathNodeLeading")]
pub fn xml_xpath_node_leading(
    mut nodes: Option<&mut XmlNodeSet>,
    node: Option<XmlGenericNodePtr>,
) -> Option<Box<XmlNodeSet>> {
    if let Some(nodes) = nodes.as_deref_mut() {
        nodes.sort();
    }
    xml_xpath_node_leading_sorted(nodes.map(|n| &*n), node)
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
/// @nodes1 and @nodes2 are sorted by document order, then
/// #exslSetsLeadingSorted is called.
///
/// Returns the nodes in @nodes1 that precede the first node in @nodes2
/// in document order, or an empty node-set if @nodes1 doesn't contain @nodes2
///
/// # Note
/// In original libxml, if `nodes2` is NULL or empty, return `nodes1`.  
/// However, this function returns the clone of `nodes1`.
#[doc(alias = "xmlXPathLeading")]
pub fn xml_xpath_leading(
    nodes1: Option<&mut XmlNodeSet>,
    nodes2: Option<&mut XmlNodeSet>,
) -> Option<Box<XmlNodeSet>> {
    let Some(nodes2) = nodes2.filter(|n| !n.is_empty()) else {
        return nodes1.cloned().map(Box::new);
    };
    let Some(nodes1) = nodes1.filter(|n| !n.is_empty()) else {
        return xml_xpath_node_set_create(None);
    };
    nodes1.sort();
    nodes2.sort();
    xml_xpath_node_leading_sorted(Some(nodes1), nodes2.get(1))
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
///
/// Returns the nodes in @nodes that follow @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
///
/// # Note
/// In original libxml, if `node` is NULL, return `nodes`.  
/// However, this function returns the clone of `nodes`.
#[doc(alias = "xmlXPathNodeTrailingSorted")]
pub fn xml_xpath_node_trailing_sorted(
    nodes: &XmlNodeSet,
    node: Option<XmlGenericNodePtr>,
) -> Option<Box<XmlNodeSet>> {
    let Some(node) = node else {
        return Some(Box::new(nodes.clone()));
    };

    let mut ret = xml_xpath_node_set_create(None)?;
    if nodes.is_empty() || !nodes.contains(Some(node)) {
        return Some(ret);
    }

    let l = nodes.len();
    for i in (0..l).rev() {
        let cur = nodes.get(i).unwrap();
        if cur == node {
            break;
        }
        // TODO: Propagate memory error.
        if ret.as_mut().add_unique(cur) < 0 {
            break;
        }
    }
    ret.as_mut().sort(); /* bug 413451 */
    Some(ret)
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
///
/// Returns the nodes in @nodes1 that follow the first node in @nodes2
/// in document order, @nodes1 if @nodes2 is NULL or empty or
/// an empty node-set if @nodes1 doesn't contain @nodes2
///
/// # Note
/// In original libxml, if `nodes2` is NULL or empty, return `nodes1`.  
/// However, this function returns the clone of `nodes1`.
#[doc(alias = "xmlXPathTrailingSorted")]
pub fn xml_xpath_trailing_sorted(
    nodes1: &XmlNodeSet,
    nodes2: Option<&XmlNodeSet>,
) -> Option<Box<XmlNodeSet>> {
    let Some(nodes2) = nodes2.filter(|n| !n.is_empty()) else {
        return Some(Box::new(nodes1.clone()));
    };
    xml_xpath_node_trailing_sorted(nodes1, nodes2.get(0))
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
/// @nodes is sorted by document order, then #xmlXPathNodeTrailingSorted
/// is called.
///
/// Returns the nodes in @nodes that follow @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
///
/// # Note
/// In original libxml, if `nodes2` is NULL or empty, return `nodes1`.  
/// However, this function returns the clone of `nodes1`.
#[doc(alias = "xmlXPathNodeTrailing")]
pub fn xml_xpath_node_trailing(
    nodes: &mut XmlNodeSet,
    node: Option<XmlGenericNodePtr>,
) -> Option<Box<XmlNodeSet>> {
    nodes.sort();
    xml_xpath_node_trailing_sorted(nodes, node)
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
/// @nodes1 and @nodes2 are sorted by document order, then
/// #xmlXPathTrailingSorted is called.
///
/// Returns the nodes in @nodes1 that follow the first node in @nodes2
/// in document order, @nodes1 if @nodes2 is NULL or empty or
/// an empty node-set if @nodes1 doesn't contain @nodes2
///
/// # Note
/// In original libxml, if `nodes2` is NULL or empty, return `nodes1`.  
/// However, this function returns the clone of `nodes1`.
#[doc(alias = "xmlXPathTrailing")]
pub fn xml_xpath_trailing(
    nodes1: Option<&mut XmlNodeSet>,
    nodes2: Option<&mut XmlNodeSet>,
) -> Option<Box<XmlNodeSet>> {
    let Some(nodes2) = nodes2.filter(|n| !n.is_empty()) else {
        return nodes1.cloned().map(Box::new);
    };
    let Some(nodes1) = nodes1.filter(|n| !n.is_empty()) else {
        return xml_xpath_node_set_create(None);
    };
    nodes1.sort();
    nodes2.sort();
    xml_xpath_node_trailing_sorted(nodes1, nodes2.get(0))
}

/// Merges two nodesets, all nodes from @set2 are added to @set1.
/// Checks for duplicate nodes. Clears set2.
///
/// Returns @set1 once extended or NULL in case of error.
///
/// Frees @set1 in case of error.
///
/// # Note
/// In original libxml, if `nodes2` is NULL or empty, return `nodes1`.  
/// However, this function returns the clone of `nodes1`.
#[doc(alias = "xmlXPathNodeSetMergeAndClear")]
pub(super) fn xml_xpath_node_set_merge_and_clear(
    set1: Option<Box<XmlNodeSet>>,
    set2: Option<&mut XmlNodeSet>,
) -> Option<Box<XmlNodeSet>> {
    let mut set1 = set1?;
    let init_nb_set1 = set1.node_tab.len();
    let set2 = set2?;
    set2.node_tab.reverse();
    'b: while let Some(n2) = set2.node_tab.pop() {
        // Skip duplicates.
        for &n1 in &set1.node_tab[..init_nb_set1] {
            if n1 == n2 {
                // goto skip_node;
                continue 'b;
            }
            if let Some((n1, n2)) = XmlNsPtr::try_from(n1).ok().zip(XmlNsPtr::try_from(n2).ok()) {
                if n1.node == n2.node && n1.prefix() == n2.prefix() {
                    // Free the namespace node.
                    xml_xpath_node_set_free_ns(n2);
                    // goto skip_node;
                    continue 'b;
                }
            }
        }
        // grow the nodeTab if needed
        if set1.node_tab.len() >= XPATH_MAX_NODESET_LENGTH {
            xml_xpath_err_memory(None, Some("merging nodeset hit limit\n"));
            // goto error;
            xml_xpath_free_node_set(Some(set1));
            set2.clear(true);
            return None;
        }
        set1.node_tab.push(n2);
    }
    Some(set1)

    // error:
    // xmlXPathFreeNodeSet(set1);
    // xmlXPathNodeSetClear(set2, 1);
    // return null_mut();
}

/// Merges two nodesets, all nodes from @set2 are added to @set1.
/// Doesn't check for duplicate nodes. Clears set2.
///
/// Returns @set1 once extended or NULL in case of error.
///
/// Frees @set1 in case of error.
#[doc(alias = "xmlXPathNodeSetMergeAndClearNoDupls")]
pub(super) fn xml_xpath_node_set_merge_and_clear_no_dupls(
    set1: Option<Box<XmlNodeSet>>,
    set2: Option<&mut XmlNodeSet>,
) -> Option<Box<XmlNodeSet>> {
    let mut set1 = set1?;
    let set2 = set2?;
    set2.node_tab.reverse();
    while let Some(n2) = set2.node_tab.pop() {
        if set1.node_tab.len() >= XPATH_MAX_NODESET_LENGTH {
            xml_xpath_err_memory(None, Some("merging nodeset hit limit\n"));
            // goto error;
            xml_xpath_free_node_set(Some(set1));
            set2.clear(true);
            return None;
        }
        set1.node_tab.push(n2);
    }
    Some(set1)

    // error:
    //     xmlXPathFreeNodeSet(set1);
    //     xmlXPathNodeSetClear(set2, 1);
    //     return null_mut();
}

/// Merges two nodesets, all nodes from @val2 are added to @val1
/// if @val1 is NULL, a new set is created and copied from @val2
///
/// Returns @val1 once extended or NULL in case of error.
///
/// Frees @val1 in case of error.
#[doc(alias = "xmlXPathNodeSetMerge")]
pub fn xml_xpath_node_set_merge(
    val1: Option<Box<XmlNodeSet>>,
    val2: Option<&XmlNodeSet>,
) -> Option<Box<XmlNodeSet>> {
    let mut skip: i32;

    let Some(val2) = val2 else {
        return val1;
    };
    let mut val1 = val1.or_else(|| xml_xpath_node_set_create(None))?;

    // @@ with_ns to check whether namespace nodes should be looked at @@
    let init_nr = val1.as_ref().node_tab.len();

    for &n2 in &val2.node_tab {
        // check against duplicates
        skip = 0;
        for &n1 in &val1.node_tab[..init_nr] {
            if n1 == n2
                || XmlNsPtr::try_from(n1)
                    .ok()
                    .zip(XmlNsPtr::try_from(n2).ok())
                    .filter(|(n1, n2)| n1.node == n2.node)
                    .filter(|(n1, n2)| n1.prefix == n2.prefix)
                    .is_some()
            {
                skip = 1;
                break;
            }
        }
        if skip != 0 {
            continue;
        }

        // grow the nodeTab if needed
        if val1.node_tab.len() >= XPATH_MAX_NODESET_LENGTH {
            xml_xpath_err_memory(None, Some("merging nodeset hit limit\n"));
            // goto error;
            xml_xpath_free_node_set(Some(val1));
            return None;
        }
        if let Ok(ns) = XmlNsPtr::try_from(n2) {
            let Some(ns_node) =
                xml_xpath_node_set_dup_ns(ns.node.or(ns.next.map(|next| next.into())), ns)
            else {
                xml_xpath_free_node_set(Some(val1));
                return None;
            };

            val1.node_tab.push(ns_node);
        } else {
            val1.node_tab.push(n2);
        }
    }

    Some(val1)

    // error:
    // xmlXPathFreeNodeSet(val1);
    // return null_mut();
}

/// Create a new xmlXPathObjectPtr of type NodeSet and initialize
/// it with the Nodeset @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewNodeSetList")]
pub fn xml_xpath_new_node_set_list(val: Option<&mut XmlNodeSet>) -> Option<XmlXPathObject> {
    if let Some(val) = val {
        let mut ret = xml_xpath_new_node_set(Some(val.node_tab[0]));
        if let Some(nodeset) = ret.nodesetval.as_deref_mut() {
            for &node in &val.node_tab[1..] {
                // TODO: Propagate memory error.
                if nodeset.add_unique(node) < 0 {
                    break;
                }
            }
        }
        Some(ret)
    } else {
        None
    }
}

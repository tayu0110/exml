use std::{
    ffi::CString,
    ptr::{null_mut, NonNull},
};

use crate::{
    hash::XmlHashTable,
    libxml::xmlstring::xml_str_equal,
    tree::{xml_free_node_list, NodeCommon, NodePtr, XmlElementType, XmlNode, XmlNsPtr},
    xpath::xml_xpath_node_set_free_ns,
};

use super::{
    xml_xpath_cast_node_to_string, xml_xpath_cmp_nodes_ext, xml_xpath_err_memory,
    xml_xpath_new_node_set, xml_xpath_node_set_dup_ns, XmlXPathObjectPtr,
};

// when evaluating an XPath expression nodesets are created and we
// arbitrary limit the maximum length of those node set. 10000000 is
// an insanely large value which should never be reached under normal
// circumstances, one would first need to construct an in memory tree
// with more than 10 millions nodes.
const XPATH_MAX_NODESET_LENGTH: usize = 10000000;

// A node-set (an unordered collection of nodes without duplicates).
pub type XmlNodeSetPtr = *mut XmlNodeSet;
#[repr(C)]
#[derive(Debug, Default)]
pub struct XmlNodeSet {
    // array of nodes in no particular order
    pub node_tab: Option<Vec<*mut XmlNode>>,
    // @@ with_ns to check whether namespace nodes should be looked at @@
}

impl XmlNodeSet {
    pub unsafe fn with_value(val: *mut XmlNode) -> Option<Self> {
        let mut ret = Self::default();
        if !val.is_null() {
            ret.node_tab = Some(vec![]);
            if matches!((*val).element_type(), XmlElementType::XmlNamespaceDecl) {
                let ns = val as XmlNsPtr;
                let ns_node = xml_xpath_node_set_dup_ns((*ns).next as *mut XmlNode, ns);

                if ns_node.is_null() {
                    return None;
                }
                ret.node_tab.as_mut().unwrap().push(ns_node);
            } else {
                ret.node_tab.as_mut().unwrap().push(val);
            }
        }
        Some(ret)
    }

    /// Checks whether @ns is empty or not.
    ///
    /// Returns %TRUE if @ns is an empty node-set.
    #[doc(alias = "xmlXPathNodeSetIsEmpty")]
    pub(crate) fn is_empty(&self) -> bool {
        self.node_tab.as_ref().map_or(true, |t| t.is_empty())
    }

    /// Implement a functionality similar to the DOM NodeList.length.
    ///
    /// Returns the number of nodes in the node-set.
    #[doc(alias = "xmlXPathNodeSetGetLength")]
    pub(crate) fn len(&self) -> usize {
        self.node_tab.as_ref().map_or(0, |t| t.len())
    }

    /// Implements a functionality similar to the DOM NodeList.item().
    ///
    /// Returns the xmlNodePtr at the given @index in @ns or NULL if
    /// @index is out of range (0 to length-1)
    #[doc(alias = "xmlXPathNodeSetItem")]
    pub(crate) fn get(&self, index: usize) -> *mut XmlNode {
        *self
            .node_tab
            .as_deref()
            .and_then(|table| table.get(index))
            .unwrap_or(&null_mut())
    }

    /// checks whether @cur contains @val
    ///
    /// Returns true (1) if @cur contains @val, false (0) otherwise
    #[doc(alias = "xmlXPathNodeSetContains")]
    pub unsafe fn contains(&self, val: *mut XmlNode) -> bool {
        if val.is_null() {
            return false;
        }
        if let Some(table) = self.node_tab.as_ref() {
            if matches!((*val).element_type(), XmlElementType::XmlNamespaceDecl) {
                for &node in table {
                    if matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl) {
                        let ns1: XmlNsPtr = val as XmlNsPtr;
                        let ns2: XmlNsPtr = node as XmlNsPtr;
                        if ns1 == ns2 {
                            return true;
                        }
                        if !(*ns1).next.is_null()
                            && (*ns2).next == (*ns1).next
                            && xml_str_equal((*ns1).prefix as _, (*ns2).prefix as _)
                        {
                            return true;
                        }
                    }
                }
            } else {
                for &node in table {
                    if node == val {
                        return true;
                    }
                }
            }
        }
        false
    }

    /// Free the NodeSet compound.
    ///
    /// If `free_actual_tree` is `true`, free the actual tree also.
    #[doc(alias = "xmlXPathFreeNodeSet", alias = "xmlXPathFreeValueTree")]
    pub unsafe fn cleanup(&mut self, free_actual_tree: bool) {
        if let Some(table) = self.node_tab.as_mut() {
            while let Some(node) = table.pop() {
                if !node.is_null() {
                    if matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl) {
                        xml_xpath_node_set_free_ns(node as XmlNsPtr);
                    } else if free_actual_tree {
                        xml_free_node_list(node);
                    }
                }
            }
        }
    }

    /// Sort the node set in document order
    #[doc(alias = "xmlXPathNodeSetSort")]
    pub unsafe fn sort(&mut self) {
        // Use the old Shell's sort implementation to sort the node-set
        // Timsort ought to be quite faster
        if let Some(table) = self.node_tab.as_mut() {
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
                            .map_or(false, |f| f.is_gt())
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
    }

    /// Removes an entry from an existing NodeSet list.
    #[doc(alias = "xmlXPathNodeSetRemove")]
    pub unsafe fn remove(&mut self, val: i32) {
        if let Some(table) = self.node_tab.as_mut() {
            if val >= table.len() as i32 {
                return;
            }
            if !table[val as usize].is_null()
                && matches!(
                    (*(table[val as usize])).element_type(),
                    XmlElementType::XmlNamespaceDecl
                )
            {
                xml_xpath_node_set_free_ns(table[val as usize] as XmlNsPtr);
            }
            table.remove(val as usize);
        }
    }

    /// Removes an xmlNodePtr from an existing NodeSet
    #[doc(alias = "xmlXPathNodeSetDel")]
    pub unsafe fn delete(&mut self, val: *mut XmlNode) {
        if val.is_null() {
            return;
        }

        // find node in nodeTab
        if let Some(table) = self.node_tab.as_mut() {
            let Some(pos) = table.iter().position(|&node| node == val) else {
                return;
            };
            if !table[pos].is_null()
                && matches!(
                    (*table[pos]).element_type(),
                    XmlElementType::XmlNamespaceDecl
                )
            {
                xml_xpath_node_set_free_ns(table[pos] as XmlNsPtr);
            }
            table.remove(pos);
        }
    }

    /// Clears the list from temporary XPath objects (e.g. namespace nodes
    /// are feed) starting with the entry at @pos, but does *not* free the list
    /// itself. Sets the length of the list to @pos.
    #[doc(alias = "xmlXPathNodeSetClearFromPos")]
    pub(super) unsafe fn truncate(&mut self, new_len: usize, has_ns_nodes: bool) {
        let Some(table) = self.node_tab.as_mut().filter(|t| new_len >= t.len()) else {
            return;
        };
        if has_ns_nodes {
            for &node in &table[new_len..] {
                if !node.is_null()
                    && matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
                {
                    xml_xpath_node_set_free_ns(node as XmlNsPtr);
                }
            }
        }
        table.truncate(new_len);
    }

    /// Clears the list from all temporary XPath objects (e.g. namespace nodes are feed),
    /// but does *not* free the list itself. Sets the length of the list to 0.
    #[doc(alias = "xmlXPathNodeSetClear")]
    pub(super) unsafe fn clear(&mut self, has_ns_nodes: bool) {
        self.truncate(0, has_ns_nodes);
    }
}

/// Create a new xmlNodeSetPtr of type f64 and of value @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNodeSetCreate")]
pub unsafe fn xml_xpath_node_set_create(val: *mut XmlNode) -> Option<NonNull<XmlNodeSet>> {
    let set = XmlNodeSet::with_value(val)?;
    NonNull::new(Box::leak(Box::new(set)))
}

/// Free the NodeSet compound (not the actual nodes !).
#[doc(alias = "xmlXPathFreeNodeSet")]
pub unsafe fn xml_xpath_free_node_set(obj: Option<NonNull<XmlNodeSet>>) {
    if let Some(mut obj) = obj {
        obj.as_mut().cleanup(false);
        let _ = *Box::from_raw(obj.as_ptr());
    }
}

/// Free the NodeSet compound and the actual tree, this is different from xmlXPathFreeNodeSet()
#[doc(alias = "xmlXPathFreeValueTree")]
pub unsafe fn xml_xpath_free_value_tree(obj: Option<NonNull<XmlNodeSet>>) {
    if let Some(mut obj) = obj {
        obj.as_mut().cleanup(true);
        let _ = *Box::from_raw(obj.as_ptr());
    }
}

/// Implements the EXSLT - Sets difference() function:
///    node-set set:difference (node-set, node-set)
///
/// Returns the difference between the two node sets, or nodes1 if nodes2 is empty
#[doc(alias = "xmlXPathDifference")]
pub unsafe fn xml_xpath_difference(
    nodes1: Option<NonNull<XmlNodeSet>>,
    nodes2: Option<NonNull<XmlNodeSet>>,
) -> Option<NonNull<XmlNodeSet>> {
    let Some(nodes2) = nodes2.filter(|n| !n.as_ref().is_empty()) else {
        return nodes1;
    };

    // TODO: Check memory error.
    let ret = xml_xpath_node_set_create(null_mut());
    let Some(nodes1) = nodes1 else {
        return ret;
    };
    if nodes1.as_ref().is_empty() {
        return ret;
    }

    let l1 = nodes1.as_ref().len();

    for i in 0..l1 {
        let cur = nodes1.as_ref().get(i);
        if !nodes2.as_ref().contains(cur) {
            // TODO: Propagate memory error.
            if xml_xpath_node_set_add_unique(ret, cur) < 0 {
                break;
            }
        }
    }
    ret
}

/// Implements the EXSLT - Sets intersection() function:
///    node-set set:intersection (node-set, node-set)
///
/// Returns a node set comprising the nodes that are within both the
/// node sets passed as arguments
#[doc(alias = "xmlXPathIntersection")]
pub unsafe fn xml_xpath_intersection(
    nodes1: Option<NonNull<XmlNodeSet>>,
    nodes2: Option<NonNull<XmlNodeSet>>,
) -> Option<NonNull<XmlNodeSet>> {
    let ret = xml_xpath_node_set_create(null_mut())?;

    let (Some(nodes1), Some(nodes2)) = (nodes1, nodes2) else {
        return Some(ret);
    };
    if nodes1.as_ref().is_empty() || nodes2.as_ref().is_empty() {
        return Some(ret);
    }

    let l1 = nodes1.as_ref().len();

    for i in 0..l1 {
        let cur = nodes1.as_ref().get(i);
        if nodes2.as_ref().contains(cur) {
            /* TODO: Propagate memory error. */
            if xml_xpath_node_set_add_unique(Some(ret), cur) < 0 {
                break;
            }
        }
    }
    Some(ret)
}

/// Implements the EXSLT - Sets distinct() function:
///    node-set set:distinct (node-set)
///
/// Returns a subset of the nodes contained in @nodes, or @nodes if it is empty
#[doc(alias = "xmlXPathDistinctSorted")]
pub unsafe fn xml_xpath_distinct_sorted(
    nodes: Option<NonNull<XmlNodeSet>>,
) -> Option<NonNull<XmlNodeSet>> {
    let mut cur: *mut XmlNode;

    let nodes = nodes?;
    if nodes.as_ref().is_empty() {
        return Some(nodes);
    }

    let ret = xml_xpath_node_set_create(null_mut())?;
    let l = nodes.as_ref().len();
    let mut hash = XmlHashTable::with_capacity(l);
    for i in 0..l {
        cur = nodes.as_ref().get(i);
        let strval = xml_xpath_cast_node_to_string(cur);
        let strval = CString::new(strval).unwrap();
        if hash.lookup(&strval).is_none() {
            if hash.add_entry(&strval, ()).is_err() {
                xml_xpath_free_node_set(Some(ret));
                return None;
            }
            if xml_xpath_node_set_add_unique(Some(ret), cur) < 0 {
                xml_xpath_free_node_set(Some(ret));
                return None;
            }
        }
    }
    Some(ret)
}

/// Implements the EXSLT - Sets distinct() function:
///    node-set set:distinct (node-set)
/// @nodes is sorted by document order, then #exslSetsDistinctSorted
/// is called with the sorted node-set
///
/// Returns a subset of the nodes contained in @nodes, or @nodes if it is empty
#[doc(alias = "xmlXPathDistinct")]
pub unsafe fn xml_xpath_distinct(
    nodes: Option<NonNull<XmlNodeSet>>,
) -> Option<NonNull<XmlNodeSet>> {
    let mut nodes = nodes?;
    if nodes.as_ref().is_empty() {
        return Some(nodes);
    }

    nodes.as_mut().sort();
    xml_xpath_distinct_sorted(Some(nodes))
}

/// Implements the EXSLT - Sets has-same-nodes function:
///    boolean set:has-same-node(node-set, node-set)
///
/// Returns true (1) if @nodes1 shares any node with @nodes2, false (0) otherwise
#[doc(alias = "xmlXPathHasSameNodes")]
pub unsafe fn xml_xpath_has_same_nodes(
    nodes1: Option<NonNull<XmlNodeSet>>,
    nodes2: Option<NonNull<XmlNodeSet>>,
) -> i32 {
    let (Some(nodes1), Some(nodes2)) = (
        nodes1.filter(|n| !n.as_ref().is_empty()),
        nodes2.filter(|n| !n.as_ref().is_empty()),
    ) else {
        return 0;
    };

    let l = nodes1.as_ref().len();
    for i in 0..l {
        let cur = nodes1.as_ref().get(i);
        if nodes2.as_ref().contains(cur) {
            return 1;
        }
    }
    0
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
///
/// Returns the nodes in @nodes that precede @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
#[doc(alias = "xmlXPathNodeLeadingSorted")]
pub unsafe fn xml_xpath_node_leading_sorted(
    nodes: Option<NonNull<XmlNodeSet>>,
    node: *mut XmlNode,
) -> Option<NonNull<XmlNodeSet>> {
    if node.is_null() {
        return nodes;
    }
    let ret = xml_xpath_node_set_create(null_mut())?;
    let Some(nodes) = nodes.filter(|n| !n.as_ref().is_empty() && n.as_ref().contains(node)) else {
        return Some(ret);
    };

    let l = nodes.as_ref().len();
    for i in 0..l {
        let cur = nodes.as_ref().get(i);
        if cur == node {
            break;
        }
        // TODO: Propagate memory error.
        if xml_xpath_node_set_add_unique(Some(ret), cur) < 0 {
            break;
        }
    }
    Some(ret)
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
///
/// Returns the nodes in @nodes1 that precede the first node in @nodes2
///         in document order, @nodes1 if @nodes2 is NULL or empty or
///         an empty node-set if @nodes1 doesn't contain @nodes2
#[doc(alias = "xmlXPathLeadingSorted")]
pub unsafe fn xml_xpath_leading_sorted(
    nodes1: Option<NonNull<XmlNodeSet>>,
    nodes2: Option<NonNull<XmlNodeSet>>,
) -> Option<NonNull<XmlNodeSet>> {
    let Some(nodes2) = nodes2.filter(|n| !n.as_ref().is_empty()) else {
        return nodes1;
    };
    xml_xpath_node_leading_sorted(nodes1, nodes2.as_ref().get(1))
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
/// @nodes is sorted by document order, then #exslSetsNodeLeadingSorted
/// is called.
///
/// Returns the nodes in @nodes that precede @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
#[doc(alias = "xmlXPathNodeLeading")]
pub unsafe fn xml_xpath_node_leading(
    nodes: Option<NonNull<XmlNodeSet>>,
    node: *mut XmlNode,
) -> Option<NonNull<XmlNodeSet>> {
    if let Some(mut nodes) = nodes {
        nodes.as_mut().sort();
    }
    xml_xpath_node_leading_sorted(nodes, node)
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
/// @nodes1 and @nodes2 are sorted by document order, then
/// #exslSetsLeadingSorted is called.
///
/// Returns the nodes in @nodes1 that precede the first node in @nodes2
/// in document order, @nodes1 if @nodes2 is NULL or empty or
/// an empty node-set if @nodes1 doesn't contain @nodes2
#[doc(alias = "xmlXPathLeading")]
pub unsafe extern "C" fn xml_xpath_leading(
    nodes1: Option<NonNull<XmlNodeSet>>,
    nodes2: Option<NonNull<XmlNodeSet>>,
) -> Option<NonNull<XmlNodeSet>> {
    let Some(mut nodes2) = nodes2.filter(|n| !n.as_ref().is_empty()) else {
        return nodes1;
    };
    let Some(mut nodes1) = nodes1.filter(|n| !n.as_ref().is_empty()) else {
        return xml_xpath_node_set_create(null_mut());
    };
    nodes1.as_mut().sort();
    nodes2.as_mut().sort();
    xml_xpath_node_leading_sorted(Some(nodes1), nodes2.as_ref().get(1))
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
///
/// Returns the nodes in @nodes that follow @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
#[doc(alias = "xmlXPathNodeTrailingSorted")]
pub unsafe fn xml_xpath_node_trailing_sorted(
    nodes: NonNull<XmlNodeSet>,
    node: *mut XmlNode,
) -> Option<NonNull<XmlNodeSet>> {
    if node.is_null() {
        return Some(nodes);
    }

    let mut ret = xml_xpath_node_set_create(null_mut())?;
    if nodes.as_ref().is_empty() || !nodes.as_ref().contains(node) {
        return Some(ret);
    }

    let l = nodes.as_ref().len();
    for i in (0..l).rev() {
        let cur = nodes.as_ref().get(i);
        if cur == node {
            break;
        }
        // TODO: Propagate memory error.
        if xml_xpath_node_set_add_unique(Some(ret), cur) < 0 {
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
#[doc(alias = "xmlXPathTrailingSorted")]
pub unsafe fn xml_xpath_trailing_sorted(
    nodes1: NonNull<XmlNodeSet>,
    nodes2: Option<NonNull<XmlNodeSet>>,
) -> Option<NonNull<XmlNodeSet>> {
    let Some(nodes2) = nodes2.filter(|n| !n.as_ref().is_empty()) else {
        return Some(nodes1);
    };
    xml_xpath_node_trailing_sorted(nodes1, nodes2.as_ref().get(0))
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
/// @nodes is sorted by document order, then #xmlXPathNodeTrailingSorted
/// is called.
///
/// Returns the nodes in @nodes that follow @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
#[doc(alias = "xmlXPathNodeTrailing")]
pub unsafe fn xml_xpath_node_trailing(
    mut nodes: NonNull<XmlNodeSet>,
    node: *mut XmlNode,
) -> Option<NonNull<XmlNodeSet>> {
    nodes.as_mut().sort();
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
#[doc(alias = "xmlXPathTrailing")]
pub unsafe fn xml_xpath_trailing(
    nodes1: Option<NonNull<XmlNodeSet>>,
    nodes2: Option<NonNull<XmlNodeSet>>,
) -> Option<NonNull<XmlNodeSet>> {
    let Some(mut nodes2) = nodes2.filter(|n| !n.as_ref().is_empty()) else {
        return nodes1;
    };
    let Some(mut nodes1) = nodes1.filter(|n| !n.as_ref().is_empty()) else {
        return xml_xpath_node_set_create(null_mut());
    };
    nodes1.as_mut().sort();
    nodes2.as_mut().sort();
    xml_xpath_node_trailing_sorted(nodes1, nodes2.as_ref().get(0))
}

/// Add a new xmlNodePtr to an existing NodeSet, optimized version
/// when we are sure the node is not already in the set.
///
/// Returns 0 in case of success and -1 in case of failure
#[doc(alias = "xmlXPathNodeSetAddUnique")]
pub unsafe fn xml_xpath_node_set_add_unique(
    cur: Option<NonNull<XmlNodeSet>>,
    val: *mut XmlNode,
) -> i32 {
    let Some(mut cur) = cur else {
        return -1;
    };
    if val.is_null() {
        return -1;
    }

    // @@ with_ns to check whether namespace nodes should be looked at @@
    // grow the nodeTab if needed
    let table = cur.as_mut().node_tab.get_or_insert_with(Vec::new);
    if table.len() >= XPATH_MAX_NODESET_LENGTH {
        xml_xpath_err_memory(null_mut(), Some("growing nodeset hit limit\n"));
        return -1;
    }
    if matches!((*val).element_type(), XmlElementType::XmlNamespaceDecl) {
        let ns: XmlNsPtr = val as XmlNsPtr;
        let ns_node: *mut XmlNode = xml_xpath_node_set_dup_ns((*ns).next as *mut XmlNode, ns);

        if ns_node.is_null() {
            return -1;
        }
        table.push(ns_node);
    } else {
        table.push(val);
    }
    0
}

/// Add a new xmlNodePtr to an existing NodeSet
///
/// Returns 0 in case of success, and -1 in case of error
#[doc(alias = "xmlXPathNodeSetAdd")]
pub unsafe fn xml_xpath_node_set_add(cur: Option<NonNull<XmlNodeSet>>, val: *mut XmlNode) -> i32 {
    let Some(mut cur) = cur else {
        return -1;
    };
    if val.is_null() {
        return -1;
    }

    // @@ with_ns to check whether namespace nodes should be looked at @@
    // prevent duplicates
    if let Some(table) = cur.as_ref().node_tab.as_ref() {
        for &node in table {
            if node == val {
                return 0;
            }
        }
    }

    // grow the nodeTab if needed
    let table = cur.as_mut().node_tab.get_or_insert_with(Vec::new);
    if table.len() >= XPATH_MAX_NODESET_LENGTH {
        xml_xpath_err_memory(null_mut(), Some("growing nodeset hit limit\n"));
        return -1;
    }
    if matches!((*val).element_type(), XmlElementType::XmlNamespaceDecl) {
        let ns: XmlNsPtr = val as XmlNsPtr;
        let ns_node: *mut XmlNode = xml_xpath_node_set_dup_ns((*ns).next as *mut XmlNode, ns);

        if ns_node.is_null() {
            return -1;
        }
        table.push(ns_node);
    } else {
        table.push(val);
    }
    0
}

/// Add a new namespace node to an existing NodeSet
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "xmlXPathNodeSetAddNs")]
pub unsafe fn xml_xpath_node_set_add_ns(
    cur: Option<NonNull<XmlNodeSet>>,
    node: *mut XmlNode,
    ns: XmlNsPtr,
) -> i32 {
    let Some(mut cur) = cur else {
        return -1;
    };
    if ns.is_null()
        || node.is_null()
        || !matches!((*ns).typ, XmlElementType::XmlNamespaceDecl)
        || !matches!((*node).element_type(), XmlElementType::XmlElementNode)
    {
        return -1;
    }

    // @@ with_ns to check whether namespace nodes should be looked at @@
    // prevent duplicates
    if let Some(table) = cur.as_ref().node_tab.as_ref() {
        for &node in table {
            if node.is_null()
                && matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
                && (*node).next == NodePtr::from_ptr(node)
                && xml_str_equal((*ns).prefix as _, (*(node as XmlNsPtr)).prefix)
            {
                return 0;
            }
        }
    }

    // grow the nodeTab if needed
    let table = cur.as_mut().node_tab.get_or_insert_with(Vec::new);
    if table.len() >= XPATH_MAX_NODESET_LENGTH {
        xml_xpath_err_memory(null_mut(), Some("growing nodeset hit limit\n"));
        return -1;
    }
    let ns_node: *mut XmlNode = xml_xpath_node_set_dup_ns(node, ns);
    if ns_node.is_null() {
        return -1;
    }
    table.push(ns_node);
    0
}

/// Merges two nodesets, all nodes from @set2 are added to @set1.
/// Checks for duplicate nodes. Clears set2.
///
/// Returns @set1 once extended or NULL in case of error.
///
/// Frees @set1 in case of error.
#[doc(alias = "xmlXPathNodeSetMergeAndClear")]
pub(super) unsafe fn xml_xpath_node_set_merge_and_clear(
    set1: Option<NonNull<XmlNodeSet>>,
    set2: Option<NonNull<XmlNodeSet>>,
) -> Option<NonNull<XmlNodeSet>> {
    let init_nb_set1 = set1?.as_ref().node_tab.as_ref().map_or(0, |t| t.len());
    let mut set2 = set2?;
    if let Some(table) = set2.as_mut().node_tab.as_mut() {
        'b: for n2 in table.drain(..) {
            // Skip duplicates.
            if let Some(set1_table) = set1?.as_mut().node_tab.as_mut() {
                for &n1 in &set1_table[..init_nb_set1] {
                    if n1 == n2 {
                        // goto skip_node;
                        continue 'b;
                    } else if matches!((*n1).element_type(), XmlElementType::XmlNamespaceDecl)
                        && matches!((*n2).element_type(), XmlElementType::XmlNamespaceDecl)
                        && (*(n1 as XmlNsPtr)).next == (*(n2 as XmlNsPtr)).next
                        && xml_str_equal((*(n1 as XmlNsPtr)).prefix, (*(n2 as XmlNsPtr)).prefix)
                    {
                        // Free the namespace node.
                        xml_xpath_node_set_free_ns(n2 as XmlNsPtr);
                        // goto skip_node;
                        continue 'b;
                    }
                }
            }
            // grow the nodeTab if needed
            let set1_table = set1?.as_mut().node_tab.get_or_insert_with(Vec::new);
            if set1_table.len() >= XPATH_MAX_NODESET_LENGTH {
                xml_xpath_err_memory(null_mut(), Some("merging nodeset hit limit\n"));
                // goto error;
                xml_xpath_free_node_set(set1);
                set2.as_mut().clear(true);
                return None;
            }
            set1_table.push(n2);
        }
    }
    set1

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
pub(super) unsafe fn xml_xpath_node_set_merge_and_clear_no_dupls(
    set1: Option<NonNull<XmlNodeSet>>,
    set2: Option<NonNull<XmlNodeSet>>,
) -> Option<NonNull<XmlNodeSet>> {
    let mut set2 = set2?;
    if let Some(table) = set2.as_mut().node_tab.as_mut() {
        for n2 in table.drain(..) {
            let set1_table = set1?.as_mut().node_tab.get_or_insert_with(Vec::new);
            if set1_table.len() >= XPATH_MAX_NODESET_LENGTH {
                xml_xpath_err_memory(null_mut(), Some("merging nodeset hit limit\n"));
                // goto error;
                xml_xpath_free_node_set(set1);
                set2.as_mut().clear(true);
                return None;
            }
            set1_table.push(n2);
        }
    }
    set1

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
pub unsafe fn xml_xpath_node_set_merge(
    val1: Option<NonNull<XmlNodeSet>>,
    val2: Option<NonNull<XmlNodeSet>>,
) -> Option<NonNull<XmlNodeSet>> {
    let mut skip: i32;

    let Some(val2) = val2 else {
        return val1;
    };
    let mut val1 = val1.or_else(|| xml_xpath_node_set_create(null_mut()))?;

    // @@ with_ns to check whether namespace nodes should be looked at @@
    let init_nr = val1.as_ref().node_tab.as_ref().map_or(0, |t| t.len());

    if let Some(table) = val2.as_ref().node_tab.as_ref() {
        for &n2 in table {
            // check against duplicates
            skip = 0;
            let val1_table = val1.as_mut().node_tab.get_or_insert_with(Vec::new);
            for &n1 in &val1_table[..init_nr] {
                if n1 == n2
                    || ((matches!((*n1).element_type(), XmlElementType::XmlNamespaceDecl)
                        && matches!((*n2).element_type(), XmlElementType::XmlNamespaceDecl))
                        && ((*(n1 as XmlNsPtr)).next == (*(n2 as XmlNsPtr)).next
                            && xml_str_equal(
                                (*(n1 as XmlNsPtr)).prefix as _,
                                (*(n2 as XmlNsPtr)).prefix as _,
                            )))
                {
                    skip = 1;
                    break;
                }
            }
            if skip != 0 {
                continue;
            }

            // grow the nodeTab if needed
            if val1_table.len() >= XPATH_MAX_NODESET_LENGTH {
                xml_xpath_err_memory(null_mut(), Some("merging nodeset hit limit\n"));
                // goto error;
                xml_xpath_free_node_set(Some(val1));
                return None;
            }
            if matches!((*n2).element_type(), XmlElementType::XmlNamespaceDecl) {
                let ns: XmlNsPtr = n2 as XmlNsPtr;
                let ns_node: *mut XmlNode =
                    xml_xpath_node_set_dup_ns((*ns).next as *mut XmlNode, ns);

                if ns_node.is_null() {
                    // goto error;
                    xml_xpath_free_node_set(Some(val1));
                    return None;
                }
                val1_table.push(ns_node);
            } else {
                val1_table.push(n2);
            }
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
pub unsafe fn xml_xpath_new_node_set_list(val: Option<NonNull<XmlNodeSet>>) -> XmlXPathObjectPtr {
    if let Some(val) = val {
        if let Some(table) = val.as_ref().node_tab.as_ref() {
            let ret = xml_xpath_new_node_set(table[0]);
            if !ret.is_null() {
                for &node in &table[1..] {
                    // TODO: Propagate memory error.
                    if xml_xpath_node_set_add_unique((*ret).nodesetval, node) < 0 {
                        break;
                    }
                }
            }
            ret
        } else {
            xml_xpath_new_node_set(null_mut())
        }
    } else {
        null_mut()
    }
}
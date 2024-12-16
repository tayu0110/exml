use std::{ffi::CString, ptr::null_mut};

use crate::{
    hash::XmlHashTable,
    libxml::{
        globals::{xml_free, xml_malloc},
        xmlstring::xml_str_equal,
    },
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
    pub node_tab: Option<Vec<*mut XmlNode>>, /* array of nodes in no particular order */
                                             /* @@ with_ns to check whether namespace nodes should be looked at @@ */
}

/// Create a new xmlNodeSetPtr of type f64 and of value @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNodeSetCreate")]
pub unsafe fn xml_xpath_node_set_create(val: *mut XmlNode) -> XmlNodeSetPtr {
    let ret: XmlNodeSetPtr = xml_malloc(size_of::<XmlNodeSet>()) as XmlNodeSetPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), Some("creating nodeset\n"));
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlNodeSet::default());
    if !val.is_null() {
        (*ret).node_tab = Some(vec![]);
        if matches!((*val).element_type(), XmlElementType::XmlNamespaceDecl) {
            let ns = val as XmlNsPtr;
            let ns_node = xml_xpath_node_set_dup_ns((*ns).next as *mut XmlNode, ns);

            if ns_node.is_null() {
                xml_xpath_free_node_set(ret);
                return null_mut();
            }
            (*ret).node_tab.as_mut().unwrap().push(ns_node);
        } else {
            (*ret).node_tab.as_mut().unwrap().push(val);
        }
    }
    ret
}

/// Free the NodeSet compound (not the actual nodes !).
#[doc(alias = "xmlXPathFreeNodeSet")]
pub unsafe fn xml_xpath_free_node_set(obj: XmlNodeSetPtr) {
    if obj.is_null() {
        return;
    }
    if let Some(mut table) = (*obj).node_tab.take() {
        // @@ with_ns to check whether namespace nodes should be looked at @@
        while let Some(node) = table.pop() {
            if !node.is_null() && matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
            {
                xml_xpath_node_set_free_ns(node as XmlNsPtr);
            }
        }
    }
    xml_free(obj as _);
}

/// Free the NodeSet compound and the actual tree, this is different from xmlXPathFreeNodeSet()
#[doc(alias = "xmlXPathFreeValueTree")]
pub unsafe fn xml_xpath_free_value_tree(obj: XmlNodeSetPtr) {
    if obj.is_null() {
        return;
    }

    if let Some(mut table) = (*obj).node_tab.take() {
        while let Some(node) = table.pop() {
            if !node.is_null() {
                if matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl) {
                    xml_xpath_node_set_free_ns(node as XmlNsPtr);
                } else {
                    xml_free_node_list(node);
                }
            }
        }
    }
    xml_free(obj as _);
}

/// checks whether @cur contains @val
///
/// Returns true (1) if @cur contains @val, false (0) otherwise
#[doc(alias = "xmlXPathNodeSetContains")]
pub unsafe fn xml_xpath_node_set_contains(cur: XmlNodeSetPtr, val: *mut XmlNode) -> i32 {
    if cur.is_null() || val.is_null() {
        return 0;
    }
    if let Some(table) = (*cur).node_tab.as_ref() {
        if matches!((*val).element_type(), XmlElementType::XmlNamespaceDecl) {
            for &node in table {
                if matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl) {
                    let ns1: XmlNsPtr = val as XmlNsPtr;
                    let ns2: XmlNsPtr = node as XmlNsPtr;
                    if ns1 == ns2 {
                        return 1;
                    }
                    if !(*ns1).next.is_null()
                        && (*ns2).next == (*ns1).next
                        && xml_str_equal((*ns1).prefix as _, (*ns2).prefix as _)
                    {
                        return 1;
                    }
                }
            }
        } else {
            for &node in table {
                if node == val {
                    return 1;
                }
            }
        }
    }
    0
}

/// Checks whether @ns is empty or not.
///
/// Returns %TRUE if @ns is an empty node-set.
#[doc(alias = "xmlXPathNodeSetIsEmpty")]
macro_rules! xml_xpath_node_set_is_empty {
    ($ns:expr) => {
        $ns.is_null() || (*$ns).node_tab.as_ref().map_or(true, |t| t.is_empty())
    };
}

/// Implement a functionality similar to the DOM NodeList.length.
///
/// Returns the number of nodes in the node-set.
#[doc(alias = "xmlXPathNodeSetGetLength")]
macro_rules! xml_xpath_node_set_get_length {
    ($ns:expr) => {
        if !$ns.is_null() {
            (*$ns).node_tab.as_ref().map_or(0, |t| t.len() as i32)
        } else {
            0
        }
    };
}

/// Implements a functionality similar to the DOM NodeList.item().
///
/// Returns the xmlNodePtr at the given @index in @ns or NULL if
/// @index is out of range (0 to length-1)
#[doc(alias = "xmlXPathNodeSetItem")]
macro_rules! xml_xpath_node_set_item {
    ($ns:expr, $index:expr) => {
        if !$ns.is_null()
            && $index >= 0
            && ($index as usize) < (*$ns).node_tab.as_ref().map_or(0, |t| t.len())
        {
            (*$ns).node_tab.as_ref().unwrap()[$index as usize]
        } else {
            null_mut()
        }
    };
}

/// Implements the EXSLT - Sets difference() function:
///    node-set set:difference (node-set, node-set)
///
/// Returns the difference between the two node sets, or nodes1 if nodes2 is empty
#[doc(alias = "xmlXPathDifference")]
pub unsafe extern "C" fn xml_xpath_difference(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let mut cur: *mut XmlNode;

    if xml_xpath_node_set_is_empty!(nodes2) {
        return nodes1;
    }

    /* TODO: Check memory error. */
    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    if xml_xpath_node_set_is_empty!(nodes1) {
        return ret;
    }

    let l1: i32 = xml_xpath_node_set_get_length!(nodes1);

    for i in 0..l1 {
        cur = xml_xpath_node_set_item!(nodes1, i);
        if xml_xpath_node_set_contains(nodes2, cur) == 0 {
            /* TODO: Propagate memory error. */
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
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    let mut cur: *mut XmlNode;

    if ret.is_null() {
        return ret;
    }
    if xml_xpath_node_set_is_empty!(nodes1) {
        return ret;
    }
    if xml_xpath_node_set_is_empty!(nodes2) {
        return ret;
    }

    let l1: i32 = xml_xpath_node_set_get_length!(nodes1);

    for i in 0..l1 {
        cur = xml_xpath_node_set_item!(nodes1, i);
        if xml_xpath_node_set_contains(nodes2, cur) != 0 {
            /* TODO: Propagate memory error. */
            if xml_xpath_node_set_add_unique(ret, cur) < 0 {
                break;
            }
        }
    }
    ret
}

/// Implements the EXSLT - Sets distinct() function:
///    node-set set:distinct (node-set)
///
/// Returns a subset of the nodes contained in @nodes, or @nodes if it is empty
#[doc(alias = "xmlXPathDistinctSorted")]
pub unsafe fn xml_xpath_distinct_sorted(nodes: XmlNodeSetPtr) -> XmlNodeSetPtr {
    let mut cur: *mut XmlNode;

    if xml_xpath_node_set_is_empty!(nodes) {
        return nodes;
    }

    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    if ret.is_null() {
        return ret;
    }
    let l: i32 = xml_xpath_node_set_get_length!(nodes);
    let mut hash = XmlHashTable::with_capacity(l as usize);
    for i in 0..l {
        cur = xml_xpath_node_set_item!(nodes, i);
        let strval = xml_xpath_cast_node_to_string(cur);
        let strval = CString::new(strval).unwrap();
        if hash.lookup(&strval).is_none() {
            if hash.add_entry(&strval, ()).is_err() {
                xml_xpath_free_node_set(ret);
                return null_mut();
            }
            if xml_xpath_node_set_add_unique(ret, cur) < 0 {
                xml_xpath_free_node_set(ret);
                return null_mut();
            }
        }
    }
    ret
}

/// Implements the EXSLT - Sets distinct() function:
///    node-set set:distinct (node-set)
/// @nodes is sorted by document order, then #exslSetsDistinctSorted
/// is called with the sorted node-set
///
/// Returns a subset of the nodes contained in @nodes, or @nodes if it is empty
#[doc(alias = "xmlXPathDistinct")]
pub unsafe fn xml_xpath_distinct(nodes: XmlNodeSetPtr) -> XmlNodeSetPtr {
    if xml_xpath_node_set_is_empty!(nodes) {
        return nodes;
    }

    xml_xpath_node_set_sort(&mut *nodes);
    xml_xpath_distinct_sorted(nodes)
}

/// Implements the EXSLT - Sets has-same-nodes function:
///    boolean set:has-same-node(node-set, node-set)
///
/// Returns true (1) if @nodes1 shares any node with @nodes2, false (0) otherwise
#[doc(alias = "xmlXPathHasSameNodes")]
pub unsafe fn xml_xpath_has_same_nodes(nodes1: XmlNodeSetPtr, nodes2: XmlNodeSetPtr) -> i32 {
    let mut cur: *mut XmlNode;

    if xml_xpath_node_set_is_empty!(nodes1) || xml_xpath_node_set_is_empty!(nodes2) {
        return 0;
    }

    let l: i32 = xml_xpath_node_set_get_length!(nodes1);
    for i in 0..l {
        cur = xml_xpath_node_set_item!(nodes1, i);
        if xml_xpath_node_set_contains(nodes2, cur) != 0 {
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
pub unsafe extern "C" fn xml_xpath_node_leading_sorted(
    nodes: XmlNodeSetPtr,
    node: *mut XmlNode,
) -> XmlNodeSetPtr {
    let mut cur: *mut XmlNode;

    if node.is_null() {
        return nodes;
    }

    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    if ret.is_null() {
        return ret;
    }
    if xml_xpath_node_set_is_empty!(nodes) || xml_xpath_node_set_contains(nodes, node) == 0 {
        return ret;
    }

    let l: i32 = xml_xpath_node_set_get_length!(nodes);
    for i in 0..l {
        cur = xml_xpath_node_set_item!(nodes, i);
        if cur == node {
            break;
        }
        /* TODO: Propagate memory error. */
        if xml_xpath_node_set_add_unique(ret, cur) < 0 {
            break;
        }
    }
    ret
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
///
/// Returns the nodes in @nodes1 that precede the first node in @nodes2
///         in document order, @nodes1 if @nodes2 is NULL or empty or
///         an empty node-set if @nodes1 doesn't contain @nodes2
#[doc(alias = "xmlXPathLeadingSorted")]
pub unsafe extern "C" fn xml_xpath_leading_sorted(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if xml_xpath_node_set_is_empty!(nodes2) {
        return nodes1;
    }
    xml_xpath_node_leading_sorted(nodes1, xml_xpath_node_set_item!(nodes2, 1))
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
/// @nodes is sorted by document order, then #exslSetsNodeLeadingSorted
/// is called.
///
/// Returns the nodes in @nodes that precede @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
#[doc(alias = "xmlXPathNodeLeading")]
pub unsafe fn xml_xpath_node_leading(nodes: XmlNodeSetPtr, node: *mut XmlNode) -> XmlNodeSetPtr {
    if !nodes.is_null() {
        xml_xpath_node_set_sort(&mut *nodes);
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
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if xml_xpath_node_set_is_empty!(nodes2) {
        return nodes1;
    }
    if xml_xpath_node_set_is_empty!(nodes1) {
        return xml_xpath_node_set_create(null_mut());
    }
    xml_xpath_node_set_sort(&mut *nodes1);
    xml_xpath_node_set_sort(&mut *nodes2);
    xml_xpath_node_leading_sorted(nodes1, xml_xpath_node_set_item!(nodes2, 1))
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
///
/// Returns the nodes in @nodes that follow @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
#[doc(alias = "xmlXPathNodeTrailingSorted")]
pub unsafe extern "C" fn xml_xpath_node_trailing_sorted(
    nodes: XmlNodeSetPtr,
    node: *mut XmlNode,
) -> XmlNodeSetPtr {
    let mut cur: *mut XmlNode;

    if node.is_null() {
        return nodes;
    }

    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    if ret.is_null() {
        return ret;
    }
    if xml_xpath_node_set_is_empty!(nodes) || xml_xpath_node_set_contains(nodes, node) == 0 {
        return ret;
    }

    let l: i32 = xml_xpath_node_set_get_length!(nodes);
    for i in (0..l).rev() {
        cur = xml_xpath_node_set_item!(nodes, i);
        if cur == node {
            break;
        }
        /* TODO: Propagate memory error. */
        if xml_xpath_node_set_add_unique(ret, cur) < 0 {
            break;
        }
    }
    xml_xpath_node_set_sort(&mut *ret); /* bug 413451 */
    ret
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
///
/// Returns the nodes in @nodes1 that follow the first node in @nodes2
/// in document order, @nodes1 if @nodes2 is NULL or empty or
/// an empty node-set if @nodes1 doesn't contain @nodes2
#[doc(alias = "xmlXPathTrailingSorted")]
pub unsafe extern "C" fn xml_xpath_trailing_sorted(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if xml_xpath_node_set_is_empty!(nodes2) {
        return nodes1;
    }
    xml_xpath_node_trailing_sorted(nodes1, xml_xpath_node_set_item!(nodes2, 0))
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
/// @nodes is sorted by document order, then #xmlXPathNodeTrailingSorted
/// is called.
///
/// Returns the nodes in @nodes that follow @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
#[doc(alias = "xmlXPathNodeTrailing")]
pub unsafe fn xml_xpath_node_trailing(nodes: XmlNodeSetPtr, node: *mut XmlNode) -> XmlNodeSetPtr {
    if !nodes.is_null() {
        xml_xpath_node_set_sort(&mut *nodes);
    }
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
pub unsafe extern "C" fn xml_xpath_trailing(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if xml_xpath_node_set_is_empty!(nodes2) {
        return nodes1;
    }
    if xml_xpath_node_set_is_empty!(nodes1) {
        return xml_xpath_node_set_create(null_mut());
    }
    xml_xpath_node_set_sort(&mut *nodes1);
    xml_xpath_node_set_sort(&mut *nodes2);
    xml_xpath_node_trailing_sorted(nodes1, xml_xpath_node_set_item!(nodes2, 0))
}

/// Add a new xmlNodePtr to an existing NodeSet, optimized version
/// when we are sure the node is not already in the set.
///
/// Returns 0 in case of success and -1 in case of failure
#[doc(alias = "xmlXPathNodeSetAddUnique")]
pub unsafe extern "C" fn xml_xpath_node_set_add_unique(
    cur: XmlNodeSetPtr,
    val: *mut XmlNode,
) -> i32 {
    if cur.is_null() || val.is_null() {
        return -1;
    }

    // @@ with_ns to check whether namespace nodes should be looked at @@
    // grow the nodeTab if needed
    let table = (*cur).node_tab.get_or_insert_with(Vec::new);
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
pub unsafe extern "C" fn xml_xpath_node_set_add(cur: XmlNodeSetPtr, val: *mut XmlNode) -> i32 {
    if cur.is_null() || val.is_null() {
        return -1;
    }

    // @@ with_ns to check whether namespace nodes should be looked at @@
    // prevent duplicates
    if let Some(table) = (*cur).node_tab.as_ref() {
        for &node in table {
            if node == val {
                return 0;
            }
        }
    }

    // grow the nodeTab if needed
    let table = (*cur).node_tab.get_or_insert_with(Vec::new);
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
pub unsafe extern "C" fn xml_xpath_node_set_add_ns(
    cur: XmlNodeSetPtr,
    node: *mut XmlNode,
    ns: XmlNsPtr,
) -> i32 {
    if cur.is_null()
        || ns.is_null()
        || node.is_null()
        || !matches!((*ns).typ, XmlElementType::XmlNamespaceDecl)
        || !matches!((*node).element_type(), XmlElementType::XmlElementNode)
    {
        return -1;
    }

    // @@ with_ns to check whether namespace nodes should be looked at @@
    // prevent duplicates
    if let Some(table) = (*cur).node_tab.as_ref() {
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
    let table = (*cur).node_tab.get_or_insert_with(Vec::new);
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

/// Clears the list from temporary XPath objects (e.g. namespace nodes
/// are feed) starting with the entry at @pos, but does *not* free the list
/// itself. Sets the length of the list to @pos.
#[doc(alias = "xmlXPathNodeSetClearFromPos")]
pub(super) unsafe fn xml_xpath_node_set_clear_from_pos(
    set: XmlNodeSetPtr,
    pos: i32,
    has_ns_nodes: i32,
) {
    if set.is_null() || pos >= (*set).node_tab.as_ref().map_or(0, |t| t.len() as i32) {
        return;
    }
    if has_ns_nodes != 0 {
        if let Some(table) = (*set).node_tab.as_ref() {
            for &node in &table[pos as usize..] {
                if !node.is_null()
                    && matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
                {
                    xml_xpath_node_set_free_ns(node as XmlNsPtr);
                }
            }
        }
    }
    if let Some(table) = (*set).node_tab.as_mut() {
        table.truncate(pos as usize);
    }
}

/// Clears the list from all temporary XPath objects (e.g. namespace nodes
/// are feed), but does *not* free the list itself. Sets the length of the list to 0.
#[doc(alias = "xmlXPathNodeSetClear")]
pub(super) unsafe extern "C" fn xml_xpath_node_set_clear(set: XmlNodeSetPtr, has_ns_nodes: i32) {
    xml_xpath_node_set_clear_from_pos(set, 0, has_ns_nodes);
}

/// Merges two nodesets, all nodes from @set2 are added to @set1.
/// Checks for duplicate nodes. Clears set2.
///
/// Returns @set1 once extended or NULL in case of error.
///
/// Frees @set1 in case of error.
#[doc(alias = "xmlXPathNodeSetMergeAndClear")]
pub(super) unsafe fn xml_xpath_node_set_merge_and_clear(
    set1: XmlNodeSetPtr,
    set2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let init_nb_set1 = (*set1).node_tab.as_ref().map_or(0, |t| t.len());
    if let Some(table) = (*set2).node_tab.as_mut() {
        'b: for n2 in table.drain(..) {
            // Skip duplicates.
            if let Some(set1_table) = (*set1).node_tab.as_mut() {
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
            let set1_table = (*set1).node_tab.get_or_insert_with(Vec::new);
            if set1_table.len() >= XPATH_MAX_NODESET_LENGTH {
                xml_xpath_err_memory(null_mut(), Some("merging nodeset hit limit\n"));
                // goto error;
                xml_xpath_free_node_set(set1);
                xml_xpath_node_set_clear(set2, 1);
                return null_mut();
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
    set1: XmlNodeSetPtr,
    set2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if let Some(table) = (*set2).node_tab.as_mut() {
        for n2 in table.drain(..) {
            let set1_table = (*set1).node_tab.get_or_insert_with(Vec::new);
            if set1_table.len() >= XPATH_MAX_NODESET_LENGTH {
                xml_xpath_err_memory(null_mut(), Some("merging nodeset hit limit\n"));
                // goto error;
                xml_xpath_free_node_set(set1);
                xml_xpath_node_set_clear(set2, 1);
                return null_mut();
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
pub unsafe extern "C" fn xml_xpath_node_set_merge(
    mut val1: XmlNodeSetPtr,
    val2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let mut skip: i32;

    if val2.is_null() {
        return val1;
    }
    if val1.is_null() {
        val1 = xml_xpath_node_set_create(null_mut());
        if val1.is_null() {
            return null_mut();
        }
    }

    // @@ with_ns to check whether namespace nodes should be looked at @@
    let init_nr = (*val1).node_tab.as_ref().map_or(0, |t| t.len());

    if let Some(table) = (*val2).node_tab.as_ref() {
        for &n2 in table {
            // check against duplicates
            skip = 0;
            let val1_table = (*val1).node_tab.get_or_insert_with(Vec::new);
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
                xml_xpath_free_node_set(val1);
                return null_mut();
            }
            if matches!((*n2).element_type(), XmlElementType::XmlNamespaceDecl) {
                let ns: XmlNsPtr = n2 as XmlNsPtr;
                let ns_node: *mut XmlNode =
                    xml_xpath_node_set_dup_ns((*ns).next as *mut XmlNode, ns);

                if ns_node.is_null() {
                    // goto error;
                    xml_xpath_free_node_set(val1);
                    return null_mut();
                }
                val1_table.push(ns_node);
            } else {
                val1_table.push(n2);
            }
        }
    }

    val1

    // error:
    // xmlXPathFreeNodeSet(val1);
    // return null_mut();
}

/// Removes an xmlNodePtr from an existing NodeSet
#[doc(alias = "xmlXPathNodeSetDel")]
pub unsafe extern "C" fn xml_xpath_node_set_del(cur: XmlNodeSetPtr, val: *mut XmlNode) {
    if cur.is_null() {
        return;
    }
    if val.is_null() {
        return;
    }

    // find node in nodeTab
    if let Some(table) = (*cur).node_tab.as_mut() {
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

/// Removes an entry from an existing NodeSet list.
#[doc(alias = "xmlXPathNodeSetRemove")]
pub unsafe extern "C" fn xml_xpath_node_set_remove(cur: XmlNodeSetPtr, val: i32) {
    if cur.is_null() {
        return;
    }
    if let Some(table) = (*cur).node_tab.as_mut() {
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

/// Create a new xmlXPathObjectPtr of type NodeSet and initialize
/// it with the Nodeset @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewNodeSetList")]
pub unsafe extern "C" fn xml_xpath_new_node_set_list(val: XmlNodeSetPtr) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr;

    if val.is_null() {
        ret = null_mut();
    } else if let Some(table) = (*val).node_tab.as_ref() {
        ret = xml_xpath_new_node_set(table[0]);
        if !ret.is_null() {
            for &node in &table[1..] {
                // TODO: Propagate memory error.
                if xml_xpath_node_set_add_unique((*ret).nodesetval, node) < 0 {
                    break;
                }
            }
        }
    } else {
        ret = xml_xpath_new_node_set(null_mut());
    }

    ret
}

/// Sort the node set in document order
#[doc(alias = "xmlXPathNodeSetSort")]
pub unsafe fn xml_xpath_node_set_sort(set: &mut XmlNodeSet) {
    // Use the old Shell's sort implementation to sort the node-set
    // Timsort ought to be quite faster
    if let Some(table) = set.node_tab.as_mut() {
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

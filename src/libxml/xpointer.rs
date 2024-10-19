//! Provide internal methods and data structures for handling XML Pointers.  
//! This module is based on `libxml/xpointer.h`, `xpointer.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, CStr},
    mem::size_of,
    ptr::{null, null_mut, NonNull},
};

#[cfg(feature = "libxml_xptr_locs")]
use libc::{c_void, memset};

use crate::{
    __xml_raise_error,
    error::{XmlErrorDomain, XmlErrorLevel},
    libxml::{
        globals::{xml_free, xml_malloc, xml_malloc_atomic},
        parser::xml_init_parser,
        tree::{XmlDocPtr, XmlElementType, XmlNodePtr},
        xmlerror::XmlParserErrors,
        xmlstring::{xml_str_equal, xml_strlen, XmlChar},
        xpath::{
            xml_xpath_free_object, xml_xpath_new_context, XmlNodeSetPtr, XmlXPathContextPtr,
            XmlXPathError, XmlXPathObjectPtr, XmlXPathObjectType, XmlXPathParserContextPtr,
        },
        xpath_internals::{
            value_pop, value_push, xml_xpath_eval_expr, xml_xpath_free_parser_context,
            xml_xpath_id_function, xml_xpath_new_node_set, xml_xpath_new_parser_context,
            xml_xpath_new_string, xml_xpath_parse_name, xml_xpath_parse_ncname,
            xml_xpath_register_ns, xml_xpath_root,
        },
    },
    CHECK_ERROR, CHECK_TYPE, XP_ERROR,
};
#[cfg(feature = "libxml_xptr_locs")]
use crate::{
    libxml::{
        globals::xml_realloc,
        tree::{
            xml_add_child, xml_add_next_sibling, xml_copy_node, xml_new_text, xml_new_text_len,
        },
        xmlstring::{xml_strchr, xml_strncmp},
        xpath::{xml_xpath_cmp_nodes, xml_xpath_object_copy, XmlXPathObject},
        xpath_internals::{
            xml_xpath_err, xml_xpath_evaluate_predicate_result, xml_xpath_register_func,
        },
    },
    CHECK_ARITY,
};

/*
 * A Location Set
 */
#[cfg(feature = "libxml_xptr_locs")]
pub type XmlLocationSetPtr = *mut XmlLocationSet;
#[cfg(feature = "libxml_xptr_locs")]
#[repr(C)]
pub struct XmlLocationSet {
    pub(crate) loc_nr: c_int,  /* number of locations in the set */
    pub(crate) loc_max: c_int, /* size of the array as allocated */
    pub(crate) loc_tab: *mut XmlXPathObjectPtr, /* array of locations */
}

#[cfg(feature = "libxml_xptr_locs")]
const XML_RANGESET_DEFAULT: usize = 10;

#[cfg(feature = "libxml_xptr_locs")]
macro_rules! STRANGE {
    () => {
        let file = std::ffi::CString::new(file!()).unwrap();
        $crate::xml_generic_error!(
            $crate::libxml::globals::xmlGenericErrorContext(),
            c"Internal error at %s:%d\n".as_ptr(),
            file.as_ptr(),
            line!()
        );
    };
}

/**
 * xmlXPtrErrMemory:
 * @extra:  extra information
 *
 * Handle a redefinition of attribute error
 */
unsafe extern "C" fn xml_xptr_err_memory(extra: *const c_char) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromXPointer,
        XmlParserErrors::XmlErrNoMemory,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        extra,
        null(),
        None,
        0,
        0,
        c"Memory allocation failed : %s\n".as_ptr(),
        extra
    );
}

/**
 * xmlXPtrNewLocationSetNodes:
 * @start:  the start NodePtr value
 * @end:  the end NodePtr value or NULL
 *
 * Create a new xmlXPathObjectPtr of type LocationSet and initialize
 * it with the single range made of the two nodes @start and @end
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_new_location_set_nodes(
    start: XmlNodePtr,
    end: XmlNodePtr,
) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xptr_err_memory(c"allocating locationset".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathLocationset;
    if end.is_null() {
        (*ret).user = xml_xptr_location_set_create(xml_xptr_new_collapsed_range(start)) as _;
    } else {
        (*ret).user = xml_xptr_location_set_create(xml_xptr_new_range_nodes(start, end)) as _;
    }
    ret
}

/*
 * Handling of location sets.
 */

/**
 * xmlXPtrLocationSetCreate:
 * @val:  an initial xmlXPathObjectPtr, or NULL
 *
 * Create a new xmlLocationSetPtr of type double and of value @val
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_location_set_create(
    val: XmlXPathObjectPtr,
) -> XmlLocationSetPtr {
    let ret: XmlLocationSetPtr = xml_malloc(size_of::<XmlLocationSet>()) as XmlLocationSetPtr;
    if ret.is_null() {
        xml_xptr_err_memory(c"allocating locationset".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlLocationSet>());
    if !val.is_null() {
        (*ret).loc_tab = xml_malloc(XML_RANGESET_DEFAULT * size_of::<XmlXPathObjectPtr>())
            as *mut XmlXPathObjectPtr;
        if (*ret).loc_tab.is_null() {
            xml_xptr_err_memory(c"allocating locationset".as_ptr());
            xml_free(ret as _);
            return null_mut();
        }
        memset(
            (*ret).loc_tab as _,
            0,
            XML_RANGESET_DEFAULT * size_of::<XmlXPathObjectPtr>(),
        );
        (*ret).loc_max = XML_RANGESET_DEFAULT as _;
        *(*ret).loc_tab.add((*ret).loc_nr as usize) = val;
        (*ret).loc_nr += 1;
    }
    ret
}

/**
 * xmlXPtrFreeLocationSet:
 * @obj:  the xmlLocationSetPtr to free
 *
 * Free the LocationSet compound (not the actual ranges !).
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_free_location_set(obj: XmlLocationSetPtr) {
    if obj.is_null() {
        return;
    }
    if !(*obj).loc_tab.is_null() {
        for i in 0..(*obj).loc_nr {
            xml_xpath_free_object(*(*obj).loc_tab.add(i as usize));
        }
        xml_free((*obj).loc_tab as _);
    }
    xml_free(obj as _);
}

/**
 * xmlXPtrLocationSetMerge:
 * @val1:  the first LocationSet
 * @val2:  the second LocationSet
 *
 * Merges two rangesets, all ranges from @val2 are added to @val1
 *
 * Returns val1 once extended or NULL in case of error.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_location_set_merge(
    val1: XmlLocationSetPtr,
    val2: XmlLocationSetPtr,
) -> XmlLocationSetPtr {
    if val1.is_null() {
        return null_mut();
    }
    if val2.is_null() {
        return val1;
    }

    /*
     * !!!!! this can be optimized a lot, knowing that both
     *       val1 and val2 already have unicity of their values.
     */
    for i in 0..(*val2).loc_nr {
        xml_xptr_location_set_add(val1, *(*val2).loc_tab.add(i as usize));
    }

    val1
}

/**
 * xmlXPtrNewRangeInternal:
 * @start:  the starting node
 * @startindex:  the start index
 * @end:  the ending point
 * @endindex:  the ending index
 *
 * Internal function to create a new xmlXPathObjectPtr of type range
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_new_range_internal(
    start: XmlNodePtr,
    startindex: c_int,
    end: XmlNodePtr,
    endindex: c_int,
) -> XmlXPathObjectPtr {
    /*
     * Namespace nodes must be copied (see xmlXPathNodeSetDupNs).
     * Disallow them for now.
     */
    if !start.is_null() && matches!((*start).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    if !end.is_null() && matches!((*end).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }

    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xptr_err_memory(c"allocating range".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathRange;
    (*ret).user = start as _;
    (*ret).index = startindex;
    (*ret).user2 = end as _;
    (*ret).index2 = endindex;
    ret
}

/**
 * xmlXPtrCmpPoints:
 * @node1:  the first node
 * @index1:  the first index
 * @node2:  the second node
 * @index2:  the second index
 *
 * Compare two points w.r.t document order
 *
 * Returns -2 in case of error 1 if first point < second point, 0 if
 *         that's the same point, -1 otherwise
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_cmp_points(
    node1: XmlNodePtr,
    index1: c_int,
    node2: XmlNodePtr,
    index2: c_int,
) -> c_int {
    if node1.is_null() || node2.is_null() {
        return -2;
    }
    /*
     * a couple of optimizations which will avoid computations in most cases
     */
    if node1 == node2 {
        if index1 < index2 {
            return 1;
        }
        if index1 > index2 {
            return -1;
        }
        return 0;
    }
    xml_xpath_cmp_nodes(node1, node2)
}

/**
 * xmlXPtrRangeCheckOrder:
 * @range:  an object range
 *
 * Make sure the points in the range are in the right order
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_range_check_order(range: XmlXPathObjectPtr) {
    let mut tmp: c_int;
    let tmp2: XmlNodePtr;
    if range.is_null() {
        return;
    }
    if !matches!((*range).typ, XmlXPathObjectType::XpathRange) {
        return;
    }
    if (*range).user2.is_null() {
        return;
    }
    tmp = xml_xptr_cmp_points(
        (*range).user as _,
        (*range).index,
        (*range).user2 as _,
        (*range).index2,
    );
    if tmp == -1 {
        tmp2 = (*range).user as _;
        (*range).user = (*range).user2;
        (*range).user2 = tmp2 as _;
        tmp = (*range).index;
        (*range).index = (*range).index2;
        (*range).index2 = tmp;
    }
}

/**
 * xmlXPtrNewRange:
 * @start:  the starting node
 * @startindex:  the start index
 * @end:  the ending point
 * @endindex:  the ending index
 *
 * Create a new xmlXPathObjectPtr of type range
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_new_range(
    start: XmlNodePtr,
    startindex: c_int,
    end: XmlNodePtr,
    endindex: c_int,
) -> XmlXPathObjectPtr {
    if start.is_null() {
        return null_mut();
    }
    if end.is_null() {
        return null_mut();
    }
    if startindex < 0 {
        return null_mut();
    }
    if endindex < 0 {
        return null_mut();
    }

    let ret: XmlXPathObjectPtr = xml_xptr_new_range_internal(start, startindex, end, endindex);
    xml_xptr_range_check_order(ret);
    ret
}

/**
 * xmlXPtrNewRangePoints:
 * @start:  the starting point
 * @end:  the ending point
 *
 * Create a new xmlXPathObjectPtr of type range using 2 Points
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_new_range_points(
    start: XmlXPathObjectPtr,
    end: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    if start.is_null() {
        return null_mut();
    }
    if end.is_null() {
        return null_mut();
    }
    if !matches!((*start).typ, XmlXPathObjectType::XpathPoint) {
        return null_mut();
    }
    if !matches!((*end).typ, XmlXPathObjectType::XpathPoint) {
        return null_mut();
    }

    let ret: XmlXPathObjectPtr = xml_xptr_new_range_internal(
        (*start).user as _,
        (*start).index,
        (*end).user as _,
        (*end).index,
    );
    xml_xptr_range_check_order(ret);
    ret
}

/**
 * xmlXPtrNewRangeNodePoint:
 * @start:  the starting node
 * @end:  the ending point
 *
 * Create a new xmlXPathObjectPtr of type range from a node to a point
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_new_range_node_point(
    start: XmlNodePtr,
    end: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    if start.is_null() {
        return null_mut();
    }
    if end.is_null() {
        return null_mut();
    }
    if !matches!((*end).typ, XmlXPathObjectType::XpathPoint) {
        return null_mut();
    }

    let ret: XmlXPathObjectPtr =
        xml_xptr_new_range_internal(start, -1, (*end).user as _, (*end).index);
    xml_xptr_range_check_order(ret);
    ret
}

/**
 * xmlXPtrNewRangePointNode:
 * @start:  the starting point
 * @end:  the ending node
 *
 * Create a new xmlXPathObjectPtr of type range from a point to a node
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_new_range_point_node(
    start: XmlXPathObjectPtr,
    end: XmlNodePtr,
) -> XmlXPathObjectPtr {
    if start.is_null() {
        return null_mut();
    }
    if end.is_null() {
        return null_mut();
    }
    if !matches!((*start).typ, XmlXPathObjectType::XpathPoint) {
        return null_mut();
    }

    let ret: XmlXPathObjectPtr =
        xml_xptr_new_range_internal((*start).user as _, (*start).index, end, -1);
    xml_xptr_range_check_order(ret);
    ret
}

/**
 * xmlXPtrNewRangeNodes:
 * @start:  the starting node
 * @end:  the ending node
 *
 * Create a new xmlXPathObjectPtr of type range using 2 nodes
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_new_range_nodes(
    start: XmlNodePtr,
    end: XmlNodePtr,
) -> XmlXPathObjectPtr {
    if start.is_null() {
        return null_mut();
    }
    if end.is_null() {
        return null_mut();
    }

    let ret: XmlXPathObjectPtr = xml_xptr_new_range_internal(start, -1, end, -1);
    xml_xptr_range_check_order(ret);
    ret
}

/**
 * xmlXPtrNewLocationSetNodeSet:
 * @set:  a node set
 *
 * Create a new xmlXPathObjectPtr of type LocationSet and initialize
 * it with all the nodes from @set
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_new_location_set_node_set(
    set: XmlNodeSetPtr,
) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xptr_err_memory(c"allocating locationset".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathLocationset;
    if !set.is_null() {
        let newset: XmlLocationSetPtr = xml_xptr_location_set_create(null_mut());
        if newset.is_null() {
            return ret;
        }

        for i in 0..(*set).node_nr {
            xml_xptr_location_set_add(
                newset,
                xml_xptr_new_collapsed_range(*(*set).node_tab.add(i as usize)),
            );
        }

        (*ret).user = newset as _;
    }
    ret
}

/**
 * xmlXPtrNewRangeNodeObject:
 * @start:  the starting node
 * @end:  the ending object
 *
 * Create a new xmlXPathObjectPtr of type range from a not to an object
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_new_range_node_object(
    start: XmlNodePtr,
    end: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    let end_node: XmlNodePtr;
    let end_index: c_int;

    if start.is_null() {
        return null_mut();
    }
    if end.is_null() {
        return null_mut();
    }
    match (*end).typ {
        XmlXPathObjectType::XpathPoint => {
            end_node = (*end).user as _;
            end_index = (*end).index;
        }
        XmlXPathObjectType::XpathRange => {
            end_node = (*end).user2 as _;
            end_index = (*end).index2;
        }
        XmlXPathObjectType::XpathNodeset => {
            /*
             * Empty set ...
             */
            if (*end).nodesetval.is_null() || (*(*end).nodesetval).node_nr <= 0 {
                return null_mut();
            }
            end_node = *(*(*end).nodesetval)
                .node_tab
                .add((*(*end).nodesetval).node_nr as usize - 1);
            end_index = -1;
        }
        _ => {
            /* TODO */
            return null_mut();
        }
    }

    let ret: XmlXPathObjectPtr = xml_xptr_new_range_internal(start, -1, end_node, end_index);
    xml_xptr_range_check_order(ret);
    ret
}

/**
 * xmlXPtrNewCollapsedRange:
 * @start:  the starting and ending node
 *
 * Create a new xmlXPathObjectPtr of type range using a single nodes
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_new_collapsed_range(
    start: XmlNodePtr,
) -> XmlXPathObjectPtr {
    if start.is_null() {
        return null_mut();
    }

    let ret: XmlXPathObjectPtr = xml_xptr_new_range_internal(start, -1, null_mut(), -1);
    ret
}

/**
 * xmlXPtrRangesEqual:
 * @range1:  the first range
 * @range2:  the second range
 *
 * Compare two ranges
 *
 * Returns 1 if equal, 0 otherwise
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_ranges_equal(
    range1: XmlXPathObjectPtr,
    range2: XmlXPathObjectPtr,
) -> c_int {
    if range1 == range2 {
        return 1;
    }
    if range1.is_null() || range2.is_null() {
        return 0;
    }
    if (*range1).typ != (*range2).typ {
        return 0;
    }
    if (*range1).typ != XmlXPathObjectType::XpathRange {
        return 0;
    }
    if (*range1).user != (*range2).user {
        return 0;
    }
    if (*range1).index != (*range2).index {
        return 0;
    }
    if (*range1).user2 != (*range2).user2 {
        return 0;
    }
    if (*range1).index2 != (*range2).index2 {
        return 0;
    }
    1
}

/**
 * xmlXPtrLocationSetAdd:
 * @cur:  the initial range set
 * @val:  a new xmlXPathObjectPtr
 *
 * add a new xmlXPathObjectPtr to an existing LocationSet
 * If the location already exist in the set @val is freed.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_location_set_add(
    cur: XmlLocationSetPtr,
    val: XmlXPathObjectPtr,
) {
    if cur.is_null() || val.is_null() {
        return;
    }

    /*
     * check against doublons
     */
    for i in 0..(*cur).loc_nr {
        if xml_xptr_ranges_equal(*(*cur).loc_tab.add(i as usize), val) != 0 {
            xml_xpath_free_object(val);
            return;
        }
    }

    /*
     * grow the locTab if needed
     */
    if (*cur).loc_max == 0 {
        (*cur).loc_tab = xml_malloc(XML_RANGESET_DEFAULT * size_of::<XmlXPathObjectPtr>())
            as *mut XmlXPathObjectPtr;
        if (*cur).loc_tab.is_null() {
            xml_xptr_err_memory(c"adding location to set".as_ptr());
            return;
        }
        memset(
            (*cur).loc_tab as _,
            0,
            XML_RANGESET_DEFAULT * size_of::<XmlXPathObjectPtr>(),
        );
        (*cur).loc_max = XML_RANGESET_DEFAULT as _;
    } else if (*cur).loc_nr == (*cur).loc_max {
        (*cur).loc_max *= 2;
        let temp: *mut XmlXPathObjectPtr = xml_realloc(
            (*cur).loc_tab as _,
            (*cur).loc_max as usize * size_of::<XmlXPathObjectPtr>(),
        ) as *mut XmlXPathObjectPtr;
        if temp.is_null() {
            xml_xptr_err_memory(c"adding location to set".as_ptr());
            return;
        }
        (*cur).loc_tab = temp;
    }
    *(*cur).loc_tab.add((*cur).loc_nr as usize) = val;
    (*cur).loc_nr += 1;
}

/**
 * xmlXPtrWrapLocationSet:
 * @val:  the LocationSet value
 *
 * Wrap the LocationSet @val in a new xmlXPathObjectPtr
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_wrap_location_set(
    val: XmlLocationSetPtr,
) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xptr_err_memory(c"allocating locationset".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathLocationset;
    (*ret).user = val as _;
    ret
}

/**
 * xmlXPtrLocationSetDel:
 * @cur:  the initial range set
 * @val:  an xmlXPathObjectPtr
 *
 * Removes an xmlXPathObjectPtr from an existing LocationSet
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_location_set_del(
    cur: XmlLocationSetPtr,
    val: XmlXPathObjectPtr,
) {
    let mut i: c_int = (*cur).loc_nr;

    if cur.is_null() {
        return;
    }
    if val.is_null() {
        return;
    }

    /*
     * check against doublons
     */
    for j in 0..(*cur).loc_nr {
        if *(*cur).loc_tab.add(j as usize) == val {
            i = j;
            break;
        }
    }

    if i >= (*cur).loc_nr {
        // #ifdef DEBUG
        //         xmlGenericError(xmlGenericErrorContext,
        // 	        "xmlXPtrLocationSetDel: Range wasn't found in RangeList\n");
        // #endif
        return;
    }
    (*cur).loc_nr -= 1;
    for i in i..(*cur).loc_nr {
        *(*cur).loc_tab.add(i as usize) = *(*cur).loc_tab.add(i as usize + 1);
    }
    *(*cur).loc_tab.add((*cur).loc_nr as usize) = null_mut();
}

/**
 * xmlXPtrLocationSetRemove:
 * @cur:  the initial range set
 * @val:  the index to remove
 *
 * Removes an entry from an existing LocationSet list.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_location_set_remove(cur: XmlLocationSetPtr, val: c_int) {
    if cur.is_null() {
        return;
    }
    if val >= (*cur).loc_nr {
        return;
    }
    (*cur).loc_nr -= 1;
    for val in val..(*cur).loc_nr {
        *(*cur).loc_tab.add(val as usize) = *(*cur).loc_tab.add(val as usize + 1);
    }
    *(*cur).loc_tab.add((*cur).loc_nr as usize) = null_mut();
}

/**
 * xmlXPtrGetArity:
 * @cur:  the node
 *
 * Returns the number of child for an element, -1 in case of error
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_get_arity(mut cur: XmlNodePtr) -> c_int {
    let mut i: c_int;
    if cur.is_null() || (*cur).typ == XmlElementType::XmlNamespaceDecl {
        return -1;
    }
    cur = (*cur).children;
    i = 0;
    while !cur.is_null() {
        if matches!(
            (*cur).typ,
            XmlElementType::XmlElementNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlHtmlDocumentNode
        ) {
            i += 1;
        }
        cur = (*cur).next;
    }
    i
}

/**
 * xmlXPtrGetIndex:
 * @cur:  the node
 *
 * Returns the index of the node in its parent children list, -1
 *         in case of error
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_get_index(mut cur: XmlNodePtr) -> c_int {
    let mut i: c_int;
    if cur.is_null() || (*cur).typ == XmlElementType::XmlNamespaceDecl {
        return -1;
    }
    i = 1;
    while !cur.is_null() {
        if matches!(
            (*cur).typ,
            XmlElementType::XmlElementNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlHtmlDocumentNode
        ) {
            i += 1;
        }
        cur = (*cur).prev;
    }
    i
}

/**
 * xmlXPtrCoveringRange:
 * @ctxt:  the XPointer Parser context
 * @loc:  the location for which the covering range must be computed
 *
 * A covering range is a range that wholly encompasses a location
 * Section 5.3.3. Covering Ranges for All Location Types
 *        http://www.w3.org/TR/xptr#N2267
 *
 * Returns a new location or NULL in case of error
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_covering_range(
    ctxt: XmlXPathParserContextPtr,
    loc: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    if loc.is_null() {
        return null_mut();
    }
    if ctxt.is_null() || (*ctxt).context.is_null() || (*(*ctxt).context).doc.is_null() {
        return null_mut();
    }
    match (*loc).typ {
        XmlXPathObjectType::XpathPoint => {
            return xml_xptr_new_range(
                (*loc).user as _,
                (*loc).index,
                (*loc).user as _,
                (*loc).index,
            )
        }
        XmlXPathObjectType::XpathRange => {
            if !(*loc).user2.is_null() {
                return xml_xptr_new_range(
                    (*loc).user as _,
                    (*loc).index,
                    (*loc).user2 as _,
                    (*loc).index2,
                );
            } else {
                let mut node: XmlNodePtr = (*loc).user as XmlNodePtr;
                if node == (*(*ctxt).context).doc as XmlNodePtr {
                    return xml_xptr_new_range(node, 0, node, xml_xptr_get_arity(node));
                } else {
                    match (*node).typ {
                        XmlElementType::XmlAttributeNode => {
                            /* !!! our model is slightly different than XPath */
                            return xml_xptr_new_range(node, 0, node, xml_xptr_get_arity(node));
                        }
                        XmlElementType::XmlElementNode
                        | XmlElementType::XmlTextNode
                        | XmlElementType::XmlCdataSectionNode
                        | XmlElementType::XmlEntityRefNode
                        | XmlElementType::XmlPiNode
                        | XmlElementType::XmlCommentNode
                        | XmlElementType::XmlDocumentNode
                        | XmlElementType::XmlNotationNode
                        | XmlElementType::XmlHtmlDocumentNode => {
                            let indx: c_int = xml_xptr_get_index(node);

                            node = (*node).parent;
                            return xml_xptr_new_range(node, indx - 1, node, indx + 1);
                        }
                        _ => return null_mut(),
                    }
                }
            }
        }
        _ => {
            // TODO /* missed one case ??? */
        }
    }
    null_mut()
}

/**
 * xmlXPtrRangeFunction:
 * @ctxt:  the XPointer Parser context
 * @nargs:  the number of args
 *
 * Function implementing the range() function 5.4.3
 *  location-set range(location-set )
 *
 *  The range function returns ranges covering the locations in
 *  the argument location-set. For each location x in the argument
 *  location-set, a range location representing the covering range of
 *  x is added to the result location-set.
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_range_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    let mut set: XmlXPathObjectPtr;

    CHECK_ARITY!(ctxt, nargs, 1);
    if (*ctxt).value.is_null()
        || !matches!(
            (*(*ctxt).value).typ,
            XmlXPathObjectType::XpathLocationset | XmlXPathObjectType::XpathNodeset
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }

    set = value_pop(ctxt);
    if (*set).typ == XmlXPathObjectType::XpathNodeset {
        /*
         * First convert to a location set
         */
        let tmp: XmlXPathObjectPtr = xml_xptr_new_location_set_node_set((*set).nodesetval);
        xml_xpath_free_object(set);
        if tmp.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XpathMemoryError as i32);
        }
        set = tmp;
    }
    let oldset: XmlLocationSetPtr = (*set).user as XmlLocationSetPtr;

    /*
     * The loop is to compute the covering range for each item and add it
     */
    let newset: XmlLocationSetPtr = xml_xptr_location_set_create(null_mut());
    if newset.is_null() {
        xml_xpath_free_object(set);
        XP_ERROR!(ctxt, XmlXPathError::XpathMemoryError as i32);
    }
    if !oldset.is_null() {
        for i in 0..(*oldset).loc_nr {
            xml_xptr_location_set_add(
                newset,
                xml_xptr_covering_range(ctxt, *(*oldset).loc_tab.add(i as usize)),
            );
        }
    }

    /*
     * Save the new value and cleanup
     */
    value_push(ctxt, xml_xptr_wrap_location_set(newset));
    xml_xpath_free_object(set);
}

/**
 * xmlXPtrInsideRange:
 * @ctxt:  the XPointer Parser context
 * @loc:  the location for which the inside range must be computed
 *
 * A inside range is a range described in the range-inside() description
 *
 * Returns a new location or NULL in case of error
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_inside_range(
    ctxt: XmlXPathParserContextPtr,
    loc: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    if loc.is_null() {
        return null_mut();
    }
    if ctxt.is_null() || (*ctxt).context.is_null() || (*(*ctxt).context).doc.is_null() {
        return null_mut();
    }
    match (*loc).typ {
        XmlXPathObjectType::XpathPoint => {
            let node: XmlNodePtr = (*loc).user as XmlNodePtr;
            match (*node).typ {
                XmlElementType::XmlPiNode
                | XmlElementType::XmlCommentNode
                | XmlElementType::XmlTextNode
                | XmlElementType::XmlCdataSectionNode => {
                    if (*node).content.is_null() {
                        return xml_xptr_new_range(node, 0, node, 0);
                    } else {
                        return xml_xptr_new_range(node, 0, node, xml_strlen((*node).content));
                    }
                }
                XmlElementType::XmlAttributeNode
                | XmlElementType::XmlElementNode
                | XmlElementType::XmlEntityRefNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlNotationNode
                | XmlElementType::XmlHtmlDocumentNode => {
                    return xml_xptr_new_range(node, 0, node, xml_xptr_get_arity(node));
                }
                _ => {}
            }
            return null_mut();
        }
        XmlXPathObjectType::XpathRange => {
            let node: XmlNodePtr = (*loc).user as XmlNodePtr;
            if !(*loc).user2.is_null() {
                return xml_xptr_new_range(node, (*loc).index, (*loc).user2 as _, (*loc).index2);
            } else {
                match (*node).typ {
                    XmlElementType::XmlPiNode
                    | XmlElementType::XmlCommentNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlCdataSectionNode => {
                        if (*node).content.is_null() {
                            return xml_xptr_new_range(node, 0, node, 0);
                        } else {
                            return xml_xptr_new_range(node, 0, node, xml_strlen((*node).content));
                        }
                    }
                    XmlElementType::XmlAttributeNode
                    | XmlElementType::XmlElementNode
                    | XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlDocumentNode
                    | XmlElementType::XmlNotationNode
                    | XmlElementType::XmlHtmlDocumentNode => {
                        return xml_xptr_new_range(node, 0, node, xml_xptr_get_arity(node));
                    }
                    _ => {}
                }
                return null_mut();
            }
        }
        _ => {
            // TODO /* missed one case ??? */
        }
    }
    null_mut()
}

/**
 * xmlXPtrRangeInsideFunction:
 * @ctxt:  the XPointer Parser context
 * @nargs:  the number of args
 *
 * Function implementing the range-inside() function 5.4.3
 *  location-set range-inside(location-set )
 *
 *  The range-inside function returns ranges covering the contents of
 *  the locations in the argument location-set. For each location x in
 *  the argument location-set, a range location is added to the result
 *  location-set. If x is a range location, then x is added to the
 *  result location-set. If x is not a range location, then x is used
 *  as the container location of the start and end points of the range
 *  location to be added; the index of the start point of the range is
 *  zero; if the end point is a character point then its index is the
 *  length of the string-value of x, and otherwise is the number of
 *  location children of x.
 *
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_range_inside_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    let mut set: XmlXPathObjectPtr;

    CHECK_ARITY!(ctxt, nargs, 1);
    if (*ctxt).value.is_null()
        || !matches!(
            (*(*ctxt).value).typ,
            XmlXPathObjectType::XpathLocationset | XmlXPathObjectType::XpathNodeset
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }

    set = value_pop(ctxt);
    if (*set).typ == XmlXPathObjectType::XpathNodeset {
        /*
         * First convert to a location set
         */
        let tmp: XmlXPathObjectPtr = xml_xptr_new_location_set_node_set((*set).nodesetval);
        xml_xpath_free_object(set);
        if tmp.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XpathMemoryError as i32);
        }
        set = tmp;
    }

    /*
     * The loop is to compute the covering range for each item and add it
     */
    let newset: XmlLocationSetPtr = xml_xptr_location_set_create(null_mut());
    if newset.is_null() {
        xml_xpath_free_object(set);
        XP_ERROR!(ctxt, XmlXPathError::XpathMemoryError as i32);
    }
    let oldset: XmlLocationSetPtr = (*set).user as XmlLocationSetPtr;
    if !oldset.is_null() {
        for i in 0..(*oldset).loc_nr {
            xml_xptr_location_set_add(
                newset,
                xml_xptr_inside_range(ctxt, *(*oldset).loc_tab.add(i as usize)),
            );
        }
    }

    /*
     * Save the new value and cleanup
     */
    value_push(ctxt, xml_xptr_wrap_location_set(newset));
    xml_xpath_free_object(set);
}

/**
 * xmlXPtrGetStartPoint:
 * @obj:  an range
 * @node:  the resulting node
 * @indx:  the resulting index
 *
 * read the object and return the start point coordinates.
 *
 * Returns -1 in case of failure, 0 otherwise
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_get_start_point(
    obj: XmlXPathObjectPtr,
    node: *mut XmlNodePtr,
    indx: *mut c_int,
) -> c_int {
    if obj.is_null() || node.is_null() || indx.is_null() {
        return -1;
    }

    match (*obj).typ {
        XmlXPathObjectType::XpathPoint => {
            *node = (*obj).user as _;
            if (*obj).index <= 0 {
                *indx = 0;
            } else {
                *indx = (*obj).index;
            }
            return 0;
        }
        XmlXPathObjectType::XpathRange => {
            *node = (*obj).user as _;
            if (*obj).index <= 0 {
                *indx = 0;
            } else {
                *indx = (*obj).index;
            }
            return 0;
        }
        _ => {}
    }
    -1
}

/**
 * xmlXPtrGetEndPoint:
 * @obj:  an range
 * @node:  the resulting node
 * @indx:  the resulting indx
 *
 * read the object and return the end point coordinates.
 *
 * Returns -1 in case of failure, 0 otherwise
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_get_end_point(
    obj: XmlXPathObjectPtr,
    node: *mut XmlNodePtr,
    indx: *mut c_int,
) -> c_int {
    if obj.is_null() || node.is_null() || indx.is_null() {
        return -1;
    }

    match (*obj).typ {
        XmlXPathObjectType::XpathPoint => {
            *node = (*obj).user as _;
            if (*obj).index <= 0 {
                *indx = 0;
            } else {
                *indx = (*obj).index;
            }
            return 0;
        }
        XmlXPathObjectType::XpathRange => {
            *node = (*obj).user as _;
            if (*obj).index <= 0 {
                *indx = 0;
            } else {
                *indx = (*obj).index;
            }
            return 0;
        }
        _ => {}
    }
    -1
}

/**
 * xmlXPtrGetNthChild:
 * @cur:  the node
 * @no:  the child number
 *
 * Returns the @no'th element child of @cur or NULL
 */
unsafe extern "C" fn xml_xptr_get_nth_child(mut cur: XmlNodePtr, no: c_int) -> XmlNodePtr {
    let mut i: c_int;
    if cur.is_null() || (*cur).typ == XmlElementType::XmlNamespaceDecl {
        return cur;
    }
    cur = (*cur).children;
    i = 0;
    while i <= no {
        if cur.is_null() {
            return cur;
        }
        if matches!(
            (*cur).typ,
            XmlElementType::XmlElementNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlHtmlDocumentNode
        ) {
            i += 1;
            if i == no {
                break;
            }
        }

        cur = (*cur).next;
    }
    cur
}

/**
 * xmlXPtrAdvanceNode:
 * @cur:  the node
 * @level: incremented/decremented to show level in tree
 *
 * Advance to the next element or text node in document order
 * TODO: add a stack for entering/exiting entities
 *
 * Returns -1 in case of failure, 0 otherwise
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_advance_node(
    mut cur: XmlNodePtr,
    level: *mut c_int,
) -> XmlNodePtr {
    // next:
    'next: loop {
        if cur.is_null() || (*cur).typ == XmlElementType::XmlNamespaceDecl {
            return null_mut();
        }
        if !(*cur).children.is_null() {
            cur = (*cur).children;
            if !level.is_null() {
                *level += 1;
            }
            // goto found;
            // found:
            if !matches!(
                (*cur).typ,
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlDocumentNode
                    | XmlElementType::XmlHtmlDocumentNode
                    | XmlElementType::XmlCdataSectionNode
            ) {
                if (*cur).typ == XmlElementType::XmlEntityRefNode {
                    /* Shouldn't happen */
                    // TODO
                    // goto skip;
                } else {
                    // goto next;
                    continue 'next;
                }
            } else {
                return cur;
            }
        }
        // skip:		/* This label should only be needed if something is wrong! */
        'skip: loop {
            if !(*cur).next.is_null() {
                cur = (*cur).next;
                // goto found;
            } else {
                loop {
                    cur = (*cur).parent;
                    if !level.is_null() {
                        *level -= 1;
                    }
                    if cur.is_null() {
                        return null_mut();
                    }
                    if !(*cur).next.is_null() {
                        cur = (*cur).next;
                        // goto found;
                        break;
                    }

                    if cur.is_null() {
                        break;
                    }
                }
            }

            // found:
            if !matches!(
                (*cur).typ,
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlTextNode
                    | XmlElementType::XmlDocumentNode
                    | XmlElementType::XmlHtmlDocumentNode
                    | XmlElementType::XmlCdataSectionNode
            ) {
                if (*cur).typ == XmlElementType::XmlEntityRefNode {
                    /* Shouldn't happen */
                    // TODO
                    // goto skip;
                    continue 'skip;
                }
                // goto next;
                continue 'next;
            }

            break 'next cur;
        }
    }
}

/**
 * xmlXPtrAdvanceChar:
 * @node:  the node
 * @indx:  the indx
 * @bytes:  the number of bytes
 *
 * Advance a point of the associated number of bytes (not UTF8 chars)
 *
 * Returns -1 in case of failure, 0 otherwise
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_advance_char(
    node: *mut XmlNodePtr,
    indx: *mut c_int,
    mut bytes: c_int,
) -> c_int {
    let mut cur: XmlNodePtr;
    let mut pos: c_int;
    let mut len: c_int;

    if node.is_null() || indx.is_null() {
        return -1;
    }
    cur = *node;
    if cur.is_null() || (*cur).typ == XmlElementType::XmlNamespaceDecl {
        return -1;
    }
    pos = *indx;

    while bytes >= 0 {
        /*
         * First position to the beginning of the first text node
         * corresponding to this point
         */
        while !cur.is_null()
            && matches!(
                (*cur).typ,
                XmlElementType::XmlElementNode
                    | XmlElementType::XmlDocumentNode
                    | XmlElementType::XmlHtmlDocumentNode
            )
        {
            if pos > 0 {
                cur = xml_xptr_get_nth_child(cur, pos);
                pos = 0;
            } else {
                cur = xml_xptr_advance_node(cur, null_mut());
                pos = 0;
            }
        }

        if cur.is_null() {
            *node = null_mut();
            *indx = 0;
            return -1;
        }

        /*
         * if there is no move needed return the current value.
         */
        if pos == 0 {
            pos = 1;
        }
        if bytes == 0 {
            *node = cur;
            *indx = pos;
            return 0;
        }
        /*
         * We should have a text (or cdata) node ...
         */
        len = 0;
        if (*cur).typ != XmlElementType::XmlElementNode && !(*cur).content.is_null() {
            len = xml_strlen((*cur).content);
        }
        if pos > len {
            /* Strange, the indx in the text node is greater than it's len */
            STRANGE!();
            pos = len;
        }
        if pos + bytes >= len {
            bytes -= len - pos;
            cur = xml_xptr_advance_node(cur, null_mut());
            pos = 0;
        } else if pos + bytes < len {
            pos += bytes;
            *node = cur;
            *indx = pos;
            return 0;
        }
    }
    -1
}

/**
 * xmlXPtrGetLastChar:
 * @node:  the node
 * @index:  the index
 *
 * Computes the point coordinates of the last c_char of this point
 *
 * Returns -1 in case of failure, 0 otherwise
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_get_last_char(node: *mut XmlNodePtr, indx: *mut c_int) -> c_int {
    let mut cur: XmlNodePtr;
    let mut len: c_int = 0;

    if node.is_null()
        || (*node).is_null()
        || (*(*node)).typ == XmlElementType::XmlNamespaceDecl
        || indx.is_null()
    {
        return -1;
    }
    cur = *node;
    let pos: c_int = *indx;

    if matches!(
        (*cur).typ,
        XmlElementType::XmlElementNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlHtmlDocumentNode
    ) && pos > 0
    {
        cur = xml_xptr_get_nth_child(cur, pos);
    }
    while !cur.is_null() {
        if !(*cur).last.is_null() {
            cur = (*cur).last;
        } else if (*cur).typ != XmlElementType::XmlElementNode && !(*cur).content.is_null() {
            len = xml_strlen((*cur).content);
            break;
        } else {
            return -1;
        }
    }
    if cur.is_null() {
        return -1;
    }
    *node = cur;
    *indx = len;
    0
}

/**
 * xmlXPtrMatchString:
 * @string:  the string to search
 * @start:  the start textnode
 * @startindex:  the start index
 * @end:  the end textnode IN/OUT
 * @endindex:  the end index IN/OUT
 *
 * Check whether the document contains @string at the position
 * (@start, @startindex) and limited by the (@end, @endindex) point
 *
 * Returns -1 in case of failure, 0 if not found, 1 if found in which case
 *            (@start, @startindex) will indicate the position of the beginning
 *            of the range and (@end, @endindex) will indicate the end
 *            of the range
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_match_string(
    mut string: *const XmlChar,
    start: XmlNodePtr,
    startindex: c_int,
    end: *mut XmlNodePtr,
    endindex: *mut c_int,
) -> c_int {
    let mut cur: XmlNodePtr;
    let mut pos: c_int; /* 0 based */
    let mut len: c_int; /* in bytes */
    let mut stringlen: c_int; /* in bytes */
    let mut is_match: c_int;

    if string.is_null() {
        return -1;
    }
    if start.is_null() || (*start).typ == XmlElementType::XmlNamespaceDecl {
        return -1;
    }
    if end.is_null()
        || (*end).is_null()
        || (*(*end)).typ == XmlElementType::XmlNamespaceDecl
        || endindex.is_null()
    {
        return -1;
    }
    cur = start;
    pos = startindex - 1;
    stringlen = xml_strlen(string);

    while stringlen > 0 {
        if cur == *end && pos + stringlen > *endindex {
            return 0;
        }

        if (*cur).typ != XmlElementType::XmlElementNode && !(*cur).content.is_null() {
            len = xml_strlen((*cur).content);
            if len >= pos + stringlen {
                is_match =
                    (xml_strncmp((*cur).content.add(pos as usize), string, stringlen) == 0) as i32;
                if is_match != 0 {
                    // #ifdef DEBUG_RANGES
                    // 		    xmlGenericError(xmlGenericErrorContext,
                    // 			    "found range %d bytes at index %d of ->",
                    // 			    stringlen, pos + 1);
                    // 		    xmlDebugDumpString(stdout, (*cur).content);
                    // 		    xmlGenericError(xmlGenericErrorContext, "\n");
                    // #endif
                    *end = cur;
                    *endindex = pos + stringlen;
                    return 1;
                } else {
                    return 0;
                }
            } else {
                let sub: c_int = len - pos;
                is_match = (xml_strncmp((*cur).content.add(pos as usize), string, sub) == 0) as i32;
                if is_match != 0 {
                    // #ifdef DEBUG_RANGES
                    // 		    xmlGenericError(xmlGenericErrorContext,
                    // 			    "found subrange %d bytes at index %d of ->",
                    // 			    sub, pos + 1);
                    // 		    xmlDebugDumpString(stdout, (*cur).content);
                    // 		    xmlGenericError(xmlGenericErrorContext, "\n");
                    // #endif
                    string = string.add(sub as usize);
                    stringlen -= sub;
                } else {
                    return 0;
                }
            }
        }
        cur = xml_xptr_advance_node(cur, null_mut());
        if cur.is_null() {
            return 0;
        }
        pos = 0;
    }
    1
}

/**
 * xmlXPtrSearchString:
 * @string:  the string to search
 * @start:  the start textnode IN/OUT
 * @startindex:  the start index IN/OUT
 * @end:  the end textnode
 * @endindex:  the end index
 *
 * Search the next occurrence of @string within the document content
 * until the (@end, @endindex) point is reached
 *
 * Returns -1 in case of failure, 0 if not found, 1 if found in which case
 *            (@start, @startindex) will indicate the position of the beginning
 *            of the range and (@end, @endindex) will indicate the end
 *            of the range
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_search_string(
    string: *const XmlChar,
    start: *mut XmlNodePtr,
    startindex: *mut c_int,
    end: *mut XmlNodePtr,
    endindex: *mut c_int,
) -> c_int {
    let mut cur: XmlNodePtr;
    let mut str: *const XmlChar;
    let mut pos: c_int; /* 0 based */
    let mut len: c_int; /* in bytes */

    if string.is_null() {
        return -1;
    }
    if start.is_null()
        || (*start).is_null()
        || (*(*start)).typ == XmlElementType::XmlNamespaceDecl
        || startindex.is_null()
    {
        return -1;
    }
    if end.is_null() || endindex.is_null() {
        return -1;
    }
    cur = *start;
    pos = *startindex - 1;
    let first: XmlChar = *string.add(0);

    while !cur.is_null() {
        if (*cur).typ != XmlElementType::XmlElementNode && !(*cur).content.is_null() {
            len = xml_strlen((*cur).content);
            while pos <= len {
                if first != 0 {
                    str = xml_strchr((*cur).content.add(pos as usize), first);
                    if !str.is_null() {
                        pos = str.offset_from((*cur).content as *mut XmlChar) as _;
                        // #ifdef DEBUG_RANGES
                        // 			xmlGenericError(xmlGenericErrorContext,
                        // 				"found '%c' at index %d of ->",
                        // 				first, pos + 1);
                        // 			xmlDebugDumpString(stdout, (*cur).content);
                        // 			xmlGenericError(xmlGenericErrorContext, "\n");
                        // #endif
                        if xml_xptr_match_string(string, cur, pos + 1, end, endindex) != 0 {
                            *start = cur;
                            *startindex = pos + 1;
                            return 1;
                        }
                        pos += 1;
                    } else {
                        pos = len + 1;
                    }
                } else {
                    /*
                     * An empty string is considered to match before each
                     * character of the string-value and after the final
                     * character.
                     */
                    // #ifdef DEBUG_RANGES
                    // 		    xmlGenericError(xmlGenericErrorContext,
                    // 			    "found '' at index %d of ->",
                    // 			    pos + 1);
                    // 		    xmlDebugDumpString(stdout, (*cur).content);
                    // 		    xmlGenericError(xmlGenericErrorContext, "\n");
                    // #endif
                    *start = cur;
                    *startindex = pos + 1;
                    *end = cur;
                    *endindex = pos + 1;
                    return 1;
                }
            }
        }
        if cur == *end && pos >= *endindex {
            return 0;
        }
        cur = xml_xptr_advance_node(cur, null_mut());
        if cur.is_null() {
            return 0;
        }
        pos = 1;
    }
    0
}

/**
 * xmlXPtrStringRangeFunction:
 * @ctxt:  the XPointer Parser context
 * @nargs:  the number of args
 *
 * Function implementing the string-range() function
 * range as described in 5.4.2
 *
 * ------------------------------
 * [Definition: For each location in the location-set argument,
 * string-range returns a set of string ranges, a set of substrings in a
 * string. Specifically, the string-value of the location is searched for
 * substrings that match the string argument, and the resulting location-set
 * will contain a range location for each non-overlapping match.]
 * An empty string is considered to match before each character of the
 * string-value and after the final character. Whitespace in a string
 * is matched literally, with no normalization except that provided by
 * XML for line ends. The third argument gives the position of the first
 * character to be in the resulting range, relative to the start of the
 * match. The default value is 1, which makes the range start immediately
 * before the first character of the matched string. The fourth argument
 * gives the number of characters in the range; the default is that the
 * range extends to the end of the matched string.
 *
 * Element boundaries, as well as entire embedded nodes such as processing
 * instructions and comments, are ignored as defined in [XPath].
 *
 * If the string in the second argument is not found in the string-value
 * of the location, or if a value in the third or fourth argument indicates
 * a string that is beyond the beginning or end of the document, the
 * expression fails.
 *
 * The points of the range-locations in the returned location-set will
 * all be character points.
 * ------------------------------
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_string_range_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    let mut startindex: c_int = 0;
    let mut endindex: c_int = 0;
    let mut fendindex: c_int;
    let mut start: XmlNodePtr = null_mut();
    let mut end: XmlNodePtr = null_mut();
    let mut fend: XmlNodePtr;
    let mut set: XmlXPathObjectPtr = null_mut();
    let oldset: XmlLocationSetPtr;
    let mut newset: XmlLocationSetPtr = null_mut();
    let mut string: XmlXPathObjectPtr = null_mut();
    let mut position: XmlXPathObjectPtr = null_mut();
    let mut number: XmlXPathObjectPtr = null_mut();
    let mut found: c_int;
    let mut pos: c_int = 0;
    let mut num: c_int = 0;

    /*
     * Grab the arguments
     */
    if !(2..=4).contains(&nargs) {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidArity as i32);
    }

    'goto_error: {
        if nargs >= 4 {
            if (*ctxt).value.is_null() || (*(*ctxt).value).typ != XmlXPathObjectType::XpathNumber {
                xml_xpath_err(ctxt, XmlXPathError::XpathInvalidType as i32);
                // goto error;
                break 'goto_error;
            }
            number = value_pop(ctxt);
            if !number.is_null() {
                num = (*number).floatval as c_int;
            }
        }
        if nargs >= 3 {
            if (*ctxt).value.is_null() || (*(*ctxt).value).typ != XmlXPathObjectType::XpathNumber {
                xml_xpath_err(ctxt, XmlXPathError::XpathInvalidType as i32);
                // goto error;
                break 'goto_error;
            }
            position = value_pop(ctxt);
            if !position.is_null() {
                pos = (*position).floatval as c_int;
            }
        }
        if (*ctxt).value.is_null() || (*(*ctxt).value).typ != XmlXPathObjectType::XpathString {
            xml_xpath_err(ctxt, XmlXPathError::XpathInvalidType as i32);
            // goto error;
            break 'goto_error;
        }
        string = value_pop(ctxt);
        if (*ctxt).value.is_null()
            || !matches!(
                (*(*ctxt).value).typ,
                XmlXPathObjectType::XpathLocationset | XmlXPathObjectType::XpathNodeset
            )
        {
            xml_xpath_err(ctxt, XmlXPathError::XpathInvalidType as i32);
            // goto error;
            break 'goto_error;
        }
        set = value_pop(ctxt);
        newset = xml_xptr_location_set_create(null_mut());
        if newset.is_null() {
            xml_xpath_err(ctxt, XmlXPathError::XpathMemoryError as i32);
            // goto error;
            break 'goto_error;
        }

        if (*set).nodesetval.is_null() {
            // goto error;
            break 'goto_error;
        }
        if (*set).typ == XmlXPathObjectType::XpathNodeset {
            /*
             * First convert to a location set
             */
            let tmp: XmlXPathObjectPtr = xml_xptr_new_location_set_node_set((*set).nodesetval);
            xml_xpath_free_object(set);
            set = null_mut();
            if tmp.is_null() {
                xml_xpath_err(ctxt, XmlXPathError::XpathMemoryError as i32);
                // goto error;
                break 'goto_error;
            }
            set = tmp;
        }
        oldset = (*set).user as XmlLocationSetPtr;

        /*
         * The loop is to search for each element in the location set
         * the list of location set corresponding to that search
         */
        for i in 0..(*oldset).loc_nr {
            // #ifdef DEBUG_RANGES
            // 	xmlXPathDebugDumpObject(stdout, *(*oldset).locTab.add(i as usize), 0);
            // #endif

            xml_xptr_get_start_point(
                *(*oldset).loc_tab.add(i as usize),
                addr_of_mut!(start),
                addr_of_mut!(startindex),
            );
            xml_xptr_get_end_point(
                *(*oldset).loc_tab.add(i as usize),
                addr_of_mut!(end),
                addr_of_mut!(endindex),
            );
            xml_xptr_advance_char(addr_of_mut!(start), addr_of_mut!(startindex), 0);
            xml_xptr_get_last_char(addr_of_mut!(end), addr_of_mut!(endindex));

            // #ifdef DEBUG_RANGES
            // 	xmlGenericError(xmlGenericErrorContext,
            // 		"from index %d of ->", startindex);
            // 	xmlDebugDumpString(stdout, (*start).content);
            // 	xmlGenericError(xmlGenericErrorContext, "\n");
            // 	xmlGenericError(xmlGenericErrorContext,
            // 		"to index %d of ->", endindex);
            // 	xmlDebugDumpString(stdout, (*end).content);
            // 	xmlGenericError(xmlGenericErrorContext, "\n");
            // #endif
            while {
                fend = end;
                fendindex = endindex;
                found = xml_xptr_search_string(
                    (*string).stringval,
                    addr_of_mut!(start),
                    addr_of_mut!(startindex),
                    addr_of_mut!(fend),
                    addr_of_mut!(fendindex),
                );
                if found == 1 {
                    if position.is_null() {
                        xml_xptr_location_set_add(
                            newset,
                            xml_xptr_new_range(start, startindex, fend, fendindex),
                        );
                    } else if xml_xptr_advance_char(
                        addr_of_mut!(start),
                        addr_of_mut!(startindex),
                        pos - 1,
                    ) == 0
                    {
                        if !number.is_null() && num > 0 {
                            let mut rindx: c_int;
                            let mut rend: XmlNodePtr;
                            rend = start;
                            rindx = startindex - 1;
                            if xml_xptr_advance_char(addr_of_mut!(rend), addr_of_mut!(rindx), num)
                                == 0
                            {
                                xml_xptr_location_set_add(
                                    newset,
                                    xml_xptr_new_range(start, startindex, rend, rindx),
                                );
                            }
                        } else if !number.is_null() && num <= 0 {
                            xml_xptr_location_set_add(
                                newset,
                                xml_xptr_new_range(start, startindex, start, startindex),
                            );
                        } else {
                            xml_xptr_location_set_add(
                                newset,
                                xml_xptr_new_range(start, startindex, fend, fendindex),
                            );
                        }
                    }
                    start = fend;
                    startindex = fendindex;
                    if *(*string).stringval.add(0) == 0 {
                        startindex += 1;
                    }
                }

                found == 1
            } {}
        }
    }

    /*
     * Save the new value and cleanup
     */
    // error:
    if !newset.is_null() {
        value_push(ctxt, xml_xptr_wrap_location_set(newset));
    }
    xml_xpath_free_object(set);
    xml_xpath_free_object(string);
    if !position.is_null() {
        xml_xpath_free_object(position);
    }
    if !number.is_null() {
        xml_xpath_free_object(number);
    }
}

/**
 * xmlXPtrNewPoint:
 * @node:  the xmlNodePtr
 * @indx:  the indx within the node
 *
 * Create a new xmlXPathObjectPtr of type point
 *
 * Returns the newly created object.
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_new_point(node: XmlNodePtr, indx: c_int) -> XmlXPathObjectPtr {
    if node.is_null() {
        return null_mut();
    }
    if indx < 0 {
        return null_mut();
    }

    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xptr_err_memory(c"allocating point".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathPoint;
    (*ret).user = node as *mut c_void;
    (*ret).index = indx;
    ret
}

/**
 * xmlXPtrStartPointFunction:
 * @ctxt:  the XPointer Parser context
 * @nargs:  the number of args
 *
 * Function implementing start-point() operation
 * as described in 5.4.3
 * ----------------
 * location-set start-point(location-set)
 *
 * For each location x in the argument location-set, start-point adds a
 * location of type point to the result location-set. That point represents
 * the start point of location x and is determined by the following rules:
 *
 * - If x is of type point, the start point is x.
 * - If x is of type range, the start point is the start point of x.
 * - If x is of type root, element, text, comment, or processing instruction,
 * - the container node of the start point is x and the index is 0.
 * - If x is of type attribute or namespace, the function must signal a
 *   syntax error.
 * ----------------
 *
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_start_point_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    let mut tmp: XmlXPathObjectPtr;
    let mut obj: XmlXPathObjectPtr;
    let mut point: XmlXPathObjectPtr;

    CHECK_ARITY!(ctxt, nargs, 1);
    if (*ctxt).value.is_null()
        || !matches!(
            (*(*ctxt).value).typ,
            XmlXPathObjectType::XpathLocationset | XmlXPathObjectType::XpathNodeset
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }

    obj = value_pop(ctxt);
    if (*obj).typ == XmlXPathObjectType::XpathNodeset {
        /*
         * First convert to a location set
         */
        tmp = xml_xptr_new_location_set_node_set((*obj).nodesetval);
        xml_xpath_free_object(obj);
        if tmp.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XpathMemoryError as i32);
        }
        obj = tmp;
    }

    let newset: XmlLocationSetPtr = xml_xptr_location_set_create(null_mut());
    if newset.is_null() {
        xml_xpath_free_object(obj);
        XP_ERROR!(ctxt, XmlXPathError::XpathMemoryError as i32);
    }
    let oldset: XmlLocationSetPtr = (*obj).user as XmlLocationSetPtr;
    if !oldset.is_null() {
        for i in 0..(*oldset).loc_nr {
            tmp = *(*oldset).loc_tab.add(i as usize);
            if tmp.is_null() {
                continue;
            }
            point = null_mut();
            match (*tmp).typ {
                XmlXPathObjectType::XpathPoint => {
                    point = xml_xptr_new_point((*tmp).user as _, (*tmp).index)
                }
                XmlXPathObjectType::XpathRange => {
                    let node: XmlNodePtr = (*tmp).user as _;
                    if !node.is_null() {
                        if matches!(
                            (*node).typ,
                            XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
                        ) {
                            xml_xpath_free_object(obj);
                            xml_xptr_free_location_set(newset);
                            XP_ERROR!(ctxt, XmlXPathError::XptrSyntaxError as i32);
                        }
                        point = xml_xptr_new_point(node, (*tmp).index);
                    }
                }
                _ => {
                    /*** Should we raise an error ?
                    xmlXPathFreeObject(obj);
                    xmlXPathFreeObject(newset);
                    XP_ERROR!(ctxt, xmlXPathError::XPATH_INVALID_TYPE as i32);
                    ***/
                }
            }
            if !point.is_null() {
                xml_xptr_location_set_add(newset, point);
            }
        }
    }
    xml_xpath_free_object(obj);
    value_push(ctxt, xml_xptr_wrap_location_set(newset));
}

/**
 * xmlXPtrNbLocChildren:
 * @node:  an xmlNodePtr
 *
 * Count the number of location children of @node or the length of the
 * string value in case of text/PI/Comments nodes
 *
 * Returns the number of location children
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_nb_loc_children(mut node: XmlNodePtr) -> c_int {
    let mut ret: c_int = 0;
    if node.is_null() {
        return -1;
    }
    match (*node).typ {
        XmlElementType::XmlHtmlDocumentNode
        | XmlElementType::XmlDocumentNode
        | XmlElementType::XmlElementNode => {
            node = (*node).children;
            while !node.is_null() {
                if (*node).typ == XmlElementType::XmlElementNode {
                    ret += 1;
                }
                node = (*node).next;
            }
        }
        XmlElementType::XmlAttributeNode => return -1,
        XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlEntityRefNode => ret = xml_strlen((*node).content),
        _ => return -1,
    }
    ret
}

/**
 * xmlXPtrEndPointFunction:
 * @ctxt:  the XPointer Parser context
 * @nargs:  the number of args
 *
 * Function implementing end-point() operation
 * as described in 5.4.3
 * ----------------------------
 * location-set end-point(location-set)
 *
 * For each location x in the argument location-set, end-point adds a
 * location of type point to the result location-set. That point represents
 * the end point of location x and is determined by the following rules:
 *
 * - If x is of type point, the resulting point is x.
 * - If x is of type range, the resulting point is the end point of x.
 * - If x is of type root or element, the container node of the resulting
 *   point is x and the index is the number of location children of x.
 * - If x is of type text, comment, or processing instruction, the container
 *   node of the resulting point is x and the index is the length of the
 *   string-value of x.
 * - If x is of type attribute or namespace, the function must signal a
 *   syntax error.
 * ----------------------------
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_end_point_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    let mut tmp: XmlXPathObjectPtr;
    let mut obj: XmlXPathObjectPtr;
    let mut point: XmlXPathObjectPtr;

    CHECK_ARITY!(ctxt, nargs, 1);
    if (*ctxt).value.is_null()
        || !matches!(
            (*(*ctxt).value).typ,
            XmlXPathObjectType::XpathLocationset | XmlXPathObjectType::XpathNodeset
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }

    obj = value_pop(ctxt);
    if (*obj).typ == XmlXPathObjectType::XpathNodeset {
        /*
         * First convert to a location set
         */
        tmp = xml_xptr_new_location_set_node_set((*obj).nodesetval);
        xml_xpath_free_object(obj);
        if tmp.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XpathMemoryError as i32);
        }
        obj = tmp;
    }

    let newset: XmlLocationSetPtr = xml_xptr_location_set_create(null_mut());
    if newset.is_null() {
        xml_xpath_free_object(obj);
        XP_ERROR!(ctxt, XmlXPathError::XpathMemoryError as i32);
    }
    let oldset: XmlLocationSetPtr = (*obj).user as XmlLocationSetPtr;
    if !oldset.is_null() {
        for i in 0..(*oldset).loc_nr {
            tmp = *(*oldset).loc_tab.add(i as usize);
            if tmp.is_null() {
                continue;
            }
            point = null_mut();
            match (*tmp).typ {
                XmlXPathObjectType::XpathPoint => {
                    point = xml_xptr_new_point((*tmp).user as _, (*tmp).index)
                }
                XmlXPathObjectType::XpathRange => {
                    let node: XmlNodePtr = (*tmp).user2 as _;
                    if !node.is_null() {
                        if matches!(
                            (*node).typ,
                            XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
                        ) {
                            xml_xpath_free_object(obj);
                            xml_xptr_free_location_set(newset);
                            XP_ERROR!(ctxt, XmlXPathError::XptrSyntaxError as i32);
                        }
                        point = xml_xptr_new_point(node, (*tmp).index2);
                    } else if (*tmp).user.is_null() {
                        point = xml_xptr_new_point(node, xml_xptr_nb_loc_children(node));
                    }
                }
                _ => {
                    /*** Should we raise an error ?
                    xmlXPathFreeObject(obj);
                    xmlXPathFreeObject(newset);
                    XP_ERROR!(ctxt, xmlXPathError::XPATH_INVALID_TYPE as i32);
                    ***/
                }
            }
            if !point.is_null() {
                xml_xptr_location_set_add(newset, point);
            }
        }
    }
    xml_xpath_free_object(obj);
    value_push(ctxt, xml_xptr_wrap_location_set(newset));
}

/**
 * xmlXPtrHereFunction:
 * @ctxt:  the XPointer Parser context
 * @nargs:  the number of args
 *
 * Function implementing here() operation
 * as described in 5.4.3
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_here_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    CHECK_ARITY!(ctxt, nargs, 0);

    if (*(*ctxt).context).here.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XptrSyntaxError as i32);
    }

    value_push(
        ctxt,
        xml_xptr_new_location_set_nodes((*(*ctxt).context).here, null_mut()),
    );
}

/**
 * xmlXPtrOriginFunction:
 * @ctxt:  the XPointer Parser context
 * @nargs:  the number of args
 *
 * Function implementing origin() operation
 * as described in 5.4.3
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_origin_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    CHECK_ARITY!(ctxt, nargs, 0);

    if (*(*ctxt).context).origin.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XptrSyntaxError as i32);
    }

    value_push(
        ctxt,
        xml_xptr_new_location_set_nodes((*(*ctxt).context).origin, null_mut()),
    );
}

/// Create a new XPointer context.  
/// Please refer to the document of `xmlXPtrNewContext` in original libxml2.
///
/// # Safety
/// - A valid pointer generated by the API for this crate must be given.
/// - If the context generation fails, this method may return null.
pub unsafe extern "C" fn xml_xptr_new_context(
    doc: XmlDocPtr,
    _here: XmlNodePtr,
    _origin: XmlNodePtr,
) -> XmlXPathContextPtr {
    let ret: XmlXPathContextPtr = xml_xpath_new_context(doc);
    if ret.is_null() {
        return ret;
    }
    #[cfg(feature = "libxml_xptr_locs")]
    {
        (*ret).xptr = 1;
        (*ret).here = _here;
        (*ret).origin = _origin;

        xml_xpath_register_func(ret, c"range".as_ptr() as _, xml_xptr_range_function);
        xml_xpath_register_func(
            ret,
            c"range-inside".as_ptr() as _,
            xml_xptr_range_inside_function,
        );
        xml_xpath_register_func(
            ret,
            c"string-range".as_ptr() as _,
            xml_xptr_string_range_function,
        );
        xml_xpath_register_func(
            ret,
            c"start-point".as_ptr() as _,
            xml_xptr_start_point_function,
        );
        xml_xpath_register_func(ret, c"end-point".as_ptr() as _, xml_xptr_end_point_function);
        xml_xpath_register_func(ret, c"here".as_ptr() as _, xml_xptr_here_function);
        xml_xpath_register_func(ret, c" origin".as_ptr() as _, xml_xptr_origin_function);
    }

    ret
}

/*
 * Macros for accessing the content. Those should be used only by the parser,
 * and not exported.
 *
 * Dirty macros, i.e. one need to make assumption on the context to use them
 *
 *   CUR     returns the current xmlChar value, i.e. a 8 bit value
 *           in ISO-Latin or UTF-8.
 *           This should be used internally by the parser
 *           only to compare to ASCII values otherwise it would break when
 *           running with UTF-8 encoding.
 *   NXT(n)  returns the n'th next xmlChar. Same as CUR is should be used only
 *           to compare on ASCII based substring.
 *   SKIP(n) Skip n xmlChar, and must also be used only to skip ASCII defined
 *           strings within the parser.
 *   CURRENT Returns the current c_char value, with the full decoding of
 *           UTF-8 if we are using this mode. It returns an c_int.
 *   NEXT    Skip to the next character, this does the proper decoding
 *           in UTF-8 mode. It also pop-up unfinished entities on the fly.
 *           It returns the pointer to the current xmlChar.
 */
macro_rules! CUR {
    ($ctxt:expr) => {
        *(*$ctxt).cur
    };
}
macro_rules! NXT {
    ($ctxt:expr, $val:expr) => {
        *(*$ctxt).cur.add($val as usize)
    };
}

macro_rules! SKIP_BLANKS {
    ($ctxt:expr) => {
        while $crate::IS_BLANK_CH!(*(*$ctxt).cur) {
            NEXT!($ctxt);
        }
    };
}

macro_rules! NEXT {
    ($ctxt:expr) => {
        if *(*$ctxt).cur != 0 {
            let res = (*$ctxt).cur;
            (*$ctxt).cur = (*$ctxt).cur.add(1);
            res
        } else {
            (*$ctxt).cur
        }
    };
}

/**
 * xmlXPtrErr:
 * @ctxt:  an XPTR evaluation context
 * @extra:  extra information
 *
 * Handle a redefinition of attribute error
 */
unsafe extern "C" fn xml_xptr_err(
    ctxt: XmlXPathParserContextPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    extra: *const XmlChar,
) {
    if !ctxt.is_null() {
        (*ctxt).error = error as i32;
    }
    if ctxt.is_null() || (*ctxt).context.is_null() {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromXPointer,
            error,
            XmlErrorLevel::XmlErrError,
            null_mut(),
            0,
            extra as _,
            null_mut(),
            None,
            0,
            0,
            msg,
            extra
        );
        return;
    }

    /* cleanup current last error */
    (*(*ctxt).context).last_error.reset();

    (*(*ctxt).context).last_error.domain = XmlErrorDomain::XmlFromXPointer;
    (*(*ctxt).context).last_error.code = error;
    (*(*ctxt).context).last_error.level = XmlErrorLevel::XmlErrError;
    (*(*ctxt).context).last_error.str1 = (!(*ctxt).base.is_null()).then(|| {
        CStr::from_ptr((*ctxt).base as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()
    });
    // (*(*ctxt).context).last_error.str1 = xml_strdup((*ctxt).base) as _;
    (*(*ctxt).context).last_error.int1 = (*ctxt).cur.offset_from((*ctxt).base) as _;
    (*(*ctxt).context).last_error.node = NonNull::new((*(*ctxt).context).debug_node as _);
    if let Some(error) = (*(*ctxt).context).error {
        error(
            (*(*ctxt).context).user_data.clone(),
            &(*(*ctxt).context).last_error,
        );
    } else {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            (*(*ctxt).context).debug_node as _,
            XmlErrorDomain::XmlFromXPointer,
            error,
            XmlErrorLevel::XmlErrError,
            null_mut(),
            0,
            extra as _,
            (*ctxt).base as _,
            None,
            (*ctxt).cur.offset_from((*ctxt).base) as _,
            0,
            msg,
            extra
        );
    }
}

/*
 * xmlXPtrGetChildNo:
 * @ctxt:  the XPointer Parser context
 * @index:  the child number
 *
 * Move the current node of the nodeset on the stack to the
 * given child if found
 */
unsafe extern "C" fn xml_xptr_get_child_no(ctxt: XmlXPathParserContextPtr, indx: c_int) {
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathNodeset);
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    let oldset: XmlNodeSetPtr = (*obj).nodesetval;
    if indx <= 0 || oldset.is_null() || (*oldset).node_nr != 1 {
        xml_xpath_free_object(obj);
        value_push(ctxt, xml_xpath_new_node_set(null_mut()));
        return;
    }
    let cur: XmlNodePtr = xml_xptr_get_nth_child(*(*oldset).node_tab.add(0), indx);
    if cur.is_null() {
        xml_xpath_free_object(obj);
        value_push(ctxt, xml_xpath_new_node_set(null_mut()));
        return;
    }
    *(*oldset).node_tab.add(0) = cur;
    value_push(ctxt, obj);
}

/**
 * xmlXPtrEvalChildSeq:
 * @ctxt:  the XPointer Parser context
 * @name:  a possible ID name of the child sequence
 *
 *  ChildSeq ::= '/1' ('/' [0-9]*)*
 *             | Name ('/' [0-9]*)+
 *
 * Parse and evaluate a Child Sequence. This routine also handle the
 * case of a Bare Name used to get a document ID.
 */
unsafe extern "C" fn xml_xptr_eval_child_seq(ctxt: XmlXPathParserContextPtr, name: *mut XmlChar) {
    /*
     * XPointer don't allow by syntax to address in multirooted trees
     * this might prove useful in some cases, warn about it.
     */
    if name.is_null() && CUR!(ctxt) == b'/' && NXT!(ctxt, 1) != b'1' {
        xml_xptr_err(
            ctxt,
            XmlParserErrors::XmlXptrChildseqStart,
            c"warning: ChildSeq not starting by /1\n".as_ptr() as _,
            null_mut(),
        );
    }

    if !name.is_null() {
        value_push(ctxt, xml_xpath_new_string(name));
        xml_free(name as _);
        xml_xpath_id_function(ctxt, 1);
        CHECK_ERROR!(ctxt);
    }

    while CUR!(ctxt) == b'/' {
        let mut child: c_int = 0;
        let mut overflow: c_int = 0;
        NEXT!(ctxt);

        while CUR!(ctxt) >= b'0' && CUR!(ctxt) <= b'9' {
            let d: c_int = (CUR!(ctxt) - b'0') as i32;
            if child > i32::MAX / 10 {
                overflow = 1;
            } else {
                child *= 10;
            }
            if child > i32::MAX - d {
                overflow = 1;
            } else {
                child += d;
            }
            NEXT!(ctxt);
        }
        if overflow != 0 {
            child = 0;
        }
        xml_xptr_get_child_no(ctxt, child);
    }
}

/**
 * xmlXPtrEvalXPtrPart:
 * @ctxt:  the XPointer Parser context
 * @name:  the preparsed Scheme for the XPtrPart
 *
 * XPtrPart ::= 'xpointer' '(' XPtrExpr ')'
 *            | Scheme '(' SchemeSpecificExpr ')'
 *
 * Scheme   ::=  NCName - 'xpointer' [VC: Non-XPointer schemes]
 *
 * SchemeSpecificExpr ::= StringWithBalancedParens
 *
 * StringWithBalancedParens ::=
 *              [^()]* ('(' StringWithBalancedParens ')' [^()]*)*
 *              [VC: Parenthesis escaping]
 *
 * XPtrExpr ::= Expr [VC: Parenthesis escaping]
 *
 * VC: Parenthesis escaping:
 *   The end of an XPointer part is signaled by the right parenthesis ")"
 *   character that is balanced with the left parenthesis "(" character
 *   that began the part. Any unbalanced parenthesis character inside the
 *   expression, even within literals, must be escaped with a circumflex (^)
 *   character preceding it. If the expression contains any literal
 *   occurrences of the circumflex, each must be escaped with an additional
 *   circumflex (that is, ^^). If the unescaped parentheses in the expression
 *   are not balanced, a syntax error results.
 *
 * Parse and evaluate an XPtrPart. Basically it generates the unescaped
 * string and if the scheme is 'xpointer' it will call the XPath interpreter.
 *
 * TODO: there is no new scheme registration mechanism
 */
unsafe extern "C" fn xml_xptr_eval_xptr_part(
    ctxt: XmlXPathParserContextPtr,
    mut name: *mut XmlChar,
) {
    let mut cur: *mut XmlChar;
    let mut len: c_int;
    let mut level: c_int;

    if name.is_null() {
        name = xml_xpath_parse_name(ctxt);
    }
    if name.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
    }

    if CUR!(ctxt) != b'(' {
        xml_free(name as _);
        XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
    }
    NEXT!(ctxt);
    level = 1;

    len = xml_strlen((*ctxt).cur);
    len += 1;
    let buffer: *mut XmlChar = xml_malloc_atomic(len as usize) as _;
    if buffer.is_null() {
        xml_xptr_err_memory(c"allocating buffer".as_ptr() as _);
        xml_free(name as _);
        return;
    }

    cur = buffer;
    while CUR!(ctxt) != 0 {
        if CUR!(ctxt) == b')' {
            level -= 1;
            if level == 0 {
                NEXT!(ctxt);
                break;
            }
        } else if CUR!(ctxt) == b'(' {
            level += 1;
        } else if CUR!(ctxt) == b'^'
            && (NXT!(ctxt, 1) == b')' || NXT!(ctxt, 1) == b'(' || NXT!(ctxt, 1) == b'^')
        {
            NEXT!(ctxt);
        }
        *cur = CUR!(ctxt);
        cur = cur.add(1);
        NEXT!(ctxt);
    }
    *cur = 0;

    if level != 0 && CUR!(ctxt) == 0 {
        xml_free(name as _);
        xml_free(buffer as _);
        XP_ERROR!(ctxt, XmlXPathError::XptrSyntaxError as i32);
    }

    if xml_str_equal(name, c"xpointer".as_ptr() as _)
        || xml_str_equal(name, c"xpath1".as_ptr() as _)
    {
        let old_base: *const XmlChar = (*ctxt).base;
        let old_cur: *const XmlChar = (*ctxt).cur;

        (*ctxt).cur = buffer;
        (*ctxt).base = buffer;
        /*
         * To evaluate an xpointer scheme element (4.3) we need:
         *   context initialized to the root
         *   context position initialized to 1
         *   context size initialized to 1
         */
        (*(*ctxt).context).node = (*(*ctxt).context).doc as XmlNodePtr;
        (*(*ctxt).context).proximity_position = 1;
        (*(*ctxt).context).context_size = 1;
        #[cfg(feature = "libxml_xptr_locs")]
        {
            (*ctxt).xptr = xml_str_equal(name, c"xpointer".as_ptr() as _);
        }
        xml_xpath_eval_expr(ctxt);
        (*ctxt).base = old_base;
        (*ctxt).cur = old_cur;
    } else if xml_str_equal(name, c"element".as_ptr() as _) {
        let old_base: *const XmlChar = (*ctxt).base;
        let old_cur: *const XmlChar = (*ctxt).cur;
        let name2: *mut XmlChar;

        (*ctxt).cur = buffer;
        (*ctxt).base = buffer;
        if *buffer.add(0) == b'/' {
            xml_xpath_root(ctxt);
            xml_xptr_eval_child_seq(ctxt, null_mut());
        } else {
            name2 = xml_xpath_parse_name(ctxt);
            if name2.is_null() {
                (*ctxt).base = old_base;
                (*ctxt).cur = old_cur;
                xml_free(buffer as _);
                xml_free(name as _);
                XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
            }
            xml_xptr_eval_child_seq(ctxt, name2);
        }
        (*ctxt).base = old_base;
        (*ctxt).cur = old_cur;
    } else if xml_str_equal(name, c"xmlns".as_ptr() as _) {
        let old_base: *const XmlChar = (*ctxt).base;
        let old_cur: *const XmlChar = (*ctxt).cur;

        (*ctxt).cur = buffer;
        (*ctxt).base = buffer;
        let prefix: *mut XmlChar = xml_xpath_parse_ncname(ctxt);
        if prefix.is_null() {
            (*ctxt).base = old_base;
            (*ctxt).cur = old_cur;
            xml_free(buffer as _);
            xml_free(name as _);
            XP_ERROR!(ctxt, XmlXPathError::XptrSyntaxError as i32);
        }
        SKIP_BLANKS!(ctxt);
        if CUR!(ctxt) != b'=' {
            (*ctxt).base = old_base;
            (*ctxt).cur = old_cur;
            xml_free(prefix as _);
            xml_free(buffer as _);
            xml_free(name as _);
            XP_ERROR!(ctxt, XmlXPathError::XptrSyntaxError as i32);
        }
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);

        xml_xpath_register_ns((*ctxt).context, prefix, (*ctxt).cur);
        (*ctxt).base = old_base;
        (*ctxt).cur = old_cur;
        xml_free(prefix as _);
    } else {
        xml_xptr_err(
            ctxt,
            XmlParserErrors::XmlXptrUnknownScheme,
            c"unsupported scheme '%s'\n".as_ptr() as _,
            name,
        );
    }
    xml_free(buffer as _);
    xml_free(name as _);
}

/**
 * xmlXPtrEvalFullXPtr:
 * @ctxt:  the XPointer Parser context
 * @name:  the preparsed Scheme for the first XPtrPart
 *
 * FullXPtr ::= XPtrPart (S? XPtrPart)*
 *
 * As the specs says:
 * -----------
 * When multiple XPtrParts are provided, they must be evaluated in
 * left-to-right order. If evaluation of one part fails, the nexti
 * is evaluated. The following conditions cause XPointer part failure:
 *
 * - An unknown scheme
 * - A scheme that does not locate any sub-resource present in the resource
 * - A scheme that is not applicable to the media type of the resource
 *
 * The XPointer application must consume a failed XPointer part and
 * attempt to evaluate the next one, if any. The result of the first
 * XPointer part whose evaluation succeeds is taken to be the fragment
 * located by the XPointer as a whole. If all the parts fail, the result
 * for the XPointer as a whole is a sub-resource error.
 * -----------
 *
 * Parse and evaluate a Full XPtr i.e. possibly a cascade of XPath based
 * expressions or other schemes.
 */
unsafe extern "C" fn xml_xptr_eval_full_xptr(
    ctxt: XmlXPathParserContextPtr,
    mut name: *mut XmlChar,
) {
    if name.is_null() {
        name = xml_xpath_parse_name(ctxt);
    }
    if name.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
    }
    while !name.is_null() {
        (*ctxt).error = XmlXPathError::XpathExpressionOk as i32;
        xml_xptr_eval_xptr_part(ctxt, name);

        /* in case of syntax error, break here */
        if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32
            && (*ctxt).error != XmlParserErrors::XmlXptrUnknownScheme as i32
        {
            return;
        }

        /*
         * If the returned value is a non-empty nodeset
         * or location set, return here.
         */
        if !(*ctxt).value.is_null() {
            let mut obj: XmlXPathObjectPtr = (*ctxt).value;

            match (*obj).typ {
                #[cfg(feature = "libxml_xptr_locs")]
                XmlXPathObjectType::XpathLocationset => {
                    let loc: XmlLocationSetPtr = (*(*ctxt).value).user as _;
                    if !loc.is_null() && (*loc).loc_nr > 0 {
                        return;
                    }
                }
                XmlXPathObjectType::XpathNodeset => {
                    let loc: XmlNodeSetPtr = (*(*ctxt).value).nodesetval;
                    if !loc.is_null() && (*loc).node_nr > 0 {
                        return;
                    }
                }
                _ => {}
            }

            /*
             * Evaluating to improper values is equivalent to
             * a sub-resource error, clean-up the stack
             */
            while {
                obj = value_pop(ctxt);
                if !obj.is_null() {
                    xml_xpath_free_object(obj);
                }

                !obj.is_null()
            } {}
        }

        /*
         * Is there another XPointer part.
         */
        SKIP_BLANKS!(ctxt);
        name = xml_xpath_parse_name(ctxt);
    }
}

/**
 * xmlXPtrEvalXPointer:
 * @ctxt:  the XPointer Parser context
 *
 *  XPointer ::= Name
 *             | ChildSeq
 *             | FullXPtr
 *
 * Parse and evaluate an XPointer
 */
unsafe extern "C" fn xml_xptr_eval_xpointer(ctxt: XmlXPathParserContextPtr) {
    if (*ctxt).value_tab.is_null() {
        /* Allocate the value stack */
        (*ctxt).value_tab =
            xml_malloc(10 * size_of::<XmlXPathObjectPtr>()) as *mut XmlXPathObjectPtr;
        if (*ctxt).value_tab.is_null() {
            xml_xptr_err_memory(c"allocating evaluation context".as_ptr() as _);
            return;
        }
        (*ctxt).value_nr = 0;
        (*ctxt).value_max = 10;
        (*ctxt).value = null_mut();
        (*ctxt).value_frame = 0;
    }
    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) == b'/' {
        xml_xpath_root(ctxt);
        xml_xptr_eval_child_seq(ctxt, null_mut());
    } else {
        let name: *mut XmlChar = xml_xpath_parse_name(ctxt);
        if name.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
        }
        if CUR!(ctxt) == b'(' {
            xml_xptr_eval_full_xptr(ctxt, name);
            /* Short evaluation */
            return;
        } else {
            /* this handle both Bare Names and Child Sequences */
            xml_xptr_eval_child_seq(ctxt, name);
        }
    }
    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) != 0 {
        XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
    }
}

/// Evaluate the XPath Location Path in the given context.  
/// Please refer to the document of `xmlXPtrEval` in original libxml2.
///
/// # Safety
/// - A valid pointer generated by the API for this crate must be given.
/// - If the evaluation fails or arguments is invalid, this method may return null.
pub unsafe extern "C" fn xml_xptr_eval(
    str: *const XmlChar,
    ctx: XmlXPathContextPtr,
) -> XmlXPathObjectPtr {
    let mut res: XmlXPathObjectPtr = null_mut();
    let mut tmp: XmlXPathObjectPtr;
    let init: XmlXPathObjectPtr = null_mut();
    let mut stack: c_int = 0;

    xml_init_parser();

    if ctx.is_null() || str.is_null() {
        return null_mut();
    }

    let ctxt: XmlXPathParserContextPtr = xml_xpath_new_parser_context(str, ctx);
    if ctxt.is_null() {
        return null_mut();
    }
    xml_xptr_eval_xpointer(ctxt);

    #[cfg(feature = "libxml_xptr_locs")]
    let f = !(*ctxt).value.is_null()
        && !matches!(
            (*(*ctxt).value).typ,
            XmlXPathObjectType::XpathLocationset | XmlXPathObjectType::XpathNodeset
        );
    #[cfg(not(feature = "libxml_xptr_locs"))]
    let f = !(*ctxt).value.is_null() && (*(*ctxt).value).typ != XmlXPathObjectType::XpathNodeset;
    if f {
        xml_xptr_err(
            ctxt,
            XmlParserErrors::XmlXptrEvalFailed,
            c"xmlXPtrEval: evaluation failed to return a node set\n".as_ptr() as _,
            null(),
        );
    } else {
        res = value_pop(ctxt);
    }

    while {
        tmp = value_pop(ctxt);
        if !tmp.is_null() {
            if tmp != init {
                if (*tmp).typ == XmlXPathObjectType::XpathNodeset {
                    /*
                     * Evaluation may push a root nodeset which is unused
                     */
                    let set: XmlNodeSetPtr = (*tmp).nodesetval;
                    if set.is_null()
                        || (*set).node_nr != 1
                        || *(*set).node_tab.add(0) != (*ctx).doc as XmlNodePtr
                    {
                        stack += 1;
                    }
                } else {
                    stack += 1;
                }
            }
            xml_xpath_free_object(tmp);
        }

        !tmp.is_null()
    } {}
    if stack != 0 {
        xml_xptr_err(
            ctxt,
            XmlParserErrors::XmlXptrExtraObjects,
            c"xmlXPtrEval: object(s) left on the eval stack\n".as_ptr() as _,
            null(),
        );
    }
    if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
        xml_xpath_free_object(res);
        res = null_mut();
    }

    xml_xpath_free_parser_context(ctxt);
    res
}

/**
 * xmlXPtrRangeToFunction:
 * @ctxt:  the XPointer Parser context
 * @nargs:  the number of args
 *
 * Implement the range-to() XPointer function
 *
 * Obsolete. range-to is not a real function but a special type of location
 * step which is handled in xpath.c.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_range_to_function(
    ctxt: XmlXPathParserContextPtr,
    _nargs: c_int,
) {
    XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
}

/**
 * xmlXPtrBuildRangeNodeList:
 * @range:  a range object
 *
 * Build a node list tree copy of the range
 *
 * Returns an xmlNodePtr list or NULL.
 *         the caller has to free the node tree.
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xptr_build_range_node_list(range: XmlXPathObjectPtr) -> XmlNodePtr {
    /* pointers to generated nodes */
    let mut list: XmlNodePtr = null_mut();
    let mut last: XmlNodePtr = null_mut();
    let mut parent: XmlNodePtr = null_mut();
    let mut tmp: XmlNodePtr;
    /* pointers to traversal nodes */
    let mut cur: XmlNodePtr;
    let mut end: XmlNodePtr;
    let mut index1: c_int;
    let mut index2: c_int;

    if range.is_null() {
        return null_mut();
    }
    if (*range).typ != XmlXPathObjectType::XpathRange {
        return null_mut();
    }
    let start: XmlNodePtr = (*range).user as XmlNodePtr;

    if start.is_null() || (*start).typ == XmlElementType::XmlNamespaceDecl {
        return null_mut();
    }
    end = (*range).user2 as _;
    if end.is_null() {
        return xmlCopyNode(start, 1);
    }
    if (*end).typ == XmlElementType::XmlNamespaceDecl {
        return null_mut();
    }

    cur = start;
    index1 = (*range).index;
    index2 = (*range).index2;
    while !cur.is_null() {
        if cur == end {
            if (*cur).typ == XmlElementType::XmlTextNode {
                let mut content: *const XmlChar = (*cur).content;
                let mut len: c_int;

                if content.is_null() {
                    tmp = xmlNewTextLen(null_mut(), 0);
                } else {
                    len = index2;
                    if cur == start && index1 > 1 {
                        content = content.add(index1 as usize - 1);
                        len -= index1 - 1;
                        index1 = 0;
                    } else {
                        len = index2;
                    }
                    tmp = xmlNewTextLen(content, len);
                }
                /* single sub text node selection */
                if list.is_null() {
                    return tmp;
                }
                /* prune and return full set */
                if !last.is_null() {
                    xml_add_next_sibling(last, tmp);
                } else {
                    xmlAddChild(parent, tmp);
                }
                return list;
            } else {
                tmp = xmlCopyNode(cur, 0);
                if list.is_null() {
                    list = tmp;
                    parent = tmp;
                } else if !last.is_null() {
                    parent = xml_add_next_sibling(last, tmp);
                } else {
                    parent = xmlAddChild(parent, tmp);
                }
                last = null_mut();

                if index2 > 1 {
                    end = xml_xptr_get_nth_child(cur, index2 - 1);
                    index2 = 0;
                }
                if cur == start && index1 > 1 {
                    cur = xml_xptr_get_nth_child(cur, index1 - 1);
                    index1 = 0;
                } else {
                    cur = (*cur).children;
                }
                /*
                 * Now gather the remaining nodes from cur to end
                 */
                continue; /* while */
            }
        } else if cur == start && list.is_null() {
            /* looks superfluous but ... */
            if matches!(
                (*cur).typ,
                XmlElementType::XmlTextNode | XmlElementType::XmlCdataSectionNode
            ) {
                let mut content: *const XmlChar = (*cur).content;

                if content.is_null() {
                    tmp = xmlNewTextLen(null_mut(), 0);
                } else {
                    if index1 > 1 {
                        content = content.add(index1 as usize - 1);
                    }
                    tmp = xmlNewText(content);
                }
                last = tmp;
                list = tmp;
            } else {
                if cur == start && index1 > 1 {
                    tmp = xmlCopyNode(cur, 0);
                    list = tmp;
                    parent = tmp;
                    last = null_mut();
                    cur = xml_xptr_get_nth_child(cur, index1 - 1);
                    index1 = 0;
                    /*
                     * Now gather the remaining nodes from cur to end
                     */
                    continue; /* while */
                }
                tmp = xmlCopyNode(cur, 1);
                list = tmp;
                parent = null_mut();
                last = tmp;
            }
        } else {
            tmp = null_mut();
            match (*cur).typ {
                XmlElementType::XmlDtdNode
                | XmlElementType::XmlElementDecl
                | XmlElementType::XmlAttributeDecl
                | XmlElementType::XmlEntityNode => { /* Do not copy DTD information */ }
                XmlElementType::XmlEntityDecl => {
                    // TODO /* handle crossing entities -> stack needed */
                }
                XmlElementType::XmlXincludeStart | XmlElementType::XmlXincludeEnd => {
                    /* don't consider it part of the tree content */
                }
                XmlElementType::XmlAttributeNode => {
                    /* Humm, should not happen ! */
                    STRANGE!();
                }
                _ => {
                    tmp = xmlCopyNode(cur, 1);
                }
            }
            if !tmp.is_null() {
                if list.is_null() || (last.is_null() && parent.is_null()) {
                    STRANGE!();
                    return null_mut();
                }
                if !last.is_null() {
                    xml_add_next_sibling(last, tmp);
                } else {
                    last = xmlAddChild(parent, tmp);
                }
            }
        }
        /*
         * Skip to next node in document order
         */
        if list.is_null() || (last.is_null() && parent.is_null()) {
            STRANGE!();
            return null_mut();
        }
        cur = xml_xptr_advance_node(cur, null_mut());
    }
    list
}

/**
 * xmlXPtrBuildNodeList:
 * @obj:  the XPointer result from the evaluation.
 *
 * Build a node list tree copy of the XPointer result.
 * This will drop Attributes and Namespace declarations.
 *
 * Returns an xmlNodePtr list or NULL.
 *         the caller has to free the node tree.
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_build_node_list(obj: XmlXPathObjectPtr) -> XmlNodePtr {
    let mut list: XmlNodePtr = null_mut();
    let mut last: XmlNodePtr = null_mut();

    if obj.is_null() {
        return null_mut();
    }
    match (*obj).typ {
        XmlXPathObjectType::XpathNodeset => {
            let set: XmlNodeSetPtr = (*obj).nodesetval;
            if set.is_null() {
                return null_mut();
            }
            for i in 0..(*set).node_nr {
                if (*(*set).node_tab.add(i as usize)).is_null() {
                    continue;
                }
                match (*(*(*set).node_tab.add(i as usize))).typ {
                    XmlElementType::XmlTextNode
                    | XmlElementType::XmlCdataSectionNode
                    | XmlElementType::XmlElementNode
                    | XmlElementType::XmlEntityRefNode
                    | XmlElementType::XmlEntityNode
                    | XmlElementType::XmlPiNode
                    | XmlElementType::XmlCommentNode
                    | XmlElementType::XmlDocumentNode
                    | XmlElementType::XmlHtmlDocumentNode
                    | XmlElementType::XmlXincludeStart
                    | XmlElementType::XmlXincludeEnd => {}
                    XmlElementType::XmlAttributeNode
                    | XmlElementType::XmlNamespaceDecl
                    | XmlElementType::XmlDocumentTypeNode
                    | XmlElementType::XmlDocumentFragNode
                    | XmlElementType::XmlNotationNode
                    | XmlElementType::XmlDtdNode
                    | XmlElementType::XmlElementDecl
                    | XmlElementType::XmlAttributeDecl
                    | XmlElementType::XmlEntityDecl => continue, /* for */
                    _ => unreachable!(),
                }
                if last.is_null() {
                    list = xmlCopyNode(*(*set).node_tab.add(i as usize), 1);
                    last = list;
                } else {
                    xml_add_next_sibling(last, xmlCopyNode(*(*set).node_tab.add(i as usize), 1));
                    if !(*last).next.is_null() {
                        last = (*last).next;
                    }
                }
            }
        }
        XmlXPathObjectType::XpathLocationset => {
            let set: XmlLocationSetPtr = (*obj).user as XmlLocationSetPtr;
            if set.is_null() {
                return null_mut();
            }
            for i in 0..(*set).loc_nr {
                if last.is_null() {
                    list = xml_xptr_build_node_list(*(*set).loc_tab.add(i as usize));
                    last = list;
                } else {
                    xml_add_next_sibling(
                        last,
                        xml_xptr_build_node_list(*(*set).loc_tab.add(i as usize)),
                    );
                }
                if !last.is_null() {
                    while !(*last).next.is_null() {
                        last = (*last).next;
                    }
                }
            }
        }
        XmlXPathObjectType::XpathRange => return xml_xptr_build_range_node_list(obj),
        XmlXPathObjectType::XpathPoint => return xmlCopyNode((*obj).user as _, 0),
        _ => {}
    }
    list
}

/**
 * xmlXPtrEvalRangePredicate:
 * @ctxt:  the XPointer Parser context
 *
 *  [8]   Predicate ::=   '[' PredicateExpr ']'
 *  [9]   PredicateExpr ::=   Expr
 *
 * Evaluate a predicate as in xmlXPathEvalPredicate() but for
 * a Location Set instead of a node set
 */
#[cfg(feature = "libxml_xptr_locs")]
pub(crate) unsafe extern "C" fn xml_xptr_eval_range_predicate(ctxt: XmlXPathParserContextPtr) {
    let cur: *const XmlChar;
    let mut res: XmlXPathObjectPtr;
    let mut tmp: XmlXPathObjectPtr;
    let newset: XmlLocationSetPtr;

    if ctxt.is_null() {
        return;
    }

    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) != b'[' {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidPredicateError as i32);
    }
    NEXT!(ctxt);
    SKIP_BLANKS!(ctxt);

    /*
     * Extract the old set, and then evaluate the result of the
     * expression for all the element in the set. use it to grow
     * up a new set.
     */
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathLocationset);
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    let oldset: XmlLocationSetPtr = (*obj).user as _;
    (*(*ctxt).context).node = null_mut();

    if oldset.is_null() || (*oldset).loc_nr == 0 {
        (*(*ctxt).context).context_size = 0;
        (*(*ctxt).context).proximity_position = 0;
        xml_xpath_eval_expr(ctxt);
        res = value_pop(ctxt);
        if !res.is_null() {
            xml_xpath_free_object(res);
        }
        value_push(ctxt, obj);
        CHECK_ERROR!(ctxt);
    } else {
        /*
         * Save the expression pointer since we will have to evaluate
         * it multiple times. Initialize the new set.
         */
        cur = (*ctxt).cur;
        newset = xml_xptr_location_set_create(null_mut());

        for i in 0..(*oldset).loc_nr {
            (*ctxt).cur = cur;

            /*
             * Run the evaluation with a node list made of a single item
             * in the nodeset.
             */
            (*(*ctxt).context).node = (*(*(*oldset).loc_tab.add(i as usize))).user as _;
            tmp = xml_xpath_new_node_set((*(*ctxt).context).node);
            value_push(ctxt, tmp);
            (*(*ctxt).context).context_size = (*oldset).loc_nr;
            (*(*ctxt).context).proximity_position = i + 1;

            xml_xpath_eval_expr(ctxt);
            CHECK_ERROR!(ctxt);

            /*
             * The result of the evaluation need to be tested to
             * decided whether the filter succeeded or not
             */
            res = value_pop(ctxt);
            if xml_xpath_evaluate_predicate_result(ctxt, res) != 0 {
                xml_xptr_location_set_add(
                    newset,
                    xml_xpath_object_copy(*(*oldset).loc_tab.add(i as usize)),
                );
            }

            /*
             * Cleanup
             */
            if !res.is_null() {
                xml_xpath_free_object(res);
            }
            if (*ctxt).value == tmp {
                res = value_pop(ctxt);
                xml_xpath_free_object(res);
            }

            (*(*ctxt).context).node = null_mut();
        }

        /*
         * The result is used as the new evaluation set.
         */
        xml_xpath_free_object(obj);
        (*(*ctxt).context).node = null_mut();
        (*(*ctxt).context).context_size = -1;
        (*(*ctxt).context).proximity_position = -1;
        value_push(ctxt, xml_xptr_wrap_location_set(newset));
    }
    if CUR!(ctxt) != b']' {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidPredicateError as i32);
    }

    NEXT!(ctxt);
    SKIP_BLANKS!(ctxt);
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_xptr_build_node_list() {
        #[cfg(all(feature = "libxml_xptr", feature = "libxml_xptr_locs"))]
        unsafe {
            let mut leaks = 0;

            for n_obj in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                let mem_base = xml_mem_blocks();
                let obj = gen_xml_xpath_object_ptr(n_obj, 0);

                let ret_val = xml_xptr_build_node_list(obj);
                desret_xml_node_ptr(ret_val);
                des_xml_xpath_object_ptr(n_obj, obj, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPtrBuildNodeList",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPtrBuildNodeList()"
                    );
                    eprintln!(" {}", n_obj);
                }
            }
        }
    }

    #[test]
    fn test_xml_xptr_eval() {
        #[cfg(feature = "libxml_xptr")]
        unsafe {
            let mut leaks = 0;

            for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_ctx in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                    let mem_base = xml_mem_blocks();
                    let str = gen_const_xml_char_ptr(n_str, 0);
                    let ctx = gen_xml_xpath_context_ptr(n_ctx, 1);

                    let ret_val = xml_xptr_eval(str as *const XmlChar, ctx);
                    desret_xml_xpath_object_ptr(ret_val);
                    des_const_xml_char_ptr(n_str, str, 0);
                    des_xml_xpath_context_ptr(n_ctx, ctx, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPtrEval",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlXPtrEval()");
                        eprint!(" {}", n_str);
                        eprintln!(" {}", n_ctx);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xptr_eval_range_predicate() {
        #[cfg(all(feature = "libxml_xptr", feature = "libxml_xptr_locs"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                xml_xptr_eval_range_predicate(ctxt);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPtrEvalRangePredicate",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPtrEvalRangePredicate()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xptr_new_collapsed_range() {
        #[cfg(all(feature = "libxml_xptr", feature = "libxml_xptr_locs"))]
        unsafe {
            let mut leaks = 0;

            for n_start in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let start = gen_xml_node_ptr(n_start, 0);

                let ret_val = xml_xptr_new_collapsed_range(start);
                desret_xml_xpath_object_ptr(ret_val);
                des_xml_node_ptr(n_start, start, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPtrNewCollapsedRange",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPtrNewCollapsedRange()"
                    );
                    eprintln!(" {}", n_start);
                }
            }
        }
    }

    #[test]
    fn test_xml_xptr_new_context() {

        /* missing type support */
    }

    #[test]
    fn test_xml_xptr_new_location_set_node_set() {
        #[cfg(all(feature = "libxml_xptr", feature = "libxml_xptr_locs"))]
        unsafe {
            let mut leaks = 0;

            for n_set in 0..GEN_NB_XML_NODE_SET_PTR {
                let mem_base = xml_mem_blocks();
                let set = gen_xml_node_set_ptr(n_set, 0);

                let ret_val = xml_xptr_new_location_set_node_set(set);
                desret_xml_xpath_object_ptr(ret_val);
                des_xml_node_set_ptr(n_set, set, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPtrNewLocationSetNodeSet",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPtrNewLocationSetNodeSet()"
                    );
                    eprintln!(" {}", n_set);
                }
            }
        }
    }

    #[test]
    fn test_xml_xptr_new_location_set_nodes() {
        #[cfg(all(feature = "libxml_xptr", feature = "libxml_xptr_locs"))]
        unsafe {
            let mut leaks = 0;

            for n_start in 0..GEN_NB_XML_NODE_PTR {
                for n_end in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let start = gen_xml_node_ptr(n_start, 0);
                    let end = gen_xml_node_ptr(n_end, 1);

                    let ret_val = xml_xptr_new_location_set_nodes(start, end);
                    desret_xml_xpath_object_ptr(ret_val);
                    des_xml_node_ptr(n_start, start, 0);
                    des_xml_node_ptr(n_end, end, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPtrNewLocationSetNodes",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPtrNewLocationSetNodes()"
                        );
                        eprint!(" {}", n_start);
                        eprintln!(" {}", n_end);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xptr_new_range() {
        #[cfg(all(feature = "libxml_xptr", feature = "libxml_xptr_locs"))]
        unsafe {
            let mut leaks = 0;

            for n_start in 0..GEN_NB_XML_NODE_PTR {
                for n_startindex in 0..GEN_NB_INT {
                    for n_end in 0..GEN_NB_XML_NODE_PTR {
                        for n_endindex in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let start = gen_xml_node_ptr(n_start, 0);
                            let startindex = gen_int(n_startindex, 1);
                            let end = gen_xml_node_ptr(n_end, 2);
                            let endindex = gen_int(n_endindex, 3);

                            let ret_val = xml_xptr_new_range(start, startindex, end, endindex);
                            desret_xml_xpath_object_ptr(ret_val);
                            des_xml_node_ptr(n_start, start, 0);
                            des_int(n_startindex, startindex, 1);
                            des_xml_node_ptr(n_end, end, 2);
                            des_int(n_endindex, endindex, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlXPtrNewRange",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlXPtrNewRange()");
                                eprint!(" {}", n_start);
                                eprint!(" {}", n_startindex);
                                eprint!(" {}", n_end);
                                eprintln!(" {}", n_endindex);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xptr_new_range_node_object() {
        #[cfg(all(feature = "libxml_xptr", feature = "libxml_xptr_locs"))]
        unsafe {
            let mut leaks = 0;

            for n_start in 0..GEN_NB_XML_NODE_PTR {
                for n_end in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                    let mem_base = xml_mem_blocks();
                    let start = gen_xml_node_ptr(n_start, 0);
                    let end = gen_xml_xpath_object_ptr(n_end, 1);

                    let ret_val = xml_xptr_new_range_node_object(start, end);
                    desret_xml_xpath_object_ptr(ret_val);
                    des_xml_node_ptr(n_start, start, 0);
                    des_xml_xpath_object_ptr(n_end, end, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPtrNewRangeNodeObject",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPtrNewRangeNodeObject()"
                        );
                        eprint!(" {}", n_start);
                        eprintln!(" {}", n_end);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xptr_new_range_node_point() {
        #[cfg(all(feature = "libxml_xptr", feature = "libxml_xptr_locs"))]
        unsafe {
            let mut leaks = 0;

            for n_start in 0..GEN_NB_XML_NODE_PTR {
                for n_end in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                    let mem_base = xml_mem_blocks();
                    let start = gen_xml_node_ptr(n_start, 0);
                    let end = gen_xml_xpath_object_ptr(n_end, 1);

                    let ret_val = xml_xptr_new_range_node_point(start, end);
                    desret_xml_xpath_object_ptr(ret_val);
                    des_xml_node_ptr(n_start, start, 0);
                    des_xml_xpath_object_ptr(n_end, end, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPtrNewRangeNodePoint",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPtrNewRangeNodePoint()"
                        );
                        eprint!(" {}", n_start);
                        eprintln!(" {}", n_end);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xptr_new_range_nodes() {
        #[cfg(all(feature = "libxml_xptr", feature = "libxml_xptr_locs"))]
        unsafe {
            let mut leaks = 0;

            for n_start in 0..GEN_NB_XML_NODE_PTR {
                for n_end in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let start = gen_xml_node_ptr(n_start, 0);
                    let end = gen_xml_node_ptr(n_end, 1);

                    let ret_val = xml_xptr_new_range_nodes(start, end);
                    desret_xml_xpath_object_ptr(ret_val);
                    des_xml_node_ptr(n_start, start, 0);
                    des_xml_node_ptr(n_end, end, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPtrNewRangeNodes",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPtrNewRangeNodes()"
                        );
                        eprint!(" {}", n_start);
                        eprintln!(" {}", n_end);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xptr_new_range_point_node() {
        #[cfg(all(feature = "libxml_xptr", feature = "libxml_xptr_locs"))]
        unsafe {
            let mut leaks = 0;

            for n_start in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                for n_end in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let start = gen_xml_xpath_object_ptr(n_start, 0);
                    let end = gen_xml_node_ptr(n_end, 1);

                    let ret_val = xml_xptr_new_range_point_node(start, end);
                    desret_xml_xpath_object_ptr(ret_val);
                    des_xml_xpath_object_ptr(n_start, start, 0);
                    des_xml_node_ptr(n_end, end, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPtrNewRangePointNode",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPtrNewRangePointNode()"
                        );
                        eprint!(" {}", n_start);
                        eprintln!(" {}", n_end);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xptr_new_range_points() {
        #[cfg(all(feature = "libxml_xptr", feature = "libxml_xptr_locs"))]
        unsafe {
            let mut leaks = 0;

            for n_start in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                for n_end in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                    let mem_base = xml_mem_blocks();
                    let start = gen_xml_xpath_object_ptr(n_start, 0);
                    let end = gen_xml_xpath_object_ptr(n_end, 1);

                    let ret_val = xml_xptr_new_range_points(start, end);
                    desret_xml_xpath_object_ptr(ret_val);
                    des_xml_xpath_object_ptr(n_start, start, 0);
                    des_xml_xpath_object_ptr(n_end, end, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPtrNewRangePoints",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPtrNewRangePoints()"
                        );
                        eprint!(" {}", n_start);
                        eprintln!(" {}", n_end);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xptr_range_to_function() {
        #[cfg(all(feature = "libxml_xptr", feature = "libxml_xptr_locs"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xptr_range_to_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPtrRangeToFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPtrRangeToFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }
}

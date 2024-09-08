//! Provide methods and data structures for lists.  
//! This module is based on `libxml/list.h`, `list.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{cmp::Ordering, ffi::c_int, mem::size_of, os::raw::c_void, ptr::null_mut};

use libc::memset;

use crate::{
    libxml::globals::{xml_free, xml_generic_error_context},
    xml_generic_error,
};

use super::globals::xml_malloc;

pub type XmlLinkPtr = *mut XmlLink;
/*
* Type definition are kept internal
*/
#[repr(C)]
pub struct XmlLink {
    next: *mut XmlLink,
    prev: *mut XmlLink,
    data: *mut c_void,
}

pub type XmlListPtr = *mut XmlList;
#[repr(C)]
pub struct XmlList {
    sentinel: XmlLinkPtr,
    link_deallocator: Option<unsafe extern "C" fn(XmlLinkPtr)>,
    link_compare: Option<unsafe extern "C" fn(*const c_void, *const c_void) -> c_int>,
}

/**
 * xmlListDeallocator:
 * @lk:  the data to deallocate
 *
 * Callback function used to free data from a list.
 */
pub type XmlListDeallocator = unsafe extern "C" fn(lk: XmlLinkPtr);
/**
 * xmlListDataCompare:
 * @data0: the first data
 * @data1: the second data
 *
 * Callback function used to compare 2 data.
 *
 * Returns 0 is equality, -1 or 1 otherwise depending on the ordering.
 */
pub type XmlListDataCompare =
    unsafe extern "C" fn(data0: *const c_void, data1: *const c_void) -> c_int;
/**
 * xmlListWalker:
 * @data: the data found in the list
 * @user: extra user provided data to the walker
 *
 * Callback function used when walking a list with xmlListWalk().
 *
 * Returns 0 to stop walking the list, 1 otherwise.
 */
pub type XmlListWalker = unsafe extern "C" fn(data: *const c_void, user: *mut c_void) -> c_int;

/**
 * xmlLinkCompare:
 * @data0:  first data
 * @data1:  second data
 *
 * Compares two arbitrary data
 *
 * Returns -1, 0 or 1 depending on whether data1 is greater equal or smaller
 *          than data0
 */
unsafe extern "C" fn xml_link_compare(data0: *const c_void, data1: *const c_void) -> c_int {
    match data0.cmp(&data1) {
        Ordering::Less => -1,
        Ordering::Equal => 0,
        Ordering::Greater => 1,
    }
}

/* Creation/Deletion */
/**
 * xmlListCreate:
 * @deallocator:  an optional deallocator function
 * @compare:  an optional comparison function
 *
 * Create a new list
 *
 * Returns the new list or NULL in case of error
 */
pub unsafe extern "C" fn xml_list_create(
    deallocator: Option<XmlListDeallocator>,
    compare: Option<XmlListDataCompare>,
) -> XmlListPtr {
    let l: XmlListPtr = xml_malloc(size_of::<XmlList>()) as XmlListPtr;
    if l.is_null() {
        xml_generic_error!(
            xml_generic_error_context(),
            c"Cannot initialize memory for list".as_ptr() as _
        );
        return null_mut();
    }
    /* Initialize the list to null_mut() */
    memset(l as _, 0, size_of::<XmlList>());

    /* Add the sentinel */
    (*l).sentinel = xml_malloc(size_of::<XmlLink>()) as XmlLinkPtr;
    if (*l).sentinel.is_null() {
        xml_generic_error!(
            xml_generic_error_context(),
            c"Cannot initialize memory for sentinel".as_ptr() as _
        );
        xml_free(l as _);
        return null_mut();
    }
    (*(*l).sentinel).next = (*l).sentinel;
    (*(*l).sentinel).prev = (*l).sentinel;
    (*(*l).sentinel).data = null_mut();

    /* If there is a link deallocator, use it */
    if let Some(deallocator) = deallocator {
        (*l).link_deallocator = Some(deallocator);
    }

    /* If there is a link comparator, use it */
    (*l).link_compare = compare.or(Some(xml_link_compare));
    l
}

/**
 * xmlListDelete:
 * @l:  a list
 *
 * Deletes the list and its associated data
 */
pub unsafe extern "C" fn xml_list_delete(l: XmlListPtr) {
    if l.is_null() {
        return;
    }

    xml_list_clear(l);
    xml_free((*l).sentinel as _);
    xml_free(l as _);
}

/**
 * xmlListLowerSearch:
 * @l:  a list
 * @data:  a data
 *
 * Search data in the ordered list walking from the beginning
 *
 * Returns the link containing the data or NULL
 */
unsafe extern "C" fn xml_list_lower_search(l: XmlListPtr, data: *mut c_void) -> XmlLinkPtr {
    if l.is_null() {
        return null_mut();
    }

    let mut lk: XmlLinkPtr = (*(*l).sentinel).next;
    while lk != (*l).sentinel && ((*l).link_compare.unwrap())((*lk).data, data) < 0 {
        lk = (*lk).next;
    }
    lk
}

/**
 * xmlListSearch:
 * @l:  a list
 * @data:  a data
 *
 * Search data in the list
 *
 * Returns the link containing the data or NULL
 */
unsafe extern "C" fn xml_list_link_search(l: XmlListPtr, data: *mut c_void) -> XmlLinkPtr {
    if l.is_null() {
        return null_mut();
    }
    let lk: XmlLinkPtr = xml_list_lower_search(l, data);
    if lk == (*l).sentinel {
        null_mut()
    } else {
        if ((*l).link_compare.unwrap())((*lk).data, data) == 0 {
            return lk;
        }
        null_mut()
    }
}

/* Basic Operators */
/**
 * xmlListSearch:
 * @l:  a list
 * @data:  a search value
 *
 * Search the list for an existing value of @data
 *
 * Returns the value associated to @data or NULL in case of error
 */
pub unsafe extern "C" fn xml_list_search(l: XmlListPtr, data: *mut c_void) -> *mut c_void {
    if l.is_null() {
        return null_mut();
    }
    let lk: XmlLinkPtr = xml_list_link_search(l, data);
    if !lk.is_null() {
        return (*lk).data;
    }
    null_mut()
}

/**
 * xmlListHigherSearch:
 * @l:  a list
 * @data:  a data
 *
 * Search data in the ordered list walking backward from the end
 *
 * Returns the link containing the data or NULL
 */
unsafe extern "C" fn xml_list_higher_search(l: XmlListPtr, data: *mut c_void) -> XmlLinkPtr {
    if l.is_null() {
        return null_mut();
    }

    let mut lk = (*(*l).sentinel).prev;
    while lk != (*l).sentinel && ((*l).link_compare.unwrap())((*lk).data, data) > 0 {
        lk = (*lk).prev;
    }
    lk
}

/**
 * xmlListLinkReverseSearch:
 * @l:  a list
 * @data:  a data
 *
 * Search data in the list processing backward
 *
 * Returns the link containing the data or NULL
 */
unsafe extern "C" fn xml_list_link_reverse_search(l: XmlListPtr, data: *mut c_void) -> XmlLinkPtr {
    if l.is_null() {
        return null_mut();
    }
    let lk: XmlLinkPtr = xml_list_higher_search(l, data);
    if lk == (*l).sentinel {
        null_mut()
    } else {
        if ((*l).link_compare.unwrap())((*lk).data, data) == 0 {
            return lk;
        }
        null_mut()
    }
}

/**
 * xmlListReverseSearch:
 * @l:  a list
 * @data:  a search value
 *
 * Search the list in reverse order for an existing value of @data
 *
 * Returns the value associated to @data or NULL in case of error
 */
pub unsafe extern "C" fn xml_list_reverse_search(l: XmlListPtr, data: *mut c_void) -> *mut c_void {
    if l.is_null() {
        return null_mut();
    }
    let lk: XmlLinkPtr = xml_list_link_reverse_search(l, data);
    if !lk.is_null() {
        return (*lk).data;
    }
    null_mut()
}

/**
 * xmlListInsert:
 * @l:  a list
 * @data:  the data
 *
 * Insert data in the ordered list at the beginning for this value
 *
 * Returns 0 in case of success, 1 in case of failure
 */
pub unsafe extern "C" fn xml_list_insert(l: XmlListPtr, data: *mut c_void) -> c_int {
    let mut lk_place: XmlLinkPtr;

    if l.is_null() {
        return 1;
    }
    lk_place = xml_list_lower_search(l, data);
    /* Add the new link */
    let lk_new: XmlLinkPtr = xml_malloc(size_of::<XmlLink>()) as XmlLinkPtr;
    if lk_new.is_null() {
        xml_generic_error!(
            xml_generic_error_context(),
            c"Cannot initialize memory for new link".as_ptr() as _
        );
        return 1;
    }
    (*lk_new).data = data;
    lk_place = (*lk_place).prev;
    (*lk_new).next = (*lk_place).next;
    (*(*lk_place).next).prev = lk_new;
    (*lk_place).next = lk_new;
    (*lk_new).prev = lk_place;
    0
}

/**
 * xmlListAppend:
 * @l:  a list
 * @data:  the data
 *
 * Insert data in the ordered list at the end for this value
 *
 * Returns 0 in case of success, 1 in case of failure
 */
pub unsafe extern "C" fn xml_list_append(l: XmlListPtr, data: *mut c_void) -> c_int {
    if l.is_null() {
        return 1;
    }
    let lk_place: XmlLinkPtr = xml_list_higher_search(l, data);
    /* Add the new link */
    let lk_new: XmlLinkPtr = xml_malloc(size_of::<XmlLink>()) as XmlLinkPtr;
    if lk_new.is_null() {
        xml_generic_error!(
            xml_generic_error_context(),
            c"Cannot initialize memory for new link".as_ptr() as _
        );
        return 1;
    }
    (*lk_new).data = data;
    (*lk_new).next = (*lk_place).next;
    (*(*lk_place).next).prev = lk_new;
    (*lk_place).next = lk_new;
    (*lk_new).prev = lk_place;
    0
}

/**
 * xmlLinkDeallocator:
 * @l:  a list
 * @lk:  a link
 *
 * Unlink and deallocate @lk from list @l
 */
unsafe extern "C" fn xml_link_deallocator(l: XmlListPtr, lk: XmlLinkPtr) {
    (*(*lk).prev).next = (*lk).next;
    (*(*lk).next).prev = (*lk).prev;
    if let Some(deallocator) = (*l).link_deallocator {
        deallocator(lk);
    }
    xml_free(lk as _);
}

/**
 * xmlListRemoveFirst:
 * @l:  a list
 * @data:  list data
 *
 * Remove the first instance associated to data in the list
 *
 * Returns 1 if a deallocation occurred, or 0 if not found
 */
pub unsafe extern "C" fn xml_list_remove_first(l: XmlListPtr, data: *mut c_void) -> c_int {
    if l.is_null() {
        return 0;
    }
    /*Find the first instance of this data */
    let lk: XmlLinkPtr = xml_list_link_search(l, data);
    if !lk.is_null() {
        xml_link_deallocator(l, lk);
        return 1;
    }
    0
}

/**
 * xmlListRemoveLast:
 * @l:  a list
 * @data:  list data
 *
 * Remove the last instance associated to data in the list
 *
 * Returns 1 if a deallocation occurred, or 0 if not found
 */
pub unsafe extern "C" fn xml_list_remove_last(l: XmlListPtr, data: *mut c_void) -> c_int {
    if l.is_null() {
        return 0;
    }
    /*Find the last instance of this data */
    let lk: XmlLinkPtr = xml_list_link_reverse_search(l, data);
    if !lk.is_null() {
        xml_link_deallocator(l, lk);
        return 1;
    }
    0
}

/**
 * xmlListRemoveAll:
 * @l:  a list
 * @data:  list data
 *
 * Remove the all instance associated to data in the list
 *
 * Returns the number of deallocation, or 0 if not found
 */
pub unsafe extern "C" fn xml_list_remove_all(l: XmlListPtr, data: *mut c_void) -> c_int {
    let mut count: c_int = 0;

    if l.is_null() {
        return 0;
    }

    while xml_list_remove_first(l, data) != 0 {
        count += 1;
    }
    count
}

/**
 * xmlListClear:
 * @l:  a list
 *
 * Remove the all data in the list
 */
pub unsafe extern "C" fn xml_list_clear(l: XmlListPtr) {
    let mut lk: XmlLinkPtr;

    if l.is_null() {
        return;
    }
    lk = (*(*l).sentinel).next;
    while lk != (*l).sentinel {
        let next: XmlLinkPtr = (*lk).next;

        xml_link_deallocator(l, lk);
        lk = next;
    }
}

/**
 * xmlListEmpty:
 * @l:  a list
 *
 * Is the list empty ?
 *
 * Returns 1 if the list is empty, 0 if not empty and -1 in case of error
 */
pub unsafe extern "C" fn xml_list_empty(l: XmlListPtr) -> c_int {
    if l.is_null() {
        return -1;
    }
    ((*(*l).sentinel).next == (*l).sentinel) as i32
}

/**
 * xmlListFront:
 * @l:  a list
 *
 * Get the first element in the list
 *
 * Returns the first element in the list, or NULL
 */
pub unsafe extern "C" fn xml_list_front(l: XmlListPtr) -> XmlLinkPtr {
    if l.is_null() {
        return null_mut();
    }
    (*(*l).sentinel).next
}

/**
 * xmlListEnd:
 * @l:  a list
 *
 * Get the last element in the list
 *
 * Returns the last element in the list, or NULL
 */
pub unsafe extern "C" fn xml_list_end(l: XmlListPtr) -> XmlLinkPtr {
    if l.is_null() {
        return null_mut();
    }
    (*(*l).sentinel).prev
}

/**
 * xmlListSize:
 * @l:  a list
 *
 * Get the number of elements in the list
 *
 * Returns the number of elements in the list or -1 in case of error
 */
pub unsafe extern "C" fn xml_list_size(l: XmlListPtr) -> c_int {
    let mut lk: XmlLinkPtr;
    let mut count: c_int = 0;

    if l.is_null() {
        return -1;
    }
    /* TODO: keep a counter in xmlList instead */
    lk = (*(*l).sentinel).next;
    while lk != (*l).sentinel {
        lk = (*lk).next;
        count += 1;
    }
    count
}

/**
 * xmlListPopFront:
 * @l:  a list
 *
 * Removes the first element in the list
 */
pub unsafe extern "C" fn xml_list_pop_front(l: XmlListPtr) {
    if xml_list_empty(l) == 0 {
        xml_link_deallocator(l, (*(*l).sentinel).next);
    }
}

/**
 * xmlListPopBack:
 * @l:  a list
 *
 * Removes the last element in the list
 */
pub unsafe extern "C" fn xml_list_pop_back(l: XmlListPtr) {
    if xml_list_empty(l) == 0 {
        xml_link_deallocator(l, (*(*l).sentinel).prev);
    }
}

/**
 * xmlListPushFront:
 * @l:  a list
 * @data:  new data
 *
 * add the new data at the beginning of the list
 *
 * Returns 1 if successful, 0 otherwise
 */
pub unsafe extern "C" fn xml_list_push_front(l: XmlListPtr, data: *mut c_void) -> c_int {
    if l.is_null() {
        return 0;
    }
    let lk_place: XmlLinkPtr = (*l).sentinel;
    /* Add the new link */
    let lk_new: XmlLinkPtr = xml_malloc(size_of::<XmlLink>()) as XmlLinkPtr;
    if lk_new.is_null() {
        xml_generic_error!(
            xml_generic_error_context(),
            c"Cannot initialize memory for new link".as_ptr() as _
        );
        return 0;
    }
    (*lk_new).data = data;
    (*lk_new).next = (*lk_place).next;
    (*(*lk_place).next).prev = lk_new;
    (*lk_place).next = lk_new;
    (*lk_new).prev = lk_place;
    1
}

/**
 * xmlListPushBack:
 * @l:  a list
 * @data:  new data
 *
 * add the new data at the end of the list
 *
 * Returns 1 if successful, 0 otherwise
 */
pub unsafe extern "C" fn xml_list_push_back(l: XmlListPtr, data: *mut c_void) -> c_int {
    if l.is_null() {
        return 0;
    }
    let lk_place: XmlLinkPtr = (*(*l).sentinel).prev;
    /* Add the new link */
    let lk_new: XmlLinkPtr = xml_malloc(size_of::<XmlLink>()) as XmlLinkPtr;
    if lk_new.is_null() {
        xml_generic_error!(
            xml_generic_error_context(),
            c"Cannot initialize memory for new link".as_ptr() as _
        );
        return 0;
    }
    (*lk_new).data = data;
    (*lk_new).next = (*lk_place).next;
    (*(*lk_place).next).prev = lk_new;
    (*lk_place).next = lk_new;
    (*lk_new).prev = lk_place;
    1
}

/* Advanced Operators */
/**
 * xmlListReverse:
 * @l:  a list
 *
 * Reverse the order of the elements in the list
 */
pub unsafe extern "C" fn xml_list_reverse(l: XmlListPtr) {
    let mut lk: XmlLinkPtr;
    let mut lk_prev: XmlLinkPtr;

    if l.is_null() {
        return;
    }
    lk_prev = (*l).sentinel;
    lk = (*(*l).sentinel).next;
    while lk != (*l).sentinel {
        (*lk_prev).next = (*lk_prev).prev;
        (*lk_prev).prev = lk;
        lk_prev = lk;
        lk = (*lk).next;
    }
    /* Fix up the last node */
    (*lk_prev).next = (*lk_prev).prev;
    (*lk_prev).prev = lk;
}

/**
 * xmlListSort:
 * @l:  a list
 *
 * Sort all the elements in the list
 */
pub unsafe extern "C" fn xml_list_sort(l: XmlListPtr) {
    if l.is_null() {
        return;
    }
    if xml_list_empty(l) != 0 {
        return;
    }

    /* I think that the real answer is to implement quicksort, the
     * alternative is to implement some list copying procedure which
     * would be based on a list copy followed by a clear followed by
     * an insert. This is slow...
     */

    let l_temp: XmlListPtr = xml_list_dup(l);
    if l_temp.is_null() {
        return;
    }
    xml_list_clear(l);
    xml_list_merge(l, l_temp);
    xml_list_delete(l_temp);
}

/**
 * xmlListWalk:
 * @l:  a list
 * @walker:  a processing function
 * @user:  a user parameter passed to the walker function
 *
 * Walk all the element of the first from first to last and
 * apply the walker function to it
 */
pub unsafe extern "C" fn xml_list_walk(
    l: XmlListPtr,
    walker: Option<XmlListWalker>,
    user: *mut c_void,
) {
    let mut lk: XmlLinkPtr;

    if l.is_null() || walker.is_none() {
        return;
    }

    let walker = walker.unwrap();
    lk = (*(*l).sentinel).next;
    while lk != (*l).sentinel {
        if walker((*lk).data, user) == 0 {
            break;
        }
        lk = (*lk).next;
    }
}

/**
 * xmlListReverseWalk:
 * @l:  a list
 * @walker:  a processing function
 * @user:  a user parameter passed to the walker function
 *
 * Walk all the element of the list in reverse order and
 * apply the walker function to it
 */
pub unsafe extern "C" fn xml_list_reverse_walk(
    l: XmlListPtr,
    walker: Option<XmlListWalker>,
    user: *mut c_void,
) {
    let mut lk: XmlLinkPtr;

    if l.is_null() || walker.is_none() {
        return;
    }

    let walker = walker.unwrap();
    lk = (*(*l).sentinel).prev;
    while lk != (*l).sentinel {
        if walker((*lk).data, user) == 0 {
            break;
        }
        lk = (*lk).prev
    }
}

/**
 * xmlListMerge:
 * @l1:  the original list
 * @l2:  the new list
 *
 * include all the elements of the second list in the first one and
 * clear the second list
 */
pub unsafe extern "C" fn xml_list_merge(l1: XmlListPtr, l2: XmlListPtr) {
    xml_list_copy(l1, l2);
    xml_list_clear(l2);
}

/**
 * xmlListDup:
 * @old:  the list
 *
 * Duplicate the list
 *
 * Returns a new copy of the list or NULL in case of error
 */
pub unsafe extern "C" fn xml_list_dup(old: XmlListPtr) -> XmlListPtr {
    if old.is_null() {
        return null_mut();
    }
    /* Hmmm, how to best deal with allocation issues when copying
     * lists. If there is a de-allocator, should responsibility lie with
     * the new list or the old list. Surely not both. I'll arbitrarily
     * set it to be the old list for the time being whilst I work out
     * the answer
     */
    let cur: XmlListPtr = xml_list_create(None, (*old).link_compare);
    if cur.is_null() {
        return null_mut();
    }
    if xml_list_copy(cur, old) != 0 {
        return null_mut();
    }
    cur
}

/**
 * xmlListCopy:
 * @cur:  the new list
 * @old:  the old list
 *
 * Move all the element from the old list in the new list
 *
 * Returns 0 in case of success 1 in case of error
 */
pub unsafe extern "C" fn xml_list_copy(cur: XmlListPtr, old: XmlListPtr) -> c_int {
    /* Walk the old tree and insert the data into the new one */
    let mut lk: XmlLinkPtr;

    if old.is_null() || cur.is_null() {
        return 1;
    }
    lk = (*(*old).sentinel).next;
    while lk != (*old).sentinel {
        if xml_list_insert(cur, (*lk).data) != 0 {
            xml_list_delete(cur);
            return 1;
        }
        lk = (*lk).next;
    }
    0
}

/* Link operators */
/**
 * xmlLinkGetData:
 * @lk:  a link
 *
 * See Returns.
 *
 * Returns a pointer to the data referenced from this link
 */
pub unsafe extern "C" fn xml_link_get_data(lk: XmlLinkPtr) -> *mut c_void {
    if lk.is_null() {
        return null_mut();
    }
    (*lk).data
}

/* xmlListUnique() */
/* xmlListSwap */

#[cfg(test)]
mod tests {
    use crate::{
        libxml::{xmlerror::xml_reset_last_error, xmlmemory::xml_mem_blocks},
        test_util::*,
    };

    use super::*;

    #[test]
    fn test_xml_link_get_data() {
        unsafe {
            let mut leaks = 0;
            for n_lk in 0..GEN_NB_XML_LINK_PTR {
                let mem_base = xml_mem_blocks();
                let lk = gen_xml_link_ptr(n_lk, 0);

                let ret_val = xml_link_get_data(lk);
                desret_void_ptr(ret_val);
                des_xml_link_ptr(n_lk, lk, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlLinkGetData",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_lk);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlLinkGetData()");
        }
    }

    #[test]
    fn test_xml_list_append() {
        unsafe {
            let mut leaks = 0;
            for n_l in 0..GEN_NB_XML_LIST_PTR {
                for n_data in 0..GEN_NB_USERDATA {
                    let mem_base = xml_mem_blocks();
                    let l = gen_xml_list_ptr(n_l, 0);
                    let data = gen_userdata(n_data, 1);

                    let ret_val = xml_list_append(l, data);
                    desret_int(ret_val);
                    des_xml_list_ptr(n_l, l, 0);
                    des_userdata(n_data, data, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlListAppend",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_l);
                        eprintln!(" {}", n_data);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListAppend()");
        }
    }

    #[test]
    fn test_xml_list_clear() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                let mem_base = xml_mem_blocks();
                let l = gen_xml_list_ptr(n_l, 0);

                xml_list_clear(l);
                des_xml_list_ptr(n_l, l, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlListClear",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_l);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListClear()");
        }
    }

    #[test]
    fn test_xml_list_copy() {
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_LIST_PTR {
                for n_old in 0..GEN_NB_CONST_XML_LIST_PTR {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_list_ptr(n_cur, 0);
                    let old = gen_const_xml_list_ptr(n_old, 1);

                    let ret_val = xml_list_copy(cur, old as *mut XmlList);
                    desret_int(ret_val);
                    des_xml_list_ptr(n_cur, cur, 0);
                    des_const_xml_list_ptr(n_old, old as *mut XmlList, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlListCopy",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_old);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found int xmlListCopy()");
        }
    }

    #[test]
    fn test_xml_list_create() {

        /* missing type support */
    }

    #[test]
    fn test_xml_list_dup() {

        /* missing type support */
    }

    #[test]
    fn test_xml_list_empty() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                let mem_base = xml_mem_blocks();
                let l = gen_xml_list_ptr(n_l, 0);

                let ret_val = xml_list_empty(l);
                desret_int(ret_val);
                des_xml_list_ptr(n_l, l, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlListEmpty",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_l);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListEmpty()");
        }
    }

    #[test]
    fn test_xml_list_end() {

        /* missing type support */
    }

    #[test]
    fn test_xml_list_front() {

        /* missing type support */
    }

    #[test]
    fn test_xml_list_insert() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                for n_data in 0..GEN_NB_USERDATA {
                    let mem_base = xml_mem_blocks();
                    let l = gen_xml_list_ptr(n_l, 0);
                    let data = gen_userdata(n_data, 1);

                    let ret_val = xml_list_insert(l, data);
                    desret_int(ret_val);
                    des_xml_list_ptr(n_l, l, 0);
                    des_userdata(n_data, data, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlListInsert",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_l);
                        eprintln!(" {}", n_data);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListInsert()");
        }
    }

    #[test]
    fn test_xml_list_merge() {
        unsafe {
            let mut leaks = 0;

            for n_l1 in 0..GEN_NB_XML_LIST_PTR {
                for n_l2 in 0..GEN_NB_XML_LIST_PTR {
                    let mem_base = xml_mem_blocks();
                    let l1 = gen_xml_list_ptr(n_l1, 0);
                    let l2 = gen_xml_list_ptr(n_l2, 1);

                    xml_list_merge(l1, l2);
                    des_xml_list_ptr(n_l1, l1, 0);
                    des_xml_list_ptr(n_l2, l2, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlListMerge",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_l1);
                        eprintln!(" {}", n_l2);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListMerge()");
        }
    }

    #[test]
    fn test_xml_list_pop_back() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                let mem_base = xml_mem_blocks();
                let l = gen_xml_list_ptr(n_l, 0);

                xml_list_pop_back(l);
                des_xml_list_ptr(n_l, l, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlListPopBack",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_l);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListPopBack()");
        }
    }

    #[test]
    fn test_xml_list_pop_front() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                let mem_base = xml_mem_blocks();
                let l = gen_xml_list_ptr(n_l, 0);

                xml_list_pop_front(l);
                des_xml_list_ptr(n_l, l, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlListPopFront",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_l);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListPopFront()");
        }
    }

    #[test]
    fn test_xml_list_push_back() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                for n_data in 0..GEN_NB_USERDATA {
                    let mem_base = xml_mem_blocks();
                    let l = gen_xml_list_ptr(n_l, 0);
                    let data = gen_userdata(n_data, 1);

                    let ret_val = xml_list_push_back(l, data);
                    desret_int(ret_val);
                    des_xml_list_ptr(n_l, l, 0);
                    des_userdata(n_data, data, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlListPushBack",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_l);
                        eprintln!(" {}", n_data);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListPushBack()");
        }
    }

    #[test]
    fn test_xml_list_push_front() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                for n_data in 0..GEN_NB_USERDATA {
                    let mem_base = xml_mem_blocks();
                    let l = gen_xml_list_ptr(n_l, 0);
                    let data = gen_userdata(n_data, 1);

                    let ret_val = xml_list_push_front(l, data);
                    desret_int(ret_val);
                    des_xml_list_ptr(n_l, l, 0);
                    des_userdata(n_data, data, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlListPushFront",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_l);
                        eprintln!(" {}", n_data);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListPushFront()");
        }
    }

    #[test]
    fn test_xml_list_remove_all() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                for n_data in 0..GEN_NB_USERDATA {
                    let mem_base = xml_mem_blocks();
                    let l = gen_xml_list_ptr(n_l, 0);
                    let data = gen_userdata(n_data, 1);

                    let ret_val = xml_list_remove_all(l, data);
                    desret_int(ret_val);
                    des_xml_list_ptr(n_l, l, 0);
                    des_userdata(n_data, data, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlListRemoveAll",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_l);
                        eprintln!(" {}", n_data);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListRemoveAll()");
        }
    }

    #[test]
    fn test_xml_list_remove_first() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                for n_data in 0..GEN_NB_USERDATA {
                    let mem_base = xml_mem_blocks();
                    let l = gen_xml_list_ptr(n_l, 0);
                    let data = gen_userdata(n_data, 1);

                    let ret_val = xml_list_remove_first(l, data);
                    desret_int(ret_val);
                    des_xml_list_ptr(n_l, l, 0);
                    des_userdata(n_data, data, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlListRemoveFirst",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_l);
                        eprintln!(" {}", n_data);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlListRemoveFirst()"
            );
        }
    }

    #[test]
    fn test_xml_list_remove_last() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                for n_data in 0..GEN_NB_USERDATA {
                    let mem_base = xml_mem_blocks();
                    let l = gen_xml_list_ptr(n_l, 0);
                    let data = gen_userdata(n_data, 1);

                    let ret_val = xml_list_remove_last(l, data);
                    desret_int(ret_val);
                    des_xml_list_ptr(n_l, l, 0);
                    des_userdata(n_data, data, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlListRemoveLast",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_l);
                        eprintln!(" {}", n_data);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListRemoveLast()");
        }
    }

    #[test]
    fn test_xml_list_reverse() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                let mem_base = xml_mem_blocks();
                let l = gen_xml_list_ptr(n_l, 0);

                xml_list_reverse(l);
                des_xml_list_ptr(n_l, l, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlListReverse",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_l);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListReverse()");
        }
    }

    #[test]
    fn test_xml_list_reverse_search() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                for n_data in 0..GEN_NB_USERDATA {
                    let mem_base = xml_mem_blocks();
                    let l = gen_xml_list_ptr(n_l, 0);
                    let data = gen_userdata(n_data, 1);

                    let ret_val = xml_list_reverse_search(l, data);
                    desret_void_ptr(ret_val);
                    des_xml_list_ptr(n_l, l, 0);
                    des_userdata(n_data, data, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlListReverseSearch",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_l);
                        eprintln!(" {}", n_data);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlListReverseSearch()"
            );
        }
    }

    #[test]
    fn test_xml_list_reverse_walk() {

        /* missing type support */
    }

    #[test]
    fn test_xml_list_search() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                for n_data in 0..GEN_NB_USERDATA {
                    let mem_base = xml_mem_blocks();
                    let l = gen_xml_list_ptr(n_l, 0);
                    let data = gen_userdata(n_data, 1);

                    let ret_val = xml_list_search(l, data);
                    desret_void_ptr(ret_val);
                    des_xml_list_ptr(n_l, l, 0);
                    des_userdata(n_data, data, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlListSearch",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_l);
                        eprintln!(" {}", n_data);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListSearch()");
        }
    }

    #[test]
    fn test_xml_list_size() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                let mem_base = xml_mem_blocks();
                let l = gen_xml_list_ptr(n_l, 0);

                let ret_val = xml_list_size(l);
                desret_int(ret_val);
                des_xml_list_ptr(n_l, l, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlListSize",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_l);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListSize()");
        }
    }

    #[test]
    fn test_xml_list_sort() {
        unsafe {
            let mut leaks = 0;

            for n_l in 0..GEN_NB_XML_LIST_PTR {
                let mem_base = xml_mem_blocks();
                let l = gen_xml_list_ptr(n_l, 0);

                xml_list_sort(l);
                des_xml_list_ptr(n_l, l, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlListSort",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_l);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlListSort()");
        }
    }

    #[test]
    fn test_xml_list_walk() {

        /* missing type support */
    }
}

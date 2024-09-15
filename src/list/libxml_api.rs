use std::{cmp::Ordering, ffi::c_void, ops::Deref, ptr::null_mut, rc::Rc};

use crate::list::{Comparator, Deallocator};

pub use super::XmlList;
use super::{XmlLink, XmlLinkRef, XmlListRef};

// In original libxml2, `XmlListDeallocator` is defined as `void (*xmlListDeallocator) (xmlLinkPtr lk)`.
// However, `XmlLinkPtr` is released at the Rust interface so that only data can be passed.
pub type XmlListDeallocator = extern "C" fn(*mut c_void);
pub type XmlListDataCompare = extern "C" fn(*const c_void, *const c_void) -> i32;
pub type XmlListWalker = extern "C" fn(data: *const c_void, user: *mut c_void) -> i32;

pub type XmlListPtr = *mut XmlList<*mut c_void>;
pub(crate) type XmlLinkPtr = *mut XmlLink<*mut c_void>;

pub extern "C" fn xml_list_create(
    deallocator: Option<XmlListDeallocator>,
    compare: Option<XmlListDataCompare>,
) -> XmlListPtr {
    fn make_deallocator(d: impl Fn(*mut c_void) + 'static) -> Deallocator<*mut c_void> {
        Rc::new(d)
    }
    let deallocator: Option<Rc<dyn Fn(*mut c_void)>> = deallocator.map(|d| {
        let f = move |value: *mut c_void| {
            d(value);
        };
        make_deallocator(f)
    });

    fn make_comparator(
        c: impl Fn(&*mut c_void, &*mut c_void) -> Ordering + 'static,
    ) -> Comparator<*mut c_void> {
        Rc::new(c)
    }
    let comparator = compare.map(|c| {
        let f = move |l: &*mut c_void, r: &*mut c_void| -> Ordering { c(*l, *r).cmp(&0) };
        make_comparator(f)
    });
    let list = XmlList::new(deallocator, comparator);
    XmlListRef::from_list(list).map_or(null_mut(), |r| r.0.as_ptr())
}

pub extern "C" fn xml_list_delete(l: XmlListPtr) {
    let Some(list) = XmlListRef::from_raw(l) else {
        return;
    };
    list.free();
}

pub extern "C" fn xml_list_search(l: XmlListPtr, data: *mut c_void) -> *mut c_void {
    let Some(list) = XmlListRef::from_raw(l) else {
        return null_mut();
    };
    list.search(&data).map_or(null_mut(), |v| *v)
}

pub extern "C" fn xml_list_reverse_search(l: XmlListPtr, data: *mut c_void) -> *mut c_void {
    let Some(list) = XmlListRef::from_raw(l) else {
        return null_mut();
    };
    list.reverse_search(&data).map_or(null_mut(), |v| *v)
}

pub extern "C" fn xml_list_insert(l: XmlListPtr, data: *mut c_void) -> i32 {
    let Some(mut list) = XmlListRef::from_raw(l) else {
        return 1;
    };

    list.insert_lower_bound(data);
    0
}

pub extern "C" fn xml_list_append(l: XmlListPtr, data: *mut c_void) -> i32 {
    let Some(mut list) = XmlListRef::from_raw(l) else {
        return 1;
    };

    list.insert_upper_bound(data);
    0
}

pub extern "C" fn xml_list_remove_first(l: XmlListPtr, data: *mut c_void) -> i32 {
    let Some(mut list) = XmlListRef::from_raw(l) else {
        return 0;
    };

    list.remove_first(&data).is_some() as i32
}

pub extern "C" fn xml_list_remove_last(l: XmlListPtr, data: *mut c_void) -> i32 {
    let Some(mut list) = XmlListRef::from_raw(l) else {
        return 0;
    };

    list.remove_last(&data).is_some() as i32
}

pub extern "C" fn xml_list_remove_all(l: XmlListPtr, data: *mut c_void) -> i32 {
    let Some(mut list) = XmlListRef::from_raw(l) else {
        return 0;
    };

    list.remove_all(&data).count() as i32
}

pub extern "C" fn xml_list_clear(l: XmlListPtr) {
    let Some(mut list) = XmlListRef::from_raw(l) else {
        return;
    };

    list.clear();
}

pub extern "C" fn xml_list_empty(l: XmlListPtr) -> i32 {
    let Some(list) = XmlListRef::from_raw(l) else {
        return -1;
    };

    list.is_empty() as i32
}

/// # Safety
/// - For consistency, DO NOT modify already connected links.
/// - If links are modified in a way other than through the public API, the behavior is not defined.
pub(crate) unsafe extern "C" fn xml_list_front(l: XmlListPtr) -> XmlLinkPtr {
    let Some(list) = XmlListRef::from_raw(l) else {
        return null_mut();
    };

    list.first().map_or(null_mut(), |link| link.0.as_ptr())
}

/// # Safety
/// - For consistency, DO NOT modify already connected links.
/// - If links are modified in a way other than through the public API, the behavior is not defined.
pub(crate) unsafe extern "C" fn xml_list_end(l: XmlListPtr) -> XmlLinkPtr {
    let Some(list) = XmlListRef::from_raw(l) else {
        return null_mut();
    };

    list.last().map_or(null_mut(), |link| link.0.as_ptr())
}

pub extern "C" fn xml_list_size(l: XmlListPtr) -> i32 {
    let Some(list) = XmlListRef::from_raw(l) else {
        return -1;
    };

    list.len() as i32
}

pub extern "C" fn xml_list_pop_front(l: XmlListPtr) {
    let Some(mut list) = XmlListRef::from_raw(l) else {
        return;
    };
    list.pop_first();
}

pub extern "C" fn xml_list_pop_back(l: XmlListPtr) {
    let Some(mut list) = XmlListRef::from_raw(l) else {
        return;
    };
    list.pop_last();
}

pub extern "C" fn xml_list_push_front(l: XmlListPtr, data: *mut c_void) -> i32 {
    let Some(mut list) = XmlListRef::from_raw(l) else {
        return 0;
    };

    list.push_first(data);
    1
}

pub extern "C" fn xml_list_push_back(l: XmlListPtr, data: *mut c_void) -> i32 {
    let Some(mut list) = XmlListRef::from_raw(l) else {
        return 0;
    };

    list.push_last(data);
    1
}

pub extern "C" fn xml_list_reverse(l: XmlListPtr) {
    let Some(mut list) = XmlListRef::from_raw(l) else {
        return;
    };
    list.reverse();
}

pub extern "C" fn xml_list_sort(l: XmlListPtr) {
    let Some(mut list) = XmlListRef::from_raw(l) else {
        return;
    };

    list.sort();
}

pub extern "C" fn xml_list_walk(l: XmlListPtr, walker: Option<XmlListWalker>, user: *mut c_void) {
    let Some(walker) = walker else {
        return;
    };

    let Some(list) = XmlListRef::from_raw(l) else {
        return;
    };

    let walker = move |data: &*mut c_void| -> bool { walker(*data as *const c_void, user) != 0 };

    list.walk(walker);
}

pub extern "C" fn xml_list_reverse_walk(
    l: XmlListPtr,
    walker: Option<XmlListWalker>,
    user: *mut c_void,
) {
    let Some(walker) = walker else {
        return;
    };

    let Some(list) = XmlListRef::from_raw(l) else {
        return;
    };

    let walker = move |data: &*mut c_void| -> bool { walker(*data as *const c_void, user) != 0 };

    list.reverse_walk(walker);
}

pub extern "C" fn xml_list_merge(l1: XmlListPtr, l2: XmlListPtr) {
    let (Some(mut l1), Some(mut l2)) = (XmlListRef::from_raw(l1), XmlListRef::from_raw(l2)) else {
        return;
    };

    // is this correct ?
    // I think this method do double-free to data...
    let cloned = XmlList::clone(l2.deref());
    l1.extend(cloned);
    l2.clear();
}

pub extern "C" fn xml_list_dup(old: XmlListPtr) -> XmlListPtr {
    let Some(list) = XmlListRef::from_raw(old) else {
        return null_mut();
    };

    let cloned = XmlList::clone(list.deref());
    XmlListRef::from_list(cloned).map_or(null_mut(), |list| list.0.as_ptr())
}

pub extern "C" fn xml_list_copy(cur: XmlListPtr, old: XmlListPtr) -> i32 {
    let (Some(mut cur), Some(mut old)) = (XmlListRef::from_raw(cur), XmlListRef::from_raw(old))
    else {
        return 1;
    };

    while let Some(data) = old.pop_first() {
        cur.push_last(data);
    }
    cur.sort();
    0
}

pub extern "C" fn xml_link_get_data(lk: XmlLinkPtr) -> *mut c_void {
    XmlLinkRef::from_raw(lk).map_or(null_mut(), |link| link.data)
}

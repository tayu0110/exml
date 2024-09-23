use std::{
    borrow::Cow,
    ffi::{c_void, CStr},
    ptr::{null, null_mut},
};

use crate::{
    dict::{XmlDictPtr, XmlDictRef},
    libxml::{globals::xml_free, xmlstring::XmlChar},
};

pub use super::XmlHashTable;
use super::XmlHashTableRef;

/// A wrapper of `*mut std::ffi::c_void`.
///
/// `XmlHashTable<T>` requires `T` implements `Default`, but `*mut std::ffi::c_void` does not.  
/// `CVoidWrapper` provides `Default` implementation that returns `null_mut()` for `*mut std::ffi::c_void`.
#[derive(Debug, Clone, Copy)]
pub struct CVoidWrapper(pub *mut c_void);

impl Default for CVoidWrapper {
    fn default() -> Self {
        Self(null_mut())
    }
}

pub type XmlHashTablePtr = *mut XmlHashTable<'static, CVoidWrapper>;

pub type XmlHashDeallocator = extern "C" fn(payload: *mut c_void, name: *const XmlChar);
pub type XmlHashCopier = extern "C" fn(payload: *mut c_void, name: *const XmlChar) -> *mut c_void;
pub type XmlHashScanner =
    extern "C" fn(payload: *mut c_void, data: *mut c_void, name: *const XmlChar);
pub type XmlHashScannerFull = extern "C" fn(
    payload: *mut c_void,
    data: *mut c_void,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
);

pub extern "C" fn xml_hash_create(mut size: i32) -> XmlHashTablePtr {
    if size <= 0 {
        size = 256;
    }
    XmlHashTableRef::<'static, CVoidWrapper>::with_capacity(size as usize)
        .map(|table| table.as_ptr())
        .unwrap_or(null_mut())
}

pub extern "C" fn xml_hash_create_dict(mut size: i32, dict: XmlDictPtr) -> XmlHashTablePtr {
    if size <= 0 {
        size = 256;
    }
    let dict = XmlDictRef::from_raw(dict);
    let table = XmlHashTable::with_capacity_dict(size as usize, dict);
    XmlHashTableRef::from_table(table)
        .map(|table| table.as_ptr())
        .unwrap_or(null_mut())
}

pub extern "C" fn xml_hash_free(table: XmlHashTablePtr, f: Option<XmlHashDeallocator>) {
    if let Some(table) = XmlHashTableRef::from_raw(table) {
        let mut table = table.into_inner();
        table.clear_with(move |payload, name| {
            if let Some(f) = f {
                f(
                    payload.0,
                    name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                );
            }
        });
    }
}

/// # Safety
/// - This method is equivalent to `xml_free(entry)`, so it has the same constraint of `xml_free`.
pub extern "C" fn xml_hash_default_deallocator(entry: *mut c_void, _name: *const XmlChar) {
    unsafe {
        xml_free(entry as _);
    }
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_add_entry(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    userdata: *mut c_void,
) -> i32 {
    xml_hash_add_entry3(table, name, null(), null(), userdata)
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_add_entry2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    userdata: *mut c_void,
) -> i32 {
    xml_hash_add_entry3(table, name, name2, null(), userdata)
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_add_entry3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
    userdata: *mut c_void,
) -> i32 {
    if name.is_null() {
        return -1;
    }
    let Some(mut table) = XmlHashTableRef::from_raw(table) else {
        return -1;
    };

    match table.do_add_entry(
        Some(CStr::from_ptr(name as *const i8)),
        (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
        (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
        CVoidWrapper(userdata),
    ) {
        Ok(_) => 0,
        Err(_) => -1,
    }
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_update_entry(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    userdata: *mut c_void,
    f: Option<XmlHashDeallocator>,
) -> i32 {
    xml_hash_update_entry3(table, name, null(), null(), userdata, f)
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_update_entry2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    userdata: *mut c_void,
    f: Option<XmlHashDeallocator>,
) -> i32 {
    xml_hash_update_entry3(table, name, name2, null(), userdata, f)
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_update_entry3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
    userdata: *mut c_void,
    f: Option<XmlHashDeallocator>,
) -> i32 {
    if name.is_null() {
        return -1;
    }
    let Some(mut table) = XmlHashTableRef::from_raw(table) else {
        return -1;
    };

    match table.do_update_entry(
        CStr::from_ptr(name as *const i8),
        (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
        (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
        CVoidWrapper(userdata),
        move |payload: CVoidWrapper, name: Option<Cow<'_, CStr>>| {
            if let Some(deallocator) = f {
                deallocator(
                    payload.0,
                    name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                );
            }
        },
    ) {
        Ok(_) => 0,
        Err(_) => -1,
    }
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_remove_entry(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    f: Option<XmlHashDeallocator>,
) -> i32 {
    xml_hash_remove_entry3(table, name, null(), null(), f)
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_remove_entry2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    f: Option<XmlHashDeallocator>,
) -> i32 {
    xml_hash_remove_entry3(table, name, name2, null(), f)
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_remove_entry3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
    f: Option<XmlHashDeallocator>,
) -> i32 {
    if name.is_null() {
        return -1;
    }
    let Some(mut table) = XmlHashTableRef::from_raw(table) else {
        return -1;
    };

    match table.do_remove_entry(
        CStr::from_ptr(name as *const i8),
        (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
        (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
        move |payload: CVoidWrapper, name: Option<Cow<'_, CStr>>| {
            if let Some(deallocator) = f {
                deallocator(
                    payload.0,
                    name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                );
            }
        },
    ) {
        Ok(_) => 0,
        Err(_) => -1,
    }
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_lookup(
    table: XmlHashTablePtr,
    name: *const XmlChar,
) -> *mut c_void {
    xml_hash_lookup3(table, name, null(), null())
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_lookup2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
) -> *mut c_void {
    xml_hash_lookup3(table, name, name2, null())
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_lookup3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
) -> *mut c_void {
    if name.is_null() {
        return null_mut();
    }
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return null_mut();
    };

    table
        .do_lookup(
            CStr::from_ptr(name as *const i8),
            (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
            (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
        )
        .map(|ptr| ptr.0)
        .unwrap_or(null_mut())
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_qlookup(
    table: XmlHashTablePtr,
    prefix: *const XmlChar,
    name: *const XmlChar,
) -> *mut c_void {
    xml_hash_qlookup3(table, prefix, name, null(), null(), null(), null())
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_qlookup2(
    table: XmlHashTablePtr,
    prefix: *const XmlChar,
    name: *const XmlChar,
    prefix2: *const XmlChar,
    name2: *const XmlChar,
) -> *mut c_void {
    xml_hash_qlookup3(table, prefix, name, prefix2, name2, null(), null())
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_qlookup3(
    table: XmlHashTablePtr,
    prefix: *const XmlChar,
    name: *const XmlChar,
    prefix2: *const XmlChar,
    name2: *const XmlChar,
    prefix3: *const XmlChar,
    name3: *const XmlChar,
) -> *mut c_void {
    if name.is_null() {
        return null_mut();
    }
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return null_mut();
    };

    table
        .do_qlookup(
            (!prefix.is_null()).then(|| CStr::from_ptr(prefix as *const i8)),
            CStr::from_ptr(name as *const i8),
            (!prefix2.is_null()).then(|| CStr::from_ptr(prefix2 as *const i8)),
            (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
            (!prefix3.is_null()).then(|| CStr::from_ptr(prefix3 as *const i8)),
            (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
        )
        .map(|ptr| ptr.0)
        .unwrap_or(null_mut())
}

pub extern "C" fn xml_hash_copy(
    table: XmlHashTablePtr,
    f: Option<XmlHashCopier>,
) -> XmlHashTablePtr {
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return null_mut();
    };

    let Some(copier) = f else {
        return null_mut();
    };

    let cloned = table.clone_with(|payload, name| {
        CVoidWrapper(copier(
            payload.0,
            name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
        ))
    });
    XmlHashTableRef::from_table(cloned)
        .map(|table| table.as_ptr())
        .unwrap_or(null_mut())
}

pub extern "C" fn xml_hash_size(table: XmlHashTablePtr) -> i32 {
    XmlHashTableRef::from_raw(table)
        .map(|table| table.len() as i32)
        .unwrap_or(-1)
}

/// # Safety
/// - The behavior of `f` when it makes changes to `table` is <strong>undefined</strong>.
pub unsafe extern "C" fn xml_hash_scan(
    table: XmlHashTablePtr,
    f: Option<XmlHashScanner>,
    data: *mut c_void,
) {
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return;
    };

    let Some(scanner) = f else {
        return;
    };

    table.scan(move |payload, name, _, _| {
        if !payload.0.is_null() {
            scanner(
                payload.0,
                data,
                name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
            );
        }
    });
}

/// # Safety
/// - The behavior of `f` when it makes changes to `table` is <strong>undefined</strong>.  
/// - All of `name`, `name2` and `name3` must be valid NULL terminated strings.
pub unsafe extern "C" fn xml_hash_scan3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
    f: Option<XmlHashScanner>,
    data: *mut c_void,
) {
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return;
    };

    let Some(scanner) = f else {
        return;
    };

    let (name, name2, name3) = (
        (!name.is_null()).then(|| CStr::from_ptr(name as *const i8)),
        (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
        (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
    );

    table.scan(move |payload, n, n2, n3| {
        if (name.is_none() || n.map(|n| n.as_ref()) == name)
            && (name2.is_none() || n2.map(|n| n.as_ref()) == name2)
            && (name3.is_none() || n3.map(|n| n.as_ref()) == name3)
            && !payload.0.is_null()
        {
            scanner(
                payload.0,
                data,
                n.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
            );
        }
    });
}

/// # Safety
/// - The behavior of `f` when it makes changes to `table` is <strong>undefined</strong>.
pub unsafe extern "C" fn xml_hash_scan_full(
    table: XmlHashTablePtr,
    f: Option<XmlHashScannerFull>,
    data: *mut c_void,
) {
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return;
    };

    let Some(scanner) = f else {
        return;
    };

    table.scan(move |payload, name, name2, name3| {
        if !payload.0.is_null() {
            scanner(
                payload.0,
                data,
                name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                name2.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                name3.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
            );
        }
    });
}

/// # Safety
/// - The behavior of `f` when it makes changes to `table` is <strong>undefined</strong>.  
/// - All of `name`, `name2` and `name3` must be valid NULL terminated strings.
pub unsafe extern "C" fn xml_hash_scan_full3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
    f: Option<XmlHashScannerFull>,
    data: *mut c_void,
) {
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return;
    };

    let Some(scanner) = f else {
        return;
    };

    let (name, name2, name3) = (
        (!name.is_null()).then(|| CStr::from_ptr(name as *const i8)),
        (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
        (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
    );

    table.scan(move |payload, n, n2, n3| {
        if (name.is_none() || n.map(|n| n.as_ref()) == name)
            && (name2.is_none() || n2.map(|n| n.as_ref()) == name2)
            && (name3.is_none() || n3.map(|n| n.as_ref()) == name3)
            && !payload.0.is_null()
        {
            scanner(
                payload.0,
                data,
                n.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                n2.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                n3.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
            );
        }
    });
}

#[cfg(test)]
mod tests {
    use std::{ffi::CString, num::NonZeroU8};

    use rand::{
        distributions::Alphanumeric, prelude::Distribution, seq::SliceRandom, thread_rng, Rng,
    };

    use super::*;

    const LEN: usize = 30;

    fn gen_chars(rng: &mut impl Rng) -> Vec<NonZeroU8> {
        Alphanumeric
            .sample_iter(rng)
            .map(|c| NonZeroU8::new(c).unwrap())
            .take(LEN)
            .collect::<Vec<_>>()
    }

    fn gen_ncname(rng: &mut impl Rng) -> CString {
        CString::from(gen_chars(rng))
    }

    fn gen_qname(rng: &mut impl Rng) -> CString {
        let mut chars = gen_chars(rng);
        chars[rng.gen_range(1..29)] = NonZeroU8::new(b':').unwrap();
        CString::from(chars)
    }

    unsafe fn ncname_insertion_test(
        table: XmlHashTablePtr,
        entries: &mut Vec<(CString, Option<CString>, Option<CString>, u64)>,
        rng: &mut impl Rng,
    ) {
        match rng.gen_range(1..=3) {
            1 => {
                let n1 = gen_ncname(rng);
                let data = rng.gen::<u64>();
                xml_hash_add_entry(table, n1.as_ptr() as _, data as _);
                assert_eq!(xml_hash_lookup(table, n1.as_ptr() as _) as u64, data);
                entries.push((n1, None, None, data));
            }
            2 => {
                let n1 = gen_ncname(rng);
                let n2 = gen_ncname(rng);
                let data = rng.gen::<u64>();
                xml_hash_add_entry2(table, n1.as_ptr() as _, n2.as_ptr() as _, data as _);
                assert_eq!(
                    xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _) as u64,
                    data
                );
                entries.push((n1, Some(n2), None, data));
            }
            3 => {
                let n1 = gen_ncname(rng);
                let n2 = gen_ncname(rng);
                let n3 = gen_ncname(rng);
                let data = rng.gen::<u64>();
                xml_hash_add_entry3(
                    table,
                    n1.as_ptr() as _,
                    n2.as_ptr() as _,
                    n3.as_ptr() as _,
                    data as _,
                );
                assert_eq!(
                    xml_hash_lookup3(table, n1.as_ptr() as _, n2.as_ptr() as _, n3.as_ptr() as _)
                        as u64,
                    data
                );
                entries.push((n1, Some(n2), Some(n3), data));
            }
            _ => {}
        }
    }

    fn split_qname(qname: &CStr) -> Option<(CString, CString)> {
        let qname = qname.to_string_lossy();
        let (prefix, local_name) = qname.split_once(':')?;
        Some((
            CString::new(prefix).unwrap(),
            CString::new(local_name).unwrap(),
        ))
    }

    unsafe fn qname_insertion_test(
        table: XmlHashTablePtr,
        entries: &mut Vec<(CString, Option<CString>, Option<CString>, u64)>,
        rng: &mut impl Rng,
    ) {
        match rng.gen_range(1..=3) {
            1 => {
                let n1 = gen_qname(rng);
                let data = rng.gen::<u64>();
                xml_hash_add_entry(table, n1.as_ptr() as _, data as _);
                assert_eq!(xml_hash_lookup(table, n1.as_ptr() as _) as u64, data);
                let (p1, l1) = split_qname(&n1).unwrap();
                assert_eq!(
                    xml_hash_qlookup(table, p1.as_ptr() as _, l1.as_ptr() as _,) as u64,
                    data
                );
                entries.push((n1, None, None, data));
            }
            2 => {
                let n1 = gen_qname(rng);
                let n2 = gen_qname(rng);
                let data = rng.gen::<u64>();
                xml_hash_add_entry2(table, n1.as_ptr() as _, n2.as_ptr() as _, data as _);
                assert_eq!(
                    xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _) as u64,
                    data
                );
                let (p1, l1) = split_qname(&n1).unwrap();
                let (p2, l2) = split_qname(&n2).unwrap();
                assert_eq!(
                    xml_hash_qlookup2(
                        table,
                        p1.as_ptr() as _,
                        l1.as_ptr() as _,
                        p2.as_ptr() as _,
                        l2.as_ptr() as _,
                    ) as u64,
                    data
                );
                entries.push((n1, Some(n2), None, data));
            }
            3 => {
                let n1 = gen_qname(rng);
                let n2 = gen_qname(rng);
                let n3 = gen_qname(rng);
                let data = rng.gen::<u64>();
                xml_hash_add_entry3(
                    table,
                    n1.as_ptr() as _,
                    n2.as_ptr() as _,
                    n3.as_ptr() as _,
                    data as _,
                );
                assert_eq!(
                    xml_hash_lookup3(table, n1.as_ptr() as _, n2.as_ptr() as _, n3.as_ptr() as _)
                        as u64,
                    data
                );
                let (p1, l1) = split_qname(&n1).unwrap();
                let (p2, l2) = split_qname(&n2).unwrap();
                let (p3, l3) = split_qname(&n3).unwrap();
                assert_eq!(
                    xml_hash_qlookup3(
                        table,
                        p1.as_ptr() as _,
                        l1.as_ptr() as _,
                        p2.as_ptr() as _,
                        l2.as_ptr() as _,
                        p3.as_ptr() as _,
                        l3.as_ptr() as _,
                    ) as u64,
                    data
                );
                entries.push((n1, Some(n2), Some(n3), data));
            }
            _ => {}
        }
    }

    #[test]
    fn random_test() {
        let table = xml_hash_create(0);
        let mut rng = thread_rng();
        let mut entries = vec![];

        unsafe {
            for _ in 0..10_000 {
                if entries.is_empty() {
                    assert!(xml_hash_size(table) == 0);
                    [ncname_insertion_test, qname_insertion_test]
                        .choose(&mut rng)
                        .unwrap()(table, &mut entries, &mut rng);
                }
                // 0: insert (ncname)
                // 1: insert (qname)
                // 2: remove
                // 3: lookup
                // 4: update
                // 5: clone
                // 6: remove_if
                let query = rng.gen_range(0..=6);
                match query {
                    0 => ncname_insertion_test(table, &mut entries, &mut rng),
                    1 => qname_insertion_test(table, &mut entries, &mut rng),
                    2 => match entries.swap_remove(rng.gen_range(0..entries.len())) {
                        (n1, None, None, data) => {
                            assert_eq!(xml_hash_lookup(table, n1.as_ptr() as _) as u64, data);
                            assert!(xml_hash_remove_entry(table, n1.as_ptr() as _, None) == 0);
                            assert!(xml_hash_lookup(table, n1.as_ptr() as _).is_null());
                        }
                        (n1, Some(n2), None, data) => {
                            assert_eq!(
                                xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _) as u64,
                                data
                            );
                            assert!(
                                xml_hash_remove_entry2(
                                    table,
                                    n1.as_ptr() as _,
                                    n2.as_ptr() as _,
                                    None
                                ) == 0
                            );
                            assert!(xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _)
                                .is_null());
                        }
                        (n1, Some(n2), Some(n3), data) => {
                            assert_eq!(
                                xml_hash_lookup3(
                                    table,
                                    n1.as_ptr() as _,
                                    n2.as_ptr() as _,
                                    n3.as_ptr() as _
                                ) as u64,
                                data
                            );
                            assert!(
                                xml_hash_remove_entry3(
                                    table,
                                    n1.as_ptr() as _,
                                    n2.as_ptr() as _,
                                    n3.as_ptr() as _,
                                    None
                                ) == 0
                            );
                            assert!(xml_hash_lookup3(
                                table,
                                n1.as_ptr() as _,
                                n2.as_ptr() as _,
                                n3.as_ptr() as _
                            )
                            .is_null());
                        }
                        _ => [ncname_insertion_test, qname_insertion_test]
                            .choose(&mut rng)
                            .unwrap()(table, &mut entries, &mut rng),
                    },
                    3 => match entries.choose(&mut rng) {
                        Some((n1, None, None, data)) => {
                            assert_eq!(xml_hash_lookup(table, n1.as_ptr() as _) as u64, *data);
                            match split_qname(n1) {
                                Some((p1, l1)) => {
                                    assert_eq!(
                                        xml_hash_qlookup(table, p1.as_ptr() as _, l1.as_ptr() as _,)
                                            as u64,
                                        *data
                                    );
                                }
                                _ => assert_eq!(
                                    xml_hash_qlookup(table, null(), n1.as_ptr() as _,) as u64,
                                    *data
                                ),
                            }
                        }
                        Some((n1, Some(n2), None, data)) => {
                            assert_eq!(
                                xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _) as u64,
                                *data
                            );
                            match (split_qname(n1), split_qname(n2)) {
                                (Some((p1, l1)), Some((p2, l2))) => {
                                    assert_eq!(
                                        xml_hash_qlookup2(
                                            table,
                                            p1.as_ptr() as _,
                                            l1.as_ptr() as _,
                                            p2.as_ptr() as _,
                                            l2.as_ptr() as _,
                                        ) as u64,
                                        *data
                                    )
                                }
                                _ => assert_eq!(
                                    xml_hash_qlookup2(
                                        table,
                                        null(),
                                        n1.as_ptr() as _,
                                        null(),
                                        n2.as_ptr() as _,
                                    ) as u64,
                                    *data
                                ),
                            }
                        }
                        Some((n1, Some(n2), Some(n3), data)) => {
                            assert_eq!(
                                xml_hash_lookup3(
                                    table,
                                    n1.as_ptr() as _,
                                    n2.as_ptr() as _,
                                    n3.as_ptr() as _
                                ) as u64,
                                *data
                            );
                            match (split_qname(n1), split_qname(n2), split_qname(n3)) {
                                (Some((p1, l1)), Some((p2, l2)), Some((p3, l3))) => {
                                    assert_eq!(
                                        xml_hash_qlookup3(
                                            table,
                                            p1.as_ptr() as _,
                                            l1.as_ptr() as _,
                                            p2.as_ptr() as _,
                                            l2.as_ptr() as _,
                                            p3.as_ptr() as _,
                                            l3.as_ptr() as _,
                                        ) as u64,
                                        *data
                                    );
                                }
                                _ => {
                                    assert_eq!(
                                        xml_hash_qlookup3(
                                            table,
                                            null(),
                                            n1.as_ptr() as _,
                                            null(),
                                            n2.as_ptr() as _,
                                            null(),
                                            n3.as_ptr() as _,
                                        ) as u64,
                                        *data
                                    )
                                }
                            }
                        }
                        _ => [ncname_insertion_test, qname_insertion_test]
                            .choose(&mut rng)
                            .unwrap()(table, &mut entries, &mut rng),
                    },
                    4 => {
                        let index = rng.gen_range(0..entries.len());
                        let data: u64 = rng.gen();
                        match &entries[index] {
                            (n1, None, None, old) => {
                                assert_eq!(xml_hash_lookup(table, n1.as_ptr() as _) as u64, *old);
                                assert_eq!(
                                    xml_hash_update_entry(table, n1.as_ptr() as _, data as _, None),
                                    0
                                );
                                assert_eq!(xml_hash_lookup(table, n1.as_ptr() as _) as u64, data);
                            }
                            (n1, Some(n2), None, old) => {
                                assert_eq!(
                                    xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _)
                                        as u64,
                                    *old
                                );
                                assert_eq!(
                                    xml_hash_update_entry2(
                                        table,
                                        n1.as_ptr() as _,
                                        n2.as_ptr() as _,
                                        data as _,
                                        None
                                    ),
                                    0
                                );
                                assert_eq!(
                                    xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _)
                                        as u64,
                                    data
                                );
                            }
                            (n1, Some(n2), Some(n3), old) => {
                                assert_eq!(
                                    xml_hash_lookup3(
                                        table,
                                        n1.as_ptr() as _,
                                        n2.as_ptr() as _,
                                        n3.as_ptr() as _
                                    ) as u64,
                                    *old
                                );
                                assert_eq!(
                                    xml_hash_update_entry3(
                                        table,
                                        n1.as_ptr() as _,
                                        n2.as_ptr() as _,
                                        n3.as_ptr() as _,
                                        data as _,
                                        None
                                    ),
                                    0
                                );
                                assert_eq!(
                                    xml_hash_lookup3(
                                        table,
                                        n1.as_ptr() as _,
                                        n2.as_ptr() as _,
                                        n3.as_ptr() as _
                                    ) as u64,
                                    data
                                );
                            }
                            _ => {}
                        }
                        entries[index].3 = data;
                    }
                    5 => {
                        extern "C" fn copier(
                            data: *mut c_void,
                            _name: *const XmlChar,
                        ) -> *mut c_void {
                            data
                        }
                        let cloned = xml_hash_copy(table, Some(copier));
                        assert_eq!(xml_hash_size(cloned), xml_hash_size(table));
                        let mut buf: Vec<(CString, Option<CString>, Option<CString>, u64)> = vec![];

                        extern "C" fn scanner(
                            payload: *mut c_void,
                            data: *mut c_void,
                            name: *const XmlChar,
                            name2: *const XmlChar,
                            name3: *const XmlChar,
                        ) {
                            let buf =
                                data as *mut Vec<(CString, Option<CString>, Option<CString>, u64)>;
                            unsafe {
                                (*buf).push((
                                    CStr::from_ptr(name as _).to_owned(),
                                    (!name2.is_null())
                                        .then(|| CStr::from_ptr(name2 as _).to_owned()),
                                    (!name3.is_null())
                                        .then(|| CStr::from_ptr(name3 as _).to_owned()),
                                    payload as u64,
                                ));
                            }
                        }

                        xml_hash_scan_full(
                            table,
                            Some(scanner),
                            &mut buf as *mut Vec<_> as *mut c_void,
                        );
                        buf.sort_unstable();
                        entries.sort_unstable();
                        assert_eq!(buf, entries);
                    }
                    6 => {
                        let parity = rng.gen_range(0..2);
                        let mut table = XmlHashTableRef::from_raw(table).unwrap();
                        entries.retain(|e| e.3 % 2 == parity);
                        table.remove_if(|data, _, _, _| data.0 as u64 % 2 != parity, |_, _| {});
                    }
                    _ => {}
                }

                assert_eq!(entries.len(), xml_hash_size(table) as usize);
            }
        }
    }
}

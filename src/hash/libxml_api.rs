use std::{
    ffi::{c_void, CStr},
    ptr::{null, null_mut},
};

use crate::{
    dict::{XmlDictPtr, XmlDictRef},
    libxml::{globals::xml_free, xmlstring::XmlChar},
};

use super::{XmlHashTable, XmlHashTableRef};

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
    let Some(mut table) = XmlHashTableRef::from_raw(table) else {
        return -1;
    };

    match table.add_entry(CStr::from_ptr(name as *const i8), CVoidWrapper(userdata)) {
        Ok(_) => 0,
        Err(_) => -1,
    }
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_add_entry2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    userdata: *mut c_void,
) -> i32 {
    let Some(mut table) = XmlHashTableRef::from_raw(table) else {
        return -1;
    };

    match table.add_entry2(
        CStr::from_ptr(name as *const i8),
        CStr::from_ptr(name2 as *const i8),
        CVoidWrapper(userdata),
    ) {
        Ok(_) => 0,
        Err(_) => -1,
    }
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
    let Some(mut table) = XmlHashTableRef::from_raw(table) else {
        return -1;
    };

    match table.add_entry3(
        CStr::from_ptr(name as *const i8),
        CStr::from_ptr(name2 as *const i8),
        CStr::from_ptr(name3 as *const i8),
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
    let Some(mut table) = XmlHashTableRef::from_raw(table) else {
        return -1;
    };

    match table.update_entry(
        CStr::from_ptr(name as *const i8),
        CVoidWrapper(userdata),
        move |payload, name| {
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
pub unsafe extern "C" fn xml_hash_update_entry2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    userdata: *mut c_void,
    f: Option<XmlHashDeallocator>,
) -> i32 {
    let Some(mut table) = XmlHashTableRef::from_raw(table) else {
        return -1;
    };

    match table.update_entry2(
        CStr::from_ptr(name as *const i8),
        CStr::from_ptr(name2 as *const i8),
        CVoidWrapper(userdata),
        move |payload, name| {
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
pub unsafe extern "C" fn xml_hash_update_entry3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
    userdata: *mut c_void,
    f: Option<XmlHashDeallocator>,
) -> i32 {
    let Some(mut table) = XmlHashTableRef::from_raw(table) else {
        return -1;
    };

    match table.update_entry3(
        CStr::from_ptr(name as *const i8),
        CStr::from_ptr(name2 as *const i8),
        CStr::from_ptr(name3 as *const i8),
        CVoidWrapper(userdata),
        move |payload, name| {
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
    let Some(mut table) = XmlHashTableRef::from_raw(table) else {
        return -1;
    };

    match table.remove_entry(CStr::from_ptr(name as *const i8), move |payload, name| {
        if let Some(deallocator) = f {
            deallocator(
                payload.0,
                name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
            );
        }
    }) {
        Ok(_) => 0,
        Err(_) => -1,
    }
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_remove_entry2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    f: Option<XmlHashDeallocator>,
) -> i32 {
    let Some(mut table) = XmlHashTableRef::from_raw(table) else {
        return -1;
    };

    match table.remove_entry2(
        CStr::from_ptr(name as *const i8),
        CStr::from_ptr(name2 as *const i8),
        move |payload, name| {
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
pub unsafe extern "C" fn xml_hash_remove_entry3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
    f: Option<XmlHashDeallocator>,
) -> i32 {
    let Some(mut table) = XmlHashTableRef::from_raw(table) else {
        return -1;
    };

    match table.remove_entry3(
        CStr::from_ptr(name as *const i8),
        CStr::from_ptr(name2 as *const i8),
        CStr::from_ptr(name3 as *const i8),
        move |payload, name| {
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
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return null_mut();
    };

    table
        .lookup(CStr::from_ptr(name as *const i8))
        .map(|ptr| ptr.0)
        .unwrap_or(null_mut())
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_lookup2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
) -> *mut c_void {
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return null_mut();
    };

    table
        .lookup2(
            CStr::from_ptr(name as *const i8),
            CStr::from_ptr(name2 as *const i8),
        )
        .map(|ptr| ptr.0)
        .unwrap_or(null_mut())
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_lookup3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
) -> *mut c_void {
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return null_mut();
    };

    table
        .lookup3(
            CStr::from_ptr(name as *const i8),
            CStr::from_ptr(name2 as *const i8),
            CStr::from_ptr(name3 as *const i8),
        )
        .map(|ptr| ptr.0)
        .unwrap_or(null_mut())
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_qlookup(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    prefix: *const XmlChar,
) -> *mut c_void {
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return null_mut();
    };

    table
        .qlookup(
            CStr::from_ptr(name as *const i8),
            (!prefix.is_null()).then(|| CStr::from_ptr(prefix as *const i8)),
        )
        .map(|ptr| ptr.0)
        .unwrap_or(null_mut())
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_qlookup2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    prefix: *const XmlChar,
    name2: *const XmlChar,
    prefix2: *const XmlChar,
) -> *mut c_void {
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return null_mut();
    };

    table
        .qlookup2(
            CStr::from_ptr(name as *const i8),
            (!prefix.is_null()).then(|| CStr::from_ptr(prefix as *const i8)),
            CStr::from_ptr(name2 as *const i8),
            (!prefix2.is_null()).then(|| CStr::from_ptr(prefix2 as *const i8)),
        )
        .map(|ptr| ptr.0)
        .unwrap_or(null_mut())
}

/// # Safety
/// - `name` must be a valid NULL terminated string.
pub unsafe extern "C" fn xml_hash_qlookup3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    prefix: *const XmlChar,
    name2: *const XmlChar,
    prefix2: *const XmlChar,
    name3: *const XmlChar,
    prefix3: *const XmlChar,
) -> *mut c_void {
    let Some(table) = XmlHashTableRef::from_raw(table) else {
        return null_mut();
    };

    table
        .qlookup3(
            CStr::from_ptr(name as *const i8),
            (!prefix.is_null()).then(|| CStr::from_ptr(prefix as *const i8)),
            CStr::from_ptr(name2 as *const i8),
            (!prefix2.is_null()).then(|| CStr::from_ptr(prefix2 as *const i8)),
            CStr::from_ptr(name3 as *const i8),
            (!prefix3.is_null()).then(|| CStr::from_ptr(prefix3 as *const i8)),
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

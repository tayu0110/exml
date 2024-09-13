//! libxml2 like APIs of `exml::dict`.
//!
//! Since memory is managed by the Rust runtime, allocation and deallocation must be done via these APIs.

use std::{
    ffi::CStr,
    ptr::{null, null_mut},
    slice::from_raw_parts,
};

use crate::libxml::{parser::xml_init_parser, xmlstring::XmlChar};

use super::{XmlDict, XmlDictRef};

/// Alias of `*mut XmlDict`.
pub type XmlDictPtr = *mut XmlDict;

pub extern "C" fn xml_dict_create() -> *mut XmlDict {
    XmlDictRef::new().map_or(null_mut(), |dict| dict.0.as_ptr())
}

pub extern "C" fn xml_dict_set_limit(dict: *mut XmlDict, limit: usize) -> usize {
    let Some(mut dict) = XmlDictRef::from_raw(dict) else {
        return 0;
    };
    dict.set_limit(limit)
}

pub extern "C" fn xml_dict_get_usage(dict: *mut XmlDict) -> usize {
    let Some(dict) = XmlDictRef::from_raw(dict) else {
        return 0;
    };
    dict.get_usage()
}

pub extern "C" fn xml_dict_create_sub(sub: *mut XmlDict) -> *mut XmlDict {
    XmlDictRef::create_sub(XmlDictRef::from_raw(sub)).map_or(null_mut(), |dict| dict.0.as_ptr())
}

pub extern "C" fn xml_dict_reference(dict: *mut XmlDict) -> i32 {
    let Some(mut dict) = XmlDictRef::from_raw(dict) else {
        return -1;
    };
    dict.add_reference();
    0
}

pub extern "C" fn xml_dict_free(dict: *mut XmlDict) {
    if let Some(dict) = XmlDictRef::from_raw(dict) {
        dict.free();
    }
}

/// # Safety
/// - `dict` and `name` can be NULL.
/// - If `len < 0`, the user must guarantee that between `name` and the first NULL character is accessible.
/// - If `len >= 0`, the user must guarantee that between `name` and `name.add(len)` is accessible.
pub unsafe extern "C" fn xml_dict_lookup(
    dict: *mut XmlDict,
    name: *const XmlChar,
    len: i32,
) -> *const XmlChar {
    let Some(mut dict) = XmlDictRef::from_raw(dict) else {
        return null();
    };

    if name.is_null() {
        return null();
    }

    let name = if len < 0 {
        CStr::from_ptr(name as _).to_bytes()
    } else {
        from_raw_parts(name, len as usize)
    };

    dict.lookup(name).map_or(null(), |name| name.as_ptr())
}

/// # Safety
/// - `dict` and `name` can be NULL.
/// - If `len < 0`, the user must guarantee that between `name` and the first NULL character is accessible.
/// - If `len >= 0`, the user must guarantee that between `name` and `name.add(len)` is accessible.
pub unsafe extern "C" fn xml_dict_exists(
    dict: *mut XmlDict,
    name: *const XmlChar,
    len: i32,
) -> *const XmlChar {
    let Some(dict) = XmlDictRef::from_raw(dict) else {
        return null();
    };

    if name.is_null() {
        return null();
    }

    let name = if len < 0 {
        CStr::from_ptr(name as _).to_bytes()
    } else {
        from_raw_parts(name, len as usize)
    };

    dict.exists(name).map_or(null(), |name| name.as_ptr())
}

/// # Safety
/// - `dict`, `prefix` and `name` can be NULL.
/// - If `prefix` is not NULL, `prefix` must be a valid NULL-terminated string.
/// - If `name` is not NULL, `name` must be a valid NULL-terminated string.
pub unsafe extern "C" fn xml_dict_qlookup(
    dict: *mut XmlDict,
    prefix: *const XmlChar,
    name: *const XmlChar,
) -> *const XmlChar {
    let Some(mut dict) = XmlDictRef::from_raw(dict) else {
        return null();
    };

    if name.is_null() {
        return null();
    }

    if prefix.is_null() {
        return dict
            .lookup(CStr::from_ptr(name as _).to_bytes())
            .map_or(null(), |name| name.as_ptr());
    }

    dict.qlookup(
        Some(CStr::from_ptr(prefix as _).to_bytes()),
        CStr::from_ptr(name as _).to_bytes(),
    )
    .map_or(null(), |qname| qname.as_ptr())
}

/// # Safety
/// - `dict` and `str` can be NULL.
/// - `str` must be a valid NULL-terminated string.
pub unsafe extern "C" fn xml_dict_owns(dict: *mut XmlDict, str: *const XmlChar) -> i32 {
    let Some(dict) = XmlDictRef::from_raw(dict) else {
        return -1;
    };
    if str.is_null() {
        return -1;
    }
    unsafe { dict.owns(CStr::from_ptr(str as _)) as i32 }
}

pub extern "C" fn xml_dict_size(dict: *mut XmlDict) -> i32 {
    let Some(dict) = XmlDictRef::from_raw(dict) else {
        return -1;
    };
    dict.len() as i32
}

#[deprecated = "Alias for xml_init_parser."]
pub extern "C" fn xml_initialize_dict() -> i32 {
    unsafe {
        xml_init_parser();
    }
    0
}

#[deprecated = "Alias for rand::random"]
pub(crate) extern "C" fn __xml_random() -> i32 {
    rand::random()
}

#[deprecated = "This method do nothing."]
pub extern "C" fn xml_dict_cleanup() {
    // This method do nothing also in original code.
}

pub(crate) extern "C" fn __xml_initialize_dict() -> i32 {
    // In original code, this method initializes Mutex and random seed.
    // However, since neither is used, this method just only returns 1.
    1
}

pub(crate) extern "C" fn xml_cleanup_dict_internal() {
    // In original code, this method clean up Mutex.
    // However, since Mutex is no longer needed, this method does nothing.
}

#[cfg(test)]
mod tests {
    use rand::{thread_rng, Rng};

    use super::*;

    unsafe fn do_lookup_test(dict: *mut XmlDict, s: *const XmlChar) -> *const XmlChar {
        assert!(xml_dict_exists(dict, s, -1).is_null());
        assert!(xml_dict_owns(dict, s) == 0);
        let stored = xml_dict_lookup(dict, s, -1);

        assert!(!xml_dict_exists(dict, s, -1).is_null());
        assert!(xml_dict_owns(dict, s) == 0);

        assert!(!xml_dict_exists(dict, stored, -1).is_null());
        assert!(xml_dict_owns(dict, stored) != 0);

        let stored2 = xml_dict_lookup(dict, stored, -1);
        assert_eq!(stored, stored2);
        stored
    }

    #[test]
    fn lookup_test() {
        const S: &[&CStr] = &[c"Hello", c"xml:ns", c""];

        let dict = xml_dict_create();
        let mut stored = vec![];
        for &s in S {
            stored.push(unsafe { do_lookup_test(dict, s.as_ptr() as *const XmlChar) });
        }

        let mut rng = thread_rng();
        let mut buf = [0u8; 51];
        for _ in 0..100 {
            buf.fill_with(|| rng.gen_range(0x31..=0x7A));
            buf[50] = 0;
            unsafe {
                do_lookup_test(dict, buf.as_ptr());
            }
        }

        for s in stored {
            unsafe {
                let found = xml_dict_lookup(dict, s, -1);
                assert_eq!(s, found);
                assert!(xml_dict_owns(dict, s) != 0);
            }
        }

        xml_dict_free(dict);
    }
}

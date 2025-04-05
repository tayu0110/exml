//! Rust implementation of original libxml2's `testdict.c`.  
//! If you want this to work, copy the `test/` and `result/` directories from the original libxml2.

use std::{
    ffi::{CStr, c_char, c_int},
    ptr::{addr_of_mut, null, null_mut},
};

use exml::{
    libxml::{
        dict::{
            XmlDictPtr, xml_dict_create, xml_dict_create_sub, xml_dict_free, xml_dict_lookup,
            xml_dict_owns, xml_dict_qlookup,
        },
        globals::xml_free,
        xmlstring::{XmlChar, xml_strdup, xml_strlen, xml_strncat_new},
    },
    parser::xml_cleanup_parser,
};
use libc::memset;

const SEEDS1: &[*const c_char] = &[
    c"a".as_ptr(),
    c"b".as_ptr(),
    c"c".as_ptr(),
    c"d".as_ptr(),
    c"e".as_ptr(),
    c"f".as_ptr(),
    c"g".as_ptr(),
    c"h".as_ptr(),
    c"i".as_ptr(),
    c"j".as_ptr(),
    c"k".as_ptr(),
    c"l".as_ptr(),
];

const SEEDS2: &[*const c_char] = &[
    c"m".as_ptr(),
    c"n".as_ptr(),
    c"o".as_ptr(),
    c"p".as_ptr(),
    c"q".as_ptr(),
    c"r".as_ptr(),
    c"s".as_ptr(),
    c"t".as_ptr(),
    c"u".as_ptr(),
    c"v".as_ptr(),
    c"w".as_ptr(),
    c"x".as_ptr(),
];

const NB_STRINGS_NS: usize = 100;
const NB_STRINGS_MAX: usize = 10000;
const NB_STRINGS_MIN: usize = 10;

unsafe fn fill_strings(strings1: &mut [*mut XmlChar], strings2: &mut [*mut XmlChar]) {
    unsafe {
        /*
         * That's a bit nasty but the output is fine and it doesn't take hours
         * there is a small but sufficient number of duplicates, and we have
         * ":xxx" and full QNames in the last NB_STRINGS_NS values
         */
        let mut ni = 0;
        for (i, &seed) in SEEDS1.iter().enumerate() {
            strings1[i] = xml_strdup(seed as *const XmlChar);
            assert!(
                !strings1[i].is_null(),
                "Out of memory while generating strings1"
            );
            ni += 1;
        }

        let (mut j, mut k) = (0, 0);
        for i in SEEDS1.len()..NB_STRINGS_MAX - NB_STRINGS_NS {
            strings1[i] = xml_strncat_new(strings1[j], strings1[k], -1);
            assert!(
                !strings1[i].is_null(),
                "Out of memory while generating strings1"
            );
            if j >= 50 {
                j = 0;
                k += 1;
            }
            j += 1;
            ni += 1;
        }
        for (i, j) in (NB_STRINGS_MAX - NB_STRINGS_NS..NB_STRINGS_MAX).zip((0..50).step_by(2)) {
            strings1[i] = xml_strncat_new(strings1[j], c":".as_ptr() as *const XmlChar, -1);
            assert!(
                !strings1[i].is_null(),
                "Out of memory while generating strings1"
            );
            ni += 1;
        }

        let mut k = 0;
        for (i, j) in (ni..NB_STRINGS_MAX).zip(NB_STRINGS_MAX - NB_STRINGS_NS..) {
            strings1[i] = xml_strncat_new(strings1[j], strings1[k], -1);
            assert!(
                !strings1[i].is_null(),
                "Out of memory while generating strings1"
            );
            k += 3;
            if k >= 50 {
                k = 0;
            }
        }

        /*
         * Now do the same with the second pool of strings
         */
        ni = 0;
        for (i, &seed) in SEEDS2.iter().enumerate() {
            strings2[i] = xml_strdup(seed as *const XmlChar);
            assert!(
                !strings2[i].is_null(),
                "Out of memory while generating strings2"
            );
            ni += 1;
        }
        let (mut j, mut k) = (0, 0);
        for i in SEEDS2.len()..NB_STRINGS_MAX - NB_STRINGS_NS {
            strings2[i] = xml_strncat_new(strings2[j], strings2[k], -1);
            assert!(
                !strings2[i].is_null(),
                "Out of memory while generating strings2"
            );
            if j >= 50 {
                j = 0;
                k += 1;
            }
            j += 1;
            ni += 1;
        }
        for (i, j) in (NB_STRINGS_MAX - NB_STRINGS_NS..NB_STRINGS_MAX).zip((0..50).step_by(2)) {
            strings2[i] = xml_strncat_new(strings2[j], c":".as_ptr() as *const XmlChar, -1);
            assert!(
                !strings2[i].is_null(),
                "Out of memory while generating strings2"
            );
            ni += 1;
        }
        k = 0;
        for (i, j) in (ni..NB_STRINGS_MAX).zip(NB_STRINGS_MAX - NB_STRINGS_NS..) {
            strings2[i] = xml_strncat_new(strings2[j], strings2[k], -1);
            assert!(
                !strings2[i].is_null(),
                "Out of memory while generating strings2"
            );
            k += 3;
            if k >= 50 {
                k = 0;
            }
        }
    }
}

unsafe fn clean_strings(strings1: &mut [*mut XmlChar], strings2: &mut [*mut XmlChar]) {
    unsafe {
        for string in strings1.iter_mut().take(NB_STRINGS_MAX) {
            /* really should not happen */
            if !string.is_null() {
                xml_free(*string as _);
            }
        }
        for string in strings2.iter_mut().take(NB_STRINGS_MAX) {
            /* really should not happen */
            if !string.is_null() {
                xml_free(*string as _);
            }
        }
    }
}

/*
 * This tests the sub-dictionary support
 */
unsafe fn run_test2(
    parent: XmlDictPtr,
    strings1: &mut [*mut XmlChar],
    strings2: &mut [*mut XmlChar],
    test1: &mut [*const XmlChar],
    test2: &mut [*const XmlChar],
) -> c_int {
    unsafe {
        let mut prefix: [XmlChar; 40] = [0; 40];
        let mut cur: *mut XmlChar;
        let mut pref: *mut XmlChar;
        let mut tmp: *const XmlChar;

        let dict: XmlDictPtr = xml_dict_create_sub(parent);
        assert!(
            !dict.is_null(),
            "Out of memory while creating sub-dictionary"
        );

        /* Cast to avoid buggy warning on MSVC. */
        memset(test2.as_mut_ptr() as _, 0, test2.len());

        /*
         * Fill in NB_STRINGS_MIN, at this point the dictionary should not grow
         * and we allocate all those doing the fast key computations
         * All the strings are based on a different seeds subset so we know
         * they are allocated in the main dictionary, not coming from the parent
         */
        for i in 0..NB_STRINGS_MIN {
            test2[i] = xml_dict_lookup(dict, strings2[i], -1);
            assert!(
                !test2[i].is_null(),
                "Failed lookup for '{}'\n",
                CStr::from_ptr(strings2[i] as _).to_string_lossy()
            );
        }
        /* ":foo" like strings2 */
        for (_, j) in (0..NB_STRINGS_MIN).zip(NB_STRINGS_MAX - NB_STRINGS_NS..) {
            test2[j] = xml_dict_lookup(dict, strings2[j], xml_strlen(strings2[j]));
            assert!(
                !test2[j].is_null(),
                "Failed lookup for '{}'",
                CStr::from_ptr(strings2[j] as _).to_string_lossy()
            );
        }
        /* "a:foo" like strings2 */
        for (_, j) in (0..NB_STRINGS_MIN).zip(NB_STRINGS_MAX - NB_STRINGS_MIN..) {
            test2[j] = xml_dict_lookup(dict, strings2[j], xml_strlen(strings2[j]));
            assert!(
                !test2[j].is_null(),
                "Failed lookup for '{}'",
                CStr::from_ptr(strings2[j] as _).to_string_lossy()
            );
        }

        /*
         * At this point allocate all the strings
         * the dictionary will grow in the process, reallocate more string tables
         * and switch to the better key generator
         */
        for i in 0..NB_STRINGS_MAX {
            if !test2[i].is_null() {
                continue;
            }
            test2[i] = xml_dict_lookup(dict, strings2[i], -1);
            assert!(
                !test2[i].is_null(),
                "Failed lookup for '{}'",
                CStr::from_ptr(strings2[i] as _).to_string_lossy()
            );
        }

        /*
         * Now we can start to test things, first that all strings2 belongs to
         * the dict, and that none of them was actually allocated in the parent
         */
        for i in 0..NB_STRINGS_MAX {
            assert!(
                xml_dict_owns(dict, test2[i]) != 0,
                "Failed ownership failure for '{}'",
                CStr::from_ptr(strings2[i] as _).to_string_lossy()
            );
            assert!(
                xml_dict_owns(parent, test2[i]) == 0,
                "Failed parent ownership failure for '{}'",
                CStr::from_ptr(strings2[i] as _).to_string_lossy(),
            );
        }

        /*
         * Also verify that all strings from the parent are seen from the subdict
         */
        for i in 0..NB_STRINGS_MAX {
            assert!(
                xml_dict_owns(dict, test1[i]) != 0,
                "Failed sub-ownership failure for '{}'",
                CStr::from_ptr(strings1[i] as _).to_string_lossy(),
            );
        }

        /*
         * Then that another lookup to the string in sub will return the same
         */
        for i in 0..NB_STRINGS_MAX {
            assert!(
                xml_dict_lookup(dict, strings2[i], -1) == test2[i],
                "Failed re-lookup check for {i}, '{}'",
                CStr::from_ptr(strings2[i] as _).to_string_lossy(),
            );
        }
        /*
         * But also that any lookup for a string in the parent will be provided
         * as in the parent
         */
        for i in 0..NB_STRINGS_MAX {
            assert!(
                xml_dict_lookup(dict, strings1[i], -1) == test1[i],
                "Failed parent string lookup check for {i}, '{}'",
                CStr::from_ptr(strings1[i] as _).to_string_lossy(),
            );
        }

        /*
         * check the QName lookups
         */
        for i in NB_STRINGS_MAX - NB_STRINGS_NS..NB_STRINGS_MAX {
            cur = strings2[i];
            pref = addr_of_mut!(prefix[0]);
            while *cur != b':' {
                *pref = *cur;
                pref = pref.add(1);
                cur = cur.add(1);
            }
            cur = cur.add(1);
            *pref = 0;
            tmp = xml_dict_qlookup(dict, addr_of_mut!(prefix[0]), cur);
            eprintln!("prefix: {:?}", prefix);
            assert!(
                tmp == test2[i],
                "Failed lookup check for '{}':'{}'",
                CStr::from_ptr(addr_of_mut!(prefix[0]) as _).to_string_lossy(),
                CStr::from_ptr(cur as _).to_string_lossy(),
            );
        }
        /*
         * check the QName lookups for strings from the parent
         */
        for i in NB_STRINGS_MAX - NB_STRINGS_NS..NB_STRINGS_MAX {
            cur = strings1[i];
            pref = addr_of_mut!(prefix[0]);
            while *cur != b':' {
                *pref = *cur;
                pref = pref.add(1);
                cur = cur.add(1);
            }
            cur = cur.add(1);
            *pref = 0;
            tmp = xml_dict_qlookup(dict, addr_of_mut!(prefix[0]), cur);
            assert!(
                tmp == test1[i],
                "Failed parent lookup check for '{}':'{}'",
                CStr::from_ptr(addr_of_mut!(prefix[0]) as _).to_string_lossy(),
                CStr::from_ptr(cur as _).to_string_lossy(),
            );
        }

        xml_dict_free(dict);
        0
    }
}

/*
 * Test a single dictionary
 */
unsafe fn run_test1(
    strings1: &mut [*mut XmlChar],
    strings2: &mut [*mut XmlChar],
    test1: &mut [*const XmlChar],
    test2: &mut [*const XmlChar],
) -> c_int {
    unsafe {
        let mut prefix: [XmlChar; 40] = [0; 40];
        let mut cur: *mut XmlChar;
        let mut pref: *mut XmlChar;
        let mut tmp: *const XmlChar;

        let dict: XmlDictPtr = xml_dict_create();
        assert!(!dict.is_null(), "Out of memory while creating dictionary");
        /* Cast to avoid buggy warning on MSVC. */
        memset(test1.as_mut_ptr() as _, 0, test1.len());

        /*
         * Fill in NB_STRINGS_MIN, at this point the dictionary should not grow
         * and we allocate all those doing the fast key computations
         */
        for i in 0..NB_STRINGS_MIN {
            test1[i] = xml_dict_lookup(dict, strings1[i], -1);
            assert!(
                !test1[i].is_null(),
                "Failed lookup for '{}'",
                CStr::from_ptr(strings1[i] as _).to_string_lossy()
            );
        }
        /* ":foo" like strings1 */
        for (_, j) in (0..NB_STRINGS_MIN).zip(NB_STRINGS_MAX - NB_STRINGS_NS..) {
            test1[j] = xml_dict_lookup(dict, strings1[j], xml_strlen(strings1[j]));
            assert!(
                !test1[j].is_null(),
                "Failed lookup for '{}'",
                CStr::from_ptr(strings1[j] as _).to_string_lossy()
            );
        }
        /* "a:foo" like strings1 */
        for (_, j) in (0..NB_STRINGS_MIN).zip(NB_STRINGS_MAX - NB_STRINGS_MIN..) {
            test1[j] = xml_dict_lookup(dict, strings1[j], xml_strlen(strings1[j]));
            assert!(
                !test1[j].is_null(),
                "Failed lookup for '{}'",
                CStr::from_ptr(strings1[j] as _).to_string_lossy()
            );
        }

        /*
         * At this point allocate all the strings
         * the dictionary will grow in the process, reallocate more string tables
         * and switch to the better key generator
         */
        for i in 0..NB_STRINGS_MAX {
            if !test1[i].is_null() {
                continue;
            }
            test1[i] = xml_dict_lookup(dict, strings1[i], -1);
            assert!(
                !test1[i].is_null(),
                "Failed lookup for '{}'",
                CStr::from_ptr(strings1[i] as _).to_string_lossy()
            );
        }

        /*
         * Now we can start to test things, first that all strings1 belongs to
         * the dict
         */
        for i in 0..NB_STRINGS_MAX {
            assert!(
                xml_dict_owns(dict, test1[i]) != 0,
                "Failed ownership failure for '{}'",
                CStr::from_ptr(strings1[i] as _).to_string_lossy()
            );
        }

        /*
         * Then that another lookup to the string will return the same
         */
        for i in 0..NB_STRINGS_MAX {
            assert!(
                xml_dict_lookup(dict, strings1[i], -1) == test1[i],
                "Failed re-lookup check for {i}, '{}'",
                CStr::from_ptr(strings1[i] as _).to_string_lossy()
            );
        }

        /*
         * More complex, check the QName lookups
         */
        for i in NB_STRINGS_MAX - NB_STRINGS_NS..NB_STRINGS_MAX {
            cur = strings1[i];
            pref = addr_of_mut!(prefix[0]);
            while *cur != b':' {
                *pref = *cur;
                pref = pref.add(1);
                cur = cur.add(1);
            }
            cur = cur.add(1);
            *pref = 0;
            tmp = xml_dict_qlookup(dict, addr_of_mut!(prefix[0]), cur);
            assert!(
                tmp == test1[i],
                "Failed lookup check for '{}':'{}'\n",
                CStr::from_ptr(addr_of_mut!(prefix[0]) as _).to_string_lossy(),
                CStr::from_ptr(cur as _).to_string_lossy()
            );
        }

        assert!(run_test2(dict, strings1, strings2, test1, test2) == 0);

        xml_dict_free(dict);
        0
    }
}

#[test]
fn main() {
    unsafe {
        let mut strings1: [*mut XmlChar; NB_STRINGS_MAX] = [null_mut(); NB_STRINGS_MAX];
        let mut strings2: [*mut XmlChar; NB_STRINGS_MAX] = [null_mut(); NB_STRINGS_MAX];
        let mut test1: [*const XmlChar; NB_STRINGS_MAX] = [null(); NB_STRINGS_MAX];
        let mut test2: [*const XmlChar; NB_STRINGS_MAX] = [null(); NB_STRINGS_MAX];

        fill_strings(&mut strings1, &mut strings2);
        assert!(
            run_test1(&mut strings1, &mut strings2, &mut test1, &mut test2) == 0,
            "dictionary tests failed"
        );
        clean_strings(&mut strings1, &mut strings2);
        xml_cleanup_parser();
    }
}

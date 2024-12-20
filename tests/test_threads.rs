//! Rust implementation of original libxml2's `testThreads.c`.  
//! If you want this to work, copy the `test/` and `result/` directories from the original libxml2.
#![cfg_attr(feature = "sax1", allow(deprecated))]

use std::{
    ffi::{c_char, c_int, c_void, CStr},
    io::{stderr, stdout, Write},
    mem::zeroed,
    ptr::{addr_of_mut, null_mut},
};

use exml::{
    globals::{
        get_do_validity_checking_default_value, set_do_validity_checking_default_value,
        set_generic_error, GenericErrorContext,
    },
    libxml::{
        catalog::{xml_catalog_cleanup, xml_load_catalog},
        parser::{xml_cleanup_parser, xml_init_parser, xml_parse_file},
        xmlmemory::xml_memory_dump,
    },
    tree::{xml_free_doc, XmlDocPtr},
};
use libc::{memset, pthread_create, pthread_join, pthread_t, strcmp};

const MAX_ARGC: usize = 20;
const TEST_REPEAT_COUNT: usize = 500;
static mut TID: [pthread_t; MAX_ARGC] = unsafe { zeroed() };

struct XmlThreadParams<'a> {
    filename: &'a CStr,
    okay: c_int,
}

const CATALOG: &str = "test/threads/complex.xml";
static mut THREAD_PARAMS: [XmlThreadParams; 7] = [
    XmlThreadParams {
        filename: c"test/threads/abc.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: c"test/threads/acb.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: c"test/threads/bac.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: c"test/threads/bca.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: c"test/threads/cab.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: c"test/threads/cba.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: c"test/threads/invalid.xml",
        okay: 0,
    },
];
static NUM_THREADS: usize = unsafe { THREAD_PARAMS.len() };

extern "C" fn thread_specific_data(private_data: *mut c_void) -> *mut c_void {
    unsafe {
        let my_doc: XmlDocPtr;
        let params: *mut XmlThreadParams = private_data as *mut XmlThreadParams;
        let filename: *const c_char = (*params).filename.as_ptr();
        let mut okay: c_int = 1;

        if strcmp(filename, c"test/threads/invalid.xml".as_ptr()) == 0 {
            set_do_validity_checking_default_value(0);
            let stdout: Box<dyn Write> = Box::new(stdout());
            set_generic_error(None, Some(GenericErrorContext::new(stdout)));
        } else {
            set_do_validity_checking_default_value(1);
            let stderr: Box<dyn Write> = Box::new(stderr());
            set_generic_error(None, Some(GenericErrorContext::new(stderr)));
        }
        #[cfg(feature = "sax1")]
        {
            my_doc = xml_parse_file(filename);
        }
        #[cfg(not(feature = "sax1"))]
        {
            myDoc = xmlReadFile(filename, NULL, XML_WITH_CATALOG);
        }
        if !my_doc.is_null() {
            xml_free_doc(my_doc);
        } else {
            println!("parse failed");
            okay = 0;
        }
        if strcmp(filename, c"test/threads/invalid.xml".as_ptr()) == 0 {
            if get_do_validity_checking_default_value() != 0 {
                println!("ValidityCheckingDefaultValue override failed");
                okay = 0;
            }
        } else if get_do_validity_checking_default_value() != 1 {
            println!("ValidityCheckingDefaultValue override failed");
            okay = 0;
        }
        (*params).okay = okay;
        null_mut()
    }
}

#[test]
fn main() {
    let mut ret: c_int;

    unsafe {
        xml_init_parser();
        for _ in 0..TEST_REPEAT_COUNT {
            xml_load_catalog(CATALOG);

            memset(
                TID.as_mut_ptr() as _,
                0xff,
                size_of::<pthread_t>() * THREAD_PARAMS.len(),
            );

            for i in 0..NUM_THREADS {
                ret = pthread_create(
                    addr_of_mut!(TID[i]),
                    null_mut(),
                    thread_specific_data,
                    addr_of_mut!(THREAD_PARAMS[i]) as _,
                );
                assert_eq!(ret, 0, "pthread_create");
            }
            for &tid in TID.iter().take(NUM_THREADS) {
                let mut result: *mut c_void = null_mut();
                ret = pthread_join(tid, addr_of_mut!(result));
                assert_eq!(ret, 0, "pthread_join");
            }

            xml_catalog_cleanup();
            for (i, param) in THREAD_PARAMS.iter().take(NUM_THREADS).enumerate() {
                assert_ne!(
                    param.okay,
                    0,
                    "Thread {i} handling {} failed",
                    param.filename.to_string_lossy()
                );
            }
        }
        xml_cleanup_parser();
        xml_memory_dump();
    }
}

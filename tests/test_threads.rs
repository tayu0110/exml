//! Rust implementation of original libxml2's `testThreads.c`.  
//! If you want this to work, copy the `test/` and `result/` directories from the original libxml2.
#![cfg_attr(feature = "sax1", allow(deprecated))]

use std::{
    ffi::{c_int, c_void},
    io::{Write, stderr, stdout},
    ptr::{addr_of_mut, null_mut},
    sync::Mutex,
};

use exml::{
    globals::{
        GenericErrorContext, get_do_validity_checking_default_value,
        set_do_validity_checking_default_value, set_generic_error,
    },
    libxml::catalog::{xml_catalog_cleanup, xml_load_catalog},
    parser::{xml_cleanup_parser, xml_init_parser, xml_parse_file},
    tree::xml_free_doc,
};
use libc::{pthread_create, pthread_join, pthread_t};

const MAX_ARGC: usize = 20;
const TEST_REPEAT_COUNT: usize = 500;
static TID: Mutex<[pthread_t; MAX_ARGC]> = Mutex::new([0; MAX_ARGC]);

struct XmlThreadParams<'a> {
    filename: &'a str,
    okay: i32,
}

const CATALOG: &str = "test/threads/complex.xml";
static THREAD_PARAMS: Mutex<[XmlThreadParams; 7]> = Mutex::new([
    XmlThreadParams {
        filename: "test/threads/abc.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: "test/threads/acb.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: "test/threads/bac.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: "test/threads/bca.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: "test/threads/cab.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: "test/threads/cba.xml",
        okay: 0,
    },
    XmlThreadParams {
        filename: "test/threads/invalid.xml",
        okay: 0,
    },
]);

extern "C" fn thread_specific_data(private_data: *mut c_void) -> *mut c_void {
    unsafe {
        let params: *mut XmlThreadParams = private_data as *mut XmlThreadParams;
        let filename = (*params).filename;
        let mut okay: c_int = 1;

        if filename == "test/threads/invalid.xml" {
            set_do_validity_checking_default_value(false);
            let stdout: Box<dyn Write> = Box::new(stdout());
            set_generic_error(None, Some(GenericErrorContext::new(stdout)));
        } else {
            set_do_validity_checking_default_value(true);
            let stderr: Box<dyn Write> = Box::new(stderr());
            set_generic_error(None, Some(GenericErrorContext::new(stderr)));
        }
        #[cfg(feature = "sax1")]
        let my_doc = xml_parse_file(Some(filename));
        #[cfg(not(feature = "sax1"))]
        let my_doc = xml_read_file(filename, NULL, XML_WITH_CATALOG);
        if let Some(my_doc) = my_doc {
            xml_free_doc(my_doc);
        } else {
            println!("parse failed");
            okay = 0;
        }
        if filename == "test/threads/invalid.xml" {
            if get_do_validity_checking_default_value() {
                println!("ValidityCheckingDefaultValue override failed");
                okay = 0;
            }
        } else if !get_do_validity_checking_default_value() {
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

        let mut tid = TID.lock().unwrap();
        let mut thread_params = THREAD_PARAMS.lock().unwrap();
        let num_threads = thread_params.len();
        for _ in 0..TEST_REPEAT_COUNT {
            xml_load_catalog(CATALOG);
            tid[..num_threads].fill(u64::MAX);

            for i in 0..num_threads {
                ret = pthread_create(
                    &raw mut tid[i],
                    null_mut(),
                    thread_specific_data,
                    &raw mut thread_params[i] as _,
                );
                assert_eq!(ret, 0, "pthread_create");
            }
            for &tid in tid.iter().take(num_threads) {
                let mut result: *mut c_void = null_mut();
                ret = pthread_join(tid, addr_of_mut!(result));
                assert_eq!(ret, 0, "pthread_join");
            }

            xml_catalog_cleanup();
            for (i, param) in thread_params.iter().take(num_threads).enumerate() {
                assert_ne!(
                    param.okay, 0,
                    "Thread {i} handling {} failed",
                    param.filename
                );
            }
        }
        xml_cleanup_parser();
    }
}

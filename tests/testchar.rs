//! Rust implementation of original libxml2's `testchar.c`.  
//! If you want this to work, copy the `test/` and `result/` directories from the original libxml2.

use std::{
    cell::RefCell,
    ffi::{c_char, c_int, c_uchar},
    io::{stdout, Write},
    ptr::{addr_of_mut, null_mut},
    rc::Rc,
    slice::from_raw_parts,
    sync::RwLock,
};

use exml::{
    encoding::XmlCharEncoding,
    error::{XmlError, XmlParserErrors},
    globals::{set_structured_error, GenericErrorContext},
    io::XmlParserInputBuffer,
    libxml::{
        globals::{xml_free, xml_malloc},
        parser::xml_cleanup_parser,
        xmlmemory::xml_memory_dump,
        xmlstring::XmlChar,
    },
    parser::{
        xml_free_parser_ctxt, xml_new_input_stream, xml_new_parser_ctxt, xml_read_memory,
        XmlParserCtxtPtr, XmlParserInputPtr,
    },
    tree::{xml_free_doc, NodeCommon, XmlNodePtr},
};
use libc::{memset, strlen};

/// Test the UTF-8 decoding routines
///
/// author: Daniel Veillard
/// copy: see Copyright for the status of this software.

static LAST_ERROR: RwLock<XmlParserErrors> = RwLock::new(XmlParserErrors::XmlErrOK);

fn error_handler(unused: Option<GenericErrorContext>, err: &XmlError) {
    let mut last_error = LAST_ERROR.write().unwrap();
    if unused.is_none() && last_error.is_ok() {
        *last_error = err.code();
    }
}

static mut DOCUMENT1: [c_char; 100] = {
    let mut buf = [0; 100];
    let s = c"<doc>XXXX</doc>".to_bytes();
    assert!(s.len() <= buf.len());
    let mut i = 0;
    while i < s.len() {
        buf[i] = s[i] as i8;
        i += 1;
    }
    buf
};
static mut DOCUMENT2: [c_char; 100] = {
    let mut buf = [0; 100];
    let s = c"<doc foo='XXXX'/>".to_bytes();
    assert!(s.len() <= buf.len());
    let mut i = 0;
    while i < s.len() {
        buf[i] = s[i] as i8;
        i += 1;
    }
    buf
};

unsafe fn test_document_range_byte1(
    ctxt: XmlParserCtxtPtr,
    document: *mut c_char,
    len: c_int,
    data: *mut c_char,
    forbid1: c_int,
    forbid2: c_int,
) -> c_int {
    for i in 0u8..=0xFF {
        *LAST_ERROR.write().unwrap() = XmlParserErrors::XmlErrOK;
        (*ctxt).reset();

        *data.add(0) = i as c_char;

        let buffer = from_raw_parts(document as *const u8, len as usize).to_vec();
        let res = xml_read_memory(buffer, Some("test"), None, 0);
        let last_error = LAST_ERROR.read().unwrap();

        if i as i32 == forbid1 || i as i32 == forbid2 {
            assert!(
                !last_error.is_ok() && res.is_none(),
                "Failed to detect invalid char for Byte 0x{i:02X}: {}",
                i as char
            );
        } else if i == b'<' || i == b'&' {
            assert!(
                !last_error.is_ok() && res.is_none(),
                "Failed to detect illegal char {} for Byte 0x{i:02X}",
                i as char
            );
        } else if !(0x20..0x80).contains(&i) && i != 0x9 && i != 0xA && i != 0xD {
            assert!(
                *last_error == XmlParserErrors::XmlErrInvalidChar || res.is_none(),
                "Failed to detect invalid char for Byte 0x{i:02X}"
            );
        } else {
            assert!(
                res.is_some(),
                "Failed to parse valid char for Byte 0x{i:02X} : {}",
                i as char
            );
        }
        if let Some(res) = res {
            xml_free_doc(res);
        }
    }
    0
}

unsafe fn test_document_range_byte2(
    ctxt: XmlParserCtxtPtr,
    document: *mut c_char,
    len: c_int,
    data: *mut c_char,
) -> c_int {
    for i in 0x80..=0xFF {
        for j in 0..=0xFF {
            *LAST_ERROR.write().unwrap() = XmlParserErrors::XmlErrOK;
            (*ctxt).reset();

            *data.add(0) = i as c_char;
            *data.add(1) = j as c_char;

            let buffer = from_raw_parts(document as *const u8, len as usize).to_vec();
            let res = xml_read_memory(buffer, Some("test"), None, 0);
            let last_error = *LAST_ERROR.read().unwrap();

            #[allow(clippy::if_same_then_else)]
            if i & 0x80 != 0 && i & 0x40 == 0 {
                // if first bit of first c_char is set, then second bit must too
                assert!(
                    !last_error.is_ok() && res.is_none(),
                    "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X}"
                );
            } else if i & 0x80 != 0 && j & 0xC0 != 0x80 {
                // if first bit of first c_char is set, then second c_char first
                // bits must be 10
                assert!(
                    !last_error.is_ok() && res.is_none(),
                    "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X}"
                );
            } else if i & 0x80 != 0 && i & 0x1E == 0 {
                // if using a 2 byte encoding then the value must be greater
                // than 0x80, i.e. one of bits 5 to 1 of i must be set
                assert!(
                    !last_error.is_ok() && res.is_none(),
                    "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X}"
                )
            } else if i & 0xE0 == 0xE0 {
                // if third bit of first c_char is set, then the sequence would need
                // at least 3 bytes, but we give only 2 !
                assert!(
                    !last_error.is_ok() && res.is_none(),
                    "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X}"
                );
            } else {
                // We should see no error in remaining cases
                assert!(
                    last_error.is_ok() && res.is_some(),
                    "Failed to parse document for Bytes 0x{i:02X} 0x{j:02X}"
                );
            }
            if let Some(res) = res {
                xml_free_doc(res);
            }
        }
    }
    0
}

/// Test the correct UTF8 character parsing in context of XML documents
/// Those are in-context injection tests checking the parser behaviour on
/// edge case values at different point in content, beginning and end of
/// CDATA in text or in attribute values.
#[doc(alias = "testDocumentRanges")]
unsafe fn test_document_ranges() -> c_int {
    let mut data: *mut c_char;
    let test_ret: c_int = 0;

    // Set up a parsing context using the first document as
    // the current input source.
    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        eprintln!("Failed to allocate parser context");
        return 1;
    }

    let mut stdout = stdout();
    print!("testing 1 byte char in document: 1");
    stdout.flush().ok();
    data = addr_of_mut!(DOCUMENT1[5]);
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    // test 1 byte injection at beginning of area
    assert_eq!(
        test_document_range_byte1(
            ctxt,
            DOCUMENT1.as_mut_ptr(),
            strlen(DOCUMENT1.as_ptr() as _) as i32,
            data,
            -1,
            -1,
        ),
        0,
        "Failed to pass 'test_document_range_byte1' - 1"
    );
    print!(" 2");
    stdout.flush().ok();
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    // test 1 byte injection at end of area
    assert_eq!(
        test_document_range_byte1(
            ctxt,
            DOCUMENT1.as_mut_ptr(),
            strlen(DOCUMENT1.as_ptr() as _) as i32,
            data.add(3),
            -1,
            -1,
        ),
        0,
        "Failed to pass 'test_document_range_byte1' - 2"
    );

    print!(" 3");
    stdout.flush().ok();
    data = addr_of_mut!(DOCUMENT2[10]);
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    // test 1 byte injection at beginning of area
    assert_eq!(
        test_document_range_byte1(
            ctxt,
            DOCUMENT2.as_mut_ptr(),
            strlen(DOCUMENT2.as_ptr() as _) as i32,
            data,
            b'\'' as i32,
            -1,
        ),
        0,
        "Failed to pass 'test_document_range_byte1' - 3"
    );
    print!(" 4");
    stdout.flush().ok();
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    // test 1 byte injection at end of area
    assert_eq!(
        test_document_range_byte1(
            ctxt,
            DOCUMENT2.as_mut_ptr(),
            strlen(DOCUMENT2.as_ptr() as _) as i32,
            data.add(3),
            b'\'' as i32,
            -1,
        ),
        0,
        "Failed to pass 'test_document_range_byte1' - 4"
    );
    println!(" done");

    print!("testing 2 byte char in document: 1");
    stdout.flush().ok();
    data = addr_of_mut!(DOCUMENT1[5]);
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    // test 2 byte injection at beginning of area
    assert_eq!(
        test_document_range_byte2(
            ctxt,
            DOCUMENT1.as_mut_ptr(),
            strlen(DOCUMENT1.as_ptr() as _) as i32,
            data,
        ),
        0,
        "Failed to pass 'test_document_range_byte2' - 1"
    );
    print!(" 2");
    stdout.flush().ok();
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    // test 2 byte injection at end of area
    assert_eq!(
        test_document_range_byte2(
            ctxt,
            DOCUMENT1.as_mut_ptr(),
            strlen(DOCUMENT1.as_ptr() as _) as i32,
            data.add(2),
        ),
        0,
        "Failed to pass 'test_document_range_byte2' - 2"
    );

    print!(" 3");
    stdout.flush().ok();
    data = addr_of_mut!(DOCUMENT2[10]);
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    // test 2 byte injection at beginning of area
    assert_eq!(
        test_document_range_byte2(
            ctxt,
            DOCUMENT2.as_mut_ptr(),
            strlen(DOCUMENT2.as_ptr() as _) as i32,
            data,
        ),
        0,
        "Failed to pass 'test_document_range_byte2' - 3"
    );
    print!(" 4");
    stdout.flush().ok();
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    // test 2 byte injection at end of area
    assert_eq!(
        test_document_range_byte2(
            ctxt,
            DOCUMENT2.as_mut_ptr(),
            strlen(DOCUMENT2.as_ptr() as _) as i32,
            data.add(2),
        ),
        0,
        "Failed to pass 'test_document_range_byte2' - 4"
    );
    println!(" done");

    xml_free_parser_ctxt(ctxt);
    test_ret
}

unsafe fn test_char_range_byte1(ctxt: XmlParserCtxtPtr) -> c_int {
    let data: *mut c_char = (*(*ctxt).input).cur as *mut c_char;

    *data.add(1) = 0;
    *data.add(2) = 0;
    *data.add(3) = 0;
    for i in 0..0xFF {
        *data.add(0) = i as c_char;
        (*ctxt).charset = XmlCharEncoding::UTF8;
        (*ctxt).nb_errors = 0;

        *LAST_ERROR.write().unwrap() = XmlParserErrors::XmlErrOK;
        let mut len = 0;
        let c = (*ctxt).current_char(&mut len).unwrap_or('\0');
        if i == 0 || i >= 0x80 {
            // we must see an error there
            if *LAST_ERROR.read().unwrap() != XmlParserErrors::XmlErrInvalidChar {
                eprintln!("Failed to detect invalid char for Byte 0x{i:02X}");
                return 1;
            }
        } else if i == 0xD {
            if c != '\u{A}' || len != 1 {
                eprintln!("Failed to convert char for Byte 0x{i:02X}");
                return 1;
            }
        } else if c as i32 != i || len != 1 {
            eprintln!("Failed to parse char for Byte 0x{i:02X}");
            return 1;
        }
    }
    0
}

unsafe fn test_char_range_byte2(ctxt: XmlParserCtxtPtr) -> c_int {
    let data: *mut c_char = (*(*ctxt).input).cur as *mut c_char;

    *data.add(2) = 0;
    *data.add(3) = 0;
    for i in 0x80..0xFF {
        for j in 0..0xFF {
            *data.add(0) = i as c_char;
            *data.add(1) = j as c_char;
            (*ctxt).charset = XmlCharEncoding::UTF8;
            (*ctxt).nb_errors = 0;

            *LAST_ERROR.write().unwrap() = XmlParserErrors::XmlErrOK;
            let mut len = 0;
            let c = (*ctxt).current_char(&mut len).unwrap_or('\0');
            let last_error = *LAST_ERROR.read().unwrap();

            #[allow(clippy::if_same_then_else)]
            if i & 0x80 != 0 && i & 0x40 == 0 {
                // if first bit of first c_char is set, then second bit must too
                if last_error != XmlParserErrors::XmlErrInvalidChar {
                    eprintln!("Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X}");
                    return 1;
                }
            } else if i & 0x80 != 0 && j & 0xC0 != 0x80 {
                // if first bit of first c_char is set, then second c_char first
                // bits must be 10
                if last_error != XmlParserErrors::XmlErrInvalidChar {
                    eprintln!("Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X}: {c}");
                    return 1;
                }
            } else if i & 0x80 != 0 && i & 0x1E == 0 {
                // if using a 2 byte encoding then the value must be greater
                // than 0x80, i.e. one of bits 5 to 1 of i must be set
                if last_error != XmlParserErrors::XmlErrInvalidChar {
                    eprintln!("Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X}: {c}");
                    return 1;
                }
            } else if i & 0xE0 == 0xE0 {
                // if third bit of first c_char is set, then the sequence would need
                // at least 3 bytes, but we give only 2 !
                if last_error != XmlParserErrors::XmlErrInvalidChar {
                    eprintln!("Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X} 0x00");
                    return 1;
                }
            } else if !last_error.is_ok() || len != 2 {
                // We should see no error in remaining cases
                eprintln!("Failed to parse char for Bytes 0x{i:02X} 0x{j:02X}",);
                return 1;
            } else if c as i32 != (j & 0x3F) + ((i & 0x1F) << 6) {
                // Finally check the value is right
                eprintln!(
                    "Failed to parse char for Bytes 0x{i:02X} 0x{j:02X}: expect {} got {c}",
                    (j & 0x3F) + ((i & 0x1F) << 6),
                );
                return 1;
            }
        }
    }
    0
}

unsafe fn test_char_range_byte3(ctxt: XmlParserCtxtPtr) -> c_int {
    let lows: [c_uchar; 6] = [0, 0x80, 0x81, 0xC1, 0xFF, 0xBF];
    let data: *mut c_char = (*(*ctxt).input).cur as *mut c_char;

    *data.add(3) = 0;
    for i in 0xE0..=0xFF {
        for j in 0..=0xFF {
            for &low in &lows {
                *data.add(0) = i as c_char;
                *data.add(1) = j as c_char;
                let nk = low as i32;
                *data.add(2) = nk as c_char;
                let value = (nk & 0x3F) + ((j & 0x3F) << 6) + ((i & 0xF) << 12);
                (*ctxt).charset = XmlCharEncoding::UTF8;
                (*ctxt).nb_errors = 0;

                *LAST_ERROR.write().unwrap() = XmlParserErrors::XmlErrOK;
                let mut len = 0;
                let c = (*ctxt).current_char(&mut len).unwrap_or('\0');
                let last_error = *LAST_ERROR.read().unwrap();

                #[allow(clippy::if_same_then_else)]
                if i & 0xF0 == 0xF0 {
                    // if fourth bit of first c_char is set, then the sequence would need
                    // at least 4 bytes, but we give only 3 !
                    if last_error != XmlParserErrors::XmlErrInvalidChar {
                        eprintln!(
                            "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X} 0x{:02X}",
                            *data.add(3) as i32
                        );
                        return 1;
                    }
                } else if j & 0xC0 != 0x80 || nk & 0xC0 != 0x80 {
                    // The second and the third bytes must start with 10
                    if last_error != XmlParserErrors::XmlErrInvalidChar {
                        eprintln!(
                            "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X}"
                        );
                        return 1;
                    }
                } else if i & 0xF == 0 && j & 0x20 == 0 {
                    // if using a 3 byte encoding then the value must be greater
                    // than 0x800, i.e. one of bits 4 to 0 of i must be set or
                    // the 6th byte of data[1] must be set
                    if last_error != XmlParserErrors::XmlErrInvalidChar {
                        eprintln!(
                            "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X}"
                        );
                        return 1;
                    }
                } else if (value > 0xD7FF && value < 0xE000) || (value > 0xFFFD && value < 0x10000)
                {
                    // There are values in that range that are not allowed in XML-1.0
                    if last_error != XmlParserErrors::XmlErrInvalidChar {
                        eprintln!("Failed to detect invalid char 0x{value:04X} for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X}");
                        return 1;
                    }
                } else {
                    // We should see no error in remaining cases
                    assert!(
                        last_error.is_ok() && len == 3,
                        "Failed to parse char for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X}",
                    );
                    // Finally check the value is right
                    assert!(c as i32 == value,
                        "Failed to parse char for Bytes 0x{i:02X} 0x{j:02X} 0x{:02X}: expect {value} got {c}",
                        *data.add(2)
                    );
                }
            }
        }
    }
    0
}

unsafe fn test_char_range_byte4(ctxt: XmlParserCtxtPtr) -> c_int {
    let lows: [c_uchar; 6] = [0, 0x80, 0x81, 0xC1, 0xFF, 0xBF];
    let data: *mut c_char = (*(*ctxt).input).cur as *mut c_char;

    *data.add(4) = 0;
    for i in 0xF0..=0xFF {
        for j in 0..=0xFF {
            for k in 0..6 {
                for l in 0..6 {
                    *data.add(0) = i as c_char;
                    *data.add(1) = j as c_char;
                    let nk = lows[k] as i32;
                    *data.add(2) = nk as c_char;
                    let nl = lows[l] as i32;
                    *data.add(3) = nl as c_char;
                    let value =
                        (nl & 0x3F) + ((nk & 0x3F) << 6) + ((j & 0x3F) << 12) + ((i & 0x7) << 18);
                    (*ctxt).charset = XmlCharEncoding::UTF8;
                    (*ctxt).nb_errors = 0;

                    *LAST_ERROR.write().unwrap() = XmlParserErrors::XmlErrOK;
                    let mut len = 0;
                    let c = (*ctxt).current_char(&mut len).unwrap_or('\0');
                    let last_error = *LAST_ERROR.read().unwrap();

                    #[allow(clippy::if_same_then_else)]
                    if i & 0xF8 == 0xF8 {
                        // if fifth bit of first c_char is set, then the sequence would need
                        // at least 5 bytes, but we give only 4 !
                        assert_eq!(last_error, XmlParserErrors::XmlErrInvalidChar,
                            "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X} 0x{:02X}", *data.add(3)
                        );
                    } else if j & 0xC0 != 0x80 || nk & 0xC0 != 0x80 || nl & 0xC0 != 0x80 {
                        // The second, third and fourth bytes must start with 10
                        assert_eq!(last_error, XmlParserErrors::XmlErrInvalidChar, "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X} 0x{nl:02X}");
                    } else if i & 0x7 == 0 && j & 0x30 == 0 {
                        // if using a 3 byte encoding then the value must be greater
                        // than 0x10000, i.e. one of bits 3 to 0 of i must be set or
                        // the 6 or 5th byte of j must be set
                        assert_eq!(last_error, XmlParserErrors::XmlErrInvalidChar, "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X} 0x{nl:02X}");
                    } else if (value > 0xD7FF && value < 0xE000)
                        || (value > 0xFFFD && value < 0x10000)
                        || value > 0x10FFFF
                    {
                        // There are values in that range that are not allowed in XML-1.0
                        assert_eq!(last_error, XmlParserErrors::XmlErrInvalidChar, "Failed to detect invalid char 0x{value:04X} for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X} 0x{nl:02X}");
                    } else if !last_error.is_ok() || len != 4 {
                        // We should see no error in remaining cases
                        eprintln!("Failed to parse char for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X}");
                        return 1;
                    } else if c as i32 != value {
                        // Finally check the value is right
                        eprintln!("Failed to parse char for Bytes 0x{i:02X} 0x{j:02X} 0x{:02X}: expect {value} got {c}", *data.add(2) as i32);
                        return 1;
                    }
                }
            }
        }
    }
    0
}

/// Test the correct UTF8 character parsing in isolation i.e.
/// not when parsing a full document, this is less expensive and we can
/// cover the full range of UTF-8 chars accepted by XML-1.0
#[doc(alias = "testCharRanges")]
unsafe fn test_char_ranges() -> c_int {
    let mut data: [u8; 5] = [0; 5];
    let mut test_ret: c_int = 0;

    memset(data.as_mut_ptr() as _, 0, 5);

    // Set up a parsing context using the above data buffer as
    // the current input source.
    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    assert!(!ctxt.is_null(), "Failed to allocate parser context");
    let Some(buf) = XmlParserInputBuffer::from_memory(data.to_vec(), XmlCharEncoding::None) else {
        xml_free_parser_ctxt(ctxt);
        panic!("Failed to allocate input buffer");
    };
    let input: XmlParserInputPtr = xml_new_input_stream(Some(&mut *ctxt));
    if input.is_null() {
        test_ret = 1;
        // goto error;
        xml_free_parser_ctxt(ctxt);
        return test_ret;
    }
    (*input).filename = None;
    (*input).buf = Some(Rc::new(RefCell::new(buf)));
    (*input).cur = (*input)
        .buf
        .as_ref()
        .unwrap()
        .borrow()
        .buffer
        .map_or(null_mut(), |buf| buf.as_ref().as_ptr());
    (*input).base = (*input).cur;
    (*input).end = (*input).base.add(4);
    (*ctxt).input_push(input);

    let mut stdout = stdout();
    print!("testing char range: 1");
    stdout.flush().ok();
    assert_eq!(test_char_range_byte1(ctxt), 0);
    print!(" 2");
    stdout.flush().ok();
    assert_eq!(test_char_range_byte2(ctxt), 0);
    print!(" 3");
    stdout.flush().ok();
    assert_eq!(test_char_range_byte3(ctxt), 0);
    print!(" 4");
    stdout.flush().ok();
    assert_eq!(test_char_range_byte4(ctxt), 0);
    println!(" done");
    stdout.flush().ok();

    // error:
    xml_free_parser_ctxt(ctxt);
    test_ret
}

unsafe fn test_user_encoding() -> c_int {
    // Create a document encoded as UTF-16LE with an ISO-8859-1 encoding
    // declaration, then parse it with xmlReadMemory and the encoding
    // argument set to UTF-16LE.
    let start: *const c_char = c"<?xml version='1.0' encoding='ISO-8859-1'?><d>".as_ptr();
    let end: *const c_char = c"</d>".as_ptr();
    let start_size: c_int = strlen(start) as _;
    let text_size: c_int = 100000; /* Make sure to exceed internal buffer sizes. */
    let end_size: c_int = strlen(end) as _;
    let total_size: c_int = start_size + text_size + end_size;
    let mut k: usize = 0;

    let buf: *mut XmlChar = xml_malloc(2 * total_size as usize) as _;
    for i in (0..).take_while(|&i| *start.add(i) != 0) {
        *buf.add(k) = *start.add(i) as _;
        k += 1;
        *buf.add(k) = 0;
        k += 1;
    }
    for _ in 0..text_size {
        *buf.add(k) = b'x' as _;
        k += 1;
        *buf.add(k) = 0;
        k += 1;
    }
    for i in (0..).take_while(|&i| *end.add(i) != 0) {
        *buf.add(k) = *end.add(i) as _;
        k += 1;
        *buf.add(k) = 0;
        k += 1;
    }

    let buffer = from_raw_parts(buf, 2 * total_size as usize).to_vec();
    let Some(doc) = xml_read_memory(buffer, None, Some("UTF-16LE"), 0) else {
        // let error = get_last_error();
        // eprintln!("error: {error:?}");
        // goto error;
        xml_free(buf as _);
        panic!("failed to parse document");
    };

    let text: *mut XmlChar = XmlNodePtr::try_from(doc.children().unwrap().children().unwrap())
        .unwrap()
        .content;
    eprintln!(
        "text: {}",
        std::str::from_utf8_unchecked(std::slice::from_raw_parts(text, text_size as usize))
    );
    for i in 0..text_size {
        if *text.add(i as usize) != b'x' {
            // goto error;
            xml_free_doc(doc);
            xml_free(buf as _);
            panic!("text node has wrong content at offset {i}");
        }
    }

    // error:
    xml_free_doc(doc);
    xml_free(buf as _);

    0
}

#[test]
fn main() {
    // this initialize the library and check potential ABI mismatches
    // between the version it was compiled for and the actual shared
    // library used.
    // LIBXML_TEST_VERSION

    // Catch errors separately

    set_structured_error(Some(error_handler), None);
    unsafe {
        // Run the tests
        assert!(
            test_char_ranges() == 0,
            "Failed to pass 'test_char_ranges()'"
        );
        assert!(
            test_document_ranges() == 0,
            "Failed to pass 'test_document_ranges()'"
        );
        assert!(
            test_user_encoding() == 0,
            "Failed to pass 'test_user_encoding()'"
        );

        // Cleanup function for the XML library.
        xml_cleanup_parser();
        // this is to debug memory for regression tests
        xml_memory_dump();
    }
}

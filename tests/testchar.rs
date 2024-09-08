use std::{
    ffi::{c_char, c_int, c_uchar},
    os::raw::c_void,
    ptr::{addr_of_mut, null_mut},
};

use exml::libxml::{
    encoding::XmlCharEncoding,
    globals::{xml_free, xml_malloc},
    parser::{
        xml_cleanup_parser, xml_ctxt_reset, xml_free_parser_ctxt, xml_new_parser_ctxt,
        xml_read_memory, XmlParserCtxtPtr, XmlParserInputPtr,
    },
    parser_internals::{input_push, xml_current_char, xml_new_input_stream},
    tree::{xml_buf_content, xml_free_doc, XmlDocPtr},
    xml_io::{
        xml_free_parser_input_buffer, xml_parser_input_buffer_create_mem, XmlParserInputBufferPtr,
    },
    xmlerror::{xml_set_structured_error_func, XmlErrorPtr, XmlParserErrors},
    xmlmemory::xml_memory_dump,
    xmlstring::XmlChar,
};
use libc::{fflush, fprintf, memset, printf, strlen, FILE};

/**
 * Test the UTF-8 decoding routines
 *
 * author: Daniel Veillard
 * copy: see Copyright for the status of this software.
 */

static mut LAST_ERROR: c_int = 0;

unsafe extern "C" fn error_handler(unused: *mut c_void, err: XmlErrorPtr) {
    if unused.is_null() && !err.is_null() && LAST_ERROR == 0 {
        LAST_ERROR = (*err).code;
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

unsafe extern "C" fn test_document_range_byte1(
    ctxt: XmlParserCtxtPtr,
    document: *mut c_char,
    len: c_int,
    data: *mut c_char,
    forbid1: c_int,
    forbid2: c_int,
) -> c_int {
    for i in 0u8..=0xFF {
        LAST_ERROR = 0;
        xml_ctxt_reset(ctxt);

        *data.add(0) = i as c_char;

        let res = xml_read_memory(document, len, c"test".as_ptr() as _, null_mut(), 0);

        if i as i32 == forbid1 || i as i32 == forbid2 {
            assert!(
                LAST_ERROR != 0 && res.is_null(),
                "Failed to detect invalid char for Byte 0x{i:02X}: {}",
                i as char
            );
        } else if i == b'<' || i == b'&' {
            assert!(
                LAST_ERROR != 0 && res.is_null(),
                "Failed to detect illegal char {} for Byte 0x{i:02X}",
                i as char
            );
        } else if !(0x20..0x80).contains(&i) && i != 0x9 && i != 0xA && i != 0xD {
            assert!(
                LAST_ERROR == XmlParserErrors::XmlErrInvalidChar as i32 || res.is_null(),
                "Failed to detect invalid char for Byte 0x{i:02X}"
            );
        } else {
            assert!(
                !res.is_null(),
                "Failed to parse valid char for Byte 0x{i:02X} : {}",
                i as char
            );
        }
        if !res.is_null() {
            xml_free_doc(res);
        }
    }
    0
}

unsafe extern "C" fn test_document_range_byte2(
    ctxt: XmlParserCtxtPtr,
    document: *mut c_char,
    len: c_int,
    data: *mut c_char,
) -> c_int {
    for i in 0x80..=0xFF {
        for j in 0..=0xFF {
            LAST_ERROR = 0;
            xml_ctxt_reset(ctxt);

            *data.add(0) = i as c_char;
            *data.add(1) = j as c_char;

            let res = xml_read_memory(document, len, c"test".as_ptr() as _, null_mut(), 0);

            #[allow(clippy::if_same_then_else)]
            /* if first bit of first c_char is set, then second bit must too */
            if i & 0x80 != 0 && i & 0x40 == 0 {
                assert!(
                    LAST_ERROR != 0 && res.is_null(),
                    "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X}"
                );
            }
            /*
             * if first bit of first c_char is set, then second c_char first
             * bits must be 10
             */
            else if i & 0x80 != 0 && j & 0xC0 != 0x80 {
                assert!(
                    LAST_ERROR != 0 && res.is_null(),
                    "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X}"
                );
            }
            /*
             * if using a 2 byte encoding then the value must be greater
             * than 0x80, i.e. one of bits 5 to 1 of i must be set
             */
            else if i & 0x80 != 0 && i & 0x1E == 0 {
                assert!(
                    !LAST_ERROR != 0 && res.is_null(),
                    "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X}"
                )
            }
            /*
             * if third bit of first c_char is set, then the sequence would need
             * at least 3 bytes, but we give only 2 !
             */
            else if i & 0xE0 == 0xE0 {
                assert!(
                    !LAST_ERROR != 0 && res.is_null(),
                    "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X}"
                );
            } else {
                /*
                 * We should see no error in remaining cases
                 */
                assert!(
                    LAST_ERROR == 0 && !res.is_null(),
                    "Failed to parse document for Bytes 0x{i:02X} 0x{j:02X}"
                );
            }
            if !res.is_null() {
                xml_free_doc(res);
            }
        }
    }
    0
}

/**
 * testDocumentRanges:
 *
 * Test the correct UTF8 character parsing in context of XML documents
 * Those are in-context injection tests checking the parser behaviour on
 * edge case values at different point in content, beginning and end of
 * CDATA in text or in attribute values.
 */

unsafe extern "C" fn test_document_ranges() -> c_int {
    let mut data: *mut c_char;
    let test_ret: c_int = 0;

    /*
     * Set up a parsing context using the first document as
     * the current input source.
     */
    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        fprintf(stderr, c"Failed to allocate parser context\n".as_ptr());
        return 1;
    }

    extern "C" {
        static stdout: *mut FILE;
        static stderr: *mut FILE;
    }

    printf(c"testing 1 byte char in document: 1".as_ptr());
    fflush(stdout);
    data = addr_of_mut!(DOCUMENT1[5]);
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    /* test 1 byte injection at beginning of area */
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
    printf(c" 2".as_ptr());
    fflush(stdout);
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    /* test 1 byte injection at end of area */
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

    printf(c" 3".as_ptr());
    fflush(stdout);
    data = addr_of_mut!(DOCUMENT2[10]);
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    /* test 1 byte injection at beginning of area */
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
    printf(c" 4".as_ptr());
    fflush(stdout);
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    /* test 1 byte injection at end of area */
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
    printf(c" done\n".as_ptr());

    printf(c"testing 2 byte char in document: 1".as_ptr());
    fflush(stdout);
    data = addr_of_mut!(DOCUMENT1[5]);
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    /* test 2 byte injection at beginning of area */
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
    printf(c" 2".as_ptr());
    fflush(stdout);
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    /* test 2 byte injection at end of area */
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

    printf(c" 3".as_ptr());
    fflush(stdout);
    data = addr_of_mut!(DOCUMENT2[10]);
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    /* test 2 byte injection at beginning of area */
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
    printf(c" 4".as_ptr());
    fflush(stdout);
    *data.add(0) = b' ' as _;
    *data.add(1) = b' ' as _;
    *data.add(2) = b' ' as _;
    *data.add(3) = b' ' as _;
    /* test 2 byte injection at end of area */
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
    printf(c" done\n".as_ptr());

    xml_free_parser_ctxt(ctxt);
    test_ret
}

unsafe extern "C" fn test_char_range_byte1(ctxt: XmlParserCtxtPtr) -> c_int {
    let mut c: c_int;
    let data: *mut c_char = (*(*ctxt).input).cur as *mut c_char;

    extern "C" {
        static stderr: *mut FILE;
    }

    *data.add(1) = 0;
    *data.add(2) = 0;
    *data.add(3) = 0;
    for i in 0..0xFF {
        *data.add(0) = i as c_char;
        (*ctxt).charset = XmlCharEncoding::XmlCharEncodingUtf8 as i32;
        (*ctxt).nb_errors = 0;

        LAST_ERROR = 0;
        let mut len = 0;
        c = xml_current_char(ctxt, addr_of_mut!(len));
        if i == 0 || i >= 0x80 {
            /* we must see an error there */
            if LAST_ERROR != XmlParserErrors::XmlErrInvalidChar as i32 {
                fprintf(
                    stderr,
                    c"Failed to detect invalid char for Byte 0x%02X\n".as_ptr(),
                    i,
                );
                return 1;
            }
        } else if i == 0xD {
            if c != 0xA || len != 1 {
                fprintf(
                    stderr,
                    c"Failed to convert char for Byte 0x%02X\n".as_ptr(),
                    i,
                );
                return 1;
            }
        } else if c != i || len != 1 {
            fprintf(
                stderr,
                c"Failed to parse char for Byte 0x%02X\n".as_ptr(),
                i,
            );
            return 1;
        }
    }
    0
}

unsafe extern "C" fn test_char_range_byte2(ctxt: XmlParserCtxtPtr) -> c_int {
    let data: *mut c_char = (*(*ctxt).input).cur as *mut c_char;

    extern "C" {
        static stderr: *mut FILE;
    }

    *data.add(2) = 0;
    *data.add(3) = 0;
    for i in 0x80..0xFF {
        for j in 0..0xFF {
            *data.add(0) = i as c_char;
            *data.add(1) = j as c_char;
            (*ctxt).charset = XmlCharEncoding::XmlCharEncodingUtf8 as i32;
            (*ctxt).nb_errors = 0;

            LAST_ERROR = 0;
            let mut len = 0;
            let c = xml_current_char(ctxt, addr_of_mut!(len));

            #[allow(clippy::if_same_then_else)]
            /* if first bit of first c_char is set, then second bit must too */
            if i & 0x80 != 0 && i & 0x40 == 0 {
                if LAST_ERROR != XmlParserErrors::XmlErrInvalidChar as i32 {
                    fprintf(
                        stderr,
                        c"Failed to detect invalid char for Bytes 0x%02X 0x%02X\n".as_ptr(),
                        i,
                        j,
                    );
                    return 1;
                }
            }
            /*
             * if first bit of first c_char is set, then second c_char first
             * bits must be 10
             */
            else if i & 0x80 != 0 && j & 0xC0 != 0x80 {
                if LAST_ERROR != XmlParserErrors::XmlErrInvalidChar as i32 {
                    fprintf(
                        stderr,
                        c"Failed to detect invalid char for Bytes 0x%02X 0x%02X: %d\n".as_ptr(),
                        i,
                        j,
                        c,
                    );
                    return 1;
                }
            }
            /*
             * if using a 2 byte encoding then the value must be greater
             * than 0x80, i.e. one of bits 5 to 1 of i must be set
             */
            else if i & 0x80 != 0 && i & 0x1E == 0 {
                if LAST_ERROR != XmlParserErrors::XmlErrInvalidChar as i32 {
                    fprintf(
                        stderr,
                        c"Failed to detect invalid char for Bytes 0x%02X 0x%02X: %d\n".as_ptr(),
                        i,
                        j,
                        c,
                    );
                    return 1;
                }
            }
            /*
             * if third bit of first c_char is set, then the sequence would need
             * at least 3 bytes, but we give only 2 !
             */
            else if i & 0xE0 == 0xE0 {
                if LAST_ERROR != XmlParserErrors::XmlErrInvalidChar as i32 {
                    fprintf(
                        stderr,
                        c"Failed to detect invalid char for Bytes 0x%02X 0x%02X 0x00\n".as_ptr(),
                        i,
                        j,
                    );
                    return 1;
                }
            }
            /*
             * We should see no error in remaining cases
             */
            else if LAST_ERROR != 0 || len != 2 {
                fprintf(
                    stderr,
                    c"Failed to parse char for Bytes 0x%02X 0x%02X\n".as_ptr(),
                    i,
                    j,
                );
                return 1;
            }
            /*
             * Finally check the value is right
             */
            else if c != (j & 0x3F) + ((i & 0x1F) << 6) {
                fprintf(
                    stderr,
                    c"Failed to parse char for Bytes 0x%02X 0x%02X: expect %d got %d\n".as_ptr(),
                    i,
                    j,
                    (j & 0x3F) + ((i & 0x1F) << 6),
                    c,
                );
                return 1;
            }
        }
    }
    0
}

unsafe extern "C" fn test_char_range_byte3(ctxt: XmlParserCtxtPtr) -> c_int {
    let lows: [c_uchar; 6] = [0, 0x80, 0x81, 0xC1, 0xFF, 0xBF];
    let data: *mut c_char = (*(*ctxt).input).cur as *mut c_char;

    extern "C" {
        static stderr: *mut FILE;
    }

    *data.add(3) = 0;
    for i in 0xE0..=0xFF {
        for j in 0..=0xFF {
            for &low in &lows {
                *data.add(0) = i as c_char;
                *data.add(1) = j as c_char;
                let nk = low as i32;
                *data.add(2) = nk as c_char;
                let value = (nk & 0x3F) + ((j & 0x3F) << 6) + ((i & 0xF) << 12);
                (*ctxt).charset = XmlCharEncoding::XmlCharEncodingUtf8 as i32;
                (*ctxt).nb_errors = 0;

                LAST_ERROR = 0;
                let mut len = 0;
                let c = xml_current_char(ctxt, addr_of_mut!(len));

                #[allow(clippy::if_same_then_else)]
                /*
                 * if fourth bit of first c_char is set, then the sequence would need
                 * at least 4 bytes, but we give only 3 !
                 */
                if i & 0xF0 == 0xF0 {
                    if LAST_ERROR != XmlParserErrors::XmlErrInvalidChar as i32 {
                        fprintf(stderr, c"Failed to detect invalid char for Bytes 0x%02X 0x%02X 0x%02X 0x%02X\n".as_ptr(), i, j, nk, *data.add(3) as i32);
                        return 1;
                    }
                }
                /*
                 * The second and the third bytes must start with 10
                 */
                else if j & 0xC0 != 0x80 || nk & 0xC0 != 0x80 {
                    if LAST_ERROR != XmlParserErrors::XmlErrInvalidChar as i32 {
                        fprintf(
                            stderr,
                            c"Failed to detect invalid char for Bytes 0x%02X 0x%02X 0x%02X\n"
                                .as_ptr(),
                            i,
                            j,
                            nk,
                        );
                        return 1;
                    }
                }
                /*
                 * if using a 3 byte encoding then the value must be greater
                 * than 0x800, i.e. one of bits 4 to 0 of i must be set or
                 * the 6th byte of data[1] must be set
                 */
                else if i & 0xF == 0 && j & 0x20 == 0 {
                    if LAST_ERROR != XmlParserErrors::XmlErrInvalidChar as i32 {
                        fprintf(
                            stderr,
                            c"Failed to detect invalid char for Bytes 0x%02X 0x%02X 0x%02X\n"
                                .as_ptr(),
                            i,
                            j,
                            nk,
                        );
                        return 1;
                    }
                }
                /*
                 * There are values in that range that are not allowed in XML-1.0
                 */
                else if ((value > 0xD7FF) && (value < 0xE000))
                    || ((value > 0xFFFD) && (value < 0x10000))
                {
                    if LAST_ERROR != XmlParserErrors::XmlErrInvalidChar as i32 {
                        fprintf(stderr, c"Failed to detect invalid char 0x%04X for Bytes 0x%02X 0x%02X 0x%02X\n".as_ptr(), value, i, j, nk);
                        return 1;
                    }
                } else {
                    /*
                     * We should see no error in remaining cases
                     */
                    assert!(
                        LAST_ERROR == 0 && len == 3,
                        "Failed to parse char for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X}",
                    );
                    /*
                     * Finally check the value is right
                     */
                    assert!(c == value,
                        "Failed to parse char for Bytes 0x{i:02X} 0x{j:02X} 0x{:02X}: expect {value} got {c}",
                        *data.add(2)
                    );
                }
            }
        }
    }
    0
}

unsafe extern "C" fn test_char_range_byte4(ctxt: XmlParserCtxtPtr) -> c_int {
    let lows: [c_uchar; 6] = [0, 0x80, 0x81, 0xC1, 0xFF, 0xBF];
    let data: *mut c_char = (*(*ctxt).input).cur as *mut c_char;

    extern "C" {
        static stderr: *mut FILE;
    }

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
                    (*ctxt).charset = XmlCharEncoding::XmlCharEncodingUtf8 as i32;
                    (*ctxt).nb_errors = 0;

                    LAST_ERROR = 0;
                    let mut len = 0;
                    let c = xml_current_char(ctxt, addr_of_mut!(len));

                    #[allow(clippy::if_same_then_else)]
                    /*
                     * if fifth bit of first c_char is set, then the sequence would need
                     * at least 5 bytes, but we give only 4 !
                     */
                    if i & 0xF8 == 0xF8 {
                        assert_eq!(LAST_ERROR, XmlParserErrors::XmlErrInvalidChar as i32,
                            "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X} 0x{:02X}", *data.add(3)
                        );
                    }
                    /*
                     * The second, third and fourth bytes must start with 10
                     */
                    else if j & 0xC0 != 0x80 || nk & 0xC0 != 0x80 || nl & 0xC0 != 0x80 {
                        assert_eq!(LAST_ERROR, XmlParserErrors::XmlErrInvalidChar as i32, "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X} 0x{nl:02X}");
                    }
                    /*
                     * if using a 3 byte encoding then the value must be greater
                     * than 0x10000, i.e. one of bits 3 to 0 of i must be set or
                     * the 6 or 5th byte of j must be set
                     */
                    else if i & 0x7 == 0 && j & 0x30 == 0 {
                        assert_eq!(LAST_ERROR, XmlParserErrors::XmlErrInvalidChar as i32, "Failed to detect invalid char for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X} 0x{nl:02X}");
                    }
                    /*
                     * There are values in that range that are not allowed in XML-1.0
                     */
                    else if (value > 0xD7FF && value < 0xE000)
                        || (value > 0xFFFD && value < 0x10000)
                        || value > 0x10FFFF
                    {
                        assert_eq!(LAST_ERROR, XmlParserErrors::XmlErrInvalidChar as i32, "Failed to detect invalid char 0x{value:04X} for Bytes 0x{i:02X} 0x{j:02X} 0x{nk:02X} 0x{nl:02X}");
                    }
                    /*
                     * We should see no error in remaining cases
                     */
                    else if LAST_ERROR != 0 || len != 4 {
                        fprintf(
                            stderr,
                            c"Failed to parse char for Bytes 0x%02X 0x%02X 0x%02X\n".as_ptr(),
                            i,
                            j,
                            nk,
                        );
                        return 1;
                    }
                    /*
                     * Finally check the value is right
                     */
                    else if c != value {
                        fprintf(stderr, c"Failed to parse char for Bytes 0x%02X 0x%02X 0x%02X: expect %d got %d\n".as_ptr(), i, j, *data.add(2) as i32, value, c);
                        return 1;
                    }
                }
            }
        }
    }
    0
}

/**
 * testCharRanges:
 *
 * Test the correct UTF8 character parsing in isolation i.e.
 * not when parsing a full document, this is less expensive and we can
 * cover the full range of UTF-8 chars accepted by XML-1.0
 */

unsafe extern "C" fn test_char_ranges() -> c_int {
    let mut data: [c_char; 5] = [0; 5];
    let mut test_ret: c_int = 0;

    memset(data.as_mut_ptr() as _, 0, 5);

    extern "C" {
        static stdout: *mut FILE;
    }

    /*
     * Set up a parsing context using the above data buffer as
     * the current input source.
     */
    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    assert!(!ctxt.is_null(), "Failed to allocate parser context");
    let buf: XmlParserInputBufferPtr = xml_parser_input_buffer_create_mem(
        data.as_mut_ptr(),
        data.len() as i32,
        XmlCharEncoding::XmlCharEncodingNone,
    );
    if buf.is_null() {
        xml_free_parser_ctxt(ctxt);
        panic!("Failed to allocate input buffer");
    }
    let input: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input.is_null() {
        xml_free_parser_input_buffer(buf);
        test_ret = 1;
        // goto error;
        xml_free_parser_ctxt(ctxt);
        return test_ret;
    }
    (*input).filename = null_mut();
    (*input).buf = buf;
    (*input).cur = xml_buf_content((*(*input).buf).buffer);
    (*input).base = (*input).cur;
    (*input).end = (*input).base.add(4);
    input_push(ctxt, input);

    printf(c"testing char range: 1".as_ptr());
    fflush(stdout);
    assert_eq!(test_char_range_byte1(ctxt), 0);
    printf(c" 2".as_ptr());
    fflush(stdout);
    assert_eq!(test_char_range_byte2(ctxt), 0);
    printf(c" 3".as_ptr());
    fflush(stdout);
    assert_eq!(test_char_range_byte3(ctxt), 0);
    printf(c" 4".as_ptr());
    fflush(stdout);
    assert_eq!(test_char_range_byte4(ctxt), 0);
    printf(c" done\n".as_ptr());
    fflush(stdout);

    // error:
    xml_free_parser_ctxt(ctxt);
    test_ret
}

unsafe extern "C" fn test_user_encoding() -> c_int {
    /*
     * Create a document encoded as UTF-16LE with an ISO-8859-1 encoding
     * declaration, then parse it with xmlReadMemory and the encoding
     * argument set to UTF-16LE.
     */
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

    let doc: XmlDocPtr = xml_read_memory(
        buf as _,
        2 * total_size,
        null_mut(),
        c"UTF-16LE".as_ptr() as _,
        0,
    );
    if doc.is_null() {
        // goto error;
        xml_free_doc(doc);
        xml_free(buf as _);
        panic!("failed to parse document");
    }

    let text: *mut XmlChar = (*(*(*doc).children).children).content;
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
    /*
     * this initialize the library and check potential ABI mismatches
     * between the version it was compiled for and the actual shared
     * library used.
     */
    // LIBXML_TEST_VERSION

    /*
     * Catch errors separately
     */

    unsafe {
        xml_set_structured_error_func(null_mut(), Some(error_handler));

        /*
         * Run the tests
         */
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

        /*
         * Cleanup function for the XML library.
         */
        xml_cleanup_parser();
        /*
         * this is to debug memory for regression tests
         */
        xml_memory_dump();
    }
}

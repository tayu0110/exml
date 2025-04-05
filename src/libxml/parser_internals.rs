//! Provide internal methods and data structures for parsing XML documents.
//!
//! This module is based on `libxml/parserInternals.h`, `parserInternals.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: internals routines and limits exported by the parser.
// Description: this module exports a number of internal parsing routines
//              they are not really all intended for applications but
//              can prove useful doing low level processing.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// parserInternals.c : Internal routines (and obsolete ones) needed for the XML and HTML parsers.
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use crate::libxml::xmlstring::XmlChar;

/// Append the char value in the array
///
/// Returns the number of xmlChar written
#[doc(alias = "xmlCopyCharMultiByte")]
pub unsafe fn xml_copy_char_multi_byte(mut out: *mut XmlChar, val: i32) -> i32 {
    unsafe {
        if out.is_null() || val < 0 {
            return 0;
        }
        // We are supposed to handle UTF8, check it's valid
        // From rfc2044: encoding of the Unicode values on UTF-8:
        //
        // UCS-4 range (hex.)           UTF-8 octet sequence (binary)
        // 0000 0000-0000 007F   0xxxxxxx
        // 0000 0080-0000 07FF   110xxxxx 10xxxxxx
        // 0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
        if val >= 0x80 {
            let savedout: *mut XmlChar = out;
            let bits: i32;
            if val < 0x800 {
                *out = (val >> 6) as u8 | 0xC0;
                out = out.add(1);
                bits = 0;
            } else if val < 0x10000 {
                *out = (val >> 12) as u8 | 0xE0;
                out = out.add(1);
                bits = 6;
            } else if val < 0x110000 {
                *out = (val >> 18) as u8 | 0xF0;
                out = out.add(1);
                bits = 12;
            } else {
                // xml_err_encoding_int!(
                //     null_mut(),
                //     XmlParserErrors::XmlErrInvalidChar,
                //     "Internal error, xmlCopyCharMultiByte 0x{:X} out of bound\n",
                //     val
                // );
                return 0;
            }

            for bits in (0..=bits).rev().step_by(6) {
                *out = ((val >> bits) as u8 & 0x3F) | 0x80;
                out = out.add(1);
            }
            return out.offset_from(savedout) as _;
        }
        *out = val as _;
        1
    }
}

/// Append the char value in the array
///
/// Returns the number of xmlChar written
#[doc(alias = "xmlCopyChar")]
pub unsafe fn xml_copy_char(_len: i32, out: *mut XmlChar, val: i32) -> i32 {
    unsafe {
        if out.is_null() || val < 0 {
            return 0;
        }
        // the len parameter is ignored
        if val >= 0x80 {
            return xml_copy_char_multi_byte(out, val);
        }
        *out = val as _;
        1
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_copy_char() {
        unsafe {
            let mut leaks = 0;

            for n_len in 0..GEN_NB_INT {
                for n_out in 0..GEN_NB_XML_CHAR_PTR {
                    for n_val in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let len = gen_int(n_len, 0);
                        let out = gen_xml_char_ptr(n_out, 1);
                        let val = gen_int(n_val, 2);

                        let ret_val = xml_copy_char(len, out, val);
                        desret_int(ret_val);
                        des_int(n_len, len, 0);
                        des_xml_char_ptr(n_out, out, 1);
                        des_int(n_val, val, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlCopyChar",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlCopyChar()");
                            eprint!(" {}", n_len);
                            eprint!(" {}", n_out);
                            eprintln!(" {}", n_val);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_copy_char_multi_byte() {
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_XML_CHAR_PTR {
                for n_val in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let out = gen_xml_char_ptr(n_out, 0);
                    let val = gen_int(n_val, 1);

                    let ret_val = xml_copy_char_multi_byte(out, val);
                    desret_int(ret_val);
                    des_xml_char_ptr(n_out, out, 0);
                    des_int(n_val, val, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCopyCharMultiByte",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlCopyCharMultiByte()"
                        );
                        eprint!(" {}", n_out);
                        eprintln!(" {}", n_val);
                    }
                }
            }
        }
    }
}

// Copyright of the original code is the following.
// --------
// Summary: the core parser module
// Description: Interfaces, constants and types related to the XML parser
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// parser.c : an XML 1.0 parser, namespaces and validity support are mostly
//            implemented on top of the SAX interfaces
//
// References:
//   The XML specification:
//     http://www.w3.org/TR/REC-xml
//   Original 1.0 version:
//     http://www.w3.org/TR/1998/REC-xml-19980210
//   XML second edition working draft
//     http://www.w3.org/TR/2000/WD-xml-2e-20000814
//
// Okay this is a big file, the parser core is around 7000 lines, then it
// is followed by the progressive parser top routines, then the various
// high level APIs to call the parser and a few miscellaneous functions.
// A number of helper functions and deprecated ones have been moved to
// parserInternals.c to reduce this file size.
// As much as possible the functions are associated with their relative
// production in the XML specification. A few productions defining the
// different ranges of character are actually implanted either in
// parserInternals.h or parserInternals.c
// The DOM tree build is realized from the default SAX callbacks in
// the module SAX.c.
// The routines doing the validation checks are in valid.c and called either
// from the SAX callbacks or as standalone functions using a preparsed
// document.
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

mod context;
mod error;
mod input;
mod node_info;
mod qname;

use std::{
    cell::RefCell,
    ffi::CStr,
    io::Read,
    ptr::null_mut,
    rc::Rc,
    str::{from_utf8, from_utf8_unchecked},
};

use crate::{
    encoding::XmlCharEncoding,
    error::XmlParserErrors,
    io::XmlParserInputBuffer,
    libxml::{
        chvalid::{xml_is_char, xml_is_combining, xml_is_digit, xml_is_extender},
        parser::{
            xml_create_doc_parser_ctxt, xml_ctxt_reset, xml_free_parser_ctxt, xml_init_parser,
            xml_load_external_entity, xml_new_io_input_stream, xml_new_parser_ctxt,
            XmlParserOption,
        },
        parser_internals::{
            xml_create_memory_parser_ctxt, xml_create_url_parser_ctxt, xml_is_letter,
        },
    },
    tree::XmlDocPtr,
};

pub use context::*;
pub(crate) use error::*;
pub use input::*;
pub use node_info::*;
pub use qname::*;

pub(crate) trait XmlParserCharValid {
    fn is_name_char(&self, ctxt: &XmlParserCtxt) -> bool;
    // The two following functions are related to the change of accepted
    // characters for Name and NmToken in the Revision 5 of XML-1.0
    // They correspond to the modified production [4] and the new production [4a]
    // changes in that revision. Also note that the macros used for the
    // productions Letter, Digit, CombiningChar and Extender are not needed
    // anymore.
    // We still keep compatibility to pre-revision5 parsing semantic if the
    // new XML_PARSE_OLD10 option is given to the parser.
    fn is_name_start_char(&self, ctxt: &XmlParserCtxt) -> bool;
}

impl XmlParserCharValid for u8 {
    fn is_name_char(&self, ctxt: &XmlParserCtxt) -> bool {
        let c = *self;
        if ctxt.options & XmlParserOption::XmlParseOld10 as i32 == 0 {
            // Use the new checks of production [4] [4a] amd [5] of the
            // Update 5 of XML-1.0
            c != b' '
                && c != b'>'
                && c != b'/'
                && (c.is_ascii_lowercase()
                    || c.is_ascii_uppercase()
                    || c.is_ascii_digit()
                    || c == b'_'
                    || c == b':'
                    || c == b'-'
                    || c == b'.'
                    || c == 0xB7
                    || (0xC0..=0xD6).contains(&c)
                    || (0xD8..=0xF6).contains(&c)
                    || (0xF8..=0xFF).contains(&c))
        } else {
            xml_is_letter(c as u32)
                || xml_is_digit(c as u32)
                || c == b'.'
                || c == b'-'
                || c == b'_'
                || c == b':'
                || xml_is_combining(c as u32)
                || xml_is_extender(c as u32)
        }
    }

    fn is_name_start_char(&self, ctxt: &XmlParserCtxt) -> bool {
        let c = *self;
        if ctxt.options & XmlParserOption::XmlParseOld10 as i32 == 0 {
            // Use the new checks of production [4] [4a] amd [5] of the
            // Update 5 of XML-1.0
            c != b' '
                && c != b'>'
                && c != b'/'
                && (c.is_ascii_lowercase()
                    || c.is_ascii_uppercase()
                    || c == b'_'
                    || c == b':'
                    || (0xC0..=0xD6).contains(&c)
                    || (0xD8..=0xF6).contains(&c)
                    || (0xF8..=0xFF).contains(&c))
        } else {
            xml_is_letter(c as u32) || c == b'_' || c == b':'
        }
    }
}

impl XmlParserCharValid for u32 {
    fn is_name_char(&self, ctxt: &XmlParserCtxt) -> bool {
        let c = *self;
        if ctxt.options & XmlParserOption::XmlParseOld10 as i32 == 0 {
            // Use the new checks of production [4] [4a] amd [5] of the
            // Update 5 of XML-1.0
            c != b' ' as u32
                && c != b'>' as u32
                && c != b'/' as u32
                && ((c >= b'a' as u32 && c <= b'z' as u32)
                    || (c >= b'A' as u32 && c <= b'Z' as u32)
                    || (c >= b'0' as u32 && c <= b'9' as u32)
                    || c == b'_' as u32
                    || c == b':' as u32
                    || c == b'-' as u32
                    || c == b'.' as u32
                    || c == 0xB7
                    || (0xC0..=0xD6).contains(&c)
                    || (0xD8..=0xF6).contains(&c)
                    || (0xF8..=0x2FF).contains(&c)
                    || (0x300..=0x36F).contains(&c)
                    || (0x370..=0x37D).contains(&c)
                    || (0x37F..=0x1FFF).contains(&c)
                    || (0x200C..=0x200D).contains(&c)
                    || (0x203F..=0x2040).contains(&c)
                    || (0x2070..=0x218F).contains(&c)
                    || (0x2C00..=0x2FEF).contains(&c)
                    || (0x3001..=0xD7FF).contains(&c)
                    || (0xF900..=0xFDCF).contains(&c)
                    || (0xFDF0..=0xFFFD).contains(&c)
                    || (0x10000..=0xEFFFF).contains(&c))
        } else {
            xml_is_letter(c)
                || xml_is_digit(c)
                || c == b'.' as u32
                || c == b'-' as u32
                || c == b'_' as u32
                || c == b':' as u32
                || xml_is_combining(c)
                || xml_is_extender(c)
        }
    }

    fn is_name_start_char(&self, ctxt: &XmlParserCtxt) -> bool {
        let c = *self;
        if ctxt.options & XmlParserOption::XmlParseOld10 as i32 == 0 {
            // Use the new checks of production [4] [4a] amd [5] of the
            // Update 5 of XML-1.0
            c != b' ' as u32
                && c != b'>' as u32
                && c != b'/' as u32
                && ((c >= b'a' as u32 && c <= b'z' as u32)
                    || (c >= b'A' as u32 && c <= b'Z' as u32)
                    || c == b'_' as u32
                    || c == b':' as u32
                    || (0xC0..=0xD6).contains(&c)
                    || (0xD8..=0xF6).contains(&c)
                    || (0xF8..=0x2FF).contains(&c)
                    || (0x370..=0x37D).contains(&c)
                    || (0x37F..=0x1FFF).contains(&c)
                    || (0x200C..=0x200D).contains(&c)
                    || (0x2070..=0x218F).contains(&c)
                    || (0x2C00..=0x2FEF).contains(&c)
                    || (0x3001..=0xD7FF).contains(&c)
                    || (0xF900..=0xFDCF).contains(&c)
                    || (0xFDF0..=0xFFFD).contains(&c)
                    || (0x10000..=0xEFFFF).contains(&c))
        } else {
            xml_is_letter(c) || c == b'_' as u32 || c == b':' as u32
        }
    }
}

impl XmlParserCharValid for char {
    fn is_name_char(&self, ctxt: &XmlParserCtxt) -> bool {
        (*self as u32).is_name_char(ctxt)
    }

    fn is_name_start_char(&self, ctxt: &XmlParserCtxt) -> bool {
        (*self as u32).is_name_start_char(ctxt)
    }
}

/// The current c_char value, if using UTF-8 this may actually span multiple
/// bytes in the input buffer.
///
/// Returns the current c_char value and its length
#[doc(alias = "xmlStringCurrentChar")]
pub(crate) unsafe fn xml_string_current_char(
    ctxt: Option<&mut XmlParserCtxt>,
    cur: &[u8],
) -> Result<char, (u8, usize)> {
    assert!(!cur.is_empty());
    if ctxt
        .as_ref()
        .map_or(true, |ctxt| ctxt.charset == XmlCharEncoding::UTF8)
    {
        // We are supposed to handle UTF8, check it's valid
        // From rfc2044: encoding of the Unicode values on UTF-8:
        let cur = &cur[..cur.len().min(4)];
        let c = match from_utf8(cur) {
            Ok(s) => s.chars().next().ok_or((0, 0))?,
            Err(e) if e.valid_up_to() > 0 => {
                let s = from_utf8_unchecked(&cur[..e.valid_up_to()]);
                s.chars().next().ok_or((0, 0))?
            }
            Err(_) => {
                // An encoding problem may arise from a truncated input buffer
                // splitting a character in the middle. In that case do not raise
                // an error but return 0 to indicate an end of stream problem
                let Some(ctxt) =
                    ctxt.filter(|ctxt| !ctxt.input.is_null() && (*ctxt.input).remainder_len() >= 4)
                else {
                    return Err((0, 0));
                };

                // If we detect an UTF8 error that probably mean that the
                // input encoding didn't get properly advertised in the
                // declaration header. Report the error and switch the encoding
                // to ISO-Latin-1 (if you don't like this policy, just declare the encoding !)
                {
                    let buffer = format!(
                        "Bytes: 0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}\n",
                        *(*ctxt.input).cur.add(0),
                        *(*ctxt.input).cur.add(1),
                        *(*ctxt.input).cur.add(2),
                        *(*ctxt.input).cur.add(3),
                    );
                    __xml_err_encoding!(
                        ctxt as *mut XmlParserCtxt,
                        XmlParserErrors::XmlErrInvalidChar,
                        "Input is not proper UTF-8, indicate encoding !\n{}",
                        buffer
                    );
                }
                return Ok(cur[0] as char);
            }
        };
        if !xml_is_char(c as u32) {
            xml_err_encoding_int!(
                ctxt.map_or(null_mut(), |ctxt| ctxt as *mut XmlParserCtxt),
                XmlParserErrors::XmlErrInvalidChar,
                "Char 0x{:X} out of allowed range\n",
                c as i32
            );
        }
        return Ok(c);
    }
    // Assume it's a fixed length encoding (1) with
    // a compatible encoding for the ASCII set, since
    // XML constructs only use < 128 chars
    Ok(cur[0] as char)
}

/// Parse an XML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "xmlReadDoc")]
pub unsafe fn xml_read_doc(
    cur: *const u8,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> XmlDocPtr {
    if cur.is_null() {
        return null_mut();
    }
    xml_init_parser();

    let ctxt: XmlParserCtxtPtr = xml_create_doc_parser_ctxt(cur);
    if ctxt.is_null() {
        return null_mut();
    }
    let res = (*ctxt).do_read(url, encoding, options);
    xml_free_parser_ctxt(ctxt);
    res
}

/// Parse an XML file from the filesystem or the network.
///
/// Returns the resulting document tree
#[doc(alias = "xmlReadFile")]
pub unsafe fn xml_read_file(filename: &str, encoding: Option<&str>, options: i32) -> XmlDocPtr {
    xml_init_parser();
    let ctxt: XmlParserCtxtPtr = xml_create_url_parser_ctxt(Some(filename), options);
    if ctxt.is_null() {
        return null_mut();
    }
    let res = (*ctxt).do_read(None, encoding, options);
    xml_free_parser_ctxt(ctxt);
    res
}

/// Parse an XML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "xmlReadMemory")]
pub unsafe fn xml_read_memory(
    buffer: Vec<u8>,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> XmlDocPtr {
    xml_init_parser();
    let ctxt: XmlParserCtxtPtr = xml_create_memory_parser_ctxt(buffer);

    if ctxt.is_null() {
        return null_mut();
    }
    let res = (*ctxt).do_read(url, encoding, options);
    xml_free_parser_ctxt(ctxt);
    res
}

/// Parse an XML document from I/O functions and source and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "xmlReadIO")]
pub unsafe fn xml_read_io(
    ioctx: impl Read + 'static,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> XmlDocPtr {
    xml_init_parser();

    let input = XmlParserInputBuffer::from_reader(ioctx, XmlCharEncoding::None);
    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        return null_mut();
    }
    let stream: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, Rc::new(RefCell::new(input)), XmlCharEncoding::None);
    if stream.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }
    (*ctxt).input_push(stream);
    let res = (*ctxt).do_read(url, encoding, options);
    xml_free_parser_ctxt(ctxt);
    res
}

/// Parse an XML in-memory document and build a tree.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "xmlCtxtReadDoc")]
pub unsafe fn xml_ctxt_read_doc(
    ctxt: XmlParserCtxtPtr,
    cur: *const u8,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> XmlDocPtr {
    if cur.is_null() {
        return null_mut();
    }
    xml_ctxt_read_memory(
        ctxt,
        CStr::from_ptr(cur as *const i8).to_bytes().to_vec(),
        url,
        encoding,
        options,
    )
}

/// Parse an XML file from the filesystem or the network.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "xmlCtxtReadFile")]
pub unsafe fn xml_ctxt_read_file(
    ctxt: XmlParserCtxtPtr,
    filename: &str,
    encoding: Option<&str>,
    options: i32,
) -> XmlDocPtr {
    if ctxt.is_null() {
        return null_mut();
    }
    xml_init_parser();

    xml_ctxt_reset(ctxt);

    let stream: XmlParserInputPtr = xml_load_external_entity(Some(filename), None, ctxt);
    if stream.is_null() {
        return null_mut();
    }
    (*ctxt).input_push(stream);
    (*ctxt).do_read(None, encoding, options)
}

/// Parse an XML in-memory document and build a tree.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "xmlCtxtReadMemory")]
pub unsafe fn xml_ctxt_read_memory(
    ctxt: XmlParserCtxtPtr,
    buffer: Vec<u8>,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> XmlDocPtr {
    if ctxt.is_null() {
        return null_mut();
    }
    xml_init_parser();
    xml_ctxt_reset(ctxt);

    let Some(input) = XmlParserInputBuffer::from_memory(buffer, XmlCharEncoding::None) else {
        return null_mut();
    };
    let stream: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, Rc::new(RefCell::new(input)), XmlCharEncoding::None);
    if stream.is_null() {
        return null_mut();
    }

    (*ctxt).input_push(stream);
    (*ctxt).do_read(url, encoding, options)
}

/// Parse an XML document from I/O functions and source and build a tree.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "xmlCtxtReadIO")]
pub unsafe fn xml_ctxt_read_io(
    ctxt: XmlParserCtxtPtr,
    ioctx: impl Read + 'static,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> XmlDocPtr {
    if ctxt.is_null() {
        return null_mut();
    }
    xml_init_parser();
    xml_ctxt_reset(ctxt);

    let input = XmlParserInputBuffer::from_reader(ioctx, XmlCharEncoding::None);
    let stream: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, Rc::new(RefCell::new(input)), XmlCharEncoding::None);
    if stream.is_null() {
        return null_mut();
    }
    (*ctxt).input_push(stream);
    (*ctxt).do_read(url, encoding, options)
}

//! Provide methods and data structures for handling HTML 4.0 documents.
//!
//! This module is based on `libxml/HTMLparser.h`, `HTMLparser.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: interface for an HTML 4.0 non-verifying parser
// Description: this module implements an HTML 4.0 non-verifying parser
//              with API compatible with the XML parser ones. It should
//              be able to parse "real world" HTML, even if severely
//              broken from a specification point of view.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// HTMLparser.c : an HTML 4.0 non-verifying parser
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{
    borrow::Cow,
    cell::RefCell,
    ffi::{CStr, c_char},
    io::Read,
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null_mut},
    rc::Rc,
    str::{from_utf8, from_utf8_unchecked},
    sync::atomic::{AtomicI32, Ordering},
};

use libc::{INT_MAX, memcpy, memset, size_t};

use crate::{
    encoding::{XmlCharEncoding, detect_encoding, find_encoding_handler},
    error::{
        __xml_raise_error, XmlErrorDomain, XmlErrorLevel, XmlParserErrors, parser_validity_error,
        parser_validity_warning,
    },
    globals::{GenericErrorContext, get_keep_blanks_default_value, get_line_numbers_default_value},
    io::XmlParserInputBuffer,
    libxml::{
        chvalid::{
            xml_is_blank_char, xml_is_char, xml_is_combining, xml_is_digit, xml_is_extender,
            xml_is_pubid_char,
        },
        dict::{xml_dict_create, xml_dict_lookup},
        globals::{xml_default_sax_locator, xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
        parser::{XmlSAXHandler, xml_init_parser, xml_load_external_entity},
        parser_internals::{
            INPUT_CHUNK, XML_MAX_HUGE_LENGTH, XML_MAX_NAME_LENGTH, XML_MAX_TEXT_LENGTH,
            XML_VCTXT_USE_PCTXT, xml_is_letter,
        },
        sax2::{xml_sax2_ignorable_whitespace, xml_sax2_init_html_default_sax_handler},
        xmlstring::{XmlChar, xml_str_equal, xml_strndup},
    },
    parser::{
        XmlParserCtxt, XmlParserCtxtPtr, XmlParserInput, XmlParserInputState, XmlParserOption,
        xml_free_parser_ctxt,
    },
    tree::{NodeCommon, XmlElementType, XmlNodePtr, xml_create_int_subset, xml_free_doc},
    uri::canonic_path,
};

use super::{
    HtmlDocPtr, HtmlNodePtr, HtmlParserCtxt, HtmlParserCtxtPtr, HtmlParserInput,
    HtmlParserNodeInfo, HtmlSAXHandler, taginfo::*,
};

#[doc(alias = "htmlInitAutoClose")]
#[deprecated = "This is a no-op"]
pub fn html_init_auto_close() {}

/// Lookup the HTML tag in the ElementTable
///
/// Returns the related htmlElemDescPtr or null_mut() if not found.
#[doc(alias = "htmlTagLookup")]
pub fn html_tag_lookup(tag: &str) -> Option<&'static HtmlElemDesc> {
    let tag = tag.to_ascii_lowercase();
    HTML40_ELEMENT_TABLE
        .binary_search_by(|desc| desc.name.to_ascii_lowercase().cmp(&tag))
        .ok()
        .and_then(|pos| HTML40_ELEMENT_TABLE.get(pos))
}

/// Lookup the given entity in EntitiesTable
///
/// TODO: the linear scan is really ugly, an hash table is really needed.
///
/// Returns the associated htmlEntityDescPtr if found, NULL otherwise.
#[doc(alias = "htmlEntityLookup")]
pub fn html_entity_lookup(name: &str) -> Option<&'static HtmlEntityDesc> {
    HTML40_ENTITIES_TABLE
        .iter()
        .find(|entry| entry.name == name)
}

/// Lookup the given entity in EntitiesTable
///
/// TODO: the linear scan is really ugly, an hash table is really needed.
///
/// Returns the associated htmlEntityDescPtr if found, NULL otherwise.
#[doc(alias = "htmlEntityValueLookup")]
pub fn html_entity_value_lookup(value: u32) -> Option<&'static HtmlEntityDesc> {
    HTML40_ENTITIES_TABLE
        .binary_search_by_key(&value, |entry| entry.value)
        .ok()
        .and_then(|index| HTML40_ENTITIES_TABLE.get(index))
}

/// The HTML DTD allows a tag to implicitly close other tags.
/// The list is kept in htmlStartClose array. This function checks
/// if a tag is autoclosed by one of it's child
///
/// Returns 1 if autoclosed, 0 otherwise
#[doc(alias = "htmlIsAutoClosed")]
pub fn html_is_auto_closed(doc: HtmlDocPtr, elem: HtmlNodePtr) -> i32 {
    let mut child = elem.children().map(|c| XmlNodePtr::try_from(c).unwrap());
    while let Some(now) = child {
        if html_auto_close_tag(doc, elem.name().as_deref().unwrap(), now) != 0 {
            return 1;
        }
        child = now.next().map(|n| XmlNodePtr::try_from(n).unwrap());
    }
    0
}

/// Checks whether the new tag is one of the registered valid tags for closing old.
///
/// Returns 0 if no, 1 if yes.
#[doc(alias = "htmlCheckAutoClose")]
fn html_check_auto_close(newtag: &str, oldtag: &str) -> bool {
    HTML_START_CLOSE
        .binary_search_by(|entry| (entry.old_tag, entry.new_tag).cmp(&(oldtag, newtag)))
        .is_ok()
}

/// The HTML DTD allows a tag to implicitly close other tags.
/// The list is kept in htmlStartClose array. This function checks
/// if the element or one of it's children would autoclose the given tag.
///
/// Returns 1 if autoclose, 0 otherwise
#[doc(alias = "htmlAutoCloseTag")]
pub fn html_auto_close_tag(_doc: HtmlDocPtr, name: &str, elem: HtmlNodePtr) -> i32 {
    if name == elem.name().as_deref().unwrap() {
        return 0;
    }
    if html_check_auto_close(elem.name().as_deref().unwrap(), name) {
        return 1;
    }
    let mut child = elem.children().map(|c| XmlNodePtr::try_from(c).unwrap());
    while let Some(now) = child {
        if html_auto_close_tag(_doc, name, now) != 0 {
            return 1;
        }
        child = now.next().map(|n| XmlNodePtr::try_from(n).unwrap());
    }
    0
}

/*
 * Macros for accessing the content. Those should be used only by the parser,
 * and not exported.
 *
 * Clean macros, not dependent of an ASCII context, expect UTF-8 encoding
 *
 */

macro_rules! COPY_BUF {
    ($ctxt:expr, $l:expr, $b:expr, $i:expr, $v:expr) => {
        if $l == 1 {
            *$b.add($i as usize) = $v as _;
            $i += 1;
        } else {
            $i += crate::libxml::parser_internals::xml_copy_char(
                $l as _,
                $b.add($i as usize) as _,
                $v as _,
            );
        }
    };
}

/// Parse a content: comment, sub-element, reference or text.
/// This is the entry point when called from parser.c
#[doc(alias = "htmlParseContent")]
pub(crate) unsafe fn __html_parse_content(ctxt: *mut c_void) {
    unsafe {
        if !ctxt.is_null() {
            html_parse_content_internal(ctxt as HtmlParserCtxtPtr);
        }
    }
}

/// skip all blanks character found at that point in the input streams.
///
/// Returns the number of space chars skipped
#[doc(alias = "htmlSkipBlankChars")]
fn html_skip_blank_chars(ctxt: &mut XmlParserCtxt) -> i32 {
    let input = ctxt.input().unwrap();
    let mut line = input.line;
    let mut col = input.col;
    ctxt.force_grow();
    let mut buffer = ctxt.content_bytes();
    let mut res = 0;
    while !buffer.is_empty() && xml_is_blank_char(buffer[0] as u32) {
        if buffer[0] == b'\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
        buffer = &buffer[1..];
        if buffer.is_empty() {
            let len = ctxt.content_bytes().len();
            res += len;
            // commit input buffer
            let input = ctxt.input_mut().unwrap();
            input.cur += len;
            input.line = line;
            input.col = col;
            ctxt.force_grow();
            buffer = ctxt.content_bytes();
        }
    }

    let diff = ctxt.content_bytes().len() - buffer.len();
    res += diff;

    let input = ctxt.input_mut().unwrap();
    input.cur += diff;
    input.line = line;
    input.col = col;

    res as i32
}

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "htmlParseErrInt")]
macro_rules! html_parse_err_int {
    ($ctxt:expr, $error:expr, $msg:literal, $val:expr) => {
        if $ctxt.disable_sax == 0 || !matches!($ctxt.instate, XmlParserInputState::XmlParserEOF) {
            $ctxt.err_no = $error as i32;
            __xml_raise_error!(
                None,
                None,
                None,
                $ctxt as *mut XmlParserCtxt as _,
                None,
                XmlErrorDomain::XmlFromHTML,
                $error,
                XmlErrorLevel::XmlErrError,
                None,
                0,
                None,
                None,
                None,
                $val,
                0,
                Some(format!($msg, $val).as_str()),
            );
            $ctxt.well_formed = 0;
        }
    };
}

/// Ty to find and encoding in the current data available in the input
/// buffer this is needed to try to match to the proper encoding when
/// one face a character error.
/// That's an heuristic, since it's operating outside of parsing it could
/// try to use a meta which had been commented out, that's the reason it
/// should only be used in case of error, not as a default.
///
/// Returns an encoding string or NULL if not found, the string need to be freed
#[doc(alias = "htmlFindEncoding")]
unsafe fn html_find_encoding(ctxt: &mut XmlParserCtxt) -> Option<String> {
    unsafe {
        if ctxt.input().is_none_or(|input| {
            input.encoding.is_some()
                || input
                    .buf
                    .as_deref()
                    .is_none_or(|buf| buf.borrow().encoder.is_some())
        }) {
            return None;
        }
        if ctxt.content_bytes().is_empty() {
            return None;
        }

        const HTTP_EQUIV: &[u8] = b"HTTP-EQUIV";
        const CONTENT: &[u8] = b"CONTENT";
        const CHARSET: &[u8] = b"CHARSET=";

        let start = ctxt.content_bytes();
        let cur = start
            .windows(HTTP_EQUIV.len())
            .position(|chunk| chunk.eq_ignore_ascii_case(HTTP_EQUIV))
            .map(|pos| &start[pos..])?;
        let cur = cur
            .windows(CONTENT.len())
            .position(|chunk| chunk.eq_ignore_ascii_case(CONTENT))
            .map(|pos| &start[pos..])?;
        let cur = cur
            .windows(CHARSET.len())
            .position(|chunk| chunk.eq_ignore_ascii_case(CHARSET))
            .map(|pos| &start[pos..])?;
        let cur = &cur[CHARSET.len()..];
        let start = cur;
        let count = cur
            .iter()
            .position(|c| !c.is_ascii_alphanumeric() && !matches!(c, b'-' | b'_' | b':' | b'/'))
            .unwrap_or(cur.len());
        if count == 0 {
            return None;
        }
        // # Safety
        // `start[..count]` only contains ASCII alphanumeric, '-', '_', ':' or '/'.
        // Therefore, UTF-8 validation won't fail.
        Some(from_utf8_unchecked(&start[..count]).to_owned())
    }
}

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "htmlParseErr")]
unsafe fn html_parse_err(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
) {
    unsafe {
        if !ctxt.is_null()
            && (*ctxt).disable_sax != 0
            && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            return;
        }
        if !ctxt.is_null() {
            (*ctxt).err_no = error as i32;
        }
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            None,
            XmlErrorDomain::XmlFromHTML,
            error,
            XmlErrorLevel::XmlErrError,
            None,
            0,
            str1.map(|s| s.to_owned().into()),
            str2.map(|s| s.to_owned().into()),
            None,
            0,
            0,
            Some(msg),
        );
        if !ctxt.is_null() {
            (*ctxt).well_formed = 0;
        }
    }
}

/// The current char value, if using UTF-8 this may actually span multiple
/// bytes in the input buffer. Implement the end of line normalization:
/// 2.11 End-of-Line Handling
/// If the encoding is unspecified, in the case we find an ISO-Latin-1
/// char, then the encoding converter is plugged in automatically.
///
/// Returns the current char value and its length
#[doc(alias = "htmlCurrentChar")]
unsafe fn html_current_char(ctxt: &mut XmlParserCtxt, len: &mut i32) -> i32 {
    unsafe {
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return 0;
        }

        if ctxt.token != 0 {
            *len = 0;
            return ctxt.token;
        }

        if ctxt.input().unwrap().remainder_len() < INPUT_CHUNK && ctxt.force_grow() < 0 {
            return 0;
        }

        if ctxt.charset != XmlCharEncoding::UTF8 {
            // Assume it's a fixed length encoding (1) with
            // a compatible encoding for the ASCII set, since
            // HTML constructs only use < 128 chars
            if ctxt.current_byte() < 0x80 {
                *len = 1;
                if ctxt.current_byte() == 0 && !ctxt.content_bytes().is_empty() {
                    html_parse_err_int!(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidChar,
                        "Char 0x{:X} out of allowed range\n",
                        0
                    );
                    return b' ' as _;
                }
                return ctxt.current_byte() as _;
            }

            // Humm this is bad, do an automatic flow conversion
            if let Some(guess) = html_find_encoding(ctxt) {
                ctxt.input_mut().unwrap().encoding = Some(guess.clone());
                if let Some(handler) = find_encoding_handler(&guess) {
                    // Don't use UTF-8 encoder which isn't required and
                    // can produce invalid UTF-8.
                    if handler.name() != "UTF-8" {
                        ctxt.switch_to_encoding(handler);
                    }
                } else {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidEncoding,
                        format!("Unsupported encoding {guess}").as_str(),
                        Some(&guess),
                        None,
                    );
                }
            } else {
                ctxt.switch_encoding(XmlCharEncoding::ISO8859_1);
            }
            ctxt.charset = XmlCharEncoding::UTF8;
        }

        // We are supposed to handle UTF8, check it's valid
        // From rfc2044: encoding of the Unicode values on UTF-8:
        //
        // UCS-4 range (hex.)           UTF-8 octet sequence (binary)
        // 0000 0000-0000 007F   0xxxxxxx
        // 0000 0080-0000 07FF   110xxxxx 10xxxxxx
        // 0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
        //
        // Check for the 0x110000 limit too
        let content = ctxt.content_bytes();
        let l = 4.min(content.len());
        if l == 0 {
            *len = 0;
            return 0;
        }
        let c = match from_utf8(&content[..l]) {
            Ok(s) => {
                let c = s.chars().next().unwrap();
                *len = c.len_utf8() as i32;
                c
            }
            Err(e) if e.valid_up_to() > 0 => {
                let s = from_utf8_unchecked(&content[..e.valid_up_to()]);
                let c = s.chars().next().unwrap();
                *len = c.len_utf8() as i32;
                c
            }
            Err(e) => {
                match e.error_len() {
                    Some(l) => {
                        *len = l as i32;
                        // If we detect an UTF8 error that probably mean that the
                        // input encoding didn't get properly advertised in the
                        // declaration header. Report the error and switch the encoding
                        // to ISO-Latin-1 (if you don't like this policy, just declare the encoding !)
                        use std::fmt::Write as _;
                        let mut buffer = String::new();

                        if ctxt.input().unwrap().remainder_len() >= 4 {
                            let content = ctxt.content_bytes();
                            writeln!(
                                buffer,
                                "Bytes: 0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}",
                                content[0] as u32,
                                content[1] as u32,
                                content[2] as u32,
                                content[3] as u32,
                            )
                            .ok();
                        } else {
                            writeln!(buffer, "Bytes: 0x{:02X}", ctxt.current_byte() as u32,).ok();
                        }
                        html_parse_err(
                            ctxt,
                            XmlParserErrors::XmlErrInvalidEncoding,
                            "Input is not proper UTF-8, indicate encoding !\n",
                            Some(&buffer),
                            None,
                        );

                        // Don't match encodings twice. Note that if there's an encoder, we
                        // shouldn't receive invalid UTF-8 anyway.
                        //
                        // Note that if ctxt.input().unwrap().buf.is_null(), switching encodings is
                        // impossible, see Gitlab issue #34.
                        if ctxt.input().unwrap().buf.is_some()
                            && ctxt
                                .input()
                                .unwrap()
                                .buf
                                .as_ref()
                                .unwrap()
                                .borrow()
                                .encoder
                                .is_none()
                        {
                            ctxt.switch_encoding(XmlCharEncoding::ISO8859_1);
                        }
                        *len = 1;
                        return ctxt.current_byte() as i32;
                    }
                    None => {
                        *len = 0;
                        return 0;
                    }
                }
            }
        };

        if (*len > 1 && !xml_is_char(c as u32))
            || (*len == 1 && c == '\0' && !ctxt.content_bytes().is_empty())
        {
            html_parse_err_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                "Char 0x{:X} out of allowed range\n",
                c as i32
            );
        }

        c as i32
    }
}

#[doc(alias = "htmlParseNameComplex")]
unsafe fn html_parse_name_complex(ctxt: &mut XmlParserCtxt) -> Option<String> {
    unsafe {
        let mut l: i32 = 0;
        let mut c: i32;
        let max_length = if ctxt.options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_TEXT_LENGTH
        } else {
            XML_MAX_NAME_LENGTH
        };
        let charset = ctxt.charset;

        // Handler for more complex cases
        c = html_current_char(ctxt, &mut l);
        if c == b' ' as i32
            || c == b'>' as i32
            || c == b'/' as i32
            || (!xml_is_letter(c as u32) && c != b'_' as i32 && c != b':' as i32)
        {
            return None;
        }

        let mut ret = String::new();
        while c != b' ' as i32
            && c != b'>' as i32
            && c != b'/' as i32
            && (xml_is_letter(c as u32)
                || xml_is_digit(c as u32)
                || c == b'.' as i32
                || c == b'-' as i32
                || c == b'_' as i32
                || c == b':' as i32
                || xml_is_combining(c as u32)
                || xml_is_extender(c as u32))
        {
            ret.push(char::from_u32(c as u32).unwrap());
            if ret.len() > max_length {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrNameTooLong,
                    "name too long",
                    None,
                    None,
                );
                return None;
            }
            ctxt.advance_with_line_handling(l as usize);
            ctxt.token = 0;
            c = html_current_char(ctxt, &mut l);
            if ctxt.charset != charset {
                // I think this process is buggy, but I'll hold off on the original code...
                // The pointer that has been advanced is not rewound and begins reading again,
                // so the text read before reaching this point is ignored.

                // We changed encoding from an unknown encoding
                // Input buffer changed location, so we better start again
                return html_parse_name_complex(ctxt);
            }
        }
        if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }

        Some(ret)
    }
}

/// Parse an HTML name, this routine is case sensitive.
///
/// Returns the Name parsed or NULL
#[doc(alias = "htmlParseName")]
unsafe fn html_parse_name(ctxt: &mut HtmlParserCtxt) -> Option<String> {
    unsafe {
        ctxt.grow();

        // Accelerator for simple ASCII names
        let mut input = ctxt.content_bytes();
        if !input.is_empty() && (input[0].is_ascii_alphabetic() || matches!(input[0], b'_' | b':'))
        {
            input = &input[1..];
            // +1 : first character stripped the above
            let count = input
                .iter()
                .position(|&b| {
                    !b.is_ascii_alphanumeric() && !matches!(b, b'_' | b'-' | b':' | b'.')
                })
                .unwrap_or(input.len())
                + 1;

            if count == ctxt.content_bytes().len() {
                return None;
            }

            if (0x01..0x80).contains(&ctxt.content_bytes()[count]) {
                // # Safety
                // `ctxt.content_bytes()[..count]` only contains ASCII alphanumeric, '_', '-', ':' or '.'.
                // Therefore, UTF-8 validation won't fail.
                let ret = String::from_utf8_unchecked(ctxt.content_bytes()[..count].to_vec());
                ctxt.input_mut().unwrap().cur += count;
                ctxt.input_mut().unwrap().col += count as i32;
                return Some(ret);
            }
        }
        html_parse_name_complex(ctxt)
    }
}

/// Parse an HTML ENTITY references
///
/// [68] EntityRef ::= b'&' Name ';'
///
/// Returns the associated htmlEntityDescPtr if found, or NULL otherwise,
///         if non-NULL *str will have to be freed by the caller.
#[doc(alias = "htmlParseEntityRef")]
pub(crate) unsafe fn html_parse_entity_ref(
    ctxt: HtmlParserCtxtPtr,
    str: &mut Option<String>,
) -> Option<&'static HtmlEntityDesc> {
    unsafe {
        *str = None;
        if ctxt.is_null() || (*ctxt).input().is_none() {
            return None;
        }

        let mut ent = None;
        if (*ctxt).current_byte() == b'&' {
            (*ctxt).skip_char();
            if let Some(name) = html_parse_name(&mut *ctxt) {
                (*ctxt).grow();
                if (*ctxt).current_byte() == b';' {
                    // Lookup the entity in the table.
                    ent = html_entity_lookup(&name);
                    if ent.is_some() {
                        // OK that's ugly !!!
                        (*ctxt).skip_char();
                    }
                    *str = Some(name);
                } else {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrEntityRefSemicolMissing,
                        "htmlParseEntityRef: expecting ';'\n",
                        None,
                        None,
                    );
                    *str = Some(name);
                }
            } else {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    "htmlParseEntityRef: no name\n",
                    None,
                    None,
                );
            }
        }
        ent
    }
}

/// Parse Reference declarations
///
/// `[66] CharRef ::= b'&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'`
///
/// Returns the value parsed (as an int)
#[doc(alias = "htmlParseCharRef")]
pub(crate) unsafe fn html_parse_char_ref(ctxt: HtmlParserCtxtPtr) -> i32 {
    unsafe {
        let mut val: i32 = 0;

        if ctxt.is_null() || (*ctxt).input().is_none() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                "htmlParseCharRef: context error\n",
                None,
                None,
            );
            return 0;
        }
        if (*ctxt).current_byte() == b'&'
            && (*ctxt).nth_byte(1) == b'#'
            && ((*ctxt).nth_byte(2) == b'x' || (*ctxt).nth_byte(2) == b'X')
        {
            (*ctxt).advance(3);
            #[allow(clippy::while_immutable_condition)]
            while (*ctxt).current_byte() != b';' {
                if (*ctxt).current_byte() >= b'0' && (*ctxt).current_byte() <= b'9' {
                    if val < 0x110000 {
                        val = val * 16 + ((*ctxt).current_byte() - b'0') as i32;
                    }
                } else if (*ctxt).current_byte() >= b'a' && (*ctxt).current_byte() <= b'f' {
                    if val < 0x110000 {
                        val = val * 16 + ((*ctxt).current_byte() - b'a') as i32 + 10;
                    }
                } else if (*ctxt).current_byte() >= b'A' && (*ctxt).current_byte() <= b'F' {
                    if val < 0x110000 {
                        val = val * 16 + ((*ctxt).current_byte() - b'A') as i32 + 10;
                    }
                } else {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidHexCharRef,
                        "htmlParseCharRef: missing semicolon\n",
                        None,
                        None,
                    );
                    break;
                }
                (*ctxt).skip_char();
            }
            if (*ctxt).current_byte() == b';' {
                (*ctxt).skip_char();
            }
        } else if (*ctxt).content_bytes().starts_with(b"&#") {
            (*ctxt).advance(2);
            #[allow(clippy::while_immutable_condition)]
            while (*ctxt).current_byte() != b';' {
                if (*ctxt).current_byte().is_ascii_digit() {
                    if val < 0x110000 {
                        val = val * 10 + ((*ctxt).current_byte() - b'0') as i32;
                    }
                } else {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidDecCharRef,
                        "htmlParseCharRef: missing semicolon\n",
                        None,
                        None,
                    );
                    break;
                }
                (*ctxt).skip_char();
            }
            if (*ctxt).current_byte() == b';' {
                (*ctxt).skip_char();
            }
        } else {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrInvalidCharRef,
                "htmlParseCharRef: invalid value\n",
                None,
                None,
            );
        }
        // Check the value IS_CHAR ...
        if xml_is_char(val as u32) {
            return val;
        } else if val >= 0x110000 {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                "htmlParseCharRef: value too large\n",
                None,
                None,
            );
        } else {
            html_parse_err_int!(
                &mut *ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                "htmlParseCharRef: invalid xmlChar value {}\n",
                val
            );
        }
        0
    }
}

// const HTML_MAX_NAMELEN: usize = 1000;
const HTML_PARSER_BIG_BUFFER_SIZE: usize = 1000;
const HTML_PARSER_BUFFER_SIZE: usize = 100;

/// Handle a redefinition of attribute error
#[doc(alias = "htmlErrMemory")]
pub(crate) unsafe fn html_err_memory(ctxt: XmlParserCtxtPtr, extra: Option<&str>) {
    unsafe {
        if !ctxt.is_null()
            && (*ctxt).disable_sax != 0
            && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            return;
        }
        if !ctxt.is_null() {
            (*ctxt).err_no = XmlParserErrors::XmlErrNoMemory as i32;
            (*ctxt).instate = XmlParserInputState::XmlParserEOF;
            (*ctxt).disable_sax = 1;
        }
        if let Some(extra) = extra {
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                None,
                XmlErrorDomain::XmlFromParser,
                XmlParserErrors::XmlErrNoMemory,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                Some(extra.to_owned().into()),
                None,
                None,
                0,
                0,
                "Memory allocation failed : {}\n",
                extra
            );
        } else {
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                None,
                XmlErrorDomain::XmlFromParser,
                XmlParserErrors::XmlErrNoMemory,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                None,
                None,
                None,
                0,
                0,
                "Memory allocation failed\n",
            );
        }
    }
}

/// Parse an HTML tag or attribute name, note that we convert it to lowercase
/// since HTML names are not case-sensitive.
///
/// Returns the Tag Name parsed or NULL
#[doc(alias = "htmlParseHTMLName")]
unsafe fn html_parse_html_name(ctxt: HtmlParserCtxtPtr) -> *const XmlChar {
    unsafe {
        let mut i: usize = 0;
        let mut loc: [XmlChar; HTML_PARSER_BUFFER_SIZE] = [0; HTML_PARSER_BUFFER_SIZE];

        if !(*ctxt).current_byte().is_ascii_alphabetic()
            && (*ctxt).current_byte() != b'_'
            && (*ctxt).current_byte() != b':'
            && (*ctxt).current_byte() != b'.'
        {
            return null_mut();
        }

        while i < HTML_PARSER_BUFFER_SIZE
            && ((*ctxt).current_byte().is_ascii_alphabetic()
                || (*ctxt).current_byte().is_ascii_digit()
                || (*ctxt).current_byte() == b':'
                || (*ctxt).current_byte() == b'-'
                || (*ctxt).current_byte() == b'_'
                || (*ctxt).current_byte() == b'.')
        {
            if (*ctxt).current_byte() >= b'A' && (*ctxt).current_byte() <= b'Z' {
                loc[i] = (*ctxt).current_byte() + 0x20;
            } else {
                loc[i] = (*ctxt).current_byte();
            }
            i += 1;

            (*ctxt).skip_char();
        }

        let ret: *const XmlChar = xml_dict_lookup((*ctxt).dict, loc.as_ptr() as _, i as i32);
        if ret.is_null() {
            html_err_memory(ctxt, None);
        }

        ret
    }
}

/// Pops the top element name from the name stack
///
/// Returns the name just removed
#[doc(alias = "htmlnamePop")]
unsafe fn html_name_pop(ctxt: HtmlParserCtxtPtr) -> Option<String> {
    unsafe {
        let res = (*ctxt).name_tab.pop();
        let name = (*ctxt).name_tab.last().cloned();
        (*ctxt).name = name;
        res
    }
}

/// Close all remaining tags at the end of the stream
#[doc(alias = "htmlAutoCloseOnEnd")]
unsafe fn html_auto_close_on_end(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        if (*ctxt).name_tab.is_empty() {
            return;
        }
        for _ in (0..(*ctxt).name_tab.len()).rev() {
            if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
                let name = (*ctxt).name.as_deref().unwrap();
                end_element((*ctxt).user_data.clone(), name);
            }
            html_name_pop(ctxt);
        }
    }
}

/// The HTML DTD allows a tag to implicitly close other tags.
/// The list is kept in htmlStartClose array.
/// This function is called when a new tag has been detected and generates the
/// appropriates closes if possible/needed.
/// If newtag is NULL this mean we are at the end of the resource
/// and we should check
#[doc(alias = "htmlAutoClose")]
unsafe fn html_auto_close(ctxt: HtmlParserCtxtPtr, newtag: Option<&str>) {
    unsafe {
        if let Some(newtag) = newtag {
            while (*ctxt)
                .name
                .as_deref()
                .is_some_and(|name| html_check_auto_close(newtag, name))
            {
                if let Some(end_element) =
                    (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element)
                {
                    let name = (*ctxt).name.as_deref().unwrap();
                    end_element((*ctxt).user_data.clone(), name);
                }
                html_name_pop(ctxt);
            }
        }
        // Why do we return when newtag is None here,
        // and why do we also make newtag being None a continuation condition in the next while...?
        if newtag.is_none() {
            html_auto_close_on_end(ctxt);
            return;
        }
        while newtag.is_none()
            && (*ctxt)
                .name
                .as_deref()
                .is_some_and(|name| name == "head" || name == "body" || name == "html")
        {
            if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
                let name = (*ctxt).name.as_deref().unwrap();
                end_element((*ctxt).user_data.clone(), name);
            }
            html_name_pop(ctxt);
        }
    }
}

static HTML_OMITTED_DEFAULT_VALUE: AtomicI32 = AtomicI32::new(1);

/// Pushes a new element name on top of the name stack
///
/// Returns -1 in case of error, the index in the stack otherwise
#[doc(alias = "htmlnamePush")]
unsafe fn html_name_push(ctxt: HtmlParserCtxtPtr, value: &str) -> i32 {
    unsafe {
        if (*ctxt).html < 3 && value == "head" {
            (*ctxt).html = 3;
        }
        if (*ctxt).html < 10 && value == "body" {
            (*ctxt).html = 10;
        }

        (*ctxt).name = Some(value.to_owned());
        (*ctxt).name_tab.push(value.to_owned());
        (*ctxt).name_tab.len() as i32 - 1
    }
}

/// The HTML DTD allows a tag to exists only implicitly
/// called when a new tag has been detected and generates the
/// appropriates implicit tags if missing
#[doc(alias = "htmlCheckImplied")]
unsafe fn html_check_implied(ctxt: HtmlParserCtxtPtr, newtag: &str) {
    unsafe {
        if (*ctxt).options & HtmlParserOption::HtmlParseNoimplied as i32 != 0 {
            return;
        }
        if HTML_OMITTED_DEFAULT_VALUE.load(Ordering::Relaxed) == 0 {
            return;
        }
        if newtag == "html" {
            return;
        }
        if (*ctxt).name_tab.is_empty() {
            html_name_push(ctxt, "html");
            if let Some(start_element) =
                (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element)
            {
                start_element((*ctxt).user_data.clone(), "html", &[]);
            }
        }
        if newtag == "body" || newtag == "head" {
            return;
        }
        if (*ctxt).name_tab.len() <= 1
            && (newtag == "script"
                || newtag == "style"
                || newtag == "meta"
                || newtag == "link"
                || newtag == "title"
                || newtag == "base")
        {
            if (*ctxt).html >= 3 {
                // we already saw or generated an <head> before
                return;
            }
            // dropped OBJECT ... i you put it first BODY will be assumed !
            html_name_push(ctxt, "head");
            if let Some(start_element) =
                (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element)
            {
                start_element((*ctxt).user_data.clone(), "head", &[]);
            }
        } else if newtag != "noframes" && newtag != "frame" && newtag != "frameset" {
            if (*ctxt).html >= 10 {
                // we already saw or generated a <body> before
                return;
            }
            for i in 0..(*ctxt).name_tab.len() {
                if (*ctxt).name_tab[i] == "body" {
                    return;
                }
                if (*ctxt).name_tab[i] == "head" {
                    return;
                }
            }

            html_name_push(ctxt, "body");
            if let Some(start_element) =
                (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element)
            {
                start_element((*ctxt).user_data.clone(), "body", &[]);
            }
        }
    }
}

// Macro used to grow the current buffer.
macro_rules! grow_buffer {
    ($ctxt:expr, $buffer:expr, $buffer_size:expr) => {
        $buffer_size *= 2;
        let tmp: *mut XmlChar = xml_realloc($buffer as _, $buffer_size as usize) as _;
        if tmp.is_null() {
            html_err_memory($ctxt, Some("growing buffer\n"));
            xml_free($buffer as _);
            return null_mut();
        }
        $buffer = tmp;
    };
}

/// Parse an HTML attribute value till the stop (quote),
/// if stop is 0 then it stops at the first space
///
/// Returns the attribute parsed or NULL
#[doc(alias = "htmlParseHTMLAttribute")]
unsafe fn html_parse_html_attribute(ctxt: HtmlParserCtxtPtr, stop: u8) -> *mut XmlChar {
    unsafe {
        let mut buffer: *mut XmlChar;
        let mut buffer_size: i32;
        let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_HUGE_LENGTH as i32
        } else {
            XML_MAX_TEXT_LENGTH as i32
        };
        let mut out: *mut XmlChar;

        // allocate a translation buffer.
        buffer_size = HTML_PARSER_BUFFER_SIZE as _;
        buffer = xml_malloc_atomic(buffer_size as usize) as _;
        if buffer.is_null() {
            html_err_memory(ctxt, Some("buffer allocation failed\n"));
            return null_mut();
        }
        out = buffer;

        // Ok loop until we reach one of the ending chars
        while (*ctxt).current_byte() != 0 && (*ctxt).current_byte() != stop {
            if stop == 0 && (*ctxt).current_byte() == b'>' {
                break;
            }
            if stop == 0 && xml_is_blank_char((*ctxt).current_byte() as u32) {
                break;
            }
            if (*ctxt).current_byte() == b'&' {
                if (*ctxt).nth_byte(1) == b'#' {
                    let mut bits: i32;

                    let c: u32 = html_parse_char_ref(ctxt) as _;
                    if c < 0x80 {
                        *out = c as _;
                        out = out.add(1);
                        bits = -6;
                    } else if c < 0x800 {
                        *out = ((c >> 6) & 0x1F) as u8 | 0xC0;
                        out = out.add(1);
                        bits = 0;
                    } else if c < 0x10000 {
                        *out = ((c >> 12) & 0x0F) as u8 | 0xE0;
                        out = out.add(1);
                        bits = 6;
                    } else {
                        *out = ((c >> 18) & 0x07) as u8 | 0xF0;
                        out = out.add(1);
                        bits = 12;
                    }

                    while bits >= 0 {
                        *out = ((c >> bits) & 0x3F) as u8 | 0x80;
                        out = out.add(1);
                        bits -= 6;
                    }

                    if out.offset_from(buffer) > buffer_size as isize - 100 {
                        let indx: i32 = out.offset_from(buffer) as _;

                        grow_buffer!(ctxt, buffer, buffer_size);
                        out = buffer.add(indx as usize) as _;
                    }
                } else {
                    let mut name = None;
                    let ent = html_parse_entity_ref(ctxt, &mut name);
                    if let Some(name) = name {
                        if let Some(ent) = ent {
                            let mut bits: i32;

                            if out.offset_from(buffer) > buffer_size as isize - 100 {
                                let indx: i32 = out.offset_from(buffer) as i32;

                                grow_buffer!(ctxt, buffer, buffer_size);
                                out = buffer.add(indx as usize) as _;
                            }
                            let c: u32 = ent.value;
                            if c < 0x80 {
                                *out = c as _;
                                out = out.add(1);
                                bits = -6;
                            } else if c < 0x800 {
                                *out = ((c >> 6) & 0x1F) as u8 | 0xC0;
                                out = out.add(1);
                                bits = 0;
                            } else if c < 0x10000 {
                                *out = ((c >> 12) & 0x0F) as u8 | 0xE0;
                                out = out.add(1);
                                bits = 6;
                            } else {
                                *out = ((c >> 18) & 0x07) as u8 | 0xF0;
                                out = out.add(1);
                                bits = 12;
                            }

                            while bits >= 0 {
                                *out = ((c >> bits) & 0x3F) as u8 | 0x80;
                                out = out.add(1);
                                bits -= 6;
                            }
                        } else {
                            *out = b'&';
                            out = out.add(1);
                            for cur in name.bytes() {
                                if out.offset_from(buffer) > buffer_size as isize - 100 {
                                    let indx: i32 = out.offset_from(buffer) as i32;

                                    grow_buffer!(ctxt, buffer, buffer_size);
                                    out = buffer.add(indx as usize) as _;
                                }
                                *out = cur;
                                out = out.add(1);
                            }
                        }
                    } else {
                        *out = b'&';
                        out = out.add(1);
                        if out.offset_from(buffer) > buffer_size as isize - 100 {
                            let indx: i32 = out.offset_from(buffer) as i32;

                            grow_buffer!(ctxt, buffer, buffer_size);
                            out = buffer.add(indx as usize) as _;
                        }
                    }
                }
            } else {
                let mut bits: i32;
                let mut l: i32 = 0;

                if out.offset_from(buffer) > buffer_size as isize - 100 {
                    let indx: i32 = out.offset_from(buffer) as i32;

                    grow_buffer!(ctxt, buffer, buffer_size);
                    out = buffer.add(indx as usize) as _;
                }
                let c: u32 = html_current_char(&mut *ctxt, &mut l) as _;
                if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    xml_free(buffer as _);
                    return null_mut();
                }
                if c < 0x80 {
                    *out = c as _;
                    out = out.add(1);
                    bits = -6;
                } else if c < 0x800 {
                    *out = ((c >> 6) & 0x1F) as u8 | 0xC0;
                    out = out.add(1);
                    bits = 0;
                } else if c < 0x10000 {
                    *out = ((c >> 12) & 0x0F) as u8 | 0xE0;
                    out = out.add(1);
                    bits = 6;
                } else {
                    *out = ((c >> 18) & 0x07) as u8 | 0xF0;
                    out = out.add(1);
                    bits = 12;
                }

                while bits >= 0 {
                    *out = ((c >> bits) & 0x3F) as u8 | 0x80;
                    out = out.add(1);
                    bits -= 6;
                }
                (*ctxt).advance_with_line_handling(l as usize);
                (*ctxt).token = 0;
            }
            if out.offset_from(buffer) > max_length as isize {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrAttributeNotFinished,
                    "attribute value too long\n",
                    None,
                    None,
                );
                xml_free(buffer as _);
                return null_mut();
            }
        }
        *out = 0;
        buffer
    }
}

/// parse a value for an attribute
///
/// # Note
/// The parser won't do substitution of entities here, this
/// will be handled later in xmlStringGetNodeList, unless it was
/// asked for (*ctxt).replaceEntities != 0
///
/// Returns the AttValue parsed or NULL.
#[doc(alias = "htmlParseAttValue")]
unsafe fn html_parse_att_value(ctxt: HtmlParserCtxtPtr) -> *mut XmlChar {
    unsafe {
        let ret: *mut XmlChar;

        if (*ctxt).current_byte() == b'"' {
            (*ctxt).skip_char();
            ret = html_parse_html_attribute(ctxt, b'"');
            if (*ctxt).current_byte() != b'"' {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrAttributeNotFinished,
                    "AttValue: \" expected\n",
                    None,
                    None,
                );
            } else {
                (*ctxt).skip_char();
            }
        } else if (*ctxt).current_byte() == b'\'' {
            (*ctxt).skip_char();
            ret = html_parse_html_attribute(ctxt, b'\'');
            if (*ctxt).current_byte() != b'\'' {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrAttributeNotFinished,
                    "AttValue: ' expected\n",
                    None,
                    None,
                );
            } else {
                (*ctxt).skip_char();
            }
        } else {
            // That's an HTMLism, the attribute value may not be quoted
            ret = html_parse_html_attribute(ctxt, 0);
            if ret.is_null() {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrAttributeWithoutValue,
                    "AttValue: no value found\n",
                    None,
                    None,
                );
            }
        }
        ret
    }
}

/// Parse an attribute
///
/// `[41] Attribute ::= Name Eq AttValue`
///
/// `[25] Eq ::= S? '=' S?`
///
/// With namespace:
///
/// `[NS 11] Attribute ::= QName Eq AttValue`
///
/// Also the case QName == xmlns:??? is handled independently as a namespace definition.
///
/// Returns the attribute name, and the value in *value.
#[doc(alias = "htmlParseAttribute")]
unsafe fn html_parse_attribute(
    ctxt: HtmlParserCtxtPtr,
    value: *mut *mut XmlChar,
) -> *const XmlChar {
    unsafe {
        let mut val: *mut XmlChar = null_mut();

        *value = null_mut();
        let name: *const XmlChar = html_parse_html_name(ctxt);
        if name.is_null() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "error parsing attribute name\n",
                None,
                None,
            );
            return null_mut();
        }

        // read the value
        html_skip_blank_chars(&mut *ctxt);
        if (*ctxt).current_byte() == b'=' {
            (*ctxt).skip_char();
            html_skip_blank_chars(&mut *ctxt);
            val = html_parse_att_value(ctxt);
        }

        *value = val;
        name
    }
}

/// Checks an attribute value to detect the encoding
/// If a new encoding is detected the parser is switched to decode it and pass UTF8
#[doc(alias = "htmlCheckEncodingDirect")]
unsafe fn html_check_encoding_direct(ctxt: HtmlParserCtxtPtr, encoding: Option<&str>) {
    unsafe {
        if ctxt.is_null()
            || encoding.is_none()
            || (*ctxt).options & HtmlParserOption::HtmlParseIgnoreEnc as i32 != 0
        {
            return;
        }

        // do not change encoding
        if (*ctxt).input().unwrap().encoding.is_some() {
            return;
        }

        if let Some(mut encoding) = encoding {
            encoding = encoding.trim_start_matches([' ', '\t']);
            (*ctxt).input_mut().unwrap().encoding = Some(encoding.to_owned());

            let enc = encoding
                .parse::<XmlCharEncoding>()
                .unwrap_or(XmlCharEncoding::Error);
            // registered set of known encodings
            if !matches!(enc, XmlCharEncoding::Error) {
                if matches!(
                    enc,
                    XmlCharEncoding::UTF16LE
                        | XmlCharEncoding::UTF16BE
                        | XmlCharEncoding::UCS4LE
                        | XmlCharEncoding::UCS4BE
                ) && (*ctxt).input().unwrap().buf.is_some()
                    && (*ctxt)
                        .input()
                        .unwrap()
                        .buf
                        .as_ref()
                        .unwrap()
                        .borrow()
                        .encoder
                        .is_none()
                {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidEncoding,
                        "htmlCheckEncoding: wrong encoding meta\n",
                        None,
                        None,
                    );
                } else {
                    (*ctxt).switch_encoding(enc);
                }
                (*ctxt).charset = XmlCharEncoding::UTF8;
            } else {
                // fallback for unknown encodings
                if let Some(handler) = find_encoding_handler(encoding) {
                    (*ctxt).switch_to_encoding(handler);
                    (*ctxt).charset = XmlCharEncoding::UTF8;
                } else {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrUnsupportedEncoding,
                        format!("htmlCheckEncoding: unknown encoding {encoding}\n").as_str(),
                        Some(encoding),
                        None,
                    );
                }
            }

            if (*ctxt)
                .input()
                .unwrap()
                .buf
                .as_deref()
                .map(|buf| buf.borrow())
                .is_some_and(|buf| {
                    buf.encoder.is_some() && buf.buffer.is_some() && buf.raw.is_some()
                })
            {
                // convert as much as possible to the parser reading buffer.
                let processed = (*ctxt).input().unwrap().offset_from_base();
                (*ctxt)
                    .input_mut()
                    .unwrap()
                    .buf
                    .as_mut()
                    .unwrap()
                    .borrow_mut()
                    .buffer
                    .unwrap()
                    .trim_head(processed);
                let res = (*ctxt)
                    .input_mut()
                    .unwrap()
                    .buf
                    .as_mut()
                    .unwrap()
                    .borrow_mut()
                    .decode(true);
                (*ctxt).input_mut().unwrap().reset_base();
                if res.is_err() {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidEncoding,
                        "htmlCheckEncoding: encoder error\n",
                        None,
                        None,
                    );
                }
            }
        }
    }
}

/// Checks an http-equiv attribute from a Meta tag to detect the encoding
/// If a new encoding is detected the parser is switched to decode it and pass UTF8
#[doc(alias = "htmlCheckEncoding")]
unsafe fn html_check_encoding(ctxt: HtmlParserCtxtPtr, attvalue: &str) {
    unsafe {
        let mut encoding = attvalue;
        let Some(pos) = attvalue
            .as_bytes()
            .windows(7)
            .position(|v| v.eq_ignore_ascii_case(b"charset"))
        else {
            return;
        };
        encoding = &encoding[pos + 7..];
        encoding = encoding.trim_start_matches(|c| xml_is_blank_char(c as u32));
        if let Some(encoding) = encoding.strip_prefix('=') {
            html_check_encoding_direct(ctxt, Some(encoding));
        }
    }
}

/// Checks an attributes from a Meta tag
#[doc(alias = "htmlCheckMeta")]
unsafe fn html_check_meta(ctxt: HtmlParserCtxtPtr, atts: &[(String, Option<String>)]) {
    unsafe {
        let mut http: i32 = 0;

        if ctxt.is_null() {
            return;
        }

        let mut content = None;
        for (att, value) in atts {
            if value
                .as_deref()
                .is_some_and(|v| v.eq_ignore_ascii_case("Content-Type"))
                && att.eq_ignore_ascii_case("http-equiv")
            {
                http = 1;
            } else if value.is_some() && att.eq_ignore_ascii_case("charset") {
                html_check_encoding_direct(ctxt, value.as_deref());
            } else if value.is_some() && att.eq_ignore_ascii_case("content") {
                content = value.as_deref();
            }
        }
        if let Some(content) = content.filter(|_| http != 0) {
            html_check_encoding(ctxt, content);
        }
    }
}

/// Parse a start of tag either for rule element or EmptyElement.  
/// In both case we don't parse the tag closing chars.
///
/// `[40] STag ::= b'<' Name (S Attribute)* S? '>'`
///
/// `[44] EmptyElemTag ::= b'<' Name (S Attribute)* S? '/>'`
///
/// With namespace:
///
/// `[NS 8] STag ::= b'<' QName (S Attribute)* S? '>'`
///
/// `[NS 10] EmptyElement ::= b'<' QName (S Attribute)* S? '/>'`
///
/// Returns 0 in case of success, -1 in case of error and 1 if discarded
#[doc(alias = "htmlParseStartTag")]
unsafe fn html_parse_start_tag(ctxt: HtmlParserCtxtPtr) -> i32 {
    unsafe {
        let mut attname: *const XmlChar;
        let mut attvalue: *mut XmlChar = null_mut();
        let mut meta: i32 = 0;
        let mut discardtag: i32 = 0;

        if ctxt.is_null() || (*ctxt).input().is_none() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                "htmlParseStartTag: context error\n",
                None,
                None,
            );
            return -1;
        }
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            return -1;
        }
        if (*ctxt).current_byte() != b'<' {
            return -1;
        }
        (*ctxt).skip_char();

        (*ctxt).grow();
        let name: *const XmlChar = html_parse_html_name(ctxt);
        if name.is_null() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "htmlParseStartTag: invalid element name\n",
                None,
                None,
            );
            // Dump the bogus tag like browsers do
            #[allow(clippy::while_immutable_condition)]
            while (*ctxt).current_byte() != 0
                && (*ctxt).current_byte() != b'>'
                && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
            {
                (*ctxt).skip_char();
            }
            return -1;
        }
        if xml_str_equal(name, c"meta".as_ptr() as _) {
            meta = 1;
        }

        // Check for auto-closure of HTML elements.
        html_auto_close(
            ctxt,
            Some(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref()),
        );

        // Check for implied HTML elements.
        html_check_implied(
            ctxt,
            CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
        );

        // Avoid html at any level > 0, head at any level != 1
        // or any attempt to recurse body
        if !(*ctxt).name_tab.is_empty() && xml_str_equal(name, c"html".as_ptr() as _) {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlHTMLStrucureError,
                "htmlParseStartTag: misplaced <html> tag\n",
                Some(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref()),
                None,
            );
            discardtag = 1;
            (*ctxt).depth += 1;
        }
        if (*ctxt).name_tab.len() != 1 && xml_str_equal(name, c"head".as_ptr() as _) {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlHTMLStrucureError,
                "htmlParseStartTag: misplaced <head> tag\n",
                Some(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref()),
                None,
            );
            discardtag = 1;
            (*ctxt).depth += 1;
        }
        if xml_str_equal(name, c"body".as_ptr() as _) {
            for indx in 0..(*ctxt).name_tab.len() {
                if (*ctxt).name_tab[indx] == "body" {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlHTMLStrucureError,
                        "htmlParseStartTag: misplaced <body> tag\n",
                        Some(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref()),
                        None,
                    );
                    discardtag = 1;
                    (*ctxt).depth += 1;
                }
            }
        }

        // Now parse the attributes, it ends up with the ending
        //
        // (S Attribute)* S?
        html_skip_blank_chars(&mut *ctxt);
        'failed: while (*ctxt).current_byte() != 0
            && (*ctxt).current_byte() != b'>'
            && !(*ctxt).content_bytes().starts_with(b"/>")
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            (*ctxt).grow();
            attname = html_parse_attribute(ctxt, addr_of_mut!(attvalue));
            if !attname.is_null() {
                let attname = CStr::from_ptr(attname as *const i8)
                    .to_string_lossy()
                    .into_owned();
                // Well formedness requires at most one declaration of an attribute
                for i in 0..(*ctxt).atts.len() {
                    let (name, _) = &(*ctxt).atts[i];
                    if name.as_str() == attname {
                        html_parse_err(
                            ctxt,
                            XmlParserErrors::XmlErrAttributeRedefined,
                            format!("Attribute {attname} redefined\n").as_str(),
                            Some(&attname),
                            None,
                        );
                        if !attvalue.is_null() {
                            xml_free(attvalue as _);
                        }
                        // goto failed;
                        html_skip_blank_chars(&mut *ctxt);
                        continue 'failed;
                    }
                }

                // Add the pair to atts
                let value = (!attvalue.is_null()).then(|| {
                    CStr::from_ptr(attvalue as *const i8)
                        .to_string_lossy()
                        .into_owned()
                });
                (*ctxt).atts.push((attname, value));
                xml_free(attvalue as _);
            } else {
                if !attvalue.is_null() {
                    xml_free(attvalue as _);
                }
                // Dump the bogus attribute string up to the next blank or the end of the tag.
                while (*ctxt).current_byte() != 0
                    && !xml_is_blank_char((*ctxt).current_byte() as u32)
                    && (*ctxt).current_byte() != b'>'
                    && !(*ctxt).content_bytes().starts_with(b"/>")
                    && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
                {
                    (*ctxt).skip_char();
                }
            }
            // failed:
            html_skip_blank_chars(&mut *ctxt);
        }

        // Handle specific association to the META tag
        if meta != 0 && !(*ctxt).atts.is_empty() {
            html_check_meta(ctxt, &(*ctxt).atts);
        }

        // SAX: Start of Element !
        if discardtag == 0 {
            html_name_push(ctxt, &CStr::from_ptr(name as *const i8).to_string_lossy());
            if let Some(start_element) =
                (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element)
            {
                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                start_element((*ctxt).user_data.clone(), &name, &(*ctxt).atts);
            }
        }

        (*ctxt).atts.clear();
        discardtag
    }
}

/// Return value: The "endtag" priority.
#[doc(alias = "htmlGetEndPriority")]
fn html_get_end_priority(name: &str) -> i32 {
    HTML_END_PRIORITY
        .iter()
        .find_map(|entry| (entry.name == name || entry.name.is_empty()).then_some(entry.priority))
        .unwrap()
}

/// The HTML DTD allows an ending tag to implicitly close other tags.
#[doc(alias = "htmlAutoCloseOnClose")]
unsafe fn html_auto_close_on_close(ctxt: HtmlParserCtxtPtr, newtag: &str) {
    unsafe {
        let priority = html_get_end_priority(newtag);

        for i in (0..(*ctxt).name_tab.len()).rev() {
            if newtag == (*ctxt).name_tab[i] {
                while Some(newtag) != (*ctxt).name.as_deref() {
                    let info = (*ctxt)
                        .name
                        .as_deref()
                        .and_then(|name| html_tag_lookup(name));
                    if info.filter(|info| info.end_tag == 3).is_some() {
                        let name = (*ctxt).name.as_deref().unwrap();
                        html_parse_err(
                            ctxt,
                            XmlParserErrors::XmlErrTagNameMismatch,
                            format!("Opening and ending tag mismatch: {newtag} and {name}\n")
                                .as_str(),
                            Some(newtag),
                            Some(name),
                        );
                    }
                    if let Some(end_element) =
                        (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element)
                    {
                        let name = (*ctxt).name.as_deref().unwrap();
                        end_element((*ctxt).user_data.clone(), name);
                    }
                    html_name_pop(ctxt);
                }

                return;
            }
            // A misplaced endtag can only close elements with lower
            // or equal priority, so if we find an element with higher
            // priority before we find an element with
            // matching name, we just ignore this endtag
            if html_get_end_priority(&(*ctxt).name_tab[i]) > priority {
                return;
            }
        }
    }
}

/// Pops the top element name from the node info stack
///
/// Returns 0 in case of error, the pointer to NodeInfo otherwise
#[doc(alias = "htmlNodeInfoPop")]
unsafe fn html_node_info_pop(ctxt: HtmlParserCtxtPtr) -> Option<Rc<RefCell<HtmlParserNodeInfo>>> {
    unsafe { (*ctxt).node_info_tab.pop() }
}

/// Parse an end of tag
///
/// `[42] ETag ::= b'</' Name S? '>'`
///
/// With namespace
///
/// `[NS 9] ETag ::= b'</' QName S? '>'`
///
/// Returns 1 if the current level should be closed.
#[doc(alias = "htmlParseEndTag")]
unsafe fn html_parse_end_tag(ctxt: HtmlParserCtxtPtr) -> i32 {
    unsafe {
        let ret: i32;

        if !(*ctxt).content_bytes().starts_with(b"</") {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrLtSlashRequired,
                "htmlParseEndTag: '</' not found\n",
                None,
                None,
            );
            return 0;
        }
        (*ctxt).advance(2);

        let name: *const XmlChar = html_parse_html_name(ctxt);
        if name.is_null() {
            return 0;
        }
        // We should definitely be at the ending "S? '>'" part
        html_skip_blank_chars(&mut *ctxt);
        if (*ctxt).current_byte() != b'>' {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrGtRequired,
                "End tag : expected '>'\n",
                None,
                None,
            );
            // Skip to next '>'
            #[allow(clippy::while_immutable_condition)]
            while (*ctxt).current_byte() != 0 && (*ctxt).current_byte() != b'>' {
                (*ctxt).skip_char();
            }
        }
        if (*ctxt).current_byte() == b'>' {
            (*ctxt).skip_char();
        }

        // if we ignored misplaced tags in htmlParseStartTag don't pop them out now.
        if (*ctxt).depth > 0
            && (xml_str_equal(name as _, c"html".as_ptr() as _)
                || xml_str_equal(name as _, c"body".as_ptr() as _)
                || xml_str_equal(name as _, c"head".as_ptr() as _))
        {
            (*ctxt).depth -= 1;
            return 0;
        }

        // If the name read is not one of the element in the parsing stack
        // then return, it's just an error.
        for i in (0..(*ctxt).name_tab.len()).rev() {
            if CStr::from_ptr(name as *const i8).to_string_lossy() == (*ctxt).name_tab[i] {
                // Check for auto-closure of HTML elements.

                html_auto_close_on_close(
                    ctxt,
                    CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
                );

                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                // Well formedness constraints, opening and closing must match.
                // With the exception that the autoclose may have popped stuff out of the stack.
                if let Some(ctxt_name) = (*ctxt)
                    .name
                    .as_deref()
                    .filter(|&ctxt_name| ctxt_name != name)
                {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrTagNameMismatch,
                        format!("Opening and ending tag mismatch: {name} and {ctxt_name}\n")
                            .as_str(),
                        Some(&name),
                        Some(ctxt_name),
                    );
                }

                // SAX: End of Tag
                let oldname = (*ctxt).name.as_deref();
                if oldname.is_some_and(|oldname| oldname == name) {
                    if let Some(end_element) =
                        (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element)
                    {
                        end_element((*ctxt).user_data.clone(), &name);
                    }
                    html_node_info_pop(ctxt);
                    html_name_pop(ctxt);
                    ret = 1;
                } else {
                    ret = 0;
                }

                return ret;
            }
        }
        let name = CStr::from_ptr(name as *const i8).to_string_lossy();
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrTagNameMismatch,
            format!("Unexpected end tag : {name}\n").as_str(),
            Some(&name),
            None,
        );
        0
    }
}

/// Parse an HTML tag or attribute name, note that we convert it to lowercase
/// since HTML names are not case-sensitive, this doesn't consume the data
/// from the stream, it's a look-ahead
///
/// Returns the Tag Name parsed or NULL
#[doc(alias = "htmlParseHTMLName_nonInvasive")]
unsafe fn html_parse_html_name_non_invasive(ctxt: HtmlParserCtxtPtr) -> *const XmlChar {
    unsafe {
        let mut i: usize = 0;
        let mut loc: [XmlChar; HTML_PARSER_BUFFER_SIZE] = [0; HTML_PARSER_BUFFER_SIZE];

        let next = (*ctxt).nth_byte(1);
        if !next.is_ascii_alphabetic() && next != b'_' && next != b':' {
            return null_mut();
        }

        while i < HTML_PARSER_BUFFER_SIZE
            && ((*ctxt).nth_byte(1 + i).is_ascii_alphabetic()
                || (*ctxt).nth_byte(1 + i).is_ascii_digit()
                || (*ctxt).nth_byte(1 + i) == b':'
                || (*ctxt).nth_byte(1 + i) == b'-'
                || (*ctxt).nth_byte(1 + i) == b'_')
        {
            if (*ctxt).nth_byte(1 + i) >= b'A' && (*ctxt).nth_byte(1 + i) <= b'Z' {
                loc[i] = (*ctxt).nth_byte(1 + i) + 0x20;
            } else {
                loc[i] = (*ctxt).nth_byte(1 + i);
            }
            i += 1;
        }

        xml_dict_lookup((*ctxt).dict, loc.as_ptr(), i as _)
    }
}

/// Parse the content of an HTML SCRIPT or STYLE element
/// http://www.w3.org/TR/html4/sgml/dtd.html#Script
/// http://www.w3.org/TR/html4/sgml/dtd.html#StyleSheet
/// http://www.w3.org/TR/html4/types.html#type-script
/// http://www.w3.org/TR/html4/types.html#h-6.15
/// http://www.w3.org/TR/html4/appendix/notes.html#h-B.3.2.1
///
/// Script data ( %Script; in the DTD) can be the content of the SCRIPT
/// element and the value of intrinsic event attributes. User agents must
/// not evaluate script data as HTML markup but instead must pass it on as
/// data to a script engine.
///
/// # Note
/// - The content is passed like CDATA
/// - the attributes for style and scripting "onXXX" are also described
///   as CDATA but SGML allows entities references in attributes so their
///   processing is identical as other attributes
#[doc(alias = "htmlParseScript")]
unsafe fn html_parse_script(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        let mut buf: [XmlChar; HTML_PARSER_BIG_BUFFER_SIZE + 5] =
            [0; HTML_PARSER_BIG_BUFFER_SIZE + 5];
        let mut nbchar: i32 = 0;
        let mut cur: i32;
        let mut l: i32 = 0;

        cur = html_current_char(&mut *ctxt, &mut l);
        while cur != 0 {
            if cur == b'<' as i32 && (*ctxt).nth_byte(1) == b'/' {
                // One should break here, the specification is clear:
                // Authors should therefore escape "</" within the content.
                // Escape mechanisms are specific to each scripting or
                // style sheet language.
                //
                // In recovery mode, only break if end tag match the
                // current tag, effectively ignoring all tags inside the
                // script/style block and treating the entire block as
                // CDATA.
                if (*ctxt).recovery != 0 {
                    let context_name = (*ctxt).name.as_deref().unwrap();
                    let content = &(*ctxt).content_bytes()[2..];
                    if context_name.len() <= content.len()
                        && content[..context_name.len()]
                            .eq_ignore_ascii_case(context_name.as_bytes())
                    {
                        break;
                    } else {
                        let name = (*ctxt).name.as_deref().unwrap();
                        html_parse_err(
                            ctxt,
                            XmlParserErrors::XmlErrTagNameMismatch,
                            format!("Element {name} embeds close tag\n").as_str(),
                            Some(name),
                            None,
                        );
                    }
                } else if ((*ctxt).nth_byte(2) >= b'A' && (*ctxt).nth_byte(2) <= b'Z')
                    || ((*ctxt).nth_byte(2) >= b'a' && (*ctxt).nth_byte(2) <= b'z')
                {
                    break;
                }
            }
            if xml_is_char(cur as u32) {
                COPY_BUF!(ctxt, l, buf.as_mut_ptr(), nbchar, cur);
            } else {
                html_parse_err_int!(
                    &mut *ctxt,
                    XmlParserErrors::XmlErrInvalidChar,
                    "Invalid char in CDATA 0x{:X}\n",
                    cur
                );
            }
            (*ctxt).advance_with_line_handling(l as usize);
            (*ctxt).token = 0;
            if nbchar >= HTML_PARSER_BIG_BUFFER_SIZE as i32 {
                buf[nbchar as usize] = 0;
                let s = from_utf8(&buf[..nbchar as usize]).expect("Internal Error");
                if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                    if let Some(cdata_block) = sax.cdata_block {
                        // Insert as CDATA, which is the same as HTML_PRESERVE_NODE
                        cdata_block((*ctxt).user_data.clone(), s);
                    } else if let Some(characters) = sax.characters {
                        characters((*ctxt).user_data.clone(), s);
                    }
                }
                nbchar = 0;
                (*ctxt).shrink();
            }
            cur = html_current_char(&mut *ctxt, &mut l);
        }

        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            return;
        }

        if nbchar != 0 && (*ctxt).disable_sax == 0 {
            if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                buf[nbchar as usize] = 0;
                let s = from_utf8(&buf[..nbchar as usize]).expect("Internal Error");
                if let Some(cdata_block) = sax.cdata_block {
                    // Insert as CDATA, which is the same as HTML_PRESERVE_NODE
                    cdata_block((*ctxt).user_data.clone(), s);
                } else if let Some(characters) = sax.characters {
                    characters((*ctxt).user_data.clone(), s);
                }
            }
        }
    }
}

/// Parse an HTML Literal
///
/// `[11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")`
///
/// Returns the SystemLiteral parsed or NULL
#[doc(alias = "htmlParseSystemLiteral")]
unsafe fn html_parse_system_literal(ctxt: HtmlParserCtxtPtr) -> *mut XmlChar {
    unsafe {
        let mut len: size_t = 0;
        let mut err: i32 = 0;

        let mut ret: *mut XmlChar = null_mut();

        if (*ctxt).current_byte() != b'"' && (*ctxt).current_byte() != b'\'' {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrLiteralNotStarted,
                "SystemLiteral \" or ' expected\n",
                None,
                None,
            );
            return null_mut();
        }
        let quote = (*ctxt).current_byte() as i32;
        (*ctxt).skip_char();

        // if (*ctxt).input().unwrap().cur < (*ctxt).input().unwrap().base {
        //     return ret;
        // }
        let start_position = (*ctxt).input().unwrap().offset_from_base();

        while (*ctxt).current_byte() != 0 && (*ctxt).current_byte() as i32 != quote {
            // TODO: Handle UTF-8
            if !xml_is_char((*ctxt).current_byte() as u32) {
                html_parse_err_int!(
                    &mut *ctxt,
                    XmlParserErrors::XmlErrInvalidChar,
                    "Invalid char in SystemLiteral 0x{:X}\n",
                    (*ctxt).current_byte() as i32
                );
                err = 1;
            }
            (*ctxt).skip_char();
            len += 1;
        }
        if (*ctxt).current_byte() as i32 != quote {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrLiteralNotFinished,
                "Unfinished SystemLiteral\n",
                None,
                None,
            );
        } else {
            if err == 0 {
                let content = &(*ctxt).input().unwrap().base[start_position..start_position + len];
                ret = xml_strndup(content.as_ptr(), content.len() as i32);
            }
            (*ctxt).skip_char();
        }

        ret
    }
}

/// Parse an HTML public literal
///
/// `[12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"`
///
/// Returns the PubidLiteral parsed or NULL.
#[doc(alias = "htmlParsePubidLiteral")]
unsafe fn html_parse_pubid_literal(ctxt: HtmlParserCtxtPtr) -> *mut XmlChar {
    unsafe {
        let mut len: size_t = 0;
        let mut err: i32 = 0;

        let mut ret: *mut XmlChar = null_mut();

        if (*ctxt).current_byte() != b'"' && (*ctxt).current_byte() != b'\'' {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrLiteralNotStarted,
                "PubidLiteral \" or ' expected\n",
                None,
                None,
            );
            return null_mut();
        }
        let quote: i32 = (*ctxt).current_byte() as _;
        (*ctxt).skip_char();

        // Name ::= (Letter | '_') (NameChar)*
        // if (*ctxt).input().unwrap().cur < (*ctxt).input().unwrap().base {
        //     return ret;
        // }
        let start_position = (*ctxt).input().unwrap().offset_from_base();

        while (*ctxt).current_byte() != 0 && (*ctxt).current_byte() as i32 != quote {
            if !xml_is_pubid_char((*ctxt).current_byte() as u32) {
                html_parse_err_int!(
                    &mut *ctxt,
                    XmlParserErrors::XmlErrInvalidChar,
                    "Invalid char in PubidLiteral 0x{:X}\n",
                    (*ctxt).current_byte() as i32
                );
                err = 1;
            }
            len += 1;
            (*ctxt).skip_char();
        }

        if (*ctxt).current_byte() as i32 != quote {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrLiteralNotFinished,
                "Unfinished PubidLiteral\n",
                None,
                None,
            );
        } else {
            if err == 0 {
                let content = &(*ctxt).input().unwrap().base[start_position..start_position + len];
                ret = xml_strndup(content.as_ptr(), content.len() as i32);
            }
            (*ctxt).skip_char();
        }

        ret
    }
}

/// Parse an External ID or a Public ID
///
/// `[75] ExternalID ::= b'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral`
///
/// `[83] PublicID ::= b'PUBLIC' S PubidLiteral`
///
/// Returns the function returns SystemLiteral and in the second
/// case publicID receives PubidLiteral, is strict is off
/// it is possible to return NULL and have publicID set.
#[doc(alias = "htmlParseExternalID")]
unsafe fn html_parse_external_id(
    ctxt: HtmlParserCtxtPtr,
    public_id: *mut *mut XmlChar,
) -> *mut XmlChar {
    unsafe {
        let mut uri: *mut XmlChar = null_mut();

        if (*ctxt).content_bytes().len() >= 6
            && (*ctxt).content_bytes()[..6].eq_ignore_ascii_case(b"SYSTEM")
        {
            (*ctxt).advance(6);
            if !xml_is_blank_char((*ctxt).current_byte() as u32) {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after 'SYSTEM'\n",
                    None,
                    None,
                );
            }
            html_skip_blank_chars(&mut *ctxt);
            uri = html_parse_system_literal(ctxt);
            if uri.is_null() {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrURIRequired,
                    "htmlParseExternalID: SYSTEM, no URI\n",
                    None,
                    None,
                );
            }
        } else if (*ctxt).content_bytes().len() >= 6
            && (*ctxt).content_bytes()[..6].eq_ignore_ascii_case(b"PUBLIC")
        {
            (*ctxt).advance(6);
            if !xml_is_blank_char((*ctxt).current_byte() as u32) {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after 'PUBLIC'\n",
                    None,
                    None,
                );
            }
            html_skip_blank_chars(&mut *ctxt);
            *public_id = html_parse_pubid_literal(ctxt);
            if (*public_id).is_null() {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrPubidRequired,
                    "htmlParseExternalID: PUBLIC, no Public Identifier\n",
                    None,
                    None,
                );
            }
            html_skip_blank_chars(&mut *ctxt);
            if (*ctxt).current_byte() == b'"' || (*ctxt).current_byte() == b'\'' {
                uri = html_parse_system_literal(ctxt);
            }
        }
        uri
    }
}

/// Parse a DOCTYPE declaration
///
/// `[28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>'`
#[doc(alias = "htmlParseDocTypeDecl")]
unsafe fn html_parse_doc_type_decl(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        let mut external_id: *mut XmlChar = null_mut();

        // We know that '<!DOCTYPE' has been detected.
        (*ctxt).advance(9);

        html_skip_blank_chars(&mut *ctxt);

        // Parse the DOCTYPE name.
        let name = html_parse_name(&mut *ctxt);
        if name.is_none() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "htmlParseDocTypeDecl : no DOCTYPE name !\n",
                None,
                None,
            );
        }
        // Check that upper(name) == "HTML" !!!!!!!!!!!!!

        html_skip_blank_chars(&mut *ctxt);

        // Check for SystemID and ExternalID
        let uri: *mut XmlChar = html_parse_external_id(ctxt, addr_of_mut!(external_id));
        html_skip_blank_chars(&mut *ctxt);

        // We should be at the end of the DOCTYPE declaration.
        if (*ctxt).current_byte() != b'>' {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrDoctypeNotFinished,
                "DOCTYPE improperly terminated\n",
                None,
                None,
            );
            // Ignore bogus content
            while !(*ctxt).content_bytes().is_empty()
                && (*ctxt).current_byte() != b'>'
                && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
            {
                (*ctxt).skip_char();
            }
        }
        if (*ctxt).current_byte() == b'>' {
            (*ctxt).skip_char();
        }

        // Create or update the document accordingly to the DOCTYPE
        if (*ctxt).disable_sax == 0 {
            if let Some(internal_subset) = (*ctxt)
                .sax
                .as_deref_mut()
                .and_then(|sax| sax.internal_subset)
            {
                internal_subset(
                    (*ctxt).user_data.clone(),
                    name.as_deref(),
                    (!external_id.is_null())
                        .then(|| CStr::from_ptr(external_id as *const i8).to_string_lossy())
                        .as_deref(),
                    (!uri.is_null())
                        .then(|| CStr::from_ptr(uri as *const i8).to_string_lossy())
                        .as_deref(),
                );
            }
        }

        // Cleanup, since we don't use all those identifiers
        if !uri.is_null() {
            xml_free(uri as _);
        }
        if !external_id.is_null() {
            xml_free(external_id as _);
        }
    }
}

/// Parse an XML (SGML) comment <!-- .... -->
///
/// `[15] Comment ::= b'<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'`
#[doc(alias = "htmlParseComment")]
unsafe fn html_parse_comment(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        let mut buf: *mut XmlChar;
        let mut len: i32;
        let mut size: i32 = HTML_PARSER_BUFFER_SIZE as i32;
        let mut q: i32;
        let mut ql: i32 = 0;
        let mut r: i32;
        let mut rl: i32 = 0;
        let mut cur: i32;
        let mut l: i32 = 0;
        let mut next: i32;
        let mut nl: i32 = 0;
        let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_HUGE_LENGTH as i32
        } else {
            XML_MAX_TEXT_LENGTH as i32
        };

        // Check that there is a comment right here.
        if (*ctxt).token == 0 && !(*ctxt).content_bytes().starts_with(b"<!--") {
            return;
        }

        let state: XmlParserInputState = (*ctxt).instate;
        (*ctxt).instate = XmlParserInputState::XmlParserComment;
        (*ctxt).advance(4);
        buf = xml_malloc_atomic(size as usize) as _;
        if buf.is_null() {
            html_err_memory(ctxt, Some("buffer allocation failed\n"));
            (*ctxt).instate = state;
            return;
        }
        len = 0;
        *buf.add(len as usize) = 0;
        q = html_current_char(&mut *ctxt, &mut ql);
        if q == 0 {
            // goto unfinished;
        } else {
            if q == b'>' as i32 {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrCommentAbruptlyEnded,
                    "Comment abruptly ended",
                    None,
                    None,
                );
                cur = b'>' as i32;
                // goto finished;
            } else {
                (*ctxt).advance_with_line_handling(ql as usize);
                (*ctxt).token = 0;
                r = html_current_char(&mut *ctxt, &mut rl);
                if r == 0 {
                    // goto unfinished;
                    let b = CStr::from_ptr(buf as *const i8).to_string_lossy();
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrCommentNotFinished,
                        format!("Comment not terminated \n<!--{b}\n").as_str(),
                        Some(&b),
                        None,
                    );
                    xml_free(buf as _);
                    return;
                }
                if q == b'-' as i32 && r == b'>' as i32 {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrCommentAbruptlyEnded,
                        "Comment abruptly ended",
                        None,
                        None,
                    );
                    cur = b'>' as i32;
                    // goto finished;
                } else {
                    (*ctxt).advance_with_line_handling(rl as usize);
                    (*ctxt).token = 0;
                    cur = html_current_char(&mut *ctxt, &mut l);
                    while cur != 0 && (cur != b'>' as i32 || r != b'-' as i32 || q != b'-' as i32) {
                        (*ctxt).advance_with_line_handling(l as usize);
                        (*ctxt).token = 0;
                        next = html_current_char(&mut *ctxt, &mut nl);

                        if q == b'-' as i32
                            && r == b'-' as i32
                            && cur == b'!' as i32
                            && next == b'>' as i32
                        {
                            html_parse_err(
                                ctxt,
                                XmlParserErrors::XmlErrCommentNotFinished,
                                "Comment incorrectly closed by '--!>'",
                                None,
                                None,
                            );
                            cur = b'>' as i32;
                            break;
                        }

                        if len + 5 >= size {
                            size *= 2;
                            let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize) as _;
                            if tmp.is_null() {
                                xml_free(buf as _);
                                html_err_memory(ctxt, Some("growing buffer failed\n"));
                                (*ctxt).instate = state;
                                return;
                            }
                            buf = tmp;
                        }
                        if xml_is_char(q as u32) {
                            COPY_BUF!(ctxt, ql, buf, len, q);
                        } else {
                            html_parse_err_int!(
                                &mut *ctxt,
                                XmlParserErrors::XmlErrInvalidChar,
                                "Invalid char in comment 0x{:X}\n",
                                q
                            );
                        }
                        if len > max_length {
                            html_parse_err(
                                ctxt,
                                XmlParserErrors::XmlErrCommentNotFinished,
                                "comment too long",
                                None,
                                None,
                            );
                            xml_free(buf as _);
                            (*ctxt).instate = state;
                            return;
                        }

                        q = r;
                        ql = rl;
                        r = cur;
                        rl = l;
                        cur = next;
                        l = nl;
                    }
                }
            }
            // finished:
            *buf.add(len as usize) = 0;
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                xml_free(buf as _);
                return;
            }
            if cur == b'>' as i32 {
                (*ctxt).skip_char();
                if (*ctxt).disable_sax == 0 {
                    if let Some(comment) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.comment) {
                        comment(
                            (*ctxt).user_data.clone(),
                            &CStr::from_ptr(buf as *const i8).to_string_lossy(),
                        );
                    }
                }
                xml_free(buf as _);
                (*ctxt).instate = state;
                return;
            }
        }

        // unfinished:
        let b = CStr::from_ptr(buf as *const i8).to_string_lossy();
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrCommentNotFinished,
            format!("Comment not terminated \n<!--{b}\n").as_str(),
            Some(&b),
            None,
        );
        xml_free(buf as _);
    }
}

unsafe fn html_skip_bogus_comment(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlHTMLIncorrectlyOpenedComment,
            "Incorrectly opened comment\n",
            None,
            None,
        );

        'b: while {
            let c = (*ctxt).current_byte();
            if c == 0 {
                break 'b;
            }
            (*ctxt).skip_char();

            c != b'>'
        } {}
    }
}

/// Parse an XML Processing Instruction.
///
/// `[16] PI ::= b'<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'`
#[doc(alias = "xmlParsePI")]
unsafe fn html_parse_pi(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        let mut buf: *mut XmlChar;
        let mut len: i32 = 0;
        let mut size: i32 = HTML_PARSER_BUFFER_SIZE as i32;
        let mut cur: i32;
        let mut l: i32 = 0;
        let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
            XML_MAX_HUGE_LENGTH as i32
        } else {
            XML_MAX_TEXT_LENGTH as i32
        };
        let state: XmlParserInputState;

        if (*ctxt).token == 0 && (*ctxt).content_bytes().starts_with(b"<?") {
            state = (*ctxt).instate;
            (*ctxt).instate = XmlParserInputState::XmlParserPI;
            // this is a Processing Instruction.
            (*ctxt).advance(2);

            // Parse the target name and check for special support like namespace.
            if let Some(target) = html_parse_name(&mut *ctxt) {
                if (*ctxt).token == 0 && (*ctxt).current_byte() == b'>' {
                    (*ctxt).advance(1);

                    // SAX: PI detected.
                    if (*ctxt).disable_sax == 0 {
                        if let Some(processing_instruction) = (*ctxt)
                            .sax
                            .as_deref_mut()
                            .and_then(|sax| sax.processing_instruction)
                        {
                            processing_instruction((*ctxt).user_data.clone(), &target, None);
                        }
                    }
                    (*ctxt).instate = state;
                    return;
                }
                buf = xml_malloc_atomic(size as usize) as _;
                if buf.is_null() {
                    html_err_memory(ctxt, None);
                    (*ctxt).instate = state;
                    return;
                }
                cur = (*ctxt).current_byte() as _;
                if !xml_is_blank_char(cur as u32) {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        format!("ParsePI: PI {target} space expected\n").as_str(),
                        Some(&target),
                        None,
                    );
                }
                html_skip_blank_chars(&mut *ctxt);
                cur = html_current_char(&mut *ctxt, &mut l);
                while cur != 0 && cur != b'>' as i32 {
                    if len + 5 >= size {
                        size *= 2;
                        let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize) as _;
                        if tmp.is_null() {
                            html_err_memory(ctxt, None);
                            xml_free(buf as _);
                            (*ctxt).instate = state;
                            return;
                        }
                        buf = tmp;
                    }
                    if xml_is_char(cur as u32) {
                        COPY_BUF!(ctxt, l, buf, len, cur);
                    } else {
                        html_parse_err_int!(
                            &mut *ctxt,
                            XmlParserErrors::XmlErrInvalidChar,
                            "Invalid char in processing instruction 0x{:X}\n",
                            cur
                        );
                    }
                    if len > max_length {
                        html_parse_err(
                            ctxt,
                            XmlParserErrors::XmlErrPINotFinished,
                            format!("PI {target} too long").as_str(),
                            Some(&target),
                            None,
                        );
                        xml_free(buf as _);
                        (*ctxt).instate = state;
                        return;
                    }
                    (*ctxt).advance_with_line_handling(l as usize);
                    (*ctxt).token = 0;
                    cur = html_current_char(&mut *ctxt, &mut l);
                }
                *buf.add(len as usize) = 0;
                if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    xml_free(buf as _);
                    return;
                }
                if cur != b'>' as i32 {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrPINotFinished,
                        format!("ParsePI: PI {target} never end ...\n").as_str(),
                        Some(&target),
                        None,
                    );
                } else {
                    (*ctxt).advance(1);

                    // SAX: PI detected.
                    if (*ctxt).disable_sax == 0 {
                        if let Some(processing_instruction) = (*ctxt)
                            .sax
                            .as_deref_mut()
                            .and_then(|sax| sax.processing_instruction)
                        {
                            processing_instruction(
                                (*ctxt).user_data.clone(),
                                &target,
                                (!buf.is_null())
                                    .then(|| CStr::from_ptr(buf as *const i8).to_string_lossy())
                                    .as_deref(),
                            );
                        }
                    }
                }
                xml_free(buf as _);
            } else {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrPINotStarted,
                    "PI is not started correctly",
                    None,
                    None,
                );
            }
            (*ctxt).instate = state;
        }
    }
}

// The list of HTML elements which are supposed not to have
// CDATA content and where a p element will be implied
//
// TODO: extend that list by reading the HTML SGML DTD on implied paragraph
const HTML_NO_CONTENT_ELEMENTS: &[&str] = &["html", "head"];

/// Check whether a p element need to be implied before inserting
/// characters in the current element.
///
/// Returns 1 if a paragraph has been inserted, 0 if not and -1 in case of error.
#[doc(alias = "htmlCheckParagraph")]
unsafe fn html_check_paragraph(ctxt: HtmlParserCtxtPtr) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return -1;
        }
        let tag = (*ctxt).name.as_deref();
        let Some(tag) = tag else {
            html_auto_close(ctxt, Some("p"));
            html_check_implied(ctxt, "p");
            html_name_push(ctxt, "p");
            if let Some(start_element) =
                (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element)
            {
                start_element((*ctxt).user_data.clone(), "p", &[]);
            }
            return 1;
        };
        if HTML_OMITTED_DEFAULT_VALUE.load(Ordering::Relaxed) == 0 {
            return 0;
        }
        for &elem in HTML_NO_CONTENT_ELEMENTS {
            if tag == elem {
                html_auto_close(ctxt, Some("p"));
                html_check_implied(ctxt, "p");
                html_name_push(ctxt, "p");
                if let Some(start_element) =
                    (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element)
                {
                    start_element((*ctxt).user_data.clone(), "p", &[]);
                }
                return 1;
            }
        }
        0
    }
}

/// Parse and handle entity references in content,
/// this will end-up in a call to character() since this is either a CharRef, or a predefined entity.
#[doc(alias = "htmlParseReference")]
unsafe fn html_parse_reference(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        let mut out: [XmlChar; 6] = [0; 6];
        if (*ctxt).current_byte() != b'&' {
            return;
        }

        if (*ctxt).nth_byte(1) == b'#' {
            let mut bits: i32;
            let mut i: i32 = 0;

            let c: u32 = html_parse_char_ref(ctxt) as _;
            if c == 0 {
                return;
            }

            if c < 0x80 {
                out[i as usize] = c as _;
                i += 1;
                bits = -6;
            } else if c < 0x800 {
                out[i as usize] = ((c >> 6) & 0x1F) as u8 | 0xC0;
                i += 1;
                bits = 0;
            } else if c < 0x10000 {
                out[i as usize] = ((c >> 12) & 0x0F) as u8 | 0xE0;
                i += 1;
                bits = 6;
            } else {
                out[i as usize] = ((c >> 18) & 0x07) as u8 | 0xF0;
                i += 1;
                bits = 12;
            }

            while bits >= 0 {
                out[i as usize] = ((c >> bits) & 0x3F) as u8 | 0x80;
                i += 1;
                bits -= 6;
            }
            out[i as usize] = 0;

            html_check_paragraph(ctxt);
            if let Some(characters) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters) {
                let s = from_utf8(&out[..i as usize]).expect("Internal Error");
                characters((*ctxt).user_data.clone(), s);
            }
        } else {
            let mut name = None;
            let ent = html_parse_entity_ref(ctxt, &mut name);
            let Some(name) = name else {
                html_check_paragraph(ctxt);
                if let Some(characters) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
                {
                    characters((*ctxt).user_data.clone(), "&");
                }
                return;
            };
            if let Some(ent) = ent.filter(|ent| ent.value != 0) {
                let mut bits: i32;
                let mut i: i32 = 0;

                let c: u32 = ent.value;
                if c < 0x80 {
                    out[i as usize] = c as _;
                    i += 1;
                    bits = -6;
                } else if c < 0x800 {
                    out[i as usize] = ((c >> 6) & 0x1F) as u8 | 0xC0;
                    i += 1;
                    bits = 0;
                } else if c < 0x10000 {
                    out[i as usize] = ((c >> 12) & 0x0F) as u8 | 0xE0;
                    i += 1;
                    bits = 6;
                } else {
                    out[i as usize] = ((c >> 18) & 0x07) as u8 | 0xF0;
                    i += 1;
                    bits = 12;
                }

                while bits >= 0 {
                    out[i as usize] = ((c >> bits) & 0x3F) as u8 | 0x80;
                    i += 1;
                    bits -= 6;
                }
                out[i as usize] = 0;

                html_check_paragraph(ctxt);
                if let Some(characters) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
                {
                    let s = from_utf8(&out[..i as usize]).expect("Internal Error");
                    characters((*ctxt).user_data.clone(), s);
                }
            } else {
                html_check_paragraph(ctxt);
                if let Some(characters) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
                {
                    characters((*ctxt).user_data.clone(), "&");
                    characters((*ctxt).user_data.clone(), &name);
                    // (*(*ctxt).sax).characters((*ctxt).userData,  c";".as_ptr() as _, 1);
                }
            }
        }
    }
}

/// Is this a sequence of blank chars that one can ignore ?
///
/// Returns 1 if ignorable 0 otherwise.
#[doc(alias = "areBlanks")]
unsafe fn are_blanks(ctxt: HtmlParserCtxtPtr, str: *const XmlChar, len: i32) -> i32 {
    unsafe {
        for j in 0..len {
            if !xml_is_blank_char(*str.add(j as usize) as u32) {
                return 0;
            }
        }

        if (*ctxt).current_byte() == 0 {
            return 1;
        }
        if (*ctxt).current_byte() != b'<' {
            return 0;
        }
        let Some(name) = (*ctxt).name.as_deref() else {
            return 1;
        };
        if name == "html" {
            return 1;
        }
        if name == "head" {
            return 1;
        }

        // Only strip CDATA children of the body tag for strict HTML DTDs
        if name == "body" {
            if let Some(my_doc) = (*ctxt).my_doc {
                let dtd = my_doc.get_int_subset();
                if dtd.is_some_and(|dtd| {
                    dtd.external_id
                        .as_deref()
                        .filter(|e| {
                            let e = e.to_ascii_uppercase();
                            e == "-//W3C//DTD HTML 4.01//EN" || e == "-//W3C//DTD HTML 4//EN"
                        })
                        .is_some()
                }) {
                    return 1;
                }
            }
        }

        let Some(context_node) = (*ctxt).node else {
            return 0;
        };
        let mut last_child = context_node.get_last_child();
        while let Some(now) = last_child
            .filter(|last_child| last_child.element_type() == XmlElementType::XmlCommentNode)
        {
            last_child = now.prev();
        }
        if let Some(last_child) = last_child {
            if last_child.is_text_node() {
                return 0;
            }
            // keep ws in constructs like <p><b>xy</b> <i>z</i><p>
            // for all tags "p" allowing PCDATA
            for &pcdata in ALLOW_PCDATA {
                if last_child.name().as_deref() == Some(pcdata) {
                    return 0;
                }
            }
        } else {
            if context_node.element_type() != XmlElementType::XmlElementNode
                && context_node.content.is_some()
            {
                return 0;
            }
            // keep ws in constructs like ...<b> </b>...
            // for all tags "b" allowing PCDATA
            for &pcdata in ALLOW_PCDATA {
                if name == pcdata {
                    return 0;
                }
            }
        }
        1
    }
}

/// Parse a CharData section.
/// if we are within a CDATA section ']]>' marks an end of section.
///
/// `[14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)`
#[doc(alias = "htmlParseCharDataInternal")]
unsafe fn html_parse_char_data_internal(ctxt: HtmlParserCtxtPtr, readahead: i32) {
    unsafe {
        let mut buf: [XmlChar; HTML_PARSER_BIG_BUFFER_SIZE + 6] =
            [0; HTML_PARSER_BIG_BUFFER_SIZE + 6];
        let mut nbchar: i32 = 0;
        let mut cur: i32;
        let mut l: i32 = 0;

        if readahead != 0 {
            buf[nbchar as usize] = readahead as _;
            nbchar += 1;
        }

        cur = html_current_char(&mut *ctxt, &mut l);
        while (cur != b'<' as i32 || (*ctxt).token == b'<' as i32)
            && (cur != b'&' as i32 || (*ctxt).token == b'&' as i32)
            && cur != 0
        {
            if !xml_is_char(cur as u32) {
                html_parse_err_int!(
                    &mut *ctxt,
                    XmlParserErrors::XmlErrInvalidChar,
                    "Invalid char in CDATA 0x{:X}\n",
                    cur
                );
            } else {
                COPY_BUF!(ctxt, l, buf.as_mut_ptr(), nbchar, cur);
            }
            (*ctxt).advance_with_line_handling(l as usize);
            (*ctxt).token = 0;
            if nbchar >= HTML_PARSER_BIG_BUFFER_SIZE as i32 {
                buf[nbchar as usize] = 0;

                // Ok the segment is to be consumed as chars.
                if (*ctxt).disable_sax == 0 {
                    if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                        let s = from_utf8(&buf[..nbchar as usize]).expect("Internal Error");
                        if are_blanks(ctxt, buf.as_ptr(), nbchar) != 0 {
                            if (*ctxt).keep_blanks != 0 {
                                if let Some(characters) = sax.characters {
                                    characters((*ctxt).user_data.clone(), s);
                                }
                            } else if let Some(ignorable_whitespace) = sax.ignorable_whitespace {
                                ignorable_whitespace((*ctxt).user_data.clone(), s);
                            }
                        } else {
                            html_check_paragraph(ctxt);
                            if let Some(characters) = sax.characters {
                                characters((*ctxt).user_data.clone(), s);
                            }
                        }
                    }
                }
                nbchar = 0;
                (*ctxt).shrink();
            }
            cur = html_current_char(&mut *ctxt, &mut l);
        }
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            return;
        }
        if nbchar != 0 {
            buf[nbchar as usize] = 0;

            // Ok the segment is to be consumed as chars.
            if (*ctxt).disable_sax == 0 {
                if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                    let s = from_utf8(&buf[..nbchar as usize]).expect("Internal Error");
                    if are_blanks(ctxt, buf.as_ptr(), nbchar) != 0 {
                        if (*ctxt).keep_blanks != 0 {
                            if let Some(characters) = sax.characters {
                                characters((*ctxt).user_data.clone(), s);
                            }
                        } else if let Some(ignorable_whitespace) = sax.ignorable_whitespace {
                            ignorable_whitespace((*ctxt).user_data.clone(), s);
                        }
                    } else {
                        html_check_paragraph(ctxt);
                        if let Some(characters) = sax.characters {
                            characters((*ctxt).user_data.clone(), s);
                        }
                    }
                }
            }
        }
    }
}

/// Parse a CharData section.
/// if we are within a CDATA section ']]>' marks an end of section.
///
/// `[14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)`
#[doc(alias = "htmlParseCharData")]
unsafe fn html_parse_char_data(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        html_parse_char_data_internal(ctxt, 0);
    }
}

// /// Parse a content: comment, sub-element, reference or text.
// /// Kept for compatibility with old code
// #[doc(alias = "htmlParseContent")]
// unsafe fn html_parse_content(ctxt: HtmlParserCtxtPtr) {
//     unsafe {
//         let mut name: *const XmlChar;

//         let current_node = (*ctxt).name.clone();
//         let depth = (*ctxt).name_tab.len();
//         loop {
//             (*ctxt).grow();

//             if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
//                 break;
//             }

//             // Our tag or one of it's parent or children is ending.
//             if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'/' {
//                 if html_parse_end_tag(ctxt) != 0
//                     && (current_node.is_some() || (*ctxt).name_tab.is_empty())
//                 {
//                     return;
//                 }
//                 continue; /* while */
//             } else if (*ctxt).current_byte() == b'<'
//                 && ((*ctxt).nth_byte(1).is_ascii_alphabetic()
//                     || (*ctxt).nth_byte(1) == b'_'
//                     || (*ctxt).nth_byte(1) == b':')
//             {
//                 name = html_parse_html_name_non_invasive(ctxt);
//                 if name.is_null() {
//                     html_parse_err(
//                         ctxt,
//                         XmlParserErrors::XmlErrNameRequired,
//                         "htmlParseStartTag: invalid element name\n",
//                         None,
//                         None,
//                     );
//                     // Dump the bogus tag like browsers do
//                     #[allow(clippy::while_immutable_condition)]
//                     while (*ctxt).current_byte() != 0 && (*ctxt).current_byte() != b'>' {
//                         (*ctxt).skip_char();
//                     }

//                     return;
//                 }

//                 if (*ctxt).name.is_some()
//                     && html_check_auto_close(name, (*ctxt).name.as_deref()) == 1
//                 {
//                     html_auto_close(ctxt, name);
//                     continue;
//                 }
//             }

//             // Has this node been popped out during parsing of the next element
//             if !(*ctxt).name_tab.is_empty()
//                 && depth >= (*ctxt).name_tab.len()
//                 && current_node != (*ctxt).name
//             {
//                 return;
//             }

//             if (*ctxt).current_byte() != 0
//                 && (current_node.as_deref() == Some("script")
//                     || current_node.as_deref() == Some("style"))
//             {
//                 // Handle SCRIPT/STYLE separately
//                 html_parse_script(ctxt);
//             } else if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'!' {
//                 // Sometimes DOCTYPE arrives in the middle of the document
//                 if (*ctxt).content_bytes().len() >= 9
//                     && (*ctxt).content_bytes()[2..9].eq_ignore_ascii_case(b"DOCTYPE")
//                 {
//                     html_parse_err(
//                         ctxt,
//                         XmlParserErrors::XmlHTMLStrucureError,
//                         "Misplaced DOCTYPE declaration\n",
//                         Some("DOCTYPE"),
//                         None,
//                     );
//                     html_parse_doc_type_decl(ctxt);
//                 } else if (*ctxt).nth_byte(2) == b'-' && (*ctxt).nth_byte(3) == b'-' {
//                     //  case :  a comment
//                     html_parse_comment(ctxt);
//                 } else {
//                     html_skip_bogus_comment(ctxt);
//                 }
//             } else if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'?' {
//                 // Second case : a Processing Instruction.
//                 html_parse_pi(ctxt);
//             } else if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1).is_ascii_alphabetic() {
//                 // Third case :  a sub-element.
//                 html_parse_element(ctxt);
//             } else if (*ctxt).current_byte() == b'<' {
//                 if (*ctxt).disable_sax == 0 {
//                     if let Some(characters) =
//                         (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
//                     {
//                         characters((*ctxt).user_data.clone(), "<");
//                     }
//                 }
//                 (*ctxt).skip_char();
//             } else if (*ctxt).current_byte() == b'&' {
//                 // Fourth case : a reference. If if has not been resolved,
//                 //    parsing returns it's Name, create the node
//                 html_parse_reference(ctxt);
//             } else if (*ctxt).current_byte() == 0 {
//                 // Fifth case : end of the resource
//                 html_auto_close_on_end(ctxt);
//                 break;
//             } else {
//                 // Last case, text. Note that References are handled directly.
//                 html_parse_char_data(ctxt);
//             }

//             (*ctxt).shrink();
//             (*ctxt).grow();
//         }
//     }
// }

// /// Parse an HTML element, this is highly recursive
// /// this is kept for compatibility with previous code versions
// ///
// /// `[39] element ::= EmptyElemTag | STag content ETag`
// ///
// /// `[41] Attribute ::= Name Eq AttValue`
// #[doc(alias = "htmlParseElement")]
// pub(crate) unsafe fn html_parse_element(ctxt: HtmlParserCtxtPtr) {
//     unsafe {
//         let mut node_info = XmlParserNodeInfo::default();
//         let mut oldptr: *const XmlChar;

//         if ctxt.is_null() || (*ctxt).input().is_none() {
//             html_parse_err(
//                 ctxt,
//                 XmlParserErrors::XmlErrInternalError,
//                 "htmlParseElement: context error\n",
//                 None,
//                 None,
//             );
//             return;
//         }

//         if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
//             return;
//         }

//         // Capture start position
//         if (*ctxt).record_info != 0 {
//             node_info.begin_pos =
//                 (*ctxt).input().unwrap().consumed + (*ctxt).input().unwrap().offset_from_base() as u64;
//             node_info.begin_line = (*ctxt).input().unwrap().line as _;
//         }

//         let failed: i32 = html_parse_start_tag(ctxt);
//         let name = (*ctxt).name.clone();
//         let Some(name) = name.filter(|_| failed != -1) else {
//             if (*ctxt).current_byte() == b'>' {
//                 (*ctxt).skip_char();
//             }
//             return;
//         };

//         // Lookup the info for that element.
//         let info = html_tag_lookup(&name);
//         if info.is_none() {
//             html_parse_err(
//                 ctxt,
//                 XmlParserErrors::XmlHTMLUnknownTag,
//                 format!("Tag {name} invalid\n").as_str(),
//                 Some(&name),
//                 None,
//             );
//         }

//         // Check for an Empty Element labeled the XML/SGML way
//         if (*ctxt).current_byte() == b'/' && (*ctxt).nth_byte(1) == b'>' {
//             (*ctxt).advance(2);
//             if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
//                 end_element((*ctxt).user_data.clone(), &name);
//             }
//             html_name_pop(ctxt);
//             return;
//         }

//         if (*ctxt).current_byte() == b'>' {
//             (*ctxt).skip_char();
//         } else {
//             html_parse_err(
//                 ctxt,
//                 XmlParserErrors::XmlErrGtRequired,
//                 format!("Couldn't find end of Start Tag {name}\n").as_str(),
//                 Some(&name),
//                 None,
//             );

//             // end of parsing of this node.
//             if Some(name.as_str()) == (*ctxt).name.as_deref() {
//                 (*ctxt).node_pop();
//                 html_name_pop(ctxt);
//             }

//             // Capture end position and add node
//             if (*ctxt).record_info != 0 {
//                 node_info.end_pos =
//                     (*ctxt).input().unwrap().consumed + (*ctxt).input().unwrap().offset_from_base() as u64;
//                 node_info.end_line = (*ctxt).input().unwrap().line as _;
//                 node_info.node = (*ctxt).node;
//                 xml_parser_add_node_info(ctxt, Rc::new(RefCell::new(node_info)));
//             }
//             return;
//         }

//         // Check for an Empty Element from DTD definition
//         if info.is_some_and(|info| info.empty != 0) {
//             if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
//                 end_element((*ctxt).user_data.clone(), &name);
//             }
//             html_name_pop(ctxt);
//             return;
//         }

//         // Parse the content of the element:
//         let current_node = (*ctxt).name.clone();
//         let depth = (*ctxt).name_tab.len();
//         #[allow(clippy::while_immutable_condition)]
//         while (*ctxt).current_byte() != 0 {
//             oldptr = (*ctxt).input().unwrap().cur;
//             html_parse_content(ctxt);
//             if oldptr == (*ctxt).input().unwrap().cur {
//                 break;
//             }
//             if (*ctxt).name_tab.len() < depth {
//                 break;
//             }
//         }

//         // Capture end position and add node
//         if current_node.is_some() && (*ctxt).record_info != 0 {
//             node_info.end_pos =
//                 (*ctxt).input().unwrap().consumed + (*ctxt).input().unwrap().offset_from_base() as u64;
//             node_info.end_line = (*ctxt).input().unwrap().line as _;
//             node_info.node = (*ctxt).node;
//             xml_parser_add_node_info(ctxt, Rc::new(RefCell::new(node_info)));
//         }
//         if (*ctxt).current_byte() == 0 {
//             html_auto_close_on_end(ctxt);
//         }
//     }
// }

/// Allocate and initialize a new parser context.
///
/// Returns the `htmlParserCtxtPtr` or NULL in case of allocation error
#[doc(alias = "htmlNewParserCtxt")]
pub unsafe fn html_new_parser_ctxt() -> HtmlParserCtxtPtr {
    unsafe { html_new_sax_parser_ctxt(None, None) }
}

/// Initialize a parser context
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "htmlInitParserCtxt")]
unsafe fn html_init_parser_ctxt(
    ctxt: HtmlParserCtxtPtr,
    sax: Option<Box<HtmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return -1;
        }
        std::ptr::write(&mut *ctxt, HtmlParserCtxt::default());

        (*ctxt).dict = xml_dict_create();
        if (*ctxt).dict.is_null() {
            html_err_memory(null_mut(), Some("htmlInitParserCtxt: out of memory\n"));
            return -1;
        }

        if sax.is_none() {
            let mut sax = HtmlSAXHandler::default();
            xml_sax2_init_html_default_sax_handler(&mut sax);
            (*ctxt).sax = Some(Box::new(sax));
            (*ctxt).user_data = Some(GenericErrorContext::new(ctxt));
        } else {
            (*ctxt).sax = sax;
            (*ctxt).user_data = user_data.or_else(|| Some(GenericErrorContext::new(ctxt)));
        }

        // Allocate the Input stack
        (*ctxt).input_tab.clear();
        (*ctxt).input_tab.shrink_to(5);
        (*ctxt).version = None;
        (*ctxt).encoding = None;
        (*ctxt).standalone = -1;
        (*ctxt).instate = XmlParserInputState::XmlParserStart;

        // Allocate the Node stack
        (*ctxt).input_tab.clear();
        (*ctxt).node_tab.shrink_to(10);
        (*ctxt).node = None;

        // Allocate the Name stack
        (*ctxt).name_tab.clear();
        (*ctxt).name_tab.shrink_to(10);
        (*ctxt).name = None;

        (*ctxt).node_info_tab.clear();

        (*ctxt).my_doc = None;
        (*ctxt).well_formed = 1;
        (*ctxt).replace_entities = 0;
        (*ctxt).linenumbers = get_line_numbers_default_value();
        (*ctxt).keep_blanks = get_keep_blanks_default_value();
        (*ctxt).html = 1;
        (*ctxt).vctxt.flags = XML_VCTXT_USE_PCTXT as _;
        (*ctxt).vctxt.user_data = Some(GenericErrorContext::new(ctxt));
        (*ctxt).vctxt.error = Some(parser_validity_error);
        (*ctxt).vctxt.warning = Some(parser_validity_warning);
        (*ctxt).record_info = 0;
        (*ctxt).validate = 0;
        (*ctxt).check_index = 0;
        #[cfg(feature = "catalog")]
        {
            (*ctxt).catalogs = None;
        }
        (*ctxt).node_seq.clear();
        0
    }
}

/// Allocate and initialize a new SAX parser context.
/// If userData is NULL, the parser context will be passed as user data.
///
/// Returns the `htmlParserCtxtPtr` or NULL in case of allocation error
#[doc(alias = "htmlNewSAXParserCtxt")]
pub unsafe fn html_new_sax_parser_ctxt(
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
) -> HtmlParserCtxtPtr {
    unsafe {
        let ctxt: XmlParserCtxtPtr = xml_malloc(size_of::<XmlParserCtxt>()) as XmlParserCtxtPtr;
        if ctxt.is_null() {
            html_err_memory(null_mut(), Some("NewParserCtxt: out of memory\n"));
            return null_mut();
        }
        memset(ctxt as _, 0, size_of::<XmlParserCtxt>());
        std::ptr::write(&mut *ctxt, XmlParserCtxt::default());
        if html_init_parser_ctxt(ctxt, sax, user_data) < 0 {
            html_free_parser_ctxt(ctxt);
            return null_mut();
        }
        ctxt
    }
}

/// Create a parser context for an HTML in-memory document.
///
/// Returns the new parser context or NULL
#[doc(alias = "htmlCreateMemoryParserCtxt")]
pub unsafe fn html_create_memory_parser_ctxt(buffer: Vec<u8>) -> HtmlParserCtxtPtr {
    unsafe {
        if buffer.is_empty() {
            return null_mut();
        }

        let ctxt: XmlParserCtxtPtr = html_new_parser_ctxt();
        if ctxt.is_null() {
            return null_mut();
        }

        let Some(buf) = XmlParserInputBuffer::from_memory(buffer, XmlCharEncoding::None) else {
            xml_free_parser_ctxt(ctxt);
            return null_mut();
        };

        let Some(mut input) = XmlParserInput::new(Some(&mut *ctxt)) else {
            xml_free_parser_ctxt(ctxt);
            return null_mut();
        };

        input.filename = None;
        input.buf = Some(Rc::new(RefCell::new(buf)));
        input.reset_base();

        (*ctxt).input_push(input);
        ctxt
    }
}

#[doc(alias = "htmlParserFinishElementParsing")]
unsafe fn html_parser_finish_element_parsing(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        // Capture end position and add node
        if let Some(node) = (*ctxt).node {
            if (*ctxt).record_info != 0 {
                let node_info = (*ctxt).node_info_tab.last_mut().expect("Internal Error");
                node_info.borrow_mut().end_pos = (*ctxt).input().unwrap().consumed
                    + (*ctxt).input().unwrap().offset_from_base() as u64;
                node_info.borrow_mut().end_line = (*ctxt).input().unwrap().line as _;
                node_info.borrow_mut().node = Some(node);
                (*ctxt).add_node_info(node_info.clone());
                html_node_info_pop(ctxt);
            }
        }
        if (*ctxt).current_byte() == 0 {
            html_auto_close_on_end(ctxt);
        }
    }
}

/// Pushes a new element name on top of the node info stack
///
/// Returns 0 in case of error, the index in the stack otherwise
#[doc(alias = "htmlNodeInfoPush")]
unsafe fn html_node_info_push(
    ctxt: HtmlParserCtxtPtr,
    value: Rc<RefCell<HtmlParserNodeInfo>>,
) -> usize {
    unsafe {
        (*ctxt).node_info_tab.push(value);
        (*ctxt).node_info_tab.len()
    }
}

/// Parse an HTML element, new version, non recursive
///
/// `[39] element ::= EmptyElemTag | STag content ETag`
///
/// `[41] Attribute ::= Name Eq AttValue`
#[doc(alias = "htmlParseElementInternal")]
unsafe fn html_parse_element_internal(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        let mut node_info = HtmlParserNodeInfo::default();

        if ctxt.is_null() || (*ctxt).input().is_none() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                "htmlParseElementInternal: context error\n",
                None,
                None,
            );
            return;
        }

        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            return;
        }

        // Capture start position
        if (*ctxt).record_info != 0 {
            node_info.begin_pos = (*ctxt).input().unwrap().consumed
                + (*ctxt).input().unwrap().offset_from_base() as u64;
            node_info.begin_line = (*ctxt).input().unwrap().line as _;
        }

        let failed: i32 = html_parse_start_tag(ctxt);
        let Some(name) = (*ctxt).name.clone().filter(|_| failed != -1) else {
            if (*ctxt).current_byte() == b'>' {
                (*ctxt).skip_char();
            }
            return;
        };

        // Lookup the info for that element.
        let info = html_tag_lookup(&name);
        if info.is_none() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlHTMLUnknownTag,
                format!("Tag {name} invalid\n").as_str(),
                Some(&name),
                None,
            );
        }

        // Check for an Empty Element labeled the XML/SGML way
        if (*ctxt).current_byte() == b'/' && (*ctxt).nth_byte(1) == b'>' {
            (*ctxt).advance(2);
            if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
                end_element((*ctxt).user_data.clone(), &name);
            }
            html_name_pop(ctxt);
            return;
        }

        if (*ctxt).current_byte() == b'>' {
            (*ctxt).skip_char();
        } else {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrGtRequired,
                format!("Couldn't find end of Start Tag {name}\n").as_str(),
                Some(name.as_str()),
                None,
            );

            // end of parsing of this node.
            if Some(name.as_str()) == (*ctxt).name.as_deref() {
                (*ctxt).node_pop();
                html_name_pop(ctxt);
            }

            if (*ctxt).record_info != 0 {
                html_node_info_push(ctxt, Rc::new(RefCell::new(node_info)));
            }
            html_parser_finish_element_parsing(ctxt);
            return;
        }

        // Check for an Empty Element from DTD definition
        if info.is_some_and(|info| info.empty != 0) {
            if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
                end_element((*ctxt).user_data.clone(), &name);
            }
            html_name_pop(ctxt);
            return;
        }

        if (*ctxt).record_info != 0 {
            html_node_info_push(ctxt, Rc::new(RefCell::new(node_info)));
        }
    }
}

/// Parse a content: comment, sub-element, reference or text.
/// New version for non recursive htmlParseElementInternal
#[doc(alias = "htmlParseContentInternal")]
unsafe fn html_parse_content_internal(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        let mut name: *const XmlChar;
        let mut depth = (*ctxt).name_tab.len();
        let mut current_node = if depth == 0 {
            None
        } else {
            (*ctxt).name.clone()
        };
        loop {
            (*ctxt).grow();

            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                break;
            }

            // Our tag or one of it's parent or children is ending.
            if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'/' {
                if html_parse_end_tag(ctxt) != 0
                    && (current_node.is_some() || (*ctxt).name_tab.is_empty())
                {
                    depth = (*ctxt).name_tab.len();
                    if depth == 0 {
                        current_node = None;
                    } else {
                        current_node = (*ctxt).name.clone();
                    }
                }
                continue; /* while */
            } else if (*ctxt).current_byte() == b'<'
                && ((*ctxt).nth_byte(1).is_ascii_alphabetic()
                    || (*ctxt).nth_byte(1) == b'_'
                    || (*ctxt).nth_byte(1) == b':')
            {
                name = html_parse_html_name_non_invasive(ctxt);
                if name.is_null() {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrNameRequired,
                        "htmlParseStartTag: invalid element name\n",
                        None,
                        None,
                    );
                    // Dump the bogus tag like browsers do
                    while (*ctxt).current_byte() == 0 && (*ctxt).current_byte() != b'>' {
                        (*ctxt).skip_char();
                    }

                    html_parser_finish_element_parsing(ctxt);

                    current_node = (*ctxt).name.clone();
                    depth = (*ctxt).name_tab.len();
                    continue;
                }

                if (*ctxt).name.is_some()
                    && html_check_auto_close(
                        CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
                        (*ctxt).name.as_deref().unwrap(),
                    )
                {
                    html_auto_close(
                        ctxt,
                        Some(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref()),
                    );
                    continue;
                }
            }

            // Has this node been popped out during parsing of the next element
            if !(*ctxt).name_tab.is_empty()
                && depth >= (*ctxt).name_tab.len()
                && current_node != (*ctxt).name
            {
                html_parser_finish_element_parsing(ctxt);

                current_node = (*ctxt).name.clone();
                depth = (*ctxt).name_tab.len();
                continue;
            }

            if (*ctxt).current_byte() != 0
                && (current_node.as_deref() == Some("script")
                    || current_node.as_deref() == Some("style"))
            {
                // Handle SCRIPT/STYLE separately
                html_parse_script(ctxt);
            } else if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'!' {
                // Sometimes DOCTYPE arrives in the middle of the document
                if (*ctxt).content_bytes().len() >= 9
                    && (*ctxt).content_bytes()[2..9].eq_ignore_ascii_case(b"DOCTYPE")
                {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlHTMLStrucureError,
                        "Misplaced DOCTYPE declaration\n",
                        Some("DOCTYPE"),
                        None,
                    );
                    html_parse_doc_type_decl(ctxt);
                } else if (*ctxt).nth_byte(2) == b'-' && (*ctxt).nth_byte(3) == b'-' {
                    // First case :  a comment
                    html_parse_comment(ctxt);
                } else {
                    html_skip_bogus_comment(ctxt);
                }
            } else if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'?' {
                // Second case : a Processing Instruction.
                html_parse_pi(ctxt);
            } else if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1).is_ascii_alphabetic() {
                // Third case :  a sub-element.
                html_parse_element_internal(ctxt);

                current_node = (*ctxt).name.clone();
                depth = (*ctxt).name_tab.len();
            } else if (*ctxt).current_byte() == b'<' {
                if (*ctxt).disable_sax == 0 {
                    if let Some(characters) =
                        (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
                    {
                        characters((*ctxt).user_data.clone(), "<");
                    }
                }
                (*ctxt).skip_char();
            } else if (*ctxt).current_byte() == b'&' {
                // Fourth case : a reference. If if has not been resolved,
                //    parsing returns it's Name, create the node
                html_parse_reference(ctxt);
            } else if (*ctxt).current_byte() == 0 {
                // Fifth case : end of the resource
                html_auto_close_on_end(ctxt);
                break;
            } else {
                // Last case, text. Note that References are handled directly.
                html_parse_char_data(ctxt);
            }

            (*ctxt).shrink();
            (*ctxt).grow();
        }
    }
}

/// Parse an HTML document (and build a tree if using the standard SAX interface).
///
/// Returns 0, -1 in case of error. the parser context is augmented as a result of the parsing.
#[doc(alias = "htmlParseDocument")]
pub unsafe fn html_parse_document(ctxt: HtmlParserCtxtPtr) -> i32 {
    unsafe {
        xml_init_parser();

        if ctxt.is_null() || (*ctxt).input().is_none() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                "htmlParseDocument: context error\n",
                None,
                None,
            );
            return XmlParserErrors::XmlErrInternalError as i32;
        }
        (*ctxt).grow();
        // SAX: beginning of the document processing.
        if let Some(set_document_locator) = (*ctxt)
            .sax
            .as_deref_mut()
            .and_then(|sax| sax.set_document_locator)
        {
            set_document_locator((*ctxt).user_data.clone(), xml_default_sax_locator());
        }

        if (*ctxt).encoding.is_none() && (*ctxt).input().unwrap().remainder_len() >= 4 {
            // Get the 4 first bytes and decode the charset
            // if enc != xmlCharEncoding::XML_CHAR_ENCODING_NONE
            // plug some encoding conversion routines.
            if (*ctxt).token == 0 {
                let enc = detect_encoding(&(*ctxt).content_bytes()[..4]);
                if !matches!(enc, XmlCharEncoding::None) {
                    (*ctxt).switch_encoding(enc);
                }
            }
        }

        // Wipe out everything which is before the first '<'
        html_skip_blank_chars(&mut *ctxt);
        if (*ctxt).current_byte() == 0 {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrDocumentEmpty,
                "Document is empty\n",
                None,
                None,
            );
        }

        if (*ctxt).disable_sax == 0 {
            if let Some(start_document) = (*ctxt)
                .sax
                .as_deref_mut()
                .and_then(|sax| sax.start_document)
            {
                start_document((*ctxt).user_data.clone());
            }
        }

        // Parse possible comments and PIs before any content
        while (*ctxt).content_bytes().starts_with(b"<!--")
            || (*ctxt).content_bytes().starts_with(b"<?")
        {
            html_parse_comment(ctxt);
            html_parse_pi(ctxt);
            html_skip_blank_chars(&mut *ctxt);
        }

        // Then possibly doc type declaration(s) and more Misc (doctypedecl Misc*)?
        if (*ctxt).content_bytes().len() >= 9
            && (*ctxt).content_bytes()[..9].eq_ignore_ascii_case(b"<!DOCTYPE")
        {
            html_parse_doc_type_decl(ctxt);
        }
        html_skip_blank_chars(&mut *ctxt);

        // Parse possible comments and PIs before any content
        while (*ctxt).content_bytes().starts_with(b"<!--")
            || (*ctxt).content_bytes().starts_with(b"<?")
        {
            html_parse_comment(ctxt);
            html_parse_pi(ctxt);
            html_skip_blank_chars(&mut *ctxt);
        }

        // Time to start parsing the tree itself
        html_parse_content_internal(ctxt);

        // autoclose
        if (*ctxt).current_byte() == 0 {
            html_auto_close_on_end(ctxt);
        }

        // SAX: end of the document processing.
        if let Some(end_document) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_document) {
            end_document((*ctxt).user_data.clone());
        }

        if (*ctxt).options & HtmlParserOption::HtmlParseNodefdtd as i32 == 0 {
            if let Some(mut my_doc) = (*ctxt).my_doc {
                let dtd = my_doc.get_int_subset();
                if dtd.is_none() {
                    my_doc.int_subset = xml_create_int_subset(
                        Some(my_doc),
                        Some("html"),
                        Some("-//W3C//DTD HTML 4.0 Transitional//EN"),
                        Some("http://www.w3.org/TR/REC-html40/loose.dtd"),
                    );
                }
            }
        }
        if (*ctxt).well_formed == 0 {
            return -1;
        }
        0
    }
}

/// Create a parser context for an HTML document.
///
/// TODO: check the need to add encoding handling there
///
/// Returns the new parser context or NULL
#[doc(alias = "htmlCreateDocParserCtxt")]
unsafe fn html_create_doc_parser_ctxt(
    cur: *const XmlChar,
    encoding: Option<&str>,
) -> HtmlParserCtxtPtr {
    unsafe {
        if cur.is_null() {
            return null_mut();
        }
        let s = CStr::from_ptr(cur as *const i8).to_bytes().to_vec();
        let ctxt: HtmlParserCtxtPtr = html_create_memory_parser_ctxt(s);
        if ctxt.is_null() {
            return null_mut();
        }

        if let Some(encoding) = encoding {
            (*ctxt).input_mut().unwrap().encoding = Some(encoding.to_owned());

            let enc = encoding.parse().unwrap_or(XmlCharEncoding::Error);
            // registered set of known encodings
            if !matches!(enc, XmlCharEncoding::Error) {
                (*ctxt).switch_encoding(enc);
                if (*ctxt).err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrUnsupportedEncoding,
                        format!("Unsupported encoding {encoding}\n").as_str(),
                        Some(encoding),
                        None,
                    );
                }
            } else {
                // fallback for unknown encodings
                if let Some(handler) = find_encoding_handler(encoding) {
                    (*ctxt).switch_to_encoding(handler);
                } else {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrUnsupportedEncoding,
                        format!("Unsupported encoding {encoding}\n").as_str(),
                        Some(encoding),
                        None,
                    );
                }
            }
        }
        ctxt
    }
}

/// Parse an HTML in-memory document.  
/// If sax is not NULL, use the SAX callbacks to handle parse events.  
/// If sax is NULL, fallback to the default DOM behavior and return a tree.
///
/// Returns the resulting document tree unless SAX is NULL or the document is not well formed.
#[doc(alias = "htmlSAXParseDoc")]
#[deprecated = "Use htmlNewSAXParserCtxt and htmlCtxtReadDoc"]
pub unsafe fn html_sax_parse_doc(
    cur: *const XmlChar,
    encoding: Option<&str>,
    sax: Option<Box<HtmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
) -> Option<HtmlDocPtr> {
    unsafe {
        xml_init_parser();

        if cur.is_null() {
            return None;
        }

        let ctxt: HtmlParserCtxtPtr = html_create_doc_parser_ctxt(cur, encoding);
        if ctxt.is_null() {
            return None;
        }
        let replaced = sax.is_some();
        if let Some(sax) = sax {
            (*ctxt).sax = Some(sax);
            (*ctxt).user_data = user_data;
        }

        html_parse_document(ctxt);
        let ret = (*ctxt).my_doc;
        if replaced {
            (*ctxt).sax = None;
            (*ctxt).user_data = None;
        }
        html_free_parser_ctxt(ctxt);

        ret
    }
}

/// Parse an HTML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "htmlParseDoc")]
pub unsafe fn html_parse_doc(cur: *const XmlChar, encoding: Option<&str>) -> Option<HtmlDocPtr> {
    unsafe { html_sax_parse_doc(cur, encoding, None, None) }
}

/// Create a parser context for a file content.
///
/// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
/// by default if found at compile-time.  
/// However, this crate does not provide currently.
///
/// Returns the new parser context or NULL
#[doc(alias = "htmlCreateFileParserCtxt")]
pub unsafe fn html_create_file_parser_ctxt(
    filename: &str,
    encoding: Option<&str>,
) -> HtmlParserCtxtPtr {
    unsafe {
        let ctxt: HtmlParserCtxtPtr = html_new_parser_ctxt();
        if ctxt.is_null() {
            return null_mut();
        }

        let canonic_filename = canonic_path(filename);
        let Some(input_stream) = xml_load_external_entity(Some(&canonic_filename), None, ctxt)
        else {
            xml_free_parser_ctxt(ctxt);
            return null_mut();
        };

        (*ctxt).input_push(input_stream);

        // set encoding
        if let Some(encoding) = encoding {
            let l = encoding.len();

            if l < 1000 {
                let content = format!("charset={encoding}");
                html_check_encoding(ctxt, &content);
            }
        }

        ctxt
    }
}

/// Parse an HTML file and build a tree. Automatic support for ZLIB/Compress
/// compressed document is provided by default if found at compile-time.
/// It use the given SAX function block to handle the parsing callback.
/// If sax is NULL, fallback to the default DOM tree building routines.
///
/// Returns the resulting document tree unless SAX is NULL or the document is not well formed.
#[doc(alias = "htmlSAXParseFile")]
#[deprecated = "Use htmlNewSAXParserCtxt and htmlCtxtReadFile"]
pub unsafe fn html_sax_parse_file(
    filename: &str,
    encoding: Option<&str>,
    sax: Option<Box<HtmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
) -> Option<HtmlDocPtr> {
    unsafe {
        let mut oldsax = None;

        xml_init_parser();

        let ctxt: HtmlParserCtxtPtr = html_create_file_parser_ctxt(filename, encoding);
        if ctxt.is_null() {
            return None;
        }
        let replaced = sax.is_some();
        if let Some(sax) = sax {
            oldsax = (*ctxt).sax.replace(sax);
            (*ctxt).user_data = user_data;
        }

        html_parse_document(ctxt);

        let ret = (*ctxt).my_doc;
        if replaced {
            (*ctxt).sax = oldsax;
            (*ctxt).user_data = None;
        }
        html_free_parser_ctxt(ctxt);

        ret
    }
}

/// Parse an HTML file and build a tree.
///
/// In original libxml2, automatic support for ZLIB/Compress
/// compressed document is provided by default if found at compile-time.  
/// However, this crate does not support currently.
///
/// Returns the resulting document tree
#[doc(alias = "htmlParseFile")]
pub unsafe fn html_parse_file(filename: &str, encoding: Option<&str>) -> Option<HtmlDocPtr> {
    unsafe { html_sax_parse_file(filename, encoding, None, None) }
}

/// Take a block of UTF-8 chars in and try to convert it to an ASCII
/// plus HTML entities block of chars out.
///
/// Returns 0 if success, -2 if the transcoding fails, or -1 otherwise.  
/// The value of @inlen after return is the number of octets consumed
/// as the return value is positive, else unpredictable.  
/// The value of @outlen after return is the number of octets consumed.
#[doc(alias = "UTF8ToHtml")]
pub unsafe fn utf8_to_html(
    mut out: *mut u8,
    outlen: *mut i32,
    mut input: *const u8,
    inlen: *mut i32,
) -> i32 {
    unsafe {
        let mut processed: *const u8 = input;
        let outstart: *const u8 = out;
        let instart: *const u8 = input;
        let mut c: u32;
        let mut d: u32;
        let mut trailing: i32;

        if out.is_null() || outlen.is_null() || inlen.is_null() {
            return -1;
        }
        if input.is_null() {
            // initialization nothing to do
            *outlen = 0;
            *inlen = 0;
            return 0;
        }
        let inend: *const u8 = input.add(*inlen as usize);
        let outend: *const u8 = out.add(*outlen as usize);
        while input < inend {
            d = *input as _;
            input = input.add(1);
            if d < 0x80 {
                c = d;
                trailing = 0;
            } else if d < 0xC0 {
                // trailing byte in leading position
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -2;
            } else if d < 0xE0 {
                c = d & 0x1F;
                trailing = 1;
            } else if d < 0xF0 {
                c = d & 0x0F;
                trailing = 2;
            } else if d < 0xF8 {
                c = d & 0x07;
                trailing = 3;
            } else {
                // no chance for this in Ascii
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -2;
            }

            if inend.offset_from(input) < trailing as isize {
                break;
            }

            while trailing != 0 {
                if input >= inend || {
                    d = *input as _;
                    input = input.add(1);
                    d & 0xC0 != 0x80
                } {
                    break;
                }
                c <<= 6;
                c |= d & 0x3F;
                trailing -= 1;
            }

            // assertion: c is a single UTF-4 value
            if c < 0x80 {
                if out.add(1) >= outend as _ {
                    break;
                }
                *out = c as _;
                out = out.add(1);
            } else {
                // Try to lookup a predefined HTML entity for it

                let cp = if let Some(ent) = html_entity_value_lookup(c) {
                    Cow::Borrowed(ent.name)
                } else {
                    Cow::Owned(format!("#{c}"))
                };
                let len = cp.len();
                if out.add(2 + len as usize) >= outend as _ {
                    break;
                }
                *out = b'&';
                out = out.add(1);
                memcpy(out as _, cp.as_ptr() as _, len as usize);
                out = out.add(len as usize);
                *out = b';';
                out = out.add(1);
            }
            processed = input;
        }
        *outlen = out.offset_from(outstart) as _;
        *inlen = processed.offset_from(instart) as _;
        0
    }
}

/// Take a block of UTF-8 chars in and try to convert it to an ASCII
/// plus HTML entities block of chars out.
///
/// Returns 0 if success, -2 if the transcoding fails, or -1 otherwise.  
/// The value of @inlen after return is the number of octets consumed
/// as the return value is positive, else unpredictable.  
/// The value of @outlen after return is the number of octets consumed.  
#[doc(alias = "htmlEncodeEntities")]
pub unsafe fn html_encode_entities(
    mut out: *mut u8,
    outlen: *mut i32,
    mut input: *const u8,
    inlen: *mut i32,
    quote_char: i32,
) -> i32 {
    unsafe {
        let mut processed: *const u8 = input;
        let outstart: *const u8 = out;
        let instart: *const u8 = input;
        let mut c: u32;
        let mut d: u32;
        let mut trailing: i32;

        if out.is_null() || outlen.is_null() || inlen.is_null() || input.is_null() {
            return -1;
        }
        let outend: *const u8 = out.add(*outlen as usize);
        let inend: *const u8 = input.add(*inlen as usize);
        while input < inend {
            d = *input as _;
            input = input.add(1);
            if d < 0x80 {
                c = d;
                trailing = 0;
            } else if d < 0xC0 {
                // trailing byte in leading position
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -2;
            } else if d < 0xE0 {
                c = d & 0x1F;
                trailing = 1;
            } else if d < 0xF0 {
                c = d & 0x0F;
                trailing = 2;
            } else if d < 0xF8 {
                c = d & 0x07;
                trailing = 3;
            } else {
                // no chance for this in Ascii
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -2;
            }

            if inend.offset_from(input) < trailing as isize {
                break;
            }

            while trailing != 0 {
                trailing -= 1;
                d = *input as _;
                input = input.add(1);
                if d & 0xC0 != 0x80 {
                    *outlen = out.offset_from(outstart) as _;
                    *inlen = processed.offset_from(instart) as _;
                    return -2;
                }
                c <<= 6;
                c |= d & 0x3F;
            }

            // assertion: c is a single UTF-4 value
            if c < 0x80
                && c != quote_char as u32
                && c != '&' as u32
                && c != '<' as u32
                && c != '>' as u32
            {
                if out >= outend as _ {
                    break;
                }
                *out = c as _;
                out = out.add(1);
            } else {
                // Try to lookup a predefined HTML entity for it
                let cp = if let Some(ent) = html_entity_value_lookup(c) {
                    Cow::Borrowed(ent.name)
                } else {
                    Cow::Owned(format!("#{c}"))
                };
                let len = cp.len();
                if outend.offset_from(out) < len as isize + 2 {
                    break;
                }
                *out = b'&';
                out = out.add(1);
                memcpy(out as _, cp.as_ptr() as _, len as usize);
                out = out.add(len as usize);
                *out = b';';
                out = out.add(1);
            }
            processed = input;
        }
        *outlen = out.offset_from(outstart) as _;
        *inlen = processed.offset_from(instart) as _;
        0
    }
}

/// Check if an attribute is of content type Script
///
/// Returns 1 is the attribute is a script 0 otherwise
#[doc(alias = "htmlIsScriptAttribute")]
pub fn html_is_script_attribute(name: &str) -> bool {
    // all script attributes start with 'on'
    if !name.starts_with("on") {
        return false;
    }

    HTML_SCRIPT_ATTRIBUTES.iter().any(|&attr| attr == name)
}

/// Set and return the previous value for handling HTML omitted tags.
///
/// Returns the last value for 0 for no handling, 1 for auto insertion.
#[doc(alias = "htmlHandleOmittedElem")]
pub unsafe fn html_handle_omitted_elem(val: i32) -> i32 {
    let old: i32 = HTML_OMITTED_DEFAULT_VALUE.load(Ordering::Acquire);

    HTML_OMITTED_DEFAULT_VALUE.store(val, Ordering::Release);
    old
}

/// Create a new input stream structure
/// Returns the new input stream or NULL
#[doc(alias = "htmlNewInputStream")]
#[cfg(feature = "libxml_push")]
unsafe fn html_new_input_stream(ctxt: HtmlParserCtxtPtr) -> HtmlParserInput {
    unsafe {
        let mut input = HtmlParserInput {
            filename: None,
            directory: None,
            base: vec![],
            cur: 0,
            buf: None,
            line: 1,
            col: 1,
            free: None,
            version: None,
            consumed: 0,
            length: 0,
            ..Default::default()
        };
        if !ctxt.is_null() {
            input.id = (*ctxt).input_id;
            (*ctxt).input_id += 1;
        }
        input
    }
}

/// Create a parser context for using the HTML parser in push mode
/// The value of `filename` is used for fetching external entities
/// and error/warning reports.
///
/// Returns the new parser context or NULL
#[doc(alias = "htmlCreatePushParserCtxt")]
#[cfg(feature = "libxml_push")]
pub unsafe fn html_create_push_parser_ctxt(
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
    chunk: *const c_char,
    size: i32,
    filename: Option<&str>,
    enc: XmlCharEncoding,
) -> HtmlParserCtxtPtr {
    unsafe {
        use std::slice::from_raw_parts;

        use crate::io::{XmlParserInputBuffer, xml_parser_get_directory};

        xml_init_parser();

        let buf = XmlParserInputBuffer::new(enc);

        let ctxt: HtmlParserCtxtPtr = html_new_sax_parser_ctxt(sax, user_data);
        if ctxt.is_null() {
            return null_mut();
        }
        if matches!(enc, XmlCharEncoding::UTF8) || buf.encoder.is_some() {
            (*ctxt).charset = XmlCharEncoding::UTF8;
        }
        if filename.is_none() {
            (*ctxt).directory = None;
        } else if let Some(dir) = filename.and_then(xml_parser_get_directory) {
            (*ctxt).directory = Some(dir.to_string_lossy().into_owned());
        }

        let mut input_stream = html_new_input_stream(ctxt);

        if let Some(filename) = filename {
            let canonic = canonic_path(filename);
            input_stream.filename = Some(canonic.into_owned());
        } else {
            input_stream.filename = None;
        }
        input_stream.buf = Some(Rc::new(RefCell::new(buf)));
        input_stream.reset_base();

        (*ctxt).input_push(input_stream);

        if size > 0
            && !chunk.is_null()
            && (*ctxt).input().is_some()
            && (*ctxt).input().unwrap().buf.is_some()
        {
            let base: size_t = (*ctxt).input().unwrap().get_base();
            let cur = (*ctxt).input().unwrap().offset_from_base();

            (*ctxt)
                .input_mut()
                .unwrap()
                .buf
                .as_mut()
                .unwrap()
                .borrow_mut()
                .push_bytes(from_raw_parts(chunk as *const u8, size as usize));
            (*ctxt).input_mut().unwrap().set_base_and_cursor(base, cur);
        }
        (*ctxt).progressive = 1;

        ctxt
    }
}

/// Try to find if a sequence (first, next, third) or just (first next) or
/// (first) is available in the input stream.
/// This function has a side effect of (possibly) incrementing (*ctxt).checkIndex
/// to avoid rescanning sequences of bytes, it DOES change the state of the
/// parser, do not use liberally.
/// This is basically similar to xmlParseLookupSequence()
///
/// Returns the index to the current parsing point if the full sequence is available, -1 otherwise.
#[doc(alias = "htmlParseLookupSequence")]
#[cfg(feature = "libxml_push")]
unsafe fn html_parse_lookup_sequence(
    ctxt: HtmlParserCtxtPtr,
    first: XmlChar,
    next: XmlChar,
    third: XmlChar,
    ignoreattrval: i32,
) -> i32 {
    unsafe {
        let mut quote: i32;

        if (*ctxt).input().is_none() {
            return -1;
        }

        let base: size_t = (*ctxt).check_index as _;
        quote = (*ctxt).end_check_state;

        let buf = (*ctxt).content_bytes();
        let mut len = buf.len();

        // take into account the sequence length
        if third != 0 {
            len -= 2;
        } else if next != 0 {
            len -= 1;
        }
        for base in base..len {
            if base >= INT_MAX as usize / 2 {
                (*ctxt).check_index = 0;
                (*ctxt).end_check_state = 0;
                return base as i32 - 2;
            }
            if ignoreattrval != 0 {
                if quote != 0 {
                    if buf[base] == quote as u8 {
                        quote = 0;
                    }
                    continue;
                }
                if buf[base] == b'"' || buf[base] == b'\'' {
                    quote = buf[base] as _;
                    continue;
                }
            }
            if buf[base] == first {
                if third != 0 {
                    if buf[base + 1] != next || buf[base + 2] != third {
                        continue;
                    }
                } else if next != 0 && buf[base + 1] != next {
                    continue;
                }
                (*ctxt).check_index = 0;
                (*ctxt).end_check_state = 0;
                return base as _;
            }
        }
        (*ctxt).check_index = base.max(len) as _;
        (*ctxt).end_check_state = quote;
        -1
    }
}

/// Try to find a comment end tag in the input stream
/// The search includes "-->" as well as WHATWG-recommended incorrectly-closed tags.
/// (See https://html.spec.whatwg.org/multipage/parsing.html#parse-error-incorrectly-closed-comment)
/// This function has a side effect of (possibly) incrementing (*ctxt).checkIndex
/// to avoid rescanning sequences of bytes,
/// it DOES change the state of the parser, do not use liberally.  
/// This wraps to htmlParseLookupSequence()
///
/// Returns the index to the current parsing point if the full sequence is available, -1 otherwise.
#[doc(alias = "htmlParseLookupCommentEnd")]
#[cfg(feature = "libxml_push")]
unsafe fn html_parse_lookup_comment_end(ctxt: HtmlParserCtxtPtr) -> i32 {
    unsafe {
        let mut mark: i32;

        loop {
            mark = html_parse_lookup_sequence(ctxt, b'-', b'-', 0, 0);
            if mark < 0 {
                break;
            }
            if (*ctxt).nth_byte(mark as usize + 2) == b'>'
                || ((*ctxt).nth_byte(mark as usize + 2) == b'!'
                    && (*ctxt).nth_byte(mark as usize + 3) == b'>')
            {
                (*ctxt).check_index = 0;
                break;
            }
            let offset = if (*ctxt).nth_byte(mark as usize + 2) == b'!' {
                3
            } else {
                2
            };
            if mark + offset >= (*ctxt).input().unwrap().remainder_len() as i32 {
                (*ctxt).check_index = mark as _;
                return -1;
            }
            (*ctxt).check_index = mark as i64 + 1;
        }
        mark
    }
}

/// Try to progress on parsing
///
/// Returns zero if no parsing was possible
#[doc(alias = "htmlParseTryOrFinish")]
#[cfg(feature = "libxml_push")]
unsafe fn html_parse_try_or_finish(ctxt: HtmlParserCtxtPtr, terminate: i32) -> i32 {
    unsafe {
        let ret: i32 = 0;
        let mut avail = 0;

        'done: loop {
            let Some(input) = (*ctxt).input() else {
                break;
            };
            avail = (*input).remainder_len();
            if avail == 0 && terminate != 0 {
                html_auto_close_on_end(ctxt);
                if (*ctxt).name_tab.is_empty()
                    && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
                {
                    // SAX: end of the document processing.
                    (*ctxt).instate = XmlParserInputState::XmlParserEOF;
                    if let Some(end_document) =
                        (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_document)
                    {
                        end_document((*ctxt).user_data.clone());
                    }
                }
            }
            if avail < 1 {
                // goto done;
                break 'done;
            }
            // This is done to make progress and avoid an infinite loop
            // if a parsing attempt was aborted by hitting a NUL byte. After
            // changing html_current_char, this probably isn't necessary anymore.
            // We should consider removing this check.
            if (*ctxt).current_byte() == 0 {
                (*ctxt).advance(1);
                continue;
            }

            match (*ctxt).instate {
                XmlParserInputState::XmlParserEOF => {
                    // Document parsing is done !
                    // goto done;
                    break 'done;
                }
                XmlParserInputState::XmlParserStart => {
                    // Very first chars read from the document flow.
                    let cur = (*ctxt).current_byte();
                    if xml_is_blank_char(cur as u32) {
                        html_skip_blank_chars(&mut *ctxt);
                        avail = input.remainder_len();
                    }
                    if let Some(set_document_locator) = (*ctxt)
                        .sax
                        .as_deref_mut()
                        .and_then(|sax| sax.set_document_locator)
                    {
                        set_document_locator((*ctxt).user_data.clone(), xml_default_sax_locator());
                    }
                    if (*ctxt).disable_sax == 0 {
                        if let Some(start_document) = (*ctxt)
                            .sax
                            .as_deref_mut()
                            .and_then(|sax| sax.start_document)
                        {
                            start_document((*ctxt).user_data.clone());
                        }
                    }

                    if (*ctxt).content_bytes().len() >= 9
                        && (*ctxt).content_bytes().starts_with(b"<!")
                        && (*ctxt).content_bytes()[2..9].eq_ignore_ascii_case(b"DOCTYPE")
                    {
                        if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 1) < 0 {
                            // goto done;
                            break 'done;
                        }
                        html_parse_doc_type_decl(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserProlog;
                    } else {
                        (*ctxt).instate = XmlParserInputState::XmlParserMisc;
                    }
                }
                XmlParserInputState::XmlParserMisc => {
                    html_skip_blank_chars(&mut *ctxt);
                    avail = input.remainder_len();
                    // no chars input buffer
                    if avail < 1 {
                        // goto done;
                        break 'done;
                    }
                    // not enough chars input buffer
                    if avail < 2 && terminate == 0 {
                        // goto done;
                        break 'done;
                    }
                    if (*ctxt).content_bytes().starts_with(b"<!--") {
                        if terminate == 0 && html_parse_lookup_comment_end(ctxt) < 0 {
                            // goto done;
                            break 'done;
                        }
                        html_parse_comment(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserMisc;
                    } else if (*ctxt).content_bytes().starts_with(b"<?") {
                        if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 0) < 0 {
                            // goto done;
                            break 'done;
                        }
                        html_parse_pi(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserMisc;
                    } else if (*ctxt).content_bytes().len() >= 9
                        && (*ctxt).content_bytes().starts_with(b"<!")
                        && (*ctxt).content_bytes()[2..9].eq_ignore_ascii_case(b"DOCTYPE")
                    {
                        if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 1) < 0 {
                            // goto done;
                            break 'done;
                        }
                        html_parse_doc_type_decl(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserProlog;
                    } else if (*ctxt).content_bytes().starts_with(b"<!") && avail < 9 {
                        // goto done;
                        break 'done;
                    } else {
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    }
                }
                XmlParserInputState::XmlParserProlog => {
                    html_skip_blank_chars(&mut *ctxt);
                    avail = input.remainder_len();
                    if avail < 2 {
                        // goto done;
                        break 'done;
                    }
                    if (*ctxt).content_bytes().starts_with(b"<!--") {
                        if terminate == 0 && html_parse_lookup_comment_end(ctxt) < 0 {
                            // goto done;
                            break 'done;
                        }
                        html_parse_comment(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserProlog;
                    } else if (*ctxt).content_bytes().starts_with(b"<?") {
                        if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 0) < 0 {
                            // goto done;
                            break 'done;
                        }
                        html_parse_pi(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserProlog;
                    } else if (*ctxt).content_bytes().starts_with(b"<!") && avail < 4 {
                        // goto done;
                        break 'done;
                    } else {
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    }
                }
                XmlParserInputState::XmlParserEpilog => {
                    avail = input.remainder_len();
                    if avail < 1 {
                        // goto done;
                        break 'done;
                    }
                    if xml_is_blank_char((*ctxt).current_byte() as u32) {
                        html_parse_char_data(ctxt);
                        // goto done;
                        break 'done;
                    }
                    if avail < 2 {
                        // goto done;
                        break 'done;
                    }
                    if (*ctxt).content_bytes().starts_with(b"<!--") {
                        if terminate == 0 && html_parse_lookup_comment_end(ctxt) < 0 {
                            // goto done;
                            break 'done;
                        }
                        html_parse_comment(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserEpilog;
                    } else if (*ctxt).content_bytes().starts_with(b"<?") {
                        if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 0) < 0 {
                            // goto done;
                            break 'done;
                        }
                        html_parse_pi(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserEpilog;
                    } else if (*ctxt).content_bytes().starts_with(b"<!") && avail < 4 {
                        // goto done;
                        break 'done;
                    } else {
                        (*ctxt).err_no = XmlParserErrors::XmlErrDocumentEnd as i32;
                        (*ctxt).well_formed = 0;
                        (*ctxt).instate = XmlParserInputState::XmlParserEOF;
                        if let Some(end_document) =
                            (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_document)
                        {
                            end_document((*ctxt).user_data.clone());
                        }
                        // goto done;
                        break 'done;
                    }
                }
                XmlParserInputState::XmlParserStartTag => 'to_break: {
                    // no chars in buffer
                    if avail < 1 {
                        // goto done;
                        break 'done;
                    }
                    // not enough chars in buffer
                    if avail < 2 && terminate == 0 {
                        // goto done;
                        break 'done;
                    }
                    if (*ctxt).current_byte() != b'<' {
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                        break 'to_break;
                    }
                    if (*ctxt).nth_byte(1) == b'/' {
                        (*ctxt).instate = XmlParserInputState::XmlParserEndTag;
                        (*ctxt).check_index = 0;
                        break 'to_break;
                    }
                    if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 1) < 0 {
                        // goto done;
                        break 'done;
                    }

                    // Capture start position
                    let mut node_info = HtmlParserNodeInfo::default();
                    if (*ctxt).record_info != 0 {
                        node_info.begin_pos = (*ctxt).input().unwrap().consumed
                            + (*ctxt).input().unwrap().offset_from_base() as u64;
                        node_info.begin_line = (*ctxt).input().unwrap().line as _;
                    }

                    let failed: i32 = html_parse_start_tag(ctxt);
                    let Some(name) = (*ctxt).name.clone().filter(|_| failed != -1) else {
                        if (*ctxt).current_byte() == b'>' {
                            (*ctxt).skip_char();
                        }
                        break 'to_break;
                    };

                    // Lookup the info for that element.
                    let info = html_tag_lookup(&name);
                    if info.is_none() {
                        html_parse_err(
                            ctxt,
                            XmlParserErrors::XmlHTMLUnknownTag,
                            format!("Tag {name} invalid\n").as_str(),
                            Some(&name),
                            None,
                        );
                    }

                    // Check for an Empty Element labeled the XML/SGML way
                    if (*ctxt).content_bytes().starts_with(b"/>") {
                        (*ctxt).advance(2);
                        if let Some(end_element) =
                            (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element)
                        {
                            end_element((*ctxt).user_data.clone(), &name);
                        }
                        html_name_pop(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                        break 'to_break;
                    }

                    if (*ctxt).current_byte() == b'>' {
                        (*ctxt).skip_char();
                    } else {
                        html_parse_err(
                            ctxt,
                            XmlParserErrors::XmlErrGtRequired,
                            format!("Couldn't find end of Start Tag {name}\n").as_str(),
                            Some(&name),
                            None,
                        );

                        // end of parsing of this node.
                        if Some(name.as_str()) == (*ctxt).name.as_deref() {
                            (*ctxt).node_pop();
                            html_name_pop(ctxt);
                        }

                        if (*ctxt).record_info != 0 {
                            html_node_info_push(ctxt, Rc::new(RefCell::new(node_info)));
                        }

                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                        break 'to_break;
                    }

                    // Check for an Empty Element from DTD definition
                    if info.is_some_and(|info| info.empty != 0) {
                        if let Some(end_element) =
                            (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element)
                        {
                            end_element((*ctxt).user_data.clone(), &name);
                        }
                        html_name_pop(ctxt);
                    }

                    if (*ctxt).record_info != 0 {
                        html_node_info_push(ctxt, Rc::new(RefCell::new(node_info)));
                    }

                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                }
                XmlParserInputState::XmlParserContent => 'to_break: {
                    let mut chr: [XmlChar; 2] = [0, 0];

                    // Handle preparsed entities and charRef
                    if (*ctxt).token != 0 {
                        chr[0] = (*ctxt).token as _;
                        html_check_paragraph(ctxt);
                        if let Some(characters) =
                            (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
                        {
                            let s = from_utf8(&chr[..1]).expect("Internal Error");
                            characters((*ctxt).user_data.clone(), s);
                        }
                        (*ctxt).token = 0;
                        (*ctxt).check_index = 0;
                    }
                    if avail == 1 && terminate != 0 {
                        let cur = (*ctxt).current_byte();
                        if cur != b'<' && cur != b'&' {
                            if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                                chr[0] = cur;
                                let s = from_utf8(&chr[..1]).expect("Internal Error");
                                if xml_is_blank_char(cur as u32) {
                                    if (*ctxt).keep_blanks != 0 {
                                        if let Some(characters) = sax.characters {
                                            characters((*ctxt).user_data.clone(), s);
                                        }
                                    } else if let Some(ignorable_whitespace) =
                                        sax.ignorable_whitespace
                                    {
                                        ignorable_whitespace((*ctxt).user_data.clone(), s);
                                    }
                                } else {
                                    html_check_paragraph(ctxt);
                                    if let Some(characters) = sax.characters {
                                        characters((*ctxt).user_data.clone(), s);
                                    }
                                }
                            }
                            (*ctxt).token = 0;
                            (*ctxt).check_index = 0;
                            (*ctxt).input_mut().unwrap().cur += 1;
                            break 'to_break;
                        }
                    }
                    if avail < 2 {
                        // goto done;
                        break 'done;
                    }
                    if (*ctxt).name.as_deref() == Some("script")
                        || (*ctxt).name.as_deref() == Some("style")
                    {
                        // Handle SCRIPT/STYLE separately
                        if terminate == 0 {
                            let idx: i32 = html_parse_lookup_sequence(ctxt, b'<', b'/', 0, 0);
                            if idx < 0 {
                                // goto done;
                                break 'done;
                            }
                            let val = (*ctxt).nth_byte(idx as usize + 2);
                            if val == 0 {
                                // bad cut of input
                                // FIXME: htmlParseScript checks for additional characters after '</'.
                                (*ctxt).check_index = idx as _;
                                // goto done;
                                break 'done;
                            }
                        }
                        html_parse_script(ctxt);
                        if (*ctxt).content_bytes().starts_with(b"</") {
                            (*ctxt).instate = XmlParserInputState::XmlParserEndTag;
                            (*ctxt).check_index = 0;
                            break 'to_break;
                        }
                    } else if (*ctxt).content_bytes().starts_with(b"<!") {
                        if avail < 4 {
                            // goto done;
                            break 'done;
                        }
                        // Sometimes DOCTYPE arrives in the middle of the document
                        if (*ctxt).content_bytes().len() >= 9
                            && (*ctxt).content_bytes()[2..9].eq_ignore_ascii_case(b"DOCTYPE")
                        {
                            if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 1) < 0
                            {
                                // goto done;
                                break 'done;
                            }
                            html_parse_err(
                                ctxt,
                                XmlParserErrors::XmlHTMLStrucureError,
                                "Misplaced DOCTYPE declaration\n",
                                Some("DOCTYPE"),
                                None,
                            );
                            html_parse_doc_type_decl(ctxt);
                        } else if (*ctxt).content_bytes()[2..].starts_with(b"--") {
                            if terminate == 0 && html_parse_lookup_comment_end(ctxt) < 0 {
                                // goto done;
                                break 'done;
                            }
                            html_parse_comment(ctxt);
                            (*ctxt).instate = XmlParserInputState::XmlParserContent;
                        } else {
                            if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 0) < 0
                            {
                                // goto done;
                                break 'done;
                            }
                            html_skip_bogus_comment(ctxt);
                        }
                    } else if (*ctxt).content_bytes().starts_with(b"<?") {
                        if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 0) < 0 {
                            // goto done;
                            break 'done;
                        }
                        html_parse_pi(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    } else if (*ctxt).content_bytes().starts_with(b"</") {
                        (*ctxt).instate = XmlParserInputState::XmlParserEndTag;
                        (*ctxt).check_index = 0;
                        break 'to_break;
                    } else if (*ctxt).current_byte() == b'<'
                        && (*ctxt).nth_byte(1).is_ascii_alphabetic()
                    {
                        if terminate == 0 && (*ctxt).nth_byte(1) == 0 {
                            // goto done;
                            break 'done;
                        }
                        (*ctxt).instate = XmlParserInputState::XmlParserStartTag;
                        (*ctxt).check_index = 0;
                        break 'to_break;
                    } else if (*ctxt).current_byte() == b'<' {
                        if (*ctxt).disable_sax == 0 {
                            if let Some(characters) =
                                (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
                            {
                                characters((*ctxt).user_data.clone(), "<");
                            }
                        }
                        (*ctxt).skip_char();
                    } else {
                        // check that the text sequence is complete
                        // before handing out the data to the parser
                        // to avoid problems with erroneous end of
                        // data detection.
                        if terminate == 0 && html_parse_lookup_sequence(ctxt, b'<', 0, 0, 0) < 0 {
                            // goto done;
                            break 'done;
                        }
                        (*ctxt).check_index = 0;
                        while !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
                            && !(*ctxt).content_bytes().is_empty()
                            && (*ctxt).current_byte() != b'<'
                        {
                            if (*ctxt).current_byte() == b'&' {
                                html_parse_reference(ctxt);
                            } else {
                                html_parse_char_data(ctxt);
                            }
                        }
                    }
                }
                XmlParserInputState::XmlParserEndTag => {
                    if avail < 2 {
                        // goto done;
                        break 'done;
                    }
                    if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 0) < 0 {
                        // goto done;
                        break 'done;
                    }
                    html_parse_end_tag(ctxt);
                    if (*ctxt).name_tab.is_empty() {
                        (*ctxt).instate = XmlParserInputState::XmlParserEpilog;
                    } else {
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    }
                    (*ctxt).check_index = 0;
                }
                XmlParserInputState::XmlParserCDATASection => {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "HPP: internal error, state == CDATA\n",
                        None,
                        None,
                    );
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    (*ctxt).check_index = 0;
                }
                XmlParserInputState::XmlParserDTD => {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "HPP: internal error, state == DTD\n",
                        None,
                        None,
                    );
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    (*ctxt).check_index = 0;
                }
                XmlParserInputState::XmlParserComment => {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "HPP: internal error, state == COMMENT\n",
                        None,
                        None,
                    );
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    (*ctxt).check_index = 0;
                }
                XmlParserInputState::XmlParserPI => {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "HPP: internal error, state == PI\n",
                        None,
                        None,
                    );
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    (*ctxt).check_index = 0;
                }
                XmlParserInputState::XmlParserEntityDecl => {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "HPP: internal error, state == ENTITY_DECL\n",
                        None,
                        None,
                    );
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    (*ctxt).check_index = 0;
                }
                XmlParserInputState::XmlParserEntityValue => {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "HPP: internal error, state == ENTITY_VALUE\n",
                        None,
                        None,
                    );
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    (*ctxt).check_index = 0;
                }
                XmlParserInputState::XmlParserAttributeValue => {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "HPP: internal error, state == ATTRIBUTE_VALUE\n",
                        None,
                        None,
                    );
                    (*ctxt).instate = XmlParserInputState::XmlParserStartTag;
                    (*ctxt).check_index = 0;
                }
                XmlParserInputState::XmlParserSystemLiteral => {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "HPP: internal error, state == XML_PARSER_SYSTEM_LITERAL\n",
                        None,
                        None,
                    );
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    (*ctxt).check_index = 0;
                }
                XmlParserInputState::XmlParserIgnore => {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "HPP: internal error, state == XML_PARSER_IGNORE\n",
                        None,
                        None,
                    );
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    (*ctxt).check_index = 0;
                }
                XmlParserInputState::XmlParserPublicLiteral => {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInternalError,
                        "HPP: internal error, state == XML_PARSER_LITERAL\n",
                        None,
                        None,
                    );
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    (*ctxt).check_index = 0;
                }
            }
        }
        // done:
        if avail == 0 && terminate != 0 {
            html_auto_close_on_end(ctxt);
            if (*ctxt).name_tab.is_empty()
                && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
            {
                // SAX: end of the document processing.
                (*ctxt).instate = XmlParserInputState::XmlParserEOF;
                if let Some(end_document) =
                    (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_document)
                {
                    end_document((*ctxt).user_data.clone());
                }
            }
        }
        if (*ctxt).options & HtmlParserOption::HtmlParseNodefdtd as i32 == 0
            && (terminate != 0
                || matches!(
                    (*ctxt).instate,
                    XmlParserInputState::XmlParserEOF | XmlParserInputState::XmlParserEpilog
                ))
        {
            if let Some(mut my_doc) = (*ctxt).my_doc {
                let dtd = my_doc.get_int_subset();
                if dtd.is_none() {
                    my_doc.int_subset = xml_create_int_subset(
                        (*ctxt).my_doc,
                        Some("html"),
                        Some("-//W3C//DTD HTML 4.0 Transitional//EN"),
                        Some("http://www.w3.org/TR/REC-html40/loose.dtd"),
                    );
                }
            }
        }
        ret
    }
}

/// Parse a Chunk of memory
///
/// Returns zero if no error, the xmlParserErrors otherwise.
#[doc(alias = "htmlParseChunk")]
#[cfg(feature = "libxml_push")]
pub unsafe fn html_parse_chunk(
    ctxt: HtmlParserCtxtPtr,
    chunk: *const c_char,
    size: i32,
    terminate: i32,
) -> i32 {
    unsafe {
        use std::slice::from_raw_parts;

        if ctxt.is_null() || (*ctxt).input().is_none() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                "htmlParseChunk: context error\n",
                None,
                None,
            );
            return XmlParserErrors::XmlErrInternalError as i32;
        }
        if size > 0
            && !chunk.is_null()
            && (*ctxt).input().is_some()
            && (*ctxt).input().unwrap().buf.is_some()
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            let base: size_t = (*ctxt).input().unwrap().get_base();
            let cur = (*ctxt).input().unwrap().offset_from_base();

            let res: i32 = (*ctxt)
                .input_mut()
                .unwrap()
                .buf
                .as_mut()
                .unwrap()
                .borrow_mut()
                .push_bytes(from_raw_parts(chunk as *const u8, size as usize));
            (*ctxt).input_mut().unwrap().set_base_and_cursor(base, cur);
            if res < 0 {
                html_err_memory(ctxt, None);
                return (*ctxt).err_no;
            }
        } else if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
            && ((*ctxt).input().is_some() && (*ctxt).input().unwrap().buf.is_some())
        {
            let input = (*ctxt).input_mut().unwrap().buf.as_mut().unwrap();
            if input.borrow().encoder.is_some()
                && input.borrow().buffer.is_some()
                && input.borrow().raw.is_some()
            {
                let base: size_t = (*ctxt).input().unwrap().get_base();
                let current = (*ctxt).input().unwrap().offset_from_base();

                let res = input.borrow_mut().decode(terminate != 0);
                (*ctxt)
                    .input_mut()
                    .unwrap()
                    .set_base_and_cursor(base, current);
                if res.is_err() {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidEncoding,
                        "encoder error\n",
                        None,
                        None,
                    );
                    return XmlParserErrors::XmlErrInvalidEncoding as i32;
                }
            }
        }
        html_parse_try_or_finish(ctxt, terminate);
        if terminate != 0 {
            if !matches!(
                (*ctxt).instate,
                XmlParserInputState::XmlParserEOF
                    | XmlParserInputState::XmlParserEpilog
                    | XmlParserInputState::XmlParserMisc
            ) {
                (*ctxt).err_no = XmlParserErrors::XmlErrDocumentEnd as i32;
                (*ctxt).well_formed = 0;
            }
            if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                if let Some(end_document) =
                    (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_document)
                {
                    end_document((*ctxt).user_data.clone());
                }
            }
            (*ctxt).instate = XmlParserInputState::XmlParserEOF;
        }
        (*ctxt).err_no
    }
}

/// Free all the memory used by a parser context.   
/// However the parsed document in (*ctxt).myDoc is not freed.
#[doc(alias = "htmlFreeParserCtxt")]
pub unsafe fn html_free_parser_ctxt(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        xml_free_parser_ctxt(ctxt);
    }
}

/// This is the set of XML parser options that can be passed down
/// to the xmlReadDoc() and similar calls.
#[doc(alias = "xmlParserOption")]
#[repr(C)]
pub enum HtmlParserOption {
    HtmlParseRecover = 1 << 0,    /* Relaxed parsing */
    HtmlParseNodefdtd = 1 << 2,   /* do not default a doctype if not found */
    HtmlParseNoerror = 1 << 5,    /* suppress error reports */
    HtmlParseNowarning = 1 << 6,  /* suppress warning reports */
    HtmlParsePedantic = 1 << 7,   /* pedantic error reporting */
    HtmlParseNoblanks = 1 << 8,   /* remove blank nodes */
    HtmlParseNonet = 1 << 11,     /* Forbid network access */
    HtmlParseNoimplied = 1 << 13, /* Do not add implied html/body... elements */
    HtmlParseCompact = 1 << 16,   /* compact small text nodes */
    HtmlParseIgnoreEnc = 1 << 21, /* ignore internal document encoding hint */
}

/// Reset a parser context
#[doc(alias = "htmlCtxtReset")]
pub unsafe fn html_ctxt_reset(ctxt: HtmlParserCtxtPtr) {
    unsafe {
        if ctxt.is_null() {
            return;
        }

        xml_init_parser();

        while (*ctxt).input_pop().is_some() {
            // drop input
        }
        (*ctxt).input_tab.clear();

        (*ctxt).space_tab.clear();

        (*ctxt).node_tab.clear();
        (*ctxt).node = None;

        (*ctxt).name_tab.clear();
        (*ctxt).name = None;

        (*ctxt).ns_tab.clear();

        (*ctxt).version = None;
        (*ctxt).encoding = None;
        (*ctxt).directory = None;
        (*ctxt).ext_sub_uri = None;
        (*ctxt).ext_sub_system = None;
        if let Some(my_doc) = (*ctxt).my_doc.take() {
            xml_free_doc(my_doc);
        }

        (*ctxt).standalone = -1;
        (*ctxt).has_external_subset = 0;
        (*ctxt).has_perefs = 0;
        (*ctxt).html = 1;
        (*ctxt).external = 0;
        (*ctxt).instate = XmlParserInputState::XmlParserStart;
        (*ctxt).token = 0;

        (*ctxt).well_formed = 1;
        (*ctxt).ns_well_formed = 1;
        (*ctxt).disable_sax = 0;
        (*ctxt).valid = 1;
        (*ctxt).vctxt.user_data = Some(GenericErrorContext::new(ctxt));
        (*ctxt).vctxt.flags = XML_VCTXT_USE_PCTXT as _;
        (*ctxt).vctxt.error = Some(parser_validity_error);
        (*ctxt).vctxt.warning = Some(parser_validity_warning);
        (*ctxt).record_info = 0;
        (*ctxt).check_index = 0;
        (*ctxt).end_check_state = 0;
        (*ctxt).in_subset = 0;
        (*ctxt).err_no = XmlParserErrors::XmlErrOK as i32;
        (*ctxt).depth = 0;
        (*ctxt).charset = XmlCharEncoding::None;
        #[cfg(feature = "catalog")]
        {
            (*ctxt).catalogs = None;
        }
        (*ctxt).node_seq.clear();

        (*ctxt).atts_default.clear();

        let _ = (*ctxt).atts_special.take().map(|t| t.into_inner());

        (*ctxt).nb_errors = 0;
        (*ctxt).nb_warnings = 0;
        if (*ctxt).last_error.is_err() {
            (*ctxt).last_error.reset();
        }
    }
}

/// Applies the options to the parser context
///
/// Returns 0 in case of success, the set of unknown or unimplemented options in case of error.
#[doc(alias = "htmlCtxtUseOptions")]
pub unsafe fn html_ctxt_use_options(ctxt: HtmlParserCtxtPtr, mut options: i32) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return -1;
        }

        if options & HtmlParserOption::HtmlParseNowarning as i32 != 0 {
            if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                sax.warning = None;
            }
            (*ctxt).vctxt.warning = None;
            options -= XmlParserOption::XmlParseNoWarning as i32;
            (*ctxt).options |= XmlParserOption::XmlParseNoWarning as i32;
        }
        if options & HtmlParserOption::HtmlParseNoerror as i32 != 0 {
            if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                sax.error = None;
                sax.fatal_error = None;
            }
            (*ctxt).vctxt.error = None;
            options -= XmlParserOption::XmlParseNoError as i32;
            (*ctxt).options |= XmlParserOption::XmlParseNoError as i32;
        }
        if options & HtmlParserOption::HtmlParsePedantic as i32 != 0 {
            (*ctxt).pedantic = 1;
            options -= XmlParserOption::XmlParsePedantic as i32;
            (*ctxt).options |= XmlParserOption::XmlParsePedantic as i32;
        } else {
            (*ctxt).pedantic = 0;
        }
        if options & XmlParserOption::XmlParseNoBlanks as i32 != 0 {
            (*ctxt).keep_blanks = 0;
            if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                sax.ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
            }
            options -= XmlParserOption::XmlParseNoBlanks as i32;
            (*ctxt).options |= XmlParserOption::XmlParseNoBlanks as i32;
        } else {
            (*ctxt).keep_blanks = 1;
        }
        if options & HtmlParserOption::HtmlParseRecover as i32 != 0 {
            (*ctxt).recovery = 1;
            options -= HtmlParserOption::HtmlParseRecover as i32;
        } else {
            (*ctxt).recovery = 0;
        }
        if options & HtmlParserOption::HtmlParseCompact as i32 != 0 {
            (*ctxt).options |= HtmlParserOption::HtmlParseCompact as i32;
            options -= HtmlParserOption::HtmlParseCompact as i32;
        }
        if options & XmlParserOption::XmlParseHuge as i32 != 0 {
            (*ctxt).options |= XmlParserOption::XmlParseHuge as i32;
            options -= XmlParserOption::XmlParseHuge as i32;
        }
        if options & HtmlParserOption::HtmlParseNodefdtd as i32 != 0 {
            (*ctxt).options |= HtmlParserOption::HtmlParseNodefdtd as i32;
            options -= HtmlParserOption::HtmlParseNodefdtd as i32;
        }
        if options & HtmlParserOption::HtmlParseIgnoreEnc as i32 != 0 {
            (*ctxt).options |= HtmlParserOption::HtmlParseIgnoreEnc as i32;
            options -= HtmlParserOption::HtmlParseIgnoreEnc as i32;
        }
        if options & HtmlParserOption::HtmlParseNoimplied as i32 != 0 {
            (*ctxt).options |= HtmlParserOption::HtmlParseNoimplied as i32;
            options -= HtmlParserOption::HtmlParseNoimplied as i32;
        }
        (*ctxt).dict_names = 0;
        (*ctxt).linenumbers = 1;
        options
    }
}

/// Common front-end for the htmlRead functions
///
/// Returns the resulting document tree or NULL
#[doc(alias = "htmlDoRead")]
unsafe fn html_do_read(
    ctxt: HtmlParserCtxtPtr,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
    reuse: i32,
) -> Option<HtmlDocPtr> {
    unsafe {
        html_ctxt_use_options(ctxt, options);
        (*ctxt).html = 1;
        if let Some(encoding) = encoding {
            if let Some(handler) = find_encoding_handler(encoding) {
                (*ctxt).switch_to_encoding(handler);
                (*ctxt).input_mut().unwrap().encoding = Some(encoding.to_owned());
            }
        }
        if url.is_some() {
            if let Some(input) = (*ctxt).input_mut().filter(|input| input.filename.is_none()) {
                input.filename = url.map(|u| u.to_owned());
            }
        }
        html_parse_document(ctxt);
        let ret = (*ctxt).my_doc.take();
        if reuse == 0 {
            xml_free_parser_ctxt(ctxt);
        }
        ret
    }
}

/// Parse an XML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "htmlReadDoc")]
pub unsafe fn html_read_doc(
    cur: *const XmlChar,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> Option<HtmlDocPtr> {
    unsafe {
        if cur.is_null() {
            return None;
        }

        xml_init_parser();
        let ctxt: HtmlParserCtxtPtr = html_create_doc_parser_ctxt(cur, None);
        if ctxt.is_null() {
            return None;
        }
        html_do_read(ctxt, url, encoding, options, 0)
    }
}

/// Parse an XML file from the filesystem or the network.
///
/// Returns the resulting document tree
#[doc(alias = "htmlReadFile")]
pub unsafe fn html_read_file(
    filename: &str,
    encoding: Option<&str>,
    options: i32,
) -> Option<HtmlDocPtr> {
    unsafe {
        xml_init_parser();
        let ctxt: HtmlParserCtxtPtr = html_create_file_parser_ctxt(filename, encoding);
        if ctxt.is_null() {
            return None;
        }
        html_do_read(ctxt, None, None, options, 0)
    }
}

/// Parse an XML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "htmlReadMemory")]
pub unsafe fn html_read_memory(
    buffer: Vec<u8>,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> Option<HtmlDocPtr> {
    unsafe {
        xml_init_parser();
        let ctxt: HtmlParserCtxtPtr = html_create_memory_parser_ctxt(buffer);
        if ctxt.is_null() {
            return None;
        }
        html_do_read(ctxt, url, encoding, options, 0)
    }
}

/// Parse an HTML document from I/O functions and source and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "htmlReadIO")]
pub unsafe fn html_read_io(
    ioctx: impl Read + 'static,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> Option<HtmlDocPtr> {
    unsafe {
        xml_init_parser();

        let input = XmlParserInputBuffer::from_reader(ioctx, XmlCharEncoding::None);
        let ctxt: HtmlParserCtxtPtr = html_new_parser_ctxt();
        if ctxt.is_null() {
            return None;
        }
        let Some(stream) =
            XmlParserInput::from_io(ctxt, Rc::new(RefCell::new(input)), XmlCharEncoding::None)
        else {
            xml_free_parser_ctxt(ctxt);
            return None;
        };
        (*ctxt).input_push(stream);
        html_do_read(ctxt, url, encoding, options, 0)
    }
}

/// Parse an XML in-memory document and build a tree.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "htmlCtxtReadDoc")]
pub unsafe fn html_ctxt_read_doc(
    ctxt: XmlParserCtxtPtr,
    cur: *const XmlChar,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> Option<HtmlDocPtr> {
    unsafe {
        if cur.is_null() {
            return None;
        }
        html_ctxt_read_memory(
            ctxt,
            CStr::from_ptr(cur as *const i8).to_bytes().to_vec(),
            url,
            encoding,
            options,
        )
    }
}

/// Parse an XML file from the filesystem or the network.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "htmlCtxtReadFile")]
pub unsafe fn html_ctxt_read_file(
    ctxt: XmlParserCtxtPtr,
    filename: &str,
    encoding: Option<&str>,
    options: i32,
) -> Option<HtmlDocPtr> {
    unsafe {
        if ctxt.is_null() {
            return None;
        }
        xml_init_parser();

        html_ctxt_reset(ctxt);

        let stream = xml_load_external_entity(Some(filename), None, ctxt)?;
        (*ctxt).input_push(stream);
        html_do_read(ctxt, None, encoding, options, 1)
    }
}

/// Parse an XML in-memory document and build a tree.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "htmlCtxtReadMemory")]
pub unsafe fn html_ctxt_read_memory(
    ctxt: XmlParserCtxtPtr,
    buffer: Vec<u8>,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> Option<HtmlDocPtr> {
    unsafe {
        if ctxt.is_null() {
            return None;
        }
        xml_init_parser();

        html_ctxt_reset(ctxt);

        let input = XmlParserInputBuffer::from_memory(buffer, XmlCharEncoding::None)?;

        let stream =
            XmlParserInput::from_io(ctxt, Rc::new(RefCell::new(input)), XmlCharEncoding::None)?;
        (*ctxt).input_push(stream);
        html_do_read(ctxt, url, encoding, options, 1)
    }
}

/// Parse an HTML document from I/O functions and source and build a tree.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "htmlCtxtReadIO")]
pub unsafe fn html_ctxt_read_io(
    ctxt: XmlParserCtxtPtr,
    ioctx: impl Read + 'static,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> Option<HtmlDocPtr> {
    unsafe {
        if ctxt.is_null() {
            return None;
        }
        xml_init_parser();

        html_ctxt_reset(ctxt);

        let input = XmlParserInputBuffer::from_reader(ioctx, XmlCharEncoding::None);
        let stream =
            XmlParserInput::from_io(ctxt, Rc::new(RefCell::new(input)), XmlCharEncoding::None)?;
        (*ctxt).input_push(stream);
        html_do_read(ctxt, url, encoding, options, 1)
    }
}

// NRK/Jan2003: further knowledge of HTML structure
#[repr(C)]
pub enum HtmlStatus {
    HtmlNa = 0, /* something we don't check at all */
    HtmlInvalid = 0x1,
    HtmlDeprecated = 0x2,
    HtmlValid = 0x4,
    HtmlRequired = 0xc, /* VALID bit set so ( & HTML_VALID ) is TRUE */
}

/// Checks whether an attribute is valid for an element
/// Has full knowledge of Required and Deprecated attributes
///
/// Returns one of HTML_REQUIRED, HTML_VALID, HTML_DEPRECATED, HTML_INVALID
#[doc(alias = "htmlAttrAllowed")]
pub fn html_attr_allowed(elt: &HtmlElemDesc, attr: &str, legacy: bool) -> HtmlStatus {
    if elt.attrs_req.iter().any(|&p| p == attr) {
        return HtmlStatus::HtmlRequired;
    }
    if elt.attrs_opt.iter().any(|&p| p == attr) {
        return HtmlStatus::HtmlValid;
    }
    if legacy && elt.attrs_depr.iter().any(|&p| p == attr) {
        return HtmlStatus::HtmlDeprecated;
    }

    HtmlStatus::HtmlInvalid
}

/// Checks whether an HTML element may be a direct child of a parent element.
///
/// # Note
/// - doesn't check for deprecated elements
///
/// Returns 1 if allowed; 0 otherwise.
#[doc(alias = "htmlElementAllowedHere")]
pub fn html_element_allowed_here(parent: &HtmlElemDesc, elt: &str) -> bool {
    parent.subelts.iter().any(|&sub| sub == elt)
}

/// Checks whether an HTML element may be a direct child of a parent element.
/// and if so whether it is valid or deprecated.
///
/// Returns one of htmlStatus::HTML_VALID, htmlStatus::HTML_DEPRECATED, htmlStatus::HTML_INVALID
#[doc(alias = "htmlElementStatusHere")]
pub fn html_element_status_here(parent: &HtmlElemDesc, elt: &HtmlElemDesc) -> HtmlStatus {
    if !html_element_allowed_here(parent, elt.name) {
        return HtmlStatus::HtmlInvalid;
    }

    if elt.dtd == 0 {
        HtmlStatus::HtmlValid
    } else {
        HtmlStatus::HtmlDeprecated
    }
}

/// Checks whether the tree node is valid.  
/// Experimental (the author only uses the HTML enhancements in a SAX parser)
///
/// Return: for Element nodes, a return from htmlElementAllowedHere (if legacy allowed)
/// or htmlElementStatusHere (otherwise).  
/// - For Attribute nodes, a return from htmlAttrAllowed
/// - For other nodes, htmlStatus::HTML_NA (no checks performed)
#[doc(alias = "htmlNodeStatus")]
pub fn html_node_status(node: HtmlNodePtr, legacy: bool) -> HtmlStatus {
    match node.element_type() {
        XmlElementType::XmlElementNode => {
            if legacy {
                if html_tag_lookup(&node.parent().unwrap().name().unwrap())
                    .is_some_and(|desc| html_element_allowed_here(desc, &node.name().unwrap()))
                {
                    HtmlStatus::HtmlValid
                } else {
                    HtmlStatus::HtmlInvalid
                }
            } else {
                html_tag_lookup(&node.parent().unwrap().name().unwrap())
                    .zip(html_tag_lookup(&node.name().unwrap()))
                    .map(|(par, chi)| html_element_status_here(par, chi))
                    .unwrap_or(HtmlStatus::HtmlInvalid)
            }
        }
        XmlElementType::XmlAttributeNode => {
            html_tag_lookup(&node.parent().unwrap().name().unwrap())
                .map(|desc| html_attr_allowed(desc, node.name().as_deref().unwrap(), legacy))
                .unwrap_or(HtmlStatus::HtmlInvalid)
        }
        _ => HtmlStatus::HtmlNa,
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_html_ctxt_reset() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_HTML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_html_parser_ctxt_ptr(n_ctxt, 0);

                html_ctxt_reset(ctxt);
                des_html_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlCtxtReset",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlCtxtReset()");
        }
    }

    #[test]
    fn test_html_ctxt_use_options() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_HTML_PARSER_CTXT_PTR {
                for n_options in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_html_parser_ctxt_ptr(n_ctxt, 0);
                    let options = gen_int(n_options, 1);

                    let ret_val = html_ctxt_use_options(ctxt, options);
                    desret_int(ret_val);
                    des_html_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_int(n_options, options, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in htmlCtxtUseOptions",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_options);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlCtxtUseOptions()"
            );
        }
    }

    #[test]
    fn test_html_encode_entities() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_UNSIGNED_CHAR_PTR {
                for n_outlen in 0..GEN_NB_INT_PTR {
                    for n_in in 0..GEN_NB_CONST_UNSIGNED_CHAR_PTR {
                        for n_inlen in 0..GEN_NB_INT_PTR {
                            for n_quote_char in 0..GEN_NB_INT {
                                let mem_base = xml_mem_blocks();
                                let out = gen_unsigned_char_ptr(n_out, 0);
                                let outlen = gen_int_ptr(n_outlen, 1);
                                let input = gen_const_unsigned_char_ptr(n_in, 2);
                                let inlen = gen_int_ptr(n_inlen, 3);
                                let quote_char = gen_int(n_quote_char, 4);

                                let ret_val = html_encode_entities(
                                    out,
                                    outlen,
                                    input as *const u8,
                                    inlen,
                                    quote_char,
                                );
                                desret_int(ret_val);
                                des_unsigned_char_ptr(n_out, out, 0);
                                des_int_ptr(n_outlen, outlen, 1);
                                des_const_unsigned_char_ptr(n_in, input as *const u8, 2);
                                des_int_ptr(n_inlen, inlen, 3);
                                des_int(n_quote_char, quote_char, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in htmlEncodeEntities",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_out);
                                    eprint!(" {}", n_outlen);
                                    eprint!(" {}", n_in);
                                    eprint!(" {}", n_inlen);
                                    eprintln!(" {}", n_quote_char);
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlEncodeEntities()"
            );
        }
    }

    #[test]
    fn test_html_handle_omitted_elem() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let val = gen_int(n_val, 0);

                let ret_val = html_handle_omitted_elem(val);
                desret_int(ret_val);
                des_int(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlHandleOmittedElem",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_val);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlHandleOmittedElem()"
            );
        }
    }

    #[test]
    fn test_html_init_auto_close() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            html_init_auto_close();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in htmlInitAutoClose",
                    xml_mem_blocks() - mem_base
                );
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlInitAutoClose()");
        }
    }

    #[test]
    fn test_html_new_parser_ctxt() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            let ret_val = html_new_parser_ctxt();
            desret_html_parser_ctxt_ptr(ret_val);
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in htmlNewParserCtxt",
                    xml_mem_blocks() - mem_base
                );
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlNewParserCtxt()");
        }
    }

    #[test]
    fn test_html_parse_char_ref() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_HTML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_html_parser_ctxt_ptr(n_ctxt, 0);

                let ret_val = html_parse_char_ref(ctxt);
                desret_int(ret_val);
                des_html_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlParseCharRef",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlParseCharRef()");
        }
    }
}

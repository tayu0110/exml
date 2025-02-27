//! Provide internal methods and data structures for parsing XML documents.  
//! This module is based on `libxml/parserInternals.h`, `parserInternals.c`, and so on in `libxml2-v2.11.8`.
//!
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

use std::{
    borrow::Cow,
    cell::RefCell,
    ffi::{CStr, CString},
    io::Write,
    mem::size_of,
    ptr::{addr_of_mut, null, null_mut},
    rc::Rc,
    str::from_utf8_unchecked,
    sync::atomic::Ordering,
};

use libc::{memcpy, INT_MAX};

use crate::{
    encoding::{detect_encoding, XmlCharEncoding},
    error::XmlParserErrors,
    generic_error,
    globals::{get_parser_debug_entities, GenericErrorContext},
    hash::XmlHashTableRef,
    libxml::{
        chvalid::{
            xml_is_base_char, xml_is_blank_char, xml_is_char, xml_is_combining, xml_is_digit,
            xml_is_extender, xml_is_ideographic,
        },
        dict::{xml_dict_free, xml_dict_lookup},
        globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
        parser::{
            xml_parse_att_value_internal, xml_parse_cdsect, xml_parse_char_data_internal,
            xml_parse_conditional_sections, xml_parse_element_children_content_decl_priv,
            xml_parse_end_tag1, xml_parse_end_tag2, xml_parse_external_entity_private,
            xml_parse_external_id, xml_parse_markup_decl, xml_parse_start_tag2,
            xml_parse_string_name, xml_parser_add_node_info, xml_parser_entity_check,
            xml_parser_find_node_info, xml_string_decode_entities_int, XmlDefAttrs, XmlDefAttrsPtr,
            XmlParserInputState, XmlParserMode, XmlParserOption, XML_SKIP_IDS,
        },
        sax2::xml_sax2_get_entity,
        valid::{
            xml_free_doc_element_content, xml_new_doc_element_content, xml_validate_element,
            xml_validate_root,
        },
        xmlstring::{xml_str_equal, xml_strchr, xml_strdup, xml_strlen, xml_strndup, XmlChar},
    },
    parser::{
        xml_err_memory, XmlParserInputPtr, XmlParserNodeInfo, __xml_err_encoding,
        xml_err_encoding_int, xml_err_msg_str, xml_fatal_err, xml_fatal_err_msg,
        xml_fatal_err_msg_int, xml_fatal_err_msg_str, xml_fatal_err_msg_str_int_str,
        xml_validity_error, xml_warning_msg, XmlParserCharValid, XmlParserCtxtPtr,
    },
    tree::{
        xml_create_int_subset, xml_doc_copy_node, xml_free_doc, xml_free_node, xml_free_node_list,
        xml_get_predefined_entity, xml_new_doc, xml_new_doc_node, xml_split_qname3, NodeCommon,
        NodePtr, XmlAttributeDefault, XmlAttributeType, XmlDocProperties, XmlDocPtr,
        XmlElementContentOccur, XmlElementContentPtr, XmlElementContentType, XmlElementType,
        XmlElementTypeVal, XmlEntityPtr, XmlEntityType, XmlEnumeration, XmlGenericNodePtr, XmlNode,
        XmlNodePtr, XML_ENT_CHECKED, XML_ENT_CHECKED_LT, XML_ENT_CONTAINS_LT, XML_ENT_EXPANDING,
        XML_ENT_PARSED, XML_XML_NAMESPACE,
    },
};
use crate::{
    parser::{
        parse_char_ref, parse_comment, parse_name, parse_nmtoken, parse_pi, parse_text_decl,
        xml_create_memory_parser_ctxt, xml_free_input_stream, xml_free_parser_ctxt,
        xml_new_entity_input_stream,
    },
    tree::xml_create_enumeration,
};

use super::hash::XmlHashTable;

macro_rules! VALID_CTXT {
    ($ctxt:expr) => {
        (*(*$ctxt).input).cur <= (*(*$ctxt).input).end
    };
}

macro_rules! NXT {
    ($ctxt:expr, $val:expr) => {
        *(*(*$ctxt).input).cur.add($val as usize)
    };
}

/// Arbitrary depth limit for the XML documents that we allow to
/// process. This is not a limitation of the parser but a safety
/// boundary feature, use XML_PARSE_HUGE option to override it.
#[doc(alias = "xmlParserMaxDepth")]
pub static mut XML_PARSER_MAX_DEPTH: u32 = 256;

/// Maximum size allowed for a single text node when building a tree.
/// This is not a limitation of the parser but a safety boundary feature,
/// use XML_PARSE_HUGE option to override it.
/// Introduced in 2.9.0
pub const XML_MAX_TEXT_LENGTH: usize = 10000000;

/// Maximum size allowed when XML_PARSE_HUGE is set.
pub const XML_MAX_HUGE_LENGTH: usize = 1000000000;

/// Maximum size allowed for a markup identifier.
/// This is not a limitation of the parser but a safety boundary feature,
/// use XML_PARSE_HUGE option to override it.
/// Note that with the use of parsing dictionaries overriding the limit
/// may result in more runtime memory usage in face of "unfriendly' content
/// Introduced in 2.9.0
pub const XML_MAX_NAME_LENGTH: usize = 50000;

/// Maximum size allowed by the parser for a dictionary by default
/// This is not a limitation of the parser but a safety boundary feature,
/// use XML_PARSE_HUGE option to override it.
/// Introduced in 2.9.0
pub const XML_MAX_DICTIONARY_LIMIT: usize = 10000000;

/// Maximum size allowed by the parser for ahead lookup
/// This is an upper boundary enforced by the parser to avoid bad
/// behaviour on "unfriendly' content
/// Introduced in 2.9.0
pub const XML_MAX_LOOKUP_LIMIT: usize = 10000000;

/// Identifiers can be longer, but this will be more costly at runtime.
pub const XML_MAX_NAMELEN: usize = 100;

/// The parser tries to always have that amount of input ready.
/// One of the point is providing context when reporting errors.
pub const INPUT_CHUNK: usize = 250;

macro_rules! COPY_BUF {
    ($l:expr, $b:expr, $i:expr, $v:expr) => {
        if $l == 1 {
            *$b.add($i as usize) = $v as _;
            $i += 1;
        } else {
            $i =
                ($i as usize + xml_copy_char_multi_byte($b.add($i as usize), $v as _) as usize) as _
        }
    };
}

macro_rules! NEXTL {
    ($ctxt:expr, $l:expr) => {
        if *(*(*$ctxt).input).cur == b'\n' {
            (*(*$ctxt).input).line += 1;
            (*(*$ctxt).input).col = 1;
        } else {
            (*(*$ctxt).input).col += 1;
        }
        (*(*$ctxt).input).cur = (*(*$ctxt).input).cur.add($l as usize);
    };
}

/// Global variables used for predefined strings.
pub static XML_STRING_TEXT: &CStr = c"text";
pub static XML_STRING_TEXT_NOENC: &CStr = c"textnoenc";
pub static XML_STRING_COMMENT: &CStr = c"comment";

/// Check whether the character is allowed by the production
/// `[84] Letter ::= BaseChar | Ideographic`
#[doc(alias = "xmlIsLetter")]
pub fn xml_is_letter(c: u32) -> bool {
    xml_is_base_char(c) || xml_is_ideographic(c)
}

/// Set after xmlValidateDtdFinal was called.
pub(crate) const XML_VCTXT_DTD_VALIDATED: usize = 1usize << 0;
/// Set if the validation context is part of a parser context.
pub(crate) const XML_VCTXT_USE_PCTXT: usize = 1usize << 1;

macro_rules! CUR_SCHAR {
    ($ctxt:expr, $s:expr, $l:expr) => {
        xml_string_current_char($ctxt, $s, addr_of_mut!($l))
    };
}

/// parse an UTF8 encoded XML qualified name string
///
/// `[NS 5] QName ::= (Prefix ':')? LocalPart`
///
/// `[NS 6] Prefix ::= NCName`
///
/// `[NS 7] LocalPart ::= NCName`
///
/// Returns the local part, and prefix is updated to get the Prefix if any.
#[doc(alias = "xmlSplitQName")]
pub unsafe fn xml_split_qname(
    ctxt: XmlParserCtxtPtr,
    name: *const XmlChar,
    prefix: *mut *mut XmlChar,
) -> *mut XmlChar {
    let mut buf: [XmlChar; XML_MAX_NAMELEN + 5] = [0; XML_MAX_NAMELEN + 5];
    let mut buffer: *mut XmlChar = null_mut();
    let mut len: i32 = 0;
    let mut max: i32 = XML_MAX_NAMELEN as i32;
    let mut ret: *mut XmlChar;
    let mut cur: *const XmlChar = name;
    let mut c: i32;

    if prefix.is_null() {
        return null_mut();
    }
    *prefix = null_mut();

    if cur.is_null() {
        return null_mut();
    }

    // nasty but well=formed
    if *cur.add(0) == b':' {
        return xml_strdup(name);
    }

    c = *cur as _;
    cur = cur.add(1);
    while c != 0 && c != b':' as i32 && len < max {
        // tested bigname.xml
        buf[len as usize] = c as _;
        len += 1;
        c = *cur as _;
        cur = cur.add(1);
    }
    if len >= max {
        // Okay someone managed to make a huge name, so he's ready to pay
        // for the processing speed.
        max = len * 2;

        buffer = xml_malloc_atomic(max as usize) as *mut XmlChar;
        if buffer.is_null() {
            xml_err_memory(ctxt, None);
            return null_mut();
        }
        memcpy(buffer as _, buf.as_ptr() as _, len as usize);
        while c != 0 && c != b':' as i32 {
            // tested bigname.xml
            if len + 10 > max {
                max *= 2;
                let tmp: *mut XmlChar = xml_realloc(buffer as _, max as usize) as *mut XmlChar;
                if tmp.is_null() {
                    xml_free(buffer as _);
                    xml_err_memory(ctxt, None);
                    return null_mut();
                }
                buffer = tmp;
            }
            *buffer.add(len as usize) = c as _;
            len += 1;
            c = *cur as _;
            cur = cur.add(1);
        }
        *buffer.add(len as usize) = 0;
    }

    if c == b':' as i32 && *cur == 0 {
        if !buffer.is_null() {
            xml_free(buffer as _);
        }
        *prefix = null_mut();
        return xml_strdup(name);
    }

    if buffer.is_null() {
        ret = xml_strndup(buf.as_mut_ptr() as _, len);
    } else {
        ret = buffer;
        buffer = null_mut();
        max = XML_MAX_NAMELEN as _;
    }

    if c == b':' as i32 {
        c = *cur as _;
        *prefix = ret;
        if c == 0 {
            return xml_strndup(c"".as_ptr() as _, 0);
        }
        len = 0;

        // Check that the first character is proper to start a new name
        if !((0x61..=0x7A).contains(&c)
            || (0x41..=0x5A).contains(&c)
            || c == b'_' as i32
            || c == b':' as i32)
        {
            let mut l: i32 = 0;
            let first: i32 = CUR_SCHAR!(ctxt, cur, l);

            if !xml_is_letter(first as u32) && first != b'_' as i32 {
                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlNsErrQname,
                    "Name {} is not XML Namespace compliant\n",
                    name
                );
            }
        }
        cur = cur.add(1);

        while c != 0 && len < max {
            // tested bigname2.xml
            buf[len as usize] = c as _;
            len += 1;
            c = *cur as _;
            cur = cur.add(1);
        }
        if len >= max {
            // Okay someone managed to make a huge name, so he's ready to pay
            // for the processing speed.
            max = len * 2;

            buffer = xml_malloc_atomic(max as usize) as *mut XmlChar;
            if buffer.is_null() {
                xml_err_memory(ctxt, None);
                return null_mut();
            }
            memcpy(buffer as _, buf.as_ptr() as _, len as usize);
            while c != 0 {
                // tested bigname2.xml
                if len + 10 > max {
                    max *= 2;
                    let tmp: *mut XmlChar = xml_realloc(buffer as _, max as usize) as *mut XmlChar;
                    if tmp.is_null() {
                        xml_err_memory(ctxt, None);
                        xml_free(buffer as _);
                        return null_mut();
                    }
                    buffer = tmp;
                }
                *buffer.add(len as usize) = c as _;
                len += 1;
                c = *cur as _;
                cur = cur.add(1);
            }
            *buffer.add(len as usize) = 0;
        }

        if buffer.is_null() {
            ret = xml_strndup(buf.as_mut_ptr() as _, len);
        } else {
            ret = buffer;
        }
    }

    ret
}

unsafe fn xml_parse_name_complex(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    let mut len: i32 = 0;
    let mut l: i32 = 0;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };

    // Handler for more complex cases
    let c = (*ctxt).current_char(&mut l).unwrap_or('\0');
    if (*ctxt).options & XmlParserOption::XmlParseOld10 as i32 == 0 {
        // Use the new checks of production [4] [4a] amd [5] of the
        // Update 5 of XML-1.0
        if c == ' '
            || c == '>'
            || c == '/' /* accelerators */
            || (!(c.is_ascii_lowercase()
                || c.is_ascii_uppercase()
                || c == '_'
                || c == ':'
                || ('\u{C0}'..='\u{D6}').contains(&c)
                || ('\u{D8}'..='\u{F6}').contains(&c)
                || ('\u{F8}'..='\u{2FF}').contains(&c)
                || ('\u{370}'..='\u{37D}').contains(&c)
                || ('\u{37F}'..='\u{1FFF}').contains(&c)
                || ('\u{200C}'..='\u{200D}').contains(&c)
                || ('\u{2070}'..='\u{218F}').contains(&c)
                || ('\u{2C00}'..='\u{2FEF}').contains(&c)
                || ('\u{3001}'..='\u{D7FF}').contains(&c)
                || ('\u{F900}'..='\u{FDCF}').contains(&c)
                || ('\u{FDF0}'..='\u{FFFD}').contains(&c)
                || ('\u{10000}'..='\u{EFFFF}').contains(&c)))
        {
            return null_mut();
        }
        len += l;
        NEXTL!(ctxt, l);
        let mut c = (*ctxt).current_char(&mut l).unwrap_or('\0');
        while c != ' '
            && c != '>'
            && c != '/'
            && (c.is_ascii_lowercase()
                || c.is_ascii_uppercase()
                || c.is_ascii_digit()
                || c == '_'
                || c == ':'
                || c == '-'
                || c == '.'
                || c == '\u{B7}'
                || ('\u{C0}'..='\u{D6}').contains(&c)
                || ('\u{D8}'..='\u{F6}').contains(&c)
                || ('\u{F8}'..='\u{2FF}').contains(&c)
                || ('\u{300}'..='\u{36F}').contains(&c)
                || ('\u{370}'..='\u{37D}').contains(&c)
                || ('\u{37F}'..='\u{1FFF}').contains(&c)
                || ('\u{200C}'..='\u{200D}').contains(&c)
                || ('\u{203F}'..='\u{2040}').contains(&c)
                || ('\u{2070}'..='\u{218F}').contains(&c)
                || ('\u{2C00}'..='\u{2FEF}').contains(&c)
                || ('\u{3001}'..='\u{D7FF}').contains(&c)
                || ('\u{F900}'..='\u{FDCF}').contains(&c)
                || ('\u{FDF0}'..='\u{FFFD}').contains(&c)
                || ('\u{10000}'..='\u{EFFFF}').contains(&c))
        {
            if len <= INT_MAX - l {
                len += l;
            }
            NEXTL!(ctxt, l);
            c = (*ctxt).current_char(&mut l).unwrap_or('\0');
        }
    } else {
        if c == ' '
            || c == '>'
            || c == '/' /* accelerators */
            || (!xml_is_letter(c as u32) && c != '_' && c != ':')
        {
            return null_mut();
        }
        len += l;
        NEXTL!(ctxt, l);

        let mut c = (*ctxt).current_char(&mut l).unwrap_or('\0');
        while c != ' '
            && c != '>'
            && c != '/' /* test bigname.xml */
            && (xml_is_letter(c as u32)
                || xml_is_digit(c as u32)
                || c == '.'
                || c == '-'
                || c == '_'
                || c == ':'
                || xml_is_combining(c as u32)
                || xml_is_extender(c as u32))
        {
            if len <= INT_MAX - l {
                len += l;
            }
            NEXTL!(ctxt, l);
            c = (*ctxt).current_char(&mut l).unwrap_or('\0');
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }
    if len > max_length {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("Name"));
        return null_mut();
    }
    if (*(*ctxt).input).offset_from_base() < len as usize {
        // There were a couple of bugs where PERefs lead to to a change
        // of the buffer. Check the buffer size to avoid passing an invalid
        // pointer to xmlDictLookup.
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            Some("unexpected change of input buffer"),
        );
        return null_mut();
    }
    if *(*(*ctxt).input).cur == b'\n' && *(*(*ctxt).input).cur.sub(1) == b'\r' {
        return xml_dict_lookup(
            (*ctxt).dict,
            (*(*ctxt).input).cur.sub(len as usize + 1),
            len,
        );
    }
    xml_dict_lookup((*ctxt).dict, (*(*ctxt).input).cur.sub(len as usize), len)
}

/// Parse an XML name.
///
/// `[4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender`
///
/// `[5] Name ::= (Letter | '_' | ':') (NameChar)*`
///
/// `[6] Names ::= Name (#x20 Name)*`
///
/// Returns the Name parsed or NULL
#[doc(alias = "xmlParseName")]
pub(crate) unsafe fn xml_parse_name(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    let mut input: *const XmlChar;
    let ret: *const XmlChar;
    let count: usize;
    let max_length: usize = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH
    } else {
        XML_MAX_NAME_LENGTH
    };

    (*ctxt).grow();
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }

    // Accelerator for simple ASCII names
    input = (*(*ctxt).input).cur;
    if (*input >= 0x61 && *input <= 0x7A)
        || (*input >= 0x41 && *input <= 0x5A)
        || *input == b'_'
        || *input == b':'
    {
        input = input.add(1);
        while (*input >= 0x61 && *input <= 0x7A)
            || (*input >= 0x41 && *input <= 0x5A)
            || (*input >= 0x30 && *input <= 0x39)
            || *input == b'_'
            || *input == b'-'
            || *input == b':'
            || *input == b'.'
        {
            input = input.add(1);
        }
        if *input > 0 && *input < 0x80 {
            count = input.offset_from((*(*ctxt).input).cur) as _;
            if count > max_length {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("Name"));
                return null_mut();
            }
            ret = xml_dict_lookup((*ctxt).dict, (*(*ctxt).input).cur, count as i32);
            (*(*ctxt).input).cur = input;
            (*(*ctxt).input).col += count as i32;
            if ret.is_null() {
                xml_err_memory(ctxt, None);
            }
            return ret;
        }
    }
    /* accelerator for special cases */
    xml_parse_name_complex(ctxt)
}

const XML_PARSER_BUFFER_SIZE: usize = 100;

/// Parse an XML Nmtoken.
///
/// `[7] Nmtoken ::= (NameChar)+`
///
/// `[8] Nmtokens ::= Nmtoken (#x20 Nmtoken)*`
///
/// Returns the Nmtoken parsed or NULL
#[doc(alias = "xmlParseNmtoken")]
pub(crate) unsafe fn xml_parse_nmtoken(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    let mut buf: [XmlChar; XML_MAX_NAMELEN + 5] = [0; XML_MAX_NAMELEN + 5];
    let mut len: i32 = 0;
    let mut l: i32 = 0;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };

    let mut c = (*ctxt).current_char(&mut l).unwrap_or('\0');
    while c.is_name_char(&*ctxt) {
        COPY_BUF!(l, buf.as_mut_ptr(), len, c);
        NEXTL!(ctxt, l);
        c = (*ctxt).current_char(&mut l).unwrap_or('\0');
        if len >= XML_MAX_NAMELEN as i32 {
            // Okay someone managed to make a huge token, so he's ready to pay
            // for the processing speed.
            let mut buffer: *mut XmlChar;
            let mut max: i32 = len * 2;

            buffer = xml_malloc_atomic(max as usize) as *mut XmlChar;
            if buffer.is_null() {
                xml_err_memory(ctxt, None);
                return null_mut();
            }
            memcpy(buffer as _, buf.as_ptr() as _, len as usize);
            while c.is_name_char(&*ctxt) {
                if len + 10 > max {
                    max *= 2;
                    let tmp: *mut XmlChar = xml_realloc(buffer as _, max as usize) as *mut XmlChar;
                    if tmp.is_null() {
                        xml_err_memory(ctxt, None);
                        xml_free(buffer as _);
                        return null_mut();
                    }
                    buffer = tmp;
                }
                COPY_BUF!(l, buffer, len, c);
                if len > max_length {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("NmToken"));
                    xml_free(buffer as _);
                    return null_mut();
                }
                NEXTL!(ctxt, l);
                c = (*ctxt).current_char(&mut l).unwrap_or('\0');
            }
            *buffer.add(len as usize) = 0;
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                xml_free(buffer as _);
                return null_mut();
            }
            return buffer;
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }
    if len == 0 {
        return null_mut();
    }
    if len > max_length {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("NmToken"));
        return null_mut();
    }
    xml_strndup(buf.as_ptr() as _, len)
}

/// Parse a value for ENTITY declarations
///
/// `[9] EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"' | "'" ([^%&'] | PEReference | Reference)* "'"`
///
/// Returns the EntityValue parsed with reference substituted or NULL
#[doc(alias = "xmlParseEntityValue")]
pub(crate) unsafe fn xml_parse_entity_value(
    ctxt: XmlParserCtxtPtr,
    orig: *mut *mut XmlChar,
) -> *mut XmlChar {
    let mut buf: *mut XmlChar;
    let mut len: i32 = 0;
    let mut size: i32 = XML_PARSER_BUFFER_SIZE as _;
    let mut l: i32 = 0;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH as i32
    } else {
        XML_MAX_TEXT_LENGTH as i32
    };
    let stop: XmlChar;
    let mut ret: *mut XmlChar = null_mut();
    let mut cur: *const XmlChar;

    if (*ctxt).current_byte() == b'"' {
        stop = b'"';
    } else if (*ctxt).current_byte() == b'\'' {
        stop = b'\'';
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityNotStarted, None);
        return null_mut();
    }
    buf = xml_malloc_atomic(size as usize) as *mut XmlChar;
    if buf.is_null() {
        xml_err_memory(ctxt, None);
        return null_mut();
    }

    // The content of the entity definition is copied in a buffer.

    (*ctxt).instate = XmlParserInputState::XmlParserEntityValue;
    let input: XmlParserInputPtr = (*ctxt).input;
    (*ctxt).grow();
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        //  goto error;
        if !buf.is_null() {
            xml_free(buf as _);
        }
        return ret;
    }
    (*ctxt).skip_char();
    let mut c = (*ctxt).current_char(&mut l).unwrap_or('\0');
    // NOTE: 4.4.5 Included in Literal
    // When a parameter entity reference appears in a literal entity
    // value, ... a single or double quote character in the replacement
    // text is always treated as a normal data character and will not
    // terminate the literal.
    // In practice it means we stop the loop only when back at parsing
    // the initial entity and the quote is found
    while (xml_is_char(c as u32) && (c as i32 != stop as i32 || (*ctxt).input != input))
        && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        if len + 5 >= size {
            size *= 2;
            let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize) as *mut XmlChar;
            if tmp.is_null() {
                xml_err_memory(ctxt, None);
                //  goto error;
                if !buf.is_null() {
                    xml_free(buf as _);
                }
                return ret;
            }
            buf = tmp;
        }
        COPY_BUF!(l, buf, len, c);
        NEXTL!(ctxt, l);

        (*ctxt).grow();
        c = (*ctxt).current_char(&mut l).unwrap_or('\0');
        if c == '\0' {
            (*ctxt).grow();
            c = (*ctxt).current_char(&mut l).unwrap_or('\0');
        }

        if len > max_length {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrEntityNotFinished,
                "entity value too long\n",
            );
            //  goto error;
            if !buf.is_null() {
                xml_free(buf as _);
            }
            return ret;
        }
    }
    *buf.add(len as usize) = 0;
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        //  goto error;
        if !buf.is_null() {
            xml_free(buf as _);
        }
        return ret;
    }
    if c as i32 != stop as i32 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityNotFinished, None);
        //  goto error;
        if !buf.is_null() {
            xml_free(buf as _);
        }
        return ret;
    }
    (*ctxt).skip_char();

    // Raise problem w.r.t. '&' and '%' being used in non-entities
    // reference constructs. Note Charref will be handled in
    // xmlStringDecodeEntities()
    cur = buf;
    while *cur != 0 {
        // non input consuming
        if *cur == b'%' || (*cur == b'&' && *cur.add(1) != b'#') {
            let tmp: XmlChar = *cur;
            let mut name_ok: i32 = 0;

            cur = cur.add(1);
            let name: *mut XmlChar = xml_parse_string_name(ctxt, addr_of_mut!(cur));
            if !name.is_null() {
                name_ok = 1;
                xml_free(name as _);
            }
            if name_ok == 0 || *cur != b';' {
                xml_fatal_err_msg_int!(
                    ctxt,
                    XmlParserErrors::XmlErrEntityCharError,
                    format!(
                        "EntityValue: '{}' forbidden except for entities references\n",
                        tmp as char
                    )
                    .as_str(),
                    tmp as i32
                );
                //  goto error;
                if !buf.is_null() {
                    xml_free(buf as _);
                }
                return ret;
            }
            if tmp == b'%' && (*ctxt).in_subset == 1 && (*ctxt).input_tab.len() == 1 {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityPEInternal, None);
                //  goto error;
                if !buf.is_null() {
                    xml_free(buf as _);
                }
                return ret;
            }
            if *cur == 0 {
                break;
            }
        }
        cur = cur.add(1);
    }

    // Then PEReference entities are substituted.
    //
    // NOTE: 4.4.7 Bypassed
    // When a general entity reference appears in the EntityValue in
    // an entity declaration, it is bypassed and left as is.
    // so XML_SUBSTITUTE_REF is not set here.
    (*ctxt).depth += 1;
    ret = xml_string_decode_entities_int(
        ctxt,
        buf,
        len,
        XML_SUBSTITUTE_PEREF as _,
        0,
        0,
        0,
        /* check */ 1,
    );
    (*ctxt).depth -= 1;

    if !orig.is_null() {
        *orig = buf;
        buf = null_mut();
    }

    //  error:
    if !buf.is_null() {
        xml_free(buf as _);
    }
    ret
}

/// parse a value for an attribute
/// Note: the parser won't do substitution of entities here, this
/// will be handled later in xmlStringGetNodeList
///
/// `[10] AttValue ::= '"' ([^<&"] | Reference)* '"' | "'" ([^<&'] | Reference)* "'"`
///
/// # 3.3.3 Attribute-Value Normalization:
/// Before the value of an attribute is passed to the application or
/// checked for validity, the XML processor must normalize it as follows:
/// - a character reference is processed by appending the referenced
///   character to the attribute value
/// - an entity reference is processed by recursively processing the
///   replacement text of the entity
/// - a whitespace character (#x20, #xD, #xA, #x9) is processed by
///   appending #x20 to the normalized value, except that only a single
///   #x20 is appended for a "#xD#xA" sequence that is part of an external
///   parsed entity or the literal entity value of an internal parsed entity
/// - other characters are processed by appending them to the normalized value
///   If the declared value is not CDATA, then the XML processor must further
///   process the normalized attribute value by discarding any leading and
///   trailing space (#x20) characters, and by replacing sequences of space
///   (#x20) characters by a single space (#x20) character.
///   All attributes for which no declaration has been read should be treated
///   by a non-validating parser as if declared CDATA.
///
/// Returns the AttValue parsed or NULL. The value has to be freed by the caller.
#[doc(alias = "xmlParseAttValue")]
pub(crate) unsafe fn xml_parse_att_value(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    if ctxt.is_null() || (*ctxt).input.is_null() {
        return null_mut();
    }
    xml_parse_att_value_internal(ctxt, null_mut(), null_mut(), 0)
}

/// Parse an XML Literal
///
/// `[11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")`
///
/// Returns the SystemLiteral parsed or NULL
#[doc(alias = "xmlParseSystemLiteral")]
pub(crate) unsafe fn xml_parse_system_literal(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    let mut buf: *mut XmlChar;
    let mut len: i32 = 0;
    let mut size: i32 = XML_PARSER_BUFFER_SIZE as _;
    let mut l: i32 = 0;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };
    let state: i32 = (*ctxt).instate as i32;

    let stop = if (*ctxt).current_byte() == b'"' {
        (*ctxt).skip_char();
        b'"'
    } else if (*ctxt).current_byte() == b'\'' {
        (*ctxt).skip_char();
        b'\''
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotStarted, None);
        return null_mut();
    };

    buf = xml_malloc_atomic(size as usize) as *mut XmlChar;
    if buf.is_null() {
        xml_err_memory(ctxt, None);
        return null_mut();
    }
    (*ctxt).instate = XmlParserInputState::XmlParserSystemLiteral;
    let mut cur = (*ctxt).current_char(&mut l).unwrap_or('\0');
    while xml_is_char(cur as u32) && cur as i32 != stop as i32 {
        if len + 5 >= size {
            size *= 2;
            let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize) as *mut XmlChar;
            if tmp.is_null() {
                xml_free(buf as _);
                xml_err_memory(ctxt, None);
                (*ctxt).instate = XmlParserInputState::try_from(state).unwrap();
                return null_mut();
            }
            buf = tmp;
        }
        COPY_BUF!(l, buf, len, cur);
        if len > max_length {
            xml_fatal_err(
                ctxt,
                XmlParserErrors::XmlErrNameTooLong,
                Some("SystemLiteral"),
            );
            xml_free(buf as _);
            (*ctxt).instate = XmlParserInputState::try_from(state).unwrap();
            return null_mut();
        }
        NEXTL!(ctxt, l);
        cur = (*ctxt).current_char(&mut l).unwrap_or('\0');
    }
    *buf.add(len as usize) = 0;
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        xml_free(buf as _);
        return null_mut();
    }
    (*ctxt).instate = XmlParserInputState::try_from(state).unwrap();
    if !xml_is_char(cur as u32) {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotFinished, None);
    } else {
        (*ctxt).skip_char();
    }
    buf
}

#[doc(alias = "xmlParseCharData")]
pub(crate) unsafe fn xml_parse_char_data(ctxt: XmlParserCtxtPtr, _cdata: i32) {
    xml_parse_char_data_internal(ctxt, 0);
}

/// Parse an attribute default declaration
///
/// `[60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)`
///
/// `[ VC: Required Attribute ]`  
/// if the default declaration is the keyword #REQUIRED, then the
/// attribute must be specified for all elements of the type in the
/// attribute-list declaration.
///
/// `[ VC: Attribute Default Legal ]`  
/// The declared default value must meet the lexical constraints of
/// the declared attribute type c.f. xmlValidateAttributeDecl()
///
/// `[ VC: Fixed Attribute Default ]`  
/// if an attribute has a default value declared with the #FIXED
/// keyword, instances of that attribute must match the default value.
///
/// `[ WFC: No < in Attribute Values ]`  
/// handled in xmlParseAttValue()
///
/// returns: XML_ATTRIBUTE_NONE, XML_ATTRIBUTE_REQUIRED, XML_ATTRIBUTE_IMPLIED
///  or XML_ATTRIBUTE_FIXED.
#[doc(alias = "xmlParseDefaultDecl")]
pub(crate) unsafe fn xml_parse_default_decl(
    ctxt: XmlParserCtxtPtr,
    value: *mut *mut XmlChar,
) -> XmlAttributeDefault {
    *value = null_mut();
    if (*ctxt).content_bytes().starts_with(b"#REQUIRED") {
        (*ctxt).advance(9);
        return XmlAttributeDefault::XmlAttributeRequired;
    }
    if (*ctxt).content_bytes().starts_with(b"#IMPLIED") {
        (*ctxt).advance(8);
        return XmlAttributeDefault::XmlAttributeImplied;
    }
    let mut val = XmlAttributeDefault::XmlAttributeNone;
    if (*ctxt).content_bytes().starts_with(b"#FIXED") {
        (*ctxt).advance(6);
        val = XmlAttributeDefault::XmlAttributeFixed;
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after '#FIXED'\n",
            );
        }
    }
    let ret: *mut XmlChar = xml_parse_att_value(ctxt);
    (*ctxt).instate = XmlParserInputState::XmlParserDTD;
    if ret.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::try_from((*ctxt).err_no).unwrap(),
            "Attribute default value declaration error\n",
        );
    } else {
        *value = ret;
    }
    val
}

/// Parse an Notation attribute type.
///
/// # Note
/// The leading 'NOTATION' S part has already being parsed...
///
/// `[58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'`
///
/// `[ VC: Notation Attributes ]`
/// Values of this type must match one of the notation names included
/// in the declaration; all notation names in the declaration must be declared.
///
/// Returns: the notation attribute tree built while parsing
#[doc(alias = "xmlParseNotationType")]
pub(crate) unsafe fn xml_parse_notation_type(
    ctxt: XmlParserCtxtPtr,
) -> Option<Box<XmlEnumeration>> {
    let mut ret = None::<Box<XmlEnumeration>>;

    if (*ctxt).current_byte() != b'(' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotStarted, None);
        return None;
    }
    while {
        (*ctxt).skip_char();
        (*ctxt).skip_blanks();
        let Some(name) = parse_name(&mut *ctxt) else {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "Name expected in NOTATION declaration\n",
            );
            return None;
        };
        let mut tmp = ret.as_deref();
        while let Some(now) = tmp {
            if name == now.name {
                xml_validity_error!(
                    ctxt,
                    XmlParserErrors::XmlDTDDupToken,
                    "standalone: attribute notation value token {} duplicated\n",
                    name
                );
                break;
            }
            tmp = now.next.as_deref();
        }
        if tmp.is_none() {
            let cur = xml_create_enumeration(&name);
            if let Some(mut ret) = ret.as_deref_mut() {
                while ret.next.is_some() {
                    ret = ret.next.as_deref_mut().unwrap();
                }
                ret.next = Some(cur);
            } else {
                ret = Some(cur);
            }
        }
        (*ctxt).skip_blanks();
        (*ctxt).current_byte() == b'|'
    } {}
    if (*ctxt).current_byte() != b')' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotFinished, None);
        return None;
    }
    (*ctxt).skip_char();
    ret
}

/// Parse an Enumeration attribute type.
///
/// `[59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'`
///
/// `[ VC: Enumeration ]`  
/// Values of this type must match one of the Nmtoken tokens in the declaration
///
/// Returns: the enumeration attribute tree built while parsing
#[doc(alias = "xmlParseEnumerationType")]
pub(crate) unsafe fn xml_parse_enumeration_type(
    ctxt: XmlParserCtxtPtr,
) -> Option<Box<XmlEnumeration>> {
    let mut ret = None::<Box<XmlEnumeration>>;

    if (*ctxt).current_byte() != b'(' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttlistNotStarted, None);
        return None;
    }
    while {
        (*ctxt).skip_char();
        (*ctxt).skip_blanks();
        let Some(name) = parse_nmtoken(&mut *ctxt) else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNmtokenRequired, None);
            return ret;
        };
        let mut tmp = ret.as_deref();
        while let Some(now) = tmp {
            if name.as_str() == now.name {
                xml_validity_error!(
                    ctxt,
                    XmlParserErrors::XmlDTDDupToken,
                    "standalone: attribute enumeration value token {} duplicated\n",
                    name
                );
                break;
            }
            tmp = now.next.as_deref();
        }
        if tmp.is_none() {
            let cur = xml_create_enumeration(&name);
            if let Some(mut ret) = ret.as_deref_mut() {
                while ret.next.is_some() {
                    ret = ret.next.as_deref_mut().unwrap();
                }
                ret.next = Some(cur);
            } else {
                ret = Some(cur);
            }
        }
        (*ctxt).skip_blanks();
        (*ctxt).current_byte() == b'|'
    } {}
    if (*ctxt).current_byte() != b')' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttlistNotFinished, None);
        return ret;
    }
    (*ctxt).skip_char();
    ret
}

/// Parse an Enumerated attribute type.
///
/// `[57] EnumeratedType ::= NotationType | Enumeration`
///
/// `[58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'`
///
/// Returns: XML_ATTRIBUTE_ENUMERATION or XML_ATTRIBUTE_NOTATION
#[doc(alias = "xmlParseEnumeratedType")]
pub(crate) unsafe fn xml_parse_enumerated_type(
    ctxt: XmlParserCtxtPtr,
    tree: &mut Option<Box<XmlEnumeration>>,
) -> Option<XmlAttributeType> {
    if (*ctxt).content_bytes().starts_with(b"NOTATION") {
        (*ctxt).advance(8);
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after 'NOTATION'\n",
            );
            return None;
        }
        *tree = xml_parse_notation_type(ctxt);
        if tree.is_none() {
            return None;
        }
        return Some(XmlAttributeType::XmlAttributeNotation);
    }
    *tree = xml_parse_enumeration_type(ctxt);
    if tree.is_none() {
        return None;
    }
    Some(XmlAttributeType::XmlAttributeEnumeration)
}

/// Parse the Attribute list def for an element
///
/// `[54] AttType ::= StringType | TokenizedType | EnumeratedType`
///
/// `[55] StringType ::= 'CDATA'`
///
/// `[56] TokenizedType ::= 'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'`
///
/// Validity constraints for attribute values syntax are checked in xmlValidateAttributeValue()
///
/// `[ VC: ID ]`  
/// Values of type ID must match the Name production. A name must not
/// appear more than once in an XML document as a value of this type;
/// i.e., ID values must uniquely identify the elements which bear them.
///
/// `[ VC: One ID per Element Type ]`  
/// No element type may have more than one ID attribute specified.
///
/// `[ VC: ID Attribute Default ]`  
/// An ID attribute must have a declared default of #IMPLIED or #REQUIRED.
///
/// `[ VC: IDREF ]`  
/// Values of type IDREF must match the Name production, and values
/// of type IDREFS must match Names; each IDREF Name must match the value
/// of an ID attribute on some element in the XML document; i.e. IDREF
/// values must match the value of some ID attribute.
///
/// `[ VC: Entity Name ]`  
/// Values of type ENTITY must match the Name production, values
/// of type ENTITIES must match Names; each Entity Name must match the
/// name of an unparsed entity declared in the DTD.
///
/// `[ VC: Name Token ]`  
/// Values of type NMTOKEN must match the Nmtoken production; values
/// of type NMTOKENS must match Nmtokens.
///
/// Returns the attribute type
#[doc(alias = "xmlParseAttributeType")]
pub(crate) unsafe fn xml_parse_attribute_type(
    ctxt: XmlParserCtxtPtr,
    tree: &mut Option<Box<XmlEnumeration>>,
) -> Option<XmlAttributeType> {
    if (*ctxt).content_bytes().starts_with(b"CDATA") {
        (*ctxt).advance(5);
        return Some(XmlAttributeType::XmlAttributeCDATA);
    } else if (*ctxt).content_bytes().starts_with(b"IDREFS") {
        (*ctxt).advance(6);
        return Some(XmlAttributeType::XmlAttributeIDREFS);
    } else if (*ctxt).content_bytes().starts_with(b"IDREF") {
        (*ctxt).advance(5);
        return Some(XmlAttributeType::XmlAttributeIDREF);
    } else if (*ctxt).current_byte() == b'I' && NXT!(ctxt, 1) == b'D' {
        (*ctxt).advance(2);
        return Some(XmlAttributeType::XmlAttributeID);
    } else if (*ctxt).content_bytes().starts_with(b"ENTITY") {
        (*ctxt).advance(6);
        return Some(XmlAttributeType::XmlAttributeEntity);
    } else if (*ctxt).content_bytes().starts_with(b"ENTITIES") {
        (*ctxt).advance(8);
        return Some(XmlAttributeType::XmlAttributeEntities);
    } else if (*ctxt).content_bytes().starts_with(b"NMTOKENS") {
        (*ctxt).advance(8);
        return Some(XmlAttributeType::XmlAttributeNmtokens);
    } else if (*ctxt).content_bytes().starts_with(b"NMTOKEN") {
        (*ctxt).advance(7);
        return Some(XmlAttributeType::XmlAttributeNmtoken);
    }
    xml_parse_enumerated_type(ctxt, tree)
}

/// Add a defaulted attribute for an element
#[doc(alias = "xmlAddDefAttrs")]
pub(crate) unsafe fn xml_add_def_attrs(
    ctxt: XmlParserCtxtPtr,
    fullname: *const XmlChar,
    fullattr: *const XmlChar,
    mut value: *const XmlChar,
) {
    let mut defaults: XmlDefAttrsPtr;
    let mut len: i32 = 0;
    let mut name: *const XmlChar;
    let mut prefix: *const XmlChar;

    // Allows to detect attribute redefinitions
    if (*ctxt)
        .atts_special
        .filter(|t| {
            t.lookup2(
                CStr::from_ptr(fullname as *const i8)
                    .to_string_lossy()
                    .as_ref(),
                (!fullattr.is_null())
                    .then(|| CStr::from_ptr(fullattr as *const i8).to_string_lossy())
                    .as_deref(),
            )
            .is_some()
        })
        .is_some()
    {
        return;
    }

    'mem_error: {
        let mut atts_default = if let Some(table) = (*ctxt).atts_default {
            table
        } else {
            let table = XmlHashTable::with_capacity(10);
            let Some(table) = XmlHashTableRef::from_table(table) else {
                break 'mem_error;
            };
            (*ctxt).atts_default = Some(table);
            table
        };

        // split the element name into prefix:localname , the string found
        // are within the DTD and then not associated to namespace names.
        name = xml_split_qname3(fullname, addr_of_mut!(len));
        if name.is_null() {
            name = xml_dict_lookup((*ctxt).dict, fullname, -1);
            prefix = null_mut();
        } else {
            name = xml_dict_lookup((*ctxt).dict, name, -1);
            prefix = xml_dict_lookup((*ctxt).dict, fullname, len);
        }

        // make sure there is some storage
        defaults = atts_default
            .lookup2(
                CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
                (!prefix.is_null())
                    .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                    .as_deref(),
            )
            .map_or(null_mut(), |p| *p);
        if defaults.is_null() {
            defaults =
                xml_malloc(size_of::<XmlDefAttrs>() + (4 * 5) * size_of::<*const XmlChar>()) as _;
            if defaults.is_null() {
                break 'mem_error;
            }
            (*defaults).nb_attrs = 0;
            (*defaults).max_attrs = 4;
            if atts_default
                .update_entry2(
                    CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
                    (!prefix.is_null())
                        .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                        .as_deref(),
                    defaults,
                    |_, _| {},
                )
                .is_err()
            {
                xml_free(defaults as _);
                break 'mem_error;
            }
        } else if (*defaults).nb_attrs >= (*defaults).max_attrs {
            let temp: XmlDefAttrsPtr = xml_realloc(
                defaults as _,
                size_of::<XmlDefAttrs>()
                    + (2 * (*defaults).max_attrs as usize * 5) * size_of::<*const XmlChar>(),
            ) as _;
            if temp.is_null() {
                break 'mem_error;
            }
            defaults = temp;
            (*defaults).max_attrs *= 2;
            if atts_default
                .update_entry2(
                    CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
                    (!prefix.is_null())
                        .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                        .as_deref(),
                    defaults,
                    |_, _| {},
                )
                .is_err()
            {
                xml_free(defaults as _);
                break 'mem_error;
            }
        }

        // Split the element name into prefix:localname , the string found
        // are within the DTD and hen not associated to namespace names.
        name = xml_split_qname3(fullattr, addr_of_mut!(len));
        if name.is_null() {
            name = xml_dict_lookup((*ctxt).dict, fullattr, -1);
            prefix = null_mut();
        } else {
            name = xml_dict_lookup((*ctxt).dict, name, -1);
            prefix = xml_dict_lookup((*ctxt).dict, fullattr, len);
        }

        *(*defaults)
            .values
            .as_mut_ptr()
            .add(5 * (*defaults).nb_attrs as usize) = name;
        *(*defaults)
            .values
            .as_mut_ptr()
            .add(5 * (*defaults).nb_attrs as usize + 1) = prefix;
        // intern the string and precompute the end
        len = xml_strlen(value);
        value = xml_dict_lookup((*ctxt).dict, value, len);
        if value.is_null() {
            break 'mem_error;
        }
        *(*defaults)
            .values
            .as_mut_ptr()
            .add(5 * (*defaults).nb_attrs as usize + 2) = value;
        *(*defaults)
            .values
            .as_mut_ptr()
            .add(5 * (*defaults).nb_attrs as usize + 3) = value.add(len as usize);
        if (*ctxt).external != 0 {
            *(*defaults)
                .values
                .as_mut_ptr()
                .add(5 * (*defaults).nb_attrs as usize + 4) = c"external".as_ptr() as _;
        } else {
            *(*defaults)
                .values
                .as_mut_ptr()
                .add(5 * (*defaults).nb_attrs as usize + 4) = null_mut();
        }
        (*defaults).nb_attrs += 1;
        return;
    }

    xml_err_memory(ctxt, None);
}

/// Register this attribute type
#[doc(alias = "xmlAddSpecialAttr")]
pub(crate) unsafe fn xml_add_special_attr(
    ctxt: XmlParserCtxtPtr,
    fullname: *const XmlChar,
    fullattr: *const XmlChar,
    typ: XmlAttributeType,
) {
    let mut atts_special = if let Some(table) = (*ctxt).atts_special {
        table
    } else {
        let table = XmlHashTable::with_capacity(10);
        let Some(table) = XmlHashTableRef::from_table(table) else {
            xml_err_memory(ctxt, None);
            return;
        };
        (*ctxt).atts_special = Some(table);
        table
    };

    let fullname = CStr::from_ptr(fullname as *const i8).to_string_lossy();
    let fullattr =
        (!fullattr.is_null()).then(|| CStr::from_ptr(fullattr as *const i8).to_string_lossy());
    if atts_special
        .lookup2(&fullname, fullattr.as_deref())
        .is_some()
    {
        return;
    }

    atts_special.add_entry2(&fullname, fullattr.as_deref(), typ);
}

/// Parse the declaration for a Mixed Element content
/// The leading '(' and spaces have been skipped in xmlParseElementContentDecl
///
/// `[51] Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' | '(' S? '#PCDATA' S? ')'`
///
/// `[ VC: Proper Group/PE Nesting ]` applies to [51] too (see [49])
///
/// `[ VC: No Duplicate Types ]`  
/// The same name must not appear more than once in a single
/// mixed-content declaration.
///
/// returns: the list of the xmlElementContentPtr describing the element choices
#[doc(alias = "xmlParseElementMixedContentDecl")]
pub(crate) unsafe fn xml_parse_element_mixed_content_decl(
    ctxt: XmlParserCtxtPtr,
    inputchk: i32,
) -> XmlElementContentPtr {
    let mut ret: XmlElementContentPtr = null_mut();
    let mut cur: XmlElementContentPtr = null_mut();
    let mut n: XmlElementContentPtr;

    (*ctxt).grow();
    if (*ctxt).content_bytes().starts_with(b"#PCDATA") {
        (*ctxt).advance(7);
        (*ctxt).skip_blanks();
        if (*ctxt).current_byte() == b')' {
            if (*(*ctxt).input).id != inputchk {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    "Element content declaration doesn't start and stop in the same entity\n",
                );
            }
            (*ctxt).skip_char();
            ret = xml_new_doc_element_content(
                (*ctxt).my_doc,
                None,
                XmlElementContentType::XmlElementContentPCDATA,
            );
            if ret.is_null() {
                return null_mut();
            }
            if (*ctxt).current_byte() == b'*' {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
                (*ctxt).skip_char();
            }
            return ret;
        }
        if (*ctxt).current_byte() == b'(' || (*ctxt).current_byte() == b'|' {
            ret = xml_new_doc_element_content(
                (*ctxt).my_doc,
                None,
                XmlElementContentType::XmlElementContentPCDATA,
            );
            cur = ret;
            if ret.is_null() {
                return null_mut();
            }
        }
        let mut elem: Option<String> = None;
        while (*ctxt).current_byte() == b'|'
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            (*ctxt).skip_char();
            if let Some(elem) = elem.as_deref() {
                n = xml_new_doc_element_content(
                    (*ctxt).my_doc,
                    None,
                    XmlElementContentType::XmlElementContentOr,
                );
                if n.is_null() {
                    xml_free_doc_element_content((*ctxt).my_doc, ret);
                    return null_mut();
                }
                (*n).c1 = xml_new_doc_element_content(
                    (*ctxt).my_doc,
                    Some(elem),
                    XmlElementContentType::XmlElementContentElement,
                );
                if !(*n).c1.is_null() {
                    (*(*n).c1).parent = n;
                }
                (*cur).c2 = n;
                if !n.is_null() {
                    (*n).parent = cur;
                }
                cur = n;
            } else {
                ret = xml_new_doc_element_content(
                    (*ctxt).my_doc,
                    None,
                    XmlElementContentType::XmlElementContentOr,
                );
                if ret.is_null() {
                    xml_free_doc_element_content((*ctxt).my_doc, cur);
                    return null_mut();
                }
                (*ret).c1 = cur;
                if !cur.is_null() {
                    (*cur).parent = ret;
                }
                cur = ret;
            }
            (*ctxt).skip_blanks();
            elem = parse_name(&mut *ctxt);
            if elem.is_none() {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    "xmlParseElementMixedContentDecl : Name expected\n",
                );
                xml_free_doc_element_content((*ctxt).my_doc, ret);
                return null_mut();
            }
            (*ctxt).skip_blanks();
            (*ctxt).grow();
        }
        if (*ctxt).current_byte() == b')' && NXT!(ctxt, 1) == b'*' {
            if let Some(elem) = elem {
                (*cur).c2 = xml_new_doc_element_content(
                    (*ctxt).my_doc,
                    Some(&elem),
                    XmlElementContentType::XmlElementContentElement,
                );
                if !(*cur).c2.is_null() {
                    (*(*cur).c2).parent = cur;
                }
            }
            if !ret.is_null() {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
            }
            if (*(*ctxt).input).id != inputchk {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    "Element content declaration doesn't start and stop in the same entity\n",
                );
            }
            (*ctxt).advance(2);
        } else {
            xml_free_doc_element_content((*ctxt).my_doc, ret);
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrMixedNotStarted, None);
            return null_mut();
        }
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrPCDATARequired, None);
    }
    ret
}

/// Parse the declaration for a Mixed Element content
/// The leading '(' and spaces have been skipped in xmlParseElementContentDecl
///
/// `[47] children ::= (choice | seq) ('?' | '*' | '+')?`
///
/// `[48] cp ::= (Name | choice | seq) ('?' | '*' | '+')?`
///
/// `[49] choice ::= '(' S? cp ( S? '|' S? cp )* S? ')'`
///
/// `[50] seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'`
///
/// `[ VC: Proper Group/PE Nesting ]` applies to [49] and [50]
///
/// TODO Parameter-entity replacement text must be properly nested
///    with parenthesized groups. That is to say, if either of the
///    opening or closing parentheses in a choice, seq, or Mixed
///    construct is contained in the replacement text for a parameter
///    entity, both must be contained in the same replacement text. For
///    interoperability, if a parameter-entity reference appears in a
///    choice, seq, or Mixed construct, its replacement text should not
///    be empty, and neither the first nor last non-blank character of
///    the replacement text should be a connector (| or ,).
///
/// Returns the tree of xmlElementContentPtr describing the element hierarchy.
#[doc(alias = "xmlParseElementChildrenContentDecl")]
pub(crate) unsafe fn xml_parse_element_children_content_decl(
    ctxt: XmlParserCtxtPtr,
    inputchk: i32,
) -> XmlElementContentPtr {
    // stub left for API/ABI compat
    xml_parse_element_children_content_decl_priv(ctxt, inputchk, 1)
}

/// Parse the declaration for an Element content either Mixed or Children,
/// the cases EMPTY and ANY are handled directly in xmlParseElementDecl
///
/// `[46] contentspec ::= 'EMPTY' | 'ANY' | Mixed | children`
///
/// returns: the type of element content XML_ELEMENT_TYPE_xxx
#[doc(alias = "xmlParseElementContentDecl")]
pub(crate) unsafe fn xml_parse_element_content_decl(
    ctxt: XmlParserCtxtPtr,
    name: *const XmlChar,
    result: *mut XmlElementContentPtr,
) -> Option<XmlElementTypeVal> {
    let tree: XmlElementContentPtr;
    let inputid: i32 = (*(*ctxt).input).id;

    *result = null_mut();

    if (*ctxt).current_byte() != b'(' {
        let name = CStr::from_ptr(name as *const i8).to_string_lossy();
        xml_fatal_err_msg_str!(
            ctxt,
            XmlParserErrors::XmlErrElemcontentNotStarted,
            "xmlParseElementContentDecl : {} '(' expected\n",
            name
        );
        return None;
    }
    (*ctxt).skip_char();
    (*ctxt).grow();
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return None;
    }
    (*ctxt).skip_blanks();
    let res = if (*ctxt).content_bytes().starts_with(b"#PCDATA") {
        tree = xml_parse_element_mixed_content_decl(ctxt, inputid);
        XmlElementTypeVal::XmlElementTypeMixed
    } else {
        tree = xml_parse_element_children_content_decl_priv(ctxt, inputid, 1);
        XmlElementTypeVal::XmlElementTypeElement
    };
    (*ctxt).skip_blanks();
    *result = tree;
    Some(res)
}

/// Parse an entitiy reference. Always consumes '&'.
///
/// `[68] EntityRef ::= '&' Name ';'`
///
/// `[ WFC: Entity Declared ]`  
/// In a document without any DTD, a document with only an internal DTD
/// subset which contains no parameter entity references, or a document
/// with "standalone='yes'", the Name given in the entity reference
/// must match that in an entity declaration, except that well-formed
/// documents need not declare any of the following entities: amp, lt,
/// gt, apos, quot.  The declaration of a parameter entity must precede
/// any reference to it.  Similarly, the declaration of a general entity
/// must precede any reference to it which appears in a default value in an
/// attribute-list declaration. Note that if entities are declared in the
/// external subset or in external parameter entities, a non-validating
/// processor is not obligated to read and process their declarations;
/// for such documents, the rule that an entity must be declared is a
/// well-formedness constraint only if standalone='yes'.
///
/// `[ WFC: Parsed Entity ]`  
/// An entity reference must not contain the name of an unparsed entity
///
/// Returns the xmlEntityPtr if found, or NULL otherwise.
#[doc(alias = "xmlParseEntityRef")]
pub(crate) unsafe fn xml_parse_entity_ref(ctxt: XmlParserCtxtPtr) -> Option<XmlEntityPtr> {
    (*ctxt).grow();
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return None;
    }

    if (*ctxt).current_byte() != b'&' {
        return None;
    }
    (*ctxt).skip_char();
    let Some(name) = parse_name(&mut *ctxt) else {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            "xmlParseEntityRef: no name\n",
        );
        return None;
    };
    if (*ctxt).current_byte() != b';' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityRefSemicolMissing, None);
        return None;
    }
    (*ctxt).skip_char();

    // Predefined entities override any extra definition
    if (*ctxt).options & XmlParserOption::XmlParseOldSAX as i32 == 0 {
        if let Some(ent) = xml_get_predefined_entity(&name) {
            return Some(ent);
        }
    }

    let mut ent = None;
    // Ask first SAX for entity resolution, otherwise try the
    // entities which may have stored in the parser context.
    if let Some(sax) = (*ctxt).sax.as_deref_mut() {
        if let Some(f) = sax.get_entity {
            ent = f((*ctxt).user_data.clone(), &name);
        }
        if (*ctxt).well_formed == 1
            && ent.is_none()
            && (*ctxt).options & XmlParserOption::XmlParseOldSAX as i32 != 0
        {
            ent = xml_get_predefined_entity(&name);
        }
        if (*ctxt).well_formed == 1
            && ent.is_none()
            && (*ctxt)
                .user_data
                .as_ref()
                .and_then(|d| d.lock().downcast_ref::<XmlParserCtxtPtr>().copied())
                == Some(ctxt)
        {
            ent = xml_sax2_get_entity(Some(GenericErrorContext::new(ctxt)), &name);
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return None;
    }
    // [ WFC: Entity Declared ]
    // In a document without any DTD, a document with only an
    // internal DTD subset which contains no parameter entity
    // references, or a document with "standalone='yes'", the
    // Name given in the entity reference must match that in an
    // entity declaration, except that well-formed documents
    // need not declare any of the following entities: amp, lt,
    // gt, apos, quot.
    // The declaration of a parameter entity must precede any
    // reference to it.
    // Similarly, the declaration of a general entity must
    // precede any reference to it which appears in a default
    // value in an attribute-list declaration. Note that if
    // entities are declared in the external subset or in
    // external parameter entities, a non-validating processor
    // is not obligated to read and process their declarations;
    // for such documents, the rule that an entity must be
    // declared is a well-formedness constraint only if
    // standalone='yes'.
    if let Some(mut ent) = ent {
        if matches!(ent.etype, XmlEntityType::XmlExternalGeneralUnparsedEntity) {
            // [ WFC: Parsed Entity ]
            // An entity reference must not contain the name of an unparsed entity
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrUnparsedEntity,
                "Entity reference to unparsed entity {}\n",
                name
            );
        } else if matches!(
            (*ctxt).instate,
            XmlParserInputState::XmlParserAttributeValue
        ) && matches!(ent.etype, XmlEntityType::XmlExternalGeneralParsedEntity)
        {
            // [ WFC: No External Entity References ]
            // Attribute values cannot contain direct or indirect
            // entity references to external entities.
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrEntityIsExternal,
                "Attribute references external entity '{}'\n",
                name
            );
        } else if matches!(
            (*ctxt).instate,
            XmlParserInputState::XmlParserAttributeValue
        ) && !matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
        {
            // [ WFC: No < in Attribute Values ]
            // The replacement text of any entity referred to directly or
            // indirectly in an attribute value (other than "&lt;") must not contain a <.
            if ent.flags & XML_ENT_CHECKED_LT as i32 == 0 {
                if !ent.content.load(Ordering::Relaxed).is_null()
                    && !xml_strchr(ent.content.load(Ordering::Relaxed) as _, b'<').is_null()
                {
                    ent.flags |= XML_ENT_CONTAINS_LT as i32;
                }
                ent.flags |= XML_ENT_CHECKED_LT as i32;
            }
            if ent.flags & XML_ENT_CONTAINS_LT as i32 != 0 {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrLtInAttribute,
                    "'<' in entity '{}' is not allowed in attributes values\n",
                    name
                );
            }
        } else {
            // Internal check, no parameter entities here ...
            match ent.etype {
                XmlEntityType::XmlInternalParameterEntity
                | XmlEntityType::XmlExternalParameterEntity => {
                    xml_fatal_err_msg_str!(
                        ctxt,
                        XmlParserErrors::XmlErrEntityIsParameter,
                        "Attempt to reference the parameter entity '{}'\n",
                        name
                    );
                }
                _ => {}
            }
        }
    } else {
        if (*ctxt).standalone == 1 || ((*ctxt).has_external_subset == 0 && (*ctxt).has_perefs == 0)
        {
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrUndeclaredEntity,
                "Entity '{}' not defined\n",
                name
            );
        } else {
            xml_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlWarUndeclaredEntity,
                "Entity '{}' not defined\n",
                name
            );
            if (*ctxt).in_subset == 0 {
                if let Some(reference) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.reference) {
                    reference((*ctxt).user_data.clone(), &name);
                }
            }
        }
        (*ctxt).valid = 0;
    }

    // [ WFC: No Recursion ]
    // A parsed entity must not contain a recursive reference
    // to itself, either directly or indirectly.
    // Done somewhere else
    ent
}

/// Parse a well-balanced chunk of an XML document called by the parser
/// The allowed sequence for the Well Balanced Chunk is the one defined by
/// the content production in the XML grammar:
///
/// `[43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*`
///
/// Returns xmlParserErrors::XML_ERR_OK if the chunk is well balanced, and the parser
/// error code otherwise
///
/// In case recover is set to 1, the nodelist will not be empty even if
/// the parsed chunk is not well balanced.
#[doc(alias = "xmlParseBalancedChunkMemoryInternal")]
unsafe fn xml_parse_balanced_chunk_memory_internal(
    oldctxt: XmlParserCtxtPtr,
    string: *const XmlChar,
    user_data: Option<GenericErrorContext>,
    mut lst: Option<&mut Option<XmlGenericNodePtr>>,
) -> XmlParserErrors {
    let mut content = None;
    let mut last = None;
    let ret: XmlParserErrors;

    if ((*oldctxt).depth > 40 && (*oldctxt).options & XmlParserOption::XmlParseHuge as i32 == 0)
        || (*oldctxt).depth > 100
    {
        xml_fatal_err_msg(
            oldctxt,
            XmlParserErrors::XmlErrEntityLoop,
            "Maximum entity nesting depth exceeded",
        );
        return XmlParserErrors::XmlErrEntityLoop;
    }

    if let Some(lst) = lst.as_mut() {
        **lst = None;
    }
    if string.is_null() {
        return XmlParserErrors::XmlErrInternalError;
    }

    let ctxt: XmlParserCtxtPtr =
        xml_create_memory_parser_ctxt(CStr::from_ptr(string as *const i8).to_bytes().to_vec());
    if ctxt.is_null() {
        return XmlParserErrors::XmlWarUndeclaredEntity;
    }
    (*ctxt).nb_errors = (*oldctxt).nb_errors;
    (*ctxt).nb_warnings = (*oldctxt).nb_warnings;
    if user_data.is_some() {
        (*ctxt).user_data = user_data;
    } else {
        (*ctxt).user_data = Some(GenericErrorContext::new(ctxt));
    }
    if !(*ctxt).dict.is_null() {
        xml_dict_free((*ctxt).dict);
    }
    (*ctxt).dict = (*oldctxt).dict;
    (*ctxt).input_id = (*oldctxt).input_id;
    (*ctxt).str_xml = Some(Cow::Borrowed("xml"));
    (*ctxt).str_xmlns = Some(Cow::Borrowed("xmlns"));
    (*ctxt).str_xml_ns = Some(Cow::Borrowed(XML_XML_NAMESPACE.to_str().unwrap()));

    // propagate namespaces down the entity
    for (pre, loc) in &(*oldctxt).ns_tab {
        (*ctxt).ns_push(pre.as_deref(), loc);
    }

    let oldsax = (*ctxt).sax.take();
    (*ctxt).sax = (*oldctxt).sax.take();
    (*ctxt).detect_sax2();
    (*ctxt).replace_entities = (*oldctxt).replace_entities;
    (*ctxt).options = (*oldctxt).options;
    (*ctxt)._private = (*oldctxt)._private;

    let mut new_doc = None;
    let mut my_doc = if let Some(my_doc) = (*oldctxt).my_doc {
        (*ctxt).my_doc = Some(my_doc);
        content = my_doc
            .children
            .and_then(|c| XmlGenericNodePtr::from_raw(c.as_ptr()));
        last = my_doc
            .last
            .and_then(|l| XmlGenericNodePtr::from_raw(l.as_ptr()));
        my_doc
    } else {
        let Some(mut new) = xml_new_doc(Some("1.0")) else {
            (*oldctxt).sax = (*ctxt).sax.take();
            (*ctxt).sax = oldsax;
            (*ctxt).dict = null_mut();
            xml_free_parser_ctxt(ctxt);
            return XmlParserErrors::XmlErrInternalError;
        };
        new_doc = Some(new);
        new.properties = XmlDocProperties::XmlDocInternal as i32;
        (*ctxt).my_doc = Some(new);
        new
    };
    let Some(new_root) = xml_new_doc_node((*ctxt).my_doc, None, "pseudoroot", null_mut()) else {
        (*oldctxt).sax = (*ctxt).sax.take();
        (*ctxt).sax = oldsax;
        (*ctxt).dict = null_mut();
        xml_free_parser_ctxt(ctxt);
        if let Some(new_doc) = new_doc {
            xml_free_doc(new_doc);
        }
        return XmlParserErrors::XmlErrInternalError;
    };
    my_doc.children = None;
    my_doc.last = None;
    my_doc.add_child(new_root.into());
    (*ctxt).node_push(
        my_doc
            .children
            .and_then(|c| XmlNodePtr::from_raw(c.as_ptr()).unwrap())
            .unwrap(),
    );
    (*ctxt).instate = XmlParserInputState::XmlParserContent;
    (*ctxt).depth = (*oldctxt).depth;

    (*ctxt).validate = 0;
    (*ctxt).loadsubset = (*oldctxt).loadsubset;
    if (*oldctxt).validate != 0 || (*oldctxt).replace_entities != 0 {
        // ID/IDREF registration will be done in xmlValidateElement below
        (*ctxt).loadsubset |= XML_SKIP_IDS as i32;
    }
    (*ctxt).dict_names = (*oldctxt).dict_names;
    (*ctxt).atts_default = (*oldctxt).atts_default;
    (*ctxt).atts_special = (*oldctxt).atts_special;

    xml_parse_content(ctxt);
    if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
    } else if (*ctxt).current_byte() != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtraContent, None);
    }
    if NodePtr::from_ptr((*ctxt).node.map_or(null_mut(), |node| node.as_ptr())) != my_doc.children {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
    }

    if (*ctxt).well_formed == 0 {
        ret = XmlParserErrors::try_from((*ctxt).err_no).unwrap();
        (*oldctxt).err_no = (*ctxt).err_no;
        (*oldctxt).well_formed = 0;
        (*oldctxt).last_error = (*ctxt).last_error.clone();
    } else {
        ret = XmlParserErrors::XmlErrOK;
    }

    if let Some(lst) = lst {
        if matches!(ret, XmlParserErrors::XmlErrOK) {
            // Return the newly created nodeset after unlinking it from
            // they pseudo parent.
            let mut cur = my_doc
                .children()
                .unwrap()
                .children()
                .and_then(|c| XmlGenericNodePtr::from_raw(c.as_ptr()));
            *lst = cur;
            while let Some(mut now) = cur {
                #[cfg(feature = "libxml_valid")]
                if (*oldctxt).validate != 0
                    && (*oldctxt).well_formed != 0
                    && now.element_type() == XmlElementType::XmlElementNode
                {
                    if let Some(my_doc) = (*oldctxt).my_doc.filter(|doc| doc.int_subset.is_some()) {
                        (*oldctxt).valid &=
                            xml_validate_element(addr_of_mut!((*oldctxt).vctxt), my_doc, cur);
                    }
                }
                now.set_parent(None);
                cur = now
                    .next()
                    .and_then(|n| XmlGenericNodePtr::from_raw(n.as_ptr()));
            }
            my_doc.children.unwrap().set_children(None);
        }
    }
    if let Some(mut my_doc) = (*ctxt).my_doc {
        xml_free_node(
            my_doc
                .children
                .and_then(|c| XmlGenericNodePtr::from_raw(c.as_ptr()))
                .unwrap(),
        );
        my_doc.children = NodePtr::from_ptr(content.map_or(null_mut(), |node| node.as_ptr()));
        my_doc.last = NodePtr::from_ptr(last.map_or(null_mut(), |node| node.as_ptr()));
    }

    // Also record the size of the entity parsed
    if !(*ctxt).input.is_null() && !oldctxt.is_null() {
        let mut consumed: u64 = (*(*ctxt).input).consumed;
        consumed = consumed.saturating_add((*(*ctxt).input).offset_from_base() as u64);

        (*oldctxt).sizeentcopy = (*oldctxt).sizeentcopy.saturating_add(consumed);
        (*oldctxt).sizeentcopy = (*oldctxt).sizeentcopy.saturating_add((*ctxt).sizeentcopy);
    }

    (*oldctxt).nb_errors = (*ctxt).nb_errors;
    (*oldctxt).nb_warnings = (*ctxt).nb_warnings;
    (*oldctxt).sax = (*ctxt).sax.take();
    (*ctxt).sax = oldsax;
    (*ctxt).dict = null_mut();
    (*ctxt).atts_default = None;
    (*ctxt).atts_special = None;
    xml_free_parser_ctxt(ctxt);
    if let Some(new_doc) = new_doc {
        xml_free_doc(new_doc);
    }

    ret
}

/// Parse and handle entity references in content, depending on the SAX interface,
/// this may end-up in a call to character() if this is a CharRef,
/// a predefined entity, if there is no reference() callback.
/// or if the parser was asked to match to that mode.
///
/// Always consumes '&'.
///
/// `[67] Reference ::= EntityRef | CharRef`
#[doc(alias = "xmlParseReference")]
pub(crate) unsafe fn xml_parse_reference(ctxt: XmlParserCtxtPtr) {
    let val: *mut XmlChar;
    let mut was_checked: i32;
    let mut list = None;
    let mut ret: XmlParserErrors;

    if (*ctxt).current_byte() != b'&' {
        return;
    }

    // Simple case of a CharRef
    if NXT!(ctxt, 1) == b'#' {
        let mut out = [0; 16];
        let hex: i32 = NXT!(ctxt, 2) as _;
        let Some(value) = parse_char_ref(&mut *ctxt) else {
            return;
        };

        if (*ctxt).charset != XmlCharEncoding::UTF8 {
            // So we are using non-UTF-8 buffers
            // Check that the c_char fit on 8bits, if not
            // generate a CharRef.
            if value as u32 <= 0xFF {
                let out = value.encode_utf8(&mut out[..]);
                if (*ctxt).disable_sax == 0 {
                    if let Some(characters) =
                        (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
                    {
                        characters((*ctxt).user_data.clone(), out);
                    }
                }
            } else {
                let mut slice = &mut out[..];
                if hex == b'x' as i32 || hex == b'X' as i32 {
                    write!(slice, "#x{:X}", value as u32);
                } else {
                    write!(slice, "#{}", value as u32);
                }
                let rem = slice.len();
                let len = out.len() - rem;
                if (*ctxt).disable_sax == 0 {
                    if let Some(reference) =
                        (*ctxt).sax.as_deref_mut().and_then(|sax| sax.reference)
                    {
                        // # Safety
                        // `out` contains only '#', 'x' and ascii hex digit characters.
                        // Therefore, UTF-8 validation won't fail.
                        let out = unsafe { from_utf8_unchecked(&out[..len]) };
                        reference((*ctxt).user_data.clone(), out);
                    }
                }
            }
        } else {
            // Just encode the value in UTF-8
            let out = value.encode_utf8(&mut out[..]);
            if (*ctxt).disable_sax == 0 {
                if let Some(characters) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
                {
                    characters((*ctxt).user_data.clone(), out);
                }
            }
        }
        return;
    }

    // We are seeing an entity reference
    let Some(mut ent) = xml_parse_entity_ref(ctxt) else {
        return;
    };
    if (*ctxt).well_formed == 0 {
        return;
    }
    was_checked = ent.flags & XML_ENT_PARSED as i32;

    // special case of predefined entities
    if ent.name.load(Ordering::Relaxed).is_null()
        || matches!(ent.etype, XmlEntityType::XmlInternalPredefinedEntity)
    {
        val = ent.content.load(Ordering::Relaxed) as _;
        if val.is_null() {
            return;
        }
        // inline the entity.
        if (*ctxt).disable_sax == 0 {
            if let Some(characters) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters) {
                characters(
                    (*ctxt).user_data.clone(),
                    &CStr::from_ptr(val as *const i8).to_string_lossy(),
                );
            }
        }
        return;
    }

    // The first reference to the entity trigger a parsing phase
    // where the (*ent).children is filled with the result from the parsing.
    // Note: external parsed entities will not be loaded, it is not
    // required for a non-validating parser, unless the parsing option
    // of validating, or substituting entities were given. Doing so is
    // far more secure as the parser will only process data coming from
    // the document entity by default.
    if ent.flags & XML_ENT_PARSED as i32 == 0
        && (!matches!(ent.etype, XmlEntityType::XmlExternalGeneralParsedEntity)
            || (*ctxt).options
                & (XmlParserOption::XmlParseNoEnt as i32
                    | XmlParserOption::XmlParseDTDValid as i32)
                != 0)
    {
        let oldsizeentcopy: u64 = (*ctxt).sizeentcopy;

        // This is a bit hackish but this seems the best
        // way to make sure both SAX and DOM entity support
        // behaves okay.
        let user_data = if (*ctxt)
            .user_data
            .as_ref()
            .and_then(|d| d.lock().downcast_ref::<XmlParserCtxtPtr>().copied())
            == Some(ctxt)
        {
            None
        } else {
            (*ctxt).user_data.clone()
        };

        // Avoid overflow as much as possible
        (*ctxt).sizeentcopy = 0;

        if ent.flags & XML_ENT_EXPANDING as i32 != 0 {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, None);
            (*ctxt).halt();
            return;
        }

        ent.flags |= XML_ENT_EXPANDING as i32;

        // Check that this entity is well formed
        // 4.3.2: An internal general parsed entity is well-formed
        // if its replacement text matches the production labeled content.
        if matches!(ent.etype, XmlEntityType::XmlInternalGeneralEntity) {
            (*ctxt).depth += 1;
            ret = xml_parse_balanced_chunk_memory_internal(
                ctxt,
                ent.content.load(Ordering::Relaxed),
                user_data,
                Some(&mut list),
            );
            (*ctxt).depth -= 1;
        } else if matches!(ent.etype, XmlEntityType::XmlExternalGeneralParsedEntity) {
            (*ctxt).depth += 1;
            let uri = ent.uri.load(Ordering::Relaxed);
            let external_id = ent.external_id.load(Ordering::Relaxed);
            let has_sax = (*ctxt).sax.is_some();
            let (sax, error) = xml_parse_external_entity_private(
                (*ctxt).my_doc.unwrap(),
                ctxt,
                (*ctxt).sax.take(),
                user_data,
                (*ctxt).depth,
                (!uri.is_null())
                    .then(|| CStr::from_ptr(uri as *const i8).to_string_lossy())
                    .as_deref(),
                (!external_id.is_null())
                    .then(|| CStr::from_ptr(external_id as *const i8).to_string_lossy())
                    .as_deref(),
                Some(&mut list),
            );
            assert_eq!(has_sax, sax.is_some());
            (*ctxt).sax = sax;
            ret = error;
            (*ctxt).depth -= 1;
        } else {
            ret = XmlParserErrors::XmlErrEntityPEInternal;
            xml_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                "invalid entity type found\n"
            );
        }

        ent.flags &= !XML_ENT_EXPANDING as i32;
        ent.flags |= (XML_ENT_PARSED | XML_ENT_CHECKED) as i32;
        ent.expanded_size = (*ctxt).sizeentcopy;
        if matches!(ret, XmlParserErrors::XmlErrEntityLoop) {
            (*ctxt).halt();
            xml_free_node_list(list);
            return;
        }
        if xml_parser_entity_check(ctxt, oldsizeentcopy) != 0 {
            xml_free_node_list(list);
            return;
        }

        if let Some(l) = list.filter(|_| matches!(ret, XmlParserErrors::XmlErrOK)) {
            ent.children.store(l.as_ptr(), Ordering::Relaxed);
            // Prune it directly in the generated document
            // except for single text nodes.
            if (*ctxt).replace_entities == 0
                || matches!((*ctxt).parse_mode, XmlParserMode::XmlParseReader)
                || (matches!(l.element_type(), XmlElementType::XmlTextNode) && l.next().is_none())
            {
                ent.owner = 1;
                let mut cur = Some(l);
                while let Some(mut now) = cur {
                    now.set_parent(NodePtr::from_ptr(ent.as_ptr() as *mut XmlNode));
                    if now.document().map_or(null_mut(), |doc| doc.as_ptr())
                        != ent.doc.load(Ordering::Relaxed) as _
                    {
                        now.set_doc(XmlDocPtr::from_raw(ent.doc.load(Ordering::Relaxed)).unwrap());
                    }
                    if now.next().is_none() {
                        ent.last.store(now.as_ptr(), Ordering::Relaxed);
                    }
                    cur = now
                        .next()
                        .and_then(|n| XmlGenericNodePtr::from_raw(n.as_ptr()));
                }
                list = None;
            } else {
                ent.owner = 0;
                while let Some(mut now) = list {
                    now.set_parent(NodePtr::from_ptr(
                        (*ctxt).node.map_or(null_mut(), |node| node.as_ptr()),
                    ));
                    now.set_document((*ctxt).my_doc);
                    if now.next().is_none() {
                        ent.last.store(now.as_ptr(), Ordering::Relaxed);
                    }
                    list = now
                        .next()
                        .and_then(|n| XmlGenericNodePtr::from_raw(n.as_ptr()));
                }
                list = XmlGenericNodePtr::from_raw(ent.children.load(Ordering::Relaxed));
            }
        } else if !matches!(
            ret,
            XmlParserErrors::XmlErrOK | XmlParserErrors::XmlWarUndeclaredEntity
        ) {
            let name =
                CStr::from_ptr(ent.name.load(Ordering::Relaxed) as *const i8).to_string_lossy();
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrUndeclaredEntity,
                "Entity '{}' failed to parse\n",
                name
            );
            if !ent.content.load(Ordering::Relaxed).is_null() {
                *ent.content.load(Ordering::Relaxed).add(0) = 0;
            }
        } else if let Some(list) = list.take() {
            xml_free_node_list(Some(list));
        }

        // Prevent entity from being parsed and expanded twice (Bug 760367).
        was_checked = 0;
    }

    // Now that the entity content has been gathered
    // provide it to the application, this can take different forms based
    // on the parsing modes.
    if ent.children.load(Ordering::Relaxed).is_null() {
        // Probably running in SAX mode and the callbacks don't
        // build the entity content. So unless we already went
        // though parsing for first checking go though the entity
        // content to generate callbacks associated to the entity
        if was_checked != 0 {
            // This is a bit hackish but this seems the best
            // way to make sure both SAX and DOM entity support behaves okay.
            let user_data = if (*ctxt)
                .user_data
                .as_ref()
                .and_then(|d| d.lock().downcast_ref::<XmlParserCtxtPtr>().copied())
                == Some(ctxt)
            {
                None
            } else {
                (*ctxt).user_data.clone()
            };

            if matches!(ent.etype, XmlEntityType::XmlInternalGeneralEntity) {
                (*ctxt).depth += 1;
                ret = xml_parse_balanced_chunk_memory_internal(
                    ctxt,
                    ent.content.load(Ordering::Relaxed),
                    user_data,
                    None,
                );
                (*ctxt).depth -= 1;
            } else if matches!(ent.etype, XmlEntityType::XmlExternalGeneralParsedEntity) {
                let oldsizeentities: u64 = (*ctxt).sizeentities;

                (*ctxt).depth += 1;
                let uri = ent.uri.load(Ordering::Relaxed);
                let external_id = ent.external_id.load(Ordering::Relaxed);
                let has_sax = (*ctxt).sax.is_some();
                let (sax, error) = xml_parse_external_entity_private(
                    (*ctxt).my_doc.unwrap(),
                    ctxt,
                    (*ctxt).sax.take(),
                    user_data,
                    (*ctxt).depth,
                    (!uri.is_null())
                        .then(|| CStr::from_ptr(uri as *const i8).to_string_lossy())
                        .as_deref(),
                    (!external_id.is_null())
                        .then(|| CStr::from_ptr(external_id as *const i8).to_string_lossy())
                        .as_deref(),
                    None,
                );
                assert_eq!(has_sax, sax.is_some());
                (*ctxt).sax = sax;
                ret = error;
                (*ctxt).depth -= 1;

                // Undo the change to sizeentities
                (*ctxt).sizeentities = oldsizeentities;
            } else {
                ret = XmlParserErrors::XmlErrEntityPEInternal;
                xml_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "invalid entity type found\n"
                );
            }
            if matches!(ret, XmlParserErrors::XmlErrEntityLoop) {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, None);
                return;
            }
            if xml_parser_entity_check(ctxt, 0) != 0 {
                return;
            }
        }
        if (*ctxt).replace_entities == 0 && (*ctxt).disable_sax == 0 {
            if let Some(reference) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.reference) {
                // Entity reference callback comes second, it's somewhat
                // superfluous but a compatibility to historical behaviour
                reference((*ctxt).user_data.clone(), &(*ent).name().unwrap());
            }
        }
        return;
    }

    // We also check for amplification if entities aren't substituted.
    // They might be expanded later.
    if was_checked != 0 && xml_parser_entity_check(ctxt, ent.expanded_size) != 0 {
        return;
    }

    // If we didn't get any children for the entity being built
    if (*ctxt).replace_entities == 0 && (*ctxt).disable_sax == 0 {
        if let Some(reference) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.reference) {
            // Create a node.
            reference((*ctxt).user_data.clone(), &ent.name().unwrap());
            return;
        }
    }

    if (*ctxt).replace_entities != 0 {
        // There is a problem on the handling of _private for entities
        // (bug 155816): Should we copy the content of the field from
        // the entity (possibly overwriting some value set by the user
        // when a copy is created), should we leave it alone, or should
        // we try to take care of different situations?  The problem
        // is exacerbated by the usage of this field by the xmlReader.
        // To fix this bug, we look at _private on the created node
        // and, if it's NULL, we copy in whatever was in the entity.
        // If it's not NULL we leave it alone.  This is somewhat of a
        // hack - maybe we should have further tests to determine what to do.
        if let Some(mut context_node) = (*ctxt).node {
            // Seems we are generating the DOM content, do
            // a simple tree copy for all references except the first
            // In the first occurrence list contains the replacement.
            if (list.is_none() && ent.owner == 0)
                || matches!((*ctxt).parse_mode, XmlParserMode::XmlParseReader)
            {
                let mut first_child = None;

                // when operating on a reader, the entities definitions
                // are always owning the entities subtree.
                // if ((*ctxt).parseMode == XML_PARSE_READER)
                //     (*ent).owner = 1;

                let mut cur =
                    XmlNodePtr::from_raw(ent.children.load(Ordering::Relaxed) as _).unwrap();
                while let Some(cur_node) = cur {
                    let mut nw =
                        xml_doc_copy_node(XmlGenericNodePtr::from(cur_node), (*ctxt).my_doc, 1)
                            .map(|nw| XmlNodePtr::try_from(nw).unwrap());
                    if let Some(mut now) = nw {
                        if now._private.is_null() {
                            now._private = cur_node._private;
                        }
                        if first_child.is_none() {
                            first_child = nw;
                        }
                        nw = XmlNodePtr::try_from(context_node.add_child(now.into()).unwrap()).ok();
                    }
                    if cur == XmlNodePtr::from_raw(ent.last.load(Ordering::Relaxed) as _).unwrap() {
                        // needed to detect some strange empty
                        // node cases in the reader tests
                        if matches!((*ctxt).parse_mode, XmlParserMode::XmlParseReader) {
                            if let Some(mut nw) = nw
                                .filter(|nw| nw.element_type() == XmlElementType::XmlElementNode)
                                .filter(|nw| nw.children.is_none())
                            {
                                nw.extra = 1;
                            }
                        }

                        break;
                    }
                    cur = cur_node
                        .next
                        .and_then(|n| XmlNodePtr::from_raw(n.as_ptr()).unwrap());
                }
            } else if list.is_none() || !(*ctxt).input_tab.is_empty() {
                let mut first_child = None;

                // Copy the entity child list and make it the new
                // entity child list. The goal is to make sure any
                // ID or REF referenced will be the one from the
                // document content and not the entity copy.
                let mut cur =
                    XmlNodePtr::from_raw(ent.children.load(Ordering::Relaxed) as _).unwrap();
                ent.children.store(null_mut(), Ordering::Relaxed);
                let last = XmlNodePtr::from_raw(ent.last.load(Ordering::Relaxed) as _).unwrap();
                ent.last.store(null_mut(), Ordering::Relaxed);
                while let Some(mut cur_node) = cur {
                    let next = cur_node
                        .next
                        .take()
                        .and_then(|n| XmlNodePtr::from_raw(n.as_ptr()).unwrap());
                    cur_node.set_parent(None);
                    let nw = xml_doc_copy_node(cur_node.into(), (*ctxt).my_doc, 1)
                        .map(|nw| XmlNodePtr::try_from(nw).unwrap());
                    if let Some(mut nw) = nw {
                        if nw._private.is_null() {
                            nw._private = cur_node._private;
                        }
                        if first_child.is_none() {
                            first_child = Some(cur_node);
                        }
                        (*ent).add_child(nw.into());
                    }
                    context_node.add_child(cur_node.into());
                    if Some(cur_node) == last {
                        break;
                    }
                    cur = next;
                }
                if ent.owner == 0 {
                    ent.owner = 1;
                }
            } else {
                // the name change is to avoid coalescing of the
                // node with a possible previous text one which
                // would make (*ent).children a dangling pointer
                let nbktext: *const XmlChar =
                    xml_dict_lookup((*ctxt).dict, c"nbktext".as_ptr() as _, -1);
                if matches!(
                    (*ent.children.load(Ordering::Relaxed)).element_type(),
                    XmlElementType::XmlTextNode
                ) {
                    (*ent.children.load(Ordering::Relaxed)).name = nbktext;
                }
                if ent.last.load(Ordering::Relaxed) != ent.children.load(Ordering::Relaxed)
                    && matches!(
                        (*ent.last.load(Ordering::Relaxed)).element_type(),
                        XmlElementType::XmlTextNode
                    )
                {
                    (*ent.last.load(Ordering::Relaxed)).name = nbktext;
                }
                context_node.add_child_list(
                    XmlGenericNodePtr::from_raw(ent.children.load(Ordering::Relaxed)).unwrap(),
                );
            }

            // This is to avoid a nasty side effect, see characters() in SAX.c
            (*ctxt).nodemem = 0;
            (*ctxt).nodelen = 0;
        }
    }
}

/// Parse a parameter entity reference. Always consumes '%'.
///
/// The entity content is handled directly by pushing it's content as a new input stream.
///
/// `[69] PEReference ::= '%' Name ';'`
///
/// `[ WFC: No Recursion ]`  
/// A parsed entity must not contain a recursive
/// reference to itself, either directly or indirectly.
///
/// `[ WFC: Entity Declared ]`  
/// In a document without any DTD, a document with only an internal DTD
/// subset which contains no parameter entity references, or a document
/// with "standalone='yes'", ...  ... The declaration of a parameter
/// entity must precede any reference to it...
///
/// `[ VC: Entity Declared ]`  
/// In a document with an external subset or external parameter entities
/// with "standalone='no'", ...  ... The declaration of a parameter entity
/// must precede any reference to it...
///
/// `[ WFC: In DTD ]`  
/// Parameter-entity references may only appear in the DTD.
/// NOTE: misleading but this is handled.
#[doc(alias = "xmlParsePEReference")]
pub(crate) unsafe fn xml_parse_pe_reference(ctxt: XmlParserCtxtPtr) {
    let input: XmlParserInputPtr;

    if (*ctxt).current_byte() != b'%' {
        return;
    }
    (*ctxt).skip_char();
    let Some(name) = parse_name(&mut *ctxt) else {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrPERefNoName,
            "PEReference: no name\n",
        );
        return;
    };
    if get_parser_debug_entities() != 0 {
        generic_error!("PEReference: {}\n", name);
    }
    if (*ctxt).current_byte() != b';' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrPERefSemicolMissing, None);
        return;
    }

    (*ctxt).skip_char();

    // Request the entity from SAX
    let entity = (*ctxt)
        .sax
        .as_deref_mut()
        .and_then(|sax| sax.get_parameter_entity)
        .and_then(|get_parameter_entity| get_parameter_entity((*ctxt).user_data.clone(), &name));
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }
    if let Some(mut entity) = entity {
        // Internal checking in case the entity quest barfed
        if !matches!(
            entity.etype,
            XmlEntityType::XmlInternalParameterEntity | XmlEntityType::XmlExternalParameterEntity
        ) {
            xml_warning_msg!(
                ctxt,
                XmlParserErrors::XmlWarUndeclaredEntity,
                "Internal: %{}; is not a parameter entity\n",
                name
            );
        } else {
            let mut start: [XmlChar; 4] = [0; 4];
            let mut parent_consumed: u64;

            if matches!(entity.etype, XmlEntityType::XmlExternalParameterEntity)
                && (*ctxt).options & XmlParserOption::XmlParseNoEnt as i32 == 0
                && (*ctxt).options & XmlParserOption::XmlParseDTDValid as i32 == 0
                && (*ctxt).options & XmlParserOption::XmlParseDTDLoad as i32 == 0
                && (*ctxt).options & XmlParserOption::XmlParseDTDAttr as i32 == 0
                && (*ctxt).replace_entities == 0
                && (*ctxt).validate == 0
            {
                return;
            }

            if entity.flags & XML_ENT_EXPANDING as i32 != 0 {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, None);
                (*ctxt).halt();
                return;
            }

            // Must be computed from old input before pushing new input.
            parent_consumed = (*(*ctxt).input).parent_consumed;
            let old_ent = (*(*ctxt).input).entity;
            if old_ent.map_or(true, |old_ent| {
                matches!(old_ent.etype, XmlEntityType::XmlExternalParameterEntity)
                    && old_ent.flags & XML_ENT_PARSED as i32 == 0
            }) {
                parent_consumed = parent_consumed.saturating_add((*(*ctxt).input).consumed);
                parent_consumed =
                    parent_consumed.saturating_add((*(*ctxt).input).offset_from_base() as u64);
            }

            input = xml_new_entity_input_stream(ctxt, entity);
            if (*ctxt).push_input(input) < 0 {
                xml_free_input_stream(input);
                return;
            }

            entity.flags |= XML_ENT_EXPANDING as i32;

            (*input).parent_consumed = parent_consumed;

            if matches!(entity.etype, XmlEntityType::XmlExternalParameterEntity) {
                // Get the 4 first bytes and decode the charset
                // if enc != XML_CHAR_ENCODING_NONE
                // plug some encoding conversion routines.
                // Note that, since we may have some non-UTF8
                // encoding (like UTF16, bug 135229), the 'length'
                // is not known, but we can calculate based upon
                // the amount of data in the buffer.
                (*ctxt).grow();
                if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    return;
                }
                if (*(*ctxt).input).remainder_len() >= 4 {
                    start[0] = (*ctxt).current_byte();
                    start[1] = NXT!(ctxt, 1);
                    start[2] = NXT!(ctxt, 2);
                    start[3] = NXT!(ctxt, 3);
                    let enc = detect_encoding(&start);
                    if !matches!(enc, XmlCharEncoding::None) {
                        (*ctxt).switch_encoding(enc);
                    }
                }

                if (*ctxt).content_bytes().starts_with(b"<?xml")
                    && xml_is_blank_char(NXT!(ctxt, 5) as u32)
                {
                    parse_text_decl(&mut *ctxt);
                }
            }
        }
    } else {
        // [ WFC: Entity Declared ]
        // In a document without any DTD, a document with only an
        // internal DTD subset which contains no parameter entity
        // references, or a document with "standalone='yes'", ...
        // ... The declaration of a parameter entity must precede
        // any reference to it...
        if (*ctxt).standalone == 1 || ((*ctxt).has_external_subset == 0 && (*ctxt).has_perefs == 0)
        {
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrUndeclaredEntity,
                "PEReference: %{}; not found\n",
                name
            );
        } else {
            // [ VC: Entity Declared ]
            // In a document with an external subset or external
            // parameter entities with "standalone='no'", ...
            // ... The declaration of a parameter entity must
            // precede any reference to it...
            if (*ctxt).validate != 0 && (*ctxt).vctxt.error.is_some() {
                xml_validity_error!(
                    ctxt,
                    XmlParserErrors::XmlWarUndeclaredEntity,
                    "PEReference: %{}; not found\n",
                    name
                );
            } else {
                xml_warning_msg!(
                    ctxt,
                    XmlParserErrors::XmlWarUndeclaredEntity,
                    "PEReference: %{}; not found\n",
                    name
                );
            }
            (*ctxt).valid = 0;
        }
    }
    (*ctxt).has_perefs = 1;
}

/// Parse a DOCTYPE declaration
///
/// `[28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>'`
///
/// `[ VC: Root Element Type ]`  
/// The Name in the document type declaration must match the element type of the root element.
#[doc(alias = "xmlParseDocTypeDecl")]
pub(crate) unsafe fn xml_parse_doc_type_decl(ctxt: XmlParserCtxtPtr) {
    let mut external_id: *mut XmlChar = null_mut();

    // We know that '<!DOCTYPE' has been detected.
    (*ctxt).advance(9);

    (*ctxt).skip_blanks();

    // Parse the DOCTYPE name.
    let name = parse_name(&mut *ctxt);
    if name.is_none() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            "xmlParseDocTypeDecl : no DOCTYPE name !\n",
        );
    }
    (*ctxt).int_sub_name = name.as_deref().map(|n| n.to_owned());
    (*ctxt).skip_blanks();

    // Check for SystemID and ExternalID
    let uri: *mut XmlChar = xml_parse_external_id(ctxt, addr_of_mut!(external_id), 1);

    if !uri.is_null() || !external_id.is_null() {
        (*ctxt).has_external_subset = 1;
    }
    (*ctxt).ext_sub_uri = (!uri.is_null()).then(|| {
        CStr::from_ptr(uri as *const i8)
            .to_string_lossy()
            .into_owned()
    });
    (*ctxt).ext_sub_system = (!external_id.is_null()).then(|| {
        CStr::from_ptr(external_id as *const i8)
            .to_string_lossy()
            .into_owned()
    });
    xml_free(uri as _);
    xml_free(external_id as _);

    (*ctxt).skip_blanks();

    // Create and update the internal subset.
    if (*ctxt).disable_sax == 0 {
        if let Some(internal_subset) = (*ctxt)
            .sax
            .as_deref_mut()
            .and_then(|sax| sax.internal_subset)
        {
            internal_subset(
                (*ctxt).user_data.clone(),
                name.as_deref(),
                (*ctxt).ext_sub_system.as_deref(),
                (*ctxt).ext_sub_uri.as_deref(),
            );
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }

    // Is there any internal subset declarations ?
    // they are handled separately in xmlParseInternalSubset()
    if (*ctxt).current_byte() == b'[' {
        return;
    }

    // We should be at the end of the DOCTYPE declaration.
    if (*ctxt).current_byte() != b'>' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDoctypeNotFinished, None);
    }
    (*ctxt).skip_char();
}

/// Parse an attribute
///
/// `[41] Attribute ::= Name Eq AttValue`
///
/// `[ WFC: No External Entity References ]`  
/// Attribute values cannot contain direct or indirect entity references
/// to external entities.
///
/// `[ WFC: No < in Attribute Values ]`  
/// The replacement text of any entity referred to directly or indirectly in
/// an attribute value (other than "&lt;") must not contain a <.
///
/// `[ VC: Attribute Value Type ]`  
/// The attribute must have been declared; the value must be of the type declared for it.
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
#[doc(alias = "xmlParseAttribute")]
#[cfg(feature = "sax1")]
pub(crate) unsafe fn xml_parse_attribute(
    ctxt: XmlParserCtxtPtr,
    value: *mut *mut XmlChar,
) -> *const XmlChar {
    let val: *mut XmlChar;

    *value = null_mut();
    (*ctxt).grow();
    let name: *const XmlChar = xml_parse_name(ctxt);
    if name.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            "error parsing attribute name\n",
        );
        return null_mut();
    }

    // read the value
    (*ctxt).skip_blanks();
    if (*ctxt).current_byte() == b'=' {
        (*ctxt).skip_char();
        (*ctxt).skip_blanks();
        val = xml_parse_att_value(ctxt);
        (*ctxt).instate = XmlParserInputState::XmlParserContent;
    } else {
        let n = CStr::from_ptr(name as *const i8).to_string_lossy();
        xml_fatal_err_msg_str!(
            ctxt,
            XmlParserErrors::XmlErrAttributeWithoutValue,
            "Specification mandates value for attribute {}\n",
            n
        );
        return name;
    }

    // Check that xml:lang conforms to the specification
    // No more registered as an error, just generate a warning now
    // since this was deprecated in XML second edition
    if (*ctxt).pedantic != 0
        && xml_str_equal(name, c"xml:lang".as_ptr() as _)
        && xml_check_language_id(val) == 0
    {
        let val = CStr::from_ptr(val as *const i8).to_string_lossy();
        xml_warning_msg!(
            ctxt,
            XmlParserErrors::XmlWarLangValue,
            "Malformed value for xml:lang : {}\n",
            val
        );
    }

    // Check that xml:space conforms to the specification
    if xml_str_equal(name, c"xml:space".as_ptr() as _) {
        if xml_str_equal(val, c"default".as_ptr() as _) {
            *(*ctxt).space_mut() = 0;
        } else if xml_str_equal(val, c"preserve".as_ptr() as _) {
            *(*ctxt).space_mut() = 1;
        } else {
            let val = CStr::from_ptr(val as *const i8).to_string_lossy();
            xml_warning_msg!(
                ctxt,
                XmlParserErrors::XmlWarSpaceValue,
                "Invalid value \"{}\" for xml:space : \"default\" or \"preserve\" expected\n",
                val
            );
        }
    }

    *value = val;
    name
}

/// Parse a start tag. Always consumes '<'.
///
/// `[40] STag ::= '<' Name (S Attribute)* S? '>'`
///
/// `[ WFC: Unique Att Spec ]`  
/// No attribute name may appear more than once in the same start-tag or empty-element tag.
///
/// `[44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'`
///
/// `[ WFC: Unique Att Spec ]`  
/// No attribute name may appear more than once in the same start-tag or empty-element tag.
///
/// With namespace:
///
/// `[NS 8] STag ::= '<' QName (S Attribute)* S? '>'`
///
/// `[NS 10] EmptyElement ::= '<' QName (S Attribute)* S? '/>'`
///
/// Returns the element name parsed
#[doc(alias = "xmlParseStartTag")]
#[cfg(feature = "sax1")]
pub(crate) unsafe fn xml_parse_start_tag(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    use crate::parser::xml_err_attribute_dup;

    let mut attname: *const XmlChar;
    let mut attvalue: *mut XmlChar = null_mut();

    if (*ctxt).current_byte() != b'<' {
        return null_mut();
    }
    (*ctxt).advance(1);

    let name: *const XmlChar = xml_parse_name(ctxt);
    if name.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            "xmlParseStartTag: invalid element name\n",
        );
        return null_mut();
    }

    // Now parse the attributes, it ends up with the ending
    //
    // (S Attribute)* S?
    (*ctxt).skip_blanks();
    (*ctxt).grow();

    let mut atts: Vec<(String, Option<String>)> = vec![];
    while (*ctxt).current_byte() != b'>'
        && ((*ctxt).current_byte() != b'/' || NXT!(ctxt, 1) != b'>')
        && xml_is_char((*ctxt).current_byte() as u32)
        && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        attname = xml_parse_attribute(ctxt, addr_of_mut!(attvalue));
        if attname.is_null() {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                "xmlParseStartTag: problem parsing attributes\n",
            );
            break;
        }

        'failed: {
            let attname = CStr::from_ptr(attname as *const i8)
                .to_string_lossy()
                .into_owned();
            if !attvalue.is_null() {
                // [ WFC: Unique Att Spec ]
                // No attribute name may appear more than once in the same
                // start-tag or empty-element tag.
                for (att, _) in &atts {
                    if att.as_str() == attname {
                        xml_err_attribute_dup(ctxt, None, &attname);
                        xml_free(attvalue as _);
                        break 'failed;
                    }
                }
                // Add the pair to atts
                atts.push((
                    attname,
                    Some(
                        CStr::from_ptr(attvalue as *const i8)
                            .to_string_lossy()
                            .into_owned(),
                    ),
                ));
                xml_free(attvalue as _);
            }
        }

        // failed:

        (*ctxt).grow();
        if (*ctxt).current_byte() == b'>'
            || ((*ctxt).current_byte() == b'/' && NXT!(ctxt, 1) == b'>')
        {
            break;
        }
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "attributes construct error\n",
            );
        }
        (*ctxt).shrink();
        (*ctxt).grow();
    }

    // SAX: Start of Element !
    if (*ctxt).disable_sax == 0 {
        if let Some(start_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element) {
            let name = CStr::from_ptr(name as *const i8).to_string_lossy();
            if !atts.is_empty() {
                start_element((*ctxt).user_data.clone(), &name, atts.as_slice());
            } else {
                start_element((*ctxt).user_data.clone(), &name, &[]);
            }
        }
    }

    name
}

/// Parse an end of tag
///
/// `[42] ETag ::= '</' Name S? '>'`
///
/// With namespace
///
/// `[NS 9] ETag ::= '</' QName S? '>'`
#[doc(alias = "xmlParseEndTag")]
#[cfg(feature = "sax1")]
pub(crate) unsafe fn xml_parse_end_tag(ctxt: XmlParserCtxtPtr) {
    xml_parse_end_tag1(ctxt, 0);
}

/// Parse the start of an XML element. Returns -1 in case of error, 0 if an
/// opening tag was parsed, 1 if an empty element was parsed.
///
/// Always consumes '<'.
#[doc(alias = "xmlParseElementStart")]
pub(crate) unsafe fn xml_parse_element_start(ctxt: XmlParserCtxtPtr) -> i32 {
    let name: *const XmlChar;
    let mut prefix: *const XmlChar = null_mut();
    let mut tlen: i32 = 0;
    let ns_nr = (*ctxt).ns_tab.len();

    if (*ctxt).name_tab.len() as u32 > XML_PARSER_MAX_DEPTH
        && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0
    {
        xml_fatal_err_msg_int!(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            format!("Excessive depth in document: {XML_PARSER_MAX_DEPTH} use xmlParserOption::XML_PARSE_HUGE option\n").as_str(),
            XML_PARSER_MAX_DEPTH as i32
        );
        (*ctxt).halt();
        return -1;
    }

    // Capture start position
    let (begin_pos, begin_line) = if (*ctxt).record_info != 0 {
        (
            (*(*ctxt).input).consumed + (*(*ctxt).input).offset_from_base() as u64,
            (*(*ctxt).input).line as u64,
        )
    } else {
        (0, 0)
    };

    if (*ctxt).space_tab.is_empty() || (*ctxt).space() == -2 {
        (*ctxt).space_push(-1);
    } else {
        (*ctxt).space_push((*ctxt).space());
    }

    let line: i32 = (*(*ctxt).input).line;
    let mut uri = None;
    #[cfg(feature = "sax1")]
    {
        if (*ctxt).sax2 != 0 {
            name = xml_parse_start_tag2(ctxt, addr_of_mut!(prefix), &mut uri, addr_of_mut!(tlen));
        } else {
            name = xml_parse_start_tag(ctxt);
        }
    }
    #[cfg(not(feature = "sax1"))]
    {
        name = xml_parse_start_tag2(
            ctxt,
            addr_of_mut!(prefix),
            addr_of_mut!(uri),
            addr_of_mut!(tlen),
        );
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }
    if name.is_null() {
        (*ctxt).space_pop();
        return -1;
    }
    let curi = uri.as_deref().map(|uri| CString::new(uri).unwrap());
    (*ctxt).name_ns_push(
        name,
        prefix,
        curi.as_deref()
            .map_or(null(), |uri| uri.as_ptr() as *const u8),
        line,
        (*ctxt).ns_tab.len() as i32 - ns_nr as i32,
    );
    let cur = (*ctxt).node;

    // [ VC: Root Element Type ]
    // The Name in the document type declaration must match the element type of the root element.
    #[cfg(feature = "libxml_valid")]
    if (*ctxt).validate != 0 && (*ctxt).well_formed != 0 {
        if let Some(context_node) = (*ctxt).node {
            if let Some(my_doc) = (*ctxt)
                .my_doc
                .filter(|doc| NodePtr::from_ptr(context_node.as_ptr()) == doc.children)
            {
                (*ctxt).valid &= xml_validate_root(addr_of_mut!((*ctxt).vctxt), my_doc);
            }
        }
    }

    // Check for an Empty Element.
    if (*ctxt).current_byte() == b'/' && NXT!(ctxt, 1) == b'>' {
        (*ctxt).advance(2);
        let name = CStr::from_ptr(name as *const i8).to_string_lossy();
        if (*ctxt).sax2 != 0 {
            if (*ctxt).disable_sax == 0 {
                if let Some(end_element_ns) = (*ctxt)
                    .sax
                    .as_deref_mut()
                    .and_then(|sax| sax.end_element_ns)
                {
                    end_element_ns(
                        (*ctxt).user_data.clone(),
                        &name,
                        (!prefix.is_null())
                            .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                            .as_deref(),
                        uri.as_deref(),
                    );
                }
            }
        } else {
            #[cfg(feature = "sax1")]
            if (*ctxt).disable_sax == 0 {
                if let Some(end_element) =
                    (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element)
                {
                    end_element((*ctxt).user_data.clone(), &name);
                }
            }
        }
        (*ctxt).name_pop();
        (*ctxt).space_pop();
        if ns_nr != (*ctxt).ns_tab.len() {
            (*ctxt).ns_pop((*ctxt).ns_tab.len() - ns_nr);
        }
        if let Some(cur) = cur {
            if (*ctxt).record_info != 0 {
                let node_info = XmlParserNodeInfo {
                    node: Some(cur),
                    begin_pos,
                    begin_line,
                    end_pos: (*(*ctxt).input).consumed + (*(*ctxt).input).offset_from_base() as u64,
                    end_line: (*(*ctxt).input).line as u64,
                };
                xml_parser_add_node_info(ctxt, Rc::new(RefCell::new(node_info)));
            }
        }
        return 1;
    }
    if (*ctxt).current_byte() == b'>' {
        (*ctxt).advance(1);
        if let Some(cur) = cur {
            if (*ctxt).record_info != 0 {
                let node_info = XmlParserNodeInfo {
                    node: Some(cur),
                    begin_pos,
                    begin_line,
                    end_pos: 0,
                    end_line: 0,
                };
                xml_parser_add_node_info(ctxt, Rc::new(RefCell::new(node_info)));
            }
        }
    } else {
        xml_fatal_err_msg_str_int_str!(
            ctxt,
            XmlParserErrors::XmlErrGtRequired,
            "Couldn't find end of Start Tag {} line {}\n",
            CStr::from_ptr(name as *const i8).to_string_lossy(),
            line
        );

        // end of parsing of this node.
        (*ctxt).node_pop();
        (*ctxt).name_pop();
        (*ctxt).space_pop();
        if ns_nr != (*ctxt).ns_tab.len() {
            (*ctxt).ns_pop((*ctxt).ns_tab.len() - ns_nr);
        }
        return -1;
    }

    0
}

/// Parse the end of an XML element. Always consumes '</'.
#[doc(alias = "xmlParseElementEnd")]
pub(crate) unsafe fn xml_parse_element_end(ctxt: XmlParserCtxtPtr) {
    let cur = (*ctxt).node;

    if (*ctxt).name_tab.is_empty() {
        if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'/' {
            (*ctxt).advance(2);
        }
        return;
    }

    // parse the end of tag: '</' should be here.
    if (*ctxt).sax2 != 0 {
        xml_parse_end_tag2(ctxt, &(*ctxt).push_tab[(*ctxt).name_tab.len() - 1]);
        (*ctxt).name_pop();
    } else {
        #[cfg(feature = "sax1")]
        {
            xml_parse_end_tag1(ctxt, 0);
        }
    }

    // Capture end position
    if let Some(cur) = cur {
        if (*ctxt).record_info != 0 {
            if let Some(node_info) = xml_parser_find_node_info(ctxt, cur) {
                node_info.borrow_mut().end_pos =
                    (*(*ctxt).input).consumed + (*(*ctxt).input).offset_from_base() as u64;
                node_info.borrow_mut().end_line = (*(*ctxt).input).line as _;
            }
        }
    }
}

/// Parse a content sequence. Stops at EOF or '</'. Leaves checking of unexpected EOF to the caller.
#[doc(alias = "xmlParseContentInternal")]
pub(crate) unsafe fn xml_parse_content_internal(ctxt: XmlParserCtxtPtr) {
    let name_nr = (*ctxt).name_tab.len();

    (*ctxt).grow();
    while (*ctxt).current_byte() != 0
        && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        let cur: *const XmlChar = (*(*ctxt).input).cur;

        // First case : a Processing Instruction.
        if *cur == b'<' && *cur.add(1) == b'?' {
            parse_pi(&mut *ctxt);
        }
        // Second case : a CDSection
        // 2.6.0 test was *cur not RAW
        else if (*ctxt).content_bytes().starts_with(b"<![CDATA[") {
            xml_parse_cdsect(ctxt);
        }
        // Third case :  a comment
        else if *cur == b'<'
            && NXT!(ctxt, 1) == b'!'
            && NXT!(ctxt, 2) == b'-'
            && NXT!(ctxt, 3) == b'-'
        {
            parse_comment(&mut *ctxt);
            (*ctxt).instate = XmlParserInputState::XmlParserContent;
        }
        // Fourth case :  a sub-element.
        else if *cur == b'<' {
            if NXT!(ctxt, 1) == b'/' {
                if (*ctxt).name_tab.len() <= name_nr {
                    break;
                }
                xml_parse_element_end(ctxt);
            } else {
                xml_parse_element_start(ctxt);
            }
        }
        // Fifth case : a reference. If if has not been resolved,
        //    parsing returns it's Name, create the node
        else if *cur == b'&' {
            xml_parse_reference(ctxt);
        }
        // Last case, text. Note that References are handled directly.
        else {
            xml_parse_char_data_internal(ctxt, 0);
        }

        (*ctxt).shrink();
        (*ctxt).grow();
    }
}

/// Parse a content sequence. Stops at EOF or '</'.
///
/// `[43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*`
#[doc(alias = "xmlParseContent")]
pub unsafe fn xml_parse_content(ctxt: XmlParserCtxtPtr) {
    let name_nr = (*ctxt).name_tab.len();

    xml_parse_content_internal(ctxt);

    if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        && (*ctxt).name_tab.len() > name_nr
    {
        let name: *const XmlChar = (*ctxt).name_tab[(*ctxt).name_tab.len() - 1];
        let line: i32 = (*ctxt).push_tab[(*ctxt).name_tab.len() - 1].line;
        xml_fatal_err_msg_str_int_str!(
            ctxt,
            XmlParserErrors::XmlErrTagNotFinished,
            "Premature end of data in tag {} line {}\n",
            CStr::from_ptr(name as *const i8).to_string_lossy(),
            line
        );
    }
}

/// Parse an XML Misc* optional field.
///
/// `[27] Misc ::= Comment | PI |  S`
#[doc(alias = "xmlParseMisc")]
pub(crate) unsafe fn xml_parse_misc(ctxt: XmlParserCtxtPtr) {
    #[allow(clippy::while_immutable_condition)]
    while !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        (*ctxt).skip_blanks();
        (*ctxt).grow();
        if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'?' {
            parse_pi(&mut *ctxt);
        } else if (*ctxt).content_bytes().starts_with(b"<!--") {
            parse_comment(&mut *ctxt);
        } else {
            break;
        }
    }
}

/// Parse Markup declarations from an external subset
///
/// `[30] extSubset ::= textDecl? extSubsetDecl`
///
/// `[31] extSubsetDecl ::= (markupdecl | conditionalSect | PEReference | S) *`
#[doc(alias = "xmlParseExternalSubset")]
pub unsafe fn xml_parse_external_subset(
    ctxt: XmlParserCtxtPtr,
    external_id: Option<&str>,
    system_id: Option<&str>,
) {
    (*ctxt).detect_sax2();
    (*ctxt).grow();

    if (*ctxt).encoding.is_none() && (*(*ctxt).input).remainder_len() >= 4 {
        let mut start: [XmlChar; 4] = [0; 4];

        start[0] = (*ctxt).current_byte();
        start[1] = NXT!(ctxt, 1);
        start[2] = NXT!(ctxt, 2);
        start[3] = NXT!(ctxt, 3);
        let enc = detect_encoding(&start);
        if !matches!(enc, XmlCharEncoding::None) {
            (*ctxt).switch_encoding(enc);
        }
    }

    if (*ctxt).content_bytes().starts_with(b"<?xml") {
        parse_text_decl(&mut *ctxt);
        if (*ctxt).err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
            // The XML REC instructs us to stop parsing right here
            (*ctxt).halt();
            return;
        }
    }
    let my_doc = if let Some(my_doc) = (*ctxt).my_doc {
        my_doc
    } else {
        (*ctxt).my_doc = xml_new_doc(Some("1.0"));
        let Some(mut my_doc) = (*ctxt).my_doc else {
            xml_err_memory(ctxt, Some("New Doc failed"));
            return;
        };
        my_doc.properties = XmlDocProperties::XmlDocInternal as i32;
        my_doc
    };
    if my_doc.int_subset.is_none() {
        xml_create_int_subset((*ctxt).my_doc, None, external_id, system_id);
    }

    (*ctxt).instate = XmlParserInputState::XmlParserDTD;
    (*ctxt).external = 1;
    (*ctxt).skip_blanks();
    #[allow(clippy::while_immutable_condition)]
    while !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        && (*ctxt).current_byte() != 0
    {
        (*ctxt).grow();
        if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'!' && NXT!(ctxt, 2) == b'[' {
            xml_parse_conditional_sections(ctxt);
        } else if (*ctxt).current_byte() == b'<' && (NXT!(ctxt, 1) == b'!' || NXT!(ctxt, 1) == b'?')
        {
            xml_parse_markup_decl(ctxt);
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtSubsetNotFinished, None);
            (*ctxt).halt();
            return;
        }
        (*ctxt).skip_blanks();
        (*ctxt).shrink();
    }

    if (*ctxt).current_byte() != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtSubsetNotFinished, None);
    }
}

/// If no entities need to be substituted.
const XML_SUBSTITUTE_NONE: usize = 0;
/// Whether general entities need to be substituted.
pub(crate) const XML_SUBSTITUTE_REF: usize = 1;
/// Whether parameter entities need to be substituted.
pub(crate) const XML_SUBSTITUTE_PEREF: usize = 2;
/// Both general and parameter entities need to be substituted.
const XML_SUBSTITUTE_BOTH: usize = 3;

/// Takes a entity string content and process to do the adequate substitutions.
///
/// `[67] Reference ::= EntityRef | CharRef`
///
/// `[69] PEReference ::= '%' Name ';'`
///
/// Returns A newly allocated string with the substitution done. The caller must deallocate it !
#[doc(alias = "xmlStringDecodeEntities")]
pub(crate) unsafe fn xml_string_decode_entities(
    ctxt: XmlParserCtxtPtr,
    str: *const XmlChar,
    what: i32,
    end: XmlChar,
    end2: XmlChar,
    end3: XmlChar,
) -> *mut XmlChar {
    if ctxt.is_null() || str.is_null() {
        return null_mut();
    }
    xml_string_decode_entities_int(ctxt, str, xml_strlen(str), what, end, end2, end3, 0)
}

/// Takes a entity string content and process to do the adequate substitutions.
///
/// `[67] Reference ::= EntityRef | CharRef`
///
/// `[69] PEReference ::= '%' Name ';'`
///
/// Returns A newly allocated string with the substitution done. The caller must deallocate it !
#[doc(alias = "xmlStringLenDecodeEntities")]
pub(crate) unsafe fn xml_string_len_decode_entities(
    ctxt: XmlParserCtxtPtr,
    str: *const XmlChar,
    len: i32,
    what: i32,
    end: XmlChar,
    end2: XmlChar,
    end3: XmlChar,
) -> *mut XmlChar {
    if ctxt.is_null() || str.is_null() || len < 0 {
        return null_mut();
    }
    xml_string_decode_entities_int(ctxt, str, len, what, end, end2, end3, 0)
}

/// The current c_char value, if using UTF-8 this may actually span multiple
/// bytes in the input buffer.
///
/// Returns the current c_char value and its length
#[doc(alias = "xmlStringCurrentChar")]
pub(crate) unsafe fn xml_string_current_char(
    ctxt: XmlParserCtxtPtr,
    cur: *const XmlChar,
    len: *mut i32,
) -> i32 {
    if len.is_null() || cur.is_null() {
        return 0;
    }
    'encoding_error: {
        if ctxt.is_null() || (*ctxt).charset == XmlCharEncoding::UTF8 {
            // We are supposed to handle UTF8, check it's valid
            // From rfc2044: encoding of the Unicode values on UTF-8:
            //
            // UCS-4 range (hex.)           UTF-8 octet sequence (binary)
            // 0000 0000-0000 007F   0xxxxxxx
            // 0000 0080-0000 07FF   110xxxxx 10xxxxxx
            // 0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
            //
            // Check for the 0x110000 limit too

            let mut val: u32;
            let c: u8 = *cur;
            if c & 0x80 != 0 {
                if *cur.add(1) & 0xc0 != 0x80 {
                    break 'encoding_error;
                }
                if c & 0xe0 == 0xe0 {
                    if *cur.add(2) & 0xc0 != 0x80 {
                        break 'encoding_error;
                    }
                    if c & 0xf0 == 0xf0 {
                        if c & 0xf8 != 0xf0 || *cur.add(3) & 0xc0 != 0x80 {
                            break 'encoding_error;
                        }
                        // 4-byte code
                        *len = 4;
                        val = (*cur.add(0) as u32 & 0x7) << 18;
                        val |= (*cur.add(1) as u32 & 0x3f) << 12;
                        val |= (*cur.add(2) as u32 & 0x3f) << 6;
                        val |= *cur.add(3) as u32 & 0x3f;
                    } else {
                        // 3-byte code
                        *len = 3;
                        val = (*cur.add(0) as u32 & 0xf) << 12;
                        val |= (*cur.add(1) as u32 & 0x3f) << 6;
                        val |= *cur.add(2) as u32 & 0x3f;
                    }
                } else {
                    // 2-byte code
                    *len = 2;
                    val = (*cur.add(0) as u32 & 0x1f) << 6;
                    val |= *cur.add(1) as u32 & 0x3f;
                }
                if !xml_is_char(val) {
                    xml_err_encoding_int!(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidChar,
                        "Char 0x{:X} out of allowed range\n",
                        val as i32
                    );
                }
                return val as _;
            } else {
                // 1-byte code
                *len = 1;
                return *cur as _;
            }
        }
        // Assume it's a fixed length encoding (1) with
        // a compatible encoding for the ASCII set, since
        // XML constructs only use < 128 chars
        *len = 1;
        return *cur as _;
    }
    // encoding_error:

    // An encoding problem may arise from a truncated input buffer
    // splitting a character in the middle. In that case do not raise
    // an error but return 0 to indicate an end of stream problem
    if ctxt.is_null() || (*ctxt).input.is_null() || (*(*ctxt).input).remainder_len() < 4 {
        *len = 0;
        return 0;
    }
    // If we detect an UTF8 error that probably mean that the
    // input encoding didn't get properly advertised in the
    // declaration header. Report the error and switch the encoding
    // to ISO-Latin-1 (if you don't like this policy, just declare the encoding !)
    {
        let buffer = format!(
            "Bytes: 0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}\n",
            *(*(*ctxt).input).cur.add(0),
            *(*(*ctxt).input).cur.add(1),
            *(*(*ctxt).input).cur.add(2),
            *(*(*ctxt).input).cur.add(3),
        );
        __xml_err_encoding!(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            "Input is not proper UTF-8, indicate encoding !\n{}",
            buffer
        );
    }
    *len = 1;
    *cur as _
}

/// ```text
/// [69] PEReference ::= '%' Name ';'
///
/// [ WFC: No Recursion ]
/// A parsed entity must not contain a recursive reference to itself, either directly or indirectly.
///
/// [ WFC: Entity Declared ]
/// In a document without any DTD, a document with only an internal DTD
/// subset which contains no parameter entity references, or a document
/// with "standalone='yes'", ...  ... The declaration of a parameter
/// entity must precede any reference to it...
///
/// [ VC: Entity Declared ]
/// In a document with an external subset or external parameter entities
/// with "standalone='no'", ...  ... The declaration of a parameter entity
/// must precede any reference to it...
///
/// [ WFC: In DTD ]
/// Parameter-entity references may only appear in the DTD.
/// NOTE: misleading but this is handled.
/// ```
///
/// A PEReference may have been detected in the current input stream
/// the handling is done accordingly to
///      http://www.w3.org/TR/REC-xml#entproc
/// i.e.
///   - Included in literal in entity values
///   - Included as Parameter Entity reference within DTDs
#[doc(alias = "xmlParserHandlePEReference")]
pub(crate) unsafe fn xml_parser_handle_pereference(ctxt: XmlParserCtxtPtr) {
    match (*ctxt).instate {
        XmlParserInputState::XmlParserCDATASection => {
            return;
        }
        XmlParserInputState::XmlParserComment => {
            return;
        }
        XmlParserInputState::XmlParserStartTag => {
            return;
        }
        XmlParserInputState::XmlParserEndTag => {
            return;
        }
        XmlParserInputState::XmlParserEOF => {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrPERefAtEOF, None);
            return;
        }
        XmlParserInputState::XmlParserProlog
        | XmlParserInputState::XmlParserStart
        | XmlParserInputState::XmlParserMisc => {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrPERefInProlog, None);
            return;
        }
        XmlParserInputState::XmlParserEntityDecl
        | XmlParserInputState::XmlParserContent
        | XmlParserInputState::XmlParserAttributeValue
        | XmlParserInputState::XmlParserPI
        | XmlParserInputState::XmlParserSystemLiteral
        | XmlParserInputState::XmlParserPublicLiteral => {
            // we just ignore it there
            return;
        }
        XmlParserInputState::XmlParserEpilog => {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrPERefInEpilog, None);
            return;
        }
        XmlParserInputState::XmlParserEntityValue => {
            // NOTE: in the case of entity values, we don't do the
            //       substitution here since we need the literal
            //       entity value to be able to save the internal
            //       subset of the document.
            //       This will be handled by xmlStringDecodeEntities
            return;
        }
        XmlParserInputState::XmlParserDTD => {
            // [WFC: Well-Formedness Constraint: PEs in Internal Subset]
            // In the internal DTD subset, parameter-entity references
            // can occur only where markup declarations can occur, not
            // within markup declarations.
            // In that case this is handled in xmlParseMarkupDecl
            if (*ctxt).external == 0 && (*ctxt).input_tab.len() == 1 {
                return;
            }
            if xml_is_blank_char(NXT!(ctxt, 1) as u32) || NXT!(ctxt, 1) == 0 {
                return;
            }
        }
        XmlParserInputState::XmlParserIgnore => {
            return;
        }
    }

    xml_parse_pe_reference(ctxt);
}

/// Checks that the value conforms to the LanguageID production:
///
/// # Note
/// This is somewhat deprecated, those productions were removed from the XML Second edition.
///
/// ```text
/// [33] LanguageID ::= Langcode ('-' Subcode)*
/// [34] Langcode ::= ISO639Code |  IanaCode |  UserCode
/// [35] ISO639Code ::= ([a-z] | [A-Z]) ([a-z] | [A-Z])
/// [36] IanaCode ::= ('i' | 'I') '-' ([a-z] | [A-Z])+
/// [37] UserCode ::= ('x' | 'X') '-' ([a-z] | [A-Z])+
/// [38] Subcode ::= ([a-z] | [A-Z])+
/// ```
///
/// The current REC reference the successors of RFC 1766, currently 5646
///
/// http://www.rfc-editor.org/rfc/rfc5646.txt
/// ```text
/// langtag       = language
///                 ["-" script]
///                 ["-" region]
///                 *("-" variant)
///                 *("-" extension)
///                 ["-" privateuse]
/// language      = 2*3ALPHA            ; shortest ISO 639 code
///                 ["-" extlang]       ; sometimes followed by
///                                     ; extended language subtags
///               / 4ALPHA              ; or reserved for future use
///               / 5*8ALPHA            ; or registered language subtag
///
/// extlang       = 3ALPHA              ; selected ISO 639 codes
///                 *2("-" 3ALPHA)      ; permanently reserved
///
/// script        = 4ALPHA              ; ISO 15924 code
///
/// region        = 2ALPHA              ; ISO 3166-1 code
///               / 3DIGIT              ; UN M.49 code
///
/// variant       = 5*8alphanum         ; registered variants
///               / (DIGIT 3alphanum)
///
/// extension     = singleton 1*("-" (2*8alphanum))
///                                     ; Single alphanumerics
///                                     ; "x" reserved for private use
/// singleton     = DIGIT               ; 0 - 9
///               / %x41-57             ; A - W
///               / %x59-5A             ; Y - Z
///               / %x61-77             ; a - w
///               / %x79-7A             ; y - z
/// ```
///
/// it sounds right to still allow Irregular i-xxx IANA and user codes too
/// The parser below doesn't try to cope with extension or privateuse
/// that could be added but that's not interoperable anyway
///
/// Returns 1 if correct 0 otherwise
#[doc(alias = "xmlCheckLanguageID")]
pub(crate) unsafe fn xml_check_language_id(lang: *const XmlChar) -> i32 {
    let mut cur: *const XmlChar = lang;
    let mut nxt: *const XmlChar;

    if cur.is_null() {
        return 0;
    }
    if (*cur.add(0) == b'i' && *cur.add(1) == b'-')
        || (*cur.add(0) == b'I' && *cur.add(1) == b'-')
        || (*cur.add(0) == b'x' && *cur.add(1) == b'-')
        || (*cur.add(0) == b'X' && *cur.add(1) == b'-')
    {
        // Still allow IANA code and user code which were coming
        // from the previous version of the XML-1.0 specification
        // it's deprecated but we should not fail
        cur = cur.add(2);
        while (*cur.add(0) >= b'A' && *cur.add(0) <= b'Z')
            || (*cur.add(0) >= b'a' && *cur.add(0) <= b'z')
        {
            cur = cur.add(1);
        }
        return (*cur.add(0) == 0) as i32;
    }
    nxt = cur;
    while (*nxt.add(0) >= b'A' && *nxt.add(0) <= b'Z')
        || (*nxt.add(0) >= b'a' && *nxt.add(0) <= b'z')
    {
        nxt = nxt.add(1);
    }
    if nxt.offset_from(cur) >= 4 {
        // Reserved
        if nxt.offset_from(cur) > 8 || *nxt.add(0) != 0 {
            return 0;
        }
        return 1;
    }
    if nxt.offset_from(cur) < 2 {
        return 0;
    }
    // we got an ISO 639 code
    if *nxt.add(0) == 0 {
        return 1;
    }
    if *nxt.add(0) != b'-' {
        return 0;
    }

    nxt = nxt.add(1);
    cur = nxt;
    'region_m49: {
        // now we can have extlang or script or region or variant
        if *nxt.add(0) >= b'0' && *nxt.add(0) <= b'9' {
            break 'region_m49;
        }
        while (*nxt.add(0) >= b'A' && *nxt.add(0) <= b'Z')
            || (*nxt.add(0) >= b'a' && *nxt.add(0) <= b'z')
        {
            nxt = nxt.add(1);
        }
        'variant: {
            'region: {
                'script: {
                    if nxt.offset_from(cur) == 4 {
                        break 'script;
                    }
                    if nxt.offset_from(cur) == 2 {
                        break 'region;
                    }
                    if nxt.offset_from(cur) >= 5 && nxt.offset_from(cur) <= 8 {
                        break 'variant;
                    }
                    if nxt.offset_from(cur) != 3 {
                        return 0;
                    }
                    // we parsed an extlang
                    if *nxt.add(0) == 0 {
                        return 1;
                    }
                    if *nxt.add(0) != b'-' {
                        return 0;
                    }
                    nxt = nxt.add(1);
                    cur = nxt;

                    // now we can have script or region or variant
                    if *nxt.add(0) >= b'0' && *nxt.add(0) <= b'9' {
                        break 'region_m49;
                    }

                    while (*nxt.add(0) >= b'A' && *nxt.add(0) <= b'Z')
                        || (*nxt.add(0) >= b'a' && *nxt.add(0) <= b'z')
                    {
                        nxt = nxt.add(1);
                    }
                    if nxt.offset_from(cur) == 2 {
                        break 'region;
                    }
                    if nxt.offset_from(cur) >= 5 && nxt.offset_from(cur) <= 8 {
                        break 'variant;
                    }
                    if nxt.offset_from(cur) != 4 {
                        return 0;
                    }
                    // we parsed a script
                }
                if *nxt.add(0) == 0 {
                    return 1;
                }
                if *nxt.add(0) != b'-' {
                    return 0;
                }

                nxt = nxt.add(1);
                cur = nxt;
                // now we can have region or variant
                if *nxt.add(0) >= b'0' && *nxt.add(0) <= b'9' {
                    break 'region_m49;
                }
                while (*nxt.add(0) >= b'A' && *nxt.add(0) <= b'Z')
                    || (*nxt.add(0) >= b'a' && *nxt.add(0) <= b'z')
                {
                    nxt = nxt.add(1);
                }

                if nxt.offset_from(cur) >= 5 && nxt.offset_from(cur) <= 8 {
                    break 'variant;
                }
                if nxt.offset_from(cur) != 2 {
                    return 0;
                }
                // we parsed a region
            }
            //  region:
            if *nxt.add(0) == 0 {
                return 1;
            }
            if *nxt.add(0) != b'-' {
                return 0;
            }

            nxt = nxt.add(1);
            cur = nxt;
            // now we can just have a variant
            while (*nxt.add(0) >= b'A' && *nxt.add(0) <= b'Z')
                || (*nxt.add(0) >= b'a' && *nxt.add(0) <= b'z')
            {
                nxt = nxt.add(1);
            }

            if nxt.offset_from(cur) < 5 || nxt.offset_from(cur) > 8 {
                return 0;
            }
        }

        // we parsed a variant
        //  variant:
        if *nxt.add(0) == 0 {
            return 1;
        }
        if *nxt.add(0) != b'-' {
            return 0;
        }
        // extensions and private use subtags not checked
        return 1;
    }

    //  region_m49:
    if (*nxt.add(1) >= b'0' && *nxt.add(1) <= b'9') && (*nxt.add(2) >= b'0' && *nxt.add(2) <= b'9')
    {
        nxt = nxt.add(3);
        // goto region;
        if *nxt.add(0) == 0 {
            return 1;
        }
        if *nxt.add(0) != b'-' {
            return 0;
        }

        nxt = nxt.add(1);
        cur = nxt;
        // now we can just have a variant
        while (*nxt.add(0) >= b'A' && *nxt.add(0) <= b'Z')
            || (*nxt.add(0) >= b'a' && *nxt.add(0) <= b'z')
        {
            nxt = nxt.add(1);
        }

        if nxt.offset_from(cur) < 5 || nxt.offset_from(cur) > 8 {
            return 0;
        }

        // we parsed a variant
        //  variant:
        if *nxt.add(0) == 0 {
            return 1;
        }
        if *nxt.add(0) != b'-' {
            return 0;
        }
        // extensions and private use subtags not checked
        return 1;
    }
    0
}

/// Append the char value in the array
///
/// Returns the number of xmlChar written
#[doc(alias = "xmlCopyCharMultiByte")]
pub unsafe fn xml_copy_char_multi_byte(mut out: *mut XmlChar, val: i32) -> i32 {
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
            xml_err_encoding_int!(
                null_mut(),
                XmlParserErrors::XmlErrInvalidChar,
                "Internal error, xmlCopyCharMultiByte 0x{:X} out of bound\n",
                val
            );
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

/// Append the char value in the array
///
/// Returns the number of xmlChar written
#[doc(alias = "xmlCopyChar")]
pub unsafe fn xml_copy_char(_len: i32, out: *mut XmlChar, val: i32) -> i32 {
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

// we need to keep enough input to show errors in context
pub(crate) const LINE_LEN: usize = 80;

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_check_language_id() {
        unsafe {
            let mut leaks = 0;

            for n_lang in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let lang = gen_const_xml_char_ptr(n_lang, 0);

                let ret_val = xml_check_language_id(lang as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_lang, lang, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCheckLanguageID",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlCheckLanguageID()"
                    );
                    eprintln!(" {}", n_lang);
                }
            }
        }
    }

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

    #[test]
    fn test_xml_split_qname() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_prefix in 0..GEN_NB_XML_CHAR_PTR_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let prefix = gen_xml_char_ptr_ptr(n_prefix, 2);

                        let ret_val = xml_split_qname(ctxt, name, prefix);
                        desret_xml_char_ptr(ret_val);
                        des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_xml_char_ptr_ptr(n_prefix, prefix, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSplitQName",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlSplitQName()");
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_prefix);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_string_current_char() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_cur in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                        let cur = gen_const_xml_char_ptr(n_cur, 1);
                        let len = gen_int_ptr(n_len, 2);

                        let ret_val = xml_string_current_char(ctxt, cur, len);
                        desret_int(ret_val);
                        des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_const_xml_char_ptr(n_cur, cur, 1);
                        des_int_ptr(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlStringCurrentChar",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlStringCurrentChar()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_cur);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_string_decode_entities() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_what in 0..GEN_NB_INT {
                        for n_end in 0..GEN_NB_XML_CHAR {
                            for n_end2 in 0..GEN_NB_XML_CHAR {
                                for n_end3 in 0..GEN_NB_XML_CHAR {
                                    let mem_base = xml_mem_blocks();
                                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                                    let str = gen_const_xml_char_ptr(n_str, 1);
                                    let what = gen_int(n_what, 2);
                                    let end = gen_xml_char(n_end, 3);
                                    let end2 = gen_xml_char(n_end2, 4);
                                    let end3 = gen_xml_char(n_end3, 5);

                                    let ret_val = xml_string_decode_entities(
                                        ctxt, str, what, end, end2, end3,
                                    );
                                    desret_xml_char_ptr(ret_val);
                                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                                    des_const_xml_char_ptr(n_str, str, 1);
                                    des_int(n_what, what, 2);
                                    des_xml_char(n_end, end, 3);
                                    des_xml_char(n_end2, end2, 4);
                                    des_xml_char(n_end3, end3, 5);
                                    reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlStringDecodeEntities",
                                            xml_mem_blocks() - mem_base
                                        );
                                        assert!(
                                            leaks == 0,
                                            "{leaks} Leaks are found in xmlStringDecodeEntities()"
                                        );
                                        eprint!(" {}", n_ctxt);
                                        eprint!(" {}", n_str);
                                        eprint!(" {}", n_what);
                                        eprint!(" {}", n_end);
                                        eprint!(" {}", n_end2);
                                        eprintln!(" {}", n_end3);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_string_len_decode_entities() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        for n_what in 0..GEN_NB_INT {
                            for n_end in 0..GEN_NB_XML_CHAR {
                                for n_end2 in 0..GEN_NB_XML_CHAR {
                                    for n_end3 in 0..GEN_NB_XML_CHAR {
                                        let mem_base = xml_mem_blocks();
                                        let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                                        let str = gen_const_xml_char_ptr(n_str, 1);
                                        let mut len = gen_int(n_len, 2);
                                        let what = gen_int(n_what, 3);
                                        let end = gen_xml_char(n_end, 4);
                                        let end2 = gen_xml_char(n_end2, 5);
                                        let end3 = gen_xml_char(n_end3, 6);
                                        if !str.is_null() && len > xml_strlen(str) {
                                            len = 0;
                                        }

                                        let ret_val = xml_string_len_decode_entities(
                                            ctxt, str, len, what, end, end2, end3,
                                        );
                                        desret_xml_char_ptr(ret_val);
                                        des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                                        des_const_xml_char_ptr(n_str, str, 1);
                                        des_int(n_len, len, 2);
                                        des_int(n_what, what, 3);
                                        des_xml_char(n_end, end, 4);
                                        des_xml_char(n_end2, end2, 5);
                                        des_xml_char(n_end3, end3, 6);
                                        reset_last_error();
                                        if mem_base != xml_mem_blocks() {
                                            leaks += 1;
                                            eprint!(
                                                "Leak of {} blocks found in xmlStringLenDecodeEntities",
                                                xml_mem_blocks() - mem_base
                                            );
                                            assert!(leaks == 0, "{leaks} Leaks are found in xmlStringLenDecodeEntities()");
                                            eprint!(" {}", n_ctxt);
                                            eprint!(" {}", n_str);
                                            eprint!(" {}", n_len);
                                            eprint!(" {}", n_what);
                                            eprint!(" {}", n_end);
                                            eprint!(" {}", n_end2);
                                            eprintln!(" {}", n_end3);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

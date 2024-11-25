//! Provide internal methods and data structures for parsing XML documents.  
//! This module is based on `libxml/parserInternals.h`, `parserInternals.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// parserInternals.c : Internal routines (and obsolete ones) needed for the XML and HTML parsers.
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::cell::RefCell;
use std::ffi::{c_char, CStr, CString};
use std::mem::{size_of, zeroed};
use std::ptr::{addr_of_mut, null, null_mut};
use std::rc::Rc;
use std::slice::from_raw_parts;
use std::str::from_utf8_mut;
use std::sync::atomic::Ordering;

#[cfg(feature = "libxml_legacy")]
pub use __parser_internal_for_legacy::*;
use libc::{memcpy, memset, snprintf, INT_MAX};

use crate::error::XmlParserErrors;
use crate::tree::{NodeCommon, NodePtr, XmlNode};
#[cfg(feature = "catalog")]
use crate::{
    __xml_raise_error,
    encoding::{
        detect_encoding, find_encoding_handler, get_encoding_handler, XmlCharEncoding,
        XmlCharEncodingHandler,
    },
    generic_error,
    globals::{get_parser_debug_entities, GenericErrorContext},
    io::{__xml_loader_err, xml_check_http_input, xml_parser_get_directory, XmlParserInputBuffer},
    libxml::{
        catalog::{
            xml_catalog_add_local, xml_catalog_get_defaults, XmlCatalogAllow, XML_CATALOG_PI,
        },
        chvalid::{
            xml_is_base_char, xml_is_blank_char, xml_is_char, xml_is_combining, xml_is_digit,
            xml_is_extender, xml_is_ideographic,
        },
        dict::{xml_dict_free, xml_dict_lookup, xml_dict_owns, xml_dict_reference},
        entities::{xml_get_predefined_entity, XmlEntityPtr, XmlEntityType},
        globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
        hash::{
            xml_hash_add_entry2, xml_hash_create_dict, xml_hash_lookup2, xml_hash_update_entry2,
        },
        parser::{
            xml_err_msg_str, xml_fatal_err_msg, xml_fatal_err_msg_int, xml_fatal_err_msg_str,
            xml_fatal_err_msg_str_int_str, xml_free_parser_ctxt, xml_is_name_char,
            xml_load_external_entity, xml_new_parser_ctxt, xml_new_sax_parser_ctxt, xml_ns_err,
            xml_parse_att_value_internal, xml_parse_cdsect, xml_parse_char_data_internal,
            xml_parse_char_ref, xml_parse_conditional_sections,
            xml_parse_element_children_content_decl_priv, xml_parse_enc_name, xml_parse_end_tag1,
            xml_parse_end_tag2, xml_parse_external_entity_private, xml_parse_external_id,
            xml_parse_markup_decl, xml_parse_start_tag2, xml_parse_string_name,
            xml_parse_text_decl, xml_parse_version_num, xml_parser_add_node_info,
            xml_parser_entity_check, xml_parser_find_node_info, xml_string_decode_entities_int,
            xml_validity_error, xml_warning_msg, XmlDefAttrs, XmlDefAttrsPtr, XmlParserCtxtPtr,
            XmlParserInput, XmlParserInputPtr, XmlParserInputState, XmlParserMode,
            XmlParserNodeInfo, XmlParserNodeInfoPtr, XmlParserOption, XmlSAXHandlerPtr,
            XML_SKIP_IDS,
        },
        sax2::xml_sax2_get_entity,
        uri::{xml_build_uri, xml_canonic_path},
        valid::{
            xml_create_enumeration, xml_free_doc_element_content, xml_free_enumeration,
            xml_new_doc_element_content, xml_validate_element, xml_validate_root,
        },
        xmlstring::{
            xml_str_equal, xml_strchr, xml_strcmp, xml_strdup, xml_strlen, xml_strncmp,
            xml_strndup, XmlChar,
        },
    },
    private::{
        entities::{
            XML_ENT_CHECKED, XML_ENT_CHECKED_LT, XML_ENT_CONTAINS_LT, XML_ENT_EXPANDING,
            XML_ENT_PARSED,
        },
        parser::{__xml_err_encoding, xml_err_memory},
    },
    tree::{
        xml_create_int_subset, xml_doc_copy_node, xml_free_doc, xml_free_node, xml_free_node_list,
        xml_new_doc, xml_new_doc_node, xml_split_qname3, XmlAttributeDefault, XmlAttributeType,
        XmlDocProperties, XmlDocPtr, XmlElementContentOccur, XmlElementContentPtr,
        XmlElementContentType, XmlElementType, XmlElementTypeVal, XmlEnumerationPtr, XmlNodePtr,
        XML_XML_NAMESPACE,
    },
};

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

/**
 * XML_MAX_DICTIONARY_LIMIT:
 *
 * Maximum size allowed by the parser for a dictionary by default
 * This is not a limitation of the parser but a safety boundary feature,
 * use XML_PARSE_HUGE option to override it.
 * Introduced in 2.9.0
 */
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

/// Handle an internal error
#[doc(alias = "xmlErrInternal")]
pub(crate) unsafe extern "C" fn xml_err_internal(
    ctxt: XmlParserCtxtPtr,
    msg: *const c_char,
    str: *const XmlChar,
) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = XmlParserErrors::XmlErrInternalError as i32;
    }
    __xml_raise_error!(
        None,
        None,
        None,
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromParser,
        XmlParserErrors::XmlErrInternalError,
        XmlErrorLevel::XmlErrFatal,
        null_mut(),
        0,
        (!str.is_null()).then(|| CStr::from_ptr(str as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()),
        None,
        None,
        0,
        0,
        msg,
        str
    );
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
        if (*ctxt).recovery == 0 {
            (*ctxt).disable_sax = 1;
        }
    }
}

/// Create a parser context for a file content.
///
/// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
/// by default if found at compile-time.  
/// However, this crate does not support currently.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateFileParserCtxt")]
pub unsafe extern "C" fn xml_create_file_parser_ctxt(filename: *const c_char) -> XmlParserCtxtPtr {
    xml_create_url_parser_ctxt(filename, 0)
}

/// Create a parser context for a file or URL content.
///
/// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
/// by default if found at compile-time.  
/// However, this crate does not support currently.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateURLParserCtxt")]
pub unsafe extern "C" fn xml_create_url_parser_ctxt(
    filename: *const c_char,
    options: i32,
) -> XmlParserCtxtPtr {
    let mut directory: *mut c_char = null_mut();

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        xml_err_memory(null_mut(), c"cannot allocate parser context".as_ptr() as _);
        return null_mut();
    }

    if options != 0 {
        (*ctxt).ctxt_use_options_internal(options, None);
    }
    (*ctxt).linenumbers = 1;

    let input_stream: XmlParserInputPtr = xml_load_external_entity(filename, null_mut(), ctxt);
    if input_stream.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    (*ctxt).input_push(input_stream);
    if (*ctxt).directory.is_none() && directory.is_null() {
        directory = xml_parser_get_directory(filename);
    }
    if (*ctxt).directory.is_none() && !directory.is_null() {
        (*ctxt).directory = Some(
            CStr::from_ptr(directory as *const i8)
                .to_string_lossy()
                .into_owned(),
        );
        xml_free(directory as _);
    }

    ctxt
}

/// Create a parser context for an XML in-memory document.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateMemoryParserCtxt")]
pub unsafe fn xml_create_memory_parser_ctxt(buffer: Vec<u8>) -> XmlParserCtxtPtr {
    if buffer.is_empty() {
        return null_mut();
    }

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        return null_mut();
    }

    let Some(buf) = XmlParserInputBuffer::from_memory(buffer, XmlCharEncoding::None) else {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    };

    let input: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    (*input).filename = None;
    (*input).buf = Some(Rc::new(RefCell::new(buf)));
    (*input).reset_base();

    (*ctxt).input_push(input);
    ctxt
}

/// Create a parser context for an external entity
///
/// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
/// by default if found at compile-time.  
/// However, this crate does not support currently.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateEntityParserCtxtInternal")]
pub(crate) unsafe fn xml_create_entity_parser_ctxt_internal(
    sax: XmlSAXHandlerPtr,
    user_data: Option<GenericErrorContext>,
    mut url: *const XmlChar,
    id: *const XmlChar,
    base: *const XmlChar,
    pctx: XmlParserCtxtPtr,
) -> XmlParserCtxtPtr {
    let input_stream: XmlParserInputPtr;
    let mut directory: *mut c_char = null_mut();

    let ctxt: XmlParserCtxtPtr = xml_new_sax_parser_ctxt(sax, user_data);
    if ctxt.is_null() {
        return null_mut();
    }

    if !pctx.is_null() {
        (*ctxt).options = (*pctx).options;
        (*ctxt)._private = (*pctx)._private;
        (*ctxt).input_id = (*pctx).input_id;
    }

    /* Don't read from stdin. */
    if xml_strcmp(url, c"-".as_ptr() as _) == 0 {
        url = c"./-".as_ptr() as _;
    }

    let uri: *mut XmlChar = xml_build_uri(url, base);

    if uri.is_null() {
        input_stream = xml_load_external_entity(url as _, id as _, ctxt);
        if input_stream.is_null() {
            xml_free_parser_ctxt(ctxt);
            return null_mut();
        }

        (*ctxt).input_push(input_stream);

        if (*ctxt).directory.is_none() && directory.is_null() {
            directory = xml_parser_get_directory(url as _);
        }
        if (*ctxt).directory.is_none() && !directory.is_null() {
            (*ctxt).directory = Some(CStr::from_ptr(directory).to_string_lossy().into_owned());
            xml_free(directory as _);
        }
    } else {
        input_stream = xml_load_external_entity(uri as _, id as _, ctxt as _);
        if input_stream.is_null() {
            xml_free(uri as _);
            xml_free_parser_ctxt(ctxt);
            return null_mut();
        }

        (*ctxt).input_push(input_stream);

        if (*ctxt).directory.is_none() && directory.is_null() {
            directory = xml_parser_get_directory(uri as _);
        }
        if (*ctxt).directory.is_none() && !directory.is_null() {
            (*ctxt).directory = Some(CStr::from_ptr(directory).to_string_lossy().into_owned());
            xml_free(directory as _);
        }
        xml_free(uri as _);
    }
    ctxt
}

/// Create a parser context for an external entity
///
/// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
/// by default if found at compile-time.  
/// However, this crate does not support currently.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateEntityParserCtxt")]
pub unsafe extern "C" fn xml_create_entity_parser_ctxt(
    url: *const XmlChar,
    id: *const XmlChar,
    base: *const XmlChar,
) -> XmlParserCtxtPtr {
    xml_create_entity_parser_ctxt_internal(null_mut(), None, url, id, base, null_mut())
}

unsafe fn xml_detect_ebcdic(input: XmlParserInputPtr) -> Option<XmlCharEncodingHandler> {
    let mut out: [XmlChar; 200] = [0; 200];

    /*
     * To detect the EBCDIC code page, we convert the first 200 bytes
     * to EBCDIC-US and try to find the encoding declaration.
     */
    let mut handler = get_encoding_handler(XmlCharEncoding::EBCDIC)?;
    let inlen = (*input).end.offset_from((*input).cur) as usize;
    let outstr = from_utf8_mut(&mut out).ok()?;
    let Ok((_, outlen)) = handler.decode(from_raw_parts((*input).cur, inlen), outstr) else {
        return Some(handler);
    };
    out[outlen] = 0;

    let mut i: usize = 0;
    while i < outlen {
        if out[i] == b'>' {
            break;
        }
        if out[i] == b'e'
            && xml_strncmp(out.as_ptr().add(i) as _, c"encoding".as_ptr() as _, 8) == 0
        {
            let mut cur: u8;

            i += 8;
            while xml_is_blank_char(out[i] as u32) {
                i += 1;
            }
            i += 1;
            if out[i - 1] != b'=' {
                break;
            }
            while xml_is_blank_char(out[i] as u32) {
                i += 1;
            }
            let quote: u8 = out[i];
            i += 1;
            if quote != b'\'' && quote != b'"' {
                break;
            }
            let start: usize = i;
            cur = out[i];
            while cur.is_ascii_lowercase()
                || cur.is_ascii_uppercase()
                || cur.is_ascii_digit()
                || cur == b'.'
                || cur == b'_'
                || cur == b'-'
            {
                i += 1;
                cur = out[i];
            }
            if cur != quote {
                break;
            }
            out[i] = 0;
            return find_encoding_handler(
                CStr::from_ptr((out.as_ptr() as *mut c_char).add(start))
                    .to_str()
                    .unwrap(),
            );
        }

        i += 1;
    }

    Some(handler)
}

/// Change the input functions when discovering the character encoding of a given entity.
///
/// Returns 0 in case of success, -1 otherwise
#[doc(alias = "xmlSwitchEncoding")]
pub unsafe fn xml_switch_encoding(ctxt: XmlParserCtxtPtr, enc: XmlCharEncoding) -> i32 {
    if ctxt.is_null() {
        return -1;
    }

    /*
     * FIXME: The BOM shouldn't be skipped here, but in the parsing code.
     *
     * Note that we look for a decoded UTF-8 BOM when switching to UTF-16.
     * This is mostly useless but Webkit/Chromium relies on this behavior.
     * See https://bugs.chromium.org/p/chromium/issues/detail?id=1451026
     */
    if !(*ctxt).input.is_null()
        && (*(*ctxt).input).consumed == 0
        && !(*(*ctxt).input).cur.is_null()
        && (*(*ctxt).input).cur == (*(*ctxt).input).base
        && matches!(
            enc,
            XmlCharEncoding::UTF8 | XmlCharEncoding::UTF16LE | XmlCharEncoding::UTF16BE
        )
    {
        /*
         * Errata on XML-1.0 June 20 2001
         * Specific handling of the Byte Order Mark for
         * UTF-8
         */
        if *(*(*ctxt).input).cur.add(0) == 0xEF
            && *(*(*ctxt).input).cur.add(1) == 0xBB
            && *(*(*ctxt).input).cur.add(2) == 0xBF
        {
            (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(3);
        }
    }

    let Some(handler) = (match enc {
        XmlCharEncoding::Error => {
            __xml_err_encoding(
                ctxt,
                XmlParserErrors::XmlErrUnknownEncoding,
                c"encoding unknown\n".as_ptr() as _,
                null(),
                null(),
            );
            return -1;
        }
        XmlCharEncoding::None => {
            /* let's assume it's UTF-8 without the XML decl */
            (*ctxt).charset = XmlCharEncoding::UTF8;
            return 0;
        }
        XmlCharEncoding::UTF8 => {
            /* default encoding, no conversion should be needed */
            (*ctxt).charset = XmlCharEncoding::UTF8;
            return 0;
        }
        XmlCharEncoding::EBCDIC => xml_detect_ebcdic((*ctxt).input),
        _ => get_encoding_handler(enc),
    }) else {
        /*
         * Default handlers.
         */
        match enc {
            XmlCharEncoding::ASCII => {
                /* default encoding, no conversion should be needed */
                (*ctxt).charset = XmlCharEncoding::UTF8;
                return 0;
            }
            XmlCharEncoding::ISO8859_1 => {
                if (*ctxt).input_tab.len() == 1
                    && (*ctxt).encoding.is_none()
                    && !(*ctxt).input.is_null()
                    && (*(*ctxt).input).encoding.is_some()
                {
                    (*ctxt).encoding = (*(*ctxt).input).encoding.clone();
                }
                (*ctxt).charset = enc;
                return 0;
            }
            _ => {
                let name = enc.get_name().unwrap_or("");
                let cstr = CString::new(name).unwrap();
                __xml_err_encoding(
                    ctxt,
                    XmlParserErrors::XmlErrUnsupportedEncoding,
                    c"encoding not supported: %s\n".as_ptr() as _,
                    cstr.as_ptr() as *const u8,
                    null(),
                );
                /*
                 * TODO: We could recover from errors in external entities
                 * if we didn't stop the parser. But most callers of this
                 * function don't check the return value.
                 */
                (*ctxt).stop();
                return -1;
            }
        }
    };
    let ret: i32 = (*ctxt).switch_input_encoding((*ctxt).input, handler);
    if ret < 0 || (*ctxt).err_no == XmlParserErrors::XmlI18NConvFailed as i32 {
        /*
         * on encoding conversion errors, stop the parser
         */
        (*ctxt).stop();
        (*ctxt).err_no = XmlParserErrors::XmlI18NConvFailed as i32;
    }
    ret
}

/// Create a new input stream based on a memory buffer.
///
/// Returns the new input stream
#[doc(alias = "xmlNewStringInputStream")]
pub unsafe extern "C" fn xml_new_string_input_stream(
    ctxt: XmlParserCtxtPtr,
    buffer: *const XmlChar,
) -> XmlParserInputPtr {
    if buffer.is_null() {
        xml_err_internal(
            ctxt,
            c"xmlNewStringInputStream string = NULL\n".as_ptr() as _,
            null(),
        );
        return null_mut();
    }
    if get_parser_debug_entities() != 0 {
        generic_error!(
            "new fixed input: {}\n",
            CStr::from_ptr(buffer as *const i8)
                .to_string_lossy()
                .chars()
                .take(30)
                .collect::<String>()
        );
    }
    let Some(buf) = XmlParserInputBuffer::from_memory(
        CStr::from_ptr(buffer as *const i8).to_bytes().to_vec(),
        XmlCharEncoding::None,
    ) else {
        xml_err_memory(ctxt, null());
        return null_mut();
    };
    let input: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input.is_null() {
        xml_err_memory(
            ctxt,
            c"couldn't allocate a new input stream\n".as_ptr() as _,
        );
        // xml_free_parser_input_buffer(buf);
        return null_mut();
    }
    (*input).buf = Some(Rc::new(RefCell::new(buf)));
    (*input).reset_base();
    input
}

/// Create a new input stream based on an xmlEntityPtr
///
/// Returns the new input stream or NULL
#[doc(alias = "xmlNewEntityInputStream")]
pub(crate) unsafe extern "C" fn xml_new_entity_input_stream(
    ctxt: XmlParserCtxtPtr,
    entity: XmlEntityPtr,
) -> XmlParserInputPtr {
    let input: XmlParserInputPtr;

    if entity.is_null() {
        xml_err_internal(
            ctxt,
            c"xmlNewEntityInputStream entity = NULL\n".as_ptr() as _,
            null(),
        );
        return null_mut();
    }
    if get_parser_debug_entities() != 0 {
        generic_error!(
            "new input from entity: {}\n",
            CStr::from_ptr((*entity).name.load(Ordering::Relaxed) as *const i8).to_string_lossy()
        );
    }
    if (*entity).content.load(Ordering::Relaxed).is_null() {
        match (*entity).etype {
            Some(XmlEntityType::XmlExternalGeneralUnparsedEntity) => {
                xml_err_internal(
                    ctxt,
                    c"Cannot parse entity %s\n".as_ptr() as _,
                    (*entity).name.load(Ordering::Relaxed) as _,
                );
            }
            Some(XmlEntityType::XmlExternalGeneralParsedEntity)
            | Some(XmlEntityType::XmlExternalParameterEntity) => {
                input = xml_load_external_entity(
                    (*entity).uri.load(Ordering::Relaxed) as _,
                    (*entity).external_id.load(Ordering::Relaxed) as _,
                    ctxt,
                );
                if !input.is_null() {
                    (*input).entity = entity;
                }
                return input;
            }
            Some(XmlEntityType::XmlInternalGeneralEntity) => {
                xml_err_internal(
                    ctxt,
                    c"Internal entity %s without content !\n".as_ptr(),
                    (*entity).name.load(Ordering::Relaxed) as _,
                );
            }
            Some(XmlEntityType::XmlInternalParameterEntity) => {
                xml_err_internal(
                    ctxt,
                    c"Internal parameter entity %s without content !\n".as_ptr(),
                    (*entity).name.load(Ordering::Relaxed) as _,
                );
            }
            Some(XmlEntityType::XmlInternalPredefinedEntity) => {
                xml_err_internal(
                    ctxt,
                    c"Predefined entity %s without content !\n".as_ptr(),
                    (*entity).name.load(Ordering::Relaxed) as _,
                );
            }
            _ => {
                unreachable!()
            }
        }
        return null_mut();
    }
    input = xml_new_input_stream(ctxt);
    if input.is_null() {
        return null_mut();
    }
    if !(*entity).uri.load(Ordering::Relaxed).is_null() {
        (*input).filename = Some(
            CStr::from_ptr((*entity).uri.load(Ordering::Relaxed) as *const i8)
                .to_string_lossy()
                .into_owned(),
        );
    }
    (*input).base = (*entity).content.load(Ordering::Relaxed) as _;
    if (*entity).length == 0 {
        (*entity).length = xml_strlen((*entity).content.load(Ordering::Relaxed) as _);
    }
    (*input).cur = (*entity).content.load(Ordering::Relaxed);
    (*input).length = (*entity).length;
    (*input).end = (*entity)
        .content
        .load(Ordering::Relaxed)
        .add((*input).length as usize);
    (*input).entity = entity;
    input
}

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlFatalErr")]
pub(crate) unsafe extern "C" fn xml_fatal_err(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    info: *const c_char,
) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    let errmsg: *const i8 = match error {
        XmlParserErrors::XmlErrInvalidHexCharRef => {
            c"CharRef: invalid hexadecimal value".as_ptr() as _
        }
        XmlParserErrors::XmlErrInvalidDecCharRef => c"CharRef: invalid decimal value".as_ptr() as _,
        XmlParserErrors::XmlErrInvalidCharRef => c"CharRef: invalid value".as_ptr() as _,
        XmlParserErrors::XmlErrInternalError => c"internal error".as_ptr() as _,
        XmlParserErrors::XmlErrPERefAtEOF => c"PEReference at end of document".as_ptr() as _,
        XmlParserErrors::XmlErrPERefInProlog => c"PEReference in prolog".as_ptr() as _,
        XmlParserErrors::XmlErrPERefInEpilog => c"PEReference in epilog".as_ptr() as _,
        XmlParserErrors::XmlErrPERefNoName => c"PEReference: no name".as_ptr() as _,
        XmlParserErrors::XmlErrPERefSemicolMissing => c"PEReference: expecting ';'".as_ptr() as _,
        XmlParserErrors::XmlErrEntityLoop => c"Detected an entity reference loop".as_ptr() as _,
        XmlParserErrors::XmlErrEntityNotStarted => c"EntityValue: \" or ' expected".as_ptr() as _,
        XmlParserErrors::XmlErrEntityPEInternal => {
            c"PEReferences forbidden in internal subset".as_ptr() as _
        }
        XmlParserErrors::XmlErrEntityNotFinished => c"EntityValue: \" or ' expected".as_ptr() as _,
        XmlParserErrors::XmlErrAttributeNotStarted => c"AttValue: \" or ' expected".as_ptr() as _,
        XmlParserErrors::XmlErrLtInAttribute => {
            c"Unescaped '<' not allowed in attributes values".as_ptr() as _
        }
        XmlParserErrors::XmlErrLiteralNotStarted => c"SystemLiteral \" or ' expected".as_ptr() as _,
        XmlParserErrors::XmlErrLiteralNotFinished => {
            c"Unfinished System or Public ID \" or ' expected".as_ptr() as _
        }
        XmlParserErrors::XmlErrMisplacedCDATAEnd => {
            c"Sequence ']]>' not allowed in content".as_ptr() as _
        }
        XmlParserErrors::XmlErrURIRequired => c"SYSTEM or PUBLIC, the URI is missing".as_ptr() as _,
        XmlParserErrors::XmlErrPubidRequired => {
            c"PUBLIC, the Public Identifier is missing".as_ptr() as _
        }
        XmlParserErrors::XmlErrHyphenInComment => {
            c"Comment must not contain '--' (double-hyphen)".as_ptr() as _
        }
        XmlParserErrors::XmlErrPINotStarted => c"xmlParsePI : no target name".as_ptr() as _,
        XmlParserErrors::XmlErrReservedXmlName => c"Invalid PI name".as_ptr() as _,
        XmlParserErrors::XmlErrNotationNotStarted => c"NOTATION: Name expected here".as_ptr() as _,
        XmlParserErrors::XmlErrNotationNotFinished => {
            c"'>' required to close NOTATION declaration".as_ptr() as _
        }
        XmlParserErrors::XmlErrValueRequired => c"Entity value required".as_ptr() as _,
        XmlParserErrors::XmlErrURIFragment => c"Fragment not allowed".as_ptr() as _,
        XmlParserErrors::XmlErrAttlistNotStarted => {
            c"'(' required to start ATTLIST enumeration".as_ptr() as _
        }
        XmlParserErrors::XmlErrNmtokenRequired => {
            c"NmToken expected in ATTLIST enumeration".as_ptr() as _
        }
        XmlParserErrors::XmlErrAttlistNotFinished => {
            c"')' required to finish ATTLIST enumeration".as_ptr() as _
        }
        XmlParserErrors::XmlErrMixedNotStarted => {
            c"MixedContentDecl : '|' or ')*' expected".as_ptr() as _
        }
        XmlParserErrors::XmlErrPCDATARequired => {
            c"MixedContentDecl : '#PCDATA' expected".as_ptr() as _
        }
        XmlParserErrors::XmlErrElemcontentNotStarted => {
            c"ContentDecl : Name or '(' expected".as_ptr() as _
        }
        XmlParserErrors::XmlErrElemcontentNotFinished => {
            c"ContentDecl : ',' '|' or ')' expected".as_ptr() as _
        }
        XmlParserErrors::XmlErrPERefInIntSubset => {
            c"PEReference: forbidden within markup decl in internal subset".as_ptr() as _
        }
        XmlParserErrors::XmlErrGtRequired => c"expected '>'".as_ptr() as _,
        XmlParserErrors::XmlErrCondsecInvalid => {
            c"XML conditional section '[' expected".as_ptr() as _
        }
        XmlParserErrors::XmlErrExtSubsetNotFinished => {
            c"Content error in the external subset".as_ptr() as _
        }
        XmlParserErrors::XmlErrCondsecInvalidKeyword => {
            c"conditional section INCLUDE or IGNORE keyword expected".as_ptr() as _
        }
        XmlParserErrors::XmlErrCondsecNotFinished => {
            c"XML conditional section not closed".as_ptr() as _
        }
        XmlParserErrors::XmlErrXMLDeclNotStarted => {
            c"Text declaration '<?xml' required".as_ptr() as _
        }
        XmlParserErrors::XmlErrXMLDeclNotFinished => {
            c"parsing XML declaration: '?>' expected".as_ptr() as _
        }
        XmlParserErrors::XmlErrExtEntityStandalone => {
            c"external parsed entities cannot be standalone".as_ptr() as _
        }
        XmlParserErrors::XmlErrEntityRefSemicolMissing => c"EntityRef: expecting ';'".as_ptr() as _,
        XmlParserErrors::XmlErrDoctypeNotFinished => c"DOCTYPE improperly terminated".as_ptr() as _,
        XmlParserErrors::XmlErrLtSlashRequired => c"EndTag: '</' not found".as_ptr() as _,
        XmlParserErrors::XmlErrEqualRequired => c"expected '='".as_ptr() as _,
        XmlParserErrors::XmlErrStringNotClosed => {
            c"String not closed expecting \" or '".as_ptr() as _
        }
        XmlParserErrors::XmlErrStringNotStarted => {
            c"String not started expecting ' or \"".as_ptr() as _
        }
        XmlParserErrors::XmlErrEncodingName => c"Invalid XML encoding name".as_ptr() as _,
        XmlParserErrors::XmlErrStandaloneValue => {
            c"standalone accepts only 'yes' or 'no'".as_ptr() as _
        }
        XmlParserErrors::XmlErrDocumentEmpty => c"Document is empty".as_ptr() as _,
        XmlParserErrors::XmlErrDocumentEnd => {
            c"Extra content at the end of the document".as_ptr() as _
        }
        XmlParserErrors::XmlErrNotWellBalanced => c"chunk is not well balanced".as_ptr() as _,
        XmlParserErrors::XmlErrExtraContent => {
            c"extra content at the end of well balanced chunk".as_ptr() as _
        }
        XmlParserErrors::XmlErrVersionMissing => {
            c"Malformed declaration expecting version".as_ptr() as _
        }
        XmlParserErrors::XmlErrNameTooLong => c"Name too long".as_ptr() as _,
        _ => c"Unregistered error message".as_ptr() as _,
    };
    if !ctxt.is_null() {
        (*ctxt).err_no = error as i32;
    }
    if info.is_null() {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
            error,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            (!info.is_null()).then(|| CStr::from_ptr(info).to_string_lossy().into_owned().into()),
            None,
            None,
            0,
            0,
            c"%s\n".as_ptr() as _,
            errmsg
        );
    } else {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
            error,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            (!info.is_null()).then(|| CStr::from_ptr(info).to_string_lossy().into_owned().into()),
            None,
            None,
            0,
            0,
            c"%s: %s\n".as_ptr() as _,
            errmsg,
            info
        );
    }
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
        if (*ctxt).recovery == 0 {
            (*ctxt).disable_sax = 1;
        }
    }
}

/// Match to a new input stream which is stacked on top of the previous one(s).
///
/// Returns -1 in case of error or the index in the input stack
#[doc(alias = "xmlPushInput")]
pub unsafe extern "C" fn xml_push_input(ctxt: XmlParserCtxtPtr, input: XmlParserInputPtr) -> i32 {
    if input.is_null() {
        return -1;
    }

    if get_parser_debug_entities() != 0 {
        if !(*ctxt).input.is_null() && (*(*ctxt).input).filename.is_some() {
            generic_error!(
                "{}({}): ",
                (*(*ctxt).input).filename.as_ref().unwrap(),
                (*(*ctxt).input).line
            );
        }
        let cur = CStr::from_ptr((*input).cur as *const i8).to_string_lossy();
        generic_error!(
            "Pushing input {} : {}\n",
            (*ctxt).input_tab.len() + 1,
            &cur[..cur.len().min(30)]
        );
    }
    if ((*ctxt).input_tab.len() > 40 && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0)
        || (*ctxt).input_tab.len() > 100
    {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, null());
        #[allow(clippy::while_immutable_condition)]
        while (*ctxt).input_tab.len() > 1 {
            xml_free_input_stream((*ctxt).input_pop());
        }
        return -1;
    }
    let ret: i32 = (*ctxt).input_push(input);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }
    (*ctxt).grow();
    ret
}

/// Free up an input stream.
#[doc(alias = "xmlFreeInputStream")]
pub unsafe extern "C" fn xml_free_input_stream(input: XmlParserInputPtr) {
    if input.is_null() {
        return;
    }

    (*input).filename = None;
    (*input).directory = None;
    (*input).encoding = None;
    (*input).version = None;
    if !(*input).base.is_null() {
        if let Some(free) = (*input).free {
            free((*input).base as _);
        }
    }
    let _ = (*input).buf.take();
    xml_free(input as _);
}

/// Create a new input stream based on a file or an URL.
///
/// Returns the new input stream or NULL in case of error
#[doc(alias = "xmlNewInputFromFile")]
pub unsafe extern "C" fn xml_new_input_from_file(
    ctxt: XmlParserCtxtPtr,
    filename: *const c_char,
) -> XmlParserInputPtr {
    let mut input_stream: XmlParserInputPtr;

    if get_parser_debug_entities() != 0 {
        generic_error!(
            "new input from file: {}\n",
            CStr::from_ptr(filename).to_string_lossy()
        );
    }
    if ctxt.is_null() {
        return null_mut();
    }

    if filename.is_null() {
        __xml_loader_err(
            ctxt as _,
            c"failed to load external entity: NULL filename \n".as_ptr() as _,
            null(),
        );
        return null_mut();
    }
    let Some(buf) = XmlParserInputBuffer::from_uri(
        CStr::from_ptr(filename).to_string_lossy().as_ref(),
        XmlCharEncoding::None,
    ) else {
        if filename.is_null() {
            __xml_loader_err(
                ctxt as _,
                c"failed to load external entity: NULL filename \n".as_ptr() as _,
                null(),
            );
        } else {
            __xml_loader_err(
                ctxt as _,
                c"failed to load external entity \"%s\"\n".as_ptr() as _,
                filename as *const c_char,
            );
        }
        return null_mut();
    };

    input_stream = xml_new_input_stream(ctxt);
    if input_stream.is_null() {
        // xml_free_parser_input_buffer(buf);
        return null_mut();
    }

    (*input_stream).buf = Some(Rc::new(RefCell::new(buf)));
    input_stream = xml_check_http_input(ctxt, input_stream);
    if input_stream.is_null() {
        return null_mut();
    }

    let uri = if let Some(filename) = (*input_stream).filename.as_deref() {
        let filename = CString::new(filename).unwrap();
        xml_strdup(filename.as_ptr() as *mut XmlChar)
    } else {
        xml_strdup(filename as *mut XmlChar)
    };
    let directory: *mut c_char = xml_parser_get_directory(uri as *const c_char);
    {
        let canonic = xml_canonic_path(uri as *const XmlChar);
        if !canonic.is_null() {
            (*input_stream).filename = Some(
                CStr::from_ptr(canonic as *const i8)
                    .to_string_lossy()
                    .into_owned(),
            );
            xml_free(canonic as _);
        }
    }
    if !uri.is_null() {
        xml_free(uri as _);
    }
    if !directory.is_null() {
        (*input_stream).directory = Some(
            CStr::from_ptr(directory as *const i8)
                .to_string_lossy()
                .into(),
        );
    } else {
        (*input_stream).directory = None;
    }
    (*input_stream).reset_base();

    if (*ctxt).directory.is_none() && !directory.is_null() {
        (*ctxt).directory = Some(
            CStr::from_ptr(directory as *const i8)
                .to_string_lossy()
                .into_owned(),
        );
    }
    xml_free(directory as _);
    input_stream
}

/// Create a new input stream structure.
///
/// Returns the new input stream or NULL
#[doc(alias = "xmlNewInputStream")]
pub unsafe extern "C" fn xml_new_input_stream(ctxt: XmlParserCtxtPtr) -> XmlParserInputPtr {
    let input: XmlParserInputPtr = xml_malloc(size_of::<XmlParserInput>()) as XmlParserInputPtr;
    if input.is_null() {
        xml_err_memory(
            ctxt,
            c"couldn't allocate a new input stream\n".as_ptr() as _,
        );
        return null_mut();
    }
    memset(input as _, 0, size_of::<XmlParserInput>());
    std::ptr::write(&mut *input, XmlParserInput::default());
    (*input).line = 1;
    (*input).col = 1;
    (*input).standalone = -1;
    std::ptr::write(&raw mut (*input).buf, None);

    /*
     * If the context is NULL the id cannot be initialized, but that
     * should not happen while parsing which is the situation where
     * the id is actually needed.
     */
    if !ctxt.is_null() {
        if (*input).id == INT_MAX {
            xml_err_memory(ctxt, c"Input ID overflow\n".as_ptr() as _);
            return null_mut();
        }
        (*input).id = (*ctxt).input_id;
        (*ctxt).input_id += 1;
    }

    input
}

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
pub unsafe extern "C" fn xml_split_qname(
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

    /* nasty but well=formed */
    if *cur.add(0) == b':' {
        return xml_strdup(name);
    }

    c = *cur as _;
    cur = cur.add(1);
    while c != 0 && c != b':' as i32 && len < max {
        /* tested bigname.xml */
        buf[len as usize] = c as _;
        len += 1;
        c = *cur as _;
        cur = cur.add(1);
    }
    if len >= max {
        /*
         * Okay someone managed to make a huge name, so he's ready to pay
         * for the processing speed.
         */
        max = len * 2;

        buffer = xml_malloc_atomic(max as usize) as *mut XmlChar;
        if buffer.is_null() {
            xml_err_memory(ctxt, null());
            return null_mut();
        }
        memcpy(buffer as _, buf.as_ptr() as _, len as usize);
        while c != 0 && c != b':' as i32 {
            /* tested bigname.xml */
            if len + 10 > max {
                max *= 2;
                let tmp: *mut XmlChar = xml_realloc(buffer as _, max as usize) as *mut XmlChar;
                if tmp.is_null() {
                    xml_free(buffer as _);
                    xml_err_memory(ctxt, null());
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

        /*
         * Check that the first character is proper to start
         * a new name
         */
        if !((0x61..=0x7A).contains(&c)
            || (0x41..=0x5A).contains(&c)
            || c == b'_' as i32
            || c == b':' as i32)
        {
            let mut l: i32 = 0;
            let first: i32 = CUR_SCHAR!(ctxt, cur, l);

            if !xml_is_letter(first as u32) && first != b'_' as i32 {
                xml_fatal_err_msg_str(
                    ctxt,
                    XmlParserErrors::XmlNsErrQname,
                    c"Name %s is not XML Namespace compliant\n".as_ptr() as _,
                    name,
                );
            }
        }
        cur = cur.add(1);

        while c != 0 && len < max {
            /* tested bigname2.xml */
            buf[len as usize] = c as _;
            len += 1;
            c = *cur as _;
            cur = cur.add(1);
        }
        if len >= max {
            /*
             * Okay someone managed to make a huge name, so he's ready to pay
             * for the processing speed.
             */
            max = len * 2;

            buffer = xml_malloc_atomic(max as usize) as *mut XmlChar;
            if buffer.is_null() {
                xml_err_memory(ctxt, null());
                return null_mut();
            }
            memcpy(buffer as _, buf.as_ptr() as _, len as usize);
            while c != 0 {
                /* tested bigname2.xml */
                if len + 10 > max {
                    max *= 2;
                    let tmp: *mut XmlChar = xml_realloc(buffer as _, max as usize) as *mut XmlChar;
                    if tmp.is_null() {
                        xml_err_memory(ctxt, null());
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

unsafe extern "C" fn xml_parse_name_complex(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    let mut len: i32 = 0;
    let mut l: i32 = 0;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };

    /*
     * Handler for more complex cases
     */
    let c = (*ctxt).current_char(&mut l).unwrap_or('\0');
    if (*ctxt).options & XmlParserOption::XmlParseOld10 as i32 == 0 {
        /*
         * Use the new checks of production [4] [4a] amd [5] of the
         * Update 5 of XML-1.0
         */
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
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrNameTooLong,
            c"Name".as_ptr() as _,
        );
        return null_mut();
    }
    if (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) < len as isize {
        /*
         * There were a couple of bugs where PERefs lead to to a change
         * of the buffer. Check the buffer size to avoid passing an invalid
         * pointer to xmlDictLookup.
         */
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            c"unexpected change of input buffer".as_ptr() as _,
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
pub(crate) unsafe extern "C" fn xml_parse_name(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
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

    /*
     * Accelerator for simple ASCII names
     */
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
                xml_fatal_err(
                    ctxt,
                    XmlParserErrors::XmlErrNameTooLong,
                    c"Name".as_ptr() as _,
                );
                return null_mut();
            }
            ret = xml_dict_lookup((*ctxt).dict, (*(*ctxt).input).cur, count as i32);
            (*(*ctxt).input).cur = input;
            (*(*ctxt).input).col += count as i32;
            if ret.is_null() {
                xml_err_memory(ctxt, null());
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
pub(crate) unsafe extern "C" fn xml_parse_nmtoken(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    let mut buf: [XmlChar; XML_MAX_NAMELEN + 5] = [0; XML_MAX_NAMELEN + 5];
    let mut len: i32 = 0;
    let mut l: i32 = 0;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };

    let mut c = (*ctxt).current_char(&mut l).unwrap_or('\0');
    while xml_is_name_char(ctxt, c as i32) != 0 {
        COPY_BUF!(l, buf.as_mut_ptr(), len, c);
        NEXTL!(ctxt, l);
        c = (*ctxt).current_char(&mut l).unwrap_or('\0');
        if len >= XML_MAX_NAMELEN as i32 {
            /*
             * Okay someone managed to make a huge token, so he's ready to pay
             * for the processing speed.
             */
            let mut buffer: *mut XmlChar;
            let mut max: i32 = len * 2;

            buffer = xml_malloc_atomic(max as usize) as *mut XmlChar;
            if buffer.is_null() {
                xml_err_memory(ctxt, null());
                return null_mut();
            }
            memcpy(buffer as _, buf.as_ptr() as _, len as usize);
            while xml_is_name_char(ctxt, c as i32) != 0 {
                if len + 10 > max {
                    max *= 2;
                    let tmp: *mut XmlChar = xml_realloc(buffer as _, max as usize) as *mut XmlChar;
                    if tmp.is_null() {
                        xml_err_memory(ctxt, null());
                        xml_free(buffer as _);
                        return null_mut();
                    }
                    buffer = tmp;
                }
                COPY_BUF!(l, buffer, len, c);
                if len > max_length {
                    xml_fatal_err(
                        ctxt,
                        XmlParserErrors::XmlErrNameTooLong,
                        c"NmToken".as_ptr() as _,
                    );
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
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrNameTooLong,
            c"NmToken".as_ptr() as _,
        );
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
pub(crate) unsafe extern "C" fn xml_parse_entity_value(
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
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityNotStarted, null_mut());
        return null_mut();
    }
    buf = xml_malloc_atomic(size as usize) as *mut XmlChar;
    if buf.is_null() {
        xml_err_memory(ctxt, null());
        return null_mut();
    }

    /*
     * The content of the entity definition is copied in a buffer.
     */

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
    /*
     * NOTE: 4.4.5 Included in Literal
     * When a parameter entity reference appears in a literal entity
     * value, ... a single or double quote character in the replacement
     * text is always treated as a normal data character and will not
     * terminate the literal.
     * In practice it means we stop the loop only when back at parsing
     * the initial entity and the quote is found
     */
    while (xml_is_char(c as u32) && (c as i32 != stop as i32 || (*ctxt).input != input))
        && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        if len + 5 >= size {
            size *= 2;
            let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize) as *mut XmlChar;
            if tmp.is_null() {
                xml_err_memory(ctxt, null());
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
                c"entity value too long\n".as_ptr() as _,
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
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityNotFinished, null());
        //  goto error;
        if !buf.is_null() {
            xml_free(buf as _);
        }
        return ret;
    }
    (*ctxt).skip_char();

    /*
     * Raise problem w.r.t. '&' and '%' being used in non-entities
     * reference constructs. Note Charref will be handled in
     * xmlStringDecodeEntities()
     */
    cur = buf;
    while *cur != 0 {
        /* non input consuming */
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
                xml_fatal_err_msg_int(
                    ctxt,
                    XmlParserErrors::XmlErrEntityCharError,
                    c"EntityValue: '%c' forbidden except for entities references\n".as_ptr() as _,
                    tmp as _,
                );
                //  goto error;
                if !buf.is_null() {
                    xml_free(buf as _);
                }
                return ret;
            }
            if tmp == b'%' && (*ctxt).in_subset == 1 && (*ctxt).input_tab.len() == 1 {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityPEInternal, null());
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

    /*
     * Then PEReference entities are substituted.
     *
     * NOTE: 4.4.7 Bypassed
     * When a general entity reference appears in the EntityValue in
     * an entity declaration, it is bypassed and left as is.
     * so XML_SUBSTITUTE_REF is not set here.
     */
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
pub(crate) unsafe extern "C" fn xml_parse_att_value(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
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
pub(crate) unsafe extern "C" fn xml_parse_system_literal(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
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
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotStarted, null());
        return null_mut();
    };

    buf = xml_malloc_atomic(size as usize) as *mut XmlChar;
    if buf.is_null() {
        xml_err_memory(ctxt, null());
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
                xml_err_memory(ctxt, null());
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
                c"SystemLiteral".as_ptr() as _,
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
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotFinished, null());
    } else {
        (*ctxt).skip_char();
    }
    buf
}

#[doc(alias = "xmlParseCharData")]
pub(crate) unsafe extern "C" fn xml_parse_char_data(ctxt: XmlParserCtxtPtr, _cdata: i32) {
    xml_parse_char_data_internal(ctxt, 0);
}

/// Skip an XML (SGML) comment `<!-- .... -->`.  
/// The spec says that "For compatibility, the string `"--"` (double-hyphen)
/// must not occur within comments."  
///
/// This is the slow routine in case the accelerator for ascii didn't work
///
/// `[15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'`
#[doc(alias = "xmlParseCommentComplex")]
unsafe extern "C" fn xml_parse_comment_complex(
    ctxt: XmlParserCtxtPtr,
    mut buf: *mut XmlChar,
    mut len: usize,
    mut size: usize,
) {
    let mut ql: i32 = 0;
    let mut rl: i32 = 0;
    let mut l: i32 = 0;
    let max_length: usize = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH
    } else {
        XML_MAX_TEXT_LENGTH
    };

    let inputid: i32 = (*(*ctxt).input).id;

    if buf.is_null() {
        len = 0;
        size = XML_PARSER_BUFFER_SIZE;
        buf = xml_malloc_atomic(size) as _;
        if buf.is_null() {
            xml_err_memory(ctxt, null());
            return;
        }
    }
    let mut q = (*ctxt).current_char(&mut ql).unwrap_or('\0');
    'not_terminated: {
        if q == '\0' {
            break 'not_terminated;
        }
        if !xml_is_char(q as u32) {
            xml_fatal_err_msg_int(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                c"xmlParseComment: invalid xmlChar value %d\n".as_ptr() as _,
                q as _,
            );
            xml_free(buf as _);
            return;
        }
        NEXTL!(ctxt, ql);
        let mut r = (*ctxt).current_char(&mut rl).unwrap_or('\0');
        if r == '\0' {
            break 'not_terminated;
        }
        if !xml_is_char(r as u32) {
            xml_fatal_err_msg_int(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                c"xmlParseComment: invalid xmlChar value %d\n".as_ptr() as _,
                r as _,
            );
            xml_free(buf as _);
            return;
        }
        NEXTL!(ctxt, rl);
        let mut cur = (*ctxt).current_char(&mut l).unwrap_or('\0');
        if cur == '\0' {
            break 'not_terminated;
        }
        while xml_is_char(cur as u32) && (cur != '>' || r != '-' || q != '-') {
            if r == '-' && q == '-' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrHyphenInComment, null());
            }
            if len + 5 >= size {
                let new_size: usize = size * 2;
                let new_buf: *mut XmlChar = xml_realloc(buf as _, new_size) as _;
                if new_buf.is_null() {
                    xml_free(buf as _);
                    xml_err_memory(ctxt, null());
                    return;
                }
                buf = new_buf;
                size = new_size;
            }
            COPY_BUF!(ql, buf, len, q);
            if len > max_length {
                xml_fatal_err_msg_str(
                    ctxt,
                    XmlParserErrors::XmlErrCommentNotFinished,
                    c"Comment too big found".as_ptr() as _,
                    null(),
                );
                xml_free(buf as _);
                return;
            }

            q = r;
            ql = rl;
            r = cur;
            rl = l;

            NEXTL!(ctxt, l);
            cur = (*ctxt).current_char(&mut l).unwrap_or('\0');
        }
        *buf.add(len) = 0;
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            xml_free(buf as _);
            return;
        }
        if cur == '\0' {
            xml_fatal_err_msg_str(
                ctxt,
                XmlParserErrors::XmlErrCommentNotFinished,
                c"Comment not terminated \n<!--%.50s\n".as_ptr() as _,
                buf,
            );
        } else if !xml_is_char(cur as u32) {
            xml_fatal_err_msg_int(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                c"xmlParseComment: invalid xmlChar value %d\n".as_ptr() as _,
                cur as _,
            );
        } else {
            if inputid != (*(*ctxt).input).id {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    c"Comment doesn't start and stop in the same entity\n".as_ptr() as _,
                );
            }
            (*ctxt).skip_char();
            if !(*ctxt).sax.is_null()
                && (*(*ctxt).sax).comment.is_some()
                && (*ctxt).disable_sax == 0
            {
                ((*(*ctxt).sax).comment.unwrap())((*ctxt).user_data.clone(), buf);
            }
        }
        xml_free(buf as _);
        return;
    }
    // not_terminated:
    xml_fatal_err_msg_str(
        ctxt,
        XmlParserErrors::XmlErrCommentNotFinished,
        c"Comment not terminated\n".as_ptr() as _,
        null(),
    );
    xml_free(buf as _);
}

/// Parse an XML (SGML) comment. Always consumes '<!'.
///
/// The spec says that "For compatibility, the string "--" (double-hyphen)
/// must not occur within comments. "
///
/// `[15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'`
#[doc(alias = "xmlParseComment")]
pub(crate) unsafe extern "C" fn xml_parse_comment(ctxt: XmlParserCtxtPtr) {
    let mut buf: *mut XmlChar = null_mut();
    let mut size: usize = XML_PARSER_BUFFER_SIZE;
    let mut len: usize = 0;
    let max_length: usize = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH
    } else {
        XML_MAX_TEXT_LENGTH
    };
    let mut input: *const XmlChar;
    let mut nbchar: usize;
    let mut ccol: i32;

    /*
     * Check that there is a comment right here.
     */
    if (*ctxt).current_byte() != b'<' || NXT!(ctxt, 1) != b'!' {
        return;
    }
    (*ctxt).advance(2);
    if (*ctxt).current_byte() != b'-' || NXT!(ctxt, 1) != b'-' {
        return;
    }
    let state: XmlParserInputState = (*ctxt).instate;
    (*ctxt).instate = XmlParserInputState::XmlParserComment;
    let inputid: i32 = (*(*ctxt).input).id;
    (*ctxt).advance(2);
    (*ctxt).grow();

    /*
     * Accelerated common case where input don't need to be
     * modified before passing it to the handler.
     */
    input = (*(*ctxt).input).cur;
    while {
        if *input == 0xA {
            while {
                (*(*ctxt).input).line += 1;
                (*(*ctxt).input).col = 1;
                input = input.add(1);

                *input == 0xA
            } {}
        }
        // get_more:
        'get_more: loop {
            ccol = (*(*ctxt).input).col;
            while (*input > b'-' && *input <= 0x7F)
                || (*input >= 0x20 && *input < b'-')
                || *input == 0x09
            {
                input = input.add(1);
                ccol += 1;
            }
            (*(*ctxt).input).col = ccol;
            if *input == 0xA {
                while {
                    (*(*ctxt).input).line += 1;
                    (*(*ctxt).input).col = 1;
                    input = input.add(1);

                    *input == 0xA
                } {}
                // goto get_more;
                continue 'get_more;
            }
            nbchar = input.offset_from((*(*ctxt).input).cur) as _;
            /*
             * save current set of data
             */
            if nbchar > 0 && !(*ctxt).sax.is_null() && (*(*ctxt).sax).comment.is_some() {
                if buf.is_null() {
                    if *input == b'-' && *input.add(1) == b'-' {
                        size = nbchar + 1;
                    } else {
                        size = XML_PARSER_BUFFER_SIZE + nbchar;
                    }
                    buf = xml_malloc_atomic(size) as *mut XmlChar;
                    if buf.is_null() {
                        xml_err_memory(ctxt, null());
                        (*ctxt).instate = state;
                        return;
                    }
                    len = 0;
                } else if len + nbchar + 1 >= size {
                    size += len + nbchar + XML_PARSER_BUFFER_SIZE;
                    let new_buf: *mut XmlChar = xml_realloc(buf as _, size) as *mut XmlChar;
                    if new_buf.is_null() {
                        xml_free(buf as _);
                        xml_err_memory(ctxt, null());
                        (*ctxt).instate = state;
                        return;
                    }
                    buf = new_buf;
                }
                memcpy(buf.add(len) as _, (*(*ctxt).input).cur as _, nbchar);
                len += nbchar;
                *buf.add(len) = 0;
            }
            if len > max_length {
                xml_fatal_err_msg_str(
                    ctxt,
                    XmlParserErrors::XmlErrCommentNotFinished,
                    c"Comment too big found".as_ptr() as _,
                    null(),
                );
                xml_free(buf as _);
                return;
            }
            (*(*ctxt).input).cur = input;
            if *input == 0xA {
                input = input.add(1);
                (*(*ctxt).input).line += 1;
                (*(*ctxt).input).col = 1;
            }
            if *input == 0xD {
                input = input.add(1);
                if *input == 0xA {
                    (*(*ctxt).input).cur = input;
                    input = input.add(1);
                    (*(*ctxt).input).line += 1;
                    (*(*ctxt).input).col = 1;
                    // goto get_more;
                    continue 'get_more;
                }
                // input = input.sub(1);
            }
            (*ctxt).shrink();
            (*ctxt).grow();
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                xml_free(buf as _);
                return;
            }
            input = (*(*ctxt).input).cur;
            if *input == b'-' {
                if *input.add(1) == b'-' {
                    if *input.add(2) == b'>' {
                        if (*(*ctxt).input).id != inputid {
                            xml_fatal_err_msg(
                                ctxt,
                                XmlParserErrors::XmlErrEntityBoundary,
                                c"comment doesn't start and stop input the same entity\n".as_ptr()
                                    as _,
                            );
                        }
                        (*ctxt).advance(3);
                        if !(*ctxt).sax.is_null()
                            && (*(*ctxt).sax).comment.is_some()
                            && (*ctxt).disable_sax == 0
                        {
                            if !buf.is_null() {
                                ((*(*ctxt).sax).comment.unwrap())((*ctxt).user_data.clone(), buf);
                            } else {
                                ((*(*ctxt).sax).comment.unwrap())(
                                    (*ctxt).user_data.clone(),
                                    c"".as_ptr() as _,
                                );
                            }
                        }
                        if !buf.is_null() {
                            xml_free(buf as _);
                        }
                        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                            (*ctxt).instate = state;
                        }
                        return;
                    }
                    if !buf.is_null() {
                        xml_fatal_err_msg_str(
                            ctxt,
                            XmlParserErrors::XmlErrHyphenInComment,
                            c"Double hyphen within comment: <!--%.50s\n".as_ptr() as _,
                            buf,
                        );
                    } else {
                        xml_fatal_err_msg_str(
                            ctxt,
                            XmlParserErrors::XmlErrHyphenInComment,
                            c"Double hyphen within comment\n".as_ptr() as _,
                            null(),
                        );
                    }
                    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                        xml_free(buf as _);
                        return;
                    }
                    input = input.add(1);
                    (*(*ctxt).input).col += 1;
                }
                input = input.add(1);
                (*(*ctxt).input).col += 1;
                // goto get_more;
                continue 'get_more;
            }

            break 'get_more;
        }

        (*input >= 0x20 && *input <= 0x7F) || *input == 0x09 || *input == 0x0a
    } {}
    xml_parse_comment_complex(ctxt, buf, len, size);
    (*ctxt).instate = state;
}

// List of XML prefixed PI allowed by W3C specs
const XML_W3_CPIS: &[*const c_char] = &[
    c"xml-stylesheet".as_ptr() as _,
    c"xml-model".as_ptr() as _,
    null(),
];

/// Parse the name of a PI
///
/// `[17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))`
///
/// Returns the PITarget name or NULL
#[doc(alias = "xmlParsePITarget")]
pub(crate) unsafe extern "C" fn xml_parse_pi_target(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    let name: *const XmlChar = xml_parse_name(ctxt);
    if !name.is_null()
        && (*name.add(0) == b'x' || *name.add(0) == b'X')
        && (*name.add(1) == b'm' || *name.add(1) == b'M')
        && (*name.add(2) == b'l' || *name.add(2) == b'L')
    {
        if *name.add(0) == b'x' && *name.add(1) == b'm' && *name.add(2) == b'l' && *name.add(3) == 0
        {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrReservedXmlName,
                c"XML declaration allowed only at the start of the document\n".as_ptr() as _,
            );
            return name;
        } else if *name.add(3) == 0 {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrReservedXmlName, null());
            return name;
        }
        for &pis in XML_W3_CPIS {
            if pis.is_null() {
                break;
            }
            if xml_str_equal(name, pis as *const XmlChar) {
                return name;
            }
        }
        xml_warning_msg(
            ctxt,
            XmlParserErrors::XmlErrReservedXmlName,
            c"xmlParsePITarget: invalid name prefix 'xml'\n".as_ptr() as _,
            null(),
            null(),
        );
    }
    if !name.is_null() && !xml_strchr(name, b':').is_null() {
        xml_ns_err(
            ctxt,
            XmlParserErrors::XmlNsErrColon,
            c"colons are forbidden from PI names '%s'\n".as_ptr() as _,
            name,
            null(),
            null(),
        );
    }
    name
}

/// Parse an XML Catalog Processing Instruction.
///
/// `<?oasis-xml-catalog catalog="http://example.com/catalog.xml"?>`
///
/// Occurs only if allowed by the user and if happening in the Misc
/// part of the document before any doctype information.  
/// This will add the given catalog to the parsing context in order
/// to be used if there is a resolution need further down in the document.
#[doc(alias = "xmlParseCatalogPI")]
#[cfg(feature = "catalog")]
unsafe extern "C" fn xml_parse_catalog_pi(ctxt: XmlParserCtxtPtr, catalog: *const XmlChar) {
    let mut url: *mut XmlChar = null_mut();
    let mut tmp: *const XmlChar;
    let base: *const XmlChar;
    let marker: XmlChar;

    tmp = catalog;
    while xml_is_blank_char(*tmp as u32) {
        tmp = tmp.add(1);
    }
    'error: {
        if xml_strncmp(tmp, c"catalog".as_ptr() as _, 7) != 0 {
            break 'error;
        }
        tmp = tmp.add(7);
        while xml_is_blank_char(*tmp as u32) {
            tmp = tmp.add(1);
        }
        if *tmp != b'=' {
            return;
        }
        tmp = tmp.add(1);
        while xml_is_blank_char(*tmp as u32) {
            tmp = tmp.add(1);
        }
        marker = *tmp;
        if marker != b'\'' && marker != b'"' {
            break 'error;
        }
        tmp = tmp.add(1);
        base = tmp;
        while *tmp != 0 && *tmp != marker {
            tmp = tmp.add(1);
        }
        if *tmp == 0 {
            break 'error;
        }
        url = xml_strndup(base, tmp.offset_from(base) as _);
        tmp = tmp.add(1);
        while xml_is_blank_char(*tmp as u32) {
            tmp = tmp.add(1);
        }
        if *tmp != 0 {
            break 'error;
        }
        if !url.is_null() {
            (*ctxt).catalogs = xml_catalog_add_local((*ctxt).catalogs, url);
            xml_free(url as _);
        }
        return;
    }

    //  error:
    xml_warning_msg(
        ctxt,
        XmlParserErrors::XmlWarCatalogPI,
        c"Catalog PI syntax error: %s\n".as_ptr() as _,
        catalog,
        null(),
    );
    if !url.is_null() {
        xml_free(url as _);
    }
}

/// Parse an XML Processing Instruction.
///
/// `[16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'`
///
/// The processing is transferred to SAX once parsed.
#[doc(alias = "xmlParsePI")]
pub(crate) unsafe extern "C" fn xml_parse_pi(ctxt: XmlParserCtxtPtr) {
    let mut buf: *mut XmlChar;
    let mut len: usize = 0;
    let mut size: usize = XML_PARSER_BUFFER_SIZE;
    let max_length: usize = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH
    } else {
        XML_MAX_TEXT_LENGTH
    };
    let mut l: i32 = 0;
    let target: *const XmlChar;
    let state: XmlParserInputState;

    if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'?' {
        let inputid: i32 = (*(*ctxt).input).id;
        state = (*ctxt).instate;
        (*ctxt).instate = XmlParserInputState::XmlParserPI;
        /*
         * this is a Processing Instruction.
         */
        (*ctxt).advance(2);

        /*
         * Parse the target name and check for special support like
         * namespace.
         */
        target = xml_parse_pi_target(ctxt);
        if !target.is_null() {
            if (*ctxt).current_byte() == b'?' && NXT!(ctxt, 1) == b'>' {
                if inputid != (*(*ctxt).input).id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        c"PI declaration doesn't start and stop in the same entity\n".as_ptr() as _,
                    );
                }
                (*ctxt).advance(2);

                /*
                 * SAX: PI detected.
                 */
                if !(*ctxt).sax.is_null()
                    && (*ctxt).disable_sax == 0
                    && (*(*ctxt).sax).processing_instruction.is_some()
                {
                    ((*(*ctxt).sax).processing_instruction.unwrap())(
                        (*ctxt).user_data.clone(),
                        target,
                        null(),
                    );
                }
                if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    (*ctxt).instate = state;
                }
                return;
            }
            buf = xml_malloc_atomic(size) as *mut XmlChar;
            if buf.is_null() {
                xml_err_memory(ctxt, null());
                (*ctxt).instate = state;
                return;
            }
            if (*ctxt).skip_blanks() == 0 {
                xml_fatal_err_msg_str(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    c"ParsePI: PI %s space expected\n".as_ptr() as _,
                    target,
                );
            }
            let mut cur = (*ctxt).current_char(&mut l).unwrap_or('\0');
            while xml_is_char(cur as u32) && (cur != '?' || NXT!(ctxt, 1) != b'>') {
                if len + 5 >= size {
                    let new_size: usize = size * 2;
                    let tmp: *mut XmlChar = xml_realloc(buf as _, new_size) as *mut XmlChar;
                    if tmp.is_null() {
                        xml_err_memory(ctxt, null());
                        xml_free(buf as _);
                        (*ctxt).instate = state;
                        return;
                    }
                    buf = tmp;
                    size = new_size;
                }
                COPY_BUF!(l, buf, len, cur);
                if len > max_length {
                    xml_fatal_err_msg_str(
                        ctxt,
                        XmlParserErrors::XmlErrPINotFinished,
                        c"PI %s too big found".as_ptr() as _,
                        target,
                    );
                    xml_free(buf as _);
                    (*ctxt).instate = state;
                    return;
                }
                NEXTL!(ctxt, l);
                cur = (*ctxt).current_char(&mut l).unwrap_or('\0');
            }
            *buf.add(len) = 0;
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                xml_free(buf as _);
                return;
            }
            if cur != '?' {
                xml_fatal_err_msg_str(
                    ctxt,
                    XmlParserErrors::XmlErrPINotFinished,
                    c"ParsePI: PI %s never end ...\n".as_ptr() as _,
                    target,
                );
            } else {
                if inputid != (*(*ctxt).input).id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        c"PI declaration doesn't start and stop in the same entity\n".as_ptr() as _,
                    );
                }
                (*ctxt).advance(2);

                #[cfg(feature = "catalog")]
                if matches!(
                    state,
                    XmlParserInputState::XmlParserMisc | XmlParserInputState::XmlParserStart
                ) && xml_str_equal(target, XML_CATALOG_PI.as_ptr() as _)
                {
                    let allow: XmlCatalogAllow = xml_catalog_get_defaults();
                    if matches!(allow, XmlCatalogAllow::Document | XmlCatalogAllow::All) {
                        xml_parse_catalog_pi(ctxt, buf);
                    }
                }

                /*
                 * SAX: PI detected.
                 */
                if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                    if let Some(pi) = (*(*ctxt).sax).processing_instruction {
                        pi((*ctxt).user_data.clone(), target, buf);
                    }
                }
            }
            xml_free(buf as _);
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrPINotStarted, null());
        }
        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            (*ctxt).instate = state;
        }
    }
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
pub(crate) unsafe extern "C" fn xml_parse_default_decl(
    ctxt: XmlParserCtxtPtr,
    value: *mut *mut XmlChar,
) -> i32 {
    let mut val: i32;

    *value = null_mut();
    if (*ctxt).content_bytes().starts_with(b"#REQUIRED") {
        (*ctxt).advance(9);
        return XmlAttributeDefault::XmlAttributeRequired as i32;
    }
    if (*ctxt).content_bytes().starts_with(b"#IMPLIED") {
        (*ctxt).advance(8);
        return XmlAttributeDefault::XmlAttributeImplied as i32;
    }
    val = XmlAttributeDefault::XmlAttributeNone as i32;
    if (*ctxt).content_bytes().starts_with(b"#FIXED") {
        (*ctxt).advance(6);
        val = XmlAttributeDefault::XmlAttributeFixed as i32;
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                c"Space required after '#FIXED'\n".as_ptr() as _,
            );
        }
    }
    let ret: *mut XmlChar = xml_parse_att_value(ctxt);
    (*ctxt).instate = XmlParserInputState::XmlParserDTD;
    if ret.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::try_from((*ctxt).err_no).unwrap(),
            c"Attribute default value declaration error\n".as_ptr() as _,
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
pub(crate) unsafe extern "C" fn xml_parse_notation_type(
    ctxt: XmlParserCtxtPtr,
) -> XmlEnumerationPtr {
    let mut name: *const XmlChar;
    let mut ret: XmlEnumerationPtr = null_mut();
    let mut last: XmlEnumerationPtr = null_mut();
    let mut cur: XmlEnumerationPtr;
    let mut tmp: XmlEnumerationPtr;

    if (*ctxt).current_byte() != b'(' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotStarted, null());
        return null_mut();
    }
    while {
        (*ctxt).skip_char();
        (*ctxt).skip_blanks();
        name = xml_parse_name(ctxt);
        if name.is_null() {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                c"Name expected in NOTATION declaration\n".as_ptr() as _,
            );
            xml_free_enumeration(ret);
            return null_mut();
        }
        tmp = ret;
        while !tmp.is_null() {
            if Some(CStr::from_ptr(name as *const i8).to_string_lossy()).as_deref()
                == (*tmp).name.as_deref()
            {
                xml_validity_error(
                    ctxt,
                    XmlParserErrors::XmlDTDDupToken,
                    c"standalone: attribute notation value token %s duplicated\n".as_ptr() as _,
                    name,
                    null(),
                );
                if xml_dict_owns((*ctxt).dict, name) == 0 {
                    xml_free(name as _);
                }
                break;
            }
            tmp = (*tmp).next;
        }
        if tmp.is_null() {
            cur = xml_create_enumeration(
                Some(CStr::from_ptr(name as *const i8).to_string_lossy()).as_deref(),
            );
            if cur.is_null() {
                xml_free_enumeration(ret);
                return null_mut();
            }
            if last.is_null() {
                ret = cur;
                last = cur;
            } else {
                (*last).next = cur;
                last = cur;
            }
        }
        (*ctxt).skip_blanks();

        (*ctxt).current_byte() == b'|'
    } {}
    if (*ctxt).current_byte() != b')' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotFinished, null());
        xml_free_enumeration(ret);
        return null_mut();
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
pub(crate) unsafe extern "C" fn xml_parse_enumeration_type(
    ctxt: XmlParserCtxtPtr,
) -> XmlEnumerationPtr {
    let mut name: *mut XmlChar;
    let mut ret: XmlEnumerationPtr = null_mut();
    let mut last: XmlEnumerationPtr = null_mut();
    let mut cur: XmlEnumerationPtr;
    let mut tmp: XmlEnumerationPtr;

    if (*ctxt).current_byte() != b'(' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttlistNotStarted, null());
        return null_mut();
    }
    while {
        (*ctxt).skip_char();
        (*ctxt).skip_blanks();
        name = xml_parse_nmtoken(ctxt);
        if name.is_null() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNmtokenRequired, null());
            return ret;
        }
        tmp = ret;
        while !tmp.is_null() {
            if Some(CStr::from_ptr(name as *const i8).to_string_lossy()).as_deref()
                == (*tmp).name.as_deref()
            {
                xml_validity_error(
                    ctxt,
                    XmlParserErrors::XmlDTDDupToken,
                    c"standalone: attribute enumeration value token %s duplicated\n".as_ptr() as _,
                    name,
                    null(),
                );
                if xml_dict_owns((*ctxt).dict, name) == 0 {
                    xml_free(name as _);
                }
                break;
            }
            tmp = (*tmp).next;
        }
        if tmp.is_null() {
            cur = xml_create_enumeration(
                Some(CStr::from_ptr(name as *const i8).to_string_lossy()).as_deref(),
            );
            if xml_dict_owns((*ctxt).dict, name) == 0 {
                xml_free(name as _);
            }
            if cur.is_null() {
                xml_free_enumeration(ret);
                return null_mut();
            }
            if last.is_null() {
                ret = cur;
                last = cur;
            } else {
                (*last).next = cur;
                last = cur;
            }
        }
        (*ctxt).skip_blanks();

        (*ctxt).current_byte() == b'|'
    } {}
    if (*ctxt).current_byte() != b')' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttlistNotFinished, null());
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
pub(crate) unsafe extern "C" fn xml_parse_enumerated_type(
    ctxt: XmlParserCtxtPtr,
    tree: *mut XmlEnumerationPtr,
) -> i32 {
    if (*ctxt).content_bytes().starts_with(b"NOTATION") {
        (*ctxt).advance(8);
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                c"Space required after 'NOTATION'\n".as_ptr() as _,
            );
            return 0;
        }
        *tree = xml_parse_notation_type(ctxt);
        if (*tree).is_null() {
            return 0;
        }
        return XmlAttributeType::XmlAttributeNotation as i32;
    }
    *tree = xml_parse_enumeration_type(ctxt);
    if (*tree).is_null() {
        return 0;
    }
    XmlAttributeType::XmlAttributeEnumeration as i32
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
pub(crate) unsafe extern "C" fn xml_parse_attribute_type(
    ctxt: XmlParserCtxtPtr,
    tree: *mut XmlEnumerationPtr,
) -> i32 {
    if (*ctxt).content_bytes().starts_with(b"CDATA") {
        (*ctxt).advance(5);
        return XmlAttributeType::XmlAttributeCDATA as i32;
    } else if (*ctxt).content_bytes().starts_with(b"IDREFS") {
        (*ctxt).advance(6);
        return XmlAttributeType::XmlAttributeIDREFS as i32;
    } else if (*ctxt).content_bytes().starts_with(b"IDREF") {
        (*ctxt).advance(5);
        return XmlAttributeType::XmlAttributeIDREF as i32;
    } else if (*ctxt).current_byte() == b'I' && NXT!(ctxt, 1) == b'D' {
        (*ctxt).advance(2);
        return XmlAttributeType::XmlAttributeID as i32;
    } else if (*ctxt).content_bytes().starts_with(b"ENTITY") {
        (*ctxt).advance(6);
        return XmlAttributeType::XmlAttributeEntity as i32;
    } else if (*ctxt).content_bytes().starts_with(b"ENTITIES") {
        (*ctxt).advance(8);
        return XmlAttributeType::XmlAttributeEntities as i32;
    } else if (*ctxt).content_bytes().starts_with(b"NMTOKENS") {
        (*ctxt).advance(8);
        return XmlAttributeType::XmlAttributeNmtokens as i32;
    } else if (*ctxt).content_bytes().starts_with(b"NMTOKEN") {
        (*ctxt).advance(7);
        return XmlAttributeType::XmlAttributeNmtoken as i32;
    }
    xml_parse_enumerated_type(ctxt, tree)
}

/// Add a defaulted attribute for an element
#[doc(alias = "xmlAddDefAttrs")]
pub(crate) unsafe extern "C" fn xml_add_def_attrs(
    ctxt: XmlParserCtxtPtr,
    fullname: *const XmlChar,
    fullattr: *const XmlChar,
    mut value: *const XmlChar,
) {
    let mut defaults: XmlDefAttrsPtr;
    let mut len: i32 = 0;
    let mut name: *const XmlChar;
    let mut prefix: *const XmlChar;

    /*
     * Allows to detect attribute redefinitions
     */
    if !(*ctxt).atts_special.is_null()
        && !xml_hash_lookup2((*ctxt).atts_special, fullname, fullattr).is_null()
    {
        return;
    }

    'mem_error: {
        if (*ctxt).atts_default.is_null() {
            (*ctxt).atts_default = xml_hash_create_dict(10, (*ctxt).dict);
            if (*ctxt).atts_default.is_null() {
                break 'mem_error;
            }
        }

        /*
         * split the element name into prefix:localname , the string found
         * are within the DTD and then not associated to namespace names.
         */
        name = xml_split_qname3(fullname, addr_of_mut!(len));
        if name.is_null() {
            name = xml_dict_lookup((*ctxt).dict, fullname, -1);
            prefix = null_mut();
        } else {
            name = xml_dict_lookup((*ctxt).dict, name, -1);
            prefix = xml_dict_lookup((*ctxt).dict, fullname, len);
        }

        /*
         * make sure there is some storage
         */
        defaults = xml_hash_lookup2((*ctxt).atts_default, name, prefix) as _;
        if defaults.is_null() {
            defaults =
                xml_malloc(size_of::<XmlDefAttrs>() + (4 * 5) * size_of::<*const XmlChar>()) as _;
            if defaults.is_null() {
                break 'mem_error;
            }
            (*defaults).nb_attrs = 0;
            (*defaults).max_attrs = 4;
            if xml_hash_update_entry2((*ctxt).atts_default, name, prefix, defaults as _, None) < 0 {
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
            if xml_hash_update_entry2((*ctxt).atts_default, name, prefix, defaults as _, None) < 0 {
                xml_free(defaults as _);
                break 'mem_error;
            }
        }

        /*
         * Split the element name into prefix:localname , the string found
         * are within the DTD and hen not associated to namespace names.
         */
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
        /* intern the string and precompute the end */
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

    xml_err_memory(ctxt, null());
}

/// Register this attribute type
#[doc(alias = "xmlAddSpecialAttr")]
pub(crate) unsafe extern "C" fn xml_add_special_attr(
    ctxt: XmlParserCtxtPtr,
    fullname: *const XmlChar,
    fullattr: *const XmlChar,
    typ: i32,
) {
    if (*ctxt).atts_special.is_null() {
        (*ctxt).atts_special = xml_hash_create_dict(10, (*ctxt).dict);
        if (*ctxt).atts_special.is_null() {
            // goto mem_error;
            xml_err_memory(ctxt, null());
            return;
        }
    }

    if !xml_hash_lookup2((*ctxt).atts_special, fullname, fullattr).is_null() {
        return;
    }

    xml_hash_add_entry2((*ctxt).atts_special, fullname, fullattr, typ as isize as _);

    // mem_error:
    // xmlErrMemory(ctxt, null());
    // return;
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
pub(crate) unsafe extern "C" fn xml_parse_element_mixed_content_decl(
    ctxt: XmlParserCtxtPtr,
    inputchk: i32,
) -> XmlElementContentPtr {
    let mut ret: XmlElementContentPtr = null_mut();
    let mut cur: XmlElementContentPtr = null_mut();
    let mut n: XmlElementContentPtr;
    let mut elem: *const XmlChar = null();

    (*ctxt).grow();
    if (*ctxt).content_bytes().starts_with(b"#PCDATA") {
        (*ctxt).advance(7);
        (*ctxt).skip_blanks();
        if (*ctxt).current_byte() == b')' {
            if (*(*ctxt).input).id != inputchk {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    c"Element content declaration doesn't start and stop in the same entity\n"
                        .as_ptr() as _,
                );
            }
            (*ctxt).skip_char();
            ret = xml_new_doc_element_content(
                (*ctxt).my_doc,
                null(),
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
                null(),
                XmlElementContentType::XmlElementContentPCDATA,
            );
            cur = ret;
            if ret.is_null() {
                return null_mut();
            }
        }
        #[allow(clippy::while_immutable_condition)]
        while (*ctxt).current_byte() == b'|'
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            (*ctxt).skip_char();
            if elem.is_null() {
                ret = xml_new_doc_element_content(
                    (*ctxt).my_doc,
                    null(),
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
            } else {
                n = xml_new_doc_element_content(
                    (*ctxt).my_doc,
                    null(),
                    XmlElementContentType::XmlElementContentOr,
                );
                if n.is_null() {
                    xml_free_doc_element_content((*ctxt).my_doc, ret);
                    return null_mut();
                }
                (*n).c1 = xml_new_doc_element_content(
                    (*ctxt).my_doc,
                    elem,
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
            }
            (*ctxt).skip_blanks();
            elem = xml_parse_name(ctxt);
            if elem.is_null() {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    c"xmlParseElementMixedContentDecl : Name expected\n".as_ptr() as _,
                );
                xml_free_doc_element_content((*ctxt).my_doc, ret);
                return null_mut();
            }
            (*ctxt).skip_blanks();
            (*ctxt).grow();
        }
        if (*ctxt).current_byte() == b')' && NXT!(ctxt, 1) == b'*' {
            if !elem.is_null() {
                (*cur).c2 = xml_new_doc_element_content(
                    (*ctxt).my_doc,
                    elem,
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
                    c"Element content declaration doesn't start and stop in the same entity\n"
                        .as_ptr() as _,
                );
            }
            (*ctxt).advance(2);
        } else {
            xml_free_doc_element_content((*ctxt).my_doc, ret);
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrMixedNotStarted, null());
            return null_mut();
        }
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrPCDATARequired, null());
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
pub(crate) unsafe extern "C" fn xml_parse_element_children_content_decl(
    ctxt: XmlParserCtxtPtr,
    inputchk: i32,
) -> XmlElementContentPtr {
    /* stub left for API/ABI compat */
    xml_parse_element_children_content_decl_priv(ctxt, inputchk, 1)
}

/// Parse the declaration for an Element content either Mixed or Children,
/// the cases EMPTY and ANY are handled directly in xmlParseElementDecl
///
/// `[46] contentspec ::= 'EMPTY' | 'ANY' | Mixed | children`
///
/// returns: the type of element content XML_ELEMENT_TYPE_xxx
#[doc(alias = "xmlParseElementContentDecl")]
pub(crate) unsafe extern "C" fn xml_parse_element_content_decl(
    ctxt: XmlParserCtxtPtr,
    name: *const XmlChar,
    result: *mut XmlElementContentPtr,
) -> i32 {
    let tree: XmlElementContentPtr;
    let inputid: i32 = (*(*ctxt).input).id;
    let res: i32;

    *result = null_mut();

    if (*ctxt).current_byte() != b'(' {
        xml_fatal_err_msg_str(
            ctxt,
            XmlParserErrors::XmlErrElemcontentNotStarted,
            c"xmlParseElementContentDecl : %s '(' expected\n".as_ptr() as _,
            name,
        );
        return -1;
    }
    (*ctxt).skip_char();
    (*ctxt).grow();
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }
    (*ctxt).skip_blanks();
    if (*ctxt).content_bytes().starts_with(b"#PCDATA") {
        tree = xml_parse_element_mixed_content_decl(ctxt, inputid);
        res = XmlElementTypeVal::XmlElementTypeMixed as i32;
    } else {
        tree = xml_parse_element_children_content_decl_priv(ctxt, inputid, 1);
        res = XmlElementTypeVal::XmlElementTypeElement as i32;
    }
    (*ctxt).skip_blanks();
    *result = tree;
    res
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
pub(crate) unsafe extern "C" fn xml_parse_entity_ref(ctxt: XmlParserCtxtPtr) -> XmlEntityPtr {
    let mut ent: XmlEntityPtr = null_mut();

    (*ctxt).grow();
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }

    if (*ctxt).current_byte() != b'&' {
        return null_mut();
    }
    (*ctxt).skip_char();
    let name: *const XmlChar = xml_parse_name(ctxt);
    if name.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            c"xmlParseEntityRef: no name\n".as_ptr() as _,
        );
        return null_mut();
    }
    if (*ctxt).current_byte() != b';' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityRefSemicolMissing, null());
        return null_mut();
    }
    (*ctxt).skip_char();

    /*
     * Predefined entities override any extra definition
     */
    if (*ctxt).options & XmlParserOption::XmlParseOldsax as i32 == 0 {
        ent = xml_get_predefined_entity(name);
        if !ent.is_null() {
            return ent;
        }
    }

    /*
     * Ask first SAX for entity resolution, otherwise try the
     * entities which may have stored in the parser context.
     */
    if !(*ctxt).sax.is_null() {
        if let Some(f) = (*(*ctxt).sax).get_entity {
            ent = f((*ctxt).user_data.clone(), name);
        }
        if (*ctxt).well_formed == 1
            && ent.is_null()
            && (*ctxt).options & XmlParserOption::XmlParseOldsax as i32 != 0
        {
            ent = xml_get_predefined_entity(name);
        }
        if (*ctxt).well_formed == 1
            && ent.is_null()
            && (*ctxt)
                .user_data
                .as_ref()
                .and_then(|d| d.lock().downcast_ref::<XmlParserCtxtPtr>().copied())
                == Some(ctxt)
        {
            ent = xml_sax2_get_entity(Some(GenericErrorContext::new(ctxt)), name);
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }
    /*
     * [ WFC: Entity Declared ]
     * In a document without any DTD, a document with only an
     * internal DTD subset which contains no parameter entity
     * references, or a document with "standalone='yes'", the
     * Name given in the entity reference must match that in an
     * entity declaration, except that well-formed documents
     * need not declare any of the following entities: amp, lt,
     * gt, apos, quot.
     * The declaration of a parameter entity must precede any
     * reference to it.
     * Similarly, the declaration of a general entity must
     * precede any reference to it which appears in a default
     * value in an attribute-list declaration. Note that if
     * entities are declared in the external subset or in
     * external parameter entities, a non-validating processor
     * is not obligated to read and process their declarations;
     * for such documents, the rule that an entity must be
     * declared is a well-formedness constraint only if
     * standalone='yes'.
     */
    if ent.is_null() {
        if (*ctxt).standalone == 1 || ((*ctxt).has_external_subset == 0 && (*ctxt).has_perefs == 0)
        {
            xml_fatal_err_msg_str(
                ctxt,
                XmlParserErrors::XmlErrUndeclaredEntity,
                c"Entity '%s' not defined\n".as_ptr() as _,
                name,
            );
        } else {
            xml_err_msg_str(
                ctxt,
                XmlParserErrors::XmlWarUndeclaredEntity,
                c"Entity '%s' not defined\n".as_ptr() as _,
                name,
            );
            if (*ctxt).in_subset == 0 && !(*ctxt).sax.is_null() {
                if let Some(refe) = (*(*ctxt).sax).reference {
                    refe((*ctxt).user_data.clone(), name);
                }
            }
        }
        (*ctxt).valid = 0;
    }
    /*
     * [ WFC: Parsed Entity ]
     * An entity reference must not contain the name of an
     * unparsed entity
     */
    else if matches!(
        (*ent).etype,
        Some(XmlEntityType::XmlExternalGeneralUnparsedEntity)
    ) {
        xml_fatal_err_msg_str(
            ctxt,
            XmlParserErrors::XmlErrUnparsedEntity,
            c"Entity reference to unparsed entity %s\n".as_ptr() as _,
            name,
        );
    }
    /*
     * [ WFC: No External Entity References ]
     * Attribute values cannot contain direct or indirect
     * entity references to external entities.
     */
    else if matches!(
        (*ctxt).instate,
        XmlParserInputState::XmlParserAttributeValue
    ) && matches!(
        (*ent).etype,
        Some(XmlEntityType::XmlExternalGeneralParsedEntity)
    ) {
        xml_fatal_err_msg_str(
            ctxt,
            XmlParserErrors::XmlErrEntityIsExternal,
            c"Attribute references external entity '%s'\n".as_ptr() as _,
            name,
        );
    }
    /*
     * [ WFC: No < in Attribute Values ]
     * The replacement text of any entity referred to directly or
     * indirectly in an attribute value (other than "&lt;") must
     * not contain a <.
     */
    else if matches!(
        (*ctxt).instate,
        XmlParserInputState::XmlParserAttributeValue
    ) && !matches!(
        (*ent).etype,
        Some(XmlEntityType::XmlInternalPredefinedEntity)
    ) {
        if (*ent).flags & XML_ENT_CHECKED_LT as i32 == 0 {
            if !(*ent).content.load(Ordering::Relaxed).is_null()
                && !xml_strchr((*ent).content.load(Ordering::Relaxed) as _, b'<').is_null()
            {
                (*ent).flags |= XML_ENT_CONTAINS_LT as i32;
            }
            (*ent).flags |= XML_ENT_CHECKED_LT as i32;
        }
        if (*ent).flags & XML_ENT_CONTAINS_LT as i32 != 0 {
            xml_fatal_err_msg_str(
                ctxt,
                XmlParserErrors::XmlErrLtInAttribute,
                c"'<' in entity '%s' is not allowed in attributes values\n".as_ptr() as _,
                name,
            );
        }
    }
    /*
     * Internal check, no parameter entities here ...
     */
    else {
        match (*ent).etype {
            Some(XmlEntityType::XmlInternalParameterEntity)
            | Some(XmlEntityType::XmlExternalParameterEntity) => {
                xml_fatal_err_msg_str(
                    ctxt,
                    XmlParserErrors::XmlErrEntityIsParameter,
                    c"Attempt to reference the parameter entity '%s'\n".as_ptr() as _,
                    name,
                );
            }
            _ => {}
        }
    }

    /*
     * [ WFC: No Recursion ]
     * A parsed entity must not contain a recursive reference
     * to itself, either directly or indirectly.
     * Done somewhere else
     */
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
    lst: *mut XmlNodePtr,
) -> XmlParserErrors {
    let mut new_doc: XmlDocPtr = null_mut();
    let mut content: XmlNodePtr = null_mut();
    let mut last: XmlNodePtr = null_mut();
    let ret: XmlParserErrors;

    if ((*oldctxt).depth > 40 && (*oldctxt).options & XmlParserOption::XmlParseHuge as i32 == 0)
        || (*oldctxt).depth > 100
    {
        xml_fatal_err_msg(
            oldctxt,
            XmlParserErrors::XmlErrEntityLoop,
            c"Maximum entity nesting depth exceeded".as_ptr() as _,
        );
        return XmlParserErrors::XmlErrEntityLoop;
    }

    if !lst.is_null() {
        *lst = null_mut();
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
    (*ctxt).str_xml = xml_dict_lookup((*ctxt).dict, c"xml".as_ptr() as _, 3);
    (*ctxt).str_xmlns = xml_dict_lookup((*ctxt).dict, c"xmlns".as_ptr() as _, 5);
    (*ctxt).str_xml_ns = xml_dict_lookup((*ctxt).dict, XML_XML_NAMESPACE.as_ptr() as _, 36);

    /* propagate namespaces down the entity */
    for i in (0..(*oldctxt).ns_tab.len()).step_by(2) {
        (*ctxt).ns_push((*oldctxt).ns_tab[i], (*oldctxt).ns_tab[i + 1]);
    }

    let oldsax: XmlSAXHandlerPtr = (*ctxt).sax;
    (*ctxt).sax = (*oldctxt).sax;
    (*ctxt).detect_sax2();
    (*ctxt).replace_entities = (*oldctxt).replace_entities;
    (*ctxt).options = (*oldctxt).options;

    (*ctxt)._private = (*oldctxt)._private;
    if (*oldctxt).my_doc.is_null() {
        new_doc = xml_new_doc(Some("1.0"));
        if new_doc.is_null() {
            (*ctxt).sax = oldsax;
            (*ctxt).dict = null_mut();
            xml_free_parser_ctxt(ctxt);
            return XmlParserErrors::XmlErrInternalError;
        }
        (*new_doc).properties = XmlDocProperties::XmlDocInternal as i32;
        (*new_doc).dict = (*ctxt).dict;
        xml_dict_reference((*new_doc).dict);
        (*ctxt).my_doc = new_doc;
    } else {
        (*ctxt).my_doc = (*oldctxt).my_doc;
        content = (*(*ctxt).my_doc)
            .children
            .map_or(null_mut(), |c| c.as_ptr());
        last = (*(*ctxt).my_doc).last.map_or(null_mut(), |l| l.as_ptr());
    }
    let new_root: XmlNodePtr = xml_new_doc_node(
        (*ctxt).my_doc,
        null_mut(),
        c"pseudoroot".as_ptr() as _,
        null_mut(),
    );
    if new_root.is_null() {
        (*ctxt).sax = oldsax;
        (*ctxt).dict = null_mut();
        xml_free_parser_ctxt(ctxt);
        if !new_doc.is_null() {
            xml_free_doc(new_doc);
        }
        return XmlParserErrors::XmlErrInternalError;
    }
    (*(*ctxt).my_doc).children = None;
    (*(*ctxt).my_doc).last = None;
    (*(*ctxt).my_doc).add_child(new_root);
    (*ctxt).node_push(
        (*(*ctxt).my_doc)
            .children
            .map_or(null_mut(), |c| c.as_ptr()),
    );
    (*ctxt).instate = XmlParserInputState::XmlParserContent;
    (*ctxt).depth = (*oldctxt).depth;

    (*ctxt).validate = 0;
    (*ctxt).loadsubset = (*oldctxt).loadsubset;
    if (*oldctxt).validate != 0 || (*oldctxt).replace_entities != 0 {
        /*
         * ID/IDREF registration will be done in xmlValidateElement below
         */
        (*ctxt).loadsubset |= XML_SKIP_IDS as i32;
    }
    (*ctxt).dict_names = (*oldctxt).dict_names;
    (*ctxt).atts_default = (*oldctxt).atts_default;
    (*ctxt).atts_special = (*oldctxt).atts_special;

    xml_parse_content(ctxt);
    if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, null());
    } else if (*ctxt).current_byte() != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtraContent, null());
    }
    if NodePtr::from_ptr((*ctxt).node) != (*(*ctxt).my_doc).children {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, null());
    }

    if (*ctxt).well_formed == 0 {
        ret = XmlParserErrors::try_from((*ctxt).err_no).unwrap();
        (*oldctxt).err_no = (*ctxt).err_no;
        (*oldctxt).well_formed = 0;
        (*oldctxt).last_error = (*ctxt).last_error.clone();
    } else {
        ret = XmlParserErrors::XmlErrOK;
    }

    if !lst.is_null() && matches!(ret, XmlParserErrors::XmlErrOK) {
        // Return the newly created nodeset after unlinking it from
        // they pseudo parent.
        let mut cur = (*(*ctxt).my_doc)
            .children
            .unwrap()
            .children
            .map_or(null_mut(), |c| c.as_ptr());
        *lst = cur;
        while !cur.is_null() {
            #[cfg(feature = "libxml_valid")]
            if (*oldctxt).validate != 0
                && (*oldctxt).well_formed != 0
                && !(*oldctxt).my_doc.is_null()
                && !(*(*oldctxt).my_doc).int_subset.is_null()
                && (*cur).typ == XmlElementType::XmlElementNode
            {
                (*oldctxt).valid &=
                    xml_validate_element(addr_of_mut!((*oldctxt).vctxt), (*oldctxt).my_doc, cur);
            }
            (*cur).parent = None;
            cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
        }
        (*(*ctxt).my_doc).children.unwrap().children = None;
    }
    if !(*ctxt).my_doc.is_null() {
        xml_free_node(
            (*(*ctxt).my_doc)
                .children
                .map_or(null_mut(), |c| c.as_ptr()),
        );
        (*(*ctxt).my_doc).children = NodePtr::from_ptr(content);
        (*(*ctxt).my_doc).last = NodePtr::from_ptr(last);
    }

    /*
     * Also record the size of the entity parsed
     */
    if !(*ctxt).input.is_null() && !oldctxt.is_null() {
        let mut consumed: u64 = (*(*ctxt).input).consumed;
        consumed =
            consumed.saturating_add((*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _);

        (*oldctxt).sizeentcopy = (*oldctxt).sizeentcopy.saturating_add(consumed);
        (*oldctxt).sizeentcopy = (*oldctxt).sizeentcopy.saturating_add((*ctxt).sizeentcopy);
    }

    (*oldctxt).nb_errors = (*ctxt).nb_errors;
    (*oldctxt).nb_warnings = (*ctxt).nb_warnings;
    (*ctxt).sax = oldsax as _;
    (*ctxt).dict = null_mut();
    (*ctxt).atts_default = null_mut();
    (*ctxt).atts_special = null_mut();
    xml_free_parser_ctxt(ctxt);
    if !new_doc.is_null() {
        xml_free_doc(new_doc);
    }

    ret
}

/**
 * xmlAddEntityReference:
 * @ent : A valid entity
 * @firstNode : A valid first node for children of entity
 * @lastNode : A valid last node of children entity
 *
 * Notify of a reference to an entity of type xmlEntityType::XML_EXTERNAL_GENERAL_PARSED_ENTITY
 */
#[cfg(feature = "libxml_legacy")]
unsafe extern "C" fn xml_add_entity_reference(
    ent: XmlEntityPtr,
    first_node: XmlNodePtr,
    last_node: XmlNodePtr,
) {
    if let Some(func) = XML_ENTITY_REF_FUNC {
        func(ent, first_node, last_node);
    }
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
pub(crate) unsafe extern "C" fn xml_parse_reference(ctxt: XmlParserCtxtPtr) {
    let val: *mut XmlChar;
    let mut was_checked: i32;
    let mut list: XmlNodePtr = null_mut();
    let mut ret: XmlParserErrors;

    if (*ctxt).current_byte() != b'&' {
        return;
    }

    /*
     * Simple case of a CharRef
     */
    if NXT!(ctxt, 1) == b'#' {
        let mut i: i32 = 0;
        let mut out: [XmlChar; 16] = [0; 16];
        let hex: i32 = NXT!(ctxt, 2) as _;
        let value: i32 = xml_parse_char_ref(ctxt);

        if value == 0 {
            return;
        }
        if (*ctxt).charset != XmlCharEncoding::UTF8 {
            /*
             * So we are using non-UTF-8 buffers
             * Check that the c_char fit on 8bits, if not
             * generate a CharRef.
             */
            if value <= 0xFF {
                out[0] = value as _;
                out[1] = 0;
                if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                    if let Some(characters) = (*(*ctxt).sax).characters {
                        characters((*ctxt).user_data.clone(), out.as_ptr() as _, 1);
                    }
                }
            } else {
                if hex == b'x' as i32 || hex == b'X' as i32 {
                    snprintf(
                        out.as_mut_ptr() as _,
                        out.len(),
                        c"#x%X".as_ptr() as _,
                        value,
                    );
                } else {
                    snprintf(
                        out.as_mut_ptr() as _,
                        out.len(),
                        c"#%d".as_ptr() as _,
                        value,
                    );
                }
                if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                    if let Some(refe) = (*(*ctxt).sax).reference {
                        refe((*ctxt).user_data.clone(), out.as_ptr() as _);
                    }
                }
            }
        } else {
            /*
             * Just encode the value in UTF-8
             */
            COPY_BUF!(0, out.as_mut_ptr(), i, value);
            out[i as usize] = 0;
            if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                if let Some(characters) = (*(*ctxt).sax).characters {
                    characters((*ctxt).user_data.clone(), out.as_ptr() as _, i);
                }
            }
        }
        return;
    }

    /*
     * We are seeing an entity reference
     */
    let ent: XmlEntityPtr = xml_parse_entity_ref(ctxt);
    if ent.is_null() {
        return;
    }
    if (*ctxt).well_formed == 0 {
        return;
    }
    was_checked = (*ent).flags & XML_ENT_PARSED as i32;

    /* special case of predefined entities */
    if (*ent).name.load(Ordering::Relaxed).is_null()
        || matches!(
            (*ent).etype,
            Some(XmlEntityType::XmlInternalPredefinedEntity)
        )
    {
        val = (*ent).content.load(Ordering::Relaxed) as _;
        if val.is_null() {
            return;
        }
        /*
         * inline the entity.
         */
        if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
            if let Some(characters) = (*(*ctxt).sax).characters {
                characters((*ctxt).user_data.clone(), val, xml_strlen(val));
            }
        }
        return;
    }

    /*
     * The first reference to the entity trigger a parsing phase
     * where the (*ent).children is filled with the result from
     * the parsing.
     * Note: external parsed entities will not be loaded, it is not
     * required for a non-validating parser, unless the parsing option
     * of validating, or substituting entities were given. Doing so is
     * far more secure as the parser will only process data coming from
     * the document entity by default.
     */
    if (*ent).flags & XML_ENT_PARSED as i32 == 0
        && (!matches!(
            (*ent).etype,
            Some(XmlEntityType::XmlExternalGeneralParsedEntity)
        ) || (*ctxt).options
            & (XmlParserOption::XmlParseNoent as i32 | XmlParserOption::XmlParseDtdvalid as i32)
            != 0)
    {
        let oldsizeentcopy: u64 = (*ctxt).sizeentcopy;

        /*
         * This is a bit hackish but this seems the best
         * way to make sure both SAX and DOM entity support
         * behaves okay.
         */
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

        /* Avoid overflow as much as possible */
        (*ctxt).sizeentcopy = 0;

        if (*ent).flags & XML_ENT_EXPANDING as i32 != 0 {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, null());
            (*ctxt).halt();
            return;
        }

        (*ent).flags |= XML_ENT_EXPANDING as i32;

        /*
         * Check that this entity is well formed
         * 4.3.2: An internal general parsed entity is well-formed
         * if its replacement text matches the production labeled
         * content.
         */
        if matches!((*ent).etype, Some(XmlEntityType::XmlInternalGeneralEntity)) {
            (*ctxt).depth += 1;
            ret = xml_parse_balanced_chunk_memory_internal(
                ctxt,
                (*ent).content.load(Ordering::Relaxed),
                user_data,
                addr_of_mut!(list),
            );
            (*ctxt).depth -= 1;
        } else if matches!(
            (*ent).etype,
            Some(XmlEntityType::XmlExternalGeneralParsedEntity)
        ) {
            (*ctxt).depth += 1;
            ret = xml_parse_external_entity_private(
                (*ctxt).my_doc,
                ctxt,
                (*ctxt).sax,
                user_data,
                (*ctxt).depth,
                (*ent).uri.load(Ordering::Relaxed) as _,
                (*ent).external_id.load(Ordering::Relaxed) as _,
                addr_of_mut!(list),
            );
            (*ctxt).depth -= 1;
        } else {
            ret = XmlParserErrors::XmlErrEntityPEInternal;
            xml_err_msg_str(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                c"invalid entity type found\n".as_ptr() as _,
                null(),
            );
        }

        (*ent).flags &= !XML_ENT_EXPANDING as i32;
        (*ent).flags |= (XML_ENT_PARSED | XML_ENT_CHECKED) as i32;
        (*ent).expanded_size = (*ctxt).sizeentcopy;
        if matches!(ret, XmlParserErrors::XmlErrEntityLoop) {
            (*ctxt).halt();
            xml_free_node_list(list);
            return;
        }
        if xml_parser_entity_check(ctxt, oldsizeentcopy) != 0 {
            xml_free_node_list(list);
            return;
        }

        if matches!(ret, XmlParserErrors::XmlErrOK) && !list.is_null() {
            (*ent).children.store(list, Ordering::Relaxed);
            /*
             * Prune it directly in the generated document
             * except for single text nodes.
             */
            if (*ctxt).replace_entities == 0
                || matches!((*ctxt).parse_mode, XmlParserMode::XmlParseReader)
                || (matches!((*list).typ, XmlElementType::XmlTextNode) && (*list).next.is_none())
            {
                (*ent).owner = 1;
                while !list.is_null() {
                    (*list).parent = NodePtr::from_ptr(ent as *mut XmlNode);
                    if (*list).doc != (*ent).doc.load(Ordering::Relaxed) as _ {
                        (*list).set_doc((*ent).doc.load(Ordering::Relaxed));
                    }
                    if (*list).next.is_none() {
                        (*ent).last.store(list, Ordering::Relaxed);
                    }
                    list = (*list).next.map_or(null_mut(), |n| n.as_ptr());
                }
                list = null_mut();
            } else {
                (*ent).owner = 0;
                while !list.is_null() {
                    (*list).parent = NodePtr::from_ptr((*ctxt).node);
                    (*list).doc = (*ctxt).my_doc;
                    if (*list).next.is_none() {
                        (*ent).last.store(list, Ordering::Relaxed);
                    }
                    list = (*list).next.map_or(null_mut(), |n| n.as_ptr());
                }
                list = (*ent).children.load(Ordering::Relaxed) as _;
                #[cfg(feature = "libxml_legacy")]
                if matches!(
                    (*ent).etype,
                    Some(XmlEntityType::XmlExternalGeneralParsedEntity)
                ) {
                    xml_add_entity_reference(ent, list, null_mut());
                }
            }
        } else if !matches!(
            ret,
            XmlParserErrors::XmlErrOK | XmlParserErrors::XmlWarUndeclaredEntity
        ) {
            xml_fatal_err_msg_str(
                ctxt,
                XmlParserErrors::XmlErrUndeclaredEntity,
                c"Entity '%s' failed to parse\n".as_ptr() as _,
                (*ent).name.load(Ordering::Relaxed) as _,
            );
            if !(*ent).content.load(Ordering::Relaxed).is_null() {
                *(*ent).content.load(Ordering::Relaxed).add(0) = 0;
            }
        } else if !list.is_null() {
            xml_free_node_list(list);
            list = null_mut();
        }

        /* Prevent entity from being parsed and expanded twice (Bug 760367). */
        was_checked = 0;
    }

    /*
     * Now that the entity content has been gathered
     * provide it to the application, this can take different forms based
     * on the parsing modes.
     */
    if (*ent).children.load(Ordering::Relaxed).is_null() {
        /*
         * Probably running in SAX mode and the callbacks don't
         * build the entity content. So unless we already went
         * though parsing for first checking go though the entity
         * content to generate callbacks associated to the entity
         */
        if was_checked != 0 {
            /*
             * This is a bit hackish but this seems the best
             * way to make sure both SAX and DOM entity support
             * behaves okay.
             */
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

            if matches!((*ent).etype, Some(XmlEntityType::XmlInternalGeneralEntity)) {
                (*ctxt).depth += 1;
                ret = xml_parse_balanced_chunk_memory_internal(
                    ctxt,
                    (*ent).content.load(Ordering::Relaxed),
                    user_data,
                    null_mut(),
                );
                (*ctxt).depth -= 1;
            } else if matches!(
                (*ent).etype,
                Some(XmlEntityType::XmlExternalGeneralParsedEntity)
            ) {
                let oldsizeentities: u64 = (*ctxt).sizeentities;

                (*ctxt).depth += 1;
                ret = xml_parse_external_entity_private(
                    (*ctxt).my_doc,
                    ctxt,
                    (*ctxt).sax,
                    user_data,
                    (*ctxt).depth,
                    (*ent).uri.load(Ordering::Relaxed) as _,
                    (*ent).external_id.load(Ordering::Relaxed) as _,
                    null_mut(),
                );
                (*ctxt).depth -= 1;

                /* Undo the change to sizeentities */
                (*ctxt).sizeentities = oldsizeentities;
            } else {
                ret = XmlParserErrors::XmlErrEntityPEInternal;
                xml_err_msg_str(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    c"invalid entity type found\n".as_ptr() as _,
                    null(),
                );
            }
            if matches!(ret, XmlParserErrors::XmlErrEntityLoop) {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, null());
                return;
            }
            if xml_parser_entity_check(ctxt, 0) != 0 {
                return;
            }
        }
        if !(*ctxt).sax.is_null() && (*ctxt).replace_entities == 0 && (*ctxt).disable_sax == 0 {
            if let Some(refe) = (*(*ctxt).sax).reference {
                /*
                 * Entity reference callback comes second, it's somewhat
                 * superfluous but a compatibility to historical behaviour
                 */
                refe(
                    (*ctxt).user_data.clone(),
                    (*ent).name.load(Ordering::Relaxed) as _,
                );
            }
        }
        return;
    }

    /*
     * We also check for amplification if entities aren't substituted.
     * They might be expanded later.
     */
    if was_checked != 0 && xml_parser_entity_check(ctxt, (*ent).expanded_size) != 0 {
        return;
    }

    /*
     * If we didn't get any children for the entity being built
     */
    if !(*ctxt).sax.is_null() && (*ctxt).replace_entities == 0 && (*ctxt).disable_sax == 0 {
        if let Some(refe) = (*(*ctxt).sax).reference {
            /*
             * Create a node.
             */
            refe(
                (*ctxt).user_data.clone(),
                (*ent).name.load(Ordering::Relaxed) as _,
            );
            return;
        }
    }

    if (*ctxt).replace_entities != 0 {
        /*
         * There is a problem on the handling of _private for entities
         * (bug 155816): Should we copy the content of the field from
         * the entity (possibly overwriting some value set by the user
         * when a copy is created), should we leave it alone, or should
         * we try to take care of different situations?  The problem
         * is exacerbated by the usage of this field by the xmlReader.
         * To fix this bug, we look at _private on the created node
         * and, if it's NULL, we copy in whatever was in the entity.
         * If it's not NULL we leave it alone.  This is somewhat of a
         * hack - maybe we should have further tests to determine
         * what to do.
         */
        if !(*ctxt).node.is_null() {
            /*
             * Seems we are generating the DOM content, do
             * a simple tree copy for all references except the first
             * In the first occurrence list contains the replacement.
             */
            if (list.is_null() && (*ent).owner == 0)
                || matches!((*ctxt).parse_mode, XmlParserMode::XmlParseReader)
            {
                let mut nw: XmlNodePtr;
                let mut cur: XmlNodePtr;
                let mut first_child: XmlNodePtr = null_mut();

                /*
                 * when operating on a reader, the entities definitions
                 * are always owning the entities subtree.
                if ((*ctxt).parseMode == XML_PARSE_READER)
                    (*ent).owner = 1;
                 */

                cur = (*ent).children.load(Ordering::Relaxed) as _;
                while !cur.is_null() {
                    nw = xml_doc_copy_node(cur, (*ctxt).my_doc, 1);
                    if !nw.is_null() {
                        if (*nw)._private.is_null() {
                            (*nw)._private = (*cur)._private;
                        }
                        if first_child.is_null() {
                            first_child = nw;
                        }
                        nw = (*(*ctxt).node).add_child(nw);
                    }
                    if cur == (*ent).last.load(Ordering::Relaxed) as _ {
                        /*
                         * needed to detect some strange empty
                         * node cases in the reader tests
                         */
                        if matches!((*ctxt).parse_mode, XmlParserMode::XmlParseReader)
                            && !nw.is_null()
                            && matches!((*nw).typ, XmlElementType::XmlElementNode)
                            && (*nw).children.is_none()
                        {
                            (*nw).extra = 1;
                        }

                        break;
                    }
                    cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
                }
                #[cfg(feature = "libxml_legacy")]
                if matches!(
                    (*ent).etype,
                    Some(XmlEntityType::XmlExternalGeneralParsedEntity)
                ) {
                    xml_add_entity_reference(ent, first_child, nw);
                }
            } else if list.is_null() || !(*ctxt).input_tab.is_empty() {
                let mut nw: XmlNodePtr;
                let mut cur: XmlNodePtr;
                let mut next: XmlNodePtr;
                let mut first_child: XmlNodePtr = null_mut();

                /*
                 * Copy the entity child list and make it the new
                 * entity child list. The goal is to make sure any
                 * ID or REF referenced will be the one from the
                 * document content and not the entity copy.
                 */
                cur = (*ent).children.load(Ordering::Relaxed) as _;
                (*ent).children.store(null_mut(), Ordering::Relaxed);
                let last: XmlNodePtr = (*ent).last.load(Ordering::Relaxed) as _;
                (*ent).last.store(null_mut(), Ordering::Relaxed);
                while !cur.is_null() {
                    next = (*cur).next.take().map_or(null_mut(), |n| n.as_ptr());
                    (*cur).parent = None;
                    nw = xml_doc_copy_node(cur, (*ctxt).my_doc, 1);
                    if !nw.is_null() {
                        if (*nw)._private.is_null() {
                            (*nw)._private = (*cur)._private;
                        }
                        if first_child.is_null() {
                            first_child = cur;
                        }
                        (*ent).add_child(nw);
                    }
                    (*(*ctxt).node).add_child(cur);
                    if cur == last {
                        break;
                    }
                    cur = next;
                }
                if (*ent).owner == 0 {
                    (*ent).owner = 1;
                }
                #[cfg(feature = "libxml_legacy")]
                if matches!(
                    (*ent).etype,
                    Some(XmlEntityType::XmlExternalGeneralParsedEntity)
                ) {
                    xml_add_entity_reference(ent, first_child, nw);
                }
            } else {
                /*
                 * the name change is to avoid coalescing of the
                 * node with a possible previous text one which
                 * would make (*ent).children a dangling pointer
                 */
                let nbktext: *const XmlChar =
                    xml_dict_lookup((*ctxt).dict, c"nbktext".as_ptr() as _, -1);
                if matches!(
                    (*(*ent).children.load(Ordering::Relaxed)).typ,
                    XmlElementType::XmlTextNode
                ) {
                    (*(*ent).children.load(Ordering::Relaxed)).name = nbktext;
                }
                if (*ent).last.load(Ordering::Relaxed) != (*ent).children.load(Ordering::Relaxed)
                    && matches!(
                        (*(*ent).last.load(Ordering::Relaxed)).typ,
                        XmlElementType::XmlTextNode
                    )
                {
                    (*(*ent).last.load(Ordering::Relaxed)).name = nbktext;
                }
                (*(*ctxt).node).add_child_list((*ent).children.load(Ordering::Relaxed));
            }

            /*
             * This is to avoid a nasty side effect, see
             * characters() in SAX.c
             */
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
pub(crate) unsafe extern "C" fn xml_parse_pe_reference(ctxt: XmlParserCtxtPtr) {
    let mut entity: XmlEntityPtr = null_mut();
    let input: XmlParserInputPtr;

    if (*ctxt).current_byte() != b'%' {
        return;
    }
    (*ctxt).skip_char();
    let name: *const XmlChar = xml_parse_name(ctxt);
    if name.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrPERefNoName,
            c"PEReference: no name\n".as_ptr() as _,
        );
        return;
    }
    if get_parser_debug_entities() != 0 {
        generic_error!(
            "PEReference: {}\n",
            CStr::from_ptr(name as *const i8).to_string_lossy()
        );
    }
    if (*ctxt).current_byte() != b';' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrPERefSemicolMissing, null());
        return;
    }

    (*ctxt).skip_char();

    /*
     * Request the entity from SAX
     */
    if !(*ctxt).sax.is_null() {
        if let Some(param) = (*(*ctxt).sax).get_parameter_entity {
            entity = param((*ctxt).user_data.clone(), name);
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }
    if entity.is_null() {
        /*
         * [ WFC: Entity Declared ]
         * In a document without any DTD, a document with only an
         * internal DTD subset which contains no parameter entity
         * references, or a document with "standalone='yes'", ...
         * ... The declaration of a parameter entity must precede
         * any reference to it...
         */
        if (*ctxt).standalone == 1 || ((*ctxt).has_external_subset == 0 && (*ctxt).has_perefs == 0)
        {
            xml_fatal_err_msg_str(
                ctxt,
                XmlParserErrors::XmlErrUndeclaredEntity,
                c"PEReference: %%%s; not found\n".as_ptr() as _,
                name,
            );
        } else {
            /*
             * [ VC: Entity Declared ]
             * In a document with an external subset or external
             * parameter entities with "standalone='no'", ...
             * ... The declaration of a parameter entity must
             * precede any reference to it...
             */
            if (*ctxt).validate != 0 && (*ctxt).vctxt.error.is_some() {
                xml_validity_error(
                    ctxt,
                    XmlParserErrors::XmlWarUndeclaredEntity,
                    c"PEReference: %%%s; not found\n".as_ptr() as _,
                    name,
                    null(),
                );
            } else {
                xml_warning_msg(
                    ctxt,
                    XmlParserErrors::XmlWarUndeclaredEntity,
                    c"PEReference: %%%s; not found\n".as_ptr() as _,
                    name,
                    null(),
                );
            }
            (*ctxt).valid = 0;
        }
    } else {
        /*
         * Internal checking in case the entity quest barfed
         */
        if !matches!(
            (*entity).etype,
            Some(XmlEntityType::XmlInternalParameterEntity)
                | Some(XmlEntityType::XmlExternalParameterEntity)
        ) {
            xml_warning_msg(
                ctxt,
                XmlParserErrors::XmlWarUndeclaredEntity,
                c"Internal: %%%s; is not a parameter entity\n".as_ptr() as _,
                name,
                null(),
            );
        } else {
            let mut start: [XmlChar; 4] = [0; 4];
            let mut parent_consumed: u64;

            if matches!(
                (*entity).etype,
                Some(XmlEntityType::XmlExternalParameterEntity)
            ) && (*ctxt).options & XmlParserOption::XmlParseNoent as i32 == 0
                && (*ctxt).options & XmlParserOption::XmlParseDtdvalid as i32 == 0
                && (*ctxt).options & XmlParserOption::XmlParseDtdload as i32 == 0
                && (*ctxt).options & XmlParserOption::XmlParseDtdattr as i32 == 0
                && (*ctxt).replace_entities == 0
                && (*ctxt).validate == 0
            {
                return;
            }

            if (*entity).flags & XML_ENT_EXPANDING as i32 != 0 {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, null());
                (*ctxt).halt();
                return;
            }

            /* Must be computed from old input before pushing new input. */
            parent_consumed = (*(*ctxt).input).parent_consumed;
            let old_ent: XmlEntityPtr = (*(*ctxt).input).entity;
            if old_ent.is_null()
                || (matches!(
                    (*old_ent).etype,
                    Some(XmlEntityType::XmlExternalParameterEntity)
                ) && (*old_ent).flags & XML_ENT_PARSED as i32 == 0)
            {
                parent_consumed = parent_consumed.saturating_add((*(*ctxt).input).consumed);
                parent_consumed = parent_consumed
                    .saturating_add((*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _);
            }

            input = xml_new_entity_input_stream(ctxt, entity);
            if xml_push_input(ctxt, input) < 0 {
                xml_free_input_stream(input);
                return;
            }

            (*entity).flags |= XML_ENT_EXPANDING as i32;

            (*input).parent_consumed = parent_consumed;

            if matches!(
                (*entity).etype,
                Some(XmlEntityType::XmlExternalParameterEntity)
            ) {
                /*
                 * Get the 4 first bytes and decode the charset
                 * if enc != XML_CHAR_ENCODING_NONE
                 * plug some encoding conversion routines.
                 * Note that, since we may have some non-UTF8
                 * encoding (like UTF16, bug 135229), the 'length'
                 * is not known, but we can calculate based upon
                 * the amount of data in the buffer.
                 */
                (*ctxt).grow();
                if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    return;
                }
                if (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4 {
                    start[0] = (*ctxt).current_byte();
                    start[1] = NXT!(ctxt, 1);
                    start[2] = NXT!(ctxt, 2);
                    start[3] = NXT!(ctxt, 3);
                    let enc = detect_encoding(&start);
                    if !matches!(enc, XmlCharEncoding::None) {
                        xml_switch_encoding(ctxt, enc);
                    }
                }

                if (*ctxt).content_bytes().starts_with(b"<?xml")
                    && xml_is_blank_char(NXT!(ctxt, 5) as u32)
                {
                    xml_parse_text_decl(ctxt);
                }
            }
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
pub(crate) unsafe extern "C" fn xml_parse_doc_type_decl(ctxt: XmlParserCtxtPtr) {
    let mut external_id: *mut XmlChar = null_mut();

    /*
     * We know that '<!DOCTYPE' has been detected.
     */
    (*ctxt).advance(9);

    (*ctxt).skip_blanks();

    /*
     * Parse the DOCTYPE name.
     */
    let name: *const XmlChar = xml_parse_name(ctxt);
    if name.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            c"xmlParseDocTypeDecl : no DOCTYPE name !\n".as_ptr() as _,
        );
    }
    (*ctxt).int_sub_name = name;

    (*ctxt).skip_blanks();

    /*
     * Check for SystemID and ExternalID
     */
    let uri: *mut XmlChar = xml_parse_external_id(ctxt, addr_of_mut!(external_id), 1);

    if !uri.is_null() || !external_id.is_null() {
        (*ctxt).has_external_subset = 1;
    }
    (*ctxt).ext_sub_uri = uri;
    (*ctxt).ext_sub_system = external_id;

    (*ctxt).skip_blanks();

    /*
     * Create and update the internal subset.
     */
    if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
        if let Some(internal_subset) = (*(*ctxt).sax).internal_subset {
            internal_subset((*ctxt).user_data.clone(), name, external_id, uri);
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }

    /*
     * Is there any internal subset declarations ?
     * they are handled separately in xmlParseInternalSubset()
     */
    if (*ctxt).current_byte() == b'[' {
        return;
    }

    /*
     * We should be at the end of the DOCTYPE declaration.
     */
    if (*ctxt).current_byte() != b'>' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDoctypeNotFinished, null());
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
pub(crate) unsafe extern "C" fn xml_parse_attribute(
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
            c"error parsing attribute name\n".as_ptr() as _,
        );
        return null_mut();
    }

    /*
     * read the value
     */
    (*ctxt).skip_blanks();
    if (*ctxt).current_byte() == b'=' {
        (*ctxt).skip_char();
        (*ctxt).skip_blanks();
        val = xml_parse_att_value(ctxt);
        (*ctxt).instate = XmlParserInputState::XmlParserContent;
    } else {
        xml_fatal_err_msg_str(
            ctxt,
            XmlParserErrors::XmlErrAttributeWithoutValue,
            c"Specification mandates value for attribute %s\n".as_ptr() as _,
            name,
        );
        return name;
    }

    /*
     * Check that xml:lang conforms to the specification
     * No more registered as an error, just generate a warning now
     * since this was deprecated in XML second edition
     */
    if (*ctxt).pedantic != 0
        && xml_str_equal(name, c"xml:lang".as_ptr() as _)
        && xml_check_language_id(val) == 0
    {
        xml_warning_msg(
            ctxt,
            XmlParserErrors::XmlWarLangValue,
            c"Malformed value for xml:lang : %s\n".as_ptr() as _,
            val,
            null(),
        );
    }

    /*
     * Check that xml:space conforms to the specification
     */
    if xml_str_equal(name, c"xml:space".as_ptr() as _) {
        if xml_str_equal(val, c"default".as_ptr() as _) {
            *(*ctxt).space_mut() = 0;
        } else if xml_str_equal(val, c"preserve".as_ptr() as _) {
            *(*ctxt).space_mut() = 1;
        } else {
            xml_warning_msg(
                ctxt,
                XmlParserErrors::XmlWarSpaceValue,
                c"Invalid value \"%s\" for xml:space : \"default\" or \"preserve\" expected\n"
                    .as_ptr() as _,
                val,
                null(),
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
pub(crate) unsafe extern "C" fn xml_parse_start_tag(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    use crate::libxml::parser::xml_err_attribute_dup;

    let mut attname: *const XmlChar;
    let mut attvalue: *mut XmlChar = null_mut();
    let mut atts: *mut *const XmlChar = (*ctxt).atts;
    let mut nbatts: i32 = 0;
    let mut maxatts: i32 = (*ctxt).maxatts;

    if (*ctxt).current_byte() != b'<' {
        return null_mut();
    }
    (*ctxt).advance(1);

    let name: *const XmlChar = xml_parse_name(ctxt);
    if name.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            c"xmlParseStartTag: invalid element name\n".as_ptr() as _,
        );
        return null_mut();
    }

    /*
     * Now parse the attributes, it ends up with the ending
     *
     * (S Attribute)* S?
     */
    (*ctxt).skip_blanks();
    (*ctxt).grow();

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
                c"xmlParseStartTag: problem parsing attributes\n".as_ptr() as _,
            );
            break;
        }

        'failed: {
            if !attvalue.is_null() {
                /*
                 * [ WFC: Unique Att Spec ]
                 * No attribute name may appear more than once in the same
                 * start-tag or empty-element tag.
                 */
                for i in (0..nbatts).step_by(2) {
                    if xml_str_equal(*atts.add(i as usize) as _, attname as _) {
                        xml_err_attribute_dup(ctxt, null_mut(), attname);
                        xml_free(attvalue as _);
                        break 'failed;
                    }
                }
                /*
                 * Add the pair to atts
                 */
                if atts.is_null() {
                    maxatts = 22; /* allow for 10 attrs by default */
                    atts = xml_malloc(maxatts as usize * size_of::<*mut XmlChar>())
                        as *mut *const XmlChar;
                    if atts.is_null() {
                        xml_err_memory(ctxt, null());
                        if !attvalue.is_null() {
                            xml_free(attvalue as _);
                        }
                        break 'failed;
                    }
                    (*ctxt).atts = atts;
                    (*ctxt).maxatts = maxatts;
                } else if nbatts + 4 > maxatts {
                    maxatts *= 2;
                    let n: *mut *const XmlChar =
                        xml_realloc(atts as _, maxatts as usize * size_of::<*const XmlChar>())
                            as *mut *const XmlChar;
                    if n.is_null() {
                        xml_err_memory(ctxt, null());
                        if !attvalue.is_null() {
                            xml_free(attvalue as _);
                        }
                        break 'failed;
                    }
                    atts = n;
                    (*ctxt).atts = atts;
                    (*ctxt).maxatts = maxatts;
                }

                *atts.add(nbatts as usize) = attname;
                nbatts += 1;
                *atts.add(nbatts as usize) = attvalue;
                nbatts += 1;
                *atts.add(nbatts as usize) = null_mut();
                *atts.add(nbatts as usize + 1) = null_mut();
            } else if !attvalue.is_null() {
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
                c"attributes construct error\n".as_ptr() as _,
            );
        }
        (*ctxt).shrink();
        (*ctxt).grow();
    }

    /*
     * SAX: Start of Element !
     */
    if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
        if let Some(elem) = (*(*ctxt).sax).start_element {
            if nbatts > 0 {
                elem((*ctxt).user_data.clone(), name, atts);
            } else {
                elem((*ctxt).user_data.clone(), name, null_mut());
            }
        }
    }

    if !atts.is_null() {
        /* Free only the content strings */
        for i in (1..nbatts).step_by(2) {
            if !(*atts.add(i as usize)).is_null() {
                xml_free(*atts.add(i as usize) as _);
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
pub(crate) unsafe extern "C" fn xml_parse_end_tag(ctxt: XmlParserCtxtPtr) {
    xml_parse_end_tag1(ctxt, 0);
}

/// Parse the start of an XML element. Returns -1 in case of error, 0 if an
/// opening tag was parsed, 1 if an empty element was parsed.
///
/// Always consumes '<'.
#[doc(alias = "xmlParseElementStart")]
pub(crate) unsafe extern "C" fn xml_parse_element_start(ctxt: XmlParserCtxtPtr) -> i32 {
    let name: *const XmlChar;
    let mut prefix: *const XmlChar = null_mut();
    let mut uri: *const XmlChar = null_mut();
    let mut node_info: XmlParserNodeInfo = unsafe { zeroed() };
    let mut tlen: i32 = 0;
    let ns_nr = (*ctxt).ns_tab.len();

    if (*ctxt).name_tab.len() as u32 > XML_PARSER_MAX_DEPTH
        && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0
    {
        xml_fatal_err_msg_int(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            c"Excessive depth in document: %d use xmlParserOption::XML_PARSE_HUGE option\n".as_ptr()
                as _,
            XML_PARSER_MAX_DEPTH as _,
        );
        (*ctxt).halt();
        return -1;
    }

    /* Capture start position */
    if (*ctxt).record_info != 0 {
        node_info.begin_pos = (*(*ctxt).input).consumed
            + (*ctxt).current_ptr().offset_from((*(*ctxt).input).base) as u64;
        node_info.begin_line = (*(*ctxt).input).line as _;
    }

    if (*ctxt).space_tab.is_empty() || (*ctxt).space() == -2 {
        (*ctxt).space_push(-1);
    } else {
        (*ctxt).space_push((*ctxt).space());
    }

    let line: i32 = (*(*ctxt).input).line;
    #[cfg(feature = "sax1")]
    {
        if (*ctxt).sax2 != 0 {
            name = xml_parse_start_tag2(
                ctxt,
                addr_of_mut!(prefix),
                addr_of_mut!(uri),
                addr_of_mut!(tlen),
            );
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
    (*ctxt).name_ns_push(
        name,
        prefix,
        uri,
        line,
        (*ctxt).ns_tab.len() as i32 - ns_nr as i32,
    );
    let cur: XmlNodePtr = (*ctxt).node;

    /*
     * [ VC: Root Element Type ]
     * The Name in the document type declaration must match the element
     * type of the root element.
     */
    #[cfg(feature = "libxml_valid")]
    if (*ctxt).validate != 0
        && (*ctxt).well_formed != 0
        && !(*ctxt).my_doc.is_null()
        && !(*ctxt).node.is_null()
        && NodePtr::from_ptr((*ctxt).node) == (*(*ctxt).my_doc).children
    {
        (*ctxt).valid &= xml_validate_root(addr_of_mut!((*ctxt).vctxt), (*ctxt).my_doc);
    }

    /*
     * Check for an Empty Element.
     */
    if (*ctxt).current_byte() == b'/' && NXT!(ctxt, 1) == b'>' {
        (*ctxt).advance(2);
        if (*ctxt).sax2 != 0 {
            if !(*ctxt).sax.is_null()
                && (*(*ctxt).sax).end_element_ns.is_some()
                && (*ctxt).disable_sax == 0
            {
                ((*(*ctxt).sax).end_element_ns.unwrap())(
                    (*ctxt).user_data.clone(),
                    name,
                    prefix,
                    uri,
                );
            }
        } else {
            #[cfg(feature = "sax1")]
            if !(*ctxt).sax.is_null()
                && (*(*ctxt).sax).end_element.is_some()
                && (*ctxt).disable_sax == 0
            {
                ((*(*ctxt).sax).end_element.unwrap())((*ctxt).user_data.clone(), name);
            }
        }
        (*ctxt).name_pop();
        (*ctxt).space_pop();
        if ns_nr != (*ctxt).ns_tab.len() {
            (*ctxt).ns_pop((*ctxt).ns_tab.len() - ns_nr);
        }
        if !cur.is_null() && (*ctxt).record_info != 0 {
            node_info.node = cur;
            node_info.end_pos = (*(*ctxt).input).consumed
                + (*ctxt).current_ptr().offset_from((*(*ctxt).input).base) as u64;
            node_info.end_line = (*(*ctxt).input).line as _;
            xml_parser_add_node_info(ctxt, addr_of_mut!(node_info));
        }
        return 1;
    }
    if (*ctxt).current_byte() == b'>' {
        (*ctxt).advance(1);
        if !cur.is_null() && (*ctxt).record_info != 0 {
            node_info.node = cur;
            node_info.end_pos = 0;
            node_info.end_line = 0;
            xml_parser_add_node_info(ctxt, addr_of_mut!(node_info));
        }
    } else {
        xml_fatal_err_msg_str_int_str(
            ctxt,
            XmlParserErrors::XmlErrGtRequired,
            c"Couldn't find end of Start Tag %s line %d\n".as_ptr() as _,
            name,
            line,
            null(),
        );

        /*
         * end of parsing of this node.
         */
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
pub(crate) unsafe extern "C" fn xml_parse_element_end(ctxt: XmlParserCtxtPtr) {
    let cur: XmlNodePtr = (*ctxt).node;

    if (*ctxt).name_tab.is_empty() {
        if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'/' {
            (*ctxt).advance(2);
        }
        return;
    }

    /*
     * parse the end of tag: '</' should be here.
     */
    if (*ctxt).sax2 != 0 {
        xml_parse_end_tag2(ctxt, &(*ctxt).push_tab[(*ctxt).name_tab.len() - 1]);
        (*ctxt).name_pop();
    } else {
        #[cfg(feature = "sax1")]
        {
            xml_parse_end_tag1(ctxt, 0);
        }
    }

    /*
     * Capture end position
     */
    if !cur.is_null() && (*ctxt).record_info != 0 {
        let node_info: XmlParserNodeInfoPtr =
            xml_parser_find_node_info(ctxt, cur) as XmlParserNodeInfoPtr;
        if !node_info.is_null() {
            (*node_info).end_pos = (*(*ctxt).input).consumed
                + (*ctxt).current_ptr().offset_from((*(*ctxt).input).base) as u64;
            (*node_info).end_line = (*(*ctxt).input).line as _;
        }
    }
}

/// Parse a content sequence. Stops at EOF or '</'. Leaves checking of unexpected EOF to the caller.
#[doc(alias = "xmlParseContentInternal")]
pub(crate) unsafe extern "C" fn xml_parse_content_internal(ctxt: XmlParserCtxtPtr) {
    let name_nr = (*ctxt).name_tab.len();

    (*ctxt).grow();
    while (*ctxt).current_byte() != 0
        && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        let cur: *const XmlChar = (*(*ctxt).input).cur;

        /*
         * First case : a Processing Instruction.
         */
        if *cur == b'<' && *cur.add(1) == b'?' {
            xml_parse_pi(ctxt);
        }
        /*
         * Second case : a CDSection
         */
        /* 2.6.0 test was *cur not RAW */
        else if (*ctxt).content_bytes().starts_with(b"<![CDATA[") {
            xml_parse_cdsect(ctxt);
        }
        /*
         * Third case :  a comment
         */
        else if *cur == b'<'
            && NXT!(ctxt, 1) == b'!'
            && NXT!(ctxt, 2) == b'-'
            && NXT!(ctxt, 3) == b'-'
        {
            xml_parse_comment(ctxt);
            (*ctxt).instate = XmlParserInputState::XmlParserContent;
        }
        /*
         * Fourth case :  a sub-element.
         */
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
        /*
         * Fifth case : a reference. If if has not been resolved,
         *    parsing returns it's Name, create the node
         */
        else if *cur == b'&' {
            xml_parse_reference(ctxt);
        }
        /*
         * Last case, text. Note that References are handled directly.
         */
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
pub unsafe extern "C" fn xml_parse_content(ctxt: XmlParserCtxtPtr) {
    let name_nr = (*ctxt).name_tab.len();

    xml_parse_content_internal(ctxt);

    if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        && (*ctxt).name_tab.len() > name_nr
    {
        let name: *const XmlChar = (*ctxt).name_tab[(*ctxt).name_tab.len() - 1];
        let line: i32 = (*ctxt).push_tab[(*ctxt).name_tab.len() - 1].line;
        xml_fatal_err_msg_str_int_str(
            ctxt,
            XmlParserErrors::XmlErrTagNotFinished,
            c"Premature end of data in tag %s line %d\n".as_ptr() as _,
            name,
            line,
            null(),
        );
    }
}

/// Parse the XML version.
///
/// `[24] VersionInfo ::= S 'version' Eq (' VersionNum ' | " VersionNum ")`
///
/// `[25] Eq ::= S? '=' S?`
///
/// Returns the version string, e.g. "1.0"
#[doc(alias = "xmlParseVersionInfo")]
pub(crate) unsafe fn xml_parse_version_info(ctxt: XmlParserCtxtPtr) -> Option<String> {
    let mut version = None;

    if (*ctxt).content_bytes().starts_with(b"version") {
        (*ctxt).advance(7);
        (*ctxt).skip_blanks();
        if (*ctxt).current_byte() != b'=' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEqualRequired, null());
            return None;
        }
        (*ctxt).skip_char();
        (*ctxt).skip_blanks();
        if (*ctxt).current_byte() == b'"' {
            (*ctxt).skip_char();
            version = xml_parse_version_num(ctxt);
            if (*ctxt).current_byte() != b'"' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, null());
            } else {
                (*ctxt).skip_char();
            }
        } else if (*ctxt).current_byte() == b'\'' {
            (*ctxt).skip_char();
            version = xml_parse_version_num(ctxt);
            if (*ctxt).current_byte() != b'\'' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, null());
            } else {
                (*ctxt).skip_char();
            }
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotStarted, null());
        }
    }
    version
}

/// Parse the XML encoding declaration
///
/// `[80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' |  "'" EncName "'")`
///
/// this setups the conversion filters.
///
/// Returns the encoding value or NULL
#[doc(alias = "xmlParseEncodingDecl")]
pub(crate) unsafe fn xml_parse_encoding_decl(ctxt: XmlParserCtxtPtr) -> Option<String> {
    let mut encoding = None;

    (*ctxt).skip_blanks();
    if (*ctxt).content_bytes().starts_with(b"encoding") {
        (*ctxt).advance(8);
        (*ctxt).skip_blanks();
        if (*ctxt).current_byte() != b'=' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEqualRequired, null());
            return None;
        }
        (*ctxt).skip_char();
        (*ctxt).skip_blanks();
        if (*ctxt).current_byte() == b'"' {
            (*ctxt).skip_char();
            encoding = xml_parse_enc_name(ctxt);
            if (*ctxt).current_byte() != b'"' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, null());
                return None;
            } else {
                (*ctxt).skip_char();
            }
        } else if (*ctxt).current_byte() == b'\'' {
            (*ctxt).skip_char();
            encoding = xml_parse_enc_name(ctxt);
            if (*ctxt).current_byte() != b'\'' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, null());
                return None;
            } else {
                (*ctxt).skip_char();
            }
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotStarted, null());
        }

        /*
         * Non standard parsing, allowing the user to ignore encoding
         */
        if (*ctxt).options & XmlParserOption::XmlParseIgnoreEnc as i32 != 0 {
            return None;
        }

        /*
         * UTF-16 encoding match has already taken place at this stage,
         * more over the little-endian/big-endian selection is already done
         */
        if let Some(encoding) = encoding.as_deref().filter(|e| {
            let e = e.to_ascii_uppercase();
            e == "UTF-16" || e == "UTF16"
        }) {
            /*
             * If no encoding was passed to the parser, that we are
             * using UTF-16 and no decoder is present i.e. the
             * document is apparently UTF-8 compatible, then raise an
             * encoding mismatch fatal error
             */
            if (*ctxt).encoding.is_none()
                && (*(*ctxt).input).buf.is_some()
                && (*(*ctxt).input)
                    .buf
                    .as_ref()
                    .unwrap()
                    .borrow()
                    .encoder
                    .is_none()
            {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidEncoding,
                    c"Document labelled UTF-16 but has UTF-8 content\n".as_ptr() as _,
                );
            }
            (*ctxt).encoding = Some(encoding.to_owned());
        }
        /*
         * UTF-8 encoding is handled natively
         */
        else if let Some(encoding) = encoding.as_deref().filter(|e| {
            let e = e.to_ascii_uppercase();
            e == "UTF-8" || e == "UTF8"
        }) {
            /* TODO: Check for encoding mismatch. */
            (*ctxt).encoding = Some(encoding.to_owned());
        } else if let Some(encoding) = encoding.as_deref() {
            (*(*ctxt).input).encoding = Some(encoding.to_owned());

            if let Some(handler) = find_encoding_handler(encoding) {
                if (*ctxt).switch_to_encoding(handler) < 0 {
                    /* failed to convert */
                    (*ctxt).err_no = XmlParserErrors::XmlErrUnsupportedEncoding as i32;
                    return None;
                }
            } else {
                let encoding = CString::new(encoding).unwrap();
                xml_fatal_err_msg_str(
                    ctxt,
                    XmlParserErrors::XmlErrUnsupportedEncoding,
                    c"Unsupported encoding %s\n".as_ptr() as _,
                    encoding.as_ptr() as *const u8,
                );
                return None;
            }
        }
    }
    encoding
}

/// Parse the XML standalone declaration
///
/// `[32] SDDecl ::= S 'standalone' Eq (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no')'"'))`
///
/// `[ VC: Standalone Document Declaration ]`  
/// TODO The standalone document declaration must have the value "no"
/// if any external markup declarations contain declarations of:
///  - attributes with default values, if elements to which these
///    attributes apply appear in the document without specifications
///    of values for these attributes, or
///  - entities (other than amp, lt, gt, apos, quot), if references
///    to those entities appear in the document, or
///  - attributes with values subject to normalization, where the
///    attribute appears in the document with a value which will change
///    as a result of normalization, or
///  - element types with element content, if white space occurs directly
///    within any instance of those types.
///
/// Returns:
/// - 1 if standalone="yes"
/// - 0 if standalone="no"
/// - -2 if standalone attribute is missing or invalid
///      (A standalone value of -2 means that the XML declaration was found,
///       but no value was specified for the standalone attribute).
#[doc(alias = "xmlParseSDDecl")]
pub(crate) unsafe extern "C" fn xml_parse_sddecl(ctxt: XmlParserCtxtPtr) -> i32 {
    let mut standalone: i32 = -2;

    (*ctxt).skip_blanks();
    if (*ctxt).content_bytes().starts_with(b"standalone") {
        (*ctxt).advance(10);
        (*ctxt).skip_blanks();
        if (*ctxt).current_byte() != b'=' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEqualRequired, null());
            return standalone;
        }
        (*ctxt).skip_char();
        (*ctxt).skip_blanks();
        if (*ctxt).current_byte() == b'\'' {
            (*ctxt).skip_char();
            if (*ctxt).current_byte() == b'n' && NXT!(ctxt, 1) == b'o' {
                standalone = 0;
                (*ctxt).advance(2);
            } else if (*ctxt).current_byte() == b'y'
                && NXT!(ctxt, 1) == b'e'
                && NXT!(ctxt, 2) == b's'
            {
                standalone = 1;
                (*ctxt).advance(3);
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStandaloneValue, null());
            }
            if (*ctxt).current_byte() != b'\'' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, null());
            } else {
                (*ctxt).skip_char();
            }
        } else if (*ctxt).current_byte() == b'"' {
            (*ctxt).skip_char();
            if (*ctxt).current_byte() == b'n' && NXT!(ctxt, 1) == b'o' {
                standalone = 0;
                (*ctxt).advance(2);
            } else if (*ctxt).current_byte() == b'y'
                && NXT!(ctxt, 1) == b'e'
                && NXT!(ctxt, 2) == b's'
            {
                standalone = 1;
                (*ctxt).advance(3);
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStandaloneValue, null());
            }
            if (*ctxt).current_byte() != b'"' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, null());
            } else {
                (*ctxt).skip_char();
            }
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotStarted, null());
        }
    }
    standalone
}

/// Parse an XML Misc* optional field.
///
/// `[27] Misc ::= Comment | PI |  S`
#[doc(alias = "xmlParseMisc")]
pub(crate) unsafe extern "C" fn xml_parse_misc(ctxt: XmlParserCtxtPtr) {
    #[allow(clippy::while_immutable_condition)]
    while !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        (*ctxt).skip_blanks();
        (*ctxt).grow();
        if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'?' {
            xml_parse_pi(ctxt);
        } else if (*ctxt).content_bytes().starts_with(b"<!--") {
            xml_parse_comment(ctxt);
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
pub unsafe extern "C" fn xml_parse_external_subset(
    ctxt: XmlParserCtxtPtr,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    (*ctxt).detect_sax2();
    (*ctxt).grow();

    if (*ctxt).encoding.is_none() && (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4 {
        let mut start: [XmlChar; 4] = [0; 4];

        start[0] = (*ctxt).current_byte();
        start[1] = NXT!(ctxt, 1);
        start[2] = NXT!(ctxt, 2);
        start[3] = NXT!(ctxt, 3);
        let enc = detect_encoding(&start);
        if !matches!(enc, XmlCharEncoding::None) {
            xml_switch_encoding(ctxt, enc);
        }
    }

    if (*ctxt).content_bytes().starts_with(b"<?xml") {
        xml_parse_text_decl(ctxt);
        if (*ctxt).err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
            /*
             * The XML REC instructs us to stop parsing right here
             */
            (*ctxt).halt();
            return;
        }
    }
    if (*ctxt).my_doc.is_null() {
        (*ctxt).my_doc = xml_new_doc(Some("1.0"));
        if (*ctxt).my_doc.is_null() {
            xml_err_memory(ctxt, c"New Doc failed".as_ptr() as _);
            return;
        }
        (*(*ctxt).my_doc).properties = XmlDocProperties::XmlDocInternal as i32;
    }
    if !(*ctxt).my_doc.is_null() && (*(*ctxt).my_doc).int_subset.is_null() {
        xml_create_int_subset(
            (*ctxt).my_doc,
            null(),
            (!external_id.is_null())
                .then(|| CStr::from_ptr(external_id as *const i8).to_string_lossy())
                .as_deref(),
            (!system_id.is_null())
                .then(|| CStr::from_ptr(system_id as *const i8).to_string_lossy())
                .as_deref(),
        );
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
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtSubsetNotFinished, null());
            (*ctxt).halt();
            return;
        }
        (*ctxt).skip_blanks();
        (*ctxt).shrink();
    }

    if (*ctxt).current_byte() != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtSubsetNotFinished, null());
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
pub(crate) unsafe extern "C" fn xml_string_decode_entities(
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
pub(crate) unsafe extern "C" fn xml_string_len_decode_entities(
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

/// n encoding error
#[doc(alias = "xmlErrEncodingInt")]
pub(crate) unsafe extern "C" fn xml_err_encoding_int(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    val: i32,
) {
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
        null_mut(),
        XmlErrorDomain::XmlFromParser,
        error,
        XmlErrorLevel::XmlErrFatal,
        null_mut(),
        0,
        None,
        None,
        None,
        val,
        0,
        msg,
        val
    );
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
        if (*ctxt).recovery == 0 {
            (*ctxt).disable_sax = 1;
        }
    }
}

/// The current c_char value, if using UTF-8 this may actually span multiple
/// bytes in the input buffer.
///
/// Returns the current c_char value and its length
#[doc(alias = "xmlStringCurrentChar")]
pub(crate) unsafe extern "C" fn xml_string_current_char(
    ctxt: XmlParserCtxtPtr,
    cur: *const XmlChar,
    len: *mut i32,
) -> i32 {
    if len.is_null() || cur.is_null() {
        return 0;
    }
    'encoding_error: {
        if ctxt.is_null() || (*ctxt).charset == XmlCharEncoding::UTF8 {
            /*
             * We are supposed to handle UTF8, check it's valid
             * From rfc2044: encoding of the Unicode values on UTF-8:
             *
             * UCS-4 range (hex.)           UTF-8 octet sequence (binary)
             * 0000 0000-0000 007F   0xxxxxxx
             * 0000 0080-0000 07FF   110xxxxx 10xxxxxx
             * 0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
             *
             * Check for the 0x110000 limit too
             */

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
                        /* 4-byte code */
                        *len = 4;
                        val = (*cur.add(0) as u32 & 0x7) << 18;
                        val |= (*cur.add(1) as u32 & 0x3f) << 12;
                        val |= (*cur.add(2) as u32 & 0x3f) << 6;
                        val |= *cur.add(3) as u32 & 0x3f;
                    } else {
                        /* 3-byte code */
                        *len = 3;
                        val = (*cur.add(0) as u32 & 0xf) << 12;
                        val |= (*cur.add(1) as u32 & 0x3f) << 6;
                        val |= *cur.add(2) as u32 & 0x3f;
                    }
                } else {
                    /* 2-byte code */
                    *len = 2;
                    val = (*cur.add(0) as u32 & 0x1f) << 6;
                    val |= *cur.add(1) as u32 & 0x3f;
                }
                if !xml_is_char(val) {
                    xml_err_encoding_int(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidChar,
                        c"Char 0x%X out of allowed range\n".as_ptr() as _,
                        val as _,
                    );
                }
                return val as _;
            } else {
                /* 1-byte code */
                *len = 1;
                return *cur as _;
            }
        }
        /*
         * Assume it's a fixed length encoding (1) with
         * a compatible encoding for the ASCII set, since
         * XML constructs only use < 128 chars
         */
        *len = 1;
        return *cur as _;
    }
    // encoding_error:

    /*
     * An encoding problem may arise from a truncated input buffer
     * splitting a character in the middle. In that case do not raise
     * an error but return 0 to indicate an end of stream problem
     */
    if ctxt.is_null()
        || (*ctxt).input.is_null()
        || (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) < 4
    {
        *len = 0;
        return 0;
    }
    /*
     * If we detect an UTF8 error that probably mean that the
     * input encoding didn't get properly advertised in the
     * declaration header. Report the error and switch the encoding
     * to ISO-Latin-1 (if you don't like this policy, just declare the
     * encoding !)
     */
    {
        let mut buffer: [c_char; 150] = [0; 150];

        snprintf(
            buffer.as_mut_ptr() as _,
            149,
            c"Bytes: 0x%02X 0x%02X 0x%02X 0x%02X\n".as_ptr() as _,
            *(*(*ctxt).input).cur.add(0) as u32,
            *(*(*ctxt).input).cur.add(1) as u32,
            *(*(*ctxt).input).cur.add(2) as u32,
            *(*(*ctxt).input).cur.add(3) as u32,
        );
        __xml_err_encoding(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            c"Input is not proper UTF-8, indicate encoding !\n%s".as_ptr() as _,
            buffer.as_ptr() as _,
            null(),
        );
    }
    *len = 1;
    *cur as _
}

/// `[69] PEReference ::= '%' Name ';'`
///
/// `[ WFC: No Recursion ]`
/// A parsed entity must not contain a recursive reference to itself, either directly or indirectly.
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
///
/// A PEReference may have been detected in the current input stream
/// the handling is done accordingly to
///      http://www.w3.org/TR/REC-xml#entproc
/// i.e.
///   - Included in literal in entity values
///   - Included as Parameter Entity reference within DTDs
#[doc(alias = "xmlParserHandlePEReference")]
pub(crate) unsafe extern "C" fn xml_parser_handle_pereference(ctxt: XmlParserCtxtPtr) {
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
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrPERefAtEOF, null());
            return;
        }
        XmlParserInputState::XmlParserProlog
        | XmlParserInputState::XmlParserStart
        | XmlParserInputState::XmlParserMisc => {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrPERefInProlog, null());
            return;
        }
        XmlParserInputState::XmlParserEntityDecl
        | XmlParserInputState::XmlParserContent
        | XmlParserInputState::XmlParserAttributeValue
        | XmlParserInputState::XmlParserPI
        | XmlParserInputState::XmlParserSystemLiteral
        | XmlParserInputState::XmlParserPublicLiteral => {
            /* we just ignore it there */
            return;
        }
        XmlParserInputState::XmlParserEpilog => {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrPERefInEpilog, null());
            return;
        }
        XmlParserInputState::XmlParserEntityValue => {
            /*
             * NOTE: in the case of entity values, we don't do the
             *       substitution here since we need the literal
             *       entity value to be able to save the internal
             *       subset of the document.
             *       This will be handled by xmlStringDecodeEntities
             */
            return;
        }
        XmlParserInputState::XmlParserDTD => {
            /*
             * [WFC: Well-Formedness Constraint: PEs in Internal Subset]
             * In the internal DTD subset, parameter-entity references
             * can occur only where markup declarations can occur, not
             * within markup declarations.
             * In that case this is handled in xmlParseMarkupDecl
             */
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
/// `[33] LanguageID ::= Langcode ('-' Subcode)*`  
/// `[34] Langcode ::= ISO639Code |  IanaCode |  UserCode`  
/// `[35] ISO639Code ::= ([a-z] | [A-Z]) ([a-z] | [A-Z])`  
/// `[36] IanaCode ::= ('i' | 'I') '-' ([a-z] | [A-Z])+`  
/// `[37] UserCode ::= ('x' | 'X') '-' ([a-z] | [A-Z])+`  
/// `[38] Subcode ::= ([a-z] | [A-Z])+`
///
/// The current REC reference the successors of RFC 1766, currently 5646
///
/// http://www.rfc-editor.org/rfc/rfc5646.txt
/// ```ignore
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
pub(crate) unsafe extern "C" fn xml_check_language_id(lang: *const XmlChar) -> i32 {
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
        /*
         * Still allow IANA code and user code which were coming
         * from the previous version of the XML-1.0 specification
         * it's deprecated but we should not fail
         */
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
        /*
         * Reserved
         */
        if nxt.offset_from(cur) > 8 || *nxt.add(0) != 0 {
            return 0;
        }
        return 1;
    }
    if nxt.offset_from(cur) < 2 {
        return 0;
    }
    /* we got an ISO 639 code */
    if *nxt.add(0) == 0 {
        return 1;
    }
    if *nxt.add(0) != b'-' {
        return 0;
    }

    nxt = nxt.add(1);
    cur = nxt;
    'region_m49: {
        /* now we can have extlang or script or region or variant */
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
                    /* we parsed an extlang */
                    if *nxt.add(0) == 0 {
                        return 1;
                    }
                    if *nxt.add(0) != b'-' {
                        return 0;
                    }
                    nxt = nxt.add(1);
                    cur = nxt;

                    /* now we can have script or region or variant */
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
                    /* we parsed a script */
                }
                if *nxt.add(0) == 0 {
                    return 1;
                }
                if *nxt.add(0) != b'-' {
                    return 0;
                }

                nxt = nxt.add(1);
                cur = nxt;
                /* now we can have region or variant */
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
                /* we parsed a region */
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
            /* now we can just have a variant */
            while (*nxt.add(0) >= b'A' && *nxt.add(0) <= b'Z')
                || (*nxt.add(0) >= b'a' && *nxt.add(0) <= b'z')
            {
                nxt = nxt.add(1);
            }

            if nxt.offset_from(cur) < 5 || nxt.offset_from(cur) > 8 {
                return 0;
            }
        }

        /* we parsed a variant */
        //  variant:
        if *nxt.add(0) == 0 {
            return 1;
        }
        if *nxt.add(0) != b'-' {
            return 0;
        }
        /* extensions and private use subtags not checked */
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
        /* now we can just have a variant */
        while (*nxt.add(0) >= b'A' && *nxt.add(0) <= b'Z')
            || (*nxt.add(0) >= b'a' && *nxt.add(0) <= b'z')
        {
            nxt = nxt.add(1);
        }

        if nxt.offset_from(cur) < 5 || nxt.offset_from(cur) > 8 {
            return 0;
        }

        /* we parsed a variant */
        //  variant:
        if *nxt.add(0) == 0 {
            return 1;
        }
        if *nxt.add(0) != b'-' {
            return 0;
        }
        /* extensions and private use subtags not checked */
        return 1;
    }
    0
}

/// Append the char value in the array
///
/// Returns the number of xmlChar written
#[doc(alias = "xmlCopyCharMultiByte")]
pub unsafe extern "C" fn xml_copy_char_multi_byte(mut out: *mut XmlChar, val: i32) -> i32 {
    if out.is_null() || val < 0 {
        return 0;
    }
    /*
     * We are supposed to handle UTF8, check it's valid
     * From rfc2044: encoding of the Unicode values on UTF-8:
     *
     * UCS-4 range (hex.)           UTF-8 octet sequence (binary)
     * 0000 0000-0000 007F   0xxxxxxx
     * 0000 0080-0000 07FF   110xxxxx 10xxxxxx
     * 0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
     */
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
            xml_err_encoding_int(
                null_mut(),
                XmlParserErrors::XmlErrInvalidChar,
                c"Internal error, xmlCopyCharMultiByte 0x%X out of bound\n".as_ptr() as _,
                val,
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
pub unsafe extern "C" fn xml_copy_char(_len: i32, out: *mut XmlChar, val: i32) -> i32 {
    if out.is_null() || val < 0 {
        return 0;
    }
    /* the len parameter is ignored */
    if val >= 0x80 {
        return xml_copy_char_multi_byte(out, val);
    }
    *out = val as _;
    1
}

/* we need to keep enough input to show errors in context */
pub(crate) const LINE_LEN: usize = 80;

/// This function removes used input for the parser.
#[doc(alias = "xmlParserInputShrink")]
pub(crate) unsafe extern "C" fn xml_parser_input_shrink(input: XmlParserInputPtr) {
    let mut used: usize;

    if input.is_null() {
        return;
    }
    if (*input).buf.is_none() {
        return;
    }
    if (*input).base.is_null() {
        return;
    }
    if (*input).cur.is_null() {
        return;
    }
    let Some(mut buf) = (*input).buf.as_ref().unwrap().borrow().buffer else {
        return;
    };

    used = (*input).cur.offset_from((*input).base) as _;
    /*
     * Do not shrink on large buffers whose only a tiny fraction
     * was consumed
     */
    if used > INPUT_CHUNK {
        let ret = buf.trim_head(used - LINE_LEN);
        if ret > 0 {
            used -= ret;
            if ret as u64 > u64::MAX || (*input).consumed > u64::MAX - ret as u64 {
                (*input).consumed = u64::MAX;
            } else {
                (*input).consumed += ret as u64;
            }
        }
    }

    if buf.len() <= INPUT_CHUNK {
        (*input)
            .buf
            .as_mut()
            .unwrap()
            .borrow_mut()
            .read(2 * INPUT_CHUNK as i32);
    }

    (*input).base = buf.as_ref().as_ptr();
    if (*input).base.is_null() {
        /* TODO: raise error */
        (*input).base = c"".as_ptr() as _;
        (*input).cur = (*input).base;
        (*input).end = (*input).base;
        return;
    }
    (*input).cur = (*input).base.add(used);
    (*input).end = buf.as_ref().as_ptr().add(buf.len());
}

/*
 * Specific function to keep track of entities references
 * and used by the XSLT debugger.
 */
#[cfg(feature = "libxml_legacy")]
mod __parser_internal_for_legacy {
    use std::{
        ffi::i32,
        ptr::null_mut,
        sync::atomic::{AtomicBool, Ordering},
    };

    use crate::{
        libxml::{
            entities::XmlEntityPtr, globals::xml_generic_error_context, parser::XmlParserCtxtPtr,
            tree::XmlNodePtr, xmlstring::XmlChar,
        },
        xml_generic_error,
    };

    pub(crate) static mut XML_ENTITY_REF_FUNC: Option<XmlEntityReferenceFunc> = None;

    /**
     * xmlEntityReferenceFunc:
     * @ent: the entity
     * @firstNode:  the fist node in the chunk
     * @lastNode:  the last nod in the chunk
     *
     * Callback function used when one needs to be able to track back the
     * provenance of a chunk of nodes inherited from an entity replacement.
     */
    pub type XmlEntityReferenceFunc =
        unsafe extern "C" fn(ent: XmlEntityPtr, firstNode: XmlNodePtr, lastNode: XmlNodePtr);

    /**
     * xmlSetEntityReferenceFunc:
     * @func: A valid function
     *
     * Set the function to call call back when a xml reference has been made
     */
    #[deprecated]
    pub unsafe extern "C" fn xml_set_entity_reference_func(func: XmlEntityReferenceFunc) {
        XML_ENTITY_REF_FUNC = Some(func);
    }

    /**
     * xmlParseQuotedString:
     * @ctxt:  an XML parser context
     *
     * Parse and return a string between quotes or doublequotes
     *
     * TODO: Deprecated, to  be removed at next drop of binary compatibility
     *
     * Returns the string parser or NULL.
     */
    #[deprecated]
    pub unsafe extern "C" fn xml_parse_quoted_string(_ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
        static DEPRECATED: AtomicBool = AtomicBool::new(false);

        if !DEPRECATED.load(Ordering::Acquire) {
            generic_error!("xmlParseQuotedString() deprecated function reached\n");
            DEPRECATED.store(true, Ordering::Release);
        }
        null_mut()
    }

    /**
     * xmlParseNamespace:
     * @ctxt:  an XML parser context
     *
     * xmlParseNamespace: parse specific PI '<?namespace ...' constructs.
     *
     * This is what the older xml-name Working Draft specified, a bunch of
     * other stuff may still rely on it, so support is still here as
     * if it was declared on the root of the Tree:-(
     *
     * TODO: remove from library
     *
     * To be removed at next drop of binary compatibility
     */
    #[deprecated]
    pub unsafe extern "C" fn xml_parse_namespace(_ctxt: XmlParserCtxtPtr) {
        static DEPRECATED: AtomicBool = AtomicBool::new(false);

        if !DEPRECATED.load(Ordering::Acquire) {
            generic_error!("xmlParseNamespace() deprecated function reached\n");
            DEPRECATED.store(true, Ordering::Release);
        }
    }

    /**
     * xmlNamespaceParseNSDef:
     * @ctxt:  an XML parser context
     *
     * parse a namespace prefix declaration
     *
     * TODO: this seems not in use anymore, the namespace handling is done on
     *       top of the SAX interfaces, i.e. not on raw input.
     *
     * [NS 1] NSDef ::= PrefixDef Eq SystemLiteral
     *
     * [NS 2] PrefixDef ::= 'xmlns' (':' NCName)?
     *
     * Returns the namespace name
     */
    #[deprecated]
    pub unsafe extern "C" fn xml_namespace_parse_nsdef(_ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
        static DEPRECATED: AtomicBool = AtomicBool::new(false);

        if !DEPRECATED.load(Ordering::Acquire) {
            generic_error!("xmlNamespaceParseNSDef() deprecated function reached\n");
            DEPRECATED.store(true, Ordering::Release);
        }
        null_mut()
    }

    /**
     * xmlScanName:
     * @ctxt:  an XML parser context
     *
     * Trickery: parse an XML name but without consuming the input flow
     * Needed for rollback cases. Used only when parsing entities references.
     *
     * TODO: seems deprecated now, only used in the default part of
     *       xmlParserHandleReference
     *
     * [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' |
     *                  CombiningChar | Extender
     *
     * [5] Name ::= (Letter | '_' | ':') (NameChar)*
     *
     * [6] Names ::= Name (S Name)*
     *
     * Returns the Name parsed or NULL
     */
    #[deprecated]
    pub unsafe extern "C" fn xml_scan_name(_ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
        static DEPRECATED: AtomicBool = AtomicBool::new(false);

        if !DEPRECATED.load(Ordering::Acquire) {
            generic_error!("xmlScanName() deprecated function reached\n");
            DEPRECATED.store(true, Ordering::Release);
        }
        null_mut()
    }

    /**
     * xmlNamespaceParseNCName:
     * @ctxt:  an XML parser context
     *
     * parse an XML namespace name.
     *
     * TODO: this seems not in use anymore, the namespace handling is done on
     *       top of the SAX interfaces, i.e. not on raw input.
     *
     * [NS 3] NCName ::= (Letter | '_') (NCNameChar)*
     *
     * [NS 4] NCNameChar ::= Letter | Digit | '.' | '-' | '_' |
     *                       CombiningChar | Extender
     *
     * Returns the namespace name or NULL
     */
    #[deprecated]
    pub unsafe extern "C" fn xml_namespace_parse_ncname(_ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
        static DEPRECATED: AtomicBool = AtomicBool::new(false);

        if !DEPRECATED.load(Ordering::Acquire) {
            generic_error!("xmlNamespaceParseNCName() deprecated function reached\n");
            DEPRECATED.store(true, Ordering::Release);
        }
        null_mut()
    }

    /**
     * xmlParserHandleReference:
     * @ctxt:  the parser context
     *
     * TODO: Remove, now deprecated ... the test is done directly in the
     *       content parsing
     * routines.
     *
     * [67] Reference ::= EntityRef | CharRef
     *
     * [68] EntityRef ::= '&' Name ';'
     *
     * [ WFC: Entity Declared ]
     * the Name given in the entity reference must match that in an entity
     * declaration, except that well-formed documents need not declare any
     * of the following entities: amp, lt, gt, apos, quot.
     *
     * [ WFC: Parsed Entity ]
     * An entity reference must not contain the name of an unparsed entity
     *
     * [66] CharRef ::= '&#' [0-9]+ ';' |
     *                  '&#x' [0-9a-fA-F]+ ';'
     *
     * A PEReference may have been detected in the current input stream
     * the handling is done accordingly to
     *      http://www.w3.org/TR/REC-xml#entproc
     */
    #[deprecated]
    pub unsafe extern "C" fn xml_parser_handle_reference(_ctxt: XmlParserCtxtPtr) {
        static DEPRECATED: AtomicBool = AtomicBool::new(false);

        if !DEPRECATED.load(Ordering::Acquire) {
            generic_error!("xmlParserHandleReference() deprecated function reached\n");
            DEPRECATED.store(true, Ordering::Release);
        }
    }

    /**
     * xmlNamespaceParseQName:
     * @ctxt:  an XML parser context
     * @prefix:  a xmlChar **
     *
     * TODO: this seems not in use anymore, the namespace handling is done on
     *       top of the SAX interfaces, i.e. not on raw input.
     *
     * parse an XML qualified name
     *
     * [NS 5] QName ::= (Prefix ':')? LocalPart
     *
     * [NS 6] Prefix ::= NCName
     *
     * [NS 7] LocalPart ::= NCName
     *
     * Returns the local part, and prefix is updated
     *   to get the Prefix if any.
     */
    #[deprecated]
    pub unsafe extern "C" fn xml_namespace_parse_qname(
        _ctxt: XmlParserCtxtPtr,
        _prefix: *mut *mut XmlChar,
    ) -> *mut XmlChar {
        static DEPRECATED: AtomicBool = AtomicBool::new(false);

        if !DEPRECATED.load(Ordering::Acquire) {
            generic_error!("xmlNamespaceParseQName() deprecated function reached\n");
            DEPRECATED.store(true, Ordering::Release);
        }
        null_mut()
    }

    /**
     * Entities
     */
    /**
     * xmlDecodeEntities:
     * @ctxt:  the parser context
     * @len:  the len to decode (in bytes !), -1 for no size limit
     * @what:  combination of XML_SUBSTITUTE_REF and XML_SUBSTITUTE_PEREF
     * @end:  an end marker xmlChar, 0 if none
     * @end2:  an end marker xmlChar, 0 if none
     * @end3:  an end marker xmlChar, 0 if none
     *
     * This function is deprecated, we now always process entities content
     * through xmlStringDecodeEntities
     *
     * TODO: remove it in next major release.
     *
     * [67] Reference ::= EntityRef | CharRef
     *
     * [69] PEReference ::= '%' Name ';'
     *
     * Returns A newly allocated string with the substitution done. The caller
     *      must deallocate it !
     */
    #[deprecated]
    pub unsafe extern "C" fn xml_decode_entities(
        _ctxt: XmlParserCtxtPtr,
        _len: i32,
        _what: i32,
        _end: XmlChar,
        _end2: XmlChar,
        _end3: XmlChar,
    ) -> *mut XmlChar {
        static DEPRECATED: AtomicBool = AtomicBool::new(false);

        if !DEPRECATED.load(Ordering::Acquire) {
            generic_error!("xmlDecodeEntities() deprecated function reached\n");
            DEPRECATED.store(true, Ordering::Release);
        }
        null_mut()
    }

    /**
     * xmlHandleEntity:
     * @ctxt:  an XML parser context
     * @entity:  an XML entity pointer.
     *
     * Default handling of defined entities, when should we define a new input
     * stream ? When do we just handle that as a set of chars ?
     *
     * OBSOLETE: to be removed at some point.
     */
    #[deprecated]
    pub unsafe extern "C" fn xml_handle_entity(_ctxt: XmlParserCtxtPtr, _entity: XmlEntityPtr) {
        static DEPRECATED: AtomicBool = AtomicBool::new(false);

        if !DEPRECATED.load(Ordering::Acquire) {
            generic_error!("xmlHandleEntity() deprecated function reached\n");
            DEPRECATED.store(true, Ordering::Release);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        globals::reset_last_error,
        libxml::{parser::xml_pop_input, xmlmemory::xml_mem_blocks},
        test_util::*,
    };

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
    fn test_xml_create_entity_parser_ctxt() {
        unsafe {
            let mut leaks = 0;

            for n_url in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_base in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let url = gen_const_xml_char_ptr(n_url, 0);
                        let id = gen_const_xml_char_ptr(n_id, 1);
                        let base = gen_const_xml_char_ptr(n_base, 2);

                        let ret_val =
                            xml_create_entity_parser_ctxt(url as *const XmlChar, id, base);
                        desret_xml_parser_ctxt_ptr(ret_val);
                        des_const_xml_char_ptr(n_url, url, 0);
                        des_const_xml_char_ptr(n_id, id, 1);
                        des_const_xml_char_ptr(n_base, base, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlCreateEntityParserCtxt",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlCreateEntityParserCtxt()"
                            );
                            eprint!(" {}", n_url);
                            eprint!(" {}", n_id);
                            eprintln!(" {}", n_base);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_create_file_parser_ctxt() {
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEOUTPUT {
                let mem_base = xml_mem_blocks();
                let filename = gen_fileoutput(n_filename, 0);

                let ret_val = xml_create_file_parser_ctxt(filename);
                desret_xml_parser_ctxt_ptr(ret_val);
                des_fileoutput(n_filename, filename, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCreateFileParserCtxt",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlCreateFileParserCtxt()"
                    );
                    eprintln!(" {}", n_filename);
                }
            }
        }
    }

    #[test]
    fn test_xml_create_urlparser_ctxt() {
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEOUTPUT {
                for n_options in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let filename = gen_fileoutput(n_filename, 0);
                    let options = gen_int(n_options, 1);

                    let ret_val = xml_create_url_parser_ctxt(filename, options);
                    desret_xml_parser_ctxt_ptr(ret_val);
                    des_fileoutput(n_filename, filename, 0);
                    des_int(n_options, options, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCreateURLParserCtxt",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlCreateURLParserCtxt()"
                        );
                        eprint!(" {}", n_filename);
                        eprintln!(" {}", n_options);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_entity_input_stream() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_entity in 0..GEN_NB_XML_ENTITY_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let entity = gen_xml_entity_ptr(n_entity, 1);

                    let ret_val = xml_new_entity_input_stream(ctxt, entity);
                    desret_xml_parser_input_ptr(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_entity_ptr(n_entity, entity, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewEntityInputStream",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlNewEntityInputStream()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_entity);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_input_from_file() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_filename in 0..GEN_NB_FILEPATH {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let filename = gen_filepath(n_filename, 1);

                    let ret_val = xml_new_input_from_file(ctxt, filename);
                    desret_xml_parser_input_ptr(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_filepath(n_filename, filename, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewInputFromFile",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlNewInputFromFile()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_filename);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_input_stream() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_new_input_stream(ctxt);
                desret_xml_parser_input_ptr(ret_val);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNewInputStream",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlNewInputStream()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_new_string_input_stream() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_buffer in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let buffer = gen_const_xml_char_ptr(n_buffer, 1);

                    let ret_val = xml_new_string_input_stream(ctxt, buffer);
                    desret_xml_parser_input_ptr(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_const_xml_char_ptr(n_buffer, buffer, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewStringInputStream",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlNewStringInputStream()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_buffer);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_parser_input_shrink() {
        unsafe {
            let mut leaks = 0;

            for n_in in 0..GEN_NB_XML_PARSER_INPUT_PTR {
                let mem_base = xml_mem_blocks();
                let input = gen_xml_parser_input_ptr(n_in, 0);

                xml_parser_input_shrink(input);
                des_xml_parser_input_ptr(n_in, input, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlParserInputShrink",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlParserInputShrink()"
                    );
                    eprintln!(" {}", n_in);
                }
            }
        }
    }

    #[test]
    fn test_xml_pop_input() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_pop_input(ctxt);
                desret_xml_char(ret_val);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlPopInput",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlPopInput()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_push_input() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_input in 0..GEN_NB_XML_PARSER_INPUT_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let input = gen_xml_parser_input_ptr(n_input, 1);

                    let ret_val = xml_push_input(ctxt, input);
                    desret_int(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_parser_input_ptr(n_input, input, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlPushInput",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlPushInput()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_input);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_set_entity_reference_func() {

        /* missing type support */
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

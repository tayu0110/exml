//! Provide internal methods and data structures for parsing XML documents.  
//! This module is based on `libxml/parserInternals.h`, `parserInternals.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::ffi::{c_char, c_int, c_uchar, c_uint, c_ulong, CStr};
use std::mem::{size_of, size_of_val, zeroed};
use std::os::raw::c_void;
use std::ptr::{addr_of_mut, null, null_mut};
use std::sync::atomic::Ordering;

#[cfg(feature = "legacy")]
pub use __parser_internal_for_legacy::*;
use libc::{memcpy, memset, size_t, snprintf, strcmp, INT_MAX};

#[cfg(feature = "catalog")]
use crate::libxml::catalog::{xml_catalog_get_defaults, XmlCatalogAllow, XML_CATALOG_PI};
use crate::libxml::dict::xml_dict_owns;
use crate::libxml::encoding::{xml_detect_char_encoding, xml_find_char_encoding_handler};
use crate::libxml::entities::{xml_get_predefined_entity, XmlEntityPtr};
use crate::libxml::globals::xml_generic_error_context;
use crate::libxml::parser::{
    ns_pop, xml_err_msg_str, xml_fatal_err_msg, xml_fatal_err_msg_int, xml_parse_cdsect,
    xml_parse_conditional_sections, xml_parse_enc_name, xml_parse_external_id,
    xml_parse_markup_decl, xml_parse_start_tag2, xml_parse_string_name, xml_parse_text_decl,
    xml_parse_version_num, xml_parser_add_node_info, xml_string_decode_entities_int,
    xml_validity_error, XmlParserInputState, XmlSAXHandlerPtr,
};
use crate::libxml::sax2::{xml_sax2_end_element, xml_sax2_get_entity, xml_sax2_start_element};
use crate::libxml::tree::{
    xml_create_int_subset, xml_new_doc, XmlAttributeDefault, XmlAttributeType, XmlDocProperties,
    XmlElementContentOccur, XmlElementContentType, XmlElementTypeVal,
};
use crate::libxml::valid::{
    xml_create_enumeration, xml_free_doc_element_content, xml_free_enumeration,
    xml_new_doc_element_content, xml_validate_element, xml_validate_root,
};
use crate::libxml::xml_io::XmlParserInputBufferPtr;
use crate::libxml::xmlerror::XmlParserErrors;
use crate::libxml::xmlstring::{xml_str_equal, xml_strcasecmp};

use crate::private::buf::{xml_buf_create, xml_buf_is_empty, xml_buf_reset_input};
use crate::private::enc::{xml_char_enc_input, xml_enc_input_chunk};
use crate::private::entities::{
    XML_ENT_CHECKED, XML_ENT_CHECKED_LT, XML_ENT_CONTAINS_LT, XML_ENT_EXPANDING, XML_ENT_PARSED,
};
use crate::private::parser::{
    __xml_err_encoding, xml_err_memory, xml_halt_parser, xml_parser_grow, xml_parser_shrink,
};
use crate::{__xml_raise_error, xml_generic_error};

use super::catalog::xml_catalog_add_local;
use super::dict::{xml_dict_free, xml_dict_lookup, xml_dict_reference, xml_dict_set_limit};
use super::encoding::{
    xml_char_enc_close_func, xml_get_char_encoding_handler, xml_get_char_encoding_name,
    XmlCharEncoding, XmlCharEncodingHandlerPtr,
};
use super::entities::XmlEntityType;
use super::globals::{
    xml_free, xml_malloc, xml_malloc_atomic, xml_parser_debug_entities, xml_realloc,
};
use super::hash::{
    xml_hash_add_entry2, xml_hash_create_dict, xml_hash_lookup2, xml_hash_update_entry2,
};
use super::parser::{
    name_ns_push, ns_push, space_pop, space_push, xml_detect_sax2, xml_fatal_err_msg_str,
    xml_fatal_err_msg_str_int_str, xml_free_parser_ctxt, xml_is_name_char,
    xml_load_external_entity, xml_new_parser_ctxt, xml_new_sax_parser_ctxt, xml_ns_err,
    xml_parse_att_value_internal, xml_parse_char_data_internal, xml_parse_char_ref,
    xml_parse_element_children_content_decl_priv, xml_parse_end_tag1, xml_parse_end_tag2,
    xml_parse_external_entity_private, xml_parser_entity_check, xml_parser_find_node_info,
    xml_saturated_add, xml_saturated_add_size_t, xml_stop_parser, xml_warning_msg, XmlDefAttrs,
    XmlDefAttrsPtr, XmlParserCtxtPtr, XmlParserInput, XmlParserInputPtr, XmlParserMode,
    XmlParserNodeInfo, XmlParserNodeInfoPtr, XmlParserOption, XML_COMPLETE_ATTRS, XML_DETECT_IDS,
    XML_SKIP_IDS,
};
use super::sax2::xml_sax2_ignorable_whitespace;
use super::tree::{
    xml_add_child, xml_add_child_list, xml_buf_content, xml_buf_end, xml_buf_shrink, xml_buf_use,
    xml_doc_copy_node, xml_free_doc, xml_free_node, xml_free_node_list, xml_new_doc_node,
    xml_set_tree_doc, xml_split_qname3, XmlDocPtr, XmlElementContentPtr, XmlElementType,
    XmlEnumerationPtr, XmlNodePtr, XML_XML_NAMESPACE,
};
use super::uri::{xml_build_uri, xml_canonic_path};
use super::xml_io::{
    __xml_loader_err, xml_check_http_input, xml_free_parser_input_buffer, xml_parser_get_directory,
    xml_parser_input_buffer_create_filename, xml_parser_input_buffer_create_mem,
    xml_parser_input_buffer_read,
};
use super::xmlerror::xml_copy_error;
use super::xmlstring::{
    xml_strchr, xml_strcmp, xml_strdup, xml_strlen, xml_strncmp, xml_strndup, XmlChar,
};

// #[cfg(target_os = "windows")]
// const XML_DIR_SEP: c_char = b'\\' as _;
// #[cfg(not(target_os = "windows"))]
// const XML_DIR_SEP: c_char = b'/' as _;

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

/**
 * xmlParserMaxDepth:
 *
 * arbitrary depth limit for the XML documents that we allow to
 * process. This is not a limitation of the parser but a safety
 * boundary feature, use XML_PARSE_HUGE option to override it.
 */
pub static mut XML_PARSER_MAX_DEPTH: c_uint = 256;

/**
 * XML_MAX_TEXT_LENGTH:
 *
 * Maximum size allowed for a single text node when building a tree.
 * This is not a limitation of the parser but a safety boundary feature,
 * use XML_PARSE_HUGE option to override it.
 * Introduced in 2.9.0
 */
pub const XML_MAX_TEXT_LENGTH: usize = 10000000;

/**
 * XML_MAX_HUGE_LENGTH:
 *
 * Maximum size allowed when XML_PARSE_HUGE is set.
 */
pub const XML_MAX_HUGE_LENGTH: usize = 1000000000;

/**
 * XML_MAX_NAME_LENGTH:
 *
 * Maximum size allowed for a markup identifier.
 * This is not a limitation of the parser but a safety boundary feature,
 * use XML_PARSE_HUGE option to override it.
 * Note that with the use of parsing dictionaries overriding the limit
 * may result in more runtime memory usage in face of "unfriendly' content
 * Introduced in 2.9.0
 */
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

/**
 * XML_MAX_LOOKUP_LIMIT:
 *
 * Maximum size allowed by the parser for ahead lookup
 * This is an upper boundary enforced by the parser to avoid bad
 * behaviour on "unfriendly' content
 * Introduced in 2.9.0
 */
pub const XML_MAX_LOOKUP_LIMIT: usize = 10000000;

/**
 * XML_MAX_NAMELEN:
 *
 * Identifiers can be longer, but this will be more costly
 * at runtime.
 */
pub const XML_MAX_NAMELEN: usize = 100;

/**
 * INPUT_CHUNK:
 *
 * The parser tries to always have that amount of input ready.
 * One of the point is providing context when reporting errors.
 */
pub const INPUT_CHUNK: usize = 250;

/************************************************************************
 *									*
 * UNICODE version of the macros.					*
 *									*
 ************************************************************************/
/**
 * IS_BYTE_CHAR:
 * @c:  an byte value (c_int)
 *
 * Macro to check the following production in the XML spec:
 *
 * [2] Char ::= #x9 | #xA | #xD | [#x20...]
 * any byte character in the accepted range
 */
#[macro_export]
macro_rules! IS_BYTE_CHAR {
    ( $c:expr ) => {
        $crate::xml_is_char_ch!($c)
    };
}

/**
 * IS_CHAR:
 * @c:  an UNICODE value (c_int)
 *
 * Macro to check the following production in the XML spec:
 *
 * [2] Char ::= #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD]
 *                  | [#x10000-#x10FFFF]
 * any Unicode character, excluding the surrogate blocks, FFFE, and FFFF.
 */
#[macro_export]
macro_rules! IS_CHAR {
    (  $c:expr ) => {
        $crate::xml_is_char_q!($c)
    };
}

/**
 * IS_CHAR_CH:
 * @c: an xmlChar (usually an c_uchar)
 *
 * Behaves like IS_CHAR on single-byte value
 */
#[macro_export]
macro_rules! IS_CHAR_CH {
    ( $c:expr ) => {
        $crate::xml_is_char_ch!($c)
    };
}

/**
 * IS_BLANK:
 * @c:  an UNICODE value (c_int)
 *
 * Macro to check the following production in the XML spec:
 *
 * [3] S ::= (#x20 | #x9 | #xD | #xA)+
 */
#[macro_export]
macro_rules! IS_BLANK {
    ( $c:expr ) => {
        $crate::xml_is_blank_q!($c) != 0
    };
}

/**
 * IS_BLANK_CH:
 * @c:  an xmlChar value (normally c_uchar)
 *
 * Behaviour same as IS_BLANK
 */
#[macro_export]
macro_rules! IS_BLANK_CH {
    ( $c:expr ) => {
        $crate::xml_is_blank_ch!($c)
    };
}

/**
 * IS_BASECHAR:
 * @c:  an UNICODE value (c_int)
 *
 * Macro to check the following production in the XML spec:
 *
 * [85] BaseChar ::= ... long list see REC ...
 */
#[macro_export]
macro_rules! IS_BASECHAR {
    ( $c:expr ) => {
        $crate::xml_is_base_char_q!($c) != 0
    };
}

/**
 * IS_DIGIT:
 * @c:  an UNICODE value (c_int)
 *
 * Macro to check the following production in the XML spec:
 *
 * [88] Digit ::= ... long list see REC ...
 */
#[macro_export]
macro_rules! IS_DIGIT {
    ( $c:expr ) => {
        $crate::xml_is_digit_q!($c) != 0
    };
}

/**
 * IS_DIGIT_CH:
 * @c:  an xmlChar value (usually an c_uchar)
 *
 * Behaves like IS_DIGIT but with a single byte argument
 */
// macro_rules! IS_DIGIT_CH {
// 	( $( $c:tt )* ) =>{
// 		  xmlIsDigit_ch($( $c )*)
// 	}
// }

/**
 * IS_COMBINING:
 * @c:  an UNICODE value (c_int)
 *
 * Macro to check the following production in the XML spec:
 *
 * [87] CombiningChar ::= ... long list see REC ...
 */
#[macro_export]
macro_rules! IS_COMBINING {
    ( $c:expr ) => {
        $crate::xml_is_combining_q!($c) != 0
    };
}

/**
 * IS_COMBINING_CH:
 * @c:  an xmlChar (usually an c_uchar)
 *
 * Always false (all combining chars > 0xff)
 */
// macro_rules! IS_COMBINING_CH {
//     ( $( $c:tt )* ) => {
//         0
//     };
// }

/**
 * IS_EXTENDER:
 * @c:  an UNICODE value (c_int)
 *
 * Macro to check the following production in the XML spec:
 *
 *
 * [89] Extender ::= #x00B7 | #x02D0 | #x02D1 | #x0387 | #x0640 |
 *                   #x0E46 | #x0EC6 | #x3005 | [#x3031-#x3035] |
 *                   [#x309D-#x309E] | [#x30FC-#x30FE]
 */
#[macro_export]
macro_rules! IS_EXTENDER {
    ( $c:expr ) => {
        $crate::xml_is_extender_q!($c) != 0
    };
}

/**
 * IS_EXTENDER_CH:
 * @c:  an xmlChar value (usually an c_uchar)
 *
 * Behaves like IS_EXTENDER but with a single-byte argument
 */
// macro_rules! IS_EXTENDER_CH {
//     ( $c:expr ) => {
//         xmlIsExtender_ch($c)
//     };
// }

/**
 * IS_IDEOGRAPHIC:
 * @c:  an UNICODE value (c_int)
 *
 * Macro to check the following production in the XML spec:
 *
 *
 * [86] Ideographic ::= [#x4E00-#x9FA5] | #x3007 | [#x3021-#x3029]
 */
#[macro_export]
macro_rules! IS_IDEOGRAPHIC {
    ( $c:expr ) => {
        $crate::xml_is_ideographic_q!($c) != 0
    };
}

/**
 * IS_LETTER:
 * @c:  an UNICODE value (c_int)
 *
 * Macro to check the following production in the XML spec:
 *
 *
 * [84] Letter ::= BaseChar | Ideographic
 */
#[macro_export]
macro_rules! IS_LETTER {
    ( $c:expr ) => {
        $crate::IS_BASECHAR!($c) || $crate::IS_IDEOGRAPHIC!($c)
    };
}

/**
 * IS_LETTER_CH:
 * @c:  an xmlChar value (normally c_uchar)
 *
 * Macro behaves like IS_LETTER, but only check base chars
 *
 */
// macro_rules! IS_LETTER_CH {
// 	( $( $c:tt )* ) =>{
// 		 xmlIsBaseChar_ch( $( $c )*)
// 	}
// }

/**
 * IS_ASCII_LETTER:
 * @c: an xmlChar value
 *
 * Macro to check [a-zA-Z]
 *
 */
#[macro_export]
macro_rules! IS_ASCII_LETTER {
    ( $c:expr ) => {
        (0x41..=0x5a).contains(&$c) || (0x61..=0x7a).contains(&$c)
    };
}

/**
 * IS_ASCII_DIGIT:
 * @c: an xmlChar value
 *
 * Macro to check [0-9]
 *
 */
#[macro_export]
macro_rules! IS_ASCII_DIGIT {
	( $( $c:tt )*) =>{
		(0x30 <= ($( $c )*)) && (($( $c )*) <= 0x39)
	}
}

/**
 * IS_PUBIDCHAR:
 * @c:  an UNICODE value (c_int)
 *
 * Macro to check the following production in the XML spec:
 *
 *
 * [13] PubidChar ::= #x20 | #xD | #xA | [a-zA-Z0-9] | [-'()+,./:=?;!*#@$_%]
 */
// macro_rules! IS_PUBIDCHAR {
// 	($( $c:tt )*) => {	xmlIsPubidCharQ($( $c )*)
// 	}
// }

/**
 * IS_PUBIDCHAR_CH:
 * @c:  an xmlChar value (normally c_uchar)
 *
 * Same as IS_PUBIDCHAR but for single-byte value
 */
#[macro_export]
macro_rules! IS_PUBIDCHAR_CH {
    ( $c:expr ) => {
        $crate::xml_is_pubid_char_ch!($c) != 0
    };
}

macro_rules! RAW {
    ($ctxt:expr) => {
        *(*(*$ctxt).input).cur
    };
}

macro_rules! NEXT {
    ($ctxt:expr) => {
        xml_next_char($ctxt)
    };
}

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

/**
 * Global variables used for predefined strings.
 */
pub static XML_STRING_TEXT: &CStr = c"text";
pub static XML_STRING_TEXT_NOENC: &CStr = c"textnoenc";
pub static XML_STRING_COMMENT: &CStr = c"comment";

/*
 * Function to finish the work of the macros where needed.
 */
/**
 * xmlIsLetter:
 * @c:  an unicode character (c_int)
 *
 * Check whether the character is allowed by the production
 * [84] Letter ::= BaseChar | Ideographic
 *
 * Returns 0 if not, non-zero otherwise
 */
pub unsafe extern "C" fn xml_is_letter(c: c_int) -> c_int {
    (IS_BASECHAR!(c as u32) || IS_IDEOGRAPHIC!(c as u32)) as i32
}

/**
 * xmlErrInternal:
 * @ctxt:  an XML parser context
 * @msg:  the error message
 * @str:  error information
 *
 * Handle an internal error
 */
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
        null_mut(),
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromParser as i32,
        XmlParserErrors::XmlErrInternalError as i32,
        XmlErrorLevel::XmlErrFatal,
        null_mut(),
        0,
        str as *const c_char,
        null_mut(),
        null_mut(),
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

/**
 * Parser context.
 */
/**
 * xmlCreateFileParserCtxt:
 * @filename:  the filename
 *
 * Create a parser context for a file content.
 * Automatic support for ZLIB/Compress compressed document is provided
 * by default if found at compile-time.
 *
 * Returns the new parser context or NULL
 */
pub unsafe extern "C" fn xml_create_file_parser_ctxt(filename: *const c_char) -> XmlParserCtxtPtr {
    xml_create_url_parser_ctxt(filename, 0)
}

/**
 * xmlCtxtUseOptionsInternal:
 * @ctxt: an XML parser context
 * @options:  a combination of xmlParserOption
 * @encoding:  the user provided encoding to use
 *
 * Applies the options to the parser context
 *
 * Returns 0 in case of success, the set of unknown or unimplemented options
 *         in case of error.
 */
pub(crate) unsafe extern "C" fn xml_ctxt_use_options_internal(
    ctxt: XmlParserCtxtPtr,
    mut options: c_int,
    encoding: *const c_char,
) -> c_int {
    if ctxt.is_null() {
        return -1;
    }
    if !encoding.is_null() {
        if !(*ctxt).encoding.is_null() {
            xml_free((*ctxt).encoding as _);
        }
        (*ctxt).encoding = xml_strdup(encoding as *const XmlChar);
    }
    if options & XmlParserOption::XmlParseRecover as i32 != 0 {
        (*ctxt).recovery = 1;
        options -= XmlParserOption::XmlParseRecover as i32;
        (*ctxt).options |= XmlParserOption::XmlParseRecover as i32;
    } else {
        (*ctxt).recovery = 0;
    }
    if options & XmlParserOption::XmlParseDtdload as i32 != 0 {
        (*ctxt).loadsubset = XML_DETECT_IDS as i32;
        options -= XmlParserOption::XmlParseDtdload as i32;
        (*ctxt).options |= XmlParserOption::XmlParseDtdload as i32;
    } else {
        (*ctxt).loadsubset = 0;
    }
    if options & XmlParserOption::XmlParseDtdattr as i32 != 0 {
        (*ctxt).loadsubset |= XML_COMPLETE_ATTRS as i32;
        options -= XmlParserOption::XmlParseDtdattr as i32;
        (*ctxt).options |= XmlParserOption::XmlParseDtdattr as i32;
    }
    if options & XmlParserOption::XmlParseNoent as i32 != 0 {
        (*ctxt).replace_entities = 1;
        /* (*ctxt).loadsubset |= XML_DETECT_IDS; */
        options -= XmlParserOption::XmlParseNoent as i32;
        (*ctxt).options |= XmlParserOption::XmlParseNoent as i32;
    } else {
        (*ctxt).replace_entities = 0;
    }
    if options & XmlParserOption::XmlParsePedantic as i32 != 0 {
        (*ctxt).pedantic = 1;
        options -= XmlParserOption::XmlParsePedantic as i32;
        (*ctxt).options |= XmlParserOption::XmlParsePedantic as i32;
    } else {
        (*ctxt).pedantic = 0;
    }
    if options & XmlParserOption::XmlParseNoblanks as i32 != 0 {
        (*ctxt).keep_blanks = 0;
        (*(*ctxt).sax).ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
        options -= XmlParserOption::XmlParseNoblanks as i32;
        (*ctxt).options |= XmlParserOption::XmlParseNoblanks as i32;
    } else {
        (*ctxt).keep_blanks = 1;
    }
    if options & XmlParserOption::XmlParseDtdvalid as i32 != 0 {
        (*ctxt).validate = 1;
        if options & XmlParserOption::XmlParseNowarning as i32 != 0 {
            (*ctxt).vctxt.warning = None;
        }
        if options & XmlParserOption::XmlParseNoerror as i32 != 0 {
            (*ctxt).vctxt.error = None;
        }
        options -= XmlParserOption::XmlParseDtdvalid as i32;
        (*ctxt).options |= XmlParserOption::XmlParseDtdvalid as i32;
    } else {
        (*ctxt).validate = 0;
    }
    if options & XmlParserOption::XmlParseNowarning as i32 != 0 {
        (*(*ctxt).sax).warning = None;
        options -= XmlParserOption::XmlParseNowarning as i32;
    }
    if options & XmlParserOption::XmlParseNoerror as i32 != 0 {
        (*(*ctxt).sax).error = None;
        (*(*ctxt).sax).fatal_error = None;
        options -= XmlParserOption::XmlParseNoerror as i32;
    }
    #[cfg(feature = "sax1")]
    if options & XmlParserOption::XmlParseSax1 as i32 != 0 {
        (*(*ctxt).sax).start_element = Some(xml_sax2_start_element);
        (*(*ctxt).sax).end_element = Some(xml_sax2_end_element);
        (*(*ctxt).sax).start_element_ns = None;
        (*(*ctxt).sax).end_element_ns = None;
        (*(*ctxt).sax).initialized = 1;
        options -= XmlParserOption::XmlParseSax1 as i32;
        (*ctxt).options |= XmlParserOption::XmlParseSax1 as i32;
    }
    if options & XmlParserOption::XmlParseNodict as i32 != 0 {
        (*ctxt).dict_names = 0;
        options -= XmlParserOption::XmlParseNodict as i32;
        (*ctxt).options |= XmlParserOption::XmlParseNodict as i32;
    } else {
        (*ctxt).dict_names = 1;
    }
    if options & XmlParserOption::XmlParseNocdata as i32 != 0 {
        (*(*ctxt).sax).cdata_block = None;
        options -= XmlParserOption::XmlParseNocdata as i32;
        (*ctxt).options |= XmlParserOption::XmlParseNocdata as i32;
    }
    if options & XmlParserOption::XmlParseNsclean as i32 != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseNsclean as i32;
        options -= XmlParserOption::XmlParseNsclean as i32;
    }
    if options & XmlParserOption::XmlParseNonet as i32 != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseNonet as i32;
        options -= XmlParserOption::XmlParseNonet as i32;
    }
    if options & XmlParserOption::XmlParseCompact as i32 != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseCompact as i32;
        options -= XmlParserOption::XmlParseCompact as i32;
    }
    if options & XmlParserOption::XmlParseOld10 as i32 != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseOld10 as i32;
        options -= XmlParserOption::XmlParseOld10 as i32;
    }
    if options & XmlParserOption::XmlParseNobasefix as i32 != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseNobasefix as i32;
        options -= XmlParserOption::XmlParseNobasefix as i32;
    }
    if options & XmlParserOption::XmlParseHuge as i32 != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseHuge as i32;
        options -= XmlParserOption::XmlParseHuge as i32;
        if !(*ctxt).dict.is_null() {
            xml_dict_set_limit((*ctxt).dict, 0);
        }
    }
    if options & XmlParserOption::XmlParseOldsax as i32 != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseOldsax as i32;
        options -= XmlParserOption::XmlParseOldsax as i32;
    }
    if options & XmlParserOption::XmlParseIgnoreEnc as i32 != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseIgnoreEnc as i32;
        options -= XmlParserOption::XmlParseIgnoreEnc as i32;
    }
    if options & XmlParserOption::XmlParseBigLines as i32 != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseBigLines as i32;
        options -= XmlParserOption::XmlParseBigLines as i32;
    }
    (*ctxt).linenumbers = 1;
    options
}

/**
 * xmlCreateURLParserCtxt:
 * @filename:  the filename or URL
 * @options:  a combination of xmlParserOption
 *
 * Create a parser context for a file or URL content.
 * Automatic support for ZLIB/Compress compressed document is provided
 * by default if found at compile-time and for file accesses
 *
 * Returns the new parser context or NULL
 */
pub unsafe extern "C" fn xml_create_url_parser_ctxt(
    filename: *const c_char,
    options: c_int,
) -> XmlParserCtxtPtr {
    let mut directory: *mut c_char = null_mut();

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        xml_err_memory(null_mut(), c"cannot allocate parser context".as_ptr() as _);
        return null_mut();
    }

    if options != 0 {
        xml_ctxt_use_options_internal(ctxt, options, null_mut());
    }
    (*ctxt).linenumbers = 1;

    let input_stream: XmlParserInputPtr = xml_load_external_entity(filename, null_mut(), ctxt);
    if input_stream.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    input_push(ctxt, input_stream);
    if (*ctxt).directory.is_null() && directory.is_null() {
        directory = xml_parser_get_directory(filename);
    }
    if (*ctxt).directory.is_null() && !directory.is_null() {
        (*ctxt).directory = directory;
    }

    ctxt
}

/**
 * xmlCreateMemoryParserCtxt:
 * @buffer:  a pointer to a c_char array
 * @size:  the size of the array
 *
 * Create a parser context for an XML in-memory document.
 *
 * Returns the new parser context or NULL
 */
pub unsafe extern "C" fn xml_create_memory_parser_ctxt(
    buffer: *const c_char,
    size: c_int,
) -> XmlParserCtxtPtr {
    if buffer.is_null() {
        return null_mut();
    }
    if size <= 0 {
        return null_mut();
    }

    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        return null_mut();
    }

    let buf: XmlParserInputBufferPtr =
        xml_parser_input_buffer_create_mem(buffer, size, XmlCharEncoding::XmlCharEncodingNone);
    if buf.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    let input: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input.is_null() {
        xml_free_parser_input_buffer(buf);
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    (*input).filename = null_mut();
    (*input).buf = buf;
    xml_buf_reset_input((*(*input).buf).buffer, input);

    input_push(ctxt, input);
    ctxt
}

/**
 * xmlCreateEntityParserCtxtInternal:
 * @URL:  the entity URL
 * @ID:  the entity PUBLIC ID
 * @base:  a possible base for the target URI
 * @pctx:  parser context used to set options on new context
 *
 * Create a parser context for an external entity
 * Automatic support for ZLIB/Compress compressed document is provided
 * by default if found at compile-time.
 *
 * Returns the new parser context or NULL
 */
pub(crate) unsafe extern "C" fn xml_create_entity_parser_ctxt_internal(
    sax: XmlSAXHandlerPtr,
    user_data: *mut c_void,
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

        input_push(ctxt, input_stream);

        if (*ctxt).directory.is_null() && directory.is_null() {
            directory = xml_parser_get_directory(url as _);
        }
        if (*ctxt).directory.is_null() && !directory.is_null() {
            (*ctxt).directory = directory;
        }
    } else {
        input_stream = xml_load_external_entity(uri as _, id as _, ctxt as _);
        if input_stream.is_null() {
            xml_free(uri as _);
            xml_free_parser_ctxt(ctxt);
            return null_mut();
        }

        input_push(ctxt, input_stream);

        if (*ctxt).directory.is_null() && directory.is_null() {
            directory = xml_parser_get_directory(uri as _);
        }
        if (*ctxt).directory.is_null() && !directory.is_null() {
            (*ctxt).directory = directory;
        }
        xml_free(uri as _);
    }
    ctxt
}

/**
 * xmlCreateEntityParserCtxt:
 * @URL:  the entity URL
 * @ID:  the entity PUBLIC ID
 * @base:  a possible base for the target URI
 *
 * Create a parser context for an external entity
 * Automatic support for ZLIB/Compress compressed document is provided
 * by default if found at compile-time.
 *
 * Returns the new parser context or NULL
 */
pub unsafe extern "C" fn xml_create_entity_parser_ctxt(
    url: *const XmlChar,
    id: *const XmlChar,
    base: *const XmlChar,
) -> XmlParserCtxtPtr {
    xml_create_entity_parser_ctxt_internal(null_mut(), null_mut(), url, id, base, null_mut())
}

unsafe extern "C" fn xml_detect_ebcdic(input: XmlParserInputPtr) -> XmlCharEncodingHandlerPtr {
    let mut out: [XmlChar; 200] = [0; 200];
    let mut handler: XmlCharEncodingHandlerPtr;
    let mut inlen: c_int;
    let mut outlen: usize;

    /*
     * To detect the EBCDIC code page, we convert the first 200 bytes
     * to EBCDIC-US and try to find the encoding declaration.
     */
    handler = xml_get_char_encoding_handler(XmlCharEncoding::XmlCharEncodingEbcdic);
    if handler.is_null() {
        return null_mut();
    }
    outlen = out.len() - 1;
    inlen = (*input).end.offset_from((*input).cur) as _;
    let res: c_int = xml_enc_input_chunk(
        handler,
        out.as_mut_ptr() as _,
        addr_of_mut!(outlen) as _,
        (*input).cur,
        addr_of_mut!(inlen) as _,
        0,
    );
    if res < 0 {
        return handler;
    }
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
            while IS_BLANK_CH!(out[i]) {
                i += 1;
            }
            i += 1;
            if out[i - 1] != b'=' {
                break;
            }
            while IS_BLANK_CH!(out[i]) {
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
            xml_char_enc_close_func(handler);
            handler = xml_find_char_encoding_handler((out.as_ptr() as *mut c_char).add(start));
            break;
        }

        i += 1;
    }

    handler
}

/**
 * xmlSwitchEncoding:
 * @ctxt:  the parser context
 * @enc:  the encoding value (number)
 *
 * change the input functions when discovering the character encoding
 * of a given entity.
 *
 * Returns 0 in case of success, -1 otherwise
 */
pub unsafe extern "C" fn xml_switch_encoding(
    ctxt: XmlParserCtxtPtr,
    enc: XmlCharEncoding,
) -> c_int {
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
            XmlCharEncoding::XmlCharEncodingUtf8
                | XmlCharEncoding::XmlCharEncodingUtf16le
                | XmlCharEncoding::XmlCharEncodingUtf16be
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

    let handler = match enc {
        XmlCharEncoding::XmlCharEncodingError => {
            __xml_err_encoding(
                ctxt,
                XmlParserErrors::XmlErrUnknownEncoding,
                c"encoding unknown\n".as_ptr() as _,
                null(),
                null(),
            );
            return -1;
        }
        XmlCharEncoding::XmlCharEncodingNone => {
            /* let's assume it's UTF-8 without the XML decl */
            (*ctxt).charset = XmlCharEncoding::XmlCharEncodingUtf8 as i32;
            return 0;
        }
        XmlCharEncoding::XmlCharEncodingUtf8 => {
            /* default encoding, no conversion should be needed */
            (*ctxt).charset = XmlCharEncoding::XmlCharEncodingUtf8 as i32;
            return 0;
        }
        XmlCharEncoding::XmlCharEncodingEbcdic => xml_detect_ebcdic((*ctxt).input),
        _ => xml_get_char_encoding_handler(enc),
    };
    if handler.is_null() {
        /*
         * Default handlers.
         */
        match enc {
            XmlCharEncoding::XmlCharEncodingAscii => {
                /* default encoding, no conversion should be needed */
                (*ctxt).charset = XmlCharEncoding::XmlCharEncodingUtf8 as i32;
                return 0;
            }
            XmlCharEncoding::XmlCharEncoding8859_1 => {
                if (*ctxt).input_nr == 1
                    && (*ctxt).encoding.is_null()
                    && !(*ctxt).input.is_null()
                    && !(*(*ctxt).input).encoding.is_null()
                {
                    (*ctxt).encoding = xml_strdup((*(*ctxt).input).encoding);
                }
                (*ctxt).charset = enc as i32;
                return 0;
            }
            _ => {
                __xml_err_encoding(
                    ctxt,
                    XmlParserErrors::XmlErrUnsupportedEncoding,
                    c"encoding not supported: %s\n".as_ptr() as _,
                    xml_get_char_encoding_name(enc) as _,
                    null(),
                );
                /*
                 * TODO: We could recover from errors in external entities
                 * if we didn't stop the parser. But most callers of this
                 * function don't check the return value.
                 */
                xml_stop_parser(ctxt);
                return -1;
            }
        }
    }
    let ret: c_int = xml_switch_input_encoding(ctxt, (*ctxt).input, handler);
    if ret < 0 || (*ctxt).err_no == XmlParserErrors::XmlI18nConvFailed as i32 {
        /*
         * on encoding conversion errors, stop the parser
         */
        xml_stop_parser(ctxt);
        (*ctxt).err_no = XmlParserErrors::XmlI18nConvFailed as i32;
    }
    ret
}

/**
 * xmlSwitchToEncoding:
 * @ctxt:  the parser context
 * @handler:  the encoding handler
 *
 * change the input functions when discovering the character encoding
 * of a given entity.
 *
 * Returns 0 in case of success, -1 otherwise
 */
pub unsafe extern "C" fn xml_switch_to_encoding(
    ctxt: XmlParserCtxtPtr,
    handler: XmlCharEncodingHandlerPtr,
) -> c_int {
    if ctxt.is_null() {
        return -1;
    }
    xml_switch_input_encoding(ctxt, (*ctxt).input, handler)
}

/**
 * xmlSwitchInputEncoding:
 * @ctxt:  the parser context
 * @input:  the input stream
 * @handler:  the encoding handler
 *
 * change the input functions when discovering the character encoding
 * of a given entity.
 *
 * Returns 0 in case of success, -1 otherwise
 */
pub(crate) unsafe extern "C" fn xml_switch_input_encoding(
    ctxt: XmlParserCtxtPtr,
    input: XmlParserInputPtr,
    handler: XmlCharEncodingHandlerPtr,
) -> c_int {
    let nbchars: c_int;

    if handler.is_null() {
        return -1;
    }
    if input.is_null() {
        return -1;
    }
    let input_buf: XmlParserInputBufferPtr = (*input).buf;
    if input_buf.is_null() {
        xml_err_internal(
            ctxt,
            c"static memory buffer doesn't support encoding\n".as_ptr() as _,
            null(),
        );
        /*
         * Callers assume that the input buffer takes ownership of the
         * encoding handler. xmlCharEncCloseFunc frees unregistered
         * handlers and avoids a memory leak.
         */
        xml_char_enc_close_func(handler);
        return -1;
    }

    if !(*input_buf).encoder.is_null() {
        if (*input_buf).encoder == handler {
            return 0;
        }

        /*
         * Switching encodings during parsing is a really bad idea,
         * but Chromium can match between ISO-8859-1 and UTF-16 before
         * separate calls to xmlParseChunk.
         *
         * TODO: We should check whether the "raw" input buffer is empty and
         * convert the old content using the old encoder.
         */

        xml_char_enc_close_func((*input_buf).encoder);
        (*input_buf).encoder = handler;
        return 0;
    }

    (*ctxt).charset = XmlCharEncoding::XmlCharEncodingUtf8 as i32;
    (*input_buf).encoder = handler;

    /*
     * Is there already some content down the pipe to convert ?
     */
    if xml_buf_is_empty((*input_buf).buffer) == 0 {
        /*
         * FIXME: The BOM shouldn't be skipped here, but in the parsing code.
         */

        /*
         * Specific handling of the Byte Order Mark for
         * UTF-16
         */
        if !(*handler).name.load(Ordering::Relaxed).is_null()
            && (strcmp(
                (*handler).name.load(Ordering::Relaxed) as _,
                c"UTF-16LE".as_ptr() as _,
            ) == 0
                || strcmp(
                    (*handler).name.load(Ordering::Relaxed) as _,
                    c"UTF-16".as_ptr() as _,
                ) == 0)
            && *(*input).cur.add(0) == 0xFF
            && *(*input).cur.add(1) == 0xFE
        {
            (*input).cur = (*input).cur.add(2);
        }
        if !(*handler).name.load(Ordering::Relaxed).is_null()
            && strcmp(
                (*handler).name.load(Ordering::Relaxed) as _,
                c"UTF-16BE".as_ptr() as _,
            ) == 0
            && *(*input).cur.add(0) == 0xFE
            && *(*input).cur.add(1) == 0xFF
        {
            (*input).cur = (*input).cur.add(2);
        }
        /*
         * Errata on XML-1.0 June 20 2001
         * Specific handling of the Byte Order Mark for
         * UTF-8
         */
        if !(*handler).name.load(Ordering::Relaxed).is_null()
            && strcmp(
                (*handler).name.load(Ordering::Relaxed) as _,
                c"UTF-8".as_ptr() as _,
            ) == 0
            && *(*input).cur.add(0) == 0xEF
            && *(*input).cur.add(1) == 0xBB
            && *(*input).cur.add(2) == 0xBF
        {
            (*input).cur = (*input).cur.add(3);
        }

        /*
         * Shrink the current input buffer.
         * Move it as the raw buffer and create a new input buffer
         */
        let processed: size_t = (*input).cur.offset_from((*input).base) as usize;
        xml_buf_shrink((*input_buf).buffer, processed);
        (*input).consumed += processed as u64;
        (*input_buf).raw = (*input_buf).buffer;
        (*input_buf).buffer = xml_buf_create();
        assert!(!(*input_buf).buffer.is_null());
        (*input_buf).rawconsumed = processed as u64;
        let using: size_t = xml_buf_use((*input_buf).raw);

        /*
         * TODO: We must flush and decode the whole buffer to make functions
         * like xmlReadMemory work with a user-provided encoding. If the
         * encoding is specified directly, we should probably set
         * XML_PARSE_IGNORE_ENC in xmlDoRead to avoid switching encodings
         * twice. Then we could set "flush" to false which should save
         * a considerable amount of memory when parsing from memory.
         * It's probably even possible to remove this whole if-block
         * completely.
         */
        nbchars = xml_char_enc_input(input_buf, 1);
        xml_buf_reset_input((*input_buf).buffer, input);
        if nbchars < 0 {
            /* TODO: This could be an out of memory or an encoding error. */
            xml_err_internal(
                ctxt,
                c"switching encoding: encoder error\n".as_ptr() as _,
                null(),
            );
            xml_halt_parser(ctxt);
            return -1;
        }
        let consumed: size_t = using - xml_buf_use((*input_buf).raw);
        if consumed as u64 > u64::MAX || (*input_buf).rawconsumed > u64::MAX - consumed as c_ulong {
            (*input_buf).rawconsumed = u64::MAX;
        } else {
            (*input_buf).rawconsumed += consumed as u64;
        }
    }
    0
}

/**
 * Input Streams.
 */
/**
 * xmlNewStringInputStream:
 * @ctxt:  an XML parser context
 * @buffer:  an memory buffer
 *
 * Create a new input stream based on a memory buffer.
 * Returns the new input stream
 */
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
    if *xml_parser_debug_entities() != 0 {
        xml_generic_error!(
            xml_generic_error_context(),
            c"new fixed input: %.30s\n".as_ptr() as _,
            buffer
        );
    }
    let buf: XmlParserInputBufferPtr = xml_parser_input_buffer_create_mem(
        buffer as *const c_char,
        xml_strlen(buffer),
        XmlCharEncoding::XmlCharEncodingNone,
    );
    if buf.is_null() {
        xml_err_memory(ctxt, null());
        return null_mut();
    }
    let input: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input.is_null() {
        xml_err_memory(
            ctxt,
            c"couldn't allocate a new input stream\n".as_ptr() as _,
        );
        xml_free_parser_input_buffer(buf);
        return null_mut();
    }
    (*input).buf = buf;
    xml_buf_reset_input((*(*input).buf).buffer, input);
    input
}

/**
 * xmlNewEntityInputStream:
 * @ctxt:  an XML parser context
 * @entity:  an Entity pointer
 *
 * DEPRECATED: Internal function, do not use.
 *
 * Create a new input stream based on an xmlEntityPtr
 *
 * Returns the new input stream or NULL
 */
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
    if *xml_parser_debug_entities() != 0 {
        xml_generic_error!(
            xml_generic_error_context(),
            c"new input from entity: %s\n".as_ptr() as _,
            (*entity).name.load(Ordering::Relaxed)
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
        (*input).filename =
            xml_strdup((*entity).uri.load(Ordering::Relaxed) as *mut XmlChar) as *mut c_char;
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

macro_rules! GROW {
    ($ctxt:expr) => {
        if (*$ctxt).progressive == 0
            && ((*(*$ctxt).input).end.offset_from((*(*$ctxt).input).cur) as usize) < INPUT_CHUNK
        {
            xml_parser_grow($ctxt);
        }
    };
}

/**
 * xmlFatalErr:
 * @ctxt:  an XML parser context
 * @error:  the error number
 * @extra:  extra information string
 *
 * Handle a fatal parser error, i.e. violating Well-Formedness constraints
 */
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
        XmlParserErrors::XmlErrEntityPeInternal => {
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
        XmlParserErrors::XmlErrMisplacedCdataEnd => {
            c"Sequence ']]>' not allowed in content".as_ptr() as _
        }
        XmlParserErrors::XmlErrUriRequired => c"SYSTEM or PUBLIC, the URI is missing".as_ptr() as _,
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
        XmlParserErrors::XmlErrUriFragment => c"Fragment not allowed".as_ptr() as _,
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
        XmlParserErrors::XmlErrPcdataRequired => {
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
        XmlParserErrors::XmlErrLtslashRequired => c"EndTag: '</' not found".as_ptr() as _,
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
            null_mut(),
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser as i32,
            error as i32,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            info,
            null_mut(),
            null_mut(),
            0,
            0,
            c"%s\n".as_ptr() as _,
            errmsg
        );
    } else {
        __xml_raise_error!(
            None,
            None,
            null_mut(),
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser as i32,
            error as i32,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            info,
            null_mut(),
            null_mut(),
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

/**
 * xmlPushInput:
 * @ctxt:  an XML parser context
 * @input:  an XML parser input fragment (entity, XML fragment ...).
 *
 * xmlPushInput: match to a new input stream which is stacked on top
 *               of the previous one(s).
 * Returns -1 in case of error or the index in the input stack
 */
pub unsafe extern "C" fn xml_push_input(ctxt: XmlParserCtxtPtr, input: XmlParserInputPtr) -> c_int {
    if input.is_null() {
        return -1;
    }

    if *xml_parser_debug_entities() != 0 {
        if !(*ctxt).input.is_null() && !(*(*ctxt).input).filename.is_null() {
            xml_generic_error!(
                xml_generic_error_context(),
                c"%s(%d): ".as_ptr() as _,
                (*(*ctxt).input).filename,
                (*(*ctxt).input).line
            );
        }
        xml_generic_error!(
            xml_generic_error_context(),
            c"Pushing input %d : %.30s\n".as_ptr() as _,
            (*ctxt).input_nr + 1,
            (*input).cur
        );
    }
    if ((*ctxt).input_nr > 40 && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0)
        || (*ctxt).input_nr > 100
    {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, null());
        #[allow(clippy::while_immutable_condition)]
        while (*ctxt).input_nr > 1 {
            xml_free_input_stream(input_pop(ctxt));
        }
        return -1;
    }
    let ret: c_int = input_push(ctxt, input);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }
    GROW!(ctxt);
    ret
}

/**
 * xmlFreeInputStream:
 * @input:  an xmlParserInputPtr
 *
 * Free up an input stream.
 */
pub unsafe extern "C" fn xml_free_input_stream(input: XmlParserInputPtr) {
    if input.is_null() {
        return;
    }

    if !(*input).filename.is_null() {
        xml_free((*input).filename as _);
    }
    if !(*input).directory.is_null() {
        xml_free((*input).directory as _);
    }
    if !(*input).encoding.is_null() {
        xml_free((*input).encoding as _);
    }
    if !(*input).version.is_null() {
        xml_free((*input).version as _);
    }
    if !(*input).base.is_null() {
        if let Some(free) = (*input).free {
            free((*input).base as _);
        }
    }
    if !(*input).buf.is_null() {
        xml_free_parser_input_buffer((*input).buf);
    }
    xml_free(input as _);
}

/**
 * xmlNewInputFromFile:
 * @ctxt:  an XML parser context
 * @filename:  the filename to use as entity
 *
 * Create a new input stream based on a file or an URL.
 *
 * Returns the new input stream or NULL in case of error
 */
pub unsafe extern "C" fn xml_new_input_from_file(
    ctxt: XmlParserCtxtPtr,
    filename: *const c_char,
) -> XmlParserInputPtr {
    let mut input_stream: XmlParserInputPtr;

    if *xml_parser_debug_entities() != 0 {
        xml_generic_error!(
            xml_generic_error_context(),
            c"new input from file: %s\n".as_ptr() as _,
            filename
        );
    }
    if ctxt.is_null() {
        return null_mut();
    }
    let buf: XmlParserInputBufferPtr =
        xml_parser_input_buffer_create_filename(filename, XmlCharEncoding::XmlCharEncodingNone);
    if buf.is_null() {
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
    }

    input_stream = xml_new_input_stream(ctxt);
    if input_stream.is_null() {
        xml_free_parser_input_buffer(buf);
        return null_mut();
    }

    (*input_stream).buf = buf;
    input_stream = xml_check_http_input(ctxt, input_stream);
    if input_stream.is_null() {
        return null_mut();
    }

    let uri = if (*input_stream).filename.is_null() {
        xml_strdup(filename as *mut XmlChar)
    } else {
        xml_strdup((*input_stream).filename as *mut XmlChar)
    };
    let directory: *mut c_char = xml_parser_get_directory(uri as *const c_char);
    if !(*input_stream).filename.is_null() {
        xml_free((*input_stream).filename as _);
    }
    (*input_stream).filename = xml_canonic_path(uri as *const XmlChar) as *mut c_char;
    if !uri.is_null() {
        xml_free(uri as _);
    }
    (*input_stream).directory = directory;

    xml_buf_reset_input((*(*input_stream).buf).buffer, input_stream);
    if (*ctxt).directory.is_null() && !directory.is_null() {
        (*ctxt).directory = xml_strdup(directory as *const XmlChar) as *mut c_char;
    }
    input_stream
}

/**
 * xmlNewInputStream:
 * @ctxt:  an XML parser context
 *
 * Create a new input stream structure.
 *
 * Returns the new input stream or NULL
 */
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
    (*input).line = 1;
    (*input).col = 1;
    (*input).standalone = -1;

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

macro_rules! CUR_CHAR {
    ($ctxt:expr, $l:expr) => {
        xml_current_char($ctxt, addr_of_mut!($l))
    };
}
macro_rules! CUR_SCHAR {
    ($ctxt:expr, $s:expr, $l:expr) => {
        xml_string_current_char($ctxt, $s, addr_of_mut!($l))
    };
}

/**
 * Namespaces.
 */
/**
 * xmlSplitQName:
 * @ctxt:  an XML parser context
 * @name:  an XML parser context
 * @prefix:  a xmlChar **
 *
 * parse an UTF8 encoded XML qualified name string
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
pub unsafe extern "C" fn xml_split_qname(
    ctxt: XmlParserCtxtPtr,
    name: *const XmlChar,
    prefix: *mut *mut XmlChar,
) -> *mut XmlChar {
    let mut buf: [XmlChar; XML_MAX_NAMELEN + 5] = [0; XML_MAX_NAMELEN + 5];
    let mut buffer: *mut XmlChar = null_mut();
    let mut len: c_int = 0;
    let mut max: c_int = XML_MAX_NAMELEN as i32;
    let mut ret: *mut XmlChar;
    let mut cur: *const XmlChar = name;
    let mut c: c_int;

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
            let mut l: c_int = 0;
            let first: c_int = CUR_SCHAR!(ctxt, cur, l);

            if !IS_LETTER!(first as u32) && first != b'_' as i32 {
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
    let mut len: c_int = 0;
    let mut l: c_int = 0;
    let mut c: i32;
    let max_length: c_int = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };

    // #ifdef DEBUG
    //     nbParseNameComplex++;
    // #endif

    /*
     * Handler for more complex cases
     */
    c = CUR_CHAR!(ctxt, l);
    if (*ctxt).options & XmlParserOption::XmlParseOld10 as i32 == 0 {
        /*
         * Use the new checks of production [4] [4a] amd [5] of the
         * Update 5 of XML-1.0
         */
        if c == b' ' as i32
            || c == b'>' as i32
            || c == b'/' as i32 /* accelerators */
            || (!((b'a' as i32..=b'z' as i32).contains(&c)
                || (b'A' as i32..=b'Z' as i32).contains(&c)
                || c == b'_' as i32
                || c == b':' as i32
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
                || (0x10000..=0xEFFFF).contains(&c)))
        {
            return null_mut();
        }
        len += l;
        NEXTL!(ctxt, l);
        c = CUR_CHAR!(ctxt, l);
        while c != b' ' as i32
            && c != b'>' as i32
            && c != b'/' as i32
            && ((b'a' as i32..=b'z' as i32).contains(&c)
                || (b'A' as i32..=b'Z' as i32).contains(&c)
                || (b'0' as i32..=b'9' as i32).contains(&c)
                || c == b'_' as i32
                || c == b':' as i32
                || c == b'-' as i32
                || c == b'.' as i32
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
        {
            if len <= INT_MAX - l {
                len += l;
            }
            NEXTL!(ctxt, l);
            c = CUR_CHAR!(ctxt, l);
        }
    } else {
        if c == b' ' as i32
            || c == b'>' as i32
            || c == b'/' as i32 /* accelerators */
            || (!IS_LETTER!(c as u32) && c != b'_' as i32 && c != b':' as i32)
        {
            return null_mut();
        }
        len += l;
        NEXTL!(ctxt, l);
        c = CUR_CHAR!(ctxt, l);

        while c != b' ' as i32
            && c != b'>' as i32
            && c != b'/' as i32 /* test bigname.xml */
            && (IS_LETTER!(c as u32)
                || IS_DIGIT!(c as u32)
                || c == b'.' as i32
                || c == b'-' as i32
                || c == b'_' as i32
                || c == b':' as i32
                || IS_COMBINING!(c as u32)
                || IS_EXTENDER!(c as u32))
        {
            if len <= INT_MAX - l {
                len += l;
            }
            NEXTL!(ctxt, l);
            c = CUR_CHAR!(ctxt, l);
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

/**
 * Generic production rules.
 */
/**
 * xmlParseName:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an XML name.
 *
 * [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' |
 *                  CombiningChar | Extender
 *
 * [5] Name ::= (Letter | '_' | ':') (NameChar)*
 *
 * [6] Names ::= Name (#x20 Name)*
 *
 * Returns the Name parsed or NULL
 */
pub(crate) unsafe extern "C" fn xml_parse_name(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    let mut input: *const XmlChar;
    let ret: *const XmlChar;
    let count: size_t;
    let max_length: size_t = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH
    } else {
        XML_MAX_NAME_LENGTH
    };

    GROW!(ctxt);
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

/**
 * xmlParseNmtoken:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an XML Nmtoken.
 *
 * [7] Nmtoken ::= (NameChar)+
 *
 * [8] Nmtokens ::= Nmtoken (#x20 Nmtoken)*
 *
 * Returns the Nmtoken parsed or NULL
 */
pub(crate) unsafe extern "C" fn xml_parse_nmtoken(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    let mut buf: [XmlChar; XML_MAX_NAMELEN + 5] = [0; XML_MAX_NAMELEN + 5];
    let mut len: c_int = 0;
    let mut l: c_int = 0;
    let mut c: c_int;
    let max_length: c_int = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };

    c = CUR_CHAR!(ctxt, l);

    while xml_is_name_char(ctxt, c) != 0 {
        COPY_BUF!(l, buf.as_mut_ptr(), len, c);
        NEXTL!(ctxt, l);
        c = CUR_CHAR!(ctxt, l);
        if len >= XML_MAX_NAMELEN as i32 {
            /*
             * Okay someone managed to make a huge token, so he's ready to pay
             * for the processing speed.
             */
            let mut buffer: *mut XmlChar;
            let mut max: c_int = len * 2;

            buffer = xml_malloc_atomic(max as usize) as *mut XmlChar;
            if buffer.is_null() {
                xml_err_memory(ctxt, null());
                return null_mut();
            }
            memcpy(buffer as _, buf.as_ptr() as _, len as usize);
            while xml_is_name_char(ctxt, c) != 0 {
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
                c = CUR_CHAR!(ctxt, l);
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

/**
 * xmlParseEntityValue:
 * @ctxt:  an XML parser context
 * @orig:  if non-NULL store a copy of the original entity value
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse a value for ENTITY declarations
 *
 * [9] EntityValue ::= '"' ([^%&"] | PEReference | Reference)* '"' |
 *                   "'" ([^%&'] | PEReference | Reference)* "'"
 *
 * Returns the EntityValue parsed with reference substituted or NULL
 */
pub(crate) unsafe extern "C" fn xml_parse_entity_value(
    ctxt: XmlParserCtxtPtr,
    orig: *mut *mut XmlChar,
) -> *mut XmlChar {
    let mut buf: *mut XmlChar;
    let mut len: c_int = 0;
    let mut size: c_int = XML_PARSER_BUFFER_SIZE as _;
    let mut c: c_int;
    let mut l: c_int = 0;
    let max_length: c_int = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH as i32
    } else {
        XML_MAX_TEXT_LENGTH as i32
    };
    let stop: XmlChar;
    let mut ret: *mut XmlChar = null_mut();
    let mut cur: *const XmlChar;

    if RAW!(ctxt) == b'"' {
        stop = b'"';
    } else if RAW!(ctxt) == b'\'' {
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
    GROW!(ctxt);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        //  goto error;
        if !buf.is_null() {
            xml_free(buf as _);
        }
        return ret;
    }
    NEXT!(ctxt);
    c = CUR_CHAR!(ctxt, l);
    /*
     * NOTE: 4.4.5 Included in Literal
     * When a parameter entity reference appears in a literal entity
     * value, ... a single or double quote character in the replacement
     * text is always treated as a normal data character and will not
     * terminate the literal.
     * In practice it means we stop the loop only when back at parsing
     * the initial entity and the quote is found
     */
    while (IS_CHAR!(c) && (c != stop as i32 || (*ctxt).input != input))
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

        GROW!(ctxt);
        c = CUR_CHAR!(ctxt, l);
        if c == 0 {
            GROW!(ctxt);
            c = CUR_CHAR!(ctxt, l);
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
    if c != stop as i32 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityNotFinished, null());
        //  goto error;
        if !buf.is_null() {
            xml_free(buf as _);
        }
        return ret;
    }
    NEXT!(ctxt);

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
            let mut name_ok: c_int = 0;

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
            if tmp == b'%' && (*ctxt).in_subset == 1 && (*ctxt).input_nr == 1 {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityPeInternal, null());
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

/**
 * xmlParseAttValue:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse a value for an attribute
 * Note: the parser won't do substitution of entities here, this
 * will be handled later in xmlStringGetNodeList
 *
 * [10] AttValue ::= '"' ([^<&"] | Reference)* '"' |
 *                   "'" ([^<&'] | Reference)* "'"
 *
 * 3.3.3 Attribute-Value Normalization:
 * Before the value of an attribute is passed to the application or
 * checked for validity, the XML processor must normalize it as follows:
 * - a character reference is processed by appending the referenced
 *   character to the attribute value
 * - an entity reference is processed by recursively processing the
 *   replacement text of the entity
 * - a whitespace character (#x20, #xD, #xA, #x9) is processed by
 *   appending #x20 to the normalized value, except that only a single
 *   #x20 is appended for a "#xD#xA" sequence that is part of an external
 *   parsed entity or the literal entity value of an internal parsed entity
 * - other characters are processed by appending them to the normalized value
 *   If the declared value is not CDATA, then the XML processor must further
 *   process the normalized attribute value by discarding any leading and
 *   trailing space (#x20) characters, and by replacing sequences of space
 *   (#x20) characters by a single space (#x20) character.
 *   All attributes for which no declaration has been read should be treated
 *   by a non-validating parser as if declared CDATA.
 *
 * Returns the AttValue parsed or NULL. The value has to be freed by the caller.
 */
pub(crate) unsafe extern "C" fn xml_parse_att_value(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    if ctxt.is_null() || (*ctxt).input.is_null() {
        return null_mut();
    }
    xml_parse_att_value_internal(ctxt, null_mut(), null_mut(), 0)
}

/**
 * xmlParseSystemLiteral:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an XML Literal
 *
 * [11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")
 *
 * Returns the SystemLiteral parsed or NULL
 */
pub(crate) unsafe extern "C" fn xml_parse_system_literal(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    let mut buf: *mut XmlChar;
    let mut len: c_int = 0;
    let mut size: c_int = XML_PARSER_BUFFER_SIZE as _;
    let mut cur: c_int;
    let mut l: c_int = 0;
    let max_length: c_int = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };
    let state: c_int = (*ctxt).instate as i32;

    let stop = if RAW!(ctxt) == b'"' {
        NEXT!(ctxt);
        b'"'
    } else if RAW!(ctxt) == b'\'' {
        NEXT!(ctxt);
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
    cur = CUR_CHAR!(ctxt, l);
    while IS_CHAR!(cur) && cur != stop as i32 {
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
        cur = CUR_CHAR!(ctxt, l);
    }
    *buf.add(len as usize) = 0;
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        xml_free(buf as _);
        return null_mut();
    }
    (*ctxt).instate = XmlParserInputState::try_from(state).unwrap();
    if !IS_CHAR!(cur) {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotFinished, null());
    } else {
        NEXT!(ctxt);
    }
    buf
}

/**
 * xmlParseCharData:
 * @ctxt:  an XML parser context
 * @cdata:  unused
 *
 * DEPRECATED: Internal function, don't use.
 */
pub(crate) unsafe extern "C" fn xml_parse_char_data(ctxt: XmlParserCtxtPtr, _cdata: c_int) {
    xml_parse_char_data_internal(ctxt, 0);
}

macro_rules! CMP4 {
    ( $s:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr ) => {
        *($s as *mut c_uchar).add(0) == $c1
            && *($s as *mut c_uchar).add(1) == $c2
            && *($s as *mut c_uchar).add(2) == $c3
            && *($s as *mut c_uchar).add(3) == $c4
    };
}
macro_rules! CMP5 {
    ( $s:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr, $c5:expr ) => {
        (CMP4!($s, $c1, $c2, $c3, $c4) && *($s as *mut c_uchar).add(4) == $c5)
    };
}
macro_rules! CMP6 {
    ( $s:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr, $c5:expr, $c6:expr ) => {
        (CMP5!($s, $c1, $c2, $c3, $c4, $c5) && *($s as *mut c_uchar).add(5) == $c6)
    };
}
macro_rules! CMP7 {
    ( $s:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr, $c5:expr, $c6:expr, $c7:expr ) => {
        (CMP6!($s, $c1, $c2, $c3, $c4, $c5, $c6) && *($s as *mut c_uchar).add(6) == $c7)
    };
}
macro_rules! CMP8 {
    ( $s:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr, $c5:expr, $c6:expr, $c7:expr, $c8:expr ) => {
        (CMP7!($s, $c1, $c2, $c3, $c4, $c5, $c6, $c7) && *($s as *mut c_uchar).add(7) == $c8)
    };
}
macro_rules! CMP9 {
    ( $s:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr, $c5:expr, $c6:expr, $c7:expr, $c8:expr, $c9:expr ) => {
        (CMP8!($s, $c1, $c2, $c3, $c4, $c5, $c6, $c7, $c8) && *($s as *mut c_uchar).add(8) == $c9)
    };
}
macro_rules! CMP10 {
    ( $s:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr, $c5:expr, $c6:expr, $c7:expr, $c8:expr, $c9:expr, $c10:expr ) => {
        (CMP9!($s, $c1, $c2, $c3, $c4, $c5, $c6, $c7, $c8, $c9)
            && *($s as *mut c_uchar).add(9) == $c10)
    };
}

macro_rules! CUR_PTR {
    ($ctxt:expr) => {
        (*(*$ctxt).input).cur
    };
}

macro_rules! SKIP {
    ($ctxt:expr, $val:expr) => {
        (*(*$ctxt).input).cur = (*(*$ctxt).input).cur.add($val as usize);
        (*(*$ctxt).input).col += $val;
        if *(*(*$ctxt).input).cur == 0 {
            xml_parser_grow($ctxt);
        }
    };
}

macro_rules! SKIP_BLANKS {
    ($ctxt:expr) => {
        $crate::libxml::parser::xml_skip_blank_chars($ctxt)
    };
}

/**
 * xmlParseCommentComplex:
 * @ctxt:  an XML parser context
 * @buf:  the already parsed part of the buffer
 * @len:  number of bytes in the buffer
 * @size:  allocated size of the buffer
 *
 * Skip an XML (SGML) comment <!-- .... -->
 *  The spec says that "For compatibility, the string "--" (double-hyphen)
 *  must not occur within comments. "
 * This is the slow routine in case the accelerator for ascii didn't work
 *
 * [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
 */
unsafe extern "C" fn xml_parse_comment_complex(
    ctxt: XmlParserCtxtPtr,
    mut buf: *mut XmlChar,
    mut len: size_t,
    mut size: size_t,
) {
    let mut q: i32;
    let mut ql: c_int = 0;
    let mut r: i32;
    let mut rl: c_int = 0;
    let mut cur: i32;
    let mut l: c_int = 0;
    let max_length: size_t = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH
    } else {
        XML_MAX_TEXT_LENGTH
    };

    let inputid: c_int = (*(*ctxt).input).id;

    if buf.is_null() {
        len = 0;
        size = XML_PARSER_BUFFER_SIZE;
        buf = xml_malloc_atomic(size) as _;
        if buf.is_null() {
            xml_err_memory(ctxt, null());
            return;
        }
    }
    q = CUR_CHAR!(ctxt, ql);
    'not_terminated: {
        if q == 0 {
            break 'not_terminated;
        }
        if !IS_CHAR!(q) {
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
        r = CUR_CHAR!(ctxt, rl);
        if r == 0 {
            break 'not_terminated;
        }
        if !IS_CHAR!(r) {
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
        cur = CUR_CHAR!(ctxt, l);
        if cur == 0 {
            break 'not_terminated;
        }
        while IS_CHAR!(cur) && (cur != b'>' as i32 || r != b'-' as i32 || q != b'-' as i32) {
            if r == b'-' as i32 && q == b'-' as i32 {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrHyphenInComment, null());
            }
            if len + 5 >= size {
                let new_size: size_t = size * 2;
                let new_buf: *mut XmlChar = xml_realloc(buf as _, new_size as usize) as _;
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
            cur = CUR_CHAR!(ctxt, l);
        }
        *buf.add(len) = 0;
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            xml_free(buf as _);
            return;
        }
        if cur == 0 {
            xml_fatal_err_msg_str(
                ctxt,
                XmlParserErrors::XmlErrCommentNotFinished,
                c"Comment not terminated \n<!--%.50s\n".as_ptr() as _,
                buf,
            );
        } else if !IS_CHAR!(cur) {
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
            NEXT!(ctxt);
            if !(*ctxt).sax.is_null()
                && (*(*ctxt).sax).comment.is_some()
                && (*ctxt).disable_sax == 0
            {
                ((*(*ctxt).sax).comment.unwrap())((*ctxt).user_data, buf);
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

macro_rules! SHRINK {
    ($ctxt:expr) => {
        if (*$ctxt).progressive == 0
            && (*(*$ctxt).input).cur.offset_from((*(*$ctxt).input).base) > 2 * INPUT_CHUNK as isize
            && (*(*$ctxt).input).end.offset_from((*(*$ctxt).input).cur) < 2 * INPUT_CHUNK as isize
        {
            xml_parser_shrink($ctxt);
        }
    };
}

/**
 * xmlParseComment:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse an XML (SGML) comment. Always consumes '<!'.
 *
 *  The spec says that "For compatibility, the string "--" (double-hyphen)
 *  must not occur within comments. "
 *
 * [15] Comment ::= '<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'
 */
pub(crate) unsafe extern "C" fn xml_parse_comment(ctxt: XmlParserCtxtPtr) {
    let mut buf: *mut XmlChar = null_mut();
    let mut size: size_t = XML_PARSER_BUFFER_SIZE;
    let mut len: size_t = 0;
    let max_length: size_t = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH
    } else {
        XML_MAX_TEXT_LENGTH
    };
    let mut input: *const XmlChar;
    let mut nbchar: size_t;
    let mut ccol: c_int;

    /*
     * Check that there is a comment right here.
     */
    if RAW!(ctxt) != b'<' || NXT!(ctxt, 1) != b'!' {
        return;
    }
    SKIP!(ctxt, 2);
    if RAW!(ctxt) != b'-' || NXT!(ctxt, 1) != b'-' {
        return;
    }
    let state: XmlParserInputState = (*ctxt).instate;
    (*ctxt).instate = XmlParserInputState::XmlParserComment;
    let inputid: c_int = (*(*ctxt).input).id;
    SKIP!(ctxt, 2);
    GROW!(ctxt);

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
                    let new_buf: *mut XmlChar =
                        xml_realloc(buf as _, size as usize) as *mut XmlChar;
                    if new_buf.is_null() {
                        xml_free(buf as _);
                        xml_err_memory(ctxt, null());
                        (*ctxt).instate = state;
                        return;
                    }
                    buf = new_buf;
                }
                memcpy(
                    buf.add(len as usize) as _,
                    (*(*ctxt).input).cur as _,
                    nbchar as usize,
                );
                len += nbchar;
                *buf.add(len as usize) = 0;
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
            SHRINK!(ctxt);
            GROW!(ctxt);
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
                        SKIP!(ctxt, 3);
                        if !(*ctxt).sax.is_null()
                            && (*(*ctxt).sax).comment.is_some()
                            && (*ctxt).disable_sax == 0
                        {
                            if !buf.is_null() {
                                ((*(*ctxt).sax).comment.unwrap())((*ctxt).user_data, buf);
                            } else {
                                ((*(*ctxt).sax).comment.unwrap())(
                                    (*ctxt).user_data,
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

/*
 * List of XML prefixed PI allowed by W3C specs
 */

const XML_W3_CPIS: &[*const c_char] = &[
    c"xml-stylesheet".as_ptr() as _,
    c"xml-model".as_ptr() as _,
    null(),
];

/**
 * xmlParsePITarget:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse the name of a PI
 *
 * [17] PITarget ::= Name - (('X' | 'x') ('M' | 'm') ('L' | 'l'))
 *
 * Returns the PITarget name or NULL
 */
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

/**
 * xmlParseCatalogPI:
 * @ctxt:  an XML parser context
 * @catalog:  the PI value string
 *
 * parse an XML Catalog Processing Instruction.
 *
 * <?oasis-xml-catalog catalog="http://example.com/catalog.xml"?>
 *
 * Occurs only if allowed by the user and if happening in the Misc
 * part of the document before any doctype information
 * This will add the given catalog to the parsing context in order
 * to be used if there is a resolution need further down in the document
 */
#[cfg(feature = "catalog")]
unsafe extern "C" fn xml_parse_catalog_pi(ctxt: XmlParserCtxtPtr, catalog: *const XmlChar) {
    let mut url: *mut XmlChar = null_mut();
    let mut tmp: *const XmlChar;
    let base: *const XmlChar;
    let marker: XmlChar;

    tmp = catalog;
    while IS_BLANK_CH!(*tmp) {
        tmp = tmp.add(1);
    }
    'error: {
        if xml_strncmp(tmp, c"catalog".as_ptr() as _, 7) != 0 {
            break 'error;
        }
        tmp = tmp.add(7);
        while IS_BLANK_CH!(*tmp) {
            tmp = tmp.add(1);
        }
        if *tmp != b'=' {
            return;
        }
        tmp = tmp.add(1);
        while IS_BLANK_CH!(*tmp) {
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
        while IS_BLANK_CH!(*tmp) {
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

/**
 * xmlParsePI:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an XML Processing Instruction.
 *
 * [16] PI ::= '<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'
 *
 * The processing is transferred to SAX once parsed.
 */
pub(crate) unsafe extern "C" fn xml_parse_pi(ctxt: XmlParserCtxtPtr) {
    let mut buf: *mut XmlChar;
    let mut len: size_t = 0;
    let mut size: size_t = XML_PARSER_BUFFER_SIZE;
    let max_length: size_t = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH
    } else {
        XML_MAX_TEXT_LENGTH
    };
    let mut cur: c_int;
    let mut l: c_int = 0;
    let target: *const XmlChar;
    let state: XmlParserInputState;

    if RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'?' {
        let inputid: c_int = (*(*ctxt).input).id;
        state = (*ctxt).instate;
        (*ctxt).instate = XmlParserInputState::XmlParserPI;
        /*
         * this is a Processing Instruction.
         */
        SKIP!(ctxt, 2);

        /*
         * Parse the target name and check for special support like
         * namespace.
         */
        target = xml_parse_pi_target(ctxt);
        if !target.is_null() {
            if RAW!(ctxt) == b'?' && NXT!(ctxt, 1) == b'>' {
                if inputid != (*(*ctxt).input).id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        c"PI declaration doesn't start and stop in the same entity\n".as_ptr() as _,
                    );
                }
                SKIP!(ctxt, 2);

                /*
                 * SAX: PI detected.
                 */
                if !(*ctxt).sax.is_null()
                    && (*ctxt).disable_sax == 0
                    && (*(*ctxt).sax).processing_instruction.is_some()
                {
                    ((*(*ctxt).sax).processing_instruction.unwrap())(
                        (*ctxt).user_data,
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
            if SKIP_BLANKS!(ctxt) == 0 {
                xml_fatal_err_msg_str(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    c"ParsePI: PI %s space expected\n".as_ptr() as _,
                    target,
                );
            }
            cur = CUR_CHAR!(ctxt, l);
            while IS_CHAR!(cur) && (cur != b'?' as i32 || NXT!(ctxt, 1) != b'>') {
                if len + 5 >= size {
                    let new_size: size_t = size * 2;
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
                cur = CUR_CHAR!(ctxt, l);
            }
            *buf.add(len as usize) = 0;
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                xml_free(buf as _);
                return;
            }
            if cur != b'?' as i32 {
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
                SKIP!(ctxt, 2);

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
                        pi((*ctxt).user_data, target, buf);
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

/**
 * xmlParseDefaultDecl:
 * @ctxt:  an XML parser context
 * @value:  Receive a possible fixed default value for the attribute
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse an attribute default declaration
 *
 * [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
 *
 * [ VC: Required Attribute ]
 * if the default declaration is the keyword #REQUIRED, then the
 * attribute must be specified for all elements of the type in the
 * attribute-list declaration.
 *
 * [ VC: Attribute Default Legal ]
 * The declared default value must meet the lexical constraints of
 * the declared attribute type c.f. xmlValidateAttributeDecl()
 *
 * [ VC: Fixed Attribute Default ]
 * if an attribute has a default value declared with the #FIXED
 * keyword, instances of that attribute must match the default value.
 *
 * [ WFC: No < in Attribute Values ]
 * handled in xmlParseAttValue()
 *
 * returns: XML_ATTRIBUTE_NONE, XML_ATTRIBUTE_REQUIRED, XML_ATTRIBUTE_IMPLIED
 *          or XML_ATTRIBUTE_FIXED.
 */
pub(crate) unsafe extern "C" fn xml_parse_default_decl(
    ctxt: XmlParserCtxtPtr,
    value: *mut *mut XmlChar,
) -> c_int {
    let mut val: c_int;

    *value = null_mut();
    if CMP9!(
        CUR_PTR!(ctxt),
        b'#',
        b'R',
        b'E',
        b'Q',
        b'U',
        b'I',
        b'R',
        b'E',
        b'D'
    ) {
        SKIP!(ctxt, 9);
        return XmlAttributeDefault::XmlAttributeRequired as i32;
    }
    if CMP8!(
        CUR_PTR!(ctxt),
        b'#',
        b'I',
        b'M',
        b'P',
        b'L',
        b'I',
        b'E',
        b'D'
    ) {
        SKIP!(ctxt, 8);
        return XmlAttributeDefault::XmlAttributeImplied as i32;
    }
    val = XmlAttributeDefault::XmlAttributeNone as i32;
    if CMP6!(CUR_PTR!(ctxt), b'#', b'F', b'I', b'X', b'E', b'D') {
        SKIP!(ctxt, 6);
        val = XmlAttributeDefault::XmlAttributeFixed as i32;
        if SKIP_BLANKS!(ctxt) == 0 {
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

/**
 * xmlParseNotationType:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an Notation attribute type.
 *
 * Note: the leading 'NOTATION' S part has already being parsed...
 *
 * [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
 *
 * [ VC: Notation Attributes ]
 * Values of this type must match one of the notation names included
 * in the declaration; all notation names in the declaration must be declared.
 *
 * Returns: the notation attribute tree built while parsing
 */
pub(crate) unsafe extern "C" fn xml_parse_notation_type(
    ctxt: XmlParserCtxtPtr,
) -> XmlEnumerationPtr {
    let mut name: *const XmlChar;
    let mut ret: XmlEnumerationPtr = null_mut();
    let mut last: XmlEnumerationPtr = null_mut();
    let mut cur: XmlEnumerationPtr;
    let mut tmp: XmlEnumerationPtr;

    if RAW!(ctxt) != b'(' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotStarted, null());
        return null_mut();
    }
    while {
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
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
            if xml_str_equal(name, (*tmp).name) {
                xml_validity_error(
                    ctxt,
                    XmlParserErrors::XmlDtdDupToken,
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
            cur = xml_create_enumeration(name);
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
        SKIP_BLANKS!(ctxt);

        RAW!(ctxt) == b'|'
    } {}
    if RAW!(ctxt) != b')' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotFinished, null());
        xml_free_enumeration(ret);
        return null_mut();
    }
    NEXT!(ctxt);
    ret
}

/**
 * xmlParseEnumerationType:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an Enumeration attribute type.
 *
 * [59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
 *
 * [ VC: Enumeration ]
 * Values of this type must match one of the Nmtoken tokens in
 * the declaration
 *
 * Returns: the enumeration attribute tree built while parsing
 */
pub(crate) unsafe extern "C" fn xml_parse_enumeration_type(
    ctxt: XmlParserCtxtPtr,
) -> XmlEnumerationPtr {
    let mut name: *mut XmlChar;
    let mut ret: XmlEnumerationPtr = null_mut();
    let mut last: XmlEnumerationPtr = null_mut();
    let mut cur: XmlEnumerationPtr;
    let mut tmp: XmlEnumerationPtr;

    if RAW!(ctxt) != b'(' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttlistNotStarted, null());
        return null_mut();
    }
    while {
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
        name = xml_parse_nmtoken(ctxt);
        if name.is_null() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNmtokenRequired, null());
            return ret;
        }
        tmp = ret;
        while !tmp.is_null() {
            if xml_str_equal(name, (*tmp).name) {
                xml_validity_error(
                    ctxt,
                    XmlParserErrors::XmlDtdDupToken,
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
            cur = xml_create_enumeration(name);
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
        SKIP_BLANKS!(ctxt);

        RAW!(ctxt) == b'|'
    } {}
    if RAW!(ctxt) != b')' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttlistNotFinished, null());
        return ret;
    }
    NEXT!(ctxt);
    ret
}

/**
 * xmlParseEnumeratedType:
 * @ctxt:  an XML parser context
 * @tree:  the enumeration tree built while parsing
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an Enumerated attribute type.
 *
 * [57] EnumeratedType ::= NotationType | Enumeration
 *
 * [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
 *
 *
 * Returns: XML_ATTRIBUTE_ENUMERATION or XML_ATTRIBUTE_NOTATION
 */
pub(crate) unsafe extern "C" fn xml_parse_enumerated_type(
    ctxt: XmlParserCtxtPtr,
    tree: *mut XmlEnumerationPtr,
) -> c_int {
    if CMP8!(
        CUR_PTR!(ctxt),
        b'N',
        b'O',
        b'T',
        b'A',
        b'T',
        b'I',
        b'O',
        b'N'
    ) {
        SKIP!(ctxt, 8);
        if SKIP_BLANKS!(ctxt) == 0 {
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

/**
 * xmlParseAttributeType:
 * @ctxt:  an XML parser context
 * @tree:  the enumeration tree built while parsing
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse the Attribute list def for an element
 *
 * [54] AttType ::= StringType | TokenizedType | EnumeratedType
 *
 * [55] StringType ::= 'CDATA'
 *
 * [56] TokenizedType ::= 'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' |
 *                        'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'
 *
 * Validity constraints for attribute values syntax are checked in
 * xmlValidateAttributeValue()
 *
 * [ VC: ID ]
 * Values of type ID must match the Name production. A name must not
 * appear more than once in an XML document as a value of this type;
 * i.e., ID values must uniquely identify the elements which bear them.
 *
 * [ VC: One ID per Element Type ]
 * No element type may have more than one ID attribute specified.
 *
 * [ VC: ID Attribute Default ]
 * An ID attribute must have a declared default of #IMPLIED or #REQUIRED.
 *
 * [ VC: IDREF ]
 * Values of type IDREF must match the Name production, and values
 * of type IDREFS must match Names; each IDREF Name must match the value
 * of an ID attribute on some element in the XML document; i.e. IDREF
 * values must match the value of some ID attribute.
 *
 * [ VC: Entity Name ]
 * Values of type ENTITY must match the Name production, values
 * of type ENTITIES must match Names; each Entity Name must match the
 * name of an unparsed entity declared in the DTD.
 *
 * [ VC: Name Token ]
 * Values of type NMTOKEN must match the Nmtoken production; values
 * of type NMTOKENS must match Nmtokens.
 *
 * Returns the attribute type
 */
pub(crate) unsafe extern "C" fn xml_parse_attribute_type(
    ctxt: XmlParserCtxtPtr,
    tree: *mut XmlEnumerationPtr,
) -> c_int {
    if CMP5!(CUR_PTR!(ctxt), b'C', b'D', b'A', b'T', b'A') {
        SKIP!(ctxt, 5);
        return XmlAttributeType::XmlAttributeCdata as i32;
    } else if CMP6!(CUR_PTR!(ctxt), b'I', b'D', b'R', b'E', b'F', b'S') {
        SKIP!(ctxt, 6);
        return XmlAttributeType::XmlAttributeIdrefs as i32;
    } else if CMP5!(CUR_PTR!(ctxt), b'I', b'D', b'R', b'E', b'F') {
        SKIP!(ctxt, 5);
        return XmlAttributeType::XmlAttributeIdref as i32;
    } else if RAW!(ctxt) == b'I' && NXT!(ctxt, 1) == b'D' {
        SKIP!(ctxt, 2);
        return XmlAttributeType::XmlAttributeId as i32;
    } else if CMP6!(CUR_PTR!(ctxt), b'E', b'N', b'T', b'I', b'T', b'Y') {
        SKIP!(ctxt, 6);
        return XmlAttributeType::XmlAttributeEntity as i32;
    } else if CMP8!(
        CUR_PTR!(ctxt),
        b'E',
        b'N',
        b'T',
        b'I',
        b'T',
        b'I',
        b'E',
        b'S'
    ) {
        SKIP!(ctxt, 8);
        return XmlAttributeType::XmlAttributeEntities as i32;
    } else if CMP8!(
        CUR_PTR!(ctxt),
        b'N',
        b'M',
        b'T',
        b'O',
        b'K',
        b'E',
        b'N',
        b'S'
    ) {
        SKIP!(ctxt, 8);
        return XmlAttributeType::XmlAttributeNmtokens as i32;
    } else if CMP7!(CUR_PTR!(ctxt), b'N', b'M', b'T', b'O', b'K', b'E', b'N') {
        SKIP!(ctxt, 7);
        return XmlAttributeType::XmlAttributeNmtoken as i32;
    }
    xml_parse_enumerated_type(ctxt, tree)
}

/**
 * xmlAddDefAttrs:
 * @ctxt:  an XML parser context
 * @fullname:  the element fullname
 * @fullattr:  the attribute fullname
 * @value:  the attribute value
 *
 * Add a defaulted attribute for an element
 */
pub(crate) unsafe extern "C" fn xml_add_def_attrs(
    ctxt: XmlParserCtxtPtr,
    fullname: *const XmlChar,
    fullattr: *const XmlChar,
    mut value: *const XmlChar,
) {
    let mut defaults: XmlDefAttrsPtr;
    let mut len: c_int = 0;
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

/**
 * xmlAddSpecialAttr:
 * @ctxt:  an XML parser context
 * @fullname:  the element fullname
 * @fullattr:  the attribute fullname
 * @type:  the attribute type
 *
 * Register this attribute type
 */
pub(crate) unsafe extern "C" fn xml_add_special_attr(
    ctxt: XmlParserCtxtPtr,
    fullname: *const XmlChar,
    fullattr: *const XmlChar,
    typ: c_int,
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

/**
 * xmlParseElementMixedContentDecl:
 * @ctxt:  an XML parser context
 * @inputchk:  the input used for the current entity, needed for boundary checks
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse the declaration for a Mixed Element content
 * The leading '(' and spaces have been skipped in xmlParseElementContentDecl
 *
 * [51] Mixed ::= '(' S? '#PCDATA' (S? '|' S? Name)* S? ')*' |
 *                '(' S? '#PCDATA' S? ')'
 *
 * [ VC: Proper Group/PE Nesting ] applies to [51] too (see [49])
 *
 * [ VC: No Duplicate Types ]
 * The same name must not appear more than once in a single
 * mixed-content declaration.
 *
 * returns: the list of the xmlElementContentPtr describing the element choices
 */
pub(crate) unsafe extern "C" fn xml_parse_element_mixed_content_decl(
    ctxt: XmlParserCtxtPtr,
    inputchk: c_int,
) -> XmlElementContentPtr {
    let mut ret: XmlElementContentPtr = null_mut();
    let mut cur: XmlElementContentPtr = null_mut();
    let mut n: XmlElementContentPtr;
    let mut elem: *const XmlChar = null();

    GROW!(ctxt);
    if CMP7!(CUR_PTR!(ctxt), b'#', b'P', b'C', b'D', b'A', b'T', b'A') {
        SKIP!(ctxt, 7);
        SKIP_BLANKS!(ctxt);
        if RAW!(ctxt) == b')' {
            if (*(*ctxt).input).id != inputchk {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    c"Element content declaration doesn't start and stop in the same entity\n"
                        .as_ptr() as _,
                );
            }
            NEXT!(ctxt);
            ret = xml_new_doc_element_content(
                (*ctxt).my_doc,
                null(),
                XmlElementContentType::XmlElementContentPcdata,
            );
            if ret.is_null() {
                return null_mut();
            }
            if RAW!(ctxt) == b'*' {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
                NEXT!(ctxt);
            }
            return ret;
        }
        if RAW!(ctxt) == b'(' || RAW!(ctxt) == b'|' {
            ret = xml_new_doc_element_content(
                (*ctxt).my_doc,
                null(),
                XmlElementContentType::XmlElementContentPcdata,
            );
            cur = ret;
            if ret.is_null() {
                return null_mut();
            }
        }
        #[allow(clippy::while_immutable_condition)]
        while RAW!(ctxt) == b'|' && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            NEXT!(ctxt);
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
            SKIP_BLANKS!(ctxt);
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
            SKIP_BLANKS!(ctxt);
            GROW!(ctxt);
        }
        if RAW!(ctxt) == b')' && NXT!(ctxt, 1) == b'*' {
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
            SKIP!(ctxt, 2);
        } else {
            xml_free_doc_element_content((*ctxt).my_doc, ret);
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrMixedNotStarted, null());
            return null_mut();
        }
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrPcdataRequired, null());
    }
    ret
}

/**
 * xmlParseElementChildrenContentDecl:
 * @ctxt:  an XML parser context
 * @inputchk:  the input used for the current entity, needed for boundary checks
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse the declaration for a Mixed Element content
 * The leading '(' and spaces have been skipped in xmlParseElementContentDecl
 *
 * [47] children ::= (choice | seq) ('?' | '*' | '+')?
 *
 * [48] cp ::= (Name | choice | seq) ('?' | '*' | '+')?
 *
 * [49] choice ::= '(' S? cp ( S? '|' S? cp )* S? ')'
 *
 * [50] seq ::= '(' S? cp ( S? ',' S? cp )* S? ')'
 *
 * [ VC: Proper Group/PE Nesting ] applies to [49] and [50]
 * TODO Parameter-entity replacement text must be properly nested
 *    with parenthesized groups. That is to say, if either of the
 *    opening or closing parentheses in a choice, seq, or Mixed
 *    construct is contained in the replacement text for a parameter
 *    entity, both must be contained in the same replacement text. For
 *    interoperability, if a parameter-entity reference appears in a
 *    choice, seq, or Mixed construct, its replacement text should not
 *    be empty, and neither the first nor last non-blank character of
 *    the replacement text should be a connector (| or ,).
 *
 * Returns the tree of xmlElementContentPtr describing the element
 *          hierarchy.
 */
pub(crate) unsafe extern "C" fn xml_parse_element_children_content_decl(
    ctxt: XmlParserCtxtPtr,
    inputchk: c_int,
) -> XmlElementContentPtr {
    /* stub left for API/ABI compat */
    xml_parse_element_children_content_decl_priv(ctxt, inputchk, 1)
}

/**
 * xmlParseElementContentDecl:
 * @ctxt:  an XML parser context
 * @name:  the name of the element being defined.
 * @result:  the Element Content pointer will be stored here if any
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse the declaration for an Element content either Mixed or Children,
 * the cases EMPTY and ANY are handled directly in xmlParseElementDecl
 *
 * [46] contentspec ::= 'EMPTY' | 'ANY' | Mixed | children
 *
 * returns: the type of element content XML_ELEMENT_TYPE_xxx
 */
pub(crate) unsafe extern "C" fn xml_parse_element_content_decl(
    ctxt: XmlParserCtxtPtr,
    name: *const XmlChar,
    result: *mut XmlElementContentPtr,
) -> c_int {
    let tree: XmlElementContentPtr;
    let inputid: c_int = (*(*ctxt).input).id;
    let res: c_int;

    *result = null_mut();

    if RAW!(ctxt) != b'(' {
        xml_fatal_err_msg_str(
            ctxt,
            XmlParserErrors::XmlErrElemcontentNotStarted,
            c"xmlParseElementContentDecl : %s '(' expected\n".as_ptr() as _,
            name,
        );
        return -1;
    }
    NEXT!(ctxt);
    GROW!(ctxt);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }
    SKIP_BLANKS!(ctxt);
    if CMP7!(CUR_PTR!(ctxt), b'#', b'P', b'C', b'D', b'A', b'T', b'A') {
        tree = xml_parse_element_mixed_content_decl(ctxt, inputid);
        res = XmlElementTypeVal::XmlElementTypeMixed as i32;
    } else {
        tree = xml_parse_element_children_content_decl_priv(ctxt, inputid, 1);
        res = XmlElementTypeVal::XmlElementTypeElement as i32;
    }
    SKIP_BLANKS!(ctxt);
    *result = tree;
    res
}

/**
 * xmlParseEntityRef:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse an entitiy reference. Always consumes '&'.
 *
 * [68] EntityRef ::= '&' Name ';'
 *
 * [ WFC: Entity Declared ]
 * In a document without any DTD, a document with only an internal DTD
 * subset which contains no parameter entity references, or a document
 * with "standalone='yes'", the Name given in the entity reference
 * must match that in an entity declaration, except that well-formed
 * documents need not declare any of the following entities: amp, lt,
 * gt, apos, quot.  The declaration of a parameter entity must precede
 * any reference to it.  Similarly, the declaration of a general entity
 * must precede any reference to it which appears in a default value in an
 * attribute-list declaration. Note that if entities are declared in the
 * external subset or in external parameter entities, a non-validating
 * processor is not obligated to read and process their declarations;
 * for such documents, the rule that an entity must be declared is a
 * well-formedness constraint only if standalone='yes'.
 *
 * [ WFC: Parsed Entity ]
 * An entity reference must not contain the name of an unparsed entity
 *
 * Returns the xmlEntityPtr if found, or NULL otherwise.
 */
pub(crate) unsafe extern "C" fn xml_parse_entity_ref(ctxt: XmlParserCtxtPtr) -> XmlEntityPtr {
    let mut ent: XmlEntityPtr = null_mut();

    GROW!(ctxt);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }

    if RAW!(ctxt) != b'&' {
        return null_mut();
    }
    NEXT!(ctxt);
    let name: *const XmlChar = xml_parse_name(ctxt);
    if name.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            c"xmlParseEntityRef: no name\n".as_ptr() as _,
        );
        return null_mut();
    }
    if RAW!(ctxt) != b';' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityRefSemicolMissing, null());
        return null_mut();
    }
    NEXT!(ctxt);

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
            ent = f((*ctxt).user_data, name);
        }
        if (*ctxt).well_formed == 1
            && ent.is_null()
            && (*ctxt).options & XmlParserOption::XmlParseOldsax as i32 != 0
        {
            ent = xml_get_predefined_entity(name);
        }
        if (*ctxt).well_formed == 1 && ent.is_null() && (*ctxt).user_data == ctxt as _ {
            ent = xml_sax2_get_entity(ctxt as _, name);
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
                    refe((*ctxt).user_data, name);
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

/**
 * xmlParseBalancedChunkMemoryInternal:
 * @oldctxt:  the existing parsing context
 * @string:  the input string in UTF8 or ISO-Latin (zero terminated)
 * @user_data:  the user data field for the parser context
 * @lst:  the return value for the set of parsed nodes
 *
 *
 * Parse a well-balanced chunk of an XML document
 * called by the parser
 * The allowed sequence for the Well Balanced Chunk is the one defined by
 * the content production in the XML grammar:
 *
 * [43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*
 *
 * Returns xmlParserErrors::XML_ERR_OK if the chunk is well balanced, and the parser
 * error code otherwise
 *
 * In case recover is set to 1, the nodelist will not be empty even if
 * the parsed chunk is not well balanced.
 */
unsafe extern "C" fn xml_parse_balanced_chunk_memory_internal(
    oldctxt: XmlParserCtxtPtr,
    string: *const XmlChar,
    user_data: *mut c_void,
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

    let size: c_int = xml_strlen(string);

    let ctxt: XmlParserCtxtPtr = xml_create_memory_parser_ctxt(string as _, size);
    if ctxt.is_null() {
        return XmlParserErrors::XmlWarUndeclaredEntity;
    }
    (*ctxt).nb_errors = (*oldctxt).nb_errors;
    (*ctxt).nb_warnings = (*oldctxt).nb_warnings;
    if !user_data.is_null() {
        (*ctxt).user_data = user_data;
    } else {
        (*ctxt).user_data = ctxt as _;
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
    for i in (0..(*oldctxt).ns_nr).step_by(2) {
        ns_push(
            ctxt,
            *(*oldctxt).ns_tab.add(i as usize),
            *(*oldctxt).ns_tab.add(i as usize + 1),
        );
    }

    let oldsax: XmlSAXHandlerPtr = (*ctxt).sax;
    (*ctxt).sax = (*oldctxt).sax;
    xml_detect_sax2(ctxt);
    (*ctxt).replace_entities = (*oldctxt).replace_entities;
    (*ctxt).options = (*oldctxt).options;

    (*ctxt)._private = (*oldctxt)._private;
    if (*oldctxt).my_doc.is_null() {
        new_doc = xml_new_doc(c"1.0".as_ptr() as _);
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
        content = (*(*ctxt).my_doc).children;
        last = (*(*ctxt).my_doc).last;
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
    (*(*ctxt).my_doc).children = null_mut();
    (*(*ctxt).my_doc).last = null_mut();
    xml_add_child((*ctxt).my_doc as _, new_root);
    node_push(ctxt, (*(*ctxt).my_doc).children);
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
    if RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, null());
    } else if RAW!(ctxt) != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtraContent, null());
    }
    if (*ctxt).node != (*(*ctxt).my_doc).children {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, null());
    }

    if (*ctxt).well_formed == 0 {
        ret = XmlParserErrors::try_from((*ctxt).err_no).unwrap();
        (*oldctxt).err_no = (*ctxt).err_no;
        (*oldctxt).well_formed = 0;
        xml_copy_error(
            addr_of_mut!((*ctxt).last_error),
            addr_of_mut!((*oldctxt).last_error),
        );
    } else {
        ret = XmlParserErrors::XmlErrOK;
    }

    if !lst.is_null() && matches!(ret, XmlParserErrors::XmlErrOK) {
        let mut cur: XmlNodePtr;

        /*
         * Return the newly created nodeset after unlinking it from
         * they pseudo parent.
         */
        cur = (*(*(*ctxt).my_doc).children).children;
        *lst = cur;
        while !cur.is_null() {
            #[cfg(feature = "valid")]
            if (*oldctxt).validate != 0
                && (*oldctxt).well_formed != 0
                && !(*oldctxt).my_doc.is_null()
                && !(*(*oldctxt).my_doc).int_subset.is_null()
                && (*cur).typ == XmlElementType::XmlElementNode
            {
                (*oldctxt).valid &=
                    xml_validate_element(addr_of_mut!((*oldctxt).vctxt), (*oldctxt).my_doc, cur);
            }
            (*cur).parent = null_mut();
            cur = (*cur).next;
        }
        (*(*(*ctxt).my_doc).children).children = null_mut();
    }
    if !(*ctxt).my_doc.is_null() {
        xml_free_node((*(*ctxt).my_doc).children);
        (*(*ctxt).my_doc).children = content;
        (*(*ctxt).my_doc).last = last;
    }

    /*
     * Also record the size of the entity parsed
     */
    if !(*ctxt).input.is_null() && !oldctxt.is_null() {
        let mut consumed: c_ulong = (*(*ctxt).input).consumed;

        xml_saturated_add_size_t(
            addr_of_mut!(consumed),
            (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _,
        );

        xml_saturated_add(addr_of_mut!((*oldctxt).sizeentcopy), consumed);
        xml_saturated_add(addr_of_mut!((*oldctxt).sizeentcopy), (*ctxt).sizeentcopy);
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
#[cfg(feature = "legacy")]
unsafe extern "C" fn xml_add_entity_reference(
    ent: XmlEntityPtr,
    first_node: XmlNodePtr,
    last_node: XmlNodePtr,
) {
    if let Some(func) = XML_ENTITY_REF_FUNC {
        func(ent, first_node, last_node);
    }
}

/**
 * xmlParseReference:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse and handle entity references in content, depending on the SAX
 * interface, this may end-up in a call to character() if this is a
 * CharRef, a predefined entity, if there is no reference() callback.
 * or if the parser was asked to match to that mode.
 *
 * Always consumes '&'.
 *
 * [67] Reference ::= EntityRef | CharRef
 */
pub(crate) unsafe extern "C" fn xml_parse_reference(ctxt: XmlParserCtxtPtr) {
    let val: *mut XmlChar;
    let mut was_checked: c_int;
    let mut list: XmlNodePtr = null_mut();
    let mut ret: XmlParserErrors;

    if RAW!(ctxt) != b'&' {
        return;
    }

    /*
     * Simple case of a CharRef
     */
    if NXT!(ctxt, 1) == b'#' {
        let mut i: c_int = 0;
        let mut out: [XmlChar; 16] = [0; 16];
        let hex: c_int = NXT!(ctxt, 2) as _;
        let value: c_int = xml_parse_char_ref(ctxt);

        if value == 0 {
            return;
        }
        if (*ctxt).charset != XmlCharEncoding::XmlCharEncodingUtf8 as i32 {
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
                        characters((*ctxt).user_data, out.as_ptr() as _, 1);
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
                        refe((*ctxt).user_data, out.as_ptr() as _);
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
                    characters((*ctxt).user_data, out.as_ptr() as _, i);
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
                characters((*ctxt).user_data, val, xml_strlen(val));
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
        let oldsizeentcopy: c_ulong = (*ctxt).sizeentcopy;

        /*
         * This is a bit hackish but this seems the best
         * way to make sure both SAX and DOM entity support
         * behaves okay.
         */
        let user_data: *mut c_void = if (*ctxt).user_data == ctxt as _ {
            null_mut()
        } else {
            (*ctxt).user_data
        };

        /* Avoid overflow as much as possible */
        (*ctxt).sizeentcopy = 0;

        if (*ent).flags & XML_ENT_EXPANDING as i32 != 0 {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, null());
            xml_halt_parser(ctxt);
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
            ret = XmlParserErrors::XmlErrEntityPeInternal;
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
            xml_halt_parser(ctxt);
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
                || (matches!((*list).typ, XmlElementType::XmlTextNode) && (*list).next.is_null())
            {
                (*ent).owner = 1;
                while !list.is_null() {
                    (*list).parent = ent as XmlNodePtr;
                    if (*list).doc != (*ent).doc.load(Ordering::Relaxed) as _ {
                        xml_set_tree_doc(list, (*ent).doc.load(Ordering::Relaxed));
                    }
                    if (*list).next.is_null() {
                        (*ent).last.store(list, Ordering::Relaxed);
                    }
                    list = (*list).next;
                }
                list = null_mut();
            } else {
                (*ent).owner = 0;
                while !list.is_null() {
                    (*list).parent = (*ctxt).node as XmlNodePtr;
                    (*list).doc = (*ctxt).my_doc;
                    if (*list).next.is_null() {
                        (*ent).last.store(list, Ordering::Relaxed);
                    }
                    list = (*list).next;
                }
                list = (*ent).children.load(Ordering::Relaxed) as _;
                #[cfg(feature = "legacy")]
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
            let user_data = if (*ctxt).user_data == ctxt as _ {
                null_mut()
            } else {
                (*ctxt).user_data
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
                let oldsizeentities: c_ulong = (*ctxt).sizeentities;

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
                ret = XmlParserErrors::XmlErrEntityPeInternal;
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
                refe((*ctxt).user_data, (*ent).name.load(Ordering::Relaxed) as _);
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
            refe((*ctxt).user_data, (*ent).name.load(Ordering::Relaxed) as _);
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
                        nw = xml_add_child((*ctxt).node, nw);
                    }
                    if cur == (*ent).last.load(Ordering::Relaxed) as _ {
                        /*
                         * needed to detect some strange empty
                         * node cases in the reader tests
                         */
                        if matches!((*ctxt).parse_mode, XmlParserMode::XmlParseReader)
                            && !nw.is_null()
                            && matches!((*nw).typ, XmlElementType::XmlElementNode)
                            && (*nw).children.is_null()
                        {
                            (*nw).extra = 1;
                        }

                        break;
                    }
                    cur = (*cur).next;
                }
                #[cfg(feature = "legacy")]
                if matches!(
                    (*ent).etype,
                    Some(XmlEntityType::XmlExternalGeneralParsedEntity)
                ) {
                    xml_add_entity_reference(ent, first_child, nw);
                }
            } else if list.is_null() || (*ctxt).input_nr > 0 {
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
                    next = (*cur).next;
                    (*cur).next = null_mut();
                    (*cur).parent = null_mut();
                    nw = xml_doc_copy_node(cur, (*ctxt).my_doc, 1);
                    if !nw.is_null() {
                        if (*nw)._private.is_null() {
                            (*nw)._private = (*cur)._private;
                        }
                        if first_child.is_null() {
                            first_child = cur;
                        }
                        xml_add_child(ent as XmlNodePtr, nw);
                    }
                    xml_add_child((*ctxt).node, cur);
                    if cur == last {
                        break;
                    }
                    cur = next;
                }
                if (*ent).owner == 0 {
                    (*ent).owner = 1;
                }
                #[cfg(feature = "legacy")]
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
                xml_add_child_list((*ctxt).node, (*ent).children.load(Ordering::Relaxed));
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

/**
 * xmlParsePEReference:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse a parameter entity reference. Always consumes '%'.
 *
 * The entity content is handled directly by pushing it's content as
 * a new input stream.
 *
 * [69] PEReference ::= '%' Name ';'
 *
 * [ WFC: No Recursion ]
 * A parsed entity must not contain a recursive
 * reference to itself, either directly or indirectly.
 *
 * [ WFC: Entity Declared ]
 * In a document without any DTD, a document with only an internal DTD
 * subset which contains no parameter entity references, or a document
 * with "standalone='yes'", ...  ... The declaration of a parameter
 * entity must precede any reference to it...
 *
 * [ VC: Entity Declared ]
 * In a document with an external subset or external parameter entities
 * with "standalone='no'", ...  ... The declaration of a parameter entity
 * must precede any reference to it...
 *
 * [ WFC: In DTD ]
 * Parameter-entity references may only appear in the DTD.
 * NOTE: misleading but this is handled.
 */
pub(crate) unsafe extern "C" fn xml_parse_pe_reference(ctxt: XmlParserCtxtPtr) {
    let mut entity: XmlEntityPtr = null_mut();
    let input: XmlParserInputPtr;

    if RAW!(ctxt) != b'%' {
        return;
    }
    NEXT!(ctxt);
    let name: *const XmlChar = xml_parse_name(ctxt);
    if name.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrPERefNoName,
            c"PEReference: no name\n".as_ptr() as _,
        );
        return;
    }
    if *xml_parser_debug_entities() != 0 {
        xml_generic_error!(
            xml_generic_error_context(),
            c"PEReference: %s\n".as_ptr() as _,
            name
        );
    }
    if RAW!(ctxt) != b';' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrPERefSemicolMissing, null());
        return;
    }

    NEXT!(ctxt);

    /*
     * Request the entity from SAX
     */
    if !(*ctxt).sax.is_null() {
        if let Some(param) = (*(*ctxt).sax).get_parameter_entity {
            entity = param((*ctxt).user_data, name);
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
            let enc: XmlCharEncoding;
            let mut parent_consumed: c_ulong;

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
                xml_halt_parser(ctxt);
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
                xml_saturated_add(addr_of_mut!(parent_consumed), (*(*ctxt).input).consumed);
                xml_saturated_add_size_t(
                    addr_of_mut!(parent_consumed),
                    (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _,
                );
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
                GROW!(ctxt);
                if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    return;
                }
                if (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4 {
                    start[0] = RAW!(ctxt);
                    start[1] = NXT!(ctxt, 1);
                    start[2] = NXT!(ctxt, 2);
                    start[3] = NXT!(ctxt, 3);
                    enc = xml_detect_char_encoding(start.as_ptr() as _, 4);
                    if !matches!(enc, XmlCharEncoding::XmlCharEncodingNone) {
                        xml_switch_encoding(ctxt, enc);
                    }
                }

                if CMP5!(CUR_PTR!(ctxt), b'<', b'?', b'x', b'm', b'l')
                    && IS_BLANK_CH!(NXT!(ctxt, 5))
                {
                    xml_parse_text_decl(ctxt);
                }
            }
        }
    }
    (*ctxt).has_perefs = 1;
}

/**
 * xmlParseDocTypeDecl:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse a DOCTYPE declaration
 *
 * [28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S?
 *                      ('[' (markupdecl | PEReference | S)* ']' S?)? '>'
 *
 * [ VC: Root Element Type ]
 * The Name in the document type declaration must match the element
 * type of the root element.
 */
pub(crate) unsafe extern "C" fn xml_parse_doc_type_decl(ctxt: XmlParserCtxtPtr) {
    let mut external_id: *mut XmlChar = null_mut();

    /*
     * We know that '<!DOCTYPE' has been detected.
     */
    SKIP!(ctxt, 9);

    SKIP_BLANKS!(ctxt);

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

    SKIP_BLANKS!(ctxt);

    /*
     * Check for SystemID and ExternalID
     */
    let uri: *mut XmlChar = xml_parse_external_id(ctxt, addr_of_mut!(external_id), 1);

    if !uri.is_null() || !external_id.is_null() {
        (*ctxt).has_external_subset = 1;
    }
    (*ctxt).ext_sub_uri = uri;
    (*ctxt).ext_sub_system = external_id;

    SKIP_BLANKS!(ctxt);

    /*
     * Create and update the internal subset.
     */
    if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
        if let Some(internal_subset) = (*(*ctxt).sax).internal_subset {
            internal_subset((*ctxt).user_data, name, external_id, uri);
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }

    /*
     * Is there any internal subset declarations ?
     * they are handled separately in xmlParseInternalSubset()
     */
    if RAW!(ctxt) == b'[' {
        return;
    }

    /*
     * We should be at the end of the DOCTYPE declaration.
     */
    if RAW!(ctxt) != b'>' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDoctypeNotFinished, null());
    }
    NEXT!(ctxt);
}

/**
 * xmlParseAttribute:
 * @ctxt:  an XML parser context
 * @value:  a xmlChar ** used to store the value of the attribute
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an attribute
 *
 * [41] Attribute ::= Name Eq AttValue
 *
 * [ WFC: No External Entity References ]
 * Attribute values cannot contain direct or indirect entity references
 * to external entities.
 *
 * [ WFC: No < in Attribute Values ]
 * The replacement text of any entity referred to directly or indirectly in
 * an attribute value (other than "&lt;") must not contain a <.
 *
 * [ VC: Attribute Value Type ]
 * The attribute must have been declared; the value must be of the type
 * declared for it.
 *
 * [25] Eq ::= S? '=' S?
 *
 * With namespace:
 *
 * [NS 11] Attribute ::= QName Eq AttValue
 *
 * Also the case QName == xmlns:??? is handled independently as a namespace
 * definition.
 *
 * Returns the attribute name, and the value in *value.
 */
#[cfg(feature = "sax1")]
pub(crate) unsafe extern "C" fn xml_parse_attribute(
    ctxt: XmlParserCtxtPtr,
    value: *mut *mut XmlChar,
) -> *const XmlChar {
    let val: *mut XmlChar;

    *value = null_mut();
    GROW!(ctxt);
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
    SKIP_BLANKS!(ctxt);
    if RAW!(ctxt) == b'=' {
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
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
            *(*ctxt).space = 0;
        } else if xml_str_equal(val, c"preserve".as_ptr() as _) {
            *(*ctxt).space = 1;
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

macro_rules! NEXT1 {
    ($ctxt:expr) => {
        (*(*$ctxt).input).col += 1;
        (*(*$ctxt).input).cur = (*(*$ctxt).input).cur.add(1);
        if *(*(*$ctxt).input).cur == 0 {
            xml_parser_grow($ctxt);
        }
    };
}

/**
 * xmlParseStartTag:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse a start tag. Always consumes '<'.
 *
 * [40] STag ::= '<' Name (S Attribute)* S? '>'
 *
 * [ WFC: Unique Att Spec ]
 * No attribute name may appear more than once in the same start-tag or
 * empty-element tag.
 *
 * [44] EmptyElemTag ::= '<' Name (S Attribute)* S? '/>'
 *
 * [ WFC: Unique Att Spec ]
 * No attribute name may appear more than once in the same start-tag or
 * empty-element tag.
 *
 * With namespace:
 *
 * [NS 8] STag ::= '<' QName (S Attribute)* S? '>'
 *
 * [NS 10] EmptyElement ::= '<' QName (S Attribute)* S? '/>'
 *
 * Returns the element name parsed
 */
#[cfg(feature = "sax1")]
pub(crate) unsafe extern "C" fn xml_parse_start_tag(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    use crate::libxml::parser::xml_err_attribute_dup;

    let mut attname: *const XmlChar;
    let mut attvalue: *mut XmlChar = null_mut();
    let mut atts: *mut *const XmlChar = (*ctxt).atts;
    let mut nbatts: c_int = 0;
    let mut maxatts: c_int = (*ctxt).maxatts;

    if RAW!(ctxt) != b'<' {
        return null_mut();
    }
    NEXT1!(ctxt);

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
    SKIP_BLANKS!(ctxt);
    GROW!(ctxt);

    while RAW!(ctxt) != b'>'
        && (RAW!(ctxt) != b'/' || NXT!(ctxt, 1) != b'>')
        && IS_BYTE_CHAR!(RAW!(ctxt))
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

        GROW!(ctxt);
        if RAW!(ctxt) == b'>' || (RAW!(ctxt) == b'/' && NXT!(ctxt, 1) == b'>') {
            break;
        }
        if SKIP_BLANKS!(ctxt) == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                c"attributes construct error\n".as_ptr() as _,
            );
        }
        SHRINK!(ctxt);
        GROW!(ctxt);
    }

    /*
     * SAX: Start of Element !
     */
    if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
        if let Some(elem) = (*(*ctxt).sax).start_element {
            if nbatts > 0 {
                elem((*ctxt).user_data, name, atts);
            } else {
                elem((*ctxt).user_data, name, null_mut());
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

/**
 * xmlParseEndTag:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an end of tag
 *
 * [42] ETag ::= '</' Name S? '>'
 *
 * With namespace
 *
 * [NS 9] ETag ::= '</' QName S? '>'
 */
#[cfg(feature = "sax1")]
pub(crate) unsafe extern "C" fn xml_parse_end_tag(ctxt: XmlParserCtxtPtr) {
    xml_parse_end_tag1(ctxt, 0);
}

/**
 * xmlParseElementStart:
 * @ctxt:  an XML parser context
 *
 * Parse the start of an XML element. Returns -1 in case of error, 0 if an
 * opening tag was parsed, 1 if an empty element was parsed.
 *
 * Always consumes '<'.
 */
pub(crate) unsafe extern "C" fn xml_parse_element_start(ctxt: XmlParserCtxtPtr) -> c_int {
    let name: *const XmlChar;
    let mut prefix: *const XmlChar = null_mut();
    let mut uri: *const XmlChar = null_mut();
    let mut node_info: XmlParserNodeInfo = unsafe { zeroed() };
    let mut tlen: c_int = 0;
    let ns_nr: c_int = (*ctxt).ns_nr;

    if (*ctxt).name_nr as c_uint > XML_PARSER_MAX_DEPTH
        && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0
    {
        xml_fatal_err_msg_int(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            c"Excessive depth in document: %d use xmlParserOption::XML_PARSE_HUGE option\n".as_ptr()
                as _,
            XML_PARSER_MAX_DEPTH as _,
        );
        xml_halt_parser(ctxt);
        return -1;
    }

    /* Capture start position */
    if (*ctxt).record_info != 0 {
        node_info.begin_pos =
            (*(*ctxt).input).consumed + CUR_PTR!(ctxt).offset_from((*(*ctxt).input).base) as u64;
        node_info.begin_line = (*(*ctxt).input).line as _;
    }

    if (*ctxt).space_nr == 0 || *(*ctxt).space == -2 {
        space_push(ctxt, -1);
    } else {
        space_push(ctxt, *(*ctxt).space);
    }

    let line: c_int = (*(*ctxt).input).line;
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
        space_pop(ctxt);
        return -1;
    }
    name_ns_push(ctxt, name, prefix, uri, line, (*ctxt).ns_nr - ns_nr);
    let cur: XmlNodePtr = (*ctxt).node;

    /*
     * [ VC: Root Element Type ]
     * The Name in the document type declaration must match the element
     * type of the root element.
     */
    #[cfg(feature = "valid")]
    if (*ctxt).validate != 0
        && (*ctxt).well_formed != 0
        && !(*ctxt).my_doc.is_null()
        && !(*ctxt).node.is_null()
        && (*ctxt).node == (*(*ctxt).my_doc).children
    {
        (*ctxt).valid &= xml_validate_root(addr_of_mut!((*ctxt).vctxt), (*ctxt).my_doc);
    }

    /*
     * Check for an Empty Element.
     */
    if RAW!(ctxt) == b'/' && NXT!(ctxt, 1) == b'>' {
        SKIP!(ctxt, 2);
        if (*ctxt).sax2 != 0 {
            if !(*ctxt).sax.is_null()
                && (*(*ctxt).sax).end_element_ns.is_some()
                && (*ctxt).disable_sax == 0
            {
                ((*(*ctxt).sax).end_element_ns.unwrap())((*ctxt).user_data, name, prefix, uri);
            }
        } else {
            #[cfg(feature = "sax1")]
            if !(*ctxt).sax.is_null()
                && (*(*ctxt).sax).end_element.is_some()
                && (*ctxt).disable_sax == 0
            {
                ((*(*ctxt).sax).end_element.unwrap())((*ctxt).user_data, name);
            }
        }
        name_pop(ctxt);
        space_pop(ctxt);
        if ns_nr != (*ctxt).ns_nr {
            ns_pop(ctxt, (*ctxt).ns_nr - ns_nr);
        }
        if !cur.is_null() && (*ctxt).record_info != 0 {
            node_info.node = cur;
            node_info.end_pos = (*(*ctxt).input).consumed
                + CUR_PTR!(ctxt).offset_from((*(*ctxt).input).base) as u64;
            node_info.end_line = (*(*ctxt).input).line as _;
            xml_parser_add_node_info(ctxt, addr_of_mut!(node_info));
        }
        return 1;
    }
    if RAW!(ctxt) == b'>' {
        NEXT1!(ctxt);
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
        node_pop(ctxt);
        name_pop(ctxt);
        space_pop(ctxt);
        if ns_nr != (*ctxt).ns_nr {
            ns_pop(ctxt, (*ctxt).ns_nr - ns_nr);
        }
        return -1;
    }

    0
}

/**
 * xmlParseElementEnd:
 * @ctxt:  an XML parser context
 *
 * Parse the end of an XML element. Always consumes '</'.
 */
pub(crate) unsafe extern "C" fn xml_parse_element_end(ctxt: XmlParserCtxtPtr) {
    let cur: XmlNodePtr = (*ctxt).node;

    if (*ctxt).name_nr <= 0 {
        if RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'/' {
            SKIP!(ctxt, 2);
        }
        return;
    }

    /*
     * parse the end of tag: '</' should be here.
     */
    if (*ctxt).sax2 != 0 {
        xml_parse_end_tag2(ctxt, (*ctxt).push_tab.add((*ctxt).name_nr as usize - 1));
        name_pop(ctxt);
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
                + CUR_PTR!(ctxt).offset_from((*(*ctxt).input).base) as u64;
            (*node_info).end_line = (*(*ctxt).input).line as _;
        }
    }
}

/**
 * xmlParseContentInternal:
 * @ctxt:  an XML parser context
 *
 * Parse a content sequence. Stops at EOF or '</'. Leaves checking of
 * unexpected EOF to the caller.
 */
pub(crate) unsafe extern "C" fn xml_parse_content_internal(ctxt: XmlParserCtxtPtr) {
    let name_nr: c_int = (*ctxt).name_nr;

    GROW!(ctxt);
    while RAW!(ctxt) != 0 && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
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
        else if CMP9!(
            CUR_PTR!(ctxt),
            b'<',
            b'!',
            b'[',
            b'C',
            b'D',
            b'A',
            b'T',
            b'A',
            b'['
        ) {
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
                if (*ctxt).name_nr <= name_nr {
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

        SHRINK!(ctxt);
        GROW!(ctxt);
    }
}

/**
 * xmlParseContent:
 * @ctxt:  an XML parser context
 *
 * Parse a content sequence. Stops at EOF or '</'.
 *
 * [43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*
 */
pub unsafe extern "C" fn xml_parse_content(ctxt: XmlParserCtxtPtr) {
    let name_nr: c_int = (*ctxt).name_nr;

    xml_parse_content_internal(ctxt);

    if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) && (*ctxt).name_nr > name_nr {
        let name: *const XmlChar = *(*ctxt).name_tab.add((*ctxt).name_nr as usize - 1);
        let line: c_int = (*(*ctxt).push_tab.add((*ctxt).name_nr as usize - 1)).line;
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

/**
 * xmlParseVersionInfo:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse the XML version.
 *
 * [24] VersionInfo ::= S 'version' Eq (' VersionNum ' | " VersionNum ")
 *
 * [25] Eq ::= S? '=' S?
 *
 * Returns the version string, e.g. "1.0"
 */
pub(crate) unsafe extern "C" fn xml_parse_version_info(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    let mut version: *mut XmlChar = null_mut();

    if CMP7!(CUR_PTR!(ctxt), b'v', b'e', b'r', b's', b'i', b'o', b'n') {
        SKIP!(ctxt, 7);
        SKIP_BLANKS!(ctxt);
        if RAW!(ctxt) != b'=' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEqualRequired, null());
            return null_mut();
        }
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
        if RAW!(ctxt) == b'"' {
            NEXT!(ctxt);
            version = xml_parse_version_num(ctxt);
            if RAW!(ctxt) != b'"' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, null());
            } else {
                NEXT!(ctxt);
            }
        } else if RAW!(ctxt) == b'\'' {
            NEXT!(ctxt);
            version = xml_parse_version_num(ctxt);
            if RAW!(ctxt) != b'\'' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, null());
            } else {
                NEXT!(ctxt);
            }
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotStarted, null());
        }
    }
    version
}

/**
 * xmlParseEncodingDecl:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse the XML encoding declaration
 *
 * [80] EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' |  "'" EncName "'")
 *
 * this setups the conversion filters.
 *
 * Returns the encoding value or NULL
 */
pub(crate) unsafe extern "C" fn xml_parse_encoding_decl(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    let mut encoding: *mut XmlChar = null_mut();

    SKIP_BLANKS!(ctxt);
    if CMP8!(
        CUR_PTR!(ctxt),
        b'e',
        b'n',
        b'c',
        b'o',
        b'd',
        b'i',
        b'n',
        b'g'
    ) {
        SKIP!(ctxt, 8);
        SKIP_BLANKS!(ctxt);
        if RAW!(ctxt) != b'=' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEqualRequired, null());
            return null_mut();
        }
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
        if RAW!(ctxt) == b'"' {
            NEXT!(ctxt);
            encoding = xml_parse_enc_name(ctxt);
            if RAW!(ctxt) != b'"' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, null());
                xml_free(encoding as _);
                return null_mut();
            } else {
                NEXT!(ctxt);
            }
        } else if RAW!(ctxt) == b'\'' {
            NEXT!(ctxt);
            encoding = xml_parse_enc_name(ctxt);
            if RAW!(ctxt) != b'\'' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, null());
                xml_free(encoding as _);
                return null_mut();
            } else {
                NEXT!(ctxt);
            }
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotStarted, null());
        }

        /*
         * Non standard parsing, allowing the user to ignore encoding
         */
        if (*ctxt).options & XmlParserOption::XmlParseIgnoreEnc as i32 != 0 {
            xml_free(encoding as _);
            return null_mut();
        }

        /*
         * UTF-16 encoding match has already taken place at this stage,
         * more over the little-endian/big-endian selection is already done
         */
        if !encoding.is_null()
            && (xml_strcasecmp(encoding, c"UTF-16".as_ptr() as _) == 0
                || xml_strcasecmp(encoding, c"UTF16".as_ptr() as _) == 0)
        {
            /*
             * If no encoding was passed to the parser, that we are
             * using UTF-16 and no decoder is present i.e. the
             * document is apparently UTF-8 compatible, then raise an
             * encoding mismatch fatal error
             */
            if (*ctxt).encoding.is_null()
                && !(*(*ctxt).input).buf.is_null()
                && (*(*(*ctxt).input).buf).encoder.is_null()
            {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidEncoding,
                    c"Document labelled UTF-16 but has UTF-8 content\n".as_ptr() as _,
                );
            }
            if !(*ctxt).encoding.is_null() {
                xml_free((*ctxt).encoding as _);
            }
            (*ctxt).encoding = encoding;
        }
        /*
         * UTF-8 encoding is handled natively
         */
        else if !encoding.is_null()
            && (xml_strcasecmp(encoding, c"UTF-8".as_ptr() as _) == 0
                || xml_strcasecmp(encoding, c"UTF8".as_ptr() as _) == 0)
        {
            /* TODO: Check for encoding mismatch. */
            if !(*ctxt).encoding.is_null() {
                xml_free((*ctxt).encoding as _);
            }
            (*ctxt).encoding = encoding;
        } else if !encoding.is_null() {
            if !(*(*ctxt).input).encoding.is_null() {
                xml_free((*(*ctxt).input).encoding as _);
            }
            (*(*ctxt).input).encoding = encoding;

            let handler: XmlCharEncodingHandlerPtr =
                xml_find_char_encoding_handler(encoding as *const c_char);
            if !handler.is_null() {
                if xml_switch_to_encoding(ctxt, handler) < 0 {
                    /* failed to convert */
                    (*ctxt).err_no = XmlParserErrors::XmlErrUnsupportedEncoding as i32;
                    return null_mut();
                }
            } else {
                xml_fatal_err_msg_str(
                    ctxt,
                    XmlParserErrors::XmlErrUnsupportedEncoding,
                    c"Unsupported encoding %s\n".as_ptr() as _,
                    encoding,
                );
                return null_mut();
            }
        }
    }
    encoding
}

/**
 * xmlParseSDDecl:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse the XML standalone declaration
 *
 * [32] SDDecl ::= S 'standalone' Eq
 *                 (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no')'"'))
 *
 * [ VC: Standalone Document Declaration ]
 * TODO The standalone document declaration must have the value "no"
 * if any external markup declarations contain declarations of:
 *  - attributes with default values, if elements to which these
 *    attributes apply appear in the document without specifications
 *    of values for these attributes, or
 *  - entities (other than amp, lt, gt, apos, quot), if references
 *    to those entities appear in the document, or
 *  - attributes with values subject to normalization, where the
 *    attribute appears in the document with a value which will change
 *    as a result of normalization, or
 *  - element types with element content, if white space occurs directly
 *    within any instance of those types.
 *
 * Returns:
 *   1 if standalone="yes"
 *   0 if standalone="no"
 *  -2 if standalone attribute is missing or invalid
 *      (A standalone value of -2 means that the XML declaration was found,
 *       but no value was specified for the standalone attribute).
 */
pub(crate) unsafe extern "C" fn xml_parse_sddecl(ctxt: XmlParserCtxtPtr) -> c_int {
    let mut standalone: c_int = -2;

    SKIP_BLANKS!(ctxt);
    if CMP10!(
        CUR_PTR!(ctxt),
        b's',
        b't',
        b'a',
        b'n',
        b'd',
        b'a',
        b'l',
        b'o',
        b'n',
        b'e'
    ) {
        SKIP!(ctxt, 10);
        SKIP_BLANKS!(ctxt);
        if RAW!(ctxt) != b'=' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEqualRequired, null());
            return standalone;
        }
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
        if RAW!(ctxt) == b'\'' {
            NEXT!(ctxt);
            if RAW!(ctxt) == b'n' && NXT!(ctxt, 1) == b'o' {
                standalone = 0;
                SKIP!(ctxt, 2);
            } else if RAW!(ctxt) == b'y' && NXT!(ctxt, 1) == b'e' && NXT!(ctxt, 2) == b's' {
                standalone = 1;
                SKIP!(ctxt, 3);
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStandaloneValue, null());
            }
            if RAW!(ctxt) != b'\'' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, null());
            } else {
                NEXT!(ctxt);
            }
        } else if RAW!(ctxt) == b'"' {
            NEXT!(ctxt);
            if RAW!(ctxt) == b'n' && NXT!(ctxt, 1) == b'o' {
                standalone = 0;
                SKIP!(ctxt, 2);
            } else if RAW!(ctxt) == b'y' && NXT!(ctxt, 1) == b'e' && NXT!(ctxt, 2) == b's' {
                standalone = 1;
                SKIP!(ctxt, 3);
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStandaloneValue, null());
            }
            if RAW!(ctxt) != b'"' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotClosed, null());
            } else {
                NEXT!(ctxt);
            }
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrStringNotStarted, null());
        }
    }
    standalone
}

/**
 * xmlParseMisc:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an XML Misc* optional field.
 *
 * [27] Misc ::= Comment | PI |  S
 */
pub(crate) unsafe extern "C" fn xml_parse_misc(ctxt: XmlParserCtxtPtr) {
    #[allow(clippy::while_immutable_condition)]
    while !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        SKIP_BLANKS!(ctxt);
        GROW!(ctxt);
        if RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'?' {
            xml_parse_pi(ctxt);
        } else if CMP4!(CUR_PTR!(ctxt), b'<', b'!', b'-', b'-') {
            xml_parse_comment(ctxt);
        } else {
            break;
        }
    }
}

/**
 * xmlParseExternalSubset:
 * @ctxt:  an XML parser context
 * @ExternalID: the external identifier
 * @SystemID: the system identifier (or URL)
 *
 * parse Markup declarations from an external subset
 *
 * [30] extSubset ::= textDecl? extSubsetDecl
 *
 * [31] extSubsetDecl ::= (markupdecl | conditionalSect | PEReference | S) *
 */
pub unsafe extern "C" fn xml_parse_external_subset(
    ctxt: XmlParserCtxtPtr,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) {
    xml_detect_sax2(ctxt);
    GROW!(ctxt);

    if (*ctxt).encoding.is_null() && (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4 {
        let mut start: [XmlChar; 4] = [0; 4];

        start[0] = RAW!(ctxt);
        start[1] = NXT!(ctxt, 1);
        start[2] = NXT!(ctxt, 2);
        start[3] = NXT!(ctxt, 3);
        let enc: XmlCharEncoding = xml_detect_char_encoding(start.as_ptr() as _, 4);
        if !matches!(enc, XmlCharEncoding::XmlCharEncodingNone) {
            xml_switch_encoding(ctxt, enc);
        }
    }

    if CMP5!(CUR_PTR!(ctxt), b'<', b'?', b'x', b'm', b'l') {
        xml_parse_text_decl(ctxt);
        if (*ctxt).err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
            /*
             * The XML REC instructs us to stop parsing right here
             */
            xml_halt_parser(ctxt);
            return;
        }
    }
    if (*ctxt).my_doc.is_null() {
        (*ctxt).my_doc = xml_new_doc(c"1.0".as_ptr() as _);
        if (*ctxt).my_doc.is_null() {
            xml_err_memory(ctxt, c"New Doc failed".as_ptr() as _);
            return;
        }
        (*(*ctxt).my_doc).properties = XmlDocProperties::XmlDocInternal as i32;
    }
    if !(*ctxt).my_doc.is_null() && (*(*ctxt).my_doc).int_subset.is_null() {
        xml_create_int_subset((*ctxt).my_doc, null(), external_id, system_id);
    }

    (*ctxt).instate = XmlParserInputState::XmlParserDTD;
    (*ctxt).external = 1;
    SKIP_BLANKS!(ctxt);
    #[allow(clippy::while_immutable_condition)]
    while !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) && RAW!(ctxt) != 0 {
        GROW!(ctxt);
        if RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'!' && NXT!(ctxt, 2) == b'[' {
            xml_parse_conditional_sections(ctxt);
        } else if RAW!(ctxt) == b'<' && (NXT!(ctxt, 1) == b'!' || NXT!(ctxt, 1) == b'?') {
            xml_parse_markup_decl(ctxt);
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtSubsetNotFinished, null());
            xml_halt_parser(ctxt);
            return;
        }
        SKIP_BLANKS!(ctxt);
        SHRINK!(ctxt);
    }

    if RAW!(ctxt) != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtSubsetNotFinished, null());
    }
}

/**
 * XML_SUBSTITUTE_NONE:
 *
 * If no entities need to be substituted.
 */
const XML_SUBSTITUTE_NONE: usize = 0;
/**
 * XML_SUBSTITUTE_REF:
 *
 * Whether general entities need to be substituted.
 */
pub(crate) const XML_SUBSTITUTE_REF: usize = 1;
/**
 * XML_SUBSTITUTE_PEREF:
 *
 * Whether parameter entities need to be substituted.
 */
pub(crate) const XML_SUBSTITUTE_PEREF: usize = 2;
/**
 * XML_SUBSTITUTE_BOTH:
 *
 * Both general and parameter entities need to be substituted.
 */
const XML_SUBSTITUTE_BOTH: usize = 3;

/**
 * xmlStringDecodeEntities:
 * @ctxt:  the parser context
 * @str:  the input string
 * @what:  combination of XML_SUBSTITUTE_REF and XML_SUBSTITUTE_PEREF
 * @end:  an end marker xmlChar, 0 if none
 * @end2:  an end marker xmlChar, 0 if none
 * @end3:  an end marker xmlChar, 0 if none
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Takes a entity string content and process to do the adequate substitutions.
 *
 * [67] Reference ::= EntityRef | CharRef
 *
 * [69] PEReference ::= '%' Name ';'
 *
 * Returns A newly allocated string with the substitution done. The caller
 *      must deallocate it !
 */
pub(crate) unsafe extern "C" fn xml_string_decode_entities(
    ctxt: XmlParserCtxtPtr,
    str: *const XmlChar,
    what: c_int,
    end: XmlChar,
    end2: XmlChar,
    end3: XmlChar,
) -> *mut XmlChar {
    if ctxt.is_null() || str.is_null() {
        return null_mut();
    }
    xml_string_decode_entities_int(ctxt, str, xml_strlen(str), what, end, end2, end3, 0)
}

/**
 * xmlStringLenDecodeEntities:
 * @ctxt:  the parser context
 * @str:  the input string
 * @len: the string length
 * @what:  combination of XML_SUBSTITUTE_REF and XML_SUBSTITUTE_PEREF
 * @end:  an end marker xmlChar, 0 if none
 * @end2:  an end marker xmlChar, 0 if none
 * @end3:  an end marker xmlChar, 0 if none
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Takes a entity string content and process to do the adequate substitutions.
 *
 * [67] Reference ::= EntityRef | CharRef
 *
 * [69] PEReference ::= '%' Name ';'
 *
 * Returns A newly allocated string with the substitution done. The caller
 *      must deallocate it !
 */
pub(crate) unsafe extern "C" fn xml_string_len_decode_entities(
    ctxt: XmlParserCtxtPtr,
    str: *const XmlChar,
    len: c_int,
    what: c_int,
    end: XmlChar,
    end2: XmlChar,
    end3: XmlChar,
) -> *mut XmlChar {
    if ctxt.is_null() || str.is_null() || len < 0 {
        return null_mut();
    }
    xml_string_decode_entities_int(ctxt, str, len, what, end, end2, end3, 0)
}

/*
 * Generated by MACROS on top of parser.c c.f. PUSH_AND_POP.
 */
/**
 * nodePush:
 * @ctxt:  an XML parser context
 * @value:  the element node
 *
 * DEPRECATED: Internal function, do not use.
 *
 * Pushes a new element node on top of the node stack
 *
 * Returns -1 in case of error, the index in the stack otherwise
 */
pub(crate) unsafe extern "C" fn node_push(ctxt: XmlParserCtxtPtr, value: XmlNodePtr) -> c_int {
    if ctxt.is_null() {
        return 0;
    }
    if (*ctxt).node_nr >= (*ctxt).node_max {
        let tmp: *mut XmlNodePtr = xml_realloc(
            (*ctxt).node_tab as _,
            (*ctxt).node_max as usize * 2 * size_of::<XmlNodePtr>(),
        ) as *mut XmlNodePtr;
        if tmp.is_null() {
            xml_err_memory(ctxt, null());
            return -1;
        }
        (*ctxt).node_tab = tmp;
        (*ctxt).node_max *= 2;
    }
    if (*ctxt).node_nr as c_uint > XML_PARSER_MAX_DEPTH
        && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0
    {
        xml_fatal_err_msg_int(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            c"Excessive depth in document: %d use XML_PARSE_HUGE option\n".as_ptr() as _,
            XML_PARSER_MAX_DEPTH as i32,
        );
        xml_halt_parser(ctxt);
        return -1;
    }
    *(*ctxt).node_tab.add((*ctxt).node_nr as usize) = value;
    (*ctxt).node = value;
    let res = (*ctxt).node_nr;
    (*ctxt).node_nr += 1;
    res
}

/**
 * nodePop:
 * @ctxt: an XML parser context
 *
 * DEPRECATED: Internal function, do not use.
 *
 * Pops the top element node from the node stack
 *
 * Returns the node just removed
 */
pub(crate) unsafe extern "C" fn node_pop(ctxt: XmlParserCtxtPtr) -> XmlNodePtr {
    if ctxt.is_null() {
        return null_mut();
    }
    if (*ctxt).node_nr <= 0 {
        return null_mut();
    }
    (*ctxt).node_nr -= 1;
    if (*ctxt).node_nr > 0 {
        (*ctxt).node = *(*ctxt).node_tab.add((*ctxt).node_nr as usize - 1);
    } else {
        (*ctxt).node = null_mut();
    }
    let ret: XmlNodePtr = *(*ctxt).node_tab.add((*ctxt).node_nr as usize);
    *(*ctxt).node_tab.add((*ctxt).node_nr as usize) = null_mut();
    ret
}

/**
 * inputPush:
 * @ctxt:  an XML parser context
 * @value:  the parser input
 *
 * Pushes a new parser input on top of the input stack
 *
 * Returns -1 in case of error, the index in the stack otherwise
 */
pub unsafe extern "C" fn input_push(ctxt: XmlParserCtxtPtr, value: XmlParserInputPtr) -> c_int {
    if ctxt.is_null() || value.is_null() {
        return -1;
    }
    if (*ctxt).input_nr >= (*ctxt).input_max {
        let new_size: size_t = (*ctxt).input_max as usize * 2;

        let tmp: *mut XmlParserInputPtr = xml_realloc(
            (*ctxt).input_tab as _,
            new_size * size_of::<XmlParserInputPtr>(),
        ) as *mut XmlParserInputPtr;
        if tmp.is_null() {
            xml_err_memory(ctxt, null());
            return -1;
        }
        (*ctxt).input_tab = tmp;
        (*ctxt).input_max = new_size as _;
    }
    *(*ctxt).input_tab.add((*ctxt).input_nr as usize) = value;
    (*ctxt).input = value;
    let res = (*ctxt).input_nr;
    (*ctxt).input_nr += 1;
    res
}

/**
 * inputPop:
 * @ctxt: an XML parser context
 *
 * Pops the top parser input from the input stack
 *
 * Returns the input just removed
 */
pub unsafe extern "C" fn input_pop(ctxt: XmlParserCtxtPtr) -> XmlParserInputPtr {
    if ctxt.is_null() {
        return null_mut();
    }
    if (*ctxt).input_nr <= 0 {
        return null_mut();
    }
    (*ctxt).input_nr -= 1;
    if (*ctxt).input_nr > 0 {
        (*ctxt).input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 1);
    } else {
        (*ctxt).input = null_mut();
    }
    let ret: XmlParserInputPtr = *(*ctxt).input_tab.add((*ctxt).input_nr as usize);
    *(*ctxt).input_tab.add((*ctxt).input_nr as usize) = null_mut();
    ret
}

/**
 * namePop:
 * @ctxt: an XML parser context
 *
 * DEPRECATED: Internal function, do not use.
 *
 * Pops the top element name from the name stack
 *
 * Returns the name just removed
 */
pub(crate) unsafe extern "C" fn name_pop(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    if ctxt.is_null() || (*ctxt).name_nr <= 0 {
        return null_mut();
    }
    (*ctxt).name_nr -= 1;
    if (*ctxt).name_nr > 0 {
        (*ctxt).name = *(*ctxt).name_tab.add((*ctxt).name_nr as usize - 1);
    } else {
        (*ctxt).name = null_mut();
    }
    let ret: *const XmlChar = *(*ctxt).name_tab.add((*ctxt).name_nr as usize);
    *(*ctxt).name_tab.add((*ctxt).name_nr as usize) = null_mut();
    ret
}

/**
 * namePush:
 * @ctxt:  an XML parser context
 * @value:  the element name
 *
 * DEPRECATED: Internal function, do not use.
 *
 * Pushes a new element name on top of the name stack
 *
 * Returns -1 in case of error, the index in the stack otherwise
 */
pub(crate) unsafe extern "C" fn name_push(ctxt: XmlParserCtxtPtr, value: *const XmlChar) -> c_int {
    if ctxt.is_null() {
        return -1;
    }

    if (*ctxt).name_nr >= (*ctxt).name_max {
        let tmp: *mut *const XmlChar = xml_realloc(
            (*ctxt).name_tab as _,
            (*ctxt).name_max as usize * 2 * size_of_val(&*(*ctxt).name_tab.add(0)),
        ) as *mut *const XmlChar;
        if tmp.is_null() {
            xml_err_memory(ctxt, null());
            return -1;
        }
        (*ctxt).name_tab = tmp;
        (*ctxt).name_max *= 2;
    }
    *(*ctxt).name_tab.add((*ctxt).name_nr as usize) = value;
    (*ctxt).name = value;
    let res = (*ctxt).name_nr;
    (*ctxt).name_nr += 1;
    res
}

/*
 * other commodities shared between parser.c and parserInternals.
 */
/**
 * xmlErrEncodingInt:
 * @ctxt:  an XML parser context
 * @error:  the error number
 * @msg:  the error message
 * @val:  an integer value
 *
 * n encoding error
 */
unsafe extern "C" fn xml_err_encoding_int(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    val: c_int,
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
        null_mut(),
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromParser as i32,
        error as i32,
        XmlErrorLevel::XmlErrFatal,
        null_mut(),
        0,
        null_mut(),
        null_mut(),
        null_mut(),
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

/**
 * xmlStringCurrentChar:
 * @ctxt:  the XML parser context
 * @cur:  pointer to the beginning of the c_char
 * @len:  pointer to the length of the c_char read
 *
 * DEPRECATED: Internal function, do not use.
 *
 * The current c_char value, if using UTF-8 this may actually span multiple
 * bytes in the input buffer.
 *
 * Returns the current c_char value and its length
 */
pub(crate) unsafe extern "C" fn xml_string_current_char(
    ctxt: XmlParserCtxtPtr,
    cur: *const XmlChar,
    len: *mut c_int,
) -> c_int {
    if len.is_null() || cur.is_null() {
        return 0;
    }
    'encoding_error: {
        if ctxt.is_null() || (*ctxt).charset == XmlCharEncoding::XmlCharEncodingUtf8 as i32 {
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

            let mut val: c_uint;
            let c: c_uchar = *cur;
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
                if !IS_CHAR!(val) {
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

/**
 * xmlParserHandlePEReference:
 * @ctxt:  the parser context
 *
 * DEPRECATED: Internal function, do not use.
 *
 * [69] PEReference ::= '%' Name ';'
 *
 * [ WFC: No Recursion ]
 * A parsed entity must not contain a recursive
 * reference to itself, either directly or indirectly.
 *
 * [ WFC: Entity Declared ]
 * In a document without any DTD, a document with only an internal DTD
 * subset which contains no parameter entity references, or a document
 * with "standalone='yes'", ...  ... The declaration of a parameter
 * entity must precede any reference to it...
 *
 * [ VC: Entity Declared ]
 * In a document with an external subset or external parameter entities
 * with "standalone='no'", ...  ... The declaration of a parameter entity
 * must precede any reference to it...
 *
 * [ WFC: In DTD ]
 * Parameter-entity references may only appear in the DTD.
 * NOTE: misleading but this is handled.
 *
 * A PEReference may have been detected in the current input stream
 * the handling is done accordingly to
 *      http://www.w3.org/TR/REC-xml#entproc
 * i.e.
 *   - Included in literal in entity values
 *   - Included as Parameter Entity reference within DTDs
 */
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
            if (*ctxt).external == 0 && (*ctxt).input_nr == 1 {
                return;
            }
            if IS_BLANK_CH!(NXT!(ctxt, 1)) || NXT!(ctxt, 1) == 0 {
                return;
            }
        }
        XmlParserInputState::XmlParserIgnore => {
            return;
        }
    }

    xml_parse_pe_reference(ctxt);
}

/**
 * xmlCheckLanguageID:
 * @lang:  pointer to the string value
 *
 * DEPRECATED: Internal function, do not use.
 *
 * Checks that the value conforms to the LanguageID production:
 *
 * NOTE: this is somewhat deprecated, those productions were removed from
 *       the XML Second edition.
 *
 * [33] LanguageID ::= Langcode ('-' Subcode)*
 * [34] Langcode ::= ISO639Code |  IanaCode |  UserCode
 * [35] ISO639Code ::= ([a-z] | [A-Z]) ([a-z] | [A-Z])
 * [36] IanaCode ::= ('i' | 'I') '-' ([a-z] | [A-Z])+
 * [37] UserCode ::= ('x' | 'X') '-' ([a-z] | [A-Z])+
 * [38] Subcode ::= ([a-z] | [A-Z])+
 *
 * The current REC reference the successors of RFC 1766, currently 5646
 *
 * http://www.rfc-editor.org/rfc/rfc5646.txt
 * langtag       = language
 *                 ["-" script]
 *                 ["-" region]
 *                 *("-" variant)
 *                 *("-" extension)
 *                 ["-" privateuse]
 * language      = 2*3ALPHA            ; shortest ISO 639 code
 *                 ["-" extlang]       ; sometimes followed by
 *                                     ; extended language subtags
 *               / 4ALPHA              ; or reserved for future use
 *               / 5*8ALPHA            ; or registered language subtag
 *
 * extlang       = 3ALPHA              ; selected ISO 639 codes
 *                 *2("-" 3ALPHA)      ; permanently reserved
 *
 * script        = 4ALPHA              ; ISO 15924 code
 *
 * region        = 2ALPHA              ; ISO 3166-1 code
 *               / 3DIGIT              ; UN M.49 code
 *
 * variant       = 5*8alphanum         ; registered variants
 *               / (DIGIT 3alphanum)
 *
 * extension     = singleton 1*("-" (2*8alphanum))
 *                                     ; Single alphanumerics
 *                                     ; "x" reserved for private use
 * singleton     = DIGIT               ; 0 - 9
 *               / %x41-57             ; A - W
 *               / %x59-5A             ; Y - Z
 *               / %x61-77             ; a - w
 *               / %x79-7A             ; y - z
 *
 * it sounds right to still allow Irregular i-xxx IANA and user codes too
 * The parser below doesn't try to cope with extension or privateuse
 * that could be added but that's not interoperable anyway
 *
 * Returns 1 if correct 0 otherwise
 **/
pub(crate) unsafe extern "C" fn xml_check_language_id(lang: *const XmlChar) -> c_int {
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

/*
 * Really core function shared with HTML parser.
 */
/**
 * xmlCurrentChar:
 * @ctxt:  the XML parser context
 * @len:  pointer to the length of the c_char read
 *
 * DEPRECATED: Internal function, do not use.
 *
 * The current c_char value, if using UTF-8 this may actually span multiple
 * bytes in the input buffer. Implement the end of line normalization:
 * 2.11 End-of-Line Handling
 * Wherever an external parsed entity or the literal entity value
 * of an internal parsed entity contains either the literal two-character
 * sequence "#xD#xA" or a standalone literal #xD, an XML processor
 * must pass to the application the single character #xA.
 * This behavior can conveniently be produced by normalizing all
 * line breaks to #xA on input, before parsing.)
 *
 * Returns the current c_char value and its length
 */
#[doc(hidden)]
pub unsafe extern "C" fn xml_current_char(ctxt: XmlParserCtxtPtr, len: *mut c_int) -> c_int {
    if ctxt.is_null() || len.is_null() || (*ctxt).input.is_null() {
        return 0;
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return 0;
    }

    if (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) < INPUT_CHUNK as isize
        && xml_parser_grow(ctxt) < 0
    {
        return 0;
    }

    if *(*(*ctxt).input).cur >= 0x20 && *(*(*ctxt).input).cur <= 0x7F {
        *len = 1;
        return *(*(*ctxt).input).cur as _;
    }
    'incomplete_sequence: {
        'encoding_error: {
            if (*ctxt).charset == XmlCharEncoding::XmlCharEncodingUtf8 as i32 {
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
                let cur: *const c_uchar = (*(*ctxt).input).cur;
                let mut val: c_uint;
                let c: c_uchar = *cur;

                if c & 0x80 != 0 {
                    if c & 0x40 == 0 || c == 0xC0 {
                        break 'encoding_error;
                    }

                    let avail: size_t = (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) as _;

                    if avail < 2 {
                        break 'incomplete_sequence;
                    }
                    if *cur.add(1) & 0xc0 != 0x80 {
                        break 'encoding_error;
                    }
                    if c & 0xe0 == 0xe0 {
                        if avail < 3 {
                            break 'incomplete_sequence;
                        }
                        if *cur.add(2) & 0xc0 != 0x80 {
                            break 'encoding_error;
                        }
                        if c & 0xf0 == 0xf0 {
                            if avail < 4 {
                                break 'incomplete_sequence;
                            }
                            if c & 0xf8 != 0xf0 || *cur.add(3) & 0xc0 != 0x80 {
                                break 'encoding_error;
                            }
                            /* 4-byte code */
                            *len = 4;
                            val = (*cur.add(0) as u32 & 0x7) << 18;
                            val |= (*cur.add(1) as u32 & 0x3f) << 12;
                            val |= (*cur.add(2) as u32 & 0x3f) << 6;
                            val |= *cur.add(3) as u32 & 0x3f;
                            if val < 0x10000 {
                                break 'encoding_error;
                            }
                        } else {
                            /* 3-byte code */
                            *len = 3;
                            val = (*cur.add(0) as u32 & 0xf) << 12;
                            val |= (*cur.add(1) as u32 & 0x3f) << 6;
                            val |= *cur.add(2) as u32 & 0x3f;
                            if val < 0x800 {
                                break 'encoding_error;
                            }
                        }
                    } else {
                        /* 2-byte code */
                        *len = 2;
                        val = (*cur.add(0) as u32 & 0x1f) << 6;
                        val |= *cur.add(1) as u32 & 0x3f;
                        if val < 0x80 {
                            break 'encoding_error;
                        }
                    }
                    if !IS_CHAR!(val) {
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
                    if *(*(*ctxt).input).cur == 0 && (*(*ctxt).input).end > (*(*ctxt).input).cur {
                        xml_err_encoding_int(
                            ctxt,
                            XmlParserErrors::XmlErrInvalidChar,
                            c"Char 0x0 out of allowed range\n".as_ptr() as _,
                            0,
                        );
                    }
                    if *(*(*ctxt).input).cur == 0xD {
                        if *(*(*ctxt).input).cur.add(1) == 0xA {
                            (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(1);
                        }
                        return 0xA;
                    }
                    return *(*(*ctxt).input).cur as _;
                }
            }

            /*
             * Assume it's a fixed length encoding (1) with
             * a compatible encoding for the ASCII set, since
             * XML constructs only use < 128 chars
             */
            *len = 1;
            if *(*(*ctxt).input).cur == 0xD {
                if *(*(*ctxt).input).cur.add(1) == 0xA {
                    (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(1);
                }
                return 0xA;
            }
            return *(*(*ctxt).input).cur as _;
        }

        // encoding_error:
        /*
         * If we detect an UTF8 error that probably mean that the
         * input encoding didn't get properly advertised in the
         * declaration header. Report the error and switch the encoding
         * to ISO-Latin-1 (if you don't like this policy, just declare the
         * encoding !)
         */
        if (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) < 4 {
            __xml_err_encoding(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                c"Input is not proper UTF-8, indicate encoding !\n".as_ptr() as _,
                null(),
                null(),
            );
        } else {
            let mut buffer: [c_char; 150] = [0; 150];

            snprintf(
                buffer.as_mut_ptr(),
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
        (*ctxt).charset = XmlCharEncoding::XmlCharEncoding8859_1 as i32;
        *len = 1;
        return *(*(*ctxt).input).cur as i32;
    }

    // incomplete_sequence:
    /*
     * An encoding problem may arise from a truncated input buffer
     * splitting a character in the middle. In that case do not raise
     * an error but return 0. This should only happen when push parsing
     * c_char data.
     */
    *len = 0;
    0
}

/**
 * xmlCopyCharMultiByte:
 * @out:  pointer to an array of xmlChar
 * @val:  the c_char value
 *
 * append the c_char value in the array
 *
 * Returns the number of xmlChar written
 */
pub unsafe extern "C" fn xml_copy_char_multi_byte(mut out: *mut XmlChar, val: c_int) -> c_int {
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
        let bits: c_int;
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

/**
 * xmlCopyChar:
 * @len:  Ignored, compatibility
 * @out:  pointer to an array of xmlChar
 * @val:  the c_char value
 *
 * append the c_char value in the array
 *
 * Returns the number of xmlChar written
 */
pub unsafe extern "C" fn xml_copy_char(_len: c_int, out: *mut XmlChar, val: c_int) -> c_int {
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

/**
 * xmlNextChar:
 * @ctxt:  the XML parser context
 *
 * DEPRECATED: Internal function, do not use.
 *
 * Skip to the next c_char input c_char.
 */
pub(crate) unsafe extern "C" fn xml_next_char(ctxt: XmlParserCtxtPtr) {
    if ctxt.is_null()
        || matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        || (*ctxt).input.is_null()
    {
        return;
    }

    if !VALID_CTXT!(ctxt) {
        xml_err_internal(
            ctxt,
            c"Parser input data memory error\n".as_ptr() as _,
            null(),
        );

        (*ctxt).err_no = XmlParserErrors::XmlErrInternalError as i32;
        xml_stop_parser(ctxt);

        return;
    }

    if (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) < INPUT_CHUNK as isize {
        if xml_parser_grow(ctxt) < 0 {
            return;
        }
        if (*(*ctxt).input).cur >= (*(*ctxt).input).end {
            return;
        }
    }

    'encoding_error: {
        if (*ctxt).charset == XmlCharEncoding::XmlCharEncodingUtf8 as i32 {
            /*
             *   2.11 End-of-Line Handling
             *   the literal two-character sequence "#xD#xA" or a standalone
             *   literal #xD, an XML processor must pass to the application
             *   the single character #xA.
             */
            if *(*(*ctxt).input).cur == b'\n' {
                (*(*ctxt).input).line += 1;
                (*(*ctxt).input).col = 1;
            } else {
                (*(*ctxt).input).col += 1;
            }

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
            let cur: *const c_uchar = (*(*ctxt).input).cur;

            let c: c_uchar = *cur;
            if c & 0x80 != 0 {
                if c == 0xC0 {
                    break 'encoding_error;
                }

                let avail: size_t = (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) as _;

                if avail < 2 || *cur.add(1) & 0xc0 != 0x80 {
                    break 'encoding_error;
                }
                if c & 0xe0 == 0xe0 {
                    let mut val: c_uint;

                    if avail < 3 || *cur.add(2) & 0xc0 != 0x80 {
                        break 'encoding_error;
                    }
                    if c & 0xf0 == 0xf0 {
                        if c & 0xf8 != 0xf0 || avail < 4 || *cur.add(3) & 0xc0 != 0x80 {
                            break 'encoding_error;
                        }
                        /* 4-byte code */
                        (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(4);
                        val = (*cur.add(0) as u32 & 0x7) << 18;
                        val |= (*cur.add(1) as u32 & 0x3f) << 12;
                        val |= (*cur.add(2) as u32 & 0x3f) << 6;
                        val |= *cur.add(3) as u32 & 0x3f;
                    } else {
                        /* 3-byte code */
                        (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(3);
                        val = (*cur.add(0) as u32 & 0xf) << 12;
                        val |= (*cur.add(1) as u32 & 0x3f) << 6;
                        val |= *cur.add(2) as u32 & 0x3f;
                    }
                    if (val > 0xd7ff && val < 0xe000)
                        || (val > 0xfffd && val < 0x10000)
                        || val >= 0x110000
                    {
                        xml_err_encoding_int(
                            ctxt,
                            XmlParserErrors::XmlErrInvalidChar,
                            c"Char 0x%X out of allowed range\n".as_ptr() as _,
                            val as _,
                        );
                    }
                } else {
                    /* 2-byte code */
                    (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(2);
                }
            } else {
                /* 1-byte code */
                (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(1);
            }
        } else {
            /*
             * Assume it's a fixed length encoding (1) with
             * a compatible encoding for the ASCII set, since
             * XML constructs only use < 128 chars
             */

            if *((*(*ctxt).input).cur) == b'\n' {
                (*(*ctxt).input).line += 1;
                (*(*ctxt).input).col = 1;
            } else {
                (*(*ctxt).input).col += 1;
            }
            (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(1);
        }
        return;
    }
    // encoding_error:
    /*
     * If we detect an UTF8 error that probably mean that the
     * input encoding didn't get properly advertised in the
     * declaration header. Report the error and switch the encoding
     * to ISO-Latin-1 (if you don't like this policy, just declare the
     * encoding !)
     */
    if ctxt.is_null()
        || (*ctxt).input.is_null()
        || (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) < 4
    {
        __xml_err_encoding(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            c"Input is not proper UTF-8, indicate encoding !\n".as_ptr() as _,
            null(),
            null(),
        );
    } else {
        let mut buffer: [c_char; 150] = [0; 150];

        snprintf(
            buffer.as_mut_ptr(),
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
    (*ctxt).charset = XmlCharEncoding::XmlCharEncoding8859_1 as i32;
    (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(1);
}

/* we need to keep enough input to show errors in context */
pub(crate) const LINE_LEN: usize = 80;

/**
 * xmlParserInputShrink:
 * @in:  an XML parser input
 *
 * DEPRECATED: Don't use.
 *
 * This function removes used input for the parser.
 */
pub(crate) unsafe extern "C" fn xml_parser_input_shrink(input: XmlParserInputPtr) {
    let mut used: size_t;
    let ret: size_t;

    // #ifdef DEBUG_INPUT
    //     xmlGenericError(xmlGenericErrorContext(), "Shrink\n");
    // #endif
    if input.is_null() {
        return;
    }
    if (*input).buf.is_null() {
        return;
    }
    if (*input).base.is_null() {
        return;
    }
    if (*input).cur.is_null() {
        return;
    }
    if (*(*input).buf).buffer.is_null() {
        return;
    }

    // CHECK_BUFFER(input);

    used = (*input).cur.offset_from((*input).base) as _;
    /*
     * Do not shrink on large buffers whose only a tiny fraction
     * was consumed
     */
    if used > INPUT_CHUNK {
        ret = xml_buf_shrink((*(*input).buf).buffer, used - LINE_LEN);
        if ret > 0 {
            used -= ret;
            if ret as u64 > u64::MAX || (*input).consumed > u64::MAX - ret as c_ulong {
                (*input).consumed = u64::MAX;
            } else {
                (*input).consumed += ret as u64;
            }
        }
    }

    if xml_buf_use((*(*input).buf).buffer) <= INPUT_CHUNK {
        xml_parser_input_buffer_read((*input).buf, 2 * INPUT_CHUNK as i32);
    }

    (*input).base = xml_buf_content((*(*input).buf).buffer);
    if (*input).base.is_null() {
        /* TODO: raise error */
        (*input).base = c"".as_ptr() as _;
        (*input).cur = (*input).base;
        (*input).end = (*input).base;
        return;
    }
    (*input).cur = (*input).base.add(used);
    (*input).end = xml_buf_end((*(*input).buf).buffer);

    // CHECK_BUFFER(input);
}

/*
 * Specific function to keep track of entities references
 * and used by the XSLT debugger.
 */
#[cfg(feature = "legacy")]
mod __parser_internal_for_legacy {
    use std::{
        ffi::c_int,
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
            xml_generic_error!(
                xml_generic_error_context(),
                c"xmlParseQuotedString() deprecated function reached\n".as_ptr()
            );
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
            xml_generic_error!(
                xml_generic_error_context(),
                c"xmlParseNamespace() deprecated function reached\n".as_ptr() as _,
            );
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
            xml_generic_error!(
                xml_generic_error_context(),
                c"xmlNamespaceParseNSDef() deprecated function reached\n".as_ptr() as _,
            );
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
            xml_generic_error!(
                xml_generic_error_context(),
                c"xmlScanName() deprecated function reached\n".as_ptr() as _,
            );
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
            xml_generic_error!(
                xml_generic_error_context(),
                c"xmlNamespaceParseNCName() deprecated function reached\n".as_ptr() as _,
            );
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
            xml_generic_error!(
                xml_generic_error_context(),
                c"xmlParserHandleReference() deprecated function reached\n".as_ptr() as _,
            );
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
            xml_generic_error!(
                xml_generic_error_context(),
                c"xmlNamespaceParseQName() deprecated function reached\n".as_ptr() as _,
            );
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
        _len: c_int,
        _what: c_int,
        _end: XmlChar,
        _end2: XmlChar,
        _end3: XmlChar,
    ) -> *mut XmlChar {
        static DEPRECATED: AtomicBool = AtomicBool::new(false);

        if !DEPRECATED.load(Ordering::Acquire) {
            xml_generic_error!(
                xml_generic_error_context(),
                c"xmlDecodeEntities() deprecated function reached\n".as_ptr() as _,
            );
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
            xml_generic_error!(
                xml_generic_error_context(),
                c"xmlHandleEntity() deprecated function reached\n".as_ptr() as _,
            );
            DEPRECATED.store(true, Ordering::Release);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        libxml::{
            parser::xml_pop_input, xmlerror::xml_reset_last_error, xmlmemory::xml_mem_blocks,
        },
        test_util::*,
    };

    use super::*;

    #[test]
    fn test_input_pop() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                let ret_val = input_pop(ctxt);
                desret_xml_parser_input_ptr(ret_val);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in inputPop",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in inputPop");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_input_push() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_value in 0..GEN_NB_XML_PARSER_INPUT_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let value = gen_xml_parser_input_ptr(n_value, 1);

                    let ret_val = input_push(ctxt, value);
                    desret_int(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_parser_input_ptr(n_value, value, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in inputPush",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in inputPush()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_value);
                    }
                }
            }
        }
    }

    #[test]
    fn test_name_pop() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                let ret_val = name_pop(ctxt);
                desret_const_xml_char_ptr(ret_val);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in namePop",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in namePop");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_name_push() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let value = gen_const_xml_char_ptr(n_value, 1);

                    let ret_val = name_push(ctxt, value);
                    desret_int(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_const_xml_char_ptr(n_value, value, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in namePush",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in namePush");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_value);
                    }
                }
            }
        }
    }

    #[test]
    fn test_node_pop() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                let ret_val = node_pop(ctxt);
                desret_xml_node_ptr(ret_val);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in nodePop",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in nodePop");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_node_push() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_value in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let value = gen_xml_node_ptr(n_value, 1);

                    let ret_val = node_push(ctxt, value);
                    desret_int(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_value, value, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in nodePush",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in nodePush");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_value);
                    }
                }
            }
        }
    }

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
                xml_reset_last_error();
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
                        xml_reset_last_error();
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
                    xml_reset_last_error();
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
                        xml_reset_last_error();
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
                xml_reset_last_error();
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
    fn test_xml_create_memory_parser_ctxt() {
        unsafe {
            let mut leaks = 0;

            for n_buffer in 0..GEN_NB_CONST_CHAR_PTR {
                for n_size in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let buffer = gen_const_char_ptr(n_buffer, 0);
                    let mut size = gen_int(n_size, 1);
                    if !buffer.is_null() && size > xml_strlen(buffer as _) {
                        size = 0;
                    }

                    let ret_val = xml_create_memory_parser_ctxt(buffer, size);
                    desret_xml_parser_ctxt_ptr(ret_val);
                    des_const_char_ptr(n_buffer, buffer, 0);
                    des_int(n_size, size, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCreateMemoryParserCtxt",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlCreateMemoryParserCtxt()"
                        );
                        eprint!(" {}", n_buffer);
                        eprintln!(" {}", n_size);
                    }
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
                    xml_reset_last_error();
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
    fn test_xml_current_char() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_len in 0..GEN_NB_INT_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let len = gen_int_ptr(n_len, 1);

                    let ret_val = xml_current_char(ctxt, len);
                    desret_int(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_int_ptr(n_len, len, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCurrentChar",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlCurrentChar()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_len);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_is_letter() {
        unsafe {
            let mut leaks = 0;

            for n_c in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let c = gen_int(n_c, 0);

                let ret_val = xml_is_letter(c);
                desret_int(ret_val);
                des_int(n_c, c, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlIsLetter",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlIsLetter()");
                    eprintln!(" {}", n_c);
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
                    xml_reset_last_error();
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
                    xml_reset_last_error();
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
                xml_reset_last_error();
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
                    xml_reset_last_error();
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
    fn test_xml_next_char() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                xml_next_char(ctxt);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNextChar",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlNextChar()");
                    eprintln!(" {}", n_ctxt);
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                    xml_reset_last_error();
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
                        xml_reset_last_error();
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
                        xml_reset_last_error();
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
                                    xml_reset_last_error();
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
                                        xml_reset_last_error();
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

    #[test]
    fn test_xml_switch_encoding() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_enc in 0..GEN_NB_XML_CHAR_ENCODING {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let enc = gen_xml_char_encoding(n_enc, 1);

                    let ret_val = xml_switch_encoding(ctxt, enc);
                    desret_int(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_char_encoding(n_enc, enc, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSwitchEncoding",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlSwitchEncoding()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_enc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_switch_input_encoding() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_input in 0..GEN_NB_XML_PARSER_INPUT_PTR {
                    for n_handler in 0..GEN_NB_XML_CHAR_ENCODING_HANDLER_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                        let input = gen_xml_parser_input_ptr(n_input, 1);
                        let handler = gen_xml_char_encoding_handler_ptr(n_handler, 2);

                        let ret_val = xml_switch_input_encoding(ctxt, input, handler);
                        desret_int(ret_val);
                        des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_parser_input_ptr(n_input, input, 1);
                        des_xml_char_encoding_handler_ptr(n_handler, handler, 2);
                        xml_reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSwitchInputEncoding",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlSwitchInputEncoding()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_input);
                            eprintln!(" {}", n_handler);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_switch_to_encoding() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_handler in 0..GEN_NB_XML_CHAR_ENCODING_HANDLER_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let handler = gen_xml_char_encoding_handler_ptr(n_handler, 1);

                    let ret_val = xml_switch_to_encoding(ctxt, handler);
                    desret_int(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_char_encoding_handler_ptr(n_handler, handler, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSwitchToEncoding",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlSwitchToEncoding()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_handler);
                    }
                }
            }
        }
    }
}

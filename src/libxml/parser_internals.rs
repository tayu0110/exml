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

use std::{
    borrow::Cow,
    ffi::CStr,
    mem::take,
    ptr::{addr_of_mut, null_mut},
};

use crate::{
    error::XmlParserErrors,
    globals::GenericErrorContext,
    libxml::{
        chvalid::{xml_is_base_char, xml_is_ideographic},
        dict::xml_dict_free,
        parser::XML_SKIP_IDS,
        valid::xml_validate_element,
        xmlstring::XmlChar,
    },
    parser::{
        XmlParserCtxtPtr, XmlParserInputState, XmlParserOption, xml_create_memory_parser_ctxt,
        xml_fatal_err, xml_fatal_err_msg, xml_free_parser_ctxt,
    },
    tree::{
        NodeCommon, XML_XML_NAMESPACE, XmlDocProperties, XmlElementType, XmlGenericNodePtr,
        XmlNodePtr, xml_free_doc, xml_free_node, xml_new_doc, xml_new_doc_node,
    },
};

macro_rules! NXT {
    ($ctxt:expr, $val:expr) => {
        *(*(*$ctxt).input().unwrap()).cur.add($val as usize)
    };
}

/// Arbitrary depth limit for the XML documents that we allow to
/// process. This is not a limitation of the parser but a safety
/// boundary feature, use XML_PARSE_HUGE option to override it.
#[doc(alias = "xmlParserMaxDepth")]
pub const XML_PARSER_MAX_DEPTH: u32 = 256;

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

// #[doc(alias = "xmlParseCharData")]
// pub(crate) unsafe fn xml_parse_char_data(ctxt: XmlParserCtxtPtr, _cdata: i32) {
//     unsafe {
//         parse_char_data_internal(&mut *ctxt, 0);
//     }
// }

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
pub(crate) unsafe fn xml_parse_balanced_chunk_memory_internal(
    oldctxt: XmlParserCtxtPtr,
    string: &str,
    user_data: Option<GenericErrorContext>,
    mut lst: Option<&mut Option<XmlGenericNodePtr>>,
) -> XmlParserErrors {
    unsafe {
        let mut content = None;
        let mut last = None;
        let ret: XmlParserErrors;

        if ((*oldctxt).depth > 40 && (*oldctxt).options & XmlParserOption::XmlParseHuge as i32 == 0)
            || (*oldctxt).depth > 100
        {
            xml_fatal_err_msg(
                &mut *oldctxt,
                XmlParserErrors::XmlErrEntityLoop,
                "Maximum entity nesting depth exceeded",
            );
            return XmlParserErrors::XmlErrEntityLoop;
        }

        if let Some(lst) = lst.as_mut() {
            **lst = None;
        }

        let ctxt: XmlParserCtxtPtr = xml_create_memory_parser_ctxt(string.as_bytes().to_vec());
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
            content = my_doc.children;
            last = my_doc.last;
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
        let Some(new_root) = xml_new_doc_node((*ctxt).my_doc, None, "pseudoroot", None) else {
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
                .map(|c| XmlNodePtr::try_from(c).unwrap())
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
        (*ctxt).atts_default = take(&mut (*oldctxt).atts_default);
        (*ctxt).atts_special = (*oldctxt).atts_special;

        (*ctxt).parse_content();
        if (*ctxt).content_bytes().starts_with(b"</") {
            xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
        } else if (*ctxt).current_byte() != 0 {
            xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrExtraContent, None);
        }
        if my_doc.children != (*ctxt).node.map(|node| node.into()) {
            xml_fatal_err(&mut *ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
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
                let mut cur = my_doc.children().unwrap().children();
                *lst = cur;
                while let Some(mut now) = cur {
                    #[cfg(feature = "libxml_valid")]
                    if (*oldctxt).validate != 0
                        && (*oldctxt).well_formed != 0
                        && now.element_type() == XmlElementType::XmlElementNode
                    {
                        if let Some(my_doc) =
                            (*oldctxt).my_doc.filter(|doc| doc.int_subset.is_some())
                        {
                            (*oldctxt).valid &=
                                xml_validate_element(addr_of_mut!((*oldctxt).vctxt), my_doc, cur);
                        }
                    }
                    now.set_parent(None);
                    cur = now.next();
                }
                my_doc.children().unwrap().set_children(None);
            }
        }
        if let Some(mut my_doc) = (*ctxt).my_doc {
            xml_free_node(my_doc.children().unwrap());
            my_doc.children = content;
            my_doc.last = last;
        }

        // Also record the size of the entity parsed
        if (*ctxt).input().is_some() && !oldctxt.is_null() {
            let mut consumed: u64 = (*ctxt).input().unwrap().consumed;
            consumed = consumed.saturating_add((*ctxt).input().unwrap().offset_from_base() as u64);

            (*oldctxt).sizeentcopy = (*oldctxt).sizeentcopy.saturating_add(consumed);
            (*oldctxt).sizeentcopy = (*oldctxt).sizeentcopy.saturating_add((*ctxt).sizeentcopy);
        }

        (*oldctxt).nb_errors = (*ctxt).nb_errors;
        (*oldctxt).nb_warnings = (*ctxt).nb_warnings;
        (*oldctxt).sax = (*ctxt).sax.take();
        (*ctxt).sax = oldsax;
        (*ctxt).dict = null_mut();
        (*ctxt).atts_default.clear();
        (*ctxt).atts_special = None;
        xml_free_parser_ctxt(ctxt);
        if let Some(new_doc) = new_doc {
            xml_free_doc(new_doc);
        }

        ret
    }
}

// /// If no entities need to be substituted.
// const XML_SUBSTITUTE_NONE: usize = 0;
/// Whether general entities need to be substituted.
pub(crate) const XML_SUBSTITUTE_REF: usize = 1;
/// Whether parameter entities need to be substituted.
pub(crate) const XML_SUBSTITUTE_PEREF: usize = 2;
// /// Both general and parameter entities need to be substituted.
// const XML_SUBSTITUTE_BOTH: usize = 3;

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

// we need to keep enough input to show errors in context
pub(crate) const LINE_LEN: usize = 80;

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

//! Provide methods and data structures for parsing XML documents.
//!
//! This module is based on `libxml/parser.h`, `parser.c`, `libxml/parserInternals.h`,
//! `parserInternals.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

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
mod legacy;
mod loader;
mod node_info;
mod parse;
#[cfg(feature = "libxml_push")]
mod push;
mod qname;
mod sax;
mod valid;

use std::{
    io::Read,
    sync::atomic::{AtomicBool, Ordering},
};

use crate::{
    encoding::XmlCharEncoding,
    io::{
        XmlParserInputBuffer, cleanup_input_callbacks, cleanup_output_callbacks,
        register_default_input_callbacks, register_default_output_callbacks,
    },
    libxml::{
        catalog::xml_catalog_cleanup,
        chvalid::{
            xml_is_base_char, xml_is_char, xml_is_combining, xml_is_digit, xml_is_extender,
            xml_is_ideographic,
        },
        threads::{
            __xml_global_init_mutex_lock, __xml_global_init_mutex_unlock,
            xml_cleanup_threads_internal, xml_init_threads_internal,
        },
        xmlmemory::{xml_cleanup_memory_internal, xml_init_memory_internal},
        xmlschemastypes::xml_schema_cleanup_types,
    },
    relaxng::xml_relaxng_cleanup_types,
    tree::XmlDocPtr,
};

pub use context::*;
pub(crate) use error::*;
pub use input::*;
pub use legacy::*;
pub use loader::*;
pub use node_info::*;
pub use parse::*;
pub use qname::*;
pub use sax::*;

// /// If no entities need to be substituted.
// const XML_SUBSTITUTE_NONE: usize = 0;
/// Whether general entities need to be substituted.
pub(crate) const XML_SUBSTITUTE_REF: usize = 1;
/// Whether parameter entities need to be substituted.
pub(crate) const XML_SUBSTITUTE_PEREF: usize = 2;
// /// Both general and parameter entities need to be substituted.
// const XML_SUBSTITUTE_BOTH: usize = 3;

/// Bit in the loadsubset context field to tell to do ID/REFs lookups.
/// Use it to initialize xmlLoadExtDtdDefaultValue.
pub const XML_DETECT_IDS: usize = 2;

/// Bit in the loadsubset context field to tell to do complete the
/// elements attributes lists with the ones defaulted from the DTDs.
/// Use it to initialize xmlLoadExtDtdDefaultValue.
pub const XML_COMPLETE_ATTRS: usize = 4;

/// Bit in the loadsubset context field to tell to not do ID/REFs registration.
/// Used to initialize xmlLoadExtDtdDefaultValue in some special cases.
pub const XML_SKIP_IDS: usize = 8;

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

// we need to keep enough input to show errors in context
pub(crate) const LINE_LEN: usize = 80;

/// Global variables used for predefined strings.
pub static XML_STRING_TEXT: &str = "text";
pub static XML_STRING_TEXT_NOENC: &str = "textnoenc";
pub static XML_STRING_COMMENT: &str = "comment";

/// Set after xmlValidateDtdFinal was called.
pub(crate) const XML_VCTXT_DTD_VALIDATED: usize = 1usize << 0;
/// Set if the validation context is part of a parser context.
pub(crate) const XML_VCTXT_USE_PCTXT: usize = 1usize << 1;

pub(crate) const XML_PARSER_BIG_BUFFER_SIZE: usize = 300;

// const XML_PARSER_BIG_ENTITY: usize = 1000;
// const XML_PARSER_LOT_ENTITY: usize = 5000;

// XML_PARSER_NON_LINEAR is roughly the maximum allowed amplification factor
// of serialized output after entity expansion.
pub(crate) const XML_PARSER_NON_LINEAR: usize = 5;

// A certain amount is always allowed.
pub(crate) const XML_PARSER_ALLOWED_EXPANSION: usize = 1000000;

// Fixed cost for each entity reference. This crudely models processing time
// as well to protect, for example, against exponential expansion of empty
// or very short entities.
pub(crate) const XML_ENT_FIXED_COST: usize = 20;

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

/// Check whether the character is allowed by the production
///
/// ```text
/// [84] Letter ::= BaseChar | Ideographic
/// ```
#[doc(alias = "xmlIsLetter")]
pub fn xml_is_letter(c: u32) -> bool {
    xml_is_base_char(c) || xml_is_ideographic(c)
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
pub(crate) fn check_language_id(lang: &str) -> bool {
    let mut cur = lang;

    if cur.starts_with("i-")
        || cur.starts_with("I-")
        || cur.starts_with("x-")
        || cur.starts_with("X-")
    {
        // Still allow IANA code and user code which were coming
        // from the previous version of the XML-1.0 specification
        // it's deprecated but we should not fail
        cur = &cur[2..];
        cur = cur.trim_start_matches(|c: char| c.is_ascii_alphabetic());
        return cur.is_empty();
    }
    let nxt = cur.trim_start_matches(|c: char| c.is_ascii_alphabetic());
    if cur.len() - nxt.len() >= 4 {
        // Reserved
        return cur.len() - nxt.len() <= 8 && nxt.is_empty();
    }
    if cur.len() - nxt.len() < 2 {
        return false;
    }
    // we got an ISO 639 code
    if nxt.is_empty() {
        return true;
    }
    let Some(mut nxt) = nxt.strip_prefix('-') else {
        return false;
    };

    cur = nxt;
    'region_m49: {
        // now we can have extlang or script or region or variant
        if nxt.starts_with(|c: char| c.is_ascii_digit()) {
            break 'region_m49;
        }
        nxt = nxt.trim_start_matches(|c: char| c.is_ascii_alphabetic());
        'variant: {
            'region: {
                'script: {
                    match cur.len() - nxt.len() {
                        4 => break 'script,
                        2 => break 'region,
                        5..=8 => break 'variant,
                        3 => {}
                        _ => return false,
                    }
                    // we parsed an extlang
                    if nxt.is_empty() {
                        return true;
                    }
                    let Some(rem) = nxt.strip_prefix('-') else {
                        return false;
                    };
                    nxt = rem;
                    cur = nxt;

                    // now we can have script or region or variant
                    if nxt.starts_with(|c: char| c.is_ascii_digit()) {
                        break 'region_m49;
                    }

                    nxt = nxt.trim_start_matches(|c: char| c.is_ascii_alphabetic());
                    match cur.len() - nxt.len() {
                        2 => break 'region,
                        5..=8 => break 'variant,
                        4 => {}
                        _ => return false,
                    }
                    // we parsed a script
                }
                if nxt.is_empty() {
                    return true;
                }
                let Some(rem) = nxt.strip_prefix('-') else {
                    return false;
                };
                nxt = rem;
                cur = nxt;
                // now we can have region or variant
                if nxt.starts_with(|c: char| c.is_ascii_digit()) {
                    break 'region_m49;
                }
                nxt = nxt.trim_start_matches(|c: char| c.is_ascii_alphabetic());

                match cur.len() - nxt.len() {
                    5..=8 => break 'variant,
                    2 => {}
                    _ => return false,
                }
                // we parsed a region
            }
            //  region:
            if nxt.is_empty() {
                return true;
            }
            let Some(rem) = nxt.strip_prefix('-') else {
                return false;
            };
            nxt = rem;
            cur = nxt;

            // now we can just have a variant
            nxt = nxt.trim_start_matches(|c: char| c.is_ascii_alphabetic());
            match cur.len() - nxt.len() {
                5..=8 => {}
                _ => return false,
            }
        }

        // we parsed a variant
        //  variant:
        // extensions and private use subtags not checked
        return nxt.is_empty() || nxt.starts_with('-');
    }

    //  region_m49:
    if nxt.len() >= 3 && nxt.as_bytes()[1].is_ascii_digit() && nxt.as_bytes()[2].is_ascii_digit() {
        nxt = &nxt[3..];
        // goto region;
        if nxt.is_empty() {
            return true;
        }
        let Some(rem) = nxt.strip_prefix('-') else {
            return false;
        };
        nxt = rem;
        cur = nxt;

        // now we can just have a variant
        nxt = nxt.trim_start_matches(|c: char| c.is_ascii_alphabetic());

        match cur.len() - nxt.len() {
            5..=8 => {}
            _ => return false,
        }

        // we parsed a variant
        //  variant:
        // extensions and private use subtags not checked
        return nxt.is_empty() || nxt.starts_with('-');
    }
    false
}

/// Parse an XML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "xmlReadDoc")]
pub fn xml_read_doc(
    cur: Vec<u8>,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> Option<XmlDocPtr> {
    xml_init_parser();

    XmlParserCtxt::from_memory(cur)?.do_read(url, encoding, options)
}

/// Parse an XML file from the filesystem or the network.
///
/// Returns the resulting document tree
#[doc(alias = "xmlReadFile")]
pub fn xml_read_file(filename: &str, encoding: Option<&str>, options: i32) -> Option<XmlDocPtr> {
    xml_init_parser();
    XmlParserCtxt::from_filename_with_options(Some(filename), options)?
        .do_read(None, encoding, options)
}

/// Parse an XML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "xmlReadMemory")]
pub fn xml_read_memory(
    buffer: Vec<u8>,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> Option<XmlDocPtr> {
    xml_init_parser();
    XmlParserCtxt::from_memory(buffer)?.do_read(url, encoding, options)
}

/// Parse an XML document from I/O functions and source and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "xmlReadIO")]
pub fn xml_read_io(
    ioctx: impl Read + 'static,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> Option<XmlDocPtr> {
    xml_init_parser();

    let input = XmlParserInputBuffer::from_reader(ioctx, XmlCharEncoding::None);
    let mut ctxt = XmlParserCtxt::new()?;
    let stream = XmlParserInput::from_io(&mut ctxt, input, XmlCharEncoding::None)?;
    ctxt.input_push(stream);
    ctxt.do_read(url, encoding, options)
}

/// Parse an XML in-memory document and build a tree.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "xmlCtxtReadDoc")]
pub fn xml_ctxt_read_doc(
    ctxt: &mut XmlParserCtxt,
    cur: Vec<u8>,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> Option<XmlDocPtr> {
    xml_ctxt_read_memory(ctxt, cur, url, encoding, options)
}

/// Parse an XML file from the filesystem or the network.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "xmlCtxtReadFile")]
pub fn xml_ctxt_read_file(
    ctxt: &mut XmlParserCtxt,
    filename: &str,
    encoding: Option<&str>,
    options: i32,
) -> Option<XmlDocPtr> {
    xml_init_parser();

    ctxt.reset();

    let stream = xml_load_external_entity(Some(filename), None, ctxt)?;
    ctxt.input_push(stream);
    ctxt.do_read(None, encoding, options)
}

/// Parse an XML in-memory document and build a tree.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "xmlCtxtReadMemory")]
pub fn xml_ctxt_read_memory(
    ctxt: &mut XmlParserCtxt,
    buffer: Vec<u8>,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> Option<XmlDocPtr> {
    xml_init_parser();
    ctxt.reset();

    let input = XmlParserInputBuffer::from_memory(buffer, XmlCharEncoding::None)?;
    let stream = XmlParserInput::from_io(ctxt, input, XmlCharEncoding::None)?;
    ctxt.input_push(stream);
    ctxt.do_read(url, encoding, options)
}

/// Parse an XML document from I/O functions and source and build a tree.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "xmlCtxtReadIO")]
pub fn xml_ctxt_read_io(
    ctxt: &mut XmlParserCtxt,
    ioctx: impl Read + 'static,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> Option<XmlDocPtr> {
    xml_init_parser();
    ctxt.reset();

    let input = XmlParserInputBuffer::from_reader(ioctx, XmlCharEncoding::None);
    let stream = XmlParserInput::from_io(ctxt, input, XmlCharEncoding::None)?;
    ctxt.input_push(stream);
    ctxt.do_read(url, encoding, options)
}

static XML_PARSER_INITIALIZED: AtomicBool = AtomicBool::new(false);

/// Initialization function for the XML parser.
/// This is not reentrant. Call once before processing in case of
/// use in multithreaded programs.
#[doc(alias = "xmlInitParser")]
pub fn xml_init_parser() {
    unsafe {
        // Note that the initialization code must not make memory allocations.
        if XML_PARSER_INITIALIZED.load(Ordering::Acquire) {
            return;
        }

        __xml_global_init_mutex_lock();
        if !XML_PARSER_INITIALIZED.load(Ordering::Acquire) {
            xml_init_threads_internal();
            xml_init_memory_internal();
            register_default_input_callbacks();
            #[cfg(feature = "libxml_output")]
            {
                register_default_output_callbacks();
            }
            XML_PARSER_INITIALIZED.store(true, Ordering::Release);
        }

        __xml_global_init_mutex_unlock();
    }
}

/// This function name is somewhat misleading. It does not clean up
/// parser state, it cleans up memory allocated by the library itself.
/// It is a cleanup function for the XML library. It tries to reclaim all
/// related global memory allocated for the library processing.
/// It doesn't deallocate any document related memory. One should
/// call xmlCleanupParser() only when the process has finished using
/// the library and all XML/HTML documents built with it.
/// See also xmlInitParser() which has the opposite function of preparing
/// the library for operations.
///
/// # Warning
/// if your application is multithreaded or has plugin support
/// calling this may crash the application if another thread or
/// a plugin is still using libxml2. It's sometimes very hard to
/// guess if libxml2 is in use in the application, some libraries
/// or plugins may use it without notice. In case of doubt abstain
/// from calling this function or do it just before calling exit()
/// to avoid leak reports from valgrind !
#[doc(alias = "xmlCleanupParser")]
pub fn xml_cleanup_parser() {
    unsafe {
        if !XML_PARSER_INITIALIZED.load(Ordering::Acquire) {
            return;
        }

        #[cfg(feature = "catalog")]
        {
            xml_catalog_cleanup();
        }
        cleanup_input_callbacks();
        #[cfg(feature = "libxml_output")]
        {
            cleanup_output_callbacks();
        }
        #[cfg(feature = "schema")]
        {
            xml_schema_cleanup_types();
            xml_relaxng_cleanup_types();
        }
        xml_cleanup_threads_internal();
        xml_cleanup_memory_internal();
        XML_PARSER_INITIALIZED.store(false, Ordering::Release);
    }
}

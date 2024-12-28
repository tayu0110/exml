//! Provide methods and data structures for parsing XML documents.  
//! This module is based on `libxml/parser.h`, `parser.c`, and so on in `libxml2-v2.11.8`.
//!
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

use std::{
    any::type_name,
    cell::RefCell,
    ffi::{c_char, c_void, CStr},
    io::Read,
    mem::{size_of, size_of_val},
    ops::DerefMut,
    ptr::{addr_of_mut, drop_in_place, null, null_mut},
    rc::Rc,
    slice::from_raw_parts,
    str::{from_utf8, from_utf8_unchecked},
    sync::atomic::{AtomicBool, AtomicPtr, Ordering},
};

#[cfg(feature = "libxml_legacy")]
use libc::strcmp;
use libc::{memchr, memcpy, memmove, memset, ptrdiff_t, size_t, strlen, strncmp, strstr};

use crate::{
    buf::{
        libxml_api::{
            xml_buf_add, xml_buf_create, xml_buf_detach, xml_buf_free,
            xml_buf_set_allocation_scheme,
        },
        xml_buf_overflow_error, XmlBufRef,
    },
    encoding::{detect_encoding, find_encoding_handler, XmlCharEncoding, XmlCharEncodingHandler},
    error::{
        __xml_raise_error, parser_validity_error, parser_validity_warning, XmlError,
        XmlParserErrors,
    },
    generic_error,
    globals::{
        get_do_validity_checking_default_value, get_get_warnings_default_value,
        get_keep_blanks_default_value, get_line_numbers_default_value,
        get_load_ext_dtd_default_value, get_parser_debug_entities,
        get_pedantic_parser_default_value, get_substitute_entities_default_value,
        set_indent_tree_output, set_keep_blanks_default_value, set_line_numbers_default_value,
        set_pedantic_parser_default_value, set_substitute_entities_default_value, GenericError,
        GenericErrorContext, StructuredError,
    },
    hash::XmlHashTableRef,
    io::{
        cleanup_input_callbacks, cleanup_output_callbacks, register_default_input_callbacks,
        register_default_output_callbacks, xml_default_external_entity_loader, xml_no_net_exists,
        xml_parser_get_directory, XmlParserInputBuffer,
    },
    libxml::{
        catalog::xml_catalog_cleanup,
        dict::{
            __xml_initialize_dict, xml_cleanup_dict_internal, xml_dict_create, xml_dict_free,
            xml_dict_lookup, xml_dict_reference, xml_dict_set_limit, XmlDictPtr,
        },
        entities::{xml_get_predefined_entity, XmlEntityPtr, XmlEntityType},
        globals::{
            xml_cleanup_globals_internal, xml_default_sax_handler, xml_default_sax_locator,
            xml_free, xml_init_globals_internal, xml_malloc, xml_malloc_atomic, xml_realloc,
        },
        htmlparser::{__html_parse_content, html_create_memory_parser_ctxt, HtmlParserOption},
        parser_internals::{
            __xml_err_encoding, xml_add_def_attrs, xml_add_special_attr, xml_check_language_id,
            xml_copy_char, xml_create_entity_parser_ctxt_internal, xml_create_file_parser_ctxt,
            xml_create_memory_parser_ctxt, xml_create_url_parser_ctxt, xml_err_encoding_int,
            xml_err_internal, xml_fatal_err, xml_free_input_stream, xml_new_entity_input_stream,
            xml_new_input_stream, xml_parse_attribute_type, xml_parse_comment, xml_parse_content,
            xml_parse_content_internal, xml_parse_default_decl, xml_parse_doc_type_decl,
            xml_parse_element_content_decl, xml_parse_element_end, xml_parse_element_start,
            xml_parse_encoding_decl, xml_parse_entity_ref, xml_parse_entity_value,
            xml_parse_external_subset, xml_parse_misc, xml_parse_name, xml_parse_nmtoken,
            xml_parse_pe_reference, xml_parse_pi, xml_parse_reference, xml_parse_sddecl,
            xml_parse_start_tag, xml_parse_system_literal, xml_parse_version_info, xml_push_input,
            xml_switch_encoding, INPUT_CHUNK, XML_MAX_DICTIONARY_LIMIT, XML_MAX_HUGE_LENGTH,
            XML_MAX_NAMELEN, XML_MAX_NAME_LENGTH, XML_MAX_TEXT_LENGTH, XML_SUBSTITUTE_PEREF,
            XML_SUBSTITUTE_REF,
        },
        relaxng::xml_relaxng_cleanup_types,
        sax2::{
            xml_sax2_entity_decl, xml_sax2_get_entity, xml_sax2_ignorable_whitespace,
            xml_sax_version,
        },
        uri::{xml_canonic_path, xml_free_uri, xml_parse_uri, XmlURIPtr},
        valid::{
            xml_free_doc_element_content, xml_free_enumeration, xml_is_mixed_element,
            xml_new_doc_element_content, xml_validate_root, XmlValidCtxt,
        },
        xmlmemory::{xml_cleanup_memory_internal, xml_init_memory_internal},
        xmlschemastypes::xml_schema_cleanup_types,
        xmlstring::{xml_str_equal, xml_strchr, xml_strlen, xml_strndup, XmlChar},
    },
    tree::{
        xml_buf_use, xml_build_qname, xml_free_doc, xml_free_node, xml_free_node_list, xml_new_doc,
        xml_new_doc_comment, xml_new_doc_node, xml_new_dtd, NodeCommon, NodePtr, XmlAttrPtr,
        XmlAttributeDefault, XmlAttributeType, XmlBufferAllocationScheme, XmlDocProperties,
        XmlDocPtr, XmlDtdPtr, XmlElementContentOccur, XmlElementContentPtr, XmlElementContentType,
        XmlElementType, XmlElementTypeVal, XmlEnumerationPtr, XmlNode, XmlNodePtr, XmlNsPtr,
        XML_XML_NAMESPACE,
    },
    uri::canonic_path,
    xpath::xml_init_xpath_internal,
};

use super::{
    catalog::XmlCatalogEntry,
    chvalid::{
        xml_is_blank_char, xml_is_char, xml_is_combining, xml_is_digit, xml_is_extender,
        xml_is_pubid_char,
    },
    entities::{
        XML_ENT_CHECKED, XML_ENT_CHECKED_LT, XML_ENT_CONTAINS_LT, XML_ENT_EXPANDING, XML_ENT_PARSED,
    },
    parser_internals::{
        xml_err_memory, xml_is_letter, LINE_LEN, XML_MAX_LOOKUP_LIMIT, XML_PARSER_MAX_DEPTH,
        XML_VCTXT_USE_PCTXT,
    },
    sax2::{xml_sax2_end_element, xml_sax2_start_element},
    threads::{
        __xml_global_init_mutex_lock, __xml_global_init_mutex_unlock, xml_cleanup_threads_internal,
        xml_init_threads_internal,
    },
};

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlFatalErrMsgInt")]
macro_rules! xml_fatal_err_msg_int {
    ($ctxt:expr, $error:expr, $msg:expr, $val:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                $error,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                None,
                None,
                None,
                $val,
                0,
                $msg,
            );
            if !ctxt.is_null() {
                (*ctxt).well_formed = 0;
                if (*ctxt).recovery == 0 {
                    (*ctxt).disable_sax = 1;
                }
            }
        }
    };
}
pub(crate) use xml_fatal_err_msg_int;

/// The default version of XML used: 1.0
pub(crate) const XML_DEFAULT_VERSION: &str = "1.0";

/// Callback for freeing some parser input allocations.
#[doc(alias = "xmlParserInputDeallocate")]
pub type XmlParserInputDeallocate = unsafe extern "C" fn(*mut XmlChar) -> c_void;

pub type XmlParserInputPtr = *mut XmlParserInput;
/// An xmlParserInput is an input flow for the XML processor.
/// Each entity parsed is associated an xmlParserInput (except the
/// few predefined ones). This is the case both for internal entities
/// - in which case the flow is already completely in memory  
/// - or external entities  
/// - in which case we use the buf structure for
///   progressive reading and I18N conversions to the internal UTF-8 format.
#[doc(alias = "xmlParserInput")]
#[repr(C)]
pub struct XmlParserInput {
    /* Input buffer */
    pub buf: Option<Rc<RefCell<XmlParserInputBuffer>>>, /* UTF-8 encoded buffer */
    pub filename: Option<String>,                       /* The file analyzed, if any */
    pub(crate) directory: Option<String>,               /* the directory/base of the file */
    pub base: *const XmlChar,                           /* Base of the array to parse */
    pub cur: *const XmlChar,                            /* Current c_char being parsed */
    pub end: *const XmlChar,                            /* end of the array to parse */
    pub(crate) length: i32,                             /* length if known */
    pub line: i32,                                      /* Current line */
    pub(crate) col: i32,                                /* Current column */
    pub consumed: u64,                                  /* How many xmlChars already consumed */
    pub(crate) free: Option<XmlParserInputDeallocate>,  /* function to deallocate the base */
    pub(crate) encoding: Option<String>,                /* the encoding string for entity */
    pub(crate) version: Option<String>,                 /* the version string for entity */
    pub(crate) standalone: i32,                         /* Was that entity marked standalone */
    pub(crate) id: i32,                                 /* an unique identifier for the entity */
    pub(crate) parent_consumed: u64,                    /* consumed bytes from parents */
    pub(crate) entity: XmlEntityPtr,                    /* entity, if any */
}

impl XmlParserInput {
    /// Update the input to use the current set of pointers from the buffer.
    ///
    /// Returns `-1` in case of error, `0` otherwise
    #[doc(alias = "xmlBufResetInput")]
    pub(crate) fn reset_base(&mut self) -> i32 {
        let Some(mut buffer) = self
            .buf
            .as_ref()
            .and_then(|buf| buf.borrow().buffer)
            .filter(|buf| buf.is_ok())
        else {
            return -1;
        };
        self.base = buffer.as_mut_ptr();
        self.cur = buffer.as_mut_ptr();
        unsafe {
            self.end = buffer.as_mut_ptr().add(buffer.len());
        }
        0
    }

    /// Returns the distance between the base and the top of the buffer.
    #[doc(alias = "xmlBufGetInputBase")]
    pub(crate) fn get_base(&self) -> usize {
        let Some(mut buffer) = self
            .buf
            .as_ref()
            .and_then(|buf| buf.borrow().buffer)
            .filter(|buf| buf.is_ok())
        else {
            return 0;
        };
        unsafe {
            let mut base = self.base.offset_from(buffer.as_mut_ptr()) as usize;
            if base > buffer.capacity() {
                xml_buf_overflow_error(buffer.deref_mut(), "Input reference outside of the buffer");
                base = 0;
            }
            base
        }
    }

    /// Update the input to use the base and cur relative to the buffer
    /// after a possible reallocation of its content
    ///
    /// Returns -1 in case of error, 0 otherwise
    #[doc(alias = "xmlBufSetInputBaseCur")]
    pub(crate) fn set_base_and_cursor(&mut self, base: usize, cur: usize) -> i32 {
        let Some(mut buffer) = self
            .buf
            .as_ref()
            .and_then(|buf| buf.borrow().buffer)
            .filter(|buf| buf.is_ok())
        else {
            self.base = c"".as_ptr() as _;
            self.cur = self.base;
            self.end = self.base;
            return -1;
        };

        unsafe {
            self.base = buffer.as_mut_ptr().add(base);
            self.cur = self.base.add(cur);
            self.end = buffer.as_mut_ptr().add(buffer.len());
        }
        0
    }
}

impl Default for XmlParserInput {
    fn default() -> Self {
        Self {
            buf: None,
            filename: None,
            directory: None,
            base: null(),
            cur: null(),
            end: null(),
            length: 0,
            line: 0,
            col: 0,
            consumed: 0,
            free: None,
            encoding: None,
            version: None,
            standalone: 0,
            id: 0,
            parent_consumed: 0,
            entity: null_mut(),
        }
    }
}

/// The parser can be asked to collect Node information, i.e. at what
/// place in the file they were detected.
/// NOTE: This is off by default and not very well tested.
#[doc(alias = "xmlParserNodeInfo")]
pub type XmlParserNodeInfoPtr = *mut XmlParserNodeInfo;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct XmlParserNodeInfo {
    pub(crate) node: *const XmlNode,
    /* Position & line # that text that created the node begins & ends on */
    pub(crate) begin_pos: u64,
    pub(crate) begin_line: u64,
    pub(crate) end_pos: u64,
    pub(crate) end_line: u64,
}

pub type XmlParserNodeInfoSeqPtr = *mut XmlParserNodeInfoSeq;
#[repr(C)]
pub struct XmlParserNodeInfoSeq {
    maximum: u64,
    length: u64,
    buffer: *mut XmlParserNodeInfo,
}

impl Default for XmlParserNodeInfoSeq {
    fn default() -> Self {
        Self {
            maximum: 0,
            length: 0,
            buffer: null_mut(),
        }
    }
}

/// The parser is now working also as a state based parser.
/// The recursive one use the state info for entities processing.
#[doc(alias = "xmlParserInputState")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlParserInputState {
    XmlParserEOF = -1, /* nothing is to be parsed */
    #[default]
    XmlParserStart = 0, /* nothing has been parsed */
    XmlParserMisc,     /* Misc* before int subset */
    XmlParserPI,       /* Within a processing instruction */
    XmlParserDTD,      /* within some DTD content */
    XmlParserProlog,   /* Misc* after internal subset */
    XmlParserComment,  /* within a comment */
    XmlParserStartTag, /* within a start tag */
    XmlParserContent,  /* within the content */
    XmlParserCDATASection, /* within a CDATA section */
    XmlParserEndTag,   /* within a closing tag */
    XmlParserEntityDecl, /* within an entity declaration */
    XmlParserEntityValue, /* within an entity value in a decl */
    XmlParserAttributeValue, /* within an attribute value */
    XmlParserSystemLiteral, /* within a SYSTEM value */
    XmlParserEpilog,   /* the Misc* after the last end tag */
    XmlParserIgnore,   /* within an IGNORED section */
    XmlParserPublicLiteral, /* within a PUBLIC value */
}

impl TryFrom<i32> for XmlParserInputState {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XmlParserEOF as i32 {
            Ok(Self::XmlParserEOF)
        } else if value == Self::XmlParserStart as i32 {
            Ok(Self::XmlParserStart)
        } else if value == Self::XmlParserMisc as i32 {
            Ok(Self::XmlParserMisc)
        } else if value == Self::XmlParserPI as i32 {
            Ok(Self::XmlParserPI)
        } else if value == Self::XmlParserDTD as i32 {
            Ok(Self::XmlParserDTD)
        } else if value == Self::XmlParserProlog as i32 {
            Ok(Self::XmlParserProlog)
        } else if value == Self::XmlParserComment as i32 {
            Ok(Self::XmlParserComment)
        } else if value == Self::XmlParserStartTag as i32 {
            Ok(Self::XmlParserStartTag)
        } else if value == Self::XmlParserContent as i32 {
            Ok(Self::XmlParserContent)
        } else if value == Self::XmlParserCDATASection as i32 {
            Ok(Self::XmlParserCDATASection)
        } else if value == Self::XmlParserEndTag as i32 {
            Ok(Self::XmlParserEndTag)
        } else if value == Self::XmlParserEntityDecl as i32 {
            Ok(Self::XmlParserEntityDecl)
        } else if value == Self::XmlParserEntityValue as i32 {
            Ok(Self::XmlParserEntityValue)
        } else if value == Self::XmlParserAttributeValue as i32 {
            Ok(Self::XmlParserAttributeValue)
        } else if value == Self::XmlParserSystemLiteral as i32 {
            Ok(Self::XmlParserSystemLiteral)
        } else if value == Self::XmlParserEpilog as i32 {
            Ok(Self::XmlParserEpilog)
        } else if value == Self::XmlParserIgnore as i32 {
            Ok(Self::XmlParserIgnore)
        } else if value == Self::XmlParserPublicLiteral as i32 {
            Ok(Self::XmlParserPublicLiteral)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

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

/// A parser can operate in various modes
#[doc(alias = "xmlParserMode")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlParserMode {
    #[default]
    XmlParseUnknown = 0,
    XmlParseDOM = 1,
    XmlParseSAX = 2,
    XmlParsePushDOM = 3,
    XmlParsePushSAX = 4,
    XmlParseReader = 5,
}

#[repr(C)]
#[derive(Clone)]
pub(crate) struct XmlStartTag {
    pub(crate) prefix: *const XmlChar,
    pub(crate) uri: *const XmlChar,
    pub(crate) line: i32,
    pub(crate) ns_nr: i32,
}

impl Default for XmlStartTag {
    fn default() -> Self {
        Self {
            prefix: null(),
            uri: null(),
            line: 0,
            ns_nr: 0,
        }
    }
}

/// The parser context.
///
/// # Note
/// This doesn't completely define the parser state, the (current ?)
/// design of the parser uses recursive function calls since this allow
/// and easy mapping from the production rules of the specification
/// to the actual code. The drawback is that the actual function call
/// also reflect the parser state. However most of the parsing routines
/// takes as the only argument the parser context pointer, so migrating
/// to a state based parser for progressive parsing shouldn't be too hard.
#[doc(alias = "xmlParserCtxt")]
pub type XmlParserCtxtPtr = *mut XmlParserCtxt;
#[repr(C)]
pub struct XmlParserCtxt {
    pub sax: *mut XmlSAXHandler,                       /* The SAX handler */
    pub(crate) user_data: Option<GenericErrorContext>, /* For SAX interface only, used by DOM build */
    pub my_doc: XmlDocPtr,                             /* the document being built */
    pub well_formed: i32,                              /* is the document well formed */
    pub(crate) replace_entities: i32,                  /* shall we replace entities ? */
    pub(crate) version: Option<String>,                /* the XML version string */
    pub encoding: Option<String>,                      /* the declared encoding, if any */
    pub(crate) standalone: i32,                        /* standalone document */
    pub(crate) html: i32,                              /* an HTML(1) document
                                                        * 3 is HTML after <head>
                                                        * 10 is HTML after <body>
                                                        */

    // Input stream stack
    pub input: XmlParserInputPtr,          /* Current input stream */
    pub input_tab: Vec<XmlParserInputPtr>, /* stack of inputs */

    // Node analysis stack only used for DOM building
    pub(crate) node: XmlNodePtr,          /* Current parsed Node */
    pub(crate) node_tab: Vec<XmlNodePtr>, /* array of nodes */

    pub(crate) record_info: i32, /* Whether node info should be kept */
    pub(crate) node_seq: XmlParserNodeInfoSeq, /* info about each node parsed */

    pub err_no: i32, /* error code */

    pub(crate) has_external_subset: i32, /* reference and external subset */
    pub(crate) has_perefs: i32,          /* the internal subset has PE refs */
    pub(crate) external: i32,            /* are we parsing an external entity */

    pub valid: i32,           /* is the document valid */
    pub(crate) validate: i32, /* shall we try to validate ? */
    pub vctxt: XmlValidCtxt,  /* The validity context */

    pub instate: XmlParserInputState, /* current type of input */
    pub(crate) token: i32,            /* next char look-ahead */

    pub(crate) directory: Option<String>, /* the data directory */

    // Node name stack
    pub(crate) name: *const XmlChar, /* Current parsed Node */
    pub(crate) name_tab: Vec<*const XmlChar>, /* array of nodes */

    nb_chars: i64,                             /* unused */
    pub(crate) check_index: i64,               /* used by progressive parsing lookup */
    pub(crate) keep_blanks: i32,               /* ugly but ... */
    pub(crate) disable_sax: i32,               /* SAX callbacks are disabled */
    pub in_subset: i32,                        /* Parsing is in int 1/ext 2 subset */
    pub(crate) int_sub_name: Option<String>,   /* name of subset */
    pub(crate) ext_sub_uri: Option<String>,    /* URI of external subset */
    pub(crate) ext_sub_system: Option<String>, /* SYSTEM ID of external subset */

    // xml:space values
    pub(crate) space_tab: Vec<i32>, /* array of space infos */

    pub(crate) depth: i32, /* to prevent entity substitution loops */
    pub(crate) entity: XmlParserInputPtr, /* used to check entities boundaries */
    pub charset: XmlCharEncoding, /* encoding of the in-memory content
                           actually an xmlCharEncoding */
    pub(crate) nodelen: i32,          /* Those two fields are there to */
    pub(crate) nodemem: i32,          /* Speed up large node parsing */
    pub(crate) pedantic: i32,         /* signal pedantic warnings */
    pub(crate) _private: *mut c_void, /* For user data, libxml won't touch it */

    pub(crate) loadsubset: i32,  /* should the external subset be loaded */
    pub(crate) linenumbers: i32, /* set line number in element content */
    #[cfg(feature = "catalog")]
    pub(crate) catalogs: Option<XmlCatalogEntry>, /* document's own catalog */
    pub(crate) recovery: i32,    /* run in recovery mode */
    pub(crate) progressive: i32, /* is this a progressive parsing */
    pub(crate) dict: XmlDictPtr, /* dictionary for the parser */
    pub(crate) atts: Vec<(String, Option<String>)>, /* array for the attributes callbacks */
    pub(crate) maxatts: i32,     /* the size of the array */
    pub(crate) docdict: i32,     /* use strings from dict to build tree */

    // pre-interned strings
    pub(crate) str_xml: *const XmlChar,
    pub(crate) str_xmlns: *const XmlChar,
    pub(crate) str_xml_ns: *const XmlChar,

    // Everything below is used only by the new SAX mode
    pub(crate) sax2: i32,                   /* operating in the new SAX mode */
    pub(crate) ns_tab: Vec<*const XmlChar>, /* the array of prefix/namespace name */
    pub(crate) attallocs: *mut i32,         /* which attribute were allocated */
    pub(crate) push_tab: Vec<XmlStartTag>,  /* array of data for push */
    pub(crate) atts_default: Option<XmlHashTableRef<'static, XmlDefAttrsPtr>>, /* defaulted attributes if any */
    pub(crate) atts_special: Option<XmlHashTableRef<'static, XmlAttributeType>>, /* non-CDATA attributes if any */
    pub(crate) ns_well_formed: i32, /* is the document XML Namespace okay */
    pub(crate) options: i32,        /* Extra options */

    // Those fields are needed only for streaming parsing so far
    pub(crate) dict_names: i32,    /* Use dictionary names for the tree */
    pub(crate) free_elems_nr: i32, /* number of freed element nodes */
    pub(crate) free_elems: XmlNodePtr, /* List of freed element nodes */
    pub(crate) free_attrs_nr: i32, /* number of freed attributes nodes */
    pub(crate) free_attrs: XmlAttrPtr, /* List of freed attributes nodes */

    // the complete error information for the last error.
    pub last_error: XmlError,
    pub(crate) parse_mode: XmlParserMode, /* the parser mode */
    pub(crate) nbentities: u64,           /* unused */
    pub sizeentities: u64,                /* size of parsed entities */

    // for use by HTML non-recursive parser
    pub(crate) node_info: *mut XmlParserNodeInfo, /* Current NodeInfo */
    pub(crate) node_info_nr: i32,                 /* Depth of the parsing stack */
    pub(crate) node_info_max: i32,                /* Max depth of the parsing stack */
    pub(crate) node_info_tab: *mut XmlParserNodeInfo, /* array of nodeInfos */

    pub(crate) input_id: i32, /* we need to label inputs */
    pub sizeentcopy: u64,     /* volume of entity copy */

    pub(crate) end_check_state: i32, /* quote state for push parser */
    pub nb_errors: u16,              /* number of errors */
    pub(crate) nb_warnings: u16,     /* number of warnings */
}

impl XmlParserCtxt {
    pub fn encoding(&self) -> Option<&str> {
        self.encoding.as_deref()
    }

    pub(crate) unsafe fn current_byte(&self) -> u8 {
        *(*self.input).cur
    }

    pub(crate) unsafe fn nth_byte(&self, nth: usize) -> u8 {
        let ptr = (*self.input).cur.add(nth);
        // debug_assert!(ptr < (*self.input).end);
        *ptr
    }

    pub(crate) unsafe fn current_ptr(&self) -> *const u8 {
        (*self.input).cur
    }

    pub(crate) unsafe fn base_ptr(&self) -> *const u8 {
        (*self.input).base
    }

    #[doc(alias = "xmlParserGrow")]
    pub(crate) unsafe fn force_grow(&mut self) -> i32 {
        let input: XmlParserInputPtr = self.input;
        let cur_end: ptrdiff_t = (*input).end.offset_from((*input).cur);
        let cur_base: ptrdiff_t = (*input).cur.offset_from((*input).base);

        let Some(buf) = (*input).buf.as_mut() else {
            return 0;
        };
        /* Don't grow push parser buffer. */
        if self.progressive != 0 {
            return 0;
        }
        /* Don't grow memory buffers. */
        if buf.borrow().encoder.is_none() && buf.borrow().context.is_none() {
            return 0;
        }

        if (cur_end > XML_MAX_LOOKUP_LIMIT as isize || cur_base > XML_MAX_LOOKUP_LIMIT as isize)
            && self.options & XmlParserOption::XmlParseHuge as i32 == 0
        {
            xml_err_internal!(self, "Huge input lookup");
            self.halt();
            return -1;
        }

        if cur_end >= INPUT_CHUNK as isize {
            return 0;
        }

        let ret: i32 = buf.borrow_mut().grow(INPUT_CHUNK as _);
        (*input).set_base_and_cursor(0, cur_base as usize);

        // TODO: Get error code from xmlParserInputBufferGrow
        if ret < 0 {
            xml_err_internal!(self, "Growing input buffer");
            self.halt();
        }

        ret
    }

    pub(crate) unsafe fn grow(&mut self) {
        if self.progressive == 0
            && ((*self.input).end.offset_from((*self.input).cur) as usize) < INPUT_CHUNK
        {
            self.force_grow();
        }
    }

    #[doc(alias = "xmlParserShrink")]
    pub(crate) unsafe fn force_shrink(&mut self) {
        let input: XmlParserInputPtr = self.input;
        let mut used: size_t;

        /* Don't shrink pull parser memory buffers. */
        let Some(buf) = (*input).buf.as_mut() else {
            return;
        };
        if self.progressive == 0
            && (*buf).borrow().encoder.is_none()
            && (*buf).borrow().context.is_none()
        {
            return;
        }

        used = (*input).cur.offset_from((*input).base) as usize;
        // Do not shrink on large buffers whose only a tiny fraction was consumed
        if used > INPUT_CHUNK {
            let res: size_t = buf
                .borrow()
                .buffer
                .map_or(0, |mut buf| buf.trim_head(used - LINE_LEN));

            if res > 0 {
                used -= res;
                if res > u64::MAX as size_t || (*input).consumed > u64::MAX - res as u64 {
                    (*input).consumed = u64::MAX;
                } else {
                    (*input).consumed += res as u64;
                }
            }
        }

        (*input).set_base_and_cursor(0, used);
    }

    pub(crate) unsafe fn shrink(&mut self) {
        if self.progressive == 0
            && (*self.input).cur.offset_from((*self.input).base) > 2 * INPUT_CHUNK as isize
            && (*self.input).end.offset_from((*self.input).cur) < 2 * INPUT_CHUNK as isize
        {
            self.force_shrink();
        }
    }

    /// Blocks further parser processing don't override error.
    #[doc(alias = "xmlHaltParser")]
    pub(crate) unsafe fn halt(&mut self) {
        self.instate = XmlParserInputState::XmlParserEOF;
        self.disable_sax = 1;
        while self.input_tab.len() > 1 {
            xml_free_input_stream(self.input_pop());
        }
        if !self.input.is_null() {
            // in case there was a specific allocation deallocate before overriding base
            if let Some(free) = (*self.input).free {
                free((*self.input).base as *mut XmlChar);
                (*self.input).free = None;
            }
            if (*self.input).buf.is_some() {
                let _ = (*self.input).buf.take();
            }
            (*self.input).cur = c"".as_ptr() as _;
            (*self.input).length = 0;
            (*self.input).base = (*self.input).cur;
            (*self.input).end = (*self.input).cur;
        }
    }

    /// Blocks further parser processing
    #[doc(alias = "xmlStopParser")]
    pub unsafe fn stop(&mut self) {
        self.halt();
        self.err_no = XmlParserErrors::XmlErrUserStop as i32;
    }

    pub(crate) unsafe fn advance(&mut self, nth: usize) {
        if (*self.input).cur.add(nth) > (*self.input).end {
            self.force_grow();
        }
        (*self.input).cur = (*self.input).cur.add(nth);
        (*self.input).col += nth as i32;
        assert!((*self.input).cur <= (*self.input).end);
    }

    /// Advance the current pointer.  
    /// If `'\n'` is found, line number is also increased.
    pub(crate) unsafe fn advance_with_line_handling(&mut self, nth: usize) {
        if (*self.input).cur.add(nth) > (*self.input).end {
            self.force_grow();
        }
        for _ in 0..nth {
            if *(*self.input).cur == b'\n' {
                (*self.input).line += 1;
                (*self.input).col = 1;
            } else {
                (*self.input).col += 1;
            }
            (*self.input).cur = (*self.input).cur.add(1);
        }
    }

    pub(crate) unsafe fn content_bytes(&self) -> &[u8] {
        let len = (*self.input).end.offset_from((*self.input).cur);
        assert!(len >= 0);
        from_raw_parts((*self.input).cur, len as usize)
    }

    /// Skip to the next character.
    #[doc(alias = "xmlNextChar")]
    pub(crate) unsafe fn skip_char(&mut self) {
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) || self.input.is_null() {
            return;
        }

        if (*self.input).cur > (*self.input).end {
            xml_err_internal!(self, "Parser input data memory error\n");

            self.err_no = XmlParserErrors::XmlErrInternalError as i32;
            self.stop();

            return;
        }

        if (*self.input).end.offset_from((*self.input).cur) < INPUT_CHUNK as isize {
            if self.force_grow() < 0 {
                return;
            }
            if (*self.input).cur >= (*self.input).end {
                return;
            }
        }

        if self.charset != XmlCharEncoding::UTF8 {
            // Assume it's a fixed length encoding (1) with
            // a compatible encoding for the ASCII set, since
            // XML constructs only use < 128 chars

            if *((*self.input).cur) == b'\n' {
                (*self.input).line += 1;
                (*self.input).col = 1;
            } else {
                (*self.input).col += 1;
            }
            (*self.input).cur = (*self.input).cur.add(1);
            return;
        }

        // 2.11 End-of-Line Handling
        //   the literal two-character sequence "#xD#xA" or a standalone
        //   literal #xD, an XML processor must pass to the application
        //   the single character #xA.
        if *(*self.input).cur == b'\n' {
            (*self.input).line += 1;
            (*self.input).col = 1;
        } else {
            (*self.input).col += 1;
        }

        let bytes = self.content_bytes();
        let len = 4.min(bytes.len());
        match from_utf8(&bytes[..len]) {
            Ok(s) => {
                let c = s.chars().next().unwrap();
                (*self.input).cur = (*self.input).cur.add(c.len_utf8());
                return;
            }
            Err(e) if e.valid_up_to() > 0 => {
                let s = from_utf8_unchecked(&bytes[..e.valid_up_to()]);
                let c = s.chars().next().unwrap();
                (*self.input).cur = (*self.input).cur.add(c.len_utf8());
                return;
            }
            Err(_) => {}
        }

        // If we detect an UTF8 error that probably mean that the
        // input encoding didn't get properly advertised in the declaration header.
        // Report the error and switch the encoding
        // to ISO-Latin-1 (if you don't like this policy, just declare the encoding !)
        if self.input.is_null() || (*self.input).end.offset_from((*self.input).cur) < 4 {
            __xml_err_encoding!(
                self,
                XmlParserErrors::XmlErrInvalidChar,
                "Input is not proper UTF-8, indicate encoding !\n"
            );
        } else {
            let buffer = format!(
                "Bytes: 0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}\n",
                *(*self.input).cur.add(0),
                *(*self.input).cur.add(1),
                *(*self.input).cur.add(2),
                *(*self.input).cur.add(3),
            );
            __xml_err_encoding!(
                self,
                XmlParserErrors::XmlErrInvalidChar,
                "Input is not proper UTF-8, indicate encoding !\n{}",
                buffer
            );
        }
        self.charset = XmlCharEncoding::ISO8859_1;
        (*self.input).cur = (*self.input).cur.add(1);
    }

    /// skip all blanks character found at that point in the input streams.  
    /// It pops up finished entities in the process if allowable at that point.
    ///
    /// Returns the number of space chars skipped
    #[doc(alias = "xmlSkipBlankChars")]
    pub(crate) unsafe fn skip_blanks(&mut self) -> i32 {
        let mut res = 0i32;

        // It's Okay to use CUR/NEXT here since all the blanks are on the ASCII range.
        if (self.input_tab.len() == 1 && !matches!(self.instate, XmlParserInputState::XmlParserDTD))
            || matches!(self.instate, XmlParserInputState::XmlParserStart)
        {
            // if we are in the document content, go really fast
            let mut cur = (*self.input).cur;
            while xml_is_blank_char(*cur as u32) {
                if *cur == b'\n' {
                    (*self.input).line += 1;
                    (*self.input).col = 1;
                } else {
                    (*self.input).col += 1;
                }
                cur = cur.add(1);
                res = res.saturating_add(1);
                if *cur == 0 {
                    (*self.input).cur = cur;
                    self.force_grow();
                    cur = (*self.input).cur;
                }
            }
            (*self.input).cur = cur;
        } else {
            let expand_pe = self.external != 0 || self.input_tab.len() != 1;

            while !matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                if xml_is_blank_char(self.current_byte() as u32) {
                    // CHECKED tstblanks.xml
                    self.skip_char();
                } else if self.current_byte() == b'%' {
                    // Need to handle support of entities branching here
                    if !expand_pe
                        || xml_is_blank_char(self.nth_byte(1) as u32)
                        || self.nth_byte(1) == 0
                    {
                        break;
                    }
                    xml_parse_pe_reference(self);
                } else if self.current_byte() == 0 {
                    let mut consumed: u64;

                    if self.input_tab.len() <= 1 {
                        break;
                    }

                    consumed = (*self.input).consumed;
                    consumed = consumed
                        .saturating_add((*self.input).cur.offset_from((*self.input).base) as _);

                    // Add to sizeentities when parsing an external entity for the first time.
                    let ent: XmlEntityPtr = (*self.input).entity;
                    if matches!((*ent).etype, XmlEntityType::XmlExternalParameterEntity)
                        && (*ent).flags & XML_ENT_PARSED as i32 == 0
                    {
                        (*ent).flags |= XML_ENT_PARSED as i32;

                        self.sizeentities = self.sizeentities.saturating_add(consumed);
                    }

                    xml_parser_entity_check(self, consumed);

                    xml_pop_input(self);
                } else {
                    break;
                }

                // Also increase the counter when entering or exiting a PERef.
                // The spec says: "When a parameter-entity reference is recognized
                // in the DTD and included, its replacement text MUST be enlarged
                // by the attachment of one leading and one following space (#x20) character."
                res = res.saturating_add(1);
            }
        }
        res
    }

    /// The current c_char value, if using UTF-8 this may actually span multiple bytes
    /// in the input buffer.  
    ///
    /// Implement the end of line normalization:  
    ///
    /// 2.11 End-of-Line Handling  
    /// Wherever an external parsed entity or the literal entity value
    /// of an internal parsed entity contains either the literal two-character
    /// sequence "#xD#xA" or a standalone literal #xD, an XML processor
    /// must pass to the application the single character #xA.  
    /// This behavior can conveniently be produced by normalizing all
    /// line breaks to #xA on input, before parsing.
    ///
    /// Returns the current char value and its length
    #[doc(hidden)]
    #[doc(alias = "xmlCurrentChar")]
    pub unsafe fn current_char(&mut self, len: &mut i32) -> Option<char> {
        if self.input.is_null() {
            return None;
        }
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }

        if (*self.input).end.offset_from((*self.input).cur) < INPUT_CHUNK as isize
            && self.force_grow() < 0
        {
            return None;
        }

        if *(*self.input).cur >= 0x20 && *(*self.input).cur <= 0x7F {
            *len = 1;
            return Some(*(*self.input).cur as char);
        }

        if self.charset != XmlCharEncoding::UTF8 {
            // Assume it's a fixed length encoding (1) with
            // a compatible encoding for the ASCII set, since
            // XML constructs only use < 128 chars
            *len = 1;
            if *(*self.input).cur == 0xD {
                if *(*self.input).cur.add(1) == 0xA {
                    (*self.input).cur = (*self.input).cur.add(1);
                }
                return Some('\u{A}');
            }
            return Some(*(*self.input).cur as char);
        }
        let content = self.content_bytes();
        let l = 4.min(content.len());
        let c = match from_utf8(&content[..l]) {
            Ok(s) => {
                let Some(c) = s.chars().next() else {
                    *len = 0;
                    return None;
                };
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
                        if (*self.input).end.offset_from((*self.input).cur) < 4 {
                            __xml_err_encoding!(
                                self,
                                XmlParserErrors::XmlErrInvalidChar,
                                "Input is not proper UTF-8, indicate encoding !\n"
                            );
                        } else {
                            let buffer = format!(
                                "Bytes: 0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}\n",
                                content[0], content[1], content[2], content[3],
                            );
                            __xml_err_encoding!(
                                self,
                                XmlParserErrors::XmlErrInvalidChar,
                                "Input is not proper UTF-8, indicate encoding !\n{}",
                                buffer
                            );
                        }
                        self.charset = XmlCharEncoding::ISO8859_1;
                        *len = 1;
                        return Some(*(*self.input).cur as char);
                    }
                    None => {
                        *len = 0;
                        return Some('\0');
                    }
                }
            }
        };
        if (*len > 1 && !xml_is_char(c as u32))
            || (*len == 1 && c == '\0' && (*self.input).cur < (*self.input).end)
        {
            xml_err_encoding_int!(
                self,
                XmlParserErrors::XmlErrInvalidChar,
                "Char 0x{:X} out of allowed range\n",
                c as i32
            );
        }
        if c == '\r' {
            let next = (*self.input).cur.add(1);
            if next < (*self.input).end && *next == b'\n' {
                (*self.input).cur = (*self.input).cur.add(1);
            }
            return Some('\n');
        }
        Some(c)
    }

    /// Pushes a new parser input on top of the input stack
    ///
    /// Returns -1 in case of error, the index in the stack otherwise
    #[doc(alias = "inputPush")]
    pub fn input_push(&mut self, value: XmlParserInputPtr) -> i32 {
        if value.is_null() {
            return -1;
        }
        self.input = value;
        self.input_tab.push(value);
        self.input_tab.len() as i32 - 1
    }

    /// Pops the top parser input from the input stack
    ///
    /// Returns the input just removed
    #[doc(alias = "inputPop")]
    pub fn input_pop(&mut self) -> XmlParserInputPtr {
        if self.input_tab.is_empty() {
            return null_mut();
        }
        let res = self.input_tab.pop().unwrap_or(null_mut());
        self.input = *self.input_tab.last().unwrap_or(&null_mut());
        res
    }

    /// Pushes a new element node on top of the node stack
    ///
    /// Returns -1 in case of error, the index in the stack otherwise
    #[doc(alias = "nodePush")]
    pub(crate) unsafe fn node_push(&mut self, value: XmlNodePtr) -> i32 {
        if self.node_tab.len() as u32 > XML_PARSER_MAX_DEPTH
            && self.options & XmlParserOption::XmlParseHuge as i32 == 0
        {
            xml_fatal_err_msg_int!(
                self,
                XmlParserErrors::XmlErrInternalError,
                format!("Excessive depth in document: {XML_PARSER_MAX_DEPTH} use XML_PARSE_HUGE option\n").as_str(),
                XML_PARSER_MAX_DEPTH as i32
            );
            self.halt();
            return -1;
        }
        self.node = value;
        self.node_tab.push(value);
        self.node_tab.len() as i32 - 1
    }

    /// Pops the top element node from the node stack
    ///
    /// Returns the node just removed
    #[doc(alias = "nodePop")]
    pub(crate) fn node_pop(&mut self) -> XmlNodePtr {
        let res = self.node_tab.pop().unwrap_or(null_mut());
        self.node = *self.node_tab.last().unwrap_or(&null_mut());
        res
    }

    /// Pushes a new element name on top of the name stack
    ///
    /// Returns -1 in case of error, the index in the stack otherwise
    #[doc(alias = "namePush")]
    pub(crate) fn name_push(&mut self, value: *const XmlChar) -> i32 {
        self.name = value;
        self.name_tab.push(value);
        self.name_tab.len() as i32 - 1
    }

    /// Pops the top element name from the name stack
    ///
    /// Returns the name just removed
    #[doc(alias = "namePop")]
    pub(crate) fn name_pop(&mut self) -> *const XmlChar {
        let res = self.name_tab.pop().unwrap_or(null_mut());
        self.name = *self.name_tab.last().unwrap_or(&null());
        res
    }

    #[doc(alias = "spacePush")]
    pub(crate) fn space_push(&mut self, val: i32) -> i32 {
        self.space_tab.push(val);
        self.space_tab.len() as i32 - 1
    }

    #[doc(alias = "spacePop")]
    pub(crate) fn space_pop(&mut self) -> i32 {
        self.space_tab.pop().unwrap_or(-1)
    }

    pub(crate) fn space(&self) -> i32 {
        *self.space_tab.last().unwrap_or(&-1)
    }

    pub(crate) fn space_mut(&mut self) -> &mut i32 {
        self.space_tab.last_mut().expect("Internal Error")
    }

    /// Pushes a new element name/prefix/URL on top of the name stack
    ///
    /// Returns -1 in case of error, the index in the stack otherwise
    #[doc(alias = "nameNsPush")]
    pub(crate) fn name_ns_push(
        &mut self,
        value: *const XmlChar,
        prefix: *const XmlChar,
        uri: *const XmlChar,
        line: i32,
        ns_nr: i32,
    ) -> i32 {
        self.name = value;
        self.name_tab.push(value);
        self.push_tab
            .resize(self.name_tab.len(), XmlStartTag::default());
        let res = self.name_tab.len() - 1;
        self.push_tab[res].prefix = prefix;
        self.push_tab[res].uri = uri;
        self.push_tab[res].line = line;
        self.push_tab[res].ns_nr = ns_nr;
        res as i32
    }

    /// Pops the top element/prefix/URI name from the name stack
    ///
    /// Returns the name just removed
    #[doc(alias = "nameNsPop")]
    #[cfg(feature = "libxml_push")]
    pub(crate) fn name_ns_pop(&mut self) -> *const XmlChar {
        let res = self.name_tab.pop().unwrap_or(null_mut());
        self.name = *self.name_tab.last().unwrap_or(&null());
        res
    }

    /// Pushes a new parser namespace on top of the ns stack
    ///
    /// Returns -1 in case of error, -2 if the namespace should be discarded and the index in the stack otherwise.
    #[doc(alias = "nsPush")]
    pub(crate) fn ns_push(&mut self, prefix: *const XmlChar, url: *const XmlChar) -> i32 {
        if self.options & XmlParserOption::XmlParseNsclean as i32 != 0 {
            for i in (0..self.ns_tab.len() - 1).rev().step_by(2) {
                if self.ns_tab[i] == prefix {
                    /* in scope */
                    if self.ns_tab[i + 1] == url {
                        return -2;
                    }
                    /* out of scope keep it */
                    break;
                }
            }
        }
        self.ns_tab.push(prefix);
        self.ns_tab.push(url);
        self.ns_tab.len() as i32
    }

    /// Pops the top @nr parser prefix/namespace from the ns stack
    ///
    /// Returns the number of namespaces removed
    #[doc(alias = "nsPop")]
    pub(crate) fn ns_pop(&mut self, mut nr: usize) -> usize {
        if self.ns_tab.len() < nr {
            generic_error!("Pbm popping {} NS\n", nr);
            nr = self.ns_tab.len();
        }
        if self.ns_tab.is_empty() {
            return 0;
        }
        let rem = self.ns_tab.len() - nr;
        self.ns_tab.truncate(rem);
        nr
    }

    /// Do the SAX2 detection and specific initialization
    #[doc(alias = "xmlDetectSAX2")]
    pub(crate) unsafe fn detect_sax2(&mut self) {
        let sax: XmlSAXHandlerPtr = self.sax;
        #[cfg(feature = "sax1")]
        {
            if !sax.is_null()
                && (*sax).initialized == XML_SAX2_MAGIC as u32
                && ((*sax).start_element_ns.is_some()
                    || (*sax).end_element_ns.is_some()
                    || ((*sax).start_element.is_none() && (*sax).end_element.is_none()))
            {
                self.sax2 = 1;
            }
        }
        #[cfg(not(feature = "sax1"))]
        {
            self.sax2 = 1;
        }

        self.str_xml = xml_dict_lookup(self.dict, c"xml".as_ptr() as _, 3);
        self.str_xmlns = xml_dict_lookup(self.dict, c"xmlns".as_ptr() as _, 5);
        self.str_xml_ns = xml_dict_lookup(self.dict, XML_XML_NAMESPACE.as_ptr() as _, 36);
        if self.str_xml.is_null() || self.str_xmlns.is_null() || self.str_xml_ns.is_null() {
            xml_err_memory(self, None);
        }
    }

    /// Applies the options to the parser context
    ///
    /// Returns 0 in case of success, the set of unknown or unimplemented options in case of error.
    #[doc(alias = "xmlCtxtUseOptionsInternal")]
    pub(crate) unsafe fn ctxt_use_options_internal(
        &mut self,
        mut options: i32,
        encoding: Option<&str>,
    ) -> i32 {
        if let Some(encoding) = encoding {
            self.encoding = Some(encoding.to_owned());
        }
        if options & XmlParserOption::XmlParseRecover as i32 != 0 {
            self.recovery = 1;
            options -= XmlParserOption::XmlParseRecover as i32;
            self.options |= XmlParserOption::XmlParseRecover as i32;
        } else {
            self.recovery = 0;
        }
        if options & XmlParserOption::XmlParseDtdload as i32 != 0 {
            self.loadsubset = XML_DETECT_IDS as i32;
            options -= XmlParserOption::XmlParseDtdload as i32;
            self.options |= XmlParserOption::XmlParseDtdload as i32;
        } else {
            self.loadsubset = 0;
        }
        if options & XmlParserOption::XmlParseDtdattr as i32 != 0 {
            self.loadsubset |= XML_COMPLETE_ATTRS as i32;
            options -= XmlParserOption::XmlParseDtdattr as i32;
            self.options |= XmlParserOption::XmlParseDtdattr as i32;
        }
        if options & XmlParserOption::XmlParseNoent as i32 != 0 {
            self.replace_entities = 1;
            /* self.loadsubset |= XML_DETECT_IDS; */
            options -= XmlParserOption::XmlParseNoent as i32;
            self.options |= XmlParserOption::XmlParseNoent as i32;
        } else {
            self.replace_entities = 0;
        }
        if options & XmlParserOption::XmlParsePedantic as i32 != 0 {
            self.pedantic = 1;
            options -= XmlParserOption::XmlParsePedantic as i32;
            self.options |= XmlParserOption::XmlParsePedantic as i32;
        } else {
            self.pedantic = 0;
        }
        if options & XmlParserOption::XmlParseNoblanks as i32 != 0 {
            self.keep_blanks = 0;
            (*self.sax).ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
            options -= XmlParserOption::XmlParseNoblanks as i32;
            self.options |= XmlParserOption::XmlParseNoblanks as i32;
        } else {
            self.keep_blanks = 1;
        }
        if options & XmlParserOption::XmlParseDtdvalid as i32 != 0 {
            self.validate = 1;
            if options & XmlParserOption::XmlParseNowarning as i32 != 0 {
                self.vctxt.warning = None;
            }
            if options & XmlParserOption::XmlParseNoerror as i32 != 0 {
                self.vctxt.error = None;
            }
            options -= XmlParserOption::XmlParseDtdvalid as i32;
            self.options |= XmlParserOption::XmlParseDtdvalid as i32;
        } else {
            self.validate = 0;
        }
        if options & XmlParserOption::XmlParseNowarning as i32 != 0 {
            (*self.sax).warning = None;
            options -= XmlParserOption::XmlParseNowarning as i32;
        }
        if options & XmlParserOption::XmlParseNoerror as i32 != 0 {
            (*self.sax).error = None;
            (*self.sax).fatal_error = None;
            options -= XmlParserOption::XmlParseNoerror as i32;
        }
        #[cfg(feature = "sax1")]
        if options & XmlParserOption::XmlParseSax1 as i32 != 0 {
            (*self.sax).start_element = Some(xml_sax2_start_element);
            (*self.sax).end_element = Some(xml_sax2_end_element);
            (*self.sax).start_element_ns = None;
            (*self.sax).end_element_ns = None;
            (*self.sax).initialized = 1;
            options -= XmlParserOption::XmlParseSax1 as i32;
            self.options |= XmlParserOption::XmlParseSax1 as i32;
        }
        if options & XmlParserOption::XmlParseNodict as i32 != 0 {
            self.dict_names = 0;
            options -= XmlParserOption::XmlParseNodict as i32;
            self.options |= XmlParserOption::XmlParseNodict as i32;
        } else {
            self.dict_names = 1;
        }
        if options & XmlParserOption::XmlParseNocdata as i32 != 0 {
            (*self.sax).cdata_block = None;
            options -= XmlParserOption::XmlParseNocdata as i32;
            self.options |= XmlParserOption::XmlParseNocdata as i32;
        }
        if options & XmlParserOption::XmlParseNsclean as i32 != 0 {
            self.options |= XmlParserOption::XmlParseNsclean as i32;
            options -= XmlParserOption::XmlParseNsclean as i32;
        }
        if options & XmlParserOption::XmlParseNonet as i32 != 0 {
            self.options |= XmlParserOption::XmlParseNonet as i32;
            options -= XmlParserOption::XmlParseNonet as i32;
        }
        if options & XmlParserOption::XmlParseCompact as i32 != 0 {
            self.options |= XmlParserOption::XmlParseCompact as i32;
            options -= XmlParserOption::XmlParseCompact as i32;
        }
        if options & XmlParserOption::XmlParseOld10 as i32 != 0 {
            self.options |= XmlParserOption::XmlParseOld10 as i32;
            options -= XmlParserOption::XmlParseOld10 as i32;
        }
        if options & XmlParserOption::XmlParseNobasefix as i32 != 0 {
            self.options |= XmlParserOption::XmlParseNobasefix as i32;
            options -= XmlParserOption::XmlParseNobasefix as i32;
        }
        if options & XmlParserOption::XmlParseHuge as i32 != 0 {
            self.options |= XmlParserOption::XmlParseHuge as i32;
            options -= XmlParserOption::XmlParseHuge as i32;
            if !self.dict.is_null() {
                xml_dict_set_limit(self.dict, 0);
            }
        }
        if options & XmlParserOption::XmlParseOldsax as i32 != 0 {
            self.options |= XmlParserOption::XmlParseOldsax as i32;
            options -= XmlParserOption::XmlParseOldsax as i32;
        }
        if options & XmlParserOption::XmlParseIgnoreEnc as i32 != 0 {
            self.options |= XmlParserOption::XmlParseIgnoreEnc as i32;
            options -= XmlParserOption::XmlParseIgnoreEnc as i32;
        }
        if options & XmlParserOption::XmlParseBigLines as i32 != 0 {
            self.options |= XmlParserOption::XmlParseBigLines as i32;
            options -= XmlParserOption::XmlParseBigLines as i32;
        }
        self.linenumbers = 1;
        options
    }

    /// Common front-end for the xmlRead functions
    ///
    /// Returns the resulting document tree or NULL
    #[doc(alias = "xmlDoRead")]
    unsafe fn do_read(
        &mut self,
        url: Option<&str>,
        encoding: Option<&str>,
        options: i32,
    ) -> XmlDocPtr {
        let ret: XmlDocPtr;

        self.ctxt_use_options_internal(options, encoding);
        if let Some(encoding) = encoding {
            // TODO: We should consider to set XML_PARSE_IGNORE_ENC if the
            // caller provided an encoding. Otherwise, we might match to
            // the encoding from the XML declaration which is likely to
            // break things. Also see xmlSwitchInputEncoding.
            if let Some(handler) = find_encoding_handler(encoding) {
                self.switch_to_encoding(handler);
            }
        }
        if url.is_some() && !self.input.is_null() && (*self.input).filename.is_none() {
            (*self.input).filename = url.map(|u| u.to_owned());
        }
        xml_parse_document(self);
        if self.well_formed != 0 || self.recovery != 0 {
            ret = self.my_doc;
        } else {
            ret = null_mut();
            if !self.my_doc.is_null() {
                xml_free_doc(self.my_doc);
            }
        }
        self.my_doc = null_mut();
        ret
    }

    /// change the input functions when discovering the character encoding of a given entity.
    ///
    /// Returns 0 in case of success, -1 otherwise
    #[doc(alias = "xmlSwitchInputEncoding")]
    pub(crate) unsafe fn switch_input_encoding(
        &mut self,
        input: XmlParserInputPtr,
        handler: XmlCharEncodingHandler,
    ) -> i32 {
        if input.is_null() {
            return -1;
        }
        let Some(input_buf) = (*input).buf.as_mut() else {
            xml_err_internal!(self, "static memory buffer doesn't support encoding\n");
            return -1;
        };

        if input_buf.borrow_mut().encoder.replace(handler).is_some() {
            // Switching encodings during parsing is a really bad idea,
            // but Chromium can match between ISO-8859-1 and UTF-16 before
            // separate calls to xmlParseChunk.
            //
            // TODO: We should check whether the "raw" input buffer is empty and
            // convert the old content using the old encoder.
            return 0;
        }

        self.charset = XmlCharEncoding::UTF8;

        // Is there already some content down the pipe to convert ?
        let Some(mut buf) = input_buf.borrow().buffer.filter(|buf| !buf.is_empty()) else {
            return 0;
        };
        // FIXME: The BOM shouldn't be skipped here, but in the parsing code.

        // Specific handling of the Byte Order Mark for UTF-16
        if matches!(
            (*input_buf).borrow().encoder.as_ref().unwrap().name(),
            "UTF-16LE" | "UTF-16"
        ) && *(*input).cur.add(0) == 0xFF
            && *(*input).cur.add(1) == 0xFE
        {
            (*input).cur = (*input).cur.add(2);
        }
        if (*input_buf).borrow().encoder.as_ref().unwrap().name() == "UTF-16BE"
            && *(*input).cur.add(0) == 0xFE
            && *(*input).cur.add(1) == 0xFF
        {
            (*input).cur = (*input).cur.add(2);
        }
        // Errata on XML-1.0 June 20 2001
        // Specific handling of the Byte Order Mark for UTF-8
        if (*input_buf).borrow().encoder.as_ref().unwrap().name() == "UTF-8"
            && *(*input).cur.add(0) == 0xEF
            && *(*input).cur.add(1) == 0xBB
            && *(*input).cur.add(2) == 0xBF
        {
            (*input).cur = (*input).cur.add(3);
        }

        // Shrink the current input buffer.
        // Move it as the raw buffer and create a new input buffer
        let processed: size_t = (*input).cur.offset_from((*input).base) as usize;
        buf.trim_head(processed as usize);
        (*input).consumed += processed as u64;
        input_buf.borrow_mut().raw = Some(buf);
        input_buf.borrow_mut().buffer = XmlBufRef::new();
        assert!(input_buf.borrow_mut().buffer.is_some());
        input_buf.borrow_mut().rawconsumed = processed as u64;
        let using: size_t = buf.len();

        // TODO: We must flush and decode the whole buffer to make functions
        // like xmlReadMemory work with a user-provided encoding. If the
        // encoding is specified directly, we should probably set
        // XML_PARSE_IGNORE_ENC in xmlDoRead to avoid switching encodings
        // twice. Then we could set "flush" to false which should save
        // a considerable amount of memory when parsing from memory.
        // It's probably even possible to remove this whole if-block
        // completely.
        let res = input_buf.borrow_mut().decode(true);
        (*input).reset_base();
        if res.is_err() {
            // TODO: This could be an out of memory or an encoding error.
            xml_err_internal!(self, "switching encoding: encoder error\n");
            self.halt();
            return -1;
        }
        let consumed: size_t = using - (*input_buf).borrow().raw.map_or(0, |raw| raw.len());
        let rawconsumed = (*input_buf)
            .borrow()
            .rawconsumed
            .saturating_add(consumed as u64);
        (*input_buf).borrow_mut().rawconsumed = rawconsumed;
        0
    }

    /// change the input functions when discovering the character encoding
    /// of a given entity.
    ///
    /// Returns 0 in case of success, -1 otherwise
    #[doc(alias = "xmlSwitchToEncoding")]
    pub unsafe fn switch_to_encoding(&mut self, handler: XmlCharEncodingHandler) -> i32 {
        self.switch_input_encoding(self.input, handler)
    }
}

impl Default for XmlParserCtxt {
    fn default() -> Self {
        Self {
            sax: null_mut(),
            user_data: None,
            my_doc: null_mut(),
            well_formed: 0,
            replace_entities: 0,
            version: None,
            encoding: None,
            standalone: 0,
            html: 0,
            input: null_mut(),
            input_tab: vec![],
            node: null_mut(),
            node_tab: vec![],
            record_info: 0,
            node_seq: XmlParserNodeInfoSeq::default(),
            err_no: 0,
            has_external_subset: 0,
            has_perefs: 0,
            external: 0,
            valid: 0,
            validate: 0,
            vctxt: XmlValidCtxt::default(),
            instate: XmlParserInputState::default(),
            token: 0,
            directory: None,
            name: null(),
            name_tab: vec![],
            nb_chars: 0,
            check_index: 0,
            keep_blanks: 0,
            disable_sax: 0,
            in_subset: 0,
            int_sub_name: None,
            ext_sub_uri: None,
            ext_sub_system: None,
            space_tab: vec![],
            depth: 0,
            entity: null_mut(),
            charset: XmlCharEncoding::None,
            nodelen: 0,
            nodemem: 0,
            pedantic: 0,
            _private: null_mut(),
            loadsubset: 0,
            linenumbers: 0,
            #[cfg(feature = "catalog")]
            catalogs: None,
            recovery: 0,
            progressive: 0,
            dict: null_mut(),
            atts: vec![],
            maxatts: 0,
            docdict: 0,
            str_xml: null(),
            str_xml_ns: null(),
            str_xmlns: null(),
            sax2: 0,
            ns_tab: vec![],
            attallocs: null_mut(),
            push_tab: vec![],
            atts_default: None,
            atts_special: None,
            ns_well_formed: 0,
            options: 0,
            dict_names: 0,
            free_elems_nr: 0,
            free_elems: null_mut(),
            free_attrs_nr: 0,
            free_attrs: null_mut(),
            last_error: XmlError::default(),
            parse_mode: XmlParserMode::default(),
            nbentities: 0,
            sizeentities: 0,
            node_info: null_mut(),
            node_info_nr: 0,
            node_info_max: 0,
            node_info_tab: null_mut(),
            input_id: 0,
            sizeentcopy: 0,
            end_check_state: 0,
            nb_errors: 0,
            nb_warnings: 0,
        }
    }
}

pub type XmlSAXLocatorPtr = *mut XmlSAXLocator;
/// A SAX Locator.
#[doc(alias = "xmlSAXLocator")]
#[repr(C)]
pub struct XmlSAXLocator {
    pub(crate) get_public_id: unsafe fn(ctx: *mut c_void) -> *const XmlChar,
    pub(crate) get_system_id: unsafe fn(ctx: *mut c_void) -> Option<String>,
    pub(crate) get_line_number: unsafe fn(ctx: *mut c_void) -> i32,
    pub(crate) get_column_number: unsafe fn(ctx: *mut c_void) -> i32,
}

/// Callback:
/// The entity loader, to control the loading of external entities,
/// the application can either:
///    - override this resolveEntity() callback in the SAX block
///    - or better use the xmlSetExternalEntityLoader() function to
///      set up it's own entity resolution routine
///
/// Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
#[doc(alias = "resolveEntitySAXFunc")]
pub type ResolveEntitySAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    public_id: Option<&str>,
    system_id: Option<&str>,
) -> XmlParserInputPtr;
/// Callback on internal subset declaration.
#[doc(alias = "internalSubsetSAXFunc")]
pub type InternalSubsetSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
);

/// Callback on external subset declaration.
#[doc(alias = "externalSubsetSAXFunc")]
pub type ExternalSubsetSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
);

/// Get an entity by name.
///
/// Returns the xmlEntityPtr if found.
#[doc(alias = "getEntitySAXFunc")]
pub type GetEntitySAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, name: &str) -> XmlEntityPtr;

/// Get a parameter entity by name.
///
/// Returns the xmlEntityPtr if found.
#[doc(alias = "getParameterEntitySAXFunc")]
pub type GetParameterEntitySAXFunc =
    unsafe fn(ctx: Option<GenericErrorContext>, name: &str) -> XmlEntityPtr;

/// An entity definition has been parsed.
#[doc(alias = "entityDeclSAXFunc")]
pub type EntityDeclSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    name: &str,
    typ: XmlEntityType,
    public_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
);

/// What to do when a notation declaration has been parsed.
#[doc(alias = "notationDeclSAXFunc")]
pub type NotationDeclSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
);

/// An attribute definition has been parsed.
#[doc(alias = "attributeDeclSAXFunc")]
pub type AttributeDeclSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    elem: &str,
    fullname: &str,
    typ: XmlAttributeType,
    def: XmlAttributeDefault,
    default_value: Option<&str>,
    tree: XmlEnumerationPtr,
);

/// An element definition has been parsed.
#[doc(alias = "elementDeclSAXFunc")]
pub type ElementDeclSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    name: &str,
    typ: Option<XmlElementTypeVal>,
    content: XmlElementContentPtr,
);

/// What to do when an unparsed entity declaration is parsed.
#[doc(alias = "unparsedEntityDeclSAXFunc")]
pub type UnparsedEntityDeclSAXFunc = unsafe fn(
    ctx: Option<GenericErrorContext>,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
    notation_name: Option<&str>,
);

/// Receive the document locator at startup, actually xmlDefaultSAXLocator.
/// Everything is available on the context, so this is useless in our case.
#[doc(alias = "setDocumentLocatorSAXFunc")]
pub type SetDocumentLocatorSAXFunc =
    unsafe fn(ctx: Option<GenericErrorContext>, loc: XmlSAXLocatorPtr);

/// Called when the document start being processed.
#[doc(alias = "startDocumentSAXFunc")]
pub type StartDocumentSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>);
/// Called when the document end has been detected.
#[doc(alias = "endDocumentSAXFunc")]
pub type EndDocumentSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>);
/// Called when an opening tag has been processed.
///
/// The elements of `atts` are `(attribute_name, attribute_value)`.
#[doc(alias = "startElementSAXFunc")]
pub type StartElementSAXFunc =
    unsafe fn(ctx: Option<GenericErrorContext>, name: &str, atts: &[(String, Option<String>)]);

/// Called when the end of an element has been detected.
#[doc(alias = "endElementSAXFunc")]
pub type EndElementSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, name: &str);

/// Handle an attribute that has been read by the parser.
/// The default handling is to convert the attribute into an
/// DOM subtree and past it in a new xmlAttr element added to
/// the element.
#[doc(alias = "attributeSAXFunc")]
pub type AttributeSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, name: &str, value: &str);

/// Called when an entity reference is detected.
#[doc(alias = "referenceSAXFunc")]
pub type ReferenceSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, name: &str);

/// Receiving some chars from the parser.
#[doc(alias = "charactersSAXFunc")]
pub type CharactersSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, ch: &str);

/// Receiving some ignorable whitespaces from the parser.
/// UNUSED: by default the DOM building will use characters.
#[doc(alias = "ignorableWhitespaceSAXFunc")]
pub type IgnorableWhitespaceSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, ch: &str);

/// A processing instruction has been parsed.
#[doc(alias = "processingInstructionSAXFunc")]
pub type ProcessingInstructionSAXFunc =
    unsafe fn(ctx: Option<GenericErrorContext>, target: &str, data: Option<&str>);

/// A comment has been parsed.
#[doc(alias = "commentSAXFunc")]
pub type CommentSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, value: &str);

/// Called when a pcdata block has been parsed.
#[doc(alias = "cdataBlockSAXFunc")]
pub type CDATABlockSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>, value: &str);

/// Display and format a warning messages, callback.
#[doc(alias = "warningSAXFunc")]
pub type WarningSAXFunc = unsafe fn(ctx: *mut c_void, msg: &str);

/// Display and format an error messages, callback.
#[doc(alias = "errorSAXFunc")]
pub type ErrorSAXFunc = unsafe fn(ctx: *mut c_void, msg: &str);

/// Display and format fatal error messages, callback.
/// # Note
/// so far fatalError() SAX callbacks are not used, error() get all the callbacks for errors.
#[doc(alias = "fatalErrorSAXFunc")]
pub type FatalErrorSAXFunc = unsafe fn(ctx: *mut c_void, msg: &str);

/// Is this document tagged standalone?
///
/// Returns 1 if true
#[doc(alias = "isStandaloneSAXFunc")]
pub type IsStandaloneSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>) -> i32;
/// Does this document has an internal subset.
///
/// Returns 1 if true
#[doc(alias = "hasInternalSubsetSAXFunc")]
pub type HasInternalSubsetSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>) -> i32;

/// Does this document has an external subset?
///
/// Returns 1 if true
#[doc(alias = "hasExternalSubsetSAXFunc")]
pub type HasExternalSubsetSAXFunc = unsafe fn(ctx: Option<GenericErrorContext>) -> i32;

/// Special constant found in SAX2 blocks initialized fields
pub const XML_SAX2_MAGIC: usize = 0xDEEDBEAF;

/// SAX2 callback when an element start has been detected by the parser.
/// It provides the namespace information for the element, as well as
/// the new namespace declarations on the element.
#[doc(alias = "startElementNsSAX2Func")]
pub type StartElementNsSAX2Func = unsafe fn(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: Option<&str>,
    uri: *const XmlChar,
    nb_namespaces: i32,
    namespaces: *mut *const XmlChar,
    nb_attributes: i32,
    nb_defaulted: i32,
    attributes: *mut *const XmlChar,
);

/// SAX2 callback when an element end has been detected by the parser.
/// It provides the namespace information for the element.
#[doc(alias = "endElementNsSAX2Func")]
pub type EndElementNsSAX2Func = unsafe fn(
    ctx: Option<GenericErrorContext>,
    localname: &str,
    prefix: *const XmlChar,
    uri: *const XmlChar,
);

pub type XmlSAXHandlerPtr = *mut XmlSAXHandler;
/// A SAX handler is bunch of callbacks called by the parser when processing
/// of the input generate data or structure information.
#[doc(alias = "xmlSAXHandler")]
#[repr(C)]
pub struct XmlSAXHandler {
    pub internal_subset: Option<InternalSubsetSAXFunc>,
    pub is_standalone: Option<IsStandaloneSAXFunc>,
    pub has_internal_subset: Option<HasInternalSubsetSAXFunc>,
    pub has_external_subset: Option<HasExternalSubsetSAXFunc>,
    pub resolve_entity: Option<ResolveEntitySAXFunc>,
    pub get_entity: Option<GetEntitySAXFunc>,
    pub entity_decl: Option<EntityDeclSAXFunc>,
    pub notation_decl: Option<NotationDeclSAXFunc>,
    pub attribute_decl: Option<AttributeDeclSAXFunc>,
    pub element_decl: Option<ElementDeclSAXFunc>,
    pub unparsed_entity_decl: Option<UnparsedEntityDeclSAXFunc>,
    pub set_document_locator: Option<SetDocumentLocatorSAXFunc>,
    pub start_document: Option<StartDocumentSAXFunc>,
    pub end_document: Option<EndDocumentSAXFunc>,
    pub start_element: Option<StartElementSAXFunc>,
    pub end_element: Option<EndElementSAXFunc>,
    pub reference: Option<ReferenceSAXFunc>,
    pub characters: Option<CharactersSAXFunc>,
    pub ignorable_whitespace: Option<IgnorableWhitespaceSAXFunc>,
    pub processing_instruction: Option<ProcessingInstructionSAXFunc>,
    pub comment: Option<CommentSAXFunc>,
    pub warning: Option<GenericError>,
    pub error: Option<GenericError>,
    pub fatal_error: Option<GenericError>, /* unused error() get all the errors */
    pub get_parameter_entity: Option<GetParameterEntitySAXFunc>,
    pub cdata_block: Option<CDATABlockSAXFunc>,
    pub external_subset: Option<ExternalSubsetSAXFunc>,
    pub initialized: u32,
    /* The following fields are extensions available only on version 2 */
    pub _private: AtomicPtr<c_void>,
    pub start_element_ns: Option<StartElementNsSAX2Func>,
    pub end_element_ns: Option<EndElementNsSAX2Func>,
    pub serror: Option<StructuredError>,
}

pub type XmlSAXHandlerV1Ptr = *mut XmlSAXHandlerV1;
#[repr(C)]
#[derive(Debug, Default)]
pub struct XmlSAXHandlerV1 {
    pub(crate) internal_subset: Option<InternalSubsetSAXFunc>,
    pub(crate) is_standalone: Option<IsStandaloneSAXFunc>,
    pub(crate) has_internal_subset: Option<HasInternalSubsetSAXFunc>,
    pub(crate) has_external_subset: Option<HasExternalSubsetSAXFunc>,
    pub(crate) resolve_entity: Option<ResolveEntitySAXFunc>,
    pub(crate) get_entity: Option<GetEntitySAXFunc>,
    pub(crate) entity_decl: Option<EntityDeclSAXFunc>,
    pub(crate) notation_decl: Option<NotationDeclSAXFunc>,
    pub(crate) attribute_decl: Option<AttributeDeclSAXFunc>,
    pub(crate) element_decl: Option<ElementDeclSAXFunc>,
    pub(crate) unparsed_entity_decl: Option<UnparsedEntityDeclSAXFunc>,
    pub(crate) set_document_locator: Option<SetDocumentLocatorSAXFunc>,
    pub(crate) start_document: Option<StartDocumentSAXFunc>,
    pub(crate) end_document: Option<EndDocumentSAXFunc>,
    pub(crate) start_element: Option<StartElementSAXFunc>,
    pub(crate) end_element: Option<EndElementSAXFunc>,
    pub(crate) reference: Option<ReferenceSAXFunc>,
    pub(crate) characters: Option<CharactersSAXFunc>,
    pub(crate) ignorable_whitespace: Option<IgnorableWhitespaceSAXFunc>,
    pub(crate) processing_instruction: Option<ProcessingInstructionSAXFunc>,
    pub(crate) comment: Option<CommentSAXFunc>,
    pub(crate) warning: Option<GenericError>,
    pub(crate) error: Option<GenericError>,
    pub(crate) fatal_error: Option<GenericError>, /* unused error() get all the errors */
    pub(crate) get_parameter_entity: Option<GetParameterEntitySAXFunc>,
    pub(crate) cdata_block: Option<CDATABlockSAXFunc>,
    pub(crate) external_subset: Option<ExternalSubsetSAXFunc>,
    pub(crate) initialized: u32,
}

/// External entity loaders types.
///
/// Returns the entity input parser.
#[doc(alias = "xmlExternalEntityLoader")]
pub type XmlExternalEntityLoader =
    unsafe fn(url: Option<&str>, id: Option<&str>, context: XmlParserCtxtPtr) -> XmlParserInputPtr;

/*
 * Macros for accessing the content. Those should be used only by the parser,
 * and not exported.
 *
 * Dirty macros, i.e. one often need to make assumption on the context to
 * use them
 *
 *   CUR_PTR return the current pointer to the XmlChar to be parsed.
 *           To be used with extreme caution since operations consuming
 *           characters may move the input buffer to a different location !
 *   CUR     returns the current XmlChar value, i.e. a 8 bit value if compiled
 *           This should be used internally by the parser
 *           only to compare to ASCII values otherwise it would break when
 *           running with UTF-8 encoding.
 *   RAW     same as CUR but in the input buffer, bypass any token
 *           extraction that may have been done
 *   NXT(n)  returns the n'th next XmlChar. Same as CUR is should be used only
 *           to compare on ASCII based substring.
 *   SKIP(n) Skip n XmlChar, and must also be used only to skip ASCII defined
 *           strings without newlines within the parser.
 *   NEXT1(l) Skip 1 XmlChar, and must also be used only to skip 1 non-newline ASCII
 *           defined c_char within the parser.
 * Clean macros, not dependent of an ASCII context, expect UTF-8 encoding
 *
 *   NEXT    Skip to the next character, this does the proper decoding
 *           in UTF-8 mode. It also pop-up unfinished entities on the fly.
 *   NEXTL!(ctxt, l) Skip the current unicode character of l xmlChars long.
 *   (*ctxt).xml_current_char( l) returns the current unicode character (int), set l
 *           to the number of xmlChars used for the encoding [0-5].
 *   CUR_SCHAR  same but operate on a string instead of the context
 *   COPY_BUF  copy the current unicode c_char to the target buffer, increment
 *            the index
 *   GROW, SHRINK  handling of input buffers
 */

macro_rules! CUR_SCHAR {
    ($ctxt:expr, $s:expr, $l:expr) => {
        crate::libxml::parser_internals::xml_string_current_char($ctxt, $s, addr_of_mut!($l))
    };
}

macro_rules! COPY_BUF {
    ($l:expr, $b:expr, $i:expr, $v:expr) => {
        if $l == 1 {
            *$b.add($i as usize) = $v as _;
            $i += 1;
        } else {
            $i = ($i as usize
                + crate::libxml::parser_internals::xml_copy_char_multi_byte(
                    $b.add($i as usize),
                    $v as _,
                ) as usize) as _
        }
    };
}

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlFatalErrMsg")]
pub(crate) unsafe fn xml_fatal_err_msg(ctxt: XmlParserCtxtPtr, error: XmlParserErrors, msg: &str) {
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
        None,
        0,
        None,
        None,
        None,
        0,
        0,
        msg,
    );
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
        if (*ctxt).recovery == 0 {
            (*ctxt).disable_sax = 1;
        }
    }
}

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlFatalErrMsgStr")]
macro_rules! xml_fatal_err_msg_str {
    ($ctxt:expr, $error:expr, $msg:literal) =>  {
        $crate::libxml::parser::xml_fatal_err_msg_str!(@inner $ctxt, $error, $msg, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $val:expr) =>  {
        let msg = format!($msg, $val);
        $crate::libxml::parser::xml_fatal_err_msg_str!(@inner $ctxt, $error, msg.as_str(), Some($val.to_owned().into()));
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $val:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                $error,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                $val,
                None,
                None,
                0,
                0,
                $msg,
            );
            if !ctxt.is_null() {
                (*ctxt).well_formed = 0;
                if (*ctxt).recovery == 0 {
                    (*ctxt).disable_sax = 1;
                }
            }
        }
    };
}
pub(crate) use xml_fatal_err_msg_str;

/// Handle a warning.
#[doc(alias = "xmlWarningMsg")]
macro_rules! xml_warning_msg {
    ($ctxt:expr, $error:expr, $msg:literal) => {
        $crate::libxml::parser::xml_warning_msg!(@inner $ctxt, $error, $msg, None, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        $crate::libxml::parser::xml_warning_msg!(@inner $ctxt, $error, &msg, Some($str1.to_owned().into()), None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $str2);
        $crate::libxml::parser::xml_warning_msg!(@inner $ctxt, $error, &msg, Some($str1.to_owned().into()), Some($str2.to_owned().into()));
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $str1:expr, $str2:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        let mut schannel: Option<$crate::globals::StructuredError> = None;

        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null()
                && !(*ctxt).sax.is_null()
                && (*(*ctxt).sax).initialized == $crate::libxml::parser::XML_SAX2_MAGIC as u32
            {
                schannel = (*(*ctxt).sax).serror;
            }
            if !ctxt.is_null() {
                __xml_raise_error!(
                    schannel,
                    if !(*ctxt).sax.is_null() {
                        (*(*ctxt).sax).warning
                    } else {
                        None
                    },
                    (*ctxt).user_data.clone(),
                    ctxt as _,
                    null_mut(),
                    XmlErrorDomain::XmlFromParser,
                    $error,
                    XmlErrorLevel::XmlErrWarning,
                    None,
                    0,
                    $str1,
                    $str2,
                    None,
                    0,
                    0,
                    $msg,
                );
            } else {
                __xml_raise_error!(
                    schannel,
                    None,
                    None,
                    null_mut(),
                    null_mut(),
                    XmlErrorDomain::XmlFromParser,
                    $error,
                    XmlErrorLevel::XmlErrWarning,
                    None,
                    0,
                    $str1,
                    $str2,
                    None,
                    0,
                    0,
                    $msg,
                );
            }
        }
    };
}
pub(crate) use xml_warning_msg;

/// Handle a non fatal parser error
#[doc(alias = "xmlErrMsgStr")]
macro_rules! xml_err_msg_str {
    ($ctxt:expr, $error:expr, $msg:literal) => {
        $crate::libxml::parser::xml_err_msg_str!(@inner $ctxt, $error, $msg, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $val:expr) => {
        let msg = format!($msg, $val);
        $crate::libxml::parser::xml_err_msg_str!(@inner $ctxt, $error, &msg, Some($val.to_owned().into()));
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $val:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                $error,
                XmlErrorLevel::XmlErrError,
                None,
                0,
                $val,
                None,
                None,
                0,
                0,
                $msg,
            );
        }
    };
}
pub(crate) use xml_err_msg_str;

/// Handle a validity error.
#[doc(alias = "xmlValidityError")]
macro_rules! xml_validity_error {
    ($ctxt:expr, $error:expr, $msg:literal) => {
        $crate::libxml::parser::xml_validity_error!(@inner $ctxt, $error, $msg, None, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        $crate::libxml::parser::xml_validity_error!(@inner $ctxt, $error, &msg, Some($str1.to_owned().into()), None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $str2);
        $crate::libxml::parser::xml_validity_error!(@inner $ctxt, $error, &msg, Some($str1.to_owned().into()), Some($str2.to_owned().into()));
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $str1:expr, $str2:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        let mut schannel: Option<$crate::globals::StructuredError> = None;

        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
                if !(*ctxt).sax.is_null() && (*(*ctxt).sax).initialized == $crate::libxml::parser::XML_SAX2_MAGIC as u32 {
                    schannel = (*(*ctxt).sax).serror;
                }
            }
            if !ctxt.is_null() {
                __xml_raise_error!(
                    schannel,
                    (*ctxt).vctxt.error,
                    (*ctxt).vctxt.user_data.clone(),
                    ctxt as _,
                    null_mut(),
                    XmlErrorDomain::XmlFromDTD,
                    $error,
                    XmlErrorLevel::XmlErrError,
                    None,
                    0,
                    $str1,
                    $str2,
                    None,
                    0,
                    0,
                    $msg,
                );
                (*ctxt).valid = 0;
            } else {
                __xml_raise_error!(
                    schannel,
                    None,
                    None,
                    ctxt as _,
                    null_mut(),
                    XmlErrorDomain::XmlFromDTD,
                    $error,
                    XmlErrorLevel::XmlErrError,
                    None,
                    0,
                    $str1,
                    $str2,
                    None,
                    0,
                    0,
                    $msg,
                );
            }
        }
    };
}
pub(crate) use xml_validity_error;

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlFatalErrMsgStrIntStr")]
macro_rules! xml_fatal_err_msg_str_int_str {
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr, $val:expr) => {
        let msg = format!($msg, $str1, $val);
        $crate::libxml::parser::xml_fatal_err_msg_str_int_str!(
            @inner
            $ctxt,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            $val,
            None
        );
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr, $val:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $val, $str2);
        $crate::libxml::parser::xml_fatal_err_msg_str_int_str!(
            @inner
            $ctxt,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            $val,
            Some($str2.to_owned().into())
        );
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $str1:expr, $val:expr, $str2:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                $error,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                $str1,
                $str2,
                None,
                $val,
                0,
                $msg,
            );
            if !ctxt.is_null() {
                (*ctxt).well_formed = 0;
                if (*ctxt).recovery == 0 {
                    (*ctxt).disable_sax = 1;
                }
            }
        }
    };
}
pub(crate) use xml_fatal_err_msg_str_int_str;

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlNsErr")]
macro_rules! xml_ns_err {
    ($ctxt:expr, $error:expr, $msg:literal) => {
        $crate::libxml::parser::xml_ns_err!(@inner $ctxt, $error, $msg, None, None, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $info1:expr) => {
        let msg = format!($msg, $info1);
        $crate::libxml::parser::xml_ns_err!(@inner $ctxt, $error, &msg, Some($info1.to_owned().into()), None, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $info1:expr, $info2:expr) => {
        let msg = format!($msg, $info1, $info2);
        $crate::libxml::parser::xml_ns_err!(@inner $ctxt, $error, &msg, Some($info1.to_owned().into()), Some($info2.to_owned().into()), None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $info1:expr, $info2:expr, $info3:expr) => {
        let msg = format!($msg, $info1, $info2, $info3);
        $crate::libxml::parser::xml_ns_err!(@inner $ctxt, $error, &msg, Some($info1.to_owned().into()), Some($info2.to_owned().into()), Some($info3.to_owned().into()));
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $info1:expr, $info2:expr, $info3:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromNamespace,
                $error,
                XmlErrorLevel::XmlErrError,
                None,
                0,
                $info1,
                $info2,
                $info3,
                0,
                0,
                $msg,
            );
            if !ctxt.is_null() {
                (*ctxt).ns_well_formed = 0;
            }
        }
    };
}
pub(crate) use xml_ns_err;

pub(crate) unsafe extern "C" fn xml_is_name_char(ctxt: XmlParserCtxtPtr, c: i32) -> i32 {
    if (*ctxt).options & XmlParserOption::XmlParseOld10 as i32 == 0 {
        /*
         * Use the new checks of production [4] [4a] amd [5] of the
         * Update 5 of XML-1.0
         */
        if c != b' ' as i32
            && c != b'>' as i32
            && c != b'/' as i32 /* accelerators */
            && ((c >= b'a' as i32 && c <= b'z' as i32)
                || (c >= b'A' as i32 && c <= b'Z' as i32)
                || (c >= b'0' as i32 && c <= b'9' as i32) /* !start */
                || c == b'_' as i32
                || c == b':' as i32
                || c == b'-' as i32
                || c == b'.' as i32
                || c == 0xB7 /* !start */
                || (0xC0..=0xD6).contains(&c)
                || (0xD8..=0xF6).contains(&c)
                || (0xF8..=0x2FF).contains(&c)
                || (0x300..=0x36F).contains(&c) /* !start */
                || (0x370..=0x37D).contains(&c)
                || (0x37F..=0x1FFF).contains(&c)
                || (0x200C..=0x200D).contains(&c)
                || (0x203F..=0x2040).contains(&c) /* !start */
                || (0x2070..=0x218F).contains(&c)
                || (0x2C00..=0x2FEF).contains(&c)
                || (0x3001..=0xD7FF).contains(&c)
                || (0xF900..=0xFDCF).contains(&c)
                || (0xFDF0..=0xFFFD).contains(&c)
                || (0x10000..=0xEFFFF).contains(&c))
        {
            return 1;
        }
    } else if xml_is_letter(c as u32)
        || xml_is_digit(c as u32)
        || c == b'.' as i32
        || c == b'-' as i32
        || c == b'_' as i32
        || c == b':' as i32
        || xml_is_combining(c as u32)
        || xml_is_extender(c as u32)
    {
        return 1;
    }
    0
}

static XML_PARSER_INITIALIZED: AtomicBool = AtomicBool::new(false);

/// Initialization function for the XML parser.
/// This is not reentrant. Call once before processing in case of
/// use in multithreaded programs.
#[doc(alias = "xmlInitParser")]
pub unsafe extern "C" fn xml_init_parser() {
    /*
     * Note that the initialization code must not make memory allocations.
     */
    if XML_PARSER_INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    __xml_global_init_mutex_lock();
    if !XML_PARSER_INITIALIZED.load(Ordering::Acquire) {
        xml_init_threads_internal();
        xml_init_globals_internal();
        xml_init_memory_internal();
        __xml_initialize_dict();
        register_default_input_callbacks();
        #[cfg(feature = "libxml_output")]
        {
            register_default_output_callbacks();
        }
        #[cfg(any(feature = "xpath", feature = "schema"))]
        {
            xml_init_xpath_internal();
        }
        XML_PARSER_INITIALIZED.store(true, Ordering::Release);
    }

    __xml_global_init_mutex_unlock();
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
pub unsafe extern "C" fn xml_cleanup_parser() {
    if !XML_PARSER_INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    #[cfg(feature = "catalog")]
    {
        xml_catalog_cleanup();
    }
    xml_cleanup_dict_internal();
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
    xml_cleanup_globals_internal();
    xml_cleanup_threads_internal();
    xml_cleanup_memory_internal();
    XML_PARSER_INITIALIZED.store(false, Ordering::Release);
}

/// Returns -1 as this is an error to use it.
#[doc(alias = "xmlParserInputRead")]
pub(crate) unsafe extern "C" fn xml_parser_input_read(_input: XmlParserInputPtr, _len: i32) -> i32 {
    -1
}

/// This function increase the input for the parser. It tries to
/// preserve pointers to the input buffer, and keep already read data
///
/// Returns the amount of c_char read, or -1 in case of error, 0 indicate the end of this entity
#[doc(alias = "xmlParserInputGrow")]
pub(crate) unsafe extern "C" fn xml_parser_input_grow(input: XmlParserInputPtr, len: i32) -> i32 {
    if input.is_null() || len < 0 {
        return -1;
    }
    let Some(input_buffer) = (*input).buf.as_mut() else {
        return -1;
    };
    if (*input).base.is_null() {
        return -1;
    }
    if (*input).cur.is_null() {
        return -1;
    }
    let Some(buf) = input_buffer.borrow().buffer else {
        return -1;
    };

    /* Don't grow memory buffers. */
    if input_buffer.borrow().encoder.is_none() && input_buffer.borrow().context.is_none() {
        return 0;
    }

    let indx: size_t = (*input).cur.offset_from((*input).base) as _;
    if buf.len() > indx + INPUT_CHUNK {
        return 0;
    }
    let ret: i32 = input_buffer.borrow_mut().grow(len);

    (*input).base = if buf.is_ok() {
        buf.as_ref().as_ptr()
    } else {
        null_mut()
    };
    if (*input).base.is_null() {
        (*input).base = c"".as_ptr() as _;
        (*input).cur = (*input).base;
        (*input).end = (*input).base;
        return -1;
    }
    (*input).cur = (*input).base.add(indx as usize);
    (*input).end = if buf.is_ok() {
        buf.as_ref().as_ptr().add(buf.len())
    } else {
        null_mut()
    };

    ret
}

/// Parse an XML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "xmlParseDoc")]
#[deprecated = "Use xmlReadDoc"]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_parse_doc(cur: *const XmlChar) -> XmlDocPtr {
    xml_sax_parse_doc(null_mut(), cur, 0)
}

/// Parse an XML file and build a tree. Automatic support for ZLIB/Compress
/// compressed document is provided by default if found at compile-time.
///
/// Returns the resulting document tree if the file was wellformed,
/// NULL otherwise.
#[doc(alias = "xmlParseFile")]
#[deprecated = "Use xmlReadFile"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_parse_file(filename: Option<&str>) -> XmlDocPtr {
    xml_sax_parse_file(null_mut(), filename, 0)
}

/// Parse an XML in-memory block and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "xmlParseMemory")]
#[deprecated = "Use xmlReadMemory"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_parse_memory(buffer: Vec<u8>) -> XmlDocPtr {
    xml_sax_parse_memory(null_mut(), buffer, 0)
}

/// Set and return the previous value for default entity support.
/// Initially the parser always keep entity references instead of substituting
/// entity values in the output. This function has to be used to change the
/// default parser behavior
/// SAX::substituteEntities() has to be used for changing that on a file by
/// file basis.
///
/// Returns the last value for 0 for no substitution, 1 for substitution.
#[doc(alias = "xmlSubstituteEntitiesDefault")]
#[deprecated = "Use the modern options API with XML_PARSE_NOENT"]
pub fn xml_substitute_entities_default(val: i32) -> i32 {
    let old = get_substitute_entities_default_value();

    set_substitute_entities_default_value(val);
    old
}

/// Set and return the previous value for default blanks text nodes support.
/// The 1.x version of the parser used an heuristic to try to detect
/// ignorable white spaces. As a result the SAX callback was generating
/// xmlSAX2IgnorableWhitespace() callbacks instead of characters() one, and when
/// using the DOM output text nodes containing those blanks were not generated.
/// The 2.x and later version will switch to the XML standard way and
/// ignorableWhitespace() are only generated when running the parser in
/// validating mode and when the current element doesn't allow CDATA or
/// mixed content.
/// This function is provided as a way to force the standard behavior
/// on 1.X libs and to switch back to the old mode for compatibility when
/// running 1.X client code on 2.X . Upgrade of 1.X code should be done
/// by using xmlIsBlankNode() commodity function to detect the "empty"
/// nodes generated.
/// This value also affect autogeneration of indentation when saving code
/// if blanks sections are kept, indentation is not generated.
///
/// Returns the last value for 0 for no substitution, 1 for substitution.
#[doc(alias = "xmlKeepBlanksDefault")]
#[deprecated = "Use the modern options API with XML_PARSE_NOBLANKS"]
pub fn xml_keep_blanks_default(val: i32) -> i32 {
    let old = get_keep_blanks_default_value();

    set_keep_blanks_default_value(val);
    if val == 0 {
        set_indent_tree_output(1);
    }
    old
}

/// Set and return the previous value for enabling pedantic warnings.
///
/// Returns the last value for 0 for no substitution, 1 for substitution.
#[doc(alias = "xmlPedanticParserDefault")]
#[deprecated = "Use the modern options API with XML_PARSE_PEDANTIC"]
pub fn xml_pedantic_parser_default(val: i32) -> i32 {
    let old = get_pedantic_parser_default_value();

    set_pedantic_parser_default_value(val);
    old
}

/// Set and return the previous value for enabling line numbers in elements
/// contents. This may break on old application and is turned off by default.
///
/// Returns the last value for 0 for no substitution, 1 for substitution.
#[doc(alias = "xmlLineNumbersDefault")]
#[deprecated = "The modern options API always enables line numbers"]
pub fn xml_line_numbers_default(val: i32) -> i32 {
    let old = get_line_numbers_default_value();

    set_line_numbers_default_value(val);
    old
}

/// Parse an XML in-memory document and build a tree.
/// In the case the document is not Well Formed, a attempt to build a
/// tree is tried anyway
///
/// Returns the resulting document tree or NULL in case of failure
#[doc(alias = "xmlRecoverDoc")]
#[deprecated = "Use xmlReadDoc with XML_PARSE_RECOVER"]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_recover_doc(cur: *const XmlChar) -> XmlDocPtr {
    xml_sax_parse_doc(null_mut(), cur, 1)
}

/// Parse an XML in-memory block and build a tree.
/// In the case the document is not Well Formed, an attempt to
/// build a tree is tried anyway
///
/// Returns the resulting document tree or NULL in case of error
#[doc(alias = "xmlRecoverMemory")]
#[deprecated = "Use xmlReadMemory with XML_PARSE_RECOVER"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_recover_memory(buffer: Vec<u8>) -> XmlDocPtr {
    xml_sax_parse_memory(null_mut(), buffer, 1)
}

/// Parse an XML file and build a tree. Automatic support for ZLIB/Compress
/// compressed document is provided by default if found at compile-time.
/// In the case the document is not Well Formed, it attempts to build
/// a tree anyway
///
/// Returns the resulting document tree or NULL in case of failure
#[doc(alias = "xmlRecoverFile")]
#[deprecated = "Use xmlReadFile with XML_PARSE_RECOVER"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_recover_file(filename: Option<&str>) -> XmlDocPtr {
    xml_sax_parse_file(null_mut(), filename, 1)
}

pub type XmlDefAttrsPtr = *mut XmlDefAttrs;
#[repr(C)]
pub struct XmlDefAttrs {
    pub(crate) nb_attrs: i32, /* number of defaulted attributes on that element */
    pub(crate) max_attrs: i32, /* the size of the array */
    pub(crate) values: [*const XmlChar; 5], /* array of localname/prefix/values/external */
}

/// Parse a conditional section. Always consumes '<!['.
///
/// ```ignore
/// [61] conditionalSect ::= includeSect | ignoreSect
/// [62] includeSect ::= '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
/// [63] ignoreSect ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
/// [64] ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
/// [65] Ignore ::= Char* - (Char* ('<![' | ']]>') Char*)
/// ```
#[doc(alias = "xmlParseConditionalSections")]
pub(crate) unsafe extern "C" fn xml_parse_conditional_sections(ctxt: XmlParserCtxtPtr) {
    let mut input_ids: *mut i32 = null_mut();
    let mut input_ids_size: size_t = 0;
    let mut depth: size_t = 0;

    while !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        if (*ctxt).current_byte() == b'<'
            && (*ctxt).nth_byte(1) == b'!'
            && (*ctxt).nth_byte(2) == b'['
        {
            let id: i32 = (*(*ctxt).input).id;

            (*ctxt).advance(3);
            (*ctxt).skip_blanks();

            if (*ctxt).content_bytes().starts_with(b"INCLUDE") {
                (*ctxt).advance(7);
                (*ctxt).skip_blanks();
                if (*ctxt).current_byte() != b'[' {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrCondsecInvalid, None);
                    (*ctxt).halt();
                    //  goto error;
                    xml_free(input_ids as _);
                    return;
                }
                if (*(*ctxt).input).id != id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        "All markup of the conditional section is not in the same entity\n",
                    );
                }
                (*ctxt).skip_char();

                if input_ids_size <= depth {
                    input_ids_size = if input_ids_size == 0 {
                        4
                    } else {
                        input_ids_size * 2
                    };
                    let tmp: *mut i32 =
                        xml_realloc(input_ids as _, input_ids_size as usize * size_of::<i32>())
                            as _;
                    if tmp.is_null() {
                        xml_err_memory(ctxt, None);
                        //  goto error;
                        xml_free(input_ids as _);
                        return;
                    }
                    input_ids = tmp;
                }
                *input_ids.add(depth as usize) = id;
                depth += 1;
            } else if (*ctxt).content_bytes().starts_with(b"IGNORE") {
                let mut ignore_depth: size_t = 0;

                (*ctxt).advance(6);
                (*ctxt).skip_blanks();
                if (*ctxt).current_byte() != b'[' {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrCondsecInvalid, None);
                    (*ctxt).halt();
                    //  goto error;
                    xml_free(input_ids as _);
                    return;
                }
                if (*(*ctxt).input).id != id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        "All markup of the conditional section is not in the same entity\n",
                    );
                }
                (*ctxt).skip_char();

                while (*ctxt).current_byte() != 0 {
                    if (*ctxt).current_byte() == b'<'
                        && (*ctxt).nth_byte(1) == b'!'
                        && (*ctxt).nth_byte(2) == b'['
                    {
                        (*ctxt).advance(3);
                        ignore_depth += 1;
                        /* Check for integer overflow */
                        if ignore_depth == 0 {
                            xml_err_memory(ctxt, None);
                            //  goto error;
                            xml_free(input_ids as _);
                            return;
                        }
                    } else if (*ctxt).current_byte() == b']'
                        && (*ctxt).nth_byte(1) == b']'
                        && (*ctxt).nth_byte(2) == b'>'
                    {
                        if ignore_depth == 0 {
                            break;
                        }
                        (*ctxt).advance(3);
                        ignore_depth -= 1;
                    } else {
                        (*ctxt).skip_char();
                    }
                }

                if (*ctxt).current_byte() == 0 {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrCondsecNotFinished, None);
                    //  goto error;
                    xml_free(input_ids as _);
                    return;
                }
                if (*(*ctxt).input).id != id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        "All markup of the conditional section is not in the same entity\n",
                    );
                }
                (*ctxt).advance(3);
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrCondsecInvalidKeyword, None);
                (*ctxt).halt();
                //  goto error;
                xml_free(input_ids as _);
                return;
            }
        } else if depth > 0
            && (*ctxt).current_byte() == b']'
            && (*ctxt).nth_byte(1) == b']'
            && (*ctxt).nth_byte(2) == b'>'
        {
            depth -= 1;
            if (*(*ctxt).input).id != *input_ids.add(depth as usize) {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    "All markup of the conditional section is not in the same entity\n",
                );
            }
            (*ctxt).advance(3);
        } else if (*ctxt).current_byte() == b'<'
            && ((*ctxt).nth_byte(1) == b'!' || (*ctxt).nth_byte(1) == b'?')
        {
            xml_parse_markup_decl(ctxt);
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtSubsetNotFinished, None);
            (*ctxt).halt();
            //  goto error;
            xml_free(input_ids as _);
            return;
        }

        if depth == 0 {
            break;
        }

        (*ctxt).skip_blanks();
        (*ctxt).shrink();
        (*ctxt).grow();
    }

    //  error:
    xml_free(input_ids as _);
}

/// Parse the internal subset declaration
///
/// `[28 end] ('[' (markupdecl | PEReference | S)* ']' S?)? '>'`
#[doc(alias = "xmlParseInternalSubset")]
unsafe extern "C" fn xml_parse_internal_subset(ctxt: XmlParserCtxtPtr) {
    /*
     * Is there any DTD definition ?
     */
    if (*ctxt).current_byte() == b'[' {
        let base_input_nr = (*ctxt).input_tab.len();
        (*ctxt).instate = XmlParserInputState::XmlParserDTD;
        (*ctxt).skip_char();
        /*
         * Parse the succession of Markup declarations and
         * PEReferences.
         * Subsequence (markupdecl | PEReference | S)*
         */
        (*ctxt).skip_blanks();
        #[allow(clippy::while_immutable_condition)]
        while ((*ctxt).current_byte() != b']' || (*ctxt).input_tab.len() > base_input_nr)
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            /*
             * Conditional sections are allowed from external entities included
             * by PE References in the internal subset.
             */
            if (*ctxt).input_tab.len() > 1
                && (*(*ctxt).input).filename.is_some()
                && (*ctxt).current_byte() == b'<'
                && (*ctxt).nth_byte(1) == b'!'
                && (*ctxt).nth_byte(2) == b'['
            {
                xml_parse_conditional_sections(ctxt);
            } else if (*ctxt).current_byte() == b'<'
                && ((*ctxt).nth_byte(1) == b'!' || (*ctxt).nth_byte(1) == b'?')
            {
                xml_parse_markup_decl(ctxt);
            } else if (*ctxt).current_byte() == b'%' {
                xml_parse_pe_reference(ctxt);
            } else {
                xml_fatal_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    Some("xmlParseInternalSubset: error detected in Markup declaration\n"),
                );
                (*ctxt).halt();
                return;
            }
            (*ctxt).skip_blanks();
            (*ctxt).shrink();
            (*ctxt).grow();
        }
        if (*ctxt).current_byte() == b']' {
            (*ctxt).skip_char();
            (*ctxt).skip_blanks();
        }
    }

    /*
     * We should be at the end of the DOCTYPE declaration.
     */
    if (*ctxt).current_byte() != b'>' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDoctypeNotFinished, None);
        return;
    }
    (*ctxt).skip_char();
}

/// Trim the list of attributes defined to remove all those of type
/// CDATA as they are not special. This call should be done when finishing
/// to parse the DTD and before starting to parse the document root.
#[doc(alias = "xmlCleanSpecialAttr")]
unsafe extern "C" fn xml_clean_special_attr(ctxt: XmlParserCtxtPtr) {
    let Some(mut atts) = (*ctxt).atts_special else {
        return;
    };

    atts.remove_if(
        |data, _, _, _| *data == XmlAttributeType::XmlAttributeCDATA,
        |_, _| {},
    );
    if atts.is_empty() {
        atts.free();
        (*ctxt).atts_special = None;
    }
}

pub(crate) const SAX_COMPAT_MODE: &str = "SAX compatibility mode document";

/// Parse an XML document (and build a tree if using the standard SAX
/// interface).
///
/// `[1] document ::= prolog element Misc*`
///
/// `[22] prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?`
///
/// Returns 0, -1 in case of error. the parser context is augmented
///                as a result of the parsing.
#[doc(alias = "xmlParseDocument")]
pub unsafe extern "C" fn xml_parse_document(ctxt: XmlParserCtxtPtr) -> i32 {
    let mut start: [XmlChar; 4] = [0; 4];

    xml_init_parser();

    if ctxt.is_null() || (*ctxt).input.is_null() {
        return -1;
    }

    (*ctxt).grow();

    /*
     * SAX: detecting the level.
     */
    (*ctxt).detect_sax2();

    /*
     * SAX: beginning of the document processing.
     */
    if !(*ctxt).sax.is_null() {
        if let Some(set_document_locator) = (*(*ctxt).sax).set_document_locator {
            set_document_locator((*ctxt).user_data.clone(), xml_default_sax_locator());
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }

    if (*ctxt).encoding().is_none() && (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4 {
        /*
         * Get the 4 first bytes and decode the charset
         * if enc != XML_CHAR_ENCODING_NONE
         * plug some encoding conversion routines.
         */
        start[0] = (*ctxt).current_byte();
        start[1] = (*ctxt).nth_byte(1);
        start[2] = (*ctxt).nth_byte(2);
        start[3] = (*ctxt).nth_byte(3);
        let enc = detect_encoding(&start);
        if !matches!(enc, XmlCharEncoding::None) {
            xml_switch_encoding(ctxt, enc);
        }
    }

    (*ctxt).grow();
    if (*ctxt).content_bytes().starts_with(b"<?xml")
        && xml_is_blank_char((*ctxt).nth_byte(5) as u32)
    {
        /*
         * Note that we will switch encoding on the fly.
         */
        xml_parse_xmldecl(ctxt);
        if (*ctxt).err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32
            || matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            /*
             * The XML REC instructs us to stop parsing right here
             */
            return -1;
        }
        (*ctxt).standalone = (*(*ctxt).input).standalone;
        (*ctxt).skip_blanks();
    } else {
        (*ctxt).version = Some(XML_DEFAULT_VERSION.to_owned());
    }
    if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
        if let Some(start_document) = (*(*ctxt).sax).start_document {
            start_document((*ctxt).user_data.clone());
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }
    if !(*ctxt).my_doc.is_null()
        && !(*ctxt).input.is_null()
        && (*(*ctxt).input).buf.is_some()
        && (*(*ctxt).input).buf.as_ref().unwrap().borrow().compressed >= 0
    {
        (*(*ctxt).my_doc).compression = (*(*ctxt).input).buf.as_ref().unwrap().borrow().compressed;
    }

    // The Misc part of the Prolog
    xml_parse_misc(ctxt);

    // Then possibly doc type declaration(s) and more Misc
    // (doctypedecl Misc*)?
    (*ctxt).grow();
    if (*ctxt).content_bytes().starts_with(b"<!DOCTYPE") {
        (*ctxt).in_subset = 1;
        xml_parse_doc_type_decl(ctxt);
        if (*ctxt).current_byte() == b'[' {
            (*ctxt).instate = XmlParserInputState::XmlParserDTD;
            xml_parse_internal_subset(ctxt);
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                return -1;
            }
        }

        // Create and update the external subset.
        (*ctxt).in_subset = 2;
        if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
            if let Some(external_subset) = (*(*ctxt).sax).external_subset {
                external_subset(
                    (*ctxt).user_data.clone(),
                    (*ctxt).int_sub_name.as_deref(),
                    (*ctxt).ext_sub_system.as_deref(),
                    (*ctxt).ext_sub_uri.as_deref(),
                );
            }
        }
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            return -1;
        }
        (*ctxt).in_subset = 0;

        xml_clean_special_attr(ctxt);

        (*ctxt).instate = XmlParserInputState::XmlParserProlog;
        xml_parse_misc(ctxt);
    }

    // Time to start parsing the tree itself
    (*ctxt).grow();
    if (*ctxt).current_byte() != b'<' {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrDocumentEmpty,
            "Start tag expected, '<' not found\n",
        );
    } else {
        (*ctxt).instate = XmlParserInputState::XmlParserContent;
        xml_parse_element(ctxt);
        (*ctxt).instate = XmlParserInputState::XmlParserEpilog;

        /*
         * The Misc part at the end
         */
        xml_parse_misc(ctxt);

        if (*ctxt).current_byte() != 0 {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEnd, None);
        }
        (*ctxt).instate = XmlParserInputState::XmlParserEOF;
    }

    /*
     * SAX: end of the document processing.
     */
    if !(*ctxt).sax.is_null() {
        if let Some(end_document) = (*(*ctxt).sax).end_document {
            end_document((*ctxt).user_data.clone());
        }
    }

    /*
     * Remove locally kept entity definitions if the tree was not built
     */
    if !(*ctxt).my_doc.is_null() && (*(*ctxt).my_doc).version.as_deref() == Some(SAX_COMPAT_MODE) {
        xml_free_doc((*ctxt).my_doc);
        (*ctxt).my_doc = null_mut();
    }

    if (*ctxt).well_formed != 0 && !(*ctxt).my_doc.is_null() {
        (*(*ctxt).my_doc).properties |= XmlDocProperties::XmlDocWellformed as i32;
        if (*ctxt).valid != 0 {
            (*(*ctxt).my_doc).properties |= XmlDocProperties::XmlDocDTDValid as i32;
        }
        if (*ctxt).ns_well_formed != 0 {
            (*(*ctxt).my_doc).properties |= XmlDocProperties::XmlDocNsvalid as i32;
        }
        if (*ctxt).options & XmlParserOption::XmlParseOld10 as i32 != 0 {
            (*(*ctxt).my_doc).properties |= XmlDocProperties::XmlDocOld10 as i32;
        }
    }
    if (*ctxt).well_formed == 0 {
        (*ctxt).valid = 0;
        return -1;
    }
    0
}

/// parse a general parsed entity
/// An external general parsed entity is well-formed if it matches the
/// production labeled extParsedEnt.
///
/// `[78] extParsedEnt ::= TextDecl? content`
///
/// Returns 0, -1 in case of error. the parser context is augmented as a result of the parsing.
#[doc(alias = "xmlParseExtParsedEnt")]
pub unsafe extern "C" fn xml_parse_ext_parsed_ent(ctxt: XmlParserCtxtPtr) -> i32 {
    let mut start: [XmlChar; 4] = [0; 4];

    if ctxt.is_null() || (*ctxt).input.is_null() {
        return -1;
    }

    (*ctxt).detect_sax2();

    (*ctxt).grow();

    /*
     * SAX: beginning of the document processing.
     */
    if !(*ctxt).sax.is_null() {
        if let Some(set_document_locator) = (*(*ctxt).sax).set_document_locator {
            set_document_locator((*ctxt).user_data.clone(), xml_default_sax_locator());
        }
    }

    /*
     * Get the 4 first bytes and decode the charset
     * if enc != XML_CHAR_ENCODING_NONE
     * plug some encoding conversion routines.
     */
    if (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4 {
        start[0] = (*ctxt).current_byte();
        start[1] = (*ctxt).nth_byte(1);
        start[2] = (*ctxt).nth_byte(2);
        start[3] = (*ctxt).nth_byte(3);
        let enc = detect_encoding(&start);
        if !matches!(enc, XmlCharEncoding::None) {
            xml_switch_encoding(ctxt, enc);
        }
    }

    if (*ctxt).current_byte() == 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEmpty, None);
    }

    /*
     * Check for the XMLDecl in the Prolog.
     */
    (*ctxt).grow();
    if (*ctxt).content_bytes().starts_with(b"<?xml")
        && xml_is_blank_char((*ctxt).nth_byte(5) as u32)
    {
        /*
         * Note that we will switch encoding on the fly.
         */
        xml_parse_xmldecl(ctxt);
        if (*ctxt).err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
            /*
             * The XML REC instructs us to stop parsing right here
             */
            return -1;
        }
        (*ctxt).skip_blanks();
    } else {
        (*ctxt).version = Some(XML_DEFAULT_VERSION.to_owned());
    }
    if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
        if let Some(start_document) = (*(*ctxt).sax).start_document {
            start_document((*ctxt).user_data.clone());
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }

    /*
     * Doing validity checking on chunk doesn't make sense
     */
    (*ctxt).instate = XmlParserInputState::XmlParserContent;
    (*ctxt).validate = 0;
    (*ctxt).loadsubset = 0;
    (*ctxt).depth = 0;

    xml_parse_content(ctxt);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }

    if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
    } else if (*ctxt).current_byte() != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtraContent, None);
    }

    /*
     * SAX: end of the document processing.
     */
    if !(*ctxt).sax.is_null() {
        if let Some(end_document) = (*(*ctxt).sax).end_document {
            end_document((*ctxt).user_data.clone());
        }
    }

    if (*ctxt).well_formed == 0 {
        return -1;
    }
    0
}

/// parse an XML file and call the given SAX handler routines.
/// Automatic support for ZLIB/Compress compressed document is provided
///
/// Returns 0 in case of success or a error number otherwise
#[doc(alias = "xmlSAXUserParseFile")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadFile"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_user_parse_file(
    sax: XmlSAXHandlerPtr,
    user_data: Option<GenericErrorContext>,
    filename: Option<&str>,
) -> i32 {
    let ret: i32;

    let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(filename);
    if ctxt.is_null() {
        return -1;
    }
    if (*ctxt).sax != xml_default_sax_handler() as XmlSAXHandlerPtr {
        xml_free((*ctxt).sax as _);
    }
    (*ctxt).sax = sax;
    (*ctxt).detect_sax2();

    // if !user_data.is_null() {
    (*ctxt).user_data = user_data;
    // }

    xml_parse_document(ctxt);

    if (*ctxt).well_formed != 0 {
        ret = 0;
    } else if (*ctxt).err_no != 0 {
        ret = (*ctxt).err_no;
    } else {
        ret = -1;
    }
    if !sax.is_null() {
        (*ctxt).sax = null_mut();
    }
    if !(*ctxt).my_doc.is_null() {
        xml_free_doc((*ctxt).my_doc);
        (*ctxt).my_doc = null_mut();
    }
    xml_free_parser_ctxt(ctxt);

    ret
}

/// Parse an XML in-memory buffer and call the given SAX handler routines.
///
/// Returns 0 in case of success or a error number otherwise
#[doc(alias = "xmlSAXUserParseMemory")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadMemory"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_user_parse_memory(
    sax: XmlSAXHandlerPtr,
    user_data: Option<GenericErrorContext>,
    buffer: Vec<u8>,
) -> i32 {
    let ret: i32;

    xml_init_parser();

    let ctxt: XmlParserCtxtPtr = xml_create_memory_parser_ctxt(buffer);
    if ctxt.is_null() {
        return -1;
    }
    if (*ctxt).sax != xml_default_sax_handler() as XmlSAXHandlerPtr {
        xml_free((*ctxt).sax as _);
    }
    (*ctxt).sax = sax;
    (*ctxt).detect_sax2();

    // if !user_data.is_null() {
    (*ctxt).user_data = user_data;
    // }

    xml_parse_document(ctxt);

    if (*ctxt).well_formed != 0 {
        ret = 0;
    } else if (*ctxt).err_no != 0 {
        ret = (*ctxt).err_no;
    } else {
        ret = -1;
    }
    if !sax.is_null() {
        (*ctxt).sax = null_mut();
    }
    if !(*ctxt).my_doc.is_null() {
        xml_free_doc((*ctxt).my_doc);
        (*ctxt).my_doc = null_mut();
    }
    xml_free_parser_ctxt(ctxt);

    ret
}

/// Parse an XML in-memory document and build a tree.
/// It use the given SAX function block to handle the parsing callback.
/// If sax is NULL, fallback to the default DOM tree building routines.
///
/// Returns the resulting document tree
#[doc(alias = "xmlSAXParseDoc")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadDoc"]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_sax_parse_doc(
    sax: XmlSAXHandlerPtr,
    cur: *const XmlChar,
    recovery: i32,
) -> XmlDocPtr {
    let ret: XmlDocPtr;

    let mut oldsax: XmlSAXHandlerPtr = null_mut();

    if cur.is_null() {
        return null_mut();
    }

    let ctxt: XmlParserCtxtPtr = xml_create_doc_parser_ctxt(cur);
    if ctxt.is_null() {
        return null_mut();
    }
    if !sax.is_null() {
        oldsax = (*ctxt).sax;
        (*ctxt).sax = sax;
        (*ctxt).user_data = None;
    }
    (*ctxt).detect_sax2();

    xml_parse_document(ctxt);
    if (*ctxt).well_formed != 0 || recovery != 0 {
        ret = (*ctxt).my_doc;
    } else {
        ret = null_mut();
        xml_free_doc((*ctxt).my_doc);
        (*ctxt).my_doc = null_mut();
    }
    if !sax.is_null() {
        (*ctxt).sax = oldsax;
    }
    xml_free_parser_ctxt(ctxt);

    ret
}

/// Parse an XML in-memory block and use the given SAX function block
/// to handle the parsing callback. If sax is NULL, fallback to the default
/// DOM tree building routines.
///
/// Returns the resulting document tree
#[doc(alias = "xmlSAXParseMemory")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadMemory"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_parse_memory(
    sax: XmlSAXHandlerPtr,
    buffer: Vec<u8>,
    recovery: i32,
) -> XmlDocPtr {
    xml_sax_parse_memory_with_data(sax, buffer, recovery, null_mut())
}

/// Parse an XML in-memory block and use the given SAX function block
/// to handle the parsing callback. If sax is NULL, fallback to the default
/// DOM tree building routines.
///
/// User data (c_void *) is stored within the parser context in the
/// context's _private member, so it is available nearly everywhere in libxml
///
/// Returns the resulting document tree
#[doc(alias = "xmlSAXParseMemoryWithData")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadMemory"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_parse_memory_with_data(
    sax: XmlSAXHandlerPtr,
    buffer: Vec<u8>,
    recovery: i32,
    data: *mut c_void,
) -> XmlDocPtr {
    let ret: XmlDocPtr;

    xml_init_parser();

    let ctxt: XmlParserCtxtPtr = xml_create_memory_parser_ctxt(buffer);
    if ctxt.is_null() {
        return null_mut();
    }
    if !sax.is_null() {
        if !(*ctxt).sax.is_null() {
            xml_free((*ctxt).sax as _);
        }
        (*ctxt).sax = sax;
    }
    (*ctxt).detect_sax2();
    if !data.is_null() {
        (*ctxt)._private = data;
    }

    (*ctxt).recovery = recovery;

    xml_parse_document(ctxt);

    if (*ctxt).well_formed != 0 || recovery != 0 {
        ret = (*ctxt).my_doc;
    } else {
        ret = null_mut();
        xml_free_doc((*ctxt).my_doc);
        (*ctxt).my_doc = null_mut();
    }
    if !sax.is_null() {
        (*ctxt).sax = null_mut();
    }
    xml_free_parser_ctxt(ctxt);

    ret
}

/// parse an XML file and build a tree. Automatic support for ZLIB/Compress
/// compressed document is provided by default if found at compile-time.
/// It use the given SAX function block to handle the parsing callback.
/// If sax is NULL, fallback to the default DOM tree building routines.
///
/// Returns the resulting document tree
#[doc(alias = "xmlSAXParseFile")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadFile"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_parse_file(
    sax: XmlSAXHandlerPtr,
    filename: Option<&str>,
    recovery: i32,
) -> XmlDocPtr {
    xml_sax_parse_file_with_data(sax, filename, recovery, null_mut())
}

/// Parse an XML file and build a tree. Automatic support for ZLIB/Compress
/// compressed document is provided by default if found at compile-time.
/// It use the given SAX function block to handle the parsing callback.
/// If sax is NULL, fallback to the default DOM tree building routines.
///
/// User data (c_void *) is stored within the parser context in the
/// context's _private member, so it is available nearly everywhere in libxml
///
/// Returns the resulting document tree
#[doc(alias = "xmlSAXParseFileWithData")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadFile"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_parse_file_with_data(
    sax: XmlSAXHandlerPtr,
    filename: Option<&str>,
    recovery: i32,
    data: *mut c_void,
) -> XmlDocPtr {
    use crate::io::xml_parser_get_directory;

    let ret: XmlDocPtr;

    xml_init_parser();

    let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(filename);
    if ctxt.is_null() {
        return null_mut();
    }
    if !sax.is_null() {
        if !(*ctxt).sax.is_null() {
            xml_free((*ctxt).sax as _);
        }
        (*ctxt).sax = sax;
    }
    (*ctxt).detect_sax2();
    if !data.is_null() {
        (*ctxt)._private = data;
    }

    if (*ctxt).directory.is_none() {
        if let Some(filename) = filename {
            if let Some(dir) = xml_parser_get_directory(filename) {
                (*ctxt).directory = Some(dir.to_string_lossy().into_owned());
            }
        }
    }

    (*ctxt).recovery = recovery;

    xml_parse_document(ctxt);

    if (*ctxt).well_formed != 0 || recovery != 0 {
        ret = (*ctxt).my_doc;
        if !ret.is_null() && (*(*ctxt).input).buf.is_some() {
            if (*(*ctxt).input).buf.as_ref().unwrap().borrow().compressed > 0 {
                (*ret).compression = 9;
            } else {
                (*ret).compression = (*(*ctxt).input).buf.as_ref().unwrap().borrow().compressed;
            }
        }
    } else {
        ret = null_mut();
        xml_free_doc((*ctxt).my_doc);
        (*ctxt).my_doc = null_mut();
    }
    if !sax.is_null() {
        (*ctxt).sax = null_mut();
    }
    xml_free_parser_ctxt(ctxt);

    ret
}

/// Parse an XML external entity out of context and build a tree.
/// It use the given SAX function block to handle the parsing callback.
/// If sax is NULL, fallback to the default DOM tree building routines.
///
/// `[78] extParsedEnt ::= TextDecl? content`
///
/// This correspond to a "Well Balanced" chunk
///
/// Returns the resulting document tree
#[doc(alias = "xmlSAXParseEntity")]
#[cfg(feature = "sax1")]
pub(crate) unsafe fn xml_sax_parse_entity(
    sax: XmlSAXHandlerPtr,
    filename: Option<&str>,
) -> XmlDocPtr {
    let ret: XmlDocPtr;

    let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(filename);
    if ctxt.is_null() {
        return null_mut();
    }
    if !sax.is_null() {
        if !(*ctxt).sax.is_null() {
            xml_free((*ctxt).sax as _);
        }
        (*ctxt).sax = sax;
        (*ctxt).user_data = None;
    }

    xml_parse_ext_parsed_ent(ctxt);

    if (*ctxt).well_formed != 0 {
        ret = (*ctxt).my_doc;
    } else {
        ret = null_mut();
        xml_free_doc((*ctxt).my_doc);
        (*ctxt).my_doc = null_mut();
    }
    if !sax.is_null() {
        (*ctxt).sax = null_mut();
    }
    xml_free_parser_ctxt(ctxt);

    ret
}

/// Parse an XML external entity out of context and build a tree.
///
/// `[78] extParsedEnt ::= TextDecl? content`
///
/// This correspond to a "Well Balanced" chunk
///
/// Returns the resulting document tree
#[doc(alias = "xmlParseEntity")]
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe fn xml_parse_entity(filename: Option<&str>) -> XmlDocPtr {
    xml_sax_parse_entity(null_mut(), filename)
}

/// Load and parse an external subset.
///
/// Returns the resulting xmlDtdPtr or NULL in case of error.
#[doc(alias = "xmlSAXParseDTD")]
#[cfg(feature = "libxml_valid")]
pub(crate) unsafe fn xml_sax_parse_dtd(
    sax: XmlSAXHandlerPtr,
    external_id: Option<&str>,
    system_id: Option<&str>,
) -> XmlDtdPtr {
    use std::slice::from_raw_parts;

    let mut ret: XmlDtdPtr = null_mut();
    let mut input: XmlParserInputPtr = null_mut();

    if external_id.is_none() && system_id.is_none() {
        return null_mut();
    }

    let ctxt: XmlParserCtxtPtr = xml_new_sax_parser_ctxt(sax, None);
    if ctxt.is_null() {
        return null_mut();
    }

    // We are loading a DTD
    (*ctxt).options |= XmlParserOption::XmlParseDtdload as i32;

    // Canonicalise the system ID
    let system_id_canonic = system_id.map(|s| canonic_path(s));

    // Ask the Entity resolver to load the damn thing

    if !(*ctxt).sax.is_null() {
        if let Some(f) = (*(*ctxt).sax).resolve_entity {
            input = f(
                (*ctxt).user_data.clone(),
                external_id,
                system_id_canonic.as_deref(),
            );
        }
    }
    if input.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    // plug some encoding conversion routines here.
    if xml_push_input(ctxt, input) < 0 {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }
    if (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4 {
        let input = from_raw_parts((*(*ctxt).input).cur, 4);
        let enc = detect_encoding(input);
        xml_switch_encoding(ctxt, enc);
    }

    if (*input).filename.is_none() {
        if let Some(canonic) = system_id_canonic {
            (*input).filename = Some(canonic.into_owned());
        }
    }
    (*input).line = 1;
    (*input).col = 1;
    (*input).base = (*(*ctxt).input).cur;
    (*input).cur = (*(*ctxt).input).cur;
    (*input).free = None;

    // let's parse that entity knowing it's an external subset.
    (*ctxt).in_subset = 2;
    (*ctxt).my_doc = xml_new_doc(Some("1.0"));
    if (*ctxt).my_doc.is_null() {
        xml_err_memory(ctxt, Some("New Doc failed"));
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }
    (*(*ctxt).my_doc).properties = XmlDocProperties::XmlDocInternal as i32;
    (*(*ctxt).my_doc).ext_subset =
        xml_new_dtd((*ctxt).my_doc, Some("none"), external_id, system_id);
    xml_parse_external_subset(ctxt, external_id, system_id);

    if !(*ctxt).my_doc.is_null() {
        if (*ctxt).well_formed != 0 {
            ret = (*(*ctxt).my_doc).ext_subset;
            (*(*ctxt).my_doc).ext_subset = null_mut();
            if !ret.is_null() {
                (*ret).doc = null_mut();
                let mut tmp = (*ret).children;
                while let Some(mut now) = tmp {
                    now.doc = null_mut();
                    tmp = now.next;
                }
            }
        } else {
            ret = null_mut();
        }
        xml_free_doc((*ctxt).my_doc);
        (*ctxt).my_doc = null_mut();
    }
    xml_free_parser_ctxt(ctxt);

    ret
}

/// Load and parse an external subset.
///
/// Returns the resulting xmlDtdPtr or NULL in case of error.
#[doc(alias = "xmlParseDTD")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_parse_dtd(external_id: Option<&str>, system_id: Option<&str>) -> XmlDtdPtr {
    xml_sax_parse_dtd(null_mut(), external_id, system_id)
}

/// Load and parse a DTD
///
/// Returns the resulting xmlDtdPtr or NULL in case of error.
/// `input` will be freed by the function in any case.
#[doc(alias = "xmlIOParseDTD")]
#[cfg(feature = "libxml_valid")]
pub unsafe fn xml_io_parse_dtd(
    sax: XmlSAXHandlerPtr,
    input: XmlParserInputBuffer,
    mut enc: XmlCharEncoding,
) -> XmlDtdPtr {
    let mut ret: XmlDtdPtr = null_mut();
    let mut start: [XmlChar; 4] = [0; 4];

    let ctxt: XmlParserCtxtPtr = xml_new_sax_parser_ctxt(sax, None);
    if ctxt.is_null() {
        return null_mut();
    }

    /* We are loading a DTD */
    (*ctxt).options |= XmlParserOption::XmlParseDtdload as i32;

    (*ctxt).detect_sax2();

    /*
     * generate a parser input from the I/O handler
     */

    let pinput: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, Rc::new(RefCell::new(input)), XmlCharEncoding::None);
    if pinput.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    /*
     * plug some encoding conversion routines here.
     */
    if xml_push_input(ctxt, pinput) < 0 {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }
    if !matches!(enc, XmlCharEncoding::None) {
        xml_switch_encoding(ctxt, enc);
    }

    (*pinput).filename = None;
    (*pinput).line = 1;
    (*pinput).col = 1;
    (*pinput).base = (*(*ctxt).input).cur;
    (*pinput).cur = (*(*ctxt).input).cur;
    (*pinput).free = None;

    /*
     * let's parse that entity knowing it's an external subset.
     */
    (*ctxt).in_subset = 2;
    (*ctxt).my_doc = xml_new_doc(Some("1.0"));
    if (*ctxt).my_doc.is_null() {
        xml_err_memory(ctxt, Some("New Doc failed"));
        return null_mut();
    }
    (*(*ctxt).my_doc).properties = XmlDocProperties::XmlDocInternal as i32;
    (*(*ctxt).my_doc).ext_subset =
        xml_new_dtd((*ctxt).my_doc, Some("none"), Some("none"), Some("none"));

    if matches!(enc, XmlCharEncoding::None)
        && (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4
    {
        // Get the 4 first bytes and decode the charset
        // if enc != xmlCharEncoding::XML_CHAR_ENCODING_NONE
        // plug some encoding conversion routines.
        start[0] = (*ctxt).current_byte();
        start[1] = (*ctxt).nth_byte(1);
        start[2] = (*ctxt).nth_byte(2);
        start[3] = (*ctxt).nth_byte(3);
        enc = detect_encoding(&start);
        if !matches!(enc, XmlCharEncoding::None) {
            xml_switch_encoding(ctxt, enc);
        }
    }

    xml_parse_external_subset(ctxt, Some("none"), Some("none"));

    if !(*ctxt).my_doc.is_null() {
        if (*ctxt).well_formed != 0 {
            ret = (*(*ctxt).my_doc).ext_subset;
            (*(*ctxt).my_doc).ext_subset = null_mut();
            if !ret.is_null() {
                (*ret).doc = null_mut();
                let mut tmp = (*ret).children;
                while let Some(mut now) = tmp {
                    now.doc = null_mut();
                    tmp = now.next;
                }
            }
        } else {
            ret = null_mut();
        }
        xml_free_doc((*ctxt).my_doc);
        (*ctxt).my_doc = null_mut();
    }
    xml_free_parser_ctxt(ctxt);

    ret
}

/// Parse a well-balanced chunk of an XML document
/// called by the parser
/// The allowed sequence for the Well Balanced Chunk is the one defined by
/// the content production in the XML grammar:
///
/// `[43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*`
///
/// Returns 0 if the chunk is well balanced, -1 in case of args problem and
/// the parser error code otherwise
#[doc(alias = "xmlParseBalancedChunkMemory")]
#[cfg(feature = "sax1")]
pub unsafe fn xml_parse_balanced_chunk_memory(
    doc: XmlDocPtr,
    sax: XmlSAXHandlerPtr,
    user_data: Option<GenericErrorContext>,
    depth: i32,
    string: *const XmlChar,
    lst: *mut XmlNodePtr,
) -> i32 {
    xml_parse_balanced_chunk_memory_recover(doc, sax, user_data, depth, string, lst, 0)
}

// Lookup the namespace name for the @prefix (which ca be NULL)
// The prefix must come from the @(*ctxt).dict dictionary
//
// Returns the namespace name or NULL if not bound
#[doc(alias = "xmlGetNamespace")]
unsafe extern "C" fn xml_get_namespace(
    ctxt: XmlParserCtxtPtr,
    prefix: *const XmlChar,
) -> *const XmlChar {
    if prefix == (*ctxt).str_xml {
        return (*ctxt).str_xml_ns;
    }
    for i in (0..(*ctxt).ns_tab.len().saturating_sub(1)).rev().step_by(2) {
        if (*ctxt).ns_tab[i] == prefix {
            if prefix.is_null() && (*(*ctxt).ns_tab[i + 1]) == 0 {
                return null_mut();
            }
            return (*ctxt).ns_tab[i + 1];
        }
    }
    null_mut()
}

/// Parse a well-balanced chunk of an XML document
/// within the context (DTD, namespaces, etc ...) of the given node.
///
/// The allowed sequence for the data is a Well Balanced Chunk defined by
/// the content production in the XML grammar:
///
/// `[43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*`
///
/// Returns xmlParserErrors::XML_ERR_OK if the chunk is well balanced, and the parser
/// error code otherwise
#[doc(alias = "xmlParseInNodeContext")]
pub unsafe fn xml_parse_in_node_context(
    mut node: XmlNodePtr,
    data: Vec<u8>,
    mut options: i32,
    lst: *mut XmlNodePtr,
) -> XmlParserErrors {
    let ctxt: XmlParserCtxtPtr;
    let mut cur: XmlNodePtr;
    let mut nsnr = 0;
    let ret: XmlParserErrors;

    // check all input parameters, grab the document
    if lst.is_null() || node.is_null() {
        return XmlParserErrors::XmlErrInternalError;
    }
    match (*node).element_type() {
        XmlElementType::XmlElementNode
        | XmlElementType::XmlAttributeNode
        | XmlElementType::XmlTextNode
        | XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlDocumentNode
        | XmlElementType::XmlHTMLDocumentNode => {}
        _ => {
            return XmlParserErrors::XmlErrInternalError;
        }
    }
    while !node.is_null()
        && !matches!(
            (*node).element_type(),
            XmlElementType::XmlElementNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlHTMLDocumentNode
        )
    {
        node = (*node).parent().map_or(null_mut(), |p| p.as_ptr());
    }
    if node.is_null() {
        return XmlParserErrors::XmlErrInternalError;
    }
    let doc = if (*node).element_type() == XmlElementType::XmlElementNode {
        (*node).doc
    } else {
        (*node).as_document_node().unwrap().as_ptr()
    };
    if doc.is_null() {
        return XmlParserErrors::XmlErrInternalError;
    }

    /*
     * allocate a context and set-up everything not related to the
     * node position in the tree
     */
    if (*doc).typ == XmlElementType::XmlDocumentNode {
        ctxt = xml_create_memory_parser_ctxt(data);
    } else if cfg!(feature = "html") && (*doc).typ == XmlElementType::XmlHTMLDocumentNode {
        #[cfg(feature = "html")]
        {
            ctxt = html_create_memory_parser_ctxt(data);
            /*
             * When parsing in context, it makes no sense to add implied
             * elements like html/body/etc...
             */
            options |= HtmlParserOption::HtmlParseNoimplied as i32;
        }
    } else {
        return XmlParserErrors::XmlErrInternalError;
    }

    if ctxt.is_null() {
        return XmlParserErrors::XmlErrNoMemory;
    }

    /*
     * Use input doc's dict if present, else assure XML_PARSE_NODICT is set.
     * We need a dictionary for xmlDetectSAX2, so if there's no doc dict
     * we must wait until the last moment to free the original one.
     */
    if !(*doc).dict.is_null() {
        if !(*ctxt).dict.is_null() {
            xml_dict_free((*ctxt).dict);
        }
        (*ctxt).dict = (*doc).dict;
    } else {
        options |= XmlParserOption::XmlParseNodict as i32;
    }

    if let Some(encoding) = (*doc).encoding.as_deref() {
        (*ctxt).encoding = Some(encoding.to_owned());

        if let Some(handler) = find_encoding_handler(encoding) {
            (*ctxt).switch_to_encoding(handler);
        } else {
            return XmlParserErrors::XmlErrUnsupportedEncoding;
        }
    }

    (*ctxt).ctxt_use_options_internal(options, None);
    (*ctxt).detect_sax2();
    (*ctxt).my_doc = doc;
    /* parsing in context, i.e. as within existing content */
    (*ctxt).input_id = 2;
    (*ctxt).instate = XmlParserInputState::XmlParserContent;

    let fake: XmlNodePtr = xml_new_doc_comment((*node).doc, "");
    if fake.is_null() {
        xml_free_parser_ctxt(ctxt);
        return XmlParserErrors::XmlErrNoMemory;
    }
    (*node).add_child(fake);

    if (*node).element_type() == XmlElementType::XmlElementNode {
        (*ctxt).node_push(node);
        // initialize the SAX2 namespaces stack
        cur = node;
        while !cur.is_null() && (*cur).element_type() == XmlElementType::XmlElementNode {
            let mut ns: XmlNsPtr = (*cur).ns_def;
            let mut iprefix: *const XmlChar;
            let mut ihref: *const XmlChar;

            while !ns.is_null() {
                if !(*ctxt).dict.is_null() {
                    iprefix = xml_dict_lookup((*ctxt).dict, (*ns).prefix, -1);
                    ihref = xml_dict_lookup((*ctxt).dict, (*ns).href, -1);
                } else {
                    iprefix = (*ns).prefix;
                    ihref = (*ns).href;
                }

                if xml_get_namespace(ctxt, iprefix).is_null() {
                    (*ctxt).ns_push(iprefix, ihref);
                    nsnr += 1;
                }
                ns = (*ns).next as _;
            }
            cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        }
    }

    if (*ctxt).validate != 0 || (*ctxt).replace_entities != 0 {
        // ID/IDREF registration will be done in xmlValidateElement below
        (*ctxt).loadsubset |= XML_SKIP_IDS as i32;
    }

    #[cfg(feature = "html")]
    {
        if (*doc).typ == XmlElementType::XmlHTMLDocumentNode {
            __html_parse_content(ctxt as _);
        } else {
            xml_parse_content(ctxt);
        }
    }
    #[cfg(not(feature = "html"))]
    {
        xml_parse_content(ctxt);
    }

    (*ctxt).ns_pop(nsnr);
    if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
    } else if (*ctxt).current_byte() != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtraContent, None);
    }
    if !(*ctxt).node.is_null() && (*ctxt).node != node {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
        (*ctxt).well_formed = 0;
    }

    if (*ctxt).well_formed == 0 {
        if (*ctxt).err_no == 0 {
            ret = XmlParserErrors::XmlErrInternalError;
        } else {
            ret = XmlParserErrors::try_from((*ctxt).err_no).unwrap();
        }
    } else {
        ret = XmlParserErrors::XmlErrOK;
    }

    // Return the newly created nodeset after unlinking it from
    // the pseudo sibling.

    cur = (*fake).next.take().map_or(null_mut(), |n| n.as_ptr());
    (*node).set_last(NodePtr::from_ptr(fake));

    if !cur.is_null() {
        (*cur).prev = None;
    }

    *lst = cur;

    while !cur.is_null() {
        (*cur).set_parent(None);
        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
    }

    (*fake).unlink();
    xml_free_node(fake);

    if !matches!(ret, XmlParserErrors::XmlErrOK) {
        xml_free_node_list(*lst);
        *lst = null_mut();
    }

    if !(*doc).dict.is_null() {
        (*ctxt).dict = null_mut();
    }
    xml_free_parser_ctxt(ctxt);

    ret
}

/// Parse a well-balanced chunk of an XML document
/// called by the parser
/// The allowed sequence for the Well Balanced Chunk is the one defined by
/// the content production in the XML grammar:
///
/// `[43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*`
///
/// Returns 0 if the chunk is well balanced, -1 in case of args problem and
///    the parser error code otherwise
///
/// In case recover is set to 1, the nodelist will not be empty even if
/// the parsed chunk is not well balanced, assuming the parsing succeeded to
/// some extent.
#[doc(alias = "xmlParseBalancedChunkMemoryRecover")]
#[cfg(feature = "sax1")]
pub unsafe fn xml_parse_balanced_chunk_memory_recover(
    doc: XmlDocPtr,
    sax: XmlSAXHandlerPtr,
    user_data: Option<GenericErrorContext>,
    depth: i32,
    string: *const XmlChar,
    lst: *mut XmlNodePtr,
    recover: i32,
) -> i32 {
    let mut oldsax: XmlSAXHandlerPtr = null_mut();
    let content: XmlNodePtr;
    let ret: i32;

    if depth > 40 {
        return XmlParserErrors::XmlErrEntityLoop as i32;
    }

    if !lst.is_null() {
        *lst = null_mut();
    }
    if string.is_null() {
        return -1;
    }

    let ctxt: XmlParserCtxtPtr =
        xml_create_memory_parser_ctxt(CStr::from_ptr(string as *const i8).to_bytes().to_vec());
    std::ptr::write(&mut (*ctxt).last_error, XmlError::default());
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).user_data = Some(GenericErrorContext::new(ctxt));
    if !sax.is_null() {
        oldsax = (*ctxt).sax;
        (*ctxt).sax = sax;
        if user_data.is_some() {
            (*ctxt).user_data = user_data;
        }
    }
    let new_doc: XmlDocPtr = xml_new_doc(Some("1.0"));
    if new_doc.is_null() {
        xml_free_parser_ctxt(ctxt);
        return -1;
    }
    (*new_doc).properties = XmlDocProperties::XmlDocInternal as i32;
    if !doc.is_null() && !(*doc).dict.is_null() {
        xml_dict_free((*ctxt).dict);
        (*ctxt).dict = (*doc).dict;
        xml_dict_reference((*ctxt).dict);
        (*ctxt).str_xml = xml_dict_lookup((*ctxt).dict, c"xml".as_ptr() as _, 3);
        (*ctxt).str_xmlns = xml_dict_lookup((*ctxt).dict, c"xmlns".as_ptr() as _, 5);
        (*ctxt).str_xml_ns = xml_dict_lookup((*ctxt).dict, XML_XML_NAMESPACE.as_ptr() as _, 36);
        (*ctxt).dict_names = 1;
    } else {
        (*ctxt).ctxt_use_options_internal(XmlParserOption::XmlParseNodict as i32, None);
    }
    /* doc.is_null() is only supported for historic reasons */
    if !doc.is_null() {
        (*new_doc).int_subset = (*doc).int_subset;
        (*new_doc).ext_subset = (*doc).ext_subset;
    }
    let new_root: XmlNodePtr =
        xml_new_doc_node(new_doc, null_mut(), c"pseudoroot".as_ptr() as _, null());
    if new_root.is_null() {
        if !sax.is_null() {
            (*ctxt).sax = oldsax;
        }
        xml_free_parser_ctxt(ctxt);
        (*new_doc).int_subset = null_mut();
        (*new_doc).ext_subset = null_mut();
        xml_free_doc(new_doc);
        return -1;
    }
    (*new_doc).add_child(new_root);
    (*ctxt).node_push(new_root);
    /* doc.is_null() is only supported for historic reasons */
    if doc.is_null() {
        (*ctxt).my_doc = new_doc;
    } else {
        (*ctxt).my_doc = new_doc;
        (*new_doc).children.unwrap().doc = doc;
        /* Ensure that doc has XML spec namespace */
        (*(doc as *mut XmlNode)).search_ns_by_href(doc, XML_XML_NAMESPACE.to_str().unwrap());
        (*new_doc).old_ns = (*doc).old_ns;
    }
    (*ctxt).instate = XmlParserInputState::XmlParserContent;
    (*ctxt).input_id = 2;
    (*ctxt).depth = depth;

    /*
     * Doing validity checking on chunk doesn't make sense
     */
    (*ctxt).validate = 0;
    (*ctxt).loadsubset = 0;
    (*ctxt).detect_sax2();

    if !doc.is_null() {
        content = (*doc).children.take().map_or(null_mut(), |c| c.as_ptr());
        xml_parse_content(ctxt);
        (*doc).children = NodePtr::from_ptr(content);
    } else {
        xml_parse_content(ctxt);
    }
    if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
    } else if (*ctxt).current_byte() != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtraContent, None);
    }
    if NodePtr::from_ptr((*ctxt).node) != (*new_doc).children {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
    }

    if (*ctxt).well_formed == 0 {
        if (*ctxt).err_no == 0 {
            ret = 1;
        } else {
            ret = (*ctxt).err_no;
        }
    } else {
        ret = 0;
    }

    if !lst.is_null() && (ret == 0 || recover == 1) {
        // Return the newly created nodeset after unlinking it from
        // they pseudo parent.
        let mut cur = (*new_doc)
            .children
            .unwrap()
            .children()
            .map_or(null_mut(), |c| c.as_ptr());
        *lst = cur;
        while !cur.is_null() {
            (*cur).set_doc(doc);
            (*cur).set_parent(None);
            cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
        }
        (*new_doc).children.unwrap().set_children(None);
    }

    if !sax.is_null() {
        (*ctxt).sax = oldsax;
    }
    xml_free_parser_ctxt(ctxt);
    (*new_doc).int_subset = null_mut();
    (*new_doc).ext_subset = null_mut();
    /* This leaks the namespace list if doc.is_null() */
    (*new_doc).old_ns = null_mut();
    xml_free_doc(new_doc);

    ret
}

/// Private version of xmlParseExternalEntity()
///
/// Returns 0 if the entity is well formed, -1 in case of args problem and
/// the parser error code otherwise
#[doc(alias = "xmlParseExternalEntityPrivate")]
#[allow(clippy::too_many_arguments)]
pub(crate) unsafe fn xml_parse_external_entity_private(
    doc: XmlDocPtr,
    oldctxt: XmlParserCtxtPtr,
    sax: XmlSAXHandlerPtr,
    user_data: Option<GenericErrorContext>,
    depth: i32,
    url: Option<&str>,
    id: Option<&str>,
    list: *mut XmlNodePtr,
) -> XmlParserErrors {
    let ret: XmlParserErrors;
    let mut start: [XmlChar; 4] = [0; 4];

    if (depth > 40
        && (oldctxt.is_null() || (*oldctxt).options & XmlParserOption::XmlParseHuge as i32 == 0))
        || depth > 100
    {
        xml_fatal_err_msg(
            oldctxt,
            XmlParserErrors::XmlErrEntityLoop,
            "Maximum entity nesting depth exceeded",
        );
        return XmlParserErrors::XmlErrEntityLoop;
    }

    if !list.is_null() {
        *list = null_mut();
    }
    if url.is_none() && id.is_none() {
        return XmlParserErrors::XmlErrInternalError;
    }
    if doc.is_null() {
        return XmlParserErrors::XmlErrInternalError;
    }

    let ctxt: XmlParserCtxtPtr =
        xml_create_entity_parser_ctxt_internal(sax, user_data, url, id, None, oldctxt);
    if ctxt.is_null() {
        return XmlParserErrors::XmlWarUndeclaredEntity;
    }
    if !oldctxt.is_null() {
        (*ctxt).nb_errors = (*oldctxt).nb_errors;
        (*ctxt).nb_warnings = (*oldctxt).nb_warnings;
    }
    (*ctxt).detect_sax2();

    let new_doc: XmlDocPtr = xml_new_doc(Some("1.0"));
    if new_doc.is_null() {
        xml_free_parser_ctxt(ctxt);
        return XmlParserErrors::XmlErrInternalError;
    }
    (*new_doc).properties = XmlDocProperties::XmlDocInternal as i32;
    if !doc.is_null() {
        (*new_doc).int_subset = (*doc).int_subset;
        (*new_doc).ext_subset = (*doc).ext_subset;
        if !(*doc).dict.is_null() {
            (*new_doc).dict = (*doc).dict;
            xml_dict_reference((*new_doc).dict);
        }
        if (*doc).url.is_some() {
            (*new_doc).url = (*doc).url.clone();
        }
    }
    let new_root: XmlNodePtr =
        xml_new_doc_node(new_doc, null_mut(), c"pseudoroot".as_ptr() as _, null());
    if new_root.is_null() {
        if !sax.is_null() {
            xml_free_parser_ctxt(ctxt);
        }
        (*new_doc).int_subset = null_mut();
        (*new_doc).ext_subset = null_mut();
        xml_free_doc(new_doc);
        return XmlParserErrors::XmlErrInternalError;
    }
    (*new_doc).add_child(new_root);
    (*ctxt).node_push((*new_doc).children.map_or(null_mut(), |c| c.as_ptr()));
    if doc.is_null() {
        (*ctxt).my_doc = new_doc;
    } else {
        (*ctxt).my_doc = doc;
        (*new_root).doc = doc;
    }

    /*
     * Get the 4 first bytes and decode the charset
     * if enc != xmlCharEncoding::XML_CHAR_ENCODING_NONE
     * plug some encoding conversion routines.
     */
    (*ctxt).grow();
    if (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4 {
        start[0] = (*ctxt).current_byte();
        start[1] = (*ctxt).nth_byte(1);
        start[2] = (*ctxt).nth_byte(2);
        start[3] = (*ctxt).nth_byte(3);
        let enc = detect_encoding(&start);
        if !matches!(enc, XmlCharEncoding::None) {
            xml_switch_encoding(ctxt, enc);
        }
    }

    /*
     * Parse a possible text declaration first
     */
    if (*ctxt).content_bytes().starts_with(b"<?xml")
        && xml_is_blank_char((*ctxt).nth_byte(5) as u32)
    {
        xml_parse_text_decl(ctxt);
        /*
         * An XML-1.0 document can't reference an entity not XML-1.0
         */
        if (*oldctxt).version.as_deref() == Some("1.0")
            && (*(*ctxt).input).version.as_deref() != Some("1.0")
        {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrVersionMismatch,
                "Version mismatch between document and entity\n",
            );
        }
    }

    (*ctxt).instate = XmlParserInputState::XmlParserContent;
    (*ctxt).depth = depth;
    if !oldctxt.is_null() {
        (*ctxt)._private = (*oldctxt)._private;
        (*ctxt).loadsubset = (*oldctxt).loadsubset;
        (*ctxt).validate = (*oldctxt).validate;
        (*ctxt).valid = (*oldctxt).valid;
        (*ctxt).replace_entities = (*oldctxt).replace_entities;
        if (*oldctxt).validate != 0 {
            (*ctxt).vctxt.error = (*oldctxt).vctxt.error;
            (*ctxt).vctxt.warning = (*oldctxt).vctxt.warning;
            (*ctxt).vctxt.user_data = (*oldctxt).vctxt.user_data.clone();
            (*ctxt).vctxt.flags = (*oldctxt).vctxt.flags;
        }
        (*ctxt).external = (*oldctxt).external;
        if !(*ctxt).dict.is_null() {
            xml_dict_free((*ctxt).dict);
        }
        (*ctxt).dict = (*oldctxt).dict;
        (*ctxt).str_xml = xml_dict_lookup((*ctxt).dict, c"xml".as_ptr() as _, 3);
        (*ctxt).str_xmlns = xml_dict_lookup((*ctxt).dict, c"xmlns".as_ptr() as _, 5);
        (*ctxt).str_xml_ns = xml_dict_lookup((*ctxt).dict, XML_XML_NAMESPACE.as_ptr() as _, 36);
        (*ctxt).dict_names = (*oldctxt).dict_names;
        (*ctxt).atts_default = (*oldctxt).atts_default;
        (*ctxt).atts_special = (*oldctxt).atts_special;
        (*ctxt).linenumbers = (*oldctxt).linenumbers;
        (*ctxt).record_info = (*oldctxt).record_info;
        (*ctxt).node_seq.maximum = (*oldctxt).node_seq.maximum;
        (*ctxt).node_seq.length = (*oldctxt).node_seq.length;
        (*ctxt).node_seq.buffer = (*oldctxt).node_seq.buffer;
    } else {
        /*
         * Doing validity checking on chunk without context
         * doesn't make sense
         */
        (*ctxt)._private = null_mut();
        (*ctxt).validate = 0;
        (*ctxt).external = 2;
        (*ctxt).loadsubset = 0;
    }

    xml_parse_content(ctxt);

    if (*ctxt).current_byte() == b'<' && (*ctxt).nth_byte(1) == b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
    } else if (*ctxt).current_byte() != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtraContent, None);
    }
    if NodePtr::from_ptr((*ctxt).node) != (*new_doc).children {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, None);
    }

    if (*ctxt).well_formed == 0 {
        ret = XmlParserErrors::try_from((*ctxt).err_no).unwrap();
        if !oldctxt.is_null() {
            (*oldctxt).err_no = (*ctxt).err_no;
            (*oldctxt).well_formed = 0;
            (*oldctxt).last_error = (*ctxt).last_error.clone();
        }
    } else {
        if !list.is_null() {
            // Return the newly created nodeset after unlinking it from they pseudo parent.
            let mut cur = (*new_doc)
                .children()
                .unwrap()
                .children()
                .map_or(null_mut(), |c| c.as_ptr());
            *list = cur;
            while !cur.is_null() {
                (*cur).set_parent(None);
                cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
            }
            (*new_doc).children.unwrap().set_children(None);
        }
        ret = XmlParserErrors::XmlErrOK;
    }

    /*
     * Also record the size of the entity parsed
     */
    if !(*ctxt).input.is_null() && !oldctxt.is_null() {
        let mut consumed: u64 = (*(*ctxt).input).consumed;
        consumed =
            consumed.saturating_add((*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _);

        (*oldctxt).sizeentities = (*oldctxt).sizeentities.saturating_add(consumed);
        (*oldctxt).sizeentities = (*oldctxt).sizeentities.saturating_add((*ctxt).sizeentities);

        (*oldctxt).sizeentcopy = (*oldctxt).sizeentcopy.saturating_add(consumed);
        (*oldctxt).sizeentcopy = (*oldctxt).sizeentcopy.saturating_add((*ctxt).sizeentcopy);
    }

    if !oldctxt.is_null() {
        (*ctxt).dict = null_mut();
        (*ctxt).atts_default = None;
        (*ctxt).atts_special = None;
        (*oldctxt).nb_errors = (*ctxt).nb_errors;
        (*oldctxt).nb_warnings = (*ctxt).nb_warnings;
        (*oldctxt).validate = (*ctxt).validate;
        (*oldctxt).valid = (*ctxt).valid;
        (*oldctxt).node_seq.maximum = (*ctxt).node_seq.maximum;
        (*oldctxt).node_seq.length = (*ctxt).node_seq.length;
        (*oldctxt).node_seq.buffer = (*ctxt).node_seq.buffer;
    }
    (*ctxt).node_seq.maximum = 0;
    (*ctxt).node_seq.length = 0;
    (*ctxt).node_seq.buffer = null_mut();
    xml_free_parser_ctxt(ctxt);
    (*new_doc).int_subset = null_mut();
    (*new_doc).ext_subset = null_mut();
    xml_free_doc(new_doc);

    ret
}

/// Parse an external general entity
/// An external general parsed entity is well-formed if it matches the
/// production labeled extParsedEnt.
///
/// `[78] extParsedEnt ::= TextDecl? content`
///
/// Returns 0 if the entity is well formed, -1 in case of args problem and
///    the parser error code otherwise
#[doc(alias = "xmlParseExternalEntity")]
#[deprecated]
#[cfg(feature = "sax1")]
pub(crate) unsafe fn xml_parse_external_entity(
    doc: XmlDocPtr,
    sax: XmlSAXHandlerPtr,
    user_data: Option<GenericErrorContext>,
    depth: i32,
    url: Option<&str>,
    id: Option<&str>,
    lst: *mut XmlNodePtr,
) -> i32 {
    xml_parse_external_entity_private(doc, null_mut(), sax, user_data, depth, url, id, lst) as i32
}

/// Parse an external general entity within an existing parsing context
/// An external general parsed entity is well-formed if it matches the
/// production labeled extParsedEnt.
///
/// `[78] extParsedEnt ::= TextDecl? content`
///
/// Returns 0 if the entity is well formed, -1 in case of args problem and
/// the parser error code otherwise
#[doc(alias = "xmlParseCtxtExternalEntity")]
pub unsafe fn xml_parse_ctxt_external_entity(
    ctx: XmlParserCtxtPtr,
    url: Option<&str>,
    id: Option<&str>,
    lst: *mut XmlNodePtr,
) -> i32 {
    if ctx.is_null() {
        return -1;
    }
    // If the user provided their own SAX callbacks, then reuse the
    // userData callback field, otherwise the expected setup in a
    // DOM builder is to have userData == ctxt
    let user_data = if (*ctx)
        .user_data
        .as_ref()
        .and_then(|d| d.lock().downcast_ref::<XmlParserCtxtPtr>().copied())
        == Some(ctx)
    {
        None
    } else {
        (*ctx).user_data.clone()
    };
    xml_parse_external_entity_private(
        (*ctx).my_doc,
        ctx,
        (*ctx).sax,
        user_data,
        (*ctx).depth + 1,
        url,
        id,
        lst,
    ) as _
}

/// Allocate and initialize a new parser context.
///
/// Returns the xmlParserCtxtPtr or NULL
#[doc(alias = "xmlNewParserCtxt")]
pub unsafe extern "C" fn xml_new_parser_ctxt() -> XmlParserCtxtPtr {
    xml_new_sax_parser_ctxt(null_mut(), None)
}

/// Initialize a SAX parser context
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "xmlInitSAXParserCtxt")]
unsafe fn xml_init_sax_parser_ctxt(
    ctxt: XmlParserCtxtPtr,
    sax: *const XmlSAXHandler,
    user_data: Option<GenericErrorContext>,
) -> i32 {
    let mut input: XmlParserInputPtr;

    if ctxt.is_null() {
        xml_err_internal!(null_mut(), "Got NULL parser context\n");
        return -1;
    }

    xml_init_parser();

    if (*ctxt).dict.is_null() {
        (*ctxt).dict = xml_dict_create();
    }
    if (*ctxt).dict.is_null() {
        xml_err_memory(null_mut(), Some("cannot initialize parser context\n"));
        return -1;
    }
    xml_dict_set_limit((*ctxt).dict, XML_MAX_DICTIONARY_LIMIT);

    if (*ctxt).sax.is_null() {
        (*ctxt).sax = xml_malloc(size_of::<XmlSAXHandler>()) as _;
    }
    if (*ctxt).sax.is_null() {
        xml_err_memory(null_mut(), Some("cannot initialize parser context\n"));
        return -1;
    }
    if sax.is_null() {
        memset((*ctxt).sax as _, 0, size_of::<XmlSAXHandler>());
        xml_sax_version((*ctxt).sax, 2);
        (*ctxt).user_data = Some(GenericErrorContext::new(ctxt));
    } else {
        if (*sax).initialized == XML_SAX2_MAGIC as u32 {
            memcpy((*ctxt).sax as _, sax as _, size_of::<XmlSAXHandler>());
        } else {
            memset((*ctxt).sax as _, 0, size_of::<XmlSAXHandler>());
            memcpy((*ctxt).sax as _, sax as _, size_of::<XmlSAXHandlerV1>());
        }
        (*ctxt).user_data = if user_data.is_some() {
            user_data
        } else {
            Some(GenericErrorContext::new(ctxt))
        };
    }

    (*ctxt).maxatts = 0;
    (*ctxt).atts = vec![];
    // Allocate the Input stack
    (*ctxt).input_tab.shrink_to(5);
    while {
        input = (*ctxt).input_pop();
        !input.is_null()
    } {
        // Non consuming
        xml_free_input_stream(input);
    }
    (*ctxt).input = null_mut();
    (*ctxt).version = None;
    (*ctxt).encoding = None;
    (*ctxt).standalone = -1;
    (*ctxt).has_external_subset = 0;
    (*ctxt).has_perefs = 0;
    (*ctxt).html = 0;
    (*ctxt).external = 0;
    (*ctxt).instate = XmlParserInputState::XmlParserStart;
    (*ctxt).token = 0;
    (*ctxt).directory = None;

    /* Allocate the Node stack */
    (*ctxt).node_tab.clear();
    (*ctxt).node_tab.shrink_to(10);
    (*ctxt).node = null_mut();

    /* Allocate the Name stack */
    (*ctxt).name_tab.clear();
    (*ctxt).name_tab.shrink_to(10);
    (*ctxt).name = null_mut();

    /* Allocate the space stack */
    (*ctxt).space_tab.clear();
    (*ctxt).space_tab.shrink_to(10);
    (*ctxt).space_tab.push(-1);

    (*ctxt).my_doc = null_mut();
    (*ctxt).well_formed = 1;
    (*ctxt).ns_well_formed = 1;
    (*ctxt).valid = 1;
    (*ctxt).loadsubset = get_load_ext_dtd_default_value();
    if (*ctxt).loadsubset != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseDtdload as i32;
    }
    (*ctxt).validate = get_do_validity_checking_default_value();
    (*ctxt).pedantic = get_pedantic_parser_default_value();
    if (*ctxt).pedantic != 0 {
        (*ctxt).options |= XmlParserOption::XmlParsePedantic as i32;
    }
    (*ctxt).linenumbers = get_line_numbers_default_value();
    (*ctxt).keep_blanks = get_keep_blanks_default_value();
    if (*ctxt).keep_blanks == 0 {
        (*(*ctxt).sax).ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
        (*ctxt).options |= XmlParserOption::XmlParseNoblanks as i32;
    }

    (*ctxt).vctxt.flags = XML_VCTXT_USE_PCTXT as _;
    (*ctxt).vctxt.user_data = Some(GenericErrorContext::new(ctxt));
    (*ctxt).vctxt.error = Some(parser_validity_error);
    (*ctxt).vctxt.warning = Some(parser_validity_warning);
    if (*ctxt).validate != 0 {
        if get_get_warnings_default_value() == 0 {
            (*ctxt).vctxt.warning = None;
        } else {
            (*ctxt).vctxt.warning = Some(parser_validity_warning);
        }
        (*ctxt).vctxt.node_max = 0;
        (*ctxt).options |= XmlParserOption::XmlParseDtdvalid as i32;
    }
    (*ctxt).replace_entities = get_substitute_entities_default_value();
    if (*ctxt).replace_entities != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseNoent as i32;
    }
    (*ctxt).record_info = 0;
    (*ctxt).check_index = 0;
    (*ctxt).in_subset = 0;
    (*ctxt).err_no = XmlParserErrors::XmlErrOK as i32;
    (*ctxt).depth = 0;
    (*ctxt).charset = XmlCharEncoding::UTF8;
    (*ctxt).catalogs = None;
    (*ctxt).sizeentities = 0;
    (*ctxt).sizeentcopy = 0;
    (*ctxt).input_id = 1;
    xml_init_node_info_seq(addr_of_mut!((*ctxt).node_seq));
    0
}

/// Allocate and initialize a new SAX parser context.   
/// If userData is NULL, the parser context will be passed as user data.
///
/// Returns the xmlParserCtxtPtr or NULL if memory allocation failed.
#[doc(alias = "xmlNewSAXParserCtxt")]
pub unsafe fn xml_new_sax_parser_ctxt(
    sax: *const XmlSAXHandler,
    user_data: Option<GenericErrorContext>,
) -> XmlParserCtxtPtr {
    let ctxt: XmlParserCtxtPtr = xml_malloc(size_of::<XmlParserCtxt>()) as XmlParserCtxtPtr;
    if ctxt.is_null() {
        xml_err_memory(null_mut(), Some("cannot allocate parser context\n"));
        return null_mut();
    }
    memset(ctxt as _, 0, size_of::<XmlParserCtxt>());
    std::ptr::write(&mut *ctxt, XmlParserCtxt::default());
    if xml_init_sax_parser_ctxt(ctxt, sax, user_data) < 0 {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }
    ctxt as _
}

/// Initialize a parser context
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "xmlInitParserCtxt")]
pub(crate) unsafe extern "C" fn xml_init_parser_ctxt(ctxt: XmlParserCtxtPtr) -> i32 {
    xml_init_sax_parser_ctxt(ctxt, null_mut(), None)
}

/// Clear (release owned resources) and reinitialize a parser context
#[doc(alias = "xmlClearParserCtxt")]
pub unsafe extern "C" fn xml_clear_parser_ctxt(ctxt: XmlParserCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    xml_clear_node_info_seq(addr_of_mut!((*ctxt).node_seq));
    xml_ctxt_reset(ctxt);
}

/// Free all the memory used by a parser context. However the parsed
/// document in (*ctxt).myDoc is not freed.
#[doc(alias = "xmlFreeParserCtxt")]
pub unsafe extern "C" fn xml_free_parser_ctxt(ctxt: XmlParserCtxtPtr) {
    let mut input: XmlParserInputPtr;

    if ctxt.is_null() {
        return;
    }

    while {
        input = (*ctxt).input_pop();
        !input.is_null()
    } {
        /* Non consuming */
        xml_free_input_stream(input);
    }
    (*ctxt).space_tab.clear();
    (*ctxt).space_tab.shrink_to_fit();
    (*ctxt).name_tab.clear();
    (*ctxt).name_tab.shrink_to_fit();
    (*ctxt).node_tab.clear();
    (*ctxt).node_tab.shrink_to_fit();
    if !(*ctxt).node_info_tab.is_null() {
        xml_free((*ctxt).node_info_tab as _);
    }
    (*ctxt).input_tab.clear();
    (*ctxt).input_tab.shrink_to_fit();
    (*ctxt).version = None;
    (*ctxt).encoding = None;
    (*ctxt).ext_sub_uri = None;
    (*ctxt).ext_sub_system = None;
    #[cfg(feature = "sax1")]
    {
        if !(*ctxt).sax.is_null() && (*ctxt).sax != xml_default_sax_handler() as _ {
            xml_free((*ctxt).sax as _);
        }
    }
    #[cfg(not(feature = "sax1"))]
    {
        if !(*ctxt).sax.is_null() {
            xml_free((*ctxt).sax as _);
        }
    }
    (*ctxt).directory = None;
    if !(*ctxt).vctxt.node_tab.is_null() {
        xml_free((*ctxt).vctxt.node_tab as _);
    }
    if !(*ctxt).dict.is_null() {
        xml_dict_free((*ctxt).dict);
    }
    (*ctxt).ns_tab.clear();
    (*ctxt).ns_tab.shrink_to_fit();
    (*ctxt).push_tab.clear();
    (*ctxt).push_tab.shrink_to_fit();
    if !(*ctxt).attallocs.is_null() {
        xml_free((*ctxt).attallocs as _);
    }
    if let Some(mut table) = (*ctxt).atts_default.take().map(|t| t.into_inner()) {
        table.clear_with(|data, _| xml_free(data as _));
    }
    let _ = (*ctxt).atts_special.take().map(|t| t.into_inner());
    if !(*ctxt).free_elems.is_null() {
        let mut cur = (*ctxt).free_elems;
        while !cur.is_null() {
            let next = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
            xml_free(cur as _);
            cur = next;
        }
    }
    if !(*ctxt).free_attrs.is_null() {
        let mut cur: XmlAttrPtr;
        let mut next: XmlAttrPtr;

        cur = (*ctxt).free_attrs;
        while !cur.is_null() {
            next = (*cur).next;
            xml_free(cur as _);
            cur = next;
        }
    }

    #[cfg(feature = "catalog")]
    {
        (*ctxt).catalogs = None;
    }
    drop_in_place(ctxt);
    xml_free(ctxt as _);
}

/// Setup the parser context to parse a new buffer; Clears any prior
/// contents from the parser context. The buffer parameter must not be
/// NULL, but the filename parameter can be
#[doc(alias = "xmlSetupParserForBuffer")]
#[cfg(feature = "sax1")]
pub(crate) unsafe extern "C" fn xml_setup_parser_for_buffer(
    ctxt: XmlParserCtxtPtr,
    buffer: *const XmlChar,
    filename: *const c_char,
) {
    if ctxt.is_null() || buffer.is_null() {
        return;
    }

    let input: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input.is_null() {
        xml_err_memory(null_mut(), Some("parsing new buffer: out of memory\n"));
        xml_clear_parser_ctxt(ctxt);
        return;
    }

    xml_clear_parser_ctxt(ctxt);
    if !filename.is_null() {
        let canonic = xml_canonic_path(filename as _);
        if !canonic.is_null() {
            (*input).filename = Some(
                CStr::from_ptr(canonic as *const i8)
                    .to_string_lossy()
                    .into_owned(),
            );
            xml_free(canonic as _);
        }
    }
    (*input).base = buffer;
    (*input).cur = buffer;
    (*input).end = buffer.add(xml_strlen(buffer as _) as _);
    (*ctxt).input_push(input);
}

/// Creates a parser context for an XML in-memory document.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateDocParserCtxt")]
pub unsafe extern "C" fn xml_create_doc_parser_ctxt(cur: *const XmlChar) -> XmlParserCtxtPtr {
    if cur.is_null() {
        return null_mut();
    }
    xml_create_memory_parser_ctxt(CStr::from_ptr(cur as *const i8).to_bytes().to_vec())
}

#[cfg(feature = "libxml_legacy")]
const XML_FEATURES_LIST: &[*const c_char] = &[
    c"validate".as_ptr() as _,
    c"load subset".as_ptr() as _,
    c"keep blanks".as_ptr() as _,
    c"disable SAX".as_ptr() as _,
    c"fetch external entities".as_ptr() as _,
    c"substitute entities".as_ptr() as _,
    c"gather line info".as_ptr() as _,
    c"user data".as_ptr() as _,
    c"is html".as_ptr() as _,
    c"is standalone".as_ptr() as _,
    c"stop parser".as_ptr() as _,
    c"document".as_ptr() as _,
    c"is well formed".as_ptr() as _,
    c"is valid".as_ptr() as _,
    c"SAX block".as_ptr() as _,
    c"SAX function internalSubset".as_ptr() as _,
    c"SAX function isStandalone".as_ptr() as _,
    c"SAX function hasInternalSubset".as_ptr() as _,
    c"SAX function hasExternalSubset".as_ptr() as _,
    c"SAX function resolveEntity".as_ptr() as _,
    c"SAX function getEntity".as_ptr() as _,
    c"SAX function entityDecl".as_ptr() as _,
    c"SAX function notationDecl".as_ptr() as _,
    c"SAX function attributeDecl".as_ptr() as _,
    c"SAX function elementDecl".as_ptr() as _,
    c"SAX function unparsedEntityDecl".as_ptr() as _,
    c"SAX function setDocumentLocator".as_ptr() as _,
    c"SAX function startDocument".as_ptr() as _,
    c"SAX function endDocument".as_ptr() as _,
    c"SAX function startElement".as_ptr() as _,
    c"SAX function endElement".as_ptr() as _,
    c"SAX function reference".as_ptr() as _,
    c"SAX function characters".as_ptr() as _,
    c"SAX function ignorableWhitespace".as_ptr() as _,
    c"SAX function processingInstruction".as_ptr() as _,
    c"SAX function comment".as_ptr() as _,
    c"SAX function warning".as_ptr() as _,
    c"SAX function error".as_ptr() as _,
    c"SAX function fatalError".as_ptr() as _,
    c"SAX function getParameterEntity".as_ptr() as _,
    c"SAX function cdataBlock".as_ptr() as _,
    c"SAX function externalSubset".as_ptr() as _,
];

/**
 * xmlGetFeaturesList:
 * @len:  the length of the features name array (input/output)
 * @result:  an array of string to be filled with the features name.
 *
 * Copy at most *@len feature names into the @result array
 *
 * Returns -1 in case or error, or the total number of features,
 *            len is updated with the number of strings copied,
 *            strings must not be deallocated
 */
#[deprecated]
#[cfg(feature = "libxml_legacy")]
pub unsafe extern "C" fn xml_get_features_list(len: *mut i32, result: *mut *const c_char) -> i32 {
    let ret: i32 = XML_FEATURES_LIST.len() as i32;
    if len.is_null() || result.is_null() {
        return ret;
    }
    if *len < 0 || *len >= 1000 {
        return -1;
    }
    if *len > ret {
        *len = ret;
    }
    for i in 0..*len {
        *result.add(i as usize) = XML_FEATURES_LIST[i as usize];
    }
    ret
}

/**
 * xmlGetFeature:
 * @ctxt:  an XML/HTML parser context
 * @name:  the feature name
 * @result:  location to store the result
 *
 * Read the current value of one feature of this parser instance
 *
 * Returns -1 in case or error, 0 otherwise
 */
#[deprecated]
#[cfg(feature = "libxml_legacy")]
pub unsafe extern "C" fn xml_get_feature(
    ctxt: XmlParserCtxtPtr,
    name: *const c_char,
    result: *mut c_void,
) -> i32 {
    if ctxt.is_null() || name.is_null() || result.is_null() {
        return -1;
    }

    if strcmp(name, c"validate".as_ptr() as _) == 0 {
        *(result as *mut i32) = (*ctxt).validate;
    } else if strcmp(name, c"keep blanks".as_ptr() as _) == 0 {
        *(result as *mut i32) = (*ctxt).keep_blanks;
    } else if strcmp(name, c"disable SAX".as_ptr() as _) == 0 {
        *(result as *mut i32) = (*ctxt).disable_sax;
    } else if strcmp(name, c"fetch external entities".as_ptr() as _) == 0 {
        *(result as *mut i32) = (*ctxt).loadsubset;
    } else if strcmp(name, c"substitute entities".as_ptr() as _) == 0 {
        *(result as *mut i32) = (*ctxt).replace_entities;
    } else if strcmp(name, c"gather line info".as_ptr() as _) == 0 {
        *(result as *mut i32) = (*ctxt).record_info;
    } else if strcmp(name, c"user data".as_ptr() as _) == 0 {
        *(result as *mut *mut c_void) = (*ctxt).user_data;
    } else if strcmp(name, c"is html".as_ptr() as _) == 0 {
        *(result as *mut i32) = (*ctxt).html;
    } else if strcmp(name, c"is standalone".as_ptr() as _) == 0 {
        *(result as *mut i32) = (*ctxt).standalone;
    } else if strcmp(name, c"document".as_ptr() as _) == 0 {
        *(result as *mut XmlDocPtr) = (*ctxt).my_doc;
    } else if strcmp(name, c"is well formed".as_ptr() as _) == 0 {
        *(result as *mut i32) = (*ctxt).well_formed;
    } else if strcmp(name, c"is valid".as_ptr() as _) == 0 {
        *(result as *mut i32) = (*ctxt).valid;
    } else if strcmp(name, c"SAX block".as_ptr() as _) == 0 {
        *(result as *mut XmlSAXHandlerPtr) = (*ctxt).sax;
    } else if strcmp(name, c"SAX function internalSubset".as_ptr() as _) == 0 {
        *(result as *mut InternalSubsetSAXFunc) = (*(*ctxt).sax).internal_subset.unwrap();
    } else if strcmp(name, c"SAX function isStandalone".as_ptr() as _) == 0 {
        *(result as *mut IsStandaloneSAXFunc) = (*(*ctxt).sax).is_standalone.unwrap();
    } else if strcmp(name, c"SAX function hasInternalSubset".as_ptr() as _) == 0 {
        *(result as *mut HasInternalSubsetSAXFunc) = (*(*ctxt).sax).has_internal_subset.unwrap();
    } else if strcmp(name, c"SAX function hasExternalSubset".as_ptr() as _) == 0 {
        *(result as *mut HasExternalSubsetSAXFunc) = (*(*ctxt).sax).has_external_subset.unwrap();
    } else if strcmp(name, c"SAX function resolveEntity".as_ptr() as _) == 0 {
        *(result as *mut ResolveEntitySAXFunc) = (*(*ctxt).sax).resolve_entity.unwrap();
    } else if strcmp(name, c"SAX function getEntity".as_ptr() as _) == 0 {
        *(result as *mut GetEntitySAXFunc) = (*(*ctxt).sax).get_entity.unwrap();
    } else if strcmp(name, c"SAX function entityDecl".as_ptr() as _) == 0 {
        *(result as *mut EntityDeclSAXFunc) = (*(*ctxt).sax).entity_decl.unwrap();
    } else if strcmp(name, c"SAX function notationDecl".as_ptr() as _) == 0 {
        *(result as *mut NotationDeclSAXFunc) = (*(*ctxt).sax).notation_decl.unwrap();
    } else if strcmp(name, c"SAX function attributeDecl".as_ptr() as _) == 0 {
        *(result as *mut AttributeDeclSAXFunc) = (*(*ctxt).sax).attribute_decl.unwrap();
    } else if strcmp(name, c"SAX function elementDecl".as_ptr() as _) == 0 {
        *(result as *mut ElementDeclSAXFunc) = (*(*ctxt).sax).element_decl.unwrap();
    } else if strcmp(name, c"SAX function unparsedEntityDecl".as_ptr() as _) == 0 {
        *(result as *mut UnparsedEntityDeclSAXFunc) = (*(*ctxt).sax).unparsed_entity_decl.unwrap();
    } else if strcmp(name, c"SAX function setDocumentLocator".as_ptr() as _) == 0 {
        *(result as *mut SetDocumentLocatorSAXFunc) = (*(*ctxt).sax).set_document_locator.unwrap();
    } else if strcmp(name, c"SAX function startDocument".as_ptr() as _) == 0 {
        *(result as *mut StartDocumentSAXFunc) = (*(*ctxt).sax).start_document.unwrap();
    } else if strcmp(name, c"SAX function endDocument".as_ptr() as _) == 0 {
        *(result as *mut EndDocumentSAXFunc) = (*(*ctxt).sax).end_document.unwrap();
    } else if strcmp(name, c"SAX function startElement".as_ptr() as _) == 0 {
        *(result as *mut StartElementSAXFunc) = (*(*ctxt).sax).start_element.unwrap();
    } else if strcmp(name, c"SAX function endElement".as_ptr() as _) == 0 {
        *(result as *mut EndElementSAXFunc) = (*(*ctxt).sax).end_element.unwrap();
    } else if strcmp(name, c"SAX function reference".as_ptr() as _) == 0 {
        *(result as *mut ReferenceSAXFunc) = (*(*ctxt).sax).reference.unwrap();
    } else if strcmp(name, c"SAX function characters".as_ptr() as _) == 0 {
        *(result as *mut CharactersSAXFunc) = (*(*ctxt).sax).characters.unwrap();
    } else if strcmp(name, c"SAX function ignorableWhitespace".as_ptr() as _) == 0 {
        *(result as *mut IgnorableWhitespaceSAXFunc) = (*(*ctxt).sax).ignorable_whitespace.unwrap();
    } else if strcmp(name, c"SAX function processingInstruction".as_ptr() as _) == 0 {
        *(result as *mut ProcessingInstructionSAXFunc) =
            (*(*ctxt).sax).processing_instruction.unwrap();
    } else if strcmp(name, c"SAX function comment".as_ptr() as _) == 0 {
        *(result as *mut CommentSAXFunc) = (*(*ctxt).sax).comment.unwrap();
    } else if strcmp(name, c"SAX function warning".as_ptr() as _) == 0 {
        *(result as *mut WarningSAXFunc) = (*(*ctxt).sax).warning.unwrap();
    } else if strcmp(name, c"SAX function error".as_ptr() as _) == 0 {
        *(result as *mut ErrorSAXFunc) = (*(*ctxt).sax).error.unwrap();
    } else if strcmp(name, c"SAX function fatalError".as_ptr() as _) == 0 {
        *(result as *mut FatalErrorSAXFunc) = (*(*ctxt).sax).fatal_error.unwrap();
    } else if strcmp(name, c"SAX function getParameterEntity".as_ptr() as _) == 0 {
        *(result as *mut GetParameterEntitySAXFunc) = (*(*ctxt).sax).get_parameter_entity.unwrap();
    } else if strcmp(name, c"SAX function cdataBlock".as_ptr() as _) == 0 {
        *(result as *mut CDATABlockSAXFunc) = (*(*ctxt).sax).cdata_block.unwrap();
    } else if strcmp(name, c"SAX function externalSubset".as_ptr() as _) == 0 {
        *(result as *mut ExternalSubsetSAXFunc) = (*(*ctxt).sax).external_subset.unwrap();
    } else {
        return -1;
    }
    0
}

/**
 * xmlSetFeature:
 * @ctxt:  an XML/HTML parser context
 * @name:  the feature name
 * @value:  pointer to the location of the new value
 *
 * Change the current value of one feature of this parser instance
 *
 * Returns -1 in case or error, 0 otherwise
 */
#[deprecated]
#[cfg(feature = "libxml_legacy")]
pub unsafe extern "C" fn xml_set_feature(
    ctxt: XmlParserCtxtPtr,
    name: *const c_char,
    value: *mut c_void,
) -> i32 {
    if ctxt.is_null() || name.is_null() || value.is_null() {
        return -1;
    }

    if strcmp(name, c"validate".as_ptr() as _) == 0 {
        let newvalidate: i32 = *(value as *mut i32);

        if (*ctxt).validate == 0 && newvalidate != 0 {
            if (*ctxt).vctxt.warning.is_none() {
                (*ctxt).vctxt.warning = Some(xml_parser_validity_warning);
            }
            if (*ctxt).vctxt.error.is_none() {
                (*ctxt).vctxt.error = Some(xml_parser_validity_error);
            }
            (*ctxt).vctxt.node_max = 0;
        }
        (*ctxt).validate = newvalidate;
    } else if strcmp(name, c"keep blanks".as_ptr() as _) == 0 {
        (*ctxt).keep_blanks = *(value as *mut i32);
    } else if strcmp(name, c"disable SAX".as_ptr() as _) == 0 {
        (*ctxt).disable_sax = *(value as *mut i32);
    } else if strcmp(name, c"fetch external entities".as_ptr() as _) == 0 {
        (*ctxt).loadsubset = *(value as *mut i32);
    } else if strcmp(name, c"substitute entities".as_ptr() as _) == 0 {
        (*ctxt).replace_entities = *(value as *mut i32);
    } else if strcmp(name, c"gather line info".as_ptr() as _) == 0 {
        (*ctxt).record_info = *(value as *mut i32);
    } else if strcmp(name, c"user data".as_ptr() as _) == 0 {
        (*ctxt).user_data = *(value as *mut *mut c_void);
    } else if strcmp(name, c"is html".as_ptr() as _) == 0 {
        (*ctxt).html = *(value as *mut i32);
    } else if strcmp(name, c"is standalone".as_ptr() as _) == 0 {
        (*ctxt).standalone = *(value as *mut i32);
    } else if strcmp(name, c"document".as_ptr() as _) == 0 {
        (*ctxt).my_doc = *(value as *mut XmlDocPtr);
    } else if strcmp(name, c"is well formed".as_ptr() as _) == 0 {
        (*ctxt).well_formed = *(value as *mut i32);
    } else if strcmp(name, c"is valid".as_ptr() as _) == 0 {
        (*ctxt).valid = *(value as *mut i32);
    } else if strcmp(name, c"SAX block".as_ptr() as _) == 0 {
        (*ctxt).sax = *(value as *mut XmlSAXHandlerPtr);
    } else if strcmp(name, c"SAX function internalSubset".as_ptr() as _) == 0 {
        (*(*ctxt).sax).internal_subset = Some(*(value as *mut InternalSubsetSAXFunc));
    } else if strcmp(name, c"SAX function isStandalone".as_ptr() as _) == 0 {
        (*(*ctxt).sax).is_standalone = Some(*(value as *mut IsStandaloneSAXFunc));
    } else if strcmp(name, c"SAX function hasInternalSubset".as_ptr() as _) == 0 {
        (*(*ctxt).sax).has_internal_subset = Some(*(value as *mut HasInternalSubsetSAXFunc));
    } else if strcmp(name, c"SAX function hasExternalSubset".as_ptr() as _) == 0 {
        (*(*ctxt).sax).has_external_subset = Some(*(value as *mut HasExternalSubsetSAXFunc));
    } else if strcmp(name, c"SAX function resolveEntity".as_ptr() as _) == 0 {
        (*(*ctxt).sax).resolve_entity = Some(*(value as *mut ResolveEntitySAXFunc));
    } else if strcmp(name, c"SAX function getEntity".as_ptr() as _) == 0 {
        (*(*ctxt).sax).get_entity = Some(*(value as *mut GetEntitySAXFunc));
    } else if strcmp(name, c"SAX function entityDecl".as_ptr() as _) == 0 {
        (*(*ctxt).sax).entity_decl = Some(*(value as *mut EntityDeclSAXFunc));
    } else if strcmp(name, c"SAX function notationDecl".as_ptr() as _) == 0 {
        (*(*ctxt).sax).notation_decl = Some(*(value as *mut NotationDeclSAXFunc));
    } else if strcmp(name, c"SAX function attributeDecl".as_ptr() as _) == 0 {
        (*(*ctxt).sax).attribute_decl = Some(*(value as *mut AttributeDeclSAXFunc));
    } else if strcmp(name, c"SAX function elementDecl".as_ptr() as _) == 0 {
        (*(*ctxt).sax).element_decl = Some(*(value as *mut ElementDeclSAXFunc));
    } else if strcmp(name, c"SAX function unparsedEntityDecl".as_ptr() as _) == 0 {
        (*(*ctxt).sax).unparsed_entity_decl = Some(*(value as *mut UnparsedEntityDeclSAXFunc));
    } else if strcmp(name, c"SAX function setDocumentLocator".as_ptr() as _) == 0 {
        (*(*ctxt).sax).set_document_locator = Some(*(value as *mut SetDocumentLocatorSAXFunc));
    } else if strcmp(name, c"SAX function startDocument".as_ptr() as _) == 0 {
        (*(*ctxt).sax).start_document = Some(*(value as *mut StartDocumentSAXFunc));
    } else if strcmp(name, c"SAX function endDocument".as_ptr() as _) == 0 {
        (*(*ctxt).sax).end_document = Some(*(value as *mut EndDocumentSAXFunc));
    } else if strcmp(name, c"SAX function startElement".as_ptr() as _) == 0 {
        (*(*ctxt).sax).start_element = Some(*(value as *mut StartElementSAXFunc));
    } else if strcmp(name, c"SAX function endElement".as_ptr() as _) == 0 {
        (*(*ctxt).sax).end_element = Some(*(value as *mut EndElementSAXFunc));
    } else if strcmp(name, c"SAX function reference".as_ptr() as _) == 0 {
        (*(*ctxt).sax).reference = Some(*(value as *mut ReferenceSAXFunc));
    } else if strcmp(name, c"SAX function characters".as_ptr() as _) == 0 {
        (*(*ctxt).sax).characters = Some(*(value as *mut CharactersSAXFunc));
    } else if strcmp(name, c"SAX function ignorableWhitespace".as_ptr() as _) == 0 {
        (*(*ctxt).sax).ignorable_whitespace = Some(*(value as *mut IgnorableWhitespaceSAXFunc));
    } else if strcmp(name, c"SAX function processingInstruction".as_ptr() as _) == 0 {
        (*(*ctxt).sax).processing_instruction = Some(*(value as *mut ProcessingInstructionSAXFunc));
    } else if strcmp(name, c"SAX function comment".as_ptr() as _) == 0 {
        (*(*ctxt).sax).comment = Some(*(value as *mut CommentSAXFunc));
    } else if strcmp(name, c"SAX function warning".as_ptr() as _) == 0 {
        (*(*ctxt).sax).warning = Some(*(value as *mut WarningSAXFunc));
    } else if strcmp(name, c"SAX function error".as_ptr() as _) == 0 {
        (*(*ctxt).sax).error = Some(*(value as *mut ErrorSAXFunc));
    } else if strcmp(name, c"SAX function fatalError".as_ptr() as _) == 0 {
        (*(*ctxt).sax).fatal_error = Some(*(value as *mut FatalErrorSAXFunc));
    } else if strcmp(name, c"SAX function getParameterEntity".as_ptr() as _) == 0 {
        (*(*ctxt).sax).get_parameter_entity = Some(*(value as *mut GetParameterEntitySAXFunc));
    } else if strcmp(name, c"SAX function cdataBlock".as_ptr() as _) == 0 {
        (*(*ctxt).sax).cdata_block = Some(*(value as *mut CDATABlockSAXFunc));
    } else if strcmp(name, c"SAX function externalSubset".as_ptr() as _) == 0 {
        (*(*ctxt).sax).external_subset = Some(*(value as *mut ExternalSubsetSAXFunc));
    } else {
        return -1;
    }
    0
}

/// Create a parser context for using the XML parser in push mode.
/// If @buffer and @size are non-NULL, the data is used to detect
/// the encoding.  The remaining characters will be parsed so they
/// don't need to be fed in again through xmlParseChunk.
/// To allow content encoding detection, @size should be >= 4
/// The value of @filename is used for fetching external entities
/// and error/warning reports.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreatePushParserCtxt")]
#[cfg(feature = "libxml_push")]
pub unsafe fn xml_create_push_parser_ctxt(
    sax: XmlSAXHandlerPtr,
    user_data: Option<GenericErrorContext>,
    chunk: *const c_char,
    size: i32,
    filename: *const c_char,
) -> XmlParserCtxtPtr {
    use crate::io::xml_parser_get_directory;

    let buf = Rc::new(RefCell::new(XmlParserInputBuffer::new(
        XmlCharEncoding::None,
    )));

    let ctxt: XmlParserCtxtPtr = xml_new_sax_parser_ctxt(sax, user_data);
    if ctxt.is_null() {
        xml_err_memory(null_mut(), Some("creating parser: out of memory\n"));
        return null_mut();
    }
    (*ctxt).dict_names = 1;
    if filename.is_null() {
        (*ctxt).directory = None;
    } else if let Some(dir) =
        xml_parser_get_directory(CStr::from_ptr(filename).to_string_lossy().as_ref())
    {
        (*ctxt).directory = Some(dir.to_string_lossy().into_owned());
    }

    let input_stream: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input_stream.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    if filename.is_null() {
        (*input_stream).filename = None;
    } else {
        let canonic = xml_canonic_path(filename as _);
        if canonic.is_null() {
            xml_free_input_stream(input_stream);
            xml_free_parser_ctxt(ctxt);
            return null_mut();
        }
        (*input_stream).filename = Some(
            CStr::from_ptr(canonic as *const i8)
                .to_string_lossy()
                .into_owned(),
        );
        xml_free(canonic as _);
    }
    (*input_stream).buf = Some(buf);
    (*input_stream).reset_base();
    (*ctxt).input_push(input_stream);

    /*
     * If the caller didn't provide an initial 'chunk' for determining
     * the encoding, we set the context to xmlCharEncoding::XML_CHAR_ENCODING_NONE so
     * that it can be automatically determined later
     */
    (*ctxt).charset = XmlCharEncoding::None;

    if size != 0 && !chunk.is_null() && !(*ctxt).input.is_null() && (*(*ctxt).input).buf.is_some() {
        let base: size_t = (*(*ctxt).input).get_base();
        let cur: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

        (*(*ctxt).input)
            .buf
            .as_mut()
            .unwrap()
            .borrow_mut()
            .push_bytes(from_raw_parts(chunk as *const u8, size as usize));
        (*(*ctxt).input).set_base_and_cursor(base, cur);
    }

    ctxt
}

const XML_PARSER_BIG_BUFFER_SIZE: usize = 300;
const XML_PARSER_BUFFER_SIZE: usize = 100;

/// Check whether the input buffer contains a string.
#[doc(alias = "xmlParseLookupString")]
unsafe extern "C" fn xml_parse_lookup_string(
    ctxt: XmlParserCtxtPtr,
    start_delta: size_t,
    str: *const c_char,
    str_len: size_t,
) -> *const XmlChar {
    let cur = if (*ctxt).check_index == 0 {
        (*(*ctxt).input).cur.add(start_delta)
    } else {
        (*(*ctxt).input).cur.add((*ctxt).check_index as usize)
    };

    let term: *const XmlChar = strstr(cur as _, str) as _;
    if term.is_null() {
        let mut end: *const XmlChar = (*(*ctxt).input).end;

        /* Rescan (strLen - 1) characters. */
        if end.offset_from(cur) < str_len as isize {
            end = cur;
        } else {
            end = end.sub(str_len - 1);
        }
        let index: size_t = end.offset_from((*(*ctxt).input).cur) as _;
        if index > i64::MAX as usize {
            (*ctxt).check_index = 0;
            return (*(*ctxt).input).end.sub(str_len);
        }
        (*ctxt).check_index = index as _;
    } else {
        (*ctxt).check_index = 0;
    }

    term
}

/// Check whether there's enough data in the input buffer to finish parsing
/// a start tag. This has to take quotes into account.
#[doc(alias = "xmlParseLookupGt")]
unsafe extern "C" fn xml_parse_lookup_gt(ctxt: XmlParserCtxtPtr) -> i32 {
    let mut cur: *const XmlChar;
    let end: *const XmlChar = (*(*ctxt).input).end;
    let mut state: i32 = (*ctxt).end_check_state;

    if (*ctxt).check_index == 0 {
        cur = (*(*ctxt).input).cur.add(1);
    } else {
        cur = (*(*ctxt).input).cur.add((*ctxt).check_index as usize);
    }

    while cur < end {
        if state != 0 {
            if *cur == state as u8 {
                state = 0;
            }
        } else if *cur == b'\'' || *cur == b'"' {
            state = *cur as _;
        } else if *cur == b'>' {
            (*ctxt).check_index = 0;
            (*ctxt).end_check_state = 0;
            return 1;
        }
        cur = cur.add(1);
    }

    let index: size_t = cur.offset_from((*(*ctxt).input).cur) as _;
    if index > i64::MAX as usize {
        (*ctxt).check_index = 0;
        (*ctxt).end_check_state = 0;
        return 1;
    }
    (*ctxt).check_index = index as _;
    (*ctxt).end_check_state = state;
    0
}

// The two following functions are related to the change of accepted
// characters for Name and NmToken in the Revision 5 of XML-1.0
// They correspond to the modified production [4] and the new production [4a]
// changes in that revision. Also note that the macros used for the
// productions Letter, Digit, CombiningChar and Extender are not needed
// anymore.
// We still keep compatibility to pre-revision5 parsing semantic if the
// new XML_PARSE_OLD10 option is given to the parser.
unsafe extern "C" fn xml_is_name_start_char(ctxt: XmlParserCtxtPtr, c: i32) -> i32 {
    if (*ctxt).options & XmlParserOption::XmlParseOld10 as i32 == 0 {
        /*
         * Use the new checks of production [4] [4a] amd [5] of the
         * Update 5 of XML-1.0
         */
        if c != b' ' as i32
            && c != b'>' as i32
            && c != b'/' as i32  /* accelerators */
            && ((c >= b'a' as i32 && c <= b'z' as i32)
                || (c >= b'A' as i32 && c <= b'Z' as i32)
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
                || (0x10000..=0xEFFFF).contains(&c))
        {
            return 1;
        }
    } else if xml_is_letter(c as u32) || c == b'_' as i32 || c == b':' as i32 {
        return 1;
    }
    0
}

unsafe extern "C" fn xml_parse_ncname_complex(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
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
    let start_position: size_t = (*ctxt).current_ptr().offset_from((*ctxt).base_ptr()) as _;
    let Some(mut c) = (*ctxt).current_char(&mut l) else {
        return null_mut();
    };
    if c == ' '
        || c == '>'
        || c == '/' /* accelerators */
        || (xml_is_name_start_char(ctxt, c as i32) == 0 || c == ':')
    {
        return null_mut();
    }

    while c != ' '
        && c != '>'
        && c != '/' /* test bigname.xml */
        && (xml_is_name_char(ctxt, c as i32) != 0 && c != ':')
    {
        if len <= i32::MAX - l {
            len += l;
        }
        (*ctxt).advance_with_line_handling(l as usize);
        c = (*ctxt).current_char(&mut l).unwrap_or('\0');
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }
    if len > max_length {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("NCName"));
        return null_mut();
    }
    xml_dict_lookup((*ctxt).dict, (*ctxt).base_ptr().add(start_position), len)
}

/// Parse an XML name.
///
/// `[4NS] NCNameChar ::= Letter | Digit | '.' | '-' | '_' | CombiningChar | Extender`
///
/// `[5NS] NCName ::= (Letter | '_') (NCNameChar)*`
///
/// Returns the Name parsed or NULL
#[doc(alias = "xmlParseNCName")]
unsafe extern "C" fn xml_parse_ncname(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    let mut input: *const XmlChar;

    let ret: *const XmlChar;
    let count: size_t;
    let max_length: size_t = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH
    } else {
        XML_MAX_NAME_LENGTH
    };

    /*
     * Accelerator for simple ASCII names
     */
    input = (*(*ctxt).input).cur;
    let e: *const XmlChar = (*(*ctxt).input).end;
    if ((*input >= 0x61 && *input <= 0x7A) || (*input >= 0x41 && *input <= 0x5A) || *input == b'_')
        && input < e
    {
        input = input.add(1);
        while ((*input >= 0x61 && *input <= 0x7A)
            || (*input >= 0x41 && *input <= 0x5A)
            || (*input >= 0x30 && *input <= 0x39)
            || *input == b'_'
            || *input == b'-'
            || *input == b'.')
            && input < e
        {
            input = input.add(1);
        }
        if input >= e {
            return xml_parse_ncname_complex(ctxt);
        }
        if *input > 0 && *input < 0x80 {
            count = input.offset_from((*(*ctxt).input).cur) as _;
            if count > max_length {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("NCName"));
                return null_mut();
            }
            ret = xml_dict_lookup((*ctxt).dict, (*(*ctxt).input).cur, count as _);
            (*(*ctxt).input).cur = input;
            (*(*ctxt).input).col += count as i32;
            if ret.is_null() {
                xml_err_memory(ctxt, None);
            }
            return ret;
        }
    }
    xml_parse_ncname_complex(ctxt)
}

/// Parse an XML Namespace QName
///
/// `[6]  QName  ::= (Prefix ':')? LocalPart`
/// `[7]  Prefix  ::= NCName`
/// `[8]  LocalPart  ::= NCName`
///
/// Returns the Name parsed or NULL
#[doc(alias = "xmlParseQName")]
unsafe extern "C" fn xml_parse_qname(
    ctxt: XmlParserCtxtPtr,
    prefix: *mut *const XmlChar,
) -> *const XmlChar {
    let mut l: *const XmlChar;
    let mut p: *const XmlChar;

    (*ctxt).grow();
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }

    l = xml_parse_ncname(ctxt);
    if l.is_null() {
        if (*ctxt).current_byte() == b':' {
            l = xml_parse_name(ctxt);
            if !l.is_null() {
                xml_ns_err!(
                    ctxt,
                    XmlParserErrors::XmlNsErrQname,
                    "Failed to parse QName '{}'\n",
                    CStr::from_ptr(l as *const i8).to_string_lossy()
                );
                *prefix = null_mut();
                return l;
            }
        }
        return null_mut();
    }
    if (*ctxt).current_byte() == b':' {
        (*ctxt).skip_char();
        p = l;
        l = xml_parse_ncname(ctxt);
        if l.is_null() {
            let tmp: *mut XmlChar;

            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                return null_mut();
            }
            xml_ns_err!(
                ctxt,
                XmlParserErrors::XmlNsErrQname,
                "Failed to parse QName '{}:'\n",
                CStr::from_ptr(p as *const i8).to_string_lossy()
            );
            l = xml_parse_nmtoken(ctxt);
            if l.is_null() {
                if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    return null_mut();
                }
                tmp = xml_build_qname(c"".as_ptr() as _, p, null_mut(), 0);
            } else {
                tmp = xml_build_qname(l, p, null_mut(), 0);
                xml_free(l as _);
            }
            p = xml_dict_lookup((*ctxt).dict, tmp, -1);
            if !tmp.is_null() {
                xml_free(tmp as _);
            }
            *prefix = null_mut();
            return p;
        }
        if (*ctxt).current_byte() == b':' {
            let mut tmp: *mut XmlChar;

            xml_ns_err!(
                ctxt,
                XmlParserErrors::XmlNsErrQname,
                "Failed to parse QName '{}:{}:'\n",
                CStr::from_ptr(p as *const i8).to_string_lossy(),
                CStr::from_ptr(l as *const i8).to_string_lossy()
            );
            (*ctxt).skip_char();
            tmp = xml_parse_name(ctxt) as _;
            if !tmp.is_null() {
                tmp = xml_build_qname(tmp, l, null_mut(), 0);
                l = xml_dict_lookup((*ctxt).dict, tmp, -1);
                if !tmp.is_null() {
                    xml_free(tmp as _);
                }
                *prefix = p;
                return l;
            }
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                return null_mut();
            }
            tmp = xml_build_qname(c"".as_ptr() as _, l, null_mut(), 0);
            l = xml_dict_lookup((*ctxt).dict, tmp, -1);
            if !tmp.is_null() {
                xml_free(tmp as _);
            }
            *prefix = p;
            return l;
        }
        *prefix = p;
    } else {
        *prefix = null_mut();
    }
    l
}

const XML_PARSER_BIG_ENTITY: usize = 1000;
const XML_PARSER_LOT_ENTITY: usize = 5000;

// XML_PARSER_NON_LINEAR is roughly the maximum allowed amplification factor
// of serialized output after entity expansion.
const XML_PARSER_NON_LINEAR: usize = 5;

// A certain amount is always allowed.
const XML_PARSER_ALLOWED_EXPANSION: usize = 1000000;

// Fixed cost for each entity reference. This crudely models processing time
// as well to protect, for example, against exponential expansion of empty
// or very short entities.
const XML_ENT_FIXED_COST: usize = 20;

/// Check for non-linear entity expansion behaviour.
///
/// In some cases like xmlStringDecodeEntities, this function is called
/// for each, possibly nested entity and its unexpanded content length.
///
/// In other cases like xmlParseReference, it's only called for each
/// top-level entity with its unexpanded content length plus the sum of
/// the unexpanded content lengths (plus fixed cost) of all nested
/// entities.
///
/// Summing the unexpanded lengths also adds the length of the reference.
/// This is by design. Taking the length of the entity name into account
/// discourages attacks that try to waste CPU time with abusively long
/// entity names. See test/recurse/lol6.xml for example. Each call also
/// adds some fixed cost XML_ENT_FIXED_COST to discourage attacks with
/// short entities.
///
/// Returns 1 on error, 0 on success.
#[doc(alias = "xmlParserEntityCheck")]
pub(crate) unsafe extern "C" fn xml_parser_entity_check(ctxt: XmlParserCtxtPtr, extra: u64) -> i32 {
    let mut consumed: u64;
    let input: XmlParserInputPtr = (*ctxt).input;
    let entity: XmlEntityPtr = (*input).entity;

    // Compute total consumed bytes so far, including input streams of external entities.
    consumed = (*input).parent_consumed;
    if entity.is_null()
        || (matches!((*entity).etype, XmlEntityType::XmlExternalParameterEntity)
            && (*entity).flags & XML_ENT_PARSED as i32 == 0)
    {
        consumed = consumed.saturating_add((*input).consumed);
        consumed = consumed.saturating_add((*input).cur.offset_from((*input).base) as _);
    }
    consumed = consumed.saturating_add((*ctxt).sizeentities);

    // Add extra cost and some fixed cost.
    (*ctxt).sizeentcopy = (*ctxt).sizeentcopy.saturating_add(extra);
    (*ctxt).sizeentcopy = (*ctxt).sizeentcopy.saturating_add(XML_ENT_FIXED_COST as _);

    // It's important to always use saturation arithmetic when tracking
    // entity sizes to make the size checks reliable. If "sizeentcopy"
    // overflows, we have to abort.
    if (*ctxt).sizeentcopy > XML_PARSER_ALLOWED_EXPANSION as u64
        && ((*ctxt).sizeentcopy == u64::MAX
            || (*ctxt).sizeentcopy / XML_PARSER_NON_LINEAR as u64 > consumed)
    {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrEntityLoop,
            "Maximum entity amplification factor exceeded",
        );
        (*ctxt).halt();
        return 1;
    }

    0
}

/// Parse Reference declarations, variant parsing from a string rather
/// than an an input flow.
///
/// `[66] CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'`
///
/// `[ WFC: Legal Character ]`  
/// Characters referred to using character references must match the production for Char.
///
/// Returns the value parsed (as an int), 0 in case of error, str will be
/// updated to the current value of the index
#[doc(alias = "xmlParseStringCharRef")]
unsafe extern "C" fn xml_parse_string_char_ref(
    ctxt: XmlParserCtxtPtr,
    str: *mut *const XmlChar,
) -> i32 {
    let mut ptr: *const XmlChar;
    let mut cur: XmlChar;
    let mut val: i32 = 0;

    if str.is_null() || (*str).is_null() {
        return 0;
    }
    ptr = *str;
    cur = *ptr;
    if cur == b'&' && *ptr.add(1) == b'#' && *ptr.add(2) == b'x' {
        ptr = ptr.add(3);
        cur = *ptr;
        while cur != b';' {
            /* Non input consuming loop */
            if cur.is_ascii_digit() {
                val = val * 16 + (cur - b'0') as i32;
            } else if (b'a'..=b'f').contains(&cur) {
                val = val * 16 + (cur - b'a') as i32 + 10;
            } else if (b'A'..=b'F').contains(&cur) {
                val = val * 16 + (cur - b'A') as i32 + 10;
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidHexCharRef, None);
                val = 0;
                break;
            }
            if val > 0x110000 {
                val = 0x110000;
            }

            ptr = ptr.add(1);
            cur = *ptr;
        }
        if cur == b';' {
            ptr = ptr.add(1);
        }
    } else if cur == b'&' && *ptr.add(1) == b'#' {
        ptr = ptr.add(2);
        cur = *ptr;
        while cur != b';' {
            /* Non input consuming loops */
            if cur.is_ascii_digit() {
                val = val * 10 + (cur - b'0') as i32;
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidDecCharRef, None);
                val = 0;
                break;
            }
            if val > 0x110000 {
                val = 0x110000;
            }

            ptr = ptr.add(1);
            cur = *ptr;
        }
        if cur == b';' {
            ptr = ptr.add(1);
        }
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidCharRef, None);
        return 0;
    }
    *str = ptr;

    /*
     * [ WFC: Legal Character ]
     * Characters referred to using character references must match the
     * production for Char.
     */
    if val >= 0x110000 {
        xml_fatal_err_msg_int!(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            "xmlParseStringCharRef: character reference out of bounds\n",
            val
        );
    } else if xml_is_char(val as u32) {
        return val;
    } else {
        xml_fatal_err_msg_int!(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            format!("xmlParseStringCharRef: invalid xmlChar value {val}\n").as_str(),
            val
        );
    }
    0
}

/// Parse an XML name.
///
/// `[4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender`
///
/// `[5] Name ::= (Letter | '_' | ':') (NameChar)*`
///
/// `[6] Names ::= Name (#x20 Name)*`
///
/// Returns the Name parsed or NULL. The @str pointer is updated to the current location in the string.
#[doc(alias = "xmlParseStringName")]
pub(crate) unsafe extern "C" fn xml_parse_string_name(
    ctxt: XmlParserCtxtPtr,
    str: *mut *const XmlChar,
) -> *mut XmlChar {
    let mut buf: [XmlChar; XML_MAX_NAMELEN + 5] = [0; XML_MAX_NAMELEN + 5];
    let mut cur: *const XmlChar = *str;
    let mut len: i32 = 0;
    let mut l: i32 = 0;
    let mut c: i32;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };

    c = CUR_SCHAR!(ctxt, cur, l);
    if xml_is_name_start_char(ctxt, c) == 0 {
        return null_mut();
    }

    COPY_BUF!(l, buf.as_mut_ptr(), len, c);
    cur = cur.add(l as usize);
    c = CUR_SCHAR!(ctxt, cur, l);
    while xml_is_name_char(ctxt, c) != 0 {
        COPY_BUF!(l, buf.as_mut_ptr(), len, c);
        cur = cur.add(l as usize);
        c = CUR_SCHAR!(ctxt, cur, l);
        if len >= XML_MAX_NAMELEN as i32 {
            /* test bigentname.xml */
            /*
             * Okay someone managed to make a huge name, so he's ready to pay
             * for the processing speed.
             */
            let mut buffer: *mut XmlChar;
            let mut max: i32 = len * 2;

            buffer = xml_malloc_atomic(max as usize) as _;
            if buffer.is_null() {
                xml_err_memory(ctxt, None);
                return null_mut();
            }
            memcpy(buffer as _, buf.as_ptr() as _, len as _);
            while xml_is_name_char(ctxt, c) != 0 {
                if len + 10 > max {
                    max *= 2;
                    let tmp: *mut XmlChar = xml_realloc(buffer as _, max as usize) as _;
                    if tmp.is_null() {
                        xml_err_memory(ctxt, None);
                        xml_free(buffer as _);
                        return null_mut();
                    }
                    buffer = tmp;
                }
                COPY_BUF!(l, buffer, len, c);
                cur = cur.add(l as usize);
                c = CUR_SCHAR!(ctxt, cur, l);
                if len > max_length {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("NCName"));
                    xml_free(buffer as _);
                    return null_mut();
                }
            }
            *buffer.add(len as usize) = 0;
            *str = cur;
            return buffer;
        }
    }
    if len > max_length {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("NCName"));
        return null_mut();
    }
    *str = cur;
    xml_strndup(buf.as_ptr() as _, len)
}

/// Parse ENTITY references declarations, but this version parses it from
/// a string value.
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
/// Returns the xmlEntityPtr if found, or NULL otherwise. The str pointer
/// is updated to the current location in the string.
#[doc(alias = "xmlParseStringEntityRef")]
unsafe fn xml_parse_string_entity_ref(
    ctxt: XmlParserCtxtPtr,
    str: *mut *const XmlChar,
) -> XmlEntityPtr {
    let mut ptr: *const XmlChar;
    let mut ent: XmlEntityPtr = null_mut();

    if str.is_null() || (*str).is_null() {
        return null_mut();
    }
    ptr = *str;
    let cur: XmlChar = *ptr;
    if cur != b'&' {
        return null_mut();
    }

    ptr = ptr.add(1);
    let name: *mut XmlChar = xml_parse_string_name(ctxt, addr_of_mut!(ptr));
    if name.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            "xmlParseStringEntityRef: no name\n",
        );
        *str = ptr;
        return null_mut();
    }
    let name = {
        let n = CStr::from_ptr(name as *const i8)
            .to_string_lossy()
            .into_owned();
        xml_free(name as _);
        n
    };
    if *ptr != b';' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityRefSemicolMissing, None);
        *str = ptr;
        return null_mut();
    }
    ptr = ptr.add(1);

    // Predefined entities override any extra definition
    if (*ctxt).options & XmlParserOption::XmlParseOldsax as i32 == 0 {
        ent = xml_get_predefined_entity(&name);
        if !ent.is_null() {
            *str = ptr;
            return ent;
        }
    }

    // Ask first SAX for entity resolution, otherwise try the
    // entities which may have stored in the parser context.
    if !(*ctxt).sax.is_null() {
        if let Some(get_entity) = (*(*ctxt).sax).get_entity {
            ent = get_entity((*ctxt).user_data.clone(), &name);
        }
        if ent.is_null() && (*ctxt).options & XmlParserOption::XmlParseOldsax as i32 != 0 {
            ent = xml_get_predefined_entity(&name);
        }
        if ent.is_null()
            && (*ctxt)
                .user_data
                .as_ref()
                .and_then(|d| d.lock().downcast_ref::<XmlParserCtxtPtr>().copied())
                == Some(ctxt)
        {
            ent = xml_sax2_get_entity(Some(GenericErrorContext::new(ctxt)) as _, &name);
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
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
    if ent.is_null() {
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
        }
    // TODO ? check regressions (*ctxt).valid = 0;
    }
    // [ WFC: Parsed Entity ]
    // An entity reference must not contain the name of an
    // unparsed entity
    else if matches!(
        (*ent).etype,
        XmlEntityType::XmlExternalGeneralUnparsedEntity
    ) {
        xml_fatal_err_msg_str!(
            ctxt,
            XmlParserErrors::XmlErrUnparsedEntity,
            "Entity reference to unparsed entity {}\n",
            name
        );
    }
    // [ WFC: No External Entity References ]
    // Attribute values cannot contain direct or indirect
    // entity references to external entities.
    else if matches!(
        (*ctxt).instate,
        XmlParserInputState::XmlParserAttributeValue
    ) && matches!((*ent).etype, XmlEntityType::XmlExternalGeneralParsedEntity)
    {
        xml_fatal_err_msg_str!(
            ctxt,
            XmlParserErrors::XmlErrEntityIsExternal,
            "Attribute references external entity '{}'\n",
            name
        );
    }
    // [ WFC: No < in Attribute Values ]
    // The replacement text of any entity referred to directly or
    // indirectly in an attribute value (other than "&lt;") must
    // not contain a <.
    else if matches!(
        (*ctxt).instate,
        XmlParserInputState::XmlParserAttributeValue
    ) && !matches!((*ent).etype, XmlEntityType::XmlInternalPredefinedEntity)
    {
        if (*ent).flags & XML_ENT_CHECKED_LT as i32 == 0 {
            if !(*ent).content.load(Ordering::Relaxed).is_null()
                && !xml_strchr((*ent).content.load(Ordering::Relaxed), b'<').is_null()
            {
                (*ent).flags |= XML_ENT_CONTAINS_LT as i32;
            }
            (*ent).flags |= XML_ENT_CHECKED_LT as i32;
        }
        if (*ent).flags & XML_ENT_CONTAINS_LT as i32 != 0 {
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrLtInAttribute,
                "'<' in entity '{}' is not allowed in attributes values\n",
                name
            );
        }
    }
    // Internal check, no parameter entities here ...
    else {
        match (*ent).etype {
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

    // [ WFC: No Recursion ]
    // A parsed entity must not contain a recursive reference
    // to itself, either directly or indirectly.
    // Done somewhere else

    *str = ptr;
    ent
}

/// Parse PEReference declarations
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
///
/// Returns the string of the entity content.
/// str is updated to the current value of the index
#[doc(alias = "xmlParseStringPEReference")]
unsafe extern "C" fn xml_parse_string_pereference(
    ctxt: XmlParserCtxtPtr,
    str: *mut *const XmlChar,
) -> XmlEntityPtr {
    let mut ptr: *const XmlChar;
    let mut cur: XmlChar;
    let mut entity: XmlEntityPtr = null_mut();

    if str.is_null() || (*str).is_null() {
        return null_mut();
    }
    ptr = *str;
    cur = *ptr;
    if cur != b'%' {
        return null_mut();
    }
    ptr = ptr.add(1);
    let name: *mut XmlChar = xml_parse_string_name(ctxt, addr_of_mut!(ptr));
    if name.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            "xmlParseStringPEReference: no name\n",
        );
        *str = ptr;
        return null_mut();
    }
    let name = {
        let n = CStr::from_ptr(name as *const i8)
            .to_string_lossy()
            .into_owned();
        xml_free(name as _);
        n
    };
    cur = *ptr;
    if cur != b';' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityRefSemicolMissing, None);
        *str = ptr;
        return null_mut();
    }
    ptr = ptr.add(1);

    // Request the entity from SAX
    if !(*ctxt).sax.is_null() && (*(*ctxt).sax).get_parameter_entity.is_some() {
        entity = ((*(*ctxt).sax).get_parameter_entity.unwrap())((*ctxt).user_data.clone(), &name);
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        *str = ptr;
        return null_mut();
    }
    if entity.is_null() {
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
            xml_warning_msg!(
                ctxt,
                XmlParserErrors::XmlWarUndeclaredEntity,
                "PEReference: %{}; not found\n",
                name
            );
            (*ctxt).valid = 0;
        }
    } else {
        // Internal checking in case the entity quest barfed
        if !matches!(
            (*entity).etype,
            XmlEntityType::XmlInternalParameterEntity | XmlEntityType::XmlExternalParameterEntity
        ) {
            xml_warning_msg!(
                ctxt,
                XmlParserErrors::XmlWarUndeclaredEntity,
                "%{}; is not a parameter entity\n",
                name
            );
        }
    }
    (*ctxt).has_perefs = 1;
    *str = ptr;
    entity
}

/// The current input pointed by (*ctxt).input came to an end pop it and return the next c_char.
///
/// Returns the current XmlChar in the parser context
#[doc(alias = "xmlPopInput")]
pub unsafe extern "C" fn xml_pop_input(ctxt: XmlParserCtxtPtr) -> XmlChar {
    if ctxt.is_null() || (*ctxt).input_tab.len() <= 1 {
        return 0;
    }
    if get_parser_debug_entities() != 0 {
        generic_error!("Popping input {}\n", (*ctxt).input_tab.len());
    }
    if (*ctxt).input_tab.len() > 1
        && (*ctxt).in_subset == 0
        && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            Some("Unfinished entity outside the DTD"),
        );
    }
    let input: XmlParserInputPtr = (*ctxt).input_pop();
    if !(*input).entity.is_null() {
        (*(*input).entity).flags &= !XML_ENT_EXPANDING as i32;
    }
    xml_free_input_stream(input);
    if *(*(*ctxt).input).cur == 0 {
        (*ctxt).force_grow();
    }
    (*ctxt).current_byte()
}

/// Load the original content of the given system entity from the
/// ExternalID/SystemID given. This is to be used for Included in Literal
/// http://www.w3.org/TR/REC-xml/#inliteral processing of entities references
///
/// Returns 0 in case of success and -1 in case of failure
#[doc(alias = "xmlLoadEntityContent")]
unsafe extern "C" fn xml_load_entity_content(ctxt: XmlParserCtxtPtr, entity: XmlEntityPtr) -> i32 {
    let mut l: i32 = 0;

    if ctxt.is_null()
        || entity.is_null()
        || !matches!(
            (*entity).etype,
            XmlEntityType::XmlExternalParameterEntity
                | XmlEntityType::XmlExternalGeneralParsedEntity
        )
        || !(*entity).content.load(Ordering::Relaxed).is_null()
    {
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            Some("xmlLoadEntityContent parameter error"),
        );
        return -1;
    }

    if get_parser_debug_entities() != 0 {
        generic_error!(
            "Reading {} entity content input\n",
            CStr::from_ptr((*entity).name.load(Ordering::Relaxed) as *const i8).to_string_lossy()
        );
    }

    let buf = xml_buf_create();
    if buf.is_null() {
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            Some("xmlLoadEntityContent parameter error"),
        );
        return -1;
    }
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);

    let input: XmlParserInputPtr = xml_new_entity_input_stream(ctxt, entity);
    if input.is_null() {
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            Some("xmlLoadEntityContent input error"),
        );
        xml_buf_free(buf);
        return -1;
    }

    /*
     * Push the entity as the current input, read c_char by c_char
     * saving to the buffer until the end of the entity or an error
     */
    if xml_push_input(ctxt, input) < 0 {
        xml_buf_free(buf);
        xml_free_input_stream(input);
        return -1;
    }

    (*ctxt).grow();
    let mut c = (*ctxt).current_char(&mut l).unwrap_or('\0');
    while (*ctxt).input == input
        && (*(*ctxt).input).cur < (*(*ctxt).input).end
        && xml_is_char(c as u32)
    {
        xml_buf_add(buf, (*(*ctxt).input).cur, l);
        (*ctxt).advance_with_line_handling(l as usize);
        c = (*ctxt).current_char(&mut l).unwrap_or('\0');
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        xml_buf_free(buf);
        return -1;
    }

    if (*ctxt).input == input && (*(*ctxt).input).cur >= (*(*ctxt).input).end {
        (*ctxt).sizeentities = (*ctxt)
            .sizeentities
            .saturating_add((*(*ctxt).input).consumed);
        xml_pop_input(ctxt);
    } else if !xml_is_char(c as u32) {
        xml_fatal_err_msg_int!(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            format!("xmlLoadEntityContent: invalid char value {}\n", c as i32).as_str(),
            c as i32
        );
        xml_buf_free(buf);
        return -1;
    }
    (*entity).length = xml_buf_use(buf) as i32;
    (*entity)
        .content
        .store(xml_buf_detach(buf), Ordering::Relaxed);
    xml_buf_free(buf);

    0
}

// Macro used to grow the current buffer.
// buffer##_size is expected to be a size_t
// mem_error: is expected to handle memory allocation failures
macro_rules! grow_buffer {
    ($ctxt:expr, $buffer:expr, $n:expr, $buffer_size:expr, $rep:expr, $mem_error:tt) => {
        let tmp: *mut XmlChar;
        let new_size: size_t = $buffer_size as usize * 2 + $n as usize;
        if new_size < $buffer_size as usize {
            break $mem_error;
        }
        tmp = xml_realloc($buffer as _, new_size as usize) as _;
        if tmp.is_null() {
            break $mem_error;
        }
        $buffer = tmp as _;
        $buffer_size = new_size as _;
    };
}

#[doc(alias = "xmlStringDecodeEntitiesInt")]
pub(crate) unsafe extern "C" fn xml_string_decode_entities_int(
    ctxt: XmlParserCtxtPtr,
    mut str: *const XmlChar,
    len: i32,
    what: i32,
    end: XmlChar,
    end2: XmlChar,
    end3: XmlChar,
    check: i32,
) -> *mut XmlChar {
    let mut buffer: *mut XmlChar;
    let mut buffer_size: size_t;
    let mut nbchars: size_t = 0;
    let mut current: *mut XmlChar;
    let mut rep: *mut XmlChar = null_mut();
    let mut ent: XmlEntityPtr;
    let mut c: i32;
    let mut l: i32 = 0;

    if str.is_null() {
        return null_mut();
    }
    let last: *const XmlChar = str.add(len as usize);

    if ((*ctxt).depth > 40 && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0)
        || (*ctxt).depth > 100
    {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrEntityLoop,
            "Maximum entity nesting depth exceeded",
        );
        return null_mut();
    }

    /*
     * allocate a translation buffer.
     */
    buffer_size = XML_PARSER_BIG_BUFFER_SIZE;
    buffer = xml_malloc_atomic(buffer_size as _) as _;
    'int_error: {
        'mem_error: {
            if buffer.is_null() {
                break 'mem_error;
            }

            /*
             * OK loop until we reach one of the ending c_char or a size limit.
             * we are operating on already parsed values.
             */
            if str < last {
                c = CUR_SCHAR!(ctxt, str, l);
            } else {
                c = 0;
            }
            while c != 0
                && c != end as i32 /* non input consuming loop */
                && c != end2 as i32
                && c != end3 as i32
                && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
            {
                if c == 0 {
                    break;
                }
                if c == b'&' as i32 && *str.add(1) == b'#' {
                    let val: i32 = xml_parse_string_char_ref(ctxt, addr_of_mut!(str));
                    if val == 0 {
                        break 'int_error;
                    }
                    COPY_BUF!(0, buffer, nbchars, val);
                    if nbchars + XML_PARSER_BUFFER_SIZE > buffer_size {
                        grow_buffer!(ctxt, buffer, XML_PARSER_BUFFER_SIZE, buffer_size, rep, 'mem_error);
                    }
                } else if c == b'&' as i32 && what & XML_SUBSTITUTE_REF as i32 != 0 {
                    if get_parser_debug_entities() != 0 {
                        generic_error!(
                            "String decoding Entity Reference: {}\n",
                            CStr::from_ptr(str as *const i8)
                                .to_string_lossy()
                                .chars()
                                .take(30)
                                .collect::<String>()
                        );
                    }
                    ent = xml_parse_string_entity_ref(ctxt, addr_of_mut!(str));
                    if !ent.is_null()
                        && matches!((*ent).etype, XmlEntityType::XmlInternalPredefinedEntity)
                    {
                        if !(*ent).content.load(Ordering::Relaxed).is_null() {
                            COPY_BUF!(
                                0,
                                buffer,
                                nbchars,
                                *(*ent).content.load(Ordering::Relaxed).add(0)
                            );
                            if nbchars + XML_PARSER_BUFFER_SIZE > buffer_size {
                                grow_buffer!(
                                    ctxt,
                                    buffer,
                                    XML_PARSER_BUFFER_SIZE,
                                    buffer_size,
                                    rep,
                                    'mem_error
                                );
                            }
                        } else {
                            xml_fatal_err_msg(
                                ctxt,
                                XmlParserErrors::XmlErrInternalError,
                                "predefined entity has no content\n",
                            );
                            break 'int_error;
                        }
                    } else if !ent.is_null() && !(*ent).content.load(Ordering::Relaxed).is_null() {
                        if check != 0 && xml_parser_entity_check(ctxt, (*ent).length as _) != 0 {
                            break 'int_error;
                        }

                        if (*ent).flags & XML_ENT_EXPANDING as i32 != 0 {
                            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, None);
                            (*ctxt).halt();
                            *(*ent).content.load(Ordering::Relaxed).add(0) = 0;
                            break 'int_error;
                        }

                        (*ent).flags |= XML_ENT_EXPANDING as i32;
                        (*ctxt).depth += 1;
                        rep = xml_string_decode_entities_int(
                            ctxt,
                            (*ent).content.load(Ordering::Relaxed) as _,
                            (*ent).length,
                            what,
                            0,
                            0,
                            0,
                            check,
                        );
                        (*ctxt).depth -= 1;
                        (*ent).flags &= !XML_ENT_EXPANDING as i32;

                        if rep.is_null() {
                            *(*ent).content.load(Ordering::Relaxed).add(0) = 0;
                            break 'int_error;
                        }

                        current = rep;
                        while *current != 0 {
                            /* non input consuming loop */
                            *buffer.add(nbchars) = *current;
                            nbchars += 1;
                            current = current.add(1);
                            if nbchars + XML_PARSER_BUFFER_SIZE > buffer_size {
                                grow_buffer!(
                                    ctxt,
                                    buffer,
                                    XML_PARSER_BUFFER_SIZE,
                                    buffer_size,
                                    rep,'mem_error
                                );
                            }
                        }
                        xml_free(rep as _);
                        rep = null_mut();
                    } else if !ent.is_null() {
                        let i: i32 = xml_strlen((*ent).name.load(Ordering::Relaxed));
                        let mut cur: *const XmlChar = (*ent).name.load(Ordering::Relaxed);

                        *buffer.add(nbchars as usize) = b'&';
                        nbchars += 1;
                        if nbchars + i as usize + XML_PARSER_BUFFER_SIZE > buffer_size {
                            grow_buffer!(
                                ctxt,
                                buffer,
                                i as usize + XML_PARSER_BUFFER_SIZE,
                                buffer_size,
                                rep,'mem_error
                            );
                        }
                        for _ in (1..=i).rev() {
                            *buffer.add(nbchars as usize) = *cur;
                            nbchars += 1;
                            cur = cur.add(1);
                        }
                        *buffer.add(nbchars as usize) = b';';
                        nbchars += 1;
                    }
                } else if c == b'%' as i32 && what & XML_SUBSTITUTE_PEREF as i32 != 0 {
                    if get_parser_debug_entities() != 0 {
                        generic_error!(
                            "String decoding PE Reference: {}\n",
                            CStr::from_ptr(str as *const i8)
                                .to_string_lossy()
                                .chars()
                                .take(30)
                                .collect::<String>()
                        );
                    }
                    ent = xml_parse_string_pereference(ctxt, addr_of_mut!(str));
                    if !ent.is_null() {
                        if (*ent).content.load(Ordering::Relaxed).is_null() {
                            /*
                             * Note: external parsed entities will not be loaded,
                             * it is not required for a non-validating parser to
                             * complete external PEReferences coming from the
                             * internal subset
                             */
                            if (*ctxt).options & XmlParserOption::XmlParseNoent as i32 != 0
                                || (*ctxt).options & XmlParserOption::XmlParseDtdvalid as i32 != 0
                                || (*ctxt).validate != 0
                            {
                                xml_load_entity_content(ctxt, ent);
                            } else {
                                let name = CStr::from_ptr(
                                    (*ent).name.load(Ordering::Relaxed) as *const i8
                                )
                                .to_string_lossy();
                                xml_warning_msg!(
                                    ctxt,
                                    XmlParserErrors::XmlErrEntityProcessing,
                                    "not validating will not read content for PE entity {}\n",
                                    name
                                );
                            }
                        }

                        if check != 0 && xml_parser_entity_check(ctxt, (*ent).length as _) != 0 {
                            break 'int_error;
                        }

                        if (*ent).flags & XML_ENT_EXPANDING as i32 != 0 {
                            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, None);
                            (*ctxt).halt();
                            if !(*ent).content.load(Ordering::Relaxed).is_null() {
                                *(*ent).content.load(Ordering::Relaxed).add(0) = 0;
                            }
                            break 'int_error;
                        }

                        (*ent).flags |= XML_ENT_EXPANDING as i32;
                        (*ctxt).depth += 1;
                        rep = xml_string_decode_entities_int(
                            ctxt,
                            (*ent).content.load(Ordering::Relaxed) as _,
                            (*ent).length,
                            what,
                            0,
                            0,
                            0,
                            check,
                        );
                        (*ctxt).depth -= 1;
                        (*ent).flags &= !XML_ENT_EXPANDING as i32;

                        if rep.is_null() {
                            if !(*ent).content.load(Ordering::Relaxed).is_null() {
                                *(*ent).content.load(Ordering::Relaxed).add(0) = 0;
                            }
                            break 'int_error;
                        }
                        current = rep;
                        while *current != 0 {
                            /* non input consuming loop */
                            *buffer.add(nbchars) = *current;
                            nbchars += 1;
                            current = current.add(1);
                            if nbchars + XML_PARSER_BUFFER_SIZE > buffer_size {
                                grow_buffer!(
                                    ctxt,
                                    buffer,
                                    XML_PARSER_BUFFER_SIZE,
                                    buffer_size,
                                    rep,'mem_error
                                );
                            }
                        }
                        xml_free(rep as _);
                        rep = null_mut();
                    }
                } else {
                    COPY_BUF!(l, buffer, nbchars, c);
                    str = str.add(l as usize);
                    if nbchars + XML_PARSER_BUFFER_SIZE > buffer_size {
                        grow_buffer!(ctxt, buffer, XML_PARSER_BUFFER_SIZE, buffer_size, rep,'mem_error);
                    }
                }
                if str < last {
                    c = CUR_SCHAR!(ctxt, str, l);
                } else {
                    c = 0;
                }
            }
            *buffer.add(nbchars) = 0;
            return buffer;
        }

        // mem_error:
        xml_err_memory(ctxt, None);
    }
    // int_error:
    if !rep.is_null() {
        xml_free(rep as _);
    }
    if !buffer.is_null() {
        xml_free(buffer as _);
    }
    null_mut()
}

/// Parse a value for an attribute, this is the fallback function
/// of xmlParseAttValue() when the attribute parsing requires handling
/// of non-ASCII characters, or normalization compaction.
///
/// Returns the AttValue parsed or NULL. The value has to be freed by the caller.
#[doc(alias = "xmlParseAttValueComplex")]
unsafe extern "C" fn xml_parse_att_value_complex(
    ctxt: XmlParserCtxtPtr,
    attlen: *mut i32,
    normalize: i32,
) -> *mut XmlChar {
    let limit: XmlChar;
    let mut buf: *mut XmlChar;
    let mut rep: *mut XmlChar = null_mut();
    let mut len: size_t = 0;
    let mut buf_size: size_t;
    let max_length: size_t = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH
    } else {
        XML_MAX_TEXT_LENGTH
    };
    let mut l: i32 = 0;
    let mut in_space: i32 = 0;
    let mut current: *mut XmlChar;
    let mut ent: XmlEntityPtr;

    if (*ctxt).current_byte() == b'"' {
        (*ctxt).instate = XmlParserInputState::XmlParserAttributeValue;
        limit = b'"';
        (*ctxt).skip_char();
    } else if (*ctxt).current_byte() == b'\'' {
        limit = b'\'';
        (*ctxt).instate = XmlParserInputState::XmlParserAttributeValue;
        (*ctxt).skip_char();
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttributeNotStarted, None);
        return null_mut();
    }

    /*
     * allocate a translation buffer.
     */
    buf_size = XML_PARSER_BUFFER_SIZE;
    buf = xml_malloc_atomic(buf_size as _) as _;
    'error: {
        'mem_error: {
            if buf.is_null() {
                break 'mem_error;
            }

            /*
             * OK loop until we reach one of the ending c_char or a size limit.
             */
            let mut c = (*ctxt).current_char(&mut l).unwrap_or('\0');
            while ((*ctxt).current_byte() != limit /* checked */ && xml_is_char(c as u32) && c != '<')
                && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
            {
                if c == '&' {
                    in_space = 0;
                    if (*ctxt).nth_byte(1) == b'#' {
                        let val: i32 = xml_parse_char_ref(ctxt);

                        if val == b'&' as i32 {
                            if (*ctxt).replace_entities != 0 {
                                if len + 10 > buf_size {
                                    grow_buffer!(ctxt, buf, 10, buf_size, rep, 'mem_error);
                                }
                                *buf.add(len as usize) = b'&';
                                len += 1;
                            } else {
                                /*
                                 * The reparsing will be done in xmlStringGetNodeList()
                                 * called by the attribute() function in SAX.c
                                 */
                                if len + 10 > buf_size {
                                    grow_buffer!(ctxt, buf, 10, buf_size, rep, 'mem_error);
                                }
                                *buf.add(len as usize) = b'&';
                                len += 1;
                                *buf.add(len as usize) = b'#';
                                len += 1;
                                *buf.add(len as usize) = b'3';
                                len += 1;
                                *buf.add(len as usize) = b'8';
                                len += 1;
                                *buf.add(len as usize) = b';';
                                len += 1;
                            }
                        } else if val != 0 {
                            if len + 10 > buf_size {
                                grow_buffer!(ctxt, buf, 10, buf_size, rep, 'mem_error);
                            }
                            len += xml_copy_char(0, buf.add(len as usize), val) as usize;
                        }
                    } else {
                        ent = xml_parse_entity_ref(ctxt);
                        if !ent.is_null()
                            && matches!((*ent).etype, XmlEntityType::XmlInternalPredefinedEntity)
                        {
                            if len + 10 > buf_size {
                                grow_buffer!(ctxt, buf, 10, buf_size, rep, 'mem_error);
                            }
                            if (*ctxt).replace_entities == 0
                                && *(*ent).content.load(Ordering::Relaxed).add(0) == b'&'
                            {
                                *buf.add(len as usize) = b'&';
                                len += 1;
                                *buf.add(len as usize) = b'#';
                                len += 1;
                                *buf.add(len as usize) = b'3';
                                len += 1;
                                *buf.add(len as usize) = b'8';
                                len += 1;
                                *buf.add(len as usize) = b';';
                                len += 1;
                            } else {
                                *buf.add(len as usize) =
                                    *(*ent).content.load(Ordering::Relaxed).add(0);
                                len += 1;
                            }
                        } else if !ent.is_null() && (*ctxt).replace_entities != 0 {
                            if !matches!((*ent).etype, XmlEntityType::XmlInternalPredefinedEntity) {
                                if xml_parser_entity_check(ctxt, (*ent).length as _) != 0 {
                                    break 'error;
                                }

                                (*ctxt).depth += 1;
                                rep = xml_string_decode_entities_int(
                                    ctxt,
                                    (*ent).content.load(Ordering::Relaxed),
                                    (*ent).length,
                                    XML_SUBSTITUTE_REF as _,
                                    0,
                                    0,
                                    0,
                                    /* check */ 1,
                                );
                                (*ctxt).depth -= 1;
                                if !rep.is_null() {
                                    current = rep;
                                    while *current != 0 {
                                        /* non input consuming */
                                        if *current == 0xD || *current == 0xA || *current == 0x9 {
                                            *buf.add(len as usize) = 0x20;
                                            len += 1;
                                            current = current.add(1);
                                        } else {
                                            *buf.add(len as usize) = *current;
                                            current = current.add(1);
                                            len += 1;
                                        }
                                        if len + 10 > buf_size {
                                            grow_buffer!(ctxt, buf, 10, buf_size, rep, 'mem_error);
                                        }
                                    }
                                    xml_free(rep as _);
                                    rep = null_mut();
                                }
                            } else {
                                if len + 10 > buf_size {
                                    grow_buffer!(ctxt, buf, 10, buf_size, rep, 'mem_error);
                                }
                                if !(*ent).content.load(Ordering::Relaxed).is_null() {
                                    *buf.add(len as usize) =
                                        *(*ent).content.load(Ordering::Relaxed).add(0);
                                    len += 1;
                                }
                            }
                        } else if !ent.is_null() {
                            let i: i32 = xml_strlen((*ent).name.load(Ordering::Relaxed));
                            let mut cur: *const XmlChar = (*ent).name.load(Ordering::Relaxed);

                            /*
                             * We also check for recursion and amplification
                             * when entities are not substituted. They're
                             * often expanded later.
                             */
                            if !matches!((*ent).etype, XmlEntityType::XmlInternalPredefinedEntity)
                                && !(*ent).content.load(Ordering::Relaxed).is_null()
                            {
                                if (*ent).flags & XML_ENT_CHECKED as i32 == 0 {
                                    let old_copy: u64 = (*ctxt).sizeentcopy;

                                    (*ctxt).sizeentcopy = (*ent).length as _;

                                    (*ctxt).depth += 1;
                                    rep = xml_string_decode_entities_int(
                                        ctxt,
                                        (*ent).content.load(Ordering::Relaxed),
                                        (*ent).length,
                                        XML_SUBSTITUTE_REF as _,
                                        0,
                                        0,
                                        0,
                                        /* check */ 1,
                                    );
                                    (*ctxt).depth -= 1;

                                    /*
                                     * If we're parsing DTD content, the entity
                                     * might reference other entities which
                                     * weren't defined yet, so the check isn't
                                     * reliable.
                                     */
                                    if (*ctxt).in_subset == 0 {
                                        (*ent).flags |= XML_ENT_CHECKED as i32;
                                        (*ent).expanded_size = (*ctxt).sizeentcopy;
                                    }

                                    if !rep.is_null() {
                                        xml_free(rep as _);
                                        rep = null_mut();
                                    } else {
                                        *(*ent).content.load(Ordering::Relaxed).add(0) = 0;
                                    }

                                    if xml_parser_entity_check(ctxt, old_copy) != 0 {
                                        break 'error;
                                    }
                                } else if xml_parser_entity_check(ctxt, (*ent).expanded_size) != 0 {
                                    break 'error;
                                }
                            }

                            /*
                             * Just output the reference
                             */
                            *buf.add(len as usize) = b'&';
                            len += 1;
                            while len + i as usize + 10 > buf_size {
                                grow_buffer!(ctxt, buf, i + 10, buf_size, rep, 'mem_error);
                            }
                            for _ in (1..=i).rev() {
                                *buf.add(len as usize) = *cur;
                                cur = cur.add(1);
                                len += 1;
                            }
                            *buf.add(len as usize) = b';';
                            len += 1;
                        }
                    }
                } else {
                    if c == '\u{20}' || c == '\u{D}' || c == '\u{A}' || c == '\u{9}' {
                        if len != 0 || normalize == 0 {
                            if normalize == 0 || in_space == 0 {
                                COPY_BUF!(l, buf, len, 0x20);
                                while len + 10 > buf_size {
                                    grow_buffer!(ctxt, buf, 10, buf_size, rep, 'mem_error);
                                }
                            }
                            in_space = 1;
                        }
                    } else {
                        in_space = 0;
                        COPY_BUF!(l, buf, len, c);
                        if len + 10 > buf_size {
                            grow_buffer!(ctxt, buf, 10, buf_size, rep, 'mem_error);
                        }
                    }
                    (*ctxt).advance_with_line_handling(l as usize);
                }
                (*ctxt).grow();
                c = (*ctxt).current_char(&mut l).unwrap_or('\0');
                if len > max_length {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrAttributeNotFinished,
                        "AttValue length too long\n",
                    );
                    break 'mem_error;
                }
            }
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                break 'error;
            }

            if in_space != 0 && normalize != 0 {
                while len > 0 && *buf.add(len - 1) == 0x20 {
                    len -= 1;
                }
            }
            *buf.add(len as usize) = 0;
            if (*ctxt).current_byte() == b'<' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrLtInAttribute, None);
            } else if (*ctxt).current_byte() != limit {
                if c != '\0' && !xml_is_char(c as u32) {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidChar,
                        "invalid character in attribute value\n",
                    );
                } else {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrAttributeNotFinished,
                        "AttValue: ' expected\n",
                    );
                }
            } else {
                (*ctxt).skip_char();
            }

            if !attlen.is_null() {
                *attlen = len as _;
            }
            return buf;
        }

        // mem_error:
        xml_err_memory(ctxt, None);
    }
    // error:
    if !buf.is_null() {
        xml_free(buf as _);
    }
    if !rep.is_null() {
        xml_free(rep as _);
    }
    null_mut()
}

macro_rules! GROW_PARSE_ATT_VALUE_INTERNAL {
    ($ctxt:expr, $input:expr, $start:expr, $end:expr) => {
        let oldbase: *const XmlChar = (*(*$ctxt).input).base;
        (*$ctxt).grow();
        if matches!((*$ctxt).instate, XmlParserInputState::XmlParserEOF) {
            return null_mut();
        }
        if oldbase != (*(*$ctxt).input).base {
            let delta: ptrdiff_t = (*(*$ctxt).input).base.offset_from(oldbase);
            $start = $start.add(delta as usize);
            $input = $input.add(delta as usize);
        }
        $end = (*(*$ctxt).input).end;
    };
}

/// Parse a value for an attribute.
///
/// # Note
/// If no normalization is needed, the routine will return pointers directly from the data buffer.
///
/// 3.3.3 Attribute-Value Normalization:
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
/// Returns the AttValue parsed or NULL. The value has to be freed by the
///     caller if it was copied, this can be detected by val[*len] == 0.
#[doc(alias = "xmlParseAttValueInternal")]
pub(crate) unsafe extern "C" fn xml_parse_att_value_internal(
    ctxt: XmlParserCtxtPtr,
    len: *mut i32,
    alloc: *mut i32,
    normalize: i32,
) -> *mut XmlChar {
    let mut start: *const XmlChar;
    let mut end: *const XmlChar;
    let mut last: *const XmlChar;
    let ret: *mut XmlChar;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH as i32
    } else {
        XML_MAX_TEXT_LENGTH as i32
    };

    (*ctxt).grow();
    let mut input: *const XmlChar = (*ctxt).current_ptr();
    let mut line: i32 = (*(*ctxt).input).line;
    let mut col: i32 = (*(*ctxt).input).col;
    if *input != b'"' && *input != b'\'' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttributeNotStarted, None);
        return null_mut();
    }
    (*ctxt).instate = XmlParserInputState::XmlParserAttributeValue;

    /*
     * try to handle in this routine the most common case where no
     * allocation of a new string is required and where content is
     * pure ASCII.
     */
    let limit: XmlChar = *input;
    input = input.add(1);
    col += 1;
    end = (*(*ctxt).input).end;
    start = input;
    if input >= end {
        GROW_PARSE_ATT_VALUE_INTERNAL!(ctxt, input, start, end);
    }
    if normalize != 0 {
        /*
         * Skip any leading spaces
         */
        while input < end
            && *input != limit
            && (*input == 0x20 || *input == 0x9 || *input == 0xA || *input == 0xD)
        {
            if *input == 0xA {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
            input = input.add(1);
            start = input;
            if input >= end {
                GROW_PARSE_ATT_VALUE_INTERNAL!(ctxt, input, start, end);
                if input.offset_from(start) > max_length as isize {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrAttributeNotFinished,
                        "AttValue length too long\n",
                    );
                    return null_mut();
                }
            }
        }
        while input < end
            && *input != limit
            && *input >= 0x20
            && *input <= 0x7f
            && *input != b'&'
            && *input != b'<'
        {
            col += 1;
            if {
                let f = *input == 0x20;
                input = input.add(1);
                f
            } && *input == 0x20
            {
                break;
            }
            if input >= end {
                GROW_PARSE_ATT_VALUE_INTERNAL!(ctxt, input, start, end);
                if input.offset_from(start) > max_length as isize {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrAttributeNotFinished,
                        "AttValue length too long\n",
                    );
                    return null_mut();
                }
            }
        }
        last = input;
        /*
         * skip the trailing blanks
         */
        while *last.sub(1) == 0x20 && last > start {
            last = last.sub(1);
        }
        while input < end
            && *input != limit
            && (*input == 0x20 || *input == 0x9 || *input == 0xA || *input == 0xD)
        {
            if *input == 0xA {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
            input = input.add(1);
            if input >= end {
                let oldbase: *const XmlChar = (*(*ctxt).input).base;
                (*ctxt).grow();
                if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    return null_mut();
                }
                if oldbase != (*(*ctxt).input).base {
                    let delta: ptrdiff_t = (*(*ctxt).input).base.offset_from(oldbase);
                    start = start.add(delta as usize);
                    input = input.add(delta as usize);
                    last = last.add(delta as usize);
                }
                end = (*(*ctxt).input).end;
                if input.offset_from(start) > max_length as isize {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrAttributeNotFinished,
                        "AttValue length too long\n",
                    );
                    return null_mut();
                }
            }
        }
        if input.offset_from(start) > max_length as isize {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrAttributeNotFinished,
                "AttValue length too long\n",
            );
            return null_mut();
        }
        if *input != limit {
            //  goto need_complex;
            if !alloc.is_null() {
                *alloc = 1;
            }
            return xml_parse_att_value_complex(ctxt, len, normalize);
        }
    } else {
        while input < end
            && *input != limit
            && *input >= 0x20
            && *input <= 0x7f
            && *input != b'&'
            && *input != b'<'
        {
            input = input.add(1);
            col += 1;
            if input >= end {
                GROW_PARSE_ATT_VALUE_INTERNAL!(ctxt, input, start, end);
                if input.offset_from(start) > max_length as isize {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrAttributeNotFinished,
                        "AttValue length too long\n",
                    );
                    return null_mut();
                }
            }
        }
        last = input;
        if input.offset_from(start) > max_length as isize {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrAttributeNotFinished,
                "AttValue length too long\n",
            );
            return null_mut();
        }
        if *input != limit {
            //  goto need_complex;
            if !alloc.is_null() {
                *alloc = 1;
            }
            return xml_parse_att_value_complex(ctxt, len, normalize);
        }
    }
    input = input.add(1);
    col += 1;
    if !len.is_null() {
        if !alloc.is_null() {
            *alloc = 0;
        }
        *len = last.offset_from(start) as _;
        ret = start as _;
    } else {
        if !alloc.is_null() {
            *alloc = 1;
        }
        ret = xml_strndup(start, last.offset_from(start) as _);
    }
    (*(*ctxt).input).cur = input;
    (*(*ctxt).input).line = line;
    (*(*ctxt).input).col = col;
    ret
    // need_complex:
    //  if !alloc.is_null() {
    //      *alloc = 1;
    //  }
    //  return xmlParseAttValueComplex(ctxt, len, normalize);
}

/// Normalize the space in non CDATA attribute values:
/// If the attribute type is not CDATA, then the XML processor MUST further
/// process the normalized attribute value by discarding any leading and
/// trailing space (#x20) characters, and by replacing sequences of space
/// (#x20) characters by a single space (#x20) character.
/// Note that the size of dst need to be at least src, and if one doesn't need
/// to preserve dst (and it doesn't come from a dictionary or read-only) then
/// passing src as dst is just fine.
///
/// Returns a pointer to the normalized value (dst) or NULL if no conversion is needed.
#[doc(alias = "xmlAttrNormalizeSpace")]
pub(crate) unsafe extern "C" fn xml_attr_normalize_space(
    mut src: *const XmlChar,
    mut dst: *mut XmlChar,
) -> *mut XmlChar {
    if src.is_null() || dst.is_null() {
        return null_mut();
    }

    while *src == 0x20 {
        src = src.add(1);
    }
    while *src != 0 {
        if *src == 0x20 {
            while *src == 0x20 {
                src = src.add(1);
            }
            if *src != 0 {
                *dst = 0x20;
                dst = dst.add(1);
            }
        } else {
            *dst = *src;
            dst = dst.add(1);
            src = src.add(1);
        }
    }
    *dst = 0;
    if dst == src as _ {
        return null_mut();
    }
    dst
}

/// Normalize the space in non CDATA attribute values, a slightly more complex
/// front end to avoid allocation problems when running on attribute values
/// coming from the input.
///
/// Returns a pointer to the normalized value (dst) or NULL if no conversion is needed.
#[doc(alias = "xmlAttrNormalizeSpace2")]
unsafe extern "C" fn xml_attr_normalize_space2(
    ctxt: XmlParserCtxtPtr,
    src: *mut XmlChar,
    len: *mut i32,
) -> *const XmlChar {
    let mut remove_head: i32 = 0;
    let mut need_realloc: i32 = 0;
    let mut cur: *const XmlChar;

    if ctxt.is_null() || src.is_null() || len.is_null() {
        return null_mut();
    }
    let i: i32 = *len;
    if i <= 0 {
        return null_mut();
    }

    cur = src;
    while *cur == 0x20 {
        cur = cur.add(1);
        remove_head += 1;
    }
    while *cur != 0 {
        if *cur == 0x20 {
            cur = cur.add(1);
            if *cur == 0x20 || *cur == 0 {
                need_realloc = 1;
                break;
            }
        } else {
            cur = cur.add(1);
        }
    }
    if need_realloc != 0 {
        let ret: *mut XmlChar = xml_strndup(src.add(remove_head as usize), i - remove_head + 1);
        if ret.is_null() {
            xml_err_memory(ctxt, None);
            return null_mut();
        }
        xml_attr_normalize_space(ret, ret);
        *len = strlen(ret as _) as _;
        return ret;
    } else if remove_head != 0 {
        *len -= remove_head;
        memmove(
            src as _,
            src.add(remove_head as usize) as _,
            1 + *len as usize,
        );
        return src;
    }
    null_mut()
}

/// Returns the attribute name, and the value in *value, .
#[doc(alias = "xmlParseAttribute2")]
unsafe extern "C" fn xml_parse_attribute2(
    ctxt: XmlParserCtxtPtr,
    pref: *const XmlChar,
    elem: *const XmlChar,
    prefix: *mut *const XmlChar,
    value: *mut *mut XmlChar,
    len: *mut i32,
    alloc: *mut i32,
) -> *const XmlChar {
    let mut val: *mut XmlChar;
    let mut internal_val: *mut XmlChar = null_mut();
    let mut normalize: i32 = 0;

    *value = null_mut();
    (*ctxt).grow();

    let name: *const XmlChar = xml_parse_qname(ctxt, prefix);

    if name.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            "error parsing attribute name\n",
        );
        return null_mut();
    }

    // get the type if needed
    if let Some(atts) = (*ctxt).atts_special {
        if atts
            .qlookup2(
                (!pref.is_null()).then(|| CStr::from_ptr(pref as *const i8)),
                CStr::from_ptr(elem as *const i8),
                (!(*prefix).is_null()).then(|| CStr::from_ptr(*prefix as *const i8)),
                (!name.is_null()).then(|| CStr::from_ptr(name as *const i8)),
            )
            .is_some()
        {
            normalize = 1;
        }
    }

    /*
     * read the value
     */
    (*ctxt).skip_blanks();

    if (*ctxt).current_byte() == b'=' {
        (*ctxt).skip_char();
        (*ctxt).skip_blanks();
        val = xml_parse_att_value_internal(ctxt, len, alloc, normalize);
        if val.is_null() {
            return null_mut();
        }
        if normalize != 0 {
            /*
             * Sometimes a second normalisation pass for spaces is needed
             * but that only happens if charrefs or entities references
             * have been used in the attribute value, i.e. the attribute
             * value have been extracted in an allocated string already.
             */
            if *alloc != 0 {
                let val2: *const XmlChar = xml_attr_normalize_space2(ctxt, val, len);
                if !val2.is_null() && val2 != val {
                    xml_free(val as _);
                    val = val2 as _;
                }
            }
        }
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

    if *prefix == (*ctxt).str_xml {
        // Check that xml:lang conforms to the specification
        // No more registered as an error, just generate a warning now
        // since this was deprecated in XML second edition
        if (*ctxt).pedantic != 0 && xml_str_equal(name, c"lang".as_ptr() as _) {
            internal_val = xml_strndup(val, *len);
            if xml_check_language_id(internal_val) == 0 {
                let internal_val = CStr::from_ptr(internal_val as *const i8).to_string_lossy();
                xml_warning_msg!(
                    ctxt,
                    XmlParserErrors::XmlWarLangValue,
                    "Malformed value for xml:lang : {}\n",
                    internal_val
                );
            }
        }

        // Check that xml:space conforms to the specification
        if xml_str_equal(name, c"space".as_ptr() as _) {
            internal_val = xml_strndup(val, *len);
            if xml_str_equal(internal_val, c"default".as_ptr() as _) {
                *(*ctxt).space_mut() = 0;
            } else if xml_str_equal(internal_val, c"preserve".as_ptr() as _) {
                *(*ctxt).space_mut() = 1;
            } else {
                let internal_val = CStr::from_ptr(internal_val as *const i8).to_string_lossy();
                xml_warning_msg!(
                    ctxt,
                    XmlParserErrors::XmlWarSpaceValue,
                    "Invalid value \"{}\" for xml:space : \"default\" or \"preserve\" expected\n",
                    internal_val
                );
            }
        }
        if !internal_val.is_null() {
            xml_free(internal_val as _);
        }
    }

    *value = val;
    name
}

/// Handle a namespace warning error
#[doc(alias = "xmlNsWarn")]
macro_rules! xml_ns_warn {
    ($ctxt:expr, $error:expr, $msg:expr) => {
        xml_ns_warn!(
            @inner
            $ctxt,
            $error,
            $msg,
            None,
            None,
            None
        )
    };
    ($ctxt:expr, $error:expr, $msg:expr, $info1:expr) => {
        let msg = format!($msg, $info1);
        xml_ns_warn!(
            @inner
            $ctxt,
            $error,
            &msg,
            Some($info1.to_owned().into()),
            None,
            None
        )
    };
    ($ctxt:expr, $error:expr, $msg:expr, $info1:expr, $info2:expr) => {
        let msg = format!($msg, $info1, $info2);
        xml_ns_warn!(
            @inner
            $ctxt,
            $error,
            &msg,
            Some($info1.to_owned().into()),
            Some($info2.to_owned().into()),
            None
        )
    };
    ($ctxt:expr, $error:expr, $msg:expr, $info1:expr, $info2:expr, $info3:expr) => {
        let msg = format!($msg, $info1, $info2, $info3);
        xml_ns_warn!(
            @inner
            $ctxt,
            $error,
            &msg,
            Some($info1.to_owned().into()),
            Some($info2.to_owned().into()),
            Some($info3.to_owned().into())
        )
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $info1:expr, $info2:expr, $info3:expr) => {
        let ctxt = $ctxt as *mut XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromNamespace,
                $error,
                XmlErrorLevel::XmlErrWarning,
                None,
                0,
                $info1,
                $info2,
                $info3,
                0,
                0,
                $msg,
            );
        }
    };
}

/// Handle a redefinition of attribute error
#[doc(alias = "xmlErrAttributeDup")]
pub(crate) unsafe fn xml_err_attribute_dup(
    ctxt: XmlParserCtxtPtr,
    prefix: Option<&str>,
    localname: &str,
) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = XmlParserErrors::XmlErrAttributeRedefined as i32;
    }

    if let Some(prefix) = prefix {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
            XmlParserErrors::XmlErrAttributeRedefined,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            Some(prefix.to_owned().into()),
            Some(localname.to_owned().into()),
            None,
            0,
            0,
            "Attribute {}:{} redefined\n",
            prefix,
            localname
        );
    } else {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
            XmlParserErrors::XmlErrAttributeRedefined,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            Some(localname.to_owned().into()),
            None,
            None,
            0,
            0,
            "Attribute {} redefined\n",
            localname
        );
    }
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
        if (*ctxt).recovery == 0 {
            (*ctxt).disable_sax = 1;
        }
    }
}

unsafe fn grow_attrs(
    ctxt: XmlParserCtxtPtr,
    atts: &mut *mut *const u8,
    attallocs: &mut *mut i32,
    maxatts: &mut i32,
    nr: i32,
) -> i32 {
    if nr + 5 > *maxatts {
        let newmaxatts = if *maxatts == 0 { 55 } else { (nr + 5) * 2 };
        let newatts =
            xml_malloc(newmaxatts as usize * size_of::<*const XmlChar>()) as *mut *const u8;
        if newatts.is_null() {
            // goto mem_error;
            xml_err_memory(ctxt, None);
            return -1;
        }
        *attallocs = xml_realloc(
            *attallocs as _,
            (newmaxatts as usize / 5) * size_of::<i32>(),
        ) as _;
        if attallocs.is_null() {
            xml_free(newatts as _);
            // goto mem_error;
            xml_err_memory(ctxt, None);
            return -1;
        }
        if *maxatts > 0 {
            memcpy(
                newatts as _,
                *atts as _,
                *maxatts as usize * size_of::<*const XmlChar>(),
            );
        }
        xml_free(*atts as _);
        *atts = newatts;
        *maxatts = newmaxatts;
    }
    *maxatts
}

/// Parse a start tag. Always consumes '<'.
///
/// This routine is called when running SAX2 parsing
///
/// `[40] STag ::= '<' Name (S Attribute)* S? '>'`
///
/// `[ WFC: Unique Att Spec ]`  
/// No attribute name may appear more than once in the same start-tag or
/// empty-element tag.
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
#[doc(alias = "xmlParseStartTag2")]
pub(crate) unsafe fn xml_parse_start_tag2(
    ctxt: XmlParserCtxtPtr,
    pref: *mut *const XmlChar,
    uri: *mut *const XmlChar,
    tlen: *mut i32,
) -> *const XmlChar {
    let mut localname: *const XmlChar;
    let mut prefix: *const XmlChar = null();
    let mut attname: *const XmlChar;
    let mut aprefix: *const XmlChar = null();
    let mut nsname: *const XmlChar;
    let mut attvalue: *mut XmlChar = null_mut();
    let mut atts: *mut *const XmlChar = null_mut();
    let mut attallocs: *mut i32 = null_mut();
    let mut maxatts: i32 = 0;

    if (*ctxt).current_byte() != b'<' {
        return null_mut();
    }
    (*ctxt).advance(1);

    let cur: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;
    let inputid: i32 = (*(*ctxt).input).id;
    let mut nbatts: i32 = 0;
    let mut nratts: i32 = 0;
    let mut nbdef: i32 = 0;
    let mut nb_ns: i32 = 0;
    let mut attval: i32 = 0;
    // /* Forget any namespaces added during an earlier parse of this element. */
    // (*ctxt).ns_tab.len() = ns_nr;

    localname = xml_parse_qname(ctxt, addr_of_mut!(prefix));
    if localname.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            "StartTag: invalid element name\n",
        );
        return null_mut();
    }
    *tlen = ((*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as usize - cur) as _;

    // Now parse the attributes, it ends up with the ending
    //
    // (S Attribute)* S?
    (*ctxt).skip_blanks();
    (*ctxt).grow();

    'done: {
        while ((*ctxt).current_byte() != b'>'
            && ((*ctxt).current_byte() != b'/' || (*ctxt).nth_byte(1) != b'>')
            && xml_is_char((*ctxt).current_byte() as u32))
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            let mut len: i32 = -1;
            let mut alloc: i32 = 0;

            attname = xml_parse_attribute2(
                ctxt,
                prefix,
                localname,
                addr_of_mut!(aprefix),
                addr_of_mut!(attvalue),
                addr_of_mut!(len),
                addr_of_mut!(alloc),
            );
            if attname.is_null() {
                xml_fatal_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    Some("xmlParseStartTag: problem parsing attributes\n"),
                );
                break;
            }
            'next_attr: {
                if attvalue.is_null() {
                    break 'next_attr;
                }
                if len < 0 {
                    len = xml_strlen(attvalue);
                }

                if attname == (*ctxt).str_xmlns && aprefix.is_null() {
                    let url: *const XmlChar = xml_dict_lookup((*ctxt).dict, attvalue, len);
                    let uri: XmlURIPtr;

                    if url.is_null() {
                        xml_err_memory(ctxt, Some("dictionary allocation failure"));
                        if !attvalue.is_null() && alloc != 0 {
                            xml_free(attvalue as _);
                        }
                        localname = null_mut();
                        break 'done;
                    }
                    if *url != 0 {
                        uri = xml_parse_uri(url as _);
                        if uri.is_null() {
                            let url = CStr::from_ptr(url as *const i8).to_string_lossy();
                            xml_ns_err!(
                                ctxt,
                                XmlParserErrors::XmlWarNsURI,
                                "xmlns: '{}' is not a valid URI\n",
                                url
                            );
                        } else {
                            if (*uri).scheme.is_null() {
                                let url = CStr::from_ptr(url as *const i8).to_string_lossy();
                                xml_ns_warn!(
                                    ctxt,
                                    XmlParserErrors::XmlWarNsURIRelative,
                                    "xmlns: URI {} is not absolute\n",
                                    url
                                );
                            }
                            xml_free_uri(uri);
                        }
                        if url == (*ctxt).str_xml_ns {
                            if attname != (*ctxt).str_xml {
                                xml_ns_err!(
                                    ctxt,
                                    XmlParserErrors::XmlNsErrXmlNamespace,
                                    "xml namespace URI cannot be the default namespace\n"
                                );
                            }
                            break 'next_attr;
                        }
                        if len == 29
                            && xml_str_equal(url, c"http://www.w3.org/2000/xmlns/".as_ptr() as _)
                        {
                            xml_ns_err!(
                                ctxt,
                                XmlParserErrors::XmlNsErrXmlNamespace,
                                "reuse of the xmlns namespace name is forbidden\n"
                            );
                            break 'next_attr;
                        }
                    }

                    // check that it's not a defined namespace
                    let mut j = 1;
                    while j <= nb_ns {
                        if (*ctxt).ns_tab[(*ctxt).ns_tab.len() - 2 * j as usize].is_null() {
                            break;
                        }
                        j += 1;
                    }
                    if j <= nb_ns {
                        let loc = CStr::from_ptr(attname as *const i8).to_string_lossy();
                        xml_err_attribute_dup(ctxt, None, &loc);
                    } else if (*ctxt).ns_push(null_mut(), url) > 0 {
                        nb_ns += 1;
                    }
                } else if aprefix == (*ctxt).str_xmlns {
                    let url: *const XmlChar = xml_dict_lookup((*ctxt).dict, attvalue, len);

                    if attname == (*ctxt).str_xml {
                        if url != (*ctxt).str_xml_ns {
                            xml_ns_err!(
                                ctxt,
                                XmlParserErrors::XmlNsErrXmlNamespace,
                                "xml namespace prefix mapped to wrong URI\n"
                            );
                        }
                        // Do not keep a namespace definition node
                        break 'next_attr;
                    }
                    if url == (*ctxt).str_xml_ns {
                        if attname != (*ctxt).str_xml {
                            xml_ns_err!(
                                ctxt,
                                XmlParserErrors::XmlNsErrXmlNamespace,
                                "xml namespace URI mapped to wrong prefix\n"
                            );
                        }
                        break 'next_attr;
                    }
                    if attname == (*ctxt).str_xmlns {
                        xml_ns_err!(
                            ctxt,
                            XmlParserErrors::XmlNsErrXmlNamespace,
                            "redefinition of the xmlns prefix is forbidden\n"
                        );
                        break 'next_attr;
                    }
                    if len == 29
                        && xml_str_equal(url, c"http://www.w3.org/2000/xmlns/".as_ptr() as _)
                    {
                        xml_ns_err!(
                            ctxt,
                            XmlParserErrors::XmlNsErrXmlNamespace,
                            "reuse of the xmlns namespace name is forbidden\n"
                        );
                        break 'next_attr;
                    }
                    if url.is_null() || *url.add(0) == 0 {
                        let attname = CStr::from_ptr(attname as *const i8).to_string_lossy();
                        xml_ns_err!(
                            ctxt,
                            XmlParserErrors::XmlNsErrXmlNamespace,
                            "xmlns:{}: Empty XML namespace is not allowed\n",
                            attname
                        );
                        break 'next_attr;
                    }
                    let uri: XmlURIPtr = xml_parse_uri(url as _);
                    if uri.is_null() {
                        let attname = CStr::from_ptr(attname as *const i8).to_string_lossy();
                        let url = CStr::from_ptr(url as *const i8).to_string_lossy();
                        xml_ns_err!(
                            ctxt,
                            XmlParserErrors::XmlWarNsURI,
                            "xmlns:{}: '{}' is not a valid URI\n",
                            attname,
                            url
                        );
                    } else {
                        if (*ctxt).pedantic != 0 && (*uri).scheme.is_null() {
                            let attname = CStr::from_ptr(attname as *const i8).to_string_lossy();
                            let url = CStr::from_ptr(url as *const i8).to_string_lossy();
                            xml_ns_warn!(
                                ctxt,
                                XmlParserErrors::XmlWarNsURIRelative,
                                "xmlns:{}: URI {} is not absolute\n",
                                attname,
                                url
                            );
                        }
                        xml_free_uri(uri);
                    }
                    // check that it's not a defined namespace
                    let mut j = 1;
                    while j <= nb_ns {
                        if (*ctxt).ns_tab[(*ctxt).ns_tab.len() - 2 * j as usize] == attname {
                            break;
                        }
                        j += 1;
                    }
                    if j <= nb_ns {
                        let pre = (!aprefix.is_null())
                            .then(|| CStr::from_ptr(aprefix as *const i8).to_string_lossy());
                        let loc = CStr::from_ptr(attname as *const i8).to_string_lossy();
                        xml_err_attribute_dup(ctxt, pre.as_deref(), &loc);
                    } else if (*ctxt).ns_push(attname, url) > 0 {
                        nb_ns += 1;
                    }
                } else {
                    // Add the pair to atts
                    if (atts.is_null() || nbatts + 5 > maxatts)
                        && grow_attrs(ctxt, &mut atts, &mut attallocs, &mut maxatts, nbatts + 5) < 0
                    {
                        break 'next_attr;
                    }

                    *attallocs.add(nratts as usize) = alloc;
                    nratts += 1;
                    *atts.add(nbatts as usize) = attname;
                    nbatts += 1;
                    *atts.add(nbatts as usize) = aprefix;
                    nbatts += 1;
                    // The namespace URI field is used temporarily to point at the
                    // base of the current input buffer for non-alloced attributes.
                    // When the input buffer is reallocated, all the pointers become
                    // invalid, but they can be reconstructed later.
                    if alloc != 0 {
                        *atts.add(nbatts as usize) = null_mut();
                        nbatts += 1;
                    } else {
                        *atts.add(nbatts as usize) = (*(*ctxt).input).base;
                        nbatts += 1;
                    }
                    *atts.add(nbatts as usize) = attvalue;
                    nbatts += 1;
                    attvalue = attvalue.add(len as usize);
                    *atts.add(nbatts as usize) = attvalue;
                    nbatts += 1;
                    // tag if some deallocation is needed
                    if alloc != 0 {
                        attval = 1;
                    }
                    attvalue = null_mut(); /* moved into atts */
                }
            }

            //  next_attr:
            if !attvalue.is_null() && alloc != 0 {
                xml_free(attvalue as _);
                attvalue = null_mut();
            }

            (*ctxt).grow();
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                break;
            }
            if (*ctxt).current_byte() == b'>'
                || ((*ctxt).current_byte() == b'/' && (*ctxt).nth_byte(1) == b'>')
            {
                break;
            }
            if (*ctxt).skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "attributes construct error\n",
                );
                break;
            }
            (*ctxt).grow();
        }

        if (*(*ctxt).input).id != inputid {
            xml_fatal_err(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                Some("Unexpected change of input\n"),
            );
            localname = null_mut();
            break 'done;
        }

        // Reconstruct attribute value pointers.
        for (i, _) in (0..).step_by(5).zip(0..nratts) {
            if !(*atts.add(i + 2)).is_null() {
                // Arithmetic on dangling pointers is technically undefined behavior, but well...
                let old: *const XmlChar = *atts.add(i + 2);
                *atts.add(i + 2) = null_mut(); /* Reset repurposed namespace URI */
                *atts.add(i + 3) = (*(*ctxt).input)
                    .base
                    .add((*atts.add(i + 3)).offset_from(old) as _); /* value */
                *atts.add(i + 4) = (*(*ctxt).input)
                    .base
                    .add((*atts.add(i + 4)).offset_from(old) as _);
                /* valuend */
            }
        }

        // The attributes defaulting
        if let Some(atts_default) = (*ctxt).atts_default {
            let defaults = atts_default.lookup2(
                CStr::from_ptr(localname as *const i8),
                (!prefix.is_null()).then(|| CStr::from_ptr(prefix as *const i8)),
            );
            if let Some(defaults) = defaults.copied() {
                'b: for i in 0..(*defaults).nb_attrs as usize {
                    attname = *(*defaults).values.as_ptr().add(5 * i);
                    aprefix = *(*defaults).values.as_ptr().add(5 * i + 1);

                    // special work for namespaces defaulted defs
                    if attname == (*ctxt).str_xmlns && aprefix.is_null() {
                        // check that it's not a defined namespace
                        for j in 1..=nb_ns {
                            if (*ctxt).ns_tab[(*ctxt).ns_tab.len() - 2 * j as usize].is_null() {
                                continue 'b;
                            }
                        }

                        nsname = xml_get_namespace(ctxt, null_mut());
                        if nsname != *(*defaults).values.as_ptr().add(5 * i + 2)
                            && (*ctxt)
                                .ns_push(null_mut(), *(*defaults).values.as_ptr().add(5 * i + 2))
                                > 0
                        {
                            nb_ns += 1;
                        }
                    } else if aprefix == (*ctxt).str_xmlns {
                        // check that it's not a defined namespace
                        for j in 1..=nb_ns {
                            if (*ctxt).ns_tab[(*ctxt).ns_tab.len() - 2 * j as usize] == attname {
                                continue 'b;
                            }
                        }

                        nsname = xml_get_namespace(ctxt, attname);
                        if nsname != *(*defaults).values.as_ptr().add(5 * i + 2)
                            && (*ctxt).ns_push(attname, *(*defaults).values.as_ptr().add(5 * i + 2))
                                > 0
                        {
                            nb_ns += 1;
                        }
                    } else {
                        // check that it's not a defined attribute
                        for j in (0..nbatts).step_by(5) {
                            if attname == *atts.add(j as usize)
                                && aprefix == *atts.add(j as usize + 1)
                            {
                                continue 'b;
                            }
                        }

                        if (atts.is_null() || nbatts + 5 > maxatts)
                            && grow_attrs(ctxt, &mut atts, &mut attallocs, &mut maxatts, nbatts + 5)
                                < 0
                        {
                            localname = null_mut();
                            break 'done;
                        }
                        *atts.add(nbatts as usize) = attname;
                        nbatts += 1;
                        *atts.add(nbatts as usize) = aprefix;
                        nbatts += 1;
                        if aprefix.is_null() {
                            *atts.add(nbatts as usize) = null_mut();
                            nbatts += 1;
                        } else {
                            *atts.add(nbatts as usize) = xml_get_namespace(ctxt, aprefix);
                            nbatts += 1;
                        }
                        *atts.add(nbatts as usize) = *(*defaults).values.as_ptr().add(5 * i + 2);
                        nbatts += 1;
                        *atts.add(nbatts as usize) = *(*defaults).values.as_ptr().add(5 * i + 3);
                        nbatts += 1;
                        if (*ctxt).standalone == 1
                            && !(*(*defaults).values.as_ptr().add(5 * i + 4)).is_null()
                        {
                            xml_validity_error!(
                                ctxt,
                                XmlParserErrors::XmlDTDStandaloneDefaulted,
                                "standalone: attribute {} on {} defaulted from external subset\n",
                                CStr::from_ptr(attname as *const i8).to_string_lossy(),
                                CStr::from_ptr(localname as *const i8).to_string_lossy()
                            );
                        }
                        nbdef += 1;
                    }
                }
            }
        }

        // The attributes checkings
        for i in (0..nbatts as usize).step_by(5) {
            // The default namespace does not apply to attribute names.
            if !(*atts.add(i + 1)).is_null() {
                nsname = xml_get_namespace(ctxt, *atts.add(i + 1));
                if nsname.is_null() {
                    let pre = CStr::from_ptr(*atts.add(i + 1) as *const i8).to_string_lossy();
                    let loc = CStr::from_ptr(*atts.add(i) as *const i8).to_string_lossy();
                    let localname = CStr::from_ptr(localname as *const i8).to_string_lossy();
                    xml_ns_err!(
                        ctxt,
                        XmlParserErrors::XmlNsErrUndefinedNamespace,
                        "Namespace prefix {} for {} on {} is not defined\n",
                        pre,
                        loc,
                        localname
                    );
                }
                *atts.add(i + 2) = nsname;
            } else {
                nsname = null_mut();
            }
            // [ WFC: Unique Att Spec ]
            // No attribute name may appear more than once in the same
            // start-tag or empty-element tag.
            // As extended by the Namespace in XML REC.
            for j in (0..i).step_by(5) {
                if *atts.add(i) == *atts.add(j) {
                    if *atts.add(i + 1) == *atts.add(j + 1) {
                        let pre = *atts.add(i + 1);
                        let pre = (!pre.is_null())
                            .then(|| CStr::from_ptr(pre as *const i8).to_string_lossy());
                        let loc = *atts.add(i);
                        let loc = CStr::from_ptr(loc as *const i8).to_string_lossy();
                        xml_err_attribute_dup(ctxt, pre.as_deref(), loc.as_ref());
                        break;
                    }
                    if !nsname.is_null() && *atts.add(j + 2) == nsname {
                        let loc = CStr::from_ptr(*atts.add(i) as *const i8).to_string_lossy();
                        let nsname = CStr::from_ptr(nsname as *const i8).to_string_lossy();
                        xml_ns_err!(
                            ctxt,
                            XmlParserErrors::XmlNsErrAttributeRedefined,
                            "Namespaced Attribute {} in '{}' redefined\n",
                            loc,
                            nsname
                        );
                        break;
                    }
                }
            }
        }

        nsname = xml_get_namespace(ctxt, prefix);
        if !prefix.is_null() && nsname.is_null() {
            let prefix = CStr::from_ptr(prefix as *const i8).to_string_lossy();
            let localname = CStr::from_ptr(localname as *const i8).to_string_lossy();
            xml_ns_err!(
                ctxt,
                XmlParserErrors::XmlNsErrUndefinedNamespace,
                "Namespace prefix {} on {} is not defined\n",
                prefix,
                localname
            );
        }
        *pref = prefix;
        *uri = nsname;

        // SAX: Start of Element !
        if !(*ctxt).sax.is_null()
            && (*(*ctxt).sax).start_element_ns.is_some()
            && (*ctxt).disable_sax == 0
        {
            let localname = &CStr::from_ptr(localname as *const i8)
                .to_string_lossy()
                .into_owned();
            if nb_ns > 0 {
                ((*(*ctxt).sax).start_element_ns.unwrap())(
                    (*ctxt).user_data.clone(),
                    localname.as_str(),
                    (!prefix.is_null())
                        .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                        .as_deref(),
                    nsname,
                    nb_ns,
                    &raw mut (*ctxt).ns_tab[(*ctxt).ns_tab.len() - 2 * nb_ns as usize],
                    nbatts / 5,
                    nbdef,
                    atts,
                );
            } else {
                ((*(*ctxt).sax).start_element_ns.unwrap())(
                    (*ctxt).user_data.clone(),
                    localname.as_str(),
                    (!prefix.is_null())
                        .then(|| CStr::from_ptr(prefix as *const i8).to_string_lossy())
                        .as_deref(),
                    nsname,
                    0,
                    null_mut(),
                    nbatts / 5,
                    nbdef,
                    atts,
                );
            }
        }
    }

    //  done:
    // Free up attribute allocated strings if needed
    if attval != 0 {
        for (i, j) in (3..).step_by(5).zip(0..nratts) {
            if *attallocs.add(j as usize) != 0 && !(*atts.add(i)).is_null() {
                xml_free(*atts.add(i) as _);
            }
        }
    }

    if !atts.is_null() {
        xml_free(atts as _);
    }
    if !attallocs.is_null() {
        xml_free(attallocs as _);
    }

    localname
}

/// Check whether the input buffer contains a character.
#[doc(alias = "xmlParseLookupChar")]
unsafe extern "C" fn xml_parse_lookup_char(ctxt: XmlParserCtxtPtr, c: i32) -> i32 {
    let cur = if (*ctxt).check_index == 0 {
        (*(*ctxt).input).cur.add(1)
    } else {
        (*(*ctxt).input).cur.add((*ctxt).check_index as usize)
    };

    if memchr(cur as _, c, (*(*ctxt).input).end.offset_from(cur) as _).is_null() {
        let index: size_t = (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) as _;

        if index > i64::MAX as usize {
            (*ctxt).check_index = 0;
            return 1;
        }
        (*ctxt).check_index = index as _;
        0
    } else {
        (*ctxt).check_index = 0;
        1
    }
}

/// Check whether the input buffer contains terminated c_char data.
#[doc(alias = "xmlParseLookupCharData")]
unsafe extern "C" fn xml_parse_lookup_char_data(ctxt: XmlParserCtxtPtr) -> i32 {
    let mut cur: *const XmlChar = (*(*ctxt).input).cur.add((*ctxt).check_index as usize);
    let end: *const XmlChar = (*(*ctxt).input).end;

    while cur < end {
        if *cur == b'<' || *cur == b'&' {
            (*ctxt).check_index = 0;
            return 1;
        }
        cur = cur.add(1);
    }

    let index: size_t = cur.offset_from((*(*ctxt).input).cur) as _;
    if index > i64::MAX as usize {
        (*ctxt).check_index = 0;
        return 1;
    }
    (*ctxt).check_index = index as _;
    0
}

// used for the test in the inner loop of the c_char data testing
const TEST_CHAR_DATA: [u8; 256] = [
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, /* 0x9, CR/LF separated */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x00, 0x27, /* & */
    0x28, 0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
    0x38, 0x39, 0x3A, 0x3B, 0x00, 0x3D, 0x3E, 0x3F, /* < */
    0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4A, 0x4B, 0x4C, 0x4D, 0x4E, 0x4F,
    0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5A, 0x5B, 0x5C, 0x00, 0x5E,
    0x5F, /* ] */
    0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6A, 0x6B, 0x6C, 0x6D, 0x6E, 0x6F,
    0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7A, 0x7B, 0x7C, 0x7D, 0x7E, 0x7F,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, /* non-ascii */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
];

/// Is this a sequence of blank chars that one can ignore ?
///
/// Returns 1 if ignorable 0 otherwise.
#[doc(alias = "areBlanks")]
unsafe extern "C" fn are_blanks(
    ctxt: XmlParserCtxtPtr,
    str: *const XmlChar,
    len: i32,
    blank_chars: i32,
) -> i32 {
    let ret: i32;

    /*
     * Don't spend time trying to differentiate them, the same callback is
     * used !
     */
    if (*(*ctxt).sax).ignorable_whitespace == (*(*ctxt).sax).characters {
        return 0;
    }

    /*
     * Check for xml:space value.
     */
    if (*ctxt).space() == 1 || (*ctxt).space() == -2 {
        return 0;
    }

    /*
     * Check that the string is made of blanks
     */
    if blank_chars == 0 {
        for i in 0..len {
            if !xml_is_blank_char(*str.add(i as usize) as u32) {
                return 0;
            }
        }
    }

    /*
     * Look if the element is mixed content in the DTD if available
     */
    if (*ctxt).node.is_null() {
        return 0;
    }
    if !(*ctxt).my_doc.is_null() {
        ret = xml_is_mixed_element((*ctxt).my_doc, (*(*ctxt).node).name);
        if ret == 0 {
            return 1;
        }
        if ret == 1 {
            return 0;
        }
    }

    // Otherwise, heuristic :-\
    if (*ctxt).current_byte() != b'<' && (*ctxt).current_byte() != 0xD {
        return 0;
    }
    if (*(*ctxt).node).children().is_none()
        && (*ctxt).current_byte() == b'<'
        && (*ctxt).nth_byte(1) == b'/'
    // index out of bound may occur at this `nth_byte` ??? It may be necessary to fix.
    {
        return 0;
    }

    let last_child: XmlNodePtr = (*(*ctxt).node).get_last_child();
    if last_child.is_null() {
        if (*(*ctxt).node).element_type() != XmlElementType::XmlElementNode
            && !(*(*ctxt).node).content.is_null()
        {
            return 0;
        }
    } else if (*last_child).is_text_node()
        || (*(*ctxt).node)
            .children()
            .filter(|c| c.is_text_node())
            .is_some()
    {
        return 0;
    }
    1
}

/// Always makes progress if the first c_char isn't '<' or '&'.
///
/// parse a CharData section.this is the fallback function
/// of xmlParseCharData() when the parsing requires handling
/// of non-ASCII characters.
#[doc(alias = "xmlParseCharDataComplex")]
unsafe fn xml_parse_char_data_complex(ctxt: XmlParserCtxtPtr, partial: i32) {
    let mut buf: [XmlChar; XML_PARSER_BIG_BUFFER_SIZE + 5] = [0; XML_PARSER_BIG_BUFFER_SIZE + 5];
    let mut nbchar: i32 = 0;
    let mut l: i32 = 0;

    let mut cur = (*ctxt).current_char(&mut l).unwrap_or('\0');
    // test also done in xmlCurrentChar()
    while cur != '<' && cur != '&' && xml_is_char(cur as u32) {
        if cur == ']' && (*ctxt).nth_byte(1) == b']' && (*ctxt).nth_byte(2) == b'>' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrMisplacedCDATAEnd, None);
        }
        COPY_BUF!(l, buf.as_mut_ptr(), nbchar, cur);
        // move current position before possible calling of (*(*ctxt).sax).characters
        (*ctxt).advance_with_line_handling(l as usize);
        if nbchar >= XML_PARSER_BIG_BUFFER_SIZE as i32 {
            buf[nbchar as usize] = 0;

            // OK the segment is to be consumed as chars.
            if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                if are_blanks(ctxt, buf.as_ptr(), nbchar, 0) != 0 {
                    if let Some(ignorable_whitespace) = (*(*ctxt).sax).ignorable_whitespace {
                        let s = from_utf8(&buf[..nbchar as usize]).expect("Internal Error");
                        ignorable_whitespace((*ctxt).user_data.clone(), s);
                    }
                } else {
                    if let Some(characters) = (*(*ctxt).sax).characters {
                        let s = from_utf8(&buf[..nbchar as usize]).expect("Internal Error");
                        characters((*ctxt).user_data.clone(), s);
                    }
                    if (*(*ctxt).sax).characters != (*(*ctxt).sax).ignorable_whitespace
                        && (*ctxt).space() == -1
                    {
                        *(*ctxt).space_mut() = -2;
                    }
                }
            }
            nbchar = 0;
            // something really bad happened in the SAX callback
            if !matches!((*ctxt).instate, XmlParserInputState::XmlParserContent) {
                return;
            }
            (*ctxt).shrink();
        }
        cur = (*ctxt).current_char(&mut l).unwrap_or('\0');
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }
    if nbchar != 0 {
        buf[nbchar as usize] = 0;
        // OK the segment is to be consumed as chars.
        if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
            if are_blanks(ctxt, buf.as_ptr(), nbchar, 0) != 0 {
                if let Some(ignorable_whitespace) = (*(*ctxt).sax).ignorable_whitespace {
                    let s = from_utf8(&buf[..nbchar as usize]).expect("Internal Error");
                    ignorable_whitespace((*ctxt).user_data.clone(), s);
                }
            } else {
                if let Some(characters) = (*(*ctxt).sax).characters {
                    let s = from_utf8(&buf[..nbchar as usize]).expect("Internal Error");
                    characters((*ctxt).user_data.clone(), s);
                }
                if (*(*ctxt).sax).characters != (*(*ctxt).sax).ignorable_whitespace
                    && (*ctxt).space() == -1
                {
                    *(*ctxt).space_mut() = -2;
                }
            }
        }
    }
    // cur == 0 can mean
    //
    // - xmlParserInputState::XmlParserEOF or memory error. This is checked above.
    // - An actual 0 character.
    // - End of buffer.
    // - An incomplete UTF-8 sequence. This is allowed if partial is set.
    if (*(*ctxt).input).cur < (*(*ctxt).input).end {
        if cur == '\0' && (*ctxt).current_byte() != 0 {
            if partial == 0 {
                xml_fatal_err_msg_int!(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidChar,
                    format!(
                        "Incomplete UTF-8 sequence starting with {:02X}\n",
                        (*ctxt).current_byte()
                    )
                    .as_str(),
                    (*ctxt).current_byte() as i32
                );
                (*ctxt).advance_with_line_handling(1);
            }
        } else if cur != '<' && cur != '&' {
            // Generate the error and skip the offending character
            xml_fatal_err_msg_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                format!("PCDATA invalid Char value {}\n", cur as i32).as_str(),
                cur as i32
            );
            (*ctxt).advance_with_line_handling(l as usize);
        }
    }
}

/// Parse character data. Always makes progress if the first c_char isn't '<' or '&'.
///
/// The right angle bracket (>) may be represented using the string "&gt;".as_ptr() as _,
/// and must, for compatibility, be escaped using "&gt;" or a character
/// reference when it appears in the string "]]>" in content, when that
/// string is not marking the end of a CDATA section.
///
/// `[14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)`
#[doc(alias = "xmlParseCharDataInternal")]
pub(crate) unsafe extern "C" fn xml_parse_char_data_internal(ctxt: XmlParserCtxtPtr, partial: i32) {
    let mut input: *const XmlChar;
    let mut nbchar: i32;
    let mut line: i32 = (*(*ctxt).input).line;
    let mut col: i32 = (*(*ctxt).input).col;
    let mut ccol: i32;

    (*ctxt).grow();
    /*
     * Accelerated common case where input don't need to be
     * modified before passing it to the handler.
     */
    input = (*(*ctxt).input).cur;
    'main: while {
        // get_more_space:
        'get_more_space: loop {
            while *input == 0x20 {
                input = input.add(1);
                (*(*ctxt).input).col += 1;
            }
            if *input == 0xA {
                while {
                    (*(*ctxt).input).line += 1;
                    (*(*ctxt).input).col = 1;
                    input = input.add(1);
                    *input == 0xA
                } {}
                // goto get_more_space;
                continue 'get_more_space;
            }

            break;
        }
        if *input == b'<' {
            nbchar = input.offset_from((*(*ctxt).input).cur) as _;
            if nbchar > 0 {
                let tmp: *const XmlChar = (*(*ctxt).input).cur;
                (*(*ctxt).input).cur = input;

                if !(*ctxt).sax.is_null()
                    && (*(*ctxt).sax).ignorable_whitespace != (*(*ctxt).sax).characters
                {
                    if are_blanks(ctxt, tmp, nbchar, 1) != 0 {
                        if let Some(ignorable_whitespace) = (*(*ctxt).sax).ignorable_whitespace {
                            let s = from_utf8(from_raw_parts(tmp, nbchar as usize))
                                .expect("Internal Error");
                            ignorable_whitespace((*ctxt).user_data.clone(), s);
                        }
                    } else {
                        if let Some(characters) = (*(*ctxt).sax).characters {
                            let s = from_utf8(from_raw_parts(tmp, nbchar as usize))
                                .expect("Internal Error");
                            characters((*ctxt).user_data.clone(), s);
                        }
                        if (*ctxt).space() == -1 {
                            *(*ctxt).space_mut() = -2;
                        }
                    }
                } else if !(*ctxt).sax.is_null() && (*(*ctxt).sax).characters.is_some() {
                    let s =
                        from_utf8(from_raw_parts(tmp, nbchar as usize)).expect("Internal Error");
                    ((*(*ctxt).sax).characters.unwrap())((*ctxt).user_data.clone(), s);
                }
            }
            return;
        }

        // get_more:
        'get_more: loop {
            ccol = (*(*ctxt).input).col;
            while TEST_CHAR_DATA[*input as usize] != 0 {
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
            if *input == b']' {
                if *input.add(1) == b']' && *input.add(2) == b'>' {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrMisplacedCDATAEnd, None);
                    if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                        (*(*ctxt).input).cur = input.add(1);
                    }
                    return;
                }
                input = input.add(1);
                (*(*ctxt).input).col += 1;
                // goto get_more;
                continue 'get_more;
            }

            break;
        }
        nbchar = input.offset_from((*(*ctxt).input).cur) as _;
        if nbchar > 0 {
            if !(*ctxt).sax.is_null()
                && (*(*ctxt).sax).ignorable_whitespace != (*(*ctxt).sax).characters
                && xml_is_blank_char(*(*(*ctxt).input).cur as u32)
            {
                let tmp: *const XmlChar = (*(*ctxt).input).cur;
                (*(*ctxt).input).cur = input;

                if are_blanks(ctxt, tmp, nbchar, 0) != 0 {
                    if let Some(ignorable_whitespace) = (*(*ctxt).sax).ignorable_whitespace {
                        let s = from_utf8(from_raw_parts(tmp, nbchar as usize))
                            .expect("Internal Error");
                        ignorable_whitespace((*ctxt).user_data.clone(), s);
                    }
                } else {
                    if let Some(characters) = (*(*ctxt).sax).characters {
                        let s = from_utf8(from_raw_parts(tmp, nbchar as usize))
                            .expect("Internal Error");
                        characters((*ctxt).user_data.clone(), s);
                    }
                    if (*ctxt).space() == -1 {
                        *(*ctxt).space_mut() = -2;
                    }
                }
                line = (*(*ctxt).input).line;
                col = (*(*ctxt).input).col;
            } else if !(*ctxt).sax.is_null() {
                if let Some(characters) = (*(*ctxt).sax).characters {
                    let s = from_utf8(from_raw_parts((*(*ctxt).input).cur, nbchar as usize))
                        .expect("Internal Error");
                    characters((*ctxt).user_data.clone(), s);
                }
                line = (*(*ctxt).input).line;
                col = (*(*ctxt).input).col;
            }
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                return;
            }
        }
        (*(*ctxt).input).cur = input;
        if *input == 0xD {
            input = input.add(1);
            if *input == 0xA {
                (*(*ctxt).input).cur = input;
                input = input.add(1);
                (*(*ctxt).input).line += 1;
                (*(*ctxt).input).col = 1;
                continue 'main; /* while */
            }
            input = input.sub(1);
        }
        if *input == b'<' {
            return;
        }
        if *input == b'&' {
            return;
        }
        (*ctxt).shrink();
        (*ctxt).grow();
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            return;
        }
        input = (*(*ctxt).input).cur;

        (*input >= 0x20 && *input <= 0x7F) || *input == 0x09 || *input == 0x0a
    } {}

    (*(*ctxt).input).line = line;
    (*(*ctxt).input).col = col;
    xml_parse_char_data_complex(ctxt, partial);
}

/// Parse an XML name and compares for match (specialized for endtag parsing)
///
/// Returns NULL for an illegal name, (XmlChar*) 1 for success
/// and the name for mismatch
#[doc(alias = "xmlParseNameAndCompare")]
unsafe extern "C" fn xml_parse_name_and_compare(
    ctxt: XmlParserCtxtPtr,
    other: *mut XmlChar,
) -> *const XmlChar {
    let mut cmp: *const XmlChar = other;
    let mut input: *const XmlChar;

    (*ctxt).grow();
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }

    input = (*(*ctxt).input).cur;
    while *input != 0 && *input == *cmp {
        input = input.add(1);
        cmp = cmp.add(1);
    }
    if *cmp == 0 && (*input == b'>' || xml_is_blank_char(*input as u32)) {
        /* success */
        (*(*ctxt).input).col += input.offset_from((*(*ctxt).input).cur) as i32;
        (*(*ctxt).input).cur = input;
        return 1 as *const XmlChar;
    }
    /* failure (or end of input buffer), check with full function */
    let ret: *const XmlChar = xml_parse_name(ctxt);
    /* strings coming from the dictionary direct compare possible */
    if ret == other {
        return 1 as *const XmlChar;
    }
    ret
}

/// Parse an XML name and compares for match
/// (specialized for endtag parsing)
///
/// Returns NULL for an illegal name, (XmlChar*) 1 for success
/// and the name for mismatch
#[doc(alias = "xmlParseQNameAndCompare")]
unsafe extern "C" fn xml_parse_qname_and_compare(
    ctxt: XmlParserCtxtPtr,
    name: *mut XmlChar,
    prefix: *mut XmlChar,
) -> *const XmlChar {
    let mut cmp: *const XmlChar;
    let mut input: *const XmlChar;
    let mut prefix2: *const XmlChar = null();

    if prefix.is_null() {
        return xml_parse_name_and_compare(ctxt, name);
    }

    (*ctxt).grow();
    input = (*(*ctxt).input).cur;

    cmp = prefix;
    while *input != 0 && *input == *cmp {
        input = input.add(1);
        cmp = cmp.add(1);
    }
    if *cmp == 0 && *input == b':' {
        input = input.add(1);
        cmp = name;
        while *input != 0 && *input == *cmp {
            input = input.add(1);
            cmp = cmp.add(1);
        }
        if *cmp == 0 && (*input == b'>' || xml_is_blank_char(*input as u32)) {
            /* success */
            (*(*ctxt).input).col += input.offset_from((*(*ctxt).input).cur) as i32;
            (*(*ctxt).input).cur = input;
            return 1 as *const XmlChar;
        }
    }
    /*
     * all strings coms from the dictionary, equality can be done directly
     */
    let ret: *const XmlChar = xml_parse_qname(ctxt, addr_of_mut!(prefix2));
    if ret == name && prefix == prefix2 as _ {
        return 1 as *const XmlChar;
    }
    ret
}

/// Parse an end tag. Always consumes '</'.
///
/// `[42] ETag ::= '</' Name S? '>'`
///
/// With namespace
///
/// `[NS 9] ETag ::= '</' QName S? '>'`
#[doc(alias = "xmlParseEndTag2")]
pub(crate) unsafe extern "C" fn xml_parse_end_tag2(ctxt: XmlParserCtxtPtr, tag: &XmlStartTag) {
    let mut name: *const XmlChar;

    (*ctxt).grow();
    if (*ctxt).current_byte() != b'<' || (*ctxt).nth_byte(1) != b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrLtSlashRequired, None);
        return;
    }
    (*ctxt).advance(2);

    if tag.prefix.is_null() {
        name = xml_parse_name_and_compare(ctxt, (*ctxt).name as _);
    } else {
        name = xml_parse_qname_and_compare(ctxt, (*ctxt).name as _, tag.prefix as _);
    }

    /*
     * We should definitely be at the ending "S? '>'" part
     */
    (*ctxt).grow();
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }
    (*ctxt).skip_blanks();
    if !xml_is_char((*ctxt).current_byte() as u32) || (*ctxt).current_byte() != b'>' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrGtRequired, None);
    } else {
        (*ctxt).advance(1);
    }

    // [ WFC: Element Type Match ]
    // The Name in an element's end-tag must match the element type in the start-tag.
    if name != 1 as *mut XmlChar {
        if name.is_null() {
            name = c"unparsable".as_ptr() as _;
        }
        xml_fatal_err_msg_str_int_str!(
            ctxt,
            XmlParserErrors::XmlErrTagNameMismatch,
            "Opening and ending tag mismatch: {} line {} and {}\n",
            CStr::from_ptr((*ctxt).name as *const i8).to_string_lossy(),
            tag.line,
            CStr::from_ptr(name as *const i8).to_string_lossy()
        );
    }

    // SAX: End of Tag
    if !(*ctxt).sax.is_null() && (*(*ctxt).sax).end_element_ns.is_some() && (*ctxt).disable_sax == 0
    {
        let name = CStr::from_ptr((*ctxt).name as *const i8).to_string_lossy();
        ((*(*ctxt).sax).end_element_ns.unwrap())(
            (*ctxt).user_data.clone(),
            &name,
            tag.prefix,
            tag.uri,
        );
    }

    (*ctxt).space_pop();
    if tag.ns_nr != 0 {
        (*ctxt).ns_pop(tag.ns_nr as usize);
    }
}

/// Parse an end tag. Always consumes '</'.
///
/// `[42] ETag ::= '</' Name S? '>'`
///
/// With namespace
///
/// `[NS 9] ETag ::= '</' QName S? '>'`
#[doc(alias = "xmlParseEndTag1")]
#[cfg(feature = "sax1")]
pub(crate) unsafe fn xml_parse_end_tag1(ctxt: XmlParserCtxtPtr, line: i32) {
    let mut name: *const XmlChar;

    (*ctxt).grow();
    if (*ctxt).current_byte() != b'<' || (*ctxt).nth_byte(1) != b'/' {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrLtSlashRequired,
            "xmlParseEndTag: '</' not found\n",
        );
        return;
    }
    (*ctxt).advance(2);

    name = xml_parse_name_and_compare(ctxt, (*ctxt).name as _);

    // We should definitely be at the ending "S? '>'" part
    (*ctxt).grow();
    (*ctxt).skip_blanks();
    if !xml_is_char((*ctxt).current_byte() as u32) || (*ctxt).current_byte() != b'>' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrGtRequired, None);
    } else {
        (*ctxt).advance(1);
    }

    // [ WFC: Element Type Match ]
    // The Name in an element's end-tag must match the element type in the start-tag.
    if name != 1 as *mut XmlChar {
        if name.is_null() {
            name = c"unparsable".as_ptr() as _;
        }
        xml_fatal_err_msg_str_int_str!(
            ctxt,
            XmlParserErrors::XmlErrTagNameMismatch,
            "Opening and ending tag mismatch: {} line {} and {}\n",
            CStr::from_ptr((*ctxt).name as *const i8).to_string_lossy(),
            line,
            CStr::from_ptr(name as *const i8).to_string_lossy()
        );
    }

    // SAX: End of Tag
    if !(*ctxt).sax.is_null() && (*(*ctxt).sax).end_element.is_some() && (*ctxt).disable_sax == 0 {
        let name = (*ctxt).name;
        let name = CStr::from_ptr(name as *const i8).to_string_lossy();
        ((*(*ctxt).sax).end_element.unwrap())((*ctxt).user_data.clone(), &name);
    }

    (*ctxt).name_pop();
    (*ctxt).space_pop();
}

/// Check that the block of characters is okay as SCdata content [20]
///
/// Returns the number of bytes to pass if okay, a negative index where an
/// UTF-8 error occurred otherwise
#[doc(alias = "xmlCheckCdataPush")]
#[cfg(feature = "libxml_push")]
unsafe extern "C" fn xml_check_cdata_push(utf: *const XmlChar, len: i32, complete: i32) -> i32 {
    use super::chvalid::xml_is_char;

    let mut ix: i32;
    let mut c: u8;
    let mut codepoint: i32;

    if utf.is_null() || len <= 0 {
        return 0;
    }

    ix = 0;
    while ix < len {
        /* string is 0-terminated */
        c = *utf.add(ix as usize);
        if c & 0x80 == 0x00 {
            /* 1-byte code, starts with 10 */
            if c >= 0x20 || (c == 0xA || c == 0xD || c == 0x9) {
                ix += 1;
            } else {
                return -ix;
            }
        } else if c & 0xe0 == 0xc0 {
            /* 2-byte code, starts with 110 */
            if ix + 2 > len {
                return if complete != 0 { -ix } else { ix };
            }
            if *utf.add(ix as usize + 1) & 0xc0 != 0x80 {
                return -ix;
            }
            codepoint = (*utf.add(ix as usize) as i32 & 0x1f) << 6;
            codepoint |= *utf.add(ix as usize + 1) as i32 & 0x3f;
            if !xml_is_char(codepoint as u32) {
                return -ix;
            }
            ix += 2;
        } else if c & 0xf0 == 0xe0 {
            /* 3-byte code, starts with 1110 */
            if ix + 3 > len {
                return if complete != 0 { -ix } else { ix };
            }
            if *utf.add(ix as usize + 1) & 0xc0 != 0x80 || *utf.add(ix as usize + 2) & 0xc0 != 0x80
            {
                return -ix;
            }
            codepoint = (*utf.add(ix as usize) as i32 & 0xf) << 12;
            codepoint |= (*utf.add(ix as usize + 1) as i32 & 0x3f) << 6;
            codepoint |= *utf.add(ix as usize + 2) as i32 & 0x3f;
            if !xml_is_char(codepoint as u32) {
                return -ix;
            }
            ix += 3;
        } else if c & 0xf8 == 0xf0 {
            /* 4-byte code, starts with 11110 */
            if ix + 4 > len {
                return if complete != 0 { -ix } else { ix };
            }
            if *utf.add(ix as usize + 1) & 0xc0 != 0x80
                || *utf.add(ix as usize + 2) & 0xc0 != 0x80
                || *utf.add(ix as usize + 3) & 0xc0 != 0x80
            {
                return -ix;
            }
            codepoint = (*utf.add(ix as usize) as i32 & 0x7) << 18;
            codepoint |= (*utf.add(ix as usize + 1) as i32 & 0x3f) << 12;
            codepoint |= (*utf.add(ix as usize + 2) as i32 & 0x3f) << 6;
            codepoint |= *utf.add(ix as usize + 3) as i32 & 0x3f;
            if !xml_is_char(codepoint as u32) {
                return -ix;
            }
            ix += 4;
        } else
        /* unknown encoding */
        {
            return -ix;
        }
    }
    ix
}

/// Check whether there's enough data in the input buffer to finish parsing the internal subset.
#[doc(alias = "xmlParseLookupInternalSubset")]
#[cfg(feature = "libxml_push")]
unsafe extern "C" fn xml_parse_lookup_internal_subset(ctxt: XmlParserCtxtPtr) -> i32 {
    /*
     * Sorry, but progressive parsing of the internal subset is not
     * supported. We first check that the full content of the internal
     * subset is available and parsing is launched only at that point.
     * Internal subset ends with "']' S? '>'" in an unescaped section and
     * not in a ']]>' sequence which are conditional sections.
     */
    let mut cur: *const XmlChar;
    let mut start: *const XmlChar;
    let end: *const XmlChar = (*(*ctxt).input).end;
    let mut state: i32 = (*ctxt).end_check_state;

    if (*ctxt).check_index == 0 {
        cur = (*(*ctxt).input).cur.add(1);
    } else {
        cur = (*(*ctxt).input).cur.add((*ctxt).check_index as usize);
    }
    start = cur;

    while cur < end {
        if state == b'-' as i32 {
            if *cur == b'-' && *cur.add(1) == b'-' && *cur.add(2) == b'>' {
                state = 0;
                cur = cur.add(3);
                start = cur;
                continue;
            }
        } else if state == b']' as i32 {
            if *cur == b'>' {
                (*ctxt).check_index = 0;
                (*ctxt).end_check_state = 0;
                return 1;
            }
            if xml_is_blank_char(*cur as u32) {
                state = b' ' as i32;
            } else if *cur != b']' {
                state = 0;
                start = cur;
                continue;
            }
        } else if state == b' ' as i32 {
            if *cur == b'>' {
                (*ctxt).check_index = 0;
                (*ctxt).end_check_state = 0;
                return 1;
            }
            if !xml_is_blank_char(*cur as u32) {
                state = 0;
                start = cur;
                continue;
            }
        } else if state != 0 {
            if *cur as i32 == state {
                state = 0;
                start = cur.add(1);
            }
        } else if *cur == b'<' {
            if *cur.add(1) == b'!' && *cur.add(2) == b'-' && *cur.add(3) == b'-' {
                state = b'-' as i32;
                cur = cur.add(4);
                /* Don't treat <!--> as comment */
                start = cur;
                continue;
            }
        } else if *cur == b'"' || *cur == b'\'' || *cur == b']' {
            state = *cur as _;
        }

        cur = cur.add(1);
    }

    /*
     * Rescan the three last characters to detect "<!--" and "-->"
     * split across chunks.
     */
    if state == 0 || state == b'-' as i32 {
        if cur.offset_from(start) < 3 {
            cur = start;
        } else {
            cur = cur.sub(3);
        }
    }
    let index: size_t = cur.offset_from((*(*ctxt).input).cur) as _;
    if index > i64::MAX as usize {
        (*ctxt).check_index = 0;
        (*ctxt).end_check_state = 0;
        return 1;
    }
    (*ctxt).check_index = index as _;
    (*ctxt).end_check_state = state;
    0
}

/// Try to progress on parsing
///
/// Returns zero if no parsing was possible
#[doc(alias = "xmlParseTryOrFinish")]
unsafe fn xml_parse_try_or_finish(ctxt: XmlParserCtxtPtr, terminate: i32) -> i32 {
    let mut ret: i32 = 0;
    let mut tlen: i32 = 0;
    let mut avail: size_t;
    let mut cur: XmlChar;
    let mut next: XmlChar;

    if (*ctxt).input.is_null() {
        return 0;
    }

    if !(*ctxt).input.is_null() && (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) > 4096 {
        (*ctxt).force_shrink();
    }

    'encoding_error: {
        while !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            if (*ctxt).err_no != XmlParserErrors::XmlErrOK as i32 && (*ctxt).disable_sax == 1 {
                return 0;
            }

            if (*ctxt).input.is_null() {
                break;
            }
            if let Some(input_buffer) = (*(*ctxt).input).buf.as_mut() {
                // If we are operating on converted input, try to flush
                // remaining chars to avoid them stalling in the non-converted buffer.
                if input_buffer.borrow().raw.is_some()
                    && !input_buffer.borrow().raw.unwrap().is_empty()
                {
                    let base: size_t = (*(*ctxt).input).get_base();
                    let current: size_t =
                        (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

                    input_buffer.borrow_mut().push_bytes(b"");
                    (*(*ctxt).input).set_base_and_cursor(base, current);
                }
            }
            avail = (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) as _;
            if avail < 1 {
                // goto done;
                return ret;
            }
            match (*ctxt).instate {
                XmlParserInputState::XmlParserEOF => {
                    // Document parsing is done !
                    // goto done;
                    return ret;
                }
                XmlParserInputState::XmlParserStart => 'to_break: {
                    if (*ctxt).charset == XmlCharEncoding::None {
                        let mut start: [XmlChar; 4] = [0; 4];

                        // Very first chars read from the document flow.
                        if avail < 4 {
                            // goto done;
                            return ret;
                        }

                        // Get the 4 first bytes and decode the charset
                        // if enc != xmlCharEncoding::XML_CHAR_ENCODING_NONE
                        // plug some encoding conversion routines,
                        // else xmlSwitchEncoding will set to (default) UTF8.
                        start[0] = (*ctxt).current_byte();
                        start[1] = (*ctxt).nth_byte(1);
                        start[2] = (*ctxt).nth_byte(2);
                        start[3] = (*ctxt).nth_byte(3);
                        let enc = detect_encoding(&start);
                        // We need more bytes to detect EBCDIC code pages.
                        // See xmlDetectEBCDIC.
                        if matches!(enc, XmlCharEncoding::EBCDIC) && terminate == 0 && avail < 200 {
                            // goto done;
                            return ret;
                        }
                        xml_switch_encoding(ctxt, enc);
                        break 'to_break;
                    }
                    if avail < 2 {
                        // goto done;
                        return ret;
                    }
                    cur = *(*(*ctxt).input).cur.add(0);
                    next = *(*(*ctxt).input).cur.add(1);
                    if cur == 0 {
                        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).set_document_locator.is_some() {
                            ((*(*ctxt).sax).set_document_locator.unwrap())(
                                (*ctxt).user_data.clone(),
                                xml_default_sax_locator(),
                            );
                        }
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEmpty, None);
                        (*ctxt).halt();
                        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).end_document.is_some() {
                            ((*(*ctxt).sax).end_document.unwrap())((*ctxt).user_data.clone());
                        }
                        // goto done;
                        return ret;
                    }
                    if cur == b'<' && next == b'?' {
                        /* PI or XML decl */
                        if avail < 5 {
                            // goto done;
                            return ret;
                        }
                        if terminate == 0
                            && xml_parse_lookup_string(ctxt, 2, c"?>".as_ptr() as _, 2).is_null()
                        {
                            // goto done;
                            return ret;
                        }
                        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).set_document_locator.is_some() {
                            ((*(*ctxt).sax).set_document_locator.unwrap())(
                                (*ctxt).user_data.clone(),
                                xml_default_sax_locator(),
                            );
                        }
                        if *(*(*ctxt).input).cur.add(2) == b'x'
                            && *(*(*ctxt).input).cur.add(3) == b'm'
                            && *(*(*ctxt).input).cur.add(4) == b'l'
                            && xml_is_blank_char(*(*(*ctxt).input).cur.add(5) as u32)
                        {
                            ret += 5;
                            xml_parse_xmldecl(ctxt);
                            if (*ctxt).err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
                                // The XML REC instructs us to stop parsing right here
                                (*ctxt).halt();
                                return 0;
                            }
                            (*ctxt).standalone = (*(*ctxt).input).standalone;
                            if (*ctxt).encoding().is_none() && (*(*ctxt).input).encoding.is_some() {
                                (*ctxt).encoding = (*(*ctxt).input).encoding.clone();
                            }
                            if !(*ctxt).sax.is_null()
                                && (*(*ctxt).sax).start_document.is_some()
                                && (*ctxt).disable_sax == 0
                            {
                                ((*(*ctxt).sax).start_document.unwrap())((*ctxt).user_data.clone());
                            }
                            (*ctxt).instate = XmlParserInputState::XmlParserMisc;
                        } else {
                            (*ctxt).version = Some(XML_DEFAULT_VERSION.to_owned());
                            if !(*ctxt).sax.is_null()
                                && (*(*ctxt).sax).start_document.is_some()
                                && (*ctxt).disable_sax == 0
                            {
                                ((*(*ctxt).sax).start_document.unwrap())((*ctxt).user_data.clone());
                            }
                            (*ctxt).instate = XmlParserInputState::XmlParserMisc;
                        }
                    } else {
                        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).set_document_locator.is_some() {
                            ((*(*ctxt).sax).set_document_locator.unwrap())(
                                (*ctxt).user_data.clone(),
                                xml_default_sax_locator(),
                            );
                        }
                        (*ctxt).version = Some(XML_DEFAULT_VERSION.to_owned());
                        if !(*ctxt).sax.is_null()
                            && (*(*ctxt).sax).start_document.is_some()
                            && (*ctxt).disable_sax == 0
                        {
                            ((*(*ctxt).sax).start_document.unwrap())((*ctxt).user_data.clone());
                        }
                        (*ctxt).instate = XmlParserInputState::XmlParserMisc;
                    }
                }
                XmlParserInputState::XmlParserStartTag => {
                    let name: *const XmlChar;
                    let mut prefix: *const XmlChar = null_mut();
                    let mut uri: *const XmlChar = null_mut();
                    let line: i32 = (*(*ctxt).input).line;
                    let ns_nr = (*ctxt).ns_tab.len();

                    if avail < 2 && (*ctxt).input_tab.len() == 1 {
                        // goto done;
                        return ret;
                    }
                    cur = *(*(*ctxt).input).cur.add(0);
                    if cur != b'<' {
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEmpty, None);
                        (*ctxt).halt();
                        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).end_document.is_some() {
                            ((*(*ctxt).sax).end_document.unwrap())((*ctxt).user_data.clone());
                        }
                        // goto done;
                        return ret;
                    }
                    if terminate == 0 && xml_parse_lookup_gt(ctxt) == 0 {
                        // goto done;
                        return ret;
                    }
                    if (*ctxt).space_tab.is_empty() || (*ctxt).space() == -2 {
                        (*ctxt).space_push(-1);
                    } else {
                        (*ctxt).space_push((*ctxt).space());
                    }
                    #[cfg(feature = "sax1")]
                    {
                        if (*ctxt).sax2 != 0 {
                            name = xml_parse_start_tag2(
                                ctxt,
                                &raw mut prefix,
                                &raw mut uri,
                                &raw mut tlen,
                            );
                        } else {
                            name = xml_parse_start_tag(ctxt);
                        }
                    }
                    #[cfg(not(feature = "sax1"))]
                    {
                        name = xml_parse_start_tag2(
                            ctxt,
                            &raw mut prefix,
                            &raw mut uri,
                            &raw mut tlen,
                        );
                    }
                    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                        // goto done;
                        return ret;
                    }
                    if name.is_null() {
                        (*ctxt).space_pop();
                        (*ctxt).halt();
                        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).end_document.is_some() {
                            ((*(*ctxt).sax).end_document.unwrap())((*ctxt).user_data.clone());
                        }
                        // goto done;
                        return ret;
                    }
                    // [ VC: Root Element Type ]
                    // The Name in the document type declaration must match
                    // the element type of the root element.
                    #[cfg(feature = "libxml_valid")]
                    if (*ctxt).validate != 0
                        && (*ctxt).well_formed != 0
                        && !(*ctxt).my_doc.is_null()
                        && !(*ctxt).node.is_null()
                        && NodePtr::from_ptr((*ctxt).node) == (*(*ctxt).my_doc).children
                    {
                        (*ctxt).valid &=
                            xml_validate_root(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc);
                    }

                    // Check for an Empty Element.
                    if (*ctxt).current_byte() == b'/' && (*ctxt).nth_byte(1) == b'>' {
                        (*ctxt).advance(2);

                        if (*ctxt).sax2 != 0 {
                            if !(*ctxt).sax.is_null()
                                && (*(*ctxt).sax).end_element_ns.is_some()
                                && (*ctxt).disable_sax == 0
                            {
                                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                                ((*(*ctxt).sax).end_element_ns.unwrap())(
                                    (*ctxt).user_data.clone(),
                                    &name,
                                    prefix,
                                    uri,
                                );
                            }
                            if (*ctxt).ns_tab.len() - ns_nr > 0 {
                                (*ctxt).ns_pop((*ctxt).ns_tab.len() - ns_nr);
                            }
                        } else {
                            #[cfg(feature = "sax1")]
                            if !(*ctxt).sax.is_null()
                                && (*(*ctxt).sax).end_element.is_some()
                                && (*ctxt).disable_sax == 0
                            {
                                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                                ((*(*ctxt).sax).end_element.unwrap())(
                                    (*ctxt).user_data.clone(),
                                    &name,
                                );
                            }
                        }
                        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                            // goto done;
                            return ret;
                        }
                        (*ctxt).space_pop();
                        if (*ctxt).name_tab.is_empty() {
                            (*ctxt).instate = XmlParserInputState::XmlParserEpilog;
                        } else {
                            (*ctxt).instate = XmlParserInputState::XmlParserContent;
                        }
                        // break;
                    } else {
                        if (*ctxt).current_byte() == b'>' {
                            (*ctxt).skip_char();
                        } else {
                            let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                            xml_fatal_err_msg_str!(
                                ctxt,
                                XmlParserErrors::XmlErrGtRequired,
                                "Couldn't find end of Start Tag {}\n",
                                name
                            );
                            (*ctxt).node_pop();
                            (*ctxt).space_pop();
                        }
                        (*ctxt).name_ns_push(
                            name,
                            prefix,
                            uri,
                            line,
                            (*ctxt).ns_tab.len() as i32 - ns_nr as i32,
                        );

                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                        // break;
                    }
                }
                XmlParserInputState::XmlParserContent => 'to_break: {
                    if avail < 2 && (*ctxt).input_tab.len() == 1 {
                        // goto done;
                        return ret;
                    }
                    cur = *(*(*ctxt).input).cur.add(0);
                    next = *(*(*ctxt).input).cur.add(1);

                    if cur == b'<' && next == b'/' {
                        (*ctxt).instate = XmlParserInputState::XmlParserEndTag;
                        break 'to_break;
                    } else if cur == b'<' && next == b'?' {
                        if terminate == 0
                            && xml_parse_lookup_string(ctxt, 2, c"?>".as_ptr() as _, 2).is_null()
                        {
                            // goto done;
                            return ret;
                        }
                        xml_parse_pi(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    } else if cur == b'<' && next != b'!' {
                        (*ctxt).instate = XmlParserInputState::XmlParserStartTag;
                        break 'to_break;
                    } else if cur == b'<'
                        && next == b'!'
                        && *(*(*ctxt).input).cur.add(2) == b'-'
                        && *(*(*ctxt).input).cur.add(3) == b'-'
                    {
                        if terminate == 0
                            && xml_parse_lookup_string(ctxt, 4, c"-->".as_ptr() as _, 3).is_null()
                        {
                            // goto done;
                            return ret;
                        }
                        xml_parse_comment(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    } else if cur == b'<'
                        && *(*(*ctxt).input).cur.add(1) == b'!'
                        && *(*(*ctxt).input).cur.add(2) == b'['
                        && *(*(*ctxt).input).cur.add(3) == b'C'
                        && *(*(*ctxt).input).cur.add(4) == b'D'
                        && *(*(*ctxt).input).cur.add(5) == b'A'
                        && *(*(*ctxt).input).cur.add(6) == b'T'
                        && *(*(*ctxt).input).cur.add(7) == b'A'
                        && *(*(*ctxt).input).cur.add(8) == b'['
                    {
                        (*ctxt).advance(9);
                        (*ctxt).instate = XmlParserInputState::XmlParserCDATASection;
                        break 'to_break;
                    } else if cur == b'<' && next == b'!' && avail < 9 {
                        // goto done;
                        return ret;
                    } else if cur == b'<' {
                        xml_fatal_err(
                            ctxt,
                            XmlParserErrors::XmlErrInternalError,
                            Some("detected an error in element content\n"),
                        );
                        (*ctxt).advance(1);
                    } else if cur == b'&' {
                        if terminate == 0 && xml_parse_lookup_char(ctxt, b';' as _) == 0 {
                            // goto done;
                            return ret;
                        }
                        xml_parse_reference(ctxt);
                    } else {
                        // TODO Avoid the extra copy, handle directly !!!

                        // Goal of the following test is:
                        //  - minimize calls to the SAX 'character' callback
                        //    when they are mergeable
                        //  - handle an problem for isBlank when we only parse
                        //    a sequence of blank chars and the next one is
                        //    not available to check against '<' presence.
                        //  - tries to homogenize the differences in SAX
                        //    callbacks between the push and pull versions
                        //    of the parser.
                        if ((*ctxt).input_tab.len() == 1 && avail < XML_PARSER_BIG_BUFFER_SIZE)
                            && (terminate == 0 && xml_parse_lookup_char_data(ctxt) == 0)
                        {
                            // goto done;
                            return ret;
                        }
                        (*ctxt).check_index = 0;
                        xml_parse_char_data_internal(ctxt, !terminate);
                    }
                }
                XmlParserInputState::XmlParserEndTag => {
                    if avail < 2 {
                        // goto done;
                        return ret;
                    }
                    if terminate == 0 && xml_parse_lookup_char(ctxt, b'>' as _) == 0 {
                        // goto done;
                        return ret;
                    }
                    if (*ctxt).sax2 != 0 {
                        xml_parse_end_tag2(ctxt, &(*ctxt).push_tab[(*ctxt).name_tab.len() - 1]);
                        (*ctxt).name_ns_pop();
                    } else {
                        #[cfg(feature = "sax1")]
                        {
                            xml_parse_end_tag1(ctxt, 0);
                        }
                    }
                    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                        // Nothing
                    } else if (*ctxt).name_tab.is_empty() {
                        (*ctxt).instate = XmlParserInputState::XmlParserEpilog;
                    } else {
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    }
                }
                XmlParserInputState::XmlParserCDATASection => {
                    // The Push mode need to have the SAX callback for
                    // cdataBlock merge back contiguous callbacks.

                    let term = if terminate != 0 {
                        // Don't call xmlParseLookupString. If 'terminate'
                        // is set, checkIndex is invalid.
                        strstr((*(*ctxt).input).cur as _, c"]]>".as_ptr() as _) as _
                    } else {
                        xml_parse_lookup_string(ctxt, 0, c"]]>".as_ptr() as _, 3)
                    };

                    if term.is_null() {
                        let mut tmp: i32;
                        let size: i32;

                        if terminate != 0 {
                            // Unfinished CDATA section
                            size = (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) as _;
                        } else {
                            if avail < XML_PARSER_BIG_BUFFER_SIZE + 2 {
                                // goto done;
                                return ret;
                            }
                            (*ctxt).check_index = 0;
                            // XXX: Why don't we pass the full buffer?
                            size = XML_PARSER_BIG_BUFFER_SIZE as i32;
                        }
                        tmp = xml_check_cdata_push((*(*ctxt).input).cur, size, 0);
                        if tmp <= 0 {
                            tmp = -tmp;
                            (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(tmp as usize);
                            break 'encoding_error;
                        }
                        if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                            let s = from_utf8(from_raw_parts((*(*ctxt).input).cur, tmp as usize))
                                .expect("Internal Error");
                            if let Some(cdata_block) = (*(*ctxt).sax).cdata_block {
                                cdata_block((*ctxt).user_data.clone(), s);
                            } else if let Some(characters) = (*(*ctxt).sax).characters {
                                characters((*ctxt).user_data.clone(), s);
                            }
                        }
                        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                            // goto done;
                            return ret;
                        }
                        (*ctxt).advance_with_line_handling(tmp as usize);
                    } else {
                        let base: i32 = term.offset_from((*ctxt).current_ptr()) as i32;
                        let mut tmp: i32;

                        tmp = xml_check_cdata_push((*(*ctxt).input).cur, base, 1);
                        if tmp < 0 || tmp != base {
                            tmp = -tmp;
                            (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(tmp as usize);
                            break 'encoding_error;
                        }
                        if !(*ctxt).sax.is_null()
                            && base == 0
                            && (*(*ctxt).sax).cdata_block.is_some()
                            && (*ctxt).disable_sax == 0
                        {
                            // Special case to provide identical behaviour
                            // between pull and push parsers on enpty CDATA sections
                            if (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) >= 9
                                && strncmp(
                                    (*(*ctxt).input).cur.sub(9) as _,
                                    c"<![CDATA[".as_ptr() as _,
                                    9,
                                ) == 0
                            {
                                ((*(*ctxt).sax).cdata_block.unwrap())(
                                    (*ctxt).user_data.clone(),
                                    "",
                                );
                            }
                        } else if !(*ctxt).sax.is_null() && base > 0 && (*ctxt).disable_sax == 0 {
                            let s = from_utf8(from_raw_parts((*(*ctxt).input).cur, base as usize))
                                .expect("Internal Error");
                            if let Some(cdata_block) = (*(*ctxt).sax).cdata_block {
                                cdata_block((*ctxt).user_data.clone(), s);
                            } else if let Some(characters) = (*(*ctxt).sax).characters {
                                characters((*ctxt).user_data.clone(), s);
                            }
                        }
                        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                            // goto done;
                            return ret;
                        }
                        (*ctxt).advance_with_line_handling(base as usize + 3);
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    }
                }
                XmlParserInputState::XmlParserMisc
                | XmlParserInputState::XmlParserProlog
                | XmlParserInputState::XmlParserEpilog => {
                    (*ctxt).skip_blanks();
                    avail = (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) as _;
                    if avail < 2 {
                        // goto done;
                        return ret;
                    }
                    cur = *(*(*ctxt).input).cur.add(0);
                    next = *(*(*ctxt).input).cur.add(1);
                    if cur == b'<' && next == b'?' {
                        if terminate == 0
                            && xml_parse_lookup_string(ctxt, 2, c"?>".as_ptr() as _, 2).is_null()
                        {
                            // goto done;
                            return ret;
                        }
                        xml_parse_pi(ctxt);
                        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                            // goto done;
                            return ret;
                        }
                    } else if cur == b'<'
                        && next == b'!'
                        && *(*(*ctxt).input).cur.add(2) == b'-'
                        && *(*(*ctxt).input).cur.add(3) == b'-'
                    {
                        if terminate == 0
                            && xml_parse_lookup_string(ctxt, 4, c"-->".as_ptr() as _, 3).is_null()
                        {
                            // goto done;
                            return ret;
                        }
                        xml_parse_comment(ctxt);
                        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                            // goto done;
                            return ret;
                        }
                    } else if matches!((*ctxt).instate, XmlParserInputState::XmlParserMisc)
                        && cur == b'<'
                        && next == b'!'
                        && *(*(*ctxt).input).cur.add(2) == b'D'
                        && *(*(*ctxt).input).cur.add(3) == b'O'
                        && *(*(*ctxt).input).cur.add(4) == b'C'
                        && *(*(*ctxt).input).cur.add(5) == b'T'
                        && *(*(*ctxt).input).cur.add(6) == b'Y'
                        && *(*(*ctxt).input).cur.add(7) == b'P'
                        && *(*(*ctxt).input).cur.add(8) == b'E'
                    {
                        if terminate == 0 && xml_parse_lookup_gt(ctxt) == 0 {
                            // goto done;
                            return ret;
                        }
                        (*ctxt).in_subset = 1;
                        xml_parse_doc_type_decl(ctxt);
                        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                            // goto done;
                            return ret;
                        }
                        if (*ctxt).current_byte() == b'[' {
                            (*ctxt).instate = XmlParserInputState::XmlParserDTD;
                        } else {
                            // Create and update the external subset.
                            (*ctxt).in_subset = 2;
                            if !(*ctxt).sax.is_null()
                                && (*ctxt).disable_sax == 0
                                && (*(*ctxt).sax).external_subset.is_some()
                            {
                                ((*(*ctxt).sax).external_subset.unwrap())(
                                    (*ctxt).user_data.clone(),
                                    (*ctxt).int_sub_name.as_deref(),
                                    (*ctxt).ext_sub_system.as_deref(),
                                    (*ctxt).ext_sub_uri.as_deref(),
                                );
                            }
                            (*ctxt).in_subset = 0;
                            xml_clean_special_attr(ctxt);
                            (*ctxt).instate = XmlParserInputState::XmlParserProlog;
                        }
                    } else if cur == b'<'
                        && next == b'!'
                        && avail
                            < if matches!((*ctxt).instate, XmlParserInputState::XmlParserMisc) {
                                9
                            } else {
                                4
                            }
                    {
                        // goto done;
                        return ret;
                    } else if matches!((*ctxt).instate, XmlParserInputState::XmlParserEpilog) {
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEnd, None);
                        (*ctxt).halt();
                        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).end_document.is_some() {
                            ((*(*ctxt).sax).end_document.unwrap())((*ctxt).user_data.clone());
                        }
                        // goto done;
                        return ret;
                    } else {
                        (*ctxt).instate = XmlParserInputState::XmlParserStartTag;
                    }
                }
                XmlParserInputState::XmlParserDTD => {
                    if terminate == 0 && xml_parse_lookup_internal_subset(ctxt) == 0 {
                        // goto done;
                        return ret;
                    }
                    xml_parse_internal_subset(ctxt);
                    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                        // goto done;
                        return ret;
                    }
                    (*ctxt).in_subset = 2;
                    if !(*ctxt).sax.is_null()
                        && (*ctxt).disable_sax == 0
                        && (*(*ctxt).sax).external_subset.is_some()
                    {
                        ((*(*ctxt).sax).external_subset.unwrap())(
                            (*ctxt).user_data.clone(),
                            (*ctxt).int_sub_name.as_deref(),
                            (*ctxt).ext_sub_system.as_deref(),
                            (*ctxt).ext_sub_uri.as_deref(),
                        );
                    }
                    (*ctxt).in_subset = 0;
                    xml_clean_special_attr(ctxt);
                    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                        // goto done;
                        return ret;
                    }
                    (*ctxt).instate = XmlParserInputState::XmlParserProlog;
                    // break;
                }
                XmlParserInputState::XmlParserComment => {
                    generic_error!("PP: internal error, state == COMMENT\n",);
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                }
                XmlParserInputState::XmlParserIgnore => {
                    generic_error!("PP: internal error, state == IGNORE",);
                    (*ctxt).instate = XmlParserInputState::XmlParserDTD;
                }
                XmlParserInputState::XmlParserPI => {
                    generic_error!("PP: internal error, state == PI\n",);
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                }
                XmlParserInputState::XmlParserEntityDecl => {
                    generic_error!("PP: internal error, state == ENTITY_DECL\n",);
                    (*ctxt).instate = XmlParserInputState::XmlParserDTD;
                }
                XmlParserInputState::XmlParserEntityValue => {
                    generic_error!("PP: internal error, state == ENTITY_VALUE\n",);
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                }
                XmlParserInputState::XmlParserAttributeValue => {
                    generic_error!("PP: internal error, state == ATTRIBUTE_VALUE\n",);
                    (*ctxt).instate = XmlParserInputState::XmlParserStartTag;
                }
                XmlParserInputState::XmlParserSystemLiteral => {
                    generic_error!("PP: internal error, state == SYSTEM_LITERAL\n",);
                    (*ctxt).instate = XmlParserInputState::XmlParserStartTag;
                }
                XmlParserInputState::XmlParserPublicLiteral => {
                    generic_error!("PP: internal error, state == PUBLIC_LITERAL\n",);
                    (*ctxt).instate = XmlParserInputState::XmlParserStartTag;
                }
            }
        }
        // done:
        return ret;
    }
    // encoding_error:
    if (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) < 4 {
        __xml_err_encoding!(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            "Input is not proper UTF-8, indicate encoding !\n"
        );
    } else {
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
    0
}

/// Parse a Chunk of memory
///
/// Returns zero if no error, the xmlParserErrors otherwise.
#[doc(alias = "xmlParseChunk")]
#[cfg(feature = "libxml_push")]
pub unsafe extern "C" fn xml_parse_chunk(
    ctxt: XmlParserCtxtPtr,
    chunk: *const c_char,
    mut size: i32,
    terminate: i32,
) -> i32 {
    use super::parser_internals::XML_MAX_LOOKUP_LIMIT;

    let mut end_in_lf: i32 = 0;

    if ctxt.is_null() {
        return XmlParserErrors::XmlErrInternalError as i32;
    }
    if (*ctxt).err_no != XmlParserErrors::XmlErrOK as i32 && (*ctxt).disable_sax == 1 {
        return (*ctxt).err_no;
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }
    if (*ctxt).input.is_null() {
        return -1;
    }

    (*ctxt).progressive = 1;
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserStart) {
        (*ctxt).detect_sax2();
    }
    if size > 0
        && !chunk.is_null()
        && terminate == 0
        && *chunk.add(size as usize - 1) == b'\r' as i8
    {
        end_in_lf = 1;
        size -= 1;
    }

    if size > 0
        && !chunk.is_null()
        && !(*ctxt).input.is_null()
        && (*(*ctxt).input).buf.is_some()
        && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        let base: size_t = (*(*ctxt).input).get_base();
        let cur: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

        let res: i32 = (*(*ctxt).input)
            .buf
            .as_mut()
            .unwrap()
            .borrow_mut()
            .push_bytes(from_raw_parts(chunk as *const u8, size as usize));
        (*(*ctxt).input).set_base_and_cursor(base, cur);
        if res < 0 {
            (*ctxt).err_no = XmlParserInputState::XmlParserEOF as i32;
            (*ctxt).halt();
            return XmlParserInputState::XmlParserEOF as i32;
        }
    } else if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        && (!(*ctxt).input.is_null() && (*(*ctxt).input).buf.is_some())
    {
        let input = (*(*ctxt).input).buf.as_mut().unwrap();
        if input.borrow().encoder.is_some()
            && input.borrow().buffer.is_some()
            && input.borrow().raw.is_some()
        {
            let base: size_t = (*(*ctxt).input).get_base();
            let current: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

            let res = input.borrow_mut().decode(terminate != 0);
            (*(*ctxt).input).set_base_and_cursor(base, current);
            if res.is_err() {
                /* TODO 2.6.0 */
                generic_error!("xmlParseChunk: encoder error\n");
                (*ctxt).halt();
                return XmlParserErrors::XmlErrInvalidEncoding as i32;
            }
        }
    }

    xml_parse_try_or_finish(ctxt, terminate);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return (*ctxt).err_no;
    }

    if !(*ctxt).input.is_null()
        && ((*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) > XML_MAX_LOOKUP_LIMIT as isize
            || (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base)
                > XML_MAX_LOOKUP_LIMIT as isize)
        && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0
    {
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            Some("Huge input lookup"),
        );
        (*ctxt).halt();
    }
    if (*ctxt).err_no != XmlParserErrors::XmlErrOK as i32 && (*ctxt).disable_sax == 1 {
        return (*ctxt).err_no;
    }

    if end_in_lf == 1 && !(*ctxt).input.is_null() && (*(*ctxt).input).buf.is_some() {
        let base: size_t = (*(*ctxt).input).get_base();
        let current: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

        (*(*ctxt).input)
            .buf
            .as_mut()
            .unwrap()
            .borrow_mut()
            .push_bytes(b"\r");
        (*(*ctxt).input).set_base_and_cursor(base, current);
    }
    if terminate != 0 {
        /*
         * Check for termination
         */
        if !matches!(
            (*ctxt).instate,
            XmlParserInputState::XmlParserEOF | XmlParserInputState::XmlParserEpilog
        ) {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEnd, None);
        }
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEpilog)
            && (*(*ctxt).input).cur < (*(*ctxt).input).end
        {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEnd, None);
        }
        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
            && (!(*ctxt).sax.is_null() && (*(*ctxt).sax).end_document.is_some())
        {
            ((*(*ctxt).sax).end_document.unwrap())((*ctxt).user_data.clone());
        }
        (*ctxt).instate = XmlParserInputState::XmlParserEOF;
    }
    if (*ctxt).well_formed == 0 {
        (*ctxt).err_no
    } else {
        0
    }
}

/// Create a parser context for using the XML parser with an existing I/O stream
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateIOParserCtxt")]
pub unsafe fn xml_create_io_parser_ctxt(
    sax: XmlSAXHandlerPtr,
    user_data: Option<GenericErrorContext>,
    ioctx: impl Read + 'static,
    enc: XmlCharEncoding,
) -> XmlParserCtxtPtr {
    let buf = XmlParserInputBuffer::from_reader(ioctx, enc);
    let ctxt: XmlParserCtxtPtr = xml_new_sax_parser_ctxt(sax, user_data);
    if ctxt.is_null() {
        return null_mut();
    }

    let input_stream: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, Rc::new(RefCell::new(buf)), enc);
    if input_stream.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }
    (*ctxt).input_push(input_stream);

    ctxt
}

/// Create a new input stream structure encapsulating the @input into
/// a stream suitable for the parser.
///
/// Returns the new input stream or NULL
#[doc(alias = "xmlNewIOInputStream")]
pub unsafe fn xml_new_io_input_stream(
    ctxt: XmlParserCtxtPtr,
    input: Rc<RefCell<XmlParserInputBuffer>>,
    enc: XmlCharEncoding,
) -> XmlParserInputPtr {
    if get_parser_debug_entities() != 0 {
        generic_error!("new input from I/O\n");
    }
    let input_stream: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input_stream.is_null() {
        return null_mut();
    }
    (*input_stream).filename = None;
    (*input_stream).buf = Some(input);
    (*input_stream).reset_base();

    if !matches!(enc, XmlCharEncoding::None) {
        xml_switch_encoding(ctxt, enc);
    }

    input_stream
}

/// Find the parser node info struct for a given node
///
/// Returns an xmlParserNodeInfo block pointer or NULL
#[doc(alias = "xmlParserFindNodeInfo")]
pub(crate) unsafe extern "C" fn xml_parser_find_node_info(
    ctxt: XmlParserCtxtPtr,
    node: XmlNodePtr,
) -> *const XmlParserNodeInfo {
    if ctxt.is_null() || node.is_null() {
        return null_mut();
    }
    /* Find position where node should be at */
    let pos: u64 = xml_parser_find_node_info_index(addr_of_mut!((*ctxt).node_seq), node);
    if pos < (*ctxt).node_seq.length && (*(*ctxt).node_seq.buffer.add(pos as usize)).node == node {
        (*ctxt).node_seq.buffer.add(pos as usize)
    } else {
        null_mut()
    }
}

/// -- Initialize (set to initial state) node info sequence
#[doc(alias = "xmlInitNodeInfoSeq")]
pub(crate) unsafe extern "C" fn xml_init_node_info_seq(seq: XmlParserNodeInfoSeqPtr) {
    if seq.is_null() {
        return;
    }
    (*seq).length = 0;
    (*seq).maximum = 0;
    (*seq).buffer = null_mut();
}

/// -- Clear (release memory and reinitialize) node
///   info sequence
#[doc(alias = "xmlClearNodeInfoSeq")]
pub(crate) unsafe extern "C" fn xml_clear_node_info_seq(seq: XmlParserNodeInfoSeqPtr) {
    if seq.is_null() {
        return;
    }
    if !(*seq).buffer.is_null() {
        xml_free((*seq).buffer as _);
    }
    xml_init_node_info_seq(seq);
}

/// Find the index that the info record for the given node is or should be at in a sorted sequence
///
/// Returns a long indicating the position of the record
#[doc(alias = "xmlParserFindNodeInfoIndex")]
pub(crate) unsafe extern "C" fn xml_parser_find_node_info_index(
    seq: XmlParserNodeInfoSeqPtr,
    node: XmlNodePtr,
) -> u64 {
    let mut upper: u64;
    let mut lower: u64;
    let mut middle: u64;
    let mut found: i32 = 0;

    if seq.is_null() || node.is_null() {
        return u64::MAX;
    }

    /* Do a binary search for the key */
    lower = 1;
    upper = (*seq).length;
    middle = 0;
    while lower <= upper && found == 0 {
        middle = lower + (upper - lower) / 2;
        match node.cmp(&((*(*seq).buffer.add(middle as usize - 1)).node as _)) {
            std::cmp::Ordering::Equal => {
                found = 1;
            }
            std::cmp::Ordering::Less => {
                upper = middle - 1;
            }
            std::cmp::Ordering::Greater => {
                lower = middle + 1;
            }
        }
    }

    /* Return position */
    if middle == 0 || (*(*seq).buffer.add(middle as usize - 1)).node < node {
        middle
    } else {
        middle - 1
    }
}

/// Insert node info record into the sorted sequence
#[doc(alias = "xmlParserAddNodeInfo")]
pub(crate) unsafe extern "C" fn xml_parser_add_node_info(
    ctxt: XmlParserCtxtPtr,
    info: XmlParserNodeInfoPtr,
) {
    if ctxt.is_null() || info.is_null() {
        return;
    }

    /* Find pos and check to see if node is already in the sequence */
    let pos: u64 =
        xml_parser_find_node_info_index(addr_of_mut!((*ctxt).node_seq), (*info).node as XmlNodePtr);

    if pos < (*ctxt).node_seq.length
        && !(*ctxt).node_seq.buffer.is_null()
        && (*(*ctxt).node_seq.buffer.add(pos as usize)).node == (*info).node
    {
        *(*ctxt).node_seq.buffer.add(pos as usize) = *info;
    }
    /* Otherwise, we need to add new node to buffer */
    else {
        if (*ctxt).node_seq.length + 1 > (*ctxt).node_seq.maximum
            || (*ctxt).node_seq.buffer.is_null()
        {
            if (*ctxt).node_seq.maximum == 0 {
                (*ctxt).node_seq.maximum = 2;
            }
            let byte_size: u32 = (size_of_val(&*(*ctxt).node_seq.buffer)
                * (2 * (*ctxt).node_seq.maximum as usize)) as _;

            let tmp_buffer: XmlParserNodeInfoPtr = if (*ctxt).node_seq.buffer.is_null() {
                xml_malloc(byte_size as usize) as _
            } else {
                xml_realloc((*ctxt).node_seq.buffer as _, byte_size as usize) as _
            };

            if tmp_buffer.is_null() {
                xml_err_memory(ctxt, Some("failed to allocate buffer\n"));
                return;
            }
            (*ctxt).node_seq.buffer = tmp_buffer;
            (*ctxt).node_seq.maximum *= 2;
        }

        /* If position is not at end, move elements out of the way */
        if pos != (*ctxt).node_seq.length {
            for i in (pos..(*ctxt).node_seq.length).rev() {
                *(*ctxt).node_seq.buffer.add(i as usize + 1) =
                    *(*ctxt).node_seq.buffer.add(i as usize);
            }
        }

        /* Copy element and increase length */
        *(*ctxt).node_seq.buffer.add(pos as usize) = *info;
        (*ctxt).node_seq.length += 1;
    }
}

static mut XML_CURRENT_EXTERNAL_ENTITY_LOADER: XmlExternalEntityLoader =
    xml_default_external_entity_loader;

/// Changes the defaultexternal entity resolver function for the application
#[doc(alias = "xmlSetExternalEntityLoader")]
pub unsafe fn xml_set_external_entity_loader(f: XmlExternalEntityLoader) {
    XML_CURRENT_EXTERNAL_ENTITY_LOADER = f;
}

/// Returns the xmlExternalEntityLoader function pointer
#[doc(alias = "xmlGetExternalEntityLoader")]
pub unsafe fn xml_get_external_entity_loader() -> XmlExternalEntityLoader {
    XML_CURRENT_EXTERNAL_ENTITY_LOADER
}

/// Load an external entity, note that the use of this function for
/// unparsed entities may generate problems
///
/// Returns the xmlParserInputPtr or NULL
#[doc(alias = "xmlLoadExternalEntity")]
pub unsafe fn xml_load_external_entity(
    url: Option<&str>,
    id: Option<&str>,
    ctxt: XmlParserCtxtPtr,
) -> XmlParserInputPtr {
    if let Some(url) = url.filter(|_| xml_no_net_exists(url) == 0) {
        let canonic_filename = canonic_path(url);
        let ret: XmlParserInputPtr =
            XML_CURRENT_EXTERNAL_ENTITY_LOADER(Some(&canonic_filename), id, ctxt);
        return ret;
    }
    XML_CURRENT_EXTERNAL_ENTITY_LOADER(url, id, ctxt)
}

/// This function provides the current index of the parser relative
/// to the start of the current entity. This function is computed in
/// bytes from the beginning starting at zero and finishing at the
/// size in byte of the file if parsing a file. The function is
/// of constant cost if the input is UTF-8 but can be costly if run
/// on non-UTF-8 input.
///
/// Returns the index in bytes from the beginning of the entity or -1
/// in case the index could not be computed.
#[doc(alias = "xmlByteConsumed")]
pub unsafe extern "C" fn xml_byte_consumed(ctxt: XmlParserCtxtPtr) -> i64 {
    if ctxt.is_null() {
        return -1;
    }
    let input: XmlParserInputPtr = (*ctxt).input;
    if input.is_null() {
        return -1;
    }
    if (*input).buf.is_some() && (*input).buf.as_ref().unwrap().borrow().encoder.is_some() {
        let mut unused: u32 = 0;
        let mut buf = (*input).buf.as_ref().unwrap().borrow_mut();
        let handler = buf.encoder.as_mut().unwrap();
        /*
         * Encoding conversion, compute the number of unused original
         * bytes from the input not consumed and subtract that from
         * the raw consumed value, this is not a cheap operation
         */
        if (*input).end.offset_from((*input).cur) > 0 {
            // The original code seems to continue processing as long as the write succeeds,
            // even if encoding errors occur.
            // However, the new API stops processing when an error occurs,
            // so it is not possible to reproduce such a process ...
            let mut out = [0u8; 32000];
            let Ok(input) = from_utf8(from_raw_parts(
                (*input).cur,
                (*input).end.offset_from((*input).cur) as usize,
            )) else {
                return -1;
            };
            let mut read = 0;
            while read < input.len() {
                let Ok((r, w)) = handler.encode(&input[read..], &mut out) else {
                    return -1;
                };
                unused += w as u32;
                read += r;
            }
        }
        if (*input).buf.as_ref().unwrap().borrow().rawconsumed < unused as u64 {
            return -1;
        }
        return ((*input).buf.as_ref().unwrap().borrow().rawconsumed - unused as u64) as i64;
    }
    (*input).consumed as i64 + (*input).cur.offset_from((*input).base) as i64
}

/// This is the set of XML parser options that can be passed down
/// to the xmlReadDoc() and similar calls.
#[doc(alias = "xmlParserOption")]
#[repr(C)]
pub enum XmlParserOption {
    XmlParseRecover = 1 << 0,     /* recover on errors */
    XmlParseNoent = 1 << 1,       /* substitute entities */
    XmlParseDtdload = 1 << 2,     /* load the external subset */
    XmlParseDtdattr = 1 << 3,     /* default DTD attributes */
    XmlParseDtdvalid = 1 << 4,    /* validate with the DTD */
    XmlParseNoerror = 1 << 5,     /* suppress error reports */
    XmlParseNowarning = 1 << 6,   /* suppress warning reports */
    XmlParsePedantic = 1 << 7,    /* pedantic error reporting */
    XmlParseNoblanks = 1 << 8,    /* remove blank nodes */
    XmlParseSax1 = 1 << 9,        /* use the SAX1 interface internally */
    XmlParseXinclude = 1 << 10,   /* Implement XInclude substitution  */
    XmlParseNonet = 1 << 11,      /* Forbid network access */
    XmlParseNodict = 1 << 12,     /* Do not reuse the context dictionary */
    XmlParseNsclean = 1 << 13,    /* remove redundant namespaces declarations */
    XmlParseNocdata = 1 << 14,    /* merge CDATA as text nodes */
    XmlParseNoxincnode = 1 << 15, /* do not generate XINCLUDE START/END nodes */
    XmlParseCompact = 1 << 16,    /* compact small text nodes; no modification of
                                                  the tree allowed afterwards (will possibly
                                  crash if you try to modify the tree) */
    XmlParseOld10 = 1 << 17,     /* parse using XML-1.0 before update 5 */
    XmlParseNobasefix = 1 << 18, /* do not fixup XINCLUDE xml:base uris */
    XmlParseHuge = 1 << 19,      /* relax any hardcoded limit from the parser */
    XmlParseOldsax = 1 << 20,    /* parse using SAX2 interface before 2.7.0 */
    XmlParseIgnoreEnc = 1 << 21, /* ignore internal document encoding hint */
    XmlParseBigLines = 1 << 22,  /* Store big lines numbers in text PSVI field */
}

/// Free a string if it is not owned by the "dict" dictionary in the current scope
macro_rules! DICT_FREE {
    ($dict:expr, $str:expr) => {
        if !$str.is_null()
            && ($dict.is_null() || crate::libxml::dict::xml_dict_owns($dict, $str as _) == 0)
        {
            xml_free($str as _);
        }
    };
}

/// Reset a parser context
#[doc(alias = "xmlCtxtReset")]
pub unsafe extern "C" fn xml_ctxt_reset(ctxt: XmlParserCtxtPtr) {
    let mut input: XmlParserInputPtr;

    if ctxt.is_null() {
        return;
    }

    while {
        input = (*ctxt).input_pop();
        !input.is_null()
    } {
        /* Non consuming */
        xml_free_input_stream(input);
    }
    (*ctxt).input_tab.clear();
    (*ctxt).input = null_mut();

    (*ctxt).space_tab.clear();

    (*ctxt).node_tab.clear();
    (*ctxt).node = null_mut();

    (*ctxt).name_tab.clear();
    (*ctxt).name = null_mut();

    (*ctxt).ns_tab.clear();

    (*ctxt).version = None;
    (*ctxt).encoding = None;
    (*ctxt).directory = None;
    (*ctxt).ext_sub_uri = None;
    (*ctxt).ext_sub_system = None;
    if !(*ctxt).my_doc.is_null() {
        xml_free_doc((*ctxt).my_doc);
    }
    (*ctxt).my_doc = null_mut();

    (*ctxt).standalone = -1;
    (*ctxt).has_external_subset = 0;
    (*ctxt).has_perefs = 0;
    (*ctxt).html = 0;
    (*ctxt).external = 0;
    (*ctxt).instate = XmlParserInputState::XmlParserStart;
    (*ctxt).token = 0;
    (*ctxt).well_formed = 1;
    (*ctxt).ns_well_formed = 1;
    (*ctxt).disable_sax = 0;
    (*ctxt).valid = 1;
    (*ctxt).record_info = 0;
    (*ctxt).check_index = 0;
    (*ctxt).end_check_state = 0;
    (*ctxt).in_subset = 0;
    (*ctxt).err_no = XmlParserErrors::XmlErrOK as i32;
    (*ctxt).depth = 0;
    (*ctxt).charset = XmlCharEncoding::UTF8;
    (*ctxt).catalogs = None;
    (*ctxt).sizeentities = 0;
    (*ctxt).sizeentcopy = 0;
    xml_init_node_info_seq(addr_of_mut!((*ctxt).node_seq));

    if let Some(mut table) = (*ctxt).atts_default.take().map(|t| t.into_inner()) {
        table.clear_with(|data, _| xml_free(data as _));
    }
    let _ = (*ctxt).atts_special.take().map(|t| t.into_inner());

    #[cfg(feature = "catalog")]
    {
        (*ctxt).catalogs = None;
    }
    (*ctxt).nb_errors = 0;
    (*ctxt).nb_warnings = 0;
    if (*ctxt).last_error.is_err() {
        (*ctxt).last_error.reset();
    }
}

/// Reset a push parser context
///
/// Returns 0 in case of success and 1 in case of error
#[doc(alias = "xmlCtxtResetPush")]
pub unsafe extern "C" fn xml_ctxt_reset_push(
    ctxt: XmlParserCtxtPtr,
    chunk: *const c_char,
    size: i32,
    filename: *const c_char,
    encoding: *const c_char,
) -> i32 {
    if ctxt.is_null() {
        return 1;
    }

    let enc = if encoding.is_null() && !chunk.is_null() && size >= 4 {
        let input = from_raw_parts(chunk as *const u8, size as usize);
        detect_encoding(input)
    } else {
        XmlCharEncoding::None
    };

    let buf = XmlParserInputBuffer::new(enc);
    // if buf.is_null() {
    //     return 1;
    // }

    if ctxt.is_null() {
        return 1;
    }

    xml_ctxt_reset(ctxt);

    if filename.is_null() {
        (*ctxt).directory = None;
    } else if let Some(dir) =
        xml_parser_get_directory(CStr::from_ptr(filename).to_string_lossy().as_ref())
    {
        (*ctxt).directory = Some(dir.to_string_lossy().into_owned());
    }

    let input_stream: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input_stream.is_null() {
        return 1;
    }

    if filename.is_null() {
        (*input_stream).filename = None;
    } else {
        let canonic = xml_canonic_path(filename as _);
        if !canonic.is_null() {
            (*input_stream).filename = Some(
                CStr::from_ptr(canonic as *const i8)
                    .to_string_lossy()
                    .into_owned(),
            );
            xml_free(canonic as _);
        }
    }
    (*input_stream).buf = Some(Rc::new(RefCell::new(buf)));
    (*input_stream).reset_base();

    (*ctxt).input_push(input_stream);

    if size > 0 && !chunk.is_null() && !(*ctxt).input.is_null() && (*(*ctxt).input).buf.is_some() {
        let base: size_t = (*(*ctxt).input).get_base();
        let cur: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

        (*(*ctxt).input)
            .buf
            .as_mut()
            .unwrap()
            .borrow_mut()
            .push_bytes(from_raw_parts(chunk as *const u8, size as usize));
        (*(*ctxt).input).set_base_and_cursor(base, cur);
    }

    if !encoding.is_null() {
        let enc = CStr::from_ptr(encoding).to_string_lossy().into_owned();
        (*ctxt).encoding = Some(enc);

        if let Some(handler) = find_encoding_handler((*ctxt).encoding().unwrap()) {
            (*ctxt).switch_to_encoding(handler);
        } else {
            let encoding = CStr::from_ptr(encoding).to_string_lossy();
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrUnsupportedEncoding,
                "Unsupported encoding {}\n",
                encoding
            );
        };
    } else if !matches!(enc, XmlCharEncoding::None) {
        xml_switch_encoding(ctxt, enc);
    }

    0
}

/// Applies the options to the parser context
///
/// Returns 0 in case of success, the set of unknown or unimplemented options
/// in case of error.
#[doc(alias = "xmlCtxtUseOptions")]
pub unsafe extern "C" fn xml_ctxt_use_options(ctxt: XmlParserCtxtPtr, options: i32) -> i32 {
    (*ctxt).ctxt_use_options_internal(options, None)
}

/// Parse an XML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "xmlReadDoc")]
pub unsafe fn xml_read_doc(
    cur: *const XmlChar,
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
    cur: *const XmlChar,
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

/// Used to examine the existence of features that can be enabled
/// or disabled at compile-time.
/// They used to be called XML_FEATURE_xxx but this clashed with Expat
#[doc(alias = "xmlFeature")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlFeature {
    XmlWithThread = 1,
    XmlWithTree = 2,
    XmlWithOutput = 3,
    XmlWithPush = 4,
    XmlWithReader = 5,
    XmlWithPattern = 6,
    XmlWithWriter = 7,
    XmlWithSax1 = 8,
    XmlWithFtp = 9,
    XmlWithHttp = 10,
    XmlWithValid = 11,
    XmlWithHtml = 12,
    XmlWithLegacy = 13,
    XmlWithC14n = 14,
    XmlWithCatalog = 15,
    XmlWithXpath = 16,
    XmlWithXptr = 17,
    XmlWithXinclude = 18,
    XmlWithIconv = 19,
    XmlWithIso8859x = 20,
    XmlWithUnicode = 21,
    XmlWithRegexp = 22,
    XmlWithAutomata = 23,
    XmlWithExpr = 24,
    XmlWithSchemas = 25,
    XmlWithSchematron = 26,
    XmlWithModules = 27,
    XmlWithDebug = 28,
    XmlWithDebugMem = 29,
    XmlWithDebugRun = 30,
    XmlWithZlib = 31,
    XmlWithIcu = 32,
    XmlWithLzma = 33,
    XmlWithNone = 99999, /* just to be sure of allocation size */
}

/// Examines if the library has been compiled with a given feature.
///
/// Returns a non-zero value if the feature exist, otherwise zero.
/// Returns zero (0) if the feature does not exist or an unknown
/// unknown feature is requested, non-zero otherwise.
#[doc(alias = "xmlHasFeature")]
pub fn xml_has_feature(feature: Option<XmlFeature>) -> bool {
    match feature {
        Some(XmlFeature::XmlWithThread) => true,
        Some(XmlFeature::XmlWithTree) => {
            cfg!(feature = "libxml_tree")
        }
        Some(XmlFeature::XmlWithOutput) => {
            cfg!(feature = "libxml_output")
        }
        Some(XmlFeature::XmlWithPush) => {
            cfg!(feature = "libxml_push")
        }
        Some(XmlFeature::XmlWithReader) => {
            cfg!(feature = "libxml_reader")
        }
        Some(XmlFeature::XmlWithPattern) => {
            cfg!(feature = "libxml_pattern")
        }
        Some(XmlFeature::XmlWithWriter) => {
            cfg!(feature = "libxml_writer")
        }
        Some(XmlFeature::XmlWithSax1) => {
            cfg!(feature = "sax1")
        }
        Some(XmlFeature::XmlWithFtp) => {
            cfg!(feature = "ftp")
        }
        Some(XmlFeature::XmlWithHttp) => {
            cfg!(feature = "http")
        }
        Some(XmlFeature::XmlWithValid) => {
            cfg!(feature = "libxml_valid")
        }
        Some(XmlFeature::XmlWithHtml) => {
            cfg!(feature = "html")
        }
        Some(XmlFeature::XmlWithLegacy) => {
            cfg!(feature = "libxml_legacy")
        }
        Some(XmlFeature::XmlWithC14n) => {
            cfg!(feature = "c14n")
        }
        Some(XmlFeature::XmlWithCatalog) => {
            cfg!(feature = "catalog")
        }
        Some(XmlFeature::XmlWithXpath) => {
            cfg!(feature = "xpath")
        }
        Some(XmlFeature::XmlWithXptr) => {
            cfg!(feature = "xpointer")
        }
        Some(XmlFeature::XmlWithXinclude) => {
            cfg!(feature = "xinclude")
        }
        //   Some(xmlFeature::XML_WITH_ICONV) => {}
        //   Some(xmlFeature::XML_WITH_ISO8859X) => {}
        Some(XmlFeature::XmlWithUnicode) => {
            cfg!(feature = "libxml_unicode")
        }
        Some(XmlFeature::XmlWithRegexp) => {
            cfg!(feature = "libxml_regexp")
        }
        Some(XmlFeature::XmlWithAutomata) => {
            cfg!(feature = "libxml_automata")
        }
        Some(XmlFeature::XmlWithExpr) => {
            cfg!(feature = "libxml_expr")
        }
        Some(XmlFeature::XmlWithSchemas) => {
            cfg!(feature = "schema")
        }
        Some(XmlFeature::XmlWithSchematron) => {
            cfg!(feature = "schematron")
        }
        //   xmlFeature::XML_WITH_MODULES => {}
        //   xmlFeature::XML_WITH_DEBUG => {}
        //   xmlFeature::XML_WITH_DEBUG_MEM => {}
        //   xmlFeature::XML_WITH_DEBUG_RUN => {}
        //   xmlFeature::XML_WITH_ZLIB => {}
        //   xmlFeature::XML_WITH_LZMA => {}
        //   xmlFeature::XML_WITH_ICU => {}
        _ => false,
    }
}

/// Parse an External ID or a Public ID
///
/// # Note
/// Productions [75] and [83] interact badly since [75] can generate
/// 'PUBLIC' S PubidLiteral S SystemLiteral
///
/// `[75] ExternalID ::= 'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral`
///
/// `[83] PublicID ::= 'PUBLIC' S PubidLiteral`
///
/// Returns the function returns SystemLiteral and in the second
/// case publicID receives PubidLiteral, is strict is off
/// it is possible to return NULL and have publicID set.
#[doc(alias = "xmlParseExternalID")]
pub(crate) unsafe fn xml_parse_external_id(
    ctxt: XmlParserCtxtPtr,
    public_id: *mut *mut XmlChar,
    strict: i32,
) -> *mut XmlChar {
    let mut uri: *mut XmlChar = null_mut();

    *public_id = null_mut();
    if (*ctxt).content_bytes().starts_with(b"SYSTEM") {
        (*ctxt).advance(6);
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after b'SYSTEM'\n",
            );
        }
        uri = xml_parse_system_literal(ctxt);
        if uri.is_null() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrURIRequired, None);
        }
    } else if (*ctxt).content_bytes().starts_with(b"PUBLIC") {
        (*ctxt).advance(6);
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after 'PUBLIC'\n",
            );
        }
        *public_id = xml_parse_pubid_literal(ctxt);
        if (*public_id).is_null() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrPubidRequired, None);
        }
        if strict != 0 {
            // We don't handle [83] so "S SystemLiteral" is required.
            if (*ctxt).skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after the Public Identifier\n",
                );
            }
        } else {
            // We handle [83] so we return immediately, if
            // "S SystemLiteral" is not detected. We skip blanks if no
            // system literal was found, but this is harmless since we must
            // be at the end of a NotationDecl.
            if (*ctxt).skip_blanks() == 0 {
                return null_mut();
            }
            if (*ctxt).current_byte() != b'\'' && (*ctxt).current_byte() != b'"' {
                return null_mut();
            }
        }
        uri = xml_parse_system_literal(ctxt);
        if uri.is_null() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrURIRequired, None);
        }
    }
    uri
}

/// Parse an XML public literal
///
/// `[12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"`
///
/// Returns the PubidLiteral parsed or NULL.
#[doc(alias = "xmlParsePubidLiteral")]
pub(crate) unsafe extern "C" fn xml_parse_pubid_literal(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    let mut buf: *mut XmlChar;
    let mut len: i32 = 0;
    let mut size: i32 = XML_PARSER_BUFFER_SIZE as i32;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };
    let mut cur: XmlChar;
    let stop: XmlChar;
    let oldstate: XmlParserInputState = (*ctxt).instate;

    if (*ctxt).current_byte() == b'"' {
        (*ctxt).skip_char();
        stop = b'"';
    } else if (*ctxt).current_byte() == b'\'' {
        (*ctxt).skip_char();
        stop = b'\'';
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotStarted, None);
        return null_mut();
    }
    buf = xml_malloc_atomic(size as usize) as *mut XmlChar;
    if buf.is_null() {
        xml_err_memory(ctxt, None);
        return null_mut();
    }
    (*ctxt).instate = XmlParserInputState::XmlParserPublicLiteral;
    cur = (*ctxt).current_byte();
    while xml_is_pubid_char(cur as u32) && cur != stop {
        /* checked */
        if len + 1 >= size {
            size *= 2;
            let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize) as *mut XmlChar;
            if tmp.is_null() {
                xml_err_memory(ctxt, None);
                xml_free(buf as _);
                return null_mut();
            }
            buf = tmp;
        }
        *buf.add(len as usize) = cur;
        len += 1;
        if len > max_length {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("Public ID"));
            xml_free(buf as _);
            return null_mut();
        }
        (*ctxt).skip_char();
        cur = (*ctxt).current_byte();
    }
    *buf.add(len as usize) = 0;
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        xml_free(buf as _);
        return null_mut();
    }
    if cur != stop {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotFinished, None);
    } else {
        (*ctxt).advance_with_line_handling(1);
    }
    (*ctxt).instate = oldstate;
    buf
}

/// Parse a notation declaration. Always consumes '<!'.
///
/// `[82] NotationDecl ::= '<!NOTATION' S Name S (ExternalID |  PublicID) S? '>'`
///
/// Hence there is actually 3 choices:
/// - 'PUBLIC' S PubidLiteral
/// - 'PUBLIC' S PubidLiteral S SystemLiteral
/// - 'SYSTEM' S SystemLiteral
///
/// See the NOTE on xmlParseExternalID().
#[doc(alias = "xmlParseNotationDecl")]
pub(crate) unsafe fn xml_parse_notation_decl(ctxt: XmlParserCtxtPtr) {
    let name: *const XmlChar;
    let mut pubid: *mut XmlChar = null_mut();
    let systemid: *mut XmlChar;

    if (*ctxt).current_byte() != b'<' || (*ctxt).nth_byte(1) != b'!' {
        return;
    }
    (*ctxt).advance(2);

    if (*ctxt).content_bytes().starts_with(b"NOTATION") {
        let inputid: i32 = (*(*ctxt).input).id;
        (*ctxt).advance(8);
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after '<!NOTATION'\n",
            );
            return;
        }

        name = xml_parse_name(ctxt);
        if name.is_null() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotStarted, None);
            return;
        }
        let name = CStr::from_ptr(name as *const i8).to_string_lossy();
        if name.contains(':') {
            xml_ns_err!(
                ctxt,
                XmlParserErrors::XmlNsErrColon,
                "colons are forbidden from notation names '{}'\n",
                name
            );
        }
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after the NOTATION name'\n",
            );
            return;
        }

        // Parse the IDs.
        systemid = xml_parse_external_id(ctxt, addr_of_mut!(pubid), 0);
        (*ctxt).skip_blanks();

        if (*ctxt).current_byte() == b'>' {
            if inputid != (*(*ctxt).input).id {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    "Notation declaration doesn't start and stop in the same entity\n",
                );
            }
            (*ctxt).skip_char();
            if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                if let Some(not) = (*(*ctxt).sax).notation_decl {
                    not(
                        (*ctxt).user_data.clone(),
                        &name,
                        (!pubid.is_null())
                            .then(|| CStr::from_ptr(pubid as *const i8).to_string_lossy())
                            .as_deref(),
                        (!systemid.is_null())
                            .then(|| CStr::from_ptr(systemid as *const i8).to_string_lossy())
                            .as_deref(),
                    );
                }
            }
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotFinished, None);
        }
        if !systemid.is_null() {
            xml_free(systemid as _);
        }
        if !pubid.is_null() {
            xml_free(pubid as _);
        }
    }
}

/// Parse an entity declaration. Always consumes '<!'.
///
/// `[70] EntityDecl ::= GEDecl | PEDecl`
///
/// `[71] GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'`
///
/// `[72] PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'`
///
/// `[73] EntityDef ::= EntityValue | (ExternalID NDataDecl?)`
///
/// `[74] PEDef ::= EntityValue | ExternalID`
///
/// `[76] NDataDecl ::= S 'NDATA' S Name`
///
/// `[ VC: Notation Declared ]`  
/// The Name must match the declared name of a notation.
#[doc(alias = "xmlParseEntityDecl")]
pub(crate) unsafe fn xml_parse_entity_decl(ctxt: XmlParserCtxtPtr) {
    let name: *const XmlChar;
    let mut value: *mut XmlChar = null_mut();
    let mut uri: *mut XmlChar = null_mut();
    let mut literal: *mut XmlChar = null_mut();
    let ndata: *const XmlChar;
    let mut is_parameter: i32 = 0;
    let mut orig: *mut XmlChar = null_mut();

    if (*ctxt).current_byte() != b'<' || (*ctxt).nth_byte(1) != b'!' {
        return;
    }
    (*ctxt).advance(2);

    /* GROW; done in the caller */
    if (*ctxt).content_bytes().starts_with(b"ENTITY") {
        let inputid: i32 = (*(*ctxt).input).id;
        (*ctxt).advance(6);
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after '<!ENTITY'\n",
            );
        }

        if (*ctxt).current_byte() == b'%' {
            (*ctxt).skip_char();
            if (*ctxt).skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after '%'\n",
                );
            }
            is_parameter = 1;
        }

        name = xml_parse_name(ctxt);
        if name.is_null() {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "xmlParseEntityDecl: no name\n",
            );
            return;
        }
        let name = CStr::from_ptr(name as *const i8).to_string_lossy();
        if name.contains(':') {
            xml_ns_err!(
                ctxt,
                XmlParserErrors::XmlNsErrColon,
                "colons are forbidden from entities names '{}'\n",
                name
            );
        }
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after the entity name\n",
            );
        }

        (*ctxt).instate = XmlParserInputState::XmlParserEntityDecl;
        // handle the various case of definitions...
        if is_parameter != 0 {
            if (*ctxt).current_byte() == b'"' || (*ctxt).current_byte() == b'\'' {
                value = xml_parse_entity_value(ctxt, addr_of_mut!(orig));
                if !value.is_null() && !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                    if let Some(ent) = (*(*ctxt).sax).entity_decl {
                        ent(
                            (*ctxt).user_data.clone(),
                            &name,
                            XmlEntityType::XmlInternalParameterEntity,
                            None,
                            None,
                            Some(&CStr::from_ptr(value as *const i8).to_string_lossy()),
                        );
                    }
                }
            } else {
                uri = xml_parse_external_id(ctxt, addr_of_mut!(literal), 1);
                if uri.is_null() && literal.is_null() {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrValueRequired, None);
                }
                if !uri.is_null() {
                    let parsed_uri = xml_parse_uri(uri as *const c_char);
                    if parsed_uri.is_null() {
                        let uri = CStr::from_ptr(uri as *const i8).to_string_lossy();
                        xml_err_msg_str!(
                            ctxt,
                            XmlParserErrors::XmlErrInvalidURI,
                            "Invalid URI: {}\n",
                            uri
                        );
                    // This really ought to be a well formedness error
                    // but the XML Core WG decided otherwise c.f. issue
                    // E26 of the XML erratas.
                    } else {
                        if !(*parsed_uri).fragment.is_null() {
                            // Okay this is foolish to block those but not invalid URIs.
                            xml_fatal_err(ctxt, XmlParserErrors::XmlErrURIFragment, None);
                        } else if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                            if let Some(ent) = (*(*ctxt).sax).entity_decl {
                                ent(
                                    (*ctxt).user_data.clone(),
                                    &name,
                                    XmlEntityType::XmlExternalParameterEntity,
                                    (!literal.is_null())
                                        .then(|| {
                                            CStr::from_ptr(literal as *const i8).to_string_lossy()
                                        })
                                        .as_deref(),
                                    (!uri.is_null())
                                        .then(|| CStr::from_ptr(uri as *const i8).to_string_lossy())
                                        .as_deref(),
                                    None,
                                );
                            }
                        }
                        xml_free_uri(parsed_uri);
                    }
                }
            }
        } else if (*ctxt).current_byte() == b'"' || (*ctxt).current_byte() == b'\'' {
            value = xml_parse_entity_value(ctxt, addr_of_mut!(orig));
            if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                if let Some(ent) = (*(*ctxt).sax).entity_decl {
                    ent(
                        (*ctxt).user_data.clone(),
                        &name,
                        XmlEntityType::XmlInternalGeneralEntity,
                        None,
                        None,
                        (!value.is_null())
                            .then(|| CStr::from_ptr(value as *const i8).to_string_lossy())
                            .as_deref(),
                    );
                }
            }
            // For expat compatibility in SAX mode.
            if (*ctxt).my_doc.is_null()
                || (*(*ctxt).my_doc).version.as_deref() == Some(SAX_COMPAT_MODE)
            {
                if (*ctxt).my_doc.is_null() {
                    (*ctxt).my_doc = xml_new_doc(Some(SAX_COMPAT_MODE));
                    if (*ctxt).my_doc.is_null() {
                        xml_err_memory(ctxt, Some("New Doc failed"));
                        // goto done;
                        if !value.is_null() {
                            xml_free(value as _);
                        }
                        if !uri.is_null() {
                            xml_free(uri as _);
                        }
                        if !literal.is_null() {
                            xml_free(literal as _);
                        }
                        if !orig.is_null() {
                            xml_free(orig as _);
                        }
                        return;
                    }
                    (*(*ctxt).my_doc).properties = XmlDocProperties::XmlDocInternal as i32;
                }
                if (*(*ctxt).my_doc).int_subset.is_null() {
                    (*(*ctxt).my_doc).int_subset =
                        xml_new_dtd((*ctxt).my_doc, Some("fake"), None, None);
                }

                xml_sax2_entity_decl(
                    Some(GenericErrorContext::new(ctxt)),
                    &name,
                    XmlEntityType::XmlInternalGeneralEntity,
                    None,
                    None,
                    (!value.is_null())
                        .then(|| CStr::from_ptr(value as *const i8).to_string_lossy())
                        .as_deref(),
                );
            }
        } else {
            uri = xml_parse_external_id(ctxt, addr_of_mut!(literal), 1);
            if uri.is_null() && literal.is_null() {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrValueRequired, None);
            }
            if !uri.is_null() {
                let parsed_uri = xml_parse_uri(uri as *const c_char);
                if parsed_uri.is_null() {
                    let uri = CStr::from_ptr(uri as *const i8).to_string_lossy();
                    xml_err_msg_str!(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidURI,
                        "Invalid URI: {}\n",
                        uri
                    );
                // This really ought to be a well formedness error
                // but the XML Core WG decided otherwise c.f. issue
                // E26 of the XML erratas.
                } else {
                    if !(*parsed_uri).fragment.is_null() {
                        // Okay this is foolish to block those but not invalid URIs.
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrURIFragment, None);
                    }
                    xml_free_uri(parsed_uri);
                }
            }
            if (*ctxt).current_byte() != b'>' && (*ctxt).skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required before 'NDATA'\n",
                );
            }
            if (*ctxt).content_bytes().starts_with(b"NDATA") {
                (*ctxt).advance(5);
                if (*ctxt).skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required after 'NDATA'\n",
                    );
                }
                ndata = xml_parse_name(ctxt);
                if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                    if let Some(unparsed_ent) = (*(*ctxt).sax).unparsed_entity_decl {
                        unparsed_ent(
                            (*ctxt).user_data.clone(),
                            &name,
                            (!literal.is_null())
                                .then(|| CStr::from_ptr(literal as *const i8).to_string_lossy())
                                .as_deref(),
                            (!uri.is_null())
                                .then(|| CStr::from_ptr(uri as *const i8).to_string_lossy())
                                .as_deref(),
                            (!ndata.is_null())
                                .then(|| CStr::from_ptr(ndata as *const i8).to_string_lossy())
                                .as_deref(),
                        );
                    }
                }
            } else {
                if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                    if let Some(ent) = (*(*ctxt).sax).entity_decl {
                        ent(
                            (*ctxt).user_data.clone(),
                            &name,
                            XmlEntityType::XmlExternalGeneralParsedEntity,
                            (!literal.is_null())
                                .then(|| CStr::from_ptr(literal as *const i8).to_string_lossy())
                                .as_deref(),
                            (!uri.is_null())
                                .then(|| CStr::from_ptr(uri as *const i8).to_string_lossy())
                                .as_deref(),
                            None,
                        );
                    }
                }
                // For expat compatibility in SAX mode.
                // assuming the entity replacement was asked for
                if (*ctxt).replace_entities != 0
                    && ((*ctxt).my_doc.is_null()
                        || (*(*ctxt).my_doc).version.as_deref() == Some(SAX_COMPAT_MODE))
                {
                    if (*ctxt).my_doc.is_null() {
                        (*ctxt).my_doc = xml_new_doc(Some(SAX_COMPAT_MODE));
                        if (*ctxt).my_doc.is_null() {
                            xml_err_memory(ctxt, Some("New Doc failed"));
                            // goto done;
                            if !value.is_null() {
                                xml_free(value as _);
                            }
                            if !uri.is_null() {
                                xml_free(uri as _);
                            }
                            if !literal.is_null() {
                                xml_free(literal as _);
                            }
                            if !orig.is_null() {
                                xml_free(orig as _);
                            }
                            return;
                        }
                        (*(*ctxt).my_doc).properties = XmlDocProperties::XmlDocInternal as i32;
                    }

                    if (*(*ctxt).my_doc).int_subset.is_null() {
                        (*(*ctxt).my_doc).int_subset =
                            xml_new_dtd((*ctxt).my_doc, Some("fake"), None, None);
                    }
                    xml_sax2_entity_decl(
                        Some(GenericErrorContext::new(ctxt)),
                        &name,
                        XmlEntityType::XmlExternalGeneralParsedEntity,
                        (!literal.is_null())
                            .then(|| CStr::from_ptr(literal as *const i8).to_string_lossy())
                            .as_deref(),
                        (!uri.is_null())
                            .then(|| CStr::from_ptr(uri as *const i8).to_string_lossy())
                            .as_deref(),
                        None,
                    );
                }
            }
        }
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            // goto done;
            if !value.is_null() {
                xml_free(value as _);
            }
            if !uri.is_null() {
                xml_free(uri as _);
            }
            if !literal.is_null() {
                xml_free(literal as _);
            }
            if !orig.is_null() {
                xml_free(orig as _);
            }
            return;
        }
        (*ctxt).skip_blanks();
        if (*ctxt).current_byte() != b'>' {
            xml_fatal_err_msg_str!(
                ctxt,
                XmlParserErrors::XmlErrEntityNotFinished,
                "xmlParseEntityDecl: entity {} not terminated\n",
                name
            );
            (*ctxt).halt();
        } else {
            if inputid != (*(*ctxt).input).id {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    "Entity declaration doesn't start and stop in the same entity\n",
                );
            }
            (*ctxt).skip_char();
        }
        if !orig.is_null() {
            // Ugly mechanism to save the raw entity value.
            let mut cur: XmlEntityPtr = null_mut();

            if is_parameter != 0 {
                if !(*ctxt).sax.is_null() {
                    if let Some(ent) = (*(*ctxt).sax).get_parameter_entity {
                        cur = ent((*ctxt).user_data.clone(), &name);
                    }
                }
            } else {
                if !(*ctxt).sax.is_null() {
                    if let Some(ent) = (*(*ctxt).sax).get_entity {
                        cur = ent((*ctxt).user_data.clone(), &name);
                    }
                }
                if cur.is_null()
                    && (*ctxt)
                        .user_data
                        .as_ref()
                        .and_then(|d| d.lock().downcast_ref::<XmlParserCtxtPtr>().copied())
                        == Some(ctxt)
                {
                    cur = xml_sax2_get_entity(Some(GenericErrorContext::new(ctxt)), &name);
                }
            }
            if !cur.is_null() && (*cur).orig.load(Ordering::Relaxed).is_null() {
                (*cur).orig.store(orig, Ordering::Relaxed);
                orig = null_mut();
            }
        }

        //  done:
        if !value.is_null() {
            xml_free(value as _);
        }
        if !uri.is_null() {
            xml_free(uri as _);
        }
        if !literal.is_null() {
            xml_free(literal as _);
        }
        if !orig.is_null() {
            xml_free(orig as _);
        }
    }
}

/// Parse an attribute list declaration for an element. Always consumes '<!'.
///
/// `[52] AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'`
///
/// `[53] AttDef ::= S Name S AttType S DefaultDecl`
#[doc(alias = "xmlParseAttributeListDecl")]
pub(crate) unsafe fn xml_parse_attribute_list_decl(ctxt: XmlParserCtxtPtr) {
    let elem_name: *const XmlChar;
    let mut attr_name: *const XmlChar;
    let mut tree: XmlEnumerationPtr;

    if (*ctxt).current_byte() != b'<' || (*ctxt).nth_byte(1) != b'!' {
        return;
    }
    (*ctxt).advance(2);

    if (*ctxt).content_bytes().starts_with(b"ATTLIST") {
        let inputid: i32 = (*(*ctxt).input).id;

        (*ctxt).advance(7);
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after '<!ATTLIST'\n",
            );
        }
        elem_name = xml_parse_name(ctxt);
        if elem_name.is_null() {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "ATTLIST: no name for Element\n",
            );
            return;
        }
        (*ctxt).skip_blanks();
        (*ctxt).grow();
        #[allow(clippy::while_immutable_condition)]
        while (*ctxt).current_byte() != b'>'
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            let mut default_value: *mut XmlChar = null_mut();

            (*ctxt).grow();
            tree = null_mut();
            attr_name = xml_parse_name(ctxt);
            if attr_name.is_null() {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    "ATTLIST: no name for Attribute\n",
                );
                break;
            }
            (*ctxt).grow();
            if (*ctxt).skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after the attribute name\n",
                );
                break;
            }

            let Some(typ) = xml_parse_attribute_type(ctxt, addr_of_mut!(tree)) else {
                break;
            };

            (*ctxt).grow();
            if (*ctxt).skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after the attribute typ\n",
                );
                if !tree.is_null() {
                    xml_free_enumeration(tree);
                }
                break;
            }

            let def = xml_parse_default_decl(ctxt, addr_of_mut!(default_value));
            if typ != XmlAttributeType::XmlAttributeCDATA && !default_value.is_null() {
                xml_attr_normalize_space(default_value, default_value);
            }

            (*ctxt).grow();
            if (*ctxt).current_byte() != b'>' && (*ctxt).skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after the attribute default value\n",
                );
                if !default_value.is_null() {
                    xml_free(default_value as _);
                }
                if !tree.is_null() {
                    xml_free_enumeration(tree);
                }
                break;
            }
            if !(*ctxt).sax.is_null()
                && (*ctxt).disable_sax == 0
                && (*(*ctxt).sax).attribute_decl.is_some()
            {
                if let Some(attr) = (*(*ctxt).sax).attribute_decl {
                    attr(
                        (*ctxt).user_data.clone(),
                        CStr::from_ptr(elem_name as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                        &CStr::from_ptr(attr_name as *const i8).to_string_lossy(),
                        typ,
                        def,
                        (!default_value.is_null())
                            .then(|| CStr::from_ptr(default_value as *const i8).to_string_lossy())
                            .as_deref(),
                        tree,
                    );
                }
            } else if !tree.is_null() {
                xml_free_enumeration(tree);
            }

            if (*ctxt).sax2 != 0
                && !default_value.is_null()
                && def != XmlAttributeDefault::XmlAttributeImplied
                && def != XmlAttributeDefault::XmlAttributeRequired
            {
                xml_add_def_attrs(ctxt, elem_name, attr_name, default_value);
            }
            if (*ctxt).sax2 != 0 {
                xml_add_special_attr(ctxt, elem_name, attr_name, typ);
            }
            if !default_value.is_null() {
                xml_free(default_value as _);
            }
            (*ctxt).grow();
        }
        if (*ctxt).current_byte() == b'>' {
            if inputid != (*(*ctxt).input).id {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    "Attribute list declaration doesn't start and stop in the same entity\n",
                );
            }
            (*ctxt).skip_char();
        }
    }
}

/// Parse the declaration for a Mixed Element content
/// The leading '(' and spaces have been skipped in xmlParseElementContentDecl
///
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
#[doc(alias = "xmlParseElementChildrenContentDeclPriv")]
pub(crate) unsafe extern "C" fn xml_parse_element_children_content_decl_priv(
    ctxt: XmlParserCtxtPtr,
    inputchk: i32,
    depth: i32,
) -> XmlElementContentPtr {
    let mut ret: XmlElementContentPtr;
    let mut cur: XmlElementContentPtr;
    let mut last: XmlElementContentPtr = null_mut();
    let mut op: XmlElementContentPtr;
    let mut elem: *const XmlChar;
    let mut typ: XmlChar = 0;

    if (depth > 128 && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0) || depth > 2048
    {
        xml_fatal_err_msg_int!(
            ctxt,
            XmlParserErrors::XmlErrElemcontentNotFinished,
            "xmlParseElementChildrenContentDecl : depth %d too deep, use xmlParserOption::XML_PARSE_HUGE\n",
            depth
        );
        return null_mut();
    }
    (*ctxt).skip_blanks();
    (*ctxt).grow();
    if (*ctxt).current_byte() == b'(' {
        let inputid: i32 = (*(*ctxt).input).id;

        /* Recurse on first child */
        (*ctxt).skip_char();
        (*ctxt).skip_blanks();
        cur = xml_parse_element_children_content_decl_priv(ctxt, inputid, depth + 1);
        ret = cur;
        if cur.is_null() {
            return null_mut();
        }
        (*ctxt).skip_blanks();
        (*ctxt).grow();
    } else {
        elem = xml_parse_name(ctxt);
        if elem.is_null() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrElemcontentNotStarted, None);
            return null_mut();
        }
        cur = xml_new_doc_element_content(
            (*ctxt).my_doc,
            elem,
            XmlElementContentType::XmlElementContentElement,
        );
        ret = cur;
        if cur.is_null() {
            xml_err_memory(ctxt, None);
            return null_mut();
        }
        (*ctxt).grow();
        if (*ctxt).current_byte() == b'?' {
            (*cur).ocur = XmlElementContentOccur::XmlElementContentOpt;
            (*ctxt).skip_char();
        } else if (*ctxt).current_byte() == b'*' {
            (*cur).ocur = XmlElementContentOccur::XmlElementContentMult;
            (*ctxt).skip_char();
        } else if (*ctxt).current_byte() == b'+' {
            (*cur).ocur = XmlElementContentOccur::XmlElementContentPlus;
            (*ctxt).skip_char();
        } else {
            (*cur).ocur = XmlElementContentOccur::XmlElementContentOnce;
        }
        (*ctxt).grow();
    }
    (*ctxt).skip_blanks();
    #[allow(clippy::while_immutable_condition)]
    while ((*ctxt).current_byte() != b')')
        && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        /*
         * Each loop we parse one separator and one element.
         */
        if (*ctxt).current_byte() == b',' {
            if typ == 0 {
                typ = (*ctxt).current_byte();
            }
            // Detect "Name | Name , Name" error
            else if typ != (*ctxt).current_byte() {
                xml_fatal_err_msg_int!(
                    ctxt,
                    XmlParserErrors::XmlErrSeparatorRequired,
                    format!(
                        "xmlParseElementChildrenContentDecl : '{}' expected\n",
                        typ as char
                    )
                    .as_str(),
                    typ as i32
                );
                if !last.is_null() && last != ret {
                    xml_free_doc_element_content((*ctxt).my_doc, last);
                }
                if !ret.is_null() {
                    xml_free_doc_element_content((*ctxt).my_doc, ret);
                }
                return null_mut();
            }
            (*ctxt).skip_char();

            op = xml_new_doc_element_content(
                (*ctxt).my_doc,
                null_mut(),
                XmlElementContentType::XmlElementContentSeq,
            );
            if op.is_null() {
                if !last.is_null() && last != ret {
                    xml_free_doc_element_content((*ctxt).my_doc, last);
                }
                xml_free_doc_element_content((*ctxt).my_doc, ret);
                return null_mut();
            }
            if last.is_null() {
                (*op).c1 = ret;
                if !ret.is_null() {
                    (*ret).parent = op;
                }
                ret = op;
                cur = ret;
            } else {
                (*cur).c2 = op;
                if !op.is_null() {
                    (*op).parent = cur;
                }
                (*op).c1 = last;
                if !last.is_null() {
                    (*last).parent = op;
                }
                cur = op;
                // last = null_mut();
            }
        } else if (*ctxt).current_byte() == b'|' {
            if typ == 0 {
                typ = (*ctxt).current_byte();
            }
            // Detect "Name , Name | Name" error
            else if typ != (*ctxt).current_byte() {
                xml_fatal_err_msg_int!(
                    ctxt,
                    XmlParserErrors::XmlErrSeparatorRequired,
                    format!(
                        "xmlParseElementChildrenContentDecl : '{}' expected\n",
                        typ as char
                    )
                    .as_str(),
                    typ as i32
                );
                if !last.is_null() && last != ret {
                    xml_free_doc_element_content((*ctxt).my_doc, last);
                }
                if !ret.is_null() {
                    xml_free_doc_element_content((*ctxt).my_doc, ret);
                }
                return null_mut();
            }
            (*ctxt).skip_char();

            op = xml_new_doc_element_content(
                (*ctxt).my_doc,
                null_mut(),
                XmlElementContentType::XmlElementContentOr,
            );
            if op.is_null() {
                if !last.is_null() && last != ret {
                    xml_free_doc_element_content((*ctxt).my_doc, last);
                }
                if !ret.is_null() {
                    xml_free_doc_element_content((*ctxt).my_doc, ret);
                }
                return null_mut();
            }
            if last.is_null() {
                (*op).c1 = ret;
                if !ret.is_null() {
                    (*ret).parent = op;
                }
                ret = op;
                cur = ret;
            } else {
                (*cur).c2 = op;
                if !op.is_null() {
                    (*op).parent = cur;
                }
                (*op).c1 = last;
                if !last.is_null() {
                    (*last).parent = op;
                }
                cur = op;
                // last = null_mut();
            }
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrElemcontentNotFinished, None);
            if !last.is_null() && last != ret {
                xml_free_doc_element_content((*ctxt).my_doc, last);
            }
            if !ret.is_null() {
                xml_free_doc_element_content((*ctxt).my_doc, ret);
            }
            return null_mut();
        }
        (*ctxt).grow();
        (*ctxt).skip_blanks();
        (*ctxt).grow();
        if (*ctxt).current_byte() == b'(' {
            let inputid: i32 = (*(*ctxt).input).id;
            /* Recurse on second child */
            (*ctxt).skip_char();
            (*ctxt).skip_blanks();
            last = xml_parse_element_children_content_decl_priv(ctxt, inputid, depth + 1);
            if last.is_null() {
                if !ret.is_null() {
                    xml_free_doc_element_content((*ctxt).my_doc, ret);
                }
                return null_mut();
            }
            (*ctxt).skip_blanks();
        } else {
            elem = xml_parse_name(ctxt);
            if elem.is_null() {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrElemcontentNotStarted, None);
                if !ret.is_null() {
                    xml_free_doc_element_content((*ctxt).my_doc, ret);
                }
                return null_mut();
            }
            last = xml_new_doc_element_content(
                (*ctxt).my_doc,
                elem,
                XmlElementContentType::XmlElementContentElement,
            );
            if last.is_null() {
                if !ret.is_null() {
                    xml_free_doc_element_content((*ctxt).my_doc, ret);
                }
                return null_mut();
            }
            if (*ctxt).current_byte() == b'?' {
                (*last).ocur = XmlElementContentOccur::XmlElementContentOpt;
                (*ctxt).skip_char();
            } else if (*ctxt).current_byte() == b'*' {
                (*last).ocur = XmlElementContentOccur::XmlElementContentMult;
                (*ctxt).skip_char();
            } else if (*ctxt).current_byte() == b'+' {
                (*last).ocur = XmlElementContentOccur::XmlElementContentPlus;
                (*ctxt).skip_char();
            } else {
                (*last).ocur = XmlElementContentOccur::XmlElementContentOnce;
            }
        }
        (*ctxt).skip_blanks();
        (*ctxt).grow();
    }
    if !cur.is_null() && !last.is_null() {
        (*cur).c2 = last;
        if !last.is_null() {
            (*last).parent = cur;
        }
    }
    if (*(*ctxt).input).id != inputchk {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrEntityBoundary,
            "Element content declaration doesn't start and stop in the same entity\n",
        );
    }
    (*ctxt).skip_char();
    if (*ctxt).current_byte() == b'?' {
        if !ret.is_null() {
            if matches!((*ret).ocur, XmlElementContentOccur::XmlElementContentPlus)
                || matches!((*ret).ocur, XmlElementContentOccur::XmlElementContentMult)
            {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
            } else {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentOpt;
            }
        }
        (*ctxt).skip_char();
    } else if (*ctxt).current_byte() == b'*' {
        if !ret.is_null() {
            (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
            cur = ret;
            /*
             * Some normalization:
             * (a | b* | c?)* == (a | b | c)*
             */
            while !cur.is_null() && matches!((*cur).typ, XmlElementContentType::XmlElementContentOr)
            {
                if !(*cur).c1.is_null()
                    && (matches!(
                        (*(*cur).c1).ocur,
                        XmlElementContentOccur::XmlElementContentOpt
                    ) || matches!(
                        (*(*cur).c1).ocur,
                        XmlElementContentOccur::XmlElementContentMult
                    ))
                {
                    (*(*cur).c1).ocur = XmlElementContentOccur::XmlElementContentOnce;
                }
                if !(*cur).c2.is_null()
                    && (matches!(
                        (*(*cur).c2).ocur,
                        XmlElementContentOccur::XmlElementContentOpt
                    ) || matches!(
                        (*(*cur).c2).ocur,
                        XmlElementContentOccur::XmlElementContentMult
                    ))
                {
                    (*(*cur).c2).ocur = XmlElementContentOccur::XmlElementContentOnce;
                }
                cur = (*cur).c2;
            }
        }
        (*ctxt).skip_char();
    } else if (*ctxt).current_byte() == b'+' {
        if !ret.is_null() {
            let mut found: i32 = 0;

            if matches!((*ret).ocur, XmlElementContentOccur::XmlElementContentOpt)
                || matches!((*ret).ocur, XmlElementContentOccur::XmlElementContentMult)
            {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
            } else {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentPlus;
            }
            /*
             * Some normalization:
             * (a | b*)+ == (a | b)*
             * (a | b?)+ == (a | b)*
             */
            while !cur.is_null() && matches!((*cur).typ, XmlElementContentType::XmlElementContentOr)
            {
                if !(*cur).c1.is_null()
                    && (matches!(
                        (*(*cur).c1).ocur,
                        XmlElementContentOccur::XmlElementContentOpt
                    ) || matches!(
                        (*(*cur).c1).ocur,
                        XmlElementContentOccur::XmlElementContentMult
                    ))
                {
                    (*(*cur).c1).ocur = XmlElementContentOccur::XmlElementContentOnce;
                    found = 1;
                }
                if !(*cur).c2.is_null()
                    && (matches!(
                        (*(*cur).c2).ocur,
                        XmlElementContentOccur::XmlElementContentOpt
                    ) || matches!(
                        (*(*cur).c2).ocur,
                        XmlElementContentOccur::XmlElementContentMult
                    ))
                {
                    (*(*cur).c2).ocur = XmlElementContentOccur::XmlElementContentOnce;
                    found = 1;
                }
                cur = (*cur).c2;
            }
            if found != 0 {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
            }
        }
        (*ctxt).skip_char();
    }
    ret
}

/// Parse an element declaration. Always consumes '<!'.
///
/// `[45] elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'`
///
/// `[ VC: Unique Element Type Declaration ]`  
/// No element type may be declared more than once
///
/// Returns the type of the element, or -1 in case of error
#[doc(alias = "xmlParseElementDecl")]
pub(crate) unsafe fn xml_parse_element_decl(ctxt: XmlParserCtxtPtr) -> i32 {
    let name: *const XmlChar;
    let mut content: XmlElementContentPtr = null_mut();

    if (*ctxt).current_byte() != b'<' || (*ctxt).nth_byte(1) != b'!' {
        return -1;
    }
    (*ctxt).advance(2);

    // GROW; done in the caller
    if (*ctxt).content_bytes().starts_with(b"ELEMENT") {
        let inputid: i32 = (*(*ctxt).input).id;

        (*ctxt).advance(7);
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after 'ELEMENT'\n",
            );
            return -1;
        }
        name = xml_parse_name(ctxt);
        if name.is_null() {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "xmlParseElementDecl: no name for Element\n",
            );
            return -1;
        }
        if (*ctxt).skip_blanks() == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after the element name\n",
            );
        }
        let ret = if (*ctxt).content_bytes().starts_with(b"EMPTY") {
            (*ctxt).advance(5);
            // Element must always be empty.
            Some(XmlElementTypeVal::XmlElementTypeEmpty)
        } else if (*ctxt).current_byte() == b'A'
            && (*ctxt).nth_byte(1) == b'N'
            && (*ctxt).nth_byte(2) == b'Y'
        {
            (*ctxt).advance(3);
            // Element is a generic container.
            Some(XmlElementTypeVal::XmlElementTypeAny)
        } else if (*ctxt).current_byte() == b'(' {
            xml_parse_element_content_decl(ctxt, name, addr_of_mut!(content))
        } else {
            // [ WFC: PEs in Internal Subset ] error handling.
            if (*ctxt).current_byte() == b'%'
                && (*ctxt).external == 0
                && (*ctxt).input_tab.len() == 1
            {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrPERefInIntSubset,
                    "PEReference: forbidden within markup decl in internal subset\n",
                );
            } else {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrElemcontentNotStarted,
                    "xmlParseElementDecl: 'EMPTY', 'ANY' or '(' expected\n",
                );
            }
            return -1;
        };

        (*ctxt).skip_blanks();

        if (*ctxt).current_byte() != b'>' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrGtRequired, None);
            if !content.is_null() {
                xml_free_doc_element_content((*ctxt).my_doc, content);
            }
        } else {
            if inputid != (*(*ctxt).input).id {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    "Element declaration doesn't start and stop in the same entity\n",
                );
            }

            (*ctxt).skip_char();
            if !(*ctxt).sax.is_null()
                && (*ctxt).disable_sax == 0
                && (*(*ctxt).sax).element_decl.is_some()
            {
                if !content.is_null() {
                    (*content).parent = null_mut();
                }
                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                ((*(*ctxt).sax).element_decl.unwrap())(
                    (*ctxt).user_data.clone(),
                    &name,
                    ret,
                    content,
                );
                if !content.is_null() && (*content).parent.is_null() {
                    // this is a trick: if xmlAddElementDecl is called,
                    // instead of copying the full tree it is plugged directly
                    // if called from the parser. Avoid duplicating the
                    // interfaces or change the API/ABI
                    xml_free_doc_element_content((*ctxt).my_doc, content);
                }
            } else if !content.is_null() {
                xml_free_doc_element_content((*ctxt).my_doc, content);
            }
        }
        return ret.map_or(-1, |ret| ret as i32);
    }
    -1
}

/// Parse markup declarations. Always consumes '<!' or '<?'.
///
/// `[29] markupdecl ::= elementdecl | AttlistDecl | EntityDecl | NotationDecl | PI | Comment`
///
/// `[ VC: Proper Declaration/PE Nesting ]`  
/// Parameter-entity replacement text must be properly nested with
/// markup declarations. That is to say, if either the first character
/// or the last character of a markup declaration (markupdecl above) is
/// contained in the replacement text for a parameter-entity reference,
/// both must be contained in the same replacement text.
///
/// `[ WFC: PEs in Internal Subset ]`  
/// In the internal DTD subset, parameter-entity references can occur
/// only where markup declarations can occur, not within markup declarations.
/// (This does not apply to references that occur in external parameter
/// entities or to the external subset.)
#[doc(alias = "xmlParseMarkupDecl")]
pub(crate) unsafe extern "C" fn xml_parse_markup_decl(ctxt: XmlParserCtxtPtr) {
    (*ctxt).grow();
    if (*ctxt).current_byte() == b'<' {
        if (*ctxt).nth_byte(1) == b'!' {
            match (*ctxt).nth_byte(2) {
                b'E' => {
                    if (*ctxt).nth_byte(3) == b'L' {
                        xml_parse_element_decl(ctxt);
                    } else if (*ctxt).nth_byte(3) == b'N' {
                        xml_parse_entity_decl(ctxt);
                    } else {
                        (*ctxt).advance(2);
                    }
                }
                b'A' => {
                    xml_parse_attribute_list_decl(ctxt);
                }
                b'N' => {
                    xml_parse_notation_decl(ctxt);
                }
                b'-' => {
                    xml_parse_comment(ctxt);
                }
                _ => {
                    /* there is an error but it will be detected later */
                    (*ctxt).advance(2);
                }
            }
        } else if (*ctxt).nth_byte(1) == b'?' {
            xml_parse_pi(ctxt);
        }
    }

    /*
     * detect requirement to exit there and act accordingly
     * and avoid having instate overridden later on
     */
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }

    (*ctxt).instate = XmlParserInputState::XmlParserDTD;
}

/// Parse a numeric character reference. Always consumes '&'.
///
/// `[66] CharRef ::= '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'`
///
/// `[ WFC: Legal Character ]`  
/// Characters referred to using character references must match the production for Char.
///
/// Returns the value parsed (as an int), 0 in case of error
#[doc(alias = "xmlParseCharRef")]
pub(crate) unsafe extern "C" fn xml_parse_char_ref(ctxt: XmlParserCtxtPtr) -> i32 {
    let mut val: i32 = 0;
    let mut count: i32 = 0;

    /*
     * Using RAW/CUR/NEXT is okay since we are working on ASCII range here
     */
    if (*ctxt).current_byte() == b'&' && (*ctxt).nth_byte(1) == b'#' && (*ctxt).nth_byte(2) == b'x'
    {
        (*ctxt).advance(3);
        (*ctxt).grow();
        #[allow(clippy::while_immutable_condition)]
        while (*ctxt).current_byte() != b';' {
            /* loop blocked by count */
            let res = {
                let f = count > 20;
                count += 1;
                f
            };
            if res {
                count = 0;
                (*ctxt).grow();
                if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    return 0;
                }
            }
            if ((*ctxt).current_byte() >= b'0') && ((*ctxt).current_byte() <= b'9') {
                val = val * 16 + ((*ctxt).current_byte() - b'0') as i32;
            } else if ((*ctxt).current_byte() >= b'a')
                && ((*ctxt).current_byte() <= b'f')
                && (count < 20)
            {
                val = val * 16 + ((*ctxt).current_byte() - b'a') as i32 + 10;
            } else if ((*ctxt).current_byte() >= b'A')
                && ((*ctxt).current_byte() <= b'F')
                && (count < 20)
            {
                val = val * 16 + ((*ctxt).current_byte() - b'A') as i32 + 10;
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidHexCharRef, None);
                val = 0;
                break;
            }
            if val > 0x110000 {
                val = 0x110000;
            }

            (*ctxt).skip_char();
            count += 1;
        }
        if (*ctxt).current_byte() == b';' {
            /* on purpose to avoid reentrancy problems with NEXT and SKIP */
            (*(*ctxt).input).col += 1;
            (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(1);
        }
    } else if (*ctxt).current_byte() == b'&' && (*ctxt).nth_byte(1) == b'#' {
        (*ctxt).advance(2);
        (*ctxt).grow();
        #[allow(clippy::while_immutable_condition)]
        while (*ctxt).current_byte() != b';' {
            /* loop blocked by count */
            let res = {
                let f = count > 20;
                count += 1;
                f
            };
            if res {
                count = 0;
                (*ctxt).grow();
                if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    return 0;
                }
            }
            if ((*ctxt).current_byte() >= b'0') && ((*ctxt).current_byte() <= b'9') {
                val = val * 10 + ((*ctxt).current_byte() - b'0') as i32;
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidDecCharRef, None);
                val = 0;
                break;
            }
            if val > 0x110000 {
                val = 0x110000;
            }

            (*ctxt).skip_char();
            count += 1;
        }
        if (*ctxt).current_byte() == b';' {
            /* on purpose to avoid reentrancy problems with NEXT and SKIP */
            (*(*ctxt).input).col += 1;
            (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(1);
        }
    } else {
        if (*ctxt).current_byte() == b'&' {
            (*ctxt).advance(1);
        }
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidCharRef, None);
    }

    // [ WFC: Legal Character ]
    // Characters referred to using character references must match the
    // production for Char.
    if val >= 0x110000 {
        xml_fatal_err_msg_int!(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            "xmlParseCharRef: character reference out of bounds\n",
            val
        );
    } else if xml_is_char(val as u32) {
        return val;
    } else {
        xml_fatal_err_msg_int!(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            format!("xmlParseCharRef: invalid XmlChar value {val}\n").as_str(),
            val
        );
    }
    0
}

/// Parse escaped pure raw content. Always consumes '<!['.
///
/// `[18] CDSect ::= CDStart CData CDEnd`
///
/// `[19] CDStart ::= '<![CDATA['`
///
/// `[20] Data ::= (Char* - (Char* ']]>' Char*))`
///
/// `[21] CDEnd ::= ']]>'`
#[doc(alias = "xmlParseCDSect")]
pub(crate) unsafe extern "C" fn xml_parse_cdsect(ctxt: XmlParserCtxtPtr) {
    let mut buf: *mut XmlChar = null_mut();
    let mut len: i32 = 0;
    let mut size: i32 = XML_PARSER_BUFFER_SIZE as i32;
    let mut rl: i32 = 0;
    let mut sl: i32 = 0;
    let mut l: i32 = 0;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH as i32
    } else {
        XML_MAX_TEXT_LENGTH as i32
    };

    if (*ctxt).current_byte() != b'<' || (*ctxt).nth_byte(1) != b'!' || (*ctxt).nth_byte(2) != b'['
    {
        return;
    }
    (*ctxt).advance(3);

    if !(*ctxt).content_bytes().starts_with(b"CDATA[") {
        return;
    }
    (*ctxt).advance(6);

    (*ctxt).instate = XmlParserInputState::XmlParserCDATASection;
    let mut r = (*ctxt).current_char(&mut rl).unwrap_or('\0');
    if !xml_is_char(r as u32) {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrCDATANotFinished, None);
        // goto out;
        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            (*ctxt).instate = XmlParserInputState::XmlParserContent;
        }
        xml_free(buf as _);
        return;
    }
    (*ctxt).advance_with_line_handling(rl as usize);
    let mut s = (*ctxt).current_char(&mut sl).unwrap_or('\0');
    if !xml_is_char(s as u32) {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrCDATANotFinished, None);
        // goto out;
        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            (*ctxt).instate = XmlParserInputState::XmlParserContent;
        }
        xml_free(buf as _);
        return;
    }
    (*ctxt).advance_with_line_handling(sl as usize);
    let mut cur = (*ctxt).current_char(&mut l).unwrap_or('\0');
    buf = xml_malloc_atomic(size as usize) as *mut XmlChar;
    if buf.is_null() {
        xml_err_memory(ctxt, None);
        // goto out;
        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            (*ctxt).instate = XmlParserInputState::XmlParserContent;
        }
        xml_free(buf as _);
        return;
    }
    while xml_is_char(cur as u32) && (r != ']' || s != ']' || cur != '>') {
        if len + 5 >= size {
            let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize * 2) as *mut XmlChar;
            if tmp.is_null() {
                xml_err_memory(ctxt, None);
                // goto out;
                if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                }
                xml_free(buf as _);
                return;
            }
            buf = tmp;
            size *= 2;
        }
        COPY_BUF!(rl, buf, len, r);
        if len > max_length {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrCDATANotFinished,
                "CData section too big found\n",
            );
            // goto out;
            if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                (*ctxt).instate = XmlParserInputState::XmlParserContent;
            }
            xml_free(buf as _);
            return;
        }
        r = s;
        rl = sl;
        s = cur;
        sl = l;
        (*ctxt).advance_with_line_handling(l as usize);
        cur = (*ctxt).current_char(&mut l).unwrap_or('\0');
    }
    *buf.add(len as usize) = 0;
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        xml_free(buf as _);
        return;
    }
    if cur != '>' {
        let b = CStr::from_ptr(buf as *const i8).to_string_lossy();
        xml_fatal_err_msg_str!(
            ctxt,
            XmlParserErrors::XmlErrCDATANotFinished,
            "CData section not finished\n{}\n",
            b
        );
        // goto out;
        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            (*ctxt).instate = XmlParserInputState::XmlParserContent;
        }
        xml_free(buf as _);
        return;
    }
    (*ctxt).advance_with_line_handling(l as usize);

    // OK the buffer is to be consumed as cdata.
    if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
        let s = from_utf8(from_raw_parts(buf, len as usize)).expect("Internal Error");
        if let Some(cdata) = (*(*ctxt).sax).cdata_block {
            cdata((*ctxt).user_data.clone(), s);
        } else if let Some(characters) = (*(*ctxt).sax).characters {
            characters((*ctxt).user_data.clone(), s);
        }
    }

    // out:
    if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        (*ctxt).instate = XmlParserInputState::XmlParserContent;
    }
    xml_free(buf as _);
}

/// parse an XML element
///
/// `[39] element ::= EmptyElemTag | STag content ETag`
///
/// `[ WFC: Element Type Match ]`
/// The Name in an element's end-tag must match the element type in the start-tag.
#[doc(alias = "xmlParseElement")]
pub(crate) unsafe extern "C" fn xml_parse_element(ctxt: XmlParserCtxtPtr) {
    if xml_parse_element_start(ctxt) != 0 {
        return;
    }

    xml_parse_content_internal(ctxt);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }

    if (*ctxt).current_byte() == 0 {
        let name: *const XmlChar = (*ctxt).name_tab[(*ctxt).name_tab.len() - 1];
        let line: i32 = (*ctxt).push_tab[(*ctxt).name_tab.len() - 1].line;
        xml_fatal_err_msg_str_int_str!(
            ctxt,
            XmlParserErrors::XmlErrTagNotFinished,
            "Premature end of data in tag {} line {}\n",
            CStr::from_ptr(name as *const i8).to_string_lossy(),
            line
        );
        return;
    }

    xml_parse_element_end(ctxt);
}

/// Parse the XML version value.
///
/// `[26] VersionNum ::= '1.' [0-9]+`
///
/// In practice allow [0-9].[0-9]+ at that level
///
/// Returns the string giving the XML version number, or NULL
#[doc(alias = "xmlParseVersionNum")]
pub(crate) unsafe fn xml_parse_version_num(ctxt: XmlParserCtxtPtr) -> Option<String> {
    let mut cur: XmlChar;

    let mut buf = String::with_capacity(10);
    cur = (*ctxt).current_byte();
    if !cur.is_ascii_digit() {
        return None;
    }
    buf.push(cur as char);
    (*ctxt).skip_char();
    cur = (*ctxt).current_byte();
    if cur != b'.' {
        return None;
    }
    buf.push(cur as char);
    (*ctxt).skip_char();
    cur = (*ctxt).current_byte();
    while cur.is_ascii_digit() {
        buf.push(cur as char);
        (*ctxt).skip_char();
        cur = (*ctxt).current_byte();
    }
    Some(buf)
}

/// parse the XML encoding name
///
/// `[81] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*`
///
/// Returns the encoding name value or NULL
#[doc(alias = "xmlParseEncName")]
pub(crate) unsafe fn xml_parse_enc_name(ctxt: XmlParserCtxtPtr) -> Option<String> {
    let mut len: i32 = 0;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };
    let mut cur: XmlChar;

    cur = (*ctxt).current_byte();
    if cur.is_ascii_lowercase() || cur.is_ascii_uppercase() {
        let mut buf = String::with_capacity(10);
        buf.push(cur as char);
        len += 1;
        (*ctxt).skip_char();
        cur = (*ctxt).current_byte();
        while cur.is_ascii_lowercase()
            || cur.is_ascii_uppercase()
            || cur.is_ascii_digit()
            || cur == b'.'
            || cur == b'_'
            || cur == b'-'
        {
            buf.push(cur as char);
            len += 1;
            if len > max_length {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrNameTooLong, Some("EncName"));
                return None;
            }
            (*ctxt).skip_char();
            cur = (*ctxt).current_byte();
        }
        return Some(buf);
    }
    xml_fatal_err(ctxt, XmlParserErrors::XmlErrEncodingName, None);
    None
}

/// parse an XML declaration header
///
/// `[23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'`
#[doc(alias = "xmlParseXMLDecl")]
pub(crate) unsafe extern "C" fn xml_parse_xmldecl(ctxt: XmlParserCtxtPtr) {
    /*
     * This value for standalone indicates that the document has an
     * XML declaration but it does not have a standalone attribute.
     * It will be overwritten later if a standalone attribute is found.
     */
    (*(*ctxt).input).standalone = -2;

    /*
     * We know that '<?xml' is here.
     */
    (*ctxt).advance(5);

    if !xml_is_blank_char((*ctxt).current_byte() as u32) {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            "Blank needed after '<?xml'\n",
        );
    }
    (*ctxt).skip_blanks();

    // We must have the VersionInfo here.
    let version = xml_parse_version_info(ctxt);
    if let Some(version) = version {
        if version != XML_DEFAULT_VERSION {
            // Changed here for XML-1.0 5th edition
            if (*ctxt).options & XmlParserOption::XmlParseOld10 as i32 != 0 {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrUnknownVersion,
                    "Unsupported version '{}'\n",
                    version
                );
            } else if version.starts_with("1.") {
                xml_warning_msg!(
                    ctxt,
                    XmlParserErrors::XmlWarUnknownVersion,
                    "Unsupported version '{}'\n",
                    version
                );
            } else {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrUnknownVersion,
                    "Unsupported version '{}'\n",
                    version
                );
            }
        }
        (*ctxt).version = Some(version);
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrVersionMissing, None);
    }

    /*
     * We may have the encoding declaration
     */
    if !xml_is_blank_char((*ctxt).current_byte() as u32) {
        if (*ctxt).current_byte() == b'?' && (*ctxt).nth_byte(1) == b'>' {
            (*ctxt).advance(2);
            return;
        }
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            "Blank needed here\n",
        );
    }
    xml_parse_encoding_decl(ctxt);
    if (*ctxt).err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32
        || matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        /*
         * The XML REC instructs us to stop parsing right here
         */
        return;
    }

    /*
     * We may have the standalone status.
     */
    if (*(*ctxt).input).encoding.is_some() && !xml_is_blank_char((*ctxt).current_byte() as u32) {
        if (*ctxt).current_byte() == b'?' && (*ctxt).nth_byte(1) == b'>' {
            (*ctxt).advance(2);
            return;
        }
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            "Blank needed here\n",
        );
    }

    /*
     * We can grow the input buffer freely at that point
     */
    (*ctxt).grow();

    (*ctxt).skip_blanks();
    (*(*ctxt).input).standalone = xml_parse_sddecl(ctxt);

    (*ctxt).skip_blanks();
    if (*ctxt).current_byte() == b'?' && (*ctxt).nth_byte(1) == b'>' {
        (*ctxt).advance(2);
    } else if (*ctxt).current_byte() == b'>' {
        /* Deprecated old WD ... */
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotFinished, None);
        (*ctxt).skip_char();
    } else {
        let mut c: i32;

        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotFinished, None);
        while {
            c = (*ctxt).current_byte() as _;
            c != 0
        } {
            (*ctxt).skip_char();
            if c == b'>' as i32 {
                break;
            }
        }
    }
}

/// Parse an XML declaration header for external entities
///
/// `[77] TextDecl ::= '<?xml' VersionInfo? EncodingDecl S? '?>'`
#[doc(alias = "xmlParseTextDecl")]
pub(crate) unsafe extern "C" fn xml_parse_text_decl(ctxt: XmlParserCtxtPtr) {
    /*
     * We know that '<?xml' is here.
     */
    if (*ctxt).content_bytes().starts_with(b"<?xml")
        && xml_is_blank_char((*ctxt).nth_byte(5) as u32)
    {
        (*ctxt).advance(5);
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotStarted, None);
        return;
    }

    /* Avoid expansion of parameter entities when skipping blanks. */
    let oldstate: i32 = (*ctxt).instate as _;
    (*ctxt).instate = XmlParserInputState::XmlParserStart;

    if (*ctxt).skip_blanks() == 0 {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            "Space needed after '<?xml'\n",
        );
    }

    /*
     * We may have the VersionInfo here.
     */
    let mut version = xml_parse_version_info(ctxt);
    if version.is_none() {
        version = Some(XML_DEFAULT_VERSION.to_owned());
    } else if (*ctxt).skip_blanks() == 0 {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            "Space needed here\n",
        );
    }
    (*(*ctxt).input).version = Some(version.unwrap());

    /*
     * We must have the encoding declaration
     */
    let encoding = xml_parse_encoding_decl(ctxt);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }
    if (*ctxt).err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
        /*
         * The XML REC instructs us to stop parsing right here
         */
        (*ctxt).instate = oldstate.try_into().unwrap();
        return;
    }
    if encoding.is_none() && (*ctxt).err_no == XmlParserErrors::XmlErrOK as i32 {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrMissingEncoding,
            "Missing encoding in text declaration\n",
        );
    }

    (*ctxt).skip_blanks();
    if (*ctxt).current_byte() == b'?' && (*ctxt).nth_byte(1) == b'>' {
        (*ctxt).advance(2);
    } else if (*ctxt).current_byte() == b'>' {
        /* Deprecated old WD ... */
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotFinished, None);
        (*ctxt).skip_char();
    } else {
        let mut c: i32;

        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotFinished, None);
        while {
            c = (*ctxt).current_byte() as _;
            c != 0
        } {
            (*ctxt).skip_char();
            if c == b'>' as i32 {
                break;
            }
        }
    }

    (*ctxt).instate = oldstate.try_into().unwrap();
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_byte_consumed() {
        unsafe {
            let mut leaks = 0;
            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_byte_consumed(ctxt);
                desret_long(ret_val);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlByteConsumed",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlByteConsumed()");
        }
    }

    #[test]
    fn test_xml_clear_node_info_seq() {
        unsafe {
            let mut leaks = 0;

            for n_seq in 0..GEN_NB_XML_PARSER_NODE_INFO_SEQ_PTR {
                let mem_base = xml_mem_blocks();
                let seq = gen_xml_parser_node_info_seq_ptr(n_seq, 0);

                xml_clear_node_info_seq(seq);
                des_xml_parser_node_info_seq_ptr(n_seq, seq, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlClearNodeInfoSeq",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_seq);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlClearNodeInfoSeq()"
            );
        }
    }

    #[test]
    fn test_xml_clear_parser_ctxt() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                xml_clear_parser_ctxt(ctxt);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlClearParserCtxt",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlClearParserCtxt()"
            );
        }
    }

    #[test]
    fn test_xml_create_doc_parser_ctxt() {
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let cur = gen_const_xml_char_ptr(n_cur, 0);

                let ret_val = xml_create_doc_parser_ctxt(cur as *const XmlChar);
                desret_xml_parser_ctxt_ptr(ret_val);
                des_const_xml_char_ptr(n_cur, cur, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCreateDocParserCtxt",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_cur);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlCreateDocParserCtxt()"
            );
        }
    }

    #[test]
    fn test_xml_ctxt_reset() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                xml_ctxt_reset(ctxt);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCtxtReset",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlCtxtReset()");
        }
    }

    #[test]
    fn test_xml_ctxt_reset_push() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_chunk in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_size in 0..GEN_NB_INT {
                        for n_filename in 0..GEN_NB_FILEPATH {
                            for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                                let mem_base = xml_mem_blocks();
                                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                                let chunk = gen_const_char_ptr(n_chunk, 1);
                                let mut size = gen_int(n_size, 2);
                                let filename = gen_filepath(n_filename, 3);
                                let encoding = gen_const_char_ptr(n_encoding, 4);
                                if !chunk.is_null() && size > xml_strlen(chunk as _) {
                                    size = 0;
                                }

                                let ret_val =
                                    xml_ctxt_reset_push(ctxt, chunk, size, filename, encoding);
                                desret_int(ret_val);
                                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                                des_const_char_ptr(n_chunk, chunk, 1);
                                des_int(n_size, size, 2);
                                des_filepath(n_filename, filename, 3);
                                des_const_char_ptr(n_encoding, encoding, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlCtxtResetPush",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_ctxt);
                                    eprint!(" {}", n_chunk);
                                    eprint!(" {}", n_size);
                                    eprint!(" {}", n_filename);
                                    eprintln!(" {}", n_encoding);
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlCtxtResetPush()");
        }
    }

    #[test]
    fn test_xml_get_external_entity_loader() {

        /* missing type support */
    }

    #[test]
    fn test_xml_has_feature() {
        unsafe {
            let mut leaks = 0;

            for n_feature in 0..GEN_NB_XML_FEATURE {
                let mem_base = xml_mem_blocks();
                let feature = gen_xml_feature(n_feature, 0);

                let ret_val = xml_has_feature(feature);
                desret_int(ret_val as _);
                des_xml_feature(n_feature, feature, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlHasFeature",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_feature);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlHasFeature()");
        }
    }

    #[test]
    fn test_xml_init_node_info_seq() {
        unsafe {
            let mut leaks = 0;

            for n_seq in 0..GEN_NB_XML_PARSER_NODE_INFO_SEQ_PTR {
                let mem_base = xml_mem_blocks();
                let seq = gen_xml_parser_node_info_seq_ptr(n_seq, 0);

                xml_init_node_info_seq(seq);
                des_xml_parser_node_info_seq_ptr(n_seq, seq, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlInitNodeInfoSeq",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_seq);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlInitNodeInfoSeq()"
            );
        }
    }

    #[test]
    fn test_xml_init_parser() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            xml_init_parser();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlInitParser",
                    xml_mem_blocks() - mem_base
                );
                assert!(leaks == 0, "{leaks} Leaks are found in xmlInitParser()");
            }
        }
    }

    #[test]
    fn test_xml_init_parser_ctxt() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_init_parser_ctxt(ctxt);
                desret_int(ret_val);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlInitParserCtxt",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlInitParserCtxt()");
        }
    }

    #[test]
    fn test_xml_keep_blanks_default() {
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let val = gen_int(n_val, 0);

                let ret_val = xml_keep_blanks_default(val);
                desret_int(ret_val);
                des_int(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlKeepBlanksDefault",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_val);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlKeepBlanksDefault()"
            );
        }
    }

    #[test]
    fn test_xml_line_numbers_default() {
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let val = gen_int(n_val, 0);

                let ret_val = xml_line_numbers_default(val);
                desret_int(ret_val);
                des_int(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlLineNumbersDefault",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_val);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlLineNumbersDefault()"
            );
        }
    }

    #[test]
    fn test_xml_new_parser_ctxt() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            let ret_val = xml_new_parser_ctxt();
            desret_xml_parser_ctxt_ptr(ret_val);
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlNewParserCtxt",
                    xml_mem_blocks() - mem_base
                );
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlNewParserCtxt()");
        }
    }

    #[test]
    fn test_xml_parse_chunk() {
        #[cfg(feature = "libxml_push")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_chunk in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_size in 0..GEN_NB_INT {
                        for n_terminate in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                            let chunk = gen_const_char_ptr(n_chunk, 1);
                            let mut size = gen_int(n_size, 2);
                            let terminate = gen_int(n_terminate, 3);
                            if !chunk.is_null() && size > xml_strlen(chunk as _) {
                                size = 0;
                            }

                            let ret_val = xml_parse_chunk(ctxt, chunk, size, terminate);
                            if !ctxt.is_null() {
                                xml_free_doc((*ctxt).my_doc);
                                (*ctxt).my_doc = null_mut();
                            }
                            desret_int(ret_val);
                            des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_const_char_ptr(n_chunk, chunk, 1);
                            des_int(n_size, size, 2);
                            des_int(n_terminate, terminate, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlParseChunk",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_chunk);
                                eprint!(" {}", n_size);
                                eprintln!(" {}", n_terminate);
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlParseChunk()");
        }
    }

    #[test]
    fn test_xml_parse_doc() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let cur = gen_const_xml_char_ptr(n_cur, 0);

                let ret_val = xml_parse_doc(cur as *const XmlChar);
                desret_xml_doc_ptr(ret_val);
                des_const_xml_char_ptr(n_cur, cur, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlParseDoc",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_cur);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlParseDoc()");
        }
    }

    #[test]
    fn test_xml_parse_document() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_parse_document(ctxt);
                if !ctxt.is_null() {
                    xml_free_doc((*ctxt).my_doc);
                    (*ctxt).my_doc = null_mut();
                }
                desret_int(ret_val);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlParseDocument",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlParseDocument()");
        }
    }

    #[test]
    fn test_xml_parse_ext_parsed_ent() {
        unsafe {
            let mut leaks = 0;
            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_parse_ext_parsed_ent(ctxt);
                if !ctxt.is_null() {
                    xml_free_doc((*ctxt).my_doc);
                    (*ctxt).my_doc = null_mut();
                }
                desret_int(ret_val);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlParseExtParsedEnt",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParseExtParsedEnt()"
            );
        }
    }

    #[test]
    fn test_xml_parser_add_node_info() {
        unsafe {
            let mut leaks = 0;
            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_info in 0..GEN_NB_CONST_XML_PARSER_NODE_INFO_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let info = gen_const_xml_parser_node_info_ptr(n_info, 1);

                    xml_parser_add_node_info(ctxt, info as XmlParserNodeInfoPtr);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_const_xml_parser_node_info_ptr(n_info, info, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlParserAddNodeInfo",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_info);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParserAddNodeInfo()"
            );
        }
    }

    #[test]
    fn test_xml_parser_find_node_info() {
        unsafe {
            let mut leaks = 0;

            for n_ctx in 0..GEN_NB_CONST_XML_PARSER_CTXT_PTR {
                for n_node in 0..GEN_NB_CONST_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctx = gen_const_xml_parser_ctxt_ptr(n_ctx, 0);
                    let node = gen_const_xml_node_ptr(n_node, 1);

                    let ret_val =
                        xml_parser_find_node_info(ctx as XmlParserCtxtPtr, node as XmlNodePtr);
                    desret_const_xml_parser_node_info_ptr(ret_val);
                    des_const_xml_parser_ctxt_ptr(n_ctx, ctx, 0);
                    des_const_xml_node_ptr(n_node, node as XmlNodePtr, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlParserFindNodeInfo",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_ctx);
                        eprintln!(" {}", n_node);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParserFindNodeInfo()"
            );
        }
    }

    #[test]
    fn test_xml_parser_find_node_info_index() {
        unsafe {
            let mut leaks = 0;

            for n_seq in 0..GEN_NB_CONST_XML_PARSER_NODE_INFO_SEQ_PTR {
                for n_node in 0..GEN_NB_CONST_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let seq = gen_const_xml_parser_node_info_seq_ptr(n_seq, 0);
                    let node = gen_const_xml_node_ptr(n_node, 1);

                    let ret_val = xml_parser_find_node_info_index(
                        seq as XmlParserNodeInfoSeqPtr,
                        node as XmlNodePtr,
                    );
                    desret_unsigned_long(ret_val);
                    des_const_xml_parser_node_info_seq_ptr(n_seq, seq, 0);
                    des_const_xml_node_ptr(n_node, node as XmlNodePtr, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlParserFindNodeInfoIndex",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_seq);
                        eprintln!(" {}", n_node);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParserFindNodeInfoIndex()"
            );
        }
    }

    #[test]
    fn test_xml_parser_input_grow() {
        unsafe {
            let mut leaks = 0;
            for n_in in 0..GEN_NB_XML_PARSER_INPUT_PTR {
                for n_len in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let input = gen_xml_parser_input_ptr(n_in, 0);
                    let len = gen_int(n_len, 1);

                    let ret_val = xml_parser_input_grow(input, len);
                    desret_int(ret_val);
                    des_xml_parser_input_ptr(n_in, input, 0);
                    des_int(n_len, len, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlParserInputGrow",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_in);
                        eprintln!(" {}", n_len);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParserInputGrow()"
            );
        }
    }

    #[test]
    fn test_xml_parser_input_read() {
        unsafe {
            let mut leaks = 0;
            for n_in in 0..GEN_NB_XML_PARSER_INPUT_PTR {
                for n_len in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let input = gen_xml_parser_input_ptr(n_in, 0);
                    let len = gen_int(n_len, 1);

                    let ret_val = xml_parser_input_read(input, len);
                    desret_int(ret_val);
                    des_xml_parser_input_ptr(n_in, input, 0);
                    des_int(n_len, len, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlParserInputRead",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_in);
                        eprintln!(" {}", n_len);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParserInputRead()"
            );
        }
    }

    #[test]
    fn test_xml_pedantic_parser_default() {
        unsafe {
            let mut leaks = 0;
            for n_val in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let val = gen_int(n_val, 0);

                let ret_val = xml_pedantic_parser_default(val);
                desret_int(ret_val);
                des_int(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlPedanticParserDefault",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_val);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlPedanticParserDefault()"
            );
        }
    }

    #[test]
    fn test_xml_recover_doc() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;
            for n_cur in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let cur = gen_const_xml_char_ptr(n_cur, 0);

                let ret_val = xml_recover_doc(cur as *const XmlChar);
                desret_xml_doc_ptr(ret_val);
                des_const_xml_char_ptr(n_cur, cur, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlRecoverDoc",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_cur);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlRecoverDoc()");
        }
    }

    #[test]
    fn test_xml_saxparse_doc() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;

            for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                for n_cur in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_recovery in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let sax = gen_xml_saxhandler_ptr(n_sax, 0);
                        let cur = gen_const_xml_char_ptr(n_cur, 1);
                        let recovery = gen_int(n_recovery, 2);

                        let ret_val = xml_sax_parse_doc(sax, cur, recovery);
                        desret_xml_doc_ptr(ret_val);
                        des_xml_saxhandler_ptr(n_sax, sax, 0);
                        des_const_xml_char_ptr(n_cur, cur, 1);
                        des_int(n_recovery, recovery, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSAXParseDoc",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_sax);
                            eprint!(" {}", n_cur);
                            eprintln!(" {}", n_recovery);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlSAXParseDoc()");
        }
    }

    #[test]
    fn test_xml_setup_parser_for_buffer() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_buffer in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_filename in 0..GEN_NB_FILEPATH {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                        let buffer = gen_const_xml_char_ptr(n_buffer, 1);
                        let filename = gen_filepath(n_filename, 2);

                        xml_setup_parser_for_buffer(ctxt, buffer, filename);
                        des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_const_xml_char_ptr(n_buffer, buffer, 1);
                        des_filepath(n_filename, filename, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSetupParserForBuffer",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_buffer);
                            eprintln!(" {}", n_filename);
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlSetupParserForBuffer()"
            );
        }
    }

    #[test]
    fn test_xml_substitute_entities_default() {
        unsafe {
            let mut leaks = 0;
            for n_val in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let val = gen_int(n_val, 0);

                let ret_val = xml_substitute_entities_default(val);
                desret_int(ret_val);
                des_int(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSubstituteEntitiesDefault",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_val);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlSubstituteEntitiesDefault()"
            );
        }
    }
}

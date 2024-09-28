//! Provide methods and data structures for parsing XML documents.  
//! This module is based on `libxml/parser.h`, `parser.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    any::type_name,
    ffi::{c_char, c_int, c_long, c_uchar, c_uint, c_ulong, c_ushort, c_void, CStr},
    mem::{size_of, size_of_val},
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::{AtomicBool, AtomicPtr, Ordering},
};

#[cfg(feature = "legacy")]
use libc::strcmp;
use libc::{memchr, memcpy, memmove, memset, ptrdiff_t, size_t, snprintf, strlen, strncmp, strstr};

use crate::{
    __xml_raise_error,
    buf::libxml_api::xml_buf_create,
    generic_error,
    hash::XmlHashTableRef,
    libxml::{
        catalog::{xml_catalog_cleanup, xml_catalog_free_local},
        dict::{
            __xml_initialize_dict, xml_cleanup_dict_internal, xml_dict_create, xml_dict_free,
            xml_dict_lookup, xml_dict_reference, xml_dict_set_limit, XmlDictPtr,
        },
        encoding::{
            xml_cleanup_char_encoding_handlers, xml_detect_char_encoding, xml_enc_output_chunk,
            xml_find_char_encoding_handler, XmlCharEncoding, XmlCharEncodingHandler,
            XmlCharEncodingHandlerPtr,
        },
        entities::{xml_get_predefined_entity, XmlEntityPtr, XmlEntityType},
        globals::{
            xml_cleanup_globals_internal, xml_default_sax_handler, xml_default_sax_locator,
            xml_do_validity_checking_default_value, xml_free, xml_get_warnings_default_value,
            xml_indent_tree_output, xml_init_globals_internal, xml_keep_blanks_default_value,
            xml_line_numbers_default_value, xml_load_ext_dtd_default_value, xml_malloc,
            xml_malloc_atomic, xml_parser_debug_entities, xml_pedantic_parser_default_value,
            xml_realloc, xml_substitute_entities_default_value,
        },
        hash::{
            xml_hash_default_deallocator, xml_hash_free, xml_hash_lookup2, xml_hash_qlookup2,
            XmlHashTablePtr,
        },
        htmlparser::{__html_parse_content, html_create_memory_parser_ctxt, HtmlParserOption},
        parser_internals::{
            input_pop, input_push, name_pop, node_pop, node_push, xml_add_def_attrs,
            xml_add_special_attr, xml_check_language_id, xml_copy_char,
            xml_create_entity_parser_ctxt_internal, xml_create_file_parser_ctxt,
            xml_create_memory_parser_ctxt, xml_create_url_parser_ctxt,
            xml_ctxt_use_options_internal, xml_err_internal, xml_fatal_err, xml_free_input_stream,
            xml_new_entity_input_stream, xml_new_input_stream, xml_parse_attribute_type,
            xml_parse_comment, xml_parse_content, xml_parse_content_internal,
            xml_parse_default_decl, xml_parse_doc_type_decl, xml_parse_element_content_decl,
            xml_parse_element_end, xml_parse_element_start, xml_parse_encoding_decl,
            xml_parse_entity_ref, xml_parse_entity_value, xml_parse_external_subset,
            xml_parse_misc, xml_parse_name, xml_parse_nmtoken, xml_parse_pe_reference,
            xml_parse_pi, xml_parse_reference, xml_parse_sddecl, xml_parse_start_tag,
            xml_parse_system_literal, xml_parse_version_info, xml_push_input, xml_switch_encoding,
            xml_switch_to_encoding, INPUT_CHUNK, XML_MAX_DICTIONARY_LIMIT, XML_MAX_HUGE_LENGTH,
            XML_MAX_NAMELEN, XML_MAX_NAME_LENGTH, XML_MAX_TEXT_LENGTH, XML_SUBSTITUTE_PEREF,
            XML_SUBSTITUTE_REF,
        },
        relaxng::xml_relaxng_cleanup_types,
        sax2::{
            xml_sax2_entity_decl, xml_sax2_get_entity, xml_sax2_ignorable_whitespace,
            xml_sax_version,
        },
        tree::{
            xml_add_child, xml_buf_use, xml_build_qname, xml_free_doc, xml_free_node,
            xml_free_node_list, xml_get_last_child, xml_new_doc, xml_new_doc_comment,
            xml_new_doc_node, xml_new_dtd, xml_node_is_text, xml_search_ns_by_href,
            xml_set_tree_doc, xml_unlink_node, XmlAttrPtr, XmlAttributeDefault, XmlAttributeType,
            XmlBufferAllocationScheme, XmlDocProperties, XmlDocPtr, XmlDtdPtr,
            XmlElementContentOccur, XmlElementContentPtr, XmlElementContentType, XmlElementType,
            XmlElementTypeVal, XmlEnumerationPtr, XmlNode, XmlNodePtr, XmlNsPtr, XML_XML_NAMESPACE,
        },
        uri::{xml_canonic_path, xml_free_uri, xml_parse_uri, XmlURIPtr},
        valid::{
            xml_free_doc_element_content, xml_free_enumeration, xml_is_mixed_element,
            xml_new_doc_element_content, xml_validate_root, XmlValidCtxt,
        },
        xml_io::{
            xml_alloc_parser_input_buffer, xml_cleanup_input_callbacks,
            xml_cleanup_output_callbacks, xml_default_external_entity_loader,
            xml_free_parser_input_buffer, xml_ioerr_memory, xml_no_net_exists,
            xml_parser_get_directory, xml_parser_input_buffer_create_fd,
            xml_parser_input_buffer_create_io, xml_parser_input_buffer_create_mem,
            xml_parser_input_buffer_grow, xml_parser_input_buffer_push,
            xml_register_default_input_callbacks, xml_register_default_output_callbacks,
            XmlInputCloseCallback, XmlInputReadCallback, XmlParserInputBufferPtr,
        },
        xmlerror::{
            xml_copy_error, xml_parser_validity_error, xml_parser_validity_warning,
            xml_reset_error, XmlError, XmlParserErrors, XmlStructuredErrorFunc,
        },
        xmlmemory::{xml_cleanup_memory_internal, xml_init_memory_internal},
        xmlschemastypes::xml_schema_cleanup_types,
        xmlstring::{
            xml_char_strdup, xml_str_equal, xml_strchr, xml_strdup, xml_strlen, xml_strndup,
            XmlChar,
        },
        xpath::xml_init_xpath_internal,
    },
    private::{
        buf::{
            xml_buf_add, xml_buf_detach, xml_buf_free, xml_buf_get_input_base, xml_buf_is_empty,
            xml_buf_reset_input, xml_buf_set_allocation_scheme, xml_buf_set_input_base_cur,
        },
        enc::{xml_char_enc_input, xml_init_encoding_internal},
        entities::{
            XML_ENT_CHECKED, XML_ENT_CHECKED_LT, XML_ENT_CONTAINS_LT, XML_ENT_EXPANDING,
            XML_ENT_PARSED,
        },
        parser::{
            __xml_err_encoding, xml_err_memory, xml_halt_parser, xml_parser_grow,
            xml_parser_shrink, XML_VCTXT_USE_PCTXT,
        },
        threads::{
            __xml_global_init_mutex_lock, __xml_global_init_mutex_unlock,
            xml_cleanup_threads_internal, xml_init_threads_internal,
        },
    },
    IS_BLANK_CH, IS_BYTE_CHAR, IS_CHAR, IS_COMBINING, IS_DIGIT, IS_EXTENDER, IS_LETTER,
    IS_PUBIDCHAR_CH,
};

/**
 * XML_DEFAULT_VERSION:
 *
 * The default version of XML used: 1.0
 */
pub(crate) const XML_DEFAULT_VERSION: &CStr = c"1.0";

/**
 * xmlParserInput:
 *
 * An xmlParserInput is an input flow for the XML processor.
 * Each entity parsed is associated an xmlParserInput (except the
 * few predefined ones). This is the case both for internal entities
 * - in which case the flow is already completely in memory - or
 *   external entities - in which case we use the buf structure for
 *   progressive reading and I18N conversions to the internal UTF-8 format.
 */

/**
 *   xmlParserInputDeallocate:
 *   @str:  the string to deallocate
 *
 * Callback for freeing some parser input allocations.
 */
pub type XmlParserInputDeallocate = unsafe extern "C" fn(*mut XmlChar) -> c_void;

pub type XmlParserInputPtr = *mut XmlParserInput;
#[repr(C)]
pub struct XmlParserInput {
    /* Input buffer */
    pub buf: XmlParserInputBufferPtr, /* UTF-8 encoded buffer */

    pub filename: *const c_char,         /* The file analyzed, if any */
    pub(crate) directory: *const c_char, /* the directory/base of the file */
    pub base: *const XmlChar,            /* Base of the array to parse */
    pub cur: *const XmlChar,             /* Current c_char being parsed */
    pub end: *const XmlChar,             /* end of the array to parse */
    pub(crate) length: c_int,            /* length if known */
    pub line: c_int,                     /* Current line */
    pub(crate) col: c_int,               /* Current column */
    pub consumed: c_ulong,               /* How many xmlChars already consumed */
    pub(crate) free: Option<XmlParserInputDeallocate>, /* function to deallocate the base */
    pub(crate) encoding: *const XmlChar, /* the encoding string for entity */
    pub(crate) version: *const XmlChar,  /* the version string for entity */
    pub(crate) standalone: c_int,        /* Was that entity marked standalone */
    pub(crate) id: c_int,                /* an unique identifier for the entity */
    pub(crate) parent_consumed: c_ulong, /* consumed bytes from parents */
    pub(crate) entity: XmlEntityPtr,     /* entity, if any */
}

/**
 * xmlParserNodeInfo:
 *
 * The parser can be asked to collect Node information, i.e. at what
 * place in the file they were detected.
 * NOTE: This is off by default and not very well tested.
 */
pub type XmlParserNodeInfoPtr = *mut XmlParserNodeInfo;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct XmlParserNodeInfo {
    pub(crate) node: *const XmlNode,
    /* Position & line # that text that created the node begins & ends on */
    pub(crate) begin_pos: c_ulong,
    pub(crate) begin_line: c_ulong,
    pub(crate) end_pos: c_ulong,
    pub(crate) end_line: c_ulong,
}

pub type XmlParserNodeInfoSeqPtr = *mut XmlParserNodeInfoSeq;
#[repr(C)]
pub struct XmlParserNodeInfoSeq {
    maximum: c_ulong,
    length: c_ulong,
    buffer: *mut XmlParserNodeInfo,
}

/**
 * xmlParserInputState:
 *
 * The parser is now working also as a state based parser.
 * The recursive one use the state info for entities processing.
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlParserInputState {
    XmlParserEOF = -1,       /* nothing is to be parsed */
    XmlParserStart = 0,      /* nothing has been parsed */
    XmlParserMisc,           /* Misc* before c_int subset */
    XmlParserPI,             /* Within a processing instruction */
    XmlParserDTD,            /* within some DTD content */
    XmlParserProlog,         /* Misc* after internal subset */
    XmlParserComment,        /* within a comment */
    XmlParserStartTag,       /* within a start tag */
    XmlParserContent,        /* within the content */
    XmlParserCDATASection,   /* within a CDATA section */
    XmlParserEndTag,         /* within a closing tag */
    XmlParserEntityDecl,     /* within an entity declaration */
    XmlParserEntityValue,    /* within an entity value in a decl */
    XmlParserAttributeValue, /* within an attribute value */
    XmlParserSystemLiteral,  /* within a SYSTEM value */
    XmlParserEpilog,         /* the Misc* after the last end tag */
    XmlParserIgnore,         /* within an IGNORED section */
    XmlParserPublicLiteral,  /* within a PUBLIC value */
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

/**
 * XML_DETECT_IDS:
 *
 * Bit in the loadsubset context field to tell to do ID/REFs lookups.
 * Use it to initialize xmlLoadExtDtdDefaultValue.
 */
pub const XML_DETECT_IDS: usize = 2;

/**
 * XML_COMPLETE_ATTRS:
 *
 * Bit in the loadsubset context field to tell to do complete the
 * elements attributes lists with the ones defaulted from the DTDs.
 * Use it to initialize xmlLoadExtDtdDefaultValue.
 */
pub const XML_COMPLETE_ATTRS: usize = 4;

/**
 * XML_SKIP_IDS:
 *
 * Bit in the loadsubset context field to tell to not do ID/REFs registration.
 * Used to initialize xmlLoadExtDtdDefaultValue in some special cases.
 */
pub const XML_SKIP_IDS: usize = 8;

/**
 * xmlParserMode:
 *
 * A parser can operate in various modes
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlParserMode {
    XmlParseUnknown = 0,
    XmlParseDom = 1,
    XmlParseSax = 2,
    XmlParsePushDom = 3,
    XmlParsePushSax = 4,
    XmlParseReader = 5,
}

#[repr(C)]
pub(crate) struct XmlStartTag {
    pub(crate) prefix: *const XmlChar,
    pub(crate) uri: *const XmlChar,
    pub(crate) line: c_int,
    pub(crate) ns_nr: c_int,
}

/**
 * xmlParserCtxt:
 *
 * The parser context.
 * NOTE This doesn't completely define the parser state, the (current ?)
 *      design of the parser uses recursive function calls since this allow
 *      and easy mapping from the production rules of the specification
 *      to the actual code. The drawback is that the actual function call
 *      also reflect the parser state. However most of the parsing routines
 *      takes as the only argument the parser context pointer, so migrating
 *      to a state based parser for progressive parsing shouldn't be too hard.
 */
pub type XmlParserCtxtPtr = *mut XmlParserCtxt;
#[repr(C)]
pub struct XmlParserCtxt {
    pub sax: *mut XmlSAXHandler,         /* The SAX handler */
    pub(crate) user_data: *mut c_void,   /* For SAX interface only, used by DOM build */
    pub my_doc: XmlDocPtr,               /* the document being built */
    pub well_formed: c_int,              /* is the document well formed */
    pub(crate) replace_entities: c_int,  /* shall we replace entities ? */
    pub(crate) version: *const XmlChar,  /* the XML version string */
    pub(crate) encoding: *const XmlChar, /* the declared encoding, if any */
    pub(crate) standalone: c_int,        /* standalone document */
    pub(crate) html: c_int,              /* an HTML(1) document
                                          * 3 is HTML after <head>
                                          * 10 is HTML after <body>
                                          */

    /* Input stream stack */
    pub input: XmlParserInputPtr,          /* Current input stream */
    pub input_nr: c_int,                   /* Number of current input streams */
    pub(crate) input_max: c_int,           /* Max number of input streams */
    pub input_tab: *mut XmlParserInputPtr, /* stack of inputs */

    /* Node analysis stack only used for DOM building */
    pub(crate) node: XmlNodePtr,          /* Current parsed Node */
    pub(crate) node_nr: c_int,            /* Depth of the parsing stack */
    pub(crate) node_max: c_int,           /* Max depth of the parsing stack */
    pub(crate) node_tab: *mut XmlNodePtr, /* array of nodes */

    pub(crate) record_info: c_int, /* Whether node info should be kept */
    pub(crate) node_seq: XmlParserNodeInfoSeq, /* info about each node parsed */

    pub err_no: c_int, /* error code */

    pub(crate) has_external_subset: c_int, /* reference and external subset */
    pub(crate) has_perefs: c_int,          /* the internal subset has PE refs */
    pub(crate) external: c_int,            /* are we parsing an external entity */

    pub valid: c_int,           /* is the document valid */
    pub(crate) validate: c_int, /* shall we try to validate ? */
    pub vctxt: XmlValidCtxt,    /* The validity context */

    pub instate: XmlParserInputState, /* current type of input */
    pub(crate) token: c_int,          /* next c_char look-ahead */

    pub(crate) directory: *mut c_char, /* the data directory */

    /* Node name stack */
    pub(crate) name: *const XmlChar, /* Current parsed Node */
    pub(crate) name_nr: c_int,       /* Depth of the parsing stack */
    pub(crate) name_max: c_int,      /* Max depth of the parsing stack */
    pub(crate) name_tab: *mut *const XmlChar, /* array of nodes */

    nb_chars: c_long,                        /* unused */
    pub(crate) check_index: c_long,          /* used by progressive parsing lookup */
    pub(crate) keep_blanks: c_int,           /* ugly but ... */
    pub(crate) disable_sax: c_int,           /* SAX callbacks are disabled */
    pub in_subset: c_int,                    /* Parsing is in c_int 1/ext 2 subset */
    pub(crate) int_sub_name: *const XmlChar, /* name of subset */
    pub(crate) ext_sub_uri: *mut XmlChar,    /* URI of external subset */
    pub(crate) ext_sub_system: *mut XmlChar, /* SYSTEM ID of external subset */

    /* xml:space values */
    pub(crate) space: *mut c_int, /* Should the parser preserve spaces */
    pub(crate) space_nr: c_int,   /* Depth of the parsing stack */
    space_max: c_int,             /* Max depth of the parsing stack */
    pub(crate) space_tab: *mut c_int, /* array of space infos */

    pub(crate) depth: c_int, /* to prevent entity substitution loops */
    pub(crate) entity: XmlParserInputPtr, /* used to check entities boundaries */
    pub charset: c_int,      /* encoding of the in-memory content
                             actually an xmlCharEncoding */
    pub(crate) nodelen: c_int,        /* Those two fields are there to */
    pub(crate) nodemem: c_int,        /* Speed up large node parsing */
    pub(crate) pedantic: c_int,       /* signal pedantic warnings */
    pub(crate) _private: *mut c_void, /* For user data, libxml won't touch it */

    pub(crate) loadsubset: c_int, /* should the external subset be loaded */
    pub(crate) linenumbers: c_int, /* set line number in element content */
    pub(crate) catalogs: *mut c_void, /* document's own catalog */
    pub(crate) recovery: c_int,   /* run in recovery mode */
    pub(crate) progressive: c_int, /* is this a progressive parsing */
    pub(crate) dict: XmlDictPtr,  /* dictionary for the parser */
    pub(crate) atts: *mut *const XmlChar, /* array for the attributes callbacks */
    pub(crate) maxatts: c_int,    /* the size of the array */
    pub(crate) docdict: c_int,    /* use strings from dict to build tree */

    /*
     * pre-interned strings
     */
    pub(crate) str_xml: *const XmlChar,
    pub(crate) str_xmlns: *const XmlChar,
    pub(crate) str_xml_ns: *const XmlChar,

    /*
     * Everything below is used only by the new SAX mode
     */
    pub(crate) sax2: c_int,                 /* operating in the new SAX mode */
    pub(crate) ns_nr: c_int,                /* the number of inherited namespaces */
    ns_max: c_int,                          /* the size of the arrays */
    pub(crate) ns_tab: *mut *const XmlChar, /* the array of prefix/namespace name */
    pub(crate) attallocs: *mut c_int,       /* which attribute were allocated */
    pub(crate) push_tab: *mut XmlStartTag,  /* array of data for push */
    pub(crate) atts_default: XmlHashTablePtr, /* defaulted attributes if any */
    pub(crate) atts_special: XmlHashTablePtr, /* non-CDATA attributes if any */
    pub(crate) ns_well_formed: c_int,       /* is the document XML Namespace okay */
    pub(crate) options: c_int,              /* Extra options */

    /*
     * Those fields are needed only for streaming parsing so far
     */
    pub(crate) dict_names: c_int, /* Use dictionary names for the tree */
    pub(crate) free_elems_nr: c_int, /* number of freed element nodes */
    pub(crate) free_elems: XmlNodePtr, /* List of freed element nodes */
    pub(crate) free_attrs_nr: c_int, /* number of freed attributes nodes */
    pub(crate) free_attrs: XmlAttrPtr, /* List of freed attributes nodes */

    /*
     * the complete error information for the last error.
     */
    pub last_error: XmlError,
    pub(crate) parse_mode: XmlParserMode, /* the parser mode */
    pub(crate) nbentities: c_ulong,       /* unused */
    pub sizeentities: c_ulong,            /* size of parsed entities */

    /* for use by HTML non-recursive parser */
    pub(crate) node_info: *mut XmlParserNodeInfo, /* Current NodeInfo */
    pub(crate) node_info_nr: c_int,               /* Depth of the parsing stack */
    pub(crate) node_info_max: c_int,              /* Max depth of the parsing stack */
    pub(crate) node_info_tab: *mut XmlParserNodeInfo, /* array of nodeInfos */

    pub(crate) input_id: c_int, /* we need to label inputs */
    pub sizeentcopy: c_ulong,   /* volume of entity copy */

    pub(crate) end_check_state: c_int, /* quote state for push parser */
    pub nb_errors: c_ushort,           /* number of errors */
    pub(crate) nb_warnings: c_ushort,  /* number of warnings */
}
/**
 * xmlSAXLocator:
 *
 * A SAX Locator.
 */
pub type XmlSaxlocatorPtr = *mut XmlSaxlocator;
#[repr(C)]
pub struct XmlSaxlocator {
    pub(crate) get_public_id: unsafe extern "C" fn(ctx: *mut c_void) -> *const XmlChar,
    pub(crate) get_system_id: unsafe extern "C" fn(ctx: *mut c_void) -> *const XmlChar,
    pub(crate) get_line_number: unsafe extern "C" fn(ctx: *mut c_void) -> c_int,
    pub(crate) get_column_number: unsafe extern "C" fn(ctx: *mut c_void) -> c_int,
}

/**
 * xmlSAXHandler:
 *
 * A SAX handler is bunch of callbacks called by the parser when processing
 * of the input generate data or structure information.
 */

/**
 * resolveEntitySAXFunc:
 * @ctx:  the user data (XML parser context)
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 *
 * Callback:
 * The entity loader, to control the loading of external entities,
 * the application can either:
 *    - override this resolveEntity() callback in the SAX block
 *    - or better use the xmlSetExternalEntityLoader() function to
 *      set up it's own entity resolution routine
 *
 * Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
 */
// typedef xmlParserInputPtr (*resolveEntitySAXFunc) (void *ctx, const XmlChar *publicId, const XmlChar *systemId);
pub type ResolveEntitySAXFunc = unsafe extern "C" fn(
    ctx: *mut c_void,
    publicId: *const XmlChar,
    systemId: *const XmlChar,
) -> XmlParserInputPtr;
/**
 * internalSubsetSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @name:  the root element name
 * @ExternalID:  the external ID
 * @SystemID:  the SYSTEM ID (e.g. filename or URL)
 *
 * Callback on internal subset declaration.
 */
// typedef void (*internalSubsetSAXFunc) (void *ctx, const XmlChar *name, const XmlChar *ExternalID, const XmlChar *SystemID);
pub type InternalSubsetSAXFunc = unsafe extern "C" fn(
    ctx: *mut c_void,
    name: *const XmlChar,
    ExternalID: *const XmlChar,
    SystemID: *const XmlChar,
);

/**
 * externalSubsetSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @name:  the root element name
 * @ExternalID:  the external ID
 * @SystemID:  the SYSTEM ID (e.g. filename or URL)
 *
 * Callback on external subset declaration.
 */
// typedef void (*externalSubsetSAXFunc) (void *ctx, const XmlChar *name, const XmlChar *ExternalID, const XmlChar *SystemID);
pub type ExternalSubsetSAXFunc = unsafe extern "C" fn(
    ctx: *mut c_void,
    name: *const XmlChar,
    ExternalID: *const XmlChar,
    SystemID: *const XmlChar,
);

/**
 * getEntitySAXFunc:
 * @ctx:  the user data (XML parser context)
 * @name: The entity name
 *
 * Get an entity by name.
 *
 * Returns the xmlEntityPtr if found.
 */
// typedef xmlEntityPtr (*getEntitySAXFunc) (void *ctx, const XmlChar *name);
pub type GetEntitySAXFunc =
    unsafe extern "C" fn(ctx: *mut c_void, name: *const XmlChar) -> XmlEntityPtr;

/**
 * getParameterEntitySAXFunc:
 * @ctx:  the user data (XML parser context)
 * @name: The entity name
 *
 * Get a parameter entity by name.
 *
 * Returns the xmlEntityPtr if found.
 */
// typedef xmlEntityPtr (*getParameterEntitySAXFunc) (void *ctx, const XmlChar *name);
pub type GetParameterEntitySAXFunc =
    unsafe extern "C" fn(ctx: *mut c_void, name: *const XmlChar) -> XmlEntityPtr;

/**
 * entityDeclSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @name:  the entity name
 * @type:  the entity type
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 * @content: the entity value (without processing).
 *
 * An entity definition has been parsed.
 */
// typedef void (*entityDeclSAXFunc) (void *ctx, const XmlChar *name, c_int type, const XmlChar *publicId, const XmlChar *systemId, XmlChar *content);
pub type EntityDeclSAXFunc = unsafe extern "C" fn(
    ctx: *mut c_void,
    name: *const XmlChar,
    typ: c_int,
    publicId: *const XmlChar,
    systemId: *const XmlChar,
    content: *mut XmlChar,
);

/**
 * notationDeclSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @name: The name of the notation
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 *
 * What to do when a notation declaration has been parsed.
 */
pub type NotationDeclSAXFunc = unsafe extern "C" fn(
    ctx: *mut c_void,
    name: *const XmlChar,
    publicId: *const XmlChar,
    systemId: *const XmlChar,
);

/**
 * attributeDeclSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @elem:  the name of the element
 * @fullname:  the attribute name
 * @type:  the attribute type
 * @def:  the type of default value
 * @defaultValue: the attribute default value
 * @tree:  the tree of enumerated value set
 *
 * An attribute definition has been parsed.
 */
pub type AttributeDeclSAXFunc = unsafe extern "C" fn(
    ctx: *mut c_void,
    elem: *const XmlChar,
    fullname: *const XmlChar,
    typ: c_int,
    def: c_int,
    defaultValue: *const XmlChar,
    tree: XmlEnumerationPtr,
);

/**
 * elementDeclSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @name:  the element name
 * @type:  the element type
 * @content: the element value tree
 *
 * An element definition has been parsed.
 */
pub type ElementDeclSAXFunc = unsafe extern "C" fn(
    ctx: *mut c_void,
    name: *const XmlChar,
    typ: c_int,
    content: XmlElementContentPtr,
);

/**
 * unparsedEntityDeclSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @name: The name of the entity
 * @publicId: The public ID of the entity
 * @systemId: The system ID of the entity
 * @notationName: the name of the notation
 *
 * What to do when an unparsed entity declaration is parsed.
 */
pub type UnparsedEntityDeclSAXFunc = unsafe extern "C" fn(
    ctx: *mut c_void,
    name: *const XmlChar,
    publicId: *const XmlChar,
    systemId: *const XmlChar,
    notationName: *const XmlChar,
);

/**
 * setDocumentLocatorSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @loc: A SAX Locator
 *
 * Receive the document locator at startup, actually xmlDefaultSAXLocator.
 * Everything is available on the context, so this is useless in our case.
 */
pub type SetDocumentLocatorSAXFunc = unsafe extern "C" fn(ctx: *mut c_void, loc: XmlSaxlocatorPtr);

/**
 * startDocumentSAXFunc:
 * @ctx:  the user data (XML parser context)
 *
 * Called when the document start being processed.
 */
pub type StartDocumentSAXFunc = unsafe extern "C" fn(ctx: *mut c_void);
/**
 * endDocumentSAXFunc:
 * @ctx:  the user data (XML parser context)
 *
 * Called when the document end has been detected.
 */
pub type EndDocumentSAXFunc = unsafe extern "C" fn(ctx: *mut c_void);
/**
 * startElementSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @name:  The element name, including namespace prefix
 * @atts:  An array of name/value attributes pairs, NULL terminated
 *
 * Called when an opening tag has been processed.
 */
pub type StartElementSAXFunc =
    unsafe extern "C" fn(ctx: *mut c_void, name: *const XmlChar, atts: *mut *const XmlChar);

/**
 * endElementSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @name:  The element name
 *
 * Called when the end of an element has been detected.
 */
pub type EndElementSAXFunc = unsafe extern "C" fn(ctx: *mut c_void, name: *const XmlChar);

/**
 * attributeSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @name:  The attribute name, including namespace prefix
 * @value:  The attribute value
 *
 * Handle an attribute that has been read by the parser.
 * The default handling is to convert the attribute into an
 * DOM subtree and past it in a new xmlAttr element added to
 * the element.
 */
pub type AttributeSAXFunc =
    unsafe extern "C" fn(ctx: *mut c_void, name: *const XmlChar, value: *const XmlChar);

/**
 * referenceSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @name:  The entity name
 *
 * Called when an entity reference is detected.
 */
pub type ReferenceSAXFunc = unsafe extern "C" fn(ctx: *mut c_void, name: *const XmlChar);

/**
 * charactersSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @ch:  a XmlChar string
 * @len: the number of XmlChar
 *
 * Receiving some chars from the parser.
 */
pub type CharactersSAXFunc = unsafe extern "C" fn(ctx: *mut c_void, ch: *const XmlChar, len: c_int);

/**
 * ignorableWhitespaceSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @ch:  a XmlChar string
 * @len: the number of XmlChar
 *
 * Receiving some ignorable whitespaces from the parser.
 * UNUSED: by default the DOM building will use characters.
 */
pub type IgnorableWhitespaceSAXFunc =
    unsafe extern "C" fn(ctx: *mut c_void, ch: *const XmlChar, len: c_int);

/**
 * processingInstructionSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @target:  the target name
 * @data: the PI data's
 *
 * A processing instruction has been parsed.
 */
pub type ProcessingInstructionSAXFunc =
    unsafe extern "C" fn(ctx: *mut c_void, target: *const XmlChar, data: *const XmlChar);

/**
 * commentSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @value:  the comment content
 *
 * A comment has been parsed.
 */
pub type CommentSAXFunc = unsafe extern "C" fn(ctx: *mut c_void, value: *const XmlChar);

/**
 * cdataBlockSAXFunc:
 * @ctx:  the user data (XML parser context)
 * @value:  The pcdata content
 * @len:  the block length
 *
 * Called when a pcdata block has been parsed.
 */
pub type CdataBlockSAXFunc =
    unsafe extern "C" fn(ctx: *mut c_void, value: *const XmlChar, len: c_int);

/**
 * warningSAXFunc:
 * @ctx:  an XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format a warning messages, callback.
 */
pub type WarningSAXFunc = unsafe extern "C" fn(ctx: *mut c_void, msg: *const c_char);

/* *
 * errorSAXFunc:
 * @ctx:  an XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format an error messages, callback.
 */
pub type ErrorSAXFunc = unsafe extern "C" fn(ctx: *mut c_void, msg: *const c_char);

/* *
 * fatalErrorSAXFunc:
 * @ctx:  an XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format fatal error messages, callback.
 * Note: so far fatalError() SAX callbacks are not used, error()
 *       get all the callbacks for errors.
 */
pub type FatalErrorSAXFunc = unsafe extern "C" fn(ctx: *mut c_void, msg: *const c_char);

/* *
 * isStandaloneSAXFunc:
 * @ctx:  the user data (XML parser context)
 *
 * Is this document tagged standalone?
 *
 * Returns 1 if true
 */
pub type IsStandaloneSAXFunc = unsafe extern "C" fn(ctx: *mut c_void) -> c_int;
/**
 * hasInternalSubsetSAXFunc:
 * @ctx:  the user data (XML parser context)
 *
 * Does this document has an internal subset.
 *
 * Returns 1 if true
 */
pub type HasInternalSubsetSAXFunc = unsafe extern "C" fn(ctx: *mut c_void) -> c_int;

/**
 * hasExternalSubsetSAXFunc:
 * @ctx:  the user data (XML parser context)
 *
 * Does this document has an external subset?
 *
 * Returns 1 if true
 */
pub type HasExternalSubsetSAXFunc = unsafe extern "C" fn(ctx: *mut c_void) -> c_int;

/************************************************************************
 *									*
 *			The SAX version 2 API extensions		*
 *									*
 ************************************************************************/
/**
 * XML_SAX2_MAGIC:
 *
 * Special constant found in SAX2 blocks initialized fields
 */
pub const XML_SAX2_MAGIC: usize = 0xDEEDBEAF;

/**
 * startElementNsSAX2Func:
 * @ctx:  the user data (XML parser context)
 * @localname:  the local name of the element
 * @prefix:  the element namespace prefix if available
 * @URI:  the element namespace name if available
 * @nb_namespaces:  number of namespace definitions on that node
 * @namespaces:  pointer to the array of prefix/URI pairs namespace definitions
 * @nb_attributes:  the number of attributes on that node
 * @nb_defaulted:  the number of defaulted attributes. The defaulted
 *                  ones are at the end of the array
 * @attributes:  pointer to the array of (localname/prefix/URI/value/end)
 *               attribute values.
 *
 * SAX2 callback when an element start has been detected by the parser.
 * It provides the namespace information for the element, as well as
 * the new namespace declarations on the element.
 */

pub type StartElementNsSAX2Func = unsafe extern "C" fn(
    ctx: *mut c_void,
    localname: *const XmlChar,
    prefix: *const XmlChar,
    URI: *const XmlChar,
    nb_namespaces: c_int,
    namespaces: *mut *const XmlChar,
    nb_attributes: c_int,
    nb_defaulted: c_int,
    attributes: *mut *const XmlChar,
);

/**
 * endElementNsSAX2Func:
 * @ctx:  the user data (XML parser context)
 * @localname:  the local name of the element
 * @prefix:  the element namespace prefix if available
 * @URI:  the element namespace name if available
 *
 * SAX2 callback when an element end has been detected by the parser.
 * It provides the namespace information for the element.
 */

pub type EndElementNsSAX2Func = unsafe extern "C" fn(
    ctx: *mut c_void,
    localname: *const XmlChar,
    prefix: *const XmlChar,
    URI: *const XmlChar,
);

pub type XmlSAXHandlerPtr = *mut XmlSAXHandler;
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
    pub warning: Option<WarningSAXFunc>,
    pub error: Option<ErrorSAXFunc>,
    pub fatal_error: Option<FatalErrorSAXFunc>, /* unused error() get all the errors */
    pub get_parameter_entity: Option<GetParameterEntitySAXFunc>,
    pub cdata_block: Option<CdataBlockSAXFunc>,
    pub external_subset: Option<ExternalSubsetSAXFunc>,
    pub initialized: c_uint,
    /* The following fields are extensions available only on version 2 */
    pub _private: AtomicPtr<c_void>,
    pub start_element_ns: Option<StartElementNsSAX2Func>,
    pub end_element_ns: Option<EndElementNsSAX2Func>,
    pub serror: Option<XmlStructuredErrorFunc>,
}

/*
 * SAX Version 1
 */
// typedef xmlSAXHandlerV1 *xmlSAXHandlerV1Ptr;
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
    pub(crate) warning: Option<WarningSAXFunc>,
    pub(crate) error: Option<ErrorSAXFunc>,
    pub(crate) fatal_error: Option<FatalErrorSAXFunc>, /* unused error() get all the errors */
    pub(crate) get_parameter_entity: Option<GetParameterEntitySAXFunc>,
    pub(crate) cdata_block: Option<CdataBlockSAXFunc>,
    pub(crate) external_subset: Option<ExternalSubsetSAXFunc>,
    pub(crate) initialized: c_uint,
}

/**
 * xmlExternalEntityLoader:
 * @URL: The System ID of the resource requested
 * @ID: The Public ID of the resource requested
 * @context: the XML parser context
 *
 * External entity loaders types.
 *
 * Returns the entity input parser.
 */
// typedef xmlParserInputPtr (*xmlExternalEntityLoader) (const c_char *URL, const c_char *ID, xmlParserCtxtPtr context);
pub type XmlExternalEntityLoader = unsafe extern "C" fn(
    URL: *const c_char,
    ID: *const c_char,
    context: XmlParserCtxtPtr,
) -> XmlParserInputPtr;

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
 *   CUR_CHAR!(ctxt, l) returns the current unicode character (c_int), set l
 *           to the number of xmlChars used for the encoding [0-5].
 *   CUR_SCHAR  same but operate on a string instead of the context
 *   COPY_BUF  copy the current unicode c_char to the target buffer, increment
 *            the index
 *   GROW, SHRINK  handling of input buffers
 */

macro_rules! RAW {
    ($ctxt:expr) => {
        *(*(*$ctxt).input).cur
    };
}
macro_rules! CUR {
    ($ctxt:expr) => {
        *(*(*$ctxt).input).cur
    };
}
macro_rules! NXT {
    ($ctxt:expr, $val:expr) => {
        *(*(*$ctxt).input).cur.add($val as usize)
    };
}
macro_rules! CUR_PTR {
    ($ctxt:expr) => {
        (*(*$ctxt).input).cur
    };
}
macro_rules! BASE_PTR {
    ($ctxt:expr) => {
        (*(*$ctxt).input).base
    };
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
// macro_rules! CMP10 {
//     ( $s:expr, $c1:expr, $c2:expr, $c3:expr, $c4:expr, $c5:expr, $c6:expr, $c7:expr, $c8:expr, $c9:expr, $c10:expr ) => {
//         (CMP9!($s, $c1, $c2, $c3, $c4, $c5, $c6, $c7, $c8, $c9)
//             && *($s as *mut c_uchar).add(9) == $c10)
//     };
// }

macro_rules! SKIP {
    ($ctxt:expr, $val:expr) => {
        (*(*$ctxt).input).cur = (*(*$ctxt).input).cur.add($val as usize);
        (*(*$ctxt).input).col += $val;
        if *(*(*$ctxt).input).cur == 0 {
            crate::private::parser::xml_parser_grow($ctxt);
        }
    };
}

macro_rules! NEXT1 {
    ($ctxt:expr) => {
        (*(*$ctxt).input).col += 1;
        (*(*$ctxt).input).cur = (*(*$ctxt).input).cur.add(1);
        if *(*(*$ctxt).input).cur == 0 {
            crate::private::parser::xml_parser_grow($ctxt);
        }
    };
}

macro_rules! SKIPL {
    ($ctxt:expr, $val:expr) => {
        for _ in 0..$val {
            if *((*(*$ctxt).input).cur) == b'\n' {
                (*(*$ctxt).input).line += 1;
                (*(*$ctxt).input).col = 1;
            } else {
                (*(*$ctxt).input).col += 1;
            }
            (*(*$ctxt).input).cur = (*(*$ctxt).input).cur.add(1);
        }
        if *(*(*$ctxt).input).cur == 0 {
            crate::private::parser::xml_parser_grow($ctxt);
        }
    };
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

macro_rules! GROW {
    ($ctxt:expr) => {
        if (*$ctxt).progressive == 0
            && ((*(*$ctxt).input).end.offset_from((*(*$ctxt).input).cur) as usize) < INPUT_CHUNK
        {
            crate::private::parser::xml_parser_grow($ctxt);
        }
    };
}

macro_rules! SKIP_BLANKS {
    ($ctxt:expr) => {
        xml_skip_blank_chars($ctxt)
    };
}

macro_rules! NEXT {
    ($ctxt:expr) => {
        crate::libxml::parser_internals::xml_next_char($ctxt)
    };
}

macro_rules! NEXTL {
    ($ctxt:expr, $l:expr) => {
        if (*((*(*$ctxt).input).cur) == b'\n') {
            (*(*$ctxt).input).line += 1;
            (*(*$ctxt).input).col = 1;
        } else {
            (*(*$ctxt).input).col += 1;
        }
        (*(*$ctxt).input).cur = (*(*$ctxt).input).cur.add($l as usize);
    };
}

macro_rules! CUR_CHAR {
    ($ctxt:expr, $l:expr) => {
        crate::libxml::parser_internals::xml_current_char($ctxt, addr_of_mut!($l))
    };
}
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

/**
 * xmlFatalErrMsg:
 * @ctxt:  an XML parser context
 * @error:  the error number
 * @msg:  the error message
 *
 * Handle a fatal parser error, i.e. violating Well-Formedness constraints
 */
pub(crate) unsafe extern "C" fn xml_fatal_err_msg(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
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
        0,
        0,
        c"%s".as_ptr() as _,
        msg
    );
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
        if (*ctxt).recovery == 0 {
            (*ctxt).disable_sax = 1;
        }
    }
}

/**
 * xmlFatalErrMsgStr:
 * @ctxt:  an XML parser context
 * @error:  the error number
 * @msg:  the error message
 * @val:  a string value
 *
 * Handle a fatal parser error, i.e. violating Well-Formedness constraints
 */
pub(crate) unsafe extern "C" fn xml_fatal_err_msg_str(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    val: *const XmlChar,
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
        val as _,
        null_mut(),
        null_mut(),
        0,
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
 * xmlFatalErrMsgInt:
 * @ctxt:  an XML parser context
 * @error:  the error number
 * @msg:  the error message
 * @val:  an integer value
 *
 * Handle a fatal parser error, i.e. violating Well-Formedness constraints
 */
pub(crate) unsafe extern "C" fn xml_fatal_err_msg_int(
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
 * xmlWarningMsg:
 * @ctxt:  an XML parser context
 * @error:  the error number
 * @msg:  the error message
 * @str1:  extra data
 * @str2:  extra data
 *
 * Handle a warning.
 */
pub(crate) unsafe extern "C" fn xml_warning_msg(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
    str2: *const XmlChar,
) {
    let mut schannel: Option<XmlStructuredErrorFunc> = None;

    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null()
        && !(*ctxt).sax.is_null()
        && (*(*ctxt).sax).initialized == XML_SAX2_MAGIC as u32
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
            (*ctxt).user_data,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser as i32,
            error as i32,
            XmlErrorLevel::XmlErrWarning,
            null_mut(),
            0,
            str1 as _,
            str2 as _,
            null_mut(),
            0,
            0,
            msg,
            str1,
            str2
        );
    } else {
        __xml_raise_error!(
            schannel,
            None,
            null_mut(),
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser as i32,
            error as i32,
            XmlErrorLevel::XmlErrWarning,
            null_mut(),
            0,
            str1 as _,
            str2 as _,
            null_mut(),
            0,
            0,
            msg,
            str1,
            str2
        );
    }
}

/**
 * xmlErrMsgStr:
 * @ctxt:  an XML parser context
 * @error:  the error number
 * @msg:  the error message
 * @val:  a string value
 *
 * Handle a non fatal parser error
 */
pub(crate) unsafe extern "C" fn xml_err_msg_str(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    val: *const XmlChar,
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
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        val as _,
        null_mut(),
        null_mut(),
        0,
        0,
        msg,
        val
    );
}

/**
 * xmlValidityError:
 * @ctxt:  an XML parser context
 * @error:  the error number
 * @msg:  the error message
 * @str1:  extra data
 *
 * Handle a validity error.
 */
pub(crate) unsafe extern "C" fn xml_validity_error(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
    str2: *const XmlChar,
) {
    let mut schannel: Option<XmlStructuredErrorFunc> = None;

    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = error as i32;
        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).initialized == XML_SAX2_MAGIC as u32 {
            schannel = (*(*ctxt).sax).serror;
        }
    }
    if !ctxt.is_null() {
        __xml_raise_error!(
            schannel,
            (*ctxt).vctxt.error,
            (*ctxt).vctxt.user_data,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromDtd as i32,
            error as i32,
            XmlErrorLevel::XmlErrError,
            null_mut(),
            0,
            str1 as _,
            str2 as _,
            null_mut(),
            0,
            0,
            msg,
            str1,
            str2
        );
        (*ctxt).valid = 0;
    } else {
        __xml_raise_error!(
            schannel,
            None,
            null_mut(),
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromDtd as i32,
            error as i32,
            XmlErrorLevel::XmlErrError,
            null_mut(),
            0,
            str1 as _,
            str2 as _,
            null_mut(),
            0,
            0,
            msg,
            str1,
            str2
        );
    }
}

/**
 * xmlFatalErrMsgStrIntStr:
 * @ctxt:  an XML parser context
 * @error:  the error number
 * @msg:  the error message
 * @str1:  an string info
 * @val:  an integer value
 * @str2:  an string info
 *
 * Handle a fatal parser error, i.e. violating Well-Formedness constraints
 */
pub(crate) unsafe extern "C" fn xml_fatal_err_msg_str_int_str(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
    val: c_int,
    str2: *const XmlChar,
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
        str1 as _,
        str2 as _,
        null_mut(),
        val,
        0,
        msg,
        str1,
        val,
        str2
    );
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
        if (*ctxt).recovery == 0 {
            (*ctxt).disable_sax = 1;
        }
    }
}

/**
 * xmlNsErr:
 * @ctxt:  an XML parser context
 * @error:  the error number
 * @msg:  the message
 * @info1:  extra information string
 * @info2:  extra information string
 *
 * Handle a fatal parser error, i.e. violating Well-Formedness constraints
 */
pub(crate) unsafe extern "C" fn xml_ns_err(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    info1: *const XmlChar,
    info2: *const XmlChar,
    info3: *const XmlChar,
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
        XmlErrorDomain::XmlFromNamespace as i32,
        error as i32,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        info1 as _,
        info2 as _,
        info3 as _,
        0,
        0,
        msg,
        info1,
        info2,
        info3
    );
    if !ctxt.is_null() {
        (*ctxt).ns_well_formed = 0;
    }
}

pub(crate) unsafe extern "C" fn xml_is_name_char(ctxt: XmlParserCtxtPtr, c: c_int) -> c_int {
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
    } else if IS_LETTER!(c as u32)
        || IS_DIGIT!(c as u32)
        || c == b'.' as i32
        || c == b'-' as i32
        || c == b'_' as i32
        || c == b':' as i32
        || IS_COMBINING!(c as u32)
        || IS_EXTENDER!(c as u32)
    {
        return 1;
    }
    0
}

/*
 * Init/Cleanup
 */
static XML_PARSER_INITIALIZED: AtomicBool = AtomicBool::new(false);

/**
 * xmlInitParser:
 *
 * Initialization function for the XML parser.
 * This is not reentrant. Call once before processing in case of
 * use in multithreaded programs.
 */
pub unsafe extern "C" fn xml_init_parser() {
    /*
     * Note that the initialization code must not make memory allocations.
     */
    if XML_PARSER_INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    #[cfg(feature = "thread")]
    {
        __xml_global_init_mutex_lock();
    }
    if !cfg!(feature = "thread") || !XML_PARSER_INITIALIZED.load(Ordering::Acquire) {
        xml_init_threads_internal();
        xml_init_globals_internal();
        xml_init_memory_internal();
        __xml_initialize_dict();
        xml_init_encoding_internal();
        xml_register_default_input_callbacks();
        #[cfg(feature = "output")]
        {
            xml_register_default_output_callbacks();
        }
        #[cfg(any(feature = "xpath", feature = "schema"))]
        {
            xml_init_xpath_internal();
        }
        XML_PARSER_INITIALIZED.store(true, Ordering::Release);
    }

    #[cfg(feature = "thread")]
    {
        __xml_global_init_mutex_unlock();
    }
}

/**
 * xmlCleanupParser:
 *
 * This function name is somewhat misleading. It does not clean up
 * parser state, it cleans up memory allocated by the library itself.
 * It is a cleanup function for the XML library. It tries to reclaim all
 * related global memory allocated for the library processing.
 * It doesn't deallocate any document related memory. One should
 * call xmlCleanupParser() only when the process has finished using
 * the library and all XML/HTML documents built with it.
 * See also xmlInitParser() which has the opposite function of preparing
 * the library for operations.
 *
 * WARNING: if your application is multithreaded or has plugin support
 *          calling this may crash the application if another thread or
 *          a plugin is still using libxml2. It's sometimes very hard to
 *          guess if libxml2 is in use in the application, some libraries
 *          or plugins may use it without notice. In case of doubt abstain
 *          from calling this function or do it just before calling exit()
 *          to avoid leak reports from valgrind !
 */
pub unsafe extern "C" fn xml_cleanup_parser() {
    if !XML_PARSER_INITIALIZED.load(Ordering::Acquire) {
        return;
    }

    xml_cleanup_char_encoding_handlers();
    #[cfg(feature = "catalog")]
    {
        xml_catalog_cleanup();
    }
    xml_cleanup_dict_internal();
    xml_cleanup_input_callbacks();
    #[cfg(feature = "output")]
    {
        xml_cleanup_output_callbacks();
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

/*
 * Input functions
 */
/**
 * xmlParserInputRead:
 * @in:  an XML parser input
 * @len:  an indicative size for the lookahead
 *
 * DEPRECATED: This function was internal and is deprecated.
 *
 * Returns -1 as this is an error to use it.
 */
pub(crate) unsafe extern "C" fn xml_parser_input_read(
    _input: XmlParserInputPtr,
    _len: c_int,
) -> c_int {
    -1
}

/**
 * xmlParserInputGrow:
 * @in:  an XML parser input
 * @len:  an indicative size for the lookahead
 *
 * DEPRECATED: Don't use.
 *
 * This function increase the input for the parser. It tries to
 * preserve pointers to the input buffer, and keep already read data
 *
 * Returns the amount of c_char read, or -1 in case of error, 0 indicate the
 * end of this entity
 */
pub(crate) unsafe extern "C" fn xml_parser_input_grow(
    input: XmlParserInputPtr,
    len: c_int,
) -> c_int {
    if input.is_null() || len < 0 {
        return -1;
    }
    if (*input).buf.is_null() {
        return -1;
    }
    if (*input).base.is_null() {
        return -1;
    }
    if (*input).cur.is_null() {
        return -1;
    }
    let Some(buf) = (*(*input).buf).buffer else {
        return -1;
    };

    /* Don't grow memory buffers. */
    if (*(*input).buf).encoder.is_null() && (*(*input).buf).readcallback.is_none() {
        return 0;
    }

    let indx: size_t = (*input).cur.offset_from((*input).base) as _;
    if buf.len() > indx + INPUT_CHUNK {
        return 0;
    }
    let ret: c_int = xml_parser_input_buffer_grow((*input).buf, len);

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

/*
 * Basic parsing Interfaces
 */
/**
 * xmlParseDoc:
 * @cur:  a pointer to an array of XmlChar
 *
 * DEPRECATED: Use xmlReadDoc.
 *
 * parse an XML in-memory document and build a tree.
 *
 * Returns the resulting document tree
 */
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_parse_doc(cur: *const XmlChar) -> XmlDocPtr {
    xml_sax_parse_doc(null_mut(), cur, 0)
}

/**
 * xmlParseFile:
 * @filename:  the filename
 *
 * DEPRECATED: Use xmlReadFile.
 *
 * parse an XML file and build a tree. Automatic support for ZLIB/Compress
 * compressed document is provided by default if found at compile-time.
 *
 * Returns the resulting document tree if the file was wellformed,
 * NULL otherwise.
 */
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_parse_file(filename: *const c_char) -> XmlDocPtr {
    xml_sax_parse_file(null_mut(), filename, 0)
}

/**
 * xmlParseMemory:
 * @buffer:  an pointer to a c_char array
 * @size:  the size of the array
 *
 * DEPRECATED: Use xmlReadMemory.
 *
 * parse an XML in-memory block and build a tree.
 *
 * Returns the resulting document tree
 */
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_parse_memory(buffer: *const c_char, size: c_int) -> XmlDocPtr {
    xml_sax_parse_memory(null_mut(), buffer, size, 0)
}

/**
 * xmlSubstituteEntitiesDefault:
 * @val:  c_int 0 or 1
 *
 * DEPRECATED: Use the modern options API with XML_PARSE_NOENT.
 *
 * Set and return the previous value for default entity support.
 * Initially the parser always keep entity references instead of substituting
 * entity values in the output. This function has to be used to change the
 * default parser behavior
 * SAX::substituteEntities() has to be used for changing that on a file by
 * file basis.
 *
 * Returns the last value for 0 for no substitution, 1 for substitution.
 */
pub unsafe extern "C" fn xml_substitute_entities_default(val: c_int) -> c_int {
    let old: c_int = *xml_substitute_entities_default_value();

    *xml_substitute_entities_default_value() = val;
    old
}

/**
 * xmlKeepBlanksDefault:
 * @val:  c_int 0 or 1
 *
 * DEPRECATED: Use the modern options API with XML_PARSE_NOBLANKS.
 *
 * Set and return the previous value for default blanks text nodes support.
 * The 1.x version of the parser used an heuristic to try to detect
 * ignorable white spaces. As a result the SAX callback was generating
 * xmlSAX2IgnorableWhitespace() callbacks instead of characters() one, and when
 * using the DOM output text nodes containing those blanks were not generated.
 * The 2.x and later version will switch to the XML standard way and
 * ignorableWhitespace() are only generated when running the parser in
 * validating mode and when the current element doesn't allow CDATA or
 * mixed content.
 * This function is provided as a way to force the standard behavior
 * on 1.X libs and to switch back to the old mode for compatibility when
 * running 1.X client code on 2.X . Upgrade of 1.X code should be done
 * by using xmlIsBlankNode() commodity function to detect the "empty"
 * nodes generated.
 * This value also affect autogeneration of indentation when saving code
 * if blanks sections are kept, indentation is not generated.
 *
 * Returns the last value for 0 for no substitution, 1 for substitution.
 */
pub unsafe extern "C" fn xml_keep_blanks_default(val: c_int) -> c_int {
    let old: c_int = *xml_keep_blanks_default_value();

    *xml_keep_blanks_default_value() = val;
    if val == 0 {
        *xml_indent_tree_output() = 1;
    }
    old
}

/**
 * xmlStopParser:
 * @ctxt:  an XML parser context
 *
 * Blocks further parser processing
 */
pub unsafe extern "C" fn xml_stop_parser(ctxt: XmlParserCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    xml_halt_parser(ctxt);

    (*ctxt).err_no = XmlParserErrors::XmlErrUserStop as i32;
}

/**
 * xmlPedanticParserDefault:
 * @val:  c_int 0 or 1
 *
 * DEPRECATED: Use the modern options API with XML_PARSE_PEDANTIC.
 *
 * Set and return the previous value for enabling pedantic warnings.
 *
 * Returns the last value for 0 for no substitution, 1 for substitution.
 */
pub unsafe extern "C" fn xml_pedantic_parser_default(val: c_int) -> c_int {
    let old: c_int = *xml_pedantic_parser_default_value();

    *xml_pedantic_parser_default_value() = val;
    old
}

/**
 * xmlLineNumbersDefault:
 * @val:  c_int 0 or 1
 *
 * DEPRECATED: The modern options API always enables line numbers.
 *
 * Set and return the previous value for enabling line numbers in elements
 * contents. This may break on old application and is turned off by default.
 *
 * Returns the last value for 0 for no substitution, 1 for substitution.
 */
pub unsafe extern "C" fn xml_line_numbers_default(val: c_int) -> c_int {
    let old: c_int = *xml_line_numbers_default_value();

    *xml_line_numbers_default_value() = val;
    old
}

/*
 * Recovery mode
 */
/**
 * xmlRecoverDoc:
 * @cur:  a pointer to an array of XmlChar
 *
 * DEPRECATED: Use xmlReadDoc with XML_PARSE_RECOVER.
 *
 * parse an XML in-memory document and build a tree.
 * In the case the document is not Well Formed, a attempt to build a
 * tree is tried anyway
 *
 * Returns the resulting document tree or NULL in case of failure
 */
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_recover_doc(cur: *const XmlChar) -> XmlDocPtr {
    xml_sax_parse_doc(null_mut(), cur, 1)
}

/**
 * xmlRecoverMemory:
 * @buffer:  an pointer to a c_char array
 * @size:  the size of the array
 *
 * DEPRECATED: Use xmlReadMemory with XML_PARSE_RECOVER.
 *
 * parse an XML in-memory block and build a tree.
 * In the case the document is not Well Formed, an attempt to
 * build a tree is tried anyway
 *
 * Returns the resulting document tree or NULL in case of error
 */
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_recover_memory(buffer: *const c_char, size: c_int) -> XmlDocPtr {
    xml_sax_parse_memory(null_mut(), buffer, size, 1)
}

/**
 * xmlRecoverFile:
 * @filename:  the filename
 *
 * DEPRECATED: Use xmlReadFile with XML_PARSE_RECOVER.
 *
 * parse an XML file and build a tree. Automatic support for ZLIB/Compress
 * compressed document is provided by default if found at compile-time.
 * In the case the document is not Well Formed, it attempts to build
 * a tree anyway
 *
 * Returns the resulting document tree or NULL in case of failure
 */
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_recover_file(filename: *const c_char) -> XmlDocPtr {
    xml_sax_parse_file(null_mut(), filename, 1)
}

/**
 * xmlDetectSAX2:
 * @ctxt:  an XML parser context
 *
 * Do the SAX2 detection and specific initialization
 */
pub(crate) unsafe extern "C" fn xml_detect_sax2(ctxt: XmlParserCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    let sax: XmlSAXHandlerPtr = (*ctxt).sax;
    #[cfg(feature = "sax1")]
    {
        if !sax.is_null()
            && (*sax).initialized == XML_SAX2_MAGIC as u32
            && ((*sax).start_element_ns.is_some()
                || (*sax).end_element_ns.is_some()
                || ((*sax).start_element.is_none() && (*sax).end_element.is_none()))
        {
            (*ctxt).sax2 = 1;
        }
    }
    #[cfg(not(feature = "sax1"))]
    {
        (*ctxt).sax2 = 1;
    }

    (*ctxt).str_xml = xml_dict_lookup((*ctxt).dict, c"xml".as_ptr() as _, 3);
    (*ctxt).str_xmlns = xml_dict_lookup((*ctxt).dict, c"xmlns".as_ptr() as _, 5);
    (*ctxt).str_xml_ns = xml_dict_lookup((*ctxt).dict, XML_XML_NAMESPACE.as_ptr() as _, 36);
    if (*ctxt).str_xml.is_null() || (*ctxt).str_xmlns.is_null() || (*ctxt).str_xml_ns.is_null() {
        xml_err_memory(ctxt, null());
    }
}

pub type XmlDefAttrsPtr = *mut XmlDefAttrs;
#[repr(C)]
pub struct XmlDefAttrs {
    pub(crate) nb_attrs: c_int, /* number of defaulted attributes on that element */
    pub(crate) max_attrs: c_int, /* the size of the array */
    pub(crate) values: [*const XmlChar; 5], /* array of localname/prefix/values/external */
}

/**
 * xmlParseConditionalSections
 * @ctxt:  an XML parser context
 *
 * Parse a conditional section. Always consumes '<!['.
 *
 * [61] conditionalSect ::= includeSect | ignoreSect
 * [62] includeSect ::= '<![' S? 'INCLUDE' S? '[' extSubsetDecl ']]>'
 * [63] ignoreSect ::= '<![' S? 'IGNORE' S? '[' ignoreSectContents* ']]>'
 * [64] ignoreSectContents ::= Ignore ('<![' ignoreSectContents ']]>' Ignore)*
 * [65] Ignore ::= Char* - (Char* ('<![' | ']]>') Char*)
 */
pub(crate) unsafe extern "C" fn xml_parse_conditional_sections(ctxt: XmlParserCtxtPtr) {
    let mut input_ids: *mut c_int = null_mut();
    let mut input_ids_size: size_t = 0;
    let mut depth: size_t = 0;

    while !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        if RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'!' && NXT!(ctxt, 2) == b'[' {
            let id: c_int = (*(*ctxt).input).id;

            SKIP!(ctxt, 3);
            SKIP_BLANKS!(ctxt);

            if CMP7!(CUR_PTR!(ctxt), b'I', b'N', b'C', b'L', b'U', b'D', b'E') {
                SKIP!(ctxt, 7);
                SKIP_BLANKS!(ctxt);
                if RAW!(ctxt) != b'[' {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrCondsecInvalid, null());
                    xml_halt_parser(ctxt);
                    //  goto error;
                    xml_free(input_ids as _);
                    return;
                }
                if (*(*ctxt).input).id != id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        c"All markup of the conditional section is not in the same entity\n"
                            .as_ptr() as _,
                    );
                }
                NEXT!(ctxt);

                if input_ids_size <= depth {
                    input_ids_size = if input_ids_size == 0 {
                        4
                    } else {
                        input_ids_size * 2
                    };
                    let tmp: *mut c_int =
                        xml_realloc(input_ids as _, input_ids_size as usize * size_of::<c_int>())
                            as _;
                    if tmp.is_null() {
                        xml_err_memory(ctxt, null());
                        //  goto error;
                        xml_free(input_ids as _);
                        return;
                    }
                    input_ids = tmp;
                }
                *input_ids.add(depth as usize) = id;
                depth += 1;
            } else if CMP6!(CUR_PTR!(ctxt), b'I', b'G', b'N', b'O', b'R', b'E') {
                let mut ignore_depth: size_t = 0;

                SKIP!(ctxt, 6);
                SKIP_BLANKS!(ctxt);
                if RAW!(ctxt) != b'[' {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrCondsecInvalid, null());
                    xml_halt_parser(ctxt);
                    //  goto error;
                    xml_free(input_ids as _);
                    return;
                }
                if (*(*ctxt).input).id != id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        c"All markup of the conditional section is not in the same entity\n"
                            .as_ptr() as _,
                    );
                }
                NEXT!(ctxt);

                while RAW!(ctxt) != 0 {
                    if RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'!' && NXT!(ctxt, 2) == b'[' {
                        SKIP!(ctxt, 3);
                        ignore_depth += 1;
                        /* Check for integer overflow */
                        if ignore_depth == 0 {
                            xml_err_memory(ctxt, null());
                            //  goto error;
                            xml_free(input_ids as _);
                            return;
                        }
                    } else if RAW!(ctxt) == b']' && NXT!(ctxt, 1) == b']' && NXT!(ctxt, 2) == b'>' {
                        if ignore_depth == 0 {
                            break;
                        }
                        SKIP!(ctxt, 3);
                        ignore_depth -= 1;
                    } else {
                        NEXT!(ctxt);
                    }
                }

                if RAW!(ctxt) == 0 {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrCondsecNotFinished, null());
                    //  goto error;
                    xml_free(input_ids as _);
                    return;
                }
                if (*(*ctxt).input).id != id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        c"All markup of the conditional section is not in the same entity\n"
                            .as_ptr() as _,
                    );
                }
                SKIP!(ctxt, 3);
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrCondsecInvalidKeyword, null());
                xml_halt_parser(ctxt);
                //  goto error;
                xml_free(input_ids as _);
                return;
            }
        } else if depth > 0 && RAW!(ctxt) == b']' && NXT!(ctxt, 1) == b']' && NXT!(ctxt, 2) == b'>'
        {
            depth -= 1;
            if (*(*ctxt).input).id != *input_ids.add(depth as usize) {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    c"All markup of the conditional section is not in the same entity\n".as_ptr()
                        as _,
                );
            }
            SKIP!(ctxt, 3);
        } else if RAW!(ctxt) == b'<' && (NXT!(ctxt, 1) == b'!' || NXT!(ctxt, 1) == b'?') {
            xml_parse_markup_decl(ctxt);
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtSubsetNotFinished, null());
            xml_halt_parser(ctxt);
            //  goto error;
            xml_free(input_ids as _);
            return;
        }

        if depth == 0 {
            break;
        }

        SKIP_BLANKS!(ctxt);
        SHRINK!(ctxt);
        GROW!(ctxt);
    }

    //  error:
    xml_free(input_ids as _);
}

/**
 * xmlParseInternalSubset:
 * @ctxt:  an XML parser context
 *
 * parse the internal subset declaration
 *
 * [28 end] ('[' (markupdecl | PEReference | S)* ']' S?)? '>'
 */
unsafe extern "C" fn xml_parse_internal_subset(ctxt: XmlParserCtxtPtr) {
    /*
     * Is there any DTD definition ?
     */
    if RAW!(ctxt) == b'[' {
        let base_input_nr: c_int = (*ctxt).input_nr;
        (*ctxt).instate = XmlParserInputState::XmlParserDTD;
        NEXT!(ctxt);
        /*
         * Parse the succession of Markup declarations and
         * PEReferences.
         * Subsequence (markupdecl | PEReference | S)*
         */
        SKIP_BLANKS!(ctxt);
        #[allow(clippy::while_immutable_condition)]
        while (RAW!(ctxt) != b']' || (*ctxt).input_nr > base_input_nr)
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            /*
             * Conditional sections are allowed from external entities included
             * by PE References in the internal subset.
             */
            if (*ctxt).input_nr > 1
                && !(*(*ctxt).input).filename.is_null()
                && RAW!(ctxt) == b'<'
                && NXT!(ctxt, 1) == b'!'
                && NXT!(ctxt, 2) == b'['
            {
                xml_parse_conditional_sections(ctxt);
            } else if RAW!(ctxt) == b'<' && (NXT!(ctxt, 1) == b'!' || NXT!(ctxt, 1) == b'?') {
                xml_parse_markup_decl(ctxt);
            } else if RAW!(ctxt) == b'%' {
                xml_parse_pe_reference(ctxt);
            } else {
                xml_fatal_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    c"xmlParseInternalSubset: error detected in Markup declaration\n".as_ptr() as _,
                );
                xml_halt_parser(ctxt);
                return;
            }
            SKIP_BLANKS!(ctxt);
            SHRINK!(ctxt);
            GROW!(ctxt);
        }
        if RAW!(ctxt) == b']' {
            NEXT!(ctxt);
            SKIP_BLANKS!(ctxt);
        }
    }

    /*
     * We should be at the end of the DOCTYPE declaration.
     */
    if RAW!(ctxt) != b'>' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDoctypeNotFinished, null());
        return;
    }
    NEXT!(ctxt);
}

/**
 * xmlCleanSpecialAttrCallback:
 *
 * Removes CDATA attributes from the special attribute table
 */
// This is used the callback for xml_hash_scan_full on xml_clean_special_attr,
// but no longer needed.
// unsafe extern "C" fn xml_clean_special_attr_callback(
//     payload: *mut c_void,
//     data: *mut c_void,
//     fullname: *const XmlChar,
//     fullattr: *const XmlChar,
//     _unused: *const XmlChar,
// ) {
//     let ctxt: XmlParserCtxtPtr = data as XmlParserCtxtPtr;

//     if payload as isize == XmlAttributeType::XmlAttributeCdata as isize {
//         xml_hash_remove_entry2((*ctxt).atts_special, fullname, fullattr, None);
//     }
// }

/**
 * xmlCleanSpecialAttr:
 * @ctxt:  an XML parser context
 *
 * Trim the list of attributes defined to remove all those of type
 * CDATA as they are not special. This call should be done when finishing
 * to parse the DTD and before starting to parse the document root.
 */
unsafe extern "C" fn xml_clean_special_attr(ctxt: XmlParserCtxtPtr) {
    let Some(mut atts) = XmlHashTableRef::from_raw((*ctxt).atts_special) else {
        return;
    };

    atts.remove_if(
        |data, _, _, _| data.0 as isize == XmlAttributeType::XmlAttributeCdata as isize,
        |_, _| {},
    );
    if atts.is_empty() {
        atts.free();
        (*ctxt).atts_special = null_mut();
    }

    // if (*ctxt).atts_special.is_null() {
    //     return;
    // }

    // xml_hash_scan_full(
    //     (*ctxt).atts_special,
    //     Some(xml_clean_special_attr_callback),
    //     ctxt as _,
    // );

    // if xml_hash_size((*ctxt).atts_special) == 0 {
    //     xml_hash_free((*ctxt).atts_special, None);
    //     (*ctxt).atts_special = null_mut();
    // }
}

pub(crate) const SAX_COMPAT_MODE: &CStr = c"SAX compatibility mode document";

/*
 * Less common routines and SAX interfaces
 */
/**
 * xmlParseDocument:
 * @ctxt:  an XML parser context
 *
 * parse an XML document (and build a tree if using the standard SAX
 * interface).
 *
 * [1] document ::= prolog element Misc*
 *
 * [22] prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
 *
 * Returns 0, -1 in case of error. the parser context is augmented
 *                as a result of the parsing.
 */
pub unsafe extern "C" fn xml_parse_document(ctxt: XmlParserCtxtPtr) -> c_int {
    let mut start: [XmlChar; 4] = [0; 4];

    xml_init_parser();

    if ctxt.is_null() || (*ctxt).input.is_null() {
        return -1;
    }

    GROW!(ctxt);

    /*
     * SAX: detecting the level.
     */
    xml_detect_sax2(ctxt);

    /*
     * SAX: beginning of the document processing.
     */
    if !(*ctxt).sax.is_null() {
        if let Some(set_document_locator) = (*(*ctxt).sax).set_document_locator {
            set_document_locator((*ctxt).user_data, xml_default_sax_locator());
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }

    if (*ctxt).encoding.is_null() && (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4 {
        /*
         * Get the 4 first bytes and decode the charset
         * if enc != XML_CHAR_ENCODING_NONE
         * plug some encoding conversion routines.
         */
        start[0] = RAW!(ctxt);
        start[1] = NXT!(ctxt, 1);
        start[2] = NXT!(ctxt, 2);
        start[3] = NXT!(ctxt, 3);
        let enc = xml_detect_char_encoding(addr_of_mut!(start[0]), 4);
        if !matches!(enc, XmlCharEncoding::XmlCharEncodingNone) {
            xml_switch_encoding(ctxt, enc);
        }
    }

    if CUR!(ctxt) == 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEmpty, null());
        return -1;
    }

    GROW!(ctxt);
    if CMP5!(CUR_PTR!(ctxt), b'<', b'?', b'x', b'm', b'l') && IS_BLANK_CH!(NXT!(ctxt, 5)) {
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
        SKIP_BLANKS!(ctxt);
    } else {
        (*ctxt).version = xml_char_strdup(XML_DEFAULT_VERSION.as_ptr() as _);
    }
    if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
        if let Some(start_document) = (*(*ctxt).sax).start_document {
            start_document((*ctxt).user_data);
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }
    if !(*ctxt).my_doc.is_null()
        && !(*ctxt).input.is_null()
        && !(*(*ctxt).input).buf.is_null()
        && (*(*(*ctxt).input).buf).compressed >= 0
    {
        (*(*ctxt).my_doc).compression = (*(*(*ctxt).input).buf).compressed;
    }

    /*
     * The Misc part of the Prolog
     */
    xml_parse_misc(ctxt);

    /*
     * Then possibly doc type declaration(s) and more Misc
     * (doctypedecl Misc*)?
     */
    GROW!(ctxt);
    if CMP9!(
        CUR_PTR!(ctxt),
        b'<',
        b'!',
        b'D',
        b'O',
        b'C',
        b'T',
        b'Y',
        b'P',
        b'E'
    ) {
        (*ctxt).in_subset = 1;
        xml_parse_doc_type_decl(ctxt);
        if RAW!(ctxt) == b'[' {
            (*ctxt).instate = XmlParserInputState::XmlParserDTD;
            xml_parse_internal_subset(ctxt);
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                return -1;
            }
        }

        /*
         * Create and update the external subset.
         */
        (*ctxt).in_subset = 2;
        if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
            if let Some(external_subset) = (*(*ctxt).sax).external_subset {
                external_subset(
                    (*ctxt).user_data,
                    (*ctxt).int_sub_name,
                    (*ctxt).ext_sub_system,
                    (*ctxt).ext_sub_uri,
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

    /*
     * Time to start parsing the tree itself
     */
    GROW!(ctxt);
    if RAW!(ctxt) != b'<' {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrDocumentEmpty,
            c"Start tag expected, '<' not found\n".as_ptr() as _,
        );
    } else {
        (*ctxt).instate = XmlParserInputState::XmlParserContent;
        xml_parse_element(ctxt);
        (*ctxt).instate = XmlParserInputState::XmlParserEpilog;

        /*
         * The Misc part at the end
         */
        xml_parse_misc(ctxt);

        if RAW!(ctxt) != 0 {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEnd, null());
        }
        (*ctxt).instate = XmlParserInputState::XmlParserEOF;
    }

    /*
     * SAX: end of the document processing.
     */
    if !(*ctxt).sax.is_null() {
        if let Some(end_document) = (*(*ctxt).sax).end_document {
            end_document((*ctxt).user_data);
        }
    }

    /*
     * Remove locally kept entity definitions if the tree was not built
     */
    if !(*ctxt).my_doc.is_null()
        && xml_str_equal((*(*ctxt).my_doc).version, SAX_COMPAT_MODE.as_ptr() as _)
    {
        xml_free_doc((*ctxt).my_doc);
        (*ctxt).my_doc = null_mut();
    }

    if (*ctxt).well_formed != 0 && !(*ctxt).my_doc.is_null() {
        (*(*ctxt).my_doc).properties |= XmlDocProperties::XmlDocWellformed as i32;
        if (*ctxt).valid != 0 {
            (*(*ctxt).my_doc).properties |= XmlDocProperties::XmlDocDtdvalid as i32;
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

/**
 * xmlParseExtParsedEnt:
 * @ctxt:  an XML parser context
 *
 * parse a general parsed entity
 * An external general parsed entity is well-formed if it matches the
 * production labeled extParsedEnt.
 *
 * [78] extParsedEnt ::= TextDecl? content
 *
 * Returns 0, -1 in case of error. the parser context is augmented
 *                as a result of the parsing.
 */
pub unsafe extern "C" fn xml_parse_ext_parsed_ent(ctxt: XmlParserCtxtPtr) -> c_int {
    let mut start: [XmlChar; 4] = [0; 4];
    let enc: XmlCharEncoding;

    if ctxt.is_null() || (*ctxt).input.is_null() {
        return -1;
    }

    xml_detect_sax2(ctxt);

    GROW!(ctxt);

    /*
     * SAX: beginning of the document processing.
     */
    if !(*ctxt).sax.is_null() {
        if let Some(set_document_locator) = (*(*ctxt).sax).set_document_locator {
            set_document_locator((*ctxt).user_data, xml_default_sax_locator());
        }
    }

    /*
     * Get the 4 first bytes and decode the charset
     * if enc != XML_CHAR_ENCODING_NONE
     * plug some encoding conversion routines.
     */
    if (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4 {
        start[0] = RAW!(ctxt);
        start[1] = NXT!(ctxt, 1);
        start[2] = NXT!(ctxt, 2);
        start[3] = NXT!(ctxt, 3);
        enc = xml_detect_char_encoding(start.as_ptr(), 4);
        if !matches!(enc, XmlCharEncoding::XmlCharEncodingNone) {
            xml_switch_encoding(ctxt, enc);
        }
    }

    if CUR!(ctxt) == 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEmpty, null());
    }

    /*
     * Check for the XMLDecl in the Prolog.
     */
    GROW!(ctxt);
    if CMP5!(CUR_PTR!(ctxt), b'<', b'?', b'x', b'm', b'l') && IS_BLANK_CH!(NXT!(ctxt, 5)) {
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
        SKIP_BLANKS!(ctxt);
    } else {
        (*ctxt).version = xml_char_strdup(XML_DEFAULT_VERSION.as_ptr() as _);
    }
    if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
        if let Some(start_document) = (*(*ctxt).sax).start_document {
            start_document((*ctxt).user_data);
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

    if RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, null());
    } else if RAW!(ctxt) != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtraContent, null());
    }

    /*
     * SAX: end of the document processing.
     */
    if !(*ctxt).sax.is_null() {
        if let Some(end_document) = (*(*ctxt).sax).end_document {
            end_document((*ctxt).user_data);
        }
    }

    if (*ctxt).well_formed == 0 {
        return -1;
    }
    0
}

/**
 * xmlSAXUserParseFile:
 * @sax:  a SAX handler
 * @user_data:  The user data returned on SAX callbacks
 * @filename:  a file name
 *
 * DEPRECATED: Use xmlNewSAXParserCtxt and xmlCtxtReadFile.
 *
 * parse an XML file and call the given SAX handler routines.
 * Automatic support for ZLIB/Compress compressed document is provided
 *
 * Returns 0 in case of success or a error number otherwise
 */
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_sax_user_parse_file(
    sax: XmlSAXHandlerPtr,
    user_data: *mut c_void,
    filename: *const c_char,
) -> c_int {
    let ret: c_int;

    let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(filename);
    if ctxt.is_null() {
        return -1;
    }
    if (*ctxt).sax != xml_default_sax_handler() as XmlSAXHandlerPtr {
        xml_free((*ctxt).sax as _);
    }
    (*ctxt).sax = sax;
    xml_detect_sax2(ctxt);

    if !user_data.is_null() {
        (*ctxt).user_data = user_data;
    }

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

/**
 * xmlSAXUserParseMemory:
 * @sax:  a SAX handler
 * @user_data:  The user data returned on SAX callbacks
 * @buffer:  an in-memory XML document input
 * @size:  the length of the XML document in bytes
 *
 * DEPRECATED: Use xmlNewSAXParserCtxt and xmlCtxtReadMemory.
 *
 * parse an XML in-memory buffer and call the given SAX handler routines.
 *
 * Returns 0 in case of success or a error number otherwise
 */
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_sax_user_parse_memory(
    sax: XmlSAXHandlerPtr,
    user_data: *mut c_void,
    buffer: *const c_char,
    size: c_int,
) -> c_int {
    let ret: c_int;

    xml_init_parser();

    let ctxt: XmlParserCtxtPtr = xml_create_memory_parser_ctxt(buffer, size);
    if ctxt.is_null() {
        return -1;
    }
    if (*ctxt).sax != xml_default_sax_handler() as XmlSAXHandlerPtr {
        xml_free((*ctxt).sax as _);
    }
    (*ctxt).sax = sax;
    xml_detect_sax2(ctxt);

    if !user_data.is_null() {
        (*ctxt).user_data = user_data;
    }

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

/**
 * xmlSAXParseDoc:
 * @sax:  the SAX handler block
 * @cur:  a pointer to an array of XmlChar
 * @recovery:  work in recovery mode, i.e. tries to read no Well Formed
 *             documents
 *
 * DEPRECATED: Use xmlNewSAXParserCtxt and xmlCtxtReadDoc.
 *
 * parse an XML in-memory document and build a tree.
 * It use the given SAX function block to handle the parsing callback.
 * If sax is NULL, fallback to the default DOM tree building routines.
 *
 * Returns the resulting document tree
 */
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_sax_parse_doc(
    sax: XmlSAXHandlerPtr,
    cur: *const XmlChar,
    recovery: c_int,
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
        (*ctxt).user_data = null_mut();
    }
    xml_detect_sax2(ctxt);

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

/**
 * xmlSAXParseMemory:
 * @sax:  the SAX handler block
 * @buffer:  an pointer to a c_char array
 * @size:  the size of the array
 * @recovery:  work in recovery mode, i.e. tries to read not Well Formed
 *             documents
 *
 * DEPRECATED: Use xmlNewSAXParserCtxt and xmlCtxtReadMemory.
 *
 * parse an XML in-memory block and use the given SAX function block
 * to handle the parsing callback. If sax is NULL, fallback to the default
 * DOM tree building routines.
 *
 * Returns the resulting document tree
 */
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_sax_parse_memory(
    sax: XmlSAXHandlerPtr,
    buffer: *const c_char,
    size: c_int,
    recovery: c_int,
) -> XmlDocPtr {
    xml_sax_parse_memory_with_data(sax, buffer, size, recovery, null_mut())
}

/**
 * xmlSAXParseMemoryWithData:
 * @sax:  the SAX handler block
 * @buffer:  an pointer to a c_char array
 * @size:  the size of the array
 * @recovery:  work in recovery mode, i.e. tries to read no Well Formed
 *             documents
 * @data:  the userdata
 *
 * DEPRECATED: Use xmlNewSAXParserCtxt and xmlCtxtReadMemory.
 *
 * parse an XML in-memory block and use the given SAX function block
 * to handle the parsing callback. If sax is NULL, fallback to the default
 * DOM tree building routines.
 *
 * User data (c_void *) is stored within the parser context in the
 * context's _private member, so it is available nearly everywhere in libxml
 *
 * Returns the resulting document tree
 */
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_sax_parse_memory_with_data(
    sax: XmlSAXHandlerPtr,
    buffer: *const c_char,
    size: c_int,
    recovery: c_int,
    data: *mut c_void,
) -> XmlDocPtr {
    let ret: XmlDocPtr;

    xml_init_parser();

    let ctxt: XmlParserCtxtPtr = xml_create_memory_parser_ctxt(buffer, size);
    if ctxt.is_null() {
        return null_mut();
    }
    if !sax.is_null() {
        if !(*ctxt).sax.is_null() {
            xml_free((*ctxt).sax as _);
        }
        (*ctxt).sax = sax;
    }
    xml_detect_sax2(ctxt);
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

/**
 * xmlSAXParseFile:
 * @sax:  the SAX handler block
 * @filename:  the filename
 * @recovery:  work in recovery mode, i.e. tries to read no Well Formed
 *             documents
 *
 * DEPRECATED: Use xmlNewSAXParserCtxt and xmlCtxtReadFile.
 *
 * parse an XML file and build a tree. Automatic support for ZLIB/Compress
 * compressed document is provided by default if found at compile-time.
 * It use the given SAX function block to handle the parsing callback.
 * If sax is NULL, fallback to the default DOM tree building routines.
 *
 * Returns the resulting document tree
 */
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_sax_parse_file(
    sax: XmlSAXHandlerPtr,
    filename: *const c_char,
    recovery: c_int,
) -> XmlDocPtr {
    xml_sax_parse_file_with_data(sax, filename, recovery, null_mut())
}

/**
 * xmlSAXParseFileWithData:
 * @sax:  the SAX handler block
 * @filename:  the filename
 * @recovery:  work in recovery mode, i.e. tries to read no Well Formed
 *             documents
 * @data:  the userdata
 *
 * DEPRECATED: Use xmlNewSAXParserCtxt and xmlCtxtReadFile.
 *
 * parse an XML file and build a tree. Automatic support for ZLIB/Compress
 * compressed document is provided by default if found at compile-time.
 * It use the given SAX function block to handle the parsing callback.
 * If sax is NULL, fallback to the default DOM tree building routines.
 *
 * User data (c_void *) is stored within the parser context in the
 * context's _private member, so it is available nearly everywhere in libxml
 *
 * Returns the resulting document tree
 */
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_sax_parse_file_with_data(
    sax: XmlSAXHandlerPtr,
    filename: *const c_char,
    recovery: c_int,
    data: *mut c_void,
) -> XmlDocPtr {
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
    xml_detect_sax2(ctxt);
    if !data.is_null() {
        (*ctxt)._private = data;
    }

    if (*ctxt).directory.is_null() {
        (*ctxt).directory = xml_parser_get_directory(filename);
    }

    (*ctxt).recovery = recovery;

    xml_parse_document(ctxt);

    if (*ctxt).well_formed != 0 || recovery != 0 {
        ret = (*ctxt).my_doc;
        if !ret.is_null() && !(*(*ctxt).input).buf.is_null() {
            if (*(*(*ctxt).input).buf).compressed > 0 {
                (*ret).compression = 9;
            } else {
                (*ret).compression = (*(*(*ctxt).input).buf).compressed;
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

/**
 * xmlSAXParseEntity:
 * @sax:  the SAX handler block
 * @filename:  the filename
 *
 * DEPRECATED: Don't use.
 *
 * parse an XML external entity out of context and build a tree.
 * It use the given SAX function block to handle the parsing callback.
 * If sax is NULL, fallback to the default DOM tree building routines.
 *
 * [78] extParsedEnt ::= TextDecl? content
 *
 * This correspond to a "Well Balanced" chunk
 *
 * Returns the resulting document tree
 */
#[cfg(feature = "sax1")]
pub(crate) unsafe extern "C" fn xml_sax_parse_entity(
    sax: XmlSAXHandlerPtr,
    filename: *const c_char,
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
        (*ctxt).user_data = null_mut();
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

/**
* xmlParseEntity:
* @filename:  the filename
*
* parse an XML external entity out of context and build a tree.
*
* [78] extParsedEnt ::= TextDecl? content
*
* This correspond to a "Well Balanced" chunk
*
* Returns the resulting document tree
*/
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_parse_entity(filename: *const c_char) -> XmlDocPtr {
    xml_sax_parse_entity(null_mut(), filename)
}

/**
 * xmlSAXParseDTD:
 * @sax:  the SAX handler block
 * @ExternalID:  a NAME* containing the External ID of the DTD
 * @SystemID:  a NAME* containing the URL to the DTD
 *
 * DEPRECATED: Don't use.
 *
 * Load and parse an external subset.
 *
 * Returns the resulting xmlDtdPtr or NULL in case of error.
 */
#[cfg(feature = "valid")]
pub(crate) unsafe extern "C" fn xml_sax_parse_dtd(
    sax: XmlSAXHandlerPtr,
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) -> XmlDtdPtr {
    let mut ret: XmlDtdPtr = null_mut();
    let mut input: XmlParserInputPtr = null_mut();
    let enc: XmlCharEncoding;

    if external_id.is_null() && system_id.is_null() {
        return null_mut();
    }

    let ctxt: XmlParserCtxtPtr = xml_new_sax_parser_ctxt(sax, null_mut());
    if ctxt.is_null() {
        return null_mut();
    }

    /* We are loading a DTD */
    (*ctxt).options |= XmlParserOption::XmlParseDtdload as i32;

    /*
     * Canonicalise the system ID
     */
    let system_id_canonic: *mut XmlChar = xml_canonic_path(system_id);
    if !system_id.is_null() && system_id_canonic.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    /*
     * Ask the Entity resolver to load the damn thing
     */

    if !(*ctxt).sax.is_null() {
        if let Some(f) = (*(*ctxt).sax).resolve_entity {
            input = f((*ctxt).user_data, external_id, system_id_canonic);
        }
    }
    if input.is_null() {
        xml_free_parser_ctxt(ctxt);
        if !system_id_canonic.is_null() {
            xml_free(system_id_canonic as _);
        }
        return null_mut();
    }

    /*
     * plug some encoding conversion routines here.
     */
    if xml_push_input(ctxt, input) < 0 {
        xml_free_parser_ctxt(ctxt);
        if !system_id_canonic.is_null() {
            xml_free(system_id_canonic as _);
        }
        return null_mut();
    }
    if (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4 {
        enc = xml_detect_char_encoding((*(*ctxt).input).cur, 4);
        xml_switch_encoding(ctxt, enc);
    }

    if (*input).filename.is_null() {
        (*input).filename = system_id_canonic as _;
    } else {
        xml_free(system_id_canonic as _);
    }
    (*input).line = 1;
    (*input).col = 1;
    (*input).base = (*(*ctxt).input).cur;
    (*input).cur = (*(*ctxt).input).cur;
    (*input).free = None;

    /*
     * let's parse that entity knowing it's an external subset.
     */
    (*ctxt).in_subset = 2;
    (*ctxt).my_doc = xml_new_doc(c"1.0".as_ptr() as _);
    if (*ctxt).my_doc.is_null() {
        xml_err_memory(ctxt, c"New Doc failed".as_ptr() as _);
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }
    (*(*ctxt).my_doc).properties = XmlDocProperties::XmlDocInternal as i32;
    (*(*ctxt).my_doc).ext_subset = xml_new_dtd(
        (*ctxt).my_doc,
        c"none".as_ptr() as _,
        external_id,
        system_id,
    );
    xml_parse_external_subset(ctxt, external_id, system_id);

    if !(*ctxt).my_doc.is_null() {
        if (*ctxt).well_formed != 0 {
            ret = (*(*ctxt).my_doc).ext_subset;
            (*(*ctxt).my_doc).ext_subset = null_mut();
            if !ret.is_null() {
                let mut tmp: XmlNodePtr;

                (*ret).doc = null_mut();
                tmp = (*ret).children;
                while !tmp.is_null() {
                    (*tmp).doc = null_mut();
                    tmp = (*tmp).next;
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

/**
 * xmlParseDTD:
 * @ExternalID:  a NAME* containing the External ID of the DTD
 * @SystemID:  a NAME* containing the URL to the DTD
 *
 * Load and parse an external subset.
 *
 * Returns the resulting xmlDtdPtr or NULL in case of error.
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_parse_dtd(
    external_id: *const XmlChar,
    system_id: *const XmlChar,
) -> XmlDtdPtr {
    xml_sax_parse_dtd(null_mut(), external_id, system_id)
}

/**
 * xmlIOParseDTD:
 * @sax:  the SAX handler block or NULL
 * @input:  an Input Buffer
 * @enc:  the charset encoding if known
 *
 * Load and parse a DTD
 *
 * Returns the resulting xmlDtdPtr or NULL in case of error.
 * @input will be freed by the function in any case.
 */
#[cfg(feature = "valid")]
pub unsafe extern "C" fn xml_io_parse_dtd(
    sax: XmlSAXHandlerPtr,
    input: XmlParserInputBufferPtr,
    mut enc: XmlCharEncoding,
) -> XmlDtdPtr {
    let mut ret: XmlDtdPtr = null_mut();
    let mut start: [XmlChar; 4] = [0; 4];

    if input.is_null() {
        return null_mut();
    }

    let ctxt: XmlParserCtxtPtr = xml_new_sax_parser_ctxt(sax, null_mut());
    if ctxt.is_null() {
        xml_free_parser_input_buffer(input);
        return null_mut();
    }

    /* We are loading a DTD */
    (*ctxt).options |= XmlParserOption::XmlParseDtdload as i32;

    xml_detect_sax2(ctxt);

    /*
     * generate a parser input from the I/O handler
     */

    let pinput: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, input, XmlCharEncoding::XmlCharEncodingNone);
    if pinput.is_null() {
        xml_free_parser_input_buffer(input);
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
    if !matches!(enc, XmlCharEncoding::XmlCharEncodingNone) {
        xml_switch_encoding(ctxt, enc);
    }

    (*pinput).filename = null_mut();
    (*pinput).line = 1;
    (*pinput).col = 1;
    (*pinput).base = (*(*ctxt).input).cur;
    (*pinput).cur = (*(*ctxt).input).cur;
    (*pinput).free = None;

    /*
     * let's parse that entity knowing it's an external subset.
     */
    (*ctxt).in_subset = 2;
    (*ctxt).my_doc = xml_new_doc(c"1.0".as_ptr() as _);
    if (*ctxt).my_doc.is_null() {
        xml_err_memory(ctxt, c"New Doc failed".as_ptr() as _);
        return null_mut();
    }
    (*(*ctxt).my_doc).properties = XmlDocProperties::XmlDocInternal as i32;
    (*(*ctxt).my_doc).ext_subset = xml_new_dtd(
        (*ctxt).my_doc,
        c"none".as_ptr() as _,
        c"none".as_ptr() as _,
        c"none".as_ptr() as _,
    );

    if matches!(enc, XmlCharEncoding::XmlCharEncodingNone)
        && (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4
    {
        /*
         * Get the 4 first bytes and decode the charset
         * if enc != xmlCharEncoding::XML_CHAR_ENCODING_NONE
         * plug some encoding conversion routines.
         */
        start[0] = RAW!(ctxt);
        start[1] = NXT!(ctxt, 1);
        start[2] = NXT!(ctxt, 2);
        start[3] = NXT!(ctxt, 3);
        enc = xml_detect_char_encoding(start.as_ptr(), 4);
        if !matches!(enc, XmlCharEncoding::XmlCharEncodingNone) {
            xml_switch_encoding(ctxt, enc);
        }
    }

    xml_parse_external_subset(ctxt, c"none".as_ptr() as _, c"none".as_ptr() as _);

    if !(*ctxt).my_doc.is_null() {
        if (*ctxt).well_formed != 0 {
            ret = (*(*ctxt).my_doc).ext_subset;
            (*(*ctxt).my_doc).ext_subset = null_mut();
            if !ret.is_null() {
                let mut tmp: XmlNodePtr;

                (*ret).doc = null_mut();
                tmp = (*ret).children;
                while !tmp.is_null() {
                    (*tmp).doc = null_mut();
                    tmp = (*tmp).next;
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

/**
 * xmlParseBalancedChunkMemory:
 * @doc:  the document the chunk pertains to (must not be NULL)
 * @sax:  the SAX handler block (possibly NULL)
 * @user_data:  The user data returned on SAX callbacks (possibly NULL)
 * @depth:  Used for loop detection, use 0
 * @string:  the input string in UTF8 or ISO-Latin (zero terminated)
 * @lst:  the return value for the set of parsed nodes
 *
 * Parse a well-balanced chunk of an XML document
 * called by the parser
 * The allowed sequence for the Well Balanced Chunk is the one defined by
 * the content production in the XML grammar:
 *
 * [43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*
 *
 * Returns 0 if the chunk is well balanced, -1 in case of args problem and
 *    the parser error code otherwise
 */
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_parse_balanced_chunk_memory(
    doc: XmlDocPtr,
    sax: XmlSAXHandlerPtr,
    user_data: *mut c_void,
    depth: c_int,
    string: *const XmlChar,
    lst: *mut XmlNodePtr,
) -> c_int {
    xml_parse_balanced_chunk_memory_recover(doc, sax, user_data, depth, string, lst, 0)
}

/*
 * xmlGetNamespace:
 * @ctxt:  an XML parser context
 * @prefix:  the prefix to lookup
 *
 * Lookup the namespace name for the @prefix (which ca be NULL)
 * The prefix must come from the @(*ctxt).dict dictionary
 *
 * Returns the namespace name or NULL if not bound
 */
unsafe extern "C" fn xml_get_namespace(
    ctxt: XmlParserCtxtPtr,
    prefix: *const XmlChar,
) -> *const XmlChar {
    if prefix == (*ctxt).str_xml {
        return (*ctxt).str_xml_ns;
    }
    for i in (0..(*ctxt).ns_nr - 1).rev().step_by(2) {
        if *(*ctxt).ns_tab.add(i as usize) == prefix {
            if prefix.is_null() && *(*(*ctxt).ns_tab.add(i as usize + 1)) == 0 {
                return null_mut();
            }
            return *(*ctxt).ns_tab.add(i as usize + 1);
        }
    }
    null_mut()
}

/**
 * nsPush:
 * @ctxt:  an XML parser context
 * @prefix:  the namespace prefix or NULL
 * @URL:  the namespace name
 *
 * Pushes a new parser namespace on top of the ns stack
 *
 * Returns -1 in case of error, -2 if the namespace should be discarded and the index in the stack otherwise.
 */
pub(crate) unsafe extern "C" fn ns_push(
    ctxt: XmlParserCtxtPtr,
    prefix: *const XmlChar,
    url: *const XmlChar,
) -> c_int {
    if (*ctxt).options & XmlParserOption::XmlParseNsclean as i32 != 0 {
        for i in (0..(*ctxt).ns_nr - 1).rev().step_by(2) {
            if *(*ctxt).ns_tab.add(i as usize) == prefix {
                /* in scope */
                if *(*ctxt).ns_tab.add(i as usize + 1) == url {
                    return -2;
                }
                /* out of scope keep it */
                break;
            }
        }
    }
    if (*ctxt).ns_max == 0 || (*ctxt).ns_tab.is_null() {
        (*ctxt).ns_max = 10;
        (*ctxt).ns_nr = 0;
        (*ctxt).ns_tab = xml_malloc((*ctxt).ns_max as usize * size_of::<*mut XmlChar>()) as _;
        if (*ctxt).ns_tab.is_null() {
            xml_err_memory(ctxt, null());
            (*ctxt).ns_max = 0;
            return -1;
        }
    } else if (*ctxt).ns_nr >= (*ctxt).ns_max {
        (*ctxt).ns_max *= 2;
        let tmp: *mut *const XmlChar = xml_realloc(
            (*ctxt).ns_tab as _,
            (*ctxt).ns_max as usize * size_of_val(&*(*ctxt).ns_tab.add(0)),
        ) as _;
        if tmp.is_null() {
            xml_err_memory(ctxt, null());
            (*ctxt).ns_max /= 2;
            return -1;
        }
        (*ctxt).ns_tab = tmp;
    }
    *(*ctxt).ns_tab.add((*ctxt).ns_nr as usize) = prefix;
    (*ctxt).ns_nr += 1;
    *(*ctxt).ns_tab.add((*ctxt).ns_nr as usize) = url;
    (*ctxt).ns_nr += 1;
    (*ctxt).ns_nr
}

/**
 * nsPop:
 * @ctxt: an XML parser context
 * @nr:  the number to pop
 *
 * Pops the top @nr parser prefix/namespace from the ns stack
 *
 * Returns the number of namespaces removed
 */
pub(crate) unsafe extern "C" fn ns_pop(ctxt: XmlParserCtxtPtr, mut nr: c_int) -> c_int {
    if (*ctxt).ns_tab.is_null() {
        return 0;
    }
    if (*ctxt).ns_nr < nr {
        generic_error!("Pbm popping {} NS\n", nr);
        nr = (*ctxt).ns_nr;
    }
    if (*ctxt).ns_nr <= 0 {
        return 0;
    }

    for _ in 0..nr {
        (*ctxt).ns_nr -= 1;
        *(*ctxt).ns_tab.add((*ctxt).ns_nr as usize) = null_mut();
    }
    nr
}

/**
 * xmlParseInNodeContext:
 * @node:  the context node
 * @data:  the input string
 * @datalen:  the input string length in bytes
 * @options:  a combination of xmlParserOption
 * @lst:  the return value for the set of parsed nodes
 *
 * Parse a well-balanced chunk of an XML document
 * within the context (DTD, namespaces, etc ...) of the given node.
 *
 * The allowed sequence for the data is a Well Balanced Chunk defined by
 * the content production in the XML grammar:
 *
 * [43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*
 *
 * Returns xmlParserErrors::XML_ERR_OK if the chunk is well balanced, and the parser
 * error code otherwise
 */
pub unsafe extern "C" fn xml_parse_in_node_context(
    mut node: XmlNodePtr,
    data: *const c_char,
    datalen: c_int,
    mut options: c_int,
    lst: *mut XmlNodePtr,
) -> XmlParserErrors {
    let ctxt: XmlParserCtxtPtr;
    let mut cur: XmlNodePtr;
    let mut nsnr: c_int = 0;
    let ret: XmlParserErrors;

    /*
     * check all input parameters, grab the document
     */
    if lst.is_null() || node.is_null() || data.is_null() || datalen < 0 {
        return XmlParserErrors::XmlErrInternalError;
    }
    match (*node).typ {
        XmlElementType::XmlElementNode
        | XmlElementType::XmlAttributeNode
        | XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlDocumentNode
        | XmlElementType::XmlHtmlDocumentNode => {}
        _ => {
            return XmlParserErrors::XmlErrInternalError;
        }
    }
    while !node.is_null()
        && !matches!(
            (*node).typ,
            XmlElementType::XmlElementNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlHtmlDocumentNode
        )
    {
        node = (*node).parent;
    }
    if node.is_null() {
        return XmlParserErrors::XmlErrInternalError;
    }
    let doc = if (*node).typ == XmlElementType::XmlElementNode {
        (*node).doc
    } else {
        node as XmlDocPtr
    };
    if doc.is_null() {
        return XmlParserErrors::XmlErrInternalError;
    }

    /*
     * allocate a context and set-up everything not related to the
     * node position in the tree
     */
    if (*doc).typ == XmlElementType::XmlDocumentNode {
        ctxt = xml_create_memory_parser_ctxt(data, datalen);
    } else if cfg!(feature = "html") && (*doc).typ == XmlElementType::XmlHtmlDocumentNode {
        #[cfg(feature = "html")]
        {
            ctxt = html_create_memory_parser_ctxt(data, datalen);
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

    if !(*doc).encoding.is_null() {
        if !(*ctxt).encoding.is_null() {
            xml_free((*ctxt).encoding as _);
        }
        (*ctxt).encoding = xml_strdup((*doc).encoding);

        let hdlr: XmlCharEncodingHandlerPtr = xml_find_char_encoding_handler((*doc).encoding as _);
        if !hdlr.is_null() {
            xml_switch_to_encoding(ctxt, hdlr);
        } else {
            return XmlParserErrors::XmlErrUnsupportedEncoding;
        }
    }

    xml_ctxt_use_options_internal(ctxt, options, null_mut());
    xml_detect_sax2(ctxt);
    (*ctxt).my_doc = doc;
    /* parsing in context, i.e. as within existing content */
    (*ctxt).input_id = 2;
    (*ctxt).instate = XmlParserInputState::XmlParserContent;

    let fake: XmlNodePtr = xml_new_doc_comment((*node).doc, null_mut());
    if fake.is_null() {
        xml_free_parser_ctxt(ctxt);
        return XmlParserErrors::XmlErrNoMemory;
    }
    xml_add_child(node, fake);

    if (*node).typ == XmlElementType::XmlElementNode {
        node_push(ctxt, node);
        /*
         * initialize the SAX2 namespaces stack
         */
        cur = node;
        while !cur.is_null() && (*cur).typ == XmlElementType::XmlElementNode {
            let mut ns: XmlNsPtr = (*cur).ns_def;
            let mut iprefix: *const XmlChar;
            let mut ihref: *const XmlChar;

            while !ns.is_null() {
                if !(*ctxt).dict.is_null() {
                    iprefix =
                        xml_dict_lookup((*ctxt).dict, (*ns).prefix.load(Ordering::Relaxed), -1);
                    ihref = xml_dict_lookup((*ctxt).dict, (*ns).href.load(Ordering::Relaxed), -1);
                } else {
                    iprefix = (*ns).prefix.load(Ordering::Relaxed) as _;
                    ihref = (*ns).href.load(Ordering::Relaxed) as _;
                }

                if xml_get_namespace(ctxt, iprefix).is_null() {
                    ns_push(ctxt, iprefix, ihref);
                    nsnr += 1;
                }
                ns = (*ns).next.load(Ordering::Relaxed) as _;
            }
            cur = (*cur).parent;
        }
    }

    if (*ctxt).validate != 0 || (*ctxt).replace_entities != 0 {
        /*
         * ID/IDREF registration will be done in xmlValidateElement below
         */
        (*ctxt).loadsubset |= XML_SKIP_IDS as i32;
    }

    #[cfg(feature = "html")]
    {
        if (*doc).typ == XmlElementType::XmlHtmlDocumentNode {
            __html_parse_content(ctxt as _);
        } else {
            xml_parse_content(ctxt);
        }
    }
    #[cfg(not(feature = "html"))]
    {
        xml_parse_content(ctxt);
    }

    ns_pop(ctxt, nsnr);
    if RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, null());
    } else if RAW!(ctxt) != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtraContent, null());
    }
    if !(*ctxt).node.is_null() && (*ctxt).node != node {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, null());
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

    /*
     * Return the newly created nodeset after unlinking it from
     * the pseudo sibling.
     */

    cur = (*fake).next;
    (*fake).next = null_mut();
    (*node).last = fake;

    if !cur.is_null() {
        (*cur).prev = null_mut();
    }

    *lst = cur;

    while !cur.is_null() {
        (*cur).parent = null_mut();
        cur = (*cur).next;
    }

    xml_unlink_node(fake);
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

/**
 * xmlParseBalancedChunkMemoryRecover:
 * @doc:  the document the chunk pertains to (must not be NULL)
 * @sax:  the SAX handler block (possibly NULL)
 * @user_data:  The user data returned on SAX callbacks (possibly NULL)
 * @depth:  Used for loop detection, use 0
 * @string:  the input string in UTF8 or ISO-Latin (zero terminated)
 * @lst:  the return value for the set of parsed nodes
 * @recover: return nodes even if the data is broken (use 0)
 *
 *
 * Parse a well-balanced chunk of an XML document
 * called by the parser
 * The allowed sequence for the Well Balanced Chunk is the one defined by
 * the content production in the XML grammar:
 *
 * [43] content ::= (element | CharData | Reference | CDSect | PI | Comment)*
 *
 * Returns 0 if the chunk is well balanced, -1 in case of args problem and
 *    the parser error code otherwise
 *
 * In case recover is set to 1, the nodelist will not be empty even if
 * the parsed chunk is not well balanced, assuming the parsing succeeded to
 * some extent.
 */
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_parse_balanced_chunk_memory_recover(
    doc: XmlDocPtr,
    sax: XmlSAXHandlerPtr,
    user_data: *mut c_void,
    depth: c_int,
    string: *const XmlChar,
    lst: *mut XmlNodePtr,
    recover: c_int,
) -> c_int {
    let mut oldsax: XmlSAXHandlerPtr = null_mut();
    let content: XmlNodePtr;
    let ret: c_int;

    if depth > 40 {
        return XmlParserErrors::XmlErrEntityLoop as i32;
    }

    if !lst.is_null() {
        *lst = null_mut();
    }
    if string.is_null() {
        return -1;
    }

    let size: c_int = xml_strlen(string);

    let ctxt: XmlParserCtxtPtr = xml_create_memory_parser_ctxt(string as _, size as _);
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).user_data = ctxt as _;
    if !sax.is_null() {
        oldsax = (*ctxt).sax;
        (*ctxt).sax = sax;
        if !user_data.is_null() {
            (*ctxt).user_data = user_data;
        }
    }
    let new_doc: XmlDocPtr = xml_new_doc(c"1.0".as_ptr() as _);
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
        xml_ctxt_use_options_internal(ctxt, XmlParserOption::XmlParseNodict as i32, null());
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
    xml_add_child(new_doc as XmlNodePtr, new_root);
    node_push(ctxt, new_root);
    /* doc.is_null() is only supported for historic reasons */
    if doc.is_null() {
        (*ctxt).my_doc = new_doc;
    } else {
        (*ctxt).my_doc = new_doc;
        (*(*new_doc).children).doc = doc;
        /* Ensure that doc has XML spec namespace */
        xml_search_ns_by_href(doc, doc as _, XML_XML_NAMESPACE.as_ptr() as _);
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
    xml_detect_sax2(ctxt);

    if !doc.is_null() {
        content = (*doc).children;
        (*doc).children = null_mut();
        xml_parse_content(ctxt);
        (*doc).children = content;
    } else {
        xml_parse_content(ctxt);
    }
    if RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, null());
    } else if RAW!(ctxt) != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtraContent, null());
    }
    if (*ctxt).node != (*new_doc).children {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, null());
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
        let mut cur: XmlNodePtr;

        /*
         * Return the newly created nodeset after unlinking it from
         * they pseudo parent.
         */
        cur = (*(*new_doc).children).children;
        *lst = cur;
        while !cur.is_null() {
            xml_set_tree_doc(cur, doc);
            (*cur).parent = null_mut();
            cur = (*cur).next;
        }
        (*(*new_doc).children).children = null_mut();
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

pub(crate) unsafe extern "C" fn xml_saturated_add(dst: *mut c_ulong, val: c_ulong) {
    *dst = (*dst).saturating_add(val);
}

pub(crate) unsafe extern "C" fn xml_saturated_add_size_t(dst: *mut c_ulong, val: c_ulong) {
    *dst = (*dst).saturating_add(val);
}

/**
 * xmlParseExternalEntityPrivate:
 * @doc:  the document the chunk pertains to
 * @oldctxt:  the previous parser context if available
 * @sax:  the SAX handler block (possibly NULL)
 * @user_data:  The user data returned on SAX callbacks (possibly NULL)
 * @depth:  Used for loop detection, use 0
 * @URL:  the URL for the entity to load
 * @ID:  the System ID for the entity to load
 * @list:  the return value for the set of parsed nodes
 *
 * Private version of xmlParseExternalEntity()
 *
 * Returns 0 if the entity is well formed, -1 in case of args problem and
 *    the parser error code otherwise
 */
pub(crate) unsafe extern "C" fn xml_parse_external_entity_private(
    doc: XmlDocPtr,
    oldctxt: XmlParserCtxtPtr,
    sax: XmlSAXHandlerPtr,
    user_data: *mut c_void,
    depth: c_int,
    url: *const XmlChar,
    id: *const XmlChar,
    list: *mut XmlNodePtr,
) -> XmlParserErrors {
    let ret: XmlParserErrors;
    let mut start: [XmlChar; 4] = [0; 4];
    let enc: XmlCharEncoding;

    if (depth > 40
        && (oldctxt.is_null() || (*oldctxt).options & XmlParserOption::XmlParseHuge as i32 == 0))
        || depth > 100
    {
        xml_fatal_err_msg(
            oldctxt,
            XmlParserErrors::XmlErrEntityLoop,
            c"Maximum entity nesting depth exceeded".as_ptr() as _,
        );
        return XmlParserErrors::XmlErrEntityLoop;
    }

    if !list.is_null() {
        *list = null_mut();
    }
    if url.is_null() && id.is_null() {
        return XmlParserErrors::XmlErrInternalError;
    }
    if doc.is_null() {
        return XmlParserErrors::XmlErrInternalError;
    }

    let ctxt: XmlParserCtxtPtr =
        xml_create_entity_parser_ctxt_internal(sax, user_data, url, id, null(), oldctxt);
    if ctxt.is_null() {
        return XmlParserErrors::XmlWarUndeclaredEntity;
    }
    if !oldctxt.is_null() {
        (*ctxt).nb_errors = (*oldctxt).nb_errors;
        (*ctxt).nb_warnings = (*oldctxt).nb_warnings;
    }
    xml_detect_sax2(ctxt);

    let new_doc: XmlDocPtr = xml_new_doc(c"1.0".as_ptr() as _);
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
        if !(*doc).url.is_null() {
            (*new_doc).url = xml_strdup((*doc).url);
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
    xml_add_child(new_doc as XmlNodePtr, new_root);
    node_push(ctxt, (*new_doc).children);
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
    GROW!(ctxt);
    if (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) >= 4 {
        start[0] = RAW!(ctxt);
        start[1] = NXT!(ctxt, 1);
        start[2] = NXT!(ctxt, 2);
        start[3] = NXT!(ctxt, 3);
        enc = xml_detect_char_encoding(start.as_ptr(), 4);
        if !matches!(enc, XmlCharEncoding::XmlCharEncodingNone) {
            xml_switch_encoding(ctxt, enc);
        }
    }

    /*
     * Parse a possible text declaration first
     */
    if CMP5!(CUR_PTR!(ctxt), b'<', b'?', b'x', b'm', b'l') && IS_BLANK_CH!(NXT!(ctxt, 5)) {
        xml_parse_text_decl(ctxt);
        /*
         * An XML-1.0 document can't reference an entity not XML-1.0
         */
        if xml_str_equal((*oldctxt).version, c"1.0".as_ptr() as _)
            && !xml_str_equal((*(*ctxt).input).version, c"1.0".as_ptr() as _)
        {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrVersionMismatch,
                c"Version mismatch between document and entity\n".as_ptr() as _,
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
            (*ctxt).vctxt.user_data = (*oldctxt).vctxt.user_data;
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

    if RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, null());
    } else if RAW!(ctxt) != 0 {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrExtraContent, null());
    }
    if (*ctxt).node != (*new_doc).children {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotWellBalanced, null());
    }

    if (*ctxt).well_formed == 0 {
        ret = XmlParserErrors::try_from((*ctxt).err_no).unwrap();
        if !oldctxt.is_null() {
            (*oldctxt).err_no = (*ctxt).err_no;
            (*oldctxt).well_formed = 0;
            xml_copy_error(
                addr_of_mut!((*ctxt).last_error),
                addr_of_mut!((*oldctxt).last_error),
            );
        }
    } else {
        if !list.is_null() {
            let mut cur: XmlNodePtr;

            /*
             * Return the newly created nodeset after unlinking it from
             * they pseudo parent.
             */
            cur = (*(*new_doc).children).children;
            *list = cur;
            while !cur.is_null() {
                (*cur).parent = null_mut();
                cur = (*cur).next;
            }
            (*(*new_doc).children).children = null_mut();
        }
        ret = XmlParserErrors::XmlErrOK;
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

        xml_saturated_add(addr_of_mut!((*oldctxt).sizeentities), consumed);
        xml_saturated_add(addr_of_mut!((*oldctxt).sizeentities), (*ctxt).sizeentities);

        xml_saturated_add(addr_of_mut!((*oldctxt).sizeentcopy), consumed);
        xml_saturated_add(addr_of_mut!((*oldctxt).sizeentcopy), (*ctxt).sizeentcopy);
    }

    if !oldctxt.is_null() {
        (*ctxt).dict = null_mut();
        (*ctxt).atts_default = null_mut();
        (*ctxt).atts_special = null_mut();
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

/**
 * xmlParseExternalEntity:
 * @doc:  the document the chunk pertains to
 * @sax:  the SAX handler block (possibly NULL)
 * @user_data:  The user data returned on SAX callbacks (possibly NULL)
 * @depth:  Used for loop detection, use 0
 * @URL:  the URL for the entity to load
 * @ID:  the System ID for the entity to load
 * @lst:  the return value for the set of parsed nodes
 *
 * Parse an external general entity
 * An external general parsed entity is well-formed if it matches the
 * production labeled extParsedEnt.
 *
 * [78] extParsedEnt ::= TextDecl? content
 *
 * Returns 0 if the entity is well formed, -1 in case of args problem and
 *    the parser error code otherwise
 */
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe extern "C" fn xml_parse_external_entity(
    doc: XmlDocPtr,
    sax: XmlSAXHandlerPtr,
    user_data: *mut c_void,
    depth: c_int,
    url: *const XmlChar,
    id: *const XmlChar,
    lst: *mut XmlNodePtr,
) -> c_int {
    xml_parse_external_entity_private(doc, null_mut(), sax, user_data, depth, url, id, lst) as i32
}

/**
 * xmlParseCtxtExternalEntity:
 * @ctx:  the existing parsing context
 * @URL:  the URL for the entity to load
 * @ID:  the System ID for the entity to load
 * @lst:  the return value for the set of parsed nodes
 *
 * Parse an external general entity within an existing parsing context
 * An external general parsed entity is well-formed if it matches the
 * production labeled extParsedEnt.
 *
 * [78] extParsedEnt ::= TextDecl? content
 *
 * Returns 0 if the entity is well formed, -1 in case of args problem and
 *    the parser error code otherwise
 */
pub unsafe extern "C" fn xml_parse_ctxt_external_entity(
    ctx: XmlParserCtxtPtr,
    url: *const XmlChar,
    id: *const XmlChar,
    lst: *mut XmlNodePtr,
) -> c_int {
    if ctx.is_null() {
        return -1;
    }
    /*
     * If the user provided their own SAX callbacks, then reuse the
     * userData callback field, otherwise the expected setup in a
     * DOM builder is to have userData == ctxt
     */
    let user_data = if (*ctx).user_data == ctx as _ {
        null_mut()
    } else {
        (*ctx).user_data
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

/*
 * Parser contexts handling.
 */
/**
 * xmlNewParserCtxt:
 *
 * Allocate and initialize a new parser context.
 *
 * Returns the xmlParserCtxtPtr or NULL
 */
pub unsafe extern "C" fn xml_new_parser_ctxt() -> XmlParserCtxtPtr {
    xml_new_sax_parser_ctxt(null_mut(), null_mut())
}

/**
 * xmlInitSAXParserCtxt:
 * @ctxt:  XML parser context
 * @sax:  SAX handlert
 * @userData:  user data
 *
 * Initialize a SAX parser context
 *
 * Returns 0 in case of success and -1 in case of error
 */
unsafe extern "C" fn xml_init_sax_parser_ctxt(
    ctxt: XmlParserCtxtPtr,
    sax: *const XmlSAXHandler,
    user_data: *mut c_void,
) -> c_int {
    let mut input: XmlParserInputPtr;

    if ctxt.is_null() {
        xml_err_internal(
            null_mut(),
            c"Got NULL parser context\n".as_ptr() as _,
            null(),
        );
        return -1;
    }

    xml_init_parser();

    if (*ctxt).dict.is_null() {
        (*ctxt).dict = xml_dict_create();
    }
    if (*ctxt).dict.is_null() {
        xml_err_memory(
            null_mut(),
            c"cannot initialize parser context\n".as_ptr() as _,
        );
        return -1;
    }
    xml_dict_set_limit((*ctxt).dict, XML_MAX_DICTIONARY_LIMIT);

    if (*ctxt).sax.is_null() {
        (*ctxt).sax = xml_malloc(size_of::<XmlSAXHandler>()) as _;
    }
    if (*ctxt).sax.is_null() {
        xml_err_memory(
            null_mut(),
            c"cannot initialize parser context\n".as_ptr() as _,
        );
        return -1;
    }
    if sax.is_null() {
        memset((*ctxt).sax as _, 0, size_of::<XmlSAXHandler>());
        xml_sax_version((*ctxt).sax, 2);
        (*ctxt).user_data = ctxt as _;
    } else {
        if (*sax).initialized == XML_SAX2_MAGIC as u32 {
            memcpy((*ctxt).sax as _, sax as _, size_of::<XmlSAXHandler>());
        } else {
            memset((*ctxt).sax as _, 0, size_of::<XmlSAXHandler>());
            memcpy((*ctxt).sax as _, sax as _, size_of::<XmlSAXHandlerV1>());
        }
        (*ctxt).user_data = if !user_data.is_null() {
            user_data
        } else {
            ctxt as _
        };
    }

    (*ctxt).maxatts = 0;
    (*ctxt).atts = null_mut();
    /* Allocate the Input stack */
    if (*ctxt).input_tab.is_null() {
        (*ctxt).input_tab = xml_malloc(5 * size_of::<XmlParserInputPtr>()) as _;
        (*ctxt).input_max = 5;
    }
    if (*ctxt).input_tab.is_null() {
        xml_err_memory(
            null_mut(),
            c"cannot initialize parser context\n".as_ptr() as _,
        );
        (*ctxt).input_nr = 0;
        (*ctxt).input_max = 0;
        (*ctxt).input = null_mut();
        return -1;
    }
    while {
        input = input_pop(ctxt);
        !input.is_null()
    } {
        /* Non consuming */
        xml_free_input_stream(input);
    }
    (*ctxt).input_nr = 0;
    (*ctxt).input = null_mut();

    (*ctxt).version = null_mut();
    (*ctxt).encoding = null_mut();
    (*ctxt).standalone = -1;
    (*ctxt).has_external_subset = 0;
    (*ctxt).has_perefs = 0;
    (*ctxt).html = 0;
    (*ctxt).external = 0;
    (*ctxt).instate = XmlParserInputState::XmlParserStart;
    (*ctxt).token = 0;
    (*ctxt).directory = null_mut();

    /* Allocate the Node stack */
    if (*ctxt).node_tab.is_null() {
        (*ctxt).node_tab = xml_malloc(10 * size_of::<XmlNodePtr>()) as _;
        (*ctxt).node_max = 10;
    }
    if (*ctxt).node_tab.is_null() {
        xml_err_memory(
            null_mut(),
            c"cannot initialize parser context\n".as_ptr() as _,
        );
        (*ctxt).node_nr = 0;
        (*ctxt).node_max = 0;
        (*ctxt).node = null_mut();
        (*ctxt).input_nr = 0;
        (*ctxt).input_max = 0;
        (*ctxt).input = null_mut();
        return -1;
    }
    (*ctxt).node_nr = 0;
    (*ctxt).node = null_mut();

    /* Allocate the Name stack */
    if (*ctxt).name_tab.is_null() {
        (*ctxt).name_tab = xml_malloc(10 * size_of::<*mut XmlChar>()) as _;
        (*ctxt).name_max = 10;
    }
    if (*ctxt).name_tab.is_null() {
        xml_err_memory(
            null_mut(),
            c"cannot initialize parser context\n".as_ptr() as _,
        );
        (*ctxt).node_nr = 0;
        (*ctxt).node_max = 0;
        (*ctxt).node = null_mut();
        (*ctxt).input_nr = 0;
        (*ctxt).input_max = 0;
        (*ctxt).input = null_mut();
        (*ctxt).name_nr = 0;
        (*ctxt).name_max = 0;
        (*ctxt).name = null_mut();
        return -1;
    }
    (*ctxt).name_nr = 0;
    (*ctxt).name = null_mut();

    /* Allocate the space stack */
    if (*ctxt).space_tab.is_null() {
        (*ctxt).space_tab = xml_malloc(10 * size_of::<c_int>()) as _;
        (*ctxt).space_max = 10;
    }
    if (*ctxt).space_tab.is_null() {
        xml_err_memory(
            null_mut(),
            c"cannot initialize parser context\n".as_ptr() as _,
        );
        (*ctxt).node_nr = 0;
        (*ctxt).node_max = 0;
        (*ctxt).node = null_mut();
        (*ctxt).input_nr = 0;
        (*ctxt).input_max = 0;
        (*ctxt).input = null_mut();
        (*ctxt).name_nr = 0;
        (*ctxt).name_max = 0;
        (*ctxt).name = null_mut();
        (*ctxt).space_nr = 0;
        (*ctxt).space_max = 0;
        (*ctxt).space = null_mut();
        return -1;
    }
    (*ctxt).space_nr = 1;
    (*ctxt).space_max = 10;
    *(*ctxt).space_tab.add(0) = -1;
    (*ctxt).space = (*ctxt).space_tab.add(0);
    (*ctxt).my_doc = null_mut();
    (*ctxt).well_formed = 1;
    (*ctxt).ns_well_formed = 1;
    (*ctxt).valid = 1;
    (*ctxt).loadsubset = *xml_load_ext_dtd_default_value();
    if (*ctxt).loadsubset != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseDtdload as i32;
    }
    (*ctxt).validate = *xml_do_validity_checking_default_value();
    (*ctxt).pedantic = *xml_pedantic_parser_default_value();
    if (*ctxt).pedantic != 0 {
        (*ctxt).options |= XmlParserOption::XmlParsePedantic as i32;
    }
    (*ctxt).linenumbers = *xml_line_numbers_default_value();
    (*ctxt).keep_blanks = *xml_keep_blanks_default_value();
    if (*ctxt).keep_blanks == 0 {
        (*(*ctxt).sax).ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
        (*ctxt).options |= XmlParserOption::XmlParseNoblanks as i32;
    }

    (*ctxt).vctxt.flags = XML_VCTXT_USE_PCTXT as _;
    (*ctxt).vctxt.user_data = ctxt as _;
    (*ctxt).vctxt.error = Some(xml_parser_validity_error);
    (*ctxt).vctxt.warning = Some(xml_parser_validity_warning);
    if (*ctxt).validate != 0 {
        if *xml_get_warnings_default_value() == 0 {
            (*ctxt).vctxt.warning = None;
        } else {
            (*ctxt).vctxt.warning = Some(xml_parser_validity_warning);
        }
        (*ctxt).vctxt.node_max = 0;
        (*ctxt).options |= XmlParserOption::XmlParseDtdvalid as i32;
    }
    (*ctxt).replace_entities = *xml_substitute_entities_default_value();
    if (*ctxt).replace_entities != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseNoent as i32;
    }
    (*ctxt).record_info = 0;
    (*ctxt).check_index = 0;
    (*ctxt).in_subset = 0;
    (*ctxt).err_no = XmlParserErrors::XmlErrOK as i32;
    (*ctxt).depth = 0;
    (*ctxt).charset = XmlCharEncoding::XmlCharEncodingUtf8 as i32;
    (*ctxt).catalogs = null_mut();
    (*ctxt).sizeentities = 0;
    (*ctxt).sizeentcopy = 0;
    (*ctxt).input_id = 1;
    xml_init_node_info_seq(addr_of_mut!((*ctxt).node_seq));
    0
}

/**
 * xmlNewSAXParserCtxt:
 * @sax:  SAX handler
 * @userData:  user data
 *
 * Allocate and initialize a new SAX parser context. If userData is NULL,
 * the parser context will be passed as user data.
 *
 * Returns the xmlParserCtxtPtr or NULL if memory allocation failed.
 */
pub unsafe extern "C" fn xml_new_sax_parser_ctxt(
    sax: *const XmlSAXHandler,
    user_data: *mut c_void,
) -> XmlParserCtxtPtr {
    let ctxt: XmlParserCtxtPtr = xml_malloc(size_of::<XmlParserCtxt>()) as XmlParserCtxtPtr;
    if ctxt.is_null() {
        xml_err_memory(
            null_mut(),
            c"cannot allocate parser context\n".as_ptr() as _,
        );
        return null_mut();
    }
    memset(ctxt as _, 0, size_of::<XmlParserCtxt>());
    if xml_init_sax_parser_ctxt(ctxt, sax, user_data) < 0 {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }
    ctxt as _
}

/**
 * xmlInitParserCtxt:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function which will be made private in a future
 * version.
 *
 * Initialize a parser context
 *
 * Returns 0 in case of success and -1 in case of error
 */
pub(crate) unsafe extern "C" fn xml_init_parser_ctxt(ctxt: XmlParserCtxtPtr) -> c_int {
    xml_init_sax_parser_ctxt(ctxt, null_mut(), null_mut())
}

/**
 * xmlClearParserCtxt:
 * @ctxt:  an XML parser context
 *
 * Clear (release owned resources) and reinitialize a parser context
 */
pub unsafe extern "C" fn xml_clear_parser_ctxt(ctxt: XmlParserCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    xml_clear_node_info_seq(addr_of_mut!((*ctxt).node_seq));
    xml_ctxt_reset(ctxt);
}

/**
 * xmlFreeParserCtxt:
 * @ctxt:  an XML parser context
 *
 * Free all the memory used by a parser context. However the parsed
 * document in (*ctxt).myDoc is not freed.
 */
pub unsafe extern "C" fn xml_free_parser_ctxt(ctxt: XmlParserCtxtPtr) {
    let mut input: XmlParserInputPtr;

    if ctxt.is_null() {
        return;
    }

    while {
        input = input_pop(ctxt);
        !input.is_null()
    } {
        /* Non consuming */
        xml_free_input_stream(input);
    }
    if !(*ctxt).space_tab.is_null() {
        xml_free((*ctxt).space_tab as _);
    }
    if !(*ctxt).name_tab.is_null() {
        xml_free((*ctxt).name_tab as _);
    }
    if !(*ctxt).node_tab.is_null() {
        xml_free((*ctxt).node_tab as _);
    }
    if !(*ctxt).node_info_tab.is_null() {
        xml_free((*ctxt).node_info_tab as _);
    }
    if !(*ctxt).input_tab.is_null() {
        xml_free((*ctxt).input_tab as _);
    }
    if !(*ctxt).version.is_null() {
        xml_free((*ctxt).version as _);
    }
    if !(*ctxt).encoding.is_null() {
        xml_free((*ctxt).encoding as _);
    }
    if !(*ctxt).ext_sub_uri.is_null() {
        xml_free((*ctxt).ext_sub_uri as _);
    }
    if !(*ctxt).ext_sub_system.is_null() {
        xml_free((*ctxt).ext_sub_system as _);
    }
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
    if !(*ctxt).directory.is_null() {
        xml_free((*ctxt).directory as _);
    }
    if !(*ctxt).vctxt.node_tab.is_null() {
        xml_free((*ctxt).vctxt.node_tab as _);
    }
    if !(*ctxt).atts.is_null() {
        xml_free((*ctxt).atts as _);
    }
    if !(*ctxt).dict.is_null() {
        xml_dict_free((*ctxt).dict);
    }
    if !(*ctxt).ns_tab.is_null() {
        xml_free((*ctxt).ns_tab as _);
    }
    if !(*ctxt).push_tab.is_null() {
        xml_free((*ctxt).push_tab as _);
    }
    if !(*ctxt).attallocs.is_null() {
        xml_free((*ctxt).attallocs as _);
    }
    if !(*ctxt).atts_default.is_null() {
        xml_hash_free((*ctxt).atts_default, Some(xml_hash_default_deallocator));
    }
    if !(*ctxt).atts_special.is_null() {
        xml_hash_free((*ctxt).atts_special, None);
    }
    if !(*ctxt).free_elems.is_null() {
        let mut cur: XmlNodePtr;
        let mut next: XmlNodePtr;

        cur = (*ctxt).free_elems;
        while !cur.is_null() {
            next = (*cur).next;
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
    /*
     * cleanup the error strings
     */
    if !(*ctxt).last_error.message.is_null() {
        xml_free((*ctxt).last_error.message as _);
    }
    if !(*ctxt).last_error.file.is_null() {
        xml_free((*ctxt).last_error.file as _);
    }
    if !(*ctxt).last_error.str1.is_null() {
        xml_free((*ctxt).last_error.str1 as _);
    }
    if !(*ctxt).last_error.str2.is_null() {
        xml_free((*ctxt).last_error.str2 as _);
    }
    if !(*ctxt).last_error.str3.is_null() {
        xml_free((*ctxt).last_error.str3 as _);
    }

    #[cfg(feature = "catalog")]
    {
        if !(*ctxt).catalogs.is_null() {
            xml_catalog_free_local((*ctxt).catalogs);
        }
    }
    xml_free(ctxt as _);
}

/**
 * xmlSetupParserForBuffer:
 * @ctxt:  an XML parser context
 * @buffer:  a XmlChar * buffer
 * @filename:  a file name
 *
 * DEPRECATED: Don't use.
 *
 * Setup the parser context to parse a new buffer; Clears any prior
 * contents from the parser context. The buffer parameter must not be
 * NULL, but the filename parameter can be
 */
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
        xml_err_memory(
            null_mut(),
            c"parsing new buffer: out of memory\n".as_ptr() as _,
        );
        xml_clear_parser_ctxt(ctxt);
        return;
    }

    xml_clear_parser_ctxt(ctxt);
    if !filename.is_null() {
        (*input).filename = xml_canonic_path(filename as _) as _;
    }
    (*input).base = buffer;
    (*input).cur = buffer;
    (*input).end = buffer.add(xml_strlen(buffer as _) as _);
    input_push(ctxt, input);
}

/**
 * xmlCreateDocParserCtxt:
 * @cur:  a pointer to an array of XmlChar
 *
 * Creates a parser context for an XML in-memory document.
 *
 * Returns the new parser context or NULL
 */
pub unsafe extern "C" fn xml_create_doc_parser_ctxt(cur: *const XmlChar) -> XmlParserCtxtPtr {
    if cur.is_null() {
        return null_mut();
    }
    let len: c_int = xml_strlen(cur);
    xml_create_memory_parser_ctxt(cur as _, len)
}

#[cfg(feature = "legacy")]
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

/*
 * Reading/setting optional parsing features.
 */
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
#[cfg(feature = "legacy")]
pub unsafe extern "C" fn xml_get_features_list(
    len: *mut c_int,
    result: *mut *const c_char,
) -> c_int {
    let ret: c_int = XML_FEATURES_LIST.len() as i32;
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
#[cfg(feature = "legacy")]
pub unsafe extern "C" fn xml_get_feature(
    ctxt: XmlParserCtxtPtr,
    name: *const c_char,
    result: *mut c_void,
) -> c_int {
    if ctxt.is_null() || name.is_null() || result.is_null() {
        return -1;
    }

    if strcmp(name, c"validate".as_ptr() as _) == 0 {
        *(result as *mut c_int) = (*ctxt).validate;
    } else if strcmp(name, c"keep blanks".as_ptr() as _) == 0 {
        *(result as *mut c_int) = (*ctxt).keep_blanks;
    } else if strcmp(name, c"disable SAX".as_ptr() as _) == 0 {
        *(result as *mut c_int) = (*ctxt).disable_sax;
    } else if strcmp(name, c"fetch external entities".as_ptr() as _) == 0 {
        *(result as *mut c_int) = (*ctxt).loadsubset;
    } else if strcmp(name, c"substitute entities".as_ptr() as _) == 0 {
        *(result as *mut c_int) = (*ctxt).replace_entities;
    } else if strcmp(name, c"gather line info".as_ptr() as _) == 0 {
        *(result as *mut c_int) = (*ctxt).record_info;
    } else if strcmp(name, c"user data".as_ptr() as _) == 0 {
        *(result as *mut *mut c_void) = (*ctxt).user_data;
    } else if strcmp(name, c"is html".as_ptr() as _) == 0 {
        *(result as *mut c_int) = (*ctxt).html;
    } else if strcmp(name, c"is standalone".as_ptr() as _) == 0 {
        *(result as *mut c_int) = (*ctxt).standalone;
    } else if strcmp(name, c"document".as_ptr() as _) == 0 {
        *(result as *mut XmlDocPtr) = (*ctxt).my_doc;
    } else if strcmp(name, c"is well formed".as_ptr() as _) == 0 {
        *(result as *mut c_int) = (*ctxt).well_formed;
    } else if strcmp(name, c"is valid".as_ptr() as _) == 0 {
        *(result as *mut c_int) = (*ctxt).valid;
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
        *(result as *mut CdataBlockSAXFunc) = (*(*ctxt).sax).cdata_block.unwrap();
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
#[cfg(feature = "legacy")]
pub unsafe extern "C" fn xml_set_feature(
    ctxt: XmlParserCtxtPtr,
    name: *const c_char,
    value: *mut c_void,
) -> c_int {
    if ctxt.is_null() || name.is_null() || value.is_null() {
        return -1;
    }

    if strcmp(name, c"validate".as_ptr() as _) == 0 {
        let newvalidate: c_int = *(value as *mut c_int);

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
        (*ctxt).keep_blanks = *(value as *mut c_int);
    } else if strcmp(name, c"disable SAX".as_ptr() as _) == 0 {
        (*ctxt).disable_sax = *(value as *mut c_int);
    } else if strcmp(name, c"fetch external entities".as_ptr() as _) == 0 {
        (*ctxt).loadsubset = *(value as *mut c_int);
    } else if strcmp(name, c"substitute entities".as_ptr() as _) == 0 {
        (*ctxt).replace_entities = *(value as *mut c_int);
    } else if strcmp(name, c"gather line info".as_ptr() as _) == 0 {
        (*ctxt).record_info = *(value as *mut c_int);
    } else if strcmp(name, c"user data".as_ptr() as _) == 0 {
        (*ctxt).user_data = *(value as *mut *mut c_void);
    } else if strcmp(name, c"is html".as_ptr() as _) == 0 {
        (*ctxt).html = *(value as *mut c_int);
    } else if strcmp(name, c"is standalone".as_ptr() as _) == 0 {
        (*ctxt).standalone = *(value as *mut c_int);
    } else if strcmp(name, c"document".as_ptr() as _) == 0 {
        (*ctxt).my_doc = *(value as *mut XmlDocPtr);
    } else if strcmp(name, c"is well formed".as_ptr() as _) == 0 {
        (*ctxt).well_formed = *(value as *mut c_int);
    } else if strcmp(name, c"is valid".as_ptr() as _) == 0 {
        (*ctxt).valid = *(value as *mut c_int);
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
        (*(*ctxt).sax).cdata_block = Some(*(value as *mut CdataBlockSAXFunc));
    } else if strcmp(name, c"SAX function externalSubset".as_ptr() as _) == 0 {
        (*(*ctxt).sax).external_subset = Some(*(value as *mut ExternalSubsetSAXFunc));
    } else {
        return -1;
    }
    0
}

/*
 * Interfaces for the Push mode.
 */
/**
 * xmlCreatePushParserCtxt:
 * @sax:  a SAX handler
 * @user_data:  The user data returned on SAX callbacks
 * @chunk:  a pointer to an array of chars
 * @size:  number of chars in the array
 * @filename:  an optional file name or URI
 *
 * Create a parser context for using the XML parser in push mode.
 * If @buffer and @size are non-NULL, the data is used to detect
 * the encoding.  The remaining characters will be parsed so they
 * don't need to be fed in again through xmlParseChunk.
 * To allow content encoding detection, @size should be >= 4
 * The value of @filename is used for fetching external entities
 * and error/warning reports.
 *
 * Returns the new parser context or NULL
 */
#[cfg(feature = "push")]
pub unsafe extern "C" fn xml_create_push_parser_ctxt(
    sax: XmlSAXHandlerPtr,
    user_data: *mut c_void,
    chunk: *const c_char,
    size: c_int,
    filename: *const c_char,
) -> XmlParserCtxtPtr {
    let buf: XmlParserInputBufferPtr =
        xml_alloc_parser_input_buffer(XmlCharEncoding::XmlCharEncodingNone);
    if buf.is_null() {
        return null_mut();
    }

    let ctxt: XmlParserCtxtPtr = xml_new_sax_parser_ctxt(sax, user_data);
    if ctxt.is_null() {
        xml_err_memory(
            null_mut(),
            c"creating parser: out of memory\n".as_ptr() as _,
        );
        xml_free_parser_input_buffer(buf);
        return null_mut();
    }
    (*ctxt).dict_names = 1;
    if filename.is_null() {
        (*ctxt).directory = null_mut();
    } else {
        (*ctxt).directory = xml_parser_get_directory(filename);
    }

    let input_stream: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input_stream.is_null() {
        xml_free_parser_ctxt(ctxt);
        xml_free_parser_input_buffer(buf);
        return null_mut();
    }

    if filename.is_null() {
        (*input_stream).filename = null_mut();
    } else {
        (*input_stream).filename = xml_canonic_path(filename as _) as _;
        if (*input_stream).filename.is_null() {
            xml_free_input_stream(input_stream);
            xml_free_parser_ctxt(ctxt);
            xml_free_parser_input_buffer(buf);
            return null_mut();
        }
    }
    (*input_stream).buf = buf;
    xml_buf_reset_input(
        (*(*input_stream).buf)
            .buffer
            .map_or(null_mut(), |buf| buf.as_ptr()),
        input_stream,
    );
    input_push(ctxt, input_stream);

    /*
     * If the caller didn't provide an initial 'chunk' for determining
     * the encoding, we set the context to xmlCharEncoding::XML_CHAR_ENCODING_NONE so
     * that it can be automatically determined later
     */
    (*ctxt).charset = XmlCharEncoding::XmlCharEncodingNone as i32;

    if size != 0 && !chunk.is_null() && !(*ctxt).input.is_null() && !(*(*ctxt).input).buf.is_null()
    {
        let base: size_t = xml_buf_get_input_base(
            (*(*(*ctxt).input).buf)
                .buffer
                .map_or(null_mut(), |buf| buf.as_ptr()),
            (*ctxt).input,
        );
        let cur: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

        xml_parser_input_buffer_push((*(*ctxt).input).buf, size, chunk);

        xml_buf_set_input_base_cur(
            (*(*(*ctxt).input).buf)
                .buffer
                .map_or(null_mut(), |buf| buf.as_ptr()),
            (*ctxt).input,
            base,
            cur,
        );
    }

    ctxt
}

const XML_PARSER_BIG_BUFFER_SIZE: usize = 300;
const XML_PARSER_BUFFER_SIZE: usize = 100;

/**
 * xmlParseLookupString:
 * @ctxt:  an XML parser context
 * @startDelta: delta to apply at the start
 * @str:  string
 * @strLen:  length of string
 *
 * Check whether the input buffer contains a string.
 */
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

/**
 * xmlParseLookupGt:
 * @ctxt:  an XML parser context
 *
 * Check whether there's enough data in the input buffer to finish parsing
 * a start tag. This has to take quotes into account.
 */
unsafe extern "C" fn xml_parse_lookup_gt(ctxt: XmlParserCtxtPtr) -> c_int {
    let mut cur: *const XmlChar;
    let end: *const XmlChar = (*(*ctxt).input).end;
    let mut state: c_int = (*ctxt).end_check_state;

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

pub(crate) unsafe extern "C" fn space_push(ctxt: XmlParserCtxtPtr, val: c_int) -> c_int {
    if (*ctxt).space_nr >= (*ctxt).space_max {
        (*ctxt).space_max *= 2;
        let tmp: *mut c_int = xml_realloc(
            (*ctxt).space_tab as _,
            (*ctxt).space_max as usize * size_of_val(&*(*ctxt).space_tab.add(0)),
        ) as _;
        if tmp.is_null() {
            xml_err_memory(ctxt, null());
            (*ctxt).space_max /= 2;
            return -1;
        }
        (*ctxt).space_tab = tmp;
    }
    *(*ctxt).space_tab.add((*ctxt).space_nr as usize) = val;
    (*ctxt).space = (*ctxt).space_tab.add((*ctxt).space_nr as usize);
    let res = (*ctxt).space_nr;
    (*ctxt).space_nr += 1;
    res
}

/*
 * The two following functions are related to the change of accepted
 * characters for Name and NmToken in the Revision 5 of XML-1.0
 * They correspond to the modified production [4] and the new production [4a]
 * changes in that revision. Also note that the macros used for the
 * productions Letter, Digit, CombiningChar and Extender are not needed
 * anymore.
 * We still keep compatibility to pre-revision5 parsing semantic if the
 * new XML_PARSE_OLD10 option is given to the parser.
 */
unsafe extern "C" fn xml_is_name_start_char(ctxt: XmlParserCtxtPtr, c: c_int) -> c_int {
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
    } else if IS_LETTER!(c as u32) || c == b'_' as i32 || c == b':' as i32 {
        return 1;
    }
    0
}

unsafe extern "C" fn xml_parse_ncname_complex(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    let mut len: c_int = 0;
    let mut l: c_int = 0;
    let mut c: i32;
    let max_length: c_int = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };

    // #ifdef DEBUG
    //     nbParseNCNameComplex++;
    // #endif

    /*
     * Handler for more complex cases
     */
    let start_position: size_t = CUR_PTR!(ctxt).offset_from(BASE_PTR!(ctxt)) as _;
    c = CUR_CHAR!(ctxt, l);
    if c == b' ' as i32
        || c == b'>' as i32
        || c == b'/' as i32 /* accelerators */
        || (xml_is_name_start_char(ctxt, c) == 0 || c == b':' as i32)
    {
        return null_mut();
    }

    while c != b' ' as i32
        && c != b'>' as i32
        && c != b'/' as i32 /* test bigname.xml */
        && (xml_is_name_char(ctxt, c) != 0 && c != b':' as i32)
    {
        if len <= i32::MAX - l {
            len += l;
        }
        NEXTL!(ctxt, l);
        c = CUR_CHAR!(ctxt, l);
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }
    if len > max_length {
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrNameTooLong,
            c"NCName".as_ptr() as _,
        );
        return null_mut();
    }
    xml_dict_lookup((*ctxt).dict, BASE_PTR!(ctxt).add(start_position), len)
}

/**
 * xmlParseNCName:
 * @ctxt:  an XML parser context
 * @len:  length of the string parsed
 *
 * parse an XML name.
 *
 * [4NS] NCNameChar ::= Letter | Digit | '.' | '-' | '_' |
 *                      CombiningChar | Extender
 *
 * [5NS] NCName ::= (Letter | '_') (NCNameChar)*
 *
 * Returns the Name parsed or NULL
 */
unsafe extern "C" fn xml_parse_ncname(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    let mut input: *const XmlChar;

    let ret: *const XmlChar;
    let count: size_t;
    let max_length: size_t = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH
    } else {
        XML_MAX_NAME_LENGTH
    };

    // #ifdef DEBUG
    //     nbParseNCName++;
    // #endif

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
                xml_fatal_err(
                    ctxt,
                    XmlParserErrors::XmlErrNameTooLong,
                    c"NCName".as_ptr() as _,
                );
                return null_mut();
            }
            ret = xml_dict_lookup((*ctxt).dict, (*(*ctxt).input).cur, count as _);
            (*(*ctxt).input).cur = input;
            (*(*ctxt).input).col += count as i32;
            if ret.is_null() {
                xml_err_memory(ctxt, null());
            }
            return ret;
        }
    }
    xml_parse_ncname_complex(ctxt)
}

/**
 * xmlParseQName:
 * @ctxt:  an XML parser context
 * @prefix:  pointer to store the prefix part
 *
 * parse an XML Namespace QName
 *
 * [6]  QName  ::= (Prefix ':')? LocalPart
 * [7]  Prefix  ::= NCName
 * [8]  LocalPart  ::= NCName
 *
 * Returns the Name parsed or NULL
 */
unsafe extern "C" fn xml_parse_qname(
    ctxt: XmlParserCtxtPtr,
    prefix: *mut *const XmlChar,
) -> *const XmlChar {
    let mut l: *const XmlChar;
    let mut p: *const XmlChar;

    GROW!(ctxt);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }

    l = xml_parse_ncname(ctxt);
    if l.is_null() {
        if CUR!(ctxt) == b':' {
            l = xml_parse_name(ctxt);
            if !l.is_null() {
                xml_ns_err(
                    ctxt,
                    XmlParserErrors::XmlNsErrQname,
                    c"Failed to parse QName '%s'\n".as_ptr() as _,
                    l,
                    null(),
                    null(),
                );
                *prefix = null_mut();
                return l;
            }
        }
        return null_mut();
    }
    if CUR!(ctxt) == b':' {
        NEXT!(ctxt);
        p = l;
        l = xml_parse_ncname(ctxt);
        if l.is_null() {
            let tmp: *mut XmlChar;

            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                return null_mut();
            }
            xml_ns_err(
                ctxt,
                XmlParserErrors::XmlNsErrQname,
                c"Failed to parse QName '%s:'\n".as_ptr() as _,
                p,
                null(),
                null(),
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
        if CUR!(ctxt) == b':' {
            let mut tmp: *mut XmlChar;

            xml_ns_err(
                ctxt,
                XmlParserErrors::XmlNsErrQname,
                c"Failed to parse QName '%s:%s:'\n".as_ptr(),
                p,
                l,
                null(),
            );
            NEXT!(ctxt);
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

/*
 * Constants for protection against abusive entity expansion
 * ("billion laughs").
 */

/*
 * XML_PARSER_NON_LINEAR is roughly the maximum allowed amplification factor
 * of serialized output after entity expansion.
 */
const XML_PARSER_NON_LINEAR: usize = 5;

/*
 * A certain amount is always allowed.
 */
const XML_PARSER_ALLOWED_EXPANSION: usize = 1000000;

/*
 * Fixed cost for each entity reference. This crudely models processing time
 * as well to protect, for example, against exponential expansion of empty
 * or very short entities.
 */
const XML_ENT_FIXED_COST: usize = 20;

/**
 * xmlParserEntityCheck:
 * @ctxt:  parser context
 * @extra:  sum of unexpanded entity sizes
 *
 * Check for non-linear entity expansion behaviour.
 *
 * In some cases like xmlStringDecodeEntities, this function is called
 * for each, possibly nested entity and its unexpanded content length.
 *
 * In other cases like xmlParseReference, it's only called for each
 * top-level entity with its unexpanded content length plus the sum of
 * the unexpanded content lengths (plus fixed cost) of all nested
 * entities.
 *
 * Summing the unexpanded lengths also adds the length of the reference.
 * This is by design. Taking the length of the entity name into account
 * discourages attacks that try to waste CPU time with abusively long
 * entity names. See test/recurse/lol6.xml for example. Each call also
 * adds some fixed cost XML_ENT_FIXED_COST to discourage attacks with
 * short entities.
 *
 * Returns 1 on error, 0 on success.
 */
pub(crate) unsafe extern "C" fn xml_parser_entity_check(
    ctxt: XmlParserCtxtPtr,
    extra: c_ulong,
) -> c_int {
    let mut consumed: c_ulong;
    let input: XmlParserInputPtr = (*ctxt).input;
    let entity: XmlEntityPtr = (*input).entity;

    /*
     * Compute total consumed bytes so far, including input streams of
     * external entities.
     */
    consumed = (*input).parent_consumed;
    if entity.is_null()
        || (matches!(
            (*entity).etype,
            Some(XmlEntityType::XmlExternalParameterEntity)
        ) && (*entity).flags & XML_ENT_PARSED as i32 == 0)
    {
        xml_saturated_add(addr_of_mut!(consumed), (*input).consumed);
        xml_saturated_add_size_t(
            addr_of_mut!(consumed),
            (*input).cur.offset_from((*input).base) as _,
        );
    }
    xml_saturated_add(addr_of_mut!(consumed), (*ctxt).sizeentities);

    /*
     * Add extra cost and some fixed cost.
     */
    xml_saturated_add(addr_of_mut!((*ctxt).sizeentcopy), extra);
    xml_saturated_add(addr_of_mut!((*ctxt).sizeentcopy), XML_ENT_FIXED_COST as _);

    /*
     * It's important to always use saturation arithmetic when tracking
     * entity sizes to make the size checks reliable. If "sizeentcopy"
     * overflows, we have to abort.
     */
    if (*ctxt).sizeentcopy > XML_PARSER_ALLOWED_EXPANSION as u64
        && ((*ctxt).sizeentcopy == u64::MAX
            || (*ctxt).sizeentcopy / XML_PARSER_NON_LINEAR as u64 > consumed)
    {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrEntityLoop,
            c"Maximum entity amplification factor exceeded".as_ptr() as _,
        );
        xml_halt_parser(ctxt);
        return 1;
    }

    0
}

/**
 * xmlParseStringCharRef:
 * @ctxt:  an XML parser context
 * @str:  a pointer to an index in the string
 *
 * parse Reference declarations, variant parsing from a string rather
 * than an an input flow.
 *
 * [66] CharRef ::= '&#' [0-9]+ ';' |
 *                  '&#x' [0-9a-fA-F]+ ';'
 *
 * [ WFC: Legal Character ]
 * Characters referred to using character references must match the
 * production for Char.
 *
 * Returns the value parsed (as an c_int), 0 in case of error, str will be
 *         updated to the current value of the index
 */
unsafe extern "C" fn xml_parse_string_char_ref(
    ctxt: XmlParserCtxtPtr,
    str: *mut *const XmlChar,
) -> c_int {
    let mut ptr: *const XmlChar;
    let mut cur: XmlChar;
    let mut val: c_int = 0;

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
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidHexCharRef, null());
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
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidDecCharRef, null());
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
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidCharRef, null());
        return 0;
    }
    *str = ptr;

    /*
     * [ WFC: Legal Character ]
     * Characters referred to using character references must match the
     * production for Char.
     */
    if val >= 0x110000 {
        xml_fatal_err_msg_int(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            c"xmlParseStringCharRef: character reference out of bounds\n".as_ptr() as _,
            val,
        );
    } else if IS_CHAR!(val) {
        return val;
    } else {
        xml_fatal_err_msg_int(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            c"xmlParseStringCharRef: invalid xmlChar value %d\n".as_ptr() as _,
            val,
        );
    }
    0
}

/**
 * xmlParseStringName:
 * @ctxt:  an XML parser context
 * @str:  a pointer to the string pointer (IN/OUT)
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
 * Returns the Name parsed or NULL. The @str pointer
 * is updated to the current location in the string.
 */
pub(crate) unsafe extern "C" fn xml_parse_string_name(
    ctxt: XmlParserCtxtPtr,
    str: *mut *const XmlChar,
) -> *mut XmlChar {
    let mut buf: [XmlChar; XML_MAX_NAMELEN + 5] = [0; XML_MAX_NAMELEN + 5];
    let mut cur: *const XmlChar = *str;
    let mut len: c_int = 0;
    let mut l: c_int = 0;
    let mut c: c_int;
    let max_length: c_int = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };

    // #ifdef DEBUG
    //     nbParseStringName++;
    // #endif

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
            let mut max: c_int = len * 2;

            buffer = xml_malloc_atomic(max as usize) as _;
            if buffer.is_null() {
                xml_err_memory(ctxt, null());
                return null_mut();
            }
            memcpy(buffer as _, buf.as_ptr() as _, len as _);
            while xml_is_name_char(ctxt, c) != 0 {
                if len + 10 > max {
                    max *= 2;
                    let tmp: *mut XmlChar = xml_realloc(buffer as _, max as usize) as _;
                    if tmp.is_null() {
                        xml_err_memory(ctxt, null());
                        xml_free(buffer as _);
                        return null_mut();
                    }
                    buffer = tmp;
                }
                COPY_BUF!(l, buffer, len, c);
                cur = cur.add(l as usize);
                c = CUR_SCHAR!(ctxt, cur, l);
                if len > max_length {
                    xml_fatal_err(
                        ctxt,
                        XmlParserErrors::XmlErrNameTooLong,
                        c"NCName".as_ptr() as _,
                    );
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
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrNameTooLong,
            c"NCName".as_ptr() as _,
        );
        return null_mut();
    }
    *str = cur;
    xml_strndup(buf.as_ptr() as _, len)
}

/**
 * xmlParseStringEntityRef:
 * @ctxt:  an XML parser context
 * @str:  a pointer to an index in the string
 *
 * parse ENTITY references declarations, but this version parses it from
 * a string value.
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
 * Returns the xmlEntityPtr if found, or NULL otherwise. The str pointer
 * is updated to the current location in the string.
 */
unsafe extern "C" fn xml_parse_string_entity_ref(
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
            c"xmlParseStringEntityRef: no name\n".as_ptr() as _,
        );
        *str = ptr;
        return null_mut();
    }
    if *ptr != b';' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityRefSemicolMissing, null());
        xml_free(name as _);
        *str = ptr;
        return null_mut();
    }
    ptr = ptr.add(1);

    /*
     * Predefined entities override any extra definition
     */
    if (*ctxt).options & XmlParserOption::XmlParseOldsax as i32 == 0 {
        ent = xml_get_predefined_entity(name);
        if !ent.is_null() {
            xml_free(name as _);
            *str = ptr;
            return ent;
        }
    }

    /*
     * Ask first SAX for entity resolution, otherwise try the
     * entities which may have stored in the parser context.
     */
    if !(*ctxt).sax.is_null() {
        if let Some(get_entity) = (*(*ctxt).sax).get_entity {
            ent = get_entity((*ctxt).user_data, name);
        }
        if ent.is_null() && (*ctxt).options & XmlParserOption::XmlParseOldsax as i32 != 0 {
            ent = xml_get_predefined_entity(name);
        }
        if ent.is_null() && (*ctxt).user_data == ctxt as _ {
            ent = xml_sax2_get_entity(ctxt as _, name);
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        xml_free(name as _);
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
        }
    /* TODO ? check regressions (*ctxt).valid = 0; */
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
                && !xml_strchr((*ent).content.load(Ordering::Relaxed), b'<').is_null()
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

    xml_free(name as _);
    *str = ptr;
    ent
}

/**
 * xmlParseStringPEReference:
 * @ctxt:  an XML parser context
 * @str:  a pointer to an index in the string
 *
 * parse PEReference declarations
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
 * Returns the string of the entity content.
 *         str is updated to the current value of the index
 */
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
            c"xmlParseStringPEReference: no name\n".as_ptr() as _,
        );
        *str = ptr;
        return null_mut();
    }
    cur = *ptr;
    if cur != b';' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityRefSemicolMissing, null());
        xml_free(name as _);
        *str = ptr;
        return null_mut();
    }
    ptr = ptr.add(1);

    /*
     * Request the entity from SAX
     */
    if !(*ctxt).sax.is_null() && (*(*ctxt).sax).get_parameter_entity.is_some() {
        entity = ((*(*ctxt).sax).get_parameter_entity.unwrap())((*ctxt).user_data, name);
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        xml_free(name as _);
        *str = ptr;
        return null_mut();
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
            xml_warning_msg(
                ctxt,
                XmlParserErrors::XmlWarUndeclaredEntity,
                c"PEReference: %%%s; not found\n".as_ptr() as _,
                name,
                null(),
            );
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
                c"%%%s; is not a parameter entity\n".as_ptr() as _,
                name,
                null(),
            );
        }
    }
    (*ctxt).has_perefs = 1;
    xml_free(name as _);
    *str = ptr;
    entity
}

/**
 * xmlPopInput:
 * @ctxt:  an XML parser context
 *
 * xmlPopInput: the current input pointed by (*ctxt).input came to an end
 *          pop it and return the next c_char.
 *
 * Returns the current XmlChar in the parser context
 */
pub unsafe extern "C" fn xml_pop_input(ctxt: XmlParserCtxtPtr) -> XmlChar {
    if ctxt.is_null() || (*ctxt).input_nr <= 1 {
        return 0;
    }
    if *xml_parser_debug_entities() != 0 {
        generic_error!("Popping input {}\n", (*ctxt).input_nr);
    }
    if (*ctxt).input_nr > 1
        && (*ctxt).in_subset == 0
        && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            c"Unfinished entity outside the DTD".as_ptr() as _,
        );
    }
    let input: XmlParserInputPtr = input_pop(ctxt);
    if !(*input).entity.is_null() {
        (*(*input).entity).flags &= !XML_ENT_EXPANDING as i32;
    }
    xml_free_input_stream(input);
    if *(*(*ctxt).input).cur == 0 {
        xml_parser_grow(ctxt);
    }
    CUR!(ctxt)
}

/**
 * xmlLoadEntityContent:
 * @ctxt:  an XML parser context
 * @entity: an unloaded system entity
 *
 * Load the original content of the given system entity from the
 * ExternalID/SystemID given. This is to be used for Included in Literal
 * http://www.w3.org/TR/REC-xml/#inliteral processing of entities references
 *
 * Returns 0 in case of success and -1 in case of failure
 */
unsafe extern "C" fn xml_load_entity_content(
    ctxt: XmlParserCtxtPtr,
    entity: XmlEntityPtr,
) -> c_int {
    let mut l: c_int = 0;
    let mut c: c_int;

    if ctxt.is_null()
        || entity.is_null()
        || !matches!(
            (*entity).etype,
            Some(XmlEntityType::XmlExternalParameterEntity)
                | Some(XmlEntityType::XmlExternalGeneralParsedEntity)
        )
        || !(*entity).content.load(Ordering::Relaxed).is_null()
    {
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            c"xmlLoadEntityContent parameter error".as_ptr() as _,
        );
        return -1;
    }

    if *xml_parser_debug_entities() != 0 {
        generic_error!(
            "Reading {} entity content input\n",
            CStr::from_ptr((*entity).name.load(Ordering::Relaxed) as *const i8).to_string_lossy()
        );
    }

    // let buf: XmlBufferPtr = xml_buffer_create();
    let buf = xml_buf_create();
    if buf.is_null() {
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            c"xmlLoadEntityContent parameter error".as_ptr() as _,
        );
        return -1;
    }
    // xml_buffer_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);

    let input: XmlParserInputPtr = xml_new_entity_input_stream(ctxt, entity);
    if input.is_null() {
        xml_fatal_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            c"xmlLoadEntityContent input error".as_ptr() as _,
        );
        // xml_buffer_free(buf);
        xml_buf_free(buf);
        return -1;
    }

    /*
     * Push the entity as the current input, read c_char by c_char
     * saving to the buffer until the end of the entity or an error
     */
    if xml_push_input(ctxt, input) < 0 {
        // xml_buffer_free(buf);
        xml_buf_free(buf);
        xml_free_input_stream(input);
        return -1;
    }

    GROW!(ctxt);
    c = CUR_CHAR!(ctxt, l);
    while (*ctxt).input == input && (*(*ctxt).input).cur < (*(*ctxt).input).end && IS_CHAR!(c) {
        // xml_buffer_add(buf, (*(*ctxt).input).cur, l);
        xml_buf_add(buf, (*(*ctxt).input).cur, l);
        NEXTL!(ctxt, l);
        c = CUR_CHAR!(ctxt, l);
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        // xml_buffer_free(buf);
        xml_buf_free(buf);
        return -1;
    }

    if (*ctxt).input == input && (*(*ctxt).input).cur >= (*(*ctxt).input).end {
        xml_saturated_add(
            addr_of_mut!((*ctxt).sizeentities),
            (*(*ctxt).input).consumed,
        );
        xml_pop_input(ctxt);
    } else if !IS_CHAR!(c) {
        xml_fatal_err_msg_int(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            c"xmlLoadEntityContent: invalid char value %d\n".as_ptr() as _,
            c,
        );
        // xml_buffer_free(buf);
        xml_buf_free(buf);
        return -1;
    }
    (*entity).length = xml_buf_use(buf) as i32;
    (*entity)
        .content
        .store(xml_buf_detach(buf), Ordering::Relaxed);
    // (*entity).content.store((*buf).content, Ordering::Relaxed);
    // (*entity).length = (*buf).using as _;
    // (*buf).content = null_mut();
    // xml_buffer_free(buf);
    xml_buf_free(buf);

    0
}

/*
 * Macro used to grow the current buffer.
 * buffer##_size is expected to be a size_t
 * mem_error: is expected to handle memory allocation failures
 */
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

/**
 * xmlStringDecodeEntitiesInt:
 * @ctxt:  the parser context
 * @str:  the input string
 * @len: the string length
 * @what:  combination of XML_SUBSTITUTE_REF and XML_SUBSTITUTE_PEREF
 * @end:  an end marker XmlChar, 0 if none
 * @end2:  an end marker XmlChar, 0 if none
 * @end3:  an end marker XmlChar, 0 if none
 * @check:  whether to perform entity checks
 */
pub(crate) unsafe extern "C" fn xml_string_decode_entities_int(
    ctxt: XmlParserCtxtPtr,
    mut str: *const XmlChar,
    len: c_int,
    what: c_int,
    end: XmlChar,
    end2: XmlChar,
    end3: XmlChar,
    check: c_int,
) -> *mut XmlChar {
    let mut buffer: *mut XmlChar;
    let mut buffer_size: size_t;
    let mut nbchars: size_t = 0;
    let mut current: *mut XmlChar;
    let mut rep: *mut XmlChar = null_mut();
    let mut ent: XmlEntityPtr;
    let mut c: i32;
    let mut l: c_int = 0;

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
            c"Maximum entity nesting depth exceeded".as_ptr() as _,
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
                    let val: c_int = xml_parse_string_char_ref(ctxt, addr_of_mut!(str));
                    if val == 0 {
                        break 'int_error;
                    }
                    COPY_BUF!(0, buffer, nbchars, val);
                    if nbchars + XML_PARSER_BUFFER_SIZE > buffer_size {
                        grow_buffer!(ctxt, buffer, XML_PARSER_BUFFER_SIZE, buffer_size, rep, 'mem_error);
                    }
                } else if c == b'&' as i32 && what & XML_SUBSTITUTE_REF as i32 != 0 {
                    if *xml_parser_debug_entities() != 0 {
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
                        && matches!(
                            (*ent).etype,
                            Some(XmlEntityType::XmlInternalPredefinedEntity)
                        )
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
                                c"predefined entity has no content\n".as_ptr() as _,
                            );
                            break 'int_error;
                        }
                    } else if !ent.is_null() && !(*ent).content.load(Ordering::Relaxed).is_null() {
                        if check != 0 && xml_parser_entity_check(ctxt, (*ent).length as _) != 0 {
                            break 'int_error;
                        }

                        if (*ent).flags & XML_ENT_EXPANDING as i32 != 0 {
                            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, null());
                            xml_halt_parser(ctxt);
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
                        let i: c_int = xml_strlen((*ent).name.load(Ordering::Relaxed));
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
                    if *xml_parser_debug_entities() != 0 {
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
                                xml_warning_msg(
                                    ctxt,
                                    XmlParserErrors::XmlErrEntityProcessing,
                                    c"not validating will not read content for PE entity %s\n"
                                        .as_ptr() as _,
                                    (*ent).name.load(Ordering::Relaxed),
                                    null(),
                                );
                            }
                        }

                        if check != 0 && xml_parser_entity_check(ctxt, (*ent).length as _) != 0 {
                            break 'int_error;
                        }

                        if (*ent).flags & XML_ENT_EXPANDING as i32 != 0 {
                            xml_fatal_err(ctxt, XmlParserErrors::XmlErrEntityLoop, null());
                            xml_halt_parser(ctxt);
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
        xml_err_memory(ctxt, null());
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

/**
 * xmlParseAttValueComplex:
 * @ctxt:  an XML parser context
 * @len:   the resulting attribute len
 * @normalize:  whether to apply the inner normalization
 *
 * parse a value for an attribute, this is the fallback function
 * of xmlParseAttValue() when the attribute parsing requires handling
 * of non-ASCII characters, or normalization compaction.
 *
 * Returns the AttValue parsed or NULL. The value has to be freed by the caller.
 */
unsafe extern "C" fn xml_parse_att_value_complex(
    ctxt: XmlParserCtxtPtr,
    attlen: *mut c_int,
    normalize: c_int,
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
    let mut c: i32;
    let mut l: c_int = 0;
    let mut in_space: c_int = 0;
    let mut current: *mut XmlChar;
    let mut ent: XmlEntityPtr;

    if NXT!(ctxt, 0) == b'"' {
        (*ctxt).instate = XmlParserInputState::XmlParserAttributeValue;
        limit = b'"';
        NEXT!(ctxt);
    } else if NXT!(ctxt, 0) == b'\'' {
        limit = b'\'';
        (*ctxt).instate = XmlParserInputState::XmlParserAttributeValue;
        NEXT!(ctxt);
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttributeNotStarted, null());
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
            c = CUR_CHAR!(ctxt, l);
            while (NXT!(ctxt, 0) != limit /* checked */ && IS_CHAR!(c) && c != b'<' as i32)
                && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
            {
                if c == b'&' as i32 {
                    in_space = 0;
                    if NXT!(ctxt, 1) == b'#' {
                        let val: c_int = xml_parse_char_ref(ctxt);

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
                            && matches!(
                                (*ent).etype,
                                Some(XmlEntityType::XmlInternalPredefinedEntity)
                            )
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
                            if !matches!(
                                (*ent).etype,
                                Some(XmlEntityType::XmlInternalPredefinedEntity)
                            ) {
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
                            let i: c_int = xml_strlen((*ent).name.load(Ordering::Relaxed));
                            let mut cur: *const XmlChar = (*ent).name.load(Ordering::Relaxed);

                            /*
                             * We also check for recursion and amplification
                             * when entities are not substituted. They're
                             * often expanded later.
                             */
                            if !matches!(
                                (*ent).etype,
                                Some(XmlEntityType::XmlInternalPredefinedEntity)
                            ) && !(*ent).content.load(Ordering::Relaxed).is_null()
                            {
                                if (*ent).flags & XML_ENT_CHECKED as i32 == 0 {
                                    let old_copy: c_ulong = (*ctxt).sizeentcopy;

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
                    if c == 0x20 || c == 0xD || c == 0xA || c == 0x9 {
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
                    NEXTL!(ctxt, l);
                }
                GROW!(ctxt);
                c = CUR_CHAR!(ctxt, l);
                if len > max_length {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrAttributeNotFinished,
                        c"AttValue length too long\n".as_ptr() as _,
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
            if RAW!(ctxt) == b'<' {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrLtInAttribute, null());
            } else if RAW!(ctxt) != limit {
                if c != 0 && !IS_CHAR!(c) {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidChar,
                        c"invalid character in attribute value\n".as_ptr() as _,
                    );
                } else {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrAttributeNotFinished,
                        c"AttValue: ' expected\n".as_ptr() as _,
                    );
                }
            } else {
                NEXT!(ctxt);
            }

            if !attlen.is_null() {
                *attlen = len as _;
            }
            return buf;
        }

        // mem_error:
        xml_err_memory(ctxt, null());
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

/**
 * xmlParseAttValueInternal:
 * @ctxt:  an XML parser context
 * @len:  attribute len result
 * @alloc:  whether the attribute was reallocated as a new string
 * @normalize:  if 1 then further non-CDATA normalization must be done
 *
 * parse a value for an attribute.
 * NOTE: if no normalization is needed, the routine will return pointers
 *       directly from the data buffer.
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
 * Returns the AttValue parsed or NULL. The value has to be freed by the
 *     caller if it was copied, this can be detected by val[*len] == 0.
 */
macro_rules! GROW_PARSE_ATT_VALUE_INTERNAL {
    ($ctxt:expr, $input:expr, $start:expr, $end:expr) => {
        let oldbase: *const XmlChar = (*(*$ctxt).input).base;
        GROW!($ctxt);
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

pub(crate) unsafe extern "C" fn xml_parse_att_value_internal(
    ctxt: XmlParserCtxtPtr,
    len: *mut c_int,
    alloc: *mut c_int,
    normalize: c_int,
) -> *mut XmlChar {
    let mut start: *const XmlChar;
    let mut end: *const XmlChar;
    let mut last: *const XmlChar;
    let ret: *mut XmlChar;
    let max_length: c_int = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH as i32
    } else {
        XML_MAX_TEXT_LENGTH as i32
    };

    GROW!(ctxt);
    let mut input: *const XmlChar = CUR_PTR!(ctxt);
    let mut line: c_int = (*(*ctxt).input).line;
    let mut col: c_int = (*(*ctxt).input).col;
    if *input != b'"' && *input != b'\'' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttributeNotStarted, null());
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
                        c"AttValue length too long\n".as_ptr() as _,
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
                        c"AttValue length too long\n".as_ptr() as _,
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
                GROW!(ctxt);
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
                        c"AttValue length too long\n".as_ptr() as _,
                    );
                    return null_mut();
                }
            }
        }
        if input.offset_from(start) > max_length as isize {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrAttributeNotFinished,
                c"AttValue length too long\n".as_ptr() as _,
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
                        c"AttValue length too long\n".as_ptr() as _,
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
                c"AttValue length too long\n".as_ptr() as _,
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
    CUR_PTR!(ctxt) = input;
    (*(*ctxt).input).line = line;
    (*(*ctxt).input).col = col;
    ret
    // need_complex:
    //  if !alloc.is_null() {
    //      *alloc = 1;
    //  }
    //  return xmlParseAttValueComplex(ctxt, len, normalize);
}

/**
 * xmlAttrNormalizeSpace:
 * @src: the source string
 * @dst: the target string
 *
 * Normalize the space in non CDATA attribute values:
 * If the attribute type is not CDATA, then the XML processor MUST further
 * process the normalized attribute value by discarding any leading and
 * trailing space (#x20) characters, and by replacing sequences of space
 * (#x20) characters by a single space (#x20) character.
 * Note that the size of dst need to be at least src, and if one doesn't need
 * to preserve dst (and it doesn't come from a dictionary or read-only) then
 * passing src as dst is just fine.
 *
 * Returns a pointer to the normalized value (dst) or NULL if no conversion
 *         is needed.
 */
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

/**
 * xmlAttrNormalizeSpace2:
 * @src: the source string
 *
 * Normalize the space in non CDATA attribute values, a slightly more complex
 * front end to avoid allocation problems when running on attribute values
 * coming from the input.
 *
 * Returns a pointer to the normalized value (dst) or NULL if no conversion
 *         is needed.
 */
unsafe extern "C" fn xml_attr_normalize_space2(
    ctxt: XmlParserCtxtPtr,
    src: *mut XmlChar,
    len: *mut c_int,
) -> *const XmlChar {
    let mut remove_head: c_int = 0;
    let mut need_realloc: c_int = 0;
    let mut cur: *const XmlChar;

    if ctxt.is_null() || src.is_null() || len.is_null() {
        return null_mut();
    }
    let i: c_int = *len;
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
            xml_err_memory(ctxt, null());
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

/**
 * xmlParseAttribute2:
 * @ctxt:  an XML parser context
 * @pref:  the element prefix
 * @elem:  the element name
 * @prefix:  a XmlChar ** used to store the value of the attribute prefix
 * @value:  a XmlChar ** used to store the value of the attribute
 * @len:  an c_int * to save the length of the attribute
 * @alloc:  an c_int * to indicate if the attribute was allocated
 *
 * parse an attribute in the new SAX2 framework.
 *
 * Returns the attribute name, and the value in *value, .
 */
unsafe extern "C" fn xml_parse_attribute2(
    ctxt: XmlParserCtxtPtr,
    pref: *const XmlChar,
    elem: *const XmlChar,
    prefix: *mut *const XmlChar,
    value: *mut *mut XmlChar,
    len: *mut c_int,
    alloc: *mut c_int,
) -> *const XmlChar {
    let mut val: *mut XmlChar;
    let mut internal_val: *mut XmlChar = null_mut();
    let mut normalize: c_int = 0;

    *value = null_mut();
    GROW!(ctxt);

    let name: *const XmlChar = xml_parse_qname(ctxt, prefix);

    if name.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            c"error parsing attribute name\n".as_ptr() as _,
        );
        return null_mut();
    }

    /*
     * get the type if needed
     */
    if !(*ctxt).atts_special.is_null() {
        let typ: c_int = xml_hash_qlookup2((*ctxt).atts_special, pref, elem, *prefix, name)
            as ptrdiff_t as c_int;
        if typ != 0 {
            normalize = 1;
        }
    }

    /*
     * read the value
     */
    SKIP_BLANKS!(ctxt);

    if RAW!(ctxt) == b'=' {
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
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
        xml_fatal_err_msg_str(
            ctxt,
            XmlParserErrors::XmlErrAttributeWithoutValue,
            c"Specification mandates value for attribute %s\n".as_ptr() as _,
            name,
        );
        return name;
    }

    if *prefix == (*ctxt).str_xml {
        /*
         * Check that xml:lang conforms to the specification
         * No more registered as an error, just generate a warning now
         * since this was deprecated in XML second edition
         */
        if (*ctxt).pedantic != 0 && xml_str_equal(name, c"lang".as_ptr() as _) {
            internal_val = xml_strndup(val, *len);
            if xml_check_language_id(internal_val) == 0 {
                xml_warning_msg(
                    ctxt,
                    XmlParserErrors::XmlWarLangValue,
                    c"Malformed value for xml:lang : %s\n".as_ptr() as _,
                    internal_val,
                    null(),
                );
            }
        }

        /*
         * Check that xml:space conforms to the specification
         */
        if xml_str_equal(name, c"space".as_ptr() as _) {
            internal_val = xml_strndup(val, *len);
            if xml_str_equal(internal_val, c"default".as_ptr() as _) {
                *(*ctxt).space = 0;
            } else if xml_str_equal(internal_val, c"preserve".as_ptr() as _) {
                *(*ctxt).space = 1;
            } else {
                xml_warning_msg(
                    ctxt,
                    XmlParserErrors::XmlWarSpaceValue,
                    c"Invalid value \"%s\" for xml:space : \"default\" or \"preserve\" expected\n"
                        .as_ptr() as _,
                    internal_val,
                    null(),
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

/**
 * xmlNsWarn
 * @ctxt:  an XML parser context
 * @error:  the error number
 * @msg:  the message
 * @info1:  extra information string
 * @info2:  extra information string
 *
 * Handle a namespace warning error
 */
unsafe extern "C" fn xml_ns_warn(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    info1: *const XmlChar,
    info2: *const XmlChar,
    info3: *const XmlChar,
) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    __xml_raise_error!(
        None,
        None,
        null_mut(),
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromNamespace as i32,
        error as i32,
        XmlErrorLevel::XmlErrWarning,
        null_mut(),
        0,
        info1 as _,
        info2 as _,
        info3 as _,
        0,
        0,
        msg,
        info1,
        info2,
        info3
    );
}

/**
 * xmlErrAttributeDup:
 * @ctxt:  an XML parser context
 * @prefix:  the attribute prefix
 * @localname:  the attribute localname
 *
 * Handle a redefinition of attribute error
 */
pub(crate) unsafe extern "C" fn xml_err_attribute_dup(
    ctxt: XmlParserCtxtPtr,
    prefix: *const XmlChar,
    localname: *const XmlChar,
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

    if prefix.is_null() {
        __xml_raise_error!(
            None,
            None,
            null_mut(),
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser as i32,
            XmlParserErrors::XmlErrAttributeRedefined as i32,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            localname as _,
            null_mut(),
            null_mut(),
            0,
            0,
            c"Attribute %s redefined\n".as_ptr() as _,
            localname
        );
    } else {
        __xml_raise_error!(
            None,
            None,
            null_mut(),
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser as i32,
            XmlParserErrors::XmlErrAttributeRedefined as i32,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            prefix as _,
            localname as _,
            null_mut(),
            0,
            0,
            c"Attribute %s:%s redefined\n".as_ptr() as _,
            prefix,
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

unsafe extern "C" fn xml_ctxt_grow_attrs(ctxt: XmlParserCtxtPtr, nr: c_int) -> c_int {
    let atts: *mut *const XmlChar;
    let attallocs: *mut c_int;
    let maxatts: c_int;

    if nr + 5 > (*ctxt).maxatts {
        maxatts = if (*ctxt).maxatts == 0 {
            55
        } else {
            (nr + 5) * 2
        };
        atts = xml_malloc(maxatts as usize * size_of::<*const XmlChar>()) as _;
        if atts.is_null() {
            // goto mem_error;
            xml_err_memory(ctxt, null());
            return -1;
        }
        attallocs = xml_realloc(
            (*ctxt).attallocs as _,
            (maxatts as usize / 5) * size_of::<c_int>(),
        ) as _;
        if attallocs.is_null() {
            xml_free(atts as _);
            // goto mem_error;
            xml_err_memory(ctxt, null());
            return -1;
        }
        if (*ctxt).maxatts > 0 {
            memcpy(
                atts as _,
                (*ctxt).atts as _,
                (*ctxt).maxatts as usize * size_of::<*const XmlChar>(),
            );
        }
        xml_free((*ctxt).atts as _);
        (*ctxt).atts = atts;
        (*ctxt).attallocs = attallocs;
        (*ctxt).maxatts = maxatts;
    }
    (*ctxt).maxatts
    // mem_error:
    //     xml_err_memory(ctxt, null());
    //     return -1;
}

/**
 * xmlParseStartTag2:
 * @ctxt:  an XML parser context
 *
 * Parse a start tag. Always consumes '<'.
 *
 * This routine is called when running SAX2 parsing
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
pub(crate) unsafe extern "C" fn xml_parse_start_tag2(
    ctxt: XmlParserCtxtPtr,
    pref: *mut *const XmlChar,
    uri: *mut *const XmlChar,
    tlen: *mut c_int,
) -> *const XmlChar {
    let mut localname: *const XmlChar;
    let mut prefix: *const XmlChar = null();
    let mut attname: *const XmlChar;
    let mut aprefix: *const XmlChar = null();
    let mut nsname: *const XmlChar;
    let mut attvalue: *mut XmlChar = null_mut();
    let mut atts: *mut *const XmlChar = (*ctxt).atts;
    let mut maxatts: c_int = (*ctxt).maxatts;
    let ns_nr: c_int = (*ctxt).ns_nr;

    if RAW!(ctxt) != b'<' {
        return null_mut();
    }
    NEXT1!(ctxt);

    let cur: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;
    let inputid: c_int = (*(*ctxt).input).id;
    let mut nbatts: c_int = 0;
    let mut nratts: c_int = 0;
    let mut nbdef: c_int = 0;
    let mut nb_ns: c_int = 0;
    let mut attval: c_int = 0;
    /* Forget any namespaces added during an earlier parse of this element. */
    (*ctxt).ns_nr = ns_nr;

    localname = xml_parse_qname(ctxt, addr_of_mut!(prefix));
    if localname.is_null() {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            c"StartTag: invalid element name\n".as_ptr() as _,
        );
        return null_mut();
    }
    *tlen = ((*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as usize - cur) as _;

    /*
     * Now parse the attributes, it ends up with the ending
     *
     * (S Attribute)* S?
     */
    SKIP_BLANKS!(ctxt);
    GROW!(ctxt);

    'done: {
        while (RAW!(ctxt) != b'>'
            && (RAW!(ctxt) != b'/' || NXT!(ctxt, 1) != b'>')
            && IS_BYTE_CHAR!(RAW!(ctxt)))
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            let mut len: c_int = -1;
            let mut alloc: c_int = 0;

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
                    c"xmlParseStartTag: problem parsing attributes\n".as_ptr() as _,
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
                        xml_err_memory(ctxt, c"dictionary allocation failure".as_ptr() as _);
                        if !attvalue.is_null() && alloc != 0 {
                            xml_free(attvalue as _);
                        }
                        localname = null_mut();
                        break 'done;
                    }
                    if *url != 0 {
                        uri = xml_parse_uri(url as _);
                        if uri.is_null() {
                            xml_ns_err(
                                ctxt,
                                XmlParserErrors::XmlWarNsUri,
                                c"xmlns: '%s' is not a valid URI\n".as_ptr() as _,
                                url,
                                null(),
                                null(),
                            );
                        } else {
                            if (*uri).scheme.is_null() {
                                xml_ns_warn(
                                    ctxt,
                                    XmlParserErrors::XmlWarNsUriRelative,
                                    c"xmlns: URI %s is not absolute\n".as_ptr() as _,
                                    url,
                                    null(),
                                    null(),
                                );
                            }
                            xml_free_uri(uri);
                        }
                        if url == (*ctxt).str_xml_ns {
                            if attname != (*ctxt).str_xml {
                                xml_ns_err(
                                    ctxt,
                                    XmlParserErrors::XmlNsErrXmlNamespace,
                                    c"xml namespace URI cannot be the default namespace\n".as_ptr()
                                        as _,
                                    null(),
                                    null(),
                                    null(),
                                );
                            }
                            break 'next_attr;
                        }
                        if len == 29
                            && xml_str_equal(url, c"http://www.w3.org/2000/xmlns/".as_ptr() as _)
                        {
                            xml_ns_err(
                                ctxt,
                                XmlParserErrors::XmlNsErrXmlNamespace,
                                c"reuse of the xmlns namespace name is forbidden\n".as_ptr() as _,
                                null(),
                                null(),
                                null(),
                            );
                            break 'next_attr;
                        }
                    }

                    /*
                     * check that it's not a defined namespace
                     */
                    let mut j = 1;
                    while j <= nb_ns {
                        if (*(*ctxt).ns_tab.add((*ctxt).ns_nr as usize - 2 * j as usize)).is_null()
                        {
                            break;
                        }
                        j += 1;
                    }
                    if j <= nb_ns {
                        xml_err_attribute_dup(ctxt, null_mut(), attname);
                    } else if ns_push(ctxt, null_mut(), url) > 0 {
                        nb_ns += 1;
                    }
                } else if aprefix == (*ctxt).str_xmlns {
                    let url: *const XmlChar = xml_dict_lookup((*ctxt).dict, attvalue, len);

                    if attname == (*ctxt).str_xml {
                        if url != (*ctxt).str_xml_ns {
                            xml_ns_err(
                                ctxt,
                                XmlParserErrors::XmlNsErrXmlNamespace,
                                c"xml namespace prefix mapped to wrong URI\n".as_ptr() as _,
                                null(),
                                null(),
                                null(),
                            );
                        }
                        /*
                         * Do not keep a namespace definition node
                         */
                        break 'next_attr;
                    }
                    if url == (*ctxt).str_xml_ns {
                        if attname != (*ctxt).str_xml {
                            xml_ns_err(
                                ctxt,
                                XmlParserErrors::XmlNsErrXmlNamespace,
                                c"xml namespace URI mapped to wrong prefix\n".as_ptr() as _,
                                null(),
                                null(),
                                null(),
                            );
                        }
                        break 'next_attr;
                    }
                    if attname == (*ctxt).str_xmlns {
                        xml_ns_err(
                            ctxt,
                            XmlParserErrors::XmlNsErrXmlNamespace,
                            c"redefinition of the xmlns prefix is forbidden\n".as_ptr() as _,
                            null(),
                            null(),
                            null(),
                        );
                        break 'next_attr;
                    }
                    if len == 29
                        && xml_str_equal(url, c"http://www.w3.org/2000/xmlns/".as_ptr() as _)
                    {
                        xml_ns_err(
                            ctxt,
                            XmlParserErrors::XmlNsErrXmlNamespace,
                            c"reuse of the xmlns namespace name is forbidden\n".as_ptr() as _,
                            null(),
                            null(),
                            null(),
                        );
                        break 'next_attr;
                    }
                    if url.is_null() || *url.add(0) == 0 {
                        xml_ns_err(
                            ctxt,
                            XmlParserErrors::XmlNsErrXmlNamespace,
                            c"xmlns:%s: Empty XML namespace is not allowed\n".as_ptr() as _,
                            attname,
                            null(),
                            null(),
                        );
                        break 'next_attr;
                    }
                    let uri: XmlURIPtr = xml_parse_uri(url as _);
                    if uri.is_null() {
                        xml_ns_err(
                            ctxt,
                            XmlParserErrors::XmlWarNsUri,
                            c"xmlns:%s: '%s' is not a valid URI\n".as_ptr() as _,
                            attname,
                            url,
                            null(),
                        );
                    } else {
                        if (*ctxt).pedantic != 0 && (*uri).scheme.is_null() {
                            xml_ns_warn(
                                ctxt,
                                XmlParserErrors::XmlWarNsUriRelative,
                                c"xmlns:%s: URI %s is not absolute\n".as_ptr() as _,
                                attname,
                                url,
                                null(),
                            );
                        }
                        xml_free_uri(uri);
                    }
                    /*
                     * check that it's not a defined namespace
                     */
                    let mut j = 1;
                    while j <= nb_ns {
                        if *(*ctxt).ns_tab.add((*ctxt).ns_nr as usize - 2 * j as usize) == attname {
                            break;
                        }
                        j += 1;
                    }
                    if j <= nb_ns {
                        xml_err_attribute_dup(ctxt, aprefix, attname);
                    } else if ns_push(ctxt, attname, url) > 0 {
                        nb_ns += 1;
                    }
                } else {
                    /*
                     * Add the pair to atts
                     */
                    if atts.is_null() || nbatts + 5 > maxatts {
                        if xml_ctxt_grow_attrs(ctxt, nbatts + 5) < 0 {
                            break 'next_attr;
                        }

                        maxatts = (*ctxt).maxatts;
                        atts = (*ctxt).atts;
                    }

                    *(*ctxt).attallocs.add(nratts as usize) = alloc;
                    nratts += 1;
                    *atts.add(nbatts as usize) = attname;
                    nbatts += 1;
                    *atts.add(nbatts as usize) = aprefix;
                    nbatts += 1;
                    /*
                     * The namespace URI field is used temporarily to point at the
                     * base of the current input buffer for non-alloced attributes.
                     * When the input buffer is reallocated, all the pointers become
                     * invalid, but they can be reconstructed later.
                     */
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
                    /*
                     * tag if some deallocation is needed
                     */
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

            GROW!(ctxt);
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                break;
            }
            if RAW!(ctxt) == b'>' || (RAW!(ctxt) == b'/' && NXT!(ctxt, 1) == b'>') {
                break;
            }
            if SKIP_BLANKS!(ctxt) == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    c"attributes construct error\n".as_ptr() as _,
                );
                break;
            }
            GROW!(ctxt);
        }

        if (*(*ctxt).input).id != inputid {
            xml_fatal_err(
                ctxt,
                XmlParserErrors::XmlErrInternalError,
                c"Unexpected change of input\n".as_ptr() as _,
            );
            localname = null_mut();
            break 'done;
        }

        /* Reconstruct attribute value pointers. */
        for (i, _) in (0..).step_by(5).zip(0..nratts) {
            if !(*atts.add(i + 2)).is_null() {
                /*
                 * Arithmetic on dangling pointers is technically undefined
                 * behavior, but well...
                 */
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

        /*
         * The attributes defaulting
         */
        if !(*ctxt).atts_default.is_null() {
            let defaults: XmlDefAttrsPtr =
                xml_hash_lookup2((*ctxt).atts_default, localname, prefix) as _;
            if !defaults.is_null() {
                'b: for i in 0..(*defaults).nb_attrs as usize {
                    attname = *(*defaults).values.as_ptr().add(5 * i);
                    aprefix = *(*defaults).values.as_ptr().add(5 * i + 1);

                    /*
                     * special work for namespaces defaulted defs
                     */
                    if attname == (*ctxt).str_xmlns && aprefix.is_null() {
                        /*
                         * check that it's not a defined namespace
                         */
                        for j in 1..=nb_ns {
                            if (*(*ctxt).ns_tab.add((*ctxt).ns_nr as usize - 2 * j as usize))
                                .is_null()
                            {
                                continue 'b;
                            }
                        }

                        nsname = xml_get_namespace(ctxt, null_mut());
                        if nsname != *(*defaults).values.as_ptr().add(5 * i + 2)
                            && ns_push(
                                ctxt,
                                null_mut(),
                                *(*defaults).values.as_ptr().add(5 * i + 2),
                            ) > 0
                        {
                            nb_ns += 1;
                        }
                    } else if aprefix == (*ctxt).str_xmlns {
                        /*
                         * check that it's not a defined namespace
                         */
                        for j in 1..=nb_ns {
                            if *(*ctxt).ns_tab.add((*ctxt).ns_nr as usize - 2 * j as usize)
                                == attname
                            {
                                continue 'b;
                            }
                        }

                        nsname = xml_get_namespace(ctxt, attname);
                        if nsname != *(*defaults).values.as_ptr().add(5 * i + 2)
                            && ns_push(ctxt, attname, *(*defaults).values.as_ptr().add(5 * i + 2))
                                > 0
                        {
                            nb_ns += 1;
                        }
                    } else {
                        /*
                         * check that it's not a defined attribute
                         */
                        for j in (0..nbatts).step_by(5) {
                            if attname == *atts.add(j as usize)
                                && aprefix == *atts.add(j as usize + 1)
                            {
                                continue 'b;
                            }
                        }

                        if atts.is_null() || nbatts + 5 > maxatts {
                            if xml_ctxt_grow_attrs(ctxt, nbatts + 5) < 0 {
                                localname = null_mut();
                                break 'done;
                            }
                            maxatts = (*ctxt).maxatts;
                            atts = (*ctxt).atts;
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
                            xml_validity_error(
                                ctxt,
                                XmlParserErrors::XmlDtdStandaloneDefaulted,
                                c"standalone: attribute %s on %s defaulted from external subset\n"
                                    .as_ptr() as _,
                                attname,
                                localname,
                            );
                        }
                        nbdef += 1;
                    }
                }
            }
        }

        /*
         * The attributes checkings
         */
        for i in (0..nbatts as usize).step_by(5) {
            /*
             * The default namespace does not apply to attribute names.
             */
            if !(*atts.add(i + 1)).is_null() {
                nsname = xml_get_namespace(ctxt, *atts.add(i + 1));
                if nsname.is_null() {
                    xml_ns_err(
                        ctxt,
                        XmlParserErrors::XmlNsErrUndefinedNamespace,
                        c"Namespace prefix %s for %s on %s is not defined\n".as_ptr() as _,
                        *atts.add(i + 1),
                        *atts.add(i),
                        localname,
                    );
                }
                *atts.add(i + 2) = nsname;
            } else {
                nsname = null_mut();
            }
            /*
             * [ WFC: Unique Att Spec ]
             * No attribute name may appear more than once in the same
             * start-tag or empty-element tag.
             * As extended by the Namespace in XML REC.
             */
            for j in (0..i).step_by(5) {
                if *atts.add(i) == *atts.add(j) {
                    if *atts.add(i + 1) == *atts.add(j + 1) {
                        xml_err_attribute_dup(ctxt, *atts.add(i + 1), *atts.add(i));
                        break;
                    }
                    if !nsname.is_null() && *atts.add(j + 2) == nsname {
                        xml_ns_err(
                            ctxt,
                            XmlParserErrors::XmlNsErrAttributeRedefined,
                            c"Namespaced Attribute %s in '%s' redefined\n".as_ptr() as _,
                            *atts.add(i),
                            nsname,
                            null(),
                        );
                        break;
                    }
                }
            }
        }

        nsname = xml_get_namespace(ctxt, prefix);
        if !prefix.is_null() && nsname.is_null() {
            xml_ns_err(
                ctxt,
                XmlParserErrors::XmlNsErrUndefinedNamespace,
                c"Namespace prefix %s on %s is not defined\n".as_ptr() as _,
                prefix,
                localname,
                null(),
            );
        }
        *pref = prefix;
        *uri = nsname;

        /*
         * SAX: Start of Element !
         */
        if !(*ctxt).sax.is_null()
            && (*(*ctxt).sax).start_element_ns.is_some()
            && (*ctxt).disable_sax == 0
        {
            if nb_ns > 0 {
                ((*(*ctxt).sax).start_element_ns.unwrap())(
                    (*ctxt).user_data,
                    localname,
                    prefix,
                    nsname,
                    nb_ns,
                    (*ctxt).ns_tab.add(((*ctxt).ns_nr - 2 * nb_ns) as usize),
                    nbatts / 5,
                    nbdef,
                    atts,
                );
            } else {
                ((*(*ctxt).sax).start_element_ns.unwrap())(
                    (*ctxt).user_data,
                    localname,
                    prefix,
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
    /*
     * Free up attribute allocated strings if needed
     */
    if attval != 0 {
        for (i, j) in (3..).step_by(5).zip(0..nratts) {
            if *(*ctxt).attallocs.add(j as usize) != 0 && !(*atts.add(i)).is_null() {
                xml_free(*atts.add(i) as _);
            }
        }
    }

    localname
}

pub(crate) unsafe extern "C" fn space_pop(ctxt: XmlParserCtxtPtr) -> c_int {
    if (*ctxt).space_nr <= 0 {
        return 0;
    }
    (*ctxt).space_nr -= 1;
    if (*ctxt).space_nr > 0 {
        (*ctxt).space = (*ctxt).space_tab.add((*ctxt).space_nr as usize - 1);
    } else {
        (*ctxt).space = (*ctxt).space_tab.add(0);
    }
    let ret: c_int = *(*ctxt).space_tab.add((*ctxt).space_nr as usize);
    *(*ctxt).space_tab.add((*ctxt).space_nr as usize) = -1;
    ret
}

/**
 * nameNsPush:
 * @ctxt:  an XML parser context
 * @value:  the element name
 * @prefix:  the element prefix
 * @URI:  the element namespace name
 * @line:  the current line number for error messages
 * @nsNr:  the number of namespaces pushed on the namespace table
 *
 * Pushes a new element name/prefix/URL on top of the name stack
 *
 * Returns -1 in case of error, the index in the stack otherwise
 */
pub(crate) unsafe extern "C" fn name_ns_push(
    ctxt: XmlParserCtxtPtr,
    value: *const XmlChar,
    prefix: *const XmlChar,
    uri: *const XmlChar,
    line: c_int,
    ns_nr: c_int,
) -> c_int {
    'mem_error: {
        if (*ctxt).name_nr >= (*ctxt).name_max {
            (*ctxt).name_max *= 2;
            let tmp: *mut *const XmlChar = xml_realloc(
                (*ctxt).name_tab as _,
                (*ctxt).name_max as usize * size_of_val(&*(*ctxt).name_tab.add(0)),
            ) as _;
            if tmp.is_null() {
                (*ctxt).name_max /= 2;
                break 'mem_error;
            }
            (*ctxt).name_tab = tmp;
            let tmp2: *mut XmlStartTag = xml_realloc(
                (*ctxt).push_tab as _,
                (*ctxt).name_max as usize * size_of_val(&*(*ctxt).push_tab.add(0)),
            ) as _;
            if tmp2.is_null() {
                (*ctxt).name_max /= 2;
                break 'mem_error;
            }
            (*ctxt).push_tab = tmp2;
        } else if (*ctxt).push_tab.is_null() {
            (*ctxt).push_tab =
                xml_malloc((*ctxt).name_max as usize * size_of_val(&*(*ctxt).push_tab.add(0))) as _;
            if (*ctxt).push_tab.is_null() {
                break 'mem_error;
            }
        }
        *(*ctxt).name_tab.add((*ctxt).name_nr as usize) = value;
        (*ctxt).name = value;
        let tag: *mut XmlStartTag = (*ctxt).push_tab.add((*ctxt).name_nr as usize);
        (*tag).prefix = prefix;
        (*tag).uri = uri;
        (*tag).line = line;
        (*tag).ns_nr = ns_nr;
        let res = (*ctxt).name_nr;
        (*ctxt).name_nr += 1;
        return res;
    }
    // mem_error:
    xml_err_memory(ctxt, null());
    -1
}

/**
 * xmlParseLookupChar:
 * @ctxt:  an XML parser context
 * @c:  character
 *
 * Check whether the input buffer contains a character.
 */
unsafe extern "C" fn xml_parse_lookup_char(ctxt: XmlParserCtxtPtr, c: c_int) -> c_int {
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

/**
 * xmlParseLookupCharData:
 * @ctxt:  an XML parser context
 *
 * Check whether the input buffer contains terminated c_char data.
 */
unsafe extern "C" fn xml_parse_lookup_char_data(ctxt: XmlParserCtxtPtr) -> c_int {
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

/*
 * used for the test in the inner loop of the c_char data testing
 */
const TEST_CHAR_DATA: [c_uchar; 256] = [
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

/**
 * areBlanks:
 * @ctxt:  an XML parser context
 * @str:  a XmlChar *
 * @len:  the size of @str
 * @blank_chars: we know the chars are blanks
 *
 * Is this a sequence of blank chars that one can ignore ?
 *
 * Returns 1 if ignorable 0 otherwise.
 */
unsafe extern "C" fn are_blanks(
    ctxt: XmlParserCtxtPtr,
    str: *const XmlChar,
    len: c_int,
    blank_chars: c_int,
) -> c_int {
    let ret: c_int;

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
    if (*ctxt).space.is_null() || *(*ctxt).space == 1 || *(*ctxt).space == -2 {
        return 0;
    }

    /*
     * Check that the string is made of blanks
     */
    if blank_chars == 0 {
        for i in 0..len {
            if !IS_BLANK_CH!(*str.add(i as usize)) {
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

    /*
     * Otherwise, heuristic :-\
     */
    if RAW!(ctxt) != b'<' && RAW!(ctxt) != 0xD {
        return 0;
    }
    if (*(*ctxt).node).children.is_null() && RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'/' {
        return 0;
    }

    let last_child: XmlNodePtr = xml_get_last_child((*ctxt).node);
    if last_child.is_null() {
        if (*(*ctxt).node).typ != XmlElementType::XmlElementNode
            && !(*(*ctxt).node).content.is_null()
        {
            return 0;
        }
    } else if xml_node_is_text(last_child) != 0
        || (!(*(*ctxt).node).children.is_null() && xml_node_is_text((*(*ctxt).node).children) != 0)
    {
        return 0;
    }
    1
}

/**
 * xmlParseCharDataComplex:
 * @ctxt:  an XML parser context
 * @cdata:  c_int indicating whether we are within a CDATA section
 *
 * Always makes progress if the first c_char isn't '<' or '&'.
 *
 * parse a CharData section.this is the fallback function
 * of xmlParseCharData() when the parsing requires handling
 * of non-ASCII characters.
 */
unsafe extern "C" fn xml_parse_char_data_complex(ctxt: XmlParserCtxtPtr, partial: c_int) {
    let mut buf: [XmlChar; XML_PARSER_BIG_BUFFER_SIZE + 5] = [0; XML_PARSER_BIG_BUFFER_SIZE + 5];
    let mut nbchar: c_int = 0;
    let mut cur: i32;
    let mut l: c_int = 0;

    cur = CUR_CHAR!(ctxt, l);
    while cur != b'<' as i32 /* checked */ && cur != b'&' as i32 && IS_CHAR!(cur)
    /* test also done in xmlCurrentChar() */
    {
        if cur == b']' as i32 && NXT!(ctxt, 1) == b']' && NXT!(ctxt, 2) == b'>' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrMisplacedCdataEnd, null());
        }
        COPY_BUF!(l, buf.as_mut_ptr(), nbchar, cur);
        /* move current position before possible calling of (*(*ctxt).sax).characters */
        NEXTL!(ctxt, l);
        if nbchar >= XML_PARSER_BIG_BUFFER_SIZE as i32 {
            buf[nbchar as usize] = 0;

            /*
             * OK the segment is to be consumed as chars.
             */
            if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                if are_blanks(ctxt, buf.as_ptr(), nbchar, 0) != 0 {
                    if let Some(ignorable_whitespace) = (*(*ctxt).sax).ignorable_whitespace {
                        ignorable_whitespace((*ctxt).user_data, buf.as_ptr(), nbchar);
                    }
                } else {
                    if let Some(characters) = (*(*ctxt).sax).characters {
                        characters((*ctxt).user_data, buf.as_ptr(), nbchar);
                    }
                    if (*(*ctxt).sax).characters != (*(*ctxt).sax).ignorable_whitespace
                        && *(*ctxt).space == -1
                    {
                        *(*ctxt).space = -2;
                    }
                }
            }
            nbchar = 0;
            /* something really bad happened in the SAX callback */
            if !matches!((*ctxt).instate, XmlParserInputState::XmlParserContent) {
                return;
            }
            SHRINK!(ctxt);
        }
        cur = CUR_CHAR!(ctxt, l);
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }
    if nbchar != 0 {
        buf[nbchar as usize] = 0;
        /*
         * OK the segment is to be consumed as chars.
         */
        if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
            if are_blanks(ctxt, buf.as_ptr(), nbchar, 0) != 0 {
                if let Some(ignorable_whitespace) = (*(*ctxt).sax).ignorable_whitespace {
                    ignorable_whitespace((*ctxt).user_data, buf.as_ptr(), nbchar);
                }
            } else {
                if let Some(characters) = (*(*ctxt).sax).characters {
                    characters((*ctxt).user_data, buf.as_ptr(), nbchar);
                }
                if (*(*ctxt).sax).characters != (*(*ctxt).sax).ignorable_whitespace
                    && *(*ctxt).space == -1
                {
                    *(*ctxt).space = -2;
                }
            }
        }
    }
    /*
     * cur == 0 can mean
     *
     * - xmlParserInputState::XmlParserEOF or memory error. This is checked above.
     * - An actual 0 character.
     * - End of buffer.
     * - An incomplete UTF-8 sequence. This is allowed if partial is set.
     */
    if (*(*ctxt).input).cur < (*(*ctxt).input).end {
        if cur == 0 && CUR!(ctxt) != 0 {
            if partial == 0 {
                xml_fatal_err_msg_int(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidChar,
                    c"Incomplete UTF-8 sequence starting with %02X\n".as_ptr() as _,
                    CUR!(ctxt) as _,
                );
                NEXTL!(ctxt, 1);
            }
        } else if cur != b'<' as i32 && cur != b'&' as i32 {
            /* Generate the error and skip the offending character */
            xml_fatal_err_msg_int(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                c"PCDATA invalid Char value %d\n".as_ptr() as _,
                cur as _,
            );
            NEXTL!(ctxt, l);
        }
    }
}

/**
 * xmlParseCharDataInternal:
 * @ctxt:  an XML parser context
 * @partial:  buffer may contain partial UTF-8 sequences
 *
 * Parse character data. Always makes progress if the first c_char isn't
 * '<' or '&'.
 *
 * The right angle bracket (>) may be represented using the string "&gt;".as_ptr() as _,
 * and must, for compatibility, be escaped using "&gt;" or a character
 * reference when it appears in the string "]]>" in content, when that
 * string is not marking the end of a CDATA section.
 *
 * [14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)
 */
pub(crate) unsafe extern "C" fn xml_parse_char_data_internal(
    ctxt: XmlParserCtxtPtr,
    partial: c_int,
) {
    let mut input: *const XmlChar;
    let mut nbchar: c_int;
    let mut line: c_int = (*(*ctxt).input).line;
    let mut col: c_int = (*(*ctxt).input).col;
    let mut ccol: c_int;

    GROW!(ctxt);
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
                            ignorable_whitespace((*ctxt).user_data, tmp, nbchar);
                        }
                    } else {
                        if let Some(characters) = (*(*ctxt).sax).characters {
                            characters((*ctxt).user_data, tmp, nbchar);
                        }
                        if *(*ctxt).space == -1 {
                            *(*ctxt).space = -2;
                        }
                    }
                } else if !(*ctxt).sax.is_null() && (*(*ctxt).sax).characters.is_some() {
                    ((*(*ctxt).sax).characters.unwrap())((*ctxt).user_data, tmp, nbchar);
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
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrMisplacedCdataEnd, null());
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
                && IS_BLANK_CH!(*(*(*ctxt).input).cur)
            {
                let tmp: *const XmlChar = (*(*ctxt).input).cur;
                (*(*ctxt).input).cur = input;

                if are_blanks(ctxt, tmp, nbchar, 0) != 0 {
                    if let Some(ignorable_whitespace) = (*(*ctxt).sax).ignorable_whitespace {
                        ignorable_whitespace((*ctxt).user_data, tmp, nbchar);
                    }
                } else {
                    if let Some(characters) = (*(*ctxt).sax).characters {
                        characters((*ctxt).user_data, tmp, nbchar);
                    }
                    if *(*ctxt).space == -1 {
                        *(*ctxt).space = -2;
                    }
                }
                line = (*(*ctxt).input).line;
                col = (*(*ctxt).input).col;
            } else if !(*ctxt).sax.is_null() {
                if let Some(characters) = (*(*ctxt).sax).characters {
                    characters((*ctxt).user_data, (*(*ctxt).input).cur, nbchar);
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
        SHRINK!(ctxt);
        GROW!(ctxt);
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

/**
 * xmlParseNameAndCompare:
 * @ctxt:  an XML parser context
 *
 * parse an XML name and compares for match
 * (specialized for endtag parsing)
 *
 * Returns NULL for an illegal name, (XmlChar*) 1 for success
 * and the name for mismatch
 */
unsafe extern "C" fn xml_parse_name_and_compare(
    ctxt: XmlParserCtxtPtr,
    other: *mut XmlChar,
) -> *const XmlChar {
    let mut cmp: *const XmlChar = other;
    let mut input: *const XmlChar;

    GROW!(ctxt);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }

    input = (*(*ctxt).input).cur;
    while *input != 0 && *input == *cmp {
        input = input.add(1);
        cmp = cmp.add(1);
    }
    if *cmp == 0 && (*input == b'>' || IS_BLANK_CH!(*input)) {
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

/**
 * xmlParseQNameAndCompare:
 * @ctxt:  an XML parser context
 * @name:  the localname
 * @prefix:  the prefix, if any.
 *
 * parse an XML name and compares for match
 * (specialized for endtag parsing)
 *
 * Returns NULL for an illegal name, (XmlChar*) 1 for success
 * and the name for mismatch
 */
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

    GROW!(ctxt);
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
        if *cmp == 0 && (*input == b'>' || IS_BLANK_CH!(*input)) {
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

/**
 * xmlParseEndTag2:
 * @ctxt:  an XML parser context
 * @line:  line of the start tag
 * @nsNr:  number of namespaces on the start tag
 *
 * Parse an end tag. Always consumes '</'.
 *
 * [42] ETag ::= '</' Name S? '>'
 *
 * With namespace
 *
 * [NS 9] ETag ::= '</' QName S? '>'
 */
pub(crate) unsafe extern "C" fn xml_parse_end_tag2(
    ctxt: XmlParserCtxtPtr,
    tag: *const XmlStartTag,
) {
    let mut name: *const XmlChar;

    GROW!(ctxt);
    if RAW!(ctxt) != b'<' || NXT!(ctxt, 1) != b'/' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrLtslashRequired, null());
        return;
    }
    SKIP!(ctxt, 2);

    if (*tag).prefix.is_null() {
        name = xml_parse_name_and_compare(ctxt, (*ctxt).name as _);
    } else {
        name = xml_parse_qname_and_compare(ctxt, (*ctxt).name as _, (*tag).prefix as _);
    }

    /*
     * We should definitely be at the ending "S? '>'" part
     */
    GROW!(ctxt);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }
    SKIP_BLANKS!(ctxt);
    if !IS_BYTE_CHAR!(RAW!(ctxt)) || RAW!(ctxt) != b'>' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrGtRequired, null());
    } else {
        NEXT1!(ctxt);
    }

    /*
     * [ WFC: Element Type Match ]
     * The Name in an element's end-tag must match the element type in the
     * start-tag.
     *
     */
    if name != 1 as *mut XmlChar {
        if name.is_null() {
            name = c"unparsable".as_ptr() as _;
        }
        xml_fatal_err_msg_str_int_str(
            ctxt,
            XmlParserErrors::XmlErrTagNameMismatch,
            c"Opening and ending tag mismatch: %s line %d and %s\n".as_ptr() as _,
            (*ctxt).name,
            (*tag).line,
            name,
        );
    }

    /*
     * SAX: End of Tag
     */
    if !(*ctxt).sax.is_null() && (*(*ctxt).sax).end_element_ns.is_some() && (*ctxt).disable_sax == 0
    {
        ((*(*ctxt).sax).end_element_ns.unwrap())(
            (*ctxt).user_data,
            (*ctxt).name,
            (*tag).prefix,
            (*tag).uri,
        );
    }

    space_pop(ctxt);
    if (*tag).ns_nr != 0 {
        ns_pop(ctxt, (*tag).ns_nr);
    }
}

/**
 * nameNsPop:
 * @ctxt: an XML parser context
 *
 * Pops the top element/prefix/URI name from the name stack
 *
 * Returns the name just removed
 */
#[cfg(feature = "push")]
unsafe extern "C" fn name_ns_pop(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    if (*ctxt).name_nr <= 0 {
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
 * xmlParseEndTag1:
 * @ctxt:  an XML parser context
 * @line:  line of the start tag
 * @nsNr:  number of namespaces on the start tag
 *
 * Parse an end tag. Always consumes '</'.
 *
 * [42] ETag ::= '</' Name S? '>'
 *
 * With namespace
 *
 * [NS 9] ETag ::= '</' QName S? '>'
 */
#[cfg(feature = "sax1")]
pub(crate) unsafe extern "C" fn xml_parse_end_tag1(ctxt: XmlParserCtxtPtr, line: c_int) {
    let mut name: *const XmlChar;

    GROW!(ctxt);
    if RAW!(ctxt) != b'<' || NXT!(ctxt, 1) != b'/' {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrLtslashRequired,
            c"xmlParseEndTag: '</' not found\n".as_ptr() as _,
        );
        return;
    }
    SKIP!(ctxt, 2);

    name = xml_parse_name_and_compare(ctxt, (*ctxt).name as _);

    /*
     * We should definitely be at the ending "S? '>'" part
     */
    GROW!(ctxt);
    SKIP_BLANKS!(ctxt);
    if !IS_BYTE_CHAR!(RAW!(ctxt)) || RAW!(ctxt) != b'>' {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrGtRequired, null());
    } else {
        NEXT1!(ctxt);
    }

    /*
     * [ WFC: Element Type Match ]
     * The Name in an element's end-tag must match the element type in the
     * start-tag.
     *
     */
    if name != 1 as *mut XmlChar {
        if name.is_null() {
            name = c"unparsable".as_ptr() as _;
        }
        xml_fatal_err_msg_str_int_str(
            ctxt,
            XmlParserErrors::XmlErrTagNameMismatch,
            c"Opening and ending tag mismatch: %s line %d and %s\n".as_ptr() as _,
            (*ctxt).name,
            line,
            name,
        );
    }

    /*
     * SAX: End of Tag
     */
    if !(*ctxt).sax.is_null() && (*(*ctxt).sax).end_element.is_some() && (*ctxt).disable_sax == 0 {
        ((*(*ctxt).sax).end_element.unwrap())((*ctxt).user_data, (*ctxt).name);
    }

    name_pop(ctxt);
    space_pop(ctxt);
}

/**
 * xmlCheckCdataPush:
 * @cur: pointer to the block of characters
 * @len: length of the block in bytes
 * @complete: 1 if complete CDATA block is passed in, 0 if partial block
 *
 * Check that the block of characters is okay as SCdata content [20]
 *
 * Returns the number of bytes to pass if okay, a negative index where an
 *         UTF-8 error occurred otherwise
 */
#[cfg(feature = "push")]
unsafe extern "C" fn xml_check_cdata_push(
    utf: *const XmlChar,
    len: c_int,
    complete: c_int,
) -> c_int {
    use crate::xml_is_char_q;

    let mut ix: c_int;
    let mut c: c_uchar;
    let mut codepoint: c_int;

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
            if !xml_is_char_q!(codepoint) {
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
            if !xml_is_char_q!(codepoint) {
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
            if !xml_is_char_q!(codepoint) {
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

/**
 * xmlParseLookupInternalSubset:
 * @ctxt:  an XML parser context
 *
 * Check whether there's enough data in the input buffer to finish parsing
 * the internal subset.
 */
#[cfg(feature = "push")]
unsafe extern "C" fn xml_parse_lookup_internal_subset(ctxt: XmlParserCtxtPtr) -> c_int {
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
    let mut state: c_int = (*ctxt).end_check_state;

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
            if IS_BLANK_CH!(*cur) {
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
            if !IS_BLANK_CH!(*cur) {
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

/**
 * xmlParseTryOrFinish:
 * @ctxt:  an XML parser context
 * @terminate:  last chunk indicator
 *
 * Try to progress on parsing
 *
 * Returns zero if no parsing was possible
 */
unsafe extern "C" fn xml_parse_try_or_finish(ctxt: XmlParserCtxtPtr, terminate: c_int) -> c_int {
    let mut ret: c_int = 0;
    let mut tlen: c_int = 0;
    let mut avail: size_t;
    let mut cur: XmlChar;
    let mut next: XmlChar;

    if (*ctxt).input.is_null() {
        return 0;
    }

    if !(*ctxt).input.is_null() && (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) > 4096 {
        xml_parser_shrink(ctxt);
    }

    'encoding_error: {
        while !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            if (*ctxt).err_no != XmlParserErrors::XmlErrOK as i32 && (*ctxt).disable_sax == 1 {
                return 0;
            }

            if (*ctxt).input.is_null() {
                break;
            }
            if !(*(*ctxt).input).buf.is_null() {
                /*
                 * If we are operating on converted input, try to flush
                 * remaining chars to avoid them stalling in the non-converted
                 * buffer.
                 */
                if !(*(*(*ctxt).input).buf).raw.is_null()
                    && xml_buf_is_empty((*(*(*ctxt).input).buf).raw) == 0
                {
                    let base: size_t = xml_buf_get_input_base(
                        (*(*(*ctxt).input).buf)
                            .buffer
                            .map_or(null_mut(), |buf| buf.as_ptr()),
                        (*ctxt).input,
                    );
                    let current: size_t =
                        (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

                    xml_parser_input_buffer_push((*(*ctxt).input).buf, 0, c"".as_ptr() as _);
                    xml_buf_set_input_base_cur(
                        (*(*(*ctxt).input).buf)
                            .buffer
                            .map_or(null_mut(), |buf| buf.as_ptr()),
                        (*ctxt).input,
                        base,
                        current,
                    );
                }
            }
            avail = (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) as _;
            if avail < 1 {
                // goto done;
                return ret;
            }
            match (*ctxt).instate {
                XmlParserInputState::XmlParserEOF => {
                    /*
                     * Document parsing is done !
                     */
                    // goto done;
                    return ret;
                }
                XmlParserInputState::XmlParserStart => 'to_break: {
                    if (*ctxt).charset == XmlCharEncoding::XmlCharEncodingNone as i32 {
                        let mut start: [XmlChar; 4] = [0; 4];

                        /*
                         * Very first chars read from the document flow.
                         */
                        if avail < 4 {
                            // goto done;
                            return ret;
                        }

                        /*
                         * Get the 4 first bytes and decode the charset
                         * if enc != xmlCharEncoding::XML_CHAR_ENCODING_NONE
                         * plug some encoding conversion routines,
                         * else xmlSwitchEncoding will set to (default)
                         * UTF8.
                         */
                        start[0] = RAW!(ctxt);
                        start[1] = NXT!(ctxt, 1);
                        start[2] = NXT!(ctxt, 2);
                        start[3] = NXT!(ctxt, 3);
                        let enc: XmlCharEncoding = xml_detect_char_encoding(start.as_ptr(), 4);
                        /*
                         * We need more bytes to detect EBCDIC code pages.
                         * See xmlDetectEBCDIC.
                         */
                        if matches!(enc, XmlCharEncoding::XmlCharEncodingEbcdic)
                            && terminate == 0
                            && avail < 200
                        {
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
                                (*ctxt).user_data,
                                xml_default_sax_locator(),
                            );
                        }
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEmpty, null());
                        xml_halt_parser(ctxt);
                        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).end_document.is_some() {
                            ((*(*ctxt).sax).end_document.unwrap())((*ctxt).user_data);
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
                                (*ctxt).user_data,
                                xml_default_sax_locator(),
                            );
                        }
                        if *(*(*ctxt).input).cur.add(2) == b'x'
                            && *(*(*ctxt).input).cur.add(3) == b'm'
                            && *(*(*ctxt).input).cur.add(4) == b'l'
                            && IS_BLANK_CH!(*(*(*ctxt).input).cur.add(5))
                        {
                            ret += 5;
                            // #ifdef DEBUG_PUSH
                            // 			xmlGenericError(xmlGenericErrorContext,
                            // 				c"PP: Parsing XML Decl\n".as_ptr() as _);
                            // #endif
                            xml_parse_xmldecl(ctxt);
                            if (*ctxt).err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
                                /*
                                 * The XML REC instructs us to stop parsing right
                                 * here
                                 */
                                xml_halt_parser(ctxt);
                                return 0;
                            }
                            (*ctxt).standalone = (*(*ctxt).input).standalone;
                            if (*ctxt).encoding.is_null() && !(*(*ctxt).input).encoding.is_null() {
                                (*ctxt).encoding = xml_strdup((*(*ctxt).input).encoding);
                            }
                            if !(*ctxt).sax.is_null()
                                && (*(*ctxt).sax).start_document.is_some()
                                && (*ctxt).disable_sax == 0
                            {
                                ((*(*ctxt).sax).start_document.unwrap())((*ctxt).user_data);
                            }
                            (*ctxt).instate = XmlParserInputState::XmlParserMisc;
                        // #ifdef DEBUG_PUSH
                        // 			xmlGenericError(xmlGenericErrorContext,
                        // 				c"PP: entering MISC\n".as_ptr() as _);
                        // #endif
                        } else {
                            (*ctxt).version = xml_char_strdup(XML_DEFAULT_VERSION.as_ptr() as _);
                            if !(*ctxt).sax.is_null()
                                && (*(*ctxt).sax).start_document.is_some()
                                && (*ctxt).disable_sax == 0
                            {
                                ((*(*ctxt).sax).start_document.unwrap())((*ctxt).user_data);
                            }
                            (*ctxt).instate = XmlParserInputState::XmlParserMisc;
                            // #ifdef DEBUG_PUSH
                            // 			xmlGenericError(xmlGenericErrorContext,
                            // 				c"PP: entering MISC\n".as_ptr() as _);
                            // #endif
                        }
                    } else {
                        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).set_document_locator.is_some() {
                            ((*(*ctxt).sax).set_document_locator.unwrap())(
                                (*ctxt).user_data,
                                xml_default_sax_locator(),
                            );
                        }
                        (*ctxt).version = xml_char_strdup(XML_DEFAULT_VERSION.as_ptr() as _);
                        if (*ctxt).version.is_null() {
                            xml_err_memory(ctxt, null());
                            break 'to_break;
                        }
                        if !(*ctxt).sax.is_null()
                            && (*(*ctxt).sax).start_document.is_some()
                            && (*ctxt).disable_sax == 0
                        {
                            ((*(*ctxt).sax).start_document.unwrap())((*ctxt).user_data);
                        }
                        (*ctxt).instate = XmlParserInputState::XmlParserMisc;
                        // #ifdef DEBUG_PUSH
                        // 		    xmlGenericError(xmlGenericErrorContext,
                        // 			    c"PP: entering MISC\n".as_ptr() as _);
                        // #endif
                    }
                }
                XmlParserInputState::XmlParserStartTag => {
                    let name: *const XmlChar;
                    let mut prefix: *const XmlChar = null_mut();
                    let mut uri: *const XmlChar = null_mut();
                    let line: c_int = (*(*ctxt).input).line;
                    let ns_nr: c_int = (*ctxt).ns_nr;

                    if avail < 2 && (*ctxt).input_nr == 1 {
                        // goto done;
                        return ret;
                    }
                    cur = *(*(*ctxt).input).cur.add(0);
                    if cur != b'<' {
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEmpty, null());
                        xml_halt_parser(ctxt);
                        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).end_document.is_some() {
                            ((*(*ctxt).sax).end_document.unwrap())((*ctxt).user_data);
                        }
                        // goto done;
                        return ret;
                    }
                    if terminate == 0 && xml_parse_lookup_gt(ctxt) == 0 {
                        // goto done;
                        return ret;
                    }
                    if (*ctxt).space_nr == 0 || *(*ctxt).space == -2 {
                        space_push(ctxt, -1);
                    } else {
                        space_push(ctxt, *(*ctxt).space);
                    }
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
                        // goto done;
                        return ret;
                    }
                    if name.is_null() {
                        space_pop(ctxt);
                        xml_halt_parser(ctxt);
                        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).end_document.is_some() {
                            ((*(*ctxt).sax).end_document.unwrap())((*ctxt).user_data);
                        }
                        // goto done;
                        return ret;
                    }
                    /*
                     * [ VC: Root Element Type ]
                     * The Name in the document type declaration must match
                     * the element type of the root element.
                     */
                    #[cfg(feature = "valid")]
                    if (*ctxt).validate != 0
                        && (*ctxt).well_formed != 0
                        && !(*ctxt).my_doc.is_null()
                        && !(*ctxt).node.is_null()
                        && (*ctxt).node == (*(*ctxt).my_doc).children
                    {
                        (*ctxt).valid &=
                            xml_validate_root(addr_of_mut!((*ctxt).vctxt) as _, (*ctxt).my_doc);
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
                                ((*(*ctxt).sax).end_element_ns.unwrap())(
                                    (*ctxt).user_data,
                                    name,
                                    prefix,
                                    uri,
                                );
                            }
                            if (*ctxt).ns_nr - ns_nr > 0 {
                                ns_pop(ctxt, (*ctxt).ns_nr - ns_nr);
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
                        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                            // goto done;
                            return ret;
                        }
                        space_pop(ctxt);
                        if (*ctxt).name_nr == 0 {
                            (*ctxt).instate = XmlParserInputState::XmlParserEpilog;
                        } else {
                            (*ctxt).instate = XmlParserInputState::XmlParserContent;
                        }
                        // break;
                    } else {
                        if RAW!(ctxt) == b'>' {
                            NEXT!(ctxt);
                        } else {
                            xml_fatal_err_msg_str(
                                ctxt,
                                XmlParserErrors::XmlErrGtRequired,
                                c"Couldn't find end of Start Tag %s\n".as_ptr() as _,
                                name,
                            );
                            node_pop(ctxt);
                            space_pop(ctxt);
                        }
                        name_ns_push(ctxt, name, prefix, uri, line, (*ctxt).ns_nr - ns_nr);

                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                        // break;
                    }
                }
                XmlParserInputState::XmlParserContent => 'to_break: {
                    if avail < 2 && (*ctxt).input_nr == 1 {
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
                        SKIP!(ctxt, 9);
                        (*ctxt).instate = XmlParserInputState::XmlParserCDATASection;
                        break 'to_break;
                    } else if cur == b'<' && next == b'!' && avail < 9 {
                        // goto done;
                        return ret;
                    } else if cur == b'<' {
                        xml_fatal_err(
                            ctxt,
                            XmlParserErrors::XmlErrInternalError,
                            c"detected an error in element content\n".as_ptr() as _,
                        );
                        SKIP!(ctxt, 1);
                    } else if cur == b'&' {
                        if terminate == 0 && xml_parse_lookup_char(ctxt, b';' as _) == 0 {
                            // goto done;
                            return ret;
                        }
                        xml_parse_reference(ctxt);
                    } else {
                        /* TODO Avoid the extra copy, handle directly !!! */
                        /*
                         * Goal of the following test is:
                         *  - minimize calls to the SAX 'character' callback
                         *    when they are mergeable
                         *  - handle an problem for isBlank when we only parse
                         *    a sequence of blank chars and the next one is
                         *    not available to check against '<' presence.
                         *  - tries to homogenize the differences in SAX
                         *    callbacks between the push and pull versions
                         *    of the parser.
                         */
                        if ((*ctxt).input_nr == 1 && avail < XML_PARSER_BIG_BUFFER_SIZE)
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
                        xml_parse_end_tag2(
                            ctxt,
                            (*ctxt).push_tab.add((*ctxt).name_nr as usize - 1),
                        );
                        name_ns_pop(ctxt);
                    } else {
                        #[cfg(feature = "sax1")]
                        {
                            xml_parse_end_tag1(ctxt, 0);
                        }
                    }
                    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                        /* Nothing */
                    } else if (*ctxt).name_nr == 0 {
                        (*ctxt).instate = XmlParserInputState::XmlParserEpilog;
                    } else {
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    }
                }
                XmlParserInputState::XmlParserCDATASection => {
                    /*
                     * The Push mode need to have the SAX callback for
                     * cdataBlock merge back contiguous callbacks.
                     */

                    let term = if terminate != 0 {
                        /*
                         * Don't call xmlParseLookupString. If 'terminate'
                         * is set, checkIndex is invalid.
                         */
                        strstr((*(*ctxt).input).cur as _, c"]]>".as_ptr() as _) as _
                    } else {
                        xml_parse_lookup_string(ctxt, 0, c"]]>".as_ptr() as _, 3)
                    };

                    if term.is_null() {
                        let mut tmp: c_int;
                        let size: c_int;

                        if terminate != 0 {
                            /* Unfinished CDATA section */
                            size = (*(*ctxt).input).end.offset_from((*(*ctxt).input).cur) as _;
                        } else {
                            if avail < XML_PARSER_BIG_BUFFER_SIZE + 2 {
                                // goto done;
                                return ret;
                            }
                            (*ctxt).check_index = 0;
                            /* XXX: Why don't we pass the full buffer? */
                            size = XML_PARSER_BIG_BUFFER_SIZE as i32;
                        }
                        tmp = xml_check_cdata_push((*(*ctxt).input).cur, size, 0);
                        if tmp <= 0 {
                            tmp = -tmp;
                            (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(tmp as usize);
                            break 'encoding_error;
                        }
                        if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                            if let Some(cdata_block) = (*(*ctxt).sax).cdata_block {
                                cdata_block((*ctxt).user_data, (*(*ctxt).input).cur, tmp);
                            } else if let Some(characters) = (*(*ctxt).sax).characters {
                                characters((*ctxt).user_data, (*(*ctxt).input).cur, tmp);
                            }
                        }
                        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                            // goto done;
                            return ret;
                        }
                        SKIPL!(ctxt, tmp);
                    } else {
                        let base: c_int = term.offset_from(CUR_PTR!(ctxt)) as i32;
                        let mut tmp: c_int;

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
                            /*
                             * Special case to provide identical behaviour
                             * between pull and push parsers on enpty CDATA
                             * sections
                             */
                            if (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) >= 9
                                && strncmp(
                                    (*(*ctxt).input).cur.sub(9) as _,
                                    c"<![CDATA[".as_ptr() as _,
                                    9,
                                ) == 0
                            {
                                ((*(*ctxt).sax).cdata_block.unwrap())(
                                    (*ctxt).user_data,
                                    c"".as_ptr() as _,
                                    0,
                                );
                            }
                        } else if !(*ctxt).sax.is_null() && base > 0 && (*ctxt).disable_sax == 0 {
                            if let Some(cdata_block) = (*(*ctxt).sax).cdata_block {
                                cdata_block((*ctxt).user_data, (*(*ctxt).input).cur, base);
                            } else if let Some(characters) = (*(*ctxt).sax).characters {
                                characters((*ctxt).user_data, (*(*ctxt).input).cur, base);
                            }
                        }
                        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                            // goto done;
                            return ret;
                        }
                        SKIPL!(ctxt, base + 3);
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                        // #ifdef DEBUG_PUSH
                        // 		    xmlGenericError(xmlGenericErrorContext,
                        // 			    c"PP: entering CONTENT\n".as_ptr() as _);
                        // #endif
                    }
                }
                XmlParserInputState::XmlParserMisc
                | XmlParserInputState::XmlParserProlog
                | XmlParserInputState::XmlParserEpilog => {
                    SKIP_BLANKS!(ctxt);
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
                        // #ifdef DEBUG_PUSH
                        // 		    xmlGenericError(xmlGenericErrorContext,
                        // 			    c"PP: Parsing PI\n".as_ptr() as _);
                        // #endif
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
                        // #ifdef DEBUG_PUSH
                        // 		    xmlGenericError(xmlGenericErrorContext,
                        // 			    c"PP: Parsing Comment\n".as_ptr() as _);
                        // #endif
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
                        // #ifdef DEBUG_PUSH
                        // 		    xmlGenericError(xmlGenericErrorContext,
                        // 			    c"PP: Parsing internal subset\n".as_ptr() as _);
                        // #endif
                        (*ctxt).in_subset = 1;
                        xml_parse_doc_type_decl(ctxt);
                        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                            // goto done;
                            return ret;
                        }
                        if RAW!(ctxt) == b'[' {
                            (*ctxt).instate = XmlParserInputState::XmlParserDTD;
                        // #ifdef DEBUG_PUSH
                        // 			xmlGenericError(xmlGenericErrorContext,
                        // 				c"PP: entering DTD\n".as_ptr() as _);
                        // #endif
                        } else {
                            /*
                             * Create and update the external subset.
                             */
                            (*ctxt).in_subset = 2;
                            if !(*ctxt).sax.is_null()
                                && (*ctxt).disable_sax == 0
                                && (*(*ctxt).sax).external_subset.is_some()
                            {
                                ((*(*ctxt).sax).external_subset.unwrap())(
                                    (*ctxt).user_data,
                                    (*ctxt).int_sub_name,
                                    (*ctxt).ext_sub_system,
                                    (*ctxt).ext_sub_uri,
                                );
                            }
                            (*ctxt).in_subset = 0;
                            xml_clean_special_attr(ctxt);
                            (*ctxt).instate = XmlParserInputState::XmlParserProlog;
                            // #ifdef DEBUG_PUSH
                            // 			xmlGenericError(xmlGenericErrorContext,
                            // 				c"PP: entering PROLOG\n".as_ptr() as _);
                            // #endif
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
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEnd, null());
                        xml_halt_parser(ctxt);
                        // #ifdef DEBUG_PUSH
                        // 		    xmlGenericError(xmlGenericErrorContext,
                        // 			    c"PP: entering EOF\n".as_ptr() as _);
                        // #endif
                        if !(*ctxt).sax.is_null() && (*(*ctxt).sax).end_document.is_some() {
                            ((*(*ctxt).sax).end_document.unwrap())((*ctxt).user_data);
                        }
                        // goto done;
                        return ret;
                    } else {
                        (*ctxt).instate = XmlParserInputState::XmlParserStartTag;
                        // #ifdef DEBUG_PUSH
                        // 		    xmlGenericError(xmlGenericErrorContext,
                        // 			    c"PP: entering START_TAG\n".as_ptr() as _);
                        // #endif
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
                            (*ctxt).user_data,
                            (*ctxt).int_sub_name,
                            (*ctxt).ext_sub_system,
                            (*ctxt).ext_sub_uri,
                        );
                    }
                    (*ctxt).in_subset = 0;
                    xml_clean_special_attr(ctxt);
                    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                        // goto done;
                        return ret;
                    }
                    (*ctxt).instate = XmlParserInputState::XmlParserProlog;
                    // #ifdef DEBUG_PUSH
                    // 		xmlGenericError(xmlGenericErrorContext,
                    // 			c"PP: entering PROLOG\n".as_ptr() as _);
                    // #endif
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
    0
}

/**
 * xmlParseChunk:
 * @ctxt:  an XML parser context
 * @chunk:  an c_char array
 * @size:  the size in byte of the chunk
 * @terminate:  last chunk indicator
 *
 * Parse a Chunk of memory
 *
 * Returns zero if no error, the xmlParserErrors otherwise.
 */
#[cfg(feature = "push")]
pub unsafe extern "C" fn xml_parse_chunk(
    ctxt: XmlParserCtxtPtr,
    chunk: *const c_char,
    mut size: c_int,
    terminate: c_int,
) -> c_int {
    use super::parser_internals::XML_MAX_LOOKUP_LIMIT;

    let mut end_in_lf: c_int = 0;

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
        xml_detect_sax2(ctxt);
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
        && !(*(*ctxt).input).buf.is_null()
        && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        let base: size_t = xml_buf_get_input_base(
            (*(*(*ctxt).input).buf)
                .buffer
                .map_or(null_mut(), |buf| buf.as_ptr()),
            (*ctxt).input,
        );
        let cur: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

        let res: c_int = xml_parser_input_buffer_push((*(*ctxt).input).buf, size, chunk);
        xml_buf_set_input_base_cur(
            (*(*(*ctxt).input).buf)
                .buffer
                .map_or(null_mut(), |buf| buf.as_ptr()),
            (*ctxt).input,
            base,
            cur,
        );
        if res < 0 {
            (*ctxt).err_no = XmlParserInputState::XmlParserEOF as i32;
            xml_halt_parser(ctxt);
            return XmlParserInputState::XmlParserEOF as i32;
        }
    } else if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        && (!(*ctxt).input.is_null() && !(*(*ctxt).input).buf.is_null())
    {
        let input: XmlParserInputBufferPtr = (*(*ctxt).input).buf;
        if !(*input).encoder.is_null() && (*input).buffer.is_some() && !(*input).raw.is_null() {
            let base: size_t =
                xml_buf_get_input_base((*input).buffer.unwrap().as_ptr(), (*ctxt).input);
            let current: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

            let nbchars: c_int = xml_char_enc_input(input, terminate);
            xml_buf_set_input_base_cur(
                (*input).buffer.unwrap().as_ptr(),
                (*ctxt).input,
                base,
                current,
            );
            if nbchars < 0 {
                /* TODO 2.6.0 */
                generic_error!("xmlParseChunk: encoder error\n");
                xml_halt_parser(ctxt);
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
            c"Huge input lookup".as_ptr() as _,
        );
        xml_halt_parser(ctxt);
    }
    if (*ctxt).err_no != XmlParserErrors::XmlErrOK as i32 && (*ctxt).disable_sax == 1 {
        return (*ctxt).err_no;
    }

    if end_in_lf == 1 && !(*ctxt).input.is_null() && !(*(*ctxt).input).buf.is_null() {
        let base: size_t = xml_buf_get_input_base(
            (*(*(*ctxt).input).buf)
                .buffer
                .map_or(null_mut(), |buf| buf.as_ptr()),
            (*ctxt).input,
        );
        let current: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

        xml_parser_input_buffer_push((*(*ctxt).input).buf, 1, c"\r".as_ptr() as _);

        xml_buf_set_input_base_cur(
            (*(*(*ctxt).input).buf)
                .buffer
                .map_or(null_mut(), |buf| buf.as_ptr()),
            (*ctxt).input,
            base,
            current,
        );
    }
    if terminate != 0 {
        /*
         * Check for termination
         */
        if !matches!(
            (*ctxt).instate,
            XmlParserInputState::XmlParserEOF | XmlParserInputState::XmlParserEpilog
        ) {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEnd, null());
        }
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEpilog)
            && (*(*ctxt).input).cur < (*(*ctxt).input).end
        {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrDocumentEnd, null());
        }
        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
            && (!(*ctxt).sax.is_null() && (*(*ctxt).sax).end_document.is_some())
        {
            ((*(*ctxt).sax).end_document.unwrap())((*ctxt).user_data);
        }
        (*ctxt).instate = XmlParserInputState::XmlParserEOF;
    }
    if (*ctxt).well_formed == 0 {
        (*ctxt).err_no
    } else {
        0
    }
}

/*
 * Special I/O mode.
 */

/**
 * xmlCreateIOParserCtxt:
 * @sax:  a SAX handler
 * @user_data:  The user data returned on SAX callbacks
 * @ioread:  an I/O read function
 * @ioclose:  an I/O close function
 * @ioctx:  an I/O handler
 * @enc:  the charset encoding if known
 *
 * Create a parser context for using the XML parser with an existing
 * I/O stream
 *
 * Returns the new parser context or NULL
 */
pub unsafe extern "C" fn xml_create_io_parser_ctxt(
    sax: XmlSAXHandlerPtr,
    user_data: *mut c_void,
    ioread: Option<XmlInputReadCallback>,
    ioclose: Option<XmlInputCloseCallback>,
    ioctx: *mut c_void,
    enc: XmlCharEncoding,
) -> XmlParserCtxtPtr {
    if ioread.is_none() {
        return null_mut();
    }

    let buf: XmlParserInputBufferPtr =
        xml_parser_input_buffer_create_io(ioread, ioclose, ioctx, enc);
    if buf.is_null() {
        if let Some(ioclose) = ioclose {
            ioclose(ioctx);
        }
        return null_mut();
    }

    let ctxt: XmlParserCtxtPtr = xml_new_sax_parser_ctxt(sax, user_data);
    if ctxt.is_null() {
        xml_free_parser_input_buffer(buf);
        return null_mut();
    }

    let input_stream: XmlParserInputPtr = xml_new_io_input_stream(ctxt, buf, enc);
    if input_stream.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }
    input_push(ctxt, input_stream);

    ctxt
}

/**
 * xmlNewIOInputStream:
 * @ctxt:  an XML parser context
 * @input:  an I/O Input
 * @enc:  the charset encoding if known
 *
 * Create a new input stream structure encapsulating the @input into
 * a stream suitable for the parser.
 *
 * Returns the new input stream or NULL
 */
pub unsafe extern "C" fn xml_new_io_input_stream(
    ctxt: XmlParserCtxtPtr,
    input: XmlParserInputBufferPtr,
    enc: XmlCharEncoding,
) -> XmlParserInputPtr {
    if input.is_null() {
        return null_mut();
    }
    if *xml_parser_debug_entities() != 0 {
        generic_error!("new input from I/O\n");
    }
    let input_stream: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input_stream.is_null() {
        return null_mut();
    }
    (*input_stream).filename = null_mut();
    (*input_stream).buf = input;
    xml_buf_reset_input(
        (*(*input_stream).buf)
            .buffer
            .map_or(null_mut(), |buf| buf.as_ptr()),
        input_stream,
    );

    if !matches!(enc, XmlCharEncoding::XmlCharEncodingNone) {
        xml_switch_encoding(ctxt, enc);
    }

    input_stream
}

/*
 * Node infos.
 */
/**
 * xmlParserFindNodeInfo:
 * @ctx:  an XML parser context
 * @node:  an XML node within the tree
 *
 * DEPRECATED: Don't use.
 *
 * Find the parser node info struct for a given node
 *
 * Returns an xmlParserNodeInfo block pointer or NULL
 */
pub unsafe extern "C" fn xml_parser_find_node_info(
    ctxt: XmlParserCtxtPtr,
    node: XmlNodePtr,
) -> *const XmlParserNodeInfo {
    if ctxt.is_null() || node.is_null() {
        return null_mut();
    }
    /* Find position where node should be at */
    let pos: c_ulong = xml_parser_find_node_info_index(addr_of_mut!((*ctxt).node_seq), node);
    if pos < (*ctxt).node_seq.length && (*(*ctxt).node_seq.buffer.add(pos as usize)).node == node {
        (*ctxt).node_seq.buffer.add(pos as usize)
    } else {
        null_mut()
    }
}

/**
 * xmlInitNodeInfoSeq:
 * @seq:  a node info sequence pointer
 *
 * DEPRECATED: Don't use.
 *
 * -- Initialize (set to initial state) node info sequence
 */
pub unsafe extern "C" fn xml_init_node_info_seq(seq: XmlParserNodeInfoSeqPtr) {
    if seq.is_null() {
        return;
    }
    (*seq).length = 0;
    (*seq).maximum = 0;
    (*seq).buffer = null_mut();
}

/**
 * xmlClearNodeInfoSeq:
 * @seq:  a node info sequence pointer
 *
 * DEPRECATED: Don't use.
 *
 * -- Clear (release memory and reinitialize) node
 *   info sequence
 */
pub unsafe extern "C" fn xml_clear_node_info_seq(seq: XmlParserNodeInfoSeqPtr) {
    if seq.is_null() {
        return;
    }
    if !(*seq).buffer.is_null() {
        xml_free((*seq).buffer as _);
    }
    xml_init_node_info_seq(seq);
}

/**
 * xmlParserFindNodeInfoIndex:
 * @seq:  a node info sequence pointer
 * @node:  an XML node pointer
 *
 * DEPRECATED: Don't use.
 *
 * xmlParserFindNodeInfoIndex : Find the index that the info record for
 *   the given node is or should be at in a sorted sequence
 *
 * Returns a long indicating the position of the record
 */
pub unsafe extern "C" fn xml_parser_find_node_info_index(
    seq: XmlParserNodeInfoSeqPtr,
    node: XmlNodePtr,
) -> c_ulong {
    let mut upper: c_ulong;
    let mut lower: c_ulong;
    let mut middle: c_ulong;
    let mut found: c_int = 0;

    if seq.is_null() || node.is_null() {
        return c_ulong::MAX;
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

/**
 * xmlParserAddNodeInfo:
 * @ctxt:  an XML parser context
 * @info:  a node info sequence pointer
 *
 * DEPRECATED: Don't use.
 *
 * Insert node info record into the sorted sequence
 */
pub unsafe extern "C" fn xml_parser_add_node_info(
    ctxt: XmlParserCtxtPtr,
    info: XmlParserNodeInfoPtr,
) {
    if ctxt.is_null() || info.is_null() {
        return;
    }

    /* Find pos and check to see if node is already in the sequence */
    let pos: c_ulong =
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
            let byte_size: c_uint = (size_of_val(&*(*ctxt).node_seq.buffer)
                * (2 * (*ctxt).node_seq.maximum as usize)) as _;

            let tmp_buffer: XmlParserNodeInfoPtr = if (*ctxt).node_seq.buffer.is_null() {
                xml_malloc(byte_size as usize) as _
            } else {
                xml_realloc((*ctxt).node_seq.buffer as _, byte_size as usize) as _
            };

            if tmp_buffer.is_null() {
                xml_err_memory(ctxt, c"failed to allocate buffer\n".as_ptr() as _);
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

/*
 * External entities handling actually implemented in xmlIO.
 */

/**
 * xmlSetExternalEntityLoader:
 * @f:  the new entity resolver function
 *
 * Changes the defaultexternal entity resolver function for the application
 */
pub unsafe extern "C" fn xml_set_external_entity_loader(f: XmlExternalEntityLoader) {
    XML_CURRENT_EXTERNAL_ENTITY_LOADER = f;
}

/**
 * xmlGetExternalEntityLoader:
 *
 * Get the default external entity resolver function for the application
 *
 * Returns the xmlExternalEntityLoader function poc_inter
 */
pub unsafe extern "C" fn xml_get_external_entity_loader() -> XmlExternalEntityLoader {
    XML_CURRENT_EXTERNAL_ENTITY_LOADER
}

/**
 * xmlLoadExternalEntity:
 * @URL:  the URL for the entity to load
 * @ID:  the Public ID for the entity to load
 * @ctxt:  the context in which the entity is called or NULL
 *
 * Load an external entity, note that the use of this function for
 * unparsed entities may generate problems
 *
 * Returns the xmlParserInputPtr or NULL
 */
pub unsafe extern "C" fn xml_load_external_entity(
    url: *const c_char,
    id: *const c_char,
    ctxt: XmlParserCtxtPtr,
) -> XmlParserInputPtr {
    if !url.is_null() && xml_no_net_exists(url) == 0 {
        let canonic_filename: *mut c_char = xml_canonic_path(url as _) as _;
        if canonic_filename.is_null() {
            xml_ioerr_memory(c"building canonical path\n".as_ptr() as _);
            return null_mut();
        }

        let ret: XmlParserInputPtr = XML_CURRENT_EXTERNAL_ENTITY_LOADER(canonic_filename, id, ctxt);
        xml_free(canonic_filename as _);
        return ret;
    }
    XML_CURRENT_EXTERNAL_ENTITY_LOADER(url, id, ctxt)
}

/*
 * Index lookup, actually implemented in the encoding module
 */
/**
 * xmlByteConsumed:
 * @ctxt: an XML parser context
 *
 * This function provides the current index of the parser relative
 * to the start of the current entity. This function is computed in
 * bytes from the beginning starting at zero and finishing at the
 * size in byte of the file if parsing a file. The function is
 * of constant cost if the input is UTF-8 but can be costly if run
 * on non-UTF-8 input.
 *
 * Returns the index in bytes from the beginning of the entity or -1
 *         in case the index could not be computed.
 */
pub unsafe extern "C" fn xml_byte_consumed(ctxt: XmlParserCtxtPtr) -> c_long {
    if ctxt.is_null() {
        return -1;
    }
    let input: XmlParserInputPtr = (*ctxt).input;
    if input.is_null() {
        return -1;
    }
    if !(*input).buf.is_null() && !(*(*input).buf).encoder.is_null() {
        let mut unused: c_uint = 0;
        let handler: *mut XmlCharEncodingHandler = (*(*input).buf).encoder;
        /*
         * Encoding conversion, compute the number of unused original
         * bytes from the input not consumed and subtract that from
         * the raw consumed value, this is not a cheap operation
         */
        if (*input).end.offset_from((*input).cur) > 0 {
            let mut convbuf: [c_uchar; 32000] = [0; 32000];
            let mut cur: *const c_uchar = (*input).cur as *const c_uchar;
            let mut toconv: c_int;
            let mut written: c_int;

            let mut ret: c_int;

            while {
                toconv = (*input).end.offset_from(cur) as _;
                written = 32000;
                ret = xml_enc_output_chunk(
                    handler,
                    addr_of_mut!(convbuf[0]),
                    addr_of_mut!(written),
                    cur,
                    addr_of_mut!(toconv),
                );
                if ret < 0 {
                    if written > 0 {
                        ret = -2;
                    } else {
                        return -1;
                    }
                }
                unused += written as u32;
                cur = cur.add(toconv as usize);

                ret == -2
            } {}
        }
        if (*(*input).buf).rawconsumed < unused as u64 {
            return -1;
        }
        return ((*(*input).buf).rawconsumed - unused as u64) as i64;
    }
    (*input).consumed as i64 + (*input).cur.offset_from((*input).base) as i64
}

/*
 * New set of simpler/more flexible APIs
 */
/**
 * xmlParserOption:
 *
 * This is the set of XML parser options that can be passed down
 * to the xmlReadDoc() and similar calls.
 */
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

/**
 * DICT_FREE:
 * @str:  a string
 *
 * Free a string if it is not owned by the "dict" dictionary in the
 * current scope
 */
macro_rules! DICT_FREE {
    ($dict:expr, $str:expr) => {
        if !$str.is_null()
            && ($dict.is_null() || crate::libxml::dict::xml_dict_owns($dict, $str as _) == 0)
        {
            xml_free($str as _);
        }
    };
}

/**
 * xmlCtxtReset:
 * @ctxt: an XML parser context
 *
 * Reset a parser context
 */
pub unsafe extern "C" fn xml_ctxt_reset(ctxt: XmlParserCtxtPtr) {
    let mut input: XmlParserInputPtr;

    if ctxt.is_null() {
        return;
    }

    let dict: XmlDictPtr = (*ctxt).dict;

    while {
        input = input_pop(ctxt);
        !input.is_null()
    } {
        /* Non consuming */
        xml_free_input_stream(input);
    }
    (*ctxt).input_nr = 0;
    (*ctxt).input = null_mut();

    (*ctxt).space_nr = 0;
    if !(*ctxt).space_tab.is_null() {
        *(*ctxt).space_tab.add(0) = -1;
        (*ctxt).space = (*ctxt).space_tab.add(0);
    } else {
        (*ctxt).space = null_mut();
    }

    (*ctxt).node_nr = 0;
    (*ctxt).node = null_mut();

    (*ctxt).name_nr = 0;
    (*ctxt).name = null_mut();

    (*ctxt).ns_nr = 0;

    DICT_FREE!(dict, (*ctxt).version);
    (*ctxt).version = null_mut();
    DICT_FREE!(dict, (*ctxt).encoding);
    (*ctxt).encoding = null_mut();
    DICT_FREE!(dict, (*ctxt).directory);
    (*ctxt).directory = null_mut();
    DICT_FREE!(dict, (*ctxt).ext_sub_uri);
    (*ctxt).ext_sub_uri = null_mut();
    DICT_FREE!(dict, (*ctxt).ext_sub_system);
    (*ctxt).ext_sub_system = null_mut();
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
    // #if 0
    //     (*ctxt).vctxt.userData = ctxt;
    //     (*ctxt).vctxt.error = xmlParserValidityError;
    //     (*ctxt).vctxt.warning = xmlParserValidityWarning;
    // #endif
    (*ctxt).record_info = 0;
    (*ctxt).check_index = 0;
    (*ctxt).end_check_state = 0;
    (*ctxt).in_subset = 0;
    (*ctxt).err_no = XmlParserErrors::XmlErrOK as i32;
    (*ctxt).depth = 0;
    (*ctxt).charset = XmlCharEncoding::XmlCharEncodingUtf8 as i32;
    (*ctxt).catalogs = null_mut();
    (*ctxt).sizeentities = 0;
    (*ctxt).sizeentcopy = 0;
    xml_init_node_info_seq(addr_of_mut!((*ctxt).node_seq));

    if !(*ctxt).atts_default.is_null() {
        xml_hash_free((*ctxt).atts_default, Some(xml_hash_default_deallocator));
        (*ctxt).atts_default = null_mut();
    }
    if !(*ctxt).atts_special.is_null() {
        xml_hash_free((*ctxt).atts_special, None);
        (*ctxt).atts_special = null_mut();
    }

    #[cfg(feature = "catalog")]
    if !(*ctxt).catalogs.is_null() {
        xml_catalog_free_local((*ctxt).catalogs);
    }
    (*ctxt).nb_errors = 0;
    (*ctxt).nb_warnings = 0;
    if (*ctxt).last_error.code != XmlParserErrors::XmlErrOK as i32 {
        xml_reset_error(addr_of_mut!((*ctxt).last_error));
    }
}

/**
 * xmlCtxtResetPush:
 * @ctxt: an XML parser context
 * @chunk:  a pointer to an array of chars
 * @size:  number of chars in the array
 * @filename:  an optional file name or URI
 * @encoding:  the document encoding, or NULL
 *
 * Reset a push parser context
 *
 * Returns 0 in case of success and 1 in case of error
 */
pub unsafe extern "C" fn xml_ctxt_reset_push(
    ctxt: XmlParserCtxtPtr,
    chunk: *const c_char,
    size: c_int,
    filename: *const c_char,
    encoding: *const c_char,
) -> c_int {
    let mut enc: XmlCharEncoding = XmlCharEncoding::XmlCharEncodingNone;

    if ctxt.is_null() {
        return 1;
    }

    if encoding.is_null() && !chunk.is_null() && size >= 4 {
        enc = xml_detect_char_encoding(chunk as _, size);
    }

    let buf: XmlParserInputBufferPtr = xml_alloc_parser_input_buffer(enc);
    if buf.is_null() {
        return 1;
    }

    if ctxt.is_null() {
        xml_free_parser_input_buffer(buf);
        return 1;
    }

    xml_ctxt_reset(ctxt);

    if filename.is_null() {
        (*ctxt).directory = null_mut();
    } else {
        (*ctxt).directory = xml_parser_get_directory(filename);
    }

    let input_stream: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input_stream.is_null() {
        xml_free_parser_input_buffer(buf);
        return 1;
    }

    if filename.is_null() {
        (*input_stream).filename = null_mut();
    } else {
        (*input_stream).filename = xml_canonic_path(filename as _) as _;
    }
    (*input_stream).buf = buf;
    xml_buf_reset_input(
        (*buf).buffer.map_or(null_mut(), |buf| buf.as_ptr()),
        input_stream,
    );

    input_push(ctxt, input_stream);

    if size > 0 && !chunk.is_null() && !(*ctxt).input.is_null() && !(*(*ctxt).input).buf.is_null() {
        let base: size_t = xml_buf_get_input_base(
            (*(*(*ctxt).input).buf)
                .buffer
                .map_or(null_mut(), |buf| buf.as_ptr()),
            (*ctxt).input,
        );
        let cur: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

        xml_parser_input_buffer_push((*(*ctxt).input).buf, size, chunk);

        xml_buf_set_input_base_cur(
            (*(*(*ctxt).input).buf)
                .buffer
                .map_or(null_mut(), |buf| buf.as_ptr()),
            (*ctxt).input,
            base,
            cur,
        );
    }

    if !encoding.is_null() {
        if !(*ctxt).encoding.is_null() {
            xml_free((*ctxt).encoding as _);
        }
        (*ctxt).encoding = xml_strdup(encoding as _);

        let hdlr: XmlCharEncodingHandlerPtr = xml_find_char_encoding_handler(encoding);
        if !hdlr.is_null() {
            xml_switch_to_encoding(ctxt, hdlr);
        } else {
            xml_fatal_err_msg_str(
                ctxt,
                XmlParserErrors::XmlErrUnsupportedEncoding,
                c"Unsupported encoding %s\n".as_ptr() as _,
                encoding as _,
            );
        }
    } else if !matches!(enc, XmlCharEncoding::XmlCharEncodingNone) {
        xml_switch_encoding(ctxt, enc);
    }

    0
}

/**
 * xmlCtxtUseOptions:
 * @ctxt: an XML parser context
 * @options:  a combination of xmlParserOption
 *
 * Applies the options to the parser context
 *
 * Returns 0 in case of success, the set of unknown or unimplemented options
 *         in case of error.
 */
pub unsafe extern "C" fn xml_ctxt_use_options(ctxt: XmlParserCtxtPtr, options: c_int) -> c_int {
    xml_ctxt_use_options_internal(ctxt, options, null())
}

/**
 * xmlDoRead:
 * @ctxt:  an XML parser context
 * @URL:  the base URL to use for the document
 * @encoding:  the document encoding, or NULL
 * @options:  a combination of xmlParserOption
 * @reuse:  keep the context for reuse
 *
 * Common front-end for the xmlRead functions
 *
 * Returns the resulting document tree or NULL
 */
unsafe extern "C" fn xml_do_read(
    ctxt: XmlParserCtxtPtr,
    url: *const c_char,
    encoding: *const c_char,
    options: c_int,
    reuse: c_int,
) -> XmlDocPtr {
    let ret: XmlDocPtr;

    xml_ctxt_use_options_internal(ctxt, options, encoding);
    if !encoding.is_null() {
        /*
         * TODO: We should consider to set XML_PARSE_IGNORE_ENC if the
         * caller provided an encoding. Otherwise, we might match to
         * the encoding from the XML declaration which is likely to
         * break things. Also see xmlSwitchInputEncoding.
         */
        let hdlr: XmlCharEncodingHandlerPtr = xml_find_char_encoding_handler(encoding);
        if !hdlr.is_null() {
            xml_switch_to_encoding(ctxt, hdlr);
        }
    }
    if !url.is_null() && !(*ctxt).input.is_null() && (*(*ctxt).input).filename.is_null() {
        (*(*ctxt).input).filename = xml_strdup(url as _) as _;
    }
    xml_parse_document(ctxt);
    if (*ctxt).well_formed != 0 || (*ctxt).recovery != 0 {
        ret = (*ctxt).my_doc;
    } else {
        ret = null_mut();
        if !(*ctxt).my_doc.is_null() {
            xml_free_doc((*ctxt).my_doc);
        }
    }
    (*ctxt).my_doc = null_mut();
    if reuse == 0 {
        xml_free_parser_ctxt(ctxt);
    }

    ret
}

/**
 * xmlReadDoc:
 * @cur:  a pointer to a zero terminated string
 * @URL:  the base URL to use for the document
 * @encoding:  the document encoding, or NULL
 * @options:  a combination of xmlParserOption
 *
 * parse an XML in-memory document and build a tree.
 *
 * Returns the resulting document tree
 */
pub unsafe extern "C" fn xml_read_doc(
    cur: *const XmlChar,
    url: *const c_char,
    encoding: *const c_char,
    options: c_int,
) -> XmlDocPtr {
    if cur.is_null() {
        return null_mut();
    }
    xml_init_parser();

    let ctxt: XmlParserCtxtPtr = xml_create_doc_parser_ctxt(cur);
    if ctxt.is_null() {
        return null_mut();
    }
    xml_do_read(ctxt, url, encoding, options, 0)
}

/**
 * xmlReadFile:
 * @filename:  a file or URL
 * @encoding:  the document encoding, or NULL
 * @options:  a combination of xmlParserOption
 *
 * parse an XML file from the filesystem or the network.
 *
 * Returns the resulting document tree
 */
pub unsafe extern "C" fn xml_read_file(
    filename: *const c_char,
    encoding: *const c_char,
    options: c_int,
) -> XmlDocPtr {
    xml_init_parser();
    let ctxt: XmlParserCtxtPtr = xml_create_url_parser_ctxt(filename, options);
    if ctxt.is_null() {
        return null_mut();
    }
    xml_do_read(ctxt, null_mut(), encoding, options, 0)
}

/**
 * xmlReadMemory:
 * @buffer:  a pointer to a c_char array
 * @size:  the size of the array
 * @URL:  the base URL to use for the document
 * @encoding:  the document encoding, or NULL
 * @options:  a combination of xmlParserOption
 *
 * parse an XML in-memory document and build a tree.
 *
 * Returns the resulting document tree
 */
pub unsafe extern "C" fn xml_read_memory(
    buffer: *const c_char,
    size: c_int,
    url: *const c_char,
    encoding: *const c_char,
    options: c_int,
) -> XmlDocPtr {
    xml_init_parser();
    let ctxt: XmlParserCtxtPtr = xml_create_memory_parser_ctxt(buffer, size);

    if ctxt.is_null() {
        return null_mut();
    }
    xml_do_read(ctxt, url, encoding, options, 0)
}

/**
 * xmlReadFd:
 * @fd:  an open file descriptor
 * @URL:  the base URL to use for the document
 * @encoding:  the document encoding, or NULL
 * @options:  a combination of xmlParserOption
 *
 * parse an XML from a file descriptor and build a tree.
 * NOTE that the file descriptor will not be closed when the
 *      reader is closed or reset.
 *
 * Returns the resulting document tree
 */
pub unsafe extern "C" fn xml_read_fd(
    fd: c_int,
    url: *const c_char,
    encoding: *const c_char,
    options: c_int,
) -> XmlDocPtr {
    if fd < 0 {
        return null_mut();
    }
    xml_init_parser();

    let input: XmlParserInputBufferPtr =
        xml_parser_input_buffer_create_fd(fd, XmlCharEncoding::XmlCharEncodingNone);
    if input.is_null() {
        return null_mut();
    }
    (*input).closecallback = None;
    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        xml_free_parser_input_buffer(input);
        return null_mut();
    }
    let stream: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, input, XmlCharEncoding::XmlCharEncodingNone);
    if stream.is_null() {
        xml_free_parser_input_buffer(input);
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }
    input_push(ctxt, stream);
    xml_do_read(ctxt, url, encoding, options, 0)
}

/**
 * xmlReadIO:
 * @ioread:  an I/O read function
 * @ioclose:  an I/O close function
 * @ioctx:  an I/O handler
 * @URL:  the base URL to use for the document
 * @encoding:  the document encoding, or NULL
 * @options:  a combination of xmlParserOption
 *
 * parse an XML document from I/O functions and source and build a tree.
 *
 * Returns the resulting document tree
 */
pub unsafe extern "C" fn xml_read_io(
    ioread: Option<XmlInputReadCallback>,
    ioclose: Option<XmlInputCloseCallback>,
    ioctx: *mut c_void,
    url: *const c_char,
    encoding: *const c_char,
    options: c_int,
) -> XmlDocPtr {
    if ioread.is_none() {
        return null_mut();
    }
    xml_init_parser();

    let input: XmlParserInputBufferPtr = xml_parser_input_buffer_create_io(
        ioread,
        ioclose,
        ioctx,
        XmlCharEncoding::XmlCharEncodingNone,
    );
    if input.is_null() {
        if let Some(ioclose) = ioclose {
            ioclose(ioctx);
        }
        return null_mut();
    }
    let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
    if ctxt.is_null() {
        xml_free_parser_input_buffer(input);
        return null_mut();
    }
    let stream: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, input, XmlCharEncoding::XmlCharEncodingNone);
    if stream.is_null() {
        xml_free_parser_input_buffer(input);
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }
    input_push(ctxt, stream);
    xml_do_read(ctxt, url, encoding, options, 0)
}

/**
 * xmlCtxtReadDoc:
 * @ctxt:  an XML parser context
 * @cur:  a pointer to a zero terminated string
 * @URL:  the base URL to use for the document
 * @encoding:  the document encoding, or NULL
 * @options:  a combination of xmlParserOption
 *
 * parse an XML in-memory document and build a tree.
 * This reuses the existing @ctxt parser context
 *
 * Returns the resulting document tree
 */
pub unsafe extern "C" fn xml_ctxt_read_doc(
    ctxt: XmlParserCtxtPtr,
    cur: *const XmlChar,
    url: *const c_char,
    encoding: *const c_char,
    options: c_int,
) -> XmlDocPtr {
    if cur.is_null() {
        return null_mut();
    }
    xml_ctxt_read_memory(ctxt, cur as _, xml_strlen(cur), url, encoding, options)
}

/**
 * xmlCtxtReadFile:
 * @ctxt:  an XML parser context
 * @filename:  a file or URL
 * @encoding:  the document encoding, or NULL
 * @options:  a combination of xmlParserOption
 *
 * parse an XML file from the filesystem or the network.
 * This reuses the existing @ctxt parser context
 *
 * Returns the resulting document tree
 */
pub unsafe extern "C" fn xml_ctxt_read_file(
    ctxt: XmlParserCtxtPtr,
    filename: *const c_char,
    encoding: *const c_char,
    options: c_int,
) -> XmlDocPtr {
    if filename.is_null() {
        return null_mut();
    }
    if ctxt.is_null() {
        return null_mut();
    }
    xml_init_parser();

    xml_ctxt_reset(ctxt);

    let stream: XmlParserInputPtr = xml_load_external_entity(filename, null(), ctxt);
    if stream.is_null() {
        return null_mut();
    }
    input_push(ctxt, stream);
    xml_do_read(ctxt, null_mut(), encoding, options, 1)
}

/**
 * xmlCtxtReadMemory:
 * @ctxt:  an XML parser context
 * @buffer:  a pointer to a c_char array
 * @size:  the size of the array
 * @URL:  the base URL to use for the document
 * @encoding:  the document encoding, or NULL
 * @options:  a combination of xmlParserOption
 *
 * parse an XML in-memory document and build a tree.
 * This reuses the existing @ctxt parser context
 *
 * Returns the resulting document tree
 */
pub unsafe extern "C" fn xml_ctxt_read_memory(
    ctxt: XmlParserCtxtPtr,
    buffer: *const c_char,
    size: c_int,
    url: *const c_char,
    encoding: *const c_char,
    options: c_int,
) -> XmlDocPtr {
    if ctxt.is_null() {
        return null_mut();
    }
    if buffer.is_null() {
        return null_mut();
    }
    xml_init_parser();

    xml_ctxt_reset(ctxt);

    let input: XmlParserInputBufferPtr =
        xml_parser_input_buffer_create_mem(buffer, size, XmlCharEncoding::XmlCharEncodingNone);
    if input.is_null() {
        return null_mut();
    }

    let stream: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, input, XmlCharEncoding::XmlCharEncodingNone);
    if stream.is_null() {
        xml_free_parser_input_buffer(input);
        return null_mut();
    }

    input_push(ctxt, stream);
    xml_do_read(ctxt, url, encoding, options, 1)
}

/**
 * xmlCtxtReadFd:
 * @ctxt:  an XML parser context
 * @fd:  an open file descriptor
 * @URL:  the base URL to use for the document
 * @encoding:  the document encoding, or NULL
 * @options:  a combination of xmlParserOption
 *
 * parse an XML from a file descriptor and build a tree.
 * This reuses the existing @ctxt parser context
 * NOTE that the file descriptor will not be closed when the
 *      reader is closed or reset.
 *
 * Returns the resulting document tree
 */
pub unsafe extern "C" fn xml_ctxt_read_fd(
    ctxt: XmlParserCtxtPtr,
    fd: c_int,
    url: *const c_char,
    encoding: *const c_char,
    options: c_int,
) -> XmlDocPtr {
    if fd < 0 {
        return null_mut();
    }
    if ctxt.is_null() {
        return null_mut();
    }
    xml_init_parser();

    xml_ctxt_reset(ctxt);

    let input: XmlParserInputBufferPtr =
        xml_parser_input_buffer_create_fd(fd, XmlCharEncoding::XmlCharEncodingNone);
    if input.is_null() {
        return null_mut();
    }
    (*input).closecallback = None;
    let stream: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, input, XmlCharEncoding::XmlCharEncodingNone);
    if stream.is_null() {
        xml_free_parser_input_buffer(input);
        return null_mut();
    }
    input_push(ctxt, stream);
    xml_do_read(ctxt, url, encoding, options, 1)
}

/**
 * xmlCtxtReadIO:
 * @ctxt:  an XML parser context
 * @ioread:  an I/O read function
 * @ioclose:  an I/O close function
 * @ioctx:  an I/O handler
 * @URL:  the base URL to use for the document
 * @encoding:  the document encoding, or NULL
 * @options:  a combination of xmlParserOption
 *
 * parse an XML document from I/O functions and source and build a tree.
 * This reuses the existing @ctxt parser context
 *
 * Returns the resulting document tree
 */
pub unsafe extern "C" fn xml_ctxt_read_io(
    ctxt: XmlParserCtxtPtr,
    ioread: Option<XmlInputReadCallback>,
    ioclose: Option<XmlInputCloseCallback>,
    ioctx: *mut c_void,
    url: *const c_char,
    encoding: *const c_char,
    options: c_int,
) -> XmlDocPtr {
    if ioread.is_none() {
        return null_mut();
    }
    if ctxt.is_null() {
        return null_mut();
    }
    xml_init_parser();

    xml_ctxt_reset(ctxt);

    let input: XmlParserInputBufferPtr = xml_parser_input_buffer_create_io(
        ioread,
        ioclose,
        ioctx,
        XmlCharEncoding::XmlCharEncodingNone,
    );
    if input.is_null() {
        if let Some(ioclose) = ioclose {
            ioclose(ioctx);
        }
        return null_mut();
    }
    let stream: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, input, XmlCharEncoding::XmlCharEncodingNone);
    if stream.is_null() {
        xml_free_parser_input_buffer(input);
        return null_mut();
    }
    input_push(ctxt, stream);
    xml_do_read(ctxt, url, encoding, options, 1)
}

/*
 * Library wide options
 */
/**
 * xmlFeature:
 *
 * Used to examine the existence of features that can be enabled
 * or disabled at compile-time.
 * They used to be called XML_FEATURE_xxx but this clashed with Expat
 */
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

/**
 * xmlHasFeature:
 * @feature: the feature to be examined
 *
 * Examines if the library has been compiled with a given feature.
 *
 * Returns a non-zero value if the feature exist, otherwise zero.
 * Returns zero (0) if the feature does not exist or an unknown
 * unknown feature is requested, non-zero otherwise.
 */
pub fn xml_has_feature(feature: Option<XmlFeature>) -> bool {
    match feature {
        Some(XmlFeature::XmlWithThread) => {
            cfg!(feature = "thread")
        }
        Some(XmlFeature::XmlWithTree) => {
            cfg!(feature = "tree")
        }
        Some(XmlFeature::XmlWithOutput) => {
            cfg!(feature = "output")
        }
        Some(XmlFeature::XmlWithPush) => {
            cfg!(feature = "push")
        }
        Some(XmlFeature::XmlWithReader) => {
            cfg!(feature = "libxml_reader")
        }
        Some(XmlFeature::XmlWithPattern) => {
            cfg!(feature = "libxml_pattern")
        }
        Some(XmlFeature::XmlWithWriter) => {
            cfg!(feature = "writer")
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
            cfg!(feature = "valid")
        }
        Some(XmlFeature::XmlWithHtml) => {
            cfg!(feature = "html")
        }
        Some(XmlFeature::XmlWithLegacy) => {
            cfg!(feature = "legacy")
        }
        Some(XmlFeature::XmlWithC14n) => {
            cfg!(feature = "libxml_c14n")
        }
        Some(XmlFeature::XmlWithCatalog) => {
            cfg!(feature = "catalog")
        }
        Some(XmlFeature::XmlWithXpath) => {
            cfg!(feature = "xpath")
        }
        Some(XmlFeature::XmlWithXptr) => {
            cfg!(feature = "libxml_xptr")
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
            cfg!(feature = "regexp")
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
            cfg!(feature = "libxml_schematron")
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

/**
 * xmlParseExternalID:
 * @ctxt:  an XML parser context
 * @publicID:  a XmlChar** receiving PubidLiteral
 * @strict: indicate whether we should restrict parsing to only
 *          production [75], see NOTE below
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse an External ID or a Public ID
 *
 * NOTE: Productions [75] and [83] interact badly since [75] can generate
 *       'PUBLIC' S PubidLiteral S SystemLiteral
 *
 * [75] ExternalID ::= 'SYSTEM' S SystemLiteral
 *                   | 'PUBLIC' S PubidLiteral S SystemLiteral
 *
 * [83] PublicID ::= 'PUBLIC' S PubidLiteral
 *
 * Returns the function returns SystemLiteral and in the second
 *                case publicID receives PubidLiteral, is strict is off
 *                it is possible to return NULL and have publicID set.
 */
pub(crate) unsafe extern "C" fn xml_parse_external_id(
    ctxt: XmlParserCtxtPtr,
    public_id: *mut *mut XmlChar,
    strict: c_int,
) -> *mut XmlChar {
    let mut uri: *mut XmlChar = null_mut();

    *public_id = null_mut();
    if CMP6!(CUR_PTR!(ctxt), b'S', b'Y', b'S', b'T', b'E', b'M') {
        SKIP!(ctxt, 6);
        if SKIP_BLANKS!(ctxt) == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                c"Space required after b'SYSTEM'\n".as_ptr() as _,
            );
        }
        uri = xml_parse_system_literal(ctxt);
        if uri.is_null() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrUriRequired, null());
        }
    } else if CMP6!(CUR_PTR!(ctxt), b'P', b'U', b'B', b'L', b'I', b'C') {
        SKIP!(ctxt, 6);
        if SKIP_BLANKS!(ctxt) == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                c"Space required after 'PUBLIC'\n".as_ptr() as _,
            );
        }
        *public_id = xml_parse_pubid_literal(ctxt);
        if (*public_id).is_null() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrPubidRequired, null());
        }
        if strict != 0 {
            /*
             * We don't handle [83] so "S SystemLiteral" is required.
             */
            if SKIP_BLANKS!(ctxt) == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    c"Space required after the Public Identifier\n".as_ptr() as _,
                );
            }
        } else {
            /*
             * We handle [83] so we return immediately, if
             * "S SystemLiteral" is not detected. We skip blanks if no
             * system literal was found, but this is harmless since we must
             * be at the end of a NotationDecl.
             */
            if SKIP_BLANKS!(ctxt) == 0 {
                return null_mut();
            }
            if CUR!(ctxt) != b'\'' && CUR!(ctxt) != b'"' {
                return null_mut();
            }
        }
        uri = xml_parse_system_literal(ctxt);
        if uri.is_null() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrUriRequired, null());
        }
    }
    uri
}

/**
 * xmlParsePubidLiteral:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an XML public literal
 *
 * [12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"
 *
 * Returns the PubidLiteral parsed or NULL.
 */
pub(crate) unsafe extern "C" fn xml_parse_pubid_literal(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    let mut buf: *mut XmlChar;
    let mut len: c_int = 0;
    let mut size: c_int = XML_PARSER_BUFFER_SIZE as i32;
    let max_length: c_int = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };
    let mut cur: XmlChar;
    let stop: XmlChar;
    let oldstate: XmlParserInputState = (*ctxt).instate;

    if RAW!(ctxt) == b'"' {
        NEXT!(ctxt);
        stop = b'"';
    } else if RAW!(ctxt) == b'\'' {
        NEXT!(ctxt);
        stop = b'\'';
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotStarted, null());
        return null_mut();
    }
    buf = xml_malloc_atomic(size as usize) as *mut XmlChar;
    if buf.is_null() {
        xml_err_memory(ctxt, null());
        return null_mut();
    }
    (*ctxt).instate = XmlParserInputState::XmlParserPublicLiteral;
    cur = CUR!(ctxt);
    while IS_PUBIDCHAR_CH!(cur) && cur != stop {
        /* checked */
        if len + 1 >= size {
            size *= 2;
            let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize) as *mut XmlChar;
            if tmp.is_null() {
                xml_err_memory(ctxt, null());
                xml_free(buf as _);
                return null_mut();
            }
            buf = tmp;
        }
        *buf.add(len as usize) = cur;
        len += 1;
        if len > max_length {
            xml_fatal_err(
                ctxt,
                XmlParserErrors::XmlErrNameTooLong,
                c"Public ID".as_ptr() as _,
            );
            xml_free(buf as _);
            return null_mut();
        }
        NEXT!(ctxt);
        cur = CUR!(ctxt);
    }
    *buf.add(len as usize) = 0;
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        xml_free(buf as _);
        return null_mut();
    }
    if cur != stop {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrLiteralNotFinished, null());
    } else {
        NEXTL!(ctxt, 1);
    }
    (*ctxt).instate = oldstate;
    buf
}

/**
 * xmlParseNotationDecl:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse a notation declaration. Always consumes '<!'.
 *
 * [82] NotationDecl ::= '<!NOTATION' S Name S (ExternalID |  PublicID) S? '>'
 *
 * Hence there is actually 3 choices:
 *     'PUBLIC' S PubidLiteral
 *     'PUBLIC' S PubidLiteral S SystemLiteral
 * and 'SYSTEM' S SystemLiteral
 *
 * See the NOTE on xmlParseExternalID().
 */
pub(crate) unsafe extern "C" fn xml_parse_notation_decl(ctxt: XmlParserCtxtPtr) {
    let name: *const XmlChar;
    let mut pubid: *mut XmlChar = null_mut();
    let systemid: *mut XmlChar;

    if CUR!(ctxt) != b'<' || NXT!(ctxt, 1) != b'!' {
        return;
    }
    SKIP!(ctxt, 2);

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
        let inputid: c_int = (*(*ctxt).input).id;
        SKIP!(ctxt, 8);
        if SKIP_BLANKS!(ctxt) == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                c"Space required after '<!NOTATION'\n".as_ptr() as _,
            );
            return;
        }

        name = xml_parse_name(ctxt);
        if name.is_null() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotStarted, null());
            return;
        }
        if !xml_strchr(name, b':').is_null() {
            xml_ns_err(
                ctxt,
                XmlParserErrors::XmlNsErrColon,
                c"colons are forbidden from notation names '%s'\n".as_ptr() as _,
                name,
                null(),
                null(),
            );
        }
        if SKIP_BLANKS!(ctxt) == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                c"Space required after the NOTATION name'\n".as_ptr() as _,
            );
            return;
        }

        /*
         * Parse the IDs.
         */
        systemid = xml_parse_external_id(ctxt, addr_of_mut!(pubid), 0);
        SKIP_BLANKS!(ctxt);

        if RAW!(ctxt) == b'>' {
            if inputid != (*(*ctxt).input).id {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    c"Notation declaration doesn't start and stop in the same entity\n".as_ptr()
                        as _,
                );
            }
            NEXT!(ctxt);
            if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                if let Some(not) = (*(*ctxt).sax).notation_decl {
                    not((*ctxt).user_data, name, pubid, systemid);
                }
            }
        } else {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotFinished, null());
        }
        if !systemid.is_null() {
            xml_free(systemid as _);
        }
        if !pubid.is_null() {
            xml_free(pubid as _);
        }
    }
}

/**
 * xmlParseEntityDecl:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse an entity declaration. Always consumes '<!'.
 *
 * [70] EntityDecl ::= GEDecl | PEDecl
 *
 * [71] GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
 *
 * [72] PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
 *
 * [73] EntityDef ::= EntityValue | (ExternalID NDataDecl?)
 *
 * [74] PEDef ::= EntityValue | ExternalID
 *
 * [76] NDataDecl ::= S 'NDATA' S Name
 *
 * [ VC: Notation Declared ]
 * The Name must match the declared name of a notation.
 */
pub(crate) unsafe extern "C" fn xml_parse_entity_decl(ctxt: XmlParserCtxtPtr) {
    let name: *const XmlChar;
    let mut value: *mut XmlChar = null_mut();
    let mut uri: *mut XmlChar = null_mut();
    let mut literal: *mut XmlChar = null_mut();
    let ndata: *const XmlChar;
    let mut is_parameter: c_int = 0;
    let mut orig: *mut XmlChar = null_mut();

    if CUR!(ctxt) != b'<' || NXT!(ctxt, 1) != b'!' {
        return;
    }
    SKIP!(ctxt, 2);

    /* GROW; done in the caller */
    if CMP6!(CUR_PTR!(ctxt), b'E', b'N', b'T', b'I', b'T', b'Y') {
        let inputid: c_int = (*(*ctxt).input).id;
        SKIP!(ctxt, 6);
        if SKIP_BLANKS!(ctxt) == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                c"Space required after '<!ENTITY'\n".as_ptr() as _,
            );
        }

        if RAW!(ctxt) == b'%' {
            NEXT!(ctxt);
            if SKIP_BLANKS!(ctxt) == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    c"Space required after '%%'\n".as_ptr() as _,
                );
            }
            is_parameter = 1;
        }

        name = xml_parse_name(ctxt);
        if name.is_null() {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                c"xmlParseEntityDecl: no name\n".as_ptr() as _,
            );
            return;
        }
        if !xml_strchr(name, b':').is_null() {
            xml_ns_err(
                ctxt,
                XmlParserErrors::XmlNsErrColon,
                c"colons are forbidden from entities names '%s'\n".as_ptr() as _,
                name,
                null(),
                null(),
            );
        }
        if SKIP_BLANKS!(ctxt) == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                c"Space required after the entity name\n".as_ptr() as _,
            );
        }

        (*ctxt).instate = XmlParserInputState::XmlParserEntityDecl;
        /*
         * handle the various case of definitions...
         */
        if is_parameter != 0 {
            if RAW!(ctxt) == b'"' || RAW!(ctxt) == b'\'' {
                value = xml_parse_entity_value(ctxt, addr_of_mut!(orig));
                if !value.is_null() && !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                    if let Some(ent) = (*(*ctxt).sax).entity_decl {
                        ent(
                            (*ctxt).user_data,
                            name,
                            XmlEntityType::XmlInternalParameterEntity as i32,
                            null(),
                            null(),
                            value,
                        );
                    }
                }
            } else {
                uri = xml_parse_external_id(ctxt, addr_of_mut!(literal), 1);
                if uri.is_null() && literal.is_null() {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrValueRequired, null());
                }
                if !uri.is_null() {
                    let parsed_uri = xml_parse_uri(uri as *const c_char);
                    if parsed_uri.is_null() {
                        xml_err_msg_str(
                            ctxt,
                            XmlParserErrors::XmlErrInvalidUri,
                            c"Invalid URI: %s\n".as_ptr() as _,
                            uri as _,
                        );
                    /*
                     * This really ought to be a well formedness error
                     * but the XML Core WG decided otherwise c.f. issue
                     * E26 of the XML erratas.
                     */
                    } else {
                        if !(*parsed_uri).fragment.is_null() {
                            /*
                             * Okay this is foolish to block those but not
                             * invalid URIs.
                             */
                            xml_fatal_err(ctxt, XmlParserErrors::XmlErrUriFragment, null());
                        } else if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                            if let Some(ent) = (*(*ctxt).sax).entity_decl {
                                ent(
                                    (*ctxt).user_data,
                                    name,
                                    XmlEntityType::XmlExternalParameterEntity as i32,
                                    literal,
                                    uri as _,
                                    null_mut(),
                                );
                            }
                        }
                        xml_free_uri(parsed_uri);
                    }
                }
            }
        } else if RAW!(ctxt) == b'"' || RAW!(ctxt) == b'\'' {
            value = xml_parse_entity_value(ctxt, addr_of_mut!(orig));
            if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                if let Some(ent) = (*(*ctxt).sax).entity_decl {
                    ent(
                        (*ctxt).user_data,
                        name,
                        XmlEntityType::XmlInternalGeneralEntity as i32,
                        null(),
                        null(),
                        value,
                    );
                }
            }
            /*
             * For expat compatibility in SAX mode.
             */
            if (*ctxt).my_doc.is_null()
                || xml_str_equal((*(*ctxt).my_doc).version, SAX_COMPAT_MODE.as_ptr() as _)
            {
                if (*ctxt).my_doc.is_null() {
                    (*ctxt).my_doc = xml_new_doc(SAX_COMPAT_MODE.as_ptr() as _);
                    if (*ctxt).my_doc.is_null() {
                        xml_err_memory(ctxt, c"New Doc failed".as_ptr() as _);
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
                        xml_new_dtd((*ctxt).my_doc, c"fake".as_ptr() as _, null(), null());
                }

                xml_sax2_entity_decl(
                    ctxt as _,
                    name,
                    XmlEntityType::XmlInternalGeneralEntity as i32,
                    null(),
                    null(),
                    value,
                );
            }
        } else {
            uri = xml_parse_external_id(ctxt, addr_of_mut!(literal), 1);
            if uri.is_null() && literal.is_null() {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrValueRequired, null());
            }
            if !uri.is_null() {
                let parsed_uri = xml_parse_uri(uri as *const c_char);
                if parsed_uri.is_null() {
                    xml_err_msg_str(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidUri,
                        c"Invalid URI: %s\n".as_ptr() as _,
                        uri as _,
                    );
                /*
                 * This really ought to be a well formedness error
                 * but the XML Core WG decided otherwise c.f. issue
                 * E26 of the XML erratas.
                 */
                } else {
                    if !(*parsed_uri).fragment.is_null() {
                        /*
                         * Okay this is foolish to block those but not
                         * invalid URIs.
                         */
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrUriFragment, null());
                    }
                    xml_free_uri(parsed_uri);
                }
            }
            if RAW!(ctxt) != b'>' && SKIP_BLANKS!(ctxt) == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    c"Space required before 'NDATA'\n".as_ptr() as _,
                );
            }
            if CMP5!(CUR_PTR!(ctxt), b'N', b'D', b'A', b'T', b'A') {
                SKIP!(ctxt, 5);
                if SKIP_BLANKS!(ctxt) == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        c"Space required after 'NDATA'\n".as_ptr() as _,
                    );
                }
                ndata = xml_parse_name(ctxt);
                if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                    if let Some(unparsed_ent) = (*(*ctxt).sax).unparsed_entity_decl {
                        unparsed_ent((*ctxt).user_data, name, literal, uri, ndata);
                    }
                }
            } else {
                if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
                    if let Some(ent) = (*(*ctxt).sax).entity_decl {
                        ent(
                            (*ctxt).user_data,
                            name,
                            XmlEntityType::XmlExternalGeneralParsedEntity as i32,
                            literal,
                            uri,
                            null_mut(),
                        );
                    }
                }
                /*
                 * For expat compatibility in SAX mode.
                 * assuming the entity replacement was asked for
                 */
                if (*ctxt).replace_entities != 0
                    && ((*ctxt).my_doc.is_null()
                        || xml_str_equal((*(*ctxt).my_doc).version, SAX_COMPAT_MODE.as_ptr() as _))
                {
                    if (*ctxt).my_doc.is_null() {
                        (*ctxt).my_doc = xml_new_doc(SAX_COMPAT_MODE.as_ptr() as _);
                        if (*ctxt).my_doc.is_null() {
                            xml_err_memory(ctxt, c"New Doc failed".as_ptr() as _);
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
                            xml_new_dtd((*ctxt).my_doc, c"fake".as_ptr() as _, null(), null());
                    }
                    xml_sax2_entity_decl(
                        ctxt as _,
                        name,
                        XmlEntityType::XmlExternalGeneralParsedEntity as i32,
                        literal,
                        uri,
                        null_mut(),
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
        SKIP_BLANKS!(ctxt);
        if RAW!(ctxt) != b'>' {
            xml_fatal_err_msg_str(
                ctxt,
                XmlParserErrors::XmlErrEntityNotFinished,
                c"xmlParseEntityDecl: entity %s not terminated\n".as_ptr() as _,
                name,
            );
            xml_halt_parser(ctxt);
        } else {
            if inputid != (*(*ctxt).input).id {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    c"Entity declaration doesn't start and stop in the same entity\n".as_ptr() as _,
                );
            }
            NEXT!(ctxt);
        }
        if !orig.is_null() {
            /*
             * Ugly mechanism to save the raw entity value.
             */
            let mut cur: XmlEntityPtr = null_mut();

            if is_parameter != 0 {
                if !(*ctxt).sax.is_null() {
                    if let Some(ent) = (*(*ctxt).sax).get_parameter_entity {
                        cur = ent((*ctxt).user_data, name);
                    }
                }
            } else {
                if !(*ctxt).sax.is_null() {
                    if let Some(ent) = (*(*ctxt).sax).get_entity {
                        cur = ent((*ctxt).user_data, name);
                    }
                }
                if cur.is_null() && (*ctxt).user_data == ctxt as _ {
                    cur = xml_sax2_get_entity(ctxt as _, name);
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

/**
 * xmlParseAttributeListDecl:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse an attribute list declaration for an element. Always consumes '<!'.
 *
 * [52] AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
 *
 * [53] AttDef ::= S Name S AttType S DefaultDecl
 *
 */
pub(crate) unsafe extern "C" fn xml_parse_attribute_list_decl(ctxt: XmlParserCtxtPtr) {
    let elem_name: *const XmlChar;
    let mut attr_name: *const XmlChar;
    let mut tree: XmlEnumerationPtr;

    if CUR!(ctxt) != b'<' || NXT!(ctxt, 1) != b'!' {
        return;
    }
    SKIP!(ctxt, 2);

    if CMP7!(CUR_PTR!(ctxt), b'A', b'T', b'T', b'L', b'I', b'S', b'T') {
        let inputid: c_int = (*(*ctxt).input).id;

        SKIP!(ctxt, 7);
        if SKIP_BLANKS!(ctxt) == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                c"Space required after '<!ATTLIST'\n".as_ptr() as _,
            );
        }
        elem_name = xml_parse_name(ctxt);
        if elem_name.is_null() {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                c"ATTLIST: no name for Element\n".as_ptr() as _,
            );
            return;
        }
        SKIP_BLANKS!(ctxt);
        GROW!(ctxt);
        #[allow(clippy::while_immutable_condition)]
        while RAW!(ctxt) != b'>' && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            let mut default_value: *mut XmlChar = null_mut();

            GROW!(ctxt);
            tree = null_mut();
            attr_name = xml_parse_name(ctxt);
            if attr_name.is_null() {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    c"ATTLIST: no name for Attribute\n".as_ptr() as _,
                );
                break;
            }
            GROW!(ctxt);
            if SKIP_BLANKS!(ctxt) == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    c"Space required after the attribute name\n".as_ptr() as _,
                );
                break;
            }

            let typ: c_int = xml_parse_attribute_type(ctxt, addr_of_mut!(tree));
            if typ <= 0 {
                break;
            }

            GROW!(ctxt);
            if SKIP_BLANKS!(ctxt) == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    c"Space required after the attribute typ\n".as_ptr() as _,
                );
                if !tree.is_null() {
                    xml_free_enumeration(tree);
                }
                break;
            }

            let def: c_int = xml_parse_default_decl(ctxt, addr_of_mut!(default_value));
            if def <= 0 {
                if !default_value.is_null() {
                    xml_free(default_value as _);
                }
                if !tree.is_null() {
                    xml_free_enumeration(tree);
                }
                break;
            }
            if typ != XmlAttributeType::XmlAttributeCdata as i32 && !default_value.is_null() {
                xml_attr_normalize_space(default_value, default_value);
            }

            GROW!(ctxt);
            if RAW!(ctxt) != b'>' && SKIP_BLANKS!(ctxt) == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    c"Space required after the attribute default value\n".as_ptr() as _,
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
                        (*ctxt).user_data,
                        elem_name,
                        attr_name,
                        typ,
                        def,
                        default_value,
                        tree,
                    );
                }
            } else if !tree.is_null() {
                xml_free_enumeration(tree);
            }

            if (*ctxt).sax2 != 0
                && !default_value.is_null()
                && def != XmlAttributeDefault::XmlAttributeImplied as i32
                && def != XmlAttributeDefault::XmlAttributeRequired as i32
            {
                xml_add_def_attrs(ctxt, elem_name, attr_name, default_value);
            }
            if (*ctxt).sax2 != 0 {
                xml_add_special_attr(ctxt, elem_name, attr_name, typ);
            }
            if !default_value.is_null() {
                xml_free(default_value as _);
            }
            GROW!(ctxt);
        }
        if RAW!(ctxt) == b'>' {
            if inputid != (*(*ctxt).input).id {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    c"Attribute list declaration doesn't start and stop in the same entity\n"
                        .as_ptr() as _,
                );
            }
            NEXT!(ctxt);
        }
    }
}

/**
 * xmlParseElementChildrenContentDeclPriv:
 * @ctxt:  an XML parser context
 * @inputchk:  the input used for the current entity, needed for boundary checks
 * @depth: the level of recursion
 *
 * parse the declaration for a Mixed Element content
 * The leading '(' and spaces have been skipped in xmlParseElementContentDecl
 *
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
pub(crate) unsafe extern "C" fn xml_parse_element_children_content_decl_priv(
    ctxt: XmlParserCtxtPtr,
    inputchk: c_int,
    depth: c_int,
) -> XmlElementContentPtr {
    let mut ret: XmlElementContentPtr;
    let mut cur: XmlElementContentPtr;
    let mut last: XmlElementContentPtr = null_mut();
    let mut op: XmlElementContentPtr;
    let mut elem: *const XmlChar;
    let mut typ: XmlChar = 0;

    if (depth > 128 && (*ctxt).options & XmlParserOption::XmlParseHuge as i32 == 0) || depth > 2048
    {
        xml_fatal_err_msg_int(ctxt, XmlParserErrors::XmlErrElemcontentNotFinished,
c"xmlParseElementChildrenContentDecl : depth %d too deep, use xmlParserOption::XML_PARSE_HUGE\n".as_ptr() as _,
                          depth);
        return null_mut();
    }
    SKIP_BLANKS!(ctxt);
    GROW!(ctxt);
    if RAW!(ctxt) == b'(' {
        let inputid: c_int = (*(*ctxt).input).id;

        /* Recurse on first child */
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
        cur = xml_parse_element_children_content_decl_priv(ctxt, inputid, depth + 1);
        ret = cur;
        if cur.is_null() {
            return null_mut();
        }
        SKIP_BLANKS!(ctxt);
        GROW!(ctxt);
    } else {
        elem = xml_parse_name(ctxt);
        if elem.is_null() {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrElemcontentNotStarted, null());
            return null_mut();
        }
        cur = xml_new_doc_element_content(
            (*ctxt).my_doc,
            elem,
            XmlElementContentType::XmlElementContentElement,
        );
        ret = cur;
        if cur.is_null() {
            xml_err_memory(ctxt, null());
            return null_mut();
        }
        GROW!(ctxt);
        if RAW!(ctxt) == b'?' {
            (*cur).ocur = XmlElementContentOccur::XmlElementContentOpt;
            NEXT!(ctxt);
        } else if RAW!(ctxt) == b'*' {
            (*cur).ocur = XmlElementContentOccur::XmlElementContentMult;
            NEXT!(ctxt);
        } else if RAW!(ctxt) == b'+' {
            (*cur).ocur = XmlElementContentOccur::XmlElementContentPlus;
            NEXT!(ctxt);
        } else {
            (*cur).ocur = XmlElementContentOccur::XmlElementContentOnce;
        }
        GROW!(ctxt);
    }
    SKIP_BLANKS!(ctxt);
    #[allow(clippy::while_immutable_condition)]
    while (RAW!(ctxt) != b')') && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        /*
         * Each loop we parse one separator and one element.
         */
        if RAW!(ctxt) == b',' {
            if typ == 0 {
                typ = CUR!(ctxt);
            }
            /*
             * Detect "Name | Name , Name" error
             */
            else if typ != CUR!(ctxt) {
                xml_fatal_err_msg_int(
                    ctxt,
                    XmlParserErrors::XmlErrSeparatorRequired,
                    c"xmlParseElementChildrenContentDecl : '%c' expected\n".as_ptr() as _,
                    typ as _,
                );
                if !last.is_null() && last != ret {
                    xml_free_doc_element_content((*ctxt).my_doc, last);
                }
                if !ret.is_null() {
                    xml_free_doc_element_content((*ctxt).my_doc, ret);
                }
                return null_mut();
            }
            NEXT!(ctxt);

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
        } else if RAW!(ctxt) == b'|' {
            if typ == 0 {
                typ = CUR!(ctxt);
            }
            /*
             * Detect "Name , Name | Name" error
             */
            else if typ != CUR!(ctxt) {
                xml_fatal_err_msg_int(
                    ctxt,
                    XmlParserErrors::XmlErrSeparatorRequired,
                    c"xmlParseElementChildrenContentDecl : '%c' expected\n".as_ptr() as _,
                    typ as _,
                );
                if !last.is_null() && last != ret {
                    xml_free_doc_element_content((*ctxt).my_doc, last);
                }
                if !ret.is_null() {
                    xml_free_doc_element_content((*ctxt).my_doc, ret);
                }
                return null_mut();
            }
            NEXT!(ctxt);

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
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrElemcontentNotFinished, null());
            if !last.is_null() && last != ret {
                xml_free_doc_element_content((*ctxt).my_doc, last);
            }
            if !ret.is_null() {
                xml_free_doc_element_content((*ctxt).my_doc, ret);
            }
            return null_mut();
        }
        GROW!(ctxt);
        SKIP_BLANKS!(ctxt);
        GROW!(ctxt);
        if RAW!(ctxt) == b'(' {
            let inputid: c_int = (*(*ctxt).input).id;
            /* Recurse on second child */
            NEXT!(ctxt);
            SKIP_BLANKS!(ctxt);
            last = xml_parse_element_children_content_decl_priv(ctxt, inputid, depth + 1);
            if last.is_null() {
                if !ret.is_null() {
                    xml_free_doc_element_content((*ctxt).my_doc, ret);
                }
                return null_mut();
            }
            SKIP_BLANKS!(ctxt);
        } else {
            elem = xml_parse_name(ctxt);
            if elem.is_null() {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrElemcontentNotStarted, null());
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
            if RAW!(ctxt) == b'?' {
                (*last).ocur = XmlElementContentOccur::XmlElementContentOpt;
                NEXT!(ctxt);
            } else if RAW!(ctxt) == b'*' {
                (*last).ocur = XmlElementContentOccur::XmlElementContentMult;
                NEXT!(ctxt);
            } else if RAW!(ctxt) == b'+' {
                (*last).ocur = XmlElementContentOccur::XmlElementContentPlus;
                NEXT!(ctxt);
            } else {
                (*last).ocur = XmlElementContentOccur::XmlElementContentOnce;
            }
        }
        SKIP_BLANKS!(ctxt);
        GROW!(ctxt);
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
            c"Element content declaration doesn't start and stop in the same entity\n".as_ptr()
                as _,
        );
    }
    NEXT!(ctxt);
    if RAW!(ctxt) == b'?' {
        if !ret.is_null() {
            if matches!((*ret).ocur, XmlElementContentOccur::XmlElementContentPlus)
                || matches!((*ret).ocur, XmlElementContentOccur::XmlElementContentMult)
            {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentMult;
            } else {
                (*ret).ocur = XmlElementContentOccur::XmlElementContentOpt;
            }
        }
        NEXT!(ctxt);
    } else if RAW!(ctxt) == b'*' {
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
        NEXT!(ctxt);
    } else if RAW!(ctxt) == b'+' {
        if !ret.is_null() {
            let mut found: c_int = 0;

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
        NEXT!(ctxt);
    }
    ret
}

/**
 * xmlParseElementDecl:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse an element declaration. Always consumes '<!'.
 *
 * [45] elementdecl ::= '<!ELEMENT' S Name S contentspec S? '>'
 *
 * [ VC: Unique Element Type Declaration ]
 * No element type may be declared more than once
 *
 * Returns the type of the element, or -1 in case of error
 */
pub(crate) unsafe extern "C" fn xml_parse_element_decl(ctxt: XmlParserCtxtPtr) -> c_int {
    let name: *const XmlChar;
    let mut ret: c_int = -1;
    let mut content: XmlElementContentPtr = null_mut();

    if CUR!(ctxt) != b'<' || NXT!(ctxt, 1) != b'!' {
        return ret;
    }
    SKIP!(ctxt, 2);

    /* GROW; done in the caller */
    if CMP7!(CUR_PTR!(ctxt), b'E', b'L', b'E', b'M', b'E', b'N', b'T') {
        let inputid: c_int = (*(*ctxt).input).id;

        SKIP!(ctxt, 7);
        if SKIP_BLANKS!(ctxt) == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                c"Space required after 'ELEMENT'\n".as_ptr() as _,
            );
            return -1;
        }
        name = xml_parse_name(ctxt);
        if name.is_null() {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                c"xmlParseElementDecl: no name for Element\n".as_ptr() as _,
            );
            return -1;
        }
        if SKIP_BLANKS!(ctxt) == 0 {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                c"Space required after the element name\n".as_ptr() as _,
            );
        }
        if CMP5!(CUR_PTR!(ctxt), b'E', b'M', b'P', b'T', b'Y') {
            SKIP!(ctxt, 5);
            /*
             * Element must always be empty.
             */
            ret = XmlElementTypeVal::XmlElementTypeEmpty as i32;
        } else if RAW!(ctxt) == b'A' && NXT!(ctxt, 1) == b'N' && NXT!(ctxt, 2) == b'Y' {
            SKIP!(ctxt, 3);
            /*
             * Element is a generic container.
             */
            ret = XmlElementTypeVal::XmlElementTypeAny as i32;
        } else if RAW!(ctxt) == b'(' {
            ret = xml_parse_element_content_decl(ctxt, name, addr_of_mut!(content));
        } else {
            /*
             * [ WFC: PEs in Internal Subset ] error handling.
             */
            if RAW!(ctxt) == b'%' && (*ctxt).external == 0 && (*ctxt).input_nr == 1 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrPERefInIntSubset,
                    c"PEReference: forbidden within markup decl in internal subset\n".as_ptr() as _,
                );
            } else {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrElemcontentNotStarted,
                    c"xmlParseElementDecl: 'EMPTY', 'ANY' or '(' expected\n".as_ptr() as _,
                );
            }
            return -1;
        }

        SKIP_BLANKS!(ctxt);

        if RAW!(ctxt) != b'>' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrGtRequired, null());
            if !content.is_null() {
                xml_free_doc_element_content((*ctxt).my_doc, content);
            }
        } else {
            if inputid != (*(*ctxt).input).id {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrEntityBoundary,
                    c"Element declaration doesn't start and stop in the same entity\n".as_ptr()
                        as _,
                );
            }

            NEXT!(ctxt);
            if !(*ctxt).sax.is_null()
                && (*ctxt).disable_sax == 0
                && (*(*ctxt).sax).element_decl.is_some()
            {
                if !content.is_null() {
                    (*content).parent = null_mut();
                }
                ((*(*ctxt).sax).element_decl.unwrap())((*ctxt).user_data, name, ret, content);
                if !content.is_null() && (*content).parent.is_null() {
                    /*
                     * this is a trick: if xmlAddElementDecl is called,
                     * instead of copying the full tree it is plugged directly
                     * if called from the parser. Avoid duplicating the
                     * interfaces or change the API/ABI
                     */
                    xml_free_doc_element_content((*ctxt).my_doc, content);
                }
            } else if !content.is_null() {
                xml_free_doc_element_content((*ctxt).my_doc, content);
            }
        }
    }
    ret
}

/**
 * xmlParseMarkupDecl:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse markup declarations. Always consumes '<!' or '<?'.
 *
 * [29] markupdecl ::= elementdecl | AttlistDecl | EntityDecl |
 *                     NotationDecl | PI | Comment
 *
 * [ VC: Proper Declaration/PE Nesting ]
 * Parameter-entity replacement text must be properly nested with
 * markup declarations. That is to say, if either the first character
 * or the last character of a markup declaration (markupdecl above) is
 * contained in the replacement text for a parameter-entity reference,
 * both must be contained in the same replacement text.
 *
 * [ WFC: PEs in Internal Subset ]
 * In the internal DTD subset, parameter-entity references can occur
 * only where markup declarations can occur, not within markup declarations.
 * (This does not apply to references that occur in external parameter
 * entities or to the external subset.)
 */
pub(crate) unsafe extern "C" fn xml_parse_markup_decl(ctxt: XmlParserCtxtPtr) {
    GROW!(ctxt);
    if CUR!(ctxt) == b'<' {
        if NXT!(ctxt, 1) == b'!' {
            match NXT!(ctxt, 2) {
                b'E' => {
                    if NXT!(ctxt, 3) == b'L' {
                        xml_parse_element_decl(ctxt);
                    } else if NXT!(ctxt, 3) == b'N' {
                        xml_parse_entity_decl(ctxt);
                    } else {
                        SKIP!(ctxt, 2);
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
                    SKIP!(ctxt, 2);
                }
            }
        } else if NXT!(ctxt, 1) == b'?' {
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

/**
 * xmlParseCharRef:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse a numeric character reference. Always consumes '&'.
 *
 * [66] CharRef ::= '&#' [0-9]+ ';' |
 *                  '&#x' [0-9a-fA-F]+ ';'
 *
 * [ WFC: Legal Character ]
 * Characters referred to using character references must match the
 * production for Char.
 *
 * Returns the value parsed (as an c_int), 0 in case of error
 */
pub(crate) unsafe extern "C" fn xml_parse_char_ref(ctxt: XmlParserCtxtPtr) -> c_int {
    let mut val: c_int = 0;
    let mut count: c_int = 0;

    /*
     * Using RAW/CUR/NEXT is okay since we are working on ASCII range here
     */
    if RAW!(ctxt) == b'&' && NXT!(ctxt, 1) == b'#' && NXT!(ctxt, 2) == b'x' {
        SKIP!(ctxt, 3);
        GROW!(ctxt);
        #[allow(clippy::while_immutable_condition)]
        while RAW!(ctxt) != b';' {
            /* loop blocked by count */
            let res = {
                let f = count > 20;
                count += 1;
                f
            };
            if res {
                count = 0;
                GROW!(ctxt);
                if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    return 0;
                }
            }
            if (RAW!(ctxt) >= b'0') && (RAW!(ctxt) <= b'9') {
                val = val * 16 + (CUR!(ctxt) - b'0') as i32;
            } else if (RAW!(ctxt) >= b'a') && (RAW!(ctxt) <= b'f') && (count < 20) {
                val = val * 16 + (CUR!(ctxt) - b'a') as i32 + 10;
            } else if (RAW!(ctxt) >= b'A') && (RAW!(ctxt) <= b'F') && (count < 20) {
                val = val * 16 + (CUR!(ctxt) - b'A') as i32 + 10;
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidHexCharRef, null());
                val = 0;
                break;
            }
            if val > 0x110000 {
                val = 0x110000;
            }

            NEXT!(ctxt);
            count += 1;
        }
        if RAW!(ctxt) == b';' {
            /* on purpose to avoid reentrancy problems with NEXT and SKIP */
            (*(*ctxt).input).col += 1;
            (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(1);
        }
    } else if RAW!(ctxt) == b'&' && NXT!(ctxt, 1) == b'#' {
        SKIP!(ctxt, 2);
        GROW!(ctxt);
        #[allow(clippy::while_immutable_condition)]
        while RAW!(ctxt) != b';' {
            /* loop blocked by count */
            let res = {
                let f = count > 20;
                count += 1;
                f
            };
            if res {
                count = 0;
                GROW!(ctxt);
                if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                    return 0;
                }
            }
            if (RAW!(ctxt) >= b'0') && (RAW!(ctxt) <= b'9') {
                val = val * 10 + (CUR!(ctxt) - b'0') as i32;
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidDecCharRef, null());
                val = 0;
                break;
            }
            if val > 0x110000 {
                val = 0x110000;
            }

            NEXT!(ctxt);
            count += 1;
        }
        if RAW!(ctxt) == b';' {
            /* on purpose to avoid reentrancy problems with NEXT and SKIP */
            (*(*ctxt).input).col += 1;
            (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(1);
        }
    } else {
        if RAW!(ctxt) == b'&' {
            SKIP!(ctxt, 1);
        }
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrInvalidCharRef, null());
    }

    /*
     * [ WFC: Legal Character ]
     * Characters referred to using character references must match the
     * production for Char.
     */
    if val >= 0x110000 {
        xml_fatal_err_msg_int(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            c"xmlParseCharRef: character reference out of bounds\n".as_ptr() as _,
            val,
        );
    } else if IS_CHAR!(val) {
        return val;
    } else {
        xml_fatal_err_msg_int(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            c"xmlParseCharRef: invalid XmlChar value %d\n".as_ptr() as _,
            val,
        );
    }
    0
}

/**
 * xmlParseCDSect:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * Parse escaped pure raw content. Always consumes '<!['.
 *
 * [18] CDSect ::= CDStart CData CDEnd
 *
 * [19] CDStart ::= '<![CDATA['
 *
 * [20] Data ::= (Char* - (Char* ']]>' Char*))
 *
 * [21] CDEnd ::= ']]>'
 */
pub(crate) unsafe extern "C" fn xml_parse_cdsect(ctxt: XmlParserCtxtPtr) {
    let mut buf: *mut XmlChar = null_mut();
    let mut len: c_int = 0;
    let mut size: c_int = XML_PARSER_BUFFER_SIZE as i32;
    let mut r: c_int;
    let mut rl: c_int = 0;
    let mut s: c_int;
    let mut sl: c_int = 0;
    let mut cur: c_int;
    let mut l: c_int = 0;
    let max_length: c_int = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH as i32
    } else {
        XML_MAX_TEXT_LENGTH as i32
    };

    if CUR!(ctxt) != b'<' || NXT!(ctxt, 1) != b'!' || NXT!(ctxt, 2) != b'[' {
        return;
    }
    SKIP!(ctxt, 3);

    if !CMP6!(CUR_PTR!(ctxt), b'C', b'D', b'A', b'T', b'A', b'[') {
        return;
    }
    SKIP!(ctxt, 6);

    (*ctxt).instate = XmlParserInputState::XmlParserCDATASection;
    r = CUR_CHAR!(ctxt, rl);
    if !IS_CHAR!(r) {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrCdataNotFinished, null());
        // goto out;
        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            (*ctxt).instate = XmlParserInputState::XmlParserContent;
        }
        xml_free(buf as _);
        return;
    }
    NEXTL!(ctxt, rl);
    s = CUR_CHAR!(ctxt, sl);
    if !IS_CHAR!(s) {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrCdataNotFinished, null());
        // goto out;
        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            (*ctxt).instate = XmlParserInputState::XmlParserContent;
        }
        xml_free(buf as _);
        return;
    }
    NEXTL!(ctxt, sl);
    cur = CUR_CHAR!(ctxt, l);
    buf = xml_malloc_atomic(size as usize) as *mut XmlChar;
    if buf.is_null() {
        xml_err_memory(ctxt, null());
        // goto out;
        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            (*ctxt).instate = XmlParserInputState::XmlParserContent;
        }
        xml_free(buf as _);
        return;
    }
    while IS_CHAR!(cur) && (r != b']' as i32 || s != b']' as i32 || cur != b'>' as i32) {
        if len + 5 >= size {
            let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize * 2) as *mut XmlChar;
            if tmp.is_null() {
                xml_err_memory(ctxt, null());
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
                XmlParserErrors::XmlErrCdataNotFinished,
                c"CData section too big found\n".as_ptr() as _,
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
        NEXTL!(ctxt, l);
        cur = CUR_CHAR!(ctxt, l);
    }
    *buf.add(len as usize) = 0;
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        xml_free(buf as _);
        return;
    }
    if cur != b'>' as i32 {
        xml_fatal_err_msg_str(
            ctxt,
            XmlParserErrors::XmlErrCdataNotFinished,
            c"CData section not finished\n%.50s\n".as_ptr() as _,
            buf,
        );
        // goto out;
        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            (*ctxt).instate = XmlParserInputState::XmlParserContent;
        }
        xml_free(buf as _);
        return;
    }
    NEXTL!(ctxt, l);

    /*
     * OK the buffer is to be consumed as cdata.
     */
    if !(*ctxt).sax.is_null() && (*ctxt).disable_sax == 0 {
        if let Some(cdata) = (*(*ctxt).sax).cdata_block {
            cdata((*ctxt).user_data, buf, len);
        } else if let Some(characters) = (*(*ctxt).sax).characters {
            characters((*ctxt).user_data, buf, len);
        }
    }

    // out:
    if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        (*ctxt).instate = XmlParserInputState::XmlParserContent;
    }
    xml_free(buf as _);
}

/**
 * xmlParseElement:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an XML element
 *
 * [39] element ::= EmptyElemTag | STag content ETag
 *
 * [ WFC: Element Type Match ]
 * The Name in an element's end-tag must match the element type in the
 * start-tag.
 *
 */
pub(crate) unsafe extern "C" fn xml_parse_element(ctxt: XmlParserCtxtPtr) {
    if xml_parse_element_start(ctxt) != 0 {
        return;
    }

    xml_parse_content_internal(ctxt);
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }

    if CUR!(ctxt) == 0 {
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
        return;
    }

    xml_parse_element_end(ctxt);
}

/**
 * xmlParseVersionNum:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse the XML version value.
 *
 * [26] VersionNum ::= '1.' [0-9]+
 *
 * In practice allow [0-9].[0-9]+ at that level
 *
 * Returns the string giving the XML version number, or NULL
 */
pub(crate) unsafe extern "C" fn xml_parse_version_num(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    let mut buf: *mut XmlChar;
    let mut len: c_int = 0;
    let mut size: c_int = 10;
    let mut cur: XmlChar;

    buf = xml_malloc_atomic(size as usize) as *mut XmlChar;
    if buf.is_null() {
        xml_err_memory(ctxt, null());
        return null_mut();
    }
    cur = CUR!(ctxt);
    if !cur.is_ascii_digit() {
        xml_free(buf as _);
        return null_mut();
    }
    *buf.add(len as usize) = cur;
    len += 1;
    NEXT!(ctxt);
    cur = CUR!(ctxt);
    if cur != b'.' {
        xml_free(buf as _);
        return null_mut();
    }
    *buf.add(len as usize) = cur;
    len += 1;
    NEXT!(ctxt);
    cur = CUR!(ctxt);
    while cur.is_ascii_digit() {
        if len + 1 >= size {
            size *= 2;
            let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize) as *mut XmlChar;
            if tmp.is_null() {
                xml_free(buf as _);
                xml_err_memory(ctxt, null());
                return null_mut();
            }
            buf = tmp;
        }
        *buf.add(len as usize) = cur;
        len += 1;
        NEXT!(ctxt);
        cur = CUR!(ctxt);
    }
    *buf.add(len as usize) = 0;
    buf
}

/**
 * xmlParseEncName:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse the XML encoding name
 *
 * [81] EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
 *
 * Returns the encoding name value or NULL
 */
pub(crate) unsafe extern "C" fn xml_parse_enc_name(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    let mut buf: *mut XmlChar = null_mut();
    let mut len: c_int = 0;
    let mut size: c_int = 10;
    let max_length: c_int = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };
    let mut cur: XmlChar;

    cur = CUR!(ctxt);
    if cur.is_ascii_lowercase() || cur.is_ascii_uppercase() {
        buf = xml_malloc_atomic(size as usize) as *mut XmlChar;
        if buf.is_null() {
            xml_err_memory(ctxt, null());
            return null_mut();
        }

        *buf.add(len as usize) = cur;
        len += 1;
        NEXT!(ctxt);
        cur = CUR!(ctxt);
        while cur.is_ascii_lowercase()
            || cur.is_ascii_uppercase()
            || cur.is_ascii_digit()
            || (cur == b'.')
            || (cur == b'_')
            || (cur == b'-')
        {
            if len + 1 >= size {
                size *= 2;
                let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize) as *mut XmlChar;
                if tmp.is_null() {
                    xml_err_memory(ctxt, null());
                    xml_free(buf as _);
                    return null_mut();
                }
                buf = tmp;
            }
            *buf.add(len as usize) = cur;
            len += 1;
            if len > max_length {
                xml_fatal_err(
                    ctxt,
                    XmlParserErrors::XmlErrNameTooLong,
                    c"EncName".as_ptr() as _,
                );
                xml_free(buf as _);
                return null_mut();
            }
            NEXT!(ctxt);
            cur = CUR!(ctxt);
        }
        *buf.add(len as usize) = 0;
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrEncodingName, null());
    }
    buf
}

/**
 * xmlParseXMLDecl:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an XML declaration header
 *
 * [23] XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
 */
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
    SKIP!(ctxt, 5);

    if !IS_BLANK_CH!(RAW!(ctxt)) {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            c"Blank needed after '<?xml'\n".as_ptr() as _,
        );
    }
    SKIP_BLANKS!(ctxt);

    /*
     * We must have the VersionInfo here.
     */
    let version: *mut XmlChar = xml_parse_version_info(ctxt);
    if version.is_null() {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrVersionMissing, null());
    } else {
        if !xml_str_equal(version, XML_DEFAULT_VERSION.as_ptr() as *const XmlChar) {
            /*
             * Changed here for XML-1.0 5th edition
             */
            if (*ctxt).options & XmlParserOption::XmlParseOld10 as i32 != 0 {
                xml_fatal_err_msg_str(
                    ctxt,
                    XmlParserErrors::XmlErrUnknownVersion,
                    c"Unsupported version '%s'\n".as_ptr() as _,
                    version,
                );
            } else if *version.add(0) == b'1' && *version.add(1) == b'.' {
                xml_warning_msg(
                    ctxt,
                    XmlParserErrors::XmlWarUnknownVersion,
                    c"Unsupported version '%s'\n".as_ptr() as _,
                    version,
                    null(),
                );
            } else {
                xml_fatal_err_msg_str(
                    ctxt,
                    XmlParserErrors::XmlErrUnknownVersion,
                    c"Unsupported version '%s'\n".as_ptr() as _,
                    version,
                );
            }
        }
        if !(*ctxt).version.is_null() {
            xml_free((*ctxt).version as _);
        }
        (*ctxt).version = version;
    }

    /*
     * We may have the encoding declaration
     */
    if !IS_BLANK_CH!(RAW!(ctxt)) {
        if RAW!(ctxt) == b'?' && NXT!(ctxt, 1) == b'>' {
            SKIP!(ctxt, 2);
            return;
        }
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            c"Blank needed here\n".as_ptr() as _,
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
    if !(*(*ctxt).input).encoding.is_null() && !IS_BLANK_CH!(RAW!(ctxt)) {
        if RAW!(ctxt) == b'?' && NXT!(ctxt, 1) == b'>' {
            SKIP!(ctxt, 2);
            return;
        }
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            c"Blank needed here\n".as_ptr() as _,
        );
    }

    /*
     * We can grow the input buffer freely at that point
     */
    GROW!(ctxt);

    SKIP_BLANKS!(ctxt);
    (*(*ctxt).input).standalone = xml_parse_sddecl(ctxt);

    SKIP_BLANKS!(ctxt);
    if RAW!(ctxt) == b'?' && NXT!(ctxt, 1) == b'>' {
        SKIP!(ctxt, 2);
    } else if RAW!(ctxt) == b'>' {
        /* Deprecated old WD ... */
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotFinished, null());
        NEXT!(ctxt);
    } else {
        let mut c: c_int;

        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotFinished, null());
        while {
            c = CUR!(ctxt) as _;
            c != 0
        } {
            NEXT!(ctxt);
            if c == b'>' as i32 {
                break;
            }
        }
    }
}

/**
 * xmlParseTextDecl:
 * @ctxt:  an XML parser context
 *
 * DEPRECATED: Internal function, don't use.
 *
 * parse an XML declaration header for external entities
 *
 * [77] TextDecl ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
 */
pub(crate) unsafe extern "C" fn xml_parse_text_decl(ctxt: XmlParserCtxtPtr) {
    let mut version: *mut XmlChar;

    /*
     * We know that '<?xml' is here.
     */
    if CMP5!(CUR_PTR!(ctxt), b'<', b'?', b'x', b'm', b'l') && IS_BLANK_CH!(NXT!(ctxt, 5)) {
        SKIP!(ctxt, 5);
    } else {
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotStarted, null());
        return;
    }

    /* Avoid expansion of parameter entities when skipping blanks. */
    let oldstate: c_int = (*ctxt).instate as _;
    (*ctxt).instate = XmlParserInputState::XmlParserStart;

    if SKIP_BLANKS!(ctxt) == 0 {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            c"Space needed after '<?xml'\n".as_ptr() as _,
        );
    }

    /*
     * We may have the VersionInfo here.
     */
    version = xml_parse_version_info(ctxt);
    if version.is_null() {
        version = xml_char_strdup(XML_DEFAULT_VERSION.as_ptr() as _);
    } else if SKIP_BLANKS!(ctxt) == 0 {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrSpaceRequired,
            c"Space needed here\n".as_ptr() as _,
        );
    }
    (*(*ctxt).input).version = version;

    /*
     * We must have the encoding declaration
     */
    let encoding: *const XmlChar = xml_parse_encoding_decl(ctxt);
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
    if encoding.is_null() && (*ctxt).err_no == XmlParserErrors::XmlErrOK as i32 {
        xml_fatal_err_msg(
            ctxt,
            XmlParserErrors::XmlErrMissingEncoding,
            c"Missing encoding in text declaration\n".as_ptr() as _,
        );
    }

    SKIP_BLANKS!(ctxt);
    if RAW!(ctxt) == b'?' && NXT!(ctxt, 1) == b'>' {
        SKIP!(ctxt, 2);
    } else if RAW!(ctxt) == b'>' {
        /* Deprecated old WD ... */
        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotFinished, null());
        NEXT!(ctxt);
    } else {
        let mut c: c_int;

        xml_fatal_err(ctxt, XmlParserErrors::XmlErrXMLDeclNotFinished, null());
        while {
            c = CUR!(ctxt) as _;
            c != 0
        } {
            NEXT!(ctxt);
            if c == b'>' as i32 {
                break;
            }
        }
    }

    (*ctxt).instate = oldstate.try_into().unwrap();
}

/**
 * xmlSkipBlankChars:
 * @ctxt:  the XML parser context
 *
 * DEPRECATED: Internal function, do not use.
 *
 * skip all blanks character found at that point in the input streams.
 * It pops up finished entities in the process if allowable at that point.
 *
 * Returns the number of space chars skipped
 */
pub(crate) unsafe extern "C" fn xml_skip_blank_chars(ctxt: XmlParserCtxtPtr) -> c_int {
    let mut res: c_int = 0;

    /*
     * It's Okay to use CUR/NEXT here since all the blanks are on
     * the ASCII range.
     */
    if ((*ctxt).input_nr == 1 && !matches!((*ctxt).instate, XmlParserInputState::XmlParserDTD))
        || matches!((*ctxt).instate, XmlParserInputState::XmlParserStart)
    {
        let mut cur: *const XmlChar;
        /*
         * if we are in the document content, go really fast
         */
        cur = (*(*ctxt).input).cur;
        while IS_BLANK_CH!(*cur) {
            if *cur == b'\n' {
                (*(*ctxt).input).line += 1;
                (*(*ctxt).input).col = 1;
            } else {
                (*(*ctxt).input).col += 1;
            }
            cur = cur.add(1);
            res = res.saturating_add(1);
            if *cur == 0 {
                (*(*ctxt).input).cur = cur;
                xml_parser_grow(ctxt);
                cur = (*(*ctxt).input).cur;
            }
        }
        (*(*ctxt).input).cur = cur;
    } else {
        let expand_pe: c_int = ((*ctxt).external != 0 || (*ctxt).input_nr != 1) as i32;

        while !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            if IS_BLANK_CH!(CUR!(ctxt)) {
                /* CHECKED tstblanks.xml */
                NEXT!(ctxt);
            } else if CUR!(ctxt) == b'%' {
                /*
                 * Need to handle support of entities branching here
                 */
                if expand_pe == 0 || IS_BLANK_CH!(NXT!(ctxt, 1)) || NXT!(ctxt, 1) == 0 {
                    break;
                }
                xml_parse_pe_reference(ctxt);
            } else if CUR!(ctxt) == 0 {
                let mut consumed: c_ulong;

                if (*ctxt).input_nr <= 1 {
                    break;
                }

                consumed = (*(*ctxt).input).consumed;
                xml_saturated_add_size_t(
                    addr_of_mut!(consumed),
                    (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _,
                );

                /*
                 * Add to sizeentities when parsing an external entity
                 * for the first time.
                 */
                let ent: XmlEntityPtr = (*(*ctxt).input).entity;
                if matches!(
                    (*ent).etype,
                    Some(XmlEntityType::XmlExternalParameterEntity)
                ) && (*ent).flags & XML_ENT_PARSED as i32 == 0
                {
                    (*ent).flags |= XML_ENT_PARSED as i32;

                    xml_saturated_add(addr_of_mut!((*ctxt).sizeentities), consumed);
                }

                xml_parser_entity_check(ctxt, consumed);

                xml_pop_input(ctxt);
            } else {
                break;
            }

            /*
             * Also increase the counter when entering or exiting a PERef.
             * The spec says: "When a parameter-entity reference is recognized
             * in the DTD and included, its replacement text MUST be enlarged
             * by the attachment of one leading and one following space (#x20)
             * character."
             */
            res = res.saturating_add(1);
        }
    }
    res
}

#[cfg(test)]
mod tests {
    use crate::{
        libxml::{xmlerror::xml_reset_last_error, xmlmemory::xml_mem_blocks},
        test_util::*,
    };

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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
    fn test_xml_create_push_parser_ctxt() {
        #[cfg(feature = "push")]
        unsafe {
            let mut leaks = 0;

            for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                for n_user_data in 0..GEN_NB_USERDATA {
                    for n_chunk in 0..GEN_NB_CONST_CHAR_PTR {
                        for n_size in 0..GEN_NB_INT {
                            for n_filename in 0..GEN_NB_FILEOUTPUT {
                                let mem_base = xml_mem_blocks();
                                let sax = gen_xml_saxhandler_ptr(n_sax, 0);
                                let user_data = gen_userdata(n_user_data, 1);
                                let chunk = gen_const_char_ptr(n_chunk, 2);
                                let mut size = gen_int(n_size, 3);
                                let filename = gen_fileoutput(n_filename, 4);
                                if !chunk.is_null() && size > xml_strlen(chunk as _) {
                                    size = 0;
                                }

                                let ret_val = xml_create_push_parser_ctxt(
                                    sax, user_data, chunk, size, filename,
                                );
                                desret_xml_parser_ctxt_ptr(ret_val);
                                des_xml_saxhandler_ptr(n_sax, sax, 0);
                                des_userdata(n_user_data, user_data, 1);
                                des_const_char_ptr(n_chunk, chunk, 2);
                                des_int(n_size, size, 3);
                                des_fileoutput(n_filename, filename, 4);
                                xml_reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlCreatePushParserCtxt",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_sax);
                                    eprint!(" {}", n_user_data);
                                    eprint!(" {}", n_chunk);
                                    eprint!(" {}", n_size);
                                    eprintln!(" {}", n_filename);
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlCreatePushParserCtxt()"
            );
        }
    }

    #[test]
    fn test_xml_ctxt_read_doc() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_cur in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_url in 0..GEN_NB_FILEPATH {
                        for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                            for n_options in 0..GEN_NB_PARSEROPTIONS {
                                let mem_base = xml_mem_blocks();
                                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                                let cur = gen_const_xml_char_ptr(n_cur, 1);
                                let url = gen_filepath(n_url, 2);
                                let encoding = gen_const_char_ptr(n_encoding, 3);
                                let options = gen_parseroptions(n_options, 4);

                                let ret_val = xml_ctxt_read_doc(ctxt, cur, url, encoding, options);
                                desret_xml_doc_ptr(ret_val);
                                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                                des_const_xml_char_ptr(n_cur, cur, 1);
                                des_filepath(n_url, url, 2);
                                des_const_char_ptr(n_encoding, encoding, 3);
                                des_parseroptions(n_options, options, 4);
                                xml_reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlCtxtReadDoc",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_ctxt);
                                    eprint!(" {}", n_cur);
                                    eprint!(" {}", n_url);
                                    eprint!(" {}", n_encoding);
                                    eprintln!(" {}", n_options);
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlCtxtReadDoc()");
        }
    }

    #[test]
    fn test_xml_ctxt_read_file() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_filename in 0..GEN_NB_FILEPATH {
                    for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                        for n_options in 0..GEN_NB_PARSEROPTIONS {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                            let filename = gen_filepath(n_filename, 1);
                            let encoding = gen_const_char_ptr(n_encoding, 2);
                            let options = gen_parseroptions(n_options, 3);

                            let ret_val = xml_ctxt_read_file(ctxt, filename, encoding, options);
                            desret_xml_doc_ptr(ret_val);
                            des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_filepath(n_filename, filename, 1);
                            des_const_char_ptr(n_encoding, encoding, 2);
                            des_parseroptions(n_options, options, 3);
                            xml_reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlCtxtReadFile",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_filename);
                                eprint!(" {}", n_encoding);
                                eprintln!(" {}", n_options);
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlCtxtReadFile()");
        }
    }

    #[test]
    fn test_xml_ctxt_read_memory() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_buffer in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_size in 0..GEN_NB_INT {
                        for n_url in 0..GEN_NB_FILEPATH {
                            for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                                for n_options in 0..GEN_NB_PARSEROPTIONS {
                                    let mem_base = xml_mem_blocks();
                                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                                    let buffer = gen_const_char_ptr(n_buffer, 1);
                                    let mut size = gen_int(n_size, 2);
                                    let url = gen_filepath(n_url, 3);
                                    let encoding = gen_const_char_ptr(n_encoding, 4);
                                    let options = gen_parseroptions(n_options, 5);
                                    if !buffer.is_null() && size > xml_strlen(buffer as _) {
                                        size = 0;
                                    }

                                    let ret_val = xml_ctxt_read_memory(
                                        ctxt, buffer, size, url, encoding, options,
                                    );
                                    desret_xml_doc_ptr(ret_val);
                                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                                    des_const_char_ptr(n_buffer, buffer, 1);
                                    des_int(n_size, size, 2);
                                    des_filepath(n_url, url, 3);
                                    des_const_char_ptr(n_encoding, encoding, 4);
                                    des_parseroptions(n_options, options, 5);
                                    xml_reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlCtxtReadMemory",
                                            xml_mem_blocks() - mem_base
                                        );
                                        eprint!(" {}", n_ctxt);
                                        eprint!(" {}", n_buffer);
                                        eprint!(" {}", n_size);
                                        eprint!(" {}", n_url);
                                        eprint!(" {}", n_encoding);
                                        eprintln!(" {}", n_options);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlCtxtReadMemory()");
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
                xml_reset_last_error();
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
                                xml_reset_last_error();
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
    fn test_xml_ctxt_use_options() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_options in 0..GEN_NB_PARSEROPTIONS {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let options = gen_parseroptions(n_options, 1);

                    let ret_val = xml_ctxt_use_options(ctxt, options);
                    desret_int(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_parseroptions(n_options, options, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCtxtUseOptions",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_options);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlCtxtUseOptions()");
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
                xml_reset_last_error();
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
    fn test_xml_ioparse_dtd() {
        #[cfg(feature = "valid")]
        unsafe {
            #[cfg(feature = "valid")]
            {
                for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                    for n_input in 0..GEN_NB_XML_PARSER_INPUT_BUFFER_PTR {
                        for n_enc in 0..GEN_NB_XML_CHAR_ENCODING {
                            let sax = gen_xml_saxhandler_ptr(n_sax, 0);
                            let mut input = gen_xml_parser_input_buffer_ptr(n_input, 1);
                            let enc = gen_xml_char_encoding(n_enc, 2);

                            let ret_val = xml_io_parse_dtd(sax, input, enc);
                            input = null_mut();
                            desret_xml_dtd_ptr(ret_val);
                            des_xml_saxhandler_ptr(n_sax, sax, 0);
                            des_xml_parser_input_buffer_ptr(n_input, input, 1);
                            des_xml_char_encoding(n_enc, enc, 2);
                            xml_reset_last_error();
                        }
                    }
                }
            }
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
                xml_reset_last_error();
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
            xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
    fn test_xml_load_external_entity() {
        unsafe {
            let mut leaks = 0;

            for n_url in 0..GEN_NB_FILEPATH {
                for n_id in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                        let mem_base = xml_mem_blocks();
                        let url = gen_filepath(n_url, 0);
                        let id = gen_const_char_ptr(n_id, 1);
                        let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 2);

                        let ret_val = xml_load_external_entity(url, id, ctxt);
                        desret_xml_parser_input_ptr(ret_val);
                        des_filepath(n_url, url, 0);
                        des_const_char_ptr(n_id, id, 1);
                        des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 2);
                        xml_reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlLoadExternalEntity",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_url);
                            eprint!(" {}", n_id);
                            eprintln!(" {}", n_ctxt);
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlLoadExternalEntity()"
            );
        }
    }

    #[test]
    fn test_xml_new_ioinput_stream() {
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_input in 0..GEN_NB_XML_PARSER_INPUT_BUFFER_PTR {
                    for n_enc in 0..GEN_NB_XML_CHAR_ENCODING {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                        let mut input = gen_xml_parser_input_buffer_ptr(n_input, 1);
                        let enc = gen_xml_char_encoding(n_enc, 2);

                        let ret_val = xml_new_io_input_stream(ctxt, input, enc);
                        if !ret_val.is_null() {
                            input = null_mut();
                        }
                        desret_xml_parser_input_ptr(ret_val);
                        des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_parser_input_buffer_ptr(n_input, input, 1);
                        des_xml_char_encoding(n_enc, enc, 2);
                        xml_reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNewIOInputStream",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_input);
                            eprintln!(" {}", n_enc);
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlNewIOInputStream()"
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
            xml_reset_last_error();
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
    fn test_xml_new_saxparser_ctxt() {
        unsafe {
            let mut leaks = 0;

            for n_sax in 0..GEN_NB_CONST_XML_SAXHANDLER_PTR {
                for n_user_data in 0..GEN_NB_USERDATA {
                    let mem_base = xml_mem_blocks();
                    let sax = gen_const_xml_saxhandler_ptr(n_sax, 0);
                    let user_data = gen_userdata(n_user_data, 1);

                    let ret_val = xml_new_sax_parser_ctxt(sax, user_data);
                    desret_xml_parser_ctxt_ptr(ret_val);
                    des_const_xml_saxhandler_ptr(n_sax, sax, 0);
                    des_userdata(n_user_data, user_data, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewSAXParserCtxt",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_sax);
                        eprintln!(" {}", n_user_data);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlNewSAXParserCtxt()"
            );
        }
    }

    #[test]
    fn test_xml_parse_balanced_chunk_memory() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;

            // buggy case
            let mem_base = xml_mem_blocks();
            let doc = gen_xml_doc_ptr(1, 0);
            let sax = gen_xml_saxhandler_ptr(1, 1);
            let mut user_data = gen_userdata(0, 2);
            let depth = gen_int(0, 3);
            let string = gen_const_xml_char_ptr(1, 4);
            let lst = gen_xml_node_ptr_ptr(0, 5);

            #[cfg(feature = "sax1")]
            if sax == xml_default_sax_handler() as XmlSAXHandlerPtr {
                user_data = null_mut();
            }

            let ret_val = xml_parse_balanced_chunk_memory(doc, sax, user_data, depth, string, lst);
            desret_int(ret_val);
            des_xml_doc_ptr(1, doc, 0);
            des_xml_saxhandler_ptr(1, sax, 1);
            des_userdata(0, user_data, 2);
            des_int(0, depth, 3);
            des_const_xml_char_ptr(1, string, 4);
            des_xml_node_ptr_ptr(0, lst, 5);
            xml_reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprint!(
                    "Leak of {} blocks found in xmlParseBalancedChunkMemory",
                    xml_mem_blocks() - mem_base
                );
                eprint!(" {}", 1);
                eprint!(" {}", 1);
                eprint!(" {}", 0);
                eprint!(" {}", 0);
                eprint!(" {}", 1);
                eprintln!(" {}", 0);
            }

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                    for n_user_data in 0..GEN_NB_USERDATA {
                        for n_depth in 0..GEN_NB_INT {
                            for n_string in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                for n_lst in 0..GEN_NB_XML_NODE_PTR_PTR {
                                    let mem_base = xml_mem_blocks();
                                    let doc = gen_xml_doc_ptr(n_doc, 0);
                                    let sax = gen_xml_saxhandler_ptr(n_sax, 1);
                                    let mut user_data = gen_userdata(n_user_data, 2);
                                    let depth = gen_int(n_depth, 3);
                                    let string = gen_const_xml_char_ptr(n_string, 4);
                                    let lst = gen_xml_node_ptr_ptr(n_lst, 5);

                                    #[cfg(feature = "sax1")]
                                    if sax == xml_default_sax_handler() as XmlSAXHandlerPtr {
                                        user_data = null_mut();
                                    }

                                    let ret_val = xml_parse_balanced_chunk_memory(
                                        doc, sax, user_data, depth, string, lst,
                                    );
                                    desret_int(ret_val);
                                    des_xml_doc_ptr(n_doc, doc, 0);
                                    des_xml_saxhandler_ptr(n_sax, sax, 1);
                                    des_userdata(n_user_data, user_data, 2);
                                    des_int(n_depth, depth, 3);
                                    des_const_xml_char_ptr(n_string, string, 4);
                                    des_xml_node_ptr_ptr(n_lst, lst, 5);
                                    xml_reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!("Leak of {} blocks found in xmlParseBalancedChunkMemory", xml_mem_blocks() - mem_base);
                                        eprint!(" {}", n_doc);
                                        eprint!(" {}", n_sax);
                                        eprint!(" {}", n_user_data);
                                        eprint!(" {}", n_depth);
                                        eprint!(" {}", n_string);
                                        eprintln!(" {}", n_lst);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParseBalancedChunkMemory()"
            );
        }
    }

    #[test]
    fn test_xml_parse_balanced_chunk_memory_recover() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;
            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                    for n_user_data in 0..GEN_NB_USERDATA {
                        for n_depth in 0..GEN_NB_INT {
                            for n_string in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                for n_lst in 0..GEN_NB_XML_NODE_PTR_PTR {
                                    for n_recover in 0..GEN_NB_INT {
                                        let mem_base = xml_mem_blocks();
                                        let doc = gen_xml_doc_ptr(n_doc, 0);
                                        let sax = gen_xml_saxhandler_ptr(n_sax, 1);
                                        let mut user_data = gen_userdata(n_user_data, 2);
                                        let depth = gen_int(n_depth, 3);
                                        let string = gen_const_xml_char_ptr(n_string, 4);
                                        let lst = gen_xml_node_ptr_ptr(n_lst, 5);
                                        let recover = gen_int(n_recover, 6);

                                        #[cfg(feature = "sax1")]
                                        if sax == xml_default_sax_handler() as XmlSAXHandlerPtr {
                                            user_data = null_mut();
                                        }

                                        let ret_val = xml_parse_balanced_chunk_memory_recover(
                                            doc, sax, user_data, depth, string, lst, recover,
                                        );
                                        desret_int(ret_val);
                                        des_xml_doc_ptr(n_doc, doc, 0);
                                        des_xml_saxhandler_ptr(n_sax, sax, 1);
                                        des_userdata(n_user_data, user_data, 2);
                                        des_int(n_depth, depth, 3);
                                        des_const_xml_char_ptr(n_string, string, 4);
                                        des_xml_node_ptr_ptr(n_lst, lst, 5);
                                        des_int(n_recover, recover, 6);
                                        xml_reset_last_error();
                                        if mem_base != xml_mem_blocks() {
                                            leaks += 1;
                                            eprint!("Leak of {} blocks found in xmlParseBalancedChunkMemoryRecover", xml_mem_blocks() - mem_base);
                                            eprint!(" {}", n_doc);
                                            eprint!(" {}", n_sax);
                                            eprint!(" {}", n_user_data);
                                            eprint!(" {}", n_depth);
                                            eprint!(" {}", n_string);
                                            eprint!(" {}", n_lst);
                                            eprintln!(" {}", n_recover);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParseBalancedChunkMemoryRecover()"
            );
        }
    }

    #[test]
    fn test_xml_parse_chunk() {
        #[cfg(feature = "push")]
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
                            xml_reset_last_error();
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
    fn test_xml_parse_ctxt_external_entity() {
        unsafe {
            let mut leaks = 0;
            for n_ctx in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_url in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_lst in 0..GEN_NB_XML_NODE_PTR_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctx = gen_xml_parser_ctxt_ptr(n_ctx, 0);
                            let url = gen_const_xml_char_ptr(n_url, 1);
                            let id = gen_const_xml_char_ptr(n_id, 2);
                            let lst = gen_xml_node_ptr_ptr(n_lst, 3);

                            let ret_val = xml_parse_ctxt_external_entity(ctx, url, id, lst);
                            desret_int(ret_val);
                            des_xml_parser_ctxt_ptr(n_ctx, ctx, 0);
                            des_const_xml_char_ptr(n_url, url, 1);
                            des_const_xml_char_ptr(n_id, id, 2);
                            des_xml_node_ptr_ptr(n_lst, lst, 3);
                            xml_reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlParseCtxtExternalEntity",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_ctx);
                                eprint!(" {}", n_url);
                                eprint!(" {}", n_id);
                                eprintln!(" {}", n_lst);
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParseCtxtExternalEntity()"
            );
        }
    }

    #[test]
    fn test_xml_parse_dtd() {
        #[cfg(feature = "valid")]
        unsafe {
            let mut leaks = 0;

            for n_external_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_system_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let external_id = gen_const_xml_char_ptr(n_external_id, 0);
                    let system_id = gen_const_xml_char_ptr(n_system_id, 1);

                    let ret_val = xml_parse_dtd(external_id as *const XmlChar, system_id);
                    desret_xml_dtd_ptr(ret_val);
                    des_const_xml_char_ptr(n_external_id, external_id, 0);
                    des_const_xml_char_ptr(n_system_id, system_id, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlParseDTD",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_external_id);
                        eprintln!(" {}", n_system_id);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlParseDTD()");
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
                xml_reset_last_error();
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
                xml_reset_last_error();
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
    fn test_xml_parse_entity() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;
            for n_filename in 0..GEN_NB_FILEPATH {
                let mem_base = xml_mem_blocks();
                let filename = gen_filepath(n_filename, 0);

                let ret_val = xml_parse_entity(filename);
                desret_xml_doc_ptr(ret_val);
                des_filepath(n_filename, filename, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlParseEntity",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_filename);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlParseEntity()");
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
                xml_reset_last_error();
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
    fn test_xml_parse_external_entity() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                    for n_user_data in 0..GEN_NB_USERDATA {
                        for n_depth in 0..GEN_NB_INT {
                            for n_url in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                for n_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                    for n_lst in 0..GEN_NB_XML_NODE_PTR_PTR {
                                        let mem_base = xml_mem_blocks();
                                        let doc = gen_xml_doc_ptr(n_doc, 0);
                                        let sax = gen_xml_saxhandler_ptr(n_sax, 1);
                                        let user_data = gen_userdata(n_user_data, 2);
                                        let depth = gen_int(n_depth, 3);
                                        let url = gen_const_xml_char_ptr(n_url, 4);
                                        let id = gen_const_xml_char_ptr(n_id, 5);
                                        let lst = gen_xml_node_ptr_ptr(n_lst, 6);

                                        let ret_val = xml_parse_external_entity(
                                            doc, sax, user_data, depth, url, id, lst,
                                        );
                                        desret_int(ret_val);
                                        des_xml_doc_ptr(n_doc, doc, 0);
                                        des_xml_saxhandler_ptr(n_sax, sax, 1);
                                        des_userdata(n_user_data, user_data, 2);
                                        des_int(n_depth, depth, 3);
                                        des_const_xml_char_ptr(n_url, url, 4);
                                        des_const_xml_char_ptr(n_id, id, 5);
                                        des_xml_node_ptr_ptr(n_lst, lst, 6);
                                        xml_reset_last_error();
                                        if mem_base != xml_mem_blocks() {
                                            leaks += 1;
                                            eprint!(
                                                "Leak of {} blocks found in xmlParseExternalEntity",
                                                xml_mem_blocks() - mem_base
                                            );
                                            eprint!(" {}", n_doc);
                                            eprint!(" {}", n_sax);
                                            eprint!(" {}", n_user_data);
                                            eprint!(" {}", n_depth);
                                            eprint!(" {}", n_url);
                                            eprint!(" {}", n_id);
                                            eprintln!(" {}", n_lst);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParseExternalEntity()"
            );
        }
    }

    #[test]
    fn test_xml_parse_file() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEPATH {
                let mem_base = xml_mem_blocks();
                let filename = gen_filepath(n_filename, 0);

                let ret_val = xml_parse_file(filename);
                desret_xml_doc_ptr(ret_val);
                des_filepath(n_filename, filename, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlParseFile",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_filename);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlParseFile()");
        }
    }

    #[test]
    fn test_xml_parse_in_node_context() {
        unsafe {
            let mut leaks = 0;
            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_data in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_datalen in 0..GEN_NB_INT {
                        for n_options in 0..GEN_NB_PARSEROPTIONS {
                            for n_lst in 0..GEN_NB_XML_NODE_PTR_PTR {
                                let mem_base = xml_mem_blocks();
                                let node = gen_xml_node_ptr(n_node, 0);
                                let data = gen_const_char_ptr(n_data, 1);
                                let datalen = gen_int(n_datalen, 2);
                                let options = gen_parseroptions(n_options, 3);
                                let lst = gen_xml_node_ptr_ptr(n_lst, 4);

                                let ret_val =
                                    xml_parse_in_node_context(node, data, datalen, options, lst);
                                desret_xml_parser_errors(ret_val);
                                des_xml_node_ptr(n_node, node, 0);
                                des_const_char_ptr(n_data, data, 1);
                                des_int(n_datalen, datalen, 2);
                                des_parseroptions(n_options, options, 3);
                                des_xml_node_ptr_ptr(n_lst, lst, 4);
                                xml_reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlParseInNodeContext",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_node);
                                    eprint!(" {}", n_data);
                                    eprint!(" {}", n_datalen);
                                    eprint!(" {}", n_options);
                                    eprintln!(" {}", n_lst);
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParseInNodeContext()"
            );
        }
    }

    #[test]
    fn test_xml_parse_memory() {
        #[cfg(feature = "sax1")]
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

                    let ret_val = xml_parse_memory(buffer, size);
                    desret_xml_doc_ptr(ret_val);
                    des_const_char_ptr(n_buffer, buffer, 0);
                    des_int(n_size, size, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlParseMemory",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_buffer);
                        eprintln!(" {}", n_size);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlParseMemory()");
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
                    xml_reset_last_error();
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
                    xml_reset_last_error();
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
                    xml_reset_last_error();
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
                    xml_reset_last_error();
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
                    xml_reset_last_error();
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
                xml_reset_last_error();
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
    fn test_xml_read_doc() {
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_url in 0..GEN_NB_FILEPATH {
                    for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                        for n_options in 0..GEN_NB_PARSEROPTIONS {
                            let mem_base = xml_mem_blocks();
                            let cur = gen_const_xml_char_ptr(n_cur, 0);
                            let url = gen_filepath(n_url, 1);
                            let encoding = gen_const_char_ptr(n_encoding, 2);
                            let options = gen_parseroptions(n_options, 3);

                            let ret_val =
                                xml_read_doc(cur as *const XmlChar, url, encoding, options);
                            desret_xml_doc_ptr(ret_val);
                            des_const_xml_char_ptr(n_cur, cur, 0);
                            des_filepath(n_url, url, 1);
                            des_const_char_ptr(n_encoding, encoding, 2);
                            des_parseroptions(n_options, options, 3);
                            xml_reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlReadDoc",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_cur);
                                eprint!(" {}", n_url);
                                eprint!(" {}", n_encoding);
                                eprintln!(" {}", n_options);
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlReadDoc()");
        }
    }

    #[test]
    fn test_xml_read_file() {
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEPATH {
                for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_options in 0..GEN_NB_PARSEROPTIONS {
                        let mem_base = xml_mem_blocks();
                        let filename = gen_filepath(n_filename, 0);
                        let encoding = gen_const_char_ptr(n_encoding, 1);
                        let options = gen_parseroptions(n_options, 2);

                        let ret_val = xml_read_file(filename, encoding, options);
                        desret_xml_doc_ptr(ret_val);
                        des_filepath(n_filename, filename, 0);
                        des_const_char_ptr(n_encoding, encoding, 1);
                        des_parseroptions(n_options, options, 2);
                        xml_reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlReadFile",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_filename);
                            eprint!(" {}", n_encoding);
                            eprintln!(" {}", n_options);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlReadFile()");
        }
    }

    #[test]
    fn test_xml_read_memory() {
        unsafe {
            let mut leaks = 0;

            for n_buffer in 0..GEN_NB_CONST_CHAR_PTR {
                for n_size in 0..GEN_NB_INT {
                    for n_url in 0..GEN_NB_FILEPATH {
                        for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                            for n_options in 0..GEN_NB_PARSEROPTIONS {
                                let mem_base = xml_mem_blocks();
                                let buffer = gen_const_char_ptr(n_buffer, 0);
                                let mut size = gen_int(n_size, 1);
                                let url = gen_filepath(n_url, 2);
                                let encoding = gen_const_char_ptr(n_encoding, 3);
                                let options = gen_parseroptions(n_options, 4);
                                if !buffer.is_null() && size > xml_strlen(buffer as _) {
                                    size = 0;
                                }

                                let ret_val = xml_read_memory(buffer, size, url, encoding, options);
                                desret_xml_doc_ptr(ret_val);
                                des_const_char_ptr(n_buffer, buffer, 0);
                                des_int(n_size, size, 1);
                                des_filepath(n_url, url, 2);
                                des_const_char_ptr(n_encoding, encoding, 3);
                                des_parseroptions(n_options, options, 4);
                                xml_reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlReadMemory",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_buffer);
                                    eprint!(" {}", n_size);
                                    eprint!(" {}", n_url);
                                    eprint!(" {}", n_encoding);
                                    eprintln!(" {}", n_options);
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlReadMemory()");
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
                xml_reset_last_error();
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
    fn test_xml_recover_file() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;

            for n_filename in 0..GEN_NB_FILEPATH {
                let mem_base = xml_mem_blocks();
                let filename = gen_filepath(n_filename, 0);

                let ret_val = xml_recover_file(filename);
                desret_xml_doc_ptr(ret_val);
                des_filepath(n_filename, filename, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlRecoverFile",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_filename);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlRecoverFile()");
        }
    }

    #[test]
    fn test_xml_recover_memory() {
        #[cfg(feature = "sax1")]
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

                    let ret_val = xml_recover_memory(buffer, size);
                    desret_xml_doc_ptr(ret_val);
                    des_const_char_ptr(n_buffer, buffer, 0);
                    des_int(n_size, size, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlRecoverMemory",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_buffer);
                        eprintln!(" {}", n_size);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlRecoverMemory()");
        }
    }

    #[test]
    fn test_xml_saxparse_dtd() {
        #[cfg(feature = "valid")]
        unsafe {
            let mut leaks = 0;

            for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                for n_external_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_system_id in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let sax = gen_xml_saxhandler_ptr(n_sax, 0);
                        let external_id = gen_const_xml_char_ptr(n_external_id, 1);
                        let system_id = gen_const_xml_char_ptr(n_system_id, 2);

                        let ret_val = xml_sax_parse_dtd(sax, external_id, system_id);
                        desret_xml_dtd_ptr(ret_val);
                        des_xml_saxhandler_ptr(n_sax, sax, 0);
                        des_const_xml_char_ptr(n_external_id, external_id, 1);
                        des_const_xml_char_ptr(n_system_id, system_id, 2);
                        xml_reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSAXParseDTD",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_sax);
                            eprint!(" {}", n_external_id);
                            eprintln!(" {}", n_system_id);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlSAXParseDTD()");
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
                        xml_reset_last_error();
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
    fn test_xml_saxparse_entity() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;

            for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                for n_filename in 0..GEN_NB_FILEPATH {
                    let mem_base = xml_mem_blocks();
                    let sax = gen_xml_saxhandler_ptr(n_sax, 0);
                    let filename = gen_filepath(n_filename, 1);

                    let ret_val = xml_sax_parse_entity(sax, filename);
                    desret_xml_doc_ptr(ret_val);
                    des_xml_saxhandler_ptr(n_sax, sax, 0);
                    des_filepath(n_filename, filename, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSAXParseEntity",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_sax);
                        eprintln!(" {}", n_filename);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlSAXParseEntity()");
        }
    }

    #[test]
    fn test_xml_saxparse_file() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;

            for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                for n_filename in 0..GEN_NB_FILEPATH {
                    for n_recovery in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let sax = gen_xml_saxhandler_ptr(n_sax, 0);
                        let filename = gen_filepath(n_filename, 1);
                        let recovery = gen_int(n_recovery, 2);

                        let ret_val = xml_sax_parse_file(sax, filename, recovery);
                        desret_xml_doc_ptr(ret_val);
                        des_xml_saxhandler_ptr(n_sax, sax, 0);
                        des_filepath(n_filename, filename, 1);
                        des_int(n_recovery, recovery, 2);
                        xml_reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSAXParseFile",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_sax);
                            eprint!(" {}", n_filename);
                            eprintln!(" {}", n_recovery);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlSAXParseFile()");
        }
    }

    #[test]
    fn test_xml_saxparse_file_with_data() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;

            for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                for n_filename in 0..GEN_NB_FILEPATH {
                    for n_recovery in 0..GEN_NB_INT {
                        for n_data in 0..GEN_NB_USERDATA {
                            let mem_base = xml_mem_blocks();
                            let sax = gen_xml_saxhandler_ptr(n_sax, 0);
                            let filename = gen_filepath(n_filename, 1);
                            let recovery = gen_int(n_recovery, 2);
                            let data = gen_userdata(n_data, 3);

                            let ret_val =
                                xml_sax_parse_file_with_data(sax, filename, recovery, data);
                            desret_xml_doc_ptr(ret_val);
                            des_xml_saxhandler_ptr(n_sax, sax, 0);
                            des_filepath(n_filename, filename, 1);
                            des_int(n_recovery, recovery, 2);
                            des_userdata(n_data, data, 3);
                            xml_reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSAXParseFileWithData",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_sax);
                                eprint!(" {}", n_filename);
                                eprint!(" {}", n_recovery);
                                eprintln!(" {}", n_data);
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlSAXParseFileWithData()"
            );
        }
    }

    #[test]
    fn test_xml_saxparse_memory() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;

            for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                for n_buffer in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_size in 0..GEN_NB_INT {
                        for n_recovery in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let sax = gen_xml_saxhandler_ptr(n_sax, 0);
                            let buffer = gen_const_char_ptr(n_buffer, 1);
                            let mut size = gen_int(n_size, 2);
                            let recovery = gen_int(n_recovery, 3);
                            if !buffer.is_null() && size > xml_strlen(buffer as _) {
                                size = 0;
                            }

                            let ret_val = xml_sax_parse_memory(sax, buffer, size, recovery);
                            desret_xml_doc_ptr(ret_val);
                            des_xml_saxhandler_ptr(n_sax, sax, 0);
                            des_const_char_ptr(n_buffer, buffer, 1);
                            des_int(n_size, size, 2);
                            des_int(n_recovery, recovery, 3);
                            xml_reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSAXParseMemory",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_sax);
                                eprint!(" {}", n_buffer);
                                eprint!(" {}", n_size);
                                eprintln!(" {}", n_recovery);
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlSAXParseMemory()");
        }
    }

    #[test]
    fn test_xml_saxparse_memory_with_data() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;

            for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                for n_buffer in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_size in 0..GEN_NB_INT {
                        for n_recovery in 0..GEN_NB_INT {
                            for n_data in 0..GEN_NB_USERDATA {
                                let mem_base = xml_mem_blocks();
                                let sax = gen_xml_saxhandler_ptr(n_sax, 0);
                                let buffer = gen_const_char_ptr(n_buffer, 1);
                                let mut size = gen_int(n_size, 2);
                                let recovery = gen_int(n_recovery, 3);
                                let data = gen_userdata(n_data, 4);
                                if !buffer.is_null() && size > xml_strlen(buffer as _) {
                                    size = 0;
                                }

                                let ret_val = xml_sax_parse_memory_with_data(
                                    sax, buffer, size, recovery, data,
                                );
                                desret_xml_doc_ptr(ret_val);
                                des_xml_saxhandler_ptr(n_sax, sax, 0);
                                des_const_char_ptr(n_buffer, buffer, 1);
                                des_int(n_size, size, 2);
                                des_int(n_recovery, recovery, 3);
                                des_userdata(n_data, data, 4);
                                xml_reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlSAXParseMemoryWithData",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_sax);
                                    eprint!(" {}", n_buffer);
                                    eprint!(" {}", n_size);
                                    eprint!(" {}", n_recovery);
                                    eprintln!(" {}", n_data);
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlSAXParseMemoryWithData()"
            );
        }
    }

    #[test]
    fn test_xml_saxuser_parse_file() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;

            for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                for n_user_data in 0..GEN_NB_USERDATA {
                    for n_filename in 0..GEN_NB_FILEPATH {
                        let mem_base = xml_mem_blocks();
                        let sax = gen_xml_saxhandler_ptr(n_sax, 0);
                        let mut user_data = gen_userdata(n_user_data, 1);
                        let filename = gen_filepath(n_filename, 2);

                        if sax == xml_default_sax_handler() as XmlSAXHandlerPtr {
                            user_data = null_mut();
                        }

                        let ret_val = xml_sax_user_parse_file(sax, user_data, filename);
                        desret_int(ret_val);
                        des_xml_saxhandler_ptr(n_sax, sax, 0);
                        des_userdata(n_user_data, user_data, 1);
                        des_filepath(n_filename, filename, 2);
                        xml_reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlSAXUserParseFile",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_sax);
                            eprint!(" {}", n_user_data);
                            eprintln!(" {}", n_filename);
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlSAXUserParseFile()"
            );
        }
    }

    #[test]
    fn test_xml_saxuser_parse_memory() {
        #[cfg(feature = "sax1")]
        unsafe {
            let mut leaks = 0;
            for n_sax in 0..GEN_NB_XML_SAXHANDLER_PTR {
                for n_user_data in 0..GEN_NB_USERDATA {
                    for n_buffer in 0..GEN_NB_CONST_CHAR_PTR {
                        for n_size in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let sax = gen_xml_saxhandler_ptr(n_sax, 0);
                            let mut user_data = gen_userdata(n_user_data, 1);
                            let buffer = gen_const_char_ptr(n_buffer, 2);
                            let mut size = gen_int(n_size, 3);
                            if !buffer.is_null() && size > xml_strlen(buffer as _) {
                                size = 0;
                            }

                            if sax == xml_default_sax_handler() as XmlSAXHandlerPtr {
                                user_data = null_mut();
                            }

                            let ret_val = xml_sax_user_parse_memory(sax, user_data, buffer, size);
                            desret_int(ret_val);
                            des_xml_saxhandler_ptr(n_sax, sax, 0);
                            des_userdata(n_user_data, user_data, 1);
                            des_const_char_ptr(n_buffer, buffer, 2);
                            des_int(n_size, size, 3);
                            xml_reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlSAXUserParseMemory",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_sax);
                                eprint!(" {}", n_user_data);
                                eprint!(" {}", n_buffer);
                                eprintln!(" {}", n_size);
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlSAXUserParseMemory()"
            );
        }
    }

    #[test]
    fn test_xml_set_external_entity_loader() {

        /* missing type support */
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
                        xml_reset_last_error();
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
    fn test_xml_stop_parser() {
        #[cfg(feature = "push")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);

                xml_stop_parser(ctxt);
                des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlStopParser",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlStopParser()");
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
                xml_reset_last_error();
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

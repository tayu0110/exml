use std::{
    borrow::Cow,
    cell::RefCell,
    collections::HashMap,
    ffi::c_void,
    io::Read,
    mem::take,
    ptr::null_mut,
    rc::Rc,
    str::{from_utf8, from_utf8_unchecked},
    sync::atomic::AtomicPtr,
};

use crate::{
    encoding::{
        XmlCharEncoding, XmlCharEncodingHandler, detect_encoding, find_encoding_handler,
        get_encoding_handler,
    },
    error::{XmlError, XmlParserErrors, parser_validity_error, parser_validity_warning},
    generic_error,
    globals::{
        GenericErrorContext, get_do_validity_checking_default_value,
        get_get_warnings_default_value, get_keep_blanks_default_value,
        get_line_numbers_default_value, get_load_ext_dtd_default_value, get_parser_debug_entities,
        get_pedantic_parser_default_value, get_substitute_entities_default_value,
    },
    hash::XmlHashTableRef,
    io::{XmlParserInputBuffer, xml_parser_get_directory},
    libxml::{
        catalog::XmlCatalogEntry,
        chvalid::{xml_is_blank_char, xml_is_char},
        sax2::{
            xml_sax_version, xml_sax2_end_element, xml_sax2_ignorable_whitespace,
            xml_sax2_start_element,
        },
    },
    parser::{
        __xml_err_encoding, INPUT_CHUNK, XML_COMPLETE_ATTRS, XML_DETECT_IDS, XML_MAX_LOOKUP_LIMIT,
        XML_PARSER_MAX_DEPTH, XML_VCTXT_USE_PCTXT, XmlParserInputState, XmlSAXHandler, XmlStartTag,
        xml_err_encoding_int, xml_err_internal, xml_fatal_err_msg_int, xml_fatal_err_msg_str,
        xml_init_parser,
    },
    tree::{
        XML_ENT_EXPANDING, XML_ENT_PARSED, XML_XML_NAMESPACE, XmlAttrPtr, XmlAttributeType,
        XmlDocPtr, XmlEntityType, XmlNodePtr, xml_free_doc,
    },
    uri::{build_uri, canonic_path},
    valid::XmlValidCtxt,
};

use super::{
    XmlParserInput, XmlParserNodeInfo, XmlParserNodeInfoSeq, xml_err_memory, xml_fatal_err,
    xml_load_external_entity,
};

/// Special constant found in SAX2 blocks initialized fields
pub const XML_SAX2_MAGIC: usize = 0xDEEDBEAF;

/// This is the set of XML parser options that can be passed down
/// to the xmlReadDoc() and similar calls.
#[doc(alias = "xmlParserOption")]
#[repr(C)]
pub enum XmlParserOption {
    XmlParseRecover = 1 << 0,     /* recover on errors */
    XmlParseNoEnt = 1 << 1,       /* substitute entities */
    XmlParseDTDLoad = 1 << 2,     /* load the external subset */
    XmlParseDTDAttr = 1 << 3,     /* default DTD attributes */
    XmlParseDTDValid = 1 << 4,    /* validate with the DTD */
    XmlParseNoError = 1 << 5,     /* suppress error reports */
    XmlParseNoWarning = 1 << 6,   /* suppress warning reports */
    XmlParsePedantic = 1 << 7,    /* pedantic error reporting */
    XmlParseNoBlanks = 1 << 8,    /* remove blank nodes */
    XmlParseSAX1 = 1 << 9,        /* use the SAX1 interface internally */
    XmlParseXInclude = 1 << 10,   /* Implement XInclude substitution  */
    XmlParseNoNet = 1 << 11,      /* Forbid network access */
    XmlParseNoDict = 1 << 12,     /* Do not reuse the context dictionary */
    XmlParseNsClean = 1 << 13,    /* remove redundant namespaces declarations */
    XmlParseNoCDATA = 1 << 14,    /* merge CDATA as text nodes */
    XmlParseNoXIncnode = 1 << 15, /* do not generate XINCLUDE START/END nodes */
    XmlParseCompact = 1 << 16,    /* compact small text nodes; no modification of
                                                  the tree allowed afterwards (will possibly
                                  crash if you try to modify the tree) */
    XmlParseOld10 = 1 << 17,     /* parse using XML-1.0 before update 5 */
    XmlParseNoBasefix = 1 << 18, /* do not fixup XINCLUDE xml:base uris */
    XmlParseHuge = 1 << 19,      /* relax any hardcoded limit from the parser */
    XmlParseOldSAX = 1 << 20,    /* parse using SAX2 interface before 2.7.0 */
    XmlParseIgnoreEnc = 1 << 21, /* ignore internal document encoding hint */
    XmlParseBigLines = 1 << 22,  /* Store big lines numbers in text PSVI field */
}

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

pub type XmlParserCtxtPtr<'a> = *mut XmlParserCtxt<'a>;
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
pub struct XmlParserCtxt<'a> {
    // The SAX handler
    pub sax: Option<Box<XmlSAXHandler>>,
    // For SAX interface only, used by DOM build
    pub(crate) user_data: Option<GenericErrorContext>,
    // the document being built
    pub my_doc: Option<XmlDocPtr>,
    // is the document well formed
    pub well_formed: bool,
    // shall we replace entities ?
    pub(crate) replace_entities: bool,
    // the XML version string
    pub(crate) version: Option<String>,
    // the declared encoding, if any
    pub encoding: Option<String>,
    // standalone document
    pub(crate) standalone: i32,
    // an HTML(1) document
    //  3 is HTML after <head>
    //  10 is HTML after <body>
    pub(crate) html: i32,

    // Input stream stack
    // stack of inputs
    pub input_tab: Vec<XmlParserInput<'a>>,

    // Node analysis stack only used for DOM building
    // Current parsed Node
    pub(crate) node: Option<XmlNodePtr>,
    // array of nodes
    pub(crate) node_tab: Vec<XmlNodePtr>,

    // Whether node info should be kept
    pub(crate) record_info: bool,
    // info about each node parsed
    pub(crate) node_seq: XmlParserNodeInfoSeq,

    // error code
    pub err_no: i32,

    // reference and external subset
    pub(crate) has_external_subset: bool,
    // the internal subset has PE refs
    pub(crate) has_perefs: bool,
    // are we parsing an external entity
    pub(crate) external: i32,

    // is the document valid
    pub valid: i32,
    // shall we try to validate ?
    pub(crate) validate: bool,
    // The validity context
    pub vctxt: XmlValidCtxt,

    // current type of input
    pub instate: XmlParserInputState,
    // next char look-ahead
    pub(crate) token: i32,

    // the data directory
    pub(crate) directory: Option<String>,

    // Node name stack
    // Current parsed Node
    pub(crate) name: Option<Rc<str>>,
    // array of nodes
    pub(crate) name_tab: Vec<Rc<str>>,

    // unused
    nb_chars: i64,
    // used by progressive parsing lookup
    pub(crate) check_index: usize,
    // ugly but ...
    pub(crate) keep_blanks: bool,
    // SAX callbacks are disabled
    pub(crate) disable_sax: bool,
    // Parsing is in int 1/ext 2 subset
    pub in_subset: i32,
    // name of subset
    pub(crate) int_sub_name: Option<Rc<str>>,
    // URI of external subset
    pub(crate) ext_sub_uri: Option<Rc<str>>,
    // SYSTEM ID of external subset
    pub(crate) ext_sub_system: Option<Rc<str>>,

    // xml:space values
    // array of space infos
    pub(crate) space_tab: Vec<i32>,

    // to prevent entity substitution loops
    pub(crate) depth: i32,
    // encoding of the in-memory content
    // actually an xmlCharEncoding
    pub charset: XmlCharEncoding,

    // signal pedantic warnings
    pub(crate) pedantic: bool,
    // For user data, libxml won't touch it
    pub(crate) _private: *mut c_void,

    // should the external subset be loaded
    pub(crate) loadsubset: i32,
    // set line number in element content
    pub(crate) linenumbers: i32,
    // document's own catalog
    #[cfg(feature = "catalog")]
    pub(crate) catalogs: Option<XmlCatalogEntry>,
    // run in recovery mode
    pub(crate) recovery: bool,
    // is this a progressive parsing
    pub(crate) progressive: bool,
    // array for the attributes callbacks
    pub(crate) atts: Vec<(String, Option<String>)>,

    // pre-interned strings
    pub(crate) str_xml: Option<Cow<'static, str>>,
    pub(crate) str_xmlns: Option<Cow<'static, str>>,
    pub(crate) str_xml_ns: Option<Cow<'static, str>>,

    // Everything below is used only by the new SAX mode
    // operating in the new SAX mode
    pub(crate) sax2: bool,
    // the array of prefix/namespace name
    pub(crate) ns_tab: Vec<(Option<String>, String)>,
    // array of data for push
    pub(crate) push_tab: Vec<XmlStartTag>,
    // defaulted attributes if any
    // Key      : (name, prefix)
    // Value    : (name, prefix, value, is_external)
    #[allow(clippy::type_complexity)]
    pub(crate) atts_default: HashMap<
        (Cow<'static, str>, Option<Cow<'static, str>>),
        Vec<(String, Option<String>, String, Option<&'static str>)>,
    >,
    // non-CDATA attributes if any
    pub(crate) atts_special: Option<XmlHashTableRef<'static, XmlAttributeType>>,
    // is the document XML Namespace okay
    pub(crate) ns_well_formed: bool,
    // Extra options
    pub(crate) options: i32,

    // Those fields are needed only for streaming parsing so far
    // number of freed element nodes
    pub(crate) free_elems_nr: i32,
    // List of freed element nodes
    pub(crate) free_elems: Option<XmlNodePtr>,
    // number of freed attributes nodes
    pub(crate) free_attrs_nr: i32,
    // List of freed attributes nodes
    pub(crate) free_attrs: Option<XmlAttrPtr>,

    // the complete error information for the last error.
    pub last_error: XmlError,
    // the parser mode
    pub(crate) parse_mode: XmlParserMode,
    // unused
    pub(crate) nbentities: u64,
    // size of parsed entities
    pub sizeentities: u64,

    // for use by HTML non-recursive parser
    // array of nodeInfos
    pub(crate) node_info_tab: Vec<Rc<RefCell<XmlParserNodeInfo>>>,

    // we need to label inputs
    pub(crate) input_id: i32,
    // volume of entity copy
    pub sizeentcopy: u64,

    // quote state for push parser
    pub(crate) end_check_state: i32,
    // number of errors
    pub nb_errors: u16,
    // number of warnings
    pub(crate) nb_warnings: u16,
}

impl<'a> XmlParserCtxt<'a> {
    /// Allocate and initialize a new parser context.
    ///
    /// Returns the xmlParserCtxtPtr or NULL
    #[doc(alias = "xmlNewParserCtxt")]
    pub fn new() -> Option<Self> {
        Self::new_sax_parser(None, None).ok()
    }

    /// Allocate and initialize a new SAX parser context.   
    /// If userData is NULL, the parser context will be passed as user data.
    ///
    /// Returns the xmlParserCtxtPtr or NULL if memory allocation failed.
    #[doc(alias = "xmlNewSAXParserCtxt")]
    pub fn new_sax_parser(
        sax: Option<Box<XmlSAXHandler>>,
        user_data: Option<GenericErrorContext>,
    ) -> Result<Self, Option<Box<XmlSAXHandler>>> {
        let mut ctxt = XmlParserCtxt::default();
        ctxt.init_sax_parser(sax, user_data).map(|_| ctxt)
    }

    /// Create a parser context for a file content.
    ///
    /// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
    /// by default if found at compile-time.  
    /// However, this crate does not support currently.
    ///
    /// Returns the new parser context or NULL
    #[doc(alias = "xmlCreateFileParserCtxt")]
    pub fn from_filename(filename: Option<&str>) -> Option<XmlParserCtxt> {
        Self::from_filename_with_options(filename, 0)
    }

    /// Create a parser context for a file or URL content.
    ///
    /// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
    /// by default if found at compile-time.  
    /// However, this crate does not support currently.
    ///
    /// Returns the new parser context or NULL
    #[doc(alias = "xmlCreateURLParserCtxt")]
    pub fn from_filename_with_options(
        filename: Option<&str>,
        options: i32,
    ) -> Option<XmlParserCtxt> {
        let Some(mut ctxt) = XmlParserCtxt::new() else {
            xml_err_memory(None, Some("cannot allocate parser context"));
            return None;
        };

        if options != 0 {
            ctxt.use_options_internal(options, None);
        }
        ctxt.linenumbers = 1;

        let input_stream = xml_load_external_entity(filename, None, &mut ctxt)?;

        ctxt.input_push(input_stream);
        if ctxt.directory.is_none() {
            if let Some(filename) = filename {
                if let Some(directory) = xml_parser_get_directory(filename) {
                    ctxt.directory = Some(directory.to_string_lossy().into_owned());
                }
            }
        }

        Some(ctxt)
    }

    /// Create a parser context for using the XML parser with an existing I/O stream
    ///
    /// Returns the new parser context or NULL
    #[doc(alias = "xmlCreateIOParserCtxt")]
    pub fn from_io(
        sax: Option<Box<XmlSAXHandler>>,
        user_data: Option<GenericErrorContext>,
        ioctx: impl Read + 'a,
        enc: XmlCharEncoding,
    ) -> Option<Self> {
        let buf = XmlParserInputBuffer::from_reader(ioctx, enc);
        let mut ctxt = XmlParserCtxt::new_sax_parser(sax, user_data).ok()?;

        let input_stream = XmlParserInput::from_io(&mut ctxt, buf, enc)?;
        ctxt.input_push(input_stream);
        Some(ctxt)
    }

    /// Create a parser context for an XML in-memory document.
    ///
    /// Returns the new parser context or NULL
    #[doc(alias = "xmlCreateMemoryParserCtxt", alias = "xmlCreateDocParserCtxt")]
    pub fn from_memory(buffer: &'a [u8]) -> Option<Self> {
        if buffer.is_empty() {
            return None;
        }

        let mut ctxt = XmlParserCtxt::new()?;

        let buf = XmlParserInputBuffer::from_memory(buffer, XmlCharEncoding::None)?;
        let mut input = XmlParserInput::new(Some(&mut ctxt))?;
        input.filename = None;
        input.buf = Some(buf);
        input.reset_base();

        ctxt.input_push(input);
        Some(ctxt)
    }

    /// Create a parser context for an external entity
    ///
    /// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
    /// by default if found at compile-time.  
    /// However, this crate does not support currently.
    ///
    /// If create new context successfully, return new context wrapped `Ok`.  
    /// Otherwise, return received SAX handler wrapped `Err`.
    /// Returns the new parser context or NULL
    #[doc(alias = "xmlCreateEntityParserCtxtInternal")]
    pub(crate) fn new_entity_parser_internal(
        sax: Option<Box<XmlSAXHandler>>,
        user_data: Option<GenericErrorContext>,
        mut url: Option<&str>,
        id: Option<&str>,
        base: Option<&str>,
        pctxt: Option<&XmlParserCtxt>,
    ) -> Result<Self, Option<Box<XmlSAXHandler>>> {
        let mut ctxt = XmlParserCtxt::new_sax_parser(sax, user_data)?;

        if let Some(pctxt) = pctxt {
            ctxt.options = pctxt.options;
            ctxt._private = pctxt._private;
            ctxt.input_id = pctxt.input_id;
        }

        // Don't read from stdin.
        if url == Some("-") {
            url = Some("./-");
        }

        if let Some(uri) = url.zip(base).and_then(|(url, base)| build_uri(url, base)) {
            let Some(input_stream) = xml_load_external_entity(Some(&uri), id, &mut ctxt) else {
                let sax = ctxt.sax.take();
                return Err(sax);
            };
            ctxt.input_push(input_stream);

            if ctxt.directory.is_none() {
                if let Some(url) = url {
                    if let Some(directory) = xml_parser_get_directory(url) {
                        ctxt.directory = Some(directory.to_string_lossy().into_owned());
                    }
                }
            }
        } else {
            let Some(input_stream) = xml_load_external_entity(url, id, &mut ctxt) else {
                let sax = ctxt.sax.take();
                return Err(sax);
            };
            ctxt.input_push(input_stream);

            if ctxt.directory.is_none() {
                if let Some(url) = url {
                    if let Some(directory) = xml_parser_get_directory(url) {
                        ctxt.directory = Some(directory.to_string_lossy().into_owned());
                    }
                }
            }
        }
        Ok(ctxt)
    }

    /// Create a parser context for an external entity
    ///
    /// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
    /// by default if found at compile-time.  
    /// However, this crate does not support currently.
    ///
    /// Returns the new parser context or NULL
    #[doc(alias = "xmlCreateEntityParserCtxt")]
    pub fn new_entity_parser(
        url: Option<&str>,
        id: Option<&str>,
        base: Option<&str>,
    ) -> Result<Self, Option<Box<XmlSAXHandler>>> {
        Self::new_entity_parser_internal(None, None, url, id, base, None)
    }

    /// Initialize a SAX parser context
    ///
    /// Returns 0 in case of success and -1 in case of error
    #[doc(alias = "xmlInitSAXParserCtxt")]
    fn init_sax_parser(
        &mut self,
        sax: Option<Box<XmlSAXHandler>>,
        user_data: Option<GenericErrorContext>,
    ) -> Result<(), Option<Box<XmlSAXHandler>>> {
        xml_init_parser();

        if let Some(mut sax) = sax {
            if sax.initialized != XML_SAX2_MAGIC as u32 {
                // These fields won't used in SAX1 handling.
                sax._private = AtomicPtr::new(null_mut());
                sax.start_element_ns = None;
                sax.end_element_ns = None;
                sax.serror = None;
            }
            self.sax = Some(sax);
            self.user_data = user_data;
        } else {
            let mut sax = XmlSAXHandler::default();
            xml_sax_version(&mut sax, 2);
            self.sax = Some(Box::new(sax));
            self.user_data = None;
        }

        self.atts = vec![];
        // Allocate the Input stack
        self.input_tab.clear();
        self.version = None;
        self.encoding = None;
        self.standalone = -1;
        self.has_external_subset = false;
        self.has_perefs = false;
        self.html = 0;
        self.external = 0;
        self.instate = XmlParserInputState::XmlParserStart;
        self.token = 0;
        self.directory = None;

        // Allocate the Node stack
        self.node_tab.clear();
        self.node = None;

        // Allocate the Name stack
        self.name_tab.clear();
        self.name = None;

        // Allocate the space stack
        self.space_tab.clear();
        self.space_tab.push(-1);

        self.my_doc = None;
        self.well_formed = true;
        self.ns_well_formed = true;
        self.valid = 1;
        self.loadsubset = get_load_ext_dtd_default_value();
        if self.loadsubset != 0 {
            self.options |= XmlParserOption::XmlParseDTDLoad as i32;
        }
        self.validate = get_do_validity_checking_default_value();
        self.pedantic = get_pedantic_parser_default_value();
        if self.pedantic {
            self.options |= XmlParserOption::XmlParsePedantic as i32;
        }
        self.linenumbers = get_line_numbers_default_value();
        self.keep_blanks = get_keep_blanks_default_value();
        if !self.keep_blanks {
            if let Some(sax) = self.sax.as_deref_mut() {
                sax.ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
            }
            self.options |= XmlParserOption::XmlParseNoBlanks as i32;
        }

        self.vctxt.flags = XML_VCTXT_USE_PCTXT as _;
        self.vctxt.user_data = None;
        self.vctxt.error = Some(parser_validity_error);
        self.vctxt.warning = Some(parser_validity_warning);
        if self.validate {
            if get_get_warnings_default_value() == 0 {
                self.vctxt.warning = None;
            } else {
                self.vctxt.warning = Some(parser_validity_warning);
            }
            self.vctxt.node_tab.clear();
            self.options |= XmlParserOption::XmlParseDTDValid as i32;
        }
        self.replace_entities = get_substitute_entities_default_value();
        if self.replace_entities {
            self.options |= XmlParserOption::XmlParseNoEnt as i32;
        }
        self.record_info = false;
        self.check_index = 0;
        self.in_subset = 0;
        self.err_no = XmlParserErrors::XmlErrOK as i32;
        self.depth = 0;
        self.charset = XmlCharEncoding::UTF8;
        #[cfg(feature = "catalog")]
        {
            self.catalogs = None;
        }
        self.sizeentities = 0;
        self.sizeentcopy = 0;
        self.input_id = 1;
        self.node_seq.clear();
        Ok(())
    }

    pub fn encoding(&self) -> Option<&str> {
        self.encoding.as_deref()
    }

    pub(crate) fn current_byte(&self) -> u8 {
        *self.content_bytes().first().unwrap_or(&0)
    }

    pub(crate) fn nth_byte(&self, nth: usize) -> u8 {
        *self.content_bytes().get(nth).unwrap_or(&0)
    }

    pub fn input(&self) -> Option<&XmlParserInput> {
        self.input_tab.last()
    }

    pub fn input_mut(&mut self) -> Option<&mut XmlParserInput<'a>> {
        self.input_tab.last_mut()
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
    pub fn byte_consumed(&mut self) -> i64 {
        let Some(input) = self.input() else {
            return -1;
        };
        if input.buf.is_some() && input.buf.as_ref().unwrap().encoder.is_some() {
            let mut unused = 0;
            // Encoding conversion, compute the number of unused original
            // bytes from the input not consumed and subtract that from
            // the raw consumed value, this is not a cheap operation
            if input.remainder_len() > 0 {
                // The original code seems to continue processing as long as the write succeeds,
                // even if encoding errors occur.
                // However, the new API stops processing when an error occurs,
                // so it is not possible to reproduce such a process ...
                let mut out = [0u8; 32000];
                let Ok(input) = from_utf8(self.content_bytes()) else {
                    return -1;
                };
                let input = input.to_owned();
                let handler = self
                    .input_mut()
                    .unwrap()
                    .buf
                    .as_mut()
                    .unwrap()
                    .encoder
                    .as_mut()
                    .unwrap();
                let mut read = 0;
                while read < input.len() {
                    let Ok((r, w)) = handler.encode(&input[read..], &mut out) else {
                        return -1;
                    };
                    unused += w;
                    read += r;
                }
            }
            let input = self.input().unwrap();
            if input.buf.as_ref().unwrap().rawconsumed < unused as u64 {
                return -1;
            }
            return (input.buf.as_ref().unwrap().rawconsumed - unused as u64) as i64;
        }
        input.consumed as i64 + input.offset_from_base() as i64
    }

    #[doc(alias = "xmlParserGrow")]
    pub(crate) fn force_grow(&mut self) -> i32 {
        // Don't grow push parser buffer.
        if self.progressive {
            return 0;
        }

        let input = self.input_mut().unwrap();
        let cur_end = input.remainder_len();
        let cur_base = input.offset_from_base();

        let Some(buf) = input.buf.as_mut() else {
            return 0;
        };
        // Don't grow memory buffers.
        if buf.encoder.is_none() && buf.context.is_none() {
            return 0;
        }

        if (cur_end > XML_MAX_LOOKUP_LIMIT || cur_base > XML_MAX_LOOKUP_LIMIT)
            && self.options & XmlParserOption::XmlParseHuge as i32 == 0
        {
            xml_err_internal!(self, "Huge input lookup");
            self.halt();
            return -1;
        }

        if cur_end >= INPUT_CHUNK {
            return 0;
        }

        let input = self.input_mut().unwrap();
        let ret: i32 = input.buf.as_mut().unwrap().grow(INPUT_CHUNK);

        // TODO: Get error code from xmlParserInputBufferGrow
        if ret < 0 {
            xml_err_internal!(self, "Growing input buffer");
            self.halt();
        }

        ret
    }

    pub(crate) fn grow(&mut self) {
        if !self.progressive && self.input().unwrap().remainder_len() < INPUT_CHUNK {
            self.force_grow();
        }
    }

    #[doc(alias = "xmlParserShrink")]
    pub(crate) fn force_shrink(&mut self) {
        let progressive = self.progressive;

        let input = self.input_mut().unwrap();

        // Don't shrink pull parser memory buffers.
        let Some(buf) = input.buf.as_mut() else {
            return;
        };
        if !progressive && buf.encoder.is_none() && buf.context.is_none() {
            return;
        }

        // Do not shrink on large buffers whose only a tiny fraction was consumed
        input.shrink();
    }

    pub(crate) fn shrink(&mut self) {
        if !self.progressive
            && self.input().unwrap().offset_from_base() > 2 * INPUT_CHUNK
            && self.input().unwrap().remainder_len() < 2 * INPUT_CHUNK
        {
            self.force_shrink();
        }
    }

    /// Blocks further parser processing don't override error.
    #[doc(alias = "xmlHaltParser")]
    pub(crate) fn halt(&mut self) {
        self.instate = XmlParserInputState::XmlParserEOF;
        self.disable_sax = true;
        while self.input_tab.len() > 1 {
            self.input_pop();
        }
        if let Some(input) = self.input_mut() {
            // in case there was a specific allocation deallocate before overriding base
            if input.buf.is_some() {
                let _ = input.buf.take();
            }
            input.cur = 0;
            input.length = 0;
            input.base = 0;
        }
    }

    /// Blocks further parser processing
    #[doc(alias = "xmlStopParser")]
    pub fn stop(&mut self) {
        self.halt();
        self.err_no = XmlParserErrors::XmlErrUserStop as i32;
    }

    /// Reset a parser context
    #[doc(alias = "xmlCtxtReset")]
    pub fn reset(&mut self) {
        self.input_tab.clear();
        self.space_tab.clear();
        self.node_tab.clear();
        self.node = None;
        self.name_tab.clear();
        self.name = None;
        self.ns_tab.clear();
        self.version = None;
        self.encoding = None;
        self.directory = None;
        self.ext_sub_uri = None;
        self.ext_sub_system = None;
        if let Some(doc) = self.my_doc.take() {
            unsafe {
                xml_free_doc(doc);
            }
        }

        self.standalone = -1;
        self.has_external_subset = false;
        self.has_perefs = false;
        self.html = 0;
        self.external = 0;
        self.instate = XmlParserInputState::XmlParserStart;
        self.token = 0;
        self.well_formed = true;
        self.ns_well_formed = true;
        self.disable_sax = false;
        self.valid = 1;
        self.record_info = false;
        self.check_index = 0;
        self.end_check_state = 0;
        self.in_subset = 0;
        self.err_no = XmlParserErrors::XmlErrOK as i32;
        self.depth = 0;
        self.charset = XmlCharEncoding::UTF8;
        #[cfg(feature = "catalog")]
        {
            self.catalogs = None;
        }
        self.sizeentities = 0;
        self.sizeentcopy = 0;
        self.node_seq.clear();
        self.atts_default.clear();
        let _ = self.atts_special.take().map(|t| t.into_inner());

        #[cfg(feature = "catalog")]
        {
            self.catalogs = None;
        }
        self.nb_errors = 0;
        self.nb_warnings = 0;
        if self.last_error.is_err() {
            self.last_error.reset();
        }
    }

    /// Reset a push parser context
    ///
    /// Returns 0 in case of success and 1 in case of error
    #[doc(alias = "xmlCtxtResetPush")]
    pub fn reset_push(
        &mut self,
        chunk: &[u8],
        filename: Option<&str>,
        encoding: Option<&str>,
    ) -> i32 {
        let enc = if encoding.is_none() && chunk.len() >= 4 {
            detect_encoding(chunk)
        } else {
            XmlCharEncoding::None
        };

        let buf = XmlParserInputBuffer::new(enc);

        self.reset();

        if filename.is_none() {
            self.directory = None;
        } else if let Some(dir) = filename.and_then(xml_parser_get_directory) {
            self.directory = Some(dir.to_string_lossy().into_owned());
        }

        let Some(mut input_stream) = XmlParserInput::new(Some(self)) else {
            return 1;
        };

        input_stream.filename = filename
            .map(canonic_path)
            .map(|filanem| filanem.into_owned());
        input_stream.buf = Some(buf);
        input_stream.reset_base();

        self.input_push(input_stream);

        if !chunk.is_empty() && self.input().is_some() && self.input().unwrap().buf.is_some() {
            self.input_mut()
                .unwrap()
                .buf
                .as_mut()
                .unwrap()
                .push_bytes(chunk);
        }

        if let Some(encoding) = encoding {
            self.encoding = Some(encoding.to_owned());
            if let Some(handler) = find_encoding_handler(self.encoding().unwrap()) {
                self.switch_to_encoding(handler);
            } else {
                xml_fatal_err_msg_str!(
                    self,
                    XmlParserErrors::XmlErrUnsupportedEncoding,
                    "Unsupported encoding {}\n",
                    encoding
                );
            };
        } else if !matches!(enc, XmlCharEncoding::None) {
            self.switch_encoding(enc);
        }

        0
    }

    /// Clear (release owned resources) and reinitialize a parser context
    #[doc(alias = "xmlClearParserCtxt")]
    pub fn clear(&mut self) {
        self.node_seq.clear();
        self.reset();
    }

    pub(crate) fn advance(&mut self, nth: usize) {
        if self.content_bytes().len() < nth {
            self.force_grow();
        }
        let input = self.input_mut().unwrap();
        input.cur += nth;
        input.col += nth as i32;
    }

    /// Advance the current pointer.  
    /// If `'\n'` is found, line number is also increased.
    pub(crate) fn advance_with_line_handling(&mut self, nth: usize) {
        if self.content_bytes().len() < nth {
            self.force_grow();
        }
        let input = self.input_mut().unwrap();
        for _ in 0..nth {
            if input.base_contents()[input.cur] == b'\n' {
                input.line += 1;
                input.col = 1;
            } else {
                input.col += 1;
            }
            input.cur += 1;
        }
    }

    pub fn content_bytes(&self) -> &[u8] {
        let input = self.input().unwrap();
        input.current_contents()
    }

    /// Skip to the next character.
    #[doc(alias = "xmlNextChar")]
    pub(crate) fn skip_char(&mut self) {
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) || self.input().is_none() {
            return;
        }

        let input = self.input().unwrap();
        if input.cur > input.base_contents().len() {
            xml_err_internal!(self, "Parser input data memory error\n");

            self.err_no = XmlParserErrors::XmlErrInternalError as i32;
            self.stop();

            return;
        }

        if input.remainder_len() < INPUT_CHUNK {
            if self.force_grow() < 0 {
                return;
            }
            if self.content_bytes().is_empty() {
                return;
            }
        }

        if self.charset != XmlCharEncoding::UTF8 {
            // Assume it's a fixed length encoding (1) with
            // a compatible encoding for the ASCII set, since
            // XML constructs only use < 128 chars

            let input = self.input_mut().unwrap();
            if input.base_contents()[input.cur] == b'\n' {
                input.line += 1;
                input.col = 1;
            } else {
                input.col += 1;
            }
            input.cur += 1;
            return;
        }

        let mut len = 0;
        let Some(c) = self.current_char(&mut len) else {
            return;
        };
        // 2.11 End-of-Line Handling
        //   the literal two-character sequence "#xD#xA" or a standalone
        //   literal #xD, an XML processor must pass to the application
        //   the single character #xA.
        let input = self.input_mut().unwrap();
        if c == '\n' {
            input.line += 1;
            input.col = 1;
        } else {
            input.col += 1;
        }
        input.cur += len as usize;
    }

    /// skip all blanks character found at that point in the input streams.  
    /// It pops up finished entities in the process if allowable at that point.
    ///
    /// Returns the number of space chars skipped
    #[doc(alias = "xmlSkipBlankChars")]
    pub(crate) fn skip_blanks(&mut self) -> i32 {
        let mut res = 0i32;

        // It's Okay to use CUR/NEXT here since all the blanks are on the ASCII range.
        if (self.input_tab.len() == 1 && !matches!(self.instate, XmlParserInputState::XmlParserDTD))
            || matches!(self.instate, XmlParserInputState::XmlParserStart)
        {
            // if we are in the document content, go really fast
            let input = self.input().unwrap();
            let mut line = input.line;
            let mut col = input.col;
            self.force_grow();
            let mut content = self.content_bytes();
            while content
                .first()
                .is_some_and(|&b| xml_is_blank_char(b as u32))
            {
                if content[0] == b'\n' {
                    line += 1;
                    col = 1;
                } else {
                    col += 1;
                }
                content = &content[1..];
                res = res.saturating_add(1);
                if content.is_empty() {
                    let len = self.content_bytes().len();
                    let input = self.input_mut().unwrap();
                    input.cur += len;
                    input.line = line;
                    input.col = col;
                    self.force_grow();
                    content = self.content_bytes();
                }
            }

            let diff = self.content_bytes().len() - content.len();
            if diff > 0 {
                let input = self.input_mut().unwrap();
                input.cur += diff;
                input.line = line;
                input.col = col;
            }
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
                    self.parse_pe_reference();
                } else if self.current_byte() == 0 {
                    let mut consumed: u64;

                    if self.input_tab.len() <= 1 {
                        break;
                    }

                    consumed = self.input().unwrap().consumed;
                    consumed =
                        consumed.saturating_add(self.input().unwrap().offset_from_base() as u64);

                    // Add to sizeentities when parsing an external entity for the first time.
                    // Is this `unwrap` OK ????
                    let mut ent = self.input().unwrap().entity.unwrap();
                    if matches!(ent.etype, XmlEntityType::XmlExternalParameterEntity)
                        && ent.flags & XML_ENT_PARSED as i32 == 0
                    {
                        ent.flags |= XML_ENT_PARSED as i32;

                        self.sizeentities = self.sizeentities.saturating_add(consumed);
                    }

                    self.parser_entity_check(consumed);

                    self.pop_input();
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
    #[doc(alias = "xmlCurrentChar")]
    pub(crate) fn current_char(&mut self, len: &mut i32) -> Option<char> {
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }

        if self.input()?.remainder_len() < INPUT_CHUNK && self.force_grow() < 0 {
            return None;
        }

        if (0x20..0x80).contains(&self.current_byte()) {
            *len = 1;
            return Some(self.current_byte() as char);
        }

        // if self.charset != XmlCharEncoding::UTF8 {
        //     // Assume it's a fixed length encoding (1) with
        //     // a compatible encoding for the ASCII set, since
        //     // XML constructs only use < 128 chars
        //     *len = 1;
        //     if self.current_byte() == 0xD {
        //         if self.nth_byte(1) == 0xA {
        //             let input = self.input_mut()?;
        //             input.cur += 1;
        //         }
        //         return Some('\u{A}');
        //     }
        //     return Some(self.current_byte() as char);
        // }

        *len = 0;
        let input = self.input_mut().unwrap();
        let c = if let Some(buf) = input.buf.as_ref() {
            if buf.encoder.is_some() {
                unsafe {
                    // # Safety
                    // If `buf.encoder` is `Some`, `buf.buffer` is decoded by `buf.encoder`.
                    // Decoded buffer is already validated as UTF-8 byte sequence,
                    // so this function works well.
                    from_utf8_unchecked(&buf.buffer[input.cur..])
                        .chars()
                        .next()?
                }
            } else if input.cur < input.valid_up_to {
                unsafe {
                    // # Safety
                    // `buf.buffer[input.cur..input.valid_up_to]` is kept valid
                    // as a UTF-8 byte sequence.
                    from_utf8_unchecked(&buf.buffer[input.cur..input.valid_up_to])
                        .chars()
                        .next()?
                }
            } else {
                match from_utf8(&buf.buffer[input.cur..]) {
                    Ok(s) => {
                        input.valid_up_to = input.cur + s.len();
                        s.chars().next()?
                    }
                    Err(e) if e.valid_up_to() > 0 => {
                        input.valid_up_to = input.cur + e.valid_up_to();
                        let s = unsafe {
                            // # Safety
                            // Refer to the documents for `from_utf8_unchecked` and `Utf8Error`.
                            from_utf8_unchecked(&buf.buffer[input.cur..][..e.valid_up_to()])
                        };
                        s.chars().next().unwrap()
                    }
                    Err(e) => {
                        return match e.error_len() {
                            Some(_) => {
                                // If we detect an UTF8 error that probably mean that the
                                // input encoding didn't get properly advertised in the
                                // declaration header. Report the error and switch the encoding
                                // to ISO-Latin-1 (if you don't like this policy, just declare the encoding !)
                                if input.remainder_len() < 4 {
                                    __xml_err_encoding!(
                                        self,
                                        XmlParserErrors::XmlErrInvalidChar,
                                        "Input is not proper UTF-8, indicate encoding !\n"
                                    );
                                } else {
                                    let buffer = format!(
                                        "Bytes: 0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}\n",
                                        buf.buffer[input.cur],
                                        buf.buffer[input.cur + 1],
                                        buf.buffer[input.cur + 2],
                                        buf.buffer[input.cur + 3],
                                    );
                                    __xml_err_encoding!(
                                        self,
                                        XmlParserErrors::XmlErrInvalidChar,
                                        "Input is not proper UTF-8, indicate encoding !\n{}",
                                        buffer
                                    );
                                }
                                self.input_mut()
                                    .unwrap()
                                    .buf
                                    .as_mut()
                                    .unwrap()
                                    .fallback_to_iso_8859_1();
                                self.charset = XmlCharEncoding::ISO8859_1;
                                self.current_char(len)
                            }
                            None => Some('\0'),
                        };
                    }
                }
            }
        } else if let Some(content) = input
            .entity
            .as_deref()
            .and_then(|ent| ent.content.as_deref())
        {
            content[input.cur..].chars().next()?
        } else {
            return None;
        };
        *len = c.len_utf8() as i32;

        if (*len > 1 && !xml_is_char(c as u32))
            || (*len == 1 && c == '\0' && !self.content_bytes().is_empty())
        {
            xml_err_encoding_int!(
                self,
                XmlParserErrors::XmlErrInvalidChar,
                "Char 0x{:X} out of allowed range\n",
                c as i32
            );
        }
        if c == '\r' {
            if self.content_bytes().get(1) == Some(&b'\n') {
                let input = self.input_mut().unwrap();
                input.cur += 1;
            }
            return Some('\n');
        }
        Some(c)
    }

    pub(super) fn consume_char_if(
        &mut self,
        mut f: impl FnMut(&Self, char) -> bool,
    ) -> Option<char> {
        let mut len = 0;
        let c = self.current_char(&mut len)?;
        f(self, c).then(|| {
            let input = self.input_mut().unwrap();
            if c == '\n' {
                input.line += 1;
                input.col = 1;
            } else {
                input.col += 1;
            }
            input.cur += c.len_utf8();
            c
        })
    }

    // Lookup the namespace name for the @prefix (which ca be NULL)
    //
    // Returns the namespace name or NULL if not bound
    #[doc(alias = "xmlGetNamespace")]
    pub(crate) fn get_namespace(&self, prefix: Option<&str>) -> Option<&str> {
        if prefix == self.str_xml.as_deref() {
            return self.str_xml_ns.as_deref();
        }
        for (pre, href) in self.ns_tab.iter().rev() {
            if pre.as_deref() == prefix {
                if prefix.is_none() && href.is_empty() {
                    return None;
                }
                return Some(href.as_str());
            }
        }
        None
    }

    /// Pushes a new parser input on top of the input stack
    ///
    /// Returns -1 in case of error, the index in the stack otherwise
    #[doc(alias = "inputPush")]
    pub fn input_push(&mut self, value: XmlParserInput<'a>) -> i32 {
        self.input_tab.push(value);
        self.input_tab.len() as i32 - 1
    }

    /// Pops the top parser input from the input stack
    ///
    /// Returns the input just removed
    #[doc(alias = "inputPop")]
    pub fn input_pop(&mut self) -> Option<XmlParserInput<'a>> {
        self.input_tab.pop()
    }

    /// Pushes a new element node on top of the node stack
    ///
    /// Returns -1 in case of error, the index in the stack otherwise
    #[doc(alias = "nodePush")]
    pub(crate) fn node_push(&mut self, value: XmlNodePtr) -> i32 {
        if self.node_tab.len() as u32 > XML_PARSER_MAX_DEPTH
            && self.options & XmlParserOption::XmlParseHuge as i32 == 0
        {
            let max_depth = XML_PARSER_MAX_DEPTH as i32;
            xml_fatal_err_msg_int!(
                self,
                XmlParserErrors::XmlErrInternalError,
                format!("Excessive depth in document: {max_depth} use XML_PARSE_HUGE option\n")
                    .as_str(),
                max_depth
            );
            self.halt();
            return -1;
        }
        self.node = Some(value);
        self.node_tab.push(value);
        self.node_tab.len() as i32 - 1
    }

    /// Pops the top element node from the node stack
    ///
    /// Returns the node just removed
    #[doc(alias = "nodePop")]
    pub(crate) fn node_pop(&mut self) -> Option<XmlNodePtr> {
        let res = self.node_tab.pop();
        self.node = self.node_tab.last().cloned();
        res
    }

    // /// Pushes a new element name on top of the name stack
    // ///
    // /// Returns -1 in case of error, the index in the stack otherwise
    // #[doc(alias = "namePush")]
    // pub(crate) fn name_push(&mut self, value: &str) -> i32 {
    //     self.name = Some(value.to_owned());
    //     self.name_tab.push(value.to_owned());
    //     self.name_tab.len() as i32 - 1
    // }

    /// Pops the top element name from the name stack
    ///
    /// Returns the name just removed
    #[doc(alias = "namePop")]
    pub(crate) fn name_pop(&mut self) -> Option<Rc<str>> {
        let res = self.name_tab.pop();
        let name = self.name_tab.last().cloned();
        self.name = name;
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
        value: &str,
        prefix: Option<&str>,
        uri: Option<&str>,
        line: i32,
        ns_nr: i32,
    ) -> i32 {
        let name: Rc<str> = value.into();
        self.name = Some(name.clone());
        self.name_tab.push(name);
        self.push_tab
            .resize(self.name_tab.len(), XmlStartTag::default());
        let res = self.name_tab.len() - 1;
        self.push_tab[res].prefix = prefix.map(|pre| pre.into());
        self.push_tab[res].uri = uri.map(|uri| uri.into());
        self.push_tab[res].line = line;
        self.push_tab[res].ns_nr = ns_nr;
        res as i32
    }

    /// Pops the top element/prefix/URI name from the name stack
    ///
    /// Returns the name just removed
    #[doc(alias = "nameNsPop")]
    #[cfg(feature = "libxml_push")]
    pub(crate) fn name_ns_pop(&mut self) -> Option<Rc<str>> {
        let res = self.name_tab.pop();
        self.name = self.name_tab.last().cloned();
        res
    }

    /// Pushes a new parser namespace on top of the ns stack
    ///
    /// Returns -1 in case of error, -2 if the namespace should be discarded and the index in the stack otherwise.
    #[doc(alias = "nsPush")]
    pub(crate) fn ns_push(&mut self, prefix: Option<&str>, url: &str) -> i32 {
        if self.options & XmlParserOption::XmlParseNsClean as i32 != 0 {
            for (pre, href) in self.ns_tab.iter().rev() {
                if pre.as_deref() == prefix {
                    // in scope
                    if href.as_str() == url {
                        return -2;
                    }
                    // out of scope keep it
                    break;
                }
            }
        }
        self.ns_tab
            .push((prefix.map(|p| p.to_owned()), url.to_owned()));
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

    /// Match to a new input stream which is stacked on top of the previous one(s).
    ///
    /// Returns -1 in case of error or the index in the input stack
    #[doc(alias = "xmlPushInput")]
    pub fn push_input(&mut self, input: XmlParserInput<'a>) -> i32 {
        if get_parser_debug_entities() != 0 {
            if self.input().is_some() && self.input().unwrap().filename.is_some() {
                generic_error!(
                    "{}({}): ",
                    self.input().unwrap().filename.as_ref().unwrap(),
                    self.input().unwrap().line
                );
            }
            let cur = match from_utf8(&input.base_contents()[input.cur..]) {
                Ok(s) => s,
                Err(e) if e.valid_up_to() > 0 => {
                    unsafe {
                        // # Safety
                        // Refer to the documents for `from_utf8_unchecked` and `Utf8Error`.
                        from_utf8_unchecked(
                            &input.base_contents()[input.cur..input.cur + e.valid_up_to()],
                        )
                    }
                }
                _ => "(Failed to read buffer)",
            };
            generic_error!(
                "Pushing input {} : {}\n",
                self.input_tab.len() + 1,
                &cur[..cur.len().min(30)]
            );
        }
        if (self.input_tab.len() > 40 && self.options & XmlParserOption::XmlParseHuge as i32 == 0)
            || self.input_tab.len() > 100
        {
            xml_fatal_err(self, XmlParserErrors::XmlErrEntityLoop, None);
            while self.input_tab.len() > 1 {
                self.input_pop();
            }
            return -1;
        }
        let ret: i32 = self.input_push(input);
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return -1;
        }
        self.grow();
        ret
    }

    /// The current input pointed by self.input came to an end pop it and return the next c_char.
    ///
    /// Returns the current XmlChar in the parser context
    #[doc(alias = "xmlPopInput")]
    pub fn pop_input(&mut self) -> u8 {
        if self.input_tab.len() <= 1 {
            return 0;
        }
        if get_parser_debug_entities() != 0 {
            generic_error!("Popping input {}\n", self.input_tab.len());
        }
        if self.input_tab.len() > 1
            && self.in_subset == 0
            && !matches!(self.instate, XmlParserInputState::XmlParserEOF)
        {
            xml_fatal_err(
                self,
                XmlParserErrors::XmlErrInternalError,
                Some("Unfinished entity outside the DTD"),
            );
        }
        let input = self.input_pop().unwrap();
        if let Some(mut entity) = input.entity {
            entity.flags &= !XML_ENT_EXPANDING as i32;
        }

        if self.current_byte() == 0 {
            self.force_grow();
        }
        self.current_byte()
    }

    /// Do the SAX2 detection and specific initialization
    #[doc(alias = "xmlDetectSAX2")]
    pub(crate) fn detect_sax2(&mut self) {
        let sax = self.sax.as_deref();
        #[cfg(feature = "sax1")]
        {
            if sax.is_some_and(|sax| {
                sax.initialized == XML_SAX2_MAGIC as u32
                    && (sax.start_element_ns.is_some()
                        || sax.end_element_ns.is_some()
                        || (sax.start_element.is_none() && sax.end_element.is_none()))
            }) {
                self.sax2 = true;
            }
        }
        #[cfg(not(feature = "sax1"))]
        {
            self.sax2 = true;
        }

        self.str_xml = Some(Cow::Borrowed("xml"));
        self.str_xmlns = Some(Cow::Borrowed("xmlns"));
        self.str_xml_ns = Some(Cow::Borrowed(XML_XML_NAMESPACE));
    }

    /// Applies the options to the parser context
    ///
    /// Returns 0 in case of success, the set of unknown or unimplemented options in case of error.
    #[doc(alias = "xmlCtxtUseOptionsInternal")]
    pub(crate) fn use_options_internal(&mut self, mut options: i32, encoding: Option<&str>) -> i32 {
        if let Some(encoding) = encoding {
            self.encoding = Some(encoding.to_owned());
        }
        if options & XmlParserOption::XmlParseRecover as i32 != 0 {
            self.recovery = true;
            options -= XmlParserOption::XmlParseRecover as i32;
            self.options |= XmlParserOption::XmlParseRecover as i32;
        } else {
            self.recovery = false;
        }
        if options & XmlParserOption::XmlParseDTDLoad as i32 != 0 {
            self.loadsubset = XML_DETECT_IDS as i32;
            options -= XmlParserOption::XmlParseDTDLoad as i32;
            self.options |= XmlParserOption::XmlParseDTDLoad as i32;
        } else {
            self.loadsubset = 0;
        }
        if options & XmlParserOption::XmlParseDTDAttr as i32 != 0 {
            self.loadsubset |= XML_COMPLETE_ATTRS as i32;
            options -= XmlParserOption::XmlParseDTDAttr as i32;
            self.options |= XmlParserOption::XmlParseDTDAttr as i32;
        }
        if options & XmlParserOption::XmlParseNoEnt as i32 != 0 {
            self.replace_entities = true;
            // self.loadsubset |= XML_DETECT_IDS;
            options -= XmlParserOption::XmlParseNoEnt as i32;
            self.options |= XmlParserOption::XmlParseNoEnt as i32;
        } else {
            self.replace_entities = false;
        }
        if options & XmlParserOption::XmlParsePedantic as i32 != 0 {
            self.pedantic = true;
            options -= XmlParserOption::XmlParsePedantic as i32;
            self.options |= XmlParserOption::XmlParsePedantic as i32;
        } else {
            self.pedantic = false;
        }
        if options & XmlParserOption::XmlParseNoBlanks as i32 != 0 {
            self.keep_blanks = false;
            if let Some(sax) = self.sax.as_deref_mut() {
                sax.ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
            }
            options -= XmlParserOption::XmlParseNoBlanks as i32;
            self.options |= XmlParserOption::XmlParseNoBlanks as i32;
        } else {
            self.keep_blanks = true;
        }
        if options & XmlParserOption::XmlParseDTDValid as i32 != 0 {
            self.validate = true;
            if options & XmlParserOption::XmlParseNoWarning as i32 != 0 {
                self.vctxt.warning = None;
            }
            if options & XmlParserOption::XmlParseNoError as i32 != 0 {
                self.vctxt.error = None;
            }
            options -= XmlParserOption::XmlParseDTDValid as i32;
            self.options |= XmlParserOption::XmlParseDTDValid as i32;
        } else {
            self.validate = false;
        }
        if options & XmlParserOption::XmlParseNoWarning as i32 != 0 {
            if let Some(sax) = self.sax.as_deref_mut() {
                sax.warning = None;
            }
            options -= XmlParserOption::XmlParseNoWarning as i32;
        }
        if options & XmlParserOption::XmlParseNoError as i32 != 0 {
            if let Some(sax) = self.sax.as_deref_mut() {
                sax.error = None;
                sax.fatal_error = None;
            }
            options -= XmlParserOption::XmlParseNoError as i32;
        }
        #[cfg(feature = "sax1")]
        if options & XmlParserOption::XmlParseSAX1 as i32 != 0 {
            if let Some(sax) = self.sax.as_deref_mut() {
                sax.start_element = Some(xml_sax2_start_element);
                sax.end_element = Some(xml_sax2_end_element);
                sax.start_element_ns = None;
                sax.end_element_ns = None;
                sax.initialized = 1;
            }
            options -= XmlParserOption::XmlParseSAX1 as i32;
            self.options |= XmlParserOption::XmlParseSAX1 as i32;
        }
        if options & XmlParserOption::XmlParseNoCDATA as i32 != 0 {
            if let Some(sax) = self.sax.as_deref_mut() {
                sax.cdata_block = None;
            }
            options -= XmlParserOption::XmlParseNoCDATA as i32;
            self.options |= XmlParserOption::XmlParseNoCDATA as i32;
        }
        if options & XmlParserOption::XmlParseNsClean as i32 != 0 {
            self.options |= XmlParserOption::XmlParseNsClean as i32;
            options -= XmlParserOption::XmlParseNsClean as i32;
        }
        if options & XmlParserOption::XmlParseNoNet as i32 != 0 {
            self.options |= XmlParserOption::XmlParseNoNet as i32;
            options -= XmlParserOption::XmlParseNoNet as i32;
        }
        if options & XmlParserOption::XmlParseCompact as i32 != 0 {
            self.options |= XmlParserOption::XmlParseCompact as i32;
            options -= XmlParserOption::XmlParseCompact as i32;
        }
        if options & XmlParserOption::XmlParseOld10 as i32 != 0 {
            self.options |= XmlParserOption::XmlParseOld10 as i32;
            options -= XmlParserOption::XmlParseOld10 as i32;
        }
        if options & XmlParserOption::XmlParseNoBasefix as i32 != 0 {
            self.options |= XmlParserOption::XmlParseNoBasefix as i32;
            options -= XmlParserOption::XmlParseNoBasefix as i32;
        }
        if options & XmlParserOption::XmlParseHuge as i32 != 0 {
            self.options |= XmlParserOption::XmlParseHuge as i32;
            options -= XmlParserOption::XmlParseHuge as i32;
        }
        if options & XmlParserOption::XmlParseOldSAX as i32 != 0 {
            self.options |= XmlParserOption::XmlParseOldSAX as i32;
            options -= XmlParserOption::XmlParseOldSAX as i32;
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

    /// Applies the options to the parser context
    ///
    /// Returns 0 in case of success, the set of unknown or unimplemented options
    /// in case of error.
    #[doc(alias = "xmlCtxtUseOptions")]
    pub fn use_options(&mut self, options: i32) -> i32 {
        self.use_options_internal(options, None)
    }

    /// Common front-end for the xmlRead functions
    ///
    /// Returns the resulting document tree or NULL
    #[doc(alias = "xmlDoRead")]
    pub(crate) fn do_read(
        &mut self,
        url: Option<&str>,
        encoding: Option<&str>,
        options: i32,
    ) -> Option<XmlDocPtr> {
        self.use_options_internal(options, encoding);
        if let Some(encoding) = encoding {
            // TODO: We should consider to set XML_PARSE_IGNORE_ENC if the
            // caller provided an encoding. Otherwise, we might match to
            // the encoding from the XML declaration which is likely to
            // break things. Also see xmlSwitchInputEncoding.
            if let Some(handler) = find_encoding_handler(encoding) {
                self.switch_to_encoding(handler);
            }
        }
        if url.is_some() {
            if let Some(input) = self.input_mut().filter(|input| input.filename.is_none()) {
                input.filename = url.map(|u| u.to_owned());
            }
        }
        self.parse_document();
        if self.well_formed || self.recovery {
            self.my_doc.take()
        } else {
            if let Some(my_doc) = self.my_doc.take() {
                unsafe {
                    // # Safety
                    // `my_doc` is no longer used and not leaked to the out of this function.
                    // Therefore, this operation is safe.
                    xml_free_doc(my_doc);
                }
            }
            None
        }
    }

    /// change the input functions when discovering the character encoding of a given entity.
    ///
    /// Returns 0 in case of success, -1 otherwise
    #[doc(alias = "xmlSwitchInputEncoding")]
    pub(crate) fn switch_input_encoding(
        &mut self,
        input: &mut XmlParserInput<'_>,
        handler: XmlCharEncodingHandler,
    ) -> i32 {
        if input.buf.as_mut().is_none() {
            xml_err_internal!(self, "static memory buffer doesn't support encoding\n");
            return -1;
        }

        if input
            .buf
            .as_mut()
            .unwrap()
            .encoder
            .replace(handler)
            .is_some()
        {
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
        if input.buf.as_mut().unwrap().buffer.is_empty() {
            return 0;
        }
        // FIXME: The BOM shouldn't be skipped here, but in the parsing code.

        // Specific handling of the Byte Order Mark for UTF-16
        if matches!(
            input.buf.as_mut().unwrap().encoder.as_ref().unwrap().name(),
            "UTF-16LE" | "UTF-16"
        ) && input.base_contents()[input.cur] == 0xFF
            && input.base_contents()[input.cur + 1] == 0xFE
        {
            input.cur += 2;
        }
        if input.buf.as_mut().unwrap().encoder.as_ref().unwrap().name() == "UTF-16BE"
            && input.base_contents()[input.cur] == 0xFE
            && input.base_contents()[input.cur + 1] == 0xFF
        {
            input.cur += 2;
        }
        // Errata on XML-1.0 June 20 2001
        // Specific handling of the Byte Order Mark for UTF-8
        if input.buf.as_mut().unwrap().encoder.as_ref().unwrap().name() == "UTF-8"
            && input.base_contents()[input.cur] == 0xEF
            && input.base_contents()[input.cur + 1] == 0xBB
            && input.base_contents()[input.cur + 2] == 0xBF
        {
            input.cur += 3;
        }

        // Shrink the current input buffer.
        // Move it as the raw buffer and create a new input buffer
        let processed = input.offset_from_base();
        input.buf.as_mut().unwrap().trim_head(processed);
        input.consumed += processed as u64;
        let input_buf = input.buf.as_mut().unwrap();
        let using = input_buf.buffer.len();
        input_buf.raw = take(&mut input_buf.buffer);
        input_buf.rawconsumed = processed as u64;

        // TODO: We must flush and decode the whole buffer to make functions
        // like xmlReadMemory work with a user-provided encoding. If the
        // encoding is specified directly, we should probably set
        // XML_PARSE_IGNORE_ENC in xmlDoRead to avoid switching encodings
        // twice. Then we could set "flush" to false which should save
        // a considerable amount of memory when parsing from memory.
        // It's probably even possible to remove this whole if-block
        // completely.
        let res = input_buf.decode(true);
        input.reset_base();
        if res.is_err() {
            // TODO: This could be an out of memory or an encoding error.
            xml_err_internal!(self, "switching encoding: encoder error\n");
            self.halt();
            return -1;
        }
        let input_buf = input.buf.as_mut().unwrap();
        let consumed = using - input_buf.raw.len();
        let rawconsumed = input_buf.rawconsumed.saturating_add(consumed as u64);
        input_buf.rawconsumed = rawconsumed;
        0
    }

    /// change the input functions when discovering the character encoding
    /// of a given entity.
    ///
    /// Returns 0 in case of success, -1 otherwise
    #[doc(alias = "xmlSwitchToEncoding")]
    pub fn switch_to_encoding(&mut self, handler: XmlCharEncodingHandler) -> i32 {
        let mut input = self.input_pop().unwrap();
        let res = self.switch_input_encoding(&mut input, handler);
        self.input_push(input);
        res
    }

    /// Change the input functions when discovering the character encoding of a given entity.
    ///
    /// Returns 0 in case of success, -1 otherwise
    #[doc(alias = "xmlSwitchEncoding")]
    pub fn switch_encoding(&mut self, enc: XmlCharEncoding) -> i32 {
        // FIXME: The BOM shouldn't be skipped here, but in the parsing code.
        //
        // Note that we look for a decoded UTF-8 BOM when switching to UTF-16.
        // This is mostly useless but Webkit/Chromium relies on this behavior.
        // See https://bugs.chromium.org/p/chromium/issues/detail?id=1451026
        if self
            .input()
            .is_some_and(|input| input.consumed == 0 && input.offset_from_base() == 0)
            && matches!(
                enc,
                XmlCharEncoding::UTF8 | XmlCharEncoding::UTF16LE | XmlCharEncoding::UTF16BE
            )
        {
            // Errata on XML-1.0 June 20 2001
            // Specific handling of the Byte Order Mark for UTF-8
            if self.content_bytes().starts_with(&[0xEF, 0xBB, 0xBF]) {
                self.input_mut().unwrap().cur += 3;
            }
        }

        let Some(handler) = (match enc {
            XmlCharEncoding::Error => {
                __xml_err_encoding!(
                    self,
                    XmlParserErrors::XmlErrUnknownEncoding,
                    "encoding unknown\n"
                );
                return -1;
            }
            XmlCharEncoding::None => {
                // let's assume it's UTF-8 without the XML decl
                self.charset = XmlCharEncoding::UTF8;
                return 0;
            }
            XmlCharEncoding::UTF8 => {
                // default encoding, no conversion should be needed
                self.charset = XmlCharEncoding::UTF8;
                return 0;
            }
            XmlCharEncoding::EBCDIC => self.input().unwrap().detect_ebcdic(),
            _ => get_encoding_handler(enc),
        }) else {
            // Default handlers.
            match enc {
                XmlCharEncoding::ASCII => {
                    // default encoding, no conversion should be needed
                    self.charset = XmlCharEncoding::UTF8;
                    return 0;
                }
                XmlCharEncoding::ISO8859_1 => {
                    if self.input_tab.len() == 1
                        && self.encoding.is_none()
                        && self.input().is_some_and(|input| input.encoding.is_some())
                    {
                        self.encoding = self.input().unwrap().encoding.clone();
                    }
                    self.charset = enc;
                    return 0;
                }
                _ => {
                    let name = enc.get_name().unwrap_or("");
                    __xml_err_encoding!(
                        self,
                        XmlParserErrors::XmlErrUnsupportedEncoding,
                        "encoding not supported: {}\n",
                        name
                    );
                    // TODO: We could recover from errors in external entities
                    // if we didn't stop the parser. But most callers of this
                    // function don't check the return value.
                    self.stop();
                    return -1;
                }
            }
        };
        let mut input = self.input_pop().unwrap();
        let ret: i32 = self.switch_input_encoding(&mut input, handler);
        self.input_push(input);
        if ret < 0 || self.err_no == XmlParserErrors::XmlI18NConvFailed as i32 {
            // on encoding conversion errors, stop the parser
            self.stop();
            self.err_no = XmlParserErrors::XmlI18NConvFailed as i32;
        }
        ret
    }
}

impl Default for XmlParserCtxt<'_> {
    fn default() -> Self {
        Self {
            sax: None,
            user_data: None,
            my_doc: None,
            well_formed: true,
            replace_entities: get_substitute_entities_default_value(),
            version: None,
            encoding: None,
            standalone: 0,
            html: 0,
            input_tab: vec![],
            node: None,
            node_tab: vec![],
            record_info: false,
            node_seq: XmlParserNodeInfoSeq::default(),
            err_no: 0,
            has_external_subset: false,
            has_perefs: false,
            external: 0,
            valid: 0,
            validate: get_do_validity_checking_default_value(),
            vctxt: XmlValidCtxt::default(),
            instate: XmlParserInputState::default(),
            token: 0,
            directory: None,
            name: None,
            name_tab: vec![],
            nb_chars: 0,
            check_index: 0,
            keep_blanks: get_keep_blanks_default_value(),
            disable_sax: false,
            in_subset: 0,
            int_sub_name: None,
            ext_sub_uri: None,
            ext_sub_system: None,
            space_tab: vec![],
            depth: 0,
            charset: XmlCharEncoding::None,
            pedantic: get_pedantic_parser_default_value(),
            _private: null_mut(),
            loadsubset: 0,
            linenumbers: 0,
            #[cfg(feature = "catalog")]
            catalogs: None,
            recovery: false,
            progressive: false,
            atts: vec![],
            str_xml: None,
            str_xml_ns: None,
            str_xmlns: None,
            sax2: false,
            ns_tab: vec![],
            push_tab: vec![],
            atts_default: HashMap::new(),
            atts_special: None,
            ns_well_formed: true,
            options: 0,
            free_elems_nr: 0,
            free_elems: None,
            free_attrs_nr: 0,
            free_attrs: None,
            last_error: XmlError::default(),
            parse_mode: XmlParserMode::default(),
            nbentities: 0,
            sizeentities: 0,
            node_info_tab: vec![],
            input_id: 0,
            sizeentcopy: 0,
            end_check_state: 0,
            nb_errors: 0,
            nb_warnings: 0,
        }
    }
}

impl Drop for XmlParserCtxt<'_> {
    /// Free all the memory used by a parser context. However the parsed
    /// document in self.myDoc is not freed.
    #[doc(alias = "xmlFreeParserCtxt")]
    fn drop(&mut self) {
        unsafe {
            let _ = self.atts_special.take().map(|t| t.into_inner());
            let mut cur = self.free_elems;
            while let Some(now) = cur {
                let next = now.next.map(|node| XmlNodePtr::try_from(node).unwrap());
                now.free();
                cur = next;
            }
            if let Some(attrs) = self.free_attrs.take() {
                let mut cur = Some(attrs);
                while let Some(now) = cur {
                    let next = now.next;
                    now.free();
                    cur = next;
                }
            }
        }
    }
}

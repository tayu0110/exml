use std::{
    borrow::Cow,
    cell::RefCell,
    ffi::{CStr, c_void},
    ptr::{drop_in_place, null, null_mut},
    rc::Rc,
    slice::from_raw_parts,
    str::{from_utf8, from_utf8_unchecked},
    sync::atomic::AtomicPtr,
};

use libc::ptrdiff_t;

use crate::{
    buf::XmlBufRef,
    dict::{XmlDictPtr, xml_dict_create, xml_dict_free, xml_dict_set_limit},
    encoding::{
        XmlCharEncoding, XmlCharEncodingHandler, find_encoding_handler, get_encoding_handler,
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
        globals::{xml_free, xml_malloc},
        parser::{
            XML_COMPLETE_ATTRS, XML_DETECT_IDS, XML_SAX2_MAGIC, XmlDefAttrsPtr,
            XmlParserInputState, XmlParserMode, XmlParserOption, XmlSAXHandler, XmlStartTag,
            xml_init_parser, xml_load_external_entity, xml_parse_document,
        },
        parser_internals::{
            INPUT_CHUNK, LINE_LEN, XML_MAX_DICTIONARY_LIMIT, XML_MAX_LOOKUP_LIMIT,
            XML_PARSER_MAX_DEPTH, XML_VCTXT_USE_PCTXT, xml_parse_pe_reference,
        },
        sax2::{
            xml_sax_version, xml_sax2_end_element, xml_sax2_ignorable_whitespace,
            xml_sax2_start_element,
        },
        valid::XmlValidCtxt,
    },
    parser::{__xml_err_encoding, xml_err_encoding_int, xml_err_internal, xml_fatal_err_msg_int},
    tree::{
        XML_ENT_EXPANDING, XML_ENT_PARSED, XML_XML_NAMESPACE, XmlAttrPtr, XmlAttributeType,
        XmlDocPtr, XmlEntityType, XmlNodePtr, xml_free_doc,
    },
    uri::build_uri,
};

use super::{
    XmlParserInput, XmlParserNodeInfo, XmlParserNodeInfoSeq, parser_entity_check, xml_err_memory,
    xml_fatal_err,
};

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
    // The SAX handler
    pub sax: Option<Box<XmlSAXHandler>>,
    // For SAX interface only, used by DOM build
    pub(crate) user_data: Option<GenericErrorContext>,
    // the document being built
    pub my_doc: Option<XmlDocPtr>,
    // is the document well formed
    pub well_formed: i32,
    // shall we replace entities ?
    pub(crate) replace_entities: i32,
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
    pub input_tab: Vec<XmlParserInput>,

    // Node analysis stack only used for DOM building
    // Current parsed Node
    pub(crate) node: Option<XmlNodePtr>,
    // array of nodes
    pub(crate) node_tab: Vec<XmlNodePtr>,

    // Whether node info should be kept
    pub(crate) record_info: i32,
    // info about each node parsed
    pub(crate) node_seq: XmlParserNodeInfoSeq,

    // error code
    pub err_no: i32,

    // reference and external subset
    pub(crate) has_external_subset: i32,
    // the internal subset has PE refs
    pub(crate) has_perefs: i32,
    // are we parsing an external entity
    pub(crate) external: i32,

    // is the document valid
    pub valid: i32,
    // shall we try to validate ?
    pub(crate) validate: i32,
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
    pub(crate) name: Option<String>,
    // array of nodes
    pub(crate) name_tab: Vec<*const u8>,

    // unused
    nb_chars: i64,
    // used by progressive parsing lookup
    pub(crate) check_index: i64,
    // ugly but ...
    pub(crate) keep_blanks: i32,
    // SAX callbacks are disabled
    pub(crate) disable_sax: i32,
    // Parsing is in int 1/ext 2 subset
    pub in_subset: i32,
    // name of subset
    pub(crate) int_sub_name: Option<String>,
    // URI of external subset
    pub(crate) ext_sub_uri: Option<String>,
    // SYSTEM ID of external subset
    pub(crate) ext_sub_system: Option<String>,

    // xml:space values
    // array of space infos
    pub(crate) space_tab: Vec<i32>,

    // to prevent entity substitution loops
    pub(crate) depth: i32,
    // used to check entities boundaries
    // pub(crate) entity: XmlParserInputPtr,
    // encoding of the in-memory content
    // actually an xmlCharEncoding
    pub charset: XmlCharEncoding,

    // Those two fields are there to
    pub(crate) nodelen: i32,
    // Speed up large node parsing
    pub(crate) nodemem: i32,
    // signal pedantic warnings
    pub(crate) pedantic: i32,
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
    pub(crate) recovery: i32,
    // is this a progressive parsing
    pub(crate) progressive: i32,
    // dictionary for the parser
    pub(crate) dict: XmlDictPtr,
    // array for the attributes callbacks
    pub(crate) atts: Vec<(String, Option<String>)>,
    // the size of the array
    pub(crate) maxatts: i32,
    // use strings from dict to build tree
    pub(crate) docdict: i32,

    // pre-interned strings
    pub(crate) str_xml: Option<Cow<'static, str>>,
    pub(crate) str_xmlns: Option<Cow<'static, str>>,
    pub(crate) str_xml_ns: Option<Cow<'static, str>>,

    // Everything below is used only by the new SAX mode
    // operating in the new SAX mode
    pub(crate) sax2: i32,
    // the array of prefix/namespace name
    pub(crate) ns_tab: Vec<(Option<String>, String)>,
    // which attribute were allocated
    pub(crate) attallocs: *mut i32,
    // array of data for push
    pub(crate) push_tab: Vec<XmlStartTag>,
    // defaulted attributes if any
    pub(crate) atts_default: Option<XmlHashTableRef<'static, XmlDefAttrsPtr>>,
    // non-CDATA attributes if any
    pub(crate) atts_special: Option<XmlHashTableRef<'static, XmlAttributeType>>,
    // is the document XML Namespace okay
    pub(crate) ns_well_formed: i32,
    // Extra options
    pub(crate) options: i32,

    // Those fields are needed only for streaming parsing so far
    // Use dictionary names for the tree
    pub(crate) dict_names: i32,
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

impl XmlParserCtxt {
    pub fn encoding(&self) -> Option<&str> {
        self.encoding.as_deref()
    }

    pub(crate) unsafe fn current_byte(&self) -> u8 {
        unsafe { *self.input().unwrap().cur }
    }

    pub(crate) unsafe fn nth_byte(&self, nth: usize) -> u8 {
        unsafe {
            let ptr = self.input().unwrap().cur.add(nth);
            // debug_assert!(ptr < self.input().unwrap().end);
            *ptr
        }
    }

    pub(crate) fn current_ptr(&self) -> *const u8 {
        self.input().unwrap().cur
    }

    pub(crate) fn base_ptr(&self) -> *const u8 {
        self.input().unwrap().base
    }

    pub fn input(&self) -> Option<&XmlParserInput> {
        self.input_tab.last()
    }

    pub fn input_mut(&mut self) -> Option<&mut XmlParserInput> {
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
    pub unsafe fn byte_consumed(&self) -> i64 {
        unsafe {
            let Some(input) = self.input() else {
                return -1;
            };
            if input.buf.is_some() && input.buf.as_ref().unwrap().borrow().encoder.is_some() {
                let mut unused = 0;
                let mut buf = input.buf.as_ref().unwrap().borrow_mut();
                let handler = buf.encoder.as_mut().unwrap();
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
                    let mut read = 0;
                    while read < input.len() {
                        let Ok((r, w)) = handler.encode(&input[read..], &mut out) else {
                            return -1;
                        };
                        unused += w;
                        read += r;
                    }
                }
                if input.buf.as_ref().unwrap().borrow().rawconsumed < unused as u64 {
                    return -1;
                }
                return (input.buf.as_ref().unwrap().borrow().rawconsumed - unused as u64) as i64;
            }
            input.consumed as i64 + input.offset_from_base() as i64
        }
    }

    #[doc(alias = "xmlParserGrow")]
    pub(crate) unsafe fn force_grow(&mut self) -> i32 {
        unsafe {
            // Don't grow push parser buffer.
            if self.progressive != 0 {
                return 0;
            }

            let input = self.input_mut().unwrap();
            let cur_end: ptrdiff_t = input.remainder_len() as isize;
            let cur_base: ptrdiff_t = input.offset_from_base() as isize;

            let Some(buf) = input.buf.as_mut() else {
                return 0;
            };
            // Don't grow memory buffers.
            if buf.borrow().encoder.is_none() && buf.borrow().context.is_none() {
                return 0;
            }

            if (cur_end > XML_MAX_LOOKUP_LIMIT as isize || cur_base > XML_MAX_LOOKUP_LIMIT as isize)
                && self.options & XmlParserOption::XmlParseHuge as i32 == 0
            {
                let backtrace = std::backtrace::Backtrace::force_capture();
                eprintln!("cur_end: {cur_end}, cur_base: {cur_base}");
                eprintln!("backtrace:\n{backtrace}");
                xml_err_internal!(self, "Huge input lookup");
                self.halt();
                return -1;
            }

            if cur_end >= INPUT_CHUNK as isize {
                return 0;
            }

            let input = self.input_mut().unwrap();
            let ret: i32 = input
                .buf
                .as_mut()
                .unwrap()
                .borrow_mut()
                .grow(INPUT_CHUNK as _);
            input.set_base_and_cursor(0, cur_base as usize);

            // TODO: Get error code from xmlParserInputBufferGrow
            if ret < 0 {
                xml_err_internal!(self, "Growing input buffer");
                self.halt();
            }

            ret
        }
    }

    pub(crate) unsafe fn grow(&mut self) {
        unsafe {
            if self.progressive == 0 && self.input().unwrap().remainder_len() < INPUT_CHUNK {
                self.force_grow();
            }
        }
    }

    #[doc(alias = "xmlParserShrink")]
    pub(crate) unsafe fn force_shrink(&mut self) {
        unsafe {
            let progressive = self.progressive;

            let input = self.input_mut().unwrap();

            // Don't shrink pull parser memory buffers.
            let mut used = input.offset_from_base();
            let Some(buf) = input.buf.as_mut() else {
                return;
            };
            if progressive == 0 && buf.borrow().encoder.is_none() && buf.borrow().context.is_none()
            {
                return;
            }

            // Do not shrink on large buffers whose only a tiny fraction was consumed
            if used > INPUT_CHUNK {
                let res = buf
                    .borrow()
                    .buffer
                    .map_or(0, |mut buf| buf.trim_head(used - LINE_LEN));

                if res > 0 {
                    used -= res;
                    if res > u64::MAX as usize || input.consumed > u64::MAX - res as u64 {
                        input.consumed = u64::MAX;
                    } else {
                        input.consumed += res as u64;
                    }
                }
            }

            input.set_base_and_cursor(0, used);
        }
    }

    pub(crate) unsafe fn shrink(&mut self) {
        unsafe {
            if self.progressive == 0
                && self.input().unwrap().offset_from_base() > 2 * INPUT_CHUNK
                && self.input().unwrap().remainder_len() < 2 * INPUT_CHUNK
            {
                self.force_shrink();
            }
        }
    }

    /// Blocks further parser processing don't override error.
    #[doc(alias = "xmlHaltParser")]
    pub(crate) unsafe fn halt(&mut self) {
        unsafe {
            self.instate = XmlParserInputState::XmlParserEOF;
            self.disable_sax = 1;
            while self.input_tab.len() > 1 {
                self.input_pop();
            }
            if let Some(input) = self.input_mut() {
                // in case there was a specific allocation deallocate before overriding base
                if let Some(free) = input.free.take() {
                    free(input.base as *mut u8);
                }
                if input.buf.is_some() {
                    let _ = input.buf.take();
                }
                input.cur = c"".as_ptr() as _;
                input.length = 0;
                input.base = input.cur;
                input.end = input.cur;
            }
        }
    }

    /// Blocks further parser processing
    #[doc(alias = "xmlStopParser")]
    pub unsafe fn stop(&mut self) {
        unsafe {
            self.halt();
            self.err_no = XmlParserErrors::XmlErrUserStop as i32;
        }
    }

    /// Reset a parser context
    #[doc(alias = "xmlCtxtReset")]
    pub unsafe fn reset(&mut self) {
        unsafe {
            while self.input_pop().is_some() {
                // drop input
            }
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
                xml_free_doc(doc);
            }

            self.standalone = -1;
            self.has_external_subset = 0;
            self.has_perefs = 0;
            self.html = 0;
            self.external = 0;
            self.instate = XmlParserInputState::XmlParserStart;
            self.token = 0;
            self.well_formed = 1;
            self.ns_well_formed = 1;
            self.disable_sax = 0;
            self.valid = 1;
            self.record_info = 0;
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

            if let Some(mut table) = self.atts_default.take().map(|t| t.into_inner()) {
                table.clear_with(|data, _| xml_free(data as _));
            }
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
    }

    pub(crate) unsafe fn advance(&mut self, nth: usize) {
        unsafe {
            let input = self.input_mut().unwrap();
            if input.cur.add(nth) > input.end {
                self.force_grow();
            }
            let input = self.input_mut().unwrap();
            input.cur = input.cur.add(nth);
            input.col += nth as i32;
            assert!(input.cur <= input.end);
        }
    }

    /// Advance the current pointer.  
    /// If `'\n'` is found, line number is also increased.
    pub(crate) unsafe fn advance_with_line_handling(&mut self, nth: usize) {
        unsafe {
            let input = self.input().unwrap();
            if input.cur.add(nth) > input.end {
                self.force_grow();
            }
            let input = self.input_mut().unwrap();
            for _ in 0..nth {
                if *input.cur == b'\n' {
                    input.line += 1;
                    input.col = 1;
                } else {
                    input.col += 1;
                }
                input.cur = input.cur.add(1);
            }
        }
    }

    pub(crate) unsafe fn content_bytes(&self) -> &[u8] {
        unsafe {
            let len = self.input().unwrap().remainder_len();
            from_raw_parts(self.input().unwrap().cur, len)
        }
    }

    /// Skip to the next character.
    #[doc(alias = "xmlNextChar")]
    pub(crate) unsafe fn skip_char(&mut self) {
        unsafe {
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) || self.input().is_none() {
                return;
            }

            let input = self.input().unwrap();
            if input.cur > input.end {
                xml_err_internal!(self, "Parser input data memory error\n");

                self.err_no = XmlParserErrors::XmlErrInternalError as i32;
                self.stop();

                return;
            }

            if input.remainder_len() < INPUT_CHUNK {
                if self.force_grow() < 0 {
                    return;
                }
                let input = self.input().unwrap();
                if input.cur >= input.end {
                    return;
                }
            }

            if self.charset != XmlCharEncoding::UTF8 {
                // Assume it's a fixed length encoding (1) with
                // a compatible encoding for the ASCII set, since
                // XML constructs only use < 128 chars

                let input = self.input_mut().unwrap();
                if *input.cur == b'\n' {
                    input.line += 1;
                    input.col = 1;
                } else {
                    input.col += 1;
                }
                input.cur = input.cur.add(1);
                return;
            }

            // 2.11 End-of-Line Handling
            //   the literal two-character sequence "#xD#xA" or a standalone
            //   literal #xD, an XML processor must pass to the application
            //   the single character #xA.
            let input = self.input_mut().unwrap();
            if *input.cur == b'\n' {
                input.line += 1;
                input.col = 1;
            } else {
                input.col += 1;
            }

            let bytes = self.content_bytes();
            let len = 4.min(bytes.len());
            match from_utf8(&bytes[..len]) {
                Ok(s) => {
                    let c = s.chars().next().unwrap();
                    let input = self.input_mut().unwrap();
                    input.cur = input.cur.add(c.len_utf8());
                    return;
                }
                Err(e) if e.valid_up_to() > 0 => {
                    let s = from_utf8_unchecked(&bytes[..e.valid_up_to()]);
                    let c = s.chars().next().unwrap();
                    let input = self.input_mut().unwrap();
                    input.cur = input.cur.add(c.len_utf8());
                    return;
                }
                Err(_) => {}
            }

            // If we detect an UTF8 error that probably mean that the
            // input encoding didn't get properly advertised in the declaration header.
            // Report the error and switch the encoding
            // to ISO-Latin-1 (if you don't like this policy, just declare the encoding !)
            if self.input().is_none_or(|input| input.remainder_len() < 4) {
                __xml_err_encoding!(
                    self,
                    XmlParserErrors::XmlErrInvalidChar,
                    "Input is not proper UTF-8, indicate encoding !\n"
                );
            } else {
                let input = self.input().unwrap();
                let buffer = format!(
                    "Bytes: 0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}\n",
                    *input.cur.add(0),
                    *input.cur.add(1),
                    *input.cur.add(2),
                    *input.cur.add(3),
                );
                __xml_err_encoding!(
                    self,
                    XmlParserErrors::XmlErrInvalidChar,
                    "Input is not proper UTF-8, indicate encoding !\n{}",
                    buffer
                );
            }
            self.charset = XmlCharEncoding::ISO8859_1;
            let input = self.input_mut().unwrap();
            input.cur = input.cur.add(1);
        }
    }

    /// skip all blanks character found at that point in the input streams.  
    /// It pops up finished entities in the process if allowable at that point.
    ///
    /// Returns the number of space chars skipped
    #[doc(alias = "xmlSkipBlankChars")]
    pub(crate) unsafe fn skip_blanks(&mut self) -> i32 {
        unsafe {
            let mut res = 0i32;

            // It's Okay to use CUR/NEXT here since all the blanks are on the ASCII range.
            if (self.input_tab.len() == 1
                && !matches!(self.instate, XmlParserInputState::XmlParserDTD))
                || matches!(self.instate, XmlParserInputState::XmlParserStart)
            {
                // if we are in the document content, go really fast
                let mut cur = self.input().unwrap().cur;
                while xml_is_blank_char(*cur as u32) {
                    let input = self.input_mut().unwrap();
                    if *cur == b'\n' {
                        input.line += 1;
                        input.col = 1;
                    } else {
                        input.col += 1;
                    }
                    cur = cur.add(1);
                    res = res.saturating_add(1);
                    if *cur == 0 {
                        self.input_mut().unwrap().cur = cur;
                        self.force_grow();
                        cur = self.input().unwrap().cur;
                    }
                }
                self.input_mut().unwrap().cur = cur;
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

                        consumed = self.input().unwrap().consumed;
                        consumed = consumed
                            .saturating_add(self.input().unwrap().offset_from_base() as u64);

                        // Add to sizeentities when parsing an external entity for the first time.
                        // Is this `unwrap` OK ????
                        let mut ent = self.input().unwrap().entity.unwrap();
                        if matches!(ent.etype, XmlEntityType::XmlExternalParameterEntity)
                            && ent.flags & XML_ENT_PARSED as i32 == 0
                        {
                            ent.flags |= XML_ENT_PARSED as i32;

                            self.sizeentities = self.sizeentities.saturating_add(consumed);
                        }

                        parser_entity_check(self, consumed);

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
        unsafe {
            let input = self.input()?;
            if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
                return None;
            }

            if input.remainder_len() < INPUT_CHUNK && self.force_grow() < 0 {
                return None;
            }

            let input = self.input()?;
            if *input.cur >= 0x20 && *input.cur <= 0x7F {
                *len = 1;
                return Some(*input.cur as char);
            }

            if self.charset != XmlCharEncoding::UTF8 {
                // Assume it's a fixed length encoding (1) with
                // a compatible encoding for the ASCII set, since
                // XML constructs only use < 128 chars
                *len = 1;
                if *input.cur == 0xD {
                    if *input.cur.add(1) == 0xA {
                        let input = self.input_mut()?;
                        input.cur = input.cur.add(1);
                    }
                    return Some('\u{A}');
                }
                return Some(*input.cur as char);
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
                            if self.input().unwrap().remainder_len() < 4 {
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
                            return Some(*self.input().unwrap().cur as char);
                        }
                        None => {
                            *len = 0;
                            return Some('\0');
                        }
                    }
                }
            };
            if (*len > 1 && !xml_is_char(c as u32))
                || (*len == 1 && c == '\0' && self.input().unwrap().cur < self.input().unwrap().end)
            {
                xml_err_encoding_int!(
                    self,
                    XmlParserErrors::XmlErrInvalidChar,
                    "Char 0x{:X} out of allowed range\n",
                    c as i32
                );
            }
            if c == '\r' {
                let input = self.input_mut().unwrap();
                let next = input.cur.add(1);
                if next < input.end && *next == b'\n' {
                    input.cur = input.cur.add(1);
                }
                return Some('\n');
            }
            Some(c)
        }
    }

    pub(super) unsafe fn consume_char_if(
        &mut self,
        mut f: impl FnMut(&Self, char) -> bool,
    ) -> Option<char> {
        unsafe {
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
                input.cur = input.cur.add(c.len_utf8());
                c
            })
        }
    }

    /// Pushes a new parser input on top of the input stack
    ///
    /// Returns -1 in case of error, the index in the stack otherwise
    #[doc(alias = "inputPush")]
    pub fn input_push(&mut self, value: XmlParserInput) -> i32 {
        self.input_tab.push(value);
        self.input_tab.len() as i32 - 1
    }

    /// Pops the top parser input from the input stack
    ///
    /// Returns the input just removed
    #[doc(alias = "inputPop")]
    pub fn input_pop(&mut self) -> Option<XmlParserInput> {
        self.input_tab.pop()
    }

    /// Pushes a new element node on top of the node stack
    ///
    /// Returns -1 in case of error, the index in the stack otherwise
    #[doc(alias = "nodePush")]
    pub(crate) unsafe fn node_push(&mut self, value: XmlNodePtr) -> i32 {
        unsafe {
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

    /// Pushes a new element name on top of the name stack
    ///
    /// Returns -1 in case of error, the index in the stack otherwise
    #[doc(alias = "namePush")]
    pub(crate) fn name_push(&mut self, value: *const u8) -> i32 {
        self.name = (!value.is_null()).then(|| unsafe {
            CStr::from_ptr(value as *const i8)
                .to_string_lossy()
                .into_owned()
        });
        self.name_tab.push(value);
        self.name_tab.len() as i32 - 1
    }

    /// Pops the top element name from the name stack
    ///
    /// Returns the name just removed
    #[doc(alias = "namePop")]
    pub(crate) fn name_pop(&mut self) -> *const u8 {
        let res = self.name_tab.pop().unwrap_or(null_mut());
        let name = *self.name_tab.last().unwrap_or(&null());
        self.name = (!name.is_null()).then(|| unsafe {
            CStr::from_ptr(name as *const i8)
                .to_string_lossy()
                .into_owned()
        });
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
        value: *const u8,
        prefix: *const u8,
        uri: *const u8,
        line: i32,
        ns_nr: i32,
    ) -> i32 {
        self.name = (!value.is_null()).then(|| unsafe {
            CStr::from_ptr(value as *const i8)
                .to_string_lossy()
                .into_owned()
        });
        self.name_tab.push(value);
        self.push_tab
            .resize(self.name_tab.len(), XmlStartTag::default());
        let res = self.name_tab.len() - 1;
        self.push_tab[res].prefix = (!prefix.is_null()).then(|| unsafe {
            CStr::from_ptr(prefix as *const i8)
                .to_string_lossy()
                .into_owned()
        });
        self.push_tab[res].uri = (!uri.is_null()).then(|| unsafe {
            CStr::from_ptr(uri as *const i8)
                .to_string_lossy()
                .into_owned()
        });
        self.push_tab[res].line = line;
        self.push_tab[res].ns_nr = ns_nr;
        res as i32
    }

    /// Pops the top element/prefix/URI name from the name stack
    ///
    /// Returns the name just removed
    #[doc(alias = "nameNsPop")]
    #[cfg(feature = "libxml_push")]
    pub(crate) fn name_ns_pop(&mut self) -> *const u8 {
        let res = self.name_tab.pop().unwrap_or(null_mut());
        let name = *self.name_tab.last().unwrap_or(&null());
        self.name = (!name.is_null()).then(|| unsafe {
            CStr::from_ptr(name as *const i8)
                .to_string_lossy()
                .into_owned()
        });
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
    pub unsafe fn push_input(&mut self, input: XmlParserInput) -> i32 {
        unsafe {
            if get_parser_debug_entities() != 0 {
                if self.input().is_some() && self.input().unwrap().filename.is_some() {
                    generic_error!(
                        "{}({}): ",
                        self.input().unwrap().filename.as_ref().unwrap(),
                        self.input().unwrap().line
                    );
                }
                let cur = CStr::from_ptr(input.cur as *const i8).to_string_lossy();
                generic_error!(
                    "Pushing input {} : {}\n",
                    self.input_tab.len() + 1,
                    &cur[..cur.len().min(30)]
                );
            }
            if (self.input_tab.len() > 40
                && self.options & XmlParserOption::XmlParseHuge as i32 == 0)
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
    }

    /// The current input pointed by self.input came to an end pop it and return the next c_char.
    ///
    /// Returns the current XmlChar in the parser context
    #[doc(alias = "xmlPopInput")]
    pub unsafe fn pop_input(&mut self) -> u8 {
        unsafe {
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

            if *self.input().unwrap().cur == 0 {
                self.force_grow();
            }
            self.current_byte()
        }
    }

    /// Do the SAX2 detection and specific initialization
    #[doc(alias = "xmlDetectSAX2")]
    pub(crate) unsafe fn detect_sax2(&mut self) {
        let sax = self.sax.as_deref();
        #[cfg(feature = "sax1")]
        {
            if sax.is_some_and(|sax| {
                sax.initialized == XML_SAX2_MAGIC as u32
                    && (sax.start_element_ns.is_some()
                        || sax.end_element_ns.is_some()
                        || (sax.start_element.is_none() && sax.end_element.is_none()))
            }) {
                self.sax2 = 1;
            }
        }
        #[cfg(not(feature = "sax1"))]
        {
            self.sax2 = 1;
        }

        self.str_xml = Some(Cow::Borrowed("xml"));
        self.str_xmlns = Some(Cow::Borrowed("xmlns"));
        self.str_xml_ns = Some(Cow::Borrowed(XML_XML_NAMESPACE.to_str().unwrap()));
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
            self.replace_entities = 1;
            // self.loadsubset |= XML_DETECT_IDS;
            options -= XmlParserOption::XmlParseNoEnt as i32;
            self.options |= XmlParserOption::XmlParseNoEnt as i32;
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
        if options & XmlParserOption::XmlParseNoBlanks as i32 != 0 {
            self.keep_blanks = 0;
            if let Some(sax) = self.sax.as_deref_mut() {
                sax.ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
            }
            options -= XmlParserOption::XmlParseNoBlanks as i32;
            self.options |= XmlParserOption::XmlParseNoBlanks as i32;
        } else {
            self.keep_blanks = 1;
        }
        if options & XmlParserOption::XmlParseDTDValid as i32 != 0 {
            self.validate = 1;
            if options & XmlParserOption::XmlParseNoWarning as i32 != 0 {
                self.vctxt.warning = None;
            }
            if options & XmlParserOption::XmlParseNoError as i32 != 0 {
                self.vctxt.error = None;
            }
            options -= XmlParserOption::XmlParseDTDValid as i32;
            self.options |= XmlParserOption::XmlParseDTDValid as i32;
        } else {
            self.validate = 0;
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
        if options & XmlParserOption::XmlParseNoDict as i32 != 0 {
            self.dict_names = 0;
            options -= XmlParserOption::XmlParseNoDict as i32;
            self.options |= XmlParserOption::XmlParseNoDict as i32;
        } else {
            self.dict_names = 1;
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
            if !self.dict.is_null() {
                xml_dict_set_limit(self.dict, 0);
            }
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

    /// Common front-end for the xmlRead functions
    ///
    /// Returns the resulting document tree or NULL
    #[doc(alias = "xmlDoRead")]
    pub(crate) unsafe fn do_read(
        &mut self,
        url: Option<&str>,
        encoding: Option<&str>,
        options: i32,
    ) -> Option<XmlDocPtr> {
        unsafe {
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
            if url.is_some() {
                if let Some(input) = self.input_mut().filter(|input| input.filename.is_none()) {
                    input.filename = url.map(|u| u.to_owned());
                }
            }
            xml_parse_document(self);
            if self.well_formed != 0 || self.recovery != 0 {
                self.my_doc.take()
            } else {
                if let Some(my_doc) = self.my_doc.take() {
                    xml_free_doc(my_doc);
                }
                None
            }
        }
    }

    /// change the input functions when discovering the character encoding of a given entity.
    ///
    /// Returns 0 in case of success, -1 otherwise
    #[doc(alias = "xmlSwitchInputEncoding")]
    pub(crate) unsafe fn switch_input_encoding(
        &mut self,
        input: &mut XmlParserInput,
        handler: XmlCharEncodingHandler,
    ) -> i32 {
        unsafe {
            let Some(input_buf) = input.buf.as_mut() else {
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
            ) && *input.cur.add(0) == 0xFF
                && *input.cur.add(1) == 0xFE
            {
                input.cur = input.cur.add(2);
            }
            if (*input_buf).borrow().encoder.as_ref().unwrap().name() == "UTF-16BE"
                && *input.cur.add(0) == 0xFE
                && *input.cur.add(1) == 0xFF
            {
                input.cur = input.cur.add(2);
            }
            // Errata on XML-1.0 June 20 2001
            // Specific handling of the Byte Order Mark for UTF-8
            if (*input_buf).borrow().encoder.as_ref().unwrap().name() == "UTF-8"
                && *input.cur.add(0) == 0xEF
                && *input.cur.add(1) == 0xBB
                && *input.cur.add(2) == 0xBF
            {
                input.cur = input.cur.add(3);
            }

            // Shrink the current input buffer.
            // Move it as the raw buffer and create a new input buffer
            let processed = input.offset_from_base();
            buf.trim_head(processed);
            input.consumed += processed as u64;
            let input_buf = input.buf.as_mut().unwrap();
            input_buf.borrow_mut().raw = Some(buf);
            input_buf.borrow_mut().buffer = XmlBufRef::new();
            assert!(input_buf.borrow_mut().buffer.is_some());
            input_buf.borrow_mut().rawconsumed = processed as u64;
            let using = buf.len();

            // TODO: We must flush and decode the whole buffer to make functions
            // like xmlReadMemory work with a user-provided encoding. If the
            // encoding is specified directly, we should probably set
            // XML_PARSE_IGNORE_ENC in xmlDoRead to avoid switching encodings
            // twice. Then we could set "flush" to false which should save
            // a considerable amount of memory when parsing from memory.
            // It's probably even possible to remove this whole if-block
            // completely.
            let res = input_buf.borrow_mut().decode(true);
            input.reset_base();
            if res.is_err() {
                // TODO: This could be an out of memory or an encoding error.
                xml_err_internal!(self, "switching encoding: encoder error\n");
                self.halt();
                return -1;
            }
            let input_buf = input.buf.as_mut().unwrap();
            let consumed = using - (*input_buf).borrow().raw.map_or(0, |raw| raw.len());
            let rawconsumed = (*input_buf)
                .borrow()
                .rawconsumed
                .saturating_add(consumed as u64);
            input_buf.borrow_mut().rawconsumed = rawconsumed;
            0
        }
    }

    /// change the input functions when discovering the character encoding
    /// of a given entity.
    ///
    /// Returns 0 in case of success, -1 otherwise
    #[doc(alias = "xmlSwitchToEncoding")]
    pub unsafe fn switch_to_encoding(&mut self, handler: XmlCharEncodingHandler) -> i32 {
        unsafe {
            let mut input = self.input_pop().unwrap();
            let res = self.switch_input_encoding(&mut input, handler);
            self.input_push(input);
            res
        }
    }

    /// Change the input functions when discovering the character encoding of a given entity.
    ///
    /// Returns 0 in case of success, -1 otherwise
    #[doc(alias = "xmlSwitchEncoding")]
    pub unsafe fn switch_encoding(&mut self, enc: XmlCharEncoding) -> i32 {
        unsafe {
            // FIXME: The BOM shouldn't be skipped here, but in the parsing code.
            //
            // Note that we look for a decoded UTF-8 BOM when switching to UTF-16.
            // This is mostly useless but Webkit/Chromium relies on this behavior.
            // See https://bugs.chromium.org/p/chromium/issues/detail?id=1451026
            if self.input().is_some()
                && self.input().unwrap().consumed == 0
                && !self.input().unwrap().cur.is_null()
                && self.input().unwrap().offset_from_base() == 0
                && matches!(
                    enc,
                    XmlCharEncoding::UTF8 | XmlCharEncoding::UTF16LE | XmlCharEncoding::UTF16BE
                )
            {
                // Errata on XML-1.0 June 20 2001
                // Specific handling of the Byte Order Mark for UTF-8
                if *self.input().unwrap().cur.add(0) == 0xEF
                    && *self.input().unwrap().cur.add(1) == 0xBB
                    && *self.input().unwrap().cur.add(2) == 0xBF
                {
                    self.input_mut().unwrap().cur = self.input().unwrap().cur.add(3);
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
                            && self.input().is_some()
                            && self.input().unwrap().encoding.is_some()
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
}

impl Default for XmlParserCtxt {
    fn default() -> Self {
        Self {
            sax: None,
            user_data: None,
            my_doc: None,
            well_formed: 0,
            replace_entities: 0,
            version: None,
            encoding: None,
            standalone: 0,
            html: 0,
            input_tab: vec![],
            node: None,
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
            name: None,
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
            // entity: null_mut(),
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
            str_xml: None,
            str_xml_ns: None,
            str_xmlns: None,
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

/// Allocate and initialize a new parser context.
///
/// Returns the xmlParserCtxtPtr or NULL
#[doc(alias = "xmlNewParserCtxt")]
pub unsafe fn xml_new_parser_ctxt() -> XmlParserCtxtPtr {
    unsafe { xml_new_sax_parser_ctxt(None, None).unwrap_or(null_mut()) }
}

/// Initialize a SAX parser context
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "xmlInitSAXParserCtxt")]
unsafe fn xml_init_sax_parser_ctxt(
    ctxt: XmlParserCtxtPtr,
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
) -> Result<(), Option<Box<XmlSAXHandler>>> {
    unsafe {
        if ctxt.is_null() {
            xml_err_internal!(null_mut(), "Got NULL parser context\n");
            return Err(sax);
        }

        xml_init_parser();

        if (*ctxt).dict.is_null() {
            (*ctxt).dict = xml_dict_create();
        }
        if (*ctxt).dict.is_null() {
            xml_err_memory(null_mut(), Some("cannot initialize parser context\n"));
            return Err(sax);
        }
        xml_dict_set_limit((*ctxt).dict, XML_MAX_DICTIONARY_LIMIT);

        if let Some(mut sax) = sax {
            if sax.initialized != XML_SAX2_MAGIC as u32 {
                // These fields won't used in SAX1 handling.
                sax._private = AtomicPtr::new(null_mut());
                sax.start_element_ns = None;
                sax.end_element_ns = None;
                sax.serror = None;
            }
            (*ctxt).sax = Some(sax);
            (*ctxt).user_data = if user_data.is_some() {
                user_data
            } else {
                Some(GenericErrorContext::new(ctxt))
            };
        } else {
            let mut sax = XmlSAXHandler::default();
            xml_sax_version(&mut sax, 2);
            (*ctxt).sax = Some(Box::new(sax));
            (*ctxt).user_data = Some(GenericErrorContext::new(ctxt));
        }

        (*ctxt).maxatts = 0;
        (*ctxt).atts = vec![];
        // Allocate the Input stack
        (*ctxt).input_tab.shrink_to(5);
        while (*ctxt).input_pop().is_some() {
            // drop input
        }
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

        // Allocate the Node stack
        (*ctxt).node_tab.clear();
        (*ctxt).node_tab.shrink_to(10);
        (*ctxt).node = None;

        // Allocate the Name stack
        (*ctxt).name_tab.clear();
        (*ctxt).name_tab.shrink_to(10);
        (*ctxt).name = None;

        // Allocate the space stack
        (*ctxt).space_tab.clear();
        (*ctxt).space_tab.shrink_to(10);
        (*ctxt).space_tab.push(-1);

        (*ctxt).my_doc = None;
        (*ctxt).well_formed = 1;
        (*ctxt).ns_well_formed = 1;
        (*ctxt).valid = 1;
        (*ctxt).loadsubset = get_load_ext_dtd_default_value();
        if (*ctxt).loadsubset != 0 {
            (*ctxt).options |= XmlParserOption::XmlParseDTDLoad as i32;
        }
        (*ctxt).validate = get_do_validity_checking_default_value();
        (*ctxt).pedantic = get_pedantic_parser_default_value();
        if (*ctxt).pedantic != 0 {
            (*ctxt).options |= XmlParserOption::XmlParsePedantic as i32;
        }
        (*ctxt).linenumbers = get_line_numbers_default_value();
        (*ctxt).keep_blanks = get_keep_blanks_default_value();
        if (*ctxt).keep_blanks == 0 {
            if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                sax.ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
            }
            (*ctxt).options |= XmlParserOption::XmlParseNoBlanks as i32;
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
            // (*ctxt).vctxt.node_max = 0;
            (*ctxt).vctxt.node_tab.clear();
            (*ctxt).options |= XmlParserOption::XmlParseDTDValid as i32;
        }
        (*ctxt).replace_entities = get_substitute_entities_default_value();
        if (*ctxt).replace_entities != 0 {
            (*ctxt).options |= XmlParserOption::XmlParseNoEnt as i32;
        }
        (*ctxt).record_info = 0;
        (*ctxt).check_index = 0;
        (*ctxt).in_subset = 0;
        (*ctxt).err_no = XmlParserErrors::XmlErrOK as i32;
        (*ctxt).depth = 0;
        (*ctxt).charset = XmlCharEncoding::UTF8;
        #[cfg(feature = "catalog")]
        {
            (*ctxt).catalogs = None;
        }
        (*ctxt).sizeentities = 0;
        (*ctxt).sizeentcopy = 0;
        (*ctxt).input_id = 1;
        (*ctxt).node_seq.clear();
        Ok(())
    }
}

/// Allocate and initialize a new SAX parser context.   
/// If userData is NULL, the parser context will be passed as user data.
///
/// Returns the xmlParserCtxtPtr or NULL if memory allocation failed.
#[doc(alias = "xmlNewSAXParserCtxt")]
pub unsafe fn xml_new_sax_parser_ctxt(
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
) -> Result<XmlParserCtxtPtr, Option<Box<XmlSAXHandler>>> {
    unsafe {
        let ctxt: XmlParserCtxtPtr = xml_malloc(size_of::<XmlParserCtxt>()) as XmlParserCtxtPtr;
        if ctxt.is_null() {
            xml_err_memory(null_mut(), Some("cannot allocate parser context\n"));
            return Err(sax);
        }
        std::ptr::write(&mut *ctxt, XmlParserCtxt::default());
        if let Err(sax) = xml_init_sax_parser_ctxt(ctxt, sax, user_data) {
            xml_free_parser_ctxt(ctxt);
            return Err(sax);
        }
        Ok(ctxt)
    }
}

/// Initialize a parser context
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "xmlInitParserCtxt")]
pub(crate) unsafe fn xml_init_parser_ctxt(ctxt: XmlParserCtxtPtr) -> i32 {
    unsafe {
        match xml_init_sax_parser_ctxt(ctxt, None, None) {
            Ok(_) => 0,
            Err(_) => -1,
        }
    }
}

/// Clear (release owned resources) and reinitialize a parser context
#[doc(alias = "xmlClearParserCtxt")]
pub unsafe fn xml_clear_parser_ctxt(ctxt: XmlParserCtxtPtr) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        (*ctxt).node_seq.clear();
        (*ctxt).reset();
    }
}

/// Free all the memory used by a parser context. However the parsed
/// document in (*ctxt).myDoc is not freed.
#[doc(alias = "xmlFreeParserCtxt")]
pub unsafe fn xml_free_parser_ctxt(ctxt: XmlParserCtxtPtr) {
    unsafe {
        if ctxt.is_null() {
            return;
        }

        while (*ctxt).input_pop().is_some() {
            // drop input
        }
        (*ctxt).space_tab.clear();
        (*ctxt).name_tab.clear();
        (*ctxt).node_tab.clear();
        (*ctxt).node_info_tab.clear();
        (*ctxt).input_tab.clear();
        (*ctxt).version = None;
        (*ctxt).encoding = None;
        (*ctxt).ext_sub_uri = None;
        (*ctxt).ext_sub_system = None;
        (*ctxt).sax = None;
        (*ctxt).directory = None;
        if !(*ctxt).dict.is_null() {
            xml_dict_free((*ctxt).dict);
        }
        (*ctxt).ns_tab.clear();
        (*ctxt).push_tab.clear();
        if !(*ctxt).attallocs.is_null() {
            xml_free((*ctxt).attallocs as _);
        }
        if let Some(mut table) = (*ctxt).atts_default.take().map(|t| t.into_inner()) {
            table.clear_with(|data, _| xml_free(data as _));
        }
        let _ = (*ctxt).atts_special.take().map(|t| t.into_inner());
        let mut cur = (*ctxt).free_elems;
        while let Some(now) = cur {
            let next = now.next.map(|node| XmlNodePtr::try_from(node).unwrap());
            now.free();
            cur = next;
        }
        if let Some(attrs) = (*ctxt).free_attrs.take() {
            let mut cur = Some(attrs);
            while let Some(now) = cur {
                let next = now.next;
                now.free();
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
}

/// Create a parser context for a file content.
///
/// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
/// by default if found at compile-time.  
/// However, this crate does not support currently.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateFileParserCtxt")]
pub unsafe fn xml_create_file_parser_ctxt(filename: Option<&str>) -> XmlParserCtxtPtr {
    unsafe { xml_create_url_parser_ctxt(filename, 0) }
}

/// Create a parser context for a file or URL content.
///
/// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
/// by default if found at compile-time.  
/// However, this crate does not support currently.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateURLParserCtxt")]
pub unsafe fn xml_create_url_parser_ctxt(filename: Option<&str>, options: i32) -> XmlParserCtxtPtr {
    unsafe {
        let ctxt: XmlParserCtxtPtr = xml_new_parser_ctxt();
        if ctxt.is_null() {
            xml_err_memory(null_mut(), Some("cannot allocate parser context"));
            return null_mut();
        }

        if options != 0 {
            (*ctxt).ctxt_use_options_internal(options, None);
        }
        (*ctxt).linenumbers = 1;

        let Some(input_stream) = xml_load_external_entity(filename, None, ctxt) else {
            xml_free_parser_ctxt(ctxt);
            return null_mut();
        };

        (*ctxt).input_push(input_stream);
        if (*ctxt).directory.is_none() {
            if let Some(filename) = filename {
                if let Some(directory) = xml_parser_get_directory(filename) {
                    (*ctxt).directory = Some(directory.to_string_lossy().into_owned());
                }
            }
        }

        ctxt
    }
}

/// Create a parser context for an XML in-memory document.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateMemoryParserCtxt")]
pub unsafe fn xml_create_memory_parser_ctxt(buffer: Vec<u8>) -> XmlParserCtxtPtr {
    unsafe {
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
pub(crate) unsafe fn xml_create_entity_parser_ctxt_internal(
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
    mut url: Option<&str>,
    id: Option<&str>,
    base: Option<&str>,
    pctx: XmlParserCtxtPtr,
) -> Result<XmlParserCtxtPtr, Option<Box<XmlSAXHandler>>> {
    unsafe {
        let ctxt = xml_new_sax_parser_ctxt(sax, user_data)?;

        if !pctx.is_null() {
            (*ctxt).options = (*pctx).options;
            (*ctxt)._private = (*pctx)._private;
            (*ctxt).input_id = (*pctx).input_id;
        }

        // Don't read from stdin.
        if url == Some("-") {
            url = Some("./-");
        }

        if let Some(uri) = url.zip(base).and_then(|(url, base)| build_uri(url, base)) {
            let Some(input_stream) = xml_load_external_entity(Some(&uri), id, ctxt as _) else {
                let sax = (*ctxt).sax.take();
                xml_free_parser_ctxt(ctxt);
                return Err(sax);
            };
            (*ctxt).input_push(input_stream);

            if (*ctxt).directory.is_none() {
                if let Some(url) = url {
                    if let Some(directory) = xml_parser_get_directory(url) {
                        (*ctxt).directory = Some(directory.to_string_lossy().into_owned());
                    }
                }
            }
        } else {
            let Some(input_stream) = xml_load_external_entity(url, id, ctxt) else {
                let sax = (*ctxt).sax.take();
                xml_free_parser_ctxt(ctxt);
                return Err(sax);
            };
            (*ctxt).input_push(input_stream);

            if (*ctxt).directory.is_none() {
                if let Some(url) = url {
                    if let Some(directory) = xml_parser_get_directory(url) {
                        (*ctxt).directory = Some(directory.to_string_lossy().into_owned());
                    }
                }
            }
        }
        Ok(ctxt)
    }
}

/// Create a parser context for an external entity
///
/// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
/// by default if found at compile-time.  
/// However, this crate does not support currently.
///
/// Returns the new parser context or NULL
#[doc(alias = "xmlCreateEntityParserCtxt")]
pub unsafe fn xml_create_entity_parser_ctxt(
    url: Option<&str>,
    id: Option<&str>,
    base: Option<&str>,
) -> XmlParserCtxtPtr {
    unsafe {
        xml_create_entity_parser_ctxt_internal(None, None, url, id, base, null_mut())
            .unwrap_or(null_mut())
    }
}

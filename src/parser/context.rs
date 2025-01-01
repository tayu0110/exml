use std::{
    borrow::Cow,
    cell::RefCell,
    ffi::{c_void, CStr},
    ptr::{null, null_mut},
    rc::Rc,
    slice::from_raw_parts,
    str::{from_utf8, from_utf8_unchecked},
};

use libc::ptrdiff_t;

use crate::{
    buf::XmlBufRef,
    dict::{xml_dict_set_limit, XmlDictPtr},
    encoding::{find_encoding_handler, XmlCharEncoding, XmlCharEncodingHandler},
    error::{XmlError, XmlParserErrors},
    generic_error,
    globals::{get_parser_debug_entities, GenericErrorContext},
    hash::XmlHashTableRef,
    libxml::{
        catalog::XmlCatalogEntry,
        chvalid::{xml_is_blank_char, xml_is_char},
        entities::{XmlEntityType, XML_ENT_EXPANDING, XML_ENT_PARSED},
        parser::{
            xml_parse_document, xml_parser_entity_check, XmlDefAttrsPtr, XmlParserInputState,
            XmlParserMode, XmlParserOption, XmlSAXHandler, XmlStartTag, XML_COMPLETE_ATTRS,
            XML_DETECT_IDS, XML_SAX2_MAGIC,
        },
        parser_internals::{
            xml_free_input_stream, xml_parse_pe_reference, INPUT_CHUNK, LINE_LEN,
            XML_MAX_LOOKUP_LIMIT, XML_PARSER_MAX_DEPTH,
        },
        sax2::{xml_sax2_end_element, xml_sax2_ignorable_whitespace, xml_sax2_start_element},
        valid::XmlValidCtxt,
    },
    parser::{__xml_err_encoding, xml_err_encoding_int, xml_err_internal, xml_fatal_err_msg_int},
    tree::{xml_free_doc, XmlAttrPtr, XmlAttributeType, XmlDocPtr, XmlNodePtr, XML_XML_NAMESPACE},
};

use super::{xml_fatal_err, XmlParserInputPtr, XmlParserNodeInfo, XmlParserNodeInfoSeq};

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
    pub my_doc: XmlDocPtr,
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
    // Current input stream
    pub input: XmlParserInputPtr,
    // stack of inputs
    pub input_tab: Vec<XmlParserInputPtr>,

    // Node analysis stack only used for DOM building
    // Current parsed Node
    pub(crate) node: XmlNodePtr,
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
    pub(crate) entity: XmlParserInputPtr,
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
    pub(crate) free_elems: XmlNodePtr,
    // number of freed attributes nodes
    pub(crate) free_attrs_nr: i32,
    // List of freed attributes nodes
    pub(crate) free_attrs: XmlAttrPtr,

    // the complete error information for the last error.
    pub last_error: XmlError,
    // the parser mode
    pub(crate) parse_mode: XmlParserMode,
    // unused
    pub(crate) nbentities: u64,
    // size of parsed entities
    pub sizeentities: u64,

    // for use by HTML non-recursive parser
    // Current NodeInfo
    // pub(crate) node_info: *mut XmlParserNodeInfo,
    // Depth of the parsing stack
    // pub(crate) node_info_nr: i32,
    // Max depth of the parsing stack
    // pub(crate) node_info_max: i32,
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
        let cur_end: ptrdiff_t = (*input).remainder_len() as isize;
        let cur_base: ptrdiff_t = (*input).offset_from_base() as isize;

        let Some(buf) = (*input).buf.as_mut() else {
            return 0;
        };
        // Don't grow push parser buffer.
        if self.progressive != 0 {
            return 0;
        }
        // Don't grow memory buffers.
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
        if self.progressive == 0 && (*self.input).remainder_len() < INPUT_CHUNK {
            self.force_grow();
        }
    }

    #[doc(alias = "xmlParserShrink")]
    pub(crate) unsafe fn force_shrink(&mut self) {
        let input: XmlParserInputPtr = self.input;

        // Don't shrink pull parser memory buffers.
        let Some(buf) = (*input).buf.as_mut() else {
            return;
        };
        if self.progressive == 0
            && (*buf).borrow().encoder.is_none()
            && (*buf).borrow().context.is_none()
        {
            return;
        }

        let mut used = (*input).offset_from_base();
        // Do not shrink on large buffers whose only a tiny fraction was consumed
        if used > INPUT_CHUNK {
            let res = buf
                .borrow()
                .buffer
                .map_or(0, |mut buf| buf.trim_head(used - LINE_LEN));

            if res > 0 {
                used -= res;
                if res > u64::MAX as usize || (*input).consumed > u64::MAX - res as u64 {
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
            && (*self.input).offset_from_base() > 2 * INPUT_CHUNK
            && (*self.input).remainder_len() < 2 * INPUT_CHUNK
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
                free((*self.input).base as *mut u8);
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
        let len = (*self.input).remainder_len();
        from_raw_parts((*self.input).cur, len)
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

        if (*self.input).remainder_len() < INPUT_CHUNK {
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
        if self.input.is_null() || (*self.input).remainder_len() < 4 {
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
                    consumed = consumed.saturating_add((*self.input).offset_from_base() as u64);

                    // Add to sizeentities when parsing an external entity for the first time.
                    let ent = (*self.input).entity;
                    if matches!((*ent).etype, XmlEntityType::XmlExternalParameterEntity)
                        && (*ent).flags & XML_ENT_PARSED as i32 == 0
                    {
                        (*ent).flags |= XML_ENT_PARSED as i32;

                        self.sizeentities = self.sizeentities.saturating_add(consumed);
                    }

                    xml_parser_entity_check(self, consumed);

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
    #[doc(hidden)]
    #[doc(alias = "xmlCurrentChar")]
    pub unsafe fn current_char(&mut self, len: &mut i32) -> Option<char> {
        if self.input.is_null() {
            return None;
        }
        if matches!(self.instate, XmlParserInputState::XmlParserEOF) {
            return None;
        }

        if (*self.input).remainder_len() < INPUT_CHUNK && self.force_grow() < 0 {
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
                        if (*self.input).remainder_len() < 4 {
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
        if self.options & XmlParserOption::XmlParseNsclean as i32 != 0 {
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
    pub unsafe fn push_input(&mut self, input: XmlParserInputPtr) -> i32 {
        if get_parser_debug_entities() != 0 {
            if !self.input.is_null() && (*self.input).filename.is_some() {
                generic_error!(
                    "{}({}): ",
                    (*self.input).filename.as_ref().unwrap(),
                    (*self.input).line
                );
            }
            let cur = CStr::from_ptr((*input).cur as *const i8).to_string_lossy();
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
                xml_free_input_stream(self.input_pop());
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
    pub unsafe fn pop_input(&mut self) -> u8 {
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
        let input: XmlParserInputPtr = self.input_pop();
        if !(*input).entity.is_null() {
            (*(*input).entity).flags &= !XML_ENT_EXPANDING as i32;
        }
        xml_free_input_stream(input);
        if *(*self.input).cur == 0 {
            self.force_grow();
        }
        self.current_byte()
    }

    /// Do the SAX2 detection and specific initialization
    #[doc(alias = "xmlDetectSAX2")]
    pub(crate) unsafe fn detect_sax2(&mut self) {
        let sax = self.sax.as_deref();
        #[cfg(feature = "sax1")]
        {
            if sax.map_or(false, |sax| {
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
            // self.loadsubset |= XML_DETECT_IDS;
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
            if let Some(sax) = self.sax.as_deref_mut() {
                sax.ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
            }
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
            if let Some(sax) = self.sax.as_deref_mut() {
                sax.warning = None;
            }
            options -= XmlParserOption::XmlParseNowarning as i32;
        }
        if options & XmlParserOption::XmlParseNoerror as i32 != 0 {
            if let Some(sax) = self.sax.as_deref_mut() {
                sax.error = None;
                sax.fatal_error = None;
            }
            options -= XmlParserOption::XmlParseNoerror as i32;
        }
        #[cfg(feature = "sax1")]
        if options & XmlParserOption::XmlParseSax1 as i32 != 0 {
            if let Some(sax) = self.sax.as_deref_mut() {
                sax.start_element = Some(xml_sax2_start_element);
                sax.end_element = Some(xml_sax2_end_element);
                sax.start_element_ns = None;
                sax.end_element_ns = None;
                sax.initialized = 1;
            }
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
            if let Some(sax) = self.sax.as_deref_mut() {
                sax.cdata_block = None;
            }
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
    pub(crate) unsafe fn do_read(
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
        let processed = (*input).offset_from_base();
        buf.trim_head(processed);
        (*input).consumed += processed as u64;
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
        (*input).reset_base();
        if res.is_err() {
            // TODO: This could be an out of memory or an encoding error.
            xml_err_internal!(self, "switching encoding: encoder error\n");
            self.halt();
            return -1;
        }
        let consumed = using - (*input_buf).borrow().raw.map_or(0, |raw| raw.len());
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
            sax: None,
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
            free_elems: null_mut(),
            free_attrs_nr: 0,
            free_attrs: null_mut(),
            last_error: XmlError::default(),
            parse_mode: XmlParserMode::default(),
            nbentities: 0,
            sizeentities: 0,
            // node_info: null_mut(),
            // node_info_nr: 0,
            // node_info_max: 0,
            node_info_tab: vec![],
            input_id: 0,
            sizeentcopy: 0,
            end_check_state: 0,
            nb_errors: 0,
            nb_warnings: 0,
        }
    }
}

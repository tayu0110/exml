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
    cell::RefCell,
    ffi::CStr,
    ops::DerefMut,
    ptr::{null, null_mut},
    rc::Rc,
    slice::from_raw_parts,
    str::{from_utf8, from_utf8_mut},
};

use crate::{
    buf::xml_buf_overflow_error,
    encoding::{
        XmlCharEncoding, XmlCharEncodingHandler, find_encoding_handler, get_encoding_handler,
    },
    generic_error,
    globals::get_parser_debug_entities,
    io::{__xml_loader_err, XmlParserInputBuffer, xml_check_http_input, xml_parser_get_directory},
    libxml::{
        chvalid::xml_is_blank_char,
        parser::{XmlParserInputDeallocate, xml_load_external_entity},
        xmlstring::xml_strlen,
    },
    parser::xml_err_internal,
    tree::{XmlEntityPtr, XmlEntityType},
    uri::canonic_path,
};

use super::{XmlParserCtxt, XmlParserCtxtPtr, xml_err_memory};

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
    // UTF-8 encoded buffer
    pub buf: Option<Rc<RefCell<XmlParserInputBuffer>>>,
    // The file analyzed, if any
    pub filename: Option<String>,
    // the directory/base of the file
    pub(crate) directory: Option<String>,
    // Base of the array to parse
    pub base: *const u8,
    // Current c_char being parsed
    pub cur: *const u8,
    // end of the array to parse
    pub end: *const u8,
    // length if known
    pub(crate) length: i32,
    // Current line
    pub line: i32,
    // Current column
    pub(crate) col: i32,
    // How many xmlChars already consumed
    pub consumed: u64,
    // function to deallocate the base
    pub(crate) free: Option<XmlParserInputDeallocate>,
    // the encoding string for entity
    pub(crate) encoding: Option<String>,
    // the version string for entity
    pub(crate) version: Option<String>,
    // Was that entity marked standalone
    pub(crate) standalone: i32,
    // an unique identifier for the entity
    pub(crate) id: i32,
    // consumed bytes from parents
    pub(crate) parent_consumed: u64,
    // entity, if any
    pub(crate) entity: Option<XmlEntityPtr>,
}

impl XmlParserInput {
    /// Create a new input stream structure.
    ///
    /// Returns the new input stream or NULL
    #[doc(alias = "xmlNewInputStream")]
    pub unsafe fn new(ctxt: Option<&mut XmlParserCtxt>) -> Option<Self> {
        unsafe {
            let mut input = XmlParserInput::default();
            input.line = 1;
            input.col = 1;
            input.standalone = -1;

            // If the context is NULL the id cannot be initialized, but that
            // should not happen while parsing which is the situation where
            // the id is actually needed.
            if let Some(ctxt) = ctxt {
                if input.id == i32::MAX {
                    xml_err_memory(ctxt, Some("Input ID overflow\n"));
                    return None;
                }
                input.id = ctxt.input_id;
                ctxt.input_id += 1;
            }

            Some(input)
        }
    }

    /// Create a new input stream based on a memory buffer.
    ///
    /// Returns the new input stream
    #[doc(alias = "xmlNewStringInputStream")]
    pub unsafe fn from_str(mut ctxt: Option<&mut XmlParserCtxt>, buffer: &str) -> Option<Self> {
        unsafe {
            let ctxt_ptr = ctxt
                .as_deref_mut()
                .map_or(null_mut(), |ctxt| ctxt as *mut XmlParserCtxt);
            if get_parser_debug_entities() != 0 {
                generic_error!("new fixed input: {buffer}\n");
            }
            let Some(buf) = XmlParserInputBuffer::from_memory(
                buffer.as_bytes().to_vec(),
                XmlCharEncoding::None,
            ) else {
                xml_err_memory(ctxt_ptr, None);
                return None;
            };
            let Some(mut input) = XmlParserInput::new(ctxt) else {
                xml_err_memory(ctxt_ptr, Some("couldn't allocate a new input stream\n"));
                return None;
            };
            input.buf = Some(Rc::new(RefCell::new(buf)));
            input.reset_base();
            Some(input)
        }
    }

    /// Create a new input stream structure encapsulating the @input into
    /// a stream suitable for the parser.
    ///
    /// Returns the new input stream or NULL
    #[doc(alias = "xmlNewIOInputStream")]
    pub unsafe fn from_io(
        ctxt: XmlParserCtxtPtr,
        input: Rc<RefCell<XmlParserInputBuffer>>,
        enc: XmlCharEncoding,
    ) -> Option<XmlParserInput> {
        unsafe {
            if get_parser_debug_entities() != 0 {
                generic_error!("new input from I/O\n");
            }
            let mut input_stream = XmlParserInput::new((!ctxt.is_null()).then(|| &mut *ctxt))?;
            input_stream.filename = None;
            input_stream.buf = Some(input);
            input_stream.reset_base();

            if !matches!(enc, XmlCharEncoding::None) {
                (*ctxt).switch_encoding(enc);
            }

            Some(input_stream)
        }
    }

    /// Create a new input stream based on a file or an URL.
    ///
    /// Returns the new input stream or NULL in case of error
    #[doc(alias = "xmlNewInputFromFile")]
    pub unsafe fn from_filename(
        ctxt: &mut XmlParserCtxt,
        filename: &str,
    ) -> Option<XmlParserInput> {
        unsafe {
            if get_parser_debug_entities() != 0 {
                generic_error!("new input from file: {filename}\n");
            }

            let Some(buf) = XmlParserInputBuffer::from_uri(filename, XmlCharEncoding::None) else {
                __xml_loader_err!(ctxt, "failed to load external entity \"{}\"\n", filename);
                return None;
            };

            let mut input_stream = XmlParserInput::new(Some(ctxt))?;
            input_stream.buf = Some(Rc::new(RefCell::new(buf)));
            let mut input_stream = xml_check_http_input(ctxt, Some(input_stream))?;

            let uri = input_stream
                .filename
                .clone()
                .unwrap_or_else(|| filename.to_owned());
            let directory = xml_parser_get_directory(&uri);
            let canonic = canonic_path(&uri);
            input_stream.filename = Some(canonic.into_owned());
            if let Some(directory) = directory.as_deref() {
                input_stream.directory = Some(directory.to_string_lossy().into_owned());
            } else {
                input_stream.directory = None;
            }
            input_stream.reset_base();

            if ctxt.directory.is_none() {
                if let Some(directory) = directory {
                    ctxt.directory = Some(directory.to_string_lossy().into_owned());
                }
            }
            Some(input_stream)
        }
    }

    /// Create a new input stream based on an xmlEntityPtr
    ///
    /// Returns the new input stream or NULL
    #[doc(alias = "xmlNewEntityInputStream")]
    pub(crate) unsafe fn from_entity(
        ctxt: XmlParserCtxtPtr,
        mut entity: XmlEntityPtr,
    ) -> Option<XmlParserInput> {
        unsafe {
            if get_parser_debug_entities() != 0 {
                generic_error!(
                    "new input from entity: {}\n",
                    CStr::from_ptr(entity.name as *const i8).to_string_lossy()
                );
            }
            if entity.content.is_null() {
                match entity.etype {
                    XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                        let name = CStr::from_ptr(entity.name as *const i8).to_string_lossy();
                        xml_err_internal!(ctxt, "Cannot parse entity {}\n", name);
                    }
                    XmlEntityType::XmlExternalGeneralParsedEntity
                    | XmlEntityType::XmlExternalParameterEntity => {
                        let mut input = xml_load_external_entity(
                            entity.uri.as_deref(),
                            entity.external_id.as_deref(),
                            ctxt,
                        );
                        if let Some(input) = input.as_mut() {
                            input.entity = Some(entity);
                        }
                        return input;
                    }
                    XmlEntityType::XmlInternalGeneralEntity => {
                        let name = CStr::from_ptr(entity.name as *const i8).to_string_lossy();
                        xml_err_internal!(ctxt, "Internal entity {} without content !\n", name);
                    }
                    XmlEntityType::XmlInternalParameterEntity => {
                        let name = CStr::from_ptr(entity.name as *const i8).to_string_lossy();
                        xml_err_internal!(
                            ctxt,
                            "Internal parameter entity {} without content !\n",
                            name
                        );
                    }
                    XmlEntityType::XmlInternalPredefinedEntity => {
                        let name = CStr::from_ptr(entity.name as *const i8).to_string_lossy();
                        xml_err_internal!(ctxt, "Predefined entity {} without content !\n", name);
                    }
                }
                return None;
            }
            let mut input = XmlParserInput::new((!ctxt.is_null()).then(|| &mut *ctxt))?;

            if let Some(uri) = entity.uri.as_deref() {
                input.filename = Some(uri.to_owned());
            }
            input.base = entity.content as _;
            if entity.length == 0 {
                entity.length = xml_strlen(entity.content as _);
            }
            input.cur = entity.content;
            input.length = entity.length;
            input.end = entity.content.add(input.length as usize);
            input.entity = Some(entity);
            Some(input)
        }
    }

    /// Return the offset of `cur` from `base`.
    pub(crate) unsafe fn offset_from_base(&self) -> usize {
        unsafe { self.cur.offset_from(self.base) as usize }
    }

    /// Return the length of remainder buffer length.  
    /// In other word, the offset of `end` from `cur`.
    pub(crate) unsafe fn remainder_len(&self) -> usize {
        unsafe { self.end.offset_from(self.cur) as usize }
    }

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

    // /// This function removes used input for the parser.
    // #[doc(alias = "xmlParserInputShrink")]
    // pub(crate) unsafe fn shrink(&mut self) {
    //     unsafe {
    //         if self.buf.is_none() {
    //             return;
    //         }
    //         if self.base.is_null() {
    //             return;
    //         }
    //         if self.cur.is_null() {
    //             return;
    //         }
    //         let Some(mut buf) = self.buf.as_ref().unwrap().borrow().buffer else {
    //             return;
    //         };

    //         let mut used = self.offset_from_base();
    //         // Do not shrink on large buffers whose only a tiny fraction was consumed
    //         if used > INPUT_CHUNK {
    //             let ret = buf.trim_head(used - LINE_LEN);
    //             if ret > 0 {
    //                 used -= ret;
    //                 if ret as u64 > u64::MAX || self.consumed > u64::MAX - ret as u64 {
    //                     self.consumed = u64::MAX;
    //                 } else {
    //                     self.consumed += ret as u64;
    //                 }
    //             }
    //         }

    //         if buf.len() <= INPUT_CHUNK {
    //             self.buf
    //                 .as_mut()
    //                 .unwrap()
    //                 .borrow_mut()
    //                 .read(2 * INPUT_CHUNK as i32);
    //         }

    //         self.base = buf.as_ref().as_ptr();
    //         if self.base.is_null() {
    //             // TODO: raise error
    //             self.base = c"".as_ptr() as _;
    //             self.cur = self.base;
    //             self.end = self.base;
    //             return;
    //         }
    //         self.cur = self.base.add(used);
    //         self.end = buf.as_ref().as_ptr().add(buf.len());
    //     }
    // }

    #[doc(alias = "xmlDetectEBCDIC")]
    pub(crate) unsafe fn detect_ebcdic(&self) -> Option<XmlCharEncodingHandler> {
        unsafe {
            let mut out: [u8; 200] = [0; 200];

            // To detect the EBCDIC code page, we convert the first 200 bytes
            // to EBCDIC-US and try to find the encoding declaration.
            let mut handler = get_encoding_handler(XmlCharEncoding::EBCDIC)?;
            let inlen = self.end.offset_from(self.cur) as usize;
            let outstr = from_utf8_mut(&mut out).ok()?;
            let Ok((_, outlen)) = handler.decode(from_raw_parts(self.cur, inlen), outstr) else {
                return Some(handler);
            };
            out[outlen] = 0;

            let mut i: usize = 0;
            while i < outlen {
                if out[i] == b'>' {
                    break;
                }
                if out[i] == b'e' && out[i..].starts_with(b"encoding") {
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
                    return from_utf8(&out[start..i])
                        .ok()
                        .and_then(find_encoding_handler);
                }

                i += 1;
            }

            Some(handler)
        }
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
            entity: None,
        }
    }
}

impl Drop for XmlParserInput {
    /// Free up an input stream.
    #[doc(alias = "xmlFreeInputStream")]
    fn drop(&mut self) {
        unsafe {
            if !self.base.is_null() {
                if let Some(free) = self.free {
                    free(self.base as _);
                }
            }
        }
    }
}

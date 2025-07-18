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

use std::str::{from_utf8, from_utf8_mut};

use crate::{
    chvalid::XmlCharValid,
    encoding::{
        XmlCharEncoding, XmlCharEncodingHandler, find_encoding_handler, get_encoding_handler,
    },
    generic_error,
    globals::get_parser_debug_entities,
    io::{__xml_loader_err, XmlParserInputBuffer, xml_check_http_input, xml_parser_get_directory},
    parser::xml_err_internal,
    tree::{XmlEntityPtr, XmlEntityType},
    uri::canonic_path,
};

use super::{
    INPUT_CHUNK, LINE_LEN, XmlParserCtxt, XmlParserCtxtPtr, xml_err_memory,
    xml_load_external_entity,
};

/// The parser is now working also as a state based parser.
/// The recursive one use the state info for entities processing.
#[doc(alias = "xmlParserInputState")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlParserInputState {
    XmlParserEOF = -1, // nothing is to be parsed
    #[default]
    XmlParserStart = 0, // nothing has been parsed
    XmlParserMisc,     // Misc* before int subset
    XmlParserPI,       // Within a processing instruction
    XmlParserDTD,      // within some DTD content
    XmlParserProlog,   // Misc* after internal subset
    XmlParserComment,  // within a comment
    XmlParserStartTag, // within a start tag
    XmlParserContent,  // within the content
    XmlParserCDATASection, // within a CDATA section
    XmlParserEndTag,   // within a closing tag
    XmlParserEntityDecl, // within an entity declaration
    XmlParserEntityValue, // within an entity value in a decl
    XmlParserAttributeValue, // within an attribute value
    XmlParserSystemLiteral, // within a SYSTEM value
    XmlParserEpilog,   // the Misc* after the last end tag
    XmlParserIgnore,   // within an IGNORED section
    XmlParserPublicLiteral, // within a PUBLIC value
}

/// An xmlParserInput is an input flow for the XML processor.
/// Each entity parsed is associated an xmlParserInput (except the
/// few predefined ones). This is the case both for internal entities
/// - in which case the flow is already completely in memory  
/// - or external entities  
/// - in which case we use the buf structure for
///   progressive reading and I18N conversions to the internal UTF-8 format.
#[doc(alias = "xmlParserInput")]
#[repr(C)]
#[derive(Default)]
pub struct XmlParserInput<'a> {
    // UTF-8 encoded buffer
    pub buf: Option<XmlParserInputBuffer<'a>>,
    // The file analyzed, if any
    pub filename: Option<String>,
    // the directory/base of the file
    pub(crate) directory: Option<String>,
    // Base of the array to parse
    pub base: usize,
    // Current char being parsed
    pub cur: usize,
    // length if known
    pub(crate) length: i32,
    // Current line
    pub line: i32,
    // Current column
    pub(crate) col: i32,
    // How many xmlChars already consumed
    pub consumed: u64,
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
    // Indicates how far `buf.buffer` is valid as UTF-8.
    // If `buf.encoder` is `Some`, `buf.buffer` is always valid, so this is always 0.
    pub(crate) valid_up_to: usize,
}

impl<'a> XmlParserInput<'a> {
    /// Create a new input stream structure.
    ///
    /// Returns the new input stream or NULL
    #[doc(alias = "xmlNewInputStream")]
    pub fn new(ctxt: Option<&mut XmlParserCtxt>) -> Option<Self> {
        let mut input = XmlParserInput {
            line: 1,
            col: 1,
            standalone: -1,
            ..Default::default()
        };

        // If the context is NULL the id cannot be initialized, but that
        // should not happen while parsing which is the situation where
        // the id is actually needed.
        if let Some(ctxt) = ctxt {
            if input.id == i32::MAX {
                xml_err_memory(Some(ctxt), Some("Input ID overflow\n"));
                return None;
            }
            input.id = ctxt.input_id;
            ctxt.input_id += 1;
        }

        Some(input)
    }

    /// Create a new input stream based on a memory buffer.
    ///
    /// Returns the new input stream
    #[doc(alias = "xmlNewStringInputStream")]
    pub fn from_str(ctxt: Option<&mut XmlParserCtxt>, buffer: &'a str) -> Option<Self> {
        if get_parser_debug_entities() != 0 {
            generic_error!("new fixed input: {buffer}\n");
        }
        let Some(buf) = XmlParserInputBuffer::from_memory(buffer.as_bytes(), XmlCharEncoding::None)
        else {
            xml_err_memory(ctxt, None);
            return None;
        };
        let Some(mut input) = XmlParserInput::new(ctxt) else {
            xml_err_memory(None, Some("couldn't allocate a new input stream\n"));
            return None;
        };
        input.buf = Some(buf);
        input.reset_base();
        Some(input)
    }

    /// Create a new input stream structure encapsulating the @input into
    /// a stream suitable for the parser.
    ///
    /// Returns the new input stream or NULL
    #[doc(alias = "xmlNewIOInputStream")]
    pub fn from_io(
        ctxt: &mut XmlParserCtxt,
        input: XmlParserInputBuffer<'a>,
        enc: XmlCharEncoding,
    ) -> Option<XmlParserInput<'a>> {
        if get_parser_debug_entities() != 0 {
            generic_error!("new input from I/O\n");
        }
        let mut input_stream = XmlParserInput::new(Some(ctxt))?;
        input_stream.filename = None;
        input_stream.buf = Some(input);
        input_stream.reset_base();

        if !matches!(enc, XmlCharEncoding::None) {
            ctxt.switch_encoding(enc);
        }

        Some(input_stream)
    }

    /// Create a new input stream based on a file or an URL.
    ///
    /// Returns the new input stream or NULL in case of error
    #[doc(alias = "xmlNewInputFromFile")]
    pub fn from_filename(ctxt: &mut XmlParserCtxt, filename: &str) -> Option<XmlParserInput<'a>> {
        if get_parser_debug_entities() != 0 {
            generic_error!("new input from file: {filename}\n");
        }

        let Some(buf) = XmlParserInputBuffer::from_uri(filename, XmlCharEncoding::None) else {
            __xml_loader_err!(ctxt, "failed to load external entity \"{}\"\n", filename);
            return None;
        };

        let mut input_stream = XmlParserInput::new(Some(ctxt))?;
        input_stream.buf = Some(buf);
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

    /// Create a new input stream based on an xmlEntityPtr
    ///
    /// Returns the new input stream or NULL
    #[doc(alias = "xmlNewEntityInputStream")]
    pub(crate) fn from_entity(ctxt: &mut XmlParserCtxt, mut entity: XmlEntityPtr) -> Option<Self> {
        if get_parser_debug_entities() != 0 {
            generic_error!("new input from entity: {}\n", entity.name);
        }
        let Some(content) = entity.content.as_deref() else {
            match entity.etype {
                XmlEntityType::XmlExternalGeneralUnparsedEntity => {
                    xml_err_internal!(ctxt, "Cannot parse entity {}\n", entity.name.clone());
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
                    xml_err_internal!(
                        ctxt,
                        "Internal entity {} without content !\n",
                        entity.name.clone()
                    );
                }
                XmlEntityType::XmlInternalParameterEntity => {
                    xml_err_internal!(
                        ctxt,
                        "Internal parameter entity {} without content !\n",
                        entity.name.clone()
                    );
                }
                XmlEntityType::XmlInternalPredefinedEntity => {
                    xml_err_internal!(
                        ctxt,
                        "Predefined entity {} without content !\n",
                        entity.name.clone()
                    );
                }
            }
            return None;
        };
        let buf = std::io::Cursor::new(content.as_bytes().to_vec());
        let mut buf = XmlParserInputBuffer::from_reader(buf, XmlCharEncoding::None);
        if ctxt.progressive {
            // If PROGRESSIVE is true, the buffer is not automatically filled,
            // so it is forced to be filled at this point.
            while buf.grow(INPUT_CHUNK) != 0 {}
        }
        let mut input = XmlParserInput::from_io(ctxt, buf, XmlCharEncoding::None)?;

        if let Some(uri) = entity.uri.as_deref() {
            input.filename = Some(uri.to_owned());
        }
        input.base = 0;
        if entity.length == 0 {
            entity.length = content.len() as i32;
        }
        input.cur = 0;
        input.length = entity.length;
        input.entity = Some(entity);
        Some(input)
    }

    pub fn base_contents(&self) -> &[u8] {
        if self.base == usize::MAX {
            return &[];
        }
        &self
            .buf
            .as_ref()
            .map(|buf| buf.buffer.as_ref())
            .unwrap_or(&[][..])[self.base..]
    }

    pub(crate) fn current_contents(&self) -> &[u8] {
        &self.base_contents()[self.cur..]
    }

    /// Return the offset of `cur` from `base`.
    pub(crate) fn offset_from_base(&self) -> usize {
        self.cur
    }

    /// Return the length of remainder buffer length.  
    /// In other word, the offset of `end` from `cur`.
    pub(crate) fn remainder_len(&self) -> usize {
        self.current_contents().len()
    }

    /// Update the input to use the current set of pointers from the buffer.
    ///
    /// Returns `-1` in case of error, `0` otherwise
    #[doc(alias = "xmlBufResetInput")]
    pub(crate) fn reset_base(&mut self) -> i32 {
        self.base = 0;
        self.cur = 0;
        self.valid_up_to = 0;
        0
    }

    /// Reduce buffer capacity.  
    /// The `base` and `cur` are automatically aligned according to the number of bytes removed.
    ///
    /// # Panics
    /// - The `buf` must not be `None`.
    pub(crate) fn shrink(&mut self) {
        if self.cur <= INPUT_CHUNK {
            return;
        }

        let buf = self.buf.as_mut().unwrap();
        let diff = self.cur - LINE_LEN;
        self.consumed = self.consumed.saturating_add(diff as u64);
        buf.buffer.drain(..diff);
        if self.cur < self.valid_up_to {
            self.valid_up_to -= diff;
        } else {
            self.valid_up_to = 0;
        }
        self.base = 0;
        self.cur = LINE_LEN;
    }

    #[doc(alias = "xmlDetectEBCDIC")]
    pub(crate) fn detect_ebcdic(&self) -> Option<XmlCharEncodingHandler> {
        let mut out: [u8; 200] = [0; 200];

        // To detect the EBCDIC code page, we convert the first 200 bytes
        // to EBCDIC-US and try to find the encoding declaration.
        let mut handler = get_encoding_handler(XmlCharEncoding::EBCDIC)?;
        let inlen = self.remainder_len();
        let outstr = from_utf8_mut(&mut out).ok()?;
        let Ok((_, outlen)) = handler.decode(&self.current_contents()[..inlen], outstr) else {
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
                while out[i].is_xml_blank_char() {
                    i += 1;
                }
                i += 1;
                if out[i - 1] != b'=' {
                    break;
                }
                while out[i].is_xml_blank_char() {
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

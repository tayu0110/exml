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
    ops::DerefMut,
    ptr::{null, null_mut},
    rc::Rc,
};

use crate::{
    buf::xml_buf_overflow_error,
    io::XmlParserInputBuffer,
    libxml::{entities::XmlEntityPtr, parser::XmlParserInputDeallocate},
};

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
    pub(crate) entity: XmlEntityPtr,
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

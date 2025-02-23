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
    ops::{Index, IndexMut},
    rc::Rc,
};

use crate::tree::XmlNodePtr;

pub type XmlParserNodeInfoSeqPtr = *mut XmlParserNodeInfoSeq;
#[repr(C)]
#[derive(Clone, Default)]
pub struct XmlParserNodeInfoSeq {
    buffer: Vec<Rc<RefCell<XmlParserNodeInfo>>>,
}

impl XmlParserNodeInfoSeq {
    #[doc(alias = "xmlInitNodeInfoSeq", alias = "xmlClearNodeInfoSeq")]
    pub(crate) fn clear(&mut self) {
        self.buffer.clear();
    }

    pub(crate) fn len(&self) -> usize {
        self.buffer.len()
    }

    pub(crate) fn insert(&mut self, index: usize, info: Rc<RefCell<XmlParserNodeInfo>>) {
        self.buffer.insert(index, info);
    }

    /// Find the index that the info record for the given node is or should be at in a sorted sequence
    ///
    /// Returns a long indicating the position of the record
    #[doc(alias = "xmlParserFindNodeInfoIndex")]
    pub(crate) fn binary_search(&self, node: Option<XmlNodePtr>) -> Result<usize, usize> {
        self.buffer
            .binary_search_by_key(&node, |node| node.borrow().node)
    }
}

impl Index<usize> for XmlParserNodeInfoSeq {
    type Output = Rc<RefCell<XmlParserNodeInfo>>;

    fn index(&self, index: usize) -> &Self::Output {
        self.buffer
            .get(index)
            .expect("Index out of bound for XmlParserNodeInfoSeq.")
    }
}

impl IndexMut<usize> for XmlParserNodeInfoSeq {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        self.buffer
            .get_mut(index)
            .expect("index out of bound for XmlParserNodeInfoSeq.")
    }
}

/// The parser can be asked to collect Node information, i.e. at what
/// place in the file they were detected.
/// NOTE: This is off by default and not very well tested.
#[doc(alias = "xmlParserNodeInfo")]
pub type XmlParserNodeInfoPtr = *mut XmlParserNodeInfo;

#[repr(C)]
#[derive(Clone, Copy, Default)]
pub struct XmlParserNodeInfo {
    pub(crate) node: Option<XmlNodePtr>,
    // Position & line # that text that created the node begins & ends on
    pub(crate) begin_pos: u64,
    pub(crate) begin_line: u64,
    pub(crate) end_pos: u64,
    pub(crate) end_line: u64,
}

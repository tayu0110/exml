use std::{
    cell::RefCell,
    ops::{Index, IndexMut},
    ptr::NonNull,
    rc::Rc,
};

use crate::tree::XmlNode;

pub type XmlParserNodeInfoSeqPtr = *mut XmlParserNodeInfoSeq;
#[repr(C)]
#[derive(Debug, Clone, Default)]
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
    pub(crate) fn binary_search(&self, node: Option<NonNull<XmlNode>>) -> Result<usize, usize> {
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
#[derive(Debug, Clone, Copy, Default)]
pub struct XmlParserNodeInfo {
    pub(crate) node: Option<NonNull<XmlNode>>,
    // Position & line # that text that created the node begins & ends on
    pub(crate) begin_pos: u64,
    pub(crate) begin_line: u64,
    pub(crate) end_pos: u64,
    pub(crate) end_line: u64,
}

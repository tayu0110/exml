use crate::tree::XmlNode;

/// The parser can be asked to collect Node information, i.e. at what
/// place in the file they were detected.
/// NOTE: This is off by default and not very well tested.
#[doc(alias = "xmlParserNodeInfo")]
pub type XmlParserNodeInfoPtr = *mut XmlParserNodeInfo;

#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct XmlParserNodeInfo {
    pub(crate) node: *const XmlNode,
    // Position & line # that text that created the node begins & ends on
    pub(crate) begin_pos: u64,
    pub(crate) begin_line: u64,
    pub(crate) end_pos: u64,
    pub(crate) end_line: u64,
}

mod define;

use std::ffi::CStr;

pub(crate) use define::*;

use crate::{
    libxml::xmlstring::xml_str_equal,
    tree::{NodeCommon, XmlElementType, XmlNodePtr},
};

// The Relax-NG namespace
pub(crate) const XML_RELAXNG_NS: &CStr = c"http://relaxng.org/ns/structure/1.0";

pub(crate) unsafe fn is_relaxng(node: XmlNodePtr, typ: *const u8) -> bool {
    !node.is_null()
        && !(*node).ns.is_null()
        && (*node).element_type() == XmlElementType::XmlElementNode
        && xml_str_equal((*node).name, typ)
        && xml_str_equal((*(*node).ns).href, XML_RELAXNG_NS.as_ptr() as _)
}

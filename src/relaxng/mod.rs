mod define;
mod parser;

pub(crate) use define::*;
pub use parser::*;

use crate::tree::{NodeCommon, XmlElementType, XmlNodePtr};

// The Relax-NG namespace
pub(crate) const XML_RELAXNG_NS: &str = "http://relaxng.org/ns/structure/1.0";

pub(crate) unsafe fn is_relaxng(node: XmlNodePtr, typ: &str) -> bool {
    !node.is_null()
        && !(*node).ns.is_null()
        && (*node).element_type() == XmlElementType::XmlElementNode
        && (*node).name().as_deref() == Some(typ)
        && (*(*node).ns).href().as_deref() == Some(XML_RELAXNG_NS)
}

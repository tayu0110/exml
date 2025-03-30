mod define;
mod error;
mod parser;
mod type_library;
mod valid;

pub(crate) use define::*;
pub(crate) use error::*;
pub use parser::*;
pub use type_library::*;
pub use valid::*;

use crate::{
    libxml::chvalid::xml_is_blank_char,
    tree::{NodeCommon, XmlElementType, XmlNodePtr},
};

// The Relax-NG namespace
pub(crate) const XML_RELAXNG_NS: &str = "http://relaxng.org/ns/structure/1.0";

pub(crate) fn is_relaxng(node: XmlNodePtr, typ: &str) -> bool {
    node.element_type() == XmlElementType::XmlElementNode
        && node.name().as_deref() == Some(typ)
        && node
            .ns
            .is_some_and(|ns| ns.href().as_deref() == Some(XML_RELAXNG_NS))
}

/// Removes the leading and ending spaces of the value.
#[doc(alias = "xmlRelaxNGNormExtSpace")]
pub(crate) fn normalize_external_space(value: &str) -> &str {
    value
        .trim_start_matches(|c| xml_is_blank_char(c as u32))
        .trim_end_matches(|c| xml_is_blank_char(c as u32))
}

//! Provide internal methods and data structures for handling XML entities.  
//! This module is based on `private/entities.h`, `entities.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use crate::libxml::{entities::xmlEncodeEntitiesInternal, tree::XmlDocPtr, xmlstring::XmlChar};

/*
 * Entity flags
 *
 * XML_ENT_PARSED: The entity was parsed and `children` points to the
 * content.
 * XML_ENT_CHECKED: The entity was checked for loops.
 */
pub(crate) const XML_ENT_PARSED: usize = 1 << 0;
pub(crate) const XML_ENT_CHECKED: usize = 1 << 1;
pub(crate) const XML_ENT_EXPANDING: usize = 1 << 2;
pub(crate) const XML_ENT_CHECKED_LT: usize = 1 << 3;
pub(crate) const XML_ENT_CONTAINS_LT: usize = 1 << 4;

/**
 * xmlEncodeAttributeEntities:
 * @doc:  the document containing the string
 * @input:  A string to convert to XML.
 *
 * Do a global encoding of a string, replacing the predefined entities
 * and non ASCII values with their entities and CharRef counterparts for
 * attribute values.
 *
 * Returns A newly allocated string with the substitution done.
 */
pub(crate) unsafe extern "C" fn xmlEncodeAttributeEntities(
    doc: XmlDocPtr,
    input: *const XmlChar,
) -> *mut XmlChar {
    xmlEncodeEntitiesInternal(doc, input, 1)
}

use crate::libxml::xmlstring::XmlChar;

/// A DTD Notation definition.
pub type XmlNotationPtr = *mut XmlNotation;
#[repr(C)]
pub struct XmlNotation {
    pub(crate) name: *const XmlChar,      /* Notation name */
    pub(crate) public_id: *const XmlChar, /* Public identifier, if any */
    pub(crate) system_id: *const XmlChar, /* System identifier, if any */
}

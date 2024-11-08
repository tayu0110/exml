use crate::libxml::xmlstring::XmlChar;

/// List structure used when there is an enumeration in DTDs.
pub type XmlEnumerationPtr = *mut XmlEnumeration;
#[repr(C)]
pub struct XmlEnumeration {
    pub(crate) next: *mut XmlEnumeration, /* next one */
    pub(crate) name: *const XmlChar,      /* Enumeration name */
}

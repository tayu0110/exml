use std::ptr::null_mut;

/// List structure used when there is an enumeration in DTDs.
pub type XmlEnumerationPtr = *mut XmlEnumeration;
#[repr(C)]
pub struct XmlEnumeration {
    pub(crate) next: *mut XmlEnumeration, /* next one */
    pub(crate) name: Option<String>,      /* Enumeration name */
}

impl Default for XmlEnumeration {
    fn default() -> Self {
        Self {
            next: null_mut(),
            name: None,
        }
    }
}

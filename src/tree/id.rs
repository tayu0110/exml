use crate::libxml::xmlstring::XmlChar;

use super::{XmlAttrPtr, XmlDoc};

/// An XML ID instance.
pub type XmlIDPtr = *mut XmlID;
#[repr(C)]
pub struct XmlID {
    pub(crate) next: *mut XmlID,      /* next ID */
    pub(crate) value: *const XmlChar, /* The ID name */
    pub(crate) attr: XmlAttrPtr,      /* The attribute holding it */
    pub(crate) name: *const XmlChar,  /* The attribute if attr is not available */
    pub(crate) lineno: i32,           /* The line number if attr is not available */
    pub(crate) doc: *mut XmlDoc,      /* The document holding the ID */
}

/// An XML IDREF instance.
pub type XmlRefPtr = *mut XmlRef;
#[repr(C)]
pub struct XmlRef {
    pub(crate) next: *mut XmlRef,     /* next Ref */
    pub(crate) value: *const XmlChar, /* The Ref name */
    pub(crate) attr: XmlAttrPtr,      /* The attribute holding it */
    pub(crate) name: *const XmlChar,  /* The attribute if attr is not available */
    pub(crate) lineno: i32,           /* The line number if attr is not available */
}

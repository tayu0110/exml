use std::{os::raw::c_void, sync::atomic::AtomicPtr};

use crate::libxml::xmlstring::XmlChar;

use super::{XmlDoc, XmlNsType};

/// An XML namespace.
/// Note that prefix == NULL is valid, it defines the default namespace
/// within the subtree (until overridden).
///
/// xmlNsType is unified with xmlElementType.
pub type XmlNsPtr = *mut XmlNs;
#[repr(C)]
pub struct XmlNs {
    pub next: AtomicPtr<XmlNs>,             /* next Ns link for this node  */
    pub(crate) typ: Option<XmlNsType>,      /* global or local */
    pub href: AtomicPtr<XmlChar>,           /* URL for the namespace */
    pub prefix: AtomicPtr<XmlChar>,         /* prefix for the namespace */
    pub(crate) _private: AtomicPtr<c_void>, /* application data */
    pub(crate) context: AtomicPtr<XmlDoc>,  /* normally an xmlDoc */
}

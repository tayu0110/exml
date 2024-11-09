use std::os::raw::c_void;

use crate::libxml::xmlstring::XmlChar;

use super::{XmlNodePtr, XmlNsPtr};

/// A function called to acquire namespaces (xmlNs) from the wrapper.
///
/// Returns an xmlNsPtr or NULL in case of an error.
#[doc(alias = "xmlDOMWrapAcquireNsFunction")]
pub type XmlDOMWrapAcquireNsFunction = unsafe extern "C" fn(
    ctxt: XmlDOMWrapCtxtPtr,
    node: XmlNodePtr,
    nsName: *const XmlChar,
    nsPrefix: *const XmlChar,
) -> XmlNsPtr;

/// Context for DOM wrapper-operations.
pub type XmlDOMWrapCtxtPtr = *mut XmlDOMWrapCtxt;
#[repr(C)]
pub struct XmlDOMWrapCtxt {
    pub(super) _private: *mut c_void,
    /// The type of this context, just in case we need specialized
    /// contexts in the future.
    pub(super) typ: i32,
    /// Internal namespace map used for various operations.
    pub(super) namespace_map: *mut c_void,
    /// Use this one to acquire an xmlNsPtr intended for node->ns.  
    /// (Note that this is not intended for elem->nsDef).
    pub(super) get_ns_for_node_func: Option<XmlDOMWrapAcquireNsFunction>,
}

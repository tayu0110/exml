//! Provide methods and data structures for unfinished XLink detection.  
//! This module is based on `libxml/xlink.h`, `xlink.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: unfinished XLink detection module
// Description: unfinished XLink detection module
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// xlink.c : implementation of the hyperlinks detection module
//           This version supports both XML XLinks and HTML simple links
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{
    ffi::CStr,
    os::raw::c_void,
    ptr::null_mut,
    sync::atomic::{AtomicPtr, Ordering},
};

use crate::tree::{XmlDocPtr, XmlElementType, XmlNodePtr, XmlNsPtr};

use super::xmlstring::{xml_str_equal, XmlChar};

// Various defines for the various Link properties.
//
// # Note
// - the link detection layer will try to resolve QName expansion
//   of namespaces. If "foo" is the prefix for "http://foo.com/"
//   then the link detection layer will expand role="foo:myrole"
//   to "http://foo.com/:myrole".
// - the link detection layer will expand URI-References found on
//   href attributes by using the base mechanism if found.
pub type XlinkHRef = *mut XmlChar;
pub type XlinkRole = *mut XmlChar;
pub type XlinkTitle = *mut XmlChar;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XlinkType {
    XlinkTypeNone = 0,
    XlinkTypeSimple,
    XlinkTypeExtended,
    XlinkTypeExtendedSet,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XlinkShow {
    XlinkShowNone = 0,
    XlinkShowNew,
    XlinkShowEmbed,
    XlinkShowReplace,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XlinkActuate {
    XlinkActuateNone = 0,
    XlinkActuateAuto,
    XlinkActuateOnrequest,
}

/// This is the prototype for the link detection routine.
/// It calls the default link detection callbacks upon link detection.
#[doc(alias = "xlinkNodeDetectFunc")]
pub type XlinkNodeDetectFunc = unsafe extern "C" fn(ctx: *mut c_void, node: XmlNodePtr);

/// This is the prototype for a simple link detection callback.
#[doc(alias = "xlinkSimpleLinkFunk")]
pub type XlinkSimpleLinkFunk = unsafe extern "C" fn(
    ctx: *mut c_void,
    node: XmlNodePtr,
    href: *const XmlChar,
    role: *const XmlChar,
    title: *const XmlChar,
);

/// This is the prototype for a extended link detection callback.
#[doc(alias = "xlinkExtendedLinkFunk")]
pub type XlinkExtendedLinkFunk = unsafe extern "C" fn(
    ctx: *mut c_void,
    node: XmlNodePtr,
    nbLocators: i32,
    hrefs: *mut *const XmlChar,
    roles: *mut *const XmlChar,
    nbArcs: i32,
    from: *mut *const XmlChar,
    to: *mut *const XmlChar,
    show: *mut XlinkShow,
    actuate: *mut XlinkActuate,
    nbTitles: i32,
    titles: *mut *const XmlChar,
    langs: *mut *const XmlChar,
);

/// This is the prototype for a extended link set detection callback.
#[doc(alias = "xlinkExtendedLinkSetFunk")]
pub type XlinkExtendedLinkSetFunk = unsafe extern "C" fn(
    ctx: *mut c_void,
    node: XmlNodePtr,
    nbLocators: i32,
    hrefs: *mut *const XmlChar,
    roles: *mut *const XmlChar,
    nbTitles: i32,
    titles: *mut *const XmlChar,
    langs: *mut *const XmlChar,
);

pub type XlinkHandlerPtr = *mut XlinkHandler;
/// This is the structure containing a set of Links detection callbacks.
///
/// There is no default xlink callbacks, if one want to get link
/// recognition activated, those call backs must be provided before parsing.
#[repr(C)]
pub struct XlinkHandler {
    simple: XlinkSimpleLinkFunk,
    extended: XlinkExtendedLinkFunk,
    set: XlinkExtendedLinkSetFunk,
}

static XLINK_DEFAULT_HANDLER: AtomicPtr<XlinkHandler> = AtomicPtr::new(null_mut());
static mut XLINK_DEFAULT_DETECT: Option<XlinkNodeDetectFunc> = None;

/// Get the default xlink detection routine
///
/// Returns the current function or NULL;
#[doc(alias = "xlinkGetDefaultDetect")]
pub unsafe extern "C" fn xlink_get_default_detect() -> Option<XlinkNodeDetectFunc> {
    XLINK_DEFAULT_DETECT
}

/// Set the default xlink detection routine
#[doc(alias = "xlinkSetDefaultDetect")]
pub unsafe extern "C" fn xlink_set_default_detect(func: Option<XlinkNodeDetectFunc>) {
    XLINK_DEFAULT_DETECT = func;
}

/// Get the default xlink handler.
///
/// Returns the current xlinkHandlerPtr value.
#[doc(alias = "xlinkGetDefaultHandler")]
pub unsafe extern "C" fn xlink_get_default_handler() -> XlinkHandlerPtr {
    XLINK_DEFAULT_HANDLER.load(Ordering::Acquire)
}

/// Set the default xlink handlers
#[doc(alias = "xlinkSetDefaultHandler")]
pub unsafe extern "C" fn xlink_set_default_handler(handler: XlinkHandlerPtr) {
    XLINK_DEFAULT_HANDLER.store(handler, Ordering::Release);
}

const XLINK_NAMESPACE: &str = "http://www.w3.org/1999/xlink/namespace/";
const XHTML_NAMESPACE: &CStr = c"http://www.w3.org/1999/xhtml/";

/// Check whether the given node carries the attributes needed
/// to be a link element (or is one of the linking elements issued
/// from the (X)HTML DtDs).
/// This routine don't try to do full checking of the link validity
/// but tries to detect and return the appropriate link type.
///
/// Returns the xlinkType of the node (XLINK_TYPE_NONE if there is no link detected.
#[doc(alias = "xlinkIsLink")]
pub unsafe extern "C" fn xlink_is_link(mut doc: XmlDocPtr, node: XmlNodePtr) -> XlinkType {
    let mut ret: XlinkType = XlinkType::XlinkTypeNone;

    if node.is_null() {
        return XlinkType::XlinkTypeNone;
    }
    if doc.is_null() {
        doc = (*node).doc;
    }
    if !doc.is_null() && (*doc).typ == XmlElementType::XmlHTMLDocumentNode {
        // This is an HTML document.
    } else if !(*node).ns.is_null()
        && xml_str_equal((*(*node).ns).href, XHTML_NAMESPACE.as_ptr() as _)
    {
        // !!!! We really need an IS_XHTML_ELEMENT function from HTMLtree.h @@@
        // This is an XHTML element within an XML document
        // Check whether it's one of the element able to carry links
        // and in that case if it holds the attributes.
    }

    // We don't prevent a-priori having XML Linking constructs on XHTML elements
    if let Some(typ) = (*node).get_ns_prop("type", Some(XLINK_NAMESPACE)) {
        if typ == "simple" {
            ret = XlinkType::XlinkTypeSimple;
        } else if typ == "extended" {
            if let Some(role) = (*node).get_ns_prop("role", Some(XLINK_NAMESPACE)) {
                let xlink: XmlNsPtr = (*node).search_ns(doc, Some(XLINK_NAMESPACE));
                if xlink.is_null() {
                    /* Humm, fallback method */
                    if role == "xlink:external-linkset" {
                        // ret = XlinkType::XlinkTypeExtendedSet;
                    }
                } else {
                    let buf = format!("{}:external-linkset", (*xlink).prefix().unwrap());
                    if role == buf {
                        // ret = XlinkType::XlinkTypeExtendedSet;
                    }
                }
            }
            ret = XlinkType::XlinkTypeExtended;
        }
    }

    ret
}

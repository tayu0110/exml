//! Provide methods and data structures for unfinished XLink detection.  
//! This module is based on `libxml/xlink.h`, `xlink.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, CStr},
    os::raw::c_void,
    ptr::null_mut,
    sync::atomic::{AtomicPtr, Ordering},
};

use libc::snprintf;

use super::{
    globals::xml_free,
    tree::{xmlSearchNs, xml_get_ns_prop, XmlDocPtr, XmlElementType, XmlNodePtr, XmlNsPtr},
    xmlstring::{xml_str_equal, XmlChar},
};

/**
 * Various defines for the various Link properties.
 *
 * NOTE: the link detection layer will try to resolve QName expansion
 *       of namespaces. If "foo" is the prefix for "http://foo.com/"
 *       then the link detection layer will expand role="foo:myrole"
 *       to "http://foo.com/:myrole".
 * NOTE: the link detection layer will expand URI-References found on
 *       href attributes by using the base mechanism if found.
 */
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

/**
 * xlinkNodeDetectFunc:
 * @ctx:  user data pointer
 * @node:  the node to check
 *
 * This is the prototype for the link detection routine.
 * It calls the default link detection callbacks upon link detection.
 */
pub type XlinkNodeDetectFunc = unsafe extern "C" fn(ctx: *mut c_void, node: XmlNodePtr);

/*
 * The link detection module interact with the upper layers using
 * a set of callback registered at parsing time.
 */

/**
 * xlinkSimpleLinkFunk:
 * @ctx:  user data pointer
 * @node:  the node carrying the link
 * @href:  the target of the link
 * @role:  the role string
 * @title:  the link title
 *
 * This is the prototype for a simple link detection callback.
 */
pub type XlinkSimpleLinkFunk = unsafe extern "C" fn(
    ctx: *mut c_void,
    node: XmlNodePtr,
    href: *const XmlChar,
    role: *const XmlChar,
    title: *const XmlChar,
);

/**
 * xlinkExtendedLinkFunk:
 * @ctx:  user data pointer
 * @node:  the node carrying the link
 * @nbLocators: the number of locators detected on the link
 * @hrefs:  pointer to the array of locator hrefs
 * @roles:  pointer to the array of locator roles
 * @nbArcs: the number of arcs detected on the link
 * @from:  pointer to the array of source roles found on the arcs
 * @to:  pointer to the array of target roles found on the arcs
 * @show:  array of values for the show attributes found on the arcs
 * @actuate:  array of values for the actuate attributes found on the arcs
 * @nbTitles: the number of titles detected on the link
 * @title:  array of titles detected on the link
 * @langs:  array of xml:lang values for the titles
 *
 * This is the prototype for a extended link detection callback.
 */
pub type XlinkExtendedLinkFunk = unsafe extern "C" fn(
    ctx: *mut c_void,
    node: XmlNodePtr,
    nbLocators: c_int,
    hrefs: *mut *const XmlChar,
    roles: *mut *const XmlChar,
    nbArcs: c_int,
    from: *mut *const XmlChar,
    to: *mut *const XmlChar,
    show: *mut XlinkShow,
    actuate: *mut XlinkActuate,
    nbTitles: c_int,
    titles: *mut *const XmlChar,
    langs: *mut *const XmlChar,
);

/**
 * xlinkExtendedLinkSetFunk:
 * @ctx:  user data pointer
 * @node:  the node carrying the link
 * @nbLocators: the number of locators detected on the link
 * @hrefs:  pointer to the array of locator hrefs
 * @roles:  pointer to the array of locator roles
 * @nbTitles: the number of titles detected on the link
 * @title:  array of titles detected on the link
 * @langs:  array of xml:lang values for the titles
 *
 * This is the prototype for a extended link set detection callback.
 */
pub type XlinkExtendedLinkSetFunk = unsafe extern "C" fn(
    ctx: *mut c_void,
    node: XmlNodePtr,
    nbLocators: c_int,
    hrefs: *mut *const XmlChar,
    roles: *mut *const XmlChar,
    nbTitles: c_int,
    titles: *mut *const XmlChar,
    langs: *mut *const XmlChar,
);

/**
 * This is the structure containing a set of Links detection callbacks.
 *
 * There is no default xlink callbacks, if one want to get link
 * recognition activated, those call backs must be provided before parsing.
 */
pub type XlinkHandlerPtr = *mut XlinkHandler;
#[repr(C)]
pub struct XlinkHandler {
    simple: XlinkSimpleLinkFunk,
    extended: XlinkExtendedLinkFunk,
    set: XlinkExtendedLinkSetFunk,
}

static XLINK_DEFAULT_HANDLER: AtomicPtr<XlinkHandler> = AtomicPtr::new(null_mut());
static mut XLINK_DEFAULT_DETECT: Option<XlinkNodeDetectFunc> = None;

/*
 * The default detection routine, can be overridden, they call the default
 * detection callbacks.
 */

/**
 * xlinkGetDefaultDetect:
 *
 * Get the default xlink detection routine
 *
 * Returns the current function or NULL;
 */
pub unsafe extern "C" fn xlink_get_default_detect() -> Option<XlinkNodeDetectFunc> {
    XLINK_DEFAULT_DETECT
}

/**
 * xlinkSetDefaultDetect:
 * @func: pointer to the new detection routine.
 *
 * Set the default xlink detection routine
 */
pub unsafe extern "C" fn xlink_set_default_detect(func: Option<XlinkNodeDetectFunc>) {
    XLINK_DEFAULT_DETECT = func;
}

/*
 * Routines to set/get the default handlers.
 */
/**
 * xlinkGetDefaultHandler:
 *
 * Get the default xlink handler.
 *
 * Returns the current xlinkHandlerPtr value.
 */
pub unsafe extern "C" fn xlink_get_default_handler() -> XlinkHandlerPtr {
    XLINK_DEFAULT_HANDLER.load(Ordering::Acquire)
}

/**
 * xlinkSetDefaultHandler:
 * @handler:  the new value for the xlink handler block
 *
 * Set the default xlink handlers
 */
pub unsafe extern "C" fn xlink_set_default_handler(handler: XlinkHandlerPtr) {
    XLINK_DEFAULT_HANDLER.store(handler, Ordering::Release);
}

const XLINK_NAMESPACE: &CStr = c"http://www.w3.org/1999/xlink/namespace/";
const XHTML_NAMESPACE: &CStr = c"http://www.w3.org/1999/xhtml/";

/*
 * Link detection module itself.
 */
/**
 * xlinkIsLink:
 * @doc:  the document containing the node
 * @node:  the node pointer itself
 *
 * Check whether the given node carries the attributes needed
 * to be a link element (or is one of the linking elements issued
 * from the (X)HTML DtDs).
 * This routine don't try to do full checking of the link validity
 * but tries to detect and return the appropriate link type.
 *
 * Returns the xlinkType of the node (XLINK_TYPE_NONE if there is no
 *         link detected.
 */
pub unsafe extern "C" fn xlink_is_link(mut doc: XmlDocPtr, node: XmlNodePtr) -> XlinkType {
    let mut role: *mut XmlChar = null_mut();
    let mut ret: XlinkType = XlinkType::XlinkTypeNone;

    if node.is_null() {
        return XlinkType::XlinkTypeNone;
    }
    if doc.is_null() {
        doc = (*node).doc;
    }
    if !doc.is_null() && (*doc).typ == XmlElementType::XmlHtmlDocumentNode {
        /*
         * This is an HTML document.
         */
    } else if !(*node).ns.is_null()
        && xml_str_equal(
            (*(*node).ns).href.load(Ordering::Relaxed),
            XHTML_NAMESPACE.as_ptr() as _,
        ) != 0
    {
        /*
         * !!!! We really need an IS_XHTML_ELEMENT function from HTMLtree.h @@@
         */
        /*
         * This is an XHTML element within an XML document
         * Check whether it's one of the element able to carry links
         * and in that case if it holds the attributes.
         */
    }

    /*
     * We don't prevent a-priori having XML Linking constructs on
     * XHTML elements
     */
    let typ: *mut XmlChar =
        xml_get_ns_prop(node, c"type".as_ptr() as _, XLINK_NAMESPACE.as_ptr() as _);
    if !typ.is_null() {
        if xml_str_equal(typ, c"simple".as_ptr() as _) != 0 {
            ret = XlinkType::XlinkTypeSimple;
        } else if xml_str_equal(typ, c"extended".as_ptr() as _) != 0 {
            role = xml_get_ns_prop(node, c"role".as_ptr() as _, XLINK_NAMESPACE.as_ptr() as _);
            if !role.is_null() {
                let xlink: XmlNsPtr = xmlSearchNs(doc, node, XLINK_NAMESPACE.as_ptr() as _);
                if xlink.is_null() {
                    /* Humm, fallback method */
                    if xml_str_equal(role, c"xlink:external-linkset".as_ptr() as _) != 0 {
                        ret = XlinkType::XlinkTypeExtendedSet;
                    }
                } else {
                    let mut buf: [XmlChar; 200] = [0; 200];
                    snprintf(
                        buf.as_mut_ptr() as _,
                        buf.len(),
                        c"%s:external-linkset".as_ptr() as _,
                        (*xlink).prefix.load(Ordering::Relaxed) as *const c_char,
                    );
                    buf[buf.len() - 1] = 0;
                    if xml_str_equal(role, buf.as_ptr() as _) != 0 {
                        ret = XlinkType::XlinkTypeExtendedSet;
                    }
                }
            }
            ret = XlinkType::XlinkTypeExtended;
        }
    }

    if !typ.is_null() {
        xml_free(typ as _);
    }
    if !role.is_null() {
        xml_free(role as _);
    }
    ret
}

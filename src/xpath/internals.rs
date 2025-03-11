//! Provide internal methods and data structures for processing XPath.  
//! This module is based on `libxml/xpathInternals.h`, `xpath.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: internal interfaces for XML Path Language implementation
// Description: internal interfaces for XML Path Language implementation
//              used to build new modules on top of XPath like XPointer and XSLT
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// xpath.c: XML Path Language implementation
//          XPath is a language for addressing parts of an XML document,
//          designed to be used by both XSLT and XPointer
//
// Reference: W3C Recommendation 16 November 1999
//     http://www.w3.org/TR/1999/REC-xpath-19991116
// Public reference:
//     http://www.w3.org/TR/xpath
//
// See Copyright for the status of this software
//
// Author: daniel@veillard.com

use std::{
    ffi::{CStr, CString},
    iter::repeat,
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
};

use libc::{INT_MAX, INT_MIN, memset};

#[cfg(feature = "libxml_xptr_locs")]
use crate::libxml::xpointer::{XmlLocationSetPtr, xml_xptr_free_location_set};
use crate::{
    error::{__xml_raise_error, XmlErrorDomain, XmlErrorLevel, XmlParserErrors},
    generic_error,
    hash::XmlHashTableRef,
    libxml::{
        chvalid::xml_is_blank_char,
        globals::{xml_free, xml_malloc, xml_realloc},
        pattern::{
            XmlPatternFlags, XmlPatternPtr, XmlStreamCtxtPtr, xml_free_pattern,
            xml_free_stream_ctxt, xml_pattern_from_root, xml_pattern_get_stream_ctxt,
            xml_pattern_max_depth, xml_pattern_min_depth, xml_pattern_streamable,
            xml_patterncompile, xml_stream_pop, xml_stream_push, xml_stream_push_node,
            xml_stream_wants_any_node,
        },
        valid::xml_get_id,
        xmlstring::{XmlChar, xml_str_equal, xml_strdup, xml_strndup},
    },
    tree::{
        NodeCommon, XML_XML_NAMESPACE, XmlAttrPtr, XmlDocPtr, XmlDtdPtr, XmlElementType,
        XmlGenericNodePtr, XmlNode, XmlNodePtr, XmlNs, XmlNsPtr, xml_build_qname,
    },
    xpath::{
        XML_XPATH_NAN, XmlXPathCompExpr, XmlXPathCompExprPtr, XmlXPathContextPtr, XmlXPathError,
        XmlXPathFuncLookupFunc, XmlXPathObject, XmlXPathObjectPtr, XmlXPathObjectType, XmlXPathOp,
        XmlXPathParserContextPtr, XmlXPathStepOpPtr, XmlXPathVariableLookupFunc,
        xml_xpath_cast_boolean_to_string, xml_xpath_cast_node_set_to_string,
        xml_xpath_cast_node_to_number, xml_xpath_cast_node_to_string,
        xml_xpath_cast_number_to_boolean, xml_xpath_cast_number_to_string,
        xml_xpath_cast_to_boolean, xml_xpath_cast_to_number, xml_xpath_cmp_nodes_ext,
        xml_xpath_free_node_set, xml_xpath_free_object, xml_xpath_free_value_tree,
        xml_xpath_is_inf, xml_xpath_is_nan, xml_xpath_node_set_create,
        xml_xpath_node_set_merge_and_clear, xml_xpath_node_set_merge_and_clear_no_dupls,
        xml_xpath_object_copy,
    },
};

use super::{
    XmlNodeSet, xml_xpath_new_boolean, xml_xpath_new_float, xml_xpath_new_node_set,
    xml_xpath_new_string, xml_xpath_node_set_merge, xml_xpath_wrap_node_set, xml_xpath_wrap_string,
};

// Many of these macros may later turn into functions.
// They shouldn't be used in #ifdef's preprocessor instructions.

/// Raises an error.
#[doc(alias = "xmlXPathSetError")]
macro_rules! xml_xpath_set_error {
    ($ctxt:expr, $err:expr) => {{
        xml_xpatherror($ctxt, $err);
        if !$ctxt.is_null() {
            (*$ctxt).error = $err;
        }
    }};
}

/// Raises an XPATH_INVALID_TYPE error.
#[doc(alias = "xmlXPathSetTypeError")]
macro_rules! xml_xpath_set_type_error {
    ($ctxt:expr) => {
        xml_xpath_set_error!($ctxt, XmlXPathError::XPathInvalidType as i32)
    };
}

/// Macro to return from the function if an XPath error was detected.
#[doc(hidden)]
#[macro_export]
macro_rules! CHECK_ERROR {
    ($ctxt:expr) => {
        if (*$ctxt).error != $crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
            return;
        }
    };
}

/// Macro to return 0 from the function if an XPath error was detected.
macro_rules! CHECK_ERROR0 {
    ($ctxt:expr) => {
        if (*$ctxt).error != XmlXPathError::XPathExpressionOK as i32 {
            return 0;
        }
    };
}

/// Macro to raise an XPath error and return.
#[doc(hidden)]
#[macro_export]
macro_rules! XP_ERROR {
    ($ctxt:expr, $x:expr) => {{
        $crate::xpath::internals::xml_xpath_err($ctxt, $x);
        return;
    }};
}

/// Macro to raise an XPath error and return 0.
macro_rules! XP_ERROR0 {
    ($ctxt:expr, $x:expr) => {{
        xml_xpath_err($ctxt, $x);
        return 0;
    }};
}

/// Macro to check that the value on top of the XPath stack is of a given type.
#[doc(hidden)]
#[macro_export]
macro_rules! CHECK_TYPE {
    ($ctxt:expr, $typeval:expr) => {
        if (*$ctxt).value.is_null() || (*(*$ctxt).value).typ != $typeval {
            $crate::XP_ERROR!($ctxt, $crate::xpath::XmlXPathError::XPathInvalidType as i32)
        }
    };
}

/// Macro to check that the value on top of the XPath stack is of a given type.  
/// Return(0) in case of failure
macro_rules! CHECK_TYPE0 {
    ($ctxt:expr, $typeval:expr) => {
        if (*$ctxt).value.is_null() || (*(*$ctxt).value).typ != $typeval {
            XP_ERROR0!($ctxt, XmlXPathError::XPathInvalidType as i32)
        }
    };
}

/// Macro to check that the number of args passed to an XPath function matches.
#[doc(hidden)]
#[macro_export]
macro_rules! CHECK_ARITY {
    ($ctxt:expr, $nargs:expr, $x:expr) => {
        if $ctxt.is_null() {
            return;
        }
        if $nargs != $x {
            $crate::XP_ERROR!(
                $ctxt,
                $crate::xpath::XmlXPathError::XPathInvalidArity as i32
            );
        }
        if ((*$ctxt).value_tab.len() as i32) < (*$ctxt).value_frame + $x {
            $crate::XP_ERROR!($ctxt, $crate::xpath::XmlXPathError::XPathStackError as i32);
        }
    };
}

/// Macro to try to cast the value on the top of the XPath stack to a string.
macro_rules! CAST_TO_STRING {
    ($ctxt:expr) => {
        if !(*$ctxt).value.is_null() && (*(*$ctxt).value).typ != XmlXPathObjectType::XPathString {
            xml_xpath_string_function($ctxt, 1);
        }
    };
}

/// Macro to try to cast the value on the top of the XPath stack to a number.
macro_rules! CAST_TO_NUMBER {
    ($ctxt:expr) => {
        if !(*$ctxt).value.is_null() && (*(*$ctxt).value).typ != XmlXPathObjectType::XPathNumber {
            xml_xpath_number_function($ctxt, 1);
        }
    };
}

/// Macro to try to cast the value on the top of the XPath stack to a boolean.
macro_rules! CAST_TO_BOOLEAN {
    ($ctxt:expr) => {
        if !(*$ctxt).value.is_null() && (*(*$ctxt).value).typ != XmlXPathObjectType::XPathBoolean {
            xml_xpath_boolean_function($ctxt, 1);
        }
    };
}

pub type XmlPointerListPtr = *mut XmlPointerList;
/// Pointer-list for various purposes.
#[doc(alias = "xsltPointerList")]
#[repr(C)]
pub struct XmlPointerList {
    pub(crate) items: *mut *mut c_void,
    pub(crate) number: i32,
    pub(crate) size: i32,
}

pub type XmlXPathContextCachePtr = *mut XmlXPathContextCache;
#[repr(C)]
pub struct XmlXPathContextCache {
    pub(crate) nodeset_objs: XmlPointerListPtr, /* contains xmlXPathObjectPtr */
    pub(crate) string_objs: XmlPointerListPtr,  /* contains xmlXPathObjectPtr */
    pub(crate) boolean_objs: XmlPointerListPtr, /* contains xmlXPathObjectPtr */
    pub(crate) number_objs: XmlPointerListPtr,  /* contains xmlXPathObjectPtr */
    pub(crate) misc_objs: XmlPointerListPtr,    /* contains xmlXPathObjectPtr */
    pub(crate) max_nodeset: i32,
    pub(crate) max_string: i32,
    pub(crate) max_boolean: i32,
    pub(crate) max_number: i32,
    pub(crate) max_misc: i32,
}

// TODO: Since such a list-handling is used in xmlschemas.c and libxslt
// and here, we should make the functions public.
unsafe fn xml_pointer_list_add_size(
    list: XmlPointerListPtr,
    item: *mut c_void,
    mut initial_size: i32,
) -> i32 {
    unsafe {
        if (*list).size <= (*list).number {
            let new_size: usize;

            if (*list).size == 0 {
                if initial_size <= 0 {
                    initial_size = 1;
                }
                new_size = initial_size as _;
            } else {
                if (*list).size > 50000000 {
                    xml_xpath_err_memory(
                        null_mut(),
                        Some("xmlPointerListAddSize: re-allocating item\n"),
                    );
                    return -1;
                }
                new_size = (*list).size as usize * 2;
            }
            let tmp: *mut *mut c_void =
                xml_realloc((*list).items as _, new_size * size_of::<*mut c_void>())
                    as *mut *mut c_void;
            if tmp.is_null() {
                xml_xpath_err_memory(
                    null_mut(),
                    Some("xmlPointerListAddSize: re-allocating item\n"),
                );
                return -1;
            }
            (*list).items = tmp;
            (*list).size = new_size as _;
        }
        *(*list).items.add((*list).number as usize) = item;
        (*list).number += 1;
        0
    }
}

/// Creates an xsltPointerList structure.
///
/// Returns a xsltPointerList structure or NULL in case of an error.
#[doc(alias = "xsltPointerListCreate")]
unsafe extern "C" fn xml_pointer_list_create(initial_size: i32) -> XmlPointerListPtr {
    unsafe {
        let ret: XmlPointerListPtr = xml_malloc(size_of::<XmlPointerList>()) as _;
        if ret.is_null() {
            xml_xpath_err_memory(null_mut(), Some("xmlPointerListCreate: allocating item\n"));
            return null_mut();
        }
        memset(ret as _, 0, size_of::<XmlPointerList>());
        if initial_size > 0 {
            xml_pointer_list_add_size(ret, null_mut(), initial_size);
            (*ret).number = 0;
        }
        ret
    }
}

macro_rules! XP_CACHE_WANTS {
    ($sl:expr, $n:expr) => {
        $sl.is_null() || (*$sl).number < $n
    };
}

macro_rules! XP_CACHE_ADD {
    ($sl:expr, $obj:expr) => {
        if $sl.is_null() {
            $sl = xml_pointer_list_create(10);
            if $sl.is_null() {
                // Cache is full; free the object.
                if let Some(nodeset) = (*$obj).nodesetval.take() {
                    xml_xpath_free_node_set(Some(nodeset));
                }
                xml_free($obj as _);
                return;
            }
        }
        if xml_pointer_list_add_size($sl, $obj as _, 0) == -1 {
            // Cache is full; free the object.
            if let Some(nodeset) = (*$obj).nodesetval.take() {
                xml_xpath_free_node_set(Some(nodeset));
            }
            xml_free($obj as _);
            return;
        }
    };
}

/// Depending on the state of the cache this frees the given
/// XPath object or stores it in the cache.
#[doc(alias = "xmlXPathReleaseObject")]
pub(crate) unsafe fn xml_xpath_release_object(ctxt: XmlXPathContextPtr, obj: XmlXPathObjectPtr) {
    unsafe {
        if obj.is_null() {
            return;
        }
        if ctxt.is_null() || (*ctxt).cache.is_null() {
            xml_xpath_free_object(obj);
        } else {
            let cache: XmlXPathContextCachePtr = (*ctxt).cache as XmlXPathContextCachePtr;

            'free_obj: {
                'obj_cached: {
                    match (*obj).typ {
                        XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
                            if let Some(nodeset) = (*obj).nodesetval.take() {
                                if (*obj).boolval {
                                    // It looks like the @boolval is used for
                                    // evaluation if this an XSLT Result Tree Fragment.
                                    // TODO: Check if this assumption is correct.
                                    (*obj).typ = XmlXPathObjectType::XPathXSLTTree; /* just for debugging */
                                    xml_xpath_free_value_tree(Some(nodeset));
                                } else if nodeset.node_tab.len() <= 40
                                    && XP_CACHE_WANTS!((*cache).nodeset_objs, (*cache).max_nodeset)
                                {
                                    XP_CACHE_ADD!((*cache).nodeset_objs, obj);
                                    (*obj).nodesetval = Some(nodeset);
                                    break 'obj_cached;
                                } else {
                                    xml_xpath_free_node_set(Some(nodeset));
                                }
                            }
                        }
                        XmlXPathObjectType::XPathString => {
                            let _ = (*obj).stringval.take();

                            if XP_CACHE_WANTS!((*cache).string_objs, (*cache).max_string) {
                                XP_CACHE_ADD!((*cache).string_objs, obj);
                                break 'obj_cached;
                            }
                        }
                        XmlXPathObjectType::XPathBoolean => {
                            if XP_CACHE_WANTS!((*cache).boolean_objs, (*cache).max_boolean) {
                                XP_CACHE_ADD!((*cache).boolean_objs, obj);
                                break 'obj_cached;
                            }
                        }
                        XmlXPathObjectType::XPathNumber => {
                            if XP_CACHE_WANTS!((*cache).number_objs, (*cache).max_number) {
                                XP_CACHE_ADD!((*cache).number_objs, obj);
                                break 'obj_cached;
                            }
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XPathLocationset => {
                            if !(*obj).user.is_null() {
                                xml_xptr_free_location_set((*obj).user as _);
                            }
                            break 'free_obj;
                        }
                        _ => {
                            break 'free_obj;
                        }
                    }

                    // Fallback to adding to the misc-objects slot.
                    if XP_CACHE_WANTS!((*cache).misc_objs, (*cache).max_misc) {
                        XP_CACHE_ADD!((*cache).misc_objs, obj);
                    } else {
                        break 'free_obj;
                    }
                }

                // obj_cached:

                if let Some(mut nodeset) = (*obj).nodesetval.take() {
                    // TODO: Due to those nasty ns-nodes, we need to traverse
                    //  the list and free the ns-nodes.
                    // URGENT TODO: Check if it's actually slowing things down.
                    //  Maybe we shouldn't try to preserve the list.
                    if nodeset.node_tab.len() > 1 {
                        for &node in &nodeset.node_tab {
                            if let Ok(ns) = XmlNsPtr::try_from(node) {
                                xml_xpath_node_set_free_ns(ns);
                            }
                        }
                    } else if let Some(ns) = nodeset
                        .node_tab
                        .first()
                        .and_then(|&node| XmlNsPtr::try_from(node).ok())
                        .filter(|_| nodeset.node_tab.len() == 1)
                    {
                        xml_xpath_node_set_free_ns(ns);
                    }
                    nodeset.node_tab.clear();
                    *obj = XmlXPathObject {
                        nodesetval: Some(nodeset),
                        ..Default::default()
                    };
                } else {
                    std::ptr::write(&mut *obj, XmlXPathObject::default());
                }

                return;
            }

            // free_obj:
            // Cache is full; free the object.
            if let Some(nodeset) = (*obj).nodesetval.take() {
                xml_xpath_free_node_set(Some(nodeset));
            }
            xml_free(obj as _);
        }
    }
}

/// Register an external mechanism to do variable lookup
#[doc(alias = "xmlXPathRegisterVariableLookup")]
pub unsafe fn xml_xpath_register_variable_lookup(
    ctxt: XmlXPathContextPtr,
    f: XmlXPathVariableLookupFunc,
    data: *mut c_void,
) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        (*ctxt).var_lookup_func = Some(f);
        (*ctxt).var_lookup_data = data;
    }
}

/// Registers an external mechanism to do function lookup.
#[doc(alias = "xmlXPathRegisterFuncLookup")]
pub unsafe fn xml_xpath_register_func_lookup(
    ctxt: XmlXPathContextPtr,
    f: XmlXPathFuncLookupFunc,
    func_ctxt: *mut c_void,
) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        (*ctxt).func_lookup_func = Some(f);
        (*ctxt).func_lookup_data = func_ctxt;
    }
}

/// Formats an error message.
#[doc(alias = "xmlXPatherror")]
pub unsafe fn xml_xpatherror(ctxt: XmlXPathParserContextPtr, no: i32) {
    unsafe {
        xml_xpath_err(ctxt, no);
    }
}

// The array xmlXPathErrorMessages corresponds to the enum XmlXPathError
const XML_XPATH_ERROR_MESSAGES: &[&str] = &[
    "Ok\n",
    "Number encoding\n",
    "Unfinished literal\n",
    "Start of literal\n",
    "Expected $ for variable reference\n",
    "Undefined variable\n",
    "Invalid predicate\n",
    "Invalid expression\n",
    "Missing closing curly brace\n",
    "Unregistered function\n",
    "Invalid operand\n",
    "Invalid type\n",
    "Invalid number of arguments\n",
    "Invalid context size\n",
    "Invalid context position\n",
    "Memory allocation error\n",
    "Syntax error\n",
    "Resource error\n",
    "Sub resource error\n",
    "Undefined namespace prefix\n",
    "Encoding error\n",
    "Char out of XML range\n",
    "Invalid or incomplete context\n",
    "Stack usage error\n",
    "Forbidden variable\n",
    "Operation limit exceeded\n",
    "Recursion limit exceeded\n",
    "?? Unknown error ??\n", /* Must be last in the list! */
];
const MAXERRNO: i32 = XML_XPATH_ERROR_MESSAGES.len() as i32 - 1;

/// Handle an XPath error
#[doc(alias = "xmlXPathErr")]
pub unsafe fn xml_xpath_err(ctxt: XmlXPathParserContextPtr, mut error: i32) {
    unsafe {
        if !(0..=MAXERRNO).contains(&error) {
            error = MAXERRNO;
        }
        if ctxt.is_null() {
            let code = error + XmlParserErrors::XmlXPathExpressionOk as i32
                - XmlXPathError::XPathExpressionOK as i32;
            let code = XmlParserErrors::try_from(code).unwrap();
            __xml_raise_error!(
                None,
                None,
                None,
                null_mut(),
                None,
                XmlErrorDomain::XmlFromXPath,
                code,
                XmlErrorLevel::XmlErrError,
                None,
                0,
                None,
                None,
                None,
                0,
                0,
                XML_XPATH_ERROR_MESSAGES[error as usize],
            );
            return;
        }
        // Only report the first error
        if (*ctxt).error != 0 {
            return;
        }
        (*ctxt).error = error;
        if (*ctxt).context.is_null() {
            let code = error + XmlParserErrors::XmlXPathExpressionOk as i32
                - XmlXPathError::XPathExpressionOK as i32;
            let code = XmlParserErrors::try_from(code).unwrap();
            __xml_raise_error!(
                None,
                None,
                None,
                null_mut(),
                None,
                XmlErrorDomain::XmlFromXPath,
                code,
                XmlErrorLevel::XmlErrError,
                None,
                0,
                Some((*ctxt).base.to_string().into()),
                None,
                None,
                (*ctxt).cur as _,
                0,
                XML_XPATH_ERROR_MESSAGES[error as usize],
            );
            return;
        }

        // cleanup current last error
        (*(*ctxt).context).last_error.reset();

        (*(*ctxt).context).last_error.domain = XmlErrorDomain::XmlFromXPath;
        (*(*ctxt).context).last_error.code = XmlParserErrors::try_from(
            error + XmlParserErrors::XmlXPathExpressionOk as i32
                - XmlXPathError::XPathExpressionOK as i32,
        )
        .unwrap();
        (*(*ctxt).context).last_error.level = XmlErrorLevel::XmlErrError;
        (*(*ctxt).context).last_error.str1 = Some((*ctxt).base.to_string().into());
        // (*(*ctxt).context).last_error.str1 = xml_strdup((*ctxt).base) as *mut c_char;
        (*(*ctxt).context).last_error.int1 = (*ctxt).cur as _;
        (*(*ctxt).context).last_error.node = (*(*ctxt).context).debug_node;
        if let Some(error) = (*(*ctxt).context).error {
            error(
                (*(*ctxt).context).user_data.clone(),
                &(*(*ctxt).context).last_error,
            );
        } else {
            let code = error + XmlParserErrors::XmlXPathExpressionOk as i32
                - XmlXPathError::XPathExpressionOK as i32;
            let code = XmlParserErrors::try_from(code).unwrap();
            __xml_raise_error!(
                None,
                None,
                None,
                null_mut(),
                (*(*ctxt).context).debug_node,
                XmlErrorDomain::XmlFromXPath,
                code,
                XmlErrorLevel::XmlErrError,
                None,
                0,
                Some((*ctxt).base.to_string().into()),
                None,
                None,
                (*ctxt).cur as _,
                0,
                XML_XPATH_ERROR_MESSAGES[error as usize],
            );
        }
    }
}

/// Register a new namespace. If @ns_uri is NULL it unregisters the namespace
///
/// Returns 0 in case of success, -1 in case of error
#[doc(alias = "xmlXPathRegisterNs")]
pub unsafe fn xml_xpath_register_ns(
    ctxt: XmlXPathContextPtr,
    prefix: &str,
    ns_uri: Option<&str>,
) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return -1;
        }
        if prefix.is_empty() {
            return -1;
        }

        let mut ns_hash = if let Some(table) = (*ctxt).ns_hash {
            table
        } else {
            let Some(table) = XmlHashTableRef::with_capacity(10) else {
                return -1;
            };
            (*ctxt).ns_hash = Some(table);
            table
        };
        let Some(ns_uri) = ns_uri else {
            return match ns_hash.remove_entry(prefix, |data, _| {
                xml_free(data as _);
            }) {
                Ok(_) => 0,
                Err(_) => -1,
            };
        };

        let copy: *mut XmlChar = xml_strndup(ns_uri.as_ptr(), ns_uri.len() as i32);
        if copy.is_null() {
            return -1;
        }
        match ns_hash.update_entry(prefix, copy, |data, _| {
            xml_free(data as _);
        }) {
            Ok(_) => 0,
            Err(_) => {
                xml_free(copy as _);
                -1
            }
        }
    }
}

/// Search in the namespace declaration array of the context for the given
/// namespace name associated to the given prefix
///
/// Returns the value or NULL if not found
#[doc(alias = "xmlXPathNsLookup")]
pub unsafe fn xml_xpath_ns_lookup(
    ctxt: XmlXPathContextPtr,
    prefix: *const XmlChar,
) -> *const XmlChar {
    unsafe {
        if ctxt.is_null() {
            return null();
        }
        if prefix.is_null() {
            return null();
        }

        let prefix = CStr::from_ptr(prefix as *const i8).to_string_lossy();
        if prefix == "xml" {
            return XML_XML_NAMESPACE.as_ptr() as _;
        }

        if let Some(namespaces) = (*ctxt).namespaces.as_deref() {
            for &ns in namespaces {
                if ns.prefix().as_deref() == Some(prefix.as_ref()) {
                    return ns.href;
                }
            }
        }

        (*ctxt)
            .ns_hash
            .and_then(|table| table.lookup(&prefix).copied())
            .unwrap_or(null_mut())
    }
}

/// Cleanup the XPath context data associated to registered variables
#[doc(alias = "xmlXPathRegisteredNsCleanup")]
pub unsafe fn xml_xpath_registered_ns_cleanup(ctxt: XmlXPathContextPtr) {
    unsafe {
        if ctxt.is_null() {
            return;
        }

        if let Some(mut table) = (*ctxt).ns_hash.take().map(|t| t.into_inner()) {
            table.clear_with(|data, _| {
                xml_free(data as _);
            });
        }
    }
}

/// Register a new variable value. If @value is NULL it unregisters the variable
///
/// Returns 0 in case of success, -1 in case of error
#[doc(alias = "xmlXPathRegisterVariable")]
pub unsafe fn xml_xpath_register_variable(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    value: XmlXPathObjectPtr,
) -> i32 {
    unsafe { xml_xpath_register_variable_ns(ctxt, name, null(), value) }
}

extern "C" fn xml_xpath_free_object_entry(obj: XmlXPathObjectPtr) {
    unsafe {
        xml_xpath_free_object(obj);
    }
}

/// Register a new variable value. If @value is NULL it unregisters the variable
///
/// Returns 0 in case of success, -1 in case of error
#[doc(alias = "xmlXPathRegisterVariableNS")]
pub unsafe fn xml_xpath_register_variable_ns(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    ns_uri: *const XmlChar,
    value: XmlXPathObjectPtr,
) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return -1;
        }
        if name.is_null() {
            return -1;
        }

        let mut var_hash = if let Some(var_hash) = (*ctxt).var_hash {
            var_hash
        } else {
            let Some(table) = XmlHashTableRef::with_capacity(0) else {
                return -1;
            };
            (*ctxt).var_hash = Some(table);
            table
        };
        if value.is_null() {
            return match var_hash.remove_entry2(
                CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
                (!ns_uri.is_null())
                    .then(|| CStr::from_ptr(ns_uri as *const i8).to_string_lossy())
                    .as_deref(),
                |data, _| {
                    xml_xpath_free_object_entry(data);
                },
            ) {
                Ok(_) => 0,
                Err(_) => -1,
            };
        }

        match var_hash.update_entry2(
            CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
            (!ns_uri.is_null())
                .then(|| CStr::from_ptr(ns_uri as *const i8).to_string_lossy())
                .as_deref(),
            value,
            |data, _| {
                xml_xpath_free_object_entry(data);
            },
        ) {
            Ok(_) => 0,
            Err(_) => -1,
        }
    }
}

/// Cleanup the XPath context data associated to registered functions
#[doc(alias = "xmlXPathRegisteredFuncsCleanup")]
pub unsafe fn xml_xpath_registered_funcs_cleanup(ctxt: XmlXPathContextPtr) {
    unsafe {
        if ctxt.is_null() {
            return;
        }

        (*ctxt).func_hash.clear();
    }
}

/// Search in the Variable array of the context for the given variable value.
///
/// Returns a copy of the value or NULL if not found
#[doc(alias = "xmlXPathVariableLookup")]
pub unsafe fn xml_xpath_variable_lookup(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
) -> XmlXPathObjectPtr {
    unsafe {
        if ctxt.is_null() {
            return null_mut();
        }

        if let Some(var_lookup_func) = (*ctxt).var_lookup_func {
            return var_lookup_func((*ctxt).var_lookup_data, name, null());
        }
        xml_xpath_variable_lookup_ns(ctxt, name, null())
    }
}

macro_rules! XP_HAS_CACHE {
    ($c:expr) => {
        !$c.is_null() && !(*$c).cache.is_null()
    };
}

/// This is the cached version of xmlXPathWrapNodeSet().
/// Wrap the Nodeset @val in a new xmlXPathObjectPtr
///
/// Returns the created or reused object.
///
/// In case of error the node set is destroyed and NULL is returned.
#[doc(alias = "xmlXPathCacheWrapNodeSet")]
unsafe fn xml_xpath_cache_wrap_node_set(
    ctxt: XmlXPathContextPtr,
    val: Option<Box<XmlNodeSet>>,
) -> XmlXPathObjectPtr {
    unsafe {
        if !ctxt.is_null() && !(*ctxt).cache.is_null() {
            let cache: XmlXPathContextCachePtr = (*ctxt).cache as XmlXPathContextCachePtr;

            if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
                (*(*cache).misc_objs).number -= 1;
                let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                    .items
                    .add((*(*cache).misc_objs).number as usize)
                    as XmlXPathObjectPtr;
                (*ret).typ = XmlXPathObjectType::XPathNodeset;
                (*ret).nodesetval = val;
                return ret;
            }
        }

        xml_xpath_wrap_node_set(val)
    }
}

/// Handle a redefinition of attribute error
#[doc(alias = "xmlXPathErrMemory")]
pub unsafe fn xml_xpath_err_memory(ctxt: XmlXPathContextPtr, extra: Option<&str>) {
    unsafe {
        if !ctxt.is_null() {
            (*ctxt).last_error.reset();
            if let Some(extra) = extra {
                let buf = format!("Memory allocation failed : {extra}\n",);
                (*ctxt).last_error.message = Some(buf.into());
                // (*ctxt).last_error.message = xml_strdup(buf.as_ptr()) as *mut c_char;
            } else {
                (*ctxt).last_error.message = Some("Memory allocation failed\n".into());
                // xml_strdup(c"Memory allocation failed\n".as_ptr() as _) as *mut c_char;
            }
            (*ctxt).last_error.domain = XmlErrorDomain::XmlFromXPath;
            (*ctxt).last_error.code = XmlParserErrors::XmlErrNoMemory;
            if let Some(error) = (*ctxt).error {
                error((*ctxt).user_data.clone(), &(*ctxt).last_error);
            }
        } else if let Some(extra) = extra {
            __xml_raise_error!(
                None,
                None,
                None,
                null_mut(),
                None,
                XmlErrorDomain::XmlFromXPath,
                XmlParserErrors::XmlErrNoMemory,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                Some(extra.to_owned().into()),
                None,
                None,
                0,
                0,
                "Memory allocation failed : {}\n",
                extra
            );
        } else {
            __xml_raise_error!(
                None,
                None,
                None,
                null_mut(),
                None,
                XmlErrorDomain::XmlFromXPath,
                XmlParserErrors::XmlErrNoMemory,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                None,
                None,
                None,
                0,
                0,
                "Memory allocation failed\n",
            );
        }
    }
}

/// This is the cached version of xmlXPathNewString().
/// Acquire an xmlXPathObjectPtr of type string and of value @val
///
/// Returns the created or reused object.
#[doc(alias = "xmlXPathCacheNewString", alias = "xmlXPathCacheNewCString")]
pub(super) unsafe fn xml_xpath_cache_new_string(
    ctxt: XmlXPathContextPtr,
    val: Option<&str>,
) -> XmlXPathObjectPtr {
    unsafe {
        if !ctxt.is_null() && !(*ctxt).cache.is_null() {
            let cache: XmlXPathContextCachePtr = (*ctxt).cache as XmlXPathContextCachePtr;

            if !(*cache).string_objs.is_null() && (*(*cache).string_objs).number != 0 {
                (*(*cache).string_objs).number -= 1;
                let ret: XmlXPathObjectPtr = *(*(*cache).string_objs)
                    .items
                    .add((*(*cache).string_objs).number as usize)
                    as XmlXPathObjectPtr;
                (*ret).typ = XmlXPathObjectType::XPathString;
                (*ret).stringval = Some(val.unwrap_or("").to_owned());
                return ret;
            } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
                (*(*cache).misc_objs).number -= 1;
                let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                    .items
                    .add((*(*cache).misc_objs).number as usize)
                    as XmlXPathObjectPtr;

                (*ret).typ = XmlXPathObjectType::XPathString;
                (*ret).stringval = Some(val.unwrap_or("").to_owned());
                return ret;
            }
        }
        xml_xpath_new_string(val)
    }
}

/// This is the cached version of xmlXPathNewBoolean().
/// Acquires an xmlXPathObjectPtr of type boolean and of value @val
///
/// Returns the created or reused object.
#[doc(alias = "xmlXPathCacheNewBoolean")]
pub(super) unsafe fn xml_xpath_cache_new_boolean(
    ctxt: XmlXPathContextPtr,
    val: bool,
) -> XmlXPathObjectPtr {
    unsafe {
        if !ctxt.is_null() && !(*ctxt).cache.is_null() {
            let cache: XmlXPathContextCachePtr = (*ctxt).cache as XmlXPathContextCachePtr;

            if !(*cache).boolean_objs.is_null() && (*(*cache).boolean_objs).number != 0 {
                (*(*cache).boolean_objs).number -= 1;
                let ret: XmlXPathObjectPtr = *(*(*cache).boolean_objs)
                    .items
                    .add((*(*cache).boolean_objs).number as usize)
                    as XmlXPathObjectPtr;
                (*ret).typ = XmlXPathObjectType::XPathBoolean;
                (*ret).boolval = val;
                return ret;
            } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
                (*(*cache).misc_objs).number -= 1;
                let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                    .items
                    .add((*(*cache).misc_objs).number as usize)
                    as XmlXPathObjectPtr;

                (*ret).typ = XmlXPathObjectType::XPathBoolean;
                (*ret).boolval = val;
                return ret;
            }
        }
        xml_xpath_new_boolean(val)
    }
}

/// This is the cached version of xmlXPathNewFloat().
/// Acquires an xmlXPathObjectPtr of type f64 and of value @val
///
/// Returns the created or reused object.
#[doc(alias = "xmlXPathCacheNewFloat")]
pub(super) unsafe fn xml_xpath_cache_new_float(
    ctxt: XmlXPathContextPtr,
    val: f64,
) -> XmlXPathObjectPtr {
    unsafe {
        if !ctxt.is_null() && !(*ctxt).cache.is_null() {
            let cache: XmlXPathContextCachePtr = (*ctxt).cache as XmlXPathContextCachePtr;

            if !(*cache).number_objs.is_null() && (*(*cache).number_objs).number != 0 {
                (*(*cache).number_objs).number -= 1;
                let ret: XmlXPathObjectPtr = *(*(*cache).number_objs)
                    .items
                    .add((*(*cache).number_objs).number as usize)
                    as XmlXPathObjectPtr;
                (*ret).typ = XmlXPathObjectType::XPathNumber;
                (*ret).floatval = val;
                return ret;
            } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
                (*(*cache).misc_objs).number -= 1;
                let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                    .items
                    .add((*(*cache).misc_objs).number as usize)
                    as XmlXPathObjectPtr;

                (*ret).typ = XmlXPathObjectType::XPathNumber;
                (*ret).floatval = val;
                return ret;
            }
        }
        xml_xpath_new_float(val)
    }
}

/// This is the cached version of xmlXPathObjectCopy().
/// Acquire a copy of a given object
///
/// Returns a created or reused created object.
#[doc(alias = "xmlXPathCacheObjectCopy")]
pub(super) unsafe fn xml_xpath_cache_object_copy(
    ctxt: XmlXPathContextPtr,
    val: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    unsafe {
        if val.is_null() {
            return null_mut();
        }

        if XP_HAS_CACHE!(ctxt) {
            match (*val).typ {
                XmlXPathObjectType::XPathNodeset => {
                    return xml_xpath_cache_wrap_node_set(
                        ctxt,
                        xml_xpath_node_set_merge(None, (*val).nodesetval.as_deref()),
                    );
                }
                XmlXPathObjectType::XPathString => {
                    return xml_xpath_cache_new_string(ctxt, (*val).stringval.as_deref());
                }
                XmlXPathObjectType::XPathBoolean => {
                    return xml_xpath_cache_new_boolean(ctxt, (*val).boolval);
                }
                XmlXPathObjectType::XPathNumber => {
                    return xml_xpath_cache_new_float(ctxt, (*val).floatval);
                }
                _ => {}
            }
        }
        xml_xpath_object_copy(val)
    }
}

/// Search in the Variable array of the context for the given variable value.
///
/// Returns the a copy of the value or NULL if not found
#[doc(alias = "xmlXPathVariableLookupNS")]
pub unsafe fn xml_xpath_variable_lookup_ns(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    ns_uri: *const XmlChar,
) -> XmlXPathObjectPtr {
    unsafe {
        if ctxt.is_null() {
            return null_mut();
        }

        if let Some(var_lookup_func) = (*ctxt).var_lookup_func {
            let ret: XmlXPathObjectPtr = var_lookup_func((*ctxt).var_lookup_data, name, ns_uri);
            if !ret.is_null() {
                return ret;
            }
        }

        let Some(var_hash) = (*ctxt).var_hash else {
            return null_mut();
        };
        if name.is_null() {
            return null_mut();
        }

        let obj = var_hash
            .lookup2(
                CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
                (!ns_uri.is_null())
                    .then(|| CStr::from_ptr(ns_uri as *const i8).to_string_lossy())
                    .as_deref(),
            )
            .copied()
            .unwrap_or(null_mut());
        xml_xpath_cache_object_copy(ctxt, obj)
    }
}

/// Cleanup the XPath context data associated to registered variables
#[doc(alias = "xmlXPathRegisteredVariablesCleanup")]
pub unsafe fn xml_xpath_registered_variables_cleanup(ctxt: XmlXPathContextPtr) {
    unsafe {
        if ctxt.is_null() {
            return;
        }

        if let Some(mut var_hash) = (*ctxt).var_hash.take().map(|t| t.into_inner()) {
            var_hash.clear_with(|data, _| {
                xml_xpath_free_object_entry(data);
            });
        }
    }
}

/// Create a new Xpath component
///
/// Returns the newly allocated xmlXPathCompExprPtr or NULL in case of error
#[doc(alias = "xmlXPathNewCompExpr")]
pub(super) unsafe fn xml_xpath_new_comp_expr() -> XmlXPathCompExprPtr {
    unsafe {
        let cur: XmlXPathCompExprPtr =
            xml_malloc(size_of::<XmlXPathCompExpr>()) as XmlXPathCompExprPtr;
        if cur.is_null() {
            xml_xpath_err_memory(null_mut(), Some("allocating component\n"));
            return null_mut();
        }
        std::ptr::write(&mut *cur, XmlXPathCompExpr::default());
        (*cur).steps.reserve(10);
        (*cur).last = -1;
        cur
    }
}

/// Handle a redefinition of attribute error
#[doc(alias = "xmlXPathPErrMemory")]
pub(super) unsafe fn xml_xpath_perr_memory(ctxt: XmlXPathParserContextPtr, extra: Option<&str>) {
    unsafe {
        if ctxt.is_null() {
            xml_xpath_err_memory(null_mut(), extra);
        } else {
            (*ctxt).error = XmlXPathError::XPathMemoryError as i32;
            xml_xpath_err_memory((*ctxt).context, extra);
        }
    }
}

pub const XML_NODESET_DEFAULT: usize = 10;

/// Namespace node in libxml don't match the XPath semantic. In a node set
/// the namespace nodes are duplicated and the next pointer is set to the
/// parent node in the XPath semantic.
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNodeSetDupNs")]
pub unsafe fn xml_xpath_node_set_dup_ns(
    node: Option<XmlGenericNodePtr>,
    mut ns: XmlNsPtr,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        // if ns.is_null() || !matches!((*ns).typ, XmlElementType::XmlNamespaceDecl) {
        //     return null_mut();
        // }
        if node.is_none_or(|node| matches!(node.element_type(), XmlElementType::XmlNamespaceDecl)) {
            if ns.node.is_none() {
                ns.node = ns.next.map(|next| next.into());
            }
            return Some(ns.into());
        }

        // Allocate a new Namespace and fill the fields.
        let Some(mut cur) = XmlNsPtr::new(XmlNs {
            typ: XmlElementType::XmlNamespaceDecl,
            ..Default::default()
        }) else {
            xml_xpath_err_memory(null_mut(), Some("duplicating namespace\n"));
            return None;
        };
        if !ns.href.is_null() {
            cur.href = xml_strdup(ns.href);
        }
        if ns.prefix().is_some() {
            cur.prefix = xml_strdup(ns.prefix);
        }
        // cur.next = node as *mut XmlNs;
        cur.node = node;
        Some(cur.into())
    }
}

/// This is the cached version of xmlXPathNewNodeSet().
/// Acquire an xmlXPathObjectPtr of type NodeSet and initialize
/// it with the single Node @val
///
/// Returns the created or reused object.
#[doc(alias = "xmlXPathCacheNewNodeSet")]
pub(super) unsafe fn xml_xpath_cache_new_node_set(
    ctxt: XmlXPathContextPtr,
    val: Option<XmlGenericNodePtr>,
) -> XmlXPathObjectPtr {
    unsafe {
        if !ctxt.is_null() && !(*ctxt).cache.is_null() {
            let cache: XmlXPathContextCachePtr = (*ctxt).cache as XmlXPathContextCachePtr;

            if !(*cache).nodeset_objs.is_null() && (*(*cache).nodeset_objs).number != 0 {
                // Use the nodeset-cache.
                (*(*cache).nodeset_objs).number -= 1;
                let ret: XmlXPathObjectPtr = *(*(*cache).nodeset_objs)
                    .items
                    .add((*(*cache).nodeset_objs).number as usize)
                    as XmlXPathObjectPtr;
                (*ret).typ = XmlXPathObjectType::XPathNodeset;
                (*ret).boolval = false;
                if let Some(val) = val {
                    if let Some(table) = (*ret)
                        .nodesetval
                        .as_mut()
                        .filter(|_| !matches!(val.element_type(), XmlElementType::XmlNamespaceDecl))
                        .map(|set| &mut set.node_tab)
                    {
                        table.clear();
                        table.push(val);
                    } else if let Some(nodeset) = (*ret).nodesetval.as_deref_mut() {
                        // TODO: Check memory error.
                        nodeset.add_unique(val);
                    }
                }
                return ret;
            } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
                // Fallback to misc-cache.

                let set = xml_xpath_node_set_create(val);
                if set.is_none() {
                    (*ctxt).last_error.domain = XmlErrorDomain::XmlFromXPath;
                    (*ctxt).last_error.code = XmlParserErrors::XmlErrNoMemory;
                    return null_mut();
                }

                (*(*cache).misc_objs).number -= 1;
                let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                    .items
                    .add((*(*cache).misc_objs).number as usize)
                    as XmlXPathObjectPtr;

                (*ret).typ = XmlXPathObjectType::XPathNodeset;
                (*ret).boolval = false;
                (*ret).nodesetval = set;
                return ret;
            }
        }
        xml_xpath_new_node_set(val)
    }
}

/// Initialize the context to the root of the document
#[doc(alias = "xmlXPathRoot")]
pub unsafe fn xml_xpath_root(ctxt: XmlXPathParserContextPtr) {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return;
        }
        (*ctxt).value_push(xml_xpath_cache_new_node_set(
            (*ctxt).context,
            (*(*ctxt).context).doc.map(|doc| doc.into()),
        ));
    }
}

/// Try to compile the XPath expression as a streamable subset.
///
/// Returns the compiled expression or NULL if failed to compile.
#[doc(alias = "xmlXPathTryStreamCompile")]
#[cfg(feature = "libxml_pattern")]
pub unsafe fn xml_xpath_try_stream_compile(
    ctxt: XmlXPathContextPtr,
    xpath: &str,
) -> XmlXPathCompExprPtr {
    unsafe {
        // Optimization: use streaming patterns when the XPath expression can
        // be compiled to a stream lookup
        let stream: XmlPatternPtr;
        let comp: XmlXPathCompExprPtr;

        if !xpath.contains(['[', '(', '@']) {
            // We don't try to handle expressions using the verbose axis
            // specifiers ("::"), just the simplified form at this point.
            // Additionally, if there is no list of namespaces available and
            //  there's a ":" in the expression, indicating a prefixed QName,
            //  then we won't try to compile either. xmlPatterncompile() needs
            //  to have a list of namespaces at compilation time in order to
            //  compile prefixed name tests.
            if let Some((_, tmp)) = xpath.split_once(':') {
                if ctxt.is_null()
                    || (*ctxt).namespaces.as_ref().map_or(0, |t| t.len()) == 0
                    || tmp.starts_with(':')
                {
                    return null_mut();
                }
            }

            let mut namespaces = None;
            if !ctxt.is_null() {
                if let Some(table) = (*ctxt).namespaces.as_deref().filter(|t| !t.is_empty()) {
                    let namespaces =
                        namespaces.get_or_insert_with(|| vec![(null(), null()); table.len()]);
                    for (i, &ns) in table.iter().enumerate() {
                        namespaces[i] = (ns.href, ns.prefix);
                    }
                }
            }

            let xpath = CString::new(xpath).unwrap();
            stream = xml_patterncompile(
                xpath.as_ptr() as *const u8,
                XmlPatternFlags::XmlPatternXpath as i32,
                namespaces,
            );
            if !stream.is_null() && xml_pattern_streamable(stream) == 1 {
                comp = xml_xpath_new_comp_expr();
                if comp.is_null() {
                    xml_xpath_err_memory(ctxt, Some("allocating streamable expression\n"));
                    xml_free_pattern(stream);
                    return null_mut();
                }
                (*comp).stream = stream;
                return comp;
            }
            xml_free_pattern(stream);
        }
        null_mut()
    }
}

pub(super) const XPATH_MAX_RECURSION_DEPTH: usize = 5000;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlXPathAxisVal {
    AxisAncestor = 1,
    AxisAncestorOrSelf = 2,
    AxisAttribute = 3,
    AxisChild = 4,
    AxisDescendant = 5,
    AxisDescendantOrSelf = 6,
    AxisFollowing = 7,
    AxisFollowingSibling = 8,
    AxisNamespace = 9,
    AxisParent = 10,
    AxisPreceding = 11,
    AxisPrecedingSibling = 12,
    AxisSelf = 13,
}

impl TryFrom<i32> for XmlXPathAxisVal {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == 1 {
            Ok(Self::AxisAncestor)
        } else if value == 2 {
            Ok(Self::AxisAncestorOrSelf)
        } else if value == 3 {
            Ok(Self::AxisAttribute)
        } else if value == 4 {
            Ok(Self::AxisChild)
        } else if value == 5 {
            Ok(Self::AxisDescendant)
        } else if value == 6 {
            Ok(Self::AxisDescendantOrSelf)
        } else if value == 7 {
            Ok(Self::AxisFollowing)
        } else if value == 8 {
            Ok(Self::AxisFollowingSibling)
        } else if value == 9 {
            Ok(Self::AxisNamespace)
        } else if value == 10 {
            Ok(Self::AxisParent)
        } else if value == 11 {
            Ok(Self::AxisPreceding)
        } else if value == 12 {
            Ok(Self::AxisPrecedingSibling)
        } else if value == 13 {
            Ok(Self::AxisSelf)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to XmlXPathAxisVal"
            ))
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlXPathTestVal {
    NodeTestNone = 0,
    NodeTestType = 1,
    NodeTestPI = 2,
    NodeTestAll = 3,
    NodeTestNs = 4,
    NodeTestName = 5,
}

impl TryFrom<i32> for XmlXPathTestVal {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == 0 {
            Ok(Self::NodeTestNone)
        } else if value == 1 {
            Ok(Self::NodeTestType)
        } else if value == 2 {
            Ok(Self::NodeTestPI)
        } else if value == 3 {
            Ok(Self::NodeTestAll)
        } else if value == 4 {
            Ok(Self::NodeTestNs)
        } else if value == 5 {
            Ok(Self::NodeTestName)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to XmlXPathTestVal"
            ))
        }
    }
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlXPathTypeVal {
    NodeTypeNode = 0,
    NodeTypeComment = XmlElementType::XmlCommentNode as isize,
    NodeTypeText = XmlElementType::XmlTextNode as isize,
    NodeTypePI = XmlElementType::XmlPINode as isize,
}

impl TryFrom<i32> for XmlXPathTypeVal {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == 0 {
            Ok(Self::NodeTypeNode)
        } else if value == Self::NodeTypeComment as i32 {
            Ok(Self::NodeTypeComment)
        } else if value == Self::NodeTypeText as i32 {
            Ok(Self::NodeTypeText)
        } else if value == Self::NodeTypePI as i32 {
            Ok(Self::NodeTypePI)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to XmlXPathTypeVal"
            ))
        }
    }
}

pub unsafe fn xml_xpath_optimize_expression(
    pctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
) {
    unsafe {
        let comp: XmlXPathCompExprPtr = (*pctxt).comp;

        // Try to rewrite "descendant-or-self::node()/foo" to an optimized
        // internal representation.

        if matches!((*op).op, XmlXPathOp::XPathOpCollect /* 11 */)
            && (*op).ch1 != -1
            && (*op).ch2 == -1
        {
            let prevop = &(*comp).steps[(*op).ch1 as usize];

            if matches!(prevop.op, XmlXPathOp::XPathOpCollect /* 11 */)
                && prevop.value == XmlXPathAxisVal::AxisDescendantOrSelf as i32
                && prevop.ch2 == -1
                && prevop.value2 == XmlXPathTestVal::NodeTestType as i32
                && prevop.value3 == XmlXPathTypeVal::NodeTypeNode as i32
            {
                // This is a "descendant-or-self::node()" without predicates.
                // Try to eliminate it.

                if (*op).value == XmlXPathAxisVal::AxisChild as i32
                    || (*op).value == XmlXPathAxisVal::AxisDescendant as i32
                {
                    // Convert "descendant-or-self::node()/child::" or
                    // "descendant-or-self::node()/descendant::" to
                    // "descendant::"
                    (*op).ch1 = prevop.ch1;
                    (*op).value = XmlXPathAxisVal::AxisDescendant as i32;
                } else if (*op).value == XmlXPathAxisVal::AxisSelf as i32
                    || (*op).value == XmlXPathAxisVal::AxisDescendantOrSelf as i32
                {
                    // Convert "descendant-or-self::node()/self::" or
                    // "descendant-or-self::node()/descendant-or-self::" to
                    // to "descendant-or-self::"
                    (*op).ch1 = prevop.ch1;
                    (*op).value = XmlXPathAxisVal::AxisDescendantOrSelf as i32;
                }
            }
        }

        // OP_VALUE has invalid ch1.
        if matches!((*op).op, XmlXPathOp::XPathOpValue) {
            return;
        }

        // Recurse
        let ctxt: XmlXPathContextPtr = (*pctxt).context;
        if !ctxt.is_null() {
            if (*ctxt).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
                return;
            }
            (*ctxt).depth += 1;
        }
        if (*op).ch1 != -1 {
            xml_xpath_optimize_expression(pctxt, &raw mut (*comp).steps[(*op).ch1 as usize]);
        }
        if (*op).ch2 != -1 {
            xml_xpath_optimize_expression(pctxt, &raw mut (*comp).steps[(*op).ch2 as usize]);
        }
        if !ctxt.is_null() {
            (*ctxt).depth -= 1;
        }
    }
}

/// Evaluate the Precompiled Streamable XPath expression in the given context.
#[doc(alias = "xmlXPathRunStreamEval")]
#[cfg(feature = "libxml_pattern")]
pub(super) unsafe fn xml_xpath_run_stream_eval(
    ctxt: XmlXPathContextPtr,
    comp: XmlPatternPtr,
    result_seq: *mut XmlXPathObjectPtr,
    to_bool: i32,
) -> i32 {
    unsafe {
        let mut max_depth: i32;
        let mut ret: i32;
        let mut depth: i32;

        if ctxt.is_null() || comp.is_null() {
            return -1;
        }
        max_depth = xml_pattern_max_depth(comp);
        if max_depth == -1 {
            return -1;
        }
        if max_depth == -2 {
            max_depth = 10000;
        }
        let min_depth: i32 = xml_pattern_min_depth(comp);
        if min_depth == -1 {
            return -1;
        }
        let from_root: i32 = xml_pattern_from_root(comp);
        if from_root < 0 {
            return -1;
        }

        if to_bool == 0 {
            if result_seq.is_null() {
                return -1;
            }
            *result_seq = xml_xpath_cache_new_node_set(ctxt, None);
            if (*result_seq).is_null() {
                return -1;
            }
        }

        // handle the special cases of "/" amd "." being matched
        if min_depth == 0 {
            if from_root != 0 {
                // Select "/"
                if to_bool != 0 {
                    return 1;
                }
                // TODO: Check memory error.
                if let Some(nodeset) = (*(*result_seq)).nodesetval.as_deref_mut() {
                    nodeset.add_unique((*ctxt).doc.unwrap().into());
                }
            } else {
                // Select "self::node()"
                if to_bool != 0 {
                    return 1;
                }
                // TODO: Check memory error.
                if let Some(nodeset) = (*(*result_seq)).nodesetval.as_deref_mut() {
                    nodeset.add_unique((*ctxt).node.unwrap());
                }
            }
        }
        if max_depth == 0 {
            return 0;
        }

        let mut limit = None;
        let cur = if from_root != 0 {
            (*ctxt).doc.map(|doc| doc.into())
        } else if let Some(node) = (*ctxt).node {
            match node.element_type() {
                XmlElementType::XmlElementNode
                | XmlElementType::XmlDocumentNode
                | XmlElementType::XmlDocumentFragNode
                | XmlElementType::XmlHTMLDocumentNode => {
                    limit = Some(node);
                }
                XmlElementType::XmlAttributeNode
                | XmlElementType::XmlTextNode
                | XmlElementType::XmlCDATASectionNode
                | XmlElementType::XmlEntityRefNode
                | XmlElementType::XmlEntityNode
                | XmlElementType::XmlPINode
                | XmlElementType::XmlCommentNode
                | XmlElementType::XmlNotationNode
                | XmlElementType::XmlDTDNode
                | XmlElementType::XmlDocumentTypeNode
                | XmlElementType::XmlElementDecl
                | XmlElementType::XmlAttributeDecl
                | XmlElementType::XmlEntityDecl
                | XmlElementType::XmlNamespaceDecl
                | XmlElementType::XmlXIncludeStart
                | XmlElementType::XmlXIncludeEnd => {}
                _ => unreachable!(),
            }
            limit
        } else {
            None
        };
        let Some(mut cur) = cur else {
            return 0;
        };

        let patstream: XmlStreamCtxtPtr = xml_pattern_get_stream_ctxt(comp);
        if patstream.is_null() {
            // QUESTION TODO: Is this an error?
            return 0;
        }

        let eval_all_nodes: i32 = xml_stream_wants_any_node(patstream);

        if from_root != 0 {
            ret = xml_stream_push(patstream, null(), null());
            if ret < 0 {
            } else if ret == 1 {
                if to_bool != 0 {
                    // goto return_1;
                    if !patstream.is_null() {
                        xml_free_stream_ctxt(patstream);
                    }
                    return 1;
                }
                // TODO: Check memory error.
                if let Some(nodeset) = (*(*result_seq)).nodesetval.as_deref_mut() {
                    nodeset.add_unique(cur);
                }
            }
        }
        depth = 0;
        let mut goto_scan_children = true;
        // goto scan_children;
        // next_node:
        'main: while {
            'to_continue_main: {
                'next_node: loop {
                    if !goto_scan_children {
                        if (*ctxt).op_limit != 0 {
                            if (*ctxt).op_count >= (*ctxt).op_limit {
                                generic_error!("XPath operation limit exceeded\n");
                                xml_free_stream_ctxt(patstream);
                                return -1;
                            }
                            (*ctxt).op_count += 1;
                        }

                        match cur.element_type() {
                            XmlElementType::XmlElementNode
                            | XmlElementType::XmlTextNode
                            | XmlElementType::XmlCDATASectionNode
                            | XmlElementType::XmlCommentNode
                            | XmlElementType::XmlPINode => 'to_break: {
                                ret =
                                    if matches!(cur.element_type(), XmlElementType::XmlElementNode)
                                    {
                                        let node = XmlNodePtr::try_from(cur).unwrap();
                                        xml_stream_push(
                                            patstream,
                                            node.name,
                                            node.ns.map_or(null_mut(), |ns| ns.href),
                                        )
                                    } else if eval_all_nodes != 0 {
                                        xml_stream_push_node(
                                            patstream,
                                            null(),
                                            null(),
                                            cur.element_type() as i32,
                                        )
                                    } else {
                                        break 'to_break;
                                    };
                                if ret < 0 {
                                    // NOP.
                                } else if ret == 1 {
                                    if to_bool != 0 {
                                        // goto return_1;
                                        if !patstream.is_null() {
                                            xml_free_stream_ctxt(patstream);
                                        }
                                        return 1;
                                    }
                                    if let Some(nodeset) =
                                        (*(*result_seq)).nodesetval.as_deref_mut()
                                    {
                                        if nodeset.add_unique(cur) < 0 {
                                            (*ctxt).last_error.domain =
                                                XmlErrorDomain::XmlFromXPath;
                                            (*ctxt).last_error.code =
                                                XmlParserErrors::XmlErrNoMemory;
                                        }
                                    }
                                }
                                if cur.children().is_none() || depth >= max_depth {
                                    // ret =
                                    xml_stream_pop(patstream);
                                    while let Some(next) = cur.next() {
                                        cur = next;
                                        if !matches!(
                                            cur.element_type(),
                                            XmlElementType::XmlEntityDecl
                                                | XmlElementType::XmlDTDNode
                                        ) {
                                            // goto next_node;
                                            continue 'next_node;
                                        }
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                    goto_scan_children = false;

                    // scan_children:
                    if matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
                        break 'main;
                    }
                    if let Some(children) = cur.children().filter(|_| depth < max_depth) {
                        // Do not descend on entities declarations
                        if !matches!(children.element_type(), XmlElementType::XmlEntityDecl) {
                            cur = children;
                            depth += 1;
                            // Skip DTDs
                            if !matches!(cur.element_type(), XmlElementType::XmlDTDNode) {
                                break 'to_continue_main;
                            }
                        }
                    }

                    if Some(cur) == limit {
                        break 'main;
                    }

                    while let Some(next) = cur.next() {
                        cur = next;
                        if !matches!(
                            cur.element_type(),
                            XmlElementType::XmlEntityDecl | XmlElementType::XmlDTDNode
                        ) {
                            // goto next_node;
                            continue 'next_node;
                        }
                    }

                    break 'next_node;
                }

                'inner: loop {
                    let Some(parent) = cur.parent() else {
                        break 'main;
                    };
                    depth -= 1;
                    cur = parent;
                    if Some(cur) == limit
                        || matches!(cur.element_type(), XmlElementType::XmlDocumentNode)
                    {
                        // goto done;
                        break 'main;
                    }
                    if matches!(cur.element_type(), XmlElementType::XmlElementNode)
                        || (eval_all_nodes != 0
                            && matches!(
                                cur.element_type(),
                                XmlElementType::XmlTextNode
                                    | XmlElementType::XmlCDATASectionNode
                                    | XmlElementType::XmlCommentNode
                                    | XmlElementType::XmlPINode
                            ))
                    {
                        // ret =
                        xml_stream_pop(patstream);
                    };
                    if let Some(next) = cur.next() {
                        cur = next;
                        break 'inner;
                    }
                }
            }

            depth >= 0
        } {}

        // done:

        if !patstream.is_null() {
            xml_free_stream_ctxt(patstream);
        }
        0

        // return_1:
        // if (patstream) {
        // 	xmlFreeStreamCtxt(patstream);
        // }
        // return 1;
    }
}

// Used for merging node sets in xmlXPathCollectAndTest().
#[doc(alias = "xmlXPathNodeSetMergeFunction")]
pub type XmlXPathNodeSetMergeFunction =
    unsafe fn(Option<Box<XmlNodeSet>>, Option<&mut XmlNodeSet>) -> Option<Box<XmlNodeSet>>;

// A traversal function enumerates nodes along an axis.
// Initially it must be called with NULL, and it indicates
// termination on the axis by returning NULL.
pub type XmlXPathTraversalFunction = unsafe fn(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr>;

/// Traversal function for the "child" direction and nodes of type element.
/// The child axis contains the children of the context node in document order.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextChildElement")]
unsafe fn xml_xpath_next_child_element(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }

        let Some(mut cur) = cur else {
            let cur = (*(*ctxt).context).node?;
            // Get the first element child.
            match cur.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlDocumentFragNode
            // URGENT TODO: entify-refs as well?
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode => {
                let mut cur = cur.children()?;
                if matches!(cur.element_type(), XmlElementType::XmlElementNode) {
                    return Some(cur);
                }
                while let Some(next) = cur.next().filter(|next| !matches!(next.element_type(), XmlElementType::XmlElementNode)) {
                    cur = next;
                }
                return Some(cur);
            }
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                return XmlDocPtr::try_from(cur).unwrap().get_root_element().map(|root| root.into());
            }
            _ => {
                return None;
            }
        }
        };
        // Get the next sibling element node.
        match cur.element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlXIncludeEnd => {}
            /* case XML_DTD_NODE: */ /* URGENT TODO: DTD-node as well? */
            _ => {
                return None;
            }
        }
        let next = cur.next()?;
        if matches!(next.element_type(), XmlElementType::XmlElementNode) {
            return Some(next);
        }
        cur = next;
        while let Some(next) = cur
            .next()
            .filter(|next| !matches!(next.element_type(), XmlElementType::XmlElementNode))
        {
            cur = next;
        }
        Some(cur)
    }
}

/// Traversal function for the "preceding" direction
/// the preceding axis contains all nodes in the same document as the context
/// node that are before the context node in document order, excluding any
/// ancestors and excluding attribute nodes and namespace nodes; the nodes are
/// ordered in reverse document order
/// This is a faster implementation but internal only since it requires a
/// state kept in the parser context: (*ctxt).ancestor.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextPrecedingInternal")]
unsafe fn xml_xpath_next_preceding_internal(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        let mut cur = cur.or_else(|| {
            let mut cur = (*(*ctxt).context).node?;
            if matches!(cur.element_type(), XmlElementType::XmlAttributeNode) {
                cur = cur.parent()?;
            } else if let Ok(ns) = XmlNsPtr::try_from(cur) {
                cur = ns
                    .node
                    .filter(|node| node.element_type() != XmlElementType::XmlNamespaceDecl)?;
            }
            (*ctxt).ancestor = cur.parent();
            Some(cur)
        })?;
        if matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        if let Some(prev) = cur
            .prev()
            .filter(|p| matches!(p.element_type(), XmlElementType::XmlDTDNode))
        {
            cur = prev;
        }
        while cur.prev().is_none() {
            cur = cur.parent()?;
            if Some(cur) == (*(*ctxt).context).doc.unwrap().children {
                return None;
            }
            if Some(cur) != (*ctxt).ancestor {
                return Some(cur);
            }
            (*ctxt).ancestor = cur.parent();
        }
        cur = cur.prev()?;
        while let Some(last) = cur.last() {
            cur = last;
        }
        Some(cur)
    }
}

unsafe fn xml_xpath_is_positional_predicate(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    max_pos: *mut i32,
) -> i32 {
    unsafe {
        // BIG NOTE: This is not intended for XPATH_OP_FILTER yet!

        // If not -1, then ch1 will point to:
        // 1) For predicates (XPATH_OP_PREDICATE):
        //    - an inner predicate operator
        // 2) For filters (XPATH_OP_FILTER):
        //    - an inner filter operator OR
        //    - an expression selecting the node set.
        //      E.g. "key('a', 'b')" or "(//foo | //bar)".
        if !matches!(
            (*op).op,
            XmlXPathOp::XPathOpPredicate | XmlXPathOp::XPathOpFilter
        ) {
            return 0;
        }

        if (*op).ch2 == -1 || (*op).ch2 >= (*(*ctxt).comp).steps.len() as i32 {
            return 0;
        }
        let expr_op = &(*(*ctxt).comp).steps[(*op).ch2 as usize];

        if matches!(expr_op.op, XmlXPathOp::XPathOpValue)
            && !expr_op.value4.is_null()
            && matches!(
                (*(expr_op.value4 as XmlXPathObjectPtr)).typ,
                XmlXPathObjectType::XPathNumber
            )
        {
            let floatval: f64 = (*(expr_op.value4 as XmlXPathObjectPtr)).floatval;

            // We have a "[n]" predicate here.
            // TODO: Unfortunately this simplistic test here is not
            // able to detect a position() predicate in compound
            // expressions like "[@attr = 'a" and position() = 1],
            // and even not the usage of position() in
            // "[position() = 1]"; thus - obviously - a position-range,
            // like it "[position() < 5]", is also not detected.
            // Maybe we could rewrite the AST to ease the optimization.

            if floatval > INT_MIN as f64 && floatval < INT_MAX as f64 {
                *max_pos = floatval as i32;
                if floatval == *max_pos as f64 {
                    return 1;
                }
            }
        }
        0
    }
}

/// Filter a node set, keeping only nodes for which the predicate expression
/// matches. Afterwards, keep only nodes between minPos and maxPos in the
/// filtered result.
#[doc(alias = "xmlXPathNodeSetFilter")]
pub(super) unsafe fn xml_xpath_node_set_filter(
    ctxt: XmlXPathParserContextPtr,
    set: Option<&mut XmlNodeSet>,
    filter_op_index: i32,
    min_pos: i32,
    max_pos: i32,
    has_ns_nodes: bool,
) {
    unsafe {
        let Some(set) = set.filter(|s| !s.is_empty()) else {
            return;
        };

        // Check if the node set contains a sufficient number of nodes for
        // the requested range.
        if (set.node_tab.len() as i32) < min_pos {
            set.clear(has_ns_nodes);
            return;
        }

        let xpctxt: XmlXPathContextPtr = (*ctxt).context;
        let oldnode = (*xpctxt).node;
        let olddoc = (*xpctxt).doc;
        let oldcs: i32 = (*xpctxt).context_size;
        let oldpp: i32 = (*xpctxt).proximity_position;

        (*xpctxt).context_size = set.node_tab.len() as i32;

        let mut i = 0;
        let mut j = 0;
        let mut pos = 1;
        while i < set.node_tab.len() {
            let node = set.node_tab[i];

            (*xpctxt).node = Some(node);
            (*xpctxt).proximity_position = i as i32 + 1;

            // Also set the xpath document in case things like
            // key() are evaluated in the predicate.
            //
            // TODO: Get real doc for namespace nodes.
            if !matches!(node.element_type(), XmlElementType::XmlNamespaceDecl)
                && node.document().is_some()
            {
                (*xpctxt).doc = node.document();
            }

            let res = (*ctxt).evaluate_precompiled_operation_to_boolean(
                &raw mut (*(*ctxt).comp).steps[filter_op_index as usize],
                1,
            );

            if (*ctxt).error != XmlXPathError::XPathExpressionOK as i32 {
                break;
            }
            if res < 0 {
                // Shouldn't happen
                xml_xpath_err(ctxt, XmlXPathError::XPathExprError as i32);
                break;
            }

            if res != 0 && (pos >= min_pos && pos <= max_pos) {
                if i != j {
                    set.node_tab[j] = node;
                    // set.node_tab[i] = null_mut();
                }

                j += 1;
            } else {
                // Remove the entry from the initial node set.
                // set.node_tab[i] = null_mut();
                if let Ok(ns) = XmlNsPtr::try_from(node) {
                    xml_xpath_node_set_free_ns(ns);
                }
            }

            if res != 0 {
                if pos == max_pos {
                    i += 1;
                    break;
                }

                pos += 1;
            }

            i += 1;
        }

        // Free remaining nodes.
        if has_ns_nodes {
            while i < set.node_tab.len() {
                let node = set.node_tab[i];
                if let Ok(ns) = XmlNsPtr::try_from(node) {
                    xml_xpath_node_set_free_ns(ns);
                }
                i += 1;
            }
        }

        set.node_tab.truncate(j);
        set.node_tab.shrink_to_fit();

        (*xpctxt).node = oldnode;
        (*xpctxt).doc = olddoc;
        (*xpctxt).context_size = oldcs;
        (*xpctxt).proximity_position = oldpp;
    }
}

/// Filter a node set, keeping only nodes for which the sequence of predicate
/// expressions matches. Afterwards, keep only nodes between minPos and maxPos
/// in the filtered result.
#[doc(alias = "xmlXPathCompOpEvalPredicate")]
unsafe fn xml_xpath_comp_op_eval_predicate(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    set: &mut XmlNodeSet,
    min_pos: i32,
    max_pos: i32,
    has_ns_nodes: bool,
) {
    unsafe {
        if (*op).ch1 != -1 {
            let comp: XmlXPathCompExprPtr = (*ctxt).comp;
            // Process inner predicates first.
            if !matches!(
                (*comp).steps[(*op).ch1 as usize].op,
                XmlXPathOp::XPathOpPredicate
            ) {
                generic_error!("xmlXPathCompOpEvalPredicate: Expected a predicate\n");
                XP_ERROR!(ctxt, XmlXPathError::XPathInvalidOperand as i32);
            }
            if (*(*ctxt).context).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
                XP_ERROR!(ctxt, XmlXPathError::XPathRecursionLimitExceeded as i32);
            }
            (*(*ctxt).context).depth += 1;
            xml_xpath_comp_op_eval_predicate(
                ctxt,
                &raw mut (*comp).steps[(*op).ch1 as usize],
                set,
                1,
                set.node_tab.len() as i32,
                has_ns_nodes,
            );
            (*(*ctxt).context).depth -= 1;
            CHECK_ERROR!(ctxt);
        }

        if (*op).ch2 != -1 {
            xml_xpath_node_set_filter(ctxt, Some(set), (*op).ch2, min_pos, max_pos, has_ns_nodes);
        }
    }
}

macro_rules! axis_range_end {
    ($out_seq:expr, $seq:expr, $merge_and_clear:ident, $to_bool:expr, $label:tt) => {
        // We have a "/foo[n]", and position() = n was reached.
        // Note that we can have as well "/foo/::parent::foo[1]", so
        // a duplicate-aware merge is still needed.
        // Merge with the result.
        if $out_seq.is_none() {
            $out_seq = $seq;
            $seq = None;
        } else {
            // TODO: Check memory error.
            $out_seq = $merge_and_clear($out_seq, $seq.as_deref_mut());
        }
        // Break if only a true/false result was requested.
        if $to_bool != 0 {
            break $label;
        }
        continue $label;
    }
}

macro_rules! first_hit {
    ($out_seq:expr, $seq:expr, $merge_and_clear:expr, $label:tt) => {
        // Break if only a true/false result was requested and
        // no predicates existed and a node test succeeded.
        if $out_seq.is_none() {
            $out_seq = $seq;
            $seq = None;
        } else {
            // TODO: Check memory error.
            $out_seq = $merge_and_clear($out_seq, $seq.as_deref_mut());
        }
        break $label;
    };
}

macro_rules! xp_test_hit {
    (
        $has_axis_range:expr,
        $pos:expr,
        $max_pos:expr,
        $seq:expr,
        $cur:expr,
        $ctxt:expr,
        $out_seq:expr,
        $merge_and_clear:ident,
        $to_bool:expr,
        $break_on_first_hit:expr,
        $label:tt
    ) => {
        if $has_axis_range != 0 {
            $pos += 1;
            if $pos == $max_pos {
                if $seq.as_deref_mut().map_or(-1, |seq| seq.add_unique($cur)) < 0 {
                    (*$ctxt).error = XmlXPathError::XPathMemoryError as i32;
                }
                axis_range_end!($out_seq, $seq, $merge_and_clear, $to_bool, $label);
            }
        } else {
            if $seq.as_deref_mut().map_or(-1, |seq| seq.add_unique($cur)) < 0 {
                (*$ctxt).error = XmlXPathError::XPathMemoryError as i32;
            }
            if $break_on_first_hit != 0 {
                first_hit!($out_seq, $seq, $merge_and_clear, $label);
            }
        }
    };
}

macro_rules! xp_test_hit_ns {
    (
        $has_axis_range:expr,
        $pos:expr,
        $max_pos:expr,
        $has_ns_nodes:expr,
        $seq:expr,
        $xpctxt:expr,
        $cur:expr,
        $ctxt:expr,
        $out_seq:expr,
        $merge_and_clear:ident,
        $to_bool:expr,
        $break_on_first_hit:expr,
        $label:tt
    ) => {
        #[allow(unused_assignments)]
        if $has_axis_range != 0 {
            $pos += 1;
            if $pos == $max_pos {
                $has_ns_nodes = true;
                if $seq.as_deref_mut().map_or(-1, |seq| {
                    seq.add_ns(
                        XmlNodePtr::try_from((*$xpctxt).node.unwrap()).unwrap(),
                        XmlNsPtr::try_from($cur).unwrap(),
                    )
                }) < 0
                {
                    (*$ctxt).error = XmlXPathError::XPathMemoryError as i32;
                }
                axis_range_end!($out_seq, $seq, $merge_and_clear, $to_bool, $label);
            }
        } else {
            $has_ns_nodes = true;
            if $seq.as_deref_mut().map_or(-1, |seq| {
                seq.add_ns(
                    XmlNodePtr::try_from((*$xpctxt).node.unwrap()).unwrap(),
                    XmlNsPtr::try_from($cur).unwrap(),
                )
            }) < 0
            {
                (*$ctxt).error = XmlXPathError::XPathMemoryError as i32;
            }
            if $break_on_first_hit != 0 {
                first_hit!($out_seq, $seq, $merge_and_clear, $label);
            }
        }
    };
}

#[doc(alias = "xmlXPathNodeCollectAndTest")]
pub(super) unsafe fn xml_xpath_node_collect_and_test(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    mut first: Option<&mut Option<XmlGenericNodePtr>>,
    mut last: Option<&mut Option<XmlGenericNodePtr>>,
    to_bool: i32,
) -> i32 {
    unsafe {
        let axis: XmlXPathAxisVal = (*op).value.try_into().unwrap();
        let test: XmlXPathTestVal = (*op).value2.try_into().unwrap();
        let typ: XmlXPathTypeVal = (*op).value3.try_into().unwrap();
        let prefix: *const XmlChar = (*op).value4 as _;
        let name: *const XmlChar = (*op).value5 as _;
        let mut uri: *const XmlChar = null();
        let mut total: i32 = 0;
        let mut has_ns_nodes: bool;

        // First predicate operator
        let mut pred_op: XmlXPathStepOpPtr;
        let mut max_pos: i32; /* The requested position() (when a "[n]" predicate) */
        let mut has_predicate_range: i32;
        let mut has_axis_range: i32;
        let mut pos: i32;

        let next: Option<XmlXPathTraversalFunction>;
        let xpctxt: XmlXPathContextPtr = (*ctxt).context;

        CHECK_TYPE0!(ctxt, XmlXPathObjectType::XPathNodeset);
        // The popped object holding the context nodes
        let obj: XmlXPathObjectPtr = (*ctxt).value_pop();
        // Setup namespaces.
        if !prefix.is_null() {
            uri = xml_xpath_ns_lookup(xpctxt, prefix);
            if uri.is_null() {
                xml_xpath_release_object(xpctxt, obj);
                XP_ERROR0!(ctxt, XmlXPathError::XPathUndefPrefixError as i32);
            }
        }
        // Setup axis.
        //
        // MAYBE FUTURE TODO: merging optimizations:
        // - If the nodes to be traversed wrt to the initial nodes and
        //   the current axis cannot overlap, then we could avoid searching
        //   for duplicates during the merge.
        //   But the question is how/when to evaluate if they cannot overlap.
        //   Example: if we know that for two initial nodes, the one is
        //   not in the ancestor-or-self axis of the other, then we could safely
        //   avoid a duplicate-aware merge, if the axis to be traversed is e.g.
        //   the descendant-or-self axis.
        let mut merge_and_clear: XmlXPathNodeSetMergeFunction = xml_xpath_node_set_merge_and_clear;
        match axis {
            XmlXPathAxisVal::AxisAncestor => {
                first = None;
                next = Some(xml_xpath_next_ancestor);
            }
            XmlXPathAxisVal::AxisAncestorOrSelf => {
                first = None;
                next = Some(xml_xpath_next_ancestor_or_self);
            }
            XmlXPathAxisVal::AxisAttribute => {
                first = None;
                last = None;
                next = Some(xml_xpath_next_attribute);
                merge_and_clear = xml_xpath_node_set_merge_and_clear_no_dupls;
            }
            XmlXPathAxisVal::AxisChild => {
                last = None;
                if matches!(
                    test,
                    XmlXPathTestVal::NodeTestName | XmlXPathTestVal::NodeTestAll
                ) && matches!(typ, XmlXPathTypeVal::NodeTypeNode)
                {
                    // Optimization if an element node type is 'element'.
                    next = Some(xml_xpath_next_child_element);
                } else {
                    next = Some(xml_xpath_next_child);
                }
                merge_and_clear = xml_xpath_node_set_merge_and_clear_no_dupls;
            }
            XmlXPathAxisVal::AxisDescendant => {
                last = None;
                next = Some(xml_xpath_next_descendant);
            }
            XmlXPathAxisVal::AxisDescendantOrSelf => {
                last = None;
                next = Some(xml_xpath_next_descendant_or_self);
            }
            XmlXPathAxisVal::AxisFollowing => {
                last = None;
                next = Some(xml_xpath_next_following);
            }
            XmlXPathAxisVal::AxisFollowingSibling => {
                last = None;
                next = Some(xml_xpath_next_following_sibling);
            }
            XmlXPathAxisVal::AxisNamespace => {
                first = None;
                last = None;
                next = Some(xml_xpath_next_namespace);
                merge_and_clear = xml_xpath_node_set_merge_and_clear_no_dupls;
            }
            XmlXPathAxisVal::AxisParent => {
                first = None;
                next = Some(xml_xpath_next_parent);
            }
            XmlXPathAxisVal::AxisPreceding => {
                first = None;
                next = Some(xml_xpath_next_preceding_internal);
            }
            XmlXPathAxisVal::AxisPrecedingSibling => {
                first = None;
                next = Some(xml_xpath_next_preceding_sibling);
            }
            XmlXPathAxisVal::AxisSelf => {
                first = None;
                last = None;
                next = Some(xml_xpath_next_self);
                merge_and_clear = xml_xpath_node_set_merge_and_clear_no_dupls;
            }
        }

        let Some(next) = next else {
            xml_xpath_release_object(xpctxt, obj);
            return 0;
        };
        // The set of context nodes for the node tests
        let Some(context_seq) = (*obj).nodesetval.as_deref().filter(|n| !n.is_empty()) else {
            xml_xpath_release_object(xpctxt, obj);
            (*ctxt).value_push(xml_xpath_cache_wrap_node_set(xpctxt, None));
            return 0;
        };
        // Predicate optimization ---------------------------------------------
        // If this step has a last predicate, which contains a position(),
        // then we'll optimize (although not exactly "position()", but only
        // the  short-hand form, i.e., "[n]".
        //
        // Example - expression "/foo[parent::bar][1]":
        //
        // COLLECT 'child' 'name' 'node' foo    -- op (we are here)
        //   ROOT                               -- (*op).ch1
        //   PREDICATE                          -- (*op).ch2 (predOp)
        //     PREDICATE                          -- (*predOp).ch1 = [parent::bar]
        //       SORT
        //         COLLECT  'parent' 'name' 'node' bar
        //           NODE
        //     ELEM Object is a number : 1        -- (*predOp).ch2 = [1]
        //
        max_pos = 0;
        pred_op = null_mut();
        has_predicate_range = 0;
        has_axis_range = 0;
        if (*op).ch2 != -1 {
            // There's at least one predicate. 16 == XPATH_OP_PREDICATE
            pred_op = &raw mut (*(*ctxt).comp).steps[(*op).ch2 as usize];
            if xml_xpath_is_positional_predicate(ctxt, pred_op, addr_of_mut!(max_pos)) != 0 {
                if (*pred_op).ch1 != -1 {
                    // Use the next inner predicate operator.
                    pred_op = &raw mut (*(*ctxt).comp).steps[(*pred_op).ch1 as usize];
                    has_predicate_range = 1;
                } else {
                    // There's no other predicate than the [n] predicate.
                    pred_op = null_mut();
                    has_axis_range = 1;
                }
            }
        }
        let break_on_first_hit: i32 = (to_bool != 0 && pred_op.is_null()) as i32;
        // Axis traversal -----------------------------------------------------
        // 2.3 Node Tests
        //  - For the attribute axis, the principal node type is attribute.
        //  - For the namespace axis, the principal node type is namespace.
        //  - For other axes, the principal node type is element.
        //
        // A node test * is true for any node of the
        // principal node type. For example, child::* will
        // select all element children of the context node
        let old_context_node = (*xpctxt).node;
        // The final resulting node set wrt to all context nodes
        let mut out_seq = None;
        // Used to feed predicate evaluation.
        let mut seq = None;
        let mut context_idx = 0;

        'main: while context_idx < context_seq.node_tab.len()
            && (*ctxt).error == XmlXPathError::XPathExpressionOK as i32
        {
            (*xpctxt).node = Some(context_seq.node_tab[context_idx]);
            context_idx += 1;

            if seq.is_none() {
                seq = xml_xpath_node_set_create(None);
                if seq.is_none() {
                    // TODO: Propagate memory error.
                    total = 0;
                    // goto error;
                    break 'main;
                }
            }
            // Traverse the axis and test the nodes.
            pos = 0;
            let mut cur = None;
            has_ns_nodes = false;
            while let Some(cur) = {
                if (*(*ctxt).context).op_limit != 0 && (*ctxt).check_operation_limit(1) < 0 {
                    // goto error;
                    break 'main;
                }

                cur = next(ctxt, cur);
                cur
            } {
                // QUESTION TODO: What does the "first" and "last" stuff do?
                if let Some(first) = first.as_mut().filter(|first| !first.is_none()) {
                    if **first == Some(cur) {
                        break;
                    }
                    if total % 256 == 0
                        && xml_xpath_cmp_nodes_ext((**first).unwrap(), cur)
                            .is_some_and(|f| f.is_le())
                    {
                        break;
                    }
                }
                if let Some(last) = last.as_mut().filter(|last| !last.is_none()) {
                    if **last == Some(cur) {
                        break;
                    }
                    if total % 256 == 0
                        && xml_xpath_cmp_nodes_ext(cur, (**last).unwrap())
                            .is_some_and(|f| f.is_le())
                    {
                        break;
                    }
                }

                total += 1;

                match test {
                    XmlXPathTestVal::NodeTestNone => {
                        total = 0;
                        generic_error!("Internal error at {}:{}\n", file!(), line!());
                        // goto error;
                        break 'main;
                    }
                    XmlXPathTestVal::NodeTestType => {
                        if matches!(typ, XmlXPathTypeVal::NodeTypeNode) {
                            match (*cur).element_type() {
                                XmlElementType::XmlDocumentNode
                                | XmlElementType::XmlHTMLDocumentNode
                                | XmlElementType::XmlElementNode
                                | XmlElementType::XmlAttributeNode
                                | XmlElementType::XmlPINode
                                | XmlElementType::XmlCommentNode
                                | XmlElementType::XmlCDATASectionNode
                                | XmlElementType::XmlTextNode => {
                                    xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                }
                                XmlElementType::XmlNamespaceDecl => {
                                    if matches!(axis, XmlXPathAxisVal::AxisNamespace) {
                                        xp_test_hit_ns!(has_axis_range, pos, max_pos, has_ns_nodes, seq, xpctxt, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    } else {
                                        has_ns_nodes = true;
                                        xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    }
                                }
                                _ => {}
                            }
                        } else if (*cur).element_type() as isize == typ as isize {
                            if matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
                                xp_test_hit_ns!(has_axis_range, pos, max_pos, has_ns_nodes, seq, xpctxt, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                            } else {
                                xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                            }
                        } else if matches!(typ, XmlXPathTypeVal::NodeTypeText)
                            && matches!((*cur).element_type(), XmlElementType::XmlCDATASectionNode)
                        {
                            xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                        }
                    }
                    XmlXPathTestVal::NodeTestPI => {
                        if matches!((*cur).element_type(), XmlElementType::XmlPINode)
                            && (name.is_null()
                                || xml_str_equal(name, XmlNodePtr::try_from(cur).unwrap().name))
                        {
                            xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                        }
                    }
                    XmlXPathTestVal::NodeTestAll => {
                        if matches!(axis, XmlXPathAxisVal::AxisAttribute) {
                            if matches!(cur.element_type(), XmlElementType::XmlAttributeNode)
                                && (prefix.is_null()
                                    || XmlAttrPtr::try_from(cur)
                                        .unwrap()
                                        .ns
                                        .is_some_and(|ns| xml_str_equal(uri, ns.href)))
                            {
                                xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                            }
                        } else if matches!(axis, XmlXPathAxisVal::AxisNamespace) {
                            if matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl) {
                                xp_test_hit_ns!(has_axis_range, pos, max_pos, has_ns_nodes, seq, xpctxt, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                            }
                        } else if matches!(cur.element_type(), XmlElementType::XmlElementNode)
                            && (prefix.is_null()
                                || XmlNodePtr::try_from(cur)
                                    .unwrap()
                                    .ns
                                    .is_some_and(|ns| xml_str_equal(uri, ns.href)))
                        {
                            xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                        }
                    }
                    XmlXPathTestVal::NodeTestNs => {
                        // todo!();
                    }
                    XmlXPathTestVal::NodeTestName => 'to_break: {
                        if matches!(axis, XmlXPathAxisVal::AxisAttribute) {
                            if !matches!(cur.element_type(), XmlElementType::XmlAttributeNode) {
                                break 'to_break;
                            }
                        } else if matches!(axis, XmlXPathAxisVal::AxisNamespace) {
                            if !matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl) {
                                break 'to_break;
                            }
                        } else if !matches!(cur.element_type(), XmlElementType::XmlElementNode) {
                            break 'to_break;
                        }
                        match cur.element_type() {
                            XmlElementType::XmlElementNode => {
                                let node = XmlNodePtr::try_from(cur).unwrap();
                                if xml_str_equal(name, node.name) {
                                    if prefix.is_null() {
                                        if node.ns.is_none() {
                                            xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                        }
                                    } else if node.ns.is_some_and(|ns| xml_str_equal(uri, ns.href))
                                    {
                                        xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    }
                                }
                            }
                            XmlElementType::XmlAttributeNode => {
                                let attr = XmlAttrPtr::try_from(cur).unwrap();

                                if xml_str_equal(name, attr.name) {
                                    if prefix.is_null() {
                                        if attr.ns.is_none_or(|ns| ns.prefix().is_none()) {
                                            xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                        }
                                    } else if attr.ns.is_some_and(|ns| xml_str_equal(uri, ns.href))
                                    {
                                        xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    }
                                }
                            }
                            XmlElementType::XmlNamespaceDecl => {
                                let ns = XmlNsPtr::try_from(cur).unwrap();
                                if ns.prefix().is_some()
                                    && !name.is_null()
                                    && xml_str_equal(ns.prefix, name)
                                {
                                    xp_test_hit_ns!(has_axis_range, pos, max_pos, has_ns_nodes, seq, xpctxt, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                }
                            }
                            _ => {}
                        }
                    }
                }

                if (*ctxt).error != XmlXPathError::XPathExpressionOK as i32 {
                    break;
                }
            }

            // goto apply_predicates;

            // apply_predicates: /* --------------------------------------------------- */
            if (*ctxt).error != XmlXPathError::XPathExpressionOK as i32 {
                // goto error;
                break 'main;
            }

            // Apply predicates.
            if !pred_op.is_null() && !seq.as_deref().unwrap().node_tab.is_empty() {
                // E.g. when we have a "/foo[some expression][n]".
                // QUESTION TODO: The old predicate evaluation took into
                // account location-sets.
                // (E.g. (*(*ctxt).value).typ == XPATH_LOCATIONSET)
                // Do we expect such a set here?
                // All what I learned now from the evaluation semantics
                // does not indicate that a location-set will be processed
                // here, so this looks OK.
                // Iterate over all predicates, starting with the outermost predicate.
                // TODO: Problem: we cannot execute the inner predicates first
                //  since we cannot go back *up* the operator tree!
                //  Options we have:
                //  1) Use of recursive functions (like is it currently done
                //     via xmlXPathCompOpEval())
                //  2) Add a predicate evaluation information stack to the
                //     context struct
                //  3) Change the way the operators are linked; we need a
                //     "parent" field on xmlXPathStepOp
                //
                // For the moment, I'll try to solve this with a recursive
                // function: xmlXPathCompOpEvalPredicate().
                if has_predicate_range != 0 {
                    xml_xpath_comp_op_eval_predicate(
                        ctxt,
                        pred_op,
                        seq.as_deref_mut().unwrap(),
                        max_pos,
                        max_pos,
                        has_ns_nodes,
                    );
                } else {
                    let max_pos = seq.as_deref().unwrap().node_tab.len() as i32;
                    xml_xpath_comp_op_eval_predicate(
                        ctxt,
                        pred_op,
                        seq.as_deref_mut().unwrap(),
                        1,
                        max_pos,
                        has_ns_nodes,
                    );
                }

                if (*ctxt).error != XmlXPathError::XPathExpressionOK as i32 {
                    total = 0;
                    // goto error;
                    break 'main;
                }
            }

            if !seq.as_deref().unwrap().node_tab.is_empty() {
                // Add to result set.
                if out_seq.is_none() {
                    out_seq = seq.take();
                } else {
                    // TODO: Check memory error.
                    out_seq = merge_and_clear(out_seq, seq.as_deref_mut());
                }

                if to_bool != 0 {
                    break 'main;
                }
            }
            continue 'main;
        }

        // error:
        if (*obj).boolval && !(*obj).user.is_null() {
            // QUESTION TODO: What does this do and why?
            // TODO: Do we have to do this also for the "error"
            // cleanup further down?
            (*(*ctxt).value).boolval = true;
            (*(*ctxt).value).user = (*obj).user;
            (*obj).user = null_mut();
            (*obj).boolval = false;
        }
        xml_xpath_release_object(xpctxt, obj);

        // Ensure we return at least an empty set.
        if out_seq.is_none() {
            if seq.as_deref().is_some_and(|seq| seq.is_empty()) {
                out_seq = seq.take();
            } else {
                // TODO: Check memory error.
                out_seq = xml_xpath_node_set_create(None);
            }
        }
        if seq.is_some() {
            xml_xpath_free_node_set(seq);
        }
        // Hand over the result. Better to push the set also in case of errors.
        (*ctxt).value_push(xml_xpath_cache_wrap_node_set(xpctxt, out_seq));
        // Reset the context node.
        (*xpctxt).node = old_context_node;
        // When traversing the namespace axis in "toBool" mode, it's
        // possible that tmpNsList wasn't freed.
        (*xpctxt).tmp_ns_list = None;

        total
    }
}

/// Move the last node to the first position and clear temporary XPath objects
/// (e.g. namespace nodes) from all other nodes. Sets the length of the list to 1.
#[doc(alias = "xmlXPathNodeSetKeepLast")]
pub(super) unsafe fn xml_xpath_node_set_keep_last(set: Option<&mut XmlNodeSet>) {
    unsafe {
        let Some(set) = set.filter(|s| s.len() > 1) else {
            return;
        };
        if set.node_tab.len() <= 1 {
            return;
        }
        let len = set.node_tab.len();
        for node in set.node_tab.drain(..len - 1) {
            if let Ok(ns) = XmlNsPtr::try_from(node) {
                xml_xpath_node_set_free_ns(ns);
            }
        }
    }
}

/// Filter a location set, keeping only nodes for which the predicate expression matches.  
/// Afterwards, keep only nodes between minPos and maxPos in the filtered result.
#[doc(alias = "xmlXPathLocationSetFilter")]
#[cfg(feature = "libxml_xptr_locs")]
pub(super) unsafe fn xml_xpath_location_set_filter(
    ctxt: XmlXPathParserContextPtr,
    locset: XmlLocationSetPtr,
    filter_op_index: i32,
    min_pos: i32,
    max_pos: i32,
) {
    unsafe {
        if locset.is_null() || (*locset).loc_tab.is_empty() || filter_op_index == -1 {
            return;
        }

        let xpctxt: XmlXPathContextPtr = (*ctxt).context;
        let oldnode = (*xpctxt).node;
        let olddoc = (*xpctxt).doc;
        let oldcs: i32 = (*xpctxt).context_size;
        let oldpp: i32 = (*xpctxt).proximity_position;

        (*xpctxt).context_size = (*locset).loc_tab.len() as i32;

        let mut i = 0;
        let mut j = 0;
        let mut pos = 1;
        while i < (*locset).loc_tab.len() {
            let context_node =
                XmlGenericNodePtr::from_raw((*(*locset).loc_tab[i]).user as *mut XmlNode).unwrap();

            (*xpctxt).node = Some(context_node);
            (*xpctxt).proximity_position = i as i32 + 1;

            // Also set the xpath document in case things like
            // key() are evaluated in the predicate.
            //
            // TODO: Get real doc for namespace nodes.
            if !matches!(
                context_node.element_type(),
                XmlElementType::XmlNamespaceDecl
            ) && context_node.document().is_some()
            {
                (*xpctxt).doc = context_node.document();
            }

            let res: i32 = (*ctxt).evaluate_precompiled_operation_to_boolean(
                &raw mut (*(*ctxt).comp).steps[filter_op_index as usize],
                1,
            );

            if (*ctxt).error != XmlXPathError::XPathExpressionOK as i32 {
                break;
            }
            if res < 0 {
                // Shouldn't happen
                xml_xpath_err(ctxt, XmlXPathError::XPathExprError as i32);
                break;
            }

            if res != 0 && (pos >= min_pos && pos <= max_pos) {
                if i != j {
                    (*locset).loc_tab[j] = (*locset).loc_tab[i];
                    (*locset).loc_tab[i] = null_mut();
                }

                j += 1;
            } else {
                /* Remove the entry from the initial location set. */
                xml_xpath_free_object((*locset).loc_tab[i]);
                (*locset).loc_tab[i] = null_mut();
            }

            if res != 0 {
                if pos == max_pos {
                    i += 1;
                    break;
                }

                pos += 1;
            }

            i += 1;
        }

        // Free remaining nodes.
        while i < (*locset).loc_tab.len() {
            xml_xpath_free_object((*locset).loc_tab[i]);
            i += 1;
        }

        (*locset).loc_tab.truncate(j);
        // If too many elements were removed, shrink table to preserve memory.
        (*locset).loc_tab.shrink_to(XML_NODESET_DEFAULT);

        (*xpctxt).node = oldnode;
        (*xpctxt).doc = olddoc;
        (*xpctxt).context_size = oldcs;
        (*xpctxt).proximity_position = oldpp;
    }
}

pub(super) const MAX_FRAC: usize = 20;

/// ```text
/// [30a]  Float  ::= Number ('e' Digits?)?
///
/// [30]   Number ::= Digits ('.' Digits?)? | '.' Digits
/// [31]   Digits ::= [0-9]+
/// ```
///
/// Compile a Number in the string
/// In complement of the Number expression, this function also handles
/// negative values : '-' Number.
///
/// Returns the let value: f64.
#[doc(alias = "xmlXPathStringEvalNumber")]
pub unsafe fn xml_xpath_string_eval_number(str: *const XmlChar) -> f64 {
    unsafe {
        let mut cur: *const XmlChar = str;
        let mut ret: f64;
        let mut ok: i32 = 0;
        let mut isneg: i32 = 0;
        let mut exponent: i32 = 0;
        let mut is_exponent_negative: i32 = 0;
        if cur.is_null() {
            return 0.;
        }
        while xml_is_blank_char(*cur as u32) {
            cur = cur.add(1);
        }
        if *cur == b'-' {
            isneg = 1;
            cur = cur.add(1);
        }
        if *cur != b'.' && (*cur < b'0' || *cur > b'9') {
            return XML_XPATH_NAN;
        }

        ret = 0.0;
        while *cur >= b'0' && *cur <= b'9' {
            ret = ret * 10. + (*cur - b'0') as f64;
            ok = 1;
            cur = cur.add(1);
        }

        if *cur == b'.' {
            let mut v: i32;
            let mut frac: i32 = 0;
            let mut fraction: f64 = 0.0;

            cur = cur.add(1);
            if (*cur < b'0' || *cur > b'9') && ok == 0 {
                return XML_XPATH_NAN;
            }
            while *cur == b'0' {
                frac += 1;
                cur = cur.add(1);
            }
            let max: i32 = frac + MAX_FRAC as i32;
            while *cur >= b'0' && *cur <= b'9' && frac < max {
                v = (*cur - b'0') as _;
                fraction = fraction * 10. + v as f64;
                frac += 1;
                cur = cur.add(1);
            }
            fraction /= 10.0f64.powi(frac);
            ret += fraction;
            while *cur >= b'0' && *cur <= b'9' {
                cur = cur.add(1);
            }
        }
        if *cur == b'e' || *cur == b'E' {
            cur = cur.add(1);
            if *cur == b'-' {
                is_exponent_negative = 1;
                cur = cur.add(1);
            } else if *cur == b'+' {
                cur = cur.add(1);
            }
            while *cur >= b'0' && *cur <= b'9' {
                if exponent < 1000000 {
                    exponent = exponent * 10 + (*cur - b'0') as i32;
                }
                cur = cur.add(1);
            }
        }
        while xml_is_blank_char(*cur as u32) {
            cur = cur.add(1);
        }
        if *cur != 0 {
            return XML_XPATH_NAN;
        }
        if isneg != 0 {
            ret = -ret;
        }
        if is_exponent_negative != 0 {
            exponent = -exponent;
        }
        ret *= 10.0f64.powi(exponent);
        ret
    }
}

/// Evaluate a predicate result for the current node.
/// A PredicateExpr is evaluated by evaluating the Expr and converting
/// the result to a boolean. If the result is a number, the result will
/// be converted to true if the number is equal to the position of the
/// context node in the context node list (as returned by the position
/// function) and will be converted to false otherwise; if the result
/// is not a number, then the result will be converted as if by a call
/// to the boolean function.
///
/// Returns 1 if predicate is true, 0 otherwise
#[doc(alias = "xmlXPathEvaluatePredicateResult")]
pub unsafe fn xml_xpath_evaluate_predicate_result(
    ctxt: XmlXPathParserContextPtr,
    res: XmlXPathObjectPtr,
) -> i32 {
    unsafe {
        if ctxt.is_null() || res.is_null() {
            return 0;
        }
        match (*res).typ {
            XmlXPathObjectType::XPathBoolean => {
                return (*res).boolval as i32;
            }
            XmlXPathObjectType::XPathNumber => {
                return ((*res).floatval == (*(*ctxt).context).proximity_position as f64) as i32;
            }
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
                if let Some(nodeset) = (*res).nodesetval.as_deref() {
                    return !nodeset.is_empty() as i32;
                } else {
                    return 0;
                }
            }
            XmlXPathObjectType::XPathString => {
                return (*res).stringval.as_deref().is_some_and(|s| !s.is_empty()) as i32;
            }
            #[cfg(feature = "libxml_xptr_locs")]
            XmlXPathObjectType::XPathLocationset => {
                let ptr: XmlLocationSetPtr = (*res).user as XmlLocationSetPtr;
                if ptr.is_null() {
                    return 0;
                }
                return (!(*ptr).loc_tab.is_empty()) as i32;
            }
            _ => {
                generic_error!("Internal error at {}:{}\n", file!(), line!());
            }
        }
        0
    }
}

/// This is the cached version of xmlXPathWrapString().
/// Wraps the @val string into an XPath object.
///
/// Returns the created or reused object.
#[doc(alias = "xmlXPathCacheWrapString")]
unsafe fn xml_xpath_cache_wrap_string(
    ctxt: XmlXPathContextPtr,
    val: Option<&str>,
) -> XmlXPathObjectPtr {
    unsafe {
        if !ctxt.is_null() && !(*ctxt).cache.is_null() {
            let cache: XmlXPathContextCachePtr = (*ctxt).cache as XmlXPathContextCachePtr;

            if !(*cache).string_objs.is_null() && (*(*cache).string_objs).number != 0 {
                (*(*cache).string_objs).number -= 1;
                let ret: XmlXPathObjectPtr = *(*(*cache).string_objs)
                    .items
                    .add((*(*cache).string_objs).number as usize)
                    as XmlXPathObjectPtr;
                (*ret).typ = XmlXPathObjectType::XPathString;
                (*ret).stringval = val.map(|s| s.to_owned());
                return ret;
            } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
                // Fallback to misc-cache.
                (*(*cache).misc_objs).number -= 1;
                let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                    .items
                    .add((*(*cache).misc_objs).number as usize)
                    as XmlXPathObjectPtr;

                (*ret).typ = XmlXPathObjectType::XPathString;
                (*ret).stringval = val.map(|s| s.to_owned());
                return ret;
            }
        }
        xml_xpath_wrap_string(val)
    }
}

/// Implement the name() XPath function
///    string name(node-set?)
/// The name function returns a string containing a QName representing
/// the name of the node in the argument node-set that is first in document
/// order. The QName must represent the name with respect to the namespace
/// declarations in effect on the node whose name is being represented.
/// Typically, this will be the form in which the name occurred in the XML
/// source. This need not be the case if there are namespace declarations
/// in effect on the node that associate multiple prefixes with the same
/// namespace. However, an implementation may include information about
/// the original prefix in its representation of nodes; in this case, an
/// implementation can ensure that the returned string is always the same
/// as the QName used in the XML source. If the argument it omitted it
/// defaults to the context node.
/// Libxml keep the original prefix so the "real qualified name" used is returned.
#[doc(alias = "xmlXPathNameFunction")]
pub(super) unsafe fn xml_xpath_name_function(ctxt: XmlXPathParserContextPtr, mut nargs: i32) {
    unsafe {
        if nargs == 0 {
            (*ctxt).value_push(xml_xpath_cache_new_node_set(
                (*ctxt).context,
                (*(*ctxt).context).node,
            ));
            nargs = 1;
        }

        CHECK_ARITY!(ctxt, nargs, 1);
        if (*ctxt).value.is_null()
            || !matches!(
                (*(*ctxt).value).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidType as i32);
        }
        let cur: XmlXPathObjectPtr = (*ctxt).value_pop();

        if let Some(nodeset) = (*cur).nodesetval.as_deref() {
            if !nodeset.node_tab.is_empty() {
                let table = &nodeset.node_tab;
                let i = 0; /* Should be first in document order !!!!! */

                match table[i].element_type() {
                    XmlElementType::XmlElementNode => {
                        let node = XmlNodePtr::try_from(table[i]).unwrap();
                        if *node.name.add(0) == b' ' {
                            (*ctxt)
                                .value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
                        } else if let Some(prefix) =
                            node.ns.map(|ns| ns.prefix).filter(|p| !p.is_null())
                        {
                            let mut fullname = xml_build_qname(node.name, prefix, null_mut(), 0);
                            if fullname == node.name as _ {
                                fullname = xml_strdup(node.name);
                            }
                            if fullname.is_null() {
                                xml_xpath_perr_memory(ctxt, None);
                            }
                            (*ctxt).value_push(xml_xpath_cache_wrap_string(
                                (*ctxt).context,
                                (!fullname.is_null())
                                    .then(|| {
                                        CStr::from_ptr(fullname as *const i8).to_string_lossy()
                                    })
                                    .as_deref(),
                            ));
                            xml_free(fullname as _);
                        } else {
                            (*ctxt).value_push(xml_xpath_cache_new_string(
                                (*ctxt).context,
                                node.name().as_deref(),
                            ));
                        }
                    }
                    XmlElementType::XmlAttributeNode => {
                        let attr = XmlAttrPtr::try_from(table[i]).unwrap();
                        if *attr.name.add(0) == b' ' {
                            (*ctxt)
                                .value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
                        } else if let Some(prefix) =
                            attr.ns.map(|ns| ns.prefix).filter(|p| !p.is_null())
                        {
                            let mut fullname = xml_build_qname(attr.name, prefix, null_mut(), 0);
                            if fullname == attr.name as _ {
                                fullname = xml_strdup(attr.name);
                            }
                            if fullname.is_null() {
                                xml_xpath_perr_memory(ctxt, None);
                            }
                            (*ctxt).value_push(xml_xpath_cache_wrap_string(
                                (*ctxt).context,
                                (!fullname.is_null())
                                    .then(|| {
                                        CStr::from_ptr(fullname as *const i8).to_string_lossy()
                                    })
                                    .as_deref(),
                            ));
                            xml_free(fullname as _);
                        } else {
                            (*ctxt).value_push(xml_xpath_cache_new_string(
                                (*ctxt).context,
                                attr.name().as_deref(),
                            ));
                        }
                    }
                    _ => {
                        (*ctxt).value_push(xml_xpath_cache_new_node_set(
                            (*ctxt).context,
                            Some(table[i]),
                        ));
                        xml_xpath_local_name_function(ctxt, 1);
                    }
                }
            } else {
                (*ctxt).value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
            }
        } else {
            (*ctxt).value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
        }
        xml_xpath_release_object((*ctxt).context, cur);
    }
}

/// Implement the escape-uri() XPath function
///    string escape-uri(string $str, bool $escape-reserved)
///
/// This function applies the URI escaping rules defined in section 2 of [RFC
/// 2396] to the string supplied as $uri-part, which typically represents all
/// or part of a URI. The effect of the function is to replace any special
/// character in the string by an escape sequence of the form %xx%yy...,
/// where xxyy... is the hexadecimal representation of the octets used to
/// represent the character in UTF-8.
///
/// The set of characters that are escaped depends on the setting of the
/// boolean argument $escape-reserved.
///
/// If $escape-reserved is true, all characters are escaped other than lower
/// case letters a-z, upper case letters A-Z, digits 0-9, and the characters
/// referred to in [RFC 2396] as "marks": specifically, "-" | "_" | "." | "!"
/// | "~" | "*" | "'" | "(" | ")". The "%" character itself is escaped only
/// if it is not followed by two hexadecimal digits (that is, 0-9, a-f, and A-F).
///
/// If $escape-reserved is false, the behavior differs in that characters
/// referred to in [RFC 2396] as reserved characters are not escaped. These
/// characters are ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | ",".
///
/// [RFC 2396] does not define whether escaped URIs should use lower case or
/// upper case for hexadecimal digits. To ensure that escaped URIs can be
/// compared using string comparison functions, this function must always use
/// the upper-case letters A-F.
///
/// Generally, $escape-reserved should be set to true when escaping a string
/// that is to form a single part of a URI, and to false when escaping an
/// entire URI or URI reference.
///
/// In the case of non-ascii characters, the string is encoded according to
/// utf-8 and then converted according to RFC 2396.
///
/// Examples
///  xf:escape-uri ("gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles#ocean"), true())
///  returns "gopher%3A%2F%2Fspinaltap.micro.umn.edu%2F00%2FWeather%2FCalifornia%2FLos%20Angeles%23ocean"
///  xf:escape-uri ("gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles#ocean"), false())
///  returns "gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles%23ocean"
#[doc(alias = "xmlXPathEscapeUriFunction")]
pub(super) unsafe fn xml_xpath_escape_uri_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        let mut escape: [XmlChar; 4] = [0; 4];

        CHECK_ARITY!(ctxt, nargs, 2);

        let escape_reserved: i32 = (*ctxt).pop_boolean();

        CAST_TO_STRING!(ctxt);
        let str: XmlXPathObjectPtr = (*ctxt).value_pop();

        escape[0] = b'%';
        escape[3] = 0;

        let mut target = String::new();
        let cptr = (*str)
            .stringval
            .as_deref()
            .expect("Internal Error")
            .as_bytes();
        for (i, &c) in cptr.iter().enumerate() {
            if c.is_ascii_uppercase()
                || c.is_ascii_lowercase()
                || c.is_ascii_digit()
                || c == b'-'
                || c == b'_'
                || c == b'.'
                || c == b'!'
                || c == b'~'
                || c == b'*'
                || c == b'\''
                || c == b'('
                || c == b')'
                || (c == b'%'
                    && (i + 1 < cptr.len()
                        && ((cptr[i + 1] >= b'A' && cptr[i + 1] <= b'F')
                            || (cptr[i + 1] >= b'a' && cptr[i + 1] <= b'f')
                            || (cptr[i + 1] >= b'0' && cptr[i + 1] <= b'9')))
                    && (i + 2 < cptr.len()
                        && ((cptr[i + 2] >= b'A' && cptr[i + 2] <= b'F')
                            || (cptr[i + 2] >= b'a' && cptr[i + 2] <= b'f')
                            || (cptr[i + 2] >= b'0' && cptr[i + 2] <= b'9'))))
                || (escape_reserved == 0
                    && (c == b';'
                        || c == b'/'
                        || c == b'?'
                        || c == b':'
                        || c == b'@'
                        || c == b'&'
                        || c == b'='
                        || c == b'+'
                        || c == b'$'
                        || c == b','))
            {
                target.push(c as char);
            } else {
                target.push('%');
                let hi = if c >> 4 < 10 {
                    b'0' + (c >> 4)
                } else {
                    b'A' - 10 + (c >> 4)
                };
                target.push(hi as char);
                let lo = if c & 0xF < 10 {
                    b'0' + (c & 0xF)
                } else {
                    b'A' - 10 + (c & 0xF)
                };
                target.push(lo as char);
            }
        }
        (*ctxt).value_push(xml_xpath_cache_new_string((*ctxt).context, Some(&target)));
        xml_xpath_release_object((*ctxt).context, str);
    }
}

/// Function computing the beginning of the string value of the node,
/// used to speed up comparisons
///
/// Returns an int usable as a hash
#[doc(alias = "xmlXPathNodeValHash")]
unsafe fn xml_xpath_node_val_hash(node: Option<XmlGenericNodePtr>) -> u32 {
    unsafe {
        let mut len: i32 = 2;
        let mut ret: u32 = 0;

        let Some(mut node) = node else {
            return 0;
        };

        if matches!(node.element_type(), XmlElementType::XmlDocumentNode) {
            if let Some(tmp) = XmlDocPtr::try_from(node)
                .unwrap()
                .get_root_element()
                .map(|root| root.into())
            {
                node = tmp;
            } else if let Some(tmp) = node.children() {
                node = tmp;
            } else {
                return 0;
            }
        }

        let mut tmp = match node.element_type() {
            XmlElementType::XmlCommentNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlTextNode => {
                let node = XmlNodePtr::try_from(node).unwrap();
                let string = node.content;
                if string.is_null() {
                    return 0;
                }
                if *string.add(0) == 0 {
                    return 0;
                }
                return *string.add(0) as u32 + ((*string.add(1) as u32) << 8);
            }
            XmlElementType::XmlNamespaceDecl => {
                let ns = XmlNsPtr::try_from(node).unwrap();
                let string = ns.href;
                if string.is_null() {
                    return 0;
                }
                if *string.add(0) == 0 {
                    return 0;
                }
                return *string.add(0) as u32 + ((*string.add(1) as u32) << 8);
            }
            XmlElementType::XmlAttributeNode => {
                let attr = XmlAttrPtr::try_from(node).unwrap();
                attr.children.map(XmlGenericNodePtr::from)
            }
            XmlElementType::XmlElementNode => node.children(),
            _ => {
                return 0;
            }
        };
        while let Some(now) = tmp {
            let string = match now.element_type() {
                XmlElementType::XmlCDATASectionNode | XmlElementType::XmlTextNode => {
                    let node = XmlNodePtr::try_from(now).unwrap();
                    node.content
                }
                _ => null_mut(),
            };
            if !string.is_null() && *string.add(0) != 0 {
                if len == 1 {
                    return ret + ((*string.add(0) as u32) << 8);
                }
                if *string.add(1) == 0 {
                    len = 1;
                    ret = *string.add(0) as _;
                } else {
                    return *string.add(0) as u32 + ((*string.add(1) as u32) << 8);
                }
            }
            // Skip to next node
            if let Some(children) = now.children().filter(|children| {
                !matches!(now.element_type(), XmlElementType::XmlDTDNode)
                    && !matches!(children.element_type(), XmlElementType::XmlEntityDecl)
            }) {
                tmp = Some(children);
                continue;
            }
            if tmp == Some(node) {
                break;
            }

            if let Some(next) = now.next() {
                tmp = Some(next);
                continue;
            }

            tmp = loop {
                let Some(tmp) = now.parent() else {
                    break None;
                };
                if tmp == node {
                    break None;
                }
                if let Some(next) = tmp.next() {
                    break Some(next);
                }
            };
        }
        ret
    }
}

/// Implement the equal / not equal operation on XPath nodesets:
/// @arg1 == @arg2  or  @arg1 != @arg2
/// If both objects to be compared are node-sets, then the comparison
/// will be true if and only if there is a node in the first node-set and
/// a node in the second node-set such that the result of performing the
/// comparison on the string-values of the two nodes is true.
///
/// (needless to say, this is a costly operation)
///
/// Returns 0 or 1 depending on the results of the test.
#[doc(alias = "xmlXPathEqualNodeSets")]
unsafe fn xml_xpath_equal_node_sets(
    arg1: XmlXPathObjectPtr,
    arg2: XmlXPathObjectPtr,
    neq: i32,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        if arg1.is_null()
            || !matches!(
                (*arg1).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            return 0;
        }
        if arg2.is_null()
            || !matches!(
                (*arg2).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            return 0;
        }

        let (Some(ns1), Some(ns2)) = ((*arg1).nodesetval.as_deref(), (*arg2).nodesetval.as_deref())
        else {
            return 0;
        };
        if ns1.node_tab.is_empty() || ns2.node_tab.is_empty() {
            return 0;
        }

        // for equal, check if there is a node pertaining to both sets
        if neq == 0 {
            for &node1 in &ns1.node_tab {
                for &node2 in &ns2.node_tab {
                    if node1 == node2 {
                        return 1;
                    }
                }
            }
        }

        let mut values1 = vec![None; ns1.node_tab.len()];
        let hashs1: *mut u32 = xml_malloc(ns1.node_tab.len() * size_of::<u32>()) as *mut u32;
        if hashs1.is_null() {
            // TODO: Propagate memory error.
            xml_xpath_err_memory(null_mut(), Some("comparing nodesets\n"));
            return 0;
        }
        let mut values2 = vec![None; ns2.node_tab.len()];
        let hashs2: *mut u32 = xml_malloc(ns2.node_tab.len() * size_of::<u32>()) as *mut u32;
        if hashs2.is_null() {
            // TODO: Propagate memory error.
            xml_xpath_err_memory(null_mut(), Some("comparing nodesets\n"));
            xml_free(hashs1 as _);
            return 0;
        }
        for (i, &node1) in ns1.node_tab.iter().enumerate() {
            *hashs1.add(i) = xml_xpath_node_val_hash(Some(node1));
            for (j, &node2) in ns2.node_tab.iter().enumerate() {
                if i == 0 {
                    *hashs2.add(j) = xml_xpath_node_val_hash(Some(node2));
                }
                if *hashs1.add(i) != *hashs2.add(j) {
                    if neq != 0 {
                        ret = 1;
                        break;
                    }
                } else {
                    if values1[i].is_none() {
                        values1[i] = node1.get_content();
                    }
                    if values2[j].is_none() {
                        values2[j] = node2.get_content();
                    }
                    ret = (values1[i] == values2[j]) as i32 ^ neq;
                    if ret != 0 {
                        break;
                    }
                }
            }
            if ret != 0 {
                break;
            }
        }
        xml_free(hashs1 as _);
        xml_free(hashs2 as _);
        ret
    }
}

/// Implement the equal operation on XPath objects content: @arg1 == @arg2
/// If one object to be compared is a node-set and the other is a number,
/// then the comparison will be true if and only if there is a node in
/// the node-set such that the result of performing the comparison on the
/// number to be compared and on the result of converting the string-value
/// of that node to a number using the number function is true.
///
/// Returns 0 or 1 depending on the results of the test.
#[doc(alias = "xmlXPathEqualNodeSetFloat")]
unsafe fn xml_xpath_equal_node_set_float(
    ctxt: XmlXPathParserContextPtr,
    arg: XmlXPathObjectPtr,
    f: f64,
    neq: i32,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        let mut val: XmlXPathObjectPtr;
        let mut v: f64;

        if arg.is_null()
            || !matches!(
                (*arg).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            return 0;
        }

        if let Some(ns) = (*arg).nodesetval.as_deref() {
            for &node in &ns.node_tab {
                (*ctxt).value_push(xml_xpath_cache_new_string(
                    (*ctxt).context,
                    Some(&xml_xpath_cast_node_to_string(Some(node))),
                ));
                xml_xpath_number_function(ctxt, 1);
                CHECK_ERROR0!(ctxt);
                val = (*ctxt).value_pop();
                v = (*val).floatval;
                xml_xpath_release_object((*ctxt).context, val);
                if !xml_xpath_is_nan(v) {
                    if (neq == 0 && v == f) || (neq != 0 && v != f) {
                        ret = 1;
                        break;
                    }
                } else {
                    // NaN is unequal to any value
                    if neq != 0 {
                        ret = 1;
                    }
                }
            }
        }

        ret
    }
}

/// Function computing the beginning of the string value of the node,
/// used to speed up comparisons
///
/// Returns an int usable as a hash
#[doc(alias = "xmlXPathStringHash")]
unsafe fn xml_xpath_string_hash(string: *const XmlChar) -> u32 {
    unsafe {
        if string.is_null() {
            return 0;
        }
        if *string.add(0) == 0 {
            return 0;
        }
        *string.add(0) as u32 + ((*string.add(1) as u32) << 8)
    }
}

/// Implement the equal operation on XPath objects content: @arg1 == @arg2
/// If one object to be compared is a node-set and the other is a string,
/// then the comparison will be true if and only if there is a node in
/// the node-set such that the result of performing the comparison on the
/// string-value of the node and the other string is true.
///
/// Returns 0 or 1 depending on the results of the test.
#[doc(alias = "xmlXPathEqualNodeSetString")]
unsafe fn xml_xpath_equal_node_set_string(arg: XmlXPathObjectPtr, str: &str, neq: i32) -> i32 {
    unsafe {
        if arg.is_null()
            || !matches!(
                (*arg).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            return 0;
        }

        // A NULL nodeset compared with a string is always false
        // (since there is no node equal, and no node not equal)
        let Some(ns) = (*arg).nodesetval.as_deref().filter(|n| !n.is_empty()) else {
            return 0;
        };
        let cstr = CString::new(str).unwrap();
        let hash: u32 = xml_xpath_string_hash(cstr.as_ptr() as *const u8);
        for &node in &ns.node_tab {
            if xml_xpath_node_val_hash(Some(node)) == hash {
                let str2 = node.get_content();
                if (str2.is_some() && Some(str) == str2.as_deref())
                    || (str2.is_none() && str.is_empty())
                {
                    if neq != 0 {
                        continue;
                    }
                    return 1;
                } else if neq != 0 {
                    return 1;
                }
            } else if neq != 0 {
                return 1;
            }
        }
        0
    }
}

unsafe fn xml_xpath_equal_values_common(
    ctxt: XmlXPathParserContextPtr,
    mut arg1: XmlXPathObjectPtr,
    mut arg2: XmlXPathObjectPtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        // At this point we are assured neither arg1 nor arg2
        // is a nodeset, so we can just pick the appropriate routine.
        match (*arg1).typ {
            XmlXPathObjectType::XPathUndefined => {}
            XmlXPathObjectType::XPathBoolean => match (*arg2).typ {
                XmlXPathObjectType::XPathUndefined => {}
                XmlXPathObjectType::XPathBoolean => {
                    ret = ((*arg1).boolval == (*arg2).boolval) as i32;
                }
                XmlXPathObjectType::XPathNumber => {
                    ret = ((*arg1).boolval == xml_xpath_cast_number_to_boolean((*arg2).floatval))
                        as i32;
                }
                XmlXPathObjectType::XPathString => {
                    let f = (*arg2).stringval.as_deref().is_some_and(|s| !s.is_empty());
                    ret = ((*arg1).boolval == f) as i32;
                }
                XmlXPathObjectType::XPathUsers => {
                    todo!()
                }
                #[cfg(feature = "libxml_xptr_locs")]
                XmlXPathObjectType::XPathPoint
                | XmlXPathObjectType::XPathRange
                | XmlXPathObjectType::XPathLocationset => {
                    todo!()
                }
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {}
            },
            XmlXPathObjectType::XPathNumber => {
                match (*arg2).typ {
                    XmlXPathObjectType::XPathUndefined => {}
                    XmlXPathObjectType::XPathBoolean => {
                        ret = ((*arg2).boolval
                            == xml_xpath_cast_number_to_boolean((*arg1).floatval))
                            as i32;
                    }

                    ty @ XmlXPathObjectType::XPathString
                    | ty @ XmlXPathObjectType::XPathNumber => 'to_break: {
                        if matches!(ty, XmlXPathObjectType::XPathString) {
                            (*ctxt).value_push(arg2);
                            xml_xpath_number_function(ctxt, 1);
                            arg2 = (*ctxt).value_pop();
                            if (*ctxt).error != 0 {
                                break 'to_break;
                            }
                            // Falls through.
                        }

                        // Hand check NaN and Infinity equalities
                        if xml_xpath_is_nan((*arg1).floatval) || xml_xpath_is_nan((*arg2).floatval)
                        {
                            ret = 0;
                        } else if xml_xpath_is_inf((*arg1).floatval) == 1 {
                            if xml_xpath_is_inf((*arg2).floatval) == 1 {
                                ret = 1;
                            } else {
                                ret = 0;
                            }
                        } else if xml_xpath_is_inf((*arg1).floatval) == -1 {
                            if xml_xpath_is_inf((*arg2).floatval) == -1 {
                                ret = 1;
                            } else {
                                ret = 0;
                            }
                        } else if xml_xpath_is_inf((*arg2).floatval) == 1 {
                            if xml_xpath_is_inf((*arg1).floatval) == 1 {
                                ret = 1;
                            } else {
                                ret = 0;
                            }
                        } else if xml_xpath_is_inf((*arg2).floatval) == -1 {
                            if xml_xpath_is_inf((*arg1).floatval) == -1 {
                                ret = 1;
                            } else {
                                ret = 0;
                            }
                        } else {
                            ret = ((*arg1).floatval == (*arg2).floatval) as i32;
                        }
                    }
                    XmlXPathObjectType::XPathUsers => {
                        todo!()
                    }
                    #[cfg(feature = "libxml_xptr_locs")]
                    XmlXPathObjectType::XPathPoint
                    | XmlXPathObjectType::XPathRange
                    | XmlXPathObjectType::XPathLocationset => {
                        todo!()
                    }
                    XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {}
                }
            }
            XmlXPathObjectType::XPathString => {
                match (*arg2).typ {
                    XmlXPathObjectType::XPathUndefined => {}
                    XmlXPathObjectType::XPathBoolean => {
                        let f = (*arg1).stringval.as_deref().is_some_and(|s| !s.is_empty());
                        ret = ((*arg2).boolval == f) as i32;
                    }
                    XmlXPathObjectType::XPathString => {
                        ret = ((*arg1).stringval == (*arg2).stringval) as i32;
                    }
                    XmlXPathObjectType::XPathNumber => {
                        (*ctxt).value_push(arg1);
                        xml_xpath_number_function(ctxt, 1);
                        arg1 = (*ctxt).value_pop();
                        if (*ctxt).error != 0 {
                            // break;
                        } else {
                            // Hand check NaN and Infinity equalities
                            if xml_xpath_is_nan((*arg1).floatval)
                                || xml_xpath_is_nan((*arg2).floatval)
                            {
                                ret = 0;
                            } else if xml_xpath_is_inf((*arg1).floatval) == 1 {
                                if xml_xpath_is_inf((*arg2).floatval) == 1 {
                                    ret = 1;
                                } else {
                                    ret = 0;
                                }
                            } else if xml_xpath_is_inf((*arg1).floatval) == -1 {
                                if xml_xpath_is_inf((*arg2).floatval) == -1 {
                                    ret = 1;
                                } else {
                                    ret = 0;
                                }
                            } else if xml_xpath_is_inf((*arg2).floatval) == 1 {
                                if xml_xpath_is_inf((*arg1).floatval) == 1 {
                                    ret = 1;
                                } else {
                                    ret = 0;
                                }
                            } else if xml_xpath_is_inf((*arg2).floatval) == -1 {
                                if xml_xpath_is_inf((*arg1).floatval) == -1 {
                                    ret = 1;
                                } else {
                                    ret = 0;
                                }
                            } else {
                                ret = ((*arg1).floatval == (*arg2).floatval) as i32;
                            }
                        }
                    }
                    XmlXPathObjectType::XPathUsers => {
                        todo!()
                    }
                    #[cfg(feature = "libxml_xptr_locs")]
                    XmlXPathObjectType::XPathPoint
                    | XmlXPathObjectType::XPathRange
                    | XmlXPathObjectType::XPathLocationset => {
                        todo!()
                    }
                    XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {}
                }
            }
            XmlXPathObjectType::XPathUsers => {
                todo!()
            }
            #[cfg(feature = "libxml_xptr_locs")]
            XmlXPathObjectType::XPathPoint
            | XmlXPathObjectType::XPathRange
            | XmlXPathObjectType::XPathLocationset => {
                todo!()
            }
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {}
        }
        xml_xpath_release_object((*ctxt).context, arg1);
        xml_xpath_release_object((*ctxt).context, arg2);
        ret
    }
}

/// Implement the equal operation on XPath objects content: @arg1 == @arg2
///
/// Returns 0 or 1 depending on the results of the test.
#[doc(alias = "xmlXPathEqualValues")]
pub unsafe fn xml_xpath_equal_values(ctxt: XmlXPathParserContextPtr) -> i32 {
    unsafe {
        let mut arg1: XmlXPathObjectPtr;
        let mut arg2: XmlXPathObjectPtr;
        let argtmp: XmlXPathObjectPtr;
        let mut ret: i32 = 0;

        if ctxt.is_null() || (*ctxt).context.is_null() {
            return 0;
        }
        arg2 = (*ctxt).value_pop();
        arg1 = (*ctxt).value_pop();
        if arg1.is_null() || arg2.is_null() {
            if !arg1.is_null() {
                xml_xpath_release_object((*ctxt).context, arg1);
            } else {
                xml_xpath_release_object((*ctxt).context, arg2);
            }
            XP_ERROR0!(ctxt, XmlXPathError::XPathInvalidOperand as i32);
        }

        if arg1 == arg2 {
            xml_xpath_free_object(arg1);
            return 1;
        }

        // If either argument is a nodeset, it's a 'special case'
        if matches!(
            (*arg2).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        ) || matches!(
            (*arg1).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        ) {
            // Hack it to assure arg1 is the nodeset
            if !matches!(
                (*arg1).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            ) {
                argtmp = arg2;
                arg2 = arg1;
                arg1 = argtmp;
            }
            match (*arg2).typ {
                XmlXPathObjectType::XPathUndefined => {}
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
                    ret = xml_xpath_equal_node_sets(arg1, arg2, 0);
                }
                XmlXPathObjectType::XPathBoolean => {
                    let f = (*arg1).nodesetval.as_deref().is_some_and(|n| !n.is_empty());
                    ret = (f == (*arg2).boolval) as i32;
                }
                XmlXPathObjectType::XPathNumber => {
                    ret = xml_xpath_equal_node_set_float(ctxt, arg1, (*arg2).floatval, 0);
                }
                XmlXPathObjectType::XPathString => {
                    ret = xml_xpath_equal_node_set_string(
                        arg1,
                        (*arg2).stringval.as_deref().expect("Internal Error"),
                        0,
                    );
                }
                XmlXPathObjectType::XPathUsers => {
                    todo!()
                }
                #[cfg(feature = "libxml_xptr_locs")]
                XmlXPathObjectType::XPathPoint
                | XmlXPathObjectType::XPathRange
                | XmlXPathObjectType::XPathLocationset => todo!(),
                // _ => {}
            }
            xml_xpath_release_object((*ctxt).context, arg1);
            xml_xpath_release_object((*ctxt).context, arg2);
            return ret;
        }

        xml_xpath_equal_values_common(ctxt, arg1, arg2)
    }
}

/// Implement the equal operation on XPath objects content: @arg1 == @arg2
///
/// Returns 0 or 1 depending on the results of the test.
#[doc(alias = "xmlXPathNotEqualValues")]
pub unsafe fn xml_xpath_not_equal_values(ctxt: XmlXPathParserContextPtr) -> i32 {
    unsafe {
        let mut arg1: XmlXPathObjectPtr;
        let mut arg2: XmlXPathObjectPtr;
        let argtmp: XmlXPathObjectPtr;
        let mut ret: i32 = 0;

        if ctxt.is_null() || (*ctxt).context.is_null() {
            return 0;
        }
        arg2 = (*ctxt).value_pop();
        arg1 = (*ctxt).value_pop();
        if arg1.is_null() || arg2.is_null() {
            if !arg1.is_null() {
                xml_xpath_release_object((*ctxt).context, arg1);
            } else {
                xml_xpath_release_object((*ctxt).context, arg2);
            }
            XP_ERROR0!(ctxt, XmlXPathError::XPathInvalidOperand as i32);
        }

        if arg1 == arg2 {
            xml_xpath_release_object((*ctxt).context, arg1);
            return 0;
        }

        // If either argument is a nodeset, it's a 'special case'
        if matches!(
            (*arg2).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        ) || matches!(
            (*arg1).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        ) {
            // Hack it to assure arg1 is the nodeset
            if !matches!(
                (*arg1).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            ) {
                argtmp = arg2;
                arg2 = arg1;
                arg1 = argtmp;
            }
            match (*arg2).typ {
                XmlXPathObjectType::XPathUndefined => {}
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
                    ret = xml_xpath_equal_node_sets(arg1, arg2, 1);
                }
                XmlXPathObjectType::XPathBoolean => {
                    let f = (*arg1).nodesetval.as_deref().is_some_and(|n| !n.is_empty());
                    ret = (f != (*arg2).boolval) as i32;
                }
                XmlXPathObjectType::XPathNumber => {
                    ret = xml_xpath_equal_node_set_float(ctxt, arg1, (*arg2).floatval, 1);
                }
                XmlXPathObjectType::XPathString => {
                    ret = xml_xpath_equal_node_set_string(
                        arg1,
                        (*arg2).stringval.as_deref().expect("Internal Error"),
                        1,
                    );
                }
                XmlXPathObjectType::XPathUsers => {
                    todo!()
                }
                #[cfg(feature = "libxml_xptr_locs")]
                XmlXPathObjectType::XPathPoint
                | XmlXPathObjectType::XPathRange
                | XmlXPathObjectType::XPathLocationset => {
                    todo!()
                } // _ => {}
            }
            xml_xpath_release_object((*ctxt).context, arg1);
            xml_xpath_release_object((*ctxt).context, arg2);
            return ret;
        }

        (xml_xpath_equal_values_common(ctxt, arg1, arg2) == 0) as i32
    }
}

/// Implement the compare operation on nodesets:
///
/// If both objects to be compared are node-sets, then the comparison
/// will be true if and only if there is a node in the first node-set
/// and a node in the second node-set such that the result of performing
/// the comparison on the string-values of the two nodes is true.
/// ....
/// When neither object to be compared is a node-set and the operator
/// is <=, <, >= or >, then the objects are compared by converting both
/// objects to numbers and comparing the numbers according to IEEE 754.
/// ....
/// The number function converts its argument to a number as follows:
///  - a string that consists of optional whitespace followed by an
///    optional minus sign followed by a Number followed by whitespace
///    is converted to the IEEE 754 number that is nearest (according
///    to the IEEE 754 round-to-nearest rule) to the mathematical value
///    represented by the string; any other string is converted to NaN
///
/// Conclusion all nodes need to be converted first to their string value
/// and then the comparison must be done when possible
#[doc(alias = "xmlXPathCompareNodeSets")]
unsafe fn xml_xpath_compare_node_sets(
    inf: i32,
    strict: i32,
    arg1: XmlXPathObjectPtr,
    arg2: XmlXPathObjectPtr,
) -> i32 {
    unsafe {
        let mut init: i32 = 0;
        let mut val1: f64;
        let mut ret: i32 = 0;

        if arg1.is_null()
            || !matches!(
                (*arg1).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            xml_xpath_free_object(arg2);
            return 0;
        }
        if arg2.is_null()
            || !matches!(
                (*arg2).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            xml_xpath_free_object(arg1);
            xml_xpath_free_object(arg2);
            return 0;
        }

        let Some(ns1_table) = (*arg1)
            .nodesetval
            .as_deref()
            .filter(|set| !set.node_tab.is_empty())
            .map(|n| n.node_tab.as_slice())
        else {
            xml_xpath_free_object(arg1);
            xml_xpath_free_object(arg2);
            return 0;
        };
        let Some(ns2_table) = (*arg2)
            .nodesetval
            .as_deref()
            .filter(|set| !set.node_tab.is_empty())
            .map(|n| n.node_tab.as_slice())
        else {
            xml_xpath_free_object(arg1);
            xml_xpath_free_object(arg2);
            return 0;
        };

        let values2: *mut f64 = xml_malloc(ns2_table.len() * size_of::<f64>()) as *mut f64;
        if values2.is_null() {
            // TODO: Propagate memory error.
            xml_xpath_err_memory(null_mut(), Some("comparing nodesets\n"));
            xml_xpath_free_object(arg1);
            xml_xpath_free_object(arg2);
            return 0;
        }
        for &node1 in ns1_table {
            val1 = xml_xpath_cast_node_to_number(Some(node1));
            if xml_xpath_is_nan(val1) {
                continue;
            }
            for (j, &node2) in ns2_table.iter().enumerate() {
                if init == 0 {
                    *values2.add(j) = xml_xpath_cast_node_to_number(Some(node2));
                }
                if xml_xpath_is_nan(*values2.add(j)) {
                    continue;
                }
                if inf != 0 && strict != 0 {
                    ret = (val1 < *values2.add(j)) as i32;
                } else if inf != 0 && strict == 0 {
                    ret = (val1 <= *values2.add(j)) as i32;
                } else if inf == 0 && strict != 0 {
                    ret = (val1 > *values2.add(j)) as i32;
                } else if inf == 0 && strict == 0 {
                    ret = (val1 >= *values2.add(j)) as i32;
                }
                if ret != 0 {
                    break;
                }
            }
            if ret != 0 {
                break;
            }
            init = 1;
        }
        xml_free(values2 as _);
        xml_xpath_free_object(arg1);
        xml_xpath_free_object(arg2);
        ret
    }
}

/// Implement the compare operation between a nodeset and a number
///     @ns < @val    (1, 1, ...
///     @ns <= @val   (1, 0, ...
///     @ns > @val    (0, 1, ...
///     @ns >= @val   (0, 0, ...
///
/// If one object to be compared is a node-set and the other is a number,
/// then the comparison will be true if and only if there is a node in the
/// node-set such that the result of performing the comparison on the number
/// to be compared and on the result of converting the string-value of that
/// node to a number using the number function is true.
///
/// Returns 0 or 1 depending on the results of the test.
#[doc(alias = "xmlXPathCompareNodeSetFloat")]
unsafe fn xml_xpath_compare_node_set_float(
    ctxt: XmlXPathParserContextPtr,
    inf: i32,
    strict: i32,
    arg: XmlXPathObjectPtr,
    f: XmlXPathObjectPtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        if f.is_null()
            || arg.is_null()
            || !matches!(
                (*arg).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            xml_xpath_release_object((*ctxt).context, arg);
            xml_xpath_release_object((*ctxt).context, f);
            return 0;
        }
        if let Some(ns) = (*arg).nodesetval.as_deref() {
            for &node in &ns.node_tab {
                (*ctxt).value_push(xml_xpath_cache_new_string(
                    (*ctxt).context,
                    Some(&xml_xpath_cast_node_to_string(Some(node))),
                ));
                xml_xpath_number_function(ctxt, 1);
                (*ctxt).value_push(xml_xpath_cache_object_copy((*ctxt).context, f));
                ret = xml_xpath_compare_values(ctxt, inf, strict);
                if ret != 0 {
                    break;
                }
            }
        }
        xml_xpath_release_object((*ctxt).context, arg);
        xml_xpath_release_object((*ctxt).context, f);
        ret
    }
}

/// Implement the compare operation between a nodeset and a string
///     @ns < @val    (1, 1, ...
///     @ns <= @val   (1, 0, ...
///     @ns > @val    (0, 1, ...
///     @ns >= @val   (0, 0, ...
///
/// If one object to be compared is a node-set and the other is a string,
/// then the comparison will be true if and only if there is a node in
/// the node-set such that the result of performing the comparison on the
/// string-value of the node and the other string is true.
///
/// Returns 0 or 1 depending on the results of the test.
#[doc(alias = "xmlXPathCompareNodeSetString")]
unsafe fn xml_xpath_compare_node_set_string(
    ctxt: XmlXPathParserContextPtr,
    inf: i32,
    strict: i32,
    arg: XmlXPathObjectPtr,
    s: XmlXPathObjectPtr,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        if s.is_null()
            || arg.is_null()
            || !matches!(
                (*arg).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            xml_xpath_release_object((*ctxt).context, arg);
            xml_xpath_release_object((*ctxt).context, s);
            return 0;
        }
        if let Some(ns) = (*arg).nodesetval.as_deref() {
            for &node in &ns.node_tab {
                (*ctxt).value_push(xml_xpath_cache_new_string(
                    (*ctxt).context,
                    Some(&xml_xpath_cast_node_to_string(Some(node))),
                ));
                (*ctxt).value_push(xml_xpath_cache_object_copy((*ctxt).context, s));
                ret = xml_xpath_compare_values(ctxt, inf, strict);
                if ret != 0 {
                    break;
                }
            }
        }
        xml_xpath_release_object((*ctxt).context, arg);
        xml_xpath_release_object((*ctxt).context, s);
        ret
    }
}

/// Implement the compare operation between a nodeset and a value
///     @ns < @val    (1, 1, ...
///     @ns <= @val   (1, 0, ...
///     @ns > @val    (0, 1, ...
///     @ns >= @val   (0, 0, ...
///
/// If one object to be compared is a node-set and the other is a boolean,
/// then the comparison will be true if and only if the result of performing
/// the comparison on the boolean and on the result of converting
/// the node-set to a boolean using the boolean function is true.
///
/// Returns 0 or 1 depending on the results of the test.
#[doc(alias = "xmlXPathCompareNodeSetValue")]
unsafe fn xml_xpath_compare_node_set_value(
    ctxt: XmlXPathParserContextPtr,
    inf: i32,
    strict: i32,
    arg: XmlXPathObjectPtr,
    val: XmlXPathObjectPtr,
) -> i32 {
    unsafe {
        if val.is_null()
            || arg.is_null()
            || !matches!(
                (*arg).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            return 0;
        }

        match (*val).typ {
            XmlXPathObjectType::XPathNumber => {
                xml_xpath_compare_node_set_float(ctxt, inf, strict, arg, val)
            }
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
                xml_xpath_compare_node_sets(inf, strict, arg, val)
            }
            XmlXPathObjectType::XPathString => {
                xml_xpath_compare_node_set_string(ctxt, inf, strict, arg, val)
            }
            XmlXPathObjectType::XPathBoolean => {
                (*ctxt).value_push(arg);
                xml_xpath_boolean_function(ctxt, 1);
                (*ctxt).value_push(val);
                xml_xpath_compare_values(ctxt, inf, strict)
            }
            _ => {
                generic_error!(
                    "xmlXPathCompareNodeSetValue: Can't compare node set and object of type {:?}\n",
                    (*val).typ
                );
                xml_xpath_release_object((*ctxt).context, arg);
                xml_xpath_release_object((*ctxt).context, val);
                XP_ERROR0!(ctxt, XmlXPathError::XPathInvalidType as i32);
            }
        }
    }
}

/// Implement the compare operation on XPath objects:
///     @arg1 < @arg2    (1, 1, ...
///     @arg1 <= @arg2   (1, 0, ...
///     @arg1 > @arg2    (0, 1, ...
///     @arg1 >= @arg2   (0, 0, ...
///
/// When neither object to be compared is a node-set and the operator is
/// <=, <, >=, >, then the objects are compared by converted both objects
/// to numbers and comparing the numbers according to IEEE 754. The <
/// comparison will be true if and only if the first number is less than the
/// second number. The <= comparison will be true if and only if the first
/// number is less than or equal to the second number. The > comparison
/// will be true if and only if the first number is greater than the second
/// number. The >= comparison will be true if and only if the first number
/// is greater than or equal to the second number.
///
/// Returns 1 if the comparison succeeded, 0 if it failed
#[doc(alias = "xmlXPathCompareValues")]
pub unsafe fn xml_xpath_compare_values(
    ctxt: XmlXPathParserContextPtr,
    inf: i32,
    strict: i32,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let arg1i: i32;
        let arg2i: i32;
        let mut arg1: XmlXPathObjectPtr;
        let mut arg2: XmlXPathObjectPtr;

        if ctxt.is_null() || (*ctxt).context.is_null() {
            return 0;
        }
        arg2 = (*ctxt).value_pop();
        arg1 = (*ctxt).value_pop();
        if arg1.is_null() || arg2.is_null() {
            if !arg1.is_null() {
                xml_xpath_release_object((*ctxt).context, arg1);
            } else {
                xml_xpath_release_object((*ctxt).context, arg2);
            }
            XP_ERROR0!(ctxt, XmlXPathError::XPathInvalidOperand as i32);
        }

        if matches!(
            (*arg2).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        ) || matches!(
            (*arg1).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        ) {
            // If either argument is a XpathNodeset or XpathXsltTree the two arguments
            // are not freed from within this routine; they will be freed from the
            // called routine, e.g. xmlXPathCompareNodeSets or xmlXPathCompareNodeSetValue
            if matches!(
                (*arg2).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            ) && matches!(
                (*arg1).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            ) {
                ret = xml_xpath_compare_node_sets(inf, strict, arg1, arg2);
            } else if matches!(
                (*arg1).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            ) {
                ret = xml_xpath_compare_node_set_value(ctxt, inf, strict, arg1, arg2);
            } else {
                ret = xml_xpath_compare_node_set_value(ctxt, !inf, strict, arg2, arg1);
            }
            return ret;
        }

        if !matches!((*arg1).typ, XmlXPathObjectType::XPathNumber) {
            (*ctxt).value_push(arg1);
            xml_xpath_number_function(ctxt, 1);
            arg1 = (*ctxt).value_pop();
        }
        if !matches!((*arg2).typ, XmlXPathObjectType::XPathNumber) {
            (*ctxt).value_push(arg2);
            xml_xpath_number_function(ctxt, 1);
            arg2 = (*ctxt).value_pop();
        }
        if (*ctxt).error != 0 {
            // goto error;
            xml_xpath_release_object((*ctxt).context, arg1);
            xml_xpath_release_object((*ctxt).context, arg2);
            return ret;
        }
        // Add tests for infinity and nan
        // => feedback on 3.4 for Inf and NaN
        /* Hand check NaN and Infinity comparisons */
        if xml_xpath_is_nan((*arg1).floatval) || xml_xpath_is_nan((*arg2).floatval) {
            ret = 0;
        } else {
            arg1i = xml_xpath_is_inf((*arg1).floatval);
            arg2i = xml_xpath_is_inf((*arg2).floatval);
            if inf != 0 && strict != 0 {
                if (arg1i == -1 && arg2i != -1) || (arg2i == 1 && arg1i != 1) {
                    ret = 1;
                } else if arg1i == 0 && arg2i == 0 {
                    ret = ((*arg1).floatval < (*arg2).floatval) as i32;
                } else {
                    ret = 0;
                }
            } else if inf != 0 && strict == 0 {
                if arg1i == -1 || arg2i == 1 {
                    ret = 1;
                } else if arg1i == 0 && arg2i == 0 {
                    ret = ((*arg1).floatval <= (*arg2).floatval) as i32;
                } else {
                    ret = 0;
                }
            } else if inf == 0 && strict != 0 {
                if (arg1i == 1 && arg2i != 1) || (arg2i == -1 && arg1i != -1) {
                    ret = 1;
                } else if arg1i == 0 && arg2i == 0 {
                    ret = ((*arg1).floatval > (*arg2).floatval) as i32;
                } else {
                    ret = 0;
                }
            } else if inf == 0 && strict == 0 {
                if arg1i == 1 || arg2i == -1 {
                    ret = 1;
                } else if arg1i == 0 && arg2i == 0 {
                    ret = ((*arg1).floatval >= (*arg2).floatval) as i32;
                } else {
                    ret = 0;
                }
            }
        }
        // error:
        xml_xpath_release_object((*ctxt).context, arg1);
        xml_xpath_release_object((*ctxt).context, arg2);
        ret
    }
}

/// Implement the unary - operation on an XPath object
/// The numeric operators convert their operands to numbers as if
/// by calling the number function.
#[doc(alias = "xmlXPathValueFlipSign")]
pub unsafe fn xml_xpath_value_flip_sign(ctxt: XmlXPathParserContextPtr) {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return;
        }
        CAST_TO_NUMBER!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
        (*(*ctxt).value).floatval = -(*(*ctxt).value).floatval;
    }
}

/// Implement the add operation on XPath objects:
/// The numeric operators convert their operands to numbers as if
/// by calling the number function.
#[doc(alias = "xmlXPathAddValues")]
pub unsafe fn xml_xpath_add_values(ctxt: XmlXPathParserContextPtr) {
    unsafe {
        let arg: XmlXPathObjectPtr = (*ctxt).value_pop();
        if arg.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidOperand as i32);
        }
        let val: f64 = xml_xpath_cast_to_number(arg);
        xml_xpath_release_object((*ctxt).context, arg);
        CAST_TO_NUMBER!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
        (*(*ctxt).value).floatval += val;
    }
}

/// Implement the subtraction operation on XPath objects:
/// The numeric operators convert their operands to numbers as if
/// by calling the number function.
#[doc(alias = "xmlXPathSubValues")]
pub unsafe fn xml_xpath_sub_values(ctxt: XmlXPathParserContextPtr) {
    unsafe {
        let arg: XmlXPathObjectPtr = (*ctxt).value_pop();
        if arg.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidOperand as i32);
        }
        let val: f64 = xml_xpath_cast_to_number(arg);
        xml_xpath_release_object((*ctxt).context, arg);
        CAST_TO_NUMBER!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
        (*(*ctxt).value).floatval -= val;
    }
}

/// Implement the multiply operation on XPath objects:
/// The numeric operators convert their operands to numbers as if
/// by calling the number function.
#[doc(alias = "xmlXPathMultValues")]
pub unsafe fn xml_xpath_mult_values(ctxt: XmlXPathParserContextPtr) {
    unsafe {
        let arg: XmlXPathObjectPtr = (*ctxt).value_pop();
        if arg.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidOperand as i32);
        }
        let val: f64 = xml_xpath_cast_to_number(arg);
        xml_xpath_release_object((*ctxt).context, arg);
        CAST_TO_NUMBER!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
        (*(*ctxt).value).floatval *= val;
    }
}

/// Implement the div operation on XPath objects @arg1 / @arg2:
/// The numeric operators convert their operands to numbers as if
/// by calling the number function.
#[doc(alias = "xmlXPathDivValues")]
pub unsafe fn xml_xpath_div_values(ctxt: XmlXPathParserContextPtr) {
    unsafe {
        let arg: XmlXPathObjectPtr = (*ctxt).value_pop();
        if arg.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidOperand as i32);
        }
        let val: f64 = xml_xpath_cast_to_number(arg);
        xml_xpath_release_object((*ctxt).context, arg);
        CAST_TO_NUMBER!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
        (*(*ctxt).value).floatval /= val;
    }
}

/// Implement the mod operation on XPath objects: @arg1 / @arg2
/// The numeric operators convert their operands to numbers as if
/// by calling the number function.
#[doc(alias = "xmlXPathModValues")]
pub unsafe fn xml_xpath_mod_values(ctxt: XmlXPathParserContextPtr) {
    unsafe {
        let arg: XmlXPathObjectPtr = (*ctxt).value_pop();
        if arg.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidOperand as i32);
        }
        let arg2: f64 = xml_xpath_cast_to_number(arg);
        xml_xpath_release_object((*ctxt).context, arg);
        CAST_TO_NUMBER!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
        let arg1: f64 = (*(*ctxt).value).floatval;
        if arg2 == 0.0 {
            (*(*ctxt).value).floatval = XML_XPATH_NAN;
        } else {
            (*(*ctxt).value).floatval = arg1 % arg2;
        }
    }
}

/// Traversal function for the "self" direction
/// The self axis contains just the context node itself
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextSelf")]
pub unsafe fn xml_xpath_next_self(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        if cur.is_none() {
            return (*(*ctxt).context).node;
        }
        None
    }
}

/// Traversal function for the "child" direction
/// The child axis contains the children of the context node in document order.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextChild")]
pub unsafe fn xml_xpath_next_child(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        let Some(cur) = cur else {
            let node = (*(*ctxt).context).node?;
            match node.element_type() {
                XmlElementType::XmlElementNode
                | XmlElementType::XmlTextNode
                | XmlElementType::XmlCDATASectionNode
                | XmlElementType::XmlEntityRefNode
                | XmlElementType::XmlEntityNode
                | XmlElementType::XmlPINode
                | XmlElementType::XmlCommentNode
                | XmlElementType::XmlNotationNode
                | XmlElementType::XmlDTDNode => {
                    return node.children();
                }
                XmlElementType::XmlDocumentNode
                | XmlElementType::XmlDocumentTypeNode
                | XmlElementType::XmlDocumentFragNode
                | XmlElementType::XmlHTMLDocumentNode => {
                    return node.children();
                }
                XmlElementType::XmlElementDecl
                | XmlElementType::XmlAttributeDecl
                | XmlElementType::XmlEntityDecl
                | XmlElementType::XmlAttributeNode
                | XmlElementType::XmlNamespaceDecl
                | XmlElementType::XmlXIncludeStart
                | XmlElementType::XmlXIncludeEnd => {
                    return None;
                }
                _ => unreachable!(),
            }
        };
        if matches!(
            cur.element_type(),
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
        ) {
            return None;
        }
        cur.next()
    }
}

/// Traversal function for the "descendant" direction
/// the descendant axis contains the descendants of the context node in document
/// order; a descendant is a child or a child of a child and so on.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextDescendant")]
pub unsafe fn xml_xpath_next_descendant(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        let Some(mut cur) = cur else {
            let node = (*(*ctxt).context).node?;
            if matches!(
                node.element_type(),
                XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
            ) {
                return None;
            }

            if (*(*ctxt).context).node == (*(*ctxt).context).doc.map(|doc| doc.into()) {
                return (*(*ctxt).context).doc.unwrap().children;
            }
            return node.children();
        };

        if matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        if let Some(children) = cur.children() {
            // Do not descend on entities declarations
            if !matches!(children.element_type(), XmlElementType::XmlEntityDecl) {
                cur = children;
                // Skip DTDs
                if !matches!(cur.element_type(), XmlElementType::XmlDTDNode) {
                    return Some(cur);
                }
            }
        }

        if Some(cur) == (*(*ctxt).context).node {
            return None;
        }

        while let Some(next) = cur.next() {
            cur = next;
            if !matches!(
                cur.element_type(),
                XmlElementType::XmlEntityDecl | XmlElementType::XmlDTDNode
            ) {
                return Some(cur);
            }
        }

        loop {
            cur = cur.parent()?;
            if Some(cur) == (*(*ctxt).context).node {
                break None;
            }
            if let Some(next) = cur.next() {
                cur = next;
                break Some(cur);
            }
        }
    }
}

/// Traversal function for the "descendant-or-self" direction
/// the descendant-or-self axis contains the context node and the descendants
/// of the context node in document order; thus the context node is the first
/// node on the axis, and the first child of the context node is the second node
/// on the axis
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextDescendantOrSelf")]
pub unsafe fn xml_xpath_next_descendant_or_self(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        if cur.is_none() {
            return (*(*ctxt).context).node;
        }

        if matches!(
            (*(*ctxt).context).node?.element_type(),
            XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
        ) {
            return None;
        }

        xml_xpath_next_descendant(ctxt, cur)
    }
}

/// Traversal function for the "parent" direction
/// The parent axis contains the parent of the context node, if there is one.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextParent")]
pub unsafe fn xml_xpath_next_parent(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        // the parent of an attribute or namespace node is the element
        // to which the attribute or namespace node is attached
        // Namespace handling !!!
        if cur.is_none() {
            let node = (*(*ctxt).context).node?;
            match node.element_type() {
                XmlElementType::XmlElementNode
                | XmlElementType::XmlTextNode
                | XmlElementType::XmlCDATASectionNode
                | XmlElementType::XmlEntityRefNode
                | XmlElementType::XmlEntityNode
                | XmlElementType::XmlPINode
                | XmlElementType::XmlCommentNode
                | XmlElementType::XmlNotationNode
                | XmlElementType::XmlDTDNode
                | XmlElementType::XmlElementDecl
                | XmlElementType::XmlAttributeDecl
                | XmlElementType::XmlXIncludeStart
                | XmlElementType::XmlXIncludeEnd
                | XmlElementType::XmlEntityDecl => {
                    let Some(parent) = node.parent() else {
                        return (*(*ctxt).context).doc.map(|doc| doc.into());
                    };
                    if XmlNodePtr::try_from(parent)
                        .ok()
                        .filter(|node| node.element_type() == XmlElementType::XmlElementNode)
                        .as_deref()
                        .and_then(|node| node.name())
                        .filter(|name| name.starts_with(' ') || name == "fake node libxslt")
                        .is_some()
                    {
                        return None;
                    }
                    return Some(parent);
                }
                XmlElementType::XmlAttributeNode => {
                    let att = XmlAttrPtr::try_from(node).unwrap();
                    return att.parent.map(XmlGenericNodePtr::from);
                }
                XmlElementType::XmlDocumentNode
                | XmlElementType::XmlDocumentTypeNode
                | XmlElementType::XmlDocumentFragNode
                | XmlElementType::XmlHTMLDocumentNode => {
                    return None;
                }
                XmlElementType::XmlNamespaceDecl => {
                    let ns = XmlNsPtr::try_from(node).unwrap();
                    if let Some(next) = ns
                        .node
                        .filter(|node| node.element_type() != XmlElementType::XmlNamespaceDecl)
                    {
                        return Some(next);
                    }
                    return None;
                }
                _ => unreachable!(),
            }
        }
        None
    }
}

/// Traversal function for the "ancestor-or-self" direction
/// he ancestor-or-self axis contains the context node and ancestors of
/// the context node in reverse document order; thus the context node is
/// the first node on the axis, and the context node's parent the second;
/// parent here is defined the same as with the parent axis.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextAncestorOrSelf")]
pub unsafe fn xml_xpath_next_ancestor_or_self(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        if cur.is_none() {
            return (*(*ctxt).context).node;
        }
        xml_xpath_next_ancestor(ctxt, cur)
    }
}

/// Traversal function for the "following-sibling" direction
/// The following-sibling axis contains the following siblings of the context
/// node in document order.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextFollowingSibling")]
pub unsafe fn xml_xpath_next_following_sibling(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        let node = (*(*ctxt).context).node?;
        if matches!(
            node.element_type(),
            XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
        ) {
            return None;
        }
        if cur == (*(*ctxt).context).doc.map(|doc| doc.into()) {
            return None;
        }
        if let Some(cur) = cur {
            cur.next()
        } else {
            node.next()
        }
    }
}

/// Traversal function for the "following" direction
/// The following axis contains all nodes in the same document as the context
/// node that are after the context node in document order, excluding any
/// descendants and excluding attribute nodes and namespace nodes; the nodes
/// are ordered in document order
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextFollowing")]
pub unsafe fn xml_xpath_next_following(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        if let Some(children) = cur
            .filter(|cur| {
                !matches!(
                    cur.element_type(),
                    XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
                )
            })
            .and_then(|cur| cur.children())
        {
            return Some(children);
        }

        let mut cur = cur.or_else(|| {
            let cur = (*(*ctxt).context).node?;
            if let Ok(attr) = XmlAttrPtr::try_from(cur) {
                attr.parent()
            } else if let Ok(ns) = XmlNsPtr::try_from(cur) {
                ns.node
                    .filter(|node| node.element_type() != XmlElementType::XmlNamespaceDecl)
            } else {
                None
            }
        })?;
        if let Some(next) = (*cur).next() {
            return Some(next);
        }
        loop {
            cur = cur.parent()?;
            if Some(cur) == (*(*ctxt).context).doc.map(|doc| doc.into()) {
                break None;
            }
            if let Some(next) = (*cur).next() {
                break Some(next);
            }
        }
    }
}

thread_local! {
    static XML_XPATH_XMLNAMESPACE_STRUCT: XmlNs = const { XmlNs {
        next: None,
        typ: XmlElementType::XmlNamespaceDecl,
        href: XML_XML_NAMESPACE.as_ptr() as *const u8,
        prefix: c"xml".as_ptr() as *const u8,
        _private: null_mut(),
        context: None,
        node: None,
    } };
}

/// Traversal function for the "namespace" direction
/// the namespace axis contains the namespace nodes of the context node;
/// the order of nodes on this axis is implementation-defined; the axis will
/// be empty unless the context node is an element
///
/// We keep the XML namespace node at the end of the list.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextNamespace")]
pub unsafe fn xml_xpath_next_namespace(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        let node = (*(*ctxt).context).node?;
        if !matches!(node.element_type(), XmlElementType::XmlElementNode) {
            return None;
        }
        if cur.is_none() {
            (*(*ctxt).context).tmp_ns_list = node.get_ns_list((*(*ctxt).context).doc);
            (*(*ctxt).context).tmp_ns_nr = 0;
            if let Some(list) = (*(*ctxt).context).tmp_ns_list.as_deref() {
                (*(*ctxt).context).tmp_ns_nr = list.len() as i32;
            }
            // Does it work ???
            let reference = XML_XPATH_XMLNAMESPACE_STRUCT.with(|s| s as *const XmlNs);
            return XmlGenericNodePtr::from_raw(reference as *mut XmlNs);
        }
        if (*(*ctxt).context).tmp_ns_nr > 0 {
            (*(*ctxt).context).tmp_ns_nr -= 1;
            Some(
                (*(*ctxt).context).tmp_ns_list.as_deref().unwrap()
                    [(*(*ctxt).context).tmp_ns_nr as usize]
                    .into(),
            )
        } else {
            (*(*ctxt).context).tmp_ns_list = None;
            None
        }
    }
}

/// Traversal function for the "attribute" direction
/// TODO: support DTD inherited default attributes
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextAttribute")]
pub unsafe fn xml_xpath_next_attribute(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }

        let node = XmlNodePtr::try_from((*(*ctxt).context).node?)
            .ok()
            .filter(|node| node.element_type() == XmlElementType::XmlElementNode)?;
        if let Some(cur) = cur {
            cur.next()
        } else {
            if (*(*ctxt).context).node == (*(*ctxt).context).doc.map(|doc| doc.into()) {
                return None;
            }
            node.properties.map(|prop| prop.into())
        }
    }
}

/// Check that @ancestor is a @node's ancestor
///
/// returns 1 if @ancestor is a @node's ancestor, 0 otherwise.
#[doc(alias = "xmlXPathIsAncestor")]
unsafe fn xml_xpath_is_ancestor(
    ancestor: Option<XmlGenericNodePtr>,
    node: Option<XmlGenericNodePtr>,
) -> i32 {
    let Some((ancestor, mut node)) = ancestor.zip(node) else {
        return 0;
    };
    if matches!(node.element_type(), XmlElementType::XmlNamespaceDecl) {
        return 0;
    }
    if matches!(ancestor.element_type(), XmlElementType::XmlNamespaceDecl) {
        return 0;
    }
    // nodes need to be in the same document
    if ancestor.document() != node.document() {
        return 0;
    }
    // avoid searching if ancestor or node is the root node
    if Some(ancestor) == node.document().map(|doc| doc.into()) {
        return 1;
    }
    if Some(node) == ancestor.document().map(|doc| doc.into()) {
        return 0;
    }
    while let Some(parent) = node.parent() {
        if parent == ancestor {
            return 1;
        }
        node = parent;
    }
    0
}

/// Traversal function for the "preceding" direction
/// the preceding axis contains all nodes in the same document as the context
/// node that are before the context node in document order, excluding any
/// ancestors and excluding attribute nodes and namespace nodes; the nodes are
/// ordered in reverse document order
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextPreceding")]
pub unsafe fn xml_xpath_next_preceding(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        let mut cur = cur.or_else(|| {
            let cur = (*(*ctxt).context).node?;
            if matches!(cur.element_type(), XmlElementType::XmlAttributeNode) {
                cur.parent()
            } else if let Ok(ns) = XmlNsPtr::try_from(cur) {
                ns.node
                    .filter(|node| node.element_type() != XmlElementType::XmlNamespaceDecl)
            } else {
                None
            }
        })?;
        if matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl) {
            return None;
        }
        if let Some(prev) = cur
            .prev()
            .filter(|p| matches!(p.element_type(), XmlElementType::XmlDTDNode))
        {
            cur = prev;
        }
        loop {
            if let Some(prev) = cur.prev() {
                cur = prev;
                while let Some(last) = cur.last() {
                    cur = last;
                }
                break Some(cur);
            }

            cur = cur.parent()?;
            if Some(cur) == (*(*ctxt).context).doc.unwrap().children {
                break None;
            }
            if xml_xpath_is_ancestor(Some(cur), (*(*ctxt).context).node) == 0 {
                break Some(cur);
            }
        }
    }
}

/// Traversal function for the "ancestor" direction
/// the ancestor axis contains the ancestors of the context node; the ancestors
/// of the context node consist of the parent of context node and the parent's
/// parent and so on; the nodes are ordered in reverse document order; thus the
/// parent is the first node on the axis, and the parent's parent is the second
/// node on the axis
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextAncestor")]
pub unsafe fn xml_xpath_next_ancestor(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        // the parent of an attribute or namespace node is the element
        // to which the attribute or namespace node is attached !!!!!!!!!!!!!
        let Some(cur) = cur else {
            let node = (*(*ctxt).context).node?;
            match node.element_type() {
                XmlElementType::XmlElementNode
                | XmlElementType::XmlTextNode
                | XmlElementType::XmlCDATASectionNode
                | XmlElementType::XmlEntityRefNode
                | XmlElementType::XmlEntityNode
                | XmlElementType::XmlPINode
                | XmlElementType::XmlCommentNode
                | XmlElementType::XmlDTDNode
                | XmlElementType::XmlElementDecl
                | XmlElementType::XmlAttributeDecl
                | XmlElementType::XmlEntityDecl
                | XmlElementType::XmlNotationNode
                | XmlElementType::XmlXIncludeStart
                | XmlElementType::XmlXIncludeEnd => {
                    let Some(parent) = node.parent() else {
                        return (*(*ctxt).context).doc.map(|doc| doc.into());
                    };
                    if XmlNodePtr::try_from(parent)
                        .ok()
                        .filter(|node| node.element_type() == XmlElementType::XmlElementNode)
                        .as_deref()
                        .and_then(|node| node.name())
                        .filter(|name| name.starts_with(' ') || name == "fake node libxslt")
                        .is_some()
                    {
                        return None;
                    }
                    return Some(parent);
                }
                XmlElementType::XmlAttributeNode => {
                    let tmp = XmlAttrPtr::try_from(node).unwrap();
                    return tmp.parent();
                }
                XmlElementType::XmlDocumentNode
                | XmlElementType::XmlDocumentTypeNode
                | XmlElementType::XmlDocumentFragNode
                | XmlElementType::XmlHTMLDocumentNode => {
                    return None;
                }
                XmlElementType::XmlNamespaceDecl => {
                    let ns = XmlNsPtr::try_from(node).unwrap();
                    if let Some(next) = ns.node.filter(|node| {
                        !matches!(node.element_type(), XmlElementType::XmlNamespaceDecl)
                    }) {
                        return Some(next);
                    }
                    // Bad, how did that namespace end up here ?
                    return None;
                }
                _ => unreachable!(),
            }
        };
        if Some(cur) == (*(*ctxt).context).doc.unwrap().children {
            return (*(*ctxt).context).doc.map(|doc| doc.into());
        }
        if Some(cur) == (*(*ctxt).context).doc.map(|doc| doc.into()) {
            return None;
        }
        match (*cur).element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDTDNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {
                let parent = cur.parent()?;

                if XmlNodePtr::try_from(parent)
                    .ok()
                    .filter(|node| node.element_type() == XmlElementType::XmlElementNode)
                    .as_deref()
                    .and_then(|node| node.name())
                    .filter(|name| name.starts_with(' ') || name == "fake node libxslt")
                    .is_some()
                {
                    return None;
                }
                Some(parent)
            }
            XmlElementType::XmlAttributeNode => {
                let att = XmlAttrPtr::try_from(cur).unwrap();
                att.parent.map(XmlGenericNodePtr::from)
            }
            XmlElementType::XmlNamespaceDecl => {
                let ns = XmlNsPtr::try_from(cur).unwrap();

                if let Some(next) = ns
                    .node
                    .filter(|node| !matches!(node.element_type(), XmlElementType::XmlNamespaceDecl))
                {
                    return Some(next);
                }
                // Bad, how did that namespace end up here ?
                None
            }
            XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => None,
            _ => unreachable!(),
        }
    }
}

/// Traversal function for the "preceding-sibling" direction
/// The preceding-sibling axis contains the preceding siblings of the context
/// node in reverse document order; the first preceding sibling is first on the
/// axis; the sibling preceding that node is the second on the axis and so on.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextPrecedingSibling")]
pub unsafe fn xml_xpath_next_preceding_sibling(
    ctxt: XmlXPathParserContextPtr,
    cur: Option<XmlGenericNodePtr>,
) -> Option<XmlGenericNodePtr> {
    unsafe {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return None;
        }
        let context_node = (*(*ctxt).context).node?;
        if matches!(
            context_node.element_type(),
            XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
        ) {
            return None;
        }
        if cur == (*(*ctxt).context).doc.map(|doc| doc.into()) {
            return None;
        }
        let Some(mut cur) = cur else {
            return context_node.prev();
        };
        if let Some(prev) = cur.prev().and_then(|p| XmlDtdPtr::try_from(p).ok()) {
            cur = prev.into();
        }
        cur.prev()
    }
}

/// Implement the last() XPath function
///    number last()
/// The last function returns the number of nodes in the context node list.
#[doc(alias = "xmlXPathLastFunction")]
pub unsafe fn xml_xpath_last_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 0);
        if (*(*ctxt).context).context_size >= 0 {
            (*ctxt).value_push(xml_xpath_cache_new_float(
                (*ctxt).context,
                (*(*ctxt).context).context_size as f64,
            ));
        } else {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidCtxtSize as i32);
        }
    }
}

/// Implement the position() XPath function
///    number position()
/// The position function returns the position of the context node in the
/// context node list. The first position is 1, and so the last position
/// will be equal to last().
#[doc(alias = "xmlXPathPositionFunction")]
pub unsafe fn xml_xpath_position_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 0);
        if (*(*ctxt).context).proximity_position >= 0 {
            (*ctxt).value_push(xml_xpath_cache_new_float(
                (*ctxt).context,
                (*(*ctxt).context).proximity_position as f64,
            ));
        } else {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidCtxtPosition as i32);
        }
    }
}

/// Implement the count() XPath function
///    number count(node-set)
#[doc(alias = "xmlXPathCountFunction")]
pub unsafe fn xml_xpath_count_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 1);
        if (*ctxt).value.is_null()
            || !matches!(
                (*(*ctxt).value).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidType as i32);
        }
        let cur: XmlXPathObjectPtr = (*ctxt).value_pop();

        if cur.is_null() {
            (*ctxt).value_push(xml_xpath_cache_new_float((*ctxt).context, 0.0));
        } else if let Some(nodeset) = (*cur).nodesetval.as_deref() {
            (*ctxt).value_push(xml_xpath_cache_new_float(
                (*ctxt).context,
                nodeset.len() as f64,
            ));
        } else {
            (*ctxt).value_push(xml_xpath_cache_new_float((*ctxt).context, 0.0));
        }
        xml_xpath_release_object((*ctxt).context, cur);
    }
}

/// Selects elements by their unique ID.
///
/// Returns a node-set of selected elements.
#[doc(alias = "xmlXPathGetElementsByIds")]
unsafe fn xml_xpath_get_elements_by_ids(
    doc: XmlDocPtr,
    mut ids: *const XmlChar,
) -> Option<Box<XmlNodeSet>> {
    unsafe {
        let mut cur: *const XmlChar = ids;
        let mut id: *mut XmlChar;

        if ids.is_null() {
            return None;
        }

        let mut ret = xml_xpath_node_set_create(None)?;

        while xml_is_blank_char(*cur as u32) {
            cur = cur.add(1);
        }
        while *cur != 0 {
            while !xml_is_blank_char(*cur as u32) && *cur != 0 {
                cur = cur.add(1);
            }

            id = xml_strndup(ids, cur.offset_from(ids) as _);
            if !id.is_null() {
                // We used to check the fact that the value passed
                // was an NCName, but this generated much troubles for
                // me and Aleksey Sanin, people blatantly violated that
                // constraint, like Visa3D spec.
                // if (xmlValidateNCName(ID, 1) == 0)
                if let Some(attr) = xml_get_id(doc, id) {
                    let elem = if let Ok(attr) = attr {
                        attr.parent.map(XmlGenericNodePtr::from)
                    // The following branch can not be reachable
                    // because `xml_get_id` can only return `XmlAttrPtr` or `XmlDocPtr`...
                    // What is the purpose of this branch ???
                    // } else if matches!(attr.element_type(), XmlElementType::XmlElementNode) {
                    //     Some(XmlGenericNodePtr::from(attr))
                    } else {
                        None
                    };
                    // TODO: Check memory error.
                    if let Some(elem) = elem {
                        ret.as_mut().add(elem);
                    }
                }
                xml_free(id as _);
            }

            while xml_is_blank_char(*cur as u32) {
                cur = cur.add(1);
            }
            ids = cur;
        }
        Some(ret)
    }
}

/// This is the cached version of xmlXPathConvertString().
/// Converts an existing object to its string() equivalent
///
/// Returns a created or reused object, the old one is freed (cached)
///         (or the operation is done directly on @val)
#[doc(alias = "xmlXPathCacheConvertString")]
unsafe fn xml_xpath_cache_convert_string(
    ctxt: XmlXPathContextPtr,
    val: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    unsafe {
        if val.is_null() {
            return xml_xpath_cache_new_string(ctxt, Some(""));
        }

        let mut res = None;
        match (*val).typ {
            XmlXPathObjectType::XPathUndefined => {}
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
                res = Some(xml_xpath_cast_node_set_to_string(
                    (*val).nodesetval.as_deref_mut(),
                ));
            }
            XmlXPathObjectType::XPathString => {
                return val;
            }
            XmlXPathObjectType::XPathBoolean => {
                res = Some(xml_xpath_cast_boolean_to_string((*val).boolval).into());
            }
            XmlXPathObjectType::XPathNumber => {
                res = Some(xml_xpath_cast_number_to_string((*val).floatval));
            }
            XmlXPathObjectType::XPathUsers => {
                todo!();
            }
            #[cfg(feature = "libxml_xptr_locs")]
            XmlXPathObjectType::XPathPoint
            | XmlXPathObjectType::XPathRange
            | XmlXPathObjectType::XPathLocationset => {
                todo!()
            }
        }
        xml_xpath_release_object(ctxt, val);
        let Some(res) = res else {
            return xml_xpath_cache_new_string(ctxt, Some(""));
        };
        xml_xpath_cache_wrap_string(ctxt, Some(&res))
    }
}

/// Implement the id() XPath function
///    node-set id(object)
/// The id function selects elements by their unique ID
/// (see [5.2.1 Unique IDs]). When the argument to id is of type node-set,
/// then the result is the union of the result of applying id to the
/// string value of each of the nodes in the argument node-set. When the
/// argument to id is of any other type, the argument is converted to a
/// string as if by a call to the string function; the string is split
/// into a whitespace-separated list of tokens (whitespace is any sequence
/// of characters matching the production S); the result is a node-set
/// containing the elements in the same document as the context node that
/// have a unique ID equal to any of the tokens in the list.
#[doc(alias = "xmlXPathIdFunction")]
pub unsafe fn xml_xpath_id_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 1);
        let mut obj = (*ctxt).value_pop();
        if obj.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidOperand as i32);
        }
        if matches!(
            (*obj).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        ) {
            // TODO: Check memory error.
            let mut ret = xml_xpath_node_set_create(None);

            if let Some(nodeset) = (*obj).nodesetval.as_deref() {
                for &node in &nodeset.node_tab {
                    let tokens = xml_xpath_cast_node_to_string(Some(node));
                    let tokens = CString::new(tokens).unwrap();
                    let ns = xml_xpath_get_elements_by_ids(
                        (*(*ctxt).context).doc.unwrap(),
                        tokens.as_ptr() as *const u8,
                    );
                    // TODO: Check memory error.
                    ret = xml_xpath_node_set_merge(ret, ns.as_deref());
                    xml_xpath_free_node_set(ns);
                }
            }
            xml_xpath_release_object((*ctxt).context, obj);
            (*ctxt).value_push(xml_xpath_cache_wrap_node_set((*ctxt).context, ret));
            return;
        }
        obj = xml_xpath_cache_convert_string((*ctxt).context, obj);
        if obj.is_null() {
            return;
        }
        let strval = (*obj)
            .stringval
            .as_deref()
            .map(|s| CString::new(s).unwrap());
        let ret = xml_xpath_get_elements_by_ids(
            (*(*ctxt).context).doc.unwrap(),
            strval
                .as_deref()
                .map_or(null_mut(), |s| s.as_ptr() as *const u8),
        );
        (*ctxt).value_push(xml_xpath_cache_wrap_node_set((*ctxt).context, ret));
        xml_xpath_release_object((*ctxt).context, obj);
    }
}

/// Implement the local-name() XPath function
///    string local-name(node-set?)
/// The local-name function returns a string containing the local part
/// of the name of the node in the argument node-set that is first in
/// document order. If the node-set is empty or the first node has no
/// name, an empty string is returned. If the argument is omitted it
/// defaults to the context node.
#[doc(alias = "xmlXPathLocalNameFunction")]
pub unsafe fn xml_xpath_local_name_function(ctxt: XmlXPathParserContextPtr, mut nargs: i32) {
    unsafe {
        if ctxt.is_null() {
            return;
        }

        if nargs == 0 {
            (*ctxt).value_push(xml_xpath_cache_new_node_set(
                (*ctxt).context,
                (*(*ctxt).context).node,
            ));
            nargs = 1;
        }

        CHECK_ARITY!(ctxt, nargs, 1);
        if (*ctxt).value.is_null()
            || !matches!(
                (*(*ctxt).value).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidType as i32);
        }
        let cur: XmlXPathObjectPtr = (*ctxt).value_pop();

        if let Some(nodeset) = (*cur).nodesetval.as_deref() {
            if !nodeset.node_tab.is_empty() {
                let table = &nodeset.node_tab;
                let i = 0; /* Should be first in document order !!!!! */
                match table[i].element_type() {
                    XmlElementType::XmlElementNode
                    | XmlElementType::XmlAttributeNode
                    | XmlElementType::XmlPINode => {
                        if table[i].name().is_some_and(|name| name.starts_with(' ')) {
                            (*ctxt)
                                .value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
                        } else {
                            (*ctxt).value_push(xml_xpath_cache_new_string(
                                (*ctxt).context,
                                (*table[i]).name().as_deref(),
                            ));
                        }
                    }
                    XmlElementType::XmlNamespaceDecl => {
                        let ns = XmlNsPtr::try_from(table[i]).unwrap();
                        let prefix = ns.prefix();
                        let value = xml_xpath_cache_new_string((*ctxt).context, prefix.as_deref());
                        (*ctxt).value_push(value);
                    }
                    _ => {
                        (*ctxt).value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
                    }
                }
            } else {
                (*ctxt).value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
            }
        } else {
            (*ctxt).value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
        }
        xml_xpath_release_object((*ctxt).context, cur);
    }
}

/// Implement the namespace-uri() XPath function
///    string namespace-uri(node-set?)
/// The namespace-uri function returns a string containing the
/// namespace URI of the expanded name of the node in the argument
/// node-set that is first in document order. If the node-set is empty,
/// the first node has no name, or the expanded name has no namespace
/// URI, an empty string is returned. If the argument is omitted it
/// defaults to the context node.
#[doc(alias = "xmlXPathNamespaceURIFunction")]
pub unsafe fn xml_xpath_namespace_uri_function(ctxt: XmlXPathParserContextPtr, mut nargs: i32) {
    unsafe {
        if ctxt.is_null() {
            return;
        }

        if nargs == 0 {
            (*ctxt).value_push(xml_xpath_cache_new_node_set(
                (*ctxt).context,
                (*(*ctxt).context).node,
            ));
            nargs = 1;
        }
        CHECK_ARITY!(ctxt, nargs, 1);
        if (*ctxt).value.is_null()
            || !matches!(
                (*(*ctxt).value).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidType as i32);
        }
        let cur: XmlXPathObjectPtr = (*ctxt).value_pop();

        if let Some(nodeset) = (*cur).nodesetval.as_deref() {
            if !nodeset.node_tab.is_empty() {
                let table = &nodeset.node_tab;
                let i = 0; /* Should be first in document order !!!!! */
                match table[i].element_type() {
                    XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
                        if let Ok(Some(ns)) = XmlNodePtr::try_from(table[i])
                            .map(|node| node.ns)
                            .or_else(|_| XmlAttrPtr::try_from(table[i]).map(|attr| attr.ns))
                        {
                            let href = ns.href;
                            (*ctxt).value_push(xml_xpath_cache_new_string(
                                (*ctxt).context,
                                (!href.is_null())
                                    .then(|| CStr::from_ptr(href as *const i8).to_string_lossy())
                                    .as_deref(),
                            ));
                        } else {
                            (*ctxt)
                                .value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
                        }
                    }
                    _ => {
                        (*ctxt).value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
                    }
                }
            } else {
                (*ctxt).value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
            }
        } else {
            (*ctxt).value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
        }
        xml_xpath_release_object((*ctxt).context, cur);
    }
}

/// Implement the string() XPath function
///    string string(object?)
/// The string function converts an object to a string as follows:
///    - A node-set is converted to a string by returning the value of
///      the node in the node-set that is first in document order.
///      If the node-set is empty, an empty string is returned.
///    - A number is converted to a string as follows
///      + NaN is converted to the string NaN
///      + positive zero is converted to the string 0
///      + negative zero is converted to the string 0
///      + positive infinity is converted to the string Infinity
///      + negative infinity is converted to the string -Infinity
///      + if the number is an integer, the number is represented in
///        decimal form as a Number with no decimal point and no leading
///        zeros, preceded by a minus sign (-) if the number is negative
///      + otherwise, the number is represented in decimal form as a
///        Number including a decimal point with at least one digit
///        before the decimal point and at least one digit after the
///        decimal point, preceded by a minus sign (-) if the number
///        is negative; there must be no leading zeros before the decimal
///        point apart possibly from the one required digit immediately
///        before the decimal point; beyond the one required digit
///        after the decimal point there must be as many, but only as
///        many, more digits as are needed to uniquely distinguish the
///        number from all other IEEE 754 numeric values.
///    - The boolean false value is converted to the string false.
///      The boolean true value is converted to the string true.
///
/// If the argument is omitted, it defaults to a node-set with the
/// context node as its only member.
#[doc(alias = "xmlXPathStringFunction")]
pub unsafe fn xml_xpath_string_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        if nargs == 0 {
            let val = xml_xpath_cast_node_to_string((*(*ctxt).context).node);
            (*ctxt).value_push(xml_xpath_cache_wrap_string((*ctxt).context, Some(&val)));
            return;
        }

        CHECK_ARITY!(ctxt, nargs, 1);
        let cur: XmlXPathObjectPtr = (*ctxt).value_pop();
        if cur.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidOperand as i32);
        }
        (*ctxt).value_push(xml_xpath_cache_convert_string((*ctxt).context, cur));
    }
}

/// Implement the string-length() XPath function
///    number string-length(string?)
/// The string-length returns the number of characters in the string
/// (see [3.6 Strings]). If the argument is omitted, it defaults to
/// the context node converted to a string, in other words the value
/// of the context node.
#[doc(alias = "xmlXPathStringLengthFunction")]
pub unsafe fn xml_xpath_string_length_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        if nargs == 0 {
            if ctxt.is_null() || (*ctxt).context.is_null() {
                return;
            }
            if let Some(context_node) = (*(*ctxt).context).node {
                let content = xml_xpath_cast_node_to_string(Some(context_node));
                (*ctxt).value_push(xml_xpath_cache_new_float(
                    (*ctxt).context,
                    content.chars().count() as f64,
                ));
            } else {
                (*ctxt).value_push(xml_xpath_cache_new_float((*ctxt).context, 0.0));
            }
            return;
        }
        CHECK_ARITY!(ctxt, nargs, 1);
        CAST_TO_STRING!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathString);
        let cur: XmlXPathObjectPtr = (*ctxt).value_pop();
        (*ctxt).value_push(xml_xpath_cache_new_float(
            (*ctxt).context,
            (*cur).stringval.as_deref().map_or(0, |s| s.chars().count()) as _,
        ));
        xml_xpath_release_object((*ctxt).context, cur);
    }
}

/// Implement the concat() XPath function
///    string concat(string, string, string*)
/// The concat function returns the concatenation of its arguments.
#[doc(alias = "xmlXPathConcatFunction")]
pub unsafe fn xml_xpath_concat_function(ctxt: XmlXPathParserContextPtr, mut nargs: i32) {
    unsafe {
        let mut newobj: XmlXPathObjectPtr;

        if ctxt.is_null() {
            return;
        }
        if nargs < 2 {
            CHECK_ARITY!(ctxt, nargs, 2);
        }

        CAST_TO_STRING!(ctxt);
        let cur: XmlXPathObjectPtr = (*ctxt).value_pop();
        if cur.is_null() || !matches!((*cur).typ, XmlXPathObjectType::XPathString) {
            xml_xpath_release_object((*ctxt).context, cur);
            return;
        }
        nargs -= 1;

        while nargs > 0 {
            CAST_TO_STRING!(ctxt);
            newobj = (*ctxt).value_pop();
            if newobj.is_null() || (*newobj).typ != XmlXPathObjectType::XPathString {
                xml_xpath_release_object((*ctxt).context, newobj);
                xml_xpath_release_object((*ctxt).context, cur);
                XP_ERROR!(ctxt, XmlXPathError::XPathInvalidType as i32);
            }
            let mut tmp = (*newobj).stringval.take();
            if let Some(curstr) = (*cur).stringval.take() {
                tmp.get_or_insert_with(String::new).push_str(&curstr);
                (*newobj).stringval = Some(curstr);
            }
            (*cur).stringval = tmp;
            xml_xpath_release_object((*ctxt).context, newobj);
            nargs -= 1;
        }
        (*ctxt).value_push(cur);
    }
}

/// Implement the contains() XPath function
///    boolean contains(string, string)
/// The contains function returns true if the first argument string
/// contains the second argument string, and otherwise returns false.
#[doc(alias = "xmlXPathContainsFunction")]
pub unsafe fn xml_xpath_contains_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 2);
        CAST_TO_STRING!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathString);
        let needle: XmlXPathObjectPtr = (*ctxt).value_pop();
        CAST_TO_STRING!(ctxt);
        let hay: XmlXPathObjectPtr = (*ctxt).value_pop();

        if hay.is_null() || (*hay).typ != XmlXPathObjectType::XPathString {
            xml_xpath_release_object((*ctxt).context, hay);
            xml_xpath_release_object((*ctxt).context, needle);
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidType as i32);
        }
        if (*needle)
            .stringval
            .as_deref()
            .filter(|&s| {
                (*hay)
                    .stringval
                    .as_deref()
                    .expect("Internal Error")
                    .contains(s)
            })
            .is_some()
        {
            (*ctxt).value_push(xml_xpath_cache_new_boolean((*ctxt).context, true));
        } else {
            (*ctxt).value_push(xml_xpath_cache_new_boolean((*ctxt).context, false));
        }
        xml_xpath_release_object((*ctxt).context, hay);
        xml_xpath_release_object((*ctxt).context, needle);
    }
}

/// Implement the starts-with() XPath function
///    boolean starts-with(string, string)
/// The starts-with function returns true if the first argument string
/// starts with the second argument string, and otherwise returns false.
#[doc(alias = "xmlXPathStartsWithFunction")]
pub unsafe fn xml_xpath_starts_with_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 2);
        CAST_TO_STRING!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathString);
        let needle: XmlXPathObjectPtr = (*ctxt).value_pop();
        CAST_TO_STRING!(ctxt);
        let hay: XmlXPathObjectPtr = (*ctxt).value_pop();

        if hay.is_null() || (*hay).typ != XmlXPathObjectType::XPathString {
            xml_xpath_release_object((*ctxt).context, hay);
            xml_xpath_release_object((*ctxt).context, needle);
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidType as i32);
        }
        let m = (*hay).stringval.as_deref().map_or(0, |s| s.len());
        let n = (*needle).stringval.as_deref().map_or(0, |s| s.len());
        if &(*hay).stringval.as_deref().unwrap()[..n.min(m)]
            != (*needle).stringval.as_deref().expect("Internal Error")
        {
            (*ctxt).value_push(xml_xpath_cache_new_boolean((*ctxt).context, false));
        } else {
            (*ctxt).value_push(xml_xpath_cache_new_boolean((*ctxt).context, true));
        }
        xml_xpath_release_object((*ctxt).context, hay);
        xml_xpath_release_object((*ctxt).context, needle);
    }
}

/// Implement the substring() XPath function
///    string substring(string, number, number?)
/// The substring function returns the substring of the first argument
/// starting at the position specified in the second argument with
/// length specified in the third argument. For example,
/// substring("12345",2,3) returns "234". If the third argument is not
/// specified, it returns the substring starting at the position specified
/// in the second argument and continuing to the end of the string. For
/// example, substring("12345",2) returns "2345".  More precisely, each
/// character in the string (see [3.6 Strings]) is considered to have a
/// numeric position: the position of the first character is 1, the position
/// of the second character is 2 and so on. The returned substring contains
/// those characters for which the position of the character is greater than
/// or equal to the second argument and, if the third argument is specified,
/// less than the sum of the second and third arguments; the comparisons
/// and addition used for the above follow the standard IEEE 754 rules. Thus:
///  - substring("12345", 1.5, 2.6) returns "234"
///  - substring("12345", 0, 3) returns "12"
///  - substring("12345", 0 div 0, 3) returns ""
///  - substring("12345", 1, 0 div 0) returns ""
///  - substring("12345", -42, 1 div 0) returns "12345"
///  - substring("12345", -1 div 0, 1 div 0) returns ""
#[doc(alias = "xmlXPathSubstringFunction")]
pub unsafe fn xml_xpath_substring_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        let len: XmlXPathObjectPtr;
        let mut le: f64 = 0.0;
        let mut i: i32 = 1;
        let mut j: i32 = INT_MAX;

        if nargs < 2 {
            CHECK_ARITY!(ctxt, nargs, 2);
        }
        if nargs > 3 {
            CHECK_ARITY!(ctxt, nargs, 3);
        }
        // take care of possible last (position) argument
        if nargs == 3 {
            CAST_TO_NUMBER!(ctxt);
            CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
            len = (*ctxt).value_pop();
            le = (*len).floatval;
            xml_xpath_release_object((*ctxt).context, len);
        }

        CAST_TO_NUMBER!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
        let start: XmlXPathObjectPtr = (*ctxt).value_pop();
        let input: f64 = (*start).floatval;
        xml_xpath_release_object((*ctxt).context, start);
        CAST_TO_STRING!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathString);
        let str: XmlXPathObjectPtr = (*ctxt).value_pop();

        if !matches!(
            input.partial_cmp(&(INT_MAX as f64)),
            Some(std::cmp::Ordering::Less)
        ) {
            // Logical NOT to handle NaNs
            i = INT_MAX;
        } else if input >= 1.0 {
            i = input as _;
            if input - input.floor() >= 0.5 {
                i += 1;
            }
        }

        if nargs == 3 {
            let mut rin: f64;
            let mut rle: f64;

            rin = input.floor();
            if input - rin >= 0.5 {
                rin += 1.0;
            }

            rle = le.floor();
            if le - rle >= 0.5 {
                rle += 1.0;
            }

            let end: f64 = rin + rle;
            if !matches!(
                end.partial_cmp(&1.0),
                Some(std::cmp::Ordering::Equal) | Some(std::cmp::Ordering::Greater)
            ) {
                // Logical NOT to handle NaNs
                j = 1;
            } else if end < INT_MAX as f64 {
                j = end as _;
            }
        }

        if i < j {
            let ret = (*str)
                .stringval
                .as_deref()
                .expect("Internal Error")
                .chars()
                .skip(i as usize - 1)
                .take((j - i) as usize)
                .collect::<String>();
            (*ctxt).value_push(xml_xpath_cache_new_string((*ctxt).context, Some(&ret)));
        } else {
            (*ctxt).value_push(xml_xpath_cache_new_string((*ctxt).context, Some("")));
        }

        xml_xpath_release_object((*ctxt).context, str);
    }
}

/// Implement the substring-before() XPath function
///    string substring-before(string, string)
/// The substring-before function returns the substring of the first
/// argument string that precedes the first occurrence of the second
/// argument string in the first argument string, or the empty string
/// if the first argument string does not contain the second argument
/// string. For example, substring-before("1999/04/01","/") returns 1999.
#[doc(alias = "xmlXPathSubstringBeforeFunction")]
pub unsafe fn xml_xpath_substring_before_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 2);
        CAST_TO_STRING!(ctxt);
        let find: XmlXPathObjectPtr = (*ctxt).value_pop();
        CAST_TO_STRING!(ctxt);
        let str: XmlXPathObjectPtr = (*ctxt).value_pop();

        let ss = (*str).stringval.as_deref().unwrap();
        let fs = (*find).stringval.as_deref().unwrap();
        let target = ss.find(fs).map(|pos| ss[..pos].to_owned());
        (*ctxt).value_push(xml_xpath_cache_new_string(
            (*ctxt).context,
            target.as_deref(),
        ));
        xml_xpath_release_object((*ctxt).context, str);
        xml_xpath_release_object((*ctxt).context, find);
    }
}

/// Implement the substring-after() XPath function
///    string substring-after(string, string)
/// The substring-after function returns the substring of the first
/// argument string that follows the first occurrence of the second
/// argument string in the first argument string, or the empty stringi
/// if the first argument string does not contain the second argument
/// string. For example, substring-after("1999/04/01","/") returns 04/01,
/// and substring-after("1999/04/01","19") returns 99/04/01.
#[doc(alias = "xmlXPathSubstringAfterFunction")]
pub unsafe fn xml_xpath_substring_after_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 2);
        CAST_TO_STRING!(ctxt);
        let find: XmlXPathObjectPtr = (*ctxt).value_pop();
        CAST_TO_STRING!(ctxt);
        let str: XmlXPathObjectPtr = (*ctxt).value_pop();

        let ss = (*str).stringval.as_deref().unwrap();
        let fs = (*find).stringval.as_deref().unwrap();
        let target = ss.find(fs).map(|pos| ss[pos..].to_owned());
        (*ctxt).value_push(xml_xpath_cache_new_string(
            (*ctxt).context,
            target.as_deref(),
        ));
        xml_xpath_release_object((*ctxt).context, str);
        xml_xpath_release_object((*ctxt).context, find);
    }
}

/// Implement the normalize-space() XPath function
///    string normalize-space(string?)
/// The normalize-space function returns the argument string with white
/// space normalized by stripping leading and trailing whitespace
/// and replacing sequences of whitespace characters by a single
/// space. Whitespace characters are the same allowed by the S production
/// in XML. If the argument is omitted, it defaults to the context
/// node converted to a string, in other words the value of the context node.
#[doc(alias = "xmlXPathNormalizeFunction")]
pub unsafe fn xml_xpath_normalize_function(ctxt: XmlXPathParserContextPtr, mut nargs: i32) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        if nargs == 0 {
            // Use current context node
            let val = xml_xpath_cast_node_to_string((*(*ctxt).context).node);
            (*ctxt).value_push(xml_xpath_cache_wrap_string((*ctxt).context, Some(&val)));
            nargs = 1;
        }

        CHECK_ARITY!(ctxt, nargs, 1);
        CAST_TO_STRING!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathString);
        let Some(source) = (*(*ctxt).value).stringval.as_deref_mut() else {
            return;
        };
        let oldlen = source.len();
        // Skip leading whitespaces
        let Some(start) = source.find(|c| !xml_is_blank_char(c as u32)) else {
            (*(*ctxt).value).stringval.as_mut().unwrap().clear();
            return;
        };
        let target = source.as_bytes_mut();

        // Collapse intermediate whitespaces, and skip trailing whitespaces
        let mut written = 0;
        let mut blank = false;
        for i in start..oldlen {
            let c = target[i];
            if xml_is_blank_char(c as u32) {
                blank = true;
            } else {
                if blank {
                    target[written] = 0x20;
                    written += 1;
                    blank = false;
                }
                target[written] = c;
                written += 1;
            }
        }
        (*(*ctxt).value)
            .stringval
            .as_mut()
            .unwrap()
            .truncate(written);
    }
}

/// Implement the translate() XPath function
///    string translate(string, string, string)
/// The translate function returns the first argument string with
/// occurrences of characters in the second argument string replaced
/// by the character at the corresponding position in the third argument
/// string. For example, translate("bar","abc","ABC") returns the string
/// BAr. If there is a character in the second argument string with no
/// character at a corresponding position in the third argument string
/// (because the second argument string is longer than the third argument
/// string), then occurrences of that character in the first argument
/// string are removed. For example, translate("--aaa--","abc-","ABC")
/// returns "AAA". If a character occurs more than once in second
/// argument string, then the first occurrence determines the replacement
/// character. If the third argument string is longer than the second
/// argument string, then excess characters are ignored.
#[doc(alias = "xmlXPathTranslateFunction")]
pub unsafe fn xml_xpath_translate_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 3);

        CAST_TO_STRING!(ctxt);
        let to: XmlXPathObjectPtr = (*ctxt).value_pop();
        CAST_TO_STRING!(ctxt);
        let from: XmlXPathObjectPtr = (*ctxt).value_pop();
        CAST_TO_STRING!(ctxt);
        let str: XmlXPathObjectPtr = (*ctxt).value_pop();

        let to_str = (*to).stringval.as_deref().unwrap();
        let from_str = (*from).stringval.as_deref().unwrap();
        let arg = (*str).stringval.as_deref().unwrap();
        let mut target = String::with_capacity(arg.len());
        for c in arg.chars() {
            if let Some((_, replace)) = from_str
                .chars()
                .zip(to_str.chars().map(Ok).chain(repeat(Err(()))))
                .find(|e| e.0 == c)
            {
                if let Ok(c) = replace {
                    target.push(c);
                }
            } else {
                target.push(c);
            }
        }
        (*ctxt).value_push(xml_xpath_cache_new_string((*ctxt).context, Some(&target)));
        xml_xpath_release_object((*ctxt).context, str);
        xml_xpath_release_object((*ctxt).context, from);
        xml_xpath_release_object((*ctxt).context, to);
    }
}

/// Implement the not() XPath function
///    boolean not(boolean)
/// The not function returns true if its argument is false,
/// and false otherwise.
#[doc(alias = "xmlXPathNotFunction")]
pub unsafe fn xml_xpath_not_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 1);
        CAST_TO_BOOLEAN!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathBoolean);
        (*(*ctxt).value).boolval = !(*(*ctxt).value).boolval;
    }
}

/// Implement the true() XPath function
///    boolean true()
#[doc(alias = "xmlXPathTrueFunction")]
pub unsafe fn xml_xpath_true_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 0);
        (*ctxt).value_push(xml_xpath_cache_new_boolean((*ctxt).context, true));
    }
}

/// Implement the false() XPath function
///    boolean false()
#[doc(alias = "xmlXPathFalseFunction")]
pub unsafe fn xml_xpath_false_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 0);
        (*ctxt).value_push(xml_xpath_cache_new_boolean((*ctxt).context, false));
    }
}

/// Implement the lang() XPath function
///    boolean lang(string)
/// The lang function returns true or false depending on whether the
/// language of the context node as specified by xml:lang attributes
/// is the same as or is a sublanguage of the language specified by
/// the argument string. The language of the context node is determined
/// by the value of the xml:lang attribute on the context node, or, if
/// the context node has no xml:lang attribute, by the value of the
/// xml:lang attribute on the nearest ancestor of the context node that
/// has an xml:lang attribute. If there is no such attribute, then lang
/// returns false. If there is such an attribute, then lang returns
/// true if the attribute value is equal to the argument ignoring case,
/// or if there is some suffix starting with - such that the attribute
/// value is equal to the argument ignoring that suffix of the attribute
/// value and ignoring case.
#[doc(alias = "xmlXPathLangFunction")]
pub unsafe fn xml_xpath_lang_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        let mut ret: i32 = 0;

        CHECK_ARITY!(ctxt, nargs, 1);
        CAST_TO_STRING!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathString);
        let val = (*ctxt).value_pop();
        let lang = (*val).stringval.as_deref();
        let the_lang = (*(*ctxt).context).node.unwrap().get_lang();
        'not_equal: {
            if let (Some(the_lang), Some(lang)) = (the_lang, lang) {
                let the_lang = the_lang.as_bytes();
                let lang = lang.as_bytes();
                let mut i = 0;
                while i < lang.len() {
                    if !lang[i].eq_ignore_ascii_case(the_lang.get(i).unwrap_or(&0)) {
                        break 'not_equal;
                    }
                    i += 1;
                }
                if the_lang.get(i).unwrap_or(&0) == &0 || the_lang[i] == b'-' {
                    ret = 1;
                }
            }
        }
        // not_equal:

        xml_xpath_release_object((*ctxt).context, val);
        (*ctxt).value_push(xml_xpath_cache_new_boolean((*ctxt).context, ret != 0));
    }
}

/// This is the cached version of xmlXPathConvertNumber().
/// Converts an existing object to its number() equivalent
///
/// Returns a created or reused object, the old one is freed (or the operation
///         is done directly on @val)
#[doc(alias = "xmlXPathCacheConvertNumber")]
unsafe fn xml_xpath_cache_convert_number(
    ctxt: XmlXPathContextPtr,
    val: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    unsafe {
        if val.is_null() {
            return xml_xpath_cache_new_float(ctxt, 0.0);
        }
        if matches!((*val).typ, XmlXPathObjectType::XPathNumber) {
            return val;
        }
        let ret: XmlXPathObjectPtr = xml_xpath_cache_new_float(ctxt, xml_xpath_cast_to_number(val));
        xml_xpath_release_object(ctxt, val);
        ret
    }
}

/// Implement the number() XPath function
///    number number(object?)
#[doc(alias = "xmlXPathNumberFunction")]
pub unsafe fn xml_xpath_number_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        let res: f64;

        if ctxt.is_null() {
            return;
        }
        if nargs == 0 {
            if let Some(context_node) = (*(*ctxt).context).node {
                let content = context_node.get_content().map(|c| CString::new(c).unwrap());
                let content = content
                    .as_ref()
                    .map_or(null_mut(), |c| c.as_ptr() as *mut u8);
                res = xml_xpath_string_eval_number(content);
                (*ctxt).value_push(xml_xpath_cache_new_float((*ctxt).context, res));
            } else {
                (*ctxt).value_push(xml_xpath_cache_new_float((*ctxt).context, 0.0));
            }
            return;
        }

        CHECK_ARITY!(ctxt, nargs, 1);
        let cur: XmlXPathObjectPtr = (*ctxt).value_pop();
        (*ctxt).value_push(xml_xpath_cache_convert_number((*ctxt).context, cur));
    }
}

/// Implement the sum() XPath function
///    number sum(node-set)
/// The sum function returns the sum of the values of the nodes in
/// the argument node-set.
#[doc(alias = "xmlXPathSumFunction")]
pub unsafe fn xml_xpath_sum_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        let mut res: f64 = 0.0;

        CHECK_ARITY!(ctxt, nargs, 1);
        if (*ctxt).value.is_null()
            || !matches!(
                (*(*ctxt).value).typ,
                XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
            )
        {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidType as i32);
        }
        let cur: XmlXPathObjectPtr = (*ctxt).value_pop();

        if let Some(nodeset) = (*cur).nodesetval.as_deref().filter(|n| !n.is_empty()) {
            if !nodeset.node_tab.is_empty() {
                for &node in &nodeset.node_tab {
                    res += xml_xpath_cast_node_to_number(Some(node));
                }
            }
        }
        (*ctxt).value_push(xml_xpath_cache_new_float((*ctxt).context, res));
        xml_xpath_release_object((*ctxt).context, cur);
    }
}

/// Implement the floor() XPath function
///    number floor(number)
/// The floor function returns the largest (closest to positive infinity)
/// number that is not greater than the argument and that is an integer.
#[doc(alias = "xmlXPathFloorFunction")]
pub unsafe fn xml_xpath_floor_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 1);
        CAST_TO_NUMBER!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);

        (*(*ctxt).value).floatval = (*(*ctxt).value).floatval.floor();
    }
}

/// Implement the ceiling() XPath function
///    number ceiling(number)
/// The ceiling function returns the smallest (closest to negative infinity)
/// number that is not less than the argument and that is an integer.
#[doc(alias = "xmlXPathCeilingFunction")]
pub unsafe fn xml_xpath_ceiling_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 1);
        CAST_TO_NUMBER!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);

        (*(*ctxt).value).floatval = (*(*ctxt).value).floatval.ceil();
    }
}

/// Implement the round() XPath function
///    number round(number)
/// The round function returns the number that is closest to the
/// argument and that is an integer. If there are two such numbers,
/// then the one that is closest to positive infinity is returned.
#[doc(alias = "xmlXPathRoundFunction")]
pub unsafe fn xml_xpath_round_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        CHECK_ARITY!(ctxt, nargs, 1);
        CAST_TO_NUMBER!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);

        let f: f64 = (*(*ctxt).value).floatval;

        if (-0.5..0.5).contains(&f) {
            /* Handles negative zero. */
            (*(*ctxt).value).floatval *= 0.0;
        } else {
            let mut rounded: f64 = f.floor();
            if f - rounded >= 0.5 {
                rounded += 1.0;
            }
            (*(*ctxt).value).floatval = rounded;
        }
    }
}

/// This is the cached version of xmlXPathConvertBoolean().
/// Converts an existing object to its boolean() equivalent
///
/// Returns a created or reused object, the old one is freed (or the operation
/// is done directly on @val)
#[doc(alias = "xmlXPathCacheConvertBoolean")]
unsafe fn xml_xpath_cache_convert_boolean(
    ctxt: XmlXPathContextPtr,
    val: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    unsafe {
        if val.is_null() {
            return xml_xpath_cache_new_boolean(ctxt, false);
        }
        if matches!((*val).typ, XmlXPathObjectType::XPathBoolean) {
            return val;
        }
        let ret: XmlXPathObjectPtr =
            xml_xpath_cache_new_boolean(ctxt, xml_xpath_cast_to_boolean(val));
        xml_xpath_release_object(ctxt, val);
        ret
    }
}

/// Implement the boolean() XPath function
///    boolean boolean(object)
/// The boolean function converts its argument to a boolean as follows:
///    - a number is true if and only if it is neither positive or
///      negative zero nor NaN
///    - a node-set is true if and only if it is non-empty
///    - a string is true if and only if its length is non-zero
#[doc(alias = "xmlXPathBooleanFunction")]
pub unsafe fn xml_xpath_boolean_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    unsafe {
        let mut cur: XmlXPathObjectPtr;

        CHECK_ARITY!(ctxt, nargs, 1);
        cur = (*ctxt).value_pop();
        if cur.is_null() {
            XP_ERROR!(ctxt, XmlXPathError::XPathInvalidOperand as i32);
        }
        cur = xml_xpath_cache_convert_boolean((*ctxt).context, cur);
        (*ctxt).value_push(cur);
    }
}

/// Namespace nodes in libxml don't match the XPath semantic. In a node set
/// the namespace nodes are duplicated and the next pointer is set to the
/// parent node in the XPath semantic. Check if such a node needs to be freed
#[doc(alias = "xmlXPathNodeSetFreeNs")]
#[cfg(feature = "xpath")]
pub(crate) unsafe fn xml_xpath_node_set_free_ns(ns: XmlNsPtr) {
    unsafe {
        if !matches!(ns.typ, XmlElementType::XmlNamespaceDecl) {
            return;
        }

        if ns
            .node
            .is_some_and(|node| !matches!(node.element_type(), XmlElementType::XmlNamespaceDecl))
        {
            if !ns.href.is_null() {
                xml_free(ns.href as _);
            }
            if !ns.prefix.is_null() {
                xml_free(ns.prefix as _);
            }
            ns.free();
        }
    }
}

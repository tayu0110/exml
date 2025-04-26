//! Provide internal methods and data structures for processing XPath.
//!
//! This module is based on `libxml/xpathInternals.h`, `xpath.c` and so on in `libxml2-v2.11.8`.  
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
    mem::size_of,
    os::raw::c_void,
    ptr::null_mut,
};

use libc::memset;

#[cfg(feature = "libxml_xptr_locs")]
use crate::libxml::xpointer::XmlLocationSet;
use crate::{
    error::{__xml_raise_error, XmlErrorDomain, XmlErrorLevel, XmlParserErrors},
    generic_error,
    hash::XmlHashTableRef,
    libxml::{
        chvalid::xml_is_blank_char,
        globals::{xml_free, xml_malloc, xml_realloc},
        valid::xml_get_id,
        xmlstring::{XmlChar, xml_strndup},
    },
    tree::{
        NodeCommon, XML_XML_NAMESPACE, XmlAttrPtr, XmlDocPtr, XmlDtdPtr, XmlElementType,
        XmlGenericNodePtr, XmlNodePtr, XmlNs, XmlNsPtr,
    },
    xpath::{
        XML_XPATH_NAN, XmlXPathContextPtr, XmlXPathError, XmlXPathObject, XmlXPathObjectPtr,
        XmlXPathObjectType, XmlXPathOp, XmlXPathParserContextPtr, XmlXPathVariableLookupFunc,
        functions::{cast_to_number, xml_xpath_number_function},
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
    XmlNodeSet, XmlXPathContext, XmlXPathParserContext, compile::XmlXPathStepOpPtr,
    functions::xml_xpath_boolean_function, xml_xpath_new_boolean, xml_xpath_new_float,
    xml_xpath_new_node_set, xml_xpath_new_string, xml_xpath_node_set_merge,
    xml_xpath_wrap_node_set, xml_xpath_wrap_string,
};

// Many of these macros may later turn into functions.
// They shouldn't be used in #ifdef's preprocessor instructions.

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
/// Return(0) in case of failure
macro_rules! CHECK_TYPE0 {
    ($ctxt:expr, $typeval:expr) => {
        if (*$ctxt).value.is_null() || (*(*$ctxt).value).typ != $typeval {
            XP_ERROR0!($ctxt, XmlXPathError::XPathInvalidType as i32)
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
                    xml_xpath_err_memory(None, Some("xmlPointerListAddSize: re-allocating item\n"));
                    return -1;
                }
                new_size = (*list).size as usize * 2;
            }
            let tmp: *mut *mut c_void =
                xml_realloc((*list).items as _, new_size * size_of::<*mut c_void>())
                    as *mut *mut c_void;
            if tmp.is_null() {
                xml_xpath_err_memory(None, Some("xmlPointerListAddSize: re-allocating item\n"));
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
unsafe fn xml_pointer_list_create(initial_size: i32) -> XmlPointerListPtr {
    unsafe {
        let ret: XmlPointerListPtr = xml_malloc(size_of::<XmlPointerList>()) as _;
        if ret.is_null() {
            xml_xpath_err_memory(None, Some("xmlPointerListCreate: allocating item\n"));
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
                            let _ = (*obj).user.take();
                            // if let Some(loc) = (*obj).user.take() {
                            //     if let Some(&loc) = loc.as_location_set() {
                            //         xml_xptr_free_location_set(loc);
                            //     }
                            // }
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
                    *obj = XmlXPathObject::default();
                    (*obj).nodesetval = Some(nodeset);
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
#[doc(alias = "xmlXPathErr", alias = "xmlXPatherror")]
pub unsafe fn xml_xpath_err(ctxt: Option<&mut XmlXPathParserContext>, mut error: i32) {
    unsafe {
        if !(0..=MAXERRNO).contains(&error) {
            error = MAXERRNO;
        }
        let Some(ctxt) = ctxt else {
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
                Some(XML_XPATH_ERROR_MESSAGES[error as usize]),
            );
            return;
        };
        // Only report the first error
        if ctxt.error != 0 {
            return;
        }
        ctxt.error = error;
        if ctxt.context.is_null() {
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
                Some(ctxt.base.to_string().into()),
                None,
                None,
                ctxt.cur as _,
                0,
                Some(XML_XPATH_ERROR_MESSAGES[error as usize]),
            );
            return;
        }

        // cleanup current last error
        (*ctxt.context).last_error.reset();

        (*ctxt.context).last_error.domain = XmlErrorDomain::XmlFromXPath;
        (*ctxt.context).last_error.code = XmlParserErrors::try_from(
            error + XmlParserErrors::XmlXPathExpressionOk as i32
                - XmlXPathError::XPathExpressionOK as i32,
        )
        .unwrap();
        (*ctxt.context).last_error.level = XmlErrorLevel::XmlErrError;
        (*ctxt.context).last_error.str1 = Some(ctxt.base.to_string().into());
        // (*(*ctxt).context).last_error.str1 = xml_strdup((*ctxt).base) as *mut c_char;
        (*ctxt.context).last_error.int1 = ctxt.cur as _;
        (*ctxt.context).last_error.node = (*ctxt.context).debug_node;
        if let Some(error) = (*ctxt.context).error {
            error(
                (*ctxt.context).user_data.clone(),
                &(*ctxt.context).last_error,
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
                (*ctxt.context).debug_node,
                XmlErrorDomain::XmlFromXPath,
                code,
                XmlErrorLevel::XmlErrError,
                None,
                0,
                Some(ctxt.base.to_string().into()),
                None,
                None,
                ctxt.cur as _,
                0,
                Some(XML_XPATH_ERROR_MESSAGES[error as usize]),
            );
        }
    }
}

/// Register a new variable value. If @value is NULL it unregisters the variable
///
/// Returns 0 in case of success, -1 in case of error
#[doc(alias = "xmlXPathRegisterVariable")]
pub unsafe fn xml_xpath_register_variable(
    ctxt: XmlXPathContextPtr,
    name: &str,
    value: XmlXPathObjectPtr,
) -> i32 {
    unsafe { xml_xpath_register_variable_ns(ctxt, name, None, value) }
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
    name: &str,
    ns_uri: Option<&str>,
    value: XmlXPathObjectPtr,
) -> i32 {
    unsafe {
        if ctxt.is_null() {
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
            return match var_hash.remove_entry2(name, ns_uri, |data, _| {
                xml_xpath_free_object_entry(data);
            }) {
                Ok(_) => 0,
                Err(_) => -1,
            };
        }

        match var_hash.update_entry2(name, ns_uri, value, |data, _| {
            xml_xpath_free_object_entry(data);
        }) {
            Ok(_) => 0,
            Err(_) => -1,
        }
    }
}

/// Search in the Variable array of the context for the given variable value.
///
/// Returns a copy of the value or NULL if not found
#[doc(alias = "xmlXPathVariableLookup")]
pub unsafe fn xml_xpath_variable_lookup(ctxt: XmlXPathContextPtr, name: &str) -> XmlXPathObjectPtr {
    unsafe {
        if ctxt.is_null() {
            return null_mut();
        }

        if let Some(var_lookup_func) = (*ctxt).var_lookup_func {
            return var_lookup_func((*ctxt).var_lookup_data, name, None);
        }
        xml_xpath_variable_lookup_ns(ctxt, name, None)
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
pub(super) unsafe fn xml_xpath_cache_wrap_node_set(
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
pub fn xml_xpath_err_memory(ctxt: Option<&mut XmlXPathContext>, extra: Option<&str>) {
    if let Some(ctxt) = ctxt {
        ctxt.last_error.reset();
        if let Some(extra) = extra {
            let buf = format!("Memory allocation failed : {extra}\n",);
            ctxt.last_error.message = Some(buf.into());
            // (*ctxt).last_error.message = xml_strdup(buf.as_ptr()) as *mut c_char;
        } else {
            ctxt.last_error.message = Some("Memory allocation failed\n".into());
            // xml_strdup(c"Memory allocation failed\n".as_ptr() as _) as *mut c_char;
        }
        ctxt.last_error.domain = XmlErrorDomain::XmlFromXPath;
        ctxt.last_error.code = XmlParserErrors::XmlErrNoMemory;
        if let Some(error) = ctxt.error {
            error(ctxt.user_data.clone(), &ctxt.last_error);
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
    val: &XmlXPathObject,
) -> XmlXPathObjectPtr {
    unsafe {
        if XP_HAS_CACHE!(ctxt) {
            match val.typ {
                XmlXPathObjectType::XPathNodeset => {
                    return xml_xpath_cache_wrap_node_set(
                        ctxt,
                        xml_xpath_node_set_merge(None, val.nodesetval.as_deref()),
                    );
                }
                XmlXPathObjectType::XPathString => {
                    return xml_xpath_cache_new_string(ctxt, val.stringval.as_deref());
                }
                XmlXPathObjectType::XPathBoolean => {
                    return xml_xpath_cache_new_boolean(ctxt, val.boolval);
                }
                XmlXPathObjectType::XPathNumber => {
                    return xml_xpath_cache_new_float(ctxt, val.floatval);
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
    name: &str,
    ns_uri: Option<&str>,
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

        let obj = var_hash
            .lookup2(name, ns_uri)
            .copied()
            .unwrap_or(null_mut());
        xml_xpath_cache_object_copy(ctxt, &*obj)
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

/// Handle a redefinition of attribute error
#[doc(alias = "xmlXPathPErrMemory")]
pub(super) unsafe fn xml_xpath_perr_memory(
    ctxt: Option<&mut XmlXPathParserContext>,
    extra: Option<&str>,
) {
    unsafe {
        if let Some(ctxt) = ctxt {
            ctxt.error = XmlXPathError::XPathMemoryError as i32;
            xml_xpath_err_memory(Some(&mut *ctxt.context), extra);
        } else {
            xml_xpath_err_memory(None, extra);
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
pub fn xml_xpath_node_set_dup_ns(
    node: Option<XmlGenericNodePtr>,
    mut ns: XmlNsPtr,
) -> Option<XmlGenericNodePtr> {
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
        xml_xpath_err_memory(None, Some("duplicating namespace\n"));
        return None;
    };
    cur.href = ns.href.clone();
    cur.prefix = ns.prefix.clone();
    // cur.next = node as *mut XmlNs;
    cur.node = node;
    Some(cur.into())
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

/// Filter a node set, keeping only nodes for which the predicate expression
/// matches. Afterwards, keep only nodes between minPos and maxPos in the
/// filtered result.
#[doc(alias = "xmlXPathNodeSetFilter")]
pub(super) unsafe fn xml_xpath_node_set_filter(
    ctxt: &mut XmlXPathParserContext,
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

        let xpctxt: XmlXPathContextPtr = ctxt.context;
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

            let mut comp = ctxt.comp.borrow_mut();
            let op = &raw mut comp.steps[filter_op_index as usize];
            drop(comp);
            let res = ctxt.evaluate_precompiled_operation_to_boolean(op, true);

            if ctxt.error != XmlXPathError::XPathExpressionOK as i32 {
                break;
            }
            if res < 0 {
                // Shouldn't happen
                xml_xpath_err(Some(ctxt), XmlXPathError::XPathExprError as i32);
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
    ctxt: &mut XmlXPathParserContext,
    op: XmlXPathStepOpPtr,
    set: &mut XmlNodeSet,
    min_pos: i32,
    max_pos: i32,
    has_ns_nodes: bool,
) {
    unsafe {
        if (*op).ch1 != -1 {
            // Process inner predicates first.
            if !matches!(
                ctxt.comp.borrow().steps[(*op).ch1 as usize].op,
                XmlXPathOp::XPathOpPredicate
            ) {
                generic_error!("xmlXPathCompOpEvalPredicate: Expected a predicate\n");
                XP_ERROR!(Some(ctxt), XmlXPathError::XPathInvalidOperand as i32);
            }
            if (*ctxt.context).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
                XP_ERROR!(
                    Some(ctxt),
                    XmlXPathError::XPathRecursionLimitExceeded as i32
                );
            }
            (*ctxt.context).depth += 1;
            let mut comp = ctxt.comp.borrow_mut();
            let op = &raw mut comp.steps[(*op).ch1 as usize];
            drop(comp);
            xml_xpath_comp_op_eval_predicate(
                ctxt,
                op,
                set,
                1,
                set.node_tab.len() as i32,
                has_ns_nodes,
            );
            (*ctxt.context).depth -= 1;
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
        let prefix = (*op).value4.as_ref().and_then(|val| val.as_str());
        let name = (*op).value5.as_ref().and_then(|val| val.as_str());
        let mut uri = None;
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

        if (*ctxt)
            .value()
            .is_none_or(|value| (*value).typ != (XmlXPathObjectType::XPathNodeset))
        {
            XP_ERROR0!(Some(&mut *ctxt), XmlXPathError::XPathInvalidType as i32)
        };
        // The popped object holding the context nodes
        let obj: XmlXPathObjectPtr = (*ctxt).value_pop();
        // Setup namespaces.
        if let Some(prefix) = prefix {
            uri = (*xpctxt).lookup_ns(prefix);
            if uri.is_none() {
                xml_xpath_release_object(xpctxt, obj);
                XP_ERROR0!(
                    Some(&mut *ctxt),
                    XmlXPathError::XPathUndefPrefixError as i32
                );
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
            let mut comp = (*ctxt).comp.borrow_mut();
            pred_op = &raw mut comp.steps[(*op).ch2 as usize];
            drop(comp);
            if (*ctxt).is_positional_predicate(&*pred_op, &mut max_pos) != 0 {
                if (*pred_op).ch1 != -1 {
                    // Use the next inner predicate operator.
                    let mut comp = (*ctxt).comp.borrow_mut();
                    pred_op = &raw mut comp.steps[(*pred_op).ch1 as usize];
                    drop(comp);
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
                            match cur.element_type() {
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
                        } else if cur.element_type() as isize == typ as isize {
                            if matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl) {
                                xp_test_hit_ns!(has_axis_range, pos, max_pos, has_ns_nodes, seq, xpctxt, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                            } else {
                                xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                            }
                        } else if matches!(typ, XmlXPathTypeVal::NodeTypeText)
                            && matches!(cur.element_type(), XmlElementType::XmlCDATASectionNode)
                        {
                            xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                        }
                    }
                    XmlXPathTestVal::NodeTestPI => {
                        if matches!(cur.element_type(), XmlElementType::XmlPINode)
                            && (name
                                .is_none_or(|name| name == XmlNodePtr::try_from(cur).unwrap().name))
                        {
                            xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                        }
                    }
                    XmlXPathTestVal::NodeTestAll => {
                        if matches!(axis, XmlXPathAxisVal::AxisAttribute) {
                            if matches!(cur.element_type(), XmlElementType::XmlAttributeNode)
                                && (prefix.is_none()
                                    || XmlAttrPtr::try_from(cur)
                                        .unwrap()
                                        .ns
                                        .as_deref()
                                        .and_then(|ns| ns.href.as_deref())
                                        .is_some_and(|href| uri.as_deref() == Some(href)))
                            {
                                xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                            }
                        } else if matches!(axis, XmlXPathAxisVal::AxisNamespace) {
                            if matches!(cur.element_type(), XmlElementType::XmlNamespaceDecl) {
                                xp_test_hit_ns!(has_axis_range, pos, max_pos, has_ns_nodes, seq, xpctxt, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                            }
                        } else if matches!(cur.element_type(), XmlElementType::XmlElementNode)
                            && (prefix.is_none()
                                || XmlNodePtr::try_from(cur)
                                    .unwrap()
                                    .ns
                                    .as_deref()
                                    .and_then(|ns| ns.href.as_deref())
                                    .is_some_and(|href| uri.as_deref() == Some(href)))
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
                                if name == Some(&node.name) {
                                    if prefix.is_none() {
                                        if node.ns.is_none() {
                                            xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                        }
                                    } else if node
                                        .ns
                                        .as_deref()
                                        .and_then(|ns| ns.href.as_deref())
                                        .is_some_and(|href| uri.as_deref() == Some(href))
                                    {
                                        xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    }
                                }
                            }
                            XmlElementType::XmlAttributeNode => {
                                let attr = XmlAttrPtr::try_from(cur).unwrap();

                                if name == Some(attr.name.as_ref()) {
                                    if prefix.is_none() {
                                        if attr.ns.is_none_or(|ns| ns.prefix().is_none()) {
                                            xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                        }
                                    } else if attr
                                        .ns
                                        .as_deref()
                                        .and_then(|ns| ns.href.as_deref())
                                        .is_some_and(|href| uri.as_deref() == Some(href))
                                    {
                                        xp_test_hit!(has_axis_range, pos, max_pos, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    }
                                }
                            }
                            XmlElementType::XmlNamespaceDecl => {
                                let ns = XmlNsPtr::try_from(cur).unwrap();
                                if ns
                                    .prefix
                                    .as_deref()
                                    .is_some_and(|prefix| name.is_some_and(|name| prefix == name))
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
                        &mut *ctxt,
                        pred_op,
                        seq.as_deref_mut().unwrap(),
                        max_pos,
                        max_pos,
                        has_ns_nodes,
                    );
                } else {
                    let max_pos = seq.as_deref().unwrap().node_tab.len() as i32;
                    xml_xpath_comp_op_eval_predicate(
                        &mut *ctxt,
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
        if (*obj).boolval && (*obj).user.is_some() {
            // QUESTION TODO: What does this do and why?
            // TODO: Do we have to do this also for the "error"
            // cleanup further down?
            let value = (*ctxt).value_mut().unwrap();
            (**value).boolval = true;
            (**value).user = (*obj).user.take();
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
    locset: &mut XmlLocationSet,
    filter_op_index: i32,
    min_pos: i32,
    max_pos: i32,
) {
    unsafe {
        if locset.loc_tab.is_empty() || filter_op_index == -1 {
            return;
        }

        let xpctxt: XmlXPathContextPtr = (*ctxt).context;
        let oldnode = (*xpctxt).node;
        let olddoc = (*xpctxt).doc;
        let oldcs: i32 = (*xpctxt).context_size;
        let oldpp: i32 = (*xpctxt).proximity_position;

        (*xpctxt).context_size = locset.loc_tab.len() as i32;

        let mut i = 0;
        let mut j = 0;
        let mut pos = 1;
        while i < locset.loc_tab.len() {
            let context_node = locset.loc_tab[i]
                .user
                .as_ref()
                .and_then(|user| user.as_node())
                .copied()
                .unwrap();

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

            let mut comp = (*ctxt).comp.borrow_mut();
            let op = &raw mut comp.steps[filter_op_index as usize];
            let res: i32 = (*ctxt).evaluate_precompiled_operation_to_boolean(op, true);

            if (*ctxt).error != XmlXPathError::XPathExpressionOK as i32 {
                break;
            }
            if res < 0 {
                // Shouldn't happen
                xml_xpath_err(Some(&mut *ctxt), XmlXPathError::XPathExprError as i32);
                break;
            }

            if res != 0 && (pos >= min_pos && pos <= max_pos) {
                if i != j {
                    locset.loc_tab[j] = locset.loc_tab[i].clone();
                    // locset.loc_tab[i] = null_mut();
                }

                j += 1;
            } else {
                /* Remove the entry from the initial location set. */
                // xml_xpath_free_object(locset.loc_tab[i]);
                // locset.loc_tab[i] = null_mut();
            }

            if res != 0 {
                if pos == max_pos {
                    // i += 1;
                    break;
                }

                pos += 1;
            }

            i += 1;
        }

        // Free remaining nodes.
        locset.loc_tab.truncate(j);
        // If too many elements were removed, shrink table to preserve memory.
        locset.loc_tab.shrink_to(XML_NODESET_DEFAULT);

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
pub fn xml_xpath_string_eval_number(s: Option<&str>) -> f64 {
    let Some(s) = s else {
        return 0.0;
    };
    let mut ok = false;
    let mut isneg = false;
    let mut exponent = 0;
    let mut cur = s.trim_matches(|c: char| xml_is_blank_char(c as u32));
    if let Some(rem) = cur.strip_prefix('-') {
        isneg = true;
        cur = rem;
    }
    if !cur.starts_with(|c: char| c == '.' || c.is_ascii_digit()) {
        return XML_XPATH_NAN;
    }

    let mut ret = 0.0;
    while let Some(b) = cur.as_bytes().first().copied().filter(u8::is_ascii_digit) {
        ret = ret * 10. + (b - b'0') as f64;
        cur = &cur[1..];
        ok = true;
    }

    if let Some(rem) = cur.strip_prefix('.') {
        if !ok && !cur.starts_with(|c: char| c.is_ascii_digit()) {
            return XML_XPATH_NAN;
        }

        cur = rem.trim_start_matches('0');
        let mut frac = rem.len() - cur.len();
        let max = frac + MAX_FRAC;
        let mut fraction = 0.0;
        while let Some(b) = cur
            .as_bytes()
            .first()
            .filter(|b| b.is_ascii_digit() && frac < max)
        {
            fraction = fraction * 10. + (b - b'0') as f64;
            frac += 1;
            cur = &cur[1..];
        }
        fraction /= 10.0f64.powi(frac as i32);
        ret += fraction;
        cur = cur.trim_start_matches(|c: char| c.is_ascii_digit());
    }

    let mut is_exponent_negative = false;
    if let Some(rem) = cur.strip_prefix(['e', 'E']) {
        if let Some(rem) = rem.strip_prefix('-') {
            is_exponent_negative = true;
            cur = rem;
        } else {
            cur = rem.strip_prefix('+').unwrap_or(rem);
        }
        while let Some(b) = cur
            .as_bytes()
            .first()
            .filter(|b| b.is_ascii_digit() && exponent < 1000000)
        {
            exponent = exponent * 10 + (b - b'0') as i32;
            cur = &cur[1..];
        }
        cur = cur.trim_start_matches(|c: char| c.is_ascii_digit());
    }
    if !cur.is_empty() {
        return XML_XPATH_NAN;
    }
    if isneg {
        ret = -ret;
    }
    if is_exponent_negative {
        exponent = -exponent;
    }
    ret * 10.0f64.powi(exponent)
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
    res: &XmlXPathObject,
) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return 0;
        }
        match res.typ {
            XmlXPathObjectType::XPathBoolean => {
                return res.boolval as i32;
            }
            XmlXPathObjectType::XPathNumber => {
                return (res.floatval == (*(*ctxt).context).proximity_position as f64) as i32;
            }
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
                if let Some(nodeset) = res.nodesetval.as_deref() {
                    return !nodeset.is_empty() as i32;
                } else {
                    return 0;
                }
            }
            XmlXPathObjectType::XPathString => {
                return res.stringval.as_deref().is_some_and(|s| !s.is_empty()) as i32;
            }
            #[cfg(feature = "libxml_xptr_locs")]
            XmlXPathObjectType::XPathLocationset => {
                let Some(loc) = res.user.as_ref().and_then(|user| user.as_location_set()) else {
                    return 0;
                };

                return (!loc.loc_tab.is_empty()) as i32;
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
pub(super) unsafe fn xml_xpath_cache_wrap_string(
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

/// Function computing the beginning of the string value of the node,
/// used to speed up comparisons
///
/// Returns an int usable as a hash
#[doc(alias = "xmlXPathNodeValHash")]
fn xml_xpath_node_val_hash(node: Option<XmlGenericNodePtr>) -> u32 {
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
            let Some(string) = node.content.as_deref() else {
                return 0;
            };
            if string.is_empty() {
                return 0;
            }
            let s0 = string.as_bytes()[0];
            let s1 = *string.as_bytes().get(1).unwrap_or(&0);
            return s0 as u32 + ((s1 as u32) << 8);
        }
        XmlElementType::XmlNamespaceDecl => {
            let ns = XmlNsPtr::try_from(node).unwrap();
            let Some(string) = ns.href.as_deref() else {
                return 0;
            };
            if string.is_empty() {
                return 0;
            }
            let s0 = string.as_bytes()[0];
            let s1 = *string.as_bytes().get(1).unwrap_or(&0);
            return s0 as u32 + ((s1 as u32) << 8);
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
                node.content.clone()
            }
            _ => None,
        };
        if let Some(string) = string.filter(|s| !s.is_empty()) {
            let bytes = string.as_bytes();
            if len == 1 {
                return ret + ((bytes[0] as u32) << 8);
            }
            if bytes.len() == 1 {
                len = 1;
                ret = bytes[0] as u32;
            } else {
                return bytes[0] as u32 + ((bytes[1] as u32) << 8);
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
            xml_xpath_err_memory(None, Some("comparing nodesets\n"));
            return 0;
        }
        let mut values2 = vec![None; ns2.node_tab.len()];
        let hashs2: *mut u32 = xml_malloc(ns2.node_tab.len() * size_of::<u32>()) as *mut u32;
        if hashs2.is_null() {
            // TODO: Propagate memory error.
            xml_xpath_err_memory(None, Some("comparing nodesets\n"));
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
                xml_xpath_number_function(&mut *ctxt, 1);
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
                            xml_xpath_number_function(&mut *ctxt, 1);
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
                        xml_xpath_number_function(&mut *ctxt, 1);
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
            XP_ERROR0!(Some(&mut *ctxt), XmlXPathError::XPathInvalidOperand as i32);
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
            XP_ERROR0!(Some(&mut *ctxt), XmlXPathError::XPathInvalidOperand as i32);
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
            xml_xpath_err_memory(None, Some("comparing nodesets\n"));
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
                xml_xpath_number_function(&mut *ctxt, 1);
                (*ctxt).value_push(xml_xpath_cache_object_copy((*ctxt).context, &*f));
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
                (*ctxt).value_push(xml_xpath_cache_object_copy((*ctxt).context, &*s));
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
                xml_xpath_boolean_function(&mut *ctxt, 1);
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
                XP_ERROR0!(Some(&mut *ctxt), XmlXPathError::XPathInvalidType as i32);
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
            XP_ERROR0!(Some(&mut *ctxt), XmlXPathError::XPathInvalidOperand as i32);
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
            xml_xpath_number_function(&mut *ctxt, 1);
            arg1 = (*ctxt).value_pop();
        }
        if !matches!((*arg2).typ, XmlXPathObjectType::XPathNumber) {
            (*ctxt).value_push(arg2);
            xml_xpath_number_function(&mut *ctxt, 1);
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
        cast_to_number(&mut *ctxt);
        if (*ctxt)
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
        {
            crate::XP_ERROR!(
                Some(&mut *ctxt),
                crate::xpath::XmlXPathError::XPathInvalidType as i32
            )
        };
        let val = &mut (**(*ctxt).value_mut().unwrap()).floatval;
        *val = -*val;
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
            XP_ERROR!(Some(&mut *ctxt), XmlXPathError::XPathInvalidOperand as i32);
        }
        let val: f64 = xml_xpath_cast_to_number(arg);
        xml_xpath_release_object((*ctxt).context, arg);
        cast_to_number(&mut *ctxt);
        if (*ctxt)
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
        {
            crate::XP_ERROR!(
                Some(&mut *ctxt),
                crate::xpath::XmlXPathError::XPathInvalidType as i32
            )
        };
        (**(*ctxt).value_mut().unwrap()).floatval += val;
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
            XP_ERROR!(Some(&mut *ctxt), XmlXPathError::XPathInvalidOperand as i32);
        }
        let val: f64 = xml_xpath_cast_to_number(arg);
        xml_xpath_release_object((*ctxt).context, arg);
        cast_to_number(&mut *ctxt);
        if (*ctxt)
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
        {
            crate::XP_ERROR!(
                Some(&mut *ctxt),
                crate::xpath::XmlXPathError::XPathInvalidType as i32
            )
        };
        (**(*ctxt).value_mut().unwrap()).floatval -= val;
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
            XP_ERROR!(Some(&mut *ctxt), XmlXPathError::XPathInvalidOperand as i32);
        }
        let val: f64 = xml_xpath_cast_to_number(arg);
        xml_xpath_release_object((*ctxt).context, arg);
        cast_to_number(&mut *ctxt);
        if (*ctxt)
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
        {
            crate::XP_ERROR!(
                Some(&mut *ctxt),
                crate::xpath::XmlXPathError::XPathInvalidType as i32
            )
        };
        (**(*ctxt).value_mut().unwrap()).floatval *= val;
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
            XP_ERROR!(Some(&mut *ctxt), XmlXPathError::XPathInvalidOperand as i32);
        }
        let val: f64 = xml_xpath_cast_to_number(arg);
        xml_xpath_release_object((*ctxt).context, arg);
        cast_to_number(&mut *ctxt);
        if (*ctxt)
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
        {
            crate::XP_ERROR!(
                Some(&mut *ctxt),
                crate::xpath::XmlXPathError::XPathInvalidType as i32
            )
        };
        (**(*ctxt).value_mut().unwrap()).floatval /= val;
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
            XP_ERROR!(Some(&mut *ctxt), XmlXPathError::XPathInvalidOperand as i32);
        }
        let arg2: f64 = xml_xpath_cast_to_number(arg);
        xml_xpath_release_object((*ctxt).context, arg);
        cast_to_number(&mut *ctxt);
        if (*ctxt)
            .value()
            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
        {
            crate::XP_ERROR!(
                Some(&mut *ctxt),
                crate::xpath::XmlXPathError::XPathInvalidType as i32
            )
        };
        let arg1 = &mut (**(*ctxt).value_mut().unwrap()).floatval;
        if arg2 == 0.0 {
            *arg1 = XML_XPATH_NAN;
        } else {
            *arg1 %= arg2;
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
        if let Some(next) = cur.next() {
            return Some(next);
        }
        loop {
            cur = cur.parent()?;
            if Some(cur) == (*(*ctxt).context).doc.map(|doc| doc.into()) {
                break None;
            }
            if let Some(next) = cur.next() {
                break Some(next);
            }
        }
    }
}

thread_local! {
    static XML_XPATH_XMLNAMESPACE_STRUCT: XmlNs = XmlNs {
        next: None,
        typ: XmlElementType::XmlNamespaceDecl,
        href: Some(XML_XML_NAMESPACE.into()),
        prefix: Some("xml".into()),
        _private: null_mut(),
        context: None,
        node: None,
    };
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
fn xml_xpath_is_ancestor(
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
        match cur.element_type() {
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

/// Selects elements by their unique ID.
///
/// Returns a node-set of selected elements.
#[doc(alias = "xmlXPathGetElementsByIds")]
pub(super) unsafe fn xml_xpath_get_elements_by_ids(
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
                if let Some(attr) = xml_get_id(
                    doc,
                    CStr::from_ptr(id as *const i8).to_string_lossy().as_ref(),
                ) {
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
pub(super) unsafe fn xml_xpath_cache_convert_string(
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

/// This is the cached version of xmlXPathConvertNumber().
/// Converts an existing object to its number() equivalent
///
/// Returns a created or reused object, the old one is freed (or the operation
///         is done directly on @val)
#[doc(alias = "xmlXPathCacheConvertNumber")]
pub(super) unsafe fn xml_xpath_cache_convert_number(
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

/// This is the cached version of xmlXPathConvertBoolean().
/// Converts an existing object to its boolean() equivalent
///
/// Returns a created or reused object, the old one is freed (or the operation
/// is done directly on @val)
#[doc(alias = "xmlXPathCacheConvertBoolean")]
pub(super) unsafe fn xml_xpath_cache_convert_boolean(
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
            xml_xpath_cache_new_boolean(ctxt, xml_xpath_cast_to_boolean(&*val));
        xml_xpath_release_object(ctxt, val);
        ret
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
            ns.free();
        }
    }
}

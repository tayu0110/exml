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
    ffi::{c_char, CStr, CString},
    io::Write,
    iter::repeat,
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut, NonNull},
};

use libc::{memcpy, memset, INT_MAX, INT_MIN};

#[cfg(feature = "libxml_xptr_locs")]
use crate::libxml::xpointer::{
    xml_xptr_free_location_set, xml_xptr_location_set_add, xml_xptr_location_set_create,
    xml_xptr_new_range, xml_xptr_new_range_node_object, xml_xptr_wrap_location_set,
    XmlLocationSetPtr,
};
use crate::{
    buf::libxml_api::{xml_buf_add, xml_buf_create, xml_buf_free},
    error::{XmlErrorDomain, XmlErrorLevel, XmlParserErrors, __xml_raise_error},
    generic_error,
    hash::XmlHashTableRef,
    libxml::{
        chvalid::{xml_is_blank_char, xml_is_char},
        dict::{xml_dict_lookup, xml_dict_reference, XmlDictPtr},
        globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
        parser_internals::{xml_copy_char, XML_MAX_NAMELEN, XML_MAX_NAME_LENGTH},
        pattern::{
            xml_free_pattern, xml_free_pattern_list, xml_free_stream_ctxt, xml_pattern_from_root,
            xml_pattern_get_stream_ctxt, xml_pattern_max_depth, xml_pattern_min_depth,
            xml_pattern_streamable, xml_patterncompile, xml_stream_pop, xml_stream_push,
            xml_stream_push_node, xml_stream_wants_any_node, XmlPatternFlags, XmlPatternPtr,
            XmlStreamCtxtPtr,
        },
        valid::xml_get_id,
        xmlstring::{
            xml_str_equal, xml_strchr, xml_strdup, xml_strlen, xml_strndup, xml_strstr,
            xml_utf8_strlen, XmlChar,
        },
        xpath::{
            xml_xpath_cast_boolean_to_string, xml_xpath_cast_node_set_to_string,
            xml_xpath_cast_node_to_number, xml_xpath_cast_node_to_string,
            xml_xpath_cast_number_to_boolean, xml_xpath_cast_number_to_string,
            xml_xpath_cast_to_boolean, xml_xpath_cast_to_number, xml_xpath_cast_to_string,
            xml_xpath_free_comp_expr, xml_xpath_free_node_set, xml_xpath_free_object,
            xml_xpath_free_value_tree, xml_xpath_is_inf, xml_xpath_is_nan,
            xml_xpath_node_set_create, xml_xpath_object_copy, XmlNodeSetPtr, XmlXPathCompExpr,
            XmlXPathCompExprPtr, XmlXPathContextPtr, XmlXPathError, XmlXPathFuncLookupFunc,
            XmlXPathFunction, XmlXPathObject, XmlXPathObjectPtr, XmlXPathObjectType, XmlXPathOp,
            XmlXPathParserContext, XmlXPathParserContextPtr, XmlXPathStepOp, XmlXPathStepOpPtr,
            XmlXPathVariableLookupFunc, XML_XPATH_CHECKNS, XML_XPATH_NAN, XML_XPATH_NOVAR,
            XPATH_MAX_NODESET_LENGTH, XPATH_MAX_STACK_DEPTH, XPATH_MAX_STEPS,
        },
    },
    tree::{
        xml_buf_content, xml_build_qname, NodeCommon, NodePtr, XmlAttrPtr, XmlBufPtr, XmlDocPtr,
        XmlElementType, XmlNodePtr, XmlNs, XmlNsPtr, XML_XML_NAMESPACE,
    },
    xml_xpath_node_set_get_length, xml_xpath_node_set_is_empty, xml_xpath_node_set_item,
};

use super::{
    chvalid::{xml_is_combining, xml_is_digit, xml_is_extender},
    hash::XmlHashTable,
    parser_internals::xml_is_letter,
};

// Many of these macros may later turn into functions.
// They shouldn't be used in #ifdef's preprocessor instructions.

/// Raises an error.
#[doc(alias = "xmlXPathSetError")]
macro_rules! xml_xpath_set_error {
    ($ctxt:expr, $err:expr) => {{
        let file = std::ffi::CString::new(file!()).unwrap();
        xml_xpatherror($ctxt, file.as_ptr() as _, line!() as _, $err);
        if !$ctxt.is_null() {
            (*$ctxt).error = $err;
        }
    }};
}

// /**
//  * xmlXPathSetArityError:
//  * @ctxt:  an XPath parser context
//  *
//  * Raises an XPATH_INVALID_ARITY error.
//  */
// macro_rules! xmlXPathSetArityError {
//     (ctxt) => {
//         xmlXPathSetError((ctxt), XPATH_INVALID_ARITY)
//     };
// }

/// Raises an XPATH_INVALID_TYPE error.
#[doc(alias = "xmlXPathSetTypeError")]
macro_rules! xml_xpath_set_type_error {
    ($ctxt:expr) => {
        xml_xpath_set_error!($ctxt, XmlXPathError::XpathInvalidType as i32)
    };
}

// /**
//  * xmlXPathGetError:
//  * @ctxt:  an XPath parser context
//  *
//  * Get the error code of an XPath context.
//  *
//  * Returns the context error.
//  */
// macro_rules! xmlXPathGetError {
// 	(ctxt) => {
// 		((ctxt)->error)
// 	}
// }

// /**
//  * xmlXPathCheckError:
//  * @ctxt:  an XPath parser context
//  *
//  * Check if an XPath error was raised.
//  *
//  * Returns true if an error has been raised, false otherwise.
//  */
// macro_rules! xmlXPathCheckError {
// 	(ctxt) => {
// 		((ctxt)->error != XPATH_EXPRESSION_OK)
// 	}
// }

// /**
//  * xmlXPathGetDocument:
//  * @ctxt:  an XPath parser context
//  *
//  * Get the document of an XPath context.
//  *
//  * Returns the context document.
//  */
// macro_rules! xmlXPathGetDocument {
// 	(ctxt) => {
// 		((ctxt)->context->doc)
// 	}
// }

// /**
//  * xmlXPathGetContextNode:
//  * @ctxt: an XPath parser context
//  *
//  * Get the context node of an XPath context.
//  *
//  * Returns the context node.
//  */
// macro_rules! xmlXPathGetContextNode {
// 	(ctxt) => {
// 		((ctxt)->context->node)
// 	}
// }

// /**
//  * xmlXPathReturnBoolean:
//  * @ctxt:  an XPath parser context
//  * @val:  a boolean
//  *
//  * Pushes the boolean @val on the context stack.
//  */
// macro_rules! xmlXPathReturnBoolean {
//     (ctxt, val) => {
//         valuePush((ctxt), xmlXPathNewBoolean(val))
//     };
// }

// /**
//  * xmlXPathReturnTrue:
//  * @ctxt:  an XPath parser context
//  *
//  * Pushes true on the context stack.
//  */
// macro_rules! xmlXPathReturnTrue {
//     (ctxt) => {
//         xmlXPathReturnBoolean((ctxt), 1)
//     };
// }

// /**
//  * xmlXPathReturnFalse:
//  * @ctxt:  an XPath parser context
//  *
//  * Pushes false on the context stack.
//  */
// macro_rules! xmlXPathReturnFalse {
//     (ctxt) => {
//         xmlXPathReturnBoolean((ctxt), 0)
//     };
// }

// /**
//  * xmlXPathReturnNumber:
//  * @ctxt:  an XPath parser context
//  * @val:  a double
//  *
//  * Pushes the double @val on the context stack.
//  */
// macro_rules! xmlXPathReturnNumber {
//     (ctxt, val) => {
//         valuePush((ctxt), xmlXPathNewFloat(val))
//     };
// }

// /**
//  * xmlXPathReturnString:
//  * @ctxt:  an XPath parser context
//  * @str:  a string
//  *
//  * Pushes the string @str on the context stack.
//  */
// macro_rules! xmlXPathReturnString {
//     (ctxt, str) => {
//         valuePush((ctxt), xmlXPathWrapString(str))
//     };
// }

// /**
//  * xmlXPathReturnEmptyString:
//  * @ctxt:  an XPath parser context
//  *
//  * Pushes an empty string on the stack.
//  */
// macro_rules! xmlXPathReturnEmptyString {
//     (ctxt) => {
//         valuePush((ctxt), xmlXPathNewCString(""))
//     };
// }

// /**
//  * xmlXPathReturnNodeSet:
//  * @ctxt:  an XPath parser context
//  * @ns:  a node-set
//  *
//  * Pushes the node-set @ns on the context stack.
//  */
// macro_rules! xmlXPathReturnNodeSet {
//     (ctxt, ns) => {
//         valuePush((ctxt), xmlXPathWrapNodeSet(ns))
//     };
// }

// /**
//  * xmlXPathReturnEmptyNodeSet:
//  * @ctxt:  an XPath parser context
//  *
//  * Pushes an empty node-set on the context stack.
//  */
// macro_rules! xmlXPathReturnEmptyNodeSet {
//     (ctxt) => {
//         valuePush((ctxt), xmlXPathNewNodeSet(NULL))
//     };
// }

// /**
//  * xmlXPathReturnExternal:
//  * @ctxt:  an XPath parser context
//  * @val:  user data
//  *
//  * Pushes user data on the context stack.
//  */
// macro_rules! xmlXPathReturnExternal {
//     (ctxt, val) => {
//         valuePush((ctxt), xmlXPathWrapExternal(val))
//     };
// }

/// Check if the current value on the XPath stack is a node set or an XSLT value tree.
///
/// Returns true if the current object on the stack is a node-set.
#[doc(alias = "xmlXPathStackIsNodeSet")]
macro_rules! xml_xpath_stack_is_node_set {
    ($ctxt:expr) => {
        !(*$ctxt).value.is_null()
            && ((*(*$ctxt).value).typ == XmlXPathObjectType::XPathNodeset
                || (*(*$ctxt).value).typ == XmlXPathObjectType::XPathXSLTTree)
    };
}

// /**
//  * xmlXPathStackIsExternal:
//  * @ctxt: an XPath parser context
//  *
//  * Checks if the current value on the XPath stack is an external
//  * object.
//  *
//  * Returns true if the current object on the stack is an external
//  * object.
//  */
// macro_rules! xmlXPathStackIsExternal {
// 	(ctxt) => {
// 		(((*$ctxt).value != NULL) && ((*(*$ctxt).value).type == XPATH_USERS))
// 	}
// }

// /**
//  * xmlXPathEmptyNodeSet:
//  * @ns:  a node-set
//  *
//  * Empties a node-set.
//  */
// macro_rules! xmlXPathEmptyNodeSet {
// 	(ns) => {
// 		{ while ((ns)->nodeNr > 0) (ns)->nodeTab[--(ns)->nodeNr] = NULL; }
// 	}
// }

/// Macro to return from the function if an XPath error was detected.
#[doc(hidden)]
#[macro_export]
macro_rules! CHECK_ERROR {
    ($ctxt:expr) => {
        if (*$ctxt).error != $crate::libxml::xpath::XmlXPathError::XpathExpressionOk as i32 {
            return;
        }
    };
}

/// Macro to return 0 from the function if an XPath error was detected.
macro_rules! CHECK_ERROR0 {
    ($ctxt:expr) => {
        if (*$ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
            return 0;
        }
    };
}

/// Macro to raise an XPath error and return.
#[doc(hidden)]
#[macro_export]
macro_rules! XP_ERROR {
    ($ctxt:expr, $x:expr) => {{
        $crate::libxml::xpath_internals::xml_xpath_err($ctxt, $x);
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
            $crate::XP_ERROR!(
                $ctxt,
                $crate::libxml::xpath::XmlXPathError::XpathInvalidType as i32
            )
        }
    };
}

/// Macro to check that the value on top of the XPath stack is of a given type.  
/// Return(0) in case of failure
macro_rules! CHECK_TYPE0 {
    ($ctxt:expr, $typeval:expr) => {
        if (*$ctxt).value.is_null() || (*(*$ctxt).value).typ != $typeval {
            XP_ERROR0!($ctxt, XmlXPathError::XpathInvalidType as i32)
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
                $crate::libxml::xpath::XmlXPathError::XpathInvalidArity as i32
            );
        }
        if (*$ctxt).value_nr < (*$ctxt).value_frame + $x {
            $crate::XP_ERROR!(
                $ctxt,
                $crate::libxml::xpath::XmlXPathError::XpathStackError as i32
            );
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

/// Macro to raise an XPath error and return NULL.
macro_rules! XP_ERRORNULL {
    ($ctxt:expr, $x:expr) => {{
        xml_xpath_err($ctxt, $x);
        return null_mut();
    }};
}

// macro_rules! XP_CACHE_ADD {
// 	($sl:expr, $obj:expr) => {
// 		if $sl.is_null() {
// 			$sl = xmlPointerListCreate(10);
// 			if $sl.is_null() {
// 				goto free_obj;
// 			}
// 		}
// 		if xmlPointerListAddSize($sl, $obj, 0) == -1 {
// 			goto free_obj;
// 		}
// 	}
// }

// macro_rules! XP_CACHE_WANTS {
//     ($sl:expr, n) => {
//         $sl.is_null() || (*$sl).number < n
//     };
// }

pub type XmlPointerListPtr = *mut XmlPointerList;
/// Pointer-list for various purposes.
#[doc(alias = "xsltPointerList")]
#[repr(C)]
pub struct XmlPointerList {
    pub(crate) items: *mut *mut c_void,
    pub(crate) number: i32,
    pub(crate) size: i32,
}

pub type XmlXpathContextCachePtr = *mut XmlXpathContextCache;
#[repr(C)]
pub struct XmlXpathContextCache {
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
unsafe extern "C" fn xml_pointer_list_add_size(
    list: XmlPointerListPtr,
    item: *mut c_void,
    mut initial_size: i32,
) -> i32 {
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

/// Creates an xsltPointerList structure.
///
/// Returns a xsltPointerList structure or NULL in case of an error.
#[doc(alias = "xsltPointerListCreate")]
unsafe extern "C" fn xml_pointer_list_create(initial_size: i32) -> XmlPointerListPtr {
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
                /*
                 * Cache is full; free the object.
                 */
                if !(*$obj).nodesetval.is_null() {
                    xml_xpath_free_node_set((*$obj).nodesetval);
                }
                xml_free($obj as _);
                return;
            }
        }
        if xml_pointer_list_add_size($sl, $obj as _, 0) == -1 {
            /*
             * Cache is full; free the object.
             */
            if !(*$obj).nodesetval.is_null() {
                xml_xpath_free_node_set((*$obj).nodesetval);
            }
            xml_free($obj as _);
            return;
        }
    };
}

/// Depending on the state of the cache this frees the given
/// XPath object or stores it in the cache.
#[doc(alias = "xmlXPathReleaseObject")]
pub(crate) unsafe extern "C" fn xml_xpath_release_object(
    ctxt: XmlXPathContextPtr,
    obj: XmlXPathObjectPtr,
) {
    if obj.is_null() {
        return;
    }
    if ctxt.is_null() || (*ctxt).cache.is_null() {
        xml_xpath_free_object(obj);
    } else {
        let cache: XmlXpathContextCachePtr = (*ctxt).cache as XmlXpathContextCachePtr;

        'free_obj: {
            'obj_cached: {
                match (*obj).typ {
                    XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
                        if !(*obj).nodesetval.is_null() {
                            if (*obj).boolval {
                                // It looks like the @boolval is used for
                                // evaluation if this an XSLT Result Tree Fragment.
                                // TODO: Check if this assumption is correct.
                                (*obj).typ = XmlXPathObjectType::XPathXSLTTree; /* just for debugging */
                                xml_xpath_free_value_tree((*obj).nodesetval);
                                (*obj).nodesetval = null_mut();
                            } else if (*(*obj).nodesetval)
                                .node_tab
                                .as_ref()
                                .map_or(0, |t| t.len())
                                <= 40
                                && XP_CACHE_WANTS!((*cache).nodeset_objs, (*cache).max_nodeset)
                            {
                                XP_CACHE_ADD!((*cache).nodeset_objs, obj);
                                break 'obj_cached;
                            } else {
                                xml_xpath_free_node_set((*obj).nodesetval);
                                (*obj).nodesetval = null_mut();
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

            if !(*obj).nodesetval.is_null() {
                let tmpset: XmlNodeSetPtr = (*obj).nodesetval;

                // TODO: Due to those nasty ns-nodes, we need to traverse
                //  the list and free the ns-nodes.
                // URGENT TODO: Check if it's actually slowing things down.
                //  Maybe we shouldn't try to preserve the list.
                if let Some(table) = (*tmpset).node_tab.as_ref().filter(|t| t.len() > 1) {
                    for &node in table {
                        if !node.is_null()
                            && matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
                        {
                            xml_xpath_node_set_free_ns(node as XmlNsPtr);
                        }
                    }
                } else if let Some(table) = (*tmpset).node_tab.as_ref().filter(|t| {
                    t.len() == 1
                        && (!t[0].is_null()
                            && matches!((*t[0]).element_type(), XmlElementType::XmlNamespaceDecl))
                }) {
                    xml_xpath_node_set_free_ns(table[0] as XmlNsPtr);
                }
                if let Some(table) = (*tmpset).node_tab.as_mut() {
                    table.clear();
                }
                std::ptr::write(&mut *obj, XmlXPathObject::default());
                (*obj).nodesetval = tmpset;
            } else {
                std::ptr::write(&mut *obj, XmlXPathObject::default());
            }

            return;
        }

        // free_obj:
        // Cache is full; free the object.
        if !(*obj).nodesetval.is_null() {
            xml_xpath_free_node_set((*obj).nodesetval);
        }
        xml_free(obj as _);
    }
}

/// Pops a boolean from the stack, handling conversion if needed.
/// Check error with #xmlXPathCheckError.
///
/// Returns the boolean
#[doc(alias = "xmlXPathPopBoolean")]
pub unsafe extern "C" fn xml_xpath_pop_boolean(ctxt: XmlXPathParserContextPtr) -> i32 {
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    if obj.is_null() {
        xml_xpath_set_error!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
        return 0;
    }
    let ret = if (*obj).typ != XmlXPathObjectType::XPathBoolean {
        xml_xpath_cast_to_boolean(obj)
    } else {
        (*obj).boolval
    };
    xml_xpath_release_object((*ctxt).context, obj);
    ret as i32
}

/// Pops a number from the stack, handling conversion if needed.
/// Check error with #xmlXPathCheckError.
///
/// Returns the number
#[doc(alias = "xmlXPathPopNumber")]
pub unsafe extern "C" fn xml_xpath_pop_number(ctxt: XmlXPathParserContextPtr) -> f64 {
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    if obj.is_null() {
        xml_xpath_set_error!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
        return 0.0;
    }
    let ret = if (*obj).typ != XmlXPathObjectType::XPathNumber {
        xml_xpath_cast_to_number(obj)
    } else {
        (*obj).floatval
    };
    xml_xpath_release_object((*ctxt).context, obj);
    ret
}

/// Pops a string from the stack, handling conversion if needed.
/// Check error with #xmlXPathCheckError.
///
/// Returns the string
#[doc(alias = "xmlXPathPopString")]
pub unsafe extern "C" fn xml_xpath_pop_string(ctxt: XmlXPathParserContextPtr) -> *mut XmlChar {
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    if obj.is_null() {
        xml_xpath_set_error!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
        return null_mut();
    }
    let ret: *mut XmlChar = xml_xpath_cast_to_string(obj); /* this does required strdup */
    /* TODO: needs refactoring somewhere else */
    // if (*obj).stringval == ret {
    //     (*obj).stringval = null_mut();
    // }
    xml_xpath_release_object((*ctxt).context, obj);
    ret
}

/// Pops a node-set from the stack, handling conversion if needed.
/// Check error with #xmlXPathCheckError.
///
/// Returns the node-set
#[doc(alias = "xmlXPathPopNodeSet")]
pub unsafe extern "C" fn xml_xpath_pop_node_set(ctxt: XmlXPathParserContextPtr) -> XmlNodeSetPtr {
    if ctxt.is_null() {
        return null_mut();
    }
    if (*ctxt).value.is_null() {
        xml_xpath_set_error!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
        return null_mut();
    }
    if !xml_xpath_stack_is_node_set!(ctxt) {
        xml_xpath_set_type_error!(ctxt);
        return null_mut();
    }
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    let ret: XmlNodeSetPtr = (*obj).nodesetval;
    // #if 0
    //     /* to fix memory leak of not clearing (*obj).user */
    //     if ((*obj).boolval && !(*obj).user.is_null())
    //         xmlFreeNodeList((xmlNodePtr) (*obj).user);
    // #endif
    (*obj).nodesetval = null_mut();
    xml_xpath_release_object((*ctxt).context, obj);
    ret
}

/// Pops an external object from the stack, handling conversion if needed.
/// Check error with #xmlXPathCheckError.
///
/// Returns the object
#[doc(alias = "xmlXPathPopExternal")]
pub unsafe extern "C" fn xml_xpath_pop_external(ctxt: XmlXPathParserContextPtr) -> *mut c_void {
    if ctxt.is_null() || (*ctxt).value.is_null() {
        xml_xpath_set_error!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
        return null_mut();
    }
    if (*(*ctxt).value).typ != XmlXPathObjectType::XPathUsers {
        xml_xpath_set_type_error!(ctxt);
        return null_mut();
    }
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    let ret: *mut c_void = (*obj).user;
    (*obj).user = null_mut();
    xml_xpath_release_object((*ctxt).context, obj);
    ret
}

/// Register an external mechanism to do variable lookup
#[doc(alias = "xmlXPathRegisterVariableLookup")]
pub unsafe fn xml_xpath_register_variable_lookup(
    ctxt: XmlXPathContextPtr,
    f: XmlXPathVariableLookupFunc,
    data: *mut c_void,
) {
    if ctxt.is_null() {
        return;
    }
    (*ctxt).var_lookup_func = Some(f);
    (*ctxt).var_lookup_data = data;
}

/// Registers an external mechanism to do function lookup.
#[doc(alias = "xmlXPathRegisterFuncLookup")]
pub unsafe fn xml_xpath_register_func_lookup(
    ctxt: XmlXPathContextPtr,
    f: XmlXPathFuncLookupFunc,
    func_ctxt: *mut c_void,
) {
    if ctxt.is_null() {
        return;
    }
    (*ctxt).func_lookup_func = Some(f);
    (*ctxt).func_lookup_data = func_ctxt;
}

/// Formats an error message.
#[doc(alias = "xmlXPatherror")]
pub unsafe extern "C" fn xml_xpatherror(
    ctxt: XmlXPathParserContextPtr,
    _file: *const c_char,
    _line: i32,
    no: i32,
) {
    xml_xpath_err(ctxt, no);
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
pub unsafe extern "C" fn xml_xpath_err(ctxt: XmlXPathParserContextPtr, mut error: i32) {
    if !(0..=MAXERRNO).contains(&error) {
        error = MAXERRNO;
    }
    if ctxt.is_null() {
        let code = error + XmlParserErrors::XmlXPathExpressionOk as i32
            - XmlXPathError::XpathExpressionOk as i32;
        let code = XmlParserErrors::try_from(code).unwrap();
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            null_mut(),
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
    /* Only report the first error */
    if (*ctxt).error != 0 {
        return;
    }
    (*ctxt).error = error;
    if (*ctxt).context.is_null() {
        let code = error + XmlParserErrors::XmlXPathExpressionOk as i32
            - XmlXPathError::XpathExpressionOk as i32;
        let code = XmlParserErrors::try_from(code).unwrap();
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromXPath,
            code,
            XmlErrorLevel::XmlErrError,
            None,
            0,
            (!(*ctxt).base.is_null()).then(|| CStr::from_ptr((*ctxt).base as *const i8)
                .to_string_lossy()
                .into_owned()
                .into()),
            None,
            None,
            (*ctxt).cur.offset_from((*ctxt).base) as _,
            0,
            XML_XPATH_ERROR_MESSAGES[error as usize],
        );
        return;
    }

    /* cleanup current last error */
    (*(*ctxt).context).last_error.reset();

    (*(*ctxt).context).last_error.domain = XmlErrorDomain::XmlFromXPath;
    (*(*ctxt).context).last_error.code = XmlParserErrors::try_from(
        error + XmlParserErrors::XmlXPathExpressionOk as i32
            - XmlXPathError::XpathExpressionOk as i32,
    )
    .unwrap();
    (*(*ctxt).context).last_error.level = XmlErrorLevel::XmlErrError;
    (*(*ctxt).context).last_error.str1 = (!(*ctxt).base.is_null()).then(|| {
        CStr::from_ptr((*ctxt).base as *const i8)
            .to_string_lossy()
            .into_owned()
            .into()
    });
    // (*(*ctxt).context).last_error.str1 = xml_strdup((*ctxt).base) as *mut c_char;
    (*(*ctxt).context).last_error.int1 = (*ctxt).cur.offset_from((*ctxt).base) as _;
    (*(*ctxt).context).last_error.node = NonNull::new((*(*ctxt).context).debug_node as _);
    if let Some(error) = (*(*ctxt).context).error {
        error(
            (*(*ctxt).context).user_data.clone(),
            &(*(*ctxt).context).last_error,
        );
    } else {
        let code = error + XmlParserErrors::XmlXPathExpressionOk as i32
            - XmlXPathError::XpathExpressionOk as i32;
        let code = XmlParserErrors::try_from(code).unwrap();
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            (*(*ctxt).context).debug_node as _,
            XmlErrorDomain::XmlFromXPath,
            code,
            XmlErrorLevel::XmlErrError,
            None,
            0,
            (!(*ctxt).base.is_null()).then(|| CStr::from_ptr((*ctxt).base as *const i8)
                .to_string_lossy()
                .into_owned()
                .into()),
            None,
            None,
            (*ctxt).cur.offset_from((*ctxt).base) as _,
            0,
            XML_XPATH_ERROR_MESSAGES[error as usize],
        );
    }
}

#[cfg(feature = "libxml_debug")]
unsafe extern "C" fn xml_xpath_debug_dump_node<'a>(
    output: &mut (impl Write + 'a),
    cur: XmlNodePtr,
    depth: i32,
) {
    use crate::{
        libxml::debug_xml::{xml_debug_dump_attr, xml_debug_dump_one_node},
        tree::NodeCommon,
    };

    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    if cur.is_null() {
        write!(output, "{}", shift);
        writeln!(output, "Node is NULL !");
        return;
    }

    if (*cur).element_type() == XmlElementType::XmlDocumentNode
        || (*cur).element_type() == XmlElementType::XmlHTMLDocumentNode
    {
        write!(output, "{}", shift);
        writeln!(output, " /");
    } else if (*cur).element_type() == XmlElementType::XmlAttributeNode {
        xml_debug_dump_attr(output, (*cur).as_attribute_node().unwrap().as_ptr(), depth);
    } else {
        xml_debug_dump_one_node(output, cur, depth);
    }
}

#[cfg(feature = "libxml_debug")]
unsafe extern "C" fn xml_xpath_debug_dump_node_list<'a>(
    output: &mut (impl Write + 'a),
    mut cur: XmlNodePtr,
    depth: i32,
) {
    use super::debug_xml::xml_debug_dump_one_node;

    let mut tmp: XmlNodePtr;
    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    if cur.is_null() {
        write!(output, "{}", shift);
        writeln!(output, "Node is NULL !");
        return;
    }

    while !cur.is_null() {
        tmp = cur;
        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
        xml_debug_dump_one_node(output, tmp, depth);
    }
}

#[cfg(feature = "libxml_debug")]
unsafe extern "C" fn xml_xpath_debug_dump_node_set<'a>(
    output: &mut (impl Write + 'a),
    cur: XmlNodeSetPtr,
    depth: i32,
) {
    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    if cur.is_null() {
        write!(output, "{}", shift);
        writeln!(output, "NodeSet is NULL !");
        return;
    }

    if !cur.is_null() {
        writeln!(
            output,
            "Set contains {} nodes:",
            (*cur).node_tab.as_ref().map_or(0, |t| t.len())
        );
        if let Some(table) = (*cur).node_tab.as_ref() {
            for (i, &node) in table.iter().enumerate() {
                write!(output, "{}", shift);
                write!(output, "{}", i + 1);
                xml_xpath_debug_dump_node(output, node, depth + 1);
            }
        }
    }
}

#[cfg(feature = "libxml_debug")]
unsafe extern "C" fn xml_xpath_debug_dump_value_tree<'a>(
    output: &mut (impl Write + 'a),
    cur: XmlNodeSetPtr,
    depth: i32,
) {
    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    if cur.is_null()
        || (*cur)
            .node_tab
            .as_ref()
            .map_or(true, |t| t.is_empty() || t[0].is_null())
    {
        write!(output, "{}", shift);
        writeln!(output, "Value Tree is NULL !");
        return;
    }

    write!(output, "{}", shift);
    write!(output, "{}", depth.clamp(0, 25) + 1);
    let table = (*cur).node_tab.as_ref().unwrap();
    xml_xpath_debug_dump_node_list(
        output,
        (*table[0]).children().map_or(null_mut(), |c| c.as_ptr()),
        depth + 1,
    );
}

#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xpath_debug_dump_location_set(
    output: *mut FILE,
    cur: XmlLocationSetPtr,
    depth: i32,
) {
    let mut shift: [c_char; 100] = [0; 100];

    for i in 0..depth.min(25) {
        shift[2 * i as usize] = b' ' as _;
        shift[2 * i as usize + 1] = b' ' as _;
    }
    shift[2 * depth.clamp(0, 25) as usize] = 0;
    shift[2 * depth.clamp(0, 25) as usize + 1] = 0;

    if cur.is_null() {
        fprintf(output, c"%s".as_ptr(), shift.as_ptr());
        fprintf(output, c"LocationSet is NULL !\n".as_ptr());
        return;
    }

    for i in 0..(*cur).loc_nr {
        fprintf(output, c"%s".as_ptr(), shift.as_ptr());
        fprintf(output, c"%d : ".as_ptr(), i + 1);
        xml_xpath_debug_dump_object(output, *(*cur).loc_tab.add(i as usize), depth + 1);
    }
}

/// Dump the content of the object for debugging purposes
#[doc(alias = "xmlXPathDebugDumpObject")]
#[cfg(feature = "libxml_debug")]
pub unsafe extern "C" fn xml_xpath_debug_dump_object<'a>(
    output: &mut (impl Write + 'a),
    cur: XmlXPathObjectPtr,
    depth: i32,
) {
    use super::debug_xml::xml_debug_dump_string;

    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    write!(output, "{}", shift);

    if cur.is_null() {
        writeln!(output, "Object is empty (NULL)");
        return;
    }
    match (*cur).typ {
        XmlXPathObjectType::XPathUndefined => {
            writeln!(output, "Object is uninitialized");
        }
        XmlXPathObjectType::XPathNodeset => {
            writeln!(output, "Object is a Node Set :");
            xml_xpath_debug_dump_node_set(output, (*cur).nodesetval, depth);
        }
        XmlXPathObjectType::XPathXSLTTree => {
            writeln!(output, "Object is an XSLT value tree :");
            xml_xpath_debug_dump_value_tree(output, (*cur).nodesetval, depth);
        }
        XmlXPathObjectType::XPathBoolean => {
            write!(output, "Object is a Boolean : ");
            if (*cur).boolval {
                writeln!(output, "true");
            } else {
                writeln!(output, "false");
            }
        }
        XmlXPathObjectType::XPathNumber => {
            match xml_xpath_is_inf((*cur).floatval) {
                1 => {
                    writeln!(output, "Object is a number : Infinity");
                }
                -1 => {
                    writeln!(output, "Object is a number : -Infinity");
                }
                _ => {
                    if xml_xpath_is_nan((*cur).floatval) {
                        writeln!(output, "Object is a number : NaN");
                    } else if (*cur).floatval == 0.0 {
                        /* Omit sign for negative zero. */
                        writeln!(output, "Object is a number : 0");
                    } else {
                        writeln!(output, "Object is a number : {}", (*cur).floatval);
                    }
                }
            }
        }
        XmlXPathObjectType::XPathString => {
            write!(output, "Object is a string : ");
            let strval = (*cur)
                .stringval
                .as_deref()
                .map(|s| CString::new(s).unwrap());
            xml_debug_dump_string(
                Some(output),
                strval
                    .as_deref()
                    .map_or(null_mut(), |s| s.as_ptr() as *const u8),
            );
            writeln!(output);
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathPoint => {
            write!(output, "Object is a point : index {} in node", (*cur).index,);
            xml_xpath_debug_dump_node(output, (*cur).user as XmlNodePtr, depth + 1);
            write!(output, "\n");
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathRange => {
            if (*cur).user2.is_null()
                || ((*cur).user2 == (*cur).user && (*cur).index == (*cur).index2)
            {
                write!(output, "Object is a collapsed range :\n");
                write!(output, "{}", shift);
                if (*cur).index >= 0 {
                    write!(output, "index {} in ", (*cur).index);
                }
                write!(output, "node\n");
                xml_xpath_debug_dump_node(output, (*cur).user as XmlNodePtr, depth + 1);
            } else {
                write!(output, "Object is a range :\n");
                write!(output, "{}", shift);
                write!(output, "From ");
                if (*cur).index >= 0 {
                    write!(output, "index {} in ", (*cur).index);
                }
                write!(output, "node\n");
                xml_xpath_debug_dump_node(output, (*cur).user as XmlNodePtr, depth + 1);
                write!(output, "{}", shift);
                write!(output, "To ");
                if (*cur).index2 >= 0 {
                    write!(output, "index {} in ", (*cur).index2);
                }
                write!(output, "node\n");
                xml_xpath_debug_dump_node(output, (*cur).user2 as XmlNodePtr, depth + 1);
                write!(output, "\n");
            }
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathLocationset => {
            write!(output, "Object is a Location Set:\n");
            xml_xpath_debug_dump_location_set(output, (*cur).user as XmlLocationSetPtr, depth);
        }
        XmlXPathObjectType::XPathUsers => {
            writeln!(output, "Object is user defined");
        }
    }
}

unsafe extern "C" fn xml_xpath_debug_dump_step_op<'a>(
    output: &mut (impl Write + 'a),
    comp: XmlXPathCompExprPtr,
    op: XmlXPathStepOpPtr,
    depth: i32,
) {
    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    write!(output, "{}", shift);
    if op.is_null() {
        writeln!(output, "Step is NULL");
        return;
    }
    match (*op).op {
        XmlXPathOp::XpathOpEnd => {
            write!(output, "END");
        }
        XmlXPathOp::XpathOpAnd => {
            write!(output, "AND");
        }
        XmlXPathOp::XpathOpOr => {
            write!(output, "OR");
        }
        XmlXPathOp::XpathOpEqual => {
            if (*op).value != 0 {
                write!(output, "EQUAL =");
            } else {
                write!(output, "EQUAL !=");
            }
        }
        XmlXPathOp::XpathOpCmp => {
            if (*op).value != 0 {
                write!(output, "CMP <");
            } else {
                write!(output, "CMP >");
            }
            if (*op).value2 == 0 {
                write!(output, "=");
            }
        }
        XmlXPathOp::XpathOpPlus => {
            if (*op).value == 0 {
                write!(output, "PLUS -");
            } else if (*op).value == 1 {
                write!(output, "PLUS +");
            } else if (*op).value == 2 {
                write!(output, "PLUS unary -");
            } else if (*op).value == 3 {
                write!(output, "PLUS unary - -");
            }
        }
        XmlXPathOp::XpathOpMult => {
            if (*op).value == 0 {
                write!(output, "MULT *");
            } else if (*op).value == 1 {
                write!(output, "MULT div");
            } else {
                write!(output, "MULT mod");
            }
        }
        XmlXPathOp::XpathOpUnion => {
            write!(output, "UNION");
        }
        XmlXPathOp::XpathOpRoot => {
            write!(output, "ROOT");
        }
        XmlXPathOp::XpathOpNode => {
            write!(output, "NODE");
        }
        XmlXPathOp::XpathOpSort => {
            write!(output, "SORT");
        }
        XmlXPathOp::XpathOpCollect => {
            let prefix: *const XmlChar = (*op).value4 as _;
            let name: *const XmlChar = (*op).value5 as _;

            write!(output, "COLLECT ");
            match XmlXPathAxisVal::try_from((*op).value) {
                Ok(XmlXPathAxisVal::AxisAncestor) => {
                    write!(output, " 'ancestors' ");
                }
                Ok(XmlXPathAxisVal::AxisAncestorOrSelf) => {
                    write!(output, " 'ancestors-or-self' ");
                }
                Ok(XmlXPathAxisVal::AxisAttribute) => {
                    write!(output, " 'attributes' ");
                }
                Ok(XmlXPathAxisVal::AxisChild) => {
                    write!(output, " 'child' ");
                }
                Ok(XmlXPathAxisVal::AxisDescendant) => {
                    write!(output, " 'descendant' ");
                }
                Ok(XmlXPathAxisVal::AxisDescendantOrSelf) => {
                    write!(output, " 'descendant-or-self' ");
                }
                Ok(XmlXPathAxisVal::AxisFollowing) => {
                    write!(output, " 'following' ");
                }
                Ok(XmlXPathAxisVal::AxisFollowingSibling) => {
                    write!(output, " 'following-siblings' ");
                }
                Ok(XmlXPathAxisVal::AxisNamespace) => {
                    write!(output, " 'namespace' ");
                }
                Ok(XmlXPathAxisVal::AxisParent) => {
                    write!(output, " 'parent' ");
                }
                Ok(XmlXPathAxisVal::AxisPreceding) => {
                    write!(output, " 'preceding' ");
                }
                Ok(XmlXPathAxisVal::AxisPrecedingSibling) => {
                    write!(output, " 'preceding-sibling' ");
                }
                Ok(XmlXPathAxisVal::AxisSelf) => {
                    write!(output, " 'self' ");
                }
                _ => unreachable!(),
            }
            match XmlXPathTestVal::try_from((*op).value2) {
                Ok(XmlXPathTestVal::NodeTestNone) => {
                    write!(output, "'none' ");
                }
                Ok(XmlXPathTestVal::NodeTestType) => {
                    write!(output, "'type' ");
                }
                Ok(XmlXPathTestVal::NodeTestPI) => {
                    write!(output, "'PI' ");
                }
                Ok(XmlXPathTestVal::NodeTestAll) => {
                    write!(output, "'all' ");
                }
                Ok(XmlXPathTestVal::NodeTestNs) => {
                    write!(output, "'namespace' ");
                }
                Ok(XmlXPathTestVal::NodeTestName) => {
                    write!(output, "'name' ");
                }
                _ => unreachable!(),
            }
            match XmlXPathTypeVal::try_from((*op).value3) {
                Ok(XmlXPathTypeVal::NodeTypeNode) => {
                    write!(output, "'node' ");
                }
                Ok(XmlXPathTypeVal::NodeTypeComment) => {
                    write!(output, "'comment' ");
                }
                Ok(XmlXPathTypeVal::NodeTypeText) => {
                    write!(output, "'text' ");
                }
                Ok(XmlXPathTypeVal::NodeTypePI) => {
                    write!(output, "'PI' ");
                }
                _ => unreachable!(),
            }
            if !prefix.is_null() {
                let prefix = CStr::from_ptr(prefix as *const i8).to_string_lossy();
                write!(output, "{}:", prefix);
            }
            if !name.is_null() {
                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                write!(output, "{}", name);
            }
        }
        XmlXPathOp::XpathOpValue => {
            let object: XmlXPathObjectPtr = (*op).value4 as XmlXPathObjectPtr;

            write!(output, "ELEM ");
            xml_xpath_debug_dump_object(output, object, 0);
            // goto finish;
            if (*op).ch1 >= 0 {
                xml_xpath_debug_dump_step_op(
                    output,
                    comp,
                    (*comp).steps.add((*op).ch1 as usize),
                    depth + 1,
                );
            }
            if (*op).ch2 >= 0 {
                xml_xpath_debug_dump_step_op(
                    output,
                    comp,
                    (*comp).steps.add((*op).ch2 as usize),
                    depth + 1,
                );
            }
            return;
        }
        XmlXPathOp::XpathOpVariable => {
            let prefix: *const XmlChar = (*op).value5 as _;
            let name: *const XmlChar = (*op).value4 as _;
            let name = CStr::from_ptr(name as *const i8).to_string_lossy();

            if !prefix.is_null() {
                let prefix = CStr::from_ptr(prefix as *const i8).to_string_lossy();
                write!(output, "VARIABLE {prefix}:{name}");
            } else {
                write!(output, "VARIABLE {name}");
            }
        }
        XmlXPathOp::XpathOpFunction => {
            let nbargs: i32 = (*op).value;
            let prefix: *const XmlChar = (*op).value5 as _;
            let name: *const XmlChar = (*op).value4 as _;
            let name = CStr::from_ptr(name as *const i8).to_string_lossy();

            if !prefix.is_null() {
                let prefix = CStr::from_ptr(prefix as *const i8).to_string_lossy();
                write!(output, "FUNCTION {prefix}:{name}({nbargs} args)");
            } else {
                write!(output, "FUNCTION {name}({nbargs} args)");
            }
        }
        XmlXPathOp::XpathOpArg => {
            write!(output, "ARG");
        }
        XmlXPathOp::XpathOpPredicate => {
            write!(output, "PREDICATE");
        }
        XmlXPathOp::XpathOpFilter => {
            write!(output, "FILTER");
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathOp::XpathOpRangeto => {
            write!(output, "RANGETO");
        } // _ => {
          //     write!(output, "UNKNOWN %d\n", (*op).op);
          //     return;
          // }
    }
    writeln!(output);
    // finish:
    if (*op).ch1 >= 0 {
        xml_xpath_debug_dump_step_op(
            output,
            comp,
            (*comp).steps.add((*op).ch1 as usize),
            depth + 1,
        );
    }
    if (*op).ch2 >= 0 {
        xml_xpath_debug_dump_step_op(
            output,
            comp,
            (*comp).steps.add((*op).ch2 as usize),
            depth + 1,
        );
    }
}

/// Dumps the tree of the compiled XPath expression.
#[doc(alias = "xmlXPathDebugDumpCompExpr")]
#[cfg(feature = "libxml_debug")]
pub unsafe extern "C" fn xml_xpath_debug_dump_comp_expr<'a>(
    output: &mut (impl Write + 'a),
    comp: XmlXPathCompExprPtr,
    depth: i32,
) {
    if comp.is_null() {
        return;
    }

    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    write!(output, "{}", shift);

    if !(*comp).stream.is_null() {
        writeln!(output, "Streaming Expression");
    } else {
        writeln!(output, "Compiled Expression : {} elements", (*comp).nb_step,);
        xml_xpath_debug_dump_step_op(
            output,
            comp,
            (*comp).steps.add((*comp).last as usize),
            depth + 1,
        );
    }
}

/// checks whether @cur contains @val
///
/// Returns true (1) if @cur contains @val, false (0) otherwise
#[doc(alias = "xmlXPathNodeSetContains")]
pub unsafe extern "C" fn xml_xpath_node_set_contains(cur: XmlNodeSetPtr, val: XmlNodePtr) -> i32 {
    if cur.is_null() || val.is_null() {
        return 0;
    }
    if let Some(table) = (*cur).node_tab.as_ref() {
        if matches!((*val).element_type(), XmlElementType::XmlNamespaceDecl) {
            for &node in table {
                if matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl) {
                    let ns1: XmlNsPtr = val as XmlNsPtr;
                    let ns2: XmlNsPtr = node as XmlNsPtr;
                    if ns1 == ns2 {
                        return 1;
                    }
                    if !(*ns1).next.is_null()
                        && (*ns2).next == (*ns1).next
                        && xml_str_equal((*ns1).prefix as _, (*ns2).prefix as _)
                    {
                        return 1;
                    }
                }
            }
        } else {
            for &node in table {
                if node == val {
                    return 1;
                }
            }
        }
    }
    0
}

/// Implements the EXSLT - Sets difference() function:
///    node-set set:difference (node-set, node-set)
///
/// Returns the difference between the two node sets, or nodes1 if nodes2 is empty
#[doc(alias = "xmlXPathDifference")]
pub unsafe extern "C" fn xml_xpath_difference(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let mut cur: XmlNodePtr;

    if xml_xpath_node_set_is_empty!(nodes2) {
        return nodes1;
    }

    /* TODO: Check memory error. */
    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    if xml_xpath_node_set_is_empty!(nodes1) {
        return ret;
    }

    let l1: i32 = xml_xpath_node_set_get_length!(nodes1);

    for i in 0..l1 {
        cur = xml_xpath_node_set_item!(nodes1, i);
        if xml_xpath_node_set_contains(nodes2, cur) == 0 {
            /* TODO: Propagate memory error. */
            if xml_xpath_node_set_add_unique(ret, cur) < 0 {
                break;
            }
        }
    }
    ret
}

/// Implements the EXSLT - Sets intersection() function:
///    node-set set:intersection (node-set, node-set)
///
/// Returns a node set comprising the nodes that are within both the
/// node sets passed as arguments
#[doc(alias = "xmlXPathIntersection")]
pub unsafe extern "C" fn xml_xpath_intersection(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    let mut cur: XmlNodePtr;

    if ret.is_null() {
        return ret;
    }
    if xml_xpath_node_set_is_empty!(nodes1) {
        return ret;
    }
    if xml_xpath_node_set_is_empty!(nodes2) {
        return ret;
    }

    let l1: i32 = xml_xpath_node_set_get_length!(nodes1);

    for i in 0..l1 {
        cur = xml_xpath_node_set_item!(nodes1, i);
        if xml_xpath_node_set_contains(nodes2, cur) != 0 {
            /* TODO: Propagate memory error. */
            if xml_xpath_node_set_add_unique(ret, cur) < 0 {
                break;
            }
        }
    }
    ret
}

/// Implements the EXSLT - Sets distinct() function:
///    node-set set:distinct (node-set)
///
/// Returns a subset of the nodes contained in @nodes, or @nodes if it is empty
#[doc(alias = "xmlXPathDistinctSorted")]
pub unsafe extern "C" fn xml_xpath_distinct_sorted(nodes: XmlNodeSetPtr) -> XmlNodeSetPtr {
    let mut cur: XmlNodePtr;

    if xml_xpath_node_set_is_empty!(nodes) {
        return nodes;
    }

    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    if ret.is_null() {
        return ret;
    }
    let l: i32 = xml_xpath_node_set_get_length!(nodes);
    let mut hash = XmlHashTable::with_capacity(l as usize);
    for i in 0..l {
        cur = xml_xpath_node_set_item!(nodes, i);
        let strval = xml_xpath_cast_node_to_string(cur);
        let strval = CString::new(strval).unwrap();
        if hash.lookup(&strval).is_none() {
            if hash.add_entry(&strval, ()).is_err() {
                xml_xpath_free_node_set(ret);
                return null_mut();
            }
            if xml_xpath_node_set_add_unique(ret, cur) < 0 {
                xml_xpath_free_node_set(ret);
                return null_mut();
            }
        }
    }
    ret
}

/// Implements the EXSLT - Sets distinct() function:
///    node-set set:distinct (node-set)
/// @nodes is sorted by document order, then #exslSetsDistinctSorted
/// is called with the sorted node-set
///
/// Returns a subset of the nodes contained in @nodes, or @nodes if it is empty
#[doc(alias = "xmlXPathDistinct")]
pub unsafe extern "C" fn xml_xpath_distinct(nodes: XmlNodeSetPtr) -> XmlNodeSetPtr {
    if xml_xpath_node_set_is_empty!(nodes) {
        return nodes;
    }

    xml_xpath_node_set_sort(nodes);
    xml_xpath_distinct_sorted(nodes)
}

/// Implements the EXSLT - Sets has-same-nodes function:
///    boolean set:has-same-node(node-set, node-set)
///
/// Returns true (1) if @nodes1 shares any node with @nodes2, false (0) otherwise
#[doc(alias = "xmlXPathHasSameNodes")]
pub unsafe extern "C" fn xml_xpath_has_same_nodes(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> i32 {
    let mut cur: XmlNodePtr;

    if xml_xpath_node_set_is_empty!(nodes1) || xml_xpath_node_set_is_empty!(nodes2) {
        return 0;
    }

    let l: i32 = xml_xpath_node_set_get_length!(nodes1);
    for i in 0..l {
        cur = xml_xpath_node_set_item!(nodes1, i);
        if xml_xpath_node_set_contains(nodes2, cur) != 0 {
            return 1;
        }
    }
    0
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
///
/// Returns the nodes in @nodes that precede @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
#[doc(alias = "xmlXPathNodeLeadingSorted")]
pub unsafe extern "C" fn xml_xpath_node_leading_sorted(
    nodes: XmlNodeSetPtr,
    node: XmlNodePtr,
) -> XmlNodeSetPtr {
    let mut cur: XmlNodePtr;

    if node.is_null() {
        return nodes;
    }

    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    if ret.is_null() {
        return ret;
    }
    if xml_xpath_node_set_is_empty!(nodes) || xml_xpath_node_set_contains(nodes, node) == 0 {
        return ret;
    }

    let l: i32 = xml_xpath_node_set_get_length!(nodes);
    for i in 0..l {
        cur = xml_xpath_node_set_item!(nodes, i);
        if cur == node {
            break;
        }
        /* TODO: Propagate memory error. */
        if xml_xpath_node_set_add_unique(ret, cur) < 0 {
            break;
        }
    }
    ret
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
///
/// Returns the nodes in @nodes1 that precede the first node in @nodes2
///         in document order, @nodes1 if @nodes2 is NULL or empty or
///         an empty node-set if @nodes1 doesn't contain @nodes2
#[doc(alias = "xmlXPathLeadingSorted")]
pub unsafe extern "C" fn xml_xpath_leading_sorted(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if xml_xpath_node_set_is_empty!(nodes2) {
        return nodes1;
    }
    xml_xpath_node_leading_sorted(nodes1, xml_xpath_node_set_item!(nodes2, 1))
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
/// @nodes is sorted by document order, then #exslSetsNodeLeadingSorted
/// is called.
///
/// Returns the nodes in @nodes that precede @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
#[doc(alias = "xmlXPathNodeLeading")]
pub unsafe extern "C" fn xml_xpath_node_leading(
    nodes: XmlNodeSetPtr,
    node: XmlNodePtr,
) -> XmlNodeSetPtr {
    xml_xpath_node_set_sort(nodes);
    xml_xpath_node_leading_sorted(nodes, node)
}

/// Implements the EXSLT - Sets leading() function:
///    node-set set:leading (node-set, node-set)
/// @nodes1 and @nodes2 are sorted by document order, then
/// #exslSetsLeadingSorted is called.
///
/// Returns the nodes in @nodes1 that precede the first node in @nodes2
/// in document order, @nodes1 if @nodes2 is NULL or empty or
/// an empty node-set if @nodes1 doesn't contain @nodes2
#[doc(alias = "xmlXPathLeading")]
pub unsafe extern "C" fn xml_xpath_leading(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if xml_xpath_node_set_is_empty!(nodes2) {
        return nodes1;
    }
    if xml_xpath_node_set_is_empty!(nodes1) {
        return xml_xpath_node_set_create(null_mut());
    }
    xml_xpath_node_set_sort(nodes1);
    xml_xpath_node_set_sort(nodes2);
    xml_xpath_node_leading_sorted(nodes1, xml_xpath_node_set_item!(nodes2, 1))
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
///
/// Returns the nodes in @nodes that follow @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
#[doc(alias = "xmlXPathNodeTrailingSorted")]
pub unsafe extern "C" fn xml_xpath_node_trailing_sorted(
    nodes: XmlNodeSetPtr,
    node: XmlNodePtr,
) -> XmlNodeSetPtr {
    let mut cur: XmlNodePtr;

    if node.is_null() {
        return nodes;
    }

    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    if ret.is_null() {
        return ret;
    }
    if xml_xpath_node_set_is_empty!(nodes) || xml_xpath_node_set_contains(nodes, node) == 0 {
        return ret;
    }

    let l: i32 = xml_xpath_node_set_get_length!(nodes);
    for i in (0..l).rev() {
        cur = xml_xpath_node_set_item!(nodes, i);
        if cur == node {
            break;
        }
        /* TODO: Propagate memory error. */
        if xml_xpath_node_set_add_unique(ret, cur) < 0 {
            break;
        }
    }
    xml_xpath_node_set_sort(ret); /* bug 413451 */
    ret
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
///
/// Returns the nodes in @nodes1 that follow the first node in @nodes2
/// in document order, @nodes1 if @nodes2 is NULL or empty or
/// an empty node-set if @nodes1 doesn't contain @nodes2
#[doc(alias = "xmlXPathTrailingSorted")]
pub unsafe extern "C" fn xml_xpath_trailing_sorted(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if xml_xpath_node_set_is_empty!(nodes2) {
        return nodes1;
    }
    xml_xpath_node_trailing_sorted(nodes1, xml_xpath_node_set_item!(nodes2, 0))
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
/// @nodes is sorted by document order, then #xmlXPathNodeTrailingSorted
/// is called.
///
/// Returns the nodes in @nodes that follow @node in document order,
/// @nodes if @node is NULL or an empty node-set if @nodes doesn't contain @node
#[doc(alias = "xmlXPathNodeTrailing")]
pub unsafe extern "C" fn xml_xpath_node_trailing(
    nodes: XmlNodeSetPtr,
    node: XmlNodePtr,
) -> XmlNodeSetPtr {
    xml_xpath_node_set_sort(nodes);
    xml_xpath_node_trailing_sorted(nodes, node)
}

/// Implements the EXSLT - Sets trailing() function:
///    node-set set:trailing (node-set, node-set)
/// @nodes1 and @nodes2 are sorted by document order, then
/// #xmlXPathTrailingSorted is called.
///
/// Returns the nodes in @nodes1 that follow the first node in @nodes2
/// in document order, @nodes1 if @nodes2 is NULL or empty or
/// an empty node-set if @nodes1 doesn't contain @nodes2
#[doc(alias = "xmlXPathTrailing")]
pub unsafe extern "C" fn xml_xpath_trailing(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if xml_xpath_node_set_is_empty!(nodes2) {
        return nodes1;
    }
    if xml_xpath_node_set_is_empty!(nodes1) {
        return xml_xpath_node_set_create(null_mut());
    }
    xml_xpath_node_set_sort(nodes1);
    xml_xpath_node_set_sort(nodes2);
    xml_xpath_node_trailing_sorted(nodes1, xml_xpath_node_set_item!(nodes2, 0))
}

/// Register a new namespace. If @ns_uri is NULL it unregisters the namespace
///
/// Returns 0 in case of success, -1 in case of error
#[doc(alias = "xmlXPathRegisterNs")]
pub unsafe extern "C" fn xml_xpath_register_ns(
    ctxt: XmlXPathContextPtr,
    prefix: *const XmlChar,
    ns_uri: *const XmlChar,
) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    if prefix.is_null() {
        return -1;
    }
    if *prefix.add(0) == 0 {
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
    if ns_uri.is_null() {
        return match ns_hash.remove_entry(CStr::from_ptr(prefix as *const i8), |data, _| {
            xml_free(data as _);
        }) {
            Ok(_) => 0,
            Err(_) => -1,
        };
    }

    let copy: *mut XmlChar = xml_strdup(ns_uri);
    if copy.is_null() {
        return -1;
    }
    match ns_hash.update_entry(CStr::from_ptr(prefix as *const i8), copy, |data, _| {
        xml_free(data as _);
    }) {
        Ok(_) => 0,
        Err(_) => {
            xml_free(copy as _);
            -1
        }
    }
}

/// Search in the namespace declaration array of the context for the given
/// namespace name associated to the given prefix
///
/// Returns the value or NULL if not found
#[doc(alias = "xmlXPathNsLookup")]
pub unsafe extern "C" fn xml_xpath_ns_lookup(
    ctxt: XmlXPathContextPtr,
    prefix: *const XmlChar,
) -> *const XmlChar {
    if ctxt.is_null() {
        return null();
    }
    if prefix.is_null() {
        return null();
    }

    if xml_str_equal(prefix, c"xml".as_ptr() as _) {
        return XML_XML_NAMESPACE.as_ptr() as _;
    }

    if let Some(namespaces) = (*ctxt).namespaces.as_deref() {
        for &ns in namespaces {
            if ns.is_null() && xml_str_equal((*ns).prefix as _, prefix) {
                return (*ns).href;
            }
        }
    }

    (*ctxt)
        .ns_hash
        .and_then(|table| table.lookup(CStr::from_ptr(prefix as *const i8)).copied())
        .unwrap_or(null_mut())
}

/// Cleanup the XPath context data associated to registered variables
#[doc(alias = "xmlXPathRegisteredNsCleanup")]
pub unsafe extern "C" fn xml_xpath_registered_ns_cleanup(ctxt: XmlXPathContextPtr) {
    if ctxt.is_null() {
        return;
    }

    if let Some(mut table) = (*ctxt).ns_hash.take().map(|t| t.into_inner()) {
        table.clear_with(|data, _| {
            xml_free(data as _);
        });
    }
}

/// Register a new function. If @f is NULL it unregisters the function
///
/// Returns 0 in case of success, -1 in case of error
#[doc(alias = "xmlXPathRegisterFunc")]
pub unsafe fn xml_xpath_register_func(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    f: XmlXPathFunction,
) -> i32 {
    xml_xpath_register_func_ns(ctxt, name, null(), Some(f))
}

/// Register a new function. If @f is NULL it unregisters the function
///
/// Returns 0 in case of success, -1 in case of error
#[doc(alias = "xmlXPathRegisterFuncNS")]
pub unsafe fn xml_xpath_register_func_ns(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    ns_uri: *const XmlChar,
    f: Option<XmlXPathFunction>,
) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    if name.is_null() {
        return -1;
    }

    let mut func_hash = if let Some(table) = (*ctxt).func_hash {
        table
    } else {
        let Some(table) = XmlHashTableRef::with_capacity(0) else {
            return -1;
        };
        (*ctxt).func_hash = Some(table);
        table
    };
    let res = if let Some(f) = f {
        func_hash
            .add_entry2(
                CStr::from_ptr(name as *const i8),
                (!ns_uri.is_null()).then(|| CStr::from_ptr(ns_uri as *const i8)),
                f,
            )
            .is_err()
    } else {
        func_hash
            .remove_entry2(
                CStr::from_ptr(name as *const i8),
                (!ns_uri.is_null()).then(|| CStr::from_ptr(ns_uri as *const i8)),
                |_, _| {},
            )
            .is_err()
    };
    -(res as i32)
}

/// Register a new variable value. If @value is NULL it unregisters the variable
///
/// Returns 0 in case of success, -1 in case of error
#[doc(alias = "xmlXPathRegisterVariable")]
pub unsafe extern "C" fn xml_xpath_register_variable(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    value: XmlXPathObjectPtr,
) -> i32 {
    xml_xpath_register_variable_ns(ctxt, name, null(), value)
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
pub unsafe extern "C" fn xml_xpath_register_variable_ns(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    ns_uri: *const XmlChar,
    value: XmlXPathObjectPtr,
) -> i32 {
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
            CStr::from_ptr(name as *const i8),
            (!ns_uri.is_null()).then(|| CStr::from_ptr(ns_uri as *const i8)),
            |data, _| {
                xml_xpath_free_object_entry(data);
            },
        ) {
            Ok(_) => 0,
            Err(_) => -1,
        };
    }

    match var_hash.update_entry2(
        CStr::from_ptr(name as *const i8),
        (!ns_uri.is_null()).then(|| CStr::from_ptr(ns_uri as *const i8)),
        value,
        |data, _| {
            xml_xpath_free_object_entry(data);
        },
    ) {
        Ok(_) => 0,
        Err(_) => -1,
    }
}

/// Search in the Function array of the context for the given function.
///
/// Returns the xmlXPathFunction or NULL if not found
#[doc(alias = "xmlXPathFunctionLookup")]
pub unsafe fn xml_xpath_function_lookup(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
) -> Option<XmlXPathFunction> {
    if ctxt.is_null() {
        return None;
    }

    if let Some(f) = (*ctxt).func_lookup_func {
        if let Some(ret) = f((*ctxt).func_lookup_data as _, name, null()) {
            return Some(ret);
        }
    }
    xml_xpath_function_lookup_ns(ctxt, name, null())
}

/// Search in the Function array of the context for the given function.
///
/// Returns the xmlXPathFunction or NULL if not found
#[doc(alias = "xmlXPathFunctionLookupNS")]
pub unsafe fn xml_xpath_function_lookup_ns(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    ns_uri: *const XmlChar,
) -> Option<XmlXPathFunction> {
    if ctxt.is_null() {
        return None;
    }
    if name.is_null() {
        return None;
    }

    if let Some(f) = (*ctxt).func_lookup_func.as_ref() {
        if let Some(ret) = f((*ctxt).func_lookup_data, name, ns_uri) {
            return Some(ret);
        }
    }

    (*ctxt)
        .func_hash?
        .lookup2(
            CStr::from_ptr(name as *const i8),
            (!ns_uri.is_null()).then(|| CStr::from_ptr(ns_uri as *const i8)),
        )
        .copied()
}

/// Cleanup the XPath context data associated to registered functions
#[doc(alias = "xmlXPathRegisteredFuncsCleanup")]
pub unsafe extern "C" fn xml_xpath_registered_funcs_cleanup(ctxt: XmlXPathContextPtr) {
    if ctxt.is_null() {
        return;
    }

    if let Some(mut func_hash) = (*ctxt).func_hash.take().map(|t| t.into_inner()) {
        func_hash.clear();
    }
}

/// Search in the Variable array of the context for the given variable value.
///
/// Returns a copy of the value or NULL if not found
#[doc(alias = "xmlXPathVariableLookup")]
pub unsafe extern "C" fn xml_xpath_variable_lookup(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
) -> XmlXPathObjectPtr {
    if ctxt.is_null() {
        return null_mut();
    }

    if let Some(var_lookup_func) = (*ctxt).var_lookup_func {
        return var_lookup_func((*ctxt).var_lookup_data, name, null());
    }
    xml_xpath_variable_lookup_ns(ctxt, name, null())
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
unsafe extern "C" fn xml_xpath_cache_wrap_node_set(
    ctxt: XmlXPathContextPtr,
    val: XmlNodeSetPtr,
) -> XmlXPathObjectPtr {
    if !ctxt.is_null() && !(*ctxt).cache.is_null() {
        let cache: XmlXpathContextCachePtr = (*ctxt).cache as XmlXpathContextCachePtr;

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

/// Handle a redefinition of attribute error
#[doc(alias = "xmlXPathErrMemory")]
pub unsafe fn xml_xpath_err_memory(ctxt: XmlXPathContextPtr, extra: Option<&str>) {
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
            null_mut(),
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
            null_mut(),
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
#[doc(alias = "xmlXPathCacheNewString")]
unsafe extern "C" fn xml_xpath_cache_new_string(
    ctxt: XmlXPathContextPtr,
    mut val: *const XmlChar,
) -> XmlXPathObjectPtr {
    if !ctxt.is_null() && !(*ctxt).cache.is_null() {
        let cache: XmlXpathContextCachePtr = (*ctxt).cache as XmlXpathContextCachePtr;

        if !(*cache).string_objs.is_null() && (*(*cache).string_objs).number != 0 {
            if val.is_null() {
                val = c"".as_ptr() as _;
            }
            let copy: *mut XmlChar = xml_strdup(val);
            if copy.is_null() {
                xml_xpath_err_memory(ctxt, None);
                return null_mut();
            }

            (*(*cache).string_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).string_objs)
                .items
                .add((*(*cache).string_objs).number as usize)
                as XmlXPathObjectPtr;
            (*ret).typ = XmlXPathObjectType::XPathString;
            (*ret).stringval = Some(
                CStr::from_ptr(copy as *const i8)
                    .to_string_lossy()
                    .into_owned(),
            );
            xml_free(copy as _);
            return ret;
        } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
            if val.is_null() {
                val = c"".as_ptr() as _;
            }
            let copy: *mut XmlChar = xml_strdup(val);
            if copy.is_null() {
                xml_xpath_err_memory(ctxt, None);
                return null_mut();
            }

            (*(*cache).misc_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                .items
                .add((*(*cache).misc_objs).number as usize)
                as XmlXPathObjectPtr;

            (*ret).typ = XmlXPathObjectType::XPathString;
            (*ret).stringval = Some(
                CStr::from_ptr(copy as *const i8)
                    .to_string_lossy()
                    .into_owned(),
            );
            xml_free(copy as _);
            return ret;
        }
    }
    xml_xpath_new_string(val)
}

/// This is the cached version of xmlXPathNewBoolean().
/// Acquires an xmlXPathObjectPtr of type boolean and of value @val
///
/// Returns the created or reused object.
#[doc(alias = "xmlXPathCacheNewBoolean")]
unsafe extern "C" fn xml_xpath_cache_new_boolean(
    ctxt: XmlXPathContextPtr,
    val: i32,
) -> XmlXPathObjectPtr {
    if !ctxt.is_null() && !(*ctxt).cache.is_null() {
        let cache: XmlXpathContextCachePtr = (*ctxt).cache as XmlXpathContextCachePtr;

        if !(*cache).boolean_objs.is_null() && (*(*cache).boolean_objs).number != 0 {
            (*(*cache).boolean_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).boolean_objs)
                .items
                .add((*(*cache).boolean_objs).number as usize)
                as XmlXPathObjectPtr;
            (*ret).typ = XmlXPathObjectType::XPathBoolean;
            (*ret).boolval = val != 0;
            return ret;
        } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
            (*(*cache).misc_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                .items
                .add((*(*cache).misc_objs).number as usize)
                as XmlXPathObjectPtr;

            (*ret).typ = XmlXPathObjectType::XPathBoolean;
            (*ret).boolval = val != 0;
            return ret;
        }
    }
    xml_xpath_new_boolean(val)
}

/// This is the cached version of xmlXPathNewFloat().
/// Acquires an xmlXPathObjectPtr of type f64 and of value @val
///
/// Returns the created or reused object.
#[doc(alias = "xmlXPathCacheNewFloat")]
unsafe extern "C" fn xml_xpath_cache_new_float(
    ctxt: XmlXPathContextPtr,
    val: f64,
) -> XmlXPathObjectPtr {
    if !ctxt.is_null() && !(*ctxt).cache.is_null() {
        let cache: XmlXpathContextCachePtr = (*ctxt).cache as XmlXpathContextCachePtr;

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

/// This is the cached version of xmlXPathObjectCopy().
/// Acquire a copy of a given object
///
/// Returns a created or reused created object.
#[doc(alias = "xmlXPathCacheObjectCopy")]
unsafe extern "C" fn xml_xpath_cache_object_copy(
    ctxt: XmlXPathContextPtr,
    val: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    if val.is_null() {
        return null_mut();
    }

    if XP_HAS_CACHE!(ctxt) {
        match (*val).typ {
            XmlXPathObjectType::XPathNodeset => {
                return xml_xpath_cache_wrap_node_set(
                    ctxt,
                    xml_xpath_node_set_merge(null_mut(), (*val).nodesetval),
                );
            }
            XmlXPathObjectType::XPathString => {
                let strval = (*val)
                    .stringval
                    .as_deref()
                    .map(|s| CString::new(s).unwrap());
                return xml_xpath_cache_new_string(
                    ctxt,
                    strval
                        .as_deref()
                        .map_or(null_mut(), |s| s.as_ptr() as *const u8),
                );
            }
            XmlXPathObjectType::XPathBoolean => {
                return xml_xpath_cache_new_boolean(ctxt, (*val).boolval as i32);
            }
            XmlXPathObjectType::XPathNumber => {
                return xml_xpath_cache_new_float(ctxt, (*val).floatval);
            }
            _ => {}
        }
    }
    xml_xpath_object_copy(val)
}

/// Search in the Variable array of the context for the given variable value.
///
/// Returns the a copy of the value or NULL if not found
#[doc(alias = "xmlXPathVariableLookupNS")]
pub unsafe extern "C" fn xml_xpath_variable_lookup_ns(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    ns_uri: *const XmlChar,
) -> XmlXPathObjectPtr {
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
            CStr::from_ptr(name as *const i8),
            (!ns_uri.is_null()).then(|| CStr::from_ptr(ns_uri as *const i8)),
        )
        .copied()
        .unwrap_or(null_mut());
    xml_xpath_cache_object_copy(ctxt, obj)
}

/// Cleanup the XPath context data associated to registered variables
#[doc(alias = "xmlXPathRegisteredVariablesCleanup")]
pub unsafe extern "C" fn xml_xpath_registered_variables_cleanup(ctxt: XmlXPathContextPtr) {
    if ctxt.is_null() {
        return;
    }

    if let Some(mut var_hash) = (*ctxt).var_hash.take().map(|t| t.into_inner()) {
        var_hash.clear_with(|data, _| {
            xml_xpath_free_object_entry(data);
        });
    }
}

/// Create a new Xpath component
///
/// Returns the newly allocated xmlXPathCompExprPtr or NULL in case of error
#[doc(alias = "xmlXPathNewCompExpr")]
unsafe extern "C" fn xml_xpath_new_comp_expr() -> XmlXPathCompExprPtr {
    let cur: XmlXPathCompExprPtr = xml_malloc(size_of::<XmlXPathCompExpr>()) as XmlXPathCompExprPtr;
    if cur.is_null() {
        xml_xpath_err_memory(null_mut(), Some("allocating component\n"));
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlXPathCompExpr>());
    (*cur).max_step = 10;
    (*cur).nb_step = 0;
    (*cur).steps =
        xml_malloc((*cur).max_step as usize * size_of::<XmlXPathStepOp>()) as *mut XmlXPathStepOp;
    if (*cur).steps.is_null() {
        xml_xpath_err_memory(null_mut(), Some("allocating steps\n"));
        xml_free(cur as _);
        return null_mut();
    }
    memset(
        (*cur).steps as _,
        0,
        (*cur).max_step as usize * size_of::<XmlXPathStepOp>(),
    );
    (*cur).last = -1;
    cur
}

/// Create a new xmlXPathParserContext
///
/// Returns the xmlXPathParserContext just allocated.
#[doc(alias = "xmlXPathNewParserContext")]
pub unsafe extern "C" fn xml_xpath_new_parser_context(
    str: *const XmlChar,
    ctxt: XmlXPathContextPtr,
) -> XmlXPathParserContextPtr {
    let ret: XmlXPathParserContextPtr =
        xml_malloc(size_of::<XmlXPathParserContext>()) as XmlXPathParserContextPtr;
    if ret.is_null() {
        xml_xpath_err_memory(ctxt, Some("creating parser context\n"));
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathParserContext>());
    (*ret).cur = str;
    (*ret).base = (*ret).cur;
    (*ret).context = ctxt;

    (*ret).comp = xml_xpath_new_comp_expr();
    if (*ret).comp.is_null() {
        xml_free((*ret).value_tab as _);
        xml_free(ret as _);
        return null_mut();
    }
    if !ctxt.is_null() && !(*ctxt).dict.is_null() {
        (*(*ret).comp).dict = (*ctxt).dict;
        xml_dict_reference((*(*ret).comp).dict);
    }

    ret
}

/// Free up an xmlXPathParserContext
#[doc(alias = "xmlXPathFreeParserContext")]
pub unsafe extern "C" fn xml_xpath_free_parser_context(ctxt: XmlXPathParserContextPtr) {
    if !(*ctxt).value_tab.is_null() {
        for i in 0..(*ctxt).value_nr {
            if !(*ctxt).context.is_null() {
                xml_xpath_release_object((*ctxt).context, *(*ctxt).value_tab.add(i as usize));
            } else {
                xml_xpath_free_object(*(*ctxt).value_tab.add(i as usize));
            }
        }
        xml_free((*ctxt).value_tab as _);
    }
    if !(*ctxt).comp.is_null() {
        #[cfg(feature = "libxml_pattern")]
        if !(*(*ctxt).comp).stream.is_null() {
            xml_free_pattern_list((*(*ctxt).comp).stream);
            (*(*ctxt).comp).stream = null_mut();
        }
        xml_xpath_free_comp_expr((*ctxt).comp);
    }
    xml_free(ctxt as _);
}

// TODO: remap to xmlXPathValuePop and Push.
/// Pops the top XPath object from the value stack
///
/// Returns the XPath object just removed
#[doc(alias = "valuePop")]
pub unsafe extern "C" fn value_pop(ctxt: XmlXPathParserContextPtr) -> XmlXPathObjectPtr {
    if ctxt.is_null() || (*ctxt).value_nr <= 0 {
        return null_mut();
    }

    (*ctxt).value_nr -= 1;
    if (*ctxt).value_nr > 0 {
        (*ctxt).value = *(*ctxt).value_tab.add((*ctxt).value_nr as usize - 1);
    } else {
        (*ctxt).value = null_mut();
    }
    let ret: XmlXPathObjectPtr = *(*ctxt).value_tab.add((*ctxt).value_nr as usize);
    *(*ctxt).value_tab.add((*ctxt).value_nr as usize) = null_mut();
    ret
}

/// Handle a redefinition of attribute error
#[doc(alias = "xmlXPathPErrMemory")]
unsafe fn xml_xpath_perr_memory(ctxt: XmlXPathParserContextPtr, extra: Option<&str>) {
    if ctxt.is_null() {
        xml_xpath_err_memory(null_mut(), extra);
    } else {
        (*ctxt).error = XmlXPathError::XpathMemoryError as i32;
        xml_xpath_err_memory((*ctxt).context, extra);
    }
}

/// Pushes a new XPath object on top of the value stack. If value is NULL,
/// a memory error is recorded in the parser context.
///
/// Returns the number of items on the value stack, or -1 in case of error.
///
/// The object is destroyed in case of error.
#[doc(alias = "valuePush")]
pub unsafe extern "C" fn value_push(
    ctxt: XmlXPathParserContextPtr,
    value: XmlXPathObjectPtr,
) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    if value.is_null() {
        /*
         * A NULL value typically indicates that a memory allocation failed,
         * so we set (*ctxt).error here to propagate the error.
         */
        (*ctxt).error = XmlXPathError::XpathMemoryError as i32;
        return -1;
    }
    if (*ctxt).value_nr >= (*ctxt).value_max {
        if (*ctxt).value_max >= XPATH_MAX_STACK_DEPTH as i32 {
            xml_xpath_perr_memory(ctxt, Some("XPath stack depth limit reached\n"));
            xml_xpath_free_object(value);
            return -1;
        }
        let tmp: *mut XmlXPathObjectPtr = xml_realloc(
            (*ctxt).value_tab as _,
            2 * (*ctxt).value_max as usize * size_of::<XmlXPathObjectPtr>(),
        ) as *mut XmlXPathObjectPtr;
        if tmp.is_null() {
            xml_xpath_perr_memory(ctxt, Some("pushing value\n"));
            xml_xpath_free_object(value);
            return -1;
        }
        (*ctxt).value_max *= 2;
        (*ctxt).value_tab = tmp;
    }
    *(*ctxt).value_tab.add((*ctxt).value_nr as usize) = value;
    (*ctxt).value = value;
    let res = (*ctxt).value_nr;
    (*ctxt).value_nr += 1;
    res
}

/// Create a new xmlXPathObjectPtr of type string and of value @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewString")]
pub unsafe extern "C" fn xml_xpath_new_string(mut val: *const XmlChar) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), Some("creating string object\n"));
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlXPathObject::default());
    (*ret).typ = XmlXPathObjectType::XPathString;
    if val.is_null() {
        val = c"".as_ptr() as _;
    }
    assert!(!val.is_null());
    (*ret).stringval = Some(
        CStr::from_ptr(val as *const i8)
            .to_string_lossy()
            .into_owned(),
    );
    ret
}

/// Create a new xmlXPathObjectPtr of type string and of value @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewCString")]
pub unsafe extern "C" fn xml_xpath_new_cstring(val: *const c_char) -> XmlXPathObjectPtr {
    xml_xpath_new_string(val as _)
}

/// Wraps the @val string into an XPath object.
///
/// Returns the newly created object.
///
/// Frees @val in case of error.
#[doc(alias = "xmlXPathWrapString")]
pub unsafe extern "C" fn xml_xpath_wrap_string(val: *mut XmlChar) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), Some("creating string object\n"));
        xml_free(val as _);
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlXPathObject::default());
    (*ret).typ = XmlXPathObjectType::XPathString;
    (*ret).stringval = (!val.is_null()).then(|| {
        CStr::from_ptr(val as *const i8)
            .to_string_lossy()
            .into_owned()
    });
    xml_free(val as _);
    ret
}

/// Wraps a string into an XPath object.
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathWrapCString")]
pub unsafe extern "C" fn xml_xpath_wrap_cstring(val: *mut c_char) -> XmlXPathObjectPtr {
    xml_xpath_wrap_string(val as *mut XmlChar)
}

/// Create a new xmlXPathObjectPtr of type f64 and of value @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewFloat")]
pub unsafe extern "C" fn xml_xpath_new_float(val: f64) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), Some("creating float object\n"));
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlXPathObject::default());
    (*ret).typ = XmlXPathObjectType::XPathNumber;
    (*ret).floatval = val;
    ret
}

/// Create a new xmlXPathObjectPtr of type boolean and of value @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewBoolean")]
pub unsafe extern "C" fn xml_xpath_new_boolean(val: i32) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), Some("creating boolean object\n"));
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlXPathObject::default());
    (*ret).typ = XmlXPathObjectType::XPathBoolean;
    (*ret).boolval = val != 0;
    ret
}

/// Create a new xmlXPathObjectPtr of type NodeSet and initialize
/// it with the single Node @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewNodeSet")]
pub unsafe extern "C" fn xml_xpath_new_node_set(val: XmlNodePtr) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), Some("creating nodeset\n"));
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlXPathObject::default());
    (*ret).typ = XmlXPathObjectType::XPathNodeset;
    (*ret).boolval = false;
    /* TODO: Check memory error. */
    (*ret).nodesetval = xml_xpath_node_set_create(val);
    /* @@ with_ns to check whether namespace nodes should be looked at @@ */
    ret
}

/// Create a new xmlXPathObjectPtr of type Value Tree (XSLT) and initialize
/// it with the tree root @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewValueTree")]
pub unsafe extern "C" fn xml_xpath_new_value_tree(val: XmlNodePtr) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), Some("creating result value tree\n"));
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlXPathObject::default());
    (*ret).typ = XmlXPathObjectType::XPathXSLTTree;
    (*ret).boolval = true;
    (*ret).user = val as *mut c_void;
    (*ret).nodesetval = xml_xpath_node_set_create(val);
    ret
}

pub const XML_NODESET_DEFAULT: usize = 10;

/// Namespace node in libxml don't match the XPath semantic. In a node set
/// the namespace nodes are duplicated and the next pointer is set to the
/// parent node in the XPath semantic.
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNodeSetDupNs")]
pub unsafe extern "C" fn xml_xpath_node_set_dup_ns(node: XmlNodePtr, ns: XmlNsPtr) -> XmlNodePtr {
    if ns.is_null() || !matches!((*ns).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    if node.is_null() || matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl) {
        return ns as XmlNodePtr;
    }

    /*
     * Allocate a new Namespace and fill the fields.
     */
    let cur: XmlNsPtr = xml_malloc(size_of::<XmlNs>()) as XmlNsPtr;
    if cur.is_null() {
        xml_xpath_err_memory(null_mut(), Some("duplicating namespace\n"));
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNs>());
    (*cur).typ = XmlElementType::XmlNamespaceDecl;
    if !(*ns).href.is_null() {
        (*cur).href = xml_strdup((*ns).href);
    }
    if !(*ns).prefix.is_null() {
        (*cur).prefix = xml_strdup((*ns).prefix);
    }
    (*cur).next = node as XmlNsPtr;
    cur as XmlNodePtr
}

/// Add a new xmlNodePtr to an existing NodeSet
///
/// Returns 0 in case of success, and -1 in case of error
#[doc(alias = "xmlXPathNodeSetAdd")]
pub unsafe extern "C" fn xml_xpath_node_set_add(cur: XmlNodeSetPtr, val: XmlNodePtr) -> i32 {
    if cur.is_null() || val.is_null() {
        return -1;
    }

    // @@ with_ns to check whether namespace nodes should be looked at @@
    // prevent duplicates
    if let Some(table) = (*cur).node_tab.as_ref() {
        for &node in table {
            if node == val {
                return 0;
            }
        }
    }

    // grow the nodeTab if needed
    let table = (*cur).node_tab.get_or_insert_with(Vec::new);
    if table.len() >= XPATH_MAX_NODESET_LENGTH {
        xml_xpath_err_memory(null_mut(), Some("growing nodeset hit limit\n"));
        return -1;
    }
    if matches!((*val).element_type(), XmlElementType::XmlNamespaceDecl) {
        let ns: XmlNsPtr = val as XmlNsPtr;
        let ns_node: XmlNodePtr = xml_xpath_node_set_dup_ns((*ns).next as XmlNodePtr, ns);

        if ns_node.is_null() {
            return -1;
        }
        table.push(ns_node);
    } else {
        table.push(val);
    }
    0
}

/// Add a new xmlNodePtr to an existing NodeSet, optimized version
/// when we are sure the node is not already in the set.
///
/// Returns 0 in case of success and -1 in case of failure
#[doc(alias = "xmlXPathNodeSetAddUnique")]
pub unsafe extern "C" fn xml_xpath_node_set_add_unique(cur: XmlNodeSetPtr, val: XmlNodePtr) -> i32 {
    if cur.is_null() || val.is_null() {
        return -1;
    }

    // @@ with_ns to check whether namespace nodes should be looked at @@
    // grow the nodeTab if needed
    let table = (*cur).node_tab.get_or_insert_with(Vec::new);
    if table.len() >= XPATH_MAX_NODESET_LENGTH {
        xml_xpath_err_memory(null_mut(), Some("growing nodeset hit limit\n"));
        return -1;
    }
    if matches!((*val).element_type(), XmlElementType::XmlNamespaceDecl) {
        let ns: XmlNsPtr = val as XmlNsPtr;
        let ns_node: XmlNodePtr = xml_xpath_node_set_dup_ns((*ns).next as XmlNodePtr, ns);

        if ns_node.is_null() {
            return -1;
        }
        table.push(ns_node);
    } else {
        table.push(val);
    }
    0
}

/// Add a new namespace node to an existing NodeSet
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "xmlXPathNodeSetAddNs")]
pub unsafe extern "C" fn xml_xpath_node_set_add_ns(
    cur: XmlNodeSetPtr,
    node: XmlNodePtr,
    ns: XmlNsPtr,
) -> i32 {
    if cur.is_null()
        || ns.is_null()
        || node.is_null()
        || !matches!((*ns).typ, XmlElementType::XmlNamespaceDecl)
        || !matches!((*node).element_type(), XmlElementType::XmlElementNode)
    {
        return -1;
    }

    // @@ with_ns to check whether namespace nodes should be looked at @@
    // prevent duplicates
    if let Some(table) = (*cur).node_tab.as_ref() {
        for &node in table {
            if node.is_null()
                && matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
                && (*node).next == NodePtr::from_ptr(node)
                && xml_str_equal((*ns).prefix as _, (*(node as XmlNsPtr)).prefix)
            {
                return 0;
            }
        }
    }

    // grow the nodeTab if needed
    let table = (*cur).node_tab.get_or_insert_with(Vec::new);
    if table.len() >= XPATH_MAX_NODESET_LENGTH {
        xml_xpath_err_memory(null_mut(), Some("growing nodeset hit limit\n"));
        return -1;
    }
    let ns_node: XmlNodePtr = xml_xpath_node_set_dup_ns(node, ns);
    if ns_node.is_null() {
        return -1;
    }
    table.push(ns_node);
    0
}

/// Compare two nodes w.r.t document order.
/// This one is optimized for handling of non-element nodes.
///
/// Returns -2 in case of error 1 if first point < second point, 0
/// if it's the same node, -1 otherwise
#[doc(alias = "xmlXPathCmpNodesExt")]
unsafe fn xml_xpath_cmp_nodes_ext(
    mut node1: XmlNodePtr,
    mut node2: XmlNodePtr,
) -> Option<std::cmp::Ordering> {
    let mut depth1: i32;
    let mut depth2: i32;
    let mut misc: i32 = 0;
    let mut precedence1: i32 = 0;
    let mut precedence2: i32 = 0;
    let mut misc_node1: XmlNodePtr = null_mut();
    let mut misc_node2: XmlNodePtr = null_mut();
    let mut cur: XmlNodePtr;

    let mut l1: isize;
    let mut l2: isize;

    if node1.is_null() || node2.is_null() {
        return None;
    }

    if node1 == node2 {
        return Some(std::cmp::Ordering::Equal);
    }

    // a couple of optimizations which will avoid computations in most cases
    'turtle_comparison: {
        match (*node1).element_type() {
            XmlElementType::XmlElementNode => {
                if matches!((*node2).element_type(), XmlElementType::XmlElementNode) {
                    if 0 > (*node1).content as isize
                        && 0 > (*node2).content as isize
                        && (*node1).doc == (*node2).doc
                    {
                        l1 = -((*node1).content as isize);
                        l2 = -((*node2).content as isize);
                        if l1 < l2 {
                            return Some(std::cmp::Ordering::Less);
                        }
                        if l1 > l2 {
                            return Some(std::cmp::Ordering::Greater);
                        }
                    } else {
                        break 'turtle_comparison;
                    }
                }
            }
            XmlElementType::XmlAttributeNode => {
                precedence1 = 1; /* element is owner */
                misc_node1 = node1;
                node1 = (*node1).parent().map_or(null_mut(), |p| p.as_ptr());
                misc = 1;
            }
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlPINode => {
                misc_node1 = node1;
                // Find nearest element node.
                if (*node1).prev.is_some() {
                    loop {
                        node1 = (*node1).prev.map_or(null_mut(), |p| p.as_ptr());
                        if matches!((*node1).element_type(), XmlElementType::XmlElementNode) {
                            precedence1 = 3; /* element in prev-sibl axis */
                            break;
                        }
                        if (*node1).prev.is_none() {
                            precedence1 = 2; /* element is parent */
                            // URGENT TODO: Are there any cases, where the
                            // parent of such a node is not an element node?
                            node1 = (*node1).parent().map_or(null_mut(), |p| p.as_ptr());
                            break;
                        }
                    }
                } else {
                    precedence1 = 2; /* element is parent */
                    node1 = (*node1).parent().map_or(null_mut(), |p| p.as_ptr());
                }
                if node1.is_null()
                    || !matches!((*node1).element_type(), XmlElementType::XmlElementNode)
                    || 0 <= (*node1).content as isize
                {
                    // Fallback for whatever case.
                    node1 = misc_node1;
                    precedence1 = 0;
                } else {
                    misc = 1;
                }
            }
            XmlElementType::XmlNamespaceDecl => {
                // TODO: why do we return 1 for namespace nodes?
                return Some(std::cmp::Ordering::Less);
            }
            _ => {}
        }

        match (*node2).element_type() {
            XmlElementType::XmlElementNode => {}
            XmlElementType::XmlAttributeNode => {
                precedence2 = 1; /* element is owner */
                misc_node2 = node2;
                node2 = (*node2).parent().map_or(null_mut(), |p| p.as_ptr());
                misc = 1;
            }
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlPINode => {
                misc_node2 = node2;
                if (*node2).prev.is_some() {
                    loop {
                        node2 = (*node2).prev.map_or(null_mut(), |p| p.as_ptr());
                        if matches!((*node2).element_type(), XmlElementType::XmlElementNode) {
                            precedence2 = 3; /* element in prev-sibl axis */
                            break;
                        }
                        if (*node2).prev.is_none() {
                            precedence2 = 2; /* element is parent */
                            node2 = (*node2).parent().map_or(null_mut(), |p| p.as_ptr());
                            break;
                        }
                    }
                } else {
                    precedence2 = 2; /* element is parent */
                    node2 = (*node2).parent().map_or(null_mut(), |p| p.as_ptr());
                }
                if node2.is_null()
                    || !matches!((*node2).element_type(), XmlElementType::XmlElementNode)
                    || 0 <= (*node2).content as isize
                {
                    node2 = misc_node2;
                    precedence2 = 0;
                } else {
                    misc = 1;
                }
            }
            XmlElementType::XmlNamespaceDecl => {
                return Some(std::cmp::Ordering::Less);
            }
            _ => {}
        }
        if misc != 0 {
            if node1 == node2 {
                if precedence1 == precedence2 {
                    // The ugly case; but normally there aren't many
                    // adjacent non-element nodes around.
                    cur = (*misc_node2).prev.map_or(null_mut(), |p| p.as_ptr());
                    while !cur.is_null() {
                        if cur == misc_node1 {
                            return Some(std::cmp::Ordering::Less);
                        }
                        if matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
                            return Some(std::cmp::Ordering::Greater);
                        }
                        cur = (*cur).prev.map_or(null_mut(), |p| p.as_ptr());
                    }
                    return Some(std::cmp::Ordering::Greater);
                } else {
                    // Evaluate based on higher precedence wrt to the element.
                    // TODO: This assumes attributes are sorted before content.
                    //   Is this 100% correct?
                    if precedence1 < precedence2 {
                        return Some(std::cmp::Ordering::Less);
                    } else {
                        return Some(std::cmp::Ordering::Greater);
                    }
                }
            }
            // Special case: One of the helper-elements is contained by the other.
            // <foo>
            //   <node2>
            //     <node1>Text-1(precedence1 == 2)</node1>
            //   </node2>
            //   Text-6(precedence2 == 3)
            // </foo>
            if precedence2 == 3 && precedence1 > 1 {
                cur = (*node1).parent().map_or(null_mut(), |p| p.as_ptr());
                while !cur.is_null() {
                    if cur == node2 {
                        return Some(std::cmp::Ordering::Less);
                    }
                    cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                }
            }
            if precedence1 == 3 && precedence2 > 1 {
                cur = (*node2).parent().map_or(null_mut(), |p| p.as_ptr());
                while !cur.is_null() {
                    if cur == node1 {
                        return Some(std::cmp::Ordering::Greater);
                    }
                    cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                }
            }
        }

        // Speedup using document order if available.
        if matches!((*node1).element_type(), XmlElementType::XmlElementNode)
            && matches!((*node2).element_type(), XmlElementType::XmlElementNode)
            && 0 > (*node1).content as isize
            && 0 > (*node2).content as isize
            && (*node1).doc == (*node2).doc
        {
            l1 = -((*node1).content as isize);
            l2 = -((*node2).content as isize);
            if l1 < l2 {
                return Some(std::cmp::Ordering::Less);
            }
            if l1 > l2 {
                return Some(std::cmp::Ordering::Greater);
            }
        }
    }

    // turtle_comparison:

    if NodePtr::from_ptr(node1) == (*node2).prev {
        return Some(std::cmp::Ordering::Less);
    }
    if NodePtr::from_ptr(node1) == (*node2).next {
        return Some(std::cmp::Ordering::Greater);
    }
    // compute depth to root
    depth2 = 0;
    cur = node2;
    while let Some(parent) = (*cur).parent() {
        if parent.as_ptr() == node1 {
            return Some(std::cmp::Ordering::Less);
        }
        depth2 += 1;
        cur = parent.as_ptr();
    }
    let root: XmlNodePtr = cur;
    depth1 = 0;
    cur = node1;
    while let Some(parent) = (*cur).parent() {
        if parent.as_ptr() == node2 {
            return Some(std::cmp::Ordering::Greater);
        }
        depth1 += 1;
        cur = parent.as_ptr();
    }
    // Distinct document (or distinct entities :-( ) case.
    if root != cur {
        return None;
    }
    // get the nearest common ancestor.
    while depth1 > depth2 {
        depth1 -= 1;
        node1 = (*node1).parent().map_or(null_mut(), |p| p.as_ptr());
    }
    while depth2 > depth1 {
        depth2 -= 1;
        node2 = (*node2).parent().map_or(null_mut(), |p| p.as_ptr());
    }
    while (*node1).parent() != (*node2).parent() {
        node1 = (*node1).parent().map_or(null_mut(), |p| p.as_ptr());
        node2 = (*node2).parent().map_or(null_mut(), |p| p.as_ptr());
        /* should not happen but just in case ... */
        if node1.is_null() || node2.is_null() {
            return None;
        }
    }
    // Find who's first.
    if NodePtr::from_ptr(node1) == (*node2).prev {
        return Some(std::cmp::Ordering::Less);
    }
    if NodePtr::from_ptr(node1) == (*node2).next {
        return Some(std::cmp::Ordering::Greater);
    }
    // Speedup using document order if available.
    if matches!((*node1).element_type(), XmlElementType::XmlElementNode)
        && matches!((*node2).element_type(), XmlElementType::XmlElementNode)
        && 0 > (*node1).content as isize
        && 0 > (*node2).content as isize
        && (*node1).doc == (*node2).doc
    {
        l1 = -((*node1).content as isize);
        l2 = -((*node2).content as isize);
        if l1 < l2 {
            return Some(std::cmp::Ordering::Less);
        }
        if l1 > l2 {
            return Some(std::cmp::Ordering::Greater);
        }
    }

    let mut cur = (*node1).next;
    while let Some(now) = cur {
        if Some(now) == NodePtr::from_ptr(node2) {
            return Some(std::cmp::Ordering::Less);
        }
        cur = now.next;
    }
    // assume there is no sibling list corruption
    Some(std::cmp::Ordering::Greater)
}

/// Sort the node set in document order
#[doc(alias = "xmlXPathNodeSetSort")]
pub unsafe extern "C" fn xml_xpath_node_set_sort(set: XmlNodeSetPtr) {
    if set.is_null() {
        return;
    }

    // Use the old Shell's sort implementation to sort the node-set
    // Timsort ought to be quite faster
    if let Some(table) = (*set).node_tab.as_mut() {
        // TODO: Use `sort_unstable` of Rust standard library.
        //       When I tried to rewirte, it did not work fine
        //       because `xml_xpath_cmp_nodes_ext` does not satisfy "total order" constraint.
        let len = table.len();
        let mut incr = len;
        while {
            incr /= 2;
            incr > 0
        } {
            for i in incr..len {
                let mut j = i as i32 - incr as i32;
                while j >= 0 {
                    if xml_xpath_cmp_nodes_ext(table[j as usize], table[j as usize + incr])
                        .map_or(false, |f| f.is_gt())
                    {
                        table.swap(j as usize, j as usize + incr);
                        j -= incr as i32;
                    } else {
                        break;
                    }
                }
            }
        }
    }
}

/// This is the cached version of xmlXPathNewNodeSet().
/// Acquire an xmlXPathObjectPtr of type NodeSet and initialize
/// it with the single Node @val
///
/// Returns the created or reused object.
#[doc(alias = "xmlXPathCacheNewNodeSet")]
unsafe extern "C" fn xml_xpath_cache_new_node_set(
    ctxt: XmlXPathContextPtr,
    val: XmlNodePtr,
) -> XmlXPathObjectPtr {
    if !ctxt.is_null() && !(*ctxt).cache.is_null() {
        let cache: XmlXpathContextCachePtr = (*ctxt).cache as XmlXpathContextCachePtr;

        if !(*cache).nodeset_objs.is_null() && (*(*cache).nodeset_objs).number != 0 {
            // Use the nodeset-cache.
            (*(*cache).nodeset_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).nodeset_objs)
                .items
                .add((*(*cache).nodeset_objs).number as usize)
                as XmlXPathObjectPtr;
            (*ret).typ = XmlXPathObjectType::XPathNodeset;
            (*ret).boolval = false;
            if !val.is_null() {
                if let Some(table) = (*(*ret).nodesetval)
                    .node_tab
                    .as_mut()
                    .filter(|_| !matches!((*val).element_type(), XmlElementType::XmlNamespaceDecl))
                {
                    table.clear();
                    table.push(val);
                } else {
                    // TODO: Check memory error.
                    xml_xpath_node_set_add_unique((*ret).nodesetval, val);
                }
            }
            return ret;
        } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
            // Fallback to misc-cache.

            let set: XmlNodeSetPtr = xml_xpath_node_set_create(val);
            if set.is_null() {
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

/// Initialize the context to the root of the document
#[doc(alias = "xmlXPathRoot")]
pub unsafe extern "C" fn xml_xpath_root(ctxt: XmlXPathParserContextPtr) {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return;
    }
    value_push(
        ctxt,
        xml_xpath_cache_new_node_set((*ctxt).context, (*(*ctxt).context).doc as XmlNodePtr),
    );
}

/// Try to compile the XPath expression as a streamable subset.
///
/// Returns the compiled expression or NULL if failed to compile.
#[doc(alias = "xmlXPathTryStreamCompile")]
#[cfg(feature = "libxml_pattern")]
pub unsafe extern "C" fn xml_xpath_try_stream_compile(
    ctxt: XmlXPathContextPtr,
    str: *const XmlChar,
) -> XmlXPathCompExprPtr {
    // Optimization: use streaming patterns when the XPath expression can
    // be compiled to a stream lookup
    let stream: XmlPatternPtr;
    let comp: XmlXPathCompExprPtr;
    let mut dict: XmlDictPtr = null_mut();

    if xml_strchr(str, b'[').is_null()
        && xml_strchr(str, b'(').is_null()
        && xml_strchr(str, b'@').is_null()
    {
        // We don't try to handle expressions using the verbose axis
        // specifiers ("::"), just the simplified form at this point.
        // Additionally, if there is no list of namespaces available and
        //  there's a ":" in the expression, indicating a prefixed QName,
        //  then we won't try to compile either. xmlPatterncompile() needs
        //  to have a list of namespaces at compilation time in order to
        //  compile prefixed name tests.
        let tmp: *const XmlChar = xml_strchr(str, b':');
        if !tmp.is_null()
            && (ctxt.is_null()
                || (*ctxt).namespaces.as_ref().map_or(0, |t| t.len()) == 0
                || *tmp.add(1) == b':')
        {
            return null_mut();
        }

        let mut namespaces = None;
        if !ctxt.is_null() {
            dict = (*ctxt).dict;
            if let Some(table) = (*ctxt).namespaces.as_deref().filter(|t| !t.is_empty()) {
                let namespaces =
                    namespaces.get_or_insert_with(|| vec![(null(), null()); table.len()]);
                for (i, &ns) in table.iter().enumerate() {
                    namespaces[i] = ((*ns).href, (*ns).prefix);
                }
            }
        }

        stream = xml_patterncompile(
            str,
            dict,
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
            (*comp).dict = dict;
            if !(*comp).dict.is_null() {
                xml_dict_reference((*comp).dict);
            }
            return comp;
        }
        xml_free_pattern(stream);
    }
    null_mut()
}

const XPATH_MAX_RECURSION_DEPTH: usize = 5000;

macro_rules! CUR {
    ($ctxt:expr) => {
        *(*$ctxt).cur
    };
}

macro_rules! NXT {
    ($ctxt:expr, $val:expr) => {
        *(*$ctxt).cur.add($val)
    };
}

macro_rules! NEXT {
    ($ctxt:expr) => {
        if *(*$ctxt).cur != 0 {
            let res = (*$ctxt).cur;
            (*$ctxt).cur = (*$ctxt).cur.add(1);
            res
        } else {
            (*$ctxt).cur
        }
    };
}

macro_rules! SKIP_BLANKS {
    ($ctxt:expr) => {
        while $crate::libxml::chvalid::xml_is_blank_char(*(*$ctxt).cur as u32) {
            NEXT!($ctxt);
        }
    };
}

macro_rules! SKIP {
    ($ctxt:expr, $val:expr) => {
        (*$ctxt).cur = (*$ctxt).cur.add($val);
    };
}

macro_rules! PUSH_BINARY_EXPR {
    ($ctxt:expr, $op:expr, $ch1:expr, $ch2:expr, $val:expr, $val2:expr) => {
        xml_xpath_comp_expr_add(
            $ctxt,
            $ch1,
            $ch2,
            $op,
            $val,
            $val2,
            0,
            null_mut(),
            null_mut(),
        )
    };
}

/// Add a step to an XPath Compiled Expression
///
/// Returns -1 in case of failure, the index otherwise
#[doc(alias = "xmlXPathCompExprAdd")]
unsafe extern "C" fn xml_xpath_comp_expr_add(
    ctxt: XmlXPathParserContextPtr,
    ch1: i32,
    ch2: i32,
    op: XmlXPathOp,
    value: i32,
    value2: i32,
    value3: i32,
    value4: *mut c_void,
    value5: *mut c_void,
) -> i32 {
    let comp: XmlXPathCompExprPtr = (*ctxt).comp;
    if (*comp).nb_step >= (*comp).max_step {
        if (*comp).max_step >= XPATH_MAX_STEPS as i32 {
            xml_xpath_perr_memory(ctxt, Some("adding step\n"));
            return -1;
        }
        (*comp).max_step *= 2;
        let real: *mut XmlXPathStepOp = xml_realloc(
            (*comp).steps as _,
            (*comp).max_step as usize * size_of::<XmlXPathStepOp>(),
        ) as *mut XmlXPathStepOp;
        if real.is_null() {
            (*comp).max_step /= 2;
            xml_xpath_perr_memory(ctxt, Some("adding step\n"));
            return -1;
        }
        (*comp).steps = real;
    }
    (*comp).last = (*comp).nb_step;
    (*(*comp).steps.add((*comp).nb_step as usize)).ch1 = ch1;
    (*(*comp).steps.add((*comp).nb_step as usize)).ch2 = ch2;
    (*(*comp).steps.add((*comp).nb_step as usize)).op = op;
    (*(*comp).steps.add((*comp).nb_step as usize)).value = value;
    (*(*comp).steps.add((*comp).nb_step as usize)).value2 = value2;
    (*(*comp).steps.add((*comp).nb_step as usize)).value3 = value3;
    if !(*comp).dict.is_null()
        && matches!(
            op,
            XmlXPathOp::XpathOpFunction | XmlXPathOp::XpathOpVariable | XmlXPathOp::XpathOpCollect
        )
    {
        if !value4.is_null() {
            (*(*comp).steps.add((*comp).nb_step as usize)).value4 =
                xml_dict_lookup((*comp).dict, value4 as _, -1) as _;
            xml_free(value4 as _);
        } else {
            (*(*comp).steps.add((*comp).nb_step as usize)).value4 = null_mut();
        }
        if !value5.is_null() {
            (*(*comp).steps.add((*comp).nb_step as usize)).value5 =
                xml_dict_lookup((*comp).dict, value5 as _, -1) as _;
            xml_free(value5 as _);
        } else {
            (*(*comp).steps.add((*comp).nb_step as usize)).value5 = null_mut();
        }
    } else {
        (*(*comp).steps.add((*comp).nb_step as usize)).value4 = value4;
        (*(*comp).steps.add((*comp).nb_step as usize)).value5 = value5;
    }
    (*(*comp).steps.add((*comp).nb_step as usize)).cache = None;
    let res = (*comp).nb_step;
    (*comp).nb_step += 1;
    res
}

macro_rules! PUSH_UNARY_EXPR {
    ($ctxt:expr, $op:expr, $ch:expr, $val:expr, $val2:expr) => {
        xml_xpath_comp_expr_add($ctxt, $ch, -1, $op, $val, $val2, 0, null_mut(), null_mut())
    };
}

macro_rules! PUSH_LEAVE_EXPR {
    ($ctxt:expr, $op:expr, $val:expr, $val2:expr) => {
        xml_xpath_comp_expr_add($ctxt, -1, -1, $op, $val, $val2, 0, null_mut(), null_mut())
    };
}

macro_rules! PUSH_LONG_EXPR {
    ($ctxt:expr, $op:expr, $val:expr, $val2:expr, $val3:expr, $val4:expr, $val5:expr) => {
        xml_xpath_comp_expr_add(
            $ctxt,
            (*(*$ctxt).comp).last,
            -1,
            $op,
            $val as i32,
            $val2 as i32,
            $val3 as i32,
            $val4,
            $val5,
        )
    };
}

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

/*
 * Macros for accessing the content. Those should be used only by the parser,
 * and not exported.
 *
 * Dirty macros, i.e. one need to make assumption on the context to use them
 *
 *   CUR_PTR return the current pointer to the xmlChar to be parsed.
 *   CUR     returns the current xmlChar value, i.e. a 8 bit value
 *           in ISO-Latin or UTF-8.
 *           This should be used internally by the parser
 *           only to compare to ASCII values otherwise it would break when
 *           running with UTF-8 encoding.
 *   NXT(n)  returns the n'th next xmlChar. Same as CUR is should be used only
 *           to compare on ASCII based substring.
 *   SKIP(n) Skip n xmlChar, and must also be used only to skip ASCII defined
 *           strings within the parser.
 *   CURRENT Returns the current c_char value, with the full decoding of
 *           UTF-8 if we are using this mode. It returns an int.
 *   NEXT    Skip to the next character, this does the proper decoding
 *           in UTF-8 mode. It also pop-up unfinished entities on the fly.
 *           It returns the pointer to the current xmlChar.
 */

macro_rules! CUR_PTR {
    ($ctxt:expr) => {
        (*$ctxt).cur
    };
}

macro_rules! CUR_CHAR {
    ($ctxt:expr, $l:expr) => {
        xml_xpath_current_char($ctxt, addr_of_mut!($l))
    };
}

macro_rules! NEXTL {
    ($ctxt:expr, $l:expr) => {
        (*$ctxt).cur = (*$ctxt).cur.add($l as usize);
    };
}

/// The current c_char value, if using UTF-8 this may actually span multiple
/// bytes in the input buffer.
///
/// Returns the current c_char value and its length
#[doc(alias = "xmlXPathCurrentChar")]
unsafe extern "C" fn xml_xpath_current_char(ctxt: XmlXPathParserContextPtr, len: *mut i32) -> i32 {
    let mut val: u32;

    if ctxt.is_null() {
        return 0;
    }
    let cur: *const XmlChar = (*ctxt).cur;

    /*
     * We are supposed to handle UTF8, check it's valid
     * From rfc2044: encoding of the Unicode values on UTF-8:
     *
     * UCS-4 range (hex.)           UTF-8 octet sequence (binary)
     * 0000 0000-0000 007F   0xxxxxxx
     * 0000 0080-0000 07FF   110xxxxx 10xxxxxx
     * 0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
     *
     * Check for the 0x110000 limit too
     */
    'encoding: {
        let c: u8 = *cur;
        if c & 0x80 != 0 {
            if *cur.add(1) & 0xc0 != 0x80 {
                break 'encoding;
            }

            if c & 0xe0 == 0xe0 {
                if *cur.add(2) & 0xc0 != 0x80 {
                    break 'encoding;
                }

                if c & 0xf0 == 0xf0 {
                    if c & 0xf8 != 0xf0 || *cur.add(3) & 0xc0 != 0x80 {
                        break 'encoding;
                    } else {
                        /* 4-byte code */
                        *len = 4;
                        val = (*cur.add(0) as u32 & 0x7) << 18;
                        val |= (*cur.add(1) as u32 & 0x3f) << 12;
                        val |= (*cur.add(2) as u32 & 0x3f) << 6;
                        val |= *cur.add(3) as u32 & 0x3f;
                    }
                } else {
                    /* 3-byte code */
                    *len = 3;
                    val = (*cur.add(0) as u32 & 0xf) << 12;
                    val |= (*cur.add(1) as u32 & 0x3f) << 6;
                    val |= *cur.add(2) as u32 & 0x3f;
                }
            } else {
                /* 2-byte code */
                *len = 2;
                val = (*cur.add(0) as u32 & 0x1f) << 6;
                val |= *cur.add(1) as u32 & 0x3f;
            }

            if !xml_is_char(val) {
                XP_ERROR0!(ctxt, XmlXPathError::XpathInvalidCharError as i32);
            }
            return val as _;
        } else {
            /* 1-byte code */
            *len = 1;
            return *cur as _;
        }
    }
    //  encoding_error:
    /*
     * If we detect an UTF8 error that probably means that the
     * input encoding didn't get properly advertised in the
     * declaration header. Report the error and switch the encoding
     * to ISO-Latin-1 (if you don't like this policy, just declare the
     * encoding !)
     */
    *len = 0;
    XP_ERROR0!(ctxt, XmlXPathError::XpathEncodingError as i32);
}

/// Trickery: parse an XML name but without consuming the input flow
/// Needed to avoid insanity in the parser state.
///
/// `[4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender`
///
/// `[5] Name ::= (Letter | '_' | ':') (NameChar)*`
///
/// `[6] Names ::= Name (S Name)*`
///
/// Returns the Name parsed or NULL
#[doc(alias = "xmlXPathScanName")]
unsafe extern "C" fn xml_xpath_scan_name(ctxt: XmlXPathParserContextPtr) -> *mut XmlChar {
    let mut l: i32 = 0;
    let mut c: i32;

    let cur: *const XmlChar = (*ctxt).cur;

    c = CUR_CHAR!(ctxt, l);
    if c == ' ' as i32
        || c == '>' as i32
        || c == '/' as i32 /* accelerators */
        || (!xml_is_letter(c as u32) && c != '_' as i32 && c != ':' as i32)
    {
        return null_mut();
    }

    while c != ' ' as i32
        && c != '>' as i32
        && c != '/' as i32   /* test bigname.xml */
        && (xml_is_letter(c as u32)
            || xml_is_digit(c as u32)
            || c == '.' as i32
            || c == '-' as i32
            || c == '_' as i32
            || c == ':' as i32
            || xml_is_combining(c as u32)
            || xml_is_extender(c as u32))
    {
        NEXTL!(ctxt, l);
        c = CUR_CHAR!(ctxt, l);
    }
    let ret: *mut XmlChar = xml_strndup(cur, (*ctxt).cur.offset_from(cur) as _);
    (*ctxt).cur = cur;
    ret
}

macro_rules! PUSH_FULL_EXPR {
    ($ctxt:expr, $op:expr, $op1:expr, $op2:expr, $val:expr, $val2:expr, $val3:expr, $val4:expr, $val5:expr) => {
        xml_xpath_comp_expr_add(
            $ctxt,
            $op1,
            $op2,
            $op,
            $val as i32,
            $val2 as i32,
            $val3 as i32,
            $val4,
            $val5,
        )
    };
}

/// ```ignore
/// [6] AxisName ::=   'ancestor'
///                  | 'ancestor-or-self'
///                  | 'attribute'
///                  | 'child'
///                  | 'descendant'
///                  | 'descendant-or-self'
///                  | 'following'
///                  | 'following-sibling'
///                  | 'namespace'
///                  | 'parent'
///                  | 'preceding'
///                  | 'preceding-sibling'
///                  | 'self'
/// ```
///
/// Returns the axis or 0
#[doc(alias = "xmlXPathIsAxisName")]
unsafe fn xml_xpath_is_axis_name(name: *const XmlChar) -> Option<XmlXPathAxisVal> {
    let mut ret: Option<XmlXPathAxisVal> = None;
    match *name.add(0) {
        b'a' => {
            if xml_str_equal(name, c"ancestor".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisAncestor);
            }
            if xml_str_equal(name, c"ancestor-or-self".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisAncestorOrSelf);
            }
            if xml_str_equal(name, c"attribute".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisAttribute);
            }
        }
        b'c' => {
            if xml_str_equal(name, c"child".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisChild);
            }
        }
        b'd' => {
            if xml_str_equal(name, c"descendant".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisDescendant);
            }
            if xml_str_equal(name, c"descendant-or-self".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisDescendantOrSelf);
            }
        }
        b'f' => {
            if xml_str_equal(name, c"following".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisFollowing);
            }
            if xml_str_equal(name, c"following-sibling".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisFollowingSibling);
            }
        }
        b'n' => {
            if xml_str_equal(name, c"namespace".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisNamespace);
            }
        }
        b'p' => {
            if xml_str_equal(name, c"parent".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisParent);
            }
            if xml_str_equal(name, c"preceding".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisPreceding);
            }
            if xml_str_equal(name, c"preceding-sibling".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisPrecedingSibling);
            }
        }
        b's' => {
            if xml_str_equal(name, c"self".as_ptr() as _) {
                ret = Some(XmlXPathAxisVal::AxisSelf);
            }
        }
        _ => {}
    }
    ret
}

/// Parse a Literal
///
/// `[29]   Literal ::=   '"' [^"]* '"' | "'" [^']* "'"`
///
/// Returns the value found or NULL in case of error
#[doc(alias = "xmlXPathParseLiteral")]
unsafe extern "C" fn xml_xpath_parse_literal(ctxt: XmlXPathParserContextPtr) -> *mut XmlChar {
    let q: *const XmlChar;
    let ret: *mut XmlChar;

    if CUR!(ctxt) == b'"' {
        NEXT!(ctxt);
        q = CUR_PTR!(ctxt);
        while xml_is_char(CUR!(ctxt) as u32) && CUR!(ctxt) != b'"' {
            NEXT!(ctxt);
        }
        if !xml_is_char(CUR!(ctxt) as u32) {
            XP_ERRORNULL!(ctxt, XmlXPathError::XpathUnfinishedLiteralError as i32);
        } else {
            ret = xml_strndup(q, CUR_PTR!(ctxt).offset_from(q) as _);
            NEXT!(ctxt);
        }
    } else if CUR!(ctxt) == b'\'' {
        NEXT!(ctxt);
        q = CUR_PTR!(ctxt);
        while xml_is_char(CUR!(ctxt) as u32) && CUR!(ctxt) != b'\'' {
            NEXT!(ctxt);
        }
        if !xml_is_char(CUR!(ctxt) as u32) {
            XP_ERRORNULL!(ctxt, XmlXPathError::XpathUnfinishedLiteralError as i32);
        } else {
            ret = xml_strndup(q, CUR_PTR!(ctxt).offset_from(q) as _);
            NEXT!(ctxt);
        }
    } else {
        XP_ERRORNULL!(ctxt, XmlXPathError::XpathStartLiteralError as i32);
    }
    ret
}

/// ```ignore
/// [7] NodeTest ::=   NameTest
///            | NodeType '(' ')'
///            | 'processing-instruction' '(' Literal ')'
///
/// [37] NameTest ::=  '*'
///            | NCName ':' '*'
///            | QName
/// [38] NodeType ::= 'comment'
///           | 'text'
///           | 'processing-instruction'
///           | 'node'
/// ```
///
/// Returns the name found and updates @test, @type and @prefix appropriately
#[doc(alias = "xmlXPathCompNodeTest")]
unsafe extern "C" fn xml_xpath_comp_node_test(
    ctxt: XmlXPathParserContextPtr,
    test: *mut XmlXPathTestVal,
    typ: *mut XmlXPathTypeVal,
    prefix: *mut *mut XmlChar,
    mut name: *mut XmlChar,
) -> *mut XmlChar {
    if test.is_null() || typ.is_null() || prefix.is_null() {
        generic_error!("Internal error at {}:{}\n", file!(), line!());
        return null_mut();
    }
    *typ = XmlXPathTypeVal::NodeTypeNode;
    *test = XmlXPathTestVal::NodeTestNone;
    *prefix = null_mut();
    SKIP_BLANKS!(ctxt);

    if name.is_null() && CUR!(ctxt) == b'*' {
        /*
         * All elements
         */
        NEXT!(ctxt);
        *test = XmlXPathTestVal::NodeTestAll;
        return null_mut();
    }

    if name.is_null() {
        name = xml_xpath_parse_ncname(ctxt);
    }
    if name.is_null() {
        XP_ERRORNULL!(ctxt, XmlXPathError::XpathExprError as i32);
    }

    let blanks: i32 = xml_is_blank_char(CUR!(ctxt) as u32) as i32;
    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) == b'(' {
        NEXT!(ctxt);
        /*
         * NodeType or PI search
         */
        if xml_str_equal(name, c"comment".as_ptr() as _) {
            *typ = XmlXPathTypeVal::NodeTypeComment;
        } else if xml_str_equal(name, c"node".as_ptr() as _) {
            *typ = XmlXPathTypeVal::NodeTypeNode;
        } else if xml_str_equal(name, c"processing-instruction".as_ptr() as _) {
            *typ = XmlXPathTypeVal::NodeTypePI;
        } else if xml_str_equal(name, c"text".as_ptr() as _) {
            *typ = XmlXPathTypeVal::NodeTypeText;
        } else {
            if !name.is_null() {
                xml_free(name as _);
            }
            XP_ERRORNULL!(ctxt, XmlXPathError::XpathExprError as i32);
        }

        *test = XmlXPathTestVal::NodeTestType;

        SKIP_BLANKS!(ctxt);
        if matches!(*typ, XmlXPathTypeVal::NodeTypePI) {
            /*
             * Specific case: search a PI by name.
             */
            if !name.is_null() {
                xml_free(name as _);
            }
            name = null_mut();
            if CUR!(ctxt) != b')' {
                name = xml_xpath_parse_literal(ctxt);
                if name.is_null() {
                    XP_ERRORNULL!(ctxt, XmlXPathError::XpathExprError as i32);
                }
                *test = XmlXPathTestVal::NodeTestPI;
                SKIP_BLANKS!(ctxt);
            }
        }
        if CUR!(ctxt) != b')' {
            if !name.is_null() {
                xml_free(name as _);
            }
            XP_ERRORNULL!(ctxt, XmlXPathError::XpathUnclosedError as i32);
        }
        NEXT!(ctxt);
        return name;
    }
    *test = XmlXPathTestVal::NodeTestName;
    if blanks == 0 && CUR!(ctxt) == b':' {
        NEXT!(ctxt);

        /*
         * Since currently the parser context don't have a
         * namespace list associated:
         * The namespace name for this prefix can be computed
         * only at evaluation time. The compilation is done
         * outside of any context.
         */
        // #if 0
        // 	*prefix = xmlXPathNsLookup((*ctxt).context, name);
        // 	if (name != NULL) {
        // 	    xmlFree(name as _);
        // 	}
        // 	if (*prefix.is_null()) {
        // 	    XP_ERROR0!(ctxt, XmlXPathError::XPATH_UNDEF_PREFIX_ERROR as i32);
        // 	}
        // #else
        *prefix = name;
        // #endif

        if CUR!(ctxt) == b'*' {
            /*
             * All elements
             */
            NEXT!(ctxt);
            *test = XmlXPathTestVal::NodeTestAll;
            return null_mut();
        }

        name = xml_xpath_parse_ncname(ctxt);
        if name.is_null() {
            XP_ERRORNULL!(ctxt, XmlXPathError::XpathExprError as i32);
        }
    }
    name
}

/// ```ignore
/// [8]   Predicate ::=   '[' PredicateExpr ']'
/// [9]   PredicateExpr ::=   Expr
/// ```
///
/// Compile a predicate expression
#[doc(alias = "xmlXPathCompPredicate")]
unsafe extern "C" fn xml_xpath_comp_predicate(ctxt: XmlXPathParserContextPtr, filter: i32) {
    let op1: i32 = (*(*ctxt).comp).last;

    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) != b'[' {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidPredicateError as i32);
    }
    NEXT!(ctxt);
    SKIP_BLANKS!(ctxt);

    (*(*ctxt).comp).last = -1;
    /*
     * This call to xmlXPathCompileExpr() will deactivate sorting
     * of the predicate result.
     * TODO: Sorting is still activated for filters, since I'm not
     *  sure if needed. Normally sorting should not be needed, since
     *  a filter can only diminish the number of items in a sequence,
     *  but won't change its order; so if the initial sequence is sorted,
     *  subsequent sorting is not needed.
     */
    if filter == 0 {
        xml_xpath_compile_expr(ctxt, 0);
    } else {
        xml_xpath_compile_expr(ctxt, 1);
    }
    CHECK_ERROR!(ctxt);

    if CUR!(ctxt) != b']' {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidPredicateError as i32);
    }

    if filter != 0 {
        PUSH_BINARY_EXPR!(
            ctxt,
            XmlXPathOp::XpathOpFilter,
            op1,
            (*(*ctxt).comp).last,
            0,
            0
        );
    } else {
        PUSH_BINARY_EXPR!(
            ctxt,
            XmlXPathOp::XpathOpPredicate,
            op1,
            (*(*ctxt).comp).last,
            0,
            0
        );
    }

    NEXT!(ctxt);
    SKIP_BLANKS!(ctxt);
}

/// ```ignore
/// [4] Step ::=   AxisSpecifier NodeTest Predicate* | AbbreviatedStep
///
/// [12] AbbreviatedStep ::=   '.' | '..'
///
/// [5] AxisSpecifier ::= AxisName '::' | AbbreviatedAxisSpecifier
///
/// [13] AbbreviatedAxisSpecifier ::= '@'?
///
/// Modified for XPtr range support as:
///
///  [4xptr] Step ::= AxisSpecifier NodeTest Predicate* | AbbreviatedStep
///                     | 'range-to' '(' Expr ')' Predicate*
/// ```
///
/// Compile one step in a Location Path
/// A location step of . is short for self::node(). This is
/// particularly useful in conjunction with //. For example, the
/// location path .//para is short for
/// self::node()/descendant-or-self::node()/child::para
/// and so will select all para descendant elements of the context node.
/// Similarly, a location step of .. is short for parent::node().
/// For example, ../title is short for parent::node()/child::title
/// and so will select the title children of the parent of the context node.
#[doc(alias = "xmlXPathCompStep")]
unsafe extern "C" fn xml_xpath_comp_step(ctxt: XmlXPathParserContextPtr) {
    #[cfg(feature = "libxml_xptr_locs")]
    let mut rangeto: i32 = 0;
    #[cfg(feature = "libxml_xptr_locs")]
    let mut op2: i32 = -1;

    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) == b'.' && NXT!(ctxt, 1) == b'.' {
        SKIP!(ctxt, 2);
        SKIP_BLANKS!(ctxt);
        PUSH_LONG_EXPR!(
            ctxt,
            XmlXPathOp::XpathOpCollect,
            XmlXPathAxisVal::AxisParent,
            XmlXPathTestVal::NodeTestType,
            XmlXPathTypeVal::NodeTypeNode,
            null_mut(),
            null_mut()
        );
    } else if CUR!(ctxt) == b'.' {
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
    } else {
        let mut name: *mut XmlChar = null_mut();
        let mut prefix: *mut XmlChar = null_mut();
        let mut test: XmlXPathTestVal = XmlXPathTestVal::NodeTestNone;
        let mut axis: Option<XmlXPathAxisVal>;
        let mut typ: XmlXPathTypeVal = XmlXPathTypeVal::NodeTypeNode;

        /*
         * The modification needed for XPointer change to the production
         */
        #[cfg_attr(not(feature = "libxml_xptr_locs"), allow(unused_labels))]
        'eval_predicates: {
            #[cfg(feature = "libxml_xptr_locs")]
            if (*ctxt).xptr != 0 {
                name = xml_xpath_parse_ncname(ctxt);
                if !name.is_null() && xml_str_equal(name, c"range-to".as_ptr() as _) != 0 {
                    op2 = (*(*ctxt).comp).last;
                    xml_free(name as _);
                    SKIP_BLANKS!(ctxt);
                    if CUR!(ctxt) != b'(' {
                        XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
                    }
                    NEXT!(ctxt);
                    SKIP_BLANKS!(ctxt);

                    xml_xpath_compile_expr(ctxt, 1);
                    /* PUSH_BINARY_EXPR(XPATH_OP_RANGETO, op2, (*(*ctxt).comp).last, 0, 0); */
                    CHECK_ERROR!(ctxt);

                    SKIP_BLANKS!(ctxt);
                    if CUR!(ctxt) != b')' {
                        XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
                    }
                    NEXT!(ctxt);
                    rangeto = 1;
                    break 'eval_predicates;
                }
            }

            if CUR!(ctxt) == b'*' {
                axis = Some(XmlXPathAxisVal::AxisChild);
            } else {
                if name.is_null() {
                    name = xml_xpath_parse_ncname(ctxt);
                }
                if !name.is_null() {
                    axis = xml_xpath_is_axis_name(name);
                    if axis.is_some() {
                        SKIP_BLANKS!(ctxt);
                        if CUR!(ctxt) == b':' && NXT!(ctxt, 1) == b':' {
                            SKIP!(ctxt, 2);
                            xml_free(name as _);
                            name = null_mut();
                        } else {
                            /* an element name can conflict with an axis one :-\ */
                            axis = Some(XmlXPathAxisVal::AxisChild);
                        }
                    } else {
                        axis = Some(XmlXPathAxisVal::AxisChild);
                    }
                } else if CUR!(ctxt) == b'@' {
                    NEXT!(ctxt);
                    axis = Some(XmlXPathAxisVal::AxisAttribute);
                } else {
                    axis = Some(XmlXPathAxisVal::AxisChild);
                }
            }

            if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                xml_free(name as _);
                return;
            }

            name = xml_xpath_comp_node_test(
                ctxt,
                addr_of_mut!(test),
                addr_of_mut!(typ),
                addr_of_mut!(prefix),
                name,
            );
            if matches!(test, XmlXPathTestVal::NodeTestNone) {
                return;
            }

            if (!prefix.is_null()
                && !(*ctxt).context.is_null()
                && (*(*ctxt).context).flags & XML_XPATH_CHECKNS as i32 != 0)
                && xml_xpath_ns_lookup((*ctxt).context, prefix).is_null()
            {
                xml_xpath_err(ctxt, XmlXPathError::XpathUndefPrefixError as i32);
            }
        }

        let op1: i32 = (*(*ctxt).comp).last;
        (*(*ctxt).comp).last = -1;

        SKIP_BLANKS!(ctxt);
        #[allow(clippy::while_immutable_condition)]
        while CUR!(ctxt) == b'[' {
            xml_xpath_comp_predicate(ctxt, 0);
        }

        #[cfg(feature = "libxml_xptr_locs")]
        if rangeto != 0 {
            PUSH_BINARY_EXPR!(ctxt, XmlXPathOp::XpathOpRangeto, op2, op1, 0, 0);
            return;
        }

        if PUSH_FULL_EXPR!(
            ctxt,
            XmlXPathOp::XpathOpCollect,
            op1,
            (*(*ctxt).comp).last,
            axis.unwrap_or_else(|| panic!(
                "Invalid xmlXPathAxisVal: file: {}, line: {}",
                file!(),
                line!()
            )),
            test,
            typ,
            prefix as _,
            name as _
        ) == -1
        {
            xml_free(prefix as _);
            xml_free(name as _);
        }
    }
}

/// ```ignore
/// [3]   RelativeLocationPath ::=   Step
///                     | RelativeLocationPath '/' Step
///                     | AbbreviatedRelativeLocationPath
/// [11]  AbbreviatedRelativeLocationPath ::=   RelativeLocationPath '//' Step
/// ```
///
/// Compile a relative location path.
#[doc(alias = "xmlXPathCompRelativeLocationPath")]
unsafe extern "C" fn xml_xpath_comp_relative_location_path(ctxt: XmlXPathParserContextPtr) {
    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) == b'/' && NXT!(ctxt, 1) == b'/' {
        SKIP!(ctxt, 2);
        SKIP_BLANKS!(ctxt);
        PUSH_LONG_EXPR!(
            ctxt,
            XmlXPathOp::XpathOpCollect,
            XmlXPathAxisVal::AxisDescendantOrSelf,
            XmlXPathTestVal::NodeTestType,
            XmlXPathTypeVal::NodeTypeNode,
            null_mut(),
            null_mut()
        );
    } else if CUR!(ctxt) == b'/' {
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
    }
    xml_xpath_comp_step(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'/' {
        if CUR!(ctxt) == b'/' && NXT!(ctxt, 1) == b'/' {
            SKIP!(ctxt, 2);
            SKIP_BLANKS!(ctxt);
            PUSH_LONG_EXPR!(
                ctxt,
                XmlXPathOp::XpathOpCollect,
                XmlXPathAxisVal::AxisDescendantOrSelf,
                XmlXPathTestVal::NodeTestType,
                XmlXPathTypeVal::NodeTypeNode,
                null_mut(),
                null_mut()
            );
            xml_xpath_comp_step(ctxt);
        } else if CUR!(ctxt) == b'/' {
            NEXT!(ctxt);
            SKIP_BLANKS!(ctxt);
            xml_xpath_comp_step(ctxt);
        }
        SKIP_BLANKS!(ctxt);
    }
}

/// ```ignore
/// [1]   LocationPath ::=   RelativeLocationPath
///                    | AbsoluteLocationPath
/// [2]   AbsoluteLocationPath ::=   '/' RelativeLocationPath?
///                    | AbbreviatedAbsoluteLocationPath
/// [10]   AbbreviatedAbsoluteLocationPath ::=
///                          '//' RelativeLocationPath
/// ```
///
/// Compile a location path
///
/// // is short for /descendant-or-self::node()/. For example,
/// //para is short for /descendant-or-self::node()/child::para and
/// so will select any para element in the document (even a para element
/// that is a document element will be selected by //para since the
/// document element node is a child of the root node); div//para is
/// short for div/descendant-or-self::node()/child::para and so will
/// select all para descendants of div children.
#[doc(alias = "xmlXPathCompLocationPath")]
unsafe extern "C" fn xml_xpath_comp_location_path(ctxt: XmlXPathParserContextPtr) {
    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) != b'/' {
        xml_xpath_comp_relative_location_path(ctxt);
    } else {
        while CUR!(ctxt) == b'/' {
            if CUR!(ctxt) == b'/' && NXT!(ctxt, 1) == b'/' {
                SKIP!(ctxt, 2);
                SKIP_BLANKS!(ctxt);
                PUSH_LONG_EXPR!(
                    ctxt,
                    XmlXPathOp::XpathOpCollect,
                    XmlXPathAxisVal::AxisDescendantOrSelf,
                    XmlXPathTestVal::NodeTestType,
                    XmlXPathTypeVal::NodeTypeNode,
                    null_mut(),
                    null_mut()
                );
                xml_xpath_comp_relative_location_path(ctxt);
            } else if CUR!(ctxt) == b'/' {
                NEXT!(ctxt);
                SKIP_BLANKS!(ctxt);
                if CUR!(ctxt) != 0
                    && (CUR!(ctxt).is_ascii_alphabetic()
                        || CUR!(ctxt) == b'_'
                        || CUR!(ctxt) == b'.'
                        || CUR!(ctxt) == b'@'
                        || (CUR!(ctxt) == b'*'))
                {
                    xml_xpath_comp_relative_location_path(ctxt);
                }
            }
            CHECK_ERROR!(ctxt);
        }
    }
}

/// parse an XML qualified name
///
/// ```ignore
/// [NS 5] QName ::= (Prefix ':')? LocalPart
///
/// [NS 6] Prefix ::= NCName
///
/// [NS 7] LocalPart ::= NCName
/// ```
///
/// Returns the function returns the local part, and prefix is updated
/// to get the Prefix if any.
#[doc(alias = "xmlXPathParseQName")]
unsafe extern "C" fn xml_xpath_parse_qname(
    ctxt: XmlXPathParserContextPtr,
    prefix: *mut *mut XmlChar,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar;

    *prefix = null_mut();
    ret = xml_xpath_parse_ncname(ctxt);
    if !ret.is_null() && CUR!(ctxt) == b':' {
        *prefix = ret;
        NEXT!(ctxt);
        ret = xml_xpath_parse_ncname(ctxt);
    }
    ret
}

/// Parse a VariableReference, evaluate it and push it on the stack.
///
/// The variable bindings consist of a mapping from variable names
/// to variable values. The value of a variable is an object, which can be
/// of any of the types that are possible for the value of an expression,
/// and may also be of additional types not specified here.
///
/// Early evaluation is possible since:
/// The variable bindings [...] used to evaluate a subexpression are
/// always the same as those used to evaluate the containing expression.
///
/// `[36]   VariableReference ::=   '$' QName`
#[doc(alias = "xmlXPathCompVariableReference")]
unsafe extern "C" fn xml_xpath_comp_variable_reference(ctxt: XmlXPathParserContextPtr) {
    let mut prefix: *mut XmlChar = null_mut();

    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) != b'$' {
        XP_ERROR!(ctxt, XmlXPathError::XpathVariableRefError as i32);
    }
    NEXT!(ctxt);
    let name: *mut XmlChar = xml_xpath_parse_qname(ctxt, addr_of_mut!(prefix));
    if name.is_null() {
        xml_free(prefix as _);
        XP_ERROR!(ctxt, XmlXPathError::XpathVariableRefError as i32);
    }
    (*(*ctxt).comp).last = -1;
    if PUSH_LONG_EXPR!(
        ctxt,
        XmlXPathOp::XpathOpVariable,
        0,
        0,
        0,
        name as _,
        prefix as _
    ) == -1
    {
        xml_free(prefix as _);
        xml_free(name as _);
    }
    SKIP_BLANKS!(ctxt);
    if !(*ctxt).context.is_null() && (*(*ctxt).context).flags & XML_XPATH_NOVAR as i32 != 0 {
        XP_ERROR!(ctxt, XmlXPathError::XpathForbidVariableError as i32);
    }
}

/// ```ignore
/// [30]   Number ::=   Digits ('.' Digits?)?
///                   | '.' Digits
/// [31]   Digits ::=   [0-9]+
/// ```
///
/// Compile a Number, then push it on the stack
#[doc(alias = "xmlXPathCompNumber")]
unsafe extern "C" fn xml_xpath_comp_number(ctxt: XmlXPathParserContextPtr) {
    let mut ret: f64;
    let mut ok: i32 = 0;
    let mut exponent: i32 = 0;
    let mut is_exponent_negative: i32 = 0;

    CHECK_ERROR!(ctxt);
    if CUR!(ctxt) != b'.' && (CUR!(ctxt) < b'0' || CUR!(ctxt) > b'9') {
        XP_ERROR!(ctxt, XmlXPathError::XpathNumberError as i32);
    }
    ret = 0.0;
    while CUR!(ctxt) >= b'0' && CUR!(ctxt) <= b'9' {
        ret = ret * 10.0 + (CUR!(ctxt) - b'0') as f64;
        ok = 1;
        NEXT!(ctxt);
    }
    if CUR!(ctxt) == b'.' {
        let mut v: i32;
        let mut frac: i32 = 0;
        let mut fraction: f64 = 0.0;

        NEXT!(ctxt);
        if (CUR!(ctxt) < b'0' || CUR!(ctxt) > b'9') && ok == 0 {
            XP_ERROR!(ctxt, XmlXPathError::XpathNumberError as i32);
        }
        while CUR!(ctxt) == b'0' {
            frac += 1;
            NEXT!(ctxt);
        }
        let max: i32 = frac + MAX_FRAC as i32;
        while CUR!(ctxt) >= b'0' && CUR!(ctxt) <= b'9' && frac < max {
            v = (CUR!(ctxt) - b'0') as i32;
            fraction = fraction * 10.0 + v as f64;
            frac += 1;
            NEXT!(ctxt);
        }
        fraction /= 10.0f64.powi(frac);
        ret += fraction;
        while CUR!(ctxt) >= b'0' && CUR!(ctxt) <= b'9' {
            NEXT!(ctxt);
        }
    }
    if CUR!(ctxt) == b'e' || CUR!(ctxt) == b'E' {
        NEXT!(ctxt);
        if CUR!(ctxt) == b'-' {
            is_exponent_negative = 1;
            NEXT!(ctxt);
        } else if CUR!(ctxt) == b'+' {
            NEXT!(ctxt);
        }
        while CUR!(ctxt) >= b'0' && CUR!(ctxt) <= b'9' {
            if exponent < 1000000 {
                exponent = exponent * 10 + (CUR!(ctxt) - b'0') as i32;
            }
            NEXT!(ctxt);
        }
        if is_exponent_negative != 0 {
            exponent = -exponent;
        }
        ret *= 10.0f64.powi(exponent);
    }
    let num: XmlXPathObjectPtr = xml_xpath_cache_new_float((*ctxt).context, ret);
    if num.is_null() {
        (*ctxt).error = XmlXPathError::XpathMemoryError as i32;
    } else if PUSH_LONG_EXPR!(
        ctxt,
        XmlXPathOp::XpathOpValue,
        XmlXPathObjectType::XPathNumber as i32,
        0,
        0,
        num as _,
        null_mut()
    ) == -1
    {
        xml_xpath_release_object((*ctxt).context, num);
    }
}

/// Parse a Literal and push it on the stack.
///
/// `[29]   Literal ::=   '"' [^"]* '"' | "'" [^']* "'"`
///
/// TODO: xmlXPathCompLiteral memory allocation could be improved.
#[doc(alias = "xmlXPathCompLiteral")]
unsafe extern "C" fn xml_xpath_comp_literal(ctxt: XmlXPathParserContextPtr) {
    let q: *const XmlChar;
    let ret: *mut XmlChar;

    if CUR!(ctxt) == b'"' {
        NEXT!(ctxt);
        q = CUR_PTR!(ctxt);
        while xml_is_char(CUR!(ctxt) as u32) && CUR!(ctxt) != b'"' {
            NEXT!(ctxt);
        }
        if !xml_is_char(CUR!(ctxt) as u32) {
            XP_ERROR!(ctxt, XmlXPathError::XpathUnfinishedLiteralError as i32);
        } else {
            ret = xml_strndup(q, CUR_PTR!(ctxt).offset_from(q) as _);
            NEXT!(ctxt);
        }
    } else if CUR!(ctxt) == b'\'' {
        NEXT!(ctxt);
        q = CUR_PTR!(ctxt);
        while xml_is_char(CUR!(ctxt) as u32) && CUR!(ctxt) != b'\'' {
            NEXT!(ctxt);
        }
        if !xml_is_char(CUR!(ctxt) as u32) {
            XP_ERROR!(ctxt, XmlXPathError::XpathUnfinishedLiteralError as i32);
        } else {
            ret = xml_strndup(q, CUR_PTR!(ctxt).offset_from(q) as _);
            NEXT!(ctxt);
        }
    } else {
        XP_ERROR!(ctxt, XmlXPathError::XpathStartLiteralError as i32);
    }
    if ret.is_null() {
        xml_xpath_perr_memory(ctxt, None);
        return;
    }
    let lit: XmlXPathObjectPtr = xml_xpath_cache_new_string((*ctxt).context, ret);
    if lit.is_null() {
        (*ctxt).error = XmlXPathError::XpathMemoryError as i32;
    } else if PUSH_LONG_EXPR!(
        ctxt,
        XmlXPathOp::XpathOpValue,
        XmlXPathObjectType::XPathString,
        0,
        0,
        lit as _,
        null_mut()
    ) == -1
    {
        xml_xpath_release_object((*ctxt).context, lit);
    }
    xml_free(ret as _);
}

/// ```ignore
/// [16]   FunctionCall ::=   FunctionName '(' ( Argument ( ',' Argument)*)? ')'
/// [17]   Argument ::=   Expr
/// ```
///
/// Compile a function call, the evaluation of all arguments are
/// pushed on the stack
#[doc(alias = "xmlXPathCompFunctionCall")]
unsafe extern "C" fn xml_xpath_comp_function_call(ctxt: XmlXPathParserContextPtr) {
    let mut prefix: *mut XmlChar = null_mut();
    let mut nbargs: i32 = 0;
    let mut sort: i32 = 1;

    let name: *mut XmlChar = xml_xpath_parse_qname(ctxt, addr_of_mut!(prefix));
    if name.is_null() {
        xml_free(prefix as _);
        XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
    }
    SKIP_BLANKS!(ctxt);

    if CUR!(ctxt) != b'(' {
        xml_free(name as _);
        xml_free(prefix as _);
        XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
    }
    NEXT!(ctxt);
    SKIP_BLANKS!(ctxt);

    /*
     * Optimization for count(): we don't need the node-set to be sorted.
     */
    if prefix.is_null() && *name.add(0) == b'c' && xml_str_equal(name, c"count".as_ptr() as _) {
        sort = 0;
    }
    (*(*ctxt).comp).last = -1;
    if CUR!(ctxt) != b')' {
        while CUR!(ctxt) != 0 {
            let op1: i32 = (*(*ctxt).comp).last;
            (*(*ctxt).comp).last = -1;
            xml_xpath_compile_expr(ctxt, sort);
            if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                xml_free(name as _);
                xml_free(prefix as _);
                return;
            }
            PUSH_BINARY_EXPR!(
                ctxt,
                XmlXPathOp::XpathOpArg,
                op1,
                (*(*ctxt).comp).last,
                0,
                0
            );
            nbargs += 1;
            if CUR!(ctxt) == b')' {
                break;
            }
            if CUR!(ctxt) != b',' {
                xml_free(name as _);
                xml_free(prefix as _);
                XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
            }
            NEXT!(ctxt);
            SKIP_BLANKS!(ctxt);
        }
    }
    if PUSH_LONG_EXPR!(
        ctxt,
        XmlXPathOp::XpathOpFunction,
        nbargs,
        0,
        0,
        name as _,
        prefix as _
    ) == -1
    {
        xml_free(prefix as _);
        xml_free(name as _);
    }
    NEXT!(ctxt);
    SKIP_BLANKS!(ctxt);
}

/// ```ignore
/// [15]   PrimaryExpr ::=   VariableReference
///                | '(' Expr ')'
///                | Literal
///                | Number
///                | FunctionCall
/// ```
///
/// Compile a primary expression.
#[doc(alias = "xmlXPathCompPrimaryExpr")]
unsafe extern "C" fn xml_xpath_comp_primary_expr(ctxt: XmlXPathParserContextPtr) {
    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) == b'$' {
        xml_xpath_comp_variable_reference(ctxt);
    } else if CUR!(ctxt) == b'(' {
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
        xml_xpath_compile_expr(ctxt, 1);
        CHECK_ERROR!(ctxt);
        if CUR!(ctxt) != b')' {
            XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
        }
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
    } else if CUR!(ctxt).is_ascii_digit() || (CUR!(ctxt) == b'.' && NXT!(ctxt, 1).is_ascii_digit())
    {
        xml_xpath_comp_number(ctxt);
    } else if CUR!(ctxt) == b'\'' || CUR!(ctxt) == b'"' {
        xml_xpath_comp_literal(ctxt);
    } else {
        xml_xpath_comp_function_call(ctxt);
    }
    SKIP_BLANKS!(ctxt);
}

/// `[20]   FilterExpr ::=   PrimaryExpr | FilterExpr Predicate`
///
/// Compile a filter expression.
/// Square brackets are used to filter expressions in the same way that
/// they are used in location paths. It is an error if the expression to
/// be filtered does not evaluate to a node-set. The context node list
/// used for evaluating the expression in square brackets is the node-set
/// to be filtered listed in document order.
#[doc(alias = "xmlXPathCompFilterExpr")]
unsafe extern "C" fn xml_xpath_comp_filter_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_primary_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);

    while CUR!(ctxt) == b'[' {
        xml_xpath_comp_predicate(ctxt, 1);
        SKIP_BLANKS!(ctxt);
    }
}

/// ```ignore
/// [19]   PathExpr ::=   LocationPath
///               | FilterExpr
///               | FilterExpr '/' RelativeLocationPath
///               | FilterExpr '//' RelativeLocationPath
/// ```
///
/// Compile a path expression.
/// The / operator and // operators combine an arbitrary expression
/// and a relative location path. It is an error if the expression
/// does not evaluate to a node-set.
/// The / operator does composition in the same way as when / is
/// used in a location path. As in location paths, // is short for
/// /descendant-or-self::node()/.
#[doc(alias = "xmlXPathCompPathExpr")]
unsafe extern "C" fn xml_xpath_comp_path_expr(ctxt: XmlXPathParserContextPtr) {
    let mut lc: i32 = 1; /* Should we branch to LocationPath ?         */
    let name: *mut XmlChar; /* we may have to preparse a name to find out */

    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) == b'$'
        || CUR!(ctxt) == b'('
        || CUR!(ctxt).is_ascii_digit()
        || CUR!(ctxt) == b'\''
        || CUR!(ctxt) == b'"'
        || (CUR!(ctxt) == b'.' && NXT!(ctxt, 1).is_ascii_digit())
    {
        lc = 0;
    } else if CUR!(ctxt) == b'*' || CUR!(ctxt) == b'/' {
        /* relative or absolute location path */
        lc = 1;
    } else if CUR!(ctxt) == b'@' || CUR!(ctxt) == b'.' {
        /* relative abbreviated attribute location path */
        lc = 1;
    } else {
        /*
         * Problem is finding if we have a name here whether it's:
         *   - a nodetype
         *   - a function call in which case it's followed by '('
         *   - an axis in which case it's followed by ':'
         *   - a element name
         * We do an a priori analysis here rather than having to
         * maintain parsed token content through the recursive function
         * calls. This looks uglier but makes the code easier to
         * read/write/debug.
         */
        SKIP_BLANKS!(ctxt);
        name = xml_xpath_scan_name(ctxt);
        if !name.is_null() && !xml_strstr(name, c"::".as_ptr() as _).is_null() {
            lc = 1;
            xml_free(name as _);
        } else if !name.is_null() {
            let mut len: i32 = xml_strlen(name);

            while NXT!(ctxt, len as usize) != 0 {
                if NXT!(ctxt, len as usize) == b'/' {
                    /* element name */
                    lc = 1;
                    break;
                } else if xml_is_blank_char(NXT!(ctxt, len as usize) as u32) {
                    /* ignore blanks */
                } else if NXT!(ctxt, len as usize) == b':' {
                    lc = 1;
                    break;
                } else if NXT!(ctxt, len as usize) == b'(' {
                    /* Node Type or Function */
                    if xml_xpath_is_node_type(name) != 0 {
                        lc = 1;
                    } else {
                        #[cfg(feature = "libxml_xptr_locs")]
                        if (*ctxt).xptr != 0 && xml_str_equal(name, c"range-to".as_ptr() as _) != 0
                        {
                            lc = 1;
                        } else {
                            lc = 0;
                        }
                        #[cfg(not(feature = "libxml_xptr_locs"))]
                        {
                            lc = 0;
                        }
                    }
                    break;
                } else if NXT!(ctxt, len as usize) == b'[' {
                    /* element name */
                    lc = 1;
                    break;
                // } else if NXT!(ctxt, len as usize) == b'<'
                //     || NXT!(ctxt, len as usize) == b'>'
                //     || NXT!(ctxt, len as usize) == b'='
                // {
                //     lc = 1;
                //     break;
                } else {
                    lc = 1;
                    break;
                }
                len += 1;
            }
            if NXT!(ctxt, len as usize) == 0 {
                /* element name */
                lc = 1;
            }
            xml_free(name as _);
        } else {
            /* make sure all cases are covered explicitly */
            XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
        }
    }

    if lc != 0 {
        if CUR!(ctxt) == b'/' {
            PUSH_LEAVE_EXPR!(ctxt, XmlXPathOp::XpathOpRoot, 0, 0);
        } else {
            PUSH_LEAVE_EXPR!(ctxt, XmlXPathOp::XpathOpNode, 0, 0);
        }
        xml_xpath_comp_location_path(ctxt);
    } else {
        xml_xpath_comp_filter_expr(ctxt);
        CHECK_ERROR!(ctxt);
        if CUR!(ctxt) == b'/' && NXT!(ctxt, 1) == b'/' {
            SKIP!(ctxt, 2);
            SKIP_BLANKS!(ctxt);

            PUSH_LONG_EXPR!(
                ctxt,
                XmlXPathOp::XpathOpCollect,
                XmlXPathAxisVal::AxisDescendantOrSelf,
                XmlXPathTestVal::NodeTestType,
                XmlXPathTypeVal::NodeTypeNode,
                null_mut(),
                null_mut()
            );

            xml_xpath_comp_relative_location_path(ctxt);
        } else if CUR!(ctxt) == b'/' {
            xml_xpath_comp_relative_location_path(ctxt);
        }
    }
    SKIP_BLANKS!(ctxt);
}

/// `[18]   UnionExpr ::=   PathExpr | UnionExpr '|' PathExpr`
///
/// Compile an union expression.
#[doc(alias = "xmlXPathCompUnionExpr")]
unsafe extern "C" fn xml_xpath_comp_union_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_path_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'|' {
        let op1: i32 = (*(*ctxt).comp).last;
        PUSH_LEAVE_EXPR!(ctxt, XmlXPathOp::XpathOpNode, 0, 0);

        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
        xml_xpath_comp_path_expr(ctxt);

        PUSH_BINARY_EXPR!(
            ctxt,
            XmlXPathOp::XpathOpUnion,
            op1,
            (*(*ctxt).comp).last,
            0,
            0
        );

        SKIP_BLANKS!(ctxt);
    }
}

/// `[27]   UnaryExpr ::=   UnionExpr | '-' UnaryExpr`
///
/// Compile an unary expression.
#[doc(alias = "xmlXPathCompUnaryExpr")]
unsafe extern "C" fn xml_xpath_comp_unary_expr(ctxt: XmlXPathParserContextPtr) {
    let mut minus: i32 = 0;
    let mut found: i32 = 0;

    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'-' {
        minus = 1 - minus;
        found = 1;
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
    }

    xml_xpath_comp_union_expr(ctxt);
    CHECK_ERROR!(ctxt);
    if found != 0 {
        if minus != 0 {
            PUSH_UNARY_EXPR!(ctxt, XmlXPathOp::XpathOpPlus, (*(*ctxt).comp).last, 2, 0);
        } else {
            PUSH_UNARY_EXPR!(ctxt, XmlXPathOp::XpathOpPlus, (*(*ctxt).comp).last, 3, 0);
        }
    }
}

/// ```ignore
/// [26]   MultiplicativeExpr ::=   UnaryExpr
///                  | MultiplicativeExpr MultiplyOperator UnaryExpr
///                  | MultiplicativeExpr 'div' UnaryExpr
///                  | MultiplicativeExpr 'mod' UnaryExpr
/// [34]   MultiplyOperator ::=   '*'
/// ```
///
/// Compile an Additive expression.
#[doc(alias = "xmlXPathCompMultiplicativeExpr")]
unsafe extern "C" fn xml_xpath_comp_multiplicative_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_unary_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'*'
        || (CUR!(ctxt) == b'd' && NXT!(ctxt, 1) == b'i' && NXT!(ctxt, 2) == b'v')
        || (CUR!(ctxt) == b'm' && NXT!(ctxt, 1) == b'o' && NXT!(ctxt, 2) == b'd')
    {
        let mut op: i32 = -1;
        let op1: i32 = (*(*ctxt).comp).last;

        if CUR!(ctxt) == b'*' {
            op = 0;
            NEXT!(ctxt);
        } else if CUR!(ctxt) == b'd' {
            op = 1;
            SKIP!(ctxt, 3);
        } else if CUR!(ctxt) == b'm' {
            op = 2;
            SKIP!(ctxt, 3);
        }
        SKIP_BLANKS!(ctxt);
        xml_xpath_comp_unary_expr(ctxt);
        CHECK_ERROR!(ctxt);
        PUSH_BINARY_EXPR!(
            ctxt,
            XmlXPathOp::XpathOpMult,
            op1,
            (*(*ctxt).comp).last,
            op,
            0
        );
        SKIP_BLANKS!(ctxt);
    }
}

/// ```ignore
/// [25]   AdditiveExpr ::=   MultiplicativeExpr
///                   | AdditiveExpr '+' MultiplicativeExpr
///                   | AdditiveExpr '-' MultiplicativeExpr
/// ```
///
/// Compile an Additive expression.
#[doc(alias = "xmlXPathCompAdditiveExpr")]
unsafe extern "C" fn xml_xpath_comp_additive_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_multiplicative_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'+' || CUR!(ctxt) == b'-' {
        let op1: i32 = (*(*ctxt).comp).last;
        let plus = (CUR!(ctxt) == b'+') as i32;

        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
        xml_xpath_comp_multiplicative_expr(ctxt);
        CHECK_ERROR!(ctxt);
        PUSH_BINARY_EXPR!(
            ctxt,
            XmlXPathOp::XpathOpPlus,
            op1,
            (*(*ctxt).comp).last,
            plus,
            0
        );
        SKIP_BLANKS!(ctxt);
    }
}

/// ```ignore
/// [24]   RelationalExpr ::=   AdditiveExpr
///                | RelationalExpr '<' AdditiveExpr
///                | RelationalExpr '>' AdditiveExpr
///                | RelationalExpr '<=' AdditiveExpr
///                | RelationalExpr '>=' AdditiveExpr
/// ```
///
///  A <= B > C is allowed ? Answer from James, yes with
///  (AdditiveExpr <= AdditiveExpr) > AdditiveExpr
///  which is basically what got implemented.
///
/// Compile a Relational expression, then push the result on the stack
#[doc(alias = "xmlXPathCompRelationalExpr")]
unsafe extern "C" fn xml_xpath_comp_relational_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_additive_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'<' || CUR!(ctxt) == b'>' {
        let op1: i32 = (*(*ctxt).comp).last;
        let inf = (CUR!(ctxt) == b'<') as i32;
        let strict = (NXT!(ctxt, 1) != b'=') as i32;

        NEXT!(ctxt);
        if strict == 0 {
            NEXT!(ctxt);
        }
        SKIP_BLANKS!(ctxt);
        xml_xpath_comp_additive_expr(ctxt);
        CHECK_ERROR!(ctxt);
        PUSH_BINARY_EXPR!(
            ctxt,
            XmlXPathOp::XpathOpCmp,
            op1,
            (*(*ctxt).comp).last,
            inf,
            strict
        );
        SKIP_BLANKS!(ctxt);
    }
}

/// ```ignore
/// [23]   EqualityExpr ::=   RelationalExpr
///                | EqualityExpr '=' RelationalExpr
///                | EqualityExpr '!=' RelationalExpr
/// ```
///
///  A != B != C is allowed ? Answer from James, yes with
///  (RelationalExpr = RelationalExpr) = RelationalExpr
///  (RelationalExpr != RelationalExpr) != RelationalExpr
///  which is basically what got implemented.
///
/// Compile an Equality expression.
///
#[doc(alias = "xmlXPathCompEqualityExpr")]
unsafe extern "C" fn xml_xpath_comp_equality_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_relational_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'=' || (CUR!(ctxt) == b'!' && NXT!(ctxt, 1) == b'=') {
        let op1: i32 = (*(*ctxt).comp).last;
        let eq = (CUR!(ctxt) == b'=') as i32;

        NEXT!(ctxt);
        if eq == 0 {
            NEXT!(ctxt);
        }
        SKIP_BLANKS!(ctxt);
        xml_xpath_comp_relational_expr(ctxt);
        CHECK_ERROR!(ctxt);
        PUSH_BINARY_EXPR!(
            ctxt,
            XmlXPathOp::XpathOpEqual,
            op1,
            (*(*ctxt).comp).last,
            eq,
            0
        );
        SKIP_BLANKS!(ctxt);
    }
}

/// `[22]   AndExpr ::=   EqualityExpr | AndExpr 'and' EqualityExpr`
///
/// Compile an AND expression.
#[doc(alias = "xmlXPathCompAndExpr")]
unsafe extern "C" fn xml_xpath_comp_and_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_equality_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'a' && NXT!(ctxt, 1) == b'n' && NXT!(ctxt, 2) == b'd' {
        let op1: i32 = (*(*ctxt).comp).last;
        SKIP!(ctxt, 3);
        SKIP_BLANKS!(ctxt);
        xml_xpath_comp_equality_expr(ctxt);
        CHECK_ERROR!(ctxt);
        PUSH_BINARY_EXPR!(
            ctxt,
            XmlXPathOp::XpathOpAnd,
            op1,
            (*(*ctxt).comp).last,
            0,
            0
        );
        SKIP_BLANKS!(ctxt);
    }
}

/// ```ignore
/// [14]   Expr ::=   OrExpr
/// [21]   OrExpr ::=   AndExpr | OrExpr 'or' AndExpr
/// ```
///
/// Parse and compile an expression
#[doc(alias = "xmlXPathCompileExpr")]
pub unsafe extern "C" fn xml_xpath_compile_expr(ctxt: XmlXPathParserContextPtr, sort: i32) {
    let xpctxt: XmlXPathContextPtr = (*ctxt).context;

    if !xpctxt.is_null() {
        if (*xpctxt).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
            XP_ERROR!(ctxt, XmlXPathError::XpathRecursionLimitExceeded as i32);
        }
        /*
         * Parsing a single '(' pushes about 10 functions on the call stack
         * before recursing!
         */
        (*xpctxt).depth += 10;
    }

    xml_xpath_comp_and_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'o' && NXT!(ctxt, 1) == b'r' {
        let op1: i32 = (*(*ctxt).comp).last;
        SKIP!(ctxt, 2);
        SKIP_BLANKS!(ctxt);
        xml_xpath_comp_and_expr(ctxt);
        CHECK_ERROR!(ctxt);
        PUSH_BINARY_EXPR!(ctxt, XmlXPathOp::XpathOpOr, op1, (*(*ctxt).comp).last, 0, 0);
        SKIP_BLANKS!(ctxt);
    }
    if sort != 0
        && !matches!(
            (*(*(*ctxt).comp).steps.add((*(*ctxt).comp).last as usize)).op,
            XmlXPathOp::XpathOpValue
        )
    {
        /* more ops could be optimized too */
        /*
        	* This is the main place to eliminate sorting for
        	* operations which don't require a sorted node-set.
        	* E.g. count().
        	*/
        PUSH_UNARY_EXPR!(ctxt, XmlXPathOp::XpathOpSort, (*(*ctxt).comp).last, 0, 0);
    }

    if !xpctxt.is_null() {
        (*xpctxt).depth -= 10;
    }
}

pub unsafe extern "C" fn xml_xpath_optimize_expression(
    pctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
) {
    let comp: XmlXPathCompExprPtr = (*pctxt).comp;

    /*
     * Try to rewrite "descendant-or-self::node()/foo" to an optimized
     * internal representation.
     */

    if matches!((*op).op, XmlXPathOp::XpathOpCollect /* 11 */) && (*op).ch1 != -1 && (*op).ch2 == -1
    {
        let prevop: XmlXPathStepOpPtr = (*comp).steps.add((*op).ch1 as usize);

        if matches!((*prevop).op, XmlXPathOp::XpathOpCollect /* 11 */)
            && (*prevop).value == XmlXPathAxisVal::AxisDescendantOrSelf as i32
            && (*prevop).ch2 == -1
            && (*prevop).value2 == XmlXPathTestVal::NodeTestType as i32
            && (*prevop).value3 == XmlXPathTypeVal::NodeTypeNode as i32
        {
            /*
             * This is a "descendant-or-self::node()" without predicates.
             * Try to eliminate it.
             */

            if (*op).value == XmlXPathAxisVal::AxisChild as i32
                || (*op).value == XmlXPathAxisVal::AxisDescendant as i32
            {
                /*
                 * Convert "descendant-or-self::node()/child::" or
                 * "descendant-or-self::node()/descendant::" to
                 * "descendant::"
                 */
                (*op).ch1 = (*prevop).ch1;
                (*op).value = XmlXPathAxisVal::AxisDescendant as i32;
            } else if (*op).value == XmlXPathAxisVal::AxisSelf as i32
                || (*op).value == XmlXPathAxisVal::AxisDescendantOrSelf as i32
            {
                /*
                 * Convert "descendant-or-self::node()/self::" or
                 * "descendant-or-self::node()/descendant-or-self::" to
                 * to "descendant-or-self::"
                 */
                (*op).ch1 = (*prevop).ch1;
                (*op).value = XmlXPathAxisVal::AxisDescendantOrSelf as i32;
            }
        }
    }

    /* OP_VALUE has invalid ch1. */
    if matches!((*op).op, XmlXPathOp::XpathOpValue) {
        return;
    }

    /* Recurse */
    let ctxt: XmlXPathContextPtr = (*pctxt).context;
    if !ctxt.is_null() {
        if (*ctxt).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
            return;
        }
        (*ctxt).depth += 1;
    }
    if (*op).ch1 != -1 {
        xml_xpath_optimize_expression(pctxt, (*comp).steps.add((*op).ch1 as usize));
    }
    if (*op).ch2 != -1 {
        xml_xpath_optimize_expression(pctxt, (*comp).steps.add((*op).ch2 as usize));
    }
    if !ctxt.is_null() {
        (*ctxt).depth -= 1;
    }
}

/// Evaluate the Precompiled Streamable XPath expression in the given context.
#[doc(alias = "xmlXPathRunStreamEval")]
#[cfg(feature = "libxml_pattern")]
unsafe extern "C" fn xml_xpath_run_stream_eval(
    ctxt: XmlXPathContextPtr,
    comp: XmlPatternPtr,
    result_seq: *mut XmlXPathObjectPtr,
    to_bool: i32,
) -> i32 {
    let mut max_depth: i32;
    let mut ret: i32;
    let mut depth: i32;
    let mut cur: XmlNodePtr = null_mut();
    let mut limit: XmlNodePtr = null_mut();

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
        *result_seq = xml_xpath_cache_new_node_set(ctxt, null_mut());
        if (*result_seq).is_null() {
            return -1;
        }
    }

    /*
     * handle the special cases of "/" amd "." being matched
     */
    if min_depth == 0 {
        if from_root != 0 {
            /* Select "/" */
            if to_bool != 0 {
                return 1;
            }
            /* TODO: Check memory error. */
            xml_xpath_node_set_add_unique((*(*result_seq)).nodesetval, (*ctxt).doc as XmlNodePtr);
        } else {
            /* Select "self::node()" */
            if to_bool != 0 {
                return 1;
            }
            /* TODO: Check memory error. */
            xml_xpath_node_set_add_unique((*(*result_seq)).nodesetval, (*ctxt).node);
        }
    }
    if max_depth == 0 {
        return 0;
    }

    if from_root != 0 {
        cur = (*ctxt).doc as XmlNodePtr;
    } else if !(*ctxt).node.is_null() {
        match (*(*ctxt).node).element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => {
                cur = (*ctxt).node;
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
        limit = cur;
    }
    if cur.is_null() {
        return 0;
    }

    let patstream: XmlStreamCtxtPtr = xml_pattern_get_stream_ctxt(comp);
    if patstream.is_null() {
        /*
         * QUESTION TODO: Is this an error?
         */
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
            /* TODO: Check memory error. */
            xml_xpath_node_set_add_unique((*(*result_seq)).nodesetval, cur);
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

                    match (*cur).element_type() {
                        XmlElementType::XmlElementNode
                        | XmlElementType::XmlTextNode
                        | XmlElementType::XmlCDATASectionNode
                        | XmlElementType::XmlCommentNode
                        | XmlElementType::XmlPINode => 'to_break: {
                            ret = if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                            {
                                xml_stream_push(
                                    patstream,
                                    (*cur).name,
                                    if !(*cur).ns.is_null() {
                                        (*(*cur).ns).href
                                    } else {
                                        null_mut()
                                    },
                                )
                            } else if eval_all_nodes != 0 {
                                xml_stream_push_node(
                                    patstream,
                                    null(),
                                    null(),
                                    (*cur).element_type() as i32,
                                )
                            } else {
                                break 'to_break;
                            };
                            if ret < 0 {
                                /* NOP. */
                            } else if ret == 1 {
                                if to_bool != 0 {
                                    // goto return_1;
                                    if !patstream.is_null() {
                                        xml_free_stream_ctxt(patstream);
                                    }
                                    return 1;
                                }
                                if xml_xpath_node_set_add_unique((*(*result_seq)).nodesetval, cur)
                                    < 0
                                {
                                    (*ctxt).last_error.domain = XmlErrorDomain::XmlFromXPath;
                                    (*ctxt).last_error.code = XmlParserErrors::XmlErrNoMemory;
                                }
                            }
                            if (*cur).children().is_none() || depth >= max_depth {
                                // ret =
                                xml_stream_pop(patstream);
                                while let Some(next) = (*cur).next {
                                    cur = next.as_ptr();
                                    if !matches!(
                                        (*cur).element_type(),
                                        XmlElementType::XmlEntityDecl | XmlElementType::XmlDTDNode
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
                if let Some(children) = (*cur).children().filter(|_| depth < max_depth) {
                    /*
                     * Do not descend on entities declarations
                     */
                    if !matches!(children.element_type(), XmlElementType::XmlEntityDecl) {
                        cur = children.as_ptr();
                        depth += 1;
                        /*
                         * Skip DTDs
                         */
                        if !matches!((*cur).element_type(), XmlElementType::XmlDTDNode) {
                            break 'to_continue_main;
                        }
                    }
                }

                if cur == limit {
                    break 'main;
                }

                while let Some(next) = (*cur).next {
                    cur = next.as_ptr();
                    if !matches!(
                        (*cur).element_type(),
                        XmlElementType::XmlEntityDecl | XmlElementType::XmlDTDNode
                    ) {
                        // goto next_node;
                        continue 'next_node;
                    }
                }

                break 'next_node;
            }

            'inner: while {
                cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
                depth -= 1;
                if cur.is_null()
                    || cur == limit
                    || matches!((*cur).element_type(), XmlElementType::XmlDocumentNode)
                {
                    // goto done;
                    break 'main;
                }
                if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                    || (eval_all_nodes != 0
                        && matches!(
                            (*cur).element_type(),
                            XmlElementType::XmlTextNode
                                | XmlElementType::XmlCDATASectionNode
                                | XmlElementType::XmlCommentNode
                                | XmlElementType::XmlPINode
                        ))
                {
                    // ret =
                    xml_stream_pop(patstream);
                };
                if let Some(next) = (*cur).next {
                    cur = next.as_ptr();
                    break 'inner;
                }
                !cur.is_null()
            } {}
        }

        !cur.is_null() && depth >= 0
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

/// Adds opCount to the running total of operations and returns -1 if the
/// operation limit is exceeded. Returns 0 otherwise.
#[doc(alias = "xmlXPathCheckOpLimit")]
unsafe extern "C" fn xml_xpath_check_op_limit(
    ctxt: XmlXPathParserContextPtr,
    op_count: u64,
) -> i32 {
    let xpctxt: XmlXPathContextPtr = (*ctxt).context;

    if op_count > (*xpctxt).op_limit || (*xpctxt).op_count > (*xpctxt).op_limit - op_count {
        (*xpctxt).op_count = (*xpctxt).op_limit;
        xml_xpath_err(ctxt, XmlXPathError::XpathOpLimitExceeded as i32);
        return -1;
    }

    (*xpctxt).op_count += op_count;
    0
}

macro_rules! OP_LIMIT_EXCEEDED {
    ($ctxt:expr, $n:expr) => {
        (*(*$ctxt).context).op_limit != 0 && xml_xpath_check_op_limit($ctxt, $n) < 0
    };
}

// Used for merging node sets in xmlXPathCollectAndTest().
#[doc(alias = "xmlXPathNodeSetMergeFunction")]
pub type XmlXPathNodeSetMergeFunction =
    unsafe extern "C" fn(XmlNodeSetPtr, XmlNodeSetPtr) -> XmlNodeSetPtr;

// A traversal function enumerates nodes along an axis.
// Initially it must be called with NULL, and it indicates
// termination on the axis by returning NULL.
pub type XmlXPathTraversalFunction =
    unsafe extern "C" fn(ctxt: XmlXPathParserContextPtr, cur: XmlNodePtr) -> XmlNodePtr;

/// Clears the list from temporary XPath objects (e.g. namespace nodes
/// are feed) starting with the entry at @pos, but does *not* free the list
/// itself. Sets the length of the list to @pos.
#[doc(alias = "xmlXPathNodeSetClearFromPos")]
unsafe extern "C" fn xml_xpath_node_set_clear_from_pos(
    set: XmlNodeSetPtr,
    pos: i32,
    has_ns_nodes: i32,
) {
    if set.is_null() || pos >= (*set).node_tab.as_ref().map_or(0, |t| t.len() as i32) {
        return;
    }
    if has_ns_nodes != 0 {
        if let Some(table) = (*set).node_tab.as_ref() {
            for &node in &table[pos as usize..] {
                if !node.is_null()
                    && matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
                {
                    xml_xpath_node_set_free_ns(node as XmlNsPtr);
                }
            }
        }
    }
    if let Some(table) = (*set).node_tab.as_mut() {
        table.truncate(pos as usize);
    }
}

/// Clears the list from all temporary XPath objects (e.g. namespace nodes
/// are feed), but does *not* free the list itself. Sets the length of the list to 0.
#[doc(alias = "xmlXPathNodeSetClear")]
unsafe extern "C" fn xml_xpath_node_set_clear(set: XmlNodeSetPtr, has_ns_nodes: i32) {
    xml_xpath_node_set_clear_from_pos(set, 0, has_ns_nodes);
}

/// Merges two nodesets, all nodes from @set2 are added to @set1.
/// Checks for duplicate nodes. Clears set2.
///
/// Returns @set1 once extended or NULL in case of error.
///
/// Frees @set1 in case of error.
#[doc(alias = "xmlXPathNodeSetMergeAndClear")]
unsafe extern "C" fn xml_xpath_node_set_merge_and_clear(
    set1: XmlNodeSetPtr,
    set2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let init_nb_set1 = (*set1).node_tab.as_ref().map_or(0, |t| t.len());
    if let Some(table) = (*set2).node_tab.as_mut() {
        'b: for n2 in table.drain(..) {
            // Skip duplicates.
            if let Some(set1_table) = (*set1).node_tab.as_mut() {
                for &n1 in &set1_table[..init_nb_set1] {
                    if n1 == n2 {
                        // goto skip_node;
                        continue 'b;
                    } else if matches!((*n1).element_type(), XmlElementType::XmlNamespaceDecl)
                        && matches!((*n2).element_type(), XmlElementType::XmlNamespaceDecl)
                        && (*(n1 as XmlNsPtr)).next == (*(n2 as XmlNsPtr)).next
                        && xml_str_equal((*(n1 as XmlNsPtr)).prefix, (*(n2 as XmlNsPtr)).prefix)
                    {
                        // Free the namespace node.
                        xml_xpath_node_set_free_ns(n2 as XmlNsPtr);
                        // goto skip_node;
                        continue 'b;
                    }
                }
            }
            // grow the nodeTab if needed
            let set1_table = (*set1).node_tab.get_or_insert_with(Vec::new);
            if set1_table.len() >= XPATH_MAX_NODESET_LENGTH {
                xml_xpath_err_memory(null_mut(), Some("merging nodeset hit limit\n"));
                // goto error;
                xml_xpath_free_node_set(set1);
                xml_xpath_node_set_clear(set2, 1);
                return null_mut();
            }
            set1_table.push(n2);
        }
    }
    set1

    // error:
    // xmlXPathFreeNodeSet(set1);
    // xmlXPathNodeSetClear(set2, 1);
    // return null_mut();
}

/// Merges two nodesets, all nodes from @set2 are added to @set1.
/// Doesn't check for duplicate nodes. Clears set2.
///
/// Returns @set1 once extended or NULL in case of error.
///
/// Frees @set1 in case of error.
#[doc(alias = "xmlXPathNodeSetMergeAndClearNoDupls")]
unsafe extern "C" fn xml_xpath_node_set_merge_and_clear_no_dupls(
    set1: XmlNodeSetPtr,
    set2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if let Some(table) = (*set2).node_tab.as_mut() {
        for n2 in table.drain(..) {
            let set1_table = (*set1).node_tab.get_or_insert_with(Vec::new);
            if set1_table.len() >= XPATH_MAX_NODESET_LENGTH {
                xml_xpath_err_memory(null_mut(), Some("merging nodeset hit limit\n"));
                // goto error;
                xml_xpath_free_node_set(set1);
                xml_xpath_node_set_clear(set2, 1);
                return null_mut();
            }
            set1_table.push(n2);
        }
    }
    set1

    // error:
    //     xmlXPathFreeNodeSet(set1);
    //     xmlXPathNodeSetClear(set2, 1);
    //     return null_mut();
}

/// Traversal function for the "child" direction and nodes of type element.
/// The child axis contains the children of the context node in document order.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextChildElement")]
unsafe extern "C" fn xml_xpath_next_child_element(
    ctxt: XmlXPathParserContextPtr,
    mut cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if cur.is_null() {
        cur = (*(*ctxt).context).node;
        if cur.is_null() {
            return null_mut();
        }
        // Get the first element child.
        match (*cur).element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlDocumentFragNode
            /* URGENT TODO: entify-refs as well? */
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode => {
                cur = (*cur).children().map_or(null_mut(), |c| c.as_ptr());
                if !cur.is_null() {
                    if matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
                        return cur;
                    }
                    while {
                        cur = (*cur).next().map_or(null_mut(), |n| n.as_ptr());
                        !cur.is_null() && !matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                    } {}
                    return cur;
                }
                return null_mut();
            }
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode => {
                return (*cur).as_document_node().unwrap().as_ref().get_root_element();
            }
            _ => {
                return null_mut();
            }
        }
    }
    // Get the next sibling element node.
    match (*cur).element_type() {
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
            return null_mut();
        }
    }
    if let Some(next) = (*cur).next {
        if matches!(next.element_type(), XmlElementType::XmlElementNode) {
            return next.as_ptr();
        }
        cur = next.as_ptr();
        while {
            cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
            !cur.is_null() && !matches!((*cur).element_type(), XmlElementType::XmlElementNode)
        } {}
        return cur;
    }
    null_mut()
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
unsafe extern "C" fn xml_xpath_next_preceding_internal(
    ctxt: XmlXPathParserContextPtr,
    mut cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if cur.is_null() {
        cur = (*(*ctxt).context).node;
        if cur.is_null() {
            return null_mut();
        }
        if matches!((*cur).element_type(), XmlElementType::XmlAttributeNode) {
            cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        } else if matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
            let ns: XmlNsPtr = cur as XmlNsPtr;

            if (*ns).next.is_null() || matches!((*(*ns).next).typ, XmlElementType::XmlNamespaceDecl)
            {
                return null_mut();
            }
            cur = (*ns).next as XmlNodePtr;
        }
        (*ctxt).ancestor = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
    }
    if matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    if let Some(prev) = (*cur)
        .prev
        .filter(|p| matches!(p.element_type(), XmlElementType::XmlDTDNode))
    {
        cur = prev.as_ptr();
    }
    while (*cur).prev.is_none() {
        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        if cur.is_null() {
            return null_mut();
        }
        if NodePtr::from_ptr(cur) == (*(*(*ctxt).context).doc).children {
            return null_mut();
        }
        if cur != (*ctxt).ancestor {
            return cur;
        }
        (*ctxt).ancestor = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
    }
    cur = (*cur).prev.map_or(null_mut(), |p| p.as_ptr());
    while let Some(last) = (*cur).last() {
        cur = last.as_ptr();
    }
    cur
}

unsafe extern "C" fn xml_xpath_is_positional_predicate(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    max_pos: *mut i32,
) -> i32 {
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
        XmlXPathOp::XpathOpPredicate | XmlXPathOp::XpathOpFilter
    ) {
        return 0;
    }

    let expr_op = if (*op).ch2 != -1 {
        (*(*ctxt).comp).steps.add((*op).ch2 as usize)
    } else {
        return 0;
    };

    if !expr_op.is_null()
        && matches!((*expr_op).op, XmlXPathOp::XpathOpValue)
        && !(*expr_op).value4.is_null()
        && matches!(
            (*((*expr_op).value4 as XmlXPathObjectPtr)).typ,
            XmlXPathObjectType::XPathNumber
        )
    {
        let floatval: f64 = (*((*expr_op).value4 as XmlXPathObjectPtr)).floatval;

        /*
        	* We have a "[n]" predicate here.
        	* TODO: Unfortunately this simplistic test here is not
        	* able to detect a position() predicate in compound
        	* expressions like "[@attr = 'a" and position() = 1],
        	* and even not the usage of position() in
        	* "[position() = 1]"; thus - obviously - a position-range,
        	* like it "[position() < 5]", is also not detected.
        	* Maybe we could rewrite the AST to ease the optimization.
        	*/

        if floatval > INT_MIN as f64 && floatval < INT_MAX as f64 {
            *max_pos = floatval as i32;
            if floatval == *max_pos as f64 {
                return 1;
            }
        }
    }
    0
}

/// Filter a node set, keeping only nodes for which the predicate expression
/// matches. Afterwards, keep only nodes between minPos and maxPos in the
/// filtered result.
#[doc(alias = "xmlXPathNodeSetFilter")]
unsafe extern "C" fn xml_xpath_node_set_filter(
    ctxt: XmlXPathParserContextPtr,
    set: XmlNodeSetPtr,
    filter_op_index: i32,
    min_pos: i32,
    max_pos: i32,
    has_ns_nodes: i32,
) {
    let mut pos: i32;

    if set.is_null() || (*set).node_tab.as_ref().map_or(0, |t| t.len()) == 0 {
        return;
    }

    // Check if the node set contains a sufficient number of nodes for
    // the requested range.
    if (*set).node_tab.as_ref().map_or(0, |t| t.len() as i32) < min_pos {
        xml_xpath_node_set_clear(set, has_ns_nodes);
        return;
    }

    let xpctxt: XmlXPathContextPtr = (*ctxt).context;
    let oldnode: XmlNodePtr = (*xpctxt).node;
    let olddoc: XmlDocPtr = (*xpctxt).doc;
    let oldcs: i32 = (*xpctxt).context_size;
    let oldpp: i32 = (*xpctxt).proximity_position;
    let filter_op: XmlXPathStepOpPtr = (*(*ctxt).comp).steps.add(filter_op_index as usize);

    (*xpctxt).context_size = (*set).node_tab.as_ref().map_or(0, |t| t.len() as i32);

    let mut i = 0;
    let mut j = 0;
    pos = 1;
    if let Some(table) = (*set).node_tab.as_mut() {
        while i < table.len() {
            let node: XmlNodePtr = table[i];

            (*xpctxt).node = node;
            (*xpctxt).proximity_position = i as i32 + 1;

            // Also set the xpath document in case things like
            // key() are evaluated in the predicate.
            //
            // TODO: Get real doc for namespace nodes.
            if !matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
                && !(*node).doc.is_null()
            {
                (*xpctxt).doc = (*node).doc;
            }

            let res: i32 = xml_xpath_comp_op_eval_to_boolean(ctxt, filter_op, 1);

            if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                break;
            }
            if res < 0 {
                // Shouldn't happen
                xml_xpath_err(ctxt, XmlXPathError::XpathExprError as i32);
                break;
            }

            if res != 0 && (pos >= min_pos && pos <= max_pos) {
                if i != j {
                    table[j] = node;
                    table[i] = null_mut();
                }

                j += 1;
            } else {
                // Remove the entry from the initial node set.
                table[i] = null_mut();
                if matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl) {
                    xml_xpath_node_set_free_ns(node as XmlNsPtr);
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
        if has_ns_nodes != 0 {
            while i < table.len() {
                let node: XmlNodePtr = table[i];
                if !node.is_null()
                    && matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl)
                {
                    xml_xpath_node_set_free_ns(node as XmlNsPtr);
                }
                i += 1;
            }
        }

        table.truncate(j);
        table.shrink_to_fit();
    }

    (*xpctxt).node = oldnode;
    (*xpctxt).doc = olddoc;
    (*xpctxt).context_size = oldcs;
    (*xpctxt).proximity_position = oldpp;
}

/// Filter a node set, keeping only nodes for which the sequence of predicate
/// expressions matches. Afterwards, keep only nodes between minPos and maxPos
/// in the filtered result.
#[doc(alias = "xmlXPathCompOpEvalPredicate")]
unsafe extern "C" fn xml_xpath_comp_op_eval_predicate(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    set: XmlNodeSetPtr,
    min_pos: i32,
    max_pos: i32,
    has_ns_nodes: i32,
) {
    if (*op).ch1 != -1 {
        let comp: XmlXPathCompExprPtr = (*ctxt).comp;
        /*
        	* Process inner predicates first.
        	*/
        if !matches!(
            (*(*comp).steps.add((*op).ch1 as usize)).op,
            XmlXPathOp::XpathOpPredicate
        ) {
            generic_error!("xmlXPathCompOpEvalPredicate: Expected a predicate\n");
            XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
        }
        if (*(*ctxt).context).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
            XP_ERROR!(ctxt, XmlXPathError::XpathRecursionLimitExceeded as i32);
        }
        (*(*ctxt).context).depth += 1;
        xml_xpath_comp_op_eval_predicate(
            ctxt,
            (*comp).steps.add((*op).ch1 as usize),
            set,
            1,
            (*set).node_tab.as_ref().map_or(0, |t| t.len() as i32),
            has_ns_nodes,
        );
        (*(*ctxt).context).depth -= 1;
        CHECK_ERROR!(ctxt);
    }

    if (*op).ch2 != -1 {
        xml_xpath_node_set_filter(ctxt, set, (*op).ch2, min_pos, max_pos, has_ns_nodes);
    }
}

macro_rules! axis_range_end {
    ($out_seq:expr, $seq:expr, $merge_and_clear:ident, $to_bool:expr, $label:tt) => {
        /*
         * We have a "/foo[n]", and position() = n was reached.
         * Note that we can have as well "/foo/::parent::foo[1]", so
         * a duplicate-aware merge is still needed.
         * Merge with the result.
         */
        if $out_seq.is_null() {
            $out_seq = $seq;
            $seq = null_mut();
        } else {
            /* TODO: Check memory error. */
            $out_seq = $merge_and_clear($out_seq, $seq);
        }
        /*
         * Break if only a true/false result was requested.
         */
        if $to_bool != 0 {
            break $label;
        }
        continue $label;
    }
}

macro_rules! first_hit {
    ($out_seq:expr, $seq:expr, $merge_and_clear:expr, $label:tt) => {
        /*
         * Break if only a true/false result was requested and
         * no predicates existed and a node test succeeded.
         */
        if $out_seq.is_null() {
            $out_seq = $seq;
            $seq = null_mut();
        } else {
            /* TODO: Check memory error. */
            $out_seq = $merge_and_clear($out_seq, $seq);
        }
        break $label;
    };
}

macro_rules! xp_test_hit {
    (
        $has_axis_range:expr,
        $pos:expr,
        $max_pos:expr,
        $add_node:ident,
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
                if $add_node($seq, $cur) < 0 {
                    (*$ctxt).error = XmlXPathError::XpathMemoryError as i32;
                }
                axis_range_end!($out_seq, $seq, $merge_and_clear, $to_bool, $label);
            }
        } else {
            if $add_node($seq, $cur) < 0 {
                (*$ctxt).error = XmlXPathError::XpathMemoryError as i32;
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
                $has_ns_nodes = 1;
                if xml_xpath_node_set_add_ns($seq, (*$xpctxt).node, $cur as XmlNsPtr) < 0 {
                    (*$ctxt).error = XmlXPathError::XpathMemoryError as i32;
                }
                axis_range_end!($out_seq, $seq, $merge_and_clear, $to_bool, $label);
            }
        } else {
            $has_ns_nodes = 1;
            if xml_xpath_node_set_add_ns($seq, (*$xpctxt).node, $cur as XmlNsPtr) < 0 {
                (*$ctxt).error = XmlXPathError::XpathMemoryError as i32;
            }
            if $break_on_first_hit != 0 {
                first_hit!($out_seq, $seq, $merge_and_clear, $label);
            }
        }
    };
}

unsafe extern "C" fn xml_xpath_node_collect_and_test(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    mut first: *mut XmlNodePtr,
    mut last: *mut XmlNodePtr,
    to_bool: i32,
) -> i32 {
    let axis: XmlXPathAxisVal = (*op).value.try_into().unwrap();
    let test: XmlXPathTestVal = (*op).value2.try_into().unwrap();
    let typ: XmlXPathTypeVal = (*op).value3.try_into().unwrap();
    let prefix: *const XmlChar = (*op).value4 as _;
    let name: *const XmlChar = (*op).value5 as _;
    let mut uri: *const XmlChar = null();
    let mut total: i32 = 0;
    let mut has_ns_nodes: i32;

    // The final resulting node set wrt to all context nodes
    let mut out_seq: XmlNodeSetPtr;
    // The temporary resulting node set wrt 1 context node.
    // Used to feed predicate evaluation.
    let mut seq: XmlNodeSetPtr;
    let mut cur: XmlNodePtr;
    // First predicate operator
    let mut pred_op: XmlXPathStepOpPtr;
    let mut max_pos: i32; /* The requested position() (when a "[n]" predicate) */
    let mut has_predicate_range: i32;
    let mut has_axis_range: i32;
    let mut pos: i32;

    let next: Option<XmlXPathTraversalFunction>;
    let mut merge_and_clear: XmlXPathNodeSetMergeFunction;
    let xpctxt: XmlXPathContextPtr = (*ctxt).context;

    CHECK_TYPE0!(ctxt, XmlXPathObjectType::XPathNodeset);
    // The popped object holding the context nodes
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    // Setup namespaces.
    if !prefix.is_null() {
        uri = xml_xpath_ns_lookup(xpctxt, prefix);
        if uri.is_null() {
            xml_xpath_release_object(xpctxt, obj);
            XP_ERROR0!(ctxt, XmlXPathError::XpathUndefPrefixError as i32);
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
    merge_and_clear = xml_xpath_node_set_merge_and_clear;
    match axis {
        XmlXPathAxisVal::AxisAncestor => {
            first = null_mut();
            next = Some(xml_xpath_next_ancestor);
        }
        XmlXPathAxisVal::AxisAncestorOrSelf => {
            first = null_mut();
            next = Some(xml_xpath_next_ancestor_or_self);
        }
        XmlXPathAxisVal::AxisAttribute => {
            first = null_mut();
            last = null_mut();
            next = Some(xml_xpath_next_attribute);
            merge_and_clear = xml_xpath_node_set_merge_and_clear_no_dupls;
        }
        XmlXPathAxisVal::AxisChild => {
            last = null_mut();
            if matches!(
                test,
                XmlXPathTestVal::NodeTestName | XmlXPathTestVal::NodeTestAll
            ) && matches!(typ, XmlXPathTypeVal::NodeTypeNode)
            {
                /*
                	* Optimization if an element node type is 'element'.
                	*/
                next = Some(xml_xpath_next_child_element);
            } else {
                next = Some(xml_xpath_next_child);
            }
            merge_and_clear = xml_xpath_node_set_merge_and_clear_no_dupls;
        }
        XmlXPathAxisVal::AxisDescendant => {
            last = null_mut();
            next = Some(xml_xpath_next_descendant);
        }
        XmlXPathAxisVal::AxisDescendantOrSelf => {
            last = null_mut();
            next = Some(xml_xpath_next_descendant_or_self);
        }
        XmlXPathAxisVal::AxisFollowing => {
            last = null_mut();
            next = Some(xml_xpath_next_following);
        }
        XmlXPathAxisVal::AxisFollowingSibling => {
            last = null_mut();
            next = Some(xml_xpath_next_following_sibling);
        }
        XmlXPathAxisVal::AxisNamespace => {
            first = null_mut();
            last = null_mut();
            next = Some(xml_xpath_next_namespace as XmlXPathTraversalFunction);
            merge_and_clear = xml_xpath_node_set_merge_and_clear_no_dupls;
        }
        XmlXPathAxisVal::AxisParent => {
            first = null_mut();
            next = Some(xml_xpath_next_parent);
        }
        XmlXPathAxisVal::AxisPreceding => {
            first = null_mut();
            next = Some(xml_xpath_next_preceding_internal);
        }
        XmlXPathAxisVal::AxisPrecedingSibling => {
            first = null_mut();
            next = Some(xml_xpath_next_preceding_sibling);
        }
        XmlXPathAxisVal::AxisSelf => {
            first = null_mut();
            last = null_mut();
            next = Some(xml_xpath_next_self);
            merge_and_clear = xml_xpath_node_set_merge_and_clear_no_dupls;
        }
    }

    let Some(next) = next else {
        xml_xpath_release_object(xpctxt, obj);
        return 0;
    };
    // The set of context nodes for the node tests
    let context_seq: XmlNodeSetPtr = (*obj).nodesetval;
    if context_seq.is_null()
        || (*context_seq)
            .node_tab
            .as_ref()
            .map_or(true, |t| t.is_empty())
    {
        xml_xpath_release_object(xpctxt, obj);
        value_push(ctxt, xml_xpath_cache_wrap_node_set(xpctxt, null_mut()));
        return 0;
    }
    /*
     * Predicate optimization ---------------------------------------------
     * If this step has a last predicate, which contains a position(),
     * then we'll optimize (although not exactly "position()", but only
     * the  short-hand form, i.e., "[n]".
     *
     * Example - expression "/foo[parent::bar][1]":
     *
     * COLLECT 'child' 'name' 'node' foo    -- op (we are here)
     *   ROOT                               -- (*op).ch1
     *   PREDICATE                          -- (*op).ch2 (predOp)
     *     PREDICATE                          -- (*predOp).ch1 = [parent::bar]
     *       SORT
     *         COLLECT  'parent' 'name' 'node' bar
     *           NODE
     *     ELEM Object is a number : 1        -- (*predOp).ch2 = [1]
     *
     */
    max_pos = 0;
    pred_op = null_mut();
    has_predicate_range = 0;
    has_axis_range = 0;
    if (*op).ch2 != -1 {
        // There's at least one predicate. 16 == XPATH_OP_PREDICATE
        pred_op = (*(*ctxt).comp).steps.add((*op).ch2 as usize);
        if xml_xpath_is_positional_predicate(ctxt, pred_op, addr_of_mut!(max_pos)) != 0 {
            if (*pred_op).ch1 != -1 {
                // Use the next inner predicate operator.
                pred_op = (*(*ctxt).comp).steps.add((*pred_op).ch1 as usize);
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
    let old_context_node: XmlNodePtr = (*xpctxt).node;
    let add_node: unsafe extern "C" fn(XmlNodeSetPtr, XmlNodePtr) -> i32 =
        xml_xpath_node_set_add_unique;
    out_seq = null_mut();
    seq = null_mut();
    let context_node: XmlNodePtr = null_mut();
    let mut context_idx = 0;

    if let Some(table) = (*context_seq).node_tab.as_ref() {
        'main: while (context_idx < table.len() || !context_node.is_null())
            && (*ctxt).error == XmlXPathError::XpathExpressionOk as i32
        {
            (*xpctxt).node = table[context_idx];
            context_idx += 1;

            if seq.is_null() {
                seq = xml_xpath_node_set_create(null_mut());
                if seq.is_null() {
                    /* TODO: Propagate memory error. */
                    total = 0;
                    // goto error;
                    break 'main;
                }
            }
            // Traverse the axis and test the nodes.
            pos = 0;
            cur = null_mut();
            has_ns_nodes = 0;
            while {
                if OP_LIMIT_EXCEEDED!(ctxt, 1) {
                    // goto error;
                    break 'main;
                }

                cur = next(ctxt, cur);
                !cur.is_null()
            } {
                // QUESTION TODO: What does the "first" and "last" stuff do?
                if !first.is_null() && !(*first).is_null() {
                    if *first == cur {
                        break;
                    }
                    if total % 256 == 0
                        && xml_xpath_cmp_nodes_ext(*first, cur).map_or(false, |f| f.is_le())
                    {
                        break;
                    }
                }
                if !last.is_null() && !(*last).is_null() {
                    if *last == cur {
                        break;
                    }
                    if total % 256 == 0
                        && xml_xpath_cmp_nodes_ext(cur, *last).map_or(false, |f| f.is_le())
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
                                    xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                }
                                XmlElementType::XmlNamespaceDecl => {
                                    if matches!(axis, XmlXPathAxisVal::AxisNamespace) {
                                        xp_test_hit_ns!(has_axis_range, pos, max_pos, has_ns_nodes, seq, xpctxt, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    } else {
                                        has_ns_nodes = 1;
                                        xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    }
                                }
                                _ => {}
                            }
                        } else if (*cur).element_type() as isize == typ as isize {
                            if matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
                                xp_test_hit_ns!(has_axis_range, pos, max_pos, has_ns_nodes, seq, xpctxt, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                            } else {
                                xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                            }
                        } else if matches!(typ, XmlXPathTypeVal::NodeTypeText)
                            && matches!((*cur).element_type(), XmlElementType::XmlCDATASectionNode)
                        {
                            xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                        }
                    }
                    XmlXPathTestVal::NodeTestPI => {
                        if matches!((*cur).element_type(), XmlElementType::XmlPINode)
                            && (name.is_null() || xml_str_equal(name, (*cur).name))
                        {
                            xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                        }
                    }
                    XmlXPathTestVal::NodeTestAll => {
                        if matches!(axis, XmlXPathAxisVal::AxisAttribute) {
                            if matches!((*cur).element_type(), XmlElementType::XmlAttributeNode)
                                && (prefix.is_null()
                                    || (!(*cur).ns.is_null()
                                        && xml_str_equal(uri, (*(*cur).ns).href)))
                            {
                                xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                            }
                        } else if matches!(axis, XmlXPathAxisVal::AxisNamespace) {
                            if matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
                                xp_test_hit_ns!(has_axis_range, pos, max_pos, has_ns_nodes, seq, xpctxt, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                            }
                        } else if matches!((*cur).element_type(), XmlElementType::XmlElementNode)
                            && (prefix.is_null()
                                || (!(*cur).ns.is_null() && xml_str_equal(uri, (*(*cur).ns).href)))
                        {
                            xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                        }
                    }
                    XmlXPathTestVal::NodeTestNs => {
                        // todo!();
                    }
                    XmlXPathTestVal::NodeTestName => 'to_break: {
                        if matches!(axis, XmlXPathAxisVal::AxisAttribute) {
                            if !matches!((*cur).element_type(), XmlElementType::XmlAttributeNode) {
                                break 'to_break;
                            }
                        } else if matches!(axis, XmlXPathAxisVal::AxisNamespace) {
                            if !matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
                                break 'to_break;
                            }
                        } else if !matches!((*cur).element_type(), XmlElementType::XmlElementNode) {
                            break 'to_break;
                        }
                        match (*cur).element_type() {
                            XmlElementType::XmlElementNode => {
                                if xml_str_equal(name, (*cur).name) {
                                    if prefix.is_null() {
                                        if (*cur).ns.is_null() {
                                            xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                        }
                                    } else if !(*cur).ns.is_null()
                                        && xml_str_equal(uri, (*(*cur).ns).href)
                                    {
                                        xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    }
                                }
                            }
                            XmlElementType::XmlAttributeNode => {
                                let attr: XmlAttrPtr = (*cur).as_attribute_node().unwrap().as_ptr();

                                if xml_str_equal(name, (*attr).name) {
                                    if prefix.is_null() {
                                        if (*attr).ns.is_null() || (*(*attr).ns).prefix.is_null() {
                                            xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                        }
                                    } else if !(*attr).ns.is_null()
                                        && xml_str_equal(uri, (*(*attr).ns).href)
                                    {
                                        xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    }
                                }
                            }
                            XmlElementType::XmlNamespaceDecl => {
                                if matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl)
                                {
                                    let ns: XmlNsPtr = cur as XmlNsPtr;

                                    if !(*ns).prefix.is_null()
                                        && !name.is_null()
                                        && xml_str_equal((*ns).prefix, name)
                                    {
                                        xp_test_hit_ns!(has_axis_range, pos, max_pos, has_ns_nodes, seq, xpctxt, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }

                if cur.is_null() || (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                    break;
                }
            }

            // goto apply_predicates;

            // apply_predicates: /* --------------------------------------------------- */
            if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                // goto error;
                break 'main;
            }

            // Apply predicates.
            if !pred_op.is_null() && (*seq).node_tab.as_ref().map_or(false, |t| !t.is_empty()) {
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
                        seq,
                        max_pos,
                        max_pos,
                        has_ns_nodes,
                    );
                } else {
                    xml_xpath_comp_op_eval_predicate(
                        ctxt,
                        pred_op,
                        seq,
                        1,
                        (*seq).node_tab.as_ref().map_or(0, |t| t.len() as i32),
                        has_ns_nodes,
                    );
                }

                if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                    total = 0;
                    // goto error;
                    break 'main;
                }
            }

            if (*seq).node_tab.as_ref().map_or(false, |t| !t.is_empty()) {
                // Add to result set.
                if out_seq.is_null() {
                    out_seq = seq;
                    seq = null_mut();
                } else {
                    // TODO: Check memory error.
                    out_seq = merge_and_clear(out_seq, seq);
                }

                if to_bool != 0 {
                    break 'main;
                }
            }
            continue 'main;
        }
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
    if out_seq.is_null() {
        if !seq.is_null() && (*seq).node_tab.as_ref().map_or(true, |t| t.is_empty()) {
            out_seq = seq;
        } else {
            // TODO: Check memory error.
            out_seq = xml_xpath_node_set_create(null_mut());
        }
    }
    if !seq.is_null() && seq != out_seq {
        xml_xpath_free_node_set(seq);
    }
    // Hand over the result. Better to push the set also in case of errors.
    value_push(ctxt, xml_xpath_cache_wrap_node_set(xpctxt, out_seq));
    // Reset the context node.
    (*xpctxt).node = old_context_node;
    // When traversing the namespace axis in "toBool" mode, it's
    // possible that tmpNsList wasn't freed.
    (*xpctxt).tmp_ns_list = None;

    total
}

/// Swaps 2 operations in the compiled expression
#[doc(alias = "xmlXPathCompSwap")]
unsafe extern "C" fn xml_xpath_comp_swap(op: XmlXPathStepOpPtr) {
    std::mem::swap(&mut (*op).ch1, &mut (*op).ch2);
}

/// Evaluate the Precompiled XPath operation searching only the last
/// element in document order
///
/// Returns the number of nodes traversed
#[doc(alias = "xmlXPathCompOpEvalLast")]
unsafe extern "C" fn xml_xpath_comp_op_eval_last(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    last: *mut XmlNodePtr,
) -> i32 {
    let mut total: i32 = 0;
    let cur: i32;
    let arg1: XmlXPathObjectPtr;
    let arg2: XmlXPathObjectPtr;

    CHECK_ERROR0!(ctxt);
    if OP_LIMIT_EXCEEDED!(ctxt, 1) {
        return 0;
    }
    if (*(*ctxt).context).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
        XP_ERROR0!(ctxt, XmlXPathError::XpathRecursionLimitExceeded as i32);
    }
    (*(*ctxt).context).depth += 1;
    let comp: XmlXPathCompExprPtr = (*ctxt).comp;
    match (*op).op {
        XmlXPathOp::XpathOpEnd => {}
        XmlXPathOp::XpathOpUnion => {
            total = xml_xpath_comp_op_eval_last(ctxt, (*comp).steps.add((*op).ch1 as usize), last);
            CHECK_ERROR0!(ctxt);
            if !(*ctxt).value.is_null()
                && (*(*ctxt).value).typ == XmlXPathObjectType::XPathNodeset
                && !(*(*ctxt).value).nodesetval.is_null()
            {
                if let Some(table) = (*(*(*ctxt).value).nodesetval)
                    .node_tab
                    .as_ref()
                    .filter(|t| !t.is_empty())
                {
                    // limit tree traversing to first node in the result
                    if (*(*(*ctxt).value).nodesetval)
                        .node_tab
                        .as_ref()
                        .map_or(false, |t| t.len() > 1)
                    {
                        xml_xpath_node_set_sort((*(*ctxt).value).nodesetval);
                    }
                    *last = *table.last().unwrap();
                }
            }
            cur = xml_xpath_comp_op_eval_last(ctxt, (*comp).steps.add((*op).ch2 as usize), last);
            CHECK_ERROR0!(ctxt);
            if !(*ctxt).value.is_null()
                && matches!((*(*ctxt).value).typ, XmlXPathObjectType::XPathNodeset)
                && !(*(*ctxt).value).nodesetval.is_null()
                && (*(*(*ctxt).value).nodesetval)
                    .node_tab
                    .as_ref()
                    .map_or(false, |t| !t.is_empty())
            { /* TODO: NOP ? */ }

            arg2 = value_pop(ctxt);
            arg1 = value_pop(ctxt);
            if arg1.is_null()
                || (*arg1).typ != XmlXPathObjectType::XPathNodeset
                || arg2.is_null()
                || (*arg2).typ != XmlXPathObjectType::XPathNodeset
            {
                xml_xpath_release_object((*ctxt).context, arg1);
                xml_xpath_release_object((*ctxt).context, arg2);
                XP_ERROR0!(ctxt, XmlXPathError::XpathInvalidType as i32);
            }
            if (*(*ctxt).context).op_limit != 0
                && ((!(*arg1).nodesetval.is_null()
                    && xml_xpath_check_op_limit(
                        ctxt,
                        (*(*arg1).nodesetval)
                            .node_tab
                            .as_ref()
                            .map_or(0, |t| t.len()) as _,
                    ) < 0)
                    || (!(*arg2).nodesetval.is_null()
                        && xml_xpath_check_op_limit(
                            ctxt,
                            (*(*arg2).nodesetval)
                                .node_tab
                                .as_ref()
                                .map_or(0, |t| t.len()) as _,
                        ) < 0))
            {
                xml_xpath_release_object((*ctxt).context, arg1);
                xml_xpath_release_object((*ctxt).context, arg2);
                // break;
            } else {
                /* TODO: Check memory error. */
                (*arg1).nodesetval =
                    xml_xpath_node_set_merge((*arg1).nodesetval, (*arg2).nodesetval);
                value_push(ctxt, arg1);
                xml_xpath_release_object((*ctxt).context, arg2);
                /* optimizer */
                if total > cur {
                    xml_xpath_comp_swap(op);
                }
                total += cur;
            }
        }
        XmlXPathOp::XpathOpRoot => {
            xml_xpath_root(ctxt);
        }
        XmlXPathOp::XpathOpNode => {
            if (*op).ch1 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            }
            CHECK_ERROR0!(ctxt);
            if (*op).ch2 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch2 as usize));
            }
            CHECK_ERROR0!(ctxt);
            value_push(
                ctxt,
                xml_xpath_cache_new_node_set((*ctxt).context, (*(*ctxt).context).node),
            );
        }
        XmlXPathOp::XpathOpCollect => {
            if (*op).ch1 == -1 {
                // break;
            } else {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
                CHECK_ERROR0!(ctxt);
                total += xml_xpath_node_collect_and_test(ctxt, op, null_mut(), last, 0);
            }
        }
        XmlXPathOp::XpathOpValue => {
            value_push(
                ctxt,
                xml_xpath_cache_object_copy((*ctxt).context, (*op).value4 as XmlXPathObjectPtr),
            );
        }
        XmlXPathOp::XpathOpSort => {
            if (*op).ch1 != -1 {
                total +=
                    xml_xpath_comp_op_eval_last(ctxt, (*comp).steps.add((*op).ch1 as usize), last);
            }
            CHECK_ERROR0!(ctxt);
            if !(*ctxt).value.is_null()
                && (*(*ctxt).value).typ == XmlXPathObjectType::XPathNodeset
                && !(*(*ctxt).value).nodesetval.is_null()
                && (*(*(*ctxt).value).nodesetval)
                    .node_tab
                    .as_ref()
                    .map_or(0, |t| t.len())
                    > 1
            {
                xml_xpath_node_set_sort((*(*ctxt).value).nodesetval);
            }
        }
        _ => {
            total += xml_xpath_comp_op_eval(ctxt, op);
        }
    }

    (*(*ctxt).context).depth -= 1;
    total
}

/// Move the last node to the first position and clear temporary XPath objects
/// (e.g. namespace nodes) from all other nodes. Sets the length of the list to 1.
#[doc(alias = "xmlXPathNodeSetKeepLast")]
unsafe extern "C" fn xml_xpath_node_set_keep_last(set: XmlNodeSetPtr) {
    if set.is_null() || (*set).node_tab.as_ref().map_or(0, |t| t.len()) <= 1 {
        return;
    }
    let Some(table) = (*set).node_tab.as_mut().filter(|t| t.len() > 1) else {
        return;
    };
    let len = table.len();
    for node in table.drain(..len - 1) {
        if !node.is_null() && matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl) {
            xml_xpath_node_set_free_ns(node as XmlNsPtr);
        }
    }
}

/// Filter a location set, keeping only nodes for which the predicate expression matches.  
/// Afterwards, keep only nodes between minPos and maxPos in the filtered result.
#[doc(alias = "xmlXPathLocationSetFilter")]
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xpath_location_set_filter(
    ctxt: XmlXPathParserContextPtr,
    locset: XmlLocationSetPtr,
    filter_op_index: i32,
    min_pos: i32,
    max_pos: i32,
) {
    let mut i: i32;
    let mut j: i32;
    let mut pos: i32;

    if locset.is_null() || (*locset).loc_nr == 0 || filter_op_index == -1 {
        return;
    }

    let xpctxt: XmlXPathContextPtr = (*ctxt).context;
    let oldnode: XmlNodePtr = (*xpctxt).node;
    let olddoc: XmlDocPtr = (*xpctxt).doc;
    let oldcs: i32 = (*xpctxt).context_size;
    let oldpp: i32 = (*xpctxt).proximity_position;
    let filter_op: XmlXPathStepOpPtr = (*(*ctxt).comp).steps.add(filter_op_index as usize);

    (*xpctxt).context_size = (*locset).loc_nr;

    i = 0;
    j = 0;
    pos = 1;
    while i < (*locset).loc_nr {
        let context_node: XmlNodePtr = (*(*(*locset).loc_tab.add(i as usize))).user as _;

        (*xpctxt).node = context_node;
        (*xpctxt).proximity_position = i + 1;

        /*
        	* Also set the xpath document in case things like
        	* key() are evaluated in the predicate.
        	*
        	* TODO: Get real doc for namespace nodes.
        	*/
        if !matches!((*context_node).typ, XmlElementType::XmlNamespaceDecl)
            && !(*context_node).doc.is_null()
        {
            (*xpctxt).doc = (*context_node).doc;
        }

        let res: i32 = xml_xpath_comp_op_eval_to_boolean(ctxt, filter_op, 1);

        if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
            break;
        }
        if res < 0 {
            /* Shouldn't happen */
            xml_xpath_err(ctxt, XmlXPathError::XpathExprError as i32);
            break;
        }

        if res != 0 && (pos >= min_pos && pos <= max_pos) {
            if i != j {
                *(*locset).loc_tab.add(j as usize) = *(*locset).loc_tab.add(i as usize);
                *(*locset).loc_tab.add(i as usize) = null_mut();
            }

            j += 1;
        } else {
            /* Remove the entry from the initial location set. */
            xml_xpath_free_object(*(*locset).loc_tab.add(i as usize));
            *(*locset).loc_tab.add(i as usize) = null_mut();
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

    /* Free remaining nodes. */
    while i < (*locset).loc_nr {
        xml_xpath_free_object(*(*locset).loc_tab.add(i as usize));
        i += 1;
    }

    (*locset).loc_nr = j;

    /* If too many elements were removed, shrink table to preserve memory. */
    if (*locset).loc_max > XML_NODESET_DEFAULT as i32 && (*locset).loc_nr < (*locset).loc_max / 2 {
        let mut loc_max: i32 = (*locset).loc_nr;

        if loc_max < XML_NODESET_DEFAULT as i32 {
            loc_max = XML_NODESET_DEFAULT as i32;
        }
        let tmp: *mut XmlXPathObjectPtr = xml_realloc(
            (*locset).loc_tab as _,
            loc_max as usize * size_of::<XmlXPathObjectPtr>(),
        ) as *mut XmlXPathObjectPtr;
        if tmp.is_null() {
            xml_xpath_perr_memory(ctxt, Some("shrinking locset\n"));
        } else {
            (*locset).loc_tab = tmp;
            (*locset).loc_max = loc_max;
        }
    }

    (*xpctxt).node = oldnode;
    (*xpctxt).doc = olddoc;
    (*xpctxt).context_size = oldcs;
    (*xpctxt).proximity_position = oldpp;
}

unsafe extern "C" fn xml_xpath_comp_op_eval_filter_first(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    first: *mut XmlNodePtr,
) -> i32 {
    let mut total: i32 = 0;

    CHECK_ERROR0!(ctxt);
    let comp: XmlXPathCompExprPtr = (*ctxt).comp;
    /*
     * Optimization for ()[last()] selection i.e. the last elem
     */
    if (*op).ch1 != -1
        && (*op).ch2 != -1
        && matches!(
            (*(*comp).steps.add((*op).ch1 as usize)).op,
            XmlXPathOp::XpathOpSort
        )
        && matches!(
            (*(*comp).steps.add((*op).ch2 as usize)).op,
            XmlXPathOp::XpathOpSort
        )
    {
        let f: i32 = (*(*comp).steps.add((*op).ch2 as usize)).ch1;

        if f != -1
            && matches!(
                (*(*comp).steps.add(f as usize)).op,
                XmlXPathOp::XpathOpFunction
            )
            && (*(*comp).steps.add(f as usize)).value5.is_null()
            && (*(*comp).steps.add(f as usize)).value == 0
            && !(*(*comp).steps.add(f as usize)).value4.is_null()
            && xml_str_equal(
                (*(*comp).steps.add(f as usize)).value4 as _,
                c"last".as_ptr() as _,
            )
        {
            let mut last: XmlNodePtr = null_mut();

            total += xml_xpath_comp_op_eval_last(
                ctxt,
                (*comp).steps.add((*op).ch1 as usize),
                addr_of_mut!(last),
            );
            CHECK_ERROR0!(ctxt);
            // The nodeset should be in document order,
            // Keep only the last value
            if !(*ctxt).value.is_null()
                && (*(*ctxt).value).typ == XmlXPathObjectType::XPathNodeset
                && !(*(*ctxt).value).nodesetval.is_null()
                && (*(*(*ctxt).value).nodesetval)
                    .node_tab
                    .as_ref()
                    .map_or(false, |t| t.len() > 1)
            {
                xml_xpath_node_set_keep_last((*(*ctxt).value).nodesetval);
                *first = (*(*(*ctxt).value).nodesetval).node_tab.as_ref().unwrap()[0];
            }
            return total;
        }
    }

    if (*op).ch1 != -1 {
        total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
    }
    CHECK_ERROR0!(ctxt);
    if (*op).ch2 == -1 {
        return total;
    }
    if (*ctxt).value.is_null() {
        return total;
    }

    #[cfg(feature = "libxml_xptr_locs")]
    {
        /*
         * Hum are we filtering the result of an XPointer expression
         */
        if matches!((*(*ctxt).value).typ, XmlXPathObjectType::XPathLocationset) {
            let locset: XmlLocationSetPtr = (*(*ctxt).value).user as _;

            if !locset.is_null() {
                xml_xpath_location_set_filter(ctxt, locset, (*op).ch2, 1, 1);
                if (*locset).loc_nr > 0 {
                    *first = (*(*(*locset).loc_tab.add(0))).user as XmlNodePtr;
                }
            }

            return total;
        }
    }

    // In case of errors, xmlXPathNodeSetFilter can pop additional nodes from the stack.
    // We have to temporarily remove the nodeset object from the
    // stack to avoid freeing it prematurely.
    CHECK_TYPE0!(ctxt, XmlXPathObjectType::XPathNodeset);
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    let set: XmlNodeSetPtr = (*obj).nodesetval;
    if !set.is_null() {
        xml_xpath_node_set_filter(ctxt, set, (*op).ch2, 1, 1, 1);
        if let Some(table) = (*set).node_tab.as_ref().filter(|t| !t.is_empty()) {
            *first = table[0];
        }
    }
    value_push(ctxt, obj);

    total
}

/// Evaluate the Precompiled XPath operation searching only the first element in document order
///
/// Returns the number of examined objects.
#[doc(alias = "xmlXPathCompOpEvalFirst")]
unsafe extern "C" fn xml_xpath_comp_op_eval_first(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    first: *mut XmlNodePtr,
) -> i32 {
    let mut total: i32 = 0;
    let cur: i32;

    let arg1: XmlXPathObjectPtr;
    let arg2: XmlXPathObjectPtr;

    CHECK_ERROR0!(ctxt);
    if OP_LIMIT_EXCEEDED!(ctxt, 1) {
        return 0;
    }
    if (*(*ctxt).context).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
        XP_ERROR0!(ctxt, XmlXPathError::XpathRecursionLimitExceeded as i32);
    }
    (*(*ctxt).context).depth += 1;
    let comp: XmlXPathCompExprPtr = (*ctxt).comp;
    match (*op).op {
        XmlXPathOp::XpathOpEnd => {}
        XmlXPathOp::XpathOpUnion => {
            total =
                xml_xpath_comp_op_eval_first(ctxt, (*comp).steps.add((*op).ch1 as usize), first);
            CHECK_ERROR0!(ctxt);
            if !(*ctxt).value.is_null()
                && (*(*ctxt).value).typ == XmlXPathObjectType::XPathNodeset
                && !(*(*ctxt).value).nodesetval.is_null()
                && (*(*(*ctxt).value).nodesetval)
                    .node_tab
                    .as_ref()
                    .map_or(0, |t| t.len())
                    >= 1
            {
                // limit tree traversing to first node in the result
                // OPTIMIZE TODO: This implicitly sorts
                //  the result, even if not needed. E.g. if the argument
                //  of the count() function, no sorting is needed.
                // OPTIMIZE TODO: How do we know if the node-list wasn't
                //  already sorted?
                if (*(*(*ctxt).value).nodesetval)
                    .node_tab
                    .as_ref()
                    .map_or(0, |t| t.len())
                    > 1
                {
                    xml_xpath_node_set_sort((*(*ctxt).value).nodesetval);
                }
                *first = (*(*(*ctxt).value).nodesetval).node_tab.as_ref().unwrap()[0];
            }
            cur = xml_xpath_comp_op_eval_first(ctxt, (*comp).steps.add((*op).ch2 as usize), first);
            CHECK_ERROR0!(ctxt);

            arg2 = value_pop(ctxt);
            arg1 = value_pop(ctxt);
            if arg1.is_null()
                || (*arg1).typ != XmlXPathObjectType::XPathNodeset
                || arg2.is_null()
                || (*arg2).typ != XmlXPathObjectType::XPathNodeset
            {
                xml_xpath_release_object((*ctxt).context, arg1);
                xml_xpath_release_object((*ctxt).context, arg2);
                XP_ERROR0!(ctxt, XmlXPathError::XpathInvalidType as i32);
            }
            if (*(*ctxt).context).op_limit != 0
                && ((!(*arg1).nodesetval.is_null()
                    && xml_xpath_check_op_limit(
                        ctxt,
                        (*(*arg1).nodesetval)
                            .node_tab
                            .as_ref()
                            .map_or(0, |t| t.len()) as _,
                    ) < 0)
                    || (!(*arg2).nodesetval.is_null()
                        && xml_xpath_check_op_limit(
                            ctxt,
                            (*(*arg2).nodesetval)
                                .node_tab
                                .as_ref()
                                .map_or(0, |t| t.len()) as _,
                        ) < 0))
            {
                xml_xpath_release_object((*ctxt).context, arg1);
                xml_xpath_release_object((*ctxt).context, arg2);
            } else {
                /* TODO: Check memory error. */
                (*arg1).nodesetval =
                    xml_xpath_node_set_merge((*arg1).nodesetval, (*arg2).nodesetval);
                value_push(ctxt, arg1);
                xml_xpath_release_object((*ctxt).context, arg2);
                /* optimizer */
                if total > cur {
                    xml_xpath_comp_swap(op);
                }
                total += cur;
            }
        }
        XmlXPathOp::XpathOpRoot => {
            xml_xpath_root(ctxt);
        }
        XmlXPathOp::XpathOpNode => {
            if (*op).ch1 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            }
            CHECK_ERROR0!(ctxt);
            if (*op).ch2 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch2 as usize));
            }
            CHECK_ERROR0!(ctxt);
            value_push(
                ctxt,
                xml_xpath_cache_new_node_set((*ctxt).context, (*(*ctxt).context).node),
            );
        }
        XmlXPathOp::XpathOpCollect => {
            if (*op).ch1 == -1 {
                // break;
            } else {
                total = xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
                CHECK_ERROR0!(ctxt);
                total += xml_xpath_node_collect_and_test(ctxt, op, first, null_mut(), 0);
            }
        }
        XmlXPathOp::XpathOpValue => {
            value_push(
                ctxt,
                xml_xpath_cache_object_copy((*ctxt).context, (*op).value4 as XmlXPathObjectPtr),
            );
        }
        XmlXPathOp::XpathOpSort => {
            if (*op).ch1 != -1 {
                total += xml_xpath_comp_op_eval_first(
                    ctxt,
                    (*comp).steps.add((*op).ch1 as usize),
                    first,
                );
            }
            CHECK_ERROR0!(ctxt);
            if !(*ctxt).value.is_null()
                && (*(*ctxt).value).typ == XmlXPathObjectType::XPathNodeset
                && !(*(*ctxt).value).nodesetval.is_null()
                && (*(*(*ctxt).value).nodesetval)
                    .node_tab
                    .as_ref()
                    .map_or(0, |t| t.len())
                    > 1
            {
                xml_xpath_node_set_sort((*(*ctxt).value).nodesetval);
            }
        }
        XmlXPathOp::XpathOpFilter => {
            total += xml_xpath_comp_op_eval_filter_first(ctxt, op, first);
        }
        _ => {
            total += xml_xpath_comp_op_eval(ctxt, op);
        }
    }

    (*(*ctxt).context).depth -= 1;
    total
}

/// Evaluate the Precompiled XPath operation
/// Returns the number of nodes traversed
#[doc(alias = "xmlXPathCompOpEval")]
unsafe extern "C" fn xml_xpath_comp_op_eval(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
) -> i32 {
    let mut total: i32 = 0;
    let equal: i32;
    let ret: i32;
    let arg1: XmlXPathObjectPtr;
    let arg2: XmlXPathObjectPtr;

    CHECK_ERROR0!(ctxt);
    if OP_LIMIT_EXCEEDED!(ctxt, 1) {
        return 0;
    }
    if (*(*ctxt).context).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
        XP_ERROR0!(ctxt, XmlXPathError::XpathRecursionLimitExceeded as i32);
    }
    (*(*ctxt).context).depth += 1;
    let comp: XmlXPathCompExprPtr = (*ctxt).comp;
    match (*op).op {
        XmlXPathOp::XpathOpEnd => {}
        XmlXPathOp::XpathOpAnd => 'to_break: {
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            CHECK_ERROR0!(ctxt);
            xml_xpath_boolean_function(ctxt, 1);
            if (*ctxt).value.is_null() || !(*(*ctxt).value).boolval {
                break 'to_break;
            }
            arg2 = value_pop(ctxt);
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch2 as usize));
            if (*ctxt).error != 0 {
                xml_xpath_free_object(arg2);
                break 'to_break;
            }
            xml_xpath_boolean_function(ctxt, 1);
            if !(*ctxt).value.is_null() {
                (*(*ctxt).value).boolval &= (*arg2).boolval;
            }
            xml_xpath_release_object((*ctxt).context, arg2);
        }
        XmlXPathOp::XpathOpOr => 'to_break: {
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            CHECK_ERROR0!(ctxt);
            xml_xpath_boolean_function(ctxt, 1);
            if (*ctxt).value.is_null() || (*(*ctxt).value).boolval {
                break 'to_break;
            }
            arg2 = value_pop(ctxt);
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch2 as usize));
            if (*ctxt).error != 0 {
                xml_xpath_free_object(arg2);
                break 'to_break;
            }
            xml_xpath_boolean_function(ctxt, 1);
            if !(*ctxt).value.is_null() {
                (*(*ctxt).value).boolval |= (*arg2).boolval;
            }
            xml_xpath_release_object((*ctxt).context, arg2);
        }
        XmlXPathOp::XpathOpEqual => {
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            CHECK_ERROR0!(ctxt);
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch2 as usize));
            CHECK_ERROR0!(ctxt);
            if (*op).value != 0 {
                equal = xml_xpath_equal_values(ctxt);
            } else {
                equal = xml_xpath_not_equal_values(ctxt);
            }
            value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, equal));
        }
        XmlXPathOp::XpathOpCmp => {
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            CHECK_ERROR0!(ctxt);
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch2 as usize));
            CHECK_ERROR0!(ctxt);
            ret = xml_xpath_compare_values(ctxt, (*op).value, (*op).value2);
            value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, ret));
        }
        XmlXPathOp::XpathOpPlus => {
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            CHECK_ERROR0!(ctxt);
            if (*op).ch2 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch2 as usize));
            }
            CHECK_ERROR0!(ctxt);
            if (*op).value == 0 {
                xml_xpath_sub_values(ctxt);
            } else if (*op).value == 1 {
                xml_xpath_add_values(ctxt);
            } else if (*op).value == 2 {
                xml_xpath_value_flip_sign(ctxt);
            } else if (*op).value == 3 {
                CAST_TO_NUMBER!(ctxt);
                CHECK_TYPE0!(ctxt, XmlXPathObjectType::XPathNumber);
            }
        }
        XmlXPathOp::XpathOpMult => {
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            CHECK_ERROR0!(ctxt);
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch2 as usize));
            CHECK_ERROR0!(ctxt);
            if (*op).value == 0 {
                xml_xpath_mult_values(ctxt);
            } else if (*op).value == 1 {
                xml_xpath_div_values(ctxt);
            } else if (*op).value == 2 {
                xml_xpath_mod_values(ctxt);
            }
        }
        XmlXPathOp::XpathOpUnion => 'to_break: {
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            CHECK_ERROR0!(ctxt);
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch2 as usize));
            CHECK_ERROR0!(ctxt);

            arg2 = value_pop(ctxt);
            arg1 = value_pop(ctxt);
            if arg1.is_null()
                || (*arg1).typ != XmlXPathObjectType::XPathNodeset
                || arg2.is_null()
                || (*arg2).typ != XmlXPathObjectType::XPathNodeset
            {
                xml_xpath_release_object((*ctxt).context, arg1);
                xml_xpath_release_object((*ctxt).context, arg2);
                XP_ERROR0!(ctxt, XmlXPathError::XpathInvalidType as i32);
            }
            if (*(*ctxt).context).op_limit != 0
                && ((!(*arg1).nodesetval.is_null()
                    && xml_xpath_check_op_limit(
                        ctxt,
                        (*(*arg1).nodesetval)
                            .node_tab
                            .as_ref()
                            .map_or(0, |t| t.len()) as _,
                    ) < 0)
                    || (!(*arg2).nodesetval.is_null()
                        && xml_xpath_check_op_limit(
                            ctxt,
                            (*(*arg2).nodesetval)
                                .node_tab
                                .as_ref()
                                .map_or(0, |t| t.len()) as _,
                        ) < 0))
            {
                xml_xpath_release_object((*ctxt).context, arg1);
                xml_xpath_release_object((*ctxt).context, arg2);
                break 'to_break;
            }
            if (*arg1).nodesetval.is_null()
                || (!(*arg2).nodesetval.is_null()
                    && (*(*arg2).nodesetval)
                        .node_tab
                        .as_ref()
                        .map_or(0, |t| t.len())
                        != 0)
            {
                /* TODO: Check memory error. */
                (*arg1).nodesetval =
                    xml_xpath_node_set_merge((*arg1).nodesetval, (*arg2).nodesetval);
            }

            value_push(ctxt, arg1);
            xml_xpath_release_object((*ctxt).context, arg2);
        }
        XmlXPathOp::XpathOpRoot => {
            xml_xpath_root(ctxt);
        }
        XmlXPathOp::XpathOpNode => {
            if (*op).ch1 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            }
            CHECK_ERROR0!(ctxt);
            if (*op).ch2 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch2 as usize));
            }
            CHECK_ERROR0!(ctxt);
            value_push(
                ctxt,
                xml_xpath_cache_new_node_set((*ctxt).context, (*(*ctxt).context).node),
            );
        }
        XmlXPathOp::XpathOpCollect => 'to_break: {
            if (*op).ch1 == -1 {
                break 'to_break;
            }
            total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            CHECK_ERROR0!(ctxt);

            total += xml_xpath_node_collect_and_test(ctxt, op, null_mut(), null_mut(), 0);
        }
        XmlXPathOp::XpathOpValue => {
            value_push(
                ctxt,
                xml_xpath_cache_object_copy((*ctxt).context, (*op).value4 as XmlXPathObjectPtr),
            );
        }
        XmlXPathOp::XpathOpVariable => 'to_break: {
            let val: XmlXPathObjectPtr;
            if (*op).ch1 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            }
            if (*op).value5.is_null() {
                val = xml_xpath_variable_lookup((*ctxt).context, (*op).value4 as _);
                if val.is_null() {
                    XP_ERROR0!(ctxt, XmlXPathError::XpathUndefVariableError as i32);
                }
                value_push(ctxt, val);
            } else {
                let uri: *const XmlChar = xml_xpath_ns_lookup((*ctxt).context, (*op).value5 as _);
                if uri.is_null() {
                    generic_error!(
                        "xmlXPathCompOpEval: variable {} bound to undefined prefix {}\n",
                        CStr::from_ptr((*op).value4 as *const i8).to_string_lossy(),
                        CStr::from_ptr((*op).value5 as *const i8).to_string_lossy()
                    );
                    (*ctxt).error = XmlXPathError::XpathUndefPrefixError as _;
                    break 'to_break;
                }
                val = xml_xpath_variable_lookup_ns((*ctxt).context, (*op).value4 as _, uri);
                if val.is_null() {
                    XP_ERROR0!(ctxt, XmlXPathError::XpathUndefVariableError as i32);
                }
                value_push(ctxt, val);
            }
        }
        XmlXPathOp::XpathOpFunction => 'to_break: {
            let func: XmlXPathFunction;

            let frame: i32 = (*ctxt).value_nr;
            if (*op).ch1 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
                if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                    break 'to_break;
                }
            }
            if (*ctxt).value_nr < frame + (*op).value {
                generic_error!("xmlXPathCompOpEval: parameter error\n");
                (*ctxt).error = XmlXPathError::XpathInvalidOperand as i32;
                break 'to_break;
            }
            for i in 0..(*op).value {
                if (*(*ctxt)
                    .value_tab
                    .add(((*ctxt).value_nr as usize - 1) - i as usize))
                .is_null()
                {
                    generic_error!("xmlXPathCompOpEval: parameter error\n");
                    (*ctxt).error = XmlXPathError::XpathInvalidOperand as i32;
                    break;
                }
            }
            if let Some(cache) = (*op).cache {
                func = cache;
            } else {
                let mut uri: *const XmlChar = null();

                let f = if (*op).value5.is_null() {
                    xml_xpath_function_lookup((*ctxt).context, (*op).value4 as _)
                } else {
                    uri = xml_xpath_ns_lookup((*ctxt).context, (*op).value5 as _);
                    if uri.is_null() {
                        generic_error!(
                            "xmlXPathCompOpEval: function {} bound to undefined prefix {}\n",
                            CStr::from_ptr((*op).value4 as *const i8).to_string_lossy(),
                            CStr::from_ptr((*op).value5 as *const i8).to_string_lossy()
                        );
                        (*ctxt).error = XmlXPathError::XpathUndefPrefixError as i32;
                        break 'to_break;
                    }
                    xml_xpath_function_lookup_ns((*ctxt).context, (*op).value4 as _, uri)
                };
                if let Some(f) = f {
                    func = f;
                } else {
                    generic_error!(
                        "xmlXPathCompOpEval: function {} not found\n",
                        CStr::from_ptr((*op).value4 as *mut i8).to_string_lossy()
                    );
                    XP_ERROR0!(ctxt, XmlXPathError::XpathUnknownFuncError as i32);
                }

                (*op).cache = Some(func);
                (*op).cache_uri = uri as *mut c_void;
            }

            let old_func: *const XmlChar = (*(*ctxt).context).function;
            let old_func_uri: *const XmlChar = (*(*ctxt).context).function_uri;
            (*(*ctxt).context).function = (*op).value4 as _;
            (*(*ctxt).context).function_uri = (*op).cache_uri as _;
            func(ctxt, (*op).value);
            (*(*ctxt).context).function = old_func;
            (*(*ctxt).context).function_uri = old_func_uri;
            if (*ctxt).error == XmlXPathError::XpathExpressionOk as i32
                && (*ctxt).value_nr != frame + 1
            {
                XP_ERROR0!(ctxt, XmlXPathError::XpathStackError as i32);
            }
        }
        XmlXPathOp::XpathOpArg => {
            if (*op).ch1 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
                CHECK_ERROR0!(ctxt);
            }
            if (*op).ch2 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch2 as usize));
                CHECK_ERROR0!(ctxt);
            }
        }
        XmlXPathOp::XpathOpPredicate | XmlXPathOp::XpathOpFilter => 'to_break: {
            /*
             * Optimization for ()[1] selection i.e. the first elem
             */
            if (*op).ch1 != -1 && (*op).ch2 != -1 &&
		    		/*
		    		* FILTER TODO: Can we assume that the inner processing
		    		*  will result in an ordered list if we have an
		    		*  XPATH_OP_FILTER?
		    		*  What about an additional field or flag on
		    		*  xmlXPathObject like @sorted ? This way we wouldn't need
		    		*  to assume anything, so it would be more robust and
		    		*  easier to optimize.
		    		*/
                    (matches!((*(*comp).steps.add((*op).ch1 as usize)).op, XmlXPathOp::XpathOpSort) || /* 18 */
			    	matches!((*(*comp).steps.add((*op).ch1 as usize)).op, XmlXPathOp::XpathOpFilter)) && /* 17 */
                    matches!((*(*comp).steps.add((*op).ch2 as usize)).op, XmlXPathOp::XpathOpValue)
            {
                /* 12 */

                let val: XmlXPathObjectPtr = (*(*comp).steps.add((*op).ch2 as usize)).value4 as _;
                if !val.is_null()
                    && (*val).typ == XmlXPathObjectType::XPathNumber
                    && (*val).floatval == 1.0
                {
                    let mut first: XmlNodePtr = null_mut();

                    total += xml_xpath_comp_op_eval_first(
                        ctxt,
                        (*comp).steps.add((*op).ch1 as usize),
                        addr_of_mut!(first),
                    );
                    CHECK_ERROR0!(ctxt);
                    // The nodeset should be in document order, Keep only the first value
                    if !(*ctxt).value.is_null()
                        && (*(*ctxt).value).typ == XmlXPathObjectType::XPathNodeset
                        && !(*(*ctxt).value).nodesetval.is_null()
                        && (*(*(*ctxt).value).nodesetval)
                            .node_tab
                            .as_ref()
                            .map_or(0, |t| t.len())
                            > 1
                    {
                        xml_xpath_node_set_clear_from_pos((*(*ctxt).value).nodesetval, 1, 1);
                    }
                    break 'to_break;
                }
            }
            // Optimization for ()[last()] selection i.e. the last elem
            if (*op).ch1 != -1
                && (*op).ch2 != -1
                && matches!(
                    (*(*comp).steps.add((*op).ch1 as usize)).op,
                    XmlXPathOp::XpathOpSort
                )
                && matches!(
                    (*(*comp).steps.add((*op).ch2 as usize)).op,
                    XmlXPathOp::XpathOpSort
                )
            {
                let f: i32 = (*(*comp).steps.add((*op).ch2 as usize)).ch1;

                if f != -1
                    && matches!(
                        (*(*comp).steps.add(f as usize)).op,
                        XmlXPathOp::XpathOpFunction
                    )
                    && (*(*comp).steps.add(f as usize)).value5.is_null()
                    && (*(*comp).steps.add(f as usize)).value == 0
                    && !(*(*comp).steps.add(f as usize)).value4.is_null()
                    && xml_str_equal(
                        (*(*comp).steps.add(f as usize)).value4 as _,
                        c"last".as_ptr() as _,
                    )
                {
                    let mut last: XmlNodePtr = null_mut();

                    total += xml_xpath_comp_op_eval_last(
                        ctxt,
                        (*comp).steps.add((*op).ch1 as usize),
                        addr_of_mut!(last),
                    );
                    CHECK_ERROR0!(ctxt);
                    // The nodeset should be in document order, Keep only the last value
                    if !(*ctxt).value.is_null()
                        && (*(*ctxt).value).typ == XmlXPathObjectType::XPathNodeset
                        && !(*(*ctxt).value).nodesetval.is_null()
                        && (*(*(*ctxt).value).nodesetval)
                            .node_tab
                            .as_ref()
                            .map_or(false, |t| t.len() > 1)
                    {
                        xml_xpath_node_set_keep_last((*(*ctxt).value).nodesetval);
                    }
                    break 'to_break;
                }
            }
            // Process inner predicates first.
            // Example "index[parent::book][1]":
            // ...
            //   PREDICATE   <-=1 we are here "[1]"
            //     PREDICATE <-=1 process "[parent::book]" first
            //       SORT
            //         COLLECT  'parent' 'name' 'node' book
            //           NODE
            //     ELEM Object is a number : 1
            if (*op).ch1 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            }
            CHECK_ERROR0!(ctxt);
            if (*op).ch2 == -1 {
                break 'to_break;
            }

            if (*ctxt).value.is_null() {
                break 'to_break;
            }

            // Hum are we filtering the result of an XPointer expression
            #[cfg(feature = "libxml_xptr_locs")]
            if (*(*ctxt).value).typ == XmlXPathObjectType::XPathLocationset {
                let locset: XmlLocationSetPtr = (*(*ctxt).value).user as _;
                xml_xpath_location_set_filter(ctxt, locset, (*op).ch2, 1, (*locset).loc_nr);
                break 'to_break;
            }

            // In xmlXPathOp::of errors, xmlXPathNodeSetFilter can pop additional
            // nodes from the stack. We have to temporarily remove the
            // nodeset object from the stack to avoid freeing it prematurely.
            CHECK_TYPE0!(ctxt, XmlXPathObjectType::XPathNodeset);
            let obj: XmlXPathObjectPtr = value_pop(ctxt);
            let set: XmlNodeSetPtr = (*obj).nodesetval;
            if !set.is_null() {
                xml_xpath_node_set_filter(
                    ctxt,
                    set,
                    (*op).ch2,
                    1,
                    (*set).node_tab.as_ref().map_or(0, |t| t.len() as i32),
                    1,
                );
            }
            value_push(ctxt, obj);
        }
        XmlXPathOp::XpathOpSort => {
            if (*op).ch1 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            }
            CHECK_ERROR0!(ctxt);
            if !(*ctxt).value.is_null()
                && (*(*ctxt).value).typ == XmlXPathObjectType::XPathNodeset
                && !(*(*ctxt).value).nodesetval.is_null()
                && (*(*(*ctxt).value).nodesetval)
                    .node_tab
                    .as_ref()
                    .map_or(0, |t| t.len())
                    > 1
            {
                xml_xpath_node_set_sort((*(*ctxt).value).nodesetval);
            }
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathOp::XpathOpRangeto => 'to_break: {
            let mut range: XmlXPathObjectPtr;
            let mut res: XmlXPathObjectPtr;
            let mut obj: XmlXPathObjectPtr = null_mut();
            let mut tmp: XmlXPathObjectPtr;
            let mut newlocset: XmlLocationSetPtr = null_mut();
            let oldlocset: XmlLocationSetPtr;
            let oldset: XmlNodeSetPtr;
            let oldnode: XmlNodePtr = (*(*ctxt).context).node;
            let oldcs: i32 = (*(*ctxt).context).context_size;
            let oldpp: i32 = (*(*ctxt).context).proximity_position;

            if (*op).ch1 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
                CHECK_ERROR0!(ctxt);
            }
            if (*ctxt).value.is_null() {
                XP_ERROR0!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
            }
            if (*op).ch2 == -1 {
                break 'to_break;
            }

            'rangeto_error: {
                if (*(*ctxt).value).typ == XmlXPathObjectType::XPathLocationset {
                    /*
                     * Extract the old locset, and then evaluate the result of the
                     * expression for all the element in the locset. use it to grow
                     * up a new locset.
                     */
                    CHECK_TYPE0!(ctxt, XmlXPathObjectType::XPathLocationset);

                    if (*(*ctxt).value).user.is_null()
                        || (*((*(*ctxt).value).user as XmlLocationSetPtr)).loc_nr == 0
                    {
                        break 'to_break;
                    }
                    obj = value_pop(ctxt);
                    oldlocset = (*obj).user as _;

                    newlocset = xml_xptr_location_set_create(null_mut());

                    for i in 0..(*oldlocset).loc_nr {
                        /*
                         * Run the evaluation with a node list made of a
                         * single item in the nodelocset.
                         */
                        (*(*ctxt).context).node =
                            (*(*(*oldlocset).loc_tab.add(i as usize))).user as _;
                        (*(*ctxt).context).context_size = (*oldlocset).loc_nr;
                        (*(*ctxt).context).proximity_position = i + 1;
                        tmp =
                            xml_xpath_cache_new_node_set((*ctxt).context, (*(*ctxt).context).node);
                        value_push(ctxt, tmp);

                        if (*op).ch2 != -1 {
                            total +=
                                xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch2 as usize));
                        }
                        if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                            xml_xptr_free_location_set(newlocset);
                            break 'rangeto_error;
                        }

                        res = value_pop(ctxt);
                        if (*res).typ == XmlXPathObjectType::XPathLocationset {
                            let rloc: XmlLocationSetPtr = (*res).user as XmlLocationSetPtr;
                            for j in 0..(*rloc).loc_nr {
                                range = xml_xptr_new_range(
                                    (*(*(*oldlocset).loc_tab.add(i as usize))).user as _,
                                    (*(*(*oldlocset).loc_tab.add(i as usize))).index,
                                    (*(*(*rloc).loc_tab.add(j as usize))).user2 as _,
                                    (*(*(*rloc).loc_tab.add(j as usize))).index2,
                                );
                                if !range.is_null() {
                                    xml_xptr_location_set_add(newlocset, range);
                                }
                            }
                        } else {
                            range = xml_xptr_new_range_node_object(
                                (*(*(*oldlocset).loc_tab.add(i as usize))).user as XmlNodePtr,
                                res,
                            );
                            if !range.is_null() {
                                xml_xptr_location_set_add(newlocset, range);
                            }
                        }

                        /*
                         * Cleanup
                         */
                        if !res.is_null() {
                            xml_xpath_release_object((*ctxt).context, res);
                        }
                        if (*ctxt).value == tmp {
                            res = value_pop(ctxt);
                            xml_xpath_release_object((*ctxt).context, res);
                        }
                    }
                } else {
                    /* Not a location set */
                    CHECK_TYPE0!(ctxt, XmlXPathObjectType::XPathNodeset);
                    obj = value_pop(ctxt);
                    oldset = (*obj).nodesetval;

                    newlocset = xml_xptr_location_set_create(null_mut());

                    if !oldset.is_null() {
                        for i in 0..(*oldset).node_nr {
                            /*
                             * Run the evaluation with a node list made of a single item
                             * in the nodeset.
                             */
                            (*(*ctxt).context).node = *(*oldset).node_tab.add(i as usize);
                            /*
                             * OPTIMIZE TODO: Avoid recreation for every iteration.
                             */
                            tmp = xml_xpath_cache_new_node_set(
                                (*ctxt).context,
                                (*(*ctxt).context).node,
                            );
                            value_push(ctxt, tmp);

                            if (*op).ch2 != -1 {
                                total += xml_xpath_comp_op_eval(
                                    ctxt,
                                    (*comp).steps.add((*op).ch2 as usize),
                                );
                            }
                            if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                                xml_xptr_free_location_set(newlocset);
                                break 'rangeto_error;
                            }

                            res = value_pop(ctxt);
                            range = xml_xptr_new_range_node_object(
                                *(*oldset).node_tab.add(i as usize),
                                res,
                            );
                            if !range.is_null() {
                                xml_xptr_location_set_add(newlocset, range);
                            }

                            /*
                             * Cleanup
                             */
                            if !res.is_null() {
                                xml_xpath_release_object((*ctxt).context, res);
                            }
                            if (*ctxt).value == tmp {
                                res = value_pop(ctxt);
                                xml_xpath_release_object((*ctxt).context, res);
                            }
                        }
                    }
                }

                /*
                 * The result is used as the new evaluation set.
                 */
                value_push(ctxt, xml_xptr_wrap_location_set(newlocset));
            }
            // rangeto_error:
            xml_xpath_release_object((*ctxt).context, obj);
            (*(*ctxt).context).node = oldnode;
            (*(*ctxt).context).context_size = oldcs;
            (*(*ctxt).context).proximity_position = oldpp;
        } // _ => {
          //     generic_error!(
          //         "XPath: unknown precompiled operation {}\n",
          //         (*op).op
          //     );
          //     (*ctxt).error = XmlXPathError::XpathInvalidOperand as i32;
          // }
    }

    (*(*ctxt).context).depth -= 1;
    total
}

/// Evaluates if the expression evaluates to true.
///
/// Returns 1 if true, 0 if false and -1 on API or internal errors.
#[doc(alias = "xmlXPathCompOpEvalToBoolean")]
unsafe extern "C" fn xml_xpath_comp_op_eval_to_boolean(
    ctxt: XmlXPathParserContextPtr,
    mut op: XmlXPathStepOpPtr,
    is_predicate: i32,
) -> i32 {
    let res_obj: XmlXPathObjectPtr;

    // start:
    loop {
        if OP_LIMIT_EXCEEDED!(ctxt, 1) {
            return 0;
        }
        /* comp = (*ctxt).comp; */
        match (*op).op {
            XmlXPathOp::XpathOpEnd => {
                return 0;
            }
            XmlXPathOp::XpathOpValue => {
                res_obj = (*op).value4 as XmlXPathObjectPtr;
                if is_predicate != 0 {
                    return xml_xpath_evaluate_predicate_result(ctxt, res_obj);
                }
                return xml_xpath_cast_to_boolean(res_obj) as i32;
            }
            XmlXPathOp::XpathOpSort => {
                // We don't need sorting for boolean results. Skip this one.
                if (*op).ch1 != -1 {
                    op = (*(*ctxt).comp).steps.add((*op).ch1 as usize);
                    // goto start;
                    continue;
                }
                return 0;
            }
            XmlXPathOp::XpathOpCollect => {
                if (*op).ch1 == -1 {
                    return 0;
                }

                xml_xpath_comp_op_eval(ctxt, (*(*ctxt).comp).steps.add((*op).ch1 as usize));
                if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                    return -1;
                }

                xml_xpath_node_collect_and_test(ctxt, op, null_mut(), null_mut(), 1);
                if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                    return -1;
                }

                res_obj = value_pop(ctxt);
                if res_obj.is_null() {
                    return -1;
                }
            }
            _ => {
                /*
                 * Fallback to call xmlXPathCompOpEval().
                 */
                xml_xpath_comp_op_eval(ctxt, op);
                if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                    return -1;
                }

                res_obj = value_pop(ctxt);
                if res_obj.is_null() {
                    return -1;
                }
            }
        }
        break;
    }

    if !res_obj.is_null() {
        let res = if (*res_obj).typ == XmlXPathObjectType::XPathBoolean {
            (*res_obj).boolval as i32
        } else if is_predicate != 0 {
            // For predicates a result of type "number" is handled
            // differently:
            // SPEC XPath 1.0:
            // "If the result is a number, the result will be converted
            //  to true if the number is equal to the context position
            //  and will be converted to false otherwise;"
            xml_xpath_evaluate_predicate_result(ctxt, res_obj)
        } else {
            xml_xpath_cast_to_boolean(res_obj) as i32
        };
        xml_xpath_release_object((*ctxt).context, res_obj);
        return res;
    }

    0
}

/// Evaluate the Precompiled XPath expression in the given context.
#[doc(alias = "xmlXPathRunEval")]
pub(crate) unsafe extern "C" fn xml_xpath_run_eval(
    ctxt: XmlXPathParserContextPtr,
    to_bool: i32,
) -> i32 {
    if ctxt.is_null() || (*ctxt).comp.is_null() {
        return -1;
    }

    if (*ctxt).value_tab.is_null() {
        /* Allocate the value stack */
        (*ctxt).value_tab =
            xml_malloc(10 * size_of::<XmlXPathObjectPtr>()) as *mut XmlXPathObjectPtr;
        if (*ctxt).value_tab.is_null() {
            xml_xpath_perr_memory(ctxt, Some("creating evaluation context\n"));
            return -1;
        }
        (*ctxt).value_nr = 0;
        (*ctxt).value_max = 10;
        (*ctxt).value = null_mut();
    }
    #[cfg(feature = "libxml_pattern")]
    if !(*(*ctxt).comp).stream.is_null() {
        let res: i32;

        if to_bool != 0 {
            /*
            	* Evaluation to boolean result.
            	*/
            res = xml_xpath_run_stream_eval((*ctxt).context, (*(*ctxt).comp).stream, null_mut(), 1);
            if res != -1 {
                return res;
            }
        } else {
            let mut res_obj: XmlXPathObjectPtr = null_mut();

            /*
            	* Evaluation to a sequence.
            	*/
            res = xml_xpath_run_stream_eval(
                (*ctxt).context,
                (*(*ctxt).comp).stream,
                addr_of_mut!(res_obj),
                0,
            );

            if res != -1 && !res_obj.is_null() {
                value_push(ctxt, res_obj);
                return 0;
            }
            if !res_obj.is_null() {
                xml_xpath_release_object((*ctxt).context, res_obj);
            }
        }
        /*
        	* QUESTION TODO: This falls back to normal XPath evaluation
        	* if res == -1. Is this intended?
        	*/
    }
    let comp: XmlXPathCompExprPtr = (*ctxt).comp;
    if (*comp).last < 0 {
        generic_error!("xmlXPathRunEval: last is less than zero\n");
        return -1;
    }
    let old_depth: i32 = (*(*ctxt).context).depth;
    if to_bool != 0 {
        return xml_xpath_comp_op_eval_to_boolean(
            ctxt,
            (*comp).steps.add((*comp).last as usize),
            0,
        );
    } else {
        xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*comp).last as usize));
    }
    (*(*ctxt).context).depth = old_depth;

    0
}

/// Parse and evaluate an XPath expression in the given context,
/// then push the result on the context stack
#[doc(alias = "xmlXPathEvalExpr")]
pub unsafe extern "C" fn xml_xpath_eval_expr(ctxt: XmlXPathParserContextPtr) {
    let mut old_depth: i32 = 0;

    if ctxt.is_null() {
        return;
    }

    #[cfg(feature = "libxml_pattern")]
    let comp: XmlXPathCompExprPtr = xml_xpath_try_stream_compile((*ctxt).context, (*ctxt).base);
    #[cfg(feature = "libxml_pattern")]
    let f = !comp.is_null();
    #[cfg(not(feature = "libxml_pattern"))]
    let f = false;

    if f {
        #[cfg(feature = "libxml_pattern")]
        {
            if !(*ctxt).comp.is_null() {
                xml_xpath_free_comp_expr((*ctxt).comp);
            }
            (*ctxt).comp = comp;
        }
    } else {
        if !(*ctxt).context.is_null() {
            old_depth = (*(*ctxt).context).depth;
        }
        xml_xpath_compile_expr(ctxt, 1);
        if !(*ctxt).context.is_null() {
            (*(*ctxt).context).depth = old_depth;
        }
        CHECK_ERROR!(ctxt);

        /* Check for trailing characters. */
        if *(*ctxt).cur != 0 {
            XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
        }

        if (*(*ctxt).comp).nb_step > 1 && (*(*ctxt).comp).last >= 0 {
            if !(*ctxt).context.is_null() {
                old_depth = (*(*ctxt).context).depth;
            }
            xml_xpath_optimize_expression(
                ctxt,
                (*(*ctxt).comp).steps.add((*(*ctxt).comp).last as usize),
            );
            if !(*ctxt).context.is_null() {
                (*(*ctxt).context).depth = old_depth;
            }
        }
    }

    xml_xpath_run_eval(ctxt, 0);
}

macro_rules! COPY_BUF {
    ($l:expr, $b:expr, $i:expr, $v:expr) => {
        if $l == 1 {
            *$b.add($i as usize) = $v as _;
            $i += 1;
        } else {
            $i += xml_copy_char($l, $b.add($i as usize) as _, $v as _);
        }
    };
}

unsafe extern "C" fn xml_xpath_parse_name_complex(
    ctxt: XmlXPathParserContextPtr,
    qualified: i32,
) -> *mut XmlChar {
    let mut buf: [XmlChar; XML_MAX_NAMELEN + 5] = [0; XML_MAX_NAMELEN + 5];
    let mut len: i32 = 0;
    let mut l: i32 = 0;
    let mut c: i32;

    /*
     * Handler for more complex cases
     */
    c = CUR_CHAR!(ctxt, l);
    if c == ' ' as i32
        || c == '>' as i32
        || c == '/' as i32
        || c == '[' as i32
        || c == ']' as i32
        || c == '@' as i32
        || c == '*' as i32  /* accelerators */
        || (!xml_is_letter(c as u32) && c != '_' as i32 && (qualified == 0 || c != ':' as i32))
    {
        return null_mut();
    }

    while c != ' ' as i32
        && c != '>' as i32
        && c != '/' as i32  /* test bigname.xml */
        && (xml_is_letter(c as u32)
            || xml_is_digit(c as u32)
            || c == '.' as i32
            || c == '-' as i32
            || c == '_' as i32
            || (qualified != 0 && c == ':' as i32)
            || xml_is_combining(c as u32)
            || xml_is_extender(c as u32))
    {
        COPY_BUF!(l, buf.as_mut_ptr(), len, c);
        NEXTL!(ctxt, l);
        c = CUR_CHAR!(ctxt, l);
        if len >= XML_MAX_NAMELEN as i32 {
            /*
             * Okay someone managed to make a huge name, so he's ready to pay
             * for the processing speed.
             */
            let mut buffer: *mut XmlChar;
            let mut max: i32 = len * 2;

            if len > XML_MAX_NAME_LENGTH as i32 {
                XP_ERRORNULL!(ctxt, XmlXPathError::XpathExprError as i32);
            }
            buffer = xml_malloc_atomic(max as usize) as *mut XmlChar;
            if buffer.is_null() {
                XP_ERRORNULL!(ctxt, XmlXPathError::XpathMemoryError as i32);
            }
            memcpy(buffer as _, buf.as_ptr() as _, len as usize);
            while xml_is_letter(c as u32)
                || xml_is_digit(c as u32)  /* test bigname.xml */
                || c == '.' as i32
                || c == '-' as i32
                || c == '_' as i32
                || (qualified != 0 && c == ':' as i32)
                || xml_is_combining(c as u32)
                || xml_is_extender(c as u32)
            {
                if len + 10 > max {
                    if max > XML_MAX_NAME_LENGTH as i32 {
                        xml_free(buffer as _);
                        XP_ERRORNULL!(ctxt, XmlXPathError::XpathExprError as i32);
                    }
                    max *= 2;
                    let tmp: *mut XmlChar = xml_realloc(buffer as _, max as usize) as *mut XmlChar;
                    if tmp.is_null() {
                        xml_free(buffer as _);
                        XP_ERRORNULL!(ctxt, XmlXPathError::XpathMemoryError as i32);
                    }
                    buffer = tmp;
                }
                COPY_BUF!(l, buffer, len, c);
                NEXTL!(ctxt, l);
                c = CUR_CHAR!(ctxt, l);
            }
            *buffer.add(len as usize) = 0;
            return buffer;
        }
    }
    if len == 0 {
        return null_mut();
    }
    xml_strndup(buf.as_ptr() as _, len)
}

/// Parse an XML name
///
/// ```ignore
/// [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
///
/// [5] Name ::= (Letter | '_' | ':') (NameChar)*
/// ```
///
/// Returns the namespace name or NULL
#[doc(alias = "xmlXPathParseName")]
pub unsafe extern "C" fn xml_xpath_parse_name(ctxt: XmlXPathParserContextPtr) -> *mut XmlChar {
    let mut input: *const XmlChar;
    let ret: *mut XmlChar;
    let count: usize;

    if ctxt.is_null() || (*ctxt).cur.is_null() {
        return null_mut();
    }
    /*
     * Accelerator for simple ASCII names
     */
    input = (*ctxt).cur;
    if (*input >= 0x61 && *input <= 0x7A)
        || (*input >= 0x41 && *input <= 0x5A)
        || *input == b'_'
        || *input == b':'
    {
        input = input.add(1);
        while (*input >= 0x61 && *input <= 0x7A)
            || (*input >= 0x41 && *input <= 0x5A)
            || (*input >= 0x30 && *input <= 0x39)
            || *input == b'_'
            || *input == b'-'
            || *input == b':'
            || *input == b'.'
        {
            input = input.add(1);
        }
        if *input > 0 && *input < 0x80 {
            count = input.offset_from((*ctxt).cur) as _;
            if count > XML_MAX_NAME_LENGTH {
                (*ctxt).cur = input;
                XP_ERRORNULL!(ctxt, XmlXPathError::XpathExprError as i32);
            }
            ret = xml_strndup((*ctxt).cur, count as _);
            (*ctxt).cur = input;
            return ret;
        }
    }
    xml_xpath_parse_name_complex(ctxt, 1)
}

/// Parse an XML namespace non qualified name.
///
/// ```ignore
/// [NS 3] NCName ::= (Letter | '_') (NCNameChar)*
///
/// [NS 4] NCNameChar ::= Letter | Digit | '.' | '-' | '_' | CombiningChar | Extender
/// ```
///
/// Returns the namespace name or NULL
#[doc(alias = "xmlXPathParseNCName")]
pub unsafe extern "C" fn xml_xpath_parse_ncname(ctxt: XmlXPathParserContextPtr) -> *mut XmlChar {
    let mut input: *const XmlChar;
    let ret: *mut XmlChar;
    let count: i32;

    if ctxt.is_null() || (*ctxt).cur.is_null() {
        return null_mut();
    }
    /*
     * Accelerator for simple ASCII names
     */
    input = (*ctxt).cur;
    if (*input >= 0x61 && *input <= 0x7A) || (*input >= 0x41 && *input <= 0x5A) || *input == b'_' {
        input = input.add(1);
        while (*input >= 0x61 && *input <= 0x7A)
            || (*input >= 0x41 && *input <= 0x5A)
            || (*input >= 0x30 && *input <= 0x39)
            || *input == b'_'
            || *input == b'.'
            || *input == b'-'
        {
            input = input.add(1);
        }
        if *input == b' '
            || *input == b'>'
            || *input == b'/'
            || *input == b'['
            || *input == b']'
            || *input == b':'
            || *input == b'@'
            || *input == b'*'
        {
            count = input.offset_from((*ctxt).cur) as _;
            if count == 0 {
                return null_mut();
            }
            ret = xml_strndup((*ctxt).cur, count);
            (*ctxt).cur = input;
            return ret;
        }
    }
    xml_xpath_parse_name_complex(ctxt, 0)
}

const MAX_FRAC: usize = 20;

/// ```ignore
/// [30a]  Float  ::= Number ('e' Digits?)?
///
/// [30]   Number ::=   Digits ('.' Digits?)?
///                    | '.' Digits
/// [31]   Digits ::=   [0-9]+
/// ```
///
/// Compile a Number in the string
/// In complement of the Number expression, this function also handles
/// negative values : '-' Number.
///
/// Returns the let value: f64.
#[doc(alias = "xmlXPathStringEvalNumber")]
pub unsafe extern "C" fn xml_xpath_string_eval_number(str: *const XmlChar) -> f64 {
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
pub unsafe extern "C" fn xml_xpath_evaluate_predicate_result(
    ctxt: XmlXPathParserContextPtr,
    res: XmlXPathObjectPtr,
) -> i32 {
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
            if (*res).nodesetval.is_null() {
                return 0;
            }
            return (*(*res).nodesetval)
                .node_tab
                .as_ref()
                .map_or(false, |t| !t.is_empty()) as i32;
        }
        XmlXPathObjectType::XPathString => {
            return (*res).stringval.as_deref().map_or(false, |s| !s.is_empty()) as i32;
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathLocationset => {
            let ptr: XmlLocationSetPtr = (*res).user as XmlLocationSetPtr;
            if ptr.is_null() {
                return 0;
            }
            return ((*ptr).loc_nr != 0) as i32;
        }
        _ => {
            generic_error!("Internal error at {}:{}\n", file!(), line!());
        }
    }
    0
}

/// This is the cached version of xmlXPathNewCString().
/// Acquire an xmlXPathObjectPtr of type string and of value @val
///
/// Returns the created or reused object.
#[doc(alias = "xmlXPathCacheNewCString")]
unsafe extern "C" fn xml_xpath_cache_new_cstring(
    ctxt: XmlXPathContextPtr,
    val: *const c_char,
) -> XmlXPathObjectPtr {
    xml_xpath_cache_new_string(ctxt, val as _)
}

/// This is the cached version of xmlXPathWrapString().
/// Wraps the @val string into an XPath object.
///
/// Returns the created or reused object.
#[doc(alias = "xmlXPathCacheWrapString")]
unsafe extern "C" fn xml_xpath_cache_wrap_string(
    ctxt: XmlXPathContextPtr,
    val: *mut XmlChar,
) -> XmlXPathObjectPtr {
    if !ctxt.is_null() && !(*ctxt).cache.is_null() {
        let cache: XmlXpathContextCachePtr = (*ctxt).cache as XmlXpathContextCachePtr;

        if !(*cache).string_objs.is_null() && (*(*cache).string_objs).number != 0 {
            (*(*cache).string_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).string_objs)
                .items
                .add((*(*cache).string_objs).number as usize)
                as XmlXPathObjectPtr;
            (*ret).typ = XmlXPathObjectType::XPathString;
            (*ret).stringval = (!val.is_null()).then(|| {
                CStr::from_ptr(val as *const i8)
                    .to_string_lossy()
                    .into_owned()
            });
            if !val.is_null() {
                xml_free(val as _);
            }
            return ret;
        } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
            // Fallback to misc-cache.
            (*(*cache).misc_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                .items
                .add((*(*cache).misc_objs).number as usize)
                as XmlXPathObjectPtr;

            (*ret).typ = XmlXPathObjectType::XPathString;
            (*ret).stringval = (!val.is_null()).then(|| {
                CStr::from_ptr(val as *const i8)
                    .to_string_lossy()
                    .into_owned()
            });
            if !val.is_null() {
                xml_free(val as _);
            }
            return ret;
        }
    }
    xml_xpath_wrap_string(val)
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
unsafe fn xml_xpath_name_function(ctxt: XmlXPathParserContextPtr, mut nargs: i32) {
    if nargs == 0 {
        value_push(
            ctxt,
            xml_xpath_cache_new_node_set((*ctxt).context, (*(*ctxt).context).node),
        );
        nargs = 1;
    }

    CHECK_ARITY!(ctxt, nargs, 1);
    if (*ctxt).value.is_null()
        || !matches!(
            (*(*ctxt).value).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }
    let cur: XmlXPathObjectPtr = value_pop(ctxt);

    if (*cur).nodesetval.is_null() {
        value_push(
            ctxt,
            xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
        );
    } else if let Some(table) = (*(*cur).nodesetval)
        .node_tab
        .as_ref()
        .filter(|t| !t.is_empty())
    {
        let i = 0; /* Should be first in document order !!!!! */

        match (*table[i]).element_type() {
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
                if *(*table[i]).name.add(0) == b' ' {
                    value_push(
                        ctxt,
                        xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
                    );
                } else if (*table[i]).ns.is_null() || (*(*table[i]).ns).prefix.is_null() {
                    value_push(
                        ctxt,
                        xml_xpath_cache_new_string((*ctxt).context, (*table[i]).name),
                    );
                } else {
                    let mut fullname: *mut XmlChar;

                    fullname =
                        xml_build_qname((*table[i]).name, (*(*table[i]).ns).prefix, null_mut(), 0);
                    if fullname == (*table[i]).name as _ {
                        fullname = xml_strdup((*table[i]).name);
                    }
                    if fullname.is_null() {
                        xml_xpath_perr_memory(ctxt, None);
                    }
                    value_push(ctxt, xml_xpath_cache_wrap_string((*ctxt).context, fullname));
                }
            }
            _ => {
                value_push(
                    ctxt,
                    xml_xpath_cache_new_node_set((*ctxt).context, table[i]),
                );
                xml_xpath_local_name_function(ctxt, 1);
            }
        }
    } else {
        value_push(
            ctxt,
            xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
        );
    }
    xml_xpath_release_object((*ctxt).context, cur);
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
unsafe fn xml_xpath_escape_uri_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    let mut escape: [XmlChar; 4] = [0; 4];

    CHECK_ARITY!(ctxt, nargs, 2);

    let escape_reserved: i32 = xml_xpath_pop_boolean(ctxt);

    CAST_TO_STRING!(ctxt);
    let str: XmlXPathObjectPtr = value_pop(ctxt);

    let target: XmlBufPtr = xml_buf_create();

    escape[0] = b'%';
    escape[3] = 0;

    if !target.is_null() {
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
                xml_buf_add(target, cptr.as_ptr(), 1);
            } else {
                if c >> 4 < 10 {
                    escape[1] = b'0' + (c >> 4);
                } else {
                    escape[1] = b'A' - 10 + (c >> 4);
                }
                if c & 0xF < 10 {
                    escape[2] = b'0' + (c & 0xF);
                } else {
                    escape[2] = b'A' - 10 + (c & 0xF);
                }

                xml_buf_add(target, &escape[0], 3);
            }
        }
    }
    value_push(
        ctxt,
        xml_xpath_cache_new_string((*ctxt).context, xml_buf_content(target)),
    );
    xml_buf_free(target);
    xml_xpath_release_object((*ctxt).context, str);
}

/// Registers all default XPath functions in this context
#[doc(alias = "xmlXPathRegisterAllFunctions")]
pub unsafe extern "C" fn xml_xpath_register_all_functions(ctxt: XmlXPathContextPtr) {
    xml_xpath_register_func(ctxt, c"boolean".as_ptr() as _, xml_xpath_boolean_function);
    xml_xpath_register_func(ctxt, c"ceiling".as_ptr() as _, xml_xpath_ceiling_function);
    xml_xpath_register_func(ctxt, c"count".as_ptr() as _, xml_xpath_count_function);
    xml_xpath_register_func(ctxt, c"concat".as_ptr() as _, xml_xpath_concat_function);
    xml_xpath_register_func(ctxt, c"contains".as_ptr() as _, xml_xpath_contains_function);
    xml_xpath_register_func(ctxt, c"id".as_ptr() as _, xml_xpath_id_function);
    xml_xpath_register_func(ctxt, c"false".as_ptr() as _, xml_xpath_false_function);
    xml_xpath_register_func(ctxt, c"floor".as_ptr() as _, xml_xpath_floor_function);
    xml_xpath_register_func(ctxt, c"last".as_ptr() as _, xml_xpath_last_function);
    xml_xpath_register_func(ctxt, c"lang".as_ptr() as _, xml_xpath_lang_function);
    xml_xpath_register_func(
        ctxt,
        c"local-name".as_ptr() as _,
        xml_xpath_local_name_function,
    );
    xml_xpath_register_func(ctxt, c"not".as_ptr() as _, xml_xpath_not_function);
    xml_xpath_register_func(ctxt, c"name".as_ptr() as _, xml_xpath_name_function);
    xml_xpath_register_func(
        ctxt,
        c"namespace-uri".as_ptr() as _,
        xml_xpath_namespace_uri_function,
    );
    xml_xpath_register_func(
        ctxt,
        c"normalize-space".as_ptr() as _,
        xml_xpath_normalize_function,
    );
    xml_xpath_register_func(ctxt, c"number".as_ptr() as _, xml_xpath_number_function);
    xml_xpath_register_func(ctxt, c"position".as_ptr() as _, xml_xpath_position_function);
    xml_xpath_register_func(ctxt, c"round".as_ptr() as _, xml_xpath_round_function);
    xml_xpath_register_func(ctxt, c"string".as_ptr() as _, xml_xpath_string_function);
    xml_xpath_register_func(
        ctxt,
        c"string-length".as_ptr() as _,
        xml_xpath_string_length_function,
    );
    xml_xpath_register_func(
        ctxt,
        c"starts-with".as_ptr() as _,
        xml_xpath_starts_with_function,
    );
    xml_xpath_register_func(
        ctxt,
        c"substring".as_ptr() as _,
        xml_xpath_substring_function,
    );
    xml_xpath_register_func(
        ctxt,
        c"substring-before".as_ptr() as _,
        xml_xpath_substring_before_function,
    );
    xml_xpath_register_func(
        ctxt,
        c"substring-after".as_ptr() as _,
        xml_xpath_substring_after_function,
    );
    xml_xpath_register_func(ctxt, c"sum".as_ptr() as _, xml_xpath_sum_function);
    xml_xpath_register_func(ctxt, c"true".as_ptr() as _, xml_xpath_true_function);
    xml_xpath_register_func(
        ctxt,
        c"translate".as_ptr() as _,
        xml_xpath_translate_function,
    );

    xml_xpath_register_func_ns(
        ctxt,
        c"escape-uri".as_ptr() as _,
        c"http://www.w3.org/2002/08/xquery-functions".as_ptr() as _,
        Some(xml_xpath_escape_uri_function),
    );
}

/// Merges two nodesets, all nodes from @val2 are added to @val1
/// if @val1 is NULL, a new set is created and copied from @val2
///
/// Returns @val1 once extended or NULL in case of error.
///
/// Frees @val1 in case of error.
#[doc(alias = "xmlXPathNodeSetMerge")]
pub unsafe extern "C" fn xml_xpath_node_set_merge(
    mut val1: XmlNodeSetPtr,
    val2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let mut skip: i32;

    if val2.is_null() {
        return val1;
    }
    if val1.is_null() {
        val1 = xml_xpath_node_set_create(null_mut());
        if val1.is_null() {
            return null_mut();
        }
    }

    // @@ with_ns to check whether namespace nodes should be looked at @@
    let init_nr = (*val1).node_tab.as_ref().map_or(0, |t| t.len());

    if let Some(table) = (*val2).node_tab.as_ref() {
        for &n2 in table {
            // check against duplicates
            skip = 0;
            let val1_table = (*val1).node_tab.get_or_insert_with(Vec::new);
            for &n1 in &val1_table[..init_nr] {
                if n1 == n2
                    || ((matches!((*n1).element_type(), XmlElementType::XmlNamespaceDecl)
                        && matches!((*n2).element_type(), XmlElementType::XmlNamespaceDecl))
                        && ((*(n1 as XmlNsPtr)).next == (*(n2 as XmlNsPtr)).next
                            && xml_str_equal(
                                (*(n1 as XmlNsPtr)).prefix as _,
                                (*(n2 as XmlNsPtr)).prefix as _,
                            )))
                {
                    skip = 1;
                    break;
                }
            }
            if skip != 0 {
                continue;
            }

            // grow the nodeTab if needed
            if val1_table.len() >= XPATH_MAX_NODESET_LENGTH {
                xml_xpath_err_memory(null_mut(), Some("merging nodeset hit limit\n"));
                // goto error;
                xml_xpath_free_node_set(val1);
                return null_mut();
            }
            if matches!((*n2).element_type(), XmlElementType::XmlNamespaceDecl) {
                let ns: XmlNsPtr = n2 as XmlNsPtr;
                let ns_node: XmlNodePtr = xml_xpath_node_set_dup_ns((*ns).next as XmlNodePtr, ns);

                if ns_node.is_null() {
                    // goto error;
                    xml_xpath_free_node_set(val1);
                    return null_mut();
                }
                val1_table.push(ns_node);
            } else {
                val1_table.push(n2);
            }
        }
    }

    val1

    // error:
    // xmlXPathFreeNodeSet(val1);
    // return null_mut();
}

/// Removes an xmlNodePtr from an existing NodeSet
#[doc(alias = "xmlXPathNodeSetDel")]
pub unsafe extern "C" fn xml_xpath_node_set_del(cur: XmlNodeSetPtr, val: XmlNodePtr) {
    if cur.is_null() {
        return;
    }
    if val.is_null() {
        return;
    }

    // find node in nodeTab
    if let Some(table) = (*cur).node_tab.as_mut() {
        let Some(pos) = table.iter().position(|&node| node == val) else {
            return;
        };
        if !table[pos].is_null()
            && matches!(
                (*table[pos]).element_type(),
                XmlElementType::XmlNamespaceDecl
            )
        {
            xml_xpath_node_set_free_ns(table[pos] as XmlNsPtr);
        }
        table.remove(pos);
    }
}

/// Removes an entry from an existing NodeSet list.
#[doc(alias = "xmlXPathNodeSetRemove")]
pub unsafe extern "C" fn xml_xpath_node_set_remove(cur: XmlNodeSetPtr, val: i32) {
    if cur.is_null() {
        return;
    }
    if let Some(table) = (*cur).node_tab.as_mut() {
        if val >= table.len() as i32 {
            return;
        }
        if !table[val as usize].is_null()
            && matches!(
                (*(table[val as usize])).element_type(),
                XmlElementType::XmlNamespaceDecl
            )
        {
            xml_xpath_node_set_free_ns(table[val as usize] as XmlNsPtr);
        }
        table.remove(val as usize);
    }
}

/// Create a new xmlXPathObjectPtr of type NodeSet and initialize
/// it with the Nodeset @val
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathNewNodeSetList")]
pub unsafe extern "C" fn xml_xpath_new_node_set_list(val: XmlNodeSetPtr) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr;

    if val.is_null() {
        ret = null_mut();
    } else if let Some(table) = (*val).node_tab.as_ref() {
        ret = xml_xpath_new_node_set(table[0]);
        if !ret.is_null() {
            for &node in &table[1..] {
                // TODO: Propagate memory error.
                if xml_xpath_node_set_add_unique((*ret).nodesetval, node) < 0 {
                    break;
                }
            }
        }
    } else {
        ret = xml_xpath_new_node_set(null_mut());
    }

    ret
}

/// Wrap the Nodeset @val in a new xmlXPathObjectPtr
///
/// Returns the newly created object.
///
/// In case of error the node set is destroyed and NULL is returned.
#[doc(alias = "xmlXPathWrapNodeSet")]
pub unsafe extern "C" fn xml_xpath_wrap_node_set(val: XmlNodeSetPtr) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), Some("creating node set object\n"));
        xml_xpath_free_node_set(val);
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlXPathObject::default());
    (*ret).typ = XmlXPathObjectType::XPathNodeset;
    (*ret).nodesetval = val;
    ret
}

/// Wraps the @val data into an XPath object.
///
/// Returns the newly created object.
#[doc(alias = "xmlXPathWrapExternal")]
pub unsafe extern "C" fn xml_xpath_wrap_external(val: *mut c_void) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), Some("creating user object\n"));
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlXPathObject::default());
    (*ret).typ = XmlXPathObjectType::XPathUsers;
    (*ret).user = val;
    ret
}

/// Function computing the beginning of the string value of the node,
/// used to speed up comparisons
///
/// Returns an int usable as a hash
#[doc(alias = "xmlXPathNodeValHash")]
unsafe extern "C" fn xml_xpath_node_val_hash(mut node: XmlNodePtr) -> u32 {
    let mut len: i32 = 2;
    let mut string: *const XmlChar;
    let mut tmp: XmlNodePtr;
    let mut ret: u32 = 0;

    if node.is_null() {
        return 0;
    }

    if matches!((*node).element_type(), XmlElementType::XmlDocumentNode) {
        tmp = (*node)
            .as_document_node()
            .unwrap()
            .as_ref()
            .get_root_element();
        if tmp.is_null() {
            node = (*node).children().map_or(null_mut(), |c| c.as_ptr());
        } else {
            node = tmp;
        }

        if node.is_null() {
            return 0;
        }
    }

    match (*node).element_type() {
        XmlElementType::XmlCommentNode
        | XmlElementType::XmlPINode
        | XmlElementType::XmlCDATASectionNode
        | XmlElementType::XmlTextNode => {
            string = (*node).content;
            if string.is_null() {
                return 0;
            }
            if *string.add(0) == 0 {
                return 0;
            }
            return *string.add(0) as u32 + ((*string.add(1) as u32) << 8);
        }
        XmlElementType::XmlNamespaceDecl => {
            string = (*(node as XmlNsPtr)).href;
            if string.is_null() {
                return 0;
            }
            if *string.add(0) == 0 {
                return 0;
            }
            return *string.add(0) as u32 + ((*string.add(1) as u32) << 8);
        }
        XmlElementType::XmlAttributeNode => {
            tmp = (*node)
                .as_attribute_node()
                .unwrap()
                .as_ref()
                .children
                .map_or(null_mut(), |c| c.as_ptr());
        }
        XmlElementType::XmlElementNode => {
            tmp = (*node).children().map_or(null_mut(), |c| c.as_ptr());
        }
        _ => {
            return 0;
        }
    }
    while !tmp.is_null() {
        match (*tmp).element_type() {
            XmlElementType::XmlCDATASectionNode | XmlElementType::XmlTextNode => {
                string = (*tmp).content;
            }
            _ => {
                string = null_mut();
            }
        }
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
        if let Some(children) = (*tmp).children().filter(|children| {
            !matches!((*tmp).element_type(), XmlElementType::XmlDTDNode)
                && !matches!(children.element_type(), XmlElementType::XmlEntityDecl)
        }) {
            tmp = children.as_ptr();
            continue;
        }
        if tmp == node {
            break;
        }

        if let Some(next) = (*tmp).next {
            tmp = next.as_ptr();
            continue;
        }

        loop {
            tmp = (*tmp).parent().map_or(null_mut(), |p| p.as_ptr());
            if tmp.is_null() {
                break;
            }
            if tmp == node {
                tmp = null_mut();
                break;
            }
            if let Some(next) = (*tmp).next() {
                tmp = next.as_ptr();
                break;
            }

            if tmp.is_null() {
                break;
            }
        }
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
unsafe extern "C" fn xml_xpath_equal_node_sets(
    arg1: XmlXPathObjectPtr,
    arg2: XmlXPathObjectPtr,
    neq: i32,
) -> i32 {
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

    let ns1: XmlNodeSetPtr = (*arg1).nodesetval;
    let ns2: XmlNodeSetPtr = (*arg2).nodesetval;

    if ns1.is_null() {
        return 0;
    }
    let Some(ns1_table) = (*ns1).node_tab.as_ref().filter(|t| !t.is_empty()) else {
        return 0;
    };
    if ns2.is_null() {
        return 0;
    }
    let Some(ns2_table) = (*ns2).node_tab.as_ref().filter(|t| !t.is_empty()) else {
        return 0;
    };

    // for equal, check if there is a node pertaining to both sets
    if neq == 0 {
        for &node1 in ns1_table {
            for &node2 in ns2_table {
                if node1 == node2 {
                    return 1;
                }
            }
        }
    }

    let mut values1 = vec![None; ns1_table.len()];
    let hashs1: *mut u32 = xml_malloc(ns1_table.len() * size_of::<u32>()) as *mut u32;
    if hashs1.is_null() {
        // TODO: Propagate memory error.
        xml_xpath_err_memory(null_mut(), Some("comparing nodesets\n"));
        return 0;
    }
    let mut values2 = vec![None; ns2_table.len()];
    let hashs2: *mut u32 = xml_malloc(ns2_table.len() * size_of::<u32>()) as *mut u32;
    if hashs2.is_null() {
        // TODO: Propagate memory error.
        xml_xpath_err_memory(null_mut(), Some("comparing nodesets\n"));
        xml_free(hashs1 as _);
        return 0;
    }
    for (i, &node1) in ns1_table.iter().enumerate() {
        *hashs1.add(i) = xml_xpath_node_val_hash(node1);
        for (j, &node2) in ns2_table.iter().enumerate() {
            if i == 0 {
                *hashs2.add(j) = xml_xpath_node_val_hash(node2);
            }
            if *hashs1.add(i) != *hashs2.add(j) {
                if neq != 0 {
                    ret = 1;
                    break;
                }
            } else {
                if values1[i].is_none() {
                    values1[i] = (*node1).get_content();
                }
                if values2[j].is_none() {
                    values2[j] = (*node2).get_content();
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

/// Implement the equal operation on XPath objects content: @arg1 == @arg2
/// If one object to be compared is a node-set and the other is a number,
/// then the comparison will be true if and only if there is a node in
/// the node-set such that the result of performing the comparison on the
/// number to be compared and on the result of converting the string-value
/// of that node to a number using the number function is true.
///
/// Returns 0 or 1 depending on the results of the test.
#[doc(alias = "xmlXPathEqualNodeSetFloat")]
unsafe extern "C" fn xml_xpath_equal_node_set_float(
    ctxt: XmlXPathParserContextPtr,
    arg: XmlXPathObjectPtr,
    f: f64,
    neq: i32,
) -> i32 {
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

    let ns: XmlNodeSetPtr = (*arg).nodesetval;
    if !ns.is_null() {
        if let Some(table) = (*ns).node_tab.as_ref() {
            for &node in table {
                let str2 = xml_xpath_cast_node_to_string(node);
                let str2 = CString::new(str2).unwrap();
                value_push(
                    ctxt,
                    xml_xpath_cache_new_string((*ctxt).context, str2.as_ptr() as *const u8),
                );
                xml_xpath_number_function(ctxt, 1);
                CHECK_ERROR0!(ctxt);
                val = value_pop(ctxt);
                v = (*val).floatval;
                xml_xpath_release_object((*ctxt).context, val);
                if !xml_xpath_is_nan(v) {
                    if (neq == 0 && v == f) || (neq != 0 && v != f) {
                        ret = 1;
                        break;
                    }
                } else {
                    /* NaN is unequal to any value */
                    if neq != 0 {
                        ret = 1;
                    }
                }
            }
        }
    }

    ret
}

/// Function computing the beginning of the string value of the node,
/// used to speed up comparisons
///
/// Returns an int usable as a hash
#[doc(alias = "xmlXPathStringHash")]
unsafe extern "C" fn xml_xpath_string_hash(string: *const XmlChar) -> u32 {
    if string.is_null() {
        return 0;
    }
    if *string.add(0) == 0 {
        return 0;
    }
    *string.add(0) as u32 + ((*string.add(1) as u32) << 8)
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
    if arg.is_null()
        || !matches!(
            (*arg).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        )
    {
        return 0;
    }
    let ns: XmlNodeSetPtr = (*arg).nodesetval;
    // A NULL nodeset compared with a string is always false
    // (since there is no node equal, and no node not equal)
    if ns.is_null() || (*ns).node_tab.as_ref().map_or(true, |t| t.is_empty()) {
        return 0;
    }
    let cstr = CString::new(str).unwrap();
    let hash: u32 = xml_xpath_string_hash(cstr.as_ptr() as *const u8);
    if let Some(table) = (*ns).node_tab.as_ref() {
        for &node in table {
            if xml_xpath_node_val_hash(node) == hash {
                let str2 = (*node).get_content();
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
    }
    0
}

unsafe extern "C" fn xml_xpath_equal_values_common(
    ctxt: XmlXPathParserContextPtr,
    mut arg1: XmlXPathObjectPtr,
    mut arg2: XmlXPathObjectPtr,
) -> i32 {
    let mut ret: i32 = 0;
    /*
     *At this point we are assured neither arg1 nor arg2
     *is a nodeset, so we can just pick the appropriate routine.
     */
    match (*arg1).typ {
        XmlXPathObjectType::XPathUndefined => {}
        XmlXPathObjectType::XPathBoolean => match (*arg2).typ {
            XmlXPathObjectType::XPathUndefined => {}
            XmlXPathObjectType::XPathBoolean => {
                ret = ((*arg1).boolval == (*arg2).boolval) as i32;
            }
            XmlXPathObjectType::XPathNumber => {
                ret =
                    ((*arg1).boolval == xml_xpath_cast_number_to_boolean((*arg2).floatval)) as i32;
            }
            XmlXPathObjectType::XPathString => {
                let f = (*arg2)
                    .stringval
                    .as_deref()
                    .map_or(false, |s| !s.is_empty());
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
                    ret = ((*arg2).boolval == xml_xpath_cast_number_to_boolean((*arg1).floatval))
                        as i32;
                }

                ty @ XmlXPathObjectType::XPathString
                | ty @ XmlXPathObjectType::XPathNumber => 'to_break: {
                    if matches!(ty, XmlXPathObjectType::XPathString) {
                        value_push(ctxt, arg2);
                        xml_xpath_number_function(ctxt, 1);
                        arg2 = value_pop(ctxt);
                        if (*ctxt).error != 0 {
                            break 'to_break;
                        }
                        /* Falls through. */
                    }

                    /* Hand check NaN and Infinity equalities */
                    if xml_xpath_is_nan((*arg1).floatval) || xml_xpath_is_nan((*arg2).floatval) {
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
                    let f = (*arg1)
                        .stringval
                        .as_deref()
                        .map_or(false, |s| !s.is_empty());
                    ret = ((*arg2).boolval == f) as i32;
                }
                XmlXPathObjectType::XPathString => {
                    ret = ((*arg1).stringval == (*arg2).stringval) as i32;
                }
                XmlXPathObjectType::XPathNumber => {
                    value_push(ctxt, arg1);
                    xml_xpath_number_function(ctxt, 1);
                    arg1 = value_pop(ctxt);
                    if (*ctxt).error != 0 {
                        // break;
                    } else {
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

/// Implement the equal operation on XPath objects content: @arg1 == @arg2
///
/// Returns 0 or 1 depending on the results of the test.
#[doc(alias = "xmlXPathEqualValues")]
pub unsafe extern "C" fn xml_xpath_equal_values(ctxt: XmlXPathParserContextPtr) -> i32 {
    let mut arg1: XmlXPathObjectPtr;
    let mut arg2: XmlXPathObjectPtr;
    let argtmp: XmlXPathObjectPtr;
    let mut ret: i32 = 0;

    if ctxt.is_null() || (*ctxt).context.is_null() {
        return 0;
    }
    arg2 = value_pop(ctxt);
    arg1 = value_pop(ctxt);
    if arg1.is_null() || arg2.is_null() {
        if !arg1.is_null() {
            xml_xpath_release_object((*ctxt).context, arg1);
        } else {
            xml_xpath_release_object((*ctxt).context, arg2);
        }
        XP_ERROR0!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }

    if arg1 == arg2 {
        xml_xpath_free_object(arg1);
        return 1;
    }

    /*
     *If either argument is a nodeset, it's a 'special case'
     */
    if matches!(
        (*arg2).typ,
        XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
    ) || matches!(
        (*arg1).typ,
        XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
    ) {
        /*
         *Hack it to assure arg1 is the nodeset
         */
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
                if (*arg1).nodesetval.is_null()
                    || (*(*arg1).nodesetval)
                        .node_tab
                        .as_ref()
                        .map_or(true, |t| t.is_empty())
                {
                    ret = 0;
                } else {
                    ret = 1;
                }
                ret = (ret == (*arg2).boolval as i32) as i32;
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

/// Implement the equal operation on XPath objects content: @arg1 == @arg2
///
/// Returns 0 or 1 depending on the results of the test.
#[doc(alias = "xmlXPathNotEqualValues")]
pub unsafe extern "C" fn xml_xpath_not_equal_values(ctxt: XmlXPathParserContextPtr) -> i32 {
    let mut arg1: XmlXPathObjectPtr;
    let mut arg2: XmlXPathObjectPtr;
    let argtmp: XmlXPathObjectPtr;
    let mut ret: i32 = 0;

    if ctxt.is_null() || (*ctxt).context.is_null() {
        return 0;
    }
    arg2 = value_pop(ctxt);
    arg1 = value_pop(ctxt);
    if arg1.is_null() || arg2.is_null() {
        if !arg1.is_null() {
            xml_xpath_release_object((*ctxt).context, arg1);
        } else {
            xml_xpath_release_object((*ctxt).context, arg2);
        }
        XP_ERROR0!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }

    if arg1 == arg2 {
        xml_xpath_release_object((*ctxt).context, arg1);
        return 0;
    }

    /*
     *If either argument is a nodeset, it's a 'special case'
     */
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
                if (*arg1).nodesetval.is_null()
                    || (*(*arg1).nodesetval)
                        .node_tab
                        .as_ref()
                        .map_or(true, |t| t.is_empty())
                {
                    ret = 0;
                } else {
                    ret = 1;
                }
                ret = (ret != (*arg2).boolval as i32) as i32;
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
unsafe extern "C" fn xml_xpath_compare_node_sets(
    inf: i32,
    strict: i32,
    arg1: XmlXPathObjectPtr,
    arg2: XmlXPathObjectPtr,
) -> i32 {
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

    let ns1: XmlNodeSetPtr = (*arg1).nodesetval;
    let ns2: XmlNodeSetPtr = (*arg2).nodesetval;

    if ns1.is_null() {
        xml_xpath_free_object(arg1);
        xml_xpath_free_object(arg2);
        return 0;
    }
    let Some(ns1_table) = (*ns1).node_tab.as_ref().filter(|t| !t.is_empty()) else {
        xml_xpath_free_object(arg1);
        xml_xpath_free_object(arg2);
        return 0;
    };
    if ns2.is_null() {
        xml_xpath_free_object(arg1);
        xml_xpath_free_object(arg2);
        return 0;
    }
    let Some(ns2_table) = (*ns2).node_tab.as_ref().filter(|t| !t.is_empty()) else {
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
        val1 = xml_xpath_cast_node_to_number(node1);
        if xml_xpath_is_nan(val1) {
            continue;
        }
        for (j, &node2) in ns2_table.iter().enumerate() {
            if init == 0 {
                *values2.add(j) = xml_xpath_cast_node_to_number(node2);
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
unsafe extern "C" fn xml_xpath_compare_node_set_float(
    ctxt: XmlXPathParserContextPtr,
    inf: i32,
    strict: i32,
    arg: XmlXPathObjectPtr,
    f: XmlXPathObjectPtr,
) -> i32 {
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
    let ns: XmlNodeSetPtr = (*arg).nodesetval;
    if !ns.is_null() {
        if let Some(table) = (*ns).node_tab.as_ref() {
            for &node in table {
                let str2 = xml_xpath_cast_node_to_string(node);
                let str2 = CString::new(str2).unwrap();
                value_push(
                    ctxt,
                    xml_xpath_cache_new_string((*ctxt).context, str2.as_ptr() as *const u8),
                );
                xml_xpath_number_function(ctxt, 1);
                value_push(ctxt, xml_xpath_cache_object_copy((*ctxt).context, f));
                ret = xml_xpath_compare_values(ctxt, inf, strict);
                if ret != 0 {
                    break;
                }
            }
        }
    }
    xml_xpath_release_object((*ctxt).context, arg);
    xml_xpath_release_object((*ctxt).context, f);
    ret
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
unsafe extern "C" fn xml_xpath_compare_node_set_string(
    ctxt: XmlXPathParserContextPtr,
    inf: i32,
    strict: i32,
    arg: XmlXPathObjectPtr,
    s: XmlXPathObjectPtr,
) -> i32 {
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
    let ns: XmlNodeSetPtr = (*arg).nodesetval;
    if !ns.is_null() {
        if let Some(table) = (*ns).node_tab.as_ref() {
            for &node in table {
                let str2 = xml_xpath_cast_node_to_string(node);
                let str2 = CString::new(str2).unwrap();
                value_push(
                    ctxt,
                    xml_xpath_cache_new_string((*ctxt).context, str2.as_ptr() as *const u8),
                );
                value_push(ctxt, xml_xpath_cache_object_copy((*ctxt).context, s));
                ret = xml_xpath_compare_values(ctxt, inf, strict);
                if ret != 0 {
                    break;
                }
            }
        }
    }
    xml_xpath_release_object((*ctxt).context, arg);
    xml_xpath_release_object((*ctxt).context, s);
    ret
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
unsafe extern "C" fn xml_xpath_compare_node_set_value(
    ctxt: XmlXPathParserContextPtr,
    inf: i32,
    strict: i32,
    arg: XmlXPathObjectPtr,
    val: XmlXPathObjectPtr,
) -> i32 {
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
            value_push(ctxt, arg);
            xml_xpath_boolean_function(ctxt, 1);
            value_push(ctxt, val);
            xml_xpath_compare_values(ctxt, inf, strict)
        }
        _ => {
            generic_error!(
                "xmlXPathCompareNodeSetValue: Can't compare node set and object of type {:?}\n",
                (*val).typ
            );
            xml_xpath_release_object((*ctxt).context, arg);
            xml_xpath_release_object((*ctxt).context, val);
            XP_ERROR0!(ctxt, XmlXPathError::XpathInvalidType as i32);
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
pub unsafe extern "C" fn xml_xpath_compare_values(
    ctxt: XmlXPathParserContextPtr,
    inf: i32,
    strict: i32,
) -> i32 {
    let mut ret: i32 = 0;
    let arg1i: i32;
    let arg2i: i32;
    let mut arg1: XmlXPathObjectPtr;
    let mut arg2: XmlXPathObjectPtr;

    if ctxt.is_null() || (*ctxt).context.is_null() {
        return 0;
    }
    arg2 = value_pop(ctxt);
    arg1 = value_pop(ctxt);
    if arg1.is_null() || arg2.is_null() {
        if !arg1.is_null() {
            xml_xpath_release_object((*ctxt).context, arg1);
        } else {
            xml_xpath_release_object((*ctxt).context, arg2);
        }
        XP_ERROR0!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
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
        value_push(ctxt, arg1);
        xml_xpath_number_function(ctxt, 1);
        arg1 = value_pop(ctxt);
    }
    if !matches!((*arg2).typ, XmlXPathObjectType::XPathNumber) {
        value_push(ctxt, arg2);
        xml_xpath_number_function(ctxt, 1);
        arg2 = value_pop(ctxt);
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

/// Implement the unary - operation on an XPath object
/// The numeric operators convert their operands to numbers as if
/// by calling the number function.
#[doc(alias = "xmlXPathValueFlipSign")]
pub unsafe extern "C" fn xml_xpath_value_flip_sign(ctxt: XmlXPathParserContextPtr) {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return;
    }
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
    (*(*ctxt).value).floatval = -(*(*ctxt).value).floatval;
}

/// Implement the add operation on XPath objects:
/// The numeric operators convert their operands to numbers as if
/// by calling the number function.
#[doc(alias = "xmlXPathAddValues")]
pub unsafe extern "C" fn xml_xpath_add_values(ctxt: XmlXPathParserContextPtr) {
    let arg: XmlXPathObjectPtr = value_pop(ctxt);
    if arg.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    let val: f64 = xml_xpath_cast_to_number(arg);
    xml_xpath_release_object((*ctxt).context, arg);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
    (*(*ctxt).value).floatval += val;
}

/// Implement the subtraction operation on XPath objects:
/// The numeric operators convert their operands to numbers as if
/// by calling the number function.
#[doc(alias = "xmlXPathSubValues")]
pub unsafe extern "C" fn xml_xpath_sub_values(ctxt: XmlXPathParserContextPtr) {
    let arg: XmlXPathObjectPtr = value_pop(ctxt);
    if arg.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    let val: f64 = xml_xpath_cast_to_number(arg);
    xml_xpath_release_object((*ctxt).context, arg);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
    (*(*ctxt).value).floatval -= val;
}

/// Implement the multiply operation on XPath objects:
/// The numeric operators convert their operands to numbers as if
/// by calling the number function.
#[doc(alias = "xmlXPathMultValues")]
pub unsafe extern "C" fn xml_xpath_mult_values(ctxt: XmlXPathParserContextPtr) {
    let arg: XmlXPathObjectPtr = value_pop(ctxt);
    if arg.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    let val: f64 = xml_xpath_cast_to_number(arg);
    xml_xpath_release_object((*ctxt).context, arg);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
    (*(*ctxt).value).floatval *= val;
}

/// Implement the div operation on XPath objects @arg1 / @arg2:
/// The numeric operators convert their operands to numbers as if
/// by calling the number function.
#[doc(alias = "xmlXPathDivValues")]
pub unsafe extern "C" fn xml_xpath_div_values(ctxt: XmlXPathParserContextPtr) {
    let arg: XmlXPathObjectPtr = value_pop(ctxt);
    if arg.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    let val: f64 = xml_xpath_cast_to_number(arg);
    xml_xpath_release_object((*ctxt).context, arg);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
    (*(*ctxt).value).floatval /= val;
}

/// Implement the mod operation on XPath objects: @arg1 / @arg2
/// The numeric operators convert their operands to numbers as if
/// by calling the number function.
#[doc(alias = "xmlXPathModValues")]
pub unsafe extern "C" fn xml_xpath_mod_values(ctxt: XmlXPathParserContextPtr) {
    let arg: XmlXPathObjectPtr = value_pop(ctxt);
    if arg.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
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

/// Is the name given a NodeType one.
///
/// ```ignore
/// [38]   NodeType ::=   'comment'
///                   | 'text'
///                   | 'processing-instruction'
///                   | 'node'
/// ```
///
/// Returns 1 if true 0 otherwise
#[doc(alias = "xmlXPathIsNodeType")]
pub unsafe extern "C" fn xml_xpath_is_node_type(name: *const XmlChar) -> i32 {
    if name.is_null() {
        return 0;
    }

    if xml_str_equal(name, c"node".as_ptr() as _) {
        return 1;
    }
    if xml_str_equal(name, c"text".as_ptr() as _) {
        return 1;
    }
    if xml_str_equal(name, c"comment".as_ptr() as _) {
        return 1;
    }
    if xml_str_equal(name, c"processing-instruction".as_ptr() as _) {
        return 1;
    }
    0
}

/// Traversal function for the "self" direction
/// The self axis contains just the context node itself
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextSelf")]
pub unsafe extern "C" fn xml_xpath_next_self(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if cur.is_null() {
        return (*(*ctxt).context).node;
    }
    null_mut()
}

/// Traversal function for the "child" direction
/// The child axis contains the children of the context node in document order.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextChild")]
pub unsafe extern "C" fn xml_xpath_next_child(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if cur.is_null() {
        if (*(*ctxt).context).node.is_null() {
            return null_mut();
        }
        match (*(*(*ctxt).context).node).element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPINode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDTDNode => {
                return (*(*(*ctxt).context).node)
                    .children()
                    .map_or(null_mut(), |c| c.as_ptr());
            }
            XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => {
                return (*(*(*ctxt).context).node)
                    .as_document_node()
                    .unwrap()
                    .as_ref()
                    .children
                    .map_or(null_mut(), |c| c.as_ptr());
            }
            XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXIncludeStart
            | XmlElementType::XmlXIncludeEnd => {
                return null_mut();
            }
            _ => unreachable!(),
        }
    }
    if matches!(
        (*cur).element_type(),
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
    ) {
        return null_mut();
    }
    (*cur).next.map_or(null_mut(), |n| n.as_ptr())
}

/// Traversal function for the "descendant" direction
/// the descendant axis contains the descendants of the context node in document
/// order; a descendant is a child or a child of a child and so on.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextDescendant")]
pub unsafe extern "C" fn xml_xpath_next_descendant(
    ctxt: XmlXPathParserContextPtr,
    mut cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if cur.is_null() {
        if (*(*ctxt).context).node.is_null() {
            return null_mut();
        }
        if matches!(
            (*(*(*ctxt).context).node).element_type(),
            XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
        ) {
            return null_mut();
        }

        if (*(*ctxt).context).node == (*(*ctxt).context).doc as XmlNodePtr {
            return (*(*(*ctxt).context).doc)
                .children
                .map_or(null_mut(), |c| c.as_ptr());
        }
        return (*(*(*ctxt).context).node)
            .children()
            .map_or(null_mut(), |c| c.as_ptr());
    }

    if matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    if let Some(children) = (*cur).children() {
        // Do not descend on entities declarations
        if !matches!(children.element_type(), XmlElementType::XmlEntityDecl) {
            cur = children.as_ptr();
            // Skip DTDs
            if !matches!((*cur).element_type(), XmlElementType::XmlDTDNode) {
                return cur;
            }
        }
    }

    if cur == (*(*ctxt).context).node {
        return null_mut();
    }

    while let Some(next) = (*cur).next {
        cur = next.as_ptr();
        if !matches!(
            (*cur).element_type(),
            XmlElementType::XmlEntityDecl | XmlElementType::XmlDTDNode
        ) {
            return cur;
        }
    }

    loop {
        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        if cur.is_null() {
            break;
        }
        if cur == (*(*ctxt).context).node {
            return null_mut();
        }
        if let Some(next) = (*cur).next {
            cur = next.as_ptr();
            return cur;
        }
        if cur.is_null() {
            break;
        }
    }
    cur
}

/// Traversal function for the "descendant-or-self" direction
/// the descendant-or-self axis contains the context node and the descendants
/// of the context node in document order; thus the context node is the first
/// node on the axis, and the first child of the context node is the second node
/// on the axis
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextDescendantOrSelf")]
pub unsafe extern "C" fn xml_xpath_next_descendant_or_self(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if cur.is_null() {
        return (*(*ctxt).context).node;
    }

    if (*(*ctxt).context).node.is_null() {
        return null_mut();
    }
    if matches!(
        (*(*(*ctxt).context).node).element_type(),
        XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
    ) {
        return null_mut();
    }

    xml_xpath_next_descendant(ctxt, cur)
}

/// Traversal function for the "parent" direction
/// The parent axis contains the parent of the context node, if there is one.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextParent")]
pub unsafe extern "C" fn xml_xpath_next_parent(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    /*
     * the parent of an attribute or namespace node is the element
     * to which the attribute or namespace node is attached
     * Namespace handling !!!
     */
    if cur.is_null() {
        if (*(*ctxt).context).node.is_null() {
            return null_mut();
        }
        match (*(*(*ctxt).context).node).element_type() {
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
                let Some(parent) = (*(*(*ctxt).context).node).parent() else {
                    return (*(*ctxt).context).doc as XmlNodePtr;
                };
                if matches!(parent.element_type(), XmlElementType::XmlElementNode)
                    && (*parent.name.add(0) == b' '
                        || xml_str_equal(parent.name, c"fake node libxslt".as_ptr() as _))
                {
                    return null_mut();
                }
                return parent.as_ptr();
            }
            XmlElementType::XmlAttributeNode => {
                let att: XmlAttrPtr = (*(*(*ctxt).context).node)
                    .as_attribute_node()
                    .unwrap()
                    .as_ptr();

                return (*att).parent.map_or(null_mut(), |p| p.as_ptr());
            }
            XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => {
                return null_mut();
            }
            XmlElementType::XmlNamespaceDecl => {
                let ns: XmlNsPtr = (*(*ctxt).context).node as XmlNsPtr;

                if !(*ns).next.is_null()
                    && !matches!((*(*ns).next).typ, XmlElementType::XmlNamespaceDecl)
                {
                    return (*ns).next as XmlNodePtr;
                }
                return null_mut();
            }
            _ => unreachable!(),
        }
    }
    null_mut()
}

/// Traversal function for the "ancestor-or-self" direction
/// he ancestor-or-self axis contains the context node and ancestors of
/// the context node in reverse document order; thus the context node is
/// the first node on the axis, and the context node's parent the second;
/// parent here is defined the same as with the parent axis.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextAncestorOrSelf")]
pub unsafe extern "C" fn xml_xpath_next_ancestor_or_self(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if cur.is_null() {
        return (*(*ctxt).context).node;
    }
    xml_xpath_next_ancestor(ctxt, cur)
}

/// Traversal function for the "following-sibling" direction
/// The following-sibling axis contains the following siblings of the context
/// node in document order.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextFollowingSibling")]
pub unsafe extern "C" fn xml_xpath_next_following_sibling(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if matches!(
        (*(*(*ctxt).context).node).element_type(),
        XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
    ) {
        return null_mut();
    }
    if cur == (*(*ctxt).context).doc as XmlNodePtr {
        return null_mut();
    }
    if cur.is_null() {
        return (*(*(*ctxt).context).node)
            .next
            .map_or(null_mut(), |n| n.as_ptr());
    }
    (*cur).next.map_or(null_mut(), |n| n.as_ptr())
}

/// Traversal function for the "following" direction
/// The following axis contains all nodes in the same document as the context
/// node that are after the context node in document order, excluding any
/// descendants and excluding attribute nodes and namespace nodes; the nodes
/// are ordered in document order
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextFollowing")]
pub unsafe extern "C" fn xml_xpath_next_following(
    ctxt: XmlXPathParserContextPtr,
    mut cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if !cur.is_null()
        && !matches!(
            (*cur).element_type(),
            XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
        )
        && (*cur).children().is_some()
    {
        return (*cur).children().unwrap().as_ptr();
    }

    if cur.is_null() {
        cur = (*(*ctxt).context).node;
        if matches!((*cur).element_type(), XmlElementType::XmlAttributeNode) {
            cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        } else if matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
            let ns: XmlNsPtr = cur as XmlNsPtr;

            if (*ns).next.is_null() || matches!((*(*ns).next).typ, XmlElementType::XmlNamespaceDecl)
            {
                return null_mut();
            }
            cur = (*ns).next as XmlNodePtr;
        }
    }
    if cur.is_null() {
        return null_mut(); /* ERROR */
    }
    if let Some(next) = (*cur).next {
        return next.as_ptr();
    }
    loop {
        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        if cur.is_null() {
            break;
        }
        if cur == (*(*ctxt).context).doc as XmlNodePtr {
            return null_mut();
        }
        if let Some(next) = (*cur).next {
            return next.as_ptr();
        }
        if cur.is_null() {
            break;
        }
    }
    cur
}

thread_local! {
    static XML_XPATH_XMLNAMESPACE_STRUCT: XmlNs = const { XmlNs {
        next: null_mut(),
        typ: XmlElementType::XmlNamespaceDecl,
        href: XML_XML_NAMESPACE.as_ptr() as *const u8,
        prefix: c"xml".as_ptr() as *const u8,
        _private: null_mut(),
        context: null_mut(),
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
pub unsafe extern "C" fn xml_xpath_next_namespace(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if !matches!(
        (*(*(*ctxt).context).node).element_type(),
        XmlElementType::XmlElementNode
    ) {
        return null_mut();
    }
    if cur.is_null() {
        (*(*ctxt).context).tmp_ns_list =
            (*(*(*ctxt).context).node).get_ns_list((*(*ctxt).context).doc);
        (*(*ctxt).context).tmp_ns_nr = 0;
        if let Some(list) = (*(*ctxt).context).tmp_ns_list.as_deref() {
            (*(*ctxt).context).tmp_ns_nr = list.len() as i32;
        }
        // Does it work ???
        let reference = XML_XPATH_XMLNAMESPACE_STRUCT.with(|s| s as *const XmlNs);
        return reference as XmlNodePtr;
    }
    if (*(*ctxt).context).tmp_ns_nr > 0 {
        (*(*ctxt).context).tmp_ns_nr -= 1;
        (*(*ctxt).context).tmp_ns_list.as_deref().unwrap()[(*(*ctxt).context).tmp_ns_nr as usize]
            as XmlNodePtr
    } else {
        (*(*ctxt).context).tmp_ns_list = None;
        null_mut()
    }
}

/// Traversal function for the "attribute" direction
/// TODO: support DTD inherited default attributes
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextAttribute")]
pub unsafe extern "C" fn xml_xpath_next_attribute(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if (*(*ctxt).context).node.is_null() {
        return null_mut();
    }
    if !matches!(
        (*(*(*ctxt).context).node).element_type(),
        XmlElementType::XmlElementNode
    ) {
        return null_mut();
    }
    if cur.is_null() {
        if (*(*ctxt).context).node == (*(*ctxt).context).doc as XmlNodePtr {
            return null_mut();
        }
        return (*(*(*ctxt).context).node).properties as XmlNodePtr;
    }
    (*cur).next.map_or(null_mut(), |n| n.as_ptr())
}

/// Check that @ancestor is a @node's ancestor
///
/// returns 1 if @ancestor is a @node's ancestor, 0 otherwise.
#[doc(alias = "xmlXPathIsAncestor")]
unsafe extern "C" fn xml_xpath_is_ancestor(ancestor: XmlNodePtr, mut node: XmlNodePtr) -> i32 {
    if ancestor.is_null() || node.is_null() {
        return 0;
    }
    if matches!((*node).element_type(), XmlElementType::XmlNamespaceDecl) {
        return 0;
    }
    if matches!((*ancestor).element_type(), XmlElementType::XmlNamespaceDecl) {
        return 0;
    }
    /* nodes need to be in the same document */
    if (*ancestor).doc != (*node).doc {
        return 0;
    }
    /* avoid searching if ancestor or node is the root node */
    if ancestor == (*node).doc as XmlNodePtr {
        return 1;
    }
    if node == (*ancestor).doc as XmlNodePtr {
        return 0;
    }
    while let Some(parent) = (*node).parent() {
        if parent.as_ptr() == ancestor {
            return 1;
        }
        node = parent.as_ptr();
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
pub unsafe extern "C" fn xml_xpath_next_preceding(
    ctxt: XmlXPathParserContextPtr,
    mut cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if cur.is_null() {
        cur = (*(*ctxt).context).node;
        if matches!((*cur).element_type(), XmlElementType::XmlAttributeNode) {
            cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        } else if matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
            let ns: XmlNsPtr = cur as XmlNsPtr;

            if (*ns).next.is_null() || matches!((*(*ns).next).typ, XmlElementType::XmlNamespaceDecl)
            {
                return null_mut();
            }
            cur = (*ns).next as XmlNodePtr;
        }
    }
    if cur.is_null() || matches!((*cur).element_type(), XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    if let Some(prev) = (*cur)
        .prev
        .filter(|p| matches!(p.element_type(), XmlElementType::XmlDTDNode))
    {
        cur = prev.as_ptr();
    }
    loop {
        if let Some(prev) = (*cur).prev {
            cur = prev.as_ptr();
            while let Some(last) = (*cur).last() {
                cur = last.as_ptr();
            }
            return cur;
        }

        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        if cur.is_null() {
            return null_mut();
        }
        if NodePtr::from_ptr(cur) == (*(*(*ctxt).context).doc).children {
            return null_mut();
        }
        if xml_xpath_is_ancestor(cur, (*(*ctxt).context).node) == 0 {
            break;
        }
    }
    cur
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
pub unsafe extern "C" fn xml_xpath_next_ancestor(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    // the parent of an attribute or namespace node is the element
    // to which the attribute or namespace node is attached !!!!!!!!!!!!!
    if cur.is_null() {
        if (*(*ctxt).context).node.is_null() {
            return null_mut();
        }
        match (*(*(*ctxt).context).node).element_type() {
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
                let Some(parent) = (*(*(*ctxt).context).node).parent() else {
                    return (*(*ctxt).context).doc as XmlNodePtr;
                };
                if matches!(parent.element_type(), XmlElementType::XmlElementNode)
                    && (*parent.name.add(0) == b' '
                        || xml_str_equal(parent.name, c"fake node libxslt".as_ptr() as _))
                {
                    return null_mut();
                }
                return parent.as_ptr();
            }
            XmlElementType::XmlAttributeNode => {
                let tmp: XmlAttrPtr = (*(*(*ctxt).context).node)
                    .as_attribute_node()
                    .unwrap()
                    .as_ptr();

                return (*tmp).parent().map_or(null_mut(), |p| p.as_ptr());
            }
            XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHTMLDocumentNode => {
                return null_mut();
            }
            XmlElementType::XmlNamespaceDecl => {
                let ns: XmlNsPtr = (*(*ctxt).context).node as XmlNsPtr;

                if !(*ns).next.is_null()
                    && !matches!((*(*ns).next).typ, XmlElementType::XmlNamespaceDecl)
                {
                    return (*ns).next as XmlNodePtr;
                }
                /* Bad, how did that namespace end up here ? */
                return null_mut();
            }
            _ => unreachable!(),
        }
    }
    if NodePtr::from_ptr(cur) == (*(*(*ctxt).context).doc).children {
        return (*(*ctxt).context).doc as XmlNodePtr;
    }
    if cur == (*(*ctxt).context).doc as XmlNodePtr {
        return null_mut();
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
            let Some(parent) = (*cur).parent() else {
                return null_mut();
            };
            if matches!(parent.element_type(), XmlElementType::XmlElementNode)
                && (*parent.name.add(0) == b' '
                    || xml_str_equal(parent.name, c"fake node libxslt".as_ptr() as _))
            {
                return null_mut();
            }
            parent.as_ptr()
        }
        XmlElementType::XmlAttributeNode => {
            let att: XmlAttrPtr = (*cur).as_attribute_node().unwrap().as_ptr();

            (*att).parent.map_or(null_mut(), |p| p.as_ptr())
        }
        XmlElementType::XmlNamespaceDecl => {
            let ns: XmlNsPtr = cur as XmlNsPtr;

            if !(*ns).next.is_null()
                && !matches!((*(*ns).next).typ, XmlElementType::XmlNamespaceDecl)
            {
                return (*ns).next as XmlNodePtr;
            }
            /* Bad, how did that namespace end up here ? */
            null_mut()
        }
        XmlElementType::XmlDocumentNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlHTMLDocumentNode => null_mut(),
        _ => unreachable!(),
    }
}

/// Traversal function for the "preceding-sibling" direction
/// The preceding-sibling axis contains the preceding siblings of the context
/// node in reverse document order; the first preceding sibling is first on the
/// axis; the sibling preceding that node is the second on the axis and so on.
///
/// Returns the next element following that axis
#[doc(alias = "xmlXPathNextPrecedingSibling")]
pub unsafe extern "C" fn xml_xpath_next_preceding_sibling(
    ctxt: XmlXPathParserContextPtr,
    mut cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if matches!(
        (*(*(*ctxt).context).node).element_type(),
        XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
    ) {
        return null_mut();
    }
    if cur == (*(*ctxt).context).doc as XmlNodePtr {
        return null_mut();
    }
    if cur.is_null() {
        return (*(*(*ctxt).context).node)
            .prev
            .map_or(null_mut(), |p| p.as_ptr());
    }
    if let Some(prev) = (*cur)
        .prev
        .filter(|p| matches!(p.element_type(), XmlElementType::XmlDTDNode))
    {
        cur = prev.as_ptr();
        if cur.is_null() {
            return (*(*(*ctxt).context).node)
                .prev
                .map_or(null_mut(), |p| p.as_ptr());
        }
    }
    (*cur).prev.map_or(null_mut(), |p| p.as_ptr())
}

/// Implement the last() XPath function
///    number last()
/// The last function returns the number of nodes in the context node list.
#[doc(alias = "xmlXPathLastFunction")]
pub unsafe fn xml_xpath_last_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    CHECK_ARITY!(ctxt, nargs, 0);
    if (*(*ctxt).context).context_size >= 0 {
        value_push(
            ctxt,
            xml_xpath_cache_new_float((*ctxt).context, (*(*ctxt).context).context_size as f64),
        );
    } else {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidCtxtSize as i32);
    }
}

/// Implement the position() XPath function
///    number position()
/// The position function returns the position of the context node in the
/// context node list. The first position is 1, and so the last position
/// will be equal to last().
#[doc(alias = "xmlXPathPositionFunction")]
pub unsafe fn xml_xpath_position_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    CHECK_ARITY!(ctxt, nargs, 0);
    if (*(*ctxt).context).proximity_position >= 0 {
        value_push(
            ctxt,
            xml_xpath_cache_new_float(
                (*ctxt).context,
                (*(*ctxt).context).proximity_position as f64,
            ),
        );
    } else {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidCtxtPosition as i32);
    }
}

/// Implement the count() XPath function
///    number count(node-set)
#[doc(alias = "xmlXPathCountFunction")]
pub unsafe fn xml_xpath_count_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    CHECK_ARITY!(ctxt, nargs, 1);
    if (*ctxt).value.is_null()
        || !matches!(
            (*(*ctxt).value).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }
    let cur: XmlXPathObjectPtr = value_pop(ctxt);

    if cur.is_null() || (*cur).nodesetval.is_null() {
        value_push(ctxt, xml_xpath_cache_new_float((*ctxt).context, 0.0));
    } else {
        value_push(
            ctxt,
            xml_xpath_cache_new_float(
                (*ctxt).context,
                (*(*cur).nodesetval)
                    .node_tab
                    .as_ref()
                    .map_or(0, |t| t.len()) as f64,
            ),
        );
    }
    xml_xpath_release_object((*ctxt).context, cur);
}

/// Selects elements by their unique ID.
///
/// Returns a node-set of selected elements.
#[doc(alias = "xmlXPathGetElementsByIds")]
unsafe extern "C" fn xml_xpath_get_elements_by_ids(
    doc: XmlDocPtr,
    mut ids: *const XmlChar,
) -> XmlNodeSetPtr {
    let mut cur: *const XmlChar = ids;
    let mut id: *mut XmlChar;
    let mut elem: XmlNodePtr;

    if ids.is_null() {
        return null_mut();
    }

    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    if ret.is_null() {
        return ret;
    }

    while xml_is_blank_char(*cur as u32) {
        cur = cur.add(1);
    }
    while *cur != 0 {
        while !xml_is_blank_char(*cur as u32) && *cur != 0 {
            cur = cur.add(1);
        }

        id = xml_strndup(ids, cur.offset_from(ids) as _);
        if !id.is_null() {
            /*
             * We used to check the fact that the value passed
             * was an NCName, but this generated much troubles for
             * me and Aleksey Sanin, people blatantly violated that
             * constraint, like Visa3D spec.
             * if (xmlValidateNCName(ID, 1) == 0)
             */
            if let Some(attr) = xml_get_id(doc, id) {
                if let Some(attr) = attr.as_ref().as_attribute_node() {
                    elem = attr.as_ref().parent.map_or(null_mut(), |p| p.as_ptr());
                } else if matches!(attr.as_ref().element_type(), XmlElementType::XmlElementNode) {
                    elem = attr.as_ref().as_node().unwrap().as_ptr();
                } else {
                    elem = null_mut();
                }
                /* TODO: Check memory error. */
                if !elem.is_null() {
                    xml_xpath_node_set_add(ret, elem);
                }
            }
            xml_free(id as _);
        }

        while xml_is_blank_char(*cur as u32) {
            cur = cur.add(1);
        }
        ids = cur;
    }
    ret
}

/// This is the cached version of xmlXPathConvertString().
/// Converts an existing object to its string() equivalent
///
/// Returns a created or reused object, the old one is freed (cached)
///         (or the operation is done directly on @val)
#[doc(alias = "xmlXPathCacheConvertString")]
unsafe extern "C" fn xml_xpath_cache_convert_string(
    ctxt: XmlXPathContextPtr,
    val: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    let mut res: *mut XmlChar = null_mut();

    if val.is_null() {
        return xml_xpath_cache_new_cstring(ctxt, c"".as_ptr() as _);
    }

    match (*val).typ {
        XmlXPathObjectType::XPathUndefined => {}
        XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
            let tmp = xml_xpath_cast_node_set_to_string((*val).nodesetval)
                .map(|c| CString::new(c).unwrap());
            res = xml_strdup(tmp.as_ref().map_or(null_mut(), |t| t.as_ptr() as *const u8));
        }
        XmlXPathObjectType::XPathString => {
            return val;
        }
        XmlXPathObjectType::XPathBoolean => {
            let s = xml_xpath_cast_boolean_to_string((*val).boolval);
            let s = CString::new(s).unwrap();
            res = xml_strdup(s.as_ptr() as *const u8);
        }
        XmlXPathObjectType::XPathNumber => {
            let s = xml_xpath_cast_number_to_string((*val).floatval);
            let s = CString::new(s.as_ref()).unwrap();
            res = xml_strdup(s.as_ptr() as *const u8);
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
    if res.is_null() {
        return xml_xpath_cache_new_cstring(ctxt, c"".as_ptr() as _);
    }
    xml_xpath_cache_wrap_string(ctxt, res)
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
    let mut ret: XmlNodeSetPtr;
    let mut obj: XmlXPathObjectPtr;

    CHECK_ARITY!(ctxt, nargs, 1);
    obj = value_pop(ctxt);
    if obj.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    if matches!(
        (*obj).typ,
        XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
    ) {
        let mut ns: XmlNodeSetPtr;

        // TODO: Check memory error.
        ret = xml_xpath_node_set_create(null_mut());

        if !(*obj).nodesetval.is_null() {
            if let Some(table) = (*(*obj).nodesetval).node_tab.as_ref() {
                for &node in table {
                    let tokens = xml_xpath_cast_node_to_string(node);
                    let tokens = CString::new(tokens).unwrap();
                    ns = xml_xpath_get_elements_by_ids(
                        (*(*ctxt).context).doc,
                        tokens.as_ptr() as *const u8,
                    );
                    // TODO: Check memory error.
                    ret = xml_xpath_node_set_merge(ret, ns);
                    xml_xpath_free_node_set(ns);
                }
            }
        }
        xml_xpath_release_object((*ctxt).context, obj);
        value_push(ctxt, xml_xpath_cache_wrap_node_set((*ctxt).context, ret));
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
    ret = xml_xpath_get_elements_by_ids(
        (*(*ctxt).context).doc,
        strval
            .as_deref()
            .map_or(null_mut(), |s| s.as_ptr() as *const u8),
    );
    value_push(ctxt, xml_xpath_cache_wrap_node_set((*ctxt).context, ret));
    xml_xpath_release_object((*ctxt).context, obj);
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
    if ctxt.is_null() {
        return;
    }

    if nargs == 0 {
        value_push(
            ctxt,
            xml_xpath_cache_new_node_set((*ctxt).context, (*(*ctxt).context).node),
        );
        nargs = 1;
    }

    CHECK_ARITY!(ctxt, nargs, 1);
    if (*ctxt).value.is_null()
        || !matches!(
            (*(*ctxt).value).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }
    let cur: XmlXPathObjectPtr = value_pop(ctxt);

    if (*cur).nodesetval.is_null() {
        value_push(
            ctxt,
            xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
        );
    } else if let Some(table) = (*(*cur).nodesetval)
        .node_tab
        .as_ref()
        .filter(|t| !t.is_empty())
    {
        let i = 0; /* Should be first in document order !!!!! */
        match (*table[i]).element_type() {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlPINode => {
                if *(*table[i]).name.add(0) == b' ' {
                    value_push(
                        ctxt,
                        xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
                    );
                } else {
                    value_push(
                        ctxt,
                        xml_xpath_cache_new_string((*ctxt).context, (*table[i]).name),
                    );
                }
            }
            XmlElementType::XmlNamespaceDecl => {
                value_push(
                    ctxt,
                    xml_xpath_cache_new_string((*ctxt).context, (*(table[i] as XmlNsPtr)).prefix),
                );
            }
            _ => {
                value_push(
                    ctxt,
                    xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
                );
            }
        }
    } else {
        value_push(
            ctxt,
            xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
        );
    }
    xml_xpath_release_object((*ctxt).context, cur);
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
    if ctxt.is_null() {
        return;
    }

    if nargs == 0 {
        value_push(
            ctxt,
            xml_xpath_cache_new_node_set((*ctxt).context, (*(*ctxt).context).node),
        );
        nargs = 1;
    }
    CHECK_ARITY!(ctxt, nargs, 1);
    if (*ctxt).value.is_null()
        || !matches!(
            (*(*ctxt).value).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }
    let cur: XmlXPathObjectPtr = value_pop(ctxt);

    if (*cur).nodesetval.is_null() {
        value_push(
            ctxt,
            xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
        );
    } else if let Some(table) = (*(*cur).nodesetval)
        .node_tab
        .as_ref()
        .filter(|t| !t.is_empty())
    {
        let i = 0; /* Should be first in document order !!!!! */
        match (*table[i]).element_type() {
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
                if (*table[i]).ns.is_null() {
                    value_push(
                        ctxt,
                        xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
                    );
                } else {
                    value_push(
                        ctxt,
                        xml_xpath_cache_new_string((*ctxt).context, (*(*table[i]).ns).href),
                    );
                }
            }
            _ => {
                value_push(
                    ctxt,
                    xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
                );
            }
        }
    } else {
        value_push(
            ctxt,
            xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
        );
    }
    xml_xpath_release_object((*ctxt).context, cur);
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
    if ctxt.is_null() {
        return;
    }
    if nargs == 0 {
        let s = xml_xpath_cast_node_to_string((*(*ctxt).context).node);
        let s = CString::new(s).unwrap();
        let val = xml_strdup(s.as_ptr() as *const u8);
        value_push(ctxt, xml_xpath_cache_wrap_string((*ctxt).context, val));
        return;
    }

    CHECK_ARITY!(ctxt, nargs, 1);
    let cur: XmlXPathObjectPtr = value_pop(ctxt);
    if cur.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    value_push(ctxt, xml_xpath_cache_convert_string((*ctxt).context, cur));
}

/// Implement the string-length() XPath function
///    number string-length(string?)
/// The string-length returns the number of characters in the string
/// (see [3.6 Strings]). If the argument is omitted, it defaults to
/// the context node converted to a string, in other words the value
/// of the context node.
#[doc(alias = "xmlXPathStringLengthFunction")]
pub unsafe fn xml_xpath_string_length_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    if nargs == 0 {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return;
        }
        if (*(*ctxt).context).node.is_null() {
            value_push(ctxt, xml_xpath_cache_new_float((*ctxt).context, 0.0));
        } else {
            let content = xml_xpath_cast_node_to_string((*(*ctxt).context).node);
            let content = CString::new(content).unwrap();
            value_push(
                ctxt,
                xml_xpath_cache_new_float(
                    (*ctxt).context,
                    xml_utf8_strlen(content.as_ptr() as *const u8) as _,
                ),
            );
        }
        return;
    }
    CHECK_ARITY!(ctxt, nargs, 1);
    CAST_TO_STRING!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathString);
    let cur: XmlXPathObjectPtr = value_pop(ctxt);
    value_push(
        ctxt,
        xml_xpath_cache_new_float(
            (*ctxt).context,
            (*cur).stringval.as_deref().map_or(0, |s| s.chars().count()) as _,
        ),
    );
    xml_xpath_release_object((*ctxt).context, cur);
}

/// Implement the concat() XPath function
///    string concat(string, string, string*)
/// The concat function returns the concatenation of its arguments.
#[doc(alias = "xmlXPathConcatFunction")]
pub unsafe fn xml_xpath_concat_function(ctxt: XmlXPathParserContextPtr, mut nargs: i32) {
    let mut newobj: XmlXPathObjectPtr;

    if ctxt.is_null() {
        return;
    }
    if nargs < 2 {
        CHECK_ARITY!(ctxt, nargs, 2);
    }

    CAST_TO_STRING!(ctxt);
    let cur: XmlXPathObjectPtr = value_pop(ctxt);
    if cur.is_null() || !matches!((*cur).typ, XmlXPathObjectType::XPathString) {
        xml_xpath_release_object((*ctxt).context, cur);
        return;
    }
    nargs -= 1;

    while nargs > 0 {
        CAST_TO_STRING!(ctxt);
        newobj = value_pop(ctxt);
        if newobj.is_null() || (*newobj).typ != XmlXPathObjectType::XPathString {
            xml_xpath_release_object((*ctxt).context, newobj);
            xml_xpath_release_object((*ctxt).context, cur);
            XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
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
    value_push(ctxt, cur);
}

/// Implement the contains() XPath function
///    boolean contains(string, string)
/// The contains function returns true if the first argument string
/// contains the second argument string, and otherwise returns false.
#[doc(alias = "xmlXPathContainsFunction")]
pub unsafe fn xml_xpath_contains_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    CHECK_ARITY!(ctxt, nargs, 2);
    CAST_TO_STRING!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathString);
    let needle: XmlXPathObjectPtr = value_pop(ctxt);
    CAST_TO_STRING!(ctxt);
    let hay: XmlXPathObjectPtr = value_pop(ctxt);

    if hay.is_null() || (*hay).typ != XmlXPathObjectType::XPathString {
        xml_xpath_release_object((*ctxt).context, hay);
        xml_xpath_release_object((*ctxt).context, needle);
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
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
        value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, 1));
    } else {
        value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, 0));
    }
    xml_xpath_release_object((*ctxt).context, hay);
    xml_xpath_release_object((*ctxt).context, needle);
}

/// Implement the starts-with() XPath function
///    boolean starts-with(string, string)
/// The starts-with function returns true if the first argument string
/// starts with the second argument string, and otherwise returns false.
#[doc(alias = "xmlXPathStartsWithFunction")]
pub unsafe fn xml_xpath_starts_with_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    CHECK_ARITY!(ctxt, nargs, 2);
    CAST_TO_STRING!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathString);
    let needle: XmlXPathObjectPtr = value_pop(ctxt);
    CAST_TO_STRING!(ctxt);
    let hay: XmlXPathObjectPtr = value_pop(ctxt);

    if hay.is_null() || (*hay).typ != XmlXPathObjectType::XPathString {
        xml_xpath_release_object((*ctxt).context, hay);
        xml_xpath_release_object((*ctxt).context, needle);
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }
    let m = (*hay).stringval.as_deref().map_or(0, |s| s.len());
    let n = (*needle).stringval.as_deref().map_or(0, |s| s.len());
    if &(*hay).stringval.as_deref().unwrap()[..n.min(m)]
        != (*needle).stringval.as_deref().expect("Internal Error")
    {
        value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, 0));
    } else {
        value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, 1));
    }
    xml_xpath_release_object((*ctxt).context, hay);
    xml_xpath_release_object((*ctxt).context, needle);
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
    /*
     * take care of possible last (position) argument
     */
    if nargs == 3 {
        CAST_TO_NUMBER!(ctxt);
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
        len = value_pop(ctxt);
        le = (*len).floatval;
        xml_xpath_release_object((*ctxt).context, len);
    }

    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);
    let start: XmlXPathObjectPtr = value_pop(ctxt);
    let input: f64 = (*start).floatval;
    xml_xpath_release_object((*ctxt).context, start);
    CAST_TO_STRING!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathString);
    let str: XmlXPathObjectPtr = value_pop(ctxt);

    if !matches!(
        input.partial_cmp(&(INT_MAX as f64)),
        Some(std::cmp::Ordering::Less)
    ) {
        /* Logical NOT to handle NaNs */
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
        let ret = CString::new(ret).unwrap();
        value_push(
            ctxt,
            xml_xpath_cache_new_string((*ctxt).context, ret.as_ptr() as *const u8),
        );
    } else {
        value_push(
            ctxt,
            xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
        );
    }

    xml_xpath_release_object((*ctxt).context, str);
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
    CHECK_ARITY!(ctxt, nargs, 2);
    CAST_TO_STRING!(ctxt);
    let find: XmlXPathObjectPtr = value_pop(ctxt);
    CAST_TO_STRING!(ctxt);
    let str: XmlXPathObjectPtr = value_pop(ctxt);

    let target: XmlBufPtr = xml_buf_create();
    if !target.is_null() {
        let ss = (*str).stringval.as_deref().unwrap();
        let fs = (*find).stringval.as_deref().unwrap();
        if let Some(pos) = ss.find(fs) {
            xml_buf_add(target, ss.as_ptr(), pos as i32);
        }
        value_push(
            ctxt,
            xml_xpath_cache_new_string((*ctxt).context, xml_buf_content(target)),
        );
        xml_buf_free(target);
    }
    xml_xpath_release_object((*ctxt).context, str);
    xml_xpath_release_object((*ctxt).context, find);
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
    CHECK_ARITY!(ctxt, nargs, 2);
    CAST_TO_STRING!(ctxt);
    let find: XmlXPathObjectPtr = value_pop(ctxt);
    CAST_TO_STRING!(ctxt);
    let str: XmlXPathObjectPtr = value_pop(ctxt);

    let target: XmlBufPtr = xml_buf_create();
    if !target.is_null() {
        let ss = (*str).stringval.as_deref().unwrap();
        let fs = (*find).stringval.as_deref().unwrap();
        if let Some(pos) = ss.find(fs) {
            let len = ss.len() - pos;
            xml_buf_add(target, ss[pos..].as_ptr(), len as i32);
        }
        value_push(
            ctxt,
            xml_xpath_cache_new_string((*ctxt).context, xml_buf_content(target)),
        );
        xml_buf_free(target);
    }
    xml_xpath_release_object((*ctxt).context, str);
    xml_xpath_release_object((*ctxt).context, find);
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
    if ctxt.is_null() {
        return;
    }
    if nargs == 0 {
        // Use current context node
        let s = xml_xpath_cast_node_to_string((*(*ctxt).context).node);
        let s = CString::new(s).unwrap();
        let val = xml_strdup(s.as_ptr() as *const u8);
        value_push(ctxt, xml_xpath_cache_wrap_string((*ctxt).context, val));
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
    CHECK_ARITY!(ctxt, nargs, 3);

    CAST_TO_STRING!(ctxt);
    let to: XmlXPathObjectPtr = value_pop(ctxt);
    CAST_TO_STRING!(ctxt);
    let from: XmlXPathObjectPtr = value_pop(ctxt);
    CAST_TO_STRING!(ctxt);
    let str: XmlXPathObjectPtr = value_pop(ctxt);

    let target: XmlBufPtr = xml_buf_create();
    if !target.is_null() {
        let to_str = (*to).stringval.as_deref().unwrap();
        let from_str = (*from).stringval.as_deref().unwrap();
        let arg = (*str).stringval.as_deref().unwrap();
        let mut buf = [0u8; 6];
        for c in arg.chars() {
            if let Some((_, replace)) = from_str
                .chars()
                .zip(to_str.chars().map(Ok).chain(repeat(Err(()))))
                .find(|e| e.0 == c)
            {
                if let Ok(c) = replace {
                    let buf = c.encode_utf8(&mut buf[..]);
                    xml_buf_add(target, buf.as_ptr(), buf.len() as i32);
                }
            } else {
                let buf = c.encode_utf8(&mut buf[..]);
                xml_buf_add(target, buf.as_ptr(), buf.len() as i32);
            }
        }
    }
    value_push(
        ctxt,
        xml_xpath_cache_new_string((*ctxt).context, xml_buf_content(target)),
    );
    xml_buf_free(target);
    xml_xpath_release_object((*ctxt).context, str);
    xml_xpath_release_object((*ctxt).context, from);
    xml_xpath_release_object((*ctxt).context, to);
}

/// Implement the not() XPath function
///    boolean not(boolean)
/// The not function returns true if its argument is false,
/// and false otherwise.
#[doc(alias = "xmlXPathNotFunction")]
pub unsafe fn xml_xpath_not_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    CHECK_ARITY!(ctxt, nargs, 1);
    CAST_TO_BOOLEAN!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathBoolean);
    (*(*ctxt).value).boolval = !(*(*ctxt).value).boolval;
}

/// Implement the true() XPath function
///    boolean true()
#[doc(alias = "xmlXPathTrueFunction")]
pub unsafe fn xml_xpath_true_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    CHECK_ARITY!(ctxt, nargs, 0);
    value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, 1));
}

/// Implement the false() XPath function
///    boolean false()
#[doc(alias = "xmlXPathFalseFunction")]
pub unsafe fn xml_xpath_false_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    CHECK_ARITY!(ctxt, nargs, 0);
    value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, 0));
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
    let mut ret: i32 = 0;

    CHECK_ARITY!(ctxt, nargs, 1);
    CAST_TO_STRING!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathString);
    let val = value_pop(ctxt);
    let lang = (*val).stringval.as_deref();
    let the_lang = (*(*(*ctxt).context).node).get_lang();
    'not_equal: {
        if let (Some(the_lang), Some(lang)) = (the_lang, lang) {
            let the_lang = the_lang.as_bytes();
            let lang = lang.as_bytes();
            let mut i = 0;
            while i < lang.len() {
                if lang[i].to_ascii_uppercase()
                    != the_lang.get(i).unwrap_or(&0).to_ascii_uppercase()
                {
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
    value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, ret));
}

/// This is the cached version of xmlXPathConvertNumber().
/// Converts an existing object to its number() equivalent
///
/// Returns a created or reused object, the old one is freed (or the operation
///         is done directly on @val)
#[doc(alias = "xmlXPathCacheConvertNumber")]
unsafe extern "C" fn xml_xpath_cache_convert_number(
    ctxt: XmlXPathContextPtr,
    val: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
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

/// Implement the number() XPath function
///    number number(object?)
#[doc(alias = "xmlXPathNumberFunction")]
pub unsafe fn xml_xpath_number_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    let res: f64;

    if ctxt.is_null() {
        return;
    }
    if nargs == 0 {
        if (*(*ctxt).context).node.is_null() {
            value_push(ctxt, xml_xpath_cache_new_float((*ctxt).context, 0.0));
        } else {
            let content = (*(*(*ctxt).context).node)
                .get_content()
                .map(|c| CString::new(c).unwrap());
            let content = content
                .as_ref()
                .map_or(null_mut(), |c| c.as_ptr() as *mut u8);
            res = xml_xpath_string_eval_number(content);
            value_push(ctxt, xml_xpath_cache_new_float((*ctxt).context, res));
        }
        return;
    }

    CHECK_ARITY!(ctxt, nargs, 1);
    let cur: XmlXPathObjectPtr = value_pop(ctxt);
    value_push(ctxt, xml_xpath_cache_convert_number((*ctxt).context, cur));
}

/// Implement the sum() XPath function
///    number sum(node-set)
/// The sum function returns the sum of the values of the nodes in
/// the argument node-set.
#[doc(alias = "xmlXPathSumFunction")]
pub unsafe fn xml_xpath_sum_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    let mut res: f64 = 0.0;

    CHECK_ARITY!(ctxt, nargs, 1);
    if (*ctxt).value.is_null()
        || !matches!(
            (*(*ctxt).value).typ,
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }
    let cur: XmlXPathObjectPtr = value_pop(ctxt);

    if !(*cur).nodesetval.is_null()
        && (*(*cur).nodesetval)
            .node_tab
            .as_ref()
            .map_or(false, |t| !t.is_empty())
    {
        if let Some(table) = (*(*cur).nodesetval)
            .node_tab
            .as_ref()
            .filter(|t| !t.is_empty())
        {
            for &node in table {
                res += xml_xpath_cast_node_to_number(node);
            }
        }
    }
    value_push(ctxt, xml_xpath_cache_new_float((*ctxt).context, res));
    xml_xpath_release_object((*ctxt).context, cur);
}

/// Implement the floor() XPath function
///    number floor(number)
/// The floor function returns the largest (closest to positive infinity)
/// number that is not greater than the argument and that is an integer.
#[doc(alias = "xmlXPathFloorFunction")]
pub unsafe fn xml_xpath_floor_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    CHECK_ARITY!(ctxt, nargs, 1);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);

    (*(*ctxt).value).floatval = (*(*ctxt).value).floatval.floor();
}

/// Implement the ceiling() XPath function
///    number ceiling(number)
/// The ceiling function returns the smallest (closest to negative infinity)
/// number that is not less than the argument and that is an integer.
#[doc(alias = "xmlXPathCeilingFunction")]
pub unsafe fn xml_xpath_ceiling_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
    CHECK_ARITY!(ctxt, nargs, 1);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XPathNumber);

    (*(*ctxt).value).floatval = (*(*ctxt).value).floatval.ceil();
}

/// Implement the round() XPath function
///    number round(number)
/// The round function returns the number that is closest to the
/// argument and that is an integer. If there are two such numbers,
/// then the one that is closest to positive infinity is returned.
#[doc(alias = "xmlXPathRoundFunction")]
pub unsafe fn xml_xpath_round_function(ctxt: XmlXPathParserContextPtr, nargs: i32) {
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

/// This is the cached version of xmlXPathConvertBoolean().
/// Converts an existing object to its boolean() equivalent
///
/// Returns a created or reused object, the old one is freed (or the operation
/// is done directly on @val)
#[doc(alias = "xmlXPathCacheConvertBoolean")]
unsafe extern "C" fn xml_xpath_cache_convert_boolean(
    ctxt: XmlXPathContextPtr,
    val: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    if val.is_null() {
        return xml_xpath_cache_new_boolean(ctxt, 0);
    }
    if matches!((*val).typ, XmlXPathObjectType::XPathBoolean) {
        return val;
    }
    let ret: XmlXPathObjectPtr =
        xml_xpath_cache_new_boolean(ctxt, xml_xpath_cast_to_boolean(val) as i32);
    xml_xpath_release_object(ctxt, val);
    ret
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
    let mut cur: XmlXPathObjectPtr;

    CHECK_ARITY!(ctxt, nargs, 1);
    cur = value_pop(ctxt);
    if cur.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    cur = xml_xpath_cache_convert_boolean((*ctxt).context, cur);
    value_push(ctxt, cur);
}

/// Namespace nodes in libxml don't match the XPath semantic. In a node set
/// the namespace nodes are duplicated and the next pointer is set to the
/// parent node in the XPath semantic. Check if such a node needs to be freed
#[doc(alias = "xmlXPathNodeSetFreeNs")]
#[cfg(feature = "xpath")]
pub(crate) unsafe extern "C" fn xml_xpath_node_set_free_ns(ns: XmlNsPtr) {
    if ns.is_null() || !matches!((*ns).typ, XmlElementType::XmlNamespaceDecl) {
        return;
    }

    if !(*ns).next.is_null() && !matches!((*(*ns).next).typ, XmlElementType::XmlNamespaceDecl) {
        if !(*ns).href.is_null() {
            xml_free((*ns).href as _);
        }
        if !(*ns).prefix.is_null() {
            xml_free((*ns).prefix as _);
        }
        xml_free(ns as _);
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_value_pop() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                let ret_val = value_pop(ctxt);
                desret_xml_xpath_object_ptr(ret_val);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in valuePop",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in valuePop()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_value_push() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_value in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let value = gen_xml_xpath_object_ptr(n_value, 1);

                    let ret_val = value_push(ctxt, value);
                    desret_int(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_xpath_object_ptr(n_value, value, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in valuePush",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in valuePush()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_value);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_add_values() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                xml_xpath_add_values(ctxt);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathAddValues",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathAddValues()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_boolean_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_boolean_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathBooleanFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathBooleanFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_ceiling_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_ceiling_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathCeilingFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathCeilingFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_compare_values() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_inf in 0..GEN_NB_INT {
                    for n_strict in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                        let inf = gen_int(n_inf, 1);
                        let strict = gen_int(n_strict, 2);

                        let ret_val = xml_xpath_compare_values(ctxt, inf, strict);
                        desret_int(ret_val);
                        des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                        des_int(n_inf, inf, 1);
                        des_int(n_strict, strict, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlXPathCompareValues",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlXPathCompareValues()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_inf);
                            eprintln!(" {}", n_strict);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_concat_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_concat_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathConcatFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathConcatFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_contains_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_contains_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathContainsFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathContainsFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_count_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_count_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathCountFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathCountFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_debug_dump_comp_expr() {
        #[cfg(all(feature = "xpath", feature = "libxml_debug"))]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_FILE_PTR {
                for n_comp in 0..GEN_NB_XML_XPATH_COMP_EXPR_PTR {
                    for n_depth in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let mut output = gen_file_ptr(n_output, 0).unwrap();
                        let comp = gen_xml_xpath_comp_expr_ptr(n_comp, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_xpath_debug_dump_comp_expr(&mut output, comp, depth);
                        des_xml_xpath_comp_expr_ptr(n_comp, comp, 1);
                        des_int(n_depth, depth, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlXPathDebugDumpCompExpr",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlXPathDebugDumpCompExpr()"
                            );
                            eprint!(" {}", n_output);
                            eprint!(" {}", n_comp);
                            eprintln!(" {}", n_depth);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_debug_dump_object() {
        #[cfg(all(feature = "xpath", feature = "libxml_debug"))]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_FILE_PTR {
                for n_cur in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                    for n_depth in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let mut output = gen_file_ptr(n_output, 0).unwrap();
                        let cur = gen_xml_xpath_object_ptr(n_cur, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_xpath_debug_dump_object(&mut output, cur, depth);
                        des_xml_xpath_object_ptr(n_cur, cur, 1);
                        des_int(n_depth, depth, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlXPathDebugDumpObject",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlXPathDebugDumpObject()"
                            );
                            eprint!(" {}", n_output);
                            eprint!(" {}", n_cur);
                            eprintln!(" {}", n_depth);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_difference() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes1 in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_nodes2 in 0..GEN_NB_XML_NODE_SET_PTR {
                    let mem_base = xml_mem_blocks();
                    let nodes1 = gen_xml_node_set_ptr(n_nodes1, 0);
                    let nodes2 = gen_xml_node_set_ptr(n_nodes2, 1);

                    let ret_val = xml_xpath_difference(nodes1, nodes2);
                    desret_xml_node_set_ptr(ret_val);
                    des_xml_node_set_ptr(n_nodes1, nodes1, 0);
                    des_xml_node_set_ptr(n_nodes2, nodes2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathDifference",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathDifference()"
                        );
                        eprint!(" {}", n_nodes1);
                        eprintln!(" {}", n_nodes2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_distinct() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes in 0..GEN_NB_XML_NODE_SET_PTR {
                let mem_base = xml_mem_blocks();
                let nodes = gen_xml_node_set_ptr(n_nodes, 0);

                let ret_val = xml_xpath_distinct(nodes);
                desret_xml_node_set_ptr(ret_val);
                des_xml_node_set_ptr(n_nodes, nodes, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathDistinct",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathDistinct()");
                    eprintln!(" {}", n_nodes);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_distinct_sorted() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes in 0..GEN_NB_XML_NODE_SET_PTR {
                let mem_base = xml_mem_blocks();
                let nodes = gen_xml_node_set_ptr(n_nodes, 0);

                let ret_val = xml_xpath_distinct_sorted(nodes);
                desret_xml_node_set_ptr(ret_val);
                des_xml_node_set_ptr(n_nodes, nodes, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathDistinctSorted",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathDistinctSorted()"
                    );
                    eprintln!(" {}", n_nodes);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_div_values() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                xml_xpath_div_values(ctxt);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathDivValues",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathDivValues()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_equal_values() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                let ret_val = xml_xpath_equal_values(ctxt);
                desret_int(ret_val);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathEqualValues",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathEqualValues()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_err() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_error in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let error = gen_int(n_error, 1);

                    xml_xpath_err(ctxt, error);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_error, error, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathErr",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathErr()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_error);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_eval_expr() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                xml_xpath_eval_expr(ctxt);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathEvalExpr",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathEvalExpr()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_evaluate_predicate_result() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_res in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let res = gen_xml_xpath_object_ptr(n_res, 1);

                    let ret_val = xml_xpath_evaluate_predicate_result(ctxt, res);
                    desret_int(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_xpath_object_ptr(n_res, res, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathEvaluatePredicateResult",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathEvaluatePredicateResult()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_res);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_false_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_false_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathFalseFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathFalseFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_floor_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_floor_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathFloorFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathFloorFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_function_lookup() {

        /* missing type support */
    }

    #[test]
    fn test_xml_xpath_function_lookup_ns() {

        /* missing type support */
    }

    #[test]
    fn test_xml_xpath_has_same_nodes() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes1 in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_nodes2 in 0..GEN_NB_XML_NODE_SET_PTR {
                    let mem_base = xml_mem_blocks();
                    let nodes1 = gen_xml_node_set_ptr(n_nodes1, 0);
                    let nodes2 = gen_xml_node_set_ptr(n_nodes2, 1);

                    let ret_val = xml_xpath_has_same_nodes(nodes1, nodes2);
                    desret_int(ret_val);
                    des_xml_node_set_ptr(n_nodes1, nodes1, 0);
                    des_xml_node_set_ptr(n_nodes2, nodes2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathHasSameNodes",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathHasSameNodes()"
                        );
                        eprint!(" {}", n_nodes1);
                        eprintln!(" {}", n_nodes2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_id_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_id_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathIdFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathIdFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_intersection() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes1 in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_nodes2 in 0..GEN_NB_XML_NODE_SET_PTR {
                    let mem_base = xml_mem_blocks();
                    let nodes1 = gen_xml_node_set_ptr(n_nodes1, 0);
                    let nodes2 = gen_xml_node_set_ptr(n_nodes2, 1);

                    let ret_val = xml_xpath_intersection(nodes1, nodes2);
                    desret_xml_node_set_ptr(ret_val);
                    des_xml_node_set_ptr(n_nodes1, nodes1, 0);
                    des_xml_node_set_ptr(n_nodes2, nodes2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathIntersection",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathIntersection()"
                        );
                        eprint!(" {}", n_nodes1);
                        eprintln!(" {}", n_nodes2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_is_node_type() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let name = gen_const_xml_char_ptr(n_name, 0);

                let ret_val = xml_xpath_is_node_type(name as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_name, name, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathIsNodeType",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathIsNodeType()"
                    );
                    eprintln!(" {}", n_name);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_lang_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_lang_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathLangFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathLangFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_last_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_last_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathLastFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathLastFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_leading() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes1 in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_nodes2 in 0..GEN_NB_XML_NODE_SET_PTR {
                    let mem_base = xml_mem_blocks();
                    let nodes1 = gen_xml_node_set_ptr(n_nodes1, 0);
                    let nodes2 = gen_xml_node_set_ptr(n_nodes2, 1);

                    let ret_val = xml_xpath_leading(nodes1, nodes2);
                    desret_xml_node_set_ptr(ret_val);
                    des_xml_node_set_ptr(n_nodes1, nodes1, 0);
                    des_xml_node_set_ptr(n_nodes2, nodes2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathLeading",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathLeading()");
                        eprint!(" {}", n_nodes1);
                        eprintln!(" {}", n_nodes2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_leading_sorted() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes1 in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_nodes2 in 0..GEN_NB_XML_NODE_SET_PTR {
                    let mem_base = xml_mem_blocks();
                    let nodes1 = gen_xml_node_set_ptr(n_nodes1, 0);
                    let nodes2 = gen_xml_node_set_ptr(n_nodes2, 1);

                    let ret_val = xml_xpath_leading_sorted(nodes1, nodes2);
                    desret_xml_node_set_ptr(ret_val);
                    des_xml_node_set_ptr(n_nodes1, nodes1, 0);
                    des_xml_node_set_ptr(n_nodes2, nodes2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathLeadingSorted",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathLeadingSorted()"
                        );
                        eprint!(" {}", n_nodes1);
                        eprintln!(" {}", n_nodes2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_local_name_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_local_name_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathLocalNameFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathLocalNameFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_mod_values() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                xml_xpath_mod_values(ctxt);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathModValues",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathModValues()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_mult_values() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                xml_xpath_mult_values(ctxt);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathMultValues",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathMultValues()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_namespace_urifunction() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_namespace_uri_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNamespaceURIFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNamespaceURIFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_new_boolean() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let val = gen_int(n_val, 0);

                let ret_val = xml_xpath_new_boolean(val);
                desret_xml_xpath_object_ptr(ret_val);
                des_int(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathNewBoolean",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathNewBoolean()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_new_cstring() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_CONST_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_const_char_ptr(n_val, 0);

                let ret_val = xml_xpath_new_cstring(val);
                desret_xml_xpath_object_ptr(ret_val);
                des_const_char_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathNewCString",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathNewCString()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_new_float() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_DOUBLE {
                let mem_base = xml_mem_blocks();
                let val = gen_double(n_val, 0);

                let ret_val = xml_xpath_new_float(val);
                desret_xml_xpath_object_ptr(ret_val);
                des_double(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathNewFloat",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathNewFloat()");
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_new_node_set() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_xml_node_ptr(n_val, 0);

                let ret_val = xml_xpath_new_node_set(val);
                desret_xml_xpath_object_ptr(ret_val);
                des_xml_node_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathNewNodeSet",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathNewNodeSet()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_new_node_set_list() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_NODE_SET_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_xml_node_set_ptr(n_val, 0);

                let ret_val = xml_xpath_new_node_set_list(val);
                desret_xml_xpath_object_ptr(ret_val);
                des_xml_node_set_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathNewNodeSetList",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathNewNodeSetList()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_new_parser_context() {

        /* missing type support */
    }

    #[test]
    fn test_xml_xpath_new_string() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_const_xml_char_ptr(n_val, 0);

                let ret_val = xml_xpath_new_string(val as *const XmlChar);
                desret_xml_xpath_object_ptr(ret_val);
                des_const_xml_char_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathNewString",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathNewString()");
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_ancestor() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_ancestor(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextAncestor",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNextAncestor()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_ancestor_or_self() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_ancestor_or_self(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextAncestorOrSelf",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNextAncestorOrSelf()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_attribute() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_attribute(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextAttribute",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNextAttribute()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_child() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_child(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextChild",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathNextChild()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_descendant() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_descendant(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextDescendant",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNextDescendant()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_descendant_or_self() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_descendant_or_self(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextDescendantOrSelf",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNextDescendantOrSelf()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_following() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_following(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextFollowing",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNextFollowing()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_following_sibling() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_following_sibling(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextFollowingSibling",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNextFollowingSibling()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_namespace() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_namespace(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextNamespace",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNextNamespace()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_parent() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_parent(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextParent",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNextParent()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_preceding() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_preceding(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextPreceding",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNextPreceding()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_preceding_sibling() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_preceding_sibling(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextPrecedingSibling",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNextPrecedingSibling()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_next_self() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xml_xpath_next_self(ctxt, cur);
                    desret_xml_node_ptr(ret_val);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNextSelf",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathNextSelf()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_leading() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let nodes = gen_xml_node_set_ptr(n_nodes, 0);
                    let node = gen_xml_node_ptr(n_node, 1);

                    let ret_val = xml_xpath_node_leading(nodes, node);
                    desret_xml_node_set_ptr(ret_val);
                    des_xml_node_set_ptr(n_nodes, nodes, 0);
                    des_xml_node_ptr(n_node, node, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNodeLeading",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNodeLeading()"
                        );
                        eprint!(" {}", n_nodes);
                        eprintln!(" {}", n_node);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_leading_sorted() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let nodes = gen_xml_node_set_ptr(n_nodes, 0);
                    let node = gen_xml_node_ptr(n_node, 1);

                    let ret_val = xml_xpath_node_leading_sorted(nodes, node);
                    desret_xml_node_set_ptr(ret_val);
                    des_xml_node_set_ptr(n_nodes, nodes, 0);
                    des_xml_node_ptr(n_node, node, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNodeLeadingSorted",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNodeLeadingSorted()"
                        );
                        eprint!(" {}", n_nodes);
                        eprintln!(" {}", n_node);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_set_add() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_val in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_set_ptr(n_cur, 0);
                    let val = gen_xml_node_ptr(n_val, 1);

                    let ret_val = xml_xpath_node_set_add(cur, val);
                    desret_int(ret_val);
                    des_xml_node_set_ptr(n_cur, cur, 0);
                    des_xml_node_ptr(n_val, val, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNodeSetAdd",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNodeSetAdd()"
                        );
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_val);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_set_add_ns() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    for n_ns in 0..GEN_NB_XML_NS_PTR {
                        let mem_base = xml_mem_blocks();
                        let cur = gen_xml_node_set_ptr(n_cur, 0);
                        let node = gen_xml_node_ptr(n_node, 1);
                        let ns = gen_xml_ns_ptr(n_ns, 2);

                        let ret_val = xml_xpath_node_set_add_ns(cur, node, ns);
                        desret_int(ret_val);
                        des_xml_node_set_ptr(n_cur, cur, 0);
                        des_xml_node_ptr(n_node, node, 1);
                        des_xml_ns_ptr(n_ns, ns, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlXPathNodeSetAddNs",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlXPathNodeSetAddNs()"
                            );
                            eprint!(" {}", n_cur);
                            eprint!(" {}", n_node);
                            eprintln!(" {}", n_ns);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_set_add_unique() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_val in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_set_ptr(n_cur, 0);
                    let val = gen_xml_node_ptr(n_val, 1);

                    let ret_val = xml_xpath_node_set_add_unique(cur, val);
                    desret_int(ret_val);
                    des_xml_node_set_ptr(n_cur, cur, 0);
                    des_xml_node_ptr(n_val, val, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNodeSetAddUnique",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNodeSetAddUnique()"
                        );
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_val);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_set_contains() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_val in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_set_ptr(n_cur, 0);
                    let val = gen_xml_node_ptr(n_val, 1);

                    let ret_val = xml_xpath_node_set_contains(cur, val);
                    desret_int(ret_val);
                    des_xml_node_set_ptr(n_cur, cur, 0);
                    des_xml_node_ptr(n_val, val, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNodeSetContains",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNodeSetContains()"
                        );
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_val);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_set_del() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_val in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_set_ptr(n_cur, 0);
                    let val = gen_xml_node_ptr(n_val, 1);

                    xml_xpath_node_set_del(cur, val);
                    des_xml_node_set_ptr(n_cur, cur, 0);
                    des_xml_node_ptr(n_val, val, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNodeSetDel",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNodeSetDel()"
                        );
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_val);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_set_merge() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val1 in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_val2 in 0..GEN_NB_XML_NODE_SET_PTR {
                    let mem_base = xml_mem_blocks();
                    let val1 = gen_xml_node_set_ptr(n_val1, 0);
                    let val2 = gen_xml_node_set_ptr(n_val2, 1);

                    let ret_val = xml_xpath_node_set_merge(val1, val2);
                    desret_xml_node_set_ptr(ret_val);
                    des_xml_node_set_ptr(n_val1, val1, 0);
                    des_xml_node_set_ptr(n_val2, val2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNodeSetMerge",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNodeSetMerge()"
                        );
                        eprint!(" {}", n_val1);
                        eprintln!(" {}", n_val2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_set_remove() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_cur in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_val in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let cur = gen_xml_node_set_ptr(n_cur, 0);
                    let val = gen_int(n_val, 1);

                    xml_xpath_node_set_remove(cur, val);
                    des_xml_node_set_ptr(n_cur, cur, 0);
                    des_int(n_val, val, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNodeSetRemove",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNodeSetRemove()"
                        );
                        eprint!(" {}", n_cur);
                        eprintln!(" {}", n_val);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_set_sort() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_set in 0..GEN_NB_XML_NODE_SET_PTR {
                let mem_base = xml_mem_blocks();
                let set = gen_xml_node_set_ptr(n_set, 0);

                xml_xpath_node_set_sort(set);
                des_xml_node_set_ptr(n_set, set, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathNodeSetSort",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathNodeSetSort()"
                    );
                    eprintln!(" {}", n_set);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_trailing() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let nodes = gen_xml_node_set_ptr(n_nodes, 0);
                    let node = gen_xml_node_ptr(n_node, 1);

                    let ret_val = xml_xpath_node_trailing(nodes, node);
                    desret_xml_node_set_ptr(ret_val);
                    des_xml_node_set_ptr(n_nodes, nodes, 0);
                    des_xml_node_ptr(n_node, node, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNodeTrailing",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNodeTrailing()"
                        );
                        eprint!(" {}", n_nodes);
                        eprintln!(" {}", n_node);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_trailing_sorted() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let nodes = gen_xml_node_set_ptr(n_nodes, 0);
                    let node = gen_xml_node_ptr(n_node, 1);

                    let ret_val = xml_xpath_node_trailing_sorted(nodes, node);
                    desret_xml_node_set_ptr(ret_val);
                    des_xml_node_set_ptr(n_nodes, nodes, 0);
                    des_xml_node_ptr(n_node, node, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNodeTrailingSorted",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNodeTrailingSorted()"
                        );
                        eprint!(" {}", n_nodes);
                        eprintln!(" {}", n_node);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_normalize_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_normalize_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNormalizeFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNormalizeFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_not_equal_values() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                let ret_val = xml_xpath_not_equal_values(ctxt);
                desret_int(ret_val);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathNotEqualValues",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathNotEqualValues()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_not_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_not_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNotFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNotFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_ns_lookup() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 0);
                    let prefix = gen_const_xml_char_ptr(n_prefix, 1);

                    let ret_val = xml_xpath_ns_lookup(ctxt, prefix);
                    desret_const_xml_char_ptr(ret_val);
                    des_xml_xpath_context_ptr(n_ctxt, ctxt, 0);
                    des_const_xml_char_ptr(n_prefix, prefix, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNsLookup",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathNsLookup()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_prefix);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_number_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_number_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathNumberFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathNumberFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_parse_ncname() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                let ret_val = xml_xpath_parse_ncname(ctxt);
                desret_xml_char_ptr(ret_val);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathParseNCName",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathParseNCName()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_parse_name() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                let ret_val = xml_xpath_parse_name(ctxt);
                desret_xml_char_ptr(ret_val);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathParseName",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathParseName()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_pop_boolean() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                let ret_val = xml_xpath_pop_boolean(ctxt);
                desret_int(ret_val);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathPopBoolean",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathPopBoolean()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_pop_external() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                let ret_val = xml_xpath_pop_external(ctxt);
                desret_void_ptr(ret_val);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathPopExternal",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathPopExternal()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_pop_node_set() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                let ret_val = xml_xpath_pop_node_set(ctxt);
                desret_xml_node_set_ptr(ret_val);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathPopNodeSet",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathPopNodeSet()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_pop_number() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                let ret_val = xml_xpath_pop_number(ctxt);
                desret_double(ret_val);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathPopNumber",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathPopNumber()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_pop_string() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                let ret_val = xml_xpath_pop_string(ctxt);
                desret_xml_char_ptr(ret_val);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathPopString",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathPopString()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_position_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_position_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathPositionFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathPositionFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_register_all_functions() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 0);

                xml_xpath_register_all_functions(ctxt);
                des_xml_xpath_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathRegisterAllFunctions",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathRegisterAllFunctions()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_register_func() {

        /* missing type support */
    }

    #[test]
    fn test_xml_xpath_register_func_lookup() {

        /* missing type support */
    }

    #[test]
    fn test_xml_xpath_register_func_ns() {

        /* missing type support */
    }

    #[test]
    fn test_xml_xpath_register_ns() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_ns_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 0);
                        let prefix = gen_const_xml_char_ptr(n_prefix, 1);
                        let ns_uri = gen_const_xml_char_ptr(n_ns_uri, 2);

                        let ret_val = xml_xpath_register_ns(ctxt, prefix, ns_uri);
                        desret_int(ret_val);
                        des_xml_xpath_context_ptr(n_ctxt, ctxt, 0);
                        des_const_xml_char_ptr(n_prefix, prefix, 1);
                        des_const_xml_char_ptr(n_ns_uri, ns_uri, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlXPathRegisterNs",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlXPathRegisterNs()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_prefix);
                            eprintln!(" {}", n_ns_uri);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_register_variable() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_value in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let value = gen_xml_xpath_object_ptr(n_value, 2);

                        let ret_val = xml_xpath_register_variable(ctxt, name, value);
                        desret_int(ret_val);
                        des_xml_xpath_context_ptr(n_ctxt, ctxt, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_xml_xpath_object_ptr(n_value, value, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlXPathRegisterVariable",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlXPathRegisterVariable()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_value);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_register_variable_lookup() {

        /* missing type support */
    }

    #[test]
    fn test_xml_xpath_register_variable_ns() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_ns_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_value in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 0);
                            let name = gen_const_xml_char_ptr(n_name, 1);
                            let ns_uri = gen_const_xml_char_ptr(n_ns_uri, 2);
                            let value = gen_xml_xpath_object_ptr(n_value, 3);

                            let ret_val = xml_xpath_register_variable_ns(ctxt, name, ns_uri, value);
                            desret_int(ret_val);
                            des_xml_xpath_context_ptr(n_ctxt, ctxt, 0);
                            des_const_xml_char_ptr(n_name, name, 1);
                            des_const_xml_char_ptr(n_ns_uri, ns_uri, 2);
                            des_xml_xpath_object_ptr(n_value, value, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlXPathRegisterVariableNS",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlXPathRegisterVariableNS()"
                                );
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_name);
                                eprint!(" {}", n_ns_uri);
                                eprintln!(" {}", n_value);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_registered_funcs_cleanup() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 0);

                xml_xpath_registered_funcs_cleanup(ctxt);
                des_xml_xpath_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathRegisteredFuncsCleanup",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathRegisteredFuncsCleanup()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_registered_ns_cleanup() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 0);

                xml_xpath_registered_ns_cleanup(ctxt);
                des_xml_xpath_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathRegisteredNsCleanup",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathRegisteredNsCleanup()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_registered_variables_cleanup() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 0);

                xml_xpath_registered_variables_cleanup(ctxt);
                des_xml_xpath_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathRegisteredVariablesCleanup",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathRegisteredVariablesCleanup()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_root() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                xml_xpath_root(ctxt);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathRoot",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathRoot()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_round_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_round_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathRoundFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathRoundFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_starts_with_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_starts_with_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathStartsWithFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathStartsWithFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_string_eval_number() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let str = gen_const_xml_char_ptr(n_str, 0);

                let ret_val = xml_xpath_string_eval_number(str as *const XmlChar);
                desret_double(ret_val);
                des_const_xml_char_ptr(n_str, str, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathStringEvalNumber",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathStringEvalNumber()"
                    );
                    eprintln!(" {}", n_str);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_string_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_string_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathStringFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathStringFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_string_length_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_string_length_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathStringLengthFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathStringLengthFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_sub_values() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                xml_xpath_sub_values(ctxt);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathSubValues",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathSubValues()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_substring_after_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_substring_after_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathSubstringAfterFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathSubstringAfterFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_substring_before_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_substring_before_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathSubstringBeforeFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathSubstringBeforeFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_substring_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_substring_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathSubstringFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathSubstringFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_sum_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_sum_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathSumFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathSumFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_trailing() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes1 in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_nodes2 in 0..GEN_NB_XML_NODE_SET_PTR {
                    let mem_base = xml_mem_blocks();
                    let nodes1 = gen_xml_node_set_ptr(n_nodes1, 0);
                    let nodes2 = gen_xml_node_set_ptr(n_nodes2, 1);

                    let ret_val = xml_xpath_trailing(nodes1, nodes2);
                    desret_xml_node_set_ptr(ret_val);
                    des_xml_node_set_ptr(n_nodes1, nodes1, 0);
                    des_xml_node_set_ptr(n_nodes2, nodes2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathTrailing",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathTrailing()");
                        eprint!(" {}", n_nodes1);
                        eprintln!(" {}", n_nodes2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_trailing_sorted() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_nodes1 in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_nodes2 in 0..GEN_NB_XML_NODE_SET_PTR {
                    let mem_base = xml_mem_blocks();
                    let nodes1 = gen_xml_node_set_ptr(n_nodes1, 0);
                    let nodes2 = gen_xml_node_set_ptr(n_nodes2, 1);

                    let ret_val = xml_xpath_trailing_sorted(nodes1, nodes2);
                    desret_xml_node_set_ptr(ret_val);
                    des_xml_node_set_ptr(n_nodes1, nodes1, 0);
                    des_xml_node_set_ptr(n_nodes2, nodes2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathTrailingSorted",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathTrailingSorted()"
                        );
                        eprint!(" {}", n_nodes1);
                        eprintln!(" {}", n_nodes2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_translate_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_translate_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathTranslateFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathTranslateFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_true_function() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_nargs in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                    let nargs = gen_int(n_nargs, 1);

                    xml_xpath_true_function(ctxt, nargs);
                    des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                    des_int(n_nargs, nargs, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathTrueFunction",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathTrueFunction()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_nargs);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_value_flip_sign() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);

                xml_xpath_value_flip_sign(ctxt);
                des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathValueFlipSign",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathValueFlipSign()"
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_variable_lookup() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_xpath_variable_lookup(ctxt, name);
                    desret_xml_xpath_object_ptr(ret_val);
                    des_xml_xpath_context_ptr(n_ctxt, ctxt, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathVariableLookup",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathVariableLookup()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_variable_lookup_ns() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_ns_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let ns_uri = gen_const_xml_char_ptr(n_ns_uri, 2);

                        let ret_val = xml_xpath_variable_lookup_ns(ctxt, name, ns_uri);
                        desret_xml_xpath_object_ptr(ret_val);
                        des_xml_xpath_context_ptr(n_ctxt, ctxt, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_ns_uri, ns_uri, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlXPathVariableLookupNS",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlXPathVariableLookupNS()"
                            );
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_ns_uri);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_wrap_cstring() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_char_ptr(n_val, 0);

                let ret_val = xml_xpath_wrap_cstring(val);
                desret_xml_xpath_object_ptr(ret_val);
                des_char_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathWrapCString",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathWrapCString()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_wrap_external() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_VOID_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_void_ptr(n_val, 0);

                let ret_val = xml_xpath_wrap_external(val);
                desret_xml_xpath_object_ptr(ret_val);
                des_void_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathWrapExternal",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathWrapExternal()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_wrap_node_set() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_NODE_SET_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_xml_node_set_ptr(n_val, 0);

                let ret_val = xml_xpath_wrap_node_set(val);
                desret_xml_xpath_object_ptr(ret_val);
                des_xml_node_set_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathWrapNodeSet",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathWrapNodeSet()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpatherror() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_PARSER_CONTEXT_PTR {
                for n_file in 0..GEN_NB_FILEPATH {
                    for n_line in 0..GEN_NB_INT {
                        for n_no in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_xpath_parser_context_ptr(n_ctxt, 0);
                            let file = gen_filepath(n_file, 1);
                            let line = gen_int(n_line, 2);
                            let no = gen_int(n_no, 3);

                            xml_xpatherror(ctxt, file, line, no);
                            des_xml_xpath_parser_context_ptr(n_ctxt, ctxt, 0);
                            des_filepath(n_file, file, 1);
                            des_int(n_line, line, 2);
                            des_int(n_no, no, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlXPatherror",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlXPatherror()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_file);
                                eprint!(" {}", n_line);
                                eprintln!(" {}", n_no);
                            }
                        }
                    }
                }
            }
        }
    }
}

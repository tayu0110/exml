//! Provide internal methods and data structures for processing XPath.  
//! This module is based on `libxml/xpathInternals.h`, `xpath.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, c_uchar, c_uint, c_ulong, CStr},
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of, addr_of_mut, null, null_mut, NonNull},
    sync::atomic::{AtomicPtr, Ordering},
};

#[cfg(feature = "libxml_debug")]
use libc::{fprintf, FILE};
use libc::{memcpy, memset, ptrdiff_t, size_t, INT_MAX, INT_MIN};

#[cfg(feature = "libxml_xptr_locs")]
use crate::libxml::xpointer::{
    xml_xptr_free_location_set, xml_xptr_location_set_add, xml_xptr_location_set_create,
    xml_xptr_new_range, xml_xptr_new_range_node_object, xml_xptr_wrap_location_set,
    XmlLocationSetPtr,
};
use crate::{
    __xml_raise_error,
    error::{XmlErrorDomain, XmlErrorLevel},
    generic_error,
    libxml::{
        chvalid::{xml_is_blank_char, xml_is_char},
        dict::{xml_dict_lookup, xml_dict_reference, XmlDictPtr},
        globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
        hash::{
            xml_hash_add_entry, xml_hash_add_entry2, xml_hash_create, xml_hash_default_deallocator,
            xml_hash_free, xml_hash_lookup, xml_hash_lookup2, xml_hash_remove_entry,
            xml_hash_remove_entry2, xml_hash_update_entry, xml_hash_update_entry2, XmlHashTablePtr,
        },
        parser_internals::{xml_copy_char, XML_MAX_NAMELEN, XML_MAX_NAME_LENGTH},
        pattern::{
            xml_free_pattern, xml_free_pattern_list, xml_free_stream_ctxt, xml_pattern_from_root,
            xml_pattern_get_stream_ctxt, xml_pattern_max_depth, xml_pattern_min_depth,
            xml_pattern_streamable, xml_patterncompile, xml_stream_pop, xml_stream_push,
            xml_stream_push_node, xml_stream_wants_any_node, XmlPatternFlags, XmlPatternPtr,
            XmlStreamCtxtPtr,
        },
        tree::{
            xml_buf_content, xml_build_qname, xml_doc_get_root_element, xml_get_ns_list,
            xml_node_get_content, xml_node_get_lang, XmlAttrPtr, XmlBufPtr, XmlDocPtr,
            XmlElementType, XmlNodePtr, XmlNs, XmlNsPtr, XML_XML_NAMESPACE,
        },
        valid::xml_get_id,
        xmlerror::XmlParserErrors,
        xmlstring::{
            xml_str_equal, xml_strcat, xml_strchr, xml_strdup, xml_strlen, xml_strncmp,
            xml_strndup, xml_strstr, xml_utf8_strlen, xml_utf8_strloc, xml_utf8_strpos,
            xml_utf8_strsize, xml_utf8_strsub, XmlChar,
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
    private::buf::{xml_buf_add, xml_buf_create, xml_buf_free},
    xmlXPathNodeSetGetLength, xmlXPathNodeSetIsEmpty, xmlXPathNodeSetItem, xml_str_printf,
    IS_ASCII_DIGIT, IS_ASCII_LETTER, IS_CHAR_CH,
};

use super::{
    chvalid::{xml_is_combining, xml_is_digit, xml_is_extender},
    parser_internals::xml_is_letter,
};

/************************************************************************
 *									*
 *			Helpers						*
 *									*
 ************************************************************************/

/*
 * Many of these macros may later turn into functions. They
 * shouldn't be used in #ifdef's preprocessor instructions.
 */
/**
 * xmlXPathSetError:
 * @ctxt:  an XPath parser context
 * @err:  an XmlXPathError code
 *
 * Raises an error.
 */
macro_rules! xmlXPathSetError {
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

/**
 * xmlXPathSetTypeError:
 * @ctxt:  an XPath parser context
 *
 * Raises an XPATH_INVALID_TYPE error.
 */
macro_rules! xmlXPathSetTypeError {
    ($ctxt:expr) => {
        xmlXPathSetError!($ctxt, XmlXPathError::XpathInvalidType as i32)
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

/**
 * xmlXPathStackIsNodeSet:
 * @ctxt: an XPath parser context
 *
 * Check if the current value on the XPath stack is a node set or
 * an XSLT value tree.
 *
 * Returns true if the current object on the stack is a node-set.
 */
macro_rules! xmlXPathStackIsNodeSet {
    ($ctxt:expr) => {
        !(*$ctxt).value.is_null()
            && ((*(*$ctxt).value).typ == XmlXPathObjectType::XpathNodeset
                || (*(*$ctxt).value).typ == XmlXPathObjectType::XpathXsltTree)
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

/**
 * CHECK_ERROR:
 *
 * Macro to return from the function if an XPath error was detected.
 */
#[macro_export]
macro_rules! CHECK_ERROR {
    ($ctxt:expr) => {
        if (*$ctxt).error != $crate::libxml::xpath::XmlXPathError::XpathExpressionOk as i32 {
            return;
        }
    };
}

/**
 * CHECK_ERROR0:
 *
 * Macro to return 0 from the function if an XPath error was detected.
 */
macro_rules! CHECK_ERROR0 {
    ($ctxt:expr) => {
        if (*$ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
            return 0;
        }
    };
}

/**
 * XP_ERROR:
 * @X:  the error code
 *
 * Macro to raise an XPath error and return.
 */
#[macro_export]
macro_rules! XP_ERROR {
    ($ctxt:expr, $x:expr) => {{
        $crate::libxml::xpath_internals::xml_xpath_err($ctxt, $x);
        return;
    }};
}

/**
 * XP_ERROR0:
 * @X:  the error code
 *
 * Macro to raise an XPath error and return 0.
 */
macro_rules! XP_ERROR0 {
    ($ctxt:expr, $x:expr) => {{
        xml_xpath_err($ctxt, $x);
        return 0;
    }};
}

/**
 * CHECK_TYPE:
 * @typeval:  the XPath type
 *
 * Macro to check that the value on top of the XPath stack is of a given
 * type.
 */
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

/**
 * CHECK_TYPE0:
 * @typeval:  the XPath type
 *
 * Macro to check that the value on top of the XPath stack is of a given
 * type. Return(0) in case of failure
 */
macro_rules! CHECK_TYPE0 {
    ($ctxt:expr, $typeval:expr) => {
        if (*$ctxt).value.is_null() || (*(*$ctxt).value).typ != $typeval {
            XP_ERROR0!($ctxt, XmlXPathError::XpathInvalidType as i32)
        }
    };
}

/**
 * CHECK_ARITY:
 * @x:  the number of expected args
 *
 * Macro to check that the number of args passed to an XPath function matches.
 */
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

/**
 * CAST_TO_STRING:
 *
 * Macro to try to cast the value on the top of the XPath stack to a string.
 */
macro_rules! CAST_TO_STRING {
    ($ctxt:expr) => {
        if !(*$ctxt).value.is_null() && (*(*$ctxt).value).typ != XmlXPathObjectType::XpathString {
            xml_xpath_string_function($ctxt, 1);
        }
    };
}

/**
 * CAST_TO_NUMBER:
 *
 * Macro to try to cast the value on the top of the XPath stack to a number.
 */
macro_rules! CAST_TO_NUMBER {
    ($ctxt:expr) => {
        if !(*$ctxt).value.is_null() && (*(*$ctxt).value).typ != XmlXPathObjectType::XpathNumber {
            xml_xpath_number_function($ctxt, 1);
        }
    };
}

/**
 * CAST_TO_BOOLEAN:
 *
 * Macro to try to cast the value on the top of the XPath stack to a boolean.
 */
macro_rules! CAST_TO_BOOLEAN {
    ($ctxt:expr) => {
        if !(*$ctxt).value.is_null() && (*(*$ctxt).value).typ != XmlXPathObjectType::XpathBoolean {
            xml_xpath_boolean_function($ctxt, 1);
        }
    };
}

/**
 * XP_ERRORNULL:
 * @X:  the error code
 *
 * Macro to raise an XPath error and return NULL.
 */
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

/**
 * xsltPointerList:
 *
 * Pointer-list for various purposes.
 */
pub type XmlPointerListPtr = *mut XmlPointerList;
#[repr(C)]
pub struct XmlPointerList {
    pub(crate) items: *mut *mut c_void,
    pub(crate) number: c_int,
    pub(crate) size: c_int,
}

pub type XmlXpathContextCachePtr = *mut XmlXpathContextCache;
#[repr(C)]
pub struct XmlXpathContextCache {
    pub(crate) nodeset_objs: XmlPointerListPtr, /* contains xmlXPathObjectPtr */
    pub(crate) string_objs: XmlPointerListPtr,  /* contains xmlXPathObjectPtr */
    pub(crate) boolean_objs: XmlPointerListPtr, /* contains xmlXPathObjectPtr */
    pub(crate) number_objs: XmlPointerListPtr,  /* contains xmlXPathObjectPtr */
    pub(crate) misc_objs: XmlPointerListPtr,    /* contains xmlXPathObjectPtr */
    pub(crate) max_nodeset: c_int,
    pub(crate) max_string: c_int,
    pub(crate) max_boolean: c_int,
    pub(crate) max_number: c_int,
    pub(crate) max_misc: c_int,
    // #ifdef XP_DEBUG_OBJ_USAGE
    //     int dbgCachedAll;
    //     int dbgCachedNodeset;
    //     int dbgCachedString;
    //     int dbgCachedBool;
    //     int dbgCachedNumber;
    //     int dbgCachedPoint;
    //     int dbgCachedRange;
    //     int dbgCachedLocset;
    //     int dbgCachedUsers;
    //     int dbgCachedXSLTTree;
    //     int dbgCachedUndefined;

    //     int dbgReusedAll;
    //     int dbgReusedNodeset;
    //     int dbgReusedString;
    //     int dbgReusedBool;
    //     int dbgReusedNumber;
    //     int dbgReusedPoint;
    //     int dbgReusedRange;
    //     int dbgReusedLocset;
    //     int dbgReusedUsers;
    //     int dbgReusedXSLTTree;
    //     int dbgReusedUndefined;

    // #endif
}

/*
* TODO: Since such a list-handling is used in xmlschemas.c and libxslt
* and here, we should make the functions public.
*/
unsafe extern "C" fn xml_pointer_list_add_size(
    list: XmlPointerListPtr,
    item: *mut c_void,
    mut initial_size: c_int,
) -> c_int {
    if (*list).size <= (*list).number {
        let new_size: size_t;

        if (*list).size == 0 {
            if initial_size <= 0 {
                initial_size = 1;
            }
            new_size = initial_size as _;
        } else {
            if (*list).size > 50000000 {
                xml_xpath_err_memory(
                    null_mut(),
                    c"xmlPointerListAddSize: re-allocating item\n".as_ptr(),
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
                c"xmlPointerListAddSize: re-allocating item\n".as_ptr(),
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

/**
 * xsltPoc_interListCreate:
 *
 * Creates an xsltPoc_interList structure.
 *
 * Returns a xsltPoc_interList structure or NULL in case of an error.
 */
unsafe extern "C" fn xml_pointer_list_create(initial_size: c_int) -> XmlPointerListPtr {
    let ret: XmlPointerListPtr = xml_malloc(size_of::<XmlPointerList>()) as _;
    if ret.is_null() {
        xml_xpath_err_memory(
            null_mut(),
            c"xmlPointerListCreate: allocating item\n".as_ptr(),
        );
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

/**
 * xmlXPathReleaseObject:
 * @obj:  the let to: xmlXPathObjectPtr free or to cache
 *
 * Depending on the state of the cache this frees the given
 * XPath object or stores it in the cache.
 */
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
                    XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {
                        if !(*obj).nodesetval.is_null() {
                            if (*obj).boolval != 0 {
                                /*
                                 * It looks like the @boolval is used for
                                 * evaluation if this an XSLT Result Tree Fragment.
                                 * TODO: Check if this assumption is correct.
                                 */
                                (*obj).typ = XmlXPathObjectType::XpathXsltTree; /* just for debugging */
                                xml_xpath_free_value_tree((*obj).nodesetval);
                                (*obj).nodesetval = null_mut();
                            } else if (*(*obj).nodesetval).node_max <= 40
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
                    XmlXPathObjectType::XpathString => {
                        if !(*obj).stringval.is_null() {
                            xml_free((*obj).stringval as _);
                        }

                        if XP_CACHE_WANTS!((*cache).string_objs, (*cache).max_string) {
                            XP_CACHE_ADD!((*cache).string_objs, obj);
                            break 'obj_cached;
                        }
                    }
                    XmlXPathObjectType::XpathBoolean => {
                        if XP_CACHE_WANTS!((*cache).boolean_objs, (*cache).max_boolean) {
                            XP_CACHE_ADD!((*cache).boolean_objs, obj);
                            break 'obj_cached;
                        }
                    }
                    XmlXPathObjectType::XpathNumber => {
                        if XP_CACHE_WANTS!((*cache).number_objs, (*cache).max_number) {
                            XP_CACHE_ADD!((*cache).number_objs, obj);
                            break 'obj_cached;
                        }
                    }
                    #[cfg(feature = "libxml_xptr_locs")]
                    XmlXPathObjectType::XpathLocationset => {
                        if !(*obj).user.is_null() {
                            xml_xptr_free_location_set((*obj).user as _);
                        }
                        break 'free_obj;
                    }
                    _ => {
                        break 'free_obj;
                    }
                }

                /*
                 * Fallback to adding to the misc-objects slot.
                 */
                if XP_CACHE_WANTS!((*cache).misc_objs, (*cache).max_misc) {
                    XP_CACHE_ADD!((*cache).misc_objs, obj);
                } else {
                    break 'free_obj;
                }
            }

            // obj_cached:

            // #ifdef XP_DEBUG_OBJ_USAGE
            // 	xmlXPathDebugObjUsageReleased(ctxt, (*obj).typ);
            // #endif

            if !(*obj).nodesetval.is_null() {
                let tmpset: XmlNodeSetPtr = (*obj).nodesetval;

                /*
                 * TODO: Due to those nasty ns-nodes, we need to traverse
                 *  the list and free the ns-nodes.
                 * URGENT TODO: Check if it's actually slowing things down.
                 *  Maybe we shouldn't try to preserve the list.
                 */
                if (*tmpset).node_nr > 1 {
                    let mut node: XmlNodePtr;

                    for i in 0..(*tmpset).node_nr {
                        node = *(*tmpset).node_tab.add(i as usize);
                        if !node.is_null()
                            && matches!((*node).typ, XmlElementType::XmlNamespaceDecl)
                        {
                            xml_xpath_node_set_free_ns(node as XmlNsPtr);
                        }
                    }
                } else if (*tmpset).node_nr == 1
                    && (!(*(*tmpset).node_tab.add(0)).is_null()
                        && matches!(
                            (*(*(*tmpset).node_tab.add(0))).typ,
                            XmlElementType::XmlNamespaceDecl
                        ))
                {
                    xml_xpath_node_set_free_ns(*(*tmpset).node_tab.add(0) as XmlNsPtr);
                }
                (*tmpset).node_nr = 0;
                memset(obj as _, 0, size_of::<XmlXPathObject>());
                (*obj).nodesetval = tmpset;
            } else {
                memset(obj as _, 0, size_of::<XmlXPathObject>());
            }

            return;
        }

        // free_obj:
        /*
         * Cache is full; free the object.
         */
        if !(*obj).nodesetval.is_null() {
            xml_xpath_free_node_set((*obj).nodesetval);
        }
        // #ifdef XP_DEBUG_OBJ_USAGE
        // 	xmlXPathDebugObjUsageReleased(NULL, (*obj).typ);
        // #endif
        xml_free(obj as _);
    }
}

/**
 * xmlXPathPopBoolean:
 * @ctxt:  an XPath parser context
 *
 * Pops a boolean from the stack, handling conversion if needed.
 * Check error with #xmlXPathCheckError.
 *
 * Returns the boolean
 */
pub unsafe extern "C" fn xml_xpath_pop_boolean(ctxt: XmlXPathParserContextPtr) -> c_int {
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    if obj.is_null() {
        xmlXPathSetError!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
        return 0;
    }
    let ret = if (*obj).typ != XmlXPathObjectType::XpathBoolean {
        xml_xpath_cast_to_boolean(obj)
    } else {
        (*obj).boolval
    };
    xml_xpath_release_object((*ctxt).context, obj);
    ret
}

/**
 * xmlXPathPopNumber:
 * @ctxt:  an XPath parser context
 *
 * Pops a number from the stack, handling conversion if needed.
 * Check error with #xmlXPathCheckError.
 *
 * Returns the number
 */
pub unsafe extern "C" fn xml_xpath_pop_number(ctxt: XmlXPathParserContextPtr) -> f64 {
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    if obj.is_null() {
        xmlXPathSetError!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
        return 0.0;
    }
    let ret = if (*obj).typ != XmlXPathObjectType::XpathNumber {
        xml_xpath_cast_to_number(obj)
    } else {
        (*obj).floatval
    };
    xml_xpath_release_object((*ctxt).context, obj);
    ret
}

/**
 * xmlXPathPopString:
 * @ctxt:  an XPath parser context
 *
 * Pops a string from the stack, handling conversion if needed.
 * Check error with #xmlXPathCheckError.
 *
 * Returns the string
 */
pub unsafe extern "C" fn xml_xpath_pop_string(ctxt: XmlXPathParserContextPtr) -> *mut XmlChar {
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    if obj.is_null() {
        xmlXPathSetError!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
        return null_mut();
    }
    let ret: *mut XmlChar = xml_xpath_cast_to_string(obj); /* this does required strdup */
    /* TODO: needs refactoring somewhere else */
    if (*obj).stringval == ret {
        (*obj).stringval = null_mut();
    }
    xml_xpath_release_object((*ctxt).context, obj);
    ret
}

/**
 * xmlXPathPopNodeSet:
 * @ctxt:  an XPath parser context
 *
 * Pops a node-set from the stack, handling conversion if needed.
 * Check error with #xmlXPathCheckError.
 *
 * Returns the node-set
 */
pub unsafe extern "C" fn xml_xpath_pop_node_set(ctxt: XmlXPathParserContextPtr) -> XmlNodeSetPtr {
    if ctxt.is_null() {
        return null_mut();
    }
    if (*ctxt).value.is_null() {
        xmlXPathSetError!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
        return null_mut();
    }
    if !xmlXPathStackIsNodeSet!(ctxt) {
        xmlXPathSetTypeError!(ctxt);
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

/**
 * xmlXPathPopExternal:
 * @ctxt:  an XPath parser context
 *
 * Pops an external object from the stack, handling conversion if needed.
 * Check error with #xmlXPathCheckError.
 *
 * Returns the object
 */
pub unsafe extern "C" fn xml_xpath_pop_external(ctxt: XmlXPathParserContextPtr) -> *mut c_void {
    if ctxt.is_null() || (*ctxt).value.is_null() {
        xmlXPathSetError!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
        return null_mut();
    }
    if (*(*ctxt).value).typ != XmlXPathObjectType::XpathUsers {
        xmlXPathSetTypeError!(ctxt);
        return null_mut();
    }
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    let ret: *mut c_void = (*obj).user;
    (*obj).user = null_mut();
    xml_xpath_release_object((*ctxt).context, obj);
    ret
}

/*
 * Variable Lookup forwarding.
 */

/**
 * xmlXPathRegisterVariableLookup:
 * @ctxt:  the XPath context
 * @f:  the lookup function
 * @data:  the lookup data
 *
 * register an external mechanism to do variable lookup
 */
pub unsafe extern "C" fn xml_xpath_register_variable_lookup(
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

/*
 * Function Lookup forwarding.
 */

/**
 * xmlXPathRegisterFuncLookup:
 * @ctxt:  the XPath context
 * @f:  the lookup function
 * @funcCtxt:  the lookup data
 *
 * Registers an external mechanism to do function lookup.
 */
pub unsafe extern "C" fn xml_xpath_register_func_lookup(
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

/*
 * Error reporting.
 */
/**
 * xmlXPatherror:
 * @ctxt:  the XPath Parser context
 * @file:  the file name
 * @line:  the line number
 * @no:  the error number
 *
 * Formats an error message.
 */
pub unsafe extern "C" fn xml_xpatherror(
    ctxt: XmlXPathParserContextPtr,
    _file: *const c_char,
    _line: c_int,
    no: c_int,
) {
    xml_xpath_err(ctxt, no);
}

/*
 * The array xmlXPathErrorMessages corresponds to the enum XmlXPathError
 */
const XML_XPATH_ERROR_MESSAGES: &[*const c_char] = &[
    c"Ok\n".as_ptr(),
    c"Number encoding\n".as_ptr(),
    c"Unfinished literal\n".as_ptr(),
    c"Start of literal\n".as_ptr(),
    c"Expected $ for variable reference\n".as_ptr(),
    c"Undefined variable\n".as_ptr(),
    c"Invalid predicate\n".as_ptr(),
    c"Invalid expression\n".as_ptr(),
    c"Missing closing curly brace\n".as_ptr(),
    c"Unregistered function\n".as_ptr(),
    c"Invalid operand\n".as_ptr(),
    c"Invalid type\n".as_ptr(),
    c"Invalid number of arguments\n".as_ptr(),
    c"Invalid context size\n".as_ptr(),
    c"Invalid context position\n".as_ptr(),
    c"Memory allocation error\n".as_ptr(),
    c"Syntax error\n".as_ptr(),
    c"Resource error\n".as_ptr(),
    c"Sub resource error\n".as_ptr(),
    c"Undefined namespace prefix\n".as_ptr(),
    c"Encoding error\n".as_ptr(),
    c"Char out of XML range\n".as_ptr(),
    c"Invalid or incomplete context\n".as_ptr(),
    c"Stack usage error\n".as_ptr(),
    c"Forbidden variable\n".as_ptr(),
    c"Operation limit exceeded\n".as_ptr(),
    c"Recursion limit exceeded\n".as_ptr(),
    c"?? Unknown error ??\n".as_ptr(), /* Must be last in the list! */
];
const MAXERRNO: i32 = XML_XPATH_ERROR_MESSAGES.len() as i32 - 1;

/**
 * xmlXPathErr:
 * @ctxt:  a XPath parser context
 * @error:  the error code
 *
 * Handle an XPath error
 */
pub unsafe extern "C" fn xml_xpath_err(ctxt: XmlXPathParserContextPtr, mut error: c_int) {
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
            null(),
            0,
            None,
            None,
            None,
            0,
            0,
            c"%s".as_ptr(),
            XML_XPATH_ERROR_MESSAGES[error as usize]
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
            null(),
            0,
            (!(*ctxt).base.is_null()).then(|| CStr::from_ptr((*ctxt).base as *const i8)
                .to_string_lossy()
                .into_owned()
                .into()),
            None,
            None,
            (*ctxt).cur.offset_from((*ctxt).base) as _,
            0,
            c"%s".as_ptr(),
            XML_XPATH_ERROR_MESSAGES[error as usize]
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
            null(),
            0,
            (!(*ctxt).base.is_null()).then(|| CStr::from_ptr((*ctxt).base as *const i8)
                .to_string_lossy()
                .into_owned()
                .into()),
            None,
            None,
            (*ctxt).cur.offset_from((*ctxt).base) as _,
            0,
            c"%s".as_ptr(),
            XML_XPATH_ERROR_MESSAGES[error as usize]
        );
    }
}

#[cfg(feature = "libxml_debug")]
unsafe extern "C" fn xml_xpath_debug_dump_node(output: *mut FILE, cur: XmlNodePtr, depth: c_int) {
    use crate::libxml::debug_xml::{xml_debug_dump_attr, xml_debug_dump_one_node};

    let mut shift: [c_char; 100] = [0; 100];

    for i in 0..depth.min(25) {
        shift[2 * i as usize] = b' ' as _;
        shift[2 * i as usize + 1] = b' ' as _;
    }
    shift[2 * depth.clamp(0, 25) as usize] = 0;
    shift[2 * depth.clamp(0, 25) as usize + 1] = 0;

    if cur.is_null() {
        fprintf(output, c"%s".as_ptr(), shift.as_ptr());
        fprintf(output, c"Node is NULL !\n".as_ptr());
        return;
    }

    if (*cur).typ == XmlElementType::XmlDocumentNode
        || (*cur).typ == XmlElementType::XmlHtmlDocumentNode
    {
        fprintf(output, c"%s".as_ptr(), shift.as_ptr());
        fprintf(output, c" /\n".as_ptr());
    } else if (*cur).typ == XmlElementType::XmlAttributeNode {
        xml_debug_dump_attr(output, cur as XmlAttrPtr, depth);
    } else {
        xml_debug_dump_one_node(output, cur, depth);
    }
}

#[cfg(feature = "libxml_debug")]
unsafe extern "C" fn xml_xpath_debug_dump_node_list(
    output: *mut FILE,
    mut cur: XmlNodePtr,
    depth: c_int,
) {
    use super::debug_xml::xml_debug_dump_one_node;

    let mut tmp: XmlNodePtr;
    let mut shift: [c_char; 100] = [0; 100];

    for i in 0..depth.min(25) {
        shift[2 * i as usize] = b' ' as _;
        shift[2 * i as usize + 1] = b' ' as _;
    }
    shift[2 * depth.clamp(0, 25) as usize] = 0;
    shift[2 * depth.clamp(0, 25) as usize + 1] = 0;

    if cur.is_null() {
        fprintf(output, c"%s".as_ptr(), shift.as_ptr());
        fprintf(output, c"Node is NULL !\n".as_ptr());
        return;
    }

    while !cur.is_null() {
        tmp = cur;
        cur = (*cur).next;
        xml_debug_dump_one_node(output, tmp, depth);
    }
}

#[cfg(feature = "libxml_debug")]
unsafe extern "C" fn xml_xpath_debug_dump_node_set(
    output: *mut FILE,
    cur: XmlNodeSetPtr,
    depth: c_int,
) {
    let mut shift: [c_char; 100] = [0; 100];

    if output.is_null() {
        return;
    }

    for i in 0..depth.min(25) {
        shift[2 * i as usize] = b' ' as _;
        shift[2 * i as usize + 1] = b' ' as _;
    }
    shift[2 * depth.clamp(0, 25) as usize] = 0;
    shift[2 * depth.clamp(0, 25) as usize + 1] = 0;

    if cur.is_null() {
        fprintf(output, c"%s".as_ptr(), shift.as_ptr());
        fprintf(output, c"NodeSet is NULL !\n".as_ptr());
        return;
    }

    if !cur.is_null() {
        fprintf(output, c"Set contains %d nodes:\n".as_ptr(), (*cur).node_nr);
        for i in 0..(*cur).node_nr {
            fprintf(output, c"%s".as_ptr(), shift.as_ptr());
            fprintf(output, c"%d".as_ptr(), i + 1);
            xml_xpath_debug_dump_node(output, *(*cur).node_tab.add(i as usize), depth + 1);
        }
    }
}

#[cfg(feature = "libxml_debug")]
unsafe extern "C" fn xml_xpath_debug_dump_value_tree(
    output: *mut FILE,
    cur: XmlNodeSetPtr,
    depth: c_int,
) {
    let mut shift: [c_char; 100] = [0; 100];

    for i in 0..depth.min(25) {
        shift[2 * i as usize] = b' ' as _;
        shift[2 * i as usize + 1] = b' ' as _;
    }
    shift[2 * depth.clamp(0, 25) as usize] = 0;
    shift[2 * depth.clamp(0, 25) as usize + 1] = 0;

    if cur.is_null() || (*cur).node_nr == 0 || (*(*cur).node_tab.add(0)).is_null() {
        fprintf(output, c"%s".as_ptr(), shift.as_ptr());
        fprintf(output, c"Value Tree is NULL !\n".as_ptr());
        return;
    }

    fprintf(output, c"%s".as_ptr(), shift.as_ptr());
    fprintf(output, c"%d".as_ptr(), depth.clamp(0, 25) + 1);
    xml_xpath_debug_dump_node_list(output, (*(*(*cur).node_tab.add(0))).children, depth + 1);
}

#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xpath_debug_dump_location_set(
    output: *mut FILE,
    cur: XmlLocationSetPtr,
    depth: c_int,
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

/**
 * xmlXPathDebugDumpObject:
 * @output:  the FILE * to dump the output
 * @cur:  the object to inspect
 * @depth:  indentation level
 *
 * Dump the content of the object for debugging purposes
 */
#[cfg(feature = "libxml_debug")]
pub unsafe extern "C" fn xml_xpath_debug_dump_object(
    output: *mut FILE,
    cur: XmlXPathObjectPtr,
    depth: c_int,
) {
    use super::debug_xml::xml_debug_dump_string;

    let mut shift: [c_char; 100] = [0; 100];

    if output.is_null() {
        return;
    }

    for i in 0..depth.min(25) {
        shift[2 * i as usize] = b' ' as _;
        shift[2 * i as usize + 1] = b' ' as _;
    }
    shift[2 * depth.clamp(0, 25) as usize] = 0;
    shift[2 * depth.clamp(0, 25) as usize + 1] = 0;

    fprintf(output, c"%s".as_ptr(), shift.as_ptr());

    if cur.is_null() {
        fprintf(output, c"Object is empty (NULL)\n".as_ptr());
        return;
    }
    match (*cur).typ {
        XmlXPathObjectType::XpathUndefined => {
            fprintf(output, c"Object is uninitialized\n".as_ptr());
        }
        XmlXPathObjectType::XpathNodeset => {
            fprintf(output, c"Object is a Node Set :\n".as_ptr());
            xml_xpath_debug_dump_node_set(output, (*cur).nodesetval, depth);
        }
        XmlXPathObjectType::XpathXsltTree => {
            fprintf(output, c"Object is an XSLT value tree :\n".as_ptr());
            xml_xpath_debug_dump_value_tree(output, (*cur).nodesetval, depth);
        }
        XmlXPathObjectType::XpathBoolean => {
            fprintf(output, c"Object is a Boolean : ".as_ptr());
            if (*cur).boolval != 0 {
                fprintf(output, c"true\n".as_ptr());
            } else {
                fprintf(output, c"false\n".as_ptr());
            }
        }
        XmlXPathObjectType::XpathNumber => {
            match xml_xpath_is_inf((*cur).floatval) {
                1 => {
                    fprintf(output, c"Object is a number : Infinity\n".as_ptr());
                }
                -1 => {
                    fprintf(output, c"Object is a number : -Infinity\n".as_ptr());
                }
                _ => {
                    if xml_xpath_is_nan((*cur).floatval) != 0 {
                        fprintf(output, c"Object is a number : NaN\n".as_ptr());
                    } else if (*cur).floatval == 0.0 {
                        /* Omit sign for negative zero. */
                        fprintf(output, c"Object is a number : 0\n".as_ptr());
                    } else {
                        fprintf(
                            output,
                            c"Object is a number : %0g\n".as_ptr(),
                            (*cur).floatval,
                        );
                    }
                }
            }
        }
        XmlXPathObjectType::XpathString => {
            fprintf(output, c"Object is a string : ".as_ptr());
            xml_debug_dump_string(output, (*cur).stringval);
            fprintf(output, c"\n".as_ptr());
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XpathPoint => {
            fprintf(
                output,
                c"Object is a point : index %d in node".as_ptr(),
                (*cur).index,
            );
            xml_xpath_debug_dump_node(output, (*cur).user as XmlNodePtr, depth + 1);
            fprintf(output, c"\n".as_ptr());
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XpathRange => {
            if (*cur).user2.is_null()
                || ((*cur).user2 == (*cur).user && (*cur).index == (*cur).index2)
            {
                fprintf(output, c"Object is a collapsed range :\n".as_ptr());
                fprintf(output, c"%s".as_ptr(), shift.as_ptr());
                if (*cur).index >= 0 {
                    fprintf(output, c"index %d in ".as_ptr(), (*cur).index);
                }
                fprintf(output, c"node\n".as_ptr());
                xml_xpath_debug_dump_node(output, (*cur).user as XmlNodePtr, depth + 1);
            } else {
                fprintf(output, c"Object is a range :\n".as_ptr());
                fprintf(output, c"%s".as_ptr(), shift.as_ptr());
                fprintf(output, c"From ".as_ptr());
                if (*cur).index >= 0 {
                    fprintf(output, c"index %d in ".as_ptr(), (*cur).index);
                }
                fprintf(output, c"node\n".as_ptr());
                xml_xpath_debug_dump_node(output, (*cur).user as XmlNodePtr, depth + 1);
                fprintf(output, c"%s".as_ptr(), shift.as_ptr());
                fprintf(output, c"To ".as_ptr());
                if (*cur).index2 >= 0 {
                    fprintf(output, c"index %d in ".as_ptr(), (*cur).index2);
                }
                fprintf(output, c"node\n".as_ptr());
                xml_xpath_debug_dump_node(output, (*cur).user2 as XmlNodePtr, depth + 1);
                fprintf(output, c"\n".as_ptr());
            }
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XpathLocationset => {
            fprintf(output, c"Object is a Location Set:\n".as_ptr());
            xml_xpath_debug_dump_location_set(output, (*cur).user as XmlLocationSetPtr, depth);
        }
        XmlXPathObjectType::XpathUsers => {
            fprintf(output, c"Object is user defined\n".as_ptr());
        }
    }
}

unsafe extern "C" fn xml_xpath_debug_dump_step_op(
    output: *mut FILE,
    comp: XmlXPathCompExprPtr,
    op: XmlXPathStepOpPtr,
    depth: c_int,
) {
    let mut shift: [c_char; 100] = [0; 100];

    for i in 0..depth.min(25) {
        shift[2 * i as usize] = b' ' as _;
        shift[2 * i as usize + 1] = b' ' as _;
    }
    shift[2 * depth.clamp(0, 25) as usize] = 0;
    shift[2 * depth.clamp(0, 25) as usize + 1] = 0;

    fprintf(output, c"%s".as_ptr(), shift.as_ptr());
    if op.is_null() {
        fprintf(output, c"Step is NULL\n".as_ptr());
        return;
    }
    match (*op).op {
        XmlXPathOp::XpathOpEnd => {
            fprintf(output, c"END".as_ptr());
        }
        XmlXPathOp::XpathOpAnd => {
            fprintf(output, c"AND".as_ptr());
        }
        XmlXPathOp::XpathOpOr => {
            fprintf(output, c"OR".as_ptr());
        }
        XmlXPathOp::XpathOpEqual => {
            if (*op).value != 0 {
                fprintf(output, c"EQUAL =".as_ptr());
            } else {
                fprintf(output, c"EQUAL !=".as_ptr());
            }
        }
        XmlXPathOp::XpathOpCmp => {
            if (*op).value != 0 {
                fprintf(output, c"CMP <".as_ptr());
            } else {
                fprintf(output, c"CMP >".as_ptr());
            }
            if (*op).value2 == 0 {
                fprintf(output, c"=".as_ptr());
            }
        }
        XmlXPathOp::XpathOpPlus => {
            if (*op).value == 0 {
                fprintf(output, c"PLUS -".as_ptr());
            } else if (*op).value == 1 {
                fprintf(output, c"PLUS +".as_ptr());
            } else if (*op).value == 2 {
                fprintf(output, c"PLUS unary -".as_ptr());
            } else if (*op).value == 3 {
                fprintf(output, c"PLUS unary - -".as_ptr());
            }
        }
        XmlXPathOp::XpathOpMult => {
            if (*op).value == 0 {
                fprintf(output, c"MULT *".as_ptr());
            } else if (*op).value == 1 {
                fprintf(output, c"MULT div".as_ptr());
            } else {
                fprintf(output, c"MULT mod".as_ptr());
            }
        }
        XmlXPathOp::XpathOpUnion => {
            fprintf(output, c"UNION".as_ptr());
        }
        XmlXPathOp::XpathOpRoot => {
            fprintf(output, c"ROOT".as_ptr());
        }
        XmlXPathOp::XpathOpNode => {
            fprintf(output, c"NODE".as_ptr());
        }
        XmlXPathOp::XpathOpSort => {
            fprintf(output, c"SORT".as_ptr());
        }
        XmlXPathOp::XpathOpCollect => {
            let prefix: *const XmlChar = (*op).value4 as _;
            let name: *const XmlChar = (*op).value5 as _;

            fprintf(output, c"COLLECT ".as_ptr());
            match XmlXPathAxisVal::try_from((*op).value) {
                Ok(XmlXPathAxisVal::AxisAncestor) => {
                    fprintf(output, c" 'ancestors' ".as_ptr());
                }
                Ok(XmlXPathAxisVal::AxisAncestorOrSelf) => {
                    fprintf(output, c" 'ancestors-or-self' ".as_ptr());
                }
                Ok(XmlXPathAxisVal::AxisAttribute) => {
                    fprintf(output, c" 'attributes' ".as_ptr());
                }
                Ok(XmlXPathAxisVal::AxisChild) => {
                    fprintf(output, c" 'child' ".as_ptr());
                }
                Ok(XmlXPathAxisVal::AxisDescendant) => {
                    fprintf(output, c" 'descendant' ".as_ptr());
                }
                Ok(XmlXPathAxisVal::AxisDescendantOrSelf) => {
                    fprintf(output, c" 'descendant-or-self' ".as_ptr());
                }
                Ok(XmlXPathAxisVal::AxisFollowing) => {
                    fprintf(output, c" 'following' ".as_ptr());
                }
                Ok(XmlXPathAxisVal::AxisFollowingSibling) => {
                    fprintf(output, c" 'following-siblings' ".as_ptr());
                }
                Ok(XmlXPathAxisVal::AxisNamespace) => {
                    fprintf(output, c" 'namespace' ".as_ptr());
                }
                Ok(XmlXPathAxisVal::AxisParent) => {
                    fprintf(output, c" 'parent' ".as_ptr());
                }
                Ok(XmlXPathAxisVal::AxisPreceding) => {
                    fprintf(output, c" 'preceding' ".as_ptr());
                }
                Ok(XmlXPathAxisVal::AxisPrecedingSibling) => {
                    fprintf(output, c" 'preceding-sibling' ".as_ptr());
                }
                Ok(XmlXPathAxisVal::AxisSelf) => {
                    fprintf(output, c" 'self' ".as_ptr());
                }
                _ => unreachable!(),
            }
            match XmlXPathTestVal::try_from((*op).value2) {
                Ok(XmlXPathTestVal::NodeTestNone) => {
                    fprintf(output, c"'none' ".as_ptr());
                }
                Ok(XmlXPathTestVal::NodeTestType) => {
                    fprintf(output, c"'type' ".as_ptr());
                }
                Ok(XmlXPathTestVal::NodeTestPI) => {
                    fprintf(output, c"'PI' ".as_ptr());
                }
                Ok(XmlXPathTestVal::NodeTestAll) => {
                    fprintf(output, c"'all' ".as_ptr());
                }
                Ok(XmlXPathTestVal::NodeTestNs) => {
                    fprintf(output, c"'namespace' ".as_ptr());
                }
                Ok(XmlXPathTestVal::NodeTestName) => {
                    fprintf(output, c"'name' ".as_ptr());
                }
                _ => unreachable!(),
            }
            match XmlXPathTypeVal::try_from((*op).value3) {
                Ok(XmlXPathTypeVal::NodeTypeNode) => {
                    fprintf(output, c"'node' ".as_ptr());
                }
                Ok(XmlXPathTypeVal::NodeTypeComment) => {
                    fprintf(output, c"'comment' ".as_ptr());
                }
                Ok(XmlXPathTypeVal::NodeTypeText) => {
                    fprintf(output, c"'text' ".as_ptr());
                }
                Ok(XmlXPathTypeVal::NodeTypePI) => {
                    fprintf(output, c"'PI' ".as_ptr());
                }
                _ => unreachable!(),
            }
            if !prefix.is_null() {
                fprintf(output, c"%s:".as_ptr(), prefix);
            }
            if !name.is_null() {
                fprintf(output, c"%s".as_ptr(), name as *const c_char);
            }
        }
        XmlXPathOp::XpathOpValue => {
            let object: XmlXPathObjectPtr = (*op).value4 as XmlXPathObjectPtr;

            fprintf(output, c"ELEM ".as_ptr());
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

            if !prefix.is_null() {
                fprintf(output, c"VARIABLE %s:%s".as_ptr(), prefix, name);
            } else {
                fprintf(output, c"VARIABLE %s".as_ptr(), name);
            }
        }
        XmlXPathOp::XpathOpFunction => {
            let nbargs: c_int = (*op).value;
            let prefix: *const XmlChar = (*op).value5 as _;
            let name: *const XmlChar = (*op).value4 as _;

            if !prefix.is_null() {
                fprintf(
                    output,
                    c"FUNCTION %s:%s(%d args)".as_ptr(),
                    prefix,
                    name,
                    nbargs,
                );
            } else {
                fprintf(output, c"FUNCTION %s(%d args)".as_ptr(), name, nbargs);
            }
        }
        XmlXPathOp::XpathOpArg => {
            fprintf(output, c"ARG".as_ptr());
        }
        XmlXPathOp::XpathOpPredicate => {
            fprintf(output, c"PREDICATE".as_ptr());
        }
        XmlXPathOp::XpathOpFilter => {
            fprintf(output, c"FILTER".as_ptr());
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathOp::XpathOpRangeto => {
            fprintf(output, c"RANGETO".as_ptr());
        } // _ => {
          //     fprintf(output, c"UNKNOWN %d\n".as_ptr(), (*op).op);
          //     return;
          // }
    }
    fprintf(output, c"\n".as_ptr());
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

/**
 * xmlXPathDebugDumpCompExpr:
 * @output:  the FILE * for the output
 * @comp:  the precompiled XPath expression
 * @depth:  the indentation level.
 *
 * Dumps the tree of the compiled XPath expression.
 */
#[cfg(feature = "libxml_debug")]
pub unsafe extern "C" fn xml_xpath_debug_dump_comp_expr(
    output: *mut FILE,
    comp: XmlXPathCompExprPtr,
    depth: c_int,
) {
    let mut shift: [c_char; 100] = [0; 100];

    if output.is_null() || comp.is_null() {
        return;
    }

    for i in 0..depth.min(25) {
        shift[2 * i as usize] = b' ' as _;
        shift[2 * i as usize + 1] = b' ' as _;
    }
    shift[2 * depth.clamp(0, 25) as usize] = 0;
    shift[2 * depth.clamp(0, 25) as usize + 1] = 0;

    fprintf(output, c"%s".as_ptr(), shift.as_ptr());

    if !(*comp).stream.is_null() {
        fprintf(output, c"Streaming Expression\n".as_ptr());
    } else {
        fprintf(
            output,
            c"Compiled Expression : %d elements\n".as_ptr(),
            (*comp).nb_step,
        );
        xml_xpath_debug_dump_step_op(
            output,
            comp,
            (*comp).steps.add((*comp).last as usize),
            depth + 1,
        );
    }
}

/**
 * NodeSet handling.
 */
/**
 * xmlXPathNodeSetContains:
 * @cur:  the node-set
 * @val:  the node
 *
 * checks whether @cur contains @val
 *
 * Returns true (1) if @cur contains @val, false (0) otherwise
 */
pub unsafe extern "C" fn xml_xpath_node_set_contains(cur: XmlNodeSetPtr, val: XmlNodePtr) -> c_int {
    if cur.is_null() || val.is_null() {
        return 0;
    }
    if matches!((*val).typ, XmlElementType::XmlNamespaceDecl) {
        for i in 0..(*cur).node_nr {
            if matches!(
                (*(*(*cur).node_tab.add(i as usize))).typ,
                XmlElementType::XmlNamespaceDecl
            ) {
                let ns1: XmlNsPtr = val as XmlNsPtr;
                let ns2: XmlNsPtr = *(*cur).node_tab.add(i as usize) as XmlNsPtr;
                if ns1 == ns2 {
                    return 1;
                }
                if !(*ns1).next.load(Ordering::Relaxed).is_null()
                    && (*ns2).next.load(Ordering::Relaxed) == (*ns1).next.load(Ordering::Relaxed)
                    && xml_str_equal(
                        (*ns1).prefix.load(Ordering::Relaxed) as _,
                        (*ns2).prefix.load(Ordering::Relaxed) as _,
                    )
                {
                    return 1;
                }
            }
        }
    } else {
        for i in 0..(*cur).node_nr {
            if *(*cur).node_tab.add(i as usize) == val {
                return 1;
            }
        }
    }
    0
}

/**
 * xmlXPathDifference:
 * @nodes1:  a node-set
 * @nodes2:  a node-set
 *
 * Implements the EXSLT - Sets difference() function:
 *    node-set set:difference (node-set, node-set)
 *
 * Returns the difference between the two node sets, or nodes1 if
 *         nodes2 is empty
 */
pub unsafe extern "C" fn xml_xpath_difference(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let mut cur: XmlNodePtr;

    if xmlXPathNodeSetIsEmpty!(nodes2) {
        return nodes1;
    }

    /* TODO: Check memory error. */
    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    if xmlXPathNodeSetIsEmpty!(nodes1) {
        return ret;
    }

    let l1: c_int = xmlXPathNodeSetGetLength!(nodes1);

    for i in 0..l1 {
        cur = xmlXPathNodeSetItem!(nodes1, i);
        if xml_xpath_node_set_contains(nodes2, cur) == 0 {
            /* TODO: Propagate memory error. */
            if xml_xpath_node_set_add_unique(ret, cur) < 0 {
                break;
            }
        }
    }
    ret
}

/**
 * xmlXPathIntersection:
 * @nodes1:  a node-set
 * @nodes2:  a node-set
 *
 * Implements the EXSLT - Sets c_intersection() function:
 *    node-set set:c_intersection (node-set, node-set)
 *
 * Returns a node set comprising the nodes that are within both the
 *         node sets passed as arguments
 */
pub unsafe extern "C" fn xml_xpath_intersection(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    let mut cur: XmlNodePtr;

    if ret.is_null() {
        return ret;
    }
    if xmlXPathNodeSetIsEmpty!(nodes1) {
        return ret;
    }
    if xmlXPathNodeSetIsEmpty!(nodes2) {
        return ret;
    }

    let l1: c_int = xmlXPathNodeSetGetLength!(nodes1);

    for i in 0..l1 {
        cur = xmlXPathNodeSetItem!(nodes1, i);
        if xml_xpath_node_set_contains(nodes2, cur) != 0 {
            /* TODO: Propagate memory error. */
            if xml_xpath_node_set_add_unique(ret, cur) < 0 {
                break;
            }
        }
    }
    ret
}

/**
 * xmlXPathDistinctSorted:
 * @nodes:  a node-set, sorted by document order
 *
 * Implements the EXSLT - Sets distinct() function:
 *    node-set set:distinct (node-set)
 *
 * Returns a subset of the nodes contained in @nodes, or @nodes if
 *         it is empty
 */
pub unsafe extern "C" fn xml_xpath_distinct_sorted(nodes: XmlNodeSetPtr) -> XmlNodeSetPtr {
    let mut strval: *mut XmlChar;
    let mut cur: XmlNodePtr;

    if xmlXPathNodeSetIsEmpty!(nodes) {
        return nodes;
    }

    let ret: XmlNodeSetPtr = xml_xpath_node_set_create(null_mut());
    if ret.is_null() {
        return ret;
    }
    let l: c_int = xmlXPathNodeSetGetLength!(nodes);
    let hash: XmlHashTablePtr = xml_hash_create(l);
    for i in 0..l {
        cur = xmlXPathNodeSetItem!(nodes, i);
        strval = xml_xpath_cast_node_to_string(cur);
        if xml_hash_lookup(hash, strval).is_null() {
            if xml_hash_add_entry(hash, strval, strval as _) < 0 {
                xml_free(strval as _);
                // goto error;
                xml_hash_free(hash, Some(xml_hash_default_deallocator));
                xml_xpath_free_node_set(ret);
                return null_mut();
            }
            if xml_xpath_node_set_add_unique(ret, cur) < 0 {
                // goto error;
                xml_hash_free(hash, Some(xml_hash_default_deallocator));
                xml_xpath_free_node_set(ret);
                return null_mut();
            }
        } else {
            xml_free(strval as _);
        }
    }
    xml_hash_free(hash, Some(xml_hash_default_deallocator));
    ret

    // error:
    // xmlHashFree(hash, Some(xmlHashDefaultDeallocator));
    // xmlXPathFreeNodeSet(ret);
    // return null_mut();
}

/**
 * xmlXPathDistinct:
 * @nodes:  a node-set
 *
 * Implements the EXSLT - Sets distinct() function:
 *    node-set set:distinct (node-set)
 * @nodes is sorted by document order, then #exslSetsDistinctSorted
 * is called with the sorted node-set
 *
 * Returns a subset of the nodes contained in @nodes, or @nodes if
 *         it is empty
 */
pub unsafe extern "C" fn xml_xpath_distinct(nodes: XmlNodeSetPtr) -> XmlNodeSetPtr {
    if xmlXPathNodeSetIsEmpty!(nodes) {
        return nodes;
    }

    xml_xpath_node_set_sort(nodes);
    xml_xpath_distinct_sorted(nodes)
}

/**
 * xmlXPathHasSameNodes:
 * @nodes1:  a node-set
 * @nodes2:  a node-set
 *
 * Implements the EXSLT - Sets has-same-nodes function:
 *    boolean set:has-same-node(node-set, node-set)
 *
 * Returns true (1) if @nodes1 shares any node with @nodes2, false (0)
 *         otherwise
 */
pub unsafe extern "C" fn xml_xpath_has_same_nodes(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> c_int {
    let mut cur: XmlNodePtr;

    if xmlXPathNodeSetIsEmpty!(nodes1) || xmlXPathNodeSetIsEmpty!(nodes2) {
        return 0;
    }

    let l: c_int = xmlXPathNodeSetGetLength!(nodes1);
    for i in 0..l {
        cur = xmlXPathNodeSetItem!(nodes1, i);
        if xml_xpath_node_set_contains(nodes2, cur) != 0 {
            return 1;
        }
    }
    0
}

/**
 * xmlXPathNodeLeadingSorted:
 * @nodes: a node-set, sorted by document order
 * @node: a node
 *
 * Implements the EXSLT - Sets leading() function:
 *    node-set set:leading (node-set, node-set)
 *
 * Returns the nodes in @nodes that precede @node in document order,
 *         @nodes if @node is NULL or an empty node-set if @nodes
 *         doesn't contain @node
 */
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
    if xmlXPathNodeSetIsEmpty!(nodes) || xml_xpath_node_set_contains(nodes, node) == 0 {
        return ret;
    }

    let l: c_int = xmlXPathNodeSetGetLength!(nodes);
    for i in 0..l {
        cur = xmlXPathNodeSetItem!(nodes, i);
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

/**
 * xmlXPathLeadingSorted:
 * @nodes1:  a node-set, sorted by document order
 * @nodes2:  a node-set, sorted by document order
 *
 * Implements the EXSLT - Sets leading() function:
 *    node-set set:leading (node-set, node-set)
 *
 * Returns the nodes in @nodes1 that precede the first node in @nodes2
 *         in document order, @nodes1 if @nodes2 is NULL or empty or
 *         an empty node-set if @nodes1 doesn't contain @nodes2
 */
pub unsafe extern "C" fn xml_xpath_leading_sorted(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if xmlXPathNodeSetIsEmpty!(nodes2) {
        return nodes1;
    }
    xml_xpath_node_leading_sorted(nodes1, xmlXPathNodeSetItem!(nodes2, 1))
}

/**
 * xmlXPathNodeLeading:
 * @nodes:  a node-set
 * @node:  a node
 *
 * Implements the EXSLT - Sets leading() function:
 *    node-set set:leading (node-set, node-set)
 * @nodes is sorted by document order, then #exslSetsNodeLeadingSorted
 * is called.
 *
 * Returns the nodes in @nodes that precede @node in document order,
 *         @nodes if @node is NULL or an empty node-set if @nodes
 *         doesn't contain @node
 */
pub unsafe extern "C" fn xml_xpath_node_leading(
    nodes: XmlNodeSetPtr,
    node: XmlNodePtr,
) -> XmlNodeSetPtr {
    xml_xpath_node_set_sort(nodes);
    xml_xpath_node_leading_sorted(nodes, node)
}

/**
 * xmlXPathLeading:
 * @nodes1:  a node-set
 * @nodes2:  a node-set
 *
 * Implements the EXSLT - Sets leading() function:
 *    node-set set:leading (node-set, node-set)
 * @nodes1 and @nodes2 are sorted by document order, then
 * #exslSetsLeadingSorted is called.
 *
 * Returns the nodes in @nodes1 that precede the first node in @nodes2
 *         in document order, @nodes1 if @nodes2 is NULL or empty or
 *         an empty node-set if @nodes1 doesn't contain @nodes2
 */
pub unsafe extern "C" fn xml_xpath_leading(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if xmlXPathNodeSetIsEmpty!(nodes2) {
        return nodes1;
    }
    if xmlXPathNodeSetIsEmpty!(nodes1) {
        return xml_xpath_node_set_create(null_mut());
    }
    xml_xpath_node_set_sort(nodes1);
    xml_xpath_node_set_sort(nodes2);
    xml_xpath_node_leading_sorted(nodes1, xmlXPathNodeSetItem!(nodes2, 1))
}

/**
 * xmlXPathNodeTrailingSorted:
 * @nodes: a node-set, sorted by document order
 * @node: a node
 *
 * Implements the EXSLT - Sets trailing() function:
 *    node-set set:trailing (node-set, node-set)
 *
 * Returns the nodes in @nodes that follow @node in document order,
 *         @nodes if @node is NULL or an empty node-set if @nodes
 *         doesn't contain @node
 */
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
    if xmlXPathNodeSetIsEmpty!(nodes) || xml_xpath_node_set_contains(nodes, node) == 0 {
        return ret;
    }

    let l: c_int = xmlXPathNodeSetGetLength!(nodes);
    for i in (0..l).rev() {
        cur = xmlXPathNodeSetItem!(nodes, i);
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

/**
 * xmlXPathTrailingSorted:
 * @nodes1:  a node-set, sorted by document order
 * @nodes2:  a node-set, sorted by document order
 *
 * Implements the EXSLT - Sets trailing() function:
 *    node-set set:trailing (node-set, node-set)
 *
 * Returns the nodes in @nodes1 that follow the first node in @nodes2
 *         in document order, @nodes1 if @nodes2 is NULL or empty or
 *         an empty node-set if @nodes1 doesn't contain @nodes2
 */
pub unsafe extern "C" fn xml_xpath_trailing_sorted(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if xmlXPathNodeSetIsEmpty!(nodes2) {
        return nodes1;
    }
    xml_xpath_node_trailing_sorted(nodes1, xmlXPathNodeSetItem!(nodes2, 0))
}

/**
 * xmlXPathNodeTrailing:
 * @nodes:  a node-set
 * @node:  a node
 *
 * Implements the EXSLT - Sets trailing() function:
 *    node-set set:trailing (node-set, node-set)
 * @nodes is sorted by document order, then #xmlXPathNodeTrailingSorted
 * is called.
 *
 * Returns the nodes in @nodes that follow @node in document order,
 *         @nodes if @node is NULL or an empty node-set if @nodes
 *         doesn't contain @node
 */
pub unsafe extern "C" fn xml_xpath_node_trailing(
    nodes: XmlNodeSetPtr,
    node: XmlNodePtr,
) -> XmlNodeSetPtr {
    xml_xpath_node_set_sort(nodes);
    xml_xpath_node_trailing_sorted(nodes, node)
}

/**
 * xmlXPathTrailing:
 * @nodes1:  a node-set
 * @nodes2:  a node-set
 *
 * Implements the EXSLT - Sets trailing() function:
 *    node-set set:trailing (node-set, node-set)
 * @nodes1 and @nodes2 are sorted by document order, then
 * #xmlXPathTrailingSorted is called.
 *
 * Returns the nodes in @nodes1 that follow the first node in @nodes2
 *         in document order, @nodes1 if @nodes2 is NULL or empty or
 *         an empty node-set if @nodes1 doesn't contain @nodes2
 */
pub unsafe extern "C" fn xml_xpath_trailing(
    nodes1: XmlNodeSetPtr,
    nodes2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    if xmlXPathNodeSetIsEmpty!(nodes2) {
        return nodes1;
    }
    if xmlXPathNodeSetIsEmpty!(nodes1) {
        return xml_xpath_node_set_create(null_mut());
    }
    xml_xpath_node_set_sort(nodes1);
    xml_xpath_node_set_sort(nodes2);
    xml_xpath_node_trailing_sorted(nodes1, xmlXPathNodeSetItem!(nodes2, 0))
}

/**
 * Extending a context.
 */

/**
 * xmlXPathRegisterNs:
 * @ctxt:  the XPath context
 * @prefix:  the namespace prefix cannot be NULL or empty string
 * @ns_uri:  the namespace name
 *
 * Register a new namespace. If @ns_uri is NULL it unregisters
 * the namespace
 *
 * Returns 0 in case of success, -1 in case of error
 */
pub unsafe extern "C" fn xml_xpath_register_ns(
    ctxt: XmlXPathContextPtr,
    prefix: *const XmlChar,
    ns_uri: *const XmlChar,
) -> c_int {
    if ctxt.is_null() {
        return -1;
    }
    if prefix.is_null() {
        return -1;
    }
    if *prefix.add(0) == 0 {
        return -1;
    }

    if (*ctxt).ns_hash.is_null() {
        (*ctxt).ns_hash = xml_hash_create(10);
    }
    if (*ctxt).ns_hash.is_null() {
        return -1;
    }
    if ns_uri.is_null() {
        return xml_hash_remove_entry((*ctxt).ns_hash, prefix, Some(xml_hash_default_deallocator));
    }

    let copy: *mut XmlChar = xml_strdup(ns_uri);
    if copy.is_null() {
        return -1;
    }
    if xml_hash_update_entry(
        (*ctxt).ns_hash,
        prefix,
        copy as _,
        Some(xml_hash_default_deallocator),
    ) < 0
    {
        xml_free(copy as _);
        return -1;
    }

    0
}

/**
 * xmlXPathNsLookup:
 * @ctxt:  the XPath context
 * @prefix:  the namespace prefix value
 *
 * Search in the namespace declaration array of the context for the given
 * namespace name associated to the given prefix
 *
 * Returns the value or NULL if not found
 */
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

    if !(*ctxt).namespaces.is_null() {
        for i in 0..(*ctxt).ns_nr {
            if (*(*ctxt).namespaces.add(i as usize)).is_null()
                && xml_str_equal(
                    (*(*(*ctxt).namespaces.add(i as usize)))
                        .prefix
                        .load(Ordering::Relaxed) as _,
                    prefix,
                )
            {
                return (*(*(*ctxt).namespaces.add(i as usize)))
                    .href
                    .load(Ordering::Relaxed) as _;
            }
        }
    }

    xml_hash_lookup((*ctxt).ns_hash, prefix) as _
}

/**
 * xmlXPathRegisteredNsCleanup:
 * @ctxt:  the XPath context
 *
 * Cleanup the XPath context data associated to registered variables
 */
pub unsafe extern "C" fn xml_xpath_registered_ns_cleanup(ctxt: XmlXPathContextPtr) {
    if ctxt.is_null() {
        return;
    }

    xml_hash_free((*ctxt).ns_hash, Some(xml_hash_default_deallocator));
    (*ctxt).ns_hash = null_mut();
}

/**
 * xmlXPathRegisterFunc:
 * @ctxt:  the XPath context
 * @name:  the function name
 * @f:  the function implementation or NULL
 *
 * Register a new function. If @f is NULL it unregisters the function
 *
 * Returns 0 in case of success, -1 in case of error
 */
pub unsafe extern "C" fn xml_xpath_register_func(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    f: XmlXPathFunction,
) -> c_int {
    xml_xpath_register_func_ns(ctxt, name, null(), Some(f))
}

/**
 * xmlXPathRegisterFuncNS:
 * @ctxt:  the XPath context
 * @name:  the function name
 * @ns_uri:  the function namespace URI
 * @f:  the function implementation or NULL
 *
 * Register a new function. If @f is NULL it unregisters the function
 *
 * Returns 0 in case of success, -1 in case of error
 */
pub unsafe extern "C" fn xml_xpath_register_func_ns(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    ns_uri: *const XmlChar,
    f: Option<XmlXPathFunction>,
) -> c_int {
    if ctxt.is_null() {
        return -1;
    }
    if name.is_null() {
        return -1;
    }

    if (*ctxt).func_hash.is_null() {
        (*ctxt).func_hash = xml_hash_create(0);
    }
    if (*ctxt).func_hash.is_null() {
        return -1;
    }
    if let Some(f) = f {
        xml_hash_add_entry2(
            (*ctxt).func_hash,
            name,
            ns_uri,
            *(addr_of!(f) as *const *mut c_void),
        )
    } else {
        xml_hash_remove_entry2((*ctxt).func_hash, name, ns_uri, None)
    }
}

/**
 * xmlXPathRegisterVariable:
 * @ctxt:  the XPath context
 * @name:  the variable name
 * @value:  the variable value or NULL
 *
 * Register a new variable value. If @value is NULL it unregisters
 * the variable
 *
 * Returns 0 in case of success, -1 in case of error
 */
pub unsafe extern "C" fn xml_xpath_register_variable(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    value: XmlXPathObjectPtr,
) -> c_int {
    xml_xpath_register_variable_ns(ctxt, name, null(), value)
}

extern "C" fn xml_xpath_free_object_entry(obj: *mut c_void, _name: *const XmlChar) {
    unsafe {
        xml_xpath_free_object(obj as XmlXPathObjectPtr);
    }
}

/**
 * xmlXPathRegisterVariableNS:
 * @ctxt:  the XPath context
 * @name:  the variable name
 * @ns_uri:  the variable namespace URI
 * @value:  the variable value or NULL
 *
 * Register a new variable value. If @value is NULL it unregisters
 * the variable
 *
 * Returns 0 in case of success, -1 in case of error
 */
pub unsafe extern "C" fn xml_xpath_register_variable_ns(
    ctxt: XmlXPathContextPtr,
    name: *const XmlChar,
    ns_uri: *const XmlChar,
    value: XmlXPathObjectPtr,
) -> c_int {
    if ctxt.is_null() {
        return -1;
    }
    if name.is_null() {
        return -1;
    }

    if (*ctxt).var_hash.is_null() {
        (*ctxt).var_hash = xml_hash_create(0);
    }
    if (*ctxt).var_hash.is_null() {
        return -1;
    }
    if value.is_null() {
        return xml_hash_remove_entry2(
            (*ctxt).var_hash,
            name,
            ns_uri,
            Some(xml_xpath_free_object_entry),
        );
    }
    xml_hash_update_entry2(
        (*ctxt).var_hash,
        name,
        ns_uri,
        value as _,
        Some(xml_xpath_free_object_entry),
    )
}

/**
 * xmlXPathFunctionLookup:
 * @ctxt:  the XPath context
 * @name:  the function name
 *
 * Search in the Function array of the context for the given
 * function.
 *
 * Returns the xmlXPathFunction or NULL if not found
 */
pub unsafe extern "C" fn xml_xpath_function_lookup(
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

/**
 * xmlXPathFunctionLookupNS:
 * @ctxt:  the XPath context
 * @name:  the function name
 * @ns_uri:  the function namespace URI
 *
 * Search in the Function array of the context for the given
 * function.
 *
 * Returns the xmlXPathFunction or NULL if not found
 */
pub unsafe extern "C" fn xml_xpath_function_lookup_ns(
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

    if (*ctxt).func_hash.is_null() {
        return None;
    }

    let ret = xml_hash_lookup2((*ctxt).func_hash, name, ns_uri);
    (!ret.is_null()).then(|| *(addr_of!(ret) as *const XmlXPathFunction))
}

/**
 * xmlXPathRegisteredFuncsCleanup:
 * @ctxt:  the XPath context
 *
 * Cleanup the XPath context data associated to registered functions
 */
pub unsafe extern "C" fn xml_xpath_registered_funcs_cleanup(ctxt: XmlXPathContextPtr) {
    if ctxt.is_null() {
        return;
    }

    xml_hash_free((*ctxt).func_hash, None);
    (*ctxt).func_hash = null_mut();
}

/**
 * xmlXPathVariableLookup:
 * @ctxt:  the XPath context
 * @name:  the variable name
 *
 * Search in the Variable array of the context for the given
 * variable value.
 *
 * Returns a copy of the value or NULL if not found
 */
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

/**
 * xmlXPathCacheWrapNodeSet:
 * @ctxt: the XPath context
 * @val:  the NodePtr value
 *
 * This is the cached version of xmlXPathWrapNodeSet().
 * Wrap the Nodeset @val in a new xmlXPathObjectPtr
 *
 * Returns the created or reused object.
 *
 * In case of error the node set is destroyed and NULL is returned.
 */
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
            (*ret).typ = XmlXPathObjectType::XpathNodeset;
            (*ret).nodesetval = val;
            // #ifdef XP_DEBUG_OBJ_USAGE
            // 	    xmlXPathDebugObjUsageRequested(ctxt, XpathNodeset);
            // #endif
            return ret;
        }
    }

    xml_xpath_wrap_node_set(val)
}

/**
 * xmlXPathErrMemory:
 * @ctxt:  an XPath context
 * @extra:  extra information
 *
 * Handle a redefinition of attribute error
 */
pub unsafe extern "C" fn xml_xpath_err_memory(ctxt: XmlXPathContextPtr, extra: *const c_char) {
    if !ctxt.is_null() {
        (*ctxt).last_error.reset();
        if !extra.is_null() {
            let mut buf: [XmlChar; 200] = [0; 200];

            xml_str_printf!(
                buf.as_mut_ptr(),
                200,
                c"Memory allocation failed : %s\n".as_ptr(),
                extra
            );
            (*ctxt).last_error.message = Some(
                CStr::from_ptr(buf.as_ptr() as *const i8)
                    .to_string_lossy()
                    .into_owned()
                    .into(),
            );
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
    } else if !extra.is_null() {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromXPath,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrFatal,
            null(),
            0,
            (!extra.is_null()).then(|| CStr::from_ptr(extra).to_string_lossy().into_owned().into()),
            None,
            None,
            0,
            0,
            c"Memory allocation failed : %s\n".as_ptr(),
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
            null(),
            0,
            None,
            None,
            None,
            0,
            0,
            c"Memory allocation failed\n".as_ptr(),
        );
    }
}

/**
 * xmlXPathCacheNewString:
 * @ctxt: the XPath context
 * @val:  the xmlChar * value
 *
 * This is the cached version of xmlXPathNewString().
 * Acquire an xmlXPathObjectPtr of type string and of value @val
 *
 * Returns the created or reused object.
 */
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
                xml_xpath_err_memory(ctxt, null());
                return null_mut();
            }

            (*(*cache).string_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).string_objs)
                .items
                .add((*(*cache).string_objs).number as usize)
                as XmlXPathObjectPtr;
            (*ret).typ = XmlXPathObjectType::XpathString;
            (*ret).stringval = copy;
            // #ifdef XP_DEBUG_OBJ_USAGE
            // 	    xmlXPathDebugObjUsageRequested(ctxt, XpathString);
            // #endif
            return ret;
        } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
            if val.is_null() {
                val = c"".as_ptr() as _;
            }
            let copy: *mut XmlChar = xml_strdup(val);
            if copy.is_null() {
                xml_xpath_err_memory(ctxt, null());
                return null_mut();
            }

            (*(*cache).misc_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                .items
                .add((*(*cache).misc_objs).number as usize)
                as XmlXPathObjectPtr;

            (*ret).typ = XmlXPathObjectType::XpathString;
            (*ret).stringval = copy;
            // #ifdef XP_DEBUG_OBJ_USAGE
            // 	    xmlXPathDebugObjUsageRequested(ctxt, XpathString);
            // #endif
            return ret;
        }
    }
    xml_xpath_new_string(val)
}

/**
 * xmlXPathCacheNewBoolean:
 * @ctxt: the XPath context
 * @val:  the boolean value
 *
 * This is the cached version of xmlXPathNewBoolean().
 * Acquires an xmlXPathObjectPtr of type boolean and of value @val
 *
 * Returns the created or reused object.
 */
unsafe extern "C" fn xml_xpath_cache_new_boolean(
    ctxt: XmlXPathContextPtr,
    val: c_int,
) -> XmlXPathObjectPtr {
    if !ctxt.is_null() && !(*ctxt).cache.is_null() {
        let cache: XmlXpathContextCachePtr = (*ctxt).cache as XmlXpathContextCachePtr;

        if !(*cache).boolean_objs.is_null() && (*(*cache).boolean_objs).number != 0 {
            (*(*cache).boolean_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).boolean_objs)
                .items
                .add((*(*cache).boolean_objs).number as usize)
                as XmlXPathObjectPtr;
            (*ret).typ = XmlXPathObjectType::XpathBoolean;
            (*ret).boolval = (val != 0) as i32;
            // #ifdef XP_DEBUG_OBJ_USAGE
            // 	    xmlXPathDebugObjUsageRequested(ctxt, XpathBoolean);
            // #endif
            return ret;
        } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
            (*(*cache).misc_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                .items
                .add((*(*cache).misc_objs).number as usize)
                as XmlXPathObjectPtr;

            (*ret).typ = XmlXPathObjectType::XpathBoolean;
            (*ret).boolval = (val != 0) as i32;
            // #ifdef XP_DEBUG_OBJ_USAGE
            // 	    xmlXPathDebugObjUsageRequested(ctxt, XpathBoolean);
            // #endif
            return ret;
        }
    }
    xml_xpath_new_boolean(val)
}

/**
 * xmlXPathCacheNewFloat:
 * @ctxt: the XPath context
 * @val:  the let value: f64
 *
 * This is the cached version of xmlXPathNewFloat().
 * Acquires an xmlXPathObjectPtr of type f64 and of value @val
 *
 * Returns the created or reused object.
 */
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
            (*ret).typ = XmlXPathObjectType::XpathNumber;
            (*ret).floatval = val;
            // #ifdef XP_DEBUG_OBJ_USAGE
            // 	    xmlXPathDebugObjUsageRequested(ctxt, XpathNumber);
            // #endif
            return ret;
        } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
            (*(*cache).misc_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                .items
                .add((*(*cache).misc_objs).number as usize)
                as XmlXPathObjectPtr;

            (*ret).typ = XmlXPathObjectType::XpathNumber;
            (*ret).floatval = val;
            // #ifdef XP_DEBUG_OBJ_USAGE
            // 	    xmlXPathDebugObjUsageRequested(ctxt, XpathNumber);
            // #endif
            return ret;
        }
    }
    xml_xpath_new_float(val)
}

/**
 * xmlXPathCacheObjectCopy:
 * @ctxt: the XPath context
 * @val:  the original object
 *
 * This is the cached version of xmlXPathObjectCopy().
 * Acquire a copy of a given object
 *
 * Returns a created or reused created object.
 */
unsafe extern "C" fn xml_xpath_cache_object_copy(
    ctxt: XmlXPathContextPtr,
    val: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    if val.is_null() {
        return null_mut();
    }

    if XP_HAS_CACHE!(ctxt) {
        match (*val).typ {
            XmlXPathObjectType::XpathNodeset => {
                return xml_xpath_cache_wrap_node_set(
                    ctxt,
                    xml_xpath_node_set_merge(null_mut(), (*val).nodesetval),
                );
            }
            XmlXPathObjectType::XpathString => {
                return xml_xpath_cache_new_string(ctxt, (*val).stringval);
            }
            XmlXPathObjectType::XpathBoolean => {
                return xml_xpath_cache_new_boolean(ctxt, (*val).boolval);
            }
            XmlXPathObjectType::XpathNumber => {
                return xml_xpath_cache_new_float(ctxt, (*val).floatval);
            }
            _ => {}
        }
    }
    xml_xpath_object_copy(val)
}

/**
 * xmlXPathVariableLookupNS:
 * @ctxt:  the XPath context
 * @name:  the variable name
 * @ns_uri:  the variable namespace URI
 *
 * Search in the Variable array of the context for the given
 * variable value.
 *
 * Returns the a copy of the value or NULL if not found
 */
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

    if (*ctxt).var_hash.is_null() {
        return null_mut();
    }
    if name.is_null() {
        return null_mut();
    }

    xml_xpath_cache_object_copy(
        ctxt,
        xml_hash_lookup2((*ctxt).var_hash, name, ns_uri) as XmlXPathObjectPtr,
    )
}

/**
 * xmlXPathRegisteredVariablesCleanup:
 * @ctxt:  the XPath context
 *
 * Cleanup the XPath context data associated to registered variables
 */
pub unsafe extern "C" fn xml_xpath_registered_variables_cleanup(ctxt: XmlXPathContextPtr) {
    if ctxt.is_null() {
        return;
    }

    xml_hash_free((*ctxt).var_hash, Some(xml_xpath_free_object_entry));
    (*ctxt).var_hash = null_mut();
}

/**
 * xmlXPathNewCompExpr:
 *
 * Create a new Xpath component
 *
 * Returns the newly allocated xmlXPathCompExprPtr or NULL in case of error
 */
unsafe extern "C" fn xml_xpath_new_comp_expr() -> XmlXPathCompExprPtr {
    let cur: XmlXPathCompExprPtr = xml_malloc(size_of::<XmlXPathCompExpr>()) as XmlXPathCompExprPtr;
    if cur.is_null() {
        xml_xpath_err_memory(null_mut(), c"allocating component\n".as_ptr());
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlXPathCompExpr>());
    (*cur).max_step = 10;
    (*cur).nb_step = 0;
    (*cur).steps =
        xml_malloc((*cur).max_step as usize * size_of::<XmlXPathStepOp>()) as *mut XmlXPathStepOp;
    if (*cur).steps.is_null() {
        xml_xpath_err_memory(null_mut(), c"allocating steps\n".as_ptr());
        xml_free(cur as _);
        return null_mut();
    }
    memset(
        (*cur).steps as _,
        0,
        (*cur).max_step as usize * size_of::<XmlXPathStepOp>(),
    );
    (*cur).last = -1;
    // #ifdef DEBUG_EVAL_COUNTS
    //     (*cur).nb = 0;
    // #endif
    cur
}

/**
 * Utilities to extend XPath.
 */
/**
 * xmlXPathNewParserContext:
 * @str:  the XPath expression
 * @ctxt:  the XPath context
 *
 * Create a new xmlXPathParserContext
 *
 * Returns the xmlXPathParserContext just allocated.
 */
pub unsafe extern "C" fn xml_xpath_new_parser_context(
    str: *const XmlChar,
    ctxt: XmlXPathContextPtr,
) -> XmlXPathParserContextPtr {
    let ret: XmlXPathParserContextPtr =
        xml_malloc(size_of::<XmlXPathParserContext>()) as XmlXPathParserContextPtr;
    if ret.is_null() {
        xml_xpath_err_memory(ctxt, c"creating parser context\n".as_ptr());
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

/**
 * xmlXPathFreeParserContext:
 * @ctxt:  the context to free
 *
 * Free up an xmlXPathParserContext
 */
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

/* TODO: remap to xmlXPathValuePop and Push. */
/**
 * valuePop:
 * @ctxt: an XPath evaluation context
 *
 * Pops the top XPath object from the value stack
 *
 * Returns the XPath object just removed
 */
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

/**
 * xmlXPathPErrMemory:
 * @ctxt:  an XPath parser context
 * @extra:  extra information
 *
 * Handle a redefinition of attribute error
 */
unsafe extern "C" fn xml_xpath_perr_memory(ctxt: XmlXPathParserContextPtr, extra: *const c_char) {
    if ctxt.is_null() {
        xml_xpath_err_memory(null_mut(), extra);
    } else {
        (*ctxt).error = XmlXPathError::XpathMemoryError as i32;
        xml_xpath_err_memory((*ctxt).context, extra);
    }
}

/**
 * valuePush:
 * @ctxt:  an XPath evaluation context
 * @value:  the XPath object
 *
 * Pushes a new XPath object on top of the value stack. If value is NULL,
 * a memory error is recorded in the parser context.
 *
 * Returns the number of items on the value stack, or -1 in case of error.
 *
 * The object is destroyed in case of error.
 */
pub unsafe extern "C" fn value_push(
    ctxt: XmlXPathParserContextPtr,
    value: XmlXPathObjectPtr,
) -> c_int {
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
            xml_xpath_perr_memory(ctxt, c"XPath stack depth limit reached\n".as_ptr());
            xml_xpath_free_object(value);
            return -1;
        }
        let tmp: *mut XmlXPathObjectPtr = xml_realloc(
            (*ctxt).value_tab as _,
            2 * (*ctxt).value_max as usize * size_of::<XmlXPathObjectPtr>(),
        ) as *mut XmlXPathObjectPtr;
        if tmp.is_null() {
            xml_xpath_perr_memory(ctxt, c"pushing value\n".as_ptr());
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

/**
 * xmlXPathNewString:
 * @val:  the xmlChar * value
 *
 * Create a new xmlXPathObjectPtr of type string and of value @val
 *
 * Returns the newly created object.
 */
pub unsafe extern "C" fn xml_xpath_new_string(mut val: *const XmlChar) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), c"creating string object\n".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathString;
    if val.is_null() {
        val = c"".as_ptr() as _;
    }
    (*ret).stringval = xml_strdup(val);
    if (*ret).stringval.is_null() {
        xml_free(ret as _);
        return null_mut();
    }
    // #ifdef XP_DEBUG_OBJ_USAGE
    //     xmlXPathDebugObjUsageRequested(NULL, XmlXPathObjectType::XpathString);
    // #endif
    ret
}

/**
 * xmlXPathNewCString:
 * @val:  the c_char * value
 *
 * Create a new xmlXPathObjectPtr of type string and of value @val
 *
 * Returns the newly created object.
 */
pub unsafe extern "C" fn xml_xpath_new_cstring(val: *const c_char) -> XmlXPathObjectPtr {
    xml_xpath_new_string(val as _)
}

/**
 * xmlXPathWrapString:
 * @val:  the xmlChar * value
 *
 * Wraps the @val string c_into an XPath object.
 *
 * Returns the newly created object.
 *
 * Frees @val in case of error.
 */
pub unsafe extern "C" fn xml_xpath_wrap_string(val: *mut XmlChar) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), c"creating string object\n".as_ptr());
        xml_free(val as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathString;
    (*ret).stringval = val;
    // #ifdef XP_DEBUG_OBJ_USAGE
    //     xmlXPathDebugObjUsageRequested(NULL, XmlXPathObjectType::XpathString);
    // #endif
    ret
}

/**
 * xmlXPathWrapCString:
 * @val:  the c_char * value
 *
 * Wraps a string c_into an XPath object.
 *
 * Returns the newly created object.
 */
pub unsafe extern "C" fn xml_xpath_wrap_cstring(val: *mut c_char) -> XmlXPathObjectPtr {
    xml_xpath_wrap_string(val as *mut XmlChar)
}

/* Allocations are terrible, one needs to optimize all this !!! */

/**
 * xmlXPathNewFloat:
 * @val:  the let value: f64
 *
 * Create a new xmlXPathObjectPtr of type f64 and of value @val
 *
 * Returns the newly created object.
 */
pub unsafe extern "C" fn xml_xpath_new_float(val: f64) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), c"creating float object\n".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathNumber;
    (*ret).floatval = val;
    // #ifdef XP_DEBUG_OBJ_USAGE
    //     xmlXPathDebugObjUsageRequested(NULL, XpathNumber);
    // #endif
    ret
}

/**
 * xmlXPathNewBoolean:
 * @val:  the boolean value
 *
 * Create a new xmlXPathObjectPtr of type boolean and of value @val
 *
 * Returns the newly created object.
 */
pub unsafe extern "C" fn xml_xpath_new_boolean(val: c_int) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), c"creating boolean object\n".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathBoolean;
    (*ret).boolval = (val != 0) as i32;
    // #ifdef XP_DEBUG_OBJ_USAGE
    //     xmlXPathDebugObjUsageRequested(NULL, XpathBoolean);
    // #endif
    ret
}

/**
 * xmlXPathNewNodeSet:
 * @val:  the NodePtr value
 *
 * Create a new xmlXPathObjectPtr of type NodeSet and initialize
 * it with the single Node @val
 *
 * Returns the newly created object.
 */
pub unsafe extern "C" fn xml_xpath_new_node_set(val: XmlNodePtr) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), c"creating nodeset\n".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathNodeset;
    (*ret).boolval = 0;
    /* TODO: Check memory error. */
    (*ret).nodesetval = xml_xpath_node_set_create(val);
    /* @@ with_ns to check whether namespace nodes should be looked at @@ */
    // #ifdef XP_DEBUG_OBJ_USAGE
    //     xmlXPathDebugObjUsageRequested(NULL, XpathNodeset);
    // #endif
    ret
}

/**
 * xmlXPathNewValueTree:
 * @val:  the NodePtr value
 *
 * Create a new xmlXPathObjectPtr of type Value Tree (XSLT) and initialize
 * it with the tree root @val
 *
 * Returns the newly created object.
 */
pub unsafe extern "C" fn xml_xpath_new_value_tree(val: XmlNodePtr) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), c"creating result value tree\n".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathXsltTree;
    (*ret).boolval = 1;
    (*ret).user = val as *mut c_void;
    (*ret).nodesetval = xml_xpath_node_set_create(val);
    // #ifdef XP_DEBUG_OBJ_USAGE
    //     xmlXPathDebugObjUsageRequested(NULL, XpathXsltTree);
    // #endif
    ret
}

pub const XML_NODESET_DEFAULT: usize = 10;

/**
 * xmlXPathNodeSetDupNs:
 * @node:  the parent node of the namespace XPath node
 * @ns:  the libxml namespace declaration node.
 *
 * Namespace node in libxml don't match the XPath semantic. In a node set
 * the namespace nodes are duplicated and the next poc_inter is set to the
 * parent node in the XPath semantic.
 *
 * Returns the newly created object.
 */
pub unsafe extern "C" fn xml_xpath_node_set_dup_ns(node: XmlNodePtr, ns: XmlNsPtr) -> XmlNodePtr {
    if ns.is_null() || !matches!((*ns).typ, Some(XmlElementType::XmlNamespaceDecl)) {
        return null_mut();
    }
    if node.is_null() || matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
        return ns as XmlNodePtr;
    }

    /*
     * Allocate a new Namespace and fill the fields.
     */
    let cur: XmlNsPtr = xml_malloc(size_of::<XmlNs>()) as XmlNsPtr;
    if cur.is_null() {
        xml_xpath_err_memory(null_mut(), c"duplicating namespace\n".as_ptr());
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlNs>());
    (*cur).typ = Some(XmlElementType::XmlNamespaceDecl);
    if !(*ns).href.load(Ordering::Relaxed).is_null() {
        (*cur).href.store(
            xml_strdup((*ns).href.load(Ordering::Relaxed)),
            Ordering::Relaxed,
        );
    }
    if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
        (*cur).prefix.store(
            xml_strdup((*ns).prefix.load(Ordering::Relaxed)),
            Ordering::Relaxed,
        );
    }
    (*cur).next.store(node as XmlNsPtr, Ordering::Relaxed);
    cur as XmlNodePtr
}

/**
 * xmlXPathNodeSetAdd:
 * @cur:  the initial node set
 * @val:  a new xmlNodePtr
 *
 * add a new xmlNodePtr to an existing NodeSet
 *
 * Returns 0 in case of success, and -1 in case of error
 */
pub unsafe extern "C" fn xml_xpath_node_set_add(cur: XmlNodeSetPtr, val: XmlNodePtr) -> c_int {
    if cur.is_null() || val.is_null() {
        return -1;
    }

    /* @@ with_ns to check whether namespace nodes should be looked at @@ */
    /*
     * prevent duplicates
     */
    for i in 0..(*cur).node_nr {
        if *(*cur).node_tab.add(i as usize) == val {
            return 0;
        }
    }

    /*
     * grow the nodeTab if needed
     */
    if (*cur).node_max == 0 {
        (*cur).node_tab =
            xml_malloc(XML_NODESET_DEFAULT * size_of::<XmlNodePtr>()) as *mut XmlNodePtr;
        if (*cur).node_tab.is_null() {
            xml_xpath_err_memory(null_mut(), c"growing nodeset\n".as_ptr());
            return -1;
        }
        memset(
            (*cur).node_tab as _,
            0,
            XML_NODESET_DEFAULT * size_of::<XmlNodePtr>(),
        );
        (*cur).node_max = XML_NODESET_DEFAULT as _;
    } else if (*cur).node_nr == (*cur).node_max {
        if (*cur).node_max >= XPATH_MAX_NODESET_LENGTH as i32 {
            xml_xpath_err_memory(null_mut(), c"growing nodeset hit limit\n".as_ptr());
            return -1;
        }
        let temp: *mut XmlNodePtr = xml_realloc(
            (*cur).node_tab as _,
            (*cur).node_max as usize * 2 * size_of::<XmlNodePtr>(),
        ) as *mut XmlNodePtr;
        if temp.is_null() {
            xml_xpath_err_memory(null_mut(), c"growing nodeset\n".as_ptr());
            return -1;
        }
        (*cur).node_max *= 2;
        (*cur).node_tab = temp;
    }
    if matches!((*val).typ, XmlElementType::XmlNamespaceDecl) {
        let ns: XmlNsPtr = val as XmlNsPtr;
        let ns_node: XmlNodePtr =
            xml_xpath_node_set_dup_ns((*ns).next.load(Ordering::Relaxed) as XmlNodePtr, ns);

        if ns_node.is_null() {
            return -1;
        }
        *(*cur).node_tab.add((*cur).node_nr as usize) = ns_node;
        (*cur).node_nr += 1;
    } else {
        *(*cur).node_tab.add((*cur).node_nr as usize) = val;
        (*cur).node_nr += 1;
    }
    0
}

/**
 * xmlXPathNodeSetAddUnique:
 * @cur:  the initial node set
 * @val:  a new xmlNodePtr
 *
 * add a new xmlNodePtr to an existing NodeSet, optimized version
 * when we are sure the node is not already in the set.
 *
 * Returns 0 in case of success and -1 in case of failure
 */
pub unsafe extern "C" fn xml_xpath_node_set_add_unique(
    cur: XmlNodeSetPtr,
    val: XmlNodePtr,
) -> c_int {
    if cur.is_null() || val.is_null() {
        return -1;
    }

    /* @@ with_ns to check whether namespace nodes should be looked at @@ */
    /*
     * grow the nodeTab if needed
     */
    if (*cur).node_max == 0 {
        (*cur).node_tab =
            xml_malloc(XML_NODESET_DEFAULT * size_of::<XmlNodePtr>()) as *mut XmlNodePtr;
        if (*cur).node_tab.is_null() {
            xml_xpath_err_memory(null_mut(), c"growing nodeset\n".as_ptr());
            return -1;
        }
        memset(
            (*cur).node_tab as _,
            0,
            XML_NODESET_DEFAULT * size_of::<XmlNodePtr>(),
        );
        (*cur).node_max = XML_NODESET_DEFAULT as i32;
    } else if (*cur).node_nr == (*cur).node_max {
        if (*cur).node_max >= XPATH_MAX_NODESET_LENGTH as i32 {
            xml_xpath_err_memory(null_mut(), c"growing nodeset hit limit\n".as_ptr());
            return -1;
        }
        let temp: *mut XmlNodePtr = xml_realloc(
            (*cur).node_tab as _,
            (*cur).node_max as usize * 2 * size_of::<XmlNodePtr>(),
        ) as *mut XmlNodePtr;
        if temp.is_null() {
            xml_xpath_err_memory(null_mut(), c"growing nodeset\n".as_ptr());
            return -1;
        }
        (*cur).node_tab = temp;
        (*cur).node_max *= 2;
    }
    if matches!((*val).typ, XmlElementType::XmlNamespaceDecl) {
        let ns: XmlNsPtr = val as XmlNsPtr;
        let ns_node: XmlNodePtr =
            xml_xpath_node_set_dup_ns((*ns).next.load(Ordering::Relaxed) as XmlNodePtr, ns);

        if ns_node.is_null() {
            return -1;
        }
        *(*cur).node_tab.add((*cur).node_nr as usize) = ns_node;
        (*cur).node_nr += 1;
    } else {
        *(*cur).node_tab.add((*cur).node_nr as usize) = val;
        (*cur).node_nr += 1;
    }
    0
}

/**
 * xmlXPathNodeSetAddNs:
 * @cur:  the initial node set
 * @node:  the hosting node
 * @ns:  a the namespace node
 *
 * add a new namespace node to an existing NodeSet
 *
 * Returns 0 in case of success and -1 in case of error
 */
pub unsafe extern "C" fn xml_xpath_node_set_add_ns(
    cur: XmlNodeSetPtr,
    node: XmlNodePtr,
    ns: XmlNsPtr,
) -> c_int {
    if cur.is_null()
        || ns.is_null()
        || node.is_null()
        || !matches!((*ns).typ, Some(XmlElementType::XmlNamespaceDecl))
        || !matches!((*node).typ, XmlElementType::XmlElementNode)
    {
        return -1;
    }

    /* @@ with_ns to check whether namespace nodes should be looked at @@ */
    /*
     * prevent duplicates
     */
    for i in 0..(*cur).node_nr {
        if (*(*cur).node_tab.add(i as usize)).is_null()
            && matches!(
                (*(*(*cur).node_tab.add(i as usize))).typ,
                XmlElementType::XmlNamespaceDecl
            )
            && std::ptr::eq((*(*(*cur).node_tab.add(i as usize))).next, node)
            && xml_str_equal(
                (*ns).prefix.load(Ordering::Relaxed) as _,
                (*((*(*cur).node_tab.add(i as usize)) as XmlNsPtr))
                    .prefix
                    .load(Ordering::Relaxed) as _,
            )
        {
            return 0;
        }
    }

    /*
     * grow the nodeTab if needed
     */
    if (*cur).node_max == 0 {
        (*cur).node_tab =
            xml_malloc(XML_NODESET_DEFAULT * size_of::<XmlNodePtr>()) as *mut XmlNodePtr;
        if (*cur).node_tab.is_null() {
            xml_xpath_err_memory(null_mut(), c"growing nodeset\n".as_ptr());
            return -1;
        }
        memset(
            (*cur).node_tab as _,
            0,
            XML_NODESET_DEFAULT * size_of::<XmlNodePtr>(),
        );
        (*cur).node_max = XML_NODESET_DEFAULT as i32;
    } else if (*cur).node_nr == (*cur).node_max {
        if (*cur).node_max >= XPATH_MAX_NODESET_LENGTH as i32 {
            xml_xpath_err_memory(null_mut(), c"growing nodeset hit limit\n".as_ptr());
            return -1;
        }
        let temp: *mut XmlNodePtr = xml_realloc(
            (*cur).node_tab as _,
            (*cur).node_max as usize * 2 * size_of::<XmlNodePtr>(),
        ) as *mut XmlNodePtr;
        if temp.is_null() {
            xml_xpath_err_memory(null_mut(), c"growing nodeset\n".as_ptr());
            return -1;
        }
        (*cur).node_max *= 2;
        (*cur).node_tab = temp;
    }
    let ns_node: XmlNodePtr = xml_xpath_node_set_dup_ns(node, ns);
    if ns_node.is_null() {
        return -1;
    }
    *(*cur).node_tab.add((*cur).node_nr as usize) = ns_node;
    (*cur).node_nr += 1;
    0
}

/**
 * xmlXPathCmpNodesExt:
 * @node1:  the first node
 * @node2:  the second node
 *
 * Compare two nodes w.r.t document order.
 * This one is optimized for handling of non-element nodes.
 *
 * Returns -2 in case of error 1 if first point < second point, 0 if
 *         it's the same node, -1 otherwise
 */
unsafe extern "C" fn xml_xpath_cmp_nodes_ext(
    mut node1: XmlNodePtr,
    mut node2: XmlNodePtr,
) -> c_int {
    let mut depth1: c_int;
    let mut depth2: c_int;
    let mut misc: c_int = 0;
    let mut precedence1: c_int = 0;
    let mut precedence2: c_int = 0;
    let mut misc_node1: XmlNodePtr = null_mut();
    let mut misc_node2: XmlNodePtr = null_mut();
    let mut cur: XmlNodePtr;

    let mut l1: ptrdiff_t;
    let mut l2: ptrdiff_t;

    if node1.is_null() || node2.is_null() {
        return -2;
    }

    if node1 == node2 {
        return 0;
    }

    /*
     * a couple of optimizations which will avoid computations in most cases
     */
    'turtle_comparison: {
        match (*node1).typ {
            XmlElementType::XmlElementNode => {
                if matches!((*node2).typ, XmlElementType::XmlElementNode) {
                    if 0 > (*node1).content as ptrdiff_t
                        && 0 > (*node2).content as ptrdiff_t
                        && (*node1).doc == (*node2).doc
                    {
                        l1 = -((*node1).content as ptrdiff_t);
                        l2 = -((*node2).content as ptrdiff_t);
                        if l1 < l2 {
                            return 1;
                        }
                        if l1 > l2 {
                            return -1;
                        }
                    } else {
                        break 'turtle_comparison;
                    }
                }
            }
            XmlElementType::XmlAttributeNode => {
                precedence1 = 1; /* element is owner */
                misc_node1 = node1;
                node1 = (*node1).parent;
                misc = 1;
            }
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCdataSectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlPiNode => {
                misc_node1 = node1;
                /*
                 * Find nearest element node.
                 */
                if !(*node1).prev.is_null() {
                    loop {
                        node1 = (*node1).prev;
                        if matches!((*node1).typ, XmlElementType::XmlElementNode) {
                            precedence1 = 3; /* element in prev-sibl axis */
                            break;
                        }
                        if (*node1).prev.is_null() {
                            precedence1 = 2; /* element is parent */
                            /*
                             * URGENT TODO: Are there any cases, where the
                             * parent of such a node is not an element node?
                             */
                            node1 = (*node1).parent;
                            break;
                        }
                    }
                } else {
                    precedence1 = 2; /* element is parent */
                    node1 = (*node1).parent;
                }
                if node1.is_null()
                    || !matches!((*node1).typ, XmlElementType::XmlElementNode)
                    || 0 <= (*node1).content as ptrdiff_t
                {
                    /*
                     * Fallback for whatever case.
                     */
                    node1 = misc_node1;
                    precedence1 = 0;
                } else {
                    misc = 1;
                }
            }
            XmlElementType::XmlNamespaceDecl => {
                /*
                 * TODO: why do we return 1 for namespace nodes?
                 */
                return 1;
            }
            _ => {}
        }

        match (*node2).typ {
            XmlElementType::XmlElementNode => {}
            XmlElementType::XmlAttributeNode => {
                precedence2 = 1; /* element is owner */
                misc_node2 = node2;
                node2 = (*node2).parent;
                misc = 1;
            }
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCdataSectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlPiNode => {
                misc_node2 = node2;
                if !(*node2).prev.is_null() {
                    loop {
                        node2 = (*node2).prev;
                        if matches!((*node2).typ, XmlElementType::XmlElementNode) {
                            precedence2 = 3; /* element in prev-sibl axis */
                            break;
                        }
                        if (*node2).prev.is_null() {
                            precedence2 = 2; /* element is parent */
                            node2 = (*node2).parent;
                            break;
                        }
                    }
                } else {
                    precedence2 = 2; /* element is parent */
                    node2 = (*node2).parent;
                }
                if node2.is_null()
                    || !matches!((*node2).typ, XmlElementType::XmlElementNode)
                    || 0 <= (*node2).content as ptrdiff_t
                {
                    node2 = misc_node2;
                    precedence2 = 0;
                } else {
                    misc = 1;
                }
            }
            XmlElementType::XmlNamespaceDecl => {
                return 1;
            }
            _ => {}
        }
        if misc != 0 {
            if node1 == node2 {
                if precedence1 == precedence2 {
                    /*
                     * The ugly case; but normally there aren't many
                     * adjacent non-element nodes around.
                     */
                    cur = (*misc_node2).prev;
                    while !cur.is_null() {
                        if cur == misc_node1 {
                            return 1;
                        }
                        if matches!((*cur).typ, XmlElementType::XmlElementNode) {
                            return -1;
                        }
                        cur = (*cur).prev;
                    }
                    return -1;
                } else {
                    /*
                     * Evaluate based on higher precedence wrt to the element.
                     * TODO: This assumes attributes are sorted before content.
                     *   Is this 100% correct?
                     */
                    if precedence1 < precedence2 {
                        return 1;
                    } else {
                        return -1;
                    }
                }
            }
            /*
             * Special case: One of the helper-elements is contained by the other.
             * <foo>
             *   <node2>
             *     <node1>Text-1(precedence1 == 2)</node1>
             *   </node2>
             *   Text-6(precedence2 == 3)
             * </foo>
             */
            if precedence2 == 3 && precedence1 > 1 {
                cur = (*node1).parent;
                while !cur.is_null() {
                    if cur == node2 {
                        return 1;
                    }
                    cur = (*cur).parent;
                }
            }
            if precedence1 == 3 && precedence2 > 1 {
                cur = (*node2).parent;
                while !cur.is_null() {
                    if cur == node1 {
                        return -1;
                    }
                    cur = (*cur).parent;
                }
            }
        }

        /*
         * Speedup using document order if available.
         */
        if matches!((*node1).typ, XmlElementType::XmlElementNode)
            && matches!((*node2).typ, XmlElementType::XmlElementNode)
            && 0 > (*node1).content as ptrdiff_t
            && 0 > (*node2).content as ptrdiff_t
            && (*node1).doc == (*node2).doc
        {
            l1 = -((*node1).content as ptrdiff_t);
            l2 = -((*node2).content as ptrdiff_t);
            if l1 < l2 {
                return 1;
            }
            if l1 > l2 {
                return -1;
            }
        }
    }

    // turtle_comparison:

    if node1 == (*node2).prev {
        return 1;
    }
    if node1 == (*node2).next {
        return -1;
    }
    /*
     * compute depth to root
     */
    depth2 = 0;
    cur = node2;
    while !(*cur).parent.is_null() {
        if (*cur).parent == node1 {
            return 1;
        }
        depth2 += 1;
        cur = (*cur).parent;
    }
    let root: XmlNodePtr = cur;
    depth1 = 0;
    cur = node1;
    while !(*cur).parent.is_null() {
        if (*cur).parent == node2 {
            return -1;
        }
        depth1 += 1;
        cur = (*cur).parent;
    }
    /*
     * Distinct document (or distinct entities :-( ) case.
     */
    if root != cur {
        return -2;
    }
    /*
     * get the nearest common ancestor.
     */
    while depth1 > depth2 {
        depth1 -= 1;
        node1 = (*node1).parent;
    }
    while depth2 > depth1 {
        depth2 -= 1;
        node2 = (*node2).parent;
    }
    while (*node1).parent != (*node2).parent {
        node1 = (*node1).parent;
        node2 = (*node2).parent;
        /* should not happen but just in case ... */
        if node1.is_null() || node2.is_null() {
            return -2;
        }
    }
    /*
     * Find who's first.
     */
    if node1 == (*node2).prev {
        return 1;
    }
    if node1 == (*node2).next {
        return -1;
    }
    /*
     * Speedup using document order if available.
     */
    if matches!((*node1).typ, XmlElementType::XmlElementNode)
        && matches!((*node2).typ, XmlElementType::XmlElementNode)
        && 0 > (*node1).content as ptrdiff_t
        && 0 > (*node2).content as ptrdiff_t
        && (*node1).doc == (*node2).doc
    {
        l1 = -((*node1).content as ptrdiff_t);
        l2 = -((*node2).content as ptrdiff_t);
        if l1 < l2 {
            return 1;
        }
        if l1 > l2 {
            return -1;
        }
    }

    cur = (*node1).next;
    while !cur.is_null() {
        if cur == node2 {
            return 1;
        }
        cur = (*cur).next;
    }
    -1 /* assume there is no sibling list corruption */
}

/**
 * xmlXPathNodeSetSort:
 * @set:  the node set
 *
 * Sort the node set in document order
 */
pub unsafe extern "C" fn xml_xpath_node_set_sort(set: XmlNodeSetPtr) {
    // #ifndef WITH_TIM_SORT

    let mut j: c_int;
    let mut incr: c_int;
    let mut tmp: XmlNodePtr;

    // #endif

    if set.is_null() {
        return;
    }

    // #ifndef WITH_TIM_SORT
    /*
     * Use the old Shell's sort implementation to sort the node-set
     * Timsort ought to be quite faster
     */
    let len: c_int = (*set).node_nr;
    incr = len;
    while {
        incr /= 2;
        incr > 0
    } {
        for i in incr..len {
            j = i - incr;
            while j >= 0 {
                if xml_xpath_cmp_nodes_ext(
                    *(*set).node_tab.add(j as usize),
                    *(*set).node_tab.add(j as usize + incr as usize),
                ) == -1
                {
                    tmp = *(*set).node_tab.add(j as usize);
                    *(*set).node_tab.add(j as usize) =
                        *(*set).node_tab.add(j as usize + incr as usize);
                    *(*set).node_tab.add(j as usize + incr as usize) = tmp;
                    j -= incr;
                } else {
                    break;
                }
            }
        }
    }
}

/**
 * xmlXPathCacheNewNodeSet:
 * @ctxt: the XPath context
 * @val:  the NodePtr value
 *
 * This is the cached version of xmlXPathNewNodeSet().
 * Acquire an xmlXPathObjectPtr of type NodeSet and initialize
 * it with the single Node @val
 *
 * Returns the created or reused object.
 */
unsafe extern "C" fn xml_xpath_cache_new_node_set(
    ctxt: XmlXPathContextPtr,
    val: XmlNodePtr,
) -> XmlXPathObjectPtr {
    if !ctxt.is_null() && !(*ctxt).cache.is_null() {
        let cache: XmlXpathContextCachePtr = (*ctxt).cache as XmlXpathContextCachePtr;

        if !(*cache).nodeset_objs.is_null() && (*(*cache).nodeset_objs).number != 0 {
            /*
             * Use the nodeset-cache.
             */
            (*(*cache).nodeset_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).nodeset_objs)
                .items
                .add((*(*cache).nodeset_objs).number as usize)
                as XmlXPathObjectPtr;
            (*ret).typ = XmlXPathObjectType::XpathNodeset;
            (*ret).boolval = 0;
            if !val.is_null() {
                if (*(*ret).nodesetval).node_max == 0
                    || matches!((*val).typ, XmlElementType::XmlNamespaceDecl)
                {
                    /* TODO: Check memory error. */
                    xml_xpath_node_set_add_unique((*ret).nodesetval, val);
                } else {
                    *(*(*ret).nodesetval).node_tab.add(0) = val;
                    (*(*ret).nodesetval).node_nr = 1;
                }
            }
            return ret;
        } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
            /*
             * Fallback to misc-cache.
             */

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

            (*ret).typ = XmlXPathObjectType::XpathNodeset;
            (*ret).boolval = 0;
            (*ret).nodesetval = set;
            return ret;
        }
    }
    xml_xpath_new_node_set(val)
}

/**
 * xmlXPathRoot:
 * @ctxt:  the XPath Parser context
 *
 * Initialize the context to the root of the document
 */
pub unsafe extern "C" fn xml_xpath_root(ctxt: XmlXPathParserContextPtr) {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return;
    }
    value_push(
        ctxt,
        xml_xpath_cache_new_node_set((*ctxt).context, (*(*ctxt).context).doc as XmlNodePtr),
    );
}

/**
 * xmlXPathTryStreamCompile:
 * @ctxt: an XPath context
 * @str:  the XPath expression
 *
 * Try to compile the XPath expression as a streamable subset.
 *
 * Returns the compiled expression or NULL if failed to compile.
 */
#[cfg(feature = "libxml_pattern")]
pub unsafe extern "C" fn xml_xpath_try_stream_compile(
    ctxt: XmlXPathContextPtr,
    str: *const XmlChar,
) -> XmlXPathCompExprPtr {
    /*
     * Optimization: use streaming patterns when the XPath expression can
     * be compiled to a stream lookup
     */
    let stream: XmlPatternPtr;
    let comp: XmlXPathCompExprPtr;
    let mut dict: XmlDictPtr = null_mut();
    let mut namespaces: *mut *const XmlChar = null_mut();
    let mut ns: XmlNsPtr;
    let mut i: c_int;

    if xml_strchr(str, b'[').is_null()
        && xml_strchr(str, b'(').is_null()
        && xml_strchr(str, b'@').is_null()
    {
        /*
         * We don't try to handle expressions using the verbose axis
         * specifiers ("::"), just the simplified form at this point.
         * Additionally, if there is no list of namespaces available and
         *  there's a ":" in the expression, indicating a prefixed QName,
         *  then we won't try to compile either. xmlPatterncompile() needs
         *  to have a list of namespaces at compilation time in order to
         *  compile prefixed name tests.
         */
        let tmp: *const XmlChar = xml_strchr(str, b':');
        if !tmp.is_null() && (ctxt.is_null() || (*ctxt).ns_nr == 0 || *tmp.add(1) == b':') {
            return null_mut();
        }

        if !ctxt.is_null() {
            dict = (*ctxt).dict;
            if (*ctxt).ns_nr > 0 {
                namespaces =
                    xml_malloc(2 * ((*ctxt).ns_nr as usize + 1) * size_of::<*mut XmlChar>()) as _;
                if namespaces.is_null() {
                    xml_xpath_err_memory(ctxt, c"allocating namespaces array\n".as_ptr());
                    return null_mut();
                }
                i = 0;
                for j in 0..(*ctxt).ns_nr {
                    ns = *(*ctxt).namespaces.add(j as usize);
                    *namespaces.add(i as usize) = (*ns).href.load(Ordering::Relaxed);
                    i += 1;
                    *namespaces.add(i as usize) = (*ns).prefix.load(Ordering::Relaxed);
                    i += 1;
                }
                *namespaces.add(i as usize) = null_mut();
                i += 1;
                *namespaces.add(i as usize) = null_mut();
            }
        }

        stream = xml_patterncompile(
            str,
            dict,
            XmlPatternFlags::XmlPatternXpath as i32,
            namespaces,
        );
        if !namespaces.is_null() {
            xml_free(namespaces as _);
        }
        if !stream.is_null() && xml_pattern_streamable(stream) == 1 {
            comp = xml_xpath_new_comp_expr();
            if comp.is_null() {
                xml_xpath_err_memory(ctxt, c"allocating streamable expression\n".as_ptr());
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

/**
 * xmlXPathCompExprAdd:
 * @comp:  the compiled expression
 * @ch1: first child index
 * @ch2: second child index
 * @op:  an op
 * @value:  the first value: c_int
 * @value2:  the second value: c_int
 * @value3:  the third value: c_int
 * @value4:  the first string value
 * @value5:  the second string value
 *
 * Add a step to an XPath Compiled Expression
 *
 * Returns -1 in case of failure, the index otherwise
 */
unsafe extern "C" fn xml_xpath_comp_expr_add(
    ctxt: XmlXPathParserContextPtr,
    ch1: c_int,
    ch2: c_int,
    op: XmlXPathOp,
    value: c_int,
    value2: c_int,
    value3: c_int,
    value4: *mut c_void,
    value5: *mut c_void,
) -> c_int {
    let comp: XmlXPathCompExprPtr = (*ctxt).comp;
    if (*comp).nb_step >= (*comp).max_step {
        if (*comp).max_step >= XPATH_MAX_STEPS as i32 {
            xml_xpath_perr_memory(ctxt, c"adding step\n".as_ptr());
            return -1;
        }
        (*comp).max_step *= 2;
        let real: *mut XmlXPathStepOp = xml_realloc(
            (*comp).steps as _,
            (*comp).max_step as usize * size_of::<XmlXPathStepOp>(),
        ) as *mut XmlXPathStepOp;
        if real.is_null() {
            (*comp).max_step /= 2;
            xml_xpath_perr_memory(ctxt, c"adding step\n".as_ptr());
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
    NodeTypePI = XmlElementType::XmlPiNode as isize,
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
 *   CUR_PTR return the current poc_inter to the xmlChar to be parsed.
 *   CUR     returns the current xmlChar value, i.e. a 8 bit value
 *           in ISO-Latin or UTF-8.
 *           This should be used c_internally by the parser
 *           only to compare to ASCII values otherwise it would break when
 *           running with UTF-8 encoding.
 *   NXT(n)  returns the n'th next xmlChar. Same as CUR is should be used only
 *           to compare on ASCII based substring.
 *   SKIP(n) Skip n xmlChar, and must also be used only to skip ASCII defined
 *           strings within the parser.
 *   CURRENT Returns the current c_char value, with the full decoding of
 *           UTF-8 if we are using this mode. It returns an c_int.
 *   NEXT    Skip to the next character, this does the proper decoding
 *           in UTF-8 mode. It also pop-up unfinished entities on the fly.
 *           It returns the poc_inter to the current xmlChar.
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

/**
 * xmlXPathCurrentChar:
 * @ctxt:  the XPath parser context
 * @cur:  poc_inter to the beginning of the c_char
 * @len:  poc_inter to the length of the c_char read
 *
 * The current c_char value, if using UTF-8 this may actually span multiple
 * bytes in the input buffer.
 *
 * Returns the current c_char value and its length
 */

unsafe extern "C" fn xml_xpath_current_char(
    ctxt: XmlXPathParserContextPtr,
    len: *mut c_int,
) -> c_int {
    let mut val: c_uint;

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
        let c: c_uchar = *cur;
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

            if !xml_is_char(val as u32) {
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

/**
 * xmlXPathScanName:
 * @ctxt:  the XPath Parser context
 *
 * Trickery: parse an XML name but without consuming the input flow
 * Needed to avoid insanity in the parser state.
 *
 * [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' |
 *                  CombiningChar | Extender
 *
 * [5] Name ::= (Letter | '_' | ':') (NameChar)*
 *
 * [6] Names ::= Name (S Name)*
 *
 * Returns the Name parsed or NULL
 */

unsafe extern "C" fn xml_xpath_scan_name(ctxt: XmlXPathParserContextPtr) -> *mut XmlChar {
    let mut l: c_int = 0;
    let mut c: c_int;

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

/**
 * xmlXPathIsAxisName:
 * @name:  a preparsed name token
 *
 * [6] AxisName ::=   'ancestor'
 *                  | 'ancestor-or-self'
 *                  | 'attribute'
 *                  | 'child'
 *                  | 'descendant'
 *                  | 'descendant-or-self'
 *                  | 'following'
 *                  | 'following-sibling'
 *                  | 'namespace'
 *                  | 'parent'
 *                  | 'preceding'
 *                  | 'preceding-sibling'
 *                  | 'self'
 *
 * Returns the axis or 0
 */
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

/**
 * xmlXPathParseLiteral:
 * @ctxt:  the XPath Parser context
 *
 * Parse a Literal
 *
 *  [29]   Literal ::=   '"' [^"]* '"'
 *                    | "'" [^']* "'"
 *
 * Returns the value found or NULL in case of error
 */
unsafe extern "C" fn xml_xpath_parse_literal(ctxt: XmlXPathParserContextPtr) -> *mut XmlChar {
    let q: *const XmlChar;
    let ret: *mut XmlChar;

    if CUR!(ctxt) == b'"' {
        NEXT!(ctxt);
        q = CUR_PTR!(ctxt);
        while IS_CHAR_CH!(CUR!(ctxt)) && CUR!(ctxt) != b'"' {
            NEXT!(ctxt);
        }
        if !IS_CHAR_CH!(CUR!(ctxt)) {
            XP_ERRORNULL!(ctxt, XmlXPathError::XpathUnfinishedLiteralError as i32);
        } else {
            ret = xml_strndup(q, CUR_PTR!(ctxt).offset_from(q) as _);
            NEXT!(ctxt);
        }
    } else if CUR!(ctxt) == b'\'' {
        NEXT!(ctxt);
        q = CUR_PTR!(ctxt);
        while IS_CHAR_CH!(CUR!(ctxt)) && CUR!(ctxt) != b'\'' {
            NEXT!(ctxt);
        }
        if !IS_CHAR_CH!(CUR!(ctxt)) {
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

/**
 * xmlXPathCompNodeTest:
 * @ctxt:  the XPath Parser context
 * @test:  poc_inter to a xmlXPathTestVal
 * @type:  poc_inter to a xmlXPathTypeVal
 * @prefix:  placeholder for a possible name prefix
 *
 * [7] NodeTest ::=   NameTest
 *            | NodeType '(' ')'
 *            | 'processing-instruction' '(' Literal ')'
 *
 * [37] NameTest ::=  '*'
 *            | NCName ':' '*'
 *            | QName
 * [38] NodeType ::= 'comment'
 *           | 'text'
 *           | 'processing-instruction'
 *           | 'node'
 *
 * Returns the name found and updates @test, @type and @prefix appropriately
 */
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

    let blanks: c_int = xml_is_blank_char(CUR!(ctxt) as u32) as i32;
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

/**
 * xmlXPathCompPredicate:
 * @ctxt:  the XPath Parser context
 * @filter:  act as a filter
 *
 *  [8]   Predicate ::=   '[' PredicateExpr ']'
 *  [9]   PredicateExpr ::=   Expr
 *
 * Compile a predicate expression
 */
unsafe extern "C" fn xml_xpath_comp_predicate(ctxt: XmlXPathParserContextPtr, filter: c_int) {
    let op1: c_int = (*(*ctxt).comp).last;

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

/**
 * xmlXPathCompStep:
 * @ctxt:  the XPath Parser context
 *
 * [4] Step ::=   AxisSpecifier NodeTest Predicate*
 *                  | AbbreviatedStep
 *
 * [12] AbbreviatedStep ::=   '.' | '..'
 *
 * [5] AxisSpecifier ::= AxisName '::'
 *                  | AbbreviatedAxisSpecifier
 *
 * [13] AbbreviatedAxisSpecifier ::= '@'?
 *
 * Modified for XPtr range support as:
 *
 *  [4xptr] Step ::= AxisSpecifier NodeTest Predicate*
 *                     | AbbreviatedStep
 *                     | 'range-to' '(' Expr ')' Predicate*
 *
 * Compile one step in a Location Path
 * A location step of . is short for self::node(). This is
 * particularly useful in conjunction with //. For example, the
 * location path .//para is short for
 * self::node()/descendant-or-self::node()/child::para
 * and so will select all para descendant elements of the context
 * node.
 * Similarly, a location step of .. is short for parent::node().
 * For example, ../title is short for parent::node()/child::title
 * and so will select the title children of the parent of the context
 * node.
 */
unsafe extern "C" fn xml_xpath_comp_step(ctxt: XmlXPathParserContextPtr) {
    #[cfg(feature = "libxml_xptr_locs")]
    let mut rangeto: c_int = 0;
    #[cfg(feature = "libxml_xptr_locs")]
    let mut op2: c_int = -1;

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
         * The modification needed for XPoc_inter change to the production
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
            // #ifdef DEBUG_STEP
            // 	xmlGenericError(xmlGenericErrorContext(),
            // 		"Basis : computing new set\n");
            // #endif

            // #ifdef DEBUG_STEP
            // 	xmlGenericError(xmlGenericErrorContext, "Basis : ");
            // 	if (*ctxt).value.is_null()
            // 	    xmlGenericError(xmlGenericErrorContext, "no value\n");
            // 	else if ((*(*ctxt).value).nodesetval.is_null())
            // 	    xmlGenericError(xmlGenericErrorContext, "Empty\n");
            // 	else
            // 	    xmlGenericErrorContextNodeSet(stdout, (*(*ctxt).value).nodesetval);
            // #endif
        }

        // #ifdef LIBXML_XPTR_LOCS_ENABLED
        // eval_predicates:
        // #endif
        let op1: c_int = (*(*ctxt).comp).last;
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
    // #ifdef DEBUG_STEP
    //     xmlGenericError(xmlGenericErrorContext, "Step : ");
    //     if (*ctxt).value.is_null()
    // 	xmlGenericError(xmlGenericErrorContext, "no value\n");
    //     else if ((*(*ctxt).value).nodesetval.is_null())
    // 	xmlGenericError(xmlGenericErrorContext, "Empty\n");
    //     else
    // 	xmlGenericErrorContextNodeSet(xmlGenericErrorContext,
    // 		(*(*ctxt).value).nodesetval);
    // #endif
}

/**
 * xmlXPathCompRelativeLocationPath:
 * @ctxt:  the XPath Parser context
 *
 *  [3]   RelativeLocationPath ::=   Step
 *                     | RelativeLocationPath '/' Step
 *                     | AbbreviatedRelativeLocationPath
 *  [11]  AbbreviatedRelativeLocationPath ::=   RelativeLocationPath '//' Step
 *
 * Compile a relative location path.
 */
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

/**
 * xmlXPathCompLocationPath:
 * @ctxt:  the XPath Parser context
 *
 *  [1]   LocationPath ::=   RelativeLocationPath
 *                     | AbsoluteLocationPath
 *  [2]   AbsoluteLocationPath ::=   '/' RelativeLocationPath?
 *                     | AbbreviatedAbsoluteLocationPath
 *  [10]   AbbreviatedAbsoluteLocationPath ::=
 *                           '//' RelativeLocationPath
 *
 * Compile a location path
 *
 * // is short for /descendant-or-self::node()/. For example,
 * //para is short for /descendant-or-self::node()/child::para and
 * so will select any para element in the document (even a para element
 * that is a document element will be selected by //para since the
 * document element node is a child of the root node); div//para is
 * short for div/descendant-or-self::node()/child::para and so will
 * select all para descendants of div children.
 */
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
                    && (IS_ASCII_LETTER!(CUR!(ctxt))
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

/**
 * xmlXPathParseQName:
 * @ctxt:  the XPath Parser context
 * @prefix:  a xmlChar **
 *
 * parse an XML qualified name
 *
 * [NS 5] QName ::= (Prefix ':')? LocalPart
 *
 * [NS 6] Prefix ::= NCName
 *
 * [NS 7] LocalPart ::= NCName
 *
 * Returns the function returns the local part, and prefix is updated
 *   to get the Prefix if any.
 */

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

/**
 * xmlXPathCompVariableReference:
 * @ctxt:  the XPath Parser context
 *
 * Parse a VariableReference, evaluate it and push it on the stack.
 *
 * The variable bindings consist of a mapping from variable names
 * to variable values. The value of a variable is an object, which can be
 * of any of the types that are possible for the value of an expression,
 * and may also be of additional types not specified here.
 *
 * Early evaluation is possible since:
 * The variable bindings [...] used to evaluate a subexpression are
 * always the same as those used to evaluate the containing expression.
 *
 *  [36]   VariableReference ::=   '$' QName
 */
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

/**
 * xmlXPathCompNumber:
 * @ctxt:  the XPath Parser context
 *
 *  [30]   Number ::=   Digits ('.' Digits?)?
 *                    | '.' Digits
 *  [31]   Digits ::=   [0-9]+
 *
 * Compile a Number, then push it on the stack
 *
 */
unsafe extern "C" fn xml_xpath_comp_number(ctxt: XmlXPathParserContextPtr) {
    let mut ret: f64;
    let mut ok: c_int = 0;
    let mut exponent: c_int = 0;
    let mut is_exponent_negative: c_int = 0;

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
        let mut v: c_int;
        let mut frac: c_int = 0;
        let mut fraction: f64 = 0.0;

        NEXT!(ctxt);
        if (CUR!(ctxt) < b'0' || CUR!(ctxt) > b'9') && ok == 0 {
            XP_ERROR!(ctxt, XmlXPathError::XpathNumberError as i32);
        }
        while CUR!(ctxt) == b'0' {
            frac += 1;
            NEXT!(ctxt);
        }
        let max: c_int = frac + MAX_FRAC as i32;
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
        XmlXPathObjectType::XpathNumber as i32,
        0,
        0,
        num as _,
        null_mut()
    ) == -1
    {
        xml_xpath_release_object((*ctxt).context, num);
    }
}

/**
 * xmlXPathCompLiteral:
 * @ctxt:  the XPath Parser context
 *
 * Parse a Literal and push it on the stack.
 *
 *  [29]   Literal ::=   '"' [^"]* '"'
 *                    | "'" [^']* "'"
 *
 * TODO: xmlXPathCompLiteral memory allocation could be improved.
 */
unsafe extern "C" fn xml_xpath_comp_literal(ctxt: XmlXPathParserContextPtr) {
    let q: *const XmlChar;
    let ret: *mut XmlChar;

    if CUR!(ctxt) == b'"' {
        NEXT!(ctxt);
        q = CUR_PTR!(ctxt);
        while IS_CHAR_CH!(CUR!(ctxt)) && CUR!(ctxt) != b'"' {
            NEXT!(ctxt);
        }
        if !IS_CHAR_CH!(CUR!(ctxt)) {
            XP_ERROR!(ctxt, XmlXPathError::XpathUnfinishedLiteralError as i32);
        } else {
            ret = xml_strndup(q, CUR_PTR!(ctxt).offset_from(q) as _);
            NEXT!(ctxt);
        }
    } else if CUR!(ctxt) == b'\'' {
        NEXT!(ctxt);
        q = CUR_PTR!(ctxt);
        while IS_CHAR_CH!(CUR!(ctxt)) && CUR!(ctxt) != b'\'' {
            NEXT!(ctxt);
        }
        if !IS_CHAR_CH!(CUR!(ctxt)) {
            XP_ERROR!(ctxt, XmlXPathError::XpathUnfinishedLiteralError as i32);
        } else {
            ret = xml_strndup(q, CUR_PTR!(ctxt).offset_from(q) as _);
            NEXT!(ctxt);
        }
    } else {
        XP_ERROR!(ctxt, XmlXPathError::XpathStartLiteralError as i32);
    }
    if ret.is_null() {
        xml_xpath_perr_memory(ctxt, null());
        return;
    }
    let lit: XmlXPathObjectPtr = xml_xpath_cache_new_string((*ctxt).context, ret);
    if lit.is_null() {
        (*ctxt).error = XmlXPathError::XpathMemoryError as i32;
    } else if PUSH_LONG_EXPR!(
        ctxt,
        XmlXPathOp::XpathOpValue,
        XmlXPathObjectType::XpathString,
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

/**
 * xmlXPathCompFunctionCall:
 * @ctxt:  the XPath Parser context
 *
 *  [16]   FunctionCall ::=   FunctionName '(' ( Argument ( ',' Argument)*)? ')'
 *  [17]   Argument ::=   Expr
 *
 * Compile a function call, the evaluation of all arguments are
 * pushed on the stack
 */
unsafe extern "C" fn xml_xpath_comp_function_call(ctxt: XmlXPathParserContextPtr) {
    let mut prefix: *mut XmlChar = null_mut();
    let mut nbargs: c_int = 0;
    let mut sort: c_int = 1;

    let name: *mut XmlChar = xml_xpath_parse_qname(ctxt, addr_of_mut!(prefix));
    if name.is_null() {
        xml_free(prefix as _);
        XP_ERROR!(ctxt, XmlXPathError::XpathExprError as i32);
    }
    SKIP_BLANKS!(ctxt);
    // #ifdef DEBUG_EXPR
    //     if prefix.is_null()
    // 	xmlGenericError(xmlGenericErrorContext, "Calling function %s\n",
    // 			name);
    //     else
    // 	xmlGenericError(xmlGenericErrorContext, "Calling function %s:%s\n",
    // 			prefix, name);
    // #endif

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
            let op1: c_int = (*(*ctxt).comp).last;
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

/**
 * xmlXPathCompPrimaryExpr:
 * @ctxt:  the XPath Parser context
 *
 *  [15]   PrimaryExpr ::=   VariableReference
 *                | '(' Expr ')'
 *                | Literal
 *                | Number
 *                | FunctionCall
 *
 * Compile a primary expression.
 */
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
    } else if IS_ASCII_DIGIT!(CUR!(ctxt)) || (CUR!(ctxt) == b'.' && IS_ASCII_DIGIT!(NXT!(ctxt, 1)))
    {
        xml_xpath_comp_number(ctxt);
    } else if CUR!(ctxt) == b'\'' || CUR!(ctxt) == b'"' {
        xml_xpath_comp_literal(ctxt);
    } else {
        xml_xpath_comp_function_call(ctxt);
    }
    SKIP_BLANKS!(ctxt);
}

/**
 * xmlXPathCompFilterExpr:
 * @ctxt:  the XPath Parser context
 *
 *  [20]   FilterExpr ::=   PrimaryExpr
 *               | FilterExpr Predicate
 *
 * Compile a filter expression.
 * Square brackets are used to filter expressions in the same way that
 * they are used in location paths. It is an error if the expression to
 * be filtered does not evaluate to a node-set. The context node list
 * used for evaluating the expression in square brackets is the node-set
 * to be filtered listed in document order.
 */

unsafe extern "C" fn xml_xpath_comp_filter_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_primary_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);

    while CUR!(ctxt) == b'[' {
        xml_xpath_comp_predicate(ctxt, 1);
        SKIP_BLANKS!(ctxt);
    }
}

/**
 * xmlXPathCompPathExpr:
 * @ctxt:  the XPath Parser context
 *
 *  [19]   PathExpr ::=   LocationPath
 *               | FilterExpr
 *               | FilterExpr '/' RelativeLocationPath
 *               | FilterExpr '//' RelativeLocationPath
 *
 * Compile a path expression.
 * The / operator and // operators combine an arbitrary expression
 * and a relative location path. It is an error if the expression
 * does not evaluate to a node-set.
 * The / operator does composition in the same way as when / is
 * used in a location path. As in location paths, // is short for
 * /descendant-or-self::node()/.
 */

unsafe extern "C" fn xml_xpath_comp_path_expr(ctxt: XmlXPathParserContextPtr) {
    let mut lc: c_int = 1; /* Should we branch to LocationPath ?         */
    let name: *mut XmlChar; /* we may have to preparse a name to find out */

    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) == b'$'
        || CUR!(ctxt) == b'('
        || IS_ASCII_DIGIT!(CUR!(ctxt))
        || CUR!(ctxt) == b'\''
        || CUR!(ctxt) == b'"'
        || (CUR!(ctxt) == b'.' && IS_ASCII_DIGIT!(NXT!(ctxt, 1)))
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
         * mac_intain parsed token content through the recursive function
         * calls. This looks uglier but makes the code easier to
         * read/write/debug.
         */
        SKIP_BLANKS!(ctxt);
        name = xml_xpath_scan_name(ctxt);
        if !name.is_null() && !xml_strstr(name, c"::".as_ptr() as _).is_null() {
            lc = 1;
            xml_free(name as _);
        } else if !name.is_null() {
            let mut len: c_int = xml_strlen(name);

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

/**
 * xmlXPathCompUnionExpr:
 * @ctxt:  the XPath Parser context
 *
 *  [18]   UnionExpr ::=   PathExpr
 *               | UnionExpr '|' PathExpr
 *
 * Compile an union expression.
 */

unsafe extern "C" fn xml_xpath_comp_union_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_path_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'|' {
        let op1: c_int = (*(*ctxt).comp).last;
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

/**
 * xmlXPathCompUnaryExpr:
 * @ctxt:  the XPath Parser context
 *
 *  [27]   UnaryExpr ::=   UnionExpr
 *                   | '-' UnaryExpr
 *
 * Compile an unary expression.
 */

unsafe extern "C" fn xml_xpath_comp_unary_expr(ctxt: XmlXPathParserContextPtr) {
    let mut minus: c_int = 0;
    let mut found: c_int = 0;

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

/**
 * xmlXPathCompMultiplicativeExpr:
 * @ctxt:  the XPath Parser context
 *
 *  [26]   MultiplicativeExpr ::=   UnaryExpr
 *                   | MultiplicativeExpr MultiplyOperator UnaryExpr
 *                   | MultiplicativeExpr 'div' UnaryExpr
 *                   | MultiplicativeExpr 'mod' UnaryExpr
 *  [34]   MultiplyOperator ::=   '*'
 *
 * Compile an Additive expression.
 */

unsafe extern "C" fn xml_xpath_comp_multiplicative_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_unary_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'*'
        || (CUR!(ctxt) == b'd' && NXT!(ctxt, 1) == b'i' && NXT!(ctxt, 2) == b'v')
        || (CUR!(ctxt) == b'm' && NXT!(ctxt, 1) == b'o' && NXT!(ctxt, 2) == b'd')
    {
        let mut op: c_int = -1;
        let op1: c_int = (*(*ctxt).comp).last;

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

/**
 * xmlXPathCompAdditiveExpr:
 * @ctxt:  the XPath Parser context
 *
 *  [25]   AdditiveExpr ::=   MultiplicativeExpr
 *                   | AdditiveExpr '+' MultiplicativeExpr
 *                   | AdditiveExpr '-' MultiplicativeExpr
 *
 * Compile an Additive expression.
 */

unsafe extern "C" fn xml_xpath_comp_additive_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_multiplicative_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'+' || CUR!(ctxt) == b'-' {
        let op1: c_int = (*(*ctxt).comp).last;
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

/**
 * xmlXPathCompRelationalExpr:
 * @ctxt:  the XPath Parser context
 *
 *  [24]   RelationalExpr ::=   AdditiveExpr
 *                 | RelationalExpr '<' AdditiveExpr
 *                 | RelationalExpr '>' AdditiveExpr
 *                 | RelationalExpr '<=' AdditiveExpr
 *                 | RelationalExpr '>=' AdditiveExpr
 *
 *  A <= B > C is allowed ? Answer from James, yes with
 *  (AdditiveExpr <= AdditiveExpr) > AdditiveExpr
 *  which is basically what got implemented.
 *
 * Compile a Relational expression, then push the result
 * on the stack
 */

unsafe extern "C" fn xml_xpath_comp_relational_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_additive_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'<' || CUR!(ctxt) == b'>' {
        let op1: c_int = (*(*ctxt).comp).last;
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

/**
 * xmlXPathCompEqualityExpr:
 * @ctxt:  the XPath Parser context
 *
 *  [23]   EqualityExpr ::=   RelationalExpr
 *                 | EqualityExpr '=' RelationalExpr
 *                 | EqualityExpr '!=' RelationalExpr
 *
 *  A != B != C is allowed ? Answer from James, yes with
 *  (RelationalExpr = RelationalExpr) = RelationalExpr
 *  (RelationalExpr != RelationalExpr) != RelationalExpr
 *  which is basically what got implemented.
 *
 * Compile an Equality expression.
 *
 */
unsafe extern "C" fn xml_xpath_comp_equality_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_relational_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'=' || (CUR!(ctxt) == b'!' && NXT!(ctxt, 1) == b'=') {
        let op1: c_int = (*(*ctxt).comp).last;
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

/**
 * xmlXPathCompAndExpr:
 * @ctxt:  the XPath Parser context
 *
 *  [22]   AndExpr ::=   EqualityExpr
 *                 | AndExpr 'and' EqualityExpr
 *
 * Compile an AND expression.
 *
 */
unsafe extern "C" fn xml_xpath_comp_and_expr(ctxt: XmlXPathParserContextPtr) {
    xml_xpath_comp_equality_expr(ctxt);
    CHECK_ERROR!(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'a' && NXT!(ctxt, 1) == b'n' && NXT!(ctxt, 2) == b'd' {
        let op1: c_int = (*(*ctxt).comp).last;
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

/**
 * xmlXPathCompileExpr:
 * @ctxt:  the XPath Parser context
 *
 *  [14]   Expr ::=   OrExpr
 *  [21]   OrExpr ::=   AndExpr
 *                 | OrExpr 'or' AndExpr
 *
 * Parse and compile an expression
 */
pub unsafe extern "C" fn xml_xpath_compile_expr(ctxt: XmlXPathParserContextPtr, sort: c_int) {
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
        let op1: c_int = (*(*ctxt).comp).last;
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
     * c_internal representation.
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

/**
 * xmlXPathRunStreamEval:
 * @ctxt:  the XPath parser context with the compiled expression
 *
 * Evaluate the Precompiled Streamable XPath expression in the given context.
 */
#[cfg(feature = "libxml_pattern")]
unsafe extern "C" fn xml_xpath_run_stream_eval(
    ctxt: XmlXPathContextPtr,
    comp: XmlPatternPtr,
    result_seq: *mut XmlXPathObjectPtr,
    to_bool: c_int,
) -> c_int {
    let mut max_depth: c_int;
    let mut ret: c_int;
    let mut depth: c_int;
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
    let min_depth: c_int = xml_pattern_min_depth(comp);
    if min_depth == -1 {
        return -1;
    }
    let from_root: c_int = xml_pattern_from_root(comp);
    if from_root < 0 {
        return -1;
    }
    // #if 0
    //     prc_intf("stream eval: depth %d from root %d\n", max_depth, from_root);
    // #endif

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
        match (*(*ctxt).node).typ {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHtmlDocumentNode => {
                cur = (*ctxt).node;
            }
            XmlElementType::XmlAttributeNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCdataSectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPiNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDtdNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXincludeStart
            | XmlElementType::XmlXincludeEnd => {}
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

    let eval_all_nodes: c_int = xml_stream_wants_any_node(patstream);

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

                    match (*cur).typ {
                        XmlElementType::XmlElementNode
                        | XmlElementType::XmlTextNode
                        | XmlElementType::XmlCdataSectionNode
                        | XmlElementType::XmlCommentNode
                        | XmlElementType::XmlPiNode => 'to_break: {
                            ret = if matches!((*cur).typ, XmlElementType::XmlElementNode) {
                                xml_stream_push(
                                    patstream,
                                    (*cur).name,
                                    if !(*cur).ns.is_null() {
                                        (*(*cur).ns).href.load(Ordering::Relaxed)
                                    } else {
                                        null_mut()
                                    },
                                )
                            } else if eval_all_nodes != 0 {
                                xml_stream_push_node(patstream, null(), null(), (*cur).typ as i32)
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
                            if (*cur).children.is_null() || depth >= max_depth {
                                // ret =
                                xml_stream_pop(patstream);
                                while !(*cur).next.is_null() {
                                    cur = (*cur).next;
                                    if !matches!(
                                        (*cur).typ,
                                        XmlElementType::XmlEntityDecl | XmlElementType::XmlDtdNode
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
                if matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
                    break 'main;
                }
                if !(*cur).children.is_null() && depth < max_depth {
                    /*
                     * Do not descend on entities declarations
                     */
                    if !matches!((*(*cur).children).typ, XmlElementType::XmlEntityDecl) {
                        cur = (*cur).children;
                        depth += 1;
                        /*
                         * Skip DTDs
                         */
                        if !matches!((*cur).typ, XmlElementType::XmlDtdNode) {
                            break 'to_continue_main;
                        }
                    }
                }

                if cur == limit {
                    break 'main;
                }

                while !(*cur).next.is_null() {
                    cur = (*cur).next;
                    if !matches!(
                        (*cur).typ,
                        XmlElementType::XmlEntityDecl | XmlElementType::XmlDtdNode
                    ) {
                        // goto next_node;
                        continue 'next_node;
                    }
                }

                break 'next_node;
            }

            'inner: while {
                cur = (*cur).parent;
                depth -= 1;
                if cur.is_null()
                    || cur == limit
                    || matches!((*cur).typ, XmlElementType::XmlDocumentNode)
                {
                    // goto done;
                    break 'main;
                }
                if matches!((*cur).typ, XmlElementType::XmlElementNode)
                    || (eval_all_nodes != 0
                        && matches!(
                            (*cur).typ,
                            XmlElementType::XmlTextNode
                                | XmlElementType::XmlCdataSectionNode
                                | XmlElementType::XmlCommentNode
                                | XmlElementType::XmlPiNode
                        ))
                {
                    // ret =
                    xml_stream_pop(patstream);
                };
                if !(*cur).next.is_null() {
                    cur = (*cur).next;
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

/**
 * xmlXPathCheckOpLimit:
 * @ctxt:  the XPath Parser context
 * @opCount:  the number of operations to be added
 *
 * Adds opCount to the running total of operations and returns -1 if the
 * operation limit is exceeded. Returns 0 otherwise.
 */
unsafe extern "C" fn xml_xpath_check_op_limit(
    ctxt: XmlXPathParserContextPtr,
    op_count: c_ulong,
) -> c_int {
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

/*
 * xmlXPathNodeSetMergeFunction:
 * Used for merging node sets in xmlXPathCollectAndTest().
 */
pub type XmlXPathNodeSetMergeFunction =
    unsafe extern "C" fn(XmlNodeSetPtr, XmlNodeSetPtr) -> XmlNodeSetPtr;

/*
 * A traversal function enumerates nodes along an axis.
 * Initially it must be called with NULL, and it indicates
 * termination on the axis by returning NULL.
 */
pub type XmlXPathTraversalFunction =
    unsafe extern "C" fn(ctxt: XmlXPathParserContextPtr, cur: XmlNodePtr) -> XmlNodePtr;

/**
 * xmlXPathNodeSetClearFromPos:
 * @set: the node set to be cleared
 * @pos: the start position to clear from
 *
 * Clears the list from temporary XPath objects (e.g. namespace nodes
 * are feed) starting with the entry at @pos, but does *not* free the list
 * itself. Sets the length of the list to @pos.
 */
unsafe extern "C" fn xml_xpath_node_set_clear_from_pos(
    set: XmlNodeSetPtr,
    pos: c_int,
    has_ns_nodes: c_int,
) {
    if set.is_null() || pos >= (*set).node_nr {
        return;
    } else if has_ns_nodes != 0 {
        let mut node: XmlNodePtr;

        for i in pos..(*set).node_nr {
            node = *(*set).node_tab.add(i as usize);
            if !node.is_null() && matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
                xml_xpath_node_set_free_ns(node as XmlNsPtr);
            }
        }
    }
    (*set).node_nr = pos;
}

/**
 * xmlXPathNodeSetClear:
 * @set:  the node set to clear
 *
 * Clears the list from all temporary XPath objects (e.g. namespace nodes
 * are feed), but does *not* free the list itself. Sets the length of the
 * list to 0.
 */
unsafe extern "C" fn xml_xpath_node_set_clear(set: XmlNodeSetPtr, has_ns_nodes: c_int) {
    xml_xpath_node_set_clear_from_pos(set, 0, has_ns_nodes);
}

/**
 * xmlXPathNodeSetMergeAndClear:
 * @set1:  the first NodeSet or NULL
 * @set2:  the second NodeSet
 *
 * Merges two nodesets, all nodes from @set2 are added to @set1.
 * Checks for duplicate nodes. Clears set2.
 *
 * Returns @set1 once extended or NULL in case of error.
 *
 * Frees @set1 in case of error.
 */
unsafe extern "C" fn xml_xpath_node_set_merge_and_clear(
    set1: XmlNodeSetPtr,
    set2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let mut n1: XmlNodePtr;
    let mut n2: XmlNodePtr;

    let init_nb_set1: c_int = (*set1).node_nr;
    'b: for i in 0..(*set2).node_nr {
        n2 = *(*set2).node_tab.add(i as usize);
        /*
         * Skip duplicates.
         */
        for j in 0..init_nb_set1 {
            n1 = *(*set1).node_tab.add(j as usize);
            if n1 == n2 {
                // goto skip_node;
                *(*set2).node_tab.add(i as usize) = null_mut();
                continue 'b;
            } else if matches!((*n1).typ, XmlElementType::XmlNamespaceDecl)
                && matches!((*n2).typ, XmlElementType::XmlNamespaceDecl)
                && (*(n1 as XmlNsPtr)).next.load(Ordering::Relaxed)
                    == (*(n2 as XmlNsPtr)).next.load(Ordering::Relaxed)
                && xml_str_equal(
                    (*(n1 as XmlNsPtr)).prefix.load(Ordering::Relaxed),
                    (*(n2 as XmlNsPtr)).prefix.load(Ordering::Relaxed),
                )
            {
                /*
                 * Free the namespace node.
                 */
                xml_xpath_node_set_free_ns(n2 as XmlNsPtr);
                // goto skip_node;
                *(*set2).node_tab.add(i as usize) = null_mut();
                continue 'b;
            }
        }
        /*
         * grow the nodeTab if needed
         */
        if (*set1).node_max == 0 {
            (*set1).node_tab =
                xml_malloc(XML_NODESET_DEFAULT * size_of::<XmlNodePtr>()) as *mut XmlNodePtr;
            if (*set1).node_tab.is_null() {
                xml_xpath_err_memory(null_mut(), c"merging nodeset\n".as_ptr() as _);
                // goto error;
                xml_xpath_free_node_set(set1);
                xml_xpath_node_set_clear(set2, 1);
                return null_mut();
            }
            memset(
                (*set1).node_tab as _,
                0,
                XML_NODESET_DEFAULT * size_of::<XmlNodePtr>(),
            );
            (*set1).node_max = XML_NODESET_DEFAULT as _;
        } else if (*set1).node_nr >= (*set1).node_max {
            if (*set1).node_max >= XPATH_MAX_NODESET_LENGTH as i32 {
                xml_xpath_err_memory(null_mut(), c"merging nodeset hit limit\n".as_ptr() as _);
                // goto error;
                xml_xpath_free_node_set(set1);
                xml_xpath_node_set_clear(set2, 1);
                return null_mut();
            }
            let temp: *mut XmlNodePtr = xml_realloc(
                (*set1).node_tab as _,
                (*set1).node_max as usize * 2 * size_of::<XmlNodePtr>(),
            ) as *mut XmlNodePtr;
            if temp.is_null() {
                xml_xpath_err_memory(null_mut(), c"merging nodeset\n".as_ptr() as _);
                // goto error;
                xml_xpath_free_node_set(set1);
                xml_xpath_node_set_clear(set2, 1);
                return null_mut();
            }
            (*set1).node_tab = temp;
            (*set1).node_max *= 2;
        }
        *(*set1).node_tab.add((*set1).node_nr as usize) = n2;
        (*set1).node_nr += 1;
        // skip_node:
        *(*set2).node_tab.add(i as usize) = null_mut();
    }
    (*set2).node_nr = 0;
    set1

    // error:
    // xmlXPathFreeNodeSet(set1);
    // xmlXPathNodeSetClear(set2, 1);
    // return null_mut();
}

/**
 * xmlXPathNodeSetMergeAndClearNoDupls:
 * @set1:  the first NodeSet or NULL
 * @set2:  the second NodeSet
 *
 * Merges two nodesets, all nodes from @set2 are added to @set1.
 * Doesn't check for duplicate nodes. Clears set2.
 *
 * Returns @set1 once extended or NULL in case of error.
 *
 * Frees @set1 in case of error.
 */
unsafe extern "C" fn xml_xpath_node_set_merge_and_clear_no_dupls(
    set1: XmlNodeSetPtr,
    set2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let mut n2: XmlNodePtr;

    for i in 0..(*set2).node_nr {
        n2 = *(*set2).node_tab.add(i as usize);
        if (*set1).node_max == 0 {
            (*set1).node_tab =
                xml_malloc(XML_NODESET_DEFAULT * size_of::<XmlNodePtr>()) as *mut XmlNodePtr;
            if (*set1).node_tab.is_null() {
                xml_xpath_err_memory(null_mut(), c"merging nodeset\n".as_ptr() as _);
                // goto error;
                xml_xpath_free_node_set(set1);
                xml_xpath_node_set_clear(set2, 1);
                return null_mut();
            }
            memset(
                (*set1).node_tab as _,
                0,
                XML_NODESET_DEFAULT * size_of::<XmlNodePtr>(),
            );
            (*set1).node_max = XML_NODESET_DEFAULT as i32;
        } else if (*set1).node_nr >= (*set1).node_max {
            if (*set1).node_max >= XPATH_MAX_NODESET_LENGTH as i32 {
                xml_xpath_err_memory(null_mut(), c"merging nodeset hit limit\n".as_ptr() as _);
                // goto error;
                xml_xpath_free_node_set(set1);
                xml_xpath_node_set_clear(set2, 1);
                return null_mut();
            }
            let temp: *mut XmlNodePtr = xml_realloc(
                (*set1).node_tab as _,
                (*set1).node_max as usize * 2 * size_of::<XmlNodePtr>(),
            ) as *mut XmlNodePtr;
            if temp.is_null() {
                xml_xpath_err_memory(null_mut(), c"merging nodeset\n".as_ptr() as _);
                // goto error;
                xml_xpath_free_node_set(set1);
                xml_xpath_node_set_clear(set2, 1);
                return null_mut();
            }
            (*set1).node_tab = temp;
            (*set1).node_max *= 2;
        }
        *(*set1).node_tab.add((*set1).node_nr as usize) = n2;
        (*set1).node_nr += 1;
        *(*set2).node_tab.add(i as usize) = null_mut();
    }
    (*set2).node_nr = 0;
    set1

    // error:
    //     xmlXPathFreeNodeSet(set1);
    //     xmlXPathNodeSetClear(set2, 1);
    //     return null_mut();
}

/**
 * xmlXPathNextChildElement:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "child" direction and nodes of type element.
 * The child axis contains the children of the context node in document order.
 *
 * Returns the next element following that axis
 */
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
        /*
        	* Get the first element child.
        	*/
        match (*cur).typ {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlDocumentFragNode
            /* URGENT TODO: entify-refs as well? */
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode => {
                cur = (*cur).children;
                if !cur.is_null() {
                    if matches!((*cur).typ, XmlElementType::XmlElementNode) {
                        return cur;
                    }
                    while {
                        cur = (*cur).next;
                        !cur.is_null() && !matches!((*cur).typ, XmlElementType::XmlElementNode)
                    } {}
                    return cur;
                }
                return null_mut();
            }
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode => {
                return xml_doc_get_root_element(cur as XmlDocPtr);
            }
            _ => {
                return null_mut();
            }
        }
    }
    /*
     * Get the next sibling element node.
     */
    match (*cur).typ {
        XmlElementType::XmlElementNode
        | XmlElementType::XmlTextNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlXincludeEnd => {}
        /* case XML_DTD_NODE: */ /* URGENT TODO: DTD-node as well? */
        _ => {
            return null_mut();
        }
    }
    if !(*cur).next.is_null() {
        if matches!((*(*cur).next).typ, XmlElementType::XmlElementNode) {
            return (*cur).next;
        }
        cur = (*cur).next;
        while {
            cur = (*cur).next;
            !cur.is_null() && !matches!((*cur).typ, XmlElementType::XmlElementNode)
        } {}
        return cur;
    }
    null_mut()
}

/**
 * xmlXPathNextPrecedingInternal:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "preceding" direction
 * the preceding axis contains all nodes in the same document as the context
 * node that are before the context node in document order, excluding any
 * ancestors and excluding attribute nodes and namespace nodes; the nodes are
 * ordered in reverse document order
 * This is a faster implementation but c_internal only since it requires a
 * state kept in the parser context: (*ctxt).ancestor.
 *
 * Returns the next element following that axis
 */
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
        if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
            cur = (*cur).parent;
        } else if matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
            let ns: XmlNsPtr = cur as XmlNsPtr;

            if (*ns).next.load(Ordering::Relaxed).is_null()
                || matches!(
                    (*(*ns).next.load(Ordering::Relaxed)).typ,
                    Some(XmlElementType::XmlNamespaceDecl)
                )
            {
                return null_mut();
            }
            cur = (*ns).next.load(Ordering::Relaxed) as XmlNodePtr;
        }
        (*ctxt).ancestor = (*cur).parent;
    }
    if matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    if !(*cur).prev.is_null() && matches!((*(*cur).prev).typ, XmlElementType::XmlDtdNode) {
        cur = (*cur).prev;
    }
    while (*cur).prev.is_null() {
        cur = (*cur).parent;
        if cur.is_null() {
            return null_mut();
        }
        if cur == (*(*(*ctxt).context).doc).children {
            return null_mut();
        }
        if cur != (*ctxt).ancestor {
            return cur;
        }
        (*ctxt).ancestor = (*cur).parent;
    }
    cur = (*cur).prev;
    while !(*cur).last.is_null() {
        cur = (*cur).last;
    }
    cur
}

unsafe extern "C" fn xml_xpath_is_positional_predicate(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    max_pos: *mut c_int,
) -> c_int {
    /*
     * BIG NOTE: This is not intended for XPATH_OP_FILTER yet!
     */

    /*
     * If not -1, then ch1 will point to:
     * 1) For predicates (XPATH_OP_PREDICATE):
     *    - an inner predicate operator
     * 2) For filters (XPATH_OP_FILTER):
     *    - an inner filter operator OR
     *    - an expression selecting the node set.
     *      E.g. "key('a', 'b')" or "(//foo | //bar)".
     */
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
            XmlXPathObjectType::XpathNumber
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
            *max_pos = floatval as c_int;
            if floatval == *max_pos as f64 {
                return 1;
            }
        }
    }
    0
}

/**
 * xmlXPathNodeSetFilter:
 * @ctxt:  the XPath Parser context
 * @set: the node set to filter
 * @filterOpIndex: the index of the predicate/filter op
 * @minPos: minimum position in the filtered set (1-based)
 * @maxPos: maximum position in the filtered set (1-based)
 * @hasNsNodes: true if the node set may contain namespace nodes
 *
 * Filter a node set, keeping only nodes for which the predicate expression
 * matches. Afterwards, keep only nodes between minPos and maxPos in the
 * filtered result.
 */
unsafe extern "C" fn xml_xpath_node_set_filter(
    ctxt: XmlXPathParserContextPtr,
    set: XmlNodeSetPtr,
    filter_op_index: c_int,
    min_pos: c_int,
    max_pos: c_int,
    has_ns_nodes: c_int,
) {
    let mut i: c_int;
    let mut j: c_int;
    let mut pos: c_int;

    if set.is_null() || (*set).node_nr == 0 {
        return;
    }

    /*
     * Check if the node set contains a sufficient number of nodes for
     * the requested range.
     */
    if (*set).node_nr < min_pos {
        xml_xpath_node_set_clear(set, has_ns_nodes);
        return;
    }

    let xpctxt: XmlXPathContextPtr = (*ctxt).context;
    let oldnode: XmlNodePtr = (*xpctxt).node;
    let olddoc: XmlDocPtr = (*xpctxt).doc;
    let oldcs: c_int = (*xpctxt).context_size;
    let oldpp: c_int = (*xpctxt).proximity_position;
    let filter_op: XmlXPathStepOpPtr = (*(*ctxt).comp).steps.add(filter_op_index as usize);

    (*xpctxt).context_size = (*set).node_nr;

    i = 0;
    j = 0;
    pos = 1;
    while i < (*set).node_nr {
        let node: XmlNodePtr = *(*set).node_tab.add(i as usize);

        (*xpctxt).node = node;
        (*xpctxt).proximity_position = i + 1;

        /*
        	* Also set the xpath document in case things like
        	* key() are evaluated in the predicate.
        	*
        	* TODO: Get real doc for namespace nodes.
        	*/
        if !matches!((*node).typ, XmlElementType::XmlNamespaceDecl) && !(*node).doc.is_null() {
            (*xpctxt).doc = (*node).doc;
        }

        let res: c_int = xml_xpath_comp_op_eval_to_boolean(ctxt, filter_op, 1);

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
                *(*set).node_tab.add(j as usize) = node;
                *(*set).node_tab.add(i as usize) = null_mut();
            }

            j += 1;
        } else {
            /* Remove the entry from the initial node set. */
            *(*set).node_tab.add(i as usize) = null_mut();
            if matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
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

    /* Free remaining nodes. */
    if has_ns_nodes != 0 {
        while i < (*set).node_nr {
            let node: XmlNodePtr = *(*set).node_tab.add(i as usize);
            if !node.is_null() && matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
                xml_xpath_node_set_free_ns(node as XmlNsPtr);
            }
            i += 1;
        }
    }

    (*set).node_nr = j;

    /* If too many elements were removed, shrink table to preserve memory. */
    if (*set).node_max > XML_NODESET_DEFAULT as i32 && (*set).node_nr < (*set).node_max / 2 {
        let mut node_max: c_int = (*set).node_nr;

        if node_max < XML_NODESET_DEFAULT as i32 {
            node_max = XML_NODESET_DEFAULT as i32;
        }
        let tmp: *mut XmlNodePtr = xml_realloc(
            (*set).node_tab as _,
            node_max as usize * size_of::<XmlNodePtr>(),
        ) as *mut XmlNodePtr;
        if tmp.is_null() {
            xml_xpath_perr_memory(ctxt, c"shrinking nodeset\n".as_ptr());
        } else {
            (*set).node_tab = tmp;
            (*set).node_max = node_max;
        }
    }

    (*xpctxt).node = oldnode;
    (*xpctxt).doc = olddoc;
    (*xpctxt).context_size = oldcs;
    (*xpctxt).proximity_position = oldpp;
}

/**
 * xmlXPathCompOpEvalPredicate:
 * @ctxt:  the XPath Parser context
 * @op: the predicate op
 * @set: the node set to filter
 * @minPos: minimum position in the filtered set (1-based)
 * @maxPos: maximum position in the filtered set (1-based)
 * @hasNsNodes: true if the node set may contain namespace nodes
 *
 * Filter a node set, keeping only nodes for which the sequence of predicate
 * expressions matches. Afterwards, keep only nodes between minPos and maxPos
 * in the filtered result.
 */
unsafe extern "C" fn xml_xpath_comp_op_eval_predicate(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    set: XmlNodeSetPtr,
    min_pos: c_int,
    max_pos: c_int,
    has_ns_nodes: c_int,
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
            (*set).node_nr,
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
    to_bool: c_int,
) -> c_int {
    let axis: XmlXPathAxisVal = (*op).value.try_into().unwrap();
    let test: XmlXPathTestVal = (*op).value2.try_into().unwrap();
    let typ: XmlXPathTypeVal = (*op).value3.try_into().unwrap();
    let prefix: *const XmlChar = (*op).value4 as _;
    let name: *const XmlChar = (*op).value5 as _;
    let mut uri: *const XmlChar = null();

    // #ifdef DEBUG_STEP
    //     let nbMatches: c_int = 0, prevMatches = 0;
    // #endif
    let mut total: c_int = 0;
    let mut has_ns_nodes: c_int;

    let mut context_idx: c_int;
    /* The final resulting node set wrt to all context nodes */
    let mut out_seq: XmlNodeSetPtr;
    /*
     * The temporary resulting node set wrt 1 context node.
     * Used to feed predicate evaluation.
     */
    let mut seq: XmlNodeSetPtr;
    let mut cur: XmlNodePtr;
    /* First predicate operator */
    let mut pred_op: XmlXPathStepOpPtr;
    let mut max_pos: c_int; /* The requested position() (when a "[n]" predicate) */
    let mut has_predicate_range: c_int;
    let mut has_axis_range: c_int;
    let mut pos: c_int;

    let next: Option<XmlXPathTraversalFunction>;
    let mut merge_and_clear: XmlXPathNodeSetMergeFunction;
    let xpctxt: XmlXPathContextPtr = (*ctxt).context;

    CHECK_TYPE0!(ctxt, XmlXPathObjectType::XpathNodeset);
    /* The popped object holding the context nodes */
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    /*
     * Setup namespaces.
     */
    if !prefix.is_null() {
        uri = xml_xpath_ns_lookup(xpctxt, prefix);
        if uri.is_null() {
            xml_xpath_release_object(xpctxt, obj);
            XP_ERROR0!(ctxt, XmlXPathError::XpathUndefPrefixError as i32);
        }
    }
    /*
     * Setup axis.
     *
     * MAYBE FUTURE TODO: merging optimizations:
     * - If the nodes to be traversed wrt to the initial nodes and
     *   the current axis cannot overlap, then we could avoid searching
     *   for duplicates during the merge.
     *   But the question is how/when to evaluate if they cannot overlap.
     *   Example: if we know that for two initial nodes, the one is
     *   not in the ancestor-or-self axis of the other, then we could safely
     *   avoid a duplicate-aware merge, if the axis to be traversed is e.g.
     *   the descendant-or-self axis.
     */
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

    // #ifdef DEBUG_STEP
    //     xmlXPathDebugDumpStepAxis(op,
    // 	((*obj).nodesetval != NULL) ? (*(*obj).nodesetval).nodeNr : 0);
    // #endif

    let Some(next) = next else {
        xml_xpath_release_object(xpctxt, obj);
        return 0;
    };
    /* The set of context nodes for the node tests */
    let context_seq: XmlNodeSetPtr = (*obj).nodesetval;
    if context_seq.is_null() || (*context_seq).node_nr <= 0 {
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
        /*
        	* There's at least one predicate. 16 == XPATH_OP_PREDICATE
        	*/
        pred_op = (*(*ctxt).comp).steps.add((*op).ch2 as usize);
        if xml_xpath_is_positional_predicate(ctxt, pred_op, addr_of_mut!(max_pos)) != 0 {
            if (*pred_op).ch1 != -1 {
                /*
                	* Use the next inner predicate operator.
                	*/
                pred_op = (*(*ctxt).comp).steps.add((*pred_op).ch1 as usize);
                has_predicate_range = 1;
            } else {
                /*
                	* There's no other predicate than the [n] predicate.
                	*/
                pred_op = null_mut();
                has_axis_range = 1;
            }
        }
    }
    let break_on_first_hit: c_int = (to_bool != 0 && pred_op.is_null()) as i32;
    /*
     * Axis traversal -----------------------------------------------------
     */
    /*
     * 2.3 Node Tests
     *  - For the attribute axis, the principal node type is attribute.
     *  - For the namespace axis, the principal node type is namespace.
     *  - For other axes, the principal node type is element.
     *
     * A node test * is true for any node of the
     * principal node type. For example, child::* will
     * select all element children of the context node
     */
    let old_context_node: XmlNodePtr = (*xpctxt).node;
    let add_node: unsafe extern "C" fn(XmlNodeSetPtr, XmlNodePtr) -> c_int =
        xml_xpath_node_set_add_unique;
    out_seq = null_mut();
    seq = null_mut();
    let context_node: XmlNodePtr = null_mut();
    context_idx = 0;

    'main: while (context_idx < (*context_seq).node_nr || !context_node.is_null())
        && (*ctxt).error == XmlXPathError::XpathExpressionOk as i32
    {
        (*xpctxt).node = *(*context_seq).node_tab.add(context_idx as usize);
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
        /*
        	* Traverse the axis and test the nodes.
        	*/
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
            /*
            	* QUESTION TODO: What does the "first" and "last" stuff do?
            	*/
            if !first.is_null() && !(*first).is_null() {
                if *first == cur {
                    break;
                }
                if total % 256 == 0 && xml_xpath_cmp_nodes_ext(*first, cur) >= 0 {
                    break;
                }
            }
            if !last.is_null() && !(*last).is_null() {
                if *last == cur {
                    break;
                }
                if total % 256 == 0 && xml_xpath_cmp_nodes_ext(cur, *last) >= 0 {
                    break;
                }
            }

            total += 1;

            // #ifdef DEBUG_STEP
            //             xmlGenericError(xmlGenericErrorContext, " %s", (*cur).name);
            // #endif

            match test {
                XmlXPathTestVal::NodeTestNone => {
                    total = 0;
                    generic_error!("Internal error at {}:{}\n", file!(), line!());
                    // goto error;
                    break 'main;
                }
                XmlXPathTestVal::NodeTestType => {
                    if matches!(typ, XmlXPathTypeVal::NodeTypeNode) {
                        match (*cur).typ {
                            XmlElementType::XmlDocumentNode
                            | XmlElementType::XmlHtmlDocumentNode
                            | XmlElementType::XmlElementNode
                            | XmlElementType::XmlAttributeNode
                            | XmlElementType::XmlPiNode
                            | XmlElementType::XmlCommentNode
                            | XmlElementType::XmlCdataSectionNode
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
                    } else if (*cur).typ as isize == typ as isize {
                        if matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
                            xp_test_hit_ns!(has_axis_range, pos, max_pos, has_ns_nodes, seq, xpctxt, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                        } else {
                            xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                        }
                    } else if matches!(typ, XmlXPathTypeVal::NodeTypeText)
                        && matches!((*cur).typ, XmlElementType::XmlCdataSectionNode)
                    {
                        xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                    }
                }
                XmlXPathTestVal::NodeTestPI => {
                    if matches!((*cur).typ, XmlElementType::XmlPiNode)
                        && (name.is_null() || xml_str_equal(name, (*cur).name))
                    {
                        xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                    }
                }
                XmlXPathTestVal::NodeTestAll => {
                    if matches!(axis, XmlXPathAxisVal::AxisAttribute) {
                        if matches!((*cur).typ, XmlElementType::XmlAttributeNode)
                            && (prefix.is_null()
                                || (!(*cur).ns.is_null()
                                    && xml_str_equal(
                                        uri,
                                        (*(*cur).ns).href.load(Ordering::Relaxed),
                                    )))
                        {
                            xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                        }
                    } else if matches!(axis, XmlXPathAxisVal::AxisNamespace) {
                        if matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
                            xp_test_hit_ns!(has_axis_range, pos, max_pos, has_ns_nodes, seq, xpctxt, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                        }
                    } else if matches!((*cur).typ, XmlElementType::XmlElementNode)
                        && (prefix.is_null()
                            || (!(*cur).ns.is_null()
                                && xml_str_equal(uri, (*(*cur).ns).href.load(Ordering::Relaxed))))
                    {
                        xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                    }
                }
                XmlXPathTestVal::NodeTestNs => {
                    // todo!();
                }
                XmlXPathTestVal::NodeTestName => 'to_break: {
                    if matches!(axis, XmlXPathAxisVal::AxisAttribute) {
                        if !matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
                            break 'to_break;
                        }
                    } else if matches!(axis, XmlXPathAxisVal::AxisNamespace) {
                        if !matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
                            break 'to_break;
                        }
                    } else if !matches!((*cur).typ, XmlElementType::XmlElementNode) {
                        break 'to_break;
                    }
                    match (*cur).typ {
                        XmlElementType::XmlElementNode => {
                            if xml_str_equal(name, (*cur).name) {
                                if prefix.is_null() {
                                    if (*cur).ns.is_null() {
                                        xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    }
                                } else if !(*cur).ns.is_null()
                                    && xml_str_equal(uri, (*(*cur).ns).href.load(Ordering::Relaxed))
                                {
                                    xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                }
                            }
                        }
                        XmlElementType::XmlAttributeNode => {
                            let attr: XmlAttrPtr = cur as XmlAttrPtr;

                            if xml_str_equal(name, (*attr).name) {
                                if prefix.is_null() {
                                    if (*attr).ns.is_null()
                                        || (*(*attr).ns).prefix.load(Ordering::Relaxed).is_null()
                                    {
                                        xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                    }
                                } else if !(*attr).ns.is_null()
                                    && xml_str_equal(
                                        uri,
                                        (*(*attr).ns).href.load(Ordering::Relaxed),
                                    )
                                {
                                    xp_test_hit!(has_axis_range, pos, max_pos, add_node, seq, cur, ctxt, out_seq, merge_and_clear, to_bool, break_on_first_hit, 'main);
                                }
                            }
                        }
                        XmlElementType::XmlNamespaceDecl => {
                            if matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
                                let ns: XmlNsPtr = cur as XmlNsPtr;

                                if !(*ns).prefix.load(Ordering::Relaxed).is_null()
                                    && !name.is_null()
                                    && xml_str_equal((*ns).prefix.load(Ordering::Relaxed), name)
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

        /*
        	* Apply predicates.
        	*/
        if !pred_op.is_null() && (*seq).node_nr > 0 {
            /*
            	* E.g. when we have a "/foo[some expression][n]".
            	*/
            /*
            	* QUESTION TODO: The old predicate evaluation took c_into
            	*  account location-sets.
            	*  (E.g. (*(*ctxt).value).typ == XPATH_LOCATIONSET)
            	*  Do we expect such a set here?
            	*  All what I learned now from the evaluation semantics
            	*  does not indicate that a location-set will be processed
            	*  here, so this looks OK.
            	*/
            /*
            	* Iterate over all predicates, starting with the outermost
            	* predicate.
            	* TODO: Problem: we cannot execute the inner predicates first
            	*  since we cannot go back *up* the operator tree!
            	*  Options we have:
            	*  1) Use of recursive functions (like is it currently done
            	*     via xmlXPathCompOpEval())
            	*  2) Add a predicate evaluation information stack to the
            	*     context struct
            	*  3) Change the way the operators are linked; we need a
            	*     "parent" field on xmlXPathStepOp
            	*
            	* For the moment, I'll try to solve this with a recursive
            	* function: xmlXPathCompOpEvalPredicate().
            	*/
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
                    (*seq).node_nr,
                    has_ns_nodes,
                );
            }

            if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
                total = 0;
                // goto error;
                break 'main;
            }
        }

        if (*seq).node_nr > 0 {
            /*
            	* Add to result set.
            	*/
            if out_seq.is_null() {
                out_seq = seq;
                seq = null_mut();
            } else {
                /* TODO: Check memory error. */
                out_seq = merge_and_clear(out_seq, seq);
            }

            if to_bool != 0 {
                break 'main;
            }
        }
        continue 'main;
    }

    // error:
    if (*obj).boolval != 0 && !(*obj).user.is_null() {
        /*
        	* QUESTION TODO: What does this do and why?
        	* TODO: Do we have to do this also for the "error"
        	* cleanup further down?
        	*/
        (*(*ctxt).value).boolval = 1;
        (*(*ctxt).value).user = (*obj).user;
        (*obj).user = null_mut();
        (*obj).boolval = 0;
    }
    xml_xpath_release_object(xpctxt, obj);

    /*
     * Ensure we return at least an empty set.
     */
    if out_seq.is_null() {
        if !seq.is_null() && (*seq).node_nr == 0 {
            out_seq = seq;
        } else {
            /* TODO: Check memory error. */
            out_seq = xml_xpath_node_set_create(null_mut());
        }
    }
    if !seq.is_null() && seq != out_seq {
        xml_xpath_free_node_set(seq);
    }
    /*
     * Hand over the result. Better to push the set also in
     * case of errors.
     */
    value_push(ctxt, xml_xpath_cache_wrap_node_set(xpctxt, out_seq));
    /*
     * Reset the context node.
     */
    (*xpctxt).node = old_context_node;
    /*
     * When traversing the namespace axis in "toBool" mode, it's
     * possible that tmpNsList wasn't freed.
     */
    if !(*xpctxt).tmp_ns_list.is_null() {
        xml_free((*xpctxt).tmp_ns_list as _);
        (*xpctxt).tmp_ns_list = null_mut();
    }

    // #ifdef DEBUG_STEP
    //     xmlGenericError(xmlGenericErrorContext,
    // 	"\nExamined %d nodes, found %d nodes at that step\n",
    // 	total, nbMatches);
    // #endif

    total
}

/*
 * Optimizer is disabled only when threaded apps are detected while
 * the library ain't compiled for thread safety.
 */
#[cfg(not(feature = "thread"))]
pub(crate) static mut XML_XPATH_DISABLE_OPTIMIZER: c_int = 0;

/**
 * xmlXPathCompSwap:
 * @comp:  the compiled expression
 * @op: operation index
 *
 * Swaps 2 operations in the compiled expression
 */
unsafe extern "C" fn xml_xpath_comp_swap(op: XmlXPathStepOpPtr) {
    /*
     * Since this manipulates possibly shared variables, this is
     * disabled if one detects that the library is used in a multithreaded
     * application
     */
    #[cfg(not(feature = "thread"))]
    if XML_XPATH_DISABLE_OPTIMIZER != 0 {
        return;
    }

    std::mem::swap(&mut (*op).ch1, &mut (*op).ch2);
}

/**
 * xmlXPathCompOpEvalLast:
 * @ctxt:  the XPath parser context with the compiled expression
 * @op:  an XPath compiled operation
 * @last:  the last elem found so far
 *
 * Evaluate the Precompiled XPath operation searching only the last
 * element in document order
 *
 * Returns the number of nodes traversed
 */
unsafe extern "C" fn xml_xpath_comp_op_eval_last(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    last: *mut XmlNodePtr,
) -> c_int {
    let mut total: c_int = 0;
    let cur: c_int;
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
                && (*(*ctxt).value).typ == XmlXPathObjectType::XpathNodeset
                && !(*(*ctxt).value).nodesetval.is_null()
                && (*(*(*ctxt).value).nodesetval).node_nr >= 1
            {
                /*
                 * limit tree traversing to first node in the result
                 */
                if (*(*(*ctxt).value).nodesetval).node_nr > 1 {
                    xml_xpath_node_set_sort((*(*ctxt).value).nodesetval);
                }
                *last = *(*(*(*ctxt).value).nodesetval)
                    .node_tab
                    .add((*(*(*ctxt).value).nodesetval).node_nr as usize - 1);
            }
            cur = xml_xpath_comp_op_eval_last(ctxt, (*comp).steps.add((*op).ch2 as usize), last);
            CHECK_ERROR0!(ctxt);
            if !(*ctxt).value.is_null()
                && matches!((*(*ctxt).value).typ, XmlXPathObjectType::XpathNodeset)
                && !(*(*ctxt).value).nodesetval.is_null()
                && (*(*(*ctxt).value).nodesetval).node_nr >= 1
            { /* TODO: NOP ? */ }

            arg2 = value_pop(ctxt);
            arg1 = value_pop(ctxt);
            if arg1.is_null()
                || (*arg1).typ != XmlXPathObjectType::XpathNodeset
                || arg2.is_null()
                || (*arg2).typ != XmlXPathObjectType::XpathNodeset
            {
                xml_xpath_release_object((*ctxt).context, arg1);
                xml_xpath_release_object((*ctxt).context, arg2);
                XP_ERROR0!(ctxt, XmlXPathError::XpathInvalidType as i32);
            }
            if (*(*ctxt).context).op_limit != 0
                && ((!(*arg1).nodesetval.is_null()
                    && xml_xpath_check_op_limit(ctxt, (*(*arg1).nodesetval).node_nr as _) < 0)
                    || (!(*arg2).nodesetval.is_null()
                        && xml_xpath_check_op_limit(ctxt, (*(*arg2).nodesetval).node_nr as _) < 0))
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
                && (*(*ctxt).value).typ == XmlXPathObjectType::XpathNodeset
                && !(*(*ctxt).value).nodesetval.is_null()
                && (*(*(*ctxt).value).nodesetval).node_nr > 1
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

/**
 * xmlXPathNodeSetKeepLast:
 * @set: the node set to be cleared
 *
 * Move the last node to the first position and clear temporary XPath objects
 * (e.g. namespace nodes) from all other nodes. Sets the length of the list
 * to 1.
 */
unsafe extern "C" fn xml_xpath_node_set_keep_last(set: XmlNodeSetPtr) {
    let mut node: XmlNodePtr;

    if set.is_null() || (*set).node_nr <= 1 {
        return;
    }

    for i in 0..(*set).node_nr - 1 {
        node = *(*set).node_tab.add(i as usize);
        if !node.is_null() && matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
            xml_xpath_node_set_free_ns(node as XmlNsPtr);
        }
    }
    *(*set).node_tab.add(0) = *(*set).node_tab.add((*set).node_nr as usize - 1);
    (*set).node_nr = 1;
}

/**
 * xmlXPathLocationSetFilter:
 * @ctxt:  the XPath Parser context
 * @locset: the location set to filter
 * @filterOpIndex: the index of the predicate/filter op
 * @minPos: minimum position in the filtered set (1-based)
 * @maxPos: maximum position in the filtered set (1-based)
 *
 * Filter a location set, keeping only nodes for which the predicate
 * expression matches. Afterwards, keep only nodes between minPos and maxPos
 * in the filtered result.
 */
#[cfg(feature = "libxml_xptr_locs")]
unsafe extern "C" fn xml_xpath_location_set_filter(
    ctxt: XmlXPathParserContextPtr,
    locset: XmlLocationSetPtr,
    filter_op_index: c_int,
    min_pos: c_int,
    max_pos: c_int,
) {
    let mut i: c_int;
    let mut j: c_int;
    let mut pos: c_int;

    if locset.is_null() || (*locset).loc_nr == 0 || filter_op_index == -1 {
        return;
    }

    let xpctxt: XmlXPathContextPtr = (*ctxt).context;
    let oldnode: XmlNodePtr = (*xpctxt).node;
    let olddoc: XmlDocPtr = (*xpctxt).doc;
    let oldcs: c_int = (*xpctxt).context_size;
    let oldpp: c_int = (*xpctxt).proximity_position;
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

        let res: c_int = xml_xpath_comp_op_eval_to_boolean(ctxt, filter_op, 1);

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
        let mut loc_max: c_int = (*locset).loc_nr;

        if loc_max < XML_NODESET_DEFAULT as i32 {
            loc_max = XML_NODESET_DEFAULT as i32;
        }
        let tmp: *mut XmlXPathObjectPtr = xml_realloc(
            (*locset).loc_tab as _,
            loc_max as usize * size_of::<XmlXPathObjectPtr>(),
        ) as *mut XmlXPathObjectPtr;
        if tmp.is_null() {
            xml_xpath_perr_memory(ctxt, c"shrinking locset\n".as_ptr());
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
) -> c_int {
    let mut total: c_int = 0;

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
        let f: c_int = (*(*comp).steps.add((*op).ch2 as usize)).ch1;

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
            /*
             * The nodeset should be in document order,
             * Keep only the last value
             */
            if !(*ctxt).value.is_null()
                && (*(*ctxt).value).typ == XmlXPathObjectType::XpathNodeset
                && !(*(*ctxt).value).nodesetval.is_null()
                && !(*(*(*ctxt).value).nodesetval).node_tab.is_null()
                && (*(*(*ctxt).value).nodesetval).node_nr > 1
            {
                xml_xpath_node_set_keep_last((*(*ctxt).value).nodesetval);
                *first = *(*(*(*ctxt).value).nodesetval).node_tab;
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
         * Hum are we filtering the result of an XPoc_inter expression
         */
        if matches!((*(*ctxt).value).typ, XmlXPathObjectType::XpathLocationset) {
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

    /*
     * In case of errors, xmlXPathNodeSetFilter can pop additional nodes from
     * the stack. We have to temporarily remove the nodeset object from the
     * stack to avoid freeing it prematurely.
     */
    CHECK_TYPE0!(ctxt, XmlXPathObjectType::XpathNodeset);
    let obj: XmlXPathObjectPtr = value_pop(ctxt);
    let set: XmlNodeSetPtr = (*obj).nodesetval;
    if !set.is_null() {
        xml_xpath_node_set_filter(ctxt, set, (*op).ch2, 1, 1, 1);
        if (*set).node_nr > 0 {
            *first = *(*set).node_tab.add(0);
        }
    }
    value_push(ctxt, obj);

    total
}

/**
 * xmlXPathCompOpEvalFirst:
 * @ctxt:  the XPath parser context with the compiled expression
 * @op:  an XPath compiled operation
 * @first:  the first elem found so far
 *
 * Evaluate the Precompiled XPath operation searching only the first
 * element in document order
 *
 * Returns the number of examined objects.
 */
unsafe extern "C" fn xml_xpath_comp_op_eval_first(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
    first: *mut XmlNodePtr,
) -> c_int {
    let mut total: c_int = 0;
    let cur: c_int;

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
                && (*(*ctxt).value).typ == XmlXPathObjectType::XpathNodeset
                && !(*(*ctxt).value).nodesetval.is_null()
                && (*(*(*ctxt).value).nodesetval).node_nr >= 1
            {
                /*
                 * limit tree traversing to first node in the result
                 */
                /*
                	* OPTIMIZE TODO: This implicitly sorts
                	*  the result, even if not needed. E.g. if the argument
                	*  of the count() function, no sorting is needed.
                	* OPTIMIZE TODO: How do we know if the node-list wasn't
                	*  already sorted?
                	*/
                if (*(*(*ctxt).value).nodesetval).node_nr > 1 {
                    xml_xpath_node_set_sort((*(*ctxt).value).nodesetval);
                }
                *first = *(*(*(*ctxt).value).nodesetval).node_tab.add(0);
            }
            cur = xml_xpath_comp_op_eval_first(ctxt, (*comp).steps.add((*op).ch2 as usize), first);
            CHECK_ERROR0!(ctxt);

            arg2 = value_pop(ctxt);
            arg1 = value_pop(ctxt);
            if arg1.is_null()
                || (*arg1).typ != XmlXPathObjectType::XpathNodeset
                || arg2.is_null()
                || (*arg2).typ != XmlXPathObjectType::XpathNodeset
            {
                xml_xpath_release_object((*ctxt).context, arg1);
                xml_xpath_release_object((*ctxt).context, arg2);
                XP_ERROR0!(ctxt, XmlXPathError::XpathInvalidType as i32);
            }
            if (*(*ctxt).context).op_limit != 0
                && ((!(*arg1).nodesetval.is_null()
                    && xml_xpath_check_op_limit(ctxt, (*(*arg1).nodesetval).node_nr as _) < 0)
                    || (!(*arg2).nodesetval.is_null()
                        && xml_xpath_check_op_limit(ctxt, (*(*arg2).nodesetval).node_nr as _) < 0))
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
                && (*(*ctxt).value).typ == XmlXPathObjectType::XpathNodeset
                && !(*(*ctxt).value).nodesetval.is_null()
                && (*(*(*ctxt).value).nodesetval).node_nr > 1
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

/**
 * xmlXPathCompOpEval:
 * @ctxt:  the XPath parser context with the compiled expression
 * @op:  an XPath compiled operation
 *
 * Evaluate the Precompiled XPath operation
 * Returns the number of nodes traversed
 */
unsafe extern "C" fn xml_xpath_comp_op_eval(
    ctxt: XmlXPathParserContextPtr,
    op: XmlXPathStepOpPtr,
) -> c_int {
    let mut total: c_int = 0;
    let equal: c_int;
    let ret: c_int;
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
            if (*ctxt).value.is_null() || (*(*ctxt).value).boolval == 0 {
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
            if (*ctxt).value.is_null() || (*(*ctxt).value).boolval == 1 {
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
                CHECK_TYPE0!(ctxt, XmlXPathObjectType::XpathNumber);
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
                || (*arg1).typ != XmlXPathObjectType::XpathNodeset
                || arg2.is_null()
                || (*arg2).typ != XmlXPathObjectType::XpathNodeset
            {
                xml_xpath_release_object((*ctxt).context, arg1);
                xml_xpath_release_object((*ctxt).context, arg2);
                XP_ERROR0!(ctxt, XmlXPathError::XpathInvalidType as i32);
            }
            if (*(*ctxt).context).op_limit != 0
                && ((!(*arg1).nodesetval.is_null()
                    && xml_xpath_check_op_limit(ctxt, (*(*arg1).nodesetval).node_nr as _) < 0)
                    || (!(*arg2).nodesetval.is_null()
                        && xml_xpath_check_op_limit(ctxt, (*(*arg2).nodesetval).node_nr as _) < 0))
            {
                xml_xpath_release_object((*ctxt).context, arg1);
                xml_xpath_release_object((*ctxt).context, arg2);
                break 'to_break;
            }
            if (*arg1).nodesetval.is_null()
                || (!(*arg2).nodesetval.is_null() && (*(*arg2).nodesetval).node_nr != 0)
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

            let frame: c_int = (*ctxt).value_nr;
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
                    && (*val).typ == XmlXPathObjectType::XpathNumber
                    && (*val).floatval == 1.0
                {
                    let mut first: XmlNodePtr = null_mut();

                    total += xml_xpath_comp_op_eval_first(
                        ctxt,
                        (*comp).steps.add((*op).ch1 as usize),
                        addr_of_mut!(first),
                    );
                    CHECK_ERROR0!(ctxt);
                    /*
                     * The nodeset should be in document order,
                     * Keep only the first value
                     */
                    if !(*ctxt).value.is_null()
                        && (*(*ctxt).value).typ == XmlXPathObjectType::XpathNodeset
                        && !(*(*ctxt).value).nodesetval.is_null()
                        && (*(*(*ctxt).value).nodesetval).node_nr > 1
                    {
                        xml_xpath_node_set_clear_from_pos((*(*ctxt).value).nodesetval, 1, 1);
                    }
                    break 'to_break;
                }
            }
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
                let f: c_int = (*(*comp).steps.add((*op).ch2 as usize)).ch1;

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
                    /*
                     * The nodeset should be in document order,
                     * Keep only the last value
                     */
                    if !(*ctxt).value.is_null()
                        && (*(*ctxt).value).typ == XmlXPathObjectType::XpathNodeset
                        && !(*(*ctxt).value).nodesetval.is_null()
                        && !(*(*(*ctxt).value).nodesetval).node_tab.is_null()
                        && (*(*(*ctxt).value).nodesetval).node_nr > 1
                    {
                        xml_xpath_node_set_keep_last((*(*ctxt).value).nodesetval);
                    }
                    break 'to_break;
                }
            }
            /*
             * Process inner predicates first.
             * Example "index[parent::book][1]":
             * ...
             *   PREDICATE   <-=1 we are here "[1]"
             *     PREDICATE <-=1 process "[parent::book]" first
             *       SORT
             *         COLLECT  'parent' 'name' 'node' book
             *           NODE
             *     ELEM Object is a number : 1
             */
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

            /*
             * Hum are we filtering the result of an XPoc_inter expression
             */
            #[cfg(feature = "libxml_xptr_locs")]
            if (*(*ctxt).value).typ == XmlXPathObjectType::XpathLocationset {
                let locset: XmlLocationSetPtr = (*(*ctxt).value).user as _;
                xml_xpath_location_set_filter(ctxt, locset, (*op).ch2, 1, (*locset).loc_nr);
                break 'to_break;
            }

            /*
             * In xmlXPathOp::of errors, xmlXPathNodeSetFilter can pop additional
             * nodes from the stack. We have to temporarily remove the
             * nodeset object from the stack to avoid freeing it
             * prematurely.
             */
            CHECK_TYPE0!(ctxt, XmlXPathObjectType::XpathNodeset);
            let obj: XmlXPathObjectPtr = value_pop(ctxt);
            let set: XmlNodeSetPtr = (*obj).nodesetval;
            if !set.is_null() {
                xml_xpath_node_set_filter(ctxt, set, (*op).ch2, 1, (*set).node_nr, 1);
            }
            value_push(ctxt, obj);
        }
        XmlXPathOp::XpathOpSort => {
            if (*op).ch1 != -1 {
                total += xml_xpath_comp_op_eval(ctxt, (*comp).steps.add((*op).ch1 as usize));
            }
            CHECK_ERROR0!(ctxt);
            if !(*ctxt).value.is_null()
                && (*(*ctxt).value).typ == XmlXPathObjectType::XpathNodeset
                && !(*(*ctxt).value).nodesetval.is_null()
                && (*(*(*ctxt).value).nodesetval).node_nr > 1
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
            let oldcs: c_int = (*(*ctxt).context).context_size;
            let oldpp: c_int = (*(*ctxt).context).proximity_position;

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
                if (*(*ctxt).value).typ == XmlXPathObjectType::XpathLocationset {
                    /*
                     * Extract the old locset, and then evaluate the result of the
                     * expression for all the element in the locset. use it to grow
                     * up a new locset.
                     */
                    CHECK_TYPE0!(ctxt, XmlXPathObjectType::XpathLocationset);

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
                        if (*res).typ == XmlXPathObjectType::XpathLocationset {
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
                    CHECK_TYPE0!(ctxt, XmlXPathObjectType::XpathNodeset);
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

/**
 * xmlXPathCompOpEvalToBoolean:
 * @ctxt:  the XPath parser context
 *
 * Evaluates if the expression evaluates to true.
 *
 * Returns 1 if true, 0 if false and -1 on API or c_internal errors.
 */
unsafe extern "C" fn xml_xpath_comp_op_eval_to_boolean(
    ctxt: XmlXPathParserContextPtr,
    mut op: XmlXPathStepOpPtr,
    is_predicate: c_int,
) -> c_int {
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
                return xml_xpath_cast_to_boolean(res_obj);
            }
            XmlXPathOp::XpathOpSort => {
                /*
                 * We don't need sorting for boolean results. Skip this one.
                 */
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
        let res: c_int;

        if (*res_obj).typ == XmlXPathObjectType::XpathBoolean {
            res = (*res_obj).boolval;
        } else if is_predicate != 0 {
            /*
             * For predicates a result of type "number" is handled
             * differently:
             * SPEC XPath 1.0:
             * "If the result is a number, the result will be converted
             *  to true if the number is equal to the context position
             *  and will be converted to false otherwise;"
             */
            res = xml_xpath_evaluate_predicate_result(ctxt, res_obj);
        } else {
            res = xml_xpath_cast_to_boolean(res_obj);
        }
        xml_xpath_release_object((*ctxt).context, res_obj);
        return res;
    }

    0
}

/**
 * xmlXPathRunEval:
 * @ctxt:  the XPath parser context with the compiled expression
 * @toBool:  evaluate to a boolean result
 *
 * Evaluate the Precompiled XPath expression in the given context.
 */
pub(crate) unsafe extern "C" fn xml_xpath_run_eval(
    ctxt: XmlXPathParserContextPtr,
    to_bool: c_int,
) -> c_int {
    if ctxt.is_null() || (*ctxt).comp.is_null() {
        return -1;
    }

    if (*ctxt).value_tab.is_null() {
        /* Allocate the value stack */
        (*ctxt).value_tab =
            xml_malloc(10 * size_of::<XmlXPathObjectPtr>()) as *mut XmlXPathObjectPtr;
        if (*ctxt).value_tab.is_null() {
            xml_xpath_perr_memory(ctxt, c"creating evaluation context\n".as_ptr());
            return -1;
        }
        (*ctxt).value_nr = 0;
        (*ctxt).value_max = 10;
        (*ctxt).value = null_mut();
    }
    #[cfg(feature = "libxml_pattern")]
    if !(*(*ctxt).comp).stream.is_null() {
        let res: c_int;

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
        	* if res == -1. Is this c_intended?
        	*/
    }
    let comp: XmlXPathCompExprPtr = (*ctxt).comp;
    if (*comp).last < 0 {
        generic_error!("xmlXPathRunEval: last is less than zero\n");
        return -1;
    }
    let old_depth: c_int = (*(*ctxt).context).depth;
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

/**
 * xmlXPathEvalExpr:
 * @ctxt:  the XPath Parser context
 *
 * Parse and evaluate an XPath expression in the given context,
 * then push the result on the context stack
 */
pub unsafe extern "C" fn xml_xpath_eval_expr(ctxt: XmlXPathParserContextPtr) {
    let mut old_depth: c_int = 0;

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
    qualified: c_int,
) -> *mut XmlChar {
    let mut buf: [XmlChar; XML_MAX_NAMELEN + 5] = [0; XML_MAX_NAMELEN + 5];
    let mut len: c_int = 0;
    let mut l: c_int = 0;
    let mut c: c_int;

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
            let mut max: c_int = len * 2;

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

/**
 * xmlXPathParseName:
 * @ctxt:  the XPath Parser context
 *
 * parse an XML name
 *
 * [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' |
 *                  CombiningChar | Extender
 *
 * [5] Name ::= (Letter | '_' | ':') (NameChar)*
 *
 * Returns the namespace name or NULL
 */
pub unsafe extern "C" fn xml_xpath_parse_name(ctxt: XmlXPathParserContextPtr) -> *mut XmlChar {
    let mut input: *const XmlChar;
    let ret: *mut XmlChar;
    let count: size_t;

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

/**
 * xmlXPathParseNCName:
 * @ctxt:  the XPath Parser context
 *
 * parse an XML namespace non qualified name.
 *
 * [NS 3] NCName ::= (Letter | '_') (NCNameChar)*
 *
 * [NS 4] NCNameChar ::= Letter | Digit | '.' | '-' | '_' |
 *                       CombiningChar | Extender
 *
 * Returns the namespace name or NULL
 */
pub unsafe extern "C" fn xml_xpath_parse_ncname(ctxt: XmlXPathParserContextPtr) -> *mut XmlChar {
    let mut input: *const XmlChar;
    let ret: *mut XmlChar;
    let count: c_int;

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

/*
 * Existing functions.
 */
/**
 * xmlXPathStringEvalNumber:
 * @str:  A string to scan
 *
 *  [30a]  Float  ::= Number ('e' Digits?)?
 *
 *  [30]   Number ::=   Digits ('.' Digits?)?
 *                    | '.' Digits
 *  [31]   Digits ::=   [0-9]+
 *
 * Compile a Number in the string
 * In complement of the Number expression, this function also handles
 * negative values : '-' Number.
 *
 * Returns the let value: f64.
 */
pub unsafe extern "C" fn xml_xpath_string_eval_number(str: *const XmlChar) -> f64 {
    let mut cur: *const XmlChar = str;
    let mut ret: f64;
    let mut ok: c_int = 0;
    let mut isneg: c_int = 0;
    let mut exponent: c_int = 0;
    let mut is_exponent_negative: c_int = 0;
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
        let mut v: c_int;
        let mut frac: c_int = 0;
        let mut fraction: f64 = 0.0;

        cur = cur.add(1);
        if (*cur < b'0' || *cur > b'9') && ok == 0 {
            return XML_XPATH_NAN;
        }
        while *cur == b'0' {
            frac += 1;
            cur = cur.add(1);
        }
        let max: c_int = frac + MAX_FRAC as i32;
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

/**
 * xmlXPathEvaluatePredicateResult:
 * @ctxt:  the XPath Parser context
 * @res:  the Predicate Expression evaluation result
 *
 * Evaluate a predicate result for the current node.
 * A PredicateExpr is evaluated by evaluating the Expr and converting
 * the result to a boolean. If the result is a number, the result will
 * be converted to true if the number is equal to the position of the
 * context node in the context node list (as returned by the position
 * function) and will be converted to false otherwise; if the result
 * is not a number, then the result will be converted as if by a call
 * to the boolean function.
 *
 * Returns 1 if predicate is true, 0 otherwise
 */
pub unsafe extern "C" fn xml_xpath_evaluate_predicate_result(
    ctxt: XmlXPathParserContextPtr,
    res: XmlXPathObjectPtr,
) -> c_int {
    if ctxt.is_null() || res.is_null() {
        return 0;
    }
    match (*res).typ {
        XmlXPathObjectType::XpathBoolean => {
            return (*res).boolval;
        }
        XmlXPathObjectType::XpathNumber => {
            return ((*res).floatval == (*(*ctxt).context).proximity_position as f64) as i32;
        }
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {
            if (*res).nodesetval.is_null() {
                return 0;
            }
            return ((*(*res).nodesetval).node_nr != 0) as i32;
        }
        XmlXPathObjectType::XpathString => {
            return (!(*res).stringval.is_null() && *(*res).stringval.add(0) != 0) as i32;
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XpathLocationset => {
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

/**
 * xmlXPathCacheNewCString:
 * @ctxt: the XPath context
 * @val:  the c_char * value
 *
 * This is the cached version of xmlXPathNewCString().
 * Acquire an xmlXPathObjectPtr of type string and of value @val
 *
 * Returns the created or reused object.
 */
unsafe extern "C" fn xml_xpath_cache_new_cstring(
    ctxt: XmlXPathContextPtr,
    val: *const c_char,
) -> XmlXPathObjectPtr {
    xml_xpath_cache_new_string(ctxt, val as _)
}

/**
 * xmlXPathCacheWrapString:
 * @ctxt: the XPath context
 * @val:  the xmlChar * value
 *
 * This is the cached version of xmlXPathWrapString().
 * Wraps the @val string c_into an XPath object.
 *
 * Returns the created or reused object.
 */
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
            (*ret).typ = XmlXPathObjectType::XpathString;
            (*ret).stringval = val;
            // #ifdef XP_DEBUG_OBJ_USAGE
            // 	    xmlXPathDebugObjUsageRequested(ctxt, XpathString);
            // #endif
            return ret;
        } else if !(*cache).misc_objs.is_null() && (*(*cache).misc_objs).number != 0 {
            /*
             * Fallback to misc-cache.
             */
            (*(*cache).misc_objs).number -= 1;
            let ret: XmlXPathObjectPtr = *(*(*cache).misc_objs)
                .items
                .add((*(*cache).misc_objs).number as usize)
                as XmlXPathObjectPtr;

            (*ret).typ = XmlXPathObjectType::XpathString;
            (*ret).stringval = val;
            // #ifdef XP_DEBUG_OBJ_USAGE
            // 	    xmlXPathDebugObjUsageRequested(ctxt, XpathString);
            // #endif
            return ret;
        }
    }
    xml_xpath_wrap_string(val)
}

/**
 * xmlXPathNameFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the name() XPath function
 *    string name(node-set?)
 * The name function returns a string containing a QName representing
 * the name of the node in the argument node-set that is first in document
 * order. The QName must represent the name with respect to the namespace
 * declarations in effect on the node whose name is being represented.
 * Typically, this will be the form in which the name occurred in the XML
 * source. This need not be the case if there are namespace declarations
 * in effect on the node that associate multiple prefixes with the same
 * namespace. However, an implementation may include information about
 * the original prefix in its representation of nodes; in this case, an
 * implementation can ensure that the returned string is always the same
 * as the QName used in the XML source. If the argument it omitted it
 * defaults to the context node.
 * Libxml keep the original prefix so the "real qualified name" used is
 * returned.
 */
unsafe extern "C" fn xml_xpath_name_function(ctxt: XmlXPathParserContextPtr, mut nargs: c_int) {
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
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }
    let cur: XmlXPathObjectPtr = value_pop(ctxt);

    if (*cur).nodesetval.is_null() || (*(*cur).nodesetval).node_nr == 0 {
        value_push(
            ctxt,
            xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
        );
    } else {
        let i: c_int = 0; /* Should be first in document order !!!!! */

        match (*(*(*(*cur).nodesetval).node_tab.add(i as usize))).typ {
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
                if *(*(*(*(*cur).nodesetval).node_tab.add(i as usize)))
                    .name
                    .add(0)
                    == b' '
                {
                    value_push(
                        ctxt,
                        xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
                    );
                } else if (*(*(*(*cur).nodesetval).node_tab.add(i as usize)))
                    .ns
                    .is_null()
                    || (*(*(*(*(*cur).nodesetval).node_tab.add(i as usize))).ns)
                        .prefix
                        .load(Ordering::Relaxed)
                        .is_null()
                {
                    value_push(
                        ctxt,
                        xml_xpath_cache_new_string(
                            (*ctxt).context,
                            (*(*(*(*cur).nodesetval).node_tab.add(i as usize))).name,
                        ),
                    );
                } else {
                    let mut fullname: *mut XmlChar;

                    fullname = xml_build_qname(
                        (*(*(*(*cur).nodesetval).node_tab.add(i as usize))).name,
                        (*(*(*(*(*cur).nodesetval).node_tab.add(i as usize))).ns)
                            .prefix
                            .load(Ordering::Relaxed),
                        null_mut(),
                        0,
                    );
                    if fullname == (*(*(*(*cur).nodesetval).node_tab.add(i as usize))).name as _ {
                        fullname =
                            xml_strdup((*(*(*(*cur).nodesetval).node_tab.add(i as usize))).name);
                    }
                    if fullname.is_null() {
                        xml_xpath_perr_memory(ctxt, null());
                    }
                    value_push(ctxt, xml_xpath_cache_wrap_string((*ctxt).context, fullname));
                }
            }
            _ => {
                value_push(
                    ctxt,
                    xml_xpath_cache_new_node_set(
                        (*ctxt).context,
                        *(*(*cur).nodesetval).node_tab.add(i as usize),
                    ),
                );
                xml_xpath_local_name_function(ctxt, 1);
            }
        }
    }
    xml_xpath_release_object((*ctxt).context, cur);
}

/**
 * xmlXPathEscapeUriFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the escape-uri() XPath function
 *    string escape-uri(string $str, bool $escape-reserved)
 *
 * This function applies the URI escaping rules defined in section 2 of [RFC
 * 2396] to the string supplied as $uri-part, which typically represents all
 * or part of a URI. The effect of the function is to replace any special
 * character in the string by an escape sequence of the form %xx%yy...,
 * where xxyy... is the hexadecimal representation of the octets used to
 * represent the character in UTF-8.
 *
 * The set of characters that are escaped depends on the setting of the
 * boolean argument $escape-reserved.
 *
 * If $escape-reserved is true, all characters are escaped other than lower
 * case letters a-z, upper case letters A-Z, digits 0-9, and the characters
 * referred to in [RFC 2396] as "marks": specifically, "-" | "_" | "." | "!"
 * | "~" | "*" | "'" | "(" | ")". The "%" character itself is escaped only
 * if it is not followed by two hexadecimal digits (that is, 0-9, a-f, and
 * A-F).
 *
 * If $escape-reserved is false, the behavior differs in that characters
 * referred to in [RFC 2396] as reserved characters are not escaped. These
 * characters are ";" | "/" | "?" | ":" | "@" | "&" | "=" | "+" | "$" | ",".
 *
 * [RFC 2396] does not define whether escaped URIs should use lower case or
 * upper case for hexadecimal digits. To ensure that escaped URIs can be
 * compared using string comparison functions, this function must always use
 * the upper-case letters A-F.
 *
 * Generally, $escape-reserved should be set to true when escaping a string
 * that is to form a single part of a URI, and to false when escaping an
 * entire URI or URI reference.
 *
 * In the case of non-ascii characters, the string is encoded according to
 * utf-8 and then converted according to RFC 2396.
 *
 * Examples
 *  xf:escape-uri ("gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles#ocean"), true())
 *  returns "gopher%3A%2F%2Fspinaltap.micro.umn.edu%2F00%2FWeather%2FCalifornia%2FLos%20Angeles%23ocean"
 *  xf:escape-uri ("gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles#ocean"), false())
 *  returns "gopher://spinaltap.micro.umn.edu/00/Weather/California/Los%20Angeles%23ocean"
 *
 */
unsafe extern "C" fn xml_xpath_escape_uri_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    let mut cptr: *mut XmlChar;
    let mut escape: [XmlChar; 4] = [0; 4];

    CHECK_ARITY!(ctxt, nargs, 2);

    let escape_reserved: c_int = xml_xpath_pop_boolean(ctxt);

    CAST_TO_STRING!(ctxt);
    let str: XmlXPathObjectPtr = value_pop(ctxt);

    let target: XmlBufPtr = xml_buf_create();

    escape[0] = b'%';
    escape[3] = 0;

    if !target.is_null() {
        cptr = (*str).stringval;
        while *cptr != 0 {
            if (*cptr >= b'A' && *cptr <= b'Z')
                || (*cptr >= b'a' && *cptr <= b'z')
                || (*cptr >= b'0' && *cptr <= b'9')
                || *cptr == b'-'
                || *cptr == b'_'
                || *cptr == b'.'
                || *cptr == b'!'
                || *cptr == b'~'
                || *cptr == b'*'
                || *cptr == b'\''
                || *cptr == b'('
                || *cptr == b')'
                || (*cptr == b'%'
                    && ((*cptr.add(1) >= b'A' && *cptr.add(1) <= b'F')
                        || (*cptr.add(1) >= b'a' && *cptr.add(1) <= b'f')
                        || (*cptr.add(1) >= b'0' && *cptr.add(1) <= b'9'))
                    && ((*cptr.add(2) >= b'A' && *cptr.add(2) <= b'F')
                        || (*cptr.add(2) >= b'a' && *cptr.add(2) <= b'f')
                        || (*cptr.add(2) >= b'0' && *cptr.add(2) <= b'9')))
                || (escape_reserved == 0
                    && (*cptr == b';'
                        || *cptr == b'/'
                        || *cptr == b'?'
                        || *cptr == b':'
                        || *cptr == b'@'
                        || *cptr == b'&'
                        || *cptr == b'='
                        || *cptr == b'+'
                        || *cptr == b'$'
                        || *cptr == b','))
            {
                xml_buf_add(target, cptr, 1);
            } else {
                if *cptr >> 4 < 10 {
                    escape[1] = b'0' + (*cptr >> 4);
                } else {
                    escape[1] = b'A' - 10 + (*cptr >> 4);
                }
                if *cptr & 0xF < 10 {
                    escape[2] = b'0' + (*cptr & 0xF);
                } else {
                    escape[2] = b'A' - 10 + (*cptr & 0xF);
                }

                xml_buf_add(target, &escape[0], 3);
            }
            cptr = cptr.add(1);
        }
    }
    value_push(
        ctxt,
        xml_xpath_cache_new_string((*ctxt).context, xml_buf_content(target)),
    );
    xml_buf_free(target);
    xml_xpath_release_object((*ctxt).context, str);
}

/**
 * xmlXPathRegisterAllFunctions:
 * @ctxt:  the XPath context
 *
 * Registers all default XPath functions in this context
 */
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

/**
 * xmlXPathNodeSetMerge:
 * @val1:  the first NodeSet or NULL
 * @val2:  the second NodeSet
 *
 * Merges two nodesets, all nodes from @val2 are added to @val1
 * if @val1 is NULL, a new set is created and copied from @val2
 *
 * Returns @val1 once extended or NULL in case of error.
 *
 * Frees @val1 in case of error.
 */
pub unsafe extern "C" fn xml_xpath_node_set_merge(
    mut val1: XmlNodeSetPtr,
    val2: XmlNodeSetPtr,
) -> XmlNodeSetPtr {
    let mut skip: c_int;
    let mut n1: XmlNodePtr;
    let mut n2: XmlNodePtr;

    if val2.is_null() {
        return val1;
    }
    if val1.is_null() {
        val1 = xml_xpath_node_set_create(null_mut());
        if val1.is_null() {
            return null_mut();
        }
    }

    /* @@ with_ns to check whether namespace nodes should be looked at @@ */
    let init_nr: c_int = (*val1).node_nr;

    for i in 0..(*val2).node_nr {
        n2 = *(*val2).node_tab.add(i as usize);
        /*
         * check against duplicates
         */
        skip = 0;
        for j in 0..init_nr {
            n1 = *(*val1).node_tab.add(j as usize);
            if n1 == n2
                || ((matches!((*n1).typ, XmlElementType::XmlNamespaceDecl)
                    && matches!((*n2).typ, XmlElementType::XmlNamespaceDecl))
                    && ((*(n1 as XmlNsPtr)).next.load(Ordering::Relaxed)
                        == (*(n2 as XmlNsPtr)).next.load(Ordering::Relaxed)
                        && xml_str_equal(
                            (*(n1 as XmlNsPtr)).prefix.load(Ordering::Relaxed) as _,
                            (*(n2 as XmlNsPtr)).prefix.load(Ordering::Relaxed) as _,
                        )))
            {
                skip = 1;
                break;
            }
        }
        if skip != 0 {
            continue;
        }

        /*
         * grow the nodeTab if needed
         */
        if (*val1).node_max == 0 {
            (*val1).node_tab =
                xml_malloc(XML_NODESET_DEFAULT * size_of::<XmlNodePtr>()) as *mut XmlNodePtr;
            if (*val1).node_tab.is_null() {
                xml_xpath_err_memory(null_mut(), c"merging nodeset\n".as_ptr() as _);
                // goto error;
                xml_xpath_free_node_set(val1);
                return null_mut();
            }
            memset(
                (*val1).node_tab as _,
                0,
                XML_NODESET_DEFAULT * size_of::<XmlNodePtr>(),
            );
            (*val1).node_max = XML_NODESET_DEFAULT as _;
        } else if (*val1).node_nr == (*val1).node_max {
            if (*val1).node_max >= XPATH_MAX_NODESET_LENGTH as i32 {
                xml_xpath_err_memory(null_mut(), c"merging nodeset hit limit\n".as_ptr() as _);
                // goto error;
                xml_xpath_free_node_set(val1);
                return null_mut();
            }
            let temp: *mut XmlNodePtr = xml_realloc(
                (*val1).node_tab as _,
                (*val1).node_max as usize * 2 * size_of::<XmlNodePtr>(),
            ) as *mut XmlNodePtr;
            if temp.is_null() {
                xml_xpath_err_memory(null_mut(), c"merging nodeset\n".as_ptr() as _);
                // goto error;
                xml_xpath_free_node_set(val1);
                return null_mut();
            }
            (*val1).node_tab = temp;
            (*val1).node_max *= 2;
        }
        if matches!((*n2).typ, XmlElementType::XmlNamespaceDecl) {
            let ns: XmlNsPtr = n2 as XmlNsPtr;
            let ns_node: XmlNodePtr =
                xml_xpath_node_set_dup_ns((*ns).next.load(Ordering::Relaxed) as XmlNodePtr, ns);

            if ns_node.is_null() {
                // goto error;
                xml_xpath_free_node_set(val1);
                return null_mut();
            }
            *(*val1).node_tab.add((*val1).node_nr as usize) = ns_node;
            (*val1).node_nr += 1;
        } else {
            *(*val1).node_tab.add((*val1).node_nr as usize) = n2;
            (*val1).node_nr += 1;
        }
    }

    val1

    // error:
    // xmlXPathFreeNodeSet(val1);
    // return null_mut();
}

/**
 * xmlXPathNodeSetDel:
 * @cur:  the initial node set
 * @val:  an xmlNodePtr
 *
 * Removes an xmlNodePtr from an existing NodeSet
 */
pub unsafe extern "C" fn xml_xpath_node_set_del(cur: XmlNodeSetPtr, val: XmlNodePtr) {
    if cur.is_null() {
        return;
    }
    if val.is_null() {
        return;
    }

    /*
     * find node in nodeTab
     */
    let mut i = 0;
    while i < (*cur).node_nr {
        if *(*cur).node_tab.add(i as usize) == val {
            break;
        }
        i += 1;
    }

    if i >= (*cur).node_nr {
        /* not found */
        return;
    }

    if !(*(*cur).node_tab.add(i as usize)).is_null()
        && matches!(
            (*(*(*cur).node_tab.add(i as usize))).typ,
            XmlElementType::XmlNamespaceDecl
        )
    {
        xml_xpath_node_set_free_ns(*(*cur).node_tab.add(i as usize) as XmlNsPtr);
    }
    (*cur).node_nr -= 1;
    while i < (*cur).node_nr {
        *(*cur).node_tab.add(i as usize) = *(*cur).node_tab.add(i as usize + 1);
        i += 1;
    }
    *(*cur).node_tab.add((*cur).node_nr as usize) = null_mut();
}

/**
 * xmlXPathNodeSetRemove:
 * @cur:  the initial node set
 * @val:  the index to remove
 *
 * Removes an entry from an existing NodeSet list.
 */
pub unsafe extern "C" fn xml_xpath_node_set_remove(cur: XmlNodeSetPtr, val: c_int) {
    if cur.is_null() {
        return;
    }
    if val >= (*cur).node_nr {
        return;
    }
    if !(*(*cur).node_tab.add(val as usize)).is_null()
        && matches!(
            (*(*(*cur).node_tab.add(val as usize))).typ,
            XmlElementType::XmlNamespaceDecl
        )
    {
        xml_xpath_node_set_free_ns(*(*cur).node_tab.add(val as usize) as XmlNsPtr);
    }
    (*cur).node_nr -= 1;
    for val in val..(*cur).node_nr {
        *(*cur).node_tab.add(val as usize) = *(*cur).node_tab.add(val as usize + 1);
    }
    *(*cur).node_tab.add((*cur).node_nr as usize) = null_mut();
}

/**
 * xmlXPathNewNodeSetList:
 * @val:  an existing NodeSet
 *
 * Create a new xmlXPathObjectPtr of type NodeSet and initialize
 * it with the Nodeset @val
 *
 * Returns the newly created object.
 */
pub unsafe extern "C" fn xml_xpath_new_node_set_list(val: XmlNodeSetPtr) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr;

    if val.is_null() {
        ret = null_mut();
    } else if (*val).node_tab.is_null() {
        ret = xml_xpath_new_node_set(null_mut());
    } else {
        ret = xml_xpath_new_node_set(*(*val).node_tab.add(0));
        if !ret.is_null() {
            for i in 1..(*val).node_nr {
                /* TODO: Propagate memory error. */
                if xml_xpath_node_set_add_unique(
                    (*ret).nodesetval,
                    *(*val).node_tab.add(i as usize),
                ) < 0
                {
                    break;
                }
            }
        }
    }

    ret
}

/**
 * xmlXPathWrapNodeSet:
 * @val:  the NodePtr value
 *
 * Wrap the Nodeset @val in a new xmlXPathObjectPtr
 *
 * Returns the newly created object.
 *
 * In case of error the node set is destroyed and NULL is returned.
 */
pub unsafe extern "C" fn xml_xpath_wrap_node_set(val: XmlNodeSetPtr) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), c"creating node set object\n".as_ptr() as _);
        xml_xpath_free_node_set(val);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathNodeset;
    (*ret).nodesetval = val;
    // #ifdef XP_DEBUG_OBJ_USAGE
    //     xmlXPathDebugObjUsageRequested(NULL, XpathNodeset);
    // #endif
    ret
}

/**
 * xmlXPathWrapExternal:
 * @val:  the user data
 *
 * Wraps the @val data c_into an XPath object.
 *
 * Returns the newly created object.
 */
pub unsafe extern "C" fn xml_xpath_wrap_external(val: *mut c_void) -> XmlXPathObjectPtr {
    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), c"creating user object\n".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathObject>());
    (*ret).typ = XmlXPathObjectType::XpathUsers;
    (*ret).user = val;
    // #ifdef XP_DEBUG_OBJ_USAGE
    //     xmlXPathDebugObjUsageRequested(NULL, XPATH_USERS);
    // #endif
    ret
}

/**
 * xmlXPathNodeValHash:
 * @node:  a node poc_inter
 *
 * Function computing the beginning of the string value of the node,
 * used to speed up comparisons
 *
 * Returns an c_int usable as a hash
 */
unsafe extern "C" fn xml_xpath_node_val_hash(mut node: XmlNodePtr) -> c_uint {
    let mut len: c_int = 2;
    let mut string: *const XmlChar;
    let mut tmp: XmlNodePtr;
    let mut ret: c_uint = 0;

    if node.is_null() {
        return 0;
    }

    if matches!((*node).typ, XmlElementType::XmlDocumentNode) {
        tmp = xml_doc_get_root_element(node as XmlDocPtr);
        if tmp.is_null() {
            node = (*node).children;
        } else {
            node = tmp;
        }

        if node.is_null() {
            return 0;
        }
    }

    match (*node).typ {
        XmlElementType::XmlCommentNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCdataSectionNode
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
            string = (*(node as XmlNsPtr)).href.load(Ordering::Relaxed);
            if string.is_null() {
                return 0;
            }
            if *string.add(0) == 0 {
                return 0;
            }
            return *string.add(0) as u32 + ((*string.add(1) as u32) << 8);
        }
        XmlElementType::XmlAttributeNode => {
            tmp = (*(node as XmlAttrPtr)).children;
        }
        XmlElementType::XmlElementNode => {
            tmp = (*node).children;
        }
        _ => {
            return 0;
        }
    }
    while !tmp.is_null() {
        match (*tmp).typ {
            XmlElementType::XmlCdataSectionNode | XmlElementType::XmlTextNode => {
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
        /*
         * Skip to next node
         */
        if (!(*tmp).children.is_null() && !matches!((*tmp).typ, XmlElementType::XmlDtdNode))
            && !matches!((*(*tmp).children).typ, XmlElementType::XmlEntityDecl)
        {
            tmp = (*tmp).children;
            continue;
        }
        if tmp == node {
            break;
        }

        if !(*tmp).next.is_null() {
            tmp = (*tmp).next;
            continue;
        }

        loop {
            tmp = (*tmp).parent;
            if tmp.is_null() {
                break;
            }
            if tmp == node {
                tmp = null_mut();
                break;
            }
            if !(*tmp).next.is_null() {
                tmp = (*tmp).next;
                break;
            }

            if tmp.is_null() {
                break;
            }
        }
    }
    ret
}

/**
 * xmlXPathEqualNodeSets:
 * @arg1:  first nodeset object argument
 * @arg2:  second nodeset object argument
 * @neq:   flag to show whether to test '=' (0) or '!=' (1)
 *
 * Implement the equal / not equal operation on XPath nodesets:
 * @arg1 == @arg2  or  @arg1 != @arg2
 * If both objects to be compared are node-sets, then the comparison
 * will be true if and only if there is a node in the first node-set and
 * a node in the second node-set such that the result of performing the
 * comparison on the string-values of the two nodes is true.
 *
 * (needless to say, this is a costly operation)
 *
 * Returns 0 or 1 depending on the results of the test.
 */
unsafe extern "C" fn xml_xpath_equal_node_sets(
    arg1: XmlXPathObjectPtr,
    arg2: XmlXPathObjectPtr,
    neq: c_int,
) -> c_int {
    let mut ret: c_int = 0;

    if arg1.is_null()
        || !matches!(
            (*arg1).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        return 0;
    }
    if arg2.is_null()
        || !matches!(
            (*arg2).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        return 0;
    }

    let ns1: XmlNodeSetPtr = (*arg1).nodesetval;
    let ns2: XmlNodeSetPtr = (*arg2).nodesetval;

    if ns1.is_null() || (*ns1).node_nr <= 0 {
        return 0;
    }
    if ns2.is_null() || (*ns2).node_nr <= 0 {
        return 0;
    }

    /*
     * for equal, check if there is a node pertaining to both sets
     */
    if neq == 0 {
        for i in 0..(*ns1).node_nr {
            for j in 0..(*ns2).node_nr {
                if *(*ns1).node_tab.add(i as usize) == *(*ns2).node_tab.add(j as usize) {
                    return 1;
                }
            }
        }
    }

    let values1: *mut *mut XmlChar =
        xml_malloc((*ns1).node_nr as usize * size_of::<*mut XmlChar>()) as *mut *mut XmlChar;
    if values1.is_null() {
        /* TODO: Propagate memory error. */
        xml_xpath_err_memory(null_mut(), c"comparing nodesets\n".as_ptr() as _);
        return 0;
    }
    let hashs1: *mut c_uint =
        xml_malloc((*ns1).node_nr as usize * size_of::<c_uint>()) as *mut c_uint;
    if hashs1.is_null() {
        /* TODO: Propagate memory error. */
        xml_xpath_err_memory(null_mut(), c"comparing nodesets\n".as_ptr() as _);
        xml_free(values1 as _);
        return 0;
    }
    memset(
        values1 as _,
        0,
        (*ns1).node_nr as usize * size_of::<*mut XmlChar>(),
    );
    let values2: *mut *mut XmlChar =
        xml_malloc((*ns2).node_nr as usize * size_of::<*mut XmlChar>()) as *mut *mut XmlChar;
    if values2.is_null() {
        /* TODO: Propagate memory error. */
        xml_xpath_err_memory(null_mut(), c"comparing nodesets\n".as_ptr() as _);
        xml_free(hashs1 as _);
        xml_free(values1 as _);
        return 0;
    }
    let hashs2: *mut c_uint =
        xml_malloc((*ns2).node_nr as usize * size_of::<c_uint>()) as *mut c_uint;
    if hashs2.is_null() {
        /* TODO: Propagate memory error. */
        xml_xpath_err_memory(null_mut(), c"comparing nodesets\n".as_ptr() as _);
        xml_free(hashs1 as _);
        xml_free(values1 as _);
        xml_free(values2 as _);
        return 0;
    }
    memset(
        values2 as _,
        0,
        (*ns2).node_nr as usize * size_of::<*mut XmlChar>(),
    );
    for i in 0..(*ns1).node_nr {
        *hashs1.add(i as usize) = xml_xpath_node_val_hash(*(*ns1).node_tab.add(i as usize));
        for j in 0..(*ns2).node_nr {
            if i == 0 {
                *hashs2.add(j as usize) = xml_xpath_node_val_hash(*(*ns2).node_tab.add(j as usize));
            }
            if *hashs1.add(i as usize) != *hashs2.add(j as usize) {
                if neq != 0 {
                    ret = 1;
                    break;
                }
            } else {
                if (*values1.add(i as usize)).is_null() {
                    *values1.add(i as usize) =
                        xml_node_get_content(*(*ns1).node_tab.add(i as usize));
                }
                if (*values2.add(j as usize)).is_null() {
                    *values2.add(j as usize) =
                        xml_node_get_content(*(*ns2).node_tab.add(j as usize));
                }
                ret =
                    xml_str_equal(*values1.add(i as usize), *values2.add(j as usize)) as i32 ^ neq;
                if ret != 0 {
                    break;
                }
            }
        }
        if ret != 0 {
            break;
        }
    }
    for i in 0..(*ns1).node_nr {
        if !(*values1.add(i as usize)).is_null() {
            xml_free(*values1.add(i as usize) as _);
        }
    }
    for j in 0..(*ns2).node_nr {
        if !(*values2.add(j as usize)).is_null() {
            xml_free(*values2.add(j as usize) as _);
        }
    }
    xml_free(values1 as _);
    xml_free(values2 as _);
    xml_free(hashs1 as _);
    xml_free(hashs2 as _);
    ret
}

/**
 * xmlXPathEqualNodeSetFloat:
 * @arg:  the nodeset object argument
 * @f:  the float to compare to
 * @neq:  flag to show whether to compare '=' (0) or '!=' (1)
 *
 * Implement the equal operation on XPath objects content: @arg1 == @arg2
 * If one object to be compared is a node-set and the other is a number,
 * then the comparison will be true if and only if there is a node in
 * the node-set such that the result of performing the comparison on the
 * number to be compared and on the result of converting the string-value
 * of that node to a number using the number function is true.
 *
 * Returns 0 or 1 depending on the results of the test.
 */
unsafe extern "C" fn xml_xpath_equal_node_set_float(
    ctxt: XmlXPathParserContextPtr,
    arg: XmlXPathObjectPtr,
    f: f64,
    neq: c_int,
) -> c_int {
    let mut ret: c_int = 0;

    let mut str2: *mut XmlChar;
    let mut val: XmlXPathObjectPtr;
    let mut v: f64;

    if arg.is_null()
        || !matches!(
            (*arg).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        return 0;
    }

    let ns: XmlNodeSetPtr = (*arg).nodesetval;
    if !ns.is_null() {
        for i in 0..(*ns).node_nr {
            str2 = xml_xpath_cast_node_to_string(*(*ns).node_tab.add(i as usize));
            if !str2.is_null() {
                value_push(ctxt, xml_xpath_cache_new_string((*ctxt).context, str2));
                xml_free(str2 as _);
                xml_xpath_number_function(ctxt, 1);
                CHECK_ERROR0!(ctxt);
                val = value_pop(ctxt);
                v = (*val).floatval;
                xml_xpath_release_object((*ctxt).context, val);
                if xml_xpath_is_nan(v) == 0 {
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

/**
 * xmlXPathStringHash:
 * @string:  a string
 *
 * Function computing the beginning of the string value of the node,
 * used to speed up comparisons
 *
 * Returns an c_int usable as a hash
 */
unsafe extern "C" fn xml_xpath_string_hash(string: *const XmlChar) -> c_uint {
    if string.is_null() {
        return 0;
    }
    if *string.add(0) == 0 {
        return 0;
    }
    *string.add(0) as u32 + ((*string.add(1) as u32) << 8)
}

/**
 * xmlXPathEqualNodeSetString:
 * @arg:  the nodeset object argument
 * @str:  the string to compare to.
 * @neq:  flag to show whether for '=' (0) or '!=' (1)
 *
 * Implement the equal operation on XPath objects content: @arg1 == @arg2
 * If one object to be compared is a node-set and the other is a string,
 * then the comparison will be true if and only if there is a node in
 * the node-set such that the result of performing the comparison on the
 * string-value of the node and the other string is true.
 *
 * Returns 0 or 1 depending on the results of the test.
 */
unsafe extern "C" fn xml_xpath_equal_node_set_string(
    arg: XmlXPathObjectPtr,
    str: *const XmlChar,
    neq: c_int,
) -> c_int {
    let mut str2: *mut XmlChar;

    if str.is_null()
        || arg.is_null()
        || !matches!(
            (*arg).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        return 0;
    }
    let ns: XmlNodeSetPtr = (*arg).nodesetval;
    /*
     * A NULL nodeset compared with a string is always false
     * (since there is no node equal, and no node not equal)
     */
    if ns.is_null() || (*ns).node_nr <= 0 {
        return 0;
    }
    let hash: c_uint = xml_xpath_string_hash(str);
    for i in 0..(*ns).node_nr {
        if xml_xpath_node_val_hash(*(*ns).node_tab.add(i as usize)) == hash {
            str2 = xml_node_get_content(*(*ns).node_tab.add(i as usize));
            if !str2.is_null() && xml_str_equal(str, str2) {
                xml_free(str2 as _);
                if neq != 0 {
                    continue;
                }
                return 1;
            } else if str2.is_null() && xml_str_equal(str, c"".as_ptr() as _) {
                if neq != 0 {
                    continue;
                }
                return 1;
            } else if neq != 0 {
                if !str2.is_null() {
                    xml_free(str2 as _);
                }
                return 1;
            }
            if !str2.is_null() {
                xml_free(str2 as _);
            }
        } else if neq != 0 {
            return 1;
        }
    }
    0
}

unsafe extern "C" fn xml_xpath_equal_values_common(
    ctxt: XmlXPathParserContextPtr,
    mut arg1: XmlXPathObjectPtr,
    mut arg2: XmlXPathObjectPtr,
) -> c_int {
    let mut ret: c_int = 0;
    /*
     *At this point we are assured neither arg1 nor arg2
     *is a nodeset, so we can just pick the appropriate routine.
     */
    match (*arg1).typ {
        XmlXPathObjectType::XpathUndefined => {
            // #ifdef DEBUG_EXPR
            // 	    xmlGenericError(xmlGenericErrorContext(),
            // 		    "Equal: undefined\n");
            // #endif
        }
        XmlXPathObjectType::XpathBoolean => {
            match (*arg2).typ {
                XmlXPathObjectType::XpathUndefined => {
                    // #ifdef DEBUG_EXPR
                    // 		    xmlGenericError(xmlGenericErrorContext(),
                    // 			    "Equal: undefined\n");
                    // #endif
                }
                XmlXPathObjectType::XpathBoolean => {
                    // #ifdef DEBUG_EXPR
                    // 		    xmlGenericError(xmlGenericErrorContext(),
                    // 			    "Equal: %d boolean %d \n",
                    // 			    (*arg1).boolval, (*arg2).boolval);
                    // #endif
                    ret = ((*arg1).boolval == (*arg2).boolval) as i32;
                }
                XmlXPathObjectType::XpathNumber => {
                    ret = ((*arg1).boolval == xml_xpath_cast_number_to_boolean((*arg2).floatval))
                        as i32;
                }
                XmlXPathObjectType::XpathString => {
                    if (*arg2).stringval.is_null() || *(*arg2).stringval.add(0) == 0 {
                        ret = 0;
                    } else {
                        ret = 1;
                    }
                    ret = ((*arg1).boolval == ret) as i32;
                }
                XmlXPathObjectType::XpathUsers => {
                    todo!()
                }
                #[cfg(feature = "libxml_xptr_locs")]
                XmlXPathObjectType::XpathPoint
                | XmlXPathObjectType::XpathRange
                | XmlXPathObjectType::XpathLocationset => {
                    todo!()
                }
                XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {}
            }
        }
        XmlXPathObjectType::XpathNumber => {
            match (*arg2).typ {
                XmlXPathObjectType::XpathUndefined => {
                    // #ifdef DEBUG_EXPR
                    // 		    xmlGenericError(xmlGenericErrorContext(),
                    // 			    "Equal: undefined\n");
                    // #endif
                }
                XmlXPathObjectType::XpathBoolean => {
                    ret = ((*arg2).boolval == xml_xpath_cast_number_to_boolean((*arg1).floatval))
                        as i32;
                }

                ty @ XmlXPathObjectType::XpathString
                | ty @ XmlXPathObjectType::XpathNumber => 'to_break: {
                    if matches!(ty, XmlXPathObjectType::XpathString) {
                        value_push(ctxt, arg2);
                        xml_xpath_number_function(ctxt, 1);
                        arg2 = value_pop(ctxt);
                        if (*ctxt).error != 0 {
                            break 'to_break;
                        }
                        /* Falls through. */
                    }

                    /* Hand check NaN and Infinity equalities */
                    if xml_xpath_is_nan((*arg1).floatval) != 0
                        || xml_xpath_is_nan((*arg2).floatval) != 0
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
                XmlXPathObjectType::XpathUsers => {
                    todo!()
                }
                #[cfg(feature = "libxml_xptr_locs")]
                XmlXPathObjectType::XpathPoint
                | XmlXPathObjectType::XpathRange
                | XmlXPathObjectType::XpathLocationset => {
                    todo!()
                }
                XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {}
            }
        }
        XmlXPathObjectType::XpathString => {
            match (*arg2).typ {
                XmlXPathObjectType::XpathUndefined => {
                    // #ifdef DEBUG_EXPR
                    // 		    xmlGenericError(xmlGenericErrorContext(),
                    // 			    "Equal: undefined\n");
                    // #endif
                }
                XmlXPathObjectType::XpathBoolean => {
                    if (*arg1).stringval.is_null() || *(*arg1).stringval.add(0) == 0 {
                        ret = 0;
                    } else {
                        ret = 1;
                    }
                    ret = ((*arg2).boolval == ret) as i32;
                }
                XmlXPathObjectType::XpathString => {
                    ret = xml_str_equal((*arg1).stringval, (*arg2).stringval) as i32;
                }
                XmlXPathObjectType::XpathNumber => {
                    value_push(ctxt, arg1);
                    xml_xpath_number_function(ctxt, 1);
                    arg1 = value_pop(ctxt);
                    if (*ctxt).error != 0 {
                        // break;
                    } else {
                        /* Hand check NaN and Infinity equalities */
                        if xml_xpath_is_nan((*arg1).floatval) != 0
                            || xml_xpath_is_nan((*arg2).floatval) != 0
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
                XmlXPathObjectType::XpathUsers => {
                    todo!()
                }
                #[cfg(feature = "libxml_xptr_locs")]
                XmlXPathObjectType::XpathPoint
                | XmlXPathObjectType::XpathRange
                | XmlXPathObjectType::XpathLocationset => {
                    todo!()
                }
                XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {}
            }
        }
        XmlXPathObjectType::XpathUsers => {
            todo!()
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XpathPoint
        | XmlXPathObjectType::XpathRange
        | XmlXPathObjectType::XpathLocationset => {
            todo!()
        }
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {}
    }
    xml_xpath_release_object((*ctxt).context, arg1);
    xml_xpath_release_object((*ctxt).context, arg2);
    ret
}

/**
 * xmlXPathEqualValues:
 * @ctxt:  the XPath Parser context
 *
 * Implement the equal operation on XPath objects content: @arg1 == @arg2
 *
 * Returns 0 or 1 depending on the results of the test.
 */
pub unsafe extern "C" fn xml_xpath_equal_values(ctxt: XmlXPathParserContextPtr) -> c_int {
    let mut arg1: XmlXPathObjectPtr;
    let mut arg2: XmlXPathObjectPtr;
    let argtmp: XmlXPathObjectPtr;
    let mut ret: c_int = 0;

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
        // #ifdef DEBUG_EXPR
        //         xmlGenericError(xmlGenericErrorContext,
        // 		"Equal: by poc_inter\n");
        // #endif
        xml_xpath_free_object(arg1);
        return 1;
    }

    /*
     *If either argument is a nodeset, it's a 'special case'
     */
    if matches!(
        (*arg2).typ,
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
    ) || matches!(
        (*arg1).typ,
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
    ) {
        /*
         *Hack it to assure arg1 is the nodeset
         */
        if !matches!(
            (*arg1).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        ) {
            argtmp = arg2;
            arg2 = arg1;
            arg1 = argtmp;
        }
        match (*arg2).typ {
            XmlXPathObjectType::XpathUndefined => {
                // #ifdef DEBUG_EXPR
                // 		xmlGenericError(xmlGenericErrorContext(),
                // 			"Equal: undefined\n");
                // #endif
            }
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {
                ret = xml_xpath_equal_node_sets(arg1, arg2, 0);
            }
            XmlXPathObjectType::XpathBoolean => {
                if (*arg1).nodesetval.is_null() || (*(*arg1).nodesetval).node_nr == 0 {
                    ret = 0;
                } else {
                    ret = 1;
                }
                ret = (ret == (*arg2).boolval) as i32;
            }
            XmlXPathObjectType::XpathNumber => {
                ret = xml_xpath_equal_node_set_float(ctxt, arg1, (*arg2).floatval, 0);
            }
            XmlXPathObjectType::XpathString => {
                ret = xml_xpath_equal_node_set_string(arg1, (*arg2).stringval, 0);
            }
            XmlXPathObjectType::XpathUsers => {
                todo!()
            }
            #[cfg(feature = "libxml_xptr_locs")]
            XmlXPathObjectType::XpathPoint
            | XmlXPathObjectType::XpathRange
            | XmlXPathObjectType::XpathLocationset => todo!(),
            // _ => {}
        }
        xml_xpath_release_object((*ctxt).context, arg1);
        xml_xpath_release_object((*ctxt).context, arg2);
        return ret;
    }

    xml_xpath_equal_values_common(ctxt, arg1, arg2)
}

/**
 * xmlXPathNotEqualValues:
 * @ctxt:  the XPath Parser context
 *
 * Implement the equal operation on XPath objects content: @arg1 == @arg2
 *
 * Returns 0 or 1 depending on the results of the test.
 */
pub unsafe extern "C" fn xml_xpath_not_equal_values(ctxt: XmlXPathParserContextPtr) -> c_int {
    let mut arg1: XmlXPathObjectPtr;
    let mut arg2: XmlXPathObjectPtr;
    let argtmp: XmlXPathObjectPtr;
    let mut ret: c_int = 0;

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
        // #ifdef DEBUG_EXPR
        //         xmlGenericError(xmlGenericErrorContext,
        // 		"NotEqual: by poc_inter\n");
        // #endif
        xml_xpath_release_object((*ctxt).context, arg1);
        return 0;
    }

    /*
     *If either argument is a nodeset, it's a 'special case'
     */
    if matches!(
        (*arg2).typ,
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
    ) || matches!(
        (*arg1).typ,
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
    ) {
        /*
         *Hack it to assure arg1 is the nodeset
         */
        if !matches!(
            (*arg1).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        ) {
            argtmp = arg2;
            arg2 = arg1;
            arg1 = argtmp;
        }
        match (*arg2).typ {
            XmlXPathObjectType::XpathUndefined => {
                // #ifdef DEBUG_EXPR
                // 		xmlGenericError(xmlGenericErrorContext(),
                // 			"NotEqual: undefined\n");
                // #endif
            }
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {
                ret = xml_xpath_equal_node_sets(arg1, arg2, 1);
            }
            XmlXPathObjectType::XpathBoolean => {
                if (*arg1).nodesetval.is_null() || (*(*arg1).nodesetval).node_nr == 0 {
                    ret = 0;
                } else {
                    ret = 1;
                }
                ret = (ret != (*arg2).boolval) as i32;
            }
            XmlXPathObjectType::XpathNumber => {
                ret = xml_xpath_equal_node_set_float(ctxt, arg1, (*arg2).floatval, 1);
            }
            XmlXPathObjectType::XpathString => {
                ret = xml_xpath_equal_node_set_string(arg1, (*arg2).stringval, 1);
            }
            XmlXPathObjectType::XpathUsers => {
                todo!()
            }
            #[cfg(feature = "libxml_xptr_locs")]
            XmlXPathObjectType::XpathPoint
            | XmlXPathObjectType::XpathRange
            | XmlXPathObjectType::XpathLocationset => {
                todo!()
            } // _ => {}
        }
        xml_xpath_release_object((*ctxt).context, arg1);
        xml_xpath_release_object((*ctxt).context, arg2);
        return ret;
    }

    (xml_xpath_equal_values_common(ctxt, arg1, arg2) == 0) as i32
}

/**
 * xmlXPathCompareNodeSets:
 * @inf:  less than (1) or greater than (0)
 * @strict:  is the comparison strict
 * @arg1:  the first node set object
 * @arg2:  the second node set object
 *
 * Implement the compare operation on nodesets:
 *
 * If both objects to be compared are node-sets, then the comparison
 * will be true if and only if there is a node in the first node-set
 * and a node in the second node-set such that the result of performing
 * the comparison on the string-values of the two nodes is true.
 * ....
 * When neither object to be compared is a node-set and the operator
 * is <=, <, >= or >, then the objects are compared by converting both
 * objects to numbers and comparing the numbers according to IEEE 754.
 * ....
 * The number function converts its argument to a number as follows:
 *  - a string that consists of optional whitespace followed by an
 *    optional minus sign followed by a Number followed by whitespace
 *    is converted to the IEEE 754 number that is nearest (according
 *    to the IEEE 754 round-to-nearest rule) to the mathematical value
 *    represented by the string; any other string is converted to NaN
 *
 * Conclusion all nodes need to be converted first to their string value
 * and then the comparison must be done when possible
 */
unsafe extern "C" fn xml_xpath_compare_node_sets(
    inf: c_int,
    strict: c_int,
    arg1: XmlXPathObjectPtr,
    arg2: XmlXPathObjectPtr,
) -> c_int {
    let mut init: c_int = 0;
    let mut val1: f64;
    let mut ret: c_int = 0;

    if arg1.is_null()
        || !matches!(
            (*arg1).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        xml_xpath_free_object(arg2);
        return 0;
    }
    if arg2.is_null()
        || !matches!(
            (*arg2).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        xml_xpath_free_object(arg1);
        xml_xpath_free_object(arg2);
        return 0;
    }

    let ns1: XmlNodeSetPtr = (*arg1).nodesetval;
    let ns2: XmlNodeSetPtr = (*arg2).nodesetval;

    if ns1.is_null() || (*ns1).node_nr <= 0 {
        xml_xpath_free_object(arg1);
        xml_xpath_free_object(arg2);
        return 0;
    }
    if ns2.is_null() || (*ns2).node_nr <= 0 {
        xml_xpath_free_object(arg1);
        xml_xpath_free_object(arg2);
        return 0;
    }

    let values2: *mut f64 = xml_malloc((*ns2).node_nr as usize * size_of::<f64>()) as *mut f64;
    if values2.is_null() {
        /* TODO: Propagate memory error. */
        xml_xpath_err_memory(null_mut(), c"comparing nodesets\n".as_ptr() as _);
        xml_xpath_free_object(arg1);
        xml_xpath_free_object(arg2);
        return 0;
    }
    for i in 0..(*ns1).node_nr {
        val1 = xml_xpath_cast_node_to_number(*(*ns1).node_tab.add(i as usize));
        if xml_xpath_is_nan(val1) != 0 {
            continue;
        }
        for j in 0..(*ns2).node_nr {
            if init == 0 {
                *values2.add(j as usize) =
                    xml_xpath_cast_node_to_number(*(*ns2).node_tab.add(j as usize));
            }
            if xml_xpath_is_nan(*values2.add(j as usize)) != 0 {
                continue;
            }
            if inf != 0 && strict != 0 {
                ret = (val1 < *values2.add(j as usize)) as i32;
            } else if inf != 0 && strict == 0 {
                ret = (val1 <= *values2.add(j as usize)) as i32;
            } else if inf == 0 && strict != 0 {
                ret = (val1 > *values2.add(j as usize)) as i32;
            } else if inf == 0 && strict == 0 {
                ret = (val1 >= *values2.add(j as usize)) as i32;
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

/**
 * xmlXPathCompareNodeSetFloat:
 * @ctxt:  the XPath Parser context
 * @inf:  less than (1) or greater than (0)
 * @strict:  is the comparison strict
 * @arg:  the node set
 * @f:  the value
 *
 * Implement the compare operation between a nodeset and a number
 *     @ns < @val    (1, 1, ...
 *     @ns <= @val   (1, 0, ...
 *     @ns > @val    (0, 1, ...
 *     @ns >= @val   (0, 0, ...
 *
 * If one object to be compared is a node-set and the other is a number,
 * then the comparison will be true if and only if there is a node in the
 * node-set such that the result of performing the comparison on the number
 * to be compared and on the result of converting the string-value of that
 * node to a number using the number function is true.
 *
 * Returns 0 or 1 depending on the results of the test.
 */
unsafe extern "C" fn xml_xpath_compare_node_set_float(
    ctxt: XmlXPathParserContextPtr,
    inf: c_int,
    strict: c_int,
    arg: XmlXPathObjectPtr,
    f: XmlXPathObjectPtr,
) -> c_int {
    let mut ret: c_int = 0;
    let mut str2: *mut XmlChar;

    if f.is_null()
        || arg.is_null()
        || !matches!(
            (*arg).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        xml_xpath_release_object((*ctxt).context, arg);
        xml_xpath_release_object((*ctxt).context, f);
        return 0;
    }
    let ns: XmlNodeSetPtr = (*arg).nodesetval;
    if !ns.is_null() {
        for i in 0..(*ns).node_nr {
            str2 = xml_xpath_cast_node_to_string(*(*ns).node_tab.add(i as usize));
            if !str2.is_null() {
                value_push(ctxt, xml_xpath_cache_new_string((*ctxt).context, str2));
                xml_free(str2 as _);
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

/**
 * xmlXPathCompareNodeSetString:
 * @ctxt:  the XPath Parser context
 * @inf:  less than (1) or greater than (0)
 * @strict:  is the comparison strict
 * @arg:  the node set
 * @s:  the value
 *
 * Implement the compare operation between a nodeset and a string
 *     @ns < @val    (1, 1, ...
 *     @ns <= @val   (1, 0, ...
 *     @ns > @val    (0, 1, ...
 *     @ns >= @val   (0, 0, ...
 *
 * If one object to be compared is a node-set and the other is a string,
 * then the comparison will be true if and only if there is a node in
 * the node-set such that the result of performing the comparison on the
 * string-value of the node and the other string is true.
 *
 * Returns 0 or 1 depending on the results of the test.
 */
unsafe extern "C" fn xml_xpath_compare_node_set_string(
    ctxt: XmlXPathParserContextPtr,
    inf: c_int,
    strict: c_int,
    arg: XmlXPathObjectPtr,
    s: XmlXPathObjectPtr,
) -> c_int {
    let mut ret: c_int = 0;
    let mut str2: *mut XmlChar;

    if s.is_null()
        || arg.is_null()
        || !matches!(
            (*arg).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        xml_xpath_release_object((*ctxt).context, arg);
        xml_xpath_release_object((*ctxt).context, s);
        return 0;
    }
    let ns: XmlNodeSetPtr = (*arg).nodesetval;
    if !ns.is_null() {
        for i in 0..(*ns).node_nr {
            str2 = xml_xpath_cast_node_to_string(*(*ns).node_tab.add(i as usize));
            if !str2.is_null() {
                value_push(ctxt, xml_xpath_cache_new_string((*ctxt).context, str2));
                xml_free(str2 as _);
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

/**
 * xmlXPathCompareNodeSetValue:
 * @ctxt:  the XPath Parser context
 * @inf:  less than (1) or greater than (0)
 * @strict:  is the comparison strict
 * @arg:  the node set
 * @val:  the value
 *
 * Implement the compare operation between a nodeset and a value
 *     @ns < @val    (1, 1, ...
 *     @ns <= @val   (1, 0, ...
 *     @ns > @val    (0, 1, ...
 *     @ns >= @val   (0, 0, ...
 *
 * If one object to be compared is a node-set and the other is a boolean,
 * then the comparison will be true if and only if the result of performing
 * the comparison on the boolean and on the result of converting
 * the node-set to a boolean using the boolean function is true.
 *
 * Returns 0 or 1 depending on the results of the test.
 */
unsafe extern "C" fn xml_xpath_compare_node_set_value(
    ctxt: XmlXPathParserContextPtr,
    inf: c_int,
    strict: c_int,
    arg: XmlXPathObjectPtr,
    val: XmlXPathObjectPtr,
) -> c_int {
    if val.is_null()
        || arg.is_null()
        || !matches!(
            (*arg).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        return 0;
    }

    match (*val).typ {
        XmlXPathObjectType::XpathNumber => {
            xml_xpath_compare_node_set_float(ctxt, inf, strict, arg, val)
        }
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {
            xml_xpath_compare_node_sets(inf, strict, arg, val)
        }
        XmlXPathObjectType::XpathString => {
            xml_xpath_compare_node_set_string(ctxt, inf, strict, arg, val)
        }
        XmlXPathObjectType::XpathBoolean => {
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

/**
 * xmlXPathCompareValues:
 * @ctxt:  the XPath Parser context
 * @inf:  less than (1) or greater than (0)
 * @strict:  is the comparison strict
 *
 * Implement the compare operation on XPath objects:
 *     @arg1 < @arg2    (1, 1, ...
 *     @arg1 <= @arg2   (1, 0, ...
 *     @arg1 > @arg2    (0, 1, ...
 *     @arg1 >= @arg2   (0, 0, ...
 *
 * When neither object to be compared is a node-set and the operator is
 * <=, <, >=, >, then the objects are compared by converted both objects
 * to numbers and comparing the numbers according to IEEE 754. The <
 * comparison will be true if and only if the first number is less than the
 * second number. The <= comparison will be true if and only if the first
 * number is less than or equal to the second number. The > comparison
 * will be true if and only if the first number is greater than the second
 * number. The >= comparison will be true if and only if the first number
 * is greater than or equal to the second number.
 *
 * Returns 1 if the comparison succeeded, 0 if it failed
 */
pub unsafe extern "C" fn xml_xpath_compare_values(
    ctxt: XmlXPathParserContextPtr,
    inf: c_int,
    strict: c_int,
) -> c_int {
    let mut ret: c_int = 0;
    let arg1i: c_int;
    let arg2i: c_int;
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
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
    ) || matches!(
        (*arg1).typ,
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
    ) {
        /*
         * If either argument is a XpathNodeset or XpathXsltTree the two arguments
         * are not freed from within this routine; they will be freed from the
         * called routine, e.g. xmlXPathCompareNodeSets or xmlXPathCompareNodeSetValue
         */
        if matches!(
            (*arg2).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        ) && matches!(
            (*arg1).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        ) {
            ret = xml_xpath_compare_node_sets(inf, strict, arg1, arg2);
        } else if matches!(
            (*arg1).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        ) {
            ret = xml_xpath_compare_node_set_value(ctxt, inf, strict, arg1, arg2);
        } else {
            ret = xml_xpath_compare_node_set_value(ctxt, !inf, strict, arg2, arg1);
        }
        return ret;
    }

    if !matches!((*arg1).typ, XmlXPathObjectType::XpathNumber) {
        value_push(ctxt, arg1);
        xml_xpath_number_function(ctxt, 1);
        arg1 = value_pop(ctxt);
    }
    if !matches!((*arg2).typ, XmlXPathObjectType::XpathNumber) {
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
    /*
     * Add tests for infinity and nan
     * => feedback on 3.4 for Inf and NaN
     */
    /* Hand check NaN and Infinity comparisons */
    if xml_xpath_is_nan((*arg1).floatval) != 0 || xml_xpath_is_nan((*arg2).floatval) != 0 {
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

/**
 * xmlXPathValueFlipSign:
 * @ctxt:  the XPath Parser context
 *
 * Implement the unary - operation on an XPath object
 * The numeric operators convert their operands to numbers as if
 * by calling the number function.
 */
pub unsafe extern "C" fn xml_xpath_value_flip_sign(ctxt: XmlXPathParserContextPtr) {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return;
    }
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathNumber);
    (*(*ctxt).value).floatval = -(*(*ctxt).value).floatval;
}

/**
 * xmlXPathAddValues:
 * @ctxt:  the XPath Parser context
 *
 * Implement the add operation on XPath objects:
 * The numeric operators convert their operands to numbers as if
 * by calling the number function.
 */
pub unsafe extern "C" fn xml_xpath_add_values(ctxt: XmlXPathParserContextPtr) {
    let arg: XmlXPathObjectPtr = value_pop(ctxt);
    if arg.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    let val: f64 = xml_xpath_cast_to_number(arg);
    xml_xpath_release_object((*ctxt).context, arg);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathNumber);
    (*(*ctxt).value).floatval += val;
}

/**
 * xmlXPathSubValues:
 * @ctxt:  the XPath Parser context
 *
 * Implement the subtraction operation on XPath objects:
 * The numeric operators convert their operands to numbers as if
 * by calling the number function.
 */
pub unsafe extern "C" fn xml_xpath_sub_values(ctxt: XmlXPathParserContextPtr) {
    let arg: XmlXPathObjectPtr = value_pop(ctxt);
    if arg.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    let val: f64 = xml_xpath_cast_to_number(arg);
    xml_xpath_release_object((*ctxt).context, arg);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathNumber);
    (*(*ctxt).value).floatval -= val;
}

/**
 * xmlXPathMultValues:
 * @ctxt:  the XPath Parser context
 *
 * Implement the multiply operation on XPath objects:
 * The numeric operators convert their operands to numbers as if
 * by calling the number function.
 */
pub unsafe extern "C" fn xml_xpath_mult_values(ctxt: XmlXPathParserContextPtr) {
    let arg: XmlXPathObjectPtr = value_pop(ctxt);
    if arg.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    let val: f64 = xml_xpath_cast_to_number(arg);
    xml_xpath_release_object((*ctxt).context, arg);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathNumber);
    (*(*ctxt).value).floatval *= val;
}

/**
 * xmlXPathDivValues:
 * @ctxt:  the XPath Parser context
 *
 * Implement the div operation on XPath objects @arg1 / @arg2:
 * The numeric operators convert their operands to numbers as if
 * by calling the number function.
 */
// ATTRIBUTE_NO_SANITIZE("float-divide-by-zero")
pub unsafe extern "C" fn xml_xpath_div_values(ctxt: XmlXPathParserContextPtr) {
    let arg: XmlXPathObjectPtr = value_pop(ctxt);
    if arg.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    let val: f64 = xml_xpath_cast_to_number(arg);
    xml_xpath_release_object((*ctxt).context, arg);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathNumber);
    (*(*ctxt).value).floatval /= val;
}

/**
 * xmlXPathModValues:
 * @ctxt:  the XPath Parser context
 *
 * Implement the mod operation on XPath objects: @arg1 / @arg2
 * The numeric operators convert their operands to numbers as if
 * by calling the number function.
 */
pub unsafe extern "C" fn xml_xpath_mod_values(ctxt: XmlXPathParserContextPtr) {
    let arg: XmlXPathObjectPtr = value_pop(ctxt);
    if arg.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    let arg2: f64 = xml_xpath_cast_to_number(arg);
    xml_xpath_release_object((*ctxt).context, arg);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathNumber);
    let arg1: f64 = (*(*ctxt).value).floatval;
    if arg2 == 0.0 {
        (*(*ctxt).value).floatval = XML_XPATH_NAN;
    } else {
        (*(*ctxt).value).floatval = arg1 % arg2;
    }
}

/**
 * xmlXPathIsNodeType:
 * @name:  a name string
 *
 * Is the name given a NodeType one.
 *
 *  [38]   NodeType ::=   'comment'
 *                    | 'text'
 *                    | 'processing-instruction'
 *                    | 'node'
 *
 * Returns 1 if true 0 otherwise
 */
pub unsafe extern "C" fn xml_xpath_is_node_type(name: *const XmlChar) -> c_int {
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

/*
 * Some of the axis navigation routines.
 */
/**
 * xmlXPathNextSelf:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "self" direction
 * The self axis contains just the context node itself
 *
 * Returns the next element following that axis
 */
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

/**
 * xmlXPathNextChild:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "child" direction
 * The child axis contains the children of the context node in document order.
 *
 * Returns the next element following that axis
 */
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
        match (*(*(*ctxt).context).node).typ {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCdataSectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPiNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDtdNode => {
                return (*(*(*ctxt).context).node).children;
            }
            XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHtmlDocumentNode => {
                return (*((*(*ctxt).context).node as XmlDocPtr)).children;
            }
            XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlNamespaceDecl
            | XmlElementType::XmlXincludeStart
            | XmlElementType::XmlXincludeEnd => {
                return null_mut();
            }
            _ => unreachable!(),
        }
    }
    if matches!(
        (*cur).typ,
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode
    ) {
        return null_mut();
    }
    (*cur).next
}

/**
 * xmlXPathNextDescendant:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "descendant" direction
 * the descendant axis contains the descendants of the context node in document
 * order; a descendant is a child or a child of a child and so on.
 *
 * Returns the next element following that axis
 */
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
            (*(*(*ctxt).context).node).typ,
            XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
        ) {
            return null_mut();
        }

        if (*(*ctxt).context).node == (*(*ctxt).context).doc as XmlNodePtr {
            return (*(*(*ctxt).context).doc).children;
        }
        return (*(*(*ctxt).context).node).children;
    }

    if matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    if !(*cur).children.is_null() {
        /*
         * Do not descend on entities declarations
         */
        if !matches!((*(*cur).children).typ, XmlElementType::XmlEntityDecl) {
            cur = (*cur).children;
            /*
             * Skip DTDs
             */
            if !matches!((*cur).typ, XmlElementType::XmlDtdNode) {
                return cur;
            }
        }
    }

    if cur == (*(*ctxt).context).node {
        return null_mut();
    }

    while !(*cur).next.is_null() {
        cur = (*cur).next;
        if !matches!(
            (*cur).typ,
            XmlElementType::XmlEntityDecl | XmlElementType::XmlDtdNode
        ) {
            return cur;
        }
    }

    loop {
        cur = (*cur).parent;
        if cur.is_null() {
            break;
        }
        if cur == (*(*ctxt).context).node {
            return null_mut();
        }
        if !(*cur).next.is_null() {
            cur = (*cur).next;
            return cur;
        }
        if cur.is_null() {
            break;
        }
    }
    cur
}

/**
 * xmlXPathNextDescendantOrSelf:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "descendant-or-self" direction
 * the descendant-or-self axis contains the context node and the descendants
 * of the context node in document order; thus the context node is the first
 * node on the axis, and the first child of the context node is the second node
 * on the axis
 *
 * Returns the next element following that axis
 */
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
        (*(*(*ctxt).context).node).typ,
        XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
    ) {
        return null_mut();
    }

    xml_xpath_next_descendant(ctxt, cur)
}

/**
 * xmlXPathNextParent:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "parent" direction
 * The parent axis contains the parent of the context node, if there is one.
 *
 * Returns the next element following that axis
 */
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
        match (*(*(*ctxt).context).node).typ {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCdataSectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPiNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlDtdNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlXincludeStart
            | XmlElementType::XmlXincludeEnd
            | XmlElementType::XmlEntityDecl => {
                if (*(*(*ctxt).context).node).parent.is_null() {
                    return (*(*ctxt).context).doc as XmlNodePtr;
                }
                if matches!(
                    (*(*(*(*ctxt).context).node).parent).typ,
                    XmlElementType::XmlElementNode
                ) && (*(*(*(*(*ctxt).context).node).parent).name.add(0) == b' '
                    || xml_str_equal(
                        (*(*(*(*ctxt).context).node).parent).name,
                        c"fake node libxslt".as_ptr() as _,
                    ))
                {
                    return null_mut();
                }
                return (*(*(*ctxt).context).node).parent;
            }
            XmlElementType::XmlAttributeNode => {
                let att: XmlAttrPtr = (*(*ctxt).context).node as XmlAttrPtr;

                return (*att).parent;
            }
            XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHtmlDocumentNode => {
                return null_mut();
            }
            XmlElementType::XmlNamespaceDecl => {
                let ns: XmlNsPtr = (*(*ctxt).context).node as XmlNsPtr;

                if !(*ns).next.load(Ordering::Relaxed).is_null()
                    && !matches!(
                        (*(*ns).next.load(Ordering::Relaxed)).typ,
                        Some(XmlElementType::XmlNamespaceDecl)
                    )
                {
                    return (*ns).next.load(Ordering::Relaxed) as XmlNodePtr;
                }
                return null_mut();
            }
            _ => unreachable!(),
        }
    }
    null_mut()
}

/**
 * xmlXPathNextAncestorOrSelf:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "ancestor-or-self" direction
 * he ancestor-or-self axis contains the context node and ancestors of
 * the context node in reverse document order; thus the context node is
 * the first node on the axis, and the context node's parent the second;
 * parent here is defined the same as with the parent axis.
 *
 * Returns the next element following that axis
 */
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

/**
 * xmlXPathNextFollowingSibling:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "following-sibling" direction
 * The following-sibling axis contains the following siblings of the context
 * node in document order.
 *
 * Returns the next element following that axis
 */
pub unsafe extern "C" fn xml_xpath_next_following_sibling(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if matches!(
        (*(*(*ctxt).context).node).typ,
        XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
    ) {
        return null_mut();
    }
    if cur == (*(*ctxt).context).doc as XmlNodePtr {
        return null_mut();
    }
    if cur.is_null() {
        return (*(*(*ctxt).context).node).next;
    }
    (*cur).next
}

/**
 * xmlXPathNextFollowing:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "following" direction
 * The following axis contains all nodes in the same document as the context
 * node that are after the context node in document order, excluding any
 * descendants and excluding attribute nodes and namespace nodes; the nodes
 * are ordered in document order
 *
 * Returns the next element following that axis
 */
pub unsafe extern "C" fn xml_xpath_next_following(
    ctxt: XmlXPathParserContextPtr,
    mut cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if !cur.is_null()
        && !matches!(
            (*cur).typ,
            XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
        )
        && !(*cur).children.is_null()
    {
        return (*cur).children;
    }

    if cur.is_null() {
        cur = (*(*ctxt).context).node;
        if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
            cur = (*cur).parent;
        } else if matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
            let ns: XmlNsPtr = cur as XmlNsPtr;

            if (*ns).next.load(Ordering::Relaxed).is_null()
                || matches!(
                    (*(*ns).next.load(Ordering::Relaxed)).typ,
                    Some(XmlElementType::XmlNamespaceDecl)
                )
            {
                return null_mut();
            }
            cur = (*ns).next.load(Ordering::Relaxed) as XmlNodePtr;
        }
    }
    if cur.is_null() {
        return null_mut(); /* ERROR */
    }
    if !(*cur).next.is_null() {
        return (*cur).next;
    }
    loop {
        cur = (*cur).parent;
        if cur.is_null() {
            break;
        }
        if cur == (*(*ctxt).context).doc as XmlNodePtr {
            return null_mut();
        }
        if !(*cur).next.is_null() {
            return (*cur).next;
        }
        if cur.is_null() {
            break;
        }
    }
    cur
}

static mut XML_XPATH_XMLNAMESPACE_STRUCT: XmlNs = XmlNs {
    next: AtomicPtr::new(null_mut()),
    typ: Some(XmlElementType::XmlNamespaceDecl),
    href: AtomicPtr::new(XML_XML_NAMESPACE.as_ptr() as *mut u8),
    prefix: AtomicPtr::new(c"xml".as_ptr() as *mut u8),
    _private: AtomicPtr::new(null_mut()),
    context: AtomicPtr::new(null_mut()),
};
// static xmlXPathXMLNamespace: AtomicPtr<xmlNs> =
//     AtomicPtr::new(addr_of_mut!(xmlXPathXMLNamespaceStruct));

/**
 * xmlXPathNextNamespace:
 * @ctxt:  the XPath Parser context
 * @cur:  the current attribute in the traversal
 *
 * Traversal function for the "namespace" direction
 * the namespace axis contains the namespace nodes of the context node;
 * the order of nodes on this axis is implementation-defined; the axis will
 * be empty unless the context node is an element
 *
 * We keep the XML namespace node at the end of the list.
 *
 * Returns the next element following that axis
 */
pub unsafe extern "C" fn xml_xpath_next_namespace(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if !matches!(
        (*(*(*ctxt).context).node).typ,
        XmlElementType::XmlElementNode
    ) {
        return null_mut();
    }
    if cur.is_null() {
        if !(*(*ctxt).context).tmp_ns_list.is_null() {
            xml_free((*(*ctxt).context).tmp_ns_list as _);
        }
        (*(*ctxt).context).tmp_ns_list =
            xml_get_ns_list((*(*ctxt).context).doc, (*(*ctxt).context).node);
        (*(*ctxt).context).tmp_ns_nr = 0;
        if !(*(*ctxt).context).tmp_ns_list.is_null() {
            while !(*(*(*ctxt).context)
                .tmp_ns_list
                .add((*(*ctxt).context).tmp_ns_nr as usize))
            .is_null()
            {
                (*(*ctxt).context).tmp_ns_nr += 1;
            }
        }
        return addr_of_mut!(XML_XPATH_XMLNAMESPACE_STRUCT) as XmlNodePtr;
    }
    if (*(*ctxt).context).tmp_ns_nr > 0 {
        (*(*ctxt).context).tmp_ns_nr -= 1;
        *(*(*ctxt).context)
            .tmp_ns_list
            .add((*(*ctxt).context).tmp_ns_nr as usize) as XmlNodePtr
    } else {
        if !(*(*ctxt).context).tmp_ns_list.is_null() {
            xml_free((*(*ctxt).context).tmp_ns_list as _);
        }
        (*(*ctxt).context).tmp_ns_list = null_mut();
        null_mut()
    }
}

/**
 * xmlXPathNextAttribute:
 * @ctxt:  the XPath Parser context
 * @cur:  the current attribute in the traversal
 *
 * Traversal function for the "attribute" direction
 * TODO: support DTD inherited default attributes
 *
 * Returns the next element following that axis
 */
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
        (*(*(*ctxt).context).node).typ,
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
    (*cur).next as XmlNodePtr
}

/*
 * xmlXPathIsAncestor:
 * @ancestor:  the ancestor node
 * @node:  the current node
 *
 * Check that @ancestor is a @node's ancestor
 *
 * returns 1 if @ancestor is a @node's ancestor, 0 otherwise.
 */
unsafe extern "C" fn xml_xpath_is_ancestor(ancestor: XmlNodePtr, mut node: XmlNodePtr) -> c_int {
    if ancestor.is_null() || node.is_null() {
        return 0;
    }
    if matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
        return 0;
    }
    if matches!((*ancestor).typ, XmlElementType::XmlNamespaceDecl) {
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
    while !(*node).parent.is_null() {
        if (*node).parent == ancestor {
            return 1;
        }
        node = (*node).parent;
    }
    0
}

/**
 * xmlXPathNextPreceding:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "preceding" direction
 * the preceding axis contains all nodes in the same document as the context
 * node that are before the context node in document order, excluding any
 * ancestors and excluding attribute nodes and namespace nodes; the nodes are
 * ordered in reverse document order
 *
 * Returns the next element following that axis
 */
pub unsafe extern "C" fn xml_xpath_next_preceding(
    ctxt: XmlXPathParserContextPtr,
    mut cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if cur.is_null() {
        cur = (*(*ctxt).context).node;
        if matches!((*cur).typ, XmlElementType::XmlAttributeNode) {
            cur = (*cur).parent;
        } else if matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
            let ns: XmlNsPtr = cur as XmlNsPtr;

            if (*ns).next.load(Ordering::Relaxed).is_null()
                || matches!(
                    (*(*ns).next.load(Ordering::Relaxed)).typ,
                    Some(XmlElementType::XmlNamespaceDecl)
                )
            {
                return null_mut();
            }
            cur = (*ns).next.load(Ordering::Relaxed) as XmlNodePtr;
        }
    }
    if cur.is_null() || matches!((*cur).typ, XmlElementType::XmlNamespaceDecl) {
        return null_mut();
    }
    if !(*cur).prev.is_null() && matches!((*(*cur).prev).typ, XmlElementType::XmlDtdNode) {
        cur = (*cur).prev;
    }
    loop {
        if !(*cur).prev.is_null() {
            cur = (*cur).prev;
            while !(*cur).last.is_null() {
                cur = (*cur).last;
            }
            return cur;
        }

        cur = (*cur).parent;
        if cur.is_null() {
            return null_mut();
        }
        if cur == (*(*(*ctxt).context).doc).children {
            return null_mut();
        }
        if xml_xpath_is_ancestor(cur, (*(*ctxt).context).node) == 0 {
            break;
        }
    }
    cur
}

/**
 * xmlXPathNextAncestor:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "ancestor" direction
 * the ancestor axis contains the ancestors of the context node; the ancestors
 * of the context node consist of the parent of context node and the parent's
 * parent and so on; the nodes are ordered in reverse document order; thus the
 * parent is the first node on the axis, and the parent's parent is the second
 * node on the axis
 *
 * Returns the next element following that axis
 */
pub unsafe extern "C" fn xml_xpath_next_ancestor(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    /*
     * the parent of an attribute or namespace node is the element
     * to which the attribute or namespace node is attached
     * !!!!!!!!!!!!!
     */
    if cur.is_null() {
        if (*(*ctxt).context).node.is_null() {
            return null_mut();
        }
        match (*(*(*ctxt).context).node).typ {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlCdataSectionNode
            | XmlElementType::XmlEntityRefNode
            | XmlElementType::XmlEntityNode
            | XmlElementType::XmlPiNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlDtdNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlEntityDecl
            | XmlElementType::XmlNotationNode
            | XmlElementType::XmlXincludeStart
            | XmlElementType::XmlXincludeEnd => {
                if (*(*(*ctxt).context).node).parent.is_null() {
                    return (*(*ctxt).context).doc as XmlNodePtr;
                }
                if matches!(
                    (*(*(*(*ctxt).context).node).parent).typ,
                    XmlElementType::XmlElementNode
                ) && (*(*(*(*(*ctxt).context).node).parent).name.add(0) == b' '
                    || xml_str_equal(
                        (*(*(*(*ctxt).context).node).parent).name,
                        c"fake node libxslt".as_ptr() as _,
                    ))
                {
                    return null_mut();
                }
                return (*(*(*ctxt).context).node).parent;
            }
            XmlElementType::XmlAttributeNode => {
                let tmp: XmlAttrPtr = (*(*ctxt).context).node as XmlAttrPtr;

                return (*tmp).parent;
            }
            XmlElementType::XmlDocumentNode
            | XmlElementType::XmlDocumentTypeNode
            | XmlElementType::XmlDocumentFragNode
            | XmlElementType::XmlHtmlDocumentNode => {
                return null_mut();
            }
            XmlElementType::XmlNamespaceDecl => {
                let ns: XmlNsPtr = (*(*ctxt).context).node as XmlNsPtr;

                if !(*ns).next.load(Ordering::Relaxed).is_null()
                    && !matches!(
                        (*(*ns).next.load(Ordering::Relaxed)).typ,
                        Some(XmlElementType::XmlNamespaceDecl)
                    )
                {
                    return (*ns).next.load(Ordering::Relaxed) as XmlNodePtr;
                }
                /* Bad, how did that namespace end up here ? */
                return null_mut();
            }
            _ => unreachable!(),
        }
    }
    if cur == (*(*(*ctxt).context).doc).children {
        return (*(*ctxt).context).doc as XmlNodePtr;
    }
    if cur == (*(*ctxt).context).doc as XmlNodePtr {
        return null_mut();
    }
    match (*cur).typ {
        XmlElementType::XmlElementNode
        | XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => {
            if (*cur).parent.is_null() {
                return null_mut();
            }
            if matches!((*(*cur).parent).typ, XmlElementType::XmlElementNode)
                && (*(*(*cur).parent).name.add(0) == b' '
                    || xml_str_equal((*(*cur).parent).name, c"fake node libxslt".as_ptr() as _))
            {
                return null_mut();
            }
            (*cur).parent
        }
        XmlElementType::XmlAttributeNode => {
            let att: XmlAttrPtr = cur as XmlAttrPtr;

            (*att).parent
        }
        XmlElementType::XmlNamespaceDecl => {
            let ns: XmlNsPtr = cur as XmlNsPtr;

            if !(*ns).next.load(Ordering::Relaxed).is_null()
                && !matches!(
                    (*(*ns).next.load(Ordering::Relaxed)).typ,
                    Some(XmlElementType::XmlNamespaceDecl)
                )
            {
                return (*ns).next.load(Ordering::Relaxed) as XmlNodePtr;
            }
            /* Bad, how did that namespace end up here ? */
            null_mut()
        }
        XmlElementType::XmlDocumentNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlHtmlDocumentNode => null_mut(),
        _ => unreachable!(),
    }
}

/**
 * xmlXPathNextPrecedingSibling:
 * @ctxt:  the XPath Parser context
 * @cur:  the current node in the traversal
 *
 * Traversal function for the "preceding-sibling" direction
 * The preceding-sibling axis contains the preceding siblings of the context
 * node in reverse document order; the first preceding sibling is first on the
 * axis; the sibling preceding that node is the second on the axis and so on.
 *
 * Returns the next element following that axis
 */
pub unsafe extern "C" fn xml_xpath_next_preceding_sibling(
    ctxt: XmlXPathParserContextPtr,
    mut cur: XmlNodePtr,
) -> XmlNodePtr {
    if ctxt.is_null() || (*ctxt).context.is_null() {
        return null_mut();
    }
    if matches!(
        (*(*(*ctxt).context).node).typ,
        XmlElementType::XmlAttributeNode | XmlElementType::XmlNamespaceDecl
    ) {
        return null_mut();
    }
    if cur == (*(*ctxt).context).doc as XmlNodePtr {
        return null_mut();
    }
    if cur.is_null() {
        return (*(*(*ctxt).context).node).prev;
    }
    if !(*cur).prev.is_null() && matches!((*(*cur).prev).typ, XmlElementType::XmlDtdNode) {
        cur = (*cur).prev;
        if cur.is_null() {
            return (*(*(*ctxt).context).node).prev;
        }
    }
    (*cur).prev
}

/*
 * The official core of XPath functions.
 */
/**
 * xmlXPathLastFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the last() XPath function
 *    number last()
 * The last function returns the number of nodes in the context node list.
 */
pub unsafe extern "C" fn xml_xpath_last_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    CHECK_ARITY!(ctxt, nargs, 0);
    if (*(*ctxt).context).context_size >= 0 {
        value_push(
            ctxt,
            xml_xpath_cache_new_float((*ctxt).context, (*(*ctxt).context).context_size as f64),
        );
    // #ifdef DEBUG_EXPR
    // 	xmlGenericError(xmlGenericErrorContext,
    // 		"last() : %d\n", (*(*ctxt).context).contextSize);
    // #endif
    } else {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidCtxtSize as i32);
    }
}

/**
 * xmlXPathPositionFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the position() XPath function
 *    number position()
 * The position function returns the position of the context node in the
 * context node list. The first position is 1, and so the last position
 * will be equal to last().
 */
pub unsafe extern "C" fn xml_xpath_position_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    CHECK_ARITY!(ctxt, nargs, 0);
    if (*(*ctxt).context).proximity_position >= 0 {
        value_push(
            ctxt,
            xml_xpath_cache_new_float(
                (*ctxt).context,
                (*(*ctxt).context).proximity_position as f64,
            ),
        );
    // #ifdef DEBUG_EXPR
    // 	xmlGenericError(xmlGenericErrorContext, "position() : %d\n",
    // 		(*(*ctxt).context).proximityPosition);
    // #endif
    } else {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidCtxtPosition as i32);
    }
}

/**
 * xmlXPathCountFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the count() XPath function
 *    number count(node-set)
 */
pub unsafe extern "C" fn xml_xpath_count_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    CHECK_ARITY!(ctxt, nargs, 1);
    if (*ctxt).value.is_null()
        || !matches!(
            (*(*ctxt).value).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
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
            xml_xpath_cache_new_float((*ctxt).context, (*(*cur).nodesetval).node_nr as f64),
        );
    }
    xml_xpath_release_object((*ctxt).context, cur);
}

/**
 * xmlXPathGetElementsByIds:
 * @doc:  the document
 * @ids:  a whitespace separated list of IDs
 *
 * Selects elements by their unique ID.
 *
 * Returns a node-set of selected elements.
 */
unsafe extern "C" fn xml_xpath_get_elements_by_ids(
    doc: XmlDocPtr,
    mut ids: *const XmlChar,
) -> XmlNodeSetPtr {
    let mut cur: *const XmlChar = ids;
    let mut id: *mut XmlChar;
    let mut attr: XmlAttrPtr;
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
             * constrac_int, like Visa3D spec.
             * if (xmlValidateNCName(ID, 1) == 0)
             */
            attr = xml_get_id(doc, id);
            if !attr.is_null() {
                if matches!((*attr).typ, XmlElementType::XmlAttributeNode) {
                    elem = (*attr).parent;
                } else if matches!((*attr).typ, XmlElementType::XmlElementNode) {
                    elem = attr as XmlNodePtr;
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

/**
 * xmlXPathCacheConvertString:
 * @ctxt: the XPath context
 * @val:  an XPath object
 *
 * This is the cached version of xmlXPathConvertString().
 * Converts an existing object to its string() equivalent
 *
 * Returns a created or reused object, the old one is freed (cached)
 *         (or the operation is done directly on @val)
 */

unsafe extern "C" fn xml_xpath_cache_convert_string(
    ctxt: XmlXPathContextPtr,
    val: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    let mut res: *mut XmlChar = null_mut();

    if val.is_null() {
        return xml_xpath_cache_new_cstring(ctxt, c"".as_ptr() as _);
    }

    match (*val).typ {
        XmlXPathObjectType::XpathUndefined => {
            // #ifdef DEBUG_EXPR
            // 	xmlGenericError(xmlGenericErrorContext, "STRING: undefined\n");
            // #endif
        }
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {
            res = xml_xpath_cast_node_set_to_string((*val).nodesetval);
        }
        XmlXPathObjectType::XpathString => {
            return val;
        }
        XmlXPathObjectType::XpathBoolean => {
            res = xml_xpath_cast_boolean_to_string((*val).boolval);
        }
        XmlXPathObjectType::XpathNumber => {
            res = xml_xpath_cast_number_to_string((*val).floatval);
        }
        XmlXPathObjectType::XpathUsers => {
            todo!();
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XpathPoint
        | XmlXPathObjectType::XpathRange
        | XmlXPathObjectType::XpathLocationset => {
            todo!()
        }
    }
    xml_xpath_release_object(ctxt, val);
    if res.is_null() {
        return xml_xpath_cache_new_cstring(ctxt, c"".as_ptr() as _);
    }
    xml_xpath_cache_wrap_string(ctxt, res)
}

/**
 * xmlXPathIdFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the id() XPath function
 *    node-set id(object)
 * The id function selects elements by their unique ID
 * (see [5.2.1 Unique IDs]). When the argument to id is of type node-set,
 * then the result is the union of the result of applying id to the
 * string value of each of the nodes in the argument node-set. When the
 * argument to id is of any other type, the argument is converted to a
 * string as if by a call to the string function; the string is split
 * c_into a whitespace-separated list of tokens (whitespace is any sequence
 * of characters matching the production S); the result is a node-set
 * containing the elements in the same document as the context node that
 * have a unique ID equal to any of the tokens in the list.
 */
pub unsafe extern "C" fn xml_xpath_id_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    let mut tokens: *mut XmlChar;
    let mut ret: XmlNodeSetPtr;
    let mut obj: XmlXPathObjectPtr;

    CHECK_ARITY!(ctxt, nargs, 1);
    obj = value_pop(ctxt);
    if obj.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    if matches!(
        (*obj).typ,
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
    ) {
        let mut ns: XmlNodeSetPtr;

        /* TODO: Check memory error. */
        ret = xml_xpath_node_set_create(null_mut());

        if !(*obj).nodesetval.is_null() {
            for i in 0..(*(*obj).nodesetval).node_nr {
                tokens =
                    xml_xpath_cast_node_to_string(*(*(*obj).nodesetval).node_tab.add(i as usize));
                ns = xml_xpath_get_elements_by_ids((*(*ctxt).context).doc, tokens);
                /* TODO: Check memory error. */
                ret = xml_xpath_node_set_merge(ret, ns);
                xml_xpath_free_node_set(ns);
                if !tokens.is_null() {
                    xml_free(tokens as _);
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
    ret = xml_xpath_get_elements_by_ids((*(*ctxt).context).doc, (*obj).stringval);
    value_push(ctxt, xml_xpath_cache_wrap_node_set((*ctxt).context, ret));
    xml_xpath_release_object((*ctxt).context, obj);
}

/**
 * xmlXPathLocalNameFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the local-name() XPath function
 *    string local-name(node-set?)
 * The local-name function returns a string containing the local part
 * of the name of the node in the argument node-set that is first in
 * document order. If the node-set is empty or the first node has no
 * name, an empty string is returned. If the argument is omitted it
 * defaults to the context node.
 */
pub unsafe extern "C" fn xml_xpath_local_name_function(
    ctxt: XmlXPathParserContextPtr,
    mut nargs: c_int,
) {
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
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }
    let cur: XmlXPathObjectPtr = value_pop(ctxt);

    if (*cur).nodesetval.is_null() || (*(*cur).nodesetval).node_nr == 0 {
        value_push(
            ctxt,
            xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
        );
    } else {
        let i: c_int = 0; /* Should be first in document order !!!!! */
        match (*(*(*(*cur).nodesetval).node_tab.add(i as usize))).typ {
            XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlPiNode => {
                if *(*(*(*(*cur).nodesetval).node_tab.add(i as usize)))
                    .name
                    .add(0)
                    == b' '
                {
                    value_push(
                        ctxt,
                        xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
                    );
                } else {
                    value_push(
                        ctxt,
                        xml_xpath_cache_new_string(
                            (*ctxt).context,
                            (*(*(*(*cur).nodesetval).node_tab.add(i as usize))).name,
                        ),
                    );
                }
            }
            XmlElementType::XmlNamespaceDecl => {
                value_push(
                    ctxt,
                    xml_xpath_cache_new_string(
                        (*ctxt).context,
                        (*(*(*(*cur).nodesetval).node_tab.add(i as usize) as XmlNsPtr))
                            .prefix
                            .load(Ordering::Relaxed),
                    ),
                );
            }
            _ => {
                value_push(
                    ctxt,
                    xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
                );
            }
        }
    }
    xml_xpath_release_object((*ctxt).context, cur);
}

/**
 * xmlXPathNamespaceURIFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the namespace-uri() XPath function
 *    string namespace-uri(node-set?)
 * The namespace-uri function returns a string containing the
 * namespace URI of the expanded name of the node in the argument
 * node-set that is first in document order. If the node-set is empty,
 * the first node has no name, or the expanded name has no namespace
 * URI, an empty string is returned. If the argument is omitted it
 * defaults to the context node.
 */
pub unsafe extern "C" fn xml_xpath_namespace_uri_function(
    ctxt: XmlXPathParserContextPtr,
    mut nargs: c_int,
) {
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
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }
    let cur: XmlXPathObjectPtr = value_pop(ctxt);

    if (*cur).nodesetval.is_null() || (*(*cur).nodesetval).node_nr == 0 {
        value_push(
            ctxt,
            xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
        );
    } else {
        let i: c_int = 0; /* Should be first in document order !!!!! */
        match (*(*(*(*cur).nodesetval).node_tab.add(i as usize))).typ {
            XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
                if (*(*(*(*cur).nodesetval).node_tab.add(i as usize)))
                    .ns
                    .is_null()
                {
                    value_push(
                        ctxt,
                        xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
                    );
                } else {
                    value_push(
                        ctxt,
                        xml_xpath_cache_new_string(
                            (*ctxt).context,
                            (*(*(*(*(*cur).nodesetval).node_tab.add(i as usize))).ns)
                                .href
                                .load(Ordering::Relaxed),
                        ),
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
    }
    xml_xpath_release_object((*ctxt).context, cur);
}

/**
 * xmlXPathStringFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the string() XPath function
 *    string string(object?)
 * The string function converts an object to a string as follows:
 *    - A node-set is converted to a string by returning the value of
 *      the node in the node-set that is first in document order.
 *      If the node-set is empty, an empty string is returned.
 *    - A number is converted to a string as follows
 *      + NaN is converted to the string NaN
 *      + positive zero is converted to the string 0
 *      + negative zero is converted to the string 0
 *      + positive infinity is converted to the string Infinity
 *      + negative infinity is converted to the string -Infinity
 *      + if the number is an c_integer, the number is represented in
 *        decimal form as a Number with no decimal point and no leading
 *        zeros, preceded by a minus sign (-) if the number is negative
 *      + otherwise, the number is represented in decimal form as a
 *        Number including a decimal point with at least one digit
 *        before the decimal point and at least one digit after the
 *        decimal point, preceded by a minus sign (-) if the number
 *        is negative; there must be no leading zeros before the decimal
 *        point apart possibly from the one required digit immediately
 *        before the decimal point; beyond the one required digit
 *        after the decimal point there must be as many, but only as
 *        many, more digits as are needed to uniquely distinguish the
 *        number from all other IEEE 754 numeric values.
 *    - The boolean false value is converted to the string false.
 *      The boolean true value is converted to the string true.
 *
 * If the argument is omitted, it defaults to a node-set with the
 * context node as its only member.
 */
pub unsafe extern "C" fn xml_xpath_string_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    if ctxt.is_null() {
        return;
    }
    if nargs == 0 {
        value_push(
            ctxt,
            xml_xpath_cache_wrap_string(
                (*ctxt).context,
                xml_xpath_cast_node_to_string((*(*ctxt).context).node),
            ),
        );
        return;
    }

    CHECK_ARITY!(ctxt, nargs, 1);
    let cur: XmlXPathObjectPtr = value_pop(ctxt);
    if cur.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    value_push(ctxt, xml_xpath_cache_convert_string((*ctxt).context, cur));
}

/**
 * xmlXPathStringLengthFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the string-length() XPath function
 *    number string-length(string?)
 * The string-length returns the number of characters in the string
 * (see [3.6 Strings]). If the argument is omitted, it defaults to
 * the context node converted to a string, in other words the value
 * of the context node.
 */
pub unsafe extern "C" fn xml_xpath_string_length_function(
    ctxt: XmlXPathParserContextPtr,
    nargs: c_int,
) {
    if nargs == 0 {
        if ctxt.is_null() || (*ctxt).context.is_null() {
            return;
        }
        if (*(*ctxt).context).node.is_null() {
            value_push(ctxt, xml_xpath_cache_new_float((*ctxt).context, 0.0));
        } else {
            let content: *mut XmlChar = xml_xpath_cast_node_to_string((*(*ctxt).context).node);
            value_push(
                ctxt,
                xml_xpath_cache_new_float((*ctxt).context, xml_utf8_strlen(content) as _),
            );
            xml_free(content as _);
        }
        return;
    }
    CHECK_ARITY!(ctxt, nargs, 1);
    CAST_TO_STRING!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathString);
    let cur: XmlXPathObjectPtr = value_pop(ctxt);
    value_push(
        ctxt,
        xml_xpath_cache_new_float((*ctxt).context, xml_utf8_strlen((*cur).stringval) as _),
    );
    xml_xpath_release_object((*ctxt).context, cur);
}

/**
 * xmlXPathConcatFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the concat() XPath function
 *    string concat(string, string, string*)
 * The concat function returns the concatenation of its arguments.
 */
pub unsafe extern "C" fn xml_xpath_concat_function(
    ctxt: XmlXPathParserContextPtr,
    mut nargs: c_int,
) {
    let mut newobj: XmlXPathObjectPtr;
    let mut tmp: *mut XmlChar;

    if ctxt.is_null() {
        return;
    }
    if nargs < 2 {
        CHECK_ARITY!(ctxt, nargs, 2);
    }

    CAST_TO_STRING!(ctxt);
    let cur: XmlXPathObjectPtr = value_pop(ctxt);
    if cur.is_null() || !matches!((*cur).typ, XmlXPathObjectType::XpathString) {
        xml_xpath_release_object((*ctxt).context, cur);
        return;
    }
    nargs -= 1;

    while nargs > 0 {
        CAST_TO_STRING!(ctxt);
        newobj = value_pop(ctxt);
        if newobj.is_null() || (*newobj).typ != XmlXPathObjectType::XpathString {
            xml_xpath_release_object((*ctxt).context, newobj);
            xml_xpath_release_object((*ctxt).context, cur);
            XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
        }
        tmp = xml_strcat((*newobj).stringval, (*cur).stringval);
        (*newobj).stringval = (*cur).stringval;
        (*cur).stringval = tmp;
        xml_xpath_release_object((*ctxt).context, newobj);
        nargs -= 1;
    }
    value_push(ctxt, cur);
}

/**
 * xmlXPathContainsFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the contains() XPath function
 *    boolean contains(string, string)
 * The contains function returns true if the first argument string
 * contains the second argument string, and otherwise returns false.
 */
pub unsafe extern "C" fn xml_xpath_contains_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    CHECK_ARITY!(ctxt, nargs, 2);
    CAST_TO_STRING!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathString);
    let needle: XmlXPathObjectPtr = value_pop(ctxt);
    CAST_TO_STRING!(ctxt);
    let hay: XmlXPathObjectPtr = value_pop(ctxt);

    if hay.is_null() || (*hay).typ != XmlXPathObjectType::XpathString {
        xml_xpath_release_object((*ctxt).context, hay);
        xml_xpath_release_object((*ctxt).context, needle);
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }
    if !xml_strstr((*hay).stringval, (*needle).stringval).is_null() {
        value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, 1));
    } else {
        value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, 0));
    }
    xml_xpath_release_object((*ctxt).context, hay);
    xml_xpath_release_object((*ctxt).context, needle);
}

/**
 * xmlXPathStartsWithFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the starts-with() XPath function
 *    boolean starts-with(string, string)
 * The starts-with function returns true if the first argument string
 * starts with the second argument string, and otherwise returns false.
 */
pub unsafe extern "C" fn xml_xpath_starts_with_function(
    ctxt: XmlXPathParserContextPtr,
    nargs: c_int,
) {
    CHECK_ARITY!(ctxt, nargs, 2);
    CAST_TO_STRING!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathString);
    let needle: XmlXPathObjectPtr = value_pop(ctxt);
    CAST_TO_STRING!(ctxt);
    let hay: XmlXPathObjectPtr = value_pop(ctxt);

    if hay.is_null() || (*hay).typ != XmlXPathObjectType::XpathString {
        xml_xpath_release_object((*ctxt).context, hay);
        xml_xpath_release_object((*ctxt).context, needle);
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }
    let n: c_int = xml_strlen((*needle).stringval);
    if xml_strncmp((*hay).stringval, (*needle).stringval, n) != 0 {
        value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, 0));
    } else {
        value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, 1));
    }
    xml_xpath_release_object((*ctxt).context, hay);
    xml_xpath_release_object((*ctxt).context, needle);
}

/**
 * xmlXPathSubstringFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the substring() XPath function
 *    string substring(string, number, number?)
 * The substring function returns the substring of the first argument
 * starting at the position specified in the second argument with
 * length specified in the third argument. For example,
 * substring("12345",2,3) returns "234". If the third argument is not
 * specified, it returns the substring starting at the position specified
 * in the second argument and continuing to the end of the string. For
 * example, substring("12345",2) returns "2345".  More precisely, each
 * character in the string (see [3.6 Strings]) is considered to have a
 * numeric position: the position of the first character is 1, the position
 * of the second character is 2 and so on. The returned substring contains
 * those characters for which the position of the character is greater than
 * or equal to the second argument and, if the third argument is specified,
 * less than the sum of the second and third arguments; the comparisons
 * and addition used for the above follow the standard IEEE 754 rules. Thus:
 *  - substring("12345", 1.5, 2.6) returns "234"
 *  - substring("12345", 0, 3) returns "12"
 *  - substring("12345", 0 div 0, 3) returns ""
 *  - substring("12345", 1, 0 div 0) returns ""
 *  - substring("12345", -42, 1 div 0) returns "12345"
 *  - substring("12345", -1 div 0, 1 div 0) returns ""
 */
pub unsafe extern "C" fn xml_xpath_substring_function(
    ctxt: XmlXPathParserContextPtr,
    nargs: c_int,
) {
    let len: XmlXPathObjectPtr;
    let mut le: f64 = 0.0;
    let mut i: c_int = 1;
    let mut j: c_int = INT_MAX;

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
        CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathNumber);
        len = value_pop(ctxt);
        le = (*len).floatval;
        xml_xpath_release_object((*ctxt).context, len);
    }

    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathNumber);
    let start: XmlXPathObjectPtr = value_pop(ctxt);
    let input: f64 = (*start).floatval;
    xml_xpath_release_object((*ctxt).context, start);
    CAST_TO_STRING!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathString);
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
            /* Logical NOT to handle NaNs */
            j = 1;
        } else if end < INT_MAX as f64 {
            j = end as _;
        }
    }

    if i < j {
        let ret: *mut XmlChar = xml_utf8_strsub((*str).stringval, i - 1, j - i);
        value_push(ctxt, xml_xpath_cache_new_string((*ctxt).context, ret));
        xml_free(ret as _);
    } else {
        value_push(
            ctxt,
            xml_xpath_cache_new_cstring((*ctxt).context, c"".as_ptr() as _),
        );
    }

    xml_xpath_release_object((*ctxt).context, str);
}

/**
 * xmlXPathSubstringBeforeFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the substring-before() XPath function
 *    string substring-before(string, string)
 * The substring-before function returns the substring of the first
 * argument string that precedes the first occurrence of the second
 * argument string in the first argument string, or the empty string
 * if the first argument string does not contain the second argument
 * string. For example, substring-before("1999/04/01","/") returns 1999.
 */
pub unsafe extern "C" fn xml_xpath_substring_before_function(
    ctxt: XmlXPathParserContextPtr,
    nargs: c_int,
) {
    let point: *const XmlChar;
    let offset: c_int;

    CHECK_ARITY!(ctxt, nargs, 2);
    CAST_TO_STRING!(ctxt);
    let find: XmlXPathObjectPtr = value_pop(ctxt);
    CAST_TO_STRING!(ctxt);
    let str: XmlXPathObjectPtr = value_pop(ctxt);

    let target: XmlBufPtr = xml_buf_create();
    if !target.is_null() {
        point = xml_strstr((*str).stringval, (*find).stringval);
        if !point.is_null() {
            offset = point.offset_from((*str).stringval) as _;
            xml_buf_add(target, (*str).stringval, offset);
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

/**
 * xmlXPathSubstringAfterFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the substring-after() XPath function
 *    string substring-after(string, string)
 * The substring-after function returns the substring of the first
 * argument string that follows the first occurrence of the second
 * argument string in the first argument string, or the empty stringi
 * if the first argument string does not contain the second argument
 * string. For example, substring-after("1999/04/01","/") returns 04/01,
 * and substring-after("1999/04/01","19") returns 99/04/01.
 */
pub unsafe extern "C" fn xml_xpath_substring_after_function(
    ctxt: XmlXPathParserContextPtr,
    nargs: c_int,
) {
    let point: *const XmlChar;
    let offset: c_int;

    CHECK_ARITY!(ctxt, nargs, 2);
    CAST_TO_STRING!(ctxt);
    let find: XmlXPathObjectPtr = value_pop(ctxt);
    CAST_TO_STRING!(ctxt);
    let str: XmlXPathObjectPtr = value_pop(ctxt);

    let target: XmlBufPtr = xml_buf_create();
    if !target.is_null() {
        point = xml_strstr((*str).stringval, (*find).stringval);
        if !point.is_null() {
            offset = (point.offset_from((*str).stringval) + xml_strlen((*find).stringval) as isize)
                as i32;
            xml_buf_add(
                target,
                (*str).stringval.add(offset as usize),
                xml_strlen((*str).stringval) - offset,
            );
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

/**
 * xmlXPathNormalizeFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the normalize-space() XPath function
 *    string normalize-space(string?)
 * The normalize-space function returns the argument string with white
 * space normalized by stripping leading and trailing whitespace
 * and replacing sequences of whitespace characters by a single
 * space. Whitespace characters are the same allowed by the S production
 * in XML. If the argument is omitted, it defaults to the context
 * node converted to a string, in other words the value of the context node.
 */
pub unsafe extern "C" fn xml_xpath_normalize_function(
    ctxt: XmlXPathParserContextPtr,
    mut nargs: c_int,
) {
    let mut source: *mut XmlChar;
    let mut target: *mut XmlChar;
    let mut blank: c_int;

    if ctxt.is_null() {
        return;
    }
    if nargs == 0 {
        /* Use current context node */
        value_push(
            ctxt,
            xml_xpath_cache_wrap_string(
                (*ctxt).context,
                xml_xpath_cast_node_to_string((*(*ctxt).context).node),
            ),
        );
        nargs = 1;
    }

    CHECK_ARITY!(ctxt, nargs, 1);
    CAST_TO_STRING!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathString);
    source = (*(*ctxt).value).stringval;
    if source.is_null() {
        return;
    }
    target = source;

    /* Skip leading whitespaces */
    while xml_is_blank_char(*source as u32) {
        source = source.add(1);
    }

    /* Collapse c_intermediate whitespaces, and skip trailing whitespaces */
    blank = 0;
    while *source != 0 {
        if xml_is_blank_char(*source as u32) {
            blank = 1;
        } else {
            if blank != 0 {
                *target = 0x20;
                target = target.add(1);
                blank = 0;
            }
            *target = *source;
            target = target.add(1);
        }
        source = source.add(1);
    }
    *target = 0;
}

/**
 * xmlXPathTranslateFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the translate() XPath function
 *    string translate(string, string, string)
 * The translate function returns the first argument string with
 * occurrences of characters in the second argument string replaced
 * by the character at the corresponding position in the third argument
 * string. For example, translate("bar","abc","ABC") returns the string
 * BAr. If there is a character in the second argument string with no
 * character at a corresponding position in the third argument string
 * (because the second argument string is longer than the third argument
 * string), then occurrences of that character in the first argument
 * string are removed. For example, translate("-=1aaa-=1","abc-","ABC")
 * returns "AAA". If a character occurs more than once in second
 * argument string, then the first occurrence determines the replacement
 * character. If the third argument string is longer than the second
 * argument string, then excess characters are ignored.
 */
pub unsafe extern "C" fn xml_xpath_translate_function(
    ctxt: XmlXPathParserContextPtr,
    nargs: c_int,
) {
    let mut offset: c_int;
    let max: c_int;
    let mut ch: c_int;
    let mut point: *const XmlChar;
    let mut cptr: *mut XmlChar;

    CHECK_ARITY!(ctxt, nargs, 3);

    CAST_TO_STRING!(ctxt);
    let to: XmlXPathObjectPtr = value_pop(ctxt);
    CAST_TO_STRING!(ctxt);
    let from: XmlXPathObjectPtr = value_pop(ctxt);
    CAST_TO_STRING!(ctxt);
    let str: XmlXPathObjectPtr = value_pop(ctxt);

    let target: XmlBufPtr = xml_buf_create();
    if !target.is_null() {
        max = xml_utf8_strlen((*to).stringval);
        cptr = (*str).stringval;
        while {
            ch = *cptr as i32;
            ch != 0
        } {
            offset = xml_utf8_strloc((*from).stringval, cptr);
            if offset >= 0 {
                if offset < max {
                    point = xml_utf8_strpos((*to).stringval, offset);
                    if !point.is_null() {
                        xml_buf_add(target, point, xml_utf8_strsize(point, 1));
                    }
                }
            } else {
                xml_buf_add(target, cptr, xml_utf8_strsize(cptr, 1));
            }

            /* Step to next character in input */
            cptr = cptr.add(1);
            if ch & 0x80 != 0 {
                /* if not simple ascii, verify proper format */
                if ch & 0xc0 != 0xc0 {
                    generic_error!("xmlXPathTranslateFunction: Invalid UTF8 string\n");
                    /* not asserting an XPath error is probably better */
                    break;
                }
                /* then skip over remaining bytes for this c_char */
                while {
                    ch <<= 1;
                    ch & 0x80 != 0
                } {
                    let f = *cptr & 0xc0 != 0x80;
                    cptr = cptr.add(1);
                    if f {
                        generic_error!("xmlXPathTranslateFunction: Invalid UTF8 string\n");
                        /* not asserting an XPath error is probably better */
                        break;
                    }
                }
                /* must have had error encountered */
                if ch & 0x80 != 0 {
                    break;
                }
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

/**
 * xmlXPathNotFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the not() XPath function
 *    boolean not(boolean)
 * The not function returns true if its argument is false,
 * and false otherwise.
 */
pub unsafe extern "C" fn xml_xpath_not_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    CHECK_ARITY!(ctxt, nargs, 1);
    CAST_TO_BOOLEAN!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathBoolean);
    (*(*ctxt).value).boolval = ((*(*ctxt).value).boolval == 0) as i32;
}

/**
 * xmlXPathTrueFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the true() XPath function
 *    boolean true()
 */
pub unsafe extern "C" fn xml_xpath_true_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    CHECK_ARITY!(ctxt, nargs, 0);
    value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, 1));
}

/**
 * xmlXPathFalseFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the false() XPath function
 *    boolean false()
 */
pub unsafe extern "C" fn xml_xpath_false_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    CHECK_ARITY!(ctxt, nargs, 0);
    value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, 0));
}

/**
 * xmlXPathLangFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the lang() XPath function
 *    boolean lang(string)
 * The lang function returns true or false depending on whether the
 * language of the context node as specified by xml:lang attributes
 * is the same as or is a sublanguage of the language specified by
 * the argument string. The language of the context node is determined
 * by the value of the xml:lang attribute on the context node, or, if
 * the context node has no xml:lang attribute, by the value of the
 * xml:lang attribute on the nearest ancestor of the context node that
 * has an xml:lang attribute. If there is no such attribute, then lang
 * returns false. If there is such an attribute, then lang returns
 * true if the attribute value is equal to the argument ignoring case,
 * or if there is some suffix starting with - such that the attribute
 * value is equal to the argument ignoring that suffix of the attribute
 * value and ignoring case.
 */
pub unsafe extern "C" fn xml_xpath_lang_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    let mut ret: c_int = 0;

    CHECK_ARITY!(ctxt, nargs, 1);
    CAST_TO_STRING!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathString);
    let val = value_pop(ctxt);
    let lang: *const XmlChar = (*val).stringval;
    let the_lang = xml_node_get_lang((*(*ctxt).context).node);
    'not_equal: {
        if !the_lang.is_null() && !lang.is_null() {
            let mut i = 0;
            while *lang.add(i) != 0 {
                if (*lang.add(i)).to_ascii_uppercase() != (*the_lang.add(i)).to_ascii_uppercase() {
                    break 'not_equal;
                }
                i += 1;
            }
            if *the_lang.add(i) == 0 || *the_lang.add(i) == b'-' {
                ret = 1;
            }
        }
    }
    // not_equal:
    if !the_lang.is_null() {
        xml_free(the_lang as _);
    }

    xml_xpath_release_object((*ctxt).context, val);
    value_push(ctxt, xml_xpath_cache_new_boolean((*ctxt).context, ret));
}

/**
 * xmlXPathCacheConvertNumber:
 * @ctxt: the XPath context
 * @val:  an XPath object
 *
 * This is the cached version of xmlXPathConvertNumber().
 * Converts an existing object to its number() equivalent
 *
 * Returns a created or reused object, the old one is freed (or the operation
 *         is done directly on @val)
 */
unsafe extern "C" fn xml_xpath_cache_convert_number(
    ctxt: XmlXPathContextPtr,
    val: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    if val.is_null() {
        return xml_xpath_cache_new_float(ctxt, 0.0);
    }
    if matches!((*val).typ, XmlXPathObjectType::XpathNumber) {
        return val;
    }
    let ret: XmlXPathObjectPtr = xml_xpath_cache_new_float(ctxt, xml_xpath_cast_to_number(val));
    xml_xpath_release_object(ctxt, val);
    ret
}

/**
 * xmlXPathNumberFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the number() XPath function
 *    number number(object?)
 */
pub unsafe extern "C" fn xml_xpath_number_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    let res: f64;

    if ctxt.is_null() {
        return;
    }
    if nargs == 0 {
        if (*(*ctxt).context).node.is_null() {
            value_push(ctxt, xml_xpath_cache_new_float((*ctxt).context, 0.0));
        } else {
            let content: *mut XmlChar = xml_node_get_content((*(*ctxt).context).node);

            res = xml_xpath_string_eval_number(content);
            value_push(ctxt, xml_xpath_cache_new_float((*ctxt).context, res));
            xml_free(content as _);
        }
        return;
    }

    CHECK_ARITY!(ctxt, nargs, 1);
    let cur: XmlXPathObjectPtr = value_pop(ctxt);
    value_push(ctxt, xml_xpath_cache_convert_number((*ctxt).context, cur));
}

/**
 * xmlXPathSumFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the sum() XPath function
 *    number sum(node-set)
 * The sum function returns the sum of the values of the nodes in
 * the argument node-set.
 */
pub unsafe extern "C" fn xml_xpath_sum_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    let mut res: f64 = 0.0;

    CHECK_ARITY!(ctxt, nargs, 1);
    if (*ctxt).value.is_null()
        || !matches!(
            (*(*ctxt).value).typ,
            XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
        )
    {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidType as i32);
    }
    let cur: XmlXPathObjectPtr = value_pop(ctxt);

    if !(*cur).nodesetval.is_null() && (*(*cur).nodesetval).node_nr != 0 {
        for i in 0..(*(*cur).nodesetval).node_nr {
            res += xml_xpath_cast_node_to_number(*(*(*cur).nodesetval).node_tab.add(i as usize));
        }
    }
    value_push(ctxt, xml_xpath_cache_new_float((*ctxt).context, res));
    xml_xpath_release_object((*ctxt).context, cur);
}

/**
 * xmlXPathFloorFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the floor() XPath function
 *    number floor(number)
 * The floor function returns the largest (closest to positive infinity)
 * number that is not greater than the argument and that is an c_integer.
 */
pub unsafe extern "C" fn xml_xpath_floor_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    CHECK_ARITY!(ctxt, nargs, 1);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathNumber);

    (*(*ctxt).value).floatval = (*(*ctxt).value).floatval.floor();
}

/**
 * xmlXPathCeilingFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the ceiling() XPath function
 *    number ceiling(number)
 * The ceiling function returns the smallest (closest to negative infinity)
 * number that is not less than the argument and that is an c_integer.
 */
pub unsafe extern "C" fn xml_xpath_ceiling_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    CHECK_ARITY!(ctxt, nargs, 1);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathNumber);

    (*(*ctxt).value).floatval = (*(*ctxt).value).floatval.ceil();
}

/**
 * xmlXPathRoundFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the round() XPath function
 *    number round(number)
 * The round function returns the number that is closest to the
 * argument and that is an c_integer. If there are two such numbers,
 * then the one that is closest to positive infinity is returned.
 */
pub unsafe extern "C" fn xml_xpath_round_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    CHECK_ARITY!(ctxt, nargs, 1);
    CAST_TO_NUMBER!(ctxt);
    CHECK_TYPE!(ctxt, XmlXPathObjectType::XpathNumber);

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

/**
 * xmlXPathCacheConvertBoolean:
 * @ctxt: the XPath context
 * @val:  an XPath object
 *
 * This is the cached version of xmlXPathConvertBoolean().
 * Converts an existing object to its boolean() equivalent
 *
 * Returns a created or reused object, the old one is freed (or the operation
 *         is done directly on @val)
 */
unsafe extern "C" fn xml_xpath_cache_convert_boolean(
    ctxt: XmlXPathContextPtr,
    val: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr {
    if val.is_null() {
        return xml_xpath_cache_new_boolean(ctxt, 0);
    }
    if matches!((*val).typ, XmlXPathObjectType::XpathBoolean) {
        return val;
    }
    let ret: XmlXPathObjectPtr = xml_xpath_cache_new_boolean(ctxt, xml_xpath_cast_to_boolean(val));
    xml_xpath_release_object(ctxt, val);
    ret
}

/**
 * xmlXPathBooleanFunction:
 * @ctxt:  the XPath Parser context
 * @nargs:  the number of arguments
 *
 * Implement the boolean() XPath function
 *    boolean boolean(object)
 * The boolean function converts its argument to a boolean as follows:
 *    - a number is true if and only if it is neither positive or
 *      negative zero nor NaN
 *    - a node-set is true if and only if it is non-empty
 *    - a string is true if and only if its length is non-zero
 */
pub unsafe extern "C" fn xml_xpath_boolean_function(ctxt: XmlXPathParserContextPtr, nargs: c_int) {
    let mut cur: XmlXPathObjectPtr;

    CHECK_ARITY!(ctxt, nargs, 1);
    cur = value_pop(ctxt);
    if cur.is_null() {
        XP_ERROR!(ctxt, XmlXPathError::XpathInvalidOperand as i32);
    }
    cur = xml_xpath_cache_convert_boolean((*ctxt).context, cur);
    value_push(ctxt, cur);
}

/**
 * Really internal functions
 */
/**
 * xmlXPathNodeSetFreeNs:
 * @ns:  the XPath namespace node found in a nodeset.
 *
 * Namespace nodes in libxml don't match the XPath semantic. In a node set
 * the namespace nodes are duplicated and the next poc_inter is set to the
 * parent node in the XPath semantic. Check if such a node needs to be freed
 */
#[cfg(feature = "xpath")]
pub(crate) unsafe extern "C" fn xml_xpath_node_set_free_ns(ns: XmlNsPtr) {
    if ns.is_null() || !matches!((*ns).typ, Some(XmlElementType::XmlNamespaceDecl)) {
        return;
    }

    if !(*ns).next.load(Ordering::Relaxed).is_null()
        && !matches!(
            (*(*ns).next.load(Ordering::Relaxed)).typ,
            Some(XmlElementType::XmlNamespaceDecl)
        )
    {
        if !(*ns).href.load(Ordering::Relaxed).is_null() {
            xml_free((*ns).href.load(Ordering::Relaxed) as _);
        }
        if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
            xml_free((*ns).prefix.load(Ordering::Relaxed) as _);
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
                        let output = gen_file_ptr(n_output, 0);
                        let comp = gen_xml_xpath_comp_expr_ptr(n_comp, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_xpath_debug_dump_comp_expr(output, comp, depth);
                        des_file_ptr(n_output, output, 0);
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
                        let output = gen_file_ptr(n_output, 0);
                        let cur = gen_xml_xpath_object_ptr(n_cur, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_xpath_debug_dump_object(output, cur, depth);
                        des_file_ptr(n_output, output, 0);
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

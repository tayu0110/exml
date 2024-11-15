//! Provide methods and data structures for processing XPath.  
//! This module is based on `libxml/xpath.h`, `xpath.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    any::type_name,
    ffi::c_char,
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null_mut},
};

use libc::{memcpy, memmove, memset, snprintf, strlen, INT_MAX, INT_MIN};

#[cfg(not(feature = "thread"))]
use crate::libxml::xpath_internals::XML_XPATH_DISABLE_OPTIMIZER;
#[cfg(feature = "libxml_xptr_locs")]
use crate::libxml::xpointer::{
    xml_xptr_free_location_set, xml_xptr_location_set_merge, XmlLocationSetPtr,
};
use crate::{
    error::XmlError,
    generic_error,
    globals::{GenericErrorContext, StructuredError},
    libxml::{
        dict::{xml_dict_free, XmlDictPtr},
        globals::{xml_free, xml_malloc},
        hash::{xml_hash_create, XmlHashTablePtr},
        parser::xml_init_parser,
        pattern::{xml_free_pattern_list, XmlPatternPtr},
        xmlstring::{xml_strdup, xml_strlen, XmlChar},
        xpath_internals::{
            value_pop, xml_xpath_compile_expr, xml_xpath_err_memory, xml_xpath_eval_expr,
            xml_xpath_free_parser_context, xml_xpath_new_boolean, xml_xpath_new_cstring,
            xml_xpath_new_float, xml_xpath_new_parser_context, xml_xpath_node_set_dup_ns,
            xml_xpath_node_set_free_ns, xml_xpath_node_set_merge, xml_xpath_node_set_sort,
            xml_xpath_optimize_expression, xml_xpath_register_all_functions,
            xml_xpath_registered_funcs_cleanup, xml_xpath_registered_ns_cleanup,
            xml_xpath_registered_variables_cleanup, xml_xpath_release_object, xml_xpath_run_eval,
            xml_xpath_string_eval_number, xml_xpath_try_stream_compile, xml_xpath_wrap_string,
            xml_xpatherror, XmlPointerListPtr, XmlXpathContextCache, XmlXpathContextCachePtr,
            XML_NODESET_DEFAULT,
        },
    },
    tree::{xml_free_node_list, XmlDocPtr, XmlElementType, XmlNodePtr, XmlNsPtr},
};

/*
 * XPATH_MAX_STEPS:
 * when compiling an XPath expression we arbitrary limit the maximum
 * number of step operation in the compiled expression. 1000000 is
 * an insanely large value which should never be reached under normal
 * circumstances
 */
pub(crate) const XPATH_MAX_STEPS: usize = 1000000;

/*
 * XPATH_MAX_STACK_DEPTH:
 * when evaluating an XPath expression we arbitrary limit the maximum
 * number of object allowed to be pushed on the stack. 1000000 is
 * an insanely large value which should never be reached under normal
 * circumstances
 */
pub(crate) const XPATH_MAX_STACK_DEPTH: usize = 1000000;

/*
 * XPATH_MAX_NODESET_LENGTH:
 * when evaluating an XPath expression nodesets are created and we
 * arbitrary limit the maximum length of those node set. 10000000 is
 * an insanely large value which should never be reached under normal
 * circumstances, one would first need to construct an in memory tree
 * with more than 10 millions nodes.
 */
pub(crate) const XPATH_MAX_NODESET_LENGTH: usize = 10000000;

/**
 * The set of XPath error codes.
 */
#[cfg(feature = "xpath")]
#[repr(C)]
pub enum XmlXPathError {
    XpathExpressionOk = 0,
    XpathNumberError,
    XpathUnfinishedLiteralError,
    XpathStartLiteralError,
    XpathVariableRefError,
    XpathUndefVariableError,
    XpathInvalidPredicateError,
    XpathExprError,
    XpathUnclosedError,
    XpathUnknownFuncError,
    XpathInvalidOperand,
    XpathInvalidType,
    XpathInvalidArity,
    XpathInvalidCtxtSize,
    XpathInvalidCtxtPosition,
    XpathMemoryError,
    XptrSyntaxError,
    XptrResourceError,
    XptrSubResourceError,
    XpathUndefPrefixError,
    XpathEncodingError,
    XpathInvalidCharError,
    XpathInvalidCtxt,
    XpathStackError,
    XpathForbidVariableError,
    XpathOpLimitExceeded,
    XpathRecursionLimitExceeded,
}

/*
 * A node-set (an unordered collection of nodes without duplicates).
 */
#[cfg(feature = "xpath")]
pub type XmlNodeSetPtr = *mut XmlNodeSet;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlNodeSet {
    pub node_nr: i32,  /* number of nodes in the set */
    pub node_max: i32, /* size of the array as allocated */
    pub node_tab: *mut XmlNodePtr, /* array of nodes in no particular order */
                       /* @@ with_ns to check whether namespace nodes should be looked at @@ */
}

/*
 * An expression is evaluated to yield an object, which
 * has one of the following four basic types:
 *   - node-set
 *   - boolean
 *   - number
 *   - string
 *
 * @@ XPointer will add more types !
 */
#[cfg(feature = "xpath")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlXPathObjectType {
    XpathUndefined = 0,
    XpathNodeset = 1,
    XpathBoolean = 2,
    XpathNumber = 3,
    XpathString = 4,
    #[cfg(feature = "libxml_xptr_locs")]
    XpathPoint = 5,
    #[cfg(feature = "libxml_xptr_locs")]
    XpathRange = 6,
    #[cfg(feature = "libxml_xptr_locs")]
    XpathLocationset = 7,
    XpathUsers = 8,
    XpathXsltTree = 9, /* An XSLT value tree, non modifiable */
}

impl TryFrom<i32> for XmlXPathObjectType {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XpathUndefined as i32 {
            return Ok(Self::XpathUndefined);
        }
        if value == Self::XpathNodeset as i32 {
            return Ok(Self::XpathNodeset);
        }
        if value == Self::XpathBoolean as i32 {
            return Ok(Self::XpathBoolean);
        }
        if value == Self::XpathNumber as i32 {
            return Ok(Self::XpathNumber);
        }
        if value == Self::XpathString as i32 {
            return Ok(Self::XpathString);
        }
        #[cfg(feature = "libxml_xptr_locs")]
        if value == Self::XpathPoint as i32 {
            return Ok(Self::XpathPoint);
        }
        #[cfg(feature = "libxml_xptr_locs")]
        if value == Self::XpathRange as i32 {
            return Ok(Self::XpathRange);
        }
        #[cfg(feature = "libxml_xptr_locs")]
        if value == Self::XpathLocationset as i32 {
            return Ok(Self::XpathLocationset);
        }
        if value == Self::XpathUsers as i32 {
            return Ok(Self::XpathUsers);
        }
        if value == Self::XpathXsltTree as i32 {
            return Ok(Self::XpathXsltTree);
        }
        Err(anyhow::anyhow!(
            "Invalid convert from value '{value}' to {}",
            type_name::<Self>()
        ))
    }
}

#[cfg(all(feature = "libxml_xptr_locs", feature = "xpath"))]
const XPATH_POINT: usize = 5;
#[cfg(all(feature = "libxml_xptr_locs", feature = "xpath"))]
const XPATH_RANGE: usize = 6;
#[cfg(all(feature = "libxml_xptr_locs", feature = "xpath"))]
const XPATH_LOCATIONSET: usize = 7;

#[cfg(feature = "xpath")]
pub type XmlXPathObjectPtr = *mut XmlXPathObject;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathObject {
    pub typ: XmlXPathObjectType,
    pub nodesetval: XmlNodeSetPtr,
    pub boolval: i32,
    pub floatval: f64,
    pub stringval: *mut XmlChar,
    pub(crate) user: *mut c_void,
    pub(crate) index: i32,
    pub(crate) user2: *mut c_void,
    pub(crate) index2: i32,
}

/**
 * xmlXPathConvertFunc:
 * @obj:  an XPath object
 * @type:  the number of the target type
 *
 * A conversion function is associated to a type and used to cast
 * the new type to primitive values.
 *
 * Returns -1 in case of error, 0 otherwise
 */
#[cfg(feature = "xpath")]
pub type XmlXPathConvertFunc = unsafe extern "C" fn(obj: XmlXPathObjectPtr, typ: i32) -> i32;

/*
 * Extra type: a name and a conversion function.
 */

#[cfg(feature = "xpath")]
pub type XmlXPathTypePtr = *mut XmlXPathType;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathType {
    name: *const XmlChar,      /* the type name */
    func: XmlXPathConvertFunc, /* the conversion function */
}

/*
 * Extra variable: a name and a value.
 */

#[cfg(feature = "xpath")]
pub type XmlXPathVariablePtr = *mut XmlXPathVariable;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathVariable {
    name: *const XmlChar,     /* the variable name */
    value: XmlXPathObjectPtr, /* the value */
}

/**
 * xmlXPathEvalFunc:
 * @ctxt: an XPath parser context
 * @nargs: the number of arguments passed to the function
 *
 * An XPath evaluation function, the parameters are on the XPath context stack.
 */

#[cfg(feature = "xpath")]
pub type XmlXPathEvalFunc = unsafe extern "C" fn(ctxt: XmlXPathParserContextPtr, nargs: i32);

/*
 * Extra function: a name and a evaluation function.
 */

#[cfg(feature = "xpath")]
pub type XmlXPathFuncPtr = *mut XmlXPathFunct;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathFunct {
    name: *const XmlChar,   /* the function name */
    func: XmlXPathEvalFunc, /* the evaluation function */
}

/**
 * xmlXPathAxisFunc:
 * @ctxt:  the XPath interpreter context
 * @cur:  the previous node being explored on that axis
 *
 * An axis traversal function. To traverse an axis, the engine calls
 * the first time with cur == NULL and repeat until the function returns
 * NULL indicating the end of the axis traversal.
 *
 * Returns the next node in that axis or NULL if at the end of the axis.
 */

#[cfg(feature = "xpath")]
pub type XmlXPathAxisFunc = unsafe extern "C" fn(
    ctxt: XmlXPathParserContextPtr,
    cur: XmlXPathObjectPtr,
) -> XmlXPathObjectPtr;

/*
 * Extra axis: a name and an axis function.
 */

#[cfg(feature = "xpath")]
pub type XmlXPathAxisPtr = *mut XmlXPathAxis;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathAxis {
    name: *const XmlChar,   /* the axis name */
    func: XmlXPathAxisFunc, /* the search function */
}

/**
 * xmlXPathFunction:
 * @ctxt:  the XPath interprestation context
 * @nargs:  the number of arguments
 *
 * An XPath function.
 * The arguments (if any) are popped out from the context stack
 * and the result is pushed on the stack.
 */

#[cfg(feature = "xpath")]
pub type XmlXPathFunction = unsafe extern "C" fn(ctxt: XmlXPathParserContextPtr, nargs: i32);

/*
 * Function and Variable Lookup.
 */

/**
 * xmlXPathVariableLookupFunc:
 * @ctxt:  an XPath context
 * @name:  name of the variable
 * @ns_uri:  the namespace name hosting this variable
 *
 * Prototype for callbacks used to plug variable lookup in the XPath
 * engine.
 *
 * Returns the XPath object value or NULL if not found.
 */
#[cfg(feature = "xpath")]
pub type XmlXPathVariableLookupFunc = unsafe extern "C" fn(
    ctxt: *mut c_void,
    name: *const XmlChar,
    ns_uri: *const XmlChar,
) -> XmlXPathObjectPtr;

/**
 * xmlXPathFuncLookupFunc:
 * @ctxt:  an XPath context
 * @name:  name of the function
 * @ns_uri:  the namespace name hosting this function
 *
 * Prototype for callbacks used to plug function lookup in the XPath
 * engine.
 *
 * Returns the XPath function or NULL if not found.
 */
#[cfg(feature = "xpath")]
pub type XmlXPathFuncLookupFunc = unsafe extern "C" fn(
    ctxt: *mut c_void,
    name: *const XmlChar,
    ns_uri: *const XmlChar,
) -> Option<XmlXPathFunction>;

/**
 * xmlXPathFlags:
 * Flags for XPath engine compilation and runtime
 */
/**
 * XML_XPATH_CHECKNS:
 *
 * check namespaces at compilation
 */
#[cfg(feature = "xpath")]
pub(crate) const XML_XPATH_CHECKNS: usize = 1 << 0;
/**
 * XML_XPATH_NOVAR:
 *
 * forbid variables in expression
 */
#[cfg(feature = "xpath")]
pub(crate) const XML_XPATH_NOVAR: usize = 1 << 1;

/**
 * xmlXPathContext:
 *
 * Expression evaluation occurs with respect to a context.
 * he context consists of:
 *    - a node (the context node)
 *    - a node list (the context node list)
 *    - a set of variable bindings
 *    - a function library
 *    - the set of namespace declarations in scope for the expression
 *      Following the switch to hash tables, this need to be trimmed up at
 *      the next binary incompatible release.
 *      The node may be modified when the context is passed to libxml2
 *      for an XPath evaluation so you may need to initialize it again
 *      before the next call.
 */

#[cfg(feature = "xpath")]
pub type XmlXPathContextPtr = *mut XmlXPathContext;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathContext {
    pub doc: XmlDocPtr,   /* The current document */
    pub node: XmlNodePtr, /* The current node */

    pub(crate) nb_variables_unused: i32, /* unused (hash table) */
    pub(crate) max_variables_unused: i32, /* unused (hash table) */
    pub(crate) var_hash: XmlHashTablePtr, /* Hash table of defined variables */

    pub(crate) nb_types: i32,          /* number of defined types */
    pub(crate) max_types: i32,         /* max number of types */
    pub(crate) types: XmlXPathTypePtr, /* Array of defined types */

    pub(crate) nb_funcs_unused: i32,       /* unused (hash table) */
    pub(crate) max_funcs_unused: i32,      /* unused (hash table) */
    pub(crate) func_hash: XmlHashTablePtr, /* Hash table of defined funcs */

    pub(crate) nb_axis: i32,          /* number of defined axis */
    pub(crate) max_axis: i32,         /* max number of axis */
    pub(crate) axis: XmlXPathAxisPtr, /* Array of defined axis */

    /* the namespace nodes of the context node */
    pub(crate) namespaces: *mut XmlNsPtr, /* Array of namespaces */
    pub(crate) ns_nr: i32,                /* number of namespace in scope */
    pub(crate) user: *mut c_void,         /* function to free */

    /* extra variables */
    pub(crate) context_size: i32,       /* the context size */
    pub(crate) proximity_position: i32, /* the proximity position */

    /* extra stuff for XPointer */
    pub(crate) xptr: i32,          /* is this an XPointer context? */
    pub(crate) here: XmlNodePtr,   /* for here() */
    pub(crate) origin: XmlNodePtr, /* for origin() */

    /* the set of namespace declarations in scope for the expression */
    pub(crate) ns_hash: XmlHashTablePtr, /* The namespaces hash table */
    pub(crate) var_lookup_func: Option<XmlXPathVariableLookupFunc>, /* variable lookup func */
    pub(crate) var_lookup_data: *mut c_void, /* variable lookup data */

    /* Possibility to link in an extra item */
    pub(crate) extra: *mut c_void, /* needed for XSLT */

    /* The function name and URI when calling a function */
    pub(crate) function: *const XmlChar,
    pub(crate) function_uri: *const XmlChar,

    /* function lookup function and data */
    pub(crate) func_lookup_func: Option<XmlXPathFuncLookupFunc>, /* function lookup func */
    pub(crate) func_lookup_data: *mut c_void,                    /* function lookup data */

    /* temporary namespace lists kept for walking the namespace axis */
    pub(crate) tmp_ns_list: *mut XmlNsPtr, /* Array of namespaces */
    pub(crate) tmp_ns_nr: i32,             /* number of namespaces in scope */

    /* error reporting mechanism */
    pub(crate) user_data: Option<GenericErrorContext>, /* user specific data block */
    pub(crate) error: Option<StructuredError>,         /* the callback in case of errors */
    pub(crate) last_error: XmlError,                   /* the last error */
    pub(crate) debug_node: XmlNodePtr,                 /* the source node XSLT */

    /* dictionary */
    pub(crate) dict: XmlDictPtr, /* dictionary if any */

    pub(crate) flags: i32, /* flags to control compilation */

    /* Cache for reusal of XPath objects */
    pub cache: *mut c_void,

    /* Resource limits */
    pub(crate) op_limit: u64,
    pub(crate) op_count: u64,
    pub(crate) depth: i32,
}

/*
 * Types are private:
 */

#[cfg(feature = "xpath")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlXPathOp {
    XpathOpEnd = 0,
    XpathOpAnd,
    XpathOpOr,
    XpathOpEqual,
    XpathOpCmp,
    XpathOpPlus,
    XpathOpMult,
    XpathOpUnion,
    XpathOpRoot,
    XpathOpNode,
    XpathOpCollect,
    XpathOpValue, /* 11 */
    XpathOpVariable,
    XpathOpFunction,
    XpathOpArg,
    XpathOpPredicate,
    XpathOpFilter, /* 16 */
    XpathOpSort,   /* 17 */
    #[cfg(feature = "libxml_xptr_locs")]
    XpathOpRangeto,
}

#[cfg(feature = "xpath")]
pub type XmlXPathStepOpPtr = *mut XmlXPathStepOp;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathStepOp {
    pub(crate) op: XmlXPathOp, /* The identifier of the operation */
    pub(crate) ch1: i32,       /* First child */
    pub(crate) ch2: i32,       /* Second child */
    pub(crate) value: i32,
    pub(crate) value2: i32,
    pub(crate) value3: i32,
    pub(crate) value4: *mut c_void,
    pub(crate) value5: *mut c_void,
    pub(crate) cache: Option<XmlXPathFunction>,
    pub(crate) cache_uri: *mut c_void,
}

/*
 * The structure of a compiled expression form is not public.
 */

#[cfg(feature = "xpath")]
pub type XmlXPathCompExprPtr = *mut XmlXPathCompExpr;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathCompExpr {
    pub(crate) nb_step: i32,  /* Number of steps in this expression */
    pub(crate) max_step: i32, /* Maximum number of steps allocated */
    pub(crate) steps: *mut XmlXPathStepOp, /* ops for computation of this expression */
    pub(crate) last: i32,     /* index of last step in expression */
    pub(crate) expr: *mut XmlChar, /* the expression being computed */
    pub(crate) dict: XmlDictPtr, /* the dictionary to use if any */
    #[cfg(feature = "libxml_pattern")]
    pub(crate) stream: XmlPatternPtr,
}

/**
 * xmlXPathParserContext:
 *
 * An XPath parser context. It contains pure parsing information,
 * an xmlXPathContext, and the stack of objects.
 */
#[cfg(feature = "xpath")]
pub type XmlXPathParserContextPtr = *mut XmlXPathParserContext;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathParserContext {
    pub(crate) cur: *const XmlChar,  /* the current char being parsed */
    pub(crate) base: *const XmlChar, /* the full expression */

    pub(crate) error: i32, /* error code */

    pub(crate) context: XmlXPathContextPtr, /* the evaluation context */
    pub(crate) value: XmlXPathObjectPtr,    /* the current value */
    pub(crate) value_nr: i32,               /* number of values stacked */
    pub(crate) value_max: i32,              /* max number of values stacked */
    pub(crate) value_tab: *mut XmlXPathObjectPtr, /* stack of values */

    pub(crate) comp: XmlXPathCompExprPtr, /* the precompiled expression */
    pub(crate) xptr: i32,                 /* it this an XPointer expression */
    pub(crate) ancestor: XmlNodePtr,      /* used for walking preceding axis */

    pub(crate) value_frame: i32, /* unused */
}

/************************************************************************
 *									*
 *			Public API					*
 *									*
 ************************************************************************/

/**
 * Objects and Nodesets handling
 */

#[cfg(feature = "xpath")]
pub static mut XML_XPATH_NAN: f64 = 0.0;
#[cfg(feature = "xpath")]
pub static mut XML_XPATH_PINF: f64 = 0.0;
#[cfg(feature = "xpath")]
pub static mut XML_XPATH_NINF: f64 = 0.0;

/* These macros may later turn into functions */
/**
 * xmlXPathNodeSetGetLength:
 * @ns:  a node-set
 *
 * Implement a functionality similar to the DOM NodeList.length.
 *
 * Returns the number of nodes in the node-set.
 */
#[cfg(feature = "xpath")]
#[macro_export]
macro_rules! xmlXPathNodeSetGetLength {
    ($ns:expr) => {
        if !$ns.is_null() {
            (*$ns).node_nr
        } else {
            0
        }
    };
}
/**
 * xmlXPathNodeSetItem:
 * @ns:  a node-set
 * @index:  index of a node in the set
 *
 * Implements a functionality similar to the DOM NodeList.item().
 *
 * Returns the xmlNodePtr at the given @index in @ns or NULL if
 *         @index is out of range (0 to length-1)
 */
#[cfg(feature = "xpath")]
#[macro_export]
macro_rules! xmlXPathNodeSetItem {
    ($ns:expr, $index:expr) => {
        if !$ns.is_null() && $index >= 0 && ($index) < (*$ns).node_nr {
            *(*$ns).node_tab.add($index as usize)
        } else {
            null_mut()
        }
    };
}
/**
 * xmlXPathNodeSetIsEmpty:
 * @ns: a node-set
 *
 * Checks whether @ns is empty or not.
 *
 * Returns %TRUE if @ns is an empty node-set.
 */
#[cfg(feature = "xpath")]
#[macro_export]
macro_rules! xmlXPathNodeSetIsEmpty {
    ($ns:expr) => {
        $ns.is_null() || (*$ns).node_nr == 0 || (*$ns).node_tab.is_null()
    };
}

/**
 * xmlXPathFreeValueTree:
 * @obj:  the xmlNodeSetPtr to free
 *
 * Free the NodeSet compound and the actual tree, this is different
 * from xmlXPathFreeNodeSet()
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_free_value_tree(obj: XmlNodeSetPtr) {
    if obj.is_null() {
        return;
    }

    if !(*obj).node_tab.is_null() {
        for i in 0..(*obj).node_nr {
            if !(*(*obj).node_tab.add(i as usize)).is_null() {
                if matches!(
                    (*(*(*obj).node_tab.add(i as usize))).typ,
                    XmlElementType::XmlNamespaceDecl
                ) {
                    xml_xpath_node_set_free_ns(*(*obj).node_tab.add(i as usize) as XmlNsPtr);
                } else {
                    xml_free_node_list(*(*obj).node_tab.add(i as usize));
                }
            }
        }
        xml_free((*obj).node_tab as _);
    }
    xml_free(obj as _);
}

/**
 * xmlXPathFreeObject:
 * @obj:  the object to free
 *
 * Free up an object: xmlXPathObjectPtr.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_free_object(obj: XmlXPathObjectPtr) {
    if obj.is_null() {
        return;
    }
    if matches!(
        (*obj).typ,
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree
    ) {
        if (*obj).boolval != 0 {
            (*obj).typ = XmlXPathObjectType::XpathXsltTree; /* TODO: Just for debugging. */
            if !(*obj).nodesetval.is_null() {
                xml_xpath_free_value_tree((*obj).nodesetval);
            }
        } else if !(*obj).nodesetval.is_null() {
            xml_xpath_free_node_set((*obj).nodesetval);
        }
    } else if matches!((*obj).typ, XmlXPathObjectType::XpathString) && !(*obj).stringval.is_null() {
        xml_free((*obj).stringval as _);
    } else {
        #[cfg(feature = "libxml_xptr_locs")]
        if (*obj).typ as usize == XPATH_LOCATIONSET && !(*obj).user.is_null() {
            xml_xptr_free_location_set((*obj).user as _);
        }
    }
    xml_free(obj as _);
}

/**
 * xmlXPathNodeSetCreate:
 * @val:  an initial xmlNodePtr, or NULL
 *
 * Create a new xmlNodeSetPtr of type f64 and of value @val
 *
 * Returns the newly created object.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_node_set_create(val: XmlNodePtr) -> XmlNodeSetPtr {
    let ret: XmlNodeSetPtr = xml_malloc(size_of::<XmlNodeSet>()) as XmlNodeSetPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), c"creating nodeset\n".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlNodeSet>());
    if !val.is_null() {
        (*ret).node_tab =
            xml_malloc(XML_NODESET_DEFAULT * size_of::<XmlNodePtr>()) as *mut XmlNodePtr;
        if (*ret).node_tab.is_null() {
            xml_xpath_err_memory(null_mut(), c"creating nodeset\n".as_ptr() as _);
            xml_free(ret as _);
            return null_mut();
        }
        memset(
            (*ret).node_tab as _,
            0,
            XML_NODESET_DEFAULT * size_of::<XmlNodePtr>(),
        );
        (*ret).node_max = XML_NODESET_DEFAULT as _;
        if matches!((*val).typ, XmlElementType::XmlNamespaceDecl) {
            let ns: XmlNsPtr = val as XmlNsPtr;
            let ns_node: XmlNodePtr = xml_xpath_node_set_dup_ns((*ns).next as XmlNodePtr, ns);

            if ns_node.is_null() {
                xml_xpath_free_node_set(ret);
                return null_mut();
            }
            *(*ret).node_tab.add((*ret).node_nr as usize) = ns_node;
            (*ret).node_nr += 1;
        } else {
            *(*ret).node_tab.add((*ret).node_nr as usize) = val;
            (*ret).node_nr += 1;
        }
    }
    ret
}

/**
 * xmlXPathFreeNodeSetList:
 * @obj:  an existing NodeSetList object
 *
 * Free up the xmlXPathObjectPtr @obj but don't deallocate the objects in
 * the list contrary to xmlXPathFreeObject().
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_free_node_set_list(obj: XmlXPathObjectPtr) {
    if obj.is_null() {
        return;
    }
    xml_free(obj as _);
}

/**
 * xmlXPathFreeNodeSet:
 * @obj:  the xmlNodeSetPtr to free
 *
 * Free the NodeSet compound (not the actual nodes !).
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_free_node_set(obj: XmlNodeSetPtr) {
    if obj.is_null() {
        return;
    }
    if !(*obj).node_tab.is_null() {
        /* @@ with_ns to check whether namespace nodes should be looked at @@ */
        for i in 0..(*obj).node_nr {
            if !(*(*obj).node_tab.add(i as usize)).is_null()
                && matches!(
                    (*(*(*obj).node_tab.add(i as usize))).typ,
                    XmlElementType::XmlNamespaceDecl
                )
            {
                xml_xpath_node_set_free_ns(*(*obj).node_tab.add(i as usize) as XmlNsPtr);
            }
        }
        xml_free((*obj).node_tab as _);
    }
    xml_free(obj as _);
}

/**
 * xmlXPathObjectCopy:
 * @val:  the original object
 *
 * allocate a new copy of a given object
 *
 * Returns the newly created object.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_object_copy(val: XmlXPathObjectPtr) -> XmlXPathObjectPtr {
    use crate::generic_error;

    if val.is_null() {
        return null_mut();
    }

    let ret: XmlXPathObjectPtr = xml_malloc(size_of::<XmlXPathObject>()) as XmlXPathObjectPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), c"copying object\n".as_ptr());
        return null_mut();
    }
    memcpy(ret as _, val as _, size_of::<XmlXPathObject>());
    match (*val).typ {
        XmlXPathObjectType::XpathBoolean | XmlXPathObjectType::XpathNumber => {}
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XpathPoint | XmlXPathObjectType::XpathRange => {}
        XmlXPathObjectType::XpathString => {
            (*ret).stringval = xml_strdup((*val).stringval);
            if (*ret).stringval.is_null() {
                xml_free(ret as _);
                return null_mut();
            }
        }
        XmlXPathObjectType::XpathXsltTree | XmlXPathObjectType::XpathNodeset => {
            /* TODO: Check memory error. */
            (*ret).nodesetval = xml_xpath_node_set_merge(null_mut(), (*val).nodesetval);
            /* Do not deallocate the copied tree value */
            (*ret).boolval = 0;
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XpathLocationset => {
            let loc: XmlLocationSetPtr = (*val).user as _;
            (*ret).user = xml_xptr_location_set_merge(null_mut(), loc) as *mut c_void;
        }
        XmlXPathObjectType::XpathUsers => {
            (*ret).user = (*val).user;
        }
        XmlXPathObjectType::XpathUndefined => {
            generic_error!(
                "xmlXPathObjectCopy: unsupported type {}\n",
                (*val).typ as i32
            );
        } // _ => {}
    }
    ret
}

/**
 * xmlXPathCmpNodes:
 * @node1:  the first node
 * @node2:  the second node
 *
 * Compare two nodes w.r.t document order
 *
 * Returns -2 in case of error 1 if first point < second point, 0 if
 *         it's the same node, -1 otherwise
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cmp_nodes(mut node1: XmlNodePtr, mut node2: XmlNodePtr) -> i32 {
    use crate::tree::NodePtr;

    let mut depth1: i32;
    let mut depth2: i32;
    let mut attr1: i32 = 0;
    let mut attr2: i32 = 0;
    let mut attr_node1: XmlNodePtr = null_mut();
    let mut attr_node2: XmlNodePtr = null_mut();
    let mut cur: XmlNodePtr;

    if node1.is_null() || node2.is_null() {
        return -2;
    }
    /*
     * a couple of optimizations which will avoid computations in most cases
     */
    /* trivial case */
    if node1 == node2 {
        return 0;
    }
    if matches!((*node1).typ, XmlElementType::XmlAttributeNode) {
        attr1 = 1;
        attr_node1 = node1;
        node1 = (*node1).parent.map_or(null_mut(), |p| p.as_ptr());
    }
    if matches!((*node2).typ, XmlElementType::XmlAttributeNode) {
        attr2 = 1;
        attr_node2 = node2;
        node2 = (*node2).parent.map_or(null_mut(), |p| p.as_ptr());
    }
    if node1 == node2 {
        if attr1 == attr2 {
            /* not required, but we keep attributes in order */
            if attr1 != 0 {
                cur = (*attr_node2).prev;
                while !cur.is_null() {
                    if cur == attr_node1 {
                        return 1;
                    }
                    cur = (*cur).prev;
                }
                return -1;
            }
            return 0;
        }
        if attr2 == 1 {
            return 1;
        }
        return -1;
    }
    if matches!((*node1).typ, XmlElementType::XmlNamespaceDecl)
        || matches!((*node2).typ, XmlElementType::XmlNamespaceDecl)
    {
        return 1;
    }
    if node1 == (*node2).prev {
        return 1;
    }
    if NodePtr::from_ptr(node1) == (*node2).next {
        return -1;
    }

    /*
     * Speedup using document order if available.
     */
    if matches!((*node1).typ, XmlElementType::XmlElementNode)
        && matches!((*node2).typ, XmlElementType::XmlElementNode)
        && 0 > (*node1).content as isize
        && 0 > (*node2).content as isize
        && (*node1).doc == (*node2).doc
    {
        let l1: isize = -((*node1).content as isize);
        let l2: isize = -((*node2).content as isize);
        if l1 < l2 {
            return 1;
        }
        if l1 > l2 {
            return -1;
        }
    }

    /*
     * compute depth to root
     */
    depth2 = 0;
    cur = node2;
    while let Some(parent) = (*cur).parent {
        if parent.as_ptr() == node1 {
            return 1;
        }
        depth2 += 1;
        cur = parent.as_ptr();
    }
    let root: XmlNodePtr = cur;

    depth1 = 0;
    cur = node1;
    while let Some(parent) = (*cur).parent {
        if parent.as_ptr() == node2 {
            return -1;
        }
        depth1 += 1;
        cur = parent.as_ptr();
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
        node1 = (*node1).parent.map_or(null_mut(), |p| p.as_ptr());
    }
    while depth2 > depth1 {
        depth2 -= 1;
        node2 = (*node2).parent.map_or(null_mut(), |p| p.as_ptr());
    }
    while (*node1).parent != (*node2).parent {
        node1 = (*node1).parent.map_or(null_mut(), |p| p.as_ptr());
        node2 = (*node2).parent.map_or(null_mut(), |p| p.as_ptr());
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
    if NodePtr::from_ptr(node1) == (*node2).next {
        return -1;
    }
    /*
     * Speedup using document order if available.
     */
    if matches!((*node1).typ, XmlElementType::XmlElementNode)
        && matches!((*node2).typ, XmlElementType::XmlElementNode)
        && 0 > (*node1).content as isize
        && 0 > (*node2).content as isize
        && (*node1).doc == (*node2).doc
    {
        let l1: isize = -((*node1).content as isize);
        let l2: isize = -((*node2).content as isize);
        if l1 < l2 {
            return 1;
        }
        if l1 > l2 {
            return -1;
        }
    }

    let mut cur = (*node1).next;
    while let Some(now) = cur {
        if now.as_ptr() == node2 {
            return 1;
        }
        cur = now.next;
    }
    -1 /* assume there is no sibling list corruption */
}

/**
 * Conversion functions to basic types.
 */
/**
 * xmlXPathCastNumberToBoolean:
 * @val:  a number
 *
 * Converts a number to its boolean value
 *
 * Returns the boolean value
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_number_to_boolean(val: f64) -> i32 {
    if xml_xpath_is_nan(val) != 0 || val == 0.0 {
        return 0;
    }
    1
}

/**
 * xmlXPathCastStringToBoolean:
 * @val:  a string
 *
 * Converts a string to its boolean value
 *
 * Returns the boolean value
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_string_to_boolean(val: *const XmlChar) -> i32 {
    if val.is_null() || xml_strlen(val) == 0 {
        return 0;
    }
    1
}

/**
 * xmlXPathCastNodeSetToBoolean:
 * @ns:  a node-set
 *
 * Converts a node-set to its boolean value
 *
 * Returns the boolean value
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_node_set_to_boolean(ns: XmlNodeSetPtr) -> i32 {
    if ns.is_null() || (*ns).node_nr == 0 {
        return 0;
    }
    1
}

/**
 * xmlXPathCastToBoolean:
 * @val:  an XPath object
 *
 * Converts an XPath object to its boolean value
 *
 * Returns the boolean value
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_to_boolean(val: XmlXPathObjectPtr) -> i32 {
    if val.is_null() {
        return 0;
    }
    match (*val).typ {
        XmlXPathObjectType::XpathUndefined => 0,
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {
            xml_xpath_cast_node_set_to_boolean((*val).nodesetval)
        }
        XmlXPathObjectType::XpathString => xml_xpath_cast_string_to_boolean((*val).stringval),
        XmlXPathObjectType::XpathNumber => xml_xpath_cast_number_to_boolean((*val).floatval),
        XmlXPathObjectType::XpathBoolean => (*val).boolval,
        XmlXPathObjectType::XpathUsers => {
            // todo!();
            0
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XpathPoint
        | XmlXPathObjectType::XpathRange
        | XmlXPathObjectType::XpathLocationset => {
            // todo!();
            0
        }
    }
}

/**
 * xmlXPathCastBooleanToNumber:
 * @val:  a boolean
 *
 * Converts a boolean to its number value
 *
 * Returns the number value
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_boolean_to_number(val: i32) -> f64 {
    if val != 0 {
        return 1.0;
    }
    0.0
}

/**
 * xmlXPathCastStringToNumber:
 * @val:  a string
 *
 * Converts a string to its number value
 *
 * Returns the number value
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_string_to_number(val: *const XmlChar) -> f64 {
    xml_xpath_string_eval_number(val)
}

/**
 * xmlXPathCastNodeToNumber:
 * @node:  a node
 *
 * Converts a node to its number value
 *
 * Returns the number value
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_node_to_number(node: XmlNodePtr) -> f64 {
    if node.is_null() {
        return XML_XPATH_NAN;
    }
    let strval: *mut XmlChar = xml_xpath_cast_node_to_string(node);
    if strval.is_null() {
        return XML_XPATH_NAN;
    }
    let ret: f64 = xml_xpath_cast_string_to_number(strval);
    xml_free(strval as _);

    ret
}

/**
 * xmlXPathCastNodeSetToNumber:
 * @ns:  a node-set
 *
 * Converts a node-set to its number value
 *
 * Returns the number value
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_node_set_to_number(ns: XmlNodeSetPtr) -> f64 {
    if ns.is_null() {
        return XML_XPATH_NAN;
    }
    let str: *mut XmlChar = xml_xpath_cast_node_set_to_string(ns);
    let ret: f64 = xml_xpath_cast_string_to_number(str);
    xml_free(str as _);
    ret
}

/**
 * xmlXPathCastToNumber:
 * @val:  an XPath object
 *
 * Converts an XPath object to its number value
 *
 * Returns the number value
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_to_number(val: XmlXPathObjectPtr) -> f64 {
    if val.is_null() {
        return XML_XPATH_NAN;
    }
    match (*val).typ {
        XmlXPathObjectType::XpathUndefined => XML_XPATH_NAN,
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {
            xml_xpath_cast_node_set_to_number((*val).nodesetval)
        }
        XmlXPathObjectType::XpathString => xml_xpath_cast_string_to_number((*val).stringval),
        XmlXPathObjectType::XpathNumber => (*val).floatval,
        XmlXPathObjectType::XpathBoolean => xml_xpath_cast_boolean_to_number((*val).boolval),
        XmlXPathObjectType::XpathUsers => {
            // todo!();
            XML_XPATH_NAN
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XpathPoint
        | XmlXPathObjectType::XpathRange
        | XmlXPathObjectType::XpathLocationset => {
            // todo!();
            XML_XPATH_NAN
        }
    }
}

/**
 * xmlXPathCastBooleanToString:
 * @val:  a boolean
 *
 * Converts a boolean to its string value.
 *
 * Returns a newly allocated string.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_boolean_to_string(val: i32) -> *mut XmlChar {
    if val != 0 {
        xml_strdup(c"true".as_ptr() as *const XmlChar)
    } else {
        xml_strdup(c"false".as_ptr() as *const XmlChar)
    }
}

const DBL_DIG: usize = 15;
const EXPONENT_DIGITS: usize = 3 + 2;
const LOWER_DOUBLE_EXP: usize = 5;
const UPPER_DOUBLE: f64 = 1E9;
const LOWER_DOUBLE: f64 = 1E-5;

/**
 * xmlXPathFormatNumber:
 * @number:     number to format
 * @buffer:     output buffer
 * @buffersize: size of output buffer
 *
 * Convert the number into a string representation.
 */
unsafe extern "C" fn xml_xpath_format_number(number: f64, buffer: *mut c_char, buffersize: i32) {
    match xml_xpath_is_inf(number) {
        1 => {
            if buffersize > "Infinity".len() as i32 + 1 {
                snprintf(buffer, buffersize as usize, c"Infinity".as_ptr() as _);
            }
        }
        -1 => {
            if buffersize > "-Infinity".len() as i32 + 1 {
                snprintf(buffer, buffersize as usize, c"-Infinity".as_ptr() as _);
            }
        }
        _ => {
            if xml_xpath_is_nan(number) != 0 {
                if buffersize > "NaN".len() as i32 + 1 {
                    snprintf(buffer, buffersize as usize, c"NaN".as_ptr() as _);
                }
            } else if number == 0.0 {
                /* Omit sign for negative zero. */
                snprintf(buffer, buffersize as usize, c"0".as_ptr() as _);
            } else if number > INT_MIN as f64
                && number < INT_MAX as f64
                && number == number as i32 as f64
            {
                let mut work: [c_char; 30] = [0; 30];
                let mut ptr: *mut c_char;
                let mut cur: *mut c_char;
                let value: i32 = number as i32;

                ptr = buffer.add(0);
                if value == 0 {
                    *ptr = b'0' as _;
                    ptr = ptr.add(1);
                } else {
                    snprintf(work.as_mut_ptr() as _, 29, c"%d".as_ptr(), value);
                    cur = work.as_mut_ptr();
                    while *cur != 0 && ptr.offset_from(buffer) < buffersize as isize {
                        *ptr = *cur;
                        ptr = ptr.add(1);
                        cur = cur.add(1);
                    }
                }
                if ptr.offset_from(buffer) < buffersize as isize {
                    *ptr = 0;
                } else if buffersize > 0 {
                    ptr = ptr.sub(1);
                    *ptr = 0;
                }
            } else {
                /*
                  For the dimension of work,
                      DBL_DIG is number of significant digits
                  EXPONENT is only needed for "scientific notation"
                      3 is sign, decimal point, and terminating zero
                  LOWER_DOUBLE_EXP is max number of leading zeroes in fraction
                  Note that this dimension is slightly (a few characters)
                  larger than actually necessary.
                */
                let mut work: [c_char; DBL_DIG + EXPONENT_DIGITS + 3 + LOWER_DOUBLE_EXP] =
                    [0; DBL_DIG + EXPONENT_DIGITS + 3 + LOWER_DOUBLE_EXP];
                let integer_place: i32;
                let fraction_place: i32;
                let mut ptr: *mut c_char;
                let mut after_fraction: *mut c_char;
                let mut size: i32;
                let absolute_value: f64 = number.abs();

                /*
                 * First choose format - scientific or regular floating point.
                 * In either case, result is in work, and after_fraction points
                 * just past the fractional part.
                 */
                if !(LOWER_DOUBLE..=UPPER_DOUBLE).contains(&absolute_value) && absolute_value != 0.0
                {
                    /* Use scientific notation */
                    integer_place = (DBL_DIG + EXPONENT_DIGITS + 1) as i32;
                    fraction_place = DBL_DIG as i32 - 1;
                    size = snprintf(
                        work.as_mut_ptr() as _,
                        work.len(),
                        c"%*.*e".as_ptr() as _,
                        integer_place,
                        fraction_place,
                        number,
                    );
                    while size > 0 && work[size as usize] != b'e' as _ {
                        size -= 1;
                    }
                } else {
                    /* Use regular notation */
                    if absolute_value > 0.0 {
                        integer_place = absolute_value.log10() as i32;
                        if integer_place > 0 {
                            fraction_place = DBL_DIG as i32 - integer_place - 1;
                        } else {
                            fraction_place = DBL_DIG as i32 - integer_place;
                        }
                    } else {
                        fraction_place = 1;
                    }
                    size = snprintf(
                        work.as_mut_ptr() as _,
                        work.len(),
                        c"%0.*f".as_ptr() as _,
                        fraction_place,
                        number,
                    );
                }

                /* Remove leading spaces sometimes inserted by snprintf */
                while work[0] == b' ' as _ {
                    ptr = work.as_mut_ptr();
                    while {
                        *ptr.add(0) = *ptr.add(1);
                        *ptr.add(0) != 0
                    } {
                        ptr = ptr.add(1);
                    }
                    size -= 1;
                }

                /* Remove fractional trailing zeroes */
                after_fraction = work.as_mut_ptr().add(size as usize);
                ptr = after_fraction;
                while {
                    ptr = ptr.sub(1);
                    *ptr == b'0' as _
                } {}
                if *ptr != b'.' as _ {
                    ptr = ptr.add(1);
                }
                while {
                    *ptr = *after_fraction;
                    let res = *ptr;
                    ptr = ptr.add(1);
                    after_fraction = after_fraction.add(1);
                    res != 0
                } {}

                /* Finally copy result back to caller */
                size = strlen(work.as_ptr() as _) as i32 + 1;
                if size > buffersize {
                    work[buffersize as usize - 1] = 0;
                    size = buffersize;
                }
                memmove(buffer as _, work.as_ptr() as _, size as usize);
            }
        }
    }
}

/**
 * xmlXPathCastNumberToString:
 * @val:  a number
 *
 * Converts a number to its string value.
 *
 * Returns a newly allocated string.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_number_to_string(val: f64) -> *mut XmlChar {
    let ret: *mut XmlChar;
    match xml_xpath_is_inf(val) {
        1 => {
            ret = xml_strdup(c"Infinity".as_ptr() as *const XmlChar);
        }
        -1 => {
            ret = xml_strdup(c"-Infinity".as_ptr() as *const XmlChar);
        }
        _ => {
            if xml_xpath_is_nan(val) != 0 {
                ret = xml_strdup(c"NaN".as_ptr() as *const XmlChar);
            } else if val == 0.0 {
                /* Omit sign for negative zero. */
                ret = xml_strdup(c"0".as_ptr() as *const XmlChar);
            } else {
                /* could be improved */
                let mut buf: [c_char; 100] = [0; 100];
                xml_xpath_format_number(val, buf.as_mut_ptr(), 99);
                buf[99] = 0;
                ret = xml_strdup(buf.as_ptr() as *const XmlChar);
            }
        }
    }
    ret
}

/**
 * xmlXPathCastNodeToString:
 * @node:  a node
 *
 * Converts a node to its string value.
 *
 * Returns a newly allocated string.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_node_to_string(node: XmlNodePtr) -> *mut XmlChar {
    let mut ret: *mut XmlChar = if node.is_null() {
        null_mut()
    } else {
        (*node).get_content()
    };
    if ret.is_null() {
        ret = xml_strdup(c"".as_ptr() as *const XmlChar);
    }
    ret
}

/**
 * xmlXPathCastNodeSetToString:
 * @ns:  a node-set
 *
 * Converts a node-set to its string value.
 *
 * Returns a newly allocated string.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_node_set_to_string(ns: XmlNodeSetPtr) -> *mut XmlChar {
    if ns.is_null() || (*ns).node_nr == 0 || (*ns).node_tab.is_null() {
        return xml_strdup(c"".as_ptr() as *const XmlChar);
    }

    if (*ns).node_nr > 1 {
        xml_xpath_node_set_sort(ns);
    }
    xml_xpath_cast_node_to_string(*(*ns).node_tab.add(0))
}

/**
 * xmlXPathCastToString:
 * @val:  an XPath object
 *
 * Converts an existing object to its string() equivalent
 *
 * Returns the allocated string value of the object, NULL in case of error.
 *         It's up to the caller to free the string memory with xmlFree( as _).
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_cast_to_string(val: XmlXPathObjectPtr) -> *mut XmlChar {
    if val.is_null() {
        return xml_strdup(c"".as_ptr() as *const XmlChar);
    }
    match (*val).typ {
        XmlXPathObjectType::XpathUndefined => xml_strdup(c"".as_ptr() as *const XmlChar),
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {
            xml_xpath_cast_node_set_to_string((*val).nodesetval)
        }
        XmlXPathObjectType::XpathString => xml_strdup((*val).stringval),
        XmlXPathObjectType::XpathBoolean => xml_xpath_cast_boolean_to_string((*val).boolval),
        XmlXPathObjectType::XpathNumber => xml_xpath_cast_number_to_string((*val).floatval),
        XmlXPathObjectType::XpathUsers => {
            // todo!();
            xml_strdup(c"".as_ptr() as *const XmlChar)
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XpathPoint
        | XmlXPathObjectType::XpathRange
        | XmlXPathObjectType::XpathLocationset => {
            // todo!();
            xml_strdup(c"".as_ptr() as *const XmlChar)
        }
    }
}

/**
 * xmlXPathConvertBoolean:
 * @val:  an XPath object
 *
 * Converts an existing object to its boolean() equivalent
 *
 * Returns the new object, the old one is freed (or the operation
 *         is done directly on @val)
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_convert_boolean(val: XmlXPathObjectPtr) -> XmlXPathObjectPtr {
    if val.is_null() {
        return xml_xpath_new_boolean(0);
    }
    if (*val).typ == XmlXPathObjectType::XpathBoolean {
        return val;
    }
    let ret: XmlXPathObjectPtr = xml_xpath_new_boolean(xml_xpath_cast_to_boolean(val));
    xml_xpath_free_object(val);
    ret
}

/**
 * xmlXPathConvertNumber:
 * @val:  an XPath object
 *
 * Converts an existing object to its number() equivalent
 *
 * Returns the new object, the old one is freed (or the operation
 *         is done directly on @val)
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_convert_number(val: XmlXPathObjectPtr) -> XmlXPathObjectPtr {
    if val.is_null() {
        return xml_xpath_new_float(0.0);
    }
    if (*val).typ == XmlXPathObjectType::XpathNumber {
        return val;
    }
    let ret: XmlXPathObjectPtr = xml_xpath_new_float(xml_xpath_cast_to_number(val));
    xml_xpath_free_object(val);
    ret
}

/**
 * xmlXPathConvertString:
 * @val:  an XPath object
 *
 * Converts an existing object to its string() equivalent
 *
 * Returns the new object, the old one is freed (or the operation
 *         is done directly on @val)
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_convert_string(val: XmlXPathObjectPtr) -> XmlXPathObjectPtr {
    let mut res: *mut XmlChar = null_mut();

    if val.is_null() {
        return xml_xpath_new_cstring(c"".as_ptr() as _);
    }

    match (*val).typ {
        XmlXPathObjectType::XpathUndefined => {}
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
            // todo!()
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XpathPoint
        | XmlXPathObjectType::XpathRange
        | XmlXPathObjectType::XpathLocationset => {
            // todo!()
        }
    }
    xml_xpath_free_object(val);
    if res.is_null() {
        return xml_xpath_new_cstring(c"".as_ptr() as _);
    }
    xml_xpath_wrap_string(res)
}

/**
 * Context handling.
 */
/**
 * xmlXPathNewContext:
 * @doc:  the XML document
 *
 * Create a new xmlXPathContext
 *
 * Returns the xmlXPathContext just allocated. The caller will need to free it.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_new_context(doc: XmlDocPtr) -> XmlXPathContextPtr {
    let ret: XmlXPathContextPtr = xml_malloc(size_of::<XmlXPathContext>()) as XmlXPathContextPtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), c"creating context\n".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathContext>());
    (*ret).doc = doc;
    (*ret).node = null_mut();

    (*ret).var_hash = null_mut();

    (*ret).nb_types = 0;
    (*ret).max_types = 0;
    (*ret).types = null_mut();

    (*ret).func_hash = xml_hash_create(0);

    (*ret).nb_axis = 0;
    (*ret).max_axis = 0;
    (*ret).axis = null_mut();

    (*ret).ns_hash = null_mut();
    (*ret).user = null_mut();

    (*ret).context_size = -1;
    (*ret).proximity_position = -1;

    // #ifdef XP_DEFAULT_CACHE_ON
    if xml_xpath_context_set_cache(ret, 1, -1, 0) == -1 {
        xml_xpath_free_context(ret);
        return null_mut();
    }
    // #endif

    xml_xpath_register_all_functions(ret);

    ret
}

/**
 * xsltPointerListFree:
 *
 * Frees the xsltPointerList structure. This does not free
 * the content of the list.
 */
unsafe extern "C" fn xml_pointer_list_free(list: XmlPointerListPtr) {
    if list.is_null() {
        return;
    }
    if !(*list).items.is_null() {
        xml_free((*list).items as _);
    }
    xml_free(list as _);
}

unsafe extern "C" fn xml_xpath_cache_free_object_list(list: XmlPointerListPtr) {
    let mut obj: XmlXPathObjectPtr;

    if list.is_null() {
        return;
    }

    for i in 0..(*list).number {
        obj = *(*list).items.add(i as usize) as _;
        /*
         * Note that it is already assured that we don't need to
         * look out for namespace nodes in the node-set.
         */
        if !(*obj).nodesetval.is_null() {
            if !(*(*obj).nodesetval).node_tab.is_null() {
                xml_free((*(*obj).nodesetval).node_tab as _);
            }
            xml_free((*obj).nodesetval as _);
        }
        xml_free(obj as _);
    }
    xml_pointer_list_free(list);
}

unsafe extern "C" fn xml_xpath_free_cache(cache: XmlXpathContextCachePtr) {
    if cache.is_null() {
        return;
    }
    if !(*cache).nodeset_objs.is_null() {
        xml_xpath_cache_free_object_list((*cache).nodeset_objs);
    }
    if !(*cache).string_objs.is_null() {
        xml_xpath_cache_free_object_list((*cache).string_objs);
    }
    if !(*cache).boolean_objs.is_null() {
        xml_xpath_cache_free_object_list((*cache).boolean_objs);
    }
    if !(*cache).number_objs.is_null() {
        xml_xpath_cache_free_object_list((*cache).number_objs);
    }
    if !(*cache).misc_objs.is_null() {
        xml_xpath_cache_free_object_list((*cache).misc_objs);
    }
    xml_free(cache as _);
}

/**
 * xmlXPathFreeContext:
 * @ctxt:  the context to free
 *
 * Free up an xmlXPathContext
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_free_context(ctxt: XmlXPathContextPtr) {
    if ctxt.is_null() {
        return;
    }

    if !(*ctxt).cache.is_null() {
        xml_xpath_free_cache((*ctxt).cache as XmlXpathContextCachePtr);
    }
    xml_xpath_registered_ns_cleanup(ctxt);
    xml_xpath_registered_funcs_cleanup(ctxt);
    xml_xpath_registered_variables_cleanup(ctxt);
    (*ctxt).last_error.reset();
    xml_free(ctxt as _);
}

/**
 * xmlXPathNewCache:
 *
 * Create a new object cache
 *
 * Returns the xmlXPathCache just allocated.
 */
unsafe extern "C" fn xml_xpath_new_cache() -> XmlXpathContextCachePtr {
    let ret: XmlXpathContextCachePtr =
        xml_malloc(size_of::<XmlXpathContextCache>()) as XmlXpathContextCachePtr;
    if ret.is_null() {
        xml_xpath_err_memory(null_mut(), c"creating object cache\n".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXpathContextCache>());
    (*ret).max_nodeset = 100;
    (*ret).max_string = 100;
    (*ret).max_boolean = 100;
    (*ret).max_number = 100;
    (*ret).max_misc = 100;
    ret
}

/**
 * xmlXPathContextSetCache:
 *
 * @ctxt:  the XPath context
 * @active: enables/disables (creates/frees) the cache
 * @value: a value with semantics dependent on @options
 * @options: options (currently only the value 0 is used)
 *
 * Creates/frees an object cache on the XPath context.
 * If activates XPath objects (xmlXPathObject) will be cached internally
 * to be reused.
 * @options:
 *   0: This will set the XPath object caching:
 *      @value:
 *        This will set the maximum number of XPath objects
 *        to be cached per slot
 *        There are 5 slots for: node-set, string, number, boolean, and
 *        misc objects. Use <0 for the default number (100).
 *   Other values for @options have currently no effect.
 *
 * Returns 0 if the setting succeeded, and -1 on API or internal errors.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_context_set_cache(
    ctxt: XmlXPathContextPtr,
    active: i32,
    mut value: i32,
    options: i32,
) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    if active != 0 {
        if (*ctxt).cache.is_null() {
            (*ctxt).cache = xml_xpath_new_cache() as _;
            if (*ctxt).cache.is_null() {
                return -1;
            }
        }
        let cache: XmlXpathContextCachePtr = (*ctxt).cache as XmlXpathContextCachePtr;
        if options == 0 {
            if value < 0 {
                value = 100;
            }
            (*cache).max_nodeset = value;
            (*cache).max_string = value;
            (*cache).max_number = value;
            (*cache).max_boolean = value;
            (*cache).max_misc = value;
        }
    } else if !(*ctxt).cache.is_null() {
        xml_xpath_free_cache((*ctxt).cache as XmlXpathContextCachePtr);
        (*ctxt).cache = null_mut();
    }
    0
}

/**
 * Evaluation functions.
 */
/**
 * xmlXPathOrderDocElems:
 * @doc:  an input document
 *
 * Call this routine to speed up XPath computation on static documents.
 * This stamps all the element nodes with the document order
 * Like for line information, the order is kept in the element->content
 * field, the value stored is actually - the node number (starting at -1)
 * to be able to differentiate from line numbers.
 *
 * Returns the number of elements found in the document or -1 in case
 *    of error.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_order_doc_elems(doc: XmlDocPtr) -> i64 {
    let mut count: isize = 0;
    let mut cur: XmlNodePtr;

    if doc.is_null() {
        return -1;
    }
    cur = (*doc).children;
    while !cur.is_null() {
        if matches!((*cur).typ, XmlElementType::XmlElementNode) {
            count += 1;
            (*cur).content = (-count) as _;
            if let Some(children) = (*cur).children {
                cur = children.as_ptr();
                continue;
            }
        }
        if let Some(next) = (*cur).next {
            cur = next.as_ptr();
            continue;
        }
        loop {
            cur = (*cur).parent.map_or(null_mut(), |p| p.as_ptr());
            if cur.is_null() {
                break;
            }
            if cur == doc as XmlNodePtr {
                cur = null_mut();
                break;
            }
            if let Some(next) = (*cur).next {
                cur = next.as_ptr();
                break;
            }

            if cur.is_null() {
                break;
            }
        }
    }
    count as _
}

/**
 * xmlXPathSetContextNode:
 * @node: the node to to use as the context node
 * @ctx:  the XPath context
 *
 * Sets 'node' as the context node. The node must be in the same
 * document as that associated with the context.
 *
 * Returns -1 in case of error or 0 if successful
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_set_context_node(
    node: XmlNodePtr,
    ctx: XmlXPathContextPtr,
) -> i32 {
    if node.is_null() || ctx.is_null() {
        return -1;
    }

    if (*node).doc == (*ctx).doc {
        (*ctx).node = node;
        return 0;
    }
    -1
}

/**
 * xmlXPathNodeEval:
 * @node: the node to to use as the context node
 * @str:  the XPath expression
 * @ctx:  the XPath context
 *
 * Evaluate the XPath Location Path in the given context. The node 'node'
 * is set as the context node. The context node is not restored.
 *
 * Returns the let resulting: xmlXPathObjectPtr from the evaluation or NULL.
 *         the caller has to free the object.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_node_eval(
    node: XmlNodePtr,
    str: *const XmlChar,
    ctx: XmlXPathContextPtr,
) -> XmlXPathObjectPtr {
    if str.is_null() {
        return null_mut();
    }
    if xml_xpath_set_context_node(node, ctx) < 0 {
        return null_mut();
    }
    xml_xpath_eval(str, ctx)
}

macro_rules! CHECK_CTXT {
    ($ctxt:expr) => {
        if $ctxt.is_null() {
            $crate::__xml_raise_error!(
                None,
                None,
                None,
                std::ptr::null_mut(),
                std::ptr::null_mut(),
                $crate::error::XmlErrorDomain::XmlFromXPath,
                $crate::error::XmlParserErrors::XmlErrInternalError,
                $crate::error::XmlErrorLevel::XmlErrFatal,
                file!().as_ptr() as _,
                line!() as i32,
                None,
                None,
                None,
                0,
                0,
                c"NULL context pointer\n".as_ptr() as _,
            );
            return null_mut();
        }
    };
}

/**
 * xmlXPathEval:
 * @str:  the XPath expression
 * @ctx:  the XPath context
 *
 * Evaluate the XPath Location Path in the given context.
 *
 * Returns the let resulting: xmlXPathObjectPtr from the evaluation or NULL.
 *         the caller has to free the object.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_eval(
    str: *const XmlChar,
    ctx: XmlXPathContextPtr,
) -> XmlXPathObjectPtr {
    use crate::generic_error;

    let res: XmlXPathObjectPtr;

    CHECK_CTXT!(ctx);

    xml_init_parser();

    let ctxt: XmlXPathParserContextPtr = xml_xpath_new_parser_context(str, ctx);
    if ctxt.is_null() {
        return null_mut();
    }
    xml_xpath_eval_expr(ctxt);

    if (*ctxt).error != XmlXPathError::XpathExpressionOk as i32 {
        res = null_mut();
    } else {
        res = value_pop(ctxt);
        if res.is_null() {
            generic_error!("xmlXPathCompiledEval: No result on the stack.\n");
        } else if (*ctxt).value_nr > 0 {
            generic_error!(
                "xmlXPathCompiledEval: {} object(s) left on the stack.\n",
                (*ctxt).value_nr as i32
            );
        }
    }

    xml_xpath_free_parser_context(ctxt);
    res
}

/**
 * xmlXPathEvalExpression:
 * @str:  the XPath expression
 * @ctxt:  the XPath context
 *
 * Alias for xmlXPathEval().
 *
 * Returns the let resulting: xmlXPathObjectPtr from the evaluation or NULL.
 *         the caller has to free the object.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_eval_expression(
    str: *const XmlChar,
    ctxt: XmlXPathContextPtr,
) -> XmlXPathObjectPtr {
    xml_xpath_eval(str, ctxt)
}

/**
 * xmlXPathEvalPredicate:
 * @ctxt:  the XPath context
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
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_eval_predicate(
    ctxt: XmlXPathContextPtr,
    res: XmlXPathObjectPtr,
) -> i32 {
    use crate::generic_error;

    if ctxt.is_null() || res.is_null() {
        return 0;
    }
    match (*res).typ {
        XmlXPathObjectType::XpathBoolean => {
            return (*res).boolval;
        }
        XmlXPathObjectType::XpathNumber => {
            return ((*res).floatval == (*ctxt).proximity_position as _) as i32;
        }
        XmlXPathObjectType::XpathNodeset | XmlXPathObjectType::XpathXsltTree => {
            if (*res).nodesetval.is_null() {
                return 0;
            }
            return ((*(*res).nodesetval).node_nr != 0) as i32;
        }
        XmlXPathObjectType::XpathString => {
            return (!(*res).stringval.is_null() && xml_strlen((*res).stringval) != 0) as i32;
        }
        _ => {
            generic_error!("Internal error at {}:{}\n", file!(), line!());
        }
    }
    0
}

/**
 * Separate compilation/evaluation entry points.
 */
/**
 * xmlXPathCompile:
 * @str:  the XPath expression
 *
 * Compile an XPath expression
 *
 * Returns the xmlXPathCompExprPtr resulting from the compilation or NULL.
 *         the caller has to free the object.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_compile(str: *const XmlChar) -> XmlXPathCompExprPtr {
    xml_xpath_ctxt_compile(null_mut(), str)
}

/**
 * xmlXPathCtxtCompile:
 * @ctxt: an XPath context
 * @str:  the XPath expression
 *
 * Compile an XPath expression
 *
 * Returns the xmlXPathCompExprPtr resulting from the compilation or NULL.
 *         the caller has to free the object.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_ctxt_compile(
    ctxt: XmlXPathContextPtr,
    str: *const XmlChar,
) -> XmlXPathCompExprPtr {
    use std::ffi::CString;

    let mut comp: XmlXPathCompExprPtr;
    let mut old_depth: i32 = 0;

    #[cfg(feature = "libxml_pattern")]
    {
        comp = xml_xpath_try_stream_compile(ctxt, str);
        if !comp.is_null() {
            return comp;
        }
    }

    xml_init_parser();

    let pctxt: XmlXPathParserContextPtr = xml_xpath_new_parser_context(str, ctxt);
    if pctxt.is_null() {
        return null_mut();
    }
    if !ctxt.is_null() {
        old_depth = (*ctxt).depth;
    }
    xml_xpath_compile_expr(pctxt, 1);
    if !ctxt.is_null() {
        (*ctxt).depth = old_depth;
    }

    if (*pctxt).error != XmlXPathError::XpathExpressionOk as i32 {
        xml_xpath_free_parser_context(pctxt);
        return null_mut();
    }

    if *(*pctxt).cur != 0 {
        /*
         * aleksey: in some cases this line prints *second* error message
         * (see bug #78858) and probably this should be fixed.
         * However, we are not sure that all error messages are printed
         * out in other places. It's not critical so we leave it as-is for now
         */
        let file = CString::new(file!()).unwrap();
        xml_xpatherror(
            pctxt,
            file.as_ptr(),
            line!() as i32,
            XmlXPathError::XpathExprError as i32,
        );
        comp = null_mut();
    } else {
        comp = (*pctxt).comp;
        if (*comp).nb_step > 1 && (*comp).last >= 0 {
            if !ctxt.is_null() {
                old_depth = (*ctxt).depth;
            }
            xml_xpath_optimize_expression(pctxt, (*comp).steps.add((*comp).last as usize));
            if !ctxt.is_null() {
                (*ctxt).depth = old_depth;
            }
        }
        (*pctxt).comp = null_mut();
    }
    xml_xpath_free_parser_context(pctxt);

    if !comp.is_null() {
        (*comp).expr = xml_strdup(str);
    }
    comp
}

macro_rules! CHECK_CTXT_NEG {
    ($ctxt:expr) => {
        if $ctxt.is_null() {
            let file = std::ffi::CString::new(file!()).unwrap();
            $crate::__xml_raise_error!(
                None,
                None,
                None,
                null_mut(),
                null_mut(),
                $crate::error::XmlErrorDomain::XmlFromXPath,
                $crate::error::XmlParserErrors::XmlErrInternalError,
                $crate::error::XmlErrorLevel::XmlErrFatal,
                file.as_ptr() as _,
                line!() as i32,
                None,
                None,
                None,
                0,
                0,
                c"NULL context pointer\n".as_ptr(),
            );
            return -1;
        }
    };
}

/**
 * xmlXPathCompParserContext:
 * @comp:  the XPath compiled expression
 * @ctxt:  the XPath context
 *
 * Create a new xmlXPathParserContext when processing a compiled expression
 *
 * Returns the xmlXPathParserContext just allocated.
 */
unsafe extern "C" fn xml_xpath_comp_parser_context(
    comp: XmlXPathCompExprPtr,
    ctxt: XmlXPathContextPtr,
) -> XmlXPathParserContextPtr {
    let ret: XmlXPathParserContextPtr =
        xml_malloc(size_of::<XmlXPathParserContext>()) as XmlXPathParserContextPtr;
    if ret.is_null() {
        xml_xpath_err_memory(ctxt, c"creating evaluation context\n".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlXPathParserContext>());

    /* Allocate the value stack */
    (*ret).value_tab = xml_malloc(10 * size_of::<XmlXPathObjectPtr>()) as *mut XmlXPathObjectPtr;
    if (*ret).value_tab.is_null() {
        xml_free(ret as _);
        xml_xpath_err_memory(ctxt, c"creating evaluation context\n".as_ptr() as _);
        return null_mut();
    }
    (*ret).value_nr = 0;
    (*ret).value_max = 10;
    (*ret).value = null_mut();

    (*ret).context = ctxt;
    (*ret).comp = comp;

    ret
}

/**
 * xmlXPathCompiledEvalInternal:
 * @comp:  the compiled XPath expression
 * @ctxt:  the XPath context
 * @resObj: the resulting XPath object or NULL
 * @toBool: 1 if only a boolean result is requested
 *
 * Evaluate the Precompiled XPath expression in the given context.
 * The caller has to free @resObj.
 *
 * Returns the let resulting: xmlXPathObjectPtr from the evaluation or NULL.
 *         the caller has to free the object.
 */
unsafe extern "C" fn xml_xpath_compiled_eval_internal(
    comp: XmlXPathCompExprPtr,
    ctxt: XmlXPathContextPtr,
    res_obj_ptr: *mut XmlXPathObjectPtr,
    to_bool: i32,
) -> i32 {
    let res_obj: XmlXPathObjectPtr;
    #[cfg(not(feature = "thread"))]
    static mut REENTANCE: i32 = 0;

    CHECK_CTXT_NEG!(ctxt);

    if comp.is_null() {
        return -1;
    }
    xml_init_parser();

    #[cfg(not(feature = "thread"))]
    {
        REENTANCE += 1;
        if REENTANCE > 1 {
            XML_XPATH_DISABLE_OPTIMIZER = 1;
        }
    }

    let pctxt: XmlXPathParserContextPtr = xml_xpath_comp_parser_context(comp, ctxt);
    if pctxt.is_null() {
        return -1;
    }
    let res: i32 = xml_xpath_run_eval(pctxt, to_bool);

    if (*pctxt).error != XmlXPathError::XpathExpressionOk as i32 {
        res_obj = null_mut();
    } else {
        res_obj = value_pop(pctxt);
        if res_obj.is_null() {
            if to_bool == 0 {
                generic_error!("xmlXPathCompiledEval: No result on the stack.\n");
            }
        } else if (*pctxt).value_nr > 0 {
            generic_error!(
                "xmlXPathCompiledEval: {} object(s) left on the stack.\n",
                (*pctxt).value_nr as i32
            );
        }
    }

    if !res_obj_ptr.is_null() {
        *res_obj_ptr = res_obj;
    } else {
        xml_xpath_release_object(ctxt, res_obj);
    }

    (*pctxt).comp = null_mut();
    xml_xpath_free_parser_context(pctxt);
    #[cfg(not(feature = "thread"))]
    {
        REENTANCE -= 1;
    }

    res
}

/**
 * xmlXPathCompiledEval:
 * @comp:  the compiled XPath expression
 * @ctx:  the XPath context
 *
 * Evaluate the Precompiled XPath expression in the given context.
 *
 * Returns the let resulting: xmlXPathObjectPtr from the evaluation or NULL.
 *         the caller has to free the object.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_compiled_eval(
    comp: XmlXPathCompExprPtr,
    ctx: XmlXPathContextPtr,
) -> XmlXPathObjectPtr {
    let mut res: XmlXPathObjectPtr = null_mut();

    xml_xpath_compiled_eval_internal(comp, ctx, addr_of_mut!(res), 0);
    res
}

/**
 * xmlXPathCompiledEvalToBoolean:
 * @comp:  the compiled XPath expression
 * @ctxt:  the XPath context
 *
 * Applies the XPath boolean() function on the result of the given
 * compiled expression.
 *
 * Returns 1 if the expression evaluated to true, 0 if to false and
 *         -1 in API and internal errors.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_compiled_eval_to_boolean(
    comp: XmlXPathCompExprPtr,
    ctxt: XmlXPathContextPtr,
) -> i32 {
    xml_xpath_compiled_eval_internal(comp, ctxt, null_mut(), 1)
}

/**
 * xmlXPathFreeCompExpr:
 * @comp:  an XPATH comp
 *
 * Free up the memory allocated by @comp
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_free_comp_expr(comp: XmlXPathCompExprPtr) {
    let mut op: XmlXPathStepOpPtr;

    if comp.is_null() {
        return;
    }
    if (*comp).dict.is_null() {
        for i in 0..(*comp).nb_step {
            op = (*comp).steps.add(i as usize);
            if !(*op).value4.is_null() {
                if matches!((*op).op, XmlXPathOp::XpathOpValue) {
                    xml_xpath_free_object((*op).value4 as _);
                } else {
                    xml_free((*op).value4 as _);
                }
            }
            if !(*op).value5.is_null() {
                xml_free((*op).value5 as _);
            }
        }
    } else {
        for i in 0..(*comp).nb_step {
            op = (*comp).steps.add(i as usize);
            if !(*op).value4.is_null() && matches!((*op).op, XmlXPathOp::XpathOpValue) {
                xml_xpath_free_object((*op).value4 as _);
            }
        }
        xml_dict_free((*comp).dict);
    }
    if !(*comp).steps.is_null() {
        xml_free((*comp).steps as _);
    }
    #[cfg(feature = "libxml_pattern")]
    {
        if !(*comp).stream.is_null() {
            xml_free_pattern_list((*comp).stream);
        }
    }
    if !(*comp).expr.is_null() {
        xml_free((*comp).expr as _);
    }

    xml_free(comp as _);
}

/**
* xmlXPathInit:
*
* DEPRECATED: Alias for xmlInitParser.
*/
#[cfg(any(feature = "xpath", feature = "schema"))]
#[deprecated]
pub unsafe extern "C" fn xml_xpath_init() {
    xml_init_parser();
}

/**
* xmlXPathIsNaN:
* @val:  a let value: f64
*
* Returns 1 if the value is a NaN, 0 otherwise
*/
#[cfg(any(feature = "xpath", feature = "schema"))]
pub unsafe extern "C" fn xml_xpath_is_nan(val: f64) -> i32 {
    val.is_nan() as i32
}

/**
 * xmlXPathIsInf:
 * @val:  a let value: f64
 *
 * Returns 1 if the value is +Infinite, -1 if -Infinite, 0 otherwise
 */
#[cfg(any(feature = "xpath", feature = "schema"))]
pub unsafe extern "C" fn xml_xpath_is_inf(val: f64) -> i32 {
    if val.is_infinite() {
        if val > 0.0 {
            1
        } else {
            -1
        }
    } else {
        0
    }
}

/**
 * xmlInitXPathInternal:
 *
 * Initialize the XPath environment
 */
pub(crate) unsafe extern "C" fn xml_init_xpath_internal() {
    XML_XPATH_NAN = f64::NAN;
    XML_XPATH_PINF = f64::INFINITY;
    XML_XPATH_NINF = f64::NEG_INFINITY;
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_xpath_cast_boolean_to_number() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let val = gen_int(n_val, 0);

                let ret_val = xml_xpath_cast_boolean_to_number(val);
                desret_double(ret_val);
                des_int(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastBooleanToNumber",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastBooleanToNumber()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_boolean_to_string() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let val = gen_int(n_val, 0);

                let ret_val = xml_xpath_cast_boolean_to_string(val);
                desret_xml_char_ptr(ret_val);
                des_int(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastBooleanToString",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastBooleanToString()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_node_set_to_boolean() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ns in 0..GEN_NB_XML_NODE_SET_PTR {
                let mem_base = xml_mem_blocks();
                let ns = gen_xml_node_set_ptr(n_ns, 0);

                let ret_val = xml_xpath_cast_node_set_to_boolean(ns);
                desret_int(ret_val);
                des_xml_node_set_ptr(n_ns, ns, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastNodeSetToBoolean",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastNodeSetToBoolean()"
                    );
                    eprintln!(" {}", n_ns);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_node_set_to_number() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ns in 0..GEN_NB_XML_NODE_SET_PTR {
                let mem_base = xml_mem_blocks();
                let ns = gen_xml_node_set_ptr(n_ns, 0);

                let ret_val = xml_xpath_cast_node_set_to_number(ns);
                desret_double(ret_val);
                des_xml_node_set_ptr(n_ns, ns, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastNodeSetToNumber",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastNodeSetToNumber()"
                    );
                    eprintln!(" {}", n_ns);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_node_set_to_string() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ns in 0..GEN_NB_XML_NODE_SET_PTR {
                let mem_base = xml_mem_blocks();
                let ns = gen_xml_node_set_ptr(n_ns, 0);

                let ret_val = xml_xpath_cast_node_set_to_string(ns);
                desret_xml_char_ptr(ret_val);
                des_xml_node_set_ptr(n_ns, ns, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastNodeSetToString",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastNodeSetToString()"
                    );
                    eprintln!(" {}", n_ns);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_node_to_number() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let node = gen_xml_node_ptr(n_node, 0);

                let ret_val = xml_xpath_cast_node_to_number(node);
                desret_double(ret_val);
                des_xml_node_ptr(n_node, node, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastNodeToNumber",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastNodeToNumber()"
                    );
                    eprintln!(" {}", n_node);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_node_to_string() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let node = gen_xml_node_ptr(n_node, 0);

                let ret_val = xml_xpath_cast_node_to_string(node);
                desret_xml_char_ptr(ret_val);
                des_xml_node_ptr(n_node, node, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastNodeToString",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastNodeToString()"
                    );
                    eprintln!(" {}", n_node);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_number_to_boolean() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_DOUBLE {
                let mem_base = xml_mem_blocks();
                let val = gen_double(n_val, 0);

                let ret_val = xml_xpath_cast_number_to_boolean(val);
                desret_int(ret_val);
                des_double(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastNumberToBoolean",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastNumberToBoolean()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_number_to_string() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_DOUBLE {
                let mem_base = xml_mem_blocks();
                let val = gen_double(n_val, 0);

                let ret_val = xml_xpath_cast_number_to_string(val);
                desret_xml_char_ptr(ret_val);
                des_double(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastNumberToString",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastNumberToString()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_string_to_boolean() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_const_xml_char_ptr(n_val, 0);

                let ret_val = xml_xpath_cast_string_to_boolean(val as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastStringToBoolean",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastStringToBoolean()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_string_to_number() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_const_xml_char_ptr(n_val, 0);

                let ret_val = xml_xpath_cast_string_to_number(val as *const XmlChar);
                desret_double(ret_val);
                des_const_xml_char_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastStringToNumber",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastStringToNumber()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_to_boolean() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_xml_xpath_object_ptr(n_val, 0);

                let ret_val = xml_xpath_cast_to_boolean(val);
                desret_int(ret_val);
                des_xml_xpath_object_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastToBoolean",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastToBoolean()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_to_number() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_xml_xpath_object_ptr(n_val, 0);

                let ret_val = xml_xpath_cast_to_number(val);
                desret_double(ret_val);
                des_xml_xpath_object_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastToNumber",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastToNumber()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cast_to_string() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_xml_xpath_object_ptr(n_val, 0);

                let ret_val = xml_xpath_cast_to_string(val);
                desret_xml_char_ptr(ret_val);
                des_xml_xpath_object_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathCastToString",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathCastToString()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_cmp_nodes() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_node1 in 0..GEN_NB_XML_NODE_PTR {
                for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let node1 = gen_xml_node_ptr(n_node1, 0);
                    let node2 = gen_xml_node_ptr(n_node2, 1);

                    let ret_val = xml_xpath_cmp_nodes(node1, node2);
                    desret_int(ret_val);
                    des_xml_node_ptr(n_node1, node1, 0);
                    des_xml_node_ptr(n_node2, node2, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathCmpNodes",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathCmpNodes()");
                        eprint!(" {}", n_node1);
                        eprintln!(" {}", n_node2);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_compile() {

        /* missing type support */
    }

    #[test]
    fn test_xml_xpath_compiled_eval() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_comp in 0..GEN_NB_XML_XPATH_COMP_EXPR_PTR {
                for n_ctx in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                    let mem_base = xml_mem_blocks();
                    let comp = gen_xml_xpath_comp_expr_ptr(n_comp, 0);
                    let ctx = gen_xml_xpath_context_ptr(n_ctx, 1);

                    let ret_val = xml_xpath_compiled_eval(comp, ctx);
                    desret_xml_xpath_object_ptr(ret_val);
                    des_xml_xpath_comp_expr_ptr(n_comp, comp, 0);
                    des_xml_xpath_context_ptr(n_ctx, ctx, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathCompiledEval",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathCompiledEval()"
                        );
                        eprint!(" {}", n_comp);
                        eprintln!(" {}", n_ctx);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_compiled_eval_to_boolean() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_comp in 0..GEN_NB_XML_XPATH_COMP_EXPR_PTR {
                for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                    let mem_base = xml_mem_blocks();
                    let comp = gen_xml_xpath_comp_expr_ptr(n_comp, 0);
                    let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 1);

                    let ret_val = xml_xpath_compiled_eval_to_boolean(comp, ctxt);
                    desret_int(ret_val);
                    des_xml_xpath_comp_expr_ptr(n_comp, comp, 0);
                    des_xml_xpath_context_ptr(n_ctxt, ctxt, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathCompiledEvalToBoolean",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathCompiledEvalToBoolean()"
                        );
                        eprint!(" {}", n_comp);
                        eprintln!(" {}", n_ctxt);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_context_set_cache() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                for n_active in 0..GEN_NB_INT {
                    for n_value in 0..GEN_NB_INT {
                        for n_options in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 0);
                            let active = gen_int(n_active, 1);
                            let value = gen_int(n_value, 2);
                            let options = gen_int(n_options, 3);

                            let ret_val = xml_xpath_context_set_cache(ctxt, active, value, options);
                            desret_int(ret_val);
                            des_xml_xpath_context_ptr(n_ctxt, ctxt, 0);
                            des_int(n_active, active, 1);
                            des_int(n_value, value, 2);
                            des_int(n_options, options, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlXPathContextSetCache",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlXPathContextSetCache()"
                                );
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_active);
                                eprint!(" {}", n_value);
                                eprintln!(" {}", n_options);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_convert_boolean() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                let mem_base = xml_mem_blocks();
                let mut val = gen_xml_xpath_object_ptr(n_val, 0);

                let ret_val = xml_xpath_convert_boolean(val);
                val = null_mut();
                desret_xml_xpath_object_ptr(ret_val);
                des_xml_xpath_object_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathConvertBoolean",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathConvertBoolean()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_convert_number() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                let mem_base = xml_mem_blocks();
                let mut val = gen_xml_xpath_object_ptr(n_val, 0);

                let ret_val = xml_xpath_convert_number(val);
                val = null_mut();
                desret_xml_xpath_object_ptr(ret_val);
                des_xml_xpath_object_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathConvertNumber",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathConvertNumber()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_convert_string() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                let mem_base = xml_mem_blocks();
                let mut val = gen_xml_xpath_object_ptr(n_val, 0);

                let ret_val = xml_xpath_convert_string(val);
                val = null_mut();
                desret_xml_xpath_object_ptr(ret_val);
                des_xml_xpath_object_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathConvertString",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathConvertString()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_ctxt_compile() {

        /* missing type support */
    }

    #[test]
    fn test_xml_xpath_eval() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_ctx in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                    let mem_base = xml_mem_blocks();
                    let str = gen_const_xml_char_ptr(n_str, 0);
                    let ctx = gen_xml_xpath_context_ptr(n_ctx, 1);

                    let ret_val = xml_xpath_eval(str as *const XmlChar, ctx);
                    desret_xml_xpath_object_ptr(ret_val);
                    des_const_xml_char_ptr(n_str, str, 0);
                    des_xml_xpath_context_ptr(n_ctx, ctx, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathEval",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathEval()");
                        eprint!(" {}", n_str);
                        eprintln!(" {}", n_ctx);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_eval_expression() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                    let mem_base = xml_mem_blocks();
                    let str = gen_const_xml_char_ptr(n_str, 0);
                    let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 1);

                    let ret_val = xml_xpath_eval_expression(str as *const XmlChar, ctxt);
                    desret_xml_xpath_object_ptr(ret_val);
                    des_const_xml_char_ptr(n_str, str, 0);
                    des_xml_xpath_context_ptr(n_ctxt, ctxt, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathEvalExpression",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathEvalExpression()"
                        );
                        eprint!(" {}", n_str);
                        eprintln!(" {}", n_ctxt);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_eval_predicate() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                for n_res in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_xpath_context_ptr(n_ctxt, 0);
                    let res = gen_xml_xpath_object_ptr(n_res, 1);

                    let ret_val = xml_xpath_eval_predicate(ctxt, res);
                    desret_int(ret_val);
                    des_xml_xpath_context_ptr(n_ctxt, ctxt, 0);
                    des_xml_xpath_object_ptr(n_res, res, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathEvalPredicate",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathEvalPredicate()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_res);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_init() {
        #[cfg(any(feature = "xpath", feature = "schema"))]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            xml_xpath_init();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlXPathInit",
                    xml_mem_blocks() - mem_base
                );
                assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathInit()");
            }
        }
    }

    #[test]
    fn test_xml_xpath_is_inf() {
        #[cfg(any(feature = "xpath", feature = "schema"))]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_DOUBLE {
                let mem_base = xml_mem_blocks();
                let val = gen_double(n_val, 0);

                let ret_val = xml_xpath_is_inf(val);
                desret_int(ret_val);
                des_double(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathIsInf",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathIsInf()");
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_is_nan() {
        #[cfg(any(feature = "xpath", feature = "schema"))]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_DOUBLE {
                let mem_base = xml_mem_blocks();
                let val = gen_double(n_val, 0);

                let ret_val = xml_xpath_is_nan(val);
                desret_int(ret_val);
                des_double(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathIsNaN",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathIsNaN()");
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_new_context() {

        /* missing type support */
    }

    #[test]
    fn test_xml_xpath_node_eval() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_ctx in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                        let mem_base = xml_mem_blocks();
                        let node = gen_xml_node_ptr(n_node, 0);
                        let str = gen_const_xml_char_ptr(n_str, 1);
                        let ctx = gen_xml_xpath_context_ptr(n_ctx, 2);

                        let ret_val = xml_xpath_node_eval(node, str, ctx);
                        desret_xml_xpath_object_ptr(ret_val);
                        des_xml_node_ptr(n_node, node, 0);
                        des_const_xml_char_ptr(n_str, str, 1);
                        des_xml_xpath_context_ptr(n_ctx, ctx, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlXPathNodeEval",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlXPathNodeEval()");
                            eprint!(" {}", n_node);
                            eprint!(" {}", n_str);
                            eprintln!(" {}", n_ctx);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_node_set_create() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_xml_node_ptr(n_val, 0);

                let ret_val = xml_xpath_node_set_create(val);
                desret_xml_node_set_ptr(ret_val);
                des_xml_node_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathNodeSetCreate",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathNodeSetCreate()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_object_copy() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                let mem_base = xml_mem_blocks();
                let val = gen_xml_xpath_object_ptr(n_val, 0);

                let ret_val = xml_xpath_object_copy(val);
                desret_xml_xpath_object_ptr(ret_val);
                des_xml_xpath_object_ptr(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathObjectCopy",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathObjectCopy()"
                    );
                    eprintln!(" {}", n_val);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_order_doc_elems() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                let mem_base = xml_mem_blocks();
                let doc = gen_xml_doc_ptr(n_doc, 0);

                let ret_val = xml_xpath_order_doc_elems(doc);
                desret_long(ret_val);
                des_xml_doc_ptr(n_doc, doc, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlXPathOrderDocElems",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlXPathOrderDocElems()"
                    );
                    eprintln!(" {}", n_doc);
                }
            }
        }
    }

    #[test]
    fn test_xml_xpath_set_context_node() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_XML_NODE_PTR {
                for n_ctx in 0..GEN_NB_XML_XPATH_CONTEXT_PTR {
                    let mem_base = xml_mem_blocks();
                    let node = gen_xml_node_ptr(n_node, 0);
                    let ctx = gen_xml_xpath_context_ptr(n_ctx, 1);

                    let ret_val = xml_xpath_set_context_node(node, ctx);
                    desret_int(ret_val);
                    des_xml_node_ptr(n_node, node, 0);
                    des_xml_xpath_context_ptr(n_ctx, ctx, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlXPathSetContextNode",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlXPathSetContextNode()"
                        );
                        eprint!(" {}", n_node);
                        eprintln!(" {}", n_ctx);
                    }
                }
            }
        }
    }
}

//! Provide methods and data structures for processing XPath.  
//! This module is based on `libxml/xpath.h`, `xpath.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: XML Path Language implementation
// Description: API for the XML Path Language implementation
//
// XML Path Language implementation
// XPath is a language for addressing parts of an XML document,
// designed to be used by both XSLT and XPointer
//     http://www.w3.org/TR/xpath
//
// Implements
// W3C Recommendation 16 November 1999
//     http://www.w3.org/TR/1999/REC-xpath-19991116
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

#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
pub mod dump;
#[cfg(feature = "xpath")]
pub mod internals;
#[cfg(feature = "xpath")]
pub mod node_set;
#[cfg(feature = "xpath")]
pub mod object;

use std::{
    borrow::Cow,
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
};

use libc::memset;

use crate::{
    error::XmlError,
    generic_error,
    globals::{GenericErrorContext, StructuredError},
    hash::XmlHashTableRef,
    libxml::{
        globals::{xml_free, xml_malloc},
        parser::xml_init_parser,
        pattern::{XmlPatternPtr, xml_free_pattern_list},
        xmlstring::{XmlChar, xml_strdup},
    },
    tree::{NodeCommon, XmlDocPtr, XmlElementType, XmlGenericNodePtr, XmlNodePtr, XmlNsPtr},
};

#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
pub use dump::*;
#[cfg(feature = "xpath")]
pub use internals::*;
#[cfg(feature = "xpath")]
pub use node_set::*;
#[cfg(feature = "xpath")]
pub use object::*;

// when compiling an XPath expression we arbitrary limit the maximum
// number of step operation in the compiled expression. 1000000 is
// an insanely large value which should never be reached under normal
// circumstances
pub(crate) const XPATH_MAX_STEPS: usize = 1000000;

// when evaluating an XPath expression we arbitrary limit the maximum
// number of object allowed to be pushed on the stack. 1000000 is
// an insanely large value which should never be reached under normal
// circumstances
pub(crate) const XPATH_MAX_STACK_DEPTH: usize = 1000000;

/// The set of XPath error codes.
#[cfg(feature = "xpath")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum XmlXPathError {
    #[default]
    XPathExpressionOK = 0,
    XPathNumberError,
    XPathUnfinishedLiteralError,
    XPathStartLiteralError,
    XPathVariableRefError,
    XPathUndefVariableError,
    XPathInvalidPredicateError,
    XPathExprError,
    XPathUnclosedError,
    XPathUnknownFuncError,
    XPathInvalidOperand,
    XPathInvalidType,
    XPathInvalidArity,
    XPathInvalidCtxtSize,
    XPathInvalidCtxtPosition,
    XPathMemoryError,
    XPtrSyntaxError,
    XPtrResourceError,
    XPtrSubResourceError,
    XPathUndefPrefixError,
    XPathEncodingError,
    XPathInvalidCharError,
    XPathInvalidCtxt,
    XPathStackError,
    XPathForbidVariableError,
    XPathOpLimitExceeded,
    XPathRecursionLimitExceeded,
}

/// A conversion function is associated to a type and used to cast
/// the new type to primitive values.
///
/// Returns -1 in case of error, 0 otherwise
#[doc(alias = "xmlXPathConvertFunc")]
#[cfg(feature = "xpath")]
pub type XmlXPathConvertFunc = unsafe fn(obj: XmlXPathObjectPtr, typ: i32) -> i32;

// Extra type: a name and a conversion function.
#[cfg(feature = "xpath")]
pub type XmlXPathTypePtr = *mut XmlXPathType;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathType {
    name: *const XmlChar,      /* the type name */
    func: XmlXPathConvertFunc, /* the conversion function */
}

// Extra variable: a name and a value.
#[cfg(feature = "xpath")]
pub type XmlXPathVariablePtr = *mut XmlXPathVariable;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathVariable {
    name: *const XmlChar,     /* the variable name */
    value: XmlXPathObjectPtr, /* the value */
}

/// An XPath evaluation function, the parameters are on the XPath context stack.
#[doc(alias = "xmlXPathEvalFunc")]
#[cfg(feature = "xpath")]
pub type XmlXPathEvalFunc = unsafe fn(ctxt: XmlXPathParserContextPtr, nargs: i32);

// Extra function: a name and a evaluation function.
#[cfg(feature = "xpath")]
pub type XmlXPathFuncPtr = *mut XmlXPathFunct;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathFunct {
    name: *const XmlChar,   /* the function name */
    func: XmlXPathEvalFunc, /* the evaluation function */
}

/// An axis traversal function. To traverse an axis, the engine calls
/// the first time with cur == NULL and repeat until the function returns
/// NULL indicating the end of the axis traversal.
///
/// Returns the next node in that axis or NULL if at the end of the axis.
#[doc(alias = "xmlXPathAxisFunc")]
#[cfg(feature = "xpath")]
pub type XmlXPathAxisFunc =
    unsafe fn(ctxt: XmlXPathParserContextPtr, cur: XmlXPathObjectPtr) -> XmlXPathObjectPtr;

// Extra axis: a name and an axis function.
#[cfg(feature = "xpath")]
pub type XmlXPathAxisPtr = *mut XmlXPathAxis;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathAxis {
    name: *const XmlChar,   /* the axis name */
    func: XmlXPathAxisFunc, /* the search function */
}

/// An XPath function.
/// The arguments (if any) are popped out from the context stack
/// and the result is pushed on the stack.
#[doc(alias = "xmlXPathFunction")]
#[cfg(feature = "xpath")]
pub type XmlXPathFunction = unsafe fn(ctxt: XmlXPathParserContextPtr, nargs: i32);

/// Prototype for callbacks used to plug variable lookup in the XPath engine.
///
/// Returns the XPath object value or NULL if not found.
#[doc(alias = "xmlXPathVariableLookupFunc")]
#[cfg(feature = "xpath")]
pub type XmlXPathVariableLookupFunc =
    unsafe fn(ctxt: *mut c_void, name: *const XmlChar, ns_uri: *const XmlChar) -> XmlXPathObjectPtr;

/// Prototype for callbacks used to plug function lookup in the XPath engine.
///
/// Returns the XPath function or NULL if not found.
#[doc(alias = "xmlXPathFuncLookupFunc")]
#[cfg(feature = "xpath")]
pub type XmlXPathFuncLookupFunc = unsafe fn(
    ctxt: *mut c_void,
    name: *const XmlChar,
    ns_uri: *const XmlChar,
) -> Option<XmlXPathFunction>;

/// check namespaces at compilation
#[cfg(feature = "xpath")]
pub(crate) const XML_XPATH_CHECKNS: usize = 1 << 0;
/// forbid variables in expression
#[cfg(feature = "xpath")]
pub(crate) const XML_XPATH_NOVAR: usize = 1 << 1;

#[cfg(feature = "xpath")]
pub type XmlXPathContextPtr = *mut XmlXPathContext;
/// Expression evaluation occurs with respect to a context.
/// he context consists of:
///    - a node (the context node)
///    - a node list (the context node list)
///    - a set of variable bindings
///    - a function library
///    - the set of namespace declarations in scope for the expression
///      Following the switch to hash tables, this need to be trimmed up at
///      the next binary incompatible release.
///      The node may be modified when the context is passed to libxml2
///      for an XPath evaluation so you may need to initialize it again
///      before the next call.
#[doc(alias = "xmlXPathContext")]
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathContext {
    pub doc: Option<XmlDocPtr>,          /* The current document */
    pub node: Option<XmlGenericNodePtr>, /* The current node */

    pub(crate) nb_variables_unused: i32, /* unused (hash table) */
    pub(crate) max_variables_unused: i32, /* unused (hash table) */
    pub(crate) var_hash: Option<XmlHashTableRef<'static, XmlXPathObjectPtr>>, /* Hash table of defined variables */

    pub(crate) nb_types: i32,          /* number of defined types */
    pub(crate) max_types: i32,         /* max number of types */
    pub(crate) types: XmlXPathTypePtr, /* Array of defined types */

    pub(crate) nb_funcs_unused: i32,  /* unused (hash table) */
    pub(crate) max_funcs_unused: i32, /* unused (hash table) */
    pub(crate) func_hash: Option<XmlHashTableRef<'static, XmlXPathFunction>>, /* Hash table of defined funcs */

    pub(crate) nb_axis: i32,          /* number of defined axis */
    pub(crate) max_axis: i32,         /* max number of axis */
    pub(crate) axis: XmlXPathAxisPtr, /* Array of defined axis */

    // the namespace nodes of the context node
    pub(crate) namespaces: Option<Vec<XmlNsPtr>>, /* Array of namespaces */
    pub(crate) user: *mut c_void,                 /* function to free */

    // extra variables
    pub(crate) context_size: i32,       /* the context size */
    pub(crate) proximity_position: i32, /* the proximity position */

    // extra stuff for XPointer
    pub(crate) xptr: i32, /* is this an XPointer context? */
    pub(crate) here: Option<XmlGenericNodePtr>, /* for here() */
    pub(crate) origin: Option<XmlGenericNodePtr>, /* for origin() */

    // the set of namespace declarations in scope for the expression
    pub(crate) ns_hash: Option<XmlHashTableRef<'static, *mut XmlChar>>, /* The namespaces hash table */
    pub(crate) var_lookup_func: Option<XmlXPathVariableLookupFunc>,     /* variable lookup func */
    pub(crate) var_lookup_data: *mut c_void,                            /* variable lookup data */

    // Possibility to link in an extra item
    pub(crate) extra: *mut c_void, /* needed for XSLT */

    // The function name and URI when calling a function
    pub(crate) function: *const XmlChar,
    pub(crate) function_uri: *const XmlChar,

    // function lookup function and data
    pub(crate) func_lookup_func: Option<XmlXPathFuncLookupFunc>, /* function lookup func */
    pub(crate) func_lookup_data: *mut c_void,                    /* function lookup data */

    // temporary namespace lists kept for walking the namespace axis
    pub(crate) tmp_ns_list: Option<Vec<XmlNsPtr>>, /* Array of namespaces */
    pub(crate) tmp_ns_nr: i32,                     /* number of namespaces in scope */

    // error reporting mechanism
    pub(crate) user_data: Option<GenericErrorContext>, /* user specific data block */
    pub(crate) error: Option<StructuredError>,         /* the callback in case of errors */
    pub(crate) last_error: XmlError,                   /* the last error */
    pub(crate) debug_node: Option<XmlGenericNodePtr>,  /* the source node XSLT */

    pub(crate) flags: i32, /* flags to control compilation */

    // Cache for reusal of XPath objects
    pub cache: *mut c_void,

    // Resource limits
    pub(crate) op_limit: u64,
    pub(crate) op_count: u64,
    pub(crate) depth: i32,
}

impl Default for XmlXPathContext {
    fn default() -> Self {
        Self {
            doc: None,
            node: None,
            nb_variables_unused: 0,
            max_variables_unused: 0,
            var_hash: None,
            nb_types: 0,
            max_types: 0,
            types: null_mut(),
            nb_funcs_unused: 0,
            max_funcs_unused: 0,
            func_hash: None,
            nb_axis: 0,
            max_axis: 0,
            axis: null_mut(),
            namespaces: None,
            user: null_mut(),
            context_size: 0,
            proximity_position: 0,
            xptr: 0,
            here: None,
            origin: None,
            ns_hash: None,
            var_lookup_func: None,
            var_lookup_data: null_mut(),
            extra: null_mut(),
            function: null_mut(),
            function_uri: null_mut(),
            func_lookup_func: None,
            func_lookup_data: null_mut(),
            tmp_ns_list: None,
            tmp_ns_nr: 0,
            user_data: None,
            error: None,
            last_error: XmlError::default(),
            debug_node: None,
            flags: 0,
            cache: null_mut(),
            op_limit: 0,
            op_count: 0,
            depth: 0,
        }
    }
}

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

// The structure of a compiled expression form is not public.
#[cfg(feature = "xpath")]
pub type XmlXPathCompExprPtr = *mut XmlXPathCompExpr;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathCompExpr {
    // pub(crate) nb_step: i32,  /* Number of steps in this expression */
    // pub(crate) max_step: i32, /* Maximum number of steps allocated */
    pub(crate) steps: Vec<XmlXPathStepOp>, /* ops for computation of this expression */
    pub(crate) last: i32,                  /* index of last step in expression */
    pub(crate) expr: *mut XmlChar,         /* the expression being computed */
    #[cfg(feature = "libxml_pattern")]
    pub(crate) stream: XmlPatternPtr,
}

impl Default for XmlXPathCompExpr {
    fn default() -> Self {
        Self {
            steps: vec![],
            last: 0,
            expr: null_mut(),
            stream: null_mut(),
        }
    }
}

#[cfg(feature = "xpath")]
pub type XmlXPathParserContextPtr = *mut XmlXPathParserContext;
/// An XPath parser context. It contains pure parsing information,
/// an xmlXPathContext, and the stack of objects.
#[doc(alias = "xmlXPathParserContext")]
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlXPathParserContext {
    pub(crate) cur: *const XmlChar,  /* the current char being parsed */
    pub(crate) base: *const XmlChar, /* the full expression */

    pub(crate) error: i32, /* error code */

    pub(crate) context: XmlXPathContextPtr, /* the evaluation context */
    pub(crate) value: XmlXPathObjectPtr,    /* the current value */
    // pub(crate) value_nr: i32,               /* number of values stacked */
    // pub(crate) value_max: i32,              /* max number of values stacked */
    pub(crate) value_tab: Vec<XmlXPathObjectPtr>, /* stack of values */

    pub(crate) comp: XmlXPathCompExprPtr, /* the precompiled expression */
    pub(crate) xptr: i32,                 /* it this an XPointer expression */
    pub(crate) ancestor: Option<XmlGenericNodePtr>, /* used for walking preceding axis */

    pub(crate) value_frame: i32, /* unused */
}

impl Default for XmlXPathParserContext {
    fn default() -> Self {
        Self {
            cur: null(),
            base: null(),
            error: 0,
            context: null_mut(),
            value: null_mut(),
            value_tab: vec![],
            comp: null_mut(),
            xptr: 0,
            ancestor: None,
            value_frame: 0,
        }
    }
}

#[cfg(feature = "xpath")]
pub const XML_XPATH_NAN: f64 = f64::NAN;
#[cfg(feature = "xpath")]
pub const XML_XPATH_PINF: f64 = f64::INFINITY;
#[cfg(feature = "xpath")]
pub const XML_XPATH_NINF: f64 = f64::NEG_INFINITY;

/// Compare two nodes w.r.t document order
///
/// Returns -2 in case of error 1 if first point < second point, 0 if
/// it's the same node, -1 otherwise
#[doc(alias = "xmlXPathCmpNodes")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_cmp_nodes(
    mut node1: XmlGenericNodePtr,
    mut node2: XmlGenericNodePtr,
) -> i32 {
    use crate::tree::{NodeCommon, XmlAttrPtr, XmlNodePtr};

    let mut depth1: i32;
    let mut depth2: i32;
    let mut attr1 = 0;
    let mut attr2 = 0;
    let mut attr_node1 = None;
    let mut attr_node2 = None;

    // if node1.is_null() || node2.is_null() {
    //     return -2;
    // }
    // a couple of optimizations which will avoid computations in most cases
    // trivial case
    if node1 == node2 {
        return 0;
    }
    if let Ok(attr) = XmlAttrPtr::try_from(node1) {
        attr1 = 1;
        attr_node1 = Some(attr);
        node1 = attr.parent().unwrap();
    }
    if let Ok(attr) = XmlAttrPtr::try_from(node2) {
        attr2 = 1;
        attr_node2 = Some(attr);
        node2 = attr.parent().unwrap();
    }
    if node1 == node2 {
        if attr1 == attr2 {
            // not required, but we keep attributes in order
            if let Some((attr1, attr2)) = attr_node1.zip(attr_node2) {
                let mut cur = attr2.prev.map(XmlGenericNodePtr::from);
                while let Some(now) = cur {
                    if now == attr1.into() {
                        return 1;
                    }
                    cur = now.prev();
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
    if matches!(node1.element_type(), XmlElementType::XmlNamespaceDecl)
        || matches!(node2.element_type(), XmlElementType::XmlNamespaceDecl)
    {
        return 1;
    }
    if Some(node1) == node2.prev() {
        return 1;
    }
    if Some(node1) == node2.next() {
        return -1;
    }

    // Speedup using document order if available.
    if let Some((node1, node2)) = XmlNodePtr::try_from(node1)
        .ok()
        .zip(XmlNodePtr::try_from(node2).ok())
    {
        if node1.element_type() == XmlElementType::XmlElementNode
            && node2.element_type() == XmlElementType::XmlElementNode
            && 0 > node1.content as isize
            && 0 > node2.content as isize
            && node1.doc == node2.doc
        {
            let l1: isize = -(node1.content as isize);
            let l2: isize = -(node2.content as isize);
            if l1 < l2 {
                return 1;
            }
            if l1 > l2 {
                return -1;
            }
        }
    }

    // compute depth to root
    depth2 = 0;
    let mut cur = node2;
    while let Some(parent) = cur.parent() {
        if parent == node1 {
            return 1;
        }
        depth2 += 1;
        cur = parent;
    }
    let root = cur;

    depth1 = 0;
    let mut cur = node1;
    while let Some(parent) = cur.parent() {
        if parent == node2 {
            return -1;
        }
        depth1 += 1;
        cur = parent;
    }
    // Distinct document (or distinct entities :-( ) case.
    if root != cur {
        return -2;
    }
    // get the nearest common ancestor.
    while depth1 > depth2 {
        depth1 -= 1;
        node1 = node1.parent().unwrap();
    }
    while depth2 > depth1 {
        depth2 -= 1;
        node2 = node2.parent().unwrap();
    }
    while node1.parent() != node2.parent() {
        let Some((n1, n2)) = node1.parent().zip(node2.parent()) else {
            // should not happen but just in case ...
            return -2;
        };
        node1 = n1;
        node2 = n2;
    }
    // Find who's first.
    if Some(node1) == node2.prev() {
        return 1;
    }
    if Some(node1) == node2.next() {
        return -1;
    }
    // Speedup using document order if available.
    if let Some((node1, node2)) = XmlNodePtr::try_from(node1)
        .ok()
        .zip(XmlNodePtr::try_from(node2).ok())
    {
        if node1.element_type() == XmlElementType::XmlElementNode
            && node2.element_type() == XmlElementType::XmlElementNode
            && 0 > node1.content as isize
            && 0 > node2.content as isize
            && node1.doc == node2.doc
        {
            let l1: isize = -(node1.content as isize);
            let l2: isize = -(node2.content as isize);
            if l1 < l2 {
                return 1;
            }
            if l1 > l2 {
                return -1;
            }
        }
    }

    let mut cur = node1.next();
    while let Some(now) = cur {
        if now == node2 {
            return 1;
        }
        cur = now.next();
    }
    -1 /* assume there is no sibling list corruption */
}

/// Converts a number to its boolean value
///
/// Returns the boolean value
#[doc(alias = "xmlXPathCastNumberToBoolean")]
#[cfg(feature = "xpath")]
pub fn xml_xpath_cast_number_to_boolean(val: f64) -> bool {
    !xml_xpath_is_nan(val) && val != 0.0
}

/// Converts a string to its boolean value
///
/// Returns the boolean value
#[doc(alias = "xmlXPathCastStringToBoolean")]
#[cfg(feature = "xpath")]
pub fn xml_xpath_cast_string_to_boolean(val: Option<&str>) -> bool {
    val.is_some_and(|v| !v.is_empty())
}

/// Converts a node-set to its boolean value
///
/// Returns the boolean value
#[doc(alias = "xmlXPathCastNodeSetToBoolean")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_cast_node_set_to_boolean(ns: Option<&XmlNodeSet>) -> bool {
    ns.is_some_and(|n| !n.is_empty())
}

/// Converts a boolean to its number value
///
/// Returns the number value
#[doc(alias = "xmlXPathCastBooleanToNumber")]
#[cfg(feature = "xpath")]
pub fn xml_xpath_cast_boolean_to_number(val: bool) -> f64 {
    if val { 1.0 } else { 0.0 }
}

/// Converts a string to its number value
///
/// Returns the number value
#[doc(alias = "xmlXPathCastStringToNumber")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_cast_string_to_number(val: *const XmlChar) -> f64 {
    unsafe { xml_xpath_string_eval_number(val) }
}

/// Converts a node to its number value
///
/// Returns the number value
#[doc(alias = "xmlXPathCastNodeToNumber")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_cast_node_to_number(node: Option<XmlGenericNodePtr>) -> f64 {
    unsafe {
        use std::ffi::CString;

        if node.is_none() {
            return XML_XPATH_NAN;
        }
        let strval = xml_xpath_cast_node_to_string(node);
        let strval = CString::new(strval).unwrap();
        let ret: f64 = xml_xpath_cast_string_to_number(strval.as_ptr() as *const u8);

        ret
    }
}

/// Converts a node-set to its number value
///
/// Returns the number value
#[doc(alias = "xmlXPathCastNodeSetToNumber")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_cast_node_set_to_number(ns: Option<&mut XmlNodeSet>) -> f64 {
    unsafe {
        use std::ffi::CString;

        let Some(ns) = ns else {
            return XML_XPATH_NAN;
        };
        let str = CString::new(xml_xpath_cast_node_set_to_string(Some(ns)).as_ref()).unwrap();
        let ret: f64 = xml_xpath_cast_string_to_number(str.as_ptr() as *const u8);
        ret
    }
}

/// Converts a boolean to its string value.
///
/// Returns a newly allocated string.
#[doc(alias = "xmlXPathCastBooleanToString")]
#[cfg(feature = "xpath")]
pub fn xml_xpath_cast_boolean_to_string(val: bool) -> &'static str {
    if val { "true" } else { "false" }
}

const DBL_DIG: usize = 15;
const EXPONENT_DIGITS: usize = 3 + 2;
// const LOWER_DOUBLE_EXP: usize = 5;
const UPPER_DOUBLE: f64 = 1E9;
const LOWER_DOUBLE: f64 = 1E-5;

/// Convert the number into a string representation.
#[doc(alias = "xmlXPathFormatNumber")]
fn xml_xpath_format_number(number: f64, buffer: &mut String) {
    use std::fmt::Write as _;

    match xml_xpath_is_inf(number) {
        1 => {
            write!(buffer, "Infinity").ok();
        }
        -1 => {
            write!(buffer, "-Infinity").ok();
        }
        _ => {
            if xml_xpath_is_nan(number) {
                write!(buffer, "NaN").ok();
            } else if number == 0.0 {
                // Omit sign for negative zero.
                write!(buffer, "0").ok();
            } else if number > i32::MIN as f64
                && number < i32::MAX as f64
                && number == number as i32 as f64
            {
                let value = number as i32;
                write!(buffer, "{value}").ok();
            } else {
                // For the dimension of work,
                //     DBL_DIG is number of significant digits
                // EXPONENT is only needed for "scientific notation"
                //     3 is sign, decimal point, and terminating zero
                // LOWER_DOUBLE_EXP is max number of leading zeroes in fraction
                // Note that this dimension is slightly (a few characters)
                // larger than actually necessary.
                let absolute_value: f64 = number.abs();

                // First choose format - scientific or regular floating point.
                // In either case, result is in work, and after_fraction points
                // just past the fractional part.
                if !(LOWER_DOUBLE..=UPPER_DOUBLE).contains(&absolute_value) && absolute_value != 0.0
                {
                    // Use scientific notation
                    let integer_place = DBL_DIG + EXPONENT_DIGITS + 1;
                    let fraction_place = DBL_DIG - 1;
                    write!(buffer, "{number:integer_place$.fraction_place$e}").ok();

                    // Remove trailing zeros
                    let epos = buffer.find('e').unwrap_or(0);
                    if let Some(mut start_trailing_zeros) = buffer[..epos].rfind(|c| c != '0') {
                        if buffer.as_bytes()[start_trailing_zeros] != b'.' {
                            start_trailing_zeros += 1;
                        }
                        buffer.replace_range(start_trailing_zeros..epos, "");
                    }
                } else {
                    // Use regular notation
                    let fraction_place = if absolute_value > 0.0 {
                        let integer_place = absolute_value.log10() as usize;
                        if integer_place > 0 {
                            DBL_DIG - integer_place - 1
                        } else {
                            DBL_DIG - integer_place
                        }
                    } else {
                        1
                    };
                    write!(buffer, "{number:0.fraction_place$}").ok();

                    // Remove trailing zeros
                    while buffer.ends_with('0') {
                        buffer.pop();
                    }
                    if buffer.ends_with('.') {
                        buffer.pop();
                    }
                }

                // Remove leading spaces sometimes inserted by snprintf
                let trimed = buffer.trim_start();
                let trimed_length = buffer.len() - trimed.len();
                buffer.drain(..trimed_length);
            }
        }
    }
}

/// Converts a number to its string value.
///
/// Returns a newly allocated string.
#[doc(alias = "xmlXPathCastNumberToString")]
#[cfg(feature = "xpath")]
pub fn xml_xpath_cast_number_to_string(val: f64) -> Cow<'static, str> {
    match xml_xpath_is_inf(val) {
        1 => "Infinity".into(),
        -1 => "-Infinity".into(),
        _ => {
            if xml_xpath_is_nan(val) {
                "NaN".into()
            } else if val == 0.0 {
                // Omit sign for negative zero.
                "0".into()
            } else {
                // could be improved
                let mut buf = String::new();
                xml_xpath_format_number(val, &mut buf);
                buf.into()
            }
        }
    }
}

/// Converts a node to its string value.
///
/// Returns a newly allocated string.
#[doc(alias = "xmlXPathCastNodeToString")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_cast_node_to_string(node: Option<XmlGenericNodePtr>) -> String {
    unsafe {
        node.and_then(|node| node.get_content())
            .unwrap_or_else(|| "".to_owned())
    }
}

/// Converts a node-set to its string value.
///
/// Returns a newly allocated string.
#[doc(alias = "xmlXPathCastNodeSetToString")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_cast_node_set_to_string(ns: Option<&mut XmlNodeSet>) -> Cow<'static, str> {
    unsafe {
        let Some(ns) = ns.filter(|n| !n.is_empty()) else {
            return "".into();
        };

        if ns.len() > 1 {
            ns.sort();
        }

        xml_xpath_cast_node_to_string(Some(ns.node_tab[0])).into()
    }
}

/// Create a new xmlXPathContext
///
/// Returns the xmlXPathContext just allocated. The caller will need to free it.
#[doc(alias = "xmlXPathNewContext")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_new_context(doc: Option<XmlDocPtr>) -> XmlXPathContextPtr {
    unsafe {
        let ret: XmlXPathContextPtr =
            xml_malloc(size_of::<XmlXPathContext>()) as XmlXPathContextPtr;
        if ret.is_null() {
            xml_xpath_err_memory(null_mut(), Some("creating context\n"));
            return null_mut();
        }
        std::ptr::write(&mut *ret, XmlXPathContext::default());
        (*ret).doc = doc;
        (*ret).node = None;

        (*ret).var_hash = None;

        (*ret).nb_types = 0;
        (*ret).max_types = 0;
        (*ret).types = null_mut();

        (*ret).func_hash = XmlHashTableRef::with_capacity(0);

        (*ret).nb_axis = 0;
        (*ret).max_axis = 0;
        (*ret).axis = null_mut();

        (*ret).ns_hash = None;
        (*ret).user = null_mut();

        (*ret).context_size = -1;
        (*ret).proximity_position = -1;

        if xml_xpath_context_set_cache(ret, 1, -1, 0) == -1 {
            xml_xpath_free_context(ret);
            return null_mut();
        }

        xml_xpath_register_all_functions(ret);

        ret
    }
}

/// Frees the xsltPointerList structure. This does not free the content of the list.
#[doc(alias = "xsltPointerListFree")]
unsafe fn xml_pointer_list_free(list: XmlPointerListPtr) {
    unsafe {
        if list.is_null() {
            return;
        }
        if !(*list).items.is_null() {
            xml_free((*list).items as _);
        }
        xml_free(list as _);
    }
}

unsafe fn xml_xpath_cache_free_object_list(list: XmlPointerListPtr) {
    unsafe {
        let mut obj: XmlXPathObjectPtr;

        if list.is_null() {
            return;
        }

        for i in 0..(*list).number {
            obj = *(*list).items.add(i as usize) as _;
            // Note that it is already assured that we don't need to
            // look out for namespace nodes in the node-set.
            let _ = (*obj).nodesetval.take();
            xml_free(obj as _);
        }
        xml_pointer_list_free(list);
    }
}

unsafe fn xml_xpath_free_cache(cache: XmlXpathContextCachePtr) {
    unsafe {
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
}

/// Free up an xmlXPathContext
#[doc(alias = "xmlXPathFreeContext")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_free_context(ctxt: XmlXPathContextPtr) {
    unsafe {
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
}

/// Create a new object cache
///
/// Returns the xmlXPathCache just allocated.
#[doc(alias = "xmlXPathNewCache")]
unsafe fn xml_xpath_new_cache() -> XmlXpathContextCachePtr {
    unsafe {
        let ret: XmlXpathContextCachePtr =
            xml_malloc(size_of::<XmlXpathContextCache>()) as XmlXpathContextCachePtr;
        if ret.is_null() {
            xml_xpath_err_memory(null_mut(), Some("creating object cache\n"));
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
}

/// Creates/frees an object cache on the XPath context.
/// If activates XPath objects (xmlXPathObject) will be cached internally to be reused.
/// @options:
///   0: This will set the XPath object caching:
///      @value:
///        This will set the maximum number of XPath objects
///        to be cached per slot
///        There are 5 slots for: node-set, string, number, boolean, and
///        misc objects. Use <0 for the default number (100).
///   Other values for @options have currently no effect.
///
/// Returns 0 if the setting succeeded, and -1 on API or internal errors.
#[doc(alias = "xmlXPathContextSetCache")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_context_set_cache(
    ctxt: XmlXPathContextPtr,
    active: i32,
    mut value: i32,
    options: i32,
) -> i32 {
    unsafe {
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
}

/// Call this routine to speed up XPath computation on static documents.
/// This stamps all the element nodes with the document order
/// Like for line information, the order is kept in the element->content
/// field, the value stored is actually - the node number (starting at -1)
/// to be able to differentiate from line numbers.
///
/// Returns the number of elements found in the document or -1 in case of error.
#[doc(alias = "xmlXPathOrderDocElems")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_order_doc_elems(doc: XmlDocPtr) -> i64 {
    use crate::tree::{NodeCommon, XmlNodePtr};

    let mut count: isize = 0;

    let mut cur = doc.children;
    while let Some(mut now) = cur {
        if let Some(mut node) = XmlNodePtr::try_from(now)
            .ok()
            .filter(|node| node.element_type() == XmlElementType::XmlElementNode)
        {
            count += 1;
            node.content = (-count) as _;
            if let Some(children) = node.children {
                cur = Some(children);
                continue;
            }
        }
        if let Some(next) = now.next() {
            cur = Some(next);
            continue;
        }
        cur = loop {
            let Some(cur) = now.parent() else {
                break None;
            };
            if cur == doc.into() {
                break None;
            }
            if let Some(next) = cur.next() {
                break Some(next);
            }
            now = cur;
        }
    }
    count as _
}

/// Sets 'node' as the context node. The node must be in the same
/// document as that associated with the context.
///
/// Returns -1 in case of error or 0 if successful
#[doc(alias = "xmlXPathSetContextNode")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_set_context_node(node: XmlGenericNodePtr, ctx: XmlXPathContextPtr) -> i32 {
    unsafe {
        // if node.is_null() {
        //     return -1;
        // }
        if ctx.is_null() {
            return -1;
        }

        if node.document() == (*ctx).doc {
            (*ctx).node = Some(node);
            return 0;
        }
        -1
    }
}

/// Evaluate the XPath Location Path in the given context. The node 'node'
/// is set as the context node. The context node is not restored.
///
/// Returns the let resulting: xmlXPathObjectPtr from the evaluation or NULL.
/// The caller has to free the object.
#[doc(alias = "xmlXPathNodeEval")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_node_eval(
    node: XmlGenericNodePtr,
    str: *const XmlChar,
    ctx: XmlXPathContextPtr,
) -> XmlXPathObjectPtr {
    unsafe {
        if str.is_null() {
            return null_mut();
        }
        if xml_xpath_set_context_node(node, ctx) < 0 {
            return null_mut();
        }
        xml_xpath_eval(str, ctx)
    }
}

macro_rules! CHECK_CTXT {
    ($ctxt:expr) => {
        if $ctxt.is_null() {
            $crate::error::__xml_raise_error!(
                None,
                None,
                None,
                std::ptr::null_mut(),
                None,
                $crate::error::XmlErrorDomain::XmlFromXPath,
                $crate::error::XmlParserErrors::XmlErrInternalError,
                $crate::error::XmlErrorLevel::XmlErrFatal,
                Some(file!().into()),
                line!() as i32,
                None,
                None,
                None,
                0,
                0,
                "NULL context pointer\n",
            );
            return null_mut();
        }
    };
}

/// Evaluate the XPath Location Path in the given context.
///
/// Returns the let resulting: xmlXPathObjectPtr from the evaluation or NULL.
/// The caller has to free the object.
#[doc(alias = "xmlXPathEval")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_eval(str: *const XmlChar, ctx: XmlXPathContextPtr) -> XmlXPathObjectPtr {
    unsafe {
        use crate::generic_error;

        let res: XmlXPathObjectPtr;

        CHECK_CTXT!(ctx);

        xml_init_parser();

        let ctxt: XmlXPathParserContextPtr = xml_xpath_new_parser_context(str, ctx);
        if ctxt.is_null() {
            return null_mut();
        }
        xml_xpath_eval_expr(ctxt);

        if (*ctxt).error != XmlXPathError::XPathExpressionOK as i32 {
            res = null_mut();
        } else {
            res = value_pop(ctxt);
            if res.is_null() {
                generic_error!("xmlXPathCompiledEval: No result on the stack.\n");
            } else if !(*ctxt).value_tab.is_empty() {
                generic_error!(
                    "xmlXPathCompiledEval: {} object(s) left on the stack.\n",
                    (*ctxt).value_tab.len() as i32
                );
            }
        }

        xml_xpath_free_parser_context(ctxt);
        res
    }
}

/// Alias for xmlXPathEval().
///
/// Returns the let resulting: xmlXPathObjectPtr from the evaluation or NULL.
/// The caller has to free the object.
#[doc(alias = "xmlXPathEvalExpression")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_eval_expression(
    str: *const XmlChar,
    ctxt: XmlXPathContextPtr,
) -> XmlXPathObjectPtr {
    unsafe { xml_xpath_eval(str, ctxt) }
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
/// Returns `true` if predicate is true, `false` otherwise
#[doc(alias = "xmlXPathEvalPredicate")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_eval_predicate(ctxt: XmlXPathContextPtr, res: XmlXPathObjectPtr) -> bool {
    unsafe {
        use crate::generic_error;

        if ctxt.is_null() || res.is_null() {
            return false;
        }
        match (*res).typ {
            XmlXPathObjectType::XPathBoolean => {
                return (*res).boolval;
            }
            XmlXPathObjectType::XPathNumber => {
                return (*res).floatval == (*ctxt).proximity_position as _;
            }
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
                let Some(nodeset) = (*res).nodesetval.as_deref() else {
                    return false;
                };
                return !nodeset.node_tab.is_empty();
            }
            XmlXPathObjectType::XPathString => {
                return (*res).stringval.as_deref().is_some_and(|s| !s.is_empty());
            }
            _ => {
                generic_error!("Internal error at {}:{}\n", file!(), line!());
            }
        }
        false
    }
}

/// Compile an XPath expression
///
/// Returns the xmlXPathCompExprPtr resulting from the compilation or NULL.
/// The caller has to free the object.
#[doc(alias = "xmlXPathCompile")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_compile(str: *const XmlChar) -> XmlXPathCompExprPtr {
    unsafe { xml_xpath_ctxt_compile(null_mut(), str) }
}

/// Compile an XPath expression
///
/// Returns the xmlXPathCompExprPtr resulting from the compilation or NULL.
/// The caller has to free the object.
#[doc(alias = "xmlXPathCtxtCompile")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_ctxt_compile(
    ctxt: XmlXPathContextPtr,
    str: *const XmlChar,
) -> XmlXPathCompExprPtr {
    unsafe {
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

        if (*pctxt).error != XmlXPathError::XPathExpressionOK as i32 {
            xml_xpath_free_parser_context(pctxt);
            return null_mut();
        }

        if *(*pctxt).cur != 0 {
            // aleksey: in some cases this line prints *second* error message
            // (see bug #78858) and probably this should be fixed.
            // However, we are not sure that all error messages are printed
            // out in other places. It's not critical so we leave it as-is for now
            let file = CString::new(file!()).unwrap();
            xml_xpatherror(
                pctxt,
                file.as_ptr(),
                line!() as i32,
                XmlXPathError::XPathExprError as i32,
            );
            comp = null_mut();
        } else {
            comp = (*pctxt).comp;
            if (*comp).steps.len() > 1 && (*comp).last >= 0 {
                if !ctxt.is_null() {
                    old_depth = (*ctxt).depth;
                }
                xml_xpath_optimize_expression(pctxt, &raw mut (*comp).steps[(*comp).last as usize]);
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
}

macro_rules! CHECK_CTXT_NEG {
    ($ctxt:expr) => {
        if $ctxt.is_null() {
            $crate::error::__xml_raise_error!(
                None,
                None,
                None,
                null_mut(),
                None,
                $crate::error::XmlErrorDomain::XmlFromXPath,
                $crate::error::XmlParserErrors::XmlErrInternalError,
                $crate::error::XmlErrorLevel::XmlErrFatal,
                Some(file!().into()),
                line!() as i32,
                None,
                None,
                None,
                0,
                0,
                "NULL context pointer\n",
            );
            return -1;
        }
    };
}

/// Create a new xmlXPathParserContext when processing a compiled expression
///
/// Returns the xmlXPathParserContext just allocated.
#[doc(alias = "xmlXPathCompParserContext")]
unsafe fn xml_xpath_comp_parser_context(
    comp: XmlXPathCompExprPtr,
    ctxt: XmlXPathContextPtr,
) -> XmlXPathParserContextPtr {
    unsafe {
        let ret: XmlXPathParserContextPtr =
            xml_malloc(size_of::<XmlXPathParserContext>()) as XmlXPathParserContextPtr;
        if ret.is_null() {
            xml_xpath_err_memory(ctxt, Some("creating evaluation context\n"));
            return null_mut();
        }
        std::ptr::write(&mut *ret, XmlXPathParserContext::default());

        // Allocate the value stack
        (*ret).value_tab.reserve(10);
        (*ret).value = null_mut();
        (*ret).context = ctxt;
        (*ret).comp = comp;

        ret
    }
}

/// Evaluate the Precompiled XPath expression in the given context.
/// The caller has to free @resObj.
///
/// Returns the let resulting: xmlXPathObjectPtr from the evaluation or NULL.
/// The caller has to free the object.
#[doc(alias = "xmlXPathCompiledEvalInternal")]
unsafe fn xml_xpath_compiled_eval_internal(
    comp: XmlXPathCompExprPtr,
    ctxt: XmlXPathContextPtr,
    res_obj_ptr: *mut XmlXPathObjectPtr,
    to_bool: i32,
) -> i32 {
    unsafe {
        let res_obj: XmlXPathObjectPtr;

        CHECK_CTXT_NEG!(ctxt);

        if comp.is_null() {
            return -1;
        }
        xml_init_parser();

        let pctxt: XmlXPathParserContextPtr = xml_xpath_comp_parser_context(comp, ctxt);
        if pctxt.is_null() {
            return -1;
        }
        let res: i32 = xml_xpath_run_eval(pctxt, to_bool);

        if (*pctxt).error != XmlXPathError::XPathExpressionOK as i32 {
            res_obj = null_mut();
        } else {
            res_obj = value_pop(pctxt);
            if res_obj.is_null() {
                if to_bool == 0 {
                    generic_error!("xmlXPathCompiledEval: No result on the stack.\n");
                }
            } else if !(*pctxt).value_tab.is_empty() {
                generic_error!(
                    "xmlXPathCompiledEval: {} object(s) left on the stack.\n",
                    (*pctxt).value_tab.len() as i32
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

        res
    }
}

/// Evaluate the Precompiled XPath expression in the given context.
///
/// Returns the let resulting: xmlXPathObjectPtr from the evaluation or NULL.
/// The caller has to free the object.
#[doc(alias = "xmlXPathCompiledEval")]
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_xpath_compiled_eval(
    comp: XmlXPathCompExprPtr,
    ctx: XmlXPathContextPtr,
) -> XmlXPathObjectPtr {
    unsafe {
        let mut res: XmlXPathObjectPtr = null_mut();

        xml_xpath_compiled_eval_internal(comp, ctx, addr_of_mut!(res), 0);
        res
    }
}

/// Applies the XPath boolean() function on the result of the given
/// compiled expression.
///
/// Returns 1 if the expression evaluated to true, 0 if to false and
/// -1 in API and internal errors.
#[doc(alias = "xmlXPathCompiledEvalToBoolean")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_compiled_eval_to_boolean(
    comp: XmlXPathCompExprPtr,
    ctxt: XmlXPathContextPtr,
) -> i32 {
    unsafe { xml_xpath_compiled_eval_internal(comp, ctxt, null_mut(), 1) }
}

/// Free up the memory allocated by @comp
#[doc(alias = "xmlXPathFreeCompExpr")]
#[cfg(feature = "xpath")]
pub unsafe fn xml_xpath_free_comp_expr(comp: XmlXPathCompExprPtr) {
    unsafe {
        use std::ptr::drop_in_place;

        if comp.is_null() {
            return;
        }
        for op in &(*comp).steps {
            if !op.value4.is_null() {
                if matches!(op.op, XmlXPathOp::XpathOpValue) {
                    xml_xpath_free_object(op.value4 as _);
                } else {
                    xml_free(op.value4 as _);
                }
            }
            if !op.value5.is_null() {
                xml_free(op.value5 as _);
            }
        }
        #[cfg(feature = "libxml_pattern")]
        if !(*comp).stream.is_null() {
            xml_free_pattern_list((*comp).stream);
        }
        if !(*comp).expr.is_null() {
            xml_free((*comp).expr as _);
        }

        drop_in_place(comp);
        xml_free(comp as _);
    }
}

/// Compare two nodes w.r.t document order.
/// This one is optimized for handling of non-element nodes.
///
/// Returns -2 in case of error 1 if first point < second point, 0
/// if it's the same node, -1 otherwise
#[doc(alias = "xmlXPathCmpNodesExt")]
unsafe fn xml_xpath_cmp_nodes_ext(
    mut node1: XmlGenericNodePtr,
    mut node2: XmlGenericNodePtr,
) -> Option<std::cmp::Ordering> {
    let mut misc: i32 = 0;
    let mut precedence1: i32 = 0;
    let mut precedence2: i32 = 0;
    let mut misc_node1 = None;
    let mut misc_node2 = None;

    // if node1.is_null() || node2.is_null() {
    //     return None;
    // }

    if node1 == node2 {
        return Some(std::cmp::Ordering::Equal);
    }

    // a couple of optimizations which will avoid computations in most cases
    'turtle_comparison: {
        match node1.element_type() {
            XmlElementType::XmlElementNode => {
                let node1 = XmlNodePtr::try_from(node1).unwrap();
                if let Some(node2) = XmlNodePtr::try_from(node2)
                    .ok()
                    .filter(|node2| node2.element_type() == XmlElementType::XmlElementNode)
                {
                    if 0 > node1.content as isize
                        && 0 > node2.content as isize
                        && node1.doc == node2.doc
                    {
                        let l1 = -(node1.content as isize);
                        let l2 = -(node2.content as isize);
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
                misc_node1 = Some(node1);
                node1 = node1.parent().unwrap();
                misc = 1;
            }
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlPINode => {
                misc_node1 = Some(node1);
                // Find nearest element node.
                let n1 = if let Some(mut prev1) = node1.prev() {
                    loop {
                        node1 = prev1;
                        if matches!(node1.element_type(), XmlElementType::XmlElementNode) {
                            precedence1 = 3; /* element in prev-sibl axis */
                            break Some(node1);
                        }
                        let Some(prev) = node1.prev() else {
                            precedence1 = 2; /* element is parent */
                            // URGENT TODO: Are there any cases, where the
                            // parent of such a node is not an element node?
                            break node1.parent();
                        };
                        prev1 = prev;
                    }
                } else {
                    precedence1 = 2; /* element is parent */
                    node1.parent()
                };
                if let Some(n1) = n1
                    .and_then(|n1| XmlNodePtr::try_from(n1).ok())
                    .filter(|n1| n1.element_type() == XmlElementType::XmlElementNode)
                    .filter(|n1| 0 > n1.content as isize)
                {
                    node1 = n1.into();
                    misc = 1;
                } else {
                    // Fallback for whatever case.
                    node1 = misc_node1.unwrap();
                    precedence1 = 0;
                }
            }
            XmlElementType::XmlNamespaceDecl => {
                // TODO: why do we return 1 for namespace nodes?
                return Some(std::cmp::Ordering::Less);
            }
            _ => {}
        }

        match node2.element_type() {
            XmlElementType::XmlElementNode => {}
            XmlElementType::XmlAttributeNode => {
                precedence2 = 1; /* element is owner */
                misc_node2 = Some(node2);
                node2 = node2.parent().unwrap();
                misc = 1;
            }
            XmlElementType::XmlTextNode
            | XmlElementType::XmlCDATASectionNode
            | XmlElementType::XmlCommentNode
            | XmlElementType::XmlPINode => {
                misc_node2 = Some(node2);
                let n2 = if let Some(mut prev2) = node2.prev() {
                    loop {
                        node2 = prev2;
                        if matches!(node2.element_type(), XmlElementType::XmlElementNode) {
                            precedence2 = 3; /* element in prev-sibl axis */
                            break Some(node2);
                        }
                        let Some(prev) = node2.prev() else {
                            precedence2 = 2; /* element is parent */
                            break node2.parent();
                        };
                        prev2 = prev;
                    }
                } else {
                    precedence2 = 2; /* element is parent */
                    node2.parent()
                };
                if let Some(n2) = n2
                    .and_then(|n2| XmlNodePtr::try_from(n2).ok())
                    .filter(|node| node.element_type() == XmlElementType::XmlElementNode)
                    .filter(|node| 0 > node.content as isize)
                {
                    node2 = n2.into();
                    misc = 1;
                } else {
                    node2 = misc_node2.unwrap();
                    precedence2 = 0;
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
                    let mut cur = misc_node2.unwrap().prev();
                    while let Some(now) = cur {
                        if cur == misc_node1 {
                            return Some(std::cmp::Ordering::Less);
                        }
                        if matches!(now.element_type(), XmlElementType::XmlElementNode) {
                            return Some(std::cmp::Ordering::Greater);
                        }
                        cur = now.prev();
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
                let mut cur = node1.parent();
                while let Some(now) = cur {
                    if now == node2 {
                        return Some(std::cmp::Ordering::Less);
                    }
                    cur = now.parent();
                }
            }
            if precedence1 == 3 && precedence2 > 1 {
                let mut cur = node2.parent();
                while let Some(now) = cur {
                    if now == node1 {
                        return Some(std::cmp::Ordering::Greater);
                    }
                    cur = now.parent();
                }
            }
        }

        // Speedup using document order if available.
        if let Some((node1, node2)) = XmlNodePtr::try_from(node1)
            .ok()
            .zip(XmlNodePtr::try_from(node2).ok())
        {
            if node1.element_type() == XmlElementType::XmlElementNode
                && node2.element_type() == XmlElementType::XmlElementNode
                && 0 > node1.content as isize
                && 0 > node2.content as isize
                && node1.doc == node2.doc
            {
                let l1 = -(node1.content as isize);
                let l2 = -(node2.content as isize);
                if l1 < l2 {
                    return Some(std::cmp::Ordering::Less);
                }
                if l1 > l2 {
                    return Some(std::cmp::Ordering::Greater);
                }
            }
        }
    }

    // turtle_comparison:

    if Some(node1) == node2.prev() {
        return Some(std::cmp::Ordering::Less);
    }
    if Some(node1) == node2.next() {
        return Some(std::cmp::Ordering::Greater);
    }

    // compute depth to root
    let mut depth2 = 0;
    let mut cur = node2;
    while let Some(parent) = cur.parent() {
        if parent == node1 {
            return Some(std::cmp::Ordering::Less);
        }
        depth2 += 1;
        cur = parent;
    }
    let root = cur;
    let mut depth1 = 0;
    let mut cur = node1;
    while let Some(parent) = cur.parent() {
        if parent == node2 {
            return Some(std::cmp::Ordering::Greater);
        }
        depth1 += 1;
        cur = parent;
    }
    // Distinct document (or distinct entities :-( ) case.
    if root != cur {
        return None;
    }
    // get the nearest common ancestor.
    while depth1 > depth2 {
        depth1 -= 1;
        node1 = node1.parent().unwrap();
    }
    while depth2 > depth1 {
        depth2 -= 1;
        node2 = node2.parent().unwrap();
    }
    while node1.parent() != node2.parent() {
        // should not happen but just in case ...
        node1 = node1.parent()?;
        node2 = node2.parent()?;
    }
    // Find who's first.
    if Some(node1) == node2.prev() {
        return Some(std::cmp::Ordering::Less);
    }
    if Some(node1) == node2.next() {
        return Some(std::cmp::Ordering::Greater);
    }
    // Speedup using document order if available.
    if let Some((node1, node2)) = XmlNodePtr::try_from(node1)
        .ok()
        .zip(XmlNodePtr::try_from(node2).ok())
    {
        if node1.element_type() == XmlElementType::XmlElementNode
            && node2.element_type() == XmlElementType::XmlElementNode
            && 0 > node1.content as isize
            && 0 > node2.content as isize
            && node1.doc == node2.doc
        {
            let l1 = -(node1.content as isize);
            let l2 = -(node2.content as isize);
            if l1 < l2 {
                return Some(std::cmp::Ordering::Less);
            }
            if l1 > l2 {
                return Some(std::cmp::Ordering::Greater);
            }
        }
    }

    let mut cur = node1.next();
    while let Some(now) = cur {
        if now == node2 {
            return Some(std::cmp::Ordering::Less);
        }
        cur = now.next();
    }
    // assume there is no sibling list corruption
    Some(std::cmp::Ordering::Greater)
}

#[doc(alias = "xmlXPathInit")]
#[cfg(any(feature = "xpath", feature = "schema"))]
#[deprecated = "Alias for xmlInitParser"]
pub unsafe fn xml_xpath_init() {
    unsafe {
        xml_init_parser();
    }
}

/// Returns 1 if the value is a NaN, 0 otherwise
#[doc(alias = "xmlXPathIsNaN")]
#[cfg(any(feature = "xpath", feature = "schema"))]
pub fn xml_xpath_is_nan(val: f64) -> bool {
    val.is_nan()
}

/// Returns 1 if the value is +Infinite, -1 if -Infinite, 0 otherwise
#[doc(alias = "xmlXPathIsInf")]
#[cfg(any(feature = "xpath", feature = "schema"))]
pub fn xml_xpath_is_inf(val: f64) -> i32 {
    if val.is_infinite() {
        if val > 0.0 { 1 } else { -1 }
    } else {
        0
    }
}

/// Initialize the XPath environment
#[doc(alias = "xmlInitXPathInternal")]
pub(crate) fn xml_init_xpath_internal() {
    // In original libxml, NAN, PINF and NINF is initialized at here.
    // They are defined as constants on this crate.
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

                let ret_val = xml_xpath_cast_boolean_to_number(val != 0);
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

                let _ = xml_xpath_cast_boolean_to_string(val != 0);
                // desret_xml_char_ptr(ret_val);
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

                let _ = xml_xpath_cast_node_set_to_boolean(ns.as_deref());
                // desret_int(ret_val);
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
                let mut ns = gen_xml_node_set_ptr(n_ns, 0);

                let ret_val = xml_xpath_cast_node_set_to_number(ns.as_deref_mut());
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
                let mut ns = gen_xml_node_set_ptr(n_ns, 0);

                let _ = xml_xpath_cast_node_set_to_string(ns.as_deref_mut());
                // desret_xml_char_ptr(ret_val);
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
    fn test_xml_xpath_cast_number_to_boolean() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_DOUBLE {
                let mem_base = xml_mem_blocks();
                let val = gen_double(n_val, 0);

                let _ = xml_xpath_cast_number_to_boolean(val);
                // desret_int(ret_val);
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

                let _ = xml_xpath_cast_number_to_string(val);
                // desret_xml_char_ptr(ret_val);
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

                let _ = xml_xpath_cast_to_boolean(val);
                // desret_int(ret_val);
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

                let _ = xml_xpath_cast_to_string(val);
                // desret_xml_char_ptr(ret_val);
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

                    let _ = xml_xpath_eval_predicate(ctxt, res);
                    // desret_int(ret_val);
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

                let _ = xml_xpath_is_nan(val);
                // desret_int(ret_val);
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

                    let ret_val = xml_xpath_difference(nodes1.as_deref(), nodes2.as_deref());
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
                let mut nodes = gen_xml_node_set_ptr(n_nodes, 0);

                let ret_val = xml_xpath_distinct(nodes.as_deref_mut());
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

                let ret_val = xml_xpath_distinct_sorted(nodes.as_deref());
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
                    let mut nodes1 = gen_xml_node_set_ptr(n_nodes1, 0);
                    let mut nodes2 = gen_xml_node_set_ptr(n_nodes2, 1);

                    let ret_val = xml_xpath_leading(nodes1.as_deref_mut(), nodes2.as_deref_mut());
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

                    let ret_val = xml_xpath_leading_sorted(nodes1.as_deref(), nodes2.as_deref());
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

                let ret_val = xml_xpath_new_boolean(val != 0);
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
    fn test_xml_xpath_new_node_set_list() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_XML_NODE_SET_PTR {
                let mem_base = xml_mem_blocks();
                let mut val = gen_xml_node_set_ptr(n_val, 0);

                let ret_val = xml_xpath_new_node_set_list(val.as_deref_mut());
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
    fn test_xml_xpath_node_set_merge() {
        #[cfg(feature = "xpath")]
        unsafe {
            let mut leaks = 0;

            for n_val1 in 0..GEN_NB_XML_NODE_SET_PTR {
                for n_val2 in 0..GEN_NB_XML_NODE_SET_PTR {
                    let mem_base = xml_mem_blocks();
                    let val1 = gen_xml_node_set_ptr(n_val1, 0);
                    let val2 = gen_xml_node_set_ptr(n_val2, 1);

                    let ret_val = xml_xpath_node_set_merge(val1, val2.as_deref());
                    desret_xml_node_set_ptr(ret_val);
                    // des_xml_node_set_ptr(n_val1, val1, 0);
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

                let _ = xml_xpath_pop_string(ctxt);
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
                    let mut nodes1 = gen_xml_node_set_ptr(n_nodes1, 0);
                    let mut nodes2 = gen_xml_node_set_ptr(n_nodes2, 1);

                    let ret_val = xml_xpath_trailing(nodes1.as_deref_mut(), nodes2.as_deref_mut());
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
                // des_xml_node_set_ptr(n_val, val, 0);
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

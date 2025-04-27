//! Provide methods and data structures for processing XPath.
//!
//! This module is based on `libxml/xpath.h`, `xpath.c` and so on in `libxml2-v2.11.8`.  
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

#[cfg(feature = "xpath")]
pub mod compile;
#[cfg(feature = "xpath")]
pub mod context;
#[cfg(all(feature = "xpath", feature = "libxml_debug"))]
pub mod dump;
#[cfg(feature = "xpath")]
pub mod evaluate;
#[cfg(feature = "xpath")]
pub mod functions;
#[cfg(feature = "xpath")]
pub mod internals;
#[cfg(feature = "xpath")]
pub mod node_set;
#[cfg(feature = "xpath")]
pub mod object;

use std::{borrow::Cow, os::raw::c_void, rc::Rc};

use compile::XmlXPathStepOp;

#[cfg(feature = "libxml_pattern")]
use crate::pattern::XmlPattern;
use crate::{
    generic_error,
    libxml::xmlstring::XmlChar,
    parser::xml_init_parser,
    tree::{XmlDocPtr, XmlElementType, XmlGenericNodePtr},
};

#[cfg(feature = "xpath")]
pub use context::*;
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
pub type XmlXPathConvertFunc = fn(obj: &XmlXPathObject, typ: i32) -> i32;

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
    name: *const XmlChar,  /* the variable name */
    value: XmlXPathObject, /* the value */
}

/// An XPath evaluation function, the parameters are on the XPath context stack.
#[doc(alias = "xmlXPathEvalFunc")]
#[cfg(feature = "xpath")]
pub type XmlXPathEvalFunc = fn(ctxt: &mut XmlXPathParserContext, nargs: i32);

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
    fn(ctxt: &mut XmlXPathParserContext, cur: &XmlXPathObject) -> Option<XmlXPathObject>;

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
pub type XmlXPathFunction = fn(ctxt: &mut XmlXPathParserContext, nargs: usize);

/// Prototype for callbacks used to plug variable lookup in the XPath engine.
///
/// Returns the XPath object value or NULL if not found.
#[doc(alias = "xmlXPathVariableLookupFunc")]
#[cfg(feature = "xpath")]
pub type XmlXPathVariableLookupFunc =
    fn(ctxt: *mut c_void, name: &str, ns_uri: Option<&str>) -> Option<XmlXPathObject>;

/// Prototype for callbacks used to plug function lookup in the XPath engine.
///
/// Returns the XPath function or NULL if not found.
#[doc(alias = "xmlXPathFuncLookupFunc")]
#[cfg(feature = "xpath")]
pub trait XmlXPathFuncLookup {
    fn lookup(&self, name: &str, ns_uri: Option<&str>) -> Option<XmlXPathFunction>;
}

/// check namespaces at compilation
#[cfg(feature = "xpath")]
pub(crate) const XML_XPATH_CHECKNS: usize = 1 << 0;
/// forbid variables in expression
#[cfg(feature = "xpath")]
pub(crate) const XML_XPATH_NOVAR: usize = 1 << 1;

#[cfg(feature = "xpath")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlXPathOp {
    XPathOpEnd = 0,
    XPathOpAnd,
    XPathOpOr,
    XPathOpEqual,
    XPathOpCmp,
    XPathOpPlus,
    XPathOpMult,
    XPathOpUnion,
    XPathOpRoot,
    XPathOpNode,
    XPathOpCollect,
    XPathOpValue, /* 11 */
    XPathOpVariable,
    XPathOpFunction,
    XPathOpArg,
    XPathOpPredicate,
    XPathOpFilter, /* 16 */
    XPathOpSort,   /* 17 */
    #[cfg(feature = "libxml_xptr_locs")]
    XPathOpRangeto,
}

/// The structure of a compiled expression form is not public.
#[cfg(feature = "xpath")]
#[repr(C)]
#[derive(Clone)]
pub struct XmlXPathCompExpr {
    pub(crate) steps: Vec<XmlXPathStepOp>, /* ops for computation of this expression */
    pub(crate) last: i32,                  /* index of last step in expression */
    pub(crate) expr: Rc<str>,              /* the expression being computed */
    #[cfg(feature = "libxml_pattern")]
    pub(crate) stream: Option<Rc<XmlPattern>>,
}

impl Default for XmlXPathCompExpr {
    /// Create a new Xpath component
    ///
    /// Returns the newly allocated xmlXPathCompExprPtr or NULL in case of error
    #[doc(alias = "xmlXPathNewCompExpr")]
    fn default() -> Self {
        Self {
            steps: Vec::with_capacity(10),
            last: -1,
            expr: Default::default(),
            #[cfg(feature = "libxml_pattern")]
            stream: None,
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
pub fn xml_xpath_cmp_nodes(mut node1: XmlGenericNodePtr, mut node2: XmlGenericNodePtr) -> i32 {
    use crate::tree::{NodeCommon, XmlAttrPtr};

    let mut depth1: i32;
    let mut depth2: i32;
    let mut attr1 = 0;
    let mut attr2 = 0;
    let mut attr_node1 = None;
    let mut attr_node2 = None;

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
                    if now == XmlGenericNodePtr::from(attr1) {
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
pub fn xml_xpath_cast_node_set_to_boolean(ns: Option<&XmlNodeSet>) -> bool {
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
pub fn xml_xpath_cast_string_to_number(val: Option<&str>) -> f64 {
    xml_xpath_string_eval_number(val)
}

/// Converts a node to its number value
///
/// Returns the number value
#[doc(alias = "xmlXPathCastNodeToNumber")]
#[cfg(feature = "xpath")]
pub fn xml_xpath_cast_node_to_number(node: Option<XmlGenericNodePtr>) -> f64 {
    if node.is_none() {
        return XML_XPATH_NAN;
    }
    let strval = xml_xpath_cast_node_to_string(node);
    let ret: f64 = xml_xpath_cast_string_to_number(Some(&strval));

    ret
}

/// Converts a node-set to its number value
///
/// Returns the number value
#[doc(alias = "xmlXPathCastNodeSetToNumber")]
#[cfg(feature = "xpath")]
pub fn xml_xpath_cast_node_set_to_number(ns: Option<&mut XmlNodeSet>) -> f64 {
    let Some(ns) = ns else {
        return XML_XPATH_NAN;
    };
    let s = xml_xpath_cast_node_set_to_string(Some(ns));
    xml_xpath_cast_string_to_number(Some(&s))
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
pub fn xml_xpath_cast_node_to_string(node: Option<XmlGenericNodePtr>) -> String {
    node.and_then(|node| node.get_content())
        .unwrap_or_else(|| "".to_owned())
}

/// Converts a node-set to its string value.
///
/// Returns a newly allocated string.
#[doc(alias = "xmlXPathCastNodeSetToString")]
#[cfg(feature = "xpath")]
pub fn xml_xpath_cast_node_set_to_string(ns: Option<&mut XmlNodeSet>) -> Cow<'static, str> {
    let Some(ns) = ns.filter(|n| !n.is_empty()) else {
        return "".into();
    };

    if ns.len() > 1 {
        ns.sort();
    }

    xml_xpath_cast_node_to_string(Some(ns.node_tab[0])).into()
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
pub fn xml_xpath_order_doc_elems(doc: XmlDocPtr) -> i64 {
    use crate::tree::{NodeCommon, XmlNodePtr};

    let mut count = 0;

    let mut cur = doc.children;
    while let Some(mut now) = cur {
        if let Some(node) = XmlNodePtr::try_from(now)
            .ok()
            .filter(|node| node.element_type() == XmlElementType::XmlElementNode)
        {
            count += 1;
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
            if cur == XmlGenericNodePtr::from(doc) {
                break None;
            }
            if let Some(next) = cur.next() {
                break Some(next);
            }
            now = cur;
        }
    }
    count
}

/// Compile an XPath expression
///
/// Returns the xmlXPathCompExprPtr resulting from the compilation or NULL.
/// The caller has to free the object.
#[doc(alias = "xmlXPathCompile")]
#[cfg(feature = "xpath")]
pub fn xml_xpath_compile(xpath: &str) -> Option<XmlXPathCompExpr> {
    xml_xpath_ctxt_compile(None, xpath)
}

/// Compile an XPath expression
///
/// Returns the xmlXPathCompExprPtr resulting from the compilation or NULL.
/// The caller has to free the object.
#[doc(alias = "xmlXPathCtxtCompile")]
#[cfg(feature = "xpath")]
pub fn xml_xpath_ctxt_compile(
    mut ctxt: Option<&mut XmlXPathContext>,
    xpath: &str,
) -> Option<XmlXPathCompExpr> {
    use std::mem::take;

    #[cfg(feature = "libxml_pattern")]
    if let Some(ctxt) = ctxt.as_mut() {
        if let Some(comp) = ctxt.try_stream_compile(xpath) {
            return Some(comp);
        }
    }

    xml_init_parser();

    let mut comp = if let Some(ctxt) = ctxt {
        let mut pctxt = XmlXPathParserContext::new(xpath, ctxt);

        let old_depth = pctxt.context.depth;
        pctxt.compile_expr(true);
        pctxt.context.depth = old_depth;

        if pctxt.error != XmlXPathError::XPathExpressionOK as i32 {
            return None;
        }

        if pctxt.cur < pctxt.base.len() {
            // aleksey: in some cases this line prints *second* error message
            // (see bug #78858) and probably this should be fixed.
            // However, we are not sure that all error messages are printed
            // out in other places. It's not critical so we leave it as-is for now
            xml_xpath_err(Some(&mut pctxt), XmlXPathError::XPathExprError as i32);
            None
        } else {
            // comp = pctxt.comp;
            if pctxt.comp.steps.len() > 1 && pctxt.comp.last >= 0 {
                let old_depth = pctxt.context.depth;
                pctxt.optimize_expression(pctxt.comp.last as usize);
                pctxt.context.depth = old_depth;
            }
            Some(take(&mut pctxt.comp))
            // pctxt.comp = null_mut();
        }
    } else {
        let mut ctxt = XmlXPathContext::default();
        let mut pctxt = XmlXPathParserContext::new(xpath, &mut ctxt);

        pctxt.compile_expr(true);

        if pctxt.error != XmlXPathError::XPathExpressionOK as i32 {
            return None;
        }

        if pctxt.cur < pctxt.base.len() {
            // aleksey: in some cases this line prints *second* error message
            // (see bug #78858) and probably this should be fixed.
            // However, we are not sure that all error messages are printed
            // out in other places. It's not critical so we leave it as-is for now
            xml_xpath_err(Some(&mut pctxt), XmlXPathError::XPathExprError as i32);
            None
        } else {
            // comp = pctxt.comp;
            if pctxt.comp.steps.len() > 1 && pctxt.comp.last >= 0 {
                pctxt.optimize_expression(pctxt.comp.last as usize);
            }
            Some(take(&mut pctxt.comp))
            // pctxt.comp = null_mut();
        }
    };
    if let Some(comp) = comp.as_mut() {
        comp.expr = xpath.into();
    }
    comp
}

/// Evaluate the Precompiled XPath expression in the given context.
/// The caller has to free @resObj.
///
/// Returns the let resulting: xmlXPathObjectPtr from the evaluation or NULL.
/// The caller has to free the object.
#[doc(alias = "xmlXPathCompiledEvalInternal")]
fn xml_xpath_compiled_eval_internal(
    comp: XmlXPathCompExpr,
    ctxt: &mut XmlXPathContext,
    res_obj_ptr: &mut Option<XmlXPathObject>,
    to_bool: bool,
) -> i32 {
    xml_init_parser();

    let mut pctxt = XmlXPathParserContext::from_compiled_expression(comp, ctxt);
    let res: i32 = pctxt.run_evaluate(to_bool);

    let res_obj = if pctxt.error != XmlXPathError::XPathExpressionOK as i32 {
        None
    } else {
        let res_obj = pctxt.value_pop();
        if res_obj.is_none() {
            if !to_bool {
                generic_error!("xmlXPathCompiledEval: No result on the stack.\n");
            }
        } else if !pctxt.value_tab.is_empty() {
            generic_error!(
                "xmlXPathCompiledEval: {} object(s) left on the stack.\n",
                pctxt.value_tab.len() as i32
            );
        }
        res_obj
    };

    *res_obj_ptr = res_obj;

    res
}

/// Evaluate the Precompiled XPath expression in the given context.
///
/// Returns the let resulting: xmlXPathObjectPtr from the evaluation or NULL.
/// The caller has to free the object.
#[doc(alias = "xmlXPathCompiledEval")]
#[cfg(feature = "xpath")]
pub fn xml_xpath_compiled_eval(
    comp: XmlXPathCompExpr,
    ctx: &mut XmlXPathContext,
) -> Option<XmlXPathObject> {
    let mut ret = None;
    xml_xpath_compiled_eval_internal(comp, ctx, &mut ret, false);
    ret
}

/// Applies the XPath boolean() function on the result of the given
/// compiled expression.
///
/// Returns 1 if the expression evaluated to true, 0 if to false and
/// -1 in API and internal errors.
#[doc(alias = "xmlXPathCompiledEvalToBoolean")]
#[cfg(feature = "xpath")]
pub fn xml_xpath_compiled_eval_to_boolean(
    comp: XmlXPathCompExpr,
    ctxt: &mut XmlXPathContext,
) -> i32 {
    xml_xpath_compiled_eval_internal(comp, ctxt, &mut None, true)
}

/// Compare two nodes w.r.t document order.
/// This one is optimized for handling of non-element nodes.
///
/// Returns -2 in case of error 1 if first point < second point, 0
/// if it's the same node, -1 otherwise
#[doc(alias = "xmlXPathCmpNodesExt")]
fn xml_xpath_cmp_nodes_ext(
    mut node1: XmlGenericNodePtr,
    mut node2: XmlGenericNodePtr,
) -> Option<std::cmp::Ordering> {
    let mut misc: i32 = 0;
    let mut precedence1: i32 = 0;
    let mut precedence2: i32 = 0;
    let mut misc_node1 = None;
    let mut misc_node2 = None;

    if node1 == node2 {
        return Some(std::cmp::Ordering::Equal);
    }

    // a couple of optimizations which will avoid computations in most cases
    'turtle_comparison: {
        match node1.element_type() {
            XmlElementType::XmlElementNode => {
                break 'turtle_comparison;
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
                precedence1 = 0;
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
                precedence2 = 0;
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
}

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

use std::{borrow::Cow, collections::HashMap, ffi::c_void, ptr::null_mut, rc::Rc};

use crate::{
    chvalid::XmlCharValid,
    error::XmlError,
    globals::{GenericErrorContext, StructuredError},
    tree::{XML_XML_NAMESPACE, XmlDocPtr, XmlGenericNodePtr, XmlNsPtr},
};

use super::{
    XPATH_MAX_STACK_DEPTH, XmlNodeSet, XmlXPathAxisPtr, XmlXPathCompExpr, XmlXPathError,
    XmlXPathFuncLookup, XmlXPathFunction, XmlXPathObject, XmlXPathObjectType, XmlXPathOp,
    XmlXPathTypePtr, XmlXPathVariableLookupFunc,
    compile::XmlXPathStepOp,
    functions::{
        xml_xpath_boolean_function, xml_xpath_ceiling_function, xml_xpath_concat_function,
        xml_xpath_contains_function, xml_xpath_count_function, xml_xpath_escape_uri_function,
        xml_xpath_false_function, xml_xpath_floor_function, xml_xpath_id_function,
        xml_xpath_lang_function, xml_xpath_last_function, xml_xpath_local_name_function,
        xml_xpath_name_function, xml_xpath_namespace_uri_function, xml_xpath_normalize_function,
        xml_xpath_not_function, xml_xpath_number_function, xml_xpath_position_function,
        xml_xpath_round_function, xml_xpath_starts_with_function, xml_xpath_string_function,
        xml_xpath_string_length_function, xml_xpath_substring_after_function,
        xml_xpath_substring_before_function, xml_xpath_substring_function, xml_xpath_sum_function,
        xml_xpath_translate_function, xml_xpath_true_function,
    },
    xml_xpath_cast_to_boolean, xml_xpath_cast_to_number, xml_xpath_cast_to_string, xml_xpath_err,
    xml_xpath_object_copy, xml_xpath_perr_memory,
};

/// An XPath parser context. It contains pure parsing information,
/// an xmlXPathContext, and the stack of objects.
#[doc(alias = "xmlXPathParserContext")]
#[repr(C)]
pub struct XmlXPathParserContext<'a> {
    pub(crate) cur: usize,     /* the current char being parsed */
    pub(crate) base: Box<str>, /* the full expression */

    pub(crate) error: i32, /* error code */

    pub(crate) context: &'a mut XmlXPathContext, /* the evaluation context */
    pub(crate) value_tab: Vec<XmlXPathObject>,   /* stack of values */

    pub(crate) comp: XmlXPathCompExpr, /* the precompiled expression */
    pub(crate) xptr: i32,              /* it this an XPointer expression */
    pub(crate) ancestor: Option<XmlGenericNodePtr>, /* used for walking preceding axis */

    pub(crate) value_frame: i32, /* unused */
}

impl<'a> XmlXPathParserContext<'a> {
    /// Create a new xmlXPathParserContext
    ///
    /// Returns the xmlXPathParserContext just allocated.
    #[doc(alias = "xmlXPathNewParserContext")]
    pub fn new(xpath: &str, ctxt: &'a mut XmlXPathContext) -> Self {
        Self {
            cur: 0,
            base: xpath.into(),
            error: 0,
            context: ctxt,
            value_tab: vec![],
            comp: XmlXPathCompExpr::default(),
            xptr: 0,
            ancestor: None,
            value_frame: 0,
        }
    }

    /// Create a new xmlXPathParserContext when processing a compiled expression
    ///
    /// Returns the xmlXPathParserContext just allocated.
    #[doc(alias = "xmlXPathCompParserContext")]
    pub(super) fn from_compiled_expression(
        comp: XmlXPathCompExpr,
        ctxt: &'a mut XmlXPathContext,
    ) -> Self {
        Self {
            cur: 0,
            base: "".into(),
            error: 0,
            context: ctxt,
            value_tab: vec![],
            comp,
            xptr: 0,
            ancestor: None,
            value_frame: 0,
        }
    }

    pub(crate) fn next_char(&mut self) -> Option<char> {
        let res = self.current_char()?;
        self.cur += res.len_utf8();
        Some(res)
    }

    pub(crate) fn current_char(&self) -> Option<char> {
        self.current_str().chars().next()
    }

    pub(crate) fn current_str(&self) -> &str {
        &self.base[self.cur..]
    }

    pub(crate) fn nth_byte(&self, index: usize) -> Option<u8> {
        self.current_str().as_bytes().get(index).copied()
    }

    pub(crate) fn skip_blanks(&mut self) {
        let rem = self
            .current_str()
            .trim_start_matches(|c: char| c.is_xml_blank_char());
        let diff = self.current_str().len() - rem.len();
        self.cur += diff;
    }

    pub(crate) fn value(&self) -> Option<&XmlXPathObject> {
        self.value_tab.last()
    }

    pub(crate) fn value_mut(&mut self) -> Option<&mut XmlXPathObject> {
        self.value_tab.last_mut()
    }

    /// Check if the current value on the XPath stack is a node set or an XSLT value tree.
    ///
    /// Returns true if the current object on the stack is a node-set.
    #[doc(alias = "xmlXPathStackIsNodeSet")]
    fn stack_is_node_set(&self) -> bool {
        self.value().is_some_and(|value| {
            value.typ == XmlXPathObjectType::XPathNodeset
                || value.typ == XmlXPathObjectType::XPathXSLTTree
        })
    }

    #[doc(alias = "xmlXPathIsPositionalPredicate")]
    pub(super) fn is_positional_predicate(&self, op: &XmlXPathStepOp, max_pos: &mut i32) -> i32 {
        // BIG NOTE: This is not intended for XPATH_OP_FILTER yet!

        // If not -1, then ch1 will point to:
        // 1) For predicates (XPATH_OP_PREDICATE):
        //    - an inner predicate operator
        // 2) For filters (XPATH_OP_FILTER):
        //    - an inner filter operator OR
        //    - an expression selecting the node set.
        //      E.g. "key('a', 'b')" or "(//foo | //bar)".
        if !matches!(
            op.op,
            XmlXPathOp::XPathOpPredicate | XmlXPathOp::XPathOpFilter
        ) {
            return 0;
        }

        if op.ch2 == -1 || op.ch2 >= self.comp.steps.len() as i32 {
            return 0;
        }
        let expr_op = &self.comp.steps[op.ch2 as usize];

        if let Some(obj) = expr_op
            .value4
            .as_ref()
            .and_then(|val| val.as_object())
            .filter(|&obj| {
                matches!(expr_op.op, XmlXPathOp::XPathOpValue)
                    && matches!(obj.typ, XmlXPathObjectType::XPathNumber)
            })
        {
            let floatval: f64 = obj.floatval;

            // We have a "[n]" predicate here.
            // TODO: Unfortunately this simplistic test here is not
            // able to detect a position() predicate in compound
            // expressions like "[@attr = 'a" and position() = 1],
            // and even not the usage of position() in
            // "[position() = 1]"; thus - obviously - a position-range,
            // like it "[position() < 5]", is also not detected.
            // Maybe we could rewrite the AST to ease the optimization.

            if floatval > i32::MIN as f64 && floatval < i32::MAX as f64 {
                *max_pos = floatval as i32;
                if floatval == *max_pos as f64 {
                    return 1;
                }
            }
        }
        0
    }

    /// Pushes a new XPath object on top of the value stack. If value is NULL,
    /// a memory error is recorded in the parser context.
    ///
    /// Returns the number of items on the value stack, or -1 in case of error.
    ///
    /// The object is destroyed in case of error.
    #[doc(alias = "valuePush")]
    pub fn value_push(&mut self, value: XmlXPathObject) -> i32 {
        if self.value_tab.len() == XPATH_MAX_STACK_DEPTH {
            xml_xpath_perr_memory(Some(self), Some("XPath stack depth limit reached\n"));
            return -1;
        }
        self.value_tab.push(value);
        self.value_tab.len() as i32 - 1
    }

    // TODO: remap to xmlXPathValuePop and Push.
    /// Pops the top XPath object from the value stack
    ///
    /// Returns the XPath object just removed
    #[doc(alias = "valuePop")]
    pub fn value_pop(&mut self) -> Option<XmlXPathObject> {
        self.value_tab.pop()
    }

    /// Pops a number from the stack, handling conversion if needed.
    /// Check error with #xmlXPathCheckError.
    ///
    /// Returns the number
    #[doc(alias = "xmlXPathPopNumber")]
    pub fn pop_number(&mut self) -> f64 {
        let Some(mut obj) = self.value_pop() else {
            xml_xpath_err(Some(self), XmlXPathError::XPathInvalidOperand as i32);
            self.error = XmlXPathError::XPathInvalidOperand as i32;
            return 0.0;
        };
        if obj.typ != XmlXPathObjectType::XPathNumber {
            xml_xpath_cast_to_number(&mut obj)
        } else {
            obj.floatval
        }
    }

    /// Pops a boolean from the stack, handling conversion if needed.
    /// Check error with #xmlXPathCheckError.
    ///
    /// Returns the boolean
    #[doc(alias = "xmlXPathPopBoolean")]
    pub fn pop_boolean(&mut self) -> bool {
        let Some(obj) = self.value_pop() else {
            xml_xpath_err(Some(self), XmlXPathError::XPathInvalidOperand as i32);
            self.error = XmlXPathError::XPathInvalidOperand as i32;
            return false;
        };
        if obj.typ != XmlXPathObjectType::XPathBoolean {
            xml_xpath_cast_to_boolean(&obj)
        } else {
            obj.boolval
        }
    }

    /// Pops a string from the stack, handling conversion if needed.
    /// Check error with #xmlXPathCheckError.
    ///
    /// Returns the string
    #[doc(alias = "xmlXPathPopString")]
    pub fn pop_string(&mut self) -> Option<Cow<'static, str>> {
        let Some(mut obj) = self.value_pop() else {
            xml_xpath_err(Some(self), XmlXPathError::XPathInvalidOperand as i32);
            self.error = XmlXPathError::XPathInvalidOperand as i32;
            return None;
        };
        let ret = xml_xpath_cast_to_string(&mut obj); /* this does required strdup */
        /* TODO: needs refactoring somewhere else */
        // if (*obj).stringval == ret {
        //     (*obj).stringval = null_mut();
        // }
        Some(ret)
    }

    /// Pops a node-set from the stack, handling conversion if needed.
    /// Check error with #xmlXPathCheckError.
    ///
    /// Returns the node-set
    #[doc(alias = "xmlXPathPopNodeSet")]
    pub fn pop_node_set(&mut self) -> Option<Box<XmlNodeSet>> {
        if self.value().is_none() {
            xml_xpath_err(Some(self), XmlXPathError::XPathInvalidOperand as i32);
            self.error = XmlXPathError::XPathInvalidOperand as i32;
            return None;
        }
        if !self.stack_is_node_set() {
            xml_xpath_err(Some(self), XmlXPathError::XPathInvalidType as i32);
            self.error = XmlXPathError::XPathInvalidType as i32;
            return None;
        }
        let mut obj = self.value_pop().unwrap();
        obj.nodesetval.take()
        // #if 0
        // /* to fix memory leak of not clearing (*obj).user */
        // if ((*obj).boolval && !(*obj).user.is_null())
        //     xmlFreeNodeList((xmlNodePtr) (*obj).user);
        // #endif
    }

    /// Pops an external object from the stack, handling conversion if needed.
    /// Check error with #xmlXPathCheckError.
    ///
    /// Returns the object
    #[doc(alias = "xmlXPathPopExternal")]
    pub fn pop_external(&mut self) -> *mut c_void {
        let Some(value) = self.value() else {
            xml_xpath_err(Some(self), XmlXPathError::XPathInvalidOperand as i32);
            self.error = XmlXPathError::XPathInvalidOperand as i32;
            return null_mut();
        };
        if value.typ != XmlXPathObjectType::XPathUsers {
            xml_xpath_err(Some(self), XmlXPathError::XPathInvalidType as i32);
            self.error = XmlXPathError::XPathInvalidType as i32;
            return null_mut();
        }
        let mut obj = self.value_pop().unwrap();
        obj.user
            .take()
            .and_then(|ret| ret.as_external().copied())
            .unwrap_or(null_mut())
    }
}

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
#[repr(C)]
pub struct XmlXPathContext {
    // The current document
    pub doc: Option<XmlDocPtr>,
    // The current node
    pub node: Option<XmlGenericNodePtr>,

    // Hash table of defined variables
    // Key: (Name, Namespace URI)
    pub(crate) var_hash: HashMap<(Cow<'static, str>, Option<Cow<'static, str>>), XmlXPathObject>,

    // number of defined types
    pub(crate) nb_types: i32,
    // max number of types
    pub(crate) max_types: i32,
    // Array of defined types
    pub(crate) types: XmlXPathTypePtr,

    // Hash table of defined funcs
    pub(crate) func_hash: HashMap<(Option<Cow<'static, str>>, Cow<'static, str>), XmlXPathFunction>,

    // number of defined axis
    pub(crate) nb_axis: i32,
    // max number of axis
    pub(crate) max_axis: i32,
    // Array of defined axis
    pub(crate) axis: XmlXPathAxisPtr,

    // the namespace nodes of the context node
    // Array of namespaces
    pub(crate) namespaces: Option<Vec<XmlNsPtr>>,
    // function to free
    pub(crate) user: *mut c_void,

    // extra variables
    // the context size
    pub(crate) context_size: i32,
    // the proximity position
    pub(crate) proximity_position: i32,

    // extra stuff for XPointer
    // is this an XPointer context?
    pub(crate) xptr: i32,
    // for here()
    pub(crate) here: Option<XmlGenericNodePtr>,
    // for origin()
    pub(crate) origin: Option<XmlGenericNodePtr>,

    // the set of namespace declarations in scope for the expression
    // The namespaces hash table
    pub(crate) ns_hash: HashMap<Cow<'static, str>, Rc<str>>,
    // variable lookup func
    pub(crate) var_lookup_func: Option<XmlXPathVariableLookupFunc>,
    // variable lookup data
    pub(crate) var_lookup_data: *mut c_void,

    // Possibility to link in an extra item
    // needed for XSLT
    pub(crate) extra: *mut c_void,

    // The function name and URI when calling a function
    pub(crate) function: Option<Rc<str>>,
    pub(crate) function_uri: Option<Rc<str>>,

    // function lookup function and data
    // function lookup func
    pub(crate) func_lookup: Option<Box<dyn XmlXPathFuncLookup>>,

    // temporary namespace lists kept for walking the namespace axis
    // Array of namespaces
    pub(crate) tmp_ns_list: Option<Vec<XmlNsPtr>>,
    // number of namespaces in scope
    pub(crate) tmp_ns_nr: i32,

    // error reporting mechanism
    // user specific data block
    pub(crate) user_data: Option<GenericErrorContext>,
    // the callback in case of errors
    pub(crate) error: Option<StructuredError>,
    // the last error
    pub(crate) last_error: XmlError,
    // the source node XSLT
    pub(crate) debug_node: Option<XmlGenericNodePtr>,

    // flags to control compilation
    pub(crate) flags: i32,

    // Resource limits
    pub(crate) op_limit: u64,
    pub(crate) op_count: u64,
    pub(crate) depth: i32,
}

impl XmlXPathContext {
    /// Create a new xmlXPathContext
    ///
    /// Returns the xmlXPathContext just allocated. The caller will need to free it.
    #[doc(alias = "xmlXPathNewContext")]
    pub fn new(doc: Option<XmlDocPtr>) -> Self {
        let mut ret = XmlXPathContext::default();
        ret.doc = doc;
        ret.node = None;
        ret.nb_types = 0;
        ret.max_types = 0;
        ret.types = null_mut();
        ret.func_hash = HashMap::new();
        ret.nb_axis = 0;
        ret.max_axis = 0;
        ret.axis = null_mut();
        ret.user = null_mut();
        ret.context_size = -1;
        ret.proximity_position = -1;
        ret.register_all_functions();
        ret
    }

    /// Sets 'node' as the context node. The node must be in the same
    /// document as that associated with the context.
    ///
    /// Returns -1 in case of error or 0 if successful
    #[doc(alias = "xmlXPathSetContextNode")]
    #[cfg(feature = "xpath")]
    pub fn set_context_node(&mut self, node: XmlGenericNodePtr) -> i32 {
        if node.document() == self.doc {
            self.node = Some(node);
            return 0;
        }
        -1
    }

    /// Register a new function. If @f is NULL it unregisters the function
    ///
    /// Returns 0 in case of success, -1 in case of error
    #[doc(alias = "xmlXPathRegisterFunc")]
    pub fn register_function(
        &mut self,
        name: Cow<'static, str>,
        f: Option<XmlXPathFunction>,
    ) -> i32 {
        self.register_function_ns(name, None, f)
    }

    /// Register a new function. If @f is NULL it unregisters the function
    ///
    /// Returns 0 in case of success, -1 in case of error
    #[doc(alias = "xmlXPathRegisterFuncNS")]
    pub fn register_function_ns(
        &mut self,
        name: Cow<'static, str>,
        ns_uri: Option<Cow<'static, str>>,
        f: Option<XmlXPathFunction>,
    ) -> i32 {
        let res = if let Some(f) = f {
            self.func_hash.insert((ns_uri, name), f).is_some()
        } else {
            self.func_hash.remove(&(ns_uri, name)).is_none()
        };
        -(res as i32)
    }

    /// Register a new namespace. If @ns_uri is NULL it unregisters the namespace
    ///
    /// Returns 0 in case of success, -1 in case of error
    #[doc(alias = "xmlXPathRegisterNs")]
    pub fn register_ns(&mut self, prefix: &str, ns_uri: Option<&str>) -> i32 {
        if prefix.is_empty() {
            return -1;
        }

        let Some(ns_uri) = ns_uri else {
            return if self.ns_hash.remove(prefix).is_some() {
                0
            } else {
                -1
            };
        };

        self.ns_hash.insert(prefix.to_owned().into(), ns_uri.into());
        0
    }

    /// Registers all default XPath functions in this context
    #[doc(alias = "xmlXPathRegisterAllFunctions")]
    pub fn register_all_functions(&mut self) {
        self.register_function("boolean".into(), Some(xml_xpath_boolean_function));
        self.register_function("ceiling".into(), Some(xml_xpath_ceiling_function));
        self.register_function("count".into(), Some(xml_xpath_count_function));
        self.register_function("concat".into(), Some(xml_xpath_concat_function));
        self.register_function("contains".into(), Some(xml_xpath_contains_function));
        self.register_function("id".into(), Some(xml_xpath_id_function));
        self.register_function("false".into(), Some(xml_xpath_false_function));
        self.register_function("floor".into(), Some(xml_xpath_floor_function));
        self.register_function("last".into(), Some(xml_xpath_last_function));
        self.register_function("lang".into(), Some(xml_xpath_lang_function));
        self.register_function("local-name".into(), Some(xml_xpath_local_name_function));
        self.register_function("not".into(), Some(xml_xpath_not_function));
        self.register_function("name".into(), Some(xml_xpath_name_function));
        self.register_function(
            "namespace-uri".into(),
            Some(xml_xpath_namespace_uri_function),
        );
        self.register_function("normalize-space".into(), Some(xml_xpath_normalize_function));
        self.register_function("number".into(), Some(xml_xpath_number_function));
        self.register_function("position".into(), Some(xml_xpath_position_function));
        self.register_function("round".into(), Some(xml_xpath_round_function));
        self.register_function("string".into(), Some(xml_xpath_string_function));
        self.register_function(
            "string-length".into(),
            Some(xml_xpath_string_length_function),
        );
        self.register_function("starts-with".into(), Some(xml_xpath_starts_with_function));
        self.register_function("substring".into(), Some(xml_xpath_substring_function));
        self.register_function(
            "substring-before".into(),
            Some(xml_xpath_substring_before_function),
        );
        self.register_function(
            "substring-after".into(),
            Some(xml_xpath_substring_after_function),
        );
        self.register_function("sum".into(), Some(xml_xpath_sum_function));
        self.register_function("true".into(), Some(xml_xpath_true_function));
        self.register_function("translate".into(), Some(xml_xpath_translate_function));

        self.register_function_ns(
            "escape-uri".into(),
            Some("http://www.w3.org/2002/08/xquery-functions".into()),
            Some(xml_xpath_escape_uri_function),
        );
    }

    /// Search in the Function array of the context for the given function.
    ///
    /// Returns the xmlXPathFunction or NULL if not found
    #[doc(alias = "xmlXPathFunctionLookup")]
    pub fn lookup_function(&self, name: &str) -> Option<XmlXPathFunction> {
        if let Some(ret) = self
            .func_lookup
            .as_deref()
            .and_then(|f| f.lookup(name, None))
        {
            return Some(ret);
        }

        self.lookup_function_ns(name, None)
    }

    /// Search in the Function array of the context for the given function.
    ///
    /// Returns the xmlXPathFunction or NULL if not found
    #[doc(alias = "xmlXPathFunctionLookupNS")]
    pub fn lookup_function_ns(&self, name: &str, ns_uri: Option<&str>) -> Option<XmlXPathFunction> {
        if let Some(ret) = self
            .func_lookup
            .as_deref()
            .and_then(|f| f.lookup(name, ns_uri))
        {
            return Some(ret);
        }

        self.func_hash
            .get(&(ns_uri.map(Cow::Borrowed), Cow::Borrowed(name)))
            .copied()
    }

    /// Search in the namespace declaration array of the context for the given
    /// namespace name associated to the given prefix
    ///
    /// Returns the value or NULL if not found
    #[doc(alias = "xmlXPathNsLookup")]
    pub fn lookup_ns(&self, prefix: &str) -> Option<Rc<str>> {
        if prefix == "xml" {
            return Some(XML_XML_NAMESPACE.into());
        }

        if let Some(namespaces) = self.namespaces.as_deref() {
            for &ns in namespaces {
                if ns.prefix().as_deref() == Some(prefix) {
                    return ns.href.as_deref().map(|href| href.into());
                }
            }
        }

        self.ns_hash.get(prefix).cloned()
    }

    /// Search in the Variable array of the context for the given variable value.
    ///
    /// Returns a copy of the value or NULL if not found
    #[doc(alias = "xmlXPathVariableLookup")]
    pub fn lookup_variable(&mut self, name: &str) -> Option<XmlXPathObject> {
        if let Some(var_lookup_func) = self.var_lookup_func {
            return var_lookup_func(self.var_lookup_data, name, None);
        }
        self.lookup_variable_ns(name, None)
    }

    /// Search in the Variable array of the context for the given variable value.
    ///
    /// Returns the a copy of the value or NULL if not found
    #[doc(alias = "xmlXPathVariableLookupNS")]
    pub fn lookup_variable_ns(
        &mut self,
        name: &str,
        ns_uri: Option<&str>,
    ) -> Option<XmlXPathObject> {
        if let Some(var_lookup_func) = self.var_lookup_func {
            if let Some(ret) = var_lookup_func(self.var_lookup_data, name, ns_uri) {
                return Some(ret);
            }
        }

        let obj = self
            .var_hash
            .get(&(name.into(), ns_uri.map(Cow::Borrowed)))?;
        Some(xml_xpath_object_copy(obj))
    }

    /// Registers an external mechanism to do function lookup.
    #[doc(alias = "xmlXPathRegisterFuncLookup")]
    pub fn register_func_lookup(&mut self, f: impl XmlXPathFuncLookup + 'static) {
        self.func_lookup = Some(Box::new(f));
    }

    /// Register a new variable value. If @value is NULL it unregisters the variable
    ///
    /// Returns 0 in case of success, -1 in case of error
    #[doc(alias = "xmlXPathRegisterVariable")]
    pub fn register_variable(&mut self, name: &str, value: Option<XmlXPathObject>) -> i32 {
        self.register_variable_ns(name, None, value)
    }

    /// Register a new variable value. If @value is NULL it unregisters the variable
    ///
    /// Returns 0 in case of success, -1 in case of error
    #[doc(alias = "xmlXPathRegisterVariableNS")]
    pub fn register_variable_ns(
        &mut self,
        name: &str,
        ns_uri: Option<&str>,
        value: Option<XmlXPathObject>,
    ) -> i32 {
        let name = Cow::Owned(name.to_owned());
        let ns_uri = ns_uri.map(|uri| Cow::Owned(uri.to_owned()));
        let Some(value) = value else {
            return if self.var_hash.remove(&(name, ns_uri)).is_some() {
                0
            } else {
                -1
            };
        };

        if self.var_hash.insert((name, ns_uri), value).is_none() {
            0
        } else {
            -1
        }
    }

    /// Register an external mechanism to do variable lookup
    #[doc(alias = "xmlXPathRegisterVariableLookup")]
    pub fn register_variable_lookup(&mut self, f: XmlXPathVariableLookupFunc, data: *mut c_void) {
        self.var_lookup_func = Some(f);
        self.var_lookup_data = data;
    }

    /// Cleanup the XPath context data associated to registered functions
    #[doc(alias = "xmlXPathRegisteredFuncsCleanup")]
    pub fn cleanup_registered_func(&mut self) {
        self.func_hash.clear();
    }

    /// Cleanup the XPath context data associated to registered variables
    #[doc(alias = "xmlXPathRegisteredNsCleanup")]
    pub fn cleanup_registered_ns(&mut self) {
        self.ns_hash.clear();
    }

    /// Cleanup the XPath context data associated to registered variables
    #[doc(alias = "xmlXPathRegisteredVariablesCleanup")]
    pub fn cleanup_registered_variables(&mut self) {
        self.var_hash.clear();
    }
}

impl Default for XmlXPathContext {
    fn default() -> Self {
        Self {
            doc: None,
            node: None,
            var_hash: HashMap::new(),
            nb_types: 0,
            max_types: 0,
            types: null_mut(),
            func_hash: HashMap::new(),
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
            ns_hash: HashMap::default(),
            var_lookup_func: None,
            var_lookup_data: null_mut(),
            extra: null_mut(),
            function: None,
            function_uri: None,
            func_lookup: None,
            tmp_ns_list: None,
            tmp_ns_nr: 0,
            user_data: None,
            error: None,
            last_error: XmlError::default(),
            debug_node: None,
            flags: 0,
            op_limit: 0,
            op_count: 0,
            depth: 0,
        }
    }
}

impl Drop for XmlXPathContext {
    /// Free up an xmlXPathContext
    #[doc(alias = "xmlXPathFreeContext")]
    fn drop(&mut self) {
        self.cleanup_registered_ns();
        self.cleanup_registered_func();
        self.cleanup_registered_variables();
        self.last_error.reset();
    }
}

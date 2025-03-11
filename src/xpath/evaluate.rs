use std::{
    ffi::{CStr, c_void},
    ptr::{null, null_mut},
};

use crate::{
    CHECK_ERROR, XP_ERROR, generic_error,
    libxml::{
        xmlstring::xml_str_equal,
        xpointer::{
            XmlLocationSetPtr, xml_xptr_free_location_set, xml_xptr_location_set_add,
            xml_xptr_location_set_create, xml_xptr_new_range, xml_xptr_new_range_node_object,
            xml_xptr_wrap_location_set,
        },
    },
    tree::{XmlGenericNodePtr, XmlNode},
    xpath::{
        XmlXPathError, XmlXPathOp, xml_xpath_location_set_filter, xml_xpath_node_set_filter,
        xml_xpath_node_set_keep_last, xml_xpath_number_function, xml_xpath_optimize_expression,
    },
};

use super::{
    XPATH_MAX_RECURSION_DEPTH, XmlXPathCompExprPtr, XmlXPathContextPtr, XmlXPathFunction,
    XmlXPathObjectPtr, XmlXPathObjectType, XmlXPathParserContext, XmlXPathStepOpPtr,
    xml_xpath_add_values, xml_xpath_boolean_function, xml_xpath_cache_new_boolean,
    xml_xpath_cache_new_node_set, xml_xpath_cache_object_copy, xml_xpath_cast_to_boolean,
    xml_xpath_compare_values, xml_xpath_div_values, xml_xpath_equal_values, xml_xpath_err,
    xml_xpath_evaluate_predicate_result, xml_xpath_free_comp_expr, xml_xpath_free_object,
    xml_xpath_mod_values, xml_xpath_mult_values, xml_xpath_node_collect_and_test,
    xml_xpath_node_set_merge, xml_xpath_not_equal_values, xml_xpath_ns_lookup,
    xml_xpath_release_object, xml_xpath_root, xml_xpath_run_stream_eval, xml_xpath_sub_values,
    xml_xpath_try_stream_compile, xml_xpath_value_flip_sign, xml_xpath_variable_lookup,
    xml_xpath_variable_lookup_ns,
};

/// Swaps 2 operations in the compiled expression
#[doc(alias = "xmlXPathCompSwap")]
unsafe fn xml_xpath_comp_swap(op: XmlXPathStepOpPtr) {
    unsafe {
        std::mem::swap(&mut (*op).ch1, &mut (*op).ch2);
    }
}

impl XmlXPathParserContext {
    /// Adds opCount to the running total of operations and returns -1 if the
    /// operation limit is exceeded. Returns 0 otherwise.
    #[doc(alias = "xmlXPathCheckOpLimit")]
    pub(super) unsafe fn check_operation_limit(&mut self, op_count: u64) -> i32 {
        unsafe {
            let xpctxt: XmlXPathContextPtr = self.context;

            if op_count > (*xpctxt).op_limit || (*xpctxt).op_count > (*xpctxt).op_limit - op_count {
                (*xpctxt).op_count = (*xpctxt).op_limit;
                xml_xpath_err(self, XmlXPathError::XPathOpLimitExceeded as i32);
                return -1;
            }

            (*xpctxt).op_count += op_count;
            0
        }
    }

    /// Parse and evaluate an XPath expression in the given context,
    /// then push the result on the context stack
    #[doc(alias = "xmlXPathEvalExpr")]
    pub unsafe fn evaluate_expression(&mut self) {
        unsafe {
            let mut old_depth: i32 = 0;

            #[cfg(feature = "libxml_pattern")]
            let comp: XmlXPathCompExprPtr = xml_xpath_try_stream_compile(self.context, &self.base);

            match () {
                #[cfg(feature = "libxml_pattern")]
                _ if !comp.is_null() => {
                    if !self.comp.is_null() {
                        xml_xpath_free_comp_expr(self.comp);
                    }
                    self.comp = comp;
                }
                _ => {
                    if !self.context.is_null() {
                        old_depth = (*self.context).depth;
                    }
                    self.compile_expr(true);
                    if !self.context.is_null() {
                        (*self.context).depth = old_depth;
                    }
                    CHECK_ERROR!(self);

                    // Check for trailing characters.
                    if self.cur < self.base.len() {
                        XP_ERROR!(self, XmlXPathError::XPathExprError as i32);
                    }

                    if (*self.comp).steps.len() > 1 && (*self.comp).last >= 0 {
                        if !self.context.is_null() {
                            old_depth = (*self.context).depth;
                        }
                        xml_xpath_optimize_expression(
                            self,
                            &raw mut (*self.comp).steps[(*self.comp).last as usize],
                        );
                        if !self.context.is_null() {
                            (*self.context).depth = old_depth;
                        }
                    }
                }
            }

            self.run_evaluate(0);
        }
    }

    /// Evaluate the Precompiled XPath expression in the given context.
    #[doc(alias = "xmlXPathRunEval")]
    pub(crate) unsafe fn run_evaluate(&mut self, to_bool: i32) -> i32 {
        unsafe {
            if self.comp.is_null() {
                return -1;
            }

            #[cfg(feature = "libxml_pattern")]
            if !(*self.comp).stream.is_null() {
                let res: i32;

                if to_bool != 0 {
                    // Evaluation to boolean result.
                    res =
                        xml_xpath_run_stream_eval(self.context, (*self.comp).stream, null_mut(), 1);
                    if res != -1 {
                        return res;
                    }
                } else {
                    let mut res_obj: XmlXPathObjectPtr = null_mut();

                    // Evaluation to a sequence.
                    res = xml_xpath_run_stream_eval(
                        self.context,
                        (*self.comp).stream,
                        &raw mut res_obj,
                        0,
                    );

                    if res != -1 && !res_obj.is_null() {
                        self.value_push(res_obj);
                        return 0;
                    }
                    if !res_obj.is_null() {
                        xml_xpath_release_object(self.context, res_obj);
                    }
                }
                // QUESTION TODO: This falls back to normal XPath evaluation
                // if res == -1. Is this intended?
            }
            let comp: XmlXPathCompExprPtr = self.comp;
            if (*comp).last < 0 {
                generic_error!("xmlXPathRunEval: last is less than zero\n");
                return -1;
            }
            let old_depth: i32 = (*self.context).depth;
            if to_bool != 0 {
                return self.evaluate_precompiled_operation_to_boolean(
                    &raw mut (*comp).steps[(*comp).last as usize],
                    0,
                );
            } else {
                self.evaluate_precompiled_operation(&raw mut (*comp).steps[(*comp).last as usize]);
            }
            (*self.context).depth = old_depth;

            0
        }
    }

    /// Evaluate the Precompiled XPath operation
    /// Returns the number of nodes traversed
    #[doc(alias = "xmlXPathCompOpEval")]
    unsafe fn evaluate_precompiled_operation(&mut self, op: XmlXPathStepOpPtr) -> i32 {
        unsafe {
            let mut total: i32 = 0;
            let equal: i32;
            let ret: i32;
            let arg1: XmlXPathObjectPtr;
            let arg2: XmlXPathObjectPtr;

            if self.error != XmlXPathError::XPathExpressionOK as i32 {
                return 0;
            };
            if (*self.context).op_limit != 0 && self.check_operation_limit(1) < 0 {
                return 0;
            }
            if (*self.context).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
                xml_xpath_err(self, XmlXPathError::XPathRecursionLimitExceeded as i32);
                return 0;
            }
            (*self.context).depth += 1;
            let comp: XmlXPathCompExprPtr = self.comp;
            match (*op).op {
                XmlXPathOp::XPathOpEnd => {}
                XmlXPathOp::XPathOpAnd => 'to_break: {
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch1 as usize]);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    xml_xpath_boolean_function(self, 1);
                    if self.value.is_null() || !(*self.value).boolval {
                        break 'to_break;
                    }
                    arg2 = self.value_pop();
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch2 as usize]);
                    if self.error != 0 {
                        xml_xpath_free_object(arg2);
                        break 'to_break;
                    }
                    xml_xpath_boolean_function(self, 1);
                    if !self.value.is_null() {
                        (*self.value).boolval &= (*arg2).boolval;
                    }
                    xml_xpath_release_object(self.context, arg2);
                }
                XmlXPathOp::XPathOpOr => 'to_break: {
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch1 as usize]);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    xml_xpath_boolean_function(self, 1);
                    if self.value.is_null() || (*self.value).boolval {
                        break 'to_break;
                    }
                    arg2 = self.value_pop();
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch2 as usize]);
                    if self.error != 0 {
                        xml_xpath_free_object(arg2);
                        break 'to_break;
                    }
                    xml_xpath_boolean_function(self, 1);
                    if !self.value.is_null() {
                        (*self.value).boolval |= (*arg2).boolval;
                    }
                    xml_xpath_release_object(self.context, arg2);
                }
                XmlXPathOp::XPathOpEqual => {
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch1 as usize]);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch2 as usize]);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if (*op).value != 0 {
                        equal = xml_xpath_equal_values(self);
                    } else {
                        equal = xml_xpath_not_equal_values(self);
                    }
                    self.value_push(xml_xpath_cache_new_boolean(self.context, equal != 0));
                }
                XmlXPathOp::XPathOpCmp => {
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch1 as usize]);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch2 as usize]);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    ret = xml_xpath_compare_values(self, (*op).value, (*op).value2);
                    self.value_push(xml_xpath_cache_new_boolean(self.context, ret != 0));
                }
                XmlXPathOp::XPathOpPlus => {
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch1 as usize]);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if (*op).ch2 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch2 as usize],
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if (*op).value == 0 {
                        xml_xpath_sub_values(self);
                    } else if (*op).value == 1 {
                        xml_xpath_add_values(self);
                    } else if (*op).value == 2 {
                        xml_xpath_value_flip_sign(self);
                    } else if (*op).value == 3 {
                        if !self.value.is_null()
                            && (*self.value).typ != XmlXPathObjectType::XPathNumber
                        {
                            xml_xpath_number_function(self, 1);
                        };
                        if self.value.is_null()
                            || (*self.value).typ != (XmlXPathObjectType::XPathNumber)
                        {
                            {
                                xml_xpath_err(self, XmlXPathError::XPathInvalidType as i32);
                                return 0;
                            }
                        };
                    }
                }
                XmlXPathOp::XPathOpMult => {
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch1 as usize]);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch2 as usize]);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if (*op).value == 0 {
                        xml_xpath_mult_values(self);
                    } else if (*op).value == 1 {
                        xml_xpath_div_values(self);
                    } else if (*op).value == 2 {
                        xml_xpath_mod_values(self);
                    }
                }
                XmlXPathOp::XPathOpUnion => 'to_break: {
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch1 as usize]);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch2 as usize]);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };

                    arg2 = self.value_pop();
                    arg1 = self.value_pop();
                    if arg1.is_null()
                        || (*arg1).typ != XmlXPathObjectType::XPathNodeset
                        || arg2.is_null()
                        || (*arg2).typ != XmlXPathObjectType::XPathNodeset
                    {
                        xml_xpath_release_object(self.context, arg1);
                        xml_xpath_release_object(self.context, arg2);
                        xml_xpath_err(self, XmlXPathError::XPathInvalidType as i32);
                        return 0;
                    }
                    if (*self.context).op_limit != 0
                        && ((*arg1)
                            .nodesetval
                            .as_deref()
                            .is_some_and(|n| self.check_operation_limit(n.len() as _) < 0)
                            || (*arg2)
                                .nodesetval
                                .as_deref()
                                .is_some_and(|n| self.check_operation_limit(n.len() as _) < 0))
                    {
                        xml_xpath_release_object(self.context, arg1);
                        xml_xpath_release_object(self.context, arg2);
                        break 'to_break;
                    }
                    if (*arg1).nodesetval.is_none()
                        || (*arg2)
                            .nodesetval
                            .as_deref()
                            .is_some_and(|n| !n.node_tab.is_empty())
                    {
                        // TODO: Check memory error.
                        (*arg1).nodesetval = xml_xpath_node_set_merge(
                            (*arg1).nodesetval.take(),
                            (*arg2).nodesetval.as_deref(),
                        );
                    }

                    self.value_push(arg1);
                    xml_xpath_release_object(self.context, arg2);
                }
                XmlXPathOp::XPathOpRoot => {
                    xml_xpath_root(self);
                }
                XmlXPathOp::XPathOpNode => {
                    if (*op).ch1 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if (*op).ch2 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch2 as usize],
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    self.value_push(xml_xpath_cache_new_node_set(
                        self.context,
                        (*self.context).node,
                    ));
                }
                XmlXPathOp::XPathOpCollect => 'to_break: {
                    if (*op).ch1 == -1 {
                        break 'to_break;
                    }
                    total += self
                        .evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch1 as usize]);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };

                    total += xml_xpath_node_collect_and_test(self, op, None, None, 0);
                }
                XmlXPathOp::XPathOpValue => {
                    self.value_push(xml_xpath_cache_object_copy(
                        self.context,
                        (*op).value4 as XmlXPathObjectPtr,
                    ));
                }
                XmlXPathOp::XPathOpVariable => 'to_break: {
                    let val: XmlXPathObjectPtr;
                    if (*op).ch1 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                        );
                    }
                    if (*op).value5.is_null() {
                        val = xml_xpath_variable_lookup(self.context, (*op).value4 as _);
                        if val.is_null() {
                            xml_xpath_err(self, XmlXPathError::XPathUndefVariableError as i32);
                            return 0;
                        }
                        self.value_push(val);
                    } else {
                        let uri: *const u8 = xml_xpath_ns_lookup(self.context, (*op).value5 as _);
                        if uri.is_null() {
                            generic_error!(
                                "xmlXPathCompOpEval: variable {} bound to undefined prefix {}\n",
                                CStr::from_ptr((*op).value4 as *const i8).to_string_lossy(),
                                CStr::from_ptr((*op).value5 as *const i8).to_string_lossy()
                            );
                            self.error = XmlXPathError::XPathUndefPrefixError as _;
                            break 'to_break;
                        }
                        val = xml_xpath_variable_lookup_ns(self.context, (*op).value4 as _, uri);
                        if val.is_null() {
                            xml_xpath_err(self, XmlXPathError::XPathUndefVariableError as i32);
                            return 0;
                        }
                        self.value_push(val);
                    }
                }
                XmlXPathOp::XPathOpFunction => 'to_break: {
                    let func: XmlXPathFunction;
                    let frame = self.value_tab.len();
                    if (*op).ch1 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                        );
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            break 'to_break;
                        }
                    }
                    if self.value_tab.len() < frame + (*op).value as usize {
                        generic_error!("xmlXPathCompOpEval: parameter error\n");
                        self.error = XmlXPathError::XPathInvalidOperand as i32;
                        break 'to_break;
                    }
                    for i in 0..(*op).value {
                        if self.value_tab[(self.value_tab.len() - 1) - i as usize].is_null() {
                            generic_error!("xmlXPathCompOpEval: parameter error\n");
                            self.error = XmlXPathError::XPathInvalidOperand as i32;
                            break;
                        }
                    }
                    if let Some(cache) = (*op).cache {
                        func = cache;
                    } else {
                        let mut uri: *const u8 = null();

                        let f = if (*op).value5.is_null() {
                            (*self.context).lookup_function(
                                CStr::from_ptr((*op).value4 as _).to_string_lossy().as_ref(),
                            )
                        } else {
                            uri = xml_xpath_ns_lookup(self.context, (*op).value5 as _);
                            if uri.is_null() {
                                generic_error!(
                                    "xmlXPathCompOpEval: function {} bound to undefined prefix {}\n",
                                    CStr::from_ptr((*op).value4 as *const i8).to_string_lossy(),
                                    CStr::from_ptr((*op).value5 as *const i8).to_string_lossy()
                                );
                                self.error = XmlXPathError::XPathUndefPrefixError as i32;
                                break 'to_break;
                            }
                            (*self.context).lookup_function_ns(
                                CStr::from_ptr((*op).value4 as _).to_string_lossy().as_ref(),
                                (!uri.is_null())
                                    .then(|| CStr::from_ptr(uri as *const i8).to_string_lossy())
                                    .as_deref(),
                            )
                        };
                        if let Some(f) = f {
                            func = f;
                        } else {
                            generic_error!(
                                "xmlXPathCompOpEval: function {} not found\n",
                                CStr::from_ptr((*op).value4 as *mut i8).to_string_lossy()
                            );
                            xml_xpath_err(self, XmlXPathError::XPathUnknownFuncError as i32);
                            return 0;
                        }

                        (*op).cache = Some(func);
                        (*op).cache_uri = uri as *mut c_void;
                    }

                    let old_func: *const u8 = (*self.context).function;
                    let old_func_uri: *const u8 = (*self.context).function_uri;
                    (*self.context).function = (*op).value4 as _;
                    (*self.context).function_uri = (*op).cache_uri as _;
                    func(self, (*op).value);
                    (*self.context).function = old_func;
                    (*self.context).function_uri = old_func_uri;
                    if self.error == XmlXPathError::XPathExpressionOK as i32
                        && self.value_tab.len() != frame + 1
                    {
                        xml_xpath_err(self, XmlXPathError::XPathStackError as i32);
                        return 0;
                    }
                }
                XmlXPathOp::XPathOpArg => {
                    if (*op).ch1 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                        );
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return 0;
                        };
                    }
                    if (*op).ch2 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch2 as usize],
                        );
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return 0;
                        };
                    }
                }
                XmlXPathOp::XPathOpPredicate | XmlXPathOp::XPathOpFilter => 'to_break: {
                    // Optimization for ()[1] selection i.e. the first elem
                    if (*op).ch1 != -1
                && (*op).ch2 != -1
                // FILTER TODO: Can we assume that the inner processing
                // will result in an ordered list if we have an
                // XPATH_OP_FILTER?
                // What about an additional field or flag on
                // xmlXPathObject like @sorted ? This way we wouldn't need
                // to assume anything, so it would be more robust and
                // easier to optimize.
                && (matches!(
                    (*comp).steps[(*op).ch1 as usize].op,
                    XmlXPathOp::XPathOpSort // 18
                ) || matches!(
                    (*comp).steps[(*op).ch1 as usize].op,
                    XmlXPathOp::XPathOpFilter // 17
                ))
                && matches!(
                    (*comp).steps[(*op).ch2 as usize].op,
                    XmlXPathOp::XPathOpValue // 12
                ) {
                        let val: XmlXPathObjectPtr = (*comp).steps[(*op).ch2 as usize].value4 as _;
                        if !val.is_null()
                            && (*val).typ == XmlXPathObjectType::XPathNumber
                            && (*val).floatval == 1.0
                        {
                            let mut first = None;

                            total += self.evaluate_precompiled_operation_first(
                                &raw mut (*comp).steps[(*op).ch1 as usize],
                                &mut first,
                            );
                            if self.error != XmlXPathError::XPathExpressionOK as i32 {
                                return 0;
                            };
                            // The nodeset should be in document order, Keep only the first value
                            if !self.value.is_null()
                                && (*self.value).typ == XmlXPathObjectType::XPathNodeset
                            {
                                if let Some(nodeset) = (*self.value)
                                    .nodesetval
                                    .as_deref_mut()
                                    .filter(|n| n.len() > 1)
                                {
                                    nodeset.truncate(1, true);
                                }
                            }
                            break 'to_break;
                        }
                    }
                    // Optimization for ()[last()] selection i.e. the last elem
                    if (*op).ch1 != -1
                        && (*op).ch2 != -1
                        && matches!(
                            (*comp).steps[(*op).ch1 as usize].op,
                            XmlXPathOp::XPathOpSort
                        )
                        && matches!(
                            (*comp).steps[(*op).ch2 as usize].op,
                            XmlXPathOp::XPathOpSort
                        )
                    {
                        let f: i32 = (*comp).steps[(*op).ch2 as usize].ch1;

                        if f != -1
                            && matches!((*comp).steps[f as usize].op, XmlXPathOp::XPathOpFunction)
                            && (*comp).steps[f as usize].value5.is_null()
                            && (*comp).steps[f as usize].value == 0
                            && !(*comp).steps[f as usize].value4.is_null()
                            && xml_str_equal(
                                (*comp).steps[f as usize].value4 as _,
                                c"last".as_ptr() as _,
                            )
                        {
                            let mut last = None;

                            total += self.evaluate_precompiled_operation_last(
                                &raw mut (*comp).steps[(*op).ch1 as usize],
                                &mut last,
                            );
                            if self.error != XmlXPathError::XPathExpressionOK as i32 {
                                return 0;
                            };
                            // The nodeset should be in document order, Keep only the last value
                            if !self.value.is_null()
                                && (*self.value).typ == XmlXPathObjectType::XPathNodeset
                            {
                                if let Some(nodeset) = (*self.value)
                                    .nodesetval
                                    .as_deref_mut()
                                    .filter(|n| n.len() > 1)
                                {
                                    xml_xpath_node_set_keep_last(Some(nodeset));
                                }
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
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if (*op).ch2 == -1 {
                        break 'to_break;
                    }

                    if self.value.is_null() {
                        break 'to_break;
                    }

                    // Hum are we filtering the result of an XPointer expression
                    #[cfg(feature = "libxml_xptr_locs")]
                    if (*self.value).typ == XmlXPathObjectType::XPathLocationset {
                        let locset: XmlLocationSetPtr = (*self.value).user as _;
                        xml_xpath_location_set_filter(
                            self,
                            locset,
                            (*op).ch2,
                            1,
                            (*locset).loc_tab.len() as i32,
                        );
                        break 'to_break;
                    }

                    // In xmlXPathOp::of errors, xmlXPathNodeSetFilter can pop additional
                    // nodes from the stack. We have to temporarily remove the
                    // nodeset object from the stack to avoid freeing it prematurely.
                    if self.value.is_null()
                        || (*self.value).typ != (XmlXPathObjectType::XPathNodeset)
                    {
                        {
                            xml_xpath_err(self, XmlXPathError::XPathInvalidType as i32);
                            return 0;
                        }
                    };
                    let obj: XmlXPathObjectPtr = self.value_pop();
                    if let Some(set) = (*obj).nodesetval.as_deref_mut() {
                        let max_pos = set.len() as i32;
                        xml_xpath_node_set_filter(self, Some(set), (*op).ch2, 1, max_pos, true);
                    }
                    self.value_push(obj);
                }
                XmlXPathOp::XPathOpSort => {
                    if (*op).ch1 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if !self.value.is_null()
                        && (*self.value).typ == XmlXPathObjectType::XPathNodeset
                    {
                        if let Some(nodeset) = (*self.value)
                            .nodesetval
                            .as_deref_mut()
                            .filter(|n| n.len() > 1)
                        {
                            nodeset.sort();
                        }
                    }
                }
                #[cfg(feature = "libxml_xptr_locs")]
                XmlXPathOp::XPathOpRangeto => 'to_break: {
                    let mut range: XmlXPathObjectPtr;
                    let mut res: XmlXPathObjectPtr;
                    let obj: XmlXPathObjectPtr;
                    let mut tmp: XmlXPathObjectPtr;
                    let newlocset: XmlLocationSetPtr;
                    let oldnode = (*self.context).node;
                    let oldcs: i32 = (*self.context).context_size;
                    let oldpp: i32 = (*self.context).proximity_position;

                    if (*op).ch1 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                        );
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return 0;
                        };
                    }
                    if self.value.is_null() {
                        xml_xpath_err(self, XmlXPathError::XPathInvalidOperand as i32);
                        return 0;
                    }
                    if (*op).ch2 == -1 {
                        break 'to_break;
                    }

                    'rangeto_error: {
                        if (*self.value).typ == XmlXPathObjectType::XPathLocationset {
                            // Extract the old locset, and then evaluate the result of the
                            // expression for all the element in the locset. use it to grow
                            // up a new locset.
                            if self.value.is_null()
                                || (*self.value).typ != (XmlXPathObjectType::XPathLocationset)
                            {
                                {
                                    xml_xpath_err(self, XmlXPathError::XPathInvalidType as i32);
                                    return 0;
                                }
                            };

                            if (*self.value).user.is_null()
                                || (*((*self.value).user as XmlLocationSetPtr))
                                    .loc_tab
                                    .is_empty()
                            {
                                break 'to_break;
                            }
                            obj = self.value_pop();
                            let oldlocset = (*obj).user as XmlLocationSetPtr;

                            newlocset = xml_xptr_location_set_create(null_mut());

                            for (i, &iloc) in (*oldlocset).loc_tab.iter().enumerate() {
                                // Run the evaluation with a node list made of a
                                // single item in the nodelocset.
                                (*self.context).node =
                                    XmlGenericNodePtr::from_raw((*iloc).user as *mut XmlNode);
                                (*self.context).context_size = (*oldlocset).loc_tab.len() as i32;
                                (*self.context).proximity_position = i as i32 + 1;
                                tmp = xml_xpath_cache_new_node_set(
                                    self.context,
                                    (*self.context).node,
                                );
                                self.value_push(tmp);

                                if (*op).ch2 != -1 {
                                    total += self.evaluate_precompiled_operation(
                                        &raw mut (*comp).steps[(*op).ch2 as usize],
                                    );
                                }
                                if self.error != XmlXPathError::XPathExpressionOK as i32 {
                                    xml_xptr_free_location_set(newlocset);
                                    break 'rangeto_error;
                                }

                                res = self.value_pop();
                                if (*res).typ == XmlXPathObjectType::XPathLocationset {
                                    let rloc: XmlLocationSetPtr = (*res).user as XmlLocationSetPtr;
                                    for &jloc in &(*rloc).loc_tab {
                                        range = xml_xptr_new_range(
                                            XmlGenericNodePtr::from_raw(
                                                (*iloc).user as *mut XmlNode,
                                            )
                                            .unwrap(),
                                            (*iloc).index,
                                            XmlGenericNodePtr::from_raw(
                                                (*jloc).user2 as *mut XmlNode,
                                            )
                                            .unwrap(),
                                            (*jloc).index2,
                                        );
                                        if !range.is_null() {
                                            xml_xptr_location_set_add(newlocset, range);
                                        }
                                    }
                                } else {
                                    range = xml_xptr_new_range_node_object(
                                        XmlGenericNodePtr::from_raw((*iloc).user as *mut XmlNode)
                                            .unwrap(),
                                        res,
                                    );
                                    if !range.is_null() {
                                        xml_xptr_location_set_add(newlocset, range);
                                    }
                                }

                                // Cleanup
                                if !res.is_null() {
                                    xml_xpath_release_object(self.context, res);
                                }
                                if self.value == tmp {
                                    res = self.value_pop();
                                    xml_xpath_release_object(self.context, res);
                                }
                            }
                        } else {
                            // Not a location set
                            if self.value.is_null()
                                || (*self.value).typ != (XmlXPathObjectType::XPathNodeset)
                            {
                                xml_xpath_err(self, XmlXPathError::XPathInvalidType as i32);
                                return 0;
                            };
                            obj = self.value_pop();
                            newlocset = xml_xptr_location_set_create(null_mut());

                            if let Some(oldset) = (*obj).nodesetval.as_deref() {
                                for &node in &oldset.node_tab {
                                    // Run the evaluation with a node list made of a single item
                                    // in the nodeset.
                                    (*self.context).node = Some(node);
                                    // OPTIMIZE TODO: Avoid recreation for every iteration.
                                    tmp = xml_xpath_cache_new_node_set(
                                        self.context,
                                        (*self.context).node,
                                    );
                                    self.value_push(tmp);

                                    if (*op).ch2 != -1 {
                                        total += self.evaluate_precompiled_operation(
                                            &raw mut (*comp).steps[(*op).ch2 as usize],
                                        );
                                    }
                                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                                        xml_xptr_free_location_set(newlocset);
                                        break 'rangeto_error;
                                    }

                                    res = self.value_pop();
                                    range = xml_xptr_new_range_node_object(node, res);
                                    if !range.is_null() {
                                        xml_xptr_location_set_add(newlocset, range);
                                    }

                                    // Cleanup
                                    if !res.is_null() {
                                        xml_xpath_release_object(self.context, res);
                                    }
                                    if self.value == tmp {
                                        res = self.value_pop();
                                        xml_xpath_release_object(self.context, res);
                                    }
                                }
                            }
                        }

                        // The result is used as the new evaluation set.
                        self.value_push(xml_xptr_wrap_location_set(newlocset));
                    }
                    // rangeto_error:
                    xml_xpath_release_object(self.context, obj);
                    (*self.context).node = oldnode;
                    (*self.context).context_size = oldcs;
                    (*self.context).proximity_position = oldpp;
                }
            }

            (*self.context).depth -= 1;
            total
        }
    }

    /// Evaluate the Precompiled XPath operation searching only the first element in document order
    ///
    /// Returns the number of examined objects.
    #[doc(alias = "xmlXPathCompOpEvalFirst")]
    unsafe fn evaluate_precompiled_operation_first(
        &mut self,
        op: XmlXPathStepOpPtr,
        first: &mut Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            let mut total: i32 = 0;
            let cur: i32;

            let arg1: XmlXPathObjectPtr;
            let arg2: XmlXPathObjectPtr;

            if self.error != XmlXPathError::XPathExpressionOK as i32 {
                return 0;
            };
            if (*self.context).op_limit != 0 && self.check_operation_limit(1) < 0 {
                return 0;
            }
            if (*self.context).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
                xml_xpath_err(self, XmlXPathError::XPathRecursionLimitExceeded as i32);
                return 0;
            }
            (*self.context).depth += 1;
            let comp: XmlXPathCompExprPtr = self.comp;
            match (*op).op {
                XmlXPathOp::XPathOpEnd => {}
                XmlXPathOp::XPathOpUnion => {
                    total = self.evaluate_precompiled_operation_first(
                        &raw mut (*comp).steps[(*op).ch1 as usize],
                        first,
                    );
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if !self.value.is_null()
                        && (*self.value).typ == XmlXPathObjectType::XPathNodeset
                    {
                        if let Some(nodeset) = (*self.value)
                            .nodesetval
                            .as_deref_mut()
                            .filter(|n| !n.is_empty())
                        {
                            // limit tree traversing to first node in the result
                            // OPTIMIZE TODO: This implicitly sorts
                            //  the result, even if not needed. E.g. if the argument
                            //  of the count() function, no sorting is needed.
                            // OPTIMIZE TODO: How do we know if the node-list wasn't
                            //  already sorted?
                            if nodeset.node_tab.len() > 1 {
                                nodeset.sort();
                            }
                            *first = Some(nodeset.node_tab[0]);
                        }
                    }
                    cur = self.evaluate_precompiled_operation_first(
                        &raw mut (*comp).steps[(*op).ch2 as usize],
                        first,
                    );
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };

                    arg2 = self.value_pop();
                    arg1 = self.value_pop();
                    if arg1.is_null()
                        || (*arg1).typ != XmlXPathObjectType::XPathNodeset
                        || arg2.is_null()
                        || (*arg2).typ != XmlXPathObjectType::XPathNodeset
                    {
                        xml_xpath_release_object(self.context, arg1);
                        xml_xpath_release_object(self.context, arg2);
                        xml_xpath_err(self, XmlXPathError::XPathInvalidType as i32);
                        return 0;
                    }
                    if (*self.context).op_limit != 0
                        && ((*arg1)
                            .nodesetval
                            .as_deref()
                            .is_some_and(|n| self.check_operation_limit(n.len() as _) < 0)
                            || (*arg2)
                                .nodesetval
                                .as_deref()
                                .is_some_and(|n| self.check_operation_limit(n.len() as _) < 0))
                    {
                        xml_xpath_release_object(self.context, arg1);
                        xml_xpath_release_object(self.context, arg2);
                    } else {
                        // TODO: Check memory error.
                        (*arg1).nodesetval = xml_xpath_node_set_merge(
                            (*arg1).nodesetval.take(),
                            (*arg2).nodesetval.as_deref(),
                        );
                        self.value_push(arg1);
                        xml_xpath_release_object(self.context, arg2);
                        // optimizer
                        if total > cur {
                            xml_xpath_comp_swap(op);
                        }
                        total += cur;
                    }
                }
                XmlXPathOp::XPathOpRoot => {
                    xml_xpath_root(self);
                }
                XmlXPathOp::XPathOpNode => {
                    if (*op).ch1 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if (*op).ch2 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch2 as usize],
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    self.value_push(xml_xpath_cache_new_node_set(
                        self.context,
                        (*self.context).node,
                    ));
                }
                XmlXPathOp::XPathOpCollect => {
                    if (*op).ch1 == -1 {
                        // break;
                    } else {
                        total = self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                        );
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return 0;
                        };
                        total += xml_xpath_node_collect_and_test(self, op, Some(first), None, 0);
                    }
                }
                XmlXPathOp::XPathOpValue => {
                    self.value_push(xml_xpath_cache_object_copy(
                        self.context,
                        (*op).value4 as XmlXPathObjectPtr,
                    ));
                }
                XmlXPathOp::XPathOpSort => {
                    if (*op).ch1 != -1 {
                        total += self.evaluate_precompiled_operation_first(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                            first,
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if !self.value.is_null()
                        && (*self.value).typ == XmlXPathObjectType::XPathNodeset
                    {
                        if let Some(nodeset) = (*self.value)
                            .nodesetval
                            .as_deref_mut()
                            .filter(|n| n.len() > 1)
                        {
                            nodeset.sort();
                        }
                    }
                }
                XmlXPathOp::XPathOpFilter => {
                    total += self.evaluate_precompiled_operation_filter_first(op, first);
                }
                _ => {
                    total += self.evaluate_precompiled_operation(op);
                }
            }

            (*self.context).depth -= 1;
            total
        }
    }

    /// Evaluate the Precompiled XPath operation searching only the last
    /// element in document order
    ///
    /// Returns the number of nodes traversed
    #[doc(alias = "xmlXPathCompOpEvalLast")]
    unsafe fn evaluate_precompiled_operation_last(
        &mut self,
        op: XmlXPathStepOpPtr,
        last: &mut Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            let mut total: i32 = 0;
            let cur: i32;
            let arg1: XmlXPathObjectPtr;
            let arg2: XmlXPathObjectPtr;

            if self.error != XmlXPathError::XPathExpressionOK as i32 {
                return 0;
            };
            if (*self.context).op_limit != 0 && self.check_operation_limit(1) < 0 {
                return 0;
            }
            if (*self.context).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
                xml_xpath_err(self, XmlXPathError::XPathRecursionLimitExceeded as i32);
                return 0;
            }
            (*self.context).depth += 1;
            let comp: XmlXPathCompExprPtr = self.comp;
            match (*op).op {
                XmlXPathOp::XPathOpEnd => {}
                XmlXPathOp::XPathOpUnion => {
                    total = self.evaluate_precompiled_operation_last(
                        &raw mut (*comp).steps[(*op).ch1 as usize],
                        last,
                    );
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if !self.value.is_null()
                        && (*self.value).typ == XmlXPathObjectType::XPathNodeset
                    {
                        if let Some(nodeset) = (*self.value).nodesetval.as_deref_mut() {
                            if !nodeset.is_empty() {
                                // limit tree traversing to first node in the result
                                if nodeset.node_tab.len() > 1 {
                                    nodeset.sort();
                                }
                                *last = Some(nodeset.node_tab.last().copied().unwrap());
                            }
                        }
                    }
                    cur = self.evaluate_precompiled_operation_last(
                        &raw mut (*comp).steps[(*op).ch2 as usize],
                        last,
                    );
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if !self.value.is_null()
                        && matches!((*self.value).typ, XmlXPathObjectType::XPathNodeset)
                    {
                        if let Some(_nodeset) = (*self.value)
                            .nodesetval
                            .as_deref()
                            .filter(|n| !n.is_empty())
                        { /* TODO: NOP ? */ }
                    }

                    arg2 = self.value_pop();
                    arg1 = self.value_pop();
                    if arg1.is_null()
                        || (*arg1).typ != XmlXPathObjectType::XPathNodeset
                        || arg2.is_null()
                        || (*arg2).typ != XmlXPathObjectType::XPathNodeset
                    {
                        xml_xpath_release_object(self.context, arg1);
                        xml_xpath_release_object(self.context, arg2);
                        xml_xpath_err(self, XmlXPathError::XPathInvalidType as i32);
                        return 0;
                    }
                    if (*self.context).op_limit != 0
                        && ((*arg1)
                            .nodesetval
                            .as_deref()
                            .is_some_and(|n| self.check_operation_limit(n.len() as _) < 0)
                            || (*arg2)
                                .nodesetval
                                .as_deref()
                                .is_some_and(|n| self.check_operation_limit(n.len() as _) < 0))
                    {
                        xml_xpath_release_object(self.context, arg1);
                        xml_xpath_release_object(self.context, arg2);
                        // break;
                    } else {
                        // TODO: Check memory error.
                        (*arg1).nodesetval = xml_xpath_node_set_merge(
                            (*arg1).nodesetval.take(),
                            (*arg2).nodesetval.as_deref(),
                        );
                        self.value_push(arg1);
                        xml_xpath_release_object(self.context, arg2);
                        // optimizer
                        if total > cur {
                            xml_xpath_comp_swap(op);
                        }
                        total += cur;
                    }
                }
                XmlXPathOp::XPathOpRoot => {
                    xml_xpath_root(self);
                }
                XmlXPathOp::XPathOpNode => {
                    if (*op).ch1 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if (*op).ch2 != -1 {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch2 as usize],
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    self.value_push(xml_xpath_cache_new_node_set(
                        self.context,
                        (*self.context).node,
                    ));
                }
                XmlXPathOp::XPathOpCollect => {
                    if (*op).ch1 == -1 {
                        // break;
                    } else {
                        total += self.evaluate_precompiled_operation(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                        );
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return 0;
                        };
                        total += xml_xpath_node_collect_and_test(self, op, None, Some(last), 0);
                    }
                }
                XmlXPathOp::XPathOpValue => {
                    self.value_push(xml_xpath_cache_object_copy(
                        self.context,
                        (*op).value4 as XmlXPathObjectPtr,
                    ));
                }
                XmlXPathOp::XPathOpSort => {
                    if (*op).ch1 != -1 {
                        total += self.evaluate_precompiled_operation_last(
                            &raw mut (*comp).steps[(*op).ch1 as usize],
                            last,
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if !self.value.is_null()
                        && (*self.value).typ == XmlXPathObjectType::XPathNodeset
                    {
                        if let Some(nodeset) = (*self.value)
                            .nodesetval
                            .as_deref_mut()
                            .filter(|n| n.len() > 1)
                        {
                            nodeset.sort();
                        }
                    }
                }
                _ => {
                    total += self.evaluate_precompiled_operation(op);
                }
            }

            (*self.context).depth -= 1;
            total
        }
    }

    #[doc(alias = "xmlXPathCompOpEvalFilterFirst")]
    unsafe fn evaluate_precompiled_operation_filter_first(
        &mut self,
        op: XmlXPathStepOpPtr,
        first: &mut Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            let mut total: i32 = 0;

            if self.error != XmlXPathError::XPathExpressionOK as i32 {
                return 0;
            };
            let comp: XmlXPathCompExprPtr = self.comp;
            // Optimization for ()[last()] selection i.e. the last elem
            if (*op).ch1 != -1
                && (*op).ch2 != -1
                && matches!(
                    (*comp).steps[(*op).ch1 as usize].op,
                    XmlXPathOp::XPathOpSort
                )
                && matches!(
                    (*comp).steps[(*op).ch2 as usize].op,
                    XmlXPathOp::XPathOpSort
                )
            {
                let f: i32 = (*comp).steps[(*op).ch2 as usize].ch1;

                if f != -1
                    && matches!((*comp).steps[f as usize].op, XmlXPathOp::XPathOpFunction)
                    && (*comp).steps[f as usize].value5.is_null()
                    && (*comp).steps[f as usize].value == 0
                    && !(*comp).steps[f as usize].value4.is_null()
                    && xml_str_equal((*comp).steps[f as usize].value4 as _, c"last".as_ptr() as _)
                {
                    let mut last = None;

                    total += self.evaluate_precompiled_operation_last(
                        &raw mut (*comp).steps[(*op).ch1 as usize],
                        &mut last,
                    );
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    // The nodeset should be in document order,
                    // Keep only the last value
                    if !self.value.is_null()
                        && (*self.value).typ == XmlXPathObjectType::XPathNodeset
                    {
                        if let Some(nodeset) = (*self.value)
                            .nodesetval
                            .as_deref_mut()
                            .filter(|n| n.len() > 1)
                        {
                            xml_xpath_node_set_keep_last(Some(nodeset));
                            *first = Some(nodeset.node_tab[0]);
                        }
                    }
                    return total;
                }
            }

            if (*op).ch1 != -1 {
                total +=
                    self.evaluate_precompiled_operation(&raw mut (*comp).steps[(*op).ch1 as usize]);
            }
            if self.error != XmlXPathError::XPathExpressionOK as i32 {
                return 0;
            };
            if (*op).ch2 == -1 {
                return total;
            }
            if self.value.is_null() {
                return total;
            }

            #[cfg(feature = "libxml_xptr_locs")]
            {
                // Hum are we filtering the result of an XPointer expression
                if matches!((*self.value).typ, XmlXPathObjectType::XPathLocationset) {
                    let locset: XmlLocationSetPtr = (*self.value).user as _;

                    if !locset.is_null() {
                        xml_xpath_location_set_filter(self, locset, (*op).ch2, 1, 1);
                        if !(*locset).loc_tab.is_empty() {
                            *first = XmlGenericNodePtr::from_raw(
                                (*(*locset).loc_tab[0]).user as *mut XmlNode,
                            );
                        }
                    }

                    return total;
                }
            }

            // In case of errors, xmlXPathNodeSetFilter can pop additional nodes from the stack.
            // We have to temporarily remove the nodeset object from the
            // stack to avoid freeing it prematurely.
            if self.value.is_null() || (*self.value).typ != (XmlXPathObjectType::XPathNodeset) {
                xml_xpath_err(self, XmlXPathError::XPathInvalidType as i32);
                return 0;
            };
            let obj: XmlXPathObjectPtr = self.value_pop();
            if let Some(set) = (*obj).nodesetval.as_deref_mut() {
                xml_xpath_node_set_filter(self, Some(set), (*op).ch2, 1, 1, true);
                if !set.node_tab.is_empty() {
                    *first = Some(set.node_tab[0]);
                }
            }
            self.value_push(obj);

            total
        }
    }

    /// Evaluates if the expression evaluates to true.
    ///
    /// Returns 1 if true, 0 if false and -1 on API or internal errors.
    #[doc(alias = "xmlXPathCompOpEvalToBoolean")]
    pub(super) unsafe fn evaluate_precompiled_operation_to_boolean(
        &mut self,
        mut op: XmlXPathStepOpPtr,
        is_predicate: i32,
    ) -> i32 {
        unsafe {
            let res_obj: XmlXPathObjectPtr;

            // start:
            loop {
                if (*self.context).op_limit != 0 && self.check_operation_limit(1) < 0 {
                    return 0;
                }
                // comp = self.comp;
                match (*op).op {
                    XmlXPathOp::XPathOpEnd => {
                        return 0;
                    }
                    XmlXPathOp::XPathOpValue => {
                        res_obj = (*op).value4 as XmlXPathObjectPtr;
                        if is_predicate != 0 {
                            return xml_xpath_evaluate_predicate_result(self, res_obj);
                        }
                        return xml_xpath_cast_to_boolean(res_obj) as i32;
                    }
                    XmlXPathOp::XPathOpSort => {
                        // We don't need sorting for boolean results. Skip this one.
                        if (*op).ch1 != -1 {
                            op = &raw mut (*self.comp).steps[(*op).ch1 as usize];
                            // goto start;
                            continue;
                        }
                        return 0;
                    }
                    XmlXPathOp::XPathOpCollect => {
                        if (*op).ch1 == -1 {
                            return 0;
                        }

                        let step = &raw mut (*self.comp).steps[(*op).ch1 as usize];
                        self.evaluate_precompiled_operation(step);
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return -1;
                        }

                        xml_xpath_node_collect_and_test(self, op, None, None, 1);
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return -1;
                        }

                        res_obj = self.value_pop();
                        if res_obj.is_null() {
                            return -1;
                        }
                    }
                    _ => {
                        // Fallback to call xmlXPathCompOpEval().
                        self.evaluate_precompiled_operation(op);
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return -1;
                        }

                        res_obj = self.value_pop();
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
                    xml_xpath_evaluate_predicate_result(self, res_obj)
                } else {
                    xml_xpath_cast_to_boolean(res_obj) as i32
                };
                xml_xpath_release_object(self.context, res_obj);
                return res;
            }

            0
        }
    }
}

use std::{mem::replace, ptr::null_mut, rc::Rc};

#[cfg(feature = "libxml_xptr_locs")]
use crate::libxml::xpointer::XmlLocationSet;
#[cfg(feature = "libxml_pattern")]
use crate::pattern::XmlPattern;
use crate::{
    CHECK_ERROR, XP_ERROR, generic_error,
    libxml::xpointer::{
        xml_xptr_new_range, xml_xptr_new_range_node_object, xml_xptr_wrap_location_set,
    },
    tree::{XmlAttrPtr, XmlElementType, XmlGenericNodePtr, XmlNodePtr, XmlNsPtr},
    xpath::{
        XmlXPathError, XmlXPathOp, xml_xpath_location_set_filter, xml_xpath_node_set_filter,
        xml_xpath_node_set_keep_last,
    },
};

use super::{
    XPATH_MAX_RECURSION_DEPTH, XmlXPathAxisVal, XmlXPathContext, XmlXPathContextPtr,
    XmlXPathFunction, XmlXPathNodeSetMergeFunction, XmlXPathObject, XmlXPathObjectPtr,
    XmlXPathObjectType, XmlXPathParserContext, XmlXPathParserContextPtr, XmlXPathTestVal,
    XmlXPathTraversalFunction, XmlXPathTypeVal,
    compile::XmlXPathStepOpPtr,
    functions::{xml_xpath_boolean_function, xml_xpath_number_function},
    xml_xpath_add_values, xml_xpath_cache_new_boolean, xml_xpath_cache_new_node_set,
    xml_xpath_cache_object_copy, xml_xpath_cache_wrap_node_set, xml_xpath_cast_to_boolean,
    xml_xpath_cmp_nodes_ext, xml_xpath_comp_op_eval_predicate, xml_xpath_compare_values,
    xml_xpath_div_values, xml_xpath_equal_values, xml_xpath_err,
    xml_xpath_evaluate_predicate_result, xml_xpath_free_node_set, xml_xpath_free_object,
    xml_xpath_mod_values, xml_xpath_mult_values, xml_xpath_next_ancestor,
    xml_xpath_next_ancestor_or_self, xml_xpath_next_attribute, xml_xpath_next_child,
    xml_xpath_next_child_element, xml_xpath_next_descendant, xml_xpath_next_descendant_or_self,
    xml_xpath_next_following, xml_xpath_next_following_sibling, xml_xpath_next_namespace,
    xml_xpath_next_parent, xml_xpath_next_preceding_internal, xml_xpath_next_preceding_sibling,
    xml_xpath_next_self, xml_xpath_node_set_create, xml_xpath_node_set_merge,
    xml_xpath_node_set_merge_and_clear, xml_xpath_node_set_merge_and_clear_no_dupls,
    xml_xpath_not_equal_values, xml_xpath_release_object, xml_xpath_root, xml_xpath_sub_values,
    xml_xpath_value_flip_sign, xml_xpath_variable_lookup, xml_xpath_variable_lookup_ns,
};

type StepOpIndex = usize;

impl XmlXPathParserContext {
    /// Adds opCount to the running total of operations and returns -1 if the
    /// operation limit is exceeded. Returns 0 otherwise.
    #[doc(alias = "xmlXPathCheckOpLimit")]
    pub(super) unsafe fn check_operation_limit(&mut self, op_count: u64) -> i32 {
        unsafe {
            let xpctxt: XmlXPathContextPtr = self.context;

            if op_count > (*xpctxt).op_limit || (*xpctxt).op_count > (*xpctxt).op_limit - op_count {
                (*xpctxt).op_count = (*xpctxt).op_limit;
                xml_xpath_err(Some(self), XmlXPathError::XPathOpLimitExceeded as i32);
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
            let comp = (*self.context).try_stream_compile(&self.base);

            match () {
                #[cfg(feature = "libxml_pattern")]
                _ if comp.is_some() => {
                    self.comp = comp.unwrap();
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
                        XP_ERROR!(Some(self), XmlXPathError::XPathExprError as i32);
                    }

                    if self.comp.steps.len() > 1 && self.comp.last >= 0 {
                        if !self.context.is_null() {
                            old_depth = (*self.context).depth;
                        }
                        let last = self.comp.last as usize;
                        let op = &raw mut self.comp.steps[last];
                        self.optimize_expression(op);
                        if !self.context.is_null() {
                            (*self.context).depth = old_depth;
                        }
                    }
                }
            }

            self.run_evaluate(false);
        }
    }

    /// Evaluate the Precompiled XPath expression in the given context.
    #[doc(alias = "xmlXPathRunEval")]
    pub(crate) unsafe fn run_evaluate(&mut self, to_bool: bool) -> i32 {
        unsafe {
            #[cfg(feature = "libxml_pattern")]
            if self.comp.stream.is_some() {
                let res: i32;

                if to_bool {
                    // Evaluation to boolean result.
                    res = (*self.context).run_stream_eval(
                        self.comp.stream.as_deref().unwrap(),
                        null_mut(),
                        1,
                    );
                    if res != -1 {
                        return res;
                    }
                } else {
                    let mut res_obj: XmlXPathObjectPtr = null_mut();

                    // Evaluation to a sequence.
                    res = (*self.context).run_stream_eval(
                        self.comp.stream.as_deref().unwrap(),
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

            if self.comp.last < 0 {
                generic_error!("xmlXPathRunEval: last is less than zero\n");
                return -1;
            }
            let old_depth: i32 = (*self.context).depth;
            if to_bool {
                return self
                    .evaluate_precompiled_operation_to_boolean(self.comp.last as usize, false);
            } else {
                self.evaluate_precompiled_operation(self.comp.last as usize);
            }
            (*self.context).depth = old_depth;

            0
        }
    }

    /// Evaluate the Precompiled XPath operation
    /// Returns the number of nodes traversed
    #[doc(alias = "xmlXPathCompOpEval")]
    unsafe fn evaluate_precompiled_operation(&mut self, op: StepOpIndex) -> i32 {
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
                xml_xpath_err(
                    Some(self),
                    XmlXPathError::XPathRecursionLimitExceeded as i32,
                );
                return 0;
            }
            (*self.context).depth += 1;
            match self.comp.steps[op].op {
                XmlXPathOp::XPathOpEnd => {}
                XmlXPathOp::XPathOpAnd => 'to_break: {
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    xml_xpath_boolean_function(self, 1);
                    if self.value().is_none_or(|value| !(*value).boolval) {
                        break 'to_break;
                    }
                    arg2 = self.value_pop();
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch2 as usize);
                    if self.error != 0 {
                        xml_xpath_free_object(arg2);
                        break 'to_break;
                    }
                    xml_xpath_boolean_function(self, 1);
                    if let Some(value) = self.value_mut() {
                        (**value).boolval &= (*arg2).boolval;
                    }
                    xml_xpath_release_object(self.context, arg2);
                }
                XmlXPathOp::XPathOpOr => 'to_break: {
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    xml_xpath_boolean_function(self, 1);
                    if self.value().is_none_or(|value| (*value).boolval) {
                        break 'to_break;
                    }
                    arg2 = self.value_pop();
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch2 as usize);
                    if self.error != 0 {
                        xml_xpath_free_object(arg2);
                        break 'to_break;
                    }
                    xml_xpath_boolean_function(self, 1);
                    if let Some(value) = self.value_mut() {
                        (**value).boolval |= (*arg2).boolval;
                    }
                    xml_xpath_release_object(self.context, arg2);
                }
                XmlXPathOp::XPathOpEqual => {
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch2 as usize);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self.comp.steps[op].value != 0 {
                        equal = xml_xpath_equal_values(self);
                    } else {
                        equal = xml_xpath_not_equal_values(self);
                    }
                    self.value_push(xml_xpath_cache_new_boolean(self.context, equal != 0));
                }
                XmlXPathOp::XPathOpCmp => {
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch2 as usize);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    ret = xml_xpath_compare_values(
                        self,
                        self.comp.steps[op].value,
                        self.comp.steps[op].value2,
                    );
                    self.value_push(xml_xpath_cache_new_boolean(self.context, ret != 0));
                }
                XmlXPathOp::XPathOpPlus => {
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self.comp.steps[op].ch2 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch2 as usize);
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self.comp.steps[op].value == 0 {
                        xml_xpath_sub_values(self);
                    } else if self.comp.steps[op].value == 1 {
                        xml_xpath_add_values(self);
                    } else if self.comp.steps[op].value == 2 {
                        xml_xpath_value_flip_sign(self);
                    } else if self.comp.steps[op].value == 3 {
                        if self
                            .value()
                            .is_some_and(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
                        {
                            xml_xpath_number_function(self, 1);
                        }
                        if self
                            .value()
                            .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNumber)
                        {
                            xml_xpath_err(Some(self), XmlXPathError::XPathInvalidType as i32);
                            return 0;
                        }
                    }
                }
                XmlXPathOp::XPathOpMult => {
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch2 as usize);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self.comp.steps[op].value == 0 {
                        xml_xpath_mult_values(self);
                    } else if self.comp.steps[op].value == 1 {
                        xml_xpath_div_values(self);
                    } else if self.comp.steps[op].value == 2 {
                        xml_xpath_mod_values(self);
                    }
                }
                XmlXPathOp::XPathOpUnion => 'to_break: {
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch2 as usize);
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
                        xml_xpath_err(Some(self), XmlXPathError::XPathInvalidType as i32);
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
                    if self.comp.steps[op].ch1 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self.comp.steps[op].ch2 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch2 as usize);
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
                    if self.comp.steps[op].ch1 == -1 {
                        break 'to_break;
                    }
                    total += self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };

                    total += xml_xpath_node_collect_and_test(self, op, None, None, 0);
                }
                XmlXPathOp::XPathOpValue => {
                    self.value_push(xml_xpath_cache_object_copy(
                        self.context,
                        self.comp.steps[op]
                            .value4
                            .as_ref()
                            .unwrap()
                            .as_object()
                            .unwrap(),
                    ));
                }
                XmlXPathOp::XPathOpVariable => 'to_break: {
                    let val: XmlXPathObjectPtr;
                    if self.comp.steps[op].ch1 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    }
                    if let Some(value5) = self.comp.steps[op]
                        .value5
                        .as_ref()
                        .and_then(|val| val.as_str())
                    {
                        let uri = (*self.context).lookup_ns(value5);
                        let value4 = self.comp.steps[op]
                            .value4
                            .as_ref()
                            .and_then(|val| val.as_str())
                            .unwrap();
                        let Some(uri) = uri else {
                            generic_error!(
                                "xmlXPathCompOpEval: variable {} bound to undefined prefix {}\n",
                                value4,
                                value5
                            );
                            self.error = XmlXPathError::XPathUndefPrefixError as _;
                            break 'to_break;
                        };
                        val = xml_xpath_variable_lookup_ns(self.context, value4, Some(&uri));
                        if val.is_null() {
                            xml_xpath_err(
                                Some(self),
                                XmlXPathError::XPathUndefVariableError as i32,
                            );
                            return 0;
                        }
                        self.value_push(val);
                    } else {
                        let value4 = self.comp.steps[op]
                            .value4
                            .as_ref()
                            .and_then(|val| val.as_str())
                            .unwrap();
                        val = xml_xpath_variable_lookup(self.context, value4);
                        if val.is_null() {
                            xml_xpath_err(
                                Some(self),
                                XmlXPathError::XPathUndefVariableError as i32,
                            );
                            return 0;
                        }
                        self.value_push(val);
                    }
                }
                XmlXPathOp::XPathOpFunction => 'to_break: {
                    let func: XmlXPathFunction;
                    let frame = self.value_tab.len();
                    if self.comp.steps[op].ch1 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            break 'to_break;
                        }
                    }
                    if self.value_tab.len() < frame + self.comp.steps[op].value as usize {
                        generic_error!("xmlXPathCompOpEval: parameter error\n");
                        self.error = XmlXPathError::XPathInvalidOperand as i32;
                        break 'to_break;
                    }
                    for i in 0..self.comp.steps[op].value {
                        if self.value_tab[(self.value_tab.len() - 1) - i as usize].is_null() {
                            generic_error!("xmlXPathCompOpEval: parameter error\n");
                            self.error = XmlXPathError::XPathInvalidOperand as i32;
                            break;
                        }
                    }
                    if let Some(cache) = self.comp.steps[op].cache {
                        func = cache;
                    } else {
                        let mut uri = None;

                        let value4 = self.comp.steps[op]
                            .value4
                            .as_ref()
                            .and_then(|val| val.as_str())
                            .unwrap();
                        let f = if let Some(value5) = self.comp.steps[op]
                            .value5
                            .as_ref()
                            .and_then(|val| val.as_str())
                        {
                            uri = (*self.context).lookup_ns(value5);
                            let Some(uri) = uri.as_deref() else {
                                generic_error!(
                                    "xmlXPathCompOpEval: function {} bound to undefined prefix {}\n",
                                    value4,
                                    value5
                                );
                                self.error = XmlXPathError::XPathUndefPrefixError as i32;
                                break 'to_break;
                            };
                            (*self.context).lookup_function_ns(value4, Some(uri))
                        } else {
                            (*self.context).lookup_function(value4)
                        };
                        if let Some(f) = f {
                            func = f;
                        } else {
                            generic_error!("xmlXPathCompOpEval: function {} not found\n", value4);
                            xml_xpath_err(Some(self), XmlXPathError::XPathUnknownFuncError as i32);
                            return 0;
                        }

                        self.comp.steps[op].cache = Some(func);
                        self.comp.steps[op].cache_uri = uri;
                    }

                    let old_func = (*self.context).function.take();
                    let old_func_uri = replace(
                        &mut (*self.context).function_uri,
                        self.comp.steps[op].cache_uri.clone(),
                    );
                    (*self.context).function = Some(
                        self.comp.steps[op]
                            .value4
                            .as_ref()
                            .unwrap()
                            .as_str()
                            .unwrap()
                            .into(),
                    );
                    func(self, self.comp.steps[op].value as usize);
                    (*self.context).function = old_func;
                    (*self.context).function_uri = old_func_uri;
                    if self.error == XmlXPathError::XPathExpressionOK as i32
                        && self.value_tab.len() != frame + 1
                    {
                        xml_xpath_err(Some(self), XmlXPathError::XPathStackError as i32);
                        return 0;
                    }
                }
                XmlXPathOp::XPathOpArg => {
                    if self.comp.steps[op].ch1 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return 0;
                        };
                    }
                    if self.comp.steps[op].ch2 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch2 as usize);
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return 0;
                        };
                    }
                }
                XmlXPathOp::XPathOpPredicate | XmlXPathOp::XPathOpFilter => 'to_break: {
                    // Optimization for ()[1] selection i.e. the first elem
                    if self.comp.steps[op].ch1 != -1
                        && self.comp.steps[op].ch2 != -1
                        // FILTER TODO: Can we assume that the inner processing
                        // will result in an ordered list if we have an
                        // XPATH_OP_FILTER?
                        // What about an additional field or flag on
                        // xmlXPathObject like @sorted ? This way we wouldn't need
                        // to assume anything, so it would be more robust and
                        // easier to optimize.
                        && (matches!(
                            self.comp.steps[self.comp.steps[op].ch1 as usize].op,
                            XmlXPathOp::XPathOpSort // 18
                        ) || matches!(
                            self.comp.steps[self.comp.steps[op].ch1 as usize].op,
                            XmlXPathOp::XPathOpFilter // 17
                        )) && matches!(
                            self.comp.steps[self.comp.steps[op].ch2 as usize].op,
                            XmlXPathOp::XPathOpValue // 12
                        ) && self.comp.steps[self.comp.steps[op].ch2 as usize]
                            .value4
                            .as_ref()
                            .and_then(|val| val.as_object())
                            .is_some_and(|obj| {
                                obj.typ == XmlXPathObjectType::XPathNumber && obj.floatval == 1.0
                            })
                    {
                        let mut first = None;

                        total += self.evaluate_precompiled_operation_first(
                            self.comp.steps[op].ch1 as usize,
                            &mut first,
                        );
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return 0;
                        };
                        // The nodeset should be in document order, Keep only the first value
                        if self
                            .value()
                            .is_some_and(|value| (*value).typ == XmlXPathObjectType::XPathNodeset)
                        {
                            if let Some(nodeset) = (**self.value_mut().unwrap())
                                .nodesetval
                                .as_deref_mut()
                                .filter(|n| n.len() > 1)
                            {
                                nodeset.truncate(1, true);
                            }
                        }
                        break 'to_break;
                    }
                    // Optimization for ()[last()] selection i.e. the last elem
                    if self.comp.steps[op].ch1 != -1
                        && self.comp.steps[op].ch2 != -1
                        && matches!(
                            self.comp.steps[self.comp.steps[op].ch1 as usize].op,
                            XmlXPathOp::XPathOpSort
                        )
                        && matches!(
                            self.comp.steps[self.comp.steps[op].ch2 as usize].op,
                            XmlXPathOp::XPathOpSort
                        )
                    {
                        let f: i32 = self.comp.steps[self.comp.steps[op].ch2 as usize].ch1;

                        if f != -1
                            && matches!(self.comp.steps[f as usize].op, XmlXPathOp::XPathOpFunction)
                            && self.comp.steps[f as usize].value5.is_none()
                            && self.comp.steps[f as usize].value == 0
                            && self.comp.steps[f as usize]
                                .value4
                                .as_ref()
                                .and_then(|val| val.as_str())
                                .is_some_and(|s| s == "last")
                        {
                            let mut last = None;

                            total += self.evaluate_precompiled_operation_last(
                                self.comp.steps[op].ch1 as usize,
                                &mut last,
                            );
                            if self.error != XmlXPathError::XPathExpressionOK as i32 {
                                return 0;
                            };
                            // The nodeset should be in document order, Keep only the last value
                            if self.value().is_some_and(|value| {
                                (*value).typ == XmlXPathObjectType::XPathNodeset
                            }) {
                                if let Some(nodeset) = (**self.value_mut().unwrap())
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
                    if self.comp.steps[op].ch1 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self.comp.steps[op].ch2 == -1 {
                        break 'to_break;
                    }

                    if self.value().is_none() {
                        break 'to_break;
                    }

                    // Hum are we filtering the result of an XPointer expression
                    #[cfg(feature = "libxml_xptr_locs")]
                    if (*self.value().unwrap()).typ == XmlXPathObjectType::XPathLocationset {
                        if let Some(locset) = (**self.value_mut().unwrap())
                            .user
                            .as_mut()
                            .and_then(|user| user.as_location_set_mut())
                        {
                            xml_xpath_location_set_filter(
                                self,
                                locset,
                                self.comp.steps[op].ch2,
                                1,
                                locset.loc_tab.len() as i32,
                            );
                        }

                        break 'to_break;
                    }

                    // In xmlXPathOp::of errors, xmlXPathNodeSetFilter can pop additional
                    // nodes from the stack. We have to temporarily remove the
                    // nodeset object from the stack to avoid freeing it prematurely.
                    if self
                        .value()
                        .is_none_or(|value| (*value).typ != (XmlXPathObjectType::XPathNodeset))
                    {
                        {
                            xml_xpath_err(Some(self), XmlXPathError::XPathInvalidType as i32);
                            return 0;
                        }
                    };
                    let obj: XmlXPathObjectPtr = self.value_pop();
                    if let Some(set) = (*obj).nodesetval.as_deref_mut() {
                        let max_pos = set.len() as i32;
                        xml_xpath_node_set_filter(
                            self,
                            Some(set),
                            self.comp.steps[op].ch2,
                            1,
                            max_pos,
                            true,
                        );
                    }
                    self.value_push(obj);
                }
                XmlXPathOp::XPathOpSort => {
                    if self.comp.steps[op].ch1 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self
                        .value()
                        .is_some_and(|value| (*value).typ == XmlXPathObjectType::XPathNodeset)
                    {
                        if let Some(nodeset) = (*self.value().unwrap())
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
                    let mut res: XmlXPathObjectPtr;
                    let obj: XmlXPathObjectPtr;
                    let mut tmp: XmlXPathObjectPtr;
                    let mut newlocset;
                    let oldnode = (*self.context).node;
                    let oldcs: i32 = (*self.context).context_size;
                    let oldpp: i32 = (*self.context).proximity_position;

                    if self.comp.steps[op].ch1 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return 0;
                        };
                    }
                    if self.value().is_none() {
                        xml_xpath_err(Some(self), XmlXPathError::XPathInvalidOperand as i32);
                        return 0;
                    }
                    if self.comp.steps[op].ch2 == -1 {
                        break 'to_break;
                    }

                    'rangeto_error: {
                        if (*self.value().unwrap()).typ == XmlXPathObjectType::XPathLocationset {
                            // Extract the old locset, and then evaluate the result of the
                            // expression for all the element in the locset. use it to grow
                            // up a new locset.
                            if self.value().is_none_or(|value| {
                                (*value).typ != XmlXPathObjectType::XPathLocationset
                            }) {
                                {
                                    xml_xpath_err(
                                        Some(self),
                                        XmlXPathError::XPathInvalidType as i32,
                                    );
                                    return 0;
                                }
                            };

                            if (*self.value().unwrap())
                                .user
                                .as_ref()
                                .and_then(|user| user.as_location_set())
                                .is_none_or(|loc| loc.loc_tab.is_empty())
                            {
                                break 'to_break;
                            }
                            obj = self.value_pop();
                            let oldlocset = (*obj)
                                .user
                                .as_ref()
                                .and_then(|user| user.as_location_set())
                                .unwrap();

                            newlocset = XmlLocationSet::new(None);

                            for (i, iloc) in oldlocset.loc_tab.iter().enumerate() {
                                // Run the evaluation with a node list made of a
                                // single item in the nodelocset.
                                (*self.context).node =
                                    iloc.user.as_ref().and_then(|user| user.as_node()).copied();
                                (*self.context).context_size = oldlocset.loc_tab.len() as i32;
                                (*self.context).proximity_position = i as i32 + 1;
                                tmp = xml_xpath_cache_new_node_set(
                                    self.context,
                                    (*self.context).node,
                                );
                                self.value_push(tmp);

                                if self.comp.steps[op].ch2 != -1 {
                                    total += self.evaluate_precompiled_operation(
                                        self.comp.steps[op].ch2 as usize,
                                    );
                                }
                                if self.error != XmlXPathError::XPathExpressionOK as i32 {
                                    break 'rangeto_error;
                                }

                                res = self.value_pop();
                                if (*res).typ == XmlXPathObjectType::XPathLocationset {
                                    let rloc = (*res)
                                        .user
                                        .as_ref()
                                        .and_then(|user| user.as_location_set())
                                        .unwrap();
                                    for jloc in &rloc.loc_tab {
                                        if let Some(range) = xml_xptr_new_range(
                                            iloc.user
                                                .as_ref()
                                                .and_then(|user| user.as_node())
                                                .copied()
                                                .unwrap(),
                                            iloc.index,
                                            jloc.user2
                                                .as_ref()
                                                .and_then(|user| user.as_node())
                                                .copied()
                                                .unwrap(),
                                            jloc.index2,
                                        ) {
                                            newlocset.push(Rc::new(range));
                                        }
                                    }
                                } else if let Some(range) = xml_xptr_new_range_node_object(
                                    iloc.user
                                        .as_ref()
                                        .and_then(|user| user.as_node())
                                        .copied()
                                        .unwrap(),
                                    res,
                                ) {
                                    newlocset.push(Rc::new(range));
                                }

                                // Cleanup
                                if !res.is_null() {
                                    xml_xpath_release_object(self.context, res);
                                }
                                if self.value().unwrap() == tmp {
                                    res = self.value_pop();
                                    xml_xpath_release_object(self.context, res);
                                }
                            }
                        } else {
                            // Not a location set
                            if self.value().is_none_or(|value| {
                                (*value).typ != XmlXPathObjectType::XPathNodeset
                            }) {
                                xml_xpath_err(Some(self), XmlXPathError::XPathInvalidType as i32);
                                return 0;
                            };
                            obj = self.value_pop();
                            newlocset = XmlLocationSet::new(None);

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

                                    if self.comp.steps[op].ch2 != -1 {
                                        total += self.evaluate_precompiled_operation(
                                            self.comp.steps[op].ch2 as usize,
                                        );
                                    }
                                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                                        break 'rangeto_error;
                                    }

                                    res = self.value_pop();
                                    if let Some(range) = xml_xptr_new_range_node_object(node, res) {
                                        newlocset.push(Rc::new(range));
                                    }

                                    // Cleanup
                                    if !res.is_null() {
                                        xml_xpath_release_object(self.context, res);
                                    }
                                    if self.value().unwrap() == tmp {
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
        op: StepOpIndex,
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
                xml_xpath_err(
                    Some(self),
                    XmlXPathError::XPathRecursionLimitExceeded as i32,
                );
                return 0;
            }
            (*self.context).depth += 1;
            match self.comp.steps[op].op {
                XmlXPathOp::XPathOpEnd => {}
                XmlXPathOp::XPathOpUnion => {
                    total = self.evaluate_precompiled_operation_first(
                        self.comp.steps[op].ch1 as usize,
                        first,
                    );
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self
                        .value()
                        .is_some_and(|value| (*value).typ == XmlXPathObjectType::XPathNodeset)
                    {
                        if let Some(nodeset) = (*self.value().unwrap())
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
                        self.comp.steps[op].ch2 as usize,
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
                        xml_xpath_err(Some(self), XmlXPathError::XPathInvalidType as i32);
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
                            self.comp.steps[op].swap_children();
                        }
                        total += cur;
                    }
                }
                XmlXPathOp::XPathOpRoot => {
                    xml_xpath_root(self);
                }
                XmlXPathOp::XPathOpNode => {
                    if self.comp.steps[op].ch1 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self.comp.steps[op].ch2 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch2 as usize);
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
                    if self.comp.steps[op].ch1 == -1 {
                        // break;
                    } else {
                        total =
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return 0;
                        };
                        total += xml_xpath_node_collect_and_test(self, op, Some(first), None, 0);
                    }
                }
                XmlXPathOp::XPathOpValue => {
                    self.value_push(xml_xpath_cache_object_copy(
                        self.context,
                        self.comp.steps[op]
                            .value4
                            .as_ref()
                            .and_then(|val| val.as_object())
                            .unwrap(),
                    ));
                }
                XmlXPathOp::XPathOpSort => {
                    if self.comp.steps[op].ch1 != -1 {
                        total += self.evaluate_precompiled_operation_first(
                            self.comp.steps[op].ch1 as usize,
                            first,
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self
                        .value()
                        .is_some_and(|value| (*value).typ == XmlXPathObjectType::XPathNodeset)
                    {
                        if let Some(nodeset) = (*self.value().unwrap())
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
        op: StepOpIndex,
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
                xml_xpath_err(
                    Some(self),
                    XmlXPathError::XPathRecursionLimitExceeded as i32,
                );
                return 0;
            }
            (*self.context).depth += 1;
            match self.comp.steps[op].op {
                XmlXPathOp::XPathOpEnd => {}
                XmlXPathOp::XPathOpUnion => {
                    total = self.evaluate_precompiled_operation_last(
                        self.comp.steps[op].ch1 as usize,
                        last,
                    );
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self
                        .value()
                        .is_some_and(|value| (*value).typ == XmlXPathObjectType::XPathNodeset)
                    {
                        if let Some(nodeset) = (*self.value().unwrap()).nodesetval.as_deref_mut() {
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
                        self.comp.steps[op].ch2 as usize,
                        last,
                    );
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self.value().is_some_and(|value| {
                        matches!((*value).typ, XmlXPathObjectType::XPathNodeset)
                    }) {
                        if let Some(_nodeset) = (*self.value().unwrap())
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
                        xml_xpath_err(Some(self), XmlXPathError::XPathInvalidType as i32);
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
                            self.comp.steps[op].swap_children();
                        }
                        total += cur;
                    }
                }
                XmlXPathOp::XPathOpRoot => {
                    xml_xpath_root(self);
                }
                XmlXPathOp::XPathOpNode => {
                    if self.comp.steps[op].ch1 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self.comp.steps[op].ch2 != -1 {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch2 as usize);
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
                    if self.comp.steps[op].ch1 == -1 {
                        // break;
                    } else {
                        total +=
                            self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
                        if self.error != XmlXPathError::XPathExpressionOK as i32 {
                            return 0;
                        };
                        total += xml_xpath_node_collect_and_test(self, op, None, Some(last), 0);
                    }
                }
                XmlXPathOp::XPathOpValue => {
                    self.value_push(xml_xpath_cache_object_copy(
                        self.context,
                        self.comp.steps[op]
                            .value4
                            .as_ref()
                            .and_then(|val| val.as_object())
                            .unwrap(),
                    ));
                }
                XmlXPathOp::XPathOpSort => {
                    if self.comp.steps[op].ch1 != -1 {
                        total += self.evaluate_precompiled_operation_last(
                            self.comp.steps[op].ch1 as usize,
                            last,
                        );
                    }
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    if self
                        .value()
                        .is_some_and(|value| (*value).typ == XmlXPathObjectType::XPathNodeset)
                    {
                        if let Some(nodeset) = (*self.value().unwrap())
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
        op: StepOpIndex,
        first: &mut Option<XmlGenericNodePtr>,
    ) -> i32 {
        unsafe {
            let mut total: i32 = 0;

            if self.error != XmlXPathError::XPathExpressionOK as i32 {
                return 0;
            };
            // Optimization for ()[last()] selection i.e. the last elem
            if self.comp.steps[op].ch1 != -1
                && self.comp.steps[op].ch2 != -1
                && matches!(
                    self.comp.steps[self.comp.steps[op].ch1 as usize].op,
                    XmlXPathOp::XPathOpSort
                )
                && matches!(
                    self.comp.steps[self.comp.steps[op].ch2 as usize].op,
                    XmlXPathOp::XPathOpSort
                )
            {
                let f: i32 = self.comp.steps[self.comp.steps[op].ch2 as usize].ch1;

                if f != -1
                    && matches!(self.comp.steps[f as usize].op, XmlXPathOp::XPathOpFunction)
                    && self.comp.steps[f as usize].value5.is_none()
                    && self.comp.steps[f as usize].value == 0
                    && self.comp.steps[f as usize]
                        .value4
                        .as_ref()
                        .and_then(|val| val.as_str())
                        .is_some_and(|s| s == "last")
                {
                    let mut last = None;

                    total += self.evaluate_precompiled_operation_last(
                        self.comp.steps[op].ch1 as usize,
                        &mut last,
                    );
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        return 0;
                    };
                    // The nodeset should be in document order,
                    // Keep only the last value
                    if self
                        .value()
                        .is_some_and(|value| (*value).typ == XmlXPathObjectType::XPathNodeset)
                    {
                        if let Some(nodeset) = (*self.value().unwrap())
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

            if self.comp.steps[op].ch1 != -1 {
                total += self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
            }
            if self.error != XmlXPathError::XPathExpressionOK as i32 {
                return 0;
            };
            if self.comp.steps[op].ch2 == -1 {
                return total;
            }
            if self.value().is_none() {
                return total;
            }

            #[cfg(feature = "libxml_xptr_locs")]
            {
                // Hum are we filtering the result of an XPointer expression
                if matches!(
                    (*self.value().unwrap()).typ,
                    XmlXPathObjectType::XPathLocationset
                ) {
                    if let Some(locset) = (**self.value_mut().unwrap())
                        .user
                        .as_mut()
                        .and_then(|user| user.as_location_set_mut())
                    {
                        xml_xpath_location_set_filter(self, locset, self.comp.steps[op].ch2, 1, 1);
                        if !locset.loc_tab.is_empty() {
                            *first = locset.loc_tab[0]
                                .user
                                .as_ref()
                                .and_then(|user| user.as_node())
                                .copied();
                        }
                    }

                    return total;
                }
            }

            // In case of errors, xmlXPathNodeSetFilter can pop additional nodes from the stack.
            // We have to temporarily remove the nodeset object from the
            // stack to avoid freeing it prematurely.
            if self
                .value()
                .is_none_or(|value| (*value).typ != XmlXPathObjectType::XPathNodeset)
            {
                xml_xpath_err(Some(self), XmlXPathError::XPathInvalidType as i32);
                return 0;
            };
            let obj: XmlXPathObjectPtr = self.value_pop();
            if let Some(set) = (*obj).nodesetval.as_deref_mut() {
                xml_xpath_node_set_filter(self, Some(set), self.comp.steps[op].ch2, 1, 1, true);
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
        mut op: StepOpIndex,
        is_predicate: bool,
    ) -> i32 {
        unsafe {
            let res_obj: XmlXPathObjectPtr;

            // start:
            loop {
                if (*self.context).op_limit != 0 && self.check_operation_limit(1) < 0 {
                    return 0;
                }
                // comp = self.comp;
                match self.comp.steps[op].op {
                    XmlXPathOp::XPathOpEnd => {
                        return 0;
                    }
                    XmlXPathOp::XPathOpValue => {
                        let res_obj = self.comp.steps[op]
                            .value4
                            .as_ref()
                            .and_then(|val| val.as_object())
                            .unwrap();
                        if is_predicate {
                            return xml_xpath_evaluate_predicate_result(self, res_obj);
                        }
                        return xml_xpath_cast_to_boolean(res_obj) as i32;
                    }
                    XmlXPathOp::XPathOpSort => {
                        // We don't need sorting for boolean results. Skip this one.
                        if self.comp.steps[op].ch1 != -1 {
                            op = self.comp.steps[op].ch1 as usize;
                            // goto start;
                            continue;
                        }
                        return 0;
                    }
                    XmlXPathOp::XPathOpCollect => {
                        if self.comp.steps[op].ch1 == -1 {
                            return 0;
                        }

                        self.evaluate_precompiled_operation(self.comp.steps[op].ch1 as usize);
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
                } else if is_predicate {
                    // For predicates a result of type "number" is handled
                    // differently:
                    // SPEC XPath 1.0:
                    // "If the result is a number, the result will be converted
                    //  to true if the number is equal to the context position
                    //  and will be converted to false otherwise;"
                    xml_xpath_evaluate_predicate_result(self, &*res_obj)
                } else {
                    xml_xpath_cast_to_boolean(&*res_obj) as i32
                };
                xml_xpath_release_object(self.context, res_obj);
                return res;
            }

            0
        }
    }
}

impl XmlXPathContext {
    /// Evaluate the Precompiled Streamable XPath expression in the given context.
    #[doc(alias = "xmlXPathRunStreamEval")]
    #[cfg(feature = "libxml_pattern")]
    unsafe fn run_stream_eval(
        &mut self,
        comp: &XmlPattern,
        result_seq: *mut XmlXPathObjectPtr,
        to_bool: i32,
    ) -> i32 {
        use crate::{
            error::{XmlErrorDomain, XmlParserErrors},
            tree::{XmlElementType, XmlNodePtr},
        };

        unsafe {
            let mut ret: i32;

            let mut max_depth = comp.max_depth();
            if max_depth == -1 {
                return -1;
            }
            if max_depth == -2 {
                max_depth = 10000;
            }
            let min_depth: i32 = comp.min_depth();
            if min_depth == -1 {
                return -1;
            }
            let from_root: i32 = comp.is_from_root();
            if from_root < 0 {
                return -1;
            }

            if to_bool == 0 {
                if result_seq.is_null() {
                    return -1;
                }
                *result_seq = xml_xpath_cache_new_node_set(self, None);
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
                        nodeset.add_unique(self.doc.unwrap().into());
                    }
                } else {
                    // Select "self::node()"
                    if to_bool != 0 {
                        return 1;
                    }
                    // TODO: Check memory error.
                    if let Some(nodeset) = (*(*result_seq)).nodesetval.as_deref_mut() {
                        nodeset.add_unique(self.node.unwrap());
                    }
                }
            }
            if max_depth == 0 {
                return 0;
            }

            let mut limit = None;
            let cur = if from_root != 0 {
                self.doc.map(|doc| doc.into())
            } else if let Some(node) = self.node {
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

            let Some(mut patstream) = comp.get_stream_context() else {
                // QUESTION TODO: Is this an error?
                return 0;
            };

            let eval_all_nodes: i32 = patstream.wants_any_node();

            if from_root != 0 {
                ret = patstream.push(None, None);
                if ret < 0 {
                    // no op
                } else if ret == 1 {
                    if to_bool != 0 {
                        return 1;
                    }
                    // TODO: Check memory error.
                    if let Some(nodeset) = (*(*result_seq)).nodesetval.as_deref_mut() {
                        nodeset.add_unique(cur);
                    }
                }
            }
            let mut depth = 0;
            let mut goto_scan_children = true;
            // goto scan_children;
            // next_node:
            'main: while {
                'to_continue_main: {
                    'next_node: loop {
                        if !goto_scan_children {
                            if self.op_limit != 0 {
                                if self.op_count >= self.op_limit {
                                    generic_error!("XPath operation limit exceeded\n");
                                    return -1;
                                }
                                self.op_count += 1;
                            }

                            match cur.element_type() {
                                XmlElementType::XmlElementNode
                                | XmlElementType::XmlTextNode
                                | XmlElementType::XmlCDATASectionNode
                                | XmlElementType::XmlCommentNode
                                | XmlElementType::XmlPINode => 'to_break: {
                                    ret = if matches!(
                                        cur.element_type(),
                                        XmlElementType::XmlElementNode
                                    ) {
                                        let node = XmlNodePtr::try_from(cur).unwrap();
                                        patstream.push(
                                            Some(&node.name),
                                            node.ns.as_deref().and_then(|ns| ns.href()).as_deref(),
                                        )
                                    } else if eval_all_nodes != 0 {
                                        patstream.push_node(None, None, cur.element_type() as i32)
                                    } else {
                                        break 'to_break;
                                    };
                                    if ret < 0 {
                                        // NOP.
                                    } else if ret == 1 {
                                        if to_bool != 0 {
                                            return 1;
                                        }
                                        if let Some(nodeset) =
                                            (*(*result_seq)).nodesetval.as_deref_mut()
                                        {
                                            if nodeset.add_unique(cur) < 0 {
                                                self.last_error.domain =
                                                    XmlErrorDomain::XmlFromXPath;
                                                self.last_error.code =
                                                    XmlParserErrors::XmlErrNoMemory;
                                            }
                                        }
                                    }
                                    if cur.children().is_none() || depth >= max_depth {
                                        // ret =
                                        patstream.pop();
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
                            patstream.pop();
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

            0
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
    /// Returns `true` if predicate is true, `false` otherwise
    #[doc(alias = "xmlXPathEvalPredicate")]
    pub fn evaluate_predicate(&mut self, res: &XmlXPathObject) -> bool {
        match res.typ {
            XmlXPathObjectType::XPathBoolean => {
                return res.boolval;
            }
            XmlXPathObjectType::XPathNumber => {
                return res.floatval == self.proximity_position as _;
            }
            XmlXPathObjectType::XPathNodeset | XmlXPathObjectType::XPathXSLTTree => {
                let Some(nodeset) = res.nodesetval.as_deref() else {
                    return false;
                };
                return !nodeset.node_tab.is_empty();
            }
            XmlXPathObjectType::XPathString => {
                return res.stringval.as_deref().is_some_and(|s| !s.is_empty());
            }
            _ => {
                generic_error!("Internal error at {}:{}\n", file!(), line!());
            }
        }
        false
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
    op: StepOpIndex,
    mut first: Option<&mut Option<XmlGenericNodePtr>>,
    mut last: Option<&mut Option<XmlGenericNodePtr>>,
    to_bool: i32,
) -> i32 {
    unsafe {
        let axis: XmlXPathAxisVal = (*ctxt).comp.steps[op].value.try_into().unwrap();
        let test: XmlXPathTestVal = (*ctxt).comp.steps[op].value2.try_into().unwrap();
        let typ: XmlXPathTypeVal = (*ctxt).comp.steps[op].value3.try_into().unwrap();
        let prefix = (*ctxt).comp.steps[op]
            .value4
            .as_ref()
            .and_then(|val| val.as_str());
        let name = (*ctxt).comp.steps[op]
            .value5
            .as_ref()
            .and_then(|val| val.as_str());
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
            xml_xpath_err(Some(&mut *ctxt), XmlXPathError::XPathInvalidType as i32);
            return 0;
        };
        // The popped object holding the context nodes
        let obj: XmlXPathObjectPtr = (*ctxt).value_pop();
        // Setup namespaces.
        if let Some(prefix) = prefix {
            uri = (*xpctxt).lookup_ns(prefix);
            if uri.is_none() {
                xml_xpath_release_object(xpctxt, obj);
                xml_xpath_err(
                    Some(&mut *ctxt),
                    XmlXPathError::XPathUndefPrefixError as i32,
                );
                return 0;
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
        //   ROOT                               -- (*ctxt).comp.steps[op].ch1
        //   PREDICATE                          -- (*ctxt).comp.steps[op].ch2 (predOp)
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
        if (*ctxt).comp.steps[op].ch2 != -1 {
            // There's at least one predicate. 16 == XPATH_OP_PREDICATE
            pred_op = &raw mut (*ctxt).comp.steps[(*ctxt).comp.steps[op].ch2 as usize];
            if (*ctxt).is_positional_predicate(&*pred_op, &mut max_pos) != 0 {
                if (*pred_op).ch1 != -1 {
                    // Use the next inner predicate operator.
                    pred_op = &raw mut (*ctxt).comp.steps[(*pred_op).ch1 as usize];
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

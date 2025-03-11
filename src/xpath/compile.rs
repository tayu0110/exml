use std::{
    ffi::{CStr, c_void},
    ptr::null_mut,
};

use libc::memcpy;

use crate::{
    generic_error,
    libxml::{
        chvalid::{
            xml_is_blank_char, xml_is_char, xml_is_combining, xml_is_digit, xml_is_extender,
        },
        globals::{xml_free, xml_malloc_atomic, xml_realloc},
        parser_internals::{XML_MAX_NAME_LENGTH, XML_MAX_NAMELEN, xml_is_letter},
        xmlstring::{xml_str_equal, xml_strlen, xml_strndup, xml_strstr},
    },
    xpath::{
        XML_XPATH_CHECKNS, XPATH_MAX_RECURSION_DEPTH, XmlXPathAxisVal, XmlXPathContextPtr,
        XmlXPathError, XmlXPathObjectPtr, XmlXPathOp, xml_xpath_cache_new_float, xml_xpath_err,
        xml_xpath_ns_lookup, xml_xpath_release_object,
    },
};

use super::{
    MAX_FRAC, XML_XPATH_NOVAR, XPATH_MAX_STEPS, XmlXPathCompExprPtr, XmlXPathObjectType,
    XmlXPathParserContext, XmlXPathStepOp, XmlXPathTestVal, XmlXPathTypeVal,
    xml_xpath_cache_new_string, xml_xpath_is_node_type, xml_xpath_perr_memory,
};

macro_rules! COPY_BUF {
    ($l:expr, $b:expr, $i:expr, $v:expr) => {
        if $l == 1 {
            *$b.add($i as usize) = $v as _;
            $i += 1;
        } else {
            $i += $crate::libxml::parser_internals::xml_copy_char(
                $l,
                $b.add($i as usize) as _,
                $v as _,
            );
        }
    };
}

impl XmlXPathParserContext {
    /// Add a step to an XPath Compiled Expression
    ///
    /// Returns -1 in case of failure, the index otherwise
    #[allow(clippy::too_many_arguments)]
    #[doc(alias = "xmlXPathCompExprAdd")]
    unsafe fn add_compiled_expression(
        &mut self,
        ch1: i32,
        ch2: i32,
        op: XmlXPathOp,
        value: i32,
        value2: i32,
        value3: i32,
        value4: *mut c_void,
        value5: *mut c_void,
    ) -> i32 {
        unsafe {
            let comp: XmlXPathCompExprPtr = self.comp;
            if (*comp).steps.len() == XPATH_MAX_STEPS {
                xml_xpath_perr_memory(self, Some("adding step\n"));
                return -1;
            }
            (*comp).last = (*comp).steps.len() as i32;
            (*comp).steps.push(XmlXPathStepOp {
                ch1,
                ch2,
                op,
                value,
                value2,
                value3,
                value4,
                value5,
                cache: None,
                cache_uri: null_mut(),
            });
            (*comp).steps.len() as i32 - 1
        }
    }

    /// ```text
    /// [14]   Expr ::=   OrExpr
    /// [21]   OrExpr ::=   AndExpr | OrExpr 'or' AndExpr
    /// ```
    ///
    /// Parse and compile an expression
    #[doc(alias = "xmlXPathCompileExpr")]
    pub unsafe fn compile_expr(&mut self, sort: i32) {
        unsafe {
            let xpctxt: XmlXPathContextPtr = self.context;

            if !xpctxt.is_null() {
                if (*xpctxt).depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
                    xml_xpath_err(self, XmlXPathError::XPathRecursionLimitExceeded as i32);
                    return;
                }
                // Parsing a single '(' pushes about 10 functions on the call stack before recursing!
                (*xpctxt).depth += 10;
            }

            self.compile_and_expr();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            self.skip_blanks();
            while self.current_char() == Some('o') && self.nth_byte(1) == Some(b'r') {
                let op1: i32 = (*self.comp).last;
                self.cur += 2;
                self.skip_blanks();
                self.compile_and_expr();
                if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                    return;
                };
                self.add_compiled_expression(
                    op1,
                    (*self.comp).last,
                    XmlXPathOp::XpathOpOr,
                    0,
                    0,
                    0,
                    null_mut(),
                    null_mut(),
                );
                self.skip_blanks();
            }
            if sort != 0
                && !matches!(
                    (*self.comp).steps[(*self.comp).last as usize].op,
                    XmlXPathOp::XpathOpValue
                )
            {
                // more ops could be optimized too
                // This is the main place to eliminate sorting for
                // operations which don't require a sorted node-set.
                // E.g. count().
                self.add_compiled_expression(
                    (*self.comp).last,
                    -1,
                    XmlXPathOp::XpathOpSort,
                    0,
                    0,
                    0,
                    null_mut(),
                    null_mut(),
                );
            }

            if !xpctxt.is_null() {
                (*xpctxt).depth -= 10;
            }
        }
    }

    /// `[22]   AndExpr ::=   EqualityExpr | AndExpr 'and' EqualityExpr`
    ///
    /// Compile an AND expression.
    #[doc(alias = "xmlXPathCompAndExpr")]
    unsafe fn compile_and_expr(&mut self) {
        unsafe {
            self.compile_equality_expr();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            self.skip_blanks();
            while self.current_char() == Some('a')
                && self.nth_byte(1) == Some(b'n')
                && self.nth_byte(2) == Some(b'd')
            {
                let op1: i32 = (*self.comp).last;
                self.cur += 3;
                self.skip_blanks();
                self.compile_equality_expr();
                if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                    return;
                };
                self.add_compiled_expression(
                    op1,
                    (*self.comp).last,
                    XmlXPathOp::XpathOpAnd,
                    0,
                    0,
                    0,
                    null_mut(),
                    null_mut(),
                );
                self.skip_blanks();
            }
        }
    }

    /// ```text
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
    unsafe fn compile_equality_expr(&mut self) {
        unsafe {
            self.compile_relational_expr();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            self.skip_blanks();
            while self.current_char() == Some('=')
                || (self.current_char() == Some('!') && self.nth_byte(1) == Some(b'='))
            {
                let op1: i32 = (*self.comp).last;
                let eq = (self.current_char() == Some('=')) as i32;

                self.next_char();
                if eq == 0 {
                    self.next_char();
                }
                self.skip_blanks();
                self.compile_relational_expr();
                if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                    return;
                };
                self.add_compiled_expression(
                    op1,
                    (*self.comp).last,
                    XmlXPathOp::XpathOpEqual,
                    eq,
                    0,
                    0,
                    null_mut(),
                    null_mut(),
                );
                self.skip_blanks();
            }
        }
    }

    /// ```text
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
    unsafe fn compile_relational_expr(&mut self) {
        unsafe {
            self.compile_additive_expr();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            self.skip_blanks();
            while self.current_char() == Some('<') || self.current_char() == Some('>') {
                let op1: i32 = (*self.comp).last;
                let inf = (self.current_char() == Some('<')) as i32;
                let strict = (self.nth_byte(1) != Some(b'=')) as i32;

                self.next_char();
                if strict == 0 {
                    self.next_char();
                }
                self.skip_blanks();
                self.compile_additive_expr();
                if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                    return;
                };
                self.add_compiled_expression(
                    op1,
                    (*self.comp).last,
                    XmlXPathOp::XpathOpCmp,
                    inf,
                    strict,
                    0,
                    null_mut(),
                    null_mut(),
                );
                self.skip_blanks();
            }
        }
    }

    /// ```text
    /// [25]   AdditiveExpr ::=   MultiplicativeExpr
    ///                   | AdditiveExpr '+' MultiplicativeExpr
    ///                   | AdditiveExpr '-' MultiplicativeExpr
    /// ```
    ///
    /// Compile an Additive expression.
    #[doc(alias = "xmlXPathCompAdditiveExpr")]
    unsafe fn compile_additive_expr(&mut self) {
        unsafe {
            self.compile_multiplicative_expr();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            self.skip_blanks();
            while self.current_char() == Some('+') || self.current_char() == Some('-') {
                let op1: i32 = (*self.comp).last;
                let plus = (self.current_char() == Some('+')) as i32;

                self.next_char();
                self.skip_blanks();
                self.compile_multiplicative_expr();
                if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                    return;
                };
                self.add_compiled_expression(
                    op1,
                    (*self.comp).last,
                    XmlXPathOp::XpathOpPlus,
                    plus,
                    0,
                    0,
                    null_mut(),
                    null_mut(),
                );
                self.skip_blanks();
            }
        }
    }

    /// ```text
    /// [26]   MultiplicativeExpr ::=   UnaryExpr
    ///                  | MultiplicativeExpr MultiplyOperator UnaryExpr
    ///                  | MultiplicativeExpr 'div' UnaryExpr
    ///                  | MultiplicativeExpr 'mod' UnaryExpr
    /// [34]   MultiplyOperator ::=   '*'
    /// ```
    ///
    /// Compile an Additive expression.
    #[doc(alias = "xmlXPathCompMultiplicativeExpr")]
    unsafe fn compile_multiplicative_expr(&mut self) {
        unsafe {
            self.compile_unary_expr();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            self.skip_blanks();
            while self.current_char() == Some('*')
                || (self.current_char() == Some('d')
                    && self.nth_byte(1) == Some(b'i')
                    && self.nth_byte(2) == Some(b'v'))
                || (self.current_char() == Some('m')
                    && self.nth_byte(1) == Some(b'o')
                    && self.nth_byte(2) == Some(b'd'))
            {
                let mut op: i32 = -1;
                let op1: i32 = (*self.comp).last;

                if self.current_char() == Some('*') {
                    op = 0;
                    self.next_char();
                } else if self.current_char() == Some('d') {
                    op = 1;
                    self.cur += 3;
                } else if self.current_char() == Some('m') {
                    op = 2;
                    self.cur += 3;
                }
                self.skip_blanks();
                self.compile_unary_expr();
                if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                    return;
                };
                self.add_compiled_expression(
                    op1,
                    (*self.comp).last,
                    XmlXPathOp::XpathOpMult,
                    op,
                    0,
                    0,
                    null_mut(),
                    null_mut(),
                );
                self.skip_blanks();
            }
        }
    }

    /// `[27]   UnaryExpr ::=   UnionExpr | '-' UnaryExpr`
    ///
    /// Compile an unary expression.
    #[doc(alias = "xmlXPathCompUnaryExpr")]
    unsafe fn compile_unary_expr(&mut self) {
        unsafe {
            let mut minus: i32 = 0;
            let mut found: i32 = 0;

            self.skip_blanks();
            while self.current_char() == Some('-') {
                minus = 1 - minus;
                found = 1;
                self.next_char();
                self.skip_blanks();
            }

            self.compile_union_expr();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            if found != 0 {
                if minus != 0 {
                    self.add_compiled_expression(
                        (*self.comp).last,
                        -1,
                        XmlXPathOp::XpathOpPlus,
                        2,
                        0,
                        0,
                        null_mut(),
                        null_mut(),
                    );
                } else {
                    self.add_compiled_expression(
                        (*self.comp).last,
                        -1,
                        XmlXPathOp::XpathOpPlus,
                        3,
                        0,
                        0,
                        null_mut(),
                        null_mut(),
                    );
                }
            }
        }
    }

    /// `[18]   UnionExpr ::=   PathExpr | UnionExpr '|' PathExpr`
    ///
    /// Compile an union expression.
    #[doc(alias = "xmlXPathCompUnionExpr")]
    unsafe fn compile_union_expr(&mut self) {
        unsafe {
            self.compile_path_expr();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            self.skip_blanks();
            while self.current_char() == Some('|') {
                let op1: i32 = (*self.comp).last;
                self.add_compiled_expression(
                    -1,
                    -1,
                    XmlXPathOp::XpathOpNode,
                    0,
                    0,
                    0,
                    null_mut(),
                    null_mut(),
                );

                self.next_char();
                self.skip_blanks();
                self.compile_path_expr();

                self.add_compiled_expression(
                    op1,
                    (*self.comp).last,
                    XmlXPathOp::XpathOpUnion,
                    0,
                    0,
                    0,
                    null_mut(),
                    null_mut(),
                );

                self.skip_blanks();
            }
        }
    }

    /// ```text
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
    unsafe fn compile_path_expr(&mut self) {
        unsafe {
            let mut lc: i32 = 1; /* Should we branch to LocationPath ?         */
            let name: *mut u8; /* we may have to preparse a name to find out */

            self.skip_blanks();
            if self.current_char() == Some('$')
                || self.current_char() == Some('(')
                || self.current_char().is_some_and(|c| c.is_ascii_digit())
                || self.current_char() == Some('\'')
                || self.current_char() == Some('"')
                || (self.current_char() == Some('.')
                    && self.nth_byte(1).is_some_and(|c| c.is_ascii_digit()))
            {
                lc = 0;
            } else if self.current_char() == Some('*') || self.current_char() == Some('/') {
                // relative or absolute location path
                lc = 1;
            } else if self.current_char() == Some('@') || self.current_char() == Some('.') {
                // relative abbreviated attribute location path
                lc = 1;
            } else {
                // Problem is finding if we have a name here whether it's:
                //   - a nodetype
                //   - a function call in which case it's followed by '('
                //   - an axis in which case it's followed by ':'
                //   - a element name
                // We do an a priori analysis here rather than having to
                // maintain parsed token content through the recursive function
                // calls. This looks uglier but makes the code easier to
                // read/write/debug.
                self.skip_blanks();
                name = self.scan_name();
                if !name.is_null() && !xml_strstr(name, c"::".as_ptr() as _).is_null() {
                    lc = 1;
                    xml_free(name as _);
                } else if !name.is_null() {
                    let mut len: i32 = xml_strlen(name);

                    while self.nth_byte(len as usize).is_some() {
                        if self.nth_byte(len as usize) == Some(b'/') {
                            // element name
                            lc = 1;
                            break;
                        } else if self
                            .nth_byte(len as usize)
                            .is_some_and(|c| xml_is_blank_char(c as u32))
                        {
                            // ignore blanks
                        } else if self.nth_byte(len as usize) == Some(b':') {
                            lc = 1;
                            break;
                        } else if self.nth_byte(len as usize) == Some(b'(') {
                            // Node Type or Function
                            if xml_xpath_is_node_type(name) != 0 {
                                lc = 1;
                            } else {
                                #[cfg(feature = "libxml_xptr_locs")]
                                if self.xptr != 0 && xml_str_equal(name, c"range-to".as_ptr() as _)
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
                        } else if self.nth_byte(len as usize) == Some(b'[') {
                            // element name
                            lc = 1;
                            break;
                        // } else if self.nth_byte(len as usize) == Some(b'<')
                        //     || self.nth_byte(len as usize) == Some(b'>')
                        //     || self.nth_byte(len as usize) == Some(b'=')
                        // {
                        //     lc = 1;
                        //     break;
                        } else {
                            lc = 1;
                            break;
                        }
                        len += 1;
                    }
                    if self.nth_byte(len as usize).is_none() {
                        // element name
                        lc = 1;
                    }
                    xml_free(name as _);
                } else {
                    // make sure all cases are covered explicitly
                    xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                    return;
                }
            }

            if lc != 0 {
                if self.current_char() == Some('/') {
                    self.add_compiled_expression(
                        -1,
                        -1,
                        XmlXPathOp::XpathOpRoot,
                        0,
                        0,
                        0,
                        null_mut(),
                        null_mut(),
                    );
                } else {
                    self.add_compiled_expression(
                        -1,
                        -1,
                        XmlXPathOp::XpathOpNode,
                        0,
                        0,
                        0,
                        null_mut(),
                        null_mut(),
                    );
                }
                self.compile_location_path();
            } else {
                self.compile_filter_expr();
                if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                    return;
                };
                if self.current_char() == Some('/') && self.nth_byte(1) == Some(b'/') {
                    self.cur += 2;
                    self.skip_blanks();

                    self.add_compiled_expression(
                        (*self.comp).last,
                        -1,
                        XmlXPathOp::XpathOpCollect,
                        (XmlXPathAxisVal::AxisDescendantOrSelf) as i32,
                        (XmlXPathTestVal::NodeTestType) as i32,
                        (XmlXPathTypeVal::NodeTypeNode) as i32,
                        null_mut(),
                        null_mut(),
                    );

                    self.compile_relative_location_path();
                } else if self.current_char() == Some('/') {
                    self.compile_relative_location_path();
                }
            }
            self.skip_blanks();
        }
    }

    /// ```text
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
    unsafe fn compile_location_path(&mut self) {
        unsafe {
            self.skip_blanks();
            if self.current_char() != Some('/') {
                self.compile_relative_location_path();
            } else {
                while self.current_char() == Some('/') {
                    if self.current_char() == Some('/') && self.nth_byte(1) == Some(b'/') {
                        self.cur += 2;
                        self.skip_blanks();
                        self.add_compiled_expression(
                            (*self.comp).last,
                            -1,
                            XmlXPathOp::XpathOpCollect,
                            (XmlXPathAxisVal::AxisDescendantOrSelf) as i32,
                            (XmlXPathTestVal::NodeTestType) as i32,
                            (XmlXPathTypeVal::NodeTypeNode) as i32,
                            null_mut(),
                            null_mut(),
                        );
                        self.compile_relative_location_path();
                    } else if self.current_char() == Some('/') {
                        self.next_char();
                        self.skip_blanks();
                        if self.current_char().is_some_and(|c| {
                            c.is_ascii_alphabetic() || c == '_' || c == '.' || c == '@' || c == '*'
                        }) {
                            self.compile_relative_location_path();
                        }
                    }
                    if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                        return;
                    };
                }
            }
        }
    }

    /// ```text
    /// [3]   RelativeLocationPath ::=   Step
    ///                     | RelativeLocationPath '/' Step
    ///                     | AbbreviatedRelativeLocationPath
    /// [11]  AbbreviatedRelativeLocationPath ::=   RelativeLocationPath '//' Step
    /// ```
    ///
    /// Compile a relative location path.
    #[doc(alias = "xmlXPathCompRelativeLocationPath")]
    unsafe fn compile_relative_location_path(&mut self) {
        unsafe {
            self.skip_blanks();
            if self.current_char() == Some('/') && self.nth_byte(1) == Some(b'/') {
                self.cur += 2;
                self.skip_blanks();
                self.add_compiled_expression(
                    (*self.comp).last,
                    -1,
                    XmlXPathOp::XpathOpCollect,
                    (XmlXPathAxisVal::AxisDescendantOrSelf) as i32,
                    (XmlXPathTestVal::NodeTestType) as i32,
                    (XmlXPathTypeVal::NodeTypeNode) as i32,
                    null_mut(),
                    null_mut(),
                );
            } else if self.current_char() == Some('/') {
                self.next_char();
                self.skip_blanks();
            }
            self.compile_step();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            self.skip_blanks();
            while self.current_char() == Some('/') {
                if self.current_char() == Some('/') && self.nth_byte(1) == Some(b'/') {
                    self.cur += 2;
                    self.skip_blanks();
                    self.add_compiled_expression(
                        (*self.comp).last,
                        -1,
                        XmlXPathOp::XpathOpCollect,
                        (XmlXPathAxisVal::AxisDescendantOrSelf) as i32,
                        (XmlXPathTestVal::NodeTestType) as i32,
                        (XmlXPathTypeVal::NodeTypeNode) as i32,
                        null_mut(),
                        null_mut(),
                    );
                    self.compile_step();
                } else if self.current_char() == Some('/') {
                    self.next_char();
                    self.skip_blanks();
                    self.compile_step();
                }
                self.skip_blanks();
            }
        }
    }

    /// ```text
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
    unsafe fn compile_step(&mut self) {
        unsafe {
            #[cfg(feature = "libxml_xptr_locs")]
            let mut rangeto: i32 = 0;
            #[cfg(feature = "libxml_xptr_locs")]
            let mut op2: i32 = -1;

            self.skip_blanks();
            if self.current_char() == Some('.') && self.nth_byte(1) == Some(b'.') {
                self.cur += 2;
                self.skip_blanks();
                self.add_compiled_expression(
                    (*self.comp).last,
                    -1,
                    XmlXPathOp::XpathOpCollect,
                    (XmlXPathAxisVal::AxisParent) as i32,
                    (XmlXPathTestVal::NodeTestType) as i32,
                    (XmlXPathTypeVal::NodeTypeNode) as i32,
                    null_mut(),
                    null_mut(),
                );
            } else if self.current_char() == Some('.') {
                self.next_char();
                self.skip_blanks();
            } else {
                let mut name: *mut u8 = null_mut();
                let mut prefix: *mut u8 = null_mut();
                let mut test: XmlXPathTestVal = XmlXPathTestVal::NodeTestNone;
                #[cfg(not(feature = "libxml_xptr_locs"))]
                let mut axis: Option<XmlXPathAxisVal>;
                #[cfg(feature = "libxml_xptr_locs")]
                let mut axis = None;
                let mut typ: XmlXPathTypeVal = XmlXPathTypeVal::NodeTypeNode;

                // The modification needed for XPointer change to the production
                #[cfg_attr(not(feature = "libxml_xptr_locs"), allow(unused_labels))]
                'eval_predicates: {
                    #[cfg(feature = "libxml_xptr_locs")]
                    if self.xptr != 0 {
                        name = self.parse_ncname();
                        if !name.is_null() && xml_str_equal(name, c"range-to".as_ptr() as _) {
                            op2 = (*self.comp).last;
                            xml_free(name as _);
                            self.skip_blanks();
                            if self.current_char() != Some('(') {
                                xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                                return;
                            }
                            self.next_char();
                            self.skip_blanks();

                            self.compile_expr(1);
                            /* PUSH_BINARY_EXPR(XPATH_OP_RANGETO, op2, (*self.comp).last, 0, 0); */
                            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                                return;
                            };

                            self.skip_blanks();
                            if self.current_char() != Some(')') {
                                xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                                return;
                            }
                            self.next_char();
                            rangeto = 1;
                            break 'eval_predicates;
                        }
                    }

                    if self.current_char() == Some('*') {
                        axis = Some(XmlXPathAxisVal::AxisChild);
                    } else {
                        if name.is_null() {
                            name = self.parse_ncname();
                        }
                        if !name.is_null() {
                            axis = is_axis_name(
                                CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
                            );
                            if axis.is_some() {
                                self.skip_blanks();
                                if self.current_char() == Some(':')
                                    && self.nth_byte(1) == Some(b':')
                                {
                                    self.cur += 2;
                                    xml_free(name as _);
                                    name = null_mut();
                                } else {
                                    // an element name can conflict with an axis one :-\
                                    axis = Some(XmlXPathAxisVal::AxisChild);
                                }
                            } else {
                                axis = Some(XmlXPathAxisVal::AxisChild);
                            }
                        } else if self.current_char() == Some('@') {
                            self.next_char();
                            axis = Some(XmlXPathAxisVal::AxisAttribute);
                        } else {
                            axis = Some(XmlXPathAxisVal::AxisChild);
                        }
                    }

                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        xml_free(name as _);
                        return;
                    }

                    name =
                        self.compile_node_test(&raw mut test, &raw mut typ, &raw mut prefix, name);
                    if matches!(test, XmlXPathTestVal::NodeTestNone) {
                        return;
                    }

                    if (!prefix.is_null()
                        && !self.context.is_null()
                        && (*self.context).flags & XML_XPATH_CHECKNS as i32 != 0)
                        && xml_xpath_ns_lookup(self.context, prefix).is_null()
                    {
                        xml_xpath_err(self, XmlXPathError::XPathUndefPrefixError as i32);
                    }
                }

                let op1: i32 = (*self.comp).last;
                (*self.comp).last = -1;

                self.skip_blanks();
                #[allow(clippy::while_immutable_condition)]
                while self.current_char() == Some('[') {
                    self.compile_predicate(0);
                }

                #[cfg(feature = "libxml_xptr_locs")]
                if rangeto != 0 {
                    self.add_compiled_expression(
                        op2,
                        op1,
                        XmlXPathOp::XpathOpRangeto,
                        0,
                        0,
                        0,
                        null_mut(),
                        null_mut(),
                    );
                    return;
                }

                if self.add_compiled_expression(
                    op1,
                    (*self.comp).last,
                    XmlXPathOp::XpathOpCollect,
                    (axis.unwrap_or_else(|| {
                        panic!(
                            "Invalid xmlXPathAxisVal: file: {}, line: {}",
                            file!(),
                            line!()
                        )
                    })) as i32,
                    test as i32,
                    typ as i32,
                    prefix as _,
                    name as _,
                ) == -1
                {
                    xml_free(prefix as _);
                    xml_free(name as _);
                }
            }
        }
    }

    /// ```text
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
    unsafe fn compile_node_test(
        &mut self,
        test: *mut XmlXPathTestVal,
        typ: *mut XmlXPathTypeVal,
        prefix: *mut *mut u8,
        mut name: *mut u8,
    ) -> *mut u8 {
        unsafe {
            if test.is_null() || typ.is_null() || prefix.is_null() {
                generic_error!("Internal error at {}:{}\n", file!(), line!());
                return null_mut();
            }
            *typ = XmlXPathTypeVal::NodeTypeNode;
            *test = XmlXPathTestVal::NodeTestNone;
            *prefix = null_mut();
            self.skip_blanks();

            if name.is_null() && self.current_char() == Some('*') {
                // All elements
                self.next_char();
                *test = XmlXPathTestVal::NodeTestAll;
                return null_mut();
            }

            if name.is_null() {
                name = self.parse_ncname();
            }
            if name.is_null() {
                xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                return null_mut();
            }

            let blanks: i32 = self
                .current_char()
                .is_some_and(|c| xml_is_blank_char(c as u32)) as i32;
            self.skip_blanks();
            if self.current_char() == Some('(') {
                self.next_char();
                // NodeType or PI search
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
                    xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                    return null_mut();
                }

                *test = XmlXPathTestVal::NodeTestType;

                self.skip_blanks();
                if matches!(*typ, XmlXPathTypeVal::NodeTypePI) {
                    // Specific case: search a PI by name.
                    if !name.is_null() {
                        xml_free(name as _);
                    }
                    name = null_mut();
                    if self.current_char() != Some(')') {
                        name = self.parse_literal();
                        if name.is_null() {
                            xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                            return null_mut();
                        }
                        *test = XmlXPathTestVal::NodeTestPI;
                        self.skip_blanks();
                    }
                }
                if self.current_char() != Some(')') {
                    if !name.is_null() {
                        xml_free(name as _);
                    }
                    xml_xpath_err(self, XmlXPathError::XPathUnclosedError as i32);
                    return null_mut();
                }
                self.next_char();
                return name;
            }
            *test = XmlXPathTestVal::NodeTestName;
            if blanks == 0 && self.current_char() == Some(':') {
                self.next_char();

                // Since currently the parser context don't have a
                // namespace list associated:
                // The namespace name for this prefix can be computed
                // only at evaluation time. The compilation is done
                // outside of any context.
                // #if 0
                // 	*prefix = xmlXPathNsLookup(self.context, name);
                // 	if (name != NULL) {
                // 	    xmlFree(name as _);
                // 	}
                // 	if (*prefix.is_null()) {
                // 	    XP_ERROR0!(ctxt, XmlXPathError::XPATH_UNDEF_PREFIX_ERROR as i32);
                // 	}
                // #else
                *prefix = name;
                // #endif

                if self.current_char() == Some('*') {
                    // All elements
                    self.next_char();
                    *test = XmlXPathTestVal::NodeTestAll;
                    return null_mut();
                }

                name = self.parse_ncname();
                if name.is_null() {
                    xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                    return null_mut();
                }
            }
            name
        }
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
    unsafe fn compile_filter_expr(&mut self) {
        unsafe {
            self.compile_primary_expr();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            self.skip_blanks();

            while self.current_char() == Some('[') {
                self.compile_predicate(1);
                self.skip_blanks();
            }
        }
    }

    /// ```text
    /// [15]   PrimaryExpr ::=   VariableReference
    ///                | '(' Expr ')'
    ///                | Literal
    ///                | Number
    ///                | FunctionCall
    /// ```
    ///
    /// Compile a primary expression.
    #[doc(alias = "xmlXPathCompPrimaryExpr")]
    unsafe fn compile_primary_expr(&mut self) {
        unsafe {
            self.skip_blanks();
            if self.current_char() == Some('$') {
                self.compile_variable_reference();
            } else if self.current_char() == Some('(') {
                self.next_char();
                self.skip_blanks();
                self.compile_expr(1);
                if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                    return;
                };
                if self.current_char() != Some(')') {
                    xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                    return;
                }
                self.next_char();
                self.skip_blanks();
            } else if self.current_char().is_some_and(|c| c.is_ascii_digit())
                || (self.current_char() == Some('.')
                    && self.nth_byte(1).is_some_and(|c| c.is_ascii_digit()))
            {
                self.compile_number();
            } else if self.current_char() == Some('\'') || self.current_char() == Some('"') {
                self.compile_literal();
            } else {
                self.compile_function_call();
            }
            self.skip_blanks();
        }
    }

    /// ```text
    /// [30]   Number ::=   Digits ('.' Digits?)?
    ///                   | '.' Digits
    /// [31]   Digits ::=   [0-9]+
    /// ```
    ///
    /// Compile a Number, then push it on the stack
    #[doc(alias = "xmlXPathCompNumber")]
    unsafe fn compile_number(&mut self) {
        unsafe {
            let mut ret: f64;
            let mut ok: i32 = 0;
            let mut exponent: i32 = 0;
            let mut is_exponent_negative: i32 = 0;

            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            if self.current_char() != Some('.')
                && self.current_char().is_none_or(|c| !c.is_ascii_digit())
            {
                xml_xpath_err(self, XmlXPathError::XPathNumberError as i32);
                return;
            }
            ret = 0.0;
            while let Some(c) = self.current_char().filter(|c| c.is_ascii_digit()) {
                ret = ret * 10.0 + (c as u8 - b'0') as f64;
                ok = 1;
                self.next_char();
            }
            if self.current_char() == Some('.') {
                let mut v: i32;
                let mut frac: i32 = 0;
                let mut fraction: f64 = 0.0;

                self.next_char();
                if self.current_char().is_none_or(|c| !c.is_ascii_digit()) && ok == 0 {
                    xml_xpath_err(self, XmlXPathError::XPathNumberError as i32);
                    return;
                }
                while self.current_char() == Some('0') {
                    frac += 1;
                    self.next_char();
                }
                let max: i32 = frac + MAX_FRAC as i32;
                while let Some(c) = self
                    .current_char()
                    .filter(|c| c.is_ascii_digit() && frac < max)
                {
                    v = (c as u8 - b'0') as i32;
                    fraction = fraction * 10.0 + v as f64;
                    frac += 1;
                    self.next_char();
                }
                fraction /= 10.0f64.powi(frac);
                ret += fraction;
                while self.current_char().is_some_and(|c| c.is_ascii_digit()) {
                    self.next_char();
                }
            }
            if self.current_char() == Some('e') || self.current_char() == Some('E') {
                self.next_char();
                if self.current_char() == Some('-') {
                    is_exponent_negative = 1;
                    self.next_char();
                } else if self.current_char() == Some('+') {
                    self.next_char();
                }
                while let Some(c) = self.current_char().filter(|c| c.is_ascii_digit()) {
                    if exponent < 1000000 {
                        exponent = exponent * 10 + (c as u8 - b'0') as i32;
                    }
                    self.next_char();
                }
                if is_exponent_negative != 0 {
                    exponent = -exponent;
                }
                ret *= 10.0f64.powi(exponent);
            }
            let num: XmlXPathObjectPtr = xml_xpath_cache_new_float(self.context, ret);
            if num.is_null() {
                self.error = XmlXPathError::XPathMemoryError as i32;
            } else if self.add_compiled_expression(
                (*self.comp).last,
                -1,
                XmlXPathOp::XpathOpValue,
                XmlXPathObjectType::XPathNumber as i32,
                0_i32,
                0_i32,
                num as _,
                null_mut(),
            ) == -1
            {
                xml_xpath_release_object(self.context, num);
            }
        }
    }

    /// Parse a Literal and push it on the stack.
    ///
    /// `[29]   Literal ::=   '"' [^"]* '"' | "'" [^']* "'"`
    ///
    /// TODO: xmlXPathCompLiteral memory allocation could be improved.
    #[doc(alias = "xmlXPathCompLiteral")]
    unsafe fn compile_literal(&mut self) {
        unsafe {
            let ret: *mut u8;

            if self.current_char() == Some('"') {
                self.next_char();
                let q = self.cur;
                while self
                    .current_char()
                    .is_some_and(|c| xml_is_char(c as u32) && c != '"')
                {
                    self.next_char();
                }
                if self.current_char().is_none_or(|c| !xml_is_char(c as u32)) {
                    xml_xpath_err(self, XmlXPathError::XPathUnfinishedLiteralError as i32);
                    return;
                } else {
                    ret = xml_strndup(self.base[q..].as_ptr(), self.cur as i32 - q as i32);
                    self.next_char();
                }
            } else if self.current_char() == Some('\'') {
                self.next_char();
                let q = self.cur;
                while self
                    .current_char()
                    .is_some_and(|c| xml_is_char(c as u32) && c != '\'')
                {
                    self.next_char();
                }
                if self.current_char().is_none_or(|c| !xml_is_char(c as u32)) {
                    xml_xpath_err(self, XmlXPathError::XPathUnfinishedLiteralError as i32);
                    return;
                } else {
                    ret = xml_strndup(self.base[q..].as_ptr(), self.cur as i32 - q as i32);
                    self.next_char();
                }
            } else {
                xml_xpath_err(self, XmlXPathError::XPathStartLiteralError as i32);
                return;
            }
            if ret.is_null() {
                xml_xpath_perr_memory(self, None);
                return;
            }
            let lit: XmlXPathObjectPtr = xml_xpath_cache_new_string(
                self.context,
                Some(CStr::from_ptr(ret as *const i8).to_string_lossy().as_ref()),
            );
            if lit.is_null() {
                self.error = XmlXPathError::XPathMemoryError as i32;
            } else if self.add_compiled_expression(
                (*self.comp).last,
                -1,
                XmlXPathOp::XpathOpValue,
                (XmlXPathObjectType::XPathString) as i32,
                0_i32,
                0_i32,
                lit as _,
                null_mut(),
            ) == -1
            {
                xml_xpath_release_object(self.context, lit);
            }
            xml_free(ret as _);
        }
    }

    /// ```text
    /// [16]   FunctionCall ::=   FunctionName '(' ( Argument ( ',' Argument)*)? ')'
    /// [17]   Argument ::=   Expr
    /// ```
    ///
    /// Compile a function call, the evaluation of all arguments are
    /// pushed on the stack
    #[doc(alias = "xmlXPathCompFunctionCall")]
    unsafe fn compile_function_call(&mut self) {
        unsafe {
            let mut prefix: *mut u8 = null_mut();
            let mut nbargs: i32 = 0;
            let mut sort: i32 = 1;

            let name: *mut u8 = self.parse_qname(&raw mut prefix);
            if name.is_null() {
                xml_free(prefix as _);
                xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                return;
            }
            self.skip_blanks();

            if self.current_char() != Some('(') {
                xml_free(name as _);
                xml_free(prefix as _);
                xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                return;
            }
            self.next_char();
            self.skip_blanks();

            // Optimization for count(): we don't need the node-set to be sorted.
            if prefix.is_null()
                && *name.add(0) == b'c'
                && xml_str_equal(name, c"count".as_ptr() as _)
            {
                sort = 0;
            }
            (*self.comp).last = -1;
            if self.current_char() != Some(')') {
                while self.current_char().is_some() {
                    let op1: i32 = (*self.comp).last;
                    (*self.comp).last = -1;
                    self.compile_expr(sort);
                    if self.error != XmlXPathError::XPathExpressionOK as i32 {
                        xml_free(name as _);
                        xml_free(prefix as _);
                        return;
                    }
                    self.add_compiled_expression(
                        op1,
                        (*self.comp).last,
                        XmlXPathOp::XpathOpArg,
                        0,
                        0,
                        0,
                        null_mut(),
                        null_mut(),
                    );
                    nbargs += 1;
                    if self.current_char() == Some(')') {
                        break;
                    }
                    if self.current_char() != Some(',') {
                        xml_free(name as _);
                        xml_free(prefix as _);
                        xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                        return;
                    }
                    self.next_char();
                    self.skip_blanks();
                }
            }
            if self.add_compiled_expression(
                (*self.comp).last,
                -1,
                XmlXPathOp::XpathOpFunction,
                nbargs,
                0_i32,
                0_i32,
                name as _,
                prefix as _,
            ) == -1
            {
                xml_free(prefix as _);
                xml_free(name as _);
            }
            self.next_char();
            self.skip_blanks();
        }
    }

    /// ```text
    /// [8]   Predicate ::=   '[' PredicateExpr ']'
    /// [9]   PredicateExpr ::=   Expr
    /// ```
    ///
    /// Compile a predicate expression
    #[doc(alias = "xmlXPathCompPredicate")]
    unsafe fn compile_predicate(&mut self, filter: i32) {
        unsafe {
            let op1: i32 = (*self.comp).last;

            self.skip_blanks();
            if self.current_char() != Some('[') {
                xml_xpath_err(self, XmlXPathError::XPathInvalidPredicateError as i32);
                return;
            }
            self.next_char();
            self.skip_blanks();

            (*self.comp).last = -1;
            // This call to xmlXPathCompileExpr() will deactivate sorting
            // of the predicate result.
            // TODO: Sorting is still activated for filters, since I'm not
            //  sure if needed. Normally sorting should not be needed, since
            //  a filter can only diminish the number of items in a sequence,
            //  but won't change its order; so if the initial sequence is sorted,
            //  subsequent sorting is not needed.
            if filter == 0 {
                self.compile_expr(0);
            } else {
                self.compile_expr(1);
            }
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };

            if self.current_char() != Some(']') {
                xml_xpath_err(self, XmlXPathError::XPathInvalidPredicateError as i32);
                return;
            }

            if filter != 0 {
                self.add_compiled_expression(
                    op1,
                    (*self.comp).last,
                    XmlXPathOp::XpathOpFilter,
                    0,
                    0,
                    0,
                    null_mut(),
                    null_mut(),
                );
            } else {
                self.add_compiled_expression(
                    op1,
                    (*self.comp).last,
                    XmlXPathOp::XpathOpPredicate,
                    0,
                    0,
                    0,
                    null_mut(),
                    null_mut(),
                );
            }

            self.next_char();
            self.skip_blanks();
        }
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
    unsafe fn compile_variable_reference(&mut self) {
        unsafe {
            let mut prefix: *mut u8 = null_mut();

            self.skip_blanks();
            if self.current_char() != Some('$') {
                xml_xpath_err(self, XmlXPathError::XPathVariableRefError as i32);
                return;
            }
            self.next_char();
            let name: *mut u8 = self.parse_qname(&raw mut prefix);
            if name.is_null() {
                xml_free(prefix as _);
                xml_xpath_err(self, XmlXPathError::XPathVariableRefError as i32);
                return;
            }
            (*self.comp).last = -1;
            if self.add_compiled_expression(
                (*self.comp).last,
                -1,
                XmlXPathOp::XpathOpVariable,
                0_i32,
                0_i32,
                0_i32,
                name as _,
                prefix as _,
            ) == -1
            {
                xml_free(prefix as _);
                xml_free(name as _);
            }
            self.skip_blanks();
            if !self.context.is_null() && (*self.context).flags & XML_XPATH_NOVAR as i32 != 0 {
                xml_xpath_err(self, XmlXPathError::XPathForbidVariableError as i32);
            }
        }
    }

    /// Trickery: parse an XML name but without consuming the input flow
    /// Needed to avoid insanity in the parser state.
    ///
    /// ```text
    /// [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
    ///
    /// [5] Name ::= (Letter | '_' | ':') (NameChar)*
    ///
    /// [6] Names ::= Name (S Name)*
    /// ```
    ///
    /// Returns the Name parsed or NULL
    #[doc(alias = "xmlXPathScanName")]
    unsafe fn scan_name(&mut self) -> *mut u8 {
        unsafe {
            let cur = self.cur;

            let Some(c) = self.current_char() else {
                return null_mut();
            };
            if c == ' '
                || c == '>'
                || c == '/'
                || (!xml_is_letter(c as u32) && c != '_' && c != ':')
            {
                return null_mut();
            }

            while self
                .current_char()
                .filter(|&c| {
                    c != ' '
                        && c != '>'
                        && c != '/'
                        && (xml_is_letter(c as u32)
                            || xml_is_digit(c as u32)
                            || c == '.'
                            || c == '-'
                            || c == '_'
                            || c == ':'
                            || xml_is_combining(c as u32)
                            || xml_is_extender(c as u32))
                })
                .is_some()
            {
                self.next_char();
            }
            let ret: *mut u8 = xml_strndup(self.base[cur..].as_ptr(), self.cur as i32 - cur as i32);
            self.cur = cur;
            ret
        }
    }

    /// Parse an XML name
    ///
    /// ```text
    /// [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | ':' | CombiningChar | Extender
    ///
    /// [5] Name ::= (Letter | '_' | ':') (NameChar)*
    /// ```
    ///
    /// Returns the namespace name or NULL
    #[doc(alias = "xmlXPathParseName")]
    pub unsafe fn parse_name(&mut self) -> *mut u8 {
        unsafe {
            let ret: *mut u8;

            // Accelerator for simple ASCII names
            let input = self.current_str();
            if let Some(mut input) =
                input.strip_prefix(|c: char| c.is_ascii_alphabetic() || c == '_' || c == ':')
            {
                input = input.trim_start_matches(|c: char| {
                    c.is_ascii_alphanumeric() || c == '_' || c == '-' || c == ':' || c == '.'
                });
                if input.starts_with(|b| b <= '\x7F') {
                    let count = self.current_str().len() - input.len();
                    if count > XML_MAX_NAME_LENGTH {
                        self.cur += count;
                        xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                        return null_mut();
                    }
                    ret = xml_strndup(self.current_str().as_ptr(), count as i32);
                    self.cur += count;
                    return ret;
                }
            }
            self.parse_name_complex(1)
        }
    }

    #[doc(alias = "xmlXPathParseNameComplex")]
    unsafe fn parse_name_complex(&mut self, qualified: i32) -> *mut u8 {
        unsafe {
            let mut buf: [u8; XML_MAX_NAMELEN + 5] = [0; XML_MAX_NAMELEN + 5];
            let mut len: i32 = 0;

            // Handler for more complex cases
            let Some(mut c) = self.current_char() else {
                return null_mut();
            };
            if c == ' '
                || c == '>'
                || c == '/'
                || c == '['
                || c == ']'
                || c == '@'
                || c == '*'
                || (!xml_is_letter(c as u32) && c != '_' && (qualified == 0 || c != ':'))
            {
                return null_mut();
            }

            while c != ' '
                && c != '>'
                && c != '/'
                && (xml_is_letter(c as u32)
                    || xml_is_digit(c as u32)
                    || c == '.'
                    || c == '-'
                    || c == '_'
                    || (qualified != 0 && c == ':')
                    || xml_is_combining(c as u32)
                    || xml_is_extender(c as u32))
            {
                let l = c.len_utf8() as i32;
                COPY_BUF!(l, buf.as_mut_ptr(), len, c);
                self.next_char();
                c = self.current_char().unwrap_or('\0');
                if len >= XML_MAX_NAMELEN as i32 {
                    // Okay someone managed to make a huge name, so he's ready to pay
                    // for the processing speed.
                    let mut buffer: *mut u8;
                    let mut max: i32 = len * 2;

                    if len > XML_MAX_NAME_LENGTH as i32 {
                        xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                        return null_mut();
                    }
                    buffer = xml_malloc_atomic(max as usize) as *mut u8;
                    if buffer.is_null() {
                        xml_xpath_err(self, XmlXPathError::XPathMemoryError as i32);
                        return null_mut();
                    }
                    memcpy(buffer as _, buf.as_ptr() as _, len as usize);
                    while xml_is_letter(c as u32)
                        || xml_is_digit(c as u32)
                        || c == '.'
                        || c == '-'
                        || c == '_'
                        || (qualified != 0 && c == ':')
                        || xml_is_combining(c as u32)
                        || xml_is_extender(c as u32)
                    {
                        if len + 10 > max {
                            if max > XML_MAX_NAME_LENGTH as i32 {
                                xml_free(buffer as _);
                                xml_xpath_err(self, XmlXPathError::XPathExprError as i32);
                                return null_mut();
                            }
                            max *= 2;
                            let tmp: *mut u8 = xml_realloc(buffer as _, max as usize) as *mut u8;
                            if tmp.is_null() {
                                xml_free(buffer as _);
                                xml_xpath_err(self, XmlXPathError::XPathMemoryError as i32);
                                return null_mut();
                            }
                            buffer = tmp;
                        }
                        let l = c.len_utf8() as i32;
                        COPY_BUF!(l, buffer, len, c);
                        self.next_char();
                        c = self.current_char().unwrap_or('\0');
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
    }

    /// Parse an XML namespace non qualified name.
    ///
    /// ```text
    /// [NS 3] NCName ::= (Letter | '_') (NCNameChar)*
    ///
    /// [NS 4] NCNameChar ::= Letter | Digit | '.' | '-' | '_' | CombiningChar | Extender
    /// ```
    ///
    /// Returns the namespace name or NULL
    #[doc(alias = "xmlXPathParseNCName")]
    pub unsafe fn parse_ncname(&mut self) -> *mut u8 {
        unsafe {
            let ret: *mut u8;

            // Accelerator for simple ASCII names
            let input = self.current_str();
            if let Some(mut input) =
                input.strip_prefix(|c: char| c.is_ascii_alphabetic() || c == '_')
            {
                input = input.trim_start_matches(|c: char| {
                    c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '-'
                });
                if input.starts_with(|c: char| {
                    c == ' '
                        || c == '>'
                        || c == '/'
                        || c == '['
                        || c == ']'
                        || c == ':'
                        || c == '@'
                        || c == '*'
                }) {
                    let count = self.current_str().len() - input.len();
                    if count == 0 {
                        return null_mut();
                    }
                    ret = xml_strndup(self.current_str().as_ptr(), count as i32);
                    self.cur += count;
                    return ret;
                }
            }
            self.parse_name_complex(0)
        }
    }

    /// parse an XML qualified name
    ///
    /// ```text
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
    unsafe fn parse_qname(&mut self, prefix: *mut *mut u8) -> *mut u8 {
        unsafe {
            let mut ret: *mut u8;

            *prefix = null_mut();
            ret = self.parse_ncname();
            if !ret.is_null() && self.current_char() == Some(':') {
                *prefix = ret;
                self.next_char();
                ret = self.parse_ncname();
            }
            ret
        }
    }

    /// Parse a Literal
    ///
    /// ```text
    /// [29]   Literal ::=   '"' [^"]* '"' | "'" [^']* "'"
    /// ```
    ///
    /// Returns the value found or NULL in case of error
    #[doc(alias = "xmlXPathParseLiteral")]
    unsafe fn parse_literal(&mut self) -> *mut u8 {
        unsafe {
            let ret: *mut u8;

            if self.current_char() == Some('"') {
                self.next_char();
                let q = self.cur;
                while self
                    .current_char()
                    .is_some_and(|c| xml_is_char(c as u32) && c != '"')
                {
                    self.next_char();
                }
                if self.current_char().is_none_or(|c| !xml_is_char(c as u32)) {
                    xml_xpath_err(self, XmlXPathError::XPathUnfinishedLiteralError as i32);
                    return null_mut();
                } else {
                    ret = xml_strndup(self.base[q..].as_ptr(), self.cur as i32 - q as i32);
                    self.next_char();
                }
            } else if self.current_char() == Some('\'') {
                self.next_char();
                let q = self.cur;
                while self
                    .current_char()
                    .is_some_and(|c| xml_is_char(c as u32) && c != '\'')
                {
                    self.next_char();
                }
                if self.current_char().is_none_or(|c| !xml_is_char(c as u32)) {
                    xml_xpath_err(self, XmlXPathError::XPathUnfinishedLiteralError as i32);
                    return null_mut();
                } else {
                    ret = xml_strndup(self.base[q..].as_ptr(), self.cur as i32 - q as i32);
                    self.next_char();
                }
            } else {
                xml_xpath_err(self, XmlXPathError::XPathStartLiteralError as i32);
                return null_mut();
            }
            ret
        }
    }
}

/// ```text
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
fn is_axis_name(name: &str) -> Option<XmlXPathAxisVal> {
    match name {
        "ancestor" => Some(XmlXPathAxisVal::AxisAncestor),
        "ancestor-or-self" => Some(XmlXPathAxisVal::AxisAncestorOrSelf),
        "attribute" => Some(XmlXPathAxisVal::AxisAttribute),
        "child" => Some(XmlXPathAxisVal::AxisChild),
        "descendant" => Some(XmlXPathAxisVal::AxisDescendant),
        "descendant-or-self" => Some(XmlXPathAxisVal::AxisDescendantOrSelf),
        "following" => Some(XmlXPathAxisVal::AxisFollowing),
        "following-sibling" => Some(XmlXPathAxisVal::AxisFollowingSibling),
        "namespace" => Some(XmlXPathAxisVal::AxisNamespace),
        "parent" => Some(XmlXPathAxisVal::AxisParent),
        "preceding" => Some(XmlXPathAxisVal::AxisPreceding),
        "preceding-sibling" => Some(XmlXPathAxisVal::AxisPrecedingSibling),
        "self" => Some(XmlXPathAxisVal::AxisSelf),
        _ => None,
    }
}

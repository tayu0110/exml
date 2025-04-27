use std::rc::Rc;

use crate::{
    libxml::chvalid::{
        xml_is_blank_char, xml_is_char, xml_is_combining, xml_is_digit, xml_is_extender,
    },
    parser::{XML_MAX_NAME_LENGTH, xml_is_letter},
    xpath::{
        XML_XPATH_CHECKNS, XPATH_MAX_RECURSION_DEPTH, XmlXPathAxisVal, XmlXPathError, XmlXPathOp,
        xml_xpath_err,
    },
};

#[cfg(feature = "libxml_pattern")]
use super::XmlXPathCompExpr;
use super::{
    MAX_FRAC, XML_XPATH_NOVAR, XPATH_MAX_STEPS, XmlXPathContext, XmlXPathFunction, XmlXPathObject,
    XmlXPathObjectType, XmlXPathParserContext, XmlXPathTestVal, XmlXPathTypeVal,
    xml_xpath_perr_memory,
};

#[cfg(feature = "xpath")]
#[repr(C)]
#[derive(Clone)]
pub struct XmlXPathStepOp {
    pub(crate) op: XmlXPathOp, /* The identifier of the operation */
    pub(crate) ch1: i32,       /* First child */
    pub(crate) ch2: i32,       /* Second child */
    pub(crate) value: i32,
    pub(crate) value2: i32,
    pub(crate) value3: i32,
    pub(crate) value4: Option<XmlXPathStepOpValue>,
    pub(crate) value5: Option<XmlXPathStepOpValue>,
    pub(crate) cache: Option<XmlXPathFunction>,
    pub(crate) cache_uri: Option<Rc<str>>,
}

impl XmlXPathStepOp {
    /// Swaps 2 operations in the compiled expression
    #[doc(alias = "xmlXPathCompSwap")]
    pub(super) fn swap_children(&mut self) {
        (self.ch1, self.ch2) = (self.ch2, self.ch1);
    }
}

#[derive(Clone)]
pub(crate) enum XmlXPathStepOpValue {
    Object(Rc<XmlXPathObject>),
    String(Rc<str>),
}

impl XmlXPathStepOpValue {
    pub(crate) fn as_object(&self) -> Option<&XmlXPathObject> {
        match self {
            Self::Object(obj) => Some(obj.as_ref()),
            _ => None,
        }
    }

    pub(crate) fn as_str(&self) -> Option<&str> {
        match self {
            Self::String(s) => Some(s.as_ref()),
            _ => None,
        }
    }
}

impl XmlXPathParserContext<'_> {
    /// Add a step to an XPath Compiled Expression
    ///
    /// Returns -1 in case of failure, the index otherwise
    #[allow(clippy::too_many_arguments)]
    #[doc(alias = "xmlXPathCompExprAdd")]
    fn add_compiled_expression(
        &mut self,
        ch1: i32,
        ch2: i32,
        op: XmlXPathOp,
        value: i32,
        value2: i32,
        value3: i32,
        value4: Option<XmlXPathStepOpValue>,
        value5: Option<XmlXPathStepOpValue>,
    ) -> i32 {
        if self.comp.steps.len() == XPATH_MAX_STEPS {
            xml_xpath_perr_memory(Some(self), Some("adding step\n"));
            return -1;
        }
        let last = self.comp.steps.len() as i32;
        self.comp.last = last;
        self.comp.steps.push(XmlXPathStepOp {
            ch1,
            ch2,
            op,
            value,
            value2,
            value3,
            value4,
            value5,
            cache: None,
            cache_uri: None,
        });
        self.comp.steps.len() as i32 - 1
    }

    /// ```text
    /// [14]   Expr ::=   OrExpr
    /// [21]   OrExpr ::=   AndExpr | OrExpr 'or' AndExpr
    /// ```
    ///
    /// Parse and compile an expression
    #[doc(alias = "xmlXPathCompileExpr")]
    pub fn compile_expr(&mut self, sort: bool) {
        if self.context.depth >= XPATH_MAX_RECURSION_DEPTH as i32 {
            xml_xpath_err(
                Some(self),
                XmlXPathError::XPathRecursionLimitExceeded as i32,
            );
            return;
        }
        // Parsing a single '(' pushes about 10 functions on the call stack before recursing!
        self.context.depth += 10;

        self.compile_and_expr();
        if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
            return;
        };
        self.skip_blanks();
        while self.current_str().starts_with("or") {
            let op1: i32 = self.comp.last;
            self.cur += 2;
            self.skip_blanks();
            self.compile_and_expr();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            let last = self.comp.last;
            self.add_compiled_expression(op1, last, XmlXPathOp::XPathOpOr, 0, 0, 0, None, None);
            self.skip_blanks();
        }
        if sort
            && !matches!(
                self.comp.steps[self.comp.last as usize].op,
                XmlXPathOp::XPathOpValue
            )
        {
            // more ops could be optimized too
            // This is the main place to eliminate sorting for
            // operations which don't require a sorted node-set.
            // E.g. count().
            let last = self.comp.last;
            self.add_compiled_expression(last, -1, XmlXPathOp::XPathOpSort, 0, 0, 0, None, None);
        }

        self.context.depth -= 10;
    }

    /// ```text
    /// [22]   AndExpr ::=   EqualityExpr | AndExpr 'and' EqualityExpr
    /// ```
    ///
    /// Compile an AND expression.
    #[doc(alias = "xmlXPathCompAndExpr")]
    fn compile_and_expr(&mut self) {
        self.compile_equality_expr();
        if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
            return;
        };
        self.skip_blanks();
        while self.current_str().starts_with("and") {
            let op1: i32 = self.comp.last;
            self.cur += 3;
            self.skip_blanks();
            self.compile_equality_expr();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            let last = self.comp.last;
            self.add_compiled_expression(op1, last, XmlXPathOp::XPathOpAnd, 0, 0, 0, None, None);
            self.skip_blanks();
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
    fn compile_equality_expr(&mut self) {
        self.compile_relational_expr();
        if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
            return;
        };
        self.skip_blanks();
        while self.current_char() == Some('=')
            || (self.current_char() == Some('!') && self.nth_byte(1) == Some(b'='))
        {
            let op1: i32 = self.comp.last;
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
            let last = self.comp.last;
            self.add_compiled_expression(op1, last, XmlXPathOp::XPathOpEqual, eq, 0, 0, None, None);
            self.skip_blanks();
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
    fn compile_relational_expr(&mut self) {
        self.compile_additive_expr();
        if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
            return;
        };
        self.skip_blanks();
        while self.current_char() == Some('<') || self.current_char() == Some('>') {
            let op1: i32 = self.comp.last;
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
            let last = self.comp.last;
            self.add_compiled_expression(
                op1,
                last,
                XmlXPathOp::XPathOpCmp,
                inf,
                strict,
                0,
                None,
                None,
            );
            self.skip_blanks();
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
    fn compile_additive_expr(&mut self) {
        self.compile_multiplicative_expr();
        if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
            return;
        };
        self.skip_blanks();
        while self.current_char() == Some('+') || self.current_char() == Some('-') {
            let op1: i32 = self.comp.last;
            let plus = (self.current_char() == Some('+')) as i32;

            self.next_char();
            self.skip_blanks();
            self.compile_multiplicative_expr();
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            let last = self.comp.last;
            self.add_compiled_expression(
                op1,
                last,
                XmlXPathOp::XPathOpPlus,
                plus,
                0,
                0,
                None,
                None,
            );
            self.skip_blanks();
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
    fn compile_multiplicative_expr(&mut self) {
        self.compile_unary_expr();
        if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
            return;
        };
        self.skip_blanks();
        while self.current_char() == Some('*')
            || self.current_str().starts_with("div")
            || self.current_str().starts_with("mod")
        {
            let mut op: i32 = -1;
            let op1: i32 = self.comp.last;

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
            let last = self.comp.last;
            self.add_compiled_expression(op1, last, XmlXPathOp::XPathOpMult, op, 0, 0, None, None);
            self.skip_blanks();
        }
    }

    /// ```text
    /// [27]   UnaryExpr ::=   UnionExpr | '-' UnaryExpr
    /// ```
    ///
    /// Compile an unary expression.
    #[doc(alias = "xmlXPathCompUnaryExpr")]
    fn compile_unary_expr(&mut self) {
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
                let last = self.comp.last;
                self.add_compiled_expression(
                    last,
                    -1,
                    XmlXPathOp::XPathOpPlus,
                    2,
                    0,
                    0,
                    None,
                    None,
                );
            } else {
                let last = self.comp.last;
                self.add_compiled_expression(
                    last,
                    -1,
                    XmlXPathOp::XPathOpPlus,
                    3,
                    0,
                    0,
                    None,
                    None,
                );
            }
        }
    }

    /// ```text
    /// [18]   UnionExpr ::=   PathExpr | UnionExpr '|' PathExpr
    /// ```
    ///
    /// Compile an union expression.
    #[doc(alias = "xmlXPathCompUnionExpr")]
    fn compile_union_expr(&mut self) {
        self.compile_path_expr();
        if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
            return;
        };
        self.skip_blanks();
        while self.current_char() == Some('|') {
            let op1: i32 = self.comp.last;
            self.add_compiled_expression(-1, -1, XmlXPathOp::XPathOpNode, 0, 0, 0, None, None);

            self.next_char();
            self.skip_blanks();
            self.compile_path_expr();

            let last = self.comp.last;
            self.add_compiled_expression(op1, last, XmlXPathOp::XPathOpUnion, 0, 0, 0, None, None);

            self.skip_blanks();
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
    fn compile_path_expr(&mut self) {
        let mut lc: i32 = 1; /* Should we branch to LocationPath ? */

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
            if let Some(name) = self.scan_name() {
                if name.contains("::") {
                    lc = 1;
                } else {
                    let mut len = name.len();

                    while self.nth_byte(len).is_some() {
                        if self.nth_byte(len) == Some(b'/') {
                            // element name
                            lc = 1;
                            break;
                        } else if self
                            .nth_byte(len)
                            .is_some_and(|c| xml_is_blank_char(c as u32))
                        {
                            // ignore blanks
                        } else if self.nth_byte(len) == Some(b':') {
                            lc = 1;
                            break;
                        } else if self.nth_byte(len) == Some(b'(') {
                            // Node Type or Function
                            if is_node_type(name) {
                                lc = 1;
                            } else {
                                #[cfg(feature = "libxml_xptr_locs")]
                                if self.xptr != 0 && name == "range-to" {
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
                        // } else if self.nth_byte(len) == Some(b'[') {
                        //     // element name
                        //     lc = 1;
                        //     break;
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
                    if self.nth_byte(len).is_none() {
                        // element name
                        lc = 1;
                    }
                }
            } else {
                // make sure all cases are covered explicitly
                xml_xpath_err(Some(self), XmlXPathError::XPathExprError as i32);
                return;
            }
        }

        if lc != 0 {
            if self.current_char() == Some('/') {
                self.add_compiled_expression(-1, -1, XmlXPathOp::XPathOpRoot, 0, 0, 0, None, None);
            } else {
                self.add_compiled_expression(-1, -1, XmlXPathOp::XPathOpNode, 0, 0, 0, None, None);
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

                let last = self.comp.last;
                self.add_compiled_expression(
                    last,
                    -1,
                    XmlXPathOp::XPathOpCollect,
                    XmlXPathAxisVal::AxisDescendantOrSelf as i32,
                    XmlXPathTestVal::NodeTestType as i32,
                    XmlXPathTypeVal::NodeTypeNode as i32,
                    None,
                    None,
                );

                self.compile_relative_location_path();
            } else if self.current_char() == Some('/') {
                self.compile_relative_location_path();
            }
        }
        self.skip_blanks();
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
    fn compile_location_path(&mut self) {
        self.skip_blanks();
        if self.current_char() != Some('/') {
            self.compile_relative_location_path();
        } else {
            while self.current_char() == Some('/') {
                if self.current_str().starts_with("//") {
                    self.cur += 2;
                    self.skip_blanks();
                    let last = self.comp.last;
                    self.add_compiled_expression(
                        last,
                        -1,
                        XmlXPathOp::XPathOpCollect,
                        XmlXPathAxisVal::AxisDescendantOrSelf as i32,
                        XmlXPathTestVal::NodeTestType as i32,
                        XmlXPathTypeVal::NodeTypeNode as i32,
                        None,
                        None,
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

    /// ```text
    /// [3]   RelativeLocationPath ::=   Step
    ///                     | RelativeLocationPath '/' Step
    ///                     | AbbreviatedRelativeLocationPath
    /// [11]  AbbreviatedRelativeLocationPath ::=   RelativeLocationPath '//' Step
    /// ```
    ///
    /// Compile a relative location path.
    #[doc(alias = "xmlXPathCompRelativeLocationPath")]
    fn compile_relative_location_path(&mut self) {
        self.skip_blanks();
        if self.current_str().starts_with("//") {
            self.cur += 2;
            self.skip_blanks();
            let last = self.comp.last;
            self.add_compiled_expression(
                last,
                -1,
                XmlXPathOp::XPathOpCollect,
                XmlXPathAxisVal::AxisDescendantOrSelf as i32,
                XmlXPathTestVal::NodeTestType as i32,
                XmlXPathTypeVal::NodeTypeNode as i32,
                None,
                None,
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
            if self.current_str().starts_with("//") {
                self.cur += 2;
                self.skip_blanks();
                let last = self.comp.last;
                self.add_compiled_expression(
                    last,
                    -1,
                    XmlXPathOp::XPathOpCollect,
                    XmlXPathAxisVal::AxisDescendantOrSelf as i32,
                    XmlXPathTestVal::NodeTestType as i32,
                    XmlXPathTypeVal::NodeTypeNode as i32,
                    None,
                    None,
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
    fn compile_step(&mut self) {
        #[cfg(feature = "libxml_xptr_locs")]
        let mut rangeto: i32 = 0;
        #[cfg(feature = "libxml_xptr_locs")]
        let mut op2: i32 = -1;

        self.skip_blanks();
        if self.current_str().starts_with("..") {
            self.cur += 2;
            self.skip_blanks();
            let last = self.comp.last;
            self.add_compiled_expression(
                last,
                -1,
                XmlXPathOp::XPathOpCollect,
                XmlXPathAxisVal::AxisParent as i32,
                XmlXPathTestVal::NodeTestType as i32,
                XmlXPathTypeVal::NodeTypeNode as i32,
                None,
                None,
            );
        } else if self.current_char() == Some('.') {
            self.next_char();
            self.skip_blanks();
        } else {
            let mut name = None;
            let mut prefix = None;
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
                    if name.as_deref() == Some("range-to") {
                        op2 = self.comp.last;
                        self.skip_blanks();
                        if self.current_char() != Some('(') {
                            xml_xpath_err(Some(self), XmlXPathError::XPathExprError as i32);
                            return;
                        }
                        self.next_char();
                        self.skip_blanks();

                        self.compile_expr(true);
                        /* PUSH_BINARY_EXPR(XPATH_OP_RANGETO, op2, self.comp.borrow().last, 0, 0); */
                        if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                            return;
                        };

                        self.skip_blanks();
                        if self.current_char() != Some(')') {
                            xml_xpath_err(Some(self), XmlXPathError::XPathExprError as i32);
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
                    if name.is_none() {
                        name = self.parse_ncname();
                    }
                    if name.is_some() {
                        axis = is_axis_name(name.as_deref().unwrap());
                        if axis.is_some() {
                            self.skip_blanks();
                            if self.current_char() == Some(':') && self.nth_byte(1) == Some(b':') {
                                self.cur += 2;
                                name = None;
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
                    return;
                }

                match self.compile_node_test(&mut test, &mut typ, name.as_deref()) {
                    Some((pref, local)) => {
                        prefix = pref;
                        name = Some(local);
                    }
                    None => {
                        prefix = None;
                        name = None;
                    }
                }
                if matches!(test, XmlXPathTestVal::NodeTestNone) {
                    return;
                }

                if let Some(prefix) = prefix.as_deref() {
                    if self.context.flags & XML_XPATH_CHECKNS as i32 != 0
                        && self.context.lookup_ns(prefix).is_none()
                    {
                        xml_xpath_err(Some(self), XmlXPathError::XPathUndefPrefixError as i32);
                    }
                }
            }

            let op1: i32 = self.comp.last;
            self.comp.last = -1;

            self.skip_blanks();
            while self.current_char() == Some('[') {
                self.compile_predicate(false);
            }

            #[cfg(feature = "libxml_xptr_locs")]
            if rangeto != 0 {
                self.add_compiled_expression(
                    op2,
                    op1,
                    XmlXPathOp::XPathOpRangeto,
                    0,
                    0,
                    0,
                    None,
                    None,
                );
                return;
            }

            let last = self.comp.last;
            self.add_compiled_expression(
                op1,
                last,
                XmlXPathOp::XPathOpCollect,
                (axis.unwrap_or_else(|| {
                    panic!(
                        "Invalid xmlXPathAxisVal: file: {}, line: {}",
                        file!(),
                        line!()
                    )
                })) as i32,
                test as i32,
                typ as i32,
                prefix.map(Rc::from).map(XmlXPathStepOpValue::String),
                name.map(Rc::from).map(XmlXPathStepOpValue::String),
            );
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
    fn compile_node_test(
        &mut self,
        test: &mut XmlXPathTestVal,
        typ: &mut XmlXPathTypeVal,
        name: Option<&str>,
    ) -> Option<(Option<String>, String)> {
        *typ = XmlXPathTypeVal::NodeTypeNode;
        *test = XmlXPathTestVal::NodeTestNone;
        self.skip_blanks();

        if name.is_none() && self.current_char() == Some('*') {
            // All elements
            self.next_char();
            *test = XmlXPathTestVal::NodeTestAll;
            return None;
        }

        let Some(mut name) = name
            .map(|name| name.to_owned())
            .or_else(|| self.parse_ncname())
        else {
            xml_xpath_err(Some(self), XmlXPathError::XPathExprError as i32);
            return None;
        };

        let blanks: i32 = self
            .current_char()
            .is_some_and(|c| xml_is_blank_char(c as u32)) as i32;
        self.skip_blanks();
        if self.current_char() == Some('(') {
            self.next_char();
            // NodeType or PI search
            if name == "comment" {
                *typ = XmlXPathTypeVal::NodeTypeComment;
            } else if name == "node" {
                *typ = XmlXPathTypeVal::NodeTypeNode;
            } else if name == "processing-instruction" {
                *typ = XmlXPathTypeVal::NodeTypePI;
            } else if name == "text" {
                *typ = XmlXPathTypeVal::NodeTypeText;
            } else {
                xml_xpath_err(Some(self), XmlXPathError::XPathExprError as i32);
                return None;
            }

            *test = XmlXPathTestVal::NodeTestType;

            self.skip_blanks();
            let mut name = Some(name);
            if matches!(*typ, XmlXPathTypeVal::NodeTypePI) {
                // Specific case: search a PI by name.
                name = None;
                if self.current_char() != Some(')') {
                    let Some(lit) = self.parse_literal() else {
                        xml_xpath_err(Some(self), XmlXPathError::XPathExprError as i32);
                        return None;
                    };
                    name = Some(lit);
                    *test = XmlXPathTestVal::NodeTestPI;
                    self.skip_blanks();
                }
            }
            if self.current_char() != Some(')') {
                xml_xpath_err(Some(self), XmlXPathError::XPathUnclosedError as i32);
                return None;
            }
            self.next_char();
            return name.map(|name| (None, name));
        }
        *test = XmlXPathTestVal::NodeTestName;
        let mut prefix = None;
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
            prefix = Some(name);
            // #endif

            if self.current_char() == Some('*') {
                // All elements
                self.next_char();
                *test = XmlXPathTestVal::NodeTestAll;
                return None;
            }

            let Some(ncname) = self.parse_ncname() else {
                xml_xpath_err(Some(self), XmlXPathError::XPathExprError as i32);
                return None;
            };
            name = ncname;
        }
        Some((prefix, name))
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
    fn compile_filter_expr(&mut self) {
        self.compile_primary_expr();
        if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
            return;
        };
        self.skip_blanks();

        while self.current_char() == Some('[') {
            self.compile_predicate(true);
            self.skip_blanks();
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
    fn compile_primary_expr(&mut self) {
        self.skip_blanks();
        if self.current_char() == Some('$') {
            self.compile_variable_reference();
        } else if self.current_char() == Some('(') {
            self.next_char();
            self.skip_blanks();
            self.compile_expr(true);
            if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
                return;
            };
            if self.current_char() != Some(')') {
                xml_xpath_err(Some(self), XmlXPathError::XPathExprError as i32);
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

    /// ```text
    /// [30]   Number ::=   Digits ('.' Digits?)?
    ///                   | '.' Digits
    /// [31]   Digits ::=   [0-9]+
    /// ```
    ///
    /// Compile a Number, then push it on the stack
    #[doc(alias = "xmlXPathCompNumber")]
    fn compile_number(&mut self) {
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
            xml_xpath_err(Some(self), XmlXPathError::XPathNumberError as i32);
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
                xml_xpath_err(Some(self), XmlXPathError::XPathNumberError as i32);
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
        let last = self.comp.last;
        self.add_compiled_expression(
            last,
            -1,
            XmlXPathOp::XPathOpValue,
            XmlXPathObjectType::XPathNumber as i32,
            0_i32,
            0_i32,
            Some(XmlXPathStepOpValue::Object(Rc::new(XmlXPathObject::from(
                ret,
            )))),
            None,
        );
    }

    /// Parse a Literal and push it on the stack.
    ///
    /// `[29]   Literal ::=   '"' [^"]* '"' | "'" [^']* "'"`
    ///
    /// TODO: xmlXPathCompLiteral memory allocation could be improved.
    #[doc(alias = "xmlXPathCompLiteral")]
    fn compile_literal(&mut self) {
        let ret;

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
                xml_xpath_err(
                    Some(self),
                    XmlXPathError::XPathUnfinishedLiteralError as i32,
                );
                return;
            } else {
                ret = self.base[q..self.cur].to_owned();
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
                xml_xpath_err(
                    Some(self),
                    XmlXPathError::XPathUnfinishedLiteralError as i32,
                );
                return;
            } else {
                ret = self.base[q..self.cur].to_owned();
                self.next_char();
            }
        } else {
            xml_xpath_err(Some(self), XmlXPathError::XPathStartLiteralError as i32);
            return;
        }
        let last = self.comp.last;
        self.add_compiled_expression(
            last,
            -1,
            XmlXPathOp::XPathOpValue,
            XmlXPathObjectType::XPathString as i32,
            0_i32,
            0_i32,
            Some(XmlXPathStepOpValue::Object(Rc::new(XmlXPathObject::from(
                ret,
            )))),
            None,
        );
    }

    /// ```text
    /// [16]   FunctionCall ::=   FunctionName '(' ( Argument ( ',' Argument)*)? ')'
    /// [17]   Argument ::=   Expr
    /// ```
    ///
    /// Compile a function call, the evaluation of all arguments are
    /// pushed on the stack
    #[doc(alias = "xmlXPathCompFunctionCall")]
    fn compile_function_call(&mut self) {
        let mut nbargs: i32 = 0;
        let mut sort = true;

        let Some((prefix, name)) = self.parse_qname() else {
            xml_xpath_err(Some(self), XmlXPathError::XPathExprError as i32);
            return;
        };
        self.skip_blanks();

        if self.current_char() != Some('(') {
            xml_xpath_err(Some(self), XmlXPathError::XPathExprError as i32);
            return;
        }
        self.next_char();
        self.skip_blanks();

        // Optimization for count(): we don't need the node-set to be sorted.
        if prefix.is_none() && name.starts_with('c') && name == "count" {
            sort = false;
        }
        self.comp.last = -1;
        if self.current_char() != Some(')') {
            while self.current_char().is_some() {
                let op1: i32 = self.comp.last;
                self.comp.last = -1;
                self.compile_expr(sort);
                if self.error != XmlXPathError::XPathExpressionOK as i32 {
                    return;
                }
                let last = self.comp.last;
                self.add_compiled_expression(
                    op1,
                    last,
                    XmlXPathOp::XPathOpArg,
                    0,
                    0,
                    0,
                    None,
                    None,
                );
                nbargs += 1;
                if self.current_char() == Some(')') {
                    break;
                }
                if self.current_char() != Some(',') {
                    xml_xpath_err(Some(self), XmlXPathError::XPathExprError as i32);
                    return;
                }
                self.next_char();
                self.skip_blanks();
            }
        }
        let last = self.comp.last;
        self.add_compiled_expression(
            last,
            -1,
            XmlXPathOp::XPathOpFunction,
            nbargs,
            0_i32,
            0_i32,
            Some(XmlXPathStepOpValue::String(name.into())),
            prefix.map(Rc::from).map(XmlXPathStepOpValue::String),
        );
        self.next_char();
        self.skip_blanks();
    }

    /// ```text
    /// [8]   Predicate ::=   '[' PredicateExpr ']'
    /// [9]   PredicateExpr ::=   Expr
    /// ```
    ///
    /// Compile a predicate expression
    #[doc(alias = "xmlXPathCompPredicate")]
    fn compile_predicate(&mut self, filter: bool) {
        let op1: i32 = self.comp.last;

        self.skip_blanks();
        if self.current_char() != Some('[') {
            xml_xpath_err(Some(self), XmlXPathError::XPathInvalidPredicateError as i32);
            return;
        }
        self.next_char();
        self.skip_blanks();

        self.comp.last = -1;
        // This call to xmlXPathCompileExpr() will deactivate sorting
        // of the predicate result.
        // TODO: Sorting is still activated for filters, since I'm not
        //  sure if needed. Normally sorting should not be needed, since
        //  a filter can only diminish the number of items in a sequence,
        //  but won't change its order; so if the initial sequence is sorted,
        //  subsequent sorting is not needed.
        if !filter {
            self.compile_expr(false);
        } else {
            self.compile_expr(true);
        }
        if self.error != crate::xpath::XmlXPathError::XPathExpressionOK as i32 {
            return;
        };

        if self.current_char() != Some(']') {
            xml_xpath_err(Some(self), XmlXPathError::XPathInvalidPredicateError as i32);
            return;
        }

        if filter {
            let last = self.comp.last;
            self.add_compiled_expression(op1, last, XmlXPathOp::XPathOpFilter, 0, 0, 0, None, None);
        } else {
            let last = self.comp.last;
            self.add_compiled_expression(
                op1,
                last,
                XmlXPathOp::XPathOpPredicate,
                0,
                0,
                0,
                None,
                None,
            );
        }

        self.next_char();
        self.skip_blanks();
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
    /// ```text
    /// [36]   VariableReference ::=   '$' QName
    /// ```
    #[doc(alias = "xmlXPathCompVariableReference")]
    fn compile_variable_reference(&mut self) {
        self.skip_blanks();
        if self.current_char() != Some('$') {
            xml_xpath_err(Some(self), XmlXPathError::XPathVariableRefError as i32);
            return;
        }
        self.next_char();
        let Some((prefix, name)) = self.parse_qname() else {
            xml_xpath_err(Some(self), XmlXPathError::XPathVariableRefError as i32);
            return;
        };
        self.comp.last = -1;
        let last = self.comp.last;
        self.add_compiled_expression(
            last,
            -1,
            XmlXPathOp::XPathOpVariable,
            0_i32,
            0_i32,
            0_i32,
            Some(XmlXPathStepOpValue::String(name.into())),
            prefix.map(Rc::from).map(XmlXPathStepOpValue::String),
        );
        self.skip_blanks();
        if self.context.flags & XML_XPATH_NOVAR as i32 != 0 {
            xml_xpath_err(Some(self), XmlXPathError::XPathForbidVariableError as i32);
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
    fn scan_name(&self) -> Option<&str> {
        let expr = self.current_str();
        if !expr.starts_with(|c: char| {
            c != ' ' && c != '>' && c != '/' && (xml_is_letter(c as u32) || c == '_' || c == ':')
        }) {
            return None;
        }
        expr.split_once(|c: char| {
            c == ' '
                || c == '>'
                || c == '/'
                || (!xml_is_letter(c as u32)
                    && !xml_is_digit(c as u32)
                    && c != '.'
                    && c != '-'
                    && c != '_'
                    && c != ':'
                    && !xml_is_combining(c as u32)
                    && !xml_is_extender(c as u32))
        })
        .map(|pair| pair.0)
        .or(Some(expr))
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
    pub fn parse_name(&mut self) -> Option<String> {
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
                    xml_xpath_err(Some(self), XmlXPathError::XPathExprError as i32);
                    return None;
                }
                let ret = self.current_str()[..count].to_owned();
                self.cur += count;
                return Some(ret);
            }
        }
        self.parse_name_complex(true)
    }

    #[doc(alias = "xmlXPathParseNameComplex")]
    fn parse_name_complex(&mut self, qualified: bool) -> Option<String> {
        // Handler for more complex cases
        let mut c = self.current_char()?;
        if c == ' '
            || c == '>'
            || c == '/'
            || c == '['
            || c == ']'
            || c == '@'
            || c == '*'
            || (!xml_is_letter(c as u32) && c != '_' && (!qualified || c != ':'))
        {
            return None;
        }

        let mut buf = String::with_capacity(XML_MAX_NAME_LENGTH);
        while c != ' '
            && c != '>'
            && c != '/'
            && (xml_is_letter(c as u32)
                || xml_is_digit(c as u32)
                || c == '.'
                || c == '-'
                || c == '_'
                || (qualified && c == ':')
                || xml_is_combining(c as u32)
                || xml_is_extender(c as u32))
        {
            buf.push(c);
            self.next_char();
            c = self.current_char().unwrap_or('\0');
        }
        (!buf.is_empty()).then_some(buf)
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
    pub fn parse_ncname(&mut self) -> Option<String> {
        // Accelerator for simple ASCII names
        let input = self.current_str();
        if let Some(mut input) = input.strip_prefix(|c: char| c.is_ascii_alphabetic() || c == '_') {
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
                    return None;
                }
                let ret = self.current_str()[..count].to_owned();
                self.cur += count;
                return Some(ret);
            }
        }
        self.parse_name_complex(false)
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
    fn parse_qname(&mut self) -> Option<(Option<String>, String)> {
        let ret = self.parse_ncname()?;
        if self.current_char() == Some(':') {
            self.next_char();
            Some((Some(ret), self.parse_ncname()?))
        } else {
            Some((None, ret))
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
    fn parse_literal(&mut self) -> Option<String> {
        let expr = self.current_str();
        let Some(sep) = expr.chars().next().filter(|&sep| sep == '"' || sep == '\'') else {
            xml_xpath_err(Some(self), XmlXPathError::XPathStartLiteralError as i32);
            return None;
        };
        let Some((lit, _)) = expr.split_once(|c: char| !xml_is_char(c as u32) || c == sep) else {
            xml_xpath_err(
                Some(self),
                XmlXPathError::XPathUnfinishedLiteralError as i32,
            );
            return None;
        };
        let expr = &expr[1 + lit.len()..];
        if !expr.starts_with(sep) {
            xml_xpath_err(
                Some(self),
                XmlXPathError::XPathUnfinishedLiteralError as i32,
            );
            None
        } else {
            let lit = lit.to_owned();
            self.cur += 2 + lit.len();
            Some(lit)
        }
    }
}

impl XmlXPathContext {
    /// Try to compile the XPath expression as a streamable subset.
    ///
    /// Returns the compiled expression or NULL if failed to compile.
    #[doc(alias = "xmlXPathTryStreamCompile")]
    #[cfg(feature = "libxml_pattern")]
    pub fn try_stream_compile(&mut self, xpath: &str) -> Option<XmlXPathCompExpr> {
        use crate::pattern::{XmlPatternFlags, xml_pattern_compile};

        // Optimization: use streaming patterns when the XPath expression can
        // be compiled to a stream lookup

        if !xpath.contains(['[', '(', '@']) {
            // We don't try to handle expressions using the verbose axis
            // specifiers ("::"), just the simplified form at this point.
            // Additionally, if there is no list of namespaces available and
            //  there's a ":" in the expression, indicating a prefixed QName,
            //  then we won't try to compile either. xmlPatterncompile() needs
            //  to have a list of namespaces at compilation time in order to
            //  compile prefixed name tests.
            if let Some((_, tmp)) = xpath.split_once(':') {
                if self.namespaces.as_ref().map_or(0, |t| t.len()) == 0 || tmp.starts_with(':') {
                    return None;
                }
            }

            let mut namespaces = None;
            if let Some(table) = self.namespaces.as_deref().filter(|t| !t.is_empty()) {
                namespaces = Some(
                    table
                        .iter()
                        .map(|ns| {
                            (
                                ns.href().unwrap().into_owned(),
                                ns.prefix().map(|pref| pref.into_owned()),
                            )
                        })
                        .collect(),
                );
            }

            if let Some(stream) =
                xml_pattern_compile(xpath, XmlPatternFlags::XmlPatternXPath as i32, namespaces)
            {
                if stream.is_streamable() == 1 {
                    return Some(XmlXPathCompExpr {
                        stream: Some(Rc::new(*stream)),
                        ..Default::default()
                    });
                }
            }
        }
        None
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

/// Is the name given a NodeType one.
///
/// ```text
/// [38]   NodeType ::=   'comment'
///                   | 'text'
///                   | 'processing-instruction'
///                   | 'node'
/// ```
///
/// Returns 1 if true 0 otherwise
#[doc(alias = "xmlXPathIsNodeType")]
pub fn is_node_type(name: &str) -> bool {
    matches!(name, "node" | "text" | "comment" | "processing-instruction")
}

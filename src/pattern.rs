//! Provide methods and data structures for handling pattern expression.  
//!
//! This module is based on `libxml/pattern.h`, `pattern.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: pattern expression handling
// Description: allows to compile and test pattern expressions for nodes
//              either in a tree or based on a parser state.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// pattern.c: Implementation of selectors for nodes
//
// Reference:
//   http://www.w3.org/TR/2001/REC-xmlschema-1-20010502/
//   to some extent
//   http://www.w3.org/TR/1999/REC-xml-19991116
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{os::raw::c_void, ptr::null_mut, rc::Rc};

use crate::tree::{NodeCommon, XmlAttrPtr};
use crate::{
    libxml::{
        chvalid::{xml_is_blank_char, xml_is_combining, xml_is_digit, xml_is_extender},
        parser_internals::xml_is_letter,
    },
    tree::{XML_XML_NAMESPACE, XmlElementType, XmlGenericNodePtr, XmlNodePtr},
};

const XML_STREAM_STEP_DESC: usize = 1;
const XML_STREAM_STEP_FINAL: usize = 2;
const XML_STREAM_STEP_ROOT: usize = 4;
const XML_STREAM_STEP_ATTR: usize = 8;
const XML_STREAM_STEP_NODE: usize = 16;
const XML_STREAM_STEP_IN_SET: usize = 32;

// NOTE: Those private flags (XML_STREAM_xxx) are used
//   in xmlStreamCtxt->flag. They extend the public
//   XmlPatternFlags, so be careful not to interfere with the
//   reserved values for XmlPatternFlags.
const XML_STREAM_FINAL_IS_ANY_NODE: usize = 1 << 14;
const XML_STREAM_FROM_ROOT: usize = 1 << 15;
const XML_STREAM_DESC: usize = 1 << 16;

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlPatOp {
    XmlOpEnd = 0,
    XmlOpRoot,
    XmlOpElem,
    XmlOpChild,
    XmlOpAttr,
    XmlOpParent,
    XmlOpAncestor,
    XmlOpNs,
    XmlOpAll,
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct XmlStepOp {
    op: XmlPatOp,
    value: Option<Rc<str>>,
    value2: Option<Rc<str>>, /* The namespace name */
}

#[repr(C)]
#[derive(Debug, Clone)]
pub struct XmlStreamStep {
    flags: i32,            /* properties of that step */
    name: Option<Rc<str>>, /* first string value if NULL accept all */
    ns: Option<Rc<str>>,   /* second string value */
    node_type: i32,        /* type of node */
}

#[repr(C)]
#[derive(Default)]
pub struct XmlStreamComp {
    steps: Vec<XmlStreamStep>, /* the array of steps */
    flags: i32,
}

impl XmlStreamComp {
    /// Build a new compiled pattern for streaming
    ///
    /// Returns the new structure or NULL in case of error.
    #[doc(alias = "xmlNewStreamComp")]
    fn new(size: usize) -> Self {
        let mut res = Self::default();
        res.steps.reserve(size.min(4));
        res
    }

    /// Add a new step to the compiled pattern
    ///
    /// Returns -1 in case of error or the step index if successful
    #[doc(alias = "xmlStreamCompAddStep")]
    fn add_step(
        &mut self,
        name: Option<Rc<str>>,
        ns: Option<Rc<str>>,
        node_type: i32,
        flags: i32,
    ) -> i32 {
        self.steps.push(XmlStreamStep {
            flags,
            name,
            ns,
            node_type,
        });
        self.steps.len() as i32 - 1
    }
}

/// A compiled (XPath based) pattern to select nodes
#[doc(alias = "xmlPattern")]
#[repr(C)]
pub struct XmlPattern {
    data: *mut c_void,                 /* the associated template */
    next: Option<Box<XmlPattern>>,     /* next pattern if | is used */
    pattern: Option<Rc<str>>,          /* the pattern */
    flags: i32,                        /* flags */
    steps: Vec<XmlStepOp>,             /* ops for computation */
    stream: Option<Rc<XmlStreamComp>>, /* the streaming data if any */
}

impl XmlPattern {
    /// Create a new XSLT Pattern
    ///
    /// Returns the newly allocated xmlPatternPtr or NULL in case of error
    #[doc(alias = "xmlNewPattern")]
    fn new() -> Self {
        Self::default()
    }

    #[doc(alias = "XML_STREAM_XS_IDC")]
    fn is_xs_idc(&self) -> bool {
        self.flags
            & (XmlPatternFlags::XmlPatternXSSel as i32 | XmlPatternFlags::XmlPatternXSField as i32)
            != 0
    }

    #[doc(alias = "XML_STREAM_XS_IDC_SEL")]
    fn is_xs_idc_sel(&self) -> bool {
        self.flags & XmlPatternFlags::XmlPatternXSSel as i32 != 0
    }

    /// Tries to stream compile a pattern
    ///
    /// Returns -1 in case of failure and 0 in case of success.
    #[doc(alias = "xmlStreamCompile")]
    fn stream_compile(&mut self) -> i32 {
        let mut s: i32 = 0;
        let mut root: i32 = 0;
        let mut flags: i32 = 0;
        let mut prevs: i32 = -1;

        if self.steps.is_empty() {
            return -1;
        }
        // special case for .
        if self.steps.len() == 1
            && matches!(self.steps[0].op, XmlPatOp::XmlOpElem)
            && self.steps[0].value.is_none()
            && self.steps[0].value2.is_none()
        {
            let mut stream = XmlStreamComp::new(0);
            // Note that the stream will have no steps in this case.
            stream.flags |= XML_STREAM_FINAL_IS_ANY_NODE as i32;
            self.stream = Some(Rc::new(stream));
            return 0;
        }

        let mut stream = XmlStreamComp::new((self.steps.len() / 2) + 1);

        if self.flags & PAT_FROM_ROOT as i32 != 0 {
            stream.flags |= XML_STREAM_FROM_ROOT as i32;
        }

        'main: for i in 0..self.steps.len() {
            let step = &self.steps[i];
            match step.op {
                XmlPatOp::XmlOpEnd => {}
                XmlPatOp::XmlOpRoot => {
                    if i != 0 {
                        return 0;
                    }
                    root = 1;
                }
                XmlPatOp::XmlOpNs => {
                    s = stream.add_step(
                        None,
                        step.value.clone(),
                        XmlElementType::XmlElementNode as i32,
                        flags,
                    );
                    if s < 0 {
                        return 0;
                    }
                    prevs = s;
                    flags = 0;
                }
                XmlPatOp::XmlOpAttr => {
                    flags |= XML_STREAM_STEP_ATTR as i32;
                    prevs = -1;
                    s = stream.add_step(
                        step.value.clone(),
                        step.value2.clone(),
                        XmlElementType::XmlAttributeNode as i32,
                        flags,
                    );
                    flags = 0;
                    if s < 0 {
                        return 0;
                    }
                }
                XmlPatOp::XmlOpElem => 'to_break: {
                    if step.value.is_none() && step.value2.is_none() {
                        // We have a "." or "self::node()" here.
                        // Eliminate redundant self::node() tests like in "/./."
                        // or "//./"
                        // The only case we won't eliminate is "//.", i.e. if
                        // self::node() is the last node test and we had
                        // continuation somewhere beforehand.
                        if self.steps.len() == i + 1 && flags & XML_STREAM_STEP_DESC as i32 != 0 {
                            // Mark the special case where the expression resolves
                            // to any type of node.
                            if self.steps.len() == i + 1 {
                                stream.flags |= XML_STREAM_FINAL_IS_ANY_NODE as i32;
                            }
                            flags |= XML_STREAM_STEP_NODE as i32;
                            s = stream.add_step(None, None, XML_STREAM_ANY_NODE as i32, flags);
                            if s < 0 {
                                return 0;
                            }
                            flags = 0;
                            // If there was a previous step, mark it to be added to
                            // the result node-set; this is needed since only
                            // the last step will be marked as "is_final" and only
                            // "is_final" nodes are added to the resulting set.
                            if prevs != -1 {
                                stream.steps[prevs as usize].flags |= XML_STREAM_STEP_IN_SET as i32;
                                prevs = -1;
                            }
                            break 'to_break;
                        } else {
                            // Just skip this one.
                            continue 'main;
                        }
                    }
                    // An element node.
                    s = stream.add_step(
                        step.value.clone(),
                        step.value2.clone(),
                        XmlElementType::XmlElementNode as i32,
                        flags,
                    );
                    if s < 0 {
                        return 0;
                    }
                    prevs = s;
                    flags = 0;
                }
                XmlPatOp::XmlOpChild => {
                    // An element node child.
                    s = stream.add_step(
                        step.value.clone(),
                        step.value2.clone(),
                        XmlElementType::XmlElementNode as i32,
                        flags,
                    );
                    if s < 0 {
                        return 0;
                    }
                    prevs = s;
                    flags = 0;
                }
                XmlPatOp::XmlOpAll => {
                    s = stream.add_step(None, None, XmlElementType::XmlElementNode as i32, flags);
                    if s < 0 {
                        return 0;
                    }
                    prevs = s;
                    flags = 0;
                }
                XmlPatOp::XmlOpParent => {}
                XmlPatOp::XmlOpAncestor => {
                    // Skip redundant continuations.
                    if flags & XML_STREAM_STEP_DESC as i32 != 0 {
                        // break;
                    } else {
                        flags |= XML_STREAM_STEP_DESC as i32;
                        // Mark the expression as having "//".
                        if stream.flags & XML_STREAM_DESC as i32 == 0 {
                            stream.flags |= XML_STREAM_DESC as i32;
                        }
                    }
                }
            }
        }
        if root == 0 && self.flags & XML_PATTERN_NOTPATTERN == 0 {
            // If this should behave like a real pattern, we will mark
            // the first step as having "//", to be reentrant on every
            // tree level.
            if stream.flags & XML_STREAM_DESC as i32 == 0 {
                stream.flags |= XML_STREAM_DESC as i32;
            }

            if !stream.steps.is_empty() && stream.steps[0].flags & XML_STREAM_STEP_DESC as i32 == 0
            {
                stream.steps[0].flags |= XML_STREAM_STEP_DESC as i32;
            }
        }
        if stream.steps.len() as i32 <= s {
            return 0;
        }
        stream.steps[s as usize].flags |= XML_STREAM_STEP_FINAL as i32;
        if root != 0 {
            stream.steps[0].flags |= XML_STREAM_STEP_ROOT as i32;
        }
        self.stream = Some(Rc::new(stream));
        0
    }

    /// Reverse all the stack of expressions
    ///
    /// Returns 0 in case of success and -1 in case of error.
    #[doc(alias = "xmlReversePattern")]
    fn reverse_pattern(&mut self) -> i32 {
        // remove the leading // for //a or .//a
        if !self.steps.is_empty() && matches!(self.steps[0].op, XmlPatOp::XmlOpAncestor) {
            self.steps.remove(0);
        }
        self.steps.reverse();
        self.steps.push(XmlStepOp {
            op: XmlPatOp::XmlOpEnd,
            value: None,
            value2: None,
        });
        0
    }

    /// Test whether the node matches the pattern
    ///
    /// Returns 1 if it matches, 0 if it doesn't and -1 in case of failure
    #[doc(alias = "xmlPatMatch")]
    fn pattern_match_internal(&self, mut node: XmlGenericNodePtr) -> i32 {
        let mut states: XmlStepStates = XmlStepStates { states: vec![] };
        // // may require backtrack

        let mut i = 0;
        // restart:
        loop {
            'rollback: {
                while i < self.steps.len() {
                    let mut step = &self.steps[i];
                    match step.op {
                        XmlPatOp::XmlOpEnd => {
                            return 1;
                        }
                        XmlPatOp::XmlOpRoot => {
                            if matches!(node.element_type(), XmlElementType::XmlNamespaceDecl) {
                                break 'rollback;
                            }
                            node = node.parent().unwrap();
                            if matches!(
                                node.element_type(),
                                XmlElementType::XmlDocumentNode
                                    | XmlElementType::XmlHTMLDocumentNode
                            ) {
                                i += 1;
                                continue;
                            }
                            break 'rollback;
                        }
                        XmlPatOp::XmlOpElem => {
                            if !matches!(node.element_type(), XmlElementType::XmlElementNode) {
                                break 'rollback;
                            }
                            let node = XmlNodePtr::try_from(node).unwrap();
                            let Some(value) = step.value.as_deref() else {
                                i += 1;
                                continue;
                            };
                            if Some(value) != node.name().as_deref() {
                                break 'rollback;
                            }

                            // Namespace test
                            if let Some(ns) = node.ns {
                                if ns.href.is_some()
                                    && step.value2.as_deref() != ns.href().as_deref()
                                {
                                    break 'rollback;
                                }
                            } else if step.value2.is_some() {
                                break 'rollback;
                            }
                            i += 1;
                            continue;
                        }
                        XmlPatOp::XmlOpChild => {
                            if !matches!(
                                node.element_type(),
                                XmlElementType::XmlElementNode
                                    | XmlElementType::XmlDocumentNode
                                    | XmlElementType::XmlHTMLDocumentNode
                            ) {
                                break 'rollback;
                            }

                            let mut lst = node.children();

                            if let Some(value) = step.value.as_deref() {
                                while let Some(now) = lst {
                                    if matches!(now.element_type(), XmlElementType::XmlElementNode)
                                    {
                                        let now = XmlNodePtr::try_from(now).unwrap();
                                        if Some(value) == now.name().as_deref() {
                                            break;
                                        }
                                    }
                                    lst = now.next();
                                }
                                if lst.is_some() {
                                    i += 1;
                                    continue;
                                }
                            }
                            break 'rollback;
                        }
                        XmlPatOp::XmlOpAttr => {
                            if !matches!(node.element_type(), XmlElementType::XmlAttributeNode) {
                                break 'rollback;
                            }
                            let node = XmlAttrPtr::try_from(node).unwrap();
                            if let Some(value) = step.value.as_deref() {
                                if Some(value) != node.name().as_deref() {
                                    break 'rollback;
                                }
                            }
                            // Namespace test
                            if let Some(ns) = node.ns {
                                if step.value2.is_some()
                                    && step.value2.as_deref() != ns.href().as_deref()
                                {
                                    break 'rollback;
                                }
                            } else if step.value2.is_some() {
                                break 'rollback;
                            }
                            i += 1;
                            continue;
                        }
                        XmlPatOp::XmlOpParent => {
                            if matches!(
                                node.element_type(),
                                XmlElementType::XmlDocumentNode
                                    | XmlElementType::XmlHTMLDocumentNode
                                    | XmlElementType::XmlNamespaceDecl
                            ) {
                                break 'rollback;
                            }
                            let Some(parent) = node.parent() else {
                                break 'rollback;
                            };
                            node = parent;
                            let Some(value) = step.value.as_deref() else {
                                i += 1;
                                continue;
                            };
                            // Is is correct ??????
                            let node = XmlNodePtr::try_from(node).unwrap();
                            if Some(value) != node.name().as_deref() {
                                break 'rollback;
                            }
                            // Namespace test
                            if let Some(ns) = node.ns {
                                if ns.href.is_some()
                                    && step.value2.as_deref() != ns.href().as_deref()
                                {
                                    break 'rollback;
                                }
                            } else if step.value2.is_some() {
                                break 'rollback;
                            }
                            i += 1;
                            continue;
                        }
                        XmlPatOp::XmlOpAncestor => {
                            // TODO: implement coalescing of ANCESTOR/NODE ops
                            if step.value.is_none() {
                                i += 1;
                                step = &self.steps[i];
                                if matches!(step.op, XmlPatOp::XmlOpRoot) {
                                    return 1;
                                }
                                if !matches!(step.op, XmlPatOp::XmlOpElem) {
                                    break 'rollback;
                                }
                                if step.value.is_none() {
                                    return -1;
                                }
                            }
                            if matches!(
                                node.element_type(),
                                XmlElementType::XmlDocumentNode
                                    | XmlElementType::XmlHTMLDocumentNode
                                    | XmlElementType::XmlNamespaceDecl
                            ) {
                                break 'rollback;
                            }
                            let mut cur = node.parent();
                            while let Some(cur_node) = cur {
                                if matches!(cur_node.element_type(), XmlElementType::XmlElementNode)
                                {
                                    let node = XmlNodePtr::try_from(cur_node).unwrap();

                                    if step.value.as_deref() == node.name().as_deref() {
                                        // Namespace test
                                        if let Some(ns) = node.ns {
                                            if ns.href.is_some()
                                                && step.value2.as_deref() == ns.href().as_deref()
                                            {
                                                break;
                                            }
                                        } else if step.value2.is_none() {
                                            break;
                                        }
                                    }
                                }
                                cur = cur_node.parent();
                            }
                            let Some(cur) = cur else {
                                break 'rollback;
                            };
                            node = cur;
                            // prepare a potential rollback from here
                            // for ancestors of that node.
                            if matches!(step.op, XmlPatOp::XmlOpAncestor) {
                                states.push_state(i as i32, node);
                            } else {
                                states.push_state(i as i32 - 1, node);
                            }
                            i += 1;
                            continue;
                        }
                        XmlPatOp::XmlOpNs => {
                            if !matches!(node.element_type(), XmlElementType::XmlElementNode) {
                                break 'rollback;
                            }
                            let node = XmlNodePtr::try_from(node).unwrap();
                            if let Some(ns) = node.ns {
                                if ns.href.is_some()
                                    && step.value.as_deref() != ns.href().as_deref()
                                {
                                    break 'rollback;
                                }
                            } else if step.value.is_some() {
                                break 'rollback;
                            }
                        }
                        XmlPatOp::XmlOpAll => {
                            if !matches!(node.element_type(), XmlElementType::XmlElementNode) {
                                break 'rollback;
                            }
                        }
                    }
                    i += 1;
                }
            }
            // rollback:
            // got an error try to rollback
            let Some(state) = states.states.pop() else {
                return 0;
            };
            i = state.step as usize;
            node = state.node;
            // goto restart;
        }
    }

    /// Test whether the node matches the pattern
    ///
    /// Returns 1 if it matches, 0 if it doesn't and -1 in case of failure
    #[doc(alias = "xmlPatternMatch")]
    pub fn pattern_match(&self, node: XmlGenericNodePtr) -> i32 {
        let mut ret: i32 = 0;

        let mut now = Some(self);
        while let Some(comp) = now {
            ret = comp.pattern_match_internal(node);
            if ret != 0 {
                return ret;
            }
            now = comp.next.as_deref();
        }
        ret
    }

    /// Check if the pattern is streamable i.e. xmlPatternGetStreamCtxt() should work.
    ///
    /// Returns 1 if streamable, 0 if not and -1 in case of error.
    #[doc(alias = "xmlPatternStreamable")]
    pub fn is_streamable(&self) -> i32 {
        let mut now = Some(self);
        while let Some(comp) = now {
            if comp.stream.is_none() {
                return 0;
            }
            now = comp.next.as_deref();
        }
        1
    }

    /// Check the maximum depth reachable by a pattern
    ///
    /// Returns -2 if no limit (using //), otherwise the depth, and -1 in case of error
    #[doc(alias = "xmlPatternMaxDepth")]
    pub fn max_depth(&self) -> i32 {
        let mut ret: i32 = 0;

        let mut now = Some(self);
        while let Some(comp) = now {
            let Some(stream) = comp.stream.as_deref() else {
                return -1;
            };

            for i in 0..stream.steps.len() {
                if stream.steps[i].flags & XML_STREAM_STEP_DESC as i32 != 0 {
                    return -2;
                }
            }
            if stream.steps.len() as i32 > ret {
                ret = stream.steps.len() as i32;
            }
            now = comp.next.as_deref();
        }
        ret
    }

    /// Check the minimum depth reachable by a pattern, 0 mean the / or . are part of the set.
    ///
    /// Returns -1 in case of error otherwise the depth,
    #[doc(alias = "xmlPatternMinDepth")]
    pub fn min_depth(&self) -> i32 {
        let mut ret: i32 = 12345678;
        let mut now = Some(self);
        while let Some(comp) = now {
            let Some(stream) = comp.stream.as_deref() else {
                return -1;
            };
            if stream.steps.len() < ret as usize {
                ret = stream.steps.len() as i32;
            }
            if ret == 0 {
                return 0;
            }
            now = comp.next.as_deref();
        }
        ret
    }

    /// Check if the pattern must be looked at from the root.
    ///
    /// Returns 1 if true, 0 if false and -1 in case of error
    #[doc(alias = "xmlPatternFromRoot")]
    pub fn is_from_root(&self) -> i32 {
        let mut now = Some(self);
        while let Some(comp) = now {
            if comp.stream.is_none() {
                return -1;
            }
            if comp.flags & PAT_FROM_ROOT as i32 != 0 {
                return 1;
            }
            now = comp.next.as_deref();
        }
        0
    }

    /// Get a streaming context for that pattern
    /// Use xmlFreeStreamCtxt to free the context.
    ///
    /// Returns a pointer to the context or NULL in case of failure
    #[doc(alias = "xmlPatternGetStreamCtxt")]
    pub fn get_stream_context(&self) -> Option<Box<XmlStreamCtxt>> {
        self.stream.as_ref()?;

        let mut ret: Option<Box<XmlStreamCtxt>> = None;
        let mut now = Some(self);
        while let Some(comp) = now {
            let mut cur = XmlStreamCtxt::new(comp.stream.clone()?);
            cur.flags = comp.flags;
            if let Some(ret) = ret.as_deref_mut() {
                cur.next = ret.next.take();
                ret.next = Some(Box::new(cur));
            } else {
                ret = Some(Box::new(cur))
            }
            now = comp.next.as_deref();
        }
        ret
    }
}

impl Default for XmlPattern {
    fn default() -> Self {
        Self {
            data: null_mut(),
            next: None,
            pattern: None,
            flags: 0,
            steps: vec![],
            stream: None,
        }
    }
}

unsafe impl Send for XmlPattern {}
unsafe impl Sync for XmlPattern {}

// XML_STREAM_ANY_NODE is used for comparison against
// xmlElementType enums, to indicate a node of any type.
const XML_STREAM_ANY_NODE: usize = 100;

/// This is the set of options affecting the behaviour of pattern matching with this module
#[doc(alias = "xmlPatternFlags")]
#[repr(C)]
pub enum XmlPatternFlags {
    XmlPatternDefault = 0,      /* simple pattern match */
    XmlPatternXPath = 1 << 0,   /* standard XPath pattern */
    XmlPatternXSSel = 1 << 1,   /* XPath subset for schema selector */
    XmlPatternXSField = 1 << 2, /* XPath subset for schema field */
}

#[doc(alias = "xmlPatParserContext")]
#[repr(C)]
pub struct XmlPatParserContext {
    cur: usize,                                        /* the current char being parsed */
    base: Box<str>,                                    /* the full expression */
    error: i32,                                        /* error code */
    comp: Option<Box<XmlPattern>>,                     /* the result */
    elem: Option<XmlNodePtr>,                          /* the current node if any */
    namespaces: Option<Vec<(String, Option<String>)>>, /* the namespaces definitions */
}

impl XmlPatParserContext {
    /// Create a new XML pattern parser context
    ///
    /// Returns the newly allocated xmlPatParserContextPtr or NULL in case of error
    #[doc(alias = "xmlNewPatParserContext")]
    fn new(pattern: &str, namespaces: Option<Vec<(String, Option<String>)>>) -> Self {
        XmlPatParserContext {
            cur: 0,
            base: pattern.to_owned().into_boxed_str(),
            namespaces,
            ..Default::default()
        }
    }

    fn current_str(&self) -> &str {
        &self.base[self.cur..]
    }

    fn current_byte(&self) -> Option<u8> {
        self.nth_byte(0)
    }

    fn nth_byte(&self, index: usize) -> Option<u8> {
        self.current_str().as_bytes().get(index).copied()
    }

    fn next(&mut self) -> Option<char> {
        let c = self.current_str().chars().next()?;
        self.cur += c.len_utf8();
        Some(c)
    }

    fn skip_blanks(&mut self) {
        let pat = self.current_str();
        let trimmed = pat.trim_start_matches(|c: char| xml_is_blank_char(c as u32));
        let diff = pat.len() - trimmed.len();
        self.cur += diff;
    }

    fn peek_prev(&self, diff: usize) -> Option<u8> {
        let prev = self.cur.checked_sub(diff)?;
        Some(self.current_str().as_bytes()[prev])
    }

    /// Compile the Path Pattern and generates a precompiled
    /// form suitable for fast matching.
    ///
    /// ```text
    /// [5]    Path    ::=    ('.//')? ( Step '/' )* ( Step | '@' NameTest )
    /// ```
    #[doc(alias = "xmlCompileIDCXPathPath")]
    fn compile_idc_xpath_path(&mut self) {
        self.skip_blanks();
        if self.current_byte() == Some(b'/') {
            self.error = 1;
            return;
        }
        self.comp.as_mut().unwrap().flags |= PAT_FROM_CUR as i32;

        if self.current_byte() == Some(b'.') {
            // "." - "self::node()"
            self.next();
            self.skip_blanks();
            if self.current_byte().is_none() {
                // Selection of the context node.
                if self.pattern_add(XmlPatOp::XmlOpElem, None, None) != 0 {
                    self.error = 1;
                };
                return;
            }
            if self.current_byte() != Some(b'/') {
                // TODO: A more meaningful error message.
                self.error = 1;
                return;
            }
            // "./" - "self::node()/"
            self.next();
            self.skip_blanks();
            if self.current_byte() == Some(b'/') {
                if self
                    .peek_prev(1)
                    .is_some_and(|c| xml_is_blank_char(c as u32))
                {
                    // Disallow "./ /"
                    self.error = 1;
                    return;
                }
                // ".//" - "self:node()/descendant-or-self::node()/"
                if self.pattern_add(XmlPatOp::XmlOpAncestor, None, None) != 0 {
                    self.error = 1;
                    return;
                };
                self.next();
                self.skip_blanks();
            }
            if self.current_byte().is_none() {
                self.error = 1;
                return;
            }
        }
        // Process steps.
        'b: while {
            self.compile_step_pattern();
            if self.error != 0 {
                self.error = 1;
                return;
            }
            self.skip_blanks();
            if self.current_byte() != Some(b'/') {
                break 'b;
            }
            if self.pattern_add(XmlPatOp::XmlOpParent, None, None) != 0 {
                self.error = 1;
                return;
            };
            self.next();
            self.skip_blanks();
            if self.current_byte() == Some(b'/') {
                // Disallow subsequent '//'.
                self.error = 1;
                return;
            }
            if self.current_byte().is_none() {
                self.error = 1;
                return;
            }

            self.current_byte().is_some()
        } {}

        if self.current_byte().is_some() {
            self.error = 1;
        }
    }

    /// Compile the Path Pattern and generates a precompiled
    /// form suitable for fast matching.
    ///
    /// ```text
    /// [5]    Path    ::=    ('.//')? ( Step '/' )* ( Step | '@' NameTest )
    /// ```
    #[doc(alias = "xmlCompilePathPattern")]
    fn compile_path_pattern(&mut self) {
        self.skip_blanks();
        if self.current_byte() == Some(b'/') {
            self.comp.as_mut().unwrap().flags |= PAT_FROM_ROOT as i32;
        } else if self.current_byte() == Some(b'.')
            || self.comp.as_mut().unwrap().flags & XML_PATTERN_NOTPATTERN != 0
        {
            self.comp.as_mut().unwrap().flags |= PAT_FROM_CUR as i32;
        }

        if self.current_byte() == Some(b'/') && self.nth_byte(1) == Some(b'/') {
            if self.pattern_add(XmlPatOp::XmlOpAncestor, None, None) != 0 {
                return;
            };
            self.next();
            self.next();
        } else if self.current_byte() == Some(b'.')
            && self.nth_byte(1) == Some(b'/')
            && self.nth_byte(2) == Some(b'/')
        {
            if self.pattern_add(XmlPatOp::XmlOpAncestor, None, None) != 0 {
                return;
            };
            self.next();
            self.next();
            self.next();
            // Check for incompleteness.
            self.skip_blanks();
            if self.current_byte().is_none() {
                self.error = 1;
                return;
            }
        }
        if self.current_byte() == Some(b'@') {
            self.next();
            self.compile_attribute_test();
            self.skip_blanks();
            // TODO: check for incompleteness
            if self.current_byte().is_some() {
                self.compile_step_pattern();
                if self.error != 0 {
                    return;
                }
            }
        } else {
            if self.current_byte() == Some(b'/') {
                if self.pattern_add(XmlPatOp::XmlOpRoot, None, None) != 0 {
                    return;
                };
                self.next();
                // Check for incompleteness.
                self.skip_blanks();
                if self.current_byte().is_none() {
                    self.error = 1;
                    return;
                }
            }
            self.compile_step_pattern();
            if self.error != 0 {
                return;
            }
            self.skip_blanks();
            while self.current_byte() == Some(b'/') {
                if self.nth_byte(1) == Some(b'/') {
                    if self.pattern_add(XmlPatOp::XmlOpAncestor, None, None) != 0 {
                        return;
                    };
                    self.next();
                    self.next();
                    self.skip_blanks();
                    self.compile_step_pattern();
                    if self.error != 0 {
                        return;
                    }
                } else {
                    if self.pattern_add(XmlPatOp::XmlOpParent, None, None) != 0 {
                        return;
                    };
                    self.next();
                    self.skip_blanks();
                    if self.current_byte().is_none() {
                        self.error = 1;
                        return;
                    }
                    self.compile_step_pattern();
                    if self.error != 0 {
                        return;
                    }
                }
            }
        }
        if self.current_byte().is_some() {
            self.error = 1;
        }
    }

    /// Compile the Step Pattern and generates a precompiled
    /// form suitable for fast matching.
    ///
    /// ```text
    /// [3]    Step    ::=    '.' | NameTest
    /// [4]    NameTest    ::=    QName | '*' | NCName ':' '*'
    /// ```
    #[doc(alias = "xmlCompileStepPattern")]
    fn compile_step_pattern(&mut self) {
        let mut has_blanks: i32 = 0;

        self.skip_blanks();
        if self.current_byte() == Some(b'.') {
            // Context node.
            self.next();
            self.pattern_add(XmlPatOp::XmlOpElem, None, None);
            return;
        }
        if self.current_byte() == Some(b'@') {
            // Attribute test.
            if self.comp.as_ref().unwrap().is_xs_idc_sel() {
                self.error = 1;
                return;
            }
            self.next();
            self.compile_attribute_test();
            return;
        }
        let Some(name) = self.scan_ncname() else {
            if self.current_byte() == Some(b'*') {
                self.next();
                self.pattern_add(XmlPatOp::XmlOpAll, None, None);
            } else {
                self.error = 1;
            }
            return;
        };
        if self
            .current_byte()
            .is_some_and(|b| xml_is_blank_char(b as u32))
        {
            has_blanks = 1;
            self.skip_blanks();
        }
        if self.current_byte() == Some(b':') {
            self.next();
            if self.current_byte() != Some(b':') {
                let prefix = name;

                if has_blanks != 0
                    || self
                        .current_byte()
                        .is_some_and(|b| xml_is_blank_char(b as u32))
                {
                    self.error = 1;
                    return;
                }
                // This is a namespace is_match
                let token = self.scan_name();
                let mut url = None;
                if prefix == "xml" {
                    url = Some(XML_XML_NAMESPACE.to_str().unwrap().to_owned());
                } else if let Some(namespaces) = self.namespaces.as_deref() {
                    if let Some((href, _)) = namespaces
                        .iter()
                        .find(|(_, pref)| pref.as_deref() == Some(&prefix))
                    {
                        url = Some(href.to_owned());
                    } else {
                        self.error = 1;
                        return;
                    }
                }
                if let Some(token) = token {
                    self.pattern_add(XmlPatOp::XmlOpElem, Some(&token), url.as_deref());
                } else if self.current_byte() == Some(b'*') {
                    self.next();
                    if self.pattern_add(XmlPatOp::XmlOpNs, url.as_deref(), None) != 0 {
                        return;
                    };
                } else {
                    self.error = 1;
                    return;
                }
            } else {
                self.next();
                if name == "child" {
                    let Some(name) = self.scan_name() else {
                        if self.current_byte() == Some(b'*') {
                            self.next();
                            self.pattern_add(XmlPatOp::XmlOpAll, None, None);
                        } else {
                            self.error = 1;
                        }
                        return;
                    };
                    if self.current_byte() == Some(b':') {
                        let prefix = name;

                        self.next();
                        if self
                            .current_byte()
                            .is_some_and(|b| xml_is_blank_char(b as u32))
                        {
                            self.error = 1;
                            return;
                        }
                        // This is a namespace is_match
                        let token = self.scan_name();
                        let mut url = None;
                        if prefix == "xml" {
                            url = Some(XML_XML_NAMESPACE.to_str().unwrap().to_owned());
                        } else if let Some(namespaces) = self.namespaces.as_deref() {
                            if let Some((href, _)) = namespaces
                                .iter()
                                .find(|(_, pref)| pref.as_deref() == Some(&prefix))
                            {
                                url = Some(href.to_owned());
                            } else {
                                self.error = 1;
                                return;
                            }
                        }
                        if let Some(token) = token {
                            self.pattern_add(XmlPatOp::XmlOpChild, Some(&token), url.as_deref());
                        } else if self.current_byte() == Some(b'*') {
                            self.next();
                            self.pattern_add(XmlPatOp::XmlOpNs, url.as_deref(), None);
                        } else {
                            self.error = 1;
                        }
                    } else {
                        self.pattern_add(XmlPatOp::XmlOpChild, Some(&name), None);
                    }
                } else if name == "attribute" {
                    if self.comp.as_ref().unwrap().is_xs_idc_sel() {
                        self.error = 1;
                        return;
                    }
                    self.compile_attribute_test();
                    return;
                } else {
                    self.error = 1;
                    return;
                }
            }
        } else if self.current_byte() == Some(b'*') {
            self.error = 1;
        } else {
            self.pattern_add(XmlPatOp::XmlOpElem, Some(&name), None);
        }
    }

    /// Compile an attribute test.
    #[doc(alias = "xmlCompileAttributeTest")]
    fn compile_attribute_test(&mut self) {
        self.skip_blanks();
        let name = self.scan_ncname();
        let Some(name) = name else {
            if self.current_byte() == Some(b'*') {
                if self.pattern_add(XmlPatOp::XmlOpAttr, None, None) != 0 {
                    return;
                };
                self.next();
            } else {
                self.error = 1;
            }
            return;
        };
        if self.current_byte() == Some(b':') {
            let prefix = name;

            self.next();

            if self
                .current_byte()
                .is_some_and(|b| xml_is_blank_char(b as u32))
            {
                self.error = 1;
                return;
            }
            // This is a namespace is_match
            let token = self.scan_name();
            let mut url = None;
            if prefix == "xml" {
                url = Some(XML_XML_NAMESPACE.to_str().unwrap().to_owned());
            } else if let Some(namespaces) = self.namespaces.as_deref() {
                if let Some((href, _)) = namespaces
                    .iter()
                    .find(|(_, pref)| pref.as_deref() == Some(&prefix))
                {
                    url = Some(href.to_owned());
                } else {
                    self.error = 1;
                    return;
                }
            }
            if let Some(token) = token {
                self.pattern_add(XmlPatOp::XmlOpAttr, Some(&token), url.as_deref());
            } else if self.current_byte() == Some(b'*') {
                self.next();
                if self.pattern_add(XmlPatOp::XmlOpAttr, None, url.as_deref()) != 0 {
                    return;
                };
            } else {
                self.error = 1;
                return;
            }
        } else if self.pattern_add(XmlPatOp::XmlOpAttr, Some(&name), None) != 0 {
            return;
        }
    }

    /// ```text
    /// [4] NameChar ::= Letter | Digit | '.' | '-' | '_' | CombiningChar | Extender
    ///
    /// [5] Name ::= (Letter | '_' | ':') (NameChar)*
    ///
    /// [6] Names ::= Name (S Name)*
    /// ```
    ///
    /// Returns the Name parsed or NULL
    #[doc(alias = "xmlPatScanName")]
    fn scan_name(&mut self) -> Option<String> {
        self.skip_blanks();

        let cur = self.current_str();
        if !cur.starts_with(|c: char| xml_is_letter(c as u32) || c == '_' || c == ':') {
            return None;
        }
        let trimmed = cur.trim_start_matches(|c: char| {
            xml_is_letter(c as u32)
                || xml_is_digit(c as u32)
                || c == '.'
                || c == '-'
                || c == '_'
                || xml_is_combining(c as u32)
                || xml_is_extender(c as u32)
        });
        let diff = cur.len() - trimmed.len();
        let ret = cur[..diff].to_owned();
        self.cur += diff;
        Some(ret)
    }

    /// Parses a non qualified name
    ///
    /// Returns the Name parsed or NULL
    #[doc(alias = "xmlPatScanNCName")]
    fn scan_ncname(&mut self) -> Option<String> {
        self.skip_blanks();

        let cur = self.current_str();
        if !cur.starts_with(|c: char| xml_is_letter(c as u32) || c == '_') {
            return None;
        }
        let trimmed = cur.trim_start_matches(|c: char| {
            xml_is_letter(c as u32)
                || xml_is_digit(c as u32)
                || c == '.'
                || c == '-'
                || c == '_'
                || xml_is_combining(c as u32)
                || xml_is_extender(c as u32)
        });
        let diff = cur.len() - trimmed.len();
        let ret = cur[..diff].to_owned();
        self.cur += diff;
        Some(ret)
    }

    /// Add a step to an XSLT Compiled Match
    ///
    /// Returns -1 in case of failure, 0 otherwise.
    #[doc(alias = "xmlPatternAdd")]
    fn pattern_add(&mut self, op: XmlPatOp, value: Option<&str>, value2: Option<&str>) -> i32 {
        self.comp.as_mut().unwrap().steps.push(XmlStepOp {
            op,
            value: value.map(Rc::from),
            value2: value2.map(Rc::from),
        });
        0
    }
}

impl Default for XmlPatParserContext {
    fn default() -> Self {
        Self {
            cur: 0,
            base: "".to_owned().into_boxed_str(),
            error: 0,
            comp: None,
            elem: None,
            namespaces: None,
        }
    }
}

const PAT_FROM_ROOT: usize = 1 << 8;
const PAT_FROM_CUR: usize = 1 << 9;

const XML_PATTERN_NOTPATTERN: i32 = XmlPatternFlags::XmlPatternXPath as i32
    | XmlPatternFlags::XmlPatternXSSel as i32
    | XmlPatternFlags::XmlPatternXSField as i32;

/// Compile a pattern.
///
/// Returns the compiled form of the pattern or NULL in case of error
#[doc(alias = "xmlPatterncompile")]
pub fn xml_pattern_compile(
    pattern: &str,
    flags: i32,
    namespaces: Option<Vec<(String, Option<String>)>>,
) -> Option<Box<XmlPattern>> {
    let mut ret: Option<Box<XmlPattern>> = None;
    let mut typ: i32 = 0;
    let mut streamable: i32 = 1;

    for pattern in pattern.split('|') {
        let mut ctxt = XmlPatParserContext::new(pattern, namespaces.clone());
        let mut cur = XmlPattern::new();
        cur.flags = flags;
        let is_xml_stream_xs_idc = cur.is_xs_idc();
        ctxt.comp = Some(Box::new(cur));

        if is_xml_stream_xs_idc {
            ctxt.compile_idc_xpath_path();
        } else {
            ctxt.compile_path_pattern();
        }
        if ctxt.error != 0 {
            return None;
        }

        let mut cur = ctxt.comp.unwrap();

        if streamable != 0 {
            if typ == 0 {
                typ = cur.flags & (PAT_FROM_ROOT | PAT_FROM_CUR) as i32;
            } else if typ == PAT_FROM_ROOT as i32 {
                if cur.flags & PAT_FROM_CUR as i32 != 0 {
                    streamable = 0;
                }
            } else if typ == PAT_FROM_CUR as i32 && cur.flags & PAT_FROM_ROOT as i32 != 0 {
                streamable = 0;
            }
        }
        if streamable != 0 {
            cur.stream_compile();
        }
        if cur.reverse_pattern() < 0 {
            return None;
        }
        if let Some(ret) = ret.as_deref_mut() {
            cur.next = ret.next.take();
            ret.next = Some(cur);
        } else {
            ret = Some(cur);
        }
    }
    if streamable == 0 {
        let mut now = ret.as_deref_mut();
        while let Some(cur) = now {
            cur.stream.take();
            now = cur.next.as_deref_mut();
        }
    }

    ret
}

struct XmlStepState {
    step: i32,
    node: XmlGenericNodePtr,
}

struct XmlStepStates {
    states: Vec<XmlStepState>,
}

impl XmlStepStates {
    fn push_state(&mut self, step: i32, node: XmlGenericNodePtr) -> i32 {
        self.states.push(XmlStepState { step, node });
        0
    }
}

// streaming interfaces
#[doc(alias = "xmlStreamCtxt")]
#[repr(C)]
pub struct XmlStreamCtxt {
    // link to next sub pattern if |
    next: Option<Box<XmlStreamCtxt>>,
    // the compiled stream
    comp: Rc<XmlStreamComp>,
    // how deep are we ?
    level: i32,
    // the array of step indexes
    // (index, level)
    states: Vec<(i32, i32)>,
    // validation options
    flags: i32,
    block_level: i32,
}

impl XmlStreamCtxt {
    /// Build a new stream context
    ///
    /// Returns the new structure or NULL in case of error.
    #[doc(alias = "xmlNewStreamCtxt")]
    fn new(stream: Rc<XmlStreamComp>) -> Self {
        Self {
            next: None,
            comp: stream,
            level: 0,
            states: Vec::with_capacity(4),
            flags: 0,
            block_level: -1,
        }
    }

    #[doc(alias = "XML_STREAM_XS_IDC")]
    fn is_xs_idc(&self) -> bool {
        self.flags
            & (XmlPatternFlags::XmlPatternXSSel as i32 | XmlPatternFlags::XmlPatternXSField as i32)
            != 0
    }

    #[doc(alias = "XML_STREAM_XS_IDC_SEL")]
    fn is_xs_idc_sel(&self) -> bool {
        self.flags & XmlPatternFlags::XmlPatternXSSel as i32 != 0
    }

    /// Add a new state to the stream context
    ///
    /// Returns -1 in case of error or the state index if successful
    #[doc(alias = "xmlStreamCtxtAddState")]
    fn add_state(&mut self, idx: i32, level: i32) -> i32 {
        for i in 0..self.states.len() {
            if self.states[i].0 < 0 {
                self.states[i] = (idx, level);
                return i as i32;
            }
        }
        self.states.push((idx, level));
        self.states.len() as i32 - 1
    }

    /// Push new data onto the stream. NOTE: if the call xmlPatterncompile()
    /// indicated a dictionary, then strings for name and ns will be expected
    /// to come from the dictionary.
    /// Both @name and @ns being NULL means the / i.e. the root of the document.
    /// This can also act as a reset.
    ///
    /// Returns: -1 in case of error, 1 if the current state in the stream is a is_match and 0 otherwise.
    #[doc(alias = "xmlStreamPushInternal")]
    fn push_internal(&mut self, name: Option<&str>, ns: Option<&str>, node_type: i32) -> i32 {
        let mut ret: i32 = 0;
        let mut err: i32 = 0;
        let mut is_final: i32 = 0;
        let mut step_nr: i32;

        let mut now = Some(self);
        'stream: while let Some(stream) = now {
            'stream_next: {
                let comp = stream.comp.clone();

                if node_type == XmlElementType::XmlElementNode as i32
                    && name.is_none()
                    && ns.is_none()
                {
                    // We have a document node here (or a reset).
                    stream.states.clear();
                    stream.level = 0;
                    stream.block_level = -1;
                    if comp.flags & XML_STREAM_FROM_ROOT as i32 != 0 {
                        if comp.steps.is_empty() {
                            // TODO: We have a "/." here?
                            ret = 1;
                        } else if comp.steps.len() == 1
                            && comp.steps[0].node_type == XML_STREAM_ANY_NODE as i32
                            && comp.steps[0].flags & XML_STREAM_STEP_DESC as i32 != 0
                        {
                            // In the case of "//." the document node will is_match as well.
                            ret = 1;
                        } else if comp.steps[0].flags & XML_STREAM_STEP_ROOT as i32 != 0 {
                            // TODO: Do we need this ?
                            let tmp = stream.add_state(0, 0);
                            if tmp < 0 {
                                err += 1;
                            }
                        }
                    }
                    now = stream.next.as_deref_mut();
                    continue 'stream;
                }

                // Fast check for ".".
                if comp.steps.is_empty() {
                    // / and . are handled at the XPath node set creation
                    // level by checking min depth
                    if stream.flags & XmlPatternFlags::XmlPatternXPath as i32 != 0 {
                        now = stream.next.as_deref_mut();
                        continue 'stream; /* while */
                    }
                    // For non-pattern like evaluation like XML Schema IDCs
                    // or traditional XPath expressions, this will is_match if
                    // we are at the first level only, otherwise on every level.
                    if node_type != XmlElementType::XmlAttributeNode as i32
                        && (stream.flags & XML_PATTERN_NOTPATTERN == 0 || stream.level == 0)
                    {
                        ret = 1;
                    }
                    stream.level += 1;
                    break 'stream_next;
                }
                if stream.block_level != -1 {
                    // Skip blocked expressions.
                    stream.level += 1;
                    break 'stream_next;
                }

                if node_type != XmlElementType::XmlElementNode as i32
                    && node_type != XmlElementType::XmlAttributeNode as i32
                    && comp.flags & XML_STREAM_FINAL_IS_ANY_NODE as i32 == 0
                {
                    // No need to process nodes of other types if we don't
                    // resolve to those types.
                    // TODO: Do we need to block the context here?
                    stream.level += 1;
                    break 'stream_next;
                }

                // Check evolution of existing states
                let mut i = 0;
                let m = stream.states.len();
                while i < m {
                    if comp.flags & XML_STREAM_DESC as i32 == 0 {
                        // If there is no "//", then only the last
                        // added state is of interest.
                        step_nr = stream.states.last().unwrap().0;
                        // TODO: Security check, should not happen, remove it.
                        if stream.states.last().unwrap().1 < stream.level {
                            return -1;
                        }
                        // desc = 0;
                        // loop-stopper
                        i = m;
                    } else {
                        // If there are "//", then we need to process every "//"
                        // occurring in the states, plus any other state for this level.
                        step_nr = stream.states[i].0;

                        // TODO: should not happen anymore: dead states
                        if step_nr < 0 {
                            i += 1;
                            continue;
                        }

                        let tmp = stream.states[i].1;

                        // skip new states just added
                        if tmp > stream.level {
                            i += 1;
                            continue;
                        }

                        // skip states at ancestor levels, except if "//"
                        let desc = comp.steps[step_nr as usize].flags & XML_STREAM_STEP_DESC as i32;
                        if tmp < stream.level && desc == 0 {
                            i += 1;
                            continue;
                        }
                    }
                    // Check for correct node-type.
                    let step = &comp.steps[step_nr as usize];
                    if step.node_type != node_type {
                        if step.node_type == XmlElementType::XmlAttributeNode as i32 {
                            // Block this expression for deeper evaluation.
                            if comp.flags & XML_STREAM_DESC as i32 == 0 {
                                stream.block_level = stream.level + 1;
                            }
                            i += 1;
                            continue;
                        } else if step.node_type != XML_STREAM_ANY_NODE as i32 {
                            i += 1;
                            continue;
                        }
                    }
                    // Compare local/namespace-name.
                    let mut is_match = false;
                    if step.node_type == XML_STREAM_ANY_NODE as i32 {
                        is_match = true;
                    } else if step.name.is_none() {
                        if step.ns.is_none() {
                            // This lets through all elements/attributes.
                            is_match = true;
                        } else if ns.is_some() {
                            is_match = step.ns.as_deref() == ns;
                        }
                    } else if step.ns.is_none() == ns.is_none()
                        && name.is_some()
                        && step.name.as_deref() == name
                        && step.ns.as_deref() == ns
                    {
                        is_match = true;
                    }
                    // #if 0
                    // /*
                    // * TODO: Pointer comparison won't work, since not guaranteed that the given
                    // *  values are in the same dict; especially if it's the namespace name,
                    // *  normally coming from ns->href. We need a namespace dict mechanism !
                    // */
                    //  } else if ((*comp).dict) {
                    //      if (step.name.is_null()) {
                    //          if (step.ns.is_null())
                    //      	is_match = 1;
                    //          else
                    //      	is_match = (step.ns == ns);
                    //      } else {
                    //          is_match = ((step.name == name) && (step.ns == ns));
                    //      }
                    // #endif /* if 0 ------------------------------------------------------- */
                    if is_match {
                        is_final = step.flags & XML_STREAM_STEP_FINAL as i32;
                        if is_final != 0 {
                            ret = 1;
                        } else {
                            stream.add_state(step_nr + 1, stream.level + 1);
                        }
                        if ret != 1 && step.flags & XML_STREAM_STEP_IN_SET as i32 != 0 {
                            // Check if we have a special case like "foo/bar//.", where
                            // "foo" is selected as well.
                            ret = 1;
                        }
                    }
                    if comp.flags & XML_STREAM_DESC as i32 == 0 && (!is_match || is_final != 0) {
                        // Mark this expression as blocked for any evaluation at
                        // deeper levels. Note that this includes "/foo"
                        // expressions if the *pattern* behaviour is used.
                        stream.block_level = stream.level + 1;
                    }
                    i += 1;
                }

                stream.level += 1;

                // Re/enter the expression.
                // Don't reenter if it's an absolute expression like "/foo",
                //   except "//foo".
                let step = &comp.steps[0];
                if step.flags & XML_STREAM_STEP_ROOT as i32 != 0 {
                    break 'stream_next;
                }

                let desc = step.flags & XML_STREAM_STEP_DESC as i32;
                'compare: {
                    if stream.flags & XML_PATTERN_NOTPATTERN != 0 {
                        // Re/enter the expression if it is a "descendant" one,
                        // or if we are at the 1st level of evaluation.
                        if stream.level == 1 {
                            if stream.is_xs_idc() {
                                // XS-IDC: The missing "self::node()" will always
                                // is_match the first given node.
                                break 'stream_next;
                            } else {
                                break 'compare;
                            }
                        }
                        // A "//" is always reentrant.
                        if desc != 0 {
                            break 'compare;
                        }
                        // XS-IDC: Process the 2nd level, since the missing
                        // "self::node()" is responsible for the 2nd level being
                        // the real start level.
                        if stream.level == 2 && stream.is_xs_idc() {
                            break 'compare;
                        }
                        break 'stream_next;
                    }
                }

                // compare:
                // Check expected node-type.
                if step.node_type != node_type
                    && (node_type == XmlElementType::XmlAttributeNode as i32
                        || step.node_type != XML_STREAM_ANY_NODE as i32)
                {
                    break 'stream_next;
                }
                // Compare local/namespace-name.
                let mut is_match = false;
                if step.node_type == XML_STREAM_ANY_NODE as i32 {
                    is_match = true;
                } else if step.name.is_none() {
                    if step.ns.is_none() {
                        // This lets through all elements/attributes.
                        is_match = true;
                    } else if ns.is_some() {
                        is_match = step.ns.as_deref() == ns;
                    }
                } else if step.ns.is_none() == ns.is_none()
                    && name.is_some()
                    && step.name.as_deref() == name
                    && step.ns.as_deref() == ns
                {
                    is_match = true;
                }
                is_final = step.flags & XML_STREAM_STEP_FINAL as i32;
                if is_match {
                    if is_final != 0 {
                        ret = 1;
                    } else {
                        stream.add_state(1, stream.level);
                    }
                    if ret != 1 && step.flags & XML_STREAM_STEP_IN_SET as i32 != 0 {
                        // Check if we have a special case like "foo//.", where
                        // "foo" is selected as well.
                        ret = 1;
                    }
                }
                if comp.flags & XML_STREAM_DESC as i32 == 0 && (!is_match || is_final != 0) {
                    // Mark this expression as blocked for any evaluation at
                    // deeper levels.
                    stream.block_level = stream.level;
                }
            }

            // stream_next:
            now = stream.next.as_deref_mut();
        }

        if err > 0 {
            ret = -1;
        }
        ret
    }

    /// Push new data onto the stream. NOTE: if the call xmlPatterncompile()
    /// indicated a dictionary, then strings for name and ns will be expected
    /// to come from the dictionary.
    /// Both @name and @ns being NULL means the / i.e. the root of the document.
    /// This can also act as a reset.
    /// Different from xmlStreamPush() this function can be fed with nodes of type:
    /// element-, attribute-, text-, cdata-section-, comment- and
    /// processing-instruction-node.
    ///
    /// Returns: -1 in case of error, 1 if the current state in the stream is a is_match and 0 otherwise.
    #[doc(alias = "xmlStreamPushNode")]
    pub fn push_node(&mut self, name: Option<&str>, ns: Option<&str>, node_type: i32) -> i32 {
        self.push_internal(name, ns, node_type)
    }

    /// Push new data onto the stream. NOTE: if the call xmlPatterncompile()
    /// indicated a dictionary, then strings for name and ns will be expected
    /// to come from the dictionary.
    /// Both @name and @ns being NULL means the / i.e. the root of the document.
    /// This can also act as a reset.
    /// Otherwise the function will act as if it has been given an element-node.
    ///
    /// Returns: -1 in case of error, 1 if the current state in the stream is a is_match and 0 otherwise.
    #[doc(alias = "xmlStreamPush")]
    pub fn push(&mut self, name: Option<&str>, ns: Option<&str>) -> i32 {
        self.push_internal(name, ns, XmlElementType::XmlElementNode as i32)
    }

    /// Push new attribute data onto the stream. NOTE: if the call xmlPatterncompile()
    /// indicated a dictionary, then strings for name and ns will be expected
    /// to come from the dictionary.
    /// Both @name and @ns being NULL means the / i.e. the root of the document.
    /// This can also act as a reset.
    /// Otherwise the function will act as if it has been given an attribute-node.
    ///
    /// Returns: -1 in case of error, 1 if the current state in the stream is a is_match and 0 otherwise.
    #[doc(alias = "xmlStreamPushAttr")]
    pub fn push_attr(&mut self, name: Option<&str>, ns: Option<&str>) -> i32 {
        self.push_internal(name, ns, XmlElementType::XmlAttributeNode as i32)
    }

    /// Push one level from the stream.
    ///
    /// Returns: -1 in case of error, 0 otherwise.
    #[doc(alias = "xmlStreamPop")]
    pub fn pop(&mut self) -> i32 {
        let mut now = Some(self);
        while let Some(stream) = now {
            // Reset block-level.
            if stream.block_level == stream.level {
                stream.block_level = -1;
            }

            //  (*stream).level can be zero when XML_FINAL_IS_ANY_NODE is set
            //  (see the thread at
            //  http://mail.gnome.org/archives/xslt/2008-July/msg00027.html)
            if stream.level != 0 {
                stream.level -= 1;
            }
            // Check evolution of existing states
            while stream
                .states
                .last()
                .is_some_and(|state| state.1 > stream.level)
            {
                stream.states.pop();
            }
            now = stream.next.as_deref_mut();
        }
        0
    }

    /// Query if the streaming pattern additionally needs to be fed with
    /// text-, cdata-section-, comment- and processing-instruction-nodes.
    /// If the result is 0 then only element-nodes and attribute-nodes
    /// need to be pushed.
    ///
    /// Returns 1 in case of need of nodes of the above described types,
    /// 0 otherwise. -1 on API errors.
    #[doc(alias = "xmlStreamWantsAnyNode")]
    pub fn wants_any_node(&self) -> i32 {
        let mut now = Some(self);
        while let Some(stream) = now {
            if stream.comp.flags & XML_STREAM_FINAL_IS_ANY_NODE as i32 != 0 {
                return 1;
            }
            now = stream.next.as_deref();
        }
        0
    }
}

unsafe impl Send for XmlStreamCtxt {}
unsafe impl Sync for XmlStreamCtxt {}

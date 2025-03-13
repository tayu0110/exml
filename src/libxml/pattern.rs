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

use std::{
    ffi::CStr,
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, drop_in_place, null, null_mut},
    rc::Rc,
};

use libc::memset;

use crate::tree::{NodeCommon, XmlAttrPtr};
use crate::{
    libxml::{
        chvalid::{xml_is_blank_char, xml_is_combining, xml_is_digit, xml_is_extender},
        globals::{xml_free, xml_malloc, xml_realloc},
        parser_internals::xml_is_letter,
        xmlstring::XmlChar,
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

pub type XmlStepOpPtr = *mut XmlStepOp;
#[repr(C)]
#[derive(Debug, Clone)]
pub struct XmlStepOp {
    op: XmlPatOp,
    value: Option<Rc<str>>,
    value2: Option<Rc<str>>, /* The namespace name */
}

pub type XmlStreamStepPtr = *mut XmlStreamStep;
#[repr(C)]
#[derive(Debug, Clone)]
pub struct XmlStreamStep {
    flags: i32,            /* properties of that step */
    name: Option<Rc<str>>, /* first string value if NULL accept all */
    ns: Option<Rc<str>>,   /* second string value */
    node_type: i32,        /* type of node */
}

pub type XmlStreamCompPtr = *mut XmlStreamComp;
#[repr(C)]
#[derive(Default)]
pub struct XmlStreamComp {
    steps: Vec<XmlStreamStep>, /* the array of steps */
    flags: i32,
}

/// A compiled (XPath based) pattern to select nodes
#[doc(alias = "xmlPattern")]
pub type XmlPatternPtr = *mut XmlPattern;
#[repr(C)]
pub struct XmlPattern {
    data: *mut c_void,        /* the associated template */
    next: *mut XmlPattern,    /* next pattern if | is used */
    pattern: *const XmlChar,  /* the pattern */
    flags: i32,               /* flags */
    steps: Vec<XmlStepOp>,    /* ops for computation */
    stream: XmlStreamCompPtr, /* the streaming data if any */
}

impl Default for XmlPattern {
    fn default() -> Self {
        Self {
            data: null_mut(),
            next: null_mut(),
            pattern: null(),
            flags: 0,
            steps: vec![],
            stream: null_mut(),
        }
    }
}

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

/// Free up the memory allocated by @comp
#[doc(alias = "xmlFreePattern")]
pub unsafe fn xml_free_pattern(comp: XmlPatternPtr) {
    unsafe {
        xml_free_pattern_list(comp);
    }
}

/// Free the compiled pattern for streaming
#[doc(alias = "xmlFreeStreamComp")]
unsafe fn xml_free_stream_comp(comp: XmlStreamCompPtr) {
    unsafe {
        if !comp.is_null() {
            drop_in_place(comp);
            xml_free(comp as _);
        }
    }
}

unsafe fn xml_free_pattern_internal(comp: XmlPatternPtr) {
    unsafe {
        if comp.is_null() {
            return;
        }
        if !(*comp).stream.is_null() {
            xml_free_stream_comp((*comp).stream);
        }
        if !(*comp).pattern.is_null() {
            xml_free((*comp).pattern as _);
        }

        drop_in_place(comp);
        xml_free(comp as _);
    }
}

/// Free up the memory allocated by all the elements of @comp
#[doc(alias = "xmlFreePatternList")]
pub unsafe fn xml_free_pattern_list(mut comp: XmlPatternPtr) {
    unsafe {
        let mut cur: XmlPatternPtr;

        while !comp.is_null() {
            cur = comp;
            comp = (*comp).next;
            (*cur).next = null_mut();
            xml_free_pattern_internal(cur);
        }
    }
}

macro_rules! XML_STREAM_XS_IDC {
    ($c:expr) => {
        (*$c).flags
            & (XmlPatternFlags::XmlPatternXSSel as i32 | XmlPatternFlags::XmlPatternXSField as i32)
            != 0
    };
}

macro_rules! XML_STREAM_XS_IDC_SEL {
    ($c:expr) => {
        (*$c).flags & XmlPatternFlags::XmlPatternXSSel as i32 != 0
    };
}

#[doc(alias = "xmlPatParserContext")]
#[repr(C)]
pub struct XmlPatParserContext {
    cur: usize,                                        /* the current char being parsed */
    base: Box<str>,                                    /* the full expression */
    error: i32,                                        /* error code */
    comp: XmlPatternPtr,                               /* the result */
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
    unsafe fn compile_idc_xpath_path(&mut self) {
        unsafe {
            self.skip_blanks();
            if self.current_byte() == Some(b'/') {
                self.error = 1;
                return;
            }
            (*self.comp).flags |= PAT_FROM_CUR as i32;

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
    }

    /// Compile the Path Pattern and generates a precompiled
    /// form suitable for fast matching.
    ///
    /// ```text
    /// [5]    Path    ::=    ('.//')? ( Step '/' )* ( Step | '@' NameTest )
    /// ```
    #[doc(alias = "xmlCompilePathPattern")]
    unsafe fn compile_path_pattern(&mut self) {
        unsafe {
            self.skip_blanks();
            if self.current_byte() == Some(b'/') {
                (*self.comp).flags |= PAT_FROM_ROOT as i32;
            } else if self.current_byte() == Some(b'.')
                || (*self.comp).flags & XML_PATTERN_NOTPATTERN != 0
            {
                (*self.comp).flags |= PAT_FROM_CUR as i32;
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
    }

    /// Compile the Step Pattern and generates a precompiled
    /// form suitable for fast matching.
    ///
    /// ```text
    /// [3]    Step    ::=    '.' | NameTest
    /// [4]    NameTest    ::=    QName | '*' | NCName ':' '*'
    /// ```
    #[doc(alias = "xmlCompileStepPattern")]
    unsafe fn compile_step_pattern(&mut self) {
        unsafe {
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
                if XML_STREAM_XS_IDC_SEL!(self.comp) {
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
                                self.pattern_add(
                                    XmlPatOp::XmlOpChild,
                                    Some(&token),
                                    url.as_deref(),
                                );
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
                        if XML_STREAM_XS_IDC_SEL!(self.comp) {
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
    }

    /// Compile an attribute test.
    #[doc(alias = "xmlCompileAttributeTest")]
    unsafe fn compile_attribute_test(&mut self) {
        self.skip_blanks();
        let name = self.scan_ncname();
        unsafe {
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
    unsafe fn pattern_add(
        &mut self,
        op: XmlPatOp,
        value: Option<&str>,
        value2: Option<&str>,
    ) -> i32 {
        unsafe {
            (*self.comp).steps.push(XmlStepOp {
                op,
                value: value.map(Rc::from),
                value2: value2.map(Rc::from),
            });
            0
        }
    }
}

impl Default for XmlPatParserContext {
    fn default() -> Self {
        Self {
            cur: 0,
            base: "".to_owned().into_boxed_str(),
            error: 0,
            comp: null_mut(),
            elem: None,
            namespaces: None,
        }
    }
}

/// Create a new XSLT Pattern
///
/// Returns the newly allocated xmlPatternPtr or NULL in case of error
#[doc(alias = "xmlNewPattern")]
unsafe fn xml_new_pattern() -> XmlPatternPtr {
    unsafe {
        let cur: XmlPatternPtr = xml_malloc(size_of::<XmlPattern>()) as XmlPatternPtr;
        if cur.is_null() {
            return null_mut();
        }
        std::ptr::write(&mut *cur, XmlPattern::default());
        (*cur).steps.reserve(10);
        cur
    }
}

const PAT_FROM_ROOT: usize = 1 << 8;
const PAT_FROM_CUR: usize = 1 << 9;

const XML_PATTERN_NOTPATTERN: i32 = XmlPatternFlags::XmlPatternXPath as i32
    | XmlPatternFlags::XmlPatternXSSel as i32
    | XmlPatternFlags::XmlPatternXSField as i32;

/// Build a new compiled pattern for streaming
///
/// Returns the new structure or NULL in case of error.
#[doc(alias = "xmlNewStreamComp")]
unsafe fn xml_new_stream_comp(mut size: i32) -> XmlStreamCompPtr {
    unsafe {
        if size < 4 {
            size = 4;
        }

        let cur: XmlStreamCompPtr = xml_malloc(size_of::<XmlStreamComp>()) as XmlStreamCompPtr;
        if cur.is_null() {
            return null_mut();
        }
        std::ptr::write(&mut *cur, XmlStreamComp::default());
        (*cur).steps.reserve(size as usize);
        cur
    }
}

/// Add a new step to the compiled pattern
///
/// Returns -1 in case of error or the step index if successful
#[doc(alias = "xmlStreamCompAddStep")]
unsafe fn xml_stream_comp_add_step(
    comp: XmlStreamCompPtr,
    name: Option<Rc<str>>,
    ns: Option<Rc<str>>,
    node_type: i32,
    flags: i32,
) -> i32 {
    unsafe {
        (*comp).steps.push(XmlStreamStep {
            flags,
            name,
            ns,
            node_type,
        });
        (*comp).steps.len() as i32 - 1
    }
}

/// Tries to stream compile a pattern
///
/// Returns -1 in case of failure and 0 in case of success.
#[doc(alias = "xmlStreamCompile")]
unsafe fn xml_stream_compile(comp: XmlPatternPtr) -> i32 {
    unsafe {
        let stream: XmlStreamCompPtr;
        let mut s: i32 = 0;
        let mut root: i32 = 0;
        let mut flags: i32 = 0;
        let mut prevs: i32 = -1;

        if comp.is_null() || (*comp).steps.is_empty() {
            return -1;
        }
        // special case for .
        if (*comp).steps.len() == 1
            && matches!((*comp).steps[0].op, XmlPatOp::XmlOpElem)
            && (*comp).steps[0].value.is_none()
            && (*comp).steps[0].value2.is_none()
        {
            stream = xml_new_stream_comp(0);
            if stream.is_null() {
                return -1;
            }
            // Note that the stream will have no steps in this case.
            (*stream).flags |= XML_STREAM_FINAL_IS_ANY_NODE as i32;
            (*comp).stream = stream;
            return 0;
        }

        stream = xml_new_stream_comp(((*comp).steps.len() as i32 / 2) + 1);
        if stream.is_null() {
            return -1;
        }

        if (*comp).flags & PAT_FROM_ROOT as i32 != 0 {
            (*stream).flags |= XML_STREAM_FROM_ROOT as i32;
        }

        'error: {
            'main: for i in 0..(*comp).steps.len() {
                let step = &(*comp).steps[i];
                match step.op {
                    XmlPatOp::XmlOpEnd => {}
                    XmlPatOp::XmlOpRoot => {
                        if i != 0 {
                            break 'error;
                        }
                        root = 1;
                    }
                    XmlPatOp::XmlOpNs => {
                        s = xml_stream_comp_add_step(
                            stream,
                            None,
                            step.value.clone(),
                            XmlElementType::XmlElementNode as i32,
                            flags,
                        );
                        if s < 0 {
                            break 'error;
                        }
                        prevs = s;
                        flags = 0;
                    }
                    XmlPatOp::XmlOpAttr => {
                        flags |= XML_STREAM_STEP_ATTR as i32;
                        prevs = -1;
                        s = xml_stream_comp_add_step(
                            stream,
                            step.value.clone(),
                            step.value2.clone(),
                            XmlElementType::XmlAttributeNode as i32,
                            flags,
                        );
                        flags = 0;
                        if s < 0 {
                            break 'error;
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
                            if (*comp).steps.len() == i + 1
                                && flags & XML_STREAM_STEP_DESC as i32 != 0
                            {
                                // Mark the special case where the expression resolves
                                // to any type of node.
                                if (*comp).steps.len() == i + 1 {
                                    (*stream).flags |= XML_STREAM_FINAL_IS_ANY_NODE as i32;
                                }
                                flags |= XML_STREAM_STEP_NODE as i32;
                                s = xml_stream_comp_add_step(
                                    stream,
                                    None,
                                    None,
                                    XML_STREAM_ANY_NODE as i32,
                                    flags,
                                );
                                if s < 0 {
                                    break 'error;
                                }
                                flags = 0;
                                // If there was a previous step, mark it to be added to
                                // the result node-set; this is needed since only
                                // the last step will be marked as "is_final" and only
                                // "is_final" nodes are added to the resulting set.
                                if prevs != -1 {
                                    (*stream).steps[prevs as usize].flags |=
                                        XML_STREAM_STEP_IN_SET as i32;
                                    prevs = -1;
                                }
                                break 'to_break;
                            } else {
                                // Just skip this one.
                                continue 'main;
                            }
                        }
                        // An element node.
                        s = xml_stream_comp_add_step(
                            stream,
                            step.value.clone(),
                            step.value2.clone(),
                            XmlElementType::XmlElementNode as i32,
                            flags,
                        );
                        if s < 0 {
                            break 'error;
                        }
                        prevs = s;
                        flags = 0;
                    }
                    XmlPatOp::XmlOpChild => {
                        // An element node child.
                        s = xml_stream_comp_add_step(
                            stream,
                            step.value.clone(),
                            step.value2.clone(),
                            XmlElementType::XmlElementNode as i32,
                            flags,
                        );
                        if s < 0 {
                            break 'error;
                        }
                        prevs = s;
                        flags = 0;
                    }
                    XmlPatOp::XmlOpAll => {
                        s = xml_stream_comp_add_step(
                            stream,
                            None,
                            None,
                            XmlElementType::XmlElementNode as i32,
                            flags,
                        );
                        if s < 0 {
                            break 'error;
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
                            if (*stream).flags & XML_STREAM_DESC as i32 == 0 {
                                (*stream).flags |= XML_STREAM_DESC as i32;
                            }
                        }
                    }
                }
            }
            if root == 0 && (*comp).flags & XML_PATTERN_NOTPATTERN == 0 {
                // If this should behave like a real pattern, we will mark
                // the first step as having "//", to be reentrant on every
                // tree level.
                if (*stream).flags & XML_STREAM_DESC as i32 == 0 {
                    (*stream).flags |= XML_STREAM_DESC as i32;
                }

                if !(*stream).steps.is_empty()
                    && (*stream).steps[0].flags & XML_STREAM_STEP_DESC as i32 == 0
                {
                    (*stream).steps[0].flags |= XML_STREAM_STEP_DESC as i32;
                }
            }
            if (*stream).steps.len() as i32 <= s {
                break 'error;
            }
            (*stream).steps[s as usize].flags |= XML_STREAM_STEP_FINAL as i32;
            if root != 0 {
                (*stream).steps[0].flags |= XML_STREAM_STEP_ROOT as i32;
            }
            (*comp).stream = stream;
            return 0;
        }
        // error:
        xml_free_stream_comp(stream);
        0
    }
}

/// Reverse all the stack of expressions
///
/// Returns 0 in case of success and -1 in case of error.
#[doc(alias = "xmlReversePattern")]
unsafe fn xml_reverse_pattern(comp: XmlPatternPtr) -> i32 {
    unsafe {
        // remove the leading // for //a or .//a
        if !(*comp).steps.is_empty() && matches!((*comp).steps[0].op, XmlPatOp::XmlOpAncestor) {
            (*comp).steps.remove(0);
        }
        (*comp).steps.reverse();
        (*comp).steps.push(XmlStepOp {
            op: XmlPatOp::XmlOpEnd,
            value: None,
            value2: None,
        });
        0
    }
}

/// Compile a pattern.
///
/// Returns the compiled form of the pattern or NULL in case of error
#[doc(alias = "xmlPatterncompile")]
pub unsafe fn xml_pattern_compile(
    pattern: &str,
    flags: i32,
    namespaces: Option<Vec<(String, Option<String>)>>,
) -> XmlPatternPtr {
    unsafe {
        let mut ret: XmlPatternPtr = null_mut();
        let mut cur: XmlPatternPtr;
        let mut typ: i32 = 0;
        let mut streamable: i32 = 1;

        'error: {
            for pattern in pattern.split('|') {
                let mut ctxt = XmlPatParserContext::new(pattern, namespaces.clone());
                cur = xml_new_pattern();
                if cur.is_null() {
                    break 'error;
                }
                // Assign string dict.
                if ret.is_null() {
                    ret = cur;
                } else {
                    (*cur).next = (*ret).next;
                    (*ret).next = cur;
                }
                (*cur).flags = flags;
                ctxt.comp = cur;

                if XML_STREAM_XS_IDC!(cur) {
                    ctxt.compile_idc_xpath_path();
                } else {
                    ctxt.compile_path_pattern();
                }
                if ctxt.error != 0 {
                    break 'error;
                }

                if streamable != 0 {
                    if typ == 0 {
                        typ = (*cur).flags & (PAT_FROM_ROOT | PAT_FROM_CUR) as i32;
                    } else if typ == PAT_FROM_ROOT as i32 {
                        if (*cur).flags & PAT_FROM_CUR as i32 != 0 {
                            streamable = 0;
                        }
                    } else if typ == PAT_FROM_CUR as i32 && (*cur).flags & PAT_FROM_ROOT as i32 != 0
                    {
                        streamable = 0;
                    }
                }
                if streamable != 0 {
                    xml_stream_compile(cur);
                }
                if xml_reverse_pattern(cur) < 0 {
                    break 'error;
                }
            }
            if streamable == 0 {
                cur = ret;
                while !cur.is_null() {
                    if !(*cur).stream.is_null() {
                        xml_free_stream_comp((*cur).stream);
                        (*cur).stream = null_mut();
                    }
                    cur = (*cur).next;
                }
            }

            return ret;
        }
        // error:
        if !ret.is_null() {
            xml_free_pattern(ret);
        }
        null_mut()
    }
}

pub type XmlStepStatePtr = *mut XmlStepState;
#[repr(C)]
pub struct XmlStepState {
    step: i32,
    node: XmlGenericNodePtr,
}

pub type XmlStepStatesPtr = *mut XmlStepStates;
#[repr(C)]
pub struct XmlStepStates {
    nbstates: i32,
    maxstates: i32,
    states: XmlStepStatePtr,
}

unsafe fn xml_pat_push_state(
    states: *mut XmlStepStates,
    step: i32,
    node: XmlGenericNodePtr,
) -> i32 {
    unsafe {
        if (*states).states.is_null() || (*states).maxstates <= 0 {
            (*states).maxstates = 4;
            (*states).nbstates = 0;
            (*states).states = xml_malloc(4 * size_of::<XmlStepState>()) as _;
        } else if (*states).maxstates <= (*states).nbstates {
            let tmp: *mut XmlStepState = xml_realloc(
                (*states).states as _,
                2 * (*states).maxstates as usize * size_of::<XmlStepState>(),
            ) as XmlStepStatePtr;
            if tmp.is_null() {
                return -1;
            }
            (*states).states = tmp;
            (*states).maxstates *= 2;
        }
        (*(*states).states.add((*states).nbstates as usize)).step = step;
        (*(*states).states.add((*states).nbstates as usize)).node = node;
        (*states).nbstates += 1;
        0
    }
}

/// Test whether the node matches the pattern
///
/// Returns 1 if it matches, 0 if it doesn't and -1 in case of failure
#[doc(alias = "xmlPatMatch")]
unsafe fn xml_pat_match(comp: XmlPatternPtr, mut node: XmlGenericNodePtr) -> i32 {
    unsafe {
        let mut states: XmlStepStates = XmlStepStates {
            nbstates: 0,
            maxstates: 0,
            states: null_mut(),
        };
        // // may require backtrack

        if comp.is_null() {
            return -1;
        }
        let mut i = 0;
        // restart:
        loop {
            'rollback: {
                'found: while i < (*comp).steps.len() {
                    'to_continue: {
                        let mut step = &(*comp).steps[i];
                        match step.op {
                            XmlPatOp::XmlOpEnd => {
                                break 'found;
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
                                    break 'to_continue;
                                }
                                break 'rollback;
                            }
                            XmlPatOp::XmlOpElem => {
                                if !matches!(node.element_type(), XmlElementType::XmlElementNode) {
                                    break 'rollback;
                                }
                                let node = XmlNodePtr::try_from(node).unwrap();
                                let Some(value) = step.value.as_deref() else {
                                    break 'to_continue;
                                };
                                if Some(value) != node.name().as_deref() {
                                    break 'rollback;
                                }

                                // Namespace test
                                if let Some(ns) = node.ns {
                                    if !ns.href.is_null()
                                        && step.value2.as_deref() != ns.href().as_deref()
                                    {
                                        break 'rollback;
                                    }
                                } else if step.value2.is_some() {
                                    break 'rollback;
                                }
                                break 'to_continue;
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
                                        if matches!(
                                            now.element_type(),
                                            XmlElementType::XmlElementNode
                                        ) {
                                            let now = XmlNodePtr::try_from(now).unwrap();
                                            if Some(value) == now.name().as_deref() {
                                                break;
                                            }
                                        }
                                        lst = now.next();
                                    }
                                    if lst.is_some() {
                                        break 'to_continue;
                                    }
                                }
                                break 'rollback;
                            }
                            XmlPatOp::XmlOpAttr => {
                                if !matches!(node.element_type(), XmlElementType::XmlAttributeNode)
                                {
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
                                break 'to_continue;
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
                                    break 'to_continue;
                                };
                                // Is is correct ??????
                                let node = XmlNodePtr::try_from(node).unwrap();
                                if Some(value) != node.name().as_deref() {
                                    break 'rollback;
                                }
                                // Namespace test
                                if let Some(ns) = node.ns {
                                    if !ns.href.is_null()
                                        && step.value2.as_deref() != ns.href().as_deref()
                                    {
                                        break 'rollback;
                                    }
                                } else if step.value2.is_some() {
                                    break 'rollback;
                                }
                                break 'to_continue;
                            }
                            XmlPatOp::XmlOpAncestor => {
                                // TODO: implement coalescing of ANCESTOR/NODE ops
                                if step.value.is_none() {
                                    i += 1;
                                    step = &(*comp).steps[i];
                                    if matches!(step.op, XmlPatOp::XmlOpRoot) {
                                        break 'found;
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
                                    if matches!(
                                        cur_node.element_type(),
                                        XmlElementType::XmlElementNode
                                    ) {
                                        let node = XmlNodePtr::try_from(cur_node).unwrap();

                                        if step.value.as_deref() == node.name().as_deref() {
                                            // Namespace test
                                            if let Some(ns) = node.ns {
                                                if !ns.href.is_null()
                                                    && step.value2.as_deref()
                                                        == ns.href().as_deref()
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
                                    xml_pat_push_state(addr_of_mut!(states), i as i32, node);
                                } else {
                                    xml_pat_push_state(addr_of_mut!(states), i as i32 - 1, node);
                                }
                                break 'to_continue;
                            }
                            XmlPatOp::XmlOpNs => {
                                if !matches!(node.element_type(), XmlElementType::XmlElementNode) {
                                    break 'rollback;
                                }
                                let node = XmlNodePtr::try_from(node).unwrap();
                                if let Some(ns) = node.ns {
                                    if !ns.href.is_null()
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
                    }
                    i += 1;
                }
                // found:
                if !states.states.is_null() {
                    /* Free the rollback states */
                    xml_free(states.states as _);
                }
                return 1;
            }
            // rollback:
            /* got an error try to rollback */
            if states.states.is_null() {
                return 0;
            }
            if states.nbstates <= 0 {
                xml_free(states.states as _);
                return 0;
            }
            states.nbstates -= 1;
            i = (*states.states.add(states.nbstates as usize)).step as usize;
            node = (*states.states.add(states.nbstates as usize)).node;
            // goto restart;
        }
    }
}

/// Test whether the node matches the pattern
///
/// Returns 1 if it matches, 0 if it doesn't and -1 in case of failure
#[doc(alias = "xmlPatternMatch")]
pub unsafe fn xml_pattern_match(mut comp: XmlPatternPtr, node: XmlGenericNodePtr) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        if comp.is_null() {
            return -1;
        }

        while !comp.is_null() {
            ret = xml_pat_match(comp, node);
            if ret != 0 {
                return ret;
            }
            comp = (*comp).next;
        }
        ret
    }
}

// streaming interfaces
pub type XmlStreamCtxtPtr = *mut XmlStreamCtxt;
#[repr(C)]
pub struct XmlStreamCtxt {
    next: *mut XmlStreamCtxt, /* link to next sub pattern if | */
    comp: XmlStreamCompPtr,   /* the compiled stream */
    nb_state: i32,            /* number of states in the automata */
    max_state: i32,           /* allocated number of states */
    level: i32,               /* how deep are we ? */
    states: *mut i32,         /* the array of step indexes */
    flags: i32,               /* validation options */
    block_level: i32,
}

/// Check if the pattern is streamable i.e. xmlPatternGetStreamCtxt() should work.
///
/// Returns 1 if streamable, 0 if not and -1 in case of error.
#[doc(alias = "xmlPatternStreamable")]
pub unsafe fn xml_pattern_streamable(mut comp: XmlPatternPtr) -> i32 {
    unsafe {
        if comp.is_null() {
            return -1;
        }
        while !comp.is_null() {
            if (*comp).stream.is_null() {
                return 0;
            }
            comp = (*comp).next;
        }
        1
    }
}

/// Check the maximum depth reachable by a pattern
///
/// Returns -2 if no limit (using //), otherwise the depth, and -1 in case of error
#[doc(alias = "xmlPatternMaxDepth")]
pub unsafe fn xml_pattern_max_depth(mut comp: XmlPatternPtr) -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        if comp.is_null() {
            return -1;
        }
        while !comp.is_null() {
            if (*comp).stream.is_null() {
                return -1;
            }

            for i in 0..(*(*comp).stream).steps.len() {
                if (*(*comp).stream).steps[i].flags & XML_STREAM_STEP_DESC as i32 != 0 {
                    return -2;
                }
            }
            if (*(*comp).stream).steps.len() as i32 > ret {
                ret = (*(*comp).stream).steps.len() as i32;
            }
            comp = (*comp).next;
        }
        ret
    }
}

/// Check the minimum depth reachable by a pattern, 0 mean the / or . are part of the set.
///
/// Returns -1 in case of error otherwise the depth,
#[doc(alias = "xmlPatternMinDepth")]
pub unsafe fn xml_pattern_min_depth(mut comp: XmlPatternPtr) -> i32 {
    unsafe {
        let mut ret: i32 = 12345678;
        if comp.is_null() {
            return -1;
        }
        while !comp.is_null() {
            if (*comp).stream.is_null() {
                return -1;
            }
            if (*(*comp).stream).steps.len() < ret as usize {
                ret = (*(*comp).stream).steps.len() as i32;
            }
            if ret == 0 {
                return 0;
            }
            comp = (*comp).next;
        }
        ret
    }
}

/// Check if the pattern must be looked at from the root.
///
/// Returns 1 if true, 0 if false and -1 in case of error
#[doc(alias = "xmlPatternFromRoot")]
pub unsafe fn xml_pattern_from_root(mut comp: XmlPatternPtr) -> i32 {
    unsafe {
        if comp.is_null() {
            return -1;
        }
        while !comp.is_null() {
            if (*comp).stream.is_null() {
                return -1;
            }
            if (*comp).flags & PAT_FROM_ROOT as i32 != 0 {
                return 1;
            }
            comp = (*comp).next;
        }
        0
    }
}

/// Build a new stream context
///
/// Returns the new structure or NULL in case of error.
#[doc(alias = "xmlNewStreamCtxt")]
unsafe fn xml_new_stream_ctxt(stream: XmlStreamCompPtr) -> XmlStreamCtxtPtr {
    unsafe {
        let cur: XmlStreamCtxtPtr = xml_malloc(size_of::<XmlStreamCtxt>()) as XmlStreamCtxtPtr;
        if cur.is_null() {
            return null_mut();
        }
        memset(cur as _, 0, size_of::<XmlStreamCtxt>());
        (*cur).states = xml_malloc(4 * 2 * size_of::<i32>()) as *mut i32;
        if (*cur).states.is_null() {
            xml_free(cur as _);
            return null_mut();
        }
        (*cur).nb_state = 0;
        (*cur).max_state = 4;
        (*cur).level = 0;
        (*cur).comp = stream;
        (*cur).block_level = -1;
        cur
    }
}

/// Get a streaming context for that pattern
/// Use xmlFreeStreamCtxt to free the context.
///
/// Returns a pointer to the context or NULL in case of failure
#[doc(alias = "xmlPatternGetStreamCtxt")]
pub unsafe fn xml_pattern_get_stream_ctxt(mut comp: XmlPatternPtr) -> XmlStreamCtxtPtr {
    unsafe {
        let mut ret: XmlStreamCtxtPtr = null_mut();
        let mut cur: XmlStreamCtxtPtr;

        if comp.is_null() || (*comp).stream.is_null() {
            return null_mut();
        }

        while !comp.is_null() {
            if (*comp).stream.is_null() {
                // goto failed;
                xml_free_stream_ctxt(ret);
                return null_mut();
            }
            cur = xml_new_stream_ctxt((*comp).stream);
            if cur.is_null() {
                // goto failed;
                xml_free_stream_ctxt(ret);
                return null_mut();
            }
            if ret.is_null() {
                ret = cur;
            } else {
                (*cur).next = (*ret).next;
                (*ret).next = cur;
            }
            (*cur).flags = (*comp).flags;
            comp = (*comp).next;
        }
        ret
        // failed:
        // xmlFreeStreamCtxt(ret);
        // return null_mut();
    }
}

/// Free the stream context
#[doc(alias = "xmlFreeStreamCtxt")]
pub unsafe fn xml_free_stream_ctxt(mut stream: XmlStreamCtxtPtr) {
    unsafe {
        let mut next: XmlStreamCtxtPtr;

        while !stream.is_null() {
            next = (*stream).next;
            if !(*stream).states.is_null() {
                xml_free((*stream).states as _);
            }
            xml_free(stream as _);
            stream = next;
        }
    }
}

/// Add a new state to the stream context
///
/// Returns -1 in case of error or the state index if successful
#[doc(alias = "xmlStreamCtxtAddState")]
unsafe fn xml_stream_ctxt_add_state(comp: XmlStreamCtxtPtr, idx: i32, level: i32) -> i32 {
    unsafe {
        for i in 0..(*comp).nb_state {
            if *(*comp).states.add(2 * i as usize) < 0 {
                *(*comp).states.add(2 * i as usize) = idx;
                *(*comp).states.add(2 * i as usize + 1) = level;
                return i;
            }
        }
        if (*comp).nb_state >= (*comp).max_state {
            let cur: *mut i32 = xml_realloc(
                (*comp).states as _,
                (*comp).max_state as usize * 4 * size_of::<i32>(),
            ) as *mut i32;
            if cur.is_null() {
                return -1;
            }
            (*comp).states = cur;
            (*comp).max_state *= 2;
        }
        *(*comp).states.add(2 * (*comp).nb_state as usize) = idx;
        *(*comp).states.add(2 * (*comp).nb_state as usize + 1) = level;
        (*comp).nb_state += 1;
        (*comp).nb_state - 1
    }
}

/// Push new data onto the stream. NOTE: if the call xmlPatterncompile()
/// indicated a dictionary, then strings for name and ns will be expected
/// to come from the dictionary.
/// Both @name and @ns being NULL means the / i.e. the root of the document.
/// This can also act as a reset.
///
/// Returns: -1 in case of error, 1 if the current state in the stream is a is_match and 0 otherwise.
#[doc(alias = "xmlStreamPushInternal")]
unsafe fn xml_stream_push_internal(
    mut stream: XmlStreamCtxtPtr,
    name: *const XmlChar,
    ns: *const XmlChar,
    node_type: i32,
) -> i32 {
    unsafe {
        let mut ret: i32 = 0;
        let mut err: i32 = 0;
        let mut is_final: i32 = 0;
        let mut tmp: i32;
        let mut i: i32;
        let mut m: i32;
        let mut is_match: i32;
        let mut step_nr: i32;
        let mut desc: i32;
        let mut comp: XmlStreamCompPtr;

        if stream.is_null() || (*stream).nb_state < 0 {
            return -1;
        }

        'stream: while !stream.is_null() {
            'stream_next: {
                comp = (*stream).comp;

                if node_type == XmlElementType::XmlElementNode as i32
                    && name.is_null()
                    && ns.is_null()
                {
                    // We have a document node here (or a reset).
                    (*stream).nb_state = 0;
                    (*stream).level = 0;
                    (*stream).block_level = -1;
                    if (*comp).flags & XML_STREAM_FROM_ROOT as i32 != 0 {
                        if (*comp).steps.is_empty() {
                            // TODO: We have a "/." here?
                            ret = 1;
                        } else if (*comp).steps.len() == 1
                            && (*comp).steps[0].node_type == XML_STREAM_ANY_NODE as i32
                            && (*comp).steps[0].flags & XML_STREAM_STEP_DESC as i32 != 0
                        {
                            // In the case of "//." the document node will is_match as well.
                            ret = 1;
                        } else if (*comp).steps[0].flags & XML_STREAM_STEP_ROOT as i32 != 0 {
                            // TODO: Do we need this ?
                            tmp = xml_stream_ctxt_add_state(stream, 0, 0);
                            if tmp < 0 {
                                err += 1;
                            }
                        }
                    }
                    stream = (*stream).next;
                    continue 'stream;
                }

                // Fast check for ".".
                if (*comp).steps.is_empty() {
                    // / and . are handled at the XPath node set creation
                    // level by checking min depth
                    if (*stream).flags & XmlPatternFlags::XmlPatternXPath as i32 != 0 {
                        stream = (*stream).next;
                        continue 'stream; /* while */
                    }
                    // For non-pattern like evaluation like XML Schema IDCs
                    // or traditional XPath expressions, this will is_match if
                    // we are at the first level only, otherwise on every level.
                    if node_type != XmlElementType::XmlAttributeNode as i32
                        && ((*stream).flags & XML_PATTERN_NOTPATTERN == 0 || (*stream).level == 0)
                    {
                        ret = 1;
                    }
                    (*stream).level += 1;
                    break 'stream_next;
                }
                if (*stream).block_level != -1 {
                    // Skip blocked expressions.
                    (*stream).level += 1;
                    break 'stream_next;
                }

                if node_type != XmlElementType::XmlElementNode as i32
                    && node_type != XmlElementType::XmlAttributeNode as i32
                    && (*comp).flags & XML_STREAM_FINAL_IS_ANY_NODE as i32 == 0
                {
                    // No need to process nodes of other types if we don't
                    // resolve to those types.
                    // TODO: Do we need to block the context here?
                    (*stream).level += 1;
                    break 'stream_next;
                }

                // Check evolution of existing states
                i = 0;
                m = (*stream).nb_state;
                while i < m {
                    'next_state: {
                        if (*comp).flags & XML_STREAM_DESC as i32 == 0 {
                            // If there is no "//", then only the last
                            // added state is of interest.
                            step_nr = *(*stream).states.add(2 * ((*stream).nb_state - 1) as usize);
                            // TODO: Security check, should not happen, remove it.
                            if *(*stream)
                                .states
                                .add((2 * ((*stream).nb_state - 1) as usize) + 1)
                                < (*stream).level
                            {
                                return -1;
                            }
                            // desc = 0;
                            // loop-stopper
                            i = m;
                        } else {
                            // If there are "//", then we need to process every "//"
                            // occurring in the states, plus any other state for this level.
                            step_nr = *(*stream).states.add(2 * i as usize);

                            // TODO: should not happen anymore: dead states
                            if step_nr < 0 {
                                break 'next_state;
                            }

                            tmp = *(*stream).states.add((2 * i) as usize + 1);

                            // skip new states just added
                            if tmp > (*stream).level {
                                break 'next_state;
                            }

                            // skip states at ancestor levels, except if "//"
                            desc =
                                (*comp).steps[step_nr as usize].flags & XML_STREAM_STEP_DESC as i32;
                            if tmp < (*stream).level && desc == 0 {
                                break 'next_state;
                            }
                        }
                        // Check for correct node-type.
                        let step = &(*comp).steps[step_nr as usize];
                        if step.node_type != node_type {
                            if step.node_type == XmlElementType::XmlAttributeNode as i32 {
                                // Block this expression for deeper evaluation.
                                if (*comp).flags & XML_STREAM_DESC as i32 == 0 {
                                    (*stream).block_level = (*stream).level + 1;
                                }
                                break 'next_state;
                            } else if step.node_type != XML_STREAM_ANY_NODE as i32 {
                                break 'next_state;
                            }
                        }
                        // Compare local/namespace-name.
                        is_match = 0;
                        if step.node_type == XML_STREAM_ANY_NODE as i32 {
                            is_match = 1;
                        } else if step.name.is_none() {
                            if step.ns.is_none() {
                                // This lets through all elements/attributes.
                                is_match = 1;
                            } else if !ns.is_null() {
                                is_match = (step.ns.as_deref()
                                    == Some(
                                        CStr::from_ptr(ns as *const i8).to_string_lossy().as_ref(),
                                    )) as i32;
                            }
                        } else if step.ns.is_none() == ns.is_null()
                            && !name.is_null()
                            && step.name.as_deref()
                                == Some(
                                    CStr::from_ptr(name as *const i8).to_string_lossy().as_ref(),
                                )
                            && step.ns.as_deref()
                                == (!ns.is_null())
                                    .then(|| CStr::from_ptr(ns as *const i8).to_string_lossy())
                                    .as_deref()
                        {
                            is_match = 1;
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
                        if is_match != 0 {
                            is_final = step.flags & XML_STREAM_STEP_FINAL as i32;
                            if is_final != 0 {
                                ret = 1;
                            } else {
                                xml_stream_ctxt_add_state(stream, step_nr + 1, (*stream).level + 1);
                            }
                            if ret != 1 && step.flags & XML_STREAM_STEP_IN_SET as i32 != 0 {
                                // Check if we have a special case like "foo/bar//.", where
                                // "foo" is selected as well.
                                ret = 1;
                            }
                        }
                        if (*comp).flags & XML_STREAM_DESC as i32 == 0
                            && (is_match == 0 || is_final != 0)
                        {
                            // Mark this expression as blocked for any evaluation at
                            // deeper levels. Note that this includes "/foo"
                            // expressions if the *pattern* behaviour is used.
                            (*stream).block_level = (*stream).level + 1;
                        }
                    }
                    // next_state:
                    i += 1;
                }

                (*stream).level += 1;

                // Re/enter the expression.
                // Don't reenter if it's an absolute expression like "/foo",
                //   except "//foo".
                let step = &(*comp).steps[0];
                if step.flags & XML_STREAM_STEP_ROOT as i32 != 0 {
                    break 'stream_next;
                }

                desc = step.flags & XML_STREAM_STEP_DESC as i32;
                'compare: {
                    if (*stream).flags & XML_PATTERN_NOTPATTERN != 0 {
                        // Re/enter the expression if it is a "descendant" one,
                        // or if we are at the 1st level of evaluation.
                        if (*stream).level == 1 {
                            if XML_STREAM_XS_IDC!(stream) {
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
                        if (*stream).level == 2 && XML_STREAM_XS_IDC!(stream) {
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
                is_match = 0;
                if step.node_type == XML_STREAM_ANY_NODE as i32 {
                    is_match = 1;
                } else if step.name.is_none() {
                    if step.ns.is_none() {
                        // This lets through all elements/attributes.
                        is_match = 1;
                    } else if !ns.is_null() {
                        is_match = (step.ns.as_deref()
                            == Some(CStr::from_ptr(ns as *const i8).to_string_lossy().as_ref()))
                            as i32;
                    }
                } else if step.ns.is_none() == ns.is_null()
                    && !name.is_null()
                    && step.name.as_deref()
                        == Some(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref())
                    && step.ns.as_deref()
                        == (!ns.is_null())
                            .then(|| CStr::from_ptr(ns as *const i8).to_string_lossy())
                            .as_deref()
                {
                    is_match = 1;
                }
                is_final = step.flags & XML_STREAM_STEP_FINAL as i32;
                if is_match != 0 {
                    if is_final != 0 {
                        ret = 1;
                    } else {
                        xml_stream_ctxt_add_state(stream, 1, (*stream).level);
                    }
                    if ret != 1 && step.flags & XML_STREAM_STEP_IN_SET as i32 != 0 {
                        // Check if we have a special case like "foo//.", where
                        // "foo" is selected as well.
                        ret = 1;
                    }
                }
                if (*comp).flags & XML_STREAM_DESC as i32 == 0 && (is_match == 0 || is_final != 0) {
                    // Mark this expression as blocked for any evaluation at
                    // deeper levels.
                    (*stream).block_level = (*stream).level;
                }
            }

            // stream_next:
            stream = (*stream).next;
        } /* while !stream.is_null() */

        if err > 0 {
            ret = -1;
        }
        ret
    }
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
pub unsafe fn xml_stream_push_node(
    stream: XmlStreamCtxtPtr,
    name: *const XmlChar,
    ns: *const XmlChar,
    node_type: i32,
) -> i32 {
    unsafe { xml_stream_push_internal(stream, name, ns, node_type) }
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
pub unsafe fn xml_stream_push(
    stream: XmlStreamCtxtPtr,
    name: *const XmlChar,
    ns: *const XmlChar,
) -> i32 {
    unsafe { xml_stream_push_internal(stream, name, ns, XmlElementType::XmlElementNode as i32) }
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
pub unsafe fn xml_stream_push_attr(
    stream: XmlStreamCtxtPtr,
    name: *const XmlChar,
    ns: *const XmlChar,
) -> i32 {
    unsafe { xml_stream_push_internal(stream, name, ns, XmlElementType::XmlAttributeNode as i32) }
}

/// Push one level from the stream.
///
/// Returns: -1 in case of error, 0 otherwise.
#[doc(alias = "xmlStreamPop")]
pub unsafe fn xml_stream_pop(mut stream: XmlStreamCtxtPtr) -> i32 {
    unsafe {
        let mut lev: i32;

        if stream.is_null() {
            return -1;
        }
        while !stream.is_null() {
            // Reset block-level.
            if (*stream).block_level == (*stream).level {
                (*stream).block_level = -1;
            }

            //  (*stream).level can be zero when XML_FINAL_IS_ANY_NODE is set
            //  (see the thread at
            //  http://mail.gnome.org/archives/xslt/2008-July/msg00027.html)
            if (*stream).level != 0 {
                (*stream).level -= 1;
            }
            // Check evolution of existing states
            for i in (0..(*stream).nb_state).rev() {
                // discard obsoleted states
                lev = *(*stream).states.add((2 * i) as usize + 1);
                if lev > (*stream).level {
                    (*stream).nb_state -= 1;
                }
                if lev <= (*stream).level {
                    break;
                }
            }
            stream = (*stream).next;
        }
        0
    }
}

/// Query if the streaming pattern additionally needs to be fed with
/// text-, cdata-section-, comment- and processing-instruction-nodes.
/// If the result is 0 then only element-nodes and attribute-nodes
/// need to be pushed.
///
/// Returns 1 in case of need of nodes of the above described types,
/// 0 otherwise. -1 on API errors.
#[doc(alias = "xmlStreamWantsAnyNode")]
pub unsafe fn xml_stream_wants_any_node(mut stream: XmlStreamCtxtPtr) -> i32 {
    unsafe {
        if stream.is_null() {
            return -1;
        }
        while !stream.is_null() {
            if (*(*stream).comp).flags & XML_STREAM_FINAL_IS_ANY_NODE as i32 != 0 {
                return 1;
            }
            stream = (*stream).next;
        }
        0
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_pattern_from_root() {
        #[cfg(feature = "libxml_pattern")]
        unsafe {
            let mut leaks = 0;

            for n_comp in 0..GEN_NB_XML_PATTERN_PTR {
                let mem_base = xml_mem_blocks();
                let comp = gen_xml_pattern_ptr(n_comp, 0);

                let ret_val = xml_pattern_from_root(comp);
                desret_int(ret_val);
                des_xml_pattern_ptr(n_comp, comp, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlPatternFromRoot",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlPatternFromRoot()"
                    );
                    eprintln!(" {}", n_comp);
                }
            }
        }
    }

    #[test]
    fn test_xml_pattern_max_depth() {
        #[cfg(feature = "libxml_pattern")]
        unsafe {
            let mut leaks = 0;

            for n_comp in 0..GEN_NB_XML_PATTERN_PTR {
                let mem_base = xml_mem_blocks();
                let comp = gen_xml_pattern_ptr(n_comp, 0);

                let ret_val = xml_pattern_max_depth(comp);
                desret_int(ret_val);
                des_xml_pattern_ptr(n_comp, comp, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlPatternMaxDepth",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlPatternMaxDepth()"
                    );
                    eprintln!(" {}", n_comp);
                }
            }
        }
    }

    #[test]
    fn test_xml_pattern_min_depth() {
        #[cfg(feature = "libxml_pattern")]
        unsafe {
            let mut leaks = 0;

            for n_comp in 0..GEN_NB_XML_PATTERN_PTR {
                let mem_base = xml_mem_blocks();
                let comp = gen_xml_pattern_ptr(n_comp, 0);

                let ret_val = xml_pattern_min_depth(comp);
                desret_int(ret_val);
                des_xml_pattern_ptr(n_comp, comp, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlPatternMinDepth",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlPatternMinDepth()"
                    );
                    eprintln!(" {}", n_comp);
                }
            }
        }
    }

    #[test]
    fn test_xml_pattern_streamable() {
        #[cfg(feature = "libxml_pattern")]
        unsafe {
            let mut leaks = 0;

            for n_comp in 0..GEN_NB_XML_PATTERN_PTR {
                let mem_base = xml_mem_blocks();
                let comp = gen_xml_pattern_ptr(n_comp, 0);

                let ret_val = xml_pattern_streamable(comp);
                desret_int(ret_val);
                des_xml_pattern_ptr(n_comp, comp, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlPatternStreamable",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlPatternStreamable()"
                    );
                    eprintln!(" {}", n_comp);
                }
            }
        }
    }

    #[test]
    fn test_xml_patterncompile() {

        /* missing type support */
    }

    #[test]
    fn test_xml_stream_pop() {
        #[cfg(feature = "libxml_pattern")]
        unsafe {
            let mut leaks = 0;

            for n_stream in 0..GEN_NB_XML_STREAM_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let stream = gen_xml_stream_ctxt_ptr(n_stream, 0);

                let ret_val = xml_stream_pop(stream);
                desret_int(ret_val);
                des_xml_stream_ctxt_ptr(n_stream, stream, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlStreamPop",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlStreamPop()");
                    eprintln!(" {}", n_stream);
                }
            }
        }
    }

    #[test]
    fn test_xml_stream_push() {
        #[cfg(feature = "libxml_pattern")]
        unsafe {
            let mut leaks = 0;

            for n_stream in 0..GEN_NB_XML_STREAM_CTXT_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_ns in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let stream = gen_xml_stream_ctxt_ptr(n_stream, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let ns = gen_const_xml_char_ptr(n_ns, 2);

                        let ret_val = xml_stream_push(stream, name, ns);
                        desret_int(ret_val);
                        des_xml_stream_ctxt_ptr(n_stream, stream, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_ns, ns, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlStreamPush",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlStreamPush()");
                            eprint!(" {}", n_stream);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_ns);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_stream_push_attr() {
        #[cfg(feature = "libxml_pattern")]
        unsafe {
            let mut leaks = 0;

            for n_stream in 0..GEN_NB_XML_STREAM_CTXT_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_ns in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let stream = gen_xml_stream_ctxt_ptr(n_stream, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let ns = gen_const_xml_char_ptr(n_ns, 2);

                        let ret_val = xml_stream_push_attr(stream, name, ns);
                        desret_int(ret_val);
                        des_xml_stream_ctxt_ptr(n_stream, stream, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_ns, ns, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlStreamPushAttr",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlStreamPushAttr()");
                            eprint!(" {}", n_stream);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_ns);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_stream_push_node() {
        #[cfg(feature = "libxml_pattern")]
        unsafe {
            let mut leaks = 0;

            for n_stream in 0..GEN_NB_XML_STREAM_CTXT_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_ns in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_node_type in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let stream = gen_xml_stream_ctxt_ptr(n_stream, 0);
                            let name = gen_const_xml_char_ptr(n_name, 1);
                            let ns = gen_const_xml_char_ptr(n_ns, 2);
                            let node_type = gen_int(n_node_type, 3);

                            let ret_val = xml_stream_push_node(stream, name, ns, node_type);
                            desret_int(ret_val);
                            des_xml_stream_ctxt_ptr(n_stream, stream, 0);
                            des_const_xml_char_ptr(n_name, name, 1);
                            des_const_xml_char_ptr(n_ns, ns, 2);
                            des_int(n_node_type, node_type, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlStreamPushNode",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlStreamPushNode()"
                                );
                                eprint!(" {}", n_stream);
                                eprint!(" {}", n_name);
                                eprint!(" {}", n_ns);
                                eprintln!(" {}", n_node_type);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_stream_wants_any_node() {
        #[cfg(feature = "libxml_pattern")]
        unsafe {
            let mut leaks = 0;

            for n_stream_ctxt in 0..GEN_NB_XML_STREAM_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let stream_ctxt = gen_xml_stream_ctxt_ptr(n_stream_ctxt, 0);

                let ret_val = xml_stream_wants_any_node(stream_ctxt);
                desret_int(ret_val);
                des_xml_stream_ctxt_ptr(n_stream_ctxt, stream_ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlStreamWantsAnyNode",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlStreamWantsAnyNode()"
                    );
                    eprintln!(" {}", n_stream_ctxt);
                }
            }
        }
    }
}

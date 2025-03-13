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
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
};

use libc::memset;

use crate::tree::XmlAttrPtr;
use crate::{
    libxml::{
        chvalid::{xml_is_blank_char, xml_is_combining, xml_is_digit, xml_is_extender},
        globals::{xml_free, xml_malloc, xml_realloc},
        parser_internals::{xml_is_letter, xml_string_current_char},
        xmlstring::{XmlChar, xml_str_equal, xml_strdup, xml_strndup},
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
#[derive(Debug, Clone, Copy)]
pub struct XmlStepOp {
    op: XmlPatOp,
    value: *const XmlChar,
    value2: *const XmlChar, /* The namespace name */
}

pub type XmlStreamStepPtr = *mut XmlStreamStep;
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct XmlStreamStep {
    flags: i32,           /* properties of that step */
    name: *const XmlChar, /* first string value if NULL accept all */
    ns: *const XmlChar,   /* second string value */
    node_type: i32,       /* type of node */
}

pub type XmlStreamCompPtr = *mut XmlStreamComp;
#[repr(C)]
pub struct XmlStreamComp {
    nb_step: i32,            /* number of steps in the automata */
    max_step: i32,           /* allocated number of steps */
    steps: XmlStreamStepPtr, /* the array of steps */
    flags: i32,
}

/// A compiled (XPath based) pattern to select nodes
#[doc(alias = "xmlPattern")]
pub type XmlPatternPtr = *mut XmlPattern;
#[repr(C)]
pub struct XmlPattern {
    data: *mut c_void,       /* the associated template */
    next: *mut XmlPattern,   /* next pattern if | is used */
    pattern: *const XmlChar, /* the pattern */
    flags: i32,              /* flags */
    nb_step: i32,
    max_step: i32,
    steps: XmlStepOpPtr,      /* ops for computation */
    stream: XmlStreamCompPtr, /* the streaming data if any */
}

// XML_STREAM_ANY_NODE is used for comparison against
// xmlElementType enums, to indicate a node of any type.
const XML_STREAM_ANY_NODE: usize = 100;

/// This is the set of options affecting the behaviour of pattern matching with this module
#[doc(alias = "xmlPatternFlags")]
#[repr(C)]
pub enum XmlPatternFlags {
    XmlPatternDefault = 0,      /* simple pattern match */
    XmlPatternXpath = 1 << 0,   /* standard XPath pattern */
    XmlPatternXssel = 1 << 1,   /* XPath subset for schema selector */
    XmlPatternXsfield = 1 << 2, /* XPath subset for schema field */
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
            if !(*comp).steps.is_null() {
                xml_free((*comp).steps as _);
            }
            xml_free(comp as _);
        }
    }
}

unsafe fn xml_free_pattern_internal(comp: XmlPatternPtr) {
    unsafe {
        let mut op: XmlStepOpPtr;

        if comp.is_null() {
            return;
        }
        if !(*comp).stream.is_null() {
            xml_free_stream_comp((*comp).stream);
        }
        if !(*comp).pattern.is_null() {
            xml_free((*comp).pattern as _);
        }
        if !(*comp).steps.is_null() {
            for i in 0..(*comp).nb_step {
                op = (*comp).steps.add(i as usize);
                if !(*op).value.is_null() {
                    xml_free((*op).value as _);
                }
                if !(*op).value2.is_null() {
                    xml_free((*op).value2 as _);
                }
            }
            xml_free((*comp).steps as _);
        }

        memset(comp as _, -1, size_of::<XmlPattern>());
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
            & (XmlPatternFlags::XmlPatternXssel as i32 | XmlPatternFlags::XmlPatternXsfield as i32)
            != 0
    };
}

macro_rules! CUR {
    ($ctxt:expr) => {
        *(*$ctxt).cur
    };
}

macro_rules! NEXT {
    ($ctxt:expr) => {
        if *(*$ctxt).cur != 0 {
            let res = (*$ctxt).cur;
            (*$ctxt).cur = (*$ctxt).cur.add(1);
            res
        } else {
            (*$ctxt).cur
        }
    };
}

macro_rules! SKIP_BLANKS {
    ($ctxt:expr) => {
        while xml_is_blank_char(CUR!($ctxt) as u32) {
            NEXT!($ctxt);
        }
    };
}

macro_rules! PUSH {
    ($ctxt:expr, $op:expr, $val:expr, $val2:expr, $error:tt) => {
        if xml_pattern_add($ctxt, (*$ctxt).comp, $op, $val, $val2) != 0 {
            break $error;
        }
    };
}

macro_rules! PEEKPREV {
    ($ctxt:expr, $val:expr) => {{
        assert!($val >= 0);
        *(*$ctxt).cur.sub($val as usize)
    }};
}

macro_rules! XML_STREAM_XS_IDC_SEL {
    ($c:expr) => {
        (*$c).flags & XmlPatternFlags::XmlPatternXssel as i32 != 0
    };
}

macro_rules! CUR_PTR {
    ($ctxt:expr) => {
        (*$ctxt).cur
    };
}

macro_rules! NXT {
    ($ctxt:expr, $val:expr) => {
        *(*$ctxt).cur.add($val as usize)
    };
}

pub type XmlPatParserContextPtr = *mut XmlPatParserContext;
#[doc(alias = "xmlPatParserContext")]
#[repr(C)]
pub struct XmlPatParserContext {
    cur: *const XmlChar,      /* the current char being parsed */
    base: *const XmlChar,     /* the full expression */
    error: i32,               /* error code */
    comp: XmlPatternPtr,      /* the result */
    elem: Option<XmlNodePtr>, /* the current node if any */
    namespaces: Option<Vec<(*const u8, *const u8)>>, /* the namespaces definitions */
}

impl XmlPatParserContext {
    /// Create a new XML pattern parser context
    ///
    /// Returns the newly allocated xmlPatParserContextPtr or NULL in case of error
    #[doc(alias = "xmlNewPatParserContext")]
    fn new(pattern: *const u8, namespaces: Option<Vec<(*const u8, *const u8)>>) -> Option<Self> {
        (!pattern.is_null()).then(|| XmlPatParserContext {
            cur: pattern,
            base: pattern,
            namespaces,
            ..Default::default()
        })
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
            SKIP_BLANKS!(self);
            'error_unfinished: {
                'error: {
                    if CUR!(self) == b'/' {
                        break 'error;
                    }
                    (*self.comp).flags |= PAT_FROM_CUR as i32;

                    if CUR!(self) == b'.' {
                        // "." - "self::node()"
                        NEXT!(self);
                        SKIP_BLANKS!(self);
                        if CUR!(self) == 0 {
                            // Selection of the context node.
                            PUSH!(self, XmlPatOp::XmlOpElem, null_mut(), null_mut(), 'error);
                            return;
                        }
                        if CUR!(self) != b'/' {
                            // TODO: A more meaningful error message.
                            break 'error;
                        }
                        // "./" - "self::node()/"
                        NEXT!(self);
                        SKIP_BLANKS!(self);
                        if CUR!(self) == b'/' {
                            if xml_is_blank_char(PEEKPREV!(self, 1) as u32) {
                                // Disallow "./ /"
                                break 'error;
                            }
                            // ".//" - "self:node()/descendant-or-self::node()/"
                            PUSH!(self, XmlPatOp::XmlOpAncestor, null_mut(), null_mut(), 'error);
                            NEXT!(self);
                            SKIP_BLANKS!(self);
                        }
                        if CUR!(self) == 0 {
                            break 'error_unfinished;
                        }
                    }
                    // Process steps.
                    'b: while {
                        xml_compile_step_pattern(self);
                        if self.error != 0 {
                            break 'error;
                        }
                        SKIP_BLANKS!(self);
                        if CUR!(self) != b'/' {
                            break 'b;
                        }
                        PUSH!(self, XmlPatOp::XmlOpParent, null_mut(), null_mut(), 'error);
                        NEXT!(self);
                        SKIP_BLANKS!(self);
                        if CUR!(self) == b'/' {
                            // Disallow subsequent '//'.
                            break 'error;
                        }
                        if CUR!(self) == 0 {
                            break 'error_unfinished;
                        }

                        CUR!(self) != 0
                    } {}

                    if CUR!(self) != 0 {
                        self.error = 1;
                    }
                    return;
                }
                // error:
                self.error = 1;
                return;
            }

            // error_unfinished:
            self.error = 1;
        }
    }

    /// Compile the Path Pattern and generates a precompiled
    /// form suitable for fast matching.
    ///
    /// `[5]    Path    ::=    ('.//')? ( Step '/' )* ( Step | '@' NameTest )`
    #[doc(alias = "xmlCompilePathPattern")]
    unsafe fn compile_path_pattern(&mut self) {
        unsafe {
            SKIP_BLANKS!(self);
            if CUR!(self) == b'/' {
                (*self.comp).flags |= PAT_FROM_ROOT as i32;
            } else if CUR!(self) == b'.' || (*self.comp).flags & XML_PATTERN_NOTPATTERN != 0 {
                (*self.comp).flags |= PAT_FROM_CUR as i32;
            }

            'error: {
                if CUR!(self) == b'/' && NXT!(self, 1) == b'/' {
                    PUSH!(self, XmlPatOp::XmlOpAncestor, null_mut(), null_mut(), 'error);
                    NEXT!(self);
                    NEXT!(self);
                } else if CUR!(self) == b'.' && NXT!(self, 1) == b'/' && NXT!(self, 2) == b'/' {
                    PUSH!(self, XmlPatOp::XmlOpAncestor, null_mut(), null_mut(), 'error);
                    NEXT!(self);
                    NEXT!(self);
                    NEXT!(self);
                    // Check for incompleteness.
                    SKIP_BLANKS!(self);
                    if CUR!(self) == 0 {
                        self.error = 1;
                        break 'error;
                    }
                }
                if CUR!(self) == b'@' {
                    NEXT!(self);
                    xml_compile_attribute_test(self);
                    SKIP_BLANKS!(self);
                    // TODO: check for incompleteness
                    if CUR!(self) != 0 {
                        xml_compile_step_pattern(self);
                        if self.error != 0 {
                            break 'error;
                        }
                    }
                } else {
                    if CUR!(self) == b'/' {
                        PUSH!(self, XmlPatOp::XmlOpRoot, null_mut(), null_mut(), 'error);
                        NEXT!(self);
                        // Check for incompleteness.
                        SKIP_BLANKS!(self);
                        if CUR!(self) == 0 {
                            self.error = 1;
                            break 'error;
                        }
                    }
                    xml_compile_step_pattern(self);
                    if self.error != 0 {
                        break 'error;
                    }
                    SKIP_BLANKS!(self);
                    while CUR!(self) == b'/' {
                        if NXT!(self, 1) == b'/' {
                            PUSH!(self, XmlPatOp::XmlOpAncestor, null_mut(), null_mut(), 'error);
                            NEXT!(self);
                            NEXT!(self);
                            SKIP_BLANKS!(self);
                            xml_compile_step_pattern(self);
                            if self.error != 0 {
                                break 'error;
                            }
                        } else {
                            PUSH!(self, XmlPatOp::XmlOpParent, null_mut(), null_mut(), 'error);
                            NEXT!(self);
                            SKIP_BLANKS!(self);
                            if CUR!(self) == 0 {
                                self.error = 1;
                                break 'error;
                            }
                            xml_compile_step_pattern(self);
                            if self.error != 0 {
                                break 'error;
                            }
                        }
                    }
                }
                if CUR!(self) != 0 {
                    self.error = 1;
                }
            }
            // error:
            // return;
        }
    }
}

impl Default for XmlPatParserContext {
    fn default() -> Self {
        Self {
            cur: null(),
            base: null(),
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
        memset(cur as _, 0, size_of::<XmlPattern>());
        (*cur).max_step = 10;
        (*cur).steps =
            xml_malloc((*cur).max_step as usize * size_of::<XmlStepOp>()) as XmlStepOpPtr;
        if (*cur).steps.is_null() {
            xml_free(cur as _);
            return null_mut();
        }
        cur
    }
}

const PAT_FROM_ROOT: usize = 1 << 8;
const PAT_FROM_CUR: usize = 1 << 9;

/// Add a step to an XSLT Compiled Match
///
/// Returns -1 in case of failure, 0 otherwise.
#[doc(alias = "xmlPatternAdd")]
unsafe fn xml_pattern_add(
    _ctxt: XmlPatParserContextPtr,
    comp: XmlPatternPtr,
    op: XmlPatOp,
    value: *mut XmlChar,
    value2: *mut XmlChar,
) -> i32 {
    unsafe {
        if (*comp).nb_step >= (*comp).max_step {
            let temp: XmlStepOpPtr = xml_realloc(
                (*comp).steps as _,
                (*comp).max_step as usize * 2 * size_of::<XmlStepOp>(),
            ) as XmlStepOpPtr;
            if temp.is_null() {
                return -1;
            }
            (*comp).steps = temp;
            (*comp).max_step *= 2;
        }
        (*(*comp).steps.add((*comp).nb_step as usize)).op = op;
        (*(*comp).steps.add((*comp).nb_step as usize)).value = value;
        (*(*comp).steps.add((*comp).nb_step as usize)).value2 = value2;
        (*comp).nb_step += 1;
        0
    }
}

/// Parses a non qualified name
///
/// Returns the Name parsed or NULL
#[doc(alias = "xmlPatScanNCName")]
unsafe fn xml_pat_scan_ncname(ctxt: XmlPatParserContextPtr) -> *mut XmlChar {
    unsafe {
        let mut cur: *const XmlChar;
        let mut val: i32;
        let mut len: i32 = 0;

        SKIP_BLANKS!(ctxt);

        cur = CUR_PTR!(ctxt);
        let q: *const XmlChar = cur;
        val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
        if !xml_is_letter(val as u32) && val != '_' as i32 {
            return null_mut();
        }

        while xml_is_letter(val as u32)
            || xml_is_digit(val as u32)
            || val == b'.' as i32
            || val == b'-' as i32
            || val == b'_' as i32
            || xml_is_combining(val as u32)
            || xml_is_extender(val as u32)
        {
            cur = cur.add(len as usize);
            val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
        }
        let ret = xml_strndup(q, cur.offset_from(q) as _);
        CUR_PTR!(ctxt) = cur;
        ret
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
unsafe fn xml_pat_scan_name(ctxt: XmlPatParserContextPtr) -> *mut XmlChar {
    unsafe {
        let mut cur: *const XmlChar;
        let mut val: i32;
        let mut len: i32 = 0;

        SKIP_BLANKS!(ctxt);

        cur = CUR_PTR!(ctxt);
        let q: *const XmlChar = cur;
        val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
        if !xml_is_letter(val as u32) && val != b'_' as i32 && val != b':' as i32 {
            return null_mut();
        }

        while xml_is_letter(val as u32)
            || xml_is_digit(val as u32)
            || val == b'.' as i32
            || val == b'-' as i32
            || val == b'_' as i32
            || xml_is_combining(val as u32)
            || xml_is_extender(val as u32)
        {
            cur = cur.add(len as usize);
            val = xml_string_current_char(null_mut(), cur, addr_of_mut!(len));
        }
        let ret = xml_strndup(q, cur.offset_from(q) as _);
        CUR_PTR!(ctxt) = cur;
        ret
    }
}

/// Compile an attribute test.
#[doc(alias = "xmlCompileAttributeTest")]
unsafe fn xml_compile_attribute_test(ctxt: XmlPatParserContextPtr) {
    unsafe {
        let mut token: *mut XmlChar = null_mut();
        let mut url: *mut XmlChar = null_mut();

        SKIP_BLANKS!(ctxt);
        let name: *mut XmlChar = xml_pat_scan_ncname(ctxt);
        'error: {
            if name.is_null() {
                if CUR!(ctxt) == b'*' {
                    PUSH!(ctxt, XmlPatOp::XmlOpAttr, null_mut(), null_mut(), 'error);
                    NEXT!(ctxt);
                } else {
                    (*ctxt).error = 1;
                }
                return;
            }
            if CUR!(ctxt) == b':' {
                let prefix: *mut XmlChar = name;

                NEXT!(ctxt);

                if xml_is_blank_char(CUR!(ctxt) as u32) {
                    xml_free(prefix as _);
                    (*ctxt).error = 1;
                    break 'error;
                }
                // This is a namespace is_match
                token = xml_pat_scan_name(ctxt);
                if *prefix.add(0) == b'x'
                    && *prefix.add(1) == b'm'
                    && *prefix.add(2) == b'l'
                    && *prefix.add(3) == 0
                {
                    url = xml_strdup(XML_XML_NAMESPACE.as_ptr() as _);
                } else if let Some(namespaces) = (*ctxt).namespaces.as_deref() {
                    let mut found = false;
                    for &(href, pref) in namespaces {
                        if xml_str_equal(pref, prefix) {
                            url = xml_strdup(href);
                            found = true;
                            break;
                        }
                    }
                    if !found {
                        xml_free(prefix as _);
                        (*ctxt).error = 1;
                        break 'error;
                    }
                }
                xml_free(prefix as _);
                if token.is_null() {
                    if CUR!(ctxt) == b'*' {
                        NEXT!(ctxt);
                        PUSH!(ctxt, XmlPatOp::XmlOpAttr, null_mut(), url, 'error);
                    } else {
                        (*ctxt).error = 1;
                        break 'error;
                    }
                } else {
                    PUSH!(ctxt, XmlPatOp::XmlOpAttr, token, url, 'error);
                }
            } else {
                PUSH!(ctxt, XmlPatOp::XmlOpAttr, name, null_mut(), 'error);
            }
            return;
        }
        // error:
        if !url.is_null() {
            xml_free(url as _)
        }
        if !token.is_null() {
            xml_free(token as _);
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
unsafe fn xml_compile_step_pattern(ctxt: XmlPatParserContextPtr) {
    unsafe {
        let mut token: *mut XmlChar = null_mut();
        let mut name: *mut XmlChar = null_mut();
        let mut url: *mut XmlChar = null_mut();
        let mut has_blanks: i32 = 0;

        SKIP_BLANKS!(ctxt);
        'error: {
            if CUR!(ctxt) == b'.' {
                // Context node.
                NEXT!(ctxt);
                PUSH!(ctxt, XmlPatOp::XmlOpElem, null_mut(), null_mut(), 'error);
                return;
            }
            if CUR!(ctxt) == b'@' {
                // Attribute test.
                if XML_STREAM_XS_IDC_SEL!((*ctxt).comp) {
                    (*ctxt).error = 1;
                    return;
                }
                NEXT!(ctxt);
                xml_compile_attribute_test(ctxt);
                if (*ctxt).error != 0 {
                    break 'error;
                }
                return;
            }
            name = xml_pat_scan_ncname(ctxt);
            if name.is_null() {
                if CUR!(ctxt) == b'*' {
                    NEXT!(ctxt);
                    PUSH!(ctxt, XmlPatOp::XmlOpAll, null_mut(), null_mut(), 'error);
                    return;
                } else {
                    (*ctxt).error = 1;
                    return;
                }
            }
            if xml_is_blank_char(CUR!(ctxt) as u32) {
                has_blanks = 1;
                SKIP_BLANKS!(ctxt);
            }
            if CUR!(ctxt) == b':' {
                NEXT!(ctxt);
                if CUR!(ctxt) != b':' {
                    let prefix: *mut XmlChar = name;

                    if has_blanks != 0 || xml_is_blank_char(CUR!(ctxt) as u32) {
                        (*ctxt).error = 1;
                        break 'error;
                    }
                    // This is a namespace is_match
                    token = xml_pat_scan_name(ctxt);
                    if *prefix.add(0) == b'x'
                        && *prefix.add(1) == b'm'
                        && *prefix.add(2) == b'l'
                        && *prefix.add(3) == 0
                    {
                        url = xml_strdup(XML_XML_NAMESPACE.as_ptr() as _);
                    } else if let Some(namespaces) = (*ctxt).namespaces.as_deref() {
                        let mut found = false;
                        for &(href, pref) in namespaces {
                            if xml_str_equal(pref, prefix) {
                                url = xml_strdup(href);
                                found = true;
                                break;
                            }
                        }
                        if !found {
                            (*ctxt).error = 1;
                            break 'error;
                        }
                    }
                    xml_free(prefix as _);
                    name = null_mut();
                    if token.is_null() {
                        if CUR!(ctxt) == b'*' {
                            NEXT!(ctxt);
                            PUSH!(ctxt, XmlPatOp::XmlOpNs, url, null_mut(), 'error);
                        } else {
                            (*ctxt).error = 1;
                            break 'error;
                        }
                    } else {
                        PUSH!(ctxt, XmlPatOp::XmlOpElem, token, url, 'error);
                    }
                } else {
                    NEXT!(ctxt);
                    if xml_str_equal(name, c"child".as_ptr() as _) {
                        xml_free(name as _);
                        name = xml_pat_scan_name(ctxt);
                        if name.is_null() {
                            if CUR!(ctxt) == b'*' {
                                NEXT!(ctxt);
                                PUSH!(ctxt, XmlPatOp::XmlOpAll, null_mut(), null_mut(), 'error);
                                return;
                            } else {
                                (*ctxt).error = 1;
                                break 'error;
                            }
                        }
                        if CUR!(ctxt) == b':' {
                            let prefix: *mut XmlChar = name;

                            NEXT!(ctxt);
                            if xml_is_blank_char(CUR!(ctxt) as u32) {
                                (*ctxt).error = 1;
                                break 'error;
                            }
                            // This is a namespace is_match
                            token = xml_pat_scan_name(ctxt);
                            if *prefix.add(0) == b'x'
                                && *prefix.add(1) == b'm'
                                && *prefix.add(2) == b'l'
                                && *prefix.add(3) == 0
                            {
                                url = xml_strdup(XML_XML_NAMESPACE.as_ptr() as _);
                            } else if let Some(namespaces) = (*ctxt).namespaces.as_deref() {
                                let mut found = false;
                                for &(href, pref) in namespaces {
                                    if xml_str_equal(pref, prefix) {
                                        url = xml_strdup(href);
                                        found = true;
                                        break;
                                    }
                                }
                                if !found {
                                    (*ctxt).error = 1;
                                    break 'error;
                                }
                            }
                            xml_free(prefix as _);
                            name = null_mut();
                            if token.is_null() {
                                if CUR!(ctxt) == b'*' {
                                    NEXT!(ctxt);
                                    PUSH!(ctxt, XmlPatOp::XmlOpNs, url, null_mut(), 'error);
                                } else {
                                    (*ctxt).error = 1;
                                    break 'error;
                                }
                            } else {
                                PUSH!(ctxt, XmlPatOp::XmlOpChild, token, url, 'error);
                            }
                        } else {
                            PUSH!(ctxt, XmlPatOp::XmlOpChild, name, null_mut(), 'error);
                        }
                    } else if xml_str_equal(name, c"attribute".as_ptr() as _) {
                        xml_free(name as _);
                        name = null_mut();
                        if XML_STREAM_XS_IDC_SEL!((*ctxt).comp) {
                            (*ctxt).error = 1;
                            break 'error;
                        }
                        xml_compile_attribute_test(ctxt);
                        if (*ctxt).error != 0 {
                            break 'error;
                        }
                        return;
                    } else {
                        (*ctxt).error = 1;
                        break 'error;
                    }
                }
            } else if CUR!(ctxt) == b'*' {
                if !name.is_null() {
                    (*ctxt).error = 1;
                    break 'error;
                }
                NEXT!(ctxt);
                PUSH!(ctxt, XmlPatOp::XmlOpAll, token, null_mut(), 'error);
            } else {
                PUSH!(ctxt, XmlPatOp::XmlOpElem, name, null_mut(), 'error);
            }
            return;
        }
        //  error:
        if !url.is_null() {
            xml_free(url as _)
        }
        if !token.is_null() {
            xml_free(token as _)
        }
        if !name.is_null() {
            xml_free(name as _)
        }
    }
}

const XML_PATTERN_NOTPATTERN: i32 = XmlPatternFlags::XmlPatternXpath as i32
    | XmlPatternFlags::XmlPatternXssel as i32
    | XmlPatternFlags::XmlPatternXsfield as i32;

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
        memset(cur as _, 0, size_of::<XmlStreamComp>());
        (*cur).steps = xml_malloc(size as usize * size_of::<XmlStreamStep>()) as XmlStreamStepPtr;
        if (*cur).steps.is_null() {
            xml_free(cur as _);
            return null_mut();
        }
        (*cur).nb_step = 0;
        (*cur).max_step = size;
        cur
    }
}

/// Add a new step to the compiled pattern
///
/// Returns -1 in case of error or the step index if successful
#[doc(alias = "xmlStreamCompAddStep")]
unsafe fn xml_stream_comp_add_step(
    comp: XmlStreamCompPtr,
    name: *const XmlChar,
    ns: *const XmlChar,
    node_type: i32,
    flags: i32,
) -> i32 {
    unsafe {
        let mut cur: XmlStreamStepPtr;

        if (*comp).nb_step >= (*comp).max_step {
            cur = xml_realloc(
                (*comp).steps as _,
                (*comp).max_step as usize * 2 * size_of::<XmlStreamStep>(),
            ) as XmlStreamStepPtr;
            if cur.is_null() {
                return -1;
            }
            (*comp).steps = cur;
            (*comp).max_step *= 2;
        }
        cur = (*comp).steps.add((*comp).nb_step as usize);
        (*comp).nb_step += 1;
        (*cur).flags = flags;
        (*cur).name = name;
        (*cur).ns = ns;
        (*cur).node_type = node_type;
        (*comp).nb_step - 1
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
        let mut step: XmlStepOp;

        if comp.is_null() || (*comp).steps.is_null() {
            return -1;
        }
        // special case for .
        if (*comp).nb_step == 1
            && matches!((*(*comp).steps.add(0)).op, XmlPatOp::XmlOpElem)
            && (*(*comp).steps.add(0)).value.is_null()
            && (*(*comp).steps.add(0)).value2.is_null()
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

        stream = xml_new_stream_comp(((*comp).nb_step / 2) + 1);
        if stream.is_null() {
            return -1;
        }

        if (*comp).flags & PAT_FROM_ROOT as i32 != 0 {
            (*stream).flags |= XML_STREAM_FROM_ROOT as i32;
        }

        'error: {
            'main: for i in 0..(*comp).nb_step {
                step = *(*comp).steps.add(i as usize);
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
                            null(),
                            step.value,
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
                            step.value,
                            step.value2,
                            XmlElementType::XmlAttributeNode as i32,
                            flags,
                        );
                        flags = 0;
                        if s < 0 {
                            break 'error;
                        }
                    }
                    XmlPatOp::XmlOpElem => 'to_break: {
                        if step.value.is_null() && step.value2.is_null() {
                            // We have a "." or "self::node()" here.
                            // Eliminate redundant self::node() tests like in "/./."
                            // or "//./"
                            // The only case we won't eliminate is "//.", i.e. if
                            // self::node() is the last node test and we had
                            // continuation somewhere beforehand.
                            if (*comp).nb_step == i + 1 && flags & XML_STREAM_STEP_DESC as i32 != 0
                            {
                                // Mark the special case where the expression resolves
                                // to any type of node.
                                if (*comp).nb_step == i + 1 {
                                    (*stream).flags |= XML_STREAM_FINAL_IS_ANY_NODE as i32;
                                }
                                flags |= XML_STREAM_STEP_NODE as i32;
                                s = xml_stream_comp_add_step(
                                    stream,
                                    null(),
                                    null(),
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
                                    (*(*stream).steps.add(prevs as usize)).flags |=
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
                            step.value,
                            step.value2,
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
                            step.value,
                            step.value2,
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
                            null(),
                            null(),
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

                if (*stream).nb_step > 0
                    && (*(*stream).steps.add(0)).flags & XML_STREAM_STEP_DESC as i32 == 0
                {
                    (*(*stream).steps.add(0)).flags |= XML_STREAM_STEP_DESC as i32;
                }
            }
            if (*stream).nb_step <= s {
                break 'error;
            }
            (*(*stream).steps.add(s as usize)).flags |= XML_STREAM_STEP_FINAL as i32;
            if root != 0 {
                (*(*stream).steps.add(0)).flags |= XML_STREAM_STEP_ROOT as i32;
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
        let mut i: i32;
        let mut j: i32;

        // remove the leading // for //a or .//a
        if (*comp).nb_step > 0 && matches!((*(*comp).steps.add(0)).op, XmlPatOp::XmlOpAncestor) {
            for (i, j) in (0..).zip(1..(*comp).nb_step) {
                (*(*comp).steps.add(i as usize)).value = (*(*comp).steps.add(j as usize)).value;
                (*(*comp).steps.add(i as usize)).value2 = (*(*comp).steps.add(j as usize)).value2;
                (*(*comp).steps.add(i as usize)).op = (*(*comp).steps.add(j as usize)).op;
            }
            (*comp).nb_step -= 1;
        }
        if (*comp).nb_step >= (*comp).max_step {
            let temp: XmlStepOpPtr = xml_realloc(
                (*comp).steps as _,
                (*comp).max_step as usize * 2 * size_of::<XmlStepOp>(),
            ) as XmlStepOpPtr;
            if temp.is_null() {
                return -1;
            }
            (*comp).steps = temp;
            (*comp).max_step *= 2;
        }
        i = 0;
        j = (*comp).nb_step - 1;
        while j > i {
            let mut tmp: *const XmlChar;

            tmp = (*(*comp).steps.add(i as usize)).value;
            (*(*comp).steps.add(i as usize)).value = (*(*comp).steps.add(j as usize)).value;
            (*(*comp).steps.add(j as usize)).value = tmp;
            tmp = (*(*comp).steps.add(i as usize)).value2;
            (*(*comp).steps.add(i as usize)).value2 = (*(*comp).steps.add(j as usize)).value2;
            (*(*comp).steps.add(j as usize)).value2 = tmp;
            let op: XmlPatOp = (*(*comp).steps.add(i as usize)).op;
            (*(*comp).steps.add(i as usize)).op = (*(*comp).steps.add(j as usize)).op;
            (*(*comp).steps.add(j as usize)).op = op;
            j -= 1;
            i += 1;
        }
        (*(*comp).steps.add((*comp).nb_step as usize)).value = null_mut();
        (*(*comp).steps.add((*comp).nb_step as usize)).value2 = null_mut();
        (*(*comp).steps.add((*comp).nb_step as usize)).op = XmlPatOp::XmlOpEnd;
        (*comp).nb_step += 1;
        0
    }
}

/// Compile a pattern.
///
/// Returns the compiled form of the pattern or NULL in case of error
#[doc(alias = "xmlPatterncompile")]
pub unsafe fn xml_patterncompile(
    pattern: *const XmlChar,
    flags: i32,
    namespaces: Option<Vec<(*const u8, *const u8)>>,
) -> XmlPatternPtr {
    unsafe {
        let mut ret: XmlPatternPtr = null_mut();
        let mut cur: XmlPatternPtr;
        let mut or: *const XmlChar;
        let mut start: *const XmlChar;
        let mut tmp: *mut XmlChar;
        let mut typ: i32 = 0;
        let mut streamable: i32 = 1;

        if pattern.is_null() {
            return null_mut();
        }

        start = pattern;
        or = start;
        'error: {
            while *or != 0 {
                tmp = null_mut();
                while *or != 0 && *or != b'|' {
                    or = or.add(1);
                }
                let ctxt = if *or == 0 {
                    XmlPatParserContext::new(start, namespaces.clone())
                } else {
                    tmp = xml_strndup(start, or.offset_from(start) as _);
                    or = or.add(1);
                    (!tmp.is_null())
                        .then(|| XmlPatParserContext::new(tmp, namespaces.clone()))
                        .flatten()
                };
                let Some(mut ctxt) = ctxt else {
                    break 'error;
                };
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
                if !tmp.is_null() {
                    xml_free(tmp as _);
                    // tmp = null_mut();
                }
                start = or;
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
        if !tmp.is_null() {
            xml_free(tmp as _);
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
        let mut i: i32;
        let mut step: XmlStepOpPtr;
        let mut states: XmlStepStates = XmlStepStates {
            nbstates: 0,
            maxstates: 0,
            states: null_mut(),
        };
        // // may require backtrack

        if comp.is_null() {
            return -1;
        }
        i = 0;
        // restart:
        loop {
            'rollback: {
                'found: {
                    while i < (*comp).nb_step {
                        'to_continue: {
                            step = (*comp).steps.add(i as usize);
                            match (*step).op {
                                XmlPatOp::XmlOpEnd => {
                                    break 'found;
                                }
                                XmlPatOp::XmlOpRoot => {
                                    if matches!(
                                        node.element_type(),
                                        XmlElementType::XmlNamespaceDecl
                                    ) {
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
                                    if !matches!(
                                        node.element_type(),
                                        XmlElementType::XmlElementNode
                                    ) {
                                        break 'rollback;
                                    }
                                    let node = XmlNodePtr::try_from(node).unwrap();
                                    if (*step).value.is_null() {
                                        break 'to_continue;
                                    }
                                    if *(*step).value.add(0) != *node.name.add(0) {
                                        break 'rollback;
                                    }
                                    if !xml_str_equal((*step).value, node.name) {
                                        break 'rollback;
                                    }

                                    // Namespace test
                                    if let Some(ns) = node.ns {
                                        if !ns.href.is_null() {
                                            if (*step).value2.is_null() {
                                                break 'rollback;
                                            }
                                            if !xml_str_equal((*step).value2, ns.href) {
                                                break 'rollback;
                                            }
                                        }
                                    } else if !(*step).value2.is_null() {
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

                                    if !(*step).value.is_null() {
                                        while let Some(now) = lst {
                                            if matches!(
                                                now.element_type(),
                                                XmlElementType::XmlElementNode
                                            ) {
                                                let now = XmlNodePtr::try_from(now).unwrap();
                                                if *(*step).value.add(0) == *now.name.add(0)
                                                    && xml_str_equal((*step).value, now.name)
                                                {
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
                                    if !matches!(
                                        node.element_type(),
                                        XmlElementType::XmlAttributeNode
                                    ) {
                                        break 'rollback;
                                    }
                                    let node = XmlAttrPtr::try_from(node).unwrap();
                                    if !(*step).value.is_null() {
                                        if *(*step).value.add(0) != *node.name.add(0) {
                                            break 'rollback;
                                        }
                                        if !xml_str_equal((*step).value, node.name) {
                                            break 'rollback;
                                        }
                                    }
                                    // Namespace test
                                    if let Some(ns) = node.ns {
                                        if !(*step).value2.is_null()
                                            && !xml_str_equal((*step).value2, ns.href)
                                        {
                                            break 'rollback;
                                        }
                                    } else if !(*step).value2.is_null() {
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
                                    if (*step).value.is_null() {
                                        break 'to_continue;
                                    }
                                    // Is is correct ??????
                                    let node = XmlNodePtr::try_from(node).unwrap();
                                    if *(*step).value.add(0) != *node.name.add(0) {
                                        break 'rollback;
                                    }
                                    if !xml_str_equal((*step).value, node.name) {
                                        break 'rollback;
                                    }
                                    // Namespace test
                                    if let Some(ns) = node.ns {
                                        if !ns.href.is_null() {
                                            if (*step).value2.is_null() {
                                                break 'rollback;
                                            }
                                            if !xml_str_equal((*step).value2, ns.href) {
                                                break 'rollback;
                                            }
                                        }
                                    } else if !(*step).value2.is_null() {
                                        break 'rollback;
                                    }
                                    break 'to_continue;
                                }
                                XmlPatOp::XmlOpAncestor => {
                                    // TODO: implement coalescing of ANCESTOR/NODE ops
                                    if (*step).value.is_null() {
                                        i += 1;
                                        step = (*comp).steps.add(i as usize);
                                        if matches!((*step).op, XmlPatOp::XmlOpRoot) {
                                            break 'found;
                                        }
                                        if !matches!((*step).op, XmlPatOp::XmlOpElem) {
                                            break 'rollback;
                                        }
                                        if (*step).value.is_null() {
                                            return -1;
                                        }
                                    }
                                    // if node.is_null() {
                                    //     break 'rollback;
                                    // }
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

                                            if *(*step).value.add(0) == *node.name.add(0)
                                                && xml_str_equal((*step).value, node.name)
                                            {
                                                // Namespace test
                                                if let Some(ns) = node.ns {
                                                    if !ns.href.is_null()
                                                        && (!(*step).value2.is_null()
                                                            && xml_str_equal(
                                                                (*step).value2,
                                                                ns.href,
                                                            ))
                                                    {
                                                        break;
                                                    }
                                                } else if (*step).value2.is_null() {
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
                                    if matches!((*step).op, XmlPatOp::XmlOpAncestor) {
                                        xml_pat_push_state(addr_of_mut!(states), i, node);
                                    } else {
                                        xml_pat_push_state(addr_of_mut!(states), i - 1, node);
                                    }
                                    break 'to_continue;
                                }
                                XmlPatOp::XmlOpNs => {
                                    if !matches!(
                                        node.element_type(),
                                        XmlElementType::XmlElementNode
                                    ) {
                                        break 'rollback;
                                    }
                                    let node = XmlNodePtr::try_from(node).unwrap();
                                    if let Some(ns) = node.ns {
                                        if !ns.href.is_null() {
                                            if (*step).value.is_null() {
                                                break 'rollback;
                                            }
                                            if !xml_str_equal((*step).value, ns.href) {
                                                break 'rollback;
                                            }
                                        }
                                    } else if !(*step).value.is_null() {
                                        break 'rollback;
                                    }
                                }
                                XmlPatOp::XmlOpAll => {
                                    if !matches!(
                                        node.element_type(),
                                        XmlElementType::XmlElementNode
                                    ) {
                                        break 'rollback;
                                    }
                                }
                            }
                        }
                        i += 1;
                    }
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
            i = (*states.states.add(states.nbstates as usize)).step;
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

            for i in 0..(*(*comp).stream).nb_step {
                if (*(*(*comp).stream).steps.add(i as usize)).flags & XML_STREAM_STEP_DESC as i32
                    != 0
                {
                    return -2;
                }
            }
            if (*(*comp).stream).nb_step > ret {
                ret = (*(*comp).stream).nb_step;
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
            if (*(*comp).stream).nb_step < ret {
                ret = (*(*comp).stream).nb_step;
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
        let mut step: XmlStreamStep;

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
                        if (*comp).nb_step == 0 {
                            // TODO: We have a "/." here?
                            ret = 1;
                        } else if (*comp).nb_step == 1
                            && (*(*comp).steps.add(0)).node_type == XML_STREAM_ANY_NODE as i32
                            && (*(*comp).steps.add(0)).flags & XML_STREAM_STEP_DESC as i32 != 0
                        {
                            // In the case of "//." the document node will is_match as well.
                            ret = 1;
                        } else if (*(*comp).steps.add(0)).flags & XML_STREAM_STEP_ROOT as i32 != 0 {
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
                if (*comp).nb_step == 0 {
                    // / and . are handled at the XPath node set creation
                    // level by checking min depth
                    if (*stream).flags & XmlPatternFlags::XmlPatternXpath as i32 != 0 {
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
                            desc = (*(*comp).steps.add(step_nr as usize)).flags
                                & XML_STREAM_STEP_DESC as i32;
                            if tmp < (*stream).level && desc == 0 {
                                break 'next_state;
                            }
                        }
                        // Check for correct node-type.
                        step = *(*comp).steps.add(step_nr as usize);
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
                        } else if step.name.is_null() {
                            if step.ns.is_null() {
                                // This lets through all elements/attributes.
                                is_match = 1;
                            } else if !ns.is_null() {
                                is_match = xml_str_equal(step.ns, ns) as i32;
                            }
                        } else if step.ns.is_null() == ns.is_null()
                            && !name.is_null()
                            && *step.name.add(0) == *name.add(0)
                            && xml_str_equal(step.name, name)
                            && (step.ns == ns || xml_str_equal(step.ns, ns))
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
                step = *(*comp).steps.add(0);
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
                } else if step.name.is_null() {
                    if step.ns.is_null() {
                        // This lets through all elements/attributes.
                        is_match = 1;
                    } else if !ns.is_null() {
                        is_match = xml_str_equal(step.ns, ns) as i32;
                    }
                } else if step.ns.is_null() == ns.is_null()
                    && !name.is_null()
                    && *step.name.add(0) == *name.add(0)
                    && xml_str_equal(step.name, name)
                    && (step.ns == ns || xml_str_equal(step.ns, ns))
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

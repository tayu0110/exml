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

use std::ptr::null_mut;

use crate::{
    libxml::{chvalid::xml_is_blank_char, pattern::xml_free_pattern_list},
    tree::XmlGenericNodePtr,
};

use super::{
    XPATH_MAX_STACK_DEPTH, XmlXPathCompExprPtr, XmlXPathContextPtr, XmlXPathError,
    XmlXPathObjectPtr, xml_xpath_free_comp_expr, xml_xpath_free_object, xml_xpath_new_comp_expr,
    xml_xpath_perr_memory, xml_xpath_release_object,
};

pub type XmlXPathParserContextPtr = *mut XmlXPathParserContext;
/// An XPath parser context. It contains pure parsing information,
/// an xmlXPathContext, and the stack of objects.
#[doc(alias = "xmlXPathParserContext")]
#[repr(C)]
pub struct XmlXPathParserContext {
    pub(crate) cur: usize,     /* the current char being parsed */
    pub(crate) base: Box<str>, /* the full expression */

    pub(crate) error: i32, /* error code */

    pub(crate) context: XmlXPathContextPtr, /* the evaluation context */
    pub(crate) value: XmlXPathObjectPtr,    /* the current value */
    pub(crate) value_tab: Vec<XmlXPathObjectPtr>, /* stack of values */

    pub(crate) comp: XmlXPathCompExprPtr, /* the precompiled expression */
    pub(crate) xptr: i32,                 /* it this an XPointer expression */
    pub(crate) ancestor: Option<XmlGenericNodePtr>, /* used for walking preceding axis */

    pub(crate) value_frame: i32, /* unused */
}

impl XmlXPathParserContext {
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
            .trim_start_matches(|c: char| xml_is_blank_char(c as u32));
        let diff = self.current_str().len() - rem.len();
        self.cur += diff;
    }

    /// Pushes a new XPath object on top of the value stack. If value is NULL,
    /// a memory error is recorded in the parser context.
    ///
    /// Returns the number of items on the value stack, or -1 in case of error.
    ///
    /// The object is destroyed in case of error.
    #[doc(alias = "valuePush")]
    pub unsafe fn value_push(&mut self, value: XmlXPathObjectPtr) -> i32 {
        unsafe {
            if value.is_null() {
                // A NULL value typically indicates that a memory allocation failed,
                // so we set self.error here to propagate the error.
                self.error = XmlXPathError::XPathMemoryError as i32;
                return -1;
            }
            if self.value_tab.len() == XPATH_MAX_STACK_DEPTH {
                xml_xpath_perr_memory(self, Some("XPath stack depth limit reached\n"));
                xml_xpath_free_object(value);
                return -1;
            }
            self.value_tab.push(value);
            self.value = value;
            self.value_tab.len() as i32 - 1
        }
    }

    // TODO: remap to xmlXPathValuePop and Push.
    /// Pops the top XPath object from the value stack
    ///
    /// Returns the XPath object just removed
    #[doc(alias = "valuePop")]
    pub fn value_pop(&mut self) -> XmlXPathObjectPtr {
        if self.value_tab.is_empty() {
            return null_mut();
        }

        let res = self.value_tab.pop().unwrap();
        self.value = self.value_tab.last().cloned().unwrap_or(null_mut());
        res
    }
}

impl Default for XmlXPathParserContext {
    fn default() -> Self {
        Self {
            cur: 0,
            base: "".to_owned().into_boxed_str(),
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

/// Create a new xmlXPathParserContext
///
/// Returns the xmlXPathParserContext just allocated.
#[doc(alias = "xmlXPathNewParserContext")]
pub unsafe fn xml_xpath_new_parser_context(
    xpath: &str,
    ctxt: XmlXPathContextPtr,
) -> XmlXPathParserContextPtr {
    unsafe {
        let ret = XmlXPathParserContext {
            cur: 0,
            base: xpath.to_owned().into_boxed_str(),
            context: ctxt,
            comp: xml_xpath_new_comp_expr(),
            ..Default::default()
        };
        if ret.comp.is_null() {
            return null_mut();
        }

        Box::leak(Box::new(ret))
    }
}

/// Create a new xmlXPathParserContext when processing a compiled expression
///
/// Returns the xmlXPathParserContext just allocated.
#[doc(alias = "xmlXPathCompParserContext")]
pub(super) unsafe fn xml_xpath_comp_parser_context(
    comp: XmlXPathCompExprPtr,
    ctxt: XmlXPathContextPtr,
) -> XmlXPathParserContextPtr {
    let ret = Box::new(XmlXPathParserContext {
        value_tab: Vec::with_capacity(10),
        value: null_mut(),
        context: ctxt,
        comp,
        ..Default::default()
    });

    Box::leak(ret)
}

/// Free up an xmlXPathParserContext
#[doc(alias = "xmlXPathFreeParserContext")]
pub unsafe fn xml_xpath_free_parser_context(ctxt: XmlXPathParserContextPtr) {
    unsafe {
        for value in (*ctxt).value_tab.drain(..) {
            if !(*ctxt).context.is_null() {
                xml_xpath_release_object((*ctxt).context, value);
            } else {
                xml_xpath_free_object(value);
            }
        }
        if !(*ctxt).comp.is_null() {
            #[cfg(feature = "libxml_pattern")]
            if !(*(*ctxt).comp).stream.is_null() {
                xml_free_pattern_list((*(*ctxt).comp).stream);
                (*(*ctxt).comp).stream = null_mut();
            }
            xml_xpath_free_comp_expr((*ctxt).comp);
        }
        let _ = Box::from_raw(ctxt);
    }
}

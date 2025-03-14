//! Provide methods and data structures for XML Schematron.  
//!
//! This module is based on `libxml/schematron.h`, `schematron.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: XML Schematron implementation
// Description: interface to the XML Schematron validity checking.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// schematron.c : implementation of the Schematron schema validity checking
//
// See Copyright for the status of this software.
//
// Daniel Veillard <daniel@veillard.com>

use std::{
    ffi::{CStr, CString, c_char},
    mem::{size_of, take},
    os::raw::c_void,
    ptr::{drop_in_place, null_mut},
    slice::from_raw_parts,
};

use libc::{FILE, malloc, memset};

#[cfg(feature = "libxml_pattern")]
use crate::pattern::{XmlPattern, XmlPatternFlags, xml_pattern_compile};
use crate::{
    error::{__xml_raise_error, __xml_simple_oom_error, XmlErrorDomain, XmlParserErrors},
    generic_error,
    globals::{GenericError, GenericErrorContext, StructuredError},
    io::{XmlOutputCloseCallback, XmlOutputWriteCallback},
    parser::{xml_read_file, xml_read_memory},
    tree::{
        NodeCommon, XmlAttrPtr, XmlDocPtr, XmlElementType, XmlGenericNodePtr, XmlNodePtr,
        xml_free_doc,
    },
    xpath::{
        XML_XPATH_CHECKNS, XmlXPathCompExprPtr, XmlXPathContextPtr, XmlXPathObjectPtr,
        XmlXPathObjectType,
        internals::{xml_xpath_register_ns, xml_xpath_register_variable_ns},
        xml_xpath_compiled_eval, xml_xpath_ctxt_compile, xml_xpath_eval, xml_xpath_free_comp_expr,
        xml_xpath_free_context, xml_xpath_free_object, xml_xpath_is_nan, xml_xpath_new_context,
    },
};

use super::{
    globals::{xml_free, xml_malloc},
    parser::XmlParserOption,
    xmlstring::{XmlChar, xml_strdup},
};

/// Handle a parser error
#[doc(alias = "xmlSchematronPErr")]
macro_rules! xml_schematron_perr {
    ($ctxt:expr, $node:expr, $error:expr, $msg:literal) => {
        xml_schematron_perr!(
            @inner,
            $ctxt,
            $node,
            $error,
            $msg,
            None,
            None
        );
    };
    ($ctxt:expr, $node:expr, $error:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        xml_schematron_perr!(
            @inner,
            $ctxt,
            $node,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            None
        );
    };
    ($ctxt:expr, $node:expr, $error:expr, $msg:literal, $str1:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $str2);
        xml_schematron_perr!(
            @inner,
            $ctxt,
            $node,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            Some($str2.to_owned().into())
        );
    };
    (@inner, $ctxt:expr, $node:expr, $error:expr, $msg:expr, $str1:expr, $str2:expr) => {
        let ctxt = $ctxt as *mut XmlSchematronParserCtxt;
        let mut channel: Option<GenericError> = None;
        let mut schannel: Option<StructuredError> = None;
        let mut data = None;

        if !ctxt.is_null() {
            (*ctxt).nberrors += 1;
            channel = (*ctxt).error;
            data = (*ctxt).user_data.clone();
            schannel = (*ctxt).serror;
        }
        __xml_raise_error!(
            schannel,
            channel,
            data,
            ctxt as _,
            $node,
            XmlErrorDomain::XmlFromSchemasp,
            $error,
            XmlErrorLevel::XmlErrError,
            None,
            0,
            $str1,
            $str2,
            None,
            0,
            0,
            $msg,
        );
    };
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlSchematronValidOptions {
    XmlSchematronOutQuiet = 1 << 0,  /* quiet no report */
    XmlSchematronOutText = 1 << 1,   /* build a textual report */
    XmlSchematronOutXml = 1 << 2,    /* output SVRL */
    XmlSchematronOutError = 1 << 3,  /* output via xmlStructuredErrorFunc */
    XmlSchematronOutFile = 1 << 8,   /* output to a file descriptor */
    XmlSchematronOutBuffer = 1 << 9, /* output to a buffer */
    XmlSchematronOutIO = 1 << 10,    /* output to I/O mechanism */
}

/// Signature of an error callback from a Schematron validation
#[doc(alias = "xmlSchematronValidityErrorFunc")]
pub type XmlSchematronValidityErrorFunc = unsafe fn(ctx: *mut c_void, msg: *const c_char);

/// Signature of a warning callback from a Schematron validation
#[doc(alias = "xmlSchematronValidityWarningFunc")]
pub type XmlSchematronValidityWarningFunc = unsafe fn(ctx: *mut c_void, msg: *const c_char);

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum XmlSchematronTestType {
    XmlSchematronAssert = 1,
    XmlSchematronReport = 2,
}

pub type XmlSchematronLetPtr = *mut XmlSchematronLet;
/// A Schematron let variable
#[doc(alias = "xmlSchematronLet")]
#[repr(C)]
pub struct XmlSchematronLet {
    next: XmlSchematronLetPtr, /* the next let variable in the list */
    name: *mut XmlChar,        /* the name of the variable */
    comp: XmlXPathCompExprPtr, /* the compiled expression */
}

pub type XmlSchematronTestPtr = *mut XmlSchematronTest;
/// A Schematrons test, either an assert or a report
#[doc(alias = "xmlSchematronTest")]
#[repr(C)]
pub struct XmlSchematronTest {
    next: XmlSchematronTestPtr, /* the next test in the list */
    typ: XmlSchematronTestType, /* the test type */
    node: Option<XmlNodePtr>,   /* the node in the tree */
    test: String,               /* the expression to test */
    comp: XmlXPathCompExprPtr,  /* the compiled expression */
    report: Option<String>,     /* the message to report */
}

pub type XmlSchematronRulePtr = *mut XmlSchematronRule;
/// A Schematrons rule
#[doc(alias = "xmlSchematronRule")]
#[repr(C)]
pub struct XmlSchematronRule {
    next: XmlSchematronRulePtr,       /* the next rule in the list */
    patnext: XmlSchematronRulePtr,    /* the next rule in the pattern list */
    node: Option<XmlNodePtr>,         /* the node in the tree */
    context: String,                  /* the context evaluation rule */
    tests: XmlSchematronTestPtr,      /* the list of tests */
    pattern: Option<Box<XmlPattern>>, /* the compiled pattern associated */
    report: Option<String>,           /* the message to report */
    lets: XmlSchematronLetPtr,        /* the list of let variables */
}

pub type XmlSchematronPatternPtr = *mut XmlSchematronPattern;
/// A Schematrons pattern
#[doc(alias = "xmlSchematronPattern")]
#[repr(C)]
pub struct XmlSchematronPattern {
    next: XmlSchematronPatternPtr, /* the next pattern in the list */
    rules: XmlSchematronRulePtr,   /* the list of rules */
    name: String,                  /* the name of the pattern */
}

pub type XmlSchematronPtr = *mut XmlSchematron;

/// A Schematrons definition
#[doc(alias = "xmlSchematron")]
#[repr(C)]
pub struct XmlSchematron {
    name: *const XmlChar,   /* schema name */
    preserve: i32,          /* was the document passed by the user */
    doc: Option<XmlDocPtr>, /* pointer to the parsed document */
    flags: i32,             /* specific to this schematron */

    _private: *mut c_void, /* unused by the library */
    title: Option<String>, /* the title if any */

    nb_ns: i32, /* the number of namespaces */

    nb_pattern: i32,                   /* the number of patterns */
    patterns: XmlSchematronPatternPtr, /* the patterns found */
    rules: XmlSchematronRulePtr,       /* the rules gathered */
    // the array of namespaces
    // (href, prefix)
    namespaces: Vec<(String, Option<String>)>,
}

impl Default for XmlSchematron {
    fn default() -> Self {
        Self {
            name: null_mut(),
            preserve: 0,
            doc: None,
            flags: 0,
            _private: null_mut(),
            title: None,
            nb_ns: 0,
            nb_pattern: 0,
            patterns: null_mut(),
            rules: null_mut(),
            namespaces: vec![],
        }
    }
}

pub type XmlSchematronValidCtxtPtr = *mut XmlSchematronValidCtxt;
/// A Schematrons validation context
#[doc(alias = "xmlSchematronValidCtxt")]
#[repr(C)]
pub struct XmlSchematronValidCtxt {
    typ: i32,
    flags: i32, /* an or of xmlSchematronValidOptions */

    nberrors: i32,
    err: i32,

    schema: XmlSchematronPtr,
    xctxt: XmlXPathContextPtr,

    output_file: *mut FILE, /* if using XML_SCHEMATRON_OUT_FILE */
    output_buffer: Vec<u8>, /* if using XML_SCHEMATRON_OUT_BUFFER */
    #[cfg(feature = "libxml_output")]
    iowrite: Option<XmlOutputWriteCallback>, /* if using XML_SCHEMATRON_OUT_IO */
    #[cfg(feature = "libxml_output")]
    ioclose: Option<XmlOutputCloseCallback>,
    ioctx: *mut c_void,

    /* error reporting data */
    user_data: Option<GenericErrorContext>, /* user specific data block */
    error: Option<GenericError>,            /* the callback in case of errors */
    warning: Option<XmlSchematronValidityWarningFunc>, /* callback in case of warning */
    serror: Option<StructuredError>,        /* the structured function */
}

impl XmlSchematronValidCtxt {
    #[doc(alias = "xmlSchematronGetNode")]
    unsafe fn get_node(
        &self,
        cur: Option<XmlGenericNodePtr>,
        xpath: &str,
    ) -> Option<XmlGenericNodePtr> {
        unsafe {
            (*self.xctxt).doc = cur?.document();
            (*self.xctxt).node = cur;
            let ret: XmlXPathObjectPtr = xml_xpath_eval(xpath, self.xctxt);
            if ret.is_null() {
                return None;
            }

            if (*ret).typ == XmlXPathObjectType::XPathNodeset {
                if let Some(nodeset) = (*ret).nodesetval.as_deref() {
                    if !nodeset.node_tab.is_empty() {
                        let node = Some(nodeset.node_tab[0]);
                        xml_xpath_free_object(ret);
                        return node;
                    }
                }
            }

            xml_xpath_free_object(ret);
            None
        }
    }

    /// Set the structured error callback
    #[doc(alias = "xmlSchematronSetValidStructuredErrors")]
    pub fn set_structured_errors(
        &mut self,
        serror: Option<StructuredError>,
        ctx: Option<GenericErrorContext>,
    ) {
        self.serror = serror;
        self.error = None;
        self.warning = None;
        self.user_data = ctx;
    }

    /// Build the string being reported to the user.
    ///
    /// Returns a report string or NULL in case of error. The string needs to be deallocated by the caller
    #[doc(alias = "xmlSchematronFormatReport")]
    unsafe fn format_report(&self, test: XmlNodePtr, cur: XmlNodePtr) -> Option<String> {
        unsafe {
            let mut ret: Option<String> = None;
            let mut comp: XmlXPathCompExprPtr;

            let mut child = test.children().map(|c| XmlNodePtr::try_from(c).unwrap());
            while let Some(cur_node) = child {
                if cur_node.element_type() == XmlElementType::XmlTextNode
                    || cur_node.element_type() == XmlElementType::XmlCDATASectionNode
                {
                    let ret = ret.get_or_insert_default();
                    ret.push_str(
                        CStr::from_ptr(cur_node.content as *const i8)
                            .to_string_lossy()
                            .as_ref(),
                    );
                } else if is_schematron(cur_node, "name") {
                    let path = cur_node.get_no_ns_prop("path");

                    let mut node = XmlGenericNodePtr::from(cur);
                    if let Some(path) = path {
                        node = self.get_node(Some(cur.into()), &path).unwrap_or(cur.into());
                    }

                    let ns = if let Ok(node) = XmlNodePtr::try_from(node) {
                        node.ns
                    } else {
                        let attr = XmlAttrPtr::try_from(node).unwrap();
                        attr.ns
                    };

                    let ret = ret.get_or_insert_default();
                    if let Some(prefix) = ns
                        .as_deref()
                        .and_then(|ns| ns.prefix())
                        .filter(|p| !p.is_empty())
                    {
                        ret.push_str(&prefix);
                        ret.push(':');
                    }
                    ret.push_str(&node.name().unwrap());
                } else if is_schematron(cur_node, "value-of") {
                    comp = cur_node
                        .get_no_ns_prop("select")
                        .map(|select| xml_xpath_ctxt_compile(self.xctxt, &select))
                        .unwrap_or(null_mut());
                    let eval: XmlXPathObjectPtr = xml_xpath_compiled_eval(comp, self.xctxt);

                    match (*eval).typ {
                        XmlXPathObjectType::XPathNodeset => {
                            let spacer = " ";
                            let ret = ret.get_or_insert_default();

                            if let Some(nodeset) = (*eval).nodesetval.as_deref() {
                                for (indx, &node) in nodeset.node_tab.iter().enumerate() {
                                    if indx > 0 {
                                        ret.push_str(spacer);
                                    }
                                    ret.push_str(&node.name().unwrap());
                                }
                            } else {
                                generic_error!("Empty node set\n");
                            }
                        }
                        XmlXPathObjectType::XPathBoolean => {
                            let s = if (*eval).boolval { "True" } else { "False" };
                            if let Some(ret) = ret.as_mut() {
                                ret.push_str(s);
                            } else {
                                ret = Some(s.to_owned());
                            }
                        }
                        XmlXPathObjectType::XPathNumber => {
                            if let Some(ret) = ret.as_mut() {
                                ret.push_str(format!("{}", (*eval).floatval).as_str());
                            } else {
                                ret = Some(format!("{}", (*eval).floatval));
                            }
                        }
                        XmlXPathObjectType::XPathString => {
                            let strval = (*eval).stringval.as_deref().unwrap();
                            let ret =
                                ret.get_or_insert_with(|| String::with_capacity(strval.len()));
                            ret.push_str(strval);
                        }
                        _ => {
                            generic_error!("Unsupported XPATH Type: {:?}\n", (*eval).typ);
                        }
                    }
                    xml_xpath_free_object(eval);
                    xml_xpath_free_comp_expr(comp);
                } else {
                    child = cur_node
                        .next
                        .map(|node| XmlNodePtr::try_from(node).unwrap());
                    continue;
                }

                // remove superfluous \n
                if let Some(ret) = ret.as_mut() {
                    let trimmed = ret.trim_end_matches([' ', '\n', '\r', '\t']);
                    if ret.len() != trimmed.len() {
                        let newlen = trimmed.len();
                        ret.truncate(newlen);
                        ret.push(' ');
                    }
                }

                child = cur_node
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
            }
            ret
        }
    }

    /// Output part of the report to whatever channel the user selected
    #[doc(alias = "xmlSchematronReportOutput")]
    unsafe fn report_output(&self, _cur: Option<XmlNodePtr>, msg: &str) {
        // TODO
        eprintln!("{}", msg);
    }

    /// Called from the validation engine when an assert or report test have been done.
    #[doc(alias = "xmlSchematronReportSuccess")]
    unsafe fn report_success(
        &self,
        test: XmlSchematronTestPtr,
        cur: XmlNodePtr,
        pattern: XmlSchematronPatternPtr,
        success: i32,
    ) {
        unsafe {
            if test.is_null() {
                return;
            }
            // if quiet and not SVRL report only failures
            if self.flags & XmlSchematronValidOptions::XmlSchematronOutQuiet as i32 != 0
                && self.flags & XmlSchematronValidOptions::XmlSchematronOutXml as i32 == 0
                && (*test).typ == XmlSchematronTestType::XmlSchematronReport
            {
                return;
            }
            if self.flags & XmlSchematronValidOptions::XmlSchematronOutXml as i32 != 0 {
                // TODO
            } else {
                if ((*test).typ == XmlSchematronTestType::XmlSchematronReport && success == 0)
                    || ((*test).typ == XmlSchematronTestType::XmlSchematronAssert && success != 0)
                {
                    return;
                }
                let line: i64 = cur.get_line_no();
                let path = cur.get_node_path().expect("Internal Error");
                let mut report = None;
                if let Some(node) = (*test).node {
                    report = self.format_report(node, cur);
                }
                let report = report.unwrap_or_else(|| {
                    if (*test).typ == XmlSchematronTestType::XmlSchematronAssert {
                        "node failed assert".to_owned()
                    } else {
                        "node failed report".to_owned()
                    }
                });
                let msg = format!("{path} line {line}: {report}\n");

                if self.flags & XmlSchematronValidOptions::XmlSchematronOutError as i32 != 0 {
                    let mut schannel: Option<StructuredError> = None;
                    let mut channel: Option<GenericError> = None;

                    if self.serror.is_some() {
                        schannel = self.serror;
                    } else {
                        channel = self.error;
                    }
                    let data = self.user_data.clone();

                    __xml_raise_error!(
                        schannel,
                        channel,
                        data,
                        null_mut(),
                        Some(cur.into()),
                        XmlErrorDomain::XmlFromSchematronv,
                        if (*test).typ == XmlSchematronTestType::XmlSchematronAssert {
                            XmlParserErrors::XmlSchematronvAssert
                        } else {
                            XmlParserErrors::XmlSchematronvReport
                        },
                        XmlErrorLevel::XmlErrError,
                        None,
                        line as _,
                        if pattern.is_null() {
                            None
                        } else {
                            Some((*pattern).name.clone().into())
                        },
                        Some(path.to_owned().into()),
                        Some(report.into()),
                        0,
                        0,
                        msg.as_str(),
                    );
                } else {
                    self.report_output(Some(cur), &msg);
                }
            }
        }
    }

    /// Called from the validation engine when starting to check a pattern
    #[doc(alias = "xmlSchematronReportPattern")]
    unsafe fn report_pattern(&self, pattern: XmlSchematronPatternPtr) {
        unsafe {
            if pattern.is_null() {
                return;
            }
            if self.flags & XmlSchematronValidOptions::XmlSchematronOutQuiet as i32 != 0
                || self.flags & XmlSchematronValidOptions::XmlSchematronOutError as i32 != 0
            {
                // Error gives pattern name as part of error
                return;
            }
            if self.flags & XmlSchematronValidOptions::XmlSchematronOutXml as i32 != 0 {
                // TODO
            } else {
                let msg = format!("Pattern: {}\n", (*pattern).name);
                self.report_output(None, &msg);
            }
        }
    }

    /// Validate a rule against a tree instance at a given position
    ///
    /// Returns 1 in case of success, 0 if error and -1 in case of internal error
    #[doc(alias = "xmlSchematronRunTest")]
    unsafe fn run_test(
        &mut self,
        test: XmlSchematronTestPtr,
        instance: XmlDocPtr,
        cur: XmlNodePtr,
        pattern: XmlSchematronPatternPtr,
    ) -> i32 {
        unsafe {
            let mut failed: i32;

            failed = 0;
            (*self.xctxt).doc = Some(instance);
            (*self.xctxt).node = Some(cur.into());
            let ret: XmlXPathObjectPtr = xml_xpath_compiled_eval((*test).comp, self.xctxt);
            if ret.is_null() {
                failed = 1;
            } else {
                match (*ret).typ {
                    XmlXPathObjectType::XPathXSLTTree | XmlXPathObjectType::XPathNodeset => {
                        if (*ret).nodesetval.as_deref().is_none_or(|n| n.is_empty()) {
                            failed = 1;
                        }
                    }
                    XmlXPathObjectType::XPathBoolean => {
                        failed = (!(*ret).boolval) as i32;
                    }
                    XmlXPathObjectType::XPathNumber => {
                        if xml_xpath_is_nan((*ret).floatval) || (*ret).floatval == 0.0 {
                            failed = 1;
                        }
                    }
                    XmlXPathObjectType::XPathString => {
                        if (*ret).stringval.as_deref().is_none_or(|s| s.is_empty()) {
                            failed = 1;
                        }
                    }
                    XmlXPathObjectType::XPathUndefined | XmlXPathObjectType::XPathUsers => {
                        failed = 1;
                    }
                    #[cfg(feature = "libxml_xptr_locs")]
                    XmlXPathObjectType::XPathPoint
                    | XmlXPathObjectType::XPathRange
                    | XmlXPathObjectType::XPathLocationset => {
                        failed = 1;
                    }
                }
                xml_xpath_free_object(ret);
            }
            if (failed != 0 && (*test).typ == XmlSchematronTestType::XmlSchematronAssert)
                || (failed == 0 && (*test).typ == XmlSchematronTestType::XmlSchematronReport)
            {
                self.nberrors += 1;
            }

            self.report_success(test, cur, pattern, (failed == 0) as i32);

            (failed == 0) as i32
        }
    }

    /// Validate a tree instance against the schematron
    ///
    /// Returns 0 in case of success, -1 in case of internal error and an error count otherwise.
    #[doc(alias = "xmlSchematronValidateDoc")]
    pub unsafe fn validate_doc(&mut self, instance: XmlDocPtr) -> i32 {
        unsafe {
            let mut pattern: XmlSchematronPatternPtr;
            let mut rule: XmlSchematronRulePtr;
            let mut test: XmlSchematronTestPtr;

            if self.schema.is_null() || (*self.schema).rules.is_null() {
                return -1;
            }
            self.nberrors = 0;
            let Some(root) = instance.get_root_element() else {
                // TODO
                self.nberrors += 1;
                return 1;
            };
            if self.flags & XmlSchematronValidOptions::XmlSchematronOutQuiet as i32 != 0
                || self.flags == 0
            {
                // we are just trying to assert the validity of the document,
                // speed primes over the output, run in a single pass
                let mut cur = Some(root);
                while let Some(cur_node) = cur {
                    rule = (*self.schema).rules;
                    while !rule.is_null() {
                        if (*rule)
                            .pattern
                            .as_deref()
                            .unwrap()
                            .pattern_match(cur_node.into())
                            == 1
                        {
                            test = (*rule).tests;

                            if xml_schematron_register_variables(
                                self.xctxt,
                                (*rule).lets,
                                instance,
                                Some(cur_node.into()),
                            ) != 0
                            {
                                return -1;
                            }

                            while !test.is_null() {
                                self.run_test(
                                    test,
                                    instance,
                                    cur_node,
                                    // What is this ????
                                    // XmlPattern and XmlSchematronPattern are not compatible...
                                    // (*rule).pattern as XmlSchematronPatternPtr,
                                    null_mut(),
                                );
                                test = (*test).next;
                            }

                            if xml_schematron_unregister_variables(self.xctxt, (*rule).lets) != 0 {
                                return -1;
                            }
                        }
                        rule = (*rule).next;
                    }

                    cur = xml_schematron_next_node(cur_node);
                }
            } else {
                // Process all contexts one at a time
                pattern = (*self.schema).patterns;

                while !pattern.is_null() {
                    self.report_pattern(pattern);

                    // TODO convert the pattern rule to a direct XPath and
                    // compute directly instead of using the pattern matching
                    // over the full document...
                    // Check the exact semantic
                    let mut cur = Some(root);
                    while let Some(cur_node) = cur {
                        rule = (*pattern).rules;
                        while !rule.is_null() {
                            if (*rule)
                                .pattern
                                .as_deref()
                                .unwrap()
                                .pattern_match(cur_node.into())
                                == 1
                            {
                                test = (*rule).tests;
                                xml_schematron_register_variables(
                                    self.xctxt,
                                    (*rule).lets,
                                    instance,
                                    Some(cur_node.into()),
                                );

                                while !test.is_null() {
                                    self.run_test(test, instance, cur_node, pattern);
                                    test = (*test).next;
                                }

                                xml_schematron_unregister_variables(self.xctxt, (*rule).lets);
                            }
                            rule = (*rule).patnext;
                        }

                        cur = xml_schematron_next_node(cur_node);
                    }
                    pattern = (*pattern).next;
                }
            }
            self.nberrors
        }
    }
}

impl Default for XmlSchematronValidCtxt {
    fn default() -> Self {
        Self {
            typ: 0,
            flags: 0,
            nberrors: 0,
            err: 0,
            schema: null_mut(),
            xctxt: null_mut(),
            output_file: null_mut(),
            output_buffer: vec![],
            iowrite: None,
            ioclose: None,
            ioctx: null_mut(),
            user_data: None,
            error: None,
            warning: None,
            serror: None,
        }
    }
}

pub type XmlSchematronParserCtxtPtr = *mut XmlSchematronParserCtxt;
/// A schemas validation context
#[doc(alias = "xmlSchematronParserCtxt")]
#[repr(C)]
pub struct XmlSchematronParserCtxt {
    typ: i32,
    url: Option<String>,
    doc: Option<XmlDocPtr>,
    preserve: i32, /* Whether the doc should be freed  */
    buffer: *const c_char,
    size: i32,

    nberrors: i32,
    err: i32,
    xctxt: XmlXPathContextPtr, /* the XPath context used for compilation */
    schema: XmlSchematronPtr,

    // the array of namespaces
    // (href, prefix)
    namespaces: Vec<(String, Option<String>)>,

    includes: Vec<XmlNodePtr>, /* the array of includes */

    /* error reporting data */
    user_data: Option<GenericErrorContext>, /* user specific data block */
    error: Option<GenericError>,            /* the callback in case of errors */
    warning: Option<XmlSchematronValidityWarningFunc>, /* callback in case of warning */
    serror: Option<StructuredError>,        /* the structured function */
}

impl XmlSchematronParserCtxt {
    /// Add a namespace definition in the context
    #[doc(alias = "xmlSchematronAddNamespace")]
    fn add_namespace(&mut self, prefix: Option<&str>, ns: &str) {
        self.namespaces
            .push((ns.to_owned(), prefix.map(|pre| pre.to_owned())));
    }

    /// Add a pattern to a schematron
    ///
    /// Returns the new pointer or NULL in case of error
    #[doc(alias = "xmlSchematronAddPattern")]
    unsafe fn add_pattern(
        &mut self,
        schema: XmlSchematronPtr,
        node: XmlNodePtr,
        name: Option<&str>,
    ) -> XmlSchematronPatternPtr {
        unsafe {
            if schema.is_null() || name.is_none() {
                return null_mut();
            }

            let ret: XmlSchematronPatternPtr =
                xml_malloc(size_of::<XmlSchematronPattern>()) as XmlSchematronPatternPtr;
            if ret.is_null() {
                xml_schematron_perr_memory(self, "allocating schema pattern", Some(node.into()));
                return null_mut();
            }
            memset(ret as _, 0, size_of::<XmlSchematronPattern>());
            std::ptr::write(&mut (*ret).name, name.unwrap().to_owned());
            (*ret).next = null_mut();
            if (*schema).patterns.is_null() {
                (*schema).patterns = ret;
            } else {
                let mut prev: XmlSchematronPatternPtr = (*schema).patterns;

                while !(*prev).next.is_null() {
                    prev = (*prev).next;
                }
                (*prev).next = ret;
            }
            ret
        }
    }

    /// Add a rule to a schematron
    ///
    /// Returns the new pointer or NULL in case of error
    #[doc(alias = "xmlSchematronAddRule")]
    unsafe fn add_rule(
        &mut self,
        schema: XmlSchematronPtr,
        pat: XmlSchematronPatternPtr,
        node: XmlNodePtr,
        context: &str,
        report: Option<&str>,
    ) -> XmlSchematronRulePtr {
        unsafe {
            if schema.is_null() {
                return null_mut();
            }

            // Try first to compile the pattern
            let pattern = xml_pattern_compile(
                context,
                XmlPatternFlags::XmlPatternXPath as i32,
                Some(self.namespaces.clone()),
            );
            if pattern.is_none() {
                xml_schematron_perr!(
                    self,
                    Some(node.into()),
                    XmlParserErrors::XmlSchemapNoroot,
                    "Failed to compile context expression {}",
                    context
                );
            }

            let ret: XmlSchematronRulePtr =
                xml_malloc(size_of::<XmlSchematronRule>()) as XmlSchematronRulePtr;
            if ret.is_null() {
                xml_schematron_perr_memory(self, "allocating schema rule", Some(node.into()));
                return null_mut();
            }
            memset(ret as _, 0, size_of::<XmlSchematronRule>());
            (*ret).node = Some(node);
            std::ptr::write(&mut (*ret).context, context.to_owned());
            (*ret).pattern = pattern;
            std::ptr::write(&mut (*ret).report, report.map(|report| report.to_owned()));
            (*ret).next = null_mut();
            (*ret).lets = null_mut();
            if (*schema).rules.is_null() {
                (*schema).rules = ret;
            } else {
                let mut prev: XmlSchematronRulePtr = (*schema).rules;

                while !(*prev).next.is_null() {
                    prev = (*prev).next;
                }
                (*prev).next = ret;
            }
            (*ret).patnext = null_mut();
            if (*pat).rules.is_null() {
                (*pat).rules = ret;
            } else {
                let mut prev: XmlSchematronRulePtr = (*pat).rules;

                while !(*prev).patnext.is_null() {
                    prev = (*prev).patnext;
                }
                (*prev).patnext = ret;
            }
            ret
        }
    }

    /// Add a test to a schematron
    ///
    /// Returns the new pointer or NULL in case of error
    #[doc(alias = "xmlSchematronAddTest")]
    unsafe fn add_test(
        &mut self,
        typ: XmlSchematronTestType,
        rule: XmlSchematronRulePtr,
        node: XmlNodePtr,
        test: &str,
        report: Option<&str>,
    ) -> XmlSchematronTestPtr {
        unsafe {
            if rule.is_null() {
                return null_mut();
            }

            // try first to compile the test expression
            let comp: XmlXPathCompExprPtr = xml_xpath_ctxt_compile(self.xctxt, test);
            if comp.is_null() {
                xml_schematron_perr!(
                    self,
                    Some(node.into()),
                    XmlParserErrors::XmlSchemapNoroot,
                    "Failed to compile test expression {}",
                    test
                );
                return null_mut();
            }

            let ret: XmlSchematronTestPtr =
                xml_malloc(size_of::<XmlSchematronTest>()) as XmlSchematronTestPtr;
            if ret.is_null() {
                xml_schematron_perr_memory(self, "allocating schema test", Some(node.into()));
                return null_mut();
            }
            memset(ret as _, 0, size_of::<XmlSchematronTest>());
            (*ret).typ = typ;
            (*ret).node = Some(node);
            std::ptr::write(&mut (*ret).test, test.to_owned());
            (*ret).comp = comp;
            std::ptr::write(&mut (*ret).report, report.map(|report| report.to_owned()));
            (*ret).next = null_mut();
            if (*rule).tests.is_null() {
                (*rule).tests = ret;
            } else {
                let mut prev: XmlSchematronTestPtr = (*rule).tests;

                while !(*prev).next.is_null() {
                    prev = (*prev).next;
                }
                (*prev).next = ret;
            }
            ret
        }
    }

    /// Parse a rule element
    #[doc(alias = "xmlSchematronParseRule")]
    unsafe fn parse_rule(&mut self, pattern: XmlSchematronPatternPtr, rule: XmlNodePtr) {
        unsafe {
            let mut nb_checks: i32 = 0;
            let ruleptr: XmlSchematronRulePtr;

            match rule.get_no_ns_prop("context") {
                Some(context) if context.is_empty() => {
                    xml_schematron_perr!(
                        self,
                        Some(rule.into()),
                        XmlParserErrors::XmlSchemapNoroot,
                        "rule has an empty context attribute"
                    );
                    return;
                }
                Some(context) => {
                    ruleptr = self.add_rule(self.schema, pattern, rule, &context, None);
                    if ruleptr.is_null() {
                        return;
                    }
                }
                None => {
                    xml_schematron_perr!(
                        self,
                        Some(rule.into()),
                        XmlParserErrors::XmlSchemapNoroot,
                        "rule has no context attribute"
                    );
                    return;
                }
            }

            let mut cur = rule.children().map(|c| XmlNodePtr::try_from(c).unwrap());
            cur = next_schematron(cur);
            while let Some(cur_node) = cur {
                if is_schematron(cur_node, "let") {
                    let name = match cur_node.get_no_ns_prop("name") {
                        Some(name) if name.is_empty() => {
                            xml_schematron_perr!(
                                self,
                                Some(cur_node.into()),
                                XmlParserErrors::XmlSchemapNoroot,
                                "let has an empty name attribute"
                            );
                            return;
                        }
                        Some(name) => {
                            let name = CString::new(name).unwrap();
                            xml_strdup(name.as_ptr() as *const u8)
                        }
                        None => {
                            xml_schematron_perr!(
                                self,
                                Some(cur_node.into()),
                                XmlParserErrors::XmlSchemapNoroot,
                                "let has no name attribute"
                            );
                            return;
                        }
                    };
                    let value = match cur_node.get_no_ns_prop("value") {
                        Some(value) if value.is_empty() => {
                            xml_schematron_perr!(
                                self,
                                Some(cur_node.into()),
                                XmlParserErrors::XmlSchemapNoroot,
                                "let has an empty value attribute"
                            );
                            return;
                        }
                        Some(value) => value,
                        None => {
                            xml_schematron_perr!(
                                self,
                                Some(cur_node.into()),
                                XmlParserErrors::XmlSchemapNoroot,
                                "let has no value attribute"
                            );
                            return;
                        }
                    };

                    let var_comp: XmlXPathCompExprPtr = xml_xpath_ctxt_compile(self.xctxt, &value);
                    if var_comp.is_null() {
                        xml_schematron_perr!(
                            self,
                            Some(cur_node.into()),
                            XmlParserErrors::XmlSchemapNoroot,
                            "Failed to compile let expression {}",
                            value
                        );
                        return;
                    }

                    let letr: XmlSchematronLetPtr =
                        malloc(size_of::<XmlSchematronLet>()) as XmlSchematronLetPtr;
                    (*letr).name = name;
                    (*letr).comp = var_comp;
                    (*letr).next = null_mut();

                    // add new let variable to the beginning of the list
                    if !(*ruleptr).lets.is_null() {
                        (*letr).next = (*ruleptr).lets;
                    }
                    (*ruleptr).lets = letr;
                } else if is_schematron(cur_node, "assert") {
                    nb_checks += 1;
                    match cur_node.get_no_ns_prop("test") {
                        Some(test) if test.is_empty() => {
                            xml_schematron_perr!(
                                self,
                                Some(cur_node.into()),
                                XmlParserErrors::XmlSchemapNoroot,
                                "assert has an empty test attribute"
                            );
                        }
                        Some(test) => {
                            self.parse_test_report_msg(cur_node);
                            let report = cur_node.get_content();

                            self.add_test(
                                XmlSchematronTestType::XmlSchematronAssert,
                                ruleptr,
                                cur_node,
                                &test,
                                report.as_deref(),
                            );
                        }
                        None => {
                            xml_schematron_perr!(
                                self,
                                Some(cur_node.into()),
                                XmlParserErrors::XmlSchemapNoroot,
                                "assert has no test attribute"
                            );
                        }
                    }
                } else if is_schematron(cur_node, "report") {
                    nb_checks += 1;
                    match cur_node.get_no_ns_prop("test") {
                        Some(test) if test.is_empty() => {
                            xml_schematron_perr!(
                                self,
                                Some(cur_node.into()),
                                XmlParserErrors::XmlSchemapNoroot,
                                "assert has an empty test attribute"
                            );
                        }
                        Some(test) => {
                            self.parse_test_report_msg(cur_node);
                            let report = cur_node.get_content();

                            self.add_test(
                                XmlSchematronTestType::XmlSchematronReport,
                                ruleptr,
                                cur_node,
                                &test,
                                report.as_deref(),
                            );
                        }
                        None => {
                            xml_schematron_perr!(
                                self,
                                Some(cur_node.into()),
                                XmlParserErrors::XmlSchemapNoroot,
                                "assert has no test attribute"
                            );
                        }
                    }
                } else {
                    xml_schematron_perr!(
                        self,
                        Some(cur_node.into()),
                        XmlParserErrors::XmlSchemapNoroot,
                        "Expecting an assert or a report element instead of {}",
                        cur_node.name().unwrap().into_owned()
                    );
                }
                cur = cur_node
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
                cur = next_schematron(cur);
            }
            if nb_checks == 0 {
                xml_schematron_perr!(
                    self,
                    Some(rule.into()),
                    XmlParserErrors::XmlSchemapNoroot,
                    "rule has no assert nor report element"
                );
            }
        }
    }

    /// Parse a pattern element
    #[doc(alias = "xmlSchematronParsePattern")]
    unsafe fn parse_pattern(&mut self, pat: XmlNodePtr) {
        unsafe {
            let mut nb_rules: i32 = 0;

            let id = pat
                .get_no_ns_prop("id")
                .or_else(|| pat.get_no_ns_prop("name"));
            let pattern: XmlSchematronPatternPtr =
                self.add_pattern(self.schema, pat, id.as_deref());
            if pattern.is_null() {
                return;
            }
            let mut cur = pat.children().map(|c| XmlNodePtr::try_from(c).unwrap());
            cur = next_schematron(cur);
            while let Some(cur_node) = cur {
                if is_schematron(cur_node, "rule") {
                    self.parse_rule(pattern, cur_node);
                    nb_rules += 1;
                } else {
                    xml_schematron_perr!(
                        self,
                        Some(cur_node.into()),
                        XmlParserErrors::XmlSchemapNoroot,
                        "Expecting a rule element instead of {}",
                        cur_node.name().unwrap().into_owned()
                    );
                }
                cur = cur_node
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
                cur = next_schematron(cur);
            }
            if nb_rules == 0 {
                xml_schematron_perr!(
                    self,
                    Some(pat.into()),
                    XmlParserErrors::XmlSchemapNoroot,
                    "Pattern has no rule element"
                );
            }
        }
    }

    /// Format the message content of the assert or report test
    #[doc(alias = "xmlSchematronParseTestReportMsg")]
    unsafe fn parse_test_report_msg(&mut self, con: XmlNodePtr) {
        unsafe {
            let mut comp: XmlXPathCompExprPtr;

            let mut child = con.children().map(|c| XmlNodePtr::try_from(c).unwrap());
            while let Some(cur_node) = child {
                #[allow(clippy::if_same_then_else)]
                if cur_node.element_type() == XmlElementType::XmlTextNode
                    || cur_node.element_type() == XmlElementType::XmlCDATASectionNode
                {
                    // Do Nothing
                } else if is_schematron(cur_node, "name") {
                    // Do Nothing
                } else if is_schematron(cur_node, "value-of") {
                    if let Some(select) = cur_node.get_no_ns_prop("select") {
                        // try first to compile the test expression
                        comp = xml_xpath_ctxt_compile(self.xctxt, &select);
                        if comp.is_null() {
                            xml_schematron_perr!(
                                self,
                                Some(cur_node.into()),
                                XmlParserErrors::XmlSchemavAttrInvalid,
                                "Failed to compile select expression {}",
                                select
                            );
                        } else {
                            xml_schematron_perr!(
                                self,
                                Some(cur_node.into()),
                                XmlParserErrors::XmlSchemavAttrInvalid,
                                "value-of has no select attribute"
                            );
                        }
                        xml_xpath_free_comp_expr(comp);
                    }
                }
                child = cur_node
                    .next
                    .map(|node| XmlNodePtr::try_from(node).unwrap());
                continue;
            }
        }
    }

    /// Parse a schema definition resource and build an internal
    /// XML Schema structure which can be used to validate instances.
    ///
    /// Returns the internal XML Schematron structure built from the resource or NULL in case of error
    #[doc(alias = "xmlSchematronParse")]
    pub unsafe fn parse(&mut self) -> XmlSchematronPtr {
        unsafe {
            let mut ret: XmlSchematronPtr = null_mut();
            let mut preserve: i32 = 0;

            self.nberrors = 0;

            // First step is to parse the input document into an DOM/Infoset
            let doc = if let Some(url) = self.url.as_deref() {
                let Some(doc) = xml_read_file(url, None, SCHEMATRON_PARSE_OPTIONS as i32) else {
                    let url = url.to_owned();
                    xml_schematron_perr!(
                        self,
                        None::<XmlGenericNodePtr>,
                        XmlParserErrors::XmlSchemapFailedLoad,
                        "xmlSchematronParse: could not load '{}'.\n",
                        url
                    );
                    return null_mut();
                };
                self.preserve = 0;
                doc
            } else if !self.buffer.is_null() {
                let mem = from_raw_parts(self.buffer as *const u8, self.size as usize).to_vec();
                let Some(mut doc) =
                    xml_read_memory(mem, None, None, SCHEMATRON_PARSE_OPTIONS as i32)
                else {
                    xml_schematron_perr!(
                        self,
                        None::<XmlGenericNodePtr>,
                        XmlParserErrors::XmlSchemapFailedParse,
                        "xmlSchematronParse: could not parse.\n"
                    );
                    return null_mut();
                };
                doc.url = Some("in_memory_buffer".to_owned());
                self.url = Some("in_memory_buffer".to_owned());
                self.preserve = 0;
                doc
            } else if let Some(doc) = self.doc {
                preserve = 1;
                self.preserve = 1;
                doc
            } else {
                xml_schematron_perr!(
                    self,
                    None::<XmlGenericNodePtr>,
                    XmlParserErrors::XmlSchemapNothingToParse,
                    "xmlSchematronParse: could not parse.\n"
                );
                return null_mut();
            };

            // Then extract the root and Schematron parse it
            let Some(root) = doc.get_root_element() else {
                xml_schematron_perr!(
                    self,
                    Some(doc.into()),
                    XmlParserErrors::XmlSchemapNoroot,
                    "The schema has no document element.\n"
                );
                if preserve == 0 {
                    xml_free_doc(doc);
                }
                return null_mut();
            };

            if !is_schematron(root, "schema") {
                xml_schematron_perr!(
                    self,
                    Some(root.into()),
                    XmlParserErrors::XmlSchemapNoroot,
                    "The XML document '{}' is not a XML schematron document",
                    self.url.as_deref().unwrap()
                );
                // goto exit;
            } else {
                ret = xml_schematron_new_schematron(self);
                if ret.is_null() {
                    // goto exit;
                } else {
                    self.schema = ret;

                    // scan the schema elements
                    let cur = root.children().map(|c| XmlNodePtr::try_from(c).unwrap());
                    let mut cur = next_schematron(cur).unwrap();
                    if is_schematron(cur, "title") {
                        if let Some(title) = cur.get_content() {
                            (*ret).title = Some(title);
                        }
                        cur = next_schematron(
                            cur.next.map(|node| XmlNodePtr::try_from(node).unwrap()),
                        )
                        .unwrap();
                    }
                    let mut next = Some(cur);
                    while is_schematron(cur, "ns") {
                        let prefix = cur.get_no_ns_prop("prefix");
                        let uri = cur.get_no_ns_prop("uri");
                        if uri.as_deref().is_none_or(|uri| uri.is_empty()) {
                            xml_schematron_perr!(
                                self,
                                Some(cur.into()),
                                XmlParserErrors::XmlSchemapNoroot,
                                "ns element has no uri"
                            );
                        }
                        if prefix.as_deref().is_none_or(|pre| pre.is_empty()) {
                            xml_schematron_perr!(
                                self,
                                Some(cur.into()),
                                XmlParserErrors::XmlSchemapNoroot,
                                "ns element has no prefix"
                            );
                        }
                        if let (Some(prefix), Some(uri)) = (prefix, uri) {
                            xml_xpath_register_ns(self.xctxt, &prefix, Some(&uri));
                            self.add_namespace(Some(&prefix), &uri);
                            (*ret).nb_ns += 1;
                        }
                        next = next_schematron(
                            cur.next.map(|node| XmlNodePtr::try_from(node).unwrap()),
                        );
                        if let Some(next) = next {
                            cur = next;
                        } else {
                            break;
                        }
                    }
                    let mut cur = next;
                    while let Some(cur_node) = cur {
                        if is_schematron(cur_node, "pattern") {
                            self.parse_pattern(cur_node);
                            (*ret).nb_pattern += 1;
                        } else {
                            xml_schematron_perr!(
                                self,
                                Some(cur_node.into()),
                                XmlParserErrors::XmlSchemapNoroot,
                                "Expecting a pattern element instead of {}",
                                cur_node.name().unwrap().into_owned()
                            );
                        }
                        cur = cur_node
                            .next
                            .map(|node| XmlNodePtr::try_from(node).unwrap());
                        cur = next_schematron(cur);
                    }
                    if (*ret).nb_pattern == 0 {
                        xml_schematron_perr!(
                            self,
                            Some(root.into()),
                            XmlParserErrors::XmlSchemapNoroot,
                            "The schematron document '{}' has no pattern",
                            self.url.as_deref().unwrap()
                        );
                    // goto exit;
                    } else {
                        // the original document must be kept for reporting
                        (*ret).doc = Some(doc);
                        if preserve != 0 {
                            (*ret).preserve = 1;
                        }
                        preserve = 1;
                    }
                }
            }

            // exit:
            if preserve == 0 {
                xml_free_doc(doc);
            }
            if !ret.is_null() {
                if self.nberrors != 0 {
                    xml_schematron_free(ret);
                    ret = null_mut();
                } else {
                    (*ret).namespaces = take(&mut self.namespaces);
                }
            }
            ret
        }
    }
}

impl Default for XmlSchematronParserCtxt {
    fn default() -> Self {
        Self {
            typ: 0,
            url: None,
            doc: None,
            preserve: 0,
            buffer: null_mut(),
            size: 0,
            nberrors: 0,
            err: 0,
            xctxt: null_mut(),
            schema: null_mut(),
            namespaces: vec![],
            includes: vec![],
            user_data: None,
            error: None,
            warning: None,
            serror: None,
        }
    }
}

const XML_STRON_CTXT_PARSER: i32 = 1;
const XML_STRON_CTXT_VALIDATOR: i32 = 2;

const SCHEMATRON_PARSE_OPTIONS: XmlParserOption = XmlParserOption::XmlParseNoEnt;

const SCT_OLD_NS: &str = "http://www.ascc.net/xml/schematron";

const XML_SCHEMATRON_NS: &str = "http://purl.oclc.org/dsdl/schematron";

const XML_OLD_SCHEMATRON_NS: &str = SCT_OLD_NS;

/// Handle an out of memory condition
#[doc(alias = "xmlSchematronPErrMemory")]
unsafe fn xml_schematron_perr_memory(
    ctxt: XmlSchematronParserCtxtPtr,
    extra: &str,
    node: Option<XmlGenericNodePtr>,
) {
    unsafe {
        if !ctxt.is_null() {
            (*ctxt).nberrors += 1;
        }
        __xml_simple_oom_error(XmlErrorDomain::XmlFromSchemasp, node, Some(extra));
    }
}

/// Create an XML Schematrons parse context for that file/resource expected
/// to contain an XML Schematrons file.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlSchematronNewParserCtxt")]
pub unsafe fn xml_schematron_new_parser_ctxt(url: &str) -> XmlSchematronParserCtxtPtr {
    unsafe {
        let ret: XmlSchematronParserCtxtPtr =
            xml_malloc(size_of::<XmlSchematronParserCtxt>()) as XmlSchematronParserCtxtPtr;
        if ret.is_null() {
            xml_schematron_perr_memory(null_mut(), "allocating schema parser context", None);
            return null_mut();
        }
        std::ptr::write(&mut *ret, XmlSchematronParserCtxt::default());
        (*ret).typ = XML_STRON_CTXT_PARSER;
        (*ret).url = Some(url.to_owned());
        (*ret).xctxt = xml_xpath_new_context(None);
        if (*ret).xctxt.is_null() {
            xml_schematron_perr_memory(null_mut(), "allocating schema parser XPath context", None);
            xml_schematron_free_parser_ctxt(ret);
            return null_mut();
        }
        (*(*ret).xctxt).flags = XML_XPATH_CHECKNS as i32;
        ret
    }
}

/// Create an XML Schematrons parse context for that memory buffer expected
/// to contain an XML Schematrons file.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlSchematronNewMemParserCtxt")]
pub unsafe fn xml_schematron_new_mem_parser_ctxt(
    buffer: *const c_char,
    size: i32,
) -> XmlSchematronParserCtxtPtr {
    unsafe {
        if buffer.is_null() || size <= 0 {
            return null_mut();
        }

        let ret: XmlSchematronParserCtxtPtr =
            xml_malloc(size_of::<XmlSchematronParserCtxt>()) as XmlSchematronParserCtxtPtr;
        if ret.is_null() {
            xml_schematron_perr_memory(null_mut(), "allocating schema parser context", None);
            return null_mut();
        }
        std::ptr::write(&mut *ret, XmlSchematronParserCtxt::default());
        (*ret).buffer = buffer;
        (*ret).size = size;
        (*ret).xctxt = xml_xpath_new_context(None);
        if (*ret).xctxt.is_null() {
            xml_schematron_perr_memory(null_mut(), "allocating schema parser XPath context", None);
            xml_schematron_free_parser_ctxt(ret);
            return null_mut();
        }
        ret
    }
}

/// Create an XML Schematrons parse context for that document.
/// NB. The document may be modified during the parsing process.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlSchematronNewDocParserCtxt")]
pub unsafe fn xml_schematron_new_doc_parser_ctxt(doc: XmlDocPtr) -> XmlSchematronParserCtxtPtr {
    unsafe {
        let ret: XmlSchematronParserCtxtPtr =
            xml_malloc(size_of::<XmlSchematronParserCtxt>()) as XmlSchematronParserCtxtPtr;
        if ret.is_null() {
            xml_schematron_perr_memory(null_mut(), "allocating schema parser context", None);
            return null_mut();
        }
        std::ptr::write(&mut *ret, XmlSchematronParserCtxt::default());
        (*ret).doc = Some(doc);
        // The application has responsibility for the document
        (*ret).preserve = 1;
        (*ret).xctxt = xml_xpath_new_context(Some(doc));
        if (*ret).xctxt.is_null() {
            xml_schematron_perr_memory(null_mut(), "allocating schema parser XPath context", None);
            xml_schematron_free_parser_ctxt(ret);
            return null_mut();
        }

        ret
    }
}

/// Free the resources associated to the schema parser context
#[doc(alias = "xmlSchematronFreeParserCtxt")]
pub unsafe fn xml_schematron_free_parser_ctxt(ctxt: XmlSchematronParserCtxtPtr) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        if let Some(doc) = (*ctxt).doc.filter(|_| (*ctxt).preserve == 0) {
            xml_free_doc(doc);
        }
        if !(*ctxt).xctxt.is_null() {
            xml_xpath_free_context((*ctxt).xctxt);
        }
        drop_in_place(ctxt);
        xml_free(ctxt as _);
    }
}

unsafe fn is_schematron(node: XmlNodePtr, elem: &str) -> bool {
    unsafe {
        node.element_type() == XmlElementType::XmlElementNode
            && node.name().as_deref() == Some(elem)
            && node.ns.is_some_and(|ns| {
                ns.href().as_deref() == Some(XML_SCHEMATRON_NS)
                    || ns.href().as_deref() == Some(XML_OLD_SCHEMATRON_NS)
            })
    }
}

unsafe fn next_schematron(mut node: Option<XmlNodePtr>) -> Option<XmlNodePtr> {
    unsafe {
        while let Some(cur) = node {
            if cur.element_type() == XmlElementType::XmlElementNode
                && cur.ns.is_some_and(|ns| {
                    ns.href().as_deref() == Some(XML_SCHEMATRON_NS)
                        || ns.href().as_deref() == Some(XML_OLD_SCHEMATRON_NS)
                })
            {
                break;
            }
            node = cur.next.map(|node| XmlNodePtr::try_from(node).unwrap());
        }
        node
    }
}

/// Allocate a new Schematron structure.
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlSchematronNewSchematron")]
unsafe fn xml_schematron_new_schematron(ctxt: XmlSchematronParserCtxtPtr) -> XmlSchematronPtr {
    unsafe {
        let ret: XmlSchematronPtr = xml_malloc(size_of::<XmlSchematron>()) as XmlSchematronPtr;
        if ret.is_null() {
            xml_schematron_perr_memory(ctxt, "allocating schema", None);
            return null_mut();
        }
        std::ptr::write(&mut *ret, XmlSchematron::default());

        ret
    }
}

/// Free a list of tests.
#[doc(alias = "xmlSchematronFreeTests")]
unsafe fn xml_schematron_free_tests(mut tests: XmlSchematronTestPtr) {
    unsafe {
        let mut next: XmlSchematronTestPtr;

        while !tests.is_null() {
            next = (*tests).next;
            if !(*tests).comp.is_null() {
                xml_xpath_free_comp_expr((*tests).comp);
            }
            drop_in_place(tests);
            xml_free(tests as _);
            tests = next;
        }
    }
}

/// Free a list of let variables.
#[doc(alias = "xmlSchematronFreeLets")]
unsafe fn xml_schematron_free_lets(mut lets: XmlSchematronLetPtr) {
    unsafe {
        let mut next: XmlSchematronLetPtr;

        while !lets.is_null() {
            next = (*lets).next;
            if !(*lets).name.is_null() {
                xml_free((*lets).name as _);
            }
            if !(*lets).comp.is_null() {
                xml_xpath_free_comp_expr((*lets).comp);
            }
            xml_free(lets as _);
            lets = next;
        }
    }
}

/// Free a list of rules.
#[doc(alias = "xmlSchematronFreeRules")]
unsafe fn xml_schematron_free_rules(mut rules: XmlSchematronRulePtr) {
    unsafe {
        let mut next: XmlSchematronRulePtr;

        while !rules.is_null() {
            next = (*rules).next;
            if !(*rules).tests.is_null() {
                xml_schematron_free_tests((*rules).tests);
            }
            (*rules).pattern.take();
            (*rules).report.take();
            if !(*rules).lets.is_null() {
                xml_schematron_free_lets((*rules).lets);
            }
            drop_in_place(rules);
            xml_free(rules as _);
            rules = next;
        }
    }
}

/// Free a list of patterns.
#[doc(alias = "xmlSchematronFreePatterns")]
unsafe fn xml_schematron_free_patterns(mut patterns: XmlSchematronPatternPtr) {
    unsafe {
        let mut next: XmlSchematronPatternPtr;

        while !patterns.is_null() {
            next = (*patterns).next;
            drop_in_place(patterns);
            xml_free(patterns as _);
            patterns = next;
        }
    }
}

/// Deallocate a Schematron structure.
#[doc(alias = "xmlSchematronFree")]
pub unsafe fn xml_schematron_free(schema: XmlSchematronPtr) {
    unsafe {
        if schema.is_null() {
            return;
        }

        if let Some(doc) = (*schema).doc.filter(|_| (*schema).preserve == 0) {
            xml_free_doc(doc);
        }

        xml_schematron_free_rules((*schema).rules);
        xml_schematron_free_patterns((*schema).patterns);
        drop_in_place(schema);
        xml_free(schema as _);
    }
}

/// Handle an out of memory condition
#[doc(alias = "xmlSchematronVTypeErrMemory")]
unsafe fn xml_schematron_verr_memory(
    ctxt: XmlSchematronValidCtxtPtr,
    extra: &str,
    node: Option<XmlGenericNodePtr>,
) {
    unsafe {
        if !ctxt.is_null() {
            (*ctxt).nberrors += 1;
            (*ctxt).err = XmlParserErrors::XmlSchemavInternal as i32;
        }
        __xml_simple_oom_error(XmlErrorDomain::XmlFromSchemasv, node, Some(extra));
    }
}

/// Create an XML Schematrons validation context based on the given schema.
///
/// Returns the validation context or NULL in case of error
#[doc(alias = "xmlSchematronNewValidCtxt")]
pub unsafe fn xml_schematron_new_valid_ctxt(
    schema: XmlSchematronPtr,
    options: i32,
) -> XmlSchematronValidCtxtPtr {
    unsafe {
        let ret: XmlSchematronValidCtxtPtr =
            xml_malloc(size_of::<XmlSchematronValidCtxt>()) as XmlSchematronValidCtxtPtr;
        if ret.is_null() {
            xml_schematron_verr_memory(null_mut(), "allocating validation context", None);
            return null_mut();
        }
        std::ptr::write(&mut *ret, XmlSchematronValidCtxt::default());
        (*ret).typ = XML_STRON_CTXT_VALIDATOR;
        (*ret).schema = schema;
        (*ret).xctxt = xml_xpath_new_context(None);
        (*ret).flags = options;
        if (*ret).xctxt.is_null() {
            xml_schematron_perr_memory(null_mut(), "allocating schema parser XPath context", None);
            xml_schematron_free_valid_ctxt(ret);
            return null_mut();
        }
        for (href, pref) in &(*schema).namespaces {
            let Some(pref) = pref.as_deref() else {
                break;
            };
            xml_xpath_register_ns((*ret).xctxt, pref, Some(href.as_str()));
        }
        ret
    }
}

/// Free the resources associated to the schema validation context
#[doc(alias = "xmlSchematronFreeValidCtxt")]
pub unsafe fn xml_schematron_free_valid_ctxt(ctxt: XmlSchematronValidCtxtPtr) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        if !(*ctxt).xctxt.is_null() {
            xml_xpath_free_context((*ctxt).xctxt);
        }
        xml_free(ctxt as _);
    }
}

/// Registers a list of let variables to the current context of @cur
///
/// Returns -1 in case of errors, otherwise 0
#[doc(alias = "xmlSchematronRegisterVariables")]
unsafe fn xml_schematron_register_variables(
    ctxt: XmlXPathContextPtr,
    mut letr: XmlSchematronLetPtr,
    instance: XmlDocPtr,
    cur: Option<XmlGenericNodePtr>,
) -> i32 {
    unsafe {
        let mut let_eval: XmlXPathObjectPtr;

        (*ctxt).doc = Some(instance);
        (*ctxt).node = cur;
        while !letr.is_null() {
            let_eval = xml_xpath_compiled_eval((*letr).comp, ctxt);
            if let_eval.is_null() {
                generic_error!("Evaluation of compiled expression failed\n");
                return -1;
            }
            if xml_xpath_register_variable_ns(ctxt, (*letr).name, null_mut(), let_eval) != 0 {
                generic_error!("Registering a let variable failed\n");
                return -1;
            }
            letr = (*letr).next;
        }
        0
    }
}

/// Unregisters a list of let variables from the context
///
/// Returns -1 in case of errors, otherwise 0
#[doc(alias = "xmlSchematronUnregisterVariables")]
unsafe fn xml_schematron_unregister_variables(
    ctxt: XmlXPathContextPtr,
    mut letr: XmlSchematronLetPtr,
) -> i32 {
    unsafe {
        while !letr.is_null() {
            if xml_xpath_register_variable_ns(ctxt, (*letr).name, null_mut(), null_mut()) != 0 {
                generic_error!("Unregistering a let variable failed\n");
                return -1;
            }
            letr = (*letr).next;
        }
        0
    }
}

unsafe fn xml_schematron_next_node(cur: XmlNodePtr) -> Option<XmlNodePtr> {
    let mut cur = if let Some(children) = cur.children() {
        // Do not descend on entities declarations
        if children.element_type() != XmlElementType::XmlEntityDecl {
            // Skip DTDs
            if children.element_type() != XmlElementType::XmlDTDNode {
                return Some(XmlNodePtr::try_from(children).unwrap());
            }
            children
        } else {
            XmlGenericNodePtr::from(cur)
        }
    } else {
        XmlGenericNodePtr::from(cur)
    };

    while let Some(next) = cur.next() {
        cur = next;
        if cur.element_type() != XmlElementType::XmlEntityDecl
            && cur.element_type() != XmlElementType::XmlDTDNode
        {
            return Some(XmlNodePtr::try_from(cur).unwrap());
        }
    }

    loop {
        cur = cur.parent()?;
        if cur.element_type() == XmlElementType::XmlDocumentNode {
            break None;
        }
        if let Some(next) = cur.next() {
            break Some(XmlNodePtr::try_from(next).unwrap());
        }
    }
}

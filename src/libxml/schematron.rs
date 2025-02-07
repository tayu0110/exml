//! Provide methods and data structures for XML Schematron.  
//! This module is based on `libxml/schematron.h`, `schematron.c`, and so on in `libxml2-v2.11.8`.
//!
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
    ffi::{c_char, CStr, CString},
    mem::size_of,
    os::raw::c_void,
    ptr::null_mut,
    slice::from_raw_parts,
};

use libc::{malloc, memset, snprintf, sprintf, FILE};

use crate::{
    error::{XmlErrorDomain, XmlParserErrors, __xml_raise_error, __xml_simple_oom_error},
    generic_error,
    globals::{GenericError, GenericErrorContext, StructuredError},
    io::{XmlOutputCloseCallback, XmlOutputWriteCallback},
    libxml::xmlstring::xml_str_equal,
    parser::{xml_read_file, xml_read_memory},
    tree::{xml_free_doc, NodeCommon, XmlDocPtr, XmlElementType, XmlNode},
    xpath::{
        internals::{xml_xpath_register_ns, xml_xpath_register_variable_ns},
        xml_xpath_compiled_eval, xml_xpath_ctxt_compile, xml_xpath_eval, xml_xpath_free_comp_expr,
        xml_xpath_free_context, xml_xpath_free_object, xml_xpath_is_nan, xml_xpath_new_context,
        XmlXPathCompExprPtr, XmlXPathContextPtr, XmlXPathObjectPtr, XmlXPathObjectType,
        XML_XPATH_CHECKNS,
    },
};

use super::{
    dict::{xml_dict_create, xml_dict_free, xml_dict_lookup, xml_dict_reference, XmlDictPtr},
    globals::{xml_free, xml_malloc},
    parser::XmlParserOption,
    pattern::{
        xml_free_pattern, xml_pattern_match, xml_patterncompile, XmlPatternFlags, XmlPatternPtr,
    },
    xmlstring::{xml_strcat, xml_strdup, xml_strlen, XmlChar},
};

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlSchematronValidOptions {
    XmlSchematronOutQuiet = 1 << 0,  /* quiet no report */
    XmlSchematronOutText = 1 << 1,   /* build a textual report */
    XmlSchematronOutXml = 1 << 2,    /* output SVRL */
    XmlSchematronOutError = 1 << 3,  /* output via xmlStructuredErrorFunc */
    XmlSchematronOutFile = 1 << 8,   /* output to a file descriptor */
    XmlSchematronOutBuffer = 1 << 9, /* output to a buffer */
    XmlSchematronOutIo = 1 << 10,    /* output to I/O mechanism */
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
    node: *mut XmlNode,         /* the node in the tree */
    test: *mut XmlChar,         /* the expression to test */
    comp: XmlXPathCompExprPtr,  /* the compiled expression */
    report: *mut XmlChar,       /* the message to report */
}

pub type XmlSchematronRulePtr = *mut XmlSchematronRule;
/// A Schematrons rule
#[doc(alias = "xmlSchematronRule")]
#[repr(C)]
pub struct XmlSchematronRule {
    next: XmlSchematronRulePtr,    /* the next rule in the list */
    patnext: XmlSchematronRulePtr, /* the next rule in the pattern list */
    node: *mut XmlNode,            /* the node in the tree */
    context: *mut XmlChar,         /* the context evaluation rule */
    tests: XmlSchematronTestPtr,   /* the list of tests */
    pattern: XmlPatternPtr,        /* the compiled pattern associated */
    report: *mut XmlChar,          /* the message to report */
    lets: XmlSchematronLetPtr,     /* the list of let variables */
}

pub type XmlSchematronPatternPtr = *mut XmlSchematronPattern;
/// A Schematrons pattern
#[doc(alias = "xmlSchematronPattern")]
#[repr(C)]
pub struct XmlSchematronPattern {
    next: XmlSchematronPatternPtr, /* the next pattern in the list */
    rules: XmlSchematronRulePtr,   /* the list of rules */
    name: *mut XmlChar,            /* the name of the pattern */
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
    dict: XmlDictPtr,      /* the dictionary used internally */

    title: *const XmlChar, /* the title if any */

    nb_ns: i32, /* the number of namespaces */

    nb_pattern: i32,                                 /* the number of patterns */
    patterns: XmlSchematronPatternPtr,               /* the patterns found */
    rules: XmlSchematronRulePtr,                     /* the rules gathered */
    namespaces: Option<Vec<(*const u8, *const u8)>>, /* the array of namespaces */
}

impl Default for XmlSchematron {
    fn default() -> Self {
        Self {
            name: null_mut(),
            preserve: 0,
            doc: None,
            flags: 0,
            _private: null_mut(),
            dict: null_mut(),
            title: null_mut(),
            nb_ns: 0,
            nb_pattern: 0,
            patterns: null_mut(),
            rules: null_mut(),
            namespaces: None,
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

    dict: XmlDictPtr,
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

impl Default for XmlSchematronValidCtxt {
    fn default() -> Self {
        Self {
            typ: 0,
            flags: 0,
            dict: null_mut(),
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
#[repr(C)]
pub struct XmlSchematronParserCtxt {
    typ: i32,
    url: *const XmlChar,
    doc: Option<XmlDocPtr>,
    preserve: i32, /* Whether the doc should be freed  */
    buffer: *const c_char,
    size: i32,

    dict: XmlDictPtr, /* dictionary for interned string names */

    nberrors: i32,
    err: i32,
    xctxt: XmlXPathContextPtr, /* the XPath context used for compilation */
    schema: XmlSchematronPtr,

    namespaces: Option<Vec<(*const u8, *const u8)>>, /* the array of namespaces */

    nb_includes: i32,            /* number of includes in the array */
    max_includes: i32,           /* size of the array */
    includes: *mut *mut XmlNode, /* the array of includes */

    /* error reporting data */
    user_data: Option<GenericErrorContext>, /* user specific data block */
    error: Option<GenericError>,            /* the callback in case of errors */
    warning: Option<XmlSchematronValidityWarningFunc>, /* callback in case of warning */
    serror: Option<StructuredError>,        /* the structured function */
}

impl Default for XmlSchematronParserCtxt {
    fn default() -> Self {
        Self {
            typ: 0,
            url: null_mut(),
            doc: None,
            preserve: 0,
            buffer: null_mut(),
            size: 0,
            dict: null_mut(),
            nberrors: 0,
            err: 0,
            xctxt: null_mut(),
            schema: null_mut(),
            namespaces: None,
            nb_includes: 0,
            max_includes: 0,
            includes: null_mut(),
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

const SCT_OLD_NS: &CStr = c"http://www.ascc.net/xml/schematron";

const XML_SCHEMATRON_NS: &CStr = c"http://purl.oclc.org/dsdl/schematron";

const XML_OLD_SCHEMATRON_NS: &CStr = SCT_OLD_NS;

/// Handle an out of memory condition
#[doc(alias = "xmlSchematronPErrMemory")]
unsafe fn xml_schematron_perr_memory(
    ctxt: XmlSchematronParserCtxtPtr,
    extra: &str,
    node: *mut XmlNode,
) {
    if !ctxt.is_null() {
        (*ctxt).nberrors += 1;
    }
    __xml_simple_oom_error(XmlErrorDomain::XmlFromSchemasp, node, Some(extra));
}

/// Create an XML Schematrons parse context for that file/resource expected
/// to contain an XML Schematrons file.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlSchematronNewParserCtxt")]
pub unsafe fn xml_schematron_new_parser_ctxt(url: *const c_char) -> XmlSchematronParserCtxtPtr {
    if url.is_null() {
        return null_mut();
    }

    let ret: XmlSchematronParserCtxtPtr =
        xml_malloc(size_of::<XmlSchematronParserCtxt>()) as XmlSchematronParserCtxtPtr;
    if ret.is_null() {
        xml_schematron_perr_memory(null_mut(), "allocating schema parser context", null_mut());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchematronParserCtxt>());
    (*ret).typ = XML_STRON_CTXT_PARSER;
    (*ret).dict = xml_dict_create();
    (*ret).url = xml_dict_lookup((*ret).dict, url as _, -1);
    (*ret).includes = null_mut();
    (*ret).xctxt = xml_xpath_new_context(None);
    if (*ret).xctxt.is_null() {
        xml_schematron_perr_memory(
            null_mut(),
            "allocating schema parser XPath context",
            null_mut(),
        );
        xml_schematron_free_parser_ctxt(ret);
        return null_mut();
    }
    (*(*ret).xctxt).flags = XML_XPATH_CHECKNS as i32;
    ret
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
    if buffer.is_null() || size <= 0 {
        return null_mut();
    }

    let ret: XmlSchematronParserCtxtPtr =
        xml_malloc(size_of::<XmlSchematronParserCtxt>()) as XmlSchematronParserCtxtPtr;
    if ret.is_null() {
        xml_schematron_perr_memory(null_mut(), "allocating schema parser context", null_mut());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchematronParserCtxt>());
    (*ret).buffer = buffer;
    (*ret).size = size;
    (*ret).dict = xml_dict_create();
    (*ret).xctxt = xml_xpath_new_context(None);
    if (*ret).xctxt.is_null() {
        xml_schematron_perr_memory(
            null_mut(),
            "allocating schema parser XPath context",
            null_mut(),
        );
        xml_schematron_free_parser_ctxt(ret);
        return null_mut();
    }
    ret
}

/// Create an XML Schematrons parse context for that document.
/// NB. The document may be modified during the parsing process.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlSchematronNewDocParserCtxt")]
pub unsafe fn xml_schematron_new_doc_parser_ctxt(doc: XmlDocPtr) -> XmlSchematronParserCtxtPtr {
    // if doc.is_null() {
    //     return null_mut();
    // }

    let ret: XmlSchematronParserCtxtPtr =
        xml_malloc(size_of::<XmlSchematronParserCtxt>()) as XmlSchematronParserCtxtPtr;
    if ret.is_null() {
        xml_schematron_perr_memory(null_mut(), "allocating schema parser context", null_mut());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchematronParserCtxt>());
    (*ret).doc = Some(doc);
    (*ret).dict = xml_dict_create();
    // The application has responsibility for the document
    (*ret).preserve = 1;
    (*ret).xctxt = xml_xpath_new_context(Some(doc));
    if (*ret).xctxt.is_null() {
        xml_schematron_perr_memory(
            null_mut(),
            "allocating schema parser XPath context",
            null_mut(),
        );
        xml_schematron_free_parser_ctxt(ret);
        return null_mut();
    }

    ret
}

/// Free the resources associated to the schema parser context
#[doc(alias = "xmlSchematronFreeParserCtxt")]
pub unsafe fn xml_schematron_free_parser_ctxt(ctxt: XmlSchematronParserCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    if let Some(doc) = (*ctxt).doc.filter(|_| (*ctxt).preserve == 0) {
        xml_free_doc(doc);
    }
    if !(*ctxt).xctxt.is_null() {
        xml_xpath_free_context((*ctxt).xctxt);
    }
    (*ctxt).namespaces = None;
    xml_dict_free((*ctxt).dict);
    xml_free(ctxt as _);
}

macro_rules! IS_SCHEMATRON {
    ($node:expr, $elem:expr) => {
        !$node.is_null()
            && ((*$node).element_type() == XmlElementType::XmlElementNode)
            && xml_str_equal((*$node).name, $elem)
            && (*$node).ns.map_or(false, |ns| {
                xml_str_equal(ns.href, XML_SCHEMATRON_NS.as_ptr() as _)
                    || xml_str_equal(ns.href, XML_OLD_SCHEMATRON_NS.as_ptr() as _)
            })
    };
}

macro_rules! NEXT_SCHEMATRON {
    ($node:expr) => {
        while !$node.is_null() {
            if ((*$node).element_type() == XmlElementType::XmlElementNode)
                && (*$node).ns.map_or(false, |ns| {
                    xml_str_equal(ns.href, XML_SCHEMATRON_NS.as_ptr() as _)
                        || xml_str_equal(ns.href, XML_OLD_SCHEMATRON_NS.as_ptr() as _)
                })
            {
                break;
            }
            $node = (*$node).next.map_or(null_mut(), |n| n.as_ptr());
        }
    };
}

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
            $node as _,
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

/// Allocate a new Schematron structure.
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlSchematronNewSchematron")]
unsafe fn xml_schematron_new_schematron(ctxt: XmlSchematronParserCtxtPtr) -> XmlSchematronPtr {
    let ret: XmlSchematronPtr = xml_malloc(size_of::<XmlSchematron>()) as XmlSchematronPtr;
    if ret.is_null() {
        xml_schematron_perr_memory(ctxt, "allocating schema", null_mut());
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlSchematron::default());
    (*ret).dict = (*ctxt).dict;
    xml_dict_reference((*ret).dict);

    ret
}

/// Add a namespace definition in the context
#[doc(alias = "xmlSchematronAddNamespace")]
unsafe fn xml_schematron_add_namespace(
    ctxt: XmlSchematronParserCtxtPtr,
    prefix: *const XmlChar,
    ns: *const XmlChar,
) {
    let namespaces = (*ctxt).namespaces.get_or_insert_with(Vec::new);
    namespaces.push((
        xml_dict_lookup((*ctxt).dict, ns, -1),
        xml_dict_lookup((*ctxt).dict, prefix, -1),
    ));
}

/// Add a pattern to a schematron
///
/// Returns the new pointer or NULL in case of error
#[doc(alias = "xmlSchematronAddPattern")]
unsafe fn xml_schematron_add_pattern(
    ctxt: XmlSchematronParserCtxtPtr,
    schema: XmlSchematronPtr,
    node: *mut XmlNode,
    name: *mut XmlChar,
) -> XmlSchematronPatternPtr {
    if ctxt.is_null() || schema.is_null() || node.is_null() || name.is_null() {
        return null_mut();
    }

    let ret: XmlSchematronPatternPtr =
        xml_malloc(size_of::<XmlSchematronPattern>()) as XmlSchematronPatternPtr;
    if ret.is_null() {
        xml_schematron_perr_memory(ctxt, "allocating schema pattern", node);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchematronPattern>());
    (*ret).name = name;
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

/// Add a rule to a schematron
///
/// Returns the new pointer or NULL in case of error
#[doc(alias = "xmlSchematronAddRule")]
unsafe fn xml_schematron_add_rule(
    ctxt: XmlSchematronParserCtxtPtr,
    schema: XmlSchematronPtr,
    pat: XmlSchematronPatternPtr,
    node: *mut XmlNode,
    context: *mut XmlChar,
    report: *mut XmlChar,
) -> XmlSchematronRulePtr {
    if ctxt.is_null() || schema.is_null() || node.is_null() || context.is_null() {
        return null_mut();
    }

    // Try first to compile the pattern
    let pattern: XmlPatternPtr = xml_patterncompile(
        context,
        XmlPatternFlags::XmlPatternXpath as i32,
        (*ctxt).namespaces.clone(),
    );
    if pattern.is_null() {
        let context = CStr::from_ptr(context as *const i8).to_string_lossy();
        xml_schematron_perr!(
            ctxt,
            node,
            XmlParserErrors::XmlSchemapNoroot,
            "Failed to compile context expression {}",
            context
        );
    }

    let ret: XmlSchematronRulePtr =
        xml_malloc(size_of::<XmlSchematronRule>()) as XmlSchematronRulePtr;
    if ret.is_null() {
        xml_schematron_perr_memory(ctxt, "allocating schema rule", node);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchematronRule>());
    (*ret).node = node;
    (*ret).context = context;
    (*ret).pattern = pattern;
    (*ret).report = report;
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

/// Format the message content of the assert or report test
#[doc(alias = "xmlSchematronParseTestReportMsg")]
unsafe fn xml_schematron_parse_test_report_msg(
    ctxt: XmlSchematronParserCtxtPtr,
    con: *mut XmlNode,
) {
    let mut comp: XmlXPathCompExprPtr;

    let mut child = (*con).children().map_or(null_mut(), |c| c.as_ptr());
    while !child.is_null() {
        if ((*child).element_type() == XmlElementType::XmlTextNode)
            || ((*child).element_type() == XmlElementType::XmlCDATASectionNode)
        // Do Nothing
        {
        } else if IS_SCHEMATRON!(child, c"name".as_ptr() as _) {
            // Do Nothing
        } else if IS_SCHEMATRON!(child, c"value-of".as_ptr() as _) {
            if let Some(select) = (*child).get_no_ns_prop("select") {
                let cselect = CString::new(select.as_str()).unwrap();
                // try first to compile the test expression
                comp = xml_xpath_ctxt_compile((*ctxt).xctxt, cselect.as_ptr() as *const u8);
                if comp.is_null() {
                    xml_schematron_perr!(
                        ctxt,
                        child,
                        XmlParserErrors::XmlSchemavAttrInvalid,
                        "Failed to compile select expression {}",
                        select
                    );
                } else {
                    xml_schematron_perr!(
                        ctxt,
                        child,
                        XmlParserErrors::XmlSchemavAttrInvalid,
                        "value-of has no select attribute"
                    );
                }
                xml_xpath_free_comp_expr(comp);
            }
        }
        child = (*child).next.map_or(null_mut(), |n| n.as_ptr());
        continue;
    }
}

/// Add a test to a schematron
///
/// Returns the new pointer or NULL in case of error
#[doc(alias = "xmlSchematronAddTest")]
unsafe fn xml_schematron_add_test(
    ctxt: XmlSchematronParserCtxtPtr,
    typ: XmlSchematronTestType,
    rule: XmlSchematronRulePtr,
    node: *mut XmlNode,
    test: *mut XmlChar,
    report: *mut XmlChar,
) -> XmlSchematronTestPtr {
    if ctxt.is_null() || rule.is_null() || node.is_null() || test.is_null() {
        return null_mut();
    }

    // try first to compile the test expression
    let comp: XmlXPathCompExprPtr = xml_xpath_ctxt_compile((*ctxt).xctxt, test);
    if comp.is_null() {
        let test = CStr::from_ptr(test as *const i8).to_string_lossy();
        xml_schematron_perr!(
            ctxt,
            node,
            XmlParserErrors::XmlSchemapNoroot,
            "Failed to compile test expression {}",
            test
        );
        return null_mut();
    }

    let ret: XmlSchematronTestPtr =
        xml_malloc(size_of::<XmlSchematronTest>()) as XmlSchematronTestPtr;
    if ret.is_null() {
        xml_schematron_perr_memory(ctxt, "allocating schema test", node);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchematronTest>());
    (*ret).typ = typ;
    (*ret).node = node;
    (*ret).test = test;
    (*ret).comp = comp;
    (*ret).report = report;
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

/// Parse a rule element
#[doc(alias = "xmlSchematronParseRule")]
unsafe fn xml_schematron_parse_rule(
    ctxt: XmlSchematronParserCtxtPtr,
    pattern: XmlSchematronPatternPtr,
    rule: *mut XmlNode,
) {
    let mut nb_checks: i32 = 0;
    let mut report: *mut XmlChar;
    let ruleptr: XmlSchematronRulePtr;
    let mut testptr: XmlSchematronTestPtr;

    if ctxt.is_null() || rule.is_null() {
        return;
    }

    match (*rule).get_no_ns_prop("context") {
        Some(context) if context.is_empty() => {
            xml_schematron_perr!(
                ctxt,
                rule,
                XmlParserErrors::XmlSchemapNoroot,
                "rule has an empty context attribute"
            );
            return;
        }
        Some(context) => {
            let context = CString::new(context).unwrap();
            let context = xml_strdup(context.as_ptr() as *const u8);
            ruleptr =
                xml_schematron_add_rule(ctxt, (*ctxt).schema, pattern, rule, context, null_mut());
            if ruleptr.is_null() {
                xml_free(context as _);
                return;
            }
        }
        None => {
            xml_schematron_perr!(
                ctxt,
                rule,
                XmlParserErrors::XmlSchemapNoroot,
                "rule has no context attribute"
            );
            return;
        }
    }

    let mut cur = (*rule).children().map_or(null_mut(), |c| c.as_ptr());
    NEXT_SCHEMATRON!(cur);
    while !cur.is_null() {
        if IS_SCHEMATRON!(cur, c"let".as_ptr() as _) {
            let name = match (*cur).get_no_ns_prop("name") {
                Some(name) if name.is_empty() => {
                    xml_schematron_perr!(
                        ctxt,
                        cur,
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
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        "let has no name attribute"
                    );
                    return;
                }
            };
            let value = match (*cur).get_no_ns_prop("value") {
                Some(value) if value.is_empty() => {
                    xml_schematron_perr!(
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        "let has an empty value attribute"
                    );
                    return;
                }
                Some(value) => CString::new(value).unwrap(),
                None => {
                    xml_schematron_perr!(
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        "let has no value attribute"
                    );
                    return;
                }
            };

            let var_comp: XmlXPathCompExprPtr =
                xml_xpath_ctxt_compile((*ctxt).xctxt, value.as_ptr() as *const u8);
            if var_comp.is_null() {
                xml_schematron_perr!(
                    ctxt,
                    cur,
                    XmlParserErrors::XmlSchemapNoroot,
                    "Failed to compile let expression {}",
                    value.to_string_lossy().into_owned()
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
        } else if IS_SCHEMATRON!(cur, c"assert".as_ptr() as _) {
            nb_checks += 1;
            match (*cur).get_no_ns_prop("test") {
                Some(test) if test.is_empty() => {
                    xml_schematron_perr!(
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        "assert has an empty test attribute"
                    );
                }
                Some(test) => {
                    xml_schematron_parse_test_report_msg(ctxt, cur);
                    let tmp = (*cur).get_content().map(|c| CString::new(c).unwrap());
                    report = tmp.map_or(null_mut(), |c| xml_strdup(c.as_ptr() as *const u8));

                    let test = CString::new(test).unwrap();
                    let test = xml_strdup(test.as_ptr() as *const u8);
                    testptr = xml_schematron_add_test(
                        ctxt,
                        XmlSchematronTestType::XmlSchematronAssert,
                        ruleptr,
                        cur,
                        test,
                        report,
                    );
                    if testptr.is_null() {
                        xml_free(test as _);
                    }
                }
                None => {
                    xml_schematron_perr!(
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        "assert has no test attribute"
                    );
                }
            }
        } else if IS_SCHEMATRON!(cur, c"report".as_ptr() as _) {
            nb_checks += 1;
            match (*cur).get_no_ns_prop("test") {
                Some(test) if test.is_empty() => {
                    xml_schematron_perr!(
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        "assert has an empty test attribute"
                    );
                }
                Some(test) => {
                    xml_schematron_parse_test_report_msg(ctxt, cur);
                    let tmp = (*cur).get_content().map(|c| CString::new(c).unwrap());
                    report = tmp.map_or(null_mut(), |c| xml_strdup(c.as_ptr() as *const u8));

                    let test = CString::new(test).unwrap();
                    let test = xml_strdup(test.as_ptr() as *const u8);
                    testptr = xml_schematron_add_test(
                        ctxt,
                        XmlSchematronTestType::XmlSchematronReport,
                        ruleptr,
                        cur,
                        test,
                        report,
                    );
                    if testptr.is_null() {
                        xml_free(test as _);
                    }
                }
                None => {
                    xml_schematron_perr!(
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        "assert has no test attribute"
                    );
                }
            }
        } else {
            xml_schematron_perr!(
                ctxt,
                cur,
                XmlParserErrors::XmlSchemapNoroot,
                "Expecting an assert or a report element instead of {}",
                (*cur).name().unwrap()
            );
        }
        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
        NEXT_SCHEMATRON!(cur);
    }
    if nb_checks == 0 {
        xml_schematron_perr!(
            ctxt,
            rule,
            XmlParserErrors::XmlSchemapNoroot,
            "rule has no assert nor report element"
        );
    }
}

/// Parse a pattern element
#[doc(alias = "xmlSchematronParsePattern")]
unsafe fn xml_schematron_parse_pattern(ctxt: XmlSchematronParserCtxtPtr, pat: *mut XmlNode) {
    let mut nb_rules: i32 = 0;

    if ctxt.is_null() || pat.is_null() {
        return;
    }

    let id = (*pat)
        .get_no_ns_prop("id")
        .or_else(|| (*pat).get_no_ns_prop("name"))
        .map(|id| CString::new(id).unwrap());
    let id = id
        .as_ref()
        .map_or(null_mut(), |id| xml_strdup(id.as_ptr() as *const u8));
    let pattern: XmlSchematronPatternPtr =
        xml_schematron_add_pattern(ctxt, (*ctxt).schema, pat, id);
    if pattern.is_null() {
        if !id.is_null() {
            xml_free(id as _);
        }
        return;
    }
    let mut cur = (*pat).children().map_or(null_mut(), |c| c.as_ptr());
    NEXT_SCHEMATRON!(cur);
    while !cur.is_null() {
        if IS_SCHEMATRON!(cur, c"rule".as_ptr() as _) {
            xml_schematron_parse_rule(ctxt, pattern, cur);
            nb_rules += 1;
        } else {
            xml_schematron_perr!(
                ctxt,
                cur,
                XmlParserErrors::XmlSchemapNoroot,
                "Expecting a rule element instead of {}",
                (*cur).name().unwrap()
            );
        }
        cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
        NEXT_SCHEMATRON!(cur);
    }
    if nb_rules == 0 {
        xml_schematron_perr!(
            ctxt,
            pat,
            XmlParserErrors::XmlSchemapNoroot,
            "Pattern has no rule element"
        );
    }
}

/// Parse a schema definition resource and build an internal
/// XML Schema structure which can be used to validate instances.
///
/// Returns the internal XML Schematron structure built from the resource or NULL in case of error
#[doc(alias = "xmlSchematronParse")]
pub unsafe fn xml_schematron_parse(ctxt: XmlSchematronParserCtxtPtr) -> XmlSchematronPtr {
    let mut ret: XmlSchematronPtr = null_mut();
    let mut cur: *mut XmlNode;
    let mut preserve: i32 = 0;

    if ctxt.is_null() {
        return null_mut();
    }

    (*ctxt).nberrors = 0;

    // First step is to parse the input document into an DOM/Infoset
    let doc = if !(*ctxt).url.is_null() {
        let url = CStr::from_ptr((*ctxt).url as *const i8).to_string_lossy();
        let Some(doc) = xml_read_file(&url, None, SCHEMATRON_PARSE_OPTIONS as i32) else {
            xml_schematron_perr!(
                ctxt,
                null_mut(),
                XmlParserErrors::XmlSchemapFailedLoad,
                "xmlSchematronParse: could not load '{}'.\n",
                url
            );
            return null_mut();
        };
        (*ctxt).preserve = 0;
        doc
    } else if !(*ctxt).buffer.is_null() {
        let mem = from_raw_parts((*ctxt).buffer as *const u8, (*ctxt).size as usize).to_vec();
        let Some(mut doc) = xml_read_memory(mem, None, None, SCHEMATRON_PARSE_OPTIONS as i32)
        else {
            xml_schematron_perr!(
                ctxt,
                null_mut(),
                XmlParserErrors::XmlSchemapFailedParse,
                "xmlSchematronParse: could not parse.\n"
            );
            return null_mut();
        };
        doc.url = Some("in_memory_buffer".to_owned());
        (*ctxt).url = xml_dict_lookup((*ctxt).dict, c"in_memory_buffer".as_ptr() as _, -1);
        (*ctxt).preserve = 0;
        doc
    } else if let Some(doc) = (*ctxt).doc {
        preserve = 1;
        (*ctxt).preserve = 1;
        doc
    } else {
        xml_schematron_perr!(
            ctxt,
            null_mut(),
            XmlParserErrors::XmlSchemapNothingToParse,
            "xmlSchematronParse: could not parse.\n"
        );
        return null_mut();
    };

    // Then extract the root and Schematron parse it
    let root: *mut XmlNode = doc.get_root_element();
    if root.is_null() {
        xml_schematron_perr!(
            ctxt,
            doc.as_ptr() as *mut XmlNode,
            XmlParserErrors::XmlSchemapNoroot,
            "The schema has no document element.\n"
        );
        if preserve == 0 {
            xml_free_doc(doc);
        }
        return null_mut();
    }

    if !IS_SCHEMATRON!(root, c"schema".as_ptr() as _) {
        let url = CStr::from_ptr((*ctxt).url as *const i8).to_string_lossy();
        xml_schematron_perr!(
            ctxt,
            root,
            XmlParserErrors::XmlSchemapNoroot,
            "The XML document '{}' is not a XML schematron document",
            url
        );
        // goto exit;
    } else {
        ret = xml_schematron_new_schematron(ctxt);
        if ret.is_null() {
            // goto exit;
        } else {
            (*ctxt).schema = ret;

            // scan the schema elements
            cur = (*root).children().map_or(null_mut(), |c| c.as_ptr());
            NEXT_SCHEMATRON!(cur);
            if IS_SCHEMATRON!(cur, c"title".as_ptr() as _) {
                if let Some(title) = (*cur).get_content() {
                    let title = CString::new(title).unwrap();
                    (*ret).title = xml_dict_lookup((*ret).dict, title.as_ptr() as *const u8, -1);
                }
                cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
                NEXT_SCHEMATRON!(cur);
            }
            while IS_SCHEMATRON!(cur, c"ns".as_ptr() as _) {
                let prefix = (*cur).get_no_ns_prop("prefix");
                let uri = (*cur).get_no_ns_prop("uri");
                if uri.as_deref().map_or(true, |uri| uri.is_empty()) {
                    xml_schematron_perr!(
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        "ns element has no uri"
                    );
                }
                if prefix.as_deref().map_or(true, |pre| pre.is_empty()) {
                    xml_schematron_perr!(
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        "ns element has no prefix"
                    );
                }
                if let (Some(prefix), Some(uri)) = (prefix, uri) {
                    let prefix = CString::new(prefix).unwrap();
                    let uri = CString::new(uri).unwrap();
                    xml_xpath_register_ns(
                        (*ctxt).xctxt,
                        prefix.as_ptr() as *const u8,
                        uri.as_ptr() as *const u8,
                    );
                    xml_schematron_add_namespace(
                        ctxt,
                        prefix.as_ptr() as *const u8,
                        uri.as_ptr() as *const u8,
                    );
                    (*ret).nb_ns += 1;
                }
                cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
                NEXT_SCHEMATRON!(cur);
            }
            while !cur.is_null() {
                if IS_SCHEMATRON!(cur, c"pattern".as_ptr() as _) {
                    xml_schematron_parse_pattern(ctxt, cur);
                    (*ret).nb_pattern += 1;
                } else {
                    xml_schematron_perr!(
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        "Expecting a pattern element instead of {}",
                        (*cur).name().unwrap()
                    );
                }
                cur = (*cur).next.map_or(null_mut(), |n| n.as_ptr());
                NEXT_SCHEMATRON!(cur);
            }
            if (*ret).nb_pattern == 0 {
                let url = CStr::from_ptr((*ctxt).url as *const i8).to_string_lossy();
                xml_schematron_perr!(
                    ctxt,
                    root,
                    XmlParserErrors::XmlSchemapNoroot,
                    "The schematron document '{}' has no pattern",
                    url
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
        if (*ctxt).nberrors != 0 {
            xml_schematron_free(ret);
            ret = null_mut();
        } else {
            (*ret).namespaces = (*ctxt).namespaces.take();
        }
    }
    ret
}

/// Free a list of tests.
#[doc(alias = "xmlSchematronFreeTests")]
unsafe fn xml_schematron_free_tests(mut tests: XmlSchematronTestPtr) {
    let mut next: XmlSchematronTestPtr;

    while !tests.is_null() {
        next = (*tests).next;
        if !(*tests).test.is_null() {
            xml_free((*tests).test as _);
        }
        if !(*tests).comp.is_null() {
            xml_xpath_free_comp_expr((*tests).comp);
        }
        if !(*tests).report.is_null() {
            xml_free((*tests).report as _);
        }
        xml_free(tests as _);
        tests = next;
    }
}

/// Free a list of let variables.
#[doc(alias = "xmlSchematronFreeLets")]
unsafe fn xml_schematron_free_lets(mut lets: XmlSchematronLetPtr) {
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

/// Free a list of rules.
#[doc(alias = "xmlSchematronFreeRules")]
unsafe fn xml_schematron_free_rules(mut rules: XmlSchematronRulePtr) {
    let mut next: XmlSchematronRulePtr;

    while !rules.is_null() {
        next = (*rules).next;
        if !(*rules).tests.is_null() {
            xml_schematron_free_tests((*rules).tests);
        }
        if !(*rules).context.is_null() {
            xml_free((*rules).context as _);
        }
        if !(*rules).pattern.is_null() {
            xml_free_pattern((*rules).pattern);
        }
        if !(*rules).report.is_null() {
            xml_free((*rules).report as _);
        }
        if !(*rules).lets.is_null() {
            xml_schematron_free_lets((*rules).lets);
        }
        xml_free(rules as _);
        rules = next;
    }
}

/// Free a list of patterns.
#[doc(alias = "xmlSchematronFreePatterns")]
unsafe fn xml_schematron_free_patterns(mut patterns: XmlSchematronPatternPtr) {
    let mut next: XmlSchematronPatternPtr;

    while !patterns.is_null() {
        next = (*patterns).next;
        if !(*patterns).name.is_null() {
            xml_free((*patterns).name as _);
        }
        xml_free(patterns as _);
        patterns = next;
    }
}

/// Deallocate a Schematron structure.
#[doc(alias = "xmlSchematronFree")]
pub unsafe fn xml_schematron_free(schema: XmlSchematronPtr) {
    if schema.is_null() {
        return;
    }

    if let Some(doc) = (*schema).doc.filter(|_| (*schema).preserve == 0) {
        xml_free_doc(doc);
    }

    (*schema).namespaces = None;

    xml_schematron_free_rules((*schema).rules);
    xml_schematron_free_patterns((*schema).patterns);
    xml_dict_free((*schema).dict);
    xml_free(schema as _);
}

/// Set the structured error callback
#[doc(alias = "xmlSchematronSetValidStructuredErrors")]
pub unsafe fn xml_schematron_set_valid_structured_errors(
    ctxt: XmlSchematronValidCtxtPtr,
    serror: Option<StructuredError>,
    ctx: Option<GenericErrorContext>,
) {
    if ctxt.is_null() {
        return;
    }
    (*ctxt).serror = serror;
    (*ctxt).error = None;
    (*ctxt).warning = None;
    (*ctxt).user_data = ctx;
}

/// Handle an out of memory condition
#[doc(alias = "xmlSchematronVTypeErrMemory")]
unsafe fn xml_schematron_verr_memory(
    ctxt: XmlSchematronValidCtxtPtr,
    extra: &str,
    node: *mut XmlNode,
) {
    if !ctxt.is_null() {
        (*ctxt).nberrors += 1;
        (*ctxt).err = XmlParserErrors::XmlSchemavInternal as i32;
    }
    __xml_simple_oom_error(XmlErrorDomain::XmlFromSchemasv, node, Some(extra));
}

/// Create an XML Schematrons validation context based on the given schema.
///
/// Returns the validation context or NULL in case of error
#[doc(alias = "xmlSchematronNewValidCtxt")]
pub unsafe fn xml_schematron_new_valid_ctxt(
    schema: XmlSchematronPtr,
    options: i32,
) -> XmlSchematronValidCtxtPtr {
    let ret: XmlSchematronValidCtxtPtr =
        xml_malloc(size_of::<XmlSchematronValidCtxt>()) as XmlSchematronValidCtxtPtr;
    if ret.is_null() {
        xml_schematron_verr_memory(null_mut(), "allocating validation context", null_mut());
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlSchematronValidCtxt::default());
    (*ret).typ = XML_STRON_CTXT_VALIDATOR;
    (*ret).schema = schema;
    (*ret).xctxt = xml_xpath_new_context(None);
    (*ret).flags = options;
    if (*ret).xctxt.is_null() {
        xml_schematron_perr_memory(
            null_mut(),
            "allocating schema parser XPath context",
            null_mut(),
        );
        xml_schematron_free_valid_ctxt(ret);
        return null_mut();
    }
    if let Some(namespaces) = (*schema).namespaces.as_deref() {
        for &(href, pref) in namespaces {
            if href.is_null() || pref.is_null() {
                break;
            }
            xml_xpath_register_ns((*ret).xctxt, pref, href);
        }
    }
    ret
}

/// Free the resources associated to the schema validation context
#[doc(alias = "xmlSchematronFreeValidCtxt")]
pub unsafe fn xml_schematron_free_valid_ctxt(ctxt: XmlSchematronValidCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    if !(*ctxt).xctxt.is_null() {
        xml_xpath_free_context((*ctxt).xctxt);
    }
    if !(*ctxt).dict.is_null() {
        xml_dict_free((*ctxt).dict);
    }
    xml_free(ctxt as _);
}

/// Registers a list of let variables to the current context of @cur
///
/// Returns -1 in case of errors, otherwise 0
#[doc(alias = "xmlSchematronRegisterVariables")]
unsafe fn xml_schematron_register_variables(
    ctxt: XmlXPathContextPtr,
    mut letr: XmlSchematronLetPtr,
    instance: XmlDocPtr,
    cur: *mut XmlNode,
) -> i32 {
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

unsafe fn xml_schematron_get_node(
    ctxt: XmlSchematronValidCtxtPtr,
    cur: *mut XmlNode,
    xpath: *const XmlChar,
) -> *mut XmlNode {
    let mut node: *mut XmlNode = null_mut();

    if ctxt.is_null() || cur.is_null() || xpath.is_null() {
        return null_mut();
    }

    (*(*ctxt).xctxt).doc = XmlDocPtr::from_raw((*cur).doc).unwrap();
    (*(*ctxt).xctxt).node = cur;
    let ret: XmlXPathObjectPtr = xml_xpath_eval(xpath, (*ctxt).xctxt);
    if ret.is_null() {
        return null_mut();
    }

    if (*ret).typ == XmlXPathObjectType::XPathNodeset {
        if let Some(nodeset) = (*ret).nodesetval.as_deref() {
            if !nodeset.node_tab.is_empty() {
                node = nodeset.node_tab[0];
            }
        }
    }

    xml_xpath_free_object(ret);
    node
}

/// Build the string being reported to the user.
///
/// Returns a report string or NULL in case of error. The string needs to be deallocated by the caller
#[doc(alias = "xmlSchematronFormatReport")]
unsafe fn xml_schematron_format_report(
    ctxt: XmlSchematronValidCtxtPtr,
    test: *mut XmlNode,
    cur: *mut XmlNode,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar = null_mut();
    let mut node: *mut XmlNode;
    let mut comp: XmlXPathCompExprPtr;

    if test.is_null() || cur.is_null() {
        return ret;
    }

    let mut child = (*test).children().map_or(null_mut(), |c| c.as_ptr());
    while !child.is_null() {
        if ((*child).element_type() == XmlElementType::XmlTextNode)
            || ((*child).element_type() == XmlElementType::XmlCDATASectionNode)
        {
            ret = xml_strcat(ret, (*child).content);
        } else if IS_SCHEMATRON!(child, c"name".as_ptr() as _) {
            let path = (*child).get_no_ns_prop("path");

            node = cur;
            if let Some(path) = path {
                let path = CString::new(path).unwrap();
                node = xml_schematron_get_node(ctxt, cur, path.as_ptr() as *const u8);
                if node.is_null() {
                    node = cur;
                }
            }

            if let Some(prefix) = (*node).ns.map(|ns| ns.prefix).filter(|p| !p.is_null()) {
                ret = xml_strcat(ret, prefix);
                ret = xml_strcat(ret, c":".as_ptr() as _);
                ret = xml_strcat(ret, (*node).name);
            } else {
                ret = xml_strcat(ret, (*node).name);
            }
        } else if IS_SCHEMATRON!(child, c"value-of".as_ptr() as _) {
            let select = (*child)
                .get_no_ns_prop("select")
                .map(|s| CString::new(s).unwrap());
            let select = select
                .as_ref()
                .map_or(null_mut(), |s| xml_strdup(s.as_ptr() as *const u8));
            comp = xml_xpath_ctxt_compile((*ctxt).xctxt, select);
            let eval: XmlXPathObjectPtr = xml_xpath_compiled_eval(comp, (*ctxt).xctxt);

            match (*eval).typ {
                XmlXPathObjectType::XPathNodeset => {
                    let spacer: *mut XmlChar = c" ".as_ptr() as _;

                    if let Some(nodeset) = (*eval).nodesetval.as_deref() {
                        for (indx, &node) in nodeset.node_tab.iter().enumerate() {
                            if indx > 0 {
                                ret = xml_strcat(ret, spacer);
                            }
                            ret = xml_strcat(ret, (*node).name);
                        }
                    } else {
                        generic_error!("Empty node set\n");
                    }
                }
                XmlXPathObjectType::XPathBoolean => {
                    let str: *const c_char = if (*eval).boolval {
                        c"True".as_ptr()
                    } else {
                        c"False".as_ptr()
                    };
                    ret = xml_strcat(ret, str as _);
                }
                XmlXPathObjectType::XPathNumber => {
                    let size: i32 = snprintf(null_mut(), 0, c"%0g".as_ptr() as _, (*eval).floatval);
                    let buf: *mut XmlChar = malloc(size as usize) as _;
                    sprintf(buf as _, c"%0g".as_ptr() as _, (*eval).floatval);
                    ret = xml_strcat(ret, buf);
                    xml_free(buf as _);
                }
                XmlXPathObjectType::XPathString => {
                    let strval = (*eval)
                        .stringval
                        .as_deref()
                        .map(|s| CString::new(s).unwrap());
                    ret = xml_strcat(
                        ret,
                        strval
                            .as_deref()
                            .map_or(null_mut(), |s| s.as_ptr() as *const u8),
                    );
                }
                _ => {
                    generic_error!("Unsupported XPATH Type: {:?}\n", (*eval).typ);
                }
            }
            xml_xpath_free_object(eval);
            xml_xpath_free_comp_expr(comp);
            xml_free(select as _);
        } else {
            child = (*child).next.map_or(null_mut(), |n| n.as_ptr());
            continue;
        }

        // remove superfluous \n
        if !ret.is_null() {
            let mut len: i32 = xml_strlen(ret) as _;
            let mut c: XmlChar;

            if len > 0 {
                c = *ret.add(len as usize - 1);
                if c == b' ' || c == b'\n' || c == b'\r' || c == b'\t' {
                    while c == b' ' || c == b'\n' || c == b'\r' || c == b'\t' {
                        len -= 1;
                        if len == 0 {
                            break;
                        }
                        c = *ret.add(len as usize - 1);
                    }
                    *ret.add(len as usize) = b' ';
                    *ret.add(len as usize + 1) = 0;
                }
            }
        }

        child = (*child).next.map_or(null_mut(), |n| n.as_ptr());
    }
    ret
}

/// Output part of the report to whatever channel the user selected
#[doc(alias = "xmlSchematronReportOutput")]
unsafe fn xml_schematron_report_output(
    _ctxt: XmlSchematronValidCtxtPtr,
    _cur: *mut XmlNode,
    msg: *const c_char,
) {
    // TODO
    eprintln!("{}", CStr::from_ptr(msg).to_string_lossy());
}

/// Called from the validation engine when an assert or report test have been done.
#[doc(alias = "xmlSchematronReportSuccess")]
unsafe fn xml_schematron_report_success(
    ctxt: XmlSchematronValidCtxtPtr,
    test: XmlSchematronTestPtr,
    cur: *mut XmlNode,
    pattern: XmlSchematronPatternPtr,
    success: i32,
) {
    if ctxt.is_null() || cur.is_null() || test.is_null() {
        return;
    }
    // if quiet and not SVRL report only failures
    if (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutQuiet as i32 != 0
        && (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutXml as i32 == 0
        && (*test).typ == XmlSchematronTestType::XmlSchematronReport
    {
        return;
    }
    if (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutXml as i32 != 0 {
        // TODO
    } else {
        let mut report: *const XmlChar = null_mut();

        if ((*test).typ == XmlSchematronTestType::XmlSchematronReport && success == 0)
            || ((*test).typ == XmlSchematronTestType::XmlSchematronAssert && success != 0)
        {
            return;
        }
        let line: i64 = (*cur).get_line_no();
        let path = (*cur).get_node_path().expect("Internal Error");
        if !(*test).node.is_null() {
            report = xml_schematron_format_report(ctxt, (*test).node, cur);
        }
        if report.is_null() {
            if (*test).typ == XmlSchematronTestType::XmlSchematronAssert {
                report = xml_strdup(c"node failed assert".as_ptr() as _);
            } else {
                report = xml_strdup(c"node failed report".as_ptr() as _);
            }
        }
        let msg = format!(
            "{} line {}: {}\n",
            path,
            line,
            CStr::from_ptr(report as *const i8).to_string_lossy()
        );

        if (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutError as i32 != 0 {
            let mut schannel: Option<StructuredError> = None;
            let mut channel: Option<GenericError> = None;
            let mut data = None;

            if !ctxt.is_null() {
                if (*ctxt).serror.is_some() {
                    schannel = (*ctxt).serror;
                } else {
                    channel = (*ctxt).error;
                }
                data = (*ctxt).user_data.clone();
            }

            __xml_raise_error!(
                schannel,
                channel,
                data,
                null_mut(),
                cur as _,
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
                    Some(
                        CStr::from_ptr((*pattern).name as _)
                            .to_string_lossy()
                            .into_owned()
                            .into(),
                    )
                },
                Some(path.to_owned().into()),
                (!report.is_null()).then(|| CStr::from_ptr(report as *const i8)
                    .to_string_lossy()
                    .into_owned()
                    .into()),
                0,
                0,
                msg.as_str(),
            );
        } else {
            let msg = CString::new(msg).unwrap();
            xml_schematron_report_output(ctxt, cur, msg.as_ptr());
        }

        xml_free(report as _);
    }
}

/// Validate a rule against a tree instance at a given position
///
/// Returns 1 in case of success, 0 if error and -1 in case of internal error
#[doc(alias = "xmlSchematronRunTest")]
unsafe fn xml_schematron_run_test(
    ctxt: XmlSchematronValidCtxtPtr,
    test: XmlSchematronTestPtr,
    instance: XmlDocPtr,
    cur: *mut XmlNode,
    pattern: XmlSchematronPatternPtr,
) -> i32 {
    let mut failed: i32;

    failed = 0;
    (*(*ctxt).xctxt).doc = Some(instance);
    (*(*ctxt).xctxt).node = cur;
    let ret: XmlXPathObjectPtr = xml_xpath_compiled_eval((*test).comp, (*ctxt).xctxt);
    if ret.is_null() {
        failed = 1;
    } else {
        match (*ret).typ {
            XmlXPathObjectType::XPathXSLTTree | XmlXPathObjectType::XPathNodeset => {
                if (*ret).nodesetval.as_deref().map_or(true, |n| n.is_empty()) {
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
                if (*ret).stringval.as_deref().map_or(true, |s| s.is_empty()) {
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
        (*ctxt).nberrors += 1;
    }

    xml_schematron_report_success(ctxt, test, cur, pattern, (failed == 0) as i32);

    (failed == 0) as i32
}

/// Unregisters a list of let variables from the context
///
/// Returns -1 in case of errors, otherwise 0
#[doc(alias = "xmlSchematronUnregisterVariables")]
unsafe fn xml_schematron_unregister_variables(
    ctxt: XmlXPathContextPtr,
    mut letr: XmlSchematronLetPtr,
) -> i32 {
    while !letr.is_null() {
        if xml_xpath_register_variable_ns(ctxt, (*letr).name, null_mut(), null_mut()) != 0 {
            generic_error!("Unregistering a let variable failed\n");
            return -1;
        }
        letr = (*letr).next;
    }
    0
}

unsafe fn xml_schematron_next_node(mut cur: *mut XmlNode) -> *mut XmlNode {
    if let Some(children) = (*cur).children() {
        // Do not descend on entities declarations
        if children.element_type() != XmlElementType::XmlEntityDecl {
            cur = children.as_ptr();
            // Skip DTDs
            if (*cur).element_type() != XmlElementType::XmlDTDNode {
                return cur;
            }
        }
    }

    while let Some(next) = (*cur).next {
        cur = next.as_ptr();
        if (*cur).element_type() != XmlElementType::XmlEntityDecl
            && (*cur).element_type() != XmlElementType::XmlDTDNode
        {
            return cur;
        }
    }

    loop {
        cur = (*cur).parent().map_or(null_mut(), |p| p.as_ptr());
        if cur.is_null() {
            break;
        }
        if (*cur).element_type() == XmlElementType::XmlDocumentNode {
            return null_mut();
        }
        if let Some(next) = (*cur).next {
            cur = next.as_ptr();
            return cur;
        }

        if cur.is_null() {
            break;
        }
    }
    cur
}

/// Called from the validation engine when starting to check a pattern
#[doc(alias = "xmlSchematronReportPattern")]
unsafe fn xml_schematron_report_pattern(
    ctxt: XmlSchematronValidCtxtPtr,
    pattern: XmlSchematronPatternPtr,
) {
    if ctxt.is_null() || pattern.is_null() {
        return;
    }
    if (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutQuiet as i32 != 0
        || (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutError as i32 != 0
    {
        // Error gives pattern name as part of error
        return;
    }
    if (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutXml as i32 != 0 {
        // TODO
    } else {
        let mut msg: [c_char; 1000] = [0; 1000];

        if (*pattern).name.is_null() {
            return;
        }
        snprintf(
            msg.as_mut_ptr(),
            999,
            c"Pattern: %s\n".as_ptr() as _,
            (*pattern).name,
        );
        xml_schematron_report_output(ctxt, null_mut(), &msg[0]);
    }
}

/// Validate a tree instance against the schematron
///
/// Returns 0 in case of success, -1 in case of internal error and an error count otherwise.
#[doc(alias = "xmlSchematronValidateDoc")]
pub unsafe fn xml_schematron_validate_doc(
    ctxt: XmlSchematronValidCtxtPtr,
    instance: XmlDocPtr,
) -> i32 {
    let mut cur: *mut XmlNode;
    let mut pattern: XmlSchematronPatternPtr;
    let mut rule: XmlSchematronRulePtr;
    let mut test: XmlSchematronTestPtr;

    if ctxt.is_null() || (*ctxt).schema.is_null() || (*(*ctxt).schema).rules.is_null()
    // || instance.is_null()
    {
        return -1;
    }
    (*ctxt).nberrors = 0;
    let root: *mut XmlNode = (*instance).get_root_element();
    if root.is_null() {
        // TODO
        (*ctxt).nberrors += 1;
        return 1;
    }
    if (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutQuiet as i32 != 0
        || (*ctxt).flags == 0
    {
        // we are just trying to assert the validity of the document,
        // speed primes over the output, run in a single pass
        cur = root;
        while !cur.is_null() {
            rule = (*(*ctxt).schema).rules;
            while !rule.is_null() {
                if xml_pattern_match((*rule).pattern, cur) == 1 {
                    test = (*rule).tests;

                    if xml_schematron_register_variables((*ctxt).xctxt, (*rule).lets, instance, cur)
                        != 0
                    {
                        return -1;
                    }

                    while !test.is_null() {
                        xml_schematron_run_test(
                            ctxt,
                            test,
                            instance,
                            cur,
                            (*rule).pattern as XmlSchematronPatternPtr,
                        );
                        test = (*test).next;
                    }

                    if xml_schematron_unregister_variables((*ctxt).xctxt, (*rule).lets) != 0 {
                        return -1;
                    }
                }
                rule = (*rule).next;
            }

            cur = xml_schematron_next_node(cur);
        }
    } else {
        // Process all contexts one at a time
        pattern = (*(*ctxt).schema).patterns;

        while !pattern.is_null() {
            xml_schematron_report_pattern(ctxt, pattern);

            // TODO convert the pattern rule to a direct XPath and
            // compute directly instead of using the pattern matching
            // over the full document...
            // Check the exact semantic
            cur = root;
            while !cur.is_null() {
                rule = (*pattern).rules;
                while !rule.is_null() {
                    if xml_pattern_match((*rule).pattern, cur) == 1 {
                        test = (*rule).tests;
                        xml_schematron_register_variables(
                            (*ctxt).xctxt,
                            (*rule).lets,
                            instance,
                            cur,
                        );

                        while !test.is_null() {
                            xml_schematron_run_test(ctxt, test, instance, cur, pattern);
                            test = (*test).next;
                        }

                        xml_schematron_unregister_variables((*ctxt).xctxt, (*rule).lets);
                    }
                    rule = (*rule).patnext;
                }

                cur = xml_schematron_next_node(cur);
            }
            pattern = (*pattern).next;
        }
    }
    (*ctxt).nberrors
}

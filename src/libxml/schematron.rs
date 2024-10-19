//! Provide methods and data structures for XML Schematron.  
//! This module is based on `libxml/schematron.h`, `schematron.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, c_long, CStr},
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of, null, null_mut},
    sync::atomic::Ordering,
};

use libc::{malloc, memset, snprintf, sprintf, FILE};

use crate::{
    __xml_raise_error,
    buf::libxml_api::XmlBufPtr,
    error::XmlErrorDomain,
    generic_error,
    globals::{GenericError, GenericErrorContext, StructuredError},
    libxml::{tree::XmlElementType, xmlstring::xml_str_equal, xpath::xml_xpath_ctxt_compile},
    private::error::__xml_simple_error,
};

use super::{
    dict::{xml_dict_create, xml_dict_free, xml_dict_lookup, xml_dict_reference, XmlDictPtr},
    globals::{xml_free, xml_malloc, xml_realloc},
    parser::{xml_read_file, xml_read_memory, XmlParserOption},
    pattern::{
        xml_free_pattern, xml_pattern_match, xml_patterncompile, XmlPatternFlags, XmlPatternPtr,
    },
    tree::{
        xml_doc_get_root_element, xml_free_doc, xml_get_line_no, xml_get_no_ns_prop,
        xml_get_node_path, xml_node_get_content, XmlDocPtr, XmlNodePtr,
    },
    xml_io::{XmlOutputCloseCallback, XmlOutputWriteCallback},
    xmlerror::XmlParserErrors,
    xmlstring::{xml_strcat, xml_strdup, xml_strlen, XmlChar},
    xpath::{
        xml_xpath_compiled_eval, xml_xpath_eval, xml_xpath_free_comp_expr, xml_xpath_free_context,
        xml_xpath_free_object, xml_xpath_is_nan, xml_xpath_new_context, XmlXPathCompExprPtr,
        XmlXPathContextPtr, XmlXPathObjectPtr, XmlXPathObjectType, XML_XPATH_CHECKNS,
    },
    xpath_internals::{xml_xpath_register_ns, xml_xpath_register_variable_ns},
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

/**
 * xmlSchematronValidityErrorFunc:
 * @ctx: the validation context
 * @msg: the message
 * @...: extra arguments
 *
 * Signature of an error callback from a Schematron validation
 */
pub type XmlSchematronValidityErrorFunc =
    unsafe extern "C" fn(ctx: *mut c_void, msg: *const c_char);

/**
 * xmlSchematronValidityWarningFunc:
 * @ctx: the validation context
 * @msg: the message
 * @...: extra arguments
 *
 * Signature of a warning callback from a Schematron validation
 */
pub type XmlSchematronValidityWarningFunc =
    unsafe extern "C" fn(ctx: *mut c_void, msg: *const c_char);

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum XmlSchematronTestType {
    XmlSchematronAssert = 1,
    XmlSchematronReport = 2,
}

/**
 * xmlSchematronLet:
 *
 * A Schematron let variable
 */
pub type XmlSchematronLetPtr = *mut XmlSchematronLet;
#[repr(C)]
pub struct XmlSchematronLet {
    next: XmlSchematronLetPtr, /* the next let variable in the list */
    name: *mut XmlChar,        /* the name of the variable */
    comp: XmlXPathCompExprPtr, /* the compiled expression */
}

/**
 * xmlSchematronTest:
 *
 * A Schematrons test, either an assert or a report
 */
pub type XmlSchematronTestPtr = *mut XmlSchematronTest;
#[repr(C)]
pub struct XmlSchematronTest {
    next: XmlSchematronTestPtr, /* the next test in the list */
    typ: XmlSchematronTestType, /* the test type */
    node: XmlNodePtr,           /* the node in the tree */
    test: *mut XmlChar,         /* the expression to test */
    comp: XmlXPathCompExprPtr,  /* the compiled expression */
    report: *mut XmlChar,       /* the message to report */
}

/**
 * xmlSchematronRule:
 *
 * A Schematrons rule
 */
pub type XmlSchematronRulePtr = *mut XmlSchematronRule;
#[repr(C)]
pub struct XmlSchematronRule {
    next: XmlSchematronRulePtr,    /* the next rule in the list */
    patnext: XmlSchematronRulePtr, /* the next rule in the pattern list */
    node: XmlNodePtr,              /* the node in the tree */
    context: *mut XmlChar,         /* the context evaluation rule */
    tests: XmlSchematronTestPtr,   /* the list of tests */
    pattern: XmlPatternPtr,        /* the compiled pattern associated */
    report: *mut XmlChar,          /* the message to report */
    lets: XmlSchematronLetPtr,     /* the list of let variables */
}

/**
 * xmlSchematronPattern:
 *
 * A Schematrons pattern
 */
pub type XmlSchematronPatternPtr = *mut XmlSchematronPattern;
#[repr(C)]
pub struct XmlSchematronPattern {
    next: XmlSchematronPatternPtr, /* the next pattern in the list */
    rules: XmlSchematronRulePtr,   /* the list of rules */
    name: *mut XmlChar,            /* the name of the pattern */
}

/**
 * The schemas related types are kept internal
 */
pub type XmlSchematronPtr = *mut XmlSchematron;

/**
 * xmlSchematron:
 *
 * A Schematrons definition
 */
#[repr(C)]
pub struct XmlSchematron {
    name: *const XmlChar, /* schema name */
    preserve: c_int,      /* was the document passed by the user */
    doc: XmlDocPtr,       /* pointer to the parsed document */
    flags: c_int,         /* specific to this schematron */

    _private: *mut c_void, /* unused by the library */
    dict: XmlDictPtr,      /* the dictionary used internally */

    title: *const XmlChar, /* the title if any */

    nb_ns: c_int, /* the number of namespaces */

    nb_pattern: c_int,                 /* the number of patterns */
    patterns: XmlSchematronPatternPtr, /* the patterns found */
    rules: XmlSchematronRulePtr,       /* the rules gathered */
    nb_namespaces: c_int,              /* number of namespaces in the array */
    max_namespaces: c_int,             /* size of the array */
    namespaces: *mut *const XmlChar,   /* the array of namespaces */
}

/**
 * xmlSchematronValidCtxt:
 *
 * A Schematrons validation context
 */
pub type XmlSchematronValidCtxtPtr = *mut XmlSchematronValidCtxt;

#[repr(C)]
pub struct XmlSchematronValidCtxt {
    typ: c_int,
    flags: c_int, /* an or of xmlSchematronValidOptions */

    dict: XmlDictPtr,
    nberrors: c_int,
    err: c_int,

    schema: XmlSchematronPtr,
    xctxt: XmlXPathContextPtr,

    output_file: *mut FILE,   /* if using XML_SCHEMATRON_OUT_FILE */
    output_buffer: XmlBufPtr, /* if using XML_SCHEMATRON_OUT_BUFFER */
    #[cfg(feature = "output")]
    iowrite: Option<XmlOutputWriteCallback>, /* if using XML_SCHEMATRON_OUT_IO */
    #[cfg(feature = "output")]
    ioclose: Option<XmlOutputCloseCallback>,
    ioctx: *mut c_void,

    /* error reporting data */
    user_data: Option<GenericErrorContext>, /* user specific data block */
    error: Option<GenericError>,            /* the callback in case of errors */
    warning: Option<XmlSchematronValidityWarningFunc>, /* callback in case of warning */
    serror: Option<StructuredError>,        /* the structured function */
}

/**
 * A schemas validation context
 */
pub type XmlSchematronParserCtxtPtr = *mut XmlSchematronParserCtxt;

#[repr(C)]
pub struct XmlSchematronParserCtxt {
    typ: c_int,
    url: *const XmlChar,
    doc: XmlDocPtr,
    preserve: c_int, /* Whether the doc should be freed  */
    buffer: *const c_char,
    size: c_int,

    dict: XmlDictPtr, /* dictionary for interned string names */

    nberrors: c_int,
    err: c_int,
    xctxt: XmlXPathContextPtr, /* the XPath context used for compilation */
    schema: XmlSchematronPtr,

    nb_namespaces: c_int,            /* number of namespaces in the array */
    max_namespaces: c_int,           /* size of the array */
    namespaces: *mut *const XmlChar, /* the array of namespaces */

    nb_includes: c_int,        /* number of includes in the array */
    max_includes: c_int,       /* size of the array */
    includes: *mut XmlNodePtr, /* the array of includes */

    /* error reporting data */
    user_data: Option<GenericErrorContext>, /* user specific data block */
    error: Option<GenericError>,            /* the callback in case of errors */
    warning: Option<XmlSchematronValidityWarningFunc>, /* callback in case of warning */
    serror: Option<StructuredError>,        /* the structured function */
}

const XML_STRON_CTXT_PARSER: i32 = 1;
const XML_STRON_CTXT_VALIDATOR: i32 = 2;

const SCHEMATRON_PARSE_OPTIONS: XmlParserOption = XmlParserOption::XmlParseNoent;

const SCT_OLD_NS: &CStr = c"http://www.ascc.net/xml/schematron";

const XML_SCHEMATRON_NS: &CStr = c"http://purl.oclc.org/dsdl/schematron";

const XML_OLD_SCHEMATRON_NS: &CStr = SCT_OLD_NS;

/**
 * xmlSchematronPErrMemory:
 * @node: a context node
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
unsafe extern "C" fn xml_schematron_perr_memory(
    ctxt: XmlSchematronParserCtxtPtr,
    extra: *const c_char,
    node: XmlNodePtr,
) {
    if !ctxt.is_null() {
        (*ctxt).nberrors += 1;
    }
    __xml_simple_error(
        XmlErrorDomain::XmlFromSchemasp,
        XmlParserErrors::XmlErrNoMemory,
        node,
        null_mut(),
        extra,
    );
}

/*
 * Interfaces for parsing.
 */
/**
 * xmlSchematronNewParserCtxt:
 * @URL:  the location of the schema
 *
 * Create an XML Schematrons parse context for that file/resource expected
 * to contain an XML Schematrons file.
 *
 * Returns the parser context or NULL in case of error
 */
pub unsafe extern "C" fn xml_schematron_new_parser_ctxt(
    url: *const c_char,
) -> XmlSchematronParserCtxtPtr {
    if url.is_null() {
        return null_mut();
    }

    let ret: XmlSchematronParserCtxtPtr =
        xml_malloc(size_of::<XmlSchematronParserCtxt>()) as XmlSchematronParserCtxtPtr;
    if ret.is_null() {
        xml_schematron_perr_memory(
            null_mut(),
            c"allocating schema parser context".as_ptr() as _,
            null_mut(),
        );
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchematronParserCtxt>());
    (*ret).typ = XML_STRON_CTXT_PARSER;
    (*ret).dict = xml_dict_create();
    (*ret).url = xml_dict_lookup((*ret).dict, url as _, -1);
    (*ret).includes = null_mut();
    (*ret).xctxt = xml_xpath_new_context(null_mut());
    if (*ret).xctxt.is_null() {
        xml_schematron_perr_memory(
            null_mut(),
            c"allocating schema parser XPath context".as_ptr() as _,
            null_mut(),
        );
        xml_schematron_free_parser_ctxt(ret);
        return null_mut();
    }
    (*(*ret).xctxt).flags = XML_XPATH_CHECKNS as i32;
    ret
}

/**
 * xmlSchematronNewMemParserCtxt:
 * @buffer:  a pointer to a c_char array containing the schemas
 * @size:  the size of the array
 *
 * Create an XML Schematrons parse context for that memory buffer expected
 * to contain an XML Schematrons file.
 *
 * Returns the parser context or NULL in case of error
 */
pub unsafe extern "C" fn xml_schematron_new_mem_parser_ctxt(
    buffer: *const c_char,
    size: c_int,
) -> XmlSchematronParserCtxtPtr {
    if buffer.is_null() || size <= 0 {
        return null_mut();
    }

    let ret: XmlSchematronParserCtxtPtr =
        xml_malloc(size_of::<XmlSchematronParserCtxt>()) as XmlSchematronParserCtxtPtr;
    if ret.is_null() {
        xml_schematron_perr_memory(
            null_mut(),
            c"allocating schema parser context".as_ptr() as _,
            null_mut(),
        );
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchematronParserCtxt>());
    (*ret).buffer = buffer;
    (*ret).size = size;
    (*ret).dict = xml_dict_create();
    (*ret).xctxt = xml_xpath_new_context(null_mut());
    if (*ret).xctxt.is_null() {
        xml_schematron_perr_memory(
            null_mut(),
            c"allocating schema parser XPath context".as_ptr() as _,
            null_mut(),
        );
        xml_schematron_free_parser_ctxt(ret);
        return null_mut();
    }
    ret
}

/**
 * xmlSchematronNewDocParserCtxt:
 * @doc:  a preparsed document tree
 *
 * Create an XML Schematrons parse context for that document.
 * NB. The document may be modified during the parsing process.
 *
 * Returns the parser context or NULL in case of error
 */
pub unsafe extern "C" fn xml_schematron_new_doc_parser_ctxt(
    doc: XmlDocPtr,
) -> XmlSchematronParserCtxtPtr {
    if doc.is_null() {
        return null_mut();
    }

    let ret: XmlSchematronParserCtxtPtr =
        xml_malloc(size_of::<XmlSchematronParserCtxt>()) as XmlSchematronParserCtxtPtr;
    if ret.is_null() {
        xml_schematron_perr_memory(
            null_mut(),
            c"allocating schema parser context".as_ptr() as _,
            null_mut(),
        );
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchematronParserCtxt>());
    (*ret).doc = doc;
    (*ret).dict = xml_dict_create();
    /* The application has responsibility for the document */
    (*ret).preserve = 1;
    (*ret).xctxt = xml_xpath_new_context(doc);
    if (*ret).xctxt.is_null() {
        xml_schematron_perr_memory(
            null_mut(),
            c"allocating schema parser XPath context".as_ptr() as _,
            null_mut(),
        );
        xml_schematron_free_parser_ctxt(ret);
        return null_mut();
    }

    ret
}

/**
 * xmlSchematronFreeParserCtxt:
 * @ctxt:  the schema parser context
 *
 * Free the resources associated to the schema parser context
 */
pub unsafe extern "C" fn xml_schematron_free_parser_ctxt(ctxt: XmlSchematronParserCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    if !(*ctxt).doc.is_null() && (*ctxt).preserve == 0 {
        xml_free_doc((*ctxt).doc);
    }
    if !(*ctxt).xctxt.is_null() {
        xml_xpath_free_context((*ctxt).xctxt);
    }
    if !(*ctxt).namespaces.is_null() {
        xml_free((*ctxt).namespaces as _);
    }
    xml_dict_free((*ctxt).dict);
    xml_free(ctxt as _);
}

/*****
pub unsafe extern "C" fn xmlSchematronSetParserErrors(ctxt: xmlSchematronParserCtxtPtr, xmlSchematronValidityErrorFunc err, xmlSchematronValidityWarningFunc warn,
                     ctx: *mut c_void);
pub unsafe extern "C" fn c_int
        xmlSchematronGetParserErrors(ctxt: xmlSchematronParserCtxtPtr,
                    xmlSchematronValidityErrorFunc * err,
                    xmlSchematronValidityWarningFunc * warn,
                    c_void **ctx);
pub unsafe extern "C" fn c_int
        xmlSchematronIsValid	(ctxt: xmlSchematronValidCtxtPtr);
 *****/

macro_rules! IS_SCHEMATRON {
    ($node:expr, $elem:expr) => {
        !$node.is_null()
            && ((*$node).typ == XmlElementType::XmlElementNode)
            && !(*$node).ns.is_null()
            && xml_str_equal((*$node).name, $elem)
            && (xml_str_equal(
                (*(*$node).ns).href.load(Ordering::Relaxed),
                XML_SCHEMATRON_NS.as_ptr() as _,
            ) || xml_str_equal(
                (*(*$node).ns).href.load(Ordering::Relaxed),
                XML_OLD_SCHEMATRON_NS.as_ptr() as _,
            ))
    };
}

macro_rules! NEXT_SCHEMATRON {
    ($node:expr) => {
        while !$node.is_null() {
            if ((*$node).typ == XmlElementType::XmlElementNode)
                && !(*$node).ns.is_null()
                && (xml_str_equal(
                    (*(*$node).ns).href.load(Ordering::Relaxed),
                    XML_SCHEMATRON_NS.as_ptr() as _,
                ) || xml_str_equal(
                    (*(*$node).ns).href.load(Ordering::Relaxed),
                    XML_OLD_SCHEMATRON_NS.as_ptr() as _,
                ))
            {
                break;
            }
            $node = (*$node).next;
        }
    };
}

/**
 * xmlSchematronPErr:
 * @ctxt: the parsing context
 * @node: the context node
 * @error: the error code
 * @msg: the error message
 * @str1: extra data
 * @str2: extra data
 *
 * Handle a parser error
 */
unsafe extern "C" fn xml_schematron_perr(
    ctxt: XmlSchematronParserCtxtPtr,
    node: XmlNodePtr,
    error: XmlParserErrors,
    msg: *const c_char,
    str1: *const XmlChar,
    str2: *const XmlChar,
) {
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
        node as _,
        XmlErrorDomain::XmlFromSchemasp,
        error,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        str1 as _,
        str2 as _,
        None,
        0,
        0,
        msg,
        str1,
        str2
    );
}

/**
 * xmlSchematronNewSchematron:
 * @ctxt:  a schema validation context
 *
 * Allocate a new Schematron structure.
 *
 * Returns the newly allocated structure or NULL in case or error
 */
unsafe extern "C" fn xml_schematron_new_schematron(
    ctxt: XmlSchematronParserCtxtPtr,
) -> XmlSchematronPtr {
    let ret: XmlSchematronPtr = xml_malloc(size_of::<XmlSchematron>()) as XmlSchematronPtr;
    if ret.is_null() {
        xml_schematron_perr_memory(ctxt, c"allocating schema".as_ptr() as _, null_mut());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchematron>());
    (*ret).dict = (*ctxt).dict;
    xml_dict_reference((*ret).dict);

    ret
}

/**
 * xmlSchematronAddNamespace:
 * @ctxt:  the schema parser context
 * @prefix:  the namespace prefix
 * @ns:  the namespace name
 *
 * Add a namespace definition in the context
 */
unsafe extern "C" fn xml_schematron_add_namespace(
    ctxt: XmlSchematronParserCtxtPtr,
    prefix: *const XmlChar,
    ns: *const XmlChar,
) {
    if (*ctxt).namespaces.is_null() {
        (*ctxt).max_namespaces = 10;
        (*ctxt).namespaces =
            xml_malloc((*ctxt).max_namespaces as usize * 2 * size_of::<*const XmlChar>()) as _;
        if (*ctxt).namespaces.is_null() {
            xml_schematron_perr_memory(
                null_mut(),
                c"allocating parser namespaces".as_ptr() as _,
                null_mut(),
            );
            return;
        }
        (*ctxt).nb_namespaces = 0;
    } else if (*ctxt).nb_namespaces + 2 >= (*ctxt).max_namespaces {
        let tmp: *mut *const XmlChar = xml_realloc(
            (*ctxt).namespaces as _,
            (*ctxt).max_namespaces as usize * 4 * size_of::<*const XmlChar>(),
        ) as _;
        if tmp.is_null() {
            xml_schematron_perr_memory(
                null_mut(),
                c"allocating parser namespaces".as_ptr() as _,
                null_mut(),
            );
            return;
        }
        (*ctxt).namespaces = tmp;
        (*ctxt).max_namespaces *= 2;
    }
    *(*ctxt).namespaces.add(2 * (*ctxt).nb_namespaces as usize) =
        xml_dict_lookup((*ctxt).dict, ns, -1);
    *(*ctxt)
        .namespaces
        .add(2 * (*ctxt).nb_namespaces as usize + 1) = xml_dict_lookup((*ctxt).dict, prefix, -1);
    (*ctxt).nb_namespaces += 1;
    *(*ctxt).namespaces.add(2 * (*ctxt).nb_namespaces as usize) = null_mut();
    *(*ctxt)
        .namespaces
        .add(2 * (*ctxt).nb_namespaces as usize + 1) = null_mut();
}

/**
 * xmlSchematronAddPattern:
 * @ctxt: the schema parsing context
 * @schema:  a schema structure
 * @node:  the node hosting the pattern
 * @id: the id or name of the pattern
 *
 * Add a pattern to a schematron
 *
 * Returns the new pointer or NULL in case of error
 */
unsafe extern "C" fn xml_schematron_add_pattern(
    ctxt: XmlSchematronParserCtxtPtr,
    schema: XmlSchematronPtr,
    node: XmlNodePtr,
    name: *mut XmlChar,
) -> XmlSchematronPatternPtr {
    if ctxt.is_null() || schema.is_null() || node.is_null() || name.is_null() {
        return null_mut();
    }

    let ret: XmlSchematronPatternPtr =
        xml_malloc(size_of::<XmlSchematronPattern>()) as XmlSchematronPatternPtr;
    if ret.is_null() {
        xml_schematron_perr_memory(ctxt, c"allocating schema pattern".as_ptr() as _, node);
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

/**
 * xmlSchematronAddRule:
 * @ctxt: the schema parsing context
 * @schema:  a schema structure
 * @node:  the node hosting the rule
 * @context: the associated context string
 * @report: the associated report string
 *
 * Add a rule to a schematron
 *
 * Returns the new pointer or NULL in case of error
 */
unsafe extern "C" fn xml_schematron_add_rule(
    ctxt: XmlSchematronParserCtxtPtr,
    schema: XmlSchematronPtr,
    pat: XmlSchematronPatternPtr,
    node: XmlNodePtr,
    context: *mut XmlChar,
    report: *mut XmlChar,
) -> XmlSchematronRulePtr {
    if ctxt.is_null() || schema.is_null() || node.is_null() || context.is_null() {
        return null_mut();
    }

    /*
     * Try first to compile the pattern
     */
    let pattern: XmlPatternPtr = xml_patterncompile(
        context,
        (*ctxt).dict,
        XmlPatternFlags::XmlPatternXpath as i32,
        (*ctxt).namespaces,
    );
    if pattern.is_null() {
        xml_schematron_perr(
            ctxt,
            node,
            XmlParserErrors::XmlSchemapNoroot,
            c"Failed to compile context expression %s".as_ptr() as _,
            context,
            null_mut(),
        );
    }

    let ret: XmlSchematronRulePtr =
        xml_malloc(size_of::<XmlSchematronRule>()) as XmlSchematronRulePtr;
    if ret.is_null() {
        xml_schematron_perr_memory(ctxt, c"allocating schema rule".as_ptr() as _, node);
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

/**
 * xmlSchematronParseTestReportMsg:
 * @ctxt:  the schema parser context
 * @con:  the assert or report node
 *
 * Format the message content of the assert or report test
 */
unsafe extern "C" fn xml_schematron_parse_test_report_msg(
    ctxt: XmlSchematronParserCtxtPtr,
    con: XmlNodePtr,
) {
    let mut child: XmlNodePtr;
    let mut comp: XmlXPathCompExprPtr;

    child = (*con).children;
    while !child.is_null() {
        if ((*child).typ == XmlElementType::XmlTextNode)
            || ((*child).typ == XmlElementType::XmlCdataSectionNode)
        /* Do Nothing */
        {
        } else if IS_SCHEMATRON!(child, c"name".as_ptr() as _) {
            /* Do Nothing */
        } else if IS_SCHEMATRON!(child, c"value-of".as_ptr() as _) {
            let select: *mut XmlChar = xml_get_no_ns_prop(child, c"select".as_ptr() as _);

            if select.is_null() {
                xml_schematron_perr(
                    ctxt,
                    child,
                    XmlParserErrors::XmlSchemavAttrinvalid,
                    c"value-of has no select attribute".as_ptr() as _,
                    null(),
                    null_mut(),
                );
            } else {
                /*
                 * try first to compile the test expression
                 */
                comp = xml_xpath_ctxt_compile((*ctxt).xctxt, select);
                if comp.is_null() {
                    xml_schematron_perr(
                        ctxt,
                        child,
                        XmlParserErrors::XmlSchemavAttrinvalid,
                        c"Failed to compile select expression %s".as_ptr() as _,
                        select,
                        null_mut(),
                    );
                }
                xml_xpath_free_comp_expr(comp);
            }
            xml_free(select as _);
        }
        child = (*child).next;
        continue;
    }
}

/**
 * xmlSchematronAddTest:
 * @ctxt: the schema parsing context
 * @type:  the type of test
 * @rule:  the parent rule
 * @node:  the node hosting the test
 * @test: the associated test
 * @report: the associated report string
 *
 * Add a test to a schematron
 *
 * Returns the new pointer or NULL in case of error
 */
unsafe extern "C" fn xml_schematron_add_test(
    ctxt: XmlSchematronParserCtxtPtr,
    typ: XmlSchematronTestType,
    rule: XmlSchematronRulePtr,
    node: XmlNodePtr,
    test: *mut XmlChar,
    report: *mut XmlChar,
) -> XmlSchematronTestPtr {
    if ctxt.is_null() || rule.is_null() || node.is_null() || test.is_null() {
        return null_mut();
    }

    /*
     * try first to compile the test expression
     */
    let comp: XmlXPathCompExprPtr = xml_xpath_ctxt_compile((*ctxt).xctxt, test);
    if comp.is_null() {
        xml_schematron_perr(
            ctxt,
            node,
            XmlParserErrors::XmlSchemapNoroot,
            c"Failed to compile test expression %s".as_ptr() as _,
            test,
            null_mut(),
        );
        return null_mut();
    }

    let ret: XmlSchematronTestPtr =
        xml_malloc(size_of::<XmlSchematronTest>()) as XmlSchematronTestPtr;
    if ret.is_null() {
        xml_schematron_perr_memory(ctxt, c"allocating schema test".as_ptr() as _, node);
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

/**
 * xmlSchematronParseRule:
 * @ctxt:  a schema validation context
 * @rule:  the rule node
 *
 * parse a rule element
 */
unsafe extern "C" fn xml_schematron_parse_rule(
    ctxt: XmlSchematronParserCtxtPtr,
    pattern: XmlSchematronPatternPtr,
    rule: XmlNodePtr,
) {
    let mut cur: XmlNodePtr;
    let mut nb_checks: c_int = 0;
    let mut test: *mut XmlChar;

    let mut report: *mut XmlChar;
    let mut name: *mut XmlChar;
    let mut value: *mut XmlChar;
    let ruleptr: XmlSchematronRulePtr;
    let mut testptr: XmlSchematronTestPtr;

    if ctxt.is_null() || rule.is_null() {
        return;
    }

    let context: *mut XmlChar = xml_get_no_ns_prop(rule, c"context".as_ptr() as _);
    if context.is_null() {
        xml_schematron_perr(
            ctxt,
            rule,
            XmlParserErrors::XmlSchemapNoroot,
            c"rule has no context attribute".as_ptr() as _,
            null(),
            null_mut(),
        );
        return;
    } else if *context.add(0) == 0 {
        xml_schematron_perr(
            ctxt,
            rule,
            XmlParserErrors::XmlSchemapNoroot,
            c"rule has an empty context attribute".as_ptr() as _,
            null(),
            null_mut(),
        );
        xml_free(context as _);
        return;
    } else {
        ruleptr = xml_schematron_add_rule(ctxt, (*ctxt).schema, pattern, rule, context, null_mut());
        if ruleptr.is_null() {
            xml_free(context as _);
            return;
        }
    }

    cur = (*rule).children;
    NEXT_SCHEMATRON!(cur);
    while !cur.is_null() {
        if IS_SCHEMATRON!(cur, c"let".as_ptr() as _) {
            name = xml_get_no_ns_prop(cur, c"name".as_ptr() as _);
            if name.is_null() {
                xml_schematron_perr(
                    ctxt,
                    cur,
                    XmlParserErrors::XmlSchemapNoroot,
                    c"let has no name attribute".as_ptr() as _,
                    null(),
                    null_mut(),
                );
                return;
            } else if *name.add(0) == 0 {
                xml_schematron_perr(
                    ctxt,
                    cur,
                    XmlParserErrors::XmlSchemapNoroot,
                    c"let has an empty name attribute".as_ptr() as _,
                    null(),
                    null_mut(),
                );
                xml_free(name as _);
                return;
            }
            value = xml_get_no_ns_prop(cur, c"value".as_ptr() as _);
            if value.is_null() {
                xml_schematron_perr(
                    ctxt,
                    cur,
                    XmlParserErrors::XmlSchemapNoroot,
                    c"let has no value attribute".as_ptr() as _,
                    null(),
                    null_mut(),
                );
                return;
            } else if *value.add(0) == 0 {
                xml_schematron_perr(
                    ctxt,
                    cur,
                    XmlParserErrors::XmlSchemapNoroot,
                    c"let has an empty value attribute".as_ptr() as _,
                    null(),
                    null_mut(),
                );
                xml_free(value as _);
                return;
            }

            let var_comp: XmlXPathCompExprPtr = xml_xpath_ctxt_compile((*ctxt).xctxt, value);
            if var_comp.is_null() {
                xml_schematron_perr(
                    ctxt,
                    cur,
                    XmlParserErrors::XmlSchemapNoroot,
                    c"Failed to compile let expression %s".as_ptr() as _,
                    value,
                    null_mut(),
                );
                return;
            }

            let letr: XmlSchematronLetPtr =
                malloc(size_of::<XmlSchematronLet>()) as XmlSchematronLetPtr;
            (*letr).name = name;
            (*letr).comp = var_comp;
            (*letr).next = null_mut();

            /* add new let variable to the beginning of the list */
            if !(*ruleptr).lets.is_null() {
                (*letr).next = (*ruleptr).lets;
            }
            (*ruleptr).lets = letr;

            xml_free(value as _);
        } else if IS_SCHEMATRON!(cur, c"assert".as_ptr() as _) {
            nb_checks += 1;
            test = xml_get_no_ns_prop(cur, c"test".as_ptr() as _);
            if test.is_null() {
                xml_schematron_perr(
                    ctxt,
                    cur,
                    XmlParserErrors::XmlSchemapNoroot,
                    c"assert has no test attribute".as_ptr() as _,
                    null(),
                    null_mut(),
                );
            } else if *test.add(0) == 0 {
                xml_schematron_perr(
                    ctxt,
                    cur,
                    XmlParserErrors::XmlSchemapNoroot,
                    c"assert has an empty test attribute".as_ptr() as _,
                    null(),
                    null_mut(),
                );
                xml_free(test as _);
            } else {
                xml_schematron_parse_test_report_msg(ctxt, cur);
                report = xml_node_get_content(cur);

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
        } else if IS_SCHEMATRON!(cur, c"report".as_ptr() as _) {
            nb_checks += 1;
            test = xml_get_no_ns_prop(cur, c"test".as_ptr() as _);
            if test.is_null() {
                xml_schematron_perr(
                    ctxt,
                    cur,
                    XmlParserErrors::XmlSchemapNoroot,
                    c"assert has no test attribute".as_ptr() as _,
                    null(),
                    null_mut(),
                );
            } else if *test.add(0) == 0 {
                xml_schematron_perr(
                    ctxt,
                    cur,
                    XmlParserErrors::XmlSchemapNoroot,
                    c"assert has an empty test attribute".as_ptr() as _,
                    null(),
                    null_mut(),
                );
                xml_free(test as _);
            } else {
                xml_schematron_parse_test_report_msg(ctxt, cur);
                report = xml_node_get_content(cur);

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
        } else {
            xml_schematron_perr(
                ctxt,
                cur,
                XmlParserErrors::XmlSchemapNoroot,
                c"Expecting an assert or a report element instead of %s".as_ptr() as _,
                (*cur).name,
                null_mut(),
            );
        }
        cur = (*cur).next;
        NEXT_SCHEMATRON!(cur);
    }
    if nb_checks == 0 {
        xml_schematron_perr(
            ctxt,
            rule,
            XmlParserErrors::XmlSchemapNoroot,
            c"rule has no assert nor report element".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
    }
}

/**
 * xmlSchematronParsePattern:
 * @ctxt:  a schema validation context
 * @pat:  the pattern node
 *
 * parse a pattern element
 */
unsafe extern "C" fn xml_schematron_parse_pattern(
    ctxt: XmlSchematronParserCtxtPtr,
    pat: XmlNodePtr,
) {
    let mut cur: XmlNodePtr;

    let mut nb_rules: c_int = 0;
    let mut id: *mut XmlChar;

    if ctxt.is_null() || pat.is_null() {
        return;
    }

    id = xml_get_no_ns_prop(pat, c"id".as_ptr() as _);
    if id.is_null() {
        id = xml_get_no_ns_prop(pat, c"name".as_ptr() as _);
    }
    let pattern: XmlSchematronPatternPtr =
        xml_schematron_add_pattern(ctxt, (*ctxt).schema, pat, id);
    if pattern.is_null() {
        if !id.is_null() {
            xml_free(id as _);
        }
        return;
    }
    cur = (*pat).children;
    NEXT_SCHEMATRON!(cur);
    while !cur.is_null() {
        if IS_SCHEMATRON!(cur, c"rule".as_ptr() as _) {
            xml_schematron_parse_rule(ctxt, pattern, cur);
            nb_rules += 1;
        } else {
            xml_schematron_perr(
                ctxt,
                cur,
                XmlParserErrors::XmlSchemapNoroot,
                c"Expecting a rule element instead of %s".as_ptr() as _,
                (*cur).name,
                null_mut(),
            );
        }
        cur = (*cur).next;
        NEXT_SCHEMATRON!(cur);
    }
    if nb_rules == 0 {
        xml_schematron_perr(
            ctxt,
            pat,
            XmlParserErrors::XmlSchemapNoroot,
            c"Pattern has no rule element".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
    }
}

/**
 * xmlSchematronParse:
 * @ctxt:  a schema validation context
 *
 * parse a schema definition resource and build an internal
 * XML Schema structure which can be used to validate instances.
 *
 * Returns the internal XML Schematron structure built from the resource or
 *         NULL in case of error
 */
pub unsafe extern "C" fn xml_schematron_parse(
    ctxt: XmlSchematronParserCtxtPtr,
) -> XmlSchematronPtr {
    let doc: XmlDocPtr;
    let mut ret: XmlSchematronPtr = null_mut();
    let mut cur: XmlNodePtr;
    let mut preserve: c_int = 0;

    if ctxt.is_null() {
        return null_mut();
    }

    (*ctxt).nberrors = 0;

    /*
     * First step is to parse the input document into an DOM/Infoset
     */
    if !(*ctxt).url.is_null() {
        doc = xml_read_file(
            (*ctxt).url as _,
            null_mut(),
            SCHEMATRON_PARSE_OPTIONS as i32,
        );
        if doc.is_null() {
            xml_schematron_perr(
                ctxt,
                null_mut(),
                XmlParserErrors::XmlSchemapFailedLoad,
                c"xmlSchematronParse: could not load '%s'.\n".as_ptr() as _,
                (*ctxt).url,
                null_mut(),
            );
            return null_mut();
        }
        (*ctxt).preserve = 0;
    } else if !(*ctxt).buffer.is_null() {
        doc = xml_read_memory(
            (*ctxt).buffer,
            (*ctxt).size,
            null_mut(),
            null_mut(),
            SCHEMATRON_PARSE_OPTIONS as i32,
        );
        if doc.is_null() {
            xml_schematron_perr(
                ctxt,
                null_mut(),
                XmlParserErrors::XmlSchemapFailedParse,
                c"xmlSchematronParse: could not parse.\n".as_ptr() as _,
                null_mut(),
                null_mut(),
            );
            return null_mut();
        }
        (*doc).url = xml_strdup(c"in_memory_buffer".as_ptr() as _);
        (*ctxt).url = xml_dict_lookup((*ctxt).dict, c"in_memory_buffer".as_ptr() as _, -1);
        (*ctxt).preserve = 0;
    } else if !(*ctxt).doc.is_null() {
        doc = (*ctxt).doc;
        preserve = 1;
        (*ctxt).preserve = 1;
    } else {
        xml_schematron_perr(
            ctxt,
            null_mut(),
            XmlParserErrors::XmlSchemapNothingToParse,
            c"xmlSchematronParse: could not parse.\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        return null_mut();
    }

    /*
     * Then extract the root and Schematron parse it
     */
    let root: XmlNodePtr = xml_doc_get_root_element(doc);
    if root.is_null() {
        xml_schematron_perr(
            ctxt,
            doc as XmlNodePtr,
            XmlParserErrors::XmlSchemapNoroot,
            c"The schema has no document element.\n".as_ptr() as _,
            null_mut(),
            null_mut(),
        );
        if preserve == 0 {
            xml_free_doc(doc);
        }
        return null_mut();
    }

    if !IS_SCHEMATRON!(root, c"schema".as_ptr() as _) {
        xml_schematron_perr(
            ctxt,
            root,
            XmlParserErrors::XmlSchemapNoroot,
            c"The XML document '%s' is not a XML schematron document".as_ptr() as _,
            (*ctxt).url,
            null_mut(),
        );
        // goto exit;
    } else {
        ret = xml_schematron_new_schematron(ctxt);
        if ret.is_null() {
            // goto exit;
        } else {
            (*ctxt).schema = ret;

            /*
             * scan the schema elements
             */
            cur = (*root).children;
            NEXT_SCHEMATRON!(cur);
            if IS_SCHEMATRON!(cur, c"title".as_ptr() as _) {
                let title: *mut XmlChar = xml_node_get_content(cur);
                if !title.is_null() {
                    (*ret).title = xml_dict_lookup((*ret).dict, title, -1);
                    xml_free(title as _);
                }
                cur = (*cur).next;
                NEXT_SCHEMATRON!(cur);
            }
            while IS_SCHEMATRON!(cur, c"ns".as_ptr() as _) {
                let prefix: *mut XmlChar = xml_get_no_ns_prop(cur, c"prefix".as_ptr() as _);
                let uri: *mut XmlChar = xml_get_no_ns_prop(cur, c"uri".as_ptr() as _);
                if uri.is_null() || *uri.add(0) == 0 {
                    xml_schematron_perr(
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        c"ns element has no uri".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
                if prefix.is_null() || *prefix.add(0) == 0 {
                    xml_schematron_perr(
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        c"ns element has no prefix".as_ptr() as _,
                        null_mut(),
                        null_mut(),
                    );
                }
                if !prefix.is_null() && !uri.is_null() {
                    xml_xpath_register_ns((*ctxt).xctxt, prefix, uri);
                    xml_schematron_add_namespace(ctxt, prefix, uri);
                    (*ret).nb_ns += 1;
                }
                if !uri.is_null() {
                    xml_free(uri as _);
                }
                if !prefix.is_null() {
                    xml_free(prefix as _);
                }
                cur = (*cur).next;
                NEXT_SCHEMATRON!(cur);
            }
            while !cur.is_null() {
                if IS_SCHEMATRON!(cur, c"pattern".as_ptr() as _) {
                    xml_schematron_parse_pattern(ctxt, cur);
                    (*ret).nb_pattern += 1;
                } else {
                    xml_schematron_perr(
                        ctxt,
                        cur,
                        XmlParserErrors::XmlSchemapNoroot,
                        c"Expecting a pattern element instead of %s".as_ptr() as _,
                        (*cur).name,
                        null_mut(),
                    );
                }
                cur = (*cur).next;
                NEXT_SCHEMATRON!(cur);
            }
            if (*ret).nb_pattern == 0 {
                xml_schematron_perr(
                    ctxt,
                    root,
                    XmlParserErrors::XmlSchemapNoroot,
                    c"The schematron document '%s' has no pattern".as_ptr() as _,
                    (*ctxt).url,
                    null_mut(),
                );
            // goto exit;
            } else {
                /* the original document must be kept for reporting */
                (*ret).doc = doc;
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
            (*ret).namespaces = (*ctxt).namespaces;
            (*ret).nb_namespaces = (*ctxt).nb_namespaces;
            (*ctxt).namespaces = null_mut();
        }
    }
    ret
}

/**
 * xmlSchematronFreeTests:
 * @tests:  a list of tests
 *
 * Free a list of tests.
 */
unsafe extern "C" fn xml_schematron_free_tests(mut tests: XmlSchematronTestPtr) {
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

/**
 * xmlSchematronFreeLets:
 * @lets:  a list of let variables
 *
 * Free a list of let variables.
 */
unsafe extern "C" fn xml_schematron_free_lets(mut lets: XmlSchematronLetPtr) {
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

/**
 * xmlSchematronFreeRules:
 * @rules:  a list of rules
 *
 * Free a list of rules.
 */
unsafe extern "C" fn xml_schematron_free_rules(mut rules: XmlSchematronRulePtr) {
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

/**
 * xmlSchematronFreePatterns:
 * @patterns:  a list of patterns
 *
 * Free a list of patterns.
 */
unsafe extern "C" fn xml_schematron_free_patterns(mut patterns: XmlSchematronPatternPtr) {
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

/**
 * xmlSchematronFree:
 * @schema:  a schema structure
 *
 * Deallocate a Schematron structure.
 */
pub unsafe extern "C" fn xml_schematron_free(schema: XmlSchematronPtr) {
    if schema.is_null() {
        return;
    }

    if !(*schema).doc.is_null() && (*schema).preserve == 0 {
        xml_free_doc((*schema).doc);
    }

    if !(*schema).namespaces.is_null() {
        xml_free((*schema).namespaces as _);
    }

    xml_schematron_free_rules((*schema).rules);
    xml_schematron_free_patterns((*schema).patterns);
    xml_dict_free((*schema).dict);
    xml_free(schema as _);
}

/*
 * Interfaces for validating
 */
/**
 * xmlSchematronSetValidStructuredErrors:
 * @ctxt:  a Schematron validation context
 * @serror:  the structured error function
 * @ctx: the functions context
 *
 * Set the structured error callback
 */
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

/******
pub unsafe extern "C" fn xmlSchematronSetValidErrors	(ctxt: xmlSchematronValidCtxtPtr, xmlSchematronValidityErrorFunc err, xmlSchematronValidityWarningFunc warn,
                     ctx: *mut c_void);
pub unsafe extern "C" fn c_int xmlSchematronGetValidErrors	(ctxt: xmlSchematronValidCtxtPtr, xmlSchematronValidityErrorFunc *err, xmlSchematronValidityWarningFunc *warn,
                     c_void **ctx);
pub unsafe extern "C" fn c_int xmlSchematronSetValidOptions(ctxt: xmlSchematronValidCtxtPtr, c_int options);
pub unsafe extern "C" fn c_int xmlSchematronValidCtxtGetOptions(ctxt: xmlSchematronValidCtxtPtr);
pub unsafe extern "C" fn c_int
            xmlSchematronValidateOneElement (ctxt: xmlSchematronValidCtxtPtr,
                             xmlNodePtr elem);
 *******/

/**
 * xmlSchematronVTypeErrMemory:
 * @node: a context node
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
unsafe extern "C" fn xml_schematron_verr_memory(
    ctxt: XmlSchematronValidCtxtPtr,
    extra: *const c_char,
    node: XmlNodePtr,
) {
    if !ctxt.is_null() {
        (*ctxt).nberrors += 1;
        (*ctxt).err = XmlParserErrors::XmlSchemavInternal as i32;
    }
    __xml_simple_error(
        XmlErrorDomain::XmlFromSchemasv,
        XmlParserErrors::XmlErrNoMemory,
        node,
        null_mut(),
        extra,
    );
}

/**
 * xmlSchematronNewValidCtxt:
 * @schema:  a precompiled XML Schematrons
 * @options: a set of xmlSchematronValidOptions
 *
 * Create an XML Schematrons validation context based on the given schema.
 *
 * Returns the validation context or NULL in case of error
 */
pub unsafe extern "C" fn xml_schematron_new_valid_ctxt(
    schema: XmlSchematronPtr,
    options: c_int,
) -> XmlSchematronValidCtxtPtr {
    let ret: XmlSchematronValidCtxtPtr =
        xml_malloc(size_of::<XmlSchematronValidCtxt>()) as XmlSchematronValidCtxtPtr;
    if ret.is_null() {
        xml_schematron_verr_memory(
            null_mut(),
            c"allocating validation context".as_ptr() as _,
            null_mut(),
        );
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSchematronValidCtxt>());
    (*ret).typ = XML_STRON_CTXT_VALIDATOR;
    (*ret).schema = schema;
    (*ret).xctxt = xml_xpath_new_context(null_mut());
    (*ret).flags = options;
    if (*ret).xctxt.is_null() {
        xml_schematron_perr_memory(
            null_mut(),
            c"allocating schema parser XPath context".as_ptr() as _,
            null_mut(),
        );
        xml_schematron_free_valid_ctxt(ret);
        return null_mut();
    }
    for i in 0..(*schema).nb_namespaces {
        if (*(*schema).namespaces.add(2 * i as usize)).is_null()
            || (*(*schema).namespaces.add(2 * i as usize + 1)).is_null()
        {
            break;
        }
        xml_xpath_register_ns(
            (*ret).xctxt,
            *(*schema).namespaces.add(2 * i as usize + 1),
            *(*schema).namespaces.add(2 * i as usize),
        );
    }
    ret
}

/**
 * xmlSchematronFreeValidCtxt:
 * @ctxt:  the schema validation context
 *
 * Free the resources associated to the schema validation context
 */
pub unsafe extern "C" fn xml_schematron_free_valid_ctxt(ctxt: XmlSchematronValidCtxtPtr) {
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

/**
 * xmlSchematronRegisterVariables:
 * @ctxt:  the schema validation context
 * @let:  the list of let variables
 * @instance:  the document instance tree
 * @cur:  the current node
 *
 * Registers a list of let variables to the current context of @cur
 *
 * Returns -1 in case of errors, otherwise 0
 */
unsafe extern "C" fn xml_schematron_register_variables(
    ctxt: XmlXPathContextPtr,
    mut letr: XmlSchematronLetPtr,
    instance: XmlDocPtr,
    cur: XmlNodePtr,
) -> c_int {
    let mut let_eval: XmlXPathObjectPtr;

    (*ctxt).doc = instance;
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

unsafe extern "C" fn xml_schematron_get_node(
    ctxt: XmlSchematronValidCtxtPtr,
    cur: XmlNodePtr,
    xpath: *const XmlChar,
) -> XmlNodePtr {
    let mut node: XmlNodePtr = null_mut();

    if ctxt.is_null() || cur.is_null() || xpath.is_null() {
        return null_mut();
    }

    (*(*ctxt).xctxt).doc = (*cur).doc;
    (*(*ctxt).xctxt).node = cur;
    let ret: XmlXPathObjectPtr = xml_xpath_eval(xpath, (*ctxt).xctxt);
    if ret.is_null() {
        return null_mut();
    }

    if (*ret).typ == XmlXPathObjectType::XpathNodeset
        && !(*ret).nodesetval.is_null()
        && (*(*ret).nodesetval).node_nr > 0
    {
        node = *(*(*ret).nodesetval).node_tab.add(0);
    }

    xml_xpath_free_object(ret);
    node
}

/**
 * xmlSchematronFormatReport:
 * @ctxt:  the validation context
 * @test: the test node
 * @cur: the current node tested
 *
 * Build the string being reported to the user.
 *
 * Returns a report string or NULL in case of error. The string needs
 *         to be deallocated by the caller
 */
unsafe extern "C" fn xml_schematron_format_report(
    ctxt: XmlSchematronValidCtxtPtr,
    test: XmlNodePtr,
    cur: XmlNodePtr,
) -> *mut XmlChar {
    let mut ret: *mut XmlChar = null_mut();
    let mut child: XmlNodePtr;
    let mut node: XmlNodePtr;
    let mut comp: XmlXPathCompExprPtr;

    if test.is_null() || cur.is_null() {
        return ret;
    }

    child = (*test).children;
    while !child.is_null() {
        if ((*child).typ == XmlElementType::XmlTextNode)
            || ((*child).typ == XmlElementType::XmlCdataSectionNode)
        {
            ret = xml_strcat(ret, (*child).content);
        } else if IS_SCHEMATRON!(child, c"name".as_ptr() as _) {
            let path: *mut XmlChar = xml_get_no_ns_prop(child, c"path".as_ptr() as _);

            node = cur;
            if !path.is_null() {
                node = xml_schematron_get_node(ctxt, cur, path);
                if node.is_null() {
                    node = cur;
                }
                xml_free(path as _);
            }

            if (*node).ns.is_null() || (*(*node).ns).prefix.load(Ordering::Relaxed).is_null() {
                ret = xml_strcat(ret, (*node).name);
            } else {
                ret = xml_strcat(ret, (*(*node).ns).prefix.load(Ordering::Relaxed));
                ret = xml_strcat(ret, c":".as_ptr() as _);
                ret = xml_strcat(ret, (*node).name);
            }
        } else if IS_SCHEMATRON!(child, c"value-of".as_ptr() as _) {
            let select: *mut XmlChar = xml_get_no_ns_prop(child, c"select".as_ptr() as _);
            comp = xml_xpath_ctxt_compile((*ctxt).xctxt, select);
            let eval: XmlXPathObjectPtr = xml_xpath_compiled_eval(comp, (*ctxt).xctxt);

            match (*eval).typ {
                XmlXPathObjectType::XpathNodeset => {
                    let spacer: *mut XmlChar = c" ".as_ptr() as _;

                    if !(*eval).nodesetval.is_null() {
                        for indx in 0..(*(*eval).nodesetval).node_nr {
                            if indx > 0 {
                                ret = xml_strcat(ret, spacer);
                            }
                            ret = xml_strcat(
                                ret,
                                (*(*(*(*eval).nodesetval).node_tab.add(indx as usize))).name,
                            );
                        }
                    } else {
                        generic_error!("Empty node set\n");
                    }
                }
                XmlXPathObjectType::XpathBoolean => {
                    let str: *const c_char = if (*eval).boolval != 0 {
                        c"True".as_ptr()
                    } else {
                        c"False".as_ptr()
                    };
                    ret = xml_strcat(ret, str as _);
                }
                XmlXPathObjectType::XpathNumber => {
                    let size: c_int =
                        snprintf(null_mut(), 0, c"%0g".as_ptr() as _, (*eval).floatval);
                    let buf: *mut XmlChar = malloc(size as usize) as _;
                    /* xmlStrPrintf(buf, size, c"%0g".as_ptr() as _, (*eval).floatval); // doesn't work */
                    sprintf(buf as _, c"%0g".as_ptr() as _, (*eval).floatval);
                    ret = xml_strcat(ret, buf);
                    xml_free(buf as _);
                }
                XmlXPathObjectType::XpathString => {
                    ret = xml_strcat(ret, (*eval).stringval);
                }
                _ => {
                    generic_error!("Unsupported XPATH Type: {:?}\n", (*eval).typ);
                }
            }
            xml_xpath_free_object(eval);
            xml_xpath_free_comp_expr(comp);
            xml_free(select as _);
        } else {
            child = (*child).next;
            continue;
        }

        /*
         * remove superfluous \n
         */
        if !ret.is_null() {
            let mut len: c_int = xml_strlen(ret) as _;
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

        child = (*child).next;
    }
    ret
}

/**
 * xmlSchematronReportOutput:
 * @ctxt: the validation context
 * @cur: the current node tested
 * @msg: the message output
 *
 * Output part of the report to whatever channel the user selected
 */
unsafe extern "C" fn xml_schematron_report_output(
    _ctxt: XmlSchematronValidCtxtPtr,
    _cur: XmlNodePtr,
    msg: *const c_char,
) {
    /* TODO */
    eprintln!("{}", CStr::from_ptr(msg).to_string_lossy());
}

/**
 * xmlSchematronReportSuccess:
 * @ctxt:  the validation context
 * @test: the compiled test
 * @cur: the current node tested
 * @success: boolean value for the result
 *
 * called from the validation engine when an assert or report test have
 * been done.
 */
unsafe extern "C" fn xml_schematron_report_success(
    ctxt: XmlSchematronValidCtxtPtr,
    test: XmlSchematronTestPtr,
    cur: XmlNodePtr,
    pattern: XmlSchematronPatternPtr,
    success: c_int,
) {
    if ctxt.is_null() || cur.is_null() || test.is_null() {
        return;
    }
    /* if quiet and not SVRL report only failures */
    if (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutQuiet as i32 != 0
        && (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutXml as i32 == 0
        && (*test).typ == XmlSchematronTestType::XmlSchematronReport
    {
        return;
    }
    if (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutXml as i32 != 0 {
        // TODO
    } else {
        let mut path: *mut XmlChar;
        let mut msg: [c_char; 1000] = [0; 1000];

        let mut report: *const XmlChar = null_mut();

        if ((*test).typ == XmlSchematronTestType::XmlSchematronReport && success == 0)
            || ((*test).typ == XmlSchematronTestType::XmlSchematronAssert && success != 0)
        {
            return;
        }
        let line: c_long = xml_get_line_no(cur);
        path = xml_get_node_path(cur);
        if path.is_null() {
            path = (*cur).name as _;
        }
        // #if 0
        //         if (((*test).report != null_mut()) && ((*test).report[0] != 0))
        //             report = (*test).report;
        // #endif
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
        snprintf(
            msg.as_mut_ptr() as _,
            999,
            c"%s line %ld: %s\n".as_ptr() as _,
            path,
            line,
            report,
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
                null_mut(),
                line as _,
                if pattern.is_null() {
                    null_mut()
                } else {
                    (*pattern).name as _
                },
                path as _,
                (!report.is_null()).then(|| CStr::from_ptr(report as *const i8)
                    .to_string_lossy()
                    .into_owned()
                    .into()),
                0,
                0,
                c"%s".as_ptr() as _,
                msg
            );
        } else {
            xml_schematron_report_output(ctxt, cur, addr_of!(msg[0]));
        }

        xml_free(report as _);

        if !path.is_null() && path != (*cur).name as _ {
            xml_free(path as _);
        }
    }
}

/**
 * xmlSchematronRunTest:
 * @ctxt:  the schema validation context
 * @test:  the current test
 * @instance:  the document instance tree
 * @cur:  the current node in the instance
 *
 * Validate a rule against a tree instance at a given position
 *
 * Returns 1 in case of success, 0 if error and -1 in case of internal error
 */
unsafe extern "C" fn xml_schematron_run_test(
    ctxt: XmlSchematronValidCtxtPtr,
    test: XmlSchematronTestPtr,
    instance: XmlDocPtr,
    cur: XmlNodePtr,
    pattern: XmlSchematronPatternPtr,
) -> c_int {
    let mut failed: c_int;

    failed = 0;
    (*(*ctxt).xctxt).doc = instance;
    (*(*ctxt).xctxt).node = cur;
    let ret: XmlXPathObjectPtr = xml_xpath_compiled_eval((*test).comp, (*ctxt).xctxt);
    if ret.is_null() {
        failed = 1;
    } else {
        match (*ret).typ {
            XmlXPathObjectType::XpathXsltTree | XmlXPathObjectType::XpathNodeset => {
                if (*ret).nodesetval.is_null() || (*(*ret).nodesetval).node_nr == 0 {
                    failed = 1;
                }
            }
            XmlXPathObjectType::XpathBoolean => {
                failed = !(*ret).boolval;
            }
            XmlXPathObjectType::XpathNumber => {
                if xml_xpath_is_nan((*ret).floatval) != 0 || (*ret).floatval == 0.0 {
                    failed = 1;
                }
            }
            XmlXPathObjectType::XpathString => {
                if (*ret).stringval.is_null() || *(*ret).stringval.add(0) == 0 {
                    failed = 1;
                }
            }
            XmlXPathObjectType::XpathUndefined | XmlXPathObjectType::XpathUsers => {
                failed = 1;
            }
            #[cfg(feature = "libxml_xptr_locs")]
            XmlXPathObjectType::XpathPoint
            | XmlXPathObjectType::XpathRange
            | XmlXPathObjectType::XpathLocationset => {
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

/**
 * xmlSchematronUnregisterVariables:
 * @ctxt:  the schema validation context
 * @let:  the list of let variables
 *
 * Unregisters a list of let variables from the context
 *
 * Returns -1 in case of errors, otherwise 0
 */
unsafe extern "C" fn xml_schematron_unregister_variables(
    ctxt: XmlXPathContextPtr,
    mut letr: XmlSchematronLetPtr,
) -> c_int {
    while !letr.is_null() {
        if xml_xpath_register_variable_ns(ctxt, (*letr).name, null_mut(), null_mut()) != 0 {
            generic_error!("Unregistering a let variable failed\n");
            return -1;
        }
        letr = (*letr).next;
    }
    0
}

unsafe extern "C" fn xml_schematron_next_node(mut cur: XmlNodePtr) -> XmlNodePtr {
    if !(*cur).children.is_null() {
        /*
         * Do not descend on entities declarations
         */
        if (*(*cur).children).typ != XmlElementType::XmlEntityDecl {
            cur = (*cur).children;
            /*
             * Skip DTDs
             */
            if (*cur).typ != XmlElementType::XmlDtdNode {
                return cur;
            }
        }
    }

    while !(*cur).next.is_null() {
        cur = (*cur).next;
        if (*cur).typ != XmlElementType::XmlEntityDecl && (*cur).typ != XmlElementType::XmlDtdNode {
            return cur;
        }
    }

    loop {
        cur = (*cur).parent;
        if cur.is_null() {
            break;
        }
        if (*cur).typ == XmlElementType::XmlDocumentNode {
            return null_mut();
        }
        if !(*cur).next.is_null() {
            cur = (*cur).next;
            return cur;
        }

        if cur.is_null() {
            break;
        }
    }
    cur
}

/**
 * xmlSchematronReportPattern:
 * @ctxt:  the validation context
 * @pattern: the current pattern
 *
 * called from the validation engine when starting to check a pattern
 */
unsafe extern "C" fn xml_schematron_report_pattern(
    ctxt: XmlSchematronValidCtxtPtr,
    pattern: XmlSchematronPatternPtr,
) {
    if ctxt.is_null() || pattern.is_null() {
        return;
    }
    if (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutQuiet as i32 != 0
        || (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutError as i32 != 0
    /* Error gives pattern name as part of error */
    {
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

/**
 * xmlSchematronValidateDoc:
 * @ctxt:  the schema validation context
 * @instance:  the document instance tree
 *
 * Validate a tree instance against the schematron
 *
 * Returns 0 in case of success, -1 in case of internal error
 *         and an error count otherwise.
 */
pub unsafe extern "C" fn xml_schematron_validate_doc(
    ctxt: XmlSchematronValidCtxtPtr,
    instance: XmlDocPtr,
) -> c_int {
    let mut cur: XmlNodePtr;
    let mut pattern: XmlSchematronPatternPtr;
    let mut rule: XmlSchematronRulePtr;
    let mut test: XmlSchematronTestPtr;

    if ctxt.is_null()
        || (*ctxt).schema.is_null()
        || (*(*ctxt).schema).rules.is_null()
        || instance.is_null()
    {
        return -1;
    }
    (*ctxt).nberrors = 0;
    let root: XmlNodePtr = xml_doc_get_root_element(instance);
    if root.is_null() {
        // TODO
        (*ctxt).nberrors += 1;
        return 1;
    }
    if (*ctxt).flags & XmlSchematronValidOptions::XmlSchematronOutQuiet as i32 != 0
        || (*ctxt).flags == 0
    {
        /*
         * we are just trying to assert the validity of the document,
         * speed primes over the output, run in a single pass
         */
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
        /*
         * Process all contexts one at a time
         */
        pattern = (*(*ctxt).schema).patterns;

        while !pattern.is_null() {
            xml_schematron_report_pattern(ctxt, pattern);

            /*
             * TODO convert the pattern rule to a direct XPath and
             * compute directly instead of using the pattern matching
             * over the full document...
             * Check the exact semantic
             */
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

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_schematron_new_doc_parser_ctxt() {

        /* missing type support */
    }

    #[test]
    fn test_xml_schematron_new_mem_parser_ctxt() {

        /* missing type support */
    }

    #[test]
    fn test_xml_schematron_new_parser_ctxt() {

        /* missing type support */
    }

    #[test]
    fn test_xml_schematron_new_valid_ctxt() {

        /* missing type support */
    }

    #[test]
    fn test_xml_schematron_parse() {

        /* missing type support */
    }

    #[test]
    fn test_xml_schematron_set_valid_structured_errors() {

        /* missing type support */
    }

    #[test]
    fn test_xml_schematron_validate_doc() {
        #[cfg(feature = "libxml_schematron")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SCHEMATRON_VALID_CTXT_PTR {
                for n_instance in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_schematron_valid_ctxt_ptr(n_ctxt, 0);
                    let instance = gen_xml_doc_ptr(n_instance, 1);

                    let ret_val = xml_schematron_validate_doc(ctxt, instance);
                    desret_int(ret_val);
                    des_xml_schematron_valid_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_doc_ptr(n_instance, instance, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSchematronValidateDoc",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlSchematronValidateDoc()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_instance);
                    }
                }
            }
        }
    }
}

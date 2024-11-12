//! Provide methods and data structures for handling pattern expression.  
//! This module is based on `libxml/pattern.h`, `pattern.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
};

use libc::memset;

use crate::{
    libxml::{
        chvalid::{xml_is_blank_char, xml_is_combining, xml_is_digit, xml_is_extender},
        dict::{xml_dict_free, xml_dict_lookup, xml_dict_reference, XmlDict, XmlDictPtr},
        globals::{xml_free, xml_malloc, xml_realloc},
        parser_internals::{xml_is_letter, xml_string_current_char},
        xmlstring::{xml_str_equal, xml_strdup, xml_strndup, XmlChar},
    },
    tree::{XmlElementType, XmlNodePtr, XML_XML_NAMESPACE},
};

const XML_STREAM_STEP_DESC: usize = 1;
const XML_STREAM_STEP_FINAL: usize = 2;
const XML_STREAM_STEP_ROOT: usize = 4;
const XML_STREAM_STEP_ATTR: usize = 8;
const XML_STREAM_STEP_NODE: usize = 16;
const XML_STREAM_STEP_IN_SET: usize = 32;

/*
* NOTE: Those private flags (XML_STREAM_xxx) are used
*   in xmlStreamCtxt->flag. They extend the public
*   XmlPatternFlags, so be careful not to interfere with the
*   reserved values for XmlPatternFlags.
*/
const XML_STREAM_FINAL_IS_ANY_NODE: usize = 1 << 14;
const XML_STREAM_FROM_ROOT: usize = 1 << 15;
const XML_STREAM_DESC: usize = 1 << 16;

/*
 * Types are private:
 */
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
    dict: *mut XmlDict,      /* the dictionary if any */
    nb_step: i32,            /* number of steps in the automata */
    max_step: i32,           /* allocated number of steps */
    steps: XmlStreamStepPtr, /* the array of steps */
    flags: i32,
}

/**
 * xmlPattern:
 *
 * A compiled (XPath based) pattern to select nodes
 */
pub type XmlPatternPtr = *mut XmlPattern;
#[repr(C)]
pub struct XmlPattern {
    data: *mut c_void,       /* the associated template */
    dict: XmlDictPtr,        /* the optional dictionary */
    next: *mut XmlPattern,   /* next pattern if | is used */
    pattern: *const XmlChar, /* the pattern */
    flags: i32,              /* flags */
    nb_step: i32,
    max_step: i32,
    steps: XmlStepOpPtr,      /* ops for computation */
    stream: XmlStreamCompPtr, /* the streaming data if any */
}

/*
* XML_STREAM_ANY_NODE is used for comparison against
* xmlElementType enums, to indicate a node of any type.
*/
const XML_STREAM_ANY_NODE: usize = 100;

/**
 * XmlPatternFlags:
 *
 * This is the set of options affecting the behaviour of pattern
 * matching with this module
 *
 */
#[repr(C)]
pub enum XmlPatternFlags {
    XmlPatternDefault = 0,      /* simple pattern match */
    XmlPatternXpath = 1 << 0,   /* standard XPath pattern */
    XmlPatternXssel = 1 << 1,   /* XPath subset for schema selector */
    XmlPatternXsfield = 1 << 2, /* XPath subset for schema field */
}

/**
 * xmlFreePattern:
 * @comp:  an XSLT comp
 *
 * Free up the memory allocated by @comp
 */
pub unsafe extern "C" fn xml_free_pattern(comp: XmlPatternPtr) {
    xml_free_pattern_list(comp);
}

/**
 * xmlFreeStreamComp:
 * @comp: the compiled pattern for streaming
 *
 * Free the compiled pattern for streaming
 */
unsafe extern "C" fn xml_free_stream_comp(comp: XmlStreamCompPtr) {
    if !comp.is_null() {
        if !(*comp).steps.is_null() {
            xml_free((*comp).steps as _);
        }
        if !(*comp).dict.is_null() {
            xml_dict_free((*comp).dict);
        }
        xml_free(comp as _);
    }
}

unsafe extern "C" fn xml_free_pattern_internal(comp: XmlPatternPtr) {
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
        if (*comp).dict.is_null() {
            for i in 0..(*comp).nb_step {
                op = (*comp).steps.add(i as usize);
                if !(*op).value.is_null() {
                    xml_free((*op).value as _);
                }
                if !(*op).value2.is_null() {
                    xml_free((*op).value2 as _);
                }
            }
        }
        xml_free((*comp).steps as _);
    }
    if !(*comp).dict.is_null() {
        xml_dict_free((*comp).dict);
    }

    memset(comp as _, -1, size_of::<XmlPattern>());
    xml_free(comp as _);
}

/**
 * xmlFreePatternList:
 * @comp:  an XSLT comp list
 *
 * Free up the memory allocated by all the elements of @comp
 */
pub unsafe extern "C" fn xml_free_pattern_list(mut comp: XmlPatternPtr) {
    let mut cur: XmlPatternPtr;

    while !comp.is_null() {
        cur = comp;
        comp = (*comp).next;
        (*cur).next = null_mut();
        xml_free_pattern_internal(cur);
    }
}

pub type XmlPatParserContextPtr = *mut XmlPatParserContext;
#[repr(C)]
pub struct XmlPatParserContext {
    cur: *const XmlChar,             /* the current char being parsed */
    base: *const XmlChar,            /* the full expression */
    error: i32,                      /* error code */
    dict: XmlDictPtr,                /* the dictionary if any */
    comp: XmlPatternPtr,             /* the result */
    elem: XmlNodePtr,                /* the current node if any */
    namespaces: *mut *const XmlChar, /* the namespaces definitions */
    nb_namespaces: i32,              /* the number of namespaces */
}

/**
 * xmlNewPatParserContext:
 * @pattern:  the pattern context
 * @dict:  the inherited dictionary or NULL
 * @namespaces: the prefix definitions, array of [URI, prefix] terminated
 *              with [NULL, NULL] or NULL if no namespace is used
 *
 * Create a new XML pattern parser context
 *
 * Returns the newly allocated xmlPatParserContextPtr or NULL in case of error
 */
unsafe extern "C" fn xml_new_pat_parser_context(
    pattern: *const XmlChar,
    dict: XmlDictPtr,
    namespaces: *mut *const XmlChar,
) -> XmlPatParserContextPtr {
    if pattern.is_null() {
        return null_mut();
    }

    let cur: XmlPatParserContextPtr =
        xml_malloc(size_of::<XmlPatParserContext>()) as XmlPatParserContextPtr;
    if cur.is_null() {
        // ERROR(NULL, NULL, NULL,
        // 	"xmlNewPatParserContext : malloc failed\n");
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlPatParserContext>());
    (*cur).dict = dict;
    (*cur).cur = pattern;
    (*cur).base = pattern;
    if !namespaces.is_null() {
        let mut i: i32 = 0;
        while !(*namespaces.add(2 * i as usize)).is_null() {
            i += 1;
        }
        (*cur).nb_namespaces = i;
    } else {
        (*cur).nb_namespaces = 0;
    }
    (*cur).namespaces = namespaces;
    cur
}

/**
 * xmlFreePatParserContext:
 * @ctxt:  an XSLT parser context
 *
 * Free up the memory allocated by @ctxt
 */
unsafe extern "C" fn xml_free_pat_parser_context(ctxt: XmlPatParserContextPtr) {
    if ctxt.is_null() {
        return;
    }
    memset(ctxt as _, -1, size_of::<XmlPatParserContext>());
    xml_free(ctxt as _);
}

/**
 * xmlNewPattern:
 *
 * Create a new XSLT Pattern
 *
 * Returns the newly allocated xmlPatternPtr or NULL in case of error
 */
unsafe extern "C" fn xml_new_pattern() -> XmlPatternPtr {
    let cur: XmlPatternPtr = xml_malloc(size_of::<XmlPattern>()) as XmlPatternPtr;
    if cur.is_null() {
        // ERROR(NULL, NULL, NULL,
        // 	"xmlNewPattern : malloc failed\n");
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlPattern>());
    (*cur).max_step = 10;
    (*cur).steps = xml_malloc((*cur).max_step as usize * size_of::<XmlStepOp>()) as XmlStepOpPtr;
    if (*cur).steps.is_null() {
        xml_free(cur as _);
        // ERROR(NULL, NULL, NULL,
        // 	"xmlNewPattern : malloc failed\n");
        return null_mut();
    }
    cur
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

const PAT_FROM_ROOT: usize = 1 << 8;
const PAT_FROM_CUR: usize = 1 << 9;

/**
 * xmlPatternAdd:
 * @comp:  the compiled is_match expression
 * @op:  an op
 * @value:  the first value
 * @value2:  the second value
 *
 * Add a step to an XSLT Compiled Match
 *
 * Returns -1 in case of failure, 0 otherwise.
 */
unsafe extern "C" fn xml_pattern_add(
    _ctxt: XmlPatParserContextPtr,
    comp: XmlPatternPtr,
    op: XmlPatOp,
    value: *mut XmlChar,
    value2: *mut XmlChar,
) -> i32 {
    if (*comp).nb_step >= (*comp).max_step {
        let temp: XmlStepOpPtr = xml_realloc(
            (*comp).steps as _,
            (*comp).max_step as usize * 2 * size_of::<XmlStepOp>(),
        ) as XmlStepOpPtr;
        if temp.is_null() {
            // ERROR(ctxt, NULL, NULL,
            // 	     "xmlPatternAdd: realloc failed\n");
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

macro_rules! XML_PAT_FREE_STRING {
    ($c:expr, $r:expr) => {
        if (*(*$c).comp).dict.is_null() {
            xml_free($r as _);
        }
    };
}

macro_rules! XML_PAT_COPY_NSNAME {
    ($c:expr, $r:expr, $nsname:expr) => {
        if !(*(*$c).comp).dict.is_null() {
            $r = xml_dict_lookup((*(*$c).comp).dict, $nsname, -1) as *mut XmlChar;
        } else {
            $r = xml_strdup($nsname);
        }
    };
}

macro_rules! CUR_PTR {
    ($ctxt:expr) => {
        (*$ctxt).cur
    };
}

/**
 * xmlPatScanNCName:
 * @ctxt:  the XPath Parser context
 *
 * Parses a non qualified name
 *
 * Returns the Name parsed or NULL
 */

unsafe extern "C" fn xml_pat_scan_ncname(ctxt: XmlPatParserContextPtr) -> *mut XmlChar {
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
    let ret = if !(*ctxt).dict.is_null() {
        xml_dict_lookup((*ctxt).dict, q, cur.offset_from(q) as _) as *mut XmlChar
    } else {
        xml_strndup(q, cur.offset_from(q) as _)
    };
    CUR_PTR!(ctxt) = cur;
    ret
}

/**
 * xmlPatScanName:
 * @ctxt:  the XPath Parser context
 *
 * [4] NameChar ::= Letter | Digit | '.' | '-' | '_' |
 *                  CombiningChar | Extender
 *
 * [5] Name ::= (Letter | '_' | ':') (NameChar)*
 *
 * [6] Names ::= Name (S Name)*
 *
 * Returns the Name parsed or NULL
 */

unsafe extern "C" fn xml_pat_scan_name(ctxt: XmlPatParserContextPtr) -> *mut XmlChar {
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
    let ret = if !(*ctxt).dict.is_null() {
        xml_dict_lookup((*ctxt).dict, q, cur.offset_from(q) as _) as *mut XmlChar
    } else {
        xml_strndup(q, cur.offset_from(q) as _)
    };
    CUR_PTR!(ctxt) = cur;
    ret
}

/**
 * xmlCompileAttributeTest:
 * @ctxt:  the compilation context
 *
 * Compile an attribute test.
 */
unsafe extern "C" fn xml_compile_attribute_test(ctxt: XmlPatParserContextPtr) {
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
                // ERROR(NULL, NULL, NULL,
                // "xmlCompileAttributeTest : Name expected\n");
                (*ctxt).error = 1;
            }
            return;
        }
        if CUR!(ctxt) == b':' {
            let mut i: i32;
            let prefix: *mut XmlChar = name;

            NEXT!(ctxt);

            if xml_is_blank_char(CUR!(ctxt) as u32) {
                // ERROR5(NULL, NULL, NULL, "Invalid QName.\n", NULL);
                XML_PAT_FREE_STRING!(ctxt, prefix);
                (*ctxt).error = 1;
                break 'error;
            }
            /*
             * This is a namespace is_match
             */
            token = xml_pat_scan_name(ctxt);
            if *prefix.add(0) == b'x'
                && *prefix.add(1) == b'm'
                && *prefix.add(2) == b'l'
                && *prefix.add(3) == 0
            {
                XML_PAT_COPY_NSNAME!(ctxt, url, XML_XML_NAMESPACE.as_ptr() as _);
            } else {
                i = 0;
                while i < (*ctxt).nb_namespaces {
                    if xml_str_equal(*(*ctxt).namespaces.add(2 * i as usize + 1), prefix) {
                        XML_PAT_COPY_NSNAME!(ctxt, url, *(*ctxt).namespaces.add(2 * i as usize));
                        break;
                    }
                    i += 1;
                }
                if i >= (*ctxt).nb_namespaces {
                    // ERROR5(NULL, NULL, NULL,
                    //     "xmlCompileAttributeTest : no namespace bound to prefix %s\n",
                    //     prefix);
                    XML_PAT_FREE_STRING!(ctxt, prefix);
                    (*ctxt).error = 1;
                    break 'error;
                }
            }
            XML_PAT_FREE_STRING!(ctxt, prefix);
            if token.is_null() {
                if CUR!(ctxt) == b'*' {
                    NEXT!(ctxt);
                    PUSH!(ctxt, XmlPatOp::XmlOpAttr, null_mut(), url, 'error);
                } else {
                    // ERROR(NULL, NULL, NULL,
                    //     "xmlCompileAttributeTest : Name expected\n");
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
        XML_PAT_FREE_STRING!(ctxt, url)
    }
    if !token.is_null() {
        XML_PAT_FREE_STRING!(ctxt, token);
    }
}

/**
 * xmlCompileStepPattern:
 * @ctxt:  the compilation context
 *
 * Compile the Step Pattern and generates a precompiled
 * form suitable for fast matching.
 *
 * [3]    Step    ::=    '.' | NameTest
 * [4]    NameTest    ::=    QName | '*' | NCName ':' '*'
 */

unsafe extern "C" fn xml_compile_step_pattern(ctxt: XmlPatParserContextPtr) {
    let mut token: *mut XmlChar = null_mut();
    let mut name: *mut XmlChar = null_mut();
    let mut url: *mut XmlChar = null_mut();
    let mut has_blanks: i32 = 0;

    SKIP_BLANKS!(ctxt);
    'error: {
        if CUR!(ctxt) == b'.' {
            /*
             * Context node.
             */
            NEXT!(ctxt);
            PUSH!(ctxt, XmlPatOp::XmlOpElem, null_mut(), null_mut(), 'error);
            return;
        }
        if CUR!(ctxt) == b'@' {
            /*
             * Attribute test.
             */
            if XML_STREAM_XS_IDC_SEL!((*ctxt).comp) {
                //  ERROR5(NULL, NULL, NULL,
                //  "Unexpected attribute axis in '%s'.\n", (*ctxt).base);
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
                //  ERROR(NULL, NULL, NULL,
                //      "xmlCompileStepPattern : Name expected\n");
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
                let mut i: i32;

                if has_blanks != 0 || xml_is_blank_char(CUR!(ctxt) as u32) {
                    //  ERROR5(NULL, NULL, NULL, "Invalid QName.\n", NULL);
                    (*ctxt).error = 1;
                    break 'error;
                }
                /*
                 * This is a namespace is_match
                 */
                token = xml_pat_scan_name(ctxt);
                if *prefix.add(0) == b'x'
                    && *prefix.add(1) == b'm'
                    && *prefix.add(2) == b'l'
                    && *prefix.add(3) == 0
                {
                    XML_PAT_COPY_NSNAME!(ctxt, url, XML_XML_NAMESPACE.as_ptr() as _);
                } else {
                    i = 0;
                    while i < (*ctxt).nb_namespaces {
                        if xml_str_equal(*(*ctxt).namespaces.add(2 * i as usize + 1), prefix) {
                            XML_PAT_COPY_NSNAME!(
                                ctxt,
                                url,
                                *(*ctxt).namespaces.add(2 * i as usize)
                            );
                            break;
                        }
                        i += 1;
                    }
                    if i >= (*ctxt).nb_namespaces {
                        //  ERROR5(NULL, NULL, NULL,
                        //  "xmlCompileStepPattern : no namespace bound to prefix %s\n",
                        //  prefix);
                        (*ctxt).error = 1;
                        break 'error;
                    }
                }
                XML_PAT_FREE_STRING!(ctxt, prefix);
                name = null_mut();
                if token.is_null() {
                    if CUR!(ctxt) == b'*' {
                        NEXT!(ctxt);
                        PUSH!(ctxt, XmlPatOp::XmlOpNs, url, null_mut(), 'error);
                    } else {
                        //  ERROR(NULL, NULL, NULL,
                        //      "xmlCompileStepPattern : Name expected\n");
                        (*ctxt).error = 1;
                        break 'error;
                    }
                } else {
                    PUSH!(ctxt, XmlPatOp::XmlOpElem, token, url, 'error);
                }
            } else {
                NEXT!(ctxt);
                if xml_str_equal(name, c"child".as_ptr() as _) {
                    XML_PAT_FREE_STRING!(ctxt, name);
                    name = xml_pat_scan_name(ctxt);
                    if name.is_null() {
                        if CUR!(ctxt) == b'*' {
                            NEXT!(ctxt);
                            PUSH!(ctxt, XmlPatOp::XmlOpAll, null_mut(), null_mut(), 'error);
                            return;
                        } else {
                            //  ERROR(NULL, NULL, NULL,
                            //      "xmlCompileStepPattern : QName expected\n");
                            (*ctxt).error = 1;
                            break 'error;
                        }
                    }
                    if CUR!(ctxt) == b':' {
                        let prefix: *mut XmlChar = name;
                        let mut i: i32;

                        NEXT!(ctxt);
                        if xml_is_blank_char(CUR!(ctxt) as u32) {
                            //  ERROR5(NULL, NULL, NULL, "Invalid QName.\n", NULL);
                            (*ctxt).error = 1;
                            break 'error;
                        }
                        /*
                         * This is a namespace is_match
                         */
                        token = xml_pat_scan_name(ctxt);
                        if *prefix.add(0) == b'x'
                            && *prefix.add(1) == b'm'
                            && *prefix.add(2) == b'l'
                            && *prefix.add(3) == 0
                        {
                            XML_PAT_COPY_NSNAME!(ctxt, url, XML_XML_NAMESPACE.as_ptr() as _);
                        } else {
                            i = 0;
                            while i < (*ctxt).nb_namespaces {
                                if xml_str_equal(
                                    *(*ctxt).namespaces.add(2 * i as usize + 1),
                                    prefix,
                                ) {
                                    XML_PAT_COPY_NSNAME!(
                                        ctxt,
                                        url,
                                        *(*ctxt).namespaces.add(2 * i as usize)
                                    );
                                    break;
                                }
                                i += 1;
                            }
                            if i >= (*ctxt).nb_namespaces {
                                //  ERROR5(NULL, NULL, NULL,
                                //  "xmlCompileStepPattern : no namespace bound "
                                //  "to prefix %s\n", prefix);
                                (*ctxt).error = 1;
                                break 'error;
                            }
                        }
                        XML_PAT_FREE_STRING!(ctxt, prefix);
                        name = null_mut();
                        if token.is_null() {
                            if CUR!(ctxt) == b'*' {
                                NEXT!(ctxt);
                                PUSH!(ctxt, XmlPatOp::XmlOpNs, url, null_mut(), 'error);
                            } else {
                                //  ERROR(NULL, NULL, NULL,
                                //  "xmlCompileStepPattern : Name expected\n");
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
                    XML_PAT_FREE_STRING!(ctxt, name);
                    name = null_mut();
                    if XML_STREAM_XS_IDC_SEL!((*ctxt).comp) {
                        //  ERROR5(NULL, NULL, NULL,
                        //  "Unexpected attribute axis in '%s'.\n", (*ctxt).base);
                        (*ctxt).error = 1;
                        break 'error;
                    }
                    xml_compile_attribute_test(ctxt);
                    if (*ctxt).error != 0 {
                        break 'error;
                    }
                    return;
                } else {
                    //  ERROR5(NULL, NULL, NULL,
                    //      "The 'element' or 'attribute' axis is expected.\n", NULL);
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
        XML_PAT_FREE_STRING!(ctxt, url)
    }
    if !token.is_null() {
        XML_PAT_FREE_STRING!(ctxt, token)
    }
    if !name.is_null() {
        XML_PAT_FREE_STRING!(ctxt, name)
    }
}

/**
 * xmlCompileIDCXPathPath:
 * @ctxt:  the compilation context
 *
 * Compile the Path Pattern and generates a precompiled
 * form suitable for fast matching.
 *
 * [5]    Path    ::=    ('.//')? ( Step '/' )* ( Step | '@' NameTest )
 */
unsafe extern "C" fn xml_compile_idc_xpath_path(ctxt: XmlPatParserContextPtr) {
    SKIP_BLANKS!(ctxt);
    'error_unfinished: {
        'error: {
            if CUR!(ctxt) == b'/' {
                // ERROR5(NULL, NULL, NULL,
                //     "Unexpected selection of the document root in '%s'.\n",
                //     (*ctxt).base);
                break 'error;
            }
            (*(*ctxt).comp).flags |= PAT_FROM_CUR as i32;

            if CUR!(ctxt) == b'.' {
                /* "." - "self::node()" */
                NEXT!(ctxt);
                SKIP_BLANKS!(ctxt);
                if CUR!(ctxt) == 0 {
                    /*
                     * Selection of the context node.
                     */
                    PUSH!(ctxt, XmlPatOp::XmlOpElem, null_mut(), null_mut(), 'error);
                    return;
                }
                if CUR!(ctxt) != b'/' {
                    /* TODO: A more meaningful error message. */
                    // ERROR5(NULL, NULL, NULL,
                    // "Unexpected token after '.' in '%s'.\n", (*ctxt).base);
                    break 'error;
                }
                /* "./" - "self::node()/" */
                NEXT!(ctxt);
                SKIP_BLANKS!(ctxt);
                if CUR!(ctxt) == b'/' {
                    if xml_is_blank_char(PEEKPREV!(ctxt, 1) as u32) {
                        /*
                         * Disallow "./ /"
                         */
                        // ERROR5(NULL, NULL, NULL,
                        //     "Unexpected '/' token in '%s'.\n", (*ctxt).base);
                        break 'error;
                    }
                    /* ".//" - "self:node()/descendant-or-self::node()/" */
                    PUSH!(ctxt, XmlPatOp::XmlOpAncestor, null_mut(), null_mut(), 'error);
                    NEXT!(ctxt);
                    SKIP_BLANKS!(ctxt);
                }
                if CUR!(ctxt) == 0 {
                    break 'error_unfinished;
                }
            }
            /*
             * Process steps.
             */
            'b: while {
                xml_compile_step_pattern(ctxt);
                if (*ctxt).error != 0 {
                    break 'error;
                }
                SKIP_BLANKS!(ctxt);
                if CUR!(ctxt) != b'/' {
                    break 'b;
                }
                PUSH!(ctxt, XmlPatOp::XmlOpParent, null_mut(), null_mut(), 'error);
                NEXT!(ctxt);
                SKIP_BLANKS!(ctxt);
                if CUR!(ctxt) == b'/' {
                    /*
                     * Disallow subsequent '//'.
                     */
                    // ERROR5(NULL, NULL, NULL,
                    // "Unexpected subsequent '//' in '%s'.\n",
                    // (*ctxt).base);
                    break 'error;
                }
                if CUR!(ctxt) == 0 {
                    break 'error_unfinished;
                }

                CUR!(ctxt) != 0
            } {}

            if CUR!(ctxt) != 0 {
                // ERROR5(NULL, NULL, NULL,
                //     "Failed to compile expression '%s'.\n", (*ctxt).base);
                (*ctxt).error = 1;
            }
            return;
        }
        // error:
        (*ctxt).error = 1;
        return;
    }

    // error_unfinished:
    (*ctxt).error = 1;
    //     // ERROR5(NULL, NULL, NULL,
    // 	// "Unfinished expression '%s'.\n", (*ctxt).base);
    //     return;
}

const XML_PATTERN_NOTPATTERN: i32 = XmlPatternFlags::XmlPatternXpath as i32
    | XmlPatternFlags::XmlPatternXssel as i32
    | XmlPatternFlags::XmlPatternXsfield as i32;

macro_rules! NXT {
    ($ctxt:expr, $val:expr) => {
        *(*$ctxt).cur.add($val as usize)
    };
}

/**
 * xmlCompilePathPattern:
 * @ctxt:  the compilation context
 *
 * Compile the Path Pattern and generates a precompiled
 * form suitable for fast matching.
 *
 * [5]    Path    ::=    ('.//')? ( Step '/' )* ( Step | '@' NameTest )
 */
unsafe extern "C" fn xml_compile_path_pattern(ctxt: XmlPatParserContextPtr) {
    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) == b'/' {
        (*(*ctxt).comp).flags |= PAT_FROM_ROOT as i32;
    } else if CUR!(ctxt) == b'.' || (*(*ctxt).comp).flags & XML_PATTERN_NOTPATTERN != 0 {
        (*(*ctxt).comp).flags |= PAT_FROM_CUR as i32;
    }

    'error: {
        if CUR!(ctxt) == b'/' && NXT!(ctxt, 1) == b'/' {
            PUSH!(ctxt, XmlPatOp::XmlOpAncestor, null_mut(), null_mut(), 'error);
            NEXT!(ctxt);
            NEXT!(ctxt);
        } else if CUR!(ctxt) == b'.' && NXT!(ctxt, 1) == b'/' && NXT!(ctxt, 2) == b'/' {
            PUSH!(ctxt, XmlPatOp::XmlOpAncestor, null_mut(), null_mut(), 'error);
            NEXT!(ctxt);
            NEXT!(ctxt);
            NEXT!(ctxt);
            /* Check for incompleteness. */
            SKIP_BLANKS!(ctxt);
            if CUR!(ctxt) == 0 {
                // ERROR5(NULL, NULL, NULL,
                //    "Incomplete expression '%s'.\n", (*ctxt).base);
                (*ctxt).error = 1;
                break 'error;
            }
        }
        if CUR!(ctxt) == b'@' {
            NEXT!(ctxt);
            xml_compile_attribute_test(ctxt);
            SKIP_BLANKS!(ctxt);
            /* TODO: check for incompleteness */
            if CUR!(ctxt) != 0 {
                xml_compile_step_pattern(ctxt);
                if (*ctxt).error != 0 {
                    break 'error;
                }
            }
        } else {
            if CUR!(ctxt) == b'/' {
                PUSH!(ctxt, XmlPatOp::XmlOpRoot, null_mut(), null_mut(), 'error);
                NEXT!(ctxt);
                /* Check for incompleteness. */
                SKIP_BLANKS!(ctxt);
                if CUR!(ctxt) == 0 {
                    // ERROR5(NULL, NULL, NULL,
                    //     "Incomplete expression '%s'.\n", (*ctxt).base);
                    (*ctxt).error = 1;
                    break 'error;
                }
            }
            xml_compile_step_pattern(ctxt);
            if (*ctxt).error != 0 {
                break 'error;
            }
            SKIP_BLANKS!(ctxt);
            while CUR!(ctxt) == b'/' {
                if NXT!(ctxt, 1) == b'/' {
                    PUSH!(ctxt, XmlPatOp::XmlOpAncestor, null_mut(), null_mut(), 'error);
                    NEXT!(ctxt);
                    NEXT!(ctxt);
                    SKIP_BLANKS!(ctxt);
                    xml_compile_step_pattern(ctxt);
                    if (*ctxt).error != 0 {
                        break 'error;
                    }
                } else {
                    PUSH!(ctxt, XmlPatOp::XmlOpParent, null_mut(), null_mut(), 'error);
                    NEXT!(ctxt);
                    SKIP_BLANKS!(ctxt);
                    if CUR!(ctxt) == 0 {
                        // ERROR5(NULL, NULL, NULL,
                        // "Incomplete expression '%s'.\n", (*ctxt).base);
                        (*ctxt).error = 1;
                        break 'error;
                    }
                    xml_compile_step_pattern(ctxt);
                    if (*ctxt).error != 0 {
                        break 'error;
                    }
                }
            }
        }
        if CUR!(ctxt) != 0 {
            // ERROR5(NULL, NULL, NULL,
            //        "Failed to compile pattern %s\n", (*ctxt).base);
            (*ctxt).error = 1;
        }
    }
    // error:
    // return;
}

/**
 * xmlNewStreamComp:
 * @size: the number of expected steps
 *
 * build a new compiled pattern for streaming
 *
 * Returns the new structure or NULL in case of error.
 */
unsafe extern "C" fn xml_new_stream_comp(mut size: i32) -> XmlStreamCompPtr {
    if size < 4 {
        size = 4;
    }

    let cur: XmlStreamCompPtr = xml_malloc(size_of::<XmlStreamComp>()) as XmlStreamCompPtr;
    if cur.is_null() {
        // ERROR(NULL, NULL, NULL,
        // 	"xmlNewStreamComp: malloc failed\n");
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlStreamComp>());
    (*cur).steps = xml_malloc(size as usize * size_of::<XmlStreamStep>()) as XmlStreamStepPtr;
    if (*cur).steps.is_null() {
        xml_free(cur as _);
        // ERROR(NULL, NULL, NULL,
        //       "xmlNewStreamComp: malloc failed\n");
        return null_mut();
    }
    (*cur).nb_step = 0;
    (*cur).max_step = size;
    cur
}

/**
 * xmlStreamCompAddStep:
 * @comp: the compiled pattern for streaming
 * @name: the first string, the name, or NULL for *
 * @ns: the second step, the namespace name
 * @flags: the flags for that step
 *
 * Add a new step to the compiled pattern
 *
 * Returns -1 in case of error or the step index if successful
 */
unsafe extern "C" fn xml_stream_comp_add_step(
    comp: XmlStreamCompPtr,
    name: *const XmlChar,
    ns: *const XmlChar,
    node_type: i32,
    flags: i32,
) -> i32 {
    let mut cur: XmlStreamStepPtr;

    if (*comp).nb_step >= (*comp).max_step {
        cur = xml_realloc(
            (*comp).steps as _,
            (*comp).max_step as usize * 2 * size_of::<XmlStreamStep>(),
        ) as XmlStreamStepPtr;
        if cur.is_null() {
            // ERROR(NULL, NULL, NULL,
            //   "xmlNewStreamComp: malloc failed\n");
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

/**
 * xmlStreamCompile:
 * @comp: the precompiled pattern
 *
 * Tries to stream compile a pattern
 *
 * Returns -1 in case of failure and 0 in case of success.
 */
unsafe extern "C" fn xml_stream_compile(comp: XmlPatternPtr) -> i32 {
    let stream: XmlStreamCompPtr;
    let mut s: i32 = 0;
    let mut root: i32 = 0;
    let mut flags: i32 = 0;
    let mut prevs: i32 = -1;
    let mut step: XmlStepOp;

    if comp.is_null() || (*comp).steps.is_null() {
        return -1;
    }
    /*
     * special case for .
     */
    if (*comp).nb_step == 1
        && matches!((*(*comp).steps.add(0)).op, XmlPatOp::XmlOpElem)
        && (*(*comp).steps.add(0)).value.is_null()
        && (*(*comp).steps.add(0)).value2.is_null()
    {
        stream = xml_new_stream_comp(0);
        if stream.is_null() {
            return -1;
        }
        /* Note that the stream will have no steps in this case. */
        (*stream).flags |= XML_STREAM_FINAL_IS_ANY_NODE as i32;
        (*comp).stream = stream;
        return 0;
    }

    stream = xml_new_stream_comp(((*comp).nb_step / 2) + 1);
    if stream.is_null() {
        return -1;
    }
    if !(*comp).dict.is_null() {
        (*stream).dict = (*comp).dict;
        xml_dict_reference((*stream).dict);
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
                        /*
                         * We have a "." or "self::node()" here.
                         * Eliminate redundant self::node() tests like in "/./."
                         * or "//./"
                         * The only case we won't eliminate is "//.", i.e. if
                         * self::node() is the last node test and we had
                         * continuation somewhere beforehand.
                         */
                        if (*comp).nb_step == i + 1 && flags & XML_STREAM_STEP_DESC as i32 != 0 {
                            /*
                             * Mark the special case where the expression resolves
                             * to any type of node.
                             */
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
                            /*
                             * If there was a previous step, mark it to be added to
                             * the result node-set; this is needed since only
                             * the last step will be marked as "is_final" and only
                             * "is_final" nodes are added to the resulting set.
                             */
                            if prevs != -1 {
                                (*(*stream).steps.add(prevs as usize)).flags |=
                                    XML_STREAM_STEP_IN_SET as i32;
                                prevs = -1;
                            }
                            break 'to_break;
                        } else {
                            /* Just skip this one. */
                            continue 'main;
                        }
                    }
                    /* An element node. */
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
                    /* An element node child. */
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
                    /* Skip redundant continuations. */
                    if flags & XML_STREAM_STEP_DESC as i32 != 0 {
                        // break;
                    } else {
                        flags |= XML_STREAM_STEP_DESC as i32;
                        /*
                         * Mark the expression as having "//".
                         */
                        if (*stream).flags & XML_STREAM_DESC as i32 == 0 {
                            (*stream).flags |= XML_STREAM_DESC as i32;
                        }
                    }
                }
            }
        }
        if root == 0 && (*comp).flags & XML_PATTERN_NOTPATTERN == 0 {
            /*
             * If this should behave like a real pattern, we will mark
             * the first step as having "//", to be reentrant on every
             * tree level.
             */
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

/**
 * xmlReversePattern:
 * @comp:  the compiled is_match expression
 *
 * reverse all the stack of expressions
 *
 * returns 0 in case of success and -1 in case of error.
 */
unsafe extern "C" fn xml_reverse_pattern(comp: XmlPatternPtr) -> i32 {
    let mut i: i32;
    let mut j: i32;

    /*
     * remove the leading // for //a or .//a
     */
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
            // ERROR(ctxt, NULL, NULL, "xmlReversePattern: realloc failed\n");
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

/**
 * xmlPatterncompile:
 * @pattern: the pattern to compile
 * @dict: an optional dictionary for interned strings
 * @flags: compilation flags, see XmlPatternFlags
 * @namespaces: the prefix definitions, array of [URI, prefix] or NULL
 *
 * Compile a pattern.
 *
 * Returns the compiled form of the pattern or NULL in case of error
 */
pub unsafe extern "C" fn xml_patterncompile(
    pattern: *const XmlChar,
    dict: *mut XmlDict,
    flags: i32,
    namespaces: *mut *const XmlChar,
) -> XmlPatternPtr {
    let mut ret: XmlPatternPtr = null_mut();
    let mut cur: XmlPatternPtr;
    let mut ctxt: XmlPatParserContextPtr = null_mut();
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
            if *or == 0 {
                ctxt = xml_new_pat_parser_context(start, dict, namespaces);
            } else {
                tmp = xml_strndup(start, or.offset_from(start) as _);
                if !tmp.is_null() {
                    ctxt = xml_new_pat_parser_context(tmp, dict, namespaces);
                }
                or = or.add(1);
            }
            if ctxt.is_null() {
                break 'error;
            }
            cur = xml_new_pattern();
            if cur.is_null() {
                break 'error;
            }
            /*
             * Assign string dict.
             */
            if !dict.is_null() {
                (*cur).dict = dict;
                xml_dict_reference(dict);
            }
            if ret.is_null() {
                ret = cur;
            } else {
                (*cur).next = (*ret).next;
                (*ret).next = cur;
            }
            (*cur).flags = flags;
            (*ctxt).comp = cur;

            if XML_STREAM_XS_IDC!(cur) {
                xml_compile_idc_xpath_path(ctxt);
            } else {
                xml_compile_path_pattern(ctxt);
            }
            if (*ctxt).error != 0 {
                break 'error;
            }
            xml_free_pat_parser_context(ctxt);
            ctxt = null_mut();

            if streamable != 0 {
                if typ == 0 {
                    typ = (*cur).flags & (PAT_FROM_ROOT | PAT_FROM_CUR) as i32;
                } else if typ == PAT_FROM_ROOT as i32 {
                    if (*cur).flags & PAT_FROM_CUR as i32 != 0 {
                        streamable = 0;
                    }
                } else if typ == PAT_FROM_CUR as i32 && (*cur).flags & PAT_FROM_ROOT as i32 != 0 {
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
    if !ctxt.is_null() {
        xml_free_pat_parser_context(ctxt);
    }
    if !ret.is_null() {
        xml_free_pattern(ret);
    }
    if !tmp.is_null() {
        xml_free(tmp as _);
    }
    null_mut()
}

pub type XmlStepStatePtr = *mut XmlStepState;
#[repr(C)]
pub struct XmlStepState {
    step: i32,
    node: XmlNodePtr,
}

pub type XmlStepStatesPtr = *mut XmlStepStates;
#[repr(C)]
pub struct XmlStepStates {
    nbstates: i32,
    maxstates: i32,
    states: XmlStepStatePtr,
}

unsafe extern "C" fn xml_pat_push_state(
    states: *mut XmlStepStates,
    step: i32,
    node: XmlNodePtr,
) -> i32 {
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

/**
 * xmlPatMatch:
 * @comp: the precompiled pattern
 * @node: a node
 *
 * Test whether the node matches the pattern
 *
 * Returns 1 if it matches, 0 if it doesn't and -1 in case of failure
 */
unsafe extern "C" fn xml_pat_match(comp: XmlPatternPtr, mut node: XmlNodePtr) -> i32 {
    let mut i: i32;
    let mut step: XmlStepOpPtr;
    let mut states: XmlStepStates = XmlStepStates {
        nbstates: 0,
        maxstates: 0,
        states: null_mut(),
    }; /* // may require backtrack */

    if comp.is_null() || node.is_null() {
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
                                if matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
                                    break 'rollback;
                                }
                                node = (*node).parent;
                                if matches!(
                                    (*node).typ,
                                    XmlElementType::XmlDocumentNode
                                        | XmlElementType::XmlHTMLDocumentNode
                                ) {
                                    break 'to_continue;
                                }
                                break 'rollback;
                            }
                            XmlPatOp::XmlOpElem => {
                                if !matches!((*node).typ, XmlElementType::XmlElementNode) {
                                    break 'rollback;
                                }
                                if (*step).value.is_null() {
                                    break 'to_continue;
                                }
                                if *(*step).value.add(0) != *(*node).name.add(0) {
                                    break 'rollback;
                                }
                                if !xml_str_equal((*step).value, (*node).name) {
                                    break 'rollback;
                                }

                                /* Namespace test */
                                if (*node).ns.is_null() {
                                    if !(*step).value2.is_null() {
                                        break 'rollback;
                                    }
                                } else if !(*(*node).ns).href.is_null() {
                                    if (*step).value2.is_null() {
                                        break 'rollback;
                                    }
                                    if !xml_str_equal((*step).value2, (*(*node).ns).href) {
                                        break 'rollback;
                                    }
                                }
                                break 'to_continue;
                            }
                            XmlPatOp::XmlOpChild => {
                                let mut lst: XmlNodePtr;

                                if !matches!(
                                    (*node).typ,
                                    XmlElementType::XmlElementNode
                                        | XmlElementType::XmlDocumentNode
                                        | XmlElementType::XmlHTMLDocumentNode
                                ) {
                                    break 'rollback;
                                }

                                lst = (*node).children;

                                if !(*step).value.is_null() {
                                    while !lst.is_null() {
                                        if matches!((*lst).typ, XmlElementType::XmlElementNode)
                                            && *(*step).value.add(0) == *(*lst).name.add(0)
                                            && xml_str_equal((*step).value, (*lst).name)
                                        {
                                            break;
                                        }
                                        lst = (*lst).next;
                                    }
                                    if !lst.is_null() {
                                        break 'to_continue;
                                    }
                                }
                                break 'rollback;
                            }
                            XmlPatOp::XmlOpAttr => {
                                if !matches!((*node).typ, XmlElementType::XmlAttributeNode) {
                                    break 'rollback;
                                }
                                if !(*step).value.is_null() {
                                    if *(*step).value.add(0) != *(*node).name.add(0) {
                                        break 'rollback;
                                    }
                                    if !xml_str_equal((*step).value, (*node).name) {
                                        break 'rollback;
                                    }
                                }
                                /* Namespace test */
                                if (*node).ns.is_null() {
                                    if !(*step).value2.is_null() {
                                        break 'rollback;
                                    }
                                } else if !(*step).value2.is_null()
                                    && !xml_str_equal((*step).value2, (*(*node).ns).href)
                                {
                                    break 'rollback;
                                }
                                break 'to_continue;
                            }
                            XmlPatOp::XmlOpParent => {
                                if matches!(
                                    (*node).typ,
                                    XmlElementType::XmlDocumentNode
                                        | XmlElementType::XmlHTMLDocumentNode
                                        | XmlElementType::XmlNamespaceDecl
                                ) {
                                    break 'rollback;
                                }
                                node = (*node).parent;
                                if node.is_null() {
                                    break 'rollback;
                                }
                                if (*step).value.is_null() {
                                    break 'to_continue;
                                }
                                if *(*step).value.add(0) != *(*node).name.add(0) {
                                    break 'rollback;
                                }
                                if !xml_str_equal((*step).value, (*node).name) {
                                    break 'rollback;
                                }
                                /* Namespace test */
                                if (*node).ns.is_null() {
                                    if !(*step).value2.is_null() {
                                        break 'rollback;
                                    }
                                } else if !(*(*node).ns).href.is_null() {
                                    if (*step).value2.is_null() {
                                        break 'rollback;
                                    }
                                    if !xml_str_equal((*step).value2, (*(*node).ns).href) {
                                        break 'rollback;
                                    }
                                }
                                break 'to_continue;
                            }
                            XmlPatOp::XmlOpAncestor => {
                                /* TODO: implement coalescing of ANCESTOR/NODE ops */
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
                                if node.is_null() {
                                    break 'rollback;
                                }
                                if matches!(
                                    (*node).typ,
                                    XmlElementType::XmlDocumentNode
                                        | XmlElementType::XmlHTMLDocumentNode
                                        | XmlElementType::XmlNamespaceDecl
                                ) {
                                    break 'rollback;
                                }
                                node = (*node).parent;
                                while !node.is_null() {
                                    if matches!((*node).typ, XmlElementType::XmlElementNode)
                                        && *(*step).value.add(0) == *(*node).name.add(0)
                                        && xml_str_equal((*step).value, (*node).name)
                                    {
                                        /* Namespace test */
                                        if (*node).ns.is_null() {
                                            if (*step).value2.is_null() {
                                                break;
                                            }
                                        } else if !(*(*node).ns).href.is_null()
                                            && (!(*step).value2.is_null()
                                                && xml_str_equal(
                                                    (*step).value2,
                                                    (*(*node).ns).href,
                                                ))
                                        {
                                            break;
                                        }
                                    }
                                    node = (*node).parent;
                                }
                                if node.is_null() {
                                    break 'rollback;
                                }
                                /*
                                 * prepare a potential rollback from here
                                 * for ancestors of that node.
                                 */
                                if matches!((*step).op, XmlPatOp::XmlOpAncestor) {
                                    xml_pat_push_state(addr_of_mut!(states), i, node);
                                } else {
                                    xml_pat_push_state(addr_of_mut!(states), i - 1, node);
                                }
                                break 'to_continue;
                            }
                            XmlPatOp::XmlOpNs => {
                                if !matches!((*node).typ, XmlElementType::XmlElementNode) {
                                    break 'rollback;
                                }
                                if (*node).ns.is_null() {
                                    if !(*step).value.is_null() {
                                        break 'rollback;
                                    }
                                } else if !(*(*node).ns).href.is_null() {
                                    if (*step).value.is_null() {
                                        break 'rollback;
                                    }
                                    if !xml_str_equal((*step).value, (*(*node).ns).href) {
                                        break 'rollback;
                                    }
                                }
                            }
                            XmlPatOp::XmlOpAll => {
                                if !matches!((*node).typ, XmlElementType::XmlElementNode) {
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

/**
 * xmlPatternMatch:
 * @comp: the precompiled pattern
 * @node: a node
 *
 * Test whether the node matches the pattern
 *
 * Returns 1 if it matches, 0 if it doesn't and -1 in case of failure
 */
pub unsafe extern "C" fn xml_pattern_match(mut comp: XmlPatternPtr, node: XmlNodePtr) -> i32 {
    let mut ret: i32 = 0;

    if comp.is_null() || node.is_null() {
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

/* streaming interfaces */
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

/**
 * xmlPatternStreamable:
 * @comp: the precompiled pattern
 *
 * Check if the pattern is streamable i.e. xmlPatternGetStreamCtxt()
 * should work.
 *
 * Returns 1 if streamable, 0 if not and -1 in case of error.
 */
pub unsafe extern "C" fn xml_pattern_streamable(mut comp: XmlPatternPtr) -> i32 {
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

/**
 * xmlPatternMaxDepth:
 * @comp: the precompiled pattern
 *
 * Check the maximum depth reachable by a pattern
 *
 * Returns -2 if no limit (using //), otherwise the depth,
 *         and -1 in case of error
 */
pub unsafe extern "C" fn xml_pattern_max_depth(mut comp: XmlPatternPtr) -> i32 {
    let mut ret: i32 = 0;

    if comp.is_null() {
        return -1;
    }
    while !comp.is_null() {
        if (*comp).stream.is_null() {
            return -1;
        }

        for i in 0..(*(*comp).stream).nb_step {
            if (*(*(*comp).stream).steps.add(i as usize)).flags & XML_STREAM_STEP_DESC as i32 != 0 {
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

/**
 * xmlPatternMinDepth:
 * @comp: the precompiled pattern
 *
 * Check the minimum depth reachable by a pattern, 0 mean the / or . are
 * part of the set.
 *
 * Returns -1 in case of error otherwise the depth,
 *
 */
pub unsafe extern "C" fn xml_pattern_min_depth(mut comp: XmlPatternPtr) -> i32 {
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

/**
 * xmlPatternFromRoot:
 * @comp: the precompiled pattern
 *
 * Check if the pattern must be looked at from the root.
 *
 * Returns 1 if true, 0 if false and -1 in case of error
 */
pub unsafe extern "C" fn xml_pattern_from_root(mut comp: XmlPatternPtr) -> i32 {
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

/**
 * xmlNewStreamCtxt:
 * @size: the number of expected states
 *
 * build a new stream context
 *
 * Returns the new structure or NULL in case of error.
 */
unsafe extern "C" fn xml_new_stream_ctxt(stream: XmlStreamCompPtr) -> XmlStreamCtxtPtr {
    let cur: XmlStreamCtxtPtr = xml_malloc(size_of::<XmlStreamCtxt>()) as XmlStreamCtxtPtr;
    if cur.is_null() {
        // ERROR(NULL, NULL, NULL,
        // 	"xmlNewStreamCtxt: malloc failed\n");
        return null_mut();
    }
    memset(cur as _, 0, size_of::<XmlStreamCtxt>());
    (*cur).states = xml_malloc(4 * 2 * size_of::<i32>()) as *mut i32;
    if (*cur).states.is_null() {
        xml_free(cur as _);
        // ERROR(NULL, NULL, NULL, "xmlNewStreamCtxt: malloc failed\n");
        return null_mut();
    }
    (*cur).nb_state = 0;
    (*cur).max_state = 4;
    (*cur).level = 0;
    (*cur).comp = stream;
    (*cur).block_level = -1;
    cur
}

/**
 * xmlPatternGetStreamCtxt:
 * @comp: the precompiled pattern
 *
 * Get a streaming context for that pattern
 * Use xmlFreeStreamCtxt to free the context.
 *
 * Returns a pointer to the context or NULL in case of failure
 */
pub unsafe extern "C" fn xml_pattern_get_stream_ctxt(mut comp: XmlPatternPtr) -> XmlStreamCtxtPtr {
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

/**
 * xmlFreeStreamCtxt:
 * @stream: the stream context
 *
 * Free the stream context
 */
pub unsafe extern "C" fn xml_free_stream_ctxt(mut stream: XmlStreamCtxtPtr) {
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

/**
 * xmlStreamCtxtAddState:
 * @comp: the stream context
 * @idx: the step index for that streaming state
 *
 * Add a new state to the stream context
 *
 * Returns -1 in case of error or the state index if successful
 */
unsafe extern "C" fn xml_stream_ctxt_add_state(
    comp: XmlStreamCtxtPtr,
    idx: i32,
    level: i32,
) -> i32 {
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
            // ERROR(NULL, NULL, NULL, "xmlNewStreamCtxt: malloc failed\n");
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

/**
 * xmlStreamPushInternal:
 * @stream: the stream context
 * @name: the current name
 * @ns: the namespace name
 * @nodeType: the type of the node
 *
 * Push new data onto the stream. NOTE: if the call xmlPatterncompile()
 * indicated a dictionary, then strings for name and ns will be expected
 * to come from the dictionary.
 * Both @name and @ns being NULL means the / i.e. the root of the document.
 * This can also act as a reset.
 *
 * Returns: -1 in case of error, 1 if the current state in the stream is a
 *    is_match and 0 otherwise.
 */
unsafe extern "C" fn xml_stream_push_internal(
    mut stream: XmlStreamCtxtPtr,
    name: *const XmlChar,
    ns: *const XmlChar,
    node_type: i32,
) -> i32 {
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

            if node_type == XmlElementType::XmlElementNode as i32 && name.is_null() && ns.is_null()
            {
                /* We have a document node here (or a reset). */
                (*stream).nb_state = 0;
                (*stream).level = 0;
                (*stream).block_level = -1;
                if (*comp).flags & XML_STREAM_FROM_ROOT as i32 != 0 {
                    if (*comp).nb_step == 0 {
                        /* TODO: We have a "/." here? */
                        ret = 1;
                    } else if (*comp).nb_step == 1
                        && (*(*comp).steps.add(0)).node_type == XML_STREAM_ANY_NODE as i32
                        && (*(*comp).steps.add(0)).flags & XML_STREAM_STEP_DESC as i32 != 0
                    {
                        /*
                         * In the case of "//." the document node will is_match
                         * as well.
                         */
                        ret = 1;
                    } else if (*(*comp).steps.add(0)).flags & XML_STREAM_STEP_ROOT as i32 != 0 {
                        /* TODO: Do we need this ? */
                        tmp = xml_stream_ctxt_add_state(stream, 0, 0);
                        if tmp < 0 {
                            err += 1;
                        }
                    }
                }
                stream = (*stream).next;
                continue 'stream; /* while */
            }

            /*
             * Fast check for ".".
             */
            if (*comp).nb_step == 0 {
                /*
                 * / and . are handled at the XPath node set creation
                 * level by checking min depth
                 */
                if (*stream).flags & XmlPatternFlags::XmlPatternXpath as i32 != 0 {
                    stream = (*stream).next;
                    continue 'stream; /* while */
                }
                /*
                 * For non-pattern like evaluation like XML Schema IDCs
                 * or traditional XPath expressions, this will is_match if
                 * we are at the first level only, otherwise on every level.
                 */
                if node_type != XmlElementType::XmlAttributeNode as i32
                    && ((*stream).flags & XML_PATTERN_NOTPATTERN == 0 || (*stream).level == 0)
                {
                    ret = 1;
                }
                (*stream).level += 1;
                break 'stream_next;
            }
            if (*stream).block_level != -1 {
                /*
                 * Skip blocked expressions.
                 */
                (*stream).level += 1;
                break 'stream_next;
            }

            if node_type != XmlElementType::XmlElementNode as i32
                && node_type != XmlElementType::XmlAttributeNode as i32
                && (*comp).flags & XML_STREAM_FINAL_IS_ANY_NODE as i32 == 0
            {
                /*
                 * No need to process nodes of other types if we don't
                 * resolve to those types.
                 * TODO: Do we need to block the context here?
                 */
                (*stream).level += 1;
                break 'stream_next;
            }

            /*
             * Check evolution of existing states
             */
            i = 0;
            m = (*stream).nb_state;
            while i < m {
                'next_state: {
                    if (*comp).flags & XML_STREAM_DESC as i32 == 0 {
                        /*
                         * If there is no "//", then only the last
                         * added state is of interest.
                         */
                        step_nr = *(*stream).states.add(2 * ((*stream).nb_state - 1) as usize);
                        /*
                         * TODO: Security check, should not happen, remove it.
                         */
                        if *(*stream)
                            .states
                            .add((2 * ((*stream).nb_state - 1) as usize) + 1)
                            < (*stream).level
                        {
                            return -1;
                        }
                        // desc = 0;
                        /* loop-stopper */
                        i = m;
                    } else {
                        /*
                         * If there are "//", then we need to process every "//"
                         * occurring in the states, plus any other state for this
                         * level.
                         */
                        step_nr = *(*stream).states.add(2 * i as usize);

                        /* TODO: should not happen anymore: dead states */
                        if step_nr < 0 {
                            break 'next_state;
                        }

                        tmp = *(*stream).states.add((2 * i) as usize + 1);

                        /* skip new states just added */
                        if tmp > (*stream).level {
                            break 'next_state;
                        }

                        /* skip states at ancestor levels, except if "//" */
                        desc = (*(*comp).steps.add(step_nr as usize)).flags
                            & XML_STREAM_STEP_DESC as i32;
                        if tmp < (*stream).level && desc == 0 {
                            break 'next_state;
                        }
                    }
                    /*
                     * Check for correct node-type.
                     */
                    step = *(*comp).steps.add(step_nr as usize);
                    if step.node_type != node_type {
                        if step.node_type == XmlElementType::XmlAttributeNode as i32 {
                            /*
                             * Block this expression for deeper evaluation.
                             */
                            if (*comp).flags & XML_STREAM_DESC as i32 == 0 {
                                (*stream).block_level = (*stream).level + 1;
                            }
                            break 'next_state;
                        } else if step.node_type != XML_STREAM_ANY_NODE as i32 {
                            break 'next_state;
                        }
                    }
                    /*
                     * Compare local/namespace-name.
                     */
                    is_match = 0;
                    if step.node_type == XML_STREAM_ANY_NODE as i32 {
                        is_match = 1;
                    } else if step.name.is_null() {
                        if step.ns.is_null() {
                            /*
                             * This lets through all elements/attributes.
                             */
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
                    // 	    } else if ((*comp).dict) {
                    // 		if (step.name.is_null()) {
                    // 		    if (step.ns.is_null())
                    // 			is_match = 1;
                    // 		    else
                    // 			is_match = (step.ns == ns);
                    // 		} else {
                    // 		    is_match = ((step.name == name) && (step.ns == ns));
                    // 		}
                    // #endif /* if 0 ------------------------------------------------------- */
                    if is_match != 0 {
                        is_final = step.flags & XML_STREAM_STEP_FINAL as i32;
                        if is_final != 0 {
                            ret = 1;
                        } else {
                            xml_stream_ctxt_add_state(stream, step_nr + 1, (*stream).level + 1);
                        }
                        if ret != 1 && step.flags & XML_STREAM_STEP_IN_SET as i32 != 0 {
                            /*
                             * Check if we have a special case like "foo/bar//.", where
                             * "foo" is selected as well.
                             */
                            ret = 1;
                        }
                    }
                    if (*comp).flags & XML_STREAM_DESC as i32 == 0
                        && (is_match == 0 || is_final != 0)
                    {
                        /*
                         * Mark this expression as blocked for any evaluation at
                         * deeper levels. Note that this includes "/foo"
                         * expressions if the *pattern* behaviour is used.
                         */
                        (*stream).block_level = (*stream).level + 1;
                    }
                }
                // next_state:
                i += 1;
            }

            (*stream).level += 1;

            /*
             * Re/enter the expression.
             * Don't reenter if it's an absolute expression like "/foo",
             *   except "//foo".
             */
            step = *(*comp).steps.add(0);
            if step.flags & XML_STREAM_STEP_ROOT as i32 != 0 {
                break 'stream_next;
            }

            desc = step.flags & XML_STREAM_STEP_DESC as i32;
            'compare: {
                if (*stream).flags & XML_PATTERN_NOTPATTERN != 0 {
                    /*
                     * Re/enter the expression if it is a "descendant" one,
                     * or if we are at the 1st level of evaluation.
                     */
                    if (*stream).level == 1 {
                        if XML_STREAM_XS_IDC!(stream) {
                            /*
                             * XS-IDC: The missing "self::node()" will always
                             * is_match the first given node.
                             */
                            break 'stream_next;
                        } else {
                            break 'compare;
                        }
                    }
                    /*
                     * A "//" is always reentrant.
                     */
                    if desc != 0 {
                        break 'compare;
                    }
                    /*
                     * XS-IDC: Process the 2nd level, since the missing
                     * "self::node()" is responsible for the 2nd level being
                     * the real start level.
                     */
                    if (*stream).level == 2 && XML_STREAM_XS_IDC!(stream) {
                        break 'compare;
                    }
                    break 'stream_next;
                }
            }

            // compare:
            /*
             * Check expected node-type.
             */
            if step.node_type != node_type
                && (node_type == XmlElementType::XmlAttributeNode as i32
                    || step.node_type != XML_STREAM_ANY_NODE as i32)
            {
                break 'stream_next;
            }
            /*
             * Compare local/namespace-name.
             */
            is_match = 0;
            if step.node_type == XML_STREAM_ANY_NODE as i32 {
                is_match = 1;
            } else if step.name.is_null() {
                if step.ns.is_null() {
                    /*
                     * This lets through all elements/attributes.
                     */
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
                    /*
                     * Check if we have a special case like "foo//.", where
                     * "foo" is selected as well.
                     */
                    ret = 1;
                }
            }
            if (*comp).flags & XML_STREAM_DESC as i32 == 0 && (is_match == 0 || is_final != 0) {
                /*
                 * Mark this expression as blocked for any evaluation at
                 * deeper levels.
                 */
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

/**
 * xmlStreamPushNode:
 * @stream: the stream context
 * @name: the current name
 * @ns: the namespace name
 * @nodeType: the type of the node being pushed
 *
 * Push new data onto the stream. NOTE: if the call xmlPatterncompile()
 * indicated a dictionary, then strings for name and ns will be expected
 * to come from the dictionary.
 * Both @name and @ns being NULL means the / i.e. the root of the document.
 * This can also act as a reset.
 * Different from xmlStreamPush() this function can be fed with nodes of type:
 * element-, attribute-, text-, cdata-section-, comment- and
 * processing-instruction-node.
 *
 * Returns: -1 in case of error, 1 if the current state in the stream is a
 *    is_match and 0 otherwise.
 */
pub unsafe extern "C" fn xml_stream_push_node(
    stream: XmlStreamCtxtPtr,
    name: *const XmlChar,
    ns: *const XmlChar,
    node_type: i32,
) -> i32 {
    xml_stream_push_internal(stream, name, ns, node_type)
}

/**
 * xmlStreamPush:
 * @stream: the stream context
 * @name: the current name
 * @ns: the namespace name
 *
 * Push new data onto the stream. NOTE: if the call xmlPatterncompile()
 * indicated a dictionary, then strings for name and ns will be expected
 * to come from the dictionary.
 * Both @name and @ns being NULL means the / i.e. the root of the document.
 * This can also act as a reset.
 * Otherwise the function will act as if it has been given an element-node.
 *
 * Returns: -1 in case of error, 1 if the current state in the stream is a
 *    is_match and 0 otherwise.
 */
pub unsafe extern "C" fn xml_stream_push(
    stream: XmlStreamCtxtPtr,
    name: *const XmlChar,
    ns: *const XmlChar,
) -> i32 {
    xml_stream_push_internal(stream, name, ns, XmlElementType::XmlElementNode as i32)
}

/**
* xmlStreamPushAttr:
* @stream: the stream context
* @name: the current name
* @ns: the namespace name
*
* Push new attribute data onto the stream. NOTE: if the call xmlPatterncompile()
* indicated a dictionary, then strings for name and ns will be expected
* to come from the dictionary.
* Both @name and @ns being NULL means the / i.e. the root of the document.
* This can also act as a reset.
* Otherwise the function will act as if it has been given an attribute-node.
*
* Returns: -1 in case of error, 1 if the current state in the stream is a
*    is_match and 0 otherwise.
*/
pub unsafe extern "C" fn xml_stream_push_attr(
    stream: XmlStreamCtxtPtr,
    name: *const XmlChar,
    ns: *const XmlChar,
) -> i32 {
    xml_stream_push_internal(stream, name, ns, XmlElementType::XmlAttributeNode as i32)
}

/**
 * xmlStreamPop:
 * @stream: the stream context
 *
 * push one level from the stream.
 *
 * Returns: -1 in case of error, 0 otherwise.
 */
pub unsafe extern "C" fn xml_stream_pop(mut stream: XmlStreamCtxtPtr) -> i32 {
    let mut lev: i32;

    if stream.is_null() {
        return -1;
    }
    while !stream.is_null() {
        /*
        	* Reset block-level.
        	*/
        if (*stream).block_level == (*stream).level {
            (*stream).block_level = -1;
        }

        /*
         *  (*stream).level can be zero when XML_FINAL_IS_ANY_NODE is set
         *  (see the thread at
         *  http://mail.gnome.org/archives/xslt/2008-July/msg00027.html)
         */
        if (*stream).level != 0 {
            (*stream).level -= 1;
        }
        /*
         * Check evolution of existing states
         */
        for i in (0..(*stream).nb_state).rev() {
            /* discard obsoleted states */
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

/**
 * xmlStreamWantsAnyNode:
 * @stream: the stream context
 *
 * Query if the streaming pattern additionally needs to be fed with
 * text-, cdata-section-, comment- and processing-instruction-nodes.
 * If the result is 0 then only element-nodes and attribute-nodes
 * need to be pushed.
 *
 * Returns: 1 in case of need of nodes of the above described types,
 *          0 otherwise. -1 on API errors.
 */
pub unsafe extern "C" fn xml_stream_wants_any_node(mut stream: XmlStreamCtxtPtr) -> i32 {
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
    fn test_xml_pattern_get_stream_ctxt() {

        /* missing type support */
    }

    #[test]
    fn test_xml_pattern_match() {
        #[cfg(feature = "libxml_pattern")]
        unsafe {
            let mut leaks = 0;

            for n_comp in 0..GEN_NB_XML_PATTERN_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let comp = gen_xml_pattern_ptr(n_comp, 0);
                    let node = gen_xml_node_ptr(n_node, 1);

                    let ret_val = xml_pattern_match(comp, node);
                    desret_int(ret_val);
                    des_xml_pattern_ptr(n_comp, comp, 0);
                    des_xml_node_ptr(n_node, node, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlPatternMatch",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlPatternMatch()");
                        eprint!(" {}", n_comp);
                        eprintln!(" {}", n_node);
                    }
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

//! Provide methods and data structures for debug XML documents.  
//! This module is based on `libxml/debugXML.h`, `debugXML.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, CStr, CString},
    mem::zeroed,
    os::raw::c_void,
    ptr::{addr_of, addr_of_mut, null_mut},
    sync::atomic::Ordering,
};

use libc::{fprintf, fputc, strlen, FILE};

use crate::{
    __xml_raise_error,
    error::XmlParserErrors,
    generic_error,
    libxml::{chvalid::xml_is_blank_char, entities::XmlEntityPtr},
    tree::{
        xml_free_node_list, xml_validate_name, XmlAttrPtr, XmlAttributeDefault, XmlAttributePtr,
        XmlAttributeType, XmlDocPtr, XmlDtdPtr, XmlElementPtr, XmlElementType, XmlElementTypeVal,
        XmlEnumerationPtr, XmlNodePtr, XmlNsPtr,
    },
};

#[cfg(feature = "xpath")]
use super::xpath::{XmlXPathContextPtr, XmlXPathObjectPtr};
use super::{
    dict::{xml_dict_lookup, xml_dict_owns, XmlDictPtr},
    entities::{xml_get_doc_entity, XmlEntitiesTablePtr, XmlEntityType},
    hash::xml_hash_scan,
    parser::{xml_parse_in_node_context, XmlParserOption},
    parser_internals::{XML_STRING_COMMENT, XML_STRING_TEXT, XML_STRING_TEXT_NOENC},
    valid::xml_snprintf_element_content,
    xmlstring::{xml_check_utf8, xml_str_equal, xml_strchr, xml_strlen, xml_strstr, XmlChar},
    xpath::XmlXPathObjectType,
};

pub type XmlDebugCtxtPtr = *mut XmlDebugCtxt;
#[repr(C)]
pub struct XmlDebugCtxt {
    output: *mut FILE,    /* the output file */
    shift: [c_char; 101], /* used for indenting */
    depth: i32,           /* current depth */
    doc: XmlDocPtr,       /* current document */
    node: XmlNodePtr,     /* current node */
    dict: XmlDictPtr,     /* the doc dictionary */
    check: i32,           /* do just checkings */
    errors: i32,          /* number of errors found */
    nodict: i32,          /* if the document has no dictionary */
    options: i32,         /* options */
}

/*
 * The standard Dump routines.
 */
/**
 * xmlDebugDumpString:
 * @output:  the FILE * for the output
 * @str:  the string
 *
 * Dumps information about the string, shorten it if necessary
 */
pub unsafe extern "C" fn xml_debug_dump_string(mut output: *mut FILE, str: *const XmlChar) {
    extern "C" {
        static stdout: *mut FILE;
    }
    if output.is_null() {
        output = stdout;
    }
    if str.is_null() {
        fprintf(output, c"(NULL)".as_ptr());
        return;
    }
    for i in 0..40 {
        if *str.add(i) == 0 {
            return;
        } else if xml_is_blank_char(*str.add(i) as u32) {
            fputc(b' ' as i32, output);
        } else if *str.add(i) >= 0x80 {
            fprintf(output, c"#%X".as_ptr(), *str.add(i) as i32);
        } else {
            fputc(*str.add(i) as i32, output);
        }
    }
    fprintf(output, c"...".as_ptr());
}

unsafe extern "C" fn xml_ctxt_dump_init_ctxt(ctxt: XmlDebugCtxtPtr) {
    extern "C" {
        static stdout: *mut FILE;
    }

    (*ctxt).depth = 0;
    (*ctxt).check = 0;
    (*ctxt).errors = 0;
    (*ctxt).output = stdout;
    (*ctxt).doc = null_mut();
    (*ctxt).node = null_mut();
    (*ctxt).dict = null_mut();
    (*ctxt).nodict = 0;
    (*ctxt).options = 0;
    for i in 0..100 {
        (*ctxt).shift[i] = b' ' as _;
    }
    (*ctxt).shift[100] = 0;
}

unsafe extern "C" fn xml_ctxt_dump_spaces(ctxt: XmlDebugCtxtPtr) {
    if (*ctxt).check != 0 {
        return;
    }
    if !(*ctxt).output.is_null() && (*ctxt).depth > 0 {
        if (*ctxt).depth < 50 {
            fprintf(
                (*ctxt).output,
                c"%s".as_ptr(),
                addr_of!((*ctxt).shift[100 - 2 * (*ctxt).depth as usize]),
            );
        } else {
            fprintf((*ctxt).output, c"%s".as_ptr(), (*ctxt).shift.as_ptr());
        }
    }
}

unsafe extern "C" fn xml_ctxt_dump_string(ctxt: XmlDebugCtxtPtr, str: *const XmlChar) {
    if (*ctxt).check != 0 {
        return;
    }
    /* TODO: check UTF8 content of the string */
    if str.is_null() {
        fprintf((*ctxt).output, c"(NULL)".as_ptr());
        return;
    }

    for i in 0..40 {
        if *str.add(i) == 0 {
            return;
        } else if xml_is_blank_char(*str.add(i) as u32) {
            fputc(b' ' as i32, (*ctxt).output);
        } else if *str.add(i) >= 0x80 {
            fprintf((*ctxt).output, c"#%X".as_ptr(), *str.add(i) as i32);
        } else {
            fputc(*str.add(i) as i32, (*ctxt).output);
        }
    }
    fprintf((*ctxt).output, c"...".as_ptr());
}

const DUMP_TEXT_TYPE: i32 = 1;

/**
 * xmlDebugErr:
 * @ctxt:  a debug context
 * @error:  the error code
 *
 * Handle a debug error.
 */
unsafe extern "C" fn xml_debug_err(
    ctxt: XmlDebugCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
) {
    (*ctxt).errors += 1;
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        (*ctxt).node as _,
        XmlErrorDomain::XmlFromCheck,
        error,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        None,
        None,
        None,
        0,
        0,
        c"%s".as_ptr(),
        msg
    );
}
unsafe extern "C" fn xml_debug_err2(
    ctxt: XmlDebugCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    extra: i32,
) {
    (*ctxt).errors += 1;
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        (*ctxt).node as _,
        XmlErrorDomain::XmlFromCheck,
        error,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        None,
        None,
        None,
        0,
        0,
        msg,
        extra
    );
}
unsafe extern "C" fn xml_debug_err3(
    ctxt: XmlDebugCtxtPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    extra: *const c_char,
) {
    (*ctxt).errors += 1;
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        (*ctxt).node as _,
        XmlErrorDomain::XmlFromCheck,
        error,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        None,
        None,
        None,
        0,
        0,
        msg,
        extra
    );
}

/**
 * xmlNsCheckScope:
 * @node: the node
 * @ns: the namespace node
 *
 * Check that a given namespace is in scope on a node.
 *
 * Returns 1 if in scope, -1 in case of argument error,
 *         -2 if the namespace is not in scope, and -3 if not on
 *         an ancestor node.
 */
unsafe extern "C" fn xml_ns_check_scope(mut node: XmlNodePtr, ns: XmlNsPtr) -> i32 {
    let mut cur: XmlNsPtr;

    if node.is_null() || ns.is_null() {
        return -1;
    }

    if !matches!(
        (*node).typ,
        XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlDocumentNode
            | XmlElementType::XmlTextNode
            | XmlElementType::XmlHtmlDocumentNode
            | XmlElementType::XmlXincludeStart
    ) {
        return -2;
    }

    while !node.is_null()
        && matches!(
            (*node).typ,
            XmlElementType::XmlElementNode
                | XmlElementType::XmlAttributeNode
                | XmlElementType::XmlTextNode
                | XmlElementType::XmlXincludeStart
        )
    {
        if matches!(
            (*node).typ,
            XmlElementType::XmlElementNode | XmlElementType::XmlXincludeStart
        ) {
            cur = (*node).ns_def;
            while !cur.is_null() {
                if cur == ns {
                    return 1;
                }
                if xml_str_equal(
                    (*cur).prefix.load(Ordering::Relaxed),
                    (*ns).prefix.load(Ordering::Relaxed),
                ) {
                    return -2;
                }
                cur = (*cur).next.load(Ordering::Relaxed);
            }
        }
        node = (*node).parent;
    }
    /* the xml namespace may be declared on the document node */
    if !node.is_null()
        && matches!(
            (*node).typ,
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode
        )
    {
        let old_ns: XmlNsPtr = (*(node as XmlDocPtr)).old_ns;
        if old_ns == ns {
            return 1;
        }
    }
    -3
}

/**
 * xmlCtxtNsCheckScope:
 * @ctxt: the debugging context
 * @node: the node
 * @ns: the namespace node
 *
 * Report if a given namespace is is not in scope.
 */
unsafe extern "C" fn xml_ctxt_ns_check_scope(
    ctxt: XmlDebugCtxtPtr,
    node: XmlNodePtr,
    ns: XmlNsPtr,
) {
    let ret: i32 = xml_ns_check_scope(node, ns);
    if ret == -2 {
        if (*ns).prefix.load(Ordering::Relaxed).is_null() {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckNsScope,
                c"Reference to default namespace not in scope\n".as_ptr(),
            );
        } else {
            xml_debug_err3(
                ctxt,
                XmlParserErrors::XmlCheckNsScope,
                c"Reference to namespace '%s' not in scope\n".as_ptr(),
                (*ns).prefix.load(Ordering::Relaxed) as *mut c_char,
            );
        }
    }
    if ret == -3 {
        if (*ns).prefix.load(Ordering::Relaxed).is_null() {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckNsAncestor,
                c"Reference to default namespace not on ancestor\n".as_ptr(),
            );
        } else {
            xml_debug_err3(
                ctxt,
                XmlParserErrors::XmlCheckNsAncestor,
                c"Reference to namespace '%s' not on ancestor\n".as_ptr(),
                (*ns).prefix.load(Ordering::Relaxed) as *mut c_char,
            );
        }
    }
}

/**
 * xmlCtxtCheckString:
 * @ctxt: the debug context
 * @str: the string
 *
 * Do debugging on the string, currently it just checks the UTF-8 content
 */
unsafe extern "C" fn xml_ctxt_check_string(ctxt: XmlDebugCtxtPtr, str: *const XmlChar) {
    if str.is_null() {
        return;
    }
    if (*ctxt).check != 0 && xml_check_utf8(str) == 0 {
        xml_debug_err3(
            ctxt,
            XmlParserErrors::XmlCheckNotUTF8,
            c"String is not UTF-8 %s".as_ptr(),
            str as *const c_char,
        );
    }
}

/**
 * xmlCtxtCheckName:
 * @ctxt: the debug context
 * @name: the name
 *
 * Do debugging on the name, for example the dictionary status and
 * conformance to the Name production.
 */
unsafe extern "C" fn xml_ctxt_check_name(ctxt: XmlDebugCtxtPtr, name: *const XmlChar) {
    if (*ctxt).check != 0 {
        if name.is_null() {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckNoName,
                c"Name is NULL".as_ptr(),
            );
            return;
        }
        #[cfg(any(feature = "tree", feature = "schema"))]
        if xml_validate_name(name, 0) != 0 {
            xml_debug_err3(
                ctxt,
                XmlParserErrors::XmlCheckNotNCName,
                c"Name is not an NCName '%s'".as_ptr(),
                name as *const c_char,
            );
        }
        if !(*ctxt).dict.is_null()
            && xml_dict_owns((*ctxt).dict, name) == 0
            && ((*ctxt).doc.is_null()
                || (*(*ctxt).doc).parse_flags
                    & (XmlParserOption::XmlParseSax1 as i32
                        | XmlParserOption::XmlParseNodict as i32)
                    == 0)
        {
            xml_debug_err3(
                ctxt,
                XmlParserErrors::XmlCheckOutsideDict,
                c"Name is not from the document dictionary '%s'".as_ptr(),
                name as *const c_char,
            );
        }
    }
}

unsafe extern "C" fn xml_ctxt_generic_node_check(ctxt: XmlDebugCtxtPtr, node: XmlNodePtr) {
    let dict: XmlDictPtr;
    let doc: XmlDocPtr = (*node).doc;

    if (*node).parent.is_null() {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckNoParent,
            c"Node has no parent\n".as_ptr(),
        );
    }
    if (*node).doc.is_null() {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckNoDoc,
            c"Node has no doc\n".as_ptr(),
        );
        // dict = null_mut();
    } else {
        dict = (*doc).dict;
        if dict.is_null() && (*ctxt).nodict == 0 {
            (*ctxt).nodict = 1;
        }
        if (*ctxt).doc.is_null() {
            (*ctxt).doc = doc;
        }

        if (*ctxt).dict.is_null() {
            (*ctxt).dict = dict;
        }
    }
    if !(*node).parent.is_null()
        && (*node).doc != (*(*node).parent).doc
        && !xml_str_equal((*node).name, c"pseudoroot".as_ptr() as _)
    {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckWrongDoc,
            c"Node doc differs from parent's one\n".as_ptr(),
        );
    }
    if (*node).prev.is_null() {
        if (*node).typ == XmlElementType::XmlAttributeNode {
            if !(*node).parent.is_null() && node != (*(*node).parent).properties as XmlNodePtr {
                xml_debug_err(
                    ctxt,
                    XmlParserErrors::XmlCheckNoPrev,
                    c"Attr has no prev and not first of attr list\n".as_ptr(),
                );
            }
        } else if !(*node).parent.is_null() && (*(*node).parent).children != node {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckNoPrev,
                c"Node has no prev and not first of parent list\n".as_ptr(),
            );
        }
    } else if (*(*node).prev).next != node {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckWrongPrev,
            c"Node prev->next : back link wrong\n".as_ptr(),
        );
    }
    if (*node).next.is_null() {
        if !(*node).parent.is_null()
            && (*node).typ != XmlElementType::XmlAttributeNode
            && (*(*node).parent).last != node
            && (*(*node).parent).typ == XmlElementType::XmlElementNode
        {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckNoNext,
                c"Node has no next and not last of parent list\n".as_ptr(),
            );
        }
    } else {
        if (*(*node).next).prev != node {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckWrongNext,
                c"Node next->prev : forward link wrong\n".as_ptr(),
            );
        }
        if (*(*node).next).parent != (*node).parent {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckWrongParent,
                c"Node next->prev : forward link wrong\n".as_ptr(),
            );
        }
    }
    if (*node).typ == XmlElementType::XmlElementNode {
        let mut ns: XmlNsPtr;

        ns = (*node).ns_def;
        while !ns.is_null() {
            xml_ctxt_ns_check_scope(ctxt, node, ns);
            ns = (*ns).next.load(Ordering::Relaxed);
        }
        if !(*node).ns.is_null() {
            xml_ctxt_ns_check_scope(ctxt, node, (*node).ns);
        }
    } else if (*node).typ == XmlElementType::XmlAttributeNode && !(*node).ns.is_null() {
        xml_ctxt_ns_check_scope(ctxt, node, (*node).ns);
    }

    if !matches!(
        (*node).typ,
        XmlElementType::XmlElementNode
            | XmlElementType::XmlAttributeNode
            | XmlElementType::XmlElementDecl
            | XmlElementType::XmlAttributeDecl
            | XmlElementType::XmlDtdNode
            | XmlElementType::XmlHtmlDocumentNode
            | XmlElementType::XmlDocumentNode
    ) && !(*node).content.is_null()
    {
        xml_ctxt_check_string(ctxt, (*node).content as *const XmlChar);
    }
    match (*node).typ {
        XmlElementType::XmlElementNode | XmlElementType::XmlAttributeNode => {
            xml_ctxt_check_name(ctxt, (*node).name);
        }
        XmlElementType::XmlTextNode => {
            if (*node).name == XML_STRING_TEXT.as_ptr() as _
                || (*node).name == XML_STRING_TEXT_NOENC.as_ptr() as _
            {
                // break;
            } else {
                /* some case of entity substitution can lead to this */
                if !(*ctxt).dict.is_null()
                    && ((*node).name == xml_dict_lookup((*ctxt).dict, c"nbktext".as_ptr() as _, 7))
                {
                    // break;
                } else {
                    xml_debug_err3(
                        ctxt,
                        XmlParserErrors::XmlCheckWrongName,
                        c"Text node has wrong name '%s'".as_ptr(),
                        (*node).name as *const c_char,
                    );
                }
            }
        }
        XmlElementType::XmlCommentNode => {
            if (*node).name == XML_STRING_COMMENT.as_ptr() as _ {
                // break;
            } else {
                xml_debug_err3(
                    ctxt,
                    XmlParserErrors::XmlCheckWrongName,
                    c"Comment node has wrong name '%s'".as_ptr(),
                    (*node).name as *const c_char,
                );
            }
        }
        XmlElementType::XmlPiNode => {
            xml_ctxt_check_name(ctxt, (*node).name);
        }
        XmlElementType::XmlCdataSectionNode => {
            if (*node).name.is_null() {
                // break;
            } else {
                xml_debug_err3(
                    ctxt,
                    XmlParserErrors::XmlCheckNameNotNull,
                    c"CData section has non NULL name '%s'".as_ptr(),
                    (*node).name as *const c_char,
                );
            }
        }
        XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl
        | XmlElementType::XmlNamespaceDecl
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd
        | XmlElementType::XmlDocumentNode
        | XmlElementType::XmlHtmlDocumentNode => {}
        _ => unreachable!(),
    }
}

unsafe extern "C" fn xml_ctxt_dump_dtd_node(ctxt: XmlDebugCtxtPtr, dtd: XmlDtdPtr) {
    xml_ctxt_dump_spaces(ctxt);

    if dtd.is_null() {
        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"DTD node is NULL\n".as_ptr());
        }
        return;
    }

    if (*dtd).typ != XmlElementType::XmlDtdNode {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckNotDTD,
            c"Node is not a DTD".as_ptr(),
        );
        return;
    }
    if (*ctxt).check == 0 {
        if !(*dtd).name.is_null() {
            fprintf(
                (*ctxt).output,
                c"DTD(%s)".as_ptr(),
                (*dtd).name as *mut c_char,
            );
        } else {
            fprintf((*ctxt).output, c"DTD".as_ptr());
        }
        if !(*dtd).external_id.is_null() {
            fprintf(
                (*ctxt).output,
                c", PUBLIC %s".as_ptr(),
                (*dtd).external_id as *mut c_char,
            );
        }
        if !(*dtd).system_id.is_null() {
            fprintf(
                (*ctxt).output,
                c", SYSTEM %s".as_ptr(),
                (*dtd).system_id as *mut c_char,
            );
        }
        fprintf((*ctxt).output, c"\n".as_ptr());
    }
    /*
     * Do a bit of checking
     */
    xml_ctxt_generic_node_check(ctxt, dtd as XmlNodePtr);
}

/**
 * xmlCtxtDumpDTD:
 * @output:  the FILE * for the output
 * @dtd:  the DTD
 *
 * Dumps debug information for the DTD
 */
unsafe extern "C" fn xml_ctxt_dump_dtd(ctxt: XmlDebugCtxtPtr, dtd: XmlDtdPtr) {
    if dtd.is_null() {
        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"DTD is NULL\n".as_ptr());
        }
        return;
    }
    xml_ctxt_dump_dtd_node(ctxt, dtd);
    if (*dtd).children.is_null() {
        fprintf((*ctxt).output, c"    DTD is empty\n".as_ptr());
    } else {
        (*ctxt).depth += 1;
        xml_ctxt_dump_node_list(ctxt, (*dtd).children);
        (*ctxt).depth -= 1;
    }
}

unsafe extern "C" fn xml_ctxt_dump_elem_decl(ctxt: XmlDebugCtxtPtr, elem: XmlElementPtr) {
    xml_ctxt_dump_spaces(ctxt);

    if elem.is_null() {
        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"Element declaration is NULL\n".as_ptr());
        }
        return;
    }
    if (*elem).typ != XmlElementType::XmlElementDecl {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckNotElemDecl,
            c"Node is not an element declaration".as_ptr(),
        );
        return;
    }
    if !(*elem).name.is_null() {
        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"ELEMDECL(".as_ptr());
            xml_ctxt_dump_string(ctxt, (*elem).name);
            fprintf((*ctxt).output, c")".as_ptr());
        }
    } else {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckNoName,
            c"Element declaration has no name".as_ptr(),
        );
    }
    if (*ctxt).check == 0 {
        match (*elem).etype {
            XmlElementTypeVal::XmlElementTypeUndefined => {
                fprintf((*ctxt).output, c", UNDEFINED".as_ptr());
            }
            XmlElementTypeVal::XmlElementTypeEmpty => {
                fprintf((*ctxt).output, c", EMPTY".as_ptr());
            }
            XmlElementTypeVal::XmlElementTypeAny => {
                fprintf((*ctxt).output, c", ANY".as_ptr());
            }
            XmlElementTypeVal::XmlElementTypeMixed => {
                fprintf((*ctxt).output, c", MIXED ".as_ptr());
            }
            XmlElementTypeVal::XmlElementTypeElement => {
                fprintf((*ctxt).output, c", MIXED ".as_ptr());
            }
        }
        if (*elem).typ != XmlElementType::XmlElementNode && !(*elem).content.is_null() {
            let mut buf: [c_char; 5001] = [0; 5001];

            buf[0] = 0;
            xml_snprintf_element_content(buf.as_mut_ptr(), 5000, (*elem).content, 1);
            buf[5000] = 0;
            fprintf((*ctxt).output, c"%s".as_ptr(), buf);
        }
        fprintf((*ctxt).output, c"\n".as_ptr());
    }

    /*
     * Do a bit of checking
     */
    xml_ctxt_generic_node_check(ctxt, elem as XmlNodePtr);
}

unsafe extern "C" fn xml_ctxt_dump_attr_decl(ctxt: XmlDebugCtxtPtr, attr: XmlAttributePtr) {
    xml_ctxt_dump_spaces(ctxt);

    if attr.is_null() {
        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"Attribute declaration is NULL\n".as_ptr());
        }
        return;
    }
    if (*attr).typ != XmlElementType::XmlAttributeDecl {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckNotAttrDecl,
            c"Node is not an attribute declaration".as_ptr(),
        );
        return;
    }
    if !(*attr).name.is_null() {
        if (*ctxt).check == 0 {
            fprintf(
                (*ctxt).output,
                c"ATTRDECL(%s)".as_ptr(),
                (*attr).name as *mut c_char,
            );
        }
    } else {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckNoName,
            c"Node attribute declaration has no name".as_ptr(),
        );
    }
    if !(*attr).elem.is_null() {
        if (*ctxt).check == 0 {
            fprintf(
                (*ctxt).output,
                c" for %s".as_ptr(),
                (*attr).elem as *mut c_char,
            );
        }
    } else {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckNoElem,
            c"Node attribute declaration has no element name".as_ptr(),
        );
    }
    if (*ctxt).check == 0 {
        match (*attr).atype {
            XmlAttributeType::XmlAttributeCdata => {
                fprintf((*ctxt).output, c" CDATA".as_ptr());
            }
            XmlAttributeType::XmlAttributeId => {
                fprintf((*ctxt).output, c" ID".as_ptr());
            }
            XmlAttributeType::XmlAttributeIdref => {
                fprintf((*ctxt).output, c" IDREF".as_ptr());
            }
            XmlAttributeType::XmlAttributeIdrefs => {
                fprintf((*ctxt).output, c" IDREFS".as_ptr());
            }
            XmlAttributeType::XmlAttributeEntity => {
                fprintf((*ctxt).output, c" ENTITY".as_ptr());
            }
            XmlAttributeType::XmlAttributeEntities => {
                fprintf((*ctxt).output, c" ENTITIES".as_ptr());
            }
            XmlAttributeType::XmlAttributeNmtoken => {
                fprintf((*ctxt).output, c" NMTOKEN".as_ptr());
            }
            XmlAttributeType::XmlAttributeNmtokens => {
                fprintf((*ctxt).output, c" NMTOKENS".as_ptr());
            }
            XmlAttributeType::XmlAttributeEnumeration => {
                fprintf((*ctxt).output, c" ENUMERATION".as_ptr());
            }
            XmlAttributeType::XmlAttributeNotation => {
                fprintf((*ctxt).output, c" NOTATION ".as_ptr());
            }
        }
        if !(*attr).tree.is_null() {
            let mut cur: XmlEnumerationPtr = (*attr).tree;

            for indx in 0..5 {
                if indx != 0 {
                    fprintf((*ctxt).output, c"|%s".as_ptr(), (*cur).name as *mut c_char);
                } else {
                    fprintf((*ctxt).output, c" (%s".as_ptr(), (*cur).name as *mut c_char);
                }
                cur = (*cur).next;
                if cur.is_null() {
                    break;
                }
            }
            if cur.is_null() {
                fprintf((*ctxt).output, c")".as_ptr());
            } else {
                fprintf((*ctxt).output, c"...)".as_ptr());
            }
        }
        match (*attr).def {
            XmlAttributeDefault::XmlAttributeNone => {}
            XmlAttributeDefault::XmlAttributeRequired => {
                fprintf((*ctxt).output, c" REQUIRED".as_ptr());
            }
            XmlAttributeDefault::XmlAttributeImplied => {
                fprintf((*ctxt).output, c" IMPLIED".as_ptr());
            }
            XmlAttributeDefault::XmlAttributeFixed => {
                fprintf((*ctxt).output, c" FIXED".as_ptr());
            }
        }
        if !(*attr).default_value.is_null() {
            fprintf((*ctxt).output, c"\"".as_ptr());
            xml_ctxt_dump_string(ctxt, (*attr).default_value);
            fprintf((*ctxt).output, c"\"".as_ptr());
        }
        fprintf((*ctxt).output, c"\n".as_ptr());
    }

    /*
     * Do a bit of checking
     */
    xml_ctxt_generic_node_check(ctxt, attr as XmlNodePtr);
}

unsafe extern "C" fn xml_ctxt_dump_entity_decl(ctxt: XmlDebugCtxtPtr, ent: XmlEntityPtr) {
    xml_ctxt_dump_spaces(ctxt);

    if ent.is_null() {
        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"Entity declaration is NULL\n".as_ptr());
        }
        return;
    }
    if (*ent).typ != XmlElementType::XmlEntityDecl {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckNotEntityDecl,
            c"Node is not an entity declaration".as_ptr(),
        );
        return;
    }
    if !(*ent).name.load(Ordering::Relaxed).is_null() {
        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"ENTITYDECL(".as_ptr());
            xml_ctxt_dump_string(ctxt, (*ent).name.load(Ordering::Relaxed));
            fprintf((*ctxt).output, c")".as_ptr());
        }
    } else {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckNoName,
            c"Entity declaration has no name".as_ptr(),
        );
    }
    if (*ctxt).check == 0 {
        match (*ent).etype {
            Some(XmlEntityType::XmlInternalGeneralEntity) => {
                fprintf((*ctxt).output, c", internal\n".as_ptr());
            }
            Some(XmlEntityType::XmlExternalGeneralParsedEntity) => {
                fprintf((*ctxt).output, c", external parsed\n".as_ptr());
            }
            Some(XmlEntityType::XmlExternalGeneralUnparsedEntity) => {
                fprintf((*ctxt).output, c", unparsed\n".as_ptr());
            }
            Some(XmlEntityType::XmlInternalParameterEntity) => {
                fprintf((*ctxt).output, c", parameter\n".as_ptr());
            }
            Some(XmlEntityType::XmlExternalParameterEntity) => {
                fprintf((*ctxt).output, c", external parameter\n".as_ptr());
            }
            Some(XmlEntityType::XmlInternalPredefinedEntity) => {
                fprintf((*ctxt).output, c", predefined\n".as_ptr());
            }
            _ => unreachable!(),
        }
        if !(*ent).external_id.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            fprintf(
                (*ctxt).output,
                c" ExternalID=%s\n".as_ptr(),
                (*ent).external_id.load(Ordering::Relaxed) as *mut c_char,
            );
        }
        if !(*ent).system_id.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            fprintf(
                (*ctxt).output,
                c" SystemID=%s\n".as_ptr(),
                (*ent).system_id.load(Ordering::Relaxed) as *mut c_char,
            );
        }
        if !(*ent).uri.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            fprintf(
                (*ctxt).output,
                c" URI=%s\n".as_ptr(),
                (*ent).uri.load(Ordering::Relaxed) as *mut c_char,
            );
        }
        if !(*ent).content.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            fprintf((*ctxt).output, c" content=".as_ptr());
            xml_ctxt_dump_string(ctxt, (*ent).content.load(Ordering::Relaxed));
            fprintf((*ctxt).output, c"\n".as_ptr());
        }
    }

    /*
     * Do a bit of checking
     */
    xml_ctxt_generic_node_check(ctxt, ent as XmlNodePtr);
}

unsafe extern "C" fn xml_ctxt_dump_namespace(ctxt: XmlDebugCtxtPtr, ns: XmlNsPtr) {
    xml_ctxt_dump_spaces(ctxt);

    if ns.is_null() {
        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"namespace node is NULL\n".as_ptr());
        }
        return;
    }
    if (*ns).typ != Some(XmlElementType::XmlNamespaceDecl) {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckNotNsDecl,
            c"Node is not a namespace declaration".as_ptr(),
        );
        return;
    }
    if (*ns).href.load(Ordering::Relaxed).is_null() {
        if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
            xml_debug_err3(
                ctxt,
                XmlParserErrors::XmlCheckNoHref,
                c"Incomplete namespace %s href=NULL\n".as_ptr(),
                (*ns).prefix.load(Ordering::Relaxed) as *mut c_char,
            );
        } else {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckNoHref,
                c"Incomplete default namespace href=NULL\n".as_ptr(),
            );
        }
    } else if (*ctxt).check == 0 {
        if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
            fprintf(
                (*ctxt).output,
                c"namespace %s href=".as_ptr(),
                (*ns).prefix.load(Ordering::Relaxed) as *mut c_char,
            );
        } else {
            fprintf((*ctxt).output, c"default namespace href=".as_ptr());
        }

        xml_ctxt_dump_string(ctxt, (*ns).href.load(Ordering::Relaxed));
        fprintf((*ctxt).output, c"\n".as_ptr());
    }
}

unsafe extern "C" fn xml_ctxt_dump_namespace_list(ctxt: XmlDebugCtxtPtr, mut ns: XmlNsPtr) {
    while !ns.is_null() {
        xml_ctxt_dump_namespace(ctxt, ns);
        ns = (*ns).next.load(Ordering::Relaxed);
    }
}

unsafe extern "C" fn xml_ctxt_dump_entity(ctxt: XmlDebugCtxtPtr, ent: XmlEntityPtr) {
    xml_ctxt_dump_spaces(ctxt);

    if ent.is_null() {
        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"Entity is NULL\n".as_ptr());
        }
        return;
    }
    if (*ctxt).check == 0 {
        match (*ent).etype {
            Some(XmlEntityType::XmlInternalGeneralEntity) => {
                fprintf((*ctxt).output, c"INTERNAL_GENERAL_ENTITY ".as_ptr());
            }
            Some(XmlEntityType::XmlExternalGeneralParsedEntity) => {
                fprintf((*ctxt).output, c"EXTERNAL_GENERAL_PARSED_ENTITY ".as_ptr());
            }
            Some(XmlEntityType::XmlExternalGeneralUnparsedEntity) => {
                fprintf(
                    (*ctxt).output,
                    c"EXTERNAL_GENERAL_UNPARSED_ENTITY ".as_ptr(),
                );
            }
            Some(XmlEntityType::XmlInternalParameterEntity) => {
                fprintf((*ctxt).output, c"INTERNAL_PARAMETER_ENTITY ".as_ptr());
            }
            Some(XmlEntityType::XmlExternalParameterEntity) => {
                fprintf((*ctxt).output, c"EXTERNAL_PARAMETER_ENTITY ".as_ptr());
            }
            Some(e) => {
                fprintf((*ctxt).output, c"ENTITY_%d ! ".as_ptr(), e as i32);
            }
            _ => unreachable!(),
        }
        fprintf(
            (*ctxt).output,
            c"%s\n".as_ptr(),
            (*ent).name.load(Ordering::Relaxed),
        );
        if !(*ent).external_id.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            fprintf(
                (*ctxt).output,
                c"ExternalID=%s\n".as_ptr(),
                (*ent).external_id.load(Ordering::Relaxed) as *mut c_char,
            );
        }
        if !(*ent).system_id.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            fprintf(
                (*ctxt).output,
                c"SystemID=%s\n".as_ptr(),
                (*ent).system_id.load(Ordering::Relaxed) as *mut c_char,
            );
        }
        if !(*ent).uri.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            fprintf(
                (*ctxt).output,
                c"URI=%s\n".as_ptr(),
                (*ent).uri.load(Ordering::Relaxed) as *mut c_char,
            );
        }
        if !(*ent).content.load(Ordering::Relaxed).is_null() {
            xml_ctxt_dump_spaces(ctxt);
            fprintf((*ctxt).output, c"content=".as_ptr());
            xml_ctxt_dump_string(ctxt, (*ent).content.load(Ordering::Relaxed));
            fprintf((*ctxt).output, c"\n".as_ptr());
        }
    }
}

/**
 * xmlCtxtDumpAttrList:
 * @output:  the FILE * for the output
 * @attr:  the attribute list
 * @depth:  the indentation level.
 *
 * Dumps debug information for the attribute list
 */
unsafe extern "C" fn xml_ctxt_dump_attr_list(ctxt: XmlDebugCtxtPtr, mut attr: XmlAttrPtr) {
    while !attr.is_null() {
        xml_ctxt_dump_attr(ctxt, attr);
        attr = (*attr).next;
    }
}

/**
 * xmlCtxtDumpOneNode:
 * @output:  the FILE * for the output
 * @node:  the node
 * @depth:  the indentation level.
 *
 * Dumps debug information for the element node, it is not recursive
 */
unsafe extern "C" fn xml_ctxt_dump_one_node(ctxt: XmlDebugCtxtPtr, node: XmlNodePtr) {
    if node.is_null() {
        if (*ctxt).check == 0 {
            xml_ctxt_dump_spaces(ctxt);
            fprintf((*ctxt).output, c"node is NULL\n".as_ptr());
        }
        return;
    }
    (*ctxt).node = node;

    match (*node).typ {
        XmlElementType::XmlElementNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                fprintf((*ctxt).output, c"ELEMENT ".as_ptr());
                if !(*node).ns.is_null() && !(*(*node).ns).prefix.load(Ordering::Relaxed).is_null()
                {
                    xml_ctxt_dump_string(ctxt, (*(*node).ns).prefix.load(Ordering::Relaxed));
                    fprintf((*ctxt).output, c":".as_ptr());
                }
                xml_ctxt_dump_string(ctxt, (*node).name);
                fprintf((*ctxt).output, c"\n".as_ptr());
            }
        }
        XmlElementType::XmlAttributeNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
            }
            fprintf((*ctxt).output, c"Error, ATTRIBUTE found here\n".as_ptr());
            xml_ctxt_generic_node_check(ctxt, node);
            return;
        }
        XmlElementType::XmlTextNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                if (*node).name == XML_STRING_TEXT_NOENC.as_ptr() as *const XmlChar {
                    fprintf((*ctxt).output, c"TEXT no enc".as_ptr());
                } else {
                    fprintf((*ctxt).output, c"TEXT".as_ptr());
                }
                if (*ctxt).options & DUMP_TEXT_TYPE != 0 {
                    if (*node).content == addr_of_mut!((*node).properties) as *mut XmlChar {
                        fprintf((*ctxt).output, c" compact\n".as_ptr());
                    } else if xml_dict_owns((*ctxt).dict, (*node).content) == 1 {
                        fprintf((*ctxt).output, c" interned\n".as_ptr());
                    } else {
                        fprintf((*ctxt).output, c"\n".as_ptr());
                    }
                } else {
                    fprintf((*ctxt).output, c"\n".as_ptr());
                }
            }
        }
        XmlElementType::XmlCdataSectionNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                fprintf((*ctxt).output, c"CDATA_SECTION\n".as_ptr());
            }
        }
        XmlElementType::XmlEntityRefNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                fprintf(
                    (*ctxt).output,
                    c"ENTITY_REF(%s)\n".as_ptr(),
                    (*node).name as *mut c_char,
                );
            }
        }
        XmlElementType::XmlEntityNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                fprintf((*ctxt).output, c"ENTITY\n".as_ptr());
            }
        }
        XmlElementType::XmlPiNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                fprintf(
                    (*ctxt).output,
                    c"PI %s\n".as_ptr(),
                    (*node).name as *mut c_char,
                );
            }
        }
        XmlElementType::XmlCommentNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                fprintf((*ctxt).output, c"COMMENT\n".as_ptr());
            }
        }
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
            }
            fprintf((*ctxt).output, c"Error, DOCUMENT found here\n".as_ptr());
            xml_ctxt_generic_node_check(ctxt, node);
            return;
        }
        XmlElementType::XmlDocumentTypeNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                fprintf((*ctxt).output, c"DOCUMENT_TYPE\n".as_ptr());
            }
        }
        XmlElementType::XmlDocumentFragNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                fprintf((*ctxt).output, c"DOCUMENT_FRAG\n".as_ptr());
            }
        }
        XmlElementType::XmlNotationNode => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                fprintf((*ctxt).output, c"NOTATION\n".as_ptr());
            }
        }
        XmlElementType::XmlDtdNode => {
            xml_ctxt_dump_dtd_node(ctxt, node as XmlDtdPtr);
            return;
        }
        XmlElementType::XmlElementDecl => {
            xml_ctxt_dump_elem_decl(ctxt, node as XmlElementPtr);
            return;
        }
        XmlElementType::XmlAttributeDecl => {
            xml_ctxt_dump_attr_decl(ctxt, node as XmlAttributePtr);
            return;
        }
        XmlElementType::XmlEntityDecl => {
            xml_ctxt_dump_entity_decl(ctxt, node as XmlEntityPtr);
            return;
        }
        XmlElementType::XmlNamespaceDecl => {
            xml_ctxt_dump_namespace(ctxt, node as XmlNsPtr);
            return;
        }
        XmlElementType::XmlXincludeStart => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                fprintf((*ctxt).output, c"INCLUDE START\n".as_ptr());
            }
            return;
        }
        XmlElementType::XmlXincludeEnd => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
                fprintf((*ctxt).output, c"INCLUDE END\n".as_ptr());
            }
            return;
        }
        _ => {
            if (*ctxt).check == 0 {
                xml_ctxt_dump_spaces(ctxt);
            }
            xml_debug_err2(
                ctxt,
                XmlParserErrors::XmlCheckUnknownNode,
                c"Unknown node type %d\n".as_ptr(),
                (*node).typ as i32,
            );
            return;
        }
    }
    if (*node).doc.is_null() {
        if (*ctxt).check == 0 {
            xml_ctxt_dump_spaces(ctxt);
        }
        fprintf((*ctxt).output, c"PBM: doc.is_null() !!!\n".as_ptr());
    }
    (*ctxt).depth += 1;
    if (*node).typ == XmlElementType::XmlElementNode && !(*node).ns_def.is_null() {
        xml_ctxt_dump_namespace_list(ctxt, (*node).ns_def);
    }
    if (*node).typ == XmlElementType::XmlElementNode && !(*node).properties.is_null() {
        xml_ctxt_dump_attr_list(ctxt, (*node).properties);
    }
    if (*node).typ != XmlElementType::XmlEntityRefNode {
        if ((*node).typ != XmlElementType::XmlElementNode && !(*node).content.is_null())
            && (*ctxt).check == 0
        {
            xml_ctxt_dump_spaces(ctxt);
            fprintf((*ctxt).output, c"content=".as_ptr());
            xml_ctxt_dump_string(ctxt, (*node).content);
            fprintf((*ctxt).output, c"\n".as_ptr());
        }
    } else {
        let ent: XmlEntityPtr = xml_get_doc_entity((*node).doc, (*node).name);
        if !ent.is_null() {
            xml_ctxt_dump_entity(ctxt, ent);
        }
    }
    (*ctxt).depth -= 1;

    /*
     * Do a bit of checking
     */
    xml_ctxt_generic_node_check(ctxt, node);
}

/**
 * xmlCtxtDumpNode:
 * @output:  the FILE * for the output
 * @node:  the node
 * @depth:  the indentation level.
 *
 * Dumps debug information for the element node, it is recursive
 */
unsafe extern "C" fn xml_ctxt_dump_node(ctxt: XmlDebugCtxtPtr, node: XmlNodePtr) {
    if node.is_null() {
        if (*ctxt).check == 0 {
            xml_ctxt_dump_spaces(ctxt);
            fprintf((*ctxt).output, c"node is NULL\n".as_ptr());
        }
        return;
    }
    xml_ctxt_dump_one_node(ctxt, node);
    if (*node).typ != XmlElementType::XmlNamespaceDecl
        && !(*node).children.is_null()
        && (*node).typ != XmlElementType::XmlEntityRefNode
    {
        (*ctxt).depth += 1;
        xml_ctxt_dump_node_list(ctxt, (*node).children);
        (*ctxt).depth -= 1;
    }
}

/**
 * xmlCtxtDumpNodeList:
 * @output:  the FILE * for the output
 * @node:  the node list
 * @depth:  the indentation level.
 *
 * Dumps debug information for the list of element node, it is recursive
 */
unsafe extern "C" fn xml_ctxt_dump_node_list(ctxt: XmlDebugCtxtPtr, mut node: XmlNodePtr) {
    while !node.is_null() {
        xml_ctxt_dump_node(ctxt, node);
        node = (*node).next;
    }
}

/**
 * xmlCtxtDumpAttr:
 * @output:  the FILE * for the output
 * @attr:  the attribute
 * @depth:  the indentation level.
 *
 * Dumps debug information for the attribute
 */
unsafe extern "C" fn xml_ctxt_dump_attr(ctxt: XmlDebugCtxtPtr, attr: XmlAttrPtr) {
    xml_ctxt_dump_spaces(ctxt);

    if attr.is_null() {
        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"Attr is NULL".as_ptr());
        }
        return;
    }
    if (*ctxt).check == 0 {
        fprintf((*ctxt).output, c"ATTRIBUTE ".as_ptr());
        xml_ctxt_dump_string(ctxt, (*attr).name);
        fprintf((*ctxt).output, c"\n".as_ptr());
        if !(*attr).children.is_null() {
            (*ctxt).depth += 1;
            xml_ctxt_dump_node_list(ctxt, (*attr).children);
            (*ctxt).depth -= 1;
        }
    }
    if (*attr).name.is_null() {
        xml_debug_err(
            ctxt,
            XmlParserErrors::XmlCheckNoName,
            c"Attribute has no name".as_ptr(),
        );
    }

    /*
     * Do a bit of checking
     */
    xml_ctxt_generic_node_check(ctxt, attr as XmlNodePtr);
}

unsafe extern "C" fn xml_ctxt_dump_clean_ctxt(_ctxt: XmlDebugCtxtPtr) {
    /* remove the ATTRIBUTE_UNUSED when this is added */
}

/**
 * xmlDebugDumpAttr:
 * @output:  the FILE * for the output
 * @attr:  the attribute
 * @depth:  the indentation level.
 *
 * Dumps debug information for the attribute
 */
pub unsafe extern "C" fn xml_debug_dump_attr(output: *mut FILE, attr: XmlAttrPtr, depth: i32) {
    let mut ctxt: XmlDebugCtxt = zeroed();

    if output.is_null() {
        return;
    }
    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = output;
    ctxt.depth = depth;
    xml_ctxt_dump_attr(addr_of_mut!(ctxt), attr);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/**
 * xmlDebugDumpAttrList:
 * @output:  the FILE * for the output
 * @attr:  the attribute list
 * @depth:  the indentation level.
 *
 * Dumps debug information for the attribute list
 */
pub unsafe extern "C" fn xml_debug_dump_attr_list(output: *mut FILE, attr: XmlAttrPtr, depth: i32) {
    let mut ctxt: XmlDebugCtxt = unsafe { zeroed() };

    if output.is_null() {
        return;
    }
    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = output;
    ctxt.depth = depth;
    xml_ctxt_dump_attr_list(addr_of_mut!(ctxt), attr);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/**
 * xmlDebugDumpOneNode:
 * @output:  the FILE * for the output
 * @node:  the node
 * @depth:  the indentation level.
 *
 * Dumps debug information for the element node, it is not recursive
 */
pub unsafe extern "C" fn xml_debug_dump_one_node(output: *mut FILE, node: XmlNodePtr, depth: i32) {
    let mut ctxt: XmlDebugCtxt = unsafe { zeroed() };

    if output.is_null() {
        return;
    }
    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = output;
    ctxt.depth = depth;
    xml_ctxt_dump_one_node(addr_of_mut!(ctxt), node);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/**
 * xmlDebugDumpNode:
 * @output:  the FILE * for the output
 * @node:  the node
 * @depth:  the indentation level.
 *
 * Dumps debug information for the element node, it is recursive
 */
pub unsafe extern "C" fn xml_debug_dump_node(mut output: *mut FILE, node: XmlNodePtr, depth: i32) {
    let mut ctxt: XmlDebugCtxt = unsafe { zeroed() };

    extern "C" {
        static stdout: *mut FILE;
    }
    if output.is_null() {
        output = stdout;
    }
    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = output;
    ctxt.depth = depth;
    xml_ctxt_dump_node(addr_of_mut!(ctxt), node);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/**
 * xmlDebugDumpNodeList:
 * @output:  the FILE * for the output
 * @node:  the node list
 * @depth:  the indentation level.
 *
 * Dumps debug information for the list of element node, it is recursive
 */
pub unsafe extern "C" fn xml_debug_dump_node_list(
    mut output: *mut FILE,
    node: XmlNodePtr,
    depth: i32,
) {
    let mut ctxt: XmlDebugCtxt = unsafe { zeroed() };

    extern "C" {
        static stdout: *mut FILE;
    }

    if output.is_null() {
        output = stdout;
    }
    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = output;
    ctxt.depth = depth;
    xml_ctxt_dump_node_list(addr_of_mut!(ctxt), node);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

unsafe extern "C" fn xml_ctxt_dump_doc_head(ctxt: XmlDebugCtxtPtr, doc: XmlDocPtr) {
    if doc.is_null() {
        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"DOCUMENT.is_null() !\n".as_ptr());
        }
        return;
    }
    (*ctxt).node = doc as XmlNodePtr;

    match (*doc).typ {
        XmlElementType::XmlElementNode => {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckFoundElement,
                c"Misplaced ELEMENT node\n".as_ptr(),
            );
        }
        XmlElementType::XmlAttributeNode => {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckFoundAttribute,
                c"Misplaced ATTRIBUTE node\n".as_ptr(),
            );
        }
        XmlElementType::XmlTextNode => {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckFoundText,
                c"Misplaced TEXT node\n".as_ptr(),
            );
        }
        XmlElementType::XmlCdataSectionNode => {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckFoundCDATA,
                c"Misplaced CDATA node\n".as_ptr(),
            );
        }
        XmlElementType::XmlEntityRefNode => {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckFoundEntityRef,
                c"Misplaced ENTITYREF node\n".as_ptr(),
            );
        }
        XmlElementType::XmlEntityNode => {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckFoundEntity,
                c"Misplaced ENTITY node\n".as_ptr(),
            );
        }
        XmlElementType::XmlPiNode => {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckFoundPI,
                c"Misplaced PI node\n".as_ptr(),
            );
        }
        XmlElementType::XmlCommentNode => {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckFoundComment,
                c"Misplaced COMMENT node\n".as_ptr(),
            );
        }
        XmlElementType::XmlDocumentNode => {
            if (*ctxt).check == 0 {
                fprintf((*ctxt).output, c"DOCUMENT\n".as_ptr());
            }
        }
        XmlElementType::XmlHtmlDocumentNode => {
            if (*ctxt).check == 0 {
                fprintf((*ctxt).output, c"HTML DOCUMENT\n".as_ptr());
            }
        }
        XmlElementType::XmlDocumentTypeNode => {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckFoundDoctype,
                c"Misplaced DOCTYPE node\n".as_ptr(),
            );
        }
        XmlElementType::XmlDocumentFragNode => {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckFoundFragment,
                c"Misplaced FRAGMENT node\n".as_ptr(),
            );
        }
        XmlElementType::XmlNotationNode => {
            xml_debug_err(
                ctxt,
                XmlParserErrors::XmlCheckFoundNotation,
                c"Misplaced NOTATION node\n".as_ptr(),
            );
        }
        _ => {
            xml_debug_err2(
                ctxt,
                XmlParserErrors::XmlCheckUnknownNode,
                c"Unknown node type %d\n".as_ptr(),
                (*doc).typ as i32,
            );
        }
    }
}

/**
 * xmlCtxtDumpDocumentHead:
 * @output:  the FILE * for the output
 * @doc:  the document
 *
 * Dumps debug information concerning the document, not recursive
 */
unsafe extern "C" fn xml_ctxt_dump_document_head(ctxt: XmlDebugCtxtPtr, doc: XmlDocPtr) {
    if doc.is_null() {
        return;
    }
    xml_ctxt_dump_doc_head(ctxt, doc);
    if (*ctxt).check == 0 {
        if !(*doc).name.is_null() {
            fprintf((*ctxt).output, c"name=".as_ptr());
            xml_ctxt_dump_string(ctxt, (*doc).name as _);
            fprintf((*ctxt).output, c"\n".as_ptr());
        }
        if let Some(version) = (*doc).version.as_deref() {
            fprintf((*ctxt).output, c"version=".as_ptr());
            let version = CString::new(version).unwrap();
            xml_ctxt_dump_string(ctxt, version.as_ptr() as *const u8);
            fprintf((*ctxt).output, c"\n".as_ptr());
        }
        if let Some(encoding) = (*doc).encoding.as_deref() {
            fprintf((*ctxt).output, c"encoding=".as_ptr());
            let encoding = CString::new(encoding).unwrap();
            xml_ctxt_dump_string(ctxt, encoding.as_ptr() as *const u8);
            fprintf((*ctxt).output, c"\n".as_ptr());
        }
        if let Some(url) = (*doc).url.as_deref() {
            fprintf((*ctxt).output, c"URL=".as_ptr());
            let url = CString::new(url).unwrap();
            xml_ctxt_dump_string(ctxt, url.as_ptr() as *const u8);
            fprintf((*ctxt).output, c"\n".as_ptr());
        }
        if (*doc).standalone != 0 {
            fprintf((*ctxt).output, c"standalone=true\n".as_ptr());
        }
    }
    if !(*doc).old_ns.is_null() {
        xml_ctxt_dump_namespace_list(ctxt, (*doc).old_ns);
    }
}

/**
 * xmlDebugDumpDocumentHead:
 * @output:  the FILE * for the output
 * @doc:  the document
 *
 * Dumps debug information concerning the document, not recursive
 */
pub unsafe extern "C" fn xml_debug_dump_document_head(mut output: *mut FILE, doc: XmlDocPtr) {
    let mut ctxt: XmlDebugCtxt = unsafe { zeroed() };

    extern "C" {
        static stdout: *mut FILE;
    }

    if output.is_null() {
        output = stdout;
    }
    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.options |= DUMP_TEXT_TYPE;
    ctxt.output = output;
    xml_ctxt_dump_document_head(addr_of_mut!(ctxt), doc);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/**
 * xmlCtxtDumpDocument:
 * @output:  the FILE * for the output
 * @doc:  the document
 *
 * Dumps debug information for the document, it's recursive
 */
unsafe extern "C" fn xml_ctxt_dump_document(ctxt: XmlDebugCtxtPtr, doc: XmlDocPtr) {
    if doc.is_null() {
        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"DOCUMENT.is_null() !\n".as_ptr());
        }
        return;
    }
    xml_ctxt_dump_document_head(ctxt, doc);
    if matches!(
        (*doc).typ,
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode
    ) && !(*doc).children.is_null()
    {
        (*ctxt).depth += 1;
        xml_ctxt_dump_node_list(ctxt, (*doc).children);
        (*ctxt).depth -= 1;
    }
}

/**
 * xmlDebugDumpDocument:
 * @output:  the FILE * for the output
 * @doc:  the document
 *
 * Dumps debug information for the document, it's recursive
 */
pub unsafe extern "C" fn xml_debug_dump_document(mut output: *mut FILE, doc: XmlDocPtr) {
    let mut ctxt: XmlDebugCtxt = unsafe { zeroed() };

    extern "C" {
        static stdout: *mut FILE;
    }

    if output.is_null() {
        output = stdout;
    }
    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.options |= DUMP_TEXT_TYPE;
    ctxt.output = output;
    xml_ctxt_dump_document(addr_of_mut!(ctxt), doc);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/**
 * xmlDebugDumpDTD:
 * @output:  the FILE * for the output
 * @dtd:  the DTD
 *
 * Dumps debug information for the DTD
 */
pub unsafe extern "C" fn xml_debug_dump_dtd(mut output: *mut FILE, dtd: XmlDtdPtr) {
    let mut ctxt: XmlDebugCtxt = unsafe { zeroed() };

    extern "C" {
        static stdout: *mut FILE;
    }

    if output.is_null() {
        output = stdout;
    }
    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.options |= DUMP_TEXT_TYPE;
    ctxt.output = output;
    xml_ctxt_dump_dtd(addr_of_mut!(ctxt), dtd);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

extern "C" fn xml_ctxt_dump_entity_callback(
    payload: *mut c_void,
    data: *mut c_void,
    _name: *const XmlChar,
) {
    let cur: XmlEntityPtr = payload as XmlEntityPtr;
    let ctxt: XmlDebugCtxtPtr = data as XmlDebugCtxtPtr;
    unsafe {
        if cur.is_null() {
            if (*ctxt).check == 0 {
                fprintf((*ctxt).output, c"Entity is NULL".as_ptr());
            }
            return;
        }
        if (*ctxt).check == 0 {
            fprintf(
                (*ctxt).output,
                c"%s : ".as_ptr(),
                (*cur).name.load(Ordering::Relaxed) as *mut c_char,
            );
            match (*cur).etype {
                Some(XmlEntityType::XmlInternalGeneralEntity) => {
                    fprintf((*ctxt).output, c"INTERNAL GENERAL, ".as_ptr());
                }
                Some(XmlEntityType::XmlExternalGeneralParsedEntity) => {
                    fprintf((*ctxt).output, c"EXTERNAL PARSED, ".as_ptr());
                }
                Some(XmlEntityType::XmlExternalGeneralUnparsedEntity) => {
                    fprintf((*ctxt).output, c"EXTERNAL UNPARSED, ".as_ptr());
                }
                Some(XmlEntityType::XmlInternalParameterEntity) => {
                    fprintf((*ctxt).output, c"INTERNAL PARAMETER, ".as_ptr());
                }
                Some(XmlEntityType::XmlExternalParameterEntity) => {
                    fprintf((*ctxt).output, c"EXTERNAL PARAMETER, ".as_ptr());
                }
                Some(e) => {
                    xml_debug_err2(
                        ctxt,
                        XmlParserErrors::XmlCheckEntityType,
                        c"Unknown entity type %d\n".as_ptr(),
                        e as i32,
                    );
                }
                _ => unreachable!(),
            }
            if !(*cur).external_id.load(Ordering::Relaxed).is_null() {
                fprintf(
                    (*ctxt).output,
                    c"ID \"%s\"".as_ptr(),
                    (*cur).external_id.load(Ordering::Relaxed) as *mut c_char,
                );
            }
            if !(*cur).system_id.load(Ordering::Relaxed).is_null() {
                fprintf(
                    (*ctxt).output,
                    c"SYSTEM \"%s\"".as_ptr(),
                    (*cur).system_id.load(Ordering::Relaxed) as *mut c_char,
                );
            }
            if !(*cur).orig.load(Ordering::Relaxed).is_null() {
                fprintf(
                    (*ctxt).output,
                    c"\n orig \"%s\"".as_ptr(),
                    (*cur).orig.load(Ordering::Relaxed) as *mut c_char,
                );
            }
            if (*cur).typ != XmlElementType::XmlElementNode
                && !(*cur).content.load(Ordering::Relaxed).is_null()
            {
                fprintf(
                    (*ctxt).output,
                    c"\n content \"%s\"".as_ptr(),
                    (*cur).content.load(Ordering::Relaxed) as *mut c_char,
                );
            }
            fprintf((*ctxt).output, c"\n".as_ptr());
        }
    }
}

/**
 * xmlCtxtDumpEntities:
 * @output:  the FILE * for the output
 * @doc:  the document
 *
 * Dumps debug information for all the entities in use by the document
 */
unsafe extern "C" fn xml_ctxt_dump_entities(ctxt: XmlDebugCtxtPtr, doc: XmlDocPtr) {
    if doc.is_null() {
        return;
    }
    xml_ctxt_dump_doc_head(ctxt, doc);
    if !(*doc).int_subset.is_null() && !(*(*doc).int_subset).entities.is_null() {
        let table: XmlEntitiesTablePtr = (*(*doc).int_subset).entities as XmlEntitiesTablePtr;

        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"Entities in internal subset\n".as_ptr());
        }
        xml_hash_scan(table, Some(xml_ctxt_dump_entity_callback), ctxt as _);
    } else {
        fprintf((*ctxt).output, c"No entities in internal subset\n".as_ptr());
    }
    if !(*doc).ext_subset.is_null() && !(*(*doc).ext_subset).entities.is_null() {
        let table: XmlEntitiesTablePtr = (*(*doc).ext_subset).entities as XmlEntitiesTablePtr;

        if (*ctxt).check == 0 {
            fprintf((*ctxt).output, c"Entities in external subset\n".as_ptr());
        }
        xml_hash_scan(table, Some(xml_ctxt_dump_entity_callback), ctxt as _);
    } else if (*ctxt).check == 0 {
        fprintf((*ctxt).output, c"No entities in external subset\n".as_ptr());
    }
}

/**
 * xmlDebugDumpEntities:
 * @output:  the FILE * for the output
 * @doc:  the document
 *
 * Dumps debug information for all the entities in use by the document
 */
pub unsafe extern "C" fn xml_debug_dump_entities(output: *mut FILE, doc: XmlDocPtr) {
    let mut ctxt: XmlDebugCtxt = unsafe { zeroed() };

    if output.is_null() {
        return;
    }
    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = output;
    xml_ctxt_dump_entities(addr_of_mut!(ctxt), doc);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
}

/****************************************************************
 *								*
 *			Checking routines			*
 *								*
 ****************************************************************/
/**
 * xmlDebugCheckDocument:
 * @output:  the FILE * for the output
 * @doc:  the document
 *
 * Check the document for potential content problems, and output
 * the errors to @output
 *
 * Returns the number of errors found
 */
pub unsafe extern "C" fn xml_debug_check_document(mut output: *mut FILE, doc: XmlDocPtr) -> i32 {
    let mut ctxt: XmlDebugCtxt = unsafe { zeroed() };

    extern "C" {
        static stdout: *mut FILE;
    }

    if output.is_null() {
        output = stdout;
    }
    xml_ctxt_dump_init_ctxt(addr_of_mut!(ctxt));
    ctxt.output = output;
    ctxt.check = 1;
    xml_ctxt_dump_document(addr_of_mut!(ctxt), doc);
    xml_ctxt_dump_clean_ctxt(addr_of_mut!(ctxt));
    ctxt.errors
}

/****************************************************************
 *								*
 *			XML shell helpers			*
 *								*
 ****************************************************************/
/**
 * xmlLsOneNode:
 * @output:  the FILE * for the output
 * @node:  the node to dump
 *
 * Dump to @output the type and name of @node.
 */
pub unsafe extern "C" fn xml_ls_one_node(output: *mut FILE, node: XmlNodePtr) {
    if output.is_null() {
        return;
    }
    if node.is_null() {
        fprintf(output, c"NULL\n".as_ptr());
        return;
    }
    match (*node).typ {
        XmlElementType::XmlElementNode => {
            fprintf(output, c"-".as_ptr());
        }
        XmlElementType::XmlAttributeNode => {
            fprintf(output, c"a".as_ptr());
        }
        XmlElementType::XmlTextNode => {
            fprintf(output, c"t".as_ptr());
        }
        XmlElementType::XmlCdataSectionNode => {
            fprintf(output, c"C".as_ptr());
        }
        XmlElementType::XmlEntityRefNode => {
            fprintf(output, c"e".as_ptr());
        }
        XmlElementType::XmlEntityNode => {
            fprintf(output, c"E".as_ptr());
        }
        XmlElementType::XmlPiNode => {
            fprintf(output, c"p".as_ptr());
        }
        XmlElementType::XmlCommentNode => {
            fprintf(output, c"c".as_ptr());
        }
        XmlElementType::XmlDocumentNode => {
            fprintf(output, c"d".as_ptr());
        }
        XmlElementType::XmlHtmlDocumentNode => {
            fprintf(output, c"h".as_ptr());
        }
        XmlElementType::XmlDocumentTypeNode => {
            fprintf(output, c"T".as_ptr());
        }
        XmlElementType::XmlDocumentFragNode => {
            fprintf(output, c"F".as_ptr());
        }
        XmlElementType::XmlNotationNode => {
            fprintf(output, c"N".as_ptr());
        }
        XmlElementType::XmlNamespaceDecl => {
            fprintf(output, c"n".as_ptr());
        }
        _ => {
            fprintf(output, c"?".as_ptr());
        }
    }
    if (*node).typ != XmlElementType::XmlNamespaceDecl {
        if !(*node).properties.is_null() {
            fprintf(output, c"a".as_ptr());
        } else {
            fprintf(output, c"-".as_ptr());
        }
        if !(*node).ns_def.is_null() {
            fprintf(output, c"n".as_ptr());
        } else {
            fprintf(output, c"-".as_ptr());
        }
    }

    fprintf(output, c" %8d ".as_ptr(), xml_ls_count_node(node));

    match (*node).typ {
        XmlElementType::XmlElementNode => {
            if !(*node).name.is_null() {
                if !(*node).ns.is_null() && !(*(*node).ns).prefix.load(Ordering::Relaxed).is_null()
                {
                    fprintf(
                        output,
                        c"%s:".as_ptr(),
                        (*(*node).ns).prefix.load(Ordering::Relaxed),
                    );
                }
                fprintf(output, c"%s".as_ptr(), (*node).name as *const c_char);
            }
        }
        XmlElementType::XmlAttributeNode => {
            if !(*node).name.is_null() {
                fprintf(output, c"%s".as_ptr(), (*node).name as *const c_char);
            }
        }
        XmlElementType::XmlTextNode => {
            if !(*node).content.is_null() {
                xml_debug_dump_string(output, (*node).content);
            }
        }
        XmlElementType::XmlCdataSectionNode => {}
        XmlElementType::XmlEntityRefNode => {
            if !(*node).name.is_null() {
                fprintf(output, c"%s".as_ptr(), (*node).name as *const c_char);
            }
        }
        XmlElementType::XmlEntityNode => {
            if !(*node).name.is_null() {
                fprintf(output, c"%s".as_ptr(), (*node).name as *const c_char);
            }
        }
        XmlElementType::XmlPiNode => {
            if !(*node).name.is_null() {
                fprintf(output, c"%s".as_ptr(), (*node).name as *const c_char);
            }
        }
        XmlElementType::XmlCommentNode => {}
        XmlElementType::XmlDocumentNode => {}
        XmlElementType::XmlHtmlDocumentNode => {}
        XmlElementType::XmlDocumentTypeNode => {}
        XmlElementType::XmlDocumentFragNode => {}
        XmlElementType::XmlNotationNode => {}
        XmlElementType::XmlNamespaceDecl => {
            let ns: XmlNsPtr = node as XmlNsPtr;

            if (*ns).prefix.load(Ordering::Relaxed).is_null() {
                fprintf(
                    output,
                    c"default -> %s".as_ptr(),
                    (*ns).href.load(Ordering::Relaxed) as *mut c_char,
                );
            } else {
                fprintf(
                    output,
                    c"%s -> %s".as_ptr(),
                    (*ns).prefix.load(Ordering::Relaxed) as *mut c_char,
                    (*ns).href.load(Ordering::Relaxed) as *mut c_char,
                );
            }
        }
        _ => {
            if !(*node).name.is_null() {
                fprintf(output, c"%s".as_ptr(), (*node).name as *const c_char);
            }
        }
    }
    fprintf(output, c"\n".as_ptr());
}

/**
 * xmlLsCountNode:
 * @node:  the node to count
 *
 * Count the children of @node.
 *
 * Returns the number of children of @node.
 */
pub unsafe extern "C" fn xml_ls_count_node(node: XmlNodePtr) -> i32 {
    let mut ret: i32 = 0;
    let mut list: XmlNodePtr = null_mut();

    if node.is_null() {
        return 0;
    }

    match (*node).typ {
        XmlElementType::XmlElementNode => {
            list = (*node).children;
        }
        XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode => {
            list = (*(node as XmlDocPtr)).children;
        }
        XmlElementType::XmlAttributeNode => {
            list = (*(node as XmlAttrPtr)).children;
        }
        XmlElementType::XmlTextNode
        | XmlElementType::XmlCdataSectionNode
        | XmlElementType::XmlPiNode
        | XmlElementType::XmlCommentNode => {
            if !(*node).content.is_null() {
                ret = xml_strlen((*node).content);
            }
        }
        XmlElementType::XmlEntityRefNode
        | XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlEntityNode
        | XmlElementType::XmlDocumentFragNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl
        | XmlElementType::XmlNamespaceDecl
        | XmlElementType::XmlXincludeStart
        | XmlElementType::XmlXincludeEnd => {
            ret = 1;
        }
        _ => unreachable!(),
    }
    while !list.is_null() {
        list = (*list).next;
        ret += 1;
    }
    ret
}

/**
 * xmlBoolToText:
 * @boolval: a bool to turn into text
 *
 * Convenient way to turn bool into text
 *
 * Returns a pointer to either "True" or "False"
 */
pub unsafe extern "C" fn xml_bool_to_text(boolval: i32) -> *const c_char {
    if boolval != 0 {
        c"True".as_ptr()
    } else {
        c"False".as_ptr()
    }
}

/****************************************************************
 *								*
 *	 The XML shell related structures and functions		*
 *								*
 ****************************************************************/
/**
 * xmlShellReadlineFunc:
 * @prompt:  a string prompt
 *
 * This is a generic signature for the XML shell input function.
 *
 * Returns a string which will be freed by the Shell.
 */
#[cfg(feature = "xpath")]
pub type XmlShellReadlineFunc = unsafe extern "C" fn(prompt: *mut c_char) -> *mut c_char;

/**
 * xmlShellCtxt:
 *
 * A debugging shell context.
 * TODO: add the defined function tables.
 */
#[cfg(feature = "xpath")]
pub type XmlShellCtxtPtr = *mut XmlShellCtxt;
#[cfg(feature = "xpath")]
#[repr(C)]
pub struct XmlShellCtxt {
    filename: *mut c_char,
    doc: XmlDocPtr,
    node: XmlNodePtr,
    pctxt: XmlXPathContextPtr,
    loaded: i32,
    output: *mut FILE,
    input: XmlShellReadlineFunc,
}

/**
 * xmlShellCmd:
 * @ctxt:  a shell context
 * @arg:  a string argument
 * @node:  a first node
 * @node2:  a second node
 *
 * This is a generic signature for the XML shell functions.
 *
 * Returns an int, negative returns indicating errors.
 */
#[cfg(feature = "xpath")]
pub type XmlShellCmd = unsafe extern "C" fn(
    ctxt: XmlShellCtxtPtr,
    arg: *mut c_char,
    node: XmlNodePtr,
    node2: XmlNodePtr,
) -> i32;

/**
 * xmlShellPrintXPathError:
 * @errorType: valid xpath error id
 * @arg: the argument that cause xpath to fail
 *
 * Print the xpath error to libxml default error channel
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_print_xpath_error(error_type: i32, mut arg: *const c_char) {
    use std::ffi::CStr;

    use crate::generic_error;

    let default_arg: *const c_char = c"Result".as_ptr();

    if arg.is_null() {
        arg = default_arg;
    }

    match XmlXPathObjectType::try_from(error_type) {
        Ok(XmlXPathObjectType::XpathUndefined) => {
            generic_error!("{}: no such node\n", CStr::from_ptr(arg).to_string_lossy());
        }
        Ok(XmlXPathObjectType::XpathBoolean) => {
            generic_error!("{} is a Boolean\n", CStr::from_ptr(arg).to_string_lossy());
        }
        Ok(XmlXPathObjectType::XpathNumber) => {
            generic_error!("{} is a number\n", CStr::from_ptr(arg).to_string_lossy());
        }
        Ok(XmlXPathObjectType::XpathString) => {
            generic_error!("{} is a string\n", CStr::from_ptr(arg).to_string_lossy());
        }
        #[cfg(feature = "libxml_xptr_locs")]
        Ok(XmlXPathObjectType::XpathPoint) => {
            generic_error!("{} is a point\n", CStr::from_ptr(arg).to_string_lossy());
        }
        #[cfg(feature = "libxml_xptr_locs")]
        Ok(XmlXPathObjectType::XpathRange) => {
            generic_error!("{} is a range\n", CStr::from_ptr(arg).to_string_lossy());
        }
        #[cfg(feature = "libxml_xptr_locs")]
        Ok(XmlXPathObjectType::XpathLocationset) => {
            generic_error!("{} is a range\n", CStr::from_ptr(arg).to_string_lossy());
        }
        Ok(XmlXPathObjectType::XpathUsers) => {
            generic_error!(
                "{} is user-defined\n",
                CStr::from_ptr(arg).to_string_lossy()
            );
        }
        Ok(XmlXPathObjectType::XpathXsltTree) => {
            generic_error!(
                "{} is an XSLT value tree\n",
                CStr::from_ptr(arg).to_string_lossy()
            );
        }
        _ => unreachable!(),
    }
}

/**
 * xmlShellPrintNodeCtxt:
 * @ctxt : a non-null shell context
 * @node : a non-null node to print to the output FILE
 *
 * Print node to the output FILE
 */
#[cfg(feature = "output")]
unsafe extern "C" fn xml_shell_print_node_ctxt(ctxt: XmlShellCtxtPtr, node: XmlNodePtr) {
    use crate::tree::xml_elem_dump;

    if !node.is_null() {
        return;
    }

    extern "C" {
        static stdout: *mut FILE;
    }

    let fp = if ctxt.is_null() {
        stdout
    } else {
        (*ctxt).output
    };

    if (*node).typ == XmlElementType::XmlDocumentNode {
        (*(node as XmlDocPtr)).dump_file(fp);
    } else if (*node).typ == XmlElementType::XmlAttributeNode {
        xml_debug_dump_attr_list(fp, node as XmlAttrPtr, 0);
    } else {
        xml_elem_dump(fp, (*node).doc, node);
    }

    fprintf(fp, c"\n".as_ptr());
}

/**
 * xmlShellPrintXPathResultCtxt:
 * @ctxt: a valid shell context
 * @list: a valid result generated by an xpath evaluation
 *
 * Prints result to the output FILE
 */
unsafe extern "C" fn xml_shell_print_xpath_result_ctxt(
    ctxt: XmlShellCtxtPtr,
    list: XmlXPathObjectPtr,
) {
    if !ctxt.is_null() {
        return;
    }

    if !list.is_null() {
        match (*list).typ {
            XmlXPathObjectType::XpathNodeset => {
                #[cfg(feature = "output")]
                if !(*list).nodesetval.is_null() {
                    for indx in 0..(*(*list).nodesetval).node_nr {
                        xml_shell_print_node_ctxt(
                            ctxt,
                            *(*(*list).nodesetval).node_tab.add(indx as usize),
                        );
                    }
                } else {
                    generic_error!("Empty node set\n");
                }
                #[cfg(not(feature = "output"))]
                {
                    generic_error!("Node set\n");
                }
            }
            XmlXPathObjectType::XpathBoolean => {
                generic_error!(
                    "Is a Boolean:{}\n",
                    CStr::from_ptr(xml_bool_to_text((*list).boolval)).to_string_lossy()
                );
            }
            XmlXPathObjectType::XpathNumber => {
                generic_error!("Is a number:{}\n", (*list).floatval);
            }
            XmlXPathObjectType::XpathString => {
                generic_error!(
                    "Is a string:{}\n",
                    CStr::from_ptr((*list).stringval as *const i8).to_string_lossy()
                );
            }
            _ => {
                xml_shell_print_xpath_error((*list).typ as i32, null_mut());
            }
        }
    }
}

/**
 * xmlShellPrintXPathResult:
 * @list: a valid result generated by an xpath evaluation
 *
 * Prints result to the output FILE
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_print_xpath_result(list: XmlXPathObjectPtr) {
    xml_shell_print_xpath_result_ctxt(null_mut(), list);
}

/**
 * xmlShellList:
 * @ctxt:  the shell context
 * @arg:  unused
 * @node:  a node
 * @node2:  unused
 *
 * Implements the XML shell function "ls"
 * Does an Unix like listing of the given node (like a directory)
 *
 * Returns 0
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_list(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    let mut cur: XmlNodePtr;
    if ctxt.is_null() {
        return 0;
    }
    if node.is_null() {
        fprintf((*ctxt).output, c"NULL\n".as_ptr());
        return 0;
    }
    if (*node).typ == XmlElementType::XmlDocumentNode
        || (*node).typ == XmlElementType::XmlHtmlDocumentNode
    {
        cur = (*(node as XmlDocPtr)).children;
    } else if (*node).typ == XmlElementType::XmlNamespaceDecl {
        xml_ls_one_node((*ctxt).output, node);
        return 0;
    } else if !(*node).children.is_null() {
        cur = (*node).children;
    } else {
        xml_ls_one_node((*ctxt).output, node);
        return 0;
    }
    while !cur.is_null() {
        xml_ls_one_node((*ctxt).output, cur);
        cur = (*cur).next;
    }
    0
}

/**
 * xmlShellBase:
 * @ctxt:  the shell context
 * @arg:  unused
 * @node:  a node
 * @node2:  unused
 *
 * Implements the XML shell function "base"
 * dumps the current XML base of the node
 *
 * Returns 0
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_base(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use crate::libxml::globals::xml_free;

    if ctxt.is_null() {
        return 0;
    }
    if node.is_null() {
        fprintf((*ctxt).output, c"NULL\n".as_ptr());
        return 0;
    }

    let base: *mut XmlChar = (*node).get_base((*node).doc);

    if base.is_null() {
        fprintf((*ctxt).output, c" No base found !!!\n".as_ptr());
    } else {
        fprintf((*ctxt).output, c"%s\n".as_ptr(), base);
        xml_free(base as _);
    }
    0
}

/**
 * xmlShellDir:
 * @ctxt:  the shell context
 * @arg:  unused
 * @node:  a node
 * @node2:  unused
 *
 * Implements the XML shell function "dir"
 * dumps information about the node (namespace, attributes, content).
 *
 * Returns 0
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_dir(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    if ctxt.is_null() {
        return 0;
    }
    if node.is_null() {
        fprintf((*ctxt).output, c"NULL\n".as_ptr());
        return 0;
    }
    if (*node).typ == XmlElementType::XmlDocumentNode
        || (*node).typ == XmlElementType::XmlHtmlDocumentNode
    {
        xml_debug_dump_document_head((*ctxt).output, node as XmlDocPtr);
    } else if (*node).typ == XmlElementType::XmlAttributeNode {
        xml_debug_dump_attr((*ctxt).output, node as XmlAttrPtr, 0);
    } else {
        xml_debug_dump_one_node((*ctxt).output, node, 0);
    }
    0
}

/**
 * xmlShellLoad:
 * @ctxt:  the shell context
 * @filename:  the file name
 * @node:  unused
 * @node2:  unused
 *
 * Implements the XML shell function "load"
 * loads a new document specified by the filename
 *
 * Returns 0 or -1 if loading failed
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_load(
    ctxt: XmlShellCtxtPtr,
    filename: *mut c_char,
    _node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use crate::libxml::{
        globals::xml_free,
        htmlparser::html_parse_file,
        parser::xml_read_file,
        uri::xml_canonic_path,
        xpath::{xml_xpath_free_context, xml_xpath_new_context},
    };

    use crate::tree::xml_free_doc;

    let doc: XmlDocPtr;
    let mut html: i32 = 0;

    if ctxt.is_null() || filename.is_null() {
        return -1;
    }
    if !(*ctxt).doc.is_null() {
        html = ((*(*ctxt).doc).typ == XmlElementType::XmlHtmlDocumentNode) as i32;
    }

    if html != 0 {
        #[cfg(feature = "html")]
        {
            doc = html_parse_file(filename, None);
        }
        #[cfg(not(feature = "html"))]
        {
            fprintf((*ctxt).output, c"HTML support not compiled in\n".as_ptr());
            doc = null_mut();
        }
    } else {
        doc = xml_read_file(filename, None, 0);
    }
    if !doc.is_null() {
        if (*ctxt).loaded == 1 {
            xml_free_doc((*ctxt).doc);
        }
        (*ctxt).loaded = 1;
        #[cfg(feature = "xpath")]
        {
            xml_xpath_free_context((*ctxt).pctxt);
        }
        xml_free((*ctxt).filename as _);
        (*ctxt).doc = doc;
        (*ctxt).node = doc as XmlNodePtr;
        #[cfg(feature = "xpath")]
        {
            (*ctxt).pctxt = xml_xpath_new_context(doc);
        }
        (*ctxt).filename = xml_canonic_path(filename as *mut XmlChar) as *mut c_char;
    } else {
        return -1;
    }
    0
}

/**
 * xmlShellPrintNode:
 * @node : a non-null node to print to the output FILE
 *
 * Print node to the output FILE
 */
#[cfg(all(feature = "xpath", feature = "output"))]
pub unsafe extern "C" fn xml_shell_print_node(node: XmlNodePtr) {
    xml_shell_print_node_ctxt(null_mut(), node);
}

/**
 * xmlShellCat:
 * @ctxt:  the shell context
 * @arg:  unused
 * @node:  a node
 * @node2:  unused
 *
 * Implements the XML shell function "cat"
 * dumps the serialization node content (XML or HTML).
 *
 * Returns 0
 */
#[cfg(all(feature = "xpath", feature = "output"))]
pub unsafe extern "C" fn xml_shell_cat(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use crate::{
        libxml::htmltree::{html_doc_dump, html_node_dump_file},
        tree::xml_elem_dump,
    };

    use super::htmlparser::HtmlDocPtr;

    if ctxt.is_null() {
        return 0;
    }
    if node.is_null() {
        fprintf((*ctxt).output, c"NULL\n".as_ptr());
        return 0;
    }
    if (*(*ctxt).doc).typ == XmlElementType::XmlHtmlDocumentNode {
        #[cfg(feature = "html")]
        if (*node).typ == XmlElementType::XmlHtmlDocumentNode {
            html_doc_dump((*ctxt).output, node as HtmlDocPtr);
        } else {
            html_node_dump_file((*ctxt).output, (*ctxt).doc, node);
        }
        #[cfg(not(feature = "html"))]
        if (*node).typ == XmlElementType::XmlDocumentNode {
            (*(node as XmlDocPtr)).dump_file((*ctxt).output);
        } else {
            xml_elem_dump((*ctxt).output, (*ctxt).doc, node);
        }
    } else if (*node).typ == XmlElementType::XmlDocumentNode {
        (*(node as XmlDocPtr)).dump_file((*ctxt).output);
    } else {
        xml_elem_dump((*ctxt).output, (*ctxt).doc, node);
    }
    fprintf((*ctxt).output, c"\n".as_ptr());
    0
}

/**
 * xmlShellWrite:
 * @ctxt:  the shell context
 * @filename:  the file name
 * @node:  a node in the tree
 * @node2:  unused
 *
 * Implements the XML shell function "write"
 * Write the current node to the filename, it saves the serialization
 * of the subtree under the @node specified
 *
 * Returns 0 or -1 in case of error
 */
#[cfg(all(feature = "xpath", feature = "output"))]
pub unsafe extern "C" fn xml_shell_write(
    ctxt: XmlShellCtxtPtr,
    filename: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use libc::{fclose, fopen};

    use crate::libxml::htmltree::html_save_file;
    use crate::tree::{xml_elem_dump, xml_save_file};

    if node.is_null() {
        return -1;
    }
    if filename.is_null() || *filename.add(0) == 0 {
        return -1;
    }
    match (*node).typ {
        XmlElementType::XmlDocumentNode => {
            if xml_save_file(filename as *mut c_char, (*ctxt).doc) < -1 {
                generic_error!(
                    "Failed to write to {}\n",
                    CStr::from_ptr(filename as *const i8).to_string_lossy()
                );
                return -1;
            }
        }
        XmlElementType::XmlHtmlDocumentNode => {
            #[cfg(feature = "html")]
            if html_save_file(filename as *mut c_char, (*ctxt).doc) < 0 {
                generic_error!(
                    "Failed to write to {}\n",
                    CStr::from_ptr(filename as *const i8).to_string_lossy()
                );
                return -1;
            }
            #[cfg(not(feature = "html"))]
            if xml_save_file(filename as *mut c_char, (*ctxt).doc) < -1 {
                generic_error!(
                    "Failed to write to {}\n",
                    CStr::from_ptr(filename as *const i8).to_string_lossy()
                );
                return -1;
            }
        }
        _ => {
            let f: *mut FILE = fopen(filename as *mut c_char, c"w".as_ptr());
            if f.is_null() {
                generic_error!(
                    "Failed to write to {}\n",
                    CStr::from_ptr(filename as *const i8).to_string_lossy()
                );
                return -1;
            }
            xml_elem_dump(f, (*ctxt).doc, node);
            fclose(f);
        }
    }
    0
}

/**
 * xmlShellSave:
 * @ctxt:  the shell context
 * @filename:  the file name (optional)
 * @node:  unused
 * @node2:  unused
 *
 * Implements the XML shell function "save"
 * Write the current document to the filename, or it's original name
 *
 * Returns 0 or -1 in case of error
 */
#[cfg(all(feature = "xpath", feature = "output"))]
pub unsafe extern "C" fn xml_shell_save(
    ctxt: XmlShellCtxtPtr,
    mut filename: *mut c_char,
    _node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use crate::libxml::htmltree::html_save_file;

    use crate::tree::xml_save_file;

    if ctxt.is_null() || (*ctxt).doc.is_null() {
        return -1;
    }
    if filename.is_null() || *filename.add(0) == 0 {
        filename = (*ctxt).filename;
    }
    if filename.is_null() {
        return -1;
    }
    match (*(*ctxt).doc).typ {
        XmlElementType::XmlDocumentNode => {
            if xml_save_file(filename as *mut c_char, (*ctxt).doc) < 0 {
                generic_error!(
                    "Failed to save to {}\n",
                    CStr::from_ptr(filename as *const i8).to_string_lossy()
                );
            }
        }
        XmlElementType::XmlHtmlDocumentNode => {
            #[cfg(feature = "html")]
            if html_save_file(filename as *mut c_char, (*ctxt).doc) < 0 {
                generic_error!(
                    "Failed to save to {}\n",
                    CStr::from_ptr(filename as *const i8).to_string_lossy()
                );
            }
            #[cfg(not(feature = "html"))]
            if (xml_save_file(filename as *mut c_char, (*ctxt).doc) < 0) {
                generic_error!(
                    "Failed to save to {}\n",
                    CStr::from_ptr(filename as *const i8).to_string_lossy()
                );
            }
        }
        _ => {
            generic_error!("To save to subparts of a document use the 'write' command\n");
            return -1;
        }
    }
    0
}

/**
 * xmlShellValidate:
 * @ctxt:  the shell context
 * @dtd:  the DTD URI (optional)
 * @node:  unused
 * @node2:  unused
 *
 * Implements the XML shell function "validate"
 * Validate the document, if a DTD path is provided, then the validation
 * is done against the given DTD.
 *
 * Returns 0 or -1 in case of error
 */
#[cfg(all(feature = "xpath", feature = "valid"))]
pub unsafe extern "C" fn xml_shell_validate(
    ctxt: XmlShellCtxtPtr,
    dtd: *mut c_char,
    _node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use std::mem::size_of_val;

    use libc::memset;

    use crate::{
        globals::GLOBAL_STATE,
        libxml::{
            parser::xml_parse_dtd,
            valid::{xml_validate_document, xml_validate_dtd},
        },
        tree::xml_free_dtd,
    };

    use super::valid::XmlValidCtxt;

    let mut vctxt: XmlValidCtxt = unsafe { zeroed() };
    let mut res: i32 = -1;

    if ctxt.is_null() || (*ctxt).doc.is_null() {
        return -1;
    }
    memset(addr_of_mut!(vctxt) as _, 0, size_of_val(&vctxt));
    vctxt.error = Some(GLOBAL_STATE.with_borrow(|state| state.generic_error));
    vctxt.warning = vctxt.error;

    if dtd.is_null() || *dtd.add(0) == 0 {
        res = xml_validate_document(addr_of_mut!(vctxt), (*ctxt).doc);
    } else {
        let subset: XmlDtdPtr = xml_parse_dtd(null_mut(), dtd as *mut XmlChar);
        if !subset.is_null() {
            res = xml_validate_dtd(addr_of_mut!(vctxt), (*ctxt).doc, subset);

            xml_free_dtd(subset);
        }
    }
    res
}

/**
 * xmlShellDu:
 * @ctxt:  the shell context
 * @arg:  unused
 * @tree:  a node defining a subtree
 * @node2:  unused
 *
 * Implements the XML shell function "du"
 * show the structure of the subtree under node @tree
 * If @tree is null, the command works on the current node.
 *
 * Returns 0 or -1 in case of error
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_du(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    tree: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    let mut node: XmlNodePtr;
    let mut indent: i32 = 0;

    if ctxt.is_null() {
        return -1;
    }

    if tree.is_null() {
        return -1;
    }
    node = tree;
    while !node.is_null() {
        if (*node).typ == XmlElementType::XmlDocumentNode
            || (*node).typ == XmlElementType::XmlHtmlDocumentNode
        {
            fprintf((*ctxt).output, c"/\n".as_ptr());
        } else if (*node).typ == XmlElementType::XmlElementNode {
            for _ in 0..indent {
                fprintf((*ctxt).output, c"  ".as_ptr());
            }
            if !(*node).ns.is_null() && !(*(*node).ns).prefix.load(Ordering::Relaxed).is_null() {
                fprintf(
                    (*ctxt).output,
                    c"%s:".as_ptr(),
                    (*(*node).ns).prefix.load(Ordering::Relaxed),
                );
            }
            fprintf((*ctxt).output, c"%s\n".as_ptr(), (*node).name);
        }

        /*
         * Browse the full subtree, deep first
         */

        if (*node).typ == XmlElementType::XmlDocumentNode
            || (*node).typ == XmlElementType::XmlHtmlDocumentNode
        {
            node = (*(node as XmlDocPtr)).children;
        } else if !(*node).children.is_null() && (*node).typ != XmlElementType::XmlEntityRefNode {
            /* deep first */
            node = (*node).children;
            indent += 1;
        } else if node != tree && !(*node).next.is_null() {
            /* then siblings */
            node = (*node).next;
        } else if node != tree {
            /* go up to parents->next if needed */
            while node != tree {
                if !(*node).parent.is_null() {
                    node = (*node).parent;
                    indent -= 1;
                }
                if node != tree && !(*node).next.is_null() {
                    node = (*node).next;
                    break;
                }
                if (*node).parent.is_null() {
                    node = null_mut();
                    break;
                }
                if node == tree {
                    node = null_mut();
                    break;
                }
            }
            /* exit condition */
            if node == tree {
                node = null_mut();
            }
        } else {
            node = null_mut();
        }
    }
    0
}

/**
 * xmlShellPwd:
 * @ctxt:  the shell context
 * @buffer:  the output buffer
 * @node:  a node
 * @node2:  unused
 *
 * Implements the XML shell function "pwd"
 * Show the full path from the root to the node, if needed building
 * thumblers when similar elements exists at a given ancestor level.
 * The output is compatible with XPath commands.
 *
 * Returns 0 or -1 in case of error
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell_pwd(
    _ctxt: XmlShellCtxtPtr,
    buffer: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use libc::snprintf;

    use crate::libxml::globals::xml_free;

    if node.is_null() || buffer.is_null() {
        return -1;
    }

    let path: *mut XmlChar = (*node).get_node_path();
    if path.is_null() {
        return -1;
    }

    /*
     * This test prevents buffer overflow, because this routine
     * is only called by xmlShell, in which the second argument is
     * 500 chars long.
     * It is a dirty hack before a cleaner solution is found.
     * Documentation should mention that the second argument must
     * be at least 500 chars long, and could be stripped if too long.
     */
    snprintf(buffer as _, 499, c"%s".as_ptr(), path);
    *buffer.add(499) = b'0' as _;
    xml_free(path as _);

    0
}

/**
 * xmlShellRNGValidate:
 * @ctxt:  the shell context
 * @schemas:  the path to the Relax-NG schemas
 * @node:  a node
 * @node2:  unused
 *
 * Implements the XML shell function "relaxng"
 * validating the instance against a Relax-NG schemas
 *
 * Returns 0
 */
#[cfg(feature = "schema")]
unsafe extern "C" fn xml_shell_rng_validate(
    sctxt: XmlShellCtxtPtr,
    schemas: *mut c_char,
    _node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use crate::{
        globals::GLOBAL_STATE,
        libxml::relaxng::{
            xml_relaxng_free, xml_relaxng_free_parser_ctxt, xml_relaxng_free_valid_ctxt,
            xml_relaxng_new_parser_ctxt, xml_relaxng_new_valid_ctxt, xml_relaxng_parse,
            xml_relaxng_set_parser_errors, xml_relaxng_set_valid_errors, xml_relaxng_validate_doc,
            XmlRelaxNGParserCtxtPtr, XmlRelaxNGValidCtxtPtr,
        },
    };

    use super::relaxng::XmlRelaxNGPtr;

    let ctxt: XmlRelaxNGParserCtxtPtr = xml_relaxng_new_parser_ctxt(schemas);
    let generic_error = GLOBAL_STATE.with_borrow(|state| state.generic_error);
    xml_relaxng_set_parser_errors(ctxt, Some(generic_error), Some(generic_error), None);
    let relaxngschemas: XmlRelaxNGPtr = xml_relaxng_parse(ctxt);
    xml_relaxng_free_parser_ctxt(ctxt);
    if relaxngschemas.is_null() {
        generic_error!(
            "Relax-NG schema {} failed to compile\n",
            CStr::from_ptr(schemas as *const i8).to_string_lossy()
        );
        return -1;
    }
    let vctxt: XmlRelaxNGValidCtxtPtr = xml_relaxng_new_valid_ctxt(relaxngschemas);
    xml_relaxng_set_valid_errors(vctxt, Some(generic_error), Some(generic_error), None);
    let ret: i32 = xml_relaxng_validate_doc(vctxt, (*sctxt).doc);

    match ret.cmp(&0) {
        std::cmp::Ordering::Equal => {
            let filename = CStr::from_ptr((*sctxt).filename).to_string_lossy();
            eprintln!("{filename} validates");
        }
        std::cmp::Ordering::Greater => {
            let filename = CStr::from_ptr((*sctxt).filename).to_string_lossy();
            eprintln!("{filename} fails to validate");
        }
        std::cmp::Ordering::Less => {
            let filename = CStr::from_ptr((*sctxt).filename).to_string_lossy();
            eprintln!("{filename} validation generated an internal error");
        }
    }
    xml_relaxng_free_valid_ctxt(vctxt);
    if !relaxngschemas.is_null() {
        xml_relaxng_free(relaxngschemas);
    }
    0
}

/**
 * xmlShellGrep:
 * @ctxt:  the shell context
 * @arg:  the string or regular expression to find
 * @node:  a node
 * @node2:  unused
 *
 * Implements the XML shell function "grep"
 * dumps information about the node (namespace, attributes, content).
 *
 * Returns 0
 */
unsafe extern "C" fn xml_shell_grep(
    ctxt: XmlShellCtxtPtr,
    arg: *mut c_char,
    mut node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    if ctxt.is_null() {
        return 0;
    }
    if node.is_null() {
        return 0;
    }
    if arg.is_null() {
        return 0;
    }
    // what does the following do... ?
    // #[cfg(feature = "regexp")]
    // if !xmlStrchr(arg as *mut xmlChar, b'?').is_null()
    //     || !xmlStrchr(arg as *mut xmlChar, b'*').is_null()
    //     || !xmlStrchr(arg as *mut xmlChar, b'.').is_null()
    //     || !xmlStrchr(arg as *mut xmlChar, b'[').is_null()
    // {}
    while !node.is_null() {
        if (*node).typ == XmlElementType::XmlCommentNode {
            if !xml_strstr((*node).content, arg as *mut XmlChar).is_null() {
                fprintf((*ctxt).output, c"%s : ".as_ptr(), (*node).get_node_path());
                xml_shell_list(ctxt, null_mut(), node, null_mut());
            }
        } else if (*node).typ == XmlElementType::XmlTextNode
            && !xml_strstr((*node).content, arg as *mut XmlChar).is_null()
        {
            fprintf(
                (*ctxt).output,
                c"%s : ".as_ptr(),
                (*(*node).parent).get_node_path(),
            );
            xml_shell_list(ctxt, null_mut(), (*node).parent, null_mut());
        }

        /*
         * Browse the full subtree, deep first
         */

        if (*node).typ == XmlElementType::XmlDocumentNode
            || (*node).typ == XmlElementType::XmlHtmlDocumentNode
        {
            node = (*(node as XmlDocPtr)).children;
        } else if !(*node).children.is_null() && (*node).typ != XmlElementType::XmlEntityRefNode {
            /* deep first */
            node = (*node).children;
        } else if !(*node).next.is_null() {
            /* then siblings */
            node = (*node).next;
        } else {
            /* go up to parents->next if needed */
            while !node.is_null() {
                if !(*node).parent.is_null() {
                    node = (*node).parent;
                }
                if !(*node).next.is_null() {
                    node = (*node).next;
                    break;
                }
                if (*node).parent.is_null() {
                    node = null_mut();
                    break;
                }
            }
        }
    }
    0
}

/**
 * xmlShellSetContent:
 * @ctxt:  the shell context
 * @value:  the content as a string
 * @node:  a node
 * @node2:  unused
 *
 * Implements the XML shell function "dir"
 * dumps information about the node (namespace, attributes, content).
 *
 * Returns 0
 */
unsafe extern "C" fn xml_shell_set_content(
    ctxt: XmlShellCtxtPtr,
    value: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    let mut results: XmlNodePtr = null_mut();

    if ctxt.is_null() {
        return 0;
    }
    if node.is_null() {
        fprintf((*ctxt).output, c"NULL\n".as_ptr());
        return 0;
    }
    if value.is_null() {
        fprintf((*ctxt).output, c"NULL\n".as_ptr());
        return 0;
    }

    let ret = xml_parse_in_node_context(
        node,
        CStr::from_ptr(value).to_bytes().to_vec(),
        0,
        addr_of_mut!(results),
    );
    if ret == XmlParserErrors::XmlErrOK {
        if !(*node).children.is_null() {
            xml_free_node_list((*node).children);
            (*node).children = null_mut();
            (*node).last = null_mut();
        }
        (*node).add_child_list(results);
    } else {
        fprintf((*ctxt).output, c"failed to parse content\n".as_ptr());
    }
    0
}

/**
 * xmlShellRegisterNamespace:
 * @ctxt:  the shell context
 * @arg:  a string in prefix=nsuri format
 * @node:  unused
 * @node2:  unused
 *
 * Implements the XML shell function "setns"
 * register/unregister a prefix=namespace pair
 * on the XPath context
 *
 * Returns 0 on success and a negative value otherwise.
 */
#[cfg(feature = "xpath")]
unsafe extern "C" fn xml_shell_register_namespace(
    ctxt: XmlShellCtxtPtr,
    arg: *mut c_char,
    _node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use crate::libxml::{xmlstring::xml_strdup, xpath_internals::xml_xpath_register_ns};

    use super::globals::xml_free;

    let mut prefix: *mut XmlChar;
    let mut href: *mut XmlChar;
    let mut next: *mut XmlChar;

    let ns_list_dup: *mut XmlChar = xml_strdup(arg as *mut XmlChar);
    next = ns_list_dup;
    while !next.is_null() {
        /* skip spaces */
        /*while ((*next) == b' ') next++;*/
        if (*next) == b'\0' {
            break;
        }

        /* find prefix */
        prefix = next;
        next = xml_strchr(next, b'=') as *mut XmlChar;
        if next.is_null() {
            fprintf((*ctxt).output, c"setns: prefix=[nsuri] required\n".as_ptr());
            xml_free(ns_list_dup as _);
            return -1;
        }
        *next = b'\0';
        next = next.add(1);

        /* find href */
        href = next;
        next = xml_strchr(next, b' ') as *mut XmlChar;
        if !next.is_null() {
            *next = b'\0';
            next = next.add(1);
        }

        /* do register namespace */
        if xml_xpath_register_ns((*ctxt).pctxt, prefix, href) != 0 {
            fprintf(
                (*ctxt).output,
                c"Error: unable to register NS with prefix=\"%s\" and href=\"%s\"\n".as_ptr(),
                prefix,
                href,
            );
            xml_free(ns_list_dup as _);
            return -1;
        }
    }

    xml_free(ns_list_dup as _);
    0
}

/**
 * xmlShellRegisterRootNamespaces:
 * @ctxt:  the shell context
 * @arg:  unused
 * @node:  the root element
 * @node2:  unused
 *
 * Implements the XML shell function "setrootns"
 * which registers all namespaces declarations found on the root element.
 *
 * Returns 0 on success and a negative value otherwise.
 */
#[cfg(feature = "xpath")]
unsafe extern "C" fn xml_shell_register_root_namespaces(
    ctxt: XmlShellCtxtPtr,
    _arg: *mut c_char,
    root: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    use super::xpath_internals::xml_xpath_register_ns;

    let mut ns: XmlNsPtr;

    if root.is_null()
        || (*root).typ != XmlElementType::XmlElementNode
        || (*root).ns_def.is_null()
        || ctxt.is_null()
        || (*ctxt).pctxt.is_null()
    {
        return -1;
    }
    ns = (*root).ns_def;
    while !ns.is_null() {
        if (*ns).prefix.load(Ordering::Relaxed).is_null() {
            xml_xpath_register_ns(
                (*ctxt).pctxt,
                c"defaultns".as_ptr() as _,
                (*ns).href.load(Ordering::Relaxed),
            );
        } else {
            xml_xpath_register_ns(
                (*ctxt).pctxt,
                (*ns).prefix.load(Ordering::Relaxed),
                (*ns).href.load(Ordering::Relaxed),
            );
        }
        ns = (*ns).next.load(Ordering::Relaxed);
    }
    0
}

/**
 * xmlShellSetBase:
 * @ctxt:  the shell context
 * @arg:  the new base
 * @node:  a node
 * @node2:  unused
 *
 * Implements the XML shell function "setbase"
 * change the current XML base of the node
 *
 * Returns 0
 */
#[cfg(feature = "tree")]
unsafe extern "C" fn xml_shell_set_base(
    _ctxt: XmlShellCtxtPtr,
    arg: *mut c_char,
    node: XmlNodePtr,
    _node2: XmlNodePtr,
) -> i32 {
    if node.is_null() {
        return 0;
    }
    (*node).set_base(arg as *mut XmlChar);
    0
}

/*
 * The Shell interface.
 */
/**
 * xmlShell:
 * @doc:  the initial document
 * @filename:  the output buffer
 * @input:  the line reading function
 * @output:  the output FILE*, defaults to stdout if NULL
 *
 * Implements the XML shell
 * This allow to load, validate, view, modify and save a document
 * using a environment similar to a UNIX commandline.
 */
#[cfg(feature = "xpath")]
pub unsafe extern "C" fn xml_shell(
    doc: XmlDocPtr,
    filename: *mut c_char,
    input: Option<XmlShellReadlineFunc>,
    mut output: *mut FILE,
) {
    use std::mem::size_of;

    use libc::{free, snprintf, sscanf, strcmp};

    use crate::{
        libxml::{
            globals::{xml_free, xml_malloc},
            xmlmemory::xml_mem_show,
            xmlstring::xml_strdup,
            xpath::{
                xml_xpath_eval, xml_xpath_free_context, xml_xpath_free_object,
                xml_xpath_new_context,
            },
            xpath_internals::xml_xpath_debug_dump_object,
        },
        tree::xml_free_doc,
    };

    let mut prompt: [u8; 500] = [0; 500];
    prompt[..c"/ > ".to_bytes().len()].copy_from_slice(c"/ > ".to_bytes());
    let mut cmdline: *mut c_char;
    let mut cur: *mut c_char;
    let mut command: [c_char; 100] = [0; 100];
    let mut arg: [c_char; 400] = [0; 400];
    let mut i: i32;
    let mut list: XmlXPathObjectPtr;

    if doc.is_null() {
        return;
    }
    if filename.is_null() {
        return;
    }
    if input.is_none() {
        return;
    }

    extern "C" {
        static stdout: *mut FILE;
    }

    if output.is_null() {
        output = stdout;
    }
    let ctxt: XmlShellCtxtPtr = xml_malloc(size_of::<XmlShellCtxt>()) as XmlShellCtxtPtr;
    if ctxt.is_null() {
        return;
    }
    (*ctxt).loaded = 0;
    (*ctxt).doc = doc;
    (*ctxt).input = input.unwrap();
    (*ctxt).output = output;
    (*ctxt).filename = xml_strdup(filename as *mut XmlChar) as *mut c_char;
    (*ctxt).node = (*ctxt).doc as XmlNodePtr;

    #[cfg(feature = "xpath")]
    {
        (*ctxt).pctxt = xml_xpath_new_context((*ctxt).doc);
        if (*ctxt).pctxt.is_null() {
            xml_free(ctxt as _);
            return;
        }
    }
    loop {
        if (*ctxt).node == (*ctxt).doc as XmlNodePtr {
            snprintf(
                prompt.as_mut_ptr() as _,
                prompt.len(),
                c"%s > ".as_ptr(),
                c"/".as_ptr(),
            );
        } else if !(*ctxt).node.is_null()
            && !(*(*ctxt).node).name.is_null()
            && !(*(*ctxt).node).ns.is_null()
            && !(*(*(*ctxt).node).ns)
                .prefix
                .load(Ordering::Relaxed)
                .is_null()
        {
            snprintf(
                prompt.as_mut_ptr() as _,
                prompt.len(),
                c"%s:%s > ".as_ptr(),
                (*(*(*ctxt).node).ns).prefix.load(Ordering::Relaxed),
                (*(*ctxt).node).name,
            );
        } else if !(*ctxt).node.is_null() && !(*(*ctxt).node).name.is_null() {
            snprintf(
                prompt.as_mut_ptr() as _,
                prompt.len(),
                c"%s > ".as_ptr(),
                (*(*ctxt).node).name,
            );
        } else {
            snprintf(prompt.as_mut_ptr() as _, prompt.len(), c"? > ".as_ptr());
        }
        prompt[prompt.len() - 1] = 0;

        /*
         * Get a new command line
         */
        cmdline = ((*ctxt).input)(prompt.as_mut_ptr() as _);
        if cmdline.is_null() {
            break;
        }

        /*
         * Parse the command itself
         */
        cur = cmdline;
        while *cur == b' ' as i8 || *cur == b'\t' as i8 {
            cur = cur.add(1);
        }
        i = 0;
        while *cur != b' ' as i8
            && *cur != b'\t' as i8
            && *cur != b'\n' as i8
            && *cur != b'\r' as i8
        {
            if *cur == 0 {
                break;
            }
            command[i as usize] = *cur;
            i += 1;
            cur = cur.add(1);
        }
        command[i as usize] = 0;
        if i == 0 {
            continue;
        }

        /*
         * Parse the argument
         */
        while *cur == b' ' as i8 || *cur == b'\t' as i8 {
            cur = cur.add(1);
        }
        i = 0;
        while *cur != b'\n' as i8 && *cur != b'\r' as i8 && *cur != 0 {
            if *cur == 0 {
                break;
            }
            arg[i as usize] = *cur;
            i += 1;
            cur = cur.add(1);
        }
        arg[i as usize] = 0;

        /*
         * start interpreting the command
         */
        if strcmp(command.as_mut_ptr(), c"exit".as_ptr()) == 0 {
            break;
        }
        if strcmp(command.as_mut_ptr(), c"quit".as_ptr()) == 0 {
            break;
        }
        if strcmp(command.as_mut_ptr(), c"bye".as_ptr()) == 0 {
            break;
        }
        if strcmp(command.as_mut_ptr(), c"help".as_ptr()) == 0 {
            fprintf(
                (*ctxt).output,
                c"\tbase         display XML base of the node\n".as_ptr(),
            );
            fprintf(
                (*ctxt).output,
                c"\tsetbase URI  change the XML base of the node\n".as_ptr(),
            );
            fprintf((*ctxt).output, c"\tbye          leave shell\n".as_ptr());
            fprintf(
                (*ctxt).output,
                c"\tcat [node]   display node or current node\n".as_ptr(),
            );
            fprintf(
                (*ctxt).output,
                c"\tcd [path]    change directory to path or to root\n".as_ptr(),
            );
            fprintf((*ctxt).output, c"\tdir [path]   dumps information about the node (namespace, attributes, content)\n".as_ptr());
            fprintf((*ctxt).output, c"\tdu [path]    show the structure of the subtree under path or the current node\n".as_ptr());
            fprintf((*ctxt).output, c"\texit         leave shell\n".as_ptr());
            fprintf(
                (*ctxt).output,
                c"\thelp         display this help\n".as_ptr(),
            );
            fprintf(
                (*ctxt).output,
                c"\tfree         display memory usage\n".as_ptr(),
            );
            fprintf(
                (*ctxt).output,
                c"\tload [name]  load a new document with name\n".as_ptr(),
            );
            fprintf(
                (*ctxt).output,
                c"\tls [path]    list contents of path or the current directory\n".as_ptr(),
            );
            fprintf((*ctxt).output, c"\tset xml_fragment replace the current node content with the fragment parsed in context\n".as_ptr());
            #[cfg(feature = "xpath")]
            {
                fprintf((*ctxt).output, c"\txpath expr   evaluate the XPath expression in that context and print the result\n".as_ptr());
                fprintf((*ctxt).output, c"\tsetns nsreg  register a namespace to a prefix in the XPath evaluation context\n".as_ptr());
                fprintf((*ctxt).output, c"\t             format for nsreg is: prefix=[nsuri] (i.e. prefix= unsets a prefix)\n".as_ptr());
                fprintf(
                    (*ctxt).output,
                    c"\tsetrootns    register all namespace found on the root element\n".as_ptr(),
                );
                fprintf(
                    (*ctxt).output,
                    c"\t             the default namespace if any uses 'defaultns' prefix\n"
                        .as_ptr(),
                );
            }
            fprintf(
                (*ctxt).output,
                c"\tpwd          display current working directory\n".as_ptr(),
            );
            fprintf(
                (*ctxt).output,
                c"\twhereis      display absolute path of [path] or current working directory\n"
                    .as_ptr(),
            );
            fprintf((*ctxt).output, c"\tquit         leave shell\n".as_ptr());
            #[cfg(feature = "output")]
            {
                fprintf(
                    (*ctxt).output,
                    c"\tsave [name]  save this document to name or the original name\n".as_ptr(),
                );
                fprintf(
                    (*ctxt).output,
                    c"\twrite [name] write the current node to the filename\n".as_ptr(),
                );
            }
            #[cfg(feature = "valid")]
            {
                fprintf(
                    (*ctxt).output,
                    c"\tvalidate     check the document for errors\n".as_ptr(),
                );
            }
            #[cfg(feature = "schema")]
            {
                fprintf(
                    (*ctxt).output,
                    c"\trelaxng rng  validate the document against the Relax-NG schemas\n".as_ptr(),
                );
            }
            fprintf(
                (*ctxt).output,
                c"\tgrep string  search for a string in the subtree\n".as_ptr(),
            );
        } else if {
            #[cfg(feature = "valid")]
            {
                strcmp(command.as_ptr(), c"validate".as_ptr()) == 0
            }
            #[cfg(not(feature = "valid"))]
            {
                false
            }
        } {
            #[cfg(feature = "valid")]
            {
                xml_shell_validate(ctxt, arg.as_mut_ptr(), null_mut(), null_mut());
            }
        } else if strcmp(command.as_ptr(), c"load".as_ptr()) == 0 {
            xml_shell_load(ctxt, arg.as_mut_ptr(), null_mut(), null_mut());
        } else if {
            #[cfg(feature = "schema")]
            {
                strcmp(command.as_ptr(), c"relaxng".as_ptr()) == 0
            }
            #[cfg(not(feature = "schema"))]
            {
                false
            }
        } {
            #[cfg(feature = "schema")]
            {
                xml_shell_rng_validate(ctxt, arg.as_mut_ptr(), null_mut(), null_mut());
            }
        } else if {
            #[cfg(feature = "output")]
            {
                strcmp(command.as_ptr(), c"save".as_ptr()) == 0
            }
            #[cfg(not(feature = "output"))]
            {
                false
            }
        } {
            #[cfg(feature = "output")]
            {
                xml_shell_save(ctxt, arg.as_mut_ptr(), null_mut(), null_mut());
            }
        } else if {
            #[cfg(feature = "output")]
            {
                strcmp(command.as_ptr(), c"write".as_ptr()) == 0
            }
            #[cfg(not(feature = "output"))]
            {
                false
            }
        } {
            #[cfg(feature = "output")]
            if arg[0] == 0 {
                generic_error!("Write command requires a filename argument\n");
            } else {
                xml_shell_write(ctxt, arg.as_mut_ptr(), (*ctxt).node, null_mut());
            }
        } else if strcmp(command.as_ptr(), c"grep".as_ptr()) == 0 {
            xml_shell_grep(ctxt, arg.as_mut_ptr(), (*ctxt).node, null_mut());
        } else if strcmp(command.as_ptr(), c"free".as_ptr()) == 0 {
            if arg[0] == 0 {
                xml_mem_show((*ctxt).output, 0);
            } else {
                let mut len: i32 = 0;

                sscanf(arg.as_mut_ptr(), c"%d".as_ptr(), addr_of_mut!(len));
                xml_mem_show((*ctxt).output, len);
            }
        } else if strcmp(command.as_ptr(), c"pwd".as_ptr()) == 0 {
            let mut dir: [c_char; 500] = [0; 500];

            if xml_shell_pwd(ctxt, dir.as_mut_ptr(), (*ctxt).node, null_mut()) == 0 {
                fprintf((*ctxt).output, c"%s\n".as_ptr(), dir);
            }
        } else if strcmp(command.as_ptr(), c"du".as_ptr()) == 0 {
            if arg[0] == 0 {
                xml_shell_du(ctxt, null_mut(), (*ctxt).node, null_mut());
            } else {
                (*(*ctxt).pctxt).node = (*ctxt).node;
                #[cfg(feature = "xpath")]
                {
                    (*(*ctxt).pctxt).node = (*ctxt).node;
                    list = xml_xpath_eval(arg.as_mut_ptr() as *mut XmlChar, (*ctxt).pctxt);
                }
                #[cfg(not(feature = "xpath"))]
                {
                    list = null_mut();
                }
                if !list.is_null() {
                    match (*list).typ {
                        XmlXPathObjectType::XpathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNodeset => {
                            if (*list).nodesetval.is_null() {
                                // break;
                            } else {
                                for indx in 0..(*(*list).nodesetval).node_nr {
                                    xml_shell_du(
                                        ctxt,
                                        null_mut(),
                                        *(*(*list).nodesetval).node_tab.add(indx as usize),
                                        null_mut(),
                                    );
                                }
                            }
                        }
                        XmlXPathObjectType::XpathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathXsltTree => {
                            generic_error!(
                                "{} is an XSLT value tree\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                    }
                    #[cfg(feature = "xpath")]
                    {
                        xml_xpath_free_object(list);
                    }
                } else {
                    generic_error!(
                        "{}: no such node\n",
                        CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                    );
                }
                (*(*ctxt).pctxt).node = null_mut();
            }
        } else if strcmp(command.as_ptr(), c"base".as_ptr()) == 0 {
            xml_shell_base(ctxt, null_mut(), (*ctxt).node, null_mut());
        } else if strcmp(command.as_ptr(), c"set".as_ptr()) == 0 {
            xml_shell_set_content(ctxt, arg.as_mut_ptr(), (*ctxt).node, null_mut());
        } else if {
            #[cfg(feature = "xpath")]
            {
                strcmp(command.as_ptr(), c"setns".as_ptr()) == 0
            }
            #[cfg(not(feature = "xpath"))]
            {
                false
            }
        } {
            #[cfg(feature = "xpath")]
            if arg[0] == 0 {
                generic_error!("setns: prefix=[nsuri] required\n");
            } else {
                xml_shell_register_namespace(ctxt, arg.as_mut_ptr(), null_mut(), null_mut());
            }
        } else if {
            #[cfg(feature = "xpath")]
            {
                strcmp(command.as_ptr(), c"setrootns".as_ptr()) == 0
            }
            #[cfg(not(feature = "xpath"))]
            {
                false
            }
        } {
            #[cfg(feature = "xpath")]
            {
                let root: XmlNodePtr = (*(*ctxt).doc).get_root_element();
                xml_shell_register_root_namespaces(ctxt, null_mut(), root, null_mut());
            }
        } else if {
            #[cfg(feature = "xpath")]
            {
                strcmp(command.as_ptr(), c"xpath".as_ptr()) == 0
            }
            #[cfg(not(feature = "xpath"))]
            {
                false
            }
        } {
            #[cfg(feature = "xpath")]
            if arg[0] == 0 {
                generic_error!("xpath: expression required\n");
            } else {
                (*(*ctxt).pctxt).node = (*ctxt).node;
                list = xml_xpath_eval(arg.as_mut_ptr() as *mut XmlChar, (*ctxt).pctxt);
                xml_xpath_debug_dump_object((*ctxt).output, list, 0);
                xml_xpath_free_object(list);
            }
        } else if {
            #[cfg(feature = "tree")]
            {
                strcmp(command.as_ptr(), c"setbase".as_ptr()) == 0
            }
            #[cfg(not(feature = "tree"))]
            {
                false
            }
        } {
            #[cfg(feature = "tree")]
            {
                xml_shell_set_base(ctxt, arg.as_mut_ptr(), (*ctxt).node, null_mut());
            }
        } else if strcmp(command.as_ptr(), c"ls".as_ptr()) == 0
            || strcmp(command.as_ptr(), c"dir".as_ptr()) == 0
        {
            let dir: i32 = (strcmp(command.as_ptr(), c"dir".as_ptr()) == 0) as i32;

            if arg[0] == 0 {
                if dir != 0 {
                    xml_shell_dir(ctxt, null_mut(), (*ctxt).node, null_mut());
                } else {
                    xml_shell_list(ctxt, null_mut(), (*ctxt).node, null_mut());
                }
            } else {
                (*(*ctxt).pctxt).node = (*ctxt).node;
                #[cfg(feature = "xpath")]
                {
                    (*(*ctxt).pctxt).node = (*ctxt).node;
                    list = xml_xpath_eval(arg.as_mut_ptr() as *mut XmlChar, (*ctxt).pctxt);
                }
                #[cfg(not(feature = "xpath"))]
                {
                    list = null_mut();
                }
                if !list.is_null() {
                    match (*list).typ {
                        XmlXPathObjectType::XpathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNodeset => {
                            if (*list).nodesetval.is_null() {
                                break;
                            }

                            for indx in 0..(*(*list).nodesetval).node_nr {
                                if dir != 0 {
                                    xml_shell_dir(
                                        ctxt,
                                        null_mut(),
                                        *(*(*list).nodesetval).node_tab.add(indx as usize),
                                        null_mut(),
                                    );
                                } else {
                                    xml_shell_list(
                                        ctxt,
                                        null_mut(),
                                        *(*(*list).nodesetval).node_tab.add(indx as usize),
                                        null_mut(),
                                    );
                                }
                            }
                        }
                        XmlXPathObjectType::XpathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathXsltTree => {
                            generic_error!(
                                "{} is an XSLT value tree\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                    }
                    #[cfg(feature = "xpath")]
                    {
                        xml_xpath_free_object(list);
                    }
                } else {
                    generic_error!(
                        "{}: no such node\n",
                        CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                    );
                }
                (*(*ctxt).pctxt).node = null_mut();
            }
        } else if strcmp(command.as_ptr(), c"whereis".as_ptr()) == 0 {
            let mut dir: [c_char; 500] = [0; 500];

            if arg[0] == 0 {
                if xml_shell_pwd(ctxt, dir.as_mut_ptr(), (*ctxt).node, null_mut()) == 0 {
                    fprintf((*ctxt).output, c"%s\n".as_ptr(), dir);
                }
            } else {
                (*(*ctxt).pctxt).node = (*ctxt).node;
                #[cfg(feature = "xpath")]
                {
                    list = xml_xpath_eval(arg.as_mut_ptr() as *mut XmlChar, (*ctxt).pctxt);
                }
                #[cfg(not(feature = "xpath"))]
                {
                    list = null_mut();
                }
                if !list.is_null() {
                    match (*list).typ {
                        XmlXPathObjectType::XpathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNodeset => {
                            if (*list).nodesetval.is_null() {
                                // break;
                            } else {
                                for indx in 0..(*(*list).nodesetval).node_nr {
                                    if xml_shell_pwd(
                                        ctxt,
                                        dir.as_mut_ptr(),
                                        *(*(*list).nodesetval).node_tab.add(indx as usize),
                                        null_mut(),
                                    ) == 0
                                    {
                                        fprintf((*ctxt).output, c"%s\n".as_ptr(), dir);
                                    }
                                }
                            }
                        }
                        XmlXPathObjectType::XpathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathXsltTree => {
                            generic_error!(
                                "{} is an XSLT value tree\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                    }
                    #[cfg(feature = "xpath")]
                    {
                        xml_xpath_free_object(list);
                    }
                } else {
                    generic_error!(
                        "{}: no such node\n",
                        CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                    );
                }
                (*(*ctxt).pctxt).node = null_mut();
            }
        } else if strcmp(command.as_ptr(), c"cd".as_ptr()) == 0 {
            if arg[0] == 0 {
                (*ctxt).node = (*ctxt).doc as XmlNodePtr;
            } else {
                #[cfg(feature = "xpath")]
                {
                    (*(*ctxt).pctxt).node = (*ctxt).node;
                    let l = strlen(arg.as_ptr());
                    if l >= 2 && arg[l - 1] == b'/' as _ {
                        arg[l - 1] = 0;
                    }
                    list = xml_xpath_eval(arg.as_mut_ptr() as *mut XmlChar, (*ctxt).pctxt);
                }
                #[cfg(not(feature = "xpath"))]
                {
                    list = null_mut();
                }
                if !list.is_null() {
                    match (*list).typ {
                        XmlXPathObjectType::XpathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNodeset => {
                            if !(*list).nodesetval.is_null() {
                                if (*(*list).nodesetval).node_nr == 1 {
                                    (*ctxt).node = *(*(*list).nodesetval).node_tab.add(0);
                                    if !(*ctxt).node.is_null()
                                        && ((*(*ctxt).node).typ == XmlElementType::XmlNamespaceDecl)
                                    {
                                        generic_error!("cannot cd to namespace\n");
                                        (*ctxt).node = null_mut();
                                    }
                                } else {
                                    generic_error!(
                                        "{} is a {} Node Set\n",
                                        CStr::from_ptr(arg.as_ptr()).to_string_lossy(),
                                        (*(*list).nodesetval).node_nr
                                    );
                                }
                            } else {
                                generic_error!(
                                    "{} is an empty Node Set\n",
                                    CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                                );
                            }
                        }
                        XmlXPathObjectType::XpathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathXsltTree => {
                            generic_error!(
                                "{} is an XSLT value tree\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                    }
                    #[cfg(feature = "xpath")]
                    {
                        xml_xpath_free_object(list);
                    }
                } else {
                    generic_error!(
                        "{}: no such node\n",
                        CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                    );
                }

                (*(*ctxt).pctxt).node = null_mut();
            }
        } else if {
            #[cfg(feature = "output")]
            {
                strcmp(command.as_ptr(), c"cat".as_ptr()) == 0
            }
            #[cfg(not(feature = "output"))]
            {
                false
            }
        } {
            #[cfg(feature = "output")]
            if arg[0] == 0 {
                xml_shell_cat(ctxt, null_mut(), (*ctxt).node, null_mut());
            } else {
                (*(*ctxt).pctxt).node = (*ctxt).node;
                #[cfg(feature = "xpath")]
                {
                    (*(*ctxt).pctxt).node = (*ctxt).node;
                    list = xml_xpath_eval(arg.as_mut_ptr() as *mut XmlChar, (*ctxt).pctxt);
                }
                #[cfg(not(feature = "xpath"))]
                {
                    list = null_mut();
                }
                if !list.is_null() {
                    match (*list).typ {
                        XmlXPathObjectType::XpathUndefined => {
                            generic_error!(
                                "{}: no such node\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNodeset => {
                            if (*list).nodesetval.is_null() {
                                // break;
                            } else {
                                for indx in 0..(*(*list).nodesetval).node_nr {
                                    if i > 0 {
                                        fprintf((*ctxt).output, c" -------\n".as_ptr());
                                    }
                                    xml_shell_cat(
                                        ctxt,
                                        null_mut(),
                                        *(*(*list).nodesetval).node_tab.add(indx as usize),
                                        null_mut(),
                                    );
                                }
                            }
                        }
                        XmlXPathObjectType::XpathBoolean => {
                            generic_error!(
                                "{} is a Boolean\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathNumber => {
                            generic_error!(
                                "{} is a number\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathString => {
                            generic_error!(
                                "{} is a string\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathPoint => {
                            generic_error!(
                                "{} is a point\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathRange => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        #[cfg(feature = "libxml_xptr_locs")]
                        XmlXPathObjectType::XpathLocationset => {
                            generic_error!(
                                "{} is a range\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathUsers => {
                            generic_error!(
                                "{} is user-defined\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                        XmlXPathObjectType::XpathXsltTree => {
                            generic_error!(
                                "{} is an XSLT value tree\n",
                                CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                            );
                        }
                    }
                    #[cfg(feature = "xpath")]
                    {
                        xml_xpath_free_object(list);
                    }
                } else {
                    generic_error!(
                        "{}: no such node\n",
                        CStr::from_ptr(arg.as_ptr()).to_string_lossy()
                    );
                }
                (*(*ctxt).pctxt).node = null_mut();
            }
        } else {
            generic_error!(
                "Unknown command {}\n",
                CStr::from_ptr(command.as_ptr()).to_string_lossy()
            );
        }
        free(cmdline as _); /* not xmlFree here ! */
        // cmdline = null_mut();
    }
    #[cfg(feature = "xpath")]
    {
        xml_xpath_free_context((*ctxt).pctxt);
    }
    if (*ctxt).loaded != 0 {
        xml_free_doc((*ctxt).doc);
    }
    if !(*ctxt).filename.is_null() {
        xml_free((*ctxt).filename as _);
    }
    xml_free(ctxt as _);
    if !cmdline.is_null() {
        free(cmdline as _); /* not xmlFree here ! */
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_bool_to_text() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_boolval in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let boolval = gen_int(n_boolval, 0);

                let ret_val = xml_bool_to_text(boolval);
                desret_const_char_ptr(ret_val);
                des_int(n_boolval, boolval, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlBoolToText",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlBoolToText()");
                    eprintln!(" {}", n_boolval);
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_check_document() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_debug_file_ptr(n_output, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    let ret_val = xml_debug_check_document(output, doc);
                    desret_int(ret_val);
                    des_debug_file_ptr(n_output, output, 0);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDebugCheckDocument",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDebugCheckDocument()"
                        );
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_attr() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_attr in 0..GEN_NB_XML_ATTR_PTR {
                    for n_depth in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let output = gen_debug_file_ptr(n_output, 0);
                        let attr = gen_xml_attr_ptr(n_attr, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_debug_dump_attr(output, attr, depth);
                        des_debug_file_ptr(n_output, output, 0);
                        des_xml_attr_ptr(n_attr, attr, 1);
                        des_int(n_depth, depth, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDebugDumpAttr",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlDebugDumpAttr()");
                            eprint!(" {}", n_output);
                            eprint!(" {}", n_attr);
                            eprintln!(" {}", n_depth);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_attr_list() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_attr in 0..GEN_NB_XML_ATTR_PTR {
                    for n_depth in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let output = gen_debug_file_ptr(n_output, 0);
                        let attr = gen_xml_attr_ptr(n_attr, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_debug_dump_attr_list(output, attr, depth);
                        des_debug_file_ptr(n_output, output, 0);
                        des_xml_attr_ptr(n_attr, attr, 1);
                        des_int(n_depth, depth, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDebugDumpAttrList",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlDebugDumpAttrList()"
                            );
                            eprint!(" {}", n_output);
                            eprint!(" {}", n_attr);
                            eprintln!(" {}", n_depth);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_dtd() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_dtd in 0..GEN_NB_XML_DTD_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_debug_file_ptr(n_output, 0);
                    let dtd = gen_xml_dtd_ptr(n_dtd, 1);

                    xml_debug_dump_dtd(output, dtd);
                    des_debug_file_ptr(n_output, output, 0);
                    des_xml_dtd_ptr(n_dtd, dtd, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDebugDumpDTD",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlDebugDumpDTD()");
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_dtd);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_document() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_debug_file_ptr(n_output, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    xml_debug_dump_document(output, doc);
                    des_debug_file_ptr(n_output, output, 0);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDebugDumpDocument",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDebugDumpDocument()"
                        );
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_document_head() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_debug_file_ptr(n_output, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    xml_debug_dump_document_head(output, doc);
                    des_debug_file_ptr(n_output, output, 0);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDebugDumpDocumentHead",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDebugDumpDocumentHead()"
                        );
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_entities() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_debug_file_ptr(n_output, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    xml_debug_dump_entities(output, doc);
                    des_debug_file_ptr(n_output, output, 0);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDebugDumpEntities",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDebugDumpEntities()"
                        );
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_node() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    for n_depth in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let output = gen_debug_file_ptr(n_output, 0);
                        let node = gen_xml_node_ptr(n_node, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_debug_dump_node(output, node, depth);
                        des_debug_file_ptr(n_output, output, 0);
                        des_xml_node_ptr(n_node, node, 1);
                        des_int(n_depth, depth, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDebugDumpNode",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlDebugDumpNode()");
                            eprint!(" {}", n_output);
                            eprint!(" {}", n_node);
                            eprintln!(" {}", n_depth);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_node_list() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    for n_depth in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let output = gen_debug_file_ptr(n_output, 0);
                        let node = gen_xml_node_ptr(n_node, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_debug_dump_node_list(output, node, depth);
                        des_debug_file_ptr(n_output, output, 0);
                        des_xml_node_ptr(n_node, node, 1);
                        des_int(n_depth, depth, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDebugDumpNodeList",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlDebugDumpNodeList()"
                            );
                            eprint!(" {}", n_output);
                            eprint!(" {}", n_node);
                            eprintln!(" {}", n_depth);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_one_node() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    for n_depth in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let output = gen_debug_file_ptr(n_output, 0);
                        let node = gen_xml_node_ptr(n_node, 1);
                        let depth = gen_int(n_depth, 2);

                        xml_debug_dump_one_node(output, node, depth);
                        des_debug_file_ptr(n_output, output, 0);
                        des_xml_node_ptr(n_node, node, 1);
                        des_int(n_depth, depth, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDebugDumpOneNode",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlDebugDumpOneNode()"
                            );
                            eprint!(" {}", n_output);
                            eprint!(" {}", n_node);
                            eprintln!(" {}", n_depth);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_debug_dump_string() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_debug_file_ptr(n_output, 0);
                    let str = gen_const_xml_char_ptr(n_str, 1);

                    xml_debug_dump_string(output, str);
                    des_debug_file_ptr(n_output, output, 0);
                    des_const_xml_char_ptr(n_str, str, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDebugDumpString",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlDebugDumpString()"
                        );
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_str);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_ls_count_node() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_XML_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let node = gen_xml_node_ptr(n_node, 0);

                let ret_val = xml_ls_count_node(node);
                desret_int(ret_val);
                des_xml_node_ptr(n_node, node, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlLsCountNode",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlLsCountNode()");
                    eprintln!(" {}", n_node);
                }
            }
        }
    }

    #[test]
    fn test_xml_ls_one_node() {
        #[cfg(feature = "libxml_debug")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_DEBUG_FILE_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_debug_file_ptr(n_output, 0);
                    let node = gen_xml_node_ptr(n_node, 1);

                    xml_ls_one_node(output, node);
                    des_debug_file_ptr(n_output, output, 0);
                    des_xml_node_ptr(n_node, node, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlLsOneNode",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlLsOneNode()");
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_node);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell() {

        /* missing type support */
    }

    #[test]
    fn test_xml_shell_base() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_arg in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let arg = gen_char_ptr(n_arg, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_base(ctxt, arg, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_arg, arg, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellBase",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellBase()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_arg);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_cat() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_arg in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let arg = gen_char_ptr(n_arg, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_cat(ctxt, arg, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_arg, arg, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellCat",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellCat()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_arg);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_dir() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_arg in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let arg = gen_char_ptr(n_arg, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_dir(ctxt, arg, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_arg, arg, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellDir",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellDir()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_arg);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_du() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_arg in 0..GEN_NB_CHAR_PTR {
                    for n_tree in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let arg = gen_char_ptr(n_arg, 1);
                            let tree = gen_xml_node_ptr(n_tree, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_du(ctxt, arg, tree, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_arg, arg, 1);
                            des_xml_node_ptr(n_tree, tree, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellDu",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellDu()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_arg);
                                eprint!(" {}", n_tree);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_list() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_arg in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let arg = gen_char_ptr(n_arg, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_list(ctxt, arg, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_arg, arg, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellList",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellList()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_arg);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_load() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_filename in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let filename = gen_char_ptr(n_filename, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_load(ctxt, filename, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_filename, filename, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellLoad",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellLoad()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_filename);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_print_xpath_result() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_list in 0..GEN_NB_XML_XPATH_OBJECT_PTR {
                let mem_base = xml_mem_blocks();
                let list = gen_xml_xpath_object_ptr(n_list, 0);

                xml_shell_print_xpath_result(list);
                des_xml_xpath_object_ptr(n_list, list, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlShellPrintXPathResult",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlShellPrintXPathResult()"
                    );
                    eprintln!(" {}", n_list);
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_pwd() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_buffer in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let buffer = gen_char_ptr(n_buffer, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_pwd(ctxt, buffer, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_buffer, buffer, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellPwd",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellPwd()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_buffer);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_save() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_filename in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let filename = gen_char_ptr(n_filename, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_save(ctxt, filename, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_filename, filename, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellSave",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellSave()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_filename);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_validate() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath", feature = "valid"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_dtd in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let dtd = gen_char_ptr(n_dtd, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_validate(ctxt, dtd, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_dtd, dtd, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellValidate",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlShellValidate()"
                                );
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_dtd);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_shell_write() {
        #[cfg(all(feature = "libxml_debug", feature = "xpath", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SHELL_CTXT_PTR {
                for n_filename in 0..GEN_NB_CHAR_PTR {
                    for n_node in 0..GEN_NB_XML_NODE_PTR {
                        for n_node2 in 0..GEN_NB_XML_NODE_PTR {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_shell_ctxt_ptr(n_ctxt, 0);
                            let filename = gen_char_ptr(n_filename, 1);
                            let node = gen_xml_node_ptr(n_node, 2);
                            let node2 = gen_xml_node_ptr(n_node2, 3);

                            let ret_val = xml_shell_write(ctxt, filename, node, node2);
                            desret_int(ret_val);
                            des_xml_shell_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_char_ptr(n_filename, filename, 1);
                            des_xml_node_ptr(n_node, node, 2);
                            des_xml_node_ptr(n_node2, node2, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlShellWrite",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlShellWrite()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_filename);
                                eprint!(" {}", n_node);
                                eprintln!(" {}", n_node2);
                            }
                        }
                    }
                }
            }
        }
    }
}

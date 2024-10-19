//! Provide methods and data structures for Canonical XML and Exclusive XML Canonicalization.  
//! This module is based on `libxml/c14n.h`, `c14n.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    any::type_name,
    ffi::{c_char, c_int},
    mem::{size_of, size_of_val, zeroed},
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::Ordering,
};

use libc::{memcpy, memset};

use crate::{__xml_raise_error, private::buf::xml_buf_write_quoted_string};

use super::{
    globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
    list::{
        xml_list_create, xml_list_delete, xml_list_insert, xml_list_search, xml_list_walk,
        XmlListPtr,
    },
    tree::{
        xml_free_prop_list, xml_has_ns_prop, xml_new_ns_prop, xml_node_list_get_string,
        xml_search_ns, XmlAttrPtr, XmlDocPtr, XmlElementType, XmlNodePtr, XmlNs, XmlNsPtr,
        XML_XML_NAMESPACE,
    },
    uri::{xml_build_uri, xml_free_uri, xml_parse_uri, XmlURIPtr},
    xml_io::{
        xml_alloc_output_buffer, xml_output_buffer_close, xml_output_buffer_create_filename,
        xml_output_buffer_flush, xml_output_buffer_write_string, XmlOutputBufferPtr,
    },
    xmlerror::XmlParserErrors,
    xmlstring::{xml_str_equal, xml_strcat, xml_strcmp, xml_strlen, xml_strndup, XmlChar},
    xpath::XmlNodeSetPtr,
    xpath_internals::xml_xpath_node_set_contains,
};

/*
 * xmlC14NMode:
 *
 * Predefined values for C14N modes
 *
 */
#[repr(C)]
pub enum XmlC14NMode {
    XmlC14N1_0 = 0,          /* Original C14N 1.0 spec */
    XmlC14NExclusive1_0 = 1, /* Exclusive C14N 1.0 spec */
    XmlC14N1_1 = 2,          /* C14N 1.1 spec */
}

impl TryFrom<i32> for XmlC14NMode {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XmlC14N1_0 as i32 {
            Ok(Self::XmlC14N1_0)
        } else if value == Self::XmlC14NExclusive1_0 as i32 {
            Ok(Self::XmlC14NExclusive1_0)
        } else if value == Self::XmlC14N1_1 as i32 {
            Ok(Self::XmlC14N1_1)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to {}",
                type_name::<Self>()
            ))
        }
    }
}

#[repr(C)]
pub enum XmlC14NPosition {
    XmlC14NBeforeDocumentElement = 0,
    XmlC14NInsideDocumentElement = 1,
    XmlC14NAfterDocumentElement = 2,
}

#[repr(C)]
pub struct XmlC14NVisibleNsStack {
    ns_cur_end: c_int,         /* number of nodes in the set */
    ns_prev_start: c_int,      /* the beginning of the stack for previous visible node */
    ns_prev_end: c_int,        /* the end of the stack for previous visible node */
    ns_max: c_int,             /* size of the array as allocated */
    ns_tab: *mut XmlNsPtr,     /* array of ns in no particular order */
    node_tab: *mut XmlNodePtr, /* array of nodes in no particular order */
}
pub type XmlC14NVisibleNsStackPtr = *mut XmlC14NVisibleNsStack;

#[repr(C)]
pub struct XmlC14NCtx {
    /* input parameters */
    doc: XmlDocPtr,
    is_visible_callback: Option<XmlC14NIsVisibleCallback>,
    user_data: *mut c_void,
    with_comments: c_int,
    buf: XmlOutputBufferPtr,

    /* position in the XML document */
    pos: XmlC14NPosition,
    parent_is_doc: c_int,
    ns_rendered: XmlC14NVisibleNsStackPtr,

    /* C14N mode */
    mode: XmlC14NMode,

    /* exclusive canonicalization */
    inclusive_ns_prefixes: *mut *mut XmlChar,

    /* error number */
    error: c_int,
}

pub type XmlC14NCtxPtr = *mut XmlC14NCtx;
#[repr(C)]
pub enum XmlC14NNormalizationMode {
    XmlC14NNormalizeAttr = 0,
    XmlC14NNormalizeComment = 1,
    XmlC14NNormalizePI = 2,
    XmlC14NNormalizeText = 3,
}

unsafe extern "C" fn xml_c14n_is_node_in_nodeset(
    user_data: *mut c_void,
    node: XmlNodePtr,
    parent: XmlNodePtr,
) -> c_int {
    let nodes: XmlNodeSetPtr = user_data as XmlNodeSetPtr;
    if !nodes.is_null() && !node.is_null() {
        if !matches!((*node).typ, XmlElementType::XmlNamespaceDecl) {
            return xml_xpath_node_set_contains(nodes, node);
        } else {
            let mut ns: XmlNs = unsafe { zeroed() };

            memcpy(addr_of_mut!(ns) as _, node as _, size_of_val(&ns));

            /* this is a libxml hack! check xpath.c for details */
            if !parent.is_null() && matches!((*parent).typ, XmlElementType::XmlAttributeNode) {
                ns.next
                    .store((*parent).parent as XmlNsPtr, Ordering::Relaxed);
            } else {
                ns.next.store(parent as XmlNsPtr, Ordering::Relaxed);
            }

            /*
             * If the input is an XPath node-set, then the node-set must explicitly
             * contain every node to be rendered to the canonical form.
             */
            return xml_xpath_node_set_contains(nodes, addr_of_mut!(ns) as XmlNodePtr);
        }
    }
    1
}

/**
 * xmlC14NDocSaveTo:
 * @doc:        the XML document for canonization
 * @nodes:        the nodes set to be included in the canonized image
 *        or NULL if all document nodes should be included
 * @mode:        the c14n mode (see @xmlC14NMode)
 * @inclusive_ns_prefixes: the list of inclusive namespace prefixes
 *            ended with a NULL or NULL if there is no
 *            inclusive namespaces (only for exclusive
 *            canonicalization, ignored otherwise)
 * @with_comments:    include comments in the result (!=0) or not (==0)
 * @buf:        the output buffer to store canonical XML; this
 *            buffer MUST have encoder==NULL because C14N requires
 *            UTF-8 output
 *
 * Dumps the canonized image of given XML document into the provided buffer.
 * For details see "Canonical XML" (http://www.w3.org/TR/xml-c14n) or
 * "Exclusive XML Canonicalization" (http://www.w3.org/TR/xml-exc-c14n)
 *
 * Returns non-negative value on success or a negative value on fail
 */
pub unsafe extern "C" fn xml_c14n_doc_save_to(
    doc: XmlDocPtr,
    nodes: XmlNodeSetPtr,
    mode: c_int,
    /* a xmlC14NMode */ inclusive_ns_prefixes: *mut *mut XmlChar,
    with_comments: c_int,
    buf: XmlOutputBufferPtr,
) -> c_int {
    xml_c14n_execute(
        doc,
        xml_c14n_is_node_in_nodeset,
        nodes as _,
        mode,
        inclusive_ns_prefixes,
        with_comments,
        buf,
    )
}

/**
 * xmlC14NErrParam:
 * @extra:  extra information
 *
 * Handle a redefinition of param error
 */
unsafe extern "C" fn xml_c14n_err_param(extra: *const c_char) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromC14N,
        XmlParserErrors::XmlErrInternalError,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        extra,
        null_mut(),
        None,
        0,
        0,
        c"Invalid parameter : %s\n".as_ptr() as _,
        extra
    );
}

/**
 * xmlC14NErrMemory:
 * @extra:  extra information
 *
 * Handle a redefinition of memory error
 */
unsafe extern "C" fn xml_c14n_err_memory(extra: *const c_char) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromC14N,
        XmlParserErrors::XmlErrNoMemory,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        extra,
        null_mut(),
        None,
        0,
        0,
        c"Memory allocation failed : %s\n".as_ptr() as _,
        extra
    );
}

/**
 * xmlC14NErrInternal:
 * @extra:  extra information
 *
 * Handle a redefinition of internal error
 */
unsafe extern "C" fn xml_c14n_err_internal(extra: *const c_char) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromC14N,
        XmlParserErrors::XmlErrInternalError,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        extra,
        null_mut(),
        None,
        0,
        0,
        c"Internal error : %s\n".as_ptr() as _,
        extra
    );
}

/**
 * xmlC14NDocDumpMemory:
 * @doc:        the XML document for canonization
 * @nodes:        the nodes set to be included in the canonized image
 *        or NULL if all document nodes should be included
 * @mode:        the c14n mode (see @xmlC14NMode)
 * @inclusive_ns_prefixes: the list of inclusive namespace prefixes
 *            ended with a NULL or NULL if there is no
 *            inclusive namespaces (only for exclusive
 *            canonicalization, ignored otherwise)
 * @with_comments:    include comments in the result (!=0) or not (==0)
 * @doc_txt_ptr:    the memory pointer for allocated canonical XML text;
 *            the caller of this functions is responsible for calling
 *            xmlFree() to free allocated memo as _ry
 *
 * Dumps the canonized image of given XML document into memory.
 * For details see "Canonical XML" (http://www.w3.org/TR/xml-c14n) or
 * "Exclusive XML Canonicalization" (http://www.w3.org/TR/xml-exc-c14n)
 *
 * Returns the number of bytes written on success or a negative value on fail
 */
pub unsafe extern "C" fn xml_c14n_doc_dump_memory(
    doc: XmlDocPtr,
    nodes: XmlNodeSetPtr,
    mode: c_int,
    /* a xmlC14NMode */ inclusive_ns_prefixes: *mut *mut XmlChar,
    with_comments: c_int,
    doc_txt_ptr: *mut *mut XmlChar,
) -> c_int {
    let mut ret: c_int;

    if doc_txt_ptr.is_null() {
        xml_c14n_err_param(c"dumping doc to memory".as_ptr() as _);
        return -1;
    }

    *doc_txt_ptr = null_mut();

    /*
     * create memory buffer with UTF8 (default) encoding
     */
    let buf: XmlOutputBufferPtr = xml_alloc_output_buffer(None);
    if buf.is_null() {
        xml_c14n_err_memory(c"creating output buffer".as_ptr() as _);
        return -1;
    }

    /*
     * canonize document and write to buffer
     */
    ret = xml_c14n_doc_save_to(doc, nodes, mode, inclusive_ns_prefixes, with_comments, buf);
    if ret < 0 {
        xml_c14n_err_internal(c"saving doc to output buffer".as_ptr() as _);
        xml_output_buffer_close(buf);
        return -1;
    }

    ret = (*buf).buffer.map_or(0, |buf| buf.len() as i32);
    if ret >= 0 {
        *doc_txt_ptr = xml_strndup(
            (*buf).buffer.map_or(null(), |buf| buf.as_ref().as_ptr()),
            ret,
        );
    }
    xml_output_buffer_close(buf);

    if (*doc_txt_ptr).is_null() && ret >= 0 {
        xml_c14n_err_memory(c"copying canonicalized document".as_ptr() as _);
        return -1;
    }
    ret
}

/**
 * xmlC14NDocSave:
 * @doc:        the XML document for canonization
 * @nodes:        the nodes set to be included in the canonized image
 *        or NULL if all document nodes should be included
 * @mode:        the c14n mode (see @xmlC14NMode)
 * @inclusive_ns_prefixes: the list of inclusive namespace prefixes
 *            ended with a NULL or NULL if there is no
 *            inclusive namespaces (only for exclusive
 *            canonicalization, ignored otherwise)
 * @with_comments:    include comments in the result (!=0) or not (==0)
 * @filename:        the filename to store canonical XML image
 * @compression:    the compression level (zlib required):
 *                -1 - libxml default,
 *                 0 - uncompressed,
 *                >0 - compression level
 *
 * Dumps the canonized image of given XML document into the file.
 * For details see "Canonical XML" (http://www.w3.org/TR/xml-c14n) or
 * "Exclusive XML Canonicalization" (http://www.w3.org/TR/xml-exc-c14n)
 *
 * Returns the number of bytes written success or a negative value on fail
 */
pub unsafe extern "C" fn xml_c14n_doc_save(
    doc: XmlDocPtr,
    nodes: XmlNodeSetPtr,
    mode: c_int,
    /* a xmlC14NMode */ inclusive_ns_prefixes: *mut *mut XmlChar,
    with_comments: c_int,
    filename: *const c_char,
    compression: c_int,
) -> c_int {
    let mut ret: c_int;

    if filename.is_null() {
        xml_c14n_err_param(c"saving doc".as_ptr() as _);
        return -1;
    }
    // #ifdef LIBXML_ZLIB_ENABLED
    //     if (compression < 0)
    //         compression = xmlGetCompressMode();
    // #endif

    /*
     * save the content to a temp buffer, use default UTF8 encoding.
     */
    let buf: XmlOutputBufferPtr = xml_output_buffer_create_filename(filename, None, compression);
    if buf.is_null() {
        xml_c14n_err_internal(c"creating temporary filename".as_ptr() as _);
        return -1;
    }

    /*
     * canonize document and write to buffer
     */
    ret = xml_c14n_doc_save_to(doc, nodes, mode, inclusive_ns_prefixes, with_comments, buf);
    if ret < 0 {
        xml_c14n_err_internal(c"canonize document to buffer".as_ptr() as _);
        xml_output_buffer_close(buf);
        return -1;
    }

    /*
     * get the numbers of bytes written
     */
    ret = xml_output_buffer_close(buf);
    ret
}

/**
 * This is the core C14N function
 */
/**
 * xmlC14NIsVisibleCallback:
 * @user_data: user data
 * @node: the current node
 * @parent: the parent node
 *
 * Signature for a C14N callback on visible nodes
 *
 * Returns 1 if the node should be included
 */
pub type XmlC14NIsVisibleCallback =
    unsafe extern "C" fn(user_data: *mut c_void, node: XmlNodePtr, parent: XmlNodePtr) -> c_int;

/**
 * xmlC14NErr:
 * @ctxt:  a C14N evaluation context
 * @node:  the context node
 * @error:  the error code
 * @msg:  the message
 * @extra:  extra information
 *
 * Handle a redefinition of attribute error
 */
unsafe extern "C" fn xml_c14n_err(
    ctxt: XmlC14NCtxPtr,
    node: XmlNodePtr,
    error: XmlParserErrors,
    msg: *const c_char,
) {
    if !ctxt.is_null() {
        (*ctxt).error = error as i32;
    }
    __xml_raise_error!(
        None,
        None,
        None,
        ctxt as _,
        node as _,
        XmlErrorDomain::XmlFromC14N,
        error,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        null_mut(),
        null_mut(),
        None,
        0,
        0,
        c"%s".as_ptr() as _,
        msg
    );
}

unsafe extern "C" fn xml_c14n_visible_ns_stack_create() -> XmlC14NVisibleNsStackPtr {
    let ret: XmlC14NVisibleNsStackPtr = xml_malloc(size_of::<XmlC14NVisibleNsStack>()) as _;
    if ret.is_null() {
        xml_c14n_err_memory(c"creating namespaces stack".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlC14NVisibleNsStack>());
    ret
}

unsafe extern "C" fn xml_c14n_visible_ns_stack_destroy(cur: XmlC14NVisibleNsStackPtr) {
    if cur.is_null() {
        xml_c14n_err_param(c"destroying namespaces stack".as_ptr() as _);
        return;
    }
    if !(*cur).ns_tab.is_null() {
        memset(
            (*cur).ns_tab as _,
            0,
            (*cur).ns_max as usize * size_of::<XmlNsPtr>(),
        );
        xml_free((*cur).ns_tab as _);
    }
    if !(*cur).node_tab.is_null() {
        memset(
            (*cur).node_tab as _,
            0,
            (*cur).ns_max as usize * size_of::<XmlNodePtr>(),
        );
        xml_free((*cur).node_tab as _);
    }
    memset(cur as _, 0, size_of::<XmlC14NVisibleNsStack>());
    xml_free(cur as _);
}

/**
 * xmlC14NFreeCtx:
 * @ctx: the pointer to C14N context object
 *
 * Cleanups the C14N context object.
 */
unsafe extern "C" fn xml_c14n_free_ctx(ctx: XmlC14NCtxPtr) {
    if ctx.is_null() {
        xml_c14n_err_param(c"freeing context".as_ptr() as _);
        return;
    }

    if !(*ctx).ns_rendered.is_null() {
        xml_c14n_visible_ns_stack_destroy((*ctx).ns_rendered);
    }
    xml_free(ctx as _);
}

macro_rules! xml_c14n_is_exclusive {
    ( $ctx:expr ) => {
        matches!((*$ctx).mode, XmlC14NMode::XmlC14NExclusive1_0)
    };
}

/**
 * xmlC14NNewCtx:
 * @doc:        the XML document for canonization
 * @is_visible_callback:the function to use to determine is node visible
 *            or not
 * @user_data:        the first parameter for @is_visible_callback function
 *            (in most cases, it is nodes set)
 * @mode:   the c14n mode (see @xmlC14NMode)
 * @inclusive_ns_prefixe the list of inclusive namespace prefixes
 *            ended with a NULL or NULL if there is no
 *            inclusive namespaces (only for `
 *            canonicalization)
 * @with_comments:    include comments in the result (!=0) or not (==0)
 * @buf:        the output buffer to store canonical XML; this
 *            buffer MUST have encoder==NULL because C14N requires
 *            UTF-8 output
 *
 * Creates new C14N context object to store C14N parameters.
 *
 * Returns pointer to newly created object (success) or NULL (fail)
 */
unsafe extern "C" fn xml_c14n_new_ctx(
    doc: XmlDocPtr,
    is_visible_callback: Option<XmlC14NIsVisibleCallback>,
    user_data: *mut c_void,
    mode: XmlC14NMode,
    inclusive_ns_prefixes: *mut *mut XmlChar,
    with_comments: c_int,
    buf: XmlOutputBufferPtr,
) -> XmlC14NCtxPtr {
    let mut ctx: XmlC14NCtxPtr = null_mut();

    if doc.is_null() || buf.is_null() {
        xml_c14n_err_param(c"creating new context".as_ptr() as _);
        return null_mut();
    }

    /*
     *  Validate the encoding output buffer encoding
     */
    if (*buf).encoder.is_some() {
        xml_c14n_err(
            ctx,
            doc as _,
            XmlParserErrors::XmlC14nRequiresUtf8,
            c"xmlC14NNewCtx: output buffer encoder != NULL but C14N requires UTF8 output\n".as_ptr()
                as _,
        );
        return null_mut();
    }

    /*
     * Allocate a new xmlC14NCtxPtr and fill the fields.
     */
    ctx = xml_malloc(size_of::<XmlC14NCtx>()) as _;
    if ctx.is_null() {
        xml_c14n_err_memory(c"creating context".as_ptr() as _);
        return null_mut();
    }
    memset(ctx as _, 0, size_of::<XmlC14NCtx>());

    /*
     * initialize C14N context
     */
    (*ctx).doc = doc;
    (*ctx).with_comments = with_comments;
    (*ctx).is_visible_callback = is_visible_callback;
    (*ctx).user_data = user_data;
    (*ctx).buf = buf;
    (*ctx).parent_is_doc = 1;
    (*ctx).pos = XmlC14NPosition::XmlC14NBeforeDocumentElement;
    (*ctx).ns_rendered = xml_c14n_visible_ns_stack_create();

    if (*ctx).ns_rendered.is_null() {
        xml_c14n_err(
            ctx,
            doc as _,
            XmlParserErrors::XmlC14nCreateStack,
            c"xmlC14NNewCtx: xmlC14NVisibleNsStackCreate failed\n".as_ptr() as _,
        );
        xml_c14n_free_ctx(ctx);
        return null_mut();
    }

    /*
     * Set "mode" flag and remember list of inclusive prefixes
     * for exclusive c14n
     */
    (*ctx).mode = mode;
    if xml_c14n_is_exclusive!(ctx) {
        (*ctx).inclusive_ns_prefixes = inclusive_ns_prefixes;
    }
    ctx
}

macro_rules! xml_c14n_is_visible {
    ( $ctx:expr, $node:expr, $parent:expr ) => {
        if let Some(callback) = (*$ctx).is_visible_callback {
            callback(
                (*$ctx).user_data,
                $node as XmlNodePtr,
                $parent as XmlNodePtr,
            )
        } else {
            1
        }
    };
}

/**
 * xmlC14NErrRelativeNamespace:
 * @extra:  extra information
 *
 * Handle a redefinition of relative namespace error
 */
unsafe extern "C" fn xml_c14n_err_relative_namespace(ns_uri: *const c_char) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromC14N,
        XmlParserErrors::XmlC14nRelativeNamespace,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        null_mut(),
        null_mut(),
        None,
        0,
        0,
        c"Relative namespace UR is invalid here : %s\n".as_ptr() as _,
        ns_uri
    );
}

/**
 * xmlC14NCheckForRelativeNamespaces:
 * @ctx:        the C14N context
 * @cur:        the current element node
 *
 * Checks that current element node has no relative namespaces defined
 *
 * Returns 0 if the node has no relative namespaces or -1 otherwise.
 */
unsafe extern "C" fn xml_c14n_check_for_relative_namespaces(
    ctx: XmlC14NCtxPtr,
    cur: XmlNodePtr,
) -> c_int {
    let mut ns: XmlNsPtr;

    if ctx.is_null() || cur.is_null() || !matches!((*cur).typ, XmlElementType::XmlElementNode) {
        xml_c14n_err_param(c"checking for relative namespaces".as_ptr() as _);
        return -1;
    }

    ns = (*cur).ns_def;
    while !ns.is_null() {
        if xml_strlen((*ns).href.load(Ordering::Relaxed) as _) > 0 {
            let uri: XmlURIPtr = xml_parse_uri((*ns).href.load(Ordering::Relaxed) as _);
            if uri.is_null() {
                xml_c14n_err_internal(c"parsing namespace uri".as_ptr() as _);
                return -1;
            }
            if xml_strlen((*uri).scheme as _) == 0 {
                xml_c14n_err_relative_namespace((*uri).scheme);
                xml_free_uri(uri);
                return -1;
            }
            xml_free_uri(uri);
        }
        ns = (*ns).next.load(Ordering::Relaxed) as _;
    }
    0
}

unsafe extern "C" fn xml_c14n_visible_ns_stack_save(
    cur: XmlC14NVisibleNsStackPtr,
    state: XmlC14NVisibleNsStackPtr,
) {
    if cur.is_null() || state.is_null() {
        xml_c14n_err_param(c"saving namespaces stack".as_ptr() as _);
        return;
    }

    (*state).ns_cur_end = (*cur).ns_cur_end;
    (*state).ns_prev_start = (*cur).ns_prev_start;
    (*state).ns_prev_end = (*cur).ns_prev_end;
}

/**
 * xmlC14NNsCompare:
 * @ns1:        the pointer to first namespace
 * @ns2:        the pointer to second namespace
 *
 * Compares the namespaces by names (prefixes).
 *
 * Returns -1 if ns1 < ns2, 0 if ns1 == ns2 or 1 if ns1 > ns2.
 */
extern "C" fn xml_c14n_ns_compare(data1: *const c_void, data2: *const c_void) -> c_int {
    let ns1: XmlNsPtr = data1 as _;
    let ns2: XmlNsPtr = data2 as _;
    if ns1 == ns2 {
        return 0;
    }
    if ns1.is_null() {
        return -1;
    }
    if ns2.is_null() {
        return 1;
    }

    unsafe {
        xml_strcmp(
            (*ns1).prefix.load(Ordering::Relaxed),
            (*ns2).prefix.load(Ordering::Relaxed),
        )
    }
}

/// Check whether `ns` is a default 'xml:' namespace with href="http://www.w3.org/XML/1998/namespace".  
/// Return `true` if so, otherwise return `false`.
///
/// Please refer to the document of `xmlC14NIsXmlNs` for original libxml2.
/* todo: make it a define? */
unsafe extern "C" fn xml_c14n_is_xml_ns(ns: XmlNsPtr) -> bool {
    !ns.is_null()
        && xml_str_equal(
            (*ns).prefix.load(Ordering::Relaxed) as _,
            c"xml".as_ptr() as _,
        )
        && xml_str_equal(
            (*ns).href.load(Ordering::Relaxed) as _,
            XML_XML_NAMESPACE.as_ptr() as _,
        )
}

unsafe extern "C" fn xml_c14n_str_equal(
    mut str1: *const XmlChar,
    mut str2: *const XmlChar,
) -> bool {
    if str1 == str2 {
        return true;
    }
    if str1.is_null() {
        return *str2 == b'\0';
    }
    if str2.is_null() {
        return *str1 == b'\0';
    }
    while {
        if *str1 != *str2 {
            return false;
        }
        str1 = str1.add(1);
        str2 = str2.add(1);
        *str2.sub(1) != 0
    } {}
    true
}

/// Check whether `ns` was already rendered or not.  
/// Return `true` if already rendered, otherwise return `false`.
///
/// Please refer to the document of `xmlC14NVisibleNsStackFind` for original libxml2.
unsafe extern "C" fn xml_c14n_visible_ns_stack_find(
    cur: XmlC14NVisibleNsStackPtr,
    ns: XmlNsPtr,
) -> bool {
    if cur.is_null() {
        xml_c14n_err_param(c"searching namespaces stack (c14n)".as_ptr() as _);
        return false;
    }

    /*
     * if the default namespace xmlns="" is not defined yet then
     * we do not want to print it out
     */
    let prefix: *const XmlChar = if ns.is_null() || (*ns).prefix.load(Ordering::Relaxed).is_null() {
        c"".as_ptr() as _
    } else {
        (*ns).prefix.load(Ordering::Relaxed)
    };
    let href: *const XmlChar = if ns.is_null() || (*ns).href.load(Ordering::Relaxed).is_null() {
        c"".as_ptr() as _
    } else {
        (*ns).href.load(Ordering::Relaxed)
    };
    let has_empty_ns =
        xml_c14n_str_equal(prefix, null_mut()) && xml_c14n_str_equal(href, null_mut());

    if !(*cur).ns_tab.is_null() {
        let start: c_int = if has_empty_ns {
            0
        } else {
            (*cur).ns_prev_start
        };
        for i in (start..(*cur).ns_cur_end).rev() {
            let ns1: XmlNsPtr = *(*cur).ns_tab.add(i as usize);

            if xml_c14n_str_equal(
                prefix,
                if !ns1.is_null() {
                    (*ns1).prefix.load(Ordering::Relaxed)
                } else {
                    null_mut()
                },
            ) {
                return xml_c14n_str_equal(
                    href,
                    if !ns1.is_null() {
                        (*ns1).href.load(Ordering::Relaxed)
                    } else {
                        null_mut()
                    },
                );
            }
        }
    }
    has_empty_ns
}

const XML_NAMESPACES_DEFAULT: usize = 16;

unsafe extern "C" fn xml_c14n_visible_ns_stack_add(
    cur: XmlC14NVisibleNsStackPtr,
    ns: XmlNsPtr,
    node: XmlNodePtr,
) {
    if cur.is_null()
        || ((*cur).ns_tab.is_null() && !(*cur).node_tab.is_null())
        || (!(*cur).ns_tab.is_null() && (*cur).node_tab.is_null())
    {
        xml_c14n_err_param(c"adding namespace to stack".as_ptr() as _);
        return;
    }

    if (*cur).ns_tab.is_null() && (*cur).node_tab.is_null() {
        (*cur).ns_tab = xml_malloc(XML_NAMESPACES_DEFAULT * size_of::<XmlNsPtr>()) as _;
        (*cur).node_tab = xml_malloc(XML_NAMESPACES_DEFAULT * size_of::<XmlNodePtr>()) as _;
        if (*cur).ns_tab.is_null() || (*cur).node_tab.is_null() {
            xml_c14n_err_memory(c"adding node to stack".as_ptr() as _);
            return;
        }
        memset(
            (*cur).ns_tab as _,
            0,
            XML_NAMESPACES_DEFAULT * size_of::<XmlNsPtr>(),
        );
        memset(
            (*cur).node_tab as _,
            0,
            XML_NAMESPACES_DEFAULT * size_of::<XmlNodePtr>(),
        );
        (*cur).ns_max = XML_NAMESPACES_DEFAULT as _;
    } else if (*cur).ns_max == (*cur).ns_cur_end {
        let mut tmp: *mut c_void;

        let tmp_size: c_int = 2 * (*cur).ns_max;
        tmp = xml_realloc(
            (*cur).ns_tab as _,
            tmp_size as usize * size_of::<XmlNsPtr>(),
        ) as _;
        if tmp.is_null() {
            xml_c14n_err_memory(c"adding node to stack".as_ptr() as _);
            return;
        }
        (*cur).ns_tab = tmp as _;

        tmp = xml_realloc(
            (*cur).node_tab as _,
            tmp_size as usize * size_of::<XmlNodePtr>(),
        );
        if tmp.is_null() {
            xml_c14n_err_memory(c"adding node to stack".as_ptr() as _);
            return;
        }
        (*cur).node_tab = tmp as _;

        (*cur).ns_max = tmp_size;
    }
    *(*cur).ns_tab.add((*cur).ns_cur_end as usize) = ns;
    *(*cur).node_tab.add((*cur).ns_cur_end as usize) = node;

    (*cur).ns_cur_end += 1;
}

/**
 * xmlC14NPrintNamespaces:
 * @ns:            the pointer to namespace
 * @ctx:        the C14N context
 *
 * Prints the given namespace to the output buffer from C14N context.
 *
 * Returns 1 on success or 0 on fail.
 */
unsafe extern "C" fn xml_c14n_print_namespaces(ns: XmlNsPtr, ctx: XmlC14NCtxPtr) -> c_int {
    if ns.is_null() || ctx.is_null() {
        xml_c14n_err_param(c"writing namespaces".as_ptr() as _);
        return 0;
    }

    if !(*ns).prefix.load(Ordering::Relaxed).is_null() {
        xml_output_buffer_write_string((*ctx).buf, c" xmlns:".as_ptr() as _);
        xml_output_buffer_write_string((*ctx).buf, (*ns).prefix.load(Ordering::Relaxed) as _);
        xml_output_buffer_write_string((*ctx).buf, c"=".as_ptr() as _);
    } else {
        xml_output_buffer_write_string((*ctx).buf, c" xmlns=".as_ptr() as _);
    }
    if !(*ns).href.load(Ordering::Relaxed).is_null() {
        xml_buf_write_quoted_string(
            (*(*ctx).buf).buffer.map_or(null_mut(), |buf| buf.as_ptr()),
            (*ns).href.load(Ordering::Relaxed),
        );
    } else {
        xml_output_buffer_write_string((*ctx).buf, c"\"\"".as_ptr() as _);
    }
    1
}

extern "C" fn xml_c14n_print_namespaces_walker(ns: *const c_void, ctx: *mut c_void) -> c_int {
    unsafe { xml_c14n_print_namespaces(ns as _, ctx as _) }
}

/**
 * xmlC14NProcessNamespacesAxis:
 * @ctx:        the C14N context
 * @node:        the current node
 *
 * Prints out canonical namespace axis of the current node to the
 * buffer from C14N context as follows
 *
 * Canonical XML v 1.0 (http://www.w3.org/TR/xml-c14n)
 *
 * Namespace Axis
 * Consider a list L containing only namespace nodes in the
 * axis and in the node-set in lexicographic order (ascending). To begin
 * processing L, if the first node is not the default namespace node (a node
 * with no namespace URI and no local name), then generate a space followed
 * by xmlns="" if and only if the following conditions are met:
 *    - the element E that owns the axis is in the node-set
 *    - The nearest ancestor element of E in the node-set has a default
 *        namespace node in the node-set (default namespace nodes always
 *      have non-empty values in XPath)
 *      The latter condition eliminates unnecessary occurrences of xmlns="" in
 *      the canonical form since an element only receives an xmlns="" if its
 *      default namespace is empty and if it has an immediate parent in the
 *      canonical form that has a non-empty default namespace. To finish
 *      processing  L, simply process every namespace node in L, except omit
 *      namespace node with local name xml, which defines the xml prefix,
 *      if its string value is http://www.w3.org/XML/1998/namespace.
 *
 * Exclusive XML Canonicalization v 1.0 (http://www.w3.org/TR/xml-exc-c14n)
 * Canonical XML applied to a document subset requires the search of the
 * ancestor nodes of each orphan element node for attributes in the xml
 * namespace, such as xml:lang and xml:space. These are copied into the
 * element node except if a declaration of the same attribute is already
 * in the attribute axis of the element (whether or not it is included in
 * the document subset). This search and copying are omitted from the
 * Exclusive XML Canonicalization method.
 *
 * Returns 0 on success or -1 on fail.
 */
unsafe extern "C" fn xml_c14n_process_namespaces_axis(
    ctx: XmlC14NCtxPtr,
    cur: XmlNodePtr,
    visible: c_int,
) -> c_int {
    let mut n: XmlNodePtr;
    let mut ns: XmlNsPtr;
    let mut tmp: XmlNsPtr;
    let mut already_rendered: c_int;
    let mut has_empty_ns: c_int = 0;

    if ctx.is_null() || cur.is_null() || !matches!((*cur).typ, XmlElementType::XmlElementNode) {
        xml_c14n_err_param(c"processing namespaces axis (c14n)".as_ptr() as _);
        return -1;
    }

    /*
     * Create a sorted list to store element namespaces
     */
    let list: XmlListPtr = xml_list_create(None, Some(xml_c14n_ns_compare));
    if list.is_null() {
        xml_c14n_err_internal(c"creating namespaces list (c14n)".as_ptr() as _);
        return -1;
    }

    /* check all namespaces */
    n = cur;
    while !n.is_null() {
        ns = (*n).ns_def;
        while !ns.is_null() {
            tmp = xml_search_ns((*cur).doc, cur, (*ns).prefix.load(Ordering::Relaxed));

            if tmp == ns && !xml_c14n_is_xml_ns(ns) && xml_c14n_is_visible!(ctx, ns, cur) != 0 {
                already_rendered = xml_c14n_visible_ns_stack_find((*ctx).ns_rendered, ns) as i32;
                if visible != 0 {
                    xml_c14n_visible_ns_stack_add((*ctx).ns_rendered, ns, cur);
                }
                if already_rendered == 0 {
                    xml_list_insert(list, ns as _);
                }
                if xml_strlen((*ns).prefix.load(Ordering::Relaxed)) == 0 {
                    has_empty_ns = 1;
                }
            }
            ns = (*ns).next.load(Ordering::Relaxed);
        }
        n = (*n).parent;
    }

    // if the first node is not the default namespace node (a node with no
    //  namespace URI and no local name), then generate a space followed by
    //  xmlns="" if and only if the following conditions are met:
    //   - the element E that owns the axis is in the node-set
    //   - the nearest ancestor element of E in the node-set has a default
    //      namespace node in the node-set (default namespace nodes always
    //      have non-empty values in XPath)
    if visible != 0 && has_empty_ns == 0 {
        static mut NS_DEFAULT: XmlNs = unsafe { zeroed() };

        memset(addr_of_mut!(NS_DEFAULT) as _, 0, size_of::<XmlNs>());
        if !xml_c14n_visible_ns_stack_find((*ctx).ns_rendered, addr_of_mut!(NS_DEFAULT)) {
            xml_c14n_print_namespaces(addr_of_mut!(NS_DEFAULT), ctx);
        }
    }

    /*
     * print out all elements from list
     */
    xml_list_walk(list, Some(xml_c14n_print_namespaces_walker), ctx as _);

    /*
     * Cleanup
     */
    xml_list_delete(list);
    0
}

unsafe extern "C" fn xml_exc_c14n_visible_ns_stack_find(
    cur: XmlC14NVisibleNsStackPtr,
    ns: XmlNsPtr,
    ctx: XmlC14NCtxPtr,
) -> c_int {
    if cur.is_null() {
        xml_c14n_err_param(c"searching namespaces stack (exc c14n)".as_ptr() as _);
        return 0;
    }

    /*
     * if the default namespace xmlns="" is not defined yet then
     * we do not want to print it out
     */
    let prefix: *const XmlChar = if ns.is_null() || (*ns).prefix.load(Ordering::Relaxed).is_null() {
        c"".as_ptr() as _
    } else {
        (*ns).prefix.load(Ordering::Relaxed)
    };
    let href: *const XmlChar = if ns.is_null() || (*ns).href.load(Ordering::Relaxed).is_null() {
        c"".as_ptr() as _
    } else {
        (*ns).href.load(Ordering::Relaxed)
    };
    let has_empty_ns: c_int =
        (xml_c14n_str_equal(prefix, null_mut()) && xml_c14n_str_equal(href, null_mut())) as _;

    if !(*cur).ns_tab.is_null() {
        let start: c_int = 0;
        for i in (start..(*cur).ns_cur_end).rev() {
            let ns1: XmlNsPtr = *(*cur).ns_tab.add(i as usize);

            if xml_c14n_str_equal(
                prefix,
                if !ns1.is_null() {
                    (*ns1).prefix.load(Ordering::Relaxed)
                } else {
                    null_mut()
                },
            ) {
                if xml_c14n_str_equal(
                    href,
                    if !ns1.is_null() {
                        (*ns1).href.load(Ordering::Relaxed)
                    } else {
                        null_mut()
                    },
                ) {
                    return xml_c14n_is_visible!(ctx, ns1, *(*cur).node_tab.add(i as usize));
                } else {
                    return 0;
                }
            }
        }
    }
    has_empty_ns
}

/**
 * xmlExcC14NProcessNamespacesAxis:
 * @ctx:        the C14N context
 * @node:        the current node
 *
 * Prints out exclusive canonical namespace axis of the current node to the
 * buffer from C14N context as follows
 *
 * Exclusive XML Canonicalization
 * http://www.w3.org/TR/xml-exc-c14n
 *
 * If the element node is in the XPath subset then output the node in
 * accordance with Canonical XML except for namespace nodes which are
 * rendered as follows:
 *
 * 1. Render each namespace node iff:
 *    * it is visibly utilized by the immediate parent element or one of
 *      its attributes, or is present in InclusiveNamespaces PrefixList, and
 *    * its prefix and value do not appear in ns_rendered. ns_rendered is
 *      obtained by popping the state stack in order to obtain a list of
 *      prefixes and their values which have already been rendered by
 *      an output ancestor of the namespace node's parent element.
 * 2. Append the rendered namespace node to the list ns_rendered of namespace
 *    nodes rendered by output ancestors. Push ns_rendered on state stack and
 *    recurse.
 * 3. After the recursion returns, pop thestate stack.
 *
 *
 * Returns 0 on success or -1 on fail.
 */
unsafe extern "C" fn xml_exc_c14n_process_namespaces_axis(
    ctx: XmlC14NCtxPtr,
    cur: XmlNodePtr,
    visible: c_int,
) -> c_int {
    let mut ns: XmlNsPtr;
    let mut attr: XmlAttrPtr;
    let mut already_rendered: c_int;
    let mut has_empty_ns: c_int = 0;
    let mut has_visibly_utilized_empty_ns: c_int = 0;
    let mut has_empty_ns_in_inclusive_list: c_int = 0;

    if ctx.is_null() || cur.is_null() || !matches!((*cur).typ, XmlElementType::XmlElementNode) {
        xml_c14n_err_param(c"processing namespaces axis (exc c14n)".as_ptr() as _);
        return -1;
    }

    if !xml_c14n_is_exclusive!(ctx) {
        xml_c14n_err_param(c"processing namespaces axis (exc c14n)".as_ptr() as _);
        return -1;
    }

    /*
     * Create a sorted list to store element namespaces
     */
    let list: XmlListPtr = xml_list_create(None, Some(xml_c14n_ns_compare));
    if list.is_null() {
        xml_c14n_err_internal(c"creating namespaces list (exc c14n)".as_ptr() as _);
        return -1;
    }

    /*
     * process inclusive namespaces:
     * All namespace nodes appearing on inclusive ns list are
     * handled as provided in Canonical XML
     */
    if !(*ctx).inclusive_ns_prefixes.is_null() {
        let mut prefix: *mut XmlChar;

        for i in (0..).take_while(|&i| !(*(*ctx).inclusive_ns_prefixes.add(i)).is_null()) {
            prefix = *(*ctx).inclusive_ns_prefixes.add(i);
            /*
             * Special values for namespace with empty prefix
             */
            if xml_str_equal(prefix, c"#default".as_ptr() as _)
                || xml_str_equal(prefix, c"".as_ptr() as _)
            {
                prefix = null_mut();
                has_empty_ns_in_inclusive_list = 1;
            }

            ns = xml_search_ns((*cur).doc, cur, prefix);
            if !ns.is_null() && !xml_c14n_is_xml_ns(ns) && xml_c14n_is_visible!(ctx, ns, cur) != 0 {
                already_rendered = xml_c14n_visible_ns_stack_find((*ctx).ns_rendered, ns) as i32;
                if visible != 0 {
                    xml_c14n_visible_ns_stack_add((*ctx).ns_rendered, ns, cur);
                }
                if already_rendered == 0 {
                    xml_list_insert(list, ns as _);
                }
                if xml_strlen((*ns).prefix.load(Ordering::Relaxed)) == 0 {
                    has_empty_ns = 1;
                }
            }
        }
    }

    /* add node namespace */
    if !(*cur).ns.is_null() {
        ns = (*cur).ns;
    } else {
        ns = xml_search_ns((*cur).doc, cur, null_mut());
        has_visibly_utilized_empty_ns = 1;
    }
    if !ns.is_null() && !xml_c14n_is_xml_ns(ns) {
        if (visible != 0 && xml_c14n_is_visible!(ctx, ns, cur) != 0)
            && xml_exc_c14n_visible_ns_stack_find((*ctx).ns_rendered, ns, ctx) == 0
        {
            xml_list_insert(list, ns as _);
        }
        if visible != 0 {
            xml_c14n_visible_ns_stack_add((*ctx).ns_rendered, ns, cur);
        }
        if xml_strlen((*ns).prefix.load(Ordering::Relaxed)) == 0 {
            has_empty_ns = 1;
        }
    }

    /* add attributes */
    attr = (*cur).properties;
    while !attr.is_null() {
        /*
         * we need to check that attribute is visible and has non
         * default namespace (XML Namespaces: "default namespaces
         * do not apply directly to attributes")
         */
        if !(*attr).ns.is_null()
            && !xml_c14n_is_xml_ns((*attr).ns)
            && xml_c14n_is_visible!(ctx, attr, cur) != 0
        {
            already_rendered =
                xml_exc_c14n_visible_ns_stack_find((*ctx).ns_rendered, (*attr).ns, ctx);
            xml_c14n_visible_ns_stack_add((*ctx).ns_rendered, (*attr).ns, cur);
            if already_rendered == 0 && visible != 0 {
                xml_list_insert(list, (*attr).ns as _);
            }
            if xml_strlen((*(*attr).ns).prefix.load(Ordering::Relaxed)) == 0 {
                has_empty_ns = 1;
            }
        } else if !(*attr).ns.is_null()
            && xml_strlen((*(*attr).ns).prefix.load(Ordering::Relaxed)) == 0
            && xml_strlen((*(*attr).ns).href.load(Ordering::Relaxed)) == 0
        {
            has_visibly_utilized_empty_ns = 1;
        }
        attr = (*attr).next;
    }

    /*
     * Process xmlns=""
     */
    if visible != 0
        && has_visibly_utilized_empty_ns != 0
        && has_empty_ns == 0
        && has_empty_ns_in_inclusive_list == 0
    {
        static mut NS_DEFAULT: XmlNs = unsafe { zeroed() };

        memset(addr_of_mut!(NS_DEFAULT) as _, 0, size_of::<XmlNs>());
        already_rendered =
            xml_exc_c14n_visible_ns_stack_find((*ctx).ns_rendered, addr_of_mut!(NS_DEFAULT), ctx);
        if already_rendered == 0 {
            xml_c14n_print_namespaces(addr_of_mut!(NS_DEFAULT), ctx);
        }
    } else if visible != 0 && has_empty_ns == 0 && has_empty_ns_in_inclusive_list != 0 {
        static mut NS_DEFAULT: XmlNs = unsafe { zeroed() };

        memset(addr_of_mut!(NS_DEFAULT) as _, 0, size_of::<XmlNs>());
        if !xml_c14n_visible_ns_stack_find((*ctx).ns_rendered, addr_of_mut!(NS_DEFAULT)) {
            xml_c14n_print_namespaces(addr_of_mut!(NS_DEFAULT), ctx);
        }
    }

    /*
     * print out all elements from list
     */
    xml_list_walk(list, Some(xml_c14n_print_namespaces_walker), ctx as _);

    /*
     * Cleanup
     */
    xml_list_delete(list);
    0
}

unsafe extern "C" fn xml_c14n_visible_ns_stack_shift(cur: XmlC14NVisibleNsStackPtr) {
    if cur.is_null() {
        xml_c14n_err_param(c"shifting namespaces stack".as_ptr() as _);
        return;
    }
    (*cur).ns_prev_start = (*cur).ns_prev_end;
    (*cur).ns_prev_end = (*cur).ns_cur_end;
}

/**
 * xmlC14NAttrsCompare:
 * @attr1:        the pointer tls o first attr
 * @attr2:        the pointer to second attr
 *
 * Prints the given attribute to the output buffer from C14N context.
 *
 * Returns -1 if attr1 < attr2, 0 if attr1 == attr2 or 1 if attr1 > attr2.
 */
extern "C" fn xml_c14n_attrs_compare(data1: *const c_void, data2: *const c_void) -> c_int {
    let attr1: XmlAttrPtr = data1 as _;
    let attr2: XmlAttrPtr = data2 as _;

    /*
     * Simple cases
     */
    if attr1 == attr2 {
        return 0;
    }
    if attr1.is_null() {
        return -1;
    }
    if attr2.is_null() {
        return 1;
    }

    unsafe {
        if (*attr1).ns == (*attr2).ns {
            return xml_strcmp((*attr1).name, (*attr2).name);
        }

        /*
         * Attributes in the default namespace are first
         * because the default namespace is not applied to
         * unqualified attributes
         */
        if (*attr1).ns.is_null() {
            return -1;
        }
        if (*attr2).ns.is_null() {
            return 1;
        }
        if (*(*attr1).ns).prefix.load(Ordering::Relaxed).is_null() {
            return -1;
        }
        if (*(*attr2).ns).prefix.load(Ordering::Relaxed).is_null() {
            return 1;
        }

        let mut ret: i32 = xml_strcmp(
            (*(*attr1).ns).href.load(Ordering::Relaxed),
            (*(*attr2).ns).href.load(Ordering::Relaxed),
        );
        if ret == 0 {
            ret = xml_strcmp((*attr1).name, (*attr2).name);
        }
        ret
    }
}

/// Checks whether `attr` is a default "xml:" namespace with href="http://www.w3.org/XML/1998/namespace".  
/// Return `true` if so, otherwise return false.
///
/// Please refer to the document of `xmlC14NIsXmlAttr` for original libxml2.
/* todo: make it a define? */
unsafe extern "C" fn xml_c14n_is_xml_attr(attr: XmlAttrPtr) -> bool {
    !(*attr).ns.is_null() && xml_c14n_is_xml_ns((*attr).ns)
}

/**
 * xmlC14NFindHiddenParentAttr:
 *
 * Finds an attribute in a hidden parent node.
 *
 * Returns a pointer to the attribute node (if found) or NULL otherwise.
 */
unsafe extern "C" fn xml_c14n_find_hidden_parent_attr(
    ctx: XmlC14NCtxPtr,
    mut cur: XmlNodePtr,
    name: *const XmlChar,
    ns: *const XmlChar,
) -> XmlAttrPtr {
    let mut res: XmlAttrPtr;
    while !cur.is_null() && xml_c14n_is_visible!(ctx, cur, (*cur).parent) == 0 {
        res = xml_has_ns_prop(cur, name, ns);
        if !res.is_null() {
            return res;
        }

        cur = (*cur).parent;
    }

    null_mut()
}

/**
 * xmlC14NFixupBaseAttr:
 *
 * Fixes up the xml:base attribute
 *
 * Returns the newly created attribute or NULL
 */
unsafe extern "C" fn xml_c14n_fixup_base_attr(
    ctx: XmlC14NCtxPtr,
    xml_base_attr: XmlAttrPtr,
) -> XmlAttrPtr {
    let mut cur: XmlNodePtr;
    let mut attr: XmlAttrPtr;
    let mut tmp_str: *mut XmlChar;
    let mut tmp_str2: *mut XmlChar;
    let mut tmp_str_len: c_int;

    if ctx.is_null() || xml_base_attr.is_null() || (*xml_base_attr).parent.is_null() {
        xml_c14n_err_param(c"processing xml:base attribute".as_ptr() as _);
        return null_mut();
    }

    /* start from current value */
    let mut res: *mut XmlChar = xml_node_list_get_string((*ctx).doc, (*xml_base_attr).children, 1);
    if res.is_null() {
        xml_c14n_err_internal(
            c"processing xml:base attribute - can't get attr value".as_ptr() as _,
        );
        return null_mut();
    }

    /* go up the stack until we find a node that we rendered already */
    cur = (*(*xml_base_attr).parent).parent;
    while !cur.is_null() && xml_c14n_is_visible!(ctx, cur, (*cur).parent) == 0 {
        attr = xml_has_ns_prop(cur, c"base".as_ptr() as _, XML_XML_NAMESPACE.as_ptr() as _);
        if !attr.is_null() {
            /* get attr value */
            tmp_str = xml_node_list_get_string((*ctx).doc, (*attr).children, 1);
            if tmp_str.is_null() {
                xml_free(res as _);

                xml_c14n_err_internal(
                    c"processing xml:base attribute - can't get attr value".as_ptr() as _,
                );
                return null_mut();
            }

            /* we need to add '/' if our current base uri ends with '..' or '.'
            to ensure that we are forced to go "up" all the time */
            tmp_str_len = xml_strlen(tmp_str);
            if tmp_str_len > 1 && *tmp_str.add(tmp_str_len as usize - 2) == b'.' {
                tmp_str2 = xml_strcat(tmp_str, c"/".as_ptr() as _);
                if tmp_str2.is_null() {
                    xml_free(tmp_str as _);
                    xml_free(res as _);

                    xml_c14n_err_internal(
                        c"processing xml:base attribute - can't modify uri".as_ptr() as _,
                    );
                    return null_mut();
                }

                tmp_str = tmp_str2;
            }

            /* build uri */
            tmp_str2 = xml_build_uri(res, tmp_str);
            if tmp_str2.is_null() {
                xml_free(tmp_str as _);
                xml_free(res as _);

                xml_c14n_err_internal(
                    c"processing xml:base attribute - can't construct uri".as_ptr() as _,
                );
                return null_mut();
            }

            /* cleanup and set the new res */
            xml_free(tmp_str as _);
            xml_free(res as _);
            res = tmp_str2;
        }

        /* next */
        cur = (*cur).parent;
    }

    /* check if result uri is empty or not */
    if res.is_null() || xml_str_equal(res, c"".as_ptr() as _) {
        xml_free(res as _);
        return null_mut();
    }

    /* create and return the new attribute node */
    attr = xml_new_ns_prop(null_mut(), (*xml_base_attr).ns, c"base".as_ptr() as _, res);
    if attr.is_null() {
        xml_free(res as _);

        xml_c14n_err_internal(
            c"processing xml:base attribute - can't construct attribute".as_ptr() as _,
        );
        return null_mut();
    }

    /* done */
    xml_free(res as _);
    attr
}

/*
 * Macro used to grow the current buffer.
 */
macro_rules! grow_buffer_reentrant {
    ($buffer:expr, $buffer_size:expr) => {
        $buffer_size *= 2;
        $buffer = xml_realloc($buffer as _, $buffer_size as usize) as _;
        if $buffer.is_null() {
            xml_c14n_err_memory(c"growing buffer".as_ptr() as _);
            return null_mut();
        }
    };
}

/**
 * xmlC11NNormalizeString:
 * @input:        the input string
 * @mode:        the normalization mode (attribute, comment, PI or text)
 *
 * Converts a string to a canonical (normalized) format. The code is stolen
 * from xmlEncodeEntitiesReentrant(). Added normalization of \x09, \x0a, \x0A
 * and the @mode parameter
 *
 * Returns a normalized string (caller is responsible for calling xmlFree( as _))
 * or NULL if an error occurs
 */
unsafe extern "C" fn xml_c11n_normalize_string(
    input: *const XmlChar,
    mode: XmlC14NNormalizationMode,
) -> *mut XmlChar {
    let mut cur: *const XmlChar = input;

    if input.is_null() {
        return null_mut();
    }

    /*
     * allocate an translation buffer.
     */
    let mut buffer_size: i32 = 1000;
    let mut buffer: *mut XmlChar = xml_malloc_atomic(buffer_size as usize) as _;
    if buffer.is_null() {
        xml_c14n_err_memory(c"allocating buffer".as_ptr() as _);
        return null_mut();
    }
    let mut out: *mut XmlChar = buffer;

    while *cur != b'\0' {
        if out.offset_from(buffer) > buffer_size as isize - 10 {
            let index: c_int = out.offset_from(buffer) as _;

            grow_buffer_reentrant!(buffer, buffer_size);
            out = buffer.add(index as usize);
        }

        if *cur == b'<'
            && matches!(
                mode,
                XmlC14NNormalizationMode::XmlC14NNormalizeAttr
                    | XmlC14NNormalizationMode::XmlC14NNormalizeText
            )
        {
            *out = b'&';
            out = out.add(1);
            *out = b'l';
            out = out.add(1);
            *out = b't';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *cur == b'>' && matches!(mode, XmlC14NNormalizationMode::XmlC14NNormalizeText) {
            *out = b'&';
            out = out.add(1);
            *out = b'g';
            out = out.add(1);
            *out = b't';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *cur == b'&'
            && matches!(
                mode,
                XmlC14NNormalizationMode::XmlC14NNormalizeAttr
                    | XmlC14NNormalizationMode::XmlC14NNormalizeText
            )
        {
            *out = b'&';
            out = out.add(1);
            *out = b'a';
            out = out.add(1);
            *out = b'm';
            out = out.add(1);
            *out = b'p';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *cur == b'"' && matches!(mode, XmlC14NNormalizationMode::XmlC14NNormalizeAttr) {
            *out = b'&';
            out = out.add(1);
            *out = b'q';
            out = out.add(1);
            *out = b'u';
            out = out.add(1);
            *out = b'o';
            out = out.add(1);
            *out = b't';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *cur == b'\x09' && matches!(mode, XmlC14NNormalizationMode::XmlC14NNormalizeAttr)
        {
            *out = b'&';
            out = out.add(1);
            *out = b'#';
            out = out.add(1);
            *out = b'x';
            out = out.add(1);
            *out = b'9';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *cur == b'\x0A' && matches!(mode, XmlC14NNormalizationMode::XmlC14NNormalizeAttr)
        {
            *out = b'&';
            out = out.add(1);
            *out = b'#';
            out = out.add(1);
            *out = b'x';
            out = out.add(1);
            *out = b'A';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else if *cur == b'\x0D'
            && matches!(
                mode,
                XmlC14NNormalizationMode::XmlC14NNormalizeAttr
                    | XmlC14NNormalizationMode::XmlC14NNormalizeText
                    | XmlC14NNormalizationMode::XmlC14NNormalizeComment
                    | XmlC14NNormalizationMode::XmlC14NNormalizePI
            )
        {
            *out = b'&';
            out = out.add(1);
            *out = b'#';
            out = out.add(1);
            *out = b'x';
            out = out.add(1);
            *out = b'D';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
        } else {
            /*
             * Works because on UTF-8, all extended sequences cannot
             * result in bytes in the ASCII range.
             */
            *out = *cur;
            out = out.add(1);
        }
        cur = cur.add(1);
    }
    *out = 0;
    buffer
}

unsafe extern "C" fn xml_c11n_normalize_attr(a: *const u8) -> *mut XmlChar {
    xml_c11n_normalize_string(a, XmlC14NNormalizationMode::XmlC14NNormalizeAttr)
}

/**
 * xmlC14NPrintAttrs:
 * @attr:        the pointer to attr
 * @ctx:        the C14N context
 *
 * Prints out canonical attribute urrent node to the
 * buffer from C14N context as follows
 *
 * Canonical XML v 1.0 (http://www.w3.org/TR/xml-c14n)
 *
 * Returns 1 on success or 0 on fail.
 */
extern "C" fn xml_c14n_print_attrs(data: *const c_void, user: *mut c_void) -> c_int {
    let attr: XmlAttrPtr = data as _;
    let ctx: XmlC14NCtxPtr = user as _;
    let buffer: *mut XmlChar;

    unsafe {
        if attr.is_null() || ctx.is_null() {
            xml_c14n_err_param(c"writing attributes".as_ptr() as _);
            return 0;
        }

        xml_output_buffer_write_string((*ctx).buf, c" ".as_ptr() as _);
        if !(*attr).ns.is_null() && xml_strlen((*(*attr).ns).prefix.load(Ordering::Relaxed)) > 0 {
            xml_output_buffer_write_string(
                (*ctx).buf,
                (*(*attr).ns).prefix.load(Ordering::Relaxed) as _,
            );
            xml_output_buffer_write_string((*ctx).buf, c":".as_ptr() as _);
        }
        xml_output_buffer_write_string((*ctx).buf, (*attr).name as _);
        xml_output_buffer_write_string((*ctx).buf, c"=\"".as_ptr() as _);

        let value: *mut XmlChar = xml_node_list_get_string((*ctx).doc, (*attr).children, 1);
        /* todo: should we log an error if value==NULL ? */
        if !value.is_null() {
            buffer = xml_c11n_normalize_attr(value);
            xml_free(value as _);
            if !buffer.is_null() {
                xml_output_buffer_write_string((*ctx).buf, buffer as _);
                xml_free(buffer as _);
            } else {
                xml_c14n_err_internal(c"normalizing attributes axis".as_ptr() as _);
                return 0;
            }
        }
        xml_output_buffer_write_string((*ctx).buf, c"\"".as_ptr() as _);
        1
    }
}

/**
 * xmlC14NProcessAttrsAxis:
 * @ctx:        the C14N context
 * @cur:        the current node
 * @parent_visible:    the visibility of parent node
 * @all_parents_visible: the visibility of all parent nodes
 *
 * Prints out canonical attribute axis of the current node to the
 * buffer from C14N context as follows
 *
 * Canonical XML v 1.0 (http://www.w3.org/TR/xml-c14n)
 *
 * Attribute Axis
 * In lexicographic order (ascending), process each node that
 * is in the element's attribute axis and in the node-set.
 *
 * The processing of an element node E MUST be modified slightly
 * when an XPath node-set is given as input and the element's
 * parent is omitted from the node-set.
 *
 *
 * Exclusive XML Canonicalization v 1.0 (http://www.w3.org/TR/xml-exc-c14n)
 *
 * Canonical XML applied to a document subset requires the search of the
 * ancestor nodes of each orphan element node for attributes in the xml
 * namespace, such as xml:lang and xml:space. These are copied into the
 * element node except if a declaration of the same attribute is already
 * in the attribute axis of the element (whether or not it is included in
 * the document subset). This search and copying are omitted from the
 * Exclusive XML Canonicalization method.
 *
 * Returns 0 on success or -1 on fail.
 */
unsafe extern "C" fn xml_c14n_process_attrs_axis(
    ctx: XmlC14NCtxPtr,
    cur: XmlNodePtr,
    parent_visible: c_int,
) -> c_int {
    let mut attr: XmlAttrPtr;
    let mut attrs_to_delete: XmlAttrPtr = null_mut();

    /* special processing for 1.1 spec */
    let mut xml_base_attr: XmlAttrPtr = null_mut();
    let mut xml_lang_attr: XmlAttrPtr = null_mut();
    let mut xml_space_attr: XmlAttrPtr = null_mut();

    if ctx.is_null() || cur.is_null() || !matches!((*cur).typ, XmlElementType::XmlElementNode) {
        xml_c14n_err_param(c"processing attributes axis".as_ptr() as _);
        return -1;
    }

    /*
     * Create a sorted list to store element attributes
     */
    let list: XmlListPtr = xml_list_create(None, Some(xml_c14n_attrs_compare));
    if list.is_null() {
        xml_c14n_err_internal(c"creating attributes list".as_ptr() as _);
        return -1;
    }

    match (*ctx).mode {
        XmlC14NMode::XmlC14N1_0 => {
            /* The processing of an element node E MUST be modified slightly when an XPath node-set is
             * given as input and the element's parent is omitted from the node-set. The method for processing
             * the attribute axis of an element E in the node-set is enhanced. All element nodes along E's
             * ancestor axis are examined for nearest occurrences of attributes in the xml namespace, such
             * as xml:lang and xml:space (whether or not they are in the node-set). From this list of attributes,
             * remove any that are in E's attribute axis (whether or not they are in the node-set). Then,
             * lexicographically merge this attribute list with the nodes of E's attribute axis that are in
             * the node-set. The result of visiting the attribute axis is computed by processing the attribute
             * nodes in this merged attribute list.
             */

            /*
             * Add all visible attributes from current node.
             */
            attr = (*cur).properties;
            while !attr.is_null() {
                /* check that attribute is visible */
                if xml_c14n_is_visible!(ctx, attr, cur) != 0 {
                    xml_list_insert(list, attr as _);
                }
                attr = (*attr).next;
            }

            /*
             * Handle xml attributes
             */
            if parent_visible != 0
                && !(*cur).parent.is_null()
                && xml_c14n_is_visible!(ctx, (*cur).parent, (*(*cur).parent).parent) == 0
            {
                let mut tmp: XmlNodePtr;

                /*
                 * If XPath node-set is not specified then the parent is always
                 * visible!
                 */
                tmp = (*cur).parent;
                while !tmp.is_null() {
                    attr = (*tmp).properties;
                    while !attr.is_null() {
                        if xml_c14n_is_xml_attr(attr) && xml_list_search(list, attr as _).is_null()
                        {
                            xml_list_insert(list, attr as _);
                        }
                        attr = (*attr).next;
                    }
                    tmp = (*tmp).parent;
                }
            }
        }
        XmlC14NMode::XmlC14NExclusive1_0 => {
            /* attributes in the XML namespace, such as xml:lang and xml:space
             * are not imported into orphan nodes of the document subset
             */

            /*
             * Add all visible attributes from current node.
             */
            attr = (*cur).properties;
            while !attr.is_null() {
                /* check that attribute is visible */
                if xml_c14n_is_visible!(ctx, attr, cur) != 0 {
                    xml_list_insert(list, attr as _);
                }
                attr = (*attr).next;
            }
        }
        XmlC14NMode::XmlC14N1_1 => {
            /* The processing of an element node E MUST be modified slightly when an XPath node-set is
             * given as input and some of the element's ancestors are omitted from the node-set.
             *
             * Simple inheritable attributes are attributes that have a value that requires at most a simple
             * redeclaration. This redeclaration is done by supplying a new value in the child axis. The
             * redeclaration of a simple inheritable attribute A contained in one of E's ancestors is done
             * by supplying a value to an attribute Ae inside E with the same name. Simple inheritable attributes
             * are xml:lang and xml:space.
             *
             * The method for processing the attribute axis of an element E in the node-set is hence enhanced.
             * All element nodes along E's ancestor axis are examined for the nearest occurrences of simple
             * inheritable attributes in the xml namespace, such as xml:lang and xml:space (whether or not they
             * are in the node-set). From this list of attributes, any simple inheritable attributes that are
             * already in E's attribute axis (whether or not they are in the node-set) are removed. Then,
             * lexicographically merge this attribute list with the nodes of E's attribute axis that are in
             * the node-set. The result of visiting the attribute axis is computed by processing the attribute
             * nodes in this merged attribute list.
             *
             * The xml:id attribute is not a simple inheritable attribute and no processing of these attributes is
             * performed.
             *
             * The xml:base attribute is not a simple inheritable attribute and requires special processing beyond
             * a simple redeclaration.
             *
             * Attributes in the XML namespace other than xml:base, xml:id, xml:lang, and xml:space MUST be processed
             * as ordinary attributes.
             */

            /*
             * Add all visible attributes from current node.
             */
            attr = (*cur).properties;
            while !attr.is_null() {
                /* special processing for XML attribute kiks in only when we have invisible parents */
                if parent_visible == 0 || !xml_c14n_is_xml_attr(attr) {
                    /* check that attribute is visible */
                    if xml_c14n_is_visible!(ctx, attr, cur) != 0 {
                        xml_list_insert(list, attr as _);
                    }
                } else {
                    let mut matched: c_int = 0;

                    /* check for simple inheritance attributes */
                    if matched == 0
                        && xml_lang_attr.is_null()
                        && xml_str_equal((*attr).name, c"lang".as_ptr() as _)
                    {
                        xml_lang_attr = attr;
                        matched = 1;
                    }
                    if matched == 0
                        && xml_space_attr.is_null()
                        && xml_str_equal((*attr).name, c"space".as_ptr() as _)
                    {
                        xml_space_attr = attr;
                        matched = 1;
                    }

                    /* check for base attr */
                    if matched == 0
                        && xml_base_attr.is_null()
                        && xml_str_equal((*attr).name, c"base".as_ptr() as _)
                    {
                        xml_base_attr = attr;
                        matched = 1;
                    }

                    /* otherwise, it is a normal attribute, so just check if it is visible */
                    if matched == 0 && xml_c14n_is_visible!(ctx, attr, cur) != 0 {
                        xml_list_insert(list, attr as _);
                    }
                }

                /* move to the next one */
                attr = (*attr).next;
            }

            /* special processing for XML attribute kiks in only when we have invisible parents */
            if parent_visible != 0 {
                /* simple inheritance attributes - copy */
                if xml_lang_attr.is_null() {
                    xml_lang_attr = xml_c14n_find_hidden_parent_attr(
                        ctx,
                        (*cur).parent,
                        c"lang".as_ptr() as _,
                        XML_XML_NAMESPACE.as_ptr() as _,
                    );
                }
                if !xml_lang_attr.is_null() {
                    xml_list_insert(list, xml_lang_attr as _);
                }
                if xml_space_attr.is_null() {
                    xml_space_attr = xml_c14n_find_hidden_parent_attr(
                        ctx,
                        (*cur).parent,
                        c"space".as_ptr() as _,
                        XML_XML_NAMESPACE.as_ptr() as _,
                    );
                }
                if !xml_space_attr.is_null() {
                    xml_list_insert(list, xml_space_attr as _);
                }

                /* base uri attribute - fix up */
                if xml_base_attr.is_null() {
                    /* if we don't have base uri attribute, check if we have a "hidden" one above */
                    xml_base_attr = xml_c14n_find_hidden_parent_attr(
                        ctx,
                        (*cur).parent,
                        c"base".as_ptr() as _,
                        XML_XML_NAMESPACE.as_ptr() as _,
                    );
                }
                if !xml_base_attr.is_null() {
                    xml_base_attr = xml_c14n_fixup_base_attr(ctx, xml_base_attr);
                    if !xml_base_attr.is_null() {
                        xml_list_insert(list, xml_base_attr as _);

                        /* note that we MUST delete returned attr node ourselves! */
                        (*xml_base_attr).next = attrs_to_delete;
                        attrs_to_delete = xml_base_attr;
                    }
                }
            }
        }
    }

    /*
     * print out all elements from list
     */
    xml_list_walk(list, Some(xml_c14n_print_attrs), ctx as _);

    /*
     * Cleanup
     */
    xml_free_prop_list(attrs_to_delete);
    xml_list_delete(list);
    0
}

unsafe extern "C" fn xml_c14n_visible_ns_stack_restore(
    cur: XmlC14NVisibleNsStackPtr,
    state: XmlC14NVisibleNsStackPtr,
) {
    if cur.is_null() || state.is_null() {
        xml_c14n_err_param(c"restoring namespaces stack".as_ptr() as _);
        return;
    }
    (*cur).ns_cur_end = (*state).ns_cur_end;
    (*cur).ns_prev_start = (*state).ns_prev_start;
    (*cur).ns_prev_end = (*state).ns_prev_end;
}

/**
 * xmlC14NProcessElementNode:
 * @ctx:        the pointer to C14N context object
 * @cur:        the node to process
 * @visible:    this node is visible
 * @all_parents_visible: whether all the parents of this node are visible
 *
 * Canonical XML v 1.0 (http://www.w3.org/TR/xml-c14n)
 *
 * Element Nodes
 * If the element is not in the node-set, then the result is obtained
 * by processing the namespace axis, then the attribute axis, then
 * processing the child nodes of the element that are in the node-set
 * (in document order). If the element is in the node-set, then the result
 * is an open angle bracket (<), the element QName, the result of
 * processing the namespace axis, the result of processing the attribute
 * axis, a close angle bracket (>), the result of processing the child
 * nodes of the element that are in the node-set (in document order), an
 * open angle bracket, a forward slash (/), the element QName, and a close
 * angle bracket.
 *
 * Returns non-negative value on success or negative value on fail
 */
unsafe extern "C" fn xml_c14n_process_element_node(
    ctx: XmlC14NCtxPtr,
    cur: XmlNodePtr,
    visible: c_int,
) -> c_int {
    let mut ret: c_int;
    let mut state: XmlC14NVisibleNsStack = unsafe { zeroed() };
    let mut parent_is_doc: c_int = 0;

    if ctx.is_null() || cur.is_null() || !matches!((*cur).typ, XmlElementType::XmlElementNode) {
        xml_c14n_err_param(c"processing element node".as_ptr() as _);
        return -1;
    }

    /*
     * Check relative relative namespaces:
     * implementations of XML canonicalization MUST report an operation
     * failure on documents containing relative namespace URIs.
     */
    if xml_c14n_check_for_relative_namespaces(ctx, cur) < 0 {
        xml_c14n_err_internal(c"checking for relative namespaces".as_ptr() as _);
        return -1;
    }

    /*
     * Save ns_rendered stack position
     */
    memset(addr_of_mut!(state) as _, 0, size_of_val(&state));
    xml_c14n_visible_ns_stack_save((*ctx).ns_rendered, addr_of_mut!(state));

    if visible != 0 {
        if (*ctx).parent_is_doc != 0 {
            /* save this flag into the stack */
            parent_is_doc = (*ctx).parent_is_doc;
            (*ctx).parent_is_doc = 0;
            (*ctx).pos = XmlC14NPosition::XmlC14NInsideDocumentElement;
        }
        xml_output_buffer_write_string((*ctx).buf, c"<".as_ptr() as _);

        if !(*cur).ns.is_null() && xml_strlen((*(*cur).ns).prefix.load(Ordering::Relaxed) as _) > 0
        {
            xml_output_buffer_write_string(
                (*ctx).buf,
                (*(*cur).ns).prefix.load(Ordering::Relaxed) as _,
            );
            xml_output_buffer_write_string((*ctx).buf, c":".as_ptr() as _);
        }
        xml_output_buffer_write_string((*ctx).buf, (*cur).name as _);
    }

    if !xml_c14n_is_exclusive!(ctx) {
        ret = xml_c14n_process_namespaces_axis(ctx, cur, visible);
    } else {
        ret = xml_exc_c14n_process_namespaces_axis(ctx, cur, visible);
    }
    if ret < 0 {
        xml_c14n_err_internal(c"processing namespaces axis".as_ptr() as _);
        return -1;
    }
    /* todo: shouldn't this go to "visible only"? */
    if visible != 0 {
        xml_c14n_visible_ns_stack_shift((*ctx).ns_rendered);
    }

    ret = xml_c14n_process_attrs_axis(ctx, cur, visible);
    if ret < 0 {
        xml_c14n_err_internal(c"processing attributes axis".as_ptr() as _);
        return -1;
    }

    if visible != 0 {
        xml_output_buffer_write_string((*ctx).buf, c">".as_ptr() as _);
    }
    if !(*cur).children.is_null() {
        ret = xml_c14n_process_node_list(ctx, (*cur).children);
        if ret < 0 {
            xml_c14n_err_internal(c"processing childrens list".as_ptr() as _);
            return -1;
        }
    }
    if visible != 0 {
        xml_output_buffer_write_string((*ctx).buf, c"</".as_ptr() as _);
        if !(*cur).ns.is_null() && xml_strlen((*(*cur).ns).prefix.load(Ordering::Relaxed)) > 0 {
            xml_output_buffer_write_string(
                (*ctx).buf,
                (*(*cur).ns).prefix.load(Ordering::Relaxed) as _,
            );
            xml_output_buffer_write_string((*ctx).buf, c":".as_ptr() as _);
        }
        xml_output_buffer_write_string((*ctx).buf, (*cur).name as _);
        xml_output_buffer_write_string((*ctx).buf, c">".as_ptr() as _);
        if parent_is_doc != 0 {
            /* restore this flag from the stack for next node */
            (*ctx).parent_is_doc = parent_is_doc;
            (*ctx).pos = XmlC14NPosition::XmlC14NAfterDocumentElement;
        }
    }

    /*
     * Restore ns_rendered stack position
     */
    xml_c14n_visible_ns_stack_restore((*ctx).ns_rendered, addr_of_mut!(state));
    0
}

unsafe extern "C" fn xml_c11n_normalize_text(a: *const u8) -> *mut XmlChar {
    xml_c11n_normalize_string(a, XmlC14NNormalizationMode::XmlC14NNormalizeText)
}

unsafe extern "C" fn xml_c11n_normalize_comment(a: *const u8) -> *mut XmlChar {
    xml_c11n_normalize_string(a, XmlC14NNormalizationMode::XmlC14NNormalizeComment)
}

unsafe extern "C" fn xml_c11n_normalize_pi(a: *const u8) -> *mut XmlChar {
    xml_c11n_normalize_string(a, XmlC14NNormalizationMode::XmlC14NNormalizePI)
}

/**
 * xmlC14NErrInvalidNode:
 * @extra:  extra information
 *
 * Handle a redefinition of invalid node error
 */
unsafe extern "C" fn xml_c14n_err_invalid_node(node_type: *const c_char, extra: *const c_char) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromC14N,
        XmlParserErrors::XmlC14nInvalidNode,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        extra,
        null_mut(),
        None,
        0,
        0,
        c"Node %s is invalid here : %s\n".as_ptr() as _,
        node_type,
        extra
    );
}

/**
 * xmlC14NErrUnknownNode:
 * @extra:  extra information
 *
 * Handle a redefinition of unknown node error
 */
unsafe extern "C" fn xml_c14n_err_unknown_node(node_type: c_int, extra: *const c_char) {
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromC14N,
        XmlParserErrors::XmlC14nUnknowNode,
        XmlErrorLevel::XmlErrError,
        null_mut(),
        0,
        extra,
        null_mut(),
        None,
        0,
        0,
        c"Unknown node type %d found : %s\n".as_ptr() as _,
        node_type,
        extra
    );
}

/**
 * xmlC14NProcessNode:
 * @ctx:        the pointer to C14N context object
 * @cur:        the node to process
 *
 * Processes the given node
 *
 * Returns non-negative value on success or negative value on fail
 */
unsafe extern "C" fn xml_c14n_process_node(ctx: XmlC14NCtxPtr, cur: XmlNodePtr) -> c_int {
    let mut ret: c_int = 0;

    if ctx.is_null() || cur.is_null() {
        xml_c14n_err_param(c"processing node".as_ptr() as _);
        return -1;
    }

    let visible: c_int = xml_c14n_is_visible!(ctx, cur, (*cur).parent);
    match (*cur).typ {
        XmlElementType::XmlElementNode => {
            ret = xml_c14n_process_element_node(ctx, cur, visible);
        }
        XmlElementType::XmlCdataSectionNode | XmlElementType::XmlTextNode => {
            /*
             * Text Nodes
             * the string value, except all ampersands are replaced
             * by &amp;, all open angle brackets (<) are replaced by &lt;, all closing
             * angle brackets (>) are replaced by &gt;, and all #xD characters are
             * replaced by &#xD;.
             */
            /* cdata sections are processed as text nodes */
            /* todo: verify that cdata sections are included in XPath nodes set */
            if visible != 0 && !(*cur).content.is_null() {
                let buffer: *mut XmlChar = xml_c11n_normalize_text((*cur).content);
                if !buffer.is_null() {
                    xml_output_buffer_write_string((*ctx).buf, buffer as _);
                    xml_free(buffer as _);
                } else {
                    xml_c14n_err_internal(c"normalizing text node".as_ptr() as _);
                    return -1;
                }
            }
        }
        XmlElementType::XmlPiNode => {
            /*
             * Processing Instruction (PI) Nodes-
             * The opening PI symbol (<?), the PI target name of the node,
             * a leading space and the string value if it is not empty, and
             * the closing PI symbol (?>). If the string value is empty,
             * then the leading space is not added. Also, a trailing #xA is
             * rendered after the closing PI symbol for PI children of the
             * root node with a lesser document order than the document
             * element, and a leading #xA is rendered before the opening PI
             * symbol of PI children of the root node with a greater document
             * order than the document element.
             */
            if visible != 0 {
                if matches!((*ctx).pos, XmlC14NPosition::XmlC14NAfterDocumentElement) {
                    xml_output_buffer_write_string((*ctx).buf, c"\x0A<?".as_ptr() as _);
                } else {
                    xml_output_buffer_write_string((*ctx).buf, c"<?".as_ptr() as _);
                }

                xml_output_buffer_write_string((*ctx).buf, (*cur).name as _);
                if !(*cur).content.is_null() && *(*cur).content != b'\0' {
                    xml_output_buffer_write_string((*ctx).buf, c" ".as_ptr() as _);

                    /* todo: do we need to normalize pi? */
                    let buffer: *mut XmlChar = xml_c11n_normalize_pi((*cur).content);
                    if !buffer.is_null() {
                        xml_output_buffer_write_string((*ctx).buf, buffer as _);
                        xml_free(buffer as _);
                    } else {
                        xml_c14n_err_internal(c"normalizing pi node".as_ptr() as _);
                        return -1;
                    }
                }

                if matches!((*ctx).pos, XmlC14NPosition::XmlC14NBeforeDocumentElement) {
                    xml_output_buffer_write_string((*ctx).buf, c"?>\x0A".as_ptr() as _);
                } else {
                    xml_output_buffer_write_string((*ctx).buf, c"?>".as_ptr() as _);
                }
            }
        }
        XmlElementType::XmlCommentNode => {
            /*
             * Comment Nodes
             * Nothing if generating canonical XML without  comments. For
             * canonical XML with comments, generate the opening comment
             * symbol (<!--), the string value of the node, and the
             * closing comment symbol (-->). Also, a trailing #xA is rendered
             * after the closing comment symbol for comment children of the
             * root node with a lesser document order than the document
             * element, and a leading #xA is rendered before the opening
             * comment symbol of comment children of the root node with a
             * greater document order than the document element. (Comment
             * children of the root node represent comments outside of the
             * top-level document element and outside of the document type
             * declaration).
             */
            if visible != 0 && (*ctx).with_comments != 0 {
                if matches!((*ctx).pos, XmlC14NPosition::XmlC14NAfterDocumentElement) {
                    xml_output_buffer_write_string((*ctx).buf, c"\x0A<!--".as_ptr() as _);
                } else {
                    xml_output_buffer_write_string((*ctx).buf, c"<!--".as_ptr() as _);
                }

                if !(*cur).content.is_null() {
                    /* todo: do we need to normalize comment? */
                    let buffer: *mut XmlChar = xml_c11n_normalize_comment((*cur).content);
                    if !buffer.is_null() {
                        xml_output_buffer_write_string((*ctx).buf, buffer as _);
                        xml_free(buffer as _);
                    } else {
                        xml_c14n_err_internal(c"normalizing comment node".as_ptr() as _);
                        return -1;
                    }
                }

                if matches!((*ctx).pos, XmlC14NPosition::XmlC14NBeforeDocumentElement) {
                    xml_output_buffer_write_string((*ctx).buf, c"-->\x0A".as_ptr() as _);
                } else {
                    xml_output_buffer_write_string((*ctx).buf, c"-->".as_ptr() as _);
                }
            }
        }
        XmlElementType::XmlDocumentNode | XmlElementType::XmlDocumentFragNode => {
            /* should be processed as document? */
            if !(*cur).children.is_null() {
                (*ctx).pos = XmlC14NPosition::XmlC14NBeforeDocumentElement;
                (*ctx).parent_is_doc = 1;
                ret = xml_c14n_process_node_list(ctx, (*cur).children);
            }
        }
        #[cfg(feature = "html")]
        XmlElementType::XmlHtmlDocumentNode => {
            /* should be processed as document? */
            if !(*cur).children.is_null() {
                (*ctx).pos = XmlC14NPosition::XmlC14NBeforeDocumentElement;
                (*ctx).parent_is_doc = 1;
                ret = xml_c14n_process_node_list(ctx, (*cur).children);
            }
        }

        XmlElementType::XmlAttributeNode => {
            xml_c14n_err_invalid_node(
                c"XML_ATTRIBUTE_NODE".as_ptr() as _,
                c"processing node".as_ptr() as _,
            );
            return -1;
        }
        XmlElementType::XmlNamespaceDecl => {
            xml_c14n_err_invalid_node(
                c"XML_NAMESPACE_DECL".as_ptr() as _,
                c"processing node".as_ptr() as _,
            );
            return -1;
        }
        XmlElementType::XmlEntityRefNode => {
            xml_c14n_err_invalid_node(
                c"XML_ENTITY_REF_NODE".as_ptr() as _,
                c"processing node".as_ptr() as _,
            );
            return -1;
        }
        XmlElementType::XmlEntityNode => {
            xml_c14n_err_invalid_node(
                c"XML_ENTITY_NODE".as_ptr() as _,
                c"processing node".as_ptr() as _,
            );
            return -1;
        }

        XmlElementType::XmlDocumentTypeNode
        | XmlElementType::XmlNotationNode
        | XmlElementType::XmlDtdNode
        | XmlElementType::XmlElementDecl
        | XmlElementType::XmlAttributeDecl
        | XmlElementType::XmlEntityDecl => {
            /*
             * should be ignored according to "W3C Canonical XML"
             */
        }
        #[cfg(feature = "xinclude")]
        XmlElementType::XmlXincludeStart | XmlElementType::XmlXincludeEnd => {
            /*
             * should be ignored according to "W3C Canonical XML"
             */
        }
        _ => {
            xml_c14n_err_unknown_node((*cur).typ as i32, c"processing node".as_ptr() as _);
            return -1;
        }
    }

    ret
}

/**
 * xmlC14NProcessNodeList:
 * @ctx:        the pointer to C14N context object
 * @cur:        the node to start from
 *
 * Processes all nodes in the row starting from cur.
 *
 * Returns non-negative value on success or negative value on fail
 */
unsafe extern "C" fn xml_c14n_process_node_list(ctx: XmlC14NCtxPtr, mut cur: XmlNodePtr) -> c_int {
    let mut ret: c_int;

    if ctx.is_null() {
        xml_c14n_err_param(c"processing node list".as_ptr() as _);
        return -1;
    }

    ret = 0;
    while !cur.is_null() && ret >= 0 {
        ret = xml_c14n_process_node(ctx, cur);
        cur = (*cur).next;
    }
    ret
}

/**
 * xmlC14NExecute:
 * @doc:        the XML document for canonization
 * @is_visible_callback:the function to use to determine is node visible
 *            or not
 * @user_data:        the first parameter for @is_visible_callback function
 *            (in most cases, it is nodes set)
 * @mode:    the c14n mode (see @xmlC14NMode)
 * @inclusive_ns_prefixes: the list of inclusive namespace prefixes
 *            ended with a NULL or NULL if there is no
 *            inclusive namespaces (only for exclusive
 *            canonicalization, ignored otherwise)
 * @with_comments:    include comments in the result (!=0) or not (==0)
 * @buf:        the output buffer to store canonical XML; this
 *            buffer MUST have encoder==NULL because C14N requires
 *            UTF-8 output
 *
 * Dumps the canonized image of given XML document into the provided buffer.
 * For details see "Canonical XML" (http://www.w3.org/TR/xml-c14n) or
 * "Exclusive XML Canonicalization" (http://www.w3.org/TR/xml-exc-c14n)
 *
 * Returns non-negative value on success or a negative value on fail
 */
pub unsafe extern "C" fn xml_c14n_execute(
    doc: XmlDocPtr,
    is_visible_callback: XmlC14NIsVisibleCallback,
    user_data: *mut c_void,
    mode: c_int,
    /* a xmlC14NMode */ inclusive_ns_prefixes: *mut *mut XmlChar,
    with_comments: c_int,
    buf: XmlOutputBufferPtr,
) -> c_int {
    let mut ret: c_int;

    if buf.is_null() || doc.is_null() {
        xml_c14n_err_param(c"executing c14n".as_ptr() as _);
        return -1;
    }

    /* for backward compatibility, we have to have "mode" as "c_int"
    and here we check that user gives valid value */
    let c14n_mode = match XmlC14NMode::try_from(mode) {
        Ok(mode @ XmlC14NMode::XmlC14N1_0)
        | Ok(mode @ XmlC14NMode::XmlC14NExclusive1_0)
        | Ok(mode @ XmlC14NMode::XmlC14N1_1) => mode,
        _ => {
            xml_c14n_err_param(c"invalid mode for executing c14n".as_ptr() as _);
            return -1;
        }
    };

    /*
     *  Validate the encoding output buffer encoding
     */
    if (*buf).encoder.is_some() {
        xml_c14n_err(
            null_mut(),
            doc as XmlNodePtr,
            XmlParserErrors::XmlC14nRequiresUtf8,
            c"xmlC14NExecute: output buffer encoder != NULL but C14N requires UTF8 output\n"
                .as_ptr() as _,
        );
        return -1;
    }

    let ctx: XmlC14NCtxPtr = xml_c14n_new_ctx(
        doc,
        Some(is_visible_callback),
        user_data,
        c14n_mode,
        inclusive_ns_prefixes,
        with_comments,
        buf,
    );
    if ctx.is_null() {
        xml_c14n_err(
            null_mut(),
            doc as XmlNodePtr,
            XmlParserErrors::XmlC14nCreateCtxt,
            c"xmlC14NExecute: unable to create C14N context\n".as_ptr() as _,
        );
        return -1;
    }

    /*
     * Root Node
     * The root node is the parent of the top-level document element. The
     * result of processing each of its child nodes that is in the node-set
     * in document order. The root node does not generate a byte order mark,
     * XML declaration, nor anything from within the document type
     * declaration.
     */
    if !(*doc).children.is_null() {
        ret = xml_c14n_process_node_list(ctx, (*doc).children);
        if ret < 0 {
            xml_c14n_err_internal(c"processing docs children list".as_ptr() as _);
            xml_c14n_free_ctx(ctx);
            return -1;
        }
    }

    /*
     * Flush buffer to get number of bytes written
     */
    ret = xml_output_buffer_flush(buf);
    if ret < 0 {
        xml_c14n_err_internal(c"flushing output buffer".as_ptr() as _);
        xml_c14n_free_ctx(ctx);
        return -1;
    }

    /*
     * Cleanup
     */
    xml_c14n_free_ctx(ctx);
    ret
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_c14n_doc_dump_memory() {
        #[cfg(all(feature = "libxml_c14n", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_nodes in 0..GEN_NB_XML_NODE_SET_PTR {
                    for n_mode in 0..GEN_NB_INT {
                        for n_inclusive_ns_prefixes in 0..GEN_NB_XML_CHAR_PTR_PTR {
                            for n_with_comments in 0..GEN_NB_INT {
                                for n_doc_txt_ptr in 0..GEN_NB_XML_CHAR_PTR_PTR {
                                    let mem_base = xml_mem_blocks();
                                    let doc = gen_xml_doc_ptr(n_doc, 0);
                                    let nodes = gen_xml_node_set_ptr(n_nodes, 1);
                                    let mode = gen_int(n_mode, 2);
                                    let inclusive_ns_prefixes =
                                        gen_xml_char_ptr_ptr(n_inclusive_ns_prefixes, 3);
                                    let with_comments = gen_int(n_with_comments, 4);
                                    let doc_txt_ptr = gen_xml_char_ptr_ptr(n_doc_txt_ptr, 5);

                                    let ret_val = xml_c14n_doc_dump_memory(
                                        doc,
                                        nodes,
                                        mode,
                                        inclusive_ns_prefixes,
                                        with_comments,
                                        doc_txt_ptr,
                                    );
                                    desret_int(ret_val);
                                    des_xml_doc_ptr(n_doc, doc, 0);
                                    des_xml_node_set_ptr(n_nodes, nodes, 1);
                                    des_int(n_mode, mode, 2);
                                    des_xml_char_ptr_ptr(
                                        n_inclusive_ns_prefixes,
                                        inclusive_ns_prefixes,
                                        3,
                                    );
                                    des_int(n_with_comments, with_comments, 4);
                                    des_xml_char_ptr_ptr(n_doc_txt_ptr, doc_txt_ptr, 5);
                                    reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlC14NDocDumpMemory",
                                            xml_mem_blocks() - mem_base
                                        );
                                        eprint!(" {}", n_doc);
                                        eprint!(" {}", n_nodes);
                                        eprint!(" {}", n_mode);
                                        eprint!(" {}", n_inclusive_ns_prefixes);
                                        eprint!(" {}", n_with_comments);
                                        eprintln!(" {}", n_doc_txt_ptr);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlC14NDocDumpMemory()"
            );
        }
    }

    #[test]
    fn test_xml_c14n_doc_save() {
        #[cfg(all(feature = "libxml_c14n", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_nodes in 0..GEN_NB_XML_NODE_SET_PTR {
                    for n_mode in 0..GEN_NB_INT {
                        for n_inclusive_ns_prefixes in 0..GEN_NB_XML_CHAR_PTR_PTR {
                            for n_with_comments in 0..GEN_NB_INT {
                                for n_filename in 0..GEN_NB_FILEOUTPUT {
                                    for n_compression in 0..GEN_NB_INT {
                                        let mem_base = xml_mem_blocks();
                                        let doc = gen_xml_doc_ptr(n_doc, 0);
                                        let nodes = gen_xml_node_set_ptr(n_nodes, 1);
                                        let mode = gen_int(n_mode, 2);
                                        let inclusive_ns_prefixes =
                                            gen_xml_char_ptr_ptr(n_inclusive_ns_prefixes, 3);
                                        let with_comments = gen_int(n_with_comments, 4);
                                        let filename = gen_fileoutput(n_filename, 5);
                                        let compression = gen_int(n_compression, 6);

                                        let ret_val = xml_c14n_doc_save(
                                            doc,
                                            nodes,
                                            mode,
                                            inclusive_ns_prefixes,
                                            with_comments,
                                            filename,
                                            compression,
                                        );
                                        desret_int(ret_val);
                                        des_xml_doc_ptr(n_doc, doc, 0);
                                        des_xml_node_set_ptr(n_nodes, nodes, 1);
                                        des_int(n_mode, mode, 2);
                                        des_xml_char_ptr_ptr(
                                            n_inclusive_ns_prefixes,
                                            inclusive_ns_prefixes,
                                            3,
                                        );
                                        des_int(n_with_comments, with_comments, 4);
                                        des_fileoutput(n_filename, filename, 5);
                                        des_int(n_compression, compression, 6);
                                        reset_last_error();
                                        if mem_base != xml_mem_blocks() {
                                            leaks += 1;
                                            eprint!(
                                                "Leak of {} blocks found in xmlC14NDocSave",
                                                xml_mem_blocks() - mem_base
                                            );
                                            eprint!(" {}", n_doc);
                                            eprint!(" {}", n_nodes);
                                            eprint!(" {}", n_mode);
                                            eprint!(" {}", n_inclusive_ns_prefixes);
                                            eprint!(" {}", n_with_comments);
                                            eprint!(" {}", n_filename);
                                            eprintln!(" {}", n_compression);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlC14NDocSave()");
        }
    }

    #[test]
    fn test_xml_c14n_doc_save_to() {
        #[cfg(all(feature = "libxml_c14n", feature = "output"))]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_nodes in 0..GEN_NB_XML_NODE_SET_PTR {
                    for n_mode in 0..GEN_NB_INT {
                        for n_inclusive_ns_prefixes in 0..GEN_NB_XML_CHAR_PTR_PTR {
                            for n_with_comments in 0..GEN_NB_INT {
                                for n_buf in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
                                    let mem_base = xml_mem_blocks();
                                    let doc = gen_xml_doc_ptr(n_doc, 0);
                                    let nodes = gen_xml_node_set_ptr(n_nodes, 1);
                                    let mode = gen_int(n_mode, 2);
                                    let inclusive_ns_prefixes =
                                        gen_xml_char_ptr_ptr(n_inclusive_ns_prefixes, 3);
                                    let with_comments = gen_int(n_with_comments, 4);
                                    let buf = gen_xml_output_buffer_ptr(n_buf, 5);

                                    let ret_val = xml_c14n_doc_save_to(
                                        doc,
                                        nodes,
                                        mode,
                                        inclusive_ns_prefixes,
                                        with_comments,
                                        buf,
                                    );
                                    desret_int(ret_val);
                                    des_xml_doc_ptr(n_doc, doc, 0);
                                    des_xml_node_set_ptr(n_nodes, nodes, 1);
                                    des_int(n_mode, mode, 2);
                                    des_xml_char_ptr_ptr(
                                        n_inclusive_ns_prefixes,
                                        inclusive_ns_prefixes,
                                        3,
                                    );
                                    des_int(n_with_comments, with_comments, 4);
                                    des_xml_output_buffer_ptr(n_buf, buf, 5);
                                    reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlC14NDocSaveTo",
                                            xml_mem_blocks() - mem_base
                                        );
                                        eprint!(" {}", n_doc);
                                        eprint!(" {}", n_nodes);
                                        eprint!(" {}", n_mode);
                                        eprint!(" {}", n_inclusive_ns_prefixes);
                                        eprint!(" {}", n_with_comments);
                                        eprintln!(" {}", n_buf);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlC14NDocSaveTo()");
        }
    }

    #[test]
    fn test_xml_c14n_execute() {

        /* missing type support */
    }
}

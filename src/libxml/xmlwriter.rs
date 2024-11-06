//! Provide methods and data structures for text writing APIs.  
//! This module is based on `libxml/xmlwriter.h`, `xmlwriter.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.
//!
//! The original API has functions that support the format, but not here.
//!
//! Although Rust has no variable-length arguments,
//! it is simpler and more natural to limit the API to writing a single string,
//! since it is easy to write the format to a single String using `format!`.

use std::{
    cell::RefCell,
    ffi::{c_char, CStr, CString},
    mem::{size_of, size_of_val, zeroed},
    os::raw::c_void,
    ptr::{addr_of_mut, null_mut},
    rc::Rc,
    slice::from_raw_parts,
};

use libc::memset;

use crate::{
    __xml_raise_error,
    buf::XmlBufRef,
    encoding::find_encoding_handler,
    globals::GenericErrorContext,
    io::{
        xml_output_buffer_close, xml_output_buffer_create_filename, xml_output_buffer_create_io,
        XmlOutputBufferPtr,
    },
    libxml::{
        entities::xml_encode_special_chars,
        globals::{xml_free, xml_malloc},
        htmltree::html_new_doc_no_dtd,
        list::{
            xml_link_get_data, xml_list_create, xml_list_delete, xml_list_empty, xml_list_front,
            xml_list_pop_front, xml_list_push_front, xml_list_search, xml_list_size, XmlLinkPtr,
            XmlListPtr,
        },
        parser::{
            xml_create_push_parser_ctxt, xml_free_parser_ctxt, xml_parse_chunk, XmlParserCtxtPtr,
            XmlParserInputState, XmlSAXHandler, XML_DEFAULT_VERSION,
        },
        sax2::{xml_sax2_end_element, xml_sax2_init_default_sax_handler, xml_sax2_start_element},
        tree::{xml_free_doc, xml_new_doc, xml_set_doc_compress_mode, XmlDocPtr, XmlNodePtr},
        uri::xml_canonic_path,
        xmlerror::XmlParserErrors,
        xmlstring::{xml_strcasecmp, xml_strcat, xml_strcmp, xml_strdup, xml_strlen, XmlChar},
    },
    private::save::xml_buf_attr_serialize_txt_content,
};

/*
 * Types are kept private
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum XmlTextWriterState {
    XmlTextwriterNone = 0,
    XmlTextwriterName,
    XmlTextwriterAttribute,
    XmlTextwriterText,
    XmlTextwriterPI,
    XmlTextwriterPIText,
    XmlTextwriterCdata,
    XmlTextwriterDtd,
    XmlTextwriterDtdText,
    XmlTextwriterDtdElem,
    XmlTextwriterDtdElemText,
    XmlTextwriterDtdAttl,
    XmlTextwriterDtdAttlText,
    XmlTextwriterDtdEnty, /* entity */
    XmlTextwriterDtdEntyText,
    XmlTextwriterDtdPEnt, /* parameter entity */
    XmlTextwriterComment,
}

#[repr(C)]
struct XmlTextWriterStackEntry {
    name: *mut XmlChar,
    state: XmlTextWriterState,
}

#[repr(C)]
struct XmlTextWriterNsStackEntry {
    prefix: *mut XmlChar,
    uri: *mut XmlChar,
    elem: XmlLinkPtr,
}

pub type XmlTextWriterPtr = *mut XmlTextWriter;
#[repr(C)]
pub struct XmlTextWriter {
    out: XmlOutputBufferPtr, /* output buffer */
    nodes: XmlListPtr,       /* element name stack */
    nsstack: XmlListPtr,     /* name spaces stack */
    level: i32,
    indent: i32,         /* enable indent */
    doindent: i32,       /* internal indent flag */
    ichar: *mut XmlChar, /* indent character */
    qchar: c_char,       /* character used for quoting attribute values */
    ctxt: XmlParserCtxtPtr,
    no_doc_free: i32,
    doc: XmlDocPtr,
}

/**
 * xmlFreeTextWriterStackEntry:
 * @lk:  the xmlLinkPtr
 *
 * Free callback for the xmlList.
 */
extern "C" fn xml_free_text_writer_stack_entry(data: *mut c_void) {
    let p: *mut XmlTextWriterStackEntry = data as _;
    if p.is_null() {
        return;
    }

    unsafe {
        if !(*p).name.is_null() {
            xml_free((*p).name as _);
        }
        xml_free(p as _);
    }
}

/**
 * xmlCmpTextWriterStackEntry:
 * @data0:  the first data
 * @data1:  the second data
 *
 * Compare callback for the xmlList.
 *
 * Returns -1, 0, 1
 */
extern "C" fn xml_cmp_text_writer_stack_entry(data0: *const c_void, data1: *const c_void) -> i32 {
    if data0 == data1 {
        return 0;
    }

    if data0.is_null() {
        return -1;
    }

    if data1.is_null() {
        return 1;
    }

    let p0: *mut XmlTextWriterStackEntry = data0 as _;
    let p1: *mut XmlTextWriterStackEntry = data1 as _;

    unsafe { xml_strcmp((*p0).name, (*p1).name) }
}

/**
 * xmlFreeTextWriterNsStackEntry:
 * @lk:  the xmlLinkPtr
 *
 * Free callback for the xmlList.
 */
extern "C" fn xml_free_text_writer_ns_stack_entry(data: *mut c_void) {
    let p: *mut XmlTextWriterNsStackEntry = data as *mut XmlTextWriterNsStackEntry;
    if p.is_null() {
        return;
    }

    unsafe {
        if !(*p).prefix.is_null() {
            xml_free((*p).prefix as _);
        }
        if !(*p).uri.is_null() {
            xml_free((*p).uri as _);
        }

        xml_free(p as _);
    }
}

/**
 * xmlCmpTextWriterNsStackEntry:
 * @data0:  the first data
 * @data1:  the second data
 *
 * Compare callback for the xmlList.
 *
 * Returns -1, 0, 1
 */
extern "C" fn xml_cmp_text_writer_ns_stack_entry(
    data0: *const c_void,
    data1: *const c_void,
) -> i32 {
    let mut rc: i32;

    if data0 == data1 {
        return 0;
    }

    if data0.is_null() {
        return -1;
    }

    if data1.is_null() {
        return 1;
    }

    let p0: *mut XmlTextWriterNsStackEntry = data0 as *mut XmlTextWriterNsStackEntry;
    let p1: *mut XmlTextWriterNsStackEntry = data1 as *mut XmlTextWriterNsStackEntry;

    unsafe {
        rc = xml_strcmp((*p0).prefix, (*p1).prefix);

        if rc != 0 || (*p0).elem != (*p1).elem {
            rc = -1;
        }
    }

    rc
}

/**
 * xmlWriterErrMsg:
 * @ctxt:  a writer context
 * @error:  the error number
 * @msg:  the error message
 *
 * Handle a writer error
 */
unsafe extern "C" fn xml_writer_err_msg(
    ctxt: XmlTextWriterPtr,
    error: XmlParserErrors,
    msg: *const c_char,
) {
    if !ctxt.is_null() {
        __xml_raise_error!(
            None,
            None,
            None,
            (*ctxt).ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromWriter,
            error,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            None,
            None,
            None,
            0,
            0,
            c"%s".as_ptr() as _,
            msg
        );
    } else {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromWriter,
            error,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            None,
            None,
            None,
            0,
            0,
            c"%s".as_ptr() as _,
            msg
        );
    }
}

/*
 * Constructors & Destructor
 */
/**
 * xmlNewTextWriter:
 * @out:  an xmlOutputBufferPtr
 *
 * Create a new xmlNewTextWriter structure using an xmlOutputBufferPtr
 * NOTE: the @out parameter will be deallocated when the writer is closed
 *       (if the call succeed.)
 *
 * Returns the new xmlTextWriterPtr or NULL in case of error
 */
pub unsafe extern "C" fn xml_new_text_writer(out: XmlOutputBufferPtr) -> XmlTextWriterPtr {
    let ret: XmlTextWriterPtr = xml_malloc(size_of::<XmlTextWriter>()) as XmlTextWriterPtr;
    if ret.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrNoMemory,
            c"xmlNewTextWriter : out of memory!\n".as_ptr() as _,
        );
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlTextWriter>());

    (*ret).nodes = xml_list_create(
        Some(xml_free_text_writer_stack_entry),
        Some(xml_cmp_text_writer_stack_entry),
    );
    if (*ret).nodes.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrNoMemory,
            c"xmlNewTextWriter : out of memory!\n".as_ptr() as _,
        );
        xml_free(ret as _);
        return null_mut();
    }

    (*ret).nsstack = xml_list_create(
        Some(xml_free_text_writer_ns_stack_entry),
        Some(xml_cmp_text_writer_ns_stack_entry),
    );
    if (*ret).nsstack.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrNoMemory,
            c"xmlNewTextWriter : out of memory!\n".as_ptr() as _,
        );
        xml_list_delete((*ret).nodes);
        xml_free(ret as _);
        return null_mut();
    }

    (*ret).out = out;
    (*ret).ichar = xml_strdup(c" ".as_ptr() as _);
    (*ret).qchar = b'"' as _;

    if (*ret).ichar.is_null() {
        xml_list_delete((*ret).nodes);
        xml_list_delete((*ret).nsstack);
        xml_free(ret as _);
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrNoMemory,
            c"xmlNewTextWriter : out of memory!\n".as_ptr() as _,
        );
        return null_mut();
    }

    (*ret).doc = xml_new_doc(None);

    (*ret).no_doc_free = 0;

    ret
}

/**
 * xmlNewTextWriterFilename:
 * @uri:  the URI of the resource for the output
 * @compression:  compress the output?
 *
 * Create a new xmlNewTextWriter structure with @uri as output
 *
 * Returns the new xmlTextWriterPtr or NULL in case of error
 */
pub unsafe extern "C" fn xml_new_text_writer_filename(
    uri: *const c_char,
    compression: i32,
) -> XmlTextWriterPtr {
    let out: XmlOutputBufferPtr = xml_output_buffer_create_filename(uri, None, compression);
    if out.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlIOEIO,
            c"xmlNewTextWriterFilename : cannot open uri\n".as_ptr() as _,
        );
        return null_mut();
    }

    let ret: XmlTextWriterPtr = xml_new_text_writer(out);
    if ret.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrNoMemory,
            c"xmlNewTextWriterFilename : out of memory!\n".as_ptr() as _,
        );
        xml_output_buffer_close(out);
        return null_mut();
    }

    (*ret).indent = 0;
    (*ret).doindent = 0;
    ret
}

// /**
//  * xmlNewTextWriterMemory:
//  * @buf:  xmlBufferPtr
//  * @compression:  compress the output?
//  *
//  * Create a new xmlNewTextWriter structure with @buf as output
//  * TODO: handle compression
//  *
//  * Returns the new xmlTextWriterPtr or NULL in case of error
//  */
// pub unsafe extern "C" fn xml_new_text_writer_memory(
//     buf: XmlBufferPtr,
//     _compression: i32,
// ) -> XmlTextWriterPtr {
//     /*::todo handle compression */
//     let out: XmlOutputBufferPtr = xml_output_buffer_create_buffer(buf, null_mut());

//     if out.is_null() {
//         xml_writer_err_msg(
//             null_mut(),
//             XmlParserErrors::XmlErrNoMemory,
//             c"xmlNewTextWriterMemory : out of memory!\n".as_ptr() as _,
//         );
//         return null_mut();
//     }

//     let ret: XmlTextWriterPtr = xml_new_text_writer(out);
//     if ret.is_null() {
//         xml_writer_err_msg(
//             null_mut(),
//             XmlParserErrors::XmlErrNoMemory,
//             c"xmlNewTextWriterMemory : out of memory!\n".as_ptr() as _,
//         );
//         xml_output_buffer_close(out);
//         return null_mut();
//     }

//     ret
// }

/**
 * xmlWriterErrMsgInt:
 * @ctxt:  a writer context
 * @error:  the error number
 * @msg:  the error message
 * @val:  an int
 *
 * Handle a writer error
 */
unsafe extern "C" fn xml_writer_err_msg_int(
    ctxt: XmlTextWriterPtr,
    error: XmlParserErrors,
    msg: *const c_char,
    val: i32,
) {
    if !ctxt.is_null() {
        __xml_raise_error!(
            None,
            None,
            None,
            (*ctxt).ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromWriter,
            error,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            None,
            None,
            None,
            val,
            0,
            msg,
            val
        );
    } else {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromWriter,
            error,
            XmlErrorLevel::XmlErrFatal,
            null_mut(),
            0,
            None,
            None,
            None,
            val,
            0,
            msg,
            val
        );
    }
}

/**
 * xmlTextWriterWriteDocCallback:
 * @context:  the xmlBufferPtr
 * @str:  the data to write
 * @len:  the length of the data
 *
 * Write callback for the xmlOutputBuffer with target xmlBuffer
 *
 * Returns -1, 0, 1
 */
unsafe extern "C" fn xml_text_writer_write_doc_callback(
    context: *mut c_void,
    str: *const c_char,
    len: i32,
) -> i32 {
    let ctxt: XmlParserCtxtPtr = context as XmlParserCtxtPtr;
    let rc: i32;

    let res = {
        rc = xml_parse_chunk(ctxt, str, len, 0);
        rc != 0
    };
    if res {
        xml_writer_err_msg_int(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterWriteDocCallback : XML error %d !\n".as_ptr() as _,
            rc,
        );
        return -1;
    }

    len
}

/**
 * xmlTextWriterCloseDocCallback:
 * @context:  the xmlBufferPtr
 *
 * Close callback for the xmlOutputBuffer with target xmlBuffer
 *
 * Returns -1, 0, 1
 */
unsafe extern "C" fn xml_text_writer_close_doc_callback(context: *mut c_void) -> i32 {
    let ctxt: XmlParserCtxtPtr = context as XmlParserCtxtPtr;
    let rc: i32;

    let res = {
        rc = xml_parse_chunk(ctxt, null_mut(), 0, 1);
        rc != 0
    };
    if res {
        xml_writer_err_msg_int(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterCloseDocCallback : XML error %d !\n".as_ptr() as _,
            rc,
        );
        return -1;
    }

    0
}

/**
 * xmlNewTextWriterPushParser:
 * @ctxt: xmlParserCtxtPtr to hold the new XML document tree
 * @compression:  compress the output?
 *
 * Create a new xmlNewTextWriter structure with @ctxt as output
 * NOTE: the @ctxt context will be freed with the resulting writer
 *       (if the call succeeds).
 * TODO: handle compression
 *
 * Returns the new xmlTextWriterPtr or NULL in case of error
 */
pub unsafe extern "C" fn xml_new_text_writer_push_parser(
    ctxt: XmlParserCtxtPtr,
    _compression: i32,
) -> XmlTextWriterPtr {
    if ctxt.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"xmlNewTextWriterPushParser : invalid context!\n".as_ptr() as _,
        );
        return null_mut();
    }

    let out: XmlOutputBufferPtr = xml_output_buffer_create_io(
        Some(xml_text_writer_write_doc_callback),
        Some(xml_text_writer_close_doc_callback),
        ctxt as _,
        None,
    );
    if out.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"xmlNewTextWriterPushParser : error at xmlOutputBufferCreateIO!\n".as_ptr() as _,
        );
        return null_mut();
    }

    let ret: XmlTextWriterPtr = xml_new_text_writer(out);
    if ret.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"xmlNewTextWriterPushParser : error at xmlNewTextWriter!\n".as_ptr() as _,
        );
        xml_output_buffer_close(out);
        return null_mut();
    }

    (*ret).ctxt = ctxt;

    ret
}

/**
 * xmlTextWriterStartDocumentCallback:
 * @ctx: the user data (XML parser context)
 *
 * called at the start of document processing.
 */
unsafe fn xml_text_writer_start_document_callback(ctx: Option<GenericErrorContext>) {
    let ctxt = {
        let ctx = ctx.as_ref().unwrap();
        let lock = ctx.lock();
        *lock.downcast_ref::<XmlParserCtxtPtr>().unwrap()
    };
    let mut doc: XmlDocPtr;

    if (*ctxt).html != 0 {
        #[cfg(feature = "html")]
        {
            if (*ctxt).my_doc.is_null() {
                (*ctxt).my_doc = html_new_doc_no_dtd(null_mut(), null_mut());
            }
            if (*ctxt).my_doc.is_null() {
                if !(*ctxt).sax.is_null() && (*(*ctxt).sax).error.is_some() {
                    (*(*ctxt).sax).error.unwrap()(
                        (*ctxt).user_data.clone(),
                        "SAX.startDocument(): out of memory\n",
                    );
                }
                (*ctxt).err_no = XmlParserErrors::XmlErrNoMemory as i32;
                (*ctxt).instate = XmlParserInputState::XmlParserEOF;
                (*ctxt).disable_sax = 1;
                return;
            }
        }
        #[cfg(not(feature = "html"))]
        {
            xml_writer_err_msg(
                null_mut(),
                XmlParserErrors::XmlErrInternalError,
                c"libxml2 built without HTML support\n".as_ptr() as _,
            );
            (*ctxt).errNo = XmlParserErrors::XmlErrInternalError as i32;
            (*ctxt).instate = XmlParserInputState::XmlParserEOF;
            (*ctxt).disableSAX = 1;
            return;
        }
    } else {
        doc = (*ctxt).my_doc;
        if doc.is_null() {
            doc = xml_new_doc((*ctxt).version.as_deref());
            (*ctxt).my_doc = doc;
        }
        if !doc.is_null() {
            if (*doc).children.is_null() {
                (*doc).encoding = (*ctxt).encoding().map(|e| e.to_owned());
                (*doc).standalone = (*ctxt).standalone;
            }
        } else {
            if !(*ctxt).sax.is_null() && (*(*ctxt).sax).error.is_some() {
                (*(*ctxt).sax).error.unwrap()(
                    (*ctxt).user_data.clone(),
                    "SAX.startDocument(): out of memory\n",
                );
            }
            (*ctxt).err_no = XmlParserErrors::XmlErrNoMemory as i32;
            (*ctxt).instate = XmlParserInputState::XmlParserEOF;
            (*ctxt).disable_sax = 1;
            return;
        }
    }
    if !(*ctxt).my_doc.is_null()
        && (*(*ctxt).my_doc).url.is_none()
        && !(*ctxt).input.is_null()
        && (*(*ctxt).input).filename.is_some()
    {
        let filename = CString::new((*(*ctxt).input).filename.as_deref().unwrap()).unwrap();
        let url = xml_canonic_path(filename.as_ptr() as _);
        if !url.is_null() {
            (*(*ctxt).my_doc).url = Some(
                CStr::from_ptr(url as *const i8)
                    .to_string_lossy()
                    .into_owned(),
            );
            xml_free(url as _);
        }
        if (*(*ctxt).my_doc).url.is_none() {
            (*(*ctxt).my_doc).url = (*(*ctxt).input).filename.clone()
        }
    }
}

/**
 * xmlNewTextWriterDoc:
 * @doc: address of a xmlDocPtr to hold the new XML document tree
 * @compression:  compress the output?
 *
 * Create a new xmlNewTextWriter structure with @*doc as output
 *
 * Returns the new xmlTextWriterPtr or NULL in case of error
 */
pub unsafe extern "C" fn xml_new_text_writer_doc(
    doc: *mut XmlDocPtr,
    compression: i32,
) -> XmlTextWriterPtr {
    let mut sax_handler: XmlSAXHandler = unsafe { zeroed() };

    memset(
        addr_of_mut!(sax_handler) as _,
        b'\0' as _,
        size_of_val(&sax_handler),
    );
    xml_sax2_init_default_sax_handler(addr_of_mut!(sax_handler), 1);
    sax_handler.start_document = Some(xml_text_writer_start_document_callback);
    sax_handler.start_element = Some(xml_sax2_start_element);
    sax_handler.end_element = Some(xml_sax2_end_element);

    let ctxt: XmlParserCtxtPtr =
        xml_create_push_parser_ctxt(addr_of_mut!(sax_handler), None, null_mut(), 0, null_mut());
    if ctxt.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            "xmlNewTextWriterDoc : error at xmlCreatePushParserCtxt!\n".as_ptr() as _,
        );
        return null_mut();
    }
    /*
     * For some reason this seems to completely break if node names
     * are interned.
     */
    (*ctxt).dict_names = 0;

    (*ctxt).my_doc = xml_new_doc(Some(XML_DEFAULT_VERSION));
    if (*ctxt).my_doc.is_null() {
        xml_free_parser_ctxt(ctxt);
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"xmlNewTextWriterDoc : error at xmlNewDoc!\n".as_ptr() as _,
        );
        return null_mut();
    }

    let ret: XmlTextWriterPtr = xml_new_text_writer_push_parser(ctxt, compression);
    if ret.is_null() {
        xml_free_doc((*ctxt).my_doc);
        xml_free_parser_ctxt(ctxt);
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            "xmlNewTextWriterDoc : error at xmlNewTextWriterPushParser!\n".as_ptr() as _,
        );
        return null_mut();
    }

    xml_set_doc_compress_mode((*ctxt).my_doc, compression);

    if !doc.is_null() {
        *doc = (*ctxt).my_doc;
        (*ret).no_doc_free = 1;
    }

    ret
}

/**
 * xmlNewTextWriterTree:
 * @doc: xmlDocPtr
 * @node: xmlNodePtr or NULL for (*doc).children
 * @compression:  compress the output?
 *
 * Create a new xmlNewTextWriter structure with @doc as output
 * starting at @node
 *
 * Returns the new xmlTextWriterPtr or NULL in case of error
 */
pub unsafe extern "C" fn xml_new_text_writer_tree(
    doc: XmlDocPtr,
    node: XmlNodePtr,
    compression: i32,
) -> XmlTextWriterPtr {
    let mut sax_handler: XmlSAXHandler = unsafe { zeroed() };

    if doc.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"xmlNewTextWriterTree : invalid document tree!\n".as_ptr() as _,
        );
        return null_mut();
    }

    memset(
        addr_of_mut!(sax_handler) as _,
        b'\0' as _,
        size_of_val(&sax_handler),
    );
    xml_sax2_init_default_sax_handler(addr_of_mut!(sax_handler), 1);
    sax_handler.start_document = Some(xml_text_writer_start_document_callback);
    sax_handler.start_element = Some(xml_sax2_start_element);
    sax_handler.end_element = Some(xml_sax2_end_element);

    let ctxt: XmlParserCtxtPtr =
        xml_create_push_parser_ctxt(addr_of_mut!(sax_handler), None, null_mut(), 0, null_mut());
    if ctxt.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"xmlNewTextWriterDoc : error at xmlCreatePushParserCtxt!\n".as_ptr() as _,
        );
        return null_mut();
    }
    /*
     * For some reason this seems to completely break if node names
     * are interned.
     */
    (*ctxt).dict_names = 0;

    let ret: XmlTextWriterPtr = xml_new_text_writer_push_parser(ctxt, compression);
    if ret.is_null() {
        xml_free_parser_ctxt(ctxt);
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"xmlNewTextWriterDoc : error at xmlNewTextWriterPushParser!\n".as_ptr() as _,
        );
        return null_mut();
    }

    (*ctxt).my_doc = doc;
    (*ctxt).node = node;
    (*ret).no_doc_free = 1;

    xml_set_doc_compress_mode(doc, compression);

    ret
}

/**
 * xmlFreeTextWriter:
 * @writer:  the xmlTextWriterPtr
 *
 * Deallocate all the resources associated to the writer
 */
pub unsafe extern "C" fn xml_free_text_writer(writer: XmlTextWriterPtr) {
    if writer.is_null() {
        return;
    }

    if !(*writer).out.is_null() {
        xml_output_buffer_close((*writer).out);
    }

    if !(*writer).nodes.is_null() {
        xml_list_delete((*writer).nodes);
    }

    if !(*writer).nsstack.is_null() {
        xml_list_delete((*writer).nsstack);
    }

    if !(*writer).ctxt.is_null() {
        if !(*(*writer).ctxt).my_doc.is_null() && (*writer).no_doc_free == 0 {
            xml_free_doc((*(*writer).ctxt).my_doc);
            (*(*writer).ctxt).my_doc = null_mut();
        }
        xml_free_parser_ctxt((*writer).ctxt);
    }

    if !(*writer).doc.is_null() {
        xml_free_doc((*writer).doc);
    }

    if !(*writer).ichar.is_null() {
        xml_free((*writer).ichar as _);
    }
    xml_free(writer as _);
}

/*
 * Functions
 */

/*
 * Document
 */
/**
 * xmlTextWriterStartDocument:
 * @writer:  the xmlTextWriterPtr
 * @version:  the xml version ("1.0") or NULL for default ("1.0")
 * @encoding:  the encoding or NULL for default
 * @standalone: "yes" or "no" or NULL for default
 *
 * Start a new xml document
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_start_document(
    writer: XmlTextWriterPtr,
    version: *const c_char,
    encoding: *const c_char,
    standalone: *const c_char,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() || (*writer).out.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterStartDocument : invalid writer!\n".as_ptr() as _,
        );
        return -1;
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() && !xml_link_get_data(lk).is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterStartDocument : not allowed in this context!\n".as_ptr() as _,
        );
        return -1;
    }

    let encoder = if let Some(Ok(encoding)) =
        (!encoding.is_null()).then(|| CStr::from_ptr(encoding).to_str())
    {
        let Some(encoder) = find_encoding_handler(encoding) else {
            xml_writer_err_msg(
                writer,
                XmlParserErrors::XmlErrUnsupportedEncoding,
                c"xmlTextWriterStartDocument : unsupported encoding\n".as_ptr() as _,
            );
            return -1;
        };
        Some(encoder)
    } else {
        None
    };

    (*(*writer).out).encoder = encoder.map(|e| Rc::new(RefCell::new(e)));
    if (*(*writer).out).encoder.is_some() {
        if (*(*writer).out).conv.is_none() {
            (*(*writer).out).conv = XmlBufRef::with_capacity(4000);
        }
        (*(*writer).out).encode(true);
        if !(*writer).doc.is_null() && (*(*writer).doc).encoding.is_none() {
            let encoder = (*(*writer).out).encoder.as_ref().unwrap().borrow();
            (*(*writer).doc).encoding = Some(encoder.name().to_owned());
        }
    } else {
        (*(*writer).out).conv = None;
    }

    sum = 0;
    count = (*(*writer).out).write_str("<?xml version=");
    if count < 0 {
        return -1;
    }
    sum += count;
    count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
    if count < 0 {
        return -1;
    }
    sum += count;
    if !version.is_null() {
        count = (*(*writer).out).write_str(CStr::from_ptr(version).to_string_lossy().as_ref());
    } else {
        count = (*(*writer).out).write_str("1.0");
    }
    if count < 0 {
        return -1;
    }
    sum += count;
    count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
    if count < 0 {
        return -1;
    }
    sum += count;
    if (*(*writer).out).encoder.is_some() {
        count = (*(*writer).out).write_str(" encoding=");
        if count < 0 {
            return -1;
        }
        sum += count;
        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;
        count =
            (*(*writer).out).write_str((*(*writer).out).encoder.as_ref().unwrap().borrow().name());
        if count < 0 {
            return -1;
        }
        sum += count;
        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    if !standalone.is_null() {
        count = (*(*writer).out).write_str(" standalone=");
        if count < 0 {
            return -1;
        }
        sum += count;
        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;
        count = (*(*writer).out).write_str(CStr::from_ptr(standalone).to_string_lossy().as_ref());
        if count < 0 {
            return -1;
        }
        sum += count;
        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    count = (*(*writer).out).write_str("?>\n");
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterEndDocument:
 * @writer:  the xmlTextWriterPtr
 *
 * End an xml document. All open elements are closed, and
 * the content is flushed to the output.
 *
 * Returns the bytes written or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_end_document(writer: XmlTextWriterPtr) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    let mut lk: XmlLinkPtr;
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterEndDocument : invalid writer!\n".as_ptr() as _,
        );
        return -1;
    }

    sum = 0;
    while {
        lk = xml_list_front((*writer).nodes);
        !lk.is_null()
    } {
        p = xml_link_get_data(lk) as _;
        if p.is_null() {
            break;
        }
        match (*p).state {
            XmlTextWriterState::XmlTextwriterName
            | XmlTextWriterState::XmlTextwriterAttribute
            | XmlTextWriterState::XmlTextwriterText => {
                count = xml_text_writer_end_element(writer);
                if count < 0 {
                    return -1;
                }
                sum += count;
            }
            XmlTextWriterState::XmlTextwriterPI | XmlTextWriterState::XmlTextwriterPIText => {
                count = xml_text_writer_end_pi(writer);
                if count < 0 {
                    return -1;
                }
                sum += count;
            }
            XmlTextWriterState::XmlTextwriterCdata => {
                count = xml_text_writer_end_cdata(writer);
                if count < 0 {
                    return -1;
                }
                sum += count;
            }
            XmlTextWriterState::XmlTextwriterDtd
            | XmlTextWriterState::XmlTextwriterDtdText
            | XmlTextWriterState::XmlTextwriterDtdElem
            | XmlTextWriterState::XmlTextwriterDtdElemText
            | XmlTextWriterState::XmlTextwriterDtdAttl
            | XmlTextWriterState::XmlTextwriterDtdAttlText
            | XmlTextWriterState::XmlTextwriterDtdEnty
            | XmlTextWriterState::XmlTextwriterDtdEntyText
            | XmlTextWriterState::XmlTextwriterDtdPEnt => {
                count = xml_text_writer_end_dtd(writer);
                if count < 0 {
                    return -1;
                }
                sum += count;
            }
            XmlTextWriterState::XmlTextwriterComment => {
                count = xml_text_writer_end_comment(writer);
                if count < 0 {
                    return -1;
                }
                sum += count;
            }
            _ => {}
        }
    }

    if (*writer).indent == 0 {
        count = (*(*writer).out).write_str("\n");
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    sum += xml_text_writer_flush(writer);

    sum
}

/**
 * xmlTextWriterOutputNSDecl:
 * @writer:  the xmlTextWriterPtr
 *
 * Output the current namespace declarations.
 */
unsafe extern "C" fn xml_text_writer_output_nsdecl(writer: XmlTextWriterPtr) -> i32 {
    let mut lk: XmlLinkPtr;
    let mut np: *mut XmlTextWriterNsStackEntry;
    let mut count: i32;
    let mut sum: i32;

    sum = 0;
    while xml_list_empty((*writer).nsstack) == 0 {
        let mut namespace_uri: *mut XmlChar = null_mut();
        let mut prefix: *mut XmlChar = null_mut();

        lk = xml_list_front((*writer).nsstack);
        np = xml_link_get_data(lk) as *mut XmlTextWriterNsStackEntry;

        if !np.is_null() {
            namespace_uri = xml_strdup((*np).uri);
            prefix = xml_strdup((*np).prefix);
        }

        xml_list_pop_front((*writer).nsstack);

        if !np.is_null() {
            count = xml_text_writer_write_attribute(writer, prefix, namespace_uri);
            xml_free(namespace_uri as _);
            xml_free(prefix as _);

            if count < 0 {
                xml_list_delete((*writer).nsstack);
                (*writer).nsstack = null_mut();
                return -1;
            }
            sum += count;
        }
    }
    sum
}

/**
 * xmlTextWriterWriteIndent:
 * @writer:  the xmlTextWriterPtr
 *
 * Write indent string.
 *
 * Returns -1 on error or the number of strings written.
 */
unsafe extern "C" fn xml_text_writer_write_indent(writer: XmlTextWriterPtr) -> i32 {
    let mut ret: i32;

    let lksize: i32 = xml_list_size((*writer).nodes);
    if lksize < 1 {
        return -1; /* list is empty */
    }
    for _ in 0..lksize - 1 {
        ret = (*(*writer).out).write_str(
            CStr::from_ptr((*writer).ichar as _)
                .to_string_lossy()
                .as_ref(),
        );
        if ret == -1 {
            return -1;
        }
    }

    lksize - 1
}

/*
 * Comments
 */
/**
 * xmlTextWriterStartComment:
 * @writer:  the xmlTextWriterPtr
 *
 * Start an xml comment.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_start_comment(writer: XmlTextWriterPtr) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterStartComment : invalid writer!\n".as_ptr() as _,
        );
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            match (*p).state {
                XmlTextWriterState::XmlTextwriterText | XmlTextWriterState::XmlTextwriterNone => {}
                XmlTextWriterState::XmlTextwriterName => {
                    /* Output namespace declarations */
                    count = xml_text_writer_output_nsdecl(writer);
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                    count = (*(*writer).out).write_str(">");
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                    if (*writer).indent != 0 {
                        count = (*(*writer).out).write_str("\n");
                        if count < 0 {
                            return -1;
                        }
                        sum += count;
                    }
                    (*p).state = XmlTextWriterState::XmlTextwriterText;
                }
                _ => {
                    return -1;
                }
            }
        }
    }

    p = xml_malloc(size_of::<XmlTextWriterStackEntry>()) as *mut XmlTextWriterStackEntry;
    if p.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartElement : out of memory!\n".as_ptr() as _,
        );
        return -1;
    }

    (*p).name = null_mut();
    (*p).state = XmlTextWriterState::XmlTextwriterComment;

    xml_list_push_front((*writer).nodes, p as _);

    if (*writer).indent != 0 {
        count = xml_text_writer_write_indent(writer);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    count = (*(*writer).out).write_str("<!--");
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterEndComment:
 * @writer:  the xmlTextWriterPtr
 *
 * End the current xml comment.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_end_comment(writer: XmlTextWriterPtr) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterEndComment : invalid writer!\n".as_ptr() as _,
        );
        return -1;
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterEndComment : not allowed in this context!\n".as_ptr() as _,
        );
        return -1;
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return -1;
    }

    sum = 0;
    match (*p).state {
        XmlTextWriterState::XmlTextwriterComment => {
            count = (*(*writer).out).write_str("-->");
            if count < 0 {
                return -1;
            }
            sum += count;
        }
        _ => {
            return -1;
        }
    }

    if (*writer).indent != 0 {
        count = (*(*writer).out).write_str("\n");
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    xml_list_pop_front((*writer).nodes);
    sum
}

// /**
//  * xmlTextWriterWriteFormatComment:
//  * @writer:  the xmlTextWriterPtr
//  * @format:  format string (see printf)
//  * @...:  extra parameters for the format
//  *
//  * Write an xml comment.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatComment(
//     writer: xmlTextWriterPtr,
//     format: *const c_char,
// ) -> i32 {
//     xmlTextWriterWriteVFormatComment(writer, format)
//     // original code is the following, but Rust cannot handle variable arguments...

//     // let rc: i32;
//     // va_list ap;

//     // va_start(ap, format);

//     // rc = xmlTextWriterWriteVFormatComment(writer, format, ap);

//     // va_end(ap);
//     // return rc;
// }

// /**
//  * xmlTextWriterWriteVFormatComment:
//  * @writer:  the xmlTextWriterPtr
//  * @format:  format string (see printf)
//  * @argptr:  pointer to the first member of the variable argument list.
//  *
//  * Write an xml comment.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatComment(
//     writer: xmlTextWriterPtr,
//     format: *const c_char,
// ) -> i32 {
//     if writer.is_null() {
//         xmlWriterErrMsg(
//             writer,
//             xmlParserErrors::XML_ERR_INTERNAL_ERROR,
//             c"xmlTextWriterWriteVFormatComment : invalid writer!\n".as_ptr() as _,
//         );
//         return -1;
//     }

//     xmlTextWriterWriteComment(writer, format as _)
//     // original code is the following, but Rust cannot handle variable arguments...

//     // let rc: i32;
//     // let buf: *mut xmlChar;

//     // if writer.is_null() {
//     //     xmlWriterErrMsg(
//     //         writer,
//     //         xmlParserErrors::XML_ERR_INTERNAL_ERROR,
//     //         c"xmlTextWriterWriteVFormatComment : invalid writer!\n".as_ptr() as _,
//     //     );
//     //     return -1;
//     // }

//     // buf = xmlTextWriterVSprintf(format, argptr);
//     // if buf.is_null() {
//     //     return -1;
//     // }

//     // rc = xmlTextWriterWriteComment(writer, buf);

//     // xmlFree(buf as _);
//     // return rc;
// }

/**
 * xmlTextWriterWriteComment:
 * @writer:  the xmlTextWriterPtr
 * @content:  comment string
 *
 * Write an xml comment.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_comment(
    writer: XmlTextWriterPtr,
    content: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    sum = 0;
    count = xml_text_writer_start_comment(writer);
    if count < 0 {
        return -1;
    }
    sum += count;
    count = xml_text_writer_write_string(writer, content);
    if count < 0 {
        return -1;
    }
    sum += count;
    count = xml_text_writer_end_comment(writer);
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/*
 * Elements
 */
/**
 * xmlTextWriterStartElement:
 * @writer:  the xmlTextWriterPtr
 * @name:  element name
 *
 * Start an xml element.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_start_element(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || name.is_null() || (*name == b'\0') {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            match (*p).state {
                XmlTextWriterState::XmlTextwriterPI | XmlTextWriterState::XmlTextwriterPIText => {
                    return -1;
                }
                XmlTextWriterState::XmlTextwriterNone => {}
                ty @ XmlTextWriterState::XmlTextwriterAttribute
                | ty @ XmlTextWriterState::XmlTextwriterName => {
                    if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                        count = xml_text_writer_end_attribute(writer);
                        if count < 0 {
                            return -1;
                        }
                        sum += count;
                    }

                    /* Output namespace declarations */
                    count = xml_text_writer_output_nsdecl(writer);
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                    count = (*(*writer).out).write_str(">");
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                    if (*writer).indent != 0 {
                        // count =
                        (*(*writer).out).write_str("\n");
                    }
                    (*p).state = XmlTextWriterState::XmlTextwriterText;
                }
                _ => {}
            }
        }
    }

    p = xml_malloc(size_of::<XmlTextWriterStackEntry>()) as *mut XmlTextWriterStackEntry;
    if p.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartElement : out of memory!\n".as_ptr() as _,
        );
        return -1;
    }

    (*p).name = xml_strdup(name);
    if (*p).name.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartElement : out of memory!\n".as_ptr() as _,
        );
        xml_free(p as _);
        return -1;
    }
    (*p).state = XmlTextWriterState::XmlTextwriterName;

    xml_list_push_front((*writer).nodes, p as _);

    if (*writer).indent != 0 {
        count = xml_text_writer_write_indent(writer);
        sum += count;
    }

    count = (*(*writer).out).write_str("<");
    if count < 0 {
        return -1;
    }
    sum += count;
    count = (*(*writer).out).write_str(CStr::from_ptr((*p).name as _).to_string_lossy().as_ref());
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterStartElementNS:
 * @writer:  the xmlTextWriterPtr
 * @prefix:  namespace prefix or NULL
 * @name:  element local name
 * @namespaceURI:  namespace URI or NULL
 *
 * Start an xml element with namespace support.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_start_element_ns(
    writer: XmlTextWriterPtr,
    prefix: *const XmlChar,
    name: *const XmlChar,
    namespace_uri: *const XmlChar,
) -> i32 {
    let mut sum: i32;
    let mut buf: *mut XmlChar;

    if writer.is_null() || name.is_null() || (*name == b'\0') {
        return -1;
    }

    buf = null_mut();
    if !prefix.is_null() {
        buf = xml_strdup(prefix);
        buf = xml_strcat(buf, c":".as_ptr() as _);
    }
    buf = xml_strcat(buf, name);

    sum = 0;
    let count: i32 = xml_text_writer_start_element(writer, buf);
    xml_free(buf as _);
    if count < 0 {
        return -1;
    }
    sum += count;

    if !namespace_uri.is_null() {
        let p: *mut XmlTextWriterNsStackEntry =
            xml_malloc(size_of::<XmlTextWriterNsStackEntry>()) as *mut XmlTextWriterNsStackEntry;
        if p.is_null() {
            xml_writer_err_msg(
                writer,
                XmlParserErrors::XmlErrNoMemory,
                c"xmlTextWriterStartElementNS : out of memory!\n".as_ptr() as _,
            );
            return -1;
        }

        buf = xml_strdup(c"xmlns".as_ptr() as _);
        if !prefix.is_null() {
            buf = xml_strcat(buf, c":".as_ptr() as _);
            buf = xml_strcat(buf, prefix);
        }

        (*p).prefix = buf;
        (*p).uri = xml_strdup(namespace_uri);
        if (*p).uri.is_null() {
            xml_writer_err_msg(
                writer,
                XmlParserErrors::XmlErrNoMemory,
                c"xmlTextWriterStartElementNS : out of memory!\n".as_ptr() as _,
            );
            xml_free(p as _);
            return -1;
        }
        (*p).elem = xml_list_front((*writer).nodes);

        xml_list_push_front((*writer).nsstack, p as _);
    }

    sum
}

/**
 * xmlTextWriterEndElement:
 * @writer:  the xmlTextWriterPtr
 *
 * End the current xml element.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_end_element(writer: XmlTextWriterPtr) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() {
        return -1;
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        xml_list_delete((*writer).nsstack);
        (*writer).nsstack = null_mut();
        return -1;
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        xml_list_delete((*writer).nsstack);
        (*writer).nsstack = null_mut();
        return -1;
    }

    sum = 0;
    match (*p).state {
        ty @ XmlTextWriterState::XmlTextwriterAttribute
        | ty @ XmlTextWriterState::XmlTextwriterName => {
            if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                count = xml_text_writer_end_attribute(writer);
                if count < 0 {
                    xml_list_delete((*writer).nsstack);
                    (*writer).nsstack = null_mut();
                    return -1;
                }
                sum += count;
            }

            /* Output namespace declarations */
            count = xml_text_writer_output_nsdecl(writer);
            if count < 0 {
                return -1;
            }
            sum += count;

            if (*writer).indent != 0
            /* next element needs indent */
            {
                (*writer).doindent = 1;
            }
            count = (*(*writer).out).write_str("/>");
            if count < 0 {
                return -1;
            }
            sum += count;
        }
        XmlTextWriterState::XmlTextwriterText => {
            if (*writer).indent != 0 && (*writer).doindent != 0 {
                count = xml_text_writer_write_indent(writer);
                sum += count;
                (*writer).doindent = 1;
            } else {
                (*writer).doindent = 1;
            }
            count = (*(*writer).out).write_str("</");
            if count < 0 {
                return -1;
            }
            sum += count;
            count = (*(*writer).out)
                .write_str(CStr::from_ptr((*p).name as _).to_string_lossy().as_ref());
            if count < 0 {
                return -1;
            }
            sum += count;
            count = (*(*writer).out).write_str(">");
            if count < 0 {
                return -1;
            }
            sum += count;
        }
        _ => {
            return -1;
        }
    }

    if (*writer).indent != 0 {
        count = (*(*writer).out).write_str("\n");
        sum += count;
    }

    xml_list_pop_front((*writer).nodes);
    sum
}

/**
 * xmlTextWriterFullEndElement:
 * @writer:  the xmlTextWriterPtr
 *
 * End the current xml element. Writes an end tag even if the element is empty
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_full_end_element(writer: XmlTextWriterPtr) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() {
        return -1;
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return -1;
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return -1;
    }

    sum = 0;
    match (*p).state {
        ty @ XmlTextWriterState::XmlTextwriterAttribute
        | ty @ XmlTextWriterState::XmlTextwriterName
        | ty @ XmlTextWriterState::XmlTextwriterText => {
            if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                count = xml_text_writer_end_attribute(writer);
                if count < 0 {
                    return -1;
                }
                sum += count;
            }

            if matches!(
                ty,
                XmlTextWriterState::XmlTextwriterAttribute | XmlTextWriterState::XmlTextwriterName
            ) {
                /* Output namespace declarations */
                count = xml_text_writer_output_nsdecl(writer);
                if count < 0 {
                    return -1;
                }
                sum += count;

                count = (*(*writer).out).write_str(">");
                if count < 0 {
                    return -1;
                }
                sum += count;
                if (*writer).indent != 0 {
                    (*writer).doindent = 0;
                }
            }

            if (*writer).indent != 0 && (*writer).doindent != 0 {
                count = xml_text_writer_write_indent(writer);
                sum += count;
                (*writer).doindent = 1;
            } else {
                (*writer).doindent = 1;
            }
            count = (*(*writer).out).write_str("</");
            if count < 0 {
                return -1;
            }
            sum += count;
            count = (*(*writer).out)
                .write_str(CStr::from_ptr((*p).name as _).to_string_lossy().as_ref());
            if count < 0 {
                return -1;
            }
            sum += count;
            count = (*(*writer).out).write_str(">");
            if count < 0 {
                return -1;
            }
            sum += count;
        }
        _ => {
            return -1;
        }
    }

    if (*writer).indent != 0 {
        count = (*(*writer).out).write_str("\n");
        sum += count;
    }

    xml_list_pop_front((*writer).nodes);
    sum
}

/*
 * Elements conveniency functions
 */
// /**
//  * xmlTextWriterWriteFormatElement:
//  * @writer:  the xmlTextWriterPtr
//  * @name:  element name
//  * @format:  format string (see printf)
//  * @...:  extra parameters for the format
//  *
//  * Write a formatted xml element.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatElement(
//     writer: xmlTextWriterPtr,
//     name: *const xmlChar,
//     format: *const c_char,
// ) -> i32 {
//     xmlTextWriterWriteVFormatElement(writer, name, format)
//     // original code is the following, but Rust cannot handle variable arguments...

//     // let rc: i32;
//     // va_list ap;

//     // va_start(ap, format);

//     // rc = xmlTextWriterWriteVFormatElement(writer, name, format, ap);

//     // va_end(ap);
//     // return rc;
// }

// /**
//  * xmlTextWriterWriteVFormatElement:
//  * @writer:  the xmlTextWriterPtr
//  * @name:  element name
//  * @format:  format string (see printf)
//  * @argptr:  pointer to the first member of the variable argument list.
//  *
//  * Write a formatted xml element.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatElement(
//     writer: xmlTextWriterPtr,
//     name: *const xmlChar,
//     format: *const c_char,
// ) -> i32 {
//     if writer.is_null() {
//         return -1;
//     }

//     xmlTextWriterWriteElement(writer, name, format as _)
//     // original code is the following, but Rust cannot handle variable arguments...

//     // let rc: i32;
//     // let buf: *mut xmlChar;

//     // if writer.is_null() {
//     //     return -1;
//     // }

//     // buf = xmlTextWriterVSprintf(format, argptr);
//     // if buf.is_null() {
//     //     return -1;
//     // }

//     // rc = xmlTextWriterWriteElement(writer, name, buf);

//     // xmlFree(buf as _);
//     // return rc;
// }

/**
 * xmlTextWriterWriteElement:
 * @writer:  the xmlTextWriterPtr
 * @name:  element name
 * @content:  element content
 *
 * Write an xml element.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_element(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    sum = 0;
    count = xml_text_writer_start_element(writer, name);
    if count == -1 {
        return -1;
    }
    sum += count;
    if !content.is_null() {
        count = xml_text_writer_write_string(writer, content);
        if count == -1 {
            return -1;
        }
        sum += count;
    }
    count = xml_text_writer_end_element(writer);
    if count == -1 {
        return -1;
    }
    sum += count;

    sum
}

// /**
//  * xmlTextWriterWriteFormatElementNS:
//  * @writer:  the xmlTextWriterPtr
//  * @prefix:  namespace prefix
//  * @name:  element local name
//  * @namespaceURI:  namespace URI
//  * @format:  format string (see printf)
//  * @...:  extra parameters for the format
//  *
//  * Write a formatted xml element with namespace support.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatElementNS(
//     writer: xmlTextWriterPtr,
//     prefix: *const xmlChar,
//     name: *const xmlChar,
//     namespaceURI: *const xmlChar,
//     format: *const c_char,
// ) -> i32 {
//     xmlTextWriterWriteVFormatElementNS(writer, prefix, name, namespaceURI, format)
//     // original code is the following, but Rust cannot handle variable arguments...

//     // let rc: i32;
//     // va_list ap;

//     // va_start(ap, format);

//     // rc = xmlTextWriterWriteVFormatElementNS(writer, prefix, name,
//     //                                         namespaceURI, format, ap);

//     // va_end(ap);
//     // return rc;
// }

// /**
//  * xmlTextWriterWriteVFormatElementNS:
//  * @writer:  the xmlTextWriterPtr
//  * @prefix:  namespace prefix
//  * @name:  element local name
//  * @namespaceURI:  namespace URI
//  * @format:  format string (see printf)
//  * @argptr:  pointer to the first member of the variable argument list.
//  *
//  * Write a formatted xml element with namespace support.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatElementNS(
//     writer: xmlTextWriterPtr,
//     prefix: *const xmlChar,
//     name: *const xmlChar,
//     namespaceURI: *const xmlChar,
//     format: *const c_char,
// ) -> i32 {
//     if writer.is_null() {
//         return -1;
//     }

//     xmlTextWriterWriteElementNS(writer, prefix, name, namespaceURI, format as _)
//     // original code is the following, but Rust cannot handle variable arguments...

//     // let rc: i32;
//     // let buf: *mut xmlChar;

//     // if writer.is_null() {
//     //     return -1;
//     // }

//     // buf = xmlTextWriterVSprintf(format, argptr);
//     // if buf.is_null() {
//     //     return -1;
//     // }

//     // rc = xmlTextWriterWriteElementNS(writer, prefix, name, namespaceURI, buf);

//     // xmlFree(buf as _);
//     // return rc;
// }

/**
 * xmlTextWriterWriteElementNS:
 * @writer:  the xmlTextWriterPtr
 * @prefix:  namespace prefix
 * @name:  element local name
 * @namespaceURI:  namespace URI
 * @content:  element content
 *
 * Write an xml element with namespace support.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_element_ns(
    writer: XmlTextWriterPtr,
    prefix: *const XmlChar,
    name: *const XmlChar,
    namespace_uri: *const XmlChar,
    content: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() || name.is_null() || *name == b'\0' {
        return -1;
    }

    sum = 0;
    count = xml_text_writer_start_element_ns(writer, prefix, name, namespace_uri);
    if count < 0 {
        return -1;
    }
    sum += count;
    count = xml_text_writer_write_string(writer, content);
    if count == -1 {
        return -1;
    }
    sum += count;
    count = xml_text_writer_end_element(writer);
    if count == -1 {
        return -1;
    }
    sum += count;

    sum
}

/*
 * Text
 */
// /**
//  * xmlTextWriterWriteFormatRaw:
//  * @writer:  the xmlTextWriterPtr
//  * @format:  format string (see printf)
//  * @...:  extra parameters for the format
//  *
//  * Write a formatted raw xml text.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatRaw(
//     writer: xmlTextWriterPtr,
//     format: *const c_char,
// ) -> i32 {
//     xmlTextWriterWriteVFormatRaw(writer, format)
//     // original code is the following, but Rust cannot handle variable arguments...
//     // let rc: i32;
//     // va_list ap;

//     // va_start(ap, format);

//     // rc = xmlTextWriterWriteVFormatRaw(writer, format, ap);

//     // va_end(ap);
//     // return rc;
// }

// /**
//  * xmlTextWriterWriteVFormatRaw:
//  * @writer:  the xmlTextWriterPtr
//  * @format:  format string (see printf)
//  * @argptr:  pointer to the first member of the variable argument list.
//  *
//  * Write a formatted raw xml text.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatRaw(
//     writer: xmlTextWriterPtr,
//     format: *const c_char,
// ) -> i32 {
//     if writer.is_null() {
//         return -1;
//     }

//     xmlTextWriterWriteRaw(writer, format as _)
//     // original code is the following, but Rust cannot handle variable arguments...
//     // let rc: i32;
//     // let buf: *mut xmlChar;

//     // if writer.is_null() {
//     //     return -1;
//     // }

//     // buf = xmlTextWriterVSprintf(format, argptr);
//     // if buf.is_null() {
//     //     return -1;
//     // }

//     // rc = xmlTextWriterWriteRaw(writer, buf);

//     // xmlFree(buf as _);
//     // return rc;
// }

/**
 * xmlTextWriterHandleStateDependencies:
 * @writer:  the xmlTextWriterPtr
 * @p:  the xmlTextWriterStackEntry
 *
 * Write state dependent strings.
 *
 * Returns -1 on error or the number of characters written.
 */
unsafe extern "C" fn xml_text_writer_handle_state_dependencies(
    writer: XmlTextWriterPtr,
    p: *mut XmlTextWriterStackEntry,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    let mut extra: [c_char; 3] = [0; 3];

    if writer.is_null() {
        return -1;
    }

    if p.is_null() {
        return 0;
    }

    sum = 0;
    extra[0] = b'\0' as _;
    extra[1] = b'\0' as _;
    extra[2] = b'\0' as _;
    if !p.is_null() {
        sum = 0;
        match (*p).state {
            XmlTextWriterState::XmlTextwriterName => {
                /* Output namespace declarations */
                count = xml_text_writer_output_nsdecl(writer);
                if count < 0 {
                    return -1;
                }
                sum += count;
                extra[0] = b'>' as _;
                (*p).state = XmlTextWriterState::XmlTextwriterText;
            }
            XmlTextWriterState::XmlTextwriterPI => {
                extra[0] = b' ' as _;
                (*p).state = XmlTextWriterState::XmlTextwriterPIText;
            }
            XmlTextWriterState::XmlTextwriterDtd => {
                extra[0] = b' ' as _;
                extra[1] = b'[' as _;
                (*p).state = XmlTextWriterState::XmlTextwriterDtdText;
            }
            XmlTextWriterState::XmlTextwriterDtdElem => {
                extra[0] = b' ' as _;
                (*p).state = XmlTextWriterState::XmlTextwriterDtdElemText;
            }
            XmlTextWriterState::XmlTextwriterDtdAttl => {
                extra[0] = b' ' as _;
                (*p).state = XmlTextWriterState::XmlTextwriterDtdAttlText;
            }
            XmlTextWriterState::XmlTextwriterDtdEnty | XmlTextWriterState::XmlTextwriterDtdPEnt => {
                extra[0] = b' ' as _;
                extra[1] = (*writer).qchar;
                (*p).state = XmlTextWriterState::XmlTextwriterDtdEntyText;
            }
            _ => {}
        }
    }

    if extra[0] != b'\0' as i8 {
        count =
            (*(*writer).out).write_str(CStr::from_ptr(extra.as_ptr()).to_string_lossy().as_ref());
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    sum
}

/**
 * xmlTextWriterWriteRawLen:
 * @writer:  the xmlTextWriterPtr
 * @content:  text string
 * @len:  length of the text string
 *
 * Write an xml text.
 * TODO: what about entities and special chars??
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_raw_len(
    writer: XmlTextWriterPtr,
    content: *const XmlChar,
    len: i32,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    let p: *mut XmlTextWriterStackEntry;

    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterWriteRawLen : invalid writer!\n".as_ptr() as _,
        );
        return -1;
    }

    if content.is_null() || len < 0 {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterWriteRawLen : invalid content!\n".as_ptr() as _,
        );
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        count = xml_text_writer_handle_state_dependencies(writer, p);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    if (*writer).indent != 0 {
        (*writer).doindent = 0;
    }

    if !content.is_null() {
        count = (*(*writer).out).write_bytes(from_raw_parts(content as _, len as usize));
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    sum
}

/**
 * xmlTextWriterWriteRaw:
 * @writer:  the xmlTextWriterPtr
 * @content:  text string
 *
 * Write a raw xml text.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_raw(
    writer: XmlTextWriterPtr,
    content: *const XmlChar,
) -> i32 {
    xml_text_writer_write_raw_len(writer, content, xml_strlen(content))
}

// /**
//  * xmlTextWriterWriteFormatString:
//  * @writer:  the xmlTextWriterPtr
//  * @format:  format string (see printf)
//  * @...:  extra parameters for the format
//  *
//  * Write a formatted xml text.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatString(
//     writer: xmlTextWriterPtr,
//     format: *const c_char,
// ) -> i32 {
//     if writer.is_null() || format.is_null() {
//         return -1;
//     }
//     xmlTextWriterWriteVFormatString(writer, format)
//     // original code is the following, but Rust cannot handle variable arguments...
//     // let rc: i32;
//     // va_list ap;

//     // if (writer.is_null() || (format.is_null())) {
//     //     return -1;
//     // }

//     // va_start(ap, format);

//     // rc = xmlTextWriterWriteVFormatString(writer, format, ap);

//     // va_end(ap);
//     // return rc;
// }

// /**
//  * xmlTextWriterWriteVFormatString:
//  * @writer:  the xmlTextWriterPtr
//  * @format:  format string (see printf)
//  * @argptr:  pointer to the first member of the variable argument list.
//  *
//  * Write a formatted xml text.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatString(
//     writer: xmlTextWriterPtr,
//     format: *const c_char,
// ) -> i32 {
//     if writer.is_null() || format.is_null() {
//         return -1;
//     }
//     xmlTextWriterWriteString(writer, format as _)
//     // original code is the following, but Rust cannot handle variable arguments...

//     // let rc: i32;
//     // let buf: *mut xmlChar;

//     // if (writer.is_null() || (format.is_null())) {
//     //     return -1;
//     // }

//     // buf = xmlTextWriterVSprintf(format, argptr);
//     // if buf.is_null() {
//     //     return -1;
//     // }

//     // rc = xmlTextWriterWriteString(writer, buf);

//     // xmlFree(buf as _);
//     // return rc;
// }

/**
 * xmlTextWriterWriteString:
 * @writer:  the xmlTextWriterPtr
 * @content:  text string
 *
 * Write an xml text.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_string(
    writer: XmlTextWriterPtr,
    content: *const XmlChar,
) -> i32 {
    let count: i32;
    let mut sum: i32;
    let p: *mut XmlTextWriterStackEntry;
    let mut buf: *mut XmlChar;

    if writer.is_null() || content.is_null() {
        return -1;
    }

    sum = 0;
    buf = content as _;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            match (*p).state {
                XmlTextWriterState::XmlTextwriterName | XmlTextWriterState::XmlTextwriterText => {
                    buf = xml_encode_special_chars(null_mut(), content);
                }
                XmlTextWriterState::XmlTextwriterAttribute => {
                    buf = null_mut();
                    xml_buf_attr_serialize_txt_content(
                        (*(*writer).out)
                            .buffer
                            .map_or(null_mut(), |buf| buf.as_ptr()),
                        (*writer).doc,
                        null_mut(),
                        content,
                    );
                }
                _ => {}
            }
        }
    }

    if !buf.is_null() {
        count = xml_text_writer_write_raw(writer, buf);

        if buf != content as _
        /* buf was allocated by us, so free it */
        {
            xml_free(buf as _);
        }

        if count < 0 {
            return -1;
        }
        sum += count;
    }

    sum
}

const B64LINELEN: usize = 72;
const B64CRLF: &str = "\r\n";

/**
 * xmlOutputBufferWriteBase64:
 * @out: the xmlOutputBufferPtr
 * @data:   binary data
 * @len:  the number of bytes to encode
 *
 * Write base64 encoded data to an xmlOutputBuffer.
 * Adapted from John Walker's base64.c (http://www.fourmilab.ch/).
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
unsafe extern "C" fn xml_output_buffer_write_base64(
    out: XmlOutputBufferPtr,
    len: i32,
    data: *const u8,
) -> i32 {
    const DTABLE: [u8; 64] = [
        b'A', b'B', b'C', b'D', b'E', b'F', b'G', b'H', b'I', b'J', b'K', b'L', b'M', b'N', b'O',
        b'P', b'Q', b'R', b'S', b'T', b'U', b'V', b'W', b'X', b'Y', b'Z', b'a', b'b', b'c', b'd',
        b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm', b'n', b'o', b'p', b'q', b'r', b's',
        b't', b'u', b'v', b'w', b'x', b'y', b'z', b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7',
        b'8', b'9', b'+', b'/',
    ];

    let mut i: i32;
    let mut linelen: i32;
    let mut count: i32;
    let mut sum: i32;

    if out.is_null() || len < 0 || data.is_null() {
        return -1;
    }

    linelen = 0;
    sum = 0;

    i = 0;
    loop {
        let mut igroup: [u8; 3] = [0; 3];
        let mut ogroup: [u8; 4] = [0; 4];
        let mut c: i32;
        let mut n: i32 = 3;

        igroup[0] = 0;
        igroup[1] = 0;
        igroup[2] = 0;
        for j in 0..3 {
            c = *data.add(i as usize) as _;
            igroup[j] = c as u8;
            i += 1;
            if i >= len {
                n = j as _;
                break;
            }
        }

        if n > 0 {
            ogroup[0] = DTABLE[igroup[0] as usize >> 2];
            ogroup[1] = DTABLE[((igroup[0] as usize & 3) << 4) | (igroup[1] as usize >> 4)];
            ogroup[2] = DTABLE[((igroup[1] as usize & 0xF) << 2) | (igroup[2] as usize >> 6)];
            ogroup[3] = DTABLE[igroup[2] as usize & 0x3F];

            if n < 3 {
                ogroup[3] = b'=';
                if n < 2 {
                    ogroup[2] = b'=';
                }
            }

            if linelen >= B64LINELEN as i32 {
                count = (*out).write_bytes(B64CRLF.as_bytes());
                if count == -1 {
                    return -1;
                }
                sum += count;
                linelen = 0;
            }
            count = (*out).write_bytes(&ogroup);
            if count == -1 {
                return -1;
            }
            sum += count;

            linelen += 4;
        }

        if i >= len {
            break;
        }
    }

    sum
}

/**
 * xmlTextWriterWriteBase64:
 * @writer: the xmlTextWriterPtr
 * @data:   binary data
 * @start:  the position within the data of the first byte to encode
 * @len:  the number of bytes to encode
 *
 * Write an base64 encoded xml text.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_base64(
    writer: XmlTextWriterPtr,
    data: *const c_char,
    start: i32,
    len: i32,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    let p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || data.is_null() || start < 0 || len < 0 {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            count = xml_text_writer_handle_state_dependencies(writer, p);
            if count < 0 {
                return -1;
            }
            sum += count;
        }
    }

    if (*writer).indent != 0 {
        (*writer).doindent = 0;
    }

    count = xml_output_buffer_write_base64((*writer).out, len, data.add(start as usize) as _);
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlOutputBufferWriteBinHex:
 * @out: the xmlOutputBufferPtr
 * @data:   binary data
 * @len:  the number of bytes to encode
 *
 * Write hqx encoded data to an xmlOutputBuffer.
 * ::todo
 *
 * Returns the bytes written (may be 0 because of buffering)
 * or -1 in case of error
 */
unsafe extern "C" fn xml_output_buffer_write_bin_hex(
    out: XmlOutputBufferPtr,
    len: i32,
    data: *const u8,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    const HEX: [u8; 16] = [
        b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D', b'E',
        b'F',
    ];
    if out.is_null() || data.is_null() || len < 0 {
        return -1;
    }

    sum = 0;
    for i in 0..len as usize {
        let hi = *data.add(i) as usize >> 4;
        count = (*out).write_bytes(&HEX[hi..hi + 1]);
        if count == -1 {
            return -1;
        }
        sum += count;
        let lo = *data.add(i) as usize & 0xF;
        count = (*out).write_bytes(&HEX[lo..lo + 1]);
        if count == -1 {
            return -1;
        }
        sum += count;
    }

    sum
}

/**
 * xmlTextWriterWriteBinHex:
 * @writer: the xmlTextWriterPtr
 * @data:   binary data
 * @start:  the position within the data of the first byte to encode
 * @len:  the number of bytes to encode
 *
 * Write a BinHex encoded xml text.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_bin_hex(
    writer: XmlTextWriterPtr,
    data: *const c_char,
    start: i32,
    len: i32,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    let p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || data.is_null() || start < 0 || len < 0 {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            count = xml_text_writer_handle_state_dependencies(writer, p);
            if count < 0 {
                return -1;
            }
            sum += count;
        }
    }

    if (*writer).indent != 0 {
        (*writer).doindent = 0;
    }

    count = xml_output_buffer_write_bin_hex((*writer).out, len, data.add(start as usize) as _);
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/*
 * Attributes
 */
/**
 * xmlTextWriterStartAttribute:
 * @writer:  the xmlTextWriterPtr
 * @name:  element name
 *
 * Start an xml attribute.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_start_attribute(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() || name.is_null() || *name == b'\0' {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return -1;
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return -1;
    }

    match (*p).state {
        ty @ XmlTextWriterState::XmlTextwriterAttribute
        | ty @ XmlTextWriterState::XmlTextwriterName => {
            if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                count = xml_text_writer_end_attribute(writer);
                if count < 0 {
                    return -1;
                }
                sum += count;
            }

            count = (*(*writer).out).write_str(" ");
            if count < 0 {
                return -1;
            }
            sum += count;
            count =
                (*(*writer).out).write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref());
            if count < 0 {
                return -1;
            }
            sum += count;
            count = (*(*writer).out).write_str("=");
            if count < 0 {
                return -1;
            }
            sum += count;
            count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
            if count < 0 {
                return -1;
            }
            sum += count;
            (*p).state = XmlTextWriterState::XmlTextwriterAttribute;
        }
        _ => {
            return -1;
        }
    }

    sum
}

/**
 * xmlTextWriterStartAttributeNS:
 * @writer:  the xmlTextWriterPtr
 * @prefix:  namespace prefix or NULL
 * @name:  element local name
 * @namespaceURI:  namespace URI or NULL
 *
 * Start an xml attribute with namespace support.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_start_attribute_ns(
    writer: XmlTextWriterPtr,
    prefix: *const XmlChar,
    name: *const XmlChar,
    namespace_uri: *const XmlChar,
) -> i32 {
    let mut sum: i32;
    let mut buf: *mut XmlChar;
    let p: *mut XmlTextWriterNsStackEntry;

    if writer.is_null() || name.is_null() || *name == b'\0' {
        return -1;
    }

    /* Handle namespace first in case of error */
    if !namespace_uri.is_null() {
        let mut nsentry: XmlTextWriterNsStackEntry = unsafe { zeroed() };

        buf = xml_strdup(c"xmlns".as_ptr() as _);
        if !prefix.is_null() {
            buf = xml_strcat(buf, c":".as_ptr() as _);
            buf = xml_strcat(buf, prefix);
        }

        nsentry.prefix = buf;
        nsentry.uri = namespace_uri as _;
        nsentry.elem = xml_list_front((*writer).nodes);

        let curns: *mut XmlTextWriterNsStackEntry =
            xml_list_search((*writer).nsstack, addr_of_mut!(nsentry) as _)
                as *mut XmlTextWriterNsStackEntry;
        if !curns.is_null() {
            xml_free(buf as _);
            if xml_strcmp((*curns).uri, namespace_uri) == 0 {
                /* Namespace already defined on element skip */
                buf = null_mut();
            } else {
                /* Prefix mismatch so error out */
                return -1;
            }
        }

        /* Do not add namespace decl to list - it is already there */
        if !buf.is_null() {
            p = xml_malloc(size_of::<XmlTextWriterNsStackEntry>())
                as *mut XmlTextWriterNsStackEntry;
            if p.is_null() {
                xml_writer_err_msg(
                    writer,
                    XmlParserErrors::XmlErrNoMemory,
                    c"xmlTextWriterStartAttributeNS : out of memory!\n".as_ptr() as _,
                );
                return -1;
            }

            (*p).prefix = buf;
            (*p).uri = xml_strdup(namespace_uri);
            if (*p).uri.is_null() {
                xml_writer_err_msg(
                    writer,
                    XmlParserErrors::XmlErrNoMemory,
                    c"xmlTextWriterStartAttributeNS : out of memory!\n".as_ptr() as _,
                );
                xml_free(p as _);
                return -1;
            }
            (*p).elem = xml_list_front((*writer).nodes);

            xml_list_push_front((*writer).nsstack, p as _);
        }
    }

    buf = null_mut();
    if !prefix.is_null() {
        buf = xml_strdup(prefix);
        buf = xml_strcat(buf, c":".as_ptr() as _);
    }
    buf = xml_strcat(buf, name);

    sum = 0;
    let count: i32 = xml_text_writer_start_attribute(writer, buf);
    xml_free(buf as _);
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterEndAttribute:
 * @writer:  the xmlTextWriterPtr
 *
 * End the current xml element.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_end_attribute(writer: XmlTextWriterPtr) -> i32 {
    let count: i32;
    let mut sum: i32;

    if writer.is_null() {
        return -1;
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return -1;
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return -1;
    }

    sum = 0;
    match (*p).state {
        XmlTextWriterState::XmlTextwriterAttribute => {
            (*p).state = XmlTextWriterState::XmlTextwriterName;

            count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
            if count < 0 {
                return -1;
            }
            sum += count;
        }
        _ => {
            return -1;
        }
    }

    sum
}

/*
 * Attributes conveniency functions
 */
// /**
//  * xmlTextWriterWriteFormatAttribute:
//  * @writer:  the xmlTextWriterPtr
//  * @name:  attribute name
//  * @format:  format string (see printf)
//  * @...:  extra parameters for the format
//  *
//  * Write a formatted xml attribute.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatAttribute(
//     writer: xmlTextWriterPtr,
//     name: *const xmlChar,
//     format: *const c_char,
// ) -> i32 {
//     xmlTextWriterWriteVFormatAttribute(writer, name, format)

//     // original code is the following, but Rust cannot handle variable arguments...
//     // let rc: i32;
//     // va_list ap;

//     // va_start(ap, format);

//     // rc = xmlTextWriterWriteVFormatAttribute(writer, name, format, ap);

//     // va_end(ap);
//     // return rc;
// }

// /**
//  * xmlTextWriterWriteVFormatAttribute:
//  * @writer:  the xmlTextWriterPtr
//  * @name:  attribute name
//  * @format:  format string (see printf)
//  * @argptr:  pointer to the first member of the variable argument list.
//  *
//  * Write a formatted xml attribute.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatAttribute(
//     writer: xmlTextWriterPtr,
//     name: *const xmlChar,
//     format: *const c_char,
// ) -> i32 {
//     if writer.is_null() {
//         return -1;
//     }

//     xmlTextWriterWriteAttribute(writer, name, format as _)
//     // original code is the following, but Rust cannot handle variable arguments...
//     // let rc: i32;
//     // let buf: *mut xmlChar;

//     // if writer.is_null() {
//     //     return -1;
//     // }

//     // buf = xmlTextWriterVSprintf(format, argptr);
//     // if buf.is_null() {
//     //     return -1;
//     // }

//     // rc = xmlTextWriterWriteAttribute(writer, name, buf);

//     // xmlFree(buf as _);
//     // return rc;
// }

/**
 * xmlTextWriterWriteAttribute:
 * @writer:  the xmlTextWriterPtr
 * @name:  attribute name
 * @content:  attribute content
 *
 * Write an xml attribute.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_attribute(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    sum = 0;
    count = xml_text_writer_start_attribute(writer, name);
    if count < 0 {
        return -1;
    }
    sum += count;
    count = xml_text_writer_write_string(writer, content);
    if count < 0 {
        return -1;
    }
    sum += count;
    count = xml_text_writer_end_attribute(writer);
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

// /**
//  * xmlTextWriterWriteFormatAttributeNS:
//  * @writer:  the xmlTextWriterPtr
//  * @prefix:  namespace prefix
//  * @name:  attribute local name
//  * @namespaceURI:  namespace URI
//  * @format:  format string (see printf)
//  * @...:  extra parameters for the format
//  *
//  * Write a formatted xml attribute.with namespace support
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatAttributeNS(
//     writer: xmlTextWriterPtr,
//     prefix: *const xmlChar,
//     name: *const xmlChar,
//     namespaceURI: *const xmlChar,
//     format: *const c_char,
// ) -> i32 {
//     xmlTextWriterWriteVFormatAttributeNS(writer, prefix, name, namespaceURI, format)
//     // original code is the following, but Rust cannot handle variable arguments...
//     // let rc: i32;
//     // va_list ap;

//     // va_start(ap, format);

//     // rc = xmlTextWriterWriteVFormatAttributeNS(writer, prefix, name,
//     //                                           namespaceURI, format, ap);

//     // va_end(ap);
//     // return rc;
// }

// /**
//  * xmlTextWriterWriteVFormatAttributeNS:
//  * @writer:  the xmlTextWriterPtr
//  * @prefix:  namespace prefix
//  * @name:  attribute local name
//  * @namespaceURI:  namespace URI
//  * @format:  format string (see printf)
//  * @argptr:  pointer to the first member of the variable argument list.
//  *
//  * Write a formatted xml attribute.with namespace support
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatAttributeNS(
//     writer: xmlTextWriterPtr,
//     prefix: *const xmlChar,
//     name: *const xmlChar,
//     namespaceURI: *const xmlChar,
//     format: *const c_char,
// ) -> i32 {
//     if writer.is_null() {
//         return -1;
//     }
//     xmlTextWriterWriteAttributeNS(writer, prefix, name, namespaceURI, format as _)
//     // original code is the following, but Rust cannot handle variable arguments...

//     // let rc: i32;
//     // let buf: *mut xmlChar;

//     // if writer.is_null() {
//     //     return -1;
//     // }

//     // buf = xmlTextWriterVSprintf(format, argptr);
//     // if buf.is_null() {
//     //     return -1;
//     // }

//     // rc = xmlTextWriterWriteAttributeNS(writer, prefix, name, namespaceURI, buf);

//     // xmlFree(buf as _);
//     // return rc;
// }

/**
 * xmlTextWriterWriteAttributeNS:
 * @writer:  the xmlTextWriterPtr
 * @prefix:  namespace prefix
 * @name:  attribute local name
 * @namespaceURI:  namespace URI
 * @content:  attribute content
 *
 * Write an xml attribute.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_attribute_ns(
    writer: XmlTextWriterPtr,
    prefix: *const XmlChar,
    name: *const XmlChar,
    namespace_uri: *const XmlChar,
    content: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() || name.is_null() || *name == b'\0' {
        return -1;
    }

    sum = 0;
    count = xml_text_writer_start_attribute_ns(writer, prefix, name, namespace_uri);
    if count < 0 {
        return -1;
    }
    sum += count;
    count = xml_text_writer_write_string(writer, content);
    if count < 0 {
        return -1;
    }
    sum += count;
    count = xml_text_writer_end_attribute(writer);
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/*
 * PI's
 */
/**
 * xmlTextWriterStartPI:
 * @writer:  the xmlTextWriterPtr
 * @target:  PI target
 *
 * Start an xml PI.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_start_pi(
    writer: XmlTextWriterPtr,
    target: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || target.is_null() || *target == b'\0' {
        return -1;
    }

    if xml_strcasecmp(target, c"xml".as_ptr() as _) == 0 {
        xml_writer_err_msg(writer, XmlParserErrors::XmlErrInternalError,
                        c"xmlTextWriterStartPI : target name [Xx][Mm][Ll] is reserved for xml standardization!\n".as_ptr() as _);
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            match (*p).state {
                ty @ XmlTextWriterState::XmlTextwriterAttribute
                | ty @ XmlTextWriterState::XmlTextwriterName => {
                    if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                        count = xml_text_writer_end_attribute(writer);
                        if count < 0 {
                            return -1;
                        }
                        sum += count;
                    }
                    /* Output namespace declarations */
                    count = xml_text_writer_output_nsdecl(writer);
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                    count = (*(*writer).out).write_str(">");
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                    (*p).state = XmlTextWriterState::XmlTextwriterText;
                }
                XmlTextWriterState::XmlTextwriterNone
                | XmlTextWriterState::XmlTextwriterText
                | XmlTextWriterState::XmlTextwriterDtd => {}
                XmlTextWriterState::XmlTextwriterPI | XmlTextWriterState::XmlTextwriterPIText => {
                    xml_writer_err_msg(
                        writer,
                        XmlParserErrors::XmlErrInternalError,
                        c"xmlTextWriterStartPI : nested PI!\n".as_ptr() as _,
                    );
                    return -1;
                }
                _ => {
                    return -1;
                }
            }
        }
    }

    p = xml_malloc(size_of::<XmlTextWriterStackEntry>()) as *mut XmlTextWriterStackEntry;
    if p.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartPI : out of memory!\n".as_ptr() as _,
        );
        return -1;
    }

    (*p).name = xml_strdup(target);
    if (*p).name.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartPI : out of memory!\n".as_ptr() as _,
        );
        xml_free(p as _);
        return -1;
    }
    (*p).state = XmlTextWriterState::XmlTextwriterPI;

    xml_list_push_front((*writer).nodes, p as _);

    count = (*(*writer).out).write_str("<?");
    if count < 0 {
        return -1;
    }
    sum += count;
    count = (*(*writer).out).write_str(CStr::from_ptr((*p).name as _).to_string_lossy().as_ref());
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterEndPI:
 * @writer:  the xmlTextWriterPtr
 *
 * End the current xml PI.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_end_pi(writer: XmlTextWriterPtr) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() {
        return -1;
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return 0;
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return 0;
    }

    sum = 0;
    match (*p).state {
        XmlTextWriterState::XmlTextwriterPI | XmlTextWriterState::XmlTextwriterPIText => {
            count = (*(*writer).out).write_str("?>");
            if count < 0 {
                return -1;
            }
            sum += count;
        }
        _ => {
            return -1;
        }
    }

    if (*writer).indent != 0 {
        count = (*(*writer).out).write_str("\n");
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    xml_list_pop_front((*writer).nodes);
    sum
}

/*
 * PI conveniency functions
 */
// /**
//  * xmlTextWriterWriteFormatPI:
//  * @writer:  the xmlTextWriterPtr
//  * @target:  PI target
//  * @format:  format string (see printf)
//  * @...:  extra parameters for the format
//  *
//  * Write a formatted PI.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatPI(
//     writer: xmlTextWriterPtr,
//     target: *const xmlChar,
//     format: *const c_char,
// ) -> i32 {
//     xmlTextWriterWriteVFormatPI(writer, target, format)
//     // original code is the following, but Rust cannot handle variable arguments...

//     // let rc: i32;
//     // va_list ap;

//     // va_start(ap, format);

//     // rc = xmlTextWriterWriteVFormatPI(writer, target, format, ap);

//     // va_end(ap);
//     // return rc;
// }

// /**
//  * xmlTextWriterWriteVFormatPI:
//  * @writer:  the xmlTextWriterPtr
//  * @target:  PI target
//  * @format:  format string (see printf)
//  * @argptr:  pointer to the first member of the variable argument list.
//  *
//  * Write a formatted xml PI.
//  *
//  * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
//  */
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatPI(
//     writer: xmlTextWriterPtr,
//     target: *const xmlChar,
//     format: *const c_char,
// ) -> i32 {
//     if writer.is_null() {
//         return -1;
//     }
//     xmlTextWriterWritePI(writer, target, format as _)
//     // let rc: i32;
//     // let buf: *mut xmlChar;

//     // if writer.is_null() {
//     //     return -1;
//     // }

//     // buf = xmlTextWriterVSprintf(format, argptr);
//     // if buf.is_null() {
//     //     return -1;
//     // }

//     // rc = xmlTextWriterWritePI(writer, target, buf);

//     // xmlFree(buf as _);
//     // return rc;
// }

/**
 * xmlTextWriterWritePI:
 * @writer:  the xmlTextWriterPtr
 * @target:  PI target
 * @content:  PI content
 *
 * Write an xml PI.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_pi(
    writer: XmlTextWriterPtr,
    target: *const XmlChar,
    content: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    sum = 0;
    count = xml_text_writer_start_pi(writer, target);
    if count == -1 {
        return -1;
    }
    sum += count;
    if !content.is_null() {
        count = xml_text_writer_write_string(writer, content);
        if count == -1 {
            return -1;
        }
        sum += count;
    }
    count = xml_text_writer_end_pi(writer);
    if count == -1 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterWriteProcessingInstruction:
 *
 * This macro maps to xmlTextWriterWritePI
 */
const XML_TEXT_WRITER_WRITE_PROCESSING_INSTRUCTION: unsafe extern "C" fn(
    writer: XmlTextWriterPtr,
    target: *const XmlChar,
    content: *const XmlChar,
) -> i32 = xml_text_writer_write_pi;

/*
 * CDATA
 */
/**
 * xmlTextWriterStartCDATA:
 * @writer:  the xmlTextWriterPtr
 *
 * Start an xml CDATA section.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_start_cdata(writer: XmlTextWriterPtr) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            match (*p).state {
                XmlTextWriterState::XmlTextwriterNone
                | XmlTextWriterState::XmlTextwriterText
                | XmlTextWriterState::XmlTextwriterPI
                | XmlTextWriterState::XmlTextwriterPIText => {}
                ty @ XmlTextWriterState::XmlTextwriterAttribute
                | ty @ XmlTextWriterState::XmlTextwriterName => {
                    if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                        count = xml_text_writer_end_attribute(writer);
                        if count < 0 {
                            return -1;
                        }
                        sum += count;
                    }

                    /* Output namespace declarations */
                    count = xml_text_writer_output_nsdecl(writer);
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                    count = (*(*writer).out).write_str(">");
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                    (*p).state = XmlTextWriterState::XmlTextwriterText;
                }
                XmlTextWriterState::XmlTextwriterCdata => {
                    xml_writer_err_msg(
                        writer,
                        XmlParserErrors::XmlErrInternalError,
                        c"xmlTextWriterStartCDATA : CDATA not allowed in this context!\n".as_ptr()
                            as _,
                    );
                    return -1;
                }
                _ => {
                    return -1;
                }
            }
        }
    }

    p = xml_malloc(size_of::<XmlTextWriterStackEntry>()) as *mut XmlTextWriterStackEntry;
    if p.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartCDATA : out of memory!\n".as_ptr() as _,
        );
        return -1;
    }

    (*p).name = null_mut();
    (*p).state = XmlTextWriterState::XmlTextwriterCdata;

    xml_list_push_front((*writer).nodes, p as _);

    count = (*(*writer).out).write_str("<![CDATA[");
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterEndCDATA:
 * @writer:  the xmlTextWriterPtr
 *
 * End an xml CDATA section.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_end_cdata(writer: XmlTextWriterPtr) -> i32 {
    let count: i32;
    let mut sum: i32;

    if writer.is_null() {
        return -1;
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return -1;
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return -1;
    }

    sum = 0;
    match (*p).state {
        XmlTextWriterState::XmlTextwriterCdata => {
            count = (*(*writer).out).write_str("]]>");
            if count < 0 {
                return -1;
            }
            sum += count;
        }
        _ => {
            return -1;
        }
    }

    xml_list_pop_front((*writer).nodes);
    sum
}

/*
 * CDATA conveniency functions
 */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatCDATA(
//     writer: xmlTextWriterPtr,
//     format: *const c_char,
//     ...
// ) -> i32;
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatCDATA(
//     writer: xmlTextWriterPtr,
//     format: *const c_char,
//     argptr: va_list,
// ) -> i32;
/**
 * xmlTextWriterWriteCDATA:
 * @writer:  the xmlTextWriterPtr
 * @content:  CDATA content
 *
 * Write an xml CDATA.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_cdata(
    writer: XmlTextWriterPtr,
    content: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    sum = 0;
    count = xml_text_writer_start_cdata(writer);
    if count == -1 {
        return -1;
    }
    sum += count;
    if !content.is_null() {
        count = xml_text_writer_write_string(writer, content);
        if count == -1 {
            return -1;
        }
        sum += count;
    }
    count = xml_text_writer_end_cdata(writer);
    if count == -1 {
        return -1;
    }
    sum += count;

    sum
}

/*
 * DTD
 */
/**
 * xmlTextWriterStartDTD:
 * @writer:  the xmlTextWriterPtr
 * @name:  the name of the DTD
 * @pubid:  the public identifier, which is an alternative to the system identifier
 * @sysid:  the system identifier, which is the URI of the DTD
 *
 * Start an xml DTD.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_start_dtd(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() || name.is_null() || *name == b'\0' {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() && !xml_link_get_data(lk).is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterStartDTD : DTD allowed only in prolog!\n".as_ptr() as _,
        );
        return -1;
    }

    let p: *mut XmlTextWriterStackEntry =
        xml_malloc(size_of::<XmlTextWriterStackEntry>()) as *mut XmlTextWriterStackEntry;
    if p.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTD : out of memory!\n".as_ptr() as _,
        );
        return -1;
    }

    (*p).name = xml_strdup(name);
    if (*p).name.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTD : out of memory!\n".as_ptr() as _,
        );
        xml_free(p as _);
        return -1;
    }
    (*p).state = XmlTextWriterState::XmlTextwriterDtd;

    xml_list_push_front((*writer).nodes, p as _);

    count = (*(*writer).out).write_str("<!DOCTYPE ");
    if count < 0 {
        return -1;
    }
    sum += count;
    count = (*(*writer).out).write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref());
    if count < 0 {
        return -1;
    }
    sum += count;

    if !pubid.is_null() {
        if sysid.is_null() {
            xml_writer_err_msg(
                writer,
                XmlParserErrors::XmlErrInternalError,
                c"xmlTextWriterStartDTD : system identifier needed!\n".as_ptr() as _,
            );
            return -1;
        }

        if (*writer).indent != 0 {
            count = (*(*writer).out).write_bytes(b"\n");
        } else {
            count = (*(*writer).out).write_bytes(b" ");
        }
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_str("PUBLIC ");
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_str(CStr::from_ptr(pubid as _).to_string_lossy().as_ref());
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    if !sysid.is_null() {
        if pubid.is_null() {
            if (*writer).indent != 0 {
                count = (*(*writer).out).write_bytes(b"\n");
            } else {
                count = (*(*writer).out).write_bytes(b" ");
            }
            if count < 0 {
                return -1;
            }
            sum += count;
            count = (*(*writer).out).write_str("SYSTEM ");
            if count < 0 {
                return -1;
            }
            sum += count;
        } else {
            if (*writer).indent != 0 {
                count = (*(*writer).out).write_str("\n       ");
            } else {
                count = (*(*writer).out).write_bytes(b" ");
            }
            if count < 0 {
                return -1;
            }
            sum += count;
        }

        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_str(CStr::from_ptr(sysid as _).to_string_lossy().as_ref());
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    sum
}

/**
 * xmlTextWriterEndDTD:
 * @writer:  the xmlTextWriterPtr
 *
 * End an xml DTD.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_end_dtd(writer: XmlTextWriterPtr) -> i32 {
    let mut do_loop: i32;
    let mut count: i32;
    let mut sum: i32;
    let mut lk: XmlLinkPtr;
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() {
        return -1;
    }

    sum = 0;
    do_loop = 1;
    while do_loop != 0 {
        lk = xml_list_front((*writer).nodes);
        if lk.is_null() {
            break;
        }
        p = xml_link_get_data(lk) as _;
        if p.is_null() {
            break;
        }
        match (*p).state {
            ty @ XmlTextWriterState::XmlTextwriterDtdText
            | ty @ XmlTextWriterState::XmlTextwriterDtd => {
                if matches!(ty, XmlTextWriterState::XmlTextwriterDtdText) {
                    count = (*(*writer).out).write_str("]");
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                }

                count = (*(*writer).out).write_str(">");

                if (*writer).indent != 0 {
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                    count = (*(*writer).out).write_str("\n");
                }

                xml_list_pop_front((*writer).nodes);
            }
            XmlTextWriterState::XmlTextwriterDtdElem
            | XmlTextWriterState::XmlTextwriterDtdElemText => {
                count = xml_text_writer_end_dtdelement(writer);
            }
            XmlTextWriterState::XmlTextwriterDtdAttl
            | XmlTextWriterState::XmlTextwriterDtdAttlText => {
                count = xml_text_writer_end_dtd_attlist(writer);
            }
            XmlTextWriterState::XmlTextwriterDtdEnty
            | XmlTextWriterState::XmlTextwriterDtdPEnt
            | XmlTextWriterState::XmlTextwriterDtdEntyText => {
                count = xml_text_writer_end_dtd_entity(writer);
            }
            XmlTextWriterState::XmlTextwriterComment => {
                count = xml_text_writer_end_comment(writer);
            }
            _ => {
                do_loop = 0;
                continue;
            }
        }

        if count < 0 {
            return -1;
        }
        sum += count;
    }

    sum
}

/*
 * DTD conveniency functions
 */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatDTD(
//     writer: xmlTextWriterPtr,
//     name: *const xmlChar,
//     pubid: *const xmlChar,
//     sysid: *const xmlChar,
//     format: *const c_char,
//     ...
// ) -> i32;
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatDTD(
//     writer: xmlTextWriterPtr,
//     name: *const xmlChar,
//     pubid: *const xmlChar,
//     sysid: *const xmlChar,
//     format: *const c_char,
//     argptr: va_list,
// ) -> i32;
/**
 * xmlTextWriterWriteDTD:
 * @writer:  the xmlTextWriterPtr
 * @name:  the name of the DTD
 * @pubid:  the public identifier, which is an alternative to the system identifier
 * @sysid:  the system identifier, which is the URI of the DTD
 * @subset:  string content of the DTD
 *
 * Write a DTD.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_dtd(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
    subset: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    sum = 0;
    count = xml_text_writer_start_dtd(writer, name, pubid, sysid);
    if count == -1 {
        return -1;
    }
    sum += count;
    if !subset.is_null() {
        count = xml_text_writer_write_string(writer, subset);
        if count == -1 {
            return -1;
        }
        sum += count;
    }
    count = xml_text_writer_end_dtd(writer);
    if count == -1 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterWriteDocType:
 *
 * this macro maps to xmlTextWriterWriteDTD
 */
const XML_TEXT_WRITER_WRITE_DOC_TYPE: unsafe extern "C" fn(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
    subset: *const XmlChar,
) -> i32 = xml_text_writer_write_dtd;

/*
 * DTD element definition
 */
/**
 * xmlTextWriterStartDTDElement:
 * @writer:  the xmlTextWriterPtr
 * @name:  the name of the DTD element
 *
 * Start an xml DTD element.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_start_dtdelement(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || name.is_null() || *name == b'\0' {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return -1;
    }

    p = xml_link_get_data(lk) as _;
    if !p.is_null() {
        match (*p).state {
            XmlTextWriterState::XmlTextwriterDtd => {
                count = (*(*writer).out).write_str(" [");
                if count < 0 {
                    return -1;
                }
                sum += count;
                if (*writer).indent != 0 {
                    count = (*(*writer).out).write_str("\n");
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                }
                (*p).state = XmlTextWriterState::XmlTextwriterDtdText;
            }
            XmlTextWriterState::XmlTextwriterDtdText | XmlTextWriterState::XmlTextwriterNone => {}
            _ => {
                return -1;
            }
        }
    }

    p = xml_malloc(size_of::<XmlTextWriterStackEntry>()) as *mut XmlTextWriterStackEntry;
    if p.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTDElement : out of memory!\n".as_ptr() as _,
        );
        return -1;
    }

    (*p).name = xml_strdup(name);
    if (*p).name.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTDElement : out of memory!\n".as_ptr() as _,
        );
        xml_free(p as _);
        return -1;
    }
    (*p).state = XmlTextWriterState::XmlTextwriterDtdElem;

    xml_list_push_front((*writer).nodes, p as _);

    if (*writer).indent != 0 {
        count = xml_text_writer_write_indent(writer);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    count = (*(*writer).out).write_str("<!ELEMENT ");
    if count < 0 {
        return -1;
    }
    sum += count;
    count = (*(*writer).out).write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref());
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterEndDTDElement:
 * @writer:  the xmlTextWriterPtr
 *
 * End an xml DTD element.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_end_dtdelement(writer: XmlTextWriterPtr) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return -1;
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return -1;
    }

    match (*p).state {
        XmlTextWriterState::XmlTextwriterDtdElem | XmlTextWriterState::XmlTextwriterDtdElemText => {
            count = (*(*writer).out).write_str(">");
            if count < 0 {
                return -1;
            }
            sum += count;
        }
        _ => {
            return -1;
        }
    }

    if (*writer).indent != 0 {
        count = (*(*writer).out).write_str("\n");
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    xml_list_pop_front((*writer).nodes);
    sum
}

/*
 * DTD element definition conveniency functions
 */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatDTDElement(
//     writer: xmlTextWriterPtr,
//     name: *const xmlChar,
//     format: *const c_char,
//     ...
// ) -> i32;
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatDTDElement(
//     writer: xmlTextWriterPtr,
//     name: *const xmlChar,
//     format: *const c_char,
//     argptr: va_list,
// ) -> i32;

/**
 * xmlTextWriterWriteDTDElement:
 * @writer:  the xmlTextWriterPtr
 * @name:  the name of the DTD element
 * @content:  content of the element
 *
 * Write a DTD element.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_dtdelement(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if content.is_null() {
        return -1;
    }

    sum = 0;
    count = xml_text_writer_start_dtdelement(writer, name);
    if count == -1 {
        return -1;
    }
    sum += count;

    count = xml_text_writer_write_string(writer, content);
    if count == -1 {
        return -1;
    }
    sum += count;

    count = xml_text_writer_end_dtdelement(writer);
    if count == -1 {
        return -1;
    }
    sum += count;

    sum
}

/*
 * DTD attribute list definition
 */
/**
 * xmlTextWriterStartDTDAttlist:
 * @writer:  the xmlTextWriterPtr
 * @name:  the name of the DTD ATTLIST
 *
 * Start an xml DTD ATTLIST.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_start_dtdattlist(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || name.is_null() || *name == b'\0' {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return -1;
    }

    p = xml_link_get_data(lk) as _;
    if !p.is_null() {
        match (*p).state {
            XmlTextWriterState::XmlTextwriterDtd => {
                count = (*(*writer).out).write_str(" [");
                if count < 0 {
                    return -1;
                }
                sum += count;
                if (*writer).indent != 0 {
                    count = (*(*writer).out).write_str("\n");
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                }
                (*p).state = XmlTextWriterState::XmlTextwriterDtdText;
            }
            XmlTextWriterState::XmlTextwriterDtdText | XmlTextWriterState::XmlTextwriterNone => {}
            _ => {
                return -1;
            }
        }
    }

    p = xml_malloc(size_of::<XmlTextWriterStackEntry>()) as *mut XmlTextWriterStackEntry;
    if p.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTDAttlist : out of memory!\n".as_ptr() as _,
        );
        return -1;
    }

    (*p).name = xml_strdup(name);
    if (*p).name.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTDAttlist : out of memory!\n".as_ptr() as _,
        );
        xml_free(p as _);
        return -1;
    }
    (*p).state = XmlTextWriterState::XmlTextwriterDtdAttl;

    xml_list_push_front((*writer).nodes, p as _);

    if (*writer).indent != 0 {
        count = xml_text_writer_write_indent(writer);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    count = (*(*writer).out).write_str("<!ATTLIST ");
    if count < 0 {
        return -1;
    }
    sum += count;
    count = (*(*writer).out).write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref());
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterEndDTDAttlist:
 * @writer:  the xmlTextWriterPtr
 *
 * End an xml DTD attribute list.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_end_dtd_attlist(writer: XmlTextWriterPtr) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return -1;
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return -1;
    }

    match (*p).state {
        XmlTextWriterState::XmlTextwriterDtdAttl | XmlTextWriterState::XmlTextwriterDtdAttlText => {
            count = (*(*writer).out).write_str(">");
            if count < 0 {
                return -1;
            }
            sum += count;
        }
        _ => {
            return -1;
        }
    }

    if (*writer).indent != 0 {
        count = (*(*writer).out).write_str("\n");
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    xml_list_pop_front((*writer).nodes);
    sum
}

/*
 * DTD attribute list definition conveniency functions
 */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatDTDAttlist(
//     writer: xmlTextWriterPtr,
//     name: *const xmlChar,
//     format: *const c_char,
//     ...
// ) -> i32;
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatDTDAttlist(
//     writer: xmlTextWriterPtr,
//     name: *const xmlChar,
//     format: *const c_char,
//     argptr: va_list,
// ) -> i32;
/**
 * xmlTextWriterWriteDTDAttlist:
 * @writer:  the xmlTextWriterPtr
 * @name:  the name of the DTD ATTLIST
 * @content:  content of the ATTLIST
 *
 * Write a DTD ATTLIST.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_dtd_attlist(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if content.is_null() {
        return -1;
    }

    sum = 0;
    count = xml_text_writer_start_dtdattlist(writer, name);
    if count == -1 {
        return -1;
    }
    sum += count;

    count = xml_text_writer_write_string(writer, content);
    if count == -1 {
        return -1;
    }
    sum += count;

    count = xml_text_writer_end_dtd_attlist(writer);
    if count == -1 {
        return -1;
    }
    sum += count;

    sum
}

/*
 * DTD entity definition
 */
/**
 * xmlTextWriterStartDTDEntity:
 * @writer:  the xmlTextWriterPtr
 * @pe:  TRUE if this is a parameter entity, FALSE if not
 * @name:  the name of the DTD ATTLIST
 *
 * Start an xml DTD ATTLIST.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_start_dtd_entity(
    writer: XmlTextWriterPtr,
    pe: i32,
    name: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || name.is_null() || *name == b'\0' {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            match (*p).state {
                XmlTextWriterState::XmlTextwriterDtd => {
                    count = (*(*writer).out).write_str(" [");
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                    if (*writer).indent != 0 {
                        count = (*(*writer).out).write_str("\n");
                        if count < 0 {
                            return -1;
                        }
                        sum += count;
                    }
                    (*p).state = XmlTextWriterState::XmlTextwriterDtdText;
                }
                _ty @ XmlTextWriterState::XmlTextwriterDtdText
                | _ty @ XmlTextWriterState::XmlTextwriterNone => {}
                _ => {
                    return -1;
                }
            }
        }
    }

    p = xml_malloc(size_of::<XmlTextWriterStackEntry>()) as *mut XmlTextWriterStackEntry;
    if p.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTDElement : out of memory!\n".as_ptr() as _,
        );
        return -1;
    }

    (*p).name = xml_strdup(name);
    if (*p).name.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTDElement : out of memory!\n".as_ptr() as _,
        );
        xml_free(p as _);
        return -1;
    }

    if pe != 0 {
        (*p).state = XmlTextWriterState::XmlTextwriterDtdPEnt;
    } else {
        (*p).state = XmlTextWriterState::XmlTextwriterDtdEnty;
    }

    xml_list_push_front((*writer).nodes, p as _);

    if (*writer).indent != 0 {
        count = xml_text_writer_write_indent(writer);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    count = (*(*writer).out).write_str("<!ENTITY ");
    if count < 0 {
        return -1;
    }
    sum += count;

    if pe != 0 {
        count = (*(*writer).out).write_str("% ");
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    count = (*(*writer).out).write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref());
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterEndDTDEntity:
 * @writer:  the xmlTextWriterPtr
 *
 * End an xml DTD entity.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_end_dtd_entity(writer: XmlTextWriterPtr) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return -1;
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return -1;
    }

    match (*p).state {
        ty @ XmlTextWriterState::XmlTextwriterDtdEntyText
        | ty @ XmlTextWriterState::XmlTextwriterDtdEnty
        | ty @ XmlTextWriterState::XmlTextwriterDtdPEnt => {
            if matches!(ty, XmlTextWriterState::XmlTextwriterDtdEntyText) {
                count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
                if count < 0 {
                    return -1;
                }
                sum += count;
            }
            count = (*(*writer).out).write_str(">");
            if count < 0 {
                return -1;
            }
            sum += count;
        }
        _ => {
            return -1;
        }
    }

    if (*writer).indent != 0 {
        count = (*(*writer).out).write_str("\n");
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    xml_list_pop_front((*writer).nodes);
    sum
}

/*
 * DTD entity definition conveniency functions
 */
// pub unsafe extern "C" fn xmlTextWriterWriteFormatDTDInternalEntity(
//     writer: xmlTextWriterPtr,
//     pe: i32,
//     name: *const xmlChar,
//     format: *const c_char,
//     ...
// ) -> i32;
// pub unsafe extern "C" fn xmlTextWriterWriteVFormatDTDInternalEntity(
//     writer: xmlTextWriterPtr,
//     pe: i32,
//     name: *const xmlChar,
//     format: *const c_char,
//     argptr: va_list,
// ) -> i32;

/**
 * xmlTextWriterWriteDTDInternalEntity:
 * @writer:  the xmlTextWriterPtr
 * @pe:  TRUE if this is a parameter entity, FALSE if not
 * @name:  the name of the DTD entity
 * @content:  content of the entity
 *
 * Write a DTD internal entity.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_dtd_internal_entity(
    writer: XmlTextWriterPtr,
    pe: i32,
    name: *const XmlChar,
    content: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if name.is_null() || *name == b'\0' || content.is_null() {
        return -1;
    }

    sum = 0;
    count = xml_text_writer_start_dtd_entity(writer, pe, name);
    if count == -1 {
        return -1;
    }
    sum += count;

    count = xml_text_writer_write_string(writer, content);
    if count == -1 {
        return -1;
    }
    sum += count;

    count = xml_text_writer_end_dtd_entity(writer);
    if count == -1 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterWriteDTDExternalEntity:
 * @writer:  the xmlTextWriterPtr
 * @pe:  TRUE if this is a parameter entity, FALSE if not
 * @name:  the name of the DTD entity
 * @pubid:  the public identifier, which is an alternative to the system identifier
 * @sysid:  the system identifier, which is the URI of the DTD
 * @ndataid:  the xml notation name.
 *
 * Write a DTD external entity. The entity must have been started with xmlTextWriterStartDTDEntity
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_dtd_external_entity(
    writer: XmlTextWriterPtr,
    pe: i32,
    name: *const XmlChar,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
    ndataid: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if pubid.is_null() && sysid.is_null() {
        return -1;
    }
    if pe != 0 && !ndataid.is_null() {
        return -1;
    }

    sum = 0;
    count = xml_text_writer_start_dtd_entity(writer, pe, name);
    if count == -1 {
        return -1;
    }
    sum += count;

    count = xml_text_writer_write_dtd_external_entity_contents(writer, pubid, sysid, ndataid);
    if count < 0 {
        return -1;
    }
    sum += count;

    count = xml_text_writer_end_dtd_entity(writer);
    if count == -1 {
        return -1;
    }
    sum += count;

    sum
}

/**
 * xmlTextWriterWriteDTDExternalEntityContents:
 * @writer:  the xmlTextWriterPtr
 * @pubid:  the public identifier, which is an alternative to the system identifier
 * @sysid:  the system identifier, which is the URI of the DTD
 * @ndataid:  the xml notation name.
 *
 * Write the contents of a DTD external entity.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_dtd_external_entity_contents(
    writer: XmlTextWriterPtr,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
    ndataid: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterWriteDTDExternalEntityContents: xmlTextWriterPtr invalid!\n".as_ptr()
                as _,
        );
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        xml_writer_err_msg(writer, XmlParserErrors::XmlErrInternalError,
                        c"xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!\n".as_ptr() as _);
        return -1;
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return -1;
    }

    match (*p).state {
        XmlTextWriterState::XmlTextwriterDtdEnty => {}
        XmlTextWriterState::XmlTextwriterDtdPEnt => {
            if !ndataid.is_null() {
                xml_writer_err_msg(writer, XmlParserErrors::XmlErrInternalError,
                                c"xmlTextWriterWriteDTDExternalEntityContents: notation not allowed with parameter entities!\n".as_ptr() as _);
                return -1;
            }
        }
        _ => {
            xml_writer_err_msg(writer, XmlParserErrors::XmlErrInternalError,
                            c"xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!\n".as_ptr() as _);
            return -1;
        }
    }

    if !pubid.is_null() {
        if sysid.is_null() {
            xml_writer_err_msg(
                writer,
                XmlParserErrors::XmlErrInternalError,
                c"xmlTextWriterWriteDTDExternalEntityContents: system identifier needed!\n".as_ptr()
                    as _,
            );
            return -1;
        }

        count = (*(*writer).out).write_str(" PUBLIC ");
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_str(CStr::from_ptr(pubid as _).to_string_lossy().as_ref());
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    if !sysid.is_null() {
        if pubid.is_null() {
            count = (*(*writer).out).write_str(" SYSTEM");
            if count < 0 {
                return -1;
            }
            sum += count;
        }

        count = (*(*writer).out).write_str(" ");
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_str(CStr::from_ptr(sysid as _).to_string_lossy().as_ref());
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    if !ndataid.is_null() {
        count = (*(*writer).out).write_str(" NDATA ");
        if count < 0 {
            return -1;
        }
        sum += count;

        count = (*(*writer).out).write_str(CStr::from_ptr(ndataid as _).to_string_lossy().as_ref());
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    sum
}

/**
 * xmlTextWriterWriteDTDEntity:
 * @writer:  the xmlTextWriterPtr
 * @pe:  TRUE if this is a parameter entity, FALSE if not
 * @name:  the name of the DTD entity
 * @pubid:  the public identifier, which is an alternative to the system identifier
 * @sysid:  the system identifier, which is the URI of the DTD
 * @ndataid:  the xml notation name.
 * @content:  content of the entity
 *
 * Write a DTD entity.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_dtd_entity(
    writer: XmlTextWriterPtr,
    pe: i32,
    name: *const XmlChar,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
    ndataid: *const XmlChar,
    content: *const XmlChar,
) -> i32 {
    if content.is_null() && pubid.is_null() && sysid.is_null() {
        return -1;
    }
    if pe != 0 && !ndataid.is_null() {
        return -1;
    }

    if pubid.is_null() && sysid.is_null() {
        return xml_text_writer_write_dtd_internal_entity(writer, pe, name, content);
    }

    xml_text_writer_write_dtd_external_entity(writer, pe, name, pubid, sysid, ndataid)
}

/*
 * DTD notation definition
 */
/**
 * xmlTextWriterWriteDTDNotation:
 * @writer:  the xmlTextWriterPtr
 * @name:  the name of the xml notation
 * @pubid:  the public identifier, which is an alternative to the system identifier
 * @sysid:  the system identifier, which is the URI of the DTD
 *
 * Write a DTD entity.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_write_dtd_notation(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
) -> i32 {
    let mut count: i32;
    let mut sum: i32;

    if writer.is_null() || name.is_null() || *name == b'\0' {
        return -1;
    }

    sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return -1;
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if !p.is_null() {
        match (*p).state {
            XmlTextWriterState::XmlTextwriterDtd => {
                count = (*(*writer).out).write_str(" [");
                if count < 0 {
                    return -1;
                }
                sum += count;
                if (*writer).indent != 0 {
                    count = (*(*writer).out).write_str("\n");
                    if count < 0 {
                        return -1;
                    }
                    sum += count;
                }
                (*p).state = XmlTextWriterState::XmlTextwriterDtdText;
            }
            XmlTextWriterState::XmlTextwriterDtdText => {}
            _ => {
                return -1;
            }
        }
    }

    if (*writer).indent != 0 {
        count = xml_text_writer_write_indent(writer);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    count = (*(*writer).out).write_str("<!NOTATION ");
    if count < 0 {
        return -1;
    }
    sum += count;
    count = (*(*writer).out).write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref());
    if count < 0 {
        return -1;
    }
    sum += count;

    if !pubid.is_null() {
        count = (*(*writer).out).write_str(" PUBLIC ");
        if count < 0 {
            return -1;
        }
        sum += count;
        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;
        count = (*(*writer).out).write_str(CStr::from_ptr(pubid as _).to_string_lossy().as_ref());
        if count < 0 {
            return -1;
        }
        sum += count;
        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    if !sysid.is_null() {
        if pubid.is_null() {
            count = (*(*writer).out).write_str(" SYSTEM");
            if count < 0 {
                return -1;
            }
            sum += count;
        }
        count = (*(*writer).out).write_str(" ");
        if count < 0 {
            return -1;
        }
        sum += count;
        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;
        count = (*(*writer).out).write_str(CStr::from_ptr(sysid as _).to_string_lossy().as_ref());
        if count < 0 {
            return -1;
        }
        sum += count;
        count = (*(*writer).out).write_bytes(&[(*writer).qchar as u8]);
        if count < 0 {
            return -1;
        }
        sum += count;
    }

    count = (*(*writer).out).write_str(">");
    if count < 0 {
        return -1;
    }
    sum += count;

    sum
}

/*
 * Indentation
 */
/**
 * xmlTextWriterSetIndent:
 * @writer:  the xmlTextWriterPtr
 * @indent:  do indentation?
 *
 * Set indentation output. indent = 0 do not indentation. indent > 0 do indentation.
 *
 * Returns -1 on error or 0 otherwise.
 */
pub unsafe extern "C" fn xml_text_writer_set_indent(writer: XmlTextWriterPtr, indent: i32) -> i32 {
    if writer.is_null() || indent < 0 {
        return -1;
    }

    (*writer).indent = indent;
    (*writer).doindent = 1;

    0
}

/**
 * xmlTextWriterSetIndentString:
 * @writer:  the xmlTextWriterPtr
 * @str:  the xmlChar string
 *
 * Set string indentation.
 *
 * Returns -1 on error or 0 otherwise.
 */
pub unsafe extern "C" fn xml_text_writer_set_indent_string(
    writer: XmlTextWriterPtr,
    str: *const XmlChar,
) -> i32 {
    if writer.is_null() || str.is_null() {
        return -1;
    }

    if !(*writer).ichar.is_null() {
        xml_free((*writer).ichar as _);
    }
    (*writer).ichar = xml_strdup(str);

    if (*writer).ichar.is_null() {
        -1
    } else {
        0
    }
}

/**
 * xmlTextWriterSetQuoteChar:
 * @writer:  the xmlTextWriterPtr
 * @quotechar:  the quote character
 *
 * Set the character used for quoting attributes.
 *
 * Returns -1 on error or 0 otherwise.
 */
pub unsafe extern "C" fn xml_text_writer_set_quote_char(
    writer: XmlTextWriterPtr,
    quotechar: XmlChar,
) -> i32 {
    if writer.is_null() || (quotechar != b'\'' && quotechar != b'"') {
        return -1;
    }

    (*writer).qchar = quotechar as _;

    0
}

/*
 * misc
 */
/**
 * xmlTextWriterFlush:
 * @writer:  the xmlTextWriterPtr
 *
 * Flush the output buffer.
 *
 * Returns the bytes written (may be 0 because of buffering) or -1 in case of error
 */
pub unsafe extern "C" fn xml_text_writer_flush(writer: XmlTextWriterPtr) -> i32 {
    if writer.is_null() {
        return -1;
    }

    if (*writer).out.is_null() {
        0
    } else {
        (*(*writer).out).flush()
    }
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_new_text_writer() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_XML_OUTPUT_BUFFER_PTR {
                let mem_base = xml_mem_blocks();
                let mut out = gen_xml_output_buffer_ptr(n_out, 0);

                let ret_val = xml_new_text_writer(out);
                if !ret_val.is_null() {
                    out = null_mut();
                }
                desret_xml_text_writer_ptr(ret_val);
                des_xml_output_buffer_ptr(n_out, out, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlNewTextWriter",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlNewTextWriter()");
                    eprintln!(" {}", n_out);
                }
            }
        }
    }

    #[test]
    fn test_xml_new_text_writer_filename() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_uri in 0..GEN_NB_FILEOUTPUT {
                for n_compression in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let uri = gen_fileoutput(n_uri, 0);
                    let compression = gen_int(n_compression, 1);

                    let ret_val = xml_new_text_writer_filename(uri, compression);
                    desret_xml_text_writer_ptr(ret_val);
                    des_fileoutput(n_uri, uri, 0);
                    des_int(n_compression, compression, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewTextWriterFilename",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlNewTextWriterFilename()"
                        );
                        eprint!(" {}", n_uri);
                        eprintln!(" {}", n_compression);
                    }
                }
            }
        }
    }

    // #[test]
    // fn test_xml_new_text_writer_memory() {
    //     #[cfg(feature = "writer")]
    //     unsafe {
    //         let mut leaks = 0;

    //         for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
    //             for n_compression in 0..GEN_NB_INT {
    //                 let mem_base = xml_mem_blocks();
    //                 let buf = gen_xml_buffer_ptr(n_buf, 0);
    //                 let compression = gen_int(n_compression, 1);

    //                 let ret_val = xml_new_text_writer_memory(buf, compression);
    //                 desret_xml_text_writer_ptr(ret_val);
    //                 des_xml_buffer_ptr(n_buf, buf, 0);
    //                 des_int(n_compression, compression, 1);
    //                 reset_last_error();
    //                 if mem_base != xml_mem_blocks() {
    //                     leaks += 1;
    //                     eprint!(
    //                         "Leak of {} blocks found in xmlNewTextWriterMemory",
    //                         xml_mem_blocks() - mem_base
    //                     );
    //                     assert!(
    //                         leaks == 0,
    //                         "{leaks} Leaks are found in xmlNewTextWriterMemory()"
    //                     );
    //                     eprint!(" {}", n_buf);
    //                     eprintln!(" {}", n_compression);
    //                 }
    //             }
    //         }
    //     }
    // }

    #[test]
    fn test_xml_new_text_writer_push_parser() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_PARSER_CTXT_PTR {
                for n_compression in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let mut ctxt = gen_xml_parser_ctxt_ptr(n_ctxt, 0);
                    let compression = gen_int(n_compression, 1);

                    let ret_val = xml_new_text_writer_push_parser(ctxt, compression);
                    if !ctxt.is_null() {
                        xml_free_doc((*ctxt).my_doc);
                        (*ctxt).my_doc = null_mut();
                    }
                    if !ret_val.is_null() {
                        ctxt = null_mut();
                    }
                    desret_xml_text_writer_ptr(ret_val);
                    des_xml_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_int(n_compression, compression, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlNewTextWriterPushParser",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlNewTextWriterPushParser()"
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_compression);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_text_writer_tree() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_XML_DOC_PTR {
                for n_node in 0..GEN_NB_XML_NODE_PTR {
                    for n_compression in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_xml_doc_ptr(n_doc, 0);
                        let node = gen_xml_node_ptr(n_node, 1);
                        let compression = gen_int(n_compression, 2);

                        let ret_val = xml_new_text_writer_tree(doc, node, compression);
                        desret_xml_text_writer_ptr(ret_val);
                        des_xml_doc_ptr(n_doc, doc, 0);
                        des_xml_node_ptr(n_node, node, 1);
                        des_int(n_compression, compression, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlNewTextWriterTree",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlNewTextWriterTree()"
                            );
                            eprint!(" {}", n_doc);
                            eprint!(" {}", n_node);
                            eprintln!(" {}", n_compression);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_end_attribute() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_end_attribute(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterEndAttribute",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterEndAttribute()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_end_cdata() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_end_cdata(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterEndCDATA",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterEndCDATA()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_end_comment() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_end_comment(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterEndComment",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterEndComment()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_end_dtd() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_end_dtd(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterEndDTD",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterEndDTD()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_end_dtdattlist() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_end_dtd_attlist(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterEndDTDAttlist",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterEndDTDAttlist()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_end_dtdelement() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_end_dtdelement(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterEndDTDElement",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterEndDTDElement()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_end_dtdentity() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_end_dtd_entity(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterEndDTDEntity",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterEndDTDEntity()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_end_document() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_end_document(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterEndDocument",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterEndDocument()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_end_element() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_end_element(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterEndElement",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterEndElement()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_end_pi() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_end_pi(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterEndPI",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterEndPI()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_flush() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_flush(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterFlush",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterFlush()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_full_end_element() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_full_end_element(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterFullEndElement",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterFullEndElement()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_set_indent() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_indent in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let indent = gen_int(n_indent, 1);

                    let ret_val = xml_text_writer_set_indent(writer, indent);
                    desret_int(ret_val);
                    des_xml_text_writer_ptr(n_writer, writer, 0);
                    des_int(n_indent, indent, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextWriterSetIndent",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextWriterSetIndent()"
                        );
                        eprint!(" {}", n_writer);
                        eprintln!(" {}", n_indent);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_set_indent_string() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let str = gen_const_xml_char_ptr(n_str, 1);

                    let ret_val = xml_text_writer_set_indent_string(writer, str);
                    desret_int(ret_val);
                    des_xml_text_writer_ptr(n_writer, writer, 0);
                    des_const_xml_char_ptr(n_str, str, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextWriterSetIndentString",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextWriterSetIndentString()"
                        );
                        eprint!(" {}", n_writer);
                        eprintln!(" {}", n_str);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_set_quote_char() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_quotechar in 0..GEN_NB_XML_CHAR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let quotechar = gen_xml_char(n_quotechar, 1);

                    let ret_val = xml_text_writer_set_quote_char(writer, quotechar);
                    desret_int(ret_val);
                    des_xml_text_writer_ptr(n_writer, writer, 0);
                    des_xml_char(n_quotechar, quotechar, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextWriterSetQuoteChar",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextWriterSetQuoteChar()"
                        );
                        eprint!(" {}", n_writer);
                        eprintln!(" {}", n_quotechar);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_start_attribute() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_text_writer_start_attribute(writer, name);
                    desret_int(ret_val);
                    des_xml_text_writer_ptr(n_writer, writer, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextWriterStartAttribute",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextWriterStartAttribute()"
                        );
                        eprint!(" {}", n_writer);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_start_attribute_ns() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_namespace_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let writer = gen_xml_text_writer_ptr(n_writer, 0);
                            let prefix = gen_const_xml_char_ptr(n_prefix, 1);
                            let name = gen_const_xml_char_ptr(n_name, 2);
                            let namespace_uri = gen_const_xml_char_ptr(n_namespace_uri, 3);

                            let ret_val = xml_text_writer_start_attribute_ns(
                                writer,
                                prefix,
                                name,
                                namespace_uri,
                            );
                            desret_int(ret_val);
                            des_xml_text_writer_ptr(n_writer, writer, 0);
                            des_const_xml_char_ptr(n_prefix, prefix, 1);
                            des_const_xml_char_ptr(n_name, name, 2);
                            des_const_xml_char_ptr(n_namespace_uri, namespace_uri, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlTextWriterStartAttributeNS",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlTextWriterStartAttributeNS()"
                                );
                                eprint!(" {}", n_writer);
                                eprint!(" {}", n_prefix);
                                eprint!(" {}", n_name);
                                eprintln!(" {}", n_namespace_uri);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_start_cdata() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_start_cdata(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterStartCDATA",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterStartCDATA()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_start_comment() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                let ret_val = xml_text_writer_start_comment(writer);
                desret_int(ret_val);
                des_xml_text_writer_ptr(n_writer, writer, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlTextWriterStartComment",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlTextWriterStartComment()"
                    );
                    eprintln!(" {}", n_writer);
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_start_dtd() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_pubid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_sysid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let writer = gen_xml_text_writer_ptr(n_writer, 0);
                            let name = gen_const_xml_char_ptr(n_name, 1);
                            let pubid = gen_const_xml_char_ptr(n_pubid, 2);
                            let sysid = gen_const_xml_char_ptr(n_sysid, 3);

                            let ret_val = xml_text_writer_start_dtd(writer, name, pubid, sysid);
                            desret_int(ret_val);
                            des_xml_text_writer_ptr(n_writer, writer, 0);
                            des_const_xml_char_ptr(n_name, name, 1);
                            des_const_xml_char_ptr(n_pubid, pubid, 2);
                            des_const_xml_char_ptr(n_sysid, sysid, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlTextWriterStartDTD",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlTextWriterStartDTD()"
                                );
                                eprint!(" {}", n_writer);
                                eprint!(" {}", n_name);
                                eprint!(" {}", n_pubid);
                                eprintln!(" {}", n_sysid);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_start_dtdattlist() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_text_writer_start_dtdattlist(writer, name);
                    desret_int(ret_val);
                    des_xml_text_writer_ptr(n_writer, writer, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextWriterStartDTDAttlist",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextWriterStartDTDAttlist()"
                        );
                        eprint!(" {}", n_writer);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_start_dtdelement() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_text_writer_start_dtdelement(writer, name);
                    desret_int(ret_val);
                    des_xml_text_writer_ptr(n_writer, writer, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextWriterStartDTDElement",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextWriterStartDTDElement()"
                        );
                        eprint!(" {}", n_writer);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_start_dtdentity() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_pe in 0..GEN_NB_INT {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let pe = gen_int(n_pe, 1);
                        let name = gen_const_xml_char_ptr(n_name, 2);

                        let ret_val = xml_text_writer_start_dtd_entity(writer, pe, name);
                        desret_int(ret_val);
                        des_xml_text_writer_ptr(n_writer, writer, 0);
                        des_int(n_pe, pe, 1);
                        des_const_xml_char_ptr(n_name, name, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlTextWriterStartDTDEntity",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlTextWriterStartDTDEntity()"
                            );
                            eprint!(" {}", n_writer);
                            eprint!(" {}", n_pe);
                            eprintln!(" {}", n_name);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_start_document() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_version in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_encoding in 0..GEN_NB_CONST_CHAR_PTR {
                        for n_standalone in 0..GEN_NB_CONST_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let writer = gen_xml_text_writer_ptr(n_writer, 0);
                            let version = gen_const_char_ptr(n_version, 1);
                            let encoding = gen_const_char_ptr(n_encoding, 2);
                            let standalone = gen_const_char_ptr(n_standalone, 3);

                            let ret_val = xml_text_writer_start_document(
                                writer, version, encoding, standalone,
                            );
                            desret_int(ret_val);
                            des_xml_text_writer_ptr(n_writer, writer, 0);
                            des_const_char_ptr(n_version, version, 1);
                            des_const_char_ptr(n_encoding, encoding, 2);
                            des_const_char_ptr(n_standalone, standalone, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlTextWriterStartDocument",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlTextWriterStartDocument()"
                                );
                                eprint!(" {}", n_writer);
                                eprint!(" {}", n_version);
                                eprint!(" {}", n_encoding);
                                eprintln!(" {}", n_standalone);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_start_element() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_text_writer_start_element(writer, name);
                    desret_int(ret_val);
                    des_xml_text_writer_ptr(n_writer, writer, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextWriterStartElement",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextWriterStartElement()"
                        );
                        eprint!(" {}", n_writer);
                        eprintln!(" {}", n_name);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_start_element_ns() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_namespace_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let writer = gen_xml_text_writer_ptr(n_writer, 0);
                            let prefix = gen_const_xml_char_ptr(n_prefix, 1);
                            let name = gen_const_xml_char_ptr(n_name, 2);
                            let namespace_uri = gen_const_xml_char_ptr(n_namespace_uri, 3);

                            let ret_val = xml_text_writer_start_element_ns(
                                writer,
                                prefix,
                                name,
                                namespace_uri,
                            );
                            desret_int(ret_val);
                            des_xml_text_writer_ptr(n_writer, writer, 0);
                            des_const_xml_char_ptr(n_prefix, prefix, 1);
                            des_const_xml_char_ptr(n_name, name, 2);
                            des_const_xml_char_ptr(n_namespace_uri, namespace_uri, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlTextWriterStartElementNS",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlTextWriterStartElementNS()"
                                );
                                eprint!(" {}", n_writer);
                                eprint!(" {}", n_prefix);
                                eprint!(" {}", n_name);
                                eprintln!(" {}", n_namespace_uri);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_start_pi() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_target in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let target = gen_const_xml_char_ptr(n_target, 1);

                    let ret_val = xml_text_writer_start_pi(writer, target);
                    desret_int(ret_val);
                    des_xml_text_writer_ptr(n_writer, writer, 0);
                    des_const_xml_char_ptr(n_target, target, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextWriterStartPI",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextWriterStartPI()"
                        );
                        eprint!(" {}", n_writer);
                        eprintln!(" {}", n_target);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_attribute() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let content = gen_const_xml_char_ptr(n_content, 2);

                        let ret_val = xml_text_writer_write_attribute(writer, name, content);
                        desret_int(ret_val);
                        des_xml_text_writer_ptr(n_writer, writer, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_content, content, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlTextWriterWriteAttribute",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlTextWriterWriteAttribute()"
                            );
                            eprint!(" {}", n_writer);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_content);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_attribute_ns() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_namespace_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                let mem_base = xml_mem_blocks();
                                let writer = gen_xml_text_writer_ptr(n_writer, 0);
                                let prefix = gen_const_xml_char_ptr(n_prefix, 1);
                                let name = gen_const_xml_char_ptr(n_name, 2);
                                let namespace_uri = gen_const_xml_char_ptr(n_namespace_uri, 3);
                                let content = gen_const_xml_char_ptr(n_content, 4);

                                let ret_val = xml_text_writer_write_attribute_ns(
                                    writer,
                                    prefix,
                                    name,
                                    namespace_uri,
                                    content,
                                );
                                desret_int(ret_val);
                                des_xml_text_writer_ptr(n_writer, writer, 0);
                                des_const_xml_char_ptr(n_prefix, prefix, 1);
                                des_const_xml_char_ptr(n_name, name, 2);
                                des_const_xml_char_ptr(n_namespace_uri, namespace_uri, 3);
                                des_const_xml_char_ptr(n_content, content, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlTextWriterWriteAttributeNS",
                                        xml_mem_blocks() - mem_base
                                    );
                                    assert!(leaks == 0, "{leaks} Leaks are found in xmlTextWriterWriteAttributeNS()");
                                    eprint!(" {}", n_writer);
                                    eprint!(" {}", n_prefix);
                                    eprint!(" {}", n_name);
                                    eprint!(" {}", n_namespace_uri);
                                    eprintln!(" {}", n_content);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_base64() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_data in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_start in 0..GEN_NB_INT {
                        for n_len in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let writer = gen_xml_text_writer_ptr(n_writer, 0);
                            let data = gen_const_char_ptr(n_data, 1);
                            let mut start = gen_int(n_start, 2);
                            let mut len = gen_int(n_len, 3);
                            if !data.is_null() && start > xml_strlen(data as _) {
                                start = 0;
                            }
                            if !data.is_null() && len > xml_strlen(data as _) {
                                len = 0;
                            }

                            let ret_val = xml_text_writer_write_base64(writer, data, start, len);
                            desret_int(ret_val);
                            des_xml_text_writer_ptr(n_writer, writer, 0);
                            des_const_char_ptr(n_data, data, 1);
                            des_int(n_start, start, 2);
                            des_int(n_len, len, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlTextWriterWriteBase64",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlTextWriterWriteBase64()"
                                );
                                eprint!(" {}", n_writer);
                                eprint!(" {}", n_data);
                                eprint!(" {}", n_start);
                                eprintln!(" {}", n_len);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_bin_hex() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_data in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_start in 0..GEN_NB_INT {
                        for n_len in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let writer = gen_xml_text_writer_ptr(n_writer, 0);
                            let data = gen_const_char_ptr(n_data, 1);
                            let mut start = gen_int(n_start, 2);
                            let mut len = gen_int(n_len, 3);
                            if !data.is_null() && start > xml_strlen(data as _) {
                                start = 0;
                            }
                            if !data.is_null() && len > xml_strlen(data as _) {
                                len = 0;
                            }

                            let ret_val = xml_text_writer_write_bin_hex(writer, data, start, len);
                            desret_int(ret_val);
                            des_xml_text_writer_ptr(n_writer, writer, 0);
                            des_const_char_ptr(n_data, data, 1);
                            des_int(n_start, start, 2);
                            des_int(n_len, len, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlTextWriterWriteBinHex",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlTextWriterWriteBinHex()"
                                );
                                eprint!(" {}", n_writer);
                                eprint!(" {}", n_data);
                                eprint!(" {}", n_start);
                                eprintln!(" {}", n_len);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_cdata() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    let ret_val = xml_text_writer_write_cdata(writer, content);
                    desret_int(ret_val);
                    des_xml_text_writer_ptr(n_writer, writer, 0);
                    des_const_xml_char_ptr(n_content, content, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextWriterWriteCDATA",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextWriterWriteCDATA()"
                        );
                        eprint!(" {}", n_writer);
                        eprintln!(" {}", n_content);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_comment() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    let ret_val = xml_text_writer_write_comment(writer, content);
                    desret_int(ret_val);
                    des_xml_text_writer_ptr(n_writer, writer, 0);
                    des_const_xml_char_ptr(n_content, content, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextWriterWriteComment",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextWriterWriteComment()"
                        );
                        eprint!(" {}", n_writer);
                        eprintln!(" {}", n_content);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_dtd() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_pubid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_sysid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_subset in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                let mem_base = xml_mem_blocks();
                                let writer = gen_xml_text_writer_ptr(n_writer, 0);
                                let name = gen_const_xml_char_ptr(n_name, 1);
                                let pubid = gen_const_xml_char_ptr(n_pubid, 2);
                                let sysid = gen_const_xml_char_ptr(n_sysid, 3);
                                let subset = gen_const_xml_char_ptr(n_subset, 4);

                                let ret_val =
                                    xml_text_writer_write_dtd(writer, name, pubid, sysid, subset);
                                desret_int(ret_val);
                                des_xml_text_writer_ptr(n_writer, writer, 0);
                                des_const_xml_char_ptr(n_name, name, 1);
                                des_const_xml_char_ptr(n_pubid, pubid, 2);
                                des_const_xml_char_ptr(n_sysid, sysid, 3);
                                des_const_xml_char_ptr(n_subset, subset, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlTextWriterWriteDTD",
                                        xml_mem_blocks() - mem_base
                                    );
                                    assert!(
                                        leaks == 0,
                                        "{leaks} Leaks are found in xmlTextWriterWriteDTD()"
                                    );
                                    eprint!(" {}", n_writer);
                                    eprint!(" {}", n_name);
                                    eprint!(" {}", n_pubid);
                                    eprint!(" {}", n_sysid);
                                    eprintln!(" {}", n_subset);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_dtdattlist() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let content = gen_const_xml_char_ptr(n_content, 2);

                        let ret_val = xml_text_writer_write_dtd_attlist(writer, name, content);
                        desret_int(ret_val);
                        des_xml_text_writer_ptr(n_writer, writer, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_content, content, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlTextWriterWriteDTDAttlist",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlTextWriterWriteDTDAttlist()"
                            );
                            eprint!(" {}", n_writer);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_content);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_dtdelement() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let content = gen_const_xml_char_ptr(n_content, 2);

                        let ret_val = xml_text_writer_write_dtdelement(writer, name, content);
                        desret_int(ret_val);
                        des_xml_text_writer_ptr(n_writer, writer, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_content, content, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlTextWriterWriteDTDElement",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlTextWriterWriteDTDElement()"
                            );
                            eprint!(" {}", n_writer);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_content);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_dtdentity() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_pe in 0..GEN_NB_INT {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_pubid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_sysid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                for n_ndataid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                    for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                        let mem_base = xml_mem_blocks();
                                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                                        let pe = gen_int(n_pe, 1);
                                        let name = gen_const_xml_char_ptr(n_name, 2);
                                        let pubid = gen_const_xml_char_ptr(n_pubid, 3);
                                        let sysid = gen_const_xml_char_ptr(n_sysid, 4);
                                        let ndataid = gen_const_xml_char_ptr(n_ndataid, 5);
                                        let content = gen_const_xml_char_ptr(n_content, 6);

                                        let ret_val = xml_text_writer_write_dtd_entity(
                                            writer, pe, name, pubid, sysid, ndataid, content,
                                        );
                                        desret_int(ret_val);
                                        des_xml_text_writer_ptr(n_writer, writer, 0);
                                        des_int(n_pe, pe, 1);
                                        des_const_xml_char_ptr(n_name, name, 2);
                                        des_const_xml_char_ptr(n_pubid, pubid, 3);
                                        des_const_xml_char_ptr(n_sysid, sysid, 4);
                                        des_const_xml_char_ptr(n_ndataid, ndataid, 5);
                                        des_const_xml_char_ptr(n_content, content, 6);
                                        reset_last_error();
                                        if mem_base != xml_mem_blocks() {
                                            leaks += 1;
                                            eprint!("Leak of {} blocks found in xmlTextWriterWriteDTDEntity", xml_mem_blocks() - mem_base);
                                            assert!(leaks == 0, "{leaks} Leaks are found in xmlTextWriterWriteDTDEntity()");
                                            eprint!(" {}", n_writer);
                                            eprint!(" {}", n_pe);
                                            eprint!(" {}", n_name);
                                            eprint!(" {}", n_pubid);
                                            eprint!(" {}", n_sysid);
                                            eprint!(" {}", n_ndataid);
                                            eprintln!(" {}", n_content);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_dtdexternal_entity() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_pe in 0..GEN_NB_INT {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_pubid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_sysid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                for n_ndataid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                    let mem_base = xml_mem_blocks();
                                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                                    let pe = gen_int(n_pe, 1);
                                    let name = gen_const_xml_char_ptr(n_name, 2);
                                    let pubid = gen_const_xml_char_ptr(n_pubid, 3);
                                    let sysid = gen_const_xml_char_ptr(n_sysid, 4);
                                    let ndataid = gen_const_xml_char_ptr(n_ndataid, 5);

                                    let ret_val = xml_text_writer_write_dtd_external_entity(
                                        writer, pe, name, pubid, sysid, ndataid,
                                    );
                                    desret_int(ret_val);
                                    des_xml_text_writer_ptr(n_writer, writer, 0);
                                    des_int(n_pe, pe, 1);
                                    des_const_xml_char_ptr(n_name, name, 2);
                                    des_const_xml_char_ptr(n_pubid, pubid, 3);
                                    des_const_xml_char_ptr(n_sysid, sysid, 4);
                                    des_const_xml_char_ptr(n_ndataid, ndataid, 5);
                                    reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!("Leak of {} blocks found in xmlTextWriterWriteDTDExternalEntity", xml_mem_blocks() - mem_base);
                                        assert!(leaks == 0, "{leaks} Leaks are found in xmlTextWriterWriteDTDExternalEntity()");
                                        eprint!(" {}", n_writer);
                                        eprint!(" {}", n_pe);
                                        eprint!(" {}", n_name);
                                        eprint!(" {}", n_pubid);
                                        eprint!(" {}", n_sysid);
                                        eprintln!(" {}", n_ndataid);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_dtdexternal_entity_contents() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_pubid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_sysid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_ndataid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let writer = gen_xml_text_writer_ptr(n_writer, 0);
                            let pubid = gen_const_xml_char_ptr(n_pubid, 1);
                            let sysid = gen_const_xml_char_ptr(n_sysid, 2);
                            let ndataid = gen_const_xml_char_ptr(n_ndataid, 3);

                            let ret_val = xml_text_writer_write_dtd_external_entity_contents(
                                writer, pubid, sysid, ndataid,
                            );
                            desret_int(ret_val);
                            des_xml_text_writer_ptr(n_writer, writer, 0);
                            des_const_xml_char_ptr(n_pubid, pubid, 1);
                            des_const_xml_char_ptr(n_sysid, sysid, 2);
                            des_const_xml_char_ptr(n_ndataid, ndataid, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!("Leak of {} blocks found in xmlTextWriterWriteDTDExternalEntityContents", xml_mem_blocks() - mem_base);
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlTextWriterWriteDTDExternalEntityContents()");
                                eprint!(" {}", n_writer);
                                eprint!(" {}", n_pubid);
                                eprint!(" {}", n_sysid);
                                eprintln!(" {}", n_ndataid);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_dtdinternal_entity() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_pe in 0..GEN_NB_INT {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let writer = gen_xml_text_writer_ptr(n_writer, 0);
                            let pe = gen_int(n_pe, 1);
                            let name = gen_const_xml_char_ptr(n_name, 2);
                            let content = gen_const_xml_char_ptr(n_content, 3);

                            let ret_val = xml_text_writer_write_dtd_internal_entity(
                                writer, pe, name, content,
                            );
                            desret_int(ret_val);
                            des_xml_text_writer_ptr(n_writer, writer, 0);
                            des_int(n_pe, pe, 1);
                            des_const_xml_char_ptr(n_name, name, 2);
                            des_const_xml_char_ptr(n_content, content, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!("Leak of {} blocks found in xmlTextWriterWriteDTDInternalEntity", xml_mem_blocks() - mem_base);
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlTextWriterWriteDTDInternalEntity()");
                                eprint!(" {}", n_writer);
                                eprint!(" {}", n_pe);
                                eprint!(" {}", n_name);
                                eprintln!(" {}", n_content);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_dtdnotation() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_pubid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_sysid in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let writer = gen_xml_text_writer_ptr(n_writer, 0);
                            let name = gen_const_xml_char_ptr(n_name, 1);
                            let pubid = gen_const_xml_char_ptr(n_pubid, 2);
                            let sysid = gen_const_xml_char_ptr(n_sysid, 3);

                            let ret_val =
                                xml_text_writer_write_dtd_notation(writer, name, pubid, sysid);
                            desret_int(ret_val);
                            des_xml_text_writer_ptr(n_writer, writer, 0);
                            des_const_xml_char_ptr(n_name, name, 1);
                            des_const_xml_char_ptr(n_pubid, pubid, 2);
                            des_const_xml_char_ptr(n_sysid, sysid, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlTextWriterWriteDTDNotation",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlTextWriterWriteDTDNotation()"
                                );
                                eprint!(" {}", n_writer);
                                eprint!(" {}", n_name);
                                eprint!(" {}", n_pubid);
                                eprintln!(" {}", n_sysid);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_element() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let content = gen_const_xml_char_ptr(n_content, 2);

                        let ret_val = xml_text_writer_write_element(writer, name, content);
                        desret_int(ret_val);
                        des_xml_text_writer_ptr(n_writer, writer, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_content, content, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlTextWriterWriteElement",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlTextWriterWriteElement()"
                            );
                            eprint!(" {}", n_writer);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_content);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_element_ns() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_namespace_uri in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                let mem_base = xml_mem_blocks();
                                let writer = gen_xml_text_writer_ptr(n_writer, 0);
                                let prefix = gen_const_xml_char_ptr(n_prefix, 1);
                                let name = gen_const_xml_char_ptr(n_name, 2);
                                let namespace_uri = gen_const_xml_char_ptr(n_namespace_uri, 3);
                                let content = gen_const_xml_char_ptr(n_content, 4);

                                let ret_val = xml_text_writer_write_element_ns(
                                    writer,
                                    prefix,
                                    name,
                                    namespace_uri,
                                    content,
                                );
                                desret_int(ret_val);
                                des_xml_text_writer_ptr(n_writer, writer, 0);
                                des_const_xml_char_ptr(n_prefix, prefix, 1);
                                des_const_xml_char_ptr(n_name, name, 2);
                                des_const_xml_char_ptr(n_namespace_uri, namespace_uri, 3);
                                des_const_xml_char_ptr(n_content, content, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlTextWriterWriteElementNS",
                                        xml_mem_blocks() - mem_base
                                    );
                                    assert!(
                                        leaks == 0,
                                        "{leaks} Leaks are found in xmlTextWriterWriteElementNS()"
                                    );
                                    eprint!(" {}", n_writer);
                                    eprint!(" {}", n_prefix);
                                    eprint!(" {}", n_name);
                                    eprint!(" {}", n_namespace_uri);
                                    eprintln!(" {}", n_content);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_format_attribute() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_format_attribute_ns() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_format_cdata() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_format_comment() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_format_dtd() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_format_dtdattlist() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_format_dtdelement() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_format_dtdinternal_entity() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_format_element() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_format_element_ns() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_format_pi() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_format_raw() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_format_string() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_pi() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_target in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let target = gen_const_xml_char_ptr(n_target, 1);
                        let content = gen_const_xml_char_ptr(n_content, 2);

                        let ret_val = xml_text_writer_write_pi(writer, target, content);
                        desret_int(ret_val);
                        des_xml_text_writer_ptr(n_writer, writer, 0);
                        des_const_xml_char_ptr(n_target, target, 1);
                        des_const_xml_char_ptr(n_content, content, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlTextWriterWritePI",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlTextWriterWritePI()"
                            );
                            eprint!(" {}", n_writer);
                            eprint!(" {}", n_target);
                            eprintln!(" {}", n_content);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_raw() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    let ret_val = xml_text_writer_write_raw(writer, content);
                    desret_int(ret_val);
                    des_xml_text_writer_ptr(n_writer, writer, 0);
                    des_const_xml_char_ptr(n_content, content, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextWriterWriteRaw",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextWriterWriteRaw()"
                        );
                        eprint!(" {}", n_writer);
                        eprintln!(" {}", n_content);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_raw_len() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let content = gen_const_xml_char_ptr(n_content, 1);
                        let mut len = gen_int(n_len, 2);
                        if !content.is_null() && len > xml_strlen(content) {
                            len = 0;
                        }

                        let ret_val = xml_text_writer_write_raw_len(writer, content, len);
                        desret_int(ret_val);
                        des_xml_text_writer_ptr(n_writer, writer, 0);
                        des_const_xml_char_ptr(n_content, content, 1);
                        des_int(n_len, len, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlTextWriterWriteRawLen",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlTextWriterWriteRawLen()"
                            );
                            eprint!(" {}", n_writer);
                            eprint!(" {}", n_content);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_string() {
        #[cfg(feature = "writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    let ret_val = xml_text_writer_write_string(writer, content);
                    desret_int(ret_val);
                    des_xml_text_writer_ptr(n_writer, writer, 0);
                    des_const_xml_char_ptr(n_content, content, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlTextWriterWriteString",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlTextWriterWriteString()"
                        );
                        eprint!(" {}", n_writer);
                        eprintln!(" {}", n_content);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_text_writer_write_vformat_attribute() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_vformat_attribute_ns() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_vformat_cdata() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_vformat_comment() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_vformat_dtd() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_vformat_dtdattlist() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_vformat_dtdelement() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_vformat_dtdinternal_entity() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_vformat_element() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_vformat_element_ns() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_vformat_pi() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_vformat_raw() {

        /* missing type support */
    }

    #[test]
    fn test_xml_text_writer_write_vformat_string() {

        /* missing type support */
    }
}

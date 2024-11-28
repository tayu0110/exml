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

// Copyright of the original code is the following.
// --------
// Summary: text writing API for XML
// Description: text writing API for XML
//
// Copy: See Copyright for the status of this software.
//
// Author: Alfred Mickautsch <alfred@mickautsch.de>
// --------
// xmlwriter.c: XML text writer implementation
//
// For license and disclaimer see the license and disclaimer of libxml2.
//
// alfred@mickautsch.de

use std::{
    cell::RefCell,
    ffi::{c_char, CStr, CString},
    io::{self, Write},
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
    error::XmlParserErrors,
    globals::GenericErrorContext,
    io::XmlOutputBuffer,
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
        uri::xml_canonic_path,
        xmlstring::{xml_strcasecmp, xml_strcat, xml_strcmp, xml_strdup, xml_strlen, XmlChar},
    },
    tree::{xml_free_doc, xml_new_doc, XmlDocPtr, XmlNodePtr},
};

use super::xmlsave::xml_buf_attr_serialize_txt_content;

// Types are kept private
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum XmlTextWriterState {
    XmlTextwriterNone = 0,
    XmlTextwriterName,
    XmlTextwriterAttribute,
    XmlTextwriterText,
    XmlTextwriterPI,
    XmlTextwriterPIText,
    XmlTextwriterCDATA,
    XmlTextwriterDTD,
    XmlTextwriterDTDText,
    XmlTextwriterDTDElem,
    XmlTextwriterDTDElemText,
    XmlTextwriterDTDAttl,
    XmlTextwriterDTDAttlText,
    XmlTextwriterDTDEnty, /* entity */
    XmlTextwriterDTDEntyText,
    XmlTextwriterDTDPEnt, /* parameter entity */
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

pub type XmlTextWriterPtr<'a> = *mut XmlTextWriter<'a>;
#[repr(C)]
pub struct XmlTextWriter<'a> {
    out: XmlOutputBuffer<'a>, /* output buffer */
    nodes: XmlListPtr,        /* element name stack */
    nsstack: XmlListPtr,      /* name spaces stack */
    level: i32,
    indent: i32,         /* enable indent */
    doindent: i32,       /* internal indent flag */
    ichar: *mut XmlChar, /* indent character */
    qchar: c_char,       /* character used for quoting attribute values */
    ctxt: XmlParserCtxtPtr,
    no_doc_free: i32,
    doc: XmlDocPtr,
}

impl Default for XmlTextWriter<'_> {
    fn default() -> Self {
        Self {
            out: XmlOutputBuffer::default(),
            nodes: null_mut(),
            nsstack: null_mut(),
            level: 0,
            indent: 0,
            doindent: 0,
            ichar: null_mut(),
            qchar: 0,
            ctxt: null_mut(),
            no_doc_free: 0,
            doc: null_mut(),
        }
    }
}

/// Free callback for the xmlList.
#[doc(alias = "xmlFreeTextWriterStackEntry")]
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

/// Compare callback for the xmlList.
///
/// Returns -1, 0, 1
#[doc(alias = "xmlCmpTextWriterStackEntry")]
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

/// Free callback for the xmlList.
#[doc(alias = "xmlFreeTextWriterNsStackEntry")]
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

/// Compare callback for the xmlList.
///
/// Returns -1, 0, 1
#[doc(alias = "xmlCmpTextWriterNsStackEntry")]
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

/// Handle a writer error
#[doc(alias = "xmlWriterErrMsg")]
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

/// Create a new xmlNewTextWriter structure using an xmlOutputBufferPtr
///
/// # Note
/// The @out parameter will be deallocated when the writer is closed
/// (if the call succeed.)
///
/// Returns the new xmlTextWriterPtr or NULL in case of error
#[doc(alias = "xmlNewTextWriter")]
pub unsafe fn xml_new_text_writer(out: XmlOutputBuffer) -> XmlTextWriterPtr {
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
    std::ptr::write(&mut *ret, XmlTextWriter::default());

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

/// Create a new xmlNewTextWriter structure with @uri as output
///
/// Returns the new xmlTextWriterPtr or NULL in case of error
#[doc(alias = "xmlNewTextWriterFilename")]
pub unsafe fn xml_new_text_writer_filename(uri: &str, compression: i32) -> XmlTextWriterPtr {
    let Some(out) = XmlOutputBuffer::from_uri(uri, None, compression) else {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlIOEIO,
            c"xmlNewTextWriterFilename : cannot open uri\n".as_ptr() as _,
        );
        return null_mut();
    };

    let ret: XmlTextWriterPtr = xml_new_text_writer(out);
    if ret.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrNoMemory,
            c"xmlNewTextWriterFilename : out of memory!\n".as_ptr() as _,
        );
        return null_mut();
    }

    (*ret).indent = 0;
    (*ret).doindent = 0;
    ret
}

/// Handle a writer error
#[doc(alias = "xmlWriterErrMsgInt")]
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

/// Write callback for the xmlOutputBuffer with target xmlBuffer
///
/// Returns -1, 0, 1
#[doc(alias = "xmlTextWriterWriteDocCallback")]
unsafe extern "C" fn xml_text_writer_write_doc_callback(
    context: *mut c_void,
    str: *const c_char,
    len: i32,
) -> i32 {
    let ctxt: XmlParserCtxtPtr = context as XmlParserCtxtPtr;

    let rc = xml_parse_chunk(ctxt, str, len, 0);
    if rc != 0 {
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

/// Close callback for the xmlOutputBuffer with target xmlBuffer
///
/// Returns -1, 0, 1
#[doc(alias = "xmlTextWriterCloseDocCallback")]
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

struct TextWriterPushContext {
    context: XmlParserCtxtPtr,
}

impl Write for TextWriterPushContext {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let ptr = buf.as_ptr() as *const i8;
        let len = buf.len() as i32;
        let res = unsafe { xml_text_writer_write_doc_callback(self.context as _, ptr, len) };
        if res >= 0 {
            Ok(res as usize)
        } else {
            Err(io::Error::last_os_error())
        }
    }
    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl Drop for TextWriterPushContext {
    fn drop(&mut self) {
        unsafe {
            xml_text_writer_close_doc_callback(self.context as _);
        }
    }
}

/// Create a new xmlNewTextWriter structure with @ctxt as output
/// NOTE: the @ctxt context will be freed with the resulting writer
///       (if the call succeeds).
/// TODO: handle compression
///
/// Returns the new xmlTextWriterPtr or NULL in case of error
#[doc(alias = "xmlNewTextWriterPushParser")]
pub unsafe extern "C" fn xml_new_text_writer_push_parser(
    ctxt: XmlParserCtxtPtr,
    _compression: i32,
) -> XmlTextWriterPtr<'static> {
    if ctxt.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"xmlNewTextWriterPushParser : invalid context!\n".as_ptr() as _,
        );
        return null_mut();
    }

    let Some(out) = XmlOutputBuffer::from_writer(TextWriterPushContext { context: ctxt }, None)
    else {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"xmlNewTextWriterPushParser : error at xmlOutputBufferCreateIO!\n".as_ptr() as _,
        );
        return null_mut();
    };

    let ret: XmlTextWriterPtr = xml_new_text_writer(out);
    if ret.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            c"xmlNewTextWriterPushParser : error at xmlNewTextWriter!\n".as_ptr() as _,
        );
        return null_mut();
    }

    (*ret).ctxt = ctxt;

    ret
}

/// called at the start of document processing.
#[doc(alias = "xmlTextWriterStartDocumentCallback")]
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
            if (*doc).children.is_none() {
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

/// Create a new xmlNewTextWriter structure with @*doc as output
///
/// Returns the new xmlTextWriterPtr or NULL in case of error
#[doc(alias = "xmlNewTextWriterDoc")]
pub unsafe extern "C" fn xml_new_text_writer_doc(
    doc: *mut XmlDocPtr,
    compression: i32,
) -> XmlTextWriterPtr<'static> {
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

    (*(*ctxt).my_doc).set_compress_mode(compression);

    if !doc.is_null() {
        *doc = (*ctxt).my_doc;
        (*ret).no_doc_free = 1;
    }

    ret
}

/// Create a new xmlNewTextWriter structure with @doc as output starting at @node
///
/// Returns the new xmlTextWriterPtr or NULL in case of error
#[doc(alias = "xmlNewTextWriterTree")]
pub unsafe extern "C" fn xml_new_text_writer_tree(
    doc: XmlDocPtr,
    node: XmlNodePtr,
    compression: i32,
) -> XmlTextWriterPtr<'static> {
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

    (*doc).set_compress_mode(compression);

    ret
}

/// Deallocate all the resources associated to the writer
#[doc(alias = "xmlFreeTextWriter")]
pub unsafe extern "C" fn xml_free_text_writer(writer: XmlTextWriterPtr) {
    if writer.is_null() {
        return;
    }

    (*writer).out.flush();
    (*writer).out = XmlOutputBuffer::default();

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

/// Start a new xml document
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartDocument")]
pub unsafe fn xml_text_writer_start_document(
    writer: XmlTextWriterPtr,
    version: *const c_char,
    encoding: *const c_char,
    standalone: *const c_char,
) -> io::Result<usize> {
    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterStartDocument : invalid writer!\n".as_ptr() as _,
        );
        return Err(io::Error::other(
            "xmlTextWriterStartDocument : invalid writer!",
        ));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() && !xml_link_get_data(lk).is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterStartDocument : not allowed in this context!\n".as_ptr() as _,
        );
        return Err(io::Error::other(
            "xmlTextWriterStartDocument : not allowed in this context!",
        ));
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
            return Err(io::Error::other(
                "xmlTextWriterStartDocument : unsupported encoding",
            ));
        };
        Some(encoder)
    } else {
        None
    };

    (*writer).out.encoder = encoder.map(|e| Rc::new(RefCell::new(e)));
    if (*writer).out.encoder.is_some() {
        if (*writer).out.conv.is_none() {
            (*writer).out.conv = XmlBufRef::with_capacity(4000);
        }
        (*writer).out.encode(true);
        if !(*writer).doc.is_null() && (*(*writer).doc).encoding.is_none() {
            let encoder = (*writer).out.encoder.as_ref().unwrap().borrow();
            (*(*writer).doc).encoding = Some(encoder.name().to_owned());
        }
    } else {
        (*writer).out.conv = None;
    }

    let mut sum = (*writer).out.write_str("<?xml version=")?;
    sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
    if !version.is_null() {
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(version).to_string_lossy().as_ref())?;
    } else {
        sum += (*writer).out.write_str("1.0")?;
    }
    sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
    if (*writer).out.encoder.is_some() {
        sum += (*writer).out.write_str(" encoding=")?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
        sum += (*writer)
            .out
            .write_str((*writer).out.encoder.as_ref().unwrap().borrow().name())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
    }

    if !standalone.is_null() {
        sum += (*writer).out.write_str(" standalone=")?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(standalone).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
    }

    sum += (*writer).out.write_str("?>\n")?;
    Ok(sum)
}

/// End an xml document. All open elements are closed, and
/// the content is flushed to the output.
///
/// Returns the bytes written or -1 in case of error
#[doc(alias = "xmlTextWriterEndDocument")]
pub unsafe fn xml_text_writer_end_document(writer: XmlTextWriterPtr) -> io::Result<usize> {
    let mut lk: XmlLinkPtr;
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterEndDocument : invalid writer!\n".as_ptr() as _,
        );
        return Err(io::Error::other(
            "xmlTextWriterEndDocument : invalid writer!",
        ));
    }

    let mut sum = 0;
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
                sum += xml_text_writer_end_element(writer)?;
            }
            XmlTextWriterState::XmlTextwriterPI | XmlTextWriterState::XmlTextwriterPIText => {
                sum += xml_text_writer_end_pi(writer)?;
            }
            XmlTextWriterState::XmlTextwriterCDATA => {
                sum += xml_text_writer_end_cdata(writer)?;
            }
            XmlTextWriterState::XmlTextwriterDTD
            | XmlTextWriterState::XmlTextwriterDTDText
            | XmlTextWriterState::XmlTextwriterDTDElem
            | XmlTextWriterState::XmlTextwriterDTDElemText
            | XmlTextWriterState::XmlTextwriterDTDAttl
            | XmlTextWriterState::XmlTextwriterDTDAttlText
            | XmlTextWriterState::XmlTextwriterDTDEnty
            | XmlTextWriterState::XmlTextwriterDTDEntyText
            | XmlTextWriterState::XmlTextwriterDTDPEnt => {
                sum += xml_text_writer_end_dtd(writer)?;
            }
            XmlTextWriterState::XmlTextwriterComment => {
                sum += xml_text_writer_end_comment(writer)?;
            }
            _ => {}
        }
    }

    if (*writer).indent == 0 {
        sum += (*writer).out.write_str("\n")?;
    }

    sum += xml_text_writer_flush(writer) as usize;

    Ok(sum)
}

/// Output the current namespace declarations.
#[doc(alias = "xmlTextWriterOutputNSDecl")]
unsafe fn xml_text_writer_output_nsdecl(writer: XmlTextWriterPtr) -> io::Result<usize> {
    let mut lk: XmlLinkPtr;
    let mut np: *mut XmlTextWriterNsStackEntry;

    let mut sum = 0;
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
            let count = xml_text_writer_write_attribute(writer, prefix, namespace_uri);
            xml_free(namespace_uri as _);
            xml_free(prefix as _);

            if count.is_err() {
                xml_list_delete((*writer).nsstack);
                (*writer).nsstack = null_mut();
                return count;
            }
            sum += count.unwrap();
        }
    }
    Ok(sum)
}

/// Write indent string.
///
/// Returns -1 on error or the number of strings written.
#[doc(alias = "xmlTextWriterWriteIndent")]
unsafe fn xml_text_writer_write_indent(writer: XmlTextWriterPtr) -> io::Result<usize> {
    let lksize = xml_list_size((*writer).nodes);
    if lksize < 1 {
        return Err(io::Error::other("List is empty"));
    }
    for _ in 0..lksize - 1 {
        (*writer).out.write_str(
            CStr::from_ptr((*writer).ichar as _)
                .to_string_lossy()
                .as_ref(),
        )?;
    }

    Ok(lksize as usize - 1)
}

/// Start an xml comment.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartComment")]
pub unsafe fn xml_text_writer_start_comment(writer: XmlTextWriterPtr) -> io::Result<usize> {
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterStartComment : invalid writer!\n".as_ptr() as _,
        );
        return Err(io::Error::other(
            "xmlTextWriterStartComment : invalid writer!",
        ));
    }

    let mut sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            match (*p).state {
                XmlTextWriterState::XmlTextwriterText | XmlTextWriterState::XmlTextwriterNone => {}
                XmlTextWriterState::XmlTextwriterName => {
                    // Output namespace declarations
                    sum += xml_text_writer_output_nsdecl(writer)?;
                    sum += (*writer).out.write_str(">")?;
                    if (*writer).indent != 0 {
                        sum += (*writer).out.write_str("\n")?;
                    }
                    (*p).state = XmlTextWriterState::XmlTextwriterText;
                }
                _ => {
                    return Err(io::Error::other("Invalid state"));
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
        return Err(io::Error::other(
            "xmlTextWriterStartElement : out of memory!",
        ));
    }

    (*p).name = null_mut();
    (*p).state = XmlTextWriterState::XmlTextwriterComment;

    xml_list_push_front((*writer).nodes, p as _);

    if (*writer).indent != 0 {
        sum += xml_text_writer_write_indent(writer)?;
    }

    sum += (*writer).out.write_str("<!--")?;
    Ok(sum)
}

/// End the current xml comment.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterEndComment")]
pub unsafe fn xml_text_writer_end_comment(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterEndComment : invalid writer!\n".as_ptr() as _,
        );
        return Err(io::Error::other(
            "xmlTextWriterEndComment : invalid writer!",
        ));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterEndComment : not allowed in this context!\n".as_ptr() as _,
        );
        return Err(io::Error::other(
            "xmlTextWriterEndComment : not allowed in this context!",
        ));
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return Err(io::Error::other("XmlTextWriterStackEntry is NULL"));
    }

    let mut sum = 0;
    match (*p).state {
        XmlTextWriterState::XmlTextwriterComment => {
            sum += (*writer).out.write_str("-->")?;
        }
        _ => {
            return Err(io::Error::other("Invalid state"));
        }
    }

    if (*writer).indent != 0 {
        sum += (*writer).out.write_str("\n")?;
    }

    xml_list_pop_front((*writer).nodes);
    Ok(sum)
}

/// Write an xml comment.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteComment")]
pub unsafe fn xml_text_writer_write_comment(
    writer: XmlTextWriterPtr,
    content: *const XmlChar,
) -> io::Result<usize> {
    let mut sum = xml_text_writer_start_comment(writer)?;
    sum += xml_text_writer_write_string(writer, content)?;
    sum += xml_text_writer_end_comment(writer)?;
    Ok(sum)
}

/// Start an xml element.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartElement")]
pub unsafe fn xml_text_writer_start_element(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
) -> io::Result<usize> {
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || name.is_null() || (*name == b'\0') {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let mut sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            match (*p).state {
                XmlTextWriterState::XmlTextwriterPI | XmlTextWriterState::XmlTextwriterPIText => {
                    return Err(io::Error::other("Not start element"));
                }
                XmlTextWriterState::XmlTextwriterNone => {}
                ty @ XmlTextWriterState::XmlTextwriterAttribute
                | ty @ XmlTextWriterState::XmlTextwriterName => {
                    if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                        sum += xml_text_writer_end_attribute(writer)?;
                    }

                    // Output namespace declarations
                    sum += xml_text_writer_output_nsdecl(writer)?;
                    sum += (*writer).out.write_str(">")?;
                    if (*writer).indent != 0 {
                        // count =
                        (*writer).out.write_str("\n");
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
        return Err(io::Error::other(
            "xmlTextWriterStartElement : out of memory!",
        ));
    }

    (*p).name = xml_strdup(name);
    if (*p).name.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartElement : out of memory!\n".as_ptr() as _,
        );
        xml_free(p as _);
        return Err(io::Error::other(
            "xmlTextWriterStartElement : out of memory!",
        ));
    }
    (*p).state = XmlTextWriterState::XmlTextwriterName;

    xml_list_push_front((*writer).nodes, p as _);

    if (*writer).indent != 0 {
        sum += xml_text_writer_write_indent(writer)?;
    }

    sum += (*writer).out.write_str("<")?;
    sum += (*writer)
        .out
        .write_str(CStr::from_ptr((*p).name as _).to_string_lossy().as_ref())?;
    Ok(sum)
}

/// Start an xml element with namespace support.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartElementNS")]
pub unsafe fn xml_text_writer_start_element_ns(
    writer: XmlTextWriterPtr,
    prefix: *const XmlChar,
    name: *const XmlChar,
    namespace_uri: *const XmlChar,
) -> io::Result<usize> {
    let mut buf: *mut XmlChar;

    if writer.is_null() || name.is_null() || (*name == b'\0') {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    buf = null_mut();
    if !prefix.is_null() {
        buf = xml_strdup(prefix);
        buf = xml_strcat(buf, c":".as_ptr() as _);
    }
    buf = xml_strcat(buf, name);

    let mut sum = 0;
    let count = xml_text_writer_start_element(writer, buf);
    xml_free(buf as _);
    sum += count?;

    if !namespace_uri.is_null() {
        let p: *mut XmlTextWriterNsStackEntry =
            xml_malloc(size_of::<XmlTextWriterNsStackEntry>()) as *mut XmlTextWriterNsStackEntry;
        if p.is_null() {
            xml_writer_err_msg(
                writer,
                XmlParserErrors::XmlErrNoMemory,
                c"xmlTextWriterStartElementNS : out of memory!\n".as_ptr() as _,
            );
            return Err(io::Error::other(
                "xmlTextWriterStartElementNS : out of memory!",
            ));
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
            return Err(io::Error::other(
                "xmlTextWriterStartElementNS : out of memory!",
            ));
        }
        (*p).elem = xml_list_front((*writer).nodes);

        xml_list_push_front((*writer).nsstack, p as _);
    }

    Ok(sum)
}

/// End the current xml element.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterEndElement")]
pub unsafe fn xml_text_writer_end_element(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        return Err(io::Error::other("Writer is invalid"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        xml_list_delete((*writer).nsstack);
        (*writer).nsstack = null_mut();
        return Err(io::Error::other("Nodes link is NULL"));
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        xml_list_delete((*writer).nsstack);
        (*writer).nsstack = null_mut();
        return Err(io::Error::other("XmlTextWriterStackEntry is NULL"));
    }

    let mut sum = 0;
    match (*p).state {
        ty @ XmlTextWriterState::XmlTextwriterAttribute
        | ty @ XmlTextWriterState::XmlTextwriterName => {
            if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                let count = xml_text_writer_end_attribute(writer);
                if count.is_err() {
                    xml_list_delete((*writer).nsstack);
                    (*writer).nsstack = null_mut();
                    return count;
                }
                sum += count.unwrap();
            }

            // Output namespace declarations
            sum += xml_text_writer_output_nsdecl(writer)?;

            if (*writer).indent != 0
            /* next element needs indent */
            {
                (*writer).doindent = 1;
            }
            sum += (*writer).out.write_str("/>")?;
        }
        XmlTextWriterState::XmlTextwriterText => {
            if (*writer).indent != 0 && (*writer).doindent != 0 {
                sum += xml_text_writer_write_indent(writer)?;
                (*writer).doindent = 1;
            } else {
                (*writer).doindent = 1;
            }
            sum += (*writer).out.write_str("</")?;
            sum += (*writer)
                .out
                .write_str(CStr::from_ptr((*p).name as _).to_string_lossy().as_ref())?;
            sum += (*writer).out.write_str(">")?;
        }
        _ => {
            return Err(io::Error::other("Invalid state"));
        }
    }

    if (*writer).indent != 0 {
        sum += (*writer).out.write_str("\n")?;
    }

    xml_list_pop_front((*writer).nodes);
    Ok(sum)
}

/// End the current xml element. Writes an end tag even if the element is empty
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterFullEndElement")]
pub unsafe fn xml_text_writer_full_end_element(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        return Err(io::Error::other("Writer is invalid"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return Err(io::Error::other("Nodes link is NULL"));
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return Err(io::Error::other("XmlTextWriterStackEntry is NULL"));
    }

    let mut sum = 0;
    match (*p).state {
        ty @ XmlTextWriterState::XmlTextwriterAttribute
        | ty @ XmlTextWriterState::XmlTextwriterName
        | ty @ XmlTextWriterState::XmlTextwriterText => {
            if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                sum += xml_text_writer_end_attribute(writer)?;
            }

            if matches!(
                ty,
                XmlTextWriterState::XmlTextwriterAttribute | XmlTextWriterState::XmlTextwriterName
            ) {
                // Output namespace declarations
                sum += xml_text_writer_output_nsdecl(writer)?;
                sum += (*writer).out.write_str(">")?;
                if (*writer).indent != 0 {
                    (*writer).doindent = 0;
                }
            }

            if (*writer).indent != 0 && (*writer).doindent != 0 {
                sum += xml_text_writer_write_indent(writer)?;
                (*writer).doindent = 1;
            } else {
                (*writer).doindent = 1;
            }
            sum += (*writer).out.write_str("</")?;
            sum += (*writer)
                .out
                .write_str(CStr::from_ptr((*p).name as _).to_string_lossy().as_ref())?;
            sum += (*writer).out.write_str(">")?;
        }
        _ => {
            return Err(io::Error::other("Invalid state"));
        }
    }

    if (*writer).indent != 0 {
        sum += (*writer).out.write_str("\n")?;
    }

    xml_list_pop_front((*writer).nodes);
    Ok(sum)
}

/// Write an xml element.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteElement")]
pub unsafe fn xml_text_writer_write_element(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> io::Result<usize> {
    let mut sum = 0;
    sum += xml_text_writer_start_element(writer, name)?;
    if !content.is_null() {
        sum += xml_text_writer_write_string(writer, content)?;
    }
    sum += xml_text_writer_end_element(writer)?;
    Ok(sum)
}

/// Write an xml element with namespace support.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteElementNS")]
pub unsafe fn xml_text_writer_write_element_ns(
    writer: XmlTextWriterPtr,
    prefix: *const XmlChar,
    name: *const XmlChar,
    namespace_uri: *const XmlChar,
    content: *const XmlChar,
) -> io::Result<usize> {
    if writer.is_null() || name.is_null() || *name == b'\0' {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let mut sum = xml_text_writer_start_element_ns(writer, prefix, name, namespace_uri)?;
    sum += xml_text_writer_write_string(writer, content)?;
    sum += xml_text_writer_end_element(writer)?;
    Ok(sum)
}

/// Write state dependent strings.
///
/// Returns -1 on error or the number of characters written.
#[doc(alias = "xmlTextWriterHandleStateDependencies")]
unsafe fn xml_text_writer_handle_state_dependencies(
    writer: XmlTextWriterPtr,
    p: *mut XmlTextWriterStackEntry,
) -> io::Result<usize> {
    let mut extra: [c_char; 3] = [0; 3];

    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    if p.is_null() {
        return Ok(0);
    }

    let mut sum = 0;
    extra[0] = b'\0' as _;
    extra[1] = b'\0' as _;
    extra[2] = b'\0' as _;
    if !p.is_null() {
        match (*p).state {
            XmlTextWriterState::XmlTextwriterName => {
                // Output namespace declarations
                sum += xml_text_writer_output_nsdecl(writer)?;
                extra[0] = b'>' as _;
                (*p).state = XmlTextWriterState::XmlTextwriterText;
            }
            XmlTextWriterState::XmlTextwriterPI => {
                extra[0] = b' ' as _;
                (*p).state = XmlTextWriterState::XmlTextwriterPIText;
            }
            XmlTextWriterState::XmlTextwriterDTD => {
                extra[0] = b' ' as _;
                extra[1] = b'[' as _;
                (*p).state = XmlTextWriterState::XmlTextwriterDTDText;
            }
            XmlTextWriterState::XmlTextwriterDTDElem => {
                extra[0] = b' ' as _;
                (*p).state = XmlTextWriterState::XmlTextwriterDTDElemText;
            }
            XmlTextWriterState::XmlTextwriterDTDAttl => {
                extra[0] = b' ' as _;
                (*p).state = XmlTextWriterState::XmlTextwriterDTDAttlText;
            }
            XmlTextWriterState::XmlTextwriterDTDEnty | XmlTextWriterState::XmlTextwriterDTDPEnt => {
                extra[0] = b' ' as _;
                extra[1] = (*writer).qchar;
                (*p).state = XmlTextWriterState::XmlTextwriterDTDEntyText;
            }
            _ => {}
        }
    }

    if extra[0] != b'\0' as i8 {
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(extra.as_ptr()).to_string_lossy().as_ref())?;
    }

    Ok(sum)
}

/// Write an xml text.
/// TODO: what about entities and special chars??
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteRawLen")]
pub unsafe fn xml_text_writer_write_raw_len(
    writer: XmlTextWriterPtr,
    content: *const XmlChar,
    len: i32,
) -> io::Result<usize> {
    let p: *mut XmlTextWriterStackEntry;

    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterWriteRawLen : invalid writer!\n".as_ptr() as _,
        );
        return Err(io::Error::other(
            "xmlTextWriterWriteRawLen : invalid writer!",
        ));
    }

    if content.is_null() || len < 0 {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterWriteRawLen : invalid content!\n".as_ptr() as _,
        );
        return Err(io::Error::other(
            "xmlTextWriterWriteRawLen : invalid content!",
        ));
    }

    let mut sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        sum += xml_text_writer_handle_state_dependencies(writer, p)?;
    }

    if (*writer).indent != 0 {
        (*writer).doindent = 0;
    }

    if !content.is_null() {
        sum += (*writer)
            .out
            .write_bytes(from_raw_parts(content as _, len as usize))?;
    }

    Ok(sum)
}

/// Write a raw xml text.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteRaw")]
pub unsafe fn xml_text_writer_write_raw(
    writer: XmlTextWriterPtr,
    content: *const XmlChar,
) -> io::Result<usize> {
    xml_text_writer_write_raw_len(writer, content, xml_strlen(content))
}

/// Write an xml text.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteString")]
pub unsafe fn xml_text_writer_write_string(
    writer: XmlTextWriterPtr,
    content: *const XmlChar,
) -> io::Result<usize> {
    let p: *mut XmlTextWriterStackEntry;
    let mut buf: *mut XmlChar;

    if writer.is_null() || content.is_null() {
        return Err(io::Error::other("Writer of content is NULL"));
    }

    let mut sum = 0;
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
                        (*writer).out.buffer.map_or(null_mut(), |buf| buf.as_ptr()),
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
        let count = xml_text_writer_write_raw(writer, buf);
        if buf != content as _ {
            // buf was allocated by us, so free it
            xml_free(buf as _);
        }
        sum += count?;
    }

    Ok(sum)
}

const B64LINELEN: usize = 72;
const B64CRLF: &str = "\r\n";

/// Write base64 encoded data to an xmlOutputBuffer.
/// Adapted from John Walker's base64.c (http://www.fourmilab.ch/).
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlOutputBufferWriteBase64")]
unsafe fn xml_output_buffer_write_base64(
    out: &mut XmlOutputBuffer,
    len: i32,
    data: *const u8,
) -> io::Result<usize> {
    const DTABLE: [u8; 64] = [
        b'A', b'B', b'C', b'D', b'E', b'F', b'G', b'H', b'I', b'J', b'K', b'L', b'M', b'N', b'O',
        b'P', b'Q', b'R', b'S', b'T', b'U', b'V', b'W', b'X', b'Y', b'Z', b'a', b'b', b'c', b'd',
        b'e', b'f', b'g', b'h', b'i', b'j', b'k', b'l', b'm', b'n', b'o', b'p', b'q', b'r', b's',
        b't', b'u', b'v', b'w', b'x', b'y', b'z', b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7',
        b'8', b'9', b'+', b'/',
    ];

    let mut i: i32;
    let mut linelen: i32;

    if len < 0 || data.is_null() {
        return Err(io::Error::other("Data is invalid"));
    }

    linelen = 0;
    let mut sum = 0;

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
                sum += out.write_bytes(B64CRLF.as_bytes())?;
                linelen = 0;
            }
            sum += out.write_bytes(&ogroup)?;
            linelen += 4;
        }

        if i >= len {
            break;
        }
    }

    Ok(sum)
}

/// Write an base64 encoded xml text.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteBase64")]
pub unsafe fn xml_text_writer_write_base64(
    writer: XmlTextWriterPtr,
    data: *const c_char,
    start: i32,
    len: i32,
) -> io::Result<usize> {
    let p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || data.is_null() || start < 0 || len < 0 {
        return Err(io::Error::other("Writer or content is invalid"));
    }

    let mut sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            sum += xml_text_writer_handle_state_dependencies(writer, p)?;
        }
    }

    if (*writer).indent != 0 {
        (*writer).doindent = 0;
    }

    sum += xml_output_buffer_write_base64(&mut (*writer).out, len, data.add(start as usize) as _)?;
    Ok(sum)
}

/// Write hqx encoded data to an xmlOutputBuffer.
/// ::todo
///
/// Returns the bytes written (may be 0 because of buffering)
/// or -1 in case of error
#[doc(alias = "xmlOutputBufferWriteBinHex")]
unsafe fn xml_output_buffer_write_bin_hex(
    out: &mut XmlOutputBuffer,
    len: i32,
    data: *const u8,
) -> io::Result<usize> {
    const HEX: [u8; 16] = [
        b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'A', b'B', b'C', b'D', b'E',
        b'F',
    ];
    if data.is_null() || len < 0 {
        return Err(io::Error::other("Data is invalid"));
    }

    let mut sum = 0;
    for i in 0..len as usize {
        let hi = *data.add(i) as usize >> 4;
        sum += (*out).write_bytes(&HEX[hi..hi + 1])?;
        let lo = *data.add(i) as usize & 0xF;
        sum += (*out).write_bytes(&HEX[lo..lo + 1])?;
    }

    Ok(sum)
}

/// Write a BinHex encoded xml text.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteBinHex")]
pub unsafe fn xml_text_writer_write_bin_hex(
    writer: XmlTextWriterPtr,
    data: *const c_char,
    start: i32,
    len: i32,
) -> io::Result<usize> {
    let p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || data.is_null() || start < 0 || len < 0 {
        return Err(io::Error::other("Writer or content is invalid"));
    }

    let mut sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            sum += xml_text_writer_handle_state_dependencies(writer, p)?;
        }
    }

    if (*writer).indent != 0 {
        (*writer).doindent = 0;
    }

    sum += xml_output_buffer_write_bin_hex(&mut (*writer).out, len, data.add(start as usize) as _)?;
    Ok(sum)
}

/// Start an xml attribute.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartAttribute")]
pub unsafe fn xml_text_writer_start_attribute(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
) -> io::Result<usize> {
    if writer.is_null() || name.is_null() || *name == b'\0' {
        return Err(io::Error::other("Writer or name is invalid"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return Err(io::Error::other("Node link is NULL"));
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return Err(io::Error::other("XmlTextWriterStackEntry is NULL"));
    }

    let mut sum = 0;
    match (*p).state {
        ty @ XmlTextWriterState::XmlTextwriterAttribute
        | ty @ XmlTextWriterState::XmlTextwriterName => {
            if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                sum += xml_text_writer_end_attribute(writer)?;
            }

            sum += (*writer).out.write_str(" ")?;
            sum += (*writer)
                .out
                .write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref())?;
            sum += (*writer).out.write_str("=")?;
            sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
            (*p).state = XmlTextWriterState::XmlTextwriterAttribute;
        }
        _ => {
            return Err(io::Error::other("State is invalid"));
        }
    }

    Ok(sum)
}

/// Start an xml attribute with namespace support.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartAttributeNS")]
pub unsafe fn xml_text_writer_start_attribute_ns(
    writer: XmlTextWriterPtr,
    prefix: *const XmlChar,
    name: *const XmlChar,
    namespace_uri: *const XmlChar,
) -> io::Result<usize> {
    let mut buf: *mut XmlChar;
    let p: *mut XmlTextWriterNsStackEntry;

    if writer.is_null() || name.is_null() || *name == b'\0' {
        return Err(io::Error::other("Writer or name is invalid"));
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
                // Namespace already defined on element skip
                buf = null_mut();
            } else {
                // Prefix mismatch so error out
                return Err(io::Error::other("Prefix mismatch"));
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
                return Err(io::Error::other(
                    "xmlTextWriterStartAttributeNS : out of memory!",
                ));
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
                return Err(io::Error::other(
                    "xmlTextWriterStartAttributeNS : out of memory!",
                ));
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

    let count = xml_text_writer_start_attribute(writer, buf);
    xml_free(buf as _);
    count
}

/// End the current xml element.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterEndAttribute")]
pub unsafe fn xml_text_writer_end_attribute(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return Err(io::Error::other("List is NULL"));
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return Err(io::Error::other("Link data is NULL"));
    }

    let mut sum = 0;
    match (*p).state {
        XmlTextWriterState::XmlTextwriterAttribute => {
            (*p).state = XmlTextWriterState::XmlTextwriterName;

            sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
        }
        _ => {
            return Err(io::Error::other("Not attribute"));
        }
    }

    Ok(sum)
}

/// Write an xml attribute.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteAttribute")]
pub unsafe fn xml_text_writer_write_attribute(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> io::Result<usize> {
    let mut sum = 0;
    sum += xml_text_writer_start_attribute(writer, name)?;
    sum += xml_text_writer_write_string(writer, content)?;
    sum += xml_text_writer_end_attribute(writer)?;
    Ok(sum)
}

/// Write an xml attribute.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteAttributeNS")]
pub unsafe fn xml_text_writer_write_attribute_ns(
    writer: XmlTextWriterPtr,
    prefix: *const XmlChar,
    name: *const XmlChar,
    namespace_uri: *const XmlChar,
    content: *const XmlChar,
) -> io::Result<usize> {
    if writer.is_null() || name.is_null() || *name == b'\0' {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let mut sum = 0;
    sum += xml_text_writer_start_attribute_ns(writer, prefix, name, namespace_uri)?;
    sum += xml_text_writer_write_string(writer, content)?;
    sum += xml_text_writer_end_attribute(writer)?;
    Ok(sum)
}

/// Start an xml PI.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartPI")]
pub unsafe fn xml_text_writer_start_pi(
    writer: XmlTextWriterPtr,
    target: *const XmlChar,
) -> io::Result<usize> {
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || target.is_null() || *target == b'\0' {
        return Err(io::Error::other("Writer or target is NULL"));
    }

    if xml_strcasecmp(target, c"xml".as_ptr() as _) == 0 {
        xml_writer_err_msg(writer, XmlParserErrors::XmlErrInternalError,
                        c"xmlTextWriterStartPI : target name [Xx][Mm][Ll] is reserved for xml standardization!\n".as_ptr() as _);
        return Err(io::Error::other(
            "xmlTextWriterStartPI : target name [Xx][Mm][Ll] is reserved for xml standardization!",
        ));
    }

    let mut sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            match (*p).state {
                ty @ XmlTextWriterState::XmlTextwriterAttribute
                | ty @ XmlTextWriterState::XmlTextwriterName => {
                    if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                        sum += xml_text_writer_end_attribute(writer)?;
                    }
                    // Output namespace declarations
                    sum += xml_text_writer_output_nsdecl(writer)?;
                    sum += (*writer).out.write_str(">")?;
                    (*p).state = XmlTextWriterState::XmlTextwriterText;
                }
                XmlTextWriterState::XmlTextwriterNone
                | XmlTextWriterState::XmlTextwriterText
                | XmlTextWriterState::XmlTextwriterDTD => {}
                XmlTextWriterState::XmlTextwriterPI | XmlTextWriterState::XmlTextwriterPIText => {
                    xml_writer_err_msg(
                        writer,
                        XmlParserErrors::XmlErrInternalError,
                        c"xmlTextWriterStartPI : nested PI!\n".as_ptr() as _,
                    );
                    return Err(io::Error::other("xmlTextWriterStartPI : nested PI!"));
                }
                _ => {
                    return Err(io::Error::other("Invalid state"));
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
        return Err(io::Error::other("xmlTextWriterStartPI : out of memory!"));
    }

    (*p).name = xml_strdup(target);
    if (*p).name.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartPI : out of memory!\n".as_ptr() as _,
        );
        xml_free(p as _);
        return Err(io::Error::other("xmlTextWriterStartPI : out of memory!"));
    }
    (*p).state = XmlTextWriterState::XmlTextwriterPI;

    xml_list_push_front((*writer).nodes, p as _);

    sum += (*writer).out.write_str("<?")?;
    sum += (*writer)
        .out
        .write_str(CStr::from_ptr((*p).name as _).to_string_lossy().as_ref())?;
    Ok(sum)
}

/// End the current xml PI.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterEndPI")]
pub unsafe fn xml_text_writer_end_pi(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return Ok(0);
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return Ok(0);
    }

    let mut sum = 0;
    match (*p).state {
        XmlTextWriterState::XmlTextwriterPI | XmlTextWriterState::XmlTextwriterPIText => {
            sum += (*writer).out.write_str("?>")?;
        }
        _ => {
            return Err(io::Error::other("Not PI"));
        }
    }

    if (*writer).indent != 0 {
        sum += (*writer).out.write_str("\n")?;
    }

    xml_list_pop_front((*writer).nodes);
    Ok(sum)
}

/// Write an xml PI.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWritePI")]
pub unsafe fn xml_text_writer_write_pi(
    writer: XmlTextWriterPtr,
    target: *const XmlChar,
    content: *const XmlChar,
) -> io::Result<usize> {
    let mut sum = 0;
    sum += xml_text_writer_start_pi(writer, target)?;
    if !content.is_null() {
        sum += xml_text_writer_write_string(writer, content)?;
    }
    sum += xml_text_writer_end_pi(writer)?;
    Ok(sum)
}

/// Start an xml CDATA section.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartCDATA")]
pub unsafe fn xml_text_writer_start_cdata(writer: XmlTextWriterPtr) -> io::Result<usize> {
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    let mut sum = 0;
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
                        sum += xml_text_writer_end_attribute(writer)?;
                    }

                    // Output namespace declarations
                    sum += xml_text_writer_output_nsdecl(writer)?;
                    sum += (*writer).out.write_str(">")?;
                    (*p).state = XmlTextWriterState::XmlTextwriterText;
                }
                XmlTextWriterState::XmlTextwriterCDATA => {
                    xml_writer_err_msg(
                        writer,
                        XmlParserErrors::XmlErrInternalError,
                        c"xmlTextWriterStartCDATA : CDATA not allowed in this context!\n".as_ptr()
                            as _,
                    );
                    return Err(io::Error::other(
                        "xmlTextWriterStartCDATA : CDATA not allowed in this context!",
                    ));
                }
                _ => {
                    return Err(io::Error::other("Invalid State"));
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
        return Err(io::Error::other("xmlTextWriterStartCDATA : out of memory!"));
    }

    (*p).name = null_mut();
    (*p).state = XmlTextWriterState::XmlTextwriterCDATA;

    xml_list_push_front((*writer).nodes, p as _);

    sum += (*writer).out.write_str("<![CDATA[")?;
    Ok(sum)
}

/// End an xml CDATA section.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterEndCDATA")]
pub unsafe fn xml_text_writer_end_cdata(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return Err(io::Error::other("Node list is NULL"));
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return Err(io::Error::other("Link data is NULL"));
    }

    let mut sum = 0;
    match (*p).state {
        XmlTextWriterState::XmlTextwriterCDATA => {
            sum += (*writer).out.write_str("]]>")?;
        }
        _ => {
            return Err(io::Error::other("Invalid state"));
        }
    }

    xml_list_pop_front((*writer).nodes);
    Ok(sum)
}

/// Write an xml CDATA.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteCDATA")]
pub unsafe fn xml_text_writer_write_cdata(
    writer: XmlTextWriterPtr,
    content: *const XmlChar,
) -> io::Result<usize> {
    let mut sum = 0;
    sum += xml_text_writer_start_cdata(writer)?;
    if !content.is_null() {
        sum += xml_text_writer_write_string(writer, content)?;
    }
    sum += xml_text_writer_end_cdata(writer)?;
    Ok(sum)
}

/// Start an xml DTD.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartDTD")]
pub unsafe fn xml_text_writer_start_dtd(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
) -> io::Result<usize> {
    if writer.is_null() || name.is_null() || *name == b'\0' {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() && !xml_link_get_data(lk).is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterStartDTD : DTD allowed only in prolog!\n".as_ptr() as _,
        );
        return Err(io::Error::other(
            "xmlTextWriterStartDTD : DTD allowed only in prolog!",
        ));
    }

    let p: *mut XmlTextWriterStackEntry =
        xml_malloc(size_of::<XmlTextWriterStackEntry>()) as *mut XmlTextWriterStackEntry;
    if p.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTD : out of memory!\n".as_ptr() as _,
        );
        return Err(io::Error::other("xmlTextWriterStartDTD : out of memory!"));
    }

    (*p).name = xml_strdup(name);
    if (*p).name.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTD : out of memory!\n".as_ptr() as _,
        );
        xml_free(p as _);
        return Err(io::Error::other("xmlTextWriterStartDTD : out of memory!"));
    }
    (*p).state = XmlTextWriterState::XmlTextwriterDTD;

    xml_list_push_front((*writer).nodes, p as _);

    let mut sum = (*writer).out.write_str("<!DOCTYPE ")?;
    sum += (*writer)
        .out
        .write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref())?;

    if !pubid.is_null() {
        if sysid.is_null() {
            xml_writer_err_msg(
                writer,
                XmlParserErrors::XmlErrInternalError,
                c"xmlTextWriterStartDTD : system identifier needed!\n".as_ptr() as _,
            );
            return Err(io::Error::other(
                "xmlTextWriterStartDTD : system identifier needed!",
            ));
        }

        if (*writer).indent != 0 {
            sum += (*writer).out.write_bytes(b"\n")?;
        } else {
            sum += (*writer).out.write_bytes(b" ")?;
        }

        sum += (*writer).out.write_str("PUBLIC ")?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(pubid as _).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
    }

    if !sysid.is_null() {
        if pubid.is_null() {
            if (*writer).indent != 0 {
                sum += (*writer).out.write_bytes(b"\n")?;
            } else {
                sum += (*writer).out.write_bytes(b" ")?;
            }
            sum += (*writer).out.write_str("SYSTEM ")?;
        } else if (*writer).indent != 0 {
            sum += (*writer).out.write_str("\n       ")?;
        } else {
            sum += (*writer).out.write_bytes(b" ")?;
        }

        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(sysid as _).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
    }

    Ok(sum)
}

/// End an xml DTD.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterEndDTD")]
pub unsafe fn xml_text_writer_end_dtd(writer: XmlTextWriterPtr) -> io::Result<usize> {
    let mut do_loop: i32;
    let mut lk: XmlLinkPtr;
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    let mut sum = 0;
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
            ty @ XmlTextWriterState::XmlTextwriterDTDText
            | ty @ XmlTextWriterState::XmlTextwriterDTD => {
                if matches!(ty, XmlTextWriterState::XmlTextwriterDTDText) {
                    sum += (*writer).out.write_str("]")?;
                }
                sum += (*writer).out.write_str(">")?;

                if (*writer).indent != 0 {
                    sum += (*writer).out.write_str("\n")?;
                }

                xml_list_pop_front((*writer).nodes);
            }
            XmlTextWriterState::XmlTextwriterDTDElem
            | XmlTextWriterState::XmlTextwriterDTDElemText => {
                sum += xml_text_writer_end_dtdelement(writer)?;
            }
            XmlTextWriterState::XmlTextwriterDTDAttl
            | XmlTextWriterState::XmlTextwriterDTDAttlText => {
                sum += xml_text_writer_end_dtd_attlist(writer)?;
            }
            XmlTextWriterState::XmlTextwriterDTDEnty
            | XmlTextWriterState::XmlTextwriterDTDPEnt
            | XmlTextWriterState::XmlTextwriterDTDEntyText => {
                sum += xml_text_writer_end_dtd_entity(writer)?;
            }
            XmlTextWriterState::XmlTextwriterComment => {
                sum += xml_text_writer_end_comment(writer)?;
            }
            _ => {
                do_loop = 0;
                continue;
            }
        }
    }

    Ok(sum)
}

/// Write a DTD.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteDTD")]
pub unsafe fn xml_text_writer_write_dtd(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
    subset: *const XmlChar,
) -> io::Result<usize> {
    let mut sum = xml_text_writer_start_dtd(writer, name, pubid, sysid)?;
    if !subset.is_null() {
        sum += xml_text_writer_write_string(writer, subset)?;
    }
    sum += xml_text_writer_end_dtd(writer)?;
    Ok(sum)
}

/// Start an xml DTD element.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartDTDElement")]
pub unsafe fn xml_text_writer_start_dtdelement(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
) -> io::Result<usize> {
    if writer.is_null() || name.is_null() || *name == b'\0' {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return Err(io::Error::other("Node list is NULL"));
    }

    let mut sum = 0;
    let mut p = xml_link_get_data(lk) as *mut XmlTextWriterStackEntry;
    if !p.is_null() {
        match (*p).state {
            XmlTextWriterState::XmlTextwriterDTD => {
                sum += (*writer).out.write_str(" [")?;
                if (*writer).indent != 0 {
                    sum += (*writer).out.write_str("\n")?;
                }
                (*p).state = XmlTextWriterState::XmlTextwriterDTDText;
            }
            XmlTextWriterState::XmlTextwriterDTDText | XmlTextWriterState::XmlTextwriterNone => {}
            _ => {
                return Err(io::Error::other("Invalid state"));
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
        return Err(io::Error::other(
            "xmlTextWriterStartDTDElement : out of memory!",
        ));
    }

    (*p).name = xml_strdup(name);
    if (*p).name.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTDElement : out of memory!\n".as_ptr() as _,
        );
        xml_free(p as _);
        return Err(io::Error::other(
            "xmlTextWriterStartDTDElement : out of memory!",
        ));
    }
    (*p).state = XmlTextWriterState::XmlTextwriterDTDElem;

    xml_list_push_front((*writer).nodes, p as _);

    if (*writer).indent != 0 {
        sum += xml_text_writer_write_indent(writer)?;
    }

    sum += (*writer).out.write_str("<!ELEMENT ")?;
    sum += (*writer)
        .out
        .write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref())?;
    Ok(sum)
}

/// End an xml DTD element.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterEndDTDElement")]
pub unsafe fn xml_text_writer_end_dtdelement(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return Err(io::Error::other("Node list is NULL"));
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return Err(io::Error::other("List data is NULL"));
    }

    let mut sum = 0;
    match (*p).state {
        XmlTextWriterState::XmlTextwriterDTDElem | XmlTextWriterState::XmlTextwriterDTDElemText => {
            sum += (*writer).out.write_str(">")?;
        }
        _ => {
            return Err(io::Error::other("Invalid state"));
        }
    }

    if (*writer).indent != 0 {
        sum += (*writer).out.write_str("\n")?;
    }

    xml_list_pop_front((*writer).nodes);
    Ok(sum)
}

/// Write a DTD element.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteDTDElement")]
pub unsafe fn xml_text_writer_write_dtdelement(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> io::Result<usize> {
    if content.is_null() {
        return Err(io::Error::other("Content is NULL"));
    }

    let mut sum = xml_text_writer_start_dtdelement(writer, name)?;
    sum += xml_text_writer_write_string(writer, content)?;
    sum += xml_text_writer_end_dtdelement(writer)?;
    Ok(sum)
}

/// Start an xml DTD ATTLIST.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartDTDAttlist")]
pub unsafe fn xml_text_writer_start_dtdattlist(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
) -> io::Result<usize> {
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || name.is_null() || *name == b'\0' {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return Err(io::Error::other("Node list is NULL"));
    }

    let mut sum = 0;
    p = xml_link_get_data(lk) as _;
    if !p.is_null() {
        match (*p).state {
            XmlTextWriterState::XmlTextwriterDTD => {
                sum += (*writer).out.write_str(" [")?;
                if (*writer).indent != 0 {
                    sum += (*writer).out.write_str("\n")?;
                }
                (*p).state = XmlTextWriterState::XmlTextwriterDTDText;
            }
            XmlTextWriterState::XmlTextwriterDTDText | XmlTextWriterState::XmlTextwriterNone => {}
            _ => {
                return Err(io::Error::other("Invalid state"));
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
        return Err(io::Error::other(
            "xmlTextWriterStartDTDAttlist : out of memory!",
        ));
    }

    (*p).name = xml_strdup(name);
    if (*p).name.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTDAttlist : out of memory!\n".as_ptr() as _,
        );
        xml_free(p as _);
        return Err(io::Error::other(
            "xmlTextWriterStartDTDAttlist : out of memory!",
        ));
    }
    (*p).state = XmlTextWriterState::XmlTextwriterDTDAttl;

    xml_list_push_front((*writer).nodes, p as _);

    if (*writer).indent != 0 {
        sum += xml_text_writer_write_indent(writer)?;
    }

    sum += (*writer).out.write_str("<!ATTLIST ")?;
    sum += (*writer)
        .out
        .write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref())?;
    Ok(sum)
}

/// End an xml DTD attribute list.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterEndDTDAttlist")]
pub unsafe fn xml_text_writer_end_dtd_attlist(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return Err(io::Error::other("Node list is NULL"));
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return Err(io::Error::other("List data is NULL"));
    }

    let mut sum = 0;
    match (*p).state {
        XmlTextWriterState::XmlTextwriterDTDAttl | XmlTextWriterState::XmlTextwriterDTDAttlText => {
            sum += (*writer).out.write_str(">")?;
        }
        _ => {
            return Err(io::Error::other("Invalid state"));
        }
    }

    if (*writer).indent != 0 {
        sum += (*writer).out.write_str("\n")?;
    }

    xml_list_pop_front((*writer).nodes);
    Ok(sum)
}

/// Write a DTD ATTLIST.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteDTDAttlist")]
pub unsafe fn xml_text_writer_write_dtd_attlist(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    content: *const XmlChar,
) -> io::Result<usize> {
    if content.is_null() {
        return Err(io::Error::other("Content is NULL"));
    }

    let mut sum = xml_text_writer_start_dtdattlist(writer, name)?;
    sum += xml_text_writer_write_string(writer, content)?;
    sum += xml_text_writer_end_dtd_attlist(writer)?;
    Ok(sum)
}

/// Start an xml DTD ATTLIST.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartDTDEntity")]
pub unsafe fn xml_text_writer_start_dtd_entity(
    writer: XmlTextWriterPtr,
    pe: i32,
    name: *const XmlChar,
) -> io::Result<usize> {
    let mut p: *mut XmlTextWriterStackEntry;

    if writer.is_null() || name.is_null() || *name == b'\0' {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let mut sum = 0;
    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if !lk.is_null() {
        p = xml_link_get_data(lk) as _;
        if !p.is_null() {
            match (*p).state {
                XmlTextWriterState::XmlTextwriterDTD => {
                    sum += (*writer).out.write_str(" [")?;
                    if (*writer).indent != 0 {
                        sum += (*writer).out.write_str("\n")?;
                    }
                    (*p).state = XmlTextWriterState::XmlTextwriterDTDText;
                }
                _ty @ XmlTextWriterState::XmlTextwriterDTDText
                | _ty @ XmlTextWriterState::XmlTextwriterNone => {}
                _ => {
                    return Err(io::Error::other("Invalid state"));
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
        return Err(io::Error::other(
            "xmlTextWriterStartDTDElement : out of memory!",
        ));
    }

    (*p).name = xml_strdup(name);
    if (*p).name.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrNoMemory,
            c"xmlTextWriterStartDTDElement : out of memory!\n".as_ptr() as _,
        );
        xml_free(p as _);
        return Err(io::Error::other(
            "xmlTextWriterStartDTDElement : out of memory!",
        ));
    }

    if pe != 0 {
        (*p).state = XmlTextWriterState::XmlTextwriterDTDPEnt;
    } else {
        (*p).state = XmlTextWriterState::XmlTextwriterDTDEnty;
    }

    xml_list_push_front((*writer).nodes, p as _);

    if (*writer).indent != 0 {
        sum += xml_text_writer_write_indent(writer)?;
    }
    sum += (*writer).out.write_str("<!ENTITY ")?;
    if pe != 0 {
        sum += (*writer).out.write_str("% ")?;
    }
    sum += (*writer)
        .out
        .write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref())?;
    Ok(sum)
}

/// End an xml DTD entity.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterEndDTDEntity")]
pub unsafe fn xml_text_writer_end_dtd_entity(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return Err(io::Error::other("Node list is NULL"));
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return Err(io::Error::other("List data is NULL"));
    }

    let mut sum = 0;
    match (*p).state {
        ty @ XmlTextWriterState::XmlTextwriterDTDEntyText
        | ty @ XmlTextWriterState::XmlTextwriterDTDEnty
        | ty @ XmlTextWriterState::XmlTextwriterDTDPEnt => {
            if matches!(ty, XmlTextWriterState::XmlTextwriterDTDEntyText) {
                sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
            }
            sum += (*writer).out.write_str(">")?;
        }
        _ => {
            return Err(io::Error::other("Invalid state"));
        }
    }

    if (*writer).indent != 0 {
        sum += (*writer).out.write_str("\n")?;
    }

    xml_list_pop_front((*writer).nodes);
    Ok(sum)
}

/// Write a DTD internal entity.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteDTDInternalEntity")]
pub unsafe fn xml_text_writer_write_dtd_internal_entity(
    writer: XmlTextWriterPtr,
    pe: i32,
    name: *const XmlChar,
    content: *const XmlChar,
) -> io::Result<usize> {
    if name.is_null() || *name == b'\0' || content.is_null() {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let mut sum = xml_text_writer_start_dtd_entity(writer, pe, name)?;
    sum += xml_text_writer_write_string(writer, content)?;
    sum += xml_text_writer_end_dtd_entity(writer)?;
    Ok(sum)
}

/// Write a DTD external entity. The entity must have been started with xmlTextWriterStartDTDEntity
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteDTDExternalEntity")]
pub unsafe fn xml_text_writer_write_dtd_external_entity(
    writer: XmlTextWriterPtr,
    pe: i32,
    name: *const XmlChar,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
    ndataid: *const XmlChar,
) -> io::Result<usize> {
    if pubid.is_null() && sysid.is_null() {
        return Err(io::Error::other("Both ExternalID and PublicID is NULL"));
    }
    if pe != 0 && !ndataid.is_null() {
        return Err(io::Error::other("This is PE, but NDATAID is NULL"));
    }

    let mut sum = xml_text_writer_start_dtd_entity(writer, pe, name)?;
    sum += xml_text_writer_write_dtd_external_entity_contents(writer, pubid, sysid, ndataid)?;
    sum += xml_text_writer_end_dtd_entity(writer)?;
    Ok(sum)
}

/// Write the contents of a DTD external entity.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteDTDExternalEntityContents")]
pub unsafe fn xml_text_writer_write_dtd_external_entity_contents(
    writer: XmlTextWriterPtr,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
    ndataid: *const XmlChar,
) -> io::Result<usize> {
    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            c"xmlTextWriterWriteDTDExternalEntityContents: xmlTextWriterPtr invalid!\n".as_ptr()
                as _,
        );
        return Err(io::Error::other(
            "xmlTextWriterWriteDTDExternalEntityContents: xmlTextWriterPtr invalid!",
        ));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        xml_writer_err_msg(writer, XmlParserErrors::XmlErrInternalError,
                        c"xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!\n".as_ptr() as _);
        return Err(io::Error::other(
            "xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!",
        ));
    }

    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if p.is_null() {
        return Err(io::Error::other("Link data is NULL"));
    }

    match (*p).state {
        XmlTextWriterState::XmlTextwriterDTDEnty => {}
        XmlTextWriterState::XmlTextwriterDTDPEnt => {
            if !ndataid.is_null() {
                xml_writer_err_msg(writer, XmlParserErrors::XmlErrInternalError,
                                c"xmlTextWriterWriteDTDExternalEntityContents: notation not allowed with parameter entities!\n".as_ptr() as _);
                return Err(io::Error::other("xmlTextWriterWriteDTDExternalEntityContents: notation not allowed with parameter entities!"));
            }
        }
        _ => {
            xml_writer_err_msg(writer, XmlParserErrors::XmlErrInternalError,
                            c"xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!\n".as_ptr() as _);
            return Err(io::Error::other("xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!"));
        }
    }

    let mut sum = 0;
    if !pubid.is_null() {
        if sysid.is_null() {
            xml_writer_err_msg(
                writer,
                XmlParserErrors::XmlErrInternalError,
                c"xmlTextWriterWriteDTDExternalEntityContents: system identifier needed!\n".as_ptr()
                    as _,
            );
            return Err(io::Error::other(
                "xmlTextWriterWriteDTDExternalEntityContents: system identifier needed!",
            ));
        }

        sum += (*writer).out.write_str(" PUBLIC ")?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(pubid as _).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
    }

    if !sysid.is_null() {
        if pubid.is_null() {
            sum += (*writer).out.write_str(" SYSTEM")?;
        }
        sum += (*writer).out.write_str(" ")?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(sysid as _).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
    }

    if !ndataid.is_null() {
        sum += (*writer).out.write_str(" NDATA ")?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(ndataid as _).to_string_lossy().as_ref())?;
    }
    Ok(sum)
}

/// Write a DTD entity.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteDTDEntity")]
pub unsafe fn xml_text_writer_write_dtd_entity(
    writer: XmlTextWriterPtr,
    pe: i32,
    name: *const XmlChar,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
    ndataid: *const XmlChar,
    content: *const XmlChar,
) -> io::Result<usize> {
    if content.is_null() && pubid.is_null() && sysid.is_null() {
        return Err(io::Error::other("Content, PublicID and SystemID are NULL"));
    }
    if pe != 0 && !ndataid.is_null() {
        return Err(io::Error::other("This is PE, but NDATAID is not NULL"));
    }

    if pubid.is_null() && sysid.is_null() {
        return xml_text_writer_write_dtd_internal_entity(writer, pe, name, content);
    }

    xml_text_writer_write_dtd_external_entity(writer, pe, name, pubid, sysid, ndataid)
}

/// Write a DTD entity.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteDTDNotation")]
pub unsafe fn xml_text_writer_write_dtd_notation(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    pubid: *const XmlChar,
    sysid: *const XmlChar,
) -> io::Result<usize> {
    if writer.is_null() || name.is_null() || *name == b'\0' {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let lk: XmlLinkPtr = xml_list_front((*writer).nodes);
    if lk.is_null() {
        return Err(io::Error::other("Node list is NULL"));
    }

    let mut sum = 0;
    let p: *mut XmlTextWriterStackEntry = xml_link_get_data(lk) as _;
    if !p.is_null() {
        match (*p).state {
            XmlTextWriterState::XmlTextwriterDTD => {
                sum += (*writer).out.write_str(" [")?;
                if (*writer).indent != 0 {
                    sum += (*writer).out.write_str("\n")?;
                }
                (*p).state = XmlTextWriterState::XmlTextwriterDTDText;
            }
            XmlTextWriterState::XmlTextwriterDTDText => {}
            _ => {
                return Err(io::Error::other("Invalid state"));
            }
        }
    }

    if (*writer).indent != 0 {
        sum += xml_text_writer_write_indent(writer)?;
    }

    sum += (*writer).out.write_str("<!NOTATION ")?;
    sum += (*writer)
        .out
        .write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref())?;

    if !pubid.is_null() {
        sum += (*writer).out.write_str(" PUBLIC ")?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(pubid as _).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
    }

    if !sysid.is_null() {
        if pubid.is_null() {
            sum += (*writer).out.write_str(" SYSTEM")?;
        }
        sum += (*writer).out.write_str(" ")?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(sysid as _).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar as u8])?;
    }

    sum += (*writer).out.write_str(">")?;
    Ok(sum)
}

/// Set indentation output. indent = 0 do not indentation. indent > 0 do indentation.
///
/// Returns -1 on error or 0 otherwise.
#[doc(alias = "xmlTextWriterSetIndent")]
pub unsafe extern "C" fn xml_text_writer_set_indent(writer: XmlTextWriterPtr, indent: i32) -> i32 {
    if writer.is_null() || indent < 0 {
        return -1;
    }

    (*writer).indent = indent;
    (*writer).doindent = 1;

    0
}

/// Set string indentation.
///
/// Returns -1 on error or 0 otherwise.
#[doc(alias = "xmlTextWriterSetIndentString")]
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

/// Set the character used for quoting attributes.
///
/// Returns -1 on error or 0 otherwise.
#[doc(alias = "xmlTextWriterSetQuoteChar")]
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

/// Flush the output buffer.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterFlush")]
pub unsafe extern "C" fn xml_text_writer_flush(writer: XmlTextWriterPtr) -> i32 {
    if writer.is_null() {
        return -1;
    }

    (*writer).out.flush()
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_new_text_writer_push_parser() {
        #[cfg(feature = "libxml_writer")]
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
        #[cfg(feature = "libxml_writer")]
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_end_attribute(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_end_cdata(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_end_comment(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_end_dtd(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_end_dtd_attlist(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_end_dtdelement(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_end_dtd_entity(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_end_document(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_end_element(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_end_pi(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_flush(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_full_end_element(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_indent in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let indent = gen_int(n_indent, 1);

                    xml_text_writer_set_indent(writer, indent);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let str = gen_const_xml_char_ptr(n_str, 1);

                    xml_text_writer_set_indent_string(writer, str);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_quotechar in 0..GEN_NB_XML_CHAR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let quotechar = gen_xml_char(n_quotechar, 1);

                    xml_text_writer_set_quote_char(writer, quotechar);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    xml_text_writer_start_attribute(writer, name);
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
        #[cfg(feature = "libxml_writer")]
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

                            xml_text_writer_start_attribute_ns(writer, prefix, name, namespace_uri);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_start_cdata(writer);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                let mem_base = xml_mem_blocks();
                let writer = gen_xml_text_writer_ptr(n_writer, 0);

                xml_text_writer_start_comment(writer);
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
        #[cfg(feature = "libxml_writer")]
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

                            xml_text_writer_start_dtd(writer, name, pubid, sysid);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    xml_text_writer_start_dtdattlist(writer, name);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    xml_text_writer_start_dtdelement(writer, name);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_pe in 0..GEN_NB_INT {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let pe = gen_int(n_pe, 1);
                        let name = gen_const_xml_char_ptr(n_name, 2);

                        xml_text_writer_start_dtd_entity(writer, pe, name);
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
        #[cfg(feature = "libxml_writer")]
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

                            xml_text_writer_start_document(writer, version, encoding, standalone);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    xml_text_writer_start_element(writer, name);
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
        #[cfg(feature = "libxml_writer")]
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

                            xml_text_writer_start_element_ns(writer, prefix, name, namespace_uri);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_target in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let target = gen_const_xml_char_ptr(n_target, 1);

                    xml_text_writer_start_pi(writer, target);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let content = gen_const_xml_char_ptr(n_content, 2);

                        xml_text_writer_write_attribute(writer, name, content);
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
        #[cfg(feature = "libxml_writer")]
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

                                xml_text_writer_write_attribute_ns(
                                    writer,
                                    prefix,
                                    name,
                                    namespace_uri,
                                    content,
                                );
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
        #[cfg(feature = "libxml_writer")]
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

                            xml_text_writer_write_base64(writer, data, start, len);
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
        #[cfg(feature = "libxml_writer")]
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

                            xml_text_writer_write_bin_hex(writer, data, start, len);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    xml_text_writer_write_cdata(writer, content);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    xml_text_writer_write_comment(writer, content);
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
        #[cfg(feature = "libxml_writer")]
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

                                xml_text_writer_write_dtd(writer, name, pubid, sysid, subset);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let content = gen_const_xml_char_ptr(n_content, 2);

                        xml_text_writer_write_dtd_attlist(writer, name, content);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let content = gen_const_xml_char_ptr(n_content, 2);

                        xml_text_writer_write_dtdelement(writer, name, content);
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
        #[cfg(feature = "libxml_writer")]
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

                                        xml_text_writer_write_dtd_entity(
                                            writer, pe, name, pubid, sysid, ndataid, content,
                                        );
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
        #[cfg(feature = "libxml_writer")]
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

                                    xml_text_writer_write_dtd_external_entity(
                                        writer, pe, name, pubid, sysid, ndataid,
                                    );
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
        #[cfg(feature = "libxml_writer")]
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

                            xml_text_writer_write_dtd_external_entity_contents(
                                writer, pubid, sysid, ndataid,
                            );
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
        #[cfg(feature = "libxml_writer")]
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

                            xml_text_writer_write_dtd_internal_entity(writer, pe, name, content);
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
        #[cfg(feature = "libxml_writer")]
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

                            xml_text_writer_write_dtd_notation(writer, name, pubid, sysid);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let content = gen_const_xml_char_ptr(n_content, 2);

                        xml_text_writer_write_element(writer, name, content);
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
        #[cfg(feature = "libxml_writer")]
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

                                xml_text_writer_write_element_ns(
                                    writer,
                                    prefix,
                                    name,
                                    namespace_uri,
                                    content,
                                );
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_target in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let writer = gen_xml_text_writer_ptr(n_writer, 0);
                        let target = gen_const_xml_char_ptr(n_target, 1);
                        let content = gen_const_xml_char_ptr(n_content, 2);

                        xml_text_writer_write_pi(writer, target, content);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    xml_text_writer_write_raw(writer, content);
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
        #[cfg(feature = "libxml_writer")]
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

                        xml_text_writer_write_raw_len(writer, content, len);
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
        #[cfg(feature = "libxml_writer")]
        unsafe {
            let mut leaks = 0;

            for n_writer in 0..GEN_NB_XML_TEXT_WRITER_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let writer = gen_xml_text_writer_ptr(n_writer, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    xml_text_writer_write_string(writer, content);
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

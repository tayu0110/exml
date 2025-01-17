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
    cell::{Cell, RefCell},
    collections::VecDeque,
    ffi::{c_char, CStr},
    io::{self, Write},
    mem::size_of,
    os::raw::c_void,
    ptr::{drop_in_place, null_mut},
    rc::Rc,
    slice::from_raw_parts,
};

use crate::{
    buf::XmlBufRef,
    encoding::find_encoding_handler,
    error::{XmlParserErrors, __xml_raise_error},
    globals::GenericErrorContext,
    io::XmlOutputBuffer,
    libxml::{
        globals::{xml_free, xml_malloc},
        htmltree::html_new_doc_no_dtd,
        parser::{
            xml_create_push_parser_ctxt, xml_parse_chunk, XmlParserInputState, XmlSAXHandler,
            XML_DEFAULT_VERSION,
        },
        sax2::{xml_sax2_end_element, xml_sax2_init_default_sax_handler, xml_sax2_start_element},
        xmlstring::{xml_strcasecmp, xml_strdup, xml_strlen, XmlChar},
    },
    list::XmlList,
    parser::{xml_free_parser_ctxt, XmlParserCtxtPtr},
    tree::{xml_encode_special_chars, xml_free_doc, xml_new_doc, XmlDocPtr, XmlNodePtr},
    uri::canonic_path,
};

use super::{xmlsave::xml_buf_attr_serialize_txt_content, xmlstring::xml_strndup};

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
    name: Option<String>,
    state: Cell<XmlTextWriterState>,
}

#[repr(C)]
struct XmlTextWriterNsStackEntry {
    prefix: String,
    uri: String,
    elem: Option<Rc<XmlTextWriterStackEntry>>,
}

pub type XmlTextWriterPtr<'a> = *mut XmlTextWriter<'a>;
#[repr(C)]
pub struct XmlTextWriter<'a> {
    out: XmlOutputBuffer<'a>,                     /* output buffer */
    nodes: VecDeque<Rc<XmlTextWriterStackEntry>>, /* element name stack */
    nsstack: XmlList<XmlTextWriterNsStackEntry>,  /* name spaces stack */
    level: i32,
    indent: i32,         /* enable indent */
    doindent: i32,       /* internal indent flag */
    ichar: *mut XmlChar, /* indent character */
    qchar: u8,           /* character used for quoting attribute values */
    ctxt: XmlParserCtxtPtr,
    no_doc_free: i32,
    doc: XmlDocPtr,
}

impl XmlTextWriter<'_> {
    /// Start a new xml document
    ///
    /// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
    #[doc(alias = "xmlTextWriterStartDocument")]
    pub unsafe fn start_document(
        &mut self,
        version: Option<&str>,
        encoding: Option<&str>,
        standalone: Option<&str>,
    ) -> io::Result<usize> {
        if self.nodes.front().is_some() {
            xml_writer_err_msg(
                self,
                XmlParserErrors::XmlErrInternalError,
                "xmlTextWriterStartDocument : not allowed in this context!\n",
            );
            return Err(io::Error::other(
                "xmlTextWriterStartDocument : not allowed in this context!",
            ));
        }

        let encoder = if let Some(encoding) = encoding {
            let Some(encoder) = find_encoding_handler(encoding) else {
                xml_writer_err_msg(
                    self,
                    XmlParserErrors::XmlErrUnsupportedEncoding,
                    "xmlTextWriterStartDocument : unsupported encoding\n",
                );
                return Err(io::Error::other(
                    "xmlTextWriterStartDocument : unsupported encoding",
                ));
            };
            Some(encoder)
        } else {
            None
        };

        self.out.encoder = encoder.map(|e| Rc::new(RefCell::new(e)));
        if self.out.encoder.is_some() {
            if self.out.conv.is_none() {
                self.out.conv = XmlBufRef::with_capacity(4000);
            }
            self.out.encode(true);
            if !self.doc.is_null() && (*self.doc).encoding.is_none() {
                let encoder = self.out.encoder.as_ref().unwrap().borrow();
                (*self.doc).encoding = Some(encoder.name().to_owned());
            }
        } else {
            self.out.conv = None;
        }

        let mut sum = self.out.write_str("<?xml version=")?;
        sum += self.out.write_bytes(&[self.qchar])?;
        sum += self.out.write_str(version.unwrap_or("1.0"))?;
        sum += self.out.write_bytes(&[self.qchar])?;
        if self.out.encoder.is_some() {
            sum += self.out.write_str(" encoding=")?;
            sum += self.out.write_bytes(&[self.qchar])?;
            let name = self
                .out
                .encoder
                .as_ref()
                .unwrap()
                .borrow()
                .name()
                .to_owned();
            sum += self.out.write_str(&name)?;
            sum += self.out.write_bytes(&[self.qchar])?;
        }

        if let Some(standalone) = standalone {
            sum += self.out.write_str(" standalone=")?;
            sum += self.out.write_bytes(&[self.qchar])?;
            sum += self.out.write_str(standalone)?;
            sum += self.out.write_bytes(&[self.qchar])?;
        }

        sum += self.out.write_str("?>\n")?;
        Ok(sum)
    }
}

impl Default for XmlTextWriter<'_> {
    fn default() -> Self {
        Self {
            out: XmlOutputBuffer::default(),
            nodes: VecDeque::new(),
            nsstack: XmlList::new(None, Rc::new(|_, _| std::cmp::Ordering::Equal)),
            level: 0,
            indent: 0,
            doindent: 0,
            ichar: null_mut(),
            qchar: b'"',
            ctxt: null_mut(),
            no_doc_free: 0,
            doc: null_mut(),
        }
    }
}

/// Handle a writer error
#[doc(alias = "xmlWriterErrMsg")]
unsafe fn xml_writer_err_msg(ctxt: XmlTextWriterPtr, error: XmlParserErrors, msg: &str) {
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
            None,
            0,
            None,
            None,
            None,
            0,
            0,
            msg,
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
            None,
            0,
            None,
            None,
            None,
            0,
            0,
            msg,
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
            "xmlNewTextWriter : out of memory!\n",
        );
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlTextWriter::default());

    (*ret).nsstack = XmlList::new(
        None,
        Rc::new(|d1, d2| {
            if std::ptr::eq(d1, d2) {
                return std::cmp::Ordering::Equal;
            }

            let mut rc = d1.prefix.cmp(&d2.prefix);

            if !rc.is_eq() {
                rc = std::cmp::Ordering::Less;
            }
            match (d1.elem.as_ref(), d2.elem.as_ref()) {
                (Some(d1), Some(d2)) => {
                    if !Rc::ptr_eq(d1, d2) {
                        rc = std::cmp::Ordering::Less;
                    }
                }
                (None, None) => {}
                _ => rc = std::cmp::Ordering::Less,
            }
            rc
        }),
    );

    (*ret).out = out;
    (*ret).ichar = xml_strdup(c" ".as_ptr() as _);
    (*ret).qchar = b'"';

    if (*ret).ichar.is_null() {
        drop_in_place(ret);
        xml_free(ret as _);
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrNoMemory,
            "xmlNewTextWriter : out of memory!\n",
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
            "xmlNewTextWriterFilename : cannot open uri\n",
        );
        return null_mut();
    };

    let ret: XmlTextWriterPtr = xml_new_text_writer(out);
    if ret.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrNoMemory,
            "xmlNewTextWriterFilename : out of memory!\n",
        );
        return null_mut();
    }

    (*ret).indent = 0;
    (*ret).doindent = 0;
    ret
}

/// Handle a writer error
#[doc(alias = "xmlWriterErrMsgInt")]
macro_rules! xml_writer_err_msg_int {
    ($ctxt:expr, $error:expr, $msg:literal, $val:expr) => {
        let ctxt = $ctxt as *mut XmlTextWriter;
        __xml_raise_error!(
            None,
            None,
            None,
            if !ctxt.is_null() {
                (*ctxt).ctxt as _
            } else {
                null_mut()
            },
            null_mut(),
            XmlErrorDomain::XmlFromWriter,
            $error,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            None,
            None,
            None,
            $val,
            0,
            format!($msg, $val).as_str(),
        );
    };
}

/// Write callback for the xmlOutputBuffer with target xmlBuffer
///
/// Returns -1, 0, 1
#[doc(alias = "xmlTextWriterWriteDocCallback")]
unsafe fn xml_text_writer_write_doc_callback(
    context: *mut c_void,
    str: *const c_char,
    len: i32,
) -> i32 {
    let ctxt: XmlParserCtxtPtr = context as XmlParserCtxtPtr;

    let rc = xml_parse_chunk(ctxt, str, len, 0);
    if rc != 0 {
        xml_writer_err_msg_int!(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            "xmlTextWriterWriteDocCallback : XML error {} !\n",
            rc
        );
        return -1;
    }

    len
}

/// Close callback for the xmlOutputBuffer with target xmlBuffer
///
/// Returns -1, 0, 1
#[doc(alias = "xmlTextWriterCloseDocCallback")]
unsafe fn xml_text_writer_close_doc_callback(context: *mut c_void) -> i32 {
    let ctxt: XmlParserCtxtPtr = context as XmlParserCtxtPtr;
    let rc: i32;

    let res = {
        rc = xml_parse_chunk(ctxt, null_mut(), 0, 1);
        rc != 0
    };
    if res {
        xml_writer_err_msg_int!(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            "xmlTextWriterCloseDocCallback : XML error {} !\n",
            rc
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
pub unsafe fn xml_new_text_writer_push_parser(
    ctxt: XmlParserCtxtPtr,
    _compression: i32,
) -> XmlTextWriterPtr<'static> {
    if ctxt.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            "xmlNewTextWriterPushParser : invalid context!\n",
        );
        return null_mut();
    }

    let Some(out) = XmlOutputBuffer::from_writer(TextWriterPushContext { context: ctxt }, None)
    else {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            "xmlNewTextWriterPushParser : error at xmlOutputBufferCreateIO!\n",
        );
        return null_mut();
    };

    let ret: XmlTextWriterPtr = xml_new_text_writer(out);
    if ret.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            "xmlNewTextWriterPushParser : error at xmlNewTextWriter!\n",
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
                if let Some(error) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.error) {
                    error(
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
            if let Some(error) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.error) {
                error(
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
        let url = canonic_path((*(*ctxt).input).filename.as_deref().unwrap());
        (*(*ctxt).my_doc).url = Some(url.into_owned());
        if (*(*ctxt).my_doc).url.is_none() {
            (*(*ctxt).my_doc).url = (*(*ctxt).input).filename.clone()
        }
    }
}

/// Create a new xmlNewTextWriter structure with @*doc as output
///
/// Returns the new xmlTextWriterPtr or NULL in case of error
#[doc(alias = "xmlNewTextWriterDoc")]
pub unsafe fn xml_new_text_writer_doc(
    doc: *mut XmlDocPtr,
    compression: i32,
) -> XmlTextWriterPtr<'static> {
    let mut sax_handler = XmlSAXHandler::default();
    xml_sax2_init_default_sax_handler(&mut sax_handler, 1);
    sax_handler.start_document = Some(xml_text_writer_start_document_callback);
    sax_handler.start_element = Some(xml_sax2_start_element);
    sax_handler.end_element = Some(xml_sax2_end_element);

    let ctxt: XmlParserCtxtPtr =
        xml_create_push_parser_ctxt(Some(Box::new(sax_handler)), None, null_mut(), 0, None);
    if ctxt.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            "xmlNewTextWriterDoc : error at xmlCreatePushParserCtxt!\n",
        );
        return null_mut();
    }
    // For some reason this seems to completely break if node names are interned.
    (*ctxt).dict_names = 0;

    (*ctxt).my_doc = xml_new_doc(Some(XML_DEFAULT_VERSION));
    if (*ctxt).my_doc.is_null() {
        xml_free_parser_ctxt(ctxt);
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            "xmlNewTextWriterDoc : error at xmlNewDoc!\n",
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
            "xmlNewTextWriterDoc : error at xmlNewTextWriterPushParser!\n",
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
pub unsafe fn xml_new_text_writer_tree(
    doc: XmlDocPtr,
    node: XmlNodePtr,
    compression: i32,
) -> XmlTextWriterPtr<'static> {
    if doc.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            "xmlNewTextWriterTree : invalid document tree!\n",
        );
        return null_mut();
    }

    let mut sax_handler = XmlSAXHandler::default();
    xml_sax2_init_default_sax_handler(&mut sax_handler, 1);
    sax_handler.start_document = Some(xml_text_writer_start_document_callback);
    sax_handler.start_element = Some(xml_sax2_start_element);
    sax_handler.end_element = Some(xml_sax2_end_element);

    let ctxt: XmlParserCtxtPtr =
        xml_create_push_parser_ctxt(Some(Box::new(sax_handler)), None, null_mut(), 0, None);
    if ctxt.is_null() {
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            "xmlNewTextWriterDoc : error at xmlCreatePushParserCtxt!\n",
        );
        return null_mut();
    }
    // For some reason this seems to completely break if node names are interned.
    (*ctxt).dict_names = 0;

    let ret: XmlTextWriterPtr = xml_new_text_writer_push_parser(ctxt, compression);
    if ret.is_null() {
        xml_free_parser_ctxt(ctxt);
        xml_writer_err_msg(
            null_mut(),
            XmlParserErrors::XmlErrInternalError,
            "xmlNewTextWriterDoc : error at xmlNewTextWriterPushParser!\n",
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
pub unsafe fn xml_free_text_writer(writer: XmlTextWriterPtr) {
    if writer.is_null() {
        return;
    }

    (*writer).out.flush();
    (*writer).out = XmlOutputBuffer::default();

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
    drop_in_place(writer);
    xml_free(writer as _);
}

/// End an xml document. All open elements are closed, and
/// the content is flushed to the output.
///
/// Returns the bytes written or -1 in case of error
#[doc(alias = "xmlTextWriterEndDocument")]
pub unsafe fn xml_text_writer_end_document(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            "xmlTextWriterEndDocument : invalid writer!\n",
        );
        return Err(io::Error::other(
            "xmlTextWriterEndDocument : invalid writer!",
        ));
    }

    let mut sum = 0;
    while let Some(lk) = (*writer).nodes.front() {
        match lk.state.get() {
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
    let mut sum = 0;
    while let Some(lk) = (*writer).nsstack.pop_first() {
        let count = xml_text_writer_write_attribute(writer, &lk.prefix, &lk.uri);

        if count.is_err() {
            (*writer).nsstack.clear();
            return count;
        }
        sum += count.unwrap();
    }
    Ok(sum)
}

/// Write indent string.
///
/// Returns -1 on error or the number of strings written.
#[doc(alias = "xmlTextWriterWriteIndent")]
unsafe fn xml_text_writer_write_indent(writer: XmlTextWriterPtr) -> io::Result<usize> {
    let lksize = (*writer).nodes.len();
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

    Ok(lksize - 1)
}

/// Start an xml comment.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartComment")]
pub unsafe fn xml_text_writer_start_comment(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            "xmlTextWriterStartComment : invalid writer!\n",
        );
        return Err(io::Error::other(
            "xmlTextWriterStartComment : invalid writer!",
        ));
    }

    let mut sum = 0;
    if let Some(lk) = (*writer).nodes.front_mut() {
        match lk.state.get() {
            XmlTextWriterState::XmlTextwriterText | XmlTextWriterState::XmlTextwriterNone => {}
            XmlTextWriterState::XmlTextwriterName => {
                // Output namespace declarations
                sum += xml_text_writer_output_nsdecl(writer)?;
                sum += (*writer).out.write_str(">")?;
                if (*writer).indent != 0 {
                    sum += (*writer).out.write_str("\n")?;
                }
                lk.state.set(XmlTextWriterState::XmlTextwriterText);
            }
            _ => {
                return Err(io::Error::other("Invalid state"));
            }
        }
    }

    (*writer).nodes.push_front(
        XmlTextWriterStackEntry {
            name: None,
            state: Cell::new(XmlTextWriterState::XmlTextwriterComment),
        }
        .into(),
    );

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
            "xmlTextWriterEndComment : invalid writer!\n",
        );
        return Err(io::Error::other(
            "xmlTextWriterEndComment : invalid writer!",
        ));
    }

    let Some(lk) = (*writer).nodes.front() else {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            "xmlTextWriterEndComment : not allowed in this context!\n",
        );
        return Err(io::Error::other(
            "xmlTextWriterEndComment : not allowed in this context!",
        ));
    };

    let mut sum = 0;
    match lk.state.get() {
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

    (*writer).nodes.pop_front();
    Ok(sum)
}

/// Write an xml comment.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteComment")]
pub unsafe fn xml_text_writer_write_comment(
    writer: XmlTextWriterPtr,
    content: &str,
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
    name: &str,
) -> io::Result<usize> {
    if writer.is_null() || name.is_empty() {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let mut sum = 0;
    if let Some(lk) = (*writer).nodes.front_mut() {
        match lk.state.get() {
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
                lk.state.set(XmlTextWriterState::XmlTextwriterText);
            }
            _ => {}
        }
    }

    let p = XmlTextWriterStackEntry {
        name: Some(name.to_owned()),
        state: Cell::new(XmlTextWriterState::XmlTextwriterName),
    };
    (*writer).nodes.push_front(p.into());

    if (*writer).indent != 0 {
        sum += xml_text_writer_write_indent(writer)?;
    }

    sum += (*writer).out.write_str("<")?;
    sum += (*writer).out.write_str(name)?;
    Ok(sum)
}

/// Start an xml element with namespace support.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterStartElementNS")]
pub unsafe fn xml_text_writer_start_element_ns(
    writer: XmlTextWriterPtr,
    prefix: Option<&str>,
    name: &str,
    namespace_uri: Option<&str>,
) -> io::Result<usize> {
    if writer.is_null() || name.is_empty() {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let mut buf = String::new();
    if let Some(prefix) = prefix {
        buf.push_str(prefix);
        buf.push(':');
    }
    buf.push_str(name);

    let sum = xml_text_writer_start_element(writer, &buf)?;

    if let Some(namespace_uri) = namespace_uri {
        let mut buf = "xmlns".to_owned();
        if let Some(prefix) = prefix {
            buf.push(':');
            buf.push_str(prefix);
        }

        let p = XmlTextWriterNsStackEntry {
            prefix: buf,
            uri: namespace_uri.to_owned(),
            elem: (*writer).nodes.front().cloned(),
        };
        (*writer).nsstack.push_first(p);
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

    let Some(lk) = (*writer).nodes.front() else {
        (*writer).nsstack.clear();
        return Err(io::Error::other("Nodes link is NULL"));
    };

    let mut sum = 0;
    match lk.state.get() {
        ty @ XmlTextWriterState::XmlTextwriterAttribute
        | ty @ XmlTextWriterState::XmlTextwriterName => {
            if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                let count = xml_text_writer_end_attribute(writer);
                if count.is_err() {
                    (*writer).nsstack.clear();
                    return count;
                }
                sum += count.unwrap();
            }

            // Output namespace declarations
            sum += xml_text_writer_output_nsdecl(writer)?;

            // next element needs indent
            if (*writer).indent != 0 {
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
            sum += (*writer).out.write_str(lk.name.as_deref().unwrap())?;
            sum += (*writer).out.write_str(">")?;
        }
        _ => {
            return Err(io::Error::other("Invalid state"));
        }
    }

    if (*writer).indent != 0 {
        sum += (*writer).out.write_str("\n")?;
    }

    (*writer).nodes.pop_front();
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

    let Some(lk) = (*writer).nodes.front() else {
        return Err(io::Error::other("Nodes link is NULL"));
    };

    let mut sum = 0;
    match lk.state.get() {
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
            sum += (*writer).out.write_str(lk.name.as_deref().unwrap())?;
            sum += (*writer).out.write_str(">")?;
        }
        _ => {
            return Err(io::Error::other("Invalid state"));
        }
    }

    if (*writer).indent != 0 {
        sum += (*writer).out.write_str("\n")?;
    }

    (*writer).nodes.pop_front();
    Ok(sum)
}

/// Write an xml element.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteElement")]
pub unsafe fn xml_text_writer_write_element(
    writer: XmlTextWriterPtr,
    name: &str,
    content: Option<&str>,
) -> io::Result<usize> {
    let mut sum = xml_text_writer_start_element(writer, name)?;
    if let Some(content) = content {
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
    prefix: Option<&str>,
    name: &str,
    namespace_uri: Option<&str>,
    content: &str,
) -> io::Result<usize> {
    if writer.is_null() || name.is_empty() {
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
    p: &mut Rc<XmlTextWriterStackEntry>,
) -> io::Result<usize> {
    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    let mut sum = 0;
    match p.state.get() {
        XmlTextWriterState::XmlTextwriterName => {
            // Output namespace declarations
            sum += xml_text_writer_output_nsdecl(writer)?;
            sum += (*writer).out.write_str(">")?;
            p.state.set(XmlTextWriterState::XmlTextwriterText);
        }
        XmlTextWriterState::XmlTextwriterPI => {
            sum += (*writer).out.write_str(" ")?;
            p.state.set(XmlTextWriterState::XmlTextwriterPIText);
        }
        XmlTextWriterState::XmlTextwriterDTD => {
            sum += (*writer).out.write_str(" [")?;
            p.state.set(XmlTextWriterState::XmlTextwriterDTDText);
        }
        XmlTextWriterState::XmlTextwriterDTDElem => {
            sum += (*writer).out.write_str(" ")?;
            p.state.set(XmlTextWriterState::XmlTextwriterDTDElemText);
        }
        XmlTextWriterState::XmlTextwriterDTDAttl => {
            sum += (*writer).out.write_str(" ")?;
            p.state.set(XmlTextWriterState::XmlTextwriterDTDAttlText);
        }
        XmlTextWriterState::XmlTextwriterDTDEnty | XmlTextWriterState::XmlTextwriterDTDPEnt => {
            sum += (*writer).out.write_bytes(&[b' ', (*writer).qchar])?;
            p.state.set(XmlTextWriterState::XmlTextwriterDTDEntyText);
        }
        _ => {}
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
    if writer.is_null() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            "xmlTextWriterWriteRawLen : invalid writer!\n",
        );
        return Err(io::Error::other(
            "xmlTextWriterWriteRawLen : invalid writer!",
        ));
    }

    if content.is_null() || len < 0 {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            "xmlTextWriterWriteRawLen : invalid content!\n",
        );
        return Err(io::Error::other(
            "xmlTextWriterWriteRawLen : invalid content!",
        ));
    }

    let mut sum = 0;
    if let Some(lk) = (*writer).nodes.front_mut() {
        sum += xml_text_writer_handle_state_dependencies(writer, lk)?;
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
    content: &str,
) -> io::Result<usize> {
    if writer.is_null() {
        return Err(io::Error::other("Writer of content is NULL"));
    }

    let mut sum = 0;
    let content = xml_strndup(content.as_ptr(), content.len() as i32);
    let mut buf = content;
    if let Some(lk) = (*writer).nodes.front() {
        match lk.state.get() {
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

    if !buf.is_null() {
        let count = xml_text_writer_write_raw(writer, buf);
        if buf != content {
            // buf was allocated by us, so free it
            xml_free(buf as _);
        }
        sum += count?;
    }
    xml_free(content as _);

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
    if writer.is_null() || data.is_null() || start < 0 || len < 0 {
        return Err(io::Error::other("Writer or content is invalid"));
    }

    let mut sum = 0;
    if let Some(lk) = (*writer).nodes.front_mut() {
        sum += xml_text_writer_handle_state_dependencies(writer, lk)?;
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
    if writer.is_null() || data.is_null() || start < 0 || len < 0 {
        return Err(io::Error::other("Writer or content is invalid"));
    }

    let mut sum = 0;
    if let Some(lk) = (*writer).nodes.front_mut() {
        sum += xml_text_writer_handle_state_dependencies(writer, lk)?;
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
    name: &str,
) -> io::Result<usize> {
    if writer.is_null() || name.is_empty() {
        return Err(io::Error::other("Writer or name is invalid"));
    }

    let Some(lk) = (*writer).nodes.front_mut() else {
        return Err(io::Error::other("Node link is NULL"));
    };

    let mut sum = 0;
    match lk.state.get() {
        ty @ XmlTextWriterState::XmlTextwriterAttribute
        | ty @ XmlTextWriterState::XmlTextwriterName => {
            if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                sum += xml_text_writer_end_attribute(writer)?;
            }

            sum += (*writer).out.write_str(" ")?;
            sum += (*writer).out.write_str(name)?;
            sum += (*writer).out.write_str("=")?;
            sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
            lk.state.set(XmlTextWriterState::XmlTextwriterAttribute);
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
    prefix: Option<&str>,
    name: &str,
    namespace_uri: Option<&str>,
) -> io::Result<usize> {
    if writer.is_null() || name.is_empty() {
        return Err(io::Error::other("Writer or name is invalid"));
    }

    // Handle namespace first in case of error
    if let Some(namespace_uri) = namespace_uri {
        let mut buf = "xmlns".to_owned();
        if let Some(prefix) = prefix {
            buf.push(':');
            buf.push_str(prefix);
        }

        let nsentry = XmlTextWriterNsStackEntry {
            prefix: buf.clone(),
            uri: namespace_uri.to_owned(),
            elem: (*writer).nodes.front().cloned(),
        };

        if let Some(curns) = (*writer).nsstack.search(&nsentry) {
            if curns.uri == namespace_uri {
                // Namespace already defined on element skip
                buf.clear();
            } else {
                // Prefix mismatch so error out
                return Err(io::Error::other("Prefix mismatch"));
            }
        }

        // Do not add namespace decl to list - it is already there
        if !buf.is_empty() {
            let p = XmlTextWriterNsStackEntry {
                prefix: buf,
                uri: namespace_uri.to_owned(),
                elem: (*writer).nodes.front().cloned(),
            };
            (*writer).nsstack.push_first(p);
        }
    }

    let mut buf = String::new();
    if let Some(prefix) = prefix {
        buf.push_str(prefix);
        buf.push(':');
    }
    buf.push_str(name);

    xml_text_writer_start_attribute(writer, &buf)
}

/// End the current xml element.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterEndAttribute")]
pub unsafe fn xml_text_writer_end_attribute(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    let Some(lk) = (*writer).nodes.front_mut() else {
        return Err(io::Error::other("List is NULL"));
    };

    let mut sum = 0;
    match lk.state.get() {
        XmlTextWriterState::XmlTextwriterAttribute => {
            lk.state.set(XmlTextWriterState::XmlTextwriterName);

            sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
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
    name: &str,
    content: &str,
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
    prefix: Option<&str>,
    name: &str,
    namespace_uri: Option<&str>,
    content: &str,
) -> io::Result<usize> {
    if writer.is_null() || name.is_empty() {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let mut sum = xml_text_writer_start_attribute_ns(writer, prefix, name, namespace_uri)?;
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
    if writer.is_null() || target.is_null() || *target == b'\0' {
        return Err(io::Error::other("Writer or target is NULL"));
    }

    if xml_strcasecmp(target, c"xml".as_ptr() as _) == 0 {
        xml_writer_err_msg(writer, XmlParserErrors::XmlErrInternalError,
                        "xmlTextWriterStartPI : target name [Xx][Mm][Ll] is reserved for xml standardization!\n");
        return Err(io::Error::other(
            "xmlTextWriterStartPI : target name [Xx][Mm][Ll] is reserved for xml standardization!",
        ));
    }

    let mut sum = 0;
    if let Some(lk) = (*writer).nodes.front_mut() {
        match lk.state.get() {
            ty @ XmlTextWriterState::XmlTextwriterAttribute
            | ty @ XmlTextWriterState::XmlTextwriterName => {
                if matches!(ty, XmlTextWriterState::XmlTextwriterAttribute) {
                    sum += xml_text_writer_end_attribute(writer)?;
                }
                // Output namespace declarations
                sum += xml_text_writer_output_nsdecl(writer)?;
                sum += (*writer).out.write_str(">")?;
                lk.state.set(XmlTextWriterState::XmlTextwriterText);
            }
            XmlTextWriterState::XmlTextwriterNone
            | XmlTextWriterState::XmlTextwriterText
            | XmlTextWriterState::XmlTextwriterDTD => {}
            XmlTextWriterState::XmlTextwriterPI | XmlTextWriterState::XmlTextwriterPIText => {
                xml_writer_err_msg(
                    writer,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlTextWriterStartPI : nested PI!\n",
                );
                return Err(io::Error::other("xmlTextWriterStartPI : nested PI!"));
            }
            _ => {
                return Err(io::Error::other("Invalid state"));
            }
        }
    }

    let p = XmlTextWriterStackEntry {
        name: Some(
            CStr::from_ptr(target as *const i8)
                .to_string_lossy()
                .into_owned(),
        ),
        state: Cell::new(XmlTextWriterState::XmlTextwriterPI),
    };
    (*writer).nodes.push_front(p.into());

    sum += (*writer).out.write_str("<?")?;
    sum += (*writer)
        .out
        .write_str(CStr::from_ptr(target as _).to_string_lossy().as_ref())?;
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

    let Some(lk) = (*writer).nodes.front() else {
        return Ok(0);
    };

    let mut sum = 0;
    match lk.state.get() {
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

    (*writer).nodes.pop_front();
    Ok(sum)
}

/// Write an xml PI.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWritePI")]
pub unsafe fn xml_text_writer_write_pi(
    writer: XmlTextWriterPtr,
    target: *const XmlChar,
    content: Option<&str>,
) -> io::Result<usize> {
    let mut sum = xml_text_writer_start_pi(writer, target)?;
    if let Some(content) = content {
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
    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    let mut sum = 0;
    if let Some(lk) = (*writer).nodes.front_mut() {
        match lk.state.get() {
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
                lk.state.set(XmlTextWriterState::XmlTextwriterText);
            }
            XmlTextWriterState::XmlTextwriterCDATA => {
                xml_writer_err_msg(
                    writer,
                    XmlParserErrors::XmlErrInternalError,
                    "xmlTextWriterStartCDATA : CDATA not allowed in this context!\n",
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

    (*writer).nodes.push_front(
        XmlTextWriterStackEntry {
            name: None,
            state: Cell::new(XmlTextWriterState::XmlTextwriterCDATA),
        }
        .into(),
    );

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

    let Some(lk) = (*writer).nodes.front() else {
        return Err(io::Error::other("Node list is NULL"));
    };

    let mut sum = 0;
    match lk.state.get() {
        XmlTextWriterState::XmlTextwriterCDATA => {
            sum += (*writer).out.write_str("]]>")?;
        }
        _ => {
            return Err(io::Error::other("Invalid state"));
        }
    }

    (*writer).nodes.pop_front();
    Ok(sum)
}

/// Write an xml CDATA.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteCDATA")]
pub unsafe fn xml_text_writer_write_cdata(
    writer: XmlTextWriterPtr,
    content: Option<&str>,
) -> io::Result<usize> {
    let mut sum = xml_text_writer_start_cdata(writer)?;
    if let Some(content) = content {
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

    if (*writer).nodes.front().is_some() {
        xml_writer_err_msg(
            writer,
            XmlParserErrors::XmlErrInternalError,
            "xmlTextWriterStartDTD : DTD allowed only in prolog!\n",
        );
        return Err(io::Error::other(
            "xmlTextWriterStartDTD : DTD allowed only in prolog!",
        ));
    }

    let p = XmlTextWriterStackEntry {
        name: Some(
            CStr::from_ptr(name as *const i8)
                .to_string_lossy()
                .into_owned(),
        ),
        state: Cell::new(XmlTextWriterState::XmlTextwriterDTD),
    };
    (*writer).nodes.push_front(p.into());

    let mut sum = (*writer).out.write_str("<!DOCTYPE ")?;
    sum += (*writer)
        .out
        .write_str(CStr::from_ptr(name as _).to_string_lossy().as_ref())?;

    if !pubid.is_null() {
        if sysid.is_null() {
            xml_writer_err_msg(
                writer,
                XmlParserErrors::XmlErrInternalError,
                "xmlTextWriterStartDTD : system identifier needed!\n",
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
        sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(pubid as _).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
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

        sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(sysid as _).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
    }

    Ok(sum)
}

/// End an xml DTD.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterEndDTD")]
pub unsafe fn xml_text_writer_end_dtd(writer: XmlTextWriterPtr) -> io::Result<usize> {
    if writer.is_null() {
        return Err(io::Error::other("Writer is NULL"));
    }

    let mut sum = 0;
    while let Some(lk) = (*writer).nodes.front() {
        match lk.state.get() {
            ty @ XmlTextWriterState::XmlTextwriterDTDText
            | ty @ XmlTextWriterState::XmlTextwriterDTD => {
                if matches!(ty, XmlTextWriterState::XmlTextwriterDTDText) {
                    sum += (*writer).out.write_str("]")?;
                }
                sum += (*writer).out.write_str(">")?;

                if (*writer).indent != 0 {
                    sum += (*writer).out.write_str("\n")?;
                }

                (*writer).nodes.pop_front();
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
                break;
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
    subset: Option<&str>,
) -> io::Result<usize> {
    let mut sum = xml_text_writer_start_dtd(writer, name, pubid, sysid)?;
    if let Some(subset) = subset {
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

    let Some(lk) = (*writer).nodes.front_mut() else {
        return Err(io::Error::other("Node list is NULL"));
    };

    let mut sum = 0;
    match lk.state.get() {
        XmlTextWriterState::XmlTextwriterDTD => {
            sum += (*writer).out.write_str(" [")?;
            if (*writer).indent != 0 {
                sum += (*writer).out.write_str("\n")?;
            }
            lk.state.set(XmlTextWriterState::XmlTextwriterDTDText);
        }
        XmlTextWriterState::XmlTextwriterDTDText | XmlTextWriterState::XmlTextwriterNone => {}
        _ => {
            return Err(io::Error::other("Invalid state"));
        }
    }

    let p = XmlTextWriterStackEntry {
        name: Some(
            CStr::from_ptr(name as *const i8)
                .to_string_lossy()
                .into_owned(),
        ),
        state: Cell::new(XmlTextWriterState::XmlTextwriterDTDElem),
    };
    (*writer).nodes.push_front(p.into());

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

    let Some(lk) = (*writer).nodes.front() else {
        return Err(io::Error::other("Node list is NULL"));
    };

    let mut sum = 0;
    match lk.state.get() {
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

    (*writer).nodes.pop_front();
    Ok(sum)
}

/// Write a DTD element.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteDTDElement")]
pub unsafe fn xml_text_writer_write_dtdelement(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    content: &str,
) -> io::Result<usize> {
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
    if writer.is_null() || name.is_null() || *name == b'\0' {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let Some(lk) = (*writer).nodes.front_mut() else {
        return Err(io::Error::other("Node list is NULL"));
    };

    let mut sum = 0;
    match lk.state.get() {
        XmlTextWriterState::XmlTextwriterDTD => {
            sum += (*writer).out.write_str(" [")?;
            if (*writer).indent != 0 {
                sum += (*writer).out.write_str("\n")?;
            }
            lk.state.set(XmlTextWriterState::XmlTextwriterDTDText);
        }
        XmlTextWriterState::XmlTextwriterDTDText | XmlTextWriterState::XmlTextwriterNone => {}
        _ => {
            return Err(io::Error::other("Invalid state"));
        }
    }

    let p = XmlTextWriterStackEntry {
        name: Some(
            CStr::from_ptr(name as *const i8)
                .to_string_lossy()
                .into_owned(),
        ),
        state: Cell::new(XmlTextWriterState::XmlTextwriterDTDAttl),
    };
    (*writer).nodes.push_front(p.into());

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

    let Some(lk) = (*writer).nodes.front() else {
        return Err(io::Error::other("Node list is NULL"));
    };

    let mut sum = 0;
    match lk.state.get() {
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
    (*writer).nodes.pop_front();
    Ok(sum)
}

/// Write a DTD ATTLIST.
///
/// Returns the bytes written (may be 0 because of buffering) or -1 in case of error
#[doc(alias = "xmlTextWriterWriteDTDAttlist")]
pub unsafe fn xml_text_writer_write_dtd_attlist(
    writer: XmlTextWriterPtr,
    name: *const XmlChar,
    content: &str,
) -> io::Result<usize> {
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
    if writer.is_null() || name.is_null() || *name == b'\0' {
        return Err(io::Error::other("Writer or name is NULL"));
    }

    let mut sum = 0;
    if let Some(lk) = (*writer).nodes.front_mut() {
        match lk.state.get() {
            XmlTextWriterState::XmlTextwriterDTD => {
                sum += (*writer).out.write_str(" [")?;
                if (*writer).indent != 0 {
                    sum += (*writer).out.write_str("\n")?;
                }
                lk.state.set(XmlTextWriterState::XmlTextwriterDTDText);
            }
            _ty @ XmlTextWriterState::XmlTextwriterDTDText
            | _ty @ XmlTextWriterState::XmlTextwriterNone => {}
            _ => {
                return Err(io::Error::other("Invalid state"));
            }
        }
    }

    let p = XmlTextWriterStackEntry {
        name: Some(
            CStr::from_ptr(name as *const i8)
                .to_string_lossy()
                .into_owned(),
        ),
        state: if pe != 0 {
            Cell::new(XmlTextWriterState::XmlTextwriterDTDPEnt)
        } else {
            Cell::new(XmlTextWriterState::XmlTextwriterDTDEnty)
        },
    };
    (*writer).nodes.push_front(p.into());

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

    let Some(lk) = (*writer).nodes.front() else {
        return Err(io::Error::other("Node list is NULL"));
    };

    let mut sum = 0;
    match lk.state.get() {
        ty @ XmlTextWriterState::XmlTextwriterDTDEntyText
        | ty @ XmlTextWriterState::XmlTextwriterDTDEnty
        | ty @ XmlTextWriterState::XmlTextwriterDTDPEnt => {
            if matches!(ty, XmlTextWriterState::XmlTextwriterDTDEntyText) {
                sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
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
    (*writer).nodes.pop_front();
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
    content: &str,
) -> io::Result<usize> {
    if name.is_null() || *name == b'\0' {
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
            "xmlTextWriterWriteDTDExternalEntityContents: xmlTextWriterPtr invalid!\n",
        );
        return Err(io::Error::other(
            "xmlTextWriterWriteDTDExternalEntityContents: xmlTextWriterPtr invalid!",
        ));
    }

    let Some(lk) = (*writer).nodes.front() else {
        xml_writer_err_msg(writer, XmlParserErrors::XmlErrInternalError,
                        "xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!\n");
        return Err(io::Error::other(
            "xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!",
        ));
    };

    match lk.state.get() {
        XmlTextWriterState::XmlTextwriterDTDEnty => {}
        XmlTextWriterState::XmlTextwriterDTDPEnt => {
            if !ndataid.is_null() {
                xml_writer_err_msg(writer, XmlParserErrors::XmlErrInternalError,
                                "xmlTextWriterWriteDTDExternalEntityContents: notation not allowed with parameter entities!\n");
                return Err(io::Error::other("xmlTextWriterWriteDTDExternalEntityContents: notation not allowed with parameter entities!"));
            }
        }
        _ => {
            xml_writer_err_msg(writer, XmlParserErrors::XmlErrInternalError,
                            "xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!\n");
            return Err(io::Error::other("xmlTextWriterWriteDTDExternalEntityContents: you must call xmlTextWriterStartDTDEntity before the call to this function!"));
        }
    }

    let mut sum = 0;
    if !pubid.is_null() {
        if sysid.is_null() {
            xml_writer_err_msg(
                writer,
                XmlParserErrors::XmlErrInternalError,
                "xmlTextWriterWriteDTDExternalEntityContents: system identifier needed!\n",
            );
            return Err(io::Error::other(
                "xmlTextWriterWriteDTDExternalEntityContents: system identifier needed!",
            ));
        }

        sum += (*writer).out.write_str(" PUBLIC ")?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(pubid as _).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
    }

    if !sysid.is_null() {
        if pubid.is_null() {
            sum += (*writer).out.write_str(" SYSTEM")?;
        }
        sum += (*writer).out.write_str(" ")?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(sysid as _).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
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
    content: Option<&str>,
) -> io::Result<usize> {
    if content.is_none() && pubid.is_null() && sysid.is_null() {
        return Err(io::Error::other("Content, PublicID and SystemID are NULL"));
    }
    if pe != 0 && !ndataid.is_null() {
        return Err(io::Error::other("This is PE, but NDATAID is not NULL"));
    }

    if pubid.is_null() && sysid.is_null() {
        return xml_text_writer_write_dtd_internal_entity(writer, pe, name, content.unwrap());
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

    let Some(lk) = (*writer).nodes.front_mut() else {
        return Err(io::Error::other("Node list is NULL"));
    };

    let mut sum = 0;
    match lk.state.get() {
        XmlTextWriterState::XmlTextwriterDTD => {
            sum += (*writer).out.write_str(" [")?;
            if (*writer).indent != 0 {
                sum += (*writer).out.write_str("\n")?;
            }
            lk.state.set(XmlTextWriterState::XmlTextwriterDTDText);
        }
        XmlTextWriterState::XmlTextwriterDTDText => {}
        _ => {
            return Err(io::Error::other("Invalid state"));
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
        sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(pubid as _).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
    }

    if !sysid.is_null() {
        if pubid.is_null() {
            sum += (*writer).out.write_str(" SYSTEM")?;
        }
        sum += (*writer).out.write_str(" ")?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
        sum += (*writer)
            .out
            .write_str(CStr::from_ptr(sysid as _).to_string_lossy().as_ref())?;
        sum += (*writer).out.write_bytes(&[(*writer).qchar])?;
    }

    sum += (*writer).out.write_str(">")?;
    Ok(sum)
}

/// Set indentation output. indent = 0 do not indentation. indent > 0 do indentation.
///
/// Returns -1 on error or 0 otherwise.
#[doc(alias = "xmlTextWriterSetIndent")]
pub unsafe fn xml_text_writer_set_indent(writer: XmlTextWriterPtr, indent: i32) -> i32 {
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
pub unsafe fn xml_text_writer_set_indent_string(
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
pub unsafe fn xml_text_writer_set_quote_char(writer: XmlTextWriterPtr, quotechar: XmlChar) -> i32 {
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
pub unsafe fn xml_text_writer_flush(writer: XmlTextWriterPtr) -> i32 {
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
}

use std::{
    cell::RefCell,
    ffi::CString,
    ptr::{null, null_mut},
    rc::Rc,
};

use libc::{memset, FILE};

use crate::{
    buf::XmlBufRef,
    encoding::find_encoding_handler,
    error::XmlParserErrors,
    io::{
        xml_alloc_output_buffer, xml_output_buffer_close, xml_output_buffer_create_file,
        xml_output_buffer_create_filename, XmlOutputBuffer, XmlOutputBufferPtr,
    },
    libxml::{
        globals::{xml_free, xml_malloc},
        htmltree::html_node_dump_output,
        parser::xml_init_parser,
        xmlsave::{
            xhtml_node_dump_output, xml_doc_content_dump_output, xml_node_dump_output_internal,
            xml_save_ctxt_init, xml_save_err, xml_save_err_memory, XmlSaveCtxt, XmlSaveOption,
        },
        xmlstring::{xml_strndup, XmlChar},
    },
    tree::{xml_buf_set_allocation_scheme, xml_buf_use, xml_is_xhtml, XmlBufferAllocationScheme},
};

use super::{
    xml_buf_get_allocation_scheme, XmlBufPtr, XmlDoc, XmlDocPtr, XmlDtdPtr, XmlElementType,
    XmlNodePtr,
};

impl XmlDoc {
    /// Dump the current DOM tree into memory using the character encoding specified by the caller.  
    ///
    /// Note it is up to the caller of this function to free the allocated memory with xml_free().
    ///
    /// Note that `format` = 1 provide node indenting only if xmlIndentTreeOutput = 1
    /// or xmlKeepBlanksDefault(0) was called
    #[doc(alias = "xmlDocDumpFormatMemoryEnc")]
    pub unsafe fn dump_format_memory_enc(
        &mut self,
        doc_txt_ptr: *mut *mut XmlChar,
        mut doc_txt_len: *mut i32,
        txt_encoding: Option<&str>,
        format: i32,
    ) {
        let mut ctxt = XmlSaveCtxt::default();
        let mut dummy: i32 = 0;

        if doc_txt_len.is_null() {
            doc_txt_len = &raw mut dummy; /*  Continue, caller just won't get length */
        }

        if doc_txt_ptr.is_null() {
            *doc_txt_len = 0;
            return;
        }

        *doc_txt_ptr = null_mut();
        *doc_txt_len = 0;

        /*
         *  Validate the encoding value, if provided.
         *  This logic is copied from xmlSaveFileEnc.
         */
        let encoding = txt_encoding.map_or_else(
            || self.encoding.as_ref().map(|e| e.to_owned()),
            |e| Some(e.to_owned()),
        );
        let conv_hdlr = if let Some(encoding) = encoding.as_deref() {
            let Some(handler) = find_encoding_handler(encoding) else {
                let enc = CString::new(encoding).unwrap();
                xml_save_err(
                    XmlParserErrors::XmlSaveUnknownEncoding,
                    self as *mut XmlDoc as XmlNodePtr,
                    enc.as_ptr(),
                );
                return;
            };
            Some(handler)
        } else {
            None
        };

        let out_buff: XmlOutputBufferPtr = xml_alloc_output_buffer(conv_hdlr);
        if out_buff.is_null() {
            xml_save_err_memory(c"creating buffer".as_ptr() as _);
            return;
        }

        ctxt.buf = out_buff;
        ctxt.level = 0;
        ctxt.format = (format != 0) as i32;
        ctxt.encoding = encoding;
        xml_save_ctxt_init(&mut ctxt);
        ctxt.options |= XmlSaveOption::XmlSaveAsXml as i32;
        xml_doc_content_dump_output(&raw mut ctxt, self);
        (*out_buff).flush();
        if let Some(conv) = (*out_buff).conv {
            *doc_txt_len = conv.len() as i32;
            *doc_txt_ptr = xml_strndup(
                if conv.is_ok() {
                    conv.as_ref().as_ptr()
                } else {
                    null()
                },
                *doc_txt_len,
            );
        } else {
            *doc_txt_len = (*out_buff).buffer.map_or(0, |buf| buf.len() as i32);
            *doc_txt_ptr = xml_strndup(
                (*out_buff)
                    .buffer
                    .map_or(null(), |buf| buf.as_ref().as_ptr()),
                *doc_txt_len,
            );
        }
        xml_output_buffer_close(out_buff);

        if (*doc_txt_ptr).is_null() && *doc_txt_len > 0 {
            *doc_txt_len = 0;
            xml_save_err_memory(c"creating output".as_ptr() as _);
        }
    }

    /// Dump an XML document in memory and return the `*mut XmlChar` and it's size.  
    /// It's up to the caller to free the memory with xml_free().
    ///
    /// Note that `format = 1` provide node indenting only if `xmlIndentTreeOutput = 1`
    /// or `xmlKeepBlanksDefault(0)` was called
    #[doc(alias = "xmlDocDumpFormatMemory")]
    pub unsafe fn dump_format_memory(
        &mut self,
        mem: *mut *mut XmlChar,
        size: *mut i32,
        format: i32,
    ) {
        self.dump_format_memory_enc(mem, size, None, format);
    }

    /// Dump an XML document in memory and return the `*mut XmlChar` and it's size in bytes.   
    /// It's up to the caller to free the memory with xml_free().
    ///
    /// The resulting byte array is zero terminated, though the last 0 is not
    /// included in the returned size.
    #[doc(alias = "xmlDocDumpMemory")]
    pub unsafe fn dump_memory(&mut self, mem: *mut *mut XmlChar, size: *mut i32) {
        self.dump_format_memory_enc(mem, size, None, 0);
    }

    /// Dump the current DOM tree into memory using the character encoding specified by the caller.  
    ///
    /// Note it is up to the caller of this function to free the allocated memory with `xml_free()`.
    #[doc(alias = "xmlDocDumpMemoryEnc")]
    pub unsafe fn dump_memory_enc(
        &mut self,
        doc_txt_ptr: *mut *mut XmlChar,
        doc_txt_len: *mut i32,
        txt_encoding: Option<&str>,
    ) {
        self.dump_format_memory_enc(doc_txt_ptr, doc_txt_len, txt_encoding, 0);
    }

    /// Dump an XML document to an open FILE.
    ///
    /// returns: the number of bytes written or -1 in case of failure.
    ///
    /// Note that `format = 1` provide node indenting only if `xmlIndentTreeOutput = 1`
    /// or `xmlKeepBlanksDefault(0)` was called
    #[doc(alias = "xmlDocFormatDump")]
    pub unsafe fn dump_format_file(&mut self, f: *mut FILE, format: i32) -> i32 {
        let mut encoding = self.encoding.clone();

        let handler = if let Some(enc) = self.encoding.as_deref() {
            if let Some(handler) = find_encoding_handler(enc) {
                Some(handler)
            } else {
                self.encoding = None;
                encoding = None;
                None
            }
        } else {
            None
        };
        let buf: XmlOutputBufferPtr = xml_output_buffer_create_file(f, handler);
        if buf.is_null() {
            return -1;
        }

        let mut ctxt = XmlSaveCtxt {
            buf,
            level: 0,
            format: (format != 0) as i32,
            encoding,
            ..Default::default()
        };
        xml_save_ctxt_init(&mut ctxt);
        ctxt.options |= XmlSaveOption::XmlSaveAsXml as i32;
        xml_doc_content_dump_output(&raw mut ctxt as _, self);

        let ret: i32 = xml_output_buffer_close(buf);
        ret
    }

    /// Dump an XML document to an open FILE.
    ///
    /// returns: the number of bytes written or -1 in case of failure.
    #[doc(alias = "xmlDocDump")]
    pub unsafe fn dump_file(&mut self, f: *mut FILE) -> i32 {
        self.dump_format_file(f, 0)
    }

    /// Dump an XML document to a file or an URL.
    ///
    /// Returns the number of bytes written or -1 in case of error.  
    ///
    /// Note that `format` = 1 provide node indenting only if `xmlIndentTreeOutput = 1`
    /// or `xmlKeepBlanksDefault(0)` was called
    #[doc(alias = "xmlSaveFormatFileEnc")]
    pub unsafe fn save_format_file_enc(
        &mut self,
        filename: *const i8,
        encoding: Option<&str>,
        format: i32,
    ) -> i32 {
        let encoding = encoding.map_or_else(
            || self.encoding.as_ref().map(|e| e.to_owned()),
            |e| Some(e.to_owned()),
        );
        let handler = if let Some(enc) = encoding.as_deref() {
            let Some(handler) = find_encoding_handler(enc) else {
                return -1;
            };
            Some(handler)
        } else {
            None
        };

        // save the content to a temp buffer.
        let buf: XmlOutputBufferPtr = xml_output_buffer_create_filename(
            filename,
            handler.map(|e| Rc::new(RefCell::new(e))),
            self.compression,
        );
        if buf.is_null() {
            return -1;
        }
        let mut ctxt = XmlSaveCtxt {
            buf,
            level: 0,
            format: (format != 0) as i32,
            encoding,
            ..Default::default()
        };
        xml_save_ctxt_init(&mut ctxt);
        ctxt.options |= XmlSaveOption::XmlSaveAsXml as i32;

        xml_doc_content_dump_output(&raw mut ctxt as _, self);

        let ret: i32 = xml_output_buffer_close(buf);
        ret
    }

    /// Dump an XML document to a file. Will use compression if compiled in and enabled.  
    ///
    /// If `filename` is "-" the stdout file is used.  
    /// returns: the number of bytes written or -1 in case of failure.
    #[doc(alias = "xmlSaveFile")]
    pub unsafe fn save_file(&mut self, filename: *const i8) -> i32 {
        self.save_format_file_enc(filename, None, 0)
    }
}

/**
 * xmlElemDump:
 * @f:  the FILE * for the output
 * @doc:  the document
 * @cur:  the current node
 *
 * Dump an XML/HTML node, recursive behaviour, children are printed too.
 */
pub unsafe extern "C" fn xml_elem_dump(f: *mut FILE, doc: XmlDocPtr, cur: XmlNodePtr) {
    xml_init_parser();

    if cur.is_null() {
        return;
    }

    let outbuf: XmlOutputBufferPtr = xml_output_buffer_create_file(f, None);
    if outbuf.is_null() {
        return;
    }
    if !doc.is_null() && matches!((*doc).typ, XmlElementType::XmlHtmlDocumentNode) {
        #[cfg(feature = "html")]
        {
            html_node_dump_output(outbuf, doc, cur, null_mut());
        }
        #[cfg(not(feature = "html"))]
        {
            xmlSaveErr(
                XmlParserErrors::XmlErrInternalError,
                cur,
                c"HTML support not compiled in\n".as_ptr() as _,
            );
        }
    } else {
        xml_node_dump_output(outbuf, doc, cur, 0, 1, None);
    }
    xml_output_buffer_close(outbuf);
}

/**
 * xmlSaveFormatFile:
 * @filename:  the filename (or URL)
 * @cur:  the document
 * @format:  should formatting spaces been added
 *
 * Dump an XML document to a file. Will use compression if
 * compiled in and enabled. If @filename is "-" the stdout file is
 * used. If @format is set then the document will be indented on output.
 * Note that @format = 1 provide node indenting only if xmlIndentTreeOutput = 1
 * or xmlKeepBlanksDefault(0) was called
 *
 * returns: the number of bytes written or -1 in case of failure.
 */
pub unsafe extern "C" fn xml_save_format_file(
    filename: *const i8,
    cur: XmlDocPtr,
    format: i32,
) -> i32 {
    if !cur.is_null() {
        (*cur).save_format_file_enc(filename, None, format)
    } else {
        -1
    }
}

/**
 * xmlBufNodeDump:
 * @buf:  the XML buffer output
 * @doc:  the document
 * @cur:  the current node
 * @level: the imbrication level for indenting
 * @format: is formatting allowed
 *
 * Dump an XML node, recursive behaviour,children are printed too.
 * Note that @format = 1 provide node indenting only if xmlIndentTreeOutput = 1
 * or xmlKeepBlanksDefault(0) was called
 *
 * Returns the number of bytes written to the buffer, in case of error 0
 *     is returned or @buf stores the error
 */
pub unsafe extern "C" fn xml_buf_node_dump(
    buf: XmlBufPtr,
    doc: XmlDocPtr,
    cur: XmlNodePtr,
    level: i32,
    format: i32,
) -> usize {
    xml_init_parser();

    if cur.is_null() {
        return usize::MAX;
    }
    if buf.is_null() {
        return usize::MAX;
    }
    let outbuf: XmlOutputBufferPtr = xml_malloc(size_of::<XmlOutputBuffer>()) as _;
    if outbuf.is_null() {
        xml_save_err_memory(c"creating buffer".as_ptr() as _);
        return usize::MAX;
    }
    memset(outbuf as _, 0, size_of::<XmlOutputBuffer>());
    (*outbuf).buffer = XmlBufRef::from_raw(buf);
    (*outbuf).encoder = None;
    (*outbuf).writecallback = None;
    (*outbuf).closecallback = None;
    (*outbuf).context = null_mut();
    (*outbuf).written = 0;

    let using: usize = xml_buf_use(buf);
    let oldalloc: i32 = xml_buf_get_allocation_scheme(buf);
    xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_node_dump_output(outbuf, doc, cur, level, format, None);
    xml_buf_set_allocation_scheme(buf, oldalloc.try_into().unwrap());
    xml_free(outbuf as _);
    let ret: i32 = (xml_buf_use(buf) - using) as i32;
    ret as _
}

/**
 * xmlSaveFileTo:
 * @buf:  an output I/O buffer
 * @cur:  the document
 * @encoding:  the encoding if any assuming the I/O layer handles the transcoding
 *
 * Dump an XML document to an I/O buffer.
 * Warning ! This call xmlOutputBufferClose() on buf which is not available
 * after this call.
 *
 * returns: the number of bytes written or -1 in case of failure.
 */
pub unsafe fn xml_save_file_to(
    buf: XmlOutputBufferPtr,
    cur: XmlDocPtr,
    encoding: Option<&str>,
) -> i32 {
    let mut ctxt = XmlSaveCtxt::default();

    if buf.is_null() {
        return -1;
    }
    if cur.is_null() {
        xml_output_buffer_close(buf);
        return -1;
    }
    ctxt.buf = buf;
    ctxt.level = 0;
    ctxt.format = 0;
    ctxt.encoding = encoding.map(|e| e.to_owned());
    xml_save_ctxt_init(&mut ctxt);
    ctxt.options |= XmlSaveOption::XmlSaveAsXml as i32;
    xml_doc_content_dump_output(&raw mut ctxt as _, cur);
    let ret: i32 = xml_output_buffer_close(buf);
    ret
}

/**
 * xmlSaveFormatFileTo:
 * @buf:  an output I/O buffer
 * @cur:  the document
 * @encoding:  the encoding if any assuming the I/O layer handles the transcoding
 * @format: should formatting spaces been added
 *
 * Dump an XML document to an I/O buffer.
 * Warning ! This call xmlOutputBufferClose() on buf which is not available
 * after this call.
 *
 * returns: the number of bytes written or -1 in case of failure.
 */
pub unsafe fn xml_save_format_file_to(
    buf: XmlOutputBufferPtr,
    cur: XmlDocPtr,
    encoding: Option<&str>,
    format: i32,
) -> i32 {
    let mut ctxt = XmlSaveCtxt::default();

    if buf.is_null() {
        return -1;
    }
    if cur.is_null()
        || (!matches!((*cur).typ, XmlElementType::XmlDocumentNode)
            && !matches!((*cur).typ, XmlElementType::XmlHtmlDocumentNode))
    {
        xml_output_buffer_close(buf);
        return -1;
    }
    ctxt.buf = buf;
    ctxt.level = 0;
    ctxt.format = if format != 0 { 1 } else { 0 };
    ctxt.encoding = encoding.map(|e| e.to_owned());
    xml_save_ctxt_init(&mut ctxt);
    ctxt.options |= XmlSaveOption::XmlSaveAsXml as i32;
    xml_doc_content_dump_output(&raw mut ctxt as _, cur);
    let ret: i32 = xml_output_buffer_close(buf);
    ret
}

/**
 * xmlNodeDumpOutput:
 * @buf:  the XML buffer output
 * @doc:  the document
 * @cur:  the current node
 * @level: the imbrication level for indenting
 * @format: is formatting allowed
 * @encoding:  an optional encoding string
 *
 * Dump an XML node, recursive behaviour, children are printed too.
 * Note that @format = 1 provide node indenting only if xmlIndentTreeOutput = 1
 * or xmlKeepBlanksDefault(0) was called
 */
pub unsafe fn xml_node_dump_output(
    buf: XmlOutputBufferPtr,
    doc: XmlDocPtr,
    cur: XmlNodePtr,
    level: i32,
    format: i32,
    mut encoding: Option<&str>,
) {
    let mut ctxt = XmlSaveCtxt::default();
    #[cfg(feature = "html")]
    let dtd: XmlDtdPtr;
    #[cfg(feature = "html")]
    let mut is_xhtml: i32 = 0;

    xml_init_parser();

    if buf.is_null() || cur.is_null() {
        return;
    }

    if encoding.is_none() {
        encoding = Some("UTF-8");
    }

    ctxt.buf = buf;
    ctxt.level = level;
    ctxt.format = if format != 0 { 1 } else { 0 };
    ctxt.encoding = encoding.map(|e| e.to_owned());
    xml_save_ctxt_init(&mut ctxt);
    ctxt.options |= XmlSaveOption::XmlSaveAsXml as i32;

    #[cfg(feature = "html")]
    {
        dtd = if doc.is_null() {
            null_mut()
        } else {
            (*doc).get_int_subset()
        };
        if !dtd.is_null() {
            is_xhtml = xml_is_xhtml((*dtd).system_id, (*dtd).external_id);
            if is_xhtml < 0 {
                is_xhtml = 0;
            }
        }

        if is_xhtml != 0 {
            xhtml_node_dump_output(&raw mut ctxt as _, cur);
        } else {
            xml_node_dump_output_internal(&raw mut ctxt as _, cur);
        }
    }
    #[cfg(not(feature = "html"))]
    {
        xml_node_dump_output_internal(addr_of_mut!(ctxt) as _, cur);
    }
}

/**
 * xmlSaveFileEnc:
 * @filename:  the filename (or URL)
 * @cur:  the document
 * @encoding:  the name of an encoding (or NULL)
 *
 * Dump an XML document, converting it to the given encoding
 *
 * returns: the number of bytes written or -1 in case of failure.
 */
pub unsafe fn xml_save_file_enc(
    filename: *const i8,
    cur: XmlDocPtr,
    encoding: Option<&str>,
) -> i32 {
    if !cur.is_null() {
        (*cur).save_format_file_enc(filename, encoding, 0)
    } else {
        -1
    }
}

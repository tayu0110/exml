use std::{
    cell::RefCell,
    ffi::CString,
    ptr::{null, null_mut},
    rc::Rc,
};

use libc::FILE;

use crate::{
    buf::XmlBufRef,
    encoding::find_encoding_handler,
    error::XmlParserErrors,
    io::XmlOutputBuffer,
    libxml::{
        htmltree::html_node_dump_output,
        parser::xml_init_parser,
        xmlsave::{
            xhtml_node_dump_output, xml_doc_content_dump_output, xml_node_dump_output_internal,
            xml_save_ctxt_init, xml_save_err, xml_save_err_memory, XmlSaveCtxt, XmlSaveOption,
        },
        xmlstring::{xml_strndup, XmlChar},
    },
    tree::{is_xhtml, xml_buf_set_allocation_scheme, xml_buf_use, XmlBufferAllocationScheme},
};

use super::{
    xml_buf_get_allocation_scheme, XmlBufPtr, XmlDoc, XmlDocPtr, XmlElementType, XmlNode,
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

        let Some(out_buff) = XmlOutputBuffer::new(conv_hdlr).map(|buf| Rc::new(RefCell::new(buf)))
        else {
            xml_save_err_memory(c"creating buffer".as_ptr() as _);
            return;
        };

        ctxt.buf = out_buff.clone();
        ctxt.level = 0;
        ctxt.format = (format != 0) as i32;
        ctxt.encoding = encoding;
        xml_save_ctxt_init(&mut ctxt);
        ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;
        xml_doc_content_dump_output(&raw mut ctxt, self);
        out_buff.borrow_mut().flush();
        if let Some(conv) = out_buff.borrow().conv {
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
            *doc_txt_len = out_buff.borrow().buffer.map_or(0, |buf| buf.len() as i32);
            *doc_txt_ptr = xml_strndup(
                out_buff
                    .borrow()
                    .buffer
                    .map_or(null(), |buf| buf.as_ref().as_ptr()),
                *doc_txt_len,
            );
        }

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
        let Some(buf) =
            XmlOutputBuffer::from_writer(f, handler).map(|buf| Rc::new(RefCell::new(buf)))
        else {
            return -1;
        };

        let mut ctxt = XmlSaveCtxt {
            buf,
            level: 0,
            format: (format != 0) as i32,
            encoding,
            ..Default::default()
        };
        xml_save_ctxt_init(&mut ctxt);
        ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;
        xml_doc_content_dump_output(&raw mut ctxt as _, self);

        let mut buf = Rc::into_inner(ctxt.buf)
            .expect("Internal Error")
            .into_inner();

        if buf.error.is_ok() {
            buf.flush();
            buf.written
        } else {
            -1
        }
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
        filename: &str,
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
        let Some(buf) = XmlOutputBuffer::from_uri(
            filename,
            handler.map(|e| Rc::new(RefCell::new(e))),
            self.compression,
        )
        .map(|buf| Rc::new(RefCell::new(buf))) else {
            return -1;
        };
        let mut ctxt = XmlSaveCtxt {
            buf,
            level: 0,
            format: (format != 0) as i32,
            encoding,
            ..Default::default()
        };
        xml_save_ctxt_init(&mut ctxt);
        ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;

        xml_doc_content_dump_output(&raw mut ctxt as _, self);

        let mut buf = Rc::into_inner(ctxt.buf)
            .expect("Internal Error")
            .into_inner();
        if buf.error.is_ok() {
            buf.flush();
            buf.written
        } else {
            -1
        }
    }

    /// Dump an XML document to a file. Will use compression if compiled in and enabled.  
    ///
    /// If `filename` is "-" the stdout file is used.   
    /// If `format` is set then the document will be indented on output.
    ///
    /// Note that `format = 1` provide node indenting only if `xmlIndentTreeOutput = 1`
    /// or `xmlKeepBlanksDefault(0)` was called.
    ///
    /// returns: the number of bytes written or -1 in case of failure.
    #[doc(alias = "xmlSaveFormatFile")]
    pub unsafe fn save_format_file(&mut self, filename: &str, format: i32) -> i32 {
        self.save_format_file_enc(filename, None, format)
    }

    /// Dump an XML document, converting it to the given encoding.
    ///
    /// returns: the number of bytes written or -1 in case of failure.
    #[doc(alias = "xmlSaveFileEnc")]
    pub unsafe fn save_file_enc(&mut self, filename: &str, encoding: Option<&str>) -> i32 {
        self.save_format_file_enc(filename, encoding, 0)
    }

    /// Dump an XML document to a file. Will use compression if compiled in and enabled.  
    ///
    /// If `filename` is "-" the stdout file is used.  
    /// returns: the number of bytes written or -1 in case of failure.
    #[doc(alias = "xmlSaveFile")]
    pub unsafe fn save_file(&mut self, filename: &str) -> i32 {
        self.save_format_file_enc(filename, None, 0)
    }

    /// Dump an XML document to an I/O buffer.
    ///
    /// returns: the number of bytes written or -1 in case of failure.
    ///
    /// # Warning
    /// This call xmlOutputBufferClose() on buf which is not available after this call.
    #[doc(alias = "xmlSaveFormatFileTo")]
    pub unsafe fn save_format_file_to(
        &mut self,
        buf: XmlOutputBuffer,
        encoding: Option<&str>,
        format: i32,
    ) -> i32 {
        if !matches!(
            self.typ,
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
        ) {
            return -1;
        }
        let mut ctxt = XmlSaveCtxt {
            buf: Rc::new(RefCell::new(buf)),
            level: 0,
            format: (format != 0) as i32,
            encoding: encoding.map(|e| e.to_owned()),
            ..Default::default()
        };
        xml_save_ctxt_init(&mut ctxt);
        ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;
        xml_doc_content_dump_output(&raw mut ctxt as _, self);
        let mut buf = Rc::into_inner(ctxt.buf)
            .expect("Internal Error")
            .into_inner();
        if buf.error.is_ok() {
            buf.flush();
            buf.written
        } else {
            -1
        }
    }

    /// Dump an XML document to an I/O buffer.
    ///
    /// returns: the number of bytes written or -1 in case of failure.
    ///
    /// # Warning
    ///  This call xmlOutputBufferClose() on buf which is not available after this call.
    #[doc(alias = "xmlSaveFileTo")]
    pub unsafe fn save_file_to(
        &mut self,
        buf: Rc<RefCell<XmlOutputBuffer>>,
        encoding: Option<&str>,
    ) -> i32 {
        let mut ctxt = XmlSaveCtxt {
            buf,
            level: 0,
            format: 0,
            encoding: encoding.map(|e| e.to_owned()),
            ..Default::default()
        };
        xml_save_ctxt_init(&mut ctxt);
        ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;
        xml_doc_content_dump_output(&raw mut ctxt as _, self);
        let mut buf = Rc::into_inner(ctxt.buf)
            .expect("Internal Error")
            .into_inner();
        if buf.error.is_ok() {
            buf.flush();
            buf.written
        } else {
            -1
        }
    }
}

impl XmlNode {
    /// Dump an XML node, recursive behaviour, children are printed too.  
    ///
    /// Note that `format = 1` provide node indenting only if `xmlIndentTreeOutput = 1`
    /// or `xmlKeepBlanksDefault(0)` was called.
    #[doc(alias = "xmlNodeDumpOutput")]
    pub unsafe fn dump_output(
        &mut self,
        buf: Rc<RefCell<XmlOutputBuffer>>,
        doc: XmlDocPtr,
        level: i32,
        format: i32,
        mut encoding: Option<&str>,
    ) {
        xml_init_parser();

        if encoding.is_none() {
            encoding = Some("UTF-8");
        }

        let mut ctxt = XmlSaveCtxt {
            buf,
            level,
            format: (format != 0) as i32,
            encoding: encoding.map(|e| e.to_owned()),
            ..Default::default()
        };
        xml_save_ctxt_init(&mut ctxt);
        ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;

        #[cfg(feature = "html")]
        {
            let mut is_html = false;
            let dtd = if doc.is_null() {
                null_mut()
            } else {
                (*doc).get_int_subset()
            };
            if !dtd.is_null() {
                is_html = is_xhtml((*dtd).system_id.as_deref(), (*dtd).external_id.as_deref());
            }

            if is_html {
                xhtml_node_dump_output(&raw mut ctxt as _, self);
            } else {
                xml_node_dump_output_internal(&raw mut ctxt as _, self);
            }
        }
        #[cfg(not(feature = "html"))]
        {
            xml_node_dump_output_internal(addr_of_mut!(ctxt) as _, cur);
        }
        ctxt.buf.borrow_mut().flush();
    }

    /// Dump an XML/HTML node, recursive behaviour, children are printed too.
    #[doc(alias = "xmlElemDump")]
    pub unsafe fn dump_file(&mut self, f: *mut FILE, doc: XmlDocPtr) {
        xml_init_parser();

        let Some(mut outbuf) = XmlOutputBuffer::from_writer(f, None) else {
            return;
        };
        if !doc.is_null() && matches!((*doc).typ, XmlElementType::XmlHTMLDocumentNode) {
            #[cfg(feature = "html")]
            {
                html_node_dump_output(&mut outbuf, doc, self, null_mut());
            }
            #[cfg(not(feature = "html"))]
            {
                xmlSaveErr(
                    XmlParserErrors::XmlErrInternalError,
                    cur,
                    c"HTML support not compiled in\n".as_ptr() as _,
                );
            }
            outbuf.flush();
        } else {
            self.dump_output(Rc::new(RefCell::new(outbuf)), doc, 0, 1, None);
        }
    }

    /// Dump an XML node, recursive behaviour,children are printed too.
    ///
    /// Note that `format = 1` provide node indenting only if `xmlIndentTreeOutput = 1`
    /// or `xmlKeepBlanksDefault(0)` was called.
    ///
    /// Returns the number of bytes written to the buffer, in case of error 0
    /// is returned or `buf` stores the error.
    #[doc(alias = "xmlBufNodeDump")]
    pub unsafe fn dump_memory(
        &mut self,
        buf: XmlBufPtr,
        doc: XmlDocPtr,
        level: i32,
        format: i32,
    ) -> usize {
        xml_init_parser();

        if buf.is_null() {
            return usize::MAX;
        }
        let mut outbuf = XmlOutputBuffer::default();
        outbuf.buffer = XmlBufRef::from_raw(buf);
        outbuf.encoder = None;
        outbuf.writecallback = None;
        outbuf.closecallback = None;
        outbuf.context = null_mut();
        outbuf.written = 0;

        let using: usize = xml_buf_use(buf);
        let oldalloc: i32 = xml_buf_get_allocation_scheme(buf);
        xml_buf_set_allocation_scheme(buf, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
        let outbuf = Rc::new(RefCell::new(outbuf));
        self.dump_output(outbuf.clone(), doc, level, format, None);
        // The buffer should not be dropped because the caller may still use it.
        let _ = outbuf.borrow_mut().buffer.take();
        xml_buf_set_allocation_scheme(buf, oldalloc.try_into().unwrap());
        let ret: i32 = (xml_buf_use(buf) - using) as i32;
        ret as _
    }
}

// Copyright of the original code is the following.
// --------
// Summary: interfaces for tree manipulation
// Description: this module describes the structures found in an tree resulting
//              from an XML or HTML parsing, as well as the API provided for
//              various processing on that tree
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// tree.c : implementation of access function for an XML tree.
//
// References:
//   XHTML 1.0 W3C REC: http://www.w3.org/TR/2002/REC-xhtml1-20020801/
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{cell::RefCell, io::Write, mem::take, rc::Rc};

use crate::{
    encoding::find_encoding_handler,
    error::XmlParserErrors,
    io::XmlOutputBuffer,
    parser::xml_init_parser,
    save::{
        XmlSaveCtxt, XmlSaveOption, xml_node_dump_output_internal, xml_save_err,
        xml_save_err_memory,
    },
    tree::XmlNodePtr,
};
#[cfg(feature = "html")]
use crate::{html::tree::html_node_dump_output, save::xhtml_node_dump_output, tree::is_xhtml};

use super::{XmlDoc, XmlDocPtr, XmlElementType, XmlGenericNodePtr, XmlNode};

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
        txt: &mut Vec<u8>,
        txt_encoding: Option<&str>,
        format: i32,
    ) {
        unsafe {
            let mut ctxt = XmlSaveCtxt::default();

            // Validate the encoding value, if provided.
            // This logic is copied from xmlSaveFileEnc.
            let encoding = txt_encoding.map_or_else(
                || self.encoding.as_ref().map(|e| e.to_owned()),
                |e| Some(e.to_owned()),
            );
            let conv_hdlr = if let Some(encoding) = encoding.as_deref() {
                let Some(handler) = find_encoding_handler(encoding) else {
                    xml_save_err(
                        XmlParserErrors::XmlSaveUnknownEncoding,
                        XmlGenericNodePtr::from_raw(self),
                        Some(encoding),
                    );
                    return;
                };
                Some(handler)
            } else {
                None
            };

            let Some(out_buff) = XmlOutputBuffer::new(conv_hdlr) else {
                xml_save_err_memory("creating buffer");
                return;
            };

            ctxt.buf = out_buff;
            ctxt.level = 0;
            ctxt.format = (format != 0) as i32;
            ctxt.encoding = encoding;
            ctxt.init();
            ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;
            ctxt.doc_content_dump_output(XmlDocPtr::from_raw(self).unwrap().unwrap());
            ctxt.buf.flush();
            if ctxt.buf.encoder.is_some() {
                txt.extend(&ctxt.buf.conv);
            } else {
                txt.extend(&ctxt.buf.buffer);
            }
        }
    }

    /// Dump an XML document in memory and return the `*mut XmlChar` and it's size.  
    /// It's up to the caller to free the memory with xml_free().
    ///
    /// Note that `format = 1` provide node indenting only if `xmlIndentTreeOutput = 1`
    /// or `xmlKeepBlanksDefault(0)` was called
    #[doc(alias = "xmlDocDumpFormatMemory")]
    pub unsafe fn dump_format_memory(&mut self, mem: &mut Vec<u8>, format: i32) {
        unsafe {
            self.dump_format_memory_enc(mem, None, format);
        }
    }

    /// Dump an XML document in memory and return the `*mut XmlChar` and it's size in bytes.   
    /// It's up to the caller to free the memory with xml_free().
    ///
    /// The resulting byte array is zero terminated, though the last 0 is not
    /// included in the returned size.
    #[doc(alias = "xmlDocDumpMemory")]
    pub unsafe fn dump_memory(&mut self, mem: &mut Vec<u8>) {
        unsafe {
            self.dump_format_memory_enc(mem, None, 0);
        }
    }

    /// Dump the current DOM tree into memory using the character encoding specified by the caller.  
    ///
    /// Note it is up to the caller of this function to free the allocated memory with `xml_free()`.
    #[doc(alias = "xmlDocDumpMemoryEnc")]
    pub unsafe fn dump_memory_enc(&mut self, txt: &mut Vec<u8>, txt_encoding: Option<&str>) {
        unsafe {
            self.dump_format_memory_enc(txt, txt_encoding, 0);
        }
    }

    /// Dump an XML document to an open FILE.
    ///
    /// returns: the number of bytes written or -1 in case of failure.
    ///
    /// Note that `format = 1` provide node indenting only if `xmlIndentTreeOutput = 1`
    /// or `xmlKeepBlanksDefault(0)` was called
    #[doc(alias = "xmlDocFormatDump")]
    pub unsafe fn dump_format_file(&mut self, f: &mut impl Write, format: i32) -> i32 {
        unsafe {
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
            let Some(buf) = XmlOutputBuffer::from_writer(f, handler) else {
                return -1;
            };

            let mut ctxt = XmlSaveCtxt {
                buf,
                level: 0,
                format: (format != 0) as i32,
                encoding,
                handler: None,
                filename: None,
                ..Default::default()
            };
            ctxt.init();
            ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;
            ctxt.doc_content_dump_output(XmlDocPtr::from_raw(self).unwrap().unwrap());

            let XmlSaveCtxt { ref mut buf, .. } = ctxt;

            if buf.error.is_ok() {
                buf.flush();
                buf.written
            } else {
                -1
            }
        }
    }

    /// Dump an XML document to an open FILE.
    ///
    /// returns: the number of bytes written or -1 in case of failure.
    #[doc(alias = "xmlDocDump")]
    pub unsafe fn dump_file<'a>(&mut self, f: &mut (impl Write + 'a)) -> i32 {
        unsafe { self.dump_format_file(f, 0) }
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
        unsafe {
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
            let Some(buf) =
                XmlOutputBuffer::from_uri(filename, handler.map(|e| Rc::new(RefCell::new(e))))
            else {
                return -1;
            };
            let mut ctxt = XmlSaveCtxt {
                buf,
                level: 0,
                format: (format != 0) as i32,
                encoding,
                handler: None,
                filename: None,
                ..Default::default()
            };
            ctxt.init();
            ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;
            ctxt.doc_content_dump_output(XmlDocPtr::from_raw(self).unwrap().unwrap());

            let XmlSaveCtxt { ref mut buf, .. } = ctxt;
            if buf.error.is_ok() {
                buf.flush();
                buf.written
            } else {
                -1
            }
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
        unsafe { self.save_format_file_enc(filename, None, format) }
    }

    /// Dump an XML document, converting it to the given encoding.
    ///
    /// returns: the number of bytes written or -1 in case of failure.
    #[doc(alias = "xmlSaveFileEnc")]
    pub unsafe fn save_file_enc(&mut self, filename: &str, encoding: Option<&str>) -> i32 {
        unsafe { self.save_format_file_enc(filename, encoding, 0) }
    }

    /// Dump an XML document to a file. Will use compression if compiled in and enabled.  
    ///
    /// If `filename` is "-" the stdout file is used.  
    /// returns: the number of bytes written or -1 in case of failure.
    #[doc(alias = "xmlSaveFile")]
    pub unsafe fn save_file(&mut self, filename: &str) -> i32 {
        unsafe { self.save_format_file_enc(filename, None, 0) }
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
        unsafe {
            if !matches!(
                self.typ,
                XmlElementType::XmlDocumentNode | XmlElementType::XmlHTMLDocumentNode
            ) {
                return -1;
            }
            let mut ctxt = XmlSaveCtxt {
                buf,
                level: 0,
                format: (format != 0) as i32,
                encoding: encoding.map(|e| e.to_owned()),
                handler: None,
                filename: None,
                ..Default::default()
            };
            ctxt.init();
            ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;
            ctxt.doc_content_dump_output(XmlDocPtr::from_raw(self).unwrap().unwrap());

            let XmlSaveCtxt { ref mut buf, .. } = ctxt;
            if buf.error.is_ok() {
                buf.flush();
                buf.written
            } else {
                -1
            }
        }
    }

    /// Dump an XML document to an I/O buffer.
    ///
    /// returns: the number of bytes written or -1 in case of failure.
    ///
    /// # Warning
    ///  This call xmlOutputBufferClose() on buf which is not available after this call.
    #[doc(alias = "xmlSaveFileTo")]
    pub unsafe fn save_file_to(&mut self, buf: XmlOutputBuffer, encoding: Option<&str>) -> i32 {
        unsafe {
            let mut ctxt = XmlSaveCtxt {
                buf,
                level: 0,
                format: 0,
                encoding: encoding.map(|e| e.to_owned()),
                handler: None,
                filename: None,
                ..Default::default()
            };
            ctxt.init();
            ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;
            ctxt.doc_content_dump_output(XmlDocPtr::from_raw(self).unwrap().unwrap());

            let XmlSaveCtxt { ref mut buf, .. } = ctxt;
            if buf.error.is_ok() {
                buf.flush();
                buf.written
            } else {
                -1
            }
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
        buf: XmlOutputBuffer,
        #[cfg_attr(not(feature = "html"), allow(unused))] doc: Option<XmlDocPtr>,
        level: i32,
        format: i32,
        mut encoding: Option<&str>,
    ) {
        unsafe {
            xml_init_parser();

            if encoding.is_none() {
                encoding = Some("UTF-8");
            }

            let mut ctxt = XmlSaveCtxt {
                buf,
                level,
                format: (format != 0) as i32,
                encoding: encoding.map(|e| e.to_owned()),
                handler: None,
                filename: None,
                ..Default::default()
            };
            ctxt.init();
            ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;

            #[cfg(feature = "html")]
            {
                let mut is_html = false;
                let dtd = doc.and_then(|doc| doc.get_int_subset());
                if let Some(dtd) = dtd {
                    is_html = is_xhtml(dtd.system_id.as_deref(), dtd.external_id.as_deref());
                }

                if is_html {
                    xhtml_node_dump_output(
                        &mut ctxt,
                        XmlNodePtr::from_raw(self).unwrap().unwrap().into(),
                    );
                } else {
                    xml_node_dump_output_internal(
                        &mut ctxt as _,
                        XmlNodePtr::from_raw(self).unwrap().unwrap().into(),
                    );
                }
            }
            #[cfg(not(feature = "html"))]
            {
                xml_node_dump_output_internal(
                    &mut ctxt as _,
                    XmlNodePtr::from_raw(self).unwrap().unwrap().into(),
                );
            }
            ctxt.buf.flush();
        }
    }

    /// Dump an XML/HTML node, recursive behaviour, children are printed too.
    #[doc(alias = "xmlElemDump")]
    pub unsafe fn dump_file<'a>(&mut self, f: &mut (impl Write + 'a), doc: Option<XmlDocPtr>) {
        unsafe {
            xml_init_parser();

            let Some(mut outbuf) = XmlOutputBuffer::from_writer(f, None) else {
                return;
            };
            #[cfg_attr(not(feature = "html"), allow(unused))]
            if let Some(doc) =
                doc.filter(|doc| matches!(doc.typ, XmlElementType::XmlHTMLDocumentNode))
            {
                #[cfg(feature = "html")]
                {
                    html_node_dump_output(
                        &mut outbuf,
                        Some(doc),
                        XmlGenericNodePtr::from_raw(self).unwrap(),
                        None,
                    );
                }
                #[cfg(not(feature = "html"))]
                {
                    xml_save_err(
                        XmlParserErrors::XmlErrInternalError,
                        XmlGenericNodePtr::from_raw(self),
                        Some("HTML support not compiled in\n"),
                    );
                }
                outbuf.flush();
            } else {
                self.dump_output(outbuf, doc, 0, 1, None);
            }
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
        buf: &mut Vec<u8>,
        doc: Option<XmlDocPtr>,
        level: i32,
        format: i32,
    ) -> usize {
        unsafe {
            xml_init_parser();

            let Some(outbuf) = XmlOutputBuffer::from_writer(&mut *buf, None) else {
                return usize::MAX;
            };

            self.dump_output(outbuf, doc, level, format, None);
            buf.len()
        }
    }
}

impl XmlGenericNodePtr {
    /// Dump an XML node, recursive behaviour, children are printed too.  
    ///
    /// Note that `format = 1` provide node indenting only if `xmlIndentTreeOutput = 1`
    /// or `xmlKeepBlanksDefault(0)` was called.
    #[doc(alias = "xmlNodeDumpOutput")]
    pub unsafe fn dump_output<'a>(
        self,
        buf: XmlOutputBuffer<'a>,
        #[cfg_attr(not(feature = "html"), allow(unused))] doc: Option<XmlDocPtr>,
        level: i32,
        format: i32,
        mut encoding: Option<&str>,
    ) -> XmlOutputBuffer<'a> {
        unsafe {
            xml_init_parser();

            if encoding.is_none() {
                encoding = Some("UTF-8");
            }

            let mut ctxt = XmlSaveCtxt {
                buf,
                level,
                format: (format != 0) as i32,
                encoding: encoding.map(|e| e.to_owned()),
                handler: None,
                filename: None,
                ..Default::default()
            };
            ctxt.init();
            ctxt.options |= XmlSaveOption::XmlSaveAsXML as i32;

            #[cfg(feature = "html")]
            {
                let mut is_html = false;
                let dtd = doc.and_then(|doc| doc.get_int_subset());
                if let Some(dtd) = dtd {
                    is_html = is_xhtml(dtd.system_id.as_deref(), dtd.external_id.as_deref());
                }

                if is_html {
                    xhtml_node_dump_output(&mut ctxt, self);
                } else {
                    xml_node_dump_output_internal(&mut ctxt as _, self);
                }
            }
            #[cfg(not(feature = "html"))]
            {
                xml_node_dump_output_internal(&mut ctxt as _, self);
            }
            ctxt.buf.flush();
            take(&mut ctxt.buf)
        }
    }

    /// Dump an XML/HTML node, recursive behaviour, children are printed too.
    #[doc(alias = "xmlElemDump")]
    pub unsafe fn dump_file<'a>(self, f: &mut (impl Write + 'a), doc: Option<XmlDocPtr>) {
        unsafe {
            xml_init_parser();

            let Some(mut outbuf) = XmlOutputBuffer::from_writer(f, None) else {
                return;
            };
            #[cfg_attr(not(feature = "html"), allow(unused))]
            if let Some(doc) =
                doc.filter(|doc| matches!(doc.typ, XmlElementType::XmlHTMLDocumentNode))
            {
                #[cfg(feature = "html")]
                {
                    html_node_dump_output(&mut outbuf, Some(doc), self, None);
                }
                #[cfg(not(feature = "html"))]
                {
                    xml_save_err(
                        XmlParserErrors::XmlErrInternalError,
                        Some(self),
                        Some("HTML support not compiled in\n"),
                    );
                }
                outbuf.flush();
            } else {
                self.dump_output(outbuf, doc, 0, 1, None);
            }
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
        self,
        buf: &mut Vec<u8>,
        doc: Option<XmlDocPtr>,
        level: i32,
        format: i32,
    ) -> usize {
        unsafe {
            xml_init_parser();

            let Some(outbuf) = XmlOutputBuffer::from_writer(&mut *buf, None) else {
                return usize::MAX;
            };

            self.dump_output(outbuf, doc, level, format, None);
            buf.len()
        }
    }
}

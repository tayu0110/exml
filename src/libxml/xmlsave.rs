//! Provide methods and data structures for serializing XML documents.  
//! This module is based on `libxml/xmlsave.h`, `xmlsave.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, c_long, c_uchar, CString},
    mem::size_of,
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::Ordering,
};

use libc::{memcpy, memset};

use crate::{
    libxml::{
        encoding::{
            xml_char_enc_close_func, xml_find_char_encoding_handler, xml_parse_char_encoding,
            XmlCharEncoding, XmlCharEncodingHandlerPtr, XmlCharEncodingOutputFunc,
        },
        entities::{xml_dump_entity_decl, XmlEntityPtr},
        globals::{
            xml_free, xml_generic_error_context, xml_indent_tree_output, xml_malloc,
            xml_save_no_empty_tags, xml_tree_indent_string,
        },
        htmltree::{
            html_doc_content_dump_format_output, html_get_meta_encoding, html_set_meta_encoding,
        },
        parser::xml_init_parser,
        parser_internals::XML_STRING_TEXT_NOENC,
        tree::{
            xml_buffer_create, xml_buffer_set_allocation_scheme, xml_get_int_subset, xml_is_xhtml,
            XmlAttrPtr, XmlAttributePtr, XmlBufPtr, XmlBufferAllocationScheme, XmlBufferPtr,
            XmlDocPtr, XmlDtdPtr, XmlElementPtr, XmlElementType, XmlNodePtr, XmlNsPtr,
            XML_LOCAL_NAMESPACE,
        },
        valid::{
            xml_dump_attribute_decl, xml_dump_element_decl, xml_dump_notation_table,
            XmlNotationTablePtr,
        },
        xml_io::{
            xml_output_buffer_close, xml_output_buffer_create_buffer, xml_output_buffer_create_fd,
            xml_output_buffer_create_filename, xml_output_buffer_create_io,
            xml_output_buffer_flush, xml_output_buffer_write, xml_output_buffer_write_escape,
            xml_output_buffer_write_string, XmlOutputBufferPtr, XmlOutputCloseCallback,
            XmlOutputWriteCallback,
        },
        xmlerror::{XmlErrorDomain, XmlParserErrors},
        xmlstring::{xml_str_equal, xml_strdup, xml_strlen, XmlChar},
    },
    private::{
        buf::{
            xml_buf_add, xml_buf_create, xml_buf_free, xml_buf_merge_buffer,
            xml_buf_write_quoted_string,
        },
        enc::xml_char_enc_output,
        error::__xml_simple_error,
        save::xml_buf_attr_serialize_txt_content,
    },
    xml_generic_error, IS_BYTE_CHAR, IS_CHAR,
};

const MAX_INDENT: usize = 60;

/**
 * xmlSaveOption:
 *
 * This is the set of XML save options that can be passed down
 * to the xmlSaveToFd() and similar calls.
 */
#[repr(C)]
pub enum XmlSaveOption {
    XmlSaveFormat = 1 << 0,   /* format save output */
    XmlSaveNoDecl = 1 << 1,   /* drop the xml declaration */
    XmlSaveNoEmpty = 1 << 2,  /* no empty tags */
    XmlSaveNoXhtml = 1 << 3,  /* disable XHTML1 specific rules */
    XmlSaveXhtml = 1 << 4,    /* force XHTML1 specific rules */
    XmlSaveAsXml = 1 << 5,    /* force XML serialization on HTML doc */
    XmlSaveAsHtml = 1 << 6,   /* force HTML serialization on XML doc */
    XmlSaveWsnonsig = 1 << 7, /* format with non-significant whitespace */
}

pub type XmlSaveCtxtPtr = *mut XmlSaveCtxt;
#[repr(C)]
pub struct XmlSaveCtxt {
    pub(crate) _private: *mut c_void,
    pub(crate) typ: c_int,
    pub(crate) fd: c_int,
    pub(crate) filename: *const XmlChar,
    pub(crate) encoding: *const XmlChar,
    pub(crate) handler: XmlCharEncodingHandlerPtr,
    pub(crate) buf: XmlOutputBufferPtr,
    pub(crate) options: c_int,
    pub(crate) level: c_int,
    pub(crate) format: c_int,
    pub(crate) indent: [c_char; MAX_INDENT + 1], /* array for indenting output */
    pub(crate) indent_nr: c_int,
    pub(crate) indent_size: c_int,
    pub(crate) escape: Option<XmlCharEncodingOutputFunc>, /* used for element content */
    pub(crate) escape_attr: Option<XmlCharEncodingOutputFunc>, /* used for attribute content */
}

/**
 * xmlSaveErr:
 * @code:  the error number
 * @node:  the location of the error.
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
pub(crate) unsafe extern "C" fn xml_save_err(code: c_int, node: XmlNodePtr, extra: *const c_char) {
    let msg = match XmlParserErrors::try_from(code) {
        Ok(XmlParserErrors::XmlSaveNotUtf8) => c"string is not in UTF-8\n".as_ptr() as _,
        Ok(XmlParserErrors::XmlSaveCharInvalid) => c"invalid character value\n".as_ptr() as _,
        Ok(XmlParserErrors::XmlSaveUnknownEncoding) => c"unknown encoding %s\n".as_ptr() as _,
        Ok(XmlParserErrors::XmlSaveNoDoctype) => c"document has no DOCTYPE\n".as_ptr() as _,
        _ => c"unexpected error number\n".as_ptr() as _,
    };
    __xml_simple_error(XmlErrorDomain::XmlFromOutput as i32, code, node, msg, extra);
}

/**
 * xmlSaveErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
pub(crate) unsafe extern "C" fn xml_save_err_memory(extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromOutput as i32,
        XmlParserErrors::XmlErrNoMemory as i32,
        null_mut(),
        null(),
        extra,
    );
}

pub(crate) unsafe extern "C" fn xmlSerializeHexCharRef(
    mut out: *mut c_uchar,
    mut val: c_int,
) -> *mut c_uchar {
    let mut ptr: *mut c_uchar;

    *out = b'&';
    out = out.add(1);
    *out = b'#';
    out = out.add(1);
    *out = b'x';
    out = out.add(1);
    if val < 0x10 {
        ptr = out;
    } else if val < 0x100 {
        ptr = out.add(1);
    } else if val < 0x1000 {
        ptr = out.add(2);
    } else if val < 0x10000 {
        ptr = out.add(3);
    } else if val < 0x100000 {
        ptr = out.add(4);
    } else {
        ptr = out.add(5);
    }
    out = ptr.add(1);
    while val > 0 {
        match val & 0xF {
            0 => {
                *ptr = b'0';
                ptr = ptr.sub(1);
            }
            1 => {
                *ptr = b'1';
                ptr = ptr.sub(1);
            }
            2 => {
                *ptr = b'2';
                ptr = ptr.sub(1);
            }
            3 => {
                *ptr = b'3';
                ptr = ptr.sub(1);
            }
            4 => {
                *ptr = b'4';
                ptr = ptr.sub(1);
            }
            5 => {
                *ptr = b'5';
                ptr = ptr.sub(1);
            }
            6 => {
                *ptr = b'6';
                ptr = ptr.sub(1);
            }
            7 => {
                *ptr = b'7';
                ptr = ptr.sub(1);
            }
            8 => {
                *ptr = b'8';
                ptr = ptr.sub(1);
            }
            9 => {
                *ptr = b'9';
                ptr = ptr.sub(1);
            }
            0xA => {
                *ptr = b'A';
                ptr = ptr.sub(1);
            }
            0xB => {
                *ptr = b'B';
                ptr = ptr.sub(1);
            }
            0xC => {
                *ptr = b'C';
                ptr = ptr.sub(1);
            }
            0xD => {
                *ptr = b'D';
                ptr = ptr.sub(1);
            }
            0xE => {
                *ptr = b'E';
                ptr = ptr.sub(1);
            }
            0xF => {
                *ptr = b'F';
                ptr = ptr.sub(1);
            }
            _ => {
                *ptr = b'0';
                ptr = ptr.sub(1);
            }
        }
        val >>= 4;
    }
    *out = b';';
    out = out.add(1);
    *out = 0;
    out
}

/**
 * xmlEscapeEntities:
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @in:  a pointer to an array of unescaped UTF-8 bytes
 * @inlen:  the length of @in
 *
 * Take a block of UTF-8 chars in and escape them. Used when there is no
 * encoding specified.
 *
 * Returns 0 if success, or -1 otherwise
 * The value of @inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictable.
 * The value of @outlen after return is the number of octets consumed.
 */
unsafe extern "C" fn xmlEscapeEntities(
    mut out: *mut c_uchar,
    outlen: *mut c_int,
    mut input: *const XmlChar,
    inlen: *mut c_int,
) -> c_int {
    let outstart: *mut c_uchar = out;
    let base: *const c_uchar = input;
    let outend: *mut c_uchar = out.add(*outlen as usize);

    let mut val: c_int;

    let inend: *const c_uchar = input.add(*inlen as usize);

    while (input < inend) && (out < outend) {
        if *input == b'<' {
            if outend.offset_from(out) < 4 {
                break;
            }
            *out = b'&';
            out = out.add(1);
            *out = b'l';
            out = out.add(1);
            *out = b't';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
            input = input.add(1);
            continue;
        } else if *input == b'>' {
            if outend.offset_from(out) < 4 {
                break;
            }
            *out = b'&';
            out = out.add(1);
            *out = b'g';
            out = out.add(1);
            *out = b't';
            out = out.add(1);
            *out = b';';
            out = out.add(1);
            input = input.add(1);
            continue;
        } else if *input == b'&' {
            if outend.offset_from(out) < 5 {
                break;
            }
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
            input = input.add(1);
            continue;
        } else if (*input >= 0x20 && *input < 0x80) || *input == b'\n' || *input == b'\t' {
            /*
             * default case, just copy !
             */
            *out = *input;
            out = out.add(1);
            input = input.add(1);
            continue;
        } else if *input >= 0x80 {
            /*
             * We assume we have UTF-8 input.
             */
            if outend.offset_from(out) < 11 {
                break;
            }

            if *input < 0xC0 {
                xml_save_err(
                    XmlParserErrors::XmlSaveNotUtf8 as i32,
                    null_mut(),
                    null_mut(),
                );
                input = input.add(1);
                // goto error;
                *outlen = out.offset_from(outstart) as _;
                *inlen = input.offset_from(base) as _;
                return -1;
            } else if *input < 0xE0 {
                if inend.offset_from(input) < 2 {
                    break;
                }
                val = *input.add(0) as i32 & 0x1F;
                val <<= 6;
                val |= *input.add(1) as i32 & 0x3F;
                input = input.add(2);
            } else if *input < 0xF0 {
                if inend.offset_from(input) < 3 {
                    break;
                }
                val = *input.add(0) as i32 & 0x0F;
                val <<= 6;
                val |= *input.add(1) as i32 & 0x3F;
                val <<= 6;
                val |= *input.add(2) as i32 & 0x3F;
                input = input.add(3);
            } else if *input < 0xF8 {
                if inend.offset_from(input) < 4 {
                    break;
                }
                val = *input.add(0) as i32 & 0x07;
                val <<= 6;
                val |= *input.add(1) as i32 & 0x3F;
                val <<= 6;
                val |= *input.add(2) as i32 & 0x3F;
                val <<= 6;
                val |= *input.add(3) as i32;
                input = input.add(4);
            } else {
                xml_save_err(
                    XmlParserErrors::XmlSaveCharInvalid as i32,
                    null_mut(),
                    null_mut(),
                );
                input = input.add(1);
                // goto error;
                *outlen = out.offset_from(outstart) as _;
                *inlen = input.offset_from(base) as _;
                return -1;
            }
            if !IS_CHAR!(val) {
                xml_save_err(
                    XmlParserErrors::XmlSaveCharInvalid as i32,
                    null_mut(),
                    null_mut(),
                );
                input = input.add(1);
                // goto error;
                *outlen = out.offset_from(outstart) as _;
                *inlen = input.offset_from(base) as _;
                return -1;
            }

            /*
             * We could do multiple things here. Just save as a c_char ref
             */
            out = xmlSerializeHexCharRef(out, val);
        } else if IS_BYTE_CHAR!(*input) {
            if outend.offset_from(out) < 6 {
                break;
            }
            out = xmlSerializeHexCharRef(out, *input as _);
            input = input.add(1);
        } else {
            xml_generic_error!(
                xml_generic_error_context(),
                c"xmlEscapeEntities : char out of range\n".as_ptr() as _
            );
            input = input.add(1);
            // goto error;
            *outlen = out.offset_from(outstart) as _;
            *inlen = input.offset_from(base) as _;
            return -1;
        }
    }
    *outlen = out.offset_from(outstart) as _;
    *inlen = input.offset_from(base) as _;
    0
    // error:
    //     *outlen = out.offset_from(outstart) as _;
    //     *inlen = input.offset_from(base) as _;
    //     return -1;
}

/**
 * xmlSaveCtxtInit:
 * @ctxt: the saving context
 *
 * Initialize a saving context
 */
pub(crate) unsafe extern "C" fn xmlSaveCtxtInit(ctxt: XmlSaveCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    if (*ctxt).encoding.is_null() && (*ctxt).escape.is_none() {
        (*ctxt).escape = Some(xmlEscapeEntities);
    }
    let len: c_int = xml_strlen(*xml_tree_indent_string() as _) as _;
    if xml_tree_indent_string().is_null() || len == 0 {
        memset(addr_of_mut!((*ctxt).indent[0]) as _, 0, MAX_INDENT + 1);
    } else {
        (*ctxt).indent_size = len;
        (*ctxt).indent_nr = MAX_INDENT as i32 / (*ctxt).indent_size;
        for i in 0..(*ctxt).indent_nr {
            memcpy(
                (*ctxt)
                    .indent
                    .as_mut_ptr()
                    .add(i as usize * (*ctxt).indent_size as usize) as _,
                *xml_tree_indent_string() as _,
                (*ctxt).indent_size as _,
            );
        }
        (*ctxt).indent[(*ctxt).indent_nr as usize * (*ctxt).indent_size as usize] = 0;
    }

    if *xml_save_no_empty_tags() != 0 {
        (*ctxt).options |= XmlSaveOption::XmlSaveNoEmpty as i32;
    }
}

unsafe extern "C" fn xmlSaveSwitchEncoding(ctxt: XmlSaveCtxtPtr, encoding: *const c_char) -> c_int {
    let buf: XmlOutputBufferPtr = (*ctxt).buf;

    if !encoding.is_null() && (*buf).encoder.is_null() && (*buf).conv.is_null() {
        (*buf).encoder = xml_find_char_encoding_handler(encoding);
        if (*buf).encoder.is_null() {
            xml_save_err(
                XmlParserErrors::XmlSaveUnknownEncoding as i32,
                null_mut(),
                encoding,
            );
            return -1;
        }
        (*buf).conv = xml_buf_create();
        if (*buf).conv.is_null() {
            xml_char_enc_close_func((*buf).encoder);
            xml_save_err_memory(c"creating encoding buffer".as_ptr() as _);
            return -1;
        }
        /*
         * initialize the state, e.g. if outputting a BOM
         */
        xml_char_enc_output(buf, 1);
    }
    0
}

/**
 * xmlOutputBufferWriteWSNonSig:
 * @ctxt:  The save context
 * @extra: Number of extra indents to apply to (*ctxt).level
 *
 * Write out formatting for non-significant whitespace output.
 */
unsafe extern "C" fn xmlOutputBufferWriteWSNonSig(ctxt: XmlSaveCtxtPtr, extra: c_int) {
    if ctxt.is_null() || (*ctxt).buf.is_null() {
        return;
    }
    xml_output_buffer_write((*ctxt).buf, 1, c"\n".as_ptr() as _);
    for i in (0..(*ctxt).level + extra).step_by((*ctxt).indent_nr as usize) {
        xml_output_buffer_write(
            (*ctxt).buf,
            (*ctxt).indent_size
                * if (*ctxt).level + extra - i > (*ctxt).indent_nr {
                    (*ctxt).indent_nr
                } else {
                    (*ctxt).level + extra - i
                },
            (*ctxt).indent.as_ptr(),
        );
    }
}

/**
 * xmlNsDumpOutput:
 * @buf:  the XML buffer output
 * @cur:  a namespace
 * @ctxt: the output save context. Optional.
 *
 * Dump a local Namespace definition.
 * Should be called in the context of attributes dumps.
 * If @ctxt is supplied, @buf should be its buffer.
 */
pub(crate) unsafe extern "C" fn xmlNsDumpOutput(
    buf: XmlOutputBufferPtr,
    cur: XmlNsPtr,
    ctxt: XmlSaveCtxtPtr,
) {
    if cur.is_null() || buf.is_null() {
        return;
    }
    if matches!((*cur).typ, Some(XML_LOCAL_NAMESPACE))
        && !(*cur).href.load(Ordering::Relaxed).is_null()
    {
        if xml_str_equal((*cur).prefix.load(Ordering::Relaxed), c"xml".as_ptr() as _) != 0 {
            return;
        }

        if !ctxt.is_null() && (*ctxt).format == 2 {
            xmlOutputBufferWriteWSNonSig(ctxt, 2);
        } else {
            xml_output_buffer_write(buf, 1, c" ".as_ptr() as _);
        }

        /* Within the context of an element attributes */
        if !(*cur).prefix.load(Ordering::Relaxed).is_null() {
            xml_output_buffer_write(buf, 6, c"xmlns:".as_ptr() as _);
            xml_output_buffer_write_string(buf, (*cur).prefix.load(Ordering::Relaxed) as _);
        } else {
            xml_output_buffer_write(buf, 5, c"xmlns".as_ptr() as _);
        }
        xml_output_buffer_write(buf, 1, c"=".as_ptr() as _);
        xml_buf_write_quoted_string((*buf).buffer, (*cur).href.load(Ordering::Relaxed));
    }
}

/**
 * xmlBufDumpNotationTable:
 * @buf:  an xmlBufPtr output
 * @table:  A notation table
 *
 * This will dump the content of the notation table as an XML DTD definition
 */
unsafe extern "C" fn xmlBufDumpNotationTable(buf: XmlBufPtr, table: XmlNotationTablePtr) {
    let buffer: XmlBufferPtr = xml_buffer_create();
    if buffer.is_null() {
        /*
         * TODO set the error in buf
         */
        return;
    }
    xml_buffer_set_allocation_scheme(buffer, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_dump_notation_table(buffer, table);
    xml_buf_merge_buffer(buf, buffer);
}

/**
 * xmlBufDumpElementDecl:
 * @buf:  an xmlBufPtr output
 * @elem:  An element table
 *
 * This will dump the content of the element declaration as an XML
 * DTD definition
 */
unsafe extern "C" fn xmlBufDumpElementDecl(buf: XmlBufPtr, elem: XmlElementPtr) {
    let buffer: XmlBufferPtr = xml_buffer_create();
    if buffer.is_null() {
        /*
         * TODO set the error in buf
         */
        return;
    }
    xml_buffer_set_allocation_scheme(buffer, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_dump_element_decl(buffer, elem);
    xml_buf_merge_buffer(buf, buffer);
}

/**
 * xmlBufDumpAttributeDecl:
 * @buf:  an xmlBufPtr output
 * @attr:  An attribute declaration
 *
 * This will dump the content of the attribute declaration as an XML
 * DTD definition
 */
unsafe extern "C" fn xmlBufDumpAttributeDecl(buf: XmlBufPtr, attr: XmlAttributePtr) {
    let buffer: XmlBufferPtr = xml_buffer_create();
    if buffer.is_null() {
        /*
         * TODO set the error in buf
         */
        return;
    }
    xml_buffer_set_allocation_scheme(buffer, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_dump_attribute_decl(buffer, attr);
    xml_buf_merge_buffer(buf, buffer);
}

/**
 * xmlBufDumpEntityDecl:
 * @buf:  an xmlBufPtr output
 * @ent:  An entity table
 *
 * This will dump the content of the entity table as an XML DTD definition
 */
unsafe extern "C" fn xmlBufDumpEntityDecl(buf: XmlBufPtr, ent: XmlEntityPtr) {
    let buffer: XmlBufferPtr = xml_buffer_create();
    if buffer.is_null() {
        /*
         * TODO set the error in buf
         */
        return;
    }
    xml_buffer_set_allocation_scheme(buffer, XmlBufferAllocationScheme::XmlBufferAllocDoubleit);
    xml_dump_entity_decl(buffer, ent);
    xml_buf_merge_buffer(buf, buffer);
}

/**
 * xmlNsListDumpOutputCtxt
 * @ctxt: the save context
 * @cur:  the first namespace
 *
 * Dump a list of local namespace definitions to a save context.
 * Should be called in the context of attribute dumps.
 */
unsafe extern "C" fn xmlNsListDumpOutputCtxt(ctxt: XmlSaveCtxtPtr, mut cur: XmlNsPtr) {
    while !cur.is_null() {
        xmlNsDumpOutput((*ctxt).buf, cur, ctxt);
        cur = (*cur).next.load(Ordering::Relaxed);
    }
}

/**
 * xmlAttrSerializeContent:
 * @buf:  the XML buffer output
 * @doc:  the document
 * @attr:  the attribute pointer
 *
 * Serialize the attribute in the buffer
 */
unsafe extern "C" fn xmlAttrSerializeContent(buf: XmlOutputBufferPtr, attr: XmlAttrPtr) {
    let mut children: XmlNodePtr;

    children = (*attr).children;
    while !children.is_null() {
        match (*children).typ {
            XmlElementType::XmlTextNode => {
                xml_buf_attr_serialize_txt_content(
                    (*buf).buffer,
                    (*attr).doc,
                    attr,
                    (*children).content,
                );
            }
            XmlElementType::XmlEntityRefNode => {
                xml_buf_add((*buf).buffer, c"&".as_ptr() as _, 1);
                xml_buf_add(
                    (*buf).buffer,
                    (*children).name,
                    xml_strlen((*children).name),
                );
                xml_buf_add((*buf).buffer, c";".as_ptr() as _, 1);
            }
            _ => { /* should not happen unless we have a badly built tree */ }
        }
        children = (*children).next;
    }
}

/**
 * xmlAttrDumpOutput:
 * @buf:  the XML buffer output
 * @cur:  the attribute pointer
 *
 * Dump an XML attribute
 */
unsafe extern "C" fn xmlAttrDumpOutput(ctxt: XmlSaveCtxtPtr, cur: XmlAttrPtr) {
    if cur.is_null() {
        return;
    }
    let buf: XmlOutputBufferPtr = (*ctxt).buf;
    if buf.is_null() {
        return;
    }
    if (*ctxt).format == 2 {
        xmlOutputBufferWriteWSNonSig(ctxt, 2);
    } else {
        xml_output_buffer_write(buf, 1, c" ".as_ptr() as _);
    }
    if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
        xml_output_buffer_write_string(buf, (*(*cur).ns).prefix.load(Ordering::Relaxed) as _);
        xml_output_buffer_write(buf, 1, c":".as_ptr() as _);
    }
    xml_output_buffer_write_string(buf, (*cur).name as _);
    xml_output_buffer_write(buf, 2, c"=\"".as_ptr() as _);
    xmlAttrSerializeContent(buf, cur);
    xml_output_buffer_write(buf, 1, c"\"".as_ptr() as _);
}

/**
 * xmlNodeDumpOutputInternal:
 * @cur:  the current node
 *
 * Dump an XML node, recursive behaviour, children are printed too.
 */
pub(crate) unsafe extern "C" fn xmlNodeDumpOutputInternal(
    ctxt: XmlSaveCtxtPtr,
    mut cur: XmlNodePtr,
) {
    let format: c_int = (*ctxt).format;
    let mut tmp: XmlNodePtr;

    let mut unformatted_node: XmlNodePtr = null_mut();
    let mut parent: XmlNodePtr;
    let mut attr: XmlAttrPtr;
    let mut start: *mut XmlChar;
    let mut end: *mut XmlChar;

    if cur.is_null() {
        return;
    }
    let buf: XmlOutputBufferPtr = (*ctxt).buf;

    let root: XmlNodePtr = cur;
    parent = (*cur).parent;
    loop {
        match (*cur).typ {
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode => {
                xmlDocContentDumpOutput(ctxt, cur as _);
            }
            XmlElementType::XmlDtdNode => {
                xmlDtdDumpOutput(ctxt, cur as _);
            }
            XmlElementType::XmlDocumentFragNode => {
                /* Always validate (*cur).parent when descending. */
                if (*cur).parent == parent && !(*cur).children.is_null() {
                    parent = cur;
                    cur = (*cur).children;
                    continue;
                }
            }
            XmlElementType::XmlElementDecl => {
                xmlBufDumpElementDecl((*buf).buffer, cur as _);
            }
            XmlElementType::XmlAttributeDecl => {
                xmlBufDumpAttributeDecl((*buf).buffer, cur as _);
            }
            XmlElementType::XmlEntityDecl => {
                xmlBufDumpEntityDecl((*buf).buffer, cur as _);
            }
            XmlElementType::XmlElementNode => {
                if cur != root && (*ctxt).format == 1 && *xml_indent_tree_output() != 0 {
                    xml_output_buffer_write(
                        buf,
                        (*ctxt).indent_size
                            * if (*ctxt).level > (*ctxt).indent_nr {
                                (*ctxt).indent_nr
                            } else {
                                (*ctxt).level
                            },
                        (*ctxt).indent.as_ptr(),
                    );
                }

                /*
                 * Some users like lxml are known to pass nodes with a corrupted
                 * tree structure. Fall back to a recursive call to handle this
                 * case.
                 */
                if (*cur).parent != parent && !(*cur).children.is_null() {
                    xmlNodeDumpOutputInternal(ctxt, cur);
                } else {
                    xml_output_buffer_write(buf, 1, c"<".as_ptr() as _);
                    if !(*cur).ns.is_null()
                        && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null()
                    {
                        xml_output_buffer_write_string(
                            buf,
                            (*(*cur).ns).prefix.load(Ordering::Relaxed) as _,
                        );
                        xml_output_buffer_write(buf, 1, c":".as_ptr() as _);
                    }
                    xml_output_buffer_write_string(buf, (*cur).name as _);
                    if !(*cur).ns_def.is_null() {
                        xmlNsListDumpOutputCtxt(ctxt, (*cur).ns_def);
                    }
                    attr = (*cur).properties;
                    while !attr.is_null() {
                        xmlAttrDumpOutput(ctxt, attr);
                        attr = (*attr).next;
                    }

                    if (*cur).children.is_null() {
                        if (*ctxt).options & XmlSaveOption::XmlSaveNoEmpty as i32 == 0 {
                            if (*ctxt).format == 2 {
                                xmlOutputBufferWriteWSNonSig(ctxt, 0);
                            }
                            xml_output_buffer_write(buf, 2, c"/>".as_ptr() as _);
                        } else {
                            if (*ctxt).format == 2 {
                                xmlOutputBufferWriteWSNonSig(ctxt, 1);
                            }
                            xml_output_buffer_write(buf, 3, c"></".as_ptr() as _);
                            if !(*cur).ns.is_null()
                                && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null()
                            {
                                xml_output_buffer_write_string(
                                    buf,
                                    (*(*cur).ns).prefix.load(Ordering::Relaxed) as _,
                                );
                                xml_output_buffer_write(buf, 1, c":".as_ptr() as _);
                            }
                            xml_output_buffer_write_string(buf, (*cur).name as _);
                            if (*ctxt).format == 2 {
                                xmlOutputBufferWriteWSNonSig(ctxt, 0);
                            }
                            xml_output_buffer_write(buf, 1, c">".as_ptr() as _);
                        }
                    } else {
                        if (*ctxt).format == 1 {
                            tmp = (*cur).children;
                            while !tmp.is_null() {
                                if matches!(
                                    (*tmp).typ,
                                    XmlElementType::XmlTextNode
                                        | XmlElementType::XmlCdataSectionNode
                                        | XmlElementType::XmlEntityRefNode
                                ) {
                                    (*ctxt).format = 0;
                                    unformatted_node = cur;
                                    break;
                                }
                                tmp = (*tmp).next;
                            }
                        }
                        if (*ctxt).format == 2 {
                            xmlOutputBufferWriteWSNonSig(ctxt, 1);
                        }
                        xml_output_buffer_write(buf, 1, c">".as_ptr() as _);
                        if (*ctxt).format == 1 {
                            xml_output_buffer_write(buf, 1, c"\n".as_ptr() as _);
                        }
                        if (*ctxt).level >= 0 {
                            (*ctxt).level += 1;
                        }
                        parent = cur;
                        cur = (*cur).children;
                        continue;
                    }
                }
            }
            XmlElementType::XmlTextNode => {
                if !(*cur).content.is_null() {
                    if (*cur).name != XML_STRING_TEXT_NOENC.as_ptr() as _ {
                        xml_output_buffer_write_escape(buf, (*cur).content, (*ctxt).escape);
                    } else {
                        /*
                         * Disable escaping, needed for XSLT
                         */
                        xml_output_buffer_write_string(buf, (*cur).content as _);
                    }
                }
            }
            XmlElementType::XmlPiNode => {
                if cur != root && (*ctxt).format == 1 && *xml_indent_tree_output() != 0 {
                    xml_output_buffer_write(
                        buf,
                        (*ctxt).indent_size
                            * if (*ctxt).level > (*ctxt).indent_nr {
                                (*ctxt).indent_nr
                            } else {
                                (*ctxt).level
                            },
                        (*ctxt).indent.as_ptr(),
                    );
                }

                if !(*cur).content.is_null() {
                    xml_output_buffer_write(buf, 2, c"<?".as_ptr() as _);
                    xml_output_buffer_write_string(buf, (*cur).name as _);
                    if !(*cur).content.is_null() {
                        if (*ctxt).format == 2 {
                            xmlOutputBufferWriteWSNonSig(ctxt, 0);
                        } else {
                            xml_output_buffer_write(buf, 1, c" ".as_ptr() as _);
                        }
                        xml_output_buffer_write_string(buf, (*cur).content as _);
                    }
                    xml_output_buffer_write(buf, 2, c"?>".as_ptr() as _);
                } else {
                    xml_output_buffer_write(buf, 2, c"<?".as_ptr() as _);
                    xml_output_buffer_write_string(buf, (*cur).name as _);
                    if (*ctxt).format == 2 {
                        xmlOutputBufferWriteWSNonSig(ctxt, 0);
                    }
                    xml_output_buffer_write(buf, 2, c"?>".as_ptr() as _);
                }
            }
            XmlElementType::XmlCommentNode => {
                if cur != root && (*ctxt).format == 1 && *xml_indent_tree_output() != 0 {
                    xml_output_buffer_write(
                        buf,
                        (*ctxt).indent_size
                            * if (*ctxt).level > (*ctxt).indent_nr {
                                (*ctxt).indent_nr
                            } else {
                                (*ctxt).level
                            },
                        (*ctxt).indent.as_ptr(),
                    );
                }

                if !(*cur).content.is_null() {
                    xml_output_buffer_write(buf, 4, c"<!--".as_ptr() as _);
                    xml_output_buffer_write_string(buf, (*cur).content as _);
                    xml_output_buffer_write(buf, 3, c"-->".as_ptr() as _);
                }
            }
            XmlElementType::XmlEntityRefNode => {
                xml_output_buffer_write(buf, 1, c"&".as_ptr() as _);
                xml_output_buffer_write_string(buf, (*cur).name as _);
                xml_output_buffer_write(buf, 1, c";".as_ptr() as _);
            }
            XmlElementType::XmlCdataSectionNode => {
                if (*cur).content.is_null() || *(*cur).content == b'\0' {
                    xml_output_buffer_write(buf, 12, c"<![CDATA[]]>".as_ptr() as _);
                } else {
                    start = (*cur).content;
                    end = (*cur).content;
                    while *end != b'\0' {
                        if *end == b']' && *end.add(1) == b']' && *end.add(2) == b'>' {
                            end = end.add(2);
                            xml_output_buffer_write(buf, 9, c"<![CDATA[".as_ptr() as _);
                            xml_output_buffer_write(buf, end.offset_from(start) as _, start as _);
                            xml_output_buffer_write(buf, 3, c"]]>".as_ptr() as _);
                            start = end;
                        }
                        end = end.add(1);
                    }
                    if start != end {
                        xml_output_buffer_write(buf, 9, c"<![CDATA[".as_ptr() as _);
                        xml_output_buffer_write_string(buf, start as _);
                        xml_output_buffer_write(buf, 3, c"]]>".as_ptr() as _);
                    }
                }
            }
            XmlElementType::XmlAttributeNode => {
                xmlAttrDumpOutput(ctxt, cur as _);
            }
            XmlElementType::XmlNamespaceDecl => {
                xmlNsDumpOutputCtxt(ctxt, cur as _);
            }
            _ => {}
        }

        loop {
            if cur == root {
                return;
            }
            if (*ctxt).format == 1
                && !matches!(
                    (*cur).typ,
                    XmlElementType::XmlXincludeStart | XmlElementType::XmlXincludeEnd
                )
            {
                xml_output_buffer_write(buf, 1, c"\n".as_ptr() as _);
            }
            if !(*cur).next.is_null() {
                cur = (*cur).next;
                break;
            }

            cur = parent;
            /* (*cur).parent was validated when descending. */
            parent = (*cur).parent;

            if matches!((*cur).typ, XmlElementType::XmlElementNode) {
                if (*ctxt).level > 0 {
                    (*ctxt).level -= 1;
                }
                if *xml_indent_tree_output() != 0 && (*ctxt).format == 1 {
                    xml_output_buffer_write(
                        buf,
                        (*ctxt).indent_size
                            * if (*ctxt).level > (*ctxt).indent_nr {
                                (*ctxt).indent_nr
                            } else {
                                (*ctxt).level
                            },
                        (*ctxt).indent.as_ptr(),
                    );
                }

                xml_output_buffer_write(buf, 2, c"</".as_ptr() as _);
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                    xml_output_buffer_write_string(
                        buf,
                        (*(*cur).ns).prefix.load(Ordering::Relaxed) as _,
                    );
                    xml_output_buffer_write(buf, 1, c":".as_ptr() as _);
                }

                xml_output_buffer_write_string(buf, (*cur).name as _);
                if (*ctxt).format == 2 {
                    xmlOutputBufferWriteWSNonSig(ctxt, 0);
                }
                xml_output_buffer_write(buf, 1, c">".as_ptr() as _);

                if cur == unformatted_node {
                    (*ctxt).format = format;
                    unformatted_node = null_mut();
                }
            }
        }
    }
}

/**
 * xmlDtdDumpOutput:
 * @buf:  the XML buffer output
 * @dtd:  the pointer to the DTD
 *
 * Dump the XML document DTD, if any.
 */
unsafe extern "C" fn xmlDtdDumpOutput(ctxt: XmlSaveCtxtPtr, dtd: XmlDtdPtr) {
    let mut cur: XmlNodePtr;

    if dtd.is_null() {
        return;
    }
    if ctxt.is_null() || (*ctxt).buf.is_null() {
        return;
    }
    let buf: XmlOutputBufferPtr = (*ctxt).buf;
    xml_output_buffer_write(buf, 10, c"<!DOCTYPE ".as_ptr() as _);
    xml_output_buffer_write_string(buf, (*dtd).name as _);
    if !(*dtd).external_id.is_null() {
        xml_output_buffer_write(buf, 8, c" PUBLIC ".as_ptr() as _);
        xml_buf_write_quoted_string((*buf).buffer, (*dtd).external_id);
        xml_output_buffer_write(buf, 1, c" ".as_ptr() as _);
        xml_buf_write_quoted_string((*buf).buffer, (*dtd).system_id);
    } else if !(*dtd).system_id.is_null() {
        xml_output_buffer_write(buf, 8, c" SYSTEM ".as_ptr() as _);
        xml_buf_write_quoted_string((*buf).buffer, (*dtd).system_id);
    }
    if (*dtd).entities.is_null()
        && (*dtd).elements.is_null()
        && (*dtd).attributes.is_null()
        && (*dtd).notations.is_null()
        && (*dtd).pentities.is_null()
    {
        xml_output_buffer_write(buf, 1, c">".as_ptr() as _);
        return;
    }
    xml_output_buffer_write(buf, 3, c" [\n".as_ptr() as _);
    /*
     * Dump the notations first they are not in the DTD children list
     * Do this only on a standalone DTD or on the internal subset though.
     */
    if !(*dtd).notations.is_null() && ((*dtd).doc.is_null() || (*(*dtd).doc).int_subset == dtd) {
        xmlBufDumpNotationTable((*buf).buffer, (*dtd).notations as _);
    }
    let format: c_int = (*ctxt).format;
    let level: c_int = (*ctxt).level;
    (*ctxt).format = 0;
    (*ctxt).level = -1;
    cur = (*dtd).children;
    while !cur.is_null() {
        xmlNodeDumpOutputInternal(ctxt, cur);
        cur = (*cur).next;
    }
    (*ctxt).format = format;
    (*ctxt).level = level;
    xml_output_buffer_write(buf, 2, c"]>".as_ptr() as _);
}

/**
 * xmlNsDumpOutputCtxt
 * @ctxt: the save context
 * @cur:  a namespace
 *
 * Dump a local Namespace definition to a save context.
 * Should be called in the context of attribute dumps.
 */
unsafe extern "C" fn xmlNsDumpOutputCtxt(ctxt: XmlSaveCtxtPtr, cur: XmlNsPtr) {
    xmlNsDumpOutput((*ctxt).buf, cur, ctxt);
}

/**
 * xhtmlAttrListDumpOutput:
 * @cur:  the first attribute pointer
 *
 * Dump a list of XML attributes
 */
#[cfg(feature = "html")]
unsafe extern "C" fn xhtmlAttrListDumpOutput(ctxt: XmlSaveCtxtPtr, mut cur: XmlAttrPtr) {
    use crate::libxml::tree::{xml_free_node, xml_new_doc_text};

    use super::htmltree::html_is_boolean_attr;

    let mut xml_lang: XmlAttrPtr = null_mut();
    let mut lang: XmlAttrPtr = null_mut();
    let mut name: XmlAttrPtr = null_mut();
    let mut id: XmlAttrPtr = null_mut();

    if cur.is_null() {
        return;
    }
    let buf: XmlOutputBufferPtr = (*ctxt).buf;
    let parent: XmlNodePtr = (*cur).parent;
    while !cur.is_null() {
        if (*cur).ns.is_null() && xml_str_equal((*cur).name, c"id".as_ptr() as _) != 0 {
            id = cur;
        } else if (*cur).ns.is_null() && xml_str_equal((*cur).name, c"name".as_ptr() as _) != 0 {
            name = cur;
        } else if (*cur).ns.is_null() && xml_str_equal((*cur).name, c"lang".as_ptr() as _) != 0 {
            lang = cur;
        } else if !(*cur).ns.is_null()
            && xml_str_equal((*cur).name, c"lang".as_ptr() as _) != 0
            && xml_str_equal(
                (*(*cur).ns).prefix.load(Ordering::Relaxed),
                c"xml".as_ptr() as _,
            ) != 0
        {
            xml_lang = cur;
        } else if (*cur).ns.is_null()
            && ((*cur).children.is_null()
                || (*(*cur).children).content.is_null()
                || *(*(*cur).children).content.add(0) == 0)
            && html_is_boolean_attr((*cur).name) != 0
        {
            if !(*cur).children.is_null() {
                xml_free_node((*cur).children);
            }
            (*cur).children = xml_new_doc_text((*cur).doc, (*cur).name);
            if !(*cur).children.is_null() {
                (*(*cur).children).parent = cur as _;
            }
        }
        xmlAttrDumpOutput(ctxt, cur);
        cur = (*cur).next;
    }
    /*
     * C.8
     */
    if (!name.is_null() && id.is_null())
        && (!parent.is_null()
            && !(*parent).name.is_null()
            && (xml_str_equal((*parent).name, c"a".as_ptr() as _) != 0
                || xml_str_equal((*parent).name, c"p".as_ptr() as _) != 0
                || xml_str_equal((*parent).name, c"div".as_ptr() as _) != 0
                || xml_str_equal((*parent).name, c"img".as_ptr() as _) != 0
                || xml_str_equal((*parent).name, c"map".as_ptr() as _) != 0
                || xml_str_equal((*parent).name, c"applet".as_ptr() as _) != 0
                || xml_str_equal((*parent).name, c"form".as_ptr() as _) != 0
                || xml_str_equal((*parent).name, c"frame".as_ptr() as _) != 0
                || xml_str_equal((*parent).name, c"iframe".as_ptr() as _) != 0))
    {
        xml_output_buffer_write(buf, 5, c" id=\"".as_ptr() as _);
        xmlAttrSerializeContent(buf, name);
        xml_output_buffer_write(buf, 1, c"\"".as_ptr() as _);
    }
    /*
     * C.7.
     */
    if !lang.is_null() && xml_lang.is_null() {
        xml_output_buffer_write(buf, 11, c" xml:lang=\"".as_ptr() as _);
        xmlAttrSerializeContent(buf, lang);
        xml_output_buffer_write(buf, 1, c"\"".as_ptr() as _);
    } else if !xml_lang.is_null() && lang.is_null() {
        xml_output_buffer_write(buf, 7, c" lang=\"".as_ptr() as _);
        xmlAttrSerializeContent(buf, xml_lang);
        xml_output_buffer_write(buf, 1, c"\"".as_ptr() as _);
    }
}

const XHTML_NS_NAME: &str = "http://www.w3.org/1999/xhtml";

/**
 * xhtmlIsEmpty:
 * @node:  the node
 *
 * Check if a node is an empty xhtml node
 *
 * Returns 1 if the node is an empty node, 0 if not and -1 in case of error
 */
#[cfg(feature = "html")]
unsafe extern "C" fn xhtmlIsEmpty(node: XmlNodePtr) -> c_int {
    if node.is_null() {
        return -1;
    }
    if !matches!((*node).typ, XmlElementType::XmlElementNode) {
        return 0;
    }
    {
        let str2 = CString::new(XHTML_NS_NAME).unwrap();
        if !(*node).ns.is_null()
            && xml_str_equal(
                (*(*node).ns).href.load(Ordering::Relaxed),
                str2.as_ptr() as _,
            ) == 0
        {
            return 0;
        }
    }
    if !(*node).children.is_null() {
        return 0;
    }
    match *(*node).name.add(0) {
        b'a' => {
            if xml_str_equal((*node).name, c"area".as_ptr() as _) != 0 {
                return 1;
            }
            return 0;
        }
        b'b' => {
            if xml_str_equal((*node).name, c"br".as_ptr() as _) != 0 {
                return 1;
            }
            if xml_str_equal((*node).name, c"base".as_ptr() as _) != 0 {
                return 1;
            }
            if xml_str_equal((*node).name, c"basefont".as_ptr() as _) != 0 {
                return 1;
            }
            return 0;
        }
        b'c' => {
            if xml_str_equal((*node).name, c"col".as_ptr() as _) != 0 {
                return 1;
            }
            return 0;
        }
        b'f' => {
            if xml_str_equal((*node).name, c"frame".as_ptr() as _) != 0 {
                return 1;
            }
            return 0;
        }
        b'h' => {
            if xml_str_equal((*node).name, c"hr".as_ptr() as _) != 0 {
                return 1;
            }
            return 0;
        }
        b'i' => {
            if xml_str_equal((*node).name, c"img".as_ptr() as _) != 0 {
                return 1;
            }
            if xml_str_equal((*node).name, c"input".as_ptr() as _) != 0 {
                return 1;
            }
            if xml_str_equal((*node).name, c"isindex".as_ptr() as _) != 0 {
                return 1;
            }
            return 0;
        }
        b'l' => {
            if xml_str_equal((*node).name, c"link".as_ptr() as _) != 0 {
                return 1;
            }
            return 0;
        }
        b'm' => {
            if xml_str_equal((*node).name, c"meta".as_ptr() as _) != 0 {
                return 1;
            }
            return 0;
        }
        b'p' => {
            if xml_str_equal((*node).name, c"param".as_ptr() as _) != 0 {
                return 1;
            }
            return 0;
        }
        _ => {}
    }
    0
}

/**
 * xhtmlNodeDumpOutput:
 * @buf:  the XML buffer output
 * @doc:  the XHTML document
 * @cur:  the current node
 * @level: the imbrication level for indenting
 * @format: is formatting allowed
 * @encoding:  an optional encoding string
 *
 * Dump an XHTML node, recursive behaviour, children are printed too.
 */
#[cfg(feature = "html")]
pub(crate) unsafe extern "C" fn xhtmlNodeDumpOutput(ctxt: XmlSaveCtxtPtr, mut cur: XmlNodePtr) {
    use crate::libxml::{
        parser_internals::XML_STRING_TEXT, tree::xml_get_prop, xmlstring::xml_strcasecmp,
    };

    let format: c_int = (*ctxt).format;
    let mut addmeta: c_int;
    let mut tmp: XmlNodePtr;

    let mut unformatted_node: XmlNodePtr = null_mut();
    let mut parent: XmlNodePtr;
    let mut start: *mut XmlChar;
    let mut end: *mut XmlChar;
    let buf: XmlOutputBufferPtr = (*ctxt).buf;

    if cur.is_null() {
        return;
    }

    let root: XmlNodePtr = cur;
    parent = (*cur).parent;
    loop {
        match (*cur).typ {
            XmlElementType::XmlDocumentNode | XmlElementType::XmlHtmlDocumentNode => {
                xmlDocContentDumpOutput(ctxt, cur as _);
            }
            XmlElementType::XmlNamespaceDecl => {
                xmlNsDumpOutputCtxt(ctxt, cur as _);
            }
            XmlElementType::XmlDtdNode => {
                xmlDtdDumpOutput(ctxt, cur as _);
            }
            XmlElementType::XmlDocumentFragNode => {
                /* Always validate (*cur).parent when descending. */
                if (*cur).parent == parent && !(*cur).children.is_null() {
                    parent = cur;
                    cur = (*cur).children;
                    continue;
                }
            }
            XmlElementType::XmlElementDecl => {
                xmlBufDumpElementDecl((*buf).buffer, cur as _);
            }
            XmlElementType::XmlAttributeDecl => {
                xmlBufDumpAttributeDecl((*buf).buffer, cur as _);
            }
            XmlElementType::XmlEntityDecl => {
                xmlBufDumpEntityDecl((*buf).buffer, cur as _);
            }
            XmlElementType::XmlElementNode => {
                addmeta = 0;

                if cur != root && (*ctxt).format == 1 && *xml_indent_tree_output() != 0 {
                    xml_output_buffer_write(
                        buf,
                        (*ctxt).indent_size
                            * if (*ctxt).level > (*ctxt).indent_nr {
                                (*ctxt).indent_nr
                            } else {
                                (*ctxt).level
                            },
                        (*ctxt).indent.as_ptr(),
                    );
                }

                /*
                 * Some users like lxml are known to pass nodes with a corrupted
                 * tree structure. Fall back to a recursive call to handle this
                 * case.
                 */
                if (*cur).parent != parent && !(*cur).children.is_null() {
                    xhtmlNodeDumpOutput(ctxt, cur);
                    break;
                }

                xml_output_buffer_write(buf, 1, c"<".as_ptr() as _);
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                    xml_output_buffer_write_string(
                        buf,
                        (*(*cur).ns).prefix.load(Ordering::Relaxed) as _,
                    );
                    xml_output_buffer_write(buf, 1, c":".as_ptr() as _);
                }

                xml_output_buffer_write_string(buf, (*cur).name as _);
                if !(*cur).ns_def.is_null() {
                    xmlNsListDumpOutputCtxt(ctxt, (*cur).ns_def);
                }
                if xml_str_equal((*cur).name, c"html".as_ptr() as _) != 0
                    && (*cur).ns.is_null()
                    && (*cur).ns_def.is_null()
                {
                    /*
                     * 3.1.1. Strictly Conforming Documents A.3.1.1 3/
                     */
                    xml_output_buffer_write_string(
                        buf,
                        c" xmlns=\"http://www.w3.org/1999/xhtml\"".as_ptr() as _,
                    );
                }
                if !(*cur).properties.is_null() {
                    xhtmlAttrListDumpOutput(ctxt, (*cur).properties);
                }

                if !parent.is_null()
                    && ((*parent).parent == (*cur).doc as _)
                    && xml_str_equal((*cur).name, c"head".as_ptr() as _) != 0
                    && xml_str_equal((*parent).name, c"html".as_ptr() as _) != 0
                {
                    tmp = (*cur).children;
                    while !tmp.is_null() {
                        if xml_str_equal((*tmp).name, c"meta".as_ptr() as _) != 0 {
                            let httpequiv: *mut XmlChar =
                                xml_get_prop(tmp, c"http-equiv".as_ptr() as _);
                            if !httpequiv.is_null() {
                                if xml_strcasecmp(httpequiv, c"Content-Type".as_ptr() as _) == 0 {
                                    xml_free(httpequiv as _);
                                    break;
                                }
                                xml_free(httpequiv as _);
                            }
                        }
                        tmp = (*tmp).next;
                    }
                    if tmp.is_null() {
                        addmeta = 1;
                    }
                }

                if (*cur).children.is_null() {
                    if ((*cur).ns.is_null()
                        || (*(*cur).ns).prefix.load(Ordering::Relaxed).is_null())
                        && (xhtmlIsEmpty(cur) == 1 && addmeta == 0)
                    {
                        /*
                         * C.2. Empty Elements
                         */
                        xml_output_buffer_write(buf, 3, c" />".as_ptr() as _);
                    } else {
                        if addmeta == 1 {
                            xml_output_buffer_write(buf, 1, c">".as_ptr() as _);
                            if (*ctxt).format == 1 {
                                xml_output_buffer_write(buf, 1, c"\n".as_ptr() as _);
                                if *xml_indent_tree_output() != 0 {
                                    xml_output_buffer_write(
                                        buf,
                                        (*ctxt).indent_size
                                            * if (*ctxt).level + 1 > (*ctxt).indent_nr {
                                                (*ctxt).indent_nr
                                            } else {
                                                (*ctxt).level + 1
                                            },
                                        (*ctxt).indent.as_ptr(),
                                    );
                                }
                            }
                            xml_output_buffer_write_string(
                                buf,
                                c"<meta http-equiv=\"Content-Type\" content=\"text/html; charset="
                                    .as_ptr() as _,
                            );
                            if !(*ctxt).encoding.is_null() {
                                xml_output_buffer_write_string(buf, (*ctxt).encoding as _);
                            } else {
                                xml_output_buffer_write(buf, 5, c"UTF-8".as_ptr() as _);
                            }
                            xml_output_buffer_write(buf, 4, c"\" />".as_ptr() as _);
                            if (*ctxt).format == 1 {
                                xml_output_buffer_write(buf, 1, c"\n".as_ptr() as _);
                            }
                        } else {
                            xml_output_buffer_write(buf, 1, c">".as_ptr() as _);
                        }
                        /*
                         * C.3. Element Minimization and Empty Element Content
                         */
                        xml_output_buffer_write(buf, 2, c"</".as_ptr() as _);
                        if !(*cur).ns.is_null()
                            && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null()
                        {
                            xml_output_buffer_write_string(
                                buf,
                                (*(*cur).ns).prefix.load(Ordering::Relaxed) as _,
                            );
                            xml_output_buffer_write(buf, 1, c":".as_ptr() as _);
                        }
                        xml_output_buffer_write_string(buf, (*cur).name as _);
                        xml_output_buffer_write(buf, 1, c">".as_ptr() as _);
                    }
                } else {
                    xml_output_buffer_write(buf, 1, c">".as_ptr() as _);
                    if addmeta == 1 {
                        if (*ctxt).format == 1 {
                            xml_output_buffer_write(buf, 1, c"\n".as_ptr() as _);
                            if *xml_indent_tree_output() != 0 {
                                xml_output_buffer_write(
                                    buf,
                                    (*ctxt).indent_size
                                        * if (*ctxt).level + 1 > (*ctxt).indent_nr {
                                            (*ctxt).indent_nr
                                        } else {
                                            (*ctxt).level + 1
                                        },
                                    (*ctxt).indent.as_ptr(),
                                );
                            }
                        }
                        xml_output_buffer_write_string(
                            buf,
                            c"<meta http-equiv=\"Content-Type\" content=\"text/html; charset="
                                .as_ptr() as _,
                        );
                        if !(*ctxt).encoding.is_null() {
                            xml_output_buffer_write_string(buf, (*ctxt).encoding as _);
                        } else {
                            xml_output_buffer_write(buf, 5, c"UTF-8".as_ptr() as _);
                        }
                        xml_output_buffer_write(buf, 4, c"\" />".as_ptr() as _);
                    }

                    if (*ctxt).format == 1 {
                        tmp = (*cur).children;
                        while !tmp.is_null() {
                            if (matches!((*tmp).typ, XmlElementType::XmlTextNode)
                                || matches!((*tmp).typ, XmlElementType::XmlEntityRefNode))
                            {
                                unformatted_node = cur;
                                (*ctxt).format = 0;
                                break;
                            }
                            tmp = (*tmp).next;
                        }
                    }

                    if (*ctxt).format == 1 {
                        xml_output_buffer_write(buf, 1, c"\n".as_ptr() as _);
                    }
                    if (*ctxt).level >= 0 {
                        (*ctxt).level += 1;
                    }
                    parent = cur;
                    cur = (*cur).children;
                    continue;
                }
            }
            XmlElementType::XmlTextNode => {
                if (*cur).content.is_null() {
                    break;
                }
                if (*cur).name == XML_STRING_TEXT.as_ptr() as _
                    || (*cur).name != XML_STRING_TEXT_NOENC.as_ptr() as _
                {
                    xml_output_buffer_write_escape(buf, (*cur).content, (*ctxt).escape);
                } else {
                    /*
                     * Disable escaping, needed for XSLT
                     */
                    xml_output_buffer_write_string(buf, (*cur).content as _);
                }
            }
            XmlElementType::XmlPiNode => {
                if !(*cur).content.is_null() {
                    xml_output_buffer_write(buf, 2, c"<?".as_ptr() as _);
                    xml_output_buffer_write_string(buf, (*cur).name as _);
                    if !(*cur).content.is_null() {
                        xml_output_buffer_write(buf, 1, c" ".as_ptr() as _);
                        xml_output_buffer_write_string(buf, (*cur).content as _);
                    }
                    xml_output_buffer_write(buf, 2, c"?>".as_ptr() as _);
                } else {
                    xml_output_buffer_write(buf, 2, c"<?".as_ptr() as _);
                    xml_output_buffer_write_string(buf, (*cur).name as _);
                    xml_output_buffer_write(buf, 2, c"?>".as_ptr() as _);
                }
            }
            XmlElementType::XmlCommentNode => {
                if !(*cur).content.is_null() {
                    xml_output_buffer_write(buf, 4, c"<!--".as_ptr() as _);
                    xml_output_buffer_write_string(buf, (*cur).content as _);
                    xml_output_buffer_write(buf, 3, c"-->".as_ptr() as _);
                }
            }
            XmlElementType::XmlEntityRefNode => {
                xml_output_buffer_write(buf, 1, c"&".as_ptr() as _);
                xml_output_buffer_write_string(buf, (*cur).name as _);
                xml_output_buffer_write(buf, 1, c";".as_ptr() as _);
            }
            XmlElementType::XmlCdataSectionNode => {
                if (*cur).content.is_null() || *(*cur).content == b'\0' {
                    xml_output_buffer_write(buf, 12, c"<![CDATA[]]>".as_ptr() as _);
                } else {
                    start = (*cur).content;
                    end = (*cur).content;
                    while *end != b'\0' {
                        if *end == b']' && *end.add(1) == b']' && *end.add(2) == b'>' {
                            end = end.add(2);
                            xml_output_buffer_write(buf, 9, c"<![CDATA[".as_ptr() as _);
                            xml_output_buffer_write(buf, end.offset_from(start) as _, start as _);
                            xml_output_buffer_write(buf, 3, c"]]>".as_ptr() as _);
                            start = end;
                        }
                        end = end.add(1);
                    }
                    if start != end {
                        xml_output_buffer_write(buf, 9, c"<![CDATA[".as_ptr() as _);
                        xml_output_buffer_write_string(buf, start as _);
                        xml_output_buffer_write(buf, 3, c"]]>".as_ptr() as _);
                    }
                }
            }
            XmlElementType::XmlAttributeNode => {
                xmlAttrDumpOutput(ctxt, cur as _);
            }
            _ => {}
        }

        loop {
            if cur == root {
                return;
            }
            if (*ctxt).format == 1 {
                xml_output_buffer_write(buf, 1, c"\n".as_ptr() as _);
            }
            if !(*cur).next.is_null() {
                cur = (*cur).next;
                break;
            }

            cur = parent;
            /* (*cur).parent was validated when descending. */
            parent = (*cur).parent;

            if matches!((*cur).typ, XmlElementType::XmlElementNode) {
                if (*ctxt).level > 0 {
                    (*ctxt).level -= 1;
                }
                if *xml_indent_tree_output() != 0 && (*ctxt).format == 1 {
                    xml_output_buffer_write(
                        buf,
                        (*ctxt).indent_size
                            * if (*ctxt).level > (*ctxt).indent_nr {
                                (*ctxt).indent_nr
                            } else {
                                (*ctxt).level
                            },
                        (*ctxt).indent.as_ptr(),
                    );
                }

                xml_output_buffer_write(buf, 2, c"</".as_ptr() as _);
                if !(*cur).ns.is_null() && !(*(*cur).ns).prefix.load(Ordering::Relaxed).is_null() {
                    xml_output_buffer_write_string(
                        buf,
                        (*(*cur).ns).prefix.load(Ordering::Relaxed) as _,
                    );
                    xml_output_buffer_write(buf, 1, c":".as_ptr() as _);
                }

                xml_output_buffer_write_string(buf, (*cur).name as _);
                xml_output_buffer_write(buf, 1, c">".as_ptr() as _);

                if cur == unformatted_node {
                    (*ctxt).format = format;
                    unformatted_node = null_mut();
                }
            }
        }
    }
}

unsafe extern "C" fn xmlSaveClearEncoding(ctxt: XmlSaveCtxtPtr) -> c_int {
    let buf: XmlOutputBufferPtr = (*ctxt).buf;
    xml_output_buffer_flush(buf);
    xml_char_enc_close_func((*buf).encoder);
    xml_buf_free((*buf).conv);
    (*buf).encoder = null_mut();
    (*buf).conv = null_mut();
    0
}

/**
 * xmlDocContentDumpOutput:
 * @cur:  the document
 *
 * Dump an XML document.
 */
pub(crate) unsafe extern "C" fn xmlDocContentDumpOutput(
    ctxt: XmlSaveCtxtPtr,
    cur: XmlDocPtr,
) -> c_int {
    #[cfg(feature = "html")]
    let dtd: XmlDtdPtr;
    #[cfg(feature = "html")]
    let mut is_xhtml: c_int = 0;
    let oldenc: *const XmlChar = (*cur).encoding;
    let oldctxtenc: *const XmlChar = (*ctxt).encoding;
    let mut encoding: *const XmlChar = (*ctxt).encoding;
    let oldescape: Option<XmlCharEncodingOutputFunc> = (*ctxt).escape;
    let oldescape_attr: Option<XmlCharEncodingOutputFunc> = (*ctxt).escape_attr;
    let buf: XmlOutputBufferPtr = (*ctxt).buf;
    let enc: XmlCharEncoding;
    let mut switched_encoding: c_int = 0;

    xml_init_parser();

    if !matches!(
        (*cur).typ,
        XmlElementType::XmlHtmlDocumentNode | XmlElementType::XmlDocumentNode
    ) {
        return -1;
    }

    if !(*ctxt).encoding.is_null() {
        (*cur).encoding = (*ctxt).encoding;
    } else if !(*cur).encoding.is_null() {
        encoding = (*cur).encoding;
    }

    if (matches!((*cur).typ, XmlElementType::XmlHtmlDocumentNode)
        && ((*ctxt).options & XmlSaveOption::XmlSaveAsXml as i32) == 0
        && ((*ctxt).options & XmlSaveOption::XmlSaveXhtml as i32) == 0)
        || (*ctxt).options & XmlSaveOption::XmlSaveAsHtml as i32 != 0
    {
        #[cfg(feature = "html")]
        {
            if !encoding.is_null() {
                html_set_meta_encoding(cur, encoding);
            }
            if encoding.is_null() {
                encoding = html_get_meta_encoding(cur);
            }
            if encoding.is_null() {
                encoding = c"HTML".as_ptr() as _;
            }
            if (!encoding.is_null()
                && oldctxtenc.is_null()
                && (*buf).encoder.is_null()
                && (*buf).conv.is_null())
                && xmlSaveSwitchEncoding(ctxt, encoding as _) < 0
            {
                (*cur).encoding = oldenc;
                return -1;
            }
            if (*ctxt).options & XmlSaveOption::XmlSaveFormat as i32 != 0 {
                html_doc_content_dump_format_output(buf, cur, encoding as _, 1);
            } else {
                html_doc_content_dump_format_output(buf, cur, encoding as _, 0);
            }
            if !(*ctxt).encoding.is_null() {
                (*cur).encoding = oldenc;
            }
            return 0;
        }
        #[cfg(not(feature = "html"))]
        {
            return -1;
        }
    } else if matches!((*cur).typ, XmlElementType::XmlDocumentNode)
        || (*ctxt).options & XmlSaveOption::XmlSaveAsXml as i32 != 0
        || (*ctxt).options & XmlSaveOption::XmlSaveXhtml as i32 != 0
    {
        enc = xml_parse_char_encoding(encoding as _);
        if !encoding.is_null()
            && oldctxtenc.is_null()
            && (*buf).encoder.is_null()
            && (*buf).conv.is_null()
            && ((*ctxt).options & XmlSaveOption::XmlSaveNoDecl as i32) == 0
        {
            if !matches!(
                enc,
                XmlCharEncoding::XmlCharEncodingUtf8
                    | XmlCharEncoding::XmlCharEncodingNone
                    | XmlCharEncoding::XmlCharEncodingAscii
            ) {
                /*
                 * we need to match to this encoding but just for this
                 * document since we output the XMLDecl the conversion
                 * must be done to not generate not well formed documents.
                 */
                if xmlSaveSwitchEncoding(ctxt, encoding as _) < 0 {
                    (*cur).encoding = oldenc;
                    return -1;
                }
                switched_encoding = 1;
            }
            if (*ctxt).escape == Some(xmlEscapeEntities) {
                (*ctxt).escape = None;
            }
            if (*ctxt).escape_attr == Some(xmlEscapeEntities) {
                (*ctxt).escape_attr = None;
            }
        }

        /*
         * Save the XML declaration
         */
        if (*ctxt).options & XmlSaveOption::XmlSaveNoDecl as i32 == 0 {
            xml_output_buffer_write(buf, 14, c"<?xml version=".as_ptr() as _);
            if !(*cur).version.is_null() {
                xml_buf_write_quoted_string((*buf).buffer, (*cur).version);
            } else {
                xml_output_buffer_write(buf, 5, c"\"1.0\"".as_ptr() as _);
            }
            if !encoding.is_null() {
                xml_output_buffer_write(buf, 10, c" encoding=".as_ptr() as _);
                xml_buf_write_quoted_string((*buf).buffer, encoding);
            }
            match (*cur).standalone {
                0 => {
                    xml_output_buffer_write(buf, 16, c" standalone=\"no\"".as_ptr() as _);
                }
                1 => {
                    xml_output_buffer_write(buf, 17, c" standalone=\"yes\"".as_ptr() as _);
                }
                _ => {}
            }
            xml_output_buffer_write(buf, 3, c"?>\n".as_ptr() as _);
        }

        #[cfg(feature = "html")]
        {
            if (*ctxt).options & XmlSaveOption::XmlSaveXhtml as i32 != 0 {
                is_xhtml = 1;
            }
            if (*ctxt).options & XmlSaveOption::XmlSaveNoXhtml as i32 == 0 {
                dtd = xml_get_int_subset(cur);
                if !dtd.is_null() {
                    is_xhtml = xml_is_xhtml((*dtd).system_id, (*dtd).external_id);
                    if is_xhtml < 0 {
                        is_xhtml = 0;
                    }
                }
            }
        }
        if !(*cur).children.is_null() {
            let mut child: XmlNodePtr = (*cur).children;

            while !child.is_null() {
                (*ctxt).level = 0;
                #[cfg(feature = "html")]
                {
                    if is_xhtml != 0 {
                        xhtmlNodeDumpOutput(ctxt, child);
                    } else {
                        xmlNodeDumpOutputInternal(ctxt, child);
                    }
                }
                #[cfg(not(feature = "html"))]
                {
                    xmlNodeDumpOutputInternal(ctxt, child);
                }
                if !matches!(
                    (*child).typ,
                    XmlElementType::XmlXincludeStart | XmlElementType::XmlXincludeEnd
                ) {
                    xml_output_buffer_write(buf, 1, c"\n".as_ptr() as _);
                }
                child = (*child).next;
            }
        }
    }

    /*
     * Restore the state of the saving context at the end of the document
     */
    if switched_encoding != 0 && oldctxtenc.is_null() {
        xmlSaveClearEncoding(ctxt);
        (*ctxt).escape = oldescape;
        (*ctxt).escape_attr = oldescape_attr;
    }
    (*cur).encoding = oldenc;
    0
}

/**
 * xmlFreeSaveCtxt:
 *
 * Free a saving context, destroying the output in any remaining buffer
 */
unsafe extern "C" fn xmlFreeSaveCtxt(ctxt: XmlSaveCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    if !(*ctxt).encoding.is_null() {
        xml_free((*ctxt).encoding as _);
    }
    if !(*ctxt).buf.is_null() {
        xml_output_buffer_close((*ctxt).buf);
    }
    xml_free(ctxt as _);
}

/**
 * xmlNewSaveCtxt:
 *
 * Create a new saving context
 *
 * Returns the new structure or NULL in case of error
 */
unsafe extern "C" fn xmlNewSaveCtxt(encoding: *const c_char, mut options: c_int) -> XmlSaveCtxtPtr {
    let ret: XmlSaveCtxtPtr = xml_malloc(size_of::<XmlSaveCtxt>()) as _;
    if ret.is_null() {
        xml_save_err_memory(c"creating saving context".as_ptr() as _);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlSaveCtxt>());

    if !encoding.is_null() {
        (*ret).handler = xml_find_char_encoding_handler(encoding);
        if (*ret).handler.is_null() {
            xml_save_err(
                XmlParserErrors::XmlSaveUnknownEncoding as i32,
                null_mut(),
                encoding,
            );
            xmlFreeSaveCtxt(ret);
            return null_mut();
        }
        (*ret).encoding = xml_strdup(encoding as _) as _;
        (*ret).escape = None;
    }
    xmlSaveCtxtInit(ret);

    /*
     * Use the options
     */

    /* Re-check this option as it may already have been set */
    if (*ret).options & XmlSaveOption::XmlSaveNoEmpty as i32 != 0
        && options & XmlSaveOption::XmlSaveNoEmpty as i32 == 0
    {
        options |= XmlSaveOption::XmlSaveNoEmpty as i32;
    }

    (*ret).options = options;
    if options & XmlSaveOption::XmlSaveFormat as i32 != 0 {
        (*ret).format = 1;
    } else if options & XmlSaveOption::XmlSaveWsnonsig as i32 != 0 {
        (*ret).format = 2;
    }

    ret
}

/**
 * xmlSaveToFd:
 * @fd:  a file descriptor number
 * @encoding:  the encoding name to use or NULL
 * @options:  a set of xmlSaveOptions
 *
 * Create a document saving context serializing to a file descriptor
 * with the encoding and the options given.
 *
 * Returns a new serialization context or NULL in case of error.
 */
pub unsafe extern "C" fn xmlSaveToFd(
    fd: c_int,
    encoding: *const c_char,
    options: c_int,
) -> XmlSaveCtxtPtr {
    let ret: XmlSaveCtxtPtr = xmlNewSaveCtxt(encoding, options);
    if ret.is_null() {
        return null_mut();
    }
    (*ret).buf = xml_output_buffer_create_fd(fd, (*ret).handler);
    if (*ret).buf.is_null() {
        xml_char_enc_close_func((*ret).handler);
        xmlFreeSaveCtxt(ret);
        return null_mut();
    }
    ret
}

/**
 * xmlSaveToFilename:
 * @filename:  a file name or an URL
 * @encoding:  the encoding name to use or NULL
 * @options:  a set of xmlSaveOptions
 *
 * Create a document saving context serializing to a filename or possibly
 * to an URL (but this is less reliable) with the encoding and the options
 * given.
 *
 * Returns a new serialization context or NULL in case of error.
 */
pub unsafe extern "C" fn xmlSaveToFilename(
    filename: *const c_char,
    encoding: *const c_char,
    options: c_int,
) -> XmlSaveCtxtPtr {
    let compression: c_int = 0; /* TODO handle compression option */

    let ret: XmlSaveCtxtPtr = xmlNewSaveCtxt(encoding, options);
    if ret.is_null() {
        return null_mut();
    }
    (*ret).buf = xml_output_buffer_create_filename(filename, (*ret).handler, compression);
    if (*ret).buf.is_null() {
        xml_char_enc_close_func((*ret).handler);
        xmlFreeSaveCtxt(ret);
        return null_mut();
    }
    ret
}

/**
 * xmlSaveToBuffer:
 * @buffer:  a buffer
 * @encoding:  the encoding name to use or NULL
 * @options:  a set of xmlSaveOptions
 *
 * Create a document saving context serializing to a buffer
 * with the encoding and the options given
 *
 * Returns a new serialization context or NULL in case of error.
 */
pub unsafe extern "C" fn xmlSaveToBuffer(
    buffer: XmlBufferPtr,
    encoding: *const c_char,
    options: c_int,
) -> XmlSaveCtxtPtr {
    let ret: XmlSaveCtxtPtr = xmlNewSaveCtxt(encoding, options);
    if ret.is_null() {
        return null_mut();
    }
    (*ret).buf = xml_output_buffer_create_buffer(buffer, (*ret).handler);
    if (*ret).buf.is_null() {
        xml_char_enc_close_func((*ret).handler);
        xmlFreeSaveCtxt(ret);
        return null_mut();
    }
    ret
}

/**
 * xmlSaveToIO:
 * @iowrite:  an I/O write function
 * @ioclose:  an I/O close function
 * @ioctx:  an I/O handler
 * @encoding:  the encoding name to use or NULL
 * @options:  a set of xmlSaveOptions
 *
 * Create a document saving context serializing to a file descriptor
 * with the encoding and the options given
 *
 * Returns a new serialization context or NULL in case of error.
 */
pub unsafe extern "C" fn xmlSaveToIO(
    iowrite: Option<XmlOutputWriteCallback>,
    ioclose: Option<XmlOutputCloseCallback>,
    ioctx: *mut c_void,
    encoding: *const c_char,
    options: c_int,
) -> XmlSaveCtxtPtr {
    let ret: XmlSaveCtxtPtr = xmlNewSaveCtxt(encoding, options);
    if ret.is_null() {
        return null_mut();
    }
    (*ret).buf = xml_output_buffer_create_io(iowrite, ioclose, ioctx, (*ret).handler);
    if (*ret).buf.is_null() {
        xml_char_enc_close_func((*ret).handler);
        xmlFreeSaveCtxt(ret);
        return null_mut();
    }
    ret
}

/**
 * xmlSaveDoc:
 * @ctxt:  a document saving context
 * @doc:  a document
 *
 * Save a full document to a saving context
 * TODO: The function is not fully implemented yet as it does not return the
 * byte count but 0 instead
 *
 * Returns the number of byte written or -1 in case of error
 */
pub unsafe extern "C" fn xmlSaveDoc(ctxt: XmlSaveCtxtPtr, doc: XmlDocPtr) -> c_long {
    let ret: c_long = 0;

    if ctxt.is_null() || doc.is_null() {
        return -1;
    }
    if xmlDocContentDumpOutput(ctxt, doc) < 0 {
        return -1;
    }
    ret
}

/**
 * htmlNodeDumpOutputInternal:
 * @cur:  the current node
 *
 * Dump an HTML node, recursive behaviour, children are printed too.
 */
#[cfg(feature = "html")]
unsafe extern "C" fn htmlNodeDumpOutputInternal(ctxt: XmlSaveCtxtPtr, cur: XmlNodePtr) -> c_int {
    use super::htmltree::html_node_dump_format_output;

    let mut oldenc: *const XmlChar = null();
    let oldctxtenc: *const XmlChar = (*ctxt).encoding;
    let mut encoding: *const XmlChar = (*ctxt).encoding;
    let buf: XmlOutputBufferPtr = (*ctxt).buf;
    let mut switched_encoding: c_int = 0;

    xml_init_parser();

    let doc: XmlDocPtr = (*cur).doc;
    if !doc.is_null() {
        oldenc = (*doc).encoding;
        if !(*ctxt).encoding.is_null() {
            (*doc).encoding = (*ctxt).encoding;
        } else if !(*doc).encoding.is_null() {
            encoding = (*doc).encoding;
        }
    }

    if !encoding.is_null() && !doc.is_null() {
        html_set_meta_encoding(doc, encoding);
    }
    if encoding.is_null() && !doc.is_null() {
        encoding = html_get_meta_encoding(doc);
    }
    if encoding.is_null() {
        encoding = c"HTML".as_ptr() as _;
    }
    if !encoding.is_null()
        && oldctxtenc.is_null()
        && (*buf).encoder.is_null()
        && (*buf).conv.is_null()
    {
        if xmlSaveSwitchEncoding(ctxt, encoding as _) < 0 {
            (*doc).encoding = oldenc;
            return -1;
        }
        switched_encoding = 1;
    }
    if (*ctxt).options & XmlSaveOption::XmlSaveFormat as i32 != 0 {
        html_node_dump_format_output(buf, doc, cur, encoding as _, 1);
    } else {
        html_node_dump_format_output(buf, doc, cur, encoding as _, 0);
    }
    /*
     * Restore the state of the saving context at the end of the document
     */
    if switched_encoding != 0 && oldctxtenc.is_null() {
        xmlSaveClearEncoding(ctxt);
    }
    if !doc.is_null() {
        (*doc).encoding = oldenc;
    }
    0
}

/**
 * xmlSaveTree:
 * @ctxt:  a document saving context
 * @node:  the top node of the subtree to save
 *
 * Save a subtree starting at the node parameter to a saving context
 * TODO: The function is not fully implemented yet as it does not return the
 * byte count but 0 instead
 *
 * Returns the number of byte written or -1 in case of error
 */
pub unsafe extern "C" fn xmlSaveTree(ctxt: XmlSaveCtxtPtr, node: XmlNodePtr) -> c_long {
    let ret: c_long = 0;

    if ctxt.is_null() || node.is_null() {
        return -1;
    }
    #[cfg(feature = "html")]
    {
        if (*ctxt).options & XmlSaveOption::XmlSaveXhtml as i32 != 0 {
            xhtmlNodeDumpOutput(ctxt, node);
            return ret;
        }
        if (!matches!((*node).typ, XmlElementType::XmlNamespaceDecl)
            && !(*node).doc.is_null()
            && matches!((*(*node).doc).typ, XmlElementType::XmlHtmlDocumentNode)
            && (*ctxt).options & XmlSaveOption::XmlSaveAsXml as i32 == 0)
            || (*ctxt).options & XmlSaveOption::XmlSaveAsHtml as i32 != 0
        {
            htmlNodeDumpOutputInternal(ctxt, node);
            return ret;
        }
    }
    xmlNodeDumpOutputInternal(ctxt, node);
    ret
}

/**
 * xmlSaveFlush:
 * @ctxt:  a document saving context
 *
 * Flush a document saving context, i.e. make sure that all bytes have
 * been output.
 *
 * Returns the number of byte written or -1 in case of error.
 */
pub unsafe extern "C" fn xmlSaveFlush(ctxt: XmlSaveCtxtPtr) -> c_int {
    if ctxt.is_null() {
        return -1;
    }
    if (*ctxt).buf.is_null() {
        return -1;
    }
    xml_output_buffer_flush((*ctxt).buf)
}

/**
 * xmlSaveClose:
 * @ctxt:  a document saving context
 *
 * Close a document saving context, i.e. make sure that all bytes have
 * been output and free the associated data.
 *
 * Returns the number of byte written or -1 in case of error.
 */
pub unsafe extern "C" fn xmlSaveClose(ctxt: XmlSaveCtxtPtr) -> c_int {
    if ctxt.is_null() {
        return -1;
    }
    let ret: c_int = xmlSaveFlush(ctxt);
    xmlFreeSaveCtxt(ctxt);
    ret
}

/**
 * xmlSaveSetEscape:
 * @ctxt:  a document saving context
 * @escape:  the escaping function
 *
 * Set a custom escaping function to be used for text in element content
 *
 * Returns 0 if successful or -1 in case of error.
 */
pub unsafe extern "C" fn xmlSaveSetEscape(
    ctxt: XmlSaveCtxtPtr,
    escape: Option<XmlCharEncodingOutputFunc>,
) -> c_int {
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).escape = escape;
    0
}

/**
 * xmlSaveSetAttrEscape:
 * @ctxt:  a document saving context
 * @escape:  the escaping function
 *
 * Set a custom escaping function to be used for text in attribute content
 *
 * Returns 0 if successful or -1 in case of error.
 */
pub unsafe extern "C" fn xmlSaveSetAttrEscape(
    ctxt: XmlSaveCtxtPtr,
    escape: Option<XmlCharEncodingOutputFunc>,
) -> c_int {
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).escape_attr = escape;
    0
}

#[cfg(test)]
mod tests {
    use crate::{
        libxml::{xmlerror::xmlResetLastError, xmlmemory::xml_mem_blocks},
        test_util::*,
    };

    use super::*;

    #[test]
    fn test_xml_save_close() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SAVE_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_save_ctxt_ptr(n_ctxt, 0);

                let ret_val = xmlSaveClose(ctxt);
                desret_int(ret_val);
                des_xml_save_ctxt_ptr(n_ctxt, ctxt, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSaveClose",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveClose()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_save_doc() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SAVE_CTXT_PTR {
                for n_doc in 0..GEN_NB_XML_DOC_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_save_ctxt_ptr(n_ctxt, 0);
                    let doc = gen_xml_doc_ptr(n_doc, 1);

                    let ret_val = xmlSaveDoc(ctxt, doc);
                    desret_long(ret_val);
                    des_xml_save_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_doc_ptr(n_doc, doc, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSaveDoc",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveDoc()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_doc);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_save_flush() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SAVE_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_save_ctxt_ptr(n_ctxt, 0);

                let ret_val = xmlSaveFlush(ctxt);
                desret_int(ret_val);
                des_xml_save_ctxt_ptr(n_ctxt, ctxt, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlSaveFlush",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveFlush()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_save_set_attr_escape() {

        /* missing type support */
    }

    #[test]
    fn test_xml_save_set_escape() {

        /* missing type support */
    }

    #[test]
    fn test_xml_save_to_buffer() {

        /* missing type support */
    }

    #[test]
    fn test_xml_save_to_fd() {

        /* missing type support */
    }

    #[test]
    fn test_xml_save_to_filename() {

        /* missing type support */
    }

    #[test]
    fn test_xml_save_tree() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_SAVE_CTXT_PTR {
                for n_cur in 0..GEN_NB_XML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_xml_save_ctxt_ptr(n_ctxt, 0);
                    let cur = gen_xml_node_ptr(n_cur, 1);

                    let ret_val = xmlSaveTree(ctxt, cur);
                    desret_long(ret_val);
                    des_xml_save_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_xml_node_ptr(n_cur, cur, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlSaveTree",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlSaveTree()");
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_cur);
                    }
                }
            }
        }
    }
}

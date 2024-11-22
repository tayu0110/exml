//! Provide internal methods and data structures for serializing XML documents.  
//! This module is based on `private/save.h`, `xmlsave.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{ffi::c_int, ptr::null_mut};

use crate::{
    error::XmlParserErrors,
    io::XmlOutputBufferPtr,
    libxml::{
        chvalid::xml_is_char,
        xmlsave::{xml_ns_dump_output, xml_save_err, xml_serialize_hex_char_ref},
        xmlstring::XmlChar,
    },
    tree::{XmlAttrPtr, XmlBufPtr, XmlDocPtr, XmlNsPtr},
};

use super::buf::xml_buf_add;

/// Serialize text attribute values to an xmlBufPtr
#[doc(alias = "xmlBufAttrSerializeTxtContent")]
pub(crate) unsafe extern "C" fn xml_buf_attr_serialize_txt_content(
    buf: XmlBufPtr,
    doc: XmlDocPtr,
    attr: XmlAttrPtr,
    string: *const XmlChar,
) {
    let mut base: *mut XmlChar;
    let mut cur: *mut XmlChar;

    if string.is_null() {
        return;
    }
    base = string as _;
    cur = base;
    while *cur != 0 {
        if *cur == b'\n' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&#10;".as_ptr() as _, 5);
            cur = cur.add(1);
            base = cur;
        } else if *cur == b'\r' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&#13;".as_ptr() as _, 5);
            cur = cur.add(1);
            base = cur;
        } else if *cur == b'\t' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&#9;".as_ptr() as _, 4);
            cur = cur.add(1);
            base = cur;
        } else if *cur == b'"' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&quot;".as_ptr() as _, 6);
            cur = cur.add(1);
            base = cur;
        } else if *cur == b'<' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&lt;".as_ptr() as _, 4);
            cur = cur.add(1);
            base = cur;
        } else if *cur == b'>' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&gt;".as_ptr() as _, 4);
            cur = cur.add(1);
            base = cur;
        } else if *cur == b'&' {
            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            xml_buf_add(buf, c"&amp;".as_ptr() as _, 5);
            cur = cur.add(1);
            base = cur;
        } else if *cur >= 0x80 && *cur.add(1) != 0 && (doc.is_null() || (*doc).encoding.is_none()) {
            /*
             * We assume we have UTF-8 content.
             */
            let mut tmp: [u8; 12] = [0; 12];
            let mut val: c_int = 0;
            let mut l: c_int = 1;

            if base != cur {
                xml_buf_add(buf, base, cur.offset_from(base) as _);
            }
            if *cur < 0xC0 {
                xml_save_err(XmlParserErrors::XmlSaveNotUTF8 as _, attr as _, null_mut());
                xml_serialize_hex_char_ref(&mut tmp, *cur as _);
                xml_buf_add(buf, tmp.as_ptr() as _, -1);
                cur = cur.add(1);
                base = cur;
                continue;
            } else if *cur < 0xE0 {
                val = *cur.add(0) as i32 & 0x1F;
                val <<= 6;
                val |= *cur.add(1) as i32 & 0x3F;
                l = 2;
            } else if *cur < 0xF0 && *cur.add(2) != 0 {
                val = *cur.add(0) as i32 & 0x0F;
                val <<= 6;
                val |= *cur.add(1) as i32 & 0x3F;
                val <<= 6;
                val |= *cur.add(2) as i32 & 0x3F;
                l = 3;
            } else if *cur < 0xF8 && *cur.add(2) != 0 && *cur.add(3) != 0 {
                val = *cur.add(0) as i32 & 0x07;
                val <<= 6;
                val |= *cur.add(1) as i32 & 0x3F;
                val <<= 6;
                val |= *cur.add(2) as i32 & 0x3F;
                val <<= 6;
                val |= *cur.add(3) as i32 & 0x3F;
                l = 4;
            }
            if l == 1 || !xml_is_char(val as u32) {
                xml_save_err(
                    XmlParserErrors::XmlSaveCharInvalid as _,
                    attr as _,
                    null_mut(),
                );
                xml_serialize_hex_char_ref(&mut tmp, *cur as _);
                xml_buf_add(buf, tmp.as_ptr() as _, -1);
                cur = cur.add(1);
                base = cur;
                continue;
            }
            /*
             * We could do multiple things here. Just save
             * as a c_char ref
             */
            xml_serialize_hex_char_ref(&mut tmp, val as u32);
            xml_buf_add(buf, tmp.as_ptr() as _, -1);
            cur = cur.add(l as usize);
            base = cur;
        } else {
            cur = cur.add(1);
        }
    }
    if base != cur {
        xml_buf_add(buf, base, cur.offset_from(base) as _);
    }
}

/**
 * xmlNsListDumpOutput:
 * @buf:  the XML buffer output
 * @cur:  the first namespace
 *
 * Dump a list of local Namespace definitions.
 * Should be called in the context of attributes dumps.
 */
pub(crate) unsafe extern "C" fn xml_ns_list_dump_output(
    buf: XmlOutputBufferPtr,
    mut cur: XmlNsPtr,
) {
    while !cur.is_null() {
        xml_ns_dump_output(buf, cur, null_mut());
        cur = (*cur).next;
    }
}

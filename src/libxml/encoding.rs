//! Provide methods and data structures for handling character encodings.  
//! This module is based on `libxml/encoding.h`, `encoding.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, c_uchar, c_uint, c_ushort},
    mem::{size_of, size_of_val},
    ptr::{addr_of_mut, null, null_mut},
    sync::atomic::{AtomicI32, AtomicPtr, AtomicUsize, Ordering},
};

use libc::{memcpy, memmove, memset, strcmp};

use crate::{
    __xml_raise_error, libxml::xmlerror::XmlErrorDomain, private::error::__xml_simple_error,
};

use super::{
    globals::{xml_free, xml_malloc, xml_mem_strdup, xml_realloc},
    htmlparser::utf8_to_html,
    parser::xml_init_parser,
    xmlerror::XmlParserErrors,
};

/*
 * xmlCharEncoding:
 *
 * Predefined values for some standard encodings.
 * Libxml does not do beforehand translation on UTF8 and ISOLatinX.
 * It also supports ASCII, ISO-8859-1, and UTF16 (LE and BE) by default.
 *
 * Anything else would have to be translated to UTF8 before being
 * given to the parser itself. The BOM for UTF16 and the encoding
 * declaration are looked at and a converter is looked for at that
 * point. If not found the parser stops here as asked by the XML REC. A
 * converter can be registered by the user using xmlRegisterCharEncodingHandler
 * but the current form doesn't allow stateful transcoding (a serious
 * problem agreed !). If iconv has been found it will be used
 * automatically and allow stateful transcoding, the simplest is then
 * to be sure to enable iconv and to provide iconv libs for the encoding
 * support needed.
 *
 * Note that the generic "UTF-16" is not a predefined value.  Instead, only
 * the specific UTF-16LE and UTF-16BE are present.
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlCharEncoding {
    Error = -1,     /* No char encoding detected */
    None = 0,       /* No char encoding detected */
    UTF8 = 1,       /* UTF-8 */
    UTF16LE = 2,    /* UTF-16 little endian */
    UTF16BE = 3,    /* UTF-16 big endian */
    UCS4LE = 4,     /* UCS-4 little endian */
    UCS4BE = 5,     /* UCS-4 big endian */
    EBCDIC = 6,     /* EBCDIC uh! */
    UCS4_2143 = 7,  /* UCS-4 unusual ordering */
    UCS4_3412 = 8,  /* UCS-4 unusual ordering */
    UCS2 = 9,       /* UCS-2 */
    ISO8859_1 = 10, /* ISO-8859-1 ISO Latin 1 */
    ISO8859_2 = 11, /* ISO-8859-2 ISO Latin 2 */
    ISO8859_3 = 12, /* ISO-8859-3 */
    ISO8859_4 = 13, /* ISO-8859-4 */
    ISO8859_5 = 14, /* ISO-8859-5 */
    ISO8859_6 = 15, /* ISO-8859-6 */
    ISO8859_7 = 16, /* ISO-8859-7 */
    ISO8859_8 = 17, /* ISO-8859-8 */
    ISO8859_9 = 18, /* ISO-8859-9 */
    ISO2022JP = 19, /* ISO-2022-JP */
    ShiftJIS = 20,  /* Shift_JIS */
    EUCJP = 21,     /* EUC-JP */
    ASCII = 22,     /* pure ASCII */
}

/**
 * xmlCharEncodingInputFunc:
 * @out:  a pointer to an array of bytes to store the UTF-8 result
 * @outlen:  the length of @out
 * @input:  a pointer to an array of chars in the original encoding
 * @inlen:  the length of @in
 *
 * Take a block of chars in the original encoding and try to convert
 * it to an UTF-8 block of chars out.
 *
 * Returns the number of bytes written, -1 if lack of space, or -2
 *     if the transcoding failed.
 * The value of @inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictiable.
 * The value of @outlen after return is the number of octets consumed.
 */
pub type XmlCharEncodingInputFunc = unsafe extern "C" fn(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int;

/**
 * xmlCharEncodingOutputFunc:
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @input:  a pointer to an array of UTF-8 chars
 * @inlen:  the length of @in
 *
 * Take a block of UTF-8 chars in and try to convert it to another
 * encoding.
 * Note: a first call designed to produce heading info is called with
 * in = NULL. If stateful this should also initialize the encoder state.
 *
 * Returns the number of bytes written, -1 if lack of space, or -2
 *     if the transcoding failed.
 * The value of @inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictiable.
 * The value of @outlen after return is the number of octets produced.
 */
pub type XmlCharEncodingOutputFunc = unsafe extern "C" fn(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int;

/*
 * Block defining the handlers for non UTF-8 encodings.
 * If iconv is supported, there are two extra fields.
 */
pub type XmlCharEncodingHandlerPtr = *mut XmlCharEncodingHandler;
#[repr(C)]
pub struct XmlCharEncodingHandler {
    pub(crate) name: AtomicPtr<c_char>,
    pub(crate) input: Option<XmlCharEncodingInputFunc>,
    pub(crate) output: Option<XmlCharEncodingOutputFunc>,
}

/*
 * Interfaces for encoding handlers.
 */
/**
 * xmlInitCharEncodingHandlers:
 *
 * DEPRECATED: Alias for xmlInitParser.
 */
#[deprecated]
pub unsafe extern "C" fn xml_init_char_encoding_handlers() {
    xml_init_parser();
}

static HANDLERS: AtomicPtr<XmlCharEncodingHandlerPtr> = AtomicPtr::new(null_mut());
static NB_CHAR_ENCODING_HANDLER: AtomicUsize = AtomicUsize::new(0);

/**
 * xmlCleanupCharEncodingHandlers:
 *
 * DEPRECATED: This function will be made private. Call xmlCleanupParser
 * to free global state but see the warnings there. xmlCleanupParser
 * should be only called once at program exit. In most cases, you don't
 * have call cleanup functions at all.
 *
 * Cleanup the memory allocated for the c_char encoding support, it
 * unregisters all the encoding handlers and the aliases.
 */
pub(crate) unsafe extern "C" fn xml_cleanup_char_encoding_handlers() {
    xml_cleanup_encoding_aliases();

    let handlers = HANDLERS.load(Ordering::Acquire);
    if handlers.is_null() {
        return;
    }

    let mut num_handlers = NB_CHAR_ENCODING_HANDLER.load(Ordering::Acquire);
    while num_handlers > 0 {
        num_handlers -= 1;
        if !(*handlers.add(num_handlers)).is_null() {
            if !(*(*handlers.add(num_handlers)))
                .name
                .load(Ordering::Relaxed)
                .is_null()
            {
                xml_free(
                    (*(*handlers.add(num_handlers)))
                        .name
                        .load(Ordering::Relaxed) as _,
                );
            }
            xml_free(*handlers.add(num_handlers) as _);
        }
    }
    xml_free(handlers as _);
    HANDLERS.store(null_mut(), Ordering::Release);
    NB_CHAR_ENCODING_HANDLER.store(0, Ordering::Release);
}

const MAX_ENCODING_HANDLERS: usize = 50;

/**
 * xmlErrEncoding:
 * @error:  the error number
 * @msg:  the error message
 *
 * n encoding error
 */
pub(crate) unsafe extern "C" fn xml_encoding_err(
    error: XmlParserErrors,
    msg: *const c_char,
    val: *const c_char,
) {
    __xml_raise_error!(
        None,
        None,
        null_mut(),
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromI18N as i32,
        error as i32,
        XmlErrorLevel::XmlErrFatal,
        null_mut(),
        0,
        val,
        null_mut(),
        null_mut(),
        0,
        0,
        msg,
        val
    );
}

/**
 * xmlEncodingErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
unsafe extern "C" fn xml_encoding_err_memory(extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromI18N as i32,
        XmlParserErrors::XmlErrNoMemory as i32,
        null_mut(),
        null(),
        extra as _,
    );
}

/**
 * xmlRegisterCharEncodingHandler:
 * @handler:  the xmlCharEncodingHandlerPtr handler block
 *
 * Register the c_char encoding handler, surprising, isn't it ?
 */
pub unsafe extern "C" fn xml_register_char_encoding_handler(handler: XmlCharEncodingHandlerPtr) {
    if handler.is_null() {
        xml_encoding_err(
            XmlParserErrors::XmlI18nNoHandler,
            c"xmlRegisterCharEncodingHandler: NULL handler\n".as_ptr() as _,
            null_mut(),
        );
        return;
    }

    let mut handlers = HANDLERS.load(Ordering::Acquire);
    if handlers.is_null() {
        handlers = xml_malloc(MAX_ENCODING_HANDLERS * size_of_val(&*handlers.add(0))) as _;
        if handlers.is_null() {
            xml_encoding_err_memory(c"allocating handler table".as_ptr() as _);
            // goto free_handler;
            if !handler.is_null() {
                if !(*handler).name.load(Ordering::Relaxed).is_null() {
                    xml_free((*handler).name.load(Ordering::Relaxed) as _);
                }
                xml_free(handler as _);
            }
            return;
        }
    }

    let mut num_handlers = NB_CHAR_ENCODING_HANDLER.load(Ordering::Acquire);
    if num_handlers >= MAX_ENCODING_HANDLERS {
        xml_encoding_err(
            XmlParserErrors::XmlI18nExcessHandler,
            c"xmlRegisterCharEncodingHandler: Too many handler registered, see %s\n".as_ptr(),
            c"MAX_ENCODING_HANDLERS".as_ptr() as _,
        );
        // goto free_handler;
        if !handler.is_null() {
            if !(*handler).name.load(Ordering::Relaxed).is_null() {
                xml_free((*handler).name.load(Ordering::Relaxed) as _);
            }
            xml_free(handler as _);
        }
        return;
    }

    *handlers.add(num_handlers) = handler;
    num_handlers += 1;
    NB_CHAR_ENCODING_HANDLER.store(num_handlers, Ordering::Release);
    HANDLERS.store(handlers, Ordering::Release);

    // free_handler:
    // if !handler.is_null() {
    //     if !(*handler).name.is_null() {
    //         xmlFree((*handler).name as _);
    //     }
    //     xmlFree(handler as _);
    // }
}

macro_rules! MAKE_HANDLER {
    ($name:expr, $input:expr, $out:expr) => {
        XmlCharEncodingHandler {
            name: AtomicPtr::new($name as _),
            input: $input,
            output: $out,
        }
    };
}

/**
 * UTF8ToUTF8:
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @inb:  a pointer to an array of UTF-8 chars
 * @inlenb:  the length of @in in UTF-8 chars
 *
 * No op copy operation for UTF8 handling.
 *
 * Returns the number of bytes written, or -1 if lack of space.
 *     The value of *inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictable.
 */
unsafe extern "C" fn utf8_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    inb: *const c_uchar,
    inlenb: *mut c_int,
) -> c_int {
    if out.is_null() || outlen.is_null() || inlenb.is_null() {
        return -1;
    }
    if inb.is_null() {
        /* inb.is_null() means output is initialized. */
        *outlen = 0;
        *inlenb = 0;
        return 0;
    }
    let len = if *outlen > *inlenb { *inlenb } else { *outlen };
    if len < 0 {
        return -1;
    }

    /*
     * FIXME: Conversion functions must assure valid UTF-8, so we have
     * to check for UTF-8 validity. Preferably, this converter shouldn't
     * be used at all.
     */
    memcpy(out as _, inb as _, len as usize);

    *outlen = len;
    *inlenb = len;
    *outlen
}

pub(crate) static XML_LITTLE_ENDIAN: AtomicI32 = AtomicI32::new(1);

/**
 * UTF16LEToUTF8:
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @inb:  a pointer to an array of UTF-16LE passwd as a byte array
 * @inlenb:  the length of @in in UTF-16LE chars
 *
 * Take a block of UTF-16LE ushorts in and try to convert it to an UTF-8
 * block of chars out. This function assumes the endian property
 * is the same between the native type of this machine and the
 * inputed one.
 *
 * Returns the number of bytes written, or -1 if lack of space, or -2
 *     if the transcoding fails (if *in is not a valid utf16 string)
 *     The value of *inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictable.
 */
unsafe extern "C" fn utf16le_to_utf8(
    mut out: *mut c_uchar,
    outlen: *mut c_int,
    inb: *const c_uchar,
    inlenb: *mut c_int,
) -> c_int {
    let outstart: *mut c_uchar = out;
    let mut processed: *const c_uchar = inb;
    let mut input: *mut c_ushort = inb as *mut c_ushort;
    let mut c: c_uint;
    let mut d: c_uint;
    let mut tmp: *mut c_uchar;
    let mut bits: c_int;

    if *outlen == 0 {
        *inlenb = 0;
        return 0;
    }
    let outend: *mut c_uchar = out.add(*outlen as usize);
    if *inlenb % 2 == 1 {
        (*inlenb) -= 1;
    }
    let inlen: c_uint = *inlenb as u32 / 2;
    let inend: *mut c_ushort = input.add(inlen as _);
    while input < inend && out.offset_from(outstart) + 5 < *outlen as isize {
        if XML_LITTLE_ENDIAN.load(Ordering::Relaxed) != 0 {
            c = *input as _;
            input = input.add(1);
        } else {
            tmp = input as *mut c_uchar;
            c = *tmp as _;
            tmp = tmp.add(1);
            c |= (*tmp as u32) << 8;
            input = input.add(1);
        }
        if c & 0xFC00 == 0xD800 {
            /* surrogates */
            if input >= inend {
                /* handle split mutli-byte characters */
                break;
            }
            if XML_LITTLE_ENDIAN.load(Ordering::Relaxed) != 0 {
                d = *input as _;
                input = input.add(1);
            } else {
                tmp = input as *mut c_uchar;
                d = *tmp as _;
                tmp = tmp.add(1);
                d |= (*tmp as u32) << 8;
                input = input.add(1);
            }
            if d & 0xFC00 == 0xDC00 {
                c &= 0x03FF;
                c <<= 10;
                c |= d & 0x03FF;
                c += 0x10000;
            } else {
                *outlen = out.offset_from(outstart) as _;
                *inlenb = processed.offset_from(inb) as _;
                return -2;
            }
        }

        /* assertion: c is a single UTF-4 value */
        if out >= outend {
            break;
        }
        if c < 0x80 {
            *out = c as _;
            out = out.add(1);
            bits = -6;
        } else if c < 0x800 {
            *out = ((c >> 6) & 0x1F) as u8 | 0xC0;
            out = out.add(1);
            bits = 0;
        } else if c < 0x10000 {
            *out = ((c >> 12) & 0x0F) as u8 | 0xE0;
            out = out.add(1);
            bits = 6;
        } else {
            *out = ((c >> 18) & 0x07) as u8 | 0xF0;
            out = out.add(1);
            bits = 12;
        }

        while bits >= 0 {
            if out >= outend {
                break;
            }
            *out = ((c >> bits) & 0x3F) as u8 | 0x80;
            out = out.add(1);
            bits -= 6;
        }
        processed = input as *const c_uchar;
    }
    *outlen = out.offset_from(outstart) as _;
    *inlenb = processed.offset_from(inb) as _;
    *outlen
}

/**
 * UTF8ToUTF16LE:
 * @outb:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @outb
 * @in:  a pointer to an array of UTF-8 chars
 * @inlen:  the length of @in
 *
 * Take a block of UTF-8 chars in and try to convert it to an UTF-16LE
 * block of chars out.
 *
 * Returns the number of bytes written, or -1 if lack of space, or -2
 *     if the transcoding failed.
 */
unsafe extern "C" fn utf8_to_utf16le(
    outb: *mut c_uchar,
    outlen: *mut c_int,
    mut input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    let mut out: *mut c_ushort = outb as *mut c_ushort;
    let mut processed: *const c_uchar = input;
    let instart: *const c_uchar = input;
    let outstart: *mut c_ushort = out;

    let mut c: c_uint;
    let mut d: c_uint;
    let mut trailing: c_int;
    let mut tmp: *mut c_uchar;
    let mut tmp1: c_ushort;
    let mut tmp2: c_ushort;

    /* UTF16LE encoding has no BOM */
    if out.is_null() || outlen.is_null() || inlen.is_null() {
        return -1;
    }
    if input.is_null() {
        *outlen = 0;
        *inlen = 0;
        return 0;
    }
    let inend: *const c_uchar = input.add(*inlen as usize);
    let outend: *mut c_ushort = out.add(*outlen as usize / 2);
    while input < inend {
        d = *input as _;
        input = input.add(1);
        if d < 0x80 {
            c = d;
            trailing = 0;
        } else if d < 0xC0 {
            /* trailing byte in leading position */
            *outlen = out.offset_from(outstart) as i32 * 2;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        } else if d < 0xE0 {
            c = d & 0x1F;
            trailing = 1;
        } else if d < 0xF0 {
            c = d & 0x0F;
            trailing = 2;
        } else if d < 0xF8 {
            c = d & 0x07;
            trailing = 3;
        } else {
            /* no chance for this in UTF-16 */
            *outlen = out.offset_from(outstart) as i32 * 2;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        }

        if inend.offset_from(input) < trailing as isize {
            break;
        }

        while trailing != 0 {
            if input >= inend || {
                d = *input as _;
                input = input.add(1);
                d & 0xC0 != 0x80
            } {
                break;
            }
            c <<= 6;
            c |= d & 0x3F;
            trailing -= 1;
        }

        /* assertion: c is a single UTF-4 value */
        if c < 0x10000 {
            if out >= outend {
                break;
            }
            if XML_LITTLE_ENDIAN.load(Ordering::Relaxed) != 0 {
                *out = c as _;
                out = out.add(1);
            } else {
                tmp = out as *mut c_uchar;
                *tmp = c as c_uchar; /* Explicit truncation */
                *tmp.add(1) = (c >> 8) as u8;
                out = out.add(1);
            }
        } else if c < 0x110000 {
            if out.add(1) >= outend {
                break;
            }
            c -= 0x10000;
            if XML_LITTLE_ENDIAN.load(Ordering::Relaxed) != 0 {
                *out = 0xD800 | (c >> 10) as u16;
                out = out.add(1);
                *out = 0xDC00 | (c & 0x03FF) as u16;
                out = out.add(1);
            } else {
                tmp1 = 0xD800 | (c >> 10) as u16;
                tmp = out as *mut c_uchar;
                *tmp = tmp1 as c_uchar; /* Explicit truncation */
                *tmp.add(1) = (tmp1 >> 8) as u8;
                out = out.add(1);

                tmp2 = 0xDC00 | (c & 0x03FF) as u16;
                tmp = out as *mut c_uchar;
                *tmp = tmp2 as c_uchar; /* Explicit truncation */
                *tmp.add(1) = (tmp2 >> 8) as u8;
                out = out.add(1);
            }
        } else {
            break;
        }
        processed = input;
    }
    *outlen = out.offset_from(outstart) as i32 * 2;
    *inlen = processed.offset_from(instart) as _;
    *outlen
}

/**
 * UTF16BEToUTF8:
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @inb:  a pointer to an array of UTF-16 passed as a byte array
 * @inlenb:  the length of @in in UTF-16 chars
 *
 * Take a block of UTF-16 ushorts in and try to convert it to an UTF-8
 * block of chars out. This function assumes the endian property
 * is the same between the native type of this machine and the
 * inputed one.
 *
 * Returns the number of bytes written, or -1 if lack of space, or -2
 *     if the transcoding fails (if *in is not a valid utf16 string)
 * The value of *inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictable.
 */
unsafe extern "C" fn utf16be_to_utf8(
    mut out: *mut c_uchar,
    outlen: *mut c_int,
    inb: *const c_uchar,
    inlenb: *mut c_int,
) -> c_int {
    let outstart: *mut c_uchar = out;
    let mut processed: *const c_uchar = inb;

    let mut input: *mut c_ushort = inb as *mut c_ushort;

    let mut c: c_uint;
    let mut d: c_uint;

    let mut tmp: *mut c_uchar;
    let mut bits: c_int;

    if *outlen == 0 {
        *inlenb = 0;
        return 0;
    }
    let outend: *mut c_uchar = out.add(*outlen as usize);
    if (*inlenb % 2) == 1 {
        (*inlenb) -= 1;
    }
    let inlen: c_uint = *inlenb as u32 / 2;
    let inend: *mut c_ushort = input.add(inlen as usize);
    while input < inend && (out.offset_from(outstart) + 5 < *outlen as isize) {
        if XML_LITTLE_ENDIAN.load(Ordering::Relaxed) != 0 {
            tmp = input as *mut c_uchar;
            c = *tmp as _;
            tmp = tmp.add(1);
            c = (c << 8) | *tmp as u32;
            input = input.add(1);
        } else {
            c = *input as _;
            input = input.add(1);
        }
        if c & 0xFC00 == 0xD800 {
            /* surrogates */
            if input >= inend {
                /* handle split mutli-byte characters */
                break;
            }
            if XML_LITTLE_ENDIAN.load(Ordering::Relaxed) != 0 {
                tmp = input as *mut c_uchar;
                d = *tmp as _;
                tmp = tmp.add(1);
                d = (d << 8) | *tmp as u32;
                input = input.add(1);
            } else {
                d = *input as _;
                input = input.add(1);
            }
            if d & 0xFC00 == 0xDC00 {
                c &= 0x03FF;
                c <<= 10;
                c |= d & 0x03FF;
                c += 0x10000;
            } else {
                *outlen = out.offset_from(outstart) as _;
                *inlenb = processed.offset_from(inb) as _;
                return -2;
            }
        }

        /* assertion: c is a single UTF-4 value */
        if out >= outend {
            break;
        }
        if c < 0x80 {
            *out = c as _;
            out = out.add(1);
            bits = -6;
        } else if c < 0x800 {
            *out = ((c >> 6) & 0x1F) as u8 | 0xC0;
            out = out.add(1);
            bits = 0;
        } else if c < 0x10000 {
            *out = ((c >> 12) & 0x0F) as u8 | 0xE0;
            out = out.add(1);
            bits = 6;
        } else {
            *out = ((c >> 18) & 0x07) as u8 | 0xF0;
            out = out.add(1);
            bits = 12;
        }

        while bits >= 0 {
            if out >= outend {
                break;
            }
            *out = ((c >> bits) & 0x3F) as u8 | 0x80;
            out = out.add(1);
            bits -= 6;
        }
        processed = input as *const c_uchar;
    }
    *outlen = out.offset_from(outstart) as _;
    *inlenb = processed.offset_from(inb) as _;
    *outlen
}

/**
 * UTF8ToUTF16BE:
 * @outb:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @outb
 * @in:  a pointer to an array of UTF-8 chars
 * @inlen:  the length of @in
 *
 * Take a block of UTF-8 chars in and try to convert it to an UTF-16BE
 * block of chars out.
 *
 * Returns the number of byte written, or -1 by lack of space, or -2
 *     if the transcoding failed.
 */
unsafe extern "C" fn utf8_to_utf16be(
    outb: *mut c_uchar,
    outlen: *mut c_int,
    mut input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    let mut out: *mut c_ushort = outb as *mut c_ushort;
    let mut processed: *const c_uchar = input;
    let instart: *const c_uchar = input;
    let outstart: *mut c_ushort = out;

    let mut c: c_uint;
    let mut d: c_uint;
    let mut trailing: c_int;
    let mut tmp: *mut c_uchar;
    let mut tmp1: c_ushort;
    let mut tmp2: c_ushort;

    /* UTF-16BE has no BOM */
    if outb.is_null() || outlen.is_null() || inlen.is_null() {
        return -1;
    }
    if input.is_null() {
        *outlen = 0;
        *inlen = 0;
        return 0;
    }
    let inend: *const c_uchar = input.add(*inlen as usize);
    let outend: *mut c_ushort = out.add(*outlen as usize / 2);
    while input < inend {
        d = *input as _;
        input = input.add(1);
        if d < 0x80 {
            c = d;
            trailing = 0;
        } else if d < 0xC0 {
            /* trailing byte in leading position */
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        } else if d < 0xE0 {
            c = d & 0x1F;
            trailing = 1;
        } else if d < 0xF0 {
            c = d & 0x0F;
            trailing = 2;
        } else if d < 0xF8 {
            c = d & 0x07;
            trailing = 3;
        } else {
            /* no chance for this in UTF-16 */
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        }

        if inend.offset_from(input) < trailing as isize {
            break;
        }

        while trailing != 0 {
            if input >= inend || {
                d = *input as _;
                input = input.add(1);
                d & 0xC0 != 0x80
            } {
                break;
            }
            c <<= 6;
            c |= d & 0x3F;
            trailing -= 1;
        }

        /* assertion: c is a single UTF-4 value */
        if c < 0x10000 {
            if out >= outend {
                break;
            }
            if XML_LITTLE_ENDIAN.load(Ordering::Relaxed) != 0 {
                tmp = out as *mut c_uchar;
                *tmp = (c >> 8) as u8;
                *tmp.add(1) = c as c_uchar; /* Explicit truncation */
                out = out.add(1);
            } else {
                *out = c as _;
                out = out.add(1);
            }
        } else if c < 0x110000 {
            if out.add(1) >= outend {
                break;
            }
            c -= 0x10000;
            if XML_LITTLE_ENDIAN.load(Ordering::Relaxed) != 0 {
                tmp1 = 0xD800 | (c >> 10) as u16;
                tmp = out as *mut c_uchar;
                *tmp = (tmp1 >> 8) as u8;
                *tmp.add(1) = tmp1 as c_uchar; /* Explicit truncation */
                out = out.add(1);

                tmp2 = 0xDC00 | (c & 0x03FF) as u16;
                tmp = out as *mut c_uchar;
                *tmp = (tmp2 >> 8) as u8;
                *tmp.add(1) = tmp2 as c_uchar; /* Explicit truncation */
                out = out.add(1);
            } else {
                *out = 0xD800 | (c >> 10) as u16;
                out = out.add(1);
                *out = 0xDC00 | (c & 0x03FF) as u16;
                out = out.add(1);
            }
        } else {
            break;
        }
        processed = input;
    }
    *outlen = out.offset_from(outstart) as i32 * 2;
    *inlen = processed.offset_from(instart) as _;
    *outlen
}

/**
 * UTF8ToUTF16:
 * @outb:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @outb
 * @in:  a pointer to an array of UTF-8 chars
 * @inlen:  the length of @in
 *
 * Take a block of UTF-8 chars in and try to convert it to an UTF-16
 * block of chars out.
 *
 * Returns the number of bytes written, or -1 if lack of space, or -2
 *     if the transcoding failed.
 */
unsafe extern "C" fn utf8_to_utf16(
    outb: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    if input.is_null() {
        /*
         * initialization, add the Byte Order Mark for UTF-16LE
         */
        if *outlen >= 2 {
            *outb.add(0) = 0xFF;
            *outb.add(1) = 0xFE;
            *outlen = 2;
            *inlen = 0;
            return 2;
        }
        *outlen = 0;
        *inlen = 0;
        return 0;
    }
    utf8_to_utf16le(outb, outlen, input, inlen)
}

/**
 * asciiToUTF8:
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @in:  a pointer to an array of ASCII chars
 * @inlen:  the length of @in
 *
 * Take a block of ASCII chars in and try to convert it to an UTF-8
 * block of chars out.
 * Returns 0 if success, or -1 otherwise
 * The value of @inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictable.
 * The value of @outlen after return is the number of octets produced.
 */
unsafe extern "C" fn ascii_to_utf8(
    mut out: *mut c_uchar,
    outlen: *mut c_int,
    mut input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    let outstart: *mut c_uchar = out;
    let base: *const c_uchar = input;
    let mut processed: *const c_uchar = input;
    let outend: *mut c_uchar = out.add(*outlen as usize);

    let mut c: c_uint;

    let inend: *const c_uchar = input.add(*inlen as usize);
    while input < inend && out.offset_from(outstart) + 5 < *outlen as isize {
        c = *input as _;
        input = input.add(1);

        if out >= outend {
            break;
        }
        if c < 0x80 {
            *out = c as _;
            out = out.add(1);
        } else {
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(base) as _;
            return -2;
        }

        processed = input as *const c_uchar;
    }
    *outlen = out.offset_from(outstart) as _;
    *inlen = processed.offset_from(base) as _;
    *outlen
}

/**
 * UTF8Toascii:
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @in:  a pointer to an array of UTF-8 chars
 * @inlen:  the length of @in
 *
 * Take a block of UTF-8 chars in and try to convert it to an ASCII
 * block of chars out.
 *
 * Returns 0 if success, -2 if the transcoding fails, or -1 otherwise
 * The value of @inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictable.
 * The value of @outlen after return is the number of octets produced.
 */
unsafe extern "C" fn utf8_to_ascii(
    mut out: *mut c_uchar,
    outlen: *mut c_int,
    mut input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    let mut processed: *const c_uchar = input;

    let outstart: *const c_uchar = out;
    let instart: *const c_uchar = input;

    let mut c: c_uint;
    let mut d: c_uint;
    let mut trailing: c_int;

    if out.is_null() || outlen.is_null() || inlen.is_null() {
        return -1;
    }
    if input.is_null() {
        /*
         * initialization nothing to do
         */
        *outlen = 0;
        *inlen = 0;
        return 0;
    }
    let inend: *const c_uchar = input.add(*inlen as usize) as _;
    let outend: *const c_uchar = out.add(*outlen as usize) as _;
    while input < inend {
        d = *input as _;
        input = input.add(1);
        if d < 0x80 {
            c = d;
            trailing = 0;
        } else if d < 0xC0 {
            /* trailing byte in leading position */
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        } else if d < 0xE0 {
            c = d & 0x1F;
            trailing = 1;
        } else if d < 0xF0 {
            c = d & 0x0F;
            trailing = 2;
        } else if d < 0xF8 {
            c = d & 0x07;
            trailing = 3;
        } else {
            /* no chance for this in Ascii */
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        }

        if inend.offset_from(input) < trailing as isize {
            break;
        }

        while trailing != 0 {
            if input >= inend || {
                d = *input as _;
                input = input.add(1);
                d & 0xC0 != 0x80
            } {
                break;
            }
            c <<= 6;
            c |= d & 0x3F;
            trailing -= 1;
        }

        /* assertion: c is a single UTF-4 value */
        if c < 0x80 {
            if out >= outend as _ {
                break;
            }
            *out = c as _;
            out = out.add(1);
        } else {
            /* no chance for this in Ascii */
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        }
        processed = input;
    }
    *outlen = out.offset_from(outstart) as _;
    *inlen = processed.offset_from(instart) as _;
    *outlen
}

static mut DEFAULT_HANDLERS: &mut [XmlCharEncodingHandler] = &mut [
    MAKE_HANDLER!(c"UTF-8".as_ptr(), Some(utf8_to_utf8), Some(utf8_to_utf8)),
    #[cfg(feature = "output")]
    MAKE_HANDLER!(
        c"UTF-16LE".as_ptr(),
        Some(utf16le_to_utf8),
        Some(utf8_to_utf16le)
    ),
    #[cfg(feature = "output")]
    MAKE_HANDLER!(
        c"UTF-16BE".as_ptr(),
        Some(utf16be_to_utf8),
        Some(utf8_to_utf16be)
    ),
    #[cfg(feature = "output")]
    MAKE_HANDLER!(
        c"UTF-16".as_ptr(),
        Some(utf16le_to_utf8),
        Some(utf8_to_utf16)
    ),
    #[cfg(feature = "output")]
    MAKE_HANDLER!(
        c"ISO-8859-1".as_ptr(),
        Some(iso_lat1_to_utf8),
        Some(utf8_to_iso_lat1)
    ),
    #[cfg(feature = "output")]
    MAKE_HANDLER!(c"ASCII".as_ptr(), Some(ascii_to_utf8), Some(utf8_to_ascii)),
    #[cfg(feature = "output")]
    MAKE_HANDLER!(
        c"US-ASCII".as_ptr(),
        Some(ascii_to_utf8),
        Some(utf8_to_ascii)
    ),
    #[cfg(all(feature = "output", feature = "html"))]
    MAKE_HANDLER!(c"HTML".as_ptr(), None, Some(utf8_to_html)),
    #[cfg(not(feature = "output"))]
    MAKE_HANDLER!(c"UTF-16LE".as_ptr(), Some(utf16le_to_utf8), None),
    #[cfg(not(feature = "output"))]
    MAKE_HANDLER!(c"UTF-16BE".as_ptr(), Some(utf16be_to_utf8), None),
    #[cfg(not(feature = "output"))]
    MAKE_HANDLER!(c"UTF-16".as_ptr(), Some(utf16le_to_utf8), None),
    #[cfg(not(feature = "output"))]
    MAKE_HANDLER!(c"ISO-8859-1".as_ptr(), Some(iso_lat1_to_utf8), None),
    #[cfg(not(feature = "output"))]
    MAKE_HANDLER!(c"ASCII".as_ptr(), Some(ascii_to_utf8), None),
    #[cfg(not(feature = "output"))]
    MAKE_HANDLER!(c"US-ASCII".as_ptr(), Some(ascii_to_utf8), None),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-2".as_ptr(),
        Some(iso8859_2_to_utf8),
        Some(utf8_to_iso8859_2)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-3".as_ptr(),
        Some(iso8859_3_to_utf8),
        Some(utf8_to_iso8859_3)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-4".as_ptr(),
        Some(iso8859_4_to_utf8),
        Some(utf8_to_iso8859_4)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-5".as_ptr(),
        Some(iso8859_5_to_utf8),
        Some(utf8_to_iso8859_5)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-6".as_ptr(),
        Some(iso8859_6_to_utf8),
        Some(utf8_to_iso8859_6)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-7".as_ptr(),
        Some(iso8859_7_to_utf8),
        Some(utf8_to_iso8859_7)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-8".as_ptr(),
        Some(iso8859_8_to_utf8),
        Some(utf8_to_iso8859_8)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-9".as_ptr(),
        Some(iso8859_9_to_utf8),
        Some(utf8_to_iso8859_9)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-10".as_ptr(),
        Some(iso8859_10_to_utf8),
        Some(utf8_to_iso8859_10)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-11".as_ptr(),
        Some(iso8859_11_to_utf8),
        Some(utf8_to_iso8859_11)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-13".as_ptr(),
        Some(iso8859_13_to_utf8),
        Some(utf8_to_iso8859_13)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-14".as_ptr(),
        Some(iso8859_14_to_utf8),
        Some(utf8_to_iso8859_14)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-15".as_ptr(),
        Some(iso8859_15_to_utf8),
        Some(utf8_to_iso8859_15)
    ),
    #[cfg(feature = "libxml_iso8859x")]
    MAKE_HANDLER!(
        c"ISO-8859-16".as_ptr(),
        Some(iso8859_16_to_utf8),
        Some(utf8_to_iso8859_16)
    ),
];

static XML_UTF16_LEHANDLER: &XmlCharEncodingHandler = unsafe { &DEFAULT_HANDLERS[1] };
static XML_UTF16_BEHANDLER: &XmlCharEncodingHandler = unsafe { &DEFAULT_HANDLERS[2] };

/**
 * xmlGetCharEncodingHandler:
 * @enc:  an xmlCharEncoding value.
 *
 * Search in the registered set the handler able to read/write that encoding.
 *
 * Returns the handler or NULL if not found
 */
pub unsafe extern "C" fn xml_get_char_encoding_handler(
    enc: XmlCharEncoding,
) -> XmlCharEncodingHandlerPtr {
    let mut handler: XmlCharEncodingHandlerPtr;

    match enc {
        XmlCharEncoding::Error => {
            return null_mut();
        }
        XmlCharEncoding::None => {
            return null_mut();
        }
        XmlCharEncoding::UTF8 => {
            return null_mut();
        }
        XmlCharEncoding::UTF16LE => {
            return XML_UTF16_LEHANDLER as *const XmlCharEncodingHandler as _;
        }
        XmlCharEncoding::UTF16BE => {
            return XML_UTF16_BEHANDLER as *const XmlCharEncodingHandler as _;
        }
        XmlCharEncoding::EBCDIC => {
            handler = xml_find_char_encoding_handler(c"EBCDIC".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
            handler = xml_find_char_encoding_handler(c"ebcdic".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
            handler = xml_find_char_encoding_handler(c"EBCDIC-US".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
            handler = xml_find_char_encoding_handler(c"IBM-037".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::UCS4BE => {
            handler = xml_find_char_encoding_handler(c"ISO-10646-UCS-4".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
            handler = xml_find_char_encoding_handler(c"UCS-4".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
            handler = xml_find_char_encoding_handler(c"UCS4".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::UCS4LE => {
            handler = xml_find_char_encoding_handler(c"ISO-10646-UCS-4".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
            handler = xml_find_char_encoding_handler(c"UCS-4".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
            handler = xml_find_char_encoding_handler(c"UCS4".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::UCS4_2143 => {}
        XmlCharEncoding::UCS4_3412 => {}
        XmlCharEncoding::UCS2 => {
            handler = xml_find_char_encoding_handler(c"ISO-10646-UCS-2".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
            handler = xml_find_char_encoding_handler(c"UCS-2".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
            handler = xml_find_char_encoding_handler(c"UCS2".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }

        /*
         * We used to keep ISO Latin encodings native in the
         * generated data. This led to so many problems that
         * this has been removed. One can still change this
         * back by registering no-ops encoders for those
         */
        XmlCharEncoding::ISO8859_1 => {
            handler = xml_find_char_encoding_handler(c"ISO-8859-1".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::ISO8859_2 => {
            handler = xml_find_char_encoding_handler(c"ISO-8859-2".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::ISO8859_3 => {
            handler = xml_find_char_encoding_handler(c"ISO-8859-3".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::ISO8859_4 => {
            handler = xml_find_char_encoding_handler(c"ISO-8859-4".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::ISO8859_5 => {
            handler = xml_find_char_encoding_handler(c"ISO-8859-5".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::ISO8859_6 => {
            handler = xml_find_char_encoding_handler(c"ISO-8859-6".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::ISO8859_7 => {
            handler = xml_find_char_encoding_handler(c"ISO-8859-7".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::ISO8859_8 => {
            handler = xml_find_char_encoding_handler(c"ISO-8859-8".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::ISO8859_9 => {
            handler = xml_find_char_encoding_handler(c"ISO-8859-9".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }

        XmlCharEncoding::ISO2022JP => {
            handler = xml_find_char_encoding_handler(c"ISO-2022-JP".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::ShiftJIS => {
            handler = xml_find_char_encoding_handler(c"SHIFT-JIS".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
            handler = xml_find_char_encoding_handler(c"SHIFT_JIS".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
            handler = xml_find_char_encoding_handler(c"Shift_JIS".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        XmlCharEncoding::EUCJP => {
            handler = xml_find_char_encoding_handler(c"EUC-JP".as_ptr() as _);
            if !handler.is_null() {
                return handler;
            }
        }
        _ => {}
    }

    null_mut()
}

static NUM_DEFAULT_HANDLERS: usize = unsafe { DEFAULT_HANDLERS.len() };

/**
 * xmlFindCharEncodingHandler:
 * @name:  a string describing the c_char encoding.
 *
 * Search in the registered set the handler able to read/write that encoding
 * or create a new one.
 *
 * Returns the handler or NULL if not found
 */
pub unsafe extern "C" fn xml_find_char_encoding_handler(
    mut name: *const c_char,
) -> XmlCharEncodingHandlerPtr {
    let mut upper: [c_char; 100] = [0; 100];

    if name.is_null() {
        return null_mut();
    }
    if *name.add(0) == 0 {
        return null_mut();
    }

    /*
     * Do the alias resolution
     */
    let norig: *const c_char = name;
    let nalias: *const c_char = xml_get_encoding_alias(name);
    if !nalias.is_null() {
        name = nalias;
    }

    /*
     * Check first for directly registered encoding names
     */
    for i in 0..upper.len() - 1 {
        upper[i] = (*name.add(i) as c_uchar).to_ascii_uppercase() as c_char;
        if upper[i] == 0 {
            break;
        }
    }
    *upper.last_mut().unwrap() = 0;

    for i in 0..NUM_DEFAULT_HANDLERS {
        if strcmp(
            upper.as_ptr() as _,
            DEFAULT_HANDLERS[i].name.load(Ordering::Relaxed) as _,
        ) == 0
        {
            return addr_of_mut!(DEFAULT_HANDLERS[i]) as XmlCharEncodingHandlerPtr;
        }
    }

    let handlers = HANDLERS.load(Ordering::Relaxed);
    if !handlers.is_null() {
        for i in 0..NB_CHAR_ENCODING_HANDLER.load(Ordering::Relaxed) {
            if strcmp(
                upper.as_ptr() as _,
                (*(*handlers.add(i))).name.load(Ordering::Relaxed) as _,
            ) == 0
            {
                return *handlers.add(i);
            }
        }
    }

    /*
     * Fallback using the canonical names
     */
    let alias: XmlCharEncoding = xml_parse_char_encoding(norig);
    if !matches!(alias, XmlCharEncoding::Error) {
        let canon: *const c_char = xml_get_char_encoding_name(alias);
        if !canon.is_null() && strcmp(name, canon) != 0 {
            return xml_find_char_encoding_handler(canon);
        }
    }

    /* If "none of the above", give up */
    null_mut()
}

/**
 * xmlNewCharEncodingHandler:
 * @name:  the encoding name, in UTF-8 format (ASCII actually)
 * @input:  the xmlCharEncodingInputFunc to read that encoding
 * @output:  the xmlCharEncodingOutputFunc to write that encoding
 *
 * Create and registers an xmlCharEncodingHandler.
 *
 * Returns the xmlCharEncodingHandlerPtr created (or NULL in case of error).
 */
pub unsafe extern "C" fn xml_new_char_encoding_handler(
    mut name: *const c_char,
    input: XmlCharEncodingInputFunc,
    output: XmlCharEncodingOutputFunc,
) -> XmlCharEncodingHandlerPtr {
    let mut upper: [c_char; 500] = [0; 500];

    /*
     * Do the alias resolution
     */
    let alias: *const c_char = xml_get_encoding_alias(name);
    if !alias.is_null() {
        name = alias;
    }

    /*
     * Keep only the uppercase version of the encoding.
     */
    if name.is_null() {
        xml_encoding_err(
            XmlParserErrors::XmlI18nNoName,
            c"xmlNewCharEncodingHandler : no name !\n".as_ptr() as _,
            null(),
        );
        return null_mut();
    }
    for i in 0..upper.len() - 1 {
        upper[i] = (*name.add(i) as c_uchar).to_ascii_uppercase() as c_char;
        if upper[i] == 0 {
            break;
        }
    }
    *upper.last_mut().unwrap() = 0;

    let up: *mut c_char = xml_mem_strdup(upper.as_ptr() as _) as _;
    if up.is_null() {
        xml_encoding_err_memory(c"xmlNewCharEncodingHandler : out of memory !\n".as_ptr() as _);
        return null_mut();
    }

    /*
     * allocate and fill-up an handler block.
     */
    let handler: XmlCharEncodingHandlerPtr =
        xml_malloc(size_of::<XmlCharEncodingHandler>()) as XmlCharEncodingHandlerPtr;
    if handler.is_null() {
        xml_free(up as _);
        xml_encoding_err_memory(c"xmlNewCharEncodingHandler : out of memory !\n".as_ptr() as _);
        return null_mut();
    }
    memset(handler as _, 0, size_of::<XmlCharEncodingHandler>());
    (*handler).input = Some(input);
    (*handler).output = Some(output);
    (*handler).name = AtomicPtr::new(up);

    /*
     * registers and returns the handler.
     */
    xml_register_char_encoding_handler(handler);
    handler
}

pub type XmlCharEncodingAliasPtr = *mut XmlCharEncodingAlias;
#[repr(C)]
pub struct XmlCharEncodingAlias {
    name: *const c_char,
    alias: *const c_char,
}

static XML_CHAR_ENCODING_ALIASES: AtomicPtr<XmlCharEncodingAlias> = AtomicPtr::new(null_mut());
static XML_CHAR_ENCODING_ALIASES_NB: AtomicUsize = AtomicUsize::new(0);
static XML_CHAR_ENCODING_ALIASES_MAX: AtomicUsize = AtomicUsize::new(0);

/*
 * Interfaces for encoding names and aliases.
 */
/**
 * xmlAddEncodingAlias:
 * @name:  the encoding name as parsed, in UTF-8 format (ASCII actually)
 * @alias:  the alias name as parsed, in UTF-8 format (ASCII actually)
 *
 * Registers an alias @alias for an encoding named @name. Existing alias
 * will be overwritten.
 *
 * Returns 0 in case of success, -1 in case of error
 */
pub unsafe extern "C" fn xml_add_encoding_alias(
    name: *const c_char,
    alias: *const c_char,
) -> c_int {
    let mut upper: [c_char; 100] = [0; 100];

    if name.is_null() || alias.is_null() {
        return -1;
    }

    for i in 0..upper.len() - 1 {
        upper[i] = (*alias.add(i) as c_uchar).to_ascii_uppercase() as c_char;
        if upper[i] == 0 {
            break;
        }
    }
    *upper.last_mut().unwrap() = 0;

    let mut aliases = XML_CHAR_ENCODING_ALIASES.load(Ordering::Acquire);
    let mut num_aliases = XML_CHAR_ENCODING_ALIASES_NB.load(Ordering::Relaxed);
    let mut max_aliases = XML_CHAR_ENCODING_ALIASES_MAX.load(Ordering::Relaxed);
    if aliases.is_null() {
        num_aliases = 0;
        max_aliases = 20;
        aliases =
            xml_malloc(max_aliases * size_of::<XmlCharEncodingAlias>()) as XmlCharEncodingAliasPtr;
        if aliases.is_null() {
            return -1;
        }
    } else if num_aliases >= max_aliases {
        max_aliases *= 2;
        aliases = xml_realloc(
            aliases as _,
            max_aliases * size_of::<XmlCharEncodingAlias>(),
        ) as XmlCharEncodingAliasPtr;
    }
    /*
     * Walk down the list looking for a definition of the alias
     */
    for i in 0..num_aliases {
        if strcmp((*aliases.add(i)).alias as _, upper.as_ptr() as _) == 0 {
            /*
             * Replace the definition.
             */
            xml_free((*aliases.add(i)).name as _);
            (*aliases.add(i)).name = xml_mem_strdup(name as _) as _;
            return 0;
        }
    }
    /*
     * Add the definition
     */
    (*aliases.add(num_aliases)).name = xml_mem_strdup(name as _) as _;
    (*aliases.add(num_aliases)).alias = xml_mem_strdup(upper.as_ptr() as _) as _;
    num_aliases += 1;
    XML_CHAR_ENCODING_ALIASES_MAX.store(max_aliases, Ordering::Release);
    XML_CHAR_ENCODING_ALIASES_NB.store(num_aliases, Ordering::Release);
    XML_CHAR_ENCODING_ALIASES.store(aliases, Ordering::Release);
    0
}

/**
 * xmlDelEncodingAlias:
 * @alias:  the alias name as parsed, in UTF-8 format (ASCII actually)
 *
 * Unregisters an encoding alias @alias
 *
 * Returns 0 in case of success, -1 in case of error
 */
pub unsafe extern "C" fn xml_del_encoding_alias(alias: *const c_char) -> c_int {
    if alias.is_null() {
        return -1;
    }

    let aliases = XML_CHAR_ENCODING_ALIASES.load(Ordering::Acquire);
    if aliases.is_null() {
        return -1;
    }
    /*
     * Walk down the list looking for a definition of the alias
     */
    let mut num_aliases = XML_CHAR_ENCODING_ALIASES_NB.load(Ordering::Acquire);
    for i in 0..XML_CHAR_ENCODING_ALIASES_NB.load(Ordering::Relaxed) {
        if strcmp((*aliases.add(i)).alias, alias) == 0 {
            xml_free((*aliases.add(i)).name as _);
            xml_free((*aliases.add(i)).alias as _);
            num_aliases -= 1;
            XML_CHAR_ENCODING_ALIASES_NB.store(num_aliases, Ordering::Release);
            memmove(
                aliases.add(i) as _,
                aliases.add(i + 1) as _,
                size_of::<XmlCharEncodingAlias>() * (num_aliases - i),
            );
            return 0;
        }
    }
    -1
}

/**
 * xmlGetEncodingAlias:
 * @alias:  the alias name as parsed, in UTF-8 format (ASCII actually)
 *
 * Lookup an encoding name for the given alias.
 *
 * Returns NULL if not found, otherwise the original name
 */
pub unsafe extern "C" fn xml_get_encoding_alias(alias: *const c_char) -> *const c_char {
    let mut upper: [c_char; 100] = [0; 100];

    if alias.is_null() {
        return null();
    }

    let aliases = XML_CHAR_ENCODING_ALIASES.load(Ordering::Acquire);
    if aliases.is_null() {
        return null();
    }

    for i in 0..upper.len() - 1 {
        upper[i] = (*alias.add(i) as c_uchar).to_ascii_uppercase() as c_char;
        if upper[i] == 0 {
            break;
        }
    }
    *upper.last_mut().unwrap() = 0;

    /*
     * Walk down the list looking for a definition of the alias
     */
    for i in 0..XML_CHAR_ENCODING_ALIASES_NB.load(Ordering::Acquire) {
        if strcmp((*aliases.add(i)).alias as _, upper.as_ptr() as _) == 0 {
            return (*aliases.add(i)).name;
        }
    }
    null()
}

/**
 * xmlCleanupEncodingAliases:
 *
 * Unregisters all aliases
 */
pub unsafe extern "C" fn xml_cleanup_encoding_aliases() {
    let aliases = XML_CHAR_ENCODING_ALIASES.load(Ordering::Acquire);
    if aliases.is_null() {
        return;
    }

    let num_aliases = XML_CHAR_ENCODING_ALIASES_NB.load(Ordering::Acquire);
    for i in 0..num_aliases {
        if !(*aliases.add(i)).name.is_null() {
            xml_free((*aliases.add(i)).name as _);
        }
        if !(*aliases.add(i)).alias.is_null() {
            xml_free((*aliases.add(i)).alias as _);
        }
    }
    XML_CHAR_ENCODING_ALIASES_NB.store(0, Ordering::Release);
    XML_CHAR_ENCODING_ALIASES_MAX.store(0, Ordering::Release);
    xml_free(aliases as _);
    XML_CHAR_ENCODING_ALIASES.store(null_mut(), Ordering::Release);
}

/**
 * xmlParseCharEncoding:
 * @name:  the encoding name as parsed, in UTF-8 format (ASCII actually)
 *
 * Compare the string to the encoding schemes already known. Note
 * that the comparison is case insensitive accordingly to the section
 * [XML] 4.3.3 Character Encoding in Entities.
 *
 * Returns one of the XML_CHAR_ENCODING_... values or XML_CHAR_ENCODING_NONE
 * if not recognized.
 */
pub unsafe extern "C" fn xml_parse_char_encoding(mut name: *const c_char) -> XmlCharEncoding {
    let mut upper: [c_char; 500] = [0; 500];

    if name.is_null() {
        return XmlCharEncoding::None;
    }

    /*
     * Do the alias resolution
     */
    let alias: *const c_char = xml_get_encoding_alias(name);
    if !alias.is_null() {
        name = alias;
    }

    for i in 0..upper.len() - 1 {
        upper[i] = (*name.add(i) as c_uchar).to_ascii_uppercase() as c_char;
        if upper[i] == 0 {
            break;
        }
    }
    *upper.last_mut().unwrap() = 0;

    if strcmp(upper.as_ptr() as _, c"".as_ptr() as _) == 0 {
        return XmlCharEncoding::None;
    }
    if strcmp(upper.as_ptr() as _, c"UTF-8".as_ptr() as _) == 0 {
        return XmlCharEncoding::UTF8;
    }
    if strcmp(upper.as_ptr() as _, c"UTF8".as_ptr() as _) == 0 {
        return XmlCharEncoding::UTF8;
    }

    /*
     * NOTE: if we were able to parse this, the endianness of UTF16 is
     *       already found and in use
     */
    if strcmp(upper.as_ptr() as _, c"UTF-16".as_ptr() as _) == 0 {
        return XmlCharEncoding::UTF16LE;
    }
    if strcmp(upper.as_ptr() as _, c"UTF16".as_ptr() as _) == 0 {
        return XmlCharEncoding::UTF16LE;
    }

    if strcmp(upper.as_ptr() as _, c"ISO-10646-UCS-2".as_ptr() as _) == 0 {
        return XmlCharEncoding::UCS2;
    }
    if strcmp(upper.as_ptr() as _, c"UCS-2".as_ptr() as _) == 0 {
        return XmlCharEncoding::UCS2;
    }
    if strcmp(upper.as_ptr() as _, c"UCS2".as_ptr() as _) == 0 {
        return XmlCharEncoding::UCS2;
    }

    /*
     * NOTE: if we were able to parse this, the endianness of UCS4 is
     *       already found and in use
     */
    if strcmp(upper.as_ptr() as _, c"ISO-10646-UCS-4".as_ptr() as _) == 0 {
        return XmlCharEncoding::UCS4LE;
    }
    if strcmp(upper.as_ptr() as _, c"UCS-4".as_ptr() as _) == 0 {
        return XmlCharEncoding::UCS4LE;
    }
    if strcmp(upper.as_ptr() as _, c"UCS4".as_ptr() as _) == 0 {
        return XmlCharEncoding::UCS4LE;
    }

    if strcmp(upper.as_ptr() as _, c"ISO-8859-1".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_1;
    }
    if strcmp(upper.as_ptr() as _, c"ISO-LATIN-1".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_1;
    }
    if strcmp(upper.as_ptr() as _, c"ISO LATIN 1".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_1;
    }

    if strcmp(upper.as_ptr() as _, c"ISO-8859-2".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_2;
    }
    if strcmp(upper.as_ptr() as _, c"ISO-LATIN-2".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_2;
    }
    if strcmp(upper.as_ptr() as _, c"ISO LATIN 2".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_2;
    }

    if strcmp(upper.as_ptr() as _, c"ISO-8859-3".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_3;
    }
    if strcmp(upper.as_ptr() as _, c"ISO-8859-4".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_4;
    }
    if strcmp(upper.as_ptr() as _, c"ISO-8859-5".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_5;
    }
    if strcmp(upper.as_ptr() as _, c"ISO-8859-6".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_6;
    }
    if strcmp(upper.as_ptr() as _, c"ISO-8859-7".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_7;
    }
    if strcmp(upper.as_ptr() as _, c"ISO-8859-8".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_8;
    }
    if strcmp(upper.as_ptr() as _, c"ISO-8859-9".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO8859_9;
    }

    if strcmp(upper.as_ptr() as _, c"ISO-2022-JP".as_ptr() as _) == 0 {
        return XmlCharEncoding::ISO2022JP;
    }
    if strcmp(upper.as_ptr() as _, c"SHIFT_JIS".as_ptr() as _) == 0 {
        return XmlCharEncoding::ShiftJIS;
    }
    if strcmp(upper.as_ptr() as _, c"EUC-JP".as_ptr() as _) == 0 {
        return XmlCharEncoding::EUCJP;
    }

    XmlCharEncoding::Error
}

/**
 * xmlGetCharEncodingName:
 * @enc:  the encoding
 *
 * The "canonical" name for XML encoding.
 * C.f. http://www.w3.org/TR/REC-xml#charencoding
 * Section 4.3.3  Character Encoding in Entities
 *
 * Returns the canonical name for the given encoding
 */
pub unsafe extern "C" fn xml_get_char_encoding_name(enc: XmlCharEncoding) -> *const c_char {
    match enc {
        XmlCharEncoding::Error => null(),
        XmlCharEncoding::None => null(),
        XmlCharEncoding::UTF8 => c"UTF-8".as_ptr() as _,
        XmlCharEncoding::UTF16LE => c"UTF-16".as_ptr() as _,
        XmlCharEncoding::UTF16BE => c"UTF-16".as_ptr() as _,
        XmlCharEncoding::EBCDIC => c"EBCDIC".as_ptr() as _,
        XmlCharEncoding::UCS4LE => c"ISO-10646-UCS-4".as_ptr() as _,
        XmlCharEncoding::UCS4BE => c"ISO-10646-UCS-4".as_ptr() as _,
        XmlCharEncoding::UCS4_2143 => c"ISO-10646-UCS-4".as_ptr() as _,
        XmlCharEncoding::UCS4_3412 => c"ISO-10646-UCS-4".as_ptr() as _,
        XmlCharEncoding::UCS2 => c"ISO-10646-UCS-2".as_ptr() as _,
        XmlCharEncoding::ISO8859_1 => c"ISO-8859-1".as_ptr() as _,
        XmlCharEncoding::ISO8859_2 => c"ISO-8859-2".as_ptr() as _,
        XmlCharEncoding::ISO8859_3 => c"ISO-8859-3".as_ptr() as _,
        XmlCharEncoding::ISO8859_4 => c"ISO-8859-4".as_ptr() as _,
        XmlCharEncoding::ISO8859_5 => c"ISO-8859-5".as_ptr() as _,
        XmlCharEncoding::ISO8859_6 => c"ISO-8859-6".as_ptr() as _,
        XmlCharEncoding::ISO8859_7 => c"ISO-8859-7".as_ptr() as _,
        XmlCharEncoding::ISO8859_8 => c"ISO-8859-8".as_ptr() as _,
        XmlCharEncoding::ISO8859_9 => c"ISO-8859-9".as_ptr() as _,
        XmlCharEncoding::ISO2022JP => c"ISO-2022-JP".as_ptr() as _,
        XmlCharEncoding::ShiftJIS => c"Shift-JIS".as_ptr() as _,
        XmlCharEncoding::EUCJP => c"EUC-JP".as_ptr() as _,
        XmlCharEncoding::ASCII => null(),
    }
}

/*
 * Interfaces directly used by the parsers.
 */
/**
 * xmlDetectCharEncoding:
 * @in:  a pointer to the first bytes of the XML entity, must be at least
 *       2 bytes long (at least 4 if encoding is UTF4 variant).
 * @len:  pointer to the length of the buffer
 *
 * Guess the encoding of the entity using the first bytes of the entity content
 * according to the non-normative appendix F of the XML-1.0 recommendation.
 *
 * Returns one of the XML_CHAR_ENCODING_... values.
 */
pub unsafe extern "C" fn xml_detect_char_encoding(
    input: *const c_uchar,
    len: c_int,
) -> XmlCharEncoding {
    if input.is_null() {
        return XmlCharEncoding::None;
    }
    if len >= 4 {
        if *input.add(0) == 0x00
            && *input.add(1) == 0x00
            && *input.add(2) == 0x00
            && *input.add(3) == 0x3C
        {
            return XmlCharEncoding::UCS4BE;
        }
        if *input.add(0) == 0x3C
            && *input.add(1) == 0x00
            && *input.add(2) == 0x00
            && *input.add(3) == 0x00
        {
            return XmlCharEncoding::UCS4LE;
        }
        if *input.add(0) == 0x00
            && *input.add(1) == 0x00
            && *input.add(2) == 0x3C
            && *input.add(3) == 0x00
        {
            return XmlCharEncoding::UCS4_2143;
        }
        if *input.add(0) == 0x00
            && *input.add(1) == 0x3C
            && *input.add(2) == 0x00
            && *input.add(3) == 0x00
        {
            return XmlCharEncoding::UCS4_3412;
        }
        if *input.add(0) == 0x4C
            && *input.add(1) == 0x6F
            && *input.add(2) == 0xA7
            && *input.add(3) == 0x94
        {
            return XmlCharEncoding::EBCDIC;
        }
        if *input.add(0) == 0x3C
            && *input.add(1) == 0x3F
            && *input.add(2) == 0x78
            && *input.add(3) == 0x6D
        {
            return XmlCharEncoding::UTF8;
        }
        /*
         * Although not part of the recommendation, we also
         * attempt an "auto-recognition" of UTF-16LE and
         * UTF-16BE encodings.
         */
        if *input.add(0) == 0x3C
            && *input.add(1) == 0x00
            && *input.add(2) == 0x3F
            && *input.add(3) == 0x00
        {
            return XmlCharEncoding::UTF16LE;
        }
        if *input.add(0) == 0x00
            && *input.add(1) == 0x3C
            && *input.add(2) == 0x00
            && *input.add(3) == 0x3F
        {
            return XmlCharEncoding::UTF16BE;
        }
    }
    if len >= 3 {
        /*
         * Errata on XML-1.0 June 20 2001
         * We now allow an UTF8 encoded BOM
         */
        if *input.add(0) == 0xEF && *input.add(1) == 0xBB && *input.add(2) == 0xBF {
            return XmlCharEncoding::UTF8;
        }
    }
    /* For UTF-16 we can recognize by the BOM */
    if len >= 2 {
        if *input.add(0) == 0xFE && *input.add(1) == 0xFF {
            return XmlCharEncoding::UTF16BE;
        }
        if *input.add(0) == 0xFF && *input.add(1) == 0xFE {
            return XmlCharEncoding::UTF16LE;
        }
    }
    XmlCharEncoding::None
}

/**
 * xmlEncOutputChunk:
 * @handler:  encoding handler
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @in:  a pointer to an array of input bytes
 * @inlen:  the length of @in
 *
 * Returns 0 if success, or
 *     -1 by lack of space, or
 *     -2 if the transcoding fails (for *in is not valid utf8 string or
 *        the result of transformation can't fit into the encoding we want), or
 *     -3 if there the last byte can't form a single output c_char.
 *     -4 if no output function was found.
 *
 * The value of @inlen after return is the number of octets consumed
 *     as the return value is 0, else unpredictable.
 * The value of @outlen after return is the number of octets produced.
 */
pub(crate) unsafe extern "C" fn xml_enc_output_chunk(
    handler: *mut XmlCharEncodingHandler,
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    let mut ret: c_int;

    if let Some(output) = (*handler).output {
        ret = output(out, outlen, input, inlen);
        if ret > 0 {
            ret = 0;
        }
    } else {
        *outlen = 0;
        *inlen = 0;
        ret = -4;
    }

    ret
}

// /**
//  * xmlCharEncOutFunc:
//  * @handler:    c_char encoding transformation data structure
//  * @out:  an xmlBuffer for the output.
//  * @in:  an xmlBuffer for the input
//  *
//  * Generic front-end for the encoding handler output function
//  * a first call with @in.is_null() has to be made firs to initiate the
//  * output in case of non-stateless encoding needing to initiate their
//  * state or the output (like the BOM in UTF16).
//  * In case of UTF8 sequence conversion errors for the given encoder,
//  * the content will be automatically remapped to a CharRef sequence.
//  *
//  * Returns the number of byte written if success, or
//  *     -1 general error
//  *     -2 if the transcoding fails (for *in is not valid utf8 string or
//  *        the result of transformation can't fit into the encoding we want), or
//  */
// pub unsafe extern "C" fn xml_char_enc_out_func(
//     handler: *mut XmlCharEncodingHandler,
//     out: XmlBufferPtr,
//     input: XmlBufferPtr,
// ) -> c_int {
//     let mut ret: c_int;
//     let mut written: c_int;
//     let mut writtentot: c_int = 0;
//     let mut toconv: c_int;

//     if handler.is_null() {
//         return -1;
//     }
//     if out.is_null() {
//         return -1;
//     }

//     // retry:
//     'retry: loop {
//         written = (*out).size as i32 - (*out).using as i32;

//         if written > 0 {
//             written -= 1; /* Gennady: count '/0' */
//         }

//         /*
//          * First specific handling of input = NULL, i.e. the initialization call
//          */
//         if input.is_null() {
//             toconv = 0;
//             /* TODO: Check return value. */
//             xml_enc_output_chunk(
//                 handler,
//                 (*out).content.add((*out).using as usize),
//                 addr_of_mut!(written),
//                 null_mut(),
//                 addr_of_mut!(toconv),
//             );
//             (*out).using += written as u32;
//             *(*out).content.add((*out).using as usize) = 0;
//             // #ifdef DEBUG_ENCODING
//             // 	xmlGenericError(xmlGenericErrorContext,
//             // 		"initialized encoder\n");
//             // #endif
//             return 0;
//         }

//         /*
//          * Conversion itself.
//          */
//         toconv = (*input).using as i32;
//         if toconv == 0 {
//             return 0;
//         }
//         if toconv * 4 >= written {
//             xml_buffer_grow(out, toconv as u32 * 4);
//             written = (*out).size as i32 - (*out).using as i32 - 1;
//         }
//         ret = xml_enc_output_chunk(
//             handler,
//             (*out).content.add((*out).using as usize),
//             addr_of_mut!(written),
//             (*input).content,
//             addr_of_mut!(toconv),
//         );
//         xml_buffer_shrink(input, toconv as u32);
//         (*out).using += written as u32;
//         writtentot += written;
//         *(*out).content.add((*out).using as usize) = 0;
//         if ret == -1 {
//             if written > 0 {
//                 /* Can be a limitation of iconv or uconv */
//                 // goto retry;
//                 continue 'retry;
//             }
//             ret = -3;
//         }

//         /*
//          * Attempt to handle error cases
//          */
//         match ret {
//             0 => {
//                 // #ifdef DEBUG_ENCODING
//                 // 	    xmlGenericError(xmlGenericErrorContext,
//                 // 		    "converted %d bytes to %d bytes of output\n",
//                 // 	            toconv, written);
//                 // #endif
//                 break;
//             }
//             -1 => {
//                 // #ifdef DEBUG_ENCODING
//                 // 	    xmlGenericError(xmlGenericErrorContext,
//                 // 		    "output conversion failed by lack of space\n");
//                 // #endif
//                 break;
//             }
//             -3 => {
//                 // #ifdef DEBUG_ENCODING
//                 // 	    xmlGenericError(xmlGenericErrorContext,"converted %d bytes to %d bytes of output %d left\n",
//                 // 	            toconv, written, (*input).using);
//                 // #endif
//                 break;
//             }
//             -4 => {
//                 xml_encoding_err(
//                     XmlParserErrors::XmlI18nNoOutput,
//                     c"xmlCharEncOutFunc: no output function !\n".as_ptr() as _,
//                     null(),
//                 );
//                 ret = -1;
//                 break;
//             }
//             -2 => {
//                 let mut charref: [XmlChar; 20] = [0; 20];
//                 let mut len: c_int = (*input).using as _;
//                 let utf: *const XmlChar = (*input).content as *const XmlChar;

//                 let cur: c_int = xml_get_utf8_char(utf, addr_of_mut!(len));
//                 if cur <= 0 {
//                     break;
//                 }

//                 // #ifdef DEBUG_ENCODING
//                 //             xmlGenericError(xmlGenericErrorContext,
//                 //                     "handling output conversion error\n");
//                 //             xmlGenericError(xmlGenericErrorContext,
//                 //                     "Bytes: 0x%02X 0x%02X 0x%02X 0x%02X\n",
//                 //                     *(*input).content.add(0), *(*input).content.add(1),
//                 //                     *(*input).content.add(2), *(*input).content.add(3));
//                 // #endif
//                 /*
//                  * Removes the UTF8 sequence, and replace it by a charref
//                  * and continue the transcoding phase, hoping the error
//                  * did not mangle the encoder state.
//                  */
//                 let charref_len: c_int = snprintf(
//                     addr_of_mut!(charref[0]) as _,
//                     size_of_val(&charref),
//                     c"&#%d;".as_ptr() as _,
//                     cur,
//                 );
//                 xml_buffer_shrink(input, len as u32);
//                 xml_buffer_grow(out, charref_len as u32 * 4);
//                 written = (*out).size as i32 - (*out).using as i32 - 1;
//                 toconv = charref_len;
//                 ret = xml_enc_output_chunk(
//                     handler,
//                     (*out).content.add((*out).using as usize),
//                     addr_of_mut!(written),
//                     charref.as_ptr() as _,
//                     addr_of_mut!(toconv),
//                 );

//                 if ret < 0 || (toconv != charref_len) {
//                     let mut buf: [c_char; 50] = [0; 50];

//                     snprintf(
//                         buf.as_mut_ptr() as _,
//                         49,
//                         c"0x%02X 0x%02X 0x%02X 0x%02X".as_ptr() as _,
//                         *(*input).content.add(0) as u32,
//                         *(*input).content.add(1) as u32,
//                         *(*input).content.add(2) as u32,
//                         *(*input).content.add(3) as u32,
//                     );
//                     buf[49] = 0;
//                     xml_encoding_err(
//                         XmlParserErrors::XmlI18nConvFailed,
//                         c"output conversion failed due to conv error, bytes %s\n".as_ptr() as _,
//                         buf.as_ptr() as _,
//                     );
//                     *(*input).content.add(0) = b' ';
//                     break;
//                 }

//                 (*out).using += written as u32;
//                 writtentot += written;
//                 *(*out).content.add((*out).using as usize) = 0;
//                 // goto retry;
//                 continue 'retry;
//             }
//             _ => {
//                 break;
//             }
//         }
//     }

//     if writtentot != 0 {
//         writtentot
//     } else {
//         ret
//     }
// }

// /**
//  * xmlCharEncInFunc:
//  * @handler:    c_char encoding transformation data structure
//  * @out:  an xmlBuffer for the output.
//  * @in:  an xmlBuffer for the input
//  *
//  * Generic front-end for the encoding handler input function
//  *
//  * Returns the number of byte written if success, or
//  *     -1 general error
//  *     -2 if the transcoding fails (for *in is not valid utf8 string or
//  *        the result of transformation can't fit into the encoding we want), or
//  */
// pub unsafe extern "C" fn xml_char_enc_in_func(
//     handler: *mut XmlCharEncodingHandler,
//     out: XmlBufferPtr,
//     input: XmlBufferPtr,
// ) -> c_int {
//     let mut ret: c_int;
//     let mut written: c_int;
//     let mut toconv: c_int;

//     if handler.is_null() {
//         return -1;
//     }
//     if out.is_null() {
//         return -1;
//     }
//     if input.is_null() {
//         return -1;
//     }

//     toconv = (*input).using as _;
//     if toconv == 0 {
//         return 0;
//     }
//     written = (*out).size as i32 - (*out).using as i32 - 1; /* count '\0' */
//     if toconv * 2 >= written {
//         xml_buffer_grow(out, (*out).size + toconv as u32 * 2);
//         written = (*out).size as i32 - (*out).using as i32 - 1;
//     }
//     ret = xml_enc_input_chunk(
//         handler,
//         (*out).content.add((*out).using as usize) as _,
//         addr_of_mut!(written),
//         (*input).content,
//         addr_of_mut!(toconv),
//         1,
//     );
//     xml_buffer_shrink(input, toconv as u32);
//     (*out).using += written as u32;
//     *(*out).content.add((*out).using as usize) = 0;
//     if ret == -1 {
//         ret = -3;
//     }

//     match ret {
//         0 => {
//             // #ifdef DEBUG_ENCODING
//             //             xmlGenericError(xmlGenericErrorContext,
//             //                             "converted %d bytes to %d bytes of input\n",
//             //                             toconv, written);
//             // #endif
//         }
//         -1 => {
//             // #ifdef DEBUG_ENCODING
//             //             xmlGenericError(xmlGenericErrorContext,
//             //                          "converted %d bytes to %d bytes of input, %d left\n",
//             //                             toconv, written, (*input).using);
//             // #endif
//         }
//         -3 => {
//             // #ifdef DEBUG_ENCODING
//             //             xmlGenericError(xmlGenericErrorContext,
//             //                         "converted %d bytes to %d bytes of input, %d left\n",
//             //                             toconv, written, (*input).using);
//             // #endif
//         }
//         -2 => {
//             let mut buf: [c_char; 50] = [0; 50];

//             snprintf(
//                 buf.as_mut_ptr() as _,
//                 49,
//                 c"0x%02X 0x%02X 0x%02X 0x%02X".as_ptr() as _,
//                 *(*input).content.add(0) as u32,
//                 *(*input).content.add(1) as u32,
//                 *(*input).content.add(2) as u32,
//                 *(*input).content.add(3) as u32,
//             );
//             buf[49] = 0;
//             xml_encoding_err(
//                 XmlParserErrors::XmlI18nConvFailed,
//                 c"input conversion failed due to input error, bytes %s\n".as_ptr() as _,
//                 buf.as_ptr() as _,
//             );
//         }
//         _ => {}
//     }
//     /*
//      * Ignore when input buffer is not on a boundary
//      */
//     if ret == -3 {
//         ret = 0;
//     }
//     if written != 0 {
//         written
//     } else {
//         ret
//     }
// }

// /**
//  * xmlCharEncFirstLine:
//  * @handler:    c_char encoding transformation data structure
//  * @out:  an xmlBuffer for the output.
//  * @in:  an xmlBuffer for the input
//  *
//  * DEPERECATED: Don't use.
//  */
// #[deprecated]
// pub unsafe extern "C" fn xml_char_enc_first_line(
//     handler: *mut XmlCharEncodingHandler,
//     out: XmlBufferPtr,
//     input: XmlBufferPtr,
// ) -> c_int {
//     xml_char_enc_in_func(handler, out, input)
// }

/**
 * xmlCharEncCloseFunc:
 * @handler:    char encoding transformation data structure
 *
 * Generic front-end for encoding handler close function
 *
 * Returns 0 if success, or -1 in case of error
 */
pub unsafe extern "C" fn xml_char_enc_close_func(handler: *mut XmlCharEncodingHandler) -> c_int {
    let ret: c_int = 0;
    let tofree: c_int = 0;

    if handler.is_null() {
        return -1;
    }

    for i in 0..NUM_DEFAULT_HANDLERS {
        if handler == DEFAULT_HANDLERS.as_mut_ptr().add(i) {
            return 0;
        }
    }

    let handlers = HANDLERS.load(Ordering::Acquire);
    if !handlers.is_null() {
        let num_handlers = NB_CHAR_ENCODING_HANDLER.load(Ordering::Acquire);
        for i in 0..num_handlers {
            if handler == *handlers.add(i) {
                return 0;
            }
        }
    }
    if tofree != 0 {
        /* free up only dynamic HANDLERS iconv/uconv */
        if !(*handler).name.load(Ordering::Relaxed).is_null() {
            xml_free((*handler).name.load(Ordering::Relaxed) as _);
        }
        (*handler).name = AtomicPtr::new(null_mut());
        xml_free(handler as _);
    }

    ret
}

/*
 * Export a few useful functions
 */

/**
 * UTF8Toisolat1:
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @in:  a pointer to an array of UTF-8 chars
 * @inlen:  the length of @in
 *
 * Take a block of UTF-8 chars in and try to convert it to an ISO Latin 1
 * block of chars out.
 *
 * Returns the number of bytes written if success, -2 if the transcoding fails, or -1 otherwise
 * The value of @inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictable.
 * The value of @outlen after return is the number of octets produced.
 */
#[cfg(feature = "output")]
pub unsafe extern "C" fn utf8_to_iso_lat1(
    mut out: *mut c_uchar,
    outlen: *mut c_int,
    mut input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    let mut processed: *const c_uchar = input;

    let outstart: *const c_uchar = out;
    let instart: *const c_uchar = input;

    let mut c: c_uint;
    let mut d: c_uint;
    let mut trailing: c_int;

    if out.is_null() || outlen.is_null() || inlen.is_null() {
        return -1;
    }
    if input.is_null() {
        /*
         * initialization nothing to do
         */
        *outlen = 0;
        *inlen = 0;
        return 0;
    }
    let inend: *const c_uchar = input.add(*inlen as usize);
    let outend: *const c_uchar = out.add(*outlen as usize);
    while input < inend {
        d = *input as _;
        input = input.add(1);
        if d < 0x80 {
            c = d;
            trailing = 0;
        } else if d < 0xC0 {
            /* trailing byte in leading position */
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        } else if d < 0xE0 {
            c = d & 0x1F;
            trailing = 1;
        } else if d < 0xF0 {
            c = d & 0x0F;
            trailing = 2;
        } else if d < 0xF8 {
            c = d & 0x07;
            trailing = 3;
        } else {
            /* no chance for this in IsoLat1 */
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        }

        if inend.offset_from(input) < trailing as isize {
            break;
        }

        while trailing != 0 {
            if input >= inend {
                break;
            }
            let res = {
                d = *input as _;
                input = input.add(1);
                d & 0xC0 != 0x80
            };
            if res {
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -2;
            }
            c <<= 6;
            c |= d & 0x3F;
            trailing -= 1;
        }

        /* assertion: c is a single UTF-4 value */
        if c <= 0xFF {
            if out >= outend as _ {
                break;
            }
            *out = c as _;
            out = out.add(1);
        } else {
            /* no chance for this in IsoLat1 */
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        }
        processed = input;
    }
    *outlen = out.offset_from(outstart) as _;
    *inlen = processed.offset_from(instart) as _;
    *outlen
}

/**
 * isolat1ToUTF8:
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @in:  a pointer to an array of ISO Latin 1 chars
 * @inlen:  the length of @in
 *
 * Take a block of ISO Latin 1 chars in and try to convert it to an UTF-8
 * block of chars out.
 * Returns the number of bytes written if success, or -1 otherwise
 * The value of @inlen after return is the number of octets consumed
 *     if the return value is positive, else unpredictable.
 * The value of @outlen after return is the number of octets produced.
 */
pub unsafe extern "C" fn iso_lat1_to_utf8(
    mut out: *mut c_uchar,
    outlen: *mut c_int,
    mut input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    let outstart: *mut c_uchar = out;
    let base: *const c_uchar = input;

    let mut instop: *const c_uchar;

    if out.is_null() || input.is_null() || outlen.is_null() || inlen.is_null() {
        return -1;
    }

    let outend: *mut c_uchar = out.add(*outlen as usize);
    let inend: *const c_uchar = input.add(*inlen as usize);
    instop = inend;

    while input < inend && (out < outend.offset(-1)) {
        if *input >= 0x80 {
            *out = (((*input) >> 6) & 0x1F) | 0xC0;
            out = out.add(1);
            *out = ((*input) & 0x3F) | 0x80;
            out = out.add(1);
            input = input.add(1);
        }
        if instop.offset_from(input) > outend.offset_from(out) {
            instop = input.offset(outend.offset_from(out));
        }
        while input < instop && *input < 0x80 {
            *out = *input;
            out = out.add(1);
            input = input.add(1);
        }
    }
    if input < inend && out < outend && *input < 0x80 {
        *out = *input;
        out = out.add(1);
        input = input.add(1);
    }
    *outlen = out.offset_from(outstart) as _;
    *inlen = input.offset_from(base) as _;
    *outlen
}

/**
 * UTF8ToISO8859x:
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @in:  a pointer to an array of UTF-8 chars
 * @inlen:  the length of @in
 * @xlattable: the 2-level transcoding table
 *
 * Take a block of UTF-8 chars in and try to convert it to an ISO 8859-*
 * block of chars out.
 *
 * Returns 0 if success, -2 if the transcoding fails, or -1 otherwise
 * The value of @inlen after return is the number of octets consumed
 *     as the return value is positive, else unpredictable.
 * The value of @outlen after return is the number of octets consumed.
 */
unsafe fn utf8_to_iso8859x(
    mut out: *mut c_uchar,
    outlen: *mut c_int,
    mut input: *const c_uchar,
    inlen: *mut c_int,
    xlattable: &[c_uchar],
) -> c_int {
    let outstart: *const c_uchar = out;
    let instart: *const c_uchar = input;
    let mut processed: *const c_uchar = input;

    if out.is_null() || outlen.is_null() || inlen.is_null() {
        return -1;
    }
    if input.is_null() {
        /*
         * initialization nothing to do
         */
        *outlen = 0;
        *inlen = 0;
        return 0;
    }
    let inend: *const c_uchar = input.add(*inlen as usize);
    while input < inend {
        let mut d: c_uchar = *input;
        input = input.add(1);
        if d < 0x80 {
            *out = d;
            out = out.add(1);
        } else if d < 0xC0 {
            /* trailing byte in leading position */
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        } else if d < 0xE0 {
            let mut c: c_uchar;
            if input >= inend {
                /* trailing byte not in input buffer */
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -3;
            }
            c = *input;
            input = input.add(1);
            if c & 0xC0 != 0x80 {
                /* not a trailing byte */
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -2;
            }
            c &= 0x3F;
            d &= 0x1F;
            d = xlattable[48 + c as usize + xlattable[d as usize] as usize * 64];
            if d == 0 {
                /* not in character set */
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -2;
            }
            *out = d;
            out = out.add(1);
        } else if d < 0xF0 {
            let mut c1: c_uchar;
            let mut c2: c_uchar;
            if input >= inend.sub(1) {
                /* trailing bytes not in input buffer */
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -3;
            }
            c1 = *input;
            input = input.add(1);
            if c1 & 0xC0 != 0x80 {
                /* not a trailing byte (c1) */
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -2;
            }
            c2 = *input;
            input = input.add(1);
            if c2 & 0xC0 != 0x80 {
                /* not a trailing byte (c2) */
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -2;
            }
            c1 &= 0x3F;
            c2 &= 0x3F;
            d &= 0x0F;
            d = xlattable[48
                + c2 as usize
                + xlattable[48 + c1 as usize + xlattable[32 + d as usize] as usize * 64] as usize
                    * 64];
            if d == 0 {
                /* not in character set */
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -2;
            }
            *out = d;
            out = out.add(1);
        } else {
            /* cannot transcode >= U+010000 */
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        }
        processed = input;
    }
    *outlen = out.offset_from(outstart) as _;
    *inlen = processed.offset_from(instart) as _;
    *outlen
}

/**
 * ISO8859xToUTF8
 * @out:  a pointer to an array of bytes to store the result
 * @outlen:  the length of @out
 * @in:  a pointer to an array of ISO Latin 1 chars
 * @inlen:  the length of @in
 *
 * Take a block of ISO 8859-* chars in and try to convert it to an UTF-8
 * block of chars out.
 * Returns 0 if success, or -1 otherwise
 * The value of @inlen after return is the number of octets consumed
 * The value of @outlen after return is the number of octets produced.
 */
unsafe fn iso8859x_to_utf8(
    mut out: *mut c_uchar,
    outlen: *mut c_int,
    mut input: *const c_uchar,
    inlen: *mut c_int,
    unicodetable: &[c_ushort],
) -> c_int {
    let outstart: *mut c_uchar = out;
    let instart: *const c_uchar = input;
    let mut instop: *const c_uchar;
    let mut c: c_uint;

    if out.is_null() || outlen.is_null() || inlen.is_null() || input.is_null() {
        return -1;
    }
    let outend: *mut c_uchar = out.add(*outlen as usize);
    let inend: *const c_uchar = input.add(*inlen as usize);
    instop = inend;

    while input < inend && out < outend.sub(2) {
        if *input >= 0x80 {
            c = unicodetable[*input as usize - 0x80] as u32;
            if c == 0 {
                /* undefined code point */
                *outlen = out.offset_from(outstart) as _;
                *inlen = input.offset_from(instart) as _;
                return -1;
            }
            if c < 0x800 {
                *out = ((c >> 6) & 0x1F) as u8 | 0xC0;
                out = out.add(1);
                *out = (c & 0x3F) as u8 | 0x80;
                out = out.add(1);
            } else {
                *out = ((c >> 12) & 0x0F) as u8 | 0xE0;
                out = out.add(1);
                *out = ((c >> 6) & 0x3F) as u8 | 0x80;
                out = out.add(1);
                *out = (c & 0x3F) as u8 | 0x80;
                out = out.add(1);
            }
            input = input.add(1);
        }
        if instop.offset_from(input) > outend.offset_from(out) {
            instop = input.add(outend.offset_from(out) as usize);
        }
        while *input < 0x80 && input < instop {
            *out = *input;
            input = input.add(1);
            out = out.add(1);
        }
    }
    if input < inend && out < outend && *input < 0x80 {
        *out = *input;
        input = input.add(1);
        out = out.add(1);
    }
    if input < inend && out < outend && *input < 0x80 {
        *out = *input;
        input = input.add(1);
        out = out.add(1);
    }
    *outlen = out.offset_from(outstart) as _;
    *inlen = input.offset_from(instart) as _;
    *outlen
}

unsafe extern "C" fn iso8859_2_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_2)
}
unsafe extern "C" fn utf8_to_iso8859_2(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_2)
}

unsafe extern "C" fn iso8859_3_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_3)
}
unsafe extern "C" fn utf8_to_iso8859_3(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_3)
}

unsafe extern "C" fn iso8859_4_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_4)
}
unsafe extern "C" fn utf8_to_iso8859_4(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_4)
}

unsafe extern "C" fn iso8859_5_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_5)
}
unsafe extern "C" fn utf8_to_iso8859_5(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_5)
}

unsafe extern "C" fn iso8859_6_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_6)
}
unsafe extern "C" fn utf8_to_iso8859_6(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_6)
}

unsafe extern "C" fn iso8859_7_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_7)
}
unsafe extern "C" fn utf8_to_iso8859_7(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_7)
}

unsafe extern "C" fn iso8859_8_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_8)
}
unsafe extern "C" fn utf8_to_iso8859_8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_8)
}

unsafe extern "C" fn iso8859_9_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_9)
}
unsafe extern "C" fn utf8_to_iso8859_9(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_9)
}

unsafe extern "C" fn iso8859_10_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_10)
}
unsafe extern "C" fn utf8_to_iso8859_10(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_10)
}

unsafe extern "C" fn iso8859_11_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_11)
}
unsafe extern "C" fn utf8_to_iso8859_11(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_11)
}

unsafe extern "C" fn iso8859_13_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_13)
}
unsafe extern "C" fn utf8_to_iso8859_13(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_13)
}

unsafe extern "C" fn iso8859_14_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_14)
}
unsafe extern "C" fn utf8_to_iso8859_14(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_14)
}

unsafe extern "C" fn iso8859_15_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_15)
}
unsafe extern "C" fn utf8_to_iso8859_15(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_15)
}

unsafe extern "C" fn iso8859_16_to_utf8(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    iso8859x_to_utf8(out, outlen, input, inlen, &XMLUNICODETABLE_ISO8859_16)
}
unsafe extern "C" fn utf8_to_iso8859_16(
    out: *mut c_uchar,
    outlen: *mut c_int,
    input: *const c_uchar,
    inlen: *mut c_int,
) -> c_int {
    utf8_to_iso8859x(out, outlen, input, inlen, &XMLTRANSCODETABLE_ISO8859_16)
}

const XMLUNICODETABLE_ISO8859_2: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x0104, 0x02d8, 0x0141,
    0x00a4, 0x013d, 0x015a, 0x00a7, 0x00a8, 0x0160, 0x015e, 0x0164, 0x0179, 0x00ad, 0x017d, 0x017b,
    0x00b0, 0x0105, 0x02db, 0x0142, 0x00b4, 0x013e, 0x015b, 0x02c7, 0x00b8, 0x0161, 0x015f, 0x0165,
    0x017a, 0x02dd, 0x017e, 0x017c, 0x0154, 0x00c1, 0x00c2, 0x0102, 0x00c4, 0x0139, 0x0106, 0x00c7,
    0x010c, 0x00c9, 0x0118, 0x00cb, 0x011a, 0x00cd, 0x00ce, 0x010e, 0x0110, 0x0143, 0x0147, 0x00d3,
    0x00d4, 0x0150, 0x00d6, 0x00d7, 0x0158, 0x016e, 0x00da, 0x0170, 0x00dc, 0x00dd, 0x0162, 0x00df,
    0x0155, 0x00e1, 0x00e2, 0x0103, 0x00e4, 0x013a, 0x0107, 0x00e7, 0x010d, 0x00e9, 0x0119, 0x00eb,
    0x011b, 0x00ed, 0x00ee, 0x010f, 0x0111, 0x0144, 0x0148, 0x00f3, 0x00f4, 0x0151, 0x00f6, 0x00f7,
    0x0159, 0x016f, 0x00fa, 0x0171, 0x00fc, 0x00fd, 0x0163, 0x02d9,
];

const XMLTRANSCODETABLE_ISO8859_2: [c_uchar; 48 + 6 * 64] = [
    0x00, 0x00, 0x01, 0x05, 0x02, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0x00, 0x00, 0x00, 0xa4, 0x00, 0x00, 0xa7, 0xa8, 0x00, 0x00, 0x00, 0x00, 0xad, 0x00, 0x00,
    0xb0, 0x00, 0x00, 0x00, 0xb4, 0x00, 0x00, 0x00, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xc3, 0xe3, 0xa1, 0xb1, 0xc6, 0xe6, 0x00, 0x00, 0x00, 0x00, 0xc8, 0xe8, 0xcf, 0xef,
    0xd0, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xca, 0xea, 0xcc, 0xec, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc5, 0xe5, 0x00, 0x00, 0xa5, 0xb5, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xb7, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa2, 0xff, 0x00, 0xb2, 0x00, 0xbd, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0xa3, 0xb3, 0xd1, 0xf1, 0x00, 0x00, 0xd2, 0xf2, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xd5, 0xf5, 0x00, 0x00, 0xc0, 0xe0, 0x00, 0x00, 0xd8, 0xf8, 0xa6, 0xb6, 0x00, 0x00, 0xaa, 0xba,
    0xa9, 0xb9, 0xde, 0xfe, 0xab, 0xbb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xd9, 0xf9,
    0xdb, 0xfb, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xac, 0xbc, 0xaf, 0xbf, 0xae, 0xbe, 0x00,
    0x00, 0xc1, 0xc2, 0x00, 0xc4, 0x00, 0x00, 0xc7, 0x00, 0xc9, 0x00, 0xcb, 0x00, 0xcd, 0xce, 0x00,
    0x00, 0x00, 0x00, 0xd3, 0xd4, 0x00, 0xd6, 0xd7, 0x00, 0x00, 0xda, 0x00, 0xdc, 0xdd, 0x00, 0xdf,
    0x00, 0xe1, 0xe2, 0x00, 0xe4, 0x00, 0x00, 0xe7, 0x00, 0xe9, 0x00, 0xeb, 0x00, 0xed, 0xee, 0x00,
    0x00, 0x00, 0x00, 0xf3, 0xf4, 0x00, 0xf6, 0xf7, 0x00, 0x00, 0xfa, 0x00, 0xfc, 0xfd, 0x00, 0x00,
];

const XMLUNICODETABLE_ISO8859_3: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x0126, 0x02d8, 0x00a3,
    0x00a4, 0x0000, 0x0124, 0x00a7, 0x00a8, 0x0130, 0x015e, 0x011e, 0x0134, 0x00ad, 0x0000, 0x017b,
    0x00b0, 0x0127, 0x00b2, 0x00b3, 0x00b4, 0x00b5, 0x0125, 0x00b7, 0x00b8, 0x0131, 0x015f, 0x011f,
    0x0135, 0x00bd, 0x0000, 0x017c, 0x00c0, 0x00c1, 0x00c2, 0x0000, 0x00c4, 0x010a, 0x0108, 0x00c7,
    0x00c8, 0x00c9, 0x00ca, 0x00cb, 0x00cc, 0x00cd, 0x00ce, 0x00cf, 0x0000, 0x00d1, 0x00d2, 0x00d3,
    0x00d4, 0x0120, 0x00d6, 0x00d7, 0x011c, 0x00d9, 0x00da, 0x00db, 0x00dc, 0x016c, 0x015c, 0x00df,
    0x00e0, 0x00e1, 0x00e2, 0x0000, 0x00e4, 0x010b, 0x0109, 0x00e7, 0x00e8, 0x00e9, 0x00ea, 0x00eb,
    0x00ec, 0x00ed, 0x00ee, 0x00ef, 0x0000, 0x00f1, 0x00f2, 0x00f3, 0x00f4, 0x0121, 0x00f6, 0x00f7,
    0x011d, 0x00f9, 0x00fa, 0x00fb, 0x00fc, 0x016d, 0x015d, 0x02d9,
];

const XMLTRANSCODETABLE_ISO8859_3: [c_uchar; 48 + 7 * 64] = [
    0x04, 0x00, 0x01, 0x06, 0x02, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0x00, 0x00, 0xa3, 0xa4, 0x00, 0x00, 0xa7, 0xa8, 0x00, 0x00, 0x00, 0x00, 0xad, 0x00, 0x00,
    0xb0, 0x00, 0xb2, 0xb3, 0xb4, 0xb5, 0x00, 0xb7, 0xb8, 0x00, 0x00, 0x00, 0x00, 0xbd, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc6, 0xe6, 0xc5, 0xe5, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xd8, 0xf8, 0xab, 0xbb,
    0xd5, 0xf5, 0x00, 0x00, 0xa6, 0xb6, 0xa1, 0xb1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xa9, 0xb9, 0x00, 0x00, 0xac, 0xbc, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa2, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xde, 0xfe, 0xaa, 0xba,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xdd, 0xfd, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xaf, 0xbf, 0x00, 0x00, 0x00,
    0xc0, 0xc1, 0xc2, 0x00, 0xc4, 0x00, 0x00, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0x00, 0xd1, 0xd2, 0xd3, 0xd4, 0x00, 0xd6, 0xd7, 0x00, 0xd9, 0xda, 0xdb, 0xdc, 0x00, 0x00, 0xdf,
    0xe0, 0xe1, 0xe2, 0x00, 0xe4, 0x00, 0x00, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0x00, 0xf1, 0xf2, 0xf3, 0xf4, 0x00, 0xf6, 0xf7, 0x00, 0xf9, 0xfa, 0xfb, 0xfc, 0x00, 0x00, 0x00,
];

const XMLUNICODETABLE_ISO8859_4: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x0104, 0x0138, 0x0156,
    0x00a4, 0x0128, 0x013b, 0x00a7, 0x00a8, 0x0160, 0x0112, 0x0122, 0x0166, 0x00ad, 0x017d, 0x00af,
    0x00b0, 0x0105, 0x02db, 0x0157, 0x00b4, 0x0129, 0x013c, 0x02c7, 0x00b8, 0x0161, 0x0113, 0x0123,
    0x0167, 0x014a, 0x017e, 0x014b, 0x0100, 0x00c1, 0x00c2, 0x00c3, 0x00c4, 0x00c5, 0x00c6, 0x012e,
    0x010c, 0x00c9, 0x0118, 0x00cb, 0x0116, 0x00cd, 0x00ce, 0x012a, 0x0110, 0x0145, 0x014c, 0x0136,
    0x00d4, 0x00d5, 0x00d6, 0x00d7, 0x00d8, 0x0172, 0x00da, 0x00db, 0x00dc, 0x0168, 0x016a, 0x00df,
    0x0101, 0x00e1, 0x00e2, 0x00e3, 0x00e4, 0x00e5, 0x00e6, 0x012f, 0x010d, 0x00e9, 0x0119, 0x00eb,
    0x0117, 0x00ed, 0x00ee, 0x012b, 0x0111, 0x0146, 0x014d, 0x0137, 0x00f4, 0x00f5, 0x00f6, 0x00f7,
    0x00f8, 0x0173, 0x00fa, 0x00fb, 0x00fc, 0x0169, 0x016b, 0x02d9,
];

const XMLTRANSCODETABLE_ISO8859_4: [c_uchar; 48 + 6 * 64] = [
    0x00, 0x00, 0x01, 0x05, 0x02, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0x00, 0x00, 0x00, 0xa4, 0x00, 0x00, 0xa7, 0xa8, 0x00, 0x00, 0x00, 0x00, 0xad, 0x00, 0xaf,
    0xb0, 0x00, 0x00, 0x00, 0xb4, 0x00, 0x00, 0x00, 0xb8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xc0, 0xe0, 0x00, 0x00, 0xa1, 0xb1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc8, 0xe8, 0x00, 0x00,
    0xd0, 0xf0, 0xaa, 0xba, 0x00, 0x00, 0xcc, 0xec, 0xca, 0xea, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xab, 0xbb, 0x00, 0x00, 0x00, 0x00, 0xa5, 0xb5, 0xcf, 0xef, 0x00, 0x00, 0xc7, 0xe7,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xd3, 0xf3, 0xa2, 0x00, 0x00, 0xa6, 0xb6, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0xd1, 0xf1, 0x00, 0x00, 0x00, 0xbd, 0xbf, 0xd2, 0xf2, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa3, 0xb3, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xa9, 0xb9, 0x00, 0x00, 0x00, 0x00, 0xac, 0xbc, 0xdd, 0xfd, 0xde, 0xfe, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xd9, 0xf9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xae, 0xbe, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xb7, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0x00, 0xb2, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0x00, 0x00, 0xc9, 0x00, 0xcb, 0x00, 0xcd, 0xce, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0x00, 0xda, 0xdb, 0xdc, 0x00, 0x00, 0xdf,
    0x00, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0x00, 0x00, 0xe9, 0x00, 0xeb, 0x00, 0xed, 0xee, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0x00, 0xfa, 0xfb, 0xfc, 0x00, 0x00, 0x00,
];

const XMLUNICODETABLE_ISO8859_5: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x0401, 0x0402, 0x0403,
    0x0404, 0x0405, 0x0406, 0x0407, 0x0408, 0x0409, 0x040a, 0x040b, 0x040c, 0x00ad, 0x040e, 0x040f,
    0x0410, 0x0411, 0x0412, 0x0413, 0x0414, 0x0415, 0x0416, 0x0417, 0x0418, 0x0419, 0x041a, 0x041b,
    0x041c, 0x041d, 0x041e, 0x041f, 0x0420, 0x0421, 0x0422, 0x0423, 0x0424, 0x0425, 0x0426, 0x0427,
    0x0428, 0x0429, 0x042a, 0x042b, 0x042c, 0x042d, 0x042e, 0x042f, 0x0430, 0x0431, 0x0432, 0x0433,
    0x0434, 0x0435, 0x0436, 0x0437, 0x0438, 0x0439, 0x043a, 0x043b, 0x043c, 0x043d, 0x043e, 0x043f,
    0x0440, 0x0441, 0x0442, 0x0443, 0x0444, 0x0445, 0x0446, 0x0447, 0x0448, 0x0449, 0x044a, 0x044b,
    0x044c, 0x044d, 0x044e, 0x044f, 0x2116, 0x0451, 0x0452, 0x0453, 0x0454, 0x0455, 0x0456, 0x0457,
    0x0458, 0x0459, 0x045a, 0x045b, 0x045c, 0x00a7, 0x045e, 0x045f,
];

const XMLTRANSCODETABLE_ISO8859_5: [c_uchar; 48 + 6 * 64] = [
    0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x02, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfd, 0x00, 0x00, 0x00, 0x00, 0x00, 0xad, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0x00, 0xae, 0xaf,
    0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
    0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0x00, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0x00, 0xfe, 0xff,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
];

const XMLUNICODETABLE_ISO8859_6: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x0000, 0x0000, 0x0000,
    0x00a4, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x060c, 0x00ad, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x061b,
    0x0000, 0x0000, 0x0000, 0x061f, 0x0000, 0x0621, 0x0622, 0x0623, 0x0624, 0x0625, 0x0626, 0x0627,
    0x0628, 0x0629, 0x062a, 0x062b, 0x062c, 0x062d, 0x062e, 0x062f, 0x0630, 0x0631, 0x0632, 0x0633,
    0x0634, 0x0635, 0x0636, 0x0637, 0x0638, 0x0639, 0x063a, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0640, 0x0641, 0x0642, 0x0643, 0x0644, 0x0645, 0x0646, 0x0647, 0x0648, 0x0649, 0x064a, 0x064b,
    0x064c, 0x064d, 0x064e, 0x064f, 0x0650, 0x0651, 0x0652, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
];

const XMLTRANSCODETABLE_ISO8859_6: [c_uchar; 48 + 5 * 64] = [
    0x02, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0x00, 0x00, 0x00, 0xa4, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xad, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xac, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xbb, 0x00, 0x00, 0x00, 0xbf,
    0x00, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf0, 0xf1, 0xf2, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
];

const XMLUNICODETABLE_ISO8859_7: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x2018, 0x2019, 0x00a3,
    0x0000, 0x0000, 0x00a6, 0x00a7, 0x00a8, 0x00a9, 0x0000, 0x00ab, 0x00ac, 0x00ad, 0x0000, 0x2015,
    0x00b0, 0x00b1, 0x00b2, 0x00b3, 0x0384, 0x0385, 0x0386, 0x00b7, 0x0388, 0x0389, 0x038a, 0x00bb,
    0x038c, 0x00bd, 0x038e, 0x038f, 0x0390, 0x0391, 0x0392, 0x0393, 0x0394, 0x0395, 0x0396, 0x0397,
    0x0398, 0x0399, 0x039a, 0x039b, 0x039c, 0x039d, 0x039e, 0x039f, 0x03a0, 0x03a1, 0x0000, 0x03a3,
    0x03a4, 0x03a5, 0x03a6, 0x03a7, 0x03a8, 0x03a9, 0x03aa, 0x03ab, 0x03ac, 0x03ad, 0x03ae, 0x03af,
    0x03b0, 0x03b1, 0x03b2, 0x03b3, 0x03b4, 0x03b5, 0x03b6, 0x03b7, 0x03b8, 0x03b9, 0x03ba, 0x03bb,
    0x03bc, 0x03bd, 0x03be, 0x03bf, 0x03c0, 0x03c1, 0x03c2, 0x03c3, 0x03c4, 0x03c5, 0x03c6, 0x03c7,
    0x03c8, 0x03c9, 0x03ca, 0x03cb, 0x03cc, 0x03cd, 0x03ce, 0x0000,
];

const XMLTRANSCODETABLE_ISO8859_7: [c_uchar; 48 + 7 * 64] = [
    0x04, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x05, 0x06,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0x00, 0x00, 0xa3, 0x00, 0x00, 0xa6, 0xa7, 0xa8, 0xa9, 0x00, 0xab, 0xac, 0xad, 0x00, 0x00,
    0xb0, 0xb1, 0xb2, 0xb3, 0x00, 0x00, 0x00, 0xb7, 0x00, 0x00, 0x00, 0xbb, 0x00, 0xbd, 0x00, 0x00,
    0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0xaf, 0x00, 0x00, 0xa1, 0xa2, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xb4, 0xb5, 0xb6, 0x00, 0xb8, 0xb9, 0xba, 0x00, 0xbc, 0x00, 0xbe, 0xbf,
    0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0xd0, 0xd1, 0x00, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
];

const XMLUNICODETABLE_ISO8859_8: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x0000, 0x00a2, 0x00a3,
    0x00a4, 0x00a5, 0x00a6, 0x00a7, 0x00a8, 0x00a9, 0x00d7, 0x00ab, 0x00ac, 0x00ad, 0x00ae, 0x00af,
    0x00b0, 0x00b1, 0x00b2, 0x00b3, 0x00b4, 0x00b5, 0x00b6, 0x00b7, 0x00b8, 0x00b9, 0x00f7, 0x00bb,
    0x00bc, 0x00bd, 0x00be, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000,
    0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x0000, 0x2017,
    0x05d0, 0x05d1, 0x05d2, 0x05d3, 0x05d4, 0x05d5, 0x05d6, 0x05d7, 0x05d8, 0x05d9, 0x05da, 0x05db,
    0x05dc, 0x05dd, 0x05de, 0x05df, 0x05e0, 0x05e1, 0x05e2, 0x05e3, 0x05e4, 0x05e5, 0x05e6, 0x05e7,
    0x05e8, 0x05e9, 0x05ea, 0x0000, 0x0000, 0x200e, 0x200f, 0x0000,
];

const XMLTRANSCODETABLE_ISO8859_8: [c_uchar; 48 + 7 * 64] = [
    0x02, 0x00, 0x01, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0x00, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0x00, 0xab, 0xac, 0xad, 0xae, 0xaf,
    0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0x00, 0xbb, 0xbc, 0xbd, 0xbe, 0x00,
    0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xba, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xfd, 0xfe,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xdf, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
];

const XMLUNICODETABLE_ISO8859_9: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x00a1, 0x00a2, 0x00a3,
    0x00a4, 0x00a5, 0x00a6, 0x00a7, 0x00a8, 0x00a9, 0x00aa, 0x00ab, 0x00ac, 0x00ad, 0x00ae, 0x00af,
    0x00b0, 0x00b1, 0x00b2, 0x00b3, 0x00b4, 0x00b5, 0x00b6, 0x00b7, 0x00b8, 0x00b9, 0x00ba, 0x00bb,
    0x00bc, 0x00bd, 0x00be, 0x00bf, 0x00c0, 0x00c1, 0x00c2, 0x00c3, 0x00c4, 0x00c5, 0x00c6, 0x00c7,
    0x00c8, 0x00c9, 0x00ca, 0x00cb, 0x00cc, 0x00cd, 0x00ce, 0x00cf, 0x011e, 0x00d1, 0x00d2, 0x00d3,
    0x00d4, 0x00d5, 0x00d6, 0x00d7, 0x00d8, 0x00d9, 0x00da, 0x00db, 0x00dc, 0x0130, 0x015e, 0x00df,
    0x00e0, 0x00e1, 0x00e2, 0x00e3, 0x00e4, 0x00e5, 0x00e6, 0x00e7, 0x00e8, 0x00e9, 0x00ea, 0x00eb,
    0x00ec, 0x00ed, 0x00ee, 0x00ef, 0x011f, 0x00f1, 0x00f2, 0x00f3, 0x00f4, 0x00f5, 0x00f6, 0x00f7,
    0x00f8, 0x00f9, 0x00fa, 0x00fb, 0x00fc, 0x0131, 0x015f, 0x00ff,
];

const XMLTRANSCODETABLE_ISO8859_9: [c_uchar; 48 + 5 * 64] = [
    0x00, 0x00, 0x01, 0x02, 0x03, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
    0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
    0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0x00, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0x00, 0x00, 0xdf,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0x00, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0x00, 0x00, 0xff,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xd0, 0xf0,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xdd, 0xfd, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xde, 0xfe,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
];

const XMLUNICODETABLE_ISO8859_10: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x0104, 0x0112, 0x0122,
    0x012a, 0x0128, 0x0136, 0x00a7, 0x013b, 0x0110, 0x0160, 0x0166, 0x017d, 0x00ad, 0x016a, 0x014a,
    0x00b0, 0x0105, 0x0113, 0x0123, 0x012b, 0x0129, 0x0137, 0x00b7, 0x013c, 0x0111, 0x0161, 0x0167,
    0x017e, 0x2015, 0x016b, 0x014b, 0x0100, 0x00c1, 0x00c2, 0x00c3, 0x00c4, 0x00c5, 0x00c6, 0x012e,
    0x010c, 0x00c9, 0x0118, 0x00cb, 0x0116, 0x00cd, 0x00ce, 0x00cf, 0x00d0, 0x0145, 0x014c, 0x00d3,
    0x00d4, 0x00d5, 0x00d6, 0x0168, 0x00d8, 0x0172, 0x00da, 0x00db, 0x00dc, 0x00dd, 0x00de, 0x00df,
    0x0101, 0x00e1, 0x00e2, 0x00e3, 0x00e4, 0x00e5, 0x00e6, 0x012f, 0x010d, 0x00e9, 0x0119, 0x00eb,
    0x0117, 0x00ed, 0x00ee, 0x00ef, 0x00f0, 0x0146, 0x014d, 0x00f3, 0x00f4, 0x00f5, 0x00f6, 0x0169,
    0x00f8, 0x0173, 0x00fa, 0x00fb, 0x00fc, 0x00fd, 0x00fe, 0x0138,
];

const XMLTRANSCODETABLE_ISO8859_10: [c_uchar; 48 + 7 * 64] = [
    0x00, 0x00, 0x01, 0x06, 0x02, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa7, 0x00, 0x00, 0x00, 0x00, 0x00, 0xad, 0x00, 0x00,
    0xb0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xb7, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xc0, 0xe0, 0x00, 0x00, 0xa1, 0xb1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xc8, 0xe8, 0x00, 0x00,
    0xa9, 0xb9, 0xa2, 0xb2, 0x00, 0x00, 0xcc, 0xec, 0xca, 0xea, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xa3, 0xb3, 0x00, 0x00, 0x00, 0x00, 0xa5, 0xb5, 0xa4, 0xb4, 0x00, 0x00, 0xc7, 0xe7,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa6, 0xb6, 0xff, 0x00, 0x00, 0xa8, 0xb8, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0xd1, 0xf1, 0x00, 0x00, 0x00, 0xaf, 0xbf, 0xd2, 0xf2, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xaa, 0xba, 0x00, 0x00, 0x00, 0x00, 0xab, 0xbb, 0xd7, 0xf7, 0xae, 0xbe, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xd9, 0xf9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xac, 0xbc, 0x00,
    0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0xbd, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0x00, 0x00, 0xc9, 0x00, 0xcb, 0x00, 0xcd, 0xce, 0xcf,
    0xd0, 0x00, 0x00, 0xd3, 0xd4, 0xd5, 0xd6, 0x00, 0xd8, 0x00, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
    0x00, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0x00, 0x00, 0xe9, 0x00, 0xeb, 0x00, 0xed, 0xee, 0xef,
    0xf0, 0x00, 0x00, 0xf3, 0xf4, 0xf5, 0xf6, 0x00, 0xf8, 0x00, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0x00,
];

const XMLUNICODETABLE_ISO8859_11: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x0e01, 0x0e02, 0x0e03,
    0x0e04, 0x0e05, 0x0e06, 0x0e07, 0x0e08, 0x0e09, 0x0e0a, 0x0e0b, 0x0e0c, 0x0e0d, 0x0e0e, 0x0e0f,
    0x0e10, 0x0e11, 0x0e12, 0x0e13, 0x0e14, 0x0e15, 0x0e16, 0x0e17, 0x0e18, 0x0e19, 0x0e1a, 0x0e1b,
    0x0e1c, 0x0e1d, 0x0e1e, 0x0e1f, 0x0e20, 0x0e21, 0x0e22, 0x0e23, 0x0e24, 0x0e25, 0x0e26, 0x0e27,
    0x0e28, 0x0e29, 0x0e2a, 0x0e2b, 0x0e2c, 0x0e2d, 0x0e2e, 0x0e2f, 0x0e30, 0x0e31, 0x0e32, 0x0e33,
    0x0e34, 0x0e35, 0x0e36, 0x0e37, 0x0e38, 0x0e39, 0x0e3a, 0x0000, 0x0000, 0x0000, 0x0000, 0x0e3f,
    0x0e40, 0x0e41, 0x0e42, 0x0e43, 0x0e44, 0x0e45, 0x0e46, 0x0e47, 0x0e48, 0x0e49, 0x0e4a, 0x0e4b,
    0x0e4c, 0x0e4d, 0x0e4e, 0x0e4f, 0x0e50, 0x0e51, 0x0e52, 0x0e53, 0x0e54, 0x0e55, 0x0e56, 0x0e57,
    0x0e58, 0x0e59, 0x0e5a, 0x0e5b, 0x0000, 0x0000, 0x0000, 0x0000,
];

const XMLTRANSCODETABLE_ISO8859_11: [c_uchar; 48 + 6 * 64] = [
    0x04, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7, 0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
    0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
    0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0x00, 0x00, 0x00, 0x00, 0xdf,
    0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
];

const XMLUNICODETABLE_ISO8859_13: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x201d, 0x00a2, 0x00a3,
    0x00a4, 0x201e, 0x00a6, 0x00a7, 0x00d8, 0x00a9, 0x0156, 0x00ab, 0x00ac, 0x00ad, 0x00ae, 0x00c6,
    0x00b0, 0x00b1, 0x00b2, 0x00b3, 0x201c, 0x00b5, 0x00b6, 0x00b7, 0x00f8, 0x00b9, 0x0157, 0x00bb,
    0x00bc, 0x00bd, 0x00be, 0x00e6, 0x0104, 0x012e, 0x0100, 0x0106, 0x00c4, 0x00c5, 0x0118, 0x0112,
    0x010c, 0x00c9, 0x0179, 0x0116, 0x0122, 0x0136, 0x012a, 0x013b, 0x0160, 0x0143, 0x0145, 0x00d3,
    0x014c, 0x00d5, 0x00d6, 0x00d7, 0x0172, 0x0141, 0x015a, 0x016a, 0x00dc, 0x017b, 0x017d, 0x00df,
    0x0105, 0x012f, 0x0101, 0x0107, 0x00e4, 0x00e5, 0x0119, 0x0113, 0x010d, 0x00e9, 0x017a, 0x0117,
    0x0123, 0x0137, 0x012b, 0x013c, 0x0161, 0x0144, 0x0146, 0x00f3, 0x014d, 0x00f5, 0x00f6, 0x00f7,
    0x0173, 0x0142, 0x015b, 0x016b, 0x00fc, 0x017c, 0x017e, 0x2019,
];

const XMLTRANSCODETABLE_ISO8859_13: [c_uchar; 48 + 7 * 64] = [
    0x00, 0x00, 0x01, 0x04, 0x06, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0x00, 0xa2, 0xa3, 0xa4, 0x00, 0xa6, 0xa7, 0x00, 0xa9, 0x00, 0xab, 0xac, 0xad, 0xae, 0x00,
    0xb0, 0xb1, 0xb2, 0xb3, 0x00, 0xb5, 0xb6, 0xb7, 0x00, 0xb9, 0x00, 0xbb, 0xbc, 0xbd, 0xbe, 0x00,
    0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0x00, 0x00, 0xb4, 0xa1, 0xa5, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xc4, 0xc5, 0xaf, 0x00, 0x00, 0xc9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xd3, 0x00, 0xd5, 0xd6, 0xd7, 0xa8, 0x00, 0x00, 0x00, 0xdc, 0x00, 0x00, 0xdf,
    0x00, 0x00, 0x00, 0x00, 0xe4, 0xe5, 0xbf, 0x00, 0x00, 0xe9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0xf3, 0x00, 0xf5, 0xf6, 0xf7, 0xb8, 0x00, 0x00, 0x00, 0xfc, 0x00, 0x00, 0x00,
    0x00, 0xd9, 0xf9, 0xd1, 0xf1, 0xd2, 0xf2, 0x00, 0x00, 0x00, 0x00, 0x00, 0xd4, 0xf4, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xba, 0x00, 0x00, 0xda, 0xfa, 0x00, 0x00, 0x00, 0x00,
    0xd0, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xdb, 0xfb, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xd8, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0xca, 0xea, 0xdd, 0xfd, 0xde, 0xfe, 0x00,
    0xc2, 0xe2, 0x00, 0x00, 0xc0, 0xe0, 0xc3, 0xe3, 0x00, 0x00, 0x00, 0x00, 0xc8, 0xe8, 0x00, 0x00,
    0x00, 0x00, 0xc7, 0xe7, 0x00, 0x00, 0xcb, 0xeb, 0xc6, 0xe6, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xcc, 0xec, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xce, 0xee, 0x00, 0x00, 0xc1, 0xe1,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xcd, 0xed, 0x00, 0x00, 0x00, 0xcf, 0xef, 0x00, 0x00, 0x00,
];

const XMLUNICODETABLE_ISO8859_14: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x1e02, 0x1e03, 0x00a3,
    0x010a, 0x010b, 0x1e0a, 0x00a7, 0x1e80, 0x00a9, 0x1e82, 0x1e0b, 0x1ef2, 0x00ad, 0x00ae, 0x0178,
    0x1e1e, 0x1e1f, 0x0120, 0x0121, 0x1e40, 0x1e41, 0x00b6, 0x1e56, 0x1e81, 0x1e57, 0x1e83, 0x1e60,
    0x1ef3, 0x1e84, 0x1e85, 0x1e61, 0x00c0, 0x00c1, 0x00c2, 0x00c3, 0x00c4, 0x00c5, 0x00c6, 0x00c7,
    0x00c8, 0x00c9, 0x00ca, 0x00cb, 0x00cc, 0x00cd, 0x00ce, 0x00cf, 0x0174, 0x00d1, 0x00d2, 0x00d3,
    0x00d4, 0x00d5, 0x00d6, 0x1e6a, 0x00d8, 0x00d9, 0x00da, 0x00db, 0x00dc, 0x00dd, 0x0176, 0x00df,
    0x00e0, 0x00e1, 0x00e2, 0x00e3, 0x00e4, 0x00e5, 0x00e6, 0x00e7, 0x00e8, 0x00e9, 0x00ea, 0x00eb,
    0x00ec, 0x00ed, 0x00ee, 0x00ef, 0x0175, 0x00f1, 0x00f2, 0x00f3, 0x00f4, 0x00f5, 0x00f6, 0x1e6b,
    0x00f8, 0x00f9, 0x00fa, 0x00fb, 0x00fc, 0x00fd, 0x0177, 0x00ff,
];

const XMLTRANSCODETABLE_ISO8859_14: [c_uchar; 48 + 10 * 64] = [
    0x00, 0x00, 0x01, 0x09, 0x04, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0x00, 0x00, 0xa3, 0x00, 0x00, 0x00, 0xa7, 0x00, 0xa9, 0x00, 0x00, 0x00, 0xad, 0xae, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xb6, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x03, 0x08, 0x05, 0x06, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xa1, 0xa2, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa6, 0xab, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xb0, 0xb1,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa4, 0xa5, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xb2, 0xb3, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xa8, 0xb8, 0xaa, 0xba, 0xbd, 0xbe, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xac, 0xbc, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0xd0, 0xf0, 0xde, 0xfe, 0xaf, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xb4, 0xb5, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xb7, 0xb9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xbb, 0xbf, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xd7, 0xf7, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0x00, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0x00, 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0x00, 0xdf,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0x00, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0x00, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0x00, 0xff,
];

const XMLUNICODETABLE_ISO8859_15: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x00a1, 0x00a2, 0x00a3,
    0x20ac, 0x00a5, 0x0160, 0x00a7, 0x0161, 0x00a9, 0x00aa, 0x00ab, 0x00ac, 0x00ad, 0x00ae, 0x00af,
    0x00b0, 0x00b1, 0x00b2, 0x00b3, 0x017d, 0x00b5, 0x00b6, 0x00b7, 0x017e, 0x00b9, 0x00ba, 0x00bb,
    0x0152, 0x0153, 0x0178, 0x00bf, 0x00c0, 0x00c1, 0x00c2, 0x00c3, 0x00c4, 0x00c5, 0x00c6, 0x00c7,
    0x00c8, 0x00c9, 0x00ca, 0x00cb, 0x00cc, 0x00cd, 0x00ce, 0x00cf, 0x00d0, 0x00d1, 0x00d2, 0x00d3,
    0x00d4, 0x00d5, 0x00d6, 0x00d7, 0x00d8, 0x00d9, 0x00da, 0x00db, 0x00dc, 0x00dd, 0x00de, 0x00df,
    0x00e0, 0x00e1, 0x00e2, 0x00e3, 0x00e4, 0x00e5, 0x00e6, 0x00e7, 0x00e8, 0x00e9, 0x00ea, 0x00eb,
    0x00ec, 0x00ed, 0x00ee, 0x00ef, 0x00f0, 0x00f1, 0x00f2, 0x00f3, 0x00f4, 0x00f5, 0x00f6, 0x00f7,
    0x00f8, 0x00f9, 0x00fa, 0x00fb, 0x00fc, 0x00fd, 0x00fe, 0x00ff,
];

const XMLTRANSCODETABLE_ISO8859_15: [c_uchar; 48 + 6 * 64] = [
    0x00, 0x00, 0x01, 0x05, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0xa1, 0xa2, 0xa3, 0x00, 0xa5, 0x00, 0xa7, 0x00, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
    0xb0, 0xb1, 0xb2, 0xb3, 0x00, 0xb5, 0xb6, 0xb7, 0x00, 0xb9, 0xba, 0xbb, 0x00, 0x00, 0x00, 0xbf,
    0x00, 0x00, 0x03, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa4, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xbc, 0xbd, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xa6, 0xa8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xbe, 0x00, 0x00, 0x00, 0x00, 0xb4, 0xb8, 0x00,
    0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff,
];

const XMLUNICODETABLE_ISO8859_16: [c_ushort; 128] = [
    0x0080, 0x0081, 0x0082, 0x0083, 0x0084, 0x0085, 0x0086, 0x0087, 0x0088, 0x0089, 0x008a, 0x008b,
    0x008c, 0x008d, 0x008e, 0x008f, 0x0090, 0x0091, 0x0092, 0x0093, 0x0094, 0x0095, 0x0096, 0x0097,
    0x0098, 0x0099, 0x009a, 0x009b, 0x009c, 0x009d, 0x009e, 0x009f, 0x00a0, 0x0104, 0x0105, 0x0141,
    0x20ac, 0x201e, 0x0160, 0x00a7, 0x0161, 0x00a9, 0x0218, 0x00ab, 0x0179, 0x00ad, 0x017a, 0x017b,
    0x00b0, 0x00b1, 0x010c, 0x0142, 0x017d, 0x201d, 0x00b6, 0x00b7, 0x017e, 0x010d, 0x0219, 0x00bb,
    0x0152, 0x0153, 0x0178, 0x017c, 0x00c0, 0x00c1, 0x00c2, 0x0102, 0x00c4, 0x0106, 0x00c6, 0x00c7,
    0x00c8, 0x00c9, 0x00ca, 0x00cb, 0x00cc, 0x00cd, 0x00ce, 0x00cf, 0x0110, 0x0143, 0x00d2, 0x00d3,
    0x00d4, 0x0150, 0x00d6, 0x015a, 0x0170, 0x00d9, 0x00da, 0x00db, 0x00dc, 0x0118, 0x021a, 0x00df,
    0x00e0, 0x00e1, 0x00e2, 0x0103, 0x00e4, 0x0107, 0x00e6, 0x00e7, 0x00e8, 0x00e9, 0x00ea, 0x00eb,
    0x00ec, 0x00ed, 0x00ee, 0x00ef, 0x0111, 0x0144, 0x00f2, 0x00f3, 0x00f4, 0x0151, 0x00f6, 0x015b,
    0x0171, 0x00f9, 0x00fa, 0x00fb, 0x00fc, 0x0119, 0x021b, 0x00ff,
];

const XMLTRANSCODETABLE_ISO8859_16: [c_uchar; 48 + 9 * 64] = [
    0x00, 0x00, 0x01, 0x08, 0x02, 0x03, 0x00, 0x00, 0x07, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa7, 0x00, 0xa9, 0x00, 0xab, 0x00, 0xad, 0x00, 0x00,
    0xb0, 0xb1, 0x00, 0x00, 0x00, 0x00, 0xb6, 0xb7, 0x00, 0x00, 0x00, 0xbb, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0xc3, 0xe3, 0xa1, 0xa2, 0xc5, 0xe5, 0x00, 0x00, 0x00, 0x00, 0xb2, 0xb9, 0x00, 0x00,
    0xd0, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xdd, 0xfd, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0xa3, 0xb3, 0xd1, 0xf1, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xd5, 0xf5, 0xbc, 0xbd, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xd7, 0xf7, 0x00, 0x00, 0x00, 0x00,
    0xa6, 0xa8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xd8, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xbe, 0xac, 0xae, 0xaf, 0xbf, 0xb4, 0xb8, 0x00,
    0x06, 0x00, 0x05, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xa4, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xb5, 0xa5, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xaa, 0xba, 0xde, 0xfe, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0xc0, 0xc1, 0xc2, 0x00, 0xc4, 0x00, 0xc6, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0x00, 0x00, 0xd2, 0xd3, 0xd4, 0x00, 0xd6, 0x00, 0x00, 0xd9, 0xda, 0xdb, 0xdc, 0x00, 0x00, 0xdf,
    0xe0, 0xe1, 0xe2, 0x00, 0xe4, 0x00, 0xe6, 0xe7, 0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0x00, 0x00, 0xf2, 0xf3, 0xf4, 0x00, 0xf6, 0x00, 0x00, 0xf9, 0xfa, 0xfb, 0xfc, 0x00, 0x00, 0xff,
];

#[cfg(test)]
mod tests {
    use crate::{
        libxml::{
            htmlparser::utf8_to_html, xmlerror::xml_reset_last_error, xmlmemory::xml_mem_blocks,
        },
        test_util::*,
    };

    use super::*;

    #[test]
    fn test_utf8_to_html() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_UNSIGNED_CHAR_PTR {
                for n_outlen in 0..GEN_NB_INT_PTR {
                    for n_in in 0..GEN_NB_CONST_UNSIGNED_CHAR_PTR {
                        for n_inlen in 0..GEN_NB_INT_PTR {
                            let mem_base = xml_mem_blocks();
                            let out = gen_unsigned_char_ptr(n_out, 0);
                            let outlen = gen_int_ptr(n_outlen, 1);
                            let input = gen_const_unsigned_char_ptr(n_in, 2);
                            let inlen = gen_int_ptr(n_inlen, 3);

                            let ret_val = utf8_to_html(out, outlen, input, inlen);
                            desret_int(ret_val);
                            des_unsigned_char_ptr(n_out, out, 0);
                            des_int_ptr(n_outlen, outlen, 1);
                            des_const_unsigned_char_ptr(n_in, input, 2);
                            des_int_ptr(n_inlen, inlen, 3);
                            xml_reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                eprintln!("Leak of {} blocks found in UTF8ToHtml {n_out} {n_outlen} {n_in} {n_inlen}", xml_mem_blocks() - mem_base);
                                leaks += 1;
                            }
                        }
                    }
                }
            }

            assert!(leaks == 0, "{leaks} Leaks are found in utf8_to_html()");
        }
    }

    #[test]
    fn test_utf8_toisolat1() {
        #[cfg(feature = "output")]
        unsafe {
            let mut leaks = 0;
            #[cfg(feature = "output")]
            {
                for n_out in 0..GEN_NB_UNSIGNED_CHAR_PTR {
                    for n_outlen in 0..GEN_NB_INT_PTR {
                        for n_in in 0..GEN_NB_CONST_UNSIGNED_CHAR_PTR {
                            for n_inlen in 0..GEN_NB_INT_PTR {
                                let mem_base = xml_mem_blocks();
                                let out = gen_unsigned_char_ptr(n_out, 0);
                                let outlen = gen_int_ptr(n_outlen, 1);
                                let input = gen_const_unsigned_char_ptr(n_in, 2);
                                let inlen = gen_int_ptr(n_inlen, 3);

                                let ret_val =
                                    utf8_to_iso_lat1(out, outlen, input as *const c_uchar, inlen);
                                desret_int(ret_val);
                                des_unsigned_char_ptr(n_out, out, 0);
                                des_int_ptr(n_outlen, outlen, 1);
                                des_const_unsigned_char_ptr(n_in, input as *const c_uchar, 2);
                                des_int_ptr(n_inlen, inlen, 3);
                                xml_reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in UTF8Toisolat1",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_out);
                                    eprint!(" {}", n_outlen);
                                    eprint!(" {}", n_in);
                                    eprintln!(" {}", n_inlen);
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in UTF8Toisolat1()");
        }
    }

    #[test]
    fn test_isolat1_to_utf8() {
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_UNSIGNED_CHAR_PTR {
                for n_outlen in 0..GEN_NB_INT_PTR {
                    for n_in in 0..GEN_NB_CONST_UNSIGNED_CHAR_PTR {
                        for n_inlen in 0..GEN_NB_INT_PTR {
                            let mem_base = xml_mem_blocks();
                            let out = gen_unsigned_char_ptr(n_out, 0);
                            let outlen = gen_int_ptr(n_outlen, 1);
                            let input = gen_const_unsigned_char_ptr(n_in, 2);
                            let inlen = gen_int_ptr(n_inlen, 3);

                            let ret_val =
                                iso_lat1_to_utf8(out, outlen, input as *const c_uchar, inlen);
                            desret_int(ret_val);
                            des_unsigned_char_ptr(n_out, out, 0);
                            des_int_ptr(n_outlen, outlen, 1);
                            des_const_unsigned_char_ptr(n_in, input as *const c_uchar, 2);
                            des_int_ptr(n_inlen, inlen, 3);
                            xml_reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in isolat1ToUTF8",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_out);
                                eprint!(" {}", n_outlen);
                                eprint!(" {}", n_in);
                                eprintln!(" {}", n_inlen);
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in isolat1ToUTF8()");
        }
    }

    #[test]
    fn test_xml_add_encoding_alias() {
        unsafe {
            for n_name in 0..GEN_NB_CONST_CHAR_PTR {
                for n_alias in 0..GEN_NB_CONST_CHAR_PTR {
                    let name = gen_const_char_ptr(n_name, 0);
                    let alias = gen_const_char_ptr(n_alias, 1);

                    let ret_val = xml_add_encoding_alias(name, alias);
                    desret_int(ret_val);
                    des_const_char_ptr(n_name, name, 0);
                    des_const_char_ptr(n_alias, alias, 1);
                    xml_reset_last_error();
                }
            }
        }
    }

    #[test]
    fn test_xml_char_enc_close_func() {
        let mut leaks = 0;

        unsafe {
            for n_handler in 0..GEN_NB_XML_CHAR_ENCODING_HANDLER_PTR {
                let mem_base = xml_mem_blocks();
                let handler = gen_xml_char_encoding_handler_ptr(n_handler, 0);

                let ret_val = xml_char_enc_close_func(handler);
                desret_int(ret_val);
                des_xml_char_encoding_handler_ptr(n_handler, handler, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlCharEncCloseFunc",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_handler);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlCharEncCloseFunc()"
            );
        }
    }

    // #[test]
    // fn test_xml_char_enc_first_line() {
    //     let mut leaks = 0;

    //     unsafe {
    //         for n_handler in 0..GEN_NB_XML_CHAR_ENCODING_HANDLER_PTR {
    //             for n_out in 0..GEN_NB_XML_BUFFER_PTR {
    //                 for n_in in 0..GEN_NB_XML_BUFFER_PTR {
    //                     let mem_base = xml_mem_blocks();
    //                     let handler = gen_xml_char_encoding_handler_ptr(n_handler, 0);
    //                     let out = gen_xml_buffer_ptr(n_out, 1);
    //                     let input = gen_xml_buffer_ptr(n_in, 2);

    //                     let ret_val = xml_char_enc_first_line(handler, out, input);
    //                     desret_int(ret_val);
    //                     des_xml_char_encoding_handler_ptr(n_handler, handler, 0);
    //                     des_xml_buffer_ptr(n_out, out, 1);
    //                     des_xml_buffer_ptr(n_in, input, 2);
    //                     xml_reset_last_error();
    //                     if mem_base != xml_mem_blocks() {
    //                         leaks += 1;
    //                         eprint!(
    //                             "Leak of {} blocks found in xmlCharEncFirstLine",
    //                             xml_mem_blocks() - mem_base
    //                         );
    //                         eprint!(" {}", n_handler);
    //                         eprint!(" {}", n_out);
    //                         eprintln!(" {}", n_in);
    //                     }
    //                 }
    //             }
    //         }

    //         assert!(
    //             leaks == 0,
    //             "{leaks} Leaks are found in xmlCharEncFirstLine()"
    //         );
    //     }
    // }

    // #[test]
    // fn test_xml_char_enc_in_func() {
    //     unsafe {
    //         let mut leaks = 0;

    //         for n_handler in 0..GEN_NB_XML_CHAR_ENCODING_HANDLER_PTR {
    //             for n_out in 0..GEN_NB_XML_BUFFER_PTR {
    //                 for n_in in 0..GEN_NB_XML_BUFFER_PTR {
    //                     let mem_base = xml_mem_blocks();
    //                     let handler = gen_xml_char_encoding_handler_ptr(n_handler, 0);
    //                     let out = gen_xml_buffer_ptr(n_out, 1);
    //                     let input = gen_xml_buffer_ptr(n_in, 2);

    //                     let ret_val = xml_char_enc_in_func(handler, out, input);
    //                     desret_int(ret_val);
    //                     des_xml_char_encoding_handler_ptr(n_handler, handler, 0);
    //                     des_xml_buffer_ptr(n_out, out, 1);
    //                     des_xml_buffer_ptr(n_in, input, 2);
    //                     xml_reset_last_error();
    //                     if mem_base != xml_mem_blocks() {
    //                         leaks += 1;
    //                         eprint!(
    //                             "Leak of {} blocks found in xmlCharEncInFunc",
    //                             xml_mem_blocks() - mem_base
    //                         );
    //                         eprint!(" {}", n_handler);
    //                         eprint!(" {}", n_out);
    //                         eprintln!(" {}", n_in);
    //                     }
    //                 }
    //             }
    //         }
    //         assert!(leaks == 0, "{leaks} Leaks are found in xmlCharEncInFunc()");
    //     }
    // }

    // #[test]
    // fn test_xml_char_enc_out_func() {
    //     unsafe {
    //         let mut leaks = 0;
    //         for n_handler in 0..GEN_NB_XML_CHAR_ENCODING_HANDLER_PTR {
    //             for n_out in 0..GEN_NB_XML_BUFFER_PTR {
    //                 for n_in in 0..GEN_NB_XML_BUFFER_PTR {
    //                     let mem_base = xml_mem_blocks();
    //                     let handler = gen_xml_char_encoding_handler_ptr(n_handler, 0);
    //                     let out = gen_xml_buffer_ptr(n_out, 1);
    //                     let input = gen_xml_buffer_ptr(n_in, 2);

    //                     let ret_val = xml_char_enc_out_func(handler, out, input);
    //                     desret_int(ret_val);
    //                     des_xml_char_encoding_handler_ptr(n_handler, handler, 0);
    //                     des_xml_buffer_ptr(n_out, out, 1);
    //                     des_xml_buffer_ptr(n_in, input, 2);
    //                     xml_reset_last_error();
    //                     if mem_base != xml_mem_blocks() {
    //                         leaks += 1;
    //                         eprint!(
    //                             "Leak of {} blocks found in xmlCharEncOutFunc",
    //                             xml_mem_blocks() - mem_base
    //                         );
    //                         eprint!(" {}", n_handler);
    //                         eprint!(" {}", n_out);
    //                         eprintln!(" {}", n_in);
    //                     }
    //                 }
    //             }
    //         }

    //         assert!(leaks == 0, "{leaks} Leaks are found in xmlCharEncOutFunc()");
    //     }
    // }

    #[test]
    fn test_xml_cleanup_char_encoding_handlers() {
        unsafe {
            xml_cleanup_char_encoding_handlers();
            xml_reset_last_error();
        }
    }

    #[test]
    fn test_xml_cleanup_encoding_aliases() {
        unsafe {
            let mut leaks = 0;
            let mem_base = xml_mem_blocks();

            xml_cleanup_encoding_aliases();
            xml_reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlCleanupEncodingAliases",
                    xml_mem_blocks() - mem_base
                );
                assert!(
                    leaks == 0,
                    "{leaks} Leaks are found in xmlCleanupEncodingAliases()"
                );
            }
        }
    }

    #[test]
    fn test_xml_del_encoding_alias() {
        unsafe {
            let mut leaks = 0;

            for n_alias in 0..GEN_NB_CONST_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let alias = gen_const_char_ptr(n_alias, 0);

                let ret_val = xml_del_encoding_alias(alias);
                desret_int(ret_val);
                des_const_char_ptr(n_alias, alias, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlDelEncodingAlias",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_alias);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlDelEncodingAlias()"
            );
        }
    }

    #[test]
    fn test_xml_detect_char_encoding() {
        unsafe {
            let mut leaks = 0;

            for n_in in 0..GEN_NB_CONST_UNSIGNED_CHAR_PTR {
                for n_len in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let input = gen_const_unsigned_char_ptr(n_in, 0);
                    let len = gen_int(n_len, 1);

                    let ret_val = xml_detect_char_encoding(input as *const c_uchar, len);
                    desret_xml_char_encoding(ret_val);
                    des_const_unsigned_char_ptr(n_in, input as *const c_uchar, 0);
                    des_int(n_len, len, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDetectCharEncoding",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_in);
                        eprintln!(" {}", n_len);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlDetectCharEncoding()"
            );
        }
    }

    #[test]
    fn test_xml_find_char_encoding_handler() {

        /* missing type support */
    }

    #[test]
    fn test_xml_get_char_encoding_handler() {

        /* missing type support */
    }

    #[test]
    fn test_xml_get_char_encoding_name() {
        unsafe {
            let mut leaks = 0;

            for n_enc in 0..GEN_NB_XML_CHAR_ENCODING {
                let mem_base = xml_mem_blocks();
                let enc = gen_xml_char_encoding(n_enc, 0);

                let ret_val = xml_get_char_encoding_name(enc);
                desret_const_char_ptr(ret_val);
                des_xml_char_encoding(n_enc, enc, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlGetCharEncodingName",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_enc);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlGetCharEncodingName()"
            );
        }
    }

    #[test]
    fn test_xml_get_encoding_alias() {
        unsafe {
            let mut leaks = 0;

            for n_alias in 0..GEN_NB_CONST_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let alias = gen_const_char_ptr(n_alias, 0);

                let ret_val = xml_get_encoding_alias(alias);
                desret_const_char_ptr(ret_val);
                des_const_char_ptr(n_alias, alias, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlGetEncodingAlias",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_alias);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlGetEncodingAlias()"
            );
        }
    }

    #[test]
    fn test_xml_init_char_encoding_handlers() {
        unsafe {
            xml_init_char_encoding_handlers();
            xml_reset_last_error();
        }
    }

    #[test]
    fn test_xml_new_char_encoding_handler() {

        /* missing type support */
    }

    #[test]
    fn test_xml_parse_char_encoding() {
        unsafe {
            let mut leaks = 0;

            for n_name in 0..GEN_NB_CONST_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let name = gen_const_char_ptr(n_name, 0);

                let ret_val = xml_parse_char_encoding(name);
                desret_xml_char_encoding(ret_val);
                des_const_char_ptr(n_name, name, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlParseCharEncoding",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_name);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlParseCharEncoding()"
            );
        }
    }

    #[test]
    fn test_xml_register_char_encoding_handler() {
        unsafe {
            let mut leaks = 0;

            for n_handler in 0..GEN_NB_XML_CHAR_ENCODING_HANDLER_PTR {
                let mem_base = xml_mem_blocks();
                let handler = gen_xml_char_encoding_handler_ptr(n_handler, 0);

                xml_register_char_encoding_handler(handler);
                des_xml_char_encoding_handler_ptr(n_handler, handler, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlRegisterCharEncodingHandler",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_handler);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlRegisterCharEncodingHandler()"
            );
        }
    }
}

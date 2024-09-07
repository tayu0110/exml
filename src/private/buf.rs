//! Provide internal methods and data structures for dynamic memory buffer.  
//! This module is based on `private/buf.h`, `buf.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int},
    mem::size_of,
    ops::Add,
    ptr::{null, null_mut},
};

use libc::{fwrite, memcpy, memmove, size_t, FILE};

use crate::libxml::{
    globals::{
        xmlBufferAllocScheme, xmlDefaultBufferSize, xml_free, xml_malloc, xml_malloc_atomic,
        xml_realloc,
    },
    parser::XmlParserInputPtr,
    parser_internals::XML_MAX_TEXT_LENGTH,
    tree::{
        xml_buffer_free, XmlBuf, XmlBufPtr, XmlBufferAllocationScheme, XmlBufferPtr,
        BASE_BUFFER_SIZE,
    },
    xmlerror::{XmlErrorDomain, XmlParserErrors},
    xmlstring::{xml_strchr, xml_strlen, XmlChar},
};

use super::error::__xml_simple_error;

const SIZE_MAX: size_t = size_t::MAX;

/*
 * Macro for compatibility with xmlBuffer to be used after an xmlBuf
 * is updated. This makes sure the compat fields are updated too.
 */
#[macro_export]
macro_rules! UPDATE_COMPAT {
    ($buf:expr) => {
        if (*$buf).size < i32::MAX as usize {
            (*$buf).compat_size = (*$buf).size as _;
        } else {
            (*$buf).compat_size = i32::MAX as _;
        }
        if (*$buf).using < i32::MAX as usize {
            (*$buf).compat_use = (*$buf).using as _;
        } else {
            (*$buf).compat_use = i32::MAX as _;
        }
    };
}

/*
 * Macro for compatibility with xmlBuffer to be used in all the xmlBuf
 * entry points, it checks that the compat fields have not been modified
 * by direct call to xmlBuffer function from code compiled before 2.9.0 .
 */
#[macro_export]
macro_rules! CHECK_COMPAT {
    ($buf:expr) => {
        if (*$buf).size != (*$buf).compat_size as size_t {
            if ((*$buf).compat_size < i32::MAX as u32) {
                (*$buf).size = (*$buf).compat_size as _;
            }
        }
        if (*$buf).using != (*$buf).compat_use as size_t {
            if ((*$buf).compat_use < i32::MAX as u32) {
                (*$buf).using = (*$buf).compat_use as _;
            }
        }
    };
}

/**
 * xmlBufMemoryError:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 * To be improved...
 */
unsafe extern "C" fn xml_buf_memory_error(buf: XmlBufPtr, extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromBuffer as _,
        XmlParserErrors::XmlErrNoMemory as _,
        null_mut(),
        null(),
        extra as _,
    );
    if !buf.is_null() && (*buf).error == 0 {
        (*buf).error = XmlParserErrors::XmlErrNoMemory as _;
    }
}

/**
 * xmlBufCreate:
 *
 * routine to create an XML buffer.
 * returns the new structure.
 */
pub(crate) unsafe extern "C" fn xmlBufCreate() -> XmlBufPtr {
    let ret: XmlBufPtr = xml_malloc(size_of::<XmlBuf>()) as XmlBufPtr;
    if ret.is_null() {
        xml_buf_memory_error(null_mut(), c"creating buffer".as_ptr() as _);
        return null_mut();
    }
    (*ret).using = 0;
    (*ret).error = 0;
    (*ret).buffer = null_mut();
    (*ret).size = *xmlDefaultBufferSize() as _;
    UPDATE_COMPAT!(ret);
    (*ret).alloc = *xmlBufferAllocScheme();
    (*ret).content = xml_malloc_atomic((*ret).size) as *mut XmlChar;
    if (*ret).content.is_null() {
        xml_buf_memory_error(ret, c"creating buffer".as_ptr() as _);
        xml_free(ret as _);
        return null_mut();
    }
    *(*ret).content = 0;
    (*ret).content_io = null_mut();
    ret
}

/**
 * xmlBufCreateSize:
 * @size: initial size of buffer
 *
 * routine to create an XML buffer.
 * returns the new structure.
 */
pub(crate) unsafe extern "C" fn xmlBufCreateSize(size: size_t) -> XmlBufPtr {
    if size == SIZE_MAX {
        return null_mut();
    }
    let ret: XmlBufPtr = xml_malloc(size_of::<XmlBuf>()) as XmlBufPtr;
    if ret.is_null() {
        xml_buf_memory_error(null_mut(), c"creating buffer".as_ptr() as _);
        return null_mut();
    }
    (*ret).using = 0;
    (*ret).error = 0;
    (*ret).buffer = null_mut();
    (*ret).alloc = *xmlBufferAllocScheme();
    (*ret).size = if size != 0 { size + 1 } else { 0 }; /* +1 for ending null */
    UPDATE_COMPAT!(ret);
    if (*ret).size != 0 {
        (*ret).content = xml_malloc_atomic((*ret).size) as *mut XmlChar;
        if (*ret).content.is_null() {
            xml_buf_memory_error(ret, c"creating buffer".as_ptr() as _);
            xml_free(ret as _);
            return null_mut();
        }
        *(*ret).content = 0;
    } else {
        (*ret).content = null_mut();
    }
    (*ret).content_io = null_mut();
    ret
}

/**
 * xmlBufSetAllocationScheme:
 * @buf:  the buffer to tune
 * @scheme:  allocation scheme to use
 *
 * Sets the allocation scheme for this buffer
 *
 * returns 0 in case of success and -1 in case of failure
 */
pub(crate) unsafe extern "C" fn xmlBufSetAllocationScheme(
    buf: XmlBufPtr,
    scheme: XmlBufferAllocationScheme,
) -> c_int {
    if buf.is_null() || (*buf).error != 0 {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		"xmlBufSetAllocationScheme: buf.is_null() or in error\n");
        // #endif
        return -1;
    }
    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo) {
        return -1;
    }
    if matches!(scheme, XmlBufferAllocationScheme::XmlBufferAllocDoubleit)
        || matches!(scheme, XmlBufferAllocationScheme::XmlBufferAllocExact)
        || matches!(scheme, XmlBufferAllocationScheme::XmlBufferAllocHybrid)
        || matches!(scheme, XmlBufferAllocationScheme::XmlBufferAllocBounded)
    {
        (*buf).alloc = scheme;
        if !(*buf).buffer.is_null() {
            (*(*buf).buffer).alloc = scheme;
        }
        return 0;
    }
    /*
     * Switching a buffer ALLOC_IO has the side effect of initializing
     * the contentIO field with the current content
     */
    if matches!(scheme, XmlBufferAllocationScheme::XmlBufferAllocIo) {
        (*buf).alloc = XmlBufferAllocationScheme::XmlBufferAllocIo;
        (*buf).content_io = (*buf).content;
    }
    -1
}

/**
 * xmlBufGetAllocationScheme:
 * @buf:  the buffer
 *
 * Get the buffer allocation scheme
 *
 * Returns the scheme or -1 in case of error
 */
pub(crate) unsafe extern "C" fn xmlBufGetAllocationScheme(buf: XmlBufPtr) -> c_int {
    if buf.is_null() {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		"xmlBufGetAllocationScheme: buf.is_null()\n");
        // #endif
        return -1;
    }
    (*buf).alloc as _
}

/**
 * xmlBufFree:
 * @buf:  the buffer to free
 *
 * Frees an XML buffer. It frees both the content and the structure which
 * encapsulate it.
 */
pub(crate) unsafe extern "C" fn xmlBufFree(buf: XmlBufPtr) {
    if buf.is_null() {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		"xmlBufFree: buf.is_null()\n");
        // #endif
        return;
    }

    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
        && !(*buf).content_io.is_null()
    {
        xml_free((*buf).content_io as _);
    } else if !(*buf).content.is_null() {
        xml_free((*buf).content as _);
    }
    xml_free(buf as _);
}

/**
 * xmlBufEmpty:
 * @buf:  the buffer
 *
 * empty a buffer.
 */
pub(crate) unsafe extern "C" fn xmlBufEmpty(buf: XmlBufPtr) {
    if (buf.is_null()) || ((*buf).error != 0) {
        return;
    }
    if (*buf).content.is_null() {
        return;
    }
    CHECK_COMPAT!(buf);
    (*buf).using = 0;
    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
        && !(*buf).content_io.is_null()
    {
        let start_buf: size_t = (*buf).content.offset_from((*buf).content_io) as _;

        (*buf).size += start_buf;
        (*buf).content = (*buf).content_io;
        *(*buf).content = 0;
    } else {
        *(*buf).content = 0;
    }
    UPDATE_COMPAT!(buf);
}

/* size_t xmlBufShrink(buf: xmlBufPtr, len: size_t); */

/**
 * xmlBufGrowInternal:
 * @buf:  the buffer
 * @len:  the minimum free size to allocate
 *
 * Grow the available space of an XML buffer, @len is the target value
 * Error checking should be done on (*buf).error since using the return
 * value doesn't work that well
 *
 * Returns 0 in case of error or the length made available otherwise
 */
unsafe extern "C" fn xml_buf_grow_internal(buf: XmlBufPtr, len: size_t) -> size_t {
    let mut size: size_t;
    let newbuf: *mut XmlChar;

    if buf.is_null() || (*buf).error != 0 {
        return 0;
    }
    CHECK_COMPAT!(buf);

    if len < (*buf).size - (*buf).using {
        return (*buf).size - (*buf).using - 1;
    }
    if len >= SIZE_MAX - (*buf).using {
        xml_buf_memory_error(buf, c"growing buffer past SIZE_MAX".as_ptr() as _);
        return 0;
    }

    if (*buf).size > len as size_t {
        size = if (*buf).size > SIZE_MAX / 2 {
            SIZE_MAX
        } else {
            (*buf).size * 2
        };
    } else {
        size = (*buf).using + len;
        size = if size > SIZE_MAX - 100 {
            SIZE_MAX
        } else {
            size + 100
        };
    }

    if matches!(
        (*buf).alloc,
        XmlBufferAllocationScheme::XmlBufferAllocBounded
    ) {
        /*
         * Used to provide parsing limits
         */
        if ((*buf).using + len + 1 >= XML_MAX_TEXT_LENGTH) || ((*buf).size >= XML_MAX_TEXT_LENGTH) {
            xml_buf_memory_error(buf, c"buffer error: text too long\n".as_ptr() as _);
            return 0;
        }
        if size >= XML_MAX_TEXT_LENGTH {
            size = XML_MAX_TEXT_LENGTH;
        }
    }
    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
        && !(*buf).content_io.is_null()
    {
        let start_buf: size_t = (*buf).content.offset_from((*buf).content_io) as _;

        newbuf = xml_realloc((*buf).content_io as _, start_buf + size) as *mut XmlChar;
        if newbuf.is_null() {
            xml_buf_memory_error(buf, c"growing buffer".as_ptr() as _);
            return 0;
        }
        (*buf).content_io = newbuf;
        (*buf).content = newbuf.add(start_buf);
    } else {
        newbuf = xml_realloc((*buf).content as _, size) as *mut XmlChar;
        if newbuf.is_null() {
            xml_buf_memory_error(buf, c"growing buffer".as_ptr() as _);
            return 0;
        }
        (*buf).content = newbuf;
    }
    (*buf).size = size;
    UPDATE_COMPAT!(buf);
    (*buf).size - (*buf).using - 1
}

/**
 * xmlBufGrow:
 * @buf:  the buffer
 * @len:  the minimum free size to allocate
 *
 * Grow the available space of an XML buffer, @len is the target value
 * This is been kept compatible with xmlBufferGrow() as much as possible
 *
 * Returns -1 in case of error or the length made available otherwise
 */
pub(crate) unsafe extern "C" fn xmlBufGrow(buf: XmlBufPtr, len: c_int) -> c_int {
    if buf.is_null() || len < 0 {
        return -1;
    }
    if len == 0 {
        return 0;
    }
    let ret: size_t = xml_buf_grow_internal(buf, len as usize);
    if (*buf).error != 0 {
        return -1;
    }
    if ret > i32::MAX as usize {
        i32::MAX
    } else {
        ret as c_int
    }
}

/**
 * xmlBufResize:
 * @buf:  the buffer to resize
 * @size:  the desired size
 *
 * Resize a buffer to accommodate minimum size of @size.
 *
 * Returns  0 in case of problems, 1 otherwise
 */
pub(crate) unsafe extern "C" fn xmlBufResize(buf: XmlBufPtr, size: size_t) -> c_int {
    let mut new_size: size_t;
    let rebuf: *mut XmlChar;
    let start_buf: size_t;

    if buf.is_null() || (*buf).error != 0 {
        return 0;
    }
    CHECK_COMPAT!(buf);

    if matches!(
        (*buf).alloc,
        XmlBufferAllocationScheme::XmlBufferAllocBounded
    ) {
        /*
         * Used to provide parsing limits
         */
        if size >= XML_MAX_TEXT_LENGTH {
            xml_buf_memory_error(buf, c"buffer error: text too long\n".as_ptr() as _);
            return 0;
        }
    }

    /* Don't resize if we don't have to */
    if size < (*buf).size {
        return 1;
    }

    /* figure out new size */
    match (*buf).alloc {
        XmlBufferAllocationScheme::XmlBufferAllocIo
        | XmlBufferAllocationScheme::XmlBufferAllocDoubleit => {
            /*take care of empty case*/
            if (*buf).size == 0 {
                new_size = if size > SIZE_MAX - 10 {
                    SIZE_MAX
                } else {
                    size + 10
                };
            } else {
                new_size = (*buf).size;
            }
            while size > new_size {
                if new_size > SIZE_MAX / 2 {
                    xml_buf_memory_error(buf, c"growing buffer".as_ptr() as _);
                    return 0;
                }
                new_size *= 2;
            }
        }
        XmlBufferAllocationScheme::XmlBufferAllocExact => {
            new_size = if size > SIZE_MAX - 10 {
                SIZE_MAX
            } else {
                size + 10
            };
        }
        XmlBufferAllocationScheme::XmlBufferAllocHybrid => {
            if (*buf).using < BASE_BUFFER_SIZE {
                new_size = size;
            } else {
                new_size = (*buf).size;
                while size > new_size {
                    if new_size > SIZE_MAX / 2 {
                        xml_buf_memory_error(buf, c"growing buffer".as_ptr() as _);
                        return 0;
                    }
                    new_size *= 2;
                }
            }
        }
        _ => {
            new_size = if size > SIZE_MAX - 10 {
                SIZE_MAX
            } else {
                size + 10
            };
        }
    }

    if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
        && !(*buf).content_io.is_null()
    {
        start_buf = (*buf).content.offset_from((*buf).content_io) as _;

        if start_buf > new_size {
            /* move data back to start */
            memmove((*buf).content_io as _, (*buf).content as _, (*buf).using);
            (*buf).content = (*buf).content_io;
            (*(*buf).content.add((*buf).using)) = 0;
            (*buf).size += start_buf;
        } else {
            rebuf = xml_realloc((*buf).content_io as _, start_buf + new_size) as *mut XmlChar;
            if rebuf.is_null() {
                xml_buf_memory_error(buf, c"growing buffer".as_ptr() as _);
                return 0;
            }
            (*buf).content_io = rebuf;
            (*buf).content = rebuf.add(start_buf);
        }
    } else {
        if (*buf).content.is_null() {
            rebuf = xml_malloc_atomic(new_size) as *mut XmlChar;
            (*buf).using = 0;
            if !rebuf.is_null() {
                *rebuf.add((*buf).using) = 0;
            }
        } else if (*buf).size - (*buf).using < 100 {
            rebuf = xml_realloc((*buf).content as _, new_size) as *mut XmlChar;
        } else {
            /*
             * if we are reallocating a buffer far from being full, it's
             * better to make a new allocation and copy only the used range
             * and free the old one.
             */
            rebuf = xml_malloc_atomic(new_size) as *mut XmlChar;
            if !rebuf.is_null() {
                memcpy(rebuf as _, (*buf).content as _, (*buf).using);
                xml_free((*buf).content as _);
                *rebuf.add((*buf).using) = 0;
            }
        }
        if rebuf.is_null() {
            xml_buf_memory_error(buf, c"growing buffer".as_ptr() as _);
            return 0;
        }
        (*buf).content = rebuf;
    }
    (*buf).size = new_size;
    UPDATE_COMPAT!(buf);

    1
}

/**
 * xmlBufAdd:
 * @buf:  the buffer to dump
 * @str:  the #xmlChar string
 * @len:  the number of #xmlChar to add
 *
 * Add a string range to an XML buffer. if len == -1, the length of
 * str is recomputed.
 *
 * Returns 0 successful, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
pub(crate) unsafe extern "C" fn xmlBufAdd(
    buf: XmlBufPtr,
    str: *const XmlChar,
    mut len: c_int,
) -> c_int {
    let need_size: size_t;

    if str.is_null() || buf.is_null() || (*buf).error != 0 {
        return -1;
    }
    CHECK_COMPAT!(buf);

    if len < -1 {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		"xmlBufAdd: len < 0\n");
        // #endif
        return -1;
    }
    if len == 0 {
        return 0;
    }

    if len < 0 {
        len = xml_strlen(str);
    }

    if len < 0 {
        return -1;
    }
    if len == 0 {
        return 0;
    }

    /* Note that both (*buf).size and (*buf).using can be zero here. */
    if (len as size_t) >= (*buf).size - (*buf).using {
        if (len as size_t) >= SIZE_MAX - (*buf).using {
            xml_buf_memory_error(buf, c"growing buffer past SIZE_MAX".as_ptr() as _);
            return -1;
        }
        need_size = (*buf).using.add(len as usize + 1);
        if matches!(
            (*buf).alloc,
            XmlBufferAllocationScheme::XmlBufferAllocBounded
        ) {
            /*
             * Used to provide parsing limits
             */
            if need_size >= XML_MAX_TEXT_LENGTH {
                xml_buf_memory_error(buf, c"buffer error: text too long\n".as_ptr() as _);
                return -1;
            }
        }
        if xmlBufResize(buf, need_size) == 0 {
            xml_buf_memory_error(buf, c"growing buffer".as_ptr() as _);
            return XmlParserErrors::XmlErrNoMemory as _;
        }
    }

    memmove((*buf).content.add((*buf).using) as _, str as _, len as _);
    (*buf).using += len as usize;
    *(*buf).content.add((*buf).using) = 0;
    UPDATE_COMPAT!(buf);
    0
}

/**
 * xmlBufCat:
 * @buf:  the buffer to add to
 * @str:  the #xmlChar string
 *
 * Append a zero terminated string to an XML buffer.
 *
 * Returns 0 successful, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
pub(crate) unsafe extern "C" fn xmlBufCat(buf: XmlBufPtr, str: *const XmlChar) -> c_int {
    if buf.is_null() || (*buf).error != 0 {
        return -1;
    }
    CHECK_COMPAT!(buf);
    if str.is_null() {
        return -1;
    }
    xmlBufAdd(buf, str, -1)
}

/**
 * xmlBufCCat:
 * @buf:  the buffer to dump
 * @str:  the C char string
 *
 * Append a zero terminated C string to an XML buffer.
 *
 * Returns 0 successful, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
pub(crate) unsafe extern "C" fn xmlBufCCat(buf: XmlBufPtr, str: *const c_char) -> c_int {
    xmlBufCat(buf, str as _)
}

/**
 * xmlBufWriteQuotedString:
 * @buf:  the XML buffer output
 * @string:  the string to add
 *
 * routine which manage and grows an output buffer. This one writes
 * a quoted or double quoted #xmlChar string, checking first if it holds
 * quote or double-quotes internally
 *
 * Returns 0 if successful, a positive error code number otherwise
 *         and -1 in case of internal or API error.
 */
pub(crate) unsafe extern "C" fn xmlBufWriteQuotedString(
    buf: XmlBufPtr,
    string: *const XmlChar,
) -> c_int {
    let mut cur: *const XmlChar;
    let mut base: *const XmlChar;
    if buf.is_null() || (*buf).error != 0 {
        return -1;
    }
    CHECK_COMPAT!(buf);
    if !xml_strchr(string, b'\"').is_null() {
        if !xml_strchr(string, b'\'').is_null() {
            // #ifdef DEBUG_BUFFER
            // 	    xmlGenericError(xmlGenericErrorContext,
            //  "xmlBufWriteQuotedString: string contains quote and double-quotes !\n");
            // #endif
            xmlBufCCat(buf, c"\"".as_ptr() as _);
            base = string;
            cur = string;
            while *cur != 0 {
                if *cur == b'"' {
                    if base != cur {
                        xmlBufAdd(buf, base, cur.offset_from(base) as _);
                    }
                    xmlBufAdd(buf, c"&quot;".as_ptr() as _, 6);
                    cur = cur.add(1);
                    base = cur;
                } else {
                    cur = cur.add(1);
                }
            }
            if base != cur {
                xmlBufAdd(buf, base, cur.offset_from(base) as _);
            }
            xmlBufCCat(buf, c"\"".as_ptr() as _);
        } else {
            xmlBufCCat(buf, c"\'".as_ptr() as _);
            xmlBufCat(buf, string);
            xmlBufCCat(buf, c"\'".as_ptr() as _);
        }
    } else {
        xmlBufCCat(buf, c"\"".as_ptr() as _);
        xmlBufCat(buf, string);
        xmlBufCCat(buf, c"\"".as_ptr() as _);
    }
    0
}

/**
 * xmlBufAvail:
 * @buf:  the buffer
 *
 * Function to find how much free space is allocated but not
 * used in the buffer. It reserves one byte for the NUL
 * terminator character that is usually needed, so there is
 * no need to subtract 1 from the result anymore.
 *
 * Returns the amount, or 0 if none or if an error occurred.
 */
pub(crate) unsafe extern "C" fn xmlBufAvail(buf: XmlBufPtr) -> size_t {
    if buf.is_null() || (*buf).error != 0 {
        return 0;
    }
    CHECK_COMPAT!(buf);

    if (*buf).size > (*buf).using {
        (*buf).size - (*buf).using - 1
    } else {
        0
    }
}

/**
 * xmlBufLength:
 * @buf:  the buffer
 *
 * Function to get the length of a buffer
 *
 * Returns the length of data in the internal content
 */
pub(crate) unsafe extern "C" fn xmlBufLength(buf: XmlBufPtr) -> size_t {
    if buf.is_null() || (*buf).error != 0 {
        return 0;
    }
    CHECK_COMPAT!(buf);

    (*buf).using
}

/* size_t xmlBufUse(const buf: xmlBufPtr); */

/**
 * xmlBufIsEmpty:
 * @buf:  the buffer
 *
 * Tell if a buffer is empty
 *
 * Returns 0 if no, 1 if yes and -1 in case of error
 */
pub(crate) unsafe extern "C" fn xmlBufIsEmpty(buf: XmlBufPtr) -> c_int {
    if buf.is_null() || (*buf).error != 0 {
        return -1;
    }
    CHECK_COMPAT!(buf);

    ((*buf).using == 0) as _
}

/**
 * xmlBufAddLen:
 * @buf:  the buffer
 * @len:  the size which were added at the end
 *
 * Sometime data may be added at the end of the buffer without
 * using the xmlBuf APIs that is used to expand the used space
 * and set the zero terminating at the end of the buffer
 *
 * Returns -1 in case of error and 0 otherwise
 */
pub(crate) unsafe extern "C" fn xmlBufAddLen(buf: XmlBufPtr, len: size_t) -> c_int {
    if buf.is_null() || (*buf).error != 0 {
        return -1;
    }
    CHECK_COMPAT!(buf);
    if len >= ((*buf).size - (*buf).using) {
        return -1;
    }
    (*buf).using += len;
    *(*buf).content.add((*buf).using) = 0;
    UPDATE_COMPAT!(buf);
    0
}

/* const xmlChar * xmlBufContent(const xmlBuf *buf); */
/* const xmlChar * xmlBufEnd(buf: xmlBufPtr); */

/**
 * xmlBufDetach:
 * @buf:  the buffer
 *
 * Remove the string contained in a buffer and give it back to the
 * caller. The buffer is reset to an empty content.
 * This doesn't work with immutable buffers as they can't be reset.
 *
 * Returns the previous string contained by the buffer.
 */
pub(crate) unsafe extern "C" fn xmlBufDetach(buf: XmlBufPtr) -> *mut XmlChar {
    if buf.is_null() {
        return null_mut();
    }
    if !(*buf).buffer.is_null() {
        return null_mut();
    }
    if (*buf).error != 0 {
        return null_mut();
    }

    let ret: *mut XmlChar = (*buf).content;
    (*buf).content = null_mut();
    (*buf).size = 0;
    (*buf).using = 0;
    UPDATE_COMPAT!(buf);

    ret
}

/**
 * xmlBufDump:
 * @file:  the file output
 * @buf:  the buffer to dump
 *
 * Dumps an XML buffer to  a FILE *.
 * Returns the number of #xmlChar written
 */
pub(crate) unsafe extern "C" fn xmlBufDump(mut file: *mut FILE, buf: XmlBufPtr) -> size_t {
    if buf.is_null() || (*buf).error != 0 {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		"xmlBufDump: buf.is_null() or in error\n");
        // #endif
        return 0;
    }
    if (*buf).content.is_null() {
        // #ifdef DEBUG_BUFFER
        //         xmlGenericError(xmlGenericErrorContext,
        // 		"xmlBufDump: (*buf).content.is_null()\n");
        // #endif
        return 0;
    }
    CHECK_COMPAT!(buf);
    if file.is_null() {
        extern "C" {
            // Does it work ???
            static stdout: *mut FILE;
        }
        file = stdout;
    }
    let ret: size_t = fwrite((*buf).content as _, 1, (*buf).using, file);
    ret
}

/**
 * xmlBufFromBuffer:
 * @buffer: incoming old buffer to convert to a new one
 *
 * Helper routine to switch from the old buffer structures in use
 * in various APIs. It creates a wrapper xmlBufPtr which will be
 * used for internal processing until the xmlBufBackToBuffer() is
 * issued.
 *
 * Returns a new xmlBufPtr unless the call failed and NULL is returned
 */
pub(crate) unsafe extern "C" fn xmlBufFromBuffer(buffer: XmlBufferPtr) -> XmlBufPtr {
    if buffer.is_null() {
        return null_mut();
    }

    let ret: XmlBufPtr = xml_malloc(size_of::<XmlBuf>()) as XmlBufPtr;
    if ret.is_null() {
        xml_buf_memory_error(null_mut(), c"creating buffer".as_ptr() as _);
        return null_mut();
    }
    (*ret).using = (*buffer).using as _;
    (*ret).size = (*buffer).size as _;
    UPDATE_COMPAT!(ret);
    (*ret).error = 0;
    (*ret).buffer = buffer;
    (*ret).alloc = (*buffer).alloc;
    (*ret).content = (*buffer).content;
    (*ret).content_io = (*buffer).content_io;

    ret
}

/**
 * xmlBufOverflowError:
 * @extra:  extra information
 *
 * Handle a buffer overflow error
 * To be improved...
 */
unsafe extern "C" fn xml_buf_overflow_error(buf: XmlBufPtr, extra: *const c_char) {
    __xml_simple_error(
        XmlErrorDomain::XmlFromBuffer as _,
        XmlParserErrors::XmlBufOverflow as _,
        null_mut(),
        null_mut(),
        extra as _,
    );
    if !buf.is_null() && (*buf).error == 0 {
        (*buf).error = XmlParserErrors::XmlBufOverflow as _;
    }
}

/**
 * xmlBufBackToBuffer:
 * @buf: new buffer wrapping the old one
 *
 * Function to be called once internal processing had been done to
 * update back the buffer provided by the user. This can lead to
 * a failure in case the size accumulated in the xmlBuf is larger
 * than what an xmlBuffer can support on 64 bits (i32::MAX)
 * The xmlBufPtr @buf wrapper is deallocated by this call in any case.
 *
 * Returns the old xmlBufferPtr unless the call failed and NULL is returned
 */
pub(crate) unsafe extern "C" fn xmlBufBackToBuffer(buf: XmlBufPtr) -> XmlBufferPtr {
    if buf.is_null() {
        return null_mut();
    }
    CHECK_COMPAT!(buf);
    if (*buf).error != 0 || (*buf).buffer.is_null() {
        xmlBufFree(buf);
        return null_mut();
    }

    let ret: XmlBufferPtr = (*buf).buffer;
    /*
     * What to do in case of error in the buffer ???
     */
    if (*buf).using > i32::MAX as usize {
        /*
         * Worse case, we really allocated and used more than the
         * maximum allowed memory for an xmlBuffer on this architecture.
         * Keep the buffer but provide a truncated size value.
         */
        xml_buf_overflow_error(buf, c"Used size too big for xmlBuffer".as_ptr() as _);
        (*ret).using = i32::MAX as _;
        (*ret).size = i32::MAX as _;
    } else if (*buf).size > i32::MAX as usize {
        /*
         * milder case, we allocated more than the maximum allowed memory
         * for an xmlBuffer on this architecture, but used less than the
         * limit.
         * Keep the buffer but provide a truncated size value.
         */
        xml_buf_overflow_error(buf, c"Allocated size too big for xmlBuffer".as_ptr() as _);
        (*ret).using = (*buf).using as _;
        (*ret).size = i32::MAX as _;
    } else {
        (*ret).using = (*buf).using as _;
        (*ret).size = (*buf).size as _;
    }
    (*ret).alloc = (*buf).alloc;
    (*ret).content = (*buf).content;
    (*ret).content_io = (*buf).content_io;
    xml_free(buf as _);
    ret
}

/**
 * xmlBufMergeBuffer:
 * @buf: an xmlBufPtr
 * @buffer: the buffer to consume into @buf
 *
 * The content of @buffer is appended to @buf and @buffer is freed
 *
 * Returns -1 in case of error, 0 otherwise, in any case @buffer is freed
 */
pub(crate) unsafe extern "C" fn xmlBufMergeBuffer(buf: XmlBufPtr, buffer: XmlBufferPtr) -> c_int {
    let mut ret: c_int = 0;

    if buf.is_null() || (*buf).error != 0 {
        xml_buffer_free(buffer);
        return -1;
    }
    CHECK_COMPAT!(buf);
    if !buffer.is_null() && !(*buffer).content.is_null() && (*buffer).using > 0 {
        ret = xmlBufAdd(buf, (*buffer).content, (*buffer).using as _);
    }
    xml_buffer_free(buffer);
    ret
}

/**
 * xmlBufResetInput:
 * @buf: an xmlBufPtr
 * @input: an xmlParserInputPtr
 *
 * Update the input to use the current set of pointers from the buffer.
 *
 * Returns -1 in case of error, 0 otherwise
 */
pub(crate) unsafe extern "C" fn xmlBufResetInput(
    buf: XmlBufPtr,
    input: XmlParserInputPtr,
) -> c_int {
    if input.is_null() || buf.is_null() || (*buf).error != 0 {
        return -1;
    }
    CHECK_COMPAT!(buf);
    (*input).base = (*buf).content;
    (*input).cur = (*buf).content;
    (*input).end = (*buf).content.add((*buf).using);
    0
}

/**
 * xmlBufGetInputBase:
 * @buf: an xmlBufPtr
 * @input: an xmlParserInputPtr
 *
 * Get the base of the @input relative to the beginning of the buffer
 *
 * Returns the size_t corresponding to the displacement
 */
pub(crate) unsafe extern "C" fn xmlBufGetInputBase(
    buf: XmlBufPtr,
    input: XmlParserInputPtr,
) -> size_t {
    let mut base: size_t;

    if input.is_null() || buf.is_null() || (*buf).error != 0 {
        return 0;
    }
    CHECK_COMPAT!(buf);
    base = (*input).base.offset_from((*buf).content) as _;
    /*
     * We could do some pointer arithmetic checks but that's probably
     * sufficient.
     */
    if base > (*buf).size {
        xml_buf_overflow_error(buf, c"Input reference outside of the buffer".as_ptr() as _);
        base = 0;
    }
    base
}

/**
 * xmlBufSetInputBaseCur:
 * @buf: an xmlBufPtr
 * @input: an xmlParserInputPtr
 * @base: the base value relative to the beginning of the buffer
 * @cur: the cur value relative to the beginning of the buffer
 *
 * Update the input to use the base and cur relative to the buffer
 * after a possible reallocation of its content
 *
 * Returns -1 in case of error, 0 otherwise
 */
pub(crate) unsafe extern "C" fn xmlBufSetInputBaseCur(
    buf: XmlBufPtr,
    input: XmlParserInputPtr,
    base: size_t,
    cur: size_t,
) -> c_int {
    if input.is_null() {
        return -1;
    }
    if buf.is_null() || (*buf).error != 0 {
        (*input).base = c"".as_ptr() as _;
        (*input).cur = (*input).base;
        (*input).end = (*input).base;
        return -1;
    }
    CHECK_COMPAT!(buf);
    (*input).base = (*buf).content.add(base);
    (*input).cur = (*input).base.add(cur);
    (*input).end = (*buf).content.add((*buf).using);
    0
}

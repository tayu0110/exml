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

pub(crate) use crate::buf::libxml_api::*;
use crate::libxml::{
    globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
    parser_internals::XML_MAX_TEXT_LENGTH,
    tree::{XmlBufferAllocationScheme, BASE_BUFFER_SIZE},
    xmlstring::{xml_strchr, xml_strlen, XmlChar},
};

const SIZE_MAX: size_t = size_t::MAX;

mod legacy {
    use std::ffi::c_uint;

    use crate::{
        error::{XmlErrorDomain, XmlParserErrors, __xml_simple_error},
        globals::{
            get_default_buffer_allocation_scheme, get_default_buffer_size,
            set_default_buffer_allocation_scheme,
        },
    };

    use super::*;

    /**
     * xmlBuffer:
     *
     * A buffer structure, this old construct is limited to 2GB and
     * is being deprecated, use API with xmlBuf instead
     */
    pub type XmlBufferPtr = *mut XmlBuffer;
    #[repr(C)]
    pub struct XmlBuffer {
        pub content: *mut XmlChar,                   /* The buffer content UTF8 */
        pub using: c_uint,                           /* The buffer size used */
        pub(crate) size: c_uint,                     /* The buffer size */
        pub(crate) alloc: XmlBufferAllocationScheme, /* The realloc method */
        pub(crate) content_io: *mut XmlChar,         /* in IO mode we may have a different base */
    }

    /**
     * xmlBuf:
     *
     * A buffer structure, new one, the actual structure internals are not public
     */

    #[repr(C)]
    pub struct XmlBuf {
        pub(crate) content: *mut XmlChar, /* The buffer content UTF8 */
        pub(crate) compat_use: c_uint,    /* for binary compatibility */
        pub(crate) compat_size: c_uint,   /* for binary compatibility */
        pub(crate) alloc: XmlBufferAllocationScheme, /* The realloc method */
        pub(crate) content_io: *mut XmlChar, /* in IO mode we may have a different base */
        pub(crate) using: size_t,         /* The buffer size used */
        pub(crate) size: size_t,          /* The buffer size */
        pub(crate) buffer: XmlBufferPtr,  /* wrapper for an old buffer */
        pub(crate) error: c_int,          /* an error code if a failure occurred */
    }

    /**
     * xmlBufPtr:
     *
     * A pointer to a buffer structure, the actual structure internals are not
     * public
     */

    pub type XmlBufPtr = *mut XmlBuf;

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
            XmlErrorDomain::XmlFromBuffer,
            XmlParserErrors::XmlErrNoMemory,
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
    pub(crate) unsafe extern "C" fn xml_buf_create() -> XmlBufPtr {
        let ret: XmlBufPtr = xml_malloc(size_of::<XmlBuf>()) as XmlBufPtr;
        if ret.is_null() {
            xml_buf_memory_error(null_mut(), c"creating buffer".as_ptr() as _);
            return null_mut();
        }
        (*ret).using = 0;
        (*ret).error = 0;
        (*ret).buffer = null_mut();
        (*ret).size = get_default_buffer_size();
        UPDATE_COMPAT!(ret);
        (*ret).alloc = get_default_buffer_allocation_scheme();
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
    pub(crate) unsafe extern "C" fn xml_buf_create_size(size: size_t) -> XmlBufPtr {
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
        (*ret).alloc = get_default_buffer_allocation_scheme();
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
    pub(crate) unsafe extern "C" fn xml_buf_set_allocation_scheme(
        buf: XmlBufPtr,
        scheme: XmlBufferAllocationScheme,
    ) -> c_int {
        if buf.is_null() || (*buf).error != 0 {
            return -1;
        }
        if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo) {
            return -1;
        }
        if matches!(
            scheme,
            XmlBufferAllocationScheme::XmlBufferAllocDoubleit
                | XmlBufferAllocationScheme::XmlBufferAllocExact
                | XmlBufferAllocationScheme::XmlBufferAllocHybrid
                | XmlBufferAllocationScheme::XmlBufferAllocBounded
        ) {
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
    pub(crate) unsafe extern "C" fn xml_buf_get_allocation_scheme(buf: XmlBufPtr) -> c_int {
        if buf.is_null() {
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
    pub(crate) unsafe extern "C" fn xml_buf_free(buf: XmlBufPtr) {
        if buf.is_null() {
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
    pub(crate) unsafe extern "C" fn xml_buf_empty(buf: XmlBufPtr) {
        if buf.is_null() || (*buf).error != 0 {
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
            if ((*buf).using + len + 1 >= XML_MAX_TEXT_LENGTH)
                || ((*buf).size >= XML_MAX_TEXT_LENGTH)
            {
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
    pub(crate) unsafe extern "C" fn xml_buf_grow(buf: XmlBufPtr, len: c_int) -> c_int {
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
    pub(crate) unsafe extern "C" fn xml_buf_resize(buf: XmlBufPtr, size: size_t) -> c_int {
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
                *(*buf).content.add((*buf).using) = 0;
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
    pub(crate) unsafe extern "C" fn xml_buf_add(
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
        if len as size_t >= (*buf).size - (*buf).using {
            if len as size_t >= SIZE_MAX - (*buf).using {
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
            if xml_buf_resize(buf, need_size) == 0 {
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
    pub(crate) unsafe extern "C" fn xml_buf_cat(buf: XmlBufPtr, str: *const XmlChar) -> c_int {
        if buf.is_null() || (*buf).error != 0 {
            return -1;
        }
        CHECK_COMPAT!(buf);
        if str.is_null() {
            return -1;
        }
        xml_buf_add(buf, str, -1)
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
    pub(crate) unsafe extern "C" fn xml_buf_ccat(buf: XmlBufPtr, str: *const c_char) -> c_int {
        xml_buf_cat(buf, str as _)
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
    pub(crate) unsafe extern "C" fn xml_buf_write_quoted_string(
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
                xml_buf_ccat(buf, c"\"".as_ptr() as _);
                base = string;
                cur = string;
                while *cur != 0 {
                    if *cur == b'"' {
                        if base != cur {
                            xml_buf_add(buf, base, cur.offset_from(base) as _);
                        }
                        xml_buf_add(buf, c"&quot;".as_ptr() as _, 6);
                        cur = cur.add(1);
                        base = cur;
                    } else {
                        cur = cur.add(1);
                    }
                }
                if base != cur {
                    xml_buf_add(buf, base, cur.offset_from(base) as _);
                }
                xml_buf_ccat(buf, c"\"".as_ptr() as _);
            } else {
                xml_buf_ccat(buf, c"\'".as_ptr() as _);
                xml_buf_cat(buf, string);
                xml_buf_ccat(buf, c"\'".as_ptr() as _);
            }
        } else {
            xml_buf_ccat(buf, c"\"".as_ptr() as _);
            xml_buf_cat(buf, string);
            xml_buf_ccat(buf, c"\"".as_ptr() as _);
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
    pub(crate) unsafe extern "C" fn xml_buf_avail(buf: XmlBufPtr) -> size_t {
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
    pub(crate) unsafe extern "C" fn xml_buf_length(buf: XmlBufPtr) -> size_t {
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
    pub(crate) unsafe extern "C" fn xml_buf_is_empty(buf: XmlBufPtr) -> c_int {
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
    pub(crate) unsafe extern "C" fn xml_buf_add_len(buf: XmlBufPtr, len: size_t) -> c_int {
        if buf.is_null() || (*buf).error != 0 {
            return -1;
        }
        CHECK_COMPAT!(buf);
        if len >= (*buf).size - (*buf).using {
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
    pub(crate) unsafe extern "C" fn xml_buf_detach(buf: XmlBufPtr) -> *mut XmlChar {
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
    pub(crate) unsafe extern "C" fn xml_buf_dump(mut file: *mut FILE, buf: XmlBufPtr) -> size_t {
        if buf.is_null() || (*buf).error != 0 {
            return 0;
        }
        if (*buf).content.is_null() {
            return 0;
        }
        CHECK_COMPAT!(buf);
        if file.is_null() {
            extern "C" {
                // Does it work ???
                #[allow(unused)]
                static stdout: *mut FILE;
            }
            file = stdout;
        }
        let ret: size_t = fwrite((*buf).content as _, 1, (*buf).using, file);
        ret
    }

    /**
     * xmlBufContent:
     * @buf:  the buffer
     *
     * Function to extract the content of a buffer
     *
     * Returns the internal content
     */
    pub unsafe extern "C" fn xml_buf_content(buf: *const XmlBuf) -> *mut XmlChar {
        if buf.is_null() || (*buf).error != 0 {
            return null_mut();
        }

        (*buf).content
    }

    /**
     * xmlBufEnd:
     * @buf:  the buffer
     *
     * Function to extract the end of the content of a buffer
     *
     * Returns the end of the internal content or NULL in case of error
     */
    pub unsafe extern "C" fn xml_buf_end(buf: XmlBufPtr) -> *mut XmlChar {
        if buf.is_null() || (*buf).error != 0 {
            return null_mut();
        }
        CHECK_COMPAT!(buf);

        (*buf).content.add((*buf).using)
    }

    /**
     * xmlBufUse:
     * @buf:  the buffer
     *
     * Function to get the length of a buffer
     *
     * Returns the length of data in the internal content
     */
    pub unsafe extern "C" fn xml_buf_use(buf: XmlBufPtr) -> size_t {
        if buf.is_null() || (*buf).error != 0 {
            return 0;
        }
        CHECK_COMPAT!(buf);

        (*buf).using
    }

    /**
     * xmlBufShrink:
     * @buf:  the buffer to dump
     * @len:  the number of XmlChar to remove
     *
     * Remove the beginning of an XML buffer.
     * NOTE that this routine behaviour differs from xmlBufferShrink()
     * as it will return 0 on error instead of -1 due to size_t being
     * used as the return type.
     *
     * Returns the number of byte removed or 0 in case of failure
     */
    pub unsafe extern "C" fn xml_buf_shrink(buf: XmlBufPtr, len: size_t) -> size_t {
        if buf.is_null() || (*buf).error != 0 {
            return 0;
        }
        CHECK_COMPAT!(buf);
        if len == 0 {
            return 0;
        }
        if len > (*buf).using {
            return 0;
        }

        (*buf).using -= len;
        if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
            && !(*buf).content_io.is_null()
        {
            /*
             * we just move the content pointer, but also make sure
             * the perceived buffer size has shrunk accordingly
             */
            (*buf).content = (*buf).content.add(len);
            (*buf).size -= len;

            /*
             * sometimes though it maybe be better to really shrink
             * on IO buffers
             */
            if matches!((*buf).alloc, XmlBufferAllocationScheme::XmlBufferAllocIo)
                && !(*buf).content_io.is_null()
            {
                let start_buf: size_t = (*buf).content.offset_from((*buf).content_io) as _;
                if start_buf >= (*buf).size {
                    memmove(
                        (*buf).content_io as _,
                        (*buf).content.add(0) as _,
                        (*buf).using,
                    );
                    (*buf).content = (*buf).content_io;
                    *(*buf).content.add((*buf).using) = 0;
                    (*buf).size += start_buf;
                }
            }
        } else {
            memmove(
                (*buf).content as _,
                (*buf).content.add(len) as _,
                (*buf).using,
            );
            *(*buf).content.add((*buf).using) = 0;
        }
        UPDATE_COMPAT!(buf);
        len
    }

    /**
     * xmlSetBufferAllocationScheme:
     * @scheme:  allocation method to use
     *
     * Set the buffer allocation method.  Types are
     * xmlBufferAllocationScheme::XML_BUFFER_ALLOC_EXACT - use exact sizes, keeps memory usage down
     * xmlBufferAllocationScheme::XML_BUFFER_ALLOC_DOUBLEIT - double buffer when extra needed,
     *                             improves performance
     */
    pub unsafe extern "C" fn xml_set_buffer_allocation_scheme(scheme: XmlBufferAllocationScheme) {
        if matches!(
            scheme,
            XmlBufferAllocationScheme::XmlBufferAllocExact
                | XmlBufferAllocationScheme::XmlBufferAllocDoubleit
                | XmlBufferAllocationScheme::XmlBufferAllocHybrid
        ) {
            set_default_buffer_allocation_scheme(scheme);
        }
    }

    /**
     * xmlGetBufferAllocationScheme:
     *
     * Types are
     * xmlBufferAllocationScheme::XML_BUFFER_ALLOC_EXACT - use exact sizes, keeps memory usage down
     * xmlBufferAllocationScheme::XML_BUFFER_ALLOC_DOUBLEIT - double buffer when extra needed,
     *                             improves performance
     * xmlBufferAllocationScheme::XML_BUFFER_ALLOC_HYBRID - use exact sizes on small strings to keep memory usage tight
     *                            in normal usage, and doubleit on large strings to avoid
     *                            pathological performance.
     *
     * Returns the current allocation scheme
     */
    pub unsafe extern "C" fn xml_get_buffer_allocation_scheme() -> XmlBufferAllocationScheme {
        get_default_buffer_allocation_scheme()
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
}

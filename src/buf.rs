use std::{
    ffi::{CStr, CString},
    io::{ErrorKind, Write},
    mem::take,
    ops::{Deref, DerefMut},
    ptr::{null_mut, NonNull},
};

use anyhow::{bail, ensure};

use crate::{
    error::{XmlErrorDomain, XmlParserErrors},
    globals::GLOBAL_STATE,
    libxml::{
        parser_internals::XML_MAX_TEXT_LENGTH,
        tree::{XmlBufferAllocationScheme, BASE_BUFFER_SIZE},
    },
    private::error::__xml_simple_error,
};

unsafe fn xml_buf_memory_error(buf: &mut XmlBuf, extra: &str) {
    let extra = CString::new(extra).unwrap();
    __xml_simple_error(
        XmlErrorDomain::XmlFromBuffer,
        XmlParserErrors::XmlErrNoMemory,
        null_mut(),
        null_mut(),
        extra.as_ptr(),
    );
    if buf.error.is_ok() {
        buf.error = XmlParserErrors::XmlErrNoMemory;
    }
}

/**
 * xmlBufOverflowError:
 * @extra:  extra information
 *
 * Handle a buffer overflow error
 * To be improved...
 */
pub(crate) unsafe fn xml_buf_overflow_error(buf: &mut XmlBuf, extra: &str) {
    let extra = CString::new(extra).unwrap();
    __xml_simple_error(
        XmlErrorDomain::XmlFromBuffer,
        XmlParserErrors::XmlBufOverflow,
        null_mut(),
        null_mut(),
        extra.as_ptr(),
    );
    if buf.is_ok() {
        buf.error = XmlParserErrors::XmlBufOverflow;
    }
}

#[derive(Debug, Clone)]
pub struct XmlBuf {
    content: Box<[u8]>,
    next_use: usize,
    // When the `scheme` is `XmlBufferAllocIO`,
    // `next_rw` is also required to indicate how far the buffer has been read or written.
    next_rw: usize,
    scheme: XmlBufferAllocationScheme,
    error: XmlParserErrors,
}

impl XmlBuf {
    pub(crate) fn new() -> Self {
        let (default_buffer_size, scheme) = GLOBAL_STATE
            .with_borrow(|state| (state.default_buffer_size, state.buffer_alloc_scheme));
        Self {
            content: vec![0; default_buffer_size].into_boxed_slice(),
            next_use: 0,
            next_rw: 0,
            scheme,
            error: XmlParserErrors::XmlErrOK,
        }
    }

    pub(crate) fn with_capacity(size: usize) -> Self {
        let scheme = GLOBAL_STATE.with_borrow(|state| state.buffer_alloc_scheme);
        Self {
            content: vec![0; if size != 0 { size + 1 } else { 0 }].into_boxed_slice(), // For NULL-terminator, +1
            next_use: 0,
            next_rw: 0,
            scheme,
            error: XmlParserErrors::XmlErrOK,
        }
    }

    pub(crate) fn set_allocation_scheme(
        &mut self,
        scheme: XmlBufferAllocationScheme,
    ) -> Result<(), anyhow::Error> {
        ensure!(
            self.error.is_ok(),
            "Failed to set scheme: Some errors have already occured"
        );
        ensure!(
            self.scheme != XmlBufferAllocationScheme::XmlBufferAllocIo,
            "Failed to set scheme: XmlBufferAllocIO has been already set."
        );

        if matches!(
            scheme,
            XmlBufferAllocationScheme::XmlBufferAllocDoubleit
                | XmlBufferAllocationScheme::XmlBufferAllocExact
                | XmlBufferAllocationScheme::XmlBufferAllocHybrid
                | XmlBufferAllocationScheme::XmlBufferAllocBounded
                | XmlBufferAllocationScheme::XmlBufferAllocIo
        ) {
            self.scheme = scheme;
            return Ok(());
        }

        Err(anyhow::anyhow!("Unsupported allocation scheme."))
    }

    pub(crate) fn current_allocation_scheme(&self) -> XmlBufferAllocationScheme {
        self.scheme
    }

    /// # Note
    /// This method *does not shrink the internal allocated buffer*.  
    pub(crate) fn clear(&mut self) {
        if !self.error.is_ok() {
            return;
        }

        self.next_use = 0;
        self.next_rw = 0;
        if !self.content.is_empty() {
            self.content[0] = 0;
        }
        // TODO: If `self.scheme` is `XmlBufferAllocIO`,
        // reset the cursor of `content_io` and put NULL-terminator to the head of `content_io`.
    }

    pub(crate) fn len(&self) -> usize {
        self.next_use
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub(crate) fn capacity(&self) -> usize {
        self.content.len()
    }

    /// Expand the buffer so that the remaining capacity is greater than or equal to `additional`.
    ///
    /// If the expantion is successful, return the remaining capacity after expantion,  
    /// otherwise, return `Err`.
    fn grow_inner(&mut self, additional: usize) -> Result<usize, anyhow::Error> {
        ensure!(
            self.error.is_ok(),
            "Failed to grow: Some errors have been already occured."
        );

        if self.next_use + additional < self.content.len() {
            return Ok(self.content.len() - self.next_use);
        }
        if additional >= usize::MAX - self.next_use {
            const MSG: &str = "growing buffer past SIZE_MAX";
            unsafe {
                xml_buf_memory_error(self, MSG);
            }
            bail!(MSG);
        }

        let mut size = if self.content.len() > additional {
            self.content.len().saturating_mul(2)
        } else {
            let size = self.next_use + additional;
            size.saturating_add(100)
        };

        if matches!(
            self.scheme,
            XmlBufferAllocationScheme::XmlBufferAllocBounded
        ) {
            /*
             * Used to provide parsing limits
             */
            if self.next_use + additional + 1 >= XML_MAX_TEXT_LENGTH
                || self.content.len() >= XML_MAX_TEXT_LENGTH
            {
                const MSG: &str = "buffer error: text too long\n";
                unsafe {
                    xml_buf_memory_error(self, MSG);
                }
                bail!(MSG);
            }
            size = size.max(XML_MAX_TEXT_LENGTH);
        }
        let mut new = vec![0; size].into_boxed_slice();
        if !self.is_empty() {
            new[..self.next_use].copy_from_slice(&self.content[..self.next_use]);
        }
        self.content = new;
        // for NULL-terminator, -1
        Ok(self.content.len() - self.next_use - 1)
    }

    pub(crate) fn grow(&mut self, additional: usize) -> Result<usize, anyhow::Error> {
        if additional == 0 {
            return Ok(0);
        }
        self.grow_inner(additional)
    }

    pub(crate) fn resize(&mut self, new_size: usize) -> Result<(), anyhow::Error> {
        ensure!(
            self.error.is_ok(),
            "Failed to resize: some errors have already occured."
        );

        if matches!(
            self.scheme,
            XmlBufferAllocationScheme::XmlBufferAllocBounded
        ) {
            /*
             * Used to provide parsing limits
             */
            if new_size >= XML_MAX_TEXT_LENGTH {
                const MSG: &str = "buffer error: text too long\n";
                unsafe {
                    xml_buf_memory_error(self, MSG);
                }
                bail!(MSG);
            }
        }

        /* Don't resize if we don't have to */
        if new_size < self.content.len() {
            return Ok(());
        }

        /* figure out new size */
        let new_size = match self.scheme {
            XmlBufferAllocationScheme::XmlBufferAllocIo
            | XmlBufferAllocationScheme::XmlBufferAllocDoubleit => {
                /*take care of empty case*/
                let mut now = if self.content.len() == 0 {
                    new_size.saturating_add(10)
                } else {
                    self.content.len()
                };
                while new_size > now {
                    let (new, f) = now.overflowing_mul(2);
                    if f {
                        const MSG: &str = "growing buffer";
                        unsafe {
                            xml_buf_memory_error(self, MSG);
                        }
                        bail!(MSG);
                    }
                    now = new;
                }
                now
            }
            XmlBufferAllocationScheme::XmlBufferAllocExact => new_size.saturating_add(10),
            XmlBufferAllocationScheme::XmlBufferAllocHybrid => {
                if self.next_use < BASE_BUFFER_SIZE {
                    new_size
                } else {
                    let mut now = self.content.len();
                    while new_size > now {
                        let (new, f) = now.overflowing_mul(2);
                        if f {
                            const MSG: &str = "growing buffer";
                            unsafe {
                                xml_buf_memory_error(self, MSG);
                            }
                            bail!(MSG);
                        }
                        now = new;
                    }
                    now
                }
            }
            _ => new_size.saturating_add(10),
        };

        // if matches!(self.scheme, XmlBufferAllocationScheme::XmlBufferAllocIo)
        //     && !(*buf).content_io.is_null()
        // {
        //     start_buf = (*buf).content.offset_from((*buf).content_io) as _;

        //     if start_buf > new_size {
        //         /* move data back to start */
        //         memmove((*buf).content_io as _, (*buf).content as _, self.next_use);
        //         (*buf).content = (*buf).content_io;
        //         *(*buf).content.add(self.next_use) = 0;
        //         self.capacity() += start_buf;
        //     } else {
        //         rebuf = xml_realloc((*buf).content_io as _, start_buf + new_size) as *mut XmlChar;
        //         if rebuf.is_null() {
        //             xml_buf_memory_error(buf, c"growing buffer".as_ptr() as _);
        //             return 0;
        //         }
        //         (*buf).content_io = rebuf;
        //         (*buf).content = rebuf.add(start_buf);
        //     }
        // } else {
        let mut new = vec![0; new_size].into_boxed_slice();
        if !self.is_empty() {
            new[..self.next_use].copy_from_slice(&self.content[..self.next_use]);
        }
        self.content = new;

        Ok(())
    }

    pub(crate) fn push_bytes(&mut self, bytes: &[u8]) -> Result<(), Option<XmlParserErrors>> {
        if !self.error.is_ok() {
            return Err(None);
        }
        let len = bytes.len();

        if len == 0 {
            return Ok(());
        }

        if self.next_use + len >= self.content.len() {
            if len >= usize::MAX - self.len() {
                unsafe {
                    xml_buf_memory_error(self, "growing buffer past SIZE_MAX");
                }
                return Err(None);
            }
            let need_size = self.len() + len + 1; // for NULL-terminator, +1
            if self.scheme == XmlBufferAllocationScheme::XmlBufferAllocBounded
                && need_size >= XML_MAX_TEXT_LENGTH
            {
                unsafe {
                    xml_buf_memory_error(self, "buffer error: text too long\n");
                }
                return Err(None);
            }

            self.resize(need_size).map_err(|_| {
                unsafe {
                    xml_buf_memory_error(self, "growing buffer");
                }
                Some(XmlParserErrors::XmlErrNoMemory)
            })?;
        }

        self.content[self.next_use..self.next_use + len].copy_from_slice(bytes);
        self.next_use += len;
        self.content[self.next_use] = 0;
        Ok(())
    }

    pub(crate) fn push_cstr(&mut self, s: &CStr) -> Result<(), Option<XmlParserErrors>> {
        self.push_bytes(s.to_bytes())
    }

    pub(crate) fn push_quoted_cstr(&mut self, s: &CStr) -> Result<(), Option<XmlParserErrors>> {
        if s.to_bytes().contains(&b'"') {
            if s.to_bytes().contains(&b'\'') {
                // If `s` contains both single and double-quote, quote with double-quote
                // and escape inner double-quotes
                self.push_cstr(c"\"")?;
                let bytes = s.to_bytes();
                let mut split = bytes.split(|&c| c == b'"');
                self.push_bytes(split.next().unwrap())?;
                for chunk in split {
                    self.push_cstr(c"&quot;")?;
                    self.push_bytes(chunk)?;
                }
                self.push_cstr(c"\"")
            } else {
                // If `s` contains only double-quote, quote with single-quote
                self.push_cstr(c"'")?;
                self.push_cstr(s)?;
                self.push_cstr(c"'")
            }
        } else {
            // If `s` does not contain double-quotes, quote with double-quote
            self.push_cstr(c"\"")?;
            self.push_cstr(s)?;
            self.push_cstr(c"\"")
        }
    }

    pub(crate) fn avail(&self) -> usize {
        if self.error.is_ok() {
            (self.content.len() - self.next_use).saturating_sub(1)
        } else {
            0
        }
    }

    pub(crate) fn detach(&mut self) -> Option<Box<[u8]>> {
        self.error.is_ok().then(|| {
            if self.next_rw != 0 {
                self.trim_head(self.next_rw);
            }
            self.next_rw = 0;
            self.next_use = 0;
            take(&mut self.content)
        })
    }

    pub(crate) fn dump(&self, file: Option<&mut impl Write>) -> Result<usize, std::io::Error> {
        if !self.error.is_ok() {
            return Err(std::io::Error::new(
                ErrorKind::Unsupported,
                "Failed to dump: some errors have already occured.",
            ));
        }

        if let Some(file) = file {
            file.write_all(&self.content[self.next_rw..self.next_use])
        } else {
            std::io::stdout().write_all(&self.content[self.next_rw..self.next_use])
        }
        .map(|_| self.len())
    }

    pub(crate) fn as_mut_ptr(&mut self) -> *mut u8 {
        self.content[self.next_rw..].as_mut_ptr()
    }

    pub(crate) fn add_len(&mut self, additional: usize) -> Result<(), anyhow::Error> {
        ensure!(
            self.error.is_ok(),
            "Failed to add len: some errors have already occured."
        );
        ensure!(
            self.len() + additional < self.capacity(),
            "requested length is too large."
        );
        self.next_use += additional;
        self.content[self.next_use] = 0;
        Ok(())
    }

    /// Remove the head of the buffer.  
    /// `self.len()` will be `self.len() - len` after execution.  
    /// `self.capacity()` will not be changed.
    ///
    /// If `len` is larger than `self.len()`, this method does nothing.
    ///
    /// Return `len` if trimming successfully, otherwise return `0`.
    ///
    /// # Note
    /// This method corresponds to `xmlBufShrink` in the original libxml2.  
    /// However, in the Rust library, `shrink` invokes a reduction of the tail element and the buffer,
    /// so it is renamed.
    #[doc(alias = "xmlBufShrink")]
    pub(crate) fn trim_head(&mut self, len: usize) -> usize {
        if !self.error.is_ok() || len == 0 || len > self.next_use {
            return 0;
        }

        if self.scheme == XmlBufferAllocationScheme::XmlBufferAllocIo {
            self.content
                .copy_within(len + self.next_rw..self.next_use, 0);
            self.next_use -= self.next_rw;
            self.next_rw = 0;
        } else {
            self.content.copy_within(len..self.next_use, 0);
        }
        self.next_use -= len;
        self.content[self.next_use] = 0;
        len
    }

    pub(crate) fn is_ok(&self) -> bool {
        self.error.is_ok()
    }
}

impl AsRef<[u8]> for XmlBuf {
    fn as_ref(&self) -> &[u8] {
        &self.content[self.next_rw..self.next_use]
    }
}

pub struct XmlBufRef(NonNull<XmlBuf>);

impl XmlBufRef {
    pub(crate) fn new() -> Option<Self> {
        let new = XmlBuf::new();
        let boxed = Box::new(new);
        let leaked = Box::leak(boxed);
        NonNull::new(leaked).map(Self)
    }

    pub(crate) fn with_capacity(size: usize) -> Option<Self> {
        let new = XmlBuf::with_capacity(size);
        let boxed = Box::new(new);
        let leaked = Box::leak(boxed);
        NonNull::new(leaked).map(Self)
    }

    pub(crate) fn from_raw(ptr: *mut XmlBuf) -> Option<Self> {
        NonNull::new(ptr).map(Self)
    }

    pub(crate) fn into_inner(self) -> XmlBuf {
        unsafe { *Box::from_raw(self.0.as_ptr()) }
    }

    pub(crate) fn as_ptr(self) -> *mut XmlBuf {
        self.0.as_ptr()
    }

    pub(crate) fn free(self) {
        let _ = self.into_inner();
    }
}

impl Clone for XmlBufRef {
    fn clone(&self) -> Self {
        *self
    }
}

impl Copy for XmlBufRef {}

impl Deref for XmlBufRef {
    type Target = XmlBuf;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlBufRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

pub mod libxml_api {
    use std::{
        fs::File,
        mem::forget,
        os::fd::FromRawFd,
        slice::{from_raw_parts, from_raw_parts_mut},
    };

    use libc::{fileno, FILE};

    use crate::libxml::globals::xml_malloc;

    use super::*;

    pub type XmlBufPtr = *mut XmlBuf;

    pub extern "C" fn xml_buf_create() -> XmlBufPtr {
        XmlBufRef::new().map_or(null_mut(), |ptr| ptr.0.as_ptr())
    }

    pub(crate) extern "C" fn xml_buf_create_size(size: usize) -> XmlBufPtr {
        XmlBufRef::with_capacity(size).map_or(null_mut(), |ptr| ptr.0.as_ptr())
    }

    pub extern "C" fn xml_buf_set_allocation_scheme(
        buf: XmlBufPtr,
        scheme: XmlBufferAllocationScheme,
    ) -> i32 {
        let Some(mut buf) = XmlBufRef::from_raw(buf) else {
            return -1;
        };

        match buf.set_allocation_scheme(scheme) {
            Ok(_) => 0,
            Err(_) => -1,
        }
    }

    pub(crate) extern "C" fn xml_buf_get_allocation_scheme(buf: XmlBufPtr) -> i32 {
        let Some(buf) = XmlBufRef::from_raw(buf) else {
            return -1;
        };
        buf.scheme as i32
    }

    pub extern "C" fn xml_buf_free(buf: XmlBufPtr) {
        let Some(buf) = XmlBufRef::from_raw(buf) else {
            return;
        };
        let _ = buf.into_inner();
    }

    pub extern "C" fn xml_buf_empty(buf: XmlBufPtr) {
        let Some(mut buf) = XmlBufRef::from_raw(buf) else {
            return;
        };
        buf.clear();
    }

    pub(crate) extern "C" fn xml_buf_grow(buf: XmlBufPtr, len: i32) -> i32 {
        let Some(mut buf) = XmlBufRef::from_raw(buf) else {
            return -1;
        };

        if len < 0 {
            return -1;
        }

        buf.grow(len as usize)
            .map_or(-1, |res| res.min(i32::MAX as usize) as i32)
    }

    pub(crate) extern "C" fn xml_buf_resize(buf: XmlBufPtr, size: usize) -> i32 {
        let Some(mut buf) = XmlBufRef::from_raw(buf) else {
            return 0;
        };
        buf.resize(size).is_ok() as i32
    }

    /// # Safety
    /// - `str` must be a valid NULL-terminated string.
    pub unsafe extern "C" fn xml_buf_add(buf: XmlBufPtr, str: *const u8, len: i32) -> i32 {
        let Some(mut buf) = XmlBufRef::from_raw(buf) else {
            return -1;
        };
        if str.is_null() || len < -1 {
            return -1;
        }

        if len == 0 {
            return 0;
        }

        let res = if len == -1 {
            let bytes = CStr::from_ptr(str as *const i8).to_bytes();
            if bytes.is_empty() {
                return 0;
            }
            buf.push_bytes(bytes)
        } else {
            let bytes = from_raw_parts(str, len as usize);
            buf.push_bytes(bytes)
        };

        match res {
            Ok(_) => 0,
            Err(None) => -1,
            Err(Some(code)) => code as i32,
        }
    }

    /// # Safety
    /// - `str` must be a valid NULL-terminated string.
    pub(crate) unsafe extern "C" fn xml_buf_cat(buf: XmlBufPtr, str: *const u8) -> i32 {
        let Some(buf) = XmlBufRef::from_raw(buf) else {
            return -1;
        };
        if str.is_null() {
            return -1;
        }
        xml_buf_add(buf.0.as_ptr(), str, -1)
    }

    /// # Safety
    /// - `str` must be a valid NULL-terminated string.
    pub(crate) unsafe extern "C" fn xml_buf_ccat(buf: XmlBufPtr, str: *const i8) -> i32 {
        xml_buf_cat(buf, str as _)
    }

    /// # Safety
    /// - `string` must be a valid NULL-terminated string.
    pub(crate) unsafe extern "C" fn xml_buf_write_quoted_string(
        buf: XmlBufPtr,
        string: *const u8,
    ) -> i32 {
        let Some(mut buf) = XmlBufRef::from_raw(buf) else {
            return -1;
        };
        let s = CStr::from_ptr(string as *const i8);
        match buf.push_quoted_cstr(s) {
            Ok(_) => 0,
            Err(_) => -1,
        }
    }

    pub(crate) extern "C" fn xml_buf_avail(buf: XmlBufPtr) -> usize {
        XmlBufRef::from_raw(buf).map_or(0, |buf| buf.avail())
    }

    pub(crate) extern "C" fn xml_buf_length(buf: XmlBufPtr) -> usize {
        XmlBufRef::from_raw(buf).map_or(0, |buf| buf.len())
    }

    pub(crate) extern "C" fn xml_buf_is_empty(buf: XmlBufPtr) -> i32 {
        XmlBufRef::from_raw(buf).map_or(-1, |buf| buf.is_empty() as i32)
    }

    pub(crate) extern "C" fn xml_buf_add_len(buf: XmlBufPtr, len: usize) -> i32 {
        let Some(mut buf) = XmlBufRef::from_raw(buf) else {
            return -1;
        };
        match buf.add_len(len) {
            Ok(_) => 0,
            Err(_) => -1,
        }
    }

    /// # Safety
    /// - This method allocates new memory at C-side.
    /// - The memory that this method returns should be released C-side.
    pub(crate) unsafe extern "C" fn xml_buf_detach(buf: XmlBufPtr) -> *mut u8 {
        let Some(mut buf) = XmlBufRef::from_raw(buf) else {
            return null_mut();
        };

        let len = buf.len();
        if let Some(mem) = buf.detach() {
            // The memory of XmlBuf is managed Rust-side,
            // so if returned memory is released by C-side, UB may occurs.
            // To prevent the above, allocate new memory at C-side and copy the original data to it.
            let new_mem = xml_malloc(len + 1) as *mut u8;
            let slice = from_raw_parts_mut(new_mem, len + 1);
            slice[..len].copy_from_slice(&mem[..len]);
            slice[len] = 0;
            new_mem
        } else {
            null_mut()
        }
    }

    /// # Safety
    /// - `file` is transformed to Rust `File` in this method,
    ///   so the order of the output may change due to buffering.
    pub(crate) unsafe extern "C" fn xml_buf_dump(file: *mut FILE, buf: XmlBufPtr) -> usize {
        let Some(buf) = XmlBufRef::from_raw(buf) else {
            return 0;
        };
        if file.is_null() {
            buf.dump(None::<&mut File>).unwrap_or(0)
        } else {
            let mut file = File::from_raw_fd(fileno(file));
            let res = buf.dump(Some(&mut file));
            file.flush().ok();
            // `File` owns the file stream and try to free it when `file` is dropped.
            // However, the file stream may be used at C side after this point,
            // so we should prevent to drop `file`.
            forget(file);
            res.unwrap_or(0)
        }
    }

    /// # Safety
    /// - If the content of the buffer is changed, it may not be consistent.
    pub unsafe extern "C" fn xml_buf_content(buf: XmlBufPtr) -> *mut u8 {
        let Some(buf) = XmlBufRef::from_raw(buf) else {
            return null_mut();
        };
        if !buf.error.is_ok() {
            return null_mut();
        }
        buf.as_ref().as_ptr() as _
    }

    /// # Safety
    /// - If the content of the buffer is changed, it may not be consistent.
    pub(crate) unsafe extern "C" fn xml_buf_end(buf: XmlBufPtr) -> *mut u8 {
        let Some(buf) = XmlBufRef::from_raw(buf) else {
            return null_mut();
        };
        if !buf.error.is_ok() {
            return null_mut();
        }
        buf.as_ref().as_ptr().add(buf.next_use) as _
    }

    pub extern "C" fn xml_buf_use(buf: XmlBufPtr) -> usize {
        let Some(buf) = XmlBufRef::from_raw(buf) else {
            return 0;
        };
        buf.next_use
    }

    pub(crate) extern "C" fn xml_buf_shrink(buf: XmlBufPtr, len: usize) -> usize {
        XmlBufRef::from_raw(buf).map_or(0, |mut buf| buf.trim_head(len))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn buffer_resize_test() {
        let mut buf = XmlBuf::with_capacity(8);
        assert_eq!(buf.len(), 0);
        assert!(buf.capacity() >= 8);
        assert!(buf.resize(16).is_ok());
        assert_eq!(buf.len(), 0);
        assert!(buf.capacity() >= 16);
        assert!(buf.resize(8).is_ok());
        assert_eq!(buf.len(), 0);
        assert!(buf.capacity() >= 16);
    }

    #[test]
    fn buffer_modify_test() {
        let mut buf = XmlBuf::with_capacity(8);
        const TREE1: &[u8] = b"<abc><def/></abc>";
        buf.push_bytes(TREE1);
        assert_eq!(buf.len(), TREE1.len());
        assert_eq!(buf.as_ref(), TREE1);
        const TREE2: &[u8] = b"<ghi/>";
        buf.push_bytes(TREE2);
        assert_eq!(buf.len(), TREE1.len() + TREE2.len());
        assert_eq!(buf.as_ref(), b"<abc><def/></abc><ghi/>");

        buf.trim_head(TREE1.len());
        assert_eq!(buf.len(), TREE2.len());
        assert_eq!(buf.as_ref(), TREE2);

        buf.push_bytes(TREE1);
        assert_eq!(buf.len(), TREE1.len() + TREE2.len());
        assert_eq!(buf.as_ref(), b"<ghi/><abc><def/></abc>");

        buf.set_allocation_scheme(XmlBufferAllocationScheme::XmlBufferAllocIo);
        assert_eq!(
            buf.current_allocation_scheme(),
            XmlBufferAllocationScheme::XmlBufferAllocIo
        );

        buf.trim_head(TREE2.len());
        assert_eq!(buf.len(), TREE1.len());
        assert_eq!(buf.as_ref(), TREE1);
    }
}

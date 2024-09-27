use std::{
    ffi::{CStr, CString},
    io::{ErrorKind, Write},
    mem::take,
    ops::{Deref, DerefMut},
    ptr::{null_mut, NonNull},
};

use anyhow::{bail, ensure};

use crate::{
    globals::GLOBAL_STATE,
    libxml::{
        parser_internals::XML_MAX_TEXT_LENGTH,
        tree::{XmlBufferAllocationScheme, BASE_BUFFER_SIZE},
        xmlerror::{XmlErrorDomain, XmlParserErrors},
    },
    private::error::__xml_simple_error,
};

unsafe fn xml_buf_memory_error(buf: &mut XmlBuf, extra: &str) {
    let extra = CString::new(extra).unwrap();
    __xml_simple_error(
        XmlErrorDomain::XmlFromBuffer as _,
        XmlParserErrors::XmlErrNoMemory as _,
        null_mut(),
        null_mut(),
        extra.as_ptr() as _,
    );
    if buf.error.is_ok() {
        buf.error = XmlParserErrors::XmlErrNoMemory;
    }
}

#[derive(Debug, Clone)]
pub(crate) struct XmlBuf {
    content: Box<[u8]>,
    next_use: usize,
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
            scheme,
            error: XmlParserErrors::XmlErrOK,
        }
    }

    pub(crate) fn with_capacity(size: usize) -> Self {
        let scheme = GLOBAL_STATE.with_borrow(|state| state.buffer_alloc_scheme);
        Self {
            content: vec![0; if size != 0 { size + 1 } else { 0 }].into_boxed_slice(), // For NULL-terminator, +1
            next_use: 0,
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
        ) {
            self.scheme = scheme;
            // TODO: is it necessary ???
            // if !(*buf).buffer.is_null() {
            //     (*(*buf).buffer).alloc = scheme;
            // }
            return Ok(());
        }

        if scheme == XmlBufferAllocationScheme::XmlBufferAllocIo {
            self.scheme = scheme;
            // TODO: why is `content_io` necessary ???
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

        if additional < self.capacity() - self.len() {
            return Ok(self.capacity() - self.len());
        }
        if additional >= usize::MAX - self.next_use {
            const MSG: &str = "growing buffer past SIZE_MAX";
            unsafe {
                xml_buf_memory_error(self, MSG);
            }
            bail!(MSG);
        }

        let mut size = if self.capacity() > additional {
            if self.capacity() > usize::MAX / 2 {
                usize::MAX
            } else {
                self.capacity() * 2
            }
        } else {
            let size = self.next_use + additional;
            if size > usize::MAX - 100 {
                usize::MAX
            } else {
                size + 100
            }
        };

        if matches!(
            self.scheme,
            XmlBufferAllocationScheme::XmlBufferAllocBounded
        ) {
            /*
             * Used to provide parsing limits
             */
            if self.next_use + additional + 1 >= XML_MAX_TEXT_LENGTH
                || self.capacity() >= XML_MAX_TEXT_LENGTH
            {
                const MSG: &str = "buffer error: text too long\n";
                unsafe {
                    xml_buf_memory_error(self, MSG);
                }
                bail!(MSG);
            }
            size = size.max(XML_MAX_TEXT_LENGTH);
        }
        // if matches!(self.scheme, XmlBufferAllocationScheme::XmlBufferAllocIo)
        //     && !self.content_io.is_null()
        // {
        //     let start_buf: size_t = self.content.offset_from(self.content_io) as _;

        //     newbuf = xml_realloc(self.content_io as _, start_buf + size) as *mut XmlChar;
        //     if newbuf.is_null() {
        //         xml_buf_memory_error(buf, c"growing buffer".as_ptr() as _);
        //         return 0;
        //     }
        //     self.content_io = newbuf;
        //     self.content = newbuf.add(start_buf);
        // } else {
        let mut new = vec![0; size].into_boxed_slice();
        if !self.is_empty() {
            new[..self.len()].copy_from_slice(&self.content[..self.len()]);
        }
        self.content = new;
        // }
        // for NULL-terminator, -1
        Ok(self.capacity() - self.len() - 1)
    }

    pub(crate) fn grow(&mut self, additional: usize) -> Result<usize, anyhow::Error> {
        if additional == 0 {
            return Ok(self.capacity() - self.len() - 1);
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
        if new_size < self.capacity() {
            return Ok(());
        }

        /* figure out new size */
        let new_size = match self.scheme {
            XmlBufferAllocationScheme::XmlBufferAllocIo
            | XmlBufferAllocationScheme::XmlBufferAllocDoubleit => {
                /*take care of empty case*/
                let mut now = if self.capacity() == 0 {
                    new_size.saturating_add(10)
                } else {
                    self.capacity()
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
                    let mut now = self.capacity();
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
            new[..self.len()].copy_from_slice(&self.content[..]);
        }
        self.content = new;
        // }

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

        if len >= self.capacity() - self.len() {
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
        Ok(())
    }

    pub(crate) fn push_str(&mut self, s: &CStr) -> Result<(), Option<XmlParserErrors>> {
        self.push_bytes(s.to_bytes())
    }

    pub(crate) fn push_quoted_str(&mut self, s: &CStr) -> Result<(), Option<XmlParserErrors>> {
        if s.to_bytes().contains(&b'"') {
            if s.to_bytes().contains(&b'\'') {
                // If `s` contains both single and double-quote, quote with double-quote
                // and escape inner double-quotes
                self.push_str(c"\"")?;
                let bytes = s.to_bytes();
                let mut split = bytes.split(|&c| c == b'"');
                self.push_bytes(split.next().unwrap())?;
                for chunk in split {
                    self.push_str(c"&quot;")?;
                    self.push_bytes(chunk)?;
                }
                self.push_str(c"\"")
            } else {
                // If `s` contains only double-quote, quote with single-quote
                self.push_str(c"'")?;
                self.push_str(s)?;
                self.push_str(c"'")
            }
        } else {
            // If `s` does not contain double-quotes, quote with double-quote
            self.push_str(c"\"")?;
            self.push_str(s)?;
            self.push_str(c"\"")
        }
    }

    pub(crate) fn avail(&self) -> usize {
        if self.error.is_ok() {
            (self.capacity() - self.len()).saturating_sub(1)
        } else {
            0
        }
    }

    pub(crate) fn detach(&mut self) -> Option<Box<[u8]>> {
        self.error.is_ok().then(|| {
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
            file.write_all(&self.content[..self.next_use])
        } else {
            std::io::stdout().write_all(&self.content[..self.next_use])
        }
        .map(|_| self.len())
    }

    pub(crate) fn as_mut_ptr(&mut self) -> *mut u8 {
        self.content.as_mut_ptr()
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
}

pub(crate) struct XmlBufRef(NonNull<XmlBuf>);

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
    use std::{fs::File, mem::forget, os::fd::FromRawFd};

    use libc::{fileno, FILE};

    use super::*;

    pub(crate) type XmlBufPtr = *mut XmlBuf;

    pub(crate) extern "C" fn xml_buf_create() -> XmlBufPtr {
        XmlBufRef::new().map_or(null_mut(), |ptr| ptr.0.as_ptr())
    }

    pub(crate) extern "C" fn xml_buf_create_size(size: usize) -> XmlBufPtr {
        XmlBufRef::with_capacity(size).map_or(null_mut(), |ptr| ptr.0.as_ptr())
    }

    pub(crate) extern "C" fn xml_buf_set_allocation_scheme(
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

    pub(crate) extern "C" fn xml_buf_free(buf: XmlBufPtr) {
        let Some(buf) = XmlBufRef::from_raw(buf) else {
            return;
        };
        let _ = buf.into_inner();
    }

    pub(crate) extern "C" fn xml_buf_empty(buf: XmlBufPtr) {
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
    pub(crate) unsafe extern "C" fn xml_buf_add(buf: XmlBufPtr, str: *const u8, len: i32) -> i32 {
        let Some(mut buf) = XmlBufRef::from_raw(buf) else {
            return -1;
        };
        if str.is_null() || len < -1 {
            return -1;
        }

        let bytes = CStr::from_ptr(str as *const i8).to_bytes();
        if len == 0 || bytes.is_empty() {
            return 0;
        }

        let res = if len == -1 {
            buf.push_bytes(bytes)
        } else {
            buf.push_bytes(&bytes[..len as usize])
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
        match buf.push_quoted_str(s) {
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
    /// - Returned memory should be managed at Rust API side.   
    ///   <strong>DO NOT return in any other way than `Box::from_raw`</strong>.
    pub(crate) unsafe extern "C" fn xml_buf_detach(buf: XmlBufPtr) -> *mut u8 {
        let Some(mut buf) = XmlBufRef::from_raw(buf) else {
            return null_mut();
        };
        buf.detach()
            .map_or(null_mut(), |buf| Box::leak(buf).as_mut_ptr())
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
}

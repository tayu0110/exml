// Copyright of the original code is the following.
// --------
// buf.c: memory buffers for libxml2
//
// new buffer structures and entry points to simplify the maintenance
// of libxml2 and ensure we keep good control over memory allocations
// and stay 64 bits clean.
// The new entry point use the xmlBufPtr opaque structure and
// xmlBuf...() counterparts to the old xmlBuf...() functions
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{
    ffi::CStr,
    io::{ErrorKind, Write},
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use anyhow::{bail, ensure};

use crate::{
    error::{__xml_simple_oom_error, XmlErrorDomain, XmlParserErrors},
    globals::GLOBAL_STATE,
    parser::XML_MAX_TEXT_LENGTH,
    tree::{BASE_BUFFER_SIZE, XmlBufferAllocationScheme},
};

fn xml_buf_memory_error(buf: &mut XmlBuf, extra: &str) {
    __xml_simple_oom_error(XmlErrorDomain::XmlFromBuffer, None, Some(extra));
    if buf.error.is_ok() {
        buf.error = XmlParserErrors::XmlErrNoMemory;
    }
}

// /// Handle a buffer overflow error
// /// To be improved...
// #[doc(alias = "xmlBufOverflowError")]
// pub(crate) fn xml_buf_overflow_error(buf: &mut XmlBuf, extra: &str) {
//     __xml_simple_error!(
//         XmlErrorDomain::XmlFromBuffer,
//         XmlParserErrors::XmlBufOverflow,
//         None,
//         None,
//         extra
//     );
//     if buf.is_ok() {
//         buf.error = XmlParserErrors::XmlBufOverflow;
//     }
// }

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

    #[cfg(test)]
    pub(crate) fn current_allocation_scheme(&self) -> XmlBufferAllocationScheme {
        self.scheme
    }

    // /// # Note
    // /// This method *does not shrink the internal allocated buffer*.
    // pub(crate) fn clear(&mut self) {
    //     if !self.error.is_ok() {
    //         return;
    //     }

    //     self.next_use = 0;
    //     self.next_rw = 0;
    //     if !self.content.is_empty() {
    //         self.content[0] = 0;
    //     }
    //     // TODO: If `self.scheme` is `XmlBufferAllocIO`,
    //     // reset the cursor of `content_io` and put NULL-terminator to the head of `content_io`.
    // }

    pub(crate) fn len(&self) -> usize {
        self.next_use
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.len() == 0
    }

    // pub(crate) fn capacity(&self) -> usize {
    //     self.content.len()
    // }

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
            xml_buf_memory_error(self, MSG);
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
            // Used to provide parsing limits
            if self.next_use + additional + 1 >= XML_MAX_TEXT_LENGTH
                || self.content.len() >= XML_MAX_TEXT_LENGTH
            {
                const MSG: &str = "buffer error: text too long\n";
                xml_buf_memory_error(self, MSG);
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
            // Used to provide parsing limits
            if new_size >= XML_MAX_TEXT_LENGTH {
                const MSG: &str = "buffer error: text too long\n";
                xml_buf_memory_error(self, MSG);
                bail!(MSG);
            }
        }

        // Don't resize if we don't have to
        if new_size < self.content.len() {
            return Ok(());
        }

        // figure out new size
        let new_size = match self.scheme {
            XmlBufferAllocationScheme::XmlBufferAllocIo
            | XmlBufferAllocationScheme::XmlBufferAllocDoubleit => {
                // take care of empty case
                let mut now = if self.content.len() == 0 {
                    new_size.saturating_add(10)
                } else {
                    self.content.len()
                };
                while new_size > now {
                    let (new, f) = now.overflowing_mul(2);
                    if f {
                        const MSG: &str = "growing buffer";
                        xml_buf_memory_error(self, MSG);
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
                            xml_buf_memory_error(self, MSG);
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
                xml_buf_memory_error(self, "growing buffer past SIZE_MAX");
                return Err(None);
            }
            let need_size = self.len() + len + 1; // for NULL-terminator, +1
            if self.scheme == XmlBufferAllocationScheme::XmlBufferAllocBounded
                && need_size >= XML_MAX_TEXT_LENGTH
            {
                xml_buf_memory_error(self, "buffer error: text too long\n");
                return Err(None);
            }

            self.resize(need_size).map_err(|_| {
                xml_buf_memory_error(self, "growing buffer");
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

    // pub(crate) fn detach(&mut self) -> Option<Box<[u8]>> {
    //     self.error.is_ok().then(|| {
    //         if self.next_rw != 0 {
    //             self.trim_head(self.next_rw);
    //         }
    //         self.next_rw = 0;
    //         self.next_use = 0;
    //         take(&mut self.content)
    //     })
    // }

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

    // pub(crate) fn as_mut_ptr(&mut self) -> *mut u8 {
    //     self.content[self.next_rw..].as_mut_ptr()
    // }

    // pub(crate) fn add_len(&mut self, additional: usize) -> Result<(), anyhow::Error> {
    //     ensure!(
    //         self.error.is_ok(),
    //         "Failed to add len: some errors have already occured."
    //     );
    //     ensure!(
    //         self.len() + additional < self.capacity(),
    //         "requested length is too large."
    //     );
    //     self.next_use += additional;
    //     self.content[self.next_use] = 0;
    //     Ok(())
    // }

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

    // pub(crate) fn from_raw(ptr: *mut XmlBuf) -> Option<Self> {
    //     NonNull::new(ptr).map(Self)
    // }

    pub(crate) fn into_inner(self) -> XmlBuf {
        unsafe { *Box::from_raw(self.0.as_ptr()) }
    }

    // pub(crate) fn as_ptr(self) -> *mut XmlBuf {
    //     self.0.as_ptr()
    // }

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

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn buffer_resize_test() {
    //     let mut buf = XmlBuf::with_capacity(8);
    //     assert_eq!(buf.len(), 0);
    //     assert!(buf.capacity() >= 8);
    //     assert!(buf.resize(16).is_ok());
    //     assert_eq!(buf.len(), 0);
    //     assert!(buf.capacity() >= 16);
    //     assert!(buf.resize(8).is_ok());
    //     assert_eq!(buf.len(), 0);
    //     assert!(buf.capacity() >= 16);
    // }

    #[test]
    fn buffer_modify_test() {
        let mut buf = XmlBuf::with_capacity(8);
        const TREE1: &[u8] = b"<abc><def/></abc>";
        buf.push_bytes(TREE1).ok();
        assert_eq!(buf.len(), TREE1.len());
        assert_eq!(buf.as_ref(), TREE1);
        const TREE2: &[u8] = b"<ghi/>";
        buf.push_bytes(TREE2).ok();
        assert_eq!(buf.len(), TREE1.len() + TREE2.len());
        assert_eq!(buf.as_ref(), b"<abc><def/></abc><ghi/>");

        buf.trim_head(TREE1.len());
        assert_eq!(buf.len(), TREE2.len());
        assert_eq!(buf.as_ref(), TREE2);

        buf.push_bytes(TREE1).ok();
        assert_eq!(buf.len(), TREE1.len() + TREE2.len());
        assert_eq!(buf.as_ref(), b"<ghi/><abc><def/></abc>");

        buf.set_allocation_scheme(XmlBufferAllocationScheme::XmlBufferAllocIo)
            .ok();
        assert_eq!(
            buf.current_allocation_scheme(),
            XmlBufferAllocationScheme::XmlBufferAllocIo
        );

        buf.trim_head(TREE2.len());
        assert_eq!(buf.len(), TREE1.len());
        assert_eq!(buf.as_ref(), TREE1);
    }
}

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
}

pub(crate) struct XmlBufRef(NonNull<XmlBuf>);

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

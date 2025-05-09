//! Provide methods and data structures for dictionary of strings.  
//!
//! This module is based on `libxml/dict.h`, `dict.c`, and so on in `libxml2-v2.11.8`.  
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: string dictionary
// Description: dictionary of reusable strings, just used to avoid allocation
//         and freeing operations.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// dict.c: dictionary of reusable strings, just used to avoid allocation
//         and freeing operations.
//
// Copyright (C) 2003-2012 Daniel Veillard.
//
// Permission to use, copy, modify, and distribute this software for any
// purpose with or without fee is hereby granted, provided that the above
// copyright notice and this permission notice appear in all copies.
//
// THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
// WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
// MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. THE AUTHORS AND
// CONTRIBUTORS ACCEPT NO RESPONSIBILITY IN ANY CONCEIVABLE MANNER.
//
// Author: daniel@veillard.com

use std::{
    ffi::CStr,
    ops::{Deref, DerefMut},
    ptr::{NonNull, null},
    slice::from_raw_parts,
    sync::atomic::{AtomicI32, Ordering},
};

use anyhow::{Context, ensure};

use crate::{
    libxml::xmlstring::{XmlChar, xml_str_qequal, xml_strncmp},
    parser::xml_init_parser,
};

pub use libxml_api::*;

const MAX_HASH_LEN: usize = 3;
const MIN_DICT_SIZE: usize = 128;
const MAX_DICT_SIZE: usize = 8 * 2048;

/// Calculate a hash key using a fast hash function that works well
/// for low hash table fill.
#[doc(alias = "xmlDictComputeFastKey")]
fn xml_dict_compute_fast_key(name: &[u8], seed: i32) -> u64 {
    let mut value: u64 = seed as _;

    if name.is_empty() {
        return value;
    }
    value = value.wrapping_add(name[0] as u64);
    value <<= 5;
    let mut namelen = name.len();
    if name.len() > 10 {
        value = value.wrapping_add(*name.last().unwrap() as u64);
        namelen = 10;
    }
    for i in 1..namelen {
        value = value.wrapping_add(name[i] as u64);
    }
    value
}

// Calculate a hash key using a good hash function that works well for
// larger hash table sizes.
//
// Hash function by "One-at-a-Time Hash" see
// http://burtleburtle.net/bob/hash/doobs.html
#[doc(alias = "xmlDictComputeBigKey")]
fn xml_dict_compute_big_key(data: &[u8], seed: i32) -> u32 {
    if data.is_empty() {
        return 0;
    }

    let mut hash = seed as u32;

    for &data in data {
        hash = hash.wrapping_add(data as _);
        hash = hash.wrapping_add(hash.wrapping_shl(10));
        hash ^= hash.wrapping_shr(6);
    }

    hash = hash.wrapping_add(hash.wrapping_shl(3));
    hash ^= hash.wrapping_shr(11);
    hash = hash.wrapping_add(hash.wrapping_shl(15));

    hash
}

macro_rules! xml_dict_compute_key {
    ($dict:expr, $name:expr) => {{
        if $dict.dict.len() == MIN_DICT_SIZE {
            xml_dict_compute_fast_key($name, $dict.seed)
        } else {
            xml_dict_compute_big_key($name, $dict.seed) as _
        }
    }};
}

/// Calculate a hash key for two strings using a fast hash function
/// that works well for low hash table fill.
///
/// Neither of the two strings must be NULL.
#[doc(alias = "xmlDictComputeFastQKey")]
fn xml_dict_compute_fast_qkey(prefix: &[u8], name: &[u8], seed: i32) -> u64 {
    let mut value: u64 = seed as _;

    if prefix.is_empty() {
        value = value.wrapping_add(30 * b':' as u64);
    } else {
        value = value.wrapping_add(30 * prefix[0] as u64);
    }

    let mut plen = prefix.len();
    let mut len = name.len();
    if len > 10 {
        let mut offset: i32 = len as i32 - (plen + 1 + 1) as i32;
        if offset < 0 {
            offset = len as i32 - (10 + 1);
        }
        value = value.wrapping_add(name[offset as usize] as u64);
        len = 10;
        plen = plen.min(10);
    }

    value = value.wrapping_add(prefix[..plen].iter().fold(0u64, |s, &v| s + v as u64));
    len = len.saturating_sub(plen);
    if len > 0 {
        value = value.wrapping_add(b':' as u64);
        len -= 1;
        value = value.wrapping_add(name[..len].iter().fold(0u64, |s, &v| s + v as u64));
    }
    value
}

/// Calculate a hash key for two strings using a good hash function
/// that works well for larger hash table sizes.
///
/// Hash function by "One-at-a-Time Hash" see
/// http://burtleburtle.net/bob/hash/doobs.html
///
/// Neither of the two strings must be NULL.
#[doc(alias = "xmlDictComputeBigQKey")]
fn xml_dict_compute_big_qkey(prefix: &[u8], name: &[u8], seed: i32) -> u64 {
    let mut hash: u32 = seed as _;

    let plen = prefix.len();
    for i in 0..plen {
        hash = hash.wrapping_add(prefix[i] as u32);
        hash = hash.wrapping_add(hash.wrapping_shl(10));
        hash ^= hash.wrapping_shr(6);
    }
    hash = hash.wrapping_add(b':' as u32);
    hash = hash.wrapping_add(hash.wrapping_shl(10));
    hash ^= hash.wrapping_shr(6);

    let len = name.len();
    for i in 0..len {
        hash = hash.wrapping_add(name[i] as u32);
        hash = hash.wrapping_add(hash.wrapping_shl(10));
        hash ^= hash.wrapping_shr(6);
    }
    hash = hash.wrapping_add(hash.wrapping_shl(3));
    hash ^= hash.wrapping_shr(11);
    hash = hash.wrapping_add(hash.wrapping_shl(15));

    hash as _
}

macro_rules! xml_dict_compute_qkey {
    ($dict:expr, $prefix:expr, $name:expr) => {{
        if $dict.dict.len() == MIN_DICT_SIZE {
            xml_dict_compute_fast_qkey($prefix, $name, $dict.seed)
        } else {
            xml_dict_compute_big_qkey($prefix, $name, $dict.seed)
        }
    }};
}

#[repr(C)]
#[derive(Debug, Clone)]
struct XmlDictEntry {
    next: Option<XmlDictEntryRef>,
    name: *const XmlChar,
    len: u32,
    valid: i32,
    okey: u64,
}

impl Default for XmlDictEntry {
    fn default() -> Self {
        Self {
            next: None,
            name: null(),
            len: 0,
            valid: 0,
            okey: 0,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct XmlDictEntryRef(NonNull<XmlDictEntry>);

impl XmlDictEntryRef {
    fn new() -> Option<Self> {
        let leaked = Box::leak(Box::new(XmlDictEntry::default()));
        NonNull::new(leaked).map(Self)
    }
}

impl Deref for XmlDictEntryRef {
    type Target = XmlDictEntry;
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlDictEntryRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

#[repr(C)]
struct XmlDictStrings {
    next: Option<XmlDictStringsRef>,
    array: Vec<XmlChar>,
    next_use: usize,
    num_strings: usize,
}

#[derive(Debug, Clone, Copy)]
struct XmlDictStringsRef(NonNull<XmlDictStrings>);

impl XmlDictStringsRef {
    fn new(size: usize, next: Option<XmlDictStringsRef>) -> Option<Self> {
        let mut ds = XmlDictStrings {
            next,
            array: vec![0; size],
            next_use: 0,
            num_strings: 0,
        };
        ds.array.shrink_to_fit();
        let boxed = Box::new(ds);
        let leaked = Box::leak(boxed);
        NonNull::new(leaked).map(Self)
    }
}

impl Deref for XmlDictStringsRef {
    type Target = XmlDictStrings;
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlDictStringsRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

#[repr(C)]
pub struct XmlDict {
    ref_counter: AtomicI32,
    dict: Vec<XmlDictEntry>,
    num_elems: u32,
    strings: Option<XmlDictStringsRef>,
    subdict: Option<XmlDictRef>,
    seed: i32,
    limit: usize,
}

impl XmlDict {
    /// Set a size limit for the dictionary.  
    /// Return the previous limit of the dictionary.
    pub fn set_limit(&mut self, limit: usize) -> usize {
        let old = self.limit;
        self.limit = limit;
        old
    }

    /// Return the amount of strings allocated.  
    ///
    /// Returned value specifies how many bytes the dictionary allocated to store strings.
    pub fn get_usage(&self) -> usize {
        let mut limit = 0;
        let mut iter = self.strings;
        while let Some(pool) = iter {
            limit += pool.array.capacity();
            iter = pool.next;
        }
        limit
    }

    /// Increment reference counter of this dictionary.
    pub fn add_reference(&mut self) {
        self.ref_counter.fetch_add(1, Ordering::AcqRel);
    }

    /// Append a string `name` and return the pointer of stored string.
    ///
    /// `name` should not contain NULL-terminator.
    fn add_string(&mut self, name: &[u8]) -> Result<NonNull<XmlChar>, anyhow::Error> {
        let mut strings = self.strings;
        let mut size = 0;
        let mut limit = 0;

        while let Some(pool) = strings {
            if pool.array[pool.next_use..].len() > name.len() {
                // Found the first pool that can store `name`.
                return Ok(Self::do_add_qstring(pool, None, name));
            }

            size = size.max(pool.array.len());
            limit += pool.array.capacity();
            strings = pool.next;
        }

        // Found no pools that can store `name`.
        // Retry after new pool.

        // However, when the amount of allocated memory is already over limit,
        // NULL should be returned without allocating new memory.
        ensure!(
            self.limit == 0 || limit <= self.limit,
            "XmlDict needs more memory, but its limit is exceeded."
        );

        if size == 0 {
            size = 1000;
        } else {
            size *= 4;
        }
        size = size.max(4 * name.len());
        let new = XmlDictStringsRef::new(size, self.strings)
            .context("Failed to generate new `XmlDictStringsRef`.")?;
        self.strings = Some(new);
        Ok(Self::do_add_qstring(new, None, name))
    }

    /// Grow buffer for the dictionary.  
    /// If `new_size < 8 || MAX_DICT_SIZE < new_size` is satisfied, return `Err`, otherwise return `Ok`.
    ///
    /// When `new_size` is smaller than current dictionary size, this method do nothing.
    fn grow(&mut self, new_size: usize) -> Result<(), anyhow::Error> {
        ensure!(
            8 <= new_size,
            "Requested new dict size is too small (new_size < 8)."
        );
        ensure!(
            new_size <= MAX_DICT_SIZE,
            "Requested new dict size it too large (new_size > MAX_DICT_SIZE)."
        );

        if new_size <= self.dict.len() {
            return Ok(());
        }

        let mut old_dict = vec![XmlDictEntry::default(); new_size];
        std::mem::swap(&mut old_dict, &mut self.dict);
        let keep_keys = old_dict.len() != MIN_DICT_SIZE;

        for entry in old_dict.iter().filter(|e| e.valid != 0) {
            let okey = if keep_keys {
                entry.okey
            } else {
                unsafe {
                    xml_dict_compute_key!(self, from_raw_parts(entry.name, entry.len as usize)) as _
                }
            };
            let key = (okey % self.dict.len() as u64) as usize;

            if self.dict[key].valid == 0 {
                self.dict[key] = entry.clone();
                self.dict[key].next = None;
                self.dict[key].okey = okey;
            } else {
                let mut new = XmlDictEntryRef::new()
                    .context("Failed to allocate memory for XmlDictEntryRef")?;
                new.name = entry.name;
                new.len = entry.len;
                new.okey = okey;
                new.next = self.dict[key].next;
                new.valid = 1;
                self.dict[key].next = Some(new);
            }
        }

        for entry in old_dict {
            let mut iter = entry.next;
            while let Some(mut now) = iter {
                iter = now.next;

                let okey = if keep_keys {
                    now.okey
                } else {
                    unsafe {
                        xml_dict_compute_key!(self, from_raw_parts(now.name, now.len as usize)) as _
                    }
                };
                let key = (okey % self.dict.len() as u64) as usize;
                if self.dict[key].valid == 0 {
                    // Free allocated memory for XmlDictEntryRef and take back ownership.
                    let taken_back = *unsafe { Box::from_raw(now.0.as_ptr()) };
                    self.dict[key] = taken_back;
                    self.dict[key].valid = 1;
                    self.dict[key].okey = okey;
                    self.dict[key].next = None;
                } else {
                    now.next = self.dict[key].next;
                    now.okey = okey;
                    self.dict[key].next = Some(now);
                }
            }
        }

        Ok(())
    }

    /// Check if `name` is present in the dictionary.
    ///
    /// If present, return its reference,
    /// otherwise append `name` to the dictionary and return its refenrence.
    ///
    /// If internal error occurs, return `Err`.
    ///
    /// # Note
    /// QName inserted by `qlookup` may be duplicated
    /// bacause the hash function is incompatible with that of `qlookup`.  
    /// This specification may be modified in the future.
    ///
    /// # Examples
    /// ```rust
    /// use std::ffi::CStr;
    ///
    /// use exml::dict::XmlDictRef;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut dict = XmlDictRef::new().expect("Failed to allocate memory !");
    ///
    /// unsafe {
    ///     const S: &CStr = c"Hello, World !";
    ///     let s = dict.lookup(S.to_bytes())?;
    ///     assert_eq!(CStr::from_ptr(s.as_ptr() as *const i8), S);
    ///     let t = dict.lookup(CStr::from_ptr(s.as_ptr() as *const i8).to_bytes())?;
    ///     assert_eq!(s, t);
    ///     
    ///     const T: &CStr = c"";
    ///     let t = dict.lookup(T.to_bytes())?;
    ///     assert_eq!(CStr::from_ptr(t.as_ptr() as *const i8), T);
    ///
    ///     const U: &CStr = c"xml:ns";
    ///     let u = dict.lookup(U.to_bytes())?;
    ///     assert_eq!(CStr::from_ptr(u.as_ptr() as *const i8), U);
    /// }
    ///
    /// dict.free();
    /// # Ok(())
    /// # }
    /// ```
    pub fn lookup(&mut self, name: &[u8]) -> Result<NonNull<XmlChar>, anyhow::Error> {
        let len = name.len();
        ensure!(
            self.limit == 0 || len < self.limit,
            "The allocation limit for the dictionary is exceeded"
        );
        ensure!(
            len <= i32::MAX as usize >> 1,
            "The length of `name` is too long."
        );

        let okey = xml_dict_compute_key!(self, name);
        let key = (okey % self.dict.len() as u64) as usize;
        let mut nbi = 0;
        if self.dict[key].valid != 0 {
            if self.dict[key].okey == okey
                && self.dict[key].len == len as u32
                && unsafe { xml_strncmp(self.dict[key].name, name.as_ptr(), len as i32) == 0 }
            {
                return NonNull::new(self.dict[key].name as _)
                    .context("`name` of XmlDictEntry is invalid");
            }

            let mut insert = self.dict[key].next;
            while let Some(now) = insert {
                if now.okey == okey
                    && now.len == len as u32
                    && unsafe { xml_strncmp(now.name, name.as_ptr(), len as i32) == 0 }
                {
                    return NonNull::new(now.name as _)
                        .context("`name` of XmlDictEntry is invalid");
                }
                nbi += 1;
                insert = now.next;
            }
        }

        if let Some(subdict) = self.subdict {
            let skey = if (self.dict.len() == MIN_DICT_SIZE && subdict.dict.len() != MIN_DICT_SIZE)
                || (self.dict.len() != MIN_DICT_SIZE && subdict.dict.len() == MIN_DICT_SIZE)
            {
                xml_dict_compute_key!(subdict, name)
            } else {
                okey
            };

            let key = (skey % subdict.dict.len() as u64) as usize;
            if subdict.dict[key].valid != 0 {
                if subdict.dict[key].okey == skey
                    && subdict.dict[key].len == len as u32
                    && unsafe {
                        xml_strncmp(subdict.dict[key].name, name.as_ptr(), len as i32) == 0
                    }
                {
                    return NonNull::new(subdict.dict[key].name as _)
                        .context("`name` of XmlDictEntry is invalid");
                }

                let mut tmp = subdict.dict[key].next;
                while let Some(now) = tmp {
                    if now.okey == skey
                        && now.len == len as u32
                        && unsafe { xml_strncmp(now.name, name.as_ptr(), len as i32) == 0 }
                    {
                        return NonNull::new(now.name as _)
                            .context("`name` of XmlDictEntry is invalid");
                    }
                    nbi += 1;
                    tmp = now.next;
                }
            }
        }

        let ret = self.add_string(name)?;
        if self.dict[key].valid == 0 {
            // If `self.dict[key].valid == 0`, this entry is empty.
            // Therefore we should put new entry to `self.dict[key]`.
            self.dict[key].name = ret.as_ptr();
            self.dict[key].len = len as u32;
            self.dict[key].next = None;
            self.dict[key].valid = 1;
            self.dict[key].okey = okey;
        } else {
            // If `self.dict[key].valid != 0`, this entry is not empty and may have some following entries.
            // Therefore we should allocate memory for the new entry.
            // In original code, the new entry is appended as the tail of chain,
            // but there should be no problem to insert it at the head of the chain, and it is more simple.
            let mut entry = XmlDictEntryRef::new().context("XmlDictEntryRef::new is failed.")?;
            entry.name = ret.as_ptr();
            entry.len = len as u32;
            entry.next = self.dict[key].next;
            entry.valid = 1;
            entry.okey = okey;
            self.dict[key].next = Some(entry);
        }

        self.num_elems += 1;

        if nbi > MAX_HASH_LEN && self.dict.len() <= MAX_DICT_SIZE / 2 / MAX_HASH_LEN {
            self.grow(MAX_HASH_LEN * 2 * self.dict.len())?;
        }

        Ok(ret)
    }

    /// Check if `name` exists in the dictionary.
    ///
    /// If exists, return its internal reference,
    /// otherwise return `None`.
    ///
    /// # Examples
    /// ```rust
    /// use std::ffi::CStr;
    ///
    /// use exml::dict::XmlDictRef;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut dict = XmlDictRef::new().expect("Failed to allocate memory !");
    /// unsafe {
    ///     const S: &CStr = c"some_element";
    ///     assert!(!dict.exists(S.to_bytes()).is_some());
    ///     let s = dict.lookup(S.to_bytes())?;
    ///     assert!(dict.exists(CStr::from_ptr(s.as_ptr() as *const i8).to_bytes()).is_some());
    ///     // Given string need not be owned string by the dictionary.
    ///     assert!(dict.exists(S.to_bytes()).is_some());
    ///
    ///     const T: &CStr = c"";
    ///     assert!(!dict.exists(T.to_bytes()).is_some());
    ///     let t = dict.lookup(T.to_bytes())?;
    ///     assert!(dict.exists(CStr::from_ptr(t.as_ptr() as *const i8).to_bytes()).is_some());
    ///     assert!(dict.exists(T.to_bytes()).is_some());
    ///
    ///     const U: &CStr = c"xml:ns";
    ///     assert!(!dict.exists(U.to_bytes()).is_some());
    ///     let u = dict.lookup(U.to_bytes())?;
    ///     assert!(dict.exists(CStr::from_ptr(u.as_ptr() as *const i8).to_bytes()).is_some());
    ///     assert!(dict.exists(U.to_bytes()).is_some());
    /// }
    ///
    /// dict.free();
    /// # Ok(())
    /// # }
    /// ```
    pub fn exists(&self, name: &[u8]) -> Option<NonNull<XmlChar>> {
        // In original code, limit check and name length check is processed at the beginning of the method.
        // Regarding to limit check, I think it is unnecessary because it is processed `XmlDict::grow` ?
        // And regarding to name length check, the process can be shortcut, so it is okay to keep it.
        let len = name.len();
        if len > i32::MAX as usize / 2 {
            return None;
        }

        let okey = xml_dict_compute_key!(self, name);
        unsafe {
            eprintln!(
                "line: {}, okey: {okey}, name: {}",
                line!(),
                CStr::from_ptr(name.as_ptr() as _).to_string_lossy()
            );
        }
        let key = (okey % self.dict.len() as u64) as usize;
        if self.dict[key].valid != 0 {
            if self.dict[key].okey == okey
                && self.dict[key].len == len as u32
                && unsafe { xml_strncmp(self.dict[key].name, name.as_ptr(), len as i32) == 0 }
            {
                return NonNull::new(self.dict[key].name as _);
            }

            let mut insert = self.dict[key].next;
            while let Some(now) = insert {
                if now.okey == okey
                    && now.len == len as u32
                    && unsafe { xml_strncmp(now.name, name.as_ptr(), len as i32) == 0 }
                {
                    return NonNull::new(now.name as _);
                }
                insert = now.next;
            }
        }

        if let Some(subdict) = self.subdict {
            let skey = if (self.dict.len() == MIN_DICT_SIZE && subdict.dict.len() != MIN_DICT_SIZE)
                || (self.dict.len() != MIN_DICT_SIZE && subdict.dict.len() == MIN_DICT_SIZE)
            {
                xml_dict_compute_key!(subdict, name)
            } else {
                okey
            };

            let key = (skey % subdict.dict.len() as u64) as usize;
            if subdict.dict[key].valid != 0 {
                if subdict.dict[key].okey == skey
                    && subdict.dict[key].len == len as u32
                    && unsafe {
                        xml_strncmp(subdict.dict[key].name, name.as_ptr(), len as i32) == 0
                    }
                {
                    return NonNull::new(subdict.dict[key].name as _);
                }

                let mut tmp = subdict.dict[key].next;
                while let Some(now) = tmp {
                    if now.okey == skey
                        && now.len == len as u32
                        && unsafe { xml_strncmp(now.name, name.as_ptr(), len as i32) == 0 }
                    {
                        return NonNull::new(now.name as _);
                    }
                    tmp = now.next;
                }
            }
        }
        None
    }

    /// Add string to `pool`.
    ///
    /// If `prefix` is `None`, only `name` is just inserted to `pool` without a colon (`:`).  
    /// If `prefix` is `Some` and empty, a colon (`:`) and `name` is inserted.  
    /// Otherwise `prefix`, a colon (`:`) and `name` is inserted.
    ///
    /// `prefix` and `name` should not contain NULL-terminator.
    fn do_add_qstring(
        mut pool: XmlDictStringsRef,
        prefix: Option<&[u8]>,
        name: &[u8],
    ) -> NonNull<XmlChar> {
        let mut start = pool.next_use;
        let res = unsafe { pool.array.as_ptr().add(start) };
        if let Some(prefix) = prefix {
            let len = prefix.len();
            // Check length with including a colon.
            assert!(pool.array[start..].len() > len);
            pool.array[start..start + len].copy_from_slice(prefix);
            pool.array[start + len] = b':';
            start += len + 1;
        }
        let len = name.len();
        assert!(pool.array[start..].len() > len);
        pool.array[start..start + len].copy_from_slice(name);
        // Returned pointer referenses `pool.array[..]` as C-string,
        // so NULL-terminator should be considered to set new `next_use`.
        pool.array[start + len] = 0;
        pool.next_use = start + len + 1;
        NonNull::new(res as _).expect("Failed to add string")
    }

    /// Add QName that its prefix is `prefix` and localname is `name` to the dictionary.  
    ///
    /// If append successfully, return the local reference,
    /// otherwise return `Err`.
    ///
    /// If `prefix` is `None`, this method works as same as `XmlDict::add_string`.
    ///
    /// `prefix` and `name` should not contain NULL-terminator.
    fn add_qstring(
        &mut self,
        prefix: Option<&[u8]>,
        name: &[u8],
    ) -> Result<NonNull<XmlChar>, anyhow::Error> {
        let Some(prefix) = prefix else {
            return self.add_string(name);
        };

        let mut strings = self.strings;
        let mut size = 0;
        let mut limit = 0;
        let plen = prefix.len();
        let nlen = name.len();

        while let Some(pool) = strings {
            if pool.array[pool.next_use..].len() > plen + nlen + 1 {
                // Found the first pool that can store `name`.
                return Ok(Self::do_add_qstring(pool, Some(prefix), name));
            }

            size = size.max(pool.array.len());
            limit += pool.array.capacity();
            strings = pool.next;
        }

        // Found no pools that can store `name`.
        // Retry after new pool.

        // However, when the amount of allocated memory is already over limit,
        // NULL should be returned without allocating new memory.
        ensure!(
            self.limit == 0 || limit <= self.limit,
            "XmlDict needs more memory, but its limit is exceeded."
        );

        if size == 0 {
            size = 1000;
        } else {
            size *= 4;
        }
        size = size.max(4 * nlen);
        let new = XmlDictStringsRef::new(size, self.strings)
            .context("Failed to generate new `XmlDictStringsRef`.")?;
        self.strings = Some(new);
        Ok(Self::do_add_qstring(new, Some(prefix), name))
    }

    /// Check if QName that its prefix is `prefix` and localname is `name` is present in the dictionary.
    ///
    /// If present, return its reference,
    /// otherwise append given QName to the dictionary and return its refenrence.
    ///
    /// If internal error occurs, return `Err`.
    ///
    /// # Note
    /// QName inserted by `lookup` may be duplicated
    /// bacause the hash function is incompatible with that of `lookup`.  
    /// This specification may be modified in the future.
    ///
    /// # Examples
    /// ```rust
    /// use std::ffi::CStr;
    ///
    /// use exml::dict::XmlDictRef;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut dict = XmlDictRef::new().expect("Failed to allocate memory !");
    /// unsafe {
    ///     const S: &CStr = c"some_element";
    ///     let s = dict.qlookup(None, S.to_bytes())?;
    ///     assert_eq!(CStr::from_ptr(s.as_ptr() as *const i8), S);
    ///     let s = dict.qlookup(Some(c"".to_bytes()), S.to_bytes())?;
    ///     // In case the first argument is an empty string,
    ///     // stored string starts with a colon (':')
    ///     assert_eq!(CStr::from_ptr(s.as_ptr() as *const i8), c":some_element");
    ///
    ///     const T: &CStr = c"";
    ///     let s = dict.qlookup(None, T.to_bytes())?;
    ///     assert_eq!(CStr::from_ptr(s.as_ptr() as *const i8), T);
    ///     let s = dict.qlookup(Some(c"".to_bytes()), T.to_bytes())?;
    ///     assert_eq!(CStr::from_ptr(s.as_ptr() as *const i8), c":");
    ///
    ///     const NS: &CStr = c"xml";
    ///     const LOC: &CStr = c"ns";
    ///     let s = dict.qlookup(Some(NS.to_bytes()), LOC.to_bytes())?;
    ///     assert_eq!(CStr::from_ptr(s.as_ptr() as *const i8), c"xml:ns");
    /// }
    ///
    /// dict.free();
    /// # Ok(())
    /// # }
    /// ```
    pub fn qlookup(
        &mut self,
        prefix: Option<&[u8]>,
        name: &[u8],
    ) -> Result<NonNull<XmlChar>, anyhow::Error> {
        let Some(prefix) = prefix else {
            return self.lookup(name);
        };
        let plen = prefix.len();
        let nlen = name.len();
        let len = plen + nlen + 1;
        ensure!(
            self.limit == 0 || len < self.limit,
            "The allocation limit for the dictionary is exceeded"
        );
        ensure!(
            len <= i32::MAX as usize >> 1,
            "The length of `prefix` and `name` is too long."
        );

        let okey = xml_dict_compute_qkey!(self, prefix, name);
        let key = (okey % self.dict.len() as u64) as usize;
        let mut nbi = 0;
        if self.dict[key].valid != 0 {
            if self.dict[key].okey == okey
                && self.dict[key].len == len as u32
                && unsafe {
                    xml_str_qequal(
                        prefix.as_ptr() as _,
                        name.as_ptr() as _,
                        self.dict[key].name,
                    )
                }
            {
                return NonNull::new(self.dict[key].name as _)
                    .context("`name` of XmlDictEntry is invalid");
            }

            let mut insert = self.dict[key].next;
            while let Some(now) = insert {
                if now.okey == okey
                    && now.len == len as u32
                    && unsafe { xml_str_qequal(prefix.as_ptr() as _, name.as_ptr() as _, now.name) }
                {
                    return NonNull::new(now.name as _)
                        .context("`name` of XmlDictEntry is invalid");
                }
                nbi += 1;
                insert = now.next;
            }
        }

        if let Some(subdict) = self.subdict {
            let skey = if (self.dict.len() == MIN_DICT_SIZE && subdict.dict.len() != MIN_DICT_SIZE)
                || (self.dict.len() != MIN_DICT_SIZE && subdict.dict.len() == MIN_DICT_SIZE)
            {
                xml_dict_compute_qkey!(subdict, prefix, name)
            } else {
                okey
            };

            let key = (skey % subdict.dict.len() as u64) as usize;
            if subdict.dict[key].valid != 0 {
                if subdict.dict[key].okey == skey
                    && subdict.dict[key].len == len as u32
                    && unsafe {
                        xml_str_qequal(
                            prefix.as_ptr() as _,
                            name.as_ptr() as _,
                            subdict.dict[key].name,
                        )
                    }
                {
                    return NonNull::new(subdict.dict[key].name as _)
                        .context("`name` of XmlDictEntry is invalid");
                }

                let mut tmp = subdict.dict[key].next;
                while let Some(now) = tmp {
                    if now.okey == skey
                        && now.len == len as u32
                        && unsafe {
                            xml_str_qequal(prefix.as_ptr() as _, name.as_ptr() as _, now.name)
                        }
                    {
                        return NonNull::new(now.name as _)
                            .context("`name` of XmlDictEntry is invalid");
                    }
                    nbi += 1;
                    tmp = now.next;
                }
            }
        }

        let ret = self.add_qstring(Some(prefix), name)?;
        if self.dict[key].valid == 0 {
            // If `self.dict[key].valid == 0`, this entry is empty.
            // Therefore we should put new entry to `self.dict[key]`.
            self.dict[key].name = ret.as_ptr();
            self.dict[key].len = len as u32;
            self.dict[key].next = None;
            self.dict[key].valid = 1;
            self.dict[key].okey = okey;
        } else {
            // If `self.dict[key].valid != 0`, this entry is not empty and may have some following entries.
            // Therefore we should allocate memory for the new entry.
            // In original code, the new entry is appended as the tail of chain,
            // but there should be no problem to insert it at the head of the chain, and it is more simple.
            let mut entry = XmlDictEntryRef::new().context("XmlDictEntryRef::new is failed.")?;
            entry.name = ret.as_ptr();
            entry.len = len as u32;
            entry.next = self.dict[key].next;
            entry.valid = 1;
            entry.okey = okey;
            self.dict[key].next = Some(entry);
        }

        self.num_elems += 1;

        if nbi > MAX_HASH_LEN && self.dict.len() <= MAX_DICT_SIZE / 2 / MAX_HASH_LEN {
            self.grow(MAX_HASH_LEN * 2 * self.dict.len())?;
        }

        Ok(ret)
    }

    /// Check if the dictionary owns `str`.  
    /// In other words, check if the ownership of the memory pointed by `str` is this dictionary.  
    ///
    /// If owns, return `true`, otherwise return `false`.
    ///
    /// # Examples
    /// ```rust
    /// use std::ffi::CStr;
    ///
    /// use exml::dict::XmlDictRef;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let mut dict = XmlDictRef::new().expect("Failed to allocate memory !");
    /// unsafe {
    ///     const S: &CStr = c"some_element";
    ///     let s = dict.lookup(S.to_bytes())?;
    ///     // `s` is a reference to the string in the dictionary, so return `true`.
    ///     assert!(dict.owns(CStr::from_ptr(s.as_ptr() as *const i8)));
    ///     // but `S` is a constant value and it does not refer to the buffer in the dictionary,
    ///     // so return `false`.
    ///     assert!(!dict.owns(S));
    ///
    ///     const T: &CStr = c"";
    ///     let s = dict.lookup(T.to_bytes())?;
    ///     assert!(dict.owns(CStr::from_ptr(s.as_ptr() as *const i8)));
    ///     assert!(!dict.owns(T));
    ///
    ///     let s = dict.qlookup(Some(c"xml".to_bytes()), c"ns".to_bytes())?;
    ///     assert!(dict.owns(CStr::from_ptr(s.as_ptr() as *const i8)));
    ///     assert!(!dict.owns(c"xml:ns"));
    /// }
    ///
    /// dict.free();
    /// # Ok(())
    /// # }
    /// ```
    pub fn owns(&self, str: &CStr) -> bool {
        let mut pool = self.strings;
        while let Some(now) = pool {
            if now.array.as_ptr() <= str.to_bytes().as_ptr()
                && str.to_bytes().as_ptr() < unsafe { now.array.as_ptr().add(now.next_use) }
            {
                return true;
            }
            pool = now.next;
        }

        self.subdict.is_some_and(|subdict| subdict.owns(str))
    }

    /// Return how many strings the dictionary owns.  
    /// Returned value contains the number of strings in the dictionary associated with `XmlDictRef::create_sub`.
    pub fn len(&self) -> usize {
        self.num_elems as usize + self.subdict.map_or(0, |subdict| subdict.num_elems) as usize
    }

    /// Return if `self.len() == 0` is satisfied.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

impl Default for XmlDict {
    fn default() -> Self {
        Self {
            ref_counter: AtomicI32::new(1),
            dict: vec![XmlDictEntry::default(); MIN_DICT_SIZE],
            num_elems: 0,
            strings: None,
            subdict: None,
            seed: rand::random(),
            limit: 0,
        }
    }
}

impl Drop for XmlDict {
    fn drop(&mut self) {
        if let Some(sub) = self.subdict {
            sub.free();
        }

        while let Some(mut entry) = self.dict.pop() {
            if entry.valid == 0 {
                continue;
            }
            // Follow `entry.next` and release the respective memory.
            while let Some(next) = entry.next.map(|e| *unsafe { Box::from_raw(e.0.as_ptr()) }) {
                entry = next;
                // Lifetime of each `entry` ends at this point
            }
        }

        let mut strings = self.strings;
        while let Some(pool) = strings {
            let next = pool.next;
            // Take back ownership and release it.
            let _ = *unsafe { Box::from_raw(pool.0.as_ptr()) };
            strings = next;
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct XmlDictRef(NonNull<XmlDict>);

impl XmlDictRef {
    /// Create new `XmlDictRef`.  
    /// Return `Some` if successfully created, otherwise return `None`.
    ///
    /// # Examples
    /// ```rust
    /// use exml::dict::XmlDictRef;
    ///
    /// let dict = XmlDictRef::new().expect("Failed to allocate memory !");
    /// // Memory needs to be released after use.
    /// dict.free();
    /// // DO NOT use after this point !
    /// ```
    pub fn new() -> Option<Self> {
        xml_init_parser();

        let boxed = Box::new(XmlDict::default());
        NonNull::new(Box::leak(boxed)).map(Self)
    }

    /// Create `XmlDictRef` from raw pointer.  
    /// If `ptr` is not NULL, return `Some`, otherwise return `None`.
    pub(crate) fn from_raw(ptr: *mut XmlDict) -> Option<Self> {
        NonNull::new(ptr).map(Self)
    }

    /// Create a new dictionary, inheriting strings from the read-only dictionary `sub`.
    pub fn create_sub(sub: Option<XmlDictRef>) -> Option<XmlDictRef> {
        let dict = XmlDictRef::new();

        if let (Some(mut dict), Some(mut sub)) = (dict, sub) {
            dict.seed = sub.seed;
            dict.subdict = Some(sub);
            sub.add_reference();
        }

        dict
    }

    /// Free the memory of the dictionary.  
    /// The memory of `XmlDictRef` is managed by Rust side, so do not use `xml_free` to free it.
    pub fn free(self) {
        let mut old_refs = self.ref_counter.load(Ordering::Acquire);
        old_refs -= 1;
        if old_refs > 0 {
            self.ref_counter.store(old_refs, Ordering::Release);
            return;
        }
        self.ref_counter.store(old_refs, Ordering::Release);

        let _ = *unsafe { Box::from_raw(self.0.as_ptr()) };
    }
}

impl Deref for XmlDictRef {
    type Target = XmlDict;
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl DerefMut for XmlDictRef {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

pub(crate) mod libxml_api {
    //! libxml2 like APIs of `exml::dict`.
    //!
    //! Since memory is managed by the Rust runtime, allocation and deallocation must be done via these APIs.

    use std::{
        ffi::CStr,
        ptr::{null, null_mut},
        slice::from_raw_parts,
    };

    use crate::libxml::xmlstring::XmlChar;

    use super::{XmlDict, XmlDictRef};

    /// Alias of `*mut XmlDict`.
    pub type XmlDictPtr = *mut XmlDict;

    pub extern "C" fn xml_dict_create() -> *mut XmlDict {
        XmlDictRef::new().map_or(null_mut(), |dict| dict.0.as_ptr())
    }

    pub extern "C" fn xml_dict_set_limit(dict: *mut XmlDict, limit: usize) -> usize {
        let Some(mut dict) = XmlDictRef::from_raw(dict) else {
            return 0;
        };
        dict.set_limit(limit)
    }

    pub extern "C" fn xml_dict_get_usage(dict: *mut XmlDict) -> usize {
        let Some(dict) = XmlDictRef::from_raw(dict) else {
            return 0;
        };
        dict.get_usage()
    }

    pub extern "C" fn xml_dict_create_sub(sub: *mut XmlDict) -> *mut XmlDict {
        XmlDictRef::create_sub(XmlDictRef::from_raw(sub)).map_or(null_mut(), |dict| dict.0.as_ptr())
    }

    pub extern "C" fn xml_dict_reference(dict: *mut XmlDict) -> i32 {
        let Some(mut dict) = XmlDictRef::from_raw(dict) else {
            return -1;
        };
        dict.add_reference();
        0
    }

    pub extern "C" fn xml_dict_free(dict: *mut XmlDict) {
        if let Some(dict) = XmlDictRef::from_raw(dict) {
            dict.free();
        }
    }

    /// # Safety
    /// - `dict` and `name` can be NULL.
    /// - If `len < 0`, the user must guarantee that between `name` and the first NULL character is accessible.
    /// - If `len >= 0`, the user must guarantee that between `name` and `name.add(len)` is accessible.
    pub unsafe extern "C" fn xml_dict_lookup(
        dict: *mut XmlDict,
        name: *const XmlChar,
        len: i32,
    ) -> *const XmlChar {
        unsafe {
            let Some(mut dict) = XmlDictRef::from_raw(dict) else {
                return null();
            };

            if name.is_null() {
                return null();
            }

            let name = if len < 0 {
                CStr::from_ptr(name as _).to_bytes()
            } else {
                from_raw_parts(name, len as usize)
            };

            dict.lookup(name).map_or(null(), |name| name.as_ptr())
        }
    }

    /// # Safety
    /// - `dict` and `name` can be NULL.
    /// - If `len < 0`, the user must guarantee that between `name` and the first NULL character is accessible.
    /// - If `len >= 0`, the user must guarantee that between `name` and `name.add(len)` is accessible.
    pub unsafe extern "C" fn xml_dict_exists(
        dict: *mut XmlDict,
        name: *const XmlChar,
        len: i32,
    ) -> *const XmlChar {
        unsafe {
            let Some(dict) = XmlDictRef::from_raw(dict) else {
                return null();
            };

            if name.is_null() {
                return null();
            }

            let name = if len < 0 {
                CStr::from_ptr(name as _).to_bytes()
            } else {
                from_raw_parts(name, len as usize)
            };

            dict.exists(name).map_or(null(), |name| name.as_ptr())
        }
    }

    /// # Safety
    /// - `dict`, `prefix` and `name` can be NULL.
    /// - If `prefix` is not NULL, `prefix` must be a valid NULL-terminated string.
    /// - If `name` is not NULL, `name` must be a valid NULL-terminated string.
    pub unsafe extern "C" fn xml_dict_qlookup(
        dict: *mut XmlDict,
        prefix: *const XmlChar,
        name: *const XmlChar,
    ) -> *const XmlChar {
        unsafe {
            let Some(mut dict) = XmlDictRef::from_raw(dict) else {
                return null();
            };

            if name.is_null() {
                return null();
            }

            if prefix.is_null() {
                return dict
                    .lookup(CStr::from_ptr(name as _).to_bytes())
                    .map_or(null(), |name| name.as_ptr());
            }

            dict.qlookup(
                Some(CStr::from_ptr(prefix as _).to_bytes()),
                CStr::from_ptr(name as _).to_bytes(),
            )
            .map_or(null(), |qname| qname.as_ptr())
        }
    }

    /// # Safety
    /// - `dict` and `str` can be NULL.
    /// - `str` must be a valid NULL-terminated string.
    pub unsafe extern "C" fn xml_dict_owns(dict: *mut XmlDict, str: *const XmlChar) -> i32 {
        let Some(dict) = XmlDictRef::from_raw(dict) else {
            return -1;
        };
        if str.is_null() {
            return -1;
        }
        unsafe { dict.owns(CStr::from_ptr(str as _)) as i32 }
    }

    pub extern "C" fn xml_dict_size(dict: *mut XmlDict) -> i32 {
        let Some(dict) = XmlDictRef::from_raw(dict) else {
            return -1;
        };
        dict.len() as i32
    }

    #[deprecated = "Alias for rand::random"]
    pub(crate) extern "C" fn __xml_random() -> i32 {
        rand::random()
    }

    #[cfg(test)]
    mod tests {
        use rand::{Rng, thread_rng};

        use super::*;

        unsafe fn do_lookup_test(dict: *mut XmlDict, s: *const XmlChar) -> *const XmlChar {
            unsafe {
                assert!(xml_dict_exists(dict, s, -1).is_null());
                assert!(xml_dict_owns(dict, s) == 0);
                let stored = xml_dict_lookup(dict, s, -1);

                assert!(!xml_dict_exists(dict, s, -1).is_null());
                assert!(xml_dict_owns(dict, s) == 0);

                assert!(!xml_dict_exists(dict, stored, -1).is_null());
                assert!(xml_dict_owns(dict, stored) != 0);

                let stored2 = xml_dict_lookup(dict, stored, -1);
                assert_eq!(stored, stored2);
                stored
            }
        }

        #[test]
        fn lookup_test() {
            const S: &[&CStr] = &[c"Hello", c"xml:ns", c""];

            let dict = xml_dict_create();
            let mut stored = vec![];
            for &s in S {
                stored.push(unsafe { do_lookup_test(dict, s.as_ptr() as *const XmlChar) });
            }

            let mut rng = thread_rng();
            let mut buf = [0u8; 51];
            for _ in 0..100 {
                buf.fill_with(|| rng.gen_range(0x31..=0x7A));
                buf[50] = 0;
                unsafe {
                    do_lookup_test(dict, buf.as_ptr());
                }
            }

            for s in stored {
                unsafe {
                    let found = xml_dict_lookup(dict, s, -1);
                    assert_eq!(s, found);
                    assert!(xml_dict_owns(dict, s) != 0);
                }
            }

            xml_dict_free(dict);
        }
    }
}

#[cfg(test)]
mod tests {
    use rand::{Rng, thread_rng};

    use super::*;

    fn do_lookup_test(mut dict: XmlDictRef, s: &CStr) -> NonNull<XmlChar> {
        assert!(dict.exists(s.to_bytes()).is_none());
        assert!(!dict.owns(s));
        let stored = dict.lookup(s.to_bytes()).unwrap();

        assert!(dict.exists(s.to_bytes()).is_some());
        assert!(!dict.owns(s));

        let cstr = unsafe { CStr::from_ptr(stored.as_ptr() as *const i8) };
        assert!(dict.exists(cstr.to_bytes()).is_some());
        assert!(dict.owns(cstr));

        let stored2 = dict.lookup(cstr.to_bytes()).unwrap();
        assert_eq!(stored, stored2);
        stored
    }

    #[test]
    fn lookup_test() {
        const S: &[&CStr] = &[c"Hello", c"xml:ns", c""];

        let mut dict = XmlDictRef::new().unwrap();
        let mut stored = vec![];
        for &s in S {
            stored.push(do_lookup_test(dict, s));
        }

        let mut rng = thread_rng();
        let mut buf = [0u8; 51];
        for _ in 0..1000 {
            buf.fill_with(|| rng.gen_range(0x31..=0x7A));
            buf[50] = 0;
            do_lookup_test(dict, unsafe { CStr::from_ptr(buf.as_ptr() as *const i8) });
        }

        for s in stored {
            let cstr = unsafe { CStr::from_ptr(s.as_ptr() as *const i8) };
            let found = dict.lookup(cstr.to_bytes()).unwrap();
            assert_eq!(s, found);
            assert!(dict.owns(cstr));
        }

        dict.free();
    }

    fn do_qlookup_test(mut dict: XmlDictRef, ns: &CStr, loc: &CStr) -> NonNull<XmlChar> {
        let nslen = ns.to_bytes().len();
        let loclen = loc.to_bytes().len();
        let mut qname = [0u8; 40];
        qname[..nslen].copy_from_slice(ns.to_bytes());
        qname[nslen] = b':';
        qname[nslen + 1..nslen + 1 + loclen].copy_from_slice(loc.to_bytes());
        qname[nslen + 1 + loclen] = 0;
        assert!(!dict.owns(CStr::from_bytes_until_nul(&qname[..]).unwrap()));
        let stored = dict.qlookup(Some(ns.to_bytes()), loc.to_bytes()).unwrap();

        assert!(!dict.owns(CStr::from_bytes_until_nul(&qname[..]).unwrap()));

        let cstr = unsafe { CStr::from_ptr(stored.as_ptr() as *const i8) };
        assert!(dict.owns(cstr));
        eprintln!("cstr: {}", cstr.to_string_lossy());
        stored
    }

    #[test]
    fn qlookup_test() {
        const NS: &[&CStr] = &[c"hoge", c"xml", c"", c"foo"];
        const LOC: &[&CStr] = &[c"fuga", c"ns", c"bar", c""];

        let mut dict = XmlDictRef::new().unwrap();
        let mut stored = vec![];
        for (&ns, &loc) in NS.iter().zip(LOC.iter()) {
            stored.push(do_qlookup_test(dict, ns, loc));
        }

        let mut rng = thread_rng();
        let mut ns = [0u8; 11];
        let mut loc = [0u8; 11];
        for _ in 0..1000 {
            ns.fill_with(|| rng.gen_range(0x40..=0x7A));
            loc.fill_with(|| rng.gen_range(0x40..=0x7A));
            ns[10] = 0;
            loc[10] = 0;
            do_qlookup_test(
                dict,
                CStr::from_bytes_until_nul(&ns).unwrap(),
                CStr::from_bytes_until_nul(&loc).unwrap(),
            );
        }

        for s in stored {
            let cstr = unsafe { CStr::from_ptr(s.as_ptr() as *const i8) };
            let found = dict.lookup(cstr.to_bytes()).unwrap();
            assert_eq!(s, found);
            assert!(dict.owns(cstr));
        }

        dict.free();
    }
}

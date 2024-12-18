// Copyright of the original code is the following.
// --------
// Summary: Chained hash tables
// Description: This module implements the hash table support used in various places in the library.
//
// Copy: See Copyright for the status of this software.
//
// Author: Bjorn Reese <bjorn.reese@systematic.dk>
// --------
// hash.c: chained hash tables
//
// Reference: Your favorite introductory book on algorithms
//
// Copyright (C) 2000,2012 Bjorn Reese and Daniel Veillard.
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
// Author: breese@users.sourceforge.net

use std::{
    borrow::Cow,
    ffi::CStr,
    mem::{replace, take},
    ops::{Deref, DerefMut},
    ptr::{null, NonNull},
};

use anyhow::{anyhow, bail, ensure, Context};

use crate::{
    dict::XmlDictRef,
    libxml::{parser::xml_init_parser, xmlstring::xml_str_qequal},
};

pub use libxml_api::*;

const MAX_HASH_LEN: usize = 8;

fn xml_hash_compute_key<T>(
    table: &XmlHashTable<T>,
    name: Option<&Cow<'_, CStr>>,
    name2: Option<&Cow<'_, CStr>>,
    name3: Option<&Cow<'_, CStr>>,
) -> u64 {
    let mut value = table.seed as u64;
    if let Some(name) = name
        .filter(|name| !name.is_empty())
        .map(|name| name.as_ref().to_bytes())
    {
        value = value.wrapping_add(30 * name[0] as u64);
        for &ch in name {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch as u64);
        }
    }
    value ^= value.wrapping_shl(5).wrapping_add(value.wrapping_shr(3));
    if let Some(name) = name2
        .filter(|name| !name.is_empty())
        .map(|name| name.to_bytes())
    {
        for &ch in name {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch as u64);
        }
    }
    value ^= value.wrapping_shl(5).wrapping_add(value.wrapping_shr(3));
    if let Some(name) = name3
        .filter(|name| !name.is_empty())
        .map(|name| name.to_bytes())
    {
        for &ch in name {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch as u64);
        }
    }
    value % table.table.len() as u64
}

fn xml_hash_compute_qkey<T>(
    table: &XmlHashTable<T>,
    prefix: Option<&Cow<'_, CStr>>,
    name: &CStr,
    prefix2: Option<&Cow<'_, CStr>>,
    name2: Option<&Cow<'_, CStr>>,
    prefix3: Option<&Cow<'_, CStr>>,
    name3: Option<&Cow<'_, CStr>>,
) -> u64 {
    let mut value = table.seed as u64;
    if let Some(prefix) = prefix
        .filter(|prefix| !prefix.is_empty())
        .map(|prefix| prefix.as_ref().to_bytes())
    {
        value = value.wrapping_add(30 * prefix[0] as u64);
        for &ch in prefix {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch as u64);
        }
        value ^= value
            .wrapping_shl(5)
            .wrapping_add(value.wrapping_shr(3))
            .wrapping_add(b':' as u64);
    } else {
        value = value.wrapping_add(30 * name.to_bytes()[0] as u64);
    }

    for &ch in name.to_bytes() {
        value ^= value
            .wrapping_shl(5)
            .wrapping_add(value.wrapping_shr(3))
            .wrapping_add(ch as u64);
    }
    value ^= value.wrapping_shl(5).wrapping_add(value.wrapping_shr(3));
    if let Some(prefix) = prefix2
        .filter(|prefix| !prefix.is_empty())
        .map(|prefix| prefix.as_ref().to_bytes())
    {
        for &ch in prefix {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch as u64);
        }
        value ^= value
            .wrapping_shl(5)
            .wrapping_add(value.wrapping_shr(3))
            .wrapping_add(b':' as u64);
    }
    if let Some(name) = name2
        .filter(|name| !name.is_empty())
        .map(|name| name.as_ref().to_bytes())
    {
        for &ch in name {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch as u64);
        }
    }
    value ^= value.wrapping_shl(5).wrapping_add(value.wrapping_shr(3));
    if let Some(prefix) = prefix3
        .filter(|prefix| !prefix.is_empty())
        .map(|prefix| prefix.as_ref().to_bytes())
    {
        for &ch in prefix {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch as u64);
        }
        value ^= value
            .wrapping_shl(5)
            .wrapping_add(value.wrapping_shr(3))
            .wrapping_add(b':' as u64);
    }
    if let Some(name) = name3
        .filter(|name| !name.is_empty())
        .map(|name| name.as_ref().to_bytes())
    {
        for &ch in name {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch as u64);
        }
    }
    value % table.table.len() as u64
}

#[derive(Clone)]
struct XmlHashEntry<'a, T> {
    next: Option<XmlHashEntryRef<'a, T>>,
    name: Option<Cow<'a, CStr>>,
    name2: Option<Cow<'a, CStr>>,
    name3: Option<Cow<'a, CStr>>,
    payload: Option<T>,
}

impl<'a, T> Default for XmlHashEntry<'a, T> {
    fn default() -> Self {
        Self {
            next: None,
            name: None,
            name2: None,
            name3: None,
            payload: None,
        }
    }
}

struct XmlHashEntryRef<'a, T>(NonNull<XmlHashEntry<'a, T>>);

impl<'a, T> XmlHashEntryRef<'a, T> {
    fn new() -> Option<Self> {
        let default = XmlHashEntry::default();
        let boxed = Box::new(default);
        let leaked = Box::leak(boxed);
        NonNull::new(leaked).map(Self)
    }

    fn into_inner(self) -> XmlHashEntry<'a, T> {
        unsafe { *Box::from_raw(self.0.as_ptr()) }
    }
}

impl<'a, T> Clone for XmlHashEntryRef<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> Copy for XmlHashEntryRef<'a, T> {}

impl<'a, T> Deref for XmlHashEntryRef<'a, T> {
    type Target = XmlHashEntry<'a, T>;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<'a, T> DerefMut for XmlHashEntryRef<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

type Deallocator<T> = fn(T, Option<Cow<'_, CStr>>);

pub struct XmlHashTable<'a, T> {
    table: Box<[XmlHashEntry<'a, T>]>,
    num_elems: usize,
    dict: Option<XmlDictRef>,
    seed: i32,
}

impl<'a, T> XmlHashTable<'a, T> {
    /// Create new `XmlHashTable`.
    pub fn new() -> Self {
        Self::with_capacity(256)
    }

    /// Create new `XmlHashTable` with capacity.
    pub fn with_capacity(size: usize) -> Self {
        unsafe {
            xml_init_parser();
        }

        let table = (0..size)
            .map(|_| XmlHashEntry::default())
            .collect::<Box<[XmlHashEntry<T>]>>();
        Self {
            table,
            num_elems: 0,
            dict: None,
            seed: rand::random(),
        }
    }

    /// Create new `XmlHashTable` with a string dictionary `dict`.
    ///
    /// `dict` is used for cache of name strings.
    pub fn with_dict(dict: Option<XmlDictRef>) -> Self {
        let mut res = Self::new();
        if let Some(mut dict) = dict {
            dict.add_reference();
        }
        res.dict = dict;
        res
    }

    /// Create new `XmlHashTable` with capacity and a string dictionary `dict`.
    ///
    /// `dict` is used for cache of name strings.
    pub fn with_capacity_dict(size: usize, dict: Option<XmlDictRef>) -> Self {
        let mut res = Self::with_capacity(size);
        if let Some(mut dict) = dict {
            dict.add_reference();
        }
        res.dict = dict;
        res
    }

    fn grow(&mut self, mut size: usize) -> Result<(), anyhow::Error> {
        size = size.max(8);
        ensure!(size <= 8 * 2024, "`size` is too big.");

        let mut old_table = (0..size)
            .map(|_| XmlHashEntry::default())
            .collect::<Box<[XmlHashEntry<T>]>>();
        std::mem::swap(&mut old_table, &mut self.table);

        for entry in &mut old_table {
            if entry.payload.is_none() {
                continue;
            }

            let key = xml_hash_compute_key(
                self,
                entry.name.as_ref(),
                entry.name2.as_ref(),
                entry.name3.as_ref(),
            );
            let new = &mut self.table[key as usize];
            new.name = entry.name.take();
            new.name2 = entry.name2.take();
            new.name3 = entry.name3.take();
            new.next = None;
            new.payload = take(&mut entry.payload);
        }

        for mut entry in old_table {
            let mut iter = entry.next;
            while let Some(mut now) = iter {
                iter = now.next;

                let key = xml_hash_compute_key(
                    self,
                    now.name.as_ref(),
                    now.name2.as_ref(),
                    now.name3.as_ref(),
                );

                if self.table[key as usize].payload.is_none() {
                    self.table[key as usize] = now.into_inner();
                    self.table[key as usize].next = None;
                } else {
                    now.next = self.table[key as usize].next;
                    self.table[key as usize].next = Some(now);
                }
            }

            // Overwrite `next` with `None` just to prevent double-free.
            entry.next = None;
        }

        Ok(())
    }

    pub fn clear(&mut self) {
        self.clear_with(|_, _| {});
    }

    pub fn clear_with(&mut self, deallocator: impl Fn(T, Option<Cow<'_, CStr>>)) {
        let table = take(&mut self.table);
        for XmlHashEntry {
            mut next,
            name,
            payload,
            ..
        } in table
        {
            let Some(payload) = payload else {
                continue;
            };

            deallocator(payload, name);

            while let Some(now) = next {
                next = now.next;

                let XmlHashEntry { name, payload, .. } = now.into_inner();
                deallocator(payload.unwrap(), name);
            }
        }
        self.num_elems = 0;
    }

    fn do_update_entry(
        &mut self,
        name: &CStr,
        name2: Option<&CStr>,
        name3: Option<&CStr>,
        data: T,
        mut deallocator: impl FnMut(T, Option<Cow<'_, CStr>>),
    ) -> Result<(), anyhow::Error> {
        let (name, name2, name3) = if let Some(mut dict) = self.dict {
            unsafe {
                (
                    Some(Cow::Borrowed(CStr::from_ptr(
                        dict.lookup(name.to_bytes())?.as_ptr() as *const i8,
                    ))),
                    name2
                        .map(|name| dict.lookup(name.to_bytes()))
                        .transpose()?
                        .map(|ptr| Cow::Borrowed(CStr::from_ptr(ptr.as_ptr() as *const i8))),
                    name3
                        .map(|name| dict.lookup(name.to_bytes()))
                        .transpose()?
                        .map(|ptr| Cow::Borrowed(CStr::from_ptr(ptr.as_ptr() as *const i8))),
                )
            }
        } else {
            (
                Some(Cow::Owned(name.to_owned())),
                name2.map(|name| Cow::Owned(name.to_owned())),
                name3.map(|name| Cow::Owned(name.to_owned())),
            )
        };

        if self.is_empty() {
            self.grow(1);
        }

        let key =
            xml_hash_compute_key(self, name.as_ref(), name2.as_ref(), name3.as_ref()) as usize;

        let entry = if self.table[key].payload.is_some() {
            let mut entry = &mut self.table[key];
            loop {
                if entry.name == name && entry.name2 == name2 && entry.name3 == name3 {
                    let old = replace(&mut entry.payload, Some(data));
                    deallocator(old.unwrap(), name);
                    return Ok(());
                }
                let Some(next) = entry.next.as_mut() else {
                    break;
                };
                entry = next.deref_mut();
            }

            let mut new =
                XmlHashEntryRef::new().context("Failed to generate new XmlHashEntryRef.")?;
            new.next = self.table[key].next;
            self.table[key].next = Some(new);
            self.table[key].next.as_mut().unwrap().deref_mut()
        } else {
            self.table[key].next = None;
            &mut self.table[key]
        };

        entry.payload = Some(data);
        entry.name = name;
        entry.name2 = name2;
        entry.name3 = name3;

        self.num_elems += 1;

        Ok(())
    }

    /// Update a entry specified by `name` with `data`.  
    ///
    /// Even if update does not occur, returns `Ok` as long as the execution is successfully completed.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    /// table.add_entry(c"hoge", 1u32);
    /// table.add_entry2(c"foo", Some(c"bar"), 2);
    ///
    /// // Update occurs.
    /// let mut updated = false;
    /// assert!(table.update_entry(c"hoge", 2, |data, name| {
    ///     assert_eq!(data, 1);
    ///     assert_eq!(name.unwrap().as_ref(), c"hoge");
    ///     updated = true;
    /// }).is_ok());
    /// assert!(updated);
    /// table.update_entry(c"hoge", 3, |data, _| assert_eq!(data, 2));
    ///
    /// // Update does not occur, but execution is successfully completed.
    /// let mut updated = false;
    /// assert!(table.update_entry(c"foo", 3, |_, _| {
    ///     updated = true;
    /// }).is_ok());
    /// assert!(!updated);
    /// ```
    pub fn update_entry(
        &mut self,
        name: &CStr,
        data: T,
        deallocator: impl FnMut(T, Option<Cow<'_, CStr>>),
    ) -> Result<(), anyhow::Error> {
        self.do_update_entry(name, None, None, data, deallocator)
    }

    /// Update a entry specified by both `name` and `name2` with `data`.  
    ///
    /// Even if update does not occur, returns `Ok` as long as the execution is successfully completed.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    /// table.add_entry2(c"foo", Some(c"bar"), 2u32);
    /// table.add_entry3(c"hoge", Some(c"fuga"), Some(c"piyo"), 3);
    ///
    /// let mut updated = false;
    /// assert!(table.update_entry2(c"foo", Some(c"bar"), 3, |data, name| {
    ///     assert_eq!(data, 2);
    ///     assert_eq!(name.unwrap().as_ref(), c"foo");
    ///     updated = true;
    /// }).is_ok());
    /// assert!(updated);
    ///
    /// let mut updated = false;
    /// assert!(table.update_entry2(c"hoge", Some(c"fuga"), 4, |_, _| {
    ///     updated = true;
    /// }).is_ok());
    /// assert!(!updated);
    /// ```
    pub fn update_entry2(
        &mut self,
        name: &CStr,
        name2: Option<&CStr>,
        data: T,
        deallocator: impl FnMut(T, Option<Cow<'_, CStr>>),
    ) -> Result<(), anyhow::Error> {
        self.do_update_entry(name, name2, None, data, deallocator)
    }

    /// Update a entry specified by all of `name`, `name2` and `name3` with `data`.  
    ///
    /// Even if update does not occur, returns `Ok` as long as the execution is successfully completed.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    /// table.add_entry3(c"hoge", Some(c"fuga"), Some(c"piyo"), 3);
    ///
    /// let mut updated = false;
    /// assert!(table.update_entry3(c"hoge", Some(c"fuga"), Some(c"piyo"), 4, |data, name| {
    ///     assert_eq!(data, 3);
    ///     assert_eq!(name.unwrap().as_ref(), c"hoge");
    ///     updated = true;
    /// }).is_ok());
    /// assert!(updated);
    /// ```
    pub fn update_entry3(
        &mut self,
        name: &CStr,
        name2: Option<&CStr>,
        name3: Option<&CStr>,
        data: T,
        deallocator: impl FnMut(T, Option<Cow<'_, CStr>>),
    ) -> Result<(), anyhow::Error> {
        self.do_update_entry(name, name2, name3, data, deallocator)
    }

    fn do_add_entry(
        &mut self,
        name: Option<&CStr>,
        name2: Option<&CStr>,
        name3: Option<&CStr>,
        data: T,
    ) -> Result<(), anyhow::Error> {
        let (name, name2, name3) = if let Some(mut dict) = self.dict {
            unsafe {
                (
                    name.map(|name| dict.lookup(name.to_bytes()))
                        .transpose()?
                        .map(|ptr| Cow::Borrowed(CStr::from_ptr(ptr.as_ptr() as *const i8))),
                    name2
                        .map(|name| dict.lookup(name.to_bytes()))
                        .transpose()?
                        .map(|ptr| Cow::Borrowed(CStr::from_ptr(ptr.as_ptr() as *const i8))),
                    name3
                        .map(|name| dict.lookup(name.to_bytes()))
                        .transpose()?
                        .map(|ptr| Cow::Borrowed(CStr::from_ptr(ptr.as_ptr() as *const i8))),
                )
            }
        } else {
            (
                name.map(|name| Cow::Owned(name.to_owned())),
                name2.map(|name| Cow::Owned(name.to_owned())),
                name3.map(|name| Cow::Owned(name.to_owned())),
            )
        };

        if self.is_empty() {
            self.grow(1);
        }

        let key =
            xml_hash_compute_key(self, name.as_ref(), name2.as_ref(), name3.as_ref()) as usize;

        let mut chain_length = 0;
        let entry = if self.table[key].payload.is_some() {
            if self.table[key].name == name
                && self.table[key].name2 == name2
                && self.table[key].name3 == name3
            {
                bail!("Already contained in table")
            }

            let mut next = self.table[key].next;
            while let Some(now) = next {
                if now.name == name && now.name2 == name2 && now.name3 == name3 {
                    bail!("Already contained in table")
                }

                chain_length += 1;
                next = now.next;
            }

            let mut new =
                XmlHashEntryRef::new().context("Failed to generate new XmlHashEntryRef.")?;
            new.next = self.table[key].next;
            self.table[key].next = Some(new);
            self.table[key].next.as_mut().unwrap().deref_mut()
        } else {
            &mut self.table[key]
        };

        entry.payload = Some(data);
        entry.name = name;
        entry.name2 = name2;
        entry.name3 = name3;

        self.num_elems += 1;

        if chain_length > MAX_HASH_LEN {
            self.grow(MAX_HASH_LEN * self.table.len());
        }

        Ok(())
    }

    /// Add an entry specified `name` with `data`.
    ///
    /// If some entries that have same `name` already exists or some error occurs, return `Err`,  
    /// otherwise return `Ok`.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    ///
    /// assert!(table.add_entry(c"foo", 1u32).is_ok());
    /// // (c"foo", 1) already exists, so `table` returns `Err`.
    /// assert!(table.add_entry(c"foo", 2u32).is_err());
    ///
    /// assert_eq!(table.lookup(c"foo"), Some(&1));
    /// ```
    pub fn add_entry(&mut self, name: &CStr, data: T) -> Result<(), anyhow::Error> {
        self.do_add_entry(Some(name), None, None, data)
    }

    /// Add an entry specified both `name` and `name2` with `data`.
    ///
    /// If some entries that have the same pair of names already exists or some error occurs, return `Err`,  
    /// otherwise return `Ok`.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    ///
    /// assert!(table.add_entry2(c"foo", Some(c"bar"), 1u32).is_ok());
    /// // (c"foo", c"bar", 1) already exists, so `table` returns `Err`.
    /// assert!(table.add_entry2(c"foo", Some(c"bar"), 2u32).is_err());
    /// // c"foo" already exists, but the pair of (c"foo", c"hoge") does not yet exist.
    /// assert!(table.add_entry2(c"foo", Some(c"hoge"), 3u32).is_ok());
    ///
    /// assert_eq!(table.lookup2(c"foo", Some(c"bar")), Some(&1));
    /// ```
    pub fn add_entry2(
        &mut self,
        name: &CStr,
        name2: Option<&CStr>,
        data: T,
    ) -> Result<(), anyhow::Error> {
        self.do_add_entry(Some(name), name2, None, data)
    }

    /// Add an entry specified all of `name`, `name2` and `name3` with `data`.
    ///
    /// If some entries that have the same pair of names already exists or some error occurs, return `Err`,  
    /// otherwise return `Ok`.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    ///
    /// assert!(table.add_entry3(c"foo", Some(c"bar"), Some(c"fuga"), 1u32).is_ok());
    /// // (c"foo", c"bar", 1) already exists, so `table` returns `Err`.
    /// assert!(table.add_entry3(c"foo", Some(c"bar"), Some(c"fuga"), 2u32).is_err());
    /// // (c"foo", c"bar") already exists,
    /// // but the pair of (c"foo", c"bar", c"piyo") does not yet exist.
    /// assert!(table.add_entry3(c"foo", Some(c"bar"), Some(c"piyo"), 3u32).is_ok());
    ///
    /// assert_eq!(table.lookup3(c"foo", Some(c"bar"), Some(c"fuga")), Some(&1));
    /// ```
    pub fn add_entry3(
        &mut self,
        name: &CStr,
        name2: Option<&CStr>,
        name3: Option<&CStr>,
        data: T,
    ) -> Result<(), anyhow::Error> {
        self.do_add_entry(Some(name), name2, name3, data)
    }

    fn do_remove_entry(
        &mut self,
        name: &CStr,
        name2: Option<&CStr>,
        name3: Option<&CStr>,
        deallocator: impl Fn(T, Option<Cow<'_, CStr>>),
    ) -> Result<(), anyhow::Error> {
        let (name, name2, name3) = (
            Some(Cow::Borrowed(name)),
            name2.map(Cow::Borrowed),
            name3.map(Cow::Borrowed),
        );

        if !self.is_empty() {
            let key =
                xml_hash_compute_key(self, name.as_ref(), name2.as_ref(), name3.as_ref()) as usize;

            ensure!(self.table[key].payload.is_some(), "Entry is not found");

            if self.table[key].name == name
                && self.table[key].name2 == name2
                && self.table[key].name3 == name3
            {
                let (payload, name) = if let Some(next) = self.table[key].next.take() {
                    let next = next.into_inner();
                    let XmlHashEntry { name, payload, .. } = replace(&mut self.table[key], next);
                    (payload, name)
                } else {
                    (
                        take(&mut self.table[key].payload),
                        self.table[key].name.take(),
                    )
                };
                deallocator(payload.unwrap(), name);

                self.num_elems -= 1;
                return Ok(());
            }

            let mut prev = None::<XmlHashEntryRef<'_, T>>;
            let mut next = self.table[key].next;
            while let Some(now) = next {
                next = now.next;

                if now.name == name && now.name2 == name2 && now.name3 == name3 {
                    let XmlHashEntry { name, payload, .. } = now.into_inner();
                    if let Some(mut prev) = prev {
                        prev.next = next;
                    } else {
                        self.table[key].next = next;
                    }

                    deallocator(payload.unwrap(), name);

                    self.num_elems -= 1;
                    return Ok(());
                }

                prev = Some(now);
            }
        }

        Err(anyhow!("Entry is not found"))
    }

    /// Remove the entry specified by `name`.
    ///
    /// If such entry is found, deallocate the data with `deallocator` and return `Ok`,  
    /// otherwise return `Err`.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    ///
    /// table.add_entry(c"hoge", 1);
    /// table.add_entry2(c"hoge", Some(c"fuga"), 2);
    ///
    /// assert!(table.remove_entry(c"hoge", |data, _| assert_eq!(data, 1)).is_ok());
    /// // No entries match with c"hoge" are found.
    /// assert!(table.remove_entry(c"hoge", |_, _| {}).is_err());
    /// ```
    pub fn remove_entry(
        &mut self,
        name: &CStr,
        deallocator: impl Fn(T, Option<Cow<'_, CStr>>),
    ) -> Result<(), anyhow::Error> {
        self.do_remove_entry(name, None, None, deallocator)
    }

    /// Remove the entry specified by both `name` and `name2`.
    ///
    /// If such entry is found, deallocate the data with `deallocator` and return `Ok`,  
    /// otherwise return `Err`.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    ///
    /// table.add_entry2(c"hoge", Some(c"fuga"), 1);
    /// table.add_entry3(c"hoge", Some(c"fuga"), Some(c"piyo"), 2);
    ///
    /// assert!(table.remove_entry2(c"hoge", Some(c"fuga"), |data, _| assert_eq!(data, 1)).is_ok());
    /// // No entries match with (c"hoge", c"fuga") are found.
    /// assert!(table.remove_entry2(c"hoge", Some(c"fuga"), |_, _| {}).is_err());
    /// ```
    pub fn remove_entry2(
        &mut self,
        name: &CStr,
        name2: Option<&CStr>,
        deallocator: impl Fn(T, Option<Cow<'_, CStr>>),
    ) -> Result<(), anyhow::Error> {
        self.do_remove_entry(name, name2, None, deallocator)
    }

    /// Remove the entry specified by all of `name`, `name2` and `name3`.
    ///
    /// If such entry is found, deallocate the data with `deallocator` and return `Ok`,  
    /// otherwise return `Err`.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    ///
    /// table.add_entry3(c"hoge", Some(c"fuga"), Some(c"piyo"), 2);
    ///
    /// assert!(table.remove_entry3(c"hoge", Some(c"fuga"), Some(c"piyo"), |data, _| assert_eq!(data, 2)).is_ok());
    /// ```
    pub fn remove_entry3(
        &mut self,
        name: &CStr,
        name2: Option<&CStr>,
        name3: Option<&CStr>,
        deallocator: impl Fn(T, Option<Cow<'_, CStr>>),
    ) -> Result<(), anyhow::Error> {
        self.do_remove_entry(name, name2, name3, deallocator)
    }

    fn do_lookup(&self, name: &CStr, name2: Option<&CStr>, name3: Option<&CStr>) -> Option<&T> {
        let (name, name2, name3) = (
            Some(Cow::Borrowed(name)),
            name2.map(Cow::Borrowed),
            name3.map(Cow::Borrowed),
        );

        if !self.is_empty() {
            let key =
                xml_hash_compute_key(self, name.as_ref(), name2.as_ref(), name3.as_ref()) as usize;

            self.table[key].payload.as_ref()?;

            let mut now = &self.table[key];
            loop {
                if now.name == name && now.name2 == name2 && now.name3 == name3 {
                    return now.payload.as_ref();
                }
                now = now.next.as_ref()?.deref();
            }
        }
        None
    }

    /// Search the entry specified by `name`.  
    ///
    /// If such entry is found, return `Some`, otherwise return `None`.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    ///
    /// table.add_entry(c"hoge", 1u32);
    ///
    /// assert_eq!(table.lookup(c"hoge"), Some(&1));
    /// assert_eq!(table.lookup(c"fuga"), None);
    /// ```
    pub fn lookup(&self, name: &CStr) -> Option<&T> {
        self.do_lookup(name, None, None)
    }

    /// Search the entry specified by both `name` and `name2`.  
    ///
    /// If such entry is found, return `Some`, otherwise return `None`.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    ///
    /// table.add_entry2(c"hoge", Some(c"fuga"), 1u32);
    ///
    /// assert_eq!(table.lookup2(c"hoge", Some(c"fuga")), Some(&1));
    /// assert_eq!(table.lookup2(c"hoge", Some(c"piyo")), None);
    /// ```
    pub fn lookup2(&self, name: &CStr, name2: Option<&CStr>) -> Option<&T> {
        self.do_lookup(name, name2, None)
    }

    /// Search the entry specified by all of `name`, `name2` and `name3`.  
    ///
    /// If such entry is found, return `Some`, otherwise return `None`.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    ///
    /// table.add_entry3(c"hoge", Some(c"fuga"), Some(c"piyo"), 1u32);
    ///
    /// assert_eq!(table.lookup3(c"hoge", Some(c"fuga"), Some(c"piyo")), Some(&1));
    /// assert_eq!(table.lookup3(c"hoge", Some(c"fuga"), Some(c"bar")), None);
    /// ```
    pub fn lookup3(&self, name: &CStr, name2: Option<&CStr>, name3: Option<&CStr>) -> Option<&T> {
        self.do_lookup(name, name2, name3)
    }

    fn do_qlookup(
        &self,
        prefix: Option<&CStr>,
        name: &CStr,
        prefix2: Option<&CStr>,
        name2: Option<&CStr>,
        prefix3: Option<&CStr>,
        name3: Option<&CStr>,
    ) -> Option<&T> {
        if !self.is_empty() {
            let key = xml_hash_compute_qkey(
                self,
                prefix.as_ref().map(|&p| Cow::Borrowed(p)).as_ref(),
                name,
                prefix2.as_ref().map(|&p| Cow::Borrowed(p)).as_ref(),
                name2.as_ref().map(|&p| Cow::Borrowed(p)).as_ref(),
                prefix3.as_ref().map(|&p| Cow::Borrowed(p)).as_ref(),
                name3.as_ref().map(|&p| Cow::Borrowed(p)).as_ref(),
            ) as usize;

            self.table[key].payload.as_ref()?;

            unsafe {
                let (prefix, prefix2, prefix3) = (
                    prefix.map_or(null(), |p| p.as_ptr()) as *const u8,
                    prefix2.map_or(null(), |p| p.as_ptr()) as *const u8,
                    prefix3.map_or(null(), |p| p.as_ptr()) as *const u8,
                );
                let (name, name2, name3) = (
                    name.as_ptr() as *const u8,
                    name2.map_or(null(), |n| n.as_ptr()) as *const u8,
                    name3.map_or(null(), |n| n.as_ptr()) as *const u8,
                );

                let mut now = &self.table[key];
                loop {
                    if xml_str_qequal(
                        prefix,
                        name,
                        now.name.as_ref().map_or(null(), |n| n.as_ptr()) as _,
                    ) && xml_str_qequal(
                        prefix2,
                        name2,
                        now.name2.as_ref().map_or(null(), |n| n.as_ptr()) as _,
                    ) && xml_str_qequal(
                        prefix3,
                        name3,
                        now.name3.as_ref().map_or(null(), |n| n.as_ptr()) as _,
                    ) {
                        return now.payload.as_ref();
                    }
                    now = now.next.as_ref()?.deref();
                }
            }
        }
        None
    }

    /// Search the entry specified by the QName `prefix:name`.  
    ///
    /// If such entry is found, return `Some`, otherwise return `None`.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    ///
    /// table.add_entry(c"hoge:fuga", 1u32);
    ///
    /// // You can search QName by all of the following way.
    /// assert_eq!(table.qlookup(Some(c"hoge"), c"fuga"), Some(&1));
    /// assert_eq!(table.qlookup(None, c"hoge:fuga"), Some(&1));
    /// assert_eq!(table.lookup(c"hoge:fuga"), Some(&1));
    ///
    /// table.add_entry(c"hoge", 2);
    ///
    /// assert_eq!(table.qlookup(None, c"hoge"), Some(&2));
    /// ```
    pub fn qlookup(&self, prefix: Option<&CStr>, name: &CStr) -> Option<&T> {
        self.do_qlookup(prefix, name, None, None, None, None)
    }

    /// Search the entry specified by the given QNames.  
    ///
    /// If such entry is found, return `Some`, otherwise return `None`.
    ///
    /// Please refer to the example for `XmlHashTable::qlookup`.
    pub fn qlookup2(
        &self,
        prefix: Option<&CStr>,
        name: &CStr,
        prefix2: Option<&CStr>,
        name2: Option<&CStr>,
    ) -> Option<&T> {
        self.do_qlookup(prefix, name, prefix2, name2, None, None)
    }

    /// Search the entry specified by the given QNames.  
    ///
    /// If such entry is found, return `Some`, otherwise return `None`.
    ///
    /// Please refer to the example for `XmlHashTable::qlookup`.
    pub fn qlookup3(
        &self,
        prefix: Option<&CStr>,
        name: &CStr,
        prefix2: Option<&CStr>,
        name2: Option<&CStr>,
        prefix3: Option<&CStr>,
        name3: Option<&CStr>,
    ) -> Option<&T> {
        self.do_qlookup(prefix, name, prefix2, name2, prefix3, name3)
    }

    /// Clone the table. `copier` is used for copying each data of entries.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    /// table.add_entry(c"hoge", 1u32);
    /// table.add_entry(c"fuga", 2);
    /// table.add_entry(c"piyo", 3);
    ///
    /// let cloned = table.clone_with(|data, _| data.clone());
    ///
    /// let mut data = vec![];
    /// cloned.scan(|d, name, _, _| data.push((*d, name.unwrap().as_ref().to_owned())));
    /// data.sort();
    /// assert_eq!(
    ///     data,
    ///     vec![
    ///         (1u32, c"hoge".to_owned()),
    ///         (2, c"fuga".to_owned()),
    ///         (3, c"piyo".to_owned())
    ///     ]
    /// );
    /// ```
    pub fn clone_with(&self, mut copier: impl FnMut(&T, Option<&Cow<'a, CStr>>) -> T) -> Self {
        let mut res = Self::with_capacity(self.table.len());
        for mut entry in &self.table {
            if entry.payload.is_none() {
                continue;
            }

            loop {
                res.do_add_entry(
                    entry.name.as_ref().map(|n| n.as_ref()),
                    entry.name2.as_ref().map(|n| n.as_ref()),
                    entry.name3.as_ref().map(|n| n.as_ref()),
                    copier(entry.payload.as_ref().unwrap(), entry.name.as_ref()),
                );

                let Some(next) = entry.next.as_ref() else {
                    break;
                };
                entry = next.deref();
            }
        }

        assert_eq!(res.num_elems, self.num_elems);
        res
    }

    /// Return the number of entries owned by the table.
    pub fn len(&self) -> usize {
        self.num_elems
    }

    /// Check if the table has no entries.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Remove entries which `cond` returns `true` when its `data`, `name`, `name2` and `name3` are given.
    ///
    /// Clone the table. `copier` is used for copying each data of entries.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    /// table.add_entry(c"hoge", 1u32);
    /// table.add_entry(c"fuga", 2);
    /// table.add_entry(c"piyo", 3);
    /// table.add_entry(c"foo", 4);
    /// assert_eq!(table.len(), 4);
    ///
    /// table.remove_if(|data, _, _, _| data % 2 == 0, |_, _| {});
    /// assert_eq!(table.len(), 2);
    ///
    /// assert!(table.lookup(c"hoge").is_some());
    /// assert!(table.lookup(c"fuga").is_none());
    /// assert!(table.lookup(c"piyo").is_some());
    /// assert!(table.lookup(c"foo").is_none());
    /// ```
    ///
    /// # Note
    /// In original libxml2, `xmlHashScan/ScanFull` allows a callback to modify the table being scanned,
    /// but obviously this is not safe.   
    /// (Deletions would work fine, but additions and updates may not work well once the table rebuild runs.)  
    /// Therefore, no changes are allowed in the scan method.
    pub fn remove_if(
        &mut self,
        mut cond: impl FnMut(
            &T,
            Option<&Cow<'_, CStr>>,
            Option<&Cow<'_, CStr>>,
            Option<&Cow<'_, CStr>>,
        ) -> bool,
        mut deallocator: impl FnMut(T, Option<Cow<'a, CStr>>),
    ) {
        for entry in &mut self.table {
            while entry.payload.is_some()
                && cond(
                    entry.payload.as_ref().unwrap(),
                    entry.name.as_ref(),
                    entry.name2.as_ref(),
                    entry.name3.as_ref(),
                )
            {
                let (payload, name) = if let Some(next) = entry.next.take() {
                    let old = replace(entry, next.into_inner());
                    (old.payload, old.name)
                } else {
                    (take(&mut entry.payload), entry.name.take())
                };
                deallocator(payload.unwrap(), name);
                self.num_elems -= 1;
            }

            if entry.payload.is_some() {
                let mut prev = None::<XmlHashEntryRef<'_, T>>;
                let mut next = entry.next;
                while let Some(now) = next {
                    next = now.next;

                    if cond(
                        now.payload.as_ref().unwrap(),
                        now.name.as_ref(),
                        now.name2.as_ref(),
                        now.name3.as_ref(),
                    ) {
                        let XmlHashEntry {
                            next,
                            name,
                            payload,
                            ..
                        } = now.into_inner();
                        if let Some(mut prev) = prev {
                            prev.next = next;
                        } else {
                            entry.next = next;
                        }
                        deallocator(payload.unwrap(), name);
                        self.num_elems -= 1;
                    } else {
                        prev = Some(now);
                    }
                }
            }
        }
    }

    /// Apply callback `f` to all entries of the table.
    ///
    /// # Note
    /// In original libxml2, xmlHashScan/ScanFull allows to modify the table inner callback,
    /// but it is obviously not safe.  
    /// Therefore, this method allow callback to modify the table.
    ///
    /// # Examples
    /// ```rust
    /// use exml::hash::XmlHashTable;
    ///
    /// let mut table = XmlHashTable::new();
    /// table.add_entry(c"foo", 1u32);
    /// table.add_entry2(c"hoge", Some(c"fuga"), 2);
    /// table.add_entry3(c"foo", Some(c"bar"), Some(c"piyo"), 3);
    ///
    /// let mut entries = vec![];
    /// table.scan(|data, s1, s2, s3| entries.push(
    ///     (
    ///         *data,
    ///         s1.map_or(c"".to_owned(), |s| s.clone().into()),
    ///         s2.map_or(c"".to_owned(), |s| s.clone().into()),
    ///         s3.map_or(c"".to_owned(), |s| s.clone().into()),
    ///     )
    /// ));
    /// entries.sort();
    /// assert_eq!(
    ///     entries,
    ///     vec![
    ///         (1u32, c"foo".to_owned(), c"".to_owned(), c"".to_owned()),
    ///         (2u32, c"hoge".to_owned(), c"fuga".to_owned(), c"".to_owned()),
    ///         (3u32, c"foo".to_owned(), c"bar".to_owned(), c"piyo".to_owned()),
    ///     ],
    /// )
    /// ```
    pub fn scan(
        &self,
        mut f: impl FnMut(&T, Option<&Cow<'_, CStr>>, Option<&Cow<'_, CStr>>, Option<&Cow<'_, CStr>>),
    ) {
        let num_elems = self.num_elems;
        for mut entry in &self.table {
            if entry.payload.is_none() {
                continue;
            }

            loop {
                f(
                    entry.payload.as_ref().unwrap(),
                    entry.name.as_ref(),
                    entry.name2.as_ref(),
                    entry.name3.as_ref(),
                );

                let Some(next) = entry.next.as_ref() else {
                    break;
                };
                entry = next.deref();
            }
        }
        // In original libxml2, some entries may be removed by callback,
        // but this method does not allowed such operations.
        // Therefore, we should check that such operations have not been applied.
        assert_eq!(self.num_elems, num_elems);
    }

    pub fn drain(&'a mut self) -> Drain<'a, T> {
        Drain::new(self)
    }
}

impl<'a, T> Default for XmlHashTable<'a, T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, T> Drop for XmlHashTable<'a, T> {
    fn drop(&mut self) {
        self.clear();
        if let Some(dict) = self.dict {
            dict.free();
            self.dict = None;
        }
    }
}

pub struct XmlHashTableRef<'a, T>(NonNull<XmlHashTable<'a, T>>);

impl<'a, T> XmlHashTableRef<'a, T> {
    pub fn new() -> Option<Self> {
        let table = XmlHashTable::new();
        Self::from_table(table)
    }

    pub fn with_capacity(size: usize) -> Option<Self> {
        let table = XmlHashTable::with_capacity(size);
        Self::from_table(table)
    }

    pub fn from_table(table: XmlHashTable<'a, T>) -> Option<Self> {
        let boxed = Box::new(table);
        let leaked = Box::leak(boxed);
        NonNull::new(leaked).map(Self)
    }

    pub(crate) fn from_raw(ptr: *mut XmlHashTable<'a, T>) -> Option<Self> {
        NonNull::new(ptr).map(Self)
    }

    pub fn as_ptr<'b>(self) -> *mut XmlHashTable<'b, T>
    where
        'a: 'b,
    {
        self.0.as_ptr()
    }

    pub fn into_inner(self) -> XmlHashTable<'a, T> {
        unsafe { *Box::from_raw(self.as_ptr()) }
    }

    pub fn free(self) {
        let mut table = self.into_inner();
        table.clear();
    }
}

impl<'a, T> Clone for XmlHashTableRef<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> Copy for XmlHashTableRef<'a, T> {}

impl<'a, T> Deref for XmlHashTableRef<'a, T> {
    type Target = XmlHashTable<'a, T>;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<'a, T> DerefMut for XmlHashTableRef<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

pub struct Drain<'a, T> {
    table: &'a mut XmlHashTable<'a, T>,
    list: Option<XmlHashEntryRef<'a, T>>,
    next_index: usize,
}

impl<'a, T> Drain<'a, T> {
    fn new(table: &'a mut XmlHashTable<'a, T>) -> Self {
        Self {
            table,
            list: None,
            next_index: 0,
        }
    }
}

impl<'a, T> Iterator for Drain<'a, T> {
    type Item = (
        T,
        Option<Cow<'a, CStr>>,
        Option<Cow<'a, CStr>>,
        Option<Cow<'a, CStr>>,
    );

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(list) = self.list {
            self.list = list.next;

            let XmlHashEntry {
                name,
                name2,
                name3,
                payload,
                ..
            } = list.into_inner();
            return Some((payload.unwrap(), name, name2, name3));
        }

        while self.next_index < self.table.table.len() {
            if self.table.table[self.next_index].payload.is_some() {
                let XmlHashEntry {
                    next,
                    name,
                    name2,
                    name3,
                    payload,
                    ..
                } = take(&mut self.table.table[self.next_index]);

                self.list = next;
                self.next_index += 1;

                return Some((payload.unwrap(), name, name2, name3));
            }
            self.next_index += 1;
        }

        None
    }
}

pub mod libxml_api {
    use std::{
        borrow::Cow,
        ffi::{c_void, CStr},
        ptr::{null, null_mut},
    };

    use crate::{
        dict::{XmlDictPtr, XmlDictRef},
        libxml::{globals::xml_free, xmlstring::XmlChar},
    };

    pub use super::XmlHashTable;
    use super::XmlHashTableRef;

    /// A wrapper of `*mut std::ffi::c_void`.
    ///
    /// `XmlHashTable<T>` requires `T` implements `Default`, but `*mut std::ffi::c_void` does not.  
    /// `CVoidWrapper` provides `Default` implementation that returns `null_mut()` for `*mut std::ffi::c_void`.
    #[derive(Debug, Clone, Copy)]
    pub struct CVoidWrapper(pub *mut c_void);

    impl Default for CVoidWrapper {
        fn default() -> Self {
            Self(null_mut())
        }
    }

    pub type XmlHashTablePtr = *mut XmlHashTable<'static, CVoidWrapper>;

    pub type XmlHashDeallocator = extern "C" fn(payload: *mut c_void, name: *const XmlChar);
    pub type XmlHashCopier =
        extern "C" fn(payload: *mut c_void, name: *const XmlChar) -> *mut c_void;
    pub type XmlHashScanner =
        extern "C" fn(payload: *mut c_void, data: *mut c_void, name: *const XmlChar);
    pub type XmlHashScannerFull = extern "C" fn(
        payload: *mut c_void,
        data: *mut c_void,
        name: *const XmlChar,
        name2: *const XmlChar,
        name3: *const XmlChar,
    );

    pub extern "C" fn xml_hash_create(mut size: i32) -> XmlHashTablePtr {
        if size <= 0 {
            size = 256;
        }
        XmlHashTableRef::<'static, CVoidWrapper>::with_capacity(size as usize)
            .map(|table| table.as_ptr())
            .unwrap_or(null_mut())
    }

    pub extern "C" fn xml_hash_create_dict(mut size: i32, dict: XmlDictPtr) -> XmlHashTablePtr {
        if size <= 0 {
            size = 256;
        }
        let dict = XmlDictRef::from_raw(dict);
        let table = XmlHashTable::with_capacity_dict(size as usize, dict);
        XmlHashTableRef::from_table(table)
            .map(|table| table.as_ptr())
            .unwrap_or(null_mut())
    }

    pub extern "C" fn xml_hash_free(table: XmlHashTablePtr, f: Option<XmlHashDeallocator>) {
        if let Some(table) = XmlHashTableRef::from_raw(table) {
            let mut table = table.into_inner();
            table.clear_with(move |payload, name| {
                if let Some(f) = f {
                    f(
                        payload.0,
                        name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                    );
                }
            });
        }
    }

    /// # Safety
    /// - This method is equivalent to `xml_free(entry)`, so it has the same constraint of `xml_free`.
    pub extern "C" fn xml_hash_default_deallocator(entry: *mut c_void, _name: *const XmlChar) {
        unsafe {
            xml_free(entry as _);
        }
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_add_entry(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        userdata: *mut c_void,
    ) -> i32 {
        xml_hash_add_entry3(table, name, null(), null(), userdata)
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_add_entry2(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        name2: *const XmlChar,
        userdata: *mut c_void,
    ) -> i32 {
        xml_hash_add_entry3(table, name, name2, null(), userdata)
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_add_entry3(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        name2: *const XmlChar,
        name3: *const XmlChar,
        userdata: *mut c_void,
    ) -> i32 {
        if name.is_null() {
            return -1;
        }
        let Some(mut table) = XmlHashTableRef::from_raw(table) else {
            return -1;
        };

        match table.do_add_entry(
            Some(CStr::from_ptr(name as *const i8)),
            (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
            (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
            CVoidWrapper(userdata),
        ) {
            Ok(_) => 0,
            Err(_) => -1,
        }
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_update_entry(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        userdata: *mut c_void,
        f: Option<XmlHashDeallocator>,
    ) -> i32 {
        xml_hash_update_entry3(table, name, null(), null(), userdata, f)
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_update_entry2(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        name2: *const XmlChar,
        userdata: *mut c_void,
        f: Option<XmlHashDeallocator>,
    ) -> i32 {
        xml_hash_update_entry3(table, name, name2, null(), userdata, f)
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_update_entry3(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        name2: *const XmlChar,
        name3: *const XmlChar,
        userdata: *mut c_void,
        f: Option<XmlHashDeallocator>,
    ) -> i32 {
        if name.is_null() {
            return -1;
        }
        let Some(mut table) = XmlHashTableRef::from_raw(table) else {
            return -1;
        };

        match table.do_update_entry(
            CStr::from_ptr(name as *const i8),
            (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
            (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
            CVoidWrapper(userdata),
            move |payload: CVoidWrapper, name: Option<Cow<'_, CStr>>| {
                if let Some(deallocator) = f {
                    deallocator(
                        payload.0,
                        name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                    );
                }
            },
        ) {
            Ok(_) => 0,
            Err(_) => -1,
        }
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_remove_entry(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        f: Option<XmlHashDeallocator>,
    ) -> i32 {
        xml_hash_remove_entry3(table, name, null(), null(), f)
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_remove_entry2(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        name2: *const XmlChar,
        f: Option<XmlHashDeallocator>,
    ) -> i32 {
        xml_hash_remove_entry3(table, name, name2, null(), f)
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_remove_entry3(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        name2: *const XmlChar,
        name3: *const XmlChar,
        f: Option<XmlHashDeallocator>,
    ) -> i32 {
        if name.is_null() {
            return -1;
        }
        let Some(mut table) = XmlHashTableRef::from_raw(table) else {
            return -1;
        };

        match table.do_remove_entry(
            CStr::from_ptr(name as *const i8),
            (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
            (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
            move |payload: CVoidWrapper, name: Option<Cow<'_, CStr>>| {
                if let Some(deallocator) = f {
                    deallocator(
                        payload.0,
                        name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                    );
                }
            },
        ) {
            Ok(_) => 0,
            Err(_) => -1,
        }
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_lookup(
        table: XmlHashTablePtr,
        name: *const XmlChar,
    ) -> *mut c_void {
        xml_hash_lookup3(table, name, null(), null())
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_lookup2(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        name2: *const XmlChar,
    ) -> *mut c_void {
        xml_hash_lookup3(table, name, name2, null())
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_lookup3(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        name2: *const XmlChar,
        name3: *const XmlChar,
    ) -> *mut c_void {
        if name.is_null() {
            return null_mut();
        }
        let Some(table) = XmlHashTableRef::from_raw(table) else {
            return null_mut();
        };

        table
            .do_lookup(
                CStr::from_ptr(name as *const i8),
                (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
                (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
            )
            .map(|ptr| ptr.0)
            .unwrap_or(null_mut())
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_qlookup(
        table: XmlHashTablePtr,
        prefix: *const XmlChar,
        name: *const XmlChar,
    ) -> *mut c_void {
        xml_hash_qlookup3(table, prefix, name, null(), null(), null(), null())
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_qlookup2(
        table: XmlHashTablePtr,
        prefix: *const XmlChar,
        name: *const XmlChar,
        prefix2: *const XmlChar,
        name2: *const XmlChar,
    ) -> *mut c_void {
        xml_hash_qlookup3(table, prefix, name, prefix2, name2, null(), null())
    }

    /// # Safety
    /// - `name` must be a valid NULL terminated string.
    pub unsafe extern "C" fn xml_hash_qlookup3(
        table: XmlHashTablePtr,
        prefix: *const XmlChar,
        name: *const XmlChar,
        prefix2: *const XmlChar,
        name2: *const XmlChar,
        prefix3: *const XmlChar,
        name3: *const XmlChar,
    ) -> *mut c_void {
        if name.is_null() {
            return null_mut();
        }
        let Some(table) = XmlHashTableRef::from_raw(table) else {
            return null_mut();
        };

        table
            .do_qlookup(
                (!prefix.is_null()).then(|| CStr::from_ptr(prefix as *const i8)),
                CStr::from_ptr(name as *const i8),
                (!prefix2.is_null()).then(|| CStr::from_ptr(prefix2 as *const i8)),
                (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
                (!prefix3.is_null()).then(|| CStr::from_ptr(prefix3 as *const i8)),
                (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
            )
            .map(|ptr| ptr.0)
            .unwrap_or(null_mut())
    }

    pub extern "C" fn xml_hash_copy(
        table: XmlHashTablePtr,
        f: Option<XmlHashCopier>,
    ) -> XmlHashTablePtr {
        let Some(table) = XmlHashTableRef::from_raw(table) else {
            return null_mut();
        };

        let Some(copier) = f else {
            return null_mut();
        };

        let cloned = table.clone_with(|payload, name| {
            CVoidWrapper(copier(
                payload.0,
                name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
            ))
        });
        XmlHashTableRef::from_table(cloned)
            .map(|table| table.as_ptr())
            .unwrap_or(null_mut())
    }

    pub extern "C" fn xml_hash_size(table: XmlHashTablePtr) -> i32 {
        XmlHashTableRef::from_raw(table)
            .map(|table| table.len() as i32)
            .unwrap_or(-1)
    }

    /// # Safety
    /// - The behavior of `f` when it makes changes to `table` is <strong>undefined</strong>.
    pub unsafe extern "C" fn xml_hash_scan(
        table: XmlHashTablePtr,
        f: Option<XmlHashScanner>,
        data: *mut c_void,
    ) {
        let Some(table) = XmlHashTableRef::from_raw(table) else {
            return;
        };

        let Some(scanner) = f else {
            return;
        };

        table.scan(move |payload, name, _, _| {
            if !payload.0.is_null() {
                scanner(
                    payload.0,
                    data,
                    name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                );
            }
        });
    }

    /// # Safety
    /// - The behavior of `f` when it makes changes to `table` is <strong>undefined</strong>.  
    /// - All of `name`, `name2` and `name3` must be valid NULL terminated strings.
    pub unsafe extern "C" fn xml_hash_scan3(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        name2: *const XmlChar,
        name3: *const XmlChar,
        f: Option<XmlHashScanner>,
        data: *mut c_void,
    ) {
        let Some(table) = XmlHashTableRef::from_raw(table) else {
            return;
        };

        let Some(scanner) = f else {
            return;
        };

        let (name, name2, name3) = (
            (!name.is_null()).then(|| CStr::from_ptr(name as *const i8)),
            (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
            (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
        );

        table.scan(move |payload, n, n2, n3| {
            if (name.is_none() || n.map(|n| n.as_ref()) == name)
                && (name2.is_none() || n2.map(|n| n.as_ref()) == name2)
                && (name3.is_none() || n3.map(|n| n.as_ref()) == name3)
                && !payload.0.is_null()
            {
                scanner(
                    payload.0,
                    data,
                    n.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                );
            }
        });
    }

    /// # Safety
    /// - The behavior of `f` when it makes changes to `table` is <strong>undefined</strong>.
    pub unsafe extern "C" fn xml_hash_scan_full(
        table: XmlHashTablePtr,
        f: Option<XmlHashScannerFull>,
        data: *mut c_void,
    ) {
        let Some(table) = XmlHashTableRef::from_raw(table) else {
            return;
        };

        let Some(scanner) = f else {
            return;
        };

        table.scan(move |payload, name, name2, name3| {
            if !payload.0.is_null() {
                scanner(
                    payload.0,
                    data,
                    name.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                    name2.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                    name3.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                );
            }
        });
    }

    /// # Safety
    /// - The behavior of `f` when it makes changes to `table` is <strong>undefined</strong>.  
    /// - All of `name`, `name2` and `name3` must be valid NULL terminated strings.
    pub unsafe extern "C" fn xml_hash_scan_full3(
        table: XmlHashTablePtr,
        name: *const XmlChar,
        name2: *const XmlChar,
        name3: *const XmlChar,
        f: Option<XmlHashScannerFull>,
        data: *mut c_void,
    ) {
        let Some(table) = XmlHashTableRef::from_raw(table) else {
            return;
        };

        let Some(scanner) = f else {
            return;
        };

        let (name, name2, name3) = (
            (!name.is_null()).then(|| CStr::from_ptr(name as *const i8)),
            (!name2.is_null()).then(|| CStr::from_ptr(name2 as *const i8)),
            (!name3.is_null()).then(|| CStr::from_ptr(name3 as *const i8)),
        );

        table.scan(move |payload, n, n2, n3| {
            if (name.is_none() || n.map(|n| n.as_ref()) == name)
                && (name2.is_none() || n2.map(|n| n.as_ref()) == name2)
                && (name3.is_none() || n3.map(|n| n.as_ref()) == name3)
                && !payload.0.is_null()
            {
                scanner(
                    payload.0,
                    data,
                    n.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                    n2.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                    n3.map(|n| n.as_ptr()).unwrap_or(null()) as *const u8,
                );
            }
        });
    }

    #[cfg(test)]
    mod tests {
        use std::{ffi::CString, num::NonZeroU8};

        use rand::{
            distributions::Alphanumeric, prelude::Distribution, seq::SliceRandom, thread_rng, Rng,
        };

        use super::*;

        const LEN: usize = 30;

        fn gen_chars(rng: &mut impl Rng) -> Vec<NonZeroU8> {
            Alphanumeric
                .sample_iter(rng)
                .map(|c| NonZeroU8::new(c).unwrap())
                .take(LEN)
                .collect::<Vec<_>>()
        }

        fn gen_ncname(rng: &mut impl Rng) -> CString {
            CString::from(gen_chars(rng))
        }

        fn gen_qname(rng: &mut impl Rng) -> CString {
            let mut chars = gen_chars(rng);
            chars[rng.gen_range(1..29)] = NonZeroU8::new(b':').unwrap();
            CString::from(chars)
        }

        unsafe fn ncname_insertion_test(
            table: XmlHashTablePtr,
            entries: &mut Vec<(CString, Option<CString>, Option<CString>, u64)>,
            rng: &mut impl Rng,
        ) {
            match rng.gen_range(1..=3) {
                1 => {
                    let n1 = gen_ncname(rng);
                    let data = rng.gen::<u64>();
                    xml_hash_add_entry(table, n1.as_ptr() as _, data as _);
                    assert_eq!(xml_hash_lookup(table, n1.as_ptr() as _) as u64, data);
                    entries.push((n1, None, None, data));
                }
                2 => {
                    let n1 = gen_ncname(rng);
                    let n2 = gen_ncname(rng);
                    let data = rng.gen::<u64>();
                    xml_hash_add_entry2(table, n1.as_ptr() as _, n2.as_ptr() as _, data as _);
                    assert_eq!(
                        xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _) as u64,
                        data
                    );
                    entries.push((n1, Some(n2), None, data));
                }
                3 => {
                    let n1 = gen_ncname(rng);
                    let n2 = gen_ncname(rng);
                    let n3 = gen_ncname(rng);
                    let data = rng.gen::<u64>();
                    xml_hash_add_entry3(
                        table,
                        n1.as_ptr() as _,
                        n2.as_ptr() as _,
                        n3.as_ptr() as _,
                        data as _,
                    );
                    assert_eq!(
                        xml_hash_lookup3(
                            table,
                            n1.as_ptr() as _,
                            n2.as_ptr() as _,
                            n3.as_ptr() as _
                        ) as u64,
                        data
                    );
                    entries.push((n1, Some(n2), Some(n3), data));
                }
                _ => {}
            }
        }

        fn split_qname(qname: &CStr) -> Option<(CString, CString)> {
            let qname = qname.to_string_lossy();
            let (prefix, local_name) = qname.split_once(':')?;
            Some((
                CString::new(prefix).unwrap(),
                CString::new(local_name).unwrap(),
            ))
        }

        unsafe fn qname_insertion_test(
            table: XmlHashTablePtr,
            entries: &mut Vec<(CString, Option<CString>, Option<CString>, u64)>,
            rng: &mut impl Rng,
        ) {
            match rng.gen_range(1..=3) {
                1 => {
                    let n1 = gen_qname(rng);
                    let data = rng.gen::<u64>();
                    xml_hash_add_entry(table, n1.as_ptr() as _, data as _);
                    assert_eq!(xml_hash_lookup(table, n1.as_ptr() as _) as u64, data);
                    let (p1, l1) = split_qname(&n1).unwrap();
                    assert_eq!(
                        xml_hash_qlookup(table, p1.as_ptr() as _, l1.as_ptr() as _,) as u64,
                        data
                    );
                    entries.push((n1, None, None, data));
                }
                2 => {
                    let n1 = gen_qname(rng);
                    let n2 = gen_qname(rng);
                    let data = rng.gen::<u64>();
                    xml_hash_add_entry2(table, n1.as_ptr() as _, n2.as_ptr() as _, data as _);
                    assert_eq!(
                        xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _) as u64,
                        data
                    );
                    let (p1, l1) = split_qname(&n1).unwrap();
                    let (p2, l2) = split_qname(&n2).unwrap();
                    assert_eq!(
                        xml_hash_qlookup2(
                            table,
                            p1.as_ptr() as _,
                            l1.as_ptr() as _,
                            p2.as_ptr() as _,
                            l2.as_ptr() as _,
                        ) as u64,
                        data
                    );
                    entries.push((n1, Some(n2), None, data));
                }
                3 => {
                    let n1 = gen_qname(rng);
                    let n2 = gen_qname(rng);
                    let n3 = gen_qname(rng);
                    let data = rng.gen::<u64>();
                    xml_hash_add_entry3(
                        table,
                        n1.as_ptr() as _,
                        n2.as_ptr() as _,
                        n3.as_ptr() as _,
                        data as _,
                    );
                    assert_eq!(
                        xml_hash_lookup3(
                            table,
                            n1.as_ptr() as _,
                            n2.as_ptr() as _,
                            n3.as_ptr() as _
                        ) as u64,
                        data
                    );
                    let (p1, l1) = split_qname(&n1).unwrap();
                    let (p2, l2) = split_qname(&n2).unwrap();
                    let (p3, l3) = split_qname(&n3).unwrap();
                    assert_eq!(
                        xml_hash_qlookup3(
                            table,
                            p1.as_ptr() as _,
                            l1.as_ptr() as _,
                            p2.as_ptr() as _,
                            l2.as_ptr() as _,
                            p3.as_ptr() as _,
                            l3.as_ptr() as _,
                        ) as u64,
                        data
                    );
                    entries.push((n1, Some(n2), Some(n3), data));
                }
                _ => {}
            }
        }

        #[test]
        fn random_test() {
            let table = xml_hash_create(0);
            let mut rng = thread_rng();
            let mut entries = vec![];

            unsafe {
                for _ in 0..10_000 {
                    if entries.is_empty() {
                        assert!(xml_hash_size(table) == 0);
                        [ncname_insertion_test, qname_insertion_test]
                            .choose(&mut rng)
                            .unwrap()(table, &mut entries, &mut rng);
                    }
                    // 0: insert (ncname)
                    // 1: insert (qname)
                    // 2: remove
                    // 3: lookup
                    // 4: update
                    // 5: clone
                    // 6: remove_if
                    let query = rng.gen_range(0..=6);
                    match query {
                        0 => ncname_insertion_test(table, &mut entries, &mut rng),
                        1 => qname_insertion_test(table, &mut entries, &mut rng),
                        2 => match entries.swap_remove(rng.gen_range(0..entries.len())) {
                            (n1, None, None, data) => {
                                assert_eq!(xml_hash_lookup(table, n1.as_ptr() as _) as u64, data);
                                assert!(xml_hash_remove_entry(table, n1.as_ptr() as _, None) == 0);
                                assert!(xml_hash_lookup(table, n1.as_ptr() as _).is_null());
                            }
                            (n1, Some(n2), None, data) => {
                                assert_eq!(
                                    xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _)
                                        as u64,
                                    data
                                );
                                assert!(
                                    xml_hash_remove_entry2(
                                        table,
                                        n1.as_ptr() as _,
                                        n2.as_ptr() as _,
                                        None
                                    ) == 0
                                );
                                assert!(xml_hash_lookup2(
                                    table,
                                    n1.as_ptr() as _,
                                    n2.as_ptr() as _
                                )
                                .is_null());
                            }
                            (n1, Some(n2), Some(n3), data) => {
                                assert_eq!(
                                    xml_hash_lookup3(
                                        table,
                                        n1.as_ptr() as _,
                                        n2.as_ptr() as _,
                                        n3.as_ptr() as _
                                    ) as u64,
                                    data
                                );
                                assert!(
                                    xml_hash_remove_entry3(
                                        table,
                                        n1.as_ptr() as _,
                                        n2.as_ptr() as _,
                                        n3.as_ptr() as _,
                                        None
                                    ) == 0
                                );
                                assert!(xml_hash_lookup3(
                                    table,
                                    n1.as_ptr() as _,
                                    n2.as_ptr() as _,
                                    n3.as_ptr() as _
                                )
                                .is_null());
                            }
                            _ => [ncname_insertion_test, qname_insertion_test]
                                .choose(&mut rng)
                                .unwrap()(
                                table, &mut entries, &mut rng
                            ),
                        },
                        3 => match entries.choose(&mut rng) {
                            Some((n1, None, None, data)) => {
                                assert_eq!(xml_hash_lookup(table, n1.as_ptr() as _) as u64, *data);
                                match split_qname(n1) {
                                    Some((p1, l1)) => {
                                        assert_eq!(
                                            xml_hash_qlookup(
                                                table,
                                                p1.as_ptr() as _,
                                                l1.as_ptr() as _,
                                            ) as u64,
                                            *data
                                        );
                                    }
                                    _ => assert_eq!(
                                        xml_hash_qlookup(table, null(), n1.as_ptr() as _,) as u64,
                                        *data
                                    ),
                                }
                            }
                            Some((n1, Some(n2), None, data)) => {
                                assert_eq!(
                                    xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _)
                                        as u64,
                                    *data
                                );
                                match (split_qname(n1), split_qname(n2)) {
                                    (Some((p1, l1)), Some((p2, l2))) => {
                                        assert_eq!(
                                            xml_hash_qlookup2(
                                                table,
                                                p1.as_ptr() as _,
                                                l1.as_ptr() as _,
                                                p2.as_ptr() as _,
                                                l2.as_ptr() as _,
                                            ) as u64,
                                            *data
                                        )
                                    }
                                    _ => assert_eq!(
                                        xml_hash_qlookup2(
                                            table,
                                            null(),
                                            n1.as_ptr() as _,
                                            null(),
                                            n2.as_ptr() as _,
                                        ) as u64,
                                        *data
                                    ),
                                }
                            }
                            Some((n1, Some(n2), Some(n3), data)) => {
                                assert_eq!(
                                    xml_hash_lookup3(
                                        table,
                                        n1.as_ptr() as _,
                                        n2.as_ptr() as _,
                                        n3.as_ptr() as _
                                    ) as u64,
                                    *data
                                );
                                match (split_qname(n1), split_qname(n2), split_qname(n3)) {
                                    (Some((p1, l1)), Some((p2, l2)), Some((p3, l3))) => {
                                        assert_eq!(
                                            xml_hash_qlookup3(
                                                table,
                                                p1.as_ptr() as _,
                                                l1.as_ptr() as _,
                                                p2.as_ptr() as _,
                                                l2.as_ptr() as _,
                                                p3.as_ptr() as _,
                                                l3.as_ptr() as _,
                                            ) as u64,
                                            *data
                                        );
                                    }
                                    _ => {
                                        assert_eq!(
                                            xml_hash_qlookup3(
                                                table,
                                                null(),
                                                n1.as_ptr() as _,
                                                null(),
                                                n2.as_ptr() as _,
                                                null(),
                                                n3.as_ptr() as _,
                                            ) as u64,
                                            *data
                                        )
                                    }
                                }
                            }
                            _ => [ncname_insertion_test, qname_insertion_test]
                                .choose(&mut rng)
                                .unwrap()(
                                table, &mut entries, &mut rng
                            ),
                        },
                        4 => {
                            let index = rng.gen_range(0..entries.len());
                            let data: u64 = rng.gen();
                            match &entries[index] {
                                (n1, None, None, old) => {
                                    assert_eq!(
                                        xml_hash_lookup(table, n1.as_ptr() as _) as u64,
                                        *old
                                    );
                                    assert_eq!(
                                        xml_hash_update_entry(
                                            table,
                                            n1.as_ptr() as _,
                                            data as _,
                                            None
                                        ),
                                        0
                                    );
                                    assert_eq!(
                                        xml_hash_lookup(table, n1.as_ptr() as _) as u64,
                                        data
                                    );
                                }
                                (n1, Some(n2), None, old) => {
                                    assert_eq!(
                                        xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _)
                                            as u64,
                                        *old
                                    );
                                    assert_eq!(
                                        xml_hash_update_entry2(
                                            table,
                                            n1.as_ptr() as _,
                                            n2.as_ptr() as _,
                                            data as _,
                                            None
                                        ),
                                        0
                                    );
                                    assert_eq!(
                                        xml_hash_lookup2(table, n1.as_ptr() as _, n2.as_ptr() as _)
                                            as u64,
                                        data
                                    );
                                }
                                (n1, Some(n2), Some(n3), old) => {
                                    assert_eq!(
                                        xml_hash_lookup3(
                                            table,
                                            n1.as_ptr() as _,
                                            n2.as_ptr() as _,
                                            n3.as_ptr() as _
                                        ) as u64,
                                        *old
                                    );
                                    assert_eq!(
                                        xml_hash_update_entry3(
                                            table,
                                            n1.as_ptr() as _,
                                            n2.as_ptr() as _,
                                            n3.as_ptr() as _,
                                            data as _,
                                            None
                                        ),
                                        0
                                    );
                                    assert_eq!(
                                        xml_hash_lookup3(
                                            table,
                                            n1.as_ptr() as _,
                                            n2.as_ptr() as _,
                                            n3.as_ptr() as _
                                        ) as u64,
                                        data
                                    );
                                }
                                _ => {}
                            }
                            entries[index].3 = data;
                        }
                        5 => {
                            extern "C" fn copier(
                                data: *mut c_void,
                                _name: *const XmlChar,
                            ) -> *mut c_void {
                                data
                            }
                            let cloned = xml_hash_copy(table, Some(copier));
                            assert_eq!(xml_hash_size(cloned), xml_hash_size(table));
                            let mut buf: Vec<(CString, Option<CString>, Option<CString>, u64)> =
                                vec![];

                            extern "C" fn scanner(
                                payload: *mut c_void,
                                data: *mut c_void,
                                name: *const XmlChar,
                                name2: *const XmlChar,
                                name3: *const XmlChar,
                            ) {
                                let buf = data as *mut Vec<(
                                    CString,
                                    Option<CString>,
                                    Option<CString>,
                                    u64,
                                )>;
                                unsafe {
                                    (*buf).push((
                                        CStr::from_ptr(name as _).to_owned(),
                                        (!name2.is_null())
                                            .then(|| CStr::from_ptr(name2 as _).to_owned()),
                                        (!name3.is_null())
                                            .then(|| CStr::from_ptr(name3 as _).to_owned()),
                                        payload as u64,
                                    ));
                                }
                            }

                            xml_hash_scan_full(
                                table,
                                Some(scanner),
                                &mut buf as *mut Vec<_> as *mut c_void,
                            );
                            buf.sort_unstable();
                            entries.sort_unstable();
                            assert_eq!(buf, entries);
                        }
                        6 => {
                            let parity = rng.gen_range(0..2);
                            let mut table = XmlHashTableRef::from_raw(table).unwrap();
                            entries.retain(|e| e.3 % 2 == parity);
                            table.remove_if(|data, _, _, _| data.0 as u64 % 2 != parity, |_, _| {});
                        }
                        _ => {}
                    }

                    assert_eq!(entries.len(), xml_hash_size(table) as usize);
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{ffi::CString, num::NonZeroU8};

    use rand::{
        distributions::Alphanumeric, prelude::Distribution, seq::SliceRandom, thread_rng, Rng,
    };

    use super::*;

    const LEN: usize = 30;

    fn gen_chars(rng: &mut impl Rng) -> Vec<NonZeroU8> {
        Alphanumeric
            .sample_iter(rng)
            .map(|c| NonZeroU8::new(c).unwrap())
            .take(LEN)
            .collect::<Vec<_>>()
    }

    fn gen_ncname(rng: &mut impl Rng) -> CString {
        CString::from(gen_chars(rng))
    }

    fn gen_qname(rng: &mut impl Rng) -> CString {
        let mut chars = gen_chars(rng);
        chars[rng.gen_range(1..29)] = NonZeroU8::new(b':').unwrap();
        CString::from(chars)
    }

    fn ncname_insertion_test(
        table: &mut XmlHashTable<u64>,
        entries: &mut Vec<(CString, Option<CString>, Option<CString>, u64)>,
        rng: &mut impl Rng,
    ) {
        match rng.gen_range(1..=3) {
            1 => {
                let n1 = gen_ncname(rng);
                let data = rng.gen::<u64>();
                table.add_entry(&n1, data);
                assert_eq!(table.lookup(&n1), Some(&data));
                entries.push((n1, None, None, data));
            }
            2 => {
                let n1 = gen_ncname(rng);
                let n2 = gen_ncname(rng);
                let data = rng.gen::<u64>();
                table.add_entry2(&n1, Some(&n2), data);
                assert_eq!(table.lookup2(&n1, Some(&n2)), Some(&data));
                entries.push((n1, Some(n2), None, data));
            }
            3 => {
                let n1 = gen_ncname(rng);
                let n2 = gen_ncname(rng);
                let n3 = gen_ncname(rng);
                let data = rng.gen::<u64>();
                table.add_entry3(&n1, Some(&n2), Some(&n3), data);
                assert_eq!(table.lookup3(&n1, Some(&n2), Some(&n3)), Some(&data));
                entries.push((n1, Some(n2), Some(n3), data));
            }
            _ => {}
        }
    }

    fn split_qname(qname: &CStr) -> Option<(CString, CString)> {
        let qname = qname.to_string_lossy();
        let (prefix, local_name) = qname.split_once(':')?;
        Some((
            CString::new(prefix).unwrap(),
            CString::new(local_name).unwrap(),
        ))
    }

    fn qname_insertion_test(
        table: &mut XmlHashTable<u64>,
        entries: &mut Vec<(CString, Option<CString>, Option<CString>, u64)>,
        rng: &mut impl Rng,
    ) {
        match rng.gen_range(1..=3) {
            1 => {
                let n1 = gen_qname(rng);
                let data = rng.gen::<u64>();
                table.add_entry(&n1, data);
                assert_eq!(table.lookup(&n1), Some(&data));
                let (p1, l1) = split_qname(&n1).unwrap();
                assert_eq!(table.qlookup(Some(&p1), &l1), Some(&data));
                entries.push((n1, None, None, data));
            }
            2 => {
                let n1 = gen_qname(rng);
                let n2 = gen_qname(rng);
                let data = rng.gen::<u64>();
                table.add_entry2(&n1, Some(&n2), data);
                assert_eq!(table.lookup2(&n1, Some(&n2)), Some(&data));
                let (p1, l1) = split_qname(&n1).unwrap();
                let (p2, l2) = split_qname(&n2).unwrap();
                assert_eq!(
                    table.qlookup2(Some(&p1), &l1, Some(&p2), Some(&l2)),
                    Some(&data)
                );
                entries.push((n1, Some(n2), None, data));
            }
            3 => {
                let n1 = gen_qname(rng);
                let n2 = gen_qname(rng);
                let n3 = gen_qname(rng);
                let data = rng.gen::<u64>();
                table.add_entry3(&n1, Some(&n2), Some(&n3), data);
                assert_eq!(table.lookup3(&n1, Some(&n2), Some(&n3)), Some(&data));
                let (p1, l1) = split_qname(&n1).unwrap();
                let (p2, l2) = split_qname(&n2).unwrap();
                let (p3, l3) = split_qname(&n3).unwrap();
                assert_eq!(
                    table.qlookup3(Some(&p1), &l1, Some(&p2), Some(&l2), Some(&p3), Some(&l3)),
                    Some(&data)
                );
                entries.push((n1, Some(n2), Some(n3), data));
            }
            _ => {}
        }
    }

    #[test]
    fn random_test() {
        let mut table = XmlHashTable::new();
        let mut rng = thread_rng();
        let mut entries = vec![];

        for _ in 0..100_000 {
            if entries.is_empty() {
                assert!(table.is_empty());
                [ncname_insertion_test, qname_insertion_test]
                    .choose(&mut rng)
                    .unwrap()(&mut table, &mut entries, &mut rng);
            }
            // 0: insert (ncname)
            // 1: insert (qname)
            // 2: remove
            // 3: lookup
            // 4: update
            // 5: clone
            // 6: remove_if
            // 7: clear
            let query = rng.gen_range(0..=6 + (table.len() > 200) as i32);
            match query {
                0 => ncname_insertion_test(&mut table, &mut entries, &mut rng),
                1 => qname_insertion_test(&mut table, &mut entries, &mut rng),
                2 => match entries.swap_remove(rng.gen_range(0..entries.len())) {
                    (n1, None, None, data) => {
                        assert_eq!(table.lookup(&n1), Some(&data));
                        assert!(table.remove_entry(&n1, |_, _| {}).is_ok());
                        assert!(table.lookup(&n1).is_none());
                        assert_eq!(table.len(), entries.len());
                    }
                    (n1, Some(n2), None, data) => {
                        assert_eq!(table.lookup2(&n1, Some(&n2)), Some(&data));
                        assert!(table.remove_entry2(&n1, Some(&n2), |_, _| {}).is_ok());
                        assert!(table.lookup2(&n1, Some(&n2)).is_none());
                        assert_eq!(table.len(), entries.len());
                    }
                    (n1, Some(n2), Some(n3), data) => {
                        assert_eq!(table.lookup3(&n1, Some(&n2), Some(&n3)), Some(&data));
                        assert!(table
                            .remove_entry3(&n1, Some(&n2), Some(&n3), |_, _| {})
                            .is_ok());
                        assert!(table.lookup3(&n1, Some(&n2), Some(&n3)).is_none());
                        assert_eq!(table.len(), entries.len());
                    }
                    _ => [ncname_insertion_test, qname_insertion_test]
                        .choose(&mut rng)
                        .unwrap()(&mut table, &mut entries, &mut rng),
                },
                3 => match entries.choose(&mut rng) {
                    Some((n1, None, None, data)) => {
                        assert_eq!(table.len(), entries.len());
                        assert_eq!(table.lookup(n1), Some(data));
                        match split_qname(n1) {
                            Some((p1, l1)) => assert_eq!(table.qlookup(Some(&p1), &l1), Some(data)),
                            _ => assert_eq!(table.qlookup(None, n1), Some(data)),
                        }
                    }
                    Some((n1, Some(n2), None, data)) => {
                        assert_eq!(table.len(), entries.len());
                        assert_eq!(table.lookup2(n1, Some(n2)), Some(data));
                        match (split_qname(n1), split_qname(n2)) {
                            (Some((p1, l1)), Some((p2, l2))) => {
                                assert_eq!(
                                    table.qlookup2(Some(&p1), &l1, Some(&p2), Some(&l2)),
                                    Some(data)
                                )
                            }
                            _ => assert_eq!(table.qlookup2(None, n1, None, Some(n2)), Some(data)),
                        }
                    }
                    Some((n1, Some(n2), Some(n3), data)) => {
                        assert_eq!(table.len(), entries.len());
                        assert_eq!(table.lookup3(n1, Some(n2), Some(n3)), Some(data));
                        match (split_qname(n1), split_qname(n2), split_qname(n3)) {
                            (Some((p1, l1)), Some((p2, l2)), Some((p3, l3))) => {
                                assert_eq!(
                                    table.qlookup3(
                                        Some(&p1),
                                        &l1,
                                        Some(&p2),
                                        Some(&l2),
                                        Some(&p3),
                                        Some(&l3)
                                    ),
                                    Some(data)
                                );
                            }
                            _ => {
                                assert_eq!(
                                    table.qlookup3(None, n1, None, Some(n2), None, Some(n3)),
                                    Some(data)
                                )
                            }
                        }
                    }
                    _ => [ncname_insertion_test, qname_insertion_test]
                        .choose(&mut rng)
                        .unwrap()(&mut table, &mut entries, &mut rng),
                },
                4 => {
                    let index = rng.gen_range(0..entries.len());
                    let data = rng.gen();
                    match &entries[index] {
                        (n1, None, None, old) => {
                            assert_eq!(table.lookup(n1), Some(old));
                            assert!(table.update_entry(n1, data, |_, _| {}).is_ok());
                            assert_eq!(table.lookup(n1), Some(&data));
                        }
                        (n1, Some(n2), None, old) => {
                            assert_eq!(table.lookup2(n1, Some(n2)), Some(old));
                            assert!(table.update_entry2(n1, Some(n2), data, |_, _| {}).is_ok());
                            assert_eq!(table.lookup2(n1, Some(n2)), Some(&data));
                        }
                        (n1, Some(n2), Some(n3), old) => {
                            assert_eq!(table.lookup3(n1, Some(n2), Some(n3)), Some(old));
                            assert!(table
                                .update_entry3(n1, Some(n2), Some(n3), data, |_, _| {})
                                .is_ok());
                            assert_eq!(table.lookup3(n1, Some(n2), Some(n3)), Some(&data));
                        }
                        _ => {}
                    }
                    entries[index].3 = data;
                }
                5 => {
                    let cloned = table.clone_with(|data, _| *data);
                    assert_eq!(cloned.len(), table.len());
                    let mut buf = vec![];
                    cloned.scan(|data, name, name2, name3| {
                        buf.push((
                            name.map(|n| n.as_ref().to_owned()).unwrap(),
                            name2.map(|n| n.as_ref().to_owned()),
                            name3.map(|n| n.as_ref().to_owned()),
                            *data,
                        ));
                    });
                    buf.sort_unstable();
                    entries.sort_unstable();
                    assert_eq!(buf, entries);
                }
                6 => {
                    let parity = rng.gen_range(0..2);
                    entries.retain(|e| e.3 % 2 == parity);
                    table.remove_if(|data, _, _, _| data % 2 != parity, |_, _| {});
                    assert!(entries.iter().all(|(name, name2, name3, _)| {
                        table
                            .do_lookup(name, name2.as_deref(), name3.as_deref())
                            .is_some()
                    }));
                }
                7 => {
                    table.clear();
                    assert!(table.is_empty());
                    let mut found = false;
                    table.scan(|_, _, _, _| found = true);
                    assert!(!found);
                    entries.clear();
                }
                _ => {}
            }
            assert_eq!(table.len(), entries.len());
        }
    }
}

mod libxml_api;

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
        value += 30 * name[0] as u64;
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
    payload: T,
    valid: i32,
}

impl<'a, T: Default> Default for XmlHashEntry<'a, T> {
    fn default() -> Self {
        Self {
            next: None,
            name: None,
            name2: None,
            name3: None,
            payload: T::default(),
            valid: 0,
        }
    }
}

struct XmlHashEntryRef<'a, T>(NonNull<XmlHashEntry<'a, T>>);

impl<'a, T: Default> XmlHashEntryRef<'a, T> {
    fn new() -> Option<Self> {
        let default = XmlHashEntry::default();
        let boxed = Box::new(default);
        let leaked = Box::leak(boxed);
        NonNull::new(leaked).map(Self)
    }
}

impl<'a, T> XmlHashEntryRef<'a, T> {
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
    pub fn clear(&mut self) {
        self.clear_with(|_, _| {});
    }

    pub fn clear_with(&mut self, deallocator: impl Fn(T, Option<Cow<'_, CStr>>)) {
        let table = take(&mut self.table);
        for XmlHashEntry {
            mut next,
            name,
            payload,
            valid,
            ..
        } in table
        {
            if valid == 0 {
                continue;
            }

            deallocator(payload, name);

            while let Some(now) = next {
                next = now.next;

                let XmlHashEntry { name, payload, .. } = now.into_inner();
                deallocator(payload, name);
            }
        }
    }
}

impl<'a, T: Default> XmlHashTable<'a, T> {
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
            if entry.valid == 0 {
                continue;
            }

            let key = xml_hash_compute_key(
                self,
                entry.name.as_ref(),
                entry.name2.as_ref(),
                entry.name3.as_ref(),
            );
            let new = &mut self.table[key as usize];
            new.valid = 1;
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

                if self.table[key as usize].valid == 0 {
                    self.table[key as usize] = now.into_inner();
                    self.table[key as usize].next = None;
                    self.table[key as usize].valid = 1;
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

        let key =
            xml_hash_compute_key(self, name.as_ref(), name2.as_ref(), name3.as_ref()) as usize;

        let entry = if self.table[key].valid != 0 {
            if self.table[key].name == name
                && self.table[key].name2 == name2
                && self.table[key].name3 == name3
            {
                let old = replace(&mut self.table[key].payload, data);
                deallocator(old, name);
                return Ok(());
            }

            let mut next = self.table[key].next;
            while let Some(now) = next {
                if now.name == name && now.name2 == name2 && now.name3 == name3 {
                    let old = replace(&mut self.table[key].payload, data);
                    deallocator(old, name);
                    return Ok(());
                }
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

        entry.payload = data;
        entry.valid = 1;
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
    /// table.add_entry2(c"foo", c"bar", 2);
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
    /// table.add_entry2(c"foo", c"bar", 2u32);
    /// table.add_entry3(c"hoge", c"fuga", c"piyo", 3);
    ///
    /// let mut updated = false;
    /// assert!(table.update_entry2(c"foo", c"bar", 3, |data, name| {
    ///     assert_eq!(data, 2);
    ///     assert_eq!(name.unwrap().as_ref(), c"foo");
    ///     updated = true;
    /// }).is_ok());
    /// assert!(updated);
    ///
    /// let mut updated = false;
    /// assert!(table.update_entry2(c"hoge", c"fuga", 4, |_, _| {
    ///     updated = true;
    /// }).is_ok());
    /// assert!(!updated);
    /// ```
    pub fn update_entry2(
        &mut self,
        name: &CStr,
        name2: &CStr,
        data: T,
        deallocator: impl FnMut(T, Option<Cow<'_, CStr>>),
    ) -> Result<(), anyhow::Error> {
        self.do_update_entry(name, Some(name2), None, data, deallocator)
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
    /// table.add_entry3(c"hoge", c"fuga", c"piyo", 3);
    ///
    /// let mut updated = false;
    /// assert!(table.update_entry3(c"hoge", c"fuga", c"piyo", 4, |data, name| {
    ///     assert_eq!(data, 3);
    ///     assert_eq!(name.unwrap().as_ref(), c"hoge");
    ///     updated = true;
    /// }).is_ok());
    /// assert!(updated);
    /// ```
    pub fn update_entry3(
        &mut self,
        name: &CStr,
        name2: &CStr,
        name3: &CStr,
        data: T,
        deallocator: impl FnMut(T, Option<Cow<'_, CStr>>),
    ) -> Result<(), anyhow::Error> {
        self.do_update_entry(name, Some(name2), Some(name3), data, deallocator)
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

        let key =
            xml_hash_compute_key(self, name.as_ref(), name2.as_ref(), name3.as_ref()) as usize;

        let mut chain_length = 0;
        let entry = if self.table[key].valid != 0 {
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

        entry.payload = data;
        entry.valid = 1;
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
    /// assert!(table.add_entry2(c"foo", c"bar", 1u32).is_ok());
    /// // (c"foo", c"bar", 1) already exists, so `table` returns `Err`.
    /// assert!(table.add_entry2(c"foo", c"bar", 2u32).is_err());
    /// // c"foo" already exists, but the pair of (c"foo", c"hoge") does not yet exist.
    /// assert!(table.add_entry2(c"foo", c"hoge", 3u32).is_ok());
    ///
    /// assert_eq!(table.lookup2(c"foo", c"bar"), Some(&1));
    /// ```
    pub fn add_entry2(&mut self, name: &CStr, name2: &CStr, data: T) -> Result<(), anyhow::Error> {
        self.do_add_entry(Some(name), Some(name2), None, data)
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
    /// assert!(table.add_entry3(c"foo", c"bar", c"fuga", 1u32).is_ok());
    /// // (c"foo", c"bar", 1) already exists, so `table` returns `Err`.
    /// assert!(table.add_entry3(c"foo", c"bar", c"fuga", 2u32).is_err());
    /// // (c"foo", c"bar") already exists,
    /// // but the pair of (c"foo", c"bar", c"piyo") does not yet exist.
    /// assert!(table.add_entry3(c"foo", c"bar", c"piyo", 3u32).is_ok());
    ///
    /// assert_eq!(table.lookup3(c"foo", c"bar", c"fuga"), Some(&1));
    /// ```
    pub fn add_entry3(
        &mut self,
        name: &CStr,
        name2: &CStr,
        name3: &CStr,
        data: T,
    ) -> Result<(), anyhow::Error> {
        self.do_add_entry(Some(name), Some(name2), Some(name3), data)
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

        let key =
            xml_hash_compute_key(self, name.as_ref(), name2.as_ref(), name3.as_ref()) as usize;

        ensure!(self.table[key].valid != 0, "Entry is not found");

        if self.table[key].name == name
            && self.table[key].name2 == name2
            && self.table[key].name3 == name3
        {
            let (payload, name) = if let Some(next) = self.table[key].next.take() {
                let next = next.into_inner();
                let XmlHashEntry { name, payload, .. } = replace(&mut self.table[key], next);
                (payload, name)
            } else {
                self.table[key].valid = 0;
                (
                    take(&mut self.table[key].payload),
                    self.table[key].name.take(),
                )
            };
            deallocator(payload, name);

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

                deallocator(payload, name);

                self.num_elems -= 1;
                return Ok(());
            }

            prev = Some(now);
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
    /// table.add_entry2(c"hoge", c"fuga", 2);
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
    /// table.add_entry2(c"hoge", c"fuga", 1);
    /// table.add_entry3(c"hoge", c"fuga", c"piyo", 2);
    ///
    /// assert!(table.remove_entry2(c"hoge", c"fuga", |data, _| assert_eq!(data, 1)).is_ok());
    /// // No entries match with (c"hoge", c"fuga") are found.
    /// assert!(table.remove_entry2(c"hoge", c"fuga", |_, _| {}).is_err());
    /// ```
    pub fn remove_entry2(
        &mut self,
        name: &CStr,
        name2: &CStr,
        deallocator: impl Fn(T, Option<Cow<'_, CStr>>),
    ) -> Result<(), anyhow::Error> {
        self.do_remove_entry(name, Some(name2), None, deallocator)
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
    /// table.add_entry3(c"hoge", c"fuga", c"piyo", 2);
    ///
    /// assert!(table.remove_entry3(c"hoge", c"fuga", c"piyo", |data, _| assert_eq!(data, 2)).is_ok());
    /// ```
    pub fn remove_entry3(
        &mut self,
        name: &CStr,
        name2: &CStr,
        name3: &CStr,
        deallocator: impl Fn(T, Option<Cow<'_, CStr>>),
    ) -> Result<(), anyhow::Error> {
        self.do_remove_entry(name, Some(name2), Some(name3), deallocator)
    }

    fn do_lookup(&self, name: &CStr, name2: Option<&CStr>, name3: Option<&CStr>) -> Option<&T> {
        let (name, name2, name3) = (
            Some(Cow::Borrowed(name)),
            name2.map(Cow::Borrowed),
            name3.map(Cow::Borrowed),
        );
        let key =
            xml_hash_compute_key(self, name.as_ref(), name2.as_ref(), name3.as_ref()) as usize;

        if self.table[key].valid == 0 {
            return None;
        }

        if self.table[key].name == name
            && self.table[key].name2 == name2
            && self.table[key].name3 == name3
        {
            return Some(&self.table[key].payload);
        }

        let mut next = self.table[key].next;
        while let Some(now) = next {
            next = now.next;

            if now.name == name && now.name2 == name2 && now.name3 == name3 {
                unsafe {
                    // Lifetime is not enough for `&now.payload`.
                    return Some(&now.0.as_ref().payload);
                }
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
    /// table.add_entry2(c"hoge", c"fuga", 1u32);
    ///
    /// assert_eq!(table.lookup2(c"hoge", c"fuga"), Some(&1));
    /// assert_eq!(table.lookup2(c"hoge", c"piyo"), None);
    /// ```
    pub fn lookup2(&self, name: &CStr, name2: &CStr) -> Option<&T> {
        self.do_lookup(name, Some(name2), None)
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
    /// table.add_entry3(c"hoge", c"fuga", c"piyo", 1u32);
    ///
    /// assert_eq!(table.lookup3(c"hoge", c"fuga", c"piyo"), Some(&1));
    /// assert_eq!(table.lookup3(c"hoge", c"fuga", c"bar"), None);
    /// ```
    pub fn lookup3(&self, name: &CStr, name2: &CStr, name3: &CStr) -> Option<&T> {
        self.do_lookup(name, Some(name2), Some(name3))
    }

    fn do_qlookup(
        &self,
        name: &CStr,
        prefix: Option<&CStr>,
        name2: Option<&CStr>,
        prefix2: Option<&CStr>,
        name3: Option<&CStr>,
        prefix3: Option<&CStr>,
    ) -> Option<&T> {
        let key = xml_hash_compute_qkey(
            self,
            prefix.as_ref().map(|&p| Cow::Borrowed(p)).as_ref(),
            name,
            prefix2.as_ref().map(|&p| Cow::Borrowed(p)).as_ref(),
            name2.as_ref().map(|&p| Cow::Borrowed(p)).as_ref(),
            prefix3.as_ref().map(|&p| Cow::Borrowed(p)).as_ref(),
            name3.as_ref().map(|&p| Cow::Borrowed(p)).as_ref(),
        ) as usize;

        if self.table[key].valid == 0 {
            return None;
        }

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

            if xml_str_qequal(
                prefix,
                name,
                self.table[key].name.as_ref().map_or(null(), |n| n.as_ptr()) as _,
            ) && xml_str_qequal(
                prefix2,
                name2,
                self.table[key]
                    .name2
                    .as_ref()
                    .map_or(null(), |n| n.as_ptr()) as _,
            ) && xml_str_qequal(
                prefix3,
                name3,
                self.table[key]
                    .name3
                    .as_ref()
                    .map_or(null(), |n| n.as_ptr()) as _,
            ) {
                return Some(&self.table[key].payload);
            }

            let mut next = self.table[key].next;
            while let Some(now) = next {
                next = now.next;

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
                    // Lifetime is not enough for `&now.payload`.
                    return Some(&now.0.as_ref().payload);
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
    /// assert_eq!(table.qlookup(c"fuga", Some(c"hoge")), Some(&1));
    /// assert_eq!(table.qlookup(c"hoge:fuga", None), Some(&1));
    /// assert_eq!(table.lookup(c"hoge:fuga"), Some(&1));
    ///
    /// table.add_entry(c"hoge", 2);
    ///
    /// assert_eq!(table.qlookup(c"hoge", None), Some(&2));
    /// ```
    pub fn qlookup(&self, name: &CStr, prefix: Option<&CStr>) -> Option<&T> {
        self.do_qlookup(name, prefix, None, None, None, None)
    }

    /// Search the entry specified by the given QNames.  
    ///
    /// If such entry is found, return `Some`, otherwise return `None`.
    ///
    /// Please refer to the example for `XmlHashTable::qlookup`.
    pub fn qlookup2(
        &self,
        name: &CStr,
        prefix: Option<&CStr>,
        name2: &CStr,
        prefix2: Option<&CStr>,
    ) -> Option<&T> {
        self.do_qlookup(name, prefix, Some(name2), prefix2, None, None)
    }

    /// Search the entry specified by the given QNames.  
    ///
    /// If such entry is found, return `Some`, otherwise return `None`.
    ///
    /// Please refer to the example for `XmlHashTable::qlookup`.
    pub fn qlookup3(
        &self,
        name: &CStr,
        prefix: Option<&CStr>,
        name2: &CStr,
        prefix2: Option<&CStr>,
        name3: &CStr,
        prefix3: Option<&CStr>,
    ) -> Option<&T> {
        self.do_qlookup(name, prefix, Some(name2), prefix2, Some(name3), prefix3)
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
        for entry in &self.table {
            if entry.valid == 0 {
                continue;
            }

            res.do_add_entry(
                entry.name.as_ref().map(|n| n.as_ref()),
                entry.name2.as_ref().map(|n| n.as_ref()),
                entry.name3.as_ref().map(|n| n.as_ref()),
                copier(&entry.payload, entry.name.as_ref()),
            );
        }

        res.num_elems = self.num_elems;
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
    ///
    /// table.remove_if(|data, _, _, _| data % 2 == 0, |_, _| {});
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
            while entry.valid != 0
                && cond(
                    &entry.payload,
                    entry.name.as_ref(),
                    entry.name2.as_ref(),
                    entry.name3.as_ref(),
                )
            {
                let (payload, name) = if let Some(next) = entry.next {
                    let old = replace(entry, next.into_inner());
                    (old.payload, old.name)
                } else {
                    entry.valid = 0;
                    (take(&mut entry.payload), entry.name.take())
                };
                deallocator(payload, name);
            }

            if entry.valid != 0 {
                let mut prev = None::<XmlHashEntryRef<'_, T>>;
                let mut next = entry.next;
                while let Some(now) = next {
                    next = now.next;

                    if cond(
                        &now.payload,
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
                        deallocator(payload, name);
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
    pub fn scan(
        &self,
        mut f: impl FnMut(&T, Option<&Cow<'_, CStr>>, Option<&Cow<'_, CStr>>, Option<&Cow<'_, CStr>>),
    ) {
        for entry in &self.table {
            if entry.valid == 0 {
                continue;
            }

            f(
                &entry.payload,
                entry.name.as_ref(),
                entry.name2.as_ref(),
                entry.name3.as_ref(),
            );

            let mut next = entry.next;
            while let Some(now) = next {
                next = now.next;
                f(
                    &now.payload,
                    now.name.as_ref(),
                    now.name2.as_ref(),
                    now.name3.as_ref(),
                );
            }
        }
    }
}

impl<'a, T: Default> Default for XmlHashTable<'a, T> {
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

impl<'a, T: Default> XmlHashTableRef<'a, T> {
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

impl<'a, T: Default> Deref for XmlHashTableRef<'a, T> {
    type Target = XmlHashTable<'a, T>;

    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl<'a, T: Default> DerefMut for XmlHashTableRef<'a, T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.0.as_mut() }
    }
}

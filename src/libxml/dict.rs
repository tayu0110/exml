//! Provide methods and data structures for dictionary of strings.  
//! This module is based on `libxml/dict.h`, `dict.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_int, c_uint, c_ulong},
    mem::{size_of, zeroed},
    ops::Add,
    ptr::{addr_of_mut, null, null_mut},
};

use libc::{memcpy, memset, rand, size_t, srand, strlen, time, INT_MAX};

use crate::private::threads::{xml_cleanup_mutex, xml_init_mutex};

use super::{
    globals::{xml_free, xml_malloc},
    parser::xml_init_parser,
    threads::{xmlMutexLock, xmlMutexUnlock, XmlMutex},
    xmlstring::{xml_str_qequal, xml_strncmp, XmlChar},
};

const MAX_HASH_LEN: usize = 3;
const MIN_DICT_SIZE: usize = 128;
const MAX_DICT_HASH: usize = 8 * 2048;

/*
 * xmlDictComputeFastKey:
 *
 * Calculate a hash key using a fast hash function that works well
 * for low hash table fill.
 */
unsafe extern "C" fn xml_dict_compute_fast_key(
    name: *const XmlChar,
    mut namelen: c_int,
    seed: c_int,
) -> c_ulong {
    let mut value: c_ulong = seed as _;

    if name.is_null() || namelen <= 0 {
        return value;
    }
    value += *name as u64;
    value <<= 5;
    if namelen > 10 {
        value += *name.add(namelen as usize - 1) as u64;
        namelen = 10;
    }
    for i in 2..=namelen as usize {
        value += *name.add(i - 1) as u64;
    }
    value
}

/*
 * xmlDictComputeBigKey:
 *
 * Calculate a hash key using a good hash function that works well for
 * larger hash table sizes.
 *
 * Hash function by "One-at-a-Time Hash" see
 * http://burtleburtle.net/bob/hash/doobs.html
 */

unsafe extern "C" fn xml_dict_compute_big_key(
    data: *const XmlChar,
    namelen: c_int,
    seed: c_int,
) -> u32 {
    if namelen <= 0 || data.is_null() {
        return 0;
    }

    let mut hash = seed as u32;

    for i in 0..namelen {
        hash = hash.wrapping_add(*data.add(i as usize) as _);
        hash = hash.wrapping_add(hash.wrapping_shl(10));
        hash ^= hash.wrapping_shr(6);
    }

    hash = hash.wrapping_add(hash.wrapping_shl(3));
    hash ^= hash.wrapping_shr(11);
    hash = hash.wrapping_add(hash.wrapping_shl(15));

    hash
}

macro_rules! xml_dict_compute_key {
    ($dict:expr, $name:expr, $len:expr) => {
        if (*$dict).size == MIN_DICT_SIZE {
            xml_dict_compute_fast_key($name, $len, (*$dict).seed)
        } else {
            xml_dict_compute_big_key($name, $len, (*$dict).seed) as _
        }
    };
}

/*
 * xmlDictComputeFastQKey:
 *
 * Calculate a hash key for two strings using a fast hash function
 * that works well for low hash table fill.
 *
 * Neither of the two strings must be NULL.
 */
unsafe extern "C" fn xml_dict_compute_fast_qkey(
    prefix: *const XmlChar,
    mut plen: c_int,
    name: *const XmlChar,
    mut len: c_int,
    seed: c_int,
) -> c_ulong {
    let mut value: c_ulong = seed as _;

    if plen == 0 {
        value += 30 * b':' as u64;
    } else {
        value += 30 * (*prefix) as u64;
    }

    if len > 10 {
        let mut offset: c_int = len - (plen + 1 + 1);
        if offset < 0 {
            offset = len - (10 + 1);
        }
        value += *name.add(offset as usize) as u64;
        len = 10;
        if plen > 10 {
            plen = 10;
        }
    }

    for i in 1..=plen {
        value += *prefix.add(i as usize - 1) as u64;
    }
    len -= plen;
    if len > 0 {
        value += b':' as u64;
        len -= 1;
    }
    for i in 1..=len {
        value += *name.add(i as usize - 1) as u64;
    }
    value
}

/*
 * xmlDictComputeBigQKey:
 *
 * Calculate a hash key for two strings using a good hash function
 * that works well for larger hash table sizes.
 *
 * Hash function by "One-at-a-Time Hash" see
 * http://burtleburtle.net/bob/hash/doobs.html
 *
 * Neither of the two strings must be NULL.
 */
unsafe extern "C" fn xml_dict_compute_big_qkey(
    prefix: *const XmlChar,
    plen: c_int,
    name: *const XmlChar,
    len: c_int,
    seed: c_int,
) -> c_ulong {
    let mut hash: u32 = seed as _;

    for i in 0..plen {
        hash = hash.wrapping_add(*prefix.add(i as usize) as u32);
        hash = hash.wrapping_add(hash.wrapping_shl(10));
        hash ^= hash.wrapping_shr(6);
    }
    hash = hash.wrapping_add(b':' as u32);
    hash = hash.wrapping_add(hash.wrapping_shl(10));
    hash ^= hash.wrapping_shr(6);

    for i in 0..len {
        hash = hash.wrapping_add(*name.add(i as usize) as u32);
        hash = hash.wrapping_add(hash.wrapping_shl(10));
        hash ^= hash.wrapping_shr(6);
    }
    hash = hash.wrapping_add(hash.wrapping_shl(3));
    hash ^= hash.wrapping_shr(11);
    hash = hash.wrapping_add(hash.wrapping_shl(15));

    hash as _
}

macro_rules! xml_dict_compute_qkey {
    ($dict:expr, $prefix:expr, $plen:expr, $name:expr, $len:expr) => {
        if $prefix.is_null() {
            xml_dict_compute_key!($dict, $name, $len)
        } else {
            if (*$dict).size == MIN_DICT_SIZE {
                xml_dict_compute_fast_qkey($prefix, $plen, $name, $len, (*$dict).seed)
            } else {
                xml_dict_compute_big_qkey($prefix, $plen, $name, $len, (*$dict).seed)
            }
        }
    };
}

/*
 * An entry in the dictionary
 */
pub type XmlDictEntryPtr = *mut XmlDictEntry;
#[repr(C)]
pub struct XmlDictEntry {
    next: *mut XmlDictEntry,
    name: *const XmlChar,
    len: c_uint,
    valid: c_int,
    okey: c_ulong,
}

pub type XmlDictStringsPtr = *mut XmlDictStrings;
#[repr(C)]
pub struct XmlDictStrings {
    next: XmlDictStringsPtr,
    free: *mut XmlChar,
    end: *mut XmlChar,
    size: size_t,
    nb_strings: size_t,
    array: [XmlChar; 1],
}

/*
 * The dictionary.
 */
pub type XmlDictPtr = *mut XmlDict;
/*
 * The entire dictionary
 */
#[repr(C)]
pub struct XmlDict {
    ref_counter: c_int,

    dict: *mut XmlDictEntry,
    size: size_t,
    nb_elems: c_uint,
    strings: XmlDictStringsPtr,

    subdict: *mut XmlDict,
    /* used for randomization */
    seed: c_int,
    /* used to impose a limit on size */
    limit: size_t,
}

/*
 * A mutex for modifying the reference counter for shared
 * dictionaries.
 */
static mut XML_DICT_MUTEX: XmlMutex = unsafe { zeroed::<XmlMutex>() };

// /*
//  * Internal data for random function, protected by xmlDictMutex
//  */
// static rand_seed: c_uint = 0;

/*
 * Initializer
 */
/**
 * xmlInitializeDict:
 *
 * DEPRECATED: Alias for xmlInitParser.
 */
#[deprecated]
pub unsafe extern "C" fn xml_initialize_dict() -> c_int {
    xml_init_parser();
    0
}

pub(crate) unsafe extern "C" fn __xml_random() -> c_int {
    xmlMutexLock(addr_of_mut!(XML_DICT_MUTEX) as _);
    let ret = rand();
    xmlMutexUnlock(addr_of_mut!(XML_DICT_MUTEX) as _);
    ret
}

/*
 * Constructor and destructor.
 */
/**
 * xmlDictCreate:
 *
 * Create a new dictionary
 *
 * Returns the newly created dictionary, or NULL if an error occurred.
 */
pub unsafe extern "C" fn xml_dict_create() -> XmlDictPtr {
    xml_init_parser();

    // #ifdef DICT_DEBUG_PATTERNS
    //     fprintf(stderr, "C");
    // #endif

    let dict: XmlDictPtr = xml_malloc(size_of::<XmlDict>()) as _;
    if !dict.is_null() {
        (*dict).ref_counter = 1;
        (*dict).limit = 0;

        (*dict).size = MIN_DICT_SIZE;
        (*dict).nb_elems = 0;
        (*dict).dict = xml_malloc(MIN_DICT_SIZE * size_of::<XmlDictEntry>()) as _;
        (*dict).strings = null_mut();
        (*dict).subdict = null_mut();
        if !(*dict).dict.is_null() {
            memset(
                (*dict).dict as _,
                0,
                MIN_DICT_SIZE * size_of::<XmlDictEntry>(),
            );
            (*dict).seed = __xml_random();
            return dict;
        }
        xml_free(dict as _);
    }
    null_mut()
}

/**
 * xmlDictSetLimit:
 * @dict: the dictionary
 * @limit: the limit in bytes
 *
 * Set a size limit for the dictionary
 * Added in 2.9.0
 *
 * Returns the previous limit of the dictionary or 0
 */
pub unsafe extern "C" fn xml_dict_set_limit(dict: XmlDictPtr, limit: size_t) -> size_t {
    if dict.is_null() {
        return 0;
    }
    let ret: size_t = (*dict).limit;
    (*dict).limit = limit;
    ret
}

/**
 * xmlDictGetUsage:
 * @dict: the dictionary
 *
 * Get how much memory is used by a dictionary for strings
 * Added in 2.9.0
 *
 * Returns the amount of strings allocated
 */
pub unsafe extern "C" fn xml_dict_get_usage(dict: XmlDictPtr) -> size_t {
    let mut pool: XmlDictStringsPtr;
    let mut limit: size_t = 0;

    if dict.is_null() {
        return 0;
    }
    pool = (*dict).strings;
    while !pool.is_null() {
        limit += (*pool).size;
        pool = (*pool).next;
    }
    limit
}

/**
 * xmlDictCreateSub:
 * @sub: an existing dictionary
 *
 * Create a new dictionary, inheriting strings from the read-only
 * dictionary @sub. On lookup, strings are first searched in the
 * new dictionary, then in @sub, and if not found are created in the
 * new dictionary.
 *
 * Returns the newly created dictionary, or NULL if an error occurred.
 */
pub unsafe extern "C" fn xml_dict_create_sub(sub: XmlDictPtr) -> XmlDictPtr {
    let dict: XmlDictPtr = xml_dict_create();

    if !dict.is_null() && !sub.is_null() {
        // #ifdef DICT_DEBUG_PATTERNS
        //         fprintf(stderr, "R");
        // #endif
        (*dict).seed = (*sub).seed;
        (*dict).subdict = sub;
        xml_dict_reference((*dict).subdict);
    }
    dict
}

/**
 * xmlDictReference:
 * @dict: the dictionary
 *
 * Increment the reference counter of a dictionary
 *
 * Returns 0 in case of success and -1 in case of error
 */
pub unsafe extern "C" fn xml_dict_reference(dict: XmlDictPtr) -> c_int {
    if dict.is_null() {
        return -1;
    }
    xmlMutexLock(addr_of_mut!(XML_DICT_MUTEX) as _);
    (*dict).ref_counter += 1;
    xmlMutexUnlock(addr_of_mut!(XML_DICT_MUTEX) as _);
    0
}

/**
 * xmlDictFree:
 * @dict: the dictionary
 *
 * Free the hash @dict and its contents. The userdata is
 * deallocated with @f if provided.
 */
pub unsafe extern "C" fn xml_dict_free(dict: XmlDictPtr) {
    let mut iter: XmlDictEntryPtr;
    let mut next: XmlDictEntryPtr;
    let mut inside_dict: c_int;
    let mut pool: XmlDictStringsPtr;
    let mut nextp: XmlDictStringsPtr;

    if dict.is_null() {
        return;
    }

    /* decrement the counter, it may be shared by a parser and docs */
    xmlMutexLock(addr_of_mut!(XML_DICT_MUTEX) as _);
    (*dict).ref_counter -= 1;
    if (*dict).ref_counter > 0 {
        xmlMutexUnlock(addr_of_mut!(XML_DICT_MUTEX) as _);
        return;
    }

    xmlMutexUnlock(addr_of_mut!(XML_DICT_MUTEX) as _);

    if !(*dict).subdict.is_null() {
        xml_dict_free((*dict).subdict);
    }

    if !(*dict).dict.is_null() {
        let mut i = 0;
        while i < (*dict).size && (*dict).nb_elems > 0 {
            'to_continue: {
                iter = (*dict).dict.add(i);
                if (*iter).valid == 0 {
                    break 'to_continue;
                }
                inside_dict = 1;
                while !iter.is_null() {
                    next = (*iter).next;
                    if inside_dict == 0 {
                        xml_free(iter as _);
                    }
                    (*dict).nb_elems -= 1;
                    inside_dict = 0;
                    iter = next;
                }
            }
            i += 1;
        }
        xml_free((*dict).dict as _);
    }
    pool = (*dict).strings;
    while !pool.is_null() {
        nextp = (*pool).next;
        xml_free(pool as _);
        pool = nextp;
    }
    xml_free(dict as _);
}

/*
 * xmlDictAddString:
 * @dict: the dictionary
 * @name: the name of the userdata
 * @len: the length of the name
 *
 * Add the string to the array[s]
 *
 * Returns the pointer of the local string, or NULL in case of error.
 */
unsafe extern "C" fn xml_dict_add_string(
    dict: XmlDictPtr,
    name: *const XmlChar,
    namelen: c_uint,
) -> *const XmlChar {
    let mut pool: XmlDictStringsPtr;
    let mut size: size_t = 0; /* + sizeof(_xmlDictStrings) == 1024 */
    let mut limit: size_t = 0;

    // #ifdef DICT_DEBUG_PATTERNS
    //     fprintf(stderr, "-");
    // #endif
    pool = (*dict).strings;
    'found_pool: {
        while !pool.is_null() {
            if (*pool).end.offset_from((*pool).free) as size_t > namelen as size_t {
                break 'found_pool;
            }
            if (*pool).size > size {
                size = (*pool).size;
            }
            limit += (*pool).size;
            pool = (*pool).next;
        }
        /*
         * Not found, need to allocate
         */
        if pool.is_null() {
            if (*dict).limit > 0 && limit > (*dict).limit {
                return null();
            }

            if size == 0 {
                size = 1000;
            } else {
                size *= 4; /* exponential growth */
            }
            if size < 4 * namelen as usize {
                size = 4 * namelen as usize; /* just in case ! */
            }
            pool = xml_malloc(size_of::<XmlDictStrings>() + size) as XmlDictStringsPtr;
            if pool.is_null() {
                return null();
            }
            (*pool).size = size;
            (*pool).nb_strings = 0;
            (*pool).free = addr_of_mut!((*pool).array[0]);
            (*pool).end = addr_of_mut!((*pool).array[0]).add(size);
            (*pool).next = (*dict).strings;
            (*dict).strings = pool;
            // #ifdef DICT_DEBUG_PATTERNS
            //         fprintf(stderr, "+");
            // #endif
        }
    }
    // found_pool:
    let ret: *const XmlChar = (*pool).free;
    memcpy((*pool).free as _, name as _, namelen as usize);
    (*pool).free = (*pool).free.add(namelen as usize);
    assert!(!pool.is_null());
    assert!(!(*pool).free.is_null());
    *(*pool).free = 0;
    (*pool).free = (*pool).free.add(1);
    (*pool).nb_strings += 1;
    ret
}

/**
 * xmlDictGrow:
 * @dict: the dictionary
 * @size: the new size of the dictionary
 *
 * resize the dictionary
 *
 * Returns 0 in case of success, -1 in case of failure
 */
unsafe extern "C" fn xml_dict_grow(dict: XmlDictPtr, size: size_t) -> c_int {
    let mut key: c_ulong;
    let mut okey: c_ulong;
    let mut iter: XmlDictEntryPtr;
    let mut next: XmlDictEntryPtr;
    // #ifdef DEBUG_GROW
    //     unsigned long nbElem = 0;
    // #endif
    let mut ret: c_int = 0;
    let mut keep_keys: c_int = 1;

    if dict.is_null() {
        return -1;
    }
    if size < 8 {
        return -1;
    }
    if size > 8 * 2048 {
        return -1;
    }

    // #ifdef DICT_DEBUG_PATTERNS
    //     fprintf(stderr, "*");
    // #endif

    let oldsize: size_t = (*dict).size;
    let olddict: *mut XmlDictEntry = (*dict).dict;
    if olddict.is_null() {
        return -1;
    }
    if oldsize == MIN_DICT_SIZE {
        keep_keys = 0;
    }

    (*dict).dict = xml_malloc(size * size_of::<XmlDictEntry>()) as _;
    if (*dict).dict.is_null() {
        (*dict).dict = olddict;
        return -1;
    }
    memset((*dict).dict as _, 0, size * size_of::<XmlDictEntry>());
    (*dict).size = size;

    /*	If the two loops are merged, there would be situations where
    a new entry needs to allocated and data copied into it from
    the main dict. It is nicer to run through the array twice, first
    copying all the elements in the main array (less probability of
    allocate) and then the rest, so we only free in the second loop.
    */
    for i in 0..oldsize {
        if (*olddict.add(i)).valid == 0 {
            continue;
        }

        if keep_keys != 0 {
            okey = (*olddict.add(i)).okey;
        } else {
            okey = xml_dict_compute_key!(dict, (*olddict.add(i)).name, (*olddict.add(i)).len as i32)
                as _;
        }
        key = okey % (*dict).size as u64;

        if (*(*dict).dict.add(key as usize)).valid == 0 {
            memcpy(
                (*dict).dict.add(key as usize) as _,
                olddict.add(i) as _,
                size_of::<XmlDictEntry>(),
            );
            (*(*dict).dict.add(key as usize)).next = null_mut();
            (*(*dict).dict.add(key as usize)).okey = okey;
        } else {
            let entry = xml_malloc(size_of::<XmlDictEntry>()) as XmlDictEntryPtr;
            if !entry.is_null() {
                (*entry).name = (*olddict.add(i)).name;
                (*entry).len = (*olddict.add(i)).len;
                (*entry).okey = okey;
                (*entry).next = (*(*dict).dict.add(key as usize)).next;
                (*entry).valid = 1;
                (*(*dict).dict.add(key as usize)).next = entry;
            } else {
                /*
                 * we don't have much ways to alert from here
                 * result is losing an entry and unicity guarantee
                 */
                ret = -1;
            }
        }
        // #ifdef DEBUG_GROW
        // 	nbElem++;
        // #endif
    }

    for i in 0..oldsize {
        iter = (*olddict.add(i)).next;
        while !iter.is_null() {
            next = (*iter).next;

            /*
             * put back the entry in the new dict
             */

            if keep_keys != 0 {
                okey = (*iter).okey;
            } else {
                okey = xml_dict_compute_key!(dict, (*iter).name, (*iter).len as _);
            }
            key = okey % (*dict).size as u64;
            if (*(*dict).dict.add(key as usize)).valid == 0 {
                memcpy(
                    ((*dict).dict.add(key as usize)) as _,
                    iter as _,
                    size_of::<XmlDictEntry>(),
                );
                (*(*dict).dict.add(key as usize)).next = null_mut();
                (*(*dict).dict.add(key as usize)).valid = 1;
                (*(*dict).dict.add(key as usize)).okey = okey;
                xml_free(iter as _);
            } else {
                (*iter).next = (*(*dict).dict.add(key as usize)).next;
                (*iter).okey = okey;
                (*(*dict).dict.add(key as usize)).next = iter;
            }

            // #ifdef DEBUG_GROW
            // 	    nbElem++;
            // #endif

            iter = next;
        }
    }

    xml_free(olddict as _);

    // #ifdef DEBUG_GROW
    //     xmlGenericError(xmlGenericErrorContext,
    // 	    "xmlDictGrow : from %lu to %lu, %u elems\n", oldsize, size, nbElem);
    // #endif

    ret
}

/*
 * Lookup of entry in the dictionary.
 */
/**
 * xmlDictLookup:
 * @dict: the dictionary
 * @name: the name of the userdata
 * @len: the length of the name, if -1 it is recomputed
 *
 * Add the @name to the dictionary @dict if not present.
 *
 * Returns the internal copy of the name or NULL in case of internal error
 */
pub unsafe extern "C" fn xml_dict_lookup(
    dict: XmlDictPtr,
    name: *const XmlChar,
    len: c_int,
) -> *const XmlChar {
    let mut key: c_ulong;
    let mut nbi: c_ulong = 0;
    let entry: XmlDictEntryPtr;
    let mut insert: XmlDictEntryPtr;

    if dict.is_null() || name.is_null() {
        return null();
    }

    let l: u32 = if len < 0 {
        strlen(name as _) as _
    } else {
        len as _
    };

    if ((*dict).limit > 0 && l >= (*dict).limit as u32) || l > INT_MAX as u32 / 2 {
        return null();
    }

    /*
     * Check for duplicate and insertion location.
     */
    let okey: c_ulong = xml_dict_compute_key!(dict, name, l as _);
    key = okey % (*dict).size as u64;
    if (*(*dict).dict.add(key as usize)).valid == 0 {
        insert = null_mut();
    } else {
        insert = (*dict).dict.add(key as usize);
        while !(*insert).next.is_null() {
            if (*insert).okey == okey
                && (*insert).len == l
                && xml_strncmp((*insert).name, name, l as _) == 0
            {
                return (*insert).name;
            }
            nbi += 1;
            insert = (*insert).next;
        }

        if (*insert).okey == okey
            && (*insert).len == l
            && xml_strncmp((*insert).name, name, l as _) == 0
        {
            return (*insert).name;
        }
    }

    if !(*dict).subdict.is_null() {
        /* we cannot always reuse the same okey for the subdict */
        let skey = if ((*dict).size == MIN_DICT_SIZE && (*(*dict).subdict).size != MIN_DICT_SIZE)
            || ((*dict).size != MIN_DICT_SIZE && (*(*dict).subdict).size == MIN_DICT_SIZE)
        {
            xml_dict_compute_key!((*dict).subdict, name, l as _)
        } else {
            okey
        };

        key = skey % (*(*dict).subdict).size as u64;
        if (*(*(*dict).subdict).dict.add(key as usize)).valid != 0 {
            let mut tmp: XmlDictEntryPtr = (*(*dict).subdict).dict.add(key as usize);
            while !(*tmp).next.is_null() {
                if (*tmp).okey == skey
                    && (*tmp).len == l
                    && xml_strncmp((*tmp).name, name, l as _) == 0
                {
                    return (*tmp).name;
                }
                nbi += 1;
                tmp = (*tmp).next
            }
            assert!(!tmp.is_null());
            if (*tmp).okey == skey && (*tmp).len == l && xml_strncmp((*tmp).name, name, l as _) == 0
            {
                return (*tmp).name;
            }
        }
        key = okey % (*dict).size as u64;
    }

    let ret: *const XmlChar = xml_dict_add_string(dict, name, l);
    if ret.is_null() {
        return null();
    }
    if insert.is_null() {
        entry = (*dict).dict.add(key as usize);
    } else {
        entry = xml_malloc(size_of::<XmlDictEntry>()) as _;
        if entry.is_null() {
            return null();
        }
    }
    (*entry).name = ret;
    (*entry).len = l;
    (*entry).next = null_mut();
    (*entry).valid = 1;
    (*entry).okey = okey;

    if !insert.is_null() {
        (*insert).next = entry;
    }

    (*dict).nb_elems += 1;

    if nbi > MAX_HASH_LEN as u64
        && (*dict).size <= ((MAX_DICT_HASH / 2) / MAX_HASH_LEN)
        && xml_dict_grow(dict, MAX_HASH_LEN * 2 * (*dict).size) != 0
    {
        return null();
    }
    /* Note that entry may have been freed at this point by xmlDictGrow */

    ret
}

/**
 * xmlDictExists:
 * @dict: the dictionary
 * @name: the name of the userdata
 * @len: the length of the name, if -1 it is recomputed
 *
 * Check if the @name exists in the dictionary @dict.
 *
 * Returns the internal copy of the name or NULL if not found.
 */
pub unsafe extern "C" fn xml_dict_exists(
    dict: XmlDictPtr,
    name: *const XmlChar,
    len: c_int,
) -> *const XmlChar {
    let mut key: c_ulong;
    let mut insert: XmlDictEntryPtr;

    if dict.is_null() || name.is_null() {
        return null();
    }

    let l: u32 = if len < 0 {
        strlen(name as _) as _
    } else {
        len as _
    };
    if ((*dict).limit > 0 && (l as usize) >= (*dict).limit) || l > INT_MAX as u32 / 2 {
        return null();
    }

    /*
     * Check for duplicate and insertion location.
     */
    let okey: c_ulong = xml_dict_compute_key!(dict, name, l as _);
    key = okey % (*dict).size as u64;
    if (*(*dict).dict.add(key as usize)).valid == 0 {
        insert = null_mut();
    } else {
        insert = (*dict).dict.add(key as usize);
        while !(*insert).next.is_null() {
            if (*insert).okey == okey
                && (*insert).len == l
                && xml_strncmp((*insert).name, name, l as _) == 0
            {
                return (*insert).name;
            }
            insert = (*insert).next;
        }
        if (*insert).okey == okey
            && (*insert).len == l
            && xml_strncmp((*insert).name, name, l as _) == 0
        {
            return (*insert).name;
        }
    }

    if !(*dict).subdict.is_null() {
        /* we cannot always reuse the same okey for the subdict */
        let skey = if ((*dict).size == MIN_DICT_SIZE && (*(*dict).subdict).size != MIN_DICT_SIZE)
            || ((*dict).size != MIN_DICT_SIZE && (*(*dict).subdict).size == MIN_DICT_SIZE)
        {
            xml_dict_compute_key!((*dict).subdict, name, l as _)
        } else {
            okey
        };

        key = skey % (*(*dict).subdict).size as u64;
        if (*(*(*dict).subdict).dict.add(key as usize)).valid != 0 {
            let mut tmp: XmlDictEntryPtr = (*(*dict).subdict).dict.add(key as usize);

            while !(*tmp).next.is_null() {
                if (*tmp).okey == skey
                    && (*tmp).len == l
                    && xml_strncmp((*tmp).name, name, l as _) == 0
                {
                    return (*tmp).name;
                }
                tmp = (*tmp).next;
            }
            if (*tmp).okey == skey && (*tmp).len == l && xml_strncmp((*tmp).name, name, l as _) == 0
            {
                return (*tmp).name;
            }
        }
    }

    /* not found */
    null()
}

/*
 * xmlDictAddQString:
 * @dict: the dictionary
 * @prefix: the prefix of the userdata
 * @plen: the prefix length
 * @name: the name of the userdata
 * @len: the length of the name
 *
 * Add the QName to the array[s]
 *
 * Returns the pointer of the local string, or NULL in case of error.
 */
unsafe extern "C" fn xmlDictAddQString(
    dict: XmlDictPtr,
    prefix: *const XmlChar,
    plen: c_uint,
    name: *const XmlChar,
    namelen: c_uint,
) -> *const XmlChar {
    let mut pool: XmlDictStringsPtr;
    let mut size: size_t = 0; /* + sizeof(_xmlDictStrings) == 1024 */
    let mut limit: size_t = 0;

    if prefix.is_null() {
        return xml_dict_add_string(dict, name, namelen);
    }

    // #ifdef DICT_DEBUG_PATTERNS
    //     fprintf(stderr, "=");
    // #endif
    pool = (*dict).strings;
    'found_pool: {
        while !pool.is_null() {
            if (*pool).end.offset_from((*pool).free) as size_t > (namelen + plen + 1) as usize {
                break 'found_pool;
            }
            if (*pool).size > size {
                size = (*pool).size;
            }
            limit += (*pool).size;
            pool = (*pool).next;
        }
        /*
         * Not found, need to allocate
         */
        if pool.is_null() {
            if (*dict).limit > 0 && limit > (*dict).limit {
                return null();
            }

            if size == 0 {
                size = 1000;
            } else {
                size *= 4; /* exponential growth */
            }
            if size < 4 * (namelen + plen + 1) as usize {
                size = 4 * (namelen + plen + 1) as usize; /* just in case ! */
            }
            pool = xml_malloc(size_of::<XmlDictStrings>() + size) as XmlDictStringsPtr;
            if pool.is_null() {
                return null();
            }
            (*pool).size = size;
            (*pool).nb_strings = 0;
            (*pool).free = addr_of_mut!((*pool).array[0]);
            (*pool).end = addr_of_mut!((*pool).array[0]).add(size);
            (*pool).next = (*dict).strings;
            (*dict).strings = pool;
            // #ifdef DICT_DEBUG_PATTERNS
            //         fprintf(stderr, "+");
            // #endif
        }
    }
    // found_pool:
    assert!(!(*pool).free.is_null());
    let ret: *const XmlChar = (*pool).free;
    memcpy((*pool).free as _, prefix as _, plen as _);
    (*pool).free = (*pool).free.add(plen as _);
    *(*pool).free = b':' as _;
    (*pool).free = (*pool).free.add(1);
    memcpy((*pool).free as _, name as _, namelen as usize);
    (*pool).free = (*pool).free.add(namelen as _);
    *(*pool).free = 0;
    (*pool).free = (*pool).free.add(1);
    (*pool).nb_strings += 1;
    ret
}

/**
 * xmlDictQLookup:
 * @dict: the dictionary
 * @prefix: the prefix
 * @name: the name
 *
 * Add the QName @prefix:@name to the hash @dict if not present.
 *
 * Returns the internal copy of the QName or NULL in case of internal error
 */
pub unsafe extern "C" fn xml_dict_qlookup(
    dict: XmlDictPtr,
    prefix: *const XmlChar,
    name: *const XmlChar,
) -> *const XmlChar {
    let mut key: c_ulong;
    let mut nbi: c_ulong = 0;
    let entry: XmlDictEntryPtr;
    let mut insert: XmlDictEntryPtr;
    let mut len: c_uint;

    if dict.is_null() || name.is_null() {
        return null();
    }
    if prefix.is_null() {
        return xml_dict_lookup(dict, name, -1);
    }

    let l: c_uint = strlen(name as _) as _;
    len = l;
    let plen: c_uint = strlen(prefix as _) as _;
    len += 1 + plen;

    /*
     * Check for duplicate and insertion location.
     */
    let okey: c_ulong = xml_dict_compute_qkey!(dict, prefix, plen as _, name, l as _);
    key = okey % (*dict).size as u64;
    if (*(*dict).dict.add(key as usize)).valid == 0 {
        insert = null_mut();
    } else {
        insert = (*dict).dict.add(key as usize);
        while !(*insert).next.is_null() {
            if (*insert).okey == okey
                && (*insert).len == len
                && xml_str_qequal(prefix, name, (*insert).name) != 0
            {
                return (*insert).name;
            }
            nbi += 1;
            insert = (*insert).next;
        }
        if (*insert).okey == okey
            && (*insert).len == len
            && xml_str_qequal(prefix, name, (*insert).name) != 0
        {
            return (*insert).name;
        }
    }

    if !(*dict).subdict.is_null() {
        /* we cannot always reuse the same okey for the subdict */
        let skey = if ((*dict).size == MIN_DICT_SIZE && (*(*dict).subdict).size != MIN_DICT_SIZE)
            || ((*dict).size != MIN_DICT_SIZE && (*(*dict).subdict).size == MIN_DICT_SIZE)
        {
            xml_dict_compute_qkey!((*dict).subdict, prefix, plen as _, name, l as _)
        } else {
            okey
        };

        key = skey % (*(*dict).subdict).size as u64;
        if (*(*(*dict).subdict).dict.add(key as usize)).valid != 0 {
            let mut tmp: XmlDictEntryPtr = (*(*dict).subdict).dict.add(key as usize);
            while !(*tmp).next.is_null() {
                if (*tmp).okey == skey
                    && (*tmp).len == len
                    && xml_str_qequal(prefix, name, (*tmp).name) != 0
                {
                    return (*tmp).name;
                }
                nbi += 1;
                tmp = (*tmp).next;
            }
            if (*tmp).okey == skey
                && (*tmp).len == len
                && xml_str_qequal(prefix, name, (*tmp).name) != 0
            {
                return (*tmp).name;
            }
        }
        key = okey % (*dict).size as u64;
    }

    let ret: *const XmlChar = xmlDictAddQString(dict, prefix, plen, name, l);
    if ret.is_null() {
        return null();
    }
    if insert.is_null() {
        entry = (*dict).dict.add(key as usize);
    } else {
        entry = xml_malloc(size_of::<XmlDictEntry>()) as _;
        if entry.is_null() {
            return null();
        }
    }
    (*entry).name = ret;
    (*entry).len = len;
    (*entry).next = null_mut();
    (*entry).valid = 1;
    (*entry).okey = okey;

    if !insert.is_null() {
        (*insert).next = entry;
    }

    (*dict).nb_elems += 1;

    if nbi > MAX_HASH_LEN as u64 && (*dict).size <= (MAX_DICT_HASH / 2) / MAX_HASH_LEN {
        xml_dict_grow(dict, MAX_HASH_LEN * 2 * (*dict).size);
        /* Note that entry may have been freed at this point by xmlDictGrow */
    }

    ret
}

/**
 * xmlDictOwns:
 * @dict: the dictionary
 * @str: the string
 *
 * check if a string is owned by the dictionary
 *
 * Returns 1 if true, 0 if false and -1 in case of error
 * -1 in case of error
 */
pub unsafe extern "C" fn xml_dict_owns(dict: XmlDictPtr, str: *const XmlChar) -> c_int {
    let mut pool: XmlDictStringsPtr;

    if dict.is_null() || str.is_null() {
        return -1;
    }
    pool = (*dict).strings;
    while !pool.is_null() {
        if str >= (*pool).array.as_ptr() && str <= (*pool).free {
            return 1;
        }
        pool = (*pool).next;
    }
    if !(*dict).subdict.is_null() {
        return xml_dict_owns((*dict).subdict, str);
    }
    0
}

/**
 * xmlDictSize:
 * @dict: the dictionary
 *
 * Query the number of elements installed in the hash @dict.
 *
 * Returns the number of elements in the dictionary or
 * -1 in case of error
 */
pub unsafe extern "C" fn xml_dict_size(dict: XmlDictPtr) -> c_int {
    if dict.is_null() {
        return -1;
    }
    if !(*dict).subdict.is_null() {
        return ((*dict).nb_elems + (*(*dict).subdict).nb_elems) as _;
    }
    (*dict).nb_elems as _
}

/*
 * Cleanup function
 */
/**
 * xmlDictCleanup:
 *
 * DEPRECATED: This function is a no-op. Call xmlCleanupParser
 * to free global state but see the warnings there. xmlCleanupParser
 * should be only called once at program exit. In most cases, you don't
 * have call cleanup functions at all.
 */
#[deprecated]
pub unsafe extern "C" fn xml_dict_cleanup() {}

/**
 * __xmlInitializeDict:
 *
 * This function is not public
 * Do the dictionary mutex initialization.
 */
pub(crate) unsafe extern "C" fn __xml_initialize_dict() -> c_int {
    xml_init_mutex(addr_of_mut!(XML_DICT_MUTEX));

    // #ifdef DICT_RANDOMIZATION
    // #ifdef HAVE_RAND_R
    //     rand_seed = time(NULL);
    //     rand_r(& rand_seed);
    // #else
    srand(time(null_mut()) as _);
    // #endif
    // #endif
    1
}

/**
 * xmlCleanupDictInternal:
 *
 * Free the dictionary mutex.
 */
pub(crate) unsafe extern "C" fn xml_cleanup_dict_internal() {
    xml_cleanup_mutex(addr_of_mut!(XML_DICT_MUTEX));
}

#[cfg(test)]
mod tests {
    use crate::{
        libxml::{xmlerror::xmlResetLastError, xmlmemory::xml_mem_blocks, xmlstring::xml_strlen},
        test_util::*,
    };

    use super::*;

    #[test]
    fn test_xml_dict_cleanup() {
        let mut leaks = 0;
        unsafe {
            let mem_base = xml_mem_blocks();

            xml_dict_cleanup();
            xmlResetLastError();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlDictCleanup",
                    xml_mem_blocks() - mem_base
                );
                assert!(leaks == 0, "{leaks} Leaks are found in xmlDictCleanup()");
            }
        }
    }

    #[test]
    fn test_xml_dict_create() {
        let mut leaks = 0;
        unsafe {
            let mem_base = xml_mem_blocks();

            let ret_val = xml_dict_create();
            desret_xml_dict_ptr(ret_val);
            xmlResetLastError();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlDictCreate",
                    xml_mem_blocks() - mem_base
                );
                assert!(leaks == 0, "{leaks} Leaks are found in xmlDictCreate()");
            }
        }
    }

    #[test]
    fn test_xml_dict_create_sub() {
        let mut leaks = 0;

        unsafe {
            for n_sub in 0..GEN_NB_XML_DICT_PTR {
                let mem_base = xml_mem_blocks();
                let sub = gen_xml_dict_ptr(n_sub, 0);

                let ret_val = xml_dict_create_sub(sub);
                desret_xml_dict_ptr(ret_val);
                des_xml_dict_ptr(n_sub, sub, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlDictCreateSub",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_sub);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlDictCreateSub()");
        }
    }

    #[test]
    fn test_xml_dict_exists() {
        let mut leaks = 0;

        unsafe {
            for n_dict in 0..GEN_NB_XML_DICT_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let dict = gen_xml_dict_ptr(n_dict, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let mut len = gen_int(n_len, 2);
                        if !name.is_null() && len > xml_strlen(name) {
                            len = 0;
                        }

                        let ret_val = xml_dict_exists(dict, name, len);
                        desret_const_xml_char_ptr(ret_val);
                        des_xml_dict_ptr(n_dict, dict, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_int(n_len, len, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDictExists",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_dict);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlDictExists()");
        }
    }

    #[test]
    fn test_xml_dict_get_usage() {

        /* missing type support */
    }

    #[test]
    fn test_xml_dict_lookup() {
        let mut leaks = 0;

        unsafe {
            for n_dict in 0..GEN_NB_XML_DICT_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_len in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let dict = gen_xml_dict_ptr(n_dict, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let mut len = gen_int(n_len, 2);
                        if !name.is_null() && len > xml_strlen(name) {
                            len = 0;
                        }

                        let ret_val = xml_dict_lookup(dict, name, len);
                        desret_const_xml_char_ptr(ret_val);
                        des_xml_dict_ptr(n_dict, dict, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_int(n_len, len, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDictLookup",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_dict);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_len);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlDictLookup()");
        }
    }

    #[test]
    fn test_xml_dict_owns() {
        let mut leaks = 0;

        unsafe {
            for n_dict in 0..GEN_NB_XML_DICT_PTR {
                for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let dict = gen_xml_dict_ptr(n_dict, 0);
                    let str = gen_const_xml_char_ptr(n_str, 1);

                    let ret_val = xml_dict_owns(dict, str);
                    desret_int(ret_val);
                    des_xml_dict_ptr(n_dict, dict, 0);
                    des_const_xml_char_ptr(n_str, str, 1);
                    xmlResetLastError();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlDictOwns",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_dict);
                        eprintln!(" {}", n_str);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlDictOwns()");
        }
    }

    #[test]
    fn test_xml_dict_qlookup() {
        let mut leaks = 0;

        unsafe {
            for n_dict in 0..GEN_NB_XML_DICT_PTR {
                for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let dict = gen_xml_dict_ptr(n_dict, 0);
                        let prefix = gen_const_xml_char_ptr(n_prefix, 1);
                        let name = gen_const_xml_char_ptr(n_name, 2);

                        let ret_val = xml_dict_qlookup(dict, prefix, name);
                        desret_const_xml_char_ptr(ret_val);
                        des_xml_dict_ptr(n_dict, dict, 0);
                        des_const_xml_char_ptr(n_prefix, prefix, 1);
                        des_const_xml_char_ptr(n_name, name, 2);
                        xmlResetLastError();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlDictQLookup",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_dict);
                            eprint!(" {}", n_prefix);
                            eprintln!(" {}", n_name);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlDictQLookup()");
        }
    }

    #[test]
    fn test_xml_dict_reference() {
        let mut leaks = 0;

        unsafe {
            for n_dict in 0..GEN_NB_XML_DICT_PTR {
                let mem_base = xml_mem_blocks();
                let dict = gen_xml_dict_ptr(n_dict, 0);

                let ret_val = xml_dict_reference(dict);
                xml_dict_free(dict);
                desret_int(ret_val);
                des_xml_dict_ptr(n_dict, dict, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlDictReference",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_dict);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlDictReference()");
        }
    }

    #[test]
    fn test_xml_dict_set_limit() {

        /* missing type support */
    }

    #[test]
    fn test_xml_dict_size() {
        let mut leaks = 0;

        unsafe {
            for n_dict in 0..GEN_NB_XML_DICT_PTR {
                let mem_base = xml_mem_blocks();
                let dict = gen_xml_dict_ptr(n_dict, 0);

                let ret_val = xml_dict_size(dict);
                desret_int(ret_val);
                des_xml_dict_ptr(n_dict, dict, 0);
                xmlResetLastError();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlDictSize",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_dict);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlDictSize()");
        }
    }

    #[test]
    fn test_xml_initialize_dict() {
        let mut leaks = 0;

        unsafe {
            let mem_base = xml_mem_blocks();
            let ret_val = xml_initialize_dict();
            desret_int(ret_val);
            xmlResetLastError();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in xmlInitializeDict",
                    xml_mem_blocks() - mem_base
                );
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlInitializeDict()");
        }
    }
}

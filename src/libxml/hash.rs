//! Provide methods and data structures for hash table.  
//! This module is based by `libxml/hash.h`, `hash.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_int, c_ulong, c_void},
    mem::{size_of, zeroed},
    ptr::{addr_of_mut, null, null_mut},
};

use libc::{memcpy, memset};

use crate::libxml::{
    dict::{
        __xml_random, xml_dict_free, xml_dict_lookup, xml_dict_owns, xml_dict_reference, XmlDictPtr,
    },
    globals::{xml_free, xml_malloc},
    parser::xml_init_parser,
    xmlstring::{xml_str_equal, xml_str_qequal, xml_strdup, XmlChar},
};

const MAX_HASH_LEN: usize = 8;

/*
 * A single entry in the hash table
 */

pub type XmlHashEntryPtr = *mut XmlHashEntry;
#[repr(C)]
pub struct XmlHashEntry {
    next: *mut XmlHashEntry,
    name: *mut XmlChar,
    name2: *mut XmlChar,
    name3: *mut XmlChar,
    payload: *mut c_void,
    valid: c_int,
}

/*
 * The hash table.
 */

pub type XmlHashTablePtr = *mut XmlHashTable;
/*
 * The entire hash table
 */
#[repr(C)]
pub struct XmlHashTable {
    table: *mut XmlHashEntry,
    size: c_int,
    nb_elems: c_int,
    dict: XmlDictPtr,
    random_seed: c_int,
}

/*
 * Recent version of gcc produce a warning when a function pointer is assigned
 * to an object pointer, or vice versa.  The following macro is a dirty hack
 * to allow suppression of the warning.  If your architecture has function
 * pointers which are a different size than a void pointer, there may be some
 * serious trouble within the library.
 */
/**
 * XML_CAST_FPTR:
 * @fptr:  pointer to a function
 *
 * Macro to do a casting from an object pointer to a
 * function pointer without encountering a warning from
 * gcc
 *
 * #define XML_CAST_FPTR(fptr) (*(void **)(&fptr))
 * This macro violated ISO C aliasing rules (gcc4 on s390 broke)
 * so it is disabled now
 */

// #define XML_CAST_FPTR(fptr) fptr

/*
 * function types:
 */
/**
 * xmlHashDeallocator:
 * @payload:  the data in the hash
 * @name:  the name associated
 *
 * Callback to free data from a hash.
 */
pub type XmlHashDeallocator = unsafe extern "C" fn(payload: *mut c_void, name: *const XmlChar);
/**
 * xmlHashCopier:
 * @payload:  the data in the hash
 * @name:  the name associated
 *
 * Callback to copy data from a hash.
 *
 * Returns a copy of the data or NULL in case of error.
 */
pub type XmlHashCopier =
    unsafe extern "C" fn(payload: *mut c_void, name: *const XmlChar) -> *mut c_void;
/**
 * xmlHashScanner:
 * @payload:  the data in the hash
 * @data:  extra scanner data
 * @name:  the name associated
 *
 * Callback when scanning data in a hash with the simple scanner.
 */
pub type XmlHashScanner =
    unsafe extern "C" fn(payload: *mut c_void, data: *mut c_void, name: *const XmlChar);
/**
 * xmlHashScannerFull:
 * @payload:  the data in the hash
 * @data:  extra scanner data
 * @name:  the name associated
 * @name2:  the second name associated
 * @name3:  the third name associated
 *
 * Callback when scanning data in a hash with the full scanner.
 */
pub type XmlHashScannerFull = unsafe extern "C" fn(
    payload: *mut c_void,
    data: *mut c_void,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
);

/*
 * Constructor and destructor.
 */
/**
 * xmlHashCreate:
 * @size: the size of the hash table
 *
 * Create a new xmlHashTablePtr.
 *
 * Returns the newly created object, or NULL if an error occurred.
 */
pub unsafe extern "C" fn xml_hash_create(mut size: c_int) -> XmlHashTablePtr {
    xml_init_parser();

    if size <= 0 {
        size = 256;
    }

    let table: XmlHashTablePtr = xml_malloc(size_of::<XmlHashTable>()) as _;
    if !table.is_null() {
        (*table).dict = null_mut();
        (*table).size = size;
        (*table).nb_elems = 0;
        (*table).table = xml_malloc(size as usize * size_of::<XmlHashEntry>()) as _;
        if !(*table).table.is_null() {
            memset(
                (*table).table as _,
                0,
                size as usize * size_of::<XmlHashEntry>(),
            );
            (*table).random_seed = __xml_random();
            return table;
        }
        xml_free(table as _);
    }
    null_mut()
}

/**
 * xmlHashCreateDict:
 * @size: the size of the hash table
 * @dict: a dictionary to use for the hash
 *
 * Create a new xmlHashTablePtr which will use @dict as the internal dictionary
 *
 * Returns the newly created object, or NULL if an error occurred.
 */
pub unsafe extern "C" fn xml_hash_create_dict(size: c_int, dict: XmlDictPtr) -> XmlHashTablePtr {
    let table: XmlHashTablePtr = xml_hash_create(size);
    if !table.is_null() {
        (*table).dict = dict;
        xml_dict_reference(dict);
    }
    table
}

/**
 * xmlHashFree:
 * @table: the hash table
 * @f:  the deallocator function for items in the hash
 *
 * Free the hash @table and its contents. The userdata is
 * deallocated with @f if provided.
 */
pub unsafe extern "C" fn xml_hash_free(table: XmlHashTablePtr, f: Option<XmlHashDeallocator>) {
    let mut iter: XmlHashEntryPtr;
    let mut next: XmlHashEntryPtr;
    let mut inside_table: c_int;
    let mut nb_elems: c_int;

    if table.is_null() {
        return;
    }
    if !(*table).table.is_null() {
        nb_elems = (*table).nb_elems;
        let mut i = 0;
        while i < (*table).size && nb_elems > 0 {
            'to_continue: {
                iter = (*table).table.add(i as usize);
                if (*iter).valid == 0 {
                    break 'to_continue;
                }
                inside_table = 1;
                while !iter.is_null() {
                    next = (*iter).next;
                    if let Some(f) = f {
                        if !(*iter).payload.is_null() {
                            f((*iter).payload, (*iter).name);
                        }
                    }
                    if (*table).dict.is_null() {
                        if !(*iter).name.is_null() {
                            xml_free((*iter).name as _);
                        }
                        if !(*iter).name2.is_null() {
                            xml_free((*iter).name2 as _);
                        }
                        if !(*iter).name3.is_null() {
                            xml_free((*iter).name3 as _);
                        }
                    }
                    (*iter).payload = null_mut();
                    if inside_table == 0 {
                        xml_free(iter as _);
                    }
                    nb_elems -= 1;
                    inside_table = 0;
                    iter = next;
                }
            }
            i += 1;
        }
        xml_free((*table).table as _);
    }
    if !(*table).dict.is_null() {
        xml_dict_free((*table).dict);
    }
    xml_free(table as _);
}

/**
 * xmlHashDefaultDeallocator:
 * @entry: the hash table entry
 * @name: the entry's name
 *
 * Free a hash table entry with xmlFree.
 */
pub unsafe extern "C" fn xml_hash_default_deallocator(entry: *mut c_void, _name: *const XmlChar) {
    xml_free(entry);
}

/*
 * Add a new entry to the hash table.
 */
/**
 * xmlHashAddEntry:
 * @table: the hash table
 * @name: the name of the userdata
 * @userdata: a pointer to the userdata
 *
 * Add the @userdata to the hash @table. This can later be retrieved
 * by using the @name. Duplicate names generate errors.
 *
 * Returns 0 the addition succeeded and -1 in case of error.
 */
pub unsafe extern "C" fn xml_hash_add_entry(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    userdata: *mut c_void,
) -> c_int {
    xml_hash_add_entry3(table, name, null(), null(), userdata)
}

/**
 * xmlHashUpdateEntry:
 * @table: the hash table
 * @name: the name of the userdata
 * @userdata: a pointer to the userdata
 * @f: the deallocator function for replaced item (if any)
 *
 * Add the @userdata to the hash @table. This can later be retrieved
 * by using the @name. Existing entry for this @name will be removed
 * and freed with @f if found.
 *
 * Returns 0 the addition succeeded and -1 in case of error.
 */
pub unsafe extern "C" fn xml_hash_update_entry(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    userdata: *mut c_void,
    f: Option<XmlHashDeallocator>,
) -> c_int {
    xml_hash_update_entry3(table, name, null(), null(), userdata, f)
}

/**
 * xmlHashAddEntry2:
 * @table: the hash table
 * @name: the name of the userdata
 * @name2: a second name of the userdata
 * @userdata: a pointer to the userdata
 *
 * Add the @userdata to the hash @table. This can later be retrieved
 * by using the (@name, @name2) tuple. Duplicate tuples generate errors.
 *
 * Returns 0 the addition succeeded and -1 in case of error.
 */
pub unsafe extern "C" fn xml_hash_add_entry2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    userdata: *mut c_void,
) -> c_int {
    xml_hash_add_entry3(table, name, name2, null(), userdata)
}

/**
 * xmlHashUpdateEntry2:
 * @table: the hash table
 * @name: the name of the userdata
 * @name2: a second name of the userdata
 * @userdata: a pointer to the userdata
 * @f: the deallocator function for replaced item (if any)
 *
 * Add the @userdata to the hash @table. This can later be retrieved
 * by using the (@name, @name2) tuple. Existing entry for this tuple will
 * be removed and freed with @f if found.
 *
 * Returns 0 the addition succeeded and -1 in case of error.
 */
pub unsafe extern "C" fn xml_hash_update_entry2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    userdata: *mut c_void,
    f: Option<XmlHashDeallocator>,
) -> c_int {
    xml_hash_update_entry3(table, name, name2, null(), userdata, f)
}

/*
 * xmlHashComputeKey:
 * Calculate the hash key
 */
unsafe extern "C" fn xml_hash_compute_key(
    table: XmlHashTablePtr,
    mut name: *const XmlChar,
    mut name2: *const XmlChar,
    mut name3: *const XmlChar,
) -> c_ulong {
    let mut value: c_ulong;
    let mut ch: c_ulong;

    value = (*table).random_seed as _;
    if !name.is_null() {
        value += 30 * (*name) as u64;
        while {
            ch = *name as u64;
            name = name.add(1);
            ch != 0
        } {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch);
        }
    }
    value ^= value.wrapping_shl(5).wrapping_add(value.wrapping_shr(3));
    if !name2.is_null() {
        while {
            ch = *name2 as u64;
            name2 = name2.add(1);
            ch != 0
        } {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch);
        }
    }
    value ^= value.wrapping_shl(5).wrapping_add(value.wrapping_shr(3));
    if !name3.is_null() {
        while {
            ch = *name3 as u64;
            name3 = name3.add(1);
            ch != 0
        } {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch);
        }
    }
    value % (*table).size as u64
}

/**
 * xmlHashGrow:
 * @table: the hash table
 * @size: the new size of the hash table
 *
 * resize the hash table
 *
 * Returns 0 in case of success, -1 in case of failure
 */
unsafe extern "C" fn xml_hash_grow(table: XmlHashTablePtr, size: c_int) -> c_int {
    let mut key: c_ulong;
    let mut next: XmlHashEntryPtr;
    let mut iter: XmlHashEntryPtr;

    // #ifdef DEBUG_GROW
    //     c_ulong nbElem = 0;
    // #endif

    if table.is_null() {
        return -1;
    }
    if size < 8 {
        return -1;
    }
    if size > 8 * 2048 {
        return -1;
    }

    let oldsize: c_int = (*table).size;
    let oldtable: *mut XmlHashEntry = (*table).table;
    if oldtable.is_null() {
        return -1;
    }

    (*table).table = xml_malloc(size as usize * size_of::<XmlHashEntry>()) as _;
    if (*table).table.is_null() {
        (*table).table = oldtable;
        return -1;
    }
    memset(
        (*table).table as _,
        0,
        size as usize * size_of::<XmlHashEntry>(),
    );
    (*table).size = size;

    /*	If the two loops are merged, there would be situations where
    a new entry needs to allocated and data copied into it from
    the main table. So instead, we run through the array twice, first
    copying all the elements in the main array (where we can't get
    conflicts) and then the rest, so we only free (and don't allocate)
    */
    for i in 0..oldsize {
        if (*oldtable.add(i as usize)).valid == 0 {
            continue;
        }
        key = xml_hash_compute_key(
            table,
            (*oldtable.add(i as usize)).name,
            (*oldtable.add(i as usize)).name2,
            (*oldtable.add(i as usize)).name3,
        );
        memcpy(
            (*table).table.add(key as usize) as _,
            oldtable.add(i as usize) as _,
            size_of::<XmlHashEntry>(),
        );
        (*(*table).table.add(key as usize)).next = null_mut();
    }

    for i in 0..oldsize {
        iter = (*oldtable.add(i as usize)).next;
        while !iter.is_null() {
            next = (*iter).next;

            /*
             * put back the entry in the new table
             */

            key = xml_hash_compute_key(table, (*iter).name, (*iter).name2, (*iter).name3);
            if (*(*table).table.add(key as usize)).valid == 0 {
                memcpy(
                    (*table).table.add(key as usize) as _,
                    iter as _,
                    size_of::<XmlHashEntry>(),
                );
                (*(*table).table.add(key as usize)).next = null_mut();
                xml_free(iter as _);
            } else {
                (*iter).next = (*(*table).table.add(key as usize)).next;
                (*(*table).table.add(key as usize)).next = iter;
            }

            // #ifdef DEBUG_GROW
            // 	    nbElem++;
            // #endif

            iter = next;
        }
    }

    xml_free(oldtable as _);

    // #ifdef DEBUG_GROW
    //     xmlGenericError(xmlGenericErrorContext,
    // 	    "xmlHashGrow : from %d to %d, %d elems\n", oldsize, size, nbElem);
    // #endif

    0
}

/**
 * xmlHashAddEntry3:
 * @table: the hash table
 * @name: the name of the userdata
 * @name2: a second name of the userdata
 * @name3: a third name of the userdata
 * @userdata: a pointer to the userdata
 *
 * Add the @userdata to the hash @table. This can later be retrieved
 * by using the tuple (@name, @name2, @name3). Duplicate entries generate
 * errors.
 *
 * Returns 0 the addition succeeded and -1 in case of error.
 */
pub unsafe extern "C" fn xml_hash_add_entry3(
    table: XmlHashTablePtr,
    mut name: *const XmlChar,
    mut name2: *const XmlChar,
    mut name3: *const XmlChar,
    userdata: *mut c_void,
) -> c_int {
    let mut len: c_ulong = 0;
    let entry: XmlHashEntryPtr;
    let mut insert: XmlHashEntryPtr;

    if table.is_null() || name.is_null() {
        return -1;
    }

    /*
     * If using a dict internalize if needed
     */
    if !(*table).dict.is_null() {
        if xml_dict_owns((*table).dict, name) == 0 {
            name = xml_dict_lookup((*table).dict, name, -1);
            if name.is_null() {
                return -1;
            }
        }
        if !name2.is_null() && xml_dict_owns((*table).dict, name2) == 0 {
            name2 = xml_dict_lookup((*table).dict, name2, -1);
            if name2.is_null() {
                return -1;
            }
        }
        if !name3.is_null() && xml_dict_owns((*table).dict, name3) == 0 {
            name3 = xml_dict_lookup((*table).dict, name3, -1);
            if name3.is_null() {
                return -1;
            }
        }
    }

    /*
     * Check for duplicate and insertion location.
     */
    let key: c_ulong = xml_hash_compute_key(table, name, name2, name3);
    if (*(*table).table.add(key as usize)).valid == 0 {
        insert = null_mut();
    } else if !(*table).dict.is_null() {
        insert = (*table).table.add(key as usize);
        while !(*insert).next.is_null() {
            if (*insert).name == name as _
                && (*insert).name2 == name2 as _
                && (*insert).name3 == name3 as _
            {
                return -1;
            }
            len += 1;
            insert = (*insert).next;
        }
        if (*insert).name == name as _
            && (*insert).name2 == name2 as _
            && (*insert).name3 == name3 as _
        {
            return -1;
        }
    } else {
        insert = (*table).table.add(key as usize);
        while !(*insert).next.is_null() {
            if xml_str_equal((*insert).name, name)
                && xml_str_equal((*insert).name2, name2)
                && xml_str_equal((*insert).name3, name3)
            {
                return -1;
            }
            len += 1;
            insert = (*insert).next;
        }
        if xml_str_equal((*insert).name, name)
            && xml_str_equal((*insert).name2, name2)
            && xml_str_equal((*insert).name3, name3)
        {
            return -1;
        }
    }

    if insert.is_null() {
        entry = (*table).table.add(key as usize);
    } else {
        entry = xml_malloc(size_of::<XmlHashEntry>()) as _;
        if entry.is_null() {
            return -1;
        }
    }

    'error: {
        if !(*table).dict.is_null() {
            (*entry).name = name as *mut XmlChar;
            (*entry).name2 = name2 as *mut XmlChar;
            (*entry).name3 = name3 as *mut XmlChar;
        } else {
            (*entry).name = xml_strdup(name);
            if (*entry).name.is_null() {
                (*entry).name2 = null_mut();
                break 'error;
            }
            if name2.is_null() {
                (*entry).name2 = null_mut();
            } else {
                (*entry).name2 = xml_strdup(name2);
                if (*entry).name2.is_null() {
                    break 'error;
                }
            }
            if name3.is_null() {
                (*entry).name3 = null_mut();
            } else {
                (*entry).name3 = xml_strdup(name3);
                if (*entry).name3.is_null() {
                    break 'error;
                }
            }
        }
        (*entry).payload = userdata;
        (*entry).next = null_mut();
        (*entry).valid = 1;

        if !insert.is_null() {
            (*insert).next = entry;
        }

        (*table).nb_elems += 1;

        if len > MAX_HASH_LEN as u64 {
            xml_hash_grow(table, MAX_HASH_LEN as i32 * (*table).size);
        }

        return 0;
    }

    // error:
    xml_free((*entry).name2 as _);
    xml_free((*entry).name as _);
    if !insert.is_null() {
        xml_free(entry as _);
    }
    -1
}

/**
 * xmlHashUpdateEntry3:
 * @table: the hash table
 * @name: the name of the userdata
 * @name2: a second name of the userdata
 * @name3: a third name of the userdata
 * @userdata: a pointer to the userdata
 * @f: the deallocator function for replaced item (if any)
 *
 * Add the @userdata to the hash @table. This can later be retrieved
 * by using the tuple (@name, @name2, @name3). Existing entry for this tuple
 * will be removed and freed with @f if found.
 *
 * Returns 0 the addition succeeded and -1 in case of error.
 */
pub unsafe extern "C" fn xml_hash_update_entry3(
    table: XmlHashTablePtr,
    mut name: *const XmlChar,
    mut name2: *const XmlChar,
    mut name3: *const XmlChar,
    userdata: *mut c_void,
    f: Option<XmlHashDeallocator>,
) -> c_int {
    let entry: XmlHashEntryPtr;
    let mut insert: XmlHashEntryPtr;

    if table.is_null() || name.is_null() {
        return -1;
    }

    /*
     * If using a dict internalize if needed
     */
    if !(*table).dict.is_null() {
        if xml_dict_owns((*table).dict, name) == 0 {
            name = xml_dict_lookup((*table).dict, name, -1);
            if name.is_null() {
                return -1;
            }
        }
        if !name2.is_null() && xml_dict_owns((*table).dict, name2) == 0 {
            name2 = xml_dict_lookup((*table).dict, name2, -1);
            if name2.is_null() {
                return -1;
            }
        }
        if !name3.is_null() && xml_dict_owns((*table).dict, name3) == 0 {
            name3 = xml_dict_lookup((*table).dict, name3, -1);
            if name3.is_null() {
                return -1;
            }
        }
    }

    /*
     * Check for duplicate and insertion location.
     */
    let key: c_ulong = xml_hash_compute_key(table, name, name2, name3);
    if (*(*table).table.add(key as usize)).valid == 0 {
        insert = null_mut();
    } else if !(*table).dict.is_null() {
        insert = (*table).table.add(key as usize);
        while !(*insert).next.is_null() {
            if (*insert).name == name as _
                && (*insert).name2 == name2 as _
                && (*insert).name3 == name3 as _
            {
                if let Some(f) = f {
                    f((*insert).payload, (*insert).name);
                }
                (*insert).payload = userdata;
                return 0;
            }
            insert = (*insert).next;
        }
        if (*insert).name == name as _
            && (*insert).name2 == name2 as _
            && (*insert).name3 == name3 as _
        {
            if let Some(f) = f {
                f((*insert).payload, (*insert).name);
            }
            (*insert).payload = userdata;
            return 0;
        }
    } else {
        insert = (*table).table.add(key as usize);
        while !(*insert).next.is_null() {
            if xml_str_equal((*insert).name, name)
                && xml_str_equal((*insert).name2, name2)
                && xml_str_equal((*insert).name3, name3)
            {
                if let Some(f) = f.as_ref() {
                    f((*insert).payload, (*insert).name);
                }
                (*insert).payload = userdata;
                return 0;
            }
            insert = (*insert).next;
        }
        if xml_str_equal((*insert).name, name)
            && xml_str_equal((*insert).name2, name2)
            && xml_str_equal((*insert).name3, name3)
        {
            if let Some(f) = f.as_ref() {
                f((*insert).payload, (*insert).name);
            }
            (*insert).payload = userdata;
            return 0;
        }
    }

    if insert.is_null() {
        entry = (*table).table.add(key as usize);
    } else {
        entry = xml_malloc(size_of::<XmlHashEntry>()) as _;
        if entry.is_null() {
            return -1;
        }
    }

    'error: {
        if !(*table).dict.is_null() {
            (*entry).name = name as *mut XmlChar;
            (*entry).name2 = name2 as *mut XmlChar;
            (*entry).name3 = name3 as *mut XmlChar;
        } else {
            (*entry).name = xml_strdup(name);
            if (*entry).name.is_null() {
                (*entry).name2 = null_mut();
                break 'error;
            }
            if name2.is_null() {
                (*entry).name2 = null_mut();
            } else {
                (*entry).name2 = xml_strdup(name2);
                if (*entry).name2.is_null() {
                    break 'error;
                }
            }
            if name3.is_null() {
                (*entry).name3 = null_mut();
            } else {
                (*entry).name3 = xml_strdup(name3);
                if (*entry).name3.is_null() {
                    break 'error;
                }
            }
        }
        (*entry).payload = userdata;
        (*entry).next = null_mut();
        (*entry).valid = 1;
        (*table).nb_elems += 1;

        if !insert.is_null() {
            (*insert).next = entry;
        }
        return 0;
    }

    // error:
    xml_free((*entry).name2 as _);
    xml_free((*entry).name as _);
    if !insert.is_null() {
        xml_free(entry as _);
    }
    -1
}

/*
 * Remove an entry from the hash table.
 */
/**
 * xmlHashRemoveEntry:
 * @table: the hash table
 * @name: the name of the userdata
 * @f: the deallocator function for removed item (if any)
 *
 * Find the userdata specified by the @name and remove
 * it from the hash @table. Existing userdata for this tuple will be removed
 * and freed with @f.
 *
 * Returns 0 if the removal succeeded and -1 in case of error or not found.
 */
pub unsafe extern "C" fn xml_hash_remove_entry(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    f: Option<XmlHashDeallocator>,
) -> c_int {
    xml_hash_remove_entry3(table, name, null(), null(), f)
}

/**
 * xmlHashRemoveEntry2:
 * @table: the hash table
 * @name: the name of the userdata
 * @name2: a second name of the userdata
 * @f: the deallocator function for removed item (if any)
 *
 * Find the userdata specified by the (@name, @name2) tuple and remove
 * it from the hash @table. Existing userdata for this tuple will be removed
 * and freed with @f.
 *
 * Returns 0 if the removal succeeded and -1 in case of error or not found.
 */
pub unsafe extern "C" fn xml_hash_remove_entry2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    f: Option<XmlHashDeallocator>,
) -> c_int {
    xml_hash_remove_entry3(table, name, name2, null(), f)
}

/**
 * xmlHashRemoveEntry3:
 * @table: the hash table
 * @name: the name of the userdata
 * @name2: a second name of the userdata
 * @name3: a third name of the userdata
 * @f: the deallocator function for removed item (if any)
 *
 * Find the userdata specified by the (@name, @name2, @name3) tuple and remove
 * it from the hash @table. Existing userdata for this tuple will be removed
 * and freed with @f.
 *
 * Returns 0 if the removal succeeded and -1 in case of error or not found.
 */
pub unsafe extern "C" fn xml_hash_remove_entry3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
    f: Option<XmlHashDeallocator>,
) -> c_int {
    let mut entry: XmlHashEntryPtr;
    let mut prev: XmlHashEntryPtr = null_mut();

    if table.is_null() || name.is_null() {
        return -1;
    }

    let key: c_ulong = xml_hash_compute_key(table, name, name2, name3);
    if (*(*table).table.add(key as usize)).valid == 0 {
        -1
    } else {
        entry = (*table).table.add(key as usize);
        while !entry.is_null() {
            if xml_str_equal((*entry).name, name)
                && xml_str_equal((*entry).name2, name2)
                && xml_str_equal((*entry).name3, name3)
            {
                if let Some(f) = f.as_ref() {
                    if !(*entry).payload.is_null() {
                        f((*entry).payload, (*entry).name);
                    }
                }
                (*entry).payload = null_mut();
                if (*table).dict.is_null() {
                    if !(*entry).name.is_null() {
                        xml_free((*entry).name as _);
                    }
                    if !(*entry).name2.is_null() {
                        xml_free((*entry).name2 as _);
                    }
                    if !(*entry).name3.is_null() {
                        xml_free((*entry).name3 as _);
                    }
                }
                if !prev.is_null() {
                    (*prev).next = (*entry).next;
                    xml_free(entry as _);
                } else if (*entry).next.is_null() {
                    (*entry).valid = 0;
                } else {
                    entry = (*entry).next;
                    memcpy(
                        (*table).table.add(key as usize) as _,
                        entry as _,
                        size_of::<XmlHashEntry>(),
                    );
                    xml_free(entry as _);
                }
                (*table).nb_elems -= 1;
                return 0;
            }
            prev = entry;
            entry = (*entry).next;
        }
        -1
    }
}

/*
 * Retrieve the userdata.
 */
/**
 * xmlHashLookup:
 * @table: the hash table
 * @name: the name of the userdata
 *
 * Find the userdata specified by the @name.
 *
 * Returns the pointer to the userdata
 */
pub unsafe extern "C" fn xml_hash_lookup(
    table: XmlHashTablePtr,
    name: *const XmlChar,
) -> *mut c_void {
    xml_hash_lookup3(table, name, null(), null())
}

/**
 * xmlHashLookup2:
 * @table: the hash table
 * @name: the name of the userdata
 * @name2: a second name of the userdata
 *
 * Find the userdata specified by the (@name, @name2) tuple.
 *
 * Returns the pointer to the userdata
 */
pub unsafe extern "C" fn xml_hash_lookup2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
) -> *mut c_void {
    xml_hash_lookup3(table, name, name2, null())
}

/**
 * xmlHashLookup3:
 * @table: the hash table
 * @name: the name of the userdata
 * @name2: a second name of the userdata
 * @name3: a third name of the userdata
 *
 * Find the userdata specified by the (@name, @name2, @name3) tuple.
 *
 * Returns the a pointer to the userdata
 */
pub unsafe extern "C" fn xml_hash_lookup3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
) -> *mut c_void {
    let mut entry: XmlHashEntryPtr;

    if table.is_null() {
        return null_mut();
    }
    if name.is_null() {
        return null_mut();
    }
    let key: c_ulong = xml_hash_compute_key(table, name, name2, name3);
    if (*(*table).table.add(key as usize)).valid == 0 {
        return null_mut();
    }
    if !(*table).dict.is_null() {
        entry = (*table).table.add(key as usize);
        while !entry.is_null() {
            if (*entry).name == name as _
                && (*entry).name2 == name2 as _
                && (*entry).name3 == name3 as _
            {
                return (*entry).payload;
            }
            entry = (*entry).next;
        }
    }
    entry = (*table).table.add(key as usize);
    while !entry.is_null() {
        if xml_str_equal((*entry).name, name)
            && xml_str_equal((*entry).name2, name2)
            && xml_str_equal((*entry).name3, name3)
        {
            return (*entry).payload;
        }
        entry = (*entry).next;
    }
    null_mut()
}

/**
 * xmlHashQLookup:
 * @table: the hash table
 * @prefix: the prefix of the userdata
 * @name: the name of the userdata
 *
 * Find the userdata specified by the QName @prefix:@name/@name.
 *
 * Returns the pointer to the userdata
 */
pub unsafe extern "C" fn xml_hash_qlookup(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    prefix: *const XmlChar,
) -> *mut c_void {
    xml_hash_qlookup3(table, prefix, name, null(), null(), null(), null())
}

/**
 * xmlHashQLookup2:
 * @table: the hash table
 * @prefix: the prefix of the userdata
 * @name: the name of the userdata
 * @prefix2: the second prefix of the userdata
 * @name2: a second name of the userdata
 *
 * Find the userdata specified by the QNames tuple
 *
 * Returns the pointer to the userdata
 */
pub unsafe extern "C" fn xml_hash_qlookup2(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    prefix: *const XmlChar,
    name2: *const XmlChar,
    prefix2: *const XmlChar,
) -> *mut c_void {
    xml_hash_qlookup3(table, prefix, name, prefix2, name2, null(), null())
}

unsafe extern "C" fn xml_hash_compute_qkey(
    table: XmlHashTablePtr,
    mut prefix: *const XmlChar,
    mut name: *const XmlChar,
    mut prefix2: *const XmlChar,
    mut name2: *const XmlChar,
    mut prefix3: *const XmlChar,
    mut name3: *const XmlChar,
) -> c_ulong {
    let mut value: c_ulong;
    let mut ch: c_ulong;

    value = (*table).random_seed as _;
    if !prefix.is_null() {
        value += 30 * (*prefix) as u64;
    } else {
        value += 30 * (*name) as u64;
    }

    if !prefix.is_null() {
        while {
            ch = *prefix as u64;
            prefix = prefix.add(1);
            ch != 0
        } {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch);
        }
        value ^= value
            .wrapping_shl(5)
            .wrapping_add(value.wrapping_shr(3))
            .wrapping_add(b':' as c_ulong);
    }
    if !name.is_null() {
        while {
            ch = *name as u64;
            name = name.add(1);
            ch != 0
        } {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch);
        }
    }
    value ^= value.wrapping_shl(5).wrapping_add(value.wrapping_shr(3));
    if !prefix2.is_null() {
        while {
            ch = *prefix2 as u64;
            prefix2 = prefix2.add(1);
            ch != 0
        } {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch);
        }
        value ^= value
            .wrapping_shl(5)
            .wrapping_add(value.wrapping_shr(3))
            .wrapping_add(b':' as c_ulong);
    }
    if !name2.is_null() {
        while {
            ch = *name2 as u64;
            name2 = name2.add(1);
            ch != 0
        } {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch);
        }
    }
    value ^= value.wrapping_shl(5).wrapping_add(value.wrapping_shr(3));
    if !prefix3.is_null() {
        while {
            ch = *prefix3 as u64;
            prefix3 = prefix3.add(1);
            ch != 0
        } {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch);
        }
        value ^= value
            .wrapping_shl(5)
            .wrapping_shr(3)
            .wrapping_add(b':' as c_ulong);
    }
    if !name3.is_null() {
        while {
            ch = *name3 as u64;
            name3 = name3.add(1);
            ch != 0
        } {
            value ^= value
                .wrapping_shl(5)
                .wrapping_add(value.wrapping_shr(3))
                .wrapping_add(ch);
        }
    }
    value % (*table).size as u64
}

/**
 * xmlHashQLookup3:
 * @table: the hash table
 * @prefix: the prefix of the userdata
 * @name: the name of the userdata
 * @prefix2: the second prefix of the userdata
 * @name2: a second name of the userdata
 * @prefix3: the third prefix of the userdata
 * @name3: a third name of the userdata
 *
 * Find the userdata specified by the (@name, @name2, @name3) tuple.
 *
 * Returns the a pointer to the userdata
 */
pub unsafe extern "C" fn xml_hash_qlookup3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    prefix: *const XmlChar,
    name2: *const XmlChar,
    prefix2: *const XmlChar,
    name3: *const XmlChar,
    prefix3: *const XmlChar,
) -> *mut c_void {
    let mut entry: XmlHashEntryPtr;

    if table.is_null() {
        return null_mut();
    }
    if name.is_null() {
        return null_mut();
    }
    let key: c_ulong = xml_hash_compute_qkey(table, prefix, name, prefix2, name2, prefix3, name3);
    if (*(*table).table.add(key as usize)).valid == 0 {
        return null_mut();
    }
    entry = (*table).table.add(key as usize);
    while !entry.is_null() {
        if xml_str_qequal(prefix, name, (*entry).name)
            && xml_str_qequal(prefix2, name2, (*entry).name2)
            && xml_str_qequal(prefix3, name3, (*entry).name3)
        {
            return (*entry).payload;
        }
        entry = (*entry).next;
    }
    null_mut()
}

/*
 * Helpers.
 */
/**
 * xmlHashCopy:
 * @table: the hash table
 * @f:  the copier function for items in the hash
 *
 * Scan the hash @table and applied @f to each value.
 *
 * Returns the new table or NULL in case of error.
 */
pub unsafe extern "C" fn xml_hash_copy(
    table: XmlHashTablePtr,
    f: Option<XmlHashCopier>,
) -> XmlHashTablePtr {
    let mut iter: XmlHashEntryPtr;
    let mut next: XmlHashEntryPtr;

    if table.is_null() {
        return null_mut();
    }
    if f.is_none() {
        return null_mut();
    }
    let f = f.unwrap();

    let ret: XmlHashTablePtr = xml_hash_create((*table).size);
    if ret.is_null() {
        return null_mut();
    }

    if !(*table).table.is_null() {
        for i in 0..(*table).size {
            if (*(*table).table.add(i as usize)).valid == 0 {
                continue;
            }
            iter = (*table).table.add(i as usize);
            while !iter.is_null() {
                next = (*iter).next;
                xml_hash_add_entry3(
                    ret,
                    (*iter).name,
                    (*iter).name2,
                    (*iter).name3,
                    f((*iter).payload, (*iter).name),
                );
                iter = next;
            }
        }
    }
    (*ret).nb_elems = (*table).nb_elems;
    ret
}

/**
 * xmlHashSize:
 * @table: the hash table
 *
 * Query the number of elements installed in the hash @table.
 *
 * Returns the number of elements in the hash table or
 * -1 in case of error
 */
pub unsafe extern "C" fn xml_hash_size(table: XmlHashTablePtr) -> c_int {
    if table.is_null() {
        return -1;
    }
    (*table).nb_elems
}

struct StubData {
    hashscanner: Option<XmlHashScanner>,
    data: *mut c_void,
}

unsafe extern "C" fn stub_hash_scanner_full(
    payload: *mut c_void,
    data: *mut c_void,
    name: *const XmlChar,
    _name2: *const XmlChar,
    _name3: *const XmlChar,
) {
    let stubdata: *mut StubData = data as *mut StubData;
    ((*stubdata).hashscanner.unwrap())(payload, (*stubdata).data, name as _);
}

/**
 * xmlHashScan:
 * @table: the hash table
 * @f:  the scanner function for items in the hash
 * @data:  extra data passed to f
 *
 * Scan the hash @table and applied @f to each value.
 */
pub unsafe extern "C" fn xml_hash_scan(
    table: XmlHashTablePtr,
    f: XmlHashScanner,
    data: *mut c_void,
) {
    let mut stubdata: StubData = unsafe { zeroed() };
    stubdata.data = data;
    stubdata.hashscanner = Some(f);
    xml_hash_scan_full(
        table,
        Some(stub_hash_scanner_full),
        addr_of_mut!(stubdata) as _,
    );
}

/**
 * xmlHashScan3:
 * @table: the hash table
 * @name: the name of the userdata or NULL
 * @name2: a second name of the userdata or NULL
 * @name3: a third name of the userdata or NULL
 * @f:  the scanner function for items in the hash
 * @data:  extra data passed to f
 *
 * Scan the hash @table and applied @f to each value matching
 * (@name, @name2, @name3) tuple. If one of the names is null,
 * the comparison is considered to match.
 */
pub unsafe extern "C" fn xml_hash_scan3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
    f: XmlHashScanner,
    data: *mut c_void,
) {
    let mut stubdata: StubData = unsafe { zeroed() };
    stubdata.data = data;
    stubdata.hashscanner = Some(f);
    xml_hash_scan_full3(
        table,
        name,
        name2,
        name3,
        Some(stub_hash_scanner_full),
        addr_of_mut!(stubdata) as _,
    );
}

/**
 * xmlHashScanFull:
 * @table: the hash table
 * @f:  the scanner function for items in the hash
 * @data:  extra data passed to f
 *
 * Scan the hash @table and applied @f to each value.
 */
pub unsafe extern "C" fn xml_hash_scan_full(
    table: XmlHashTablePtr,
    f: Option<XmlHashScannerFull>,
    data: *mut c_void,
) {
    let mut nb: c_int;
    let mut iter: XmlHashEntryPtr;
    let mut next: XmlHashEntryPtr;

    if table.is_null() {
        return;
    }
    if f.is_none() {
        return;
    }

    if !(*table).table.is_null() {
        for i in 0..(*table).size {
            if (*(*table).table.add(i as usize)).valid == 0 {
                continue;
            }
            iter = (*table).table.add(i as usize);
            while !iter.is_null() {
                next = (*iter).next;
                nb = (*table).nb_elems;
                if let Some(f) = f {
                    if !(*iter).payload.is_null() {
                        f(
                            (*iter).payload,
                            data,
                            (*iter).name,
                            (*iter).name2,
                            (*iter).name3,
                        );
                    }
                }
                if nb != (*table).nb_elems {
                    /* table was modified by the callback, be careful */
                    if iter == (*table).table.add(i as usize) {
                        if (*(*table).table.add(i as usize)).valid == 0 {
                            iter = null_mut();
                        }
                        if (*(*table).table.add(i as usize)).next != next {
                            iter = (*table).table.add(i as usize);
                        }
                    } else {
                        iter = next;
                    }
                } else {
                    iter = next;
                }
            }
        }
    }
}

/**
 * xmlHashScanFull3:
 * @table: the hash table
 * @name: the name of the userdata or NULL
 * @name2: a second name of the userdata or NULL
 * @name3: a third name of the userdata or NULL
 * @f:  the scanner function for items in the hash
 * @data:  extra data passed to f
 *
 * Scan the hash @table and applied @f to each value matching
 * (@name, @name2, @name3) tuple. If one of the names is null,
 * the comparison is considered to match.
 */
pub unsafe extern "C" fn xml_hash_scan_full3(
    table: XmlHashTablePtr,
    name: *const XmlChar,
    name2: *const XmlChar,
    name3: *const XmlChar,
    f: Option<XmlHashScannerFull>,
    data: *mut c_void,
) {
    let mut iter: XmlHashEntryPtr;
    let mut next: XmlHashEntryPtr;

    if table.is_null() {
        return;
    }
    if f.is_none() {
        return;
    }
    let f = f.unwrap();

    if !(*table).table.is_null() {
        for i in 0..(*table).size {
            if (*(*table).table.add(i as usize)).valid == 0 {
                continue;
            }
            iter = (*table).table.add(i as usize);
            while !iter.is_null() {
                next = (*iter).next;
                if (name.is_null() || xml_str_equal(name, (*iter).name))
                    && (name2.is_null() || xml_str_equal(name2, (*iter).name2))
                    && (name3.is_null() || xml_str_equal(name3, (*iter).name3))
                    && !(*iter).payload.is_null()
                {
                    f(
                        (*iter).payload,
                        data,
                        (*iter).name,
                        (*iter).name2,
                        (*iter).name3,
                    );
                }
                iter = next;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        libxml::{xmlerror::xml_reset_last_error, xmlmemory::xml_mem_blocks},
        test_util::*,
    };

    use super::*;

    #[test]
    fn test_xml_hash_add_entry() {
        unsafe {
            let mut leaks = 0;

            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_userdata in 0..GEN_NB_USERDATA {
                        let mem_base = xml_mem_blocks();
                        let table = gen_xml_hash_table_ptr(n_table, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let userdata = gen_userdata(n_userdata, 2);

                        let ret_val = xml_hash_add_entry(table, name, userdata);
                        desret_int(ret_val);
                        des_xml_hash_table_ptr(n_table, table, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_userdata(n_userdata, userdata, 2);
                        xml_reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlHashAddEntry",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_table);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_userdata);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlHashAddEntry()");
        }
    }

    #[test]
    fn test_xml_hash_add_entry2() {
        unsafe {
            let mut leaks = 0;

            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_userdata in 0..GEN_NB_USERDATA {
                            let mem_base = xml_mem_blocks();
                            let table = gen_xml_hash_table_ptr(n_table, 0);
                            let name = gen_const_xml_char_ptr(n_name, 1);
                            let name2 = gen_const_xml_char_ptr(n_name2, 2);
                            let userdata = gen_userdata(n_userdata, 3);

                            let ret_val = xml_hash_add_entry2(table, name, name2, userdata);
                            desret_int(ret_val);
                            des_xml_hash_table_ptr(n_table, table, 0);
                            des_const_xml_char_ptr(n_name, name, 1);
                            des_const_xml_char_ptr(n_name2, name2, 2);
                            des_userdata(n_userdata, userdata, 3);
                            xml_reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlHashAddEntry2",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_table);
                                eprint!(" {}", n_name);
                                eprint!(" {}", n_name2);
                                eprintln!(" {}", n_userdata);
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlHashAddEntry2()");
        }
    }

    #[test]
    fn test_xml_hash_add_entry3() {
        unsafe {
            let mut leaks = 0;

            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_name3 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_userdata in 0..GEN_NB_USERDATA {
                                let mem_base = xml_mem_blocks();
                                let table = gen_xml_hash_table_ptr(n_table, 0);
                                let name = gen_const_xml_char_ptr(n_name, 1);
                                let name2 = gen_const_xml_char_ptr(n_name2, 2);
                                let name3 = gen_const_xml_char_ptr(n_name3, 3);
                                let userdata = gen_userdata(n_userdata, 4);

                                let ret_val =
                                    xml_hash_add_entry3(table, name, name2, name3, userdata);
                                desret_int(ret_val);
                                des_xml_hash_table_ptr(n_table, table, 0);
                                des_const_xml_char_ptr(n_name, name, 1);
                                des_const_xml_char_ptr(n_name2, name2, 2);
                                des_const_xml_char_ptr(n_name3, name3, 3);
                                des_userdata(n_userdata, userdata, 4);
                                xml_reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlHashAddEntry3",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_table);
                                    eprint!(" {}", n_name);
                                    eprint!(" {}", n_name2);
                                    eprint!(" {}", n_name3);
                                    eprintln!(" {}", n_userdata);
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlHashAddEntry3()");
        }
    }

    #[test]
    fn test_xml_hash_copy() {

        /* missing type support */
    }

    #[test]
    fn test_xml_hash_create() {

        /* missing type support */
    }

    #[test]
    fn test_xml_hash_create_dict() {

        /* missing type support */
    }

    #[test]
    fn test_xml_hash_default_deallocator() {
        unsafe {
            let mut leaks = 0;
            for n_entry in 0..GEN_NB_VOID_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let entry = gen_void_ptr(n_entry, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    xml_hash_default_deallocator(entry, name);
                    des_void_ptr(n_entry, entry, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlHashDefaultDeallocator",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_entry);
                        eprintln!(" {}", n_name);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlHashDefaultDeallocator()"
            );
        }
    }

    #[test]
    fn test_xml_hash_lookup() {
        unsafe {
            let mut leaks = 0;
            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let table = gen_xml_hash_table_ptr(n_table, 0);
                    let name = gen_const_xml_char_ptr(n_name, 1);

                    let ret_val = xml_hash_lookup(table, name);
                    desret_void_ptr(ret_val);
                    des_xml_hash_table_ptr(n_table, table, 0);
                    des_const_xml_char_ptr(n_name, name, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlHashLookup",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_table);
                        eprintln!(" {}", n_name);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlHashLookup()");
        }
    }

    #[test]
    fn test_xml_hash_lookup2() {
        unsafe {
            let mut leaks = 0;
            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let table = gen_xml_hash_table_ptr(n_table, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let name2 = gen_const_xml_char_ptr(n_name2, 2);

                        let ret_val = xml_hash_lookup2(table, name, name2);
                        desret_void_ptr(ret_val);
                        des_xml_hash_table_ptr(n_table, table, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_const_xml_char_ptr(n_name2, name2, 2);
                        xml_reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlHashLookup2",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_table);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_name2);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlHashLookup2()");
        }
    }

    #[test]
    fn test_xml_hash_lookup3() {
        unsafe {
            let mut leaks = 0;

            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_name3 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            let mem_base = xml_mem_blocks();
                            let table = gen_xml_hash_table_ptr(n_table, 0);
                            let name = gen_const_xml_char_ptr(n_name, 1);
                            let name2 = gen_const_xml_char_ptr(n_name2, 2);
                            let name3 = gen_const_xml_char_ptr(n_name3, 3);

                            let ret_val = xml_hash_lookup3(table, name, name2, name3);
                            desret_void_ptr(ret_val);
                            des_xml_hash_table_ptr(n_table, table, 0);
                            des_const_xml_char_ptr(n_name, name, 1);
                            des_const_xml_char_ptr(n_name2, name2, 2);
                            des_const_xml_char_ptr(n_name3, name3, 3);
                            xml_reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlHashLookup3",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_table);
                                eprint!(" {}", n_name);
                                eprint!(" {}", n_name2);
                                eprintln!(" {}", n_name3);
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlHashLookup3()");
        }
    }

    #[test]
    fn test_xml_hash_qlookup() {
        unsafe {
            let mut leaks = 0;

            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        let mem_base = xml_mem_blocks();
                        let table = gen_xml_hash_table_ptr(n_table, 0);
                        let prefix = gen_const_xml_char_ptr(n_prefix, 1);
                        let name = gen_const_xml_char_ptr(n_name, 2);

                        let ret_val = xml_hash_qlookup(table, prefix, name);
                        desret_void_ptr(ret_val);
                        des_xml_hash_table_ptr(n_table, table, 0);
                        des_const_xml_char_ptr(n_prefix, prefix, 1);
                        des_const_xml_char_ptr(n_name, name, 2);
                        xml_reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlHashQLookup",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_table);
                            eprint!(" {}", n_prefix);
                            eprintln!(" {}", n_name);
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlHashQLookup()");
        }
    }

    #[test]
    fn test_xml_hash_qlookup2() {
        unsafe {
            let mut leaks = 0;

            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_prefix2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_name2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                let mem_base = xml_mem_blocks();
                                let table = gen_xml_hash_table_ptr(n_table, 0);
                                let prefix = gen_const_xml_char_ptr(n_prefix, 1);
                                let name = gen_const_xml_char_ptr(n_name, 2);
                                let prefix2 = gen_const_xml_char_ptr(n_prefix2, 3);
                                let name2 = gen_const_xml_char_ptr(n_name2, 4);

                                let ret_val =
                                    xml_hash_qlookup2(table, prefix, name, prefix2, name2);
                                desret_void_ptr(ret_val);
                                des_xml_hash_table_ptr(n_table, table, 0);
                                des_const_xml_char_ptr(n_prefix, prefix, 1);
                                des_const_xml_char_ptr(n_name, name, 2);
                                des_const_xml_char_ptr(n_prefix2, prefix2, 3);
                                des_const_xml_char_ptr(n_name2, name2, 4);
                                xml_reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlHashQLookup2",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_table);
                                    eprint!(" {}", n_prefix);
                                    eprint!(" {}", n_name);
                                    eprint!(" {}", n_prefix2);
                                    eprintln!(" {}", n_name2);
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlHashQLookup2()");
        }
    }

    #[test]
    fn test_xml_hash_qlookup3() {
        unsafe {
            let mut leaks = 0;

            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_prefix in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_prefix2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_name2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                for n_prefix3 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                    for n_name3 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                                        let mem_base = xml_mem_blocks();
                                        let table = gen_xml_hash_table_ptr(n_table, 0);
                                        let prefix = gen_const_xml_char_ptr(n_prefix, 1);
                                        let name = gen_const_xml_char_ptr(n_name, 2);
                                        let prefix2 = gen_const_xml_char_ptr(n_prefix2, 3);
                                        let name2 = gen_const_xml_char_ptr(n_name2, 4);
                                        let prefix3 = gen_const_xml_char_ptr(n_prefix3, 5);
                                        let name3 = gen_const_xml_char_ptr(n_name3, 6);

                                        let ret_val = xml_hash_qlookup3(
                                            table, prefix, name, prefix2, name2, prefix3, name3,
                                        );
                                        desret_void_ptr(ret_val);
                                        des_xml_hash_table_ptr(n_table, table, 0);
                                        des_const_xml_char_ptr(n_prefix, prefix, 1);
                                        des_const_xml_char_ptr(n_name, name, 2);
                                        des_const_xml_char_ptr(n_prefix2, prefix2, 3);
                                        des_const_xml_char_ptr(n_name2, name2, 4);
                                        des_const_xml_char_ptr(n_prefix3, prefix3, 5);
                                        des_const_xml_char_ptr(n_name3, name3, 6);
                                        xml_reset_last_error();
                                        if mem_base != xml_mem_blocks() {
                                            leaks += 1;
                                            eprint!(
                                                "Leak of {} blocks found in xmlHashQLookup3",
                                                xml_mem_blocks() - mem_base
                                            );
                                            eprint!(" {}", n_table);
                                            eprint!(" {}", n_prefix);
                                            eprint!(" {}", n_name);
                                            eprint!(" {}", n_prefix2);
                                            eprint!(" {}", n_name2);
                                            eprint!(" {}", n_prefix3);
                                            eprintln!(" {}", n_name3);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlHashQLookup3()");
        }
    }

    #[test]
    fn test_xml_hash_remove_entry() {
        unsafe {
            let mut leaks = 0;
            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_f in 0..GEN_NB_XML_HASH_DEALLOCATOR {
                        let mem_base = xml_mem_blocks();
                        let table = gen_xml_hash_table_ptr(n_table, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let f = gen_xml_hash_deallocator(n_f, 2);

                        let ret_val = xml_hash_remove_entry(table, name, f);
                        desret_int(ret_val);
                        des_xml_hash_table_ptr(n_table, table, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_xml_hash_deallocator(n_f, f, 2);
                        xml_reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlHashRemoveEntry",
                                xml_mem_blocks() - mem_base
                            );
                            eprint!(" {}", n_table);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_f);
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlHashRemoveEntry()"
            );
        }
    }

    #[test]
    fn test_xml_hash_remove_entry2() {
        unsafe {
            let mut leaks = 0;
            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_f in 0..GEN_NB_XML_HASH_DEALLOCATOR {
                            let mem_base = xml_mem_blocks();
                            let table = gen_xml_hash_table_ptr(n_table, 0);
                            let name = gen_const_xml_char_ptr(n_name, 1);
                            let name2 = gen_const_xml_char_ptr(n_name2, 2);
                            let f = gen_xml_hash_deallocator(n_f, 3);

                            let ret_val = xml_hash_remove_entry2(table, name, name2, f);
                            desret_int(ret_val);
                            des_xml_hash_table_ptr(n_table, table, 0);
                            des_const_xml_char_ptr(n_name, name, 1);
                            des_const_xml_char_ptr(n_name2, name2, 2);
                            des_xml_hash_deallocator(n_f, f, 3);
                            xml_reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlHashRemoveEntry2",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_table);
                                eprint!(" {}", n_name);
                                eprint!(" {}", n_name2);
                                eprintln!(" {}", n_f);
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlHashRemoveEntry2()"
            );
        }
    }

    #[test]
    fn test_xml_hash_remove_entry3() {
        unsafe {
            let mut leaks = 0;
            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_name3 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_f in 0..GEN_NB_XML_HASH_DEALLOCATOR {
                                let mem_base = xml_mem_blocks();
                                let table = gen_xml_hash_table_ptr(n_table, 0);
                                let name = gen_const_xml_char_ptr(n_name, 1);
                                let name2 = gen_const_xml_char_ptr(n_name2, 2);
                                let name3 = gen_const_xml_char_ptr(n_name3, 3);
                                let f = gen_xml_hash_deallocator(n_f, 4);

                                let ret_val = xml_hash_remove_entry3(table, name, name2, name3, f);
                                desret_int(ret_val);
                                des_xml_hash_table_ptr(n_table, table, 0);
                                des_const_xml_char_ptr(n_name, name, 1);
                                des_const_xml_char_ptr(n_name2, name2, 2);
                                des_const_xml_char_ptr(n_name3, name3, 3);
                                des_xml_hash_deallocator(n_f, f, 4);
                                xml_reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlHashRemoveEntry3",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_table);
                                    eprint!(" {}", n_name);
                                    eprint!(" {}", n_name2);
                                    eprint!(" {}", n_name3);
                                    eprintln!(" {}", n_f);
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlHashRemoveEntry3()"
            );
        }
    }

    #[test]
    fn test_xml_hash_scan() {

        /* missing type support */
    }

    #[test]
    fn test_xml_hash_scan3() {

        /* missing type support */
    }

    #[test]
    fn test_xml_hash_scan_full() {

        /* missing type support */
    }

    #[test]
    fn test_xml_hash_scan_full3() {

        /* missing type support */
    }

    #[test]
    fn test_xml_hash_size() {
        unsafe {
            let mut leaks = 0;

            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                let mem_base = xml_mem_blocks();
                let table = gen_xml_hash_table_ptr(n_table, 0);

                let ret_val = xml_hash_size(table);
                desret_int(ret_val);
                des_xml_hash_table_ptr(n_table, table, 0);
                xml_reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlHashSize",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_table);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in xmlHashSize()");
        }
    }

    #[test]
    fn test_xml_hash_update_entry() {
        unsafe {
            let mut leaks = 0;
            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_userdata in 0..GEN_NB_USERDATA {
                        for n_f in 0..GEN_NB_XML_HASH_DEALLOCATOR {
                            let mem_base = xml_mem_blocks();
                            let table = gen_xml_hash_table_ptr(n_table, 0);
                            let name = gen_const_xml_char_ptr(n_name, 1);
                            let userdata = gen_userdata(n_userdata, 2);
                            let f = gen_xml_hash_deallocator(n_f, 3);

                            let ret_val = xml_hash_update_entry(table, name, userdata, f);
                            desret_int(ret_val);
                            des_xml_hash_table_ptr(n_table, table, 0);
                            des_const_xml_char_ptr(n_name, name, 1);
                            des_userdata(n_userdata, userdata, 2);
                            des_xml_hash_deallocator(n_f, f, 3);
                            xml_reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlHashUpdateEntry",
                                    xml_mem_blocks() - mem_base
                                );
                                eprint!(" {}", n_table);
                                eprint!(" {}", n_name);
                                eprint!(" {}", n_userdata);
                                eprintln!(" {}", n_f);
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlHashUpdateEntry()"
            );
        }
    }

    #[test]
    fn test_xml_hash_update_entry2() {
        unsafe {
            let mut leaks = 0;
            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_userdata in 0..GEN_NB_USERDATA {
                            for n_f in 0..GEN_NB_XML_HASH_DEALLOCATOR {
                                let mem_base = xml_mem_blocks();
                                let table = gen_xml_hash_table_ptr(n_table, 0);
                                let name = gen_const_xml_char_ptr(n_name, 1);
                                let name2 = gen_const_xml_char_ptr(n_name2, 2);
                                let userdata = gen_userdata(n_userdata, 3);
                                let f = gen_xml_hash_deallocator(n_f, 4);

                                let ret_val =
                                    xml_hash_update_entry2(table, name, name2, userdata, f);
                                desret_int(ret_val);
                                des_xml_hash_table_ptr(n_table, table, 0);
                                des_const_xml_char_ptr(n_name, name, 1);
                                des_const_xml_char_ptr(n_name2, name2, 2);
                                des_userdata(n_userdata, userdata, 3);
                                des_xml_hash_deallocator(n_f, f, 4);
                                xml_reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlHashUpdateEntry2",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_table);
                                    eprint!(" {}", n_name);
                                    eprint!(" {}", n_name2);
                                    eprint!(" {}", n_userdata);
                                    eprintln!(" {}", n_f);
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlHashUpdateEntry2()"
            );
        }
    }

    #[test]
    fn test_xml_hash_update_entry3() {
        unsafe {
            let mut leaks = 0;
            for n_table in 0..GEN_NB_XML_HASH_TABLE_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_name2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_name3 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                            for n_userdata in 0..GEN_NB_USERDATA {
                                for n_f in 0..GEN_NB_XML_HASH_DEALLOCATOR {
                                    let mem_base = xml_mem_blocks();
                                    let table = gen_xml_hash_table_ptr(n_table, 0);
                                    let name = gen_const_xml_char_ptr(n_name, 1);
                                    let name2 = gen_const_xml_char_ptr(n_name2, 2);
                                    let name3 = gen_const_xml_char_ptr(n_name3, 3);
                                    let userdata = gen_userdata(n_userdata, 4);
                                    let f = gen_xml_hash_deallocator(n_f, 5);

                                    let ret_val = xml_hash_update_entry3(
                                        table, name, name2, name3, userdata, f,
                                    );
                                    desret_int(ret_val);
                                    des_xml_hash_table_ptr(n_table, table, 0);
                                    des_const_xml_char_ptr(n_name, name, 1);
                                    des_const_xml_char_ptr(n_name2, name2, 2);
                                    des_const_xml_char_ptr(n_name3, name3, 3);
                                    des_userdata(n_userdata, userdata, 4);
                                    des_xml_hash_deallocator(n_f, f, 5);
                                    xml_reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlHashUpdateEntry3",
                                            xml_mem_blocks() - mem_base
                                        );
                                        eprint!(" {}", n_table);
                                        eprint!(" {}", n_name);
                                        eprint!(" {}", n_name2);
                                        eprint!(" {}", n_name3);
                                        eprint!(" {}", n_userdata);
                                        eprintln!(" {}", n_f);
                                    }
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in xmlHashUpdateEntry3()"
            );
        }
    }
}

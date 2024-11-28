//! Provide methods and data structures for memory allocation.  
//! This module is based on `libxml/xmlmemory.h`, `xmlmemory.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: interface for the memory allocator
// Description: provides interfaces for the memory allocator,
//              including debugging capabilities.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// xmlmemory.c:  libxml memory allocator wrapper.
//
// daniel@veillard.com

use std::cell::Cell;
use std::ffi::{c_char, c_void};
use std::io::Write;
use std::mem::{size_of, zeroed};
use std::ptr::{addr_of_mut, null_mut};

use libc::{
    fclose, fopen, fprintf, free, getenv, malloc, memset, realloc, sscanf, strcpy, strlen, FILE,
};

use crate::generic_error;
use crate::libxml::globals::{
    xml_free, xml_malloc, xml_malloc_atomic, xml_mem_strdup, xml_realloc,
};
use crate::libxml::parser::xml_init_parser;

use super::globals::{_XML_FREE, _XML_MALLOC, _XML_MALLOC_ATOMIC, _XML_MEM_STRDUP, _XML_REALLOC};
use super::threads::{
    xml_cleanup_mutex, xml_init_mutex, xml_mutex_lock, xml_mutex_unlock, XmlMutex,
};
use super::xmlstring::XmlChar;

thread_local! {
    static DEBUG_MEM_SIZE: Cell<i64> = const { Cell::new(0) };
    static DEBUG_MEM_BLOCKS: Cell<i64> = const { Cell::new(0) };
    static DEBUG_MAX_MEM_SIZE: Cell<i64> = const { Cell::new(0) };
}
static mut XML_MEM_MUTEX: XmlMutex = unsafe { zeroed() };

const MEMTAG: usize = 0x5aa5;

const MALLOC_TYPE: usize = 1;
const REALLOC_TYPE: usize = 2;
const STRDUP_TYPE: usize = 3;
const MALLOC_ATOMIC_TYPE: usize = 4;
const REALLOC_ATOMIC_TYPE: usize = 5;

#[repr(C)]
struct Memnod {
    mh_tag: u32,
    mh_type: u32,
    mh_number: u64,
    mh_size: usize,
    mh_file: *const c_char,
    mh_line: u32,
}

type Memhdr = Memnod;

#[cfg(target_os = "solaris")]
const ALIGN_SIZE: usize = 16;
#[cfg(not(target_os = "solaris"))]
const ALIGN_SIZE: usize = size_of::<f64>();
const HDR_SIZE: usize = size_of::<Memhdr>();
const RESERVE_SIZE: usize = ((HDR_SIZE + ALIGN_SIZE - 1) / ALIGN_SIZE) * ALIGN_SIZE;

const MAX_SIZE_T: usize = 1usize.wrapping_neg();

macro_rules! CLIENT_2_HDR {
    ( $t:expr ) => {
        ($t as *mut c_char).sub(RESERVE_SIZE) as _
    };
}
macro_rules! HDR_2_CLIENT {
    ( $t:expr ) => {
        ($t as *mut c_char).add(RESERVE_SIZE)
    };
}

static mut BLOCK: u32 = 0;
static mut XML_MEM_STOP_AT_BLOCK: u32 = 0;
static mut XML_MEM_TRACE_BLOCK_AT: *mut c_void = null_mut();

// internal error function.
unsafe extern "C" fn debugmem_tag_error(addr: *mut c_void) {
    generic_error!("Memory tag error occurs :{:?} \n\t bye\n", addr);
}

macro_rules! mem_tag_err {
    ( $t:expr ) => {
        debugmem_tag_error($t)
    };
}

/// Signature for a free() implementation.
#[doc(alias = "xmlFreeFunc")]
pub type XmlFreeFunc = unsafe extern "C" fn(mem: *mut c_void);
/// Signature for a malloc() implementation.
///
/// Returns a pointer to the newly allocated block or NULL in case of error.
#[doc(alias = "xmlMallocFunc")]
pub type XmlMallocFunc = unsafe extern "C" fn(size: usize) -> *mut c_void;
/// Signature for a realloc() implementation.
///
/// Returns a pointer to the newly reallocated block or NULL in case of error.
#[doc(alias = "xmlReallocFunc")]
pub type XmlReallocFunc = unsafe extern "C" fn(mem: *mut c_void, size: usize) -> *mut c_void;
/// Signature for an strdup() implementation.
///
/// Returns the copy of the string or NULL in case of error.
#[doc(alias = "xmlStrdupFunc")]
pub type XmlStrdupFunc = unsafe extern "C" fn(str: *const XmlChar) -> *mut XmlChar;

/// Breakpoint to use in conjunction with xmlMemStopAtBlock. When the block
/// number reaches the specified value this function is called. One need to add a breakpoint
/// to it to get the context in which the given block is allocated.
#[doc(alias = "xmlMallocBreakpoint")]
pub unsafe extern "C" fn xml_malloc_breakpoint() {
    generic_error!(
        "xmlMallocBreakpoint reached on block {}\n",
        XML_MEM_STOP_AT_BLOCK
    );
}

/// Override the default memory access functions with a new set
/// This has to be called before any other libxml routines !
///
/// Should this be blocked if there was already some allocations done ?
///
/// Returns 0 on success
#[doc(alias = "xmlMemSetup")]
pub unsafe extern "C" fn xml_mem_setup(
    free_func: Option<XmlFreeFunc>,
    malloc_func: Option<XmlMallocFunc>,
    realloc_func: Option<XmlReallocFunc>,
    strdup_func: Option<XmlStrdupFunc>,
) -> i32 {
    if free_func.is_none() {
        return -1;
    }
    if malloc_func.is_none() {
        return -1;
    }
    if realloc_func.is_none() {
        return -1;
    }
    if strdup_func.is_none() {
        return -1;
    }
    _XML_FREE = free_func;
    _XML_MALLOC = malloc_func;
    _XML_MALLOC_ATOMIC = malloc_func.unwrap();
    _XML_REALLOC = realloc_func;
    _XML_MEM_STRDUP = strdup_func;
    0
}

/// Provides the memory access functions set currently in use
///
/// Returns 0 on success
#[doc(alias = "xmlMemGet")]
pub unsafe extern "C" fn xml_mem_get(
    free_func: *mut XmlFreeFunc,
    malloc_func: *mut XmlMallocFunc,
    realloc_func: *mut XmlReallocFunc,
    strdup_func: *mut XmlStrdupFunc,
) -> i32 {
    if !free_func.is_null() {
        *free_func = xml_free;
    }
    if !malloc_func.is_null() {
        *malloc_func = xml_malloc;
    }
    if !realloc_func.is_null() {
        *realloc_func = xml_realloc;
    }
    if !strdup_func.is_null() {
        *strdup_func = xml_mem_strdup;
    }
    0
}

/// Override the default memory access functions with a new set
/// This has to be called before any other libxml routines !
/// The mallocAtomicFunc is specialized for atomic block
/// allocations (i.e. of areas  useful for garbage collected memory allocators
///
/// Should this be blocked if there was already some allocations done ?
///
/// Returns 0 on success
#[doc(alias = "xmlGcMemSetup")]
pub unsafe extern "C" fn xml_gc_mem_setup(
    free_func: Option<XmlFreeFunc>,
    malloc_func: Option<XmlMallocFunc>,
    malloc_atomic_func: Option<XmlMallocFunc>,
    realloc_func: Option<XmlReallocFunc>,
    strdup_func: Option<XmlStrdupFunc>,
) -> i32 {
    if free_func.is_none() {
        return -1;
    }
    if malloc_func.is_none() {
        return -1;
    }
    if malloc_atomic_func.is_none() {
        return -1;
    }
    if realloc_func.is_none() {
        return -1;
    }
    if strdup_func.is_none() {
        return -1;
    }
    _XML_FREE = free_func;
    _XML_MALLOC = malloc_func;
    _XML_MALLOC_ATOMIC = malloc_atomic_func.unwrap();
    _XML_REALLOC = realloc_func;
    _XML_MEM_STRDUP = strdup_func;
    0
}

/// Provides the memory access functions set currently in use
/// The mallocAtomicFunc is specialized for atomic block
/// allocations (i.e. of areas  useful for garbage collected memory allocators
///
/// Returns 0 on success
#[doc(alias = "xmlGcMemGet")]
pub unsafe extern "C" fn xml_gc_mem_get(
    free_func: *mut XmlFreeFunc,
    malloc_func: *mut XmlMallocFunc,
    malloc_atomic_func: *mut XmlMallocFunc,
    realloc_func: *mut XmlReallocFunc,
    strdup_func: *mut XmlStrdupFunc,
) -> i32 {
    if !free_func.is_null() {
        *free_func = xml_free;
    }
    if !malloc_func.is_null() {
        *malloc_func = xml_malloc;
    }
    if !malloc_atomic_func.is_null() {
        *malloc_atomic_func = xml_malloc_atomic;
    }
    if !realloc_func.is_null() {
        *realloc_func = xml_realloc;
    }
    if !strdup_func.is_null() {
        *strdup_func = xml_mem_strdup;
    }
    0
}

#[doc(alias = "xmlInitMemory")]
#[deprecated = "Alias for xmlInitParser"]
pub unsafe extern "C" fn xml_init_memory() -> i32 {
    xml_init_parser();
    0
}

#[doc(alias = "xmlCleanupMemory")]
#[deprecated = "This function is a no-op. Call xmlCleanupParser
to free global state but see the warnings there. xmlCleanupParser
should be only called once at program exit. In most cases, you don't
have call cleanup functions at all"]
pub unsafe extern "C" fn xml_cleanup_memory() {}

/// Returns the size of a memory allocation.
#[doc(alias = "xmlMemSize")]
pub unsafe extern "C" fn xml_mem_size(ptr: *mut c_void) -> usize {
    if ptr.is_null() {
        return 0;
    }

    let p: *mut Memhdr = CLIENT_2_HDR!(ptr);
    if (*p).mh_tag != MEMTAG as _ {
        return 0;
    }

    (*p).mh_size
}

/// Provides the amount of memory currently allocated
///
/// Returns an int representing the amount of memory allocated.
#[doc(alias = "xmlMemUsed")]
pub unsafe extern "C" fn xml_mem_used() -> i32 {
    DEBUG_MEM_SIZE.get() as _
}

/// Provides the number of memory areas currently allocated
///
/// Returns an int representing the number of blocks
#[doc(alias = "xmlMemBlocks")]
pub unsafe extern "C" fn xml_mem_blocks() -> i32 {
    xml_mutex_lock(addr_of_mut!(XML_MEM_MUTEX));
    let res: i32 = DEBUG_MEM_BLOCKS.get() as _;
    xml_mutex_unlock(addr_of_mut!(XML_MEM_MUTEX));
    res
}

/// show in-extenso the memory blocks allocated
#[doc(alias = "xmlMemDisplay")]
pub unsafe extern "C" fn xml_mem_display(mut fp: *mut FILE) {
    let old_fp: *mut FILE = fp;

    if fp.is_null() {
        fp = fopen(c".memorylist".as_ptr() as _, c"w".as_ptr() as _);
        if fp.is_null() {
            return;
        }
    }

    fprintf(
        fp,
        c"Memory list not compiled (MEM_LIST not defined !)\n".as_ptr() as _,
    );
    if old_fp.is_null() {
        fclose(fp);
    }
}

/// The last nbBytes of memory allocated and not freed, useful for dumping
/// the memory left allocated between two places at runtime.
#[doc(alias = "xmlMemDisplayLast")]
pub unsafe extern "C" fn xml_mem_display_last(mut fp: *mut FILE, nb_bytes: i64) {
    let old_fp: *mut FILE = fp;

    if nb_bytes <= 0 {
        return;
    }

    if fp.is_null() {
        fp = fopen(c".memorylist".as_ptr() as _, c"w".as_ptr() as _);
        if fp.is_null() {
            return;
        }
    }

    fprintf(
        fp,
        c"Memory list not compiled (MEM_LIST not defined !)\n".as_ptr() as _,
    );
    if old_fp.is_null() {
        fclose(fp);
    }
}

/// Show a show display of the memory allocated, and dump
/// the @nr last allocated areas which were not freed
#[doc(alias = "xmlMemShow")]
pub unsafe extern "C" fn xml_mem_show<'a>(fp: &mut (impl Write + 'a), _nr: i32) {
    writeln!(
        fp,
        "      MEMORY ALLOCATED : {}, MAX was {}",
        DEBUG_MEM_SIZE.get(),
        DEBUG_MAX_MEM_SIZE.get(),
    );
}

/// Dump in-extenso the memory blocks allocated to the file .memorylist
#[doc(alias = "xmlMemoryDump")]
pub unsafe extern "C" fn xml_memory_dump() {}

/// A malloc() equivalent, with logging of the allocation info.
///
/// Returns a pointer to the allocated area or NULL in case of lack of memory.
#[doc(alias = "xmlMemMalloc")]
pub unsafe extern "C" fn xml_mem_malloc(size: usize) -> *mut c_void {
    xml_malloc_loc(size, c"none".as_ptr() as _, 0)
}

/// A realloc() equivalent, with logging of the allocation info.
///
/// Returns a pointer to the allocated area or NULL in case of lack of memory.
#[doc(alias = "xmlMemRealloc")]
pub unsafe extern "C" fn xml_mem_realloc(ptr: *mut c_void, size: usize) -> *mut c_void {
    xml_realloc_loc(ptr, size, c"none".as_ptr() as _, 0)
}

/// A free() equivalent, with error checking.
#[doc(alias = "xmlMemFree")]
pub unsafe extern "C" fn xml_mem_free(ptr: *mut c_void) {
    if ptr.is_null() {
        return;
    }

    'error: {
        if ptr == -1 as _ {
            generic_error!("trying to free pointer from freed area\n");
            break 'error;
        }

        if XML_MEM_TRACE_BLOCK_AT == ptr {
            generic_error!("{:?} : Freed()\n", XML_MEM_TRACE_BLOCK_AT);
            xml_malloc_breakpoint();
        }

        let target: *mut char = ptr as _;

        let p: *mut Memhdr = CLIENT_2_HDR!(ptr);
        if (*p).mh_tag != MEMTAG as _ {
            mem_tag_err!(p as _);
            break 'error;
        }
        if XML_MEM_STOP_AT_BLOCK == (*p).mh_number as _ {
            xml_malloc_breakpoint();
        }
        (*p).mh_tag = !MEMTAG as _;
        memset(target as _, -1, (*p).mh_size);
        xml_mutex_lock(addr_of_mut!(XML_MEM_MUTEX));
        DEBUG_MEM_SIZE.set(DEBUG_MEM_SIZE.get() - (*p).mh_size as i64);
        DEBUG_MEM_BLOCKS.set(DEBUG_MEM_BLOCKS.get() - 1);
        xml_mutex_unlock(addr_of_mut!(XML_MEM_MUTEX));

        free(p as _);

        return;
    }

    // error:
    generic_error!("xmlMemFree({:?}) error\n", ptr);
    xml_malloc_breakpoint();
}

/// A strdup() equivalent, with logging of the allocation info.
///
/// Returns a pointer to the new string or NULL if allocation error occurred.
#[doc(alias = "xmlMemoryStrdup")]
pub unsafe extern "C" fn xml_memory_strdup(str: *const XmlChar) -> *mut XmlChar {
    xml_mem_strdup_loc(str as _, c"none".as_ptr() as _, 0) as _
}

/// A malloc() equivalent, with logging of the allocation info.
///
/// Returns a pointer to the allocated area or NULL in case of lack of memory.
#[doc(alias = "xmlMallocLoc")]
pub unsafe extern "C" fn xml_malloc_loc(
    size: usize,
    file: *const c_char,
    line: i32,
) -> *mut c_void {
    xml_init_parser();

    if size > MAX_SIZE_T - RESERVE_SIZE {
        generic_error!("xmlMallocLoc : Unsigned overflow\n");
        xml_memory_dump();
        return null_mut();
    }

    let p: *mut Memhdr = malloc(RESERVE_SIZE + size) as *mut Memhdr;

    if p.is_null() {
        generic_error!("xmlMallocLoc : Out of free space\n");
        xml_memory_dump();
        return null_mut();
    }
    (*p).mh_tag = MEMTAG as _;
    (*p).mh_size = size;
    (*p).mh_type = MALLOC_TYPE as _;
    (*p).mh_file = file;
    (*p).mh_line = line as _;
    xml_mutex_lock(addr_of_mut!(XML_MEM_MUTEX));
    BLOCK += 1;
    (*p).mh_number = BLOCK as _;
    DEBUG_MEM_SIZE.set(DEBUG_MEM_SIZE.get() + size as i64);
    DEBUG_MEM_BLOCKS.set(DEBUG_MEM_BLOCKS.get() + 1);
    if DEBUG_MEM_SIZE.get() > DEBUG_MAX_MEM_SIZE.get() {
        DEBUG_MAX_MEM_SIZE.set(DEBUG_MEM_SIZE.get());
    }
    xml_mutex_unlock(addr_of_mut!(XML_MEM_MUTEX));

    if XML_MEM_STOP_AT_BLOCK == (*p).mh_number as _ {
        xml_malloc_breakpoint();
    }

    let ret: *mut c_void = HDR_2_CLIENT!(p) as _;

    if XML_MEM_TRACE_BLOCK_AT == ret {
        generic_error!(
            "{:?} : Malloc({}) Ok\n",
            XML_MEM_TRACE_BLOCK_AT,
            size as u64
        );
        xml_malloc_breakpoint();
    }

    ret
}

/// A realloc() equivalent, with logging of the allocation info.
///
/// Returns a pointer to the allocated area or NULL in case of lack of memory.
#[doc(alias = "xmlReallocLoc")]
pub unsafe extern "C" fn xml_realloc_loc(
    ptr: *mut c_void,
    size: usize,
    file: *const c_char,
    line: i32,
) -> *mut c_void {
    let mut p: *mut Memhdr;

    if ptr.is_null() {
        return xml_malloc_loc(size, file, line);
    }

    xml_init_parser();

    p = CLIENT_2_HDR!(ptr);
    let number: u64 = (*p).mh_number;
    if XML_MEM_STOP_AT_BLOCK == number as _ {
        xml_malloc_breakpoint();
    }
    if (*p).mh_tag != MEMTAG as _ {
        mem_tag_err!(p as _);
        // goto error;
        return null_mut();
    }
    (*p).mh_tag = !MEMTAG as _;
    xml_mutex_lock(addr_of_mut!(XML_MEM_MUTEX));
    DEBUG_MEM_SIZE.set(DEBUG_MEM_SIZE.get() - (*p).mh_size as i64);
    DEBUG_MEM_BLOCKS.set(DEBUG_MEM_BLOCKS.get() - 1);
    xml_mutex_unlock(addr_of_mut!(XML_MEM_MUTEX));

    if size > MAX_SIZE_T - RESERVE_SIZE {
        generic_error!("xmlReallocLoc : Unsigned overflow\n");
        xml_memory_dump();
        return null_mut();
    }

    let tmp: *mut Memhdr = realloc(p as _, RESERVE_SIZE + size) as *mut Memhdr;
    if tmp.is_null() {
        free(p as _);
        // goto error;
        return null_mut();
    }
    p = tmp;
    if XML_MEM_TRACE_BLOCK_AT == ptr {
        generic_error!(
            "{:?} : Realloced({} -> {}) Ok\n",
            XML_MEM_TRACE_BLOCK_AT,
            (*p).mh_size as u64,
            size as u64
        );
        xml_malloc_breakpoint();
    }
    (*p).mh_tag = MEMTAG as _;
    (*p).mh_number = number;
    (*p).mh_type = REALLOC_TYPE as _;
    (*p).mh_size = size;
    (*p).mh_file = file;
    (*p).mh_line = line as _;
    xml_mutex_lock(addr_of_mut!(XML_MEM_MUTEX));
    DEBUG_MEM_SIZE.set(DEBUG_MEM_SIZE.get() + size as i64);
    DEBUG_MEM_BLOCKS.set(DEBUG_MEM_BLOCKS.get() + 1);
    if DEBUG_MEM_SIZE.get() > DEBUG_MAX_MEM_SIZE.get() {
        DEBUG_MAX_MEM_SIZE.set(DEBUG_MEM_SIZE.get());
    }
    xml_mutex_unlock(addr_of_mut!(XML_MEM_MUTEX));

    HDR_2_CLIENT!(p) as _

    //  error:
    // 	 return(null_mut());
}

/// A malloc() equivalent, with logging of the allocation info.
///
/// Returns a pointer to the allocated area or NULL in case of lack of memory.
#[doc(alias = "xmlMallocAtomicLoc")]
pub unsafe extern "C" fn xml_malloc_atomic_loc(
    size: usize,
    file: *const c_char,
    line: i32,
) -> *mut c_void {
    xml_init_parser();

    if size > MAX_SIZE_T - RESERVE_SIZE {
        generic_error!("xmlMallocAtomicLoc : Unsigned overflow\n");
        xml_memory_dump();
        return null_mut();
    }

    let p: *mut Memhdr = malloc(RESERVE_SIZE + size) as *mut Memhdr;

    if p.is_null() {
        generic_error!("xmlMallocAtomicLoc : Out of free space\n");
        xml_memory_dump();
        return null_mut();
    }
    (*p).mh_tag = MEMTAG as _;
    (*p).mh_size = size;
    (*p).mh_type = MALLOC_ATOMIC_TYPE as _;
    (*p).mh_file = file;
    (*p).mh_line = line as _;
    xml_mutex_lock(addr_of_mut!(XML_MEM_MUTEX));
    BLOCK += 1;
    (*p).mh_number = BLOCK as _;
    DEBUG_MEM_SIZE.set(DEBUG_MEM_SIZE.get() + size as i64);
    DEBUG_MEM_BLOCKS.set(DEBUG_MEM_BLOCKS.get() + 1);
    if DEBUG_MEM_SIZE.get() > DEBUG_MAX_MEM_SIZE.get() {
        DEBUG_MAX_MEM_SIZE.set(DEBUG_MEM_SIZE.get());
    }
    xml_mutex_unlock(addr_of_mut!(XML_MEM_MUTEX));

    if XML_MEM_STOP_AT_BLOCK == (*p).mh_number as _ {
        xml_malloc_breakpoint();
    }

    let ret: *mut c_void = HDR_2_CLIENT!(p) as _;

    if XML_MEM_TRACE_BLOCK_AT == ret {
        generic_error!(
            "{:?} : Malloc({}) Ok\n",
            XML_MEM_TRACE_BLOCK_AT,
            size as u64
        );
        xml_malloc_breakpoint();
    }

    ret
}

/// A strdup() equivalent, with logging of the allocation info.
///
/// Returns a pointer to the new string or NULL if allocation error occurred.
#[doc(alias = "xmlMemStrdupLoc")]
pub unsafe extern "C" fn xml_mem_strdup_loc(
    str: *const c_char,
    file: *const c_char,
    line: i32,
) -> *mut char {
    let size: usize = strlen(str) + 1;

    xml_init_parser();

    if size > MAX_SIZE_T - RESERVE_SIZE {
        generic_error!("xmlMemStrdupLoc : Unsigned overflow\n");
        xml_memory_dump();
        return null_mut();
    }

    let p: *mut Memhdr = malloc(RESERVE_SIZE + size) as _;
    if p.is_null() {
        return null_mut();
    }
    (*p).mh_tag = MEMTAG as _;
    (*p).mh_size = size;
    (*p).mh_type = STRDUP_TYPE as _;
    (*p).mh_file = file;
    (*p).mh_line = line as _;
    xml_mutex_lock(addr_of_mut!(XML_MEM_MUTEX));
    BLOCK += 1;
    (*p).mh_number = BLOCK as _;
    DEBUG_MEM_SIZE.set(DEBUG_MEM_SIZE.get() + size as i64);
    DEBUG_MEM_BLOCKS.set(DEBUG_MEM_BLOCKS.get() + 1);
    if DEBUG_MEM_SIZE.get() > DEBUG_MAX_MEM_SIZE.get() {
        DEBUG_MAX_MEM_SIZE.set(DEBUG_MEM_SIZE.get());
    }
    xml_mutex_unlock(addr_of_mut!(XML_MEM_MUTEX));

    let s: *mut char = HDR_2_CLIENT!(p) as _;

    if XML_MEM_STOP_AT_BLOCK == (*p).mh_number as _ {
        xml_malloc_breakpoint();
    }

    strcpy(s as _, str);

    if XML_MEM_TRACE_BLOCK_AT == s as _ {
        generic_error!("{:?} : Strdup() Ok\n", XML_MEM_TRACE_BLOCK_AT);
        xml_malloc_breakpoint();
    }

    s
}

/// Initialize the memory layer.
///
/// Returns 0 on success
#[doc(alias = "xmlInitMemoryInternal")]
pub(crate) unsafe extern "C" fn xml_init_memory_internal() {
    let mut breakpoint: *mut c_char;
    xml_init_mutex(addr_of_mut!(XML_MEM_MUTEX));

    breakpoint = getenv(c"XML_MEM_BREAKPOINT".as_ptr());
    if !breakpoint.is_null() {
        sscanf(
            breakpoint,
            c"%ud".as_ptr() as _,
            addr_of_mut!(XML_MEM_STOP_AT_BLOCK),
        );
    }
    breakpoint = getenv(c"XML_MEM_TRACE".as_ptr());
    if !breakpoint.is_null() {
        sscanf(
            breakpoint,
            c"%p".as_ptr() as _,
            addr_of_mut!(XML_MEM_TRACE_BLOCK_AT),
        );
    }
}

/// Free up all the memory allocated by the library for its own use.  
/// This should not be called by user level code.
#[doc(alias = "xmlCleanupMemoryInternal")]
pub(crate) unsafe extern "C" fn xml_cleanup_memory_internal() {
    xml_cleanup_mutex(addr_of_mut!(XML_MEM_MUTEX));
}

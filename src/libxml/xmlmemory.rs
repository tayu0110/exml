//! Provide methods and data structures for memory allocation.  
//! This module is based on `libxml/xmlmemory.h`, `xmlmemory.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::ffi::{c_char, c_void};
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
use crate::private::threads::{xml_cleanup_mutex, xml_init_mutex};

use super::globals::{_XML_FREE, _XML_MALLOC, _XML_MALLOC_ATOMIC, _XML_MEM_STRDUP, _XML_REALLOC};
use super::threads::{xml_mutex_lock, xml_mutex_unlock, XmlMutex};
use super::xmlstring::XmlChar;

static mut DEBUG_MEM_SIZE: u64 = 0;
static mut DEBUG_MEM_BLOCKS: u64 = 0;
static mut DEBUG_MAX_MEM_SIZE: u64 = 0;
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
// #[cfg(feature = "debug_memory_location")]
// static mut memlist: *mut Memhdr = null_mut();

/*
 * debugmem_tag_error:
 *
 * internal error function.
 */
unsafe extern "C" fn debugmem_tag_error(addr: *mut c_void) {
    generic_error!("Memory tag error occurs :{:?} \n\t bye\n", addr);
}

macro_rules! mem_tag_err {
    ( $t:expr ) => {
        debugmem_tag_error($t)
    };
}

/*
 * The XML memory wrapper support 4 basic overloadable functions.
 */
/**
 * xmlFreeFunc:
 * @mem: an already allocated block of memory
 *
 * Signature for a free() implementation.
 */
pub type XmlFreeFunc = unsafe extern "C" fn(mem: *mut c_void);
/**
 * xmlMallocFunc:
 * @size:  the size requested in bytes
 *
 * Signature for a malloc() implementation.
 *
 * Returns a pointer to the newly allocated block or NULL in case of error.
 */
pub type XmlMallocFunc = unsafe extern "C" fn(size: usize) -> *mut c_void;

/**
 * xmlReallocFunc:
 * @mem: an already allocated block of memory
 * @size:  the new size requested in bytes
 *
 * Signature for a realloc() implementation.
 *
 * Returns a pointer to the newly reallocated block or NULL in case of error.
 */
pub type XmlReallocFunc = unsafe extern "C" fn(mem: *mut c_void, size: usize) -> *mut c_void;

/**
 * xmlStrdupFunc:
 * @str: a zero terminated string
 *
 * Signature for an strdup() implementation.
 *
 * Returns the copy of the string or NULL in case of error.
 */
pub type XmlStrdupFunc = unsafe extern "C" fn(str: *const XmlChar) -> *mut XmlChar;

/**
 * xmlMallocBreakpoint:
 *
 * Breakpoint to use in conjunction with xmlMemStopAtBlock. When the block
 * number reaches the specified value this function is called. One need to add a breakpoint
 * to it to get the context in which the given block is allocated.
 */
pub unsafe extern "C" fn xml_malloc_breakpoint() {
    generic_error!(
        "xmlMallocBreakpoint reached on block {}\n",
        XML_MEM_STOP_AT_BLOCK
    );
}

/****************************************************************
 *								*
 *		Initialization Routines				*
 *								*
 ****************************************************************/
/*
 * The way to overload the existing functions.
 * The xmlGc function have an extra entry for atomic block
 * allocations useful for garbage collected memory allocators
 */
/**
 * xmlMemSetup:
 * @freeFunc: the free() function to use
 * @mallocFunc: the malloc() function to use
 * @reallocFunc: the realloc() function to use
 * @strdupFunc: the strdup() function to use
 *
 * Override the default memory access functions with a new set
 * This has to be called before any other libxml routines !
 *
 * Should this be blocked if there was already some allocations
 * done ?
 *
 * Returns 0 on success
 */
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

/**
 * xmlMemGet:
 * @freeFunc: place to save the free() function in use
 * @mallocFunc: place to save the malloc() function in use
 * @reallocFunc: place to save the realloc() function in use
 * @strdupFunc: place to save the strdup() function in use
 *
 * Provides the memory access functions set currently in use
 *
 * Returns 0 on success
 */
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

/**
 * xmlGcMemSetup:
 * @freeFunc: the free() function to use
 * @mallocFunc: the malloc() function to use
 * @mallocAtomicFunc: the malloc() function to use for atomic allocations
 * @reallocFunc: the realloc() function to use
 * @strdupFunc: the strdup() function to use
 *
 * Override the default memory access functions with a new set
 * This has to be called before any other libxml routines !
 * The mallocAtomicFunc is specialized for atomic block
 * allocations (i.e. of areas  useful for garbage collected memory allocators
 *
 * Should this be blocked if there was already some allocations
 * done ?
 *
 * Returns 0 on success
 */
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

/**
 * xmlGcMemGet:
 * @freeFunc: place to save the free() function in use
 * @mallocFunc: place to save the malloc() function in use
 * @mallocAtomicFunc: place to save the atomic malloc() function in use
 * @reallocFunc: place to save the realloc() function in use
 * @strdupFunc: place to save the strdup() function in use
 *
 * Provides the memory access functions set currently in use
 * The mallocAtomicFunc is specialized for atomic block
 * allocations (i.e. of areas  useful for garbage collected memory allocators
 *
 * Returns 0 on success
 */
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

/*
 * Initialization of the memory layer.
 */
/**
 * xmlInitMemory:
 *
 * DEPRECATED: Alias for xmlInitParser.
 */
#[deprecated]
pub unsafe extern "C" fn xml_init_memory() -> i32 {
    xml_init_parser();
    0
}

/*
 * Cleanup of the memory layer.
 */
/**
 * xmlCleanupMemory:
 *
 * DEPRECATED: This function is a no-op. Call xmlCleanupParser
 * to free global state but see the warnings there. xmlCleanupParser
 * should be only called once at program exit. In most cases, you don't
 * have call cleanup functions at all.
 */
#[deprecated]
pub unsafe extern "C" fn xml_cleanup_memory() {}

/*
 * These are specific to the XML debug memory wrapper.
 */
/**
 * xmlMemSize:
 * @ptr:  pointer to the memory allocation
 *
 * Returns the size of a memory allocation.
 */
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

/**
 * xmlMemUsed:
 *
 * Provides the amount of memory currently allocated
 *
 * Returns an int representing the amount of memory allocated.
 */
pub unsafe extern "C" fn xml_mem_used() -> i32 {
    DEBUG_MEM_SIZE as _
}

/**
 * xmlMemBlocks:
 *
 * Provides the number of memory areas currently allocated
 *
 * Returns an int representing the number of blocks
 */
pub unsafe extern "C" fn xml_mem_blocks() -> i32 {
    xml_mutex_lock(addr_of_mut!(XML_MEM_MUTEX));
    let res: i32 = DEBUG_MEM_BLOCKS as _;
    xml_mutex_unlock(addr_of_mut!(XML_MEM_MUTEX));
    res
}

/**
 * xmlMemDisplay:
 * @fp:  a FILE descriptor used as the output file, if NULL, the result is
 *       written to the file .memorylist
 *
 * show in-extenso the memory blocks allocated
 */
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

/**
 * xmlMemDisplayLast:
 * @fp:  a FILE descriptor used as the output file, if NULL, the result is
 *       written to the file .memorylist
 * @nbBytes: the amount of memory to dump
 *
 * the last nbBytes of memory allocated and not freed, useful for dumping
 * the memory left allocated between two places at runtime.
 */
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

/**
 * xmlMemShow:
 * @fp:  a FILE descriptor used as the output file
 * @nr:  number of entries to dump
 *
 * show a show display of the memory allocated, and dump
 * the @nr last allocated areas which were not freed
 */
pub unsafe extern "C" fn xml_mem_show(fp: *mut FILE, _nr: i32) {
    // #[cfg(feature = "debug_memory_location")]
    // let p: *mut Memhdr;

    if !fp.is_null() {
        fprintf(
            fp,
            c"      MEMORY ALLOCATED : %lu, MAX was %lu\n".as_ptr() as _,
            DEBUG_MEM_SIZE,
            DEBUG_MAX_MEM_SIZE,
        );
    }
}

/**
 * xmlMemoryDump:
 *
 * Dump in-extenso the memory blocks allocated to the file .memorylist
 */
pub unsafe extern "C" fn xml_memory_dump() {}

/**
 * xmlMemMalloc:
 * @size:  an int specifying the size in byte to allocate.
 *
 * a malloc() equivalent, with logging of the allocation info.
 *
 * Returns a pointer to the allocated area or NULL in case of lack of memory.
 */
pub unsafe extern "C" fn xml_mem_malloc(size: usize) -> *mut c_void {
    xml_malloc_loc(size, c"none".as_ptr() as _, 0)
}

/**
 * xmlMemRealloc:
 * @ptr:  the initial memory block pointer
 * @size:  an int specifying the size in byte to allocate.
 *
 * a realloc() equivalent, with logging of the allocation info.
 *
 * Returns a pointer to the allocated area or NULL in case of lack of memory.
 */
pub unsafe extern "C" fn xml_mem_realloc(ptr: *mut c_void, size: usize) -> *mut c_void {
    xml_realloc_loc(ptr, size, c"none".as_ptr() as _, 0)
}

/**
 * xmlMemFree:
 * @ptr:  the memory block pointer
 *
 * a free() equivalent, with error checking.
 */
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
        DEBUG_MEM_SIZE -= (*p).mh_size as u64;
        DEBUG_MEM_BLOCKS -= 1;
        xml_mutex_unlock(addr_of_mut!(XML_MEM_MUTEX));

        free(p as _);

        return;
    }

    // error:
    generic_error!("xmlMemFree({:?}) error\n", ptr);
    xml_malloc_breakpoint();
}

/**
 * xmlMemoryStrdup:
 * @str:  the initial string pointer
 *
 * a strdup() equivalent, with logging of the allocation info.
 *
 * Returns a pointer to the new string or NULL if allocation error occurred.
 */
pub unsafe extern "C" fn xml_memory_strdup(str: *const XmlChar) -> *mut XmlChar {
    xml_mem_strdup_loc(str as _, c"none".as_ptr() as _, 0) as _
}

/**
 * xmlMallocLoc:
 * @size:  an int specifying the size in byte to allocate.
 * @file:  the file name or NULL
 * @line:  the line number
 *
 * a malloc() equivalent, with logging of the allocation info.
 *
 * Returns a pointer to the allocated area or NULL in case of lack of memory.
 */
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
    DEBUG_MEM_SIZE += size as u64;
    DEBUG_MEM_BLOCKS += 1;
    if DEBUG_MEM_SIZE > DEBUG_MAX_MEM_SIZE {
        DEBUG_MAX_MEM_SIZE = DEBUG_MEM_SIZE;
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

/**
 * xmlReallocLoc:
 * @ptr:  the initial memory block pointer
 * @size:  an int specifying the size in byte to allocate.
 * @file:  the file name or NULL
 * @line:  the line number
 *
 * a realloc() equivalent, with logging of the allocation info.
 *
 * Returns a pointer to the allocated area or NULL in case of lack of memory.
 */
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
    DEBUG_MEM_SIZE -= (*p).mh_size as u64;
    DEBUG_MEM_BLOCKS -= 1;
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
    DEBUG_MEM_SIZE += size as u64;
    DEBUG_MEM_BLOCKS += 1;
    if DEBUG_MEM_SIZE > DEBUG_MAX_MEM_SIZE {
        DEBUG_MAX_MEM_SIZE = DEBUG_MEM_SIZE;
    }
    xml_mutex_unlock(addr_of_mut!(XML_MEM_MUTEX));

    HDR_2_CLIENT!(p) as _

    //  error:
    // 	 return(null_mut());
}

/**
 * xmlMallocAtomicLoc:
 * @size:  an unsigned int specifying the size in byte to allocate.
 * @file:  the file name or NULL
 * @line:  the line number
 *
 * a malloc() equivalent, with logging of the allocation info.
 *
 * Returns a pointer to the allocated area or NULL in case of lack of memory.
 */
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
    DEBUG_MEM_SIZE += size as u64;
    DEBUG_MEM_BLOCKS += 1;
    if DEBUG_MEM_SIZE > DEBUG_MAX_MEM_SIZE {
        DEBUG_MAX_MEM_SIZE = DEBUG_MEM_SIZE;
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

/**
 * xmlMemStrdupLoc:
 * @str:  the initial string pointer
 * @file:  the file name or NULL
 * @line:  the line number
 *
 * a strdup() equivalent, with logging of the allocation info.
 *
 * Returns a pointer to the new string or NULL if allocation error occurred.
 */
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
    DEBUG_MEM_SIZE += size as u64;
    DEBUG_MEM_BLOCKS += 1;
    if DEBUG_MEM_SIZE > DEBUG_MAX_MEM_SIZE {
        DEBUG_MAX_MEM_SIZE = DEBUG_MEM_SIZE;
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

/**
 * xmlInitMemoryInternal:
 *
 * Initialize the memory layer.
 *
 * Returns 0 on success
 */
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

/**
 * xmlCleanupMemoryInternal:
 *
 * Free up all the memory allocated by the library for its own
 * use. This should not be called by user level code.
 */
pub(crate) unsafe extern "C" fn xml_cleanup_memory_internal() {
    xml_cleanup_mutex(addr_of_mut!(XML_MEM_MUTEX));
}

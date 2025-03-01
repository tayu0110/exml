//! Provide methods and data structures for handling threads.  
//! This module is based on `libxml/threads.h`, `threads.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: interfaces for thread handling
// Description: set of generic threading related routines
//              should work with pthreads, Windows native or TLS threads
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// threads.c: set of generic threading related routines
//
// See Copyright for the status of this software.
//
// Gary Pennington <Gary.Pennington@uk.sun.com>
// daniel@veillard.com

use std::{
    ffi::c_void,
    mem::{size_of, size_of_val},
    ptr::{null, null_mut},
};

use libc::{
    PTHREAD_MUTEX_INITIALIZER, free, malloc, memcpy, memset, pthread_cond_destroy,
    pthread_cond_init, pthread_cond_signal, pthread_cond_t, pthread_cond_wait, pthread_getspecific,
    pthread_key_create, pthread_key_delete, pthread_key_t, pthread_mutex_destroy,
    pthread_mutex_init, pthread_mutex_lock, pthread_mutex_t, pthread_mutex_unlock, pthread_self,
    pthread_setspecific, pthread_t,
};

use crate::libxml::globals::XmlGlobalStatePtr;

use super::{globals::XmlGlobalState, parser::xml_init_parser};

unsafe extern "C" {
    // Does it work ???
    fn pthread_equal(t1: pthread_t, t2: pthread_t) -> i32;
}

pub(crate) const XML_IS_THREADED: bool = true;
pub(crate) const XML_IS_NEVER_THREADED: bool = false;

// xmlMutex are a simple mutual exception locks.
pub type XmlMutexPtr = *mut XmlMutex;
// xmlMutex are a simple mutual exception locks
pub struct XmlMutex {
    pub(crate) lock: pthread_mutex_t,
}

// xmlRMutex are reentrant mutual exception locks.

pub type XmlRMutexPtr = *mut XmlRMutex;

// xmlRMutex are reentrant mutual exception locks
pub struct XmlRMutex {
    lock: pthread_mutex_t,
    held: u32,
    waiters: u32,
    tid: pthread_t,
    cv: pthread_cond_t,
}

pub(crate) static mut GLOBALKEY: pthread_key_t = 0;
pub(crate) static mut MAINTHREAD: pthread_t = 0;
pub(crate) static mut GLOBAL_INIT_LOCK: pthread_mutex_t = PTHREAD_MUTEX_INITIALIZER;

static mut XML_LIBRARY_LOCK: XmlRMutexPtr = null_mut();

/// xmlNewMutex() is used to allocate a libxml2 token struct for use in
/// synchronizing access to data.
///
/// Returns a new simple mutex pointer or NULL in case of error
#[doc(alias = "xmlNewMutex")]
pub unsafe extern "C" fn xml_new_mutex() -> XmlMutexPtr {
    unsafe {
        let tok: XmlMutexPtr = malloc(size_of::<XmlMutex>()) as _;
        if tok.is_null() {
            return null_mut();
        }
        xml_init_mutex(tok);
        tok
    }
}

/// xmlMutexLock() is used to lock a libxml2 token.
#[doc(alias = "xmlMutexLock")]
pub unsafe extern "C" fn xml_mutex_lock(tok: XmlMutexPtr) {
    unsafe {
        if tok.is_null() {
            return;
        }
        /*
         * This assumes that __libc_single_threaded won't change while the
         * lock is held.
         */
        if XML_IS_THREADED {
            pthread_mutex_lock(&mut (*tok).lock as _);
        }
    }
}

/// xmlMutexUnlock() is used to unlock a libxml2 token.
#[doc(alias = "xmlMutexUnlock")]
pub unsafe extern "C" fn xml_mutex_unlock(tok: XmlMutexPtr) {
    unsafe {
        if tok.is_null() {
            return;
        }
        if XML_IS_THREADED {
            pthread_mutex_unlock(&mut (*tok).lock as _);
        }
    }
}

/// Free a mutex.
#[doc(alias = "xmlFreeMutex")]
pub unsafe extern "C" fn xml_free_mutex(tok: XmlMutexPtr) {
    unsafe {
        if tok.is_null() {
            return;
        }

        xml_cleanup_mutex(tok);
        free(tok as _);
    }
}

/// xmlRNewMutex() is used to allocate a reentrant mutex for use in
/// synchronizing access to data. token_r is a re-entrant lock and thus useful
/// for synchronizing access to data structures that may be manipulated in a
/// recursive fashion.
///
/// Returns the new reentrant mutex pointer or NULL in case of error
#[doc(alias = "xmlNewRMutex")]
pub unsafe extern "C" fn xml_new_rmutex() -> XmlRMutexPtr {
    unsafe {
        let tok: XmlRMutexPtr = malloc(size_of::<XmlRMutex>()) as _;
        if tok.is_null() {
            return null_mut();
        }
        if !XML_IS_NEVER_THREADED {
            pthread_mutex_init(&mut (*tok).lock as _, null_mut());
            (*tok).held = 0;
            (*tok).waiters = 0;
            pthread_cond_init(&mut (*tok).cv as _, null_mut());
        }
        tok
    }
}

/// xmlRMutexLock() is used to lock a libxml2 token_r.
#[doc(alias = "xmlRMutexLock")]
pub unsafe extern "C" fn xml_rmutex_lock(tok: XmlRMutexPtr) {
    unsafe {
        if tok.is_null() {
            return;
        }
        if !XML_IS_THREADED {
            return;
        }

        pthread_mutex_lock(&mut (*tok).lock as _);
        if (*tok).held != 0 {
            if pthread_equal((*tok).tid, pthread_self()) != 0 {
                (*tok).held += 1;
                pthread_mutex_unlock(&mut (*tok).lock as _);
                return;
            } else {
                (*tok).waiters += 1;
                while (*tok).held != 0 {
                    pthread_cond_wait(&mut (*tok).cv as _, &mut (*tok).lock as _);
                }
                (*tok).waiters -= 1;
            }
        }
        (*tok).tid = pthread_self();
        (*tok).held = 1;
        pthread_mutex_unlock(&mut (*tok).lock as _);
    }
}

/// xmlRMutexUnlock() is used to unlock a libxml2 token_r.
#[doc(alias = "xmlRMutexUnlock")]
pub unsafe extern "C" fn xml_rmutex_unlock(tok: XmlRMutexPtr) {
    unsafe {
        if tok.is_null() {
            return;
        }
        if !XML_IS_THREADED {
            return;
        }

        pthread_mutex_lock(&mut (*tok).lock as _);
        (*tok).held -= 1;
        if (*tok).held == 0 {
            if (*tok).waiters != 0 {
                pthread_cond_signal(&mut (*tok).cv as _);
            }
            memset(&raw mut (*tok).tid as _, 0, size_of_val(&(*tok).tid));
        }
        pthread_mutex_unlock(&mut (*tok).lock as _);
    }
}

/// xmlRFreeMutex() is used to reclaim resources associated with a reentrant mutex.
#[doc(alias = "xmlFreeRMutex")]
pub unsafe extern "C" fn xml_free_rmutex(tok: XmlRMutexPtr) {
    unsafe {
        if tok.is_null() {
            return;
        }
        if !XML_IS_NEVER_THREADED {
            pthread_mutex_destroy(&mut (*tok).lock as _);
            pthread_cond_destroy(&mut (*tok).cv as _);
        }
        free(tok as _);
    }
}

#[doc(alias = "xmlInitThreads")]
#[deprecated = "Alias for xmlInitParser"]
pub unsafe extern "C" fn xml_init_threads() {
    unsafe {
        xml_init_parser();
    }
}

/// xmlLockLibrary() is used to take out a re-entrant lock on the libxml2 library.
#[doc(alias = "xmlLockLibrary")]
pub unsafe extern "C" fn xml_lock_library() {
    unsafe {
        xml_rmutex_lock(XML_LIBRARY_LOCK);
    }
}

/// xmlUnlockLibrary() is used to release a re-entrant lock on the libxml2 library.
#[doc(alias = "xmlUnlockLibrary")]
pub unsafe extern "C" fn xml_unlock_library() {
    unsafe {
        xml_rmutex_unlock(XML_LIBRARY_LOCK);
    }
}

/// xmlGetThreadId() find the current thread ID number
/// Note that this is likely to be broken on some platforms using pthreads
/// as the specification doesn't mandate pthread_t to be an integer type
///
/// Returns the current thread ID number
#[doc(alias = "xmlGetThreadId")]
pub(crate) unsafe extern "C" fn xml_get_thread_id() -> i32 {
    unsafe {
        let mut ret: i32 = 0;

        if !XML_IS_THREADED {
            return 0;
        }
        let id: pthread_t = pthread_self();
        /* horrible but preserves compat, see warning above */
        memcpy(&raw mut ret as _, &raw const id as _, size_of_val(&ret));
        ret
    }
}

/// xmlIsMainThread() check whether the current thread is the main thread.
///
/// Returns 1 if the current thread is the main thread, 0 otherwise
#[doc(alias = "xmlIsMainThread")]
pub(crate) unsafe extern "C" fn xml_is_main_thread() -> i32 {
    unsafe {
        xml_init_parser();

        if !XML_IS_THREADED {
            return 1;
        }
        pthread_equal(MAINTHREAD, pthread_self())
    }
}

#[doc(alias = "xmlCleanupThreads")]
#[deprecated = "This function is a no-op. Call xmlCleanupParser
to free global state but see the warnings there. xmlCleanupParser
should be only called once at program exit. In most cases, you don't
have call cleanup functions at all"]
pub unsafe extern "C" fn xml_cleanup_threads() {}

/// xmlNewGlobalState() allocates a global state. This structure is used to
/// hold all data for use by a thread when supporting backwards compatibility
/// of libxml2 to pre-thread-safe behaviour.
///
/// Returns the newly allocated xmlGlobalStatePtr or NULL in case of error
#[doc(alias = "xmlNewGlobalState")]
unsafe extern "C" fn xml_new_global_state() -> XmlGlobalStatePtr {
    unsafe {
        use crate::generic_error;

        use super::globals::xml_initialize_global_state;

        let gs: *mut XmlGlobalState = malloc(size_of::<XmlGlobalState>()) as _;
        if gs.is_null() {
            generic_error!("xmlGetGlobalState: out of memory\n");
            return null_mut();
        }

        memset(gs as _, 0, size_of::<XmlGlobalState>());
        xml_initialize_global_state(gs);
        gs
    }
}

/// xmlGetGlobalState() is called to retrieve the global state for a thread.
///
/// Returns the thread global state or NULL in case of error
#[doc(alias = "xmlGetGlobalState")]
pub(crate) unsafe extern "C" fn xml_get_global_state() -> XmlGlobalStatePtr {
    unsafe {
        if !XML_IS_THREADED {
            return null_mut();
        }

        let globalval: *mut XmlGlobalState = pthread_getspecific(GLOBALKEY) as *mut XmlGlobalState;
        if globalval.is_null() {
            let tsd: *mut XmlGlobalState = xml_new_global_state();
            if tsd.is_null() {
                return null_mut();
            }

            pthread_setspecific(GLOBALKEY, tsd as _);
            return tsd;
        }
        globalval
    }
}

// #if defined(LIBXML_THREAD_ENABLED) && defined(_WIN32) && \
//     !defined(HAVE_COMPILER_TLS) && defined(LIBXML_STATIC_FOR_DLL)
#[doc(hidden)]
#[cfg(target_os = "windows")]
pub unsafe extern "C" fn xmlDllMain(
    hinstDLL: *mut c_void,
    fdwReason: c_ulong,
    lpvReserved: *mut c_void,
) -> i32;
// #endif

/// Makes sure that the global initialization mutex is initialized and locks it.
#[doc(alias = "xmlGlobalInitMutexLock")]
pub(crate) unsafe extern "C" fn __xml_global_init_mutex_lock() {
    unsafe {
        /* Make sure the global init lock is initialized and then lock it. */
        if !XML_IS_THREADED {
            return;
        }
        /* The mutex is statically initialized, so we just lock it. */
        pthread_mutex_lock(&raw mut GLOBAL_INIT_LOCK);
    }
}

pub(crate) unsafe extern "C" fn __xml_global_init_mutex_unlock() {
    unsafe {
        if !XML_IS_THREADED {
            return;
        }
        pthread_mutex_unlock(&raw mut GLOBAL_INIT_LOCK);
    }
}

/// Makes sure that the global initialization mutex is destroyed before
/// application termination.
#[doc(alias = "xmlGlobalInitMutexDestroy")]
pub(crate) unsafe extern "C" fn __xml_global_init_mutex_destroy() {}

/// xmlFreeGlobalState() is called when a thread terminates with a non-NULL
/// global state. It is is used here to reclaim memory resources.
#[doc(alias = "xmlFreeGlobalState")]
unsafe extern "C" fn xml_free_global_state(state: *mut c_void) {
    unsafe {
        let gs: *mut XmlGlobalState = state as _;

        /* free any memory allocated in the thread's xmlLastError */
        (*gs).xml_last_error.reset();
        free(state);
    }
}

/// Used to to initialize all the thread related data.
#[doc(alias = "xmlInitThreadsInternal")]
pub(crate) unsafe extern "C" fn xml_init_threads_internal() {
    unsafe {
        pthread_key_create(&raw mut GLOBALKEY, Some(xml_free_global_state));
        MAINTHREAD = pthread_self();
    }
}

/// Used to to cleanup all the thread related data.
#[doc(alias = "xmlCleanupThreadsInternal")]
pub(crate) unsafe extern "C" fn xml_cleanup_threads_internal() {
    unsafe {
        pthread_key_delete(GLOBALKEY);
    }
}

/// Initialize a mutex.
#[doc(alias = "xmlInitMutex")]
pub(crate) unsafe extern "C" fn xml_init_mutex(mutex: XmlMutexPtr) {
    unsafe {
        if !XML_IS_NEVER_THREADED {
            pthread_mutex_init(&mut (*mutex).lock as _, null());
        }
    }
}

/// Reclaim resources associated with a mutex.
#[doc(alias = "xmlCleanupMutex")]
pub(crate) unsafe extern "C" fn xml_cleanup_mutex(mutex: XmlMutexPtr) {
    unsafe {
        if !XML_IS_NEVER_THREADED {
            pthread_mutex_destroy(&mut (*mutex).lock as _);
        }
    }
}

//! Provide internal methods and data structures for handling threads.  
//! This module is based on `private/threads.h`, `encoding.c` and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::c_void,
    ptr::{addr_of_mut, null},
};

use libc::{
    free, pthread_key_create, pthread_key_delete, pthread_mutex_destroy, pthread_mutex_init,
    pthread_mutex_lock, pthread_mutex_unlock, pthread_self,
};

use crate::libxml::{
    globals::XmlGlobalState,
    threads::{
        XmlMutexPtr, GLOBALKEY, GLOBAL_INIT_LOCK, MAINTHREAD, XML_IS_NEVER_THREADED,
        XML_IS_THREADED,
    },
    xmlerror::xmlResetError,
};

/**
 * xmlGlobalInitMutexLock
 *
 * Makes sure that the global initialization mutex is initialized and
 * locks it.
 */
pub(crate) unsafe extern "C" fn __xmlGlobalInitMutexLock() {
    /* Make sure the global init lock is initialized and then lock it. */
    if !XML_IS_THREADED {
        return;
    }
    /* The mutex is statically initialized, so we just lock it. */
    pthread_mutex_lock(addr_of_mut!(GLOBAL_INIT_LOCK));
}

pub(crate) unsafe extern "C" fn __xmlGlobalInitMutexUnlock() {
    if !XML_IS_THREADED {
        return;
    }
    pthread_mutex_unlock(addr_of_mut!(GLOBAL_INIT_LOCK));
}

/**
 * xmlGlobalInitMutexDestroy
 *
 * Makes sure that the global initialization mutex is destroyed before
 * application termination.
 */
pub(crate) unsafe extern "C" fn __xmlGlobalInitMutexDestroy() {}

/**
 * xmlFreeGlobalState:
 * @state:  a thread global state
 *
 * xmlFreeGlobalState() is called when a thread terminates with a non-NULL
 * global state. It is is used here to reclaim memory resources.
 */
unsafe extern "C" fn xmlFreeGlobalState(state: *mut c_void) {
    let gs: *mut XmlGlobalState = state as _;

    /* free any memory allocated in the thread's xmlLastError */
    xmlResetError(&mut (*gs).xmlLastError as _);
    free(state);
}

/**
 * xmlInitThreadsInternal:
 *
 * Used to to initialize all the thread related data.
 */
pub(crate) unsafe extern "C" fn xmlInitThreadsInternal() {
    pthread_key_create(addr_of_mut!(GLOBALKEY), Some(xmlFreeGlobalState));
    MAINTHREAD = pthread_self();
}

/**
 * xmlCleanupThreadsInternal:
 *
 * Used to to cleanup all the thread related data.
 */
pub(crate) unsafe extern "C" fn xmlCleanupThreadsInternal() {
    pthread_key_delete(GLOBALKEY);
}

/**
 * xmlInitMutex:
 * @mutex:  the mutex
 *
 * Initialize a mutex.
 */
pub(crate) unsafe extern "C" fn xmlInitMutex(mutex: XmlMutexPtr) {
    if !XML_IS_NEVER_THREADED {
        pthread_mutex_init(&mut (*mutex).lock as _, null());
    }
}

/**
 * xmlCleanupMutex:
 * @mutex:  the simple mutex
 *
 * Reclaim resources associated with a mutex.
 */
pub(crate) unsafe extern "C" fn xmlCleanupMutex(mutex: XmlMutexPtr) {
    if !XML_IS_NEVER_THREADED {
        pthread_mutex_destroy(&mut (*mutex).lock as _);
    }
}

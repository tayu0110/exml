//! Provide methods and data structures for dynamic module loading.  
//! This module is based on `libxml/xmlmodule.h`, `xmlmodule.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: dynamic module loading
// Description: basic API for dynamic module loading, used by libexslt added in 2.6.17
//
// Copy: See Copyright for the status of this software.
//
// Author: Joel W. Reed
// --------
// xmlmodule.c : basic API for dynamic module loading added 2.6.17
//
// See Copyright for the status of this software.
//
// joelwreed@comcast.net
//
// http://www.fortran-2000.com/ArnaudRecipes/sharedlib.html

use std::{
    ffi::c_char,
    mem::size_of,
    os::raw::c_void,
    ptr::{null, null_mut},
};

use libc::{dlclose, dlerror, dlopen, dlsym, memset, RTLD_GLOBAL, RTLD_NOW};

use crate::__xml_raise_error;

use super::{
    globals::{xml_free, xml_malloc},
    xmlstring::xml_strdup,
};

/// A handle to a dynamically loaded module
#[doc(alias = "xmlModulePtr")]
pub type XmlModulePtr = *mut XmlModule;
#[repr(C)]
pub struct XmlModule {
    name: *mut u8,
    handle: *mut c_void,
}

/// Enumeration of options that can be passed down to xmlModuleOpen()
#[doc(alias = "xmlModuleOption")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlModuleOption {
    XmlModuleLazy = 1,  /* lazy binding */
    XmlModuleLocal = 2, /* local binding */
}

/// Handle an out of memory condition
#[doc(alias = "xmlModuleErrMemory")]
unsafe extern "C" fn xml_module_err_memory(module: XmlModulePtr, extra: *const c_char) {
    let mut name: *const c_char = null();

    if !module.is_null() {
        name = (*module).name as _;
    }

    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromModule,
        XmlParserErrors::XmlErrNoMemory,
        XmlErrorLevel::XmlErrFatal,
        None,
        0,
        (!extra.is_null()).then(|| CStr::from_ptr(extra).to_string_lossy().into_owned().into()),
        (!name.is_null()).then(|| CStr::from_ptr(name).to_string_lossy().into_owned().into()),
        None,
        0,
        0,
        Some("Memory allocation failed : %s\n"),
        extra
    );
}

/// Returns a handle on success, and zero on error.
#[doc(alias = "xmlModulePlatformOpen")]
#[cfg(not(target_os = "windows"))]
unsafe extern "C" fn xml_module_platform_open(name: *const c_char) -> *mut c_void {
    dlopen(name, RTLD_GLOBAL | RTLD_NOW)
}

// returns a handle on success, and zero on error.
#[doc(alias = "xmlModulePlatformOpen")]
#[cfg(target_os = "windows")]
unsafe extern "C" fn xml_module_platform_open(name: *const c_char) -> *mut c_void {
    todo!("replace to libloading");
    LoadLibraryA(name)
}

/// Opens a module/shared library given its name or path
/// NOTE: that due to portability issues, behaviour can only be
/// guaranteed with @name using ASCII. We cannot guarantee that
/// an UTF-8 string would work, which is why name is a const char *
/// and not a const xmlChar * .
/// TODO: options are not yet implemented.
///
/// Returns a handle for the module or NULL in case of error
#[doc(alias = "xmlModuleOpen")]
pub unsafe extern "C" fn xml_module_open(name: *const c_char, _options: i32) -> XmlModulePtr {
    let module: XmlModulePtr = xml_malloc(size_of::<XmlModule>()) as XmlModulePtr;
    if module.is_null() {
        xml_module_err_memory(null_mut(), c"creating module".as_ptr() as _);
        return null_mut();
    }

    memset(module as _, 0, size_of::<XmlModule>());

    (*module).handle = xml_module_platform_open(name);

    if (*module).handle.is_null() {
        xml_free(module as _);
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromModule,
            XmlParserErrors::XmlModuleOpen,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            None,
            (!name.is_null()).then(|| CStr::from_ptr(name).to_string_lossy().into_owned().into()),
            None,
            0,
            0,
            Some("failed to open %s\n"),
            name
        );
        return null_mut();
    }

    (*module).name = xml_strdup(name as _);
    module
}

// http://www.opengroup.org/onlinepubs/009695399/functions/dlsym.html
// returns 0 on success and the loaded symbol in result, and -1 on error.
#[doc(alias = "xmlModulePlatformSymbol")]
#[cfg(not(target_os = "windows"))]
unsafe extern "C" fn xml_module_platform_symbol(
    handle: *mut c_void,
    name: *const c_char,
    symbol: *mut *mut c_void,
) -> i32 {
    *symbol = dlsym(handle, name);
    if !dlerror().is_null() {
        return -1;
    }
    0
}

// http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/getprocaddress.asp
// returns 0 on success and the loaded symbol in result, and -1 on error.
#[doc(alias = "xmlModulePlatformSymbol")]
#[cfg(target_os = "windows")]
unsafe extern "C" fn xml_module_platform_symbol(
    handle: *mut c_void,
    name: *const c_char,
    symbol: *mut *mut c_void,
) -> i32 {
    todo!("replace to libloading");
    *symbol = GetProcAddress(handle, name);
    -((*symbol).is_null() as i32)
}

/// Lookup for a symbol address in the given module
/// NOTE: that due to portability issues, behaviour can only be
/// guaranteed with @name using ASCII. We cannot guarantee that
/// an UTF-8 string would work, which is why name is a const c_char *
/// and not a const xmlChar * .
///
/// Returns 0 if the symbol was found, or -1 in case of error
#[doc(alias = "xmlModuleSymbol")]
pub unsafe extern "C" fn xml_module_symbol(
    module: XmlModulePtr,
    name: *const c_char,
    symbol: *mut *mut c_void,
) -> i32 {
    let mut rc: i32 = -1;

    if module.is_null() || symbol.is_null() || name.is_null() {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromModule,
            XmlParserErrors::XmlModuleOpen,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            None,
            None,
            None,
            0,
            0,
            Some("null parameter\n"),
        );
        return rc;
    }

    rc = xml_module_platform_symbol((*module).handle, name, symbol);

    if rc == -1 {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromModule,
            XmlParserErrors::XmlModuleOpen,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            None,
            (!name.is_null()).then(|| CStr::from_ptr(name).to_string_lossy().into_owned().into()),
            None,
            0,
            0,
            Some("failed to find symbol: %s\n"),
            if name.is_null() {
                c"NULL".as_ptr()
            } else {
                name
            }
        );
        return rc;
    }

    rc
}

/// Returns 0 on success, and non-zero on error.
#[doc(alias = "xmlModulePlatformClose")]
#[cfg(not(target_os = "windows"))]
unsafe extern "C" fn xml_module_platform_close(handle: *mut c_void) -> i32 {
    dlclose(handle)
}

/// Returns 0 on success, and non-zero on error.
#[doc(alias = "xmlModulePlatformClose")]
#[cfg(target_os = "windows")]
unsafe extern "C" fn xml_module_platform_close(handle: *mut c_void) -> i32 {
    let rc: i32;

    todo!("replace to libloading");
    rc = FreeLibrary(handle);
    (0 == rc) as i32
}

/// The close operations unload the associated module and free the
/// data associated to the module.
///
/// Returns 0 in case of success, -1 in case of argument error and -2
/// if the module could not be closed/unloaded.
#[doc(alias = "xmlModuleClose")]
pub unsafe extern "C" fn xml_module_close(module: XmlModulePtr) -> i32 {
    if module.is_null() {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromModule,
            XmlParserErrors::XmlModuleClose,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            None,
            None,
            None,
            0,
            0,
            Some("null module pointer\n"),
        );
        return -1;
    }

    let rc: i32 = xml_module_platform_close((*module).handle);

    if rc != 0 {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromModule,
            XmlParserErrors::XmlModuleClose,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            None,
            (!(*module).name.is_null()).then(|| CStr::from_ptr((*module).name as *const i8)
                .to_string_lossy()
                .into_owned()
                .into()),
            None,
            0,
            0,
            Some("failed to close: %s\n"),
            (*module).name
        );
        return -2;
    }

    xml_module_free(module)
}

/// The free operations free the data associated to the module
/// but does not unload the associated shared library which may still be in use.
///
/// Returns 0 in case of success, -1 in case of argument error
#[doc(alias = "xmlModuleFree")]
pub unsafe extern "C" fn xml_module_free(module: XmlModulePtr) -> i32 {
    if module.is_null() {
        __xml_raise_error!(
            None,
            None,
            None,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromModule,
            XmlParserErrors::XmlModuleClose,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            None,
            None,
            None,
            0,
            0,
            Some("null module pointer\n"),
        );
        return -1;
    }

    xml_free((*module).name as _);
    xml_free(module as _);

    0
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_module_close() {
        #[cfg(feature = "libxml_modules")]
        unsafe {
            let mut leaks = 0;

            for n_module in 0..GEN_NB_XML_MODULE_PTR {
                let mem_base = xml_mem_blocks();
                let module = gen_xml_module_ptr(n_module, 0);

                let ret_val = xml_module_close(module);
                desret_int(ret_val);
                des_xml_module_ptr(n_module, module, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlModuleClose",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlModuleClose()");
                    eprintln!(" {}", n_module);
                }
            }
        }
    }

    #[test]
    fn test_xml_module_open() {

        /* missing type support */
    }

    #[test]
    fn test_xml_module_symbol() {
        #[cfg(feature = "libxml_modules")]
        unsafe {
            let mut leaks = 0;

            for n_module in 0..GEN_NB_XML_MODULE_PTR {
                for n_name in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_symbol in 0..GEN_NB_VOID_PTR_PTR {
                        let mem_base = xml_mem_blocks();
                        let module = gen_xml_module_ptr(n_module, 0);
                        let name = gen_const_char_ptr(n_name, 1);
                        let symbol = gen_void_ptr_ptr(n_symbol, 2);

                        let ret_val = xml_module_symbol(module, name, symbol);
                        desret_int(ret_val);
                        des_xml_module_ptr(n_module, module, 0);
                        des_const_char_ptr(n_name, name, 1);
                        des_void_ptr_ptr(n_symbol, symbol, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlModuleSymbol",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlModuleSymbol()");
                            eprint!(" {}", n_module);
                            eprint!(" {}", n_name);
                            eprintln!(" {}", n_symbol);
                        }
                    }
                }
            }
        }
    }
}

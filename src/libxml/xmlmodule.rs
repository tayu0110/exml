//! Provide methods and data structures for dynamic module loading.  
//! This module is based on `libxml/xmlmodule.h`, `xmlmodule.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, c_uchar},
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

/**
 * xmlModulePtr:
 *
 * A handle to a dynamically loaded module
 */
pub type XmlModulePtr = *mut XmlModule;
#[repr(C)]
pub struct XmlModule {
    name: *mut c_uchar,
    handle: *mut c_void,
}

/**
 * xmlModuleOption:
 *
 * enumeration of options that can be passed down to xmlModuleOpen()
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlModuleOption {
    XmlModuleLazy = 1,  /* lazy binding */
    XmlModuleLocal = 2, /* local binding */
}

/**
 * xmlModuleErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
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
        null_mut(),
        0,
        extra,
        name,
        null_mut(),
        0,
        0,
        c"Memory allocation failed : %s\n".as_ptr(),
        extra
    );
}

/**
 * xmlModulePlatformOpen:
 * @name: path to the module
 *
 * returns a handle on success, and zero on error.
 */
#[cfg(not(target_os = "windows"))]
unsafe extern "C" fn xml_module_platform_open(name: *const c_char) -> *mut c_void {
    dlopen(name, RTLD_GLOBAL | RTLD_NOW)
}

/*
 * xmlModulePlatformOpen:
 * returns a handle on success, and zero on error.
 */
#[cfg(target_os = "windows")]
unsafe extern "C" fn xmlModulePlatformOpen(name: *const c_char) -> *mut c_void {
    todo!("replace to libloading");
    LoadLibraryA(name)
}

/**
 * xmlModuleOpen:
 * @name: the module name
 * @options: a set of xmlModuleOption
 *
 * Opens a module/shared library given its name or path
 * NOTE: that due to portability issues, behaviour can only be
 * guaranteed with @name using ASCII. We cannot guarantee that
 * an UTF-8 string would work, which is why name is a const char *
 * and not a const xmlChar * .
 * TODO: options are not yet implemented.
 *
 * Returns a handle for the module or NULL in case of error
 */
pub unsafe extern "C" fn xml_module_open(name: *const c_char, _options: c_int) -> XmlModulePtr {
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
            null_mut(),
            0,
            null_mut(),
            name,
            null_mut(),
            0,
            0,
            c"failed to open %s\n".as_ptr(),
            name
        );
        return null_mut();
    }

    (*module).name = xml_strdup(name as _);
    module
}

/*
 * xmlModulePlatformSymbol:
 * http://www.opengroup.org/onlinepubs/009695399/functions/dlsym.html
 * returns 0 on success and the loaded symbol in result, and -1 on error.
 */
#[cfg(not(target_os = "windows"))]
unsafe extern "C" fn xml_module_platform_symbol(
    handle: *mut c_void,
    name: *const c_char,
    symbol: *mut *mut c_void,
) -> c_int {
    *symbol = dlsym(handle, name);
    if !dlerror().is_null() {
        return -1;
    }
    0
}

/*
 * xmlModulePlatformSymbol:
 * http://msdn.microsoft.com/library/default.asp?url=/library/en-us/dllproc/base/getprocaddress.asp
 * returns 0 on success and the loaded symbol in result, and -1 on error.
 */
#[cfg(target_os = "windows")]
unsafe extern "C" fn xmlModulePlatformSymbol(
    handle: *mut c_void,
    name: *const c_char,
    symbol: *mut *mut c_void,
) -> c_int {
    todo!("replace to libloading");
    *symbol = GetProcAddress(handle, name);
    -((*symbol).is_null() as i32)
}

/**
 * xmlModuleSymbol:
 * @module: the module
 * @name: the name of the symbol
 * @symbol: the resulting symbol address
 *
 * Lookup for a symbol address in the given module
 * NOTE: that due to portability issues, behaviour can only be
 * guaranteed with @name using ASCII. We cannot guarantee that
 * an UTF-8 string would work, which is why name is a const c_char *
 * and not a const xmlChar * .
 *
 * Returns 0 if the symbol was found, or -1 in case of error
 */
pub unsafe extern "C" fn xml_module_symbol(
    module: XmlModulePtr,
    name: *const c_char,
    symbol: *mut *mut c_void,
) -> c_int {
    let mut rc: c_int = -1;

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
            null_mut(),
            0,
            null_mut(),
            null_mut(),
            null_mut(),
            0,
            0,
            c"null parameter\n".as_ptr(),
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
            null_mut(),
            0,
            null_mut(),
            name,
            null_mut(),
            0,
            0,
            c"failed to find symbol: %s\n".as_ptr(),
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

/*
 * xmlModulePlatformClose:
 * @handle: handle to the module
 *
 * returns 0 on success, and non-zero on error.
 */
#[cfg(not(target_os = "windows"))]
unsafe extern "C" fn xml_module_platform_close(handle: *mut c_void) -> c_int {
    dlclose(handle)
}

/*
 * xmlModulePlatformClose:
 * returns 0 on success, and non-zero on error.
 */
#[cfg(target_os = "windows")]
unsafe extern "C" fn xmlModulePlatformClose(handle: *mut c_void) -> c_int {
    let rc: c_int;

    todo!("replace to libloading");
    rc = FreeLibrary(handle);
    (0 == rc) as i32
}

/**
 * xmlModuleClose:
 * @module: the module handle
 *
 * The close operations unload the associated module and free the
 * data associated to the module.
 *
 * Returns 0 in case of success, -1 in case of argument error and -2
 *         if the module could not be closed/unloaded.
 */
pub unsafe extern "C" fn xml_module_close(module: XmlModulePtr) -> c_int {
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
            null_mut(),
            0,
            null_mut(),
            null_mut(),
            null_mut(),
            0,
            0,
            c"null module pointer\n".as_ptr(),
        );
        return -1;
    }

    let rc: c_int = xml_module_platform_close((*module).handle);

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
            null_mut(),
            0,
            null_mut(),
            (*module).name as _,
            null_mut(),
            0,
            0,
            c"failed to close: %s\n".as_ptr(),
            (*module).name
        );
        return -2;
    }

    xml_module_free(module)
}

/**
 * xmlModuleFree:
 * @module: the module handle
 *
 * The free operations free the data associated to the module
 * but does not unload the associated shared library which may still
 * be in use.
 *
 * Returns 0 in case of success, -1 in case of argument error
 */
pub unsafe extern "C" fn xml_module_free(module: XmlModulePtr) -> c_int {
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
            null_mut(),
            0,
            null_mut(),
            null_mut(),
            null_mut(),
            0,
            0,
            c"null module pointer\n".as_ptr(),
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

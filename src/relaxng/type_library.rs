use std::{
    cell::Cell,
    ffi::{CStr, c_void},
    ptr::{drop_in_place, null_mut},
};

use crate::{
    generic_error,
    hash::XmlHashTableRef,
    libxml::{
        globals::{xml_free, xml_malloc},
        schemas_internals::XmlSchemaTypeType,
        xmlschemastypes::{
            XmlSchemaValPtr, xml_schema_check_facet, xml_schema_cleanup_types,
            xml_schema_compare_values, xml_schema_free_facet, xml_schema_free_value,
            xml_schema_get_predefined_type, xml_schema_new_facet, xml_schema_val_predef_type_node,
            xml_schema_validate_facet,
        },
        xmlstring::xml_str_equal,
    },
    tree::XmlGenericNodePtr,
};

use super::{XML_RELAXNG_NS, relaxng_normalize, xml_rng_verr_memory};

/// Function provided by a type library to check if a type is exported
///
/// Returns 1 if yes, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGTypeHave")]
type XmlRelaxNGTypeHave = unsafe fn(data: *mut c_void, typ: &str) -> bool;

/// Function provided by a type library to check if a value match a type
///
/// Returns 1 if yes, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGTypeCheck")]
type XmlRelaxNGTypeCheck = unsafe fn(
    data: *mut c_void,
    typ: &str,
    value: *const u8,
    result: *mut *mut c_void,
    node: Option<XmlGenericNodePtr>,
) -> i32;

/// Function provided by a type library to check a value facet
///
/// Returns 1 if yes, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGFacetCheck")]
type XmlRelaxNGFacetCheck = unsafe fn(
    data: *mut c_void,
    typ: &str,
    facet: &str,
    val: *const u8,
    strval: *const u8,
    value: *mut c_void,
) -> i32;

/// Function provided by a type library to free a returned result
#[doc(alias = "xmlRelaxNGTypeFree")]
type XmlRelaxNGTypeFree = unsafe fn(data: *mut c_void, result: *mut c_void);

/// Function provided by a type library to compare two values accordingly to a type.
///
/// Returns 1 if yes, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGTypeCompare")]
type XmlRelaxNGTypeCompare = unsafe fn(
    data: *mut c_void,
    typ: &str,
    value1: *const u8,
    ctxt1: Option<XmlGenericNodePtr>,
    comp1: *mut c_void,
    value2: *const u8,
    ctxt2: Option<XmlGenericNodePtr>,
) -> i32;

pub type XmlRelaxNGTypeLibraryPtr = *mut XmlRelaxNGTypeLibrary;
#[repr(C)]
pub struct XmlRelaxNGTypeLibrary {
    namespace: String,                              // the datatypeLibrary value
    pub(crate) data: *mut c_void,                   // data needed for the library
    pub(crate) have: Option<XmlRelaxNGTypeHave>,    // the export function
    pub(crate) check: Option<XmlRelaxNGTypeCheck>,  // the checking function
    pub(crate) comp: Option<XmlRelaxNGTypeCompare>, // the compare function
    pub(crate) facet: Option<XmlRelaxNGFacetCheck>, // the facet check function
    pub(crate) freef: Option<XmlRelaxNGTypeFree>,   // the freeing function
}

impl Default for XmlRelaxNGTypeLibrary {
    fn default() -> Self {
        Self {
            namespace: "".to_string(),
            data: null_mut(),
            have: None,
            check: None,
            comp: None,
            facet: None,
            freef: None,
        }
    }
}

thread_local! {
    pub(crate) static XML_RELAXNG_TYPE_INITIALIZED: Cell<bool> = const { Cell::new(false) };
    pub(crate) static XML_RELAXNG_REGISTERED_TYPES: Cell<Option<XmlHashTableRef<'static, XmlRelaxNGTypeLibraryPtr>>> =
        const { Cell::new(None) };
}

/// Initialize the default type libraries.
///
/// Returns 0 in case of success and -1 in case of error.
#[doc(alias = "xmlRelaxNGInitTypes")]
pub unsafe fn xml_relaxng_init_types() -> i32 {
    unsafe {
        if XML_RELAXNG_TYPE_INITIALIZED.get() {
            return 0;
        }

        let Some(registered_types) = XmlHashTableRef::with_capacity(10) else {
            generic_error!("Failed to allocate sh table for Relax-NG types\n");
            return -1;
        };

        XML_RELAXNG_REGISTERED_TYPES.set(Some(registered_types));
        xml_relaxng_register_type_library(
            "http://www.w3.org/2001/XMLSchema-datatypes",
            null_mut(),
            Some(xml_relaxng_schema_type_have),
            Some(xml_relaxng_schema_type_check),
            Some(xml_relaxng_schema_type_compare),
            Some(xml_relaxng_schema_facet_check),
            Some(xml_relaxng_schema_free_value),
        );
        xml_relaxng_register_type_library(
            XML_RELAXNG_NS,
            null_mut(),
            Some(xml_relaxng_default_type_have),
            Some(xml_relaxng_default_type_check),
            Some(xml_relaxng_default_type_compare),
            None,
            None,
        );
        XML_RELAXNG_TYPE_INITIALIZED.set(true);
        0
    }
}

/// Register a new type library
///
/// Returns 0 in case of success and -1 in case of error.
#[doc(alias = "xmlRelaxNGRegisterTypeLibrary")]
unsafe fn xml_relaxng_register_type_library(
    namespace: &str,
    data: *mut c_void,
    have: Option<XmlRelaxNGTypeHave>,
    check: Option<XmlRelaxNGTypeCheck>,
    comp: Option<XmlRelaxNGTypeCompare>,
    facet: Option<XmlRelaxNGFacetCheck>,
    freef: Option<XmlRelaxNGTypeFree>,
) -> i32 {
    unsafe {
        let Some(mut registered_types) = XML_RELAXNG_REGISTERED_TYPES.get() else {
            return -1;
        };
        if check.is_none() || comp.is_none() {
            return -1;
        }
        if registered_types.lookup(namespace).is_some() {
            generic_error!("Relax-NG types library '{namespace}' already registered\n");
            return -1;
        }
        let lib: XmlRelaxNGTypeLibraryPtr = xml_malloc(size_of::<XmlRelaxNGTypeLibrary>()) as _;
        if lib.is_null() {
            xml_rng_verr_memory(null_mut(), "adding types library\n");
            return -1;
        }
        std::ptr::write(&mut *lib, XmlRelaxNGTypeLibrary::default());
        (*lib).namespace = namespace.to_owned();
        (*lib).data = data;
        (*lib).have = have;
        (*lib).comp = comp;
        (*lib).check = check;
        (*lib).facet = facet;
        (*lib).freef = freef;
        match registered_types.add_entry(namespace, lib) {
            Ok(_) => 0,
            Err(_) => {
                generic_error!("Relax-NG types library failed to register '{namespace}'\n");
                xml_relaxng_free_type_library(lib as _);
                -1
            }
        }
    }
}

/// Free the structure associated to the type library
#[doc(alias = "xmlRelaxNGFreeTypeLibrary")]
unsafe fn xml_relaxng_free_type_library(lib: XmlRelaxNGTypeLibraryPtr) {
    unsafe {
        if lib.is_null() {
            return;
        }
        drop_in_place(lib);
        xml_free(lib as _);
    }
}

/// Cleanup the default Schemas type library associated to RelaxNG
#[doc(alias = "xmlRelaxNGCleanupTypes")]
pub(crate) unsafe fn xml_relaxng_cleanup_types() {
    unsafe {
        xml_schema_cleanup_types();
        if !XML_RELAXNG_TYPE_INITIALIZED.get() {
            return;
        }
        if let Some(mut table) = XML_RELAXNG_REGISTERED_TYPES.take().map(|t| t.into_inner()) {
            table.clear_with(|data, _| {
                xml_relaxng_free_type_library(data);
            });
        }
        XML_RELAXNG_TYPE_INITIALIZED.set(false);
    }
}

/// Check if the given type is provided by the W3C XMLSchema Datatype library.
///
/// Returns 1 if yes, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGSchemaTypeHave")]
unsafe fn xml_relaxng_schema_type_have(_data: *mut c_void, typ: &str) -> bool {
    unsafe {
        let _typ = xml_schema_get_predefined_type(typ, "http://www.w3.org/2001/XMLSchema");
        !_typ.is_null()
    }
}

/// Check if the given type and value are validated by the W3C XMLSchema Datatype library.
///
/// Returns 1 if yes, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGSchemaTypeCheck")]
unsafe fn xml_relaxng_schema_type_check(
    _data: *mut c_void,
    r#type: &str,
    value: *const u8,
    result: *mut *mut c_void,
    node: Option<XmlGenericNodePtr>,
) -> i32 {
    unsafe {
        if value.is_null() {
            return -1;
        }
        let typ = xml_schema_get_predefined_type(r#type, "http://www.w3.org/2001/XMLSchema");
        if typ.is_null() {
            return -1;
        }
        let ret = xml_schema_val_predef_type_node(typ, value, result as _, node);
        if ret == 2 {
            // special ID error code
            return 2;
        }
        if ret == 0 {
            return 1;
        }
        if ret > 0 {
            return 0;
        }
        -1
    }
}

/// Compare two values for equality accordingly a type from the W3C XMLSchema Datatype library.
///
/// Returns 1 if equal, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGSchemaTypeCompare")]
unsafe fn xml_relaxng_schema_type_compare(
    _data: *mut c_void,
    r#type: &str,
    value1: *const u8,
    ctxt1: Option<XmlGenericNodePtr>,
    comp1: *mut c_void,
    value2: *const u8,
    ctxt2: Option<XmlGenericNodePtr>,
) -> i32 {
    unsafe {
        let mut ret: i32;
        let mut res1: XmlSchemaValPtr = null_mut();
        let mut res2: XmlSchemaValPtr = null_mut();

        if value1.is_null() || value2.is_null() {
            return -1;
        }
        let typ = xml_schema_get_predefined_type(r#type, "http://www.w3.org/2001/XMLSchema");
        if typ.is_null() {
            return -1;
        }
        if comp1.is_null() {
            ret = xml_schema_val_predef_type_node(typ, value1, &raw mut res1, ctxt1);
            if ret != 0 {
                return -1;
            }
            if res1.is_null() {
                return -1;
            }
        } else {
            res1 = comp1 as _;
        }
        ret = xml_schema_val_predef_type_node(typ, value2, &raw mut res2, ctxt2);
        if ret != 0 {
            if res1 != comp1 as _ {
                xml_schema_free_value(res1);
            }
            return -1;
        }
        ret = xml_schema_compare_values(res1, res2);
        if res1 != comp1 as _ {
            xml_schema_free_value(res1);
        }
        xml_schema_free_value(res2);
        if ret == -2 {
            return -1;
        }
        if ret == 0 {
            return 1;
        }
        0
    }
}

/// Function provided by a type library to check a value facet
///
/// Returns 1 if yes, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGSchemaFacetCheck")]
unsafe fn xml_relaxng_schema_facet_check(
    _data: *mut c_void,
    r#type: &str,
    facetname: &str,
    val: *const u8,
    strval: *const u8,
    value: *mut c_void,
) -> i32 {
    unsafe {
        let mut ret: i32;

        if strval.is_null() {
            return -1;
        }
        let typ = xml_schema_get_predefined_type(r#type, "http://www.w3.org/2001/XMLSchema");
        if typ.is_null() {
            return -1;
        }

        let facet = xml_schema_new_facet();
        if facet.is_null() {
            return -1;
        }

        if facetname == "minInclusive" {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMinInclusive;
        } else if facetname == "minExclusive" {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMinExclusive;
        } else if facetname == "maxInclusive" {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMaxInclusive;
        } else if facetname == "maxExclusive" {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMaxExclusive;
        } else if facetname == "totalDigits" {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetTotalDigits;
        } else if facetname == "fractionDigits" {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetFractionDigits;
        } else if facetname == "pattern" {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetPattern;
        } else if facetname == "enumeration" {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetEnumeration;
        } else if facetname == "whiteSpace" {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetWhitespace;
        } else if facetname == "length" {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetLength;
        } else if facetname == "maxLength" {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMaxLength;
        } else if facetname == "minLength" {
            (*facet).typ = XmlSchemaTypeType::XmlSchemaFacetMinLength;
        } else {
            xml_schema_free_facet(facet);
            return -1;
        }
        (*facet).value = val;
        ret = xml_schema_check_facet(facet, typ, null_mut(), typ as _);
        if ret != 0 {
            xml_schema_free_facet(facet);
            return -1;
        }
        ret = xml_schema_validate_facet(typ, facet, strval, value as _);
        xml_schema_free_facet(facet);
        if ret != 0 {
            return -1;
        }
        0
    }
}

/// Function provided by a type library to free a Schemas value
///
/// Returns 1 if yes, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGSchemaFreeValue")]
unsafe fn xml_relaxng_schema_free_value(_data: *mut c_void, value: *mut c_void) {
    unsafe {
        xml_schema_free_value(value as _);
    }
}

/// Check if the given type is provided by the default datatype library.
///
/// Returns 1 if yes, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGDefaultTypeHave")]
fn xml_relaxng_default_type_have(_data: *mut c_void, typ: &str) -> bool {
    typ == "string" || typ == "token"
}

/// Check if the given type and value are validated by the default datatype library.
///
/// Returns 1 if yes, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGDefaultTypeCheck")]
fn xml_relaxng_default_type_check(
    _data: *mut c_void,
    typ: &str,
    value: *const u8,
    _result: *mut *mut c_void,
    _node: Option<XmlGenericNodePtr>,
) -> i32 {
    if value.is_null() {
        return -1;
    }
    if typ == "string" || typ == "token" {
        return 1;
    }

    0
}

/// Compare two values accordingly a type from the default datatype library.
///
/// Returns 1 if yes, 0 if no and -1 in case of error.
#[doc(alias = "xmlRelaxNGDefaultTypeCompare")]
unsafe fn xml_relaxng_default_type_compare(
    _data: *mut c_void,
    typ: &str,
    value1: *const u8,
    _ctxt1: Option<XmlGenericNodePtr>,
    _comp1: *mut c_void,
    value2: *const u8,
    _ctxt2: Option<XmlGenericNodePtr>,
) -> i32 {
    unsafe {
        let mut ret: i32 = -1;

        if typ == "string" {
            ret = xml_str_equal(value1, value2) as i32;
        } else if typ == "token" {
            if !xml_str_equal(value1, value2) {
                let value1 = CStr::from_ptr(value1 as *const i8).to_string_lossy();
                let value2 = CStr::from_ptr(value2 as *const i8).to_string_lossy();
                // TODO: trivial optimizations are possible by
                // computing at compile-time
                let nval = relaxng_normalize(&value1);
                let nvalue = relaxng_normalize(&value2);
                ret = (nval == nvalue) as i32;
            } else {
                ret = 1;
            }
        }
        ret
    }
}

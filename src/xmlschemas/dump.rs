use std::{
    ffi::{CStr, CString, c_void},
    io::Write,
};

use crate::{
    libxml::{
        schemas_internals::{
            XML_SCHEMAS_ELEM_ABSTRACT, XML_SCHEMAS_ELEM_DEFAULT, XML_SCHEMAS_ELEM_FIXED,
            XML_SCHEMAS_ELEM_GLOBAL, XML_SCHEMAS_ELEM_NILLABLE, XmlSchemaAnnotPtr,
            XmlSchemaContentType, XmlSchemaTypeType,
        },
        xmlschemas::xml_schema_format_qname,
    },
    xmlschemas::items::{
        XmlSchemaAttributeUseProhibPtr, XmlSchemaAttributeUsePtr, XmlSchemaElementPtr,
        XmlSchemaQNameRefPtr, XmlSchemaTreeItemPtr,
    },
};

use super::{
    item_list::XmlSchemaItemListPtr,
    items::{XmlSchemaParticlePtr, XmlSchemaTypePtr},
    schema::XmlSchemaPtr,
};

const UNBOUNDED: usize = 1 << 30;

/// Dumps a list of attribute use components.
#[doc(alias = "xmlSchemaAttrUsesDump")]
unsafe fn xml_schema_attr_uses_dump<'a>(
    uses: XmlSchemaItemListPtr<*mut c_void>,
    output: &mut (impl Write + 'a),
) {
    unsafe {
        let mut prohib: XmlSchemaAttributeUseProhibPtr;
        let mut refe: XmlSchemaQNameRefPtr;
        let mut name: *const u8;
        let mut tns: *const u8;

        if uses.is_null() || (*uses).items.is_empty() {
            return;
        }

        writeln!(output, "  attributes:");
        for using in (*uses)
            .items
            .iter()
            .map(|&using| using as XmlSchemaAttributeUsePtr)
        {
            if (*using).typ == XmlSchemaTypeType::XmlSchemaExtraAttrUseProhib {
                write!(output, "  [prohibition] ");
                prohib = using as XmlSchemaAttributeUseProhibPtr;
                name = (*prohib).name;
                tns = (*prohib).target_namespace;
            } else if (*using).typ == XmlSchemaTypeType::XmlSchemaExtraQNameRef {
                write!(output, "  [reference] ");
                refe = using as XmlSchemaQNameRefPtr;
                name = (*refe).name;
                tns = (*refe).target_namespace;
            } else {
                write!(output, "  [use] ");
                name = (*(*using).attr_decl).name;
                tns = (*(*using).attr_decl).target_namespace;
            }
            writeln!(
                output,
                "'{}'",
                xml_schema_format_qname(
                    Some(CStr::from_ptr(tns as *const i8).to_string_lossy().as_ref()),
                    Some(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref())
                )
            );
        }
    }
}

/// Dump the annotation
#[doc(alias = "xmlSchemaAnnotDump")]
unsafe fn xml_schema_annot_dump<'a>(output: &mut (impl Write + 'a), annot: XmlSchemaAnnotPtr) {
    unsafe {
        if annot.is_null() {
            return;
        }

        if let Some(content) = (*annot).content.get_content() {
            writeln!(output, "  Annot: {content}");
        } else {
            writeln!(output, "  Annot: empty");
        }
    }
}

/// Dump a SchemaType structure
#[doc(alias = "xmlSchemaContentModelDump")]
unsafe fn xml_schema_content_model_dump<'a>(
    particle: XmlSchemaParticlePtr,
    output: &mut (impl Write + 'a),
    depth: i32,
) {
    unsafe {
        if particle.is_null() {
            return;
        }
        let shift = "  ".repeat(depth.clamp(0, 25) as usize);
        write!(output, "{}", shift);
        if (*particle).children.is_null() {
            writeln!(output, "MISSING particle term");
            return;
        }
        let term: XmlSchemaTreeItemPtr = (*particle).children;
        if term.is_null() {
            write!(output, "(NULL)");
        } else {
            match (*term).typ {
                XmlSchemaTypeType::XmlSchemaTypeElement => {
                    write!(
                        output,
                        "ELEM '{}'",
                        xml_schema_format_qname(
                            Some(
                                CStr::from_ptr(
                                    (*(term as XmlSchemaElementPtr)).target_namespace as *const i8
                                )
                                .to_string_lossy()
                                .as_ref()
                            ),
                            Some(
                                CStr::from_ptr((*(term as XmlSchemaElementPtr)).name as *const i8)
                                    .to_string_lossy()
                                    .as_ref()
                            ),
                        )
                    );
                }
                XmlSchemaTypeType::XmlSchemaTypeSequence => {
                    write!(output, "SEQUENCE");
                }
                XmlSchemaTypeType::XmlSchemaTypeChoice => {
                    write!(output, "CHOICE");
                }
                XmlSchemaTypeType::XmlSchemaTypeAll => {
                    write!(output, "ALL");
                }
                XmlSchemaTypeType::XmlSchemaTypeAny => {
                    write!(output, "ANY");
                }
                _ => {
                    writeln!(output, "UNKNOWN");
                    return;
                }
            }
        }
        if (*particle).min_occurs != 1 {
            write!(output, " min: {}", (*particle).min_occurs);
        }
        if (*particle).max_occurs >= UNBOUNDED as i32 {
            write!(output, " max: unbounded");
        } else if (*particle).max_occurs != 1 {
            write!(output, " max: {}", (*particle).max_occurs);
        }
        writeln!(output);
        if !term.is_null()
            && matches!(
                (*term).typ,
                XmlSchemaTypeType::XmlSchemaTypeSequence
                    | XmlSchemaTypeType::XmlSchemaTypeChoice
                    | XmlSchemaTypeType::XmlSchemaTypeAll
            )
            && !(*term).children.is_null()
        {
            xml_schema_content_model_dump(
                (*term).children as XmlSchemaParticlePtr,
                output,
                depth + 1,
            );
        }
        if !(*particle).next.is_null() {
            xml_schema_content_model_dump((*particle).next as XmlSchemaParticlePtr, output, depth);
        }
    }
}

/// Dump a SchemaType structure
#[doc(alias = "xmlSchemaTypeDump")]
unsafe fn xml_schema_type_dump<'a>(typ: XmlSchemaTypePtr, output: &mut (impl Write + 'a)) {
    unsafe {
        if typ.is_null() {
            writeln!(output, "Type: NULL");
            return;
        }
        write!(output, "Type: ");
        if !(*typ).name.is_null() {
            write!(
                output,
                "'{}' ",
                CStr::from_ptr((*typ).name as *const i8).to_string_lossy()
            );
        } else {
            write!(output, "(no name) ");
        }
        if !(*typ).target_namespace.is_null() {
            write!(
                output,
                "ns '{}' ",
                CStr::from_ptr((*typ).target_namespace as *const i8).to_string_lossy()
            );
        }
        match (*typ).typ {
            XmlSchemaTypeType::XmlSchemaTypeBasic => {
                write!(output, "[basic] ");
            }
            XmlSchemaTypeType::XmlSchemaTypeSimple => {
                write!(output, "[simple] ");
            }
            XmlSchemaTypeType::XmlSchemaTypeComplex => {
                write!(output, "[complex] ");
            }
            XmlSchemaTypeType::XmlSchemaTypeSequence => {
                write!(output, "[sequence] ");
            }
            XmlSchemaTypeType::XmlSchemaTypeChoice => {
                write!(output, "[choice] ");
            }
            XmlSchemaTypeType::XmlSchemaTypeAll => {
                write!(output, "[all] ");
            }
            XmlSchemaTypeType::XmlSchemaTypeUr => {
                write!(output, "[ur] ");
            }
            XmlSchemaTypeType::XmlSchemaTypeRestriction => {
                write!(output, "[restriction] ");
            }
            XmlSchemaTypeType::XmlSchemaTypeExtension => {
                write!(output, "[extension] ");
            }
            _ => {
                write!(output, "[unknown type {}] ", (*typ).typ as i32);
            }
        }
        write!(output, "content: ");
        match (*typ).content_type {
            XmlSchemaContentType::XmlSchemaContentUnknown => {
                write!(output, "[unknown] ");
            }
            XmlSchemaContentType::XmlSchemaContentEmpty => {
                write!(output, "[empty] ");
            }
            XmlSchemaContentType::XmlSchemaContentElements => {
                write!(output, "[element] ");
            }
            XmlSchemaContentType::XmlSchemaContentMixed => {
                write!(output, "[mixed] ");
            }
            XmlSchemaContentType::XmlSchemaContentMixedOrElements => { /* not used. */ }
            XmlSchemaContentType::XmlSchemaContentBasic => {
                write!(output, "[basic] ");
            }
            XmlSchemaContentType::XmlSchemaContentSimple => {
                write!(output, "[simple] ");
            }
            XmlSchemaContentType::XmlSchemaContentAny => {
                write!(output, "[any] ");
            }
        }
        writeln!(output);
        if !(*typ).base.is_null() {
            write!(
                output,
                "  base type: '{}'",
                CStr::from_ptr((*typ).base as *const i8).to_string_lossy()
            );
            if !(*typ).base_ns.is_null() {
                writeln!(
                    output,
                    " ns '{}'",
                    CStr::from_ptr((*typ).base_ns as *const i8).to_string_lossy()
                );
            } else {
                writeln!(output);
            }
        }
        if !(*typ).attr_uses.is_null() {
            xml_schema_attr_uses_dump((*typ).attr_uses as _, output);
        }
        if !(*typ).annot.is_null() {
            xml_schema_annot_dump(output, (*typ).annot);
        }
        // #ifdef DUMP_CONTENT_MODEL
        if (*typ).typ == XmlSchemaTypeType::XmlSchemaTypeComplex && !(*typ).subtypes.is_null() {
            xml_schema_content_model_dump((*typ).subtypes as XmlSchemaParticlePtr, output, 1);
        }
        // #endif
    }
}

extern "C" fn xml_schema_type_dump_entry<'a>(typ: *mut c_void, output: &mut (impl Write + 'a)) {
    unsafe {
        xml_schema_type_dump(typ as XmlSchemaTypePtr, output);
    }
}

/// Dump the element
#[doc(alias = "xmlSchemaElementDump")]
extern "C" fn xml_schema_element_dump<'a>(
    payload: *mut c_void,
    output: &mut (impl Write + 'a),
    namespace: *const u8,
) {
    let elem: XmlSchemaElementPtr = payload as XmlSchemaElementPtr;
    if elem.is_null() {
        return;
    }

    unsafe {
        write!(output, "Element");
        if (*elem).flags & XML_SCHEMAS_ELEM_GLOBAL != 0 {
            write!(output, " (global)");
        }
        write!(
            output,
            ": '{}' ",
            CStr::from_ptr((*elem).name as *const i8).to_string_lossy()
        );
        if !namespace.is_null() {
            write!(
                output,
                "ns '{}'",
                CStr::from_ptr(namespace as *const i8).to_string_lossy()
            );
        }
        writeln!(output);
        // Misc other properties.
        if (*elem).flags & XML_SCHEMAS_ELEM_NILLABLE != 0
            || (*elem).flags & XML_SCHEMAS_ELEM_ABSTRACT != 0
            || (*elem).flags & XML_SCHEMAS_ELEM_FIXED != 0
            || (*elem).flags & XML_SCHEMAS_ELEM_DEFAULT != 0
        {
            write!(output, "  props: ");
            if (*elem).flags & XML_SCHEMAS_ELEM_FIXED != 0 {
                write!(output, "[fixed] ");
            }
            if (*elem).flags & XML_SCHEMAS_ELEM_DEFAULT != 0 {
                write!(output, "[default] ");
            }
            if (*elem).flags & XML_SCHEMAS_ELEM_ABSTRACT != 0 {
                write!(output, "[abstract] ");
            }
            if (*elem).flags & XML_SCHEMAS_ELEM_NILLABLE != 0 {
                write!(output, "[nillable] ");
            }
            writeln!(output);
        }
        // Default/fixed value.
        if !(*elem).value.is_null() {
            writeln!(
                output,
                "  value: '{}'",
                CStr::from_ptr((*elem).value as *const i8).to_string_lossy()
            );
        }
        // Type.
        if !(*elem).named_type.is_null() {
            write!(
                output,
                "  type: '{}' ",
                CStr::from_ptr((*elem).named_type as *const i8).to_string_lossy()
            );
            if !(*elem).named_type_ns.is_null() {
                writeln!(
                    output,
                    "ns '{}'",
                    CStr::from_ptr((*elem).named_type_ns as *const i8).to_string_lossy()
                );
            } else {
                writeln!(output);
            }
        } else if !(*elem).subtypes.is_null() {
            // Dump local types.
            xml_schema_type_dump((*elem).subtypes, output);
        }
        // Substitution group.
        if !(*elem).subst_group.is_null() {
            write!(
                output,
                "  substitutionGroup: '{}' ",
                CStr::from_ptr((*elem).subst_group as *const i8).to_string_lossy()
            );
            if !(*elem).subst_group_ns.is_null() {
                writeln!(
                    output,
                    "ns '{}'",
                    CStr::from_ptr((*elem).subst_group_ns as *const i8).to_string_lossy()
                );
            } else {
                writeln!(output);
            }
        }
    }
}

/// Dump a Schema structure.
#[doc(alias = "xmlSchemaDump")]
pub unsafe fn xml_schema_dump<'a>(output: &mut (impl Write + 'a), schema: XmlSchemaPtr) {
    unsafe {
        if schema.is_null() {
            writeln!(output, "Schemas: NULL");
            return;
        }
        write!(output, "Schemas: ");
        if !(*schema).name.is_null() {
            write!(
                output,
                "{}, ",
                CStr::from_ptr((*schema).name as *const i8).to_string_lossy()
            );
        } else {
            write!(output, "no name, ");
        }
        if let Some(target_namespace) = (*schema).target_namespace.as_deref() {
            write!(output, "{}", target_namespace);
        } else {
            write!(output, "no target namespace");
        }
        writeln!(output);
        if !(*schema).annot.is_null() {
            xml_schema_annot_dump(output, (*schema).annot);
        }
        for &data in (*schema).type_decl.values() {
            if !data.is_null() {
                xml_schema_type_dump_entry(data as _, output);
            }
        }
        for (namespace, &data) in &(*schema).elem_decl {
            if !data.is_null() {
                let namespace = CString::new(namespace.as_str()).unwrap();
                xml_schema_element_dump(data as _, output, namespace.as_ptr() as *const u8);
            }
        }
    }
}

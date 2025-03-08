use std::{
    ffi::{CStr, c_void},
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

        writeln!(output, "  attributes:").ok();
        for using in (*uses)
            .items
            .iter()
            .map(|&using| using as XmlSchemaAttributeUsePtr)
        {
            if (*using).typ == XmlSchemaTypeType::XmlSchemaExtraAttrUseProhib {
                write!(output, "  [prohibition] ").ok();
                prohib = using as XmlSchemaAttributeUseProhibPtr;
                name = (*prohib).name;
                tns = (*prohib).target_namespace;
            } else if (*using).typ == XmlSchemaTypeType::XmlSchemaExtraQNameRef {
                write!(output, "  [reference] ").ok();
                refe = using as XmlSchemaQNameRefPtr;
                name = (*refe).name;
                tns = (*refe).target_namespace;
            } else {
                write!(output, "  [use] ").ok();
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
            )
            .ok();
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
            writeln!(output, "  Annot: {content}").ok();
        } else {
            writeln!(output, "  Annot: empty").ok();
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
        write!(output, "{}", shift).ok();
        if (*particle).children.is_null() {
            writeln!(output, "MISSING particle term").ok();
            return;
        }
        let term: XmlSchemaTreeItemPtr = (*particle).children;
        if term.is_null() {
            write!(output, "(NULL)").ok();
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
                    )
                    .ok();
                }
                XmlSchemaTypeType::XmlSchemaTypeSequence => {
                    write!(output, "SEQUENCE").ok();
                }
                XmlSchemaTypeType::XmlSchemaTypeChoice => {
                    write!(output, "CHOICE").ok();
                }
                XmlSchemaTypeType::XmlSchemaTypeAll => {
                    write!(output, "ALL").ok();
                }
                XmlSchemaTypeType::XmlSchemaTypeAny => {
                    write!(output, "ANY").ok();
                }
                _ => {
                    writeln!(output, "UNKNOWN").ok();
                    return;
                }
            }
        }
        if (*particle).min_occurs != 1 {
            write!(output, " min: {}", (*particle).min_occurs).ok();
        }
        if (*particle).max_occurs >= UNBOUNDED as i32 {
            write!(output, " max: unbounded").ok();
        } else if (*particle).max_occurs != 1 {
            write!(output, " max: {}", (*particle).max_occurs).ok();
        }
        writeln!(output).ok();
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
            writeln!(output, "Type: NULL").ok();
            return;
        }
        write!(output, "Type: ").ok();
        if !(*typ).name.is_null() {
            write!(
                output,
                "'{}' ",
                CStr::from_ptr((*typ).name as *const i8).to_string_lossy()
            )
            .ok();
        } else {
            write!(output, "(no name) ").ok();
        }
        if !(*typ).target_namespace.is_null() {
            write!(
                output,
                "ns '{}' ",
                CStr::from_ptr((*typ).target_namespace as *const i8).to_string_lossy()
            )
            .ok();
        }
        match (*typ).typ {
            XmlSchemaTypeType::XmlSchemaTypeBasic => {
                write!(output, "[basic] ").ok();
            }
            XmlSchemaTypeType::XmlSchemaTypeSimple => {
                write!(output, "[simple] ").ok();
            }
            XmlSchemaTypeType::XmlSchemaTypeComplex => {
                write!(output, "[complex] ").ok();
            }
            XmlSchemaTypeType::XmlSchemaTypeSequence => {
                write!(output, "[sequence] ").ok();
            }
            XmlSchemaTypeType::XmlSchemaTypeChoice => {
                write!(output, "[choice] ").ok();
            }
            XmlSchemaTypeType::XmlSchemaTypeAll => {
                write!(output, "[all] ").ok();
            }
            XmlSchemaTypeType::XmlSchemaTypeUr => {
                write!(output, "[ur] ").ok();
            }
            XmlSchemaTypeType::XmlSchemaTypeRestriction => {
                write!(output, "[restriction] ").ok();
            }
            XmlSchemaTypeType::XmlSchemaTypeExtension => {
                write!(output, "[extension] ").ok();
            }
            _ => {
                write!(output, "[unknown type {}] ", (*typ).typ as i32).ok();
            }
        }
        write!(output, "content: ").ok();
        match (*typ).content_type {
            XmlSchemaContentType::XmlSchemaContentUnknown => {
                write!(output, "[unknown] ").ok();
            }
            XmlSchemaContentType::XmlSchemaContentEmpty => {
                write!(output, "[empty] ").ok();
            }
            XmlSchemaContentType::XmlSchemaContentElements => {
                write!(output, "[element] ").ok();
            }
            XmlSchemaContentType::XmlSchemaContentMixed => {
                write!(output, "[mixed] ").ok();
            }
            XmlSchemaContentType::XmlSchemaContentMixedOrElements => { /* not used. */ }
            XmlSchemaContentType::XmlSchemaContentBasic => {
                write!(output, "[basic] ").ok();
            }
            XmlSchemaContentType::XmlSchemaContentSimple => {
                write!(output, "[simple] ").ok();
            }
            XmlSchemaContentType::XmlSchemaContentAny => {
                write!(output, "[any] ").ok();
            }
        }
        writeln!(output).ok();
        if !(*typ).base.is_null() {
            write!(
                output,
                "  base type: '{}'",
                CStr::from_ptr((*typ).base as *const i8).to_string_lossy()
            )
            .ok();
            if !(*typ).base_ns.is_null() {
                writeln!(
                    output,
                    " ns '{}'",
                    CStr::from_ptr((*typ).base_ns as *const i8).to_string_lossy()
                )
                .ok();
            } else {
                writeln!(output).ok();
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

/// Dump the element
#[doc(alias = "xmlSchemaElementDump")]
unsafe fn xml_schema_element_dump<'a>(
    elem: XmlSchemaElementPtr,
    output: &mut (impl Write + 'a),
    namespace: &str,
) {
    if elem.is_null() {
        return;
    }

    unsafe {
        write!(output, "Element").ok();
        if (*elem).flags & XML_SCHEMAS_ELEM_GLOBAL != 0 {
            write!(output, " (global)").ok();
        }
        write!(
            output,
            ": '{}' ",
            CStr::from_ptr((*elem).name as *const i8).to_string_lossy()
        )
        .ok();
        write!(output, "ns '{}'", namespace).ok();
        writeln!(output).ok();
        // Misc other properties.
        if (*elem).flags & XML_SCHEMAS_ELEM_NILLABLE != 0
            || (*elem).flags & XML_SCHEMAS_ELEM_ABSTRACT != 0
            || (*elem).flags & XML_SCHEMAS_ELEM_FIXED != 0
            || (*elem).flags & XML_SCHEMAS_ELEM_DEFAULT != 0
        {
            write!(output, "  props: ").ok();
            if (*elem).flags & XML_SCHEMAS_ELEM_FIXED != 0 {
                write!(output, "[fixed] ").ok();
            }
            if (*elem).flags & XML_SCHEMAS_ELEM_DEFAULT != 0 {
                write!(output, "[default] ").ok();
            }
            if (*elem).flags & XML_SCHEMAS_ELEM_ABSTRACT != 0 {
                write!(output, "[abstract] ").ok();
            }
            if (*elem).flags & XML_SCHEMAS_ELEM_NILLABLE != 0 {
                write!(output, "[nillable] ").ok();
            }
            writeln!(output).ok();
        }
        // Default/fixed value.
        if !(*elem).value.is_null() {
            writeln!(
                output,
                "  value: '{}'",
                CStr::from_ptr((*elem).value as *const i8).to_string_lossy()
            )
            .ok();
        }
        // Type.
        if !(*elem).named_type.is_null() {
            write!(
                output,
                "  type: '{}' ",
                CStr::from_ptr((*elem).named_type as *const i8).to_string_lossy()
            )
            .ok();
            if !(*elem).named_type_ns.is_null() {
                writeln!(
                    output,
                    "ns '{}'",
                    CStr::from_ptr((*elem).named_type_ns as *const i8).to_string_lossy()
                )
                .ok();
            } else {
                writeln!(output).ok();
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
            )
            .ok();
            if !(*elem).subst_group_ns.is_null() {
                writeln!(
                    output,
                    "ns '{}'",
                    CStr::from_ptr((*elem).subst_group_ns as *const i8).to_string_lossy()
                )
                .ok();
            } else {
                writeln!(output).ok();
            }
        }
    }
}

/// Dump a Schema structure.
#[doc(alias = "xmlSchemaDump")]
pub unsafe fn xml_schema_dump<'a>(output: &mut (impl Write + 'a), schema: XmlSchemaPtr) {
    unsafe {
        if schema.is_null() {
            writeln!(output, "Schemas: NULL").ok();
            return;
        }
        write!(output, "Schemas: ").ok();
        if let Some(name) = (*schema).name.as_deref() {
            write!(output, "{name}, ").ok();
        } else {
            write!(output, "no name, ").ok();
        }
        if let Some(target_namespace) = (*schema).target_namespace.as_deref() {
            write!(output, "{}", target_namespace).ok();
        } else {
            write!(output, "no target namespace").ok();
        }
        writeln!(output).ok();
        if !(*schema).annot.is_null() {
            xml_schema_annot_dump(output, (*schema).annot);
        }
        for &data in (*schema).type_decl.values() {
            if !data.is_null() {
                xml_schema_type_dump(data, output);
            }
        }
        for (namespace, &element) in &(*schema).elem_decl {
            if !element.is_null() {
                xml_schema_element_dump(element, output, namespace.as_str());
            }
        }
    }
}

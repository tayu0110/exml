use std::{ffi::c_void, ptr::null_mut};

use crate::{
    globals::{
        GenericErrorContext, get_keep_blanks_default_value, get_line_numbers_default_value,
        get_pedantic_parser_default_value, get_substitute_entities_default_value,
        set_indent_tree_output, set_keep_blanks_default_value, set_line_numbers_default_value,
        set_pedantic_parser_default_value, set_substitute_entities_default_value,
    },
    libxml::parser::{
        XmlSAXHandler, xml_create_doc_parser_ctxt, xml_init_parser, xml_parse_ext_parsed_ent,
    },
    parser::{XmlParserCtxtPtr, xml_free_parser_ctxt},
    tree::{XmlDocPtr, XmlDtdPtr, xml_free_doc},
};

use super::xml_create_memory_parser_ctxt;

/// Parse an XML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "xmlParseDoc")]
#[deprecated = "Use xmlReadDoc"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_parse_doc(cur: Vec<u8>) -> Option<XmlDocPtr> {
    unsafe { xml_sax_parse_doc(None, cur, 0) }
}

/// Parse an XML file and build a tree. Automatic support for ZLIB/Compress
/// compressed document is provided by default if found at compile-time.
///
/// Returns the resulting document tree if the file was wellformed,
/// NULL otherwise.
#[doc(alias = "xmlParseFile")]
#[deprecated = "Use xmlReadFile"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_parse_file(filename: Option<&str>) -> Option<XmlDocPtr> {
    unsafe { xml_sax_parse_file(None, filename, 0) }
}

/// Parse an XML in-memory block and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "xmlParseMemory")]
#[deprecated = "Use xmlReadMemory"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_parse_memory(buffer: Vec<u8>) -> Option<XmlDocPtr> {
    unsafe { xml_sax_parse_memory(None, buffer, 0) }
}

/// Set and return the previous value for default entity support.
/// Initially the parser always keep entity references instead of substituting
/// entity values in the output. This function has to be used to change the
/// default parser behavior
/// SAX::substituteEntities() has to be used for changing that on a file by
/// file basis.
///
/// Returns the last value for 0 for no substitution, 1 for substitution.
#[doc(alias = "xmlSubstituteEntitiesDefault")]
#[deprecated = "Use the modern options API with XML_PARSE_NOENT"]
pub fn xml_substitute_entities_default(val: i32) -> i32 {
    let old = get_substitute_entities_default_value();

    set_substitute_entities_default_value(val);
    old
}

/// Set and return the previous value for default blanks text nodes support.
/// The 1.x version of the parser used an heuristic to try to detect
/// ignorable white spaces. As a result the SAX callback was generating
/// xmlSAX2IgnorableWhitespace() callbacks instead of characters() one, and when
/// using the DOM output text nodes containing those blanks were not generated.
/// The 2.x and later version will switch to the XML standard way and
/// ignorableWhitespace() are only generated when running the parser in
/// validating mode and when the current element doesn't allow CDATA or
/// mixed content.
/// This function is provided as a way to force the standard behavior
/// on 1.X libs and to switch back to the old mode for compatibility when
/// running 1.X client code on 2.X . Upgrade of 1.X code should be done
/// by using xmlIsBlankNode() commodity function to detect the "empty"
/// nodes generated.
/// This value also affect autogeneration of indentation when saving code
/// if blanks sections are kept, indentation is not generated.
///
/// Returns the last value for 0 for no substitution, 1 for substitution.
#[doc(alias = "xmlKeepBlanksDefault")]
#[deprecated = "Use the modern options API with XML_PARSE_NOBLANKS"]
pub fn xml_keep_blanks_default(val: i32) -> i32 {
    let old = get_keep_blanks_default_value();

    set_keep_blanks_default_value(val);
    if val == 0 {
        set_indent_tree_output(1);
    }
    old
}

/// Set and return the previous value for enabling pedantic warnings.
///
/// Returns the last value for 0 for no substitution, 1 for substitution.
#[doc(alias = "xmlPedanticParserDefault")]
#[deprecated = "Use the modern options API with XML_PARSE_PEDANTIC"]
pub fn xml_pedantic_parser_default(val: i32) -> i32 {
    let old = get_pedantic_parser_default_value();

    set_pedantic_parser_default_value(val);
    old
}

/// Set and return the previous value for enabling line numbers in elements
/// contents. This may break on old application and is turned off by default.
///
/// Returns the last value for 0 for no substitution, 1 for substitution.
#[doc(alias = "xmlLineNumbersDefault")]
#[deprecated = "The modern options API always enables line numbers"]
pub fn xml_line_numbers_default(val: i32) -> i32 {
    let old = get_line_numbers_default_value();

    set_line_numbers_default_value(val);
    old
}

/// Parse an XML in-memory document and build a tree.
/// In the case the document is not Well Formed, a attempt to build a
/// tree is tried anyway
///
/// Returns the resulting document tree or NULL in case of failure
#[doc(alias = "xmlRecoverDoc")]
#[deprecated = "Use xmlReadDoc with XML_PARSE_RECOVER"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_recover_doc(cur: Vec<u8>) -> Option<XmlDocPtr> {
    unsafe { xml_sax_parse_doc(None, cur, 1) }
}

/// Parse an XML in-memory block and build a tree.
/// In the case the document is not Well Formed, an attempt to
/// build a tree is tried anyway
///
/// Returns the resulting document tree or NULL in case of error
#[doc(alias = "xmlRecoverMemory")]
#[deprecated = "Use xmlReadMemory with XML_PARSE_RECOVER"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_recover_memory(buffer: Vec<u8>) -> Option<XmlDocPtr> {
    unsafe { xml_sax_parse_memory(None, buffer, 1) }
}

/// Parse an XML file and build a tree. Automatic support for ZLIB/Compress
/// compressed document is provided by default if found at compile-time.
/// In the case the document is not Well Formed, it attempts to build
/// a tree anyway
///
/// Returns the resulting document tree or NULL in case of failure
#[doc(alias = "xmlRecoverFile")]
#[deprecated = "Use xmlReadFile with XML_PARSE_RECOVER"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_recover_file(filename: Option<&str>) -> Option<XmlDocPtr> {
    unsafe { xml_sax_parse_file(None, filename, 1) }
}

/// parse an XML file and call the given SAX handler routines.
/// Automatic support for ZLIB/Compress compressed document is provided
///
/// Returns 0 in case of success or a error number otherwise
#[doc(alias = "xmlSAXUserParseFile")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadFile"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_user_parse_file(
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
    filename: Option<&str>,
) -> i32 {
    unsafe {
        use crate::parser::{xml_create_file_parser_ctxt, xml_free_parser_ctxt};

        let ret: i32;

        let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(filename);
        if ctxt.is_null() {
            return -1;
        }
        (*ctxt).sax = sax;
        (*ctxt).detect_sax2();

        (*ctxt).user_data = user_data;

        (*ctxt).parse_document();

        if (*ctxt).well_formed != 0 {
            ret = 0;
        } else if (*ctxt).err_no != 0 {
            ret = (*ctxt).err_no;
        } else {
            ret = -1;
        }
        (*ctxt).sax = None;
        if let Some(my_doc) = (*ctxt).my_doc.take() {
            xml_free_doc(my_doc);
        }
        xml_free_parser_ctxt(ctxt);

        ret
    }
}

/// Parse an XML in-memory buffer and call the given SAX handler routines.
///
/// Returns 0 in case of success or a error number otherwise
#[doc(alias = "xmlSAXUserParseMemory")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadMemory"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_user_parse_memory(
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
    buffer: Vec<u8>,
) -> i32 {
    unsafe {
        use crate::parser::xml_create_memory_parser_ctxt;

        let ret: i32;

        xml_init_parser();

        let ctxt: XmlParserCtxtPtr = xml_create_memory_parser_ctxt(buffer);
        if ctxt.is_null() {
            return -1;
        }
        (*ctxt).sax = sax;
        (*ctxt).detect_sax2();
        (*ctxt).user_data = user_data;

        (*ctxt).parse_document();

        if (*ctxt).well_formed != 0 {
            ret = 0;
        } else if (*ctxt).err_no != 0 {
            ret = (*ctxt).err_no;
        } else {
            ret = -1;
        }
        (*ctxt).sax = None;
        if let Some(my_doc) = (*ctxt).my_doc.take() {
            xml_free_doc(my_doc);
        }
        xml_free_parser_ctxt(ctxt);

        ret
    }
}

/// Parse an XML in-memory document and build a tree.
/// It use the given SAX function block to handle the parsing callback.
/// If sax is NULL, fallback to the default DOM tree building routines.
///
/// Returns the resulting document tree
#[doc(alias = "xmlSAXParseDoc")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadDoc"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_parse_doc(
    sax: Option<Box<XmlSAXHandler>>,
    cur: Vec<u8>,
    recovery: i32,
) -> Option<XmlDocPtr> {
    unsafe {
        let replaced = sax.is_some();
        let mut oldsax = None;

        let ctxt: XmlParserCtxtPtr = xml_create_doc_parser_ctxt(cur);
        if ctxt.is_null() {
            return None;
        }
        if let Some(sax) = sax {
            oldsax = (*ctxt).sax.replace(sax);
            (*ctxt).user_data = None;
        }
        (*ctxt).detect_sax2();
        (*ctxt).parse_document();
        let ret = if (*ctxt).well_formed != 0 || recovery != 0 {
            (*ctxt).my_doc
        } else {
            if let Some(my_doc) = (*ctxt).my_doc.take() {
                xml_free_doc(my_doc);
            }
            None
        };
        if replaced {
            (*ctxt).sax = oldsax;
        }
        xml_free_parser_ctxt(ctxt);

        ret
    }
}

/// Parse an XML in-memory block and use the given SAX function block
/// to handle the parsing callback. If sax is NULL, fallback to the default
/// DOM tree building routines.
///
/// Returns the resulting document tree
#[doc(alias = "xmlSAXParseMemory")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadMemory"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_parse_memory(
    sax: Option<Box<XmlSAXHandler>>,
    buffer: Vec<u8>,
    recovery: i32,
) -> Option<XmlDocPtr> {
    unsafe { xml_sax_parse_memory_with_data(sax, buffer, recovery, null_mut()) }
}

/// Parse an XML in-memory block and use the given SAX function block
/// to handle the parsing callback. If sax is NULL, fallback to the default
/// DOM tree building routines.
///
/// User data (c_void *) is stored within the parser context in the
/// context's _private member, so it is available nearly everywhere in libxml
///
/// Returns the resulting document tree
#[doc(alias = "xmlSAXParseMemoryWithData")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadMemory"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_parse_memory_with_data(
    sax: Option<Box<XmlSAXHandler>>,
    buffer: Vec<u8>,
    recovery: i32,
    data: *mut c_void,
) -> Option<XmlDocPtr> {
    unsafe {
        let replaced = sax.is_some();

        xml_init_parser();

        let ctxt: XmlParserCtxtPtr = xml_create_memory_parser_ctxt(buffer);
        if ctxt.is_null() {
            return None;
        }
        if let Some(sax) = sax {
            (*ctxt).sax = Some(sax);
        }
        (*ctxt).detect_sax2();
        if !data.is_null() {
            (*ctxt)._private = data;
        }

        (*ctxt).recovery = recovery;

        (*ctxt).parse_document();

        let ret = if (*ctxt).well_formed != 0 || recovery != 0 {
            (*ctxt).my_doc
        } else {
            if let Some(my_doc) = (*ctxt).my_doc.take() {
                xml_free_doc(my_doc);
            }
            None
        };
        if replaced {
            (*ctxt).sax = None;
        }
        xml_free_parser_ctxt(ctxt);

        ret
    }
}

/// parse an XML file and build a tree. Automatic support for ZLIB/Compress
/// compressed document is provided by default if found at compile-time.
/// It use the given SAX function block to handle the parsing callback.
/// If sax is NULL, fallback to the default DOM tree building routines.
///
/// Returns the resulting document tree
#[doc(alias = "xmlSAXParseFile")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadFile"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_parse_file(
    sax: Option<Box<XmlSAXHandler>>,
    filename: Option<&str>,
    recovery: i32,
) -> Option<XmlDocPtr> {
    unsafe { xml_sax_parse_file_with_data(sax, filename, recovery, null_mut()) }
}

/// Parse an XML file and build a tree. Automatic support for ZLIB/Compress
/// compressed document is provided by default if found at compile-time.
/// It use the given SAX function block to handle the parsing callback.
/// If sax is NULL, fallback to the default DOM tree building routines.
///
/// User data (c_void *) is stored within the parser context in the
/// context's _private member, so it is available nearly everywhere in libxml
///
/// Returns the resulting document tree
#[doc(alias = "xmlSAXParseFileWithData")]
#[deprecated = "Use xmlNewSAXParserCtxt and xmlCtxtReadFile"]
#[cfg(feature = "sax1")]
pub unsafe fn xml_sax_parse_file_with_data(
    sax: Option<Box<XmlSAXHandler>>,
    filename: Option<&str>,
    recovery: i32,
    data: *mut c_void,
) -> Option<XmlDocPtr> {
    unsafe {
        use crate::{io::xml_parser_get_directory, parser::xml_create_file_parser_ctxt};

        let replaced = sax.is_some();

        xml_init_parser();

        let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(filename);
        if ctxt.is_null() {
            return None;
        }
        if let Some(sax) = sax {
            (*ctxt).sax = Some(sax);
        }
        (*ctxt).detect_sax2();
        if !data.is_null() {
            (*ctxt)._private = data;
        }

        if (*ctxt).directory.is_none() {
            if let Some(filename) = filename {
                if let Some(dir) = xml_parser_get_directory(filename) {
                    (*ctxt).directory = Some(dir.to_string_lossy().into_owned());
                }
            }
        }

        (*ctxt).recovery = recovery;

        (*ctxt).parse_document();

        let ret = if (*ctxt).well_formed != 0 || recovery != 0 {
            let ret = (*ctxt).my_doc;
            if (*ctxt).input().unwrap().buf.is_some() {
                if let Some(mut ret) = ret {
                    if (*ctxt)
                        .input()
                        .unwrap()
                        .buf
                        .as_ref()
                        .unwrap()
                        .borrow()
                        .compressed
                        > 0
                    {
                        ret.compression = 9;
                    } else {
                        ret.compression = (*ctxt)
                            .input()
                            .unwrap()
                            .buf
                            .as_ref()
                            .unwrap()
                            .borrow()
                            .compressed;
                    }
                }
            }
            ret
        } else {
            if let Some(my_doc) = (*ctxt).my_doc.take() {
                xml_free_doc(my_doc);
            }
            None
        };
        if replaced {
            (*ctxt).sax = None;
        }
        xml_free_parser_ctxt(ctxt);

        ret
    }
}

/// Parse an XML external entity out of context and build a tree.
/// It use the given SAX function block to handle the parsing callback.
/// If sax is NULL, fallback to the default DOM tree building routines.
///
/// ```text
/// [78] extParsedEnt ::= TextDecl? content
/// ```
///
/// This correspond to a "Well Balanced" chunk
///
/// Returns the resulting document tree
#[doc(alias = "xmlSAXParseEntity")]
#[deprecated]
#[cfg(feature = "sax1")]
pub(crate) unsafe fn xml_sax_parse_entity(
    sax: Option<Box<XmlSAXHandler>>,
    filename: Option<&str>,
) -> Option<XmlDocPtr> {
    unsafe {
        use crate::parser::xml_create_file_parser_ctxt;

        let replaced = sax.is_some();

        let ctxt: XmlParserCtxtPtr = xml_create_file_parser_ctxt(filename);
        if ctxt.is_null() {
            return None;
        }
        if let Some(sax) = sax {
            (*ctxt).sax = Some(sax);
            (*ctxt).user_data = None;
        }

        xml_parse_ext_parsed_ent(ctxt);

        let ret = if (*ctxt).well_formed != 0 {
            (*ctxt).my_doc
        } else {
            if let Some(my_doc) = (*ctxt).my_doc.take() {
                xml_free_doc(my_doc);
            }
            None
        };
        if replaced {
            (*ctxt).sax = None;
        }
        xml_free_parser_ctxt(ctxt);

        ret
    }
}

/// Parse an XML external entity out of context and build a tree.
///
/// ```text
/// [78] extParsedEnt ::= TextDecl? content
/// ```
///
/// This correspond to a "Well Balanced" chunk
///
/// Returns the resulting document tree
#[doc(alias = "xmlParseEntity")]
#[deprecated]
#[cfg(feature = "sax1")]
pub unsafe fn xml_parse_entity(filename: Option<&str>) -> Option<XmlDocPtr> {
    unsafe { xml_sax_parse_entity(None, filename) }
}

/// Load and parse an external subset.
///
/// Returns the resulting xmlDtdPtr or NULL in case of error.
#[doc(alias = "xmlSAXParseDTD")]
#[deprecated]
#[cfg(feature = "libxml_valid")]
pub(crate) unsafe fn xml_sax_parse_dtd(
    sax: Option<Box<XmlSAXHandler>>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) -> Option<XmlDtdPtr> {
    use crate::{
        encoding::detect_encoding,
        parser::{XmlParserOption, xml_err_memory},
        tree::{XmlDocProperties, xml_new_doc, xml_new_dtd},
        uri::canonic_path,
    };

    unsafe {
        use std::slice::from_raw_parts;

        use crate::parser::{xml_free_parser_ctxt, xml_new_sax_parser_ctxt};

        if external_id.is_none() && system_id.is_none() {
            return None;
        }

        let Ok(ctxt) = xml_new_sax_parser_ctxt(sax, None) else {
            return None;
        };

        // We are loading a DTD
        (*ctxt).options |= XmlParserOption::XmlParseDTDLoad as i32;

        // Canonicalise the system ID
        let system_id_canonic = system_id.map(|s| canonic_path(s));

        // Ask the Entity resolver to load the damn thing
        let Some(input) = (*ctxt)
            .sax
            .as_deref_mut()
            .and_then(|sax| sax.resolve_entity)
            .and_then(|resolve_entity| {
                resolve_entity(
                    (*ctxt).user_data.clone(),
                    external_id,
                    system_id_canonic.as_deref(),
                )
            })
        else {
            xml_free_parser_ctxt(ctxt);
            return None;
        };

        // plug some encoding conversion routines here.
        if (*ctxt).push_input(input) < 0 {
            return None;
        }
        if (*ctxt).input().unwrap().remainder_len() >= 4 {
            let input = from_raw_parts((*ctxt).input().unwrap().cur, 4);
            let enc = detect_encoding(input);
            (*ctxt).switch_encoding(enc);
        }

        let cur = (*ctxt).input().unwrap().cur;
        if let Some(input) = (*ctxt).input_mut() {
            if input.filename.is_none() {
                if let Some(canonic) = system_id_canonic {
                    input.filename = Some(canonic.into_owned());
                }
            }
            input.line = 1;
            input.col = 1;
            input.base = cur;
            input.cur = cur;
            input.free = None;
        }

        // let's parse that entity knowing it's an external subset.
        (*ctxt).in_subset = 2;
        (*ctxt).my_doc = xml_new_doc(Some("1.0"));
        let Some(mut my_doc) = (*ctxt).my_doc else {
            xml_err_memory(ctxt, Some("New Doc failed"));
            xml_free_parser_ctxt(ctxt);
            return None;
        };
        my_doc.properties = XmlDocProperties::XmlDocInternal as i32;
        my_doc.ext_subset = xml_new_dtd((*ctxt).my_doc, Some("none"), external_id, system_id);
        (*ctxt).parse_external_subset(external_id, system_id);

        let mut ret = None;
        if let Some(mut my_doc) = (*ctxt).my_doc.take() {
            if (*ctxt).well_formed != 0 {
                ret = my_doc.ext_subset.take();
                if let Some(mut ret) = ret {
                    ret.doc = None;
                    let mut tmp = ret.children;
                    while let Some(mut now) = tmp {
                        now.set_document(None);
                        tmp = now.next();
                    }
                }
            } else {
                ret = None;
            }
            xml_free_doc(my_doc);
        }
        xml_free_parser_ctxt(ctxt);

        ret
    }
}

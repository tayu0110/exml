use std::ptr::null_mut;

use crate::{
    dict::xml_dict_lookup,
    error::XmlParserErrors,
    globals::GenericErrorContext,
    hash::{XmlHashTable, XmlHashTableRef},
    libxml::{
        globals::{xml_free, xml_malloc, xml_realloc},
        parser::{SAX_COMPAT_MODE, XmlDefAttrs, XmlDefAttrsPtr, XmlParserInputState},
        sax2::{xml_sax2_entity_decl, xml_sax2_get_entity},
        xmlstring::xml_strndup,
    },
    parser::{
        XmlParserCtxt, split_qname2, xml_err_memory, xml_err_msg_str, xml_fatal_err,
        xml_fatal_err_msg, xml_fatal_err_msg_str, xml_ns_err, xml_validity_error,
    },
    tree::{
        XmlAttributeDefault, XmlAttributeType, XmlDocProperties, XmlEntityType, XmlEnumeration,
        xml_create_enumeration, xml_new_doc, xml_new_dtd,
    },
    uri::XmlURI,
};

use super::{
    attr_normalize_space, parse_att_value, parse_entity_value, parse_external_id, parse_name,
    parse_nmtoken,
};

impl XmlParserCtxt {
    /// Add a defaulted attribute for an element
    #[doc(alias = "xmlAddDefAttrs")]
    pub(crate) unsafe fn add_def_attrs(&mut self, fullname: &str, fullattr: &str, value: &str) {
        unsafe {
            let mut defaults: XmlDefAttrsPtr;

            // Allows to detect attribute redefinitions
            if self
                .atts_special
                .filter(|t| t.lookup2(fullname, Some(fullattr)).is_some())
                .is_some()
            {
                return;
            }

            'mem_error: {
                let mut atts_default = if let Some(table) = self.atts_default {
                    table
                } else {
                    let table = XmlHashTable::with_capacity(10);
                    let Some(table) = XmlHashTableRef::from_table(table) else {
                        break 'mem_error;
                    };
                    self.atts_default = Some(table);
                    table
                };

                // split the element name into prefix:localname , the string found
                // are within the DTD and then not associated to namespace names.
                let (prefix, name) = split_qname2(fullname)
                    .map(|(pre, loc)| (Some(pre), loc))
                    .unwrap_or((None, fullname));

                // make sure there is some storage
                defaults = atts_default
                    .lookup2(name, prefix)
                    .map_or(null_mut(), |p| *p);
                if defaults.is_null() {
                    defaults =
                        xml_malloc(size_of::<XmlDefAttrs>() + (4 * 5) * size_of::<*const u8>())
                            as _;
                    if defaults.is_null() {
                        break 'mem_error;
                    }
                    (*defaults).nb_attrs = 0;
                    (*defaults).max_attrs = 4;
                    if atts_default
                        .update_entry2(name, prefix, defaults, |_, _| {})
                        .is_err()
                    {
                        xml_free(defaults as _);
                        break 'mem_error;
                    }
                } else if (*defaults).nb_attrs >= (*defaults).max_attrs {
                    let temp: XmlDefAttrsPtr = xml_realloc(
                        defaults as _,
                        size_of::<XmlDefAttrs>()
                            + (2 * (*defaults).max_attrs as usize * 5) * size_of::<*const u8>(),
                    ) as _;
                    if temp.is_null() {
                        break 'mem_error;
                    }
                    defaults = temp;
                    (*defaults).max_attrs *= 2;
                    if atts_default
                        .update_entry2(name, prefix, defaults, |_, _| {})
                        .is_err()
                    {
                        xml_free(defaults as _);
                        break 'mem_error;
                    }
                }

                // Split the element name into prefix:localname , the string found
                // are within the DTD and hen not associated to namespace names.
                let (prefix, name) = split_qname2(fullattr)
                    .map(|(pre, loc)| (Some(pre), loc))
                    .unwrap_or((None, fullattr));

                *(*defaults)
                    .values
                    .as_mut_ptr()
                    .add(5 * (*defaults).nb_attrs as usize) =
                    xml_dict_lookup(self.dict, name.as_ptr(), name.len() as i32);
                *(*defaults)
                    .values
                    .as_mut_ptr()
                    .add(5 * (*defaults).nb_attrs as usize + 1) = prefix
                    .map_or(null_mut(), |pre| {
                        xml_dict_lookup(self.dict, pre.as_ptr(), pre.len() as i32)
                    });
                // intern the string and precompute the end
                let len = value.len();
                let value = xml_dict_lookup(self.dict, value.as_ptr(), len as i32);
                if value.is_null() {
                    break 'mem_error;
                }
                *(*defaults)
                    .values
                    .as_mut_ptr()
                    .add(5 * (*defaults).nb_attrs as usize + 2) = value;
                *(*defaults)
                    .values
                    .as_mut_ptr()
                    .add(5 * (*defaults).nb_attrs as usize + 3) = value.add(len);
                if self.external != 0 {
                    *(*defaults)
                        .values
                        .as_mut_ptr()
                        .add(5 * (*defaults).nb_attrs as usize + 4) = c"external".as_ptr() as _;
                } else {
                    *(*defaults)
                        .values
                        .as_mut_ptr()
                        .add(5 * (*defaults).nb_attrs as usize + 4) = null_mut();
                }
                (*defaults).nb_attrs += 1;
                return;
            }

            xml_err_memory(self, None);
        }
    }

    /// Register this attribute type
    #[doc(alias = "xmlAddSpecialAttr")]
    pub(crate) unsafe fn add_special_attr(
        &mut self,
        fullname: &str,
        fullattr: &str,
        typ: XmlAttributeType,
    ) {
        unsafe {
            let mut atts_special = if let Some(table) = self.atts_special {
                table
            } else {
                let table = XmlHashTable::with_capacity(10);
                let Some(table) = XmlHashTableRef::from_table(table) else {
                    xml_err_memory(self, None);
                    return;
                };
                self.atts_special = Some(table);
                table
            };

            if atts_special.lookup2(fullname, Some(fullattr)).is_some() {
                return;
            }

            atts_special.add_entry2(fullname, Some(fullattr), typ).ok();
        }
    }
}

/// Parse an attribute list declaration for an element. Always consumes '<!'.
///
/// ```text
/// [52] AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
/// [53] AttDef ::= S Name S AttType S DefaultDecl
/// ```
#[doc(alias = "xmlParseAttributeListDecl")]
pub(crate) unsafe fn parse_attribute_list_decl(ctxt: &mut XmlParserCtxt) {
    unsafe {
        if !ctxt.content_bytes().starts_with(b"<!") {
            return;
        }
        ctxt.advance(2);

        if ctxt.content_bytes().starts_with(b"ATTLIST") {
            let inputid: i32 = ctxt.input().unwrap().id;

            ctxt.advance(7);
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after '<!ATTLIST'\n",
                );
            }
            let Some(elem_name) = parse_name(ctxt) else {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    "ATTLIST: no name for Element\n",
                );
                return;
            };
            ctxt.skip_blanks();
            ctxt.grow();
            while ctxt.current_byte() != b'>'
                && !matches!(ctxt.instate, XmlParserInputState::XmlParserEOF)
            {
                ctxt.grow();
                let Some(attr_name) = parse_name(ctxt) else {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrNameRequired,
                        "ATTLIST: no name for Attribute\n",
                    );
                    break;
                };
                ctxt.grow();
                if ctxt.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required after the attribute name\n",
                    );
                    break;
                }

                let mut tree = None;
                let Some(typ) = parse_attribute_type(ctxt, &mut tree) else {
                    break;
                };

                ctxt.grow();
                if ctxt.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required after the attribute typ\n",
                    );
                    break;
                }

                let (def, mut default_value) = parse_default_decl(ctxt);
                if typ != XmlAttributeType::XmlAttributeCDATA {
                    if let Some(value) = default_value {
                        default_value = Some(attr_normalize_space(&value).into_owned());
                    }
                }

                ctxt.grow();
                if ctxt.current_byte() != b'>' && ctxt.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required after the attribute default value\n",
                    );
                    break;
                }
                if let Some(attribute_decl) = ctxt
                    .sax
                    .as_deref_mut()
                    .filter(|_| ctxt.disable_sax == 0)
                    .and_then(|sax| sax.attribute_decl)
                {
                    attribute_decl(
                        ctxt.user_data.clone(),
                        &elem_name,
                        &attr_name,
                        typ,
                        def,
                        default_value.as_deref(),
                        tree,
                    );
                }

                if ctxt.sax2 != 0
                    && default_value.is_some()
                    && def != XmlAttributeDefault::XmlAttributeImplied
                    && def != XmlAttributeDefault::XmlAttributeRequired
                {
                    ctxt.add_def_attrs(&elem_name, &attr_name, default_value.as_deref().unwrap());
                }
                if ctxt.sax2 != 0 {
                    ctxt.add_special_attr(&elem_name, &attr_name, typ);
                }
                ctxt.grow();
            }
            if ctxt.current_byte() == b'>' {
                if inputid != ctxt.input().unwrap().id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        "Attribute list declaration doesn't start and stop in the same entity\n",
                    );
                }
                ctxt.skip_char();
            }
        }
    }
}

/// Parse the Attribute list def for an element
///
/// ```text
/// [54] AttType ::= StringType | TokenizedType | EnumeratedType
/// [55] StringType ::= 'CDATA'
/// [56] TokenizedType ::= 'ID' | 'IDREF' | 'IDREFS' | 'ENTITY' | 'ENTITIES' | 'NMTOKEN' | 'NMTOKENS'
/// ```
///
/// Validity constraints for attribute values syntax are checked in xmlValidateAttributeValue()
///
/// `[ VC: ID ]`  
/// Values of type ID must match the Name production. A name must not
/// appear more than once in an XML document as a value of this type;
/// i.e., ID values must uniquely identify the elements which bear them.
///
/// `[ VC: One ID per Element Type ]`  
/// No element type may have more than one ID attribute specified.
///
/// `[ VC: ID Attribute Default ]`  
/// An ID attribute must have a declared default of #IMPLIED or #REQUIRED.
///
/// `[ VC: IDREF ]`  
/// Values of type IDREF must match the Name production, and values
/// of type IDREFS must match Names; each IDREF Name must match the value
/// of an ID attribute on some element in the XML document; i.e. IDREF
/// values must match the value of some ID attribute.
///
/// `[ VC: Entity Name ]`  
/// Values of type ENTITY must match the Name production, values
/// of type ENTITIES must match Names; each Entity Name must match the
/// name of an unparsed entity declared in the DTD.
///
/// `[ VC: Name Token ]`  
/// Values of type NMTOKEN must match the Nmtoken production; values
/// of type NMTOKENS must match Nmtokens.
///
/// Returns the attribute type
#[doc(alias = "xmlParseAttributeType")]
pub(crate) unsafe fn parse_attribute_type(
    ctxt: &mut XmlParserCtxt,
    tree: &mut Option<Box<XmlEnumeration>>,
) -> Option<XmlAttributeType> {
    unsafe {
        if ctxt.content_bytes().starts_with(b"CDATA") {
            ctxt.advance(5);
            return Some(XmlAttributeType::XmlAttributeCDATA);
        } else if ctxt.content_bytes().starts_with(b"IDREFS") {
            ctxt.advance(6);
            return Some(XmlAttributeType::XmlAttributeIDREFS);
        } else if ctxt.content_bytes().starts_with(b"IDREF") {
            ctxt.advance(5);
            return Some(XmlAttributeType::XmlAttributeIDREF);
        } else if ctxt.content_bytes().starts_with(b"ID") {
            ctxt.advance(2);
            return Some(XmlAttributeType::XmlAttributeID);
        } else if ctxt.content_bytes().starts_with(b"ENTITY") {
            ctxt.advance(6);
            return Some(XmlAttributeType::XmlAttributeEntity);
        } else if ctxt.content_bytes().starts_with(b"ENTITIES") {
            ctxt.advance(8);
            return Some(XmlAttributeType::XmlAttributeEntities);
        } else if ctxt.content_bytes().starts_with(b"NMTOKENS") {
            ctxt.advance(8);
            return Some(XmlAttributeType::XmlAttributeNmtokens);
        } else if ctxt.content_bytes().starts_with(b"NMTOKEN") {
            ctxt.advance(7);
            return Some(XmlAttributeType::XmlAttributeNmtoken);
        }
        parse_enumerated_type(ctxt, tree)
    }
}

/// Parse an Enumerated attribute type.
///
/// ```text
/// [57] EnumeratedType ::= NotationType | Enumeration
/// [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
/// ```
///
/// Returns: XML_ATTRIBUTE_ENUMERATION or XML_ATTRIBUTE_NOTATION
#[doc(alias = "xmlParseEnumeratedType")]
pub(crate) unsafe fn parse_enumerated_type(
    ctxt: &mut XmlParserCtxt,
    tree: &mut Option<Box<XmlEnumeration>>,
) -> Option<XmlAttributeType> {
    unsafe {
        if ctxt.content_bytes().starts_with(b"NOTATION") {
            ctxt.advance(8);
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after 'NOTATION'\n",
                );
                return None;
            }
            *tree = parse_notation_type(ctxt);
            if tree.is_none() {
                return None;
            }
            return Some(XmlAttributeType::XmlAttributeNotation);
        }
        *tree = parse_enumeration_type(ctxt);
        if tree.is_none() {
            return None;
        }
        Some(XmlAttributeType::XmlAttributeEnumeration)
    }
}

/// Parse an Notation attribute type.
///
/// # Note
/// The leading 'NOTATION' S part has already being parsed...
///
/// ```text
/// [58] NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
/// ```
///
/// `[ VC: Notation Attributes ]`
/// Values of this type must match one of the notation names included
/// in the declaration; all notation names in the declaration must be declared.
///
/// Returns: the notation attribute tree built while parsing
#[doc(alias = "xmlParseNotationType")]
pub(crate) unsafe fn parse_notation_type(ctxt: &mut XmlParserCtxt) -> Option<Box<XmlEnumeration>> {
    unsafe {
        let mut ret = None::<Box<XmlEnumeration>>;

        if ctxt.current_byte() != b'(' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotStarted, None);
            return None;
        }
        while {
            ctxt.skip_char();
            ctxt.skip_blanks();
            let Some(name) = parse_name(ctxt) else {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    "Name expected in NOTATION declaration\n",
                );
                return None;
            };
            let mut tmp = ret.as_deref();
            while let Some(now) = tmp {
                if name == now.name {
                    xml_validity_error!(
                        ctxt,
                        XmlParserErrors::XmlDTDDupToken,
                        "standalone: attribute notation value token {} duplicated\n",
                        name
                    );
                    break;
                }
                tmp = now.next.as_deref();
            }
            if tmp.is_none() {
                let cur = xml_create_enumeration(&name);
                if let Some(mut ret) = ret.as_deref_mut() {
                    while ret.next.is_some() {
                        ret = ret.next.as_deref_mut().unwrap();
                    }
                    ret.next = Some(cur);
                } else {
                    ret = Some(cur);
                }
            }
            ctxt.skip_blanks();
            ctxt.current_byte() == b'|'
        } {}
        if ctxt.current_byte() != b')' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotFinished, None);
            return None;
        }
        ctxt.skip_char();
        ret
    }
}

/// Parse an Enumeration attribute type.
///
/// ```text
/// [59] Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
/// ```
///
/// `[ VC: Enumeration ]`  
/// Values of this type must match one of the Nmtoken tokens in the declaration
///
/// Returns: the enumeration attribute tree built while parsing
#[doc(alias = "xmlParseEnumerationType")]
pub(crate) unsafe fn parse_enumeration_type(
    ctxt: &mut XmlParserCtxt,
) -> Option<Box<XmlEnumeration>> {
    unsafe {
        let mut ret = None::<Box<XmlEnumeration>>;

        if ctxt.current_byte() != b'(' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttlistNotStarted, None);
            return None;
        }
        while {
            ctxt.skip_char();
            ctxt.skip_blanks();
            let Some(name) = parse_nmtoken(ctxt) else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrNmtokenRequired, None);
                return ret;
            };
            let mut tmp = ret.as_deref();
            while let Some(now) = tmp {
                if name == now.name {
                    xml_validity_error!(
                        ctxt,
                        XmlParserErrors::XmlDTDDupToken,
                        "standalone: attribute enumeration value token {} duplicated\n",
                        name
                    );
                    break;
                }
                tmp = now.next.as_deref();
            }
            if tmp.is_none() {
                let cur = xml_create_enumeration(&name);
                if let Some(mut ret) = ret.as_deref_mut() {
                    while ret.next.is_some() {
                        ret = ret.next.as_deref_mut().unwrap();
                    }
                    ret.next = Some(cur);
                } else {
                    ret = Some(cur);
                }
            }
            ctxt.skip_blanks();
            ctxt.current_byte() == b'|'
        } {}
        if ctxt.current_byte() != b')' {
            xml_fatal_err(ctxt, XmlParserErrors::XmlErrAttlistNotFinished, None);
            return ret;
        }
        ctxt.skip_char();
        ret
    }
}

/// Parse an attribute default declaration
///
/// ```text
/// [60] DefaultDecl ::= '#REQUIRED' | '#IMPLIED' | (('#FIXED' S)? AttValue)
/// ```
///
/// `[ VC: Required Attribute ]`  
/// if the default declaration is the keyword #REQUIRED, then the
/// attribute must be specified for all elements of the type in the
/// attribute-list declaration.
///
/// `[ VC: Attribute Default Legal ]`  
/// The declared default value must meet the lexical constraints of
/// the declared attribute type c.f. xmlValidateAttributeDecl()
///
/// `[ VC: Fixed Attribute Default ]`  
/// if an attribute has a default value declared with the #FIXED
/// keyword, instances of that attribute must match the default value.
///
/// `[ WFC: No < in Attribute Values ]`  
/// handled in xmlParseAttValue()
///
/// returns: XML_ATTRIBUTE_NONE, XML_ATTRIBUTE_REQUIRED, XML_ATTRIBUTE_IMPLIED
///  or XML_ATTRIBUTE_FIXED.
#[doc(alias = "xmlParseDefaultDecl")]
pub(crate) unsafe fn parse_default_decl(
    ctxt: &mut XmlParserCtxt,
) -> (XmlAttributeDefault, Option<String>) {
    unsafe {
        if ctxt.content_bytes().starts_with(b"#REQUIRED") {
            ctxt.advance(9);
            return (XmlAttributeDefault::XmlAttributeRequired, None);
        }
        if ctxt.content_bytes().starts_with(b"#IMPLIED") {
            ctxt.advance(8);
            return (XmlAttributeDefault::XmlAttributeImplied, None);
        }
        let mut val = XmlAttributeDefault::XmlAttributeNone;
        if ctxt.content_bytes().starts_with(b"#FIXED") {
            ctxt.advance(6);
            val = XmlAttributeDefault::XmlAttributeFixed;
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after '#FIXED'\n",
                );
            }
        }
        let ret = parse_att_value(ctxt);
        ctxt.instate = XmlParserInputState::XmlParserDTD;
        if ret.is_none() {
            xml_fatal_err_msg(
                ctxt,
                XmlParserErrors::try_from(ctxt.err_no).unwrap(),
                "Attribute default value declaration error\n",
            );
        }
        (val, ret)
    }
}

/// Parse an entity declaration. Always consumes '<!'.
///
/// ```text
/// [70] EntityDecl ::= GEDecl | PEDecl
/// [71] GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
/// [72] PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
/// [73] EntityDef ::= EntityValue | (ExternalID NDataDecl?)
/// [74] PEDef ::= EntityValue | ExternalID
/// [76] NDataDecl ::= S 'NDATA' S Name
/// ```
///
/// `[ VC: Notation Declared ]`  
/// The Name must match the declared name of a notation.
#[doc(alias = "xmlParseEntityDecl")]
pub(crate) unsafe fn parse_entity_decl(ctxt: &mut XmlParserCtxt) {
    unsafe {
        let mut is_parameter: i32 = 0;

        if !ctxt.content_bytes().starts_with(b"<!") {
            return;
        }
        ctxt.advance(2);

        // GROW; done in the caller
        if ctxt.content_bytes().starts_with(b"ENTITY") {
            let inputid: i32 = ctxt.input().unwrap().id;
            ctxt.advance(6);
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after '<!ENTITY'\n",
                );
            }

            if ctxt.current_byte() == b'%' {
                ctxt.skip_char();
                if ctxt.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required after '%'\n",
                    );
                }
                is_parameter = 1;
            }

            let Some(name) = parse_name(ctxt) else {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    "xmlParseEntityDecl: no name\n",
                );
                return;
            };
            if name.contains(':') {
                xml_ns_err!(
                    ctxt,
                    XmlParserErrors::XmlNsErrColon,
                    "colons are forbidden from entities names '{}'\n",
                    name
                );
            }
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after the entity name\n",
                );
            }

            ctxt.instate = XmlParserInputState::XmlParserEntityDecl;

            // parsed entity value without reference substituted
            let mut orig = None;

            // handle the various case of definitions...
            if is_parameter != 0 {
                if ctxt.current_byte() == b'"' || ctxt.current_byte() == b'\'' {
                    let (value, original) = parse_entity_value(ctxt);
                    orig = original;
                    if let Some(value) = value {
                        if ctxt.disable_sax == 0 {
                            if let Some(entity_decl) =
                                ctxt.sax.as_deref_mut().and_then(|sax| sax.entity_decl)
                            {
                                entity_decl(
                                    ctxt.user_data.clone(),
                                    &name,
                                    XmlEntityType::XmlInternalParameterEntity,
                                    None,
                                    None,
                                    Some(&value),
                                );
                            }
                        }
                    }
                } else {
                    let (literal, uri) = parse_external_id(ctxt, true);
                    if uri.is_none() && literal.is_none() {
                        xml_fatal_err(ctxt, XmlParserErrors::XmlErrValueRequired, None);
                    }
                    if let Some(uri) = uri {
                        if let Some(parsed_uri) = XmlURI::parse(&uri) {
                            // This really ought to be a well formedness error
                            // but the XML Core WG decided otherwise c.f. issue
                            // E26 of the XML erratas.
                            if parsed_uri.fragment.is_some() {
                                // Okay this is foolish to block those but not invalid URIs.
                                xml_fatal_err(ctxt, XmlParserErrors::XmlErrURIFragment, None);
                            } else if ctxt.disable_sax == 0 {
                                if let Some(entity_decl) =
                                    ctxt.sax.as_deref_mut().and_then(|sax| sax.entity_decl)
                                {
                                    entity_decl(
                                        ctxt.user_data.clone(),
                                        &name,
                                        XmlEntityType::XmlExternalParameterEntity,
                                        literal.as_deref(),
                                        Some(&uri),
                                        None,
                                    );
                                }
                            }
                        } else {
                            xml_err_msg_str!(
                                ctxt,
                                XmlParserErrors::XmlErrInvalidURI,
                                "Invalid URI: {}\n",
                                uri
                            );
                        }
                    }
                }
            } else if ctxt.current_byte() == b'"' || ctxt.current_byte() == b'\'' {
                let (value, original) = parse_entity_value(ctxt);
                orig = original;
                if ctxt.disable_sax == 0 {
                    if let Some(entity_decl) =
                        ctxt.sax.as_deref_mut().and_then(|sax| sax.entity_decl)
                    {
                        entity_decl(
                            ctxt.user_data.clone(),
                            &name,
                            XmlEntityType::XmlInternalGeneralEntity,
                            None,
                            None,
                            value.as_deref(),
                        );
                    }
                }
                // For expat compatibility in SAX mode.
                if ctxt
                    .my_doc
                    .is_none_or(|doc| doc.version.as_deref() == Some(SAX_COMPAT_MODE))
                {
                    let mut my_doc = if let Some(my_doc) = ctxt.my_doc {
                        my_doc
                    } else {
                        ctxt.my_doc = xml_new_doc(Some(SAX_COMPAT_MODE));
                        let Some(mut my_doc) = ctxt.my_doc else {
                            xml_err_memory(ctxt, Some("New Doc failed"));
                            return;
                        };
                        my_doc.properties = XmlDocProperties::XmlDocInternal as i32;
                        my_doc
                    };
                    if my_doc.int_subset.is_none() {
                        my_doc.int_subset = xml_new_dtd(ctxt.my_doc, Some("fake"), None, None);
                    }

                    xml_sax2_entity_decl(
                        Some(GenericErrorContext::new(ctxt as *mut XmlParserCtxt)),
                        &name,
                        XmlEntityType::XmlInternalGeneralEntity,
                        None,
                        None,
                        value.as_deref(),
                    );
                }
            } else {
                let (literal, uri) = parse_external_id(ctxt, true);
                if uri.is_none() && literal.is_none() {
                    xml_fatal_err(ctxt, XmlParserErrors::XmlErrValueRequired, None);
                }
                if let Some(uri) = uri.as_deref() {
                    if let Some(parsed_uri) = XmlURI::parse(uri) {
                        // This really ought to be a well formedness error
                        // but the XML Core WG decided otherwise c.f. issue
                        // E26 of the XML erratas.
                        if parsed_uri.fragment.is_some() {
                            // Okay this is foolish to block those but not invalid URIs.
                            xml_fatal_err(ctxt, XmlParserErrors::XmlErrURIFragment, None);
                        }
                    } else {
                        xml_err_msg_str!(
                            ctxt,
                            XmlParserErrors::XmlErrInvalidURI,
                            "Invalid URI: {}\n",
                            uri
                        );
                    }
                }
                if ctxt.current_byte() != b'>' && ctxt.skip_blanks() == 0 {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrSpaceRequired,
                        "Space required before 'NDATA'\n",
                    );
                }
                if ctxt.content_bytes().starts_with(b"NDATA") {
                    ctxt.advance(5);
                    if ctxt.skip_blanks() == 0 {
                        xml_fatal_err_msg(
                            ctxt,
                            XmlParserErrors::XmlErrSpaceRequired,
                            "Space required after 'NDATA'\n",
                        );
                    }
                    let ndata = parse_name(ctxt);
                    if ctxt.disable_sax == 0 {
                        if let Some(unparsed_ent) = ctxt
                            .sax
                            .as_deref_mut()
                            .and_then(|sax| sax.unparsed_entity_decl)
                        {
                            unparsed_ent(
                                ctxt.user_data.clone(),
                                &name,
                                literal.as_deref(),
                                uri.as_deref(),
                                ndata.as_deref(),
                            );
                        }
                    }
                } else {
                    if ctxt.disable_sax == 0 {
                        if let Some(entity_decl) =
                            ctxt.sax.as_deref_mut().and_then(|sax| sax.entity_decl)
                        {
                            entity_decl(
                                ctxt.user_data.clone(),
                                &name,
                                XmlEntityType::XmlExternalGeneralParsedEntity,
                                literal.as_deref(),
                                uri.as_deref(),
                                None,
                            );
                        }
                    }
                    // For expat compatibility in SAX mode.
                    // assuming the entity replacement was asked for
                    if ctxt.replace_entities != 0
                        && ctxt
                            .my_doc
                            .is_none_or(|doc| doc.version.as_deref() == Some(SAX_COMPAT_MODE))
                    {
                        let mut my_doc = if let Some(my_doc) = ctxt.my_doc {
                            my_doc
                        } else {
                            ctxt.my_doc = xml_new_doc(Some(SAX_COMPAT_MODE));
                            let Some(mut my_doc) = ctxt.my_doc else {
                                xml_err_memory(ctxt, Some("New Doc failed"));
                                return;
                            };
                            my_doc.properties = XmlDocProperties::XmlDocInternal as i32;
                            my_doc
                        };

                        if my_doc.int_subset.is_none() {
                            my_doc.int_subset = xml_new_dtd(ctxt.my_doc, Some("fake"), None, None);
                        }
                        xml_sax2_entity_decl(
                            Some(GenericErrorContext::new(ctxt as *mut XmlParserCtxt)),
                            &name,
                            XmlEntityType::XmlExternalGeneralParsedEntity,
                            literal.as_deref(),
                            uri.as_deref(),
                            None,
                        );
                    }
                }
            }
            if matches!(ctxt.instate, XmlParserInputState::XmlParserEOF) {
                return;
            }
            ctxt.skip_blanks();
            if ctxt.current_byte() != b'>' {
                xml_fatal_err_msg_str!(
                    ctxt,
                    XmlParserErrors::XmlErrEntityNotFinished,
                    "xmlParseEntityDecl: entity {} not terminated\n",
                    name
                );
                ctxt.halt();
            } else {
                if inputid != ctxt.input().unwrap().id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        "Entity declaration doesn't start and stop in the same entity\n",
                    );
                }
                ctxt.skip_char();
            }
            if let Some(orig) = orig {
                // Ugly mechanism to save the raw entity value.
                let mut cur = None;

                if is_parameter != 0 {
                    if let Some(get_parameter_entity) = ctxt
                        .sax
                        .as_deref_mut()
                        .and_then(|sax| sax.get_parameter_entity)
                    {
                        cur = get_parameter_entity(ctxt.user_data.clone(), &name);
                    }
                } else {
                    if let Some(get_entity) = ctxt.sax.as_deref_mut().and_then(|sax| sax.get_entity)
                    {
                        cur = get_entity(ctxt.user_data.clone(), &name);
                    }
                    if cur.is_none()
                        && ctxt
                            .user_data
                            .as_ref()
                            .and_then(|d| d.lock().downcast_ref::<*mut XmlParserCtxt>().copied())
                            == Some(ctxt)
                    {
                        cur = xml_sax2_get_entity(
                            Some(GenericErrorContext::new(ctxt as *mut XmlParserCtxt)),
                            &name,
                        );
                    }
                }
                if let Some(mut cur) = cur.filter(|cur| cur.orig.is_null()) {
                    cur.orig = xml_strndup(orig.as_ptr(), orig.len() as i32);
                }
            }
        }
    }
}

/// Parse a notation declaration. Always consumes '<!'.
///
/// ```text
/// [82] NotationDecl ::= '<!NOTATION' S Name S (ExternalID |  PublicID) S? '>'
/// ```
///
/// Hence there is actually 3 choices:
/// - 'PUBLIC' S PubidLiteral
/// - 'PUBLIC' S PubidLiteral S SystemLiteral
/// - 'SYSTEM' S SystemLiteral
///
/// See the NOTE on xmlParseExternalID().
#[doc(alias = "xmlParseNotationDecl")]
pub(crate) unsafe fn parse_notation_decl(ctxt: &mut XmlParserCtxt) {
    unsafe {
        if !ctxt.content_bytes().starts_with(b"<!") {
            return;
        }
        ctxt.advance(2);

        if ctxt.content_bytes().starts_with(b"NOTATION") {
            let inputid: i32 = ctxt.input().unwrap().id;
            ctxt.advance(8);
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after '<!NOTATION'\n",
                );
                return;
            }

            let Some(name) = parse_name(ctxt) else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotStarted, None);
                return;
            };
            if name.contains(':') {
                xml_ns_err!(
                    ctxt,
                    XmlParserErrors::XmlNsErrColon,
                    "colons are forbidden from notation names '{}'\n",
                    name
                );
            }
            if ctxt.skip_blanks() == 0 {
                xml_fatal_err_msg(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    "Space required after the NOTATION name'\n",
                );
                return;
            }

            // Parse the IDs.
            let (pubid, systemid) = parse_external_id(ctxt, false);
            ctxt.skip_blanks();

            if ctxt.current_byte() == b'>' {
                if inputid != ctxt.input().unwrap().id {
                    xml_fatal_err_msg(
                        ctxt,
                        XmlParserErrors::XmlErrEntityBoundary,
                        "Notation declaration doesn't start and stop in the same entity\n",
                    );
                }
                ctxt.skip_char();
                if ctxt.disable_sax == 0 {
                    if let Some(notation_decl) =
                        ctxt.sax.as_deref_mut().and_then(|sax| sax.notation_decl)
                    {
                        notation_decl(
                            ctxt.user_data.clone(),
                            &name,
                            pubid.as_deref(),
                            systemid.as_deref(),
                        );
                    }
                }
            } else {
                xml_fatal_err(ctxt, XmlParserErrors::XmlErrNotationNotFinished, None);
            }
        }
    }
}

use std::{
    cell::RefCell,
    ffi::{CStr, CString, c_void},
    ptr::{null, null_mut},
    rc::Rc,
};

use crate::{
    dict::{XmlDictPtr, xml_dict_create, xml_dict_free, xml_dict_lookup, xml_dict_reference},
    encoding::XmlCharEncoding,
    globals::{GenericError, GenericErrorContext, StructuredError},
    io::XmlParserInputBuffer,
    libxml::{
        globals::xml_free,
        xmlautomata::{XmlAutomataPtr, XmlAutomataStatePtr},
        xmlreader::{XmlTextReaderPtr, xml_text_reader_lookup_namespace},
        xmlregexp::XmlRegExecCtxtPtr,
        xmlschemas::{
            XML_SCHEMA_CTXT_PARSER, XML_SCHEMA_CTXT_VALIDATOR, XmlSchemaAttrInfoPtr,
            XmlSchemaBucketPtr, XmlSchemaConstructionCtxtPtr, XmlSchemaIDCAugPtr,
            XmlSchemaIDCMatcherPtr, XmlSchemaIDCStateObjPtr, XmlSchemaNodeInfoPtr,
            XmlSchemaPSVIIDCKeyPtr, XmlSchemaPSVIIDCNodePtr, XmlSchemaPtr, XmlSchemaRedefPtr,
            XmlSchemaValidityLocatorFunc, xml_schema_clear_attr_infos, xml_schema_clear_elem_info,
            xml_schema_construction_ctxt_free, xml_schema_free_idc_state_obj_list,
            xml_schema_idc_free_key, xml_schema_set_valid_errors,
            xml_schema_set_valid_structured_errors,
        },
        xmlschemastypes::{XmlSchemaValPtr, xml_schema_free_value},
    },
    parser::XmlParserCtxtPtr,
    tree::{XmlDocPtr, XmlNodePtr, xml_free_doc},
    xmlschemas::item_list::xml_schema_item_list_free,
};

use super::{
    error::xml_schema_internal_err,
    item_list::{XmlSchemaItemListPtr, xml_schema_item_list_create},
    items::{XmlSchemaAttributeUseProhibPtr, XmlSchemaTypePtr},
};

/// A schemas validation context
#[doc(alias = "xmlSchemaParserCtxtPtr")]
pub type XmlSchemaParserCtxtPtr = *mut XmlSchemaParserCtxt;
#[doc(alias = "xmlSchemaParserCtxt")]
#[repr(C)]
pub struct XmlSchemaParserCtxt {
    pub(crate) typ: i32,
    pub(crate) err_ctxt: Option<GenericErrorContext>, /* user specific error context */
    pub(crate) error: Option<GenericError>,           /* the callback in case of errors */
    pub(crate) warning: Option<GenericError>,         /* the callback in case of warning */
    pub(crate) err: i32,
    pub(crate) nberrors: i32,
    pub(crate) serror: Option<StructuredError>,

    pub(crate) constructor: XmlSchemaConstructionCtxtPtr,
    pub(crate) owns_constructor: i32, /* TODO: Move this to parser *flags*. */

    // xmlSchemaPtr topschema;
    // xmlHashTablePtr namespaces;
    pub(crate) schema: XmlSchemaPtr, /* The main schema in use */
    pub(crate) counter: i32,

    pub(crate) url: Option<String>,
    pub(crate) doc: Option<XmlDocPtr>,
    pub(crate) preserve: i32, /* Whether the doc should be freed  */

    pub(crate) buffer: *const i8,
    pub(crate) size: i32,

    // Used to build complex element content models
    pub(crate) am: XmlAutomataPtr,
    start: XmlAutomataStatePtr,
    end: XmlAutomataStatePtr,
    pub(crate) state: XmlAutomataStatePtr,

    pub(crate) dict: XmlDictPtr, /* dictionary for interned string names */
    pub(crate) ctxt_type: XmlSchemaTypePtr, /* The current context simple/complex type */
    options: i32,
    pub(crate) vctxt: XmlSchemaValidCtxtPtr,
    pub(crate) is_s4s: i32,
    pub(crate) is_redefine: i32,
    pub(crate) xsi_assemble: i32,
    pub(crate) stop: i32, /* If the parser should stop; i.e. a critical error. */
    pub(crate) target_namespace: *const u8,
    pub(crate) redefined: XmlSchemaBucketPtr, /* The schema to be redefined. */

    pub(crate) redef: XmlSchemaRedefPtr, /* Used for redefinitions. */
    pub(crate) redef_counter: i32,       /* Used for redefinitions. */
    pub(crate) attr_prohibs: XmlSchemaItemListPtr<XmlSchemaAttributeUseProhibPtr>,
}

impl XmlSchemaParserCtxt {
    /// Get the callback information used to handle errors for a parser context
    ///
    /// Returns -1 in case of failure, 0 otherwise
    #[doc(alias = "xmlSchemaGetParserErrors")]
    pub unsafe fn get_errors(
        &self,
        err: Option<&mut Option<GenericError>>,
        warn: Option<&mut Option<GenericError>>,
        ctx: Option<&mut Option<GenericErrorContext>>,
    ) -> i32 {
        if let Some(err) = err {
            *err = self.error;
        }
        if let Some(warn) = warn {
            *warn = self.warning;
        }
        if let Some(ctx) = ctx {
            *ctx = self.err_ctxt.clone();
        }
        0
    }

    /// Set the callback functions used to handle errors for a validation context
    #[doc(alias = "xmlSchemaSetParserErrors")]
    pub unsafe fn set_errors(
        &mut self,
        err: Option<GenericError>,
        warn: Option<GenericError>,
        ctx: Option<GenericErrorContext>,
    ) {
        unsafe {
            self.error = err;
            self.warning = warn;
            self.err_ctxt = ctx.clone();
            if !self.vctxt.is_null() {
                xml_schema_set_valid_errors(self.vctxt, err, warn, ctx);
            }
        }
    }

    /// Set the structured error callback
    #[doc(alias = "xmlSchemaSetParserStructuredErrors")]
    pub unsafe fn set_structured_errors(
        &mut self,
        serror: Option<StructuredError>,
        ctx: Option<GenericErrorContext>,
    ) {
        unsafe {
            self.serror = serror;
            self.err_ctxt = ctx.clone();
            if !self.vctxt.is_null() {
                xml_schema_set_valid_structured_errors(self.vctxt, serror, ctx);
            }
        }
    }
}

impl Default for XmlSchemaParserCtxt {
    fn default() -> Self {
        Self {
            typ: XML_SCHEMA_CTXT_PARSER,
            err_ctxt: None,
            error: None,
            warning: None,
            err: 0,
            nberrors: 0,
            serror: None,
            constructor: null_mut(),
            owns_constructor: 0,
            schema: null_mut(),
            counter: 0,
            url: None,
            doc: None,
            preserve: 0,
            buffer: null(),
            size: 0,
            am: null_mut(),
            start: null_mut(),
            end: null_mut(),
            state: null_mut(),
            dict: null_mut(),
            ctxt_type: null_mut(),
            options: 0,
            vctxt: null_mut(),
            is_s4s: 0,
            is_redefine: 0,
            xsi_assemble: 0,
            stop: 0,
            target_namespace: null(),
            redefined: null_mut(),
            redef: null_mut(),
            redef_counter: 0,
            attr_prohibs: null_mut(),
        }
    }
}

pub(crate) unsafe fn xml_schema_parser_ctxt_create() -> XmlSchemaParserCtxtPtr {
    let mut ret = Box::new(XmlSchemaParserCtxt::default());
    ret.attr_prohibs = xml_schema_item_list_create();
    if ret.attr_prohibs.is_null() {
        return null_mut();
    }
    Box::leak(ret)
}

/// Create an XML Schemas parse context for that file/resource expected
/// to contain an XML Schemas file.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlSchemaNewParserCtxt")]
pub unsafe fn xml_schema_new_parser_ctxt(url: &str) -> XmlSchemaParserCtxtPtr {
    unsafe {
        let ret: XmlSchemaParserCtxtPtr = xml_schema_parser_ctxt_create();
        if ret.is_null() {
            return null_mut();
        }
        (*ret).dict = xml_dict_create();
        (*ret).url = Some(url.to_owned());
        ret
    }
}

/// Create an XML Schemas parse context for that file/resource expected
/// to contain an XML Schemas file.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlSchemaNewParserCtxtUseDict")]
pub(crate) unsafe fn xml_schema_new_parser_ctxt_use_dict(
    url: Option<&str>,
    dict: XmlDictPtr,
) -> XmlSchemaParserCtxtPtr {
    unsafe {
        let ret: XmlSchemaParserCtxtPtr = xml_schema_parser_ctxt_create();
        if ret.is_null() {
            return null_mut();
        }
        (*ret).dict = dict;
        xml_dict_reference(dict);
        if let Some(url) = url {
            (*ret).url = Some(url.to_owned());
        }
        ret
    }
}

/// Create an XML Schemas parse context for that memory buffer expected
/// to contain an XML Schemas file.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlSchemaNewMemParserCtxt")]
pub unsafe fn xml_schema_new_mem_parser_ctxt(
    buffer: *const i8,
    size: i32,
) -> XmlSchemaParserCtxtPtr {
    unsafe {
        if buffer.is_null() || size <= 0 {
            return null_mut();
        }
        let ret: XmlSchemaParserCtxtPtr = xml_schema_parser_ctxt_create();
        if ret.is_null() {
            return null_mut();
        }
        (*ret).buffer = buffer;
        (*ret).size = size;
        (*ret).dict = xml_dict_create();
        ret
    }
}

/// Create an XML Schemas parse context for that document.
/// NB. The document may be modified during the parsing process.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlSchemaNewDocParserCtxt")]
pub unsafe fn xml_schema_new_doc_parser_ctxt(doc: XmlDocPtr) -> XmlSchemaParserCtxtPtr {
    unsafe {
        let ret: XmlSchemaParserCtxtPtr = xml_schema_parser_ctxt_create();
        if ret.is_null() {
            return null_mut();
        }
        (*ret).doc = Some(doc);
        (*ret).dict = xml_dict_create();
        // The application has responsibility for the document
        (*ret).preserve = 1;

        ret
    }
}

/// Free the resources associated to the schema parser context
///
/// # Safety
/// - `ctxt` MUST be a pointer of `XmlSchemaParserCtxt` exactly.
///
/// # Panics
/// - `ctxt.typ` must be equal to `XML_SCHEMA_CTXT_PARSER`.
#[doc(alias = "xmlSchemaFreeParserCtxt")]
pub unsafe fn xml_schema_free_parser_ctxt(ctxt: XmlSchemaParserCtxtPtr) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        assert_eq!((*ctxt).typ, XML_SCHEMA_CTXT_PARSER);
        if let Some(doc) = (*ctxt).doc.filter(|_| (*ctxt).preserve == 0) {
            xml_free_doc(doc);
        }
        if !(*ctxt).vctxt.is_null() {
            xml_schema_free_valid_ctxt((*ctxt).vctxt);
        }
        if (*ctxt).owns_constructor != 0 && !(*ctxt).constructor.is_null() {
            xml_schema_construction_ctxt_free((*ctxt).constructor);
            (*ctxt).constructor = null_mut();
            (*ctxt).owns_constructor = 0;
        }
        if !(*ctxt).attr_prohibs.is_null() {
            xml_schema_item_list_free((*ctxt).attr_prohibs);
        }
        xml_dict_free((*ctxt).dict);
        let _ = Box::from_raw(ctxt);
    }
}

#[doc(alias = "xmlSchemaValidCtxtPtr")]
pub type XmlSchemaValidCtxtPtr = *mut XmlSchemaValidCtxt;
/// A Schemas validation context
#[doc(alias = "xmlSchemaValidCtxt")]
#[repr(C)]
pub struct XmlSchemaValidCtxt {
    typ: i32,
    pub(crate) err_ctxt: Option<GenericErrorContext>, /* user specific data block */
    pub(crate) error: Option<GenericError>,           /* the callback in case of errors */
    pub(crate) warning: Option<GenericError>,         /* the callback in case of warning */
    pub(crate) serror: Option<StructuredError>,

    pub(crate) schema: XmlSchemaPtr, /* The schema in use */
    pub(crate) doc: Option<XmlDocPtr>,
    pub(crate) input: Option<Rc<RefCell<XmlParserInputBuffer>>>,
    pub(crate) enc: XmlCharEncoding,
    // sax: XmlSAXHandlerPtr,
    pub(crate) parser_ctxt: XmlParserCtxtPtr,
    user_data: *mut c_void, /* TODO: What is this for? */
    pub(crate) filename: *mut i8,

    pub(crate) err: i32,
    pub(crate) nberrors: i32,

    pub(crate) node: Option<XmlNodePtr>,
    cur: Option<XmlNodePtr>,
    /* typ: XmlSchemaTypePtr, */
    regexp: XmlRegExecCtxtPtr,
    pub(crate) value: XmlSchemaValPtr,

    value_ws: i32,
    pub(crate) options: i32,
    pub(crate) validation_root: Option<XmlNodePtr>,
    pub(crate) pctxt: XmlSchemaParserCtxtPtr,
    pub(crate) xsi_assemble: i32,

    pub(crate) depth: i32,
    pub(crate) elem_infos: *mut XmlSchemaNodeInfoPtr, /* array of element information */
    pub(crate) size_elem_infos: i32,
    pub(crate) inode: XmlSchemaNodeInfoPtr, /* the current element information */

    pub(crate) aidcs: XmlSchemaIDCAugPtr, /* a list of augmented IDC information */

    pub(crate) xpath_states: XmlSchemaIDCStateObjPtr, /* first active state object. */
    pub(crate) xpath_state_pool: XmlSchemaIDCStateObjPtr, /* first stored state object. */
    pub(crate) idc_matcher_cache: XmlSchemaIDCMatcherPtr, /* Cache for IDC matcher objects. */

    pub(crate) idc_nodes: *mut XmlSchemaPSVIIDCNodePtr, /* list of all IDC node-table entries*/
    pub(crate) nb_idc_nodes: i32,
    pub(crate) size_idc_nodes: i32,

    pub(crate) idc_keys: *mut XmlSchemaPSVIIDCKeyPtr, /* list of all IDC node-table entries */
    pub(crate) nb_idc_keys: i32,
    pub(crate) size_idc_keys: i32,

    pub(crate) flags: i32,

    pub(crate) dict: XmlDictPtr,

    #[cfg(feature = "libxml_reader")]
    pub(crate) reader: XmlTextReaderPtr,

    pub(crate) attr_infos: *mut XmlSchemaAttrInfoPtr,
    pub(crate) nb_attr_infos: i32,
    pub(crate) size_attr_infos: i32,

    pub(crate) skip_depth: i32,
    pub(crate) node_qnames: XmlSchemaItemListPtr<*mut c_void>,
    pub(crate) has_keyrefs: i32,
    pub(crate) create_idcnode_tables: i32,
    pub(crate) psvi_expose_idcnode_tables: i32,

    // Locator for error reporting in streaming mode
    pub(crate) loc_func: Option<XmlSchemaValidityLocatorFunc>,
    pub(crate) loc_ctxt: *mut c_void,
}

impl XmlSchemaValidCtxt {
    #[doc(alias = "xmlSchemaLookupNamespace")]
    pub(crate) unsafe fn lookup_namespace(&self, prefix: Option<&str>) -> *const u8 {
        unsafe {
            match () {
                _ if !self.parser_ctxt.is_null() && (*self.parser_ctxt).sax.is_some() => {
                    for i in (0..=self.depth).rev() {
                        if (*(*self.elem_infos.add(i as usize))).nb_ns_bindings != 0 {
                            let inode: XmlSchemaNodeInfoPtr = *self.elem_infos.add(i as usize);
                            for j in (0..(*inode).nb_ns_bindings * 2).step_by(2) {
                                if (prefix.is_none()
                                    && (*(*inode).ns_bindings.add(j as usize)).is_null())
                                    || (prefix.is_some_and(|prefix| {
                                        !(*(*inode).ns_bindings.add(j as usize)).is_null()
                                            && prefix
                                                == CStr::from_ptr(
                                                    *(*inode).ns_bindings.add(j as usize)
                                                        as *const i8,
                                                )
                                                .to_string_lossy()
                                                .as_ref()
                                    }))
                                {
                                    // Note that the namespace bindings are already
                                    // in a string dict.
                                    return *(*inode).ns_bindings.add(j as usize + 1);
                                }
                            }
                        }
                    }
                    null_mut()
                }
                #[cfg(feature = "libxml_reader")]
                _ if !self.reader.is_null() => {
                    let prefix = prefix.map(|prefix| CString::new(prefix).unwrap());
                    let ns_name: *mut u8 = xml_text_reader_lookup_namespace(
                        &mut *self.reader,
                        prefix
                            .as_deref()
                            .map_or(null_mut(), |prefix| prefix.as_ptr() as *const u8),
                    );
                    if !ns_name.is_null() {
                        let ret: *const u8 = xml_dict_lookup(self.dict, ns_name, -1);
                        xml_free(ns_name as _);
                        ret
                    } else {
                        null_mut()
                    }
                }
                _ => {
                    let Some(mut node) = (*self.inode).node.filter(|node| node.doc.is_some())
                    else {
                        xml_schema_internal_err(
                            self as *const Self as _,
                            "xmlSchemaLookupNamespace",
                            "no node or node's doc available",
                        );
                        return null_mut();
                    };
                    let doc = node.doc;
                    let ns = node.search_ns(doc, prefix);
                    if let Some(ns) = ns {
                        return ns.href;
                    }
                    null_mut()
                }
            }
        }
    }
}

impl Default for XmlSchemaValidCtxt {
    fn default() -> Self {
        Self {
            typ: XML_SCHEMA_CTXT_VALIDATOR,
            err_ctxt: None,
            error: None,
            warning: None,
            serror: None,
            schema: null_mut(),
            doc: None,
            input: None,
            enc: XmlCharEncoding::None,
            parser_ctxt: null_mut(),
            user_data: null_mut(),
            filename: null_mut(),
            err: 0,
            nberrors: 0,
            node: None,
            cur: None,
            regexp: null_mut(),
            value: null_mut(),
            value_ws: 0,
            options: 0,
            validation_root: None,
            pctxt: null_mut(),
            xsi_assemble: 0,
            depth: 0,
            elem_infos: null_mut(),
            size_elem_infos: 0,
            inode: null_mut(),
            aidcs: null_mut(),
            xpath_states: null_mut(),
            xpath_state_pool: null_mut(),
            idc_matcher_cache: null_mut(),
            idc_nodes: null_mut(),
            nb_idc_nodes: 0,
            size_idc_nodes: 0,
            idc_keys: null_mut(),
            nb_idc_keys: 0,
            size_idc_keys: 0,
            flags: 0,
            dict: null_mut(),
            reader: null_mut(),
            attr_infos: null_mut(),
            nb_attr_infos: 0,
            size_attr_infos: 0,
            skip_depth: 0,
            node_qnames: null_mut(),
            has_keyrefs: 0,
            create_idcnode_tables: 0,
            psvi_expose_idcnode_tables: 0,
            loc_func: None,
            loc_ctxt: null_mut(),
        }
    }
}

/// Create an XML Schemas validation context based on the given schema.
///
/// Returns the validation context or NULL in case of error
#[doc(alias = "xmlSchemaNewValidCtxt")]
pub fn xml_schema_new_valid_ctxt(schema: XmlSchemaPtr) -> XmlSchemaValidCtxtPtr {
    let mut ret = Box::new(XmlSchemaValidCtxt::default());
    ret.typ = XML_SCHEMA_CTXT_VALIDATOR;
    ret.dict = xml_dict_create();
    ret.node_qnames = xml_schema_item_list_create();
    ret.schema = schema;
    Box::leak(ret)
}

/// Free the resources associated to the schema validation context
#[doc(alias = "xmlSchemaFreeValidCtxt")]
pub unsafe fn xml_schema_free_valid_ctxt(ctxt: XmlSchemaValidCtxtPtr) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        assert_eq!((*ctxt).typ, XML_SCHEMA_CTXT_VALIDATOR);
        if !(*ctxt).value.is_null() {
            xml_schema_free_value((*ctxt).value);
        }
        if !(*ctxt).pctxt.is_null() {
            xml_schema_free_parser_ctxt((*ctxt).pctxt);
        }
        if !(*ctxt).idc_nodes.is_null() {
            let mut item: XmlSchemaPSVIIDCNodePtr;

            for i in 0..(*ctxt).nb_idc_nodes {
                item = *(*ctxt).idc_nodes.add(i as usize) as _;
                xml_free((*item).keys as _);
                xml_free(item as _);
            }
            xml_free((*ctxt).idc_nodes as _);
        }
        if !(*ctxt).idc_keys.is_null() {
            for i in 0..(*ctxt).nb_idc_keys {
                xml_schema_idc_free_key(*(*ctxt).idc_keys.add(i as usize));
            }
            xml_free((*ctxt).idc_keys as _);
        }

        if !(*ctxt).xpath_states.is_null() {
            xml_schema_free_idc_state_obj_list((*ctxt).xpath_states);
            (*ctxt).xpath_states = null_mut();
        }
        if !(*ctxt).xpath_state_pool.is_null() {
            xml_schema_free_idc_state_obj_list((*ctxt).xpath_state_pool);
            (*ctxt).xpath_state_pool = null_mut();
        }

        // Augmented IDC information.
        if !(*ctxt).aidcs.is_null() {
            let mut cur: XmlSchemaIDCAugPtr = (*ctxt).aidcs;
            let mut next: XmlSchemaIDCAugPtr;
            while {
                next = (*cur).next;
                xml_free(cur as _);
                cur = next;
                !cur.is_null()
            } {}
        }
        if !(*ctxt).attr_infos.is_null() {
            let mut attr: XmlSchemaAttrInfoPtr;

            // Just a paranoid call to the cleanup.
            if (*ctxt).nb_attr_infos != 0 {
                xml_schema_clear_attr_infos(ctxt);
            }
            for i in 0..(*ctxt).size_attr_infos {
                attr = *(*ctxt).attr_infos.add(i as usize);
                xml_free(attr as _);
            }
            xml_free((*ctxt).attr_infos as _);
        }
        if !(*ctxt).elem_infos.is_null() {
            let mut ei: XmlSchemaNodeInfoPtr;

            for i in 0..(*ctxt).size_elem_infos {
                ei = *(*ctxt).elem_infos.add(i as usize);
                if ei.is_null() {
                    break;
                }
                xml_schema_clear_elem_info(ctxt, ei);
                xml_free(ei as _);
            }
            xml_free((*ctxt).elem_infos as _);
        }
        if !(*ctxt).node_qnames.is_null() {
            xml_schema_item_list_free((*ctxt).node_qnames);
        }
        if !(*ctxt).dict.is_null() {
            xml_dict_free((*ctxt).dict);
        }
        if !(*ctxt).filename.is_null() {
            xml_free((*ctxt).filename as _);
        }

        let _ = Box::from_raw(ctxt);
    }
}

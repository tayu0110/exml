use std::ptr::{null, null_mut};

use crate::{
    dict::{XmlDictPtr, xml_dict_create, xml_dict_free, xml_dict_lookup},
    globals::{GenericError, GenericErrorContext, StructuredError},
    libxml::{
        schemas_internals::xml_schema_item_list_free,
        xmlautomata::{XmlAutomataPtr, XmlAutomataStatePtr},
        xmlschemas::{
            XML_SCHEMA_CTXT_PARSER, XmlSchemaBucketPtr, XmlSchemaConstructionCtxtPtr,
            XmlSchemaItemListPtr, XmlSchemaPtr, XmlSchemaRedefPtr, XmlSchemaValidCtxtPtr,
            xml_schema_construction_ctxt_free, xml_schema_free_valid_ctxt,
            xml_schema_item_list_create, xml_schema_set_valid_errors,
            xml_schema_set_valid_structured_errors,
        },
    },
    tree::{XmlDocPtr, xml_free_doc},
};

use super::items::XmlSchemaTypePtr;

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

    pub(crate) url: *const u8,
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
    pub(crate) attr_prohibs: XmlSchemaItemListPtr,
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
            url: null(),
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
    unsafe {
        ret.attr_prohibs = xml_schema_item_list_create();
        if ret.attr_prohibs.is_null() {
            return null_mut();
        }
    }
    Box::leak(ret)
}

/// Create an XML Schemas parse context for that file/resource expected
/// to contain an XML Schemas file.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlSchemaNewParserCtxt")]
pub unsafe fn xml_schema_new_parser_ctxt(url: *const i8) -> XmlSchemaParserCtxtPtr {
    unsafe {
        if url.is_null() {
            return null_mut();
        }

        let ret: XmlSchemaParserCtxtPtr = xml_schema_parser_ctxt_create();
        if ret.is_null() {
            return null_mut();
        }
        (*ret).dict = xml_dict_create();
        (*ret).url = xml_dict_lookup((*ret).dict, url as _, -1);
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

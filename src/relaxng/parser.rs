use std::ptr::null_mut;

use libc::memset;

use crate::{
    globals::{GenericError, GenericErrorContext, StructuredError, GLOBAL_STATE},
    hash::XmlHashTableRef,
    libxml::{
        globals::{xml_free, xml_malloc},
        relaxng::{
            xml_relaxng_free_document, xml_relaxng_free_document_list,
            xml_relaxng_free_include_list, XmlRelaxNGDocumentPtr, XmlRelaxNGGrammarPtr,
            XmlRelaxNGIncludePtr, XmlRelaxNGParserFlag, XmlRelaxNGPtr, XmlRelaxNGValidErr,
        },
        xmlautomata::{XmlAutomataPtr, XmlAutomataStatePtr},
        xmlstring::xml_strdup,
    },
    tree::{xml_copy_doc, xml_free_doc, XmlDocPtr},
};

use super::{xml_relaxng_free_define, xml_rng_perr_memory, XmlRelaxNGDefinePtr};

pub type XmlRelaxNGParserCtxtPtr = *mut XmlRelaxNGParserCtxt;
// TODO: all fieleds are used in only relaxng module.
#[repr(C)]
pub struct XmlRelaxNGParserCtxt {
    pub(crate) user_data: Option<GenericErrorContext>, // user specific data block
    pub(crate) error: Option<GenericError>,            // the callback in case of errors
    pub(crate) warning: Option<GenericError>,          // the callback in case of warning
    pub(crate) serror: Option<StructuredError>,
    err: XmlRelaxNGValidErr,

    schema: XmlRelaxNGPtr,                          // The schema in use
    pub(crate) grammar: XmlRelaxNGGrammarPtr,       // the current grammar
    pub(crate) parentgrammar: XmlRelaxNGGrammarPtr, // the parent grammar
    pub(crate) flags: i32,                          // parser flags
    pub(crate) nb_errors: i32,                      // number of errors at parse time
    nb_warnings: i32,                               // number of warnings at parse time
    pub(crate) define: *const u8,                   // the current define scope
    pub(crate) def: XmlRelaxNGDefinePtr,            // the current define

    pub(crate) nb_interleaves: i32,
    pub(crate) interleaves: Option<XmlHashTableRef<'static, XmlRelaxNGDefinePtr>>, // keep track of all the interleaves

    pub(crate) documents: XmlRelaxNGDocumentPtr, // all the documents loaded
    pub(crate) includes: XmlRelaxNGIncludePtr,   // all the includes loaded
    pub(crate) url: *mut u8,
    pub(crate) document: XmlDocPtr,

    pub(crate) def_nr: i32,                       // number of defines used
    pub(crate) def_max: i32,                      // number of defines allocated
    pub(crate) def_tab: *mut XmlRelaxNGDefinePtr, // pointer to the allocated definitions

    pub(crate) buffer: *const i8,
    pub(crate) size: i32,

    // the document stack
    pub(crate) doc: XmlRelaxNGDocumentPtr, // Current parsed external ref
    pub(crate) doc_nr: i32,                // Depth of the parsing stack
    pub(crate) doc_max: i32,               // Max depth of the parsing stack
    pub(crate) doc_tab: *mut XmlRelaxNGDocumentPtr, // array of docs

    // the include stack
    pub(crate) inc: XmlRelaxNGIncludePtr, // Current parsed include
    pub(crate) inc_nr: i32,               // Depth of the include parsing stack
    pub(crate) inc_max: i32,              // Max depth of the parsing stack
    pub(crate) inc_tab: *mut XmlRelaxNGIncludePtr, // array of incs

    pub(crate) idref: i32, // requires idref checking

    // used to compile content models
    pub(crate) am: XmlAutomataPtr,         // the automata
    pub(crate) state: XmlAutomataStatePtr, // used to build the automata

    pub(crate) crng: i32,    // compact syntax and other flags
    pub(crate) freedoc: i32, // need to free the document
}

impl XmlRelaxNGParserCtxt {
    /// Semi private function used to pass information to a parser context
    /// which are a combination of xmlRelaxNGParserFlag .
    ///
    /// Returns 0 if success and -1 in case of error
    #[doc(alias = "xmlRelaxParserSetFlag")]
    pub fn set_flag(&mut self, mut flags: i32) -> i32 {
        if flags & XmlRelaxNGParserFlag::FreeDoc as i32 != 0 {
            self.crng |= XmlRelaxNGParserFlag::FreeDoc as i32;
            flags -= XmlRelaxNGParserFlag::FreeDoc as i32;
        }
        if flags & XmlRelaxNGParserFlag::Crng as i32 != 0 {
            self.crng |= XmlRelaxNGParserFlag::Crng as i32;
            flags -= XmlRelaxNGParserFlag::Crng as i32;
        }
        if flags != 0 {
            return -1;
        }
        0
    }

    /// Get the callback information used to handle errors for a validation context
    ///
    /// Returns `(Error func, Warning func, Error context)`.
    #[doc(alias = "xmlRelaxNGGetParserErrors")]
    pub fn get_parser_errors(
        &self,
    ) -> (
        Option<GenericError>,
        Option<GenericError>,
        Option<GenericErrorContext>,
    ) {
        (self.error, self.warning, self.user_data.clone())
    }

    /// Set the callback functions used to handle errors for a validation context
    #[doc(alias = "xmlRelaxNGSetParserErrors")]
    pub fn set_parser_errors(
        &mut self,
        err: Option<GenericError>,
        warn: Option<GenericError>,
        ctx: Option<GenericErrorContext>,
    ) {
        self.error = err;
        self.warning = warn;
        self.serror = None;
        self.user_data = ctx;
    }

    /// Set the callback functions used to handle errors for a parsing context
    #[doc(alias = "xmlRelaxNGSetParserStructuredErrors")]
    pub fn set_parser_structured_errors(
        &mut self,
        serror: Option<StructuredError>,
        ctx: Option<GenericErrorContext>,
    ) {
        self.serror = serror;
        self.error = None;
        self.warning = None;
        self.user_data = ctx;
    }
}

/// Create an XML RelaxNGs parse context for that file/resource expected
/// to contain an XML RelaxNGs file.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlRelaxNGNewParserCtxt")]
pub unsafe fn xml_relaxng_new_parser_ctxt(url: *const i8) -> XmlRelaxNGParserCtxtPtr {
    if url.is_null() {
        return null_mut();
    }

    let ret: XmlRelaxNGParserCtxtPtr = xml_malloc(size_of::<XmlRelaxNGParserCtxt>()) as _;
    if ret.is_null() {
        xml_rng_perr_memory(null_mut(), Some("building parser\n"));
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNGParserCtxt>());
    (*ret).url = xml_strdup(url as _) as _;
    GLOBAL_STATE.with_borrow(|state| {
        (*ret).error = Some(state.generic_error);
        (*ret).user_data = state.generic_error_context.clone();
    });
    ret
}

/// Create an XML RelaxNGs parse context for that memory buffer expected
/// to contain an XML RelaxNGs file.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlRelaxNGNewMemParserCtxt")]
pub unsafe fn xml_relaxng_new_mem_parser_ctxt(
    buffer: *const i8,
    size: i32,
) -> XmlRelaxNGParserCtxtPtr {
    if buffer.is_null() || size <= 0 {
        return null_mut();
    }

    let ret: XmlRelaxNGParserCtxtPtr = xml_malloc(size_of::<XmlRelaxNGParserCtxt>()) as _;
    if ret.is_null() {
        xml_rng_perr_memory(null_mut(), Some("building parser\n"));
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNGParserCtxt>());
    (*ret).buffer = buffer;
    (*ret).size = size;
    GLOBAL_STATE.with_borrow(|state| {
        (*ret).error = Some(state.generic_error);
        (*ret).user_data = state.generic_error_context.clone();
    });
    ret
}

/// Create an XML RelaxNGs parser context for that document.
///
/// # Note
/// since the process of compiling a RelaxNG schemas modifies the document,
/// the @doc parameter is duplicated internally.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlRelaxNGNewDocParserCtxt")]
pub unsafe fn xml_relaxng_new_doc_parser_ctxt(doc: XmlDocPtr) -> XmlRelaxNGParserCtxtPtr {
    if doc.is_null() {
        return null_mut();
    }
    let copy: XmlDocPtr = xml_copy_doc(doc, 1);
    if copy.is_null() {
        return null_mut();
    }

    let ret: XmlRelaxNGParserCtxtPtr = xml_malloc(size_of::<XmlRelaxNGParserCtxt>()) as _;
    if ret.is_null() {
        xml_rng_perr_memory(null_mut(), Some("building parser\n"));
        xml_free_doc(copy);
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNGParserCtxt>());
    (*ret).document = copy;
    (*ret).freedoc = 1;
    GLOBAL_STATE.with_borrow(|state| {
        (*ret).user_data = state.generic_error_context.clone();
    });
    ret
}

/// Free the resources associated to the schema parser context
#[doc(alias = "xmlRelaxNGFreeParserCtxt")]
pub unsafe fn xml_relaxng_free_parser_ctxt(ctxt: XmlRelaxNGParserCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    if !(*ctxt).url.is_null() {
        xml_free((*ctxt).url as _);
    }
    if !(*ctxt).doc.is_null() {
        xml_relaxng_free_document((*ctxt).doc);
    }
    if let Some(mut table) = (*ctxt).interleaves.take().map(|t| t.into_inner()) {
        table.clear();
    }
    if !(*ctxt).documents.is_null() {
        xml_relaxng_free_document_list((*ctxt).documents);
    }
    if !(*ctxt).includes.is_null() {
        xml_relaxng_free_include_list((*ctxt).includes);
    }
    if !(*ctxt).doc_tab.is_null() {
        xml_free((*ctxt).doc_tab as _);
    }
    if !(*ctxt).inc_tab.is_null() {
        xml_free((*ctxt).inc_tab as _);
    }
    if !(*ctxt).def_tab.is_null() {
        for i in 0..(*ctxt).def_nr {
            xml_relaxng_free_define(*(*ctxt).def_tab.add(i as usize));
        }
        xml_free((*ctxt).def_tab as _);
    }
    if !(*ctxt).document.is_null() && (*ctxt).freedoc != 0 {
        xml_free_doc((*ctxt).document);
    }
    xml_free(ctxt as _);
}

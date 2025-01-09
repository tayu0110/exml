use std::ptr::{drop_in_place, null_mut};

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
    pub(crate) url: Option<String>,
    pub(crate) document: XmlDocPtr,

    pub(crate) def_tab: Vec<XmlRelaxNGDefinePtr>, // pointer to the allocated definitions

    pub(crate) buffer: *const i8,
    pub(crate) size: i32,

    // the document stack
    pub(crate) doc: XmlRelaxNGDocumentPtr, // Current parsed external ref
    pub(crate) doc_tab: Vec<XmlRelaxNGDocumentPtr>, // array of docs

    // the include stack
    pub(crate) inc: XmlRelaxNGIncludePtr, // Current parsed include
    pub(crate) inc_tab: Vec<XmlRelaxNGIncludePtr>, // array of incs

    pub(crate) idref: i32, // requires idref checking

    // used to compile content models
    pub(crate) am: XmlAutomataPtr,         // the automata
    pub(crate) state: XmlAutomataStatePtr, // used to build the automata

    pub(crate) crng: i32,    // compact syntax and other flags
    pub(crate) freedoc: i32, // need to free the document
}

impl XmlRelaxNGParserCtxt {
    /// Pushes a new doc on top of the doc stack.
    ///
    /// Returns the index in the stack.
    #[doc(alias = "xmlRelaxNGDocumentPush")]
    pub(crate) fn document_push(&mut self, value: XmlRelaxNGDocumentPtr) -> usize {
        self.doc_tab.push(value);
        self.doc = value;
        self.doc_tab.len() - 1
    }

    /// Pops the top doc from the doc stack
    ///
    /// Returns the doc just removed
    #[doc(alias = "xmlRelaxNGDocumentPop")]
    pub(crate) fn document_pop(&mut self) -> XmlRelaxNGDocumentPtr {
        let Some(doc) = self.doc_tab.pop() else {
            return null_mut();
        };
        self.doc = *self.doc_tab.last().unwrap_or(&null_mut());
        doc
    }

    /// Pushes a new include on top of the include stack
    ///
    /// Returns the index in the stack.
    #[doc(alias = "xmlRelaxNGIncludePush")]
    pub(crate) fn include_push(&mut self, value: XmlRelaxNGIncludePtr) -> usize {
        self.inc_tab.push(value);
        self.inc = value;
        self.inc_tab.len() - 1
    }

    /// Pops the top include from the include stack
    ///
    /// Returns the include just removed
    #[doc(alias = "xmlRelaxNGIncludePop")]
    pub(crate) fn include_pop(&mut self) -> XmlRelaxNGIncludePtr {
        let Some(inc) = self.inc_tab.pop() else {
            return null_mut();
        };
        self.inc = *self.inc_tab.last().unwrap_or(&null_mut());
        inc
    }

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

impl Default for XmlRelaxNGParserCtxt {
    fn default() -> Self {
        Self {
            user_data: None,
            error: None,
            warning: None,
            serror: None,
            err: XmlRelaxNGValidErr::default(),
            schema: null_mut(),
            grammar: null_mut(),
            parentgrammar: null_mut(),
            flags: 0,
            nb_errors: 0,
            nb_warnings: 0,
            define: null_mut(),
            def: null_mut(),
            nb_interleaves: 0,
            interleaves: None,
            documents: null_mut(),
            includes: null_mut(),
            url: None,
            document: null_mut(),
            def_tab: vec![],
            buffer: null_mut(),
            size: 0,
            doc: null_mut(),
            doc_tab: vec![],
            inc: null_mut(),
            inc_tab: vec![],
            idref: 0,
            am: null_mut(),
            state: null_mut(),
            crng: 0,
            freedoc: 0,
        }
    }
}

/// Create an XML RelaxNGs parse context for that file/resource expected
/// to contain an XML RelaxNGs file.
///
/// Returns the parser context or NULL in case of error
#[doc(alias = "xmlRelaxNGNewParserCtxt")]
pub unsafe fn xml_relaxng_new_parser_ctxt(url: &str) -> XmlRelaxNGParserCtxtPtr {
    let ret: XmlRelaxNGParserCtxtPtr = xml_malloc(size_of::<XmlRelaxNGParserCtxt>()) as _;
    if ret.is_null() {
        xml_rng_perr_memory(null_mut(), Some("building parser\n"));
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlRelaxNGParserCtxt::default());
    (*ret).url = Some(url.to_owned());
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
    std::ptr::write(&mut *ret, XmlRelaxNGParserCtxt::default());
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
    std::ptr::write(&mut *ret, XmlRelaxNGParserCtxt::default());
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
    (*ctxt).url.take();
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
    for def in (*ctxt).def_tab.drain(..) {
        xml_relaxng_free_define(def);
    }
    if !(*ctxt).document.is_null() && (*ctxt).freedoc != 0 {
        xml_free_doc((*ctxt).document);
    }
    drop_in_place(ctxt);
    xml_free(ctxt as _);
}

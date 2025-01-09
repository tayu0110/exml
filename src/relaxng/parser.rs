use crate::{
    globals::{GenericError, GenericErrorContext, StructuredError},
    hash::XmlHashTableRef,
    libxml::{
        relaxng::{
            XmlRelaxNGDocumentPtr, XmlRelaxNGGrammarPtr, XmlRelaxNGIncludePtr, XmlRelaxNGPtr,
            XmlRelaxNGValidErr,
        },
        xmlautomata::{XmlAutomataPtr, XmlAutomataStatePtr},
    },
    tree::XmlDocPtr,
};

use super::XmlRelaxNGDefinePtr;

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

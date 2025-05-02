//! Rust implementation of `xmllint.c` in the original libxml2.  

// Copyright of the original code is the following.
// --------
// xmllint.c : a small tester program for XML input.
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{
    borrow::Cow,
    cell::RefCell,
    env::args,
    ffi::{CStr, c_long, c_void},
    fs::File,
    io::{Read, Write, stderr, stdin, stdout},
    process::exit,
    ptr::null_mut,
    rc::Rc,
    sync::{
        LazyLock, Mutex,
        atomic::{AtomicI32, AtomicPtr, AtomicUsize, Ordering},
    },
    time::Instant,
};

use clap::Parser;
#[cfg(feature = "c14n")]
use exml::c14n::{XmlC14NMode, xml_c14n_doc_dump_memory};
#[cfg(feature = "libxml_debug")]
use exml::debug_xml::{xml_debug_dump_document, xml_debug_dump_entities, xml_shell};
#[cfg(all(feature = "libxml_push", feature = "html"))]
use exml::html::parser::{html_create_push_parser_ctxt, html_parse_chunk};
#[cfg(feature = "html")]
use exml::html::{
    parser::{HtmlParserOption, html_ctxt_use_options, html_read_file, html_read_memory},
    tree::{html_doc_dump, html_save_file_format},
};
#[cfg(feature = "catalog")]
use exml::libxml::catalog::xml_load_catalogs;
#[cfg(feature = "libxml_reader")]
use exml::libxml::xmlreader::XmlTextReaderPtr;
#[cfg(feature = "libxml_pattern")]
use exml::pattern::{XmlPattern, XmlStreamCtxt, xml_pattern_compile};
#[cfg(feature = "schematron")]
use exml::schematron::{
    XmlSchematron, XmlSchematronParserCtxt, XmlSchematronValidCtxt, XmlSchematronValidOptions,
};
#[cfg(feature = "xinclude")]
use exml::xinclude::xml_xinclude_process_flags;
#[cfg(feature = "schema")]
use exml::xmlschemas::schema::XmlSchema;
use exml::{
    encoding::{XmlCharEncoding, add_encoding_alias},
    error::generic_error_default,
    generic_error,
    globals::{
        GenericError, GenericErrorContext, get_load_ext_dtd_default_value,
        set_deregister_node_func, set_load_ext_dtd_default_value, set_parser_debug_entities,
        set_register_node_func, set_tree_indent_string,
    },
    io::XmlParserInputBuffer,
    libxml::{
        globals::xml_free,
        relaxng::{
            XmlRelaxNG, xml_relaxng_free, xml_relaxng_parse, xml_relaxng_set_valid_errors,
            xml_relaxng_validate_doc,
        },
        xmlmemory::{
            xml_mem_free, xml_mem_malloc, xml_mem_realloc, xml_mem_setup, xml_mem_size,
            xml_mem_used, xml_memory_strdup,
        },
        xmlschemas::{xml_schema_validate_doc, xml_schema_validate_stream},
    },
    parser::{
        XML_COMPLETE_ATTRS, XML_DETECT_IDS, XML_SAX2_MAGIC, XmlExternalEntityLoader, XmlParserCtxt,
        XmlParserCtxtPtr, XmlParserInput, XmlParserOption, XmlSAXHandler, XmlSAXLocator,
        xml_cleanup_parser, xml_ctxt_read_file, xml_ctxt_read_io, xml_ctxt_read_memory,
        xml_get_external_entity_loader, xml_no_net_external_entity_loader, xml_parse_dtd,
        xml_read_file, xml_read_io, xml_read_memory, xml_set_external_entity_loader,
    },
    relaxng::{
        xml_relaxng_free_parser_ctxt, xml_relaxng_free_valid_ctxt, xml_relaxng_new_parser_ctxt,
        xml_relaxng_new_valid_ctxt,
    },
    save::{XmlSaveCtxt, XmlSaveOption},
    tree::{
        NodeCommon, XmlAttrPtr, XmlAttributeDefault, XmlAttributePtr, XmlAttributeType, XmlDocPtr,
        XmlDtdPtr, XmlElementContent, XmlElementPtr, XmlElementTypeVal, XmlEntityPtr,
        XmlEntityType, XmlEnumeration, XmlGenericNodePtr, XmlNodePtr, XmlNsPtr, xml_copy_doc,
        xml_encode_entities_reentrant, xml_free_doc, xml_free_dtd, xml_new_doc, xml_new_doc_node,
    },
    valid::{XmlValidCtxt, xml_valid_get_valid_elements, xml_validate_document, xml_validate_dtd},
    xmlschemas::{
        context::{
            XmlSchemaParserCtxtPtr, XmlSchemaValidCtxtPtr, xml_schema_free_parser_ctxt,
            xml_schema_free_valid_ctxt, xml_schema_new_parser_ctxt, xml_schema_new_valid_ctxt,
        },
        schema::xml_schema_free,
    },
    xpath::{XmlXPathObject, xml_xpath_order_doc_elems},
};

// Error codes.
// These are similar to `xmllintReturnCode` in original xmllint.
const RETURN_OK: i32 = 0; // No error
const ERR_UNCLASS: i32 = 1; // Unclassified
const ERR_DTD: i32 = 2; // Error in DTD
const ERR_VALID: i32 = 3; // Validation error
const ERR_RDFILE: i32 = 4; // CtxtReadFile error
const ERR_SCHEMACOMP: i32 = 5; // Schema compilation
const ERR_OUT: i32 = 6; // Error writing output
const ERR_SCHEMAPAT: i32 = 7; // Error in schema pattern
const ERR_RDREGIS: i32 = 8; // Error in Reader registration
const ERR_MEM: i32 = 9; // Out of memory error
const ERR_XPATH: i32 = 10; // XPath evaluation error

// Internal timing routines to remove the necessity to have
// unix-specific function calls.
static TIMER: Mutex<Option<Instant>> = Mutex::new(None);

// startTimer: call where you want to start timing
fn start_timer() {
    *TIMER.lock().unwrap() = Some(Instant::now());
}

// end_timer: call where you want to stop timing and to print out a
//            message about the timing performed; format is a printf
//            type argument
macro_rules! end_timer {
    ( $fmt:literal, $( $args:expr ),* ) => {
        let lock = TIMER.lock().unwrap();
        let timer = lock.as_ref().expect("Timer has not set.");
        eprint!($fmt, $( $args ),*);
        eprintln!(" took {} ms", timer.elapsed().as_millis());
    };
    ( $fmt:literal ) => {
        end_timer!($fmt, );
    }
}

#[derive(clap::Parser, Debug)]
#[cfg_attr(
    feature = "libxml_output",
    command(
        about = "Parse the XML files and output the result of the parsing.\nThis tool is based on xmllint."
    )
)]
#[cfg_attr(
    not(feature = "libxml_output"),
    command(about = "Parse the XML files.\nThis tool is based on xmllint.")
)]
#[command(version, name = "exmllint", arg_required_else_help = true)]
struct CmdArgs {
    #[clap(required = true)]
    xml_files: Vec<String>,
    /// limits memory allocation to nbbytes bytes
    #[arg(long, value_name = "nbbytes")]
    maxmem: Option<usize>,
    #[cfg(any(feature = "libxml_debug", feature = "libxml_reader"))]
    /// dump a debug tree of the in-memory document
    #[arg(long)]
    debug: bool,
    #[cfg(feature = "libxml_debug")]
    /// run a navigating shell
    #[arg(long)]
    shell: bool,
    #[cfg(feature = "libxml_tree")]
    /// used to test the internal copy implementation
    #[arg(long)]
    copy: bool,
    /// output what was parsable on broken XML documents
    #[arg(long)]
    recover: bool,
    /// remove any internal arbitrary parser limits
    #[arg(long)]
    huge: bool,
    /// substitute entity references by their value
    #[arg(long)]
    noent: bool,
    /// ignore any encoding specified inside the document
    #[arg(long)]
    noenc: bool,
    /// remove redundant namespace declarations
    #[arg(long)]
    nsclean: bool,
    /// replace cdata section with text nodes
    #[arg(long)]
    nocdata: bool,
    /// create document without dictionary
    #[arg(long)]
    nodict: bool,
    /// don't output the result tree
    #[arg(long)]
    noout: bool,
    #[cfg(feature = "libxml_output")]
    /// save to a given file
    #[arg(short, long, value_name = "file")]
    output: Option<String>,
    /// output results as HTML
    #[arg(long)]
    htmlout: bool,
    /// do not put HTML doc wrapper
    #[arg(long)]
    nowrap: bool,
    #[cfg(feature = "html")]
    /// use the HTML parser
    #[arg(long)]
    html: bool,
    #[cfg(feature = "html")]
    /// force to use the XML serializer when using --html
    #[arg(long)]
    xmlout: bool,
    #[cfg(feature = "html")]
    /// do not default HTML doctype
    #[arg(long)]
    nodefdtd: bool,
    /// fetch external DTD
    #[arg(long)]
    loaddtd: bool,
    /// loaddtd + populate the tree with inherited attributes
    #[arg(long)]
    dtdattr: bool,
    #[cfg(feature = "libxml_valid")]
    /// validate the document in addition to std well-formed check
    #[arg(long)]
    valid: bool,
    #[cfg(feature = "libxml_valid")]
    /// do a posteriori validation, i.e after parsing
    #[arg(long)]
    postvalid: bool,
    #[cfg(feature = "libxml_valid")]
    /// do a posteriori validation against a given DTD
    #[arg(long, value_name = "URL")]
    dtdvalid: Option<String>,
    #[cfg(feature = "libxml_valid")]
    /// same but name the DTD with a Public Identifier
    #[arg(long, value_name = "FPI")]
    dtdvalidfpi: Option<String>,
    /// remove the DOCTYPE of the input docs
    #[arg(long)]
    dropdtd: bool,
    /// ad-hoc test for valid insertions
    #[arg(long)]
    insert: bool,
    /// be quiet when succeeded
    #[arg(long)]
    quiet: bool,
    /// print some timings
    #[arg(long)]
    timing: bool,
    /// generate a small doc on the fly
    #[arg(long)]
    auto: bool,
    /// repeat 100 times, for timing or profiling
    #[arg(long, action = clap::ArgAction::Count)]
    repeat: u8,
    #[cfg(feature = "libxml_push")]
    /// use the push mode of the parser
    #[arg(long)]
    push: bool,
    #[cfg(feature = "libxml_push")]
    /// use the push mode of the parser using tiny increments
    #[arg(long)]
    pushsmall: bool,
    /// parse from memory
    #[arg(long)]
    memory: bool,
    /// test user I/O support
    #[arg(long = "test-io")]
    test_io: bool,
    #[cfg(feature = "xinclude")]
    /// do XInclude processing
    #[arg(long)]
    xinclude: bool,
    #[cfg(feature = "xinclude")]
    /// same but do not generate XInclude nodes
    #[arg(long)]
    noxincludenode: bool,
    #[cfg(feature = "xinclude")]
    /// do not fixup xml:base uris
    #[arg(long = "nofixup-base-uris")]
    nofixup_base_uris: bool,
    // `compress` is not supported yet. This requires LIBXML_ZLIB_ENABLED feature.
    // #[cfg(feature = "libxml_output")]
    // /// turn on gzip compression of output
    // #[arg(long)]
    // compress: bool,
    #[arg(long)]
    /// do not emit warnings from parser/validator
    nowarning: bool,
    #[arg(long)]
    /// enable additional warnings
    pedantic: bool,
    #[cfg(feature = "libxml_debug")]
    /// debug the entities defined in the document
    #[arg(long)]
    debugent: bool,
    #[cfg(feature = "c14n")]
    /// save in W3C canonical format v1.0 (with comments)
    #[arg(long)]
    c14n: bool,
    #[cfg(feature = "c14n")]
    /// save in W3C canonical format v1.1 (with comments)
    #[arg(long)]
    c14n11: bool,
    #[cfg(feature = "c14n")]
    /// save in W3C exclusive canonical format (with comments)
    #[arg(long = "exc-c14n")]
    exc_c14n: bool,
    #[cfg(feature = "catalog")]
    /// use SGML catalogs from $SGML_CATALOG_FILES
    /// otherwise XML Catalogs starting from XML_XML_DEFAULT_CATALOG are activated by default
    #[arg(long)]
    catalogs: bool,
    #[cfg(feature = "catalog")]
    /// deactivate all catalogs
    #[arg(long)]
    nocatalogs: bool,
    #[cfg(feature = "libxml_output")]
    /// output in the given encoding
    #[arg(long, value_name = "encoding")]
    encode: Option<String>,
    /// drop (ignorable?) blanks spaces
    #[arg(long)]
    noblanks: bool,
    #[cfg(feature = "libxml_output")]
    /// reformat/reindent the output
    #[arg(long)]
    format: bool,
    #[cfg(feature = "libxml_output")]
    /// pretty-print in a particular style  
    /// - 0: Do not pretty print  
    /// - 1: Format the XML content, as --format  
    /// - 2: Add whitespace inside tags, preserving content  
    #[arg(long, value_name = "STYLE", value_parser = clap::value_parser!(u8).range(0..=2))]
    pretty: Option<u8>,
    #[cfg(feature = "libxml_reader")]
    /// use the streaming interface to process very large files
    #[arg(long)]
    stream: bool,
    #[cfg(feature = "libxml_reader")]
    /// create a reader and walk though the resulting doc
    #[arg(long)]
    walker: bool,
    #[cfg(all(feature = "libxml_reader", feature = "libxml_pattern"))]
    /// test the pattern support
    #[arg(long, value_name = "pattern_value")]
    pattern: Option<String>,
    #[cfg(feature = "sax1")]
    /// use the old SAX1 interfaces for processing
    #[arg(long)]
    sax1: bool,
    /// do not build a tree but work just at the SAX level
    #[arg(long)]
    sax: bool,
    /// verify the node registration code
    #[arg(long)]
    chkregister: bool,
    #[cfg(feature = "schema")]
    /// do RelaxNG validation against the schema
    #[arg(long, value_name = "schema")]
    relaxng: Option<String>,
    #[cfg(feature = "schema")]
    /// do validation against the WXS schema
    #[arg(long, value_name = "schema")]
    schema: Option<String>,
    #[cfg(feature = "schematron")]
    /// do validation against a schematron
    #[arg(long, value_name = "schema")]
    schematron: Option<String>,
    /// refuse to fetch DTDs or entities over network
    #[arg(long)]
    nonet: bool,
    /// do not generate compact text nodes
    #[arg(long)]
    nocompact: bool,
    /// print trace of all external entities loaded
    #[arg(long = "load-trace")]
    load_trace: bool,
    /// provide a set of paths for resources
    #[arg(long, value_name = "paths")]
    path: Option<String>,
    #[cfg(feature = "xpath")]
    /// evaluate the XPath expression, imply --noout
    #[arg(long, value_name = "expr")]
    xpath: Option<String>,
    /// use XML-1.0 parsing rules before the 5th edition
    #[arg(long)]
    oldxml10: bool,
}

static CMD_ARGS: LazyLock<CmdArgs> = LazyLock::new(|| {
    let mut cmd_args = CmdArgs::parse();
    if let Some(_maxmem) = cmd_args.maxmem.filter(|&m| m > 0) {
        unsafe {
            xml_mem_setup(
                Some(my_free_func),
                Some(my_malloc_func),
                Some(my_realloc_func),
                Some(my_strdup_func),
            );
        }
    }
    #[cfg(feature = "libxml_debug")]
    if cmd_args.shell {
        cmd_args.noout = true;
    }
    if cmd_args.recover {
        OPTIONS.fetch_or(XmlParserOption::XmlParseRecover as i32, Ordering::Relaxed);
    }
    if cmd_args.huge {
        OPTIONS.fetch_or(XmlParserOption::XmlParseHuge as i32, Ordering::Relaxed);
    }
    if cmd_args.noenc {
        OPTIONS.fetch_or(XmlParserOption::XmlParseIgnoreEnc as i32, Ordering::Relaxed);
    }
    if cmd_args.nsclean {
        OPTIONS.fetch_or(XmlParserOption::XmlParseNsClean as i32, Ordering::Relaxed);
    }
    if cmd_args.nocdata {
        OPTIONS.fetch_or(XmlParserOption::XmlParseNoCDATA as i32, Ordering::Relaxed);
    }
    if cmd_args.nodict {
        OPTIONS.fetch_or(XmlParserOption::XmlParseNoDict as i32, Ordering::Relaxed);
    }
    #[cfg(feature = "html")]
    if cmd_args.nodefdtd {
        OPTIONS.fetch_or(
            HtmlParserOption::HtmlParseNodefdtd as i32,
            Ordering::Relaxed,
        );
    }
    if cmd_args.loaddtd {
        OPTIONS.fetch_or(XmlParserOption::XmlParseDTDLoad as i32, Ordering::Relaxed);
    }
    if cmd_args.dtdattr {
        cmd_args.loaddtd = true;
        OPTIONS.fetch_or(XmlParserOption::XmlParseDTDAttr as i32, Ordering::Relaxed);
    }
    if cmd_args.valid {
        OPTIONS.fetch_or(XmlParserOption::XmlParseDTDValid as i32, Ordering::Relaxed);
    }
    if cmd_args.postvalid {
        cmd_args.loaddtd = true;
        OPTIONS.fetch_or(XmlParserOption::XmlParseDTDLoad as i32, Ordering::Relaxed);
    }
    if cmd_args.dtdvalid.is_some() {
        cmd_args.loaddtd = true;
        OPTIONS.fetch_or(XmlParserOption::XmlParseDTDLoad as i32, Ordering::Relaxed);
    }
    if cmd_args.dtdvalidfpi.is_some() {
        cmd_args.loaddtd = true;
        OPTIONS.fetch_or(XmlParserOption::XmlParseDTDLoad as i32, Ordering::Relaxed);
    }
    if cmd_args.repeat > 0 {
        REPEAT.store(
            100 * 10usize.pow(cmd_args.repeat as u32 - 1),
            Ordering::Relaxed,
        );
    }
    #[cfg(feature = "libxml_push")]
    if cmd_args.pushsmall {
        PUSHSIZE.store(10, Ordering::Relaxed);
    }
    if cmd_args.xinclude {
        OPTIONS.fetch_or(XmlParserOption::XmlParseXInclude as i32, Ordering::Relaxed);
    }
    if cmd_args.noxincludenode {
        cmd_args.xinclude = true;
        OPTIONS.fetch_or(XmlParserOption::XmlParseXInclude as i32, Ordering::Relaxed);
        OPTIONS.fetch_or(
            XmlParserOption::XmlParseNoXIncnode as i32,
            Ordering::Relaxed,
        );
    }
    if cmd_args.nofixup_base_uris {
        cmd_args.xinclude = true;
        OPTIONS.fetch_or(XmlParserOption::XmlParseXInclude as i32, Ordering::Relaxed);
        OPTIONS.fetch_or(XmlParserOption::XmlParseNoBasefix as i32, Ordering::Relaxed);
    }
    // if cmd_args.compress {
    //     set_compress_mode(9);
    // }
    if cmd_args.nowarning {
        OPTIONS.fetch_or(XmlParserOption::XmlParseNoWarning as i32, Ordering::Relaxed);
        OPTIONS.fetch_and(
            !(XmlParserOption::XmlParsePedantic as i32),
            Ordering::Relaxed,
        );
    }
    if cmd_args.pedantic {
        OPTIONS.fetch_or(XmlParserOption::XmlParsePedantic as i32, Ordering::Relaxed);
        OPTIONS.fetch_and(
            !(XmlParserOption::XmlParseNoWarning as i32),
            Ordering::Relaxed,
        );
    }
    #[cfg(feature = "libxml_debug")]
    if cmd_args.debugent {
        set_parser_debug_entities(1);
    }
    if cmd_args.c14n {
        OPTIONS.fetch_or(
            XmlParserOption::XmlParseNoEnt as i32
                | XmlParserOption::XmlParseDTDAttr as i32
                | XmlParserOption::XmlParseDTDLoad as i32,
            Ordering::Relaxed,
        );
    }
    if cmd_args.c14n11 {
        OPTIONS.fetch_or(
            XmlParserOption::XmlParseNoEnt as i32
                | XmlParserOption::XmlParseDTDAttr as i32
                | XmlParserOption::XmlParseDTDLoad as i32,
            Ordering::Relaxed,
        );
    }
    if cmd_args.exc_c14n {
        OPTIONS.fetch_or(
            XmlParserOption::XmlParseNoEnt as i32
                | XmlParserOption::XmlParseDTDAttr as i32
                | XmlParserOption::XmlParseDTDLoad as i32,
            Ordering::Relaxed,
        );
    }
    if cmd_args.encode.is_some() {
        // OK it's for testing purposes
        add_encoding_alias("UTF-8", "DVEnc");
    }
    if let Some(pretty) = cmd_args.pretty {
        if pretty == 1 {
            cmd_args.format = true;
        }
    }
    #[cfg(feature = "libxml_reader")]
    if cmd_args.walker {
        cmd_args.noout = true;
    }
    if cmd_args.sax1 {
        OPTIONS.fetch_or(XmlParserOption::XmlParseSAX1 as i32, Ordering::Relaxed);
    }
    if cmd_args.relaxng.is_some() {
        cmd_args.noent = true;
    }
    if cmd_args.schema.is_some() {
        cmd_args.noent = true;
    }
    if cmd_args.schematron.is_some() {
        cmd_args.noent = true;
    }
    if cmd_args.nonet {
        OPTIONS.fetch_or(XmlParserOption::XmlParseNoNet as i32, Ordering::Relaxed);
        xml_set_external_entity_loader(xml_no_net_external_entity_loader);
    }
    if cmd_args.nocompact {
        OPTIONS.fetch_and(
            !(XmlParserOption::XmlParseCompact as i32),
            Ordering::Relaxed,
        );
    }
    if let Some(path) = cmd_args.path.as_deref() {
        parse_path(path);
    }
    if cmd_args.xpath.is_some() {
        cmd_args.noout = true;
    }
    if cmd_args.oldxml10 {
        OPTIONS.fetch_or(XmlParserOption::XmlParseOld10 as i32, Ordering::Relaxed);
    }

    if !cmd_args.nocatalogs && cmd_args.catalogs {
        if let Some(catal) = option_env!("SGML_CATALOG_FILES") {
            xml_load_catalogs(catal);
        } else {
            eprintln!("Variable $SGML_CATALOG_FILES not set");
        }
    }

    if cmd_args.chkregister {
        set_register_node_func(Some(register_node));
        set_deregister_node_func(Some(deregister_node));
    }

    if let Ok(indent) = std::env::var("XMLLINT_INDENT") {
        set_tree_indent_string(indent.into());
    }

    unsafe {
        DEFAULT_ENTITY_LOADER = Some(xml_get_external_entity_loader());
        xml_set_external_entity_loader(xmllint_external_entity_loader);
    }

    if cmd_args.loaddtd {
        let mut old = get_load_ext_dtd_default_value();
        old |= XML_DETECT_IDS as i32;
        set_load_ext_dtd_default_value(old);
    }
    if cmd_args.dtdattr {
        let mut old = get_load_ext_dtd_default_value();
        old |= XML_COMPLETE_ATTRS as i32;
        set_load_ext_dtd_default_value(old);
    }
    if cmd_args.noent {
        OPTIONS.fetch_or(XmlParserOption::XmlParseNoEnt as i32, Ordering::Relaxed);
    }
    if cmd_args.noblanks || cmd_args.format {
        OPTIONS.fetch_or(XmlParserOption::XmlParseNoBlanks as i32, Ordering::Relaxed);
    }
    if cmd_args.htmlout && !cmd_args.nowrap {
        let program_name = args().next().expect("Failed to acquire program name");
        generic_error!("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\"\n");
        generic_error!("\t\"http://www.w3.org/TR/REC-html40/loose.dtd\">\n");
        generic_error!("<html><head><title>{program_name} output</title></head>\n");
        generic_error!(
            "<body bgcolor=\"#ffffff\"><h1 align=\"center\">{program_name} output</h1>\n",
        );
    }

    #[cfg(feature = "libxml_reader")]
    let not_stream = !cmd_args.stream;
    #[cfg(not(feature = "libxml_reader"))]
    let not_stream = true;
    #[cfg(feature = "schematron")]
    if !cmd_args.sax && not_stream {
        if let Some(s) = cmd_args.schematron.as_deref() {
            // forces loading the DTDs
            let load_ext = get_load_ext_dtd_default_value() | 1;
            set_load_ext_dtd_default_value(load_ext);
            OPTIONS.fetch_or(XmlParserOption::XmlParseDTDLoad as i32, Ordering::Relaxed);
            if cmd_args.timing {
                start_timer();
            }
            let Some(mut ctxt) = XmlSchematronParserCtxt::new(s) else {
                PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                // goto error;
                xml_cleanup_parser();
                exit(PROGRESULT.load(Ordering::Relaxed));
            };

            let schematron = ctxt.parse();
            if schematron.is_none() {
                generic_error!("Schematron schema {s} failed to compile\n",);
                PROGRESULT.store(ERR_SCHEMACOMP, Ordering::Relaxed);
            }
            *WXSCHEMATRON.lock().unwrap() = schematron;
            if cmd_args.timing {
                end_timer!("Compiling the schemas");
            }
        }
    }
    #[cfg(feature = "schema")]
    if cmd_args.relaxng.is_some() && !cmd_args.sax && not_stream {
        if let Some(r) = cmd_args.relaxng.as_deref() {
            // forces loading the DTDs
            let load_ext = get_load_ext_dtd_default_value() | 1;
            set_load_ext_dtd_default_value(load_ext);
            OPTIONS.fetch_or(XmlParserOption::XmlParseDTDLoad as i32, Ordering::Relaxed);
            unsafe {
                if cmd_args.timing {
                    start_timer();
                }
                let ctxt = xml_relaxng_new_parser_ctxt(r);
                if ctxt.is_null() {
                    PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                    // goto error;
                    xml_cleanup_parser();
                    exit(PROGRESULT.load(Ordering::Relaxed));
                }
                (*ctxt).set_parser_errors(
                    Some(generic_error_default),
                    Some(generic_error_default),
                    None,
                );
                RELAXNGSCHEMAS.store(xml_relaxng_parse(ctxt), Ordering::Relaxed);
                if RELAXNGSCHEMAS.load(Ordering::Relaxed).is_null() {
                    generic_error!("Relax-NG schema {r} failed to compile\n");
                    PROGRESULT.store(ERR_SCHEMACOMP, Ordering::Relaxed);
                }
                xml_relaxng_free_parser_ctxt(ctxt);
                if cmd_args.timing {
                    end_timer!("Compiling the schemas");
                }
            }
        }
    } else if cmd_args.schema.is_some() && not_stream {
        if let Some(schema) = cmd_args.schema.as_deref() {
            unsafe {
                if cmd_args.timing {
                    start_timer();
                }
                let ctxt: XmlSchemaParserCtxtPtr = xml_schema_new_parser_ctxt(schema);
                if ctxt.is_null() {
                    PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                    // goto error;
                    xml_cleanup_parser();
                    exit(PROGRESULT.load(Ordering::Relaxed));
                }
                (*ctxt).set_errors(
                    Some(generic_error_default),
                    Some(generic_error_default),
                    None,
                );
                let wxschemas = (*ctxt).parse();
                if wxschemas.is_null() {
                    generic_error!("WXS schema {schema} failed to compile\n");
                    PROGRESULT.store(ERR_SCHEMACOMP, Ordering::Relaxed);
                }
                WXSCHEMAS.store(wxschemas, Ordering::Relaxed);
                xml_schema_free_parser_ctxt(ctxt);
                if cmd_args.timing {
                    end_timer!("Compiling the schemas");
                }
            }
        }
    }

    #[cfg(all(feature = "libxml_reader", feature = "libxml_pattern"))]
    if cmd_args.pattern.is_some() && !cmd_args.walker {
        if let Some(p) = cmd_args.pattern.as_deref() {
            if let Some(pattern) = xml_pattern_compile(p, 0, None) {
                *PATTERNC.lock().unwrap() = Some(*pattern);
            } else {
                generic_error!("Pattern {p} failed to compile\n");
                PROGRESULT.store(ERR_SCHEMAPAT, Ordering::Relaxed);
            }
        }
    }
    cmd_args
});

#[cfg(feature = "schema")]
static RELAXNGSCHEMAS: AtomicPtr<XmlRelaxNG> = AtomicPtr::new(null_mut());
#[cfg(feature = "schema")]
static WXSCHEMAS: AtomicPtr<XmlSchema> = AtomicPtr::new(null_mut());
#[cfg(feature = "schematron")]
static WXSCHEMATRON: Mutex<Option<XmlSchematron>> = Mutex::new(None);
static REPEAT: AtomicUsize = AtomicUsize::new(0);
#[cfg(feature = "libxml_push")]
static PUSHSIZE: AtomicUsize = AtomicUsize::new(4096);
static PROGRESULT: AtomicI32 = AtomicI32::new(RETURN_OK);
#[cfg(all(feature = "libxml_reader", feature = "libxml_pattern"))]
static PATTERNC: Mutex<Option<XmlPattern>> = Mutex::new(None);
#[cfg(all(feature = "libxml_reader", feature = "libxml_pattern"))]
static PATSTREAM: Mutex<Option<XmlStreamCtxt>> = Mutex::new(None);
static NBREGISTER: AtomicUsize = AtomicUsize::new(0);
static OPTIONS: AtomicI32 = AtomicI32::new(
    XmlParserOption::XmlParseCompact as i32 | XmlParserOption::XmlParseBigLines as i32,
);

// Entity loading control and customization.

const MAX_PATHS: usize = 64;
#[cfg(target_os = "windows")]
const PATH_SEPARATOR: char = ';';
#[cfg(not(target_os = "windows"))]
const PATH_SEPARATOR: char = ':';
static PATHS: Mutex<Vec<String>> = Mutex::new(vec![]);

fn parse_path(mut path: &str) {
    let mut paths = PATHS.lock().unwrap();
    path = path.trim_start_matches([' ', PATH_SEPARATOR]);
    while let Some((p, rem)) = path.split_once([' ', PATH_SEPARATOR]) {
        if paths.len() >= MAX_PATHS {
            eprintln!("MAX_PATHS reached: too many paths");
            return;
        }

        if !p.is_empty() {
            paths.push(p.to_owned());
        }

        path = rem.trim_start_matches([' ', PATH_SEPARATOR]);
    }
}

static mut DEFAULT_ENTITY_LOADER: Option<XmlExternalEntityLoader> = None;

fn xmllint_external_entity_loader(
    url: Option<&str>,
    id: Option<&str>,
    ctxt: &mut XmlParserCtxt,
) -> Option<XmlParserInput<'static>> {
    unsafe {
        let mut warning: Option<GenericError> = None;
        let mut err: Option<GenericError> = None;
        let paths = PATHS.lock().unwrap();
        let mut lastsegment = url;
        let iter = url;

        if let Some(mut iter) = iter.filter(|_| !paths.is_empty()) {
            while !iter.is_empty() {
                if let Some(rem) = iter.strip_prefix('/') {
                    lastsegment = Some(rem);
                }
                iter = &iter[1..];
            }
        }

        if let Some(sax) = ctxt.sax.as_deref_mut() {
            warning = sax.warning.take();
            err = sax.error.take();
        }

        if let Some(loader) = DEFAULT_ENTITY_LOADER {
            let mut ret = loader(url, id, &mut *ctxt);
            if ret.is_some() {
                if let Some(sax) = ctxt.sax.as_deref_mut() {
                    if warning.is_some() {
                        sax.warning = warning;
                    }
                    if err.is_some() {
                        sax.error = err;
                    }
                }
                if CMD_ARGS.load_trace {
                    eprintln!(
                        "Loaded URL=\"{}\" ID=\"{}\"",
                        url.unwrap_or("(null)"),
                        id.unwrap_or("(null)"),
                    );
                }
                return ret;
            }

            for path in paths.iter() {
                let mut new_url = path.clone();
                new_url.push('/');
                new_url.push_str(lastsegment.unwrap());
                ret = loader(Some(&new_url), id, ctxt);
                if ret.is_some() {
                    if let Some(sax) = ctxt.sax.as_deref_mut() {
                        if warning.is_some() {
                            sax.warning = warning;
                        }
                        if err.is_some() {
                            sax.error = err;
                        }
                    }
                    if CMD_ARGS.load_trace {
                        eprintln!(
                            "Loaded URL=\"{}\" ID=\"{}\"",
                            new_url,
                            id.unwrap_or("(null)"),
                        );
                    }
                    return ret;
                }
            }
        }
        if err.is_some() {
            if let Some(sax) = ctxt.sax.as_deref_mut() {
                sax.error = err;
            }
        }
        if let Some(warning) = warning {
            if let Some(sax) = ctxt.sax.as_deref_mut() {
                sax.warning = Some(warning);
            }
            if let Some(url) = url {
                warning(
                    Some(GenericErrorContext::new(
                        ctxt as XmlParserCtxtPtr as XmlParserCtxtPtr<'static>,
                    )),
                    format!("failed to load external entity \"{url}\"\n").as_str(),
                );
            } else if let Some(id) = id {
                warning(
                    Some(GenericErrorContext::new(
                        ctxt as XmlParserCtxtPtr as XmlParserCtxtPtr<'static>,
                    )),
                    format!("failed to load external entity \"{id}\"\n").as_str(),
                );
            }
        }
        None
    }
}

// Memory allocation consumption debugging

unsafe fn oom() {
    let maxmem = CMD_ARGS.maxmem.unwrap_or(0);
    eprintln!("Ran out of memory needs > {} bytes", maxmem);
    PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
}

unsafe extern "C" fn my_free_func(mem: *mut c_void) {
    unsafe {
        xml_mem_free(mem);
    }
}
unsafe extern "C" fn my_malloc_func(size: usize) -> *mut c_void {
    unsafe {
        let ret: *mut c_void = xml_mem_malloc(size);
        let maxmem = CMD_ARGS.maxmem.unwrap_or(0);
        if !ret.is_null() && xml_mem_used() > maxmem as i32 {
            oom();
            xml_mem_free(ret);
            return null_mut();
        }
        ret
    }
}
unsafe extern "C" fn my_realloc_func(mem: *mut c_void, size: usize) -> *mut c_void {
    unsafe {
        let oldsize: usize = xml_mem_size(mem);
        let maxmem = CMD_ARGS.maxmem.unwrap_or(0);

        if xml_mem_used() as usize + size - oldsize > maxmem {
            oom();
            return null_mut();
        }

        xml_mem_realloc(mem, size)
    }
}
unsafe extern "C" fn my_strdup_func(str: *const u8) -> *mut u8 {
    unsafe {
        let ret = xml_memory_strdup(str);
        let maxmem = CMD_ARGS.maxmem.unwrap_or(0);

        if !ret.is_null() && xml_mem_used() > maxmem as i32 {
            oom();
            xml_free(ret as _);
            return null_mut();
        }
        ret
    }
}

// HTML output
thread_local! {
    static BUFFER: RefCell<[u8; 50000]> = const { RefCell::new([0; 50000]) };
}

unsafe fn xml_htmlencode_send() {
    unsafe {
        // xmlEncodeEntitiesReentrant assumes valid UTF-8, but the buffer might
        // end with a truncated UTF-8 sequence. This is a hack to at least avoid
        // an out-of-bounds read.
        BUFFER.with_borrow_mut(|buffer| {
            let len = buffer.len();
            buffer[len - 4..].fill(0);
            let result = xml_encode_entities_reentrant(
                None,
                &CStr::from_ptr(buffer.as_ptr() as *const i8).to_string_lossy(),
            );
            generic_error!("{result}");
            buffer[0] = 0;
        })
    }
}

/// Displays the associated file and line information for the current input
#[doc(alias = "xmlHTMLPrintFileInfo")]
unsafe fn xml_htmlprint_file_info(input: Option<&XmlParserInput>) {
    unsafe {
        generic_error!("<p>");

        BUFFER.with_borrow_mut(|buffer| {
            let len = buffer.iter().position(|b| *b == 0).unwrap_or(buffer.len());
            if let Some(input) = input {
                if let Some(filename) = input.filename.as_deref() {
                    write!(&mut buffer[len..], "{filename}:{}: ", input.line).ok();
                } else {
                    write!(&mut buffer[len..], "Entity: line {}: ", input.line).ok();
                }
            }
            xml_htmlencode_send();
        });
    }
}

/// Displays current context within the input content for error tracking
#[doc(alias = "xmlHTMLPrintFileContext")]
unsafe fn xml_htmlprint_file_context(input: Option<&XmlParserInput>) {
    unsafe {
        const SIZE: usize = 80;

        let Some(input) = input else {
            return;
        };

        let mut cur = input.cur;
        // skip backwards over any end-of-lines
        while 0 < cur
            && cur < input.base_contents().len()
            && matches!(input.base_contents()[cur], b'\n' | b'\r')
        {
            cur -= 1;
        }
        let mut n = 0;
        if cur == input.base_contents().len() {
            cur -= 1;
            n += 1;
        }
        // search backwards for beginning-of-line (to max buff size)
        while n < SIZE && cur > 0 && !matches!(input.base_contents()[cur], b'\n' | b'\r') {
            cur -= 1;
            n += 1;
        }
        let mut cur = &input.base_contents()[cur..];
        if n > 0 && matches!(cur.first(), Some(&(b'\n' | b'\r'))) {
            cur = &cur[1..];
        } else {
            // skip over continuation bytes
            while cur.len() > input.base_contents().len() - input.cur && cur[0] & 0xC0 == 0x80 {
                cur = &cur[1..];
            }
        }
        let col = input.cur - (input.base_contents().len() - cur.len());
        let mut content = String::with_capacity(SIZE);

        // search forward for end-of-line (to max buff size)
        let mut n = 0;
        let chunk = {
            let mut i = 0;
            while i < SIZE.min(cur.len()) && cur[i] != b'\n' && cur[i] != b'\r' {
                i += 1;
            }
            &cur[..i]
        };
        if let Some(chunk) = chunk.utf8_chunks().next() {
            for c in chunk
                .valid()
                .chars()
                .take_while(|&c| c != '\n' && c != '\r')
            {
                n += c.len_utf8();
                if n > SIZE {
                    break;
                }
                content.push(c);
            }
        }
        generic_error!("<pre>\n");
        BUFFER.with_borrow_mut(|buffer| {
            let len = buffer.iter().position(|&b| b == 0).unwrap_or(buffer.len());
            // print out the selected text
            writeln!(&mut buffer[len..], "{content}").ok();
            // create blank line with problem pointer
            let mut ptr = content
                .bytes()
                .take(col)
                .map(|c| if c == b'\t' { '\t' } else { ' ' })
                .collect::<String>();
            if ptr.len() == SIZE {
                ptr.pop();
            }
            ptr.push_str("^\n");
            write!(&mut buffer[len..], "{}", ptr).ok();
            xml_htmlencode_send();
        });
        generic_error!("</pre>");
    }
}

/// Display and format an error messages, gives file, line, position and
/// extra parameters.
#[doc(alias = "xmlHTMLError")]
fn xml_html_error(ctx: Option<GenericErrorContext>, msg: &str) {
    unsafe {
        if let Some(ctx) = ctx {
            let ctxt = ctx.lock();
            let ctxt = *ctxt.downcast_ref::<*mut XmlParserCtxt>().unwrap();

            BUFFER.with_borrow_mut(|buffer| {
                buffer[0] = 0;
                let mut input = (*ctxt).input();
                if let Some(inp) = input {
                    if inp.filename.is_none() && (*ctxt).input_tab.len() > 1 {
                        input = Some(&(*ctxt).input_tab[(*ctxt).input_tab.len() - 2]);
                    }
                }

                xml_htmlprint_file_info(input);

                generic_error!("<b>error</b>: ");
                let len = buffer.iter().position(|&b| b == 0).unwrap_or(buffer.len());
                write!(&mut buffer[len..], "{msg}").ok();
                xml_htmlencode_send();
                generic_error!("</p>\n");
                xml_htmlprint_file_context(input);
                xml_htmlencode_send();
            });
        }
    }
}

/// Display and format a warning messages, gives file, line, position and
/// extra parameters.
#[doc(alias = "xmlHTMLWarning")]
fn xml_html_warning(ctx: Option<GenericErrorContext>, msg: &str) {
    unsafe {
        if let Some(ctx) = ctx {
            let ctxt = ctx.lock();
            let ctxt = *ctxt.downcast_ref::<*mut XmlParserCtxt>().unwrap();

            BUFFER.with_borrow_mut(|buffer| {
                buffer[0] = 0;
                let mut input = (*ctxt).input();
                if let Some(inp) = input {
                    if inp.filename.is_none() && (*ctxt).input_tab.len() > 1 {
                        input = Some(&(*ctxt).input_tab[(*ctxt).input_tab.len() - 2]);
                    }
                }

                xml_htmlprint_file_info(input);

                generic_error!("<b>warning</b>: ");
                let len = buffer.iter().position(|&b| b == 0).unwrap_or(buffer.len());
                write!(&mut buffer[len..], "{msg}").ok();
                xml_htmlencode_send();
                generic_error!("</p>\n");
                xml_htmlprint_file_context(input);
                xml_htmlencode_send();
            });
        }
    }
}

/// Display and format an validity error messages, gives file,
/// line, position and extra parameters.
#[doc(alias = "xmlHTMLValidityError")]
fn xml_html_validity_error(ctx: Option<GenericErrorContext>, msg: &str) {
    unsafe {
        if let Some(ctx) = ctx {
            let ctxt = ctx.lock();
            let ctxt = *ctxt.downcast_ref::<*mut XmlParserCtxt>().unwrap();

            BUFFER.with_borrow_mut(|buffer| {
                buffer[0] = 0;
                let mut input = (*ctxt).input();
                if let Some(inp) = input {
                    if inp.filename.is_none() && (*ctxt).input_tab.len() > 1 {
                        input = Some(&(*ctxt).input_tab[(*ctxt).input_tab.len() - 2]);
                    }
                }

                xml_htmlprint_file_info(input);

                generic_error!("<b>validity error</b>: ");
                let len = buffer.iter().position(|&b| b == 0).unwrap_or(buffer.len());
                write!(&mut buffer[len..], "{msg}").ok();
                xml_htmlencode_send();
                generic_error!("</p>\n");
                xml_htmlprint_file_context(input);
                xml_htmlencode_send();
            });
        }
        PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
    }
}

/// Display and format a validity warning messages, gives file, line,
/// position and extra parameters.
#[doc(alias = "xmlHTMLValidityWarning")]
fn xml_html_validity_warning(ctx: Option<GenericErrorContext>, msg: &str) {
    unsafe {
        if let Some(ctx) = ctx {
            let ctxt = ctx.lock();
            let ctxt = *ctxt.downcast_ref::<*mut XmlParserCtxt>().unwrap();

            BUFFER.with_borrow_mut(|buffer| {
                buffer[0] = 0;
                let mut input = (*ctxt).input();
                if let Some(inp) = input {
                    if inp.filename.is_none() && (*ctxt).input_tab.len() > 1 {
                        input = Some(&(*ctxt).input_tab[(*ctxt).input_tab.len() - 2]);
                    }
                }

                xml_htmlprint_file_info(input);

                generic_error!("<b>validity warning</b>: ");
                let len = buffer.iter().position(|&b| b == 0).unwrap_or(buffer.len());
                write!(&mut buffer[len..], "{msg}").ok();
                xml_htmlencode_send();
                generic_error!("</p>\n");
                xml_htmlprint_file_context(input);
                xml_htmlencode_send();
            });
        }
    }
}

// Shell Interface

/// Read a string
///
/// Returns a pointer to it or NULL on EOF the caller is expected to
///     free the returned string.
#[doc(alias = "xmlShellReadline")]
#[cfg(all(feature = "libxml_debug", feature = "xpath"))]
fn xml_shell_readline(prompt: &str) -> Option<String> {
    use std::io::{Write, stdin, stdout};

    print!("{prompt}");
    stdout().flush().ok();
    let mut line_read = String::new();
    match stdin().read_line(&mut line_read) {
        Ok(len) if len > 0 => Some(line_read),
        _ => None,
    }
}

// SAX based tests

// empty SAX block
static EMPTY_SAXHANDLER_STRUCT: XmlSAXHandler = XmlSAXHandler {
    internal_subset: None,
    is_standalone: None,
    has_internal_subset: None,
    has_external_subset: None,
    resolve_entity: None,
    get_entity: None,
    entity_decl: None,
    notation_decl: None,
    attribute_decl: None,
    element_decl: None,
    unparsed_entity_decl: None,
    set_document_locator: None,
    start_document: None,
    end_document: None,
    start_element: None,
    end_element: None,
    reference: None,
    characters: None,
    ignorable_whitespace: None,
    processing_instruction: None,
    comment: None,
    warning: None,
    error: None,
    fatal_error: None,
    get_parameter_entity: None,
    cdata_block: None,
    external_subset: None,
    initialized: XML_SAX2_MAGIC as _,
    _private: AtomicPtr::new(null_mut()),
    start_element_ns: None,
    end_element_ns: None,
    serror: None,
};

// static xmlSAXHandlerPtr emptySAXHandler = &emptySAXHandlerStruct;
// extern xmlSAXHandlerPtr debugSAXHandler;
static CALLBACKS: AtomicUsize = AtomicUsize::new(0);

/// Is this document tagged standalone ?
///
/// Returns 1 if true
#[doc(alias = "isStandaloneDebug")]
fn is_standalone_debug(_ctx: &mut XmlParserCtxt) -> i32 {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return 0;
    }
    println!("SAX.isStandalone()");
    0
}

/// Does this document has an internal subset
///
/// Returns 1 if true
#[doc(alias = "hasInternalSubsetDebug")]
fn has_internal_subset_debug(_ctx: &mut XmlParserCtxt) -> i32 {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return 0;
    }
    println!("SAX.hasInternalSubset()");
    0
}

/// Does this document has an external subset
///
/// Returns 1 if true
#[doc(alias = "hasExternalSubsetDebug")]
fn has_external_subset_debug(_ctx: &mut XmlParserCtxt) -> i32 {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return 0;
    }
    println!("SAX.hasExternalSubset()");
    0
}

/// Does this document has an internal subset
#[doc(alias = "internalSubsetDebug")]
fn internal_subset_debug(
    _ctx: &mut XmlParserCtxt,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    print!("SAX.internalSubset({},", name.unwrap_or("(null)"));
    if let Some(external_id) = external_id {
        print!(" {external_id},");
    } else {
        print!(" ,");
    }
    if let Some(system_id) = system_id {
        println!(" {system_id})");
    } else {
        println!(" )");
    }
}

/// Does this document has an external subset
#[doc(alias = "externalSubsetDebug")]
fn external_subset_debug(
    _ctx: &mut XmlParserCtxt,
    name: Option<&str>,
    external_id: Option<&str>,
    system_id: Option<&str>,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    print!("SAX.externalSubset({},", name.unwrap_or("(null)"));
    if let Some(external_id) = external_id {
        print!(" {external_id},");
    } else {
        print!(" ,");
    }
    if let Some(system_id) = system_id {
        println!(" {system_id})");
    } else {
        println!(" )");
    }
}

/// Special entity resolver, better left to the parser, it has
/// more context than the application layer.
/// The default behaviour is to NOT resolve the entities, in that case
/// the ENTITY_REF nodes are built in the structure (and the parameter values).
///
/// Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
#[doc(alias = "resolveEntityDebug")]
fn resolve_entity_debug(
    _ctx: &mut XmlParserCtxt,
    public_id: Option<&str>,
    system_id: Option<&str>,
) -> Option<XmlParserInput<'static>> {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return None;
    }
    // let ctxt: xmlParserCtxtPtr = ctx as xmlParserCtxtPtr;

    print!("SAX.resolveEntity(");
    print!("{}", public_id.unwrap_or(" "));
    println!(", {})", system_id.unwrap_or(""));
    None
}

/// Get an entity by name
///
/// Returns the xmlParserInputPtr if inlined or NULL for DOM behaviour.
#[doc(alias = "getEntityDebug")]
fn get_entity_debug(_ctx: &mut XmlParserCtxt, name: &str) -> Option<XmlEntityPtr> {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return None;
    }
    println!("SAX.getEntity({name})");
    None
}

/// Get a parameter entity by name
///
/// Returns the xmlParserInputPtr
#[doc(alias = "getParameterEntityDebug")]
fn get_parameter_entity_debug(_ctx: &mut XmlParserCtxt, name: &str) -> Option<XmlEntityPtr> {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return None;
    }
    println!("SAX.getParameterEntity({name})");
    None
}

/// An entity definition has been parsed
#[doc(alias = "entityDeclDebug")]
fn entity_decl_debug(
    _ctx: &mut XmlParserCtxt,
    name: &str,
    typ: XmlEntityType,
    public_id: Option<&str>,
    system_id: Option<&str>,
    content: Option<&str>,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!(
        "SAX.entityDecl({name}, {}, {}, {}, {})",
        typ as i32,
        public_id.unwrap_or("(null)"),
        system_id.unwrap_or("(null)"),
        content.unwrap_or("(null)")
    );
}

/// An attribute definition has been parsed
#[doc(alias = "attributeDeclDebug")]
fn attribute_decl_debug(
    _ctx: &mut XmlParserCtxt,
    elem: &str,
    name: &str,
    typ: XmlAttributeType,
    def: XmlAttributeDefault,
    default_value: Option<&str>,
    _tree: Option<Box<XmlEnumeration>>,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    if let Some(default_value) = default_value {
        println!(
            "SAX.attributeDecl({elem}, {name}, {}, {}, {default_value}, ...)",
            typ as i32, def as i32,
        );
    } else {
        println!(
            "SAX.attributeDecl({elem}, {name}, {}, {}, NULL, ...)",
            typ as i32, def as i32
        );
    }
}

/// An element definition has been parsed
#[doc(alias = "elementDeclDebug")]
fn element_decl_debug(
    _ctx: &mut XmlParserCtxt,
    name: &str,
    typ: Option<XmlElementTypeVal>,
    _content: Option<Rc<RefCell<XmlElementContent>>>,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!(
        "SAX.elementDecl({name}, {}, ...)",
        typ.map_or(-1, |t| t as i32)
    );
}

/// What to do when a notation declaration has been parsed.
#[doc(alias = "notationDeclDebug")]
fn notation_decl_debug(
    _ctx: &mut XmlParserCtxt,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!(
        "SAX.notationDecl({name}, {}, {})",
        public_id.unwrap_or("(null)"),
        system_id.unwrap_or("(null)"),
    );
}

/// What to do when an unparsed entity declaration is parsed
#[doc(alias = "unparsedEntityDeclDebug")]
fn unparsed_entity_decl_debug(
    _ctx: &mut XmlParserCtxt,
    name: &str,
    public_id: Option<&str>,
    system_id: Option<&str>,
    notation_name: Option<&str>,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!(
        "SAX.unparsedEntityDecl({name}, {}, {}, {})",
        public_id.unwrap_or("(null)"),
        system_id.unwrap_or("(null)"),
        notation_name.unwrap_or("(null)")
    );
}

/// Receive the document locator at startup, actually xmlDefaultSAXLocator
/// Everything is available on the context, so this is useless in our case.
#[doc(alias = "setDocumentLocatorDebug")]
fn set_document_locator_debug(_ctx: &mut XmlParserCtxt, _loc: XmlSAXLocator) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!("SAX.setDocumentLocator()");
}

/// called when the document start being processed.
#[doc(alias = "startDocumentDebug")]
fn start_document_debug(_ctx: &mut XmlParserCtxt) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!("SAX.startDocument()");
}

/// called when the document end has been detected.
#[doc(alias = "endDocumentDebug")]
fn end_document_debug(_ctx: &mut XmlParserCtxt) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!("SAX.endDocument()");
}

/// called when an opening tag has been processed.
#[doc(alias = "startElementDebug")]
fn start_element_debug(_ctx: &mut XmlParserCtxt, name: &str, atts: &[(String, Option<String>)]) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    print!("SAX.startElement({name}");
    for (key, value) in atts {
        print!(", {key}='");
        if let Some(value) = value {
            print!("{value}'");
        }
    }
    println!(")");
}

/// called when the end of an element has been detected.
#[doc(alias = "endElementDebug")]
fn end_element_debug(_ctx: &mut XmlParserCtxt, name: &str) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!("SAX.endElement({name})");
}

/// receiving some chars from the parser.
/// Question: how much at a time ???
#[doc(alias = "charactersDebug")]
fn characters_debug(_ctx: &mut XmlParserCtxt, ch: &str) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!("SAX.characters({ch:30}, {})", ch.len());
}

/// called when an entity reference is detected.
#[doc(alias = "referenceDebug")]
fn reference_debug(_ctx: &mut XmlParserCtxt, name: &str) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!("SAX.reference({name})");
}

/// receiving some ignorable whitespaces from the parser.
/// Question: how much at a time ???
#[doc(alias = "ignorableWhitespaceDebug")]
fn ignorable_whitespace_debug(_ctx: &mut XmlParserCtxt, ch: &str) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!("SAX.ignorableWhitespace({ch:30}, {})", ch.len());
}

/// A processing instruction has been parsed.
#[doc(alias = "processingInstructionDebug")]
fn processing_instruction_debug(_ctx: &mut XmlParserCtxt, target: &str, data: Option<&str>) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    if let Some(data) = data {
        println!("SAX.processingInstruction({target}, {data})");
    } else {
        println!("SAX.processingInstruction({target}, NULL)");
    }
}

/// called when a pcdata block has been parsed
#[doc(alias = "cdataBlockDebug")]
fn cdata_block_debug(_ctx: &mut XmlParserCtxt, value: &str) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!("SAX.pcdata({value:20}, {})", value.len());
}

/// A comment has been parsed.
#[doc(alias = "commentDebug")]
fn comment_debug(_ctx: &mut XmlParserCtxt, value: &str) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    println!("SAX.comment({value})");
}

/// Display and format a warning messages, gives file, line, position and
/// extra parameters.
#[doc(alias = "warningDebug")]
fn warning_debug(_ctx: Option<GenericErrorContext>, msg: &str) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    print!("SAX.warning: {}", msg);
}

/// Display and format a error messages, gives file, line, position and
/// extra parameters.
#[doc(alias = "errorDebug")]
fn error_debug(_ctx: Option<GenericErrorContext>, msg: &str) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    print!("SAX.error: {}", msg);
}

/// Display and format a fatalError messages, gives file, line, position and
/// extra parameters.
#[doc(alias = "fatalErrorDebug")]
fn fatal_error_debug(_ctx: Option<GenericErrorContext>, msg: &str) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    print!("SAX.fatalError: {msg}");
}

static DEBUG_SAXHANDLER_STRUCT: XmlSAXHandler = XmlSAXHandler {
    internal_subset: Some(internal_subset_debug),
    is_standalone: Some(is_standalone_debug),
    has_internal_subset: Some(has_internal_subset_debug),
    has_external_subset: Some(has_external_subset_debug),
    resolve_entity: Some(resolve_entity_debug),
    get_entity: Some(get_entity_debug),
    entity_decl: Some(entity_decl_debug),
    notation_decl: Some(notation_decl_debug),
    attribute_decl: Some(attribute_decl_debug),
    element_decl: Some(element_decl_debug),
    unparsed_entity_decl: Some(unparsed_entity_decl_debug),
    set_document_locator: Some(set_document_locator_debug),
    start_document: Some(start_document_debug),
    end_document: Some(end_document_debug),
    start_element: Some(start_element_debug),
    end_element: Some(end_element_debug),
    reference: Some(reference_debug),
    characters: Some(characters_debug),
    ignorable_whitespace: Some(ignorable_whitespace_debug),
    processing_instruction: Some(processing_instruction_debug),
    comment: Some(comment_debug),
    warning: Some(warning_debug),
    error: Some(error_debug),
    fatal_error: Some(fatal_error_debug),
    get_parameter_entity: Some(get_parameter_entity_debug),
    cdata_block: Some(cdata_block_debug),
    external_subset: Some(external_subset_debug),
    initialized: 1,
    _private: AtomicPtr::new(null_mut()),
    start_element_ns: None,
    end_element_ns: None,
    serror: None,
};

// xmlSAXHandlerPtr debugSAXHandler = &debugSAXHandlerStruct;

/// called when an opening tag has been processed.
#[doc(alias = "startElementNsDebug")]
fn start_element_ns_debug(
    _ctx: &mut XmlParserCtxt,
    localname: &str,
    prefix: Option<&str>,
    uri: Option<&str>,
    namespaces: &[(Option<String>, String)],
    nb_defaulted: usize,
    attributes: &[(String, Option<String>, Option<String>, String)],
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    print!("SAX.startElementNs({localname}");
    print!(", {}", prefix.unwrap_or("NULL"));
    if let Some(uri) = uri {
        print!(", '{uri}'");
    } else {
        print!(", NULL");
    }
    print!(", {}", namespaces.len());

    for (pre, loc) in namespaces {
        print!(", xmlns");
        if let Some(pre) = pre.as_deref() {
            print!(":{pre}");
        }
        print!("='{loc}'");
    }
    print!(", {}, {}", attributes.len(), nb_defaulted);
    for attr in attributes {
        if let Some(prefix) = attr.1.as_deref() {
            print!(", {prefix}:{}='", attr.0);
        } else {
            print!(", {}='", attr.0);
        }
        print!(
            "{}...', {}",
            attr.3.chars().take(4).collect::<String>(),
            attr.3.len()
        );
    }
    println!(")");
}

/// called when the end of an element has been detected.
#[doc(alias = "endElementDebug")]
fn end_element_ns_debug(
    _ctx: &mut XmlParserCtxt,
    localname: &str,
    prefix: Option<&str>,
    uri: Option<&str>,
) {
    CALLBACKS.fetch_add(1, Ordering::Relaxed);
    if CMD_ARGS.noout {
        return;
    }
    print!("SAX.endElementNs({localname}");
    print!(", {}", prefix.unwrap_or("NULL"));
    if let Some(uri) = uri {
        println!(", '{uri}')");
    } else {
        println!(", NULL)");
    }
}

static DEBUG_SAX2_HANDLER_STRUCT: XmlSAXHandler = XmlSAXHandler {
    internal_subset: Some(internal_subset_debug),
    is_standalone: Some(is_standalone_debug),
    has_internal_subset: Some(has_internal_subset_debug),
    has_external_subset: Some(has_external_subset_debug),
    resolve_entity: Some(resolve_entity_debug),
    get_entity: Some(get_entity_debug),
    entity_decl: Some(entity_decl_debug),
    notation_decl: Some(notation_decl_debug),
    attribute_decl: Some(attribute_decl_debug),
    element_decl: Some(element_decl_debug),
    unparsed_entity_decl: Some(unparsed_entity_decl_debug),
    set_document_locator: Some(set_document_locator_debug),
    start_document: Some(start_document_debug),
    end_document: Some(end_document_debug),
    start_element: None,
    end_element: None,
    reference: Some(reference_debug),
    characters: Some(characters_debug),
    ignorable_whitespace: Some(ignorable_whitespace_debug),
    processing_instruction: Some(processing_instruction_debug),
    comment: Some(comment_debug),
    warning: Some(warning_debug),
    error: Some(error_debug),
    fatal_error: Some(fatal_error_debug),
    get_parameter_entity: Some(get_parameter_entity_debug),
    cdata_block: Some(cdata_block_debug),
    external_subset: Some(external_subset_debug),
    initialized: XML_SAX2_MAGIC as _,
    _private: AtomicPtr::new(null_mut()),
    start_element_ns: Some(start_element_ns_debug),
    end_element_ns: Some(end_element_ns_debug),
    serror: None,
};

// static xmlSAXHandlerPtr debugSAX2Handler = &debugSAX2HandlerStruct;

unsafe fn test_sax(filename: &str) {
    unsafe {
        let user_data: &CStr = c"user_data"; /* mostly for debugging */

        CALLBACKS.store(0, Ordering::Relaxed);

        let handler = if CMD_ARGS.noout {
            &raw const EMPTY_SAXHANDLER_STRUCT
        } else {
            #[cfg(feature = "sax1")]
            if CMD_ARGS.sax1 {
                &raw const DEBUG_SAXHANDLER_STRUCT
            } else {
                &raw const DEBUG_SAX2_HANDLER_STRUCT
            }
            #[cfg(not(feature = "sax1"))]
            {
                &raw const DEBUG_SAX2_HANDLER_STRUCT
            }
        };

        #[cfg(not(feature = "schema"))]
        let f = false;
        #[cfg(feature = "schema")]
        let f = !WXSCHEMAS.load(Ordering::Relaxed).is_null();
        if f {
            #[cfg(feature = "schema")]
            {
                let Some(buf) = XmlParserInputBuffer::from_uri(filename, XmlCharEncoding::None)
                else {
                    return;
                };

                let vctxt: XmlSchemaValidCtxtPtr =
                    xml_schema_new_valid_ctxt(WXSCHEMAS.load(Ordering::Relaxed));
                if vctxt.is_null() {
                    PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                    return;
                }
                (*vctxt).set_errors(
                    Some(generic_error_default),
                    Some(generic_error_default),
                    None,
                );
                (*vctxt).set_filename(Some(filename));
                let handler = {
                    let mut hdl = XmlSAXHandler::default();
                    std::ptr::copy(handler, &mut hdl, 1);
                    hdl
                };

                let ret: i32 = xml_schema_validate_stream(
                    vctxt,
                    buf,
                    XmlCharEncoding::None,
                    Some(Box::new(handler)),
                    Some(GenericErrorContext::new(user_data.as_ptr())),
                );
                if REPEAT.load(Ordering::Relaxed) == 0 {
                    match ret.cmp(&0) {
                        std::cmp::Ordering::Equal => {
                            if !CMD_ARGS.quiet {
                                eprintln!("{} validates", filename);
                            }
                        }
                        std::cmp::Ordering::Greater => {
                            eprintln!("{} fails to validate", filename);
                            PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
                        }
                        std::cmp::Ordering::Less => {
                            eprintln!("{} validation generated an internal error", filename);
                            PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
                        }
                    }
                }
                xml_schema_free_valid_ctxt(vctxt);
            }
        } else {
            let handler = {
                let mut hdl = XmlSAXHandler::default();
                std::ptr::copy(handler, &mut hdl, 1);
                hdl
            };
            // Create the parser context amd hook the input
            let Ok(mut ctxt) = XmlParserCtxt::new_sax_parser(
                Some(Box::new(handler)),
                Some(GenericErrorContext::new(user_data.as_ptr())),
            ) else {
                PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                return;
            };
            xml_ctxt_read_file(&mut ctxt, filename, None, OPTIONS.load(Ordering::Relaxed));

            if let Some(my_doc) = ctxt.my_doc.take() {
                eprintln!("SAX generated a doc !");
                xml_free_doc(my_doc);
            }
        }
    }
}

// Stream Test processing

#[cfg(feature = "libxml_reader")]
unsafe fn process_node(reader: XmlTextReaderPtr) {
    unsafe {
        use exml::libxml::xmlreader::XmlReaderTypes;

        let typ = (*reader).node_type();
        let empty = (*reader).is_empty_element();

        if CMD_ARGS.debug {
            let name = (*reader).name().unwrap_or_else(|| "--".to_owned());
            let value = (*reader).text_value();

            print!(
                "{} {} {} {} {}",
                (*reader).depth(),
                typ as i32,
                name,
                empty.map_or(-1, |e| e as i32),
                (*reader).has_value() as i32
            );
            if let Some(value) = value {
                println!(" {value}");
            } else {
                println!();
            }
        }
        #[cfg(feature = "libxml_pattern")]
        if let Some(pattern) = PATTERNC.lock().unwrap().as_ref() {
            let mut path = None;
            let mut is_match: i32 = -1;

            if typ == XmlReaderTypes::XmlReaderTypeElement {
                // do the check only on element start
                is_match = pattern.pattern_match((*reader).current_node().unwrap());

                if is_match != 0 {
                    let pattern = CMD_ARGS.pattern.as_deref().unwrap_or("(null)");
                    #[cfg(any(feature = "libxml_tree", feature = "libxml_debug"))]
                    {
                        path = (*reader).current_node().unwrap().get_node_path();
                        println!(
                            "Node {} matches pattern {pattern}",
                            path.as_deref().unwrap()
                        );
                    }
                    #[cfg(not(any(feature = "libxml_tree", feature = "libxml_debug")))]
                    {
                        println!(
                            "Node {} matches pattern {pattern}",
                            CStr::from_ptr(xml_text_reader_const_name(reader)).to_string_lossy(),
                        );
                    }
                }
            }
            if let Some(stream) = PATSTREAM.lock().unwrap().as_mut() {
                let mut ret: i32;

                if typ == XmlReaderTypes::XmlReaderTypeElement {
                    let name = (*reader).local_name();
                    let ns = (*reader).namespace_uri();
                    ret = stream.push(name.as_deref(), ns.as_deref());
                    if ret < 0 {
                        eprintln!("xmlStreamPush() failure");
                    } else if ret != is_match {
                        #[cfg(any(feature = "libxml_tree", feature = "libxml_debug"))]
                        if path.is_none() {
                            path = (*reader).current_node().unwrap().get_node_path();
                        }
                        eprintln!("xmlPatternMatch and xmlStreamPush disagree");
                        let pattern = CMD_ARGS.pattern.as_deref().unwrap_or("(null)");
                        if let Some(path) = path.as_deref() {
                            eprintln!("  pattern {pattern} node {path}",);
                        } else {
                            eprintln!("  pattern {pattern} node {}", (*reader).name().unwrap(),);
                        }
                    }
                }
                if typ == XmlReaderTypes::XmlReaderTypeEndElement
                    || (typ == XmlReaderTypes::XmlReaderTypeElement && empty.unwrap())
                {
                    ret = stream.pop();
                    if ret < 0 {
                        eprintln!("xmlStreamPop() failure");
                    }
                }
            }
        }
    }
}

#[cfg(feature = "libxml_reader")]
unsafe fn stream_file(filename: &str) {
    unsafe {
        use exml::libxml::xmlreader::{
            XmlParserProperties, xml_free_text_reader, xml_reader_for_file, xml_reader_for_memory,
        };

        let mut ret: i32;

        let mut mem = vec![];
        let reader = if CMD_ARGS.memory {
            let Ok(mut file) = File::open(filename) else {
                return;
            };
            if file.read_to_end(&mut mem).is_err() {
                eprintln!("mmap failure for file {}", filename);
                PROGRESULT.store(ERR_RDFILE, Ordering::Relaxed);
                return;
            }
            xml_reader_for_memory(&mem, Some(filename), None, OPTIONS.load(Ordering::Relaxed))
        } else {
            xml_reader_for_file(filename, None, OPTIONS.load(Ordering::Relaxed))
        };
        #[cfg(feature = "libxml_pattern")]
        if let Some(pattern) = PATTERNC.lock().unwrap().as_ref() {
            *PATSTREAM.lock().unwrap() = pattern.get_stream_context().map(|pat| *pat);
            if let Some(stream) = PATSTREAM.lock().unwrap().as_mut() {
                ret = stream.push(None, None);
                if ret < 0 {
                    eprintln!("xmlStreamPush() failure");
                }
            }
        }

        if !reader.is_null() {
            #[cfg(feature = "libxml_valid")]
            if CMD_ARGS.valid {
                (*reader).set_parser_prop(XmlParserProperties::XmlParserValidate, 1);
            } else if CMD_ARGS.loaddtd {
                (*reader).set_parser_prop(XmlParserProperties::XmlParserLoadDTD, 1);
            }
            #[cfg(not(feature = "libxml_valid"))]
            if CMD_ARGS.loaddtd {
                xml_text_reader_set_parser_prop(reader, XmlParserProperties::XmlParserLoadDTD, 1);
            }
            #[cfg(feature = "schema")]
            if let Some(relaxng) = CMD_ARGS.relaxng.as_deref() {
                if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                    start_timer();
                }

                ret = (*reader).relaxng_validate(relaxng);
                if ret < 0 {
                    generic_error!("Relax-NG schema {relaxng} failed to compile\n");
                    PROGRESULT.store(ERR_SCHEMACOMP, Ordering::Relaxed);
                }
                if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                    end_timer!("Compiling the schemas");
                }
            }
            #[cfg(feature = "schema")]
            if let Some(schema) = CMD_ARGS.schema.as_deref() {
                if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                    start_timer();
                }
                ret = (*reader).schema_validate(schema);
                if ret < 0 {
                    generic_error!("XSD schema {schema} failed to compile\n");
                    PROGRESULT.store(ERR_SCHEMACOMP, Ordering::Relaxed);
                }
                if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                    end_timer!("Compiling the schemas");
                }
            }

            // Process all nodes in sequence
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                start_timer();
            }
            ret = (*reader).read();
            while ret == 1 {
                #[cfg(feature = "libxml_pattern")]
                let f = PATTERNC.lock().unwrap().is_some();
                #[cfg(not(feature = "libxml_pattern"))]
                let f = false;
                if CMD_ARGS.debug || f {
                    process_node(reader);
                }
                ret = (*reader).read();
            }
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                #[cfg(any(feature = "schema", feature = "libxml_valid"))]
                {
                    let mut is_validating = false;
                    #[cfg(feature = "schema")]
                    {
                        is_validating |= CMD_ARGS.relaxng.is_some();
                    }
                    #[cfg(feature = "libxml_valid")]
                    {
                        is_validating |= CMD_ARGS.valid;
                    }
                    if is_validating {
                        end_timer!("Parsing and validating");
                    } else {
                        end_timer!("Parsing");
                    }
                }
                #[cfg(not(any(feature = "schema", feature = "libxml_valid")))]
                {
                    end_timer!("Parsing");
                }
            }

            #[cfg(feature = "libxml_valid")]
            if CMD_ARGS.valid && !(*reader).is_valid().unwrap_or(false) {
                generic_error!("Document {filename} does not validate\n");
                PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
            }
            #[cfg(feature = "schema")]
            if CMD_ARGS.relaxng.is_some() || CMD_ARGS.schema.is_some() {
                if !(*reader).is_valid().unwrap_or(false) {
                    eprintln!("{} fails to validate", filename);
                    PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
                } else if !CMD_ARGS.quiet {
                    eprintln!("{} validates", filename);
                }
            }
            // Done, cleanup and status
            xml_free_text_reader(reader);
            if ret != 0 {
                eprintln!("{} : failed to parse", filename);
                PROGRESULT.store(ERR_UNCLASS, Ordering::Relaxed);
            }
        } else {
            eprintln!("Unable to open {}", filename);
            PROGRESULT.store(ERR_UNCLASS, Ordering::Relaxed);
        }
        #[cfg(feature = "libxml_pattern")]
        let _ = PATSTREAM.lock().unwrap().take();
    }
}

#[cfg(feature = "libxml_reader")]
unsafe fn walk_doc(doc: XmlDocPtr) {
    unsafe {
        use std::sync::atomic::Ordering;

        use exml::libxml::xmlreader::{xml_free_text_reader, xml_reader_walker};
        #[cfg(feature = "libxml_pattern")]
        use exml::pattern::xml_pattern_compile;

        let mut ret: i32;

        #[cfg(feature = "libxml_pattern")]
        {
            let mut namespaces = [const { (None, None) }; 22];

            let Some(root) = doc.get_root_element() else {
                generic_error!("Document does not have a root element");
                PROGRESULT.store(ERR_UNCLASS, Ordering::Relaxed);
                return;
            };
            let mut i = 0;
            let mut ns = root.ns_def;
            while let Some(now) = ns.filter(|_| i < 10) {
                namespaces[i] = (now.href.clone(), now.prefix.clone());
                i += 1;
                ns = now.next;
            }

            if let Some(pattern) = CMD_ARGS.path.as_deref() {
                if let Some(pattern) = xml_pattern_compile(
                    pattern,
                    0,
                    Some(
                        namespaces[..i]
                            .iter()
                            .map(|(href, pref)| {
                                (
                                    href.clone().map(|href| href.to_string()).unwrap(),
                                    pref.clone().map(|pref| pref.to_string()),
                                )
                            })
                            .collect(),
                    ),
                ) {
                    *PATTERNC.lock().unwrap() = Some(*pattern);
                } else {
                    PATTERNC.lock().unwrap().take();
                    generic_error!("Pattern {pattern} failed to compile\n");
                    PROGRESULT.store(ERR_SCHEMAPAT, Ordering::Relaxed);
                }
            }
            if let Some(pattern) = PATTERNC.lock().unwrap().as_ref() {
                *PATSTREAM.lock().unwrap() = pattern.get_stream_context().map(|pat| *pat);
                if let Some(stream) = PATSTREAM.lock().unwrap().as_mut() {
                    ret = stream.push(None, None);
                    if ret < 0 {
                        eprintln!("xmlStreamPush() failure");
                    }
                }
            }
        }
        let reader: XmlTextReaderPtr = xml_reader_walker(doc);
        if !reader.is_null() {
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                start_timer();
            }
            ret = (*reader).read();
            while ret == 1 {
                #[cfg(feature = "libxml_pattern")]
                let f = PATTERNC.lock().unwrap().is_some();
                #[cfg(not(feature = "libxml_pattern"))]
                let f = false;
                if CMD_ARGS.debug || f {
                    process_node(reader);
                }
                ret = (*reader).read();
            }
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                end_timer!("walking through the doc");
            }
            xml_free_text_reader(reader);
            if ret != 0 {
                eprintln!("failed to walk through the doc");
                PROGRESULT.store(ERR_UNCLASS, Ordering::Relaxed);
            }
        } else {
            eprintln!("Failed to crate a reader from the document");
            PROGRESULT.store(ERR_UNCLASS, Ordering::Relaxed);
        }
        #[cfg(feature = "libxml_pattern")]
        let _ = PATSTREAM.lock().unwrap().take();
    }
}

// XPath Query
#[cfg(feature = "xpath")]
unsafe fn do_xpath_dump(cur: &XmlXPathObject) {
    unsafe {
        use std::io::stdout;

        use exml::{
            io::XmlOutputBuffer,
            xpath::{XmlXPathObjectType, xml_xpath_is_inf, xml_xpath_is_nan},
        };

        match cur.typ {
            XmlXPathObjectType::XPathNodeset => {
                #[cfg(feature = "libxml_output")]
                {
                    if let Some(nodeset) = cur.nodesetval.as_deref() {
                        if !nodeset.node_tab.is_empty() {
                            let Some(mut buf) = XmlOutputBuffer::from_writer(stdout(), None) else {
                                eprintln!("Out of memory for XPath");
                                PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                                return;
                            };
                            for &node in &nodeset.node_tab {
                                buf = node.dump_output(buf, None, 0, 0, None);
                                buf.write_bytes(b"\n").ok();
                            }
                            buf.flush();
                        } else if !CMD_ARGS.quiet {
                            eprintln!("XPath set is empty");
                        }
                    } else if !CMD_ARGS.quiet {
                        eprintln!("XPath set is empty");
                    }
                }
                #[cfg(not(feature = "libxml_output"))]
                {
                    println!("xpath returned {} nodes", cur.nodesetval.node_tab.len());
                }
            }
            XmlXPathObjectType::XPathBoolean => {
                if cur.boolval {
                    println!("true");
                } else {
                    println!("false");
                }
            }
            XmlXPathObjectType::XPathNumber => match xml_xpath_is_inf(cur.floatval) {
                1 => {
                    println!("Infinity");
                }
                -1 => {
                    println!("-Infinity");
                }
                _ => {
                    if xml_xpath_is_nan(cur.floatval) {
                        println!("NaN");
                    } else {
                        println!("{}", cur.floatval);
                    }
                }
            },
            XmlXPathObjectType::XPathString => {
                println!("{}", cur.stringval.as_deref().unwrap());
            }
            XmlXPathObjectType::XPathUndefined => {
                eprintln!("XPath Object is uninitialized");
                PROGRESULT.store(ERR_XPATH, Ordering::Relaxed);
            }
            _ => {
                eprintln!("XPath object of unexpected type");
                PROGRESULT.store(ERR_XPATH, Ordering::Relaxed);
            }
        }
    }
}

#[cfg(feature = "xpath")]
unsafe fn do_xpath_query(doc: XmlDocPtr, query: &str) {
    use exml::xpath::XmlXPathContext;

    unsafe {
        let mut ctxt = XmlXPathContext::new(Some(doc));
        ctxt.node = Some(doc.into());
        let res = ctxt.evaluate(query);

        let Some(res) = res else {
            eprintln!("XPath evaluation failure");
            PROGRESULT.store(ERR_XPATH, Ordering::Relaxed);
            return;
        };
        do_xpath_dump(&res);
    }
}

// Tree Test processing

unsafe fn parse_and_print_file(filename: Option<&str>, rectxt: Option<XmlParserCtxt>) {
    unsafe {
        if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
            start_timer();
        }

        let doc = match filename {
            #[cfg(feature = "libxml_tree")]
            None => {
                if CMD_ARGS.auto {
                    let mut doc = xml_new_doc(Some("1.0")).unwrap();
                    let mut n = xml_new_doc_node(Some(doc), None, "info", None).unwrap();
                    n.set_content("abc");
                    doc.set_root_element(n);
                    Some(doc)
                } else {
                    None
                }
            }
            #[cfg(all(feature = "html", feature = "libxml_push"))]
            _ if CMD_ARGS.html && CMD_ARGS.push => {
                let filename = filename.unwrap();
                let mut stdin = stdin();
                let mut file = File::open(filename).ok();
                let file = file
                    .as_mut()
                    .map(|f| f as &mut dyn Read)
                    .or_else(|| (filename == "-").then_some(&mut stdin as &mut dyn Read));
                let mut doc = None;
                if let Some(f) = file {
                    let mut chars = [0; 4096];

                    if let Some(res) = f.read(&mut chars[..4]).ok().filter(|&res| res > 0) {
                        let Some(mut ctxt) = html_create_push_parser_ctxt(
                            None,
                            None,
                            &chars[..res],
                            Some(filename),
                            XmlCharEncoding::None,
                        ) else {
                            PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                            return;
                        };
                        html_ctxt_use_options(&mut ctxt, OPTIONS.load(Ordering::Relaxed));
                        while let Some(res) = f.read(&mut chars[..]).ok().filter(|&res| res > 0) {
                            html_parse_chunk(&mut ctxt, &chars[..res], 0);
                        }
                        html_parse_chunk(&mut ctxt, &[], 1);
                        doc = ctxt.my_doc;
                    }
                }
                doc
            }
            #[cfg(feature = "html")]
            _ if CMD_ARGS.html && CMD_ARGS.memory => {
                let Ok(mut file) = File::open(filename.unwrap()) else {
                    return;
                };
                let mut mem = vec![];
                if file.read_to_end(&mut mem).is_err() {
                    eprintln!("mmap failure for file {}", filename.unwrap());
                    PROGRESULT.store(ERR_RDFILE, Ordering::Relaxed);
                    return;
                }

                html_read_memory(&mem, filename, None, OPTIONS.load(Ordering::Relaxed))
            }
            #[cfg(feature = "html")]
            _ if CMD_ARGS.html => {
                html_read_file(filename.unwrap(), None, OPTIONS.load(Ordering::Relaxed))
            }
            #[cfg(feature = "libxml_push")]
            _ if CMD_ARGS.push => {
                // build an XML tree from a string;

                let filename = filename.unwrap();
                let mut stdin = stdin();
                let mut file = File::open(filename).ok();
                let file = file
                    .as_mut()
                    .map(|f| f as &mut dyn Read)
                    .or_else(|| (filename == "-").then_some(&mut stdin as &mut dyn Read));
                let mut doc = None;
                if let Some(f) = file {
                    let mut chars = [0; 1024];

                    // if (repeat) size = 1024;
                    if let Some(res) = f.read(&mut chars[..4]).ok().filter(|&res| res > 0) {
                        let Some(mut ctxt) = XmlParserCtxt::new_push_parser(
                            None,
                            None,
                            &chars[..res],
                            Some(filename),
                        ) else {
                            PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                            return;
                        };
                        ctxt.use_options(OPTIONS.load(Ordering::Relaxed));
                        while let Some(res) = f.read(&mut chars[..]).ok().filter(|&res| res > 0) {
                            ctxt.parse_chunk(&chars[..res], 0);
                        }
                        ctxt.parse_chunk(b"", 1);
                        doc = ctxt.my_doc;
                        let ret = ctxt.well_formed;
                        if !ret && !CMD_ARGS.recover {
                            if let Some(doc) = doc.take() {
                                xml_free_doc(doc);
                            }
                        }
                    }
                }
                doc
            }
            _ if CMD_ARGS.test_io => {
                if filename == Some("-") {
                    xml_read_io(stdin(), None, None, OPTIONS.load(Ordering::Relaxed))
                } else if let Some(Ok(f)) = filename.map(File::open) {
                    if let Some(mut rectxt) = rectxt {
                        xml_ctxt_read_io(
                            &mut rectxt,
                            f,
                            filename,
                            None,
                            OPTIONS.load(Ordering::Relaxed),
                        )
                    } else {
                        xml_read_io(f, filename, None, OPTIONS.load(Ordering::Relaxed))
                    }
                } else {
                    None
                }
            }
            _ if CMD_ARGS.htmlout => {
                let mut ctxt = if let Some(rectxt) = rectxt {
                    rectxt
                } else {
                    let Some(ctxt) = XmlParserCtxt::new() else {
                        PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                        return;
                    };
                    ctxt
                };

                if let Some(sax) = ctxt.sax.as_deref_mut() {
                    sax.error = Some(xml_html_error);
                    sax.warning = Some(xml_html_warning);
                }
                ctxt.vctxt.error = Some(xml_html_validity_error);
                ctxt.vctxt.warning = Some(xml_html_validity_warning);

                xml_ctxt_read_file(
                    &mut ctxt,
                    filename.unwrap(),
                    None,
                    OPTIONS.load(Ordering::Relaxed),
                )
            }
            _ if CMD_ARGS.memory => {
                let Ok(mut file) = File::open(filename.unwrap()) else {
                    return;
                };
                let mut mem = vec![];
                if file.read_to_end(&mut mem).is_err() {
                    eprintln!("mmap failure for file {}", filename.unwrap());
                    PROGRESULT.store(ERR_RDFILE, Ordering::Relaxed);
                    return;
                }

                if let Some(mut rectxt) = rectxt {
                    xml_ctxt_read_memory(
                        &mut rectxt,
                        &mem,
                        filename,
                        None,
                        OPTIONS.load(Ordering::Relaxed),
                    )
                } else {
                    xml_read_memory(&mem, filename, None, OPTIONS.load(Ordering::Relaxed))
                }
            }
            #[cfg(feature = "libxml_valid")]
            _ if CMD_ARGS.valid => {
                let mut ctxt = if let Some(rectxt) = rectxt {
                    rectxt
                } else if let Some(ctxt) = XmlParserCtxt::new() {
                    ctxt
                } else {
                    PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                    return;
                };

                let doc = xml_ctxt_read_file(
                    &mut ctxt,
                    filename.unwrap(),
                    None,
                    OPTIONS.load(Ordering::Relaxed),
                );

                if ctxt.valid == 0 {
                    PROGRESULT.store(ERR_RDFILE, Ordering::Relaxed);
                }
                doc
            }
            _ => {
                if let Some(mut rectxt) = rectxt {
                    xml_ctxt_read_file(
                        &mut rectxt,
                        filename.unwrap(),
                        None,
                        OPTIONS.load(Ordering::Relaxed),
                    )
                } else {
                    xml_read_file(filename.unwrap(), None, OPTIONS.load(Ordering::Relaxed))
                }
            }
        };

        // If we don't have a document we might as well give up.
        // Do we want an error message here?  <sven@zen.org>
        let Some(mut doc) = doc else {
            PROGRESULT.store(ERR_UNCLASS, Ordering::Relaxed);
            return;
        };

        if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
            end_timer!("Parsing");
        }

        // Remove DOCTYPE nodes
        if CMD_ARGS.dropdtd {
            let dtd = doc.get_int_subset();
            if let Some(mut dtd) = dtd {
                (*dtd).unlink();
                doc.int_subset = None;
                xml_free_dtd(dtd);
            }
        }

        #[cfg(feature = "xinclude")]
        if CMD_ARGS.xinclude {
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                start_timer();
            }
            if xml_xinclude_process_flags(doc, OPTIONS.load(Ordering::Relaxed)) < 0 {
                PROGRESULT.store(ERR_UNCLASS, Ordering::Relaxed);
            }
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                end_timer!("Xinclude processing");
            }
        }

        #[cfg(feature = "xpath")]
        if let Some(query) = CMD_ARGS.xpath.as_deref() {
            do_xpath_query(doc, query);
        }

        // shell interaction
        #[cfg(all(feature = "libxml_debug", feature = "xpath"))]
        if CMD_ARGS.shell {
            xml_xpath_order_doc_elems(doc);
            xml_shell(
                doc,
                filename.unwrap(),
                Some(xml_shell_readline),
                Some(stdout()),
            );
        }

        // test intermediate copy if needed.
        #[cfg(feature = "libxml_tree")]
        if CMD_ARGS.copy {
            let tmp = doc;
            if CMD_ARGS.timing {
                start_timer();
            }
            doc = xml_copy_doc(doc, 1).unwrap();
            if CMD_ARGS.timing {
                end_timer!("Copying");
            }
            if CMD_ARGS.timing {
                start_timer();
            }
            xml_free_doc(tmp);
            if CMD_ARGS.timing {
                end_timer!("Freeing original");
            }
        }

        match () {
            #[cfg(all(feature = "libxml_valid", feature = "html"))]
            _ if CMD_ARGS.insert && !CMD_ARGS.html => {
                let mut list = [const { Cow::Borrowed("") }; 256];

                if let Some(children) = doc.children() {
                    let mut node = Some(children);
                    while let Some(now) = node.filter(|n| n.last().is_none()) {
                        node = now.next();
                    }
                    if let Some(node) = node {
                        let nb = xml_valid_get_valid_elements(node.last(), None, &mut list[..]);
                        match nb.cmp(&0) {
                            std::cmp::Ordering::Less => {
                                eprintln!("could not get valid list of elements")
                            }
                            std::cmp::Ordering::Equal => {
                                eprintln!("No element can be inserted under root")
                            }
                            std::cmp::Ordering::Greater => {
                                eprintln!("{} element types can be inserted under root:", nb);
                                for l in list.iter().take(nb as usize) {
                                    eprintln!("{l}");
                                }
                            }
                        }
                    }
                }
            }
            #[cfg(feature = "libxml_reader")]
            _ if CMD_ARGS.walker => {
                walk_doc(doc);
            }
            _ => {}
        }
        #[cfg(feature = "libxml_output")]
        if !CMD_ARGS.noout {
            // print it.
            if !cfg!(feature = "libxml_debug") || !CMD_ARGS.debug {
                if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                    start_timer();
                }
                match () {
                    #[cfg(feature = "html")]
                    _ if CMD_ARGS.html && !CMD_ARGS.xmlout => {
                        // if COMPRESS != 0 {
                        //     let o = OUTPUT
                        //         .lock()
                        //         .unwrap()
                        //         .as_ref()
                        //         .map_or(c"-".as_ptr(), |o| o.as_ptr());
                        //     html_save_file(o, doc);
                        // } else
                        if let Some(encoding) = CMD_ARGS.encode.as_deref() {
                            let o = CMD_ARGS.output.as_deref().unwrap_or("-");
                            if CMD_ARGS.format {
                                html_save_file_format(o, doc, Some(encoding), 1);
                            } else {
                                html_save_file_format(o, doc, Some(encoding), 0);
                            }
                        } else if CMD_ARGS.format {
                            let o = CMD_ARGS.output.as_deref().unwrap_or("-");
                            html_save_file_format(o, doc, None, 1);
                        } else if let Some(filename) = CMD_ARGS.output.as_deref() {
                            match File::options().write(true).truncate(true).open(filename) {
                                Ok(mut f) => {
                                    if html_doc_dump(&mut f, doc) < 0 {
                                        PROGRESULT.store(ERR_OUT, Ordering::Relaxed);
                                    }
                                }
                                _ => {
                                    eprintln!("failed to open {filename}");
                                    PROGRESULT.store(ERR_OUT, Ordering::Relaxed);
                                }
                            }
                        } else if html_doc_dump(&mut stdout(), doc) < 0 {
                            PROGRESULT.store(ERR_OUT, Ordering::Relaxed);
                        }
                        if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                            end_timer!("Saving");
                        }
                    }
                    #[cfg(feature = "c14n")]
                    _ if CMD_ARGS.c14n => {
                        let mut result = String::new();

                        let size = xml_c14n_doc_dump_memory(
                            doc,
                            None,
                            XmlC14NMode::XmlC14N1_0,
                            None,
                            true,
                            &mut result,
                        );
                        if size >= 0 {
                            print!("{result}");
                        } else {
                            eprintln!("Failed to canonicalize");
                            PROGRESULT.store(ERR_OUT, Ordering::Relaxed);
                        }
                    }
                    #[cfg(feature = "c14n")]
                    _ if CMD_ARGS.c14n11 => {
                        let mut result = String::new();

                        let size: i32 = xml_c14n_doc_dump_memory(
                            doc,
                            None,
                            XmlC14NMode::XmlC14N1_1,
                            None,
                            true,
                            &mut result,
                        );
                        if size >= 0 {
                            print!("{result}");
                        } else {
                            eprintln!("Failed to canonicalize");
                            PROGRESULT.store(ERR_OUT, Ordering::Relaxed);
                        }
                    }
                    #[cfg(feature = "c14n")]
                    _ if CMD_ARGS.exc_c14n => {
                        let mut result = String::new();

                        let size: i32 = xml_c14n_doc_dump_memory(
                            doc,
                            None,
                            XmlC14NMode::XmlC14NExclusive1_0,
                            None,
                            true,
                            &mut result,
                        );
                        if size >= 0 {
                            print!("{result}");
                        } else {
                            eprintln!("Failed to canonicalize");
                            PROGRESULT.store(ERR_OUT, Ordering::Relaxed);
                        }
                    }
                    _ if CMD_ARGS.memory => {
                        let mut result = vec![];

                        if let Some(encoding) = CMD_ARGS.encode.as_deref() {
                            if CMD_ARGS.format {
                                doc.dump_format_memory_enc(&mut result, Some(encoding), 1);
                            } else {
                                doc.dump_memory_enc(&mut result, Some(encoding));
                            }
                        } else if CMD_ARGS.format {
                            doc.dump_format_memory(&mut result, 1);
                        } else {
                            doc.dump_memory(&mut result);
                        }
                        if stdout().write_all(&result).is_err() {
                            eprintln!("Can't write data");
                        }
                    }
                    _ => {
                        let mut save_opts: i32 = 0;

                        if CMD_ARGS.format {
                            save_opts |= XmlSaveOption::XmlSaveFormat as i32;
                        } else if CMD_ARGS.pretty == Some(2) {
                            save_opts |= XmlSaveOption::XmlSaveWsNonSig as i32;
                        }

                        #[cfg(all(feature = "html", feature = "libxml_valid"))]
                        if CMD_ARGS.xmlout {
                            save_opts |= XmlSaveOption::XmlSaveAsXML as i32;
                        }

                        let encoding = CMD_ARGS.encode.as_deref();
                        let ctxt = if let Some(o) = CMD_ARGS.output.as_deref() {
                            XmlSaveCtxt::save_to_filename(o, encoding, save_opts)
                        } else {
                            XmlSaveCtxt::save_to_io(stdout(), encoding, save_opts)
                        };

                        if let Some(mut ctxt) = ctxt {
                            if ctxt.save_doc(doc) < 0 {
                                let o = CMD_ARGS.output.as_deref().unwrap_or("-");
                                eprintln!("failed save to {o}");
                                PROGRESULT.store(ERR_OUT, Ordering::Relaxed);
                            }
                        } else {
                            PROGRESULT.store(ERR_OUT, Ordering::Relaxed);
                        }
                    }
                }
                if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                    end_timer!("Saving");
                }
            } else {
                #[cfg(feature = "libxml_debug")]
                {
                    if let Some(filename) = CMD_ARGS.output.as_deref() {
                        match File::options().write(true).truncate(true).open(filename) {
                            Ok(f) => {
                                xml_debug_dump_document(Some(f), Some(doc));
                            }
                            _ => {
                                eprintln!("failed to open {filename}");
                                PROGRESULT.store(ERR_OUT, Ordering::Relaxed);
                            }
                        }
                    } else {
                        xml_debug_dump_document(Some(stdout()), Some(doc));
                    }
                }
            }
        }

        // A posteriori validation test
        #[cfg(feature = "libxml_valid")]
        if CMD_ARGS.dtdvalid.is_some() || CMD_ARGS.dtdvalidfpi.is_some() {
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                start_timer();
            }
            let dtd = if let Some(dtd_valid) = CMD_ARGS.dtdvalid.as_deref() {
                xml_parse_dtd(None, Some(dtd_valid))
            } else {
                xml_parse_dtd(CMD_ARGS.dtdvalidfpi.as_deref(), None)
            };
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                end_timer!("Parsing DTD");
            }
            if let Some(dtd) = dtd {
                let mut cvp = XmlValidCtxt::new();
                cvp.error = Some(generic_error_default);
                cvp.warning = Some(generic_error_default);

                if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                    start_timer();
                }
                if xml_validate_dtd(&mut cvp, doc, dtd) == 0 {
                    let filename = filename.unwrap();
                    if let Some(dtd_valid) = CMD_ARGS.dtdvalid.as_deref() {
                        generic_error!(
                            "Document {filename} does not validate against {dtd_valid}\n"
                        );
                    } else {
                        generic_error!(
                            "Document {filename} does not validate against {}\n",
                            CMD_ARGS.dtdvalidfpi.as_deref().unwrap()
                        );
                    }
                    PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
                }
                if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                    end_timer!("Validating against DTD");
                }
                xml_free_dtd(dtd);
            } else {
                if let Some(dtd_valid) = CMD_ARGS.dtdvalid.as_deref() {
                    generic_error!("Could not parse DTD {}\n", dtd_valid);
                } else {
                    generic_error!(
                        "Could not parse DTD {}\n",
                        CMD_ARGS.dtdvalidfpi.as_deref().unwrap()
                    );
                }
                PROGRESULT.store(ERR_DTD, Ordering::Relaxed);
            }
        } else if CMD_ARGS.postvalid {
            let mut cvp = XmlValidCtxt::new();

            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                start_timer();
            }
            cvp.error = Some(generic_error_default);
            cvp.warning = Some(generic_error_default);
            if xml_validate_document(&mut cvp, doc) == 0 {
                let filename = filename.unwrap();
                generic_error!("Document {filename} does not validate\n");
                PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
            }
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                end_timer!("Validating");
            }
        }
        #[cfg(feature = "schematron")]
        if let Some(schematron) = WXSCHEMATRON.lock().unwrap().as_mut() {
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                start_timer();
            }

            let mut flag = if CMD_ARGS.debug {
                XmlSchematronValidOptions::XmlSchematronOutXml as i32
            } else {
                XmlSchematronValidOptions::XmlSchematronOutText as i32
            };
            if CMD_ARGS.noout {
                flag |= XmlSchematronValidOptions::XmlSchematronOutQuiet as i32;
            }
            let Some(mut ctxt) = XmlSchematronValidCtxt::new(schematron, flag) else {
                PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                xml_free_doc(doc);
                return;
            };
            match ctxt.validate_doc(doc).cmp(&0) {
                std::cmp::Ordering::Equal => {
                    if !CMD_ARGS.quiet {
                        eprintln!("{} validates", filename.unwrap());
                    }
                }
                std::cmp::Ordering::Greater => {
                    eprintln!("{} fails to validate", filename.unwrap());
                    PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
                }
                std::cmp::Ordering::Less => {
                    eprintln!(
                        "{} validation generated an internal error",
                        filename.unwrap()
                    );
                    PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
                }
            }
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                end_timer!("Validating");
            }
        }
        #[cfg(feature = "schema")]
        if !RELAXNGSCHEMAS.load(Ordering::Relaxed).is_null() {
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                start_timer();
            }

            let ctxt = xml_relaxng_new_valid_ctxt(RELAXNGSCHEMAS.load(Ordering::Relaxed));
            if ctxt.is_null() {
                PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                xml_free_doc(doc);
                return;
            }
            xml_relaxng_set_valid_errors(
                ctxt,
                Some(generic_error_default),
                Some(generic_error_default),
                None,
            );
            match xml_relaxng_validate_doc(ctxt, doc).cmp(&0) {
                std::cmp::Ordering::Equal => {
                    if !CMD_ARGS.quiet {
                        eprintln!("{} validates", filename.unwrap());
                    }
                }
                std::cmp::Ordering::Greater => {
                    eprintln!("{} fails to validate", filename.unwrap());
                    PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
                }
                std::cmp::Ordering::Less => {
                    eprintln!(
                        "{} validation generated an internal error",
                        filename.unwrap()
                    );
                    PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
                }
            }
            xml_relaxng_free_valid_ctxt(ctxt);
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                end_timer!("Validating");
            }
        } else if !WXSCHEMAS.load(Ordering::Relaxed).is_null() {
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                start_timer();
            }

            let ctxt: XmlSchemaValidCtxtPtr =
                xml_schema_new_valid_ctxt(WXSCHEMAS.load(Ordering::Relaxed));
            if ctxt.is_null() {
                PROGRESULT.store(ERR_MEM, Ordering::Relaxed);
                xml_free_doc(doc);
                return;
            }
            (*ctxt).set_errors(
                Some(generic_error_default),
                Some(generic_error_default),
                None,
            );
            match xml_schema_validate_doc(ctxt, doc).cmp(&0) {
                std::cmp::Ordering::Equal => {
                    if !CMD_ARGS.quiet {
                        eprintln!("{} validates", filename.unwrap());
                    }
                }
                std::cmp::Ordering::Greater => {
                    eprintln!("{} fails to validate", filename.unwrap());
                    PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
                }
                std::cmp::Ordering::Less => {
                    eprintln!(
                        "{} validation generated an internal error",
                        filename.unwrap()
                    );
                    PROGRESULT.store(ERR_VALID, Ordering::Relaxed);
                }
            }
            xml_schema_free_valid_ctxt(ctxt);
            if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
                end_timer!("Validating");
            }
        }

        #[cfg(all(feature = "libxml_debug", feature = "html", feature = "libxml_valid"))]
        if CMD_ARGS.debugent && !CMD_ARGS.html {
            xml_debug_dump_entities(stderr(), Some(doc));
        }

        // free it.
        if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
            start_timer();
        }
        xml_free_doc(doc);
        if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) == 0 {
            end_timer!("Freeing");
        }
    }
}

// Usage and Main

fn register_node(node: XmlGenericNodePtr) {
    let private = Box::leak(Box::new(0x81726354u64)) as *mut u64 as *mut c_void;
    NBREGISTER.fetch_add(1, Ordering::Relaxed);
    if let Ok(mut node) = XmlNodePtr::try_from(node) {
        node._private = private;
    } else if let Ok(mut node) = XmlAttrPtr::try_from(node) {
        node._private = private;
    } else if let Ok(mut node) = XmlDocPtr::try_from(node) {
        node._private = private;
    } else if let Ok(mut node) = XmlNsPtr::try_from(node) {
        node._private = private;
    } else if let Ok(mut node) = XmlEntityPtr::try_from(node) {
        node._private = private;
    } else if let Ok(mut node) = XmlDtdPtr::try_from(node) {
        node._private = private;
    } else if let Ok(mut node) = XmlAttributePtr::try_from(node) {
        node._private = private;
    } else if let Ok(mut node) = XmlElementPtr::try_from(node) {
        node._private = private;
    } else {
        panic!("Unknown Node Type");
    }
}

fn deregister_node(node: XmlGenericNodePtr) {
    unsafe {
        let private = if let Ok(node) = XmlNodePtr::try_from(node) {
            node._private
        } else if let Ok(node) = XmlAttrPtr::try_from(node) {
            node._private
        } else if let Ok(node) = XmlDocPtr::try_from(node) {
            node._private
        } else if let Ok(node) = XmlNsPtr::try_from(node) {
            node._private
        } else if let Ok(node) = XmlEntityPtr::try_from(node) {
            node._private
        } else if let Ok(node) = XmlDtdPtr::try_from(node) {
            node._private
        } else if let Ok(node) = XmlAttributePtr::try_from(node) {
            node._private
        } else if let Ok(node) = XmlElementPtr::try_from(node) {
            node._private
        } else {
            panic!("Unknown Node Type");
        };
        assert!(!private.is_null());
        assert!(*(private as *mut c_long) == 0x81726354);
        let _ = Box::from_raw(private as *mut u64);
        NBREGISTER.fetch_sub(1, Ordering::Relaxed);
    }
}

fn main() {
    let xml_files = CMD_ARGS.xml_files.clone();
    for arg in xml_files {
        if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) != 0 {
            start_timer();
        }
        unsafe {
            // Remember file names.  "-" means stdin.  <sven@zen.org>
            if !arg.starts_with('-') || arg == "-" {
                if REPEAT.load(Ordering::Relaxed) != 0 {
                    for _ in 0..REPEAT.load(Ordering::Relaxed) {
                        #[cfg(feature = "libxml_reader")]
                        {
                            if CMD_ARGS.stream {
                                stream_file(arg.as_str());
                            } else if CMD_ARGS.sax {
                                test_sax(arg.as_str());
                            } else {
                                parse_and_print_file(Some(&arg), XmlParserCtxt::new());
                            }
                        }
                        #[cfg(not(feature = "libxml_reader"))]
                        if CMD_ARGS.sax {
                            test_sax(arg.as_str());
                        } else {
                            parse_and_print_file(Some(&arg), XmlParserCtxt::new());
                        }
                    }
                } else {
                    NBREGISTER.store(0, Ordering::Relaxed);

                    match () {
                        #[cfg(feature = "libxml_reader")]
                        _ if CMD_ARGS.stream => stream_file(arg.as_str()),
                        _ if CMD_ARGS.sax => test_sax(arg.as_str()),
                        _ => parse_and_print_file(Some(&arg), None),
                    }

                    if CMD_ARGS.chkregister && NBREGISTER.load(Ordering::Relaxed) != 0 {
                        eprintln!(
                            "Registration count off: {}",
                            NBREGISTER.load(Ordering::Relaxed)
                        );
                        PROGRESULT.store(ERR_RDREGIS, Ordering::Relaxed);
                    }
                }
                if CMD_ARGS.timing && REPEAT.load(Ordering::Relaxed) != 0 {
                    end_timer!("{} iterations", REPEAT.load(Ordering::Relaxed));
                }
            }
        }
    }
    if CMD_ARGS.auto {
        unsafe {
            parse_and_print_file(None, None);
        }
    }
    if CMD_ARGS.htmlout && !CMD_ARGS.nowrap {
        generic_error!("</body></html>\n");
    }
    #[cfg(feature = "schema")]
    {
        let relaxngschemas = RELAXNGSCHEMAS.load(Ordering::Relaxed);
        if !relaxngschemas.is_null() {
            unsafe {
                xml_relaxng_free(relaxngschemas);
            }
        }
    }
    #[cfg(feature = "schema")]
    {
        let wxschemas = WXSCHEMAS.load(Ordering::Relaxed);
        if !wxschemas.is_null() {
            unsafe {
                xml_schema_free(wxschemas);
            }
        }
    }

    xml_cleanup_parser();

    exit(PROGRESULT.load(Ordering::Relaxed))
}

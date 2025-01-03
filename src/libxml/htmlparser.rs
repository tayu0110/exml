//! Provide methods and data structures for handling HTML 4.0 documents.  
//! This module is based on `libxml/HTMLparser.h`, `HTMLparser.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: interface for an HTML 4.0 non-verifying parser
// Description: this module implements an HTML 4.0 non-verifying parser
//              with API compatible with the XML parser ones. It should
//              be able to parse "real world" HTML, even if severely
//              broken from a specification point of view.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// HTMLparser.c : an HTML 4.0 non-verifying parser
//
// See Copyright for the status of this software.
//
// daniel@veillard.com

use std::{
    cell::RefCell,
    ffi::{c_char, CStr, CString},
    io::Read,
    mem::{size_of, zeroed},
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut, NonNull},
    rc::Rc,
    str::from_utf8,
    sync::atomic::{AtomicI32, Ordering},
};

use libc::{bsearch, memcpy, memset, size_t, snprintf, strcmp, strlen, INT_MAX};

use crate::{
    encoding::{detect_encoding, find_encoding_handler, XmlCharEncoding},
    error::{__xml_raise_error, parser_validity_error, parser_validity_warning, XmlParserErrors},
    globals::{get_keep_blanks_default_value, get_line_numbers_default_value, GenericErrorContext},
    io::XmlParserInputBuffer,
    libxml::{
        dict::{xml_dict_create, xml_dict_lookup},
        globals::{xml_default_sax_locator, xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
        parser::{
            xml_init_parser, xml_load_external_entity, xml_new_io_input_stream,
            xml_parser_add_node_info, XmlParserInputState, XmlParserOption, XmlSAXHandler,
            XmlSAXHandlerPtr,
        },
        parser_internals::{
            xml_free_input_stream, xml_new_input_stream, INPUT_CHUNK, XML_MAX_HUGE_LENGTH,
            XML_MAX_NAME_LENGTH, XML_MAX_TEXT_LENGTH,
        },
        sax2::{xml_sax2_ignorable_whitespace, xml_sax2_init_html_default_sax_handler},
        uri::xml_canonic_path,
        xmlstring::{
            xml_str_equal, xml_strcasestr, xml_strcmp, xml_strlen, xml_strncasecmp, xml_strndup,
            XmlChar,
        },
    },
    parser::{
        xml_free_parser_ctxt, XmlParserCtxt, XmlParserCtxtPtr, XmlParserInput, XmlParserInputPtr,
        XmlParserNodeInfo,
    },
    tree::{
        xml_create_int_subset, xml_free_doc, NodeCommon, XmlDocPtr, XmlDtdPtr, XmlElementType,
        XmlNodePtr,
    },
    uri::canonic_path,
};

use super::{
    chvalid::{
        xml_is_blank_char, xml_is_char, xml_is_combining, xml_is_digit, xml_is_extender,
        xml_is_pubid_char,
    },
    parser_internals::{xml_is_letter, XML_VCTXT_USE_PCTXT},
};

// Most of the back-end structures from XML and HTML are shared.
pub type HtmlParserCtxt = XmlParserCtxt;
pub type HtmlParserCtxtPtr = XmlParserCtxtPtr;
pub type HtmlParserNodeInfo = XmlParserNodeInfo;
pub type HtmlSAXHandler = XmlSAXHandler;
pub type HtmlSAXHandlerPtr = XmlSAXHandlerPtr;
pub type HtmlParserInput = XmlParserInput;
pub type HtmlParserInputPtr = XmlParserInputPtr;
pub type HtmlDocPtr = XmlDocPtr;
pub type HtmlNodePtr = XmlNodePtr;

pub type HtmlElemDescPtr = *mut HtmlElemDesc;
// Internal description of an HTML element, representing HTML 4.01
// and XHTML 1.0 (which share the same structure).
#[repr(C)]
pub struct HtmlElemDesc {
    pub(crate) name: &'static str,   /* The tag name */
    pub(crate) start_tag: c_char,    /* Whether the start tag can be implied */
    pub(crate) end_tag: c_char,      /* Whether the end tag can be implied */
    pub(crate) save_end_tag: c_char, /* Whether the end tag should be saved */
    pub(crate) empty: c_char,        /* Is this an empty element ? */
    pub(crate) depr: c_char,         /* Is this a deprecated element ? */
    pub(crate) dtd: c_char,          /* 1: only in Loose DTD, 2: only Frameset one */
    pub(crate) isinline: c_char,     /* is this a block 0 or inline 1 element */
    pub(crate) desc: *const c_char,  /* the description */

    // NRK Jan.2003
    // New fields encapsulating HTML structure
    //
    // Bugs:
    //     This is a very limited representation.  It fails to tell us when
    //     an element *requires* subelements (we only have whether they're
    //     allowed or not), and it doesn't tell us where CDATA and PCDATA
    //     are allowed.  Some element relationships are not fully represented:
    //     these are flagged with the word MODIFIER
    pub(crate) subelts: &'static [&'static str], /* allowed sub-elements of this element */
    pub(crate) defaultsubelt: *const c_char,     /* subelement for suggested auto-repair
                                                 if necessary or null_mut() */
    pub(crate) attrs_opt: *mut *const c_char, /* Optional Attributes */
    pub(crate) attrs_depr: *mut *const c_char, /* Additional deprecated attributes */
    pub(crate) attrs_req: *mut *const c_char, /* Required attributes */
}

pub type HtmlEntityDescPtr = *mut HtmlEntityDesc;
/// Internal description of an HTML entity.
pub struct HtmlEntityDesc {
    value: u32,          /* the UNICODE value for the character */
    name: *const c_char, /* The entity name */
    #[allow(unused)]
    desc: *const c_char, /* the description */
}

const HTML_FLOW: &[&str] = &[
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "ul",
    "ol",
    "dir",
    "menu",
    "pre",
    "p",
    "dl",
    "div",
    "center",
    "noscript",
    "noframes",
    "blockquote",
    "form",
    "isindex",
    "hr",
    "table",
    "fieldset",
    "address",
    "tt",
    "i",
    "b",
    "u",
    "s",
    "strike",
    "big",
    "small",
    "em",
    "strong",
    "dfn",
    "code",
    "samp",
    "kbd",
    "var",
    "cite",
    "abbr",
    "acronym",
    "a",
    "img",
    "applet",
    "embed",
    "object",
    "font",
    "basefont",
    "br",
    "script",
    "map",
    "q",
    "sub",
    "sup",
    "span",
    "bdo",
    "iframe",
    "input",
    "select",
    "textarea",
    "label",
    "button",
];
const HTML_INLINE: &[&str] = &[
    "tt", "i", "b", "u", "s", "strike", "big", "small", "em", "strong", "dfn", "code", "samp",
    "kbd", "var", "cite", "abbr", "acronym", "a", "img", "applet", "embed", "object", "font",
    "basefont", "br", "script", "map", "q", "sub", "sup", "span", "bdo", "iframe", "input",
    "select", "textarea", "label", "button",
];

// placeholders: elts with content but no subelements
const HTML_PCDATA: &[&str] = &[];
const HTML_CDATA: &[&str] = HTML_PCDATA;

const HTML_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    null(),
];
const CORE_I18N_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    null(),
];
const CORE_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    null(),
];
const I18N_ATTRS: &[*const c_char] = &["lang".as_ptr() as _, c"dir".as_ptr() as _, null()];

// Other declarations that should go inline ...
const A_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"charset".as_ptr() as _,
    c"type".as_ptr() as _,
    c"name".as_ptr() as _,
    c"href".as_ptr() as _,
    c"hreflang".as_ptr() as _,
    c"rel".as_ptr() as _,
    c"rev".as_ptr() as _,
    c"accesskey".as_ptr() as _,
    c"shape".as_ptr() as _,
    c"coords".as_ptr() as _,
    c"tabindex".as_ptr() as _,
    c"onfocus".as_ptr() as _,
    c"onblur".as_ptr() as _,
    null(),
];
const TARGET_ATTR: &[*const c_char] = &[c"target".as_ptr() as _, null()];
const ROWS_COLS_ATTR: &[*const c_char] = &[c"rows".as_ptr() as _, c"cols".as_ptr() as _, null()];
const ALT_ATTR: &[*const c_char] = &[c"alt".as_ptr() as _, null()];
const SRC_ALT_ATTRS: &[*const c_char] = &[c"src".as_ptr() as _, c"alt".as_ptr() as _, null()];
const HREF_ATTRS: &[*const c_char] = &[c"href".as_ptr() as _, null()];
const CLEAR_ATTRS: &[*const c_char] = &[c"clear".as_ptr() as _, null()];
const INLINE_P: &[&str] = &[
    "tt", "i", "b", "u", "s", "strike", "big", "small", "em", "strong", "dfn", "code", "samp",
    "kbd", "var", "cite", "abbr", "acronym", "a", "img", "applet", "embed", "object", "font",
    "basefont", "br", "script", "map", "q", "sub", "sup", "span", "bdo", "iframe", "input",
    "select", "textarea", "label", "button", "p",
];

const FLOW_PARAM: &[&str] = &[
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "ul",
    "ol",
    "dir",
    "menu",
    "pre",
    "p",
    "dl",
    "div",
    "center",
    "noscript",
    "noframes",
    "blockquote",
    "form",
    "isindex",
    "hr",
    "table",
    "fieldset",
    "address",
    "tt",
    "i",
    "b",
    "u",
    "s",
    "strike",
    "big",
    "small",
    "em",
    "strong",
    "dfn",
    "code",
    "samp",
    "kbd",
    "var",
    "cite",
    "abbr",
    "acronym",
    "a",
    "img",
    "applet",
    "embed",
    "object",
    "font",
    "basefont",
    "br",
    "script",
    "map",
    "q",
    "sub",
    "sup",
    "span",
    "bdo",
    "iframe",
    "input",
    "select",
    "textarea",
    "label",
    "button",
    "param",
];
const APPLET_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"codebase".as_ptr() as _,
    c"archive".as_ptr() as _,
    c"alt".as_ptr() as _,
    c"name".as_ptr() as _,
    c"height".as_ptr() as _,
    c"width".as_ptr() as _,
    c"align".as_ptr() as _,
    c"hspace".as_ptr() as _,
    c"vspace".as_ptr() as _,
    null(),
];
const AREA_ATTRS: &[*const c_char] = &[
    c"shape".as_ptr() as _,
    c"coords".as_ptr() as _,
    c"href".as_ptr() as _,
    c"nohref".as_ptr() as _,
    c"tabindex".as_ptr() as _,
    c"accesskey".as_ptr() as _,
    c"onfocus".as_ptr() as _,
    c"onblur".as_ptr() as _,
    null(),
];
const BASEFONT_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"size".as_ptr() as _,
    c"color".as_ptr() as _,
    c"face".as_ptr() as _,
    null(),
];
const QUOTE_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"cite".as_ptr() as _,
    null(),
];
const BODY_CONTENTS: &[&str] = &[
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "ul",
    "ol",
    "dir",
    "menu",
    "pre",
    "p",
    "dl",
    "div",
    "center",
    "noscript",
    "noframes",
    "blockquote",
    "form",
    "isindex",
    "hr",
    "table",
    "fieldset",
    "address",
    "tt",
    "i",
    "b",
    "u",
    "s",
    "strike",
    "big",
    "small",
    "em",
    "strong",
    "dfn",
    "code",
    "samp",
    "kbd",
    "var",
    "cite",
    "abbr",
    "acronym",
    "a",
    "img",
    "applet",
    "embed",
    "object",
    "font",
    "basefont",
    "br",
    "script",
    "map",
    "q",
    "sub",
    "sup",
    "span",
    "bdo",
    "iframe",
    "input",
    "select",
    "textarea",
    "label",
    "button",
    "ins",
    "del",
];
const BODY_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"onload".as_ptr() as _,
    c"onunload".as_ptr() as _,
    null(),
];
const BODY_DEPR: &[*const c_char] = &[
    c"background".as_ptr() as _,
    c"bgcolor".as_ptr() as _,
    c"text".as_ptr() as _,
    c"link".as_ptr() as _,
    c"vlink".as_ptr() as _,
    c"alink".as_ptr() as _,
    null(),
];
const BUTTON_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"name".as_ptr() as _,
    c"value".as_ptr() as _,
    c"type".as_ptr() as _,
    c"disabled".as_ptr() as _,
    c"tabindex".as_ptr() as _,
    c"accesskey".as_ptr() as _,
    c"onfocus".as_ptr() as _,
    c"onblur".as_ptr() as _,
    null(),
];

const COL_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"span".as_ptr() as _,
    c"width".as_ptr() as _,
    c"align".as_ptr() as _,
    c"char".as_ptr() as _,
    c"charoff".as_ptr() as _,
    c"valign".as_ptr() as _,
    null(),
];
const COL_ELT: &[&str] = &["col"];
const EDIT_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"datetime".as_ptr() as _,
    c"cite".as_ptr() as _,
    null(),
];
const COMPACT_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"compact".as_ptr() as _,
    null(),
];
const DL_CONTENTS: &[&str] = &["dt", "dd"];
const COMPACT_ATTR: &[*const c_char] = &[c"compact".as_ptr() as _, null()];
const LABEL_ATTR: &[*const c_char] = &[c"label".as_ptr() as _, null()];
const FIELDSET_CONTENTS: &[&str] = &[
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "ul",
    "ol",
    "dir",
    "menu",
    "pre",
    "p",
    "dl",
    "div",
    "center",
    "noscript",
    "noframes",
    "blockquote",
    "form",
    "isindex",
    "hr",
    "table",
    "fieldset",
    "address",
    "tt",
    "i",
    "b",
    "u",
    "s",
    "strike",
    "big",
    "small",
    "em",
    "strong",
    "dfn",
    "code",
    "samp",
    "kbd",
    "var",
    "cite",
    "abbr",
    "acronym",
    "a",
    "img",
    "applet",
    "embed",
    "object",
    "font",
    "basefont",
    "br",
    "script",
    "map",
    "q",
    "sub",
    "sup",
    "span",
    "bdo",
    "iframe",
    "input",
    "select",
    "textarea",
    "label",
    "button",
    "legend",
];
const FONT_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"size".as_ptr() as _,
    c"color".as_ptr() as _,
    c"face".as_ptr() as _,
    null(),
];
const FORM_CONTENTS: &[&str] = &[
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "ul",
    "ol",
    "dir",
    "menu",
    "tt",
    "i",
    "b",
    "u",
    "s",
    "strike",
    "big",
    "small",
    "em",
    "strong",
    "dfn",
    "code",
    "samp",
    "kbd",
    "var",
    "cite",
    "abbr",
    "acronym",
    "a",
    "img",
    "applet",
    "embed",
    "object",
    "font",
    "basefont",
    "br",
    "script",
    "map",
    "q",
    "sub",
    "sup",
    "span",
    "bdo",
    "iframe",
    "input",
    "select",
    "textarea",
    "label",
    "button",
    "pre",
    "p",
    "div",
    "center",
    "noscript",
    "noframes",
    "blockquote",
    "isindex",
    "hr",
    "table",
    "fieldset",
    "address",
];
const FORM_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"method".as_ptr() as _,
    c"enctype".as_ptr() as _,
    c"accept".as_ptr() as _,
    c"name".as_ptr() as _,
    c"onsubmit".as_ptr() as _,
    c"onreset".as_ptr() as _,
    c"accept-charset".as_ptr() as _,
    null(),
];
const FRAME_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"longdesc".as_ptr() as _,
    c"name".as_ptr() as _,
    c"src".as_ptr() as _,
    c"frameborder".as_ptr() as _,
    c"marginwidth".as_ptr() as _,
    c"marginheight".as_ptr() as _,
    c"noresize".as_ptr() as _,
    c"scrolling".as_ptr() as _,
    null(),
];
const FRAMESET_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"rows".as_ptr() as _,
    c"cols".as_ptr() as _,
    c"onload".as_ptr() as _,
    c"onunload".as_ptr() as _,
    null(),
];
const FRAMESET_CONTENTS: &[&str] = &["frameset", "frame", "noframes"];
const HEAD_ATTRS: &[*const c_char] = &[
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"profile".as_ptr() as _,
    null(),
];
const HEAD_CONTENTS: &[&str] = &[
    "title", "isindex", "base", "script", "style", "meta", "link", "object",
];
const HR_DEPR: &[*const c_char] = &[
    c"align".as_ptr() as _,
    c"noshade".as_ptr() as _,
    c"size".as_ptr() as _,
    c"width".as_ptr() as _,
    null(),
];
const VERSION_ATTR: &[*const c_char] = &[c"version".as_ptr() as _, null()];
const HTML_CONTENT: &[&str] = &["head", "body", "frameset"];
const IFRAME_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"longdesc".as_ptr() as _,
    c"name".as_ptr() as _,
    c"src".as_ptr() as _,
    c"frameborder".as_ptr() as _,
    c"marginwidth".as_ptr() as _,
    c"marginheight".as_ptr() as _,
    c"scrolling".as_ptr() as _,
    c"align".as_ptr() as _,
    c"height".as_ptr() as _,
    c"width".as_ptr() as _,
    null(),
];
const IMG_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"longdesc".as_ptr() as _,
    c"name".as_ptr() as _,
    c"height".as_ptr() as _,
    c"width".as_ptr() as _,
    c"usemap".as_ptr() as _,
    c"ismap".as_ptr() as _,
    null(),
];
const EMBED_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"align".as_ptr() as _,
    c"alt".as_ptr() as _,
    c"border".as_ptr() as _,
    c"code".as_ptr() as _,
    c"codebase".as_ptr() as _,
    c"frameborder".as_ptr() as _,
    c"height".as_ptr() as _,
    c"hidden".as_ptr() as _,
    c"hspace".as_ptr() as _,
    c"name".as_ptr() as _,
    c"palette".as_ptr() as _,
    c"pluginspace".as_ptr() as _,
    c"pluginurl".as_ptr() as _,
    c"src".as_ptr() as _,
    c"type".as_ptr() as _,
    c"units".as_ptr() as _,
    c"vspace".as_ptr() as _,
    c"width".as_ptr() as _,
    null(),
];
const INPUT_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"type".as_ptr() as _,
    c"name".as_ptr() as _,
    c"value".as_ptr() as _,
    c"checked".as_ptr() as _,
    c"disabled".as_ptr() as _,
    c"readonly".as_ptr() as _,
    c"size".as_ptr() as _,
    c"maxlength".as_ptr() as _,
    c"src".as_ptr() as _,
    c"alt".as_ptr() as _,
    c"usemap".as_ptr() as _,
    c"ismap".as_ptr() as _,
    c"tabindex".as_ptr() as _,
    c"accesskey".as_ptr() as _,
    c"onfocus".as_ptr() as _,
    c"onblur".as_ptr() as _,
    c"onselect".as_ptr() as _,
    c"onchange".as_ptr() as _,
    c"accept".as_ptr() as _,
    null(),
];
const PROMPT_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"prompt".as_ptr() as _,
    null(),
];
const LABEL_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"for".as_ptr() as _,
    c"accesskey".as_ptr() as _,
    c"onfocus".as_ptr() as _,
    c"onblur".as_ptr() as _,
    null(),
];
const LEGEND_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"accesskey".as_ptr() as _,
    null(),
];
const ALIGN_ATTR: &[*const c_char] = &[c"align".as_ptr() as _, null()];
const LINK_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"charset".as_ptr() as _,
    c"href".as_ptr() as _,
    c"hreflang".as_ptr() as _,
    c"type".as_ptr() as _,
    c"rel".as_ptr() as _,
    c"rev".as_ptr() as _,
    c"media".as_ptr() as _,
    null(),
];
const MAP_CONTENTS: &[&str] = &[
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "ul",
    "ol",
    "dir",
    "menu",
    "pre",
    "p",
    "dl",
    "div",
    "center",
    "noscript",
    "noframes",
    "blockquote",
    "form",
    "isindex",
    "hr",
    "table",
    "fieldset",
    "address",
    "area",
];
const NAME_ATTR: &[*const c_char] = &[c"name".as_ptr() as _, null()];
const ACTION_ATTR: &[*const c_char] = &[c"action".as_ptr() as _, null()];
const BLOCKLI_ELT: &[&str] = &[
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "ul",
    "ol",
    "dir",
    "menu",
    "pre",
    "p",
    "dl",
    "div",
    "center",
    "noscript",
    "noframes",
    "blockquote",
    "form",
    "isindex",
    "hr",
    "table",
    "fieldset",
    "address",
    "li",
];
const META_ATTRS: &[*const c_char] = &[
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"http-equiv".as_ptr() as _,
    c"name".as_ptr() as _,
    c"scheme".as_ptr() as _,
    c"charset".as_ptr() as _,
    null(),
];
const CONTENT_ATTR: &[*const c_char] = &[c"content".as_ptr() as _, null()];
const TYPE_ATTR: &[*const c_char] = &[c"type".as_ptr() as _, null()];
const NOFRAMES_CONTENT: &[&str] = &[
    "body",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "ul",
    "ol",
    "dir",
    "menu",
    "pre",
    "p",
    "dl",
    "div",
    "center",
    "noscript",
    "noframes",
    "blockquote",
    "form",
    "isindex",
    "hr",
    "table",
    "fieldset",
    "address",
    "tt",
    "i",
    "b",
    "u",
    "s",
    "strike",
    "big",
    "small",
    "em",
    "strong",
    "dfn",
    "code",
    "samp",
    "kbd",
    "var",
    "cite",
    "abbr",
    "acronym",
    "a",
    "img",
    "applet",
    "embed",
    "object",
    "font",
    "basefont",
    "br",
    "script",
    "map",
    "q",
    "sub",
    "sup",
    "span",
    "bdo",
    "iframe",
    "input",
    "select",
    "textarea",
    "label",
    "button",
];
const OBJECT_CONTENTS: &[&str] = &[
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "ul",
    "ol",
    "dir",
    "menu",
    "pre",
    "p",
    "dl",
    "div",
    "center",
    "noscript",
    "noframes",
    "blockquote",
    "form",
    "isindex",
    "hr",
    "table",
    "fieldset",
    "address",
    "tt",
    "i",
    "b",
    "u",
    "s",
    "strike",
    "big",
    "small",
    "em",
    "strong",
    "dfn",
    "code",
    "samp",
    "kbd",
    "var",
    "cite",
    "abbr",
    "acronym",
    "a",
    "img",
    "applet",
    "embed",
    "object",
    "font",
    "basefont",
    "br",
    "script",
    "map",
    "q",
    "sub",
    "sup",
    "span",
    "bdo",
    "iframe",
    "input",
    "select",
    "textarea",
    "label",
    "button",
    "param",
];
const OBJECT_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"declare".as_ptr() as _,
    c"classid".as_ptr() as _,
    c"codebase".as_ptr() as _,
    c"data".as_ptr() as _,
    c"type".as_ptr() as _,
    c"codetype".as_ptr() as _,
    c"archive".as_ptr() as _,
    c"standby".as_ptr() as _,
    c"height".as_ptr() as _,
    c"width".as_ptr() as _,
    c"usemap".as_ptr() as _,
    c"name".as_ptr() as _,
    c"tabindex".as_ptr() as _,
    null(),
];
const OBJECT_DEPR: &[*const c_char] = &[
    c"align".as_ptr() as _,
    c"border".as_ptr() as _,
    c"hspace".as_ptr() as _,
    c"vspace".as_ptr() as _,
    null(),
];
const OL_ATTRS: &[*const c_char] = &[
    c"type".as_ptr() as _,
    c"compact".as_ptr() as _,
    c"start".as_ptr() as _,
    null(),
];
const OPTION_ELT: &[&str] = &["option"];
const OPTGROUP_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"disabled".as_ptr() as _,
    null(),
];
const OPTION_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"disabled".as_ptr() as _,
    c"label".as_ptr() as _,
    c"selected".as_ptr() as _,
    c"value".as_ptr() as _,
    null(),
];
const PARAM_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"value".as_ptr() as _,
    c"valuetype".as_ptr() as _,
    c"type".as_ptr() as _,
    null(),
];
const WIDTH_ATTR: &[*const c_char] = &[c"width".as_ptr() as _, null()];
const PRE_CONTENT: &[&str] = &[
    "em", "strong", "dfn", "code", "samp", "kbd", "var", "cite", "abbr", "acronym", "tt", "i", "b",
    "u", "s", "strike", "a", "br", "script", "map", "q", "span", "bdo", "iframe",
];
const SCRIPT_ATTRS: &[*const c_char] = &[
    c"charset".as_ptr() as _,
    c"src".as_ptr() as _,
    c"defer".as_ptr() as _,
    c"event".as_ptr() as _,
    c"for".as_ptr() as _,
    null(),
];
const LANGUAGE_ATTR: &[*const c_char] = &[c"language".as_ptr() as _, null()];
const SELECT_CONTENT: &[&str] = &["optgroup", "option"];
const SELECT_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"name".as_ptr() as _,
    c"size".as_ptr() as _,
    c"multiple".as_ptr() as _,
    c"disabled".as_ptr() as _,
    c"tabindex".as_ptr() as _,
    c"onfocus".as_ptr() as _,
    c"onblur".as_ptr() as _,
    c"onchange".as_ptr() as _,
    null(),
];
const STYLE_ATTRS: &[*const c_char] = &[
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"media".as_ptr() as _,
    c"title".as_ptr() as _,
    null(),
];
const TABLE_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"summary".as_ptr() as _,
    c"width".as_ptr() as _,
    c"border".as_ptr() as _,
    c"frame".as_ptr() as _,
    c"rules".as_ptr() as _,
    c"cellspacing".as_ptr() as _,
    c"cellpadding".as_ptr() as _,
    c"datapagesize".as_ptr() as _,
    null(),
];
const TABLE_DEPR: &[*const c_char] = &[c"align".as_ptr() as _, c"bgcolor".as_ptr() as _, null()];
const TABLE_CONTENTS: &[&str] = &[
    "caption", "col", "colgroup", "thead", "tfoot", "tbody", "tr",
];
const TR_ELT: &[&str] = &["tr"];
const TALIGN_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"align".as_ptr() as _,
    c"char".as_ptr() as _,
    c"charoff".as_ptr() as _,
    c"valign".as_ptr() as _,
    null(),
];
const TH_TD_DEPR: &[*const c_char] = &[
    c"nowrap".as_ptr() as _,
    c"bgcolor".as_ptr() as _,
    c"width".as_ptr() as _,
    c"height".as_ptr() as _,
    null(),
];
const TH_TD_ATTR: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"abbr".as_ptr() as _,
    c"axis".as_ptr() as _,
    c"headers".as_ptr() as _,
    c"scope".as_ptr() as _,
    c"rowspan".as_ptr() as _,
    c"colspan".as_ptr() as _,
    c"align".as_ptr() as _,
    c"char".as_ptr() as _,
    c"charoff".as_ptr() as _,
    c"valign".as_ptr() as _,
    null(),
];
const TEXTAREA_ATTRS: &[*const c_char] = &[
    c"id".as_ptr() as _,
    c"class".as_ptr() as _,
    c"style".as_ptr() as _,
    c"title".as_ptr() as _,
    c"lang".as_ptr() as _,
    c"dir".as_ptr() as _,
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"name".as_ptr() as _,
    c"disabled".as_ptr() as _,
    c"readonly".as_ptr() as _,
    c"tabindex".as_ptr() as _,
    c"accesskey".as_ptr() as _,
    c"onfocus".as_ptr() as _,
    c"onblur".as_ptr() as _,
    c"onselect".as_ptr() as _,
    c"onchange".as_ptr() as _,
    null(),
];
const TR_CONTENTS: &[&str] = &["th", "td"];
const BGCOLOR_ATTR: &[*const c_char] = &[c"bgcolor".as_ptr() as _, null()];
const LI_ELT: &[&str] = &["li"];
const UL_DEPR: &[*const c_char] = &[c"type".as_ptr() as _, c"compact".as_ptr() as _, null()];
const DIR_ATTR: &[*const c_char] = &[c"dir".as_ptr() as _, null()];

const HTML40_ELEMENT_TABLE: &[HtmlElemDesc] = &[
    HtmlElemDesc {
        name: "a",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"anchor ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: A_ATTRS.as_ptr() as _,
        attrs_depr: TARGET_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "abbr",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"abbreviated form".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "acronym",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "address",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"information on author ".as_ptr() as _,
        subelts: INLINE_P,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "applet",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 1,
        dtd: 1,
        isinline: 2,
        desc: c"java applet ".as_ptr() as _,
        subelts: FLOW_PARAM,
        defaultsubelt: null_mut(),
        attrs_opt: null_mut(),
        attrs_depr: APPLET_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "area",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"client-side image map area ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: AREA_ATTRS.as_ptr() as _,
        attrs_depr: TARGET_ATTR.as_ptr() as _,
        attrs_req: ALT_ATTR.as_ptr() as _,
    },
    HtmlElemDesc {
        name: "b",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"bold text style".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "base",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"document base uri ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: null_mut(),
        attrs_depr: TARGET_ATTR.as_ptr() as _,
        attrs_req: HREF_ATTRS.as_ptr() as _,
    },
    HtmlElemDesc {
        name: "basefont",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 1,
        dtd: 1,
        isinline: 1,
        desc: c"base font size ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: null_mut(),
        attrs_depr: BASEFONT_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "bdo",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"i18n bidi over-ride ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: CORE_I18N_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: DIR_ATTR.as_ptr() as _,
    },
    HtmlElemDesc {
        name: "big",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"large text style".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "blockquote",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"long quotation ".as_ptr() as _,
        subelts: HTML_FLOW,
        defaultsubelt: null_mut(),
        attrs_opt: QUOTE_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "body",
        start_tag: 1,
        end_tag: 1,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"document body ".as_ptr() as _,
        subelts: BODY_CONTENTS,
        defaultsubelt: c"div".as_ptr() as _,
        attrs_opt: BODY_ATTRS.as_ptr() as _,
        attrs_depr: BODY_DEPR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "br",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"forced line break ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: CORE_ATTRS.as_ptr() as _,
        attrs_depr: CLEAR_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "button",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 2,
        desc: c"push button ".as_ptr() as _,
        subelts: HTML_FLOW,
        defaultsubelt: null_mut(),
        attrs_opt: BUTTON_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "caption",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"table caption ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "center",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 1,
        dtd: 1,
        isinline: 0,
        desc: c"shorthand for div align=center ".as_ptr() as _,
        subelts: HTML_FLOW,
        defaultsubelt: null_mut(),
        attrs_opt: null_mut(),
        attrs_depr: HTML_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "cite",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"citation".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "code",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"computer code fragment".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "col",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"table column ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: COL_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "colgroup",
        start_tag: 0,
        end_tag: 1,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"table column group ".as_ptr() as _,
        subelts: COL_ELT,
        defaultsubelt: c"col".as_ptr() as _,
        attrs_opt: COL_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "dd",
        start_tag: 0,
        end_tag: 1,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"definition description ".as_ptr() as _,
        subelts: HTML_FLOW,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "del",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 2,
        desc: c"deleted text ".as_ptr() as _,
        subelts: HTML_FLOW,
        defaultsubelt: null_mut(),
        attrs_opt: EDIT_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "dfn",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"instance definition".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "dir",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 1,
        dtd: 1,
        isinline: 0,
        desc: c"directory list".as_ptr() as _,
        subelts: BLOCKLI_ELT,
        defaultsubelt: c"li".as_ptr() as _,
        attrs_opt: null_mut(),
        attrs_depr: COMPACT_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "div",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"generic language/style container".as_ptr() as _,
        subelts: HTML_FLOW,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: ALIGN_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "dl",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"definition list ".as_ptr() as _,
        subelts: DL_CONTENTS,
        defaultsubelt: c"dd".as_ptr() as _,
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: COMPACT_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "dt",
        start_tag: 0,
        end_tag: 1,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"definition term ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "em",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"emphasis".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "embed",
        start_tag: 0,
        end_tag: 1,
        save_end_tag: 0,
        empty: 0,
        depr: 1,
        dtd: 1,
        isinline: 1,
        desc: c"generic embedded object ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: EMBED_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "fieldset",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"form control group ".as_ptr() as _,
        subelts: FIELDSET_CONTENTS,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "font",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 1,
        dtd: 1,
        isinline: 1,
        desc: c"local change to font ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: null_mut(),
        attrs_depr: FONT_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "form",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"interactive form ".as_ptr() as _,
        subelts: FORM_CONTENTS,
        defaultsubelt: c"fieldset".as_ptr() as _,
        attrs_opt: FORM_ATTRS.as_ptr() as _,
        attrs_depr: TARGET_ATTR.as_ptr() as _,
        attrs_req: ACTION_ATTR.as_ptr() as _,
    },
    HtmlElemDesc {
        name: "frame",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 0,
        dtd: 2,
        isinline: 0,
        desc: c"subwindow ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: null_mut(),
        attrs_depr: FRAME_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "frameset",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 2,
        isinline: 0,
        desc: c"window subdivision".as_ptr() as _,
        subelts: FRAMESET_CONTENTS,
        defaultsubelt: c"noframes".as_ptr() as _,
        attrs_opt: null_mut(),
        attrs_depr: FRAMESET_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "h1",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"heading ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: ALIGN_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "h2",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"heading ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: ALIGN_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "h3",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"heading ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: ALIGN_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "h4",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"heading ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: ALIGN_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "h5",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"heading ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: ALIGN_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "h6",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"heading ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: ALIGN_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "head",
        start_tag: 1,
        end_tag: 1,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"document head ".as_ptr() as _,
        subelts: HEAD_CONTENTS,
        defaultsubelt: null_mut(),
        attrs_opt: HEAD_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "hr",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"horizontal rule ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: HR_DEPR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "html",
        start_tag: 1,
        end_tag: 1,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"document root element ".as_ptr() as _,
        subelts: HTML_CONTENT,
        defaultsubelt: null_mut(),
        attrs_opt: I18N_ATTRS.as_ptr() as _,
        attrs_depr: VERSION_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "i",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"italic text style".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "iframe",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 1,
        isinline: 2,
        desc: c"inline subwindow ".as_ptr() as _,
        subelts: HTML_FLOW,
        defaultsubelt: null_mut(),
        attrs_opt: null_mut(),
        attrs_depr: IFRAME_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "img",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"embedded image ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: IMG_ATTRS.as_ptr() as _,
        attrs_depr: ALIGN_ATTR.as_ptr() as _,
        attrs_req: SRC_ALT_ATTRS.as_ptr() as _,
    },
    HtmlElemDesc {
        name: "input",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"form control ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: INPUT_ATTRS.as_ptr() as _,
        attrs_depr: ALIGN_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "ins",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 2,
        desc: c"inserted text".as_ptr() as _,
        subelts: HTML_FLOW,
        defaultsubelt: null_mut(),
        attrs_opt: EDIT_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "isindex",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 1,
        dtd: 1,
        isinline: 0,
        desc: c"single line prompt ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: null_mut(),
        attrs_depr: PROMPT_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "kbd",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"text to be entered by the user".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "label",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"form field label text ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: LABEL_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "legend",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"fieldset legend ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: LEGEND_ATTRS.as_ptr() as _,
        attrs_depr: ALIGN_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "li",
        start_tag: 0,
        end_tag: 1,
        save_end_tag: 1,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"list item ".as_ptr() as _,
        subelts: HTML_FLOW,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "link",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"a media-independent link ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: LINK_ATTRS.as_ptr() as _,
        attrs_depr: TARGET_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "map",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 2,
        desc: c"client-side image map ".as_ptr() as _,
        subelts: MAP_CONTENTS,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: NAME_ATTR.as_ptr() as _,
    },
    HtmlElemDesc {
        name: "menu",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 1,
        dtd: 1,
        isinline: 0,
        desc: c"menu list ".as_ptr() as _,
        subelts: BLOCKLI_ELT,
        defaultsubelt: null_mut(),
        attrs_opt: null_mut(),
        attrs_depr: COMPACT_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "meta",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"generic metainformation ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: META_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: CONTENT_ATTR.as_ptr() as _,
    },
    HtmlElemDesc {
        name: "noframes",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 2,
        isinline: 0,
        desc: c"alternate content container for non frame-based rendering ".as_ptr() as _,
        subelts: NOFRAMES_CONTENT,
        defaultsubelt: c"body".as_ptr() as _,
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "noscript",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"alternate content container for non script-based rendering ".as_ptr() as _,
        subelts: HTML_FLOW,
        defaultsubelt: c"div".as_ptr() as _,
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "object",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 2,
        desc: c"generic embedded object ".as_ptr() as _,
        subelts: OBJECT_CONTENTS,
        defaultsubelt: c"div".as_ptr() as _,
        attrs_opt: OBJECT_ATTRS.as_ptr() as _,
        attrs_depr: OBJECT_DEPR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "ol",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"ordered list ".as_ptr() as _,
        subelts: LI_ELT,
        defaultsubelt: c"li".as_ptr() as _,
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: OL_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "optgroup",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"option group ".as_ptr() as _,
        subelts: OPTION_ELT,
        defaultsubelt: c"option".as_ptr() as _,
        attrs_opt: OPTGROUP_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: LABEL_ATTR.as_ptr() as _,
    },
    HtmlElemDesc {
        name: "option",
        start_tag: 0,
        end_tag: 1,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"selectable choice ".as_ptr() as _,
        subelts: HTML_PCDATA,
        defaultsubelt: null_mut(),
        attrs_opt: OPTION_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "p",
        start_tag: 0,
        end_tag: 1,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"paragraph ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: ALIGN_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "param",
        start_tag: 0,
        end_tag: 2,
        save_end_tag: 2,
        empty: 1,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"named property value ".as_ptr() as _,
        subelts: &[],
        defaultsubelt: null_mut(),
        attrs_opt: PARAM_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: NAME_ATTR.as_ptr() as _,
    },
    HtmlElemDesc {
        name: "pre",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"preformatted text ".as_ptr() as _,
        subelts: PRE_CONTENT,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: WIDTH_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "q",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"short inline quotation ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: QUOTE_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "s",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 1,
        dtd: 1,
        isinline: 1,
        desc: c"strike-through text style".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: null_mut(),
        attrs_depr: HTML_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "samp",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"sample program output, scripts, etc.".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "script",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 2,
        desc: c"script statements ".as_ptr() as _,
        subelts: HTML_CDATA,
        defaultsubelt: null_mut(),
        attrs_opt: SCRIPT_ATTRS.as_ptr() as _,
        attrs_depr: LANGUAGE_ATTR.as_ptr() as _,
        attrs_req: TYPE_ATTR.as_ptr() as _,
    },
    HtmlElemDesc {
        name: "select",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"option selector ".as_ptr() as _,
        subelts: SELECT_CONTENT,
        defaultsubelt: null_mut(),
        attrs_opt: SELECT_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "small",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"small text style".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "span",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"generic language/style container ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "strike",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 1,
        dtd: 1,
        isinline: 1,
        desc: c"strike-through text".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: null_mut(),
        attrs_depr: HTML_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "strong",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"strong emphasis".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "style",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"style info ".as_ptr() as _,
        subelts: HTML_CDATA,
        defaultsubelt: null_mut(),
        attrs_opt: STYLE_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: TYPE_ATTR.as_ptr() as _,
    },
    HtmlElemDesc {
        name: "sub",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"subscript".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "sup",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"superscript ".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "table",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"".as_ptr() as _,
        subelts: TABLE_CONTENTS,
        defaultsubelt: c"tr".as_ptr() as _,
        attrs_opt: TABLE_ATTRS.as_ptr() as _,
        attrs_depr: TABLE_DEPR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "tbody",
        start_tag: 1,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"table body ".as_ptr() as _,
        subelts: TR_ELT,
        defaultsubelt: c"tr".as_ptr() as _,
        attrs_opt: TALIGN_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "td",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"table data cell".as_ptr() as _,
        subelts: HTML_FLOW,
        defaultsubelt: null_mut(),
        attrs_opt: TH_TD_ATTR.as_ptr() as _,
        attrs_depr: TH_TD_DEPR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "textarea",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"multi-line text field ".as_ptr() as _,
        subelts: HTML_PCDATA,
        defaultsubelt: null_mut(),
        attrs_opt: TEXTAREA_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: ROWS_COLS_ATTR.as_ptr() as _,
    },
    HtmlElemDesc {
        name: "tfoot",
        start_tag: 0,
        end_tag: 1,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"table footer ".as_ptr() as _,
        subelts: TR_ELT,
        defaultsubelt: c"tr".as_ptr() as _,
        attrs_opt: TALIGN_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "th",
        start_tag: 0,
        end_tag: 1,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"table header cell".as_ptr() as _,
        subelts: HTML_FLOW,
        defaultsubelt: null_mut(),
        attrs_opt: TH_TD_ATTR.as_ptr() as _,
        attrs_depr: TH_TD_DEPR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "thead",
        start_tag: 0,
        end_tag: 1,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"table header ".as_ptr() as _,
        subelts: TR_ELT,
        defaultsubelt: c"tr".as_ptr() as _,
        attrs_opt: TALIGN_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "title",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"document title ".as_ptr() as _,
        subelts: HTML_PCDATA,
        defaultsubelt: null_mut(),
        attrs_opt: I18N_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "tr",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"table row ".as_ptr() as _,
        subelts: TR_CONTENTS,
        defaultsubelt: c"td".as_ptr() as _,
        attrs_opt: TALIGN_ATTRS.as_ptr() as _,
        attrs_depr: BGCOLOR_ATTR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "tt",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"teletype or monospaced text style".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "u",
        start_tag: 0,
        end_tag: 3,
        save_end_tag: 0,
        empty: 0,
        depr: 1,
        dtd: 1,
        isinline: 1,
        desc: c"underlined text style".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: null_mut(),
        attrs_depr: HTML_ATTRS.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "ul",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 0,
        desc: c"unordered list ".as_ptr() as _,
        subelts: LI_ELT,
        defaultsubelt: c"li".as_ptr() as _,
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: UL_DEPR.as_ptr() as _,
        attrs_req: null_mut(),
    },
    HtmlElemDesc {
        name: "var",
        start_tag: 0,
        end_tag: 0,
        save_end_tag: 0,
        empty: 0,
        depr: 0,
        dtd: 0,
        isinline: 1,
        desc: c"instance of a variable or program argument".as_ptr() as _,
        subelts: HTML_INLINE,
        defaultsubelt: null_mut(),
        attrs_opt: HTML_ATTRS.as_ptr() as _,
        attrs_depr: null_mut(),
        attrs_req: null_mut(),
    },
];

const HTML40_ENTITIES_TABLE: &[HtmlEntityDesc] = &[
    /*
     * the 4 absolute ones, plus apostrophe.
     */
    HtmlEntityDesc {
        value: 34,
        name: c"quot".as_ptr() as _,
        desc: c"quotation mark = APL quote, U+0022 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 38,
        name: c"amp".as_ptr() as _,
        desc: c"ampersand, U+0026 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 39,
        name: c"apos".as_ptr() as _,
        desc: c"single quote".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 60,
        name: c"lt".as_ptr() as _,
        desc: c"less-than sign, U+003C ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 62,
        name: c"gt".as_ptr() as _,
        desc: c"greater-than sign, U+003E ISOnum".as_ptr() as _,
    },
    /*
     * A bunch still in the 128-255 range
     * Replacing them depend really on the charset used.
     */
    HtmlEntityDesc {
        value: 160,
        name: c"nbsp".as_ptr() as _,
        desc: c"no-break space = non-breaking space, U+00A0 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 161,
        name: c"iexcl".as_ptr() as _,
        desc: c"inverted exclamation mark, U+00A1 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 162,
        name: c"cent".as_ptr() as _,
        desc: c"cent sign, U+00A2 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 163,
        name: c"pound".as_ptr() as _,
        desc: c"pound sign, U+00A3 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 164,
        name: c"curren".as_ptr() as _,
        desc: c"currency sign, U+00A4 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 165,
        name: c"yen".as_ptr() as _,
        desc: c"yen sign = yuan sign, U+00A5 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 166,
        name: c"brvbar".as_ptr() as _,
        desc: c"broken bar = broken vertical bar, U+00A6 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 167,
        name: c"sect".as_ptr() as _,
        desc: c"section sign, U+00A7 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 168,
        name: c"uml".as_ptr() as _,
        desc: c"diaeresis = spacing diaeresis, U+00A8 ISOdia".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 169,
        name: c"copy".as_ptr() as _,
        desc: c"copyright sign, U+00A9 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 170,
        name: c"ordf".as_ptr() as _,
        desc: c"feminine ordinal indicator, U+00AA ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 171,
        name: c"laquo".as_ptr() as _,
        desc: c"left-pointing double angle quotation mark = left pointing guillemet, U+00AB ISOnum"
            .as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 172,
        name: c"not".as_ptr() as _,
        desc: c"not sign, U+00AC ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 173,
        name: c"shy".as_ptr() as _,
        desc: c"soft hyphen = discretionary hyphen, U+00AD ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 174,
        name: c"reg".as_ptr() as _,
        desc: c"registered sign = registered trade mark sign, U+00AE ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 175,
        name: c"macr".as_ptr() as _,
        desc: c"macron = spacing macron = overline = APL overbar, U+00AF ISOdia".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 176,
        name: c"deg".as_ptr() as _,
        desc: c"degree sign, U+00B0 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 177,
        name: c"plusmn".as_ptr() as _,
        desc: c"plus-minus sign = plus-or-minus sign, U+00B1 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 178,
        name: c"sup2".as_ptr() as _,
        desc: c"superscript two = superscript digit two = squared, U+00B2 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 179,
        name: c"sup3".as_ptr() as _,
        desc: c"superscript three = superscript digit three = cubed, U+00B3 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 180,
        name: c"acute".as_ptr() as _,
        desc: c"acute accent = spacing acute, U+00B4 ISOdia".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 181,
        name: c"micro".as_ptr() as _,
        desc: c"micro sign, U+00B5 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 182,
        name: c"para".as_ptr() as _,
        desc: c"pilcrow sign = paragraph sign, U+00B6 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 183,
        name: c"middot".as_ptr() as _,
        desc: c"middle dot = Georgian comma Greek middle dot, U+00B7 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 184,
        name: c"cedil".as_ptr() as _,
        desc: c"cedilla = spacing cedilla, U+00B8 ISOdia".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 185,
        name: c"sup1".as_ptr() as _,
        desc: c"superscript one = superscript digit one, U+00B9 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 186,
        name: c"ordm".as_ptr() as _,
        desc: c"masculine ordinal indicator, U+00BA ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 187,
        name: c"raquo".as_ptr() as _,
        desc: c"right-pointing double angle quotation mark right pointing guillemet, U+00BB ISOnum"
            .as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 188,
        name: c"frac14".as_ptr() as _,
        desc: c"vulgar fraction one quarter = fraction one quarter, U+00BC ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 189,
        name: c"frac12".as_ptr() as _,
        desc: c"vulgar fraction one half = fraction one half, U+00BD ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 190,
        name: c"frac34".as_ptr() as _,
        desc: c"vulgar fraction three quarters = fraction three quarters, U+00BE ISOnum".as_ptr()
            as _,
    },
    HtmlEntityDesc {
        value: 191,
        name: c"iquest".as_ptr() as _,
        desc: c"inverted question mark = turned question mark, U+00BF ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 192,
        name: c"Agrave".as_ptr() as _,
        desc: c"latin capital letter A with grave = latin capital letter A grave, U+00C0 ISOlat1"
            .as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 193,
        name: c"Aacute".as_ptr() as _,
        desc: c"latin capital letter A with acute, U+00C1 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 194,
        name: c"Acirc".as_ptr() as _,
        desc: c"latin capital letter A with circumflex, U+00C2 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 195,
        name: c"Atilde".as_ptr() as _,
        desc: c"latin capital letter A with tilde, U+00C3 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 196,
        name: c"Auml".as_ptr() as _,
        desc: c"latin capital letter A with diaeresis, U+00C4 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 197,
        name: c"Aring".as_ptr() as _,
        desc:
            c"latin capital letter A with ring above = latin capital letter A ring, U+00C5 ISOlat1"
                .as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 198,
        name: c"AElig".as_ptr() as _,
        desc: c"latin capital letter AE = latin capital ligature AE, U+00C6 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 199,
        name: c"Ccedil".as_ptr() as _,
        desc: c"latin capital letter C with cedilla, U+00C7 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 200,
        name: c"Egrave".as_ptr() as _,
        desc: c"latin capital letter E with grave, U+00C8 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 201,
        name: c"Eacute".as_ptr() as _,
        desc: c"latin capital letter E with acute, U+00C9 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 202,
        name: c"Ecirc".as_ptr() as _,
        desc: c"latin capital letter E with circumflex, U+00CA ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 203,
        name: c"Euml".as_ptr() as _,
        desc: c"latin capital letter E with diaeresis, U+00CB ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 204,
        name: c"Igrave".as_ptr() as _,
        desc: c"latin capital letter I with grave, U+00CC ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 205,
        name: c"Iacute".as_ptr() as _,
        desc: c"latin capital letter I with acute, U+00CD ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 206,
        name: c"Icirc".as_ptr() as _,
        desc: c"latin capital letter I with circumflex, U+00CE ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 207,
        name: c"Iuml".as_ptr() as _,
        desc: c"latin capital letter I with diaeresis, U+00CF ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 208,
        name: c"ETH".as_ptr() as _,
        desc: c"latin capital letter ETH, U+00D0 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 209,
        name: c"Ntilde".as_ptr() as _,
        desc: c"latin capital letter N with tilde, U+00D1 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 210,
        name: c"Ograve".as_ptr() as _,
        desc: c"latin capital letter O with grave, U+00D2 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 211,
        name: c"Oacute".as_ptr() as _,
        desc: c"latin capital letter O with acute, U+00D3 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 212,
        name: c"Ocirc".as_ptr() as _,
        desc: c"latin capital letter O with circumflex, U+00D4 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 213,
        name: c"Otilde".as_ptr() as _,
        desc: c"latin capital letter O with tilde, U+00D5 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 214,
        name: c"Ouml".as_ptr() as _,
        desc: c"latin capital letter O with diaeresis, U+00D6 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 215,
        name: c"times".as_ptr() as _,
        desc: c"multiplication sign, U+00D7 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 216,
        name: c"Oslash".as_ptr() as _,
        desc: c"latin capital letter O with stroke latin capital letter O slash, U+00D8 ISOlat1"
            .as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 217,
        name: c"Ugrave".as_ptr() as _,
        desc: c"latin capital letter U with grave, U+00D9 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 218,
        name: c"Uacute".as_ptr() as _,
        desc: c"latin capital letter U with acute, U+00DA ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 219,
        name: c"Ucirc".as_ptr() as _,
        desc: c"latin capital letter U with circumflex, U+00DB ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 220,
        name: c"Uuml".as_ptr() as _,
        desc: c"latin capital letter U with diaeresis, U+00DC ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 221,
        name: c"Yacute".as_ptr() as _,
        desc: c"latin capital letter Y with acute, U+00DD ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 222,
        name: c"THORN".as_ptr() as _,
        desc: c"latin capital letter THORN, U+00DE ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 223,
        name: c"szlig".as_ptr() as _,
        desc: c"latin small letter sharp s = ess-zed, U+00DF ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 224,
        name: c"agrave".as_ptr() as _,
        desc: c"latin small letter a with grave = latin small letter a grave, U+00E0 ISOlat1"
            .as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 225,
        name: c"aacute".as_ptr() as _,
        desc: c"latin small letter a with acute, U+00E1 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 226,
        name: c"acirc".as_ptr() as _,
        desc: c"latin small letter a with circumflex, U+00E2 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 227,
        name: c"atilde".as_ptr() as _,
        desc: c"latin small letter a with tilde, U+00E3 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 228,
        name: c"auml".as_ptr() as _,
        desc: c"latin small letter a with diaeresis, U+00E4 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 229,
        name: c"aring".as_ptr() as _,
        desc: c"latin small letter a with ring above = latin small letter a ring, U+00E5 ISOlat1"
            .as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 230,
        name: c"aelig".as_ptr() as _,
        desc: c"latin small letter ae = latin small ligature ae, U+00E6 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 231,
        name: c"ccedil".as_ptr() as _,
        desc: c"latin small letter c with cedilla, U+00E7 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 232,
        name: c"egrave".as_ptr() as _,
        desc: c"latin small letter e with grave, U+00E8 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 233,
        name: c"eacute".as_ptr() as _,
        desc: c"latin small letter e with acute, U+00E9 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 234,
        name: c"ecirc".as_ptr() as _,
        desc: c"latin small letter e with circumflex, U+00EA ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 235,
        name: c"euml".as_ptr() as _,
        desc: c"latin small letter e with diaeresis, U+00EB ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 236,
        name: c"igrave".as_ptr() as _,
        desc: c"latin small letter i with grave, U+00EC ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 237,
        name: c"iacute".as_ptr() as _,
        desc: c"latin small letter i with acute, U+00ED ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 238,
        name: c"icirc".as_ptr() as _,
        desc: c"latin small letter i with circumflex, U+00EE ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 239,
        name: c"iuml".as_ptr() as _,
        desc: c"latin small letter i with diaeresis, U+00EF ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 240,
        name: c"eth".as_ptr() as _,
        desc: c"latin small letter eth, U+00F0 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 241,
        name: c"ntilde".as_ptr() as _,
        desc: c"latin small letter n with tilde, U+00F1 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 242,
        name: c"ograve".as_ptr() as _,
        desc: c"latin small letter o with grave, U+00F2 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 243,
        name: c"oacute".as_ptr() as _,
        desc: c"latin small letter o with acute, U+00F3 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 244,
        name: c"ocirc".as_ptr() as _,
        desc: c"latin small letter o with circumflex, U+00F4 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 245,
        name: c"otilde".as_ptr() as _,
        desc: c"latin small letter o with tilde, U+00F5 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 246,
        name: c"ouml".as_ptr() as _,
        desc: c"latin small letter o with diaeresis, U+00F6 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 247,
        name: c"divide".as_ptr() as _,
        desc: c"division sign, U+00F7 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 248,
        name: c"oslash".as_ptr() as _,
        desc: c"latin small letter o with stroke, = latin small letter o slash, U+00F8 ISOlat1"
            .as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 249,
        name: c"ugrave".as_ptr() as _,
        desc: c"latin small letter u with grave, U+00F9 ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 250,
        name: c"uacute".as_ptr() as _,
        desc: c"latin small letter u with acute, U+00FA ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 251,
        name: c"ucirc".as_ptr() as _,
        desc: c"latin small letter u with circumflex, U+00FB ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 252,
        name: c"uuml".as_ptr() as _,
        desc: c"latin small letter u with diaeresis, U+00FC ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 253,
        name: c"yacute".as_ptr() as _,
        desc: c"latin small letter y with acute, U+00FD ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 254,
        name: c"thorn".as_ptr() as _,
        desc: c"latin small letter thorn with, U+00FE ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 255,
        name: c"yuml".as_ptr() as _,
        desc: c"latin small letter y with diaeresis, U+00FF ISOlat1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 338,
        name: c"OElig".as_ptr() as _,
        desc: c"latin capital ligature OE, U+0152 ISOlat2".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 339,
        name: c"oelig".as_ptr() as _,
        desc: c"latin small ligature oe, U+0153 ISOlat2".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 352,
        name: c"Scaron".as_ptr() as _,
        desc: c"latin capital letter S with caron, U+0160 ISOlat2".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 353,
        name: c"scaron".as_ptr() as _,
        desc: c"latin small letter s with caron, U+0161 ISOlat2".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 376,
        name: c"Yuml".as_ptr() as _,
        desc: c"latin capital letter Y with diaeresis, U+0178 ISOlat2".as_ptr() as _,
    },
    /*
     * Anything below should really be kept as entities references
     */
    HtmlEntityDesc {
        value: 402,
        name: c"fnof".as_ptr() as _,
        desc: c"latin small f with hook = function = florin, U+0192 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 710,
        name: c"circ".as_ptr() as _,
        desc: c"modifier letter circumflex accent, U+02C6 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 732,
        name: c"tilde".as_ptr() as _,
        desc: c"small tilde, U+02DC ISOdia".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 913,
        name: c"Alpha".as_ptr() as _,
        desc: c"greek capital letter alpha, U+0391".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 914,
        name: c"Beta".as_ptr() as _,
        desc: c"greek capital letter beta, U+0392".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 915,
        name: c"Gamma".as_ptr() as _,
        desc: c"greek capital letter gamma, U+0393 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 916,
        name: c"Delta".as_ptr() as _,
        desc: c"greek capital letter delta, U+0394 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 917,
        name: c"Epsilon".as_ptr() as _,
        desc: c"greek capital letter epsilon, U+0395".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 918,
        name: c"Zeta".as_ptr() as _,
        desc: c"greek capital letter zeta, U+0396".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 919,
        name: c"Eta".as_ptr() as _,
        desc: c"greek capital letter eta, U+0397".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 920,
        name: c"Theta".as_ptr() as _,
        desc: c"greek capital letter theta, U+0398 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 921,
        name: c"Iota".as_ptr() as _,
        desc: c"greek capital letter iota, U+0399".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 922,
        name: c"Kappa".as_ptr() as _,
        desc: c"greek capital letter kappa, U+039A".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 923,
        name: c"Lambda".as_ptr() as _,
        desc: c"greek capital letter lambda, U+039B ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 924,
        name: c"Mu".as_ptr() as _,
        desc: c"greek capital letter mu, U+039C".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 925,
        name: c"Nu".as_ptr() as _,
        desc: c"greek capital letter nu, U+039D".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 926,
        name: c"Xi".as_ptr() as _,
        desc: c"greek capital letter xi, U+039E ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 927,
        name: c"Omicron".as_ptr() as _,
        desc: c"greek capital letter omicron, U+039F".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 928,
        name: c"Pi".as_ptr() as _,
        desc: c"greek capital letter pi, U+03A0 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 929,
        name: c"Rho".as_ptr() as _,
        desc: c"greek capital letter rho, U+03A1".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 931,
        name: c"Sigma".as_ptr() as _,
        desc: c"greek capital letter sigma, U+03A3 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 932,
        name: c"Tau".as_ptr() as _,
        desc: c"greek capital letter tau, U+03A4".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 933,
        name: c"Upsilon".as_ptr() as _,
        desc: c"greek capital letter upsilon, U+03A5 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 934,
        name: c"Phi".as_ptr() as _,
        desc: c"greek capital letter phi, U+03A6 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 935,
        name: c"Chi".as_ptr() as _,
        desc: c"greek capital letter chi, U+03A7".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 936,
        name: c"Psi".as_ptr() as _,
        desc: c"greek capital letter psi, U+03A8 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 937,
        name: c"Omega".as_ptr() as _,
        desc: c"greek capital letter omega, U+03A9 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 945,
        name: c"alpha".as_ptr() as _,
        desc: c"greek small letter alpha, U+03B1 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 946,
        name: c"beta".as_ptr() as _,
        desc: c"greek small letter beta, U+03B2 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 947,
        name: c"gamma".as_ptr() as _,
        desc: c"greek small letter gamma, U+03B3 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 948,
        name: c"delta".as_ptr() as _,
        desc: c"greek small letter delta, U+03B4 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 949,
        name: c"epsilon".as_ptr() as _,
        desc: c"greek small letter epsilon, U+03B5 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 950,
        name: c"zeta".as_ptr() as _,
        desc: c"greek small letter zeta, U+03B6 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 951,
        name: c"eta".as_ptr() as _,
        desc: c"greek small letter eta, U+03B7 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 952,
        name: c"theta".as_ptr() as _,
        desc: c"greek small letter theta, U+03B8 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 953,
        name: c"iota".as_ptr() as _,
        desc: c"greek small letter iota, U+03B9 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 954,
        name: c"kappa".as_ptr() as _,
        desc: c"greek small letter kappa, U+03BA ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 955,
        name: c"lambda".as_ptr() as _,
        desc: c"greek small letter lambda, U+03BB ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 956,
        name: c"mu".as_ptr() as _,
        desc: c"greek small letter mu, U+03BC ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 957,
        name: c"nu".as_ptr() as _,
        desc: c"greek small letter nu, U+03BD ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 958,
        name: c"xi".as_ptr() as _,
        desc: c"greek small letter xi, U+03BE ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 959,
        name: c"omicron".as_ptr() as _,
        desc: c"greek small letter omicron, U+03BF NEW".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 960,
        name: c"pi".as_ptr() as _,
        desc: c"greek small letter pi, U+03C0 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 961,
        name: c"rho".as_ptr() as _,
        desc: c"greek small letter rho, U+03C1 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 962,
        name: c"sigmaf".as_ptr() as _,
        desc: c"greek small letter final sigma, U+03C2 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 963,
        name: c"sigma".as_ptr() as _,
        desc: c"greek small letter sigma, U+03C3 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 964,
        name: c"tau".as_ptr() as _,
        desc: c"greek small letter tau, U+03C4 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 965,
        name: c"upsilon".as_ptr() as _,
        desc: c"greek small letter upsilon, U+03C5 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 966,
        name: c"phi".as_ptr() as _,
        desc: c"greek small letter phi, U+03C6 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 967,
        name: c"chi".as_ptr() as _,
        desc: c"greek small letter chi, U+03C7 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 968,
        name: c"psi".as_ptr() as _,
        desc: c"greek small letter psi, U+03C8 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 969,
        name: c"omega".as_ptr() as _,
        desc: c"greek small letter omega, U+03C9 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 977,
        name: c"thetasym".as_ptr() as _,
        desc: c"greek small letter theta symbol, U+03D1 NEW".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 978,
        name: c"upsih".as_ptr() as _,
        desc: c"greek upsilon with hook symbol, U+03D2 NEW".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 982,
        name: c"piv".as_ptr() as _,
        desc: c"greek pi symbol, U+03D6 ISOgrk3".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8194,
        name: c"ensp".as_ptr() as _,
        desc: c"en space, U+2002 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8195,
        name: c"emsp".as_ptr() as _,
        desc: c"em space, U+2003 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8201,
        name: c"thinsp".as_ptr() as _,
        desc: c"thin space, U+2009 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8204,
        name: c"zwnj".as_ptr() as _,
        desc: c"zero width non-joiner, U+200C NEW RFC 2070".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8205,
        name: c"zwj".as_ptr() as _,
        desc: c"zero width joiner, U+200D NEW RFC 2070".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8206,
        name: c"lrm".as_ptr() as _,
        desc: c"left-to-right mark, U+200E NEW RFC 2070".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8207,
        name: c"rlm".as_ptr() as _,
        desc: c"right-to-left mark, U+200F NEW RFC 2070".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8211,
        name: c"ndash".as_ptr() as _,
        desc: c"en dash, U+2013 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8212,
        name: c"mdash".as_ptr() as _,
        desc: c"em dash, U+2014 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8216,
        name: c"lsquo".as_ptr() as _,
        desc: c"left single quotation mark, U+2018 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8217,
        name: c"rsquo".as_ptr() as _,
        desc: c"right single quotation mark, U+2019 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8218,
        name: c"sbquo".as_ptr() as _,
        desc: c"single low-9 quotation mark, U+201A NEW".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8220,
        name: c"ldquo".as_ptr() as _,
        desc: c"left double quotation mark, U+201C ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8221,
        name: c"rdquo".as_ptr() as _,
        desc: c"right double quotation mark, U+201D ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8222,
        name: c"bdquo".as_ptr() as _,
        desc: c"double low-9 quotation mark, U+201E NEW".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8224,
        name: c"dagger".as_ptr() as _,
        desc: c"dagger, U+2020 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8225,
        name: c"Dagger".as_ptr() as _,
        desc: c"double dagger, U+2021 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8226,
        name: c"bull".as_ptr() as _,
        desc: c"bullet = black small circle, U+2022 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8230,
        name: c"hellip".as_ptr() as _,
        desc: c"horizontal ellipsis = three dot leader, U+2026 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8240,
        name: c"permil".as_ptr() as _,
        desc: c"per mille sign, U+2030 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8242,
        name: c"prime".as_ptr() as _,
        desc: c"prime = minutes = feet, U+2032 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8243,
        name: c"Prime".as_ptr() as _,
        desc: c"double prime = seconds = inches, U+2033 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8249,
        name: c"lsaquo".as_ptr() as _,
        desc: c"single left-pointing angle quotation mark, U+2039 ISO proposed".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8250,
        name: c"rsaquo".as_ptr() as _,
        desc: c"single right-pointing angle quotation mark, U+203A ISO proposed".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8254,
        name: c"oline".as_ptr() as _,
        desc: c"overline = spacing overscore, U+203E NEW".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8260,
        name: c"frasl".as_ptr() as _,
        desc: c"fraction slash, U+2044 NEW".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8364,
        name: c"euro".as_ptr() as _,
        desc: c"euro sign, U+20AC NEW".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8465,
        name: c"image".as_ptr() as _,
        desc: c"blackletter capital I = imaginary part, U+2111 ISOamso".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8472,
        name: c"weierp".as_ptr() as _,
        desc: c"script capital P = power set = Weierstrass p, U+2118 ISOamso".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8476,
        name: c"real".as_ptr() as _,
        desc: c"blackletter capital R = real part symbol, U+211C ISOamso".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8482,
        name: c"trade".as_ptr() as _,
        desc: c"trade mark sign, U+2122 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8501,
        name: c"alefsym".as_ptr() as _,
        desc: c"alef symbol = first transfinite cardinal, U+2135 NEW".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8592,
        name: c"larr".as_ptr() as _,
        desc: c"leftwards arrow, U+2190 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8593,
        name: c"uarr".as_ptr() as _,
        desc: c"upwards arrow, U+2191 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8594,
        name: c"rarr".as_ptr() as _,
        desc: c"rightwards arrow, U+2192 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8595,
        name: c"darr".as_ptr() as _,
        desc: c"downwards arrow, U+2193 ISOnum".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8596,
        name: c"harr".as_ptr() as _,
        desc: c"left right arrow, U+2194 ISOamsa".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8629,
        name: c"crarr".as_ptr() as _,
        desc: c"downwards arrow with corner leftwards = carriage return, U+21B5 NEW".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8656,
        name: c"lArr".as_ptr() as _,
        desc: c"leftwards double arrow, U+21D0 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8657,
        name: c"uArr".as_ptr() as _,
        desc: c"upwards double arrow, U+21D1 ISOamsa".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8658,
        name: c"rArr".as_ptr() as _,
        desc: c"rightwards double arrow, U+21D2 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8659,
        name: c"dArr".as_ptr() as _,
        desc: c"downwards double arrow, U+21D3 ISOamsa".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8660,
        name: c"hArr".as_ptr() as _,
        desc: c"left right double arrow, U+21D4 ISOamsa".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8704,
        name: c"forall".as_ptr() as _,
        desc: c"for all, U+2200 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8706,
        name: c"part".as_ptr() as _,
        desc: c"partial differential, U+2202 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8707,
        name: c"exist".as_ptr() as _,
        desc: c"there exists, U+2203 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8709,
        name: c"empty".as_ptr() as _,
        desc: c"empty set = null set = diameter, U+2205 ISOamso".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8711,
        name: c"nabla".as_ptr() as _,
        desc: c"nabla = backward difference, U+2207 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8712,
        name: c"isin".as_ptr() as _,
        desc: c"element of, U+2208 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8713,
        name: c"notin".as_ptr() as _,
        desc: c"not an element of, U+2209 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8715,
        name: c"ni".as_ptr() as _,
        desc: c"contains as member, U+220B ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8719,
        name: c"prod".as_ptr() as _,
        desc: c"n-ary product = product sign, U+220F ISOamsb".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8721,
        name: c"sum".as_ptr() as _,
        desc: c"n-ary summation, U+2211 ISOamsb".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8722,
        name: c"minus".as_ptr() as _,
        desc: c"minus sign, U+2212 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8727,
        name: c"lowast".as_ptr() as _,
        desc: c"asterisk operator, U+2217 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8730,
        name: c"radic".as_ptr() as _,
        desc: c"square root = radical sign, U+221A ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8733,
        name: c"prop".as_ptr() as _,
        desc: c"proportional to, U+221D ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8734,
        name: c"infin".as_ptr() as _,
        desc: c"infinity, U+221E ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8736,
        name: c"ang".as_ptr() as _,
        desc: c"angle, U+2220 ISOamso".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8743,
        name: c"and".as_ptr() as _,
        desc: c"logical and = wedge, U+2227 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8744,
        name: c"or".as_ptr() as _,
        desc: c"logical or = vee, U+2228 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8745,
        name: c"cap".as_ptr() as _,
        desc: c"intersection = cap, U+2229 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8746,
        name: c"cup".as_ptr() as _,
        desc: c"union = cup, U+222A ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8747,
        name: c"int".as_ptr() as _,
        desc: c"integral, U+222B ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8756,
        name: c"there4".as_ptr() as _,
        desc: c"therefore, U+2234 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8764,
        name: c"sim".as_ptr() as _,
        desc: c"tilde operator = varies with = similar to, U+223C ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8773,
        name: c"cong".as_ptr() as _,
        desc: c"approximately equal to, U+2245 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8776,
        name: c"asymp".as_ptr() as _,
        desc: c"almost equal to = asymptotic to, U+2248 ISOamsr".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8800,
        name: c"ne".as_ptr() as _,
        desc: c"not equal to, U+2260 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8801,
        name: c"equiv".as_ptr() as _,
        desc: c"identical to, U+2261 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8804,
        name: c"le".as_ptr() as _,
        desc: c"less-than or equal to, U+2264 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8805,
        name: c"ge".as_ptr() as _,
        desc: c"greater-than or equal to, U+2265 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8834,
        name: c"sub".as_ptr() as _,
        desc: c"subset of, U+2282 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8835,
        name: c"sup".as_ptr() as _,
        desc: c"superset of, U+2283 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8836,
        name: c"nsub".as_ptr() as _,
        desc: c"not a subset of, U+2284 ISOamsn".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8838,
        name: c"sube".as_ptr() as _,
        desc: c"subset of or equal to, U+2286 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8839,
        name: c"supe".as_ptr() as _,
        desc: c"superset of or equal to, U+2287 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8853,
        name: c"oplus".as_ptr() as _,
        desc: c"circled plus = direct sum, U+2295 ISOamsb".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8855,
        name: c"otimes".as_ptr() as _,
        desc: c"circled times = vector product, U+2297 ISOamsb".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8869,
        name: c"perp".as_ptr() as _,
        desc: c"up tack = orthogonal to = perpendicular, U+22A5 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8901,
        name: c"sdot".as_ptr() as _,
        desc: c"dot operator, U+22C5 ISOamsb".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8968,
        name: c"lceil".as_ptr() as _,
        desc: c"left ceiling = apl upstile, U+2308 ISOamsc".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8969,
        name: c"rceil".as_ptr() as _,
        desc: c"right ceiling, U+2309 ISOamsc".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8970,
        name: c"lfloor".as_ptr() as _,
        desc: c"left floor = apl downstile, U+230A ISOamsc".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 8971,
        name: c"rfloor".as_ptr() as _,
        desc: c"right floor, U+230B ISOamsc".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 9001,
        name: c"lang".as_ptr() as _,
        desc: c"left-pointing angle bracket = bra, U+2329 ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 9002,
        name: c"rang".as_ptr() as _,
        desc: c"right-pointing angle bracket = ket, U+232A ISOtech".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 9674,
        name: c"loz".as_ptr() as _,
        desc: c"lozenge, U+25CA ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 9824,
        name: c"spades".as_ptr() as _,
        desc: c"black spade suit, U+2660 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 9827,
        name: c"clubs".as_ptr() as _,
        desc: c"black club suit = shamrock, U+2663 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 9829,
        name: c"hearts".as_ptr() as _,
        desc: c"black heart suit = valentine, U+2665 ISOpub".as_ptr() as _,
    },
    HtmlEntityDesc {
        value: 9830,
        name: c"diams".as_ptr() as _,
        desc: c"black diamond suit, U+2666 ISOpub".as_ptr() as _,
    },
];

#[doc(alias = "htmlInitAutoClose")]
#[deprecated = "This is a no-op"]
pub unsafe extern "C" fn html_init_auto_close() {}

/// Lookup the HTML tag in the ElementTable
///
/// Returns the related htmlElemDescPtr or null_mut() if not found.
#[doc(alias = "htmlTagLookup")]
pub unsafe fn html_tag_lookup(tag: &str) -> Option<&'static HtmlElemDesc> {
    let tag = tag.to_ascii_lowercase();
    HTML40_ELEMENT_TABLE
        .binary_search_by(|desc| desc.name.to_ascii_lowercase().cmp(&tag))
        .ok()
        .and_then(|pos| HTML40_ELEMENT_TABLE.get(pos))
}

/// Lookup the given entity in EntitiesTable
///
/// TODO: the linear scan is really ugly, an hash table is really needed.
///
/// Returns the associated htmlEntityDescPtr if found, NULL otherwise.
#[doc(alias = "htmlEntityLookup")]
pub unsafe extern "C" fn html_entity_lookup(name: *const XmlChar) -> *const HtmlEntityDesc {
    for entry in HTML40_ENTITIES_TABLE {
        if xml_str_equal(name, entry.name as _) {
            return entry as *const HtmlEntityDesc;
        }
    }
    null_mut()
}

/// Lookup the given entity in EntitiesTable
///
/// TODO: the linear scan is really ugly, an hash table is really needed.
///
/// Returns the associated htmlEntityDescPtr if found, NULL otherwise.
#[doc(alias = "htmlEntityValueLookup")]
pub unsafe extern "C" fn html_entity_value_lookup(value: u32) -> *const HtmlEntityDesc {
    for entry in HTML40_ENTITIES_TABLE {
        if entry.value >= value {
            if entry.value > value {
                break;
            }
            return entry as *const HtmlEntityDesc;
        }
    }
    null_mut()
}

/// The HTML DTD allows a tag to implicitly close other tags.
/// The list is kept in htmlStartClose array. This function checks
/// if a tag is autoclosed by one of it's child
///
/// Returns 1 if autoclosed, 0 otherwise
#[doc(alias = "htmlIsAutoClosed")]
pub unsafe extern "C" fn html_is_auto_closed(doc: HtmlDocPtr, elem: HtmlNodePtr) -> i32 {
    if elem.is_null() {
        return 1;
    }
    let mut child = (*elem).children().map_or(null_mut(), |c| c.as_ptr());
    while !child.is_null() {
        if html_auto_close_tag(doc, (*elem).name, child) != 0 {
            return 1;
        }
        child = (*child).next.map_or(null_mut(), |n| n.as_ptr());
    }
    0
}

#[repr(C)]
pub struct HtmlStartCloseEntry {
    old_tag: *const c_char,
    new_tag: *const c_char,
}

// start tags that imply the end of current element
const HTML_START_CLOSE: &[HtmlStartCloseEntry] = &[
    HtmlStartCloseEntry {
        old_tag: c"a".as_ptr() as _,
        new_tag: c"a".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"a".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"a".as_ptr() as _,
        new_tag: c"table".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"a".as_ptr() as _,
        new_tag: c"td".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"a".as_ptr() as _,
        new_tag: c"th".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"address".as_ptr() as _,
        new_tag: c"dd".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"address".as_ptr() as _,
        new_tag: c"dl".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"address".as_ptr() as _,
        new_tag: c"dt".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"address".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"address".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"address".as_ptr() as _,
        new_tag: c"ul".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"b".as_ptr() as _,
        new_tag: c"center".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"b".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"b".as_ptr() as _,
        new_tag: c"td".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"b".as_ptr() as _,
        new_tag: c"th".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"big".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"caption".as_ptr() as _,
        new_tag: c"col".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"caption".as_ptr() as _,
        new_tag: c"colgroup".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"caption".as_ptr() as _,
        new_tag: c"tbody".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"caption".as_ptr() as _,
        new_tag: c"tfoot".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"caption".as_ptr() as _,
        new_tag: c"thead".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"caption".as_ptr() as _,
        new_tag: c"tr".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"col".as_ptr() as _,
        new_tag: c"col".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"col".as_ptr() as _,
        new_tag: c"colgroup".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"col".as_ptr() as _,
        new_tag: c"tbody".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"col".as_ptr() as _,
        new_tag: c"tfoot".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"col".as_ptr() as _,
        new_tag: c"thead".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"col".as_ptr() as _,
        new_tag: c"tr".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"colgroup".as_ptr() as _,
        new_tag: c"colgroup".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"colgroup".as_ptr() as _,
        new_tag: c"tbody".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"colgroup".as_ptr() as _,
        new_tag: c"tfoot".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"colgroup".as_ptr() as _,
        new_tag: c"thead".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"colgroup".as_ptr() as _,
        new_tag: c"tr".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"dd".as_ptr() as _,
        new_tag: c"dt".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"dir".as_ptr() as _,
        new_tag: c"dd".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"dir".as_ptr() as _,
        new_tag: c"dl".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"dir".as_ptr() as _,
        new_tag: c"dt".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"dir".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"dir".as_ptr() as _,
        new_tag: c"ul".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"dl".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"dl".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"dt".as_ptr() as _,
        new_tag: c"dd".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"dt".as_ptr() as _,
        new_tag: c"dl".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"font".as_ptr() as _,
        new_tag: c"center".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"font".as_ptr() as _,
        new_tag: c"td".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"font".as_ptr() as _,
        new_tag: c"th".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"form".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h1".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h1".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h1".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h1".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h1".as_ptr() as _,
        new_tag: c"table".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h2".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h2".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h2".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h2".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h2".as_ptr() as _,
        new_tag: c"table".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h3".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h3".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h3".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h3".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h3".as_ptr() as _,
        new_tag: c"table".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h4".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h4".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h4".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h4".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h4".as_ptr() as _,
        new_tag: c"table".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h5".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h5".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h5".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h5".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h5".as_ptr() as _,
        new_tag: c"table".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h6".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h6".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h6".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h6".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"h6".as_ptr() as _,
        new_tag: c"table".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"a".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"abbr".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"acronym".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"address".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"b".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"bdo".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"big".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"blockquote".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"body".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"br".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"center".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"cite".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"code".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"dd".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"dfn".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"dir".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"div".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"dl".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"dt".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"em".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"font".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"frameset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"h1".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"h2".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"h3".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"h4".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"h5".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"h6".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"hr".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"i".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"iframe".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"img".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"kbd".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"listing".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"map".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"menu".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"ol".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"pre".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"q".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"s".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"samp".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"small".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"span".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"strike".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"strong".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"sub".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"sup".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"table".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"tt".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"u".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"ul".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"var".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"head".as_ptr() as _,
        new_tag: c"xmp".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"hr".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"i".as_ptr() as _,
        new_tag: c"center".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"i".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"i".as_ptr() as _,
        new_tag: c"td".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"i".as_ptr() as _,
        new_tag: c"th".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"legend".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"li".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"link".as_ptr() as _,
        new_tag: c"body".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"link".as_ptr() as _,
        new_tag: c"frameset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"listing".as_ptr() as _,
        new_tag: c"dd".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"listing".as_ptr() as _,
        new_tag: c"dl".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"listing".as_ptr() as _,
        new_tag: c"dt".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"listing".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"listing".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"listing".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"listing".as_ptr() as _,
        new_tag: c"table".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"listing".as_ptr() as _,
        new_tag: c"ul".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"menu".as_ptr() as _,
        new_tag: c"dd".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"menu".as_ptr() as _,
        new_tag: c"dl".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"menu".as_ptr() as _,
        new_tag: c"dt".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"menu".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"menu".as_ptr() as _,
        new_tag: c"ul".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"ol".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"option".as_ptr() as _,
        new_tag: c"optgroup".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"option".as_ptr() as _,
        new_tag: c"option".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"address".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"blockquote".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"body".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"caption".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"center".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"col".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"colgroup".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"dd".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"dir".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"div".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"dl".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"dt".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"frameset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"h1".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"h2".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"h3".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"h4".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"h5".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"h6".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"head".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"hr".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"listing".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"menu".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"ol".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"pre".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"table".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"tbody".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"td".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"tfoot".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"th".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"title".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"tr".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"ul".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"p".as_ptr() as _,
        new_tag: c"xmp".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"pre".as_ptr() as _,
        new_tag: c"dd".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"pre".as_ptr() as _,
        new_tag: c"dl".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"pre".as_ptr() as _,
        new_tag: c"dt".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"pre".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"pre".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"pre".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"pre".as_ptr() as _,
        new_tag: c"table".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"pre".as_ptr() as _,
        new_tag: c"ul".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"s".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"script".as_ptr() as _,
        new_tag: c"noscript".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"small".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"span".as_ptr() as _,
        new_tag: c"td".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"span".as_ptr() as _,
        new_tag: c"th".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"strike".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"style".as_ptr() as _,
        new_tag: c"body".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"style".as_ptr() as _,
        new_tag: c"frameset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"tbody".as_ptr() as _,
        new_tag: c"tbody".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"tbody".as_ptr() as _,
        new_tag: c"tfoot".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"td".as_ptr() as _,
        new_tag: c"tbody".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"td".as_ptr() as _,
        new_tag: c"td".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"td".as_ptr() as _,
        new_tag: c"tfoot".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"td".as_ptr() as _,
        new_tag: c"th".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"td".as_ptr() as _,
        new_tag: c"tr".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"tfoot".as_ptr() as _,
        new_tag: c"tbody".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"th".as_ptr() as _,
        new_tag: c"tbody".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"th".as_ptr() as _,
        new_tag: c"td".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"th".as_ptr() as _,
        new_tag: c"tfoot".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"th".as_ptr() as _,
        new_tag: c"th".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"th".as_ptr() as _,
        new_tag: c"tr".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"thead".as_ptr() as _,
        new_tag: c"tbody".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"thead".as_ptr() as _,
        new_tag: c"tfoot".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"title".as_ptr() as _,
        new_tag: c"body".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"title".as_ptr() as _,
        new_tag: c"frameset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"tr".as_ptr() as _,
        new_tag: c"tbody".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"tr".as_ptr() as _,
        new_tag: c"tfoot".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"tr".as_ptr() as _,
        new_tag: c"tr".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"tt".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"u".as_ptr() as _,
        new_tag: c"p".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"u".as_ptr() as _,
        new_tag: c"td".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"u".as_ptr() as _,
        new_tag: c"th".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"ul".as_ptr() as _,
        new_tag: c"address".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"ul".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"ul".as_ptr() as _,
        new_tag: c"menu".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"ul".as_ptr() as _,
        new_tag: c"pre".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"xmp".as_ptr() as _,
        new_tag: c"dd".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"xmp".as_ptr() as _,
        new_tag: c"dl".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"xmp".as_ptr() as _,
        new_tag: c"dt".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"xmp".as_ptr() as _,
        new_tag: c"fieldset".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"xmp".as_ptr() as _,
        new_tag: c"form".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"xmp".as_ptr() as _,
        new_tag: c"li".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"xmp".as_ptr() as _,
        new_tag: c"table".as_ptr() as _,
    },
    HtmlStartCloseEntry {
        old_tag: c"xmp".as_ptr() as _,
        new_tag: c"ul".as_ptr() as _,
    },
];

unsafe extern "C" fn html_compare_start_close(vkey: *const c_void, member: *const c_void) -> i32 {
    let key: *const HtmlStartCloseEntry = vkey as *const HtmlStartCloseEntry;
    let entry: *const HtmlStartCloseEntry = member as *const HtmlStartCloseEntry;
    let mut ret: i32;

    ret = strcmp((*key).old_tag, (*entry).old_tag);
    if ret == 0 {
        ret = strcmp((*key).new_tag, (*entry).new_tag);
    }

    ret
}

/// Checks whether the new tag is one of the registered valid tags for closing old.
///
/// Returns 0 if no, 1 if yes.
#[doc(alias = "htmlCheckAutoClose")]
unsafe fn html_check_auto_close(newtag: *const XmlChar, oldtag: Option<&str>) -> i32 {
    let mut key: HtmlStartCloseEntry = unsafe { zeroed() };

    let oldtag = oldtag.map(|t| CString::new(t).unwrap());
    key.old_tag = oldtag.as_deref().map_or(null(), |t| t.as_ptr());
    key.new_tag = newtag as *const c_char;
    let res: *mut c_void = bsearch(
        addr_of_mut!(key) as _,
        HTML_START_CLOSE.as_ptr() as _,
        HTML_START_CLOSE.len(),
        size_of::<HtmlStartCloseEntry>(),
        Some(html_compare_start_close),
    );
    !res.is_null() as i32
}

/// The HTML DTD allows a tag to implicitly close other tags.
/// The list is kept in htmlStartClose array. This function checks
/// if the element or one of it's children would autoclose the given tag.
///
/// Returns 1 if autoclose, 0 otherwise
#[doc(alias = "htmlAutoCloseTag")]
pub unsafe extern "C" fn html_auto_close_tag(
    _doc: HtmlDocPtr,
    name: *const XmlChar,
    elem: HtmlNodePtr,
) -> i32 {
    if elem.is_null() {
        return 1;
    }
    if xml_str_equal(name, (*elem).name) {
        return 0;
    }
    if html_check_auto_close(
        (*elem).name,
        (!name.is_null())
            .then(|| CStr::from_ptr(name as *const i8).to_string_lossy())
            .as_deref(),
    ) != 0
    {
        return 1;
    }
    let mut child = (*elem).children().map_or(null_mut(), |c| c.as_ptr());
    while !child.is_null() {
        if html_auto_close_tag(_doc, name, child) != 0 {
            return 1;
        }
        child = (*child).next.map_or(null_mut(), |n| n.as_ptr());
    }
    0
}

/*
 * Macros for accessing the content. Those should be used only by the parser,
 * and not exported.
 *
 * Dirty macros, i.e. one need to make assumption on the context to use them
 *
 *   CUR_PTR return the current pointer to the XmlChar to be parsed.
 *   CUR     returns the current XmlChar value, i.e. a 8 bit value if compiled
 *           in ISO-Latin or UTF-8, and the current 16 bit value if compiled
 *           in UNICODE mode. This should be used internally by the parser
 *           only to compare to ASCII values otherwise it would break when
 *           running with UTF-8 encoding.
 *   NXT(n)  returns the n'th next XmlChar. Same as CUR is should be used only
 *           to compare on ASCII based substring.
 *   UPP!(ctxt, n)  returns the n'th next XmlChar converted to uppercase. Same as CUR
 *           it should be used only to compare on ASCII based substring.
 *   SKIP(n) Skip n XmlChar, and must also be used only to skip ASCII defined
 *           strings without newlines within the parser.
 *
 * Clean macros, not dependent of an ASCII context, expect UTF-8 encoding
 *
 *   NEXT    Skip to the next character, this does the proper decoding
 *           in UTF-8 mode. It also pop-up unfinished entities on the fly.
 *   NEXTL(l) Skip the current unicode character of l xmlChars long.
 *   COPY(to) copy one c_char to *to, increment CUR_PTR and to accordingly
 */

macro_rules! UPPER {
    ($ctxt:expr) => {
        (*(*(*$ctxt).input).cur).to_ascii_uppercase()
    };
}

macro_rules! NXT {
    ($ctxt:expr, $val:expr) => {
        *(*(*$ctxt).input).cur.add($val as usize)
    };
}

macro_rules! UPP {
    ($ctxt:expr, $val:expr) => {
        (*(*(*$ctxt).input).cur.add($val as usize)).to_ascii_uppercase()
    };
}

macro_rules! SKIP_BLANKS {
    ($ctxt:expr) => {
        html_skip_blank_chars($ctxt)
    };
}

/* Imported from XML */

macro_rules! RAW {
    ($ctxt:expr) => {
        if (*$ctxt).token != 0 {
            1u8.wrapping_neg()
        } else {
            *(*(*$ctxt).input).cur as u8
        }
    };
}

macro_rules! NEXTL {
    ($ctxt:expr, $l:expr) => {
        if *((*(*$ctxt).input).cur) == b'\n' {
            (*(*$ctxt).input).line += 1;
            (*(*$ctxt).input).col = 1;
        } else {
            (*(*$ctxt).input).col += 1;
        }
        (*$ctxt).token = 0;
        (*(*$ctxt).input).cur = (*(*$ctxt).input).cur.add($l as usize);
    };
}
/************
   \
   if (*(*(*ctxt).input).cur == b'%') xmlParserHandlePEReference(ctxt);	\
   if (*(*(*ctxt).input).cur == b'&') xmlParserHandleReference(ctxt);
************/

macro_rules! CUR_CHAR {
    ($ctxt:expr, $l:expr) => {
        html_current_char($ctxt, addr_of_mut!($l))
    };
}
macro_rules! COPY_BUF {
    ($ctxt:expr, $l:expr, $b:expr, $i:expr, $v:expr) => {
        if $l == 1 {
            *$b.add($i as usize) = $v as _;
            $i += 1;
        } else {
            $i += crate::libxml::parser_internals::xml_copy_char(
                $l as _,
                $b.add($i as usize) as _,
                $v as _,
            );
        }
    };
}

/// Parse a content: comment, sub-element, reference or text.
/// This is the entry point when called from parser.c
#[doc(alias = "htmlParseContent")]
pub(crate) unsafe fn __html_parse_content(ctxt: *mut c_void) {
    if !ctxt.is_null() {
        html_parse_content_internal(ctxt as HtmlParserCtxtPtr);
    }
}

/// skip all blanks character found at that point in the input streams.
///
/// Returns the number of space chars skipped
#[doc(alias = "htmlSkipBlankChars")]
unsafe fn html_skip_blank_chars(ctxt: XmlParserCtxtPtr) -> i32 {
    let mut res: i32 = 0;

    while xml_is_blank_char(*(*(*ctxt).input).cur as u32) {
        if *(*(*ctxt).input).cur == b'\n' {
            (*(*ctxt).input).line += 1;
            (*(*ctxt).input).col = 1;
        } else {
            (*(*ctxt).input).col += 1;
        }
        (*(*ctxt).input).cur = (*(*ctxt).input).cur.add(1);
        if *(*(*ctxt).input).cur == 0 {
            (*ctxt).force_grow();
        }
        res = res.saturating_add(1);
    }
    res
}

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "htmlParseErrInt")]
macro_rules! html_parse_err_int {
    ($ctxt:expr, $error:expr, $msg:literal, $val:expr) => {
        if $ctxt.is_null()
            || (*$ctxt).disable_sax == 0
            || !matches!((*$ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !$ctxt.is_null() {
                (*$ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                $ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromHTML,
                $error,
                XmlErrorLevel::XmlErrError,
                None,
                0,
                None,
                None,
                None,
                $val,
                0,
                format!($msg, $val).as_str(),
            );
            if !$ctxt.is_null() {
                (*$ctxt).well_formed = 0;
            }
        }
    };
}

/// Ty to find and encoding in the current data available in the input
/// buffer this is needed to try to match to the proper encoding when
/// one face a character error.
/// That's an heuristic, since it's operating outside of parsing it could
/// try to use a meta which had been commented out, that's the reason it
/// should only be used in case of error, not as a default.
///
/// Returns an encoding string or NULL if not found, the string need to be freed
#[doc(alias = "htmlFindEncoding")]
unsafe fn html_find_encoding(ctxt: XmlParserCtxtPtr) -> *mut XmlChar {
    let mut start: *const XmlChar;
    let mut cur: *const XmlChar;

    if ctxt.is_null()
        || (*ctxt).input.is_null()
        || (*(*ctxt).input).encoding.is_some()
        || (*(*ctxt).input).buf.is_none()
        || (*(*ctxt).input)
            .buf
            .as_ref()
            .unwrap()
            .borrow()
            .encoder
            .is_some()
    {
        return null_mut();
    }
    if (*(*ctxt).input).cur.is_null() || (*(*ctxt).input).end.is_null() {
        return null_mut();
    }

    start = (*(*ctxt).input).cur;
    let end: *const XmlChar = (*(*ctxt).input).end;
    // we also expect the input buffer to be zero terminated
    if *end != 0 {
        return null_mut();
    }

    cur = xml_strcasestr(start, c"HTTP-EQUIV".as_ptr() as _);
    if cur.is_null() {
        return null_mut();
    }
    cur = xml_strcasestr(cur, c"CONTENT".as_ptr() as _);
    if cur.is_null() {
        return null_mut();
    }
    cur = xml_strcasestr(cur, c"CHARSET=".as_ptr() as _);
    if cur.is_null() {
        return null_mut();
    }
    cur = cur.add(8);
    start = cur;
    while (*cur >= b'A' && *cur <= b'Z')
        || (*cur >= b'a' && *cur <= b'z')
        || (*cur >= b'0' && *cur <= b'9')
        || *cur == b'-'
        || *cur == b'_'
        || *cur == b':'
        || *cur == b'/'
    {
        cur = cur.add(1);
    }
    if cur == start {
        return null_mut();
    }
    xml_strndup(start, cur.offset_from(start) as _)
}

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "htmlParseErr")]
unsafe fn html_parse_err(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = error as i32;
    }
    __xml_raise_error!(
        None,
        None,
        None,
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromHTML,
        error,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        str1.map(|s| s.to_owned().into()),
        str2.map(|s| s.to_owned().into()),
        None,
        0,
        0,
        msg,
    );
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
    }
}

/// The current char value, if using UTF-8 this may actually span multiple
/// bytes in the input buffer. Implement the end of line normalization:
/// 2.11 End-of-Line Handling
/// If the encoding is unspecified, in the case we find an ISO-Latin-1
/// char, then the encoding converter is plugged in automatically.
///
/// Returns the current char value and its length
#[doc(alias = "htmlCurrentChar")]
unsafe fn html_current_char(ctxt: XmlParserCtxtPtr, len: *mut i32) -> i32 {
    let mut val: u32;

    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return 0;
    }

    if (*ctxt).token != 0 {
        *len = 0;
        return (*ctxt).token;
    }

    if (*(*ctxt).input).remainder_len() < INPUT_CHUNK && (*ctxt).force_grow() < 0 {
        return 0;
    }

    if (*ctxt).charset != XmlCharEncoding::UTF8 {
        // Assume it's a fixed length encoding (1) with
        // a compatible encoding for the ASCII set, since
        // HTML constructs only use < 128 chars
        if *(*(*ctxt).input).cur < 0x80 {
            *len = 1;
            if *(*(*ctxt).input).cur == 0 && (*(*ctxt).input).cur < (*(*ctxt).input).end {
                html_parse_err_int!(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidChar,
                    "Char 0x{:X} out of allowed range\n",
                    0
                );
                return b' ' as _;
            }
            return *(*(*ctxt).input).cur as _;
        }

        // Humm this is bad, do an automatic flow conversion
        let guess: *mut XmlChar = html_find_encoding(ctxt);
        if guess.is_null() {
            (*ctxt).switch_encoding(XmlCharEncoding::ISO8859_1);
        } else {
            (*(*ctxt).input).encoding = Some(
                CStr::from_ptr(guess as *const i8)
                    .to_string_lossy()
                    .into_owned(),
            );
            if let Some(handler) =
                find_encoding_handler(CStr::from_ptr(guess as *const i8).to_str().unwrap())
            {
                // Don't use UTF-8 encoder which isn't required and
                // can produce invalid UTF-8.
                if handler.name() != "UTF-8" {
                    (*ctxt).switch_to_encoding(handler);
                }
            } else {
                let guess = CStr::from_ptr(guess as *const i8).to_string_lossy();
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidEncoding,
                    format!("Unsupported encoding {guess}").as_str(),
                    Some(&guess),
                    None,
                );
            }
        }
        (*ctxt).charset = XmlCharEncoding::UTF8;
    }

    // We are supposed to handle UTF8, check it's valid
    // From rfc2044: encoding of the Unicode values on UTF-8:
    //
    // UCS-4 range (hex.)           UTF-8 octet sequence (binary)
    // 0000 0000-0000 007F   0xxxxxxx
    // 0000 0080-0000 07FF   110xxxxx 10xxxxxx
    // 0000 0800-0000 FFFF   1110xxxx 10xxxxxx 10xxxxxx
    //
    // Check for the 0x110000 limit too
    let cur: *const u8 = (*(*ctxt).input).cur;
    let c: u8 = *cur;
    'goto_encoding_error: {
        if c & 0x80 != 0 {
            if c & 0x40 == 0 {
                break 'goto_encoding_error;
            }
            let avail = (*(*ctxt).input).remainder_len();

            if avail < 2 || *cur.add(1) & 0xc0 != 0x80 {
                break 'goto_encoding_error;
            }
            if c & 0xe0 == 0xe0 {
                if avail < 3 || *cur.add(2) & 0xc0 != 0x80 {
                    break 'goto_encoding_error;
                }
                if c & 0xf0 == 0xf0 {
                    if c & 0xf8 != 0xf0 || avail < 4 || *cur.add(3) & 0xc0 != 0x80 {
                        break 'goto_encoding_error;
                    }
                    // 4-byte code
                    *len = 4;
                    val = (*cur.add(0) as u32 & 0x7) << 18;
                    val |= (*cur.add(1) as u32 & 0x3f) << 12;
                    val |= (*cur.add(2) as u32 & 0x3f) << 6;
                    val |= *cur.add(3) as u32 & 0x3f;
                    if val < 0x10000 {
                        break 'goto_encoding_error;
                    }
                } else {
                    // 3-byte code
                    *len = 3;
                    val = (*cur.add(0) as u32 & 0xf) << 12;
                    val |= (*cur.add(1) as u32 & 0x3f) << 6;
                    val |= *cur.add(2) as u32 & 0x3f;
                    if val < 0x800 {
                        break 'goto_encoding_error;
                    }
                }
            } else {
                // 2-byte code
                *len = 2;
                val = (*cur.add(0) as u32 & 0x1f) << 6;
                val |= *cur.add(1) as u32 & 0x3f;
                if val < 0x80 {
                    break 'goto_encoding_error;
                }
            }

            if !xml_is_char(val) {
                html_parse_err_int!(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidChar,
                    "Char 0x{:X} out of allowed range\n",
                    val as i32
                );
            }
            return val as _;
        } else {
            if *(*(*ctxt).input).cur == 0 && (*(*ctxt).input).cur < (*(*ctxt).input).end {
                html_parse_err_int!(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidChar,
                    "Char 0x{:X} out of allowed range\n",
                    0
                );
                *len = 1;
                return b' ' as _;
            }
            // 1-byte code
            *len = 1;
            return *(*(*ctxt).input).cur as _;
        }
    }

    //  encoding_error:
    // If we detect an UTF8 error that probably mean that the
    // input encoding didn't get properly advertised in the declaration header.
    // Report the error and match the encoding
    // to ISO-Latin-1 (if you don't like this policy, just declare the encoding !)
    {
        use std::fmt::Write as _;
        let mut buffer = String::new();

        if (*(*ctxt).input).remainder_len() >= 4 {
            writeln!(
                buffer,
                "Bytes: 0x{:02X} 0x{:02X} 0x{:02X} 0x{:02X}",
                *(*(*ctxt).input).cur.add(0) as u32,
                *(*(*ctxt).input).cur.add(1) as u32,
                *(*(*ctxt).input).cur.add(2) as u32,
                *(*(*ctxt).input).cur.add(3) as u32,
            )
            .ok();
        } else {
            writeln!(
                buffer,
                "Bytes: 0x{:02X}",
                *(*(*ctxt).input).cur.add(0) as u32,
            )
            .ok();
        }
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrInvalidEncoding,
            "Input is not proper UTF-8, indicate encoding !\n",
            Some(&buffer),
            None,
        );
    }

    /*
     * Don't match encodings twice. Note that if there's an encoder, we
     * shouldn't receive invalid UTF-8 anyway.
     *
     * Note that if (*(*ctxt).input).buf.is_null(), switching encodings is
     * impossible, see Gitlab issue #34.
     */
    if (*(*ctxt).input).buf.is_some()
        && (*(*ctxt).input)
            .buf
            .as_ref()
            .unwrap()
            .borrow()
            .encoder
            .is_none()
    {
        (*ctxt).switch_encoding(XmlCharEncoding::ISO8859_1);
    }
    *len = 1;
    *(*(*ctxt).input).cur as _
}

#[doc(alias = "htmlParseNameComplex")]
unsafe fn html_parse_name_complex(ctxt: XmlParserCtxtPtr) -> *const XmlChar {
    let mut len: i32 = 0;
    let mut l: i32 = 0;
    let mut c: i32;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_TEXT_LENGTH as i32
    } else {
        XML_MAX_NAME_LENGTH as i32
    };
    let base: *const XmlChar = (*(*ctxt).input).base;

    // Handler for more complex cases
    c = CUR_CHAR!(ctxt, l);
    if c == b' ' as i32
        || c == b'>' as i32
        || c == b'/' as i32  /* accelerators */
        || (!xml_is_letter(c as u32) && c != b'_' as i32 && c != b':' as i32)
    {
        return null_mut();
    }

    while c != b' ' as i32
        && c != b'>' as i32
        && c != b'/' as i32 /* test bigname.xml */
        && (xml_is_letter(c as u32)
            || xml_is_digit(c as u32)
            || c == b'.' as i32
            || c == b'-' as i32
            || c == b'_' as i32
            || c == b':' as i32
            || xml_is_combining(c as u32)
            || xml_is_extender(c as u32))
    {
        len += l;
        if len > max_length {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrNameTooLong,
                "name too long",
                None,
                None,
            );
            return null_mut();
        }
        NEXTL!(ctxt, l);
        c = CUR_CHAR!(ctxt, l);
        if (*(*ctxt).input).base != base {
            // We changed encoding from an unknown encoding
            // Input buffer changed location, so we better start again
            return html_parse_name_complex(ctxt);
        }
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return null_mut();
    }

    if (*(*ctxt).input).offset_from_base() < len as usize {
        // Sanity check
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            "unexpected change of input buffer",
            None,
            None,
        );
        return null_mut();
    }

    xml_dict_lookup((*ctxt).dict, (*(*ctxt).input).cur.sub(len as usize), len)
}

/// Parse an HTML name, this routine is case sensitive.
///
/// Returns the Name parsed or NULL
#[doc(alias = "htmlParseName")]
unsafe fn html_parse_name(ctxt: HtmlParserCtxtPtr) -> *const XmlChar {
    let mut input: *const XmlChar;
    let ret: *const XmlChar;
    let count: i32;

    (*ctxt).grow();

    // Accelerator for simple ASCII names
    input = (*(*ctxt).input).cur;
    if (*input >= 0x61 && *input <= 0x7A)
        || (*input >= 0x41 && *input <= 0x5A)
        || *input == b'_'
        || *input == b':'
    {
        input = input.add(1);
        while (*input >= 0x61 && *input <= 0x7A)
            || (*input >= 0x41 && *input <= 0x5A)
            || (*input >= 0x30 && *input <= 0x39)
            || *input == b'_'
            || *input == b'-'
            || *input == b':'
            || *input == b'.'
        {
            input = input.add(1);
        }

        if input == (*(*ctxt).input).end {
            return null_mut();
        }

        if *input > 0 && *input < 0x80 {
            count = input.offset_from((*(*ctxt).input).cur) as _;
            ret = xml_dict_lookup((*ctxt).dict, (*(*ctxt).input).cur, count);
            (*(*ctxt).input).cur = input;
            (*(*ctxt).input).col += count;
            return ret;
        }
    }
    html_parse_name_complex(ctxt)
}

/// Parse an HTML ENTITY references
///
/// [68] EntityRef ::= b'&' Name ';'
///
/// Returns the associated htmlEntityDescPtr if found, or NULL otherwise,
///         if non-NULL *str will have to be freed by the caller.
#[doc(alias = "htmlParseEntityRef")]
pub(crate) unsafe fn html_parse_entity_ref(
    ctxt: HtmlParserCtxtPtr,
    str: *mut *const XmlChar,
) -> *const HtmlEntityDesc {
    let name: *const XmlChar;
    let mut ent: *const HtmlEntityDesc = null();

    if !str.is_null() {
        *str = null_mut();
    }
    if ctxt.is_null() || (*ctxt).input.is_null() {
        return null_mut();
    }

    if (*ctxt).current_byte() == b'&' {
        (*ctxt).skip_char();
        name = html_parse_name(ctxt);
        if name.is_null() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrNameRequired,
                "htmlParseEntityRef: no name\n",
                None,
                None,
            );
        } else {
            (*ctxt).grow();
            if (*ctxt).current_byte() == b';' {
                if !str.is_null() {
                    *str = name;
                }

                // Lookup the entity in the table.
                ent = html_entity_lookup(name);
                if !ent.is_null() {
                    /* OK that's ugly !!! */
                    (*ctxt).skip_char();
                }
            } else {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrEntityRefSemicolMissing,
                    "htmlParseEntityRef: expecting ';'\n",
                    None,
                    None,
                );
                if !str.is_null() {
                    *str = name;
                }
            }
        }
    }
    ent
}

/// Parse Reference declarations
///
/// `[66] CharRef ::= b'&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';'`
///
/// Returns the value parsed (as an int)
#[doc(alias = "htmlParseCharRef")]
pub(crate) unsafe fn html_parse_char_ref(ctxt: HtmlParserCtxtPtr) -> i32 {
    let mut val: i32 = 0;

    if ctxt.is_null() || (*ctxt).input.is_null() {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            "htmlParseCharRef: context error\n",
            None,
            None,
        );
        return 0;
    }
    if (*ctxt).current_byte() == b'&'
        && NXT!(ctxt, 1) == b'#'
        && (NXT!(ctxt, 2) == b'x' || NXT!(ctxt, 2) == b'X')
    {
        (*ctxt).advance(3);
        #[allow(clippy::while_immutable_condition)]
        while (*ctxt).current_byte() != b';' {
            if (*ctxt).current_byte() >= b'0' && (*ctxt).current_byte() <= b'9' {
                if val < 0x110000 {
                    val = val * 16 + ((*ctxt).current_byte() - b'0') as i32;
                }
            } else if (*ctxt).current_byte() >= b'a' && (*ctxt).current_byte() <= b'f' {
                if val < 0x110000 {
                    val = val * 16 + ((*ctxt).current_byte() - b'a') as i32 + 10;
                }
            } else if (*ctxt).current_byte() >= b'A' && (*ctxt).current_byte() <= b'F' {
                if val < 0x110000 {
                    val = val * 16 + ((*ctxt).current_byte() - b'A') as i32 + 10;
                }
            } else {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidHexCharRef,
                    "htmlParseCharRef: missing semicolon\n",
                    None,
                    None,
                );
                break;
            }
            (*ctxt).skip_char();
        }
        if (*ctxt).current_byte() == b';' {
            (*ctxt).skip_char();
        }
    } else if (*ctxt).current_byte() == b'&' && NXT!(ctxt, 1) == b'#' {
        (*ctxt).advance(2);
        #[allow(clippy::while_immutable_condition)]
        while (*ctxt).current_byte() != b';' {
            if (*ctxt).current_byte() >= b'0' && (*ctxt).current_byte() <= b'9' {
                if val < 0x110000 {
                    val = val * 10 + ((*ctxt).current_byte() - b'0') as i32;
                }
            } else {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidDecCharRef,
                    "htmlParseCharRef: missing semicolon\n",
                    None,
                    None,
                );
                break;
            }
            (*ctxt).skip_char();
        }
        if (*ctxt).current_byte() == b';' {
            (*ctxt).skip_char();
        }
    } else {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrInvalidCharRef,
            "htmlParseCharRef: invalid value\n",
            None,
            None,
        );
    }
    // Check the value IS_CHAR ...
    if xml_is_char(val as u32) {
        return val;
    } else if val >= 0x110000 {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            "htmlParseCharRef: value too large\n",
            None,
            None,
        );
    } else {
        html_parse_err_int!(
            ctxt,
            XmlParserErrors::XmlErrInvalidChar,
            "htmlParseCharRef: invalid xmlChar value {}\n",
            val
        );
    }
    0
}

const HTML_MAX_NAMELEN: usize = 1000;
const HTML_PARSER_BIG_BUFFER_SIZE: usize = 1000;
const HTML_PARSER_BUFFER_SIZE: usize = 100;

/// Handle a redefinition of attribute error
#[doc(alias = "htmlErrMemory")]
pub(crate) unsafe fn html_err_memory(ctxt: XmlParserCtxtPtr, extra: Option<&str>) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = XmlParserErrors::XmlErrNoMemory as i32;
        (*ctxt).instate = XmlParserInputState::XmlParserEOF;
        (*ctxt).disable_sax = 1;
    }
    if let Some(extra) = extra {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            Some(extra.to_owned().into()),
            None,
            None,
            0,
            0,
            "Memory allocation failed : {}\n",
            extra
        );
    } else {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
            XmlParserErrors::XmlErrNoMemory,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            None,
            None,
            None,
            0,
            0,
            "Memory allocation failed\n",
        );
    }
}

/// Parse an HTML tag or attribute name, note that we convert it to lowercase
/// since HTML names are not case-sensitive.
///
/// Returns the Tag Name parsed or NULL
#[doc(alias = "htmlParseHTMLName")]
unsafe fn html_parse_html_name(ctxt: HtmlParserCtxtPtr) -> *const XmlChar {
    let mut i: usize = 0;
    let mut loc: [XmlChar; HTML_PARSER_BUFFER_SIZE] = [0; HTML_PARSER_BUFFER_SIZE];

    if !(*ctxt).current_byte().is_ascii_alphabetic()
        && (*ctxt).current_byte() != b'_'
        && (*ctxt).current_byte() != b':'
        && (*ctxt).current_byte() != b'.'
    {
        return null_mut();
    }

    while i < HTML_PARSER_BUFFER_SIZE
        && ((*ctxt).current_byte().is_ascii_alphabetic()
            || (*ctxt).current_byte().is_ascii_digit()
            || (*ctxt).current_byte() == b':'
            || (*ctxt).current_byte() == b'-'
            || (*ctxt).current_byte() == b'_'
            || (*ctxt).current_byte() == b'.')
    {
        if (*ctxt).current_byte() >= b'A' && (*ctxt).current_byte() <= b'Z' {
            loc[i] = (*ctxt).current_byte() + 0x20;
        } else {
            loc[i] = (*ctxt).current_byte();
        }
        i += 1;

        (*ctxt).skip_char();
    }

    let ret: *const XmlChar = xml_dict_lookup((*ctxt).dict, loc.as_ptr() as _, i as i32);
    if ret.is_null() {
        html_err_memory(ctxt, None);
    }

    ret
}

/// Pops the top element name from the name stack
///
/// Returns the name just removed
#[doc(alias = "htmlnamePop")]
unsafe fn html_name_pop(ctxt: HtmlParserCtxtPtr) -> *const XmlChar {
    let res = (*ctxt).name_tab.pop().unwrap_or(null());
    let name = *(*ctxt).name_tab.last().unwrap_or(&null());
    (*ctxt).name = (!name.is_null()).then(|| {
        CStr::from_ptr(name as *const i8)
            .to_string_lossy()
            .into_owned()
    });
    res
}

/// Close all remaining tags at the end of the stream
#[doc(alias = "htmlAutoCloseOnEnd")]
unsafe fn html_auto_close_on_end(ctxt: HtmlParserCtxtPtr) {
    if (*ctxt).name_tab.is_empty() {
        return;
    }
    for _ in (0..(*ctxt).name_tab.len()).rev() {
        if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
            let name = (*ctxt).name.as_deref().unwrap();
            end_element((*ctxt).user_data.clone(), name);
        }
        html_name_pop(ctxt);
    }
}

/// The HTML DTD allows a tag to implicitly close other tags.
/// The list is kept in htmlStartClose array.
/// This function is called when a new tag has been detected and generates the
/// appropriates closes if possible/needed.
/// If newtag is NULL this mean we are at the end of the resource
/// and we should check
#[doc(alias = "htmlAutoClose")]
unsafe fn html_auto_close(ctxt: HtmlParserCtxtPtr, newtag: *const XmlChar) {
    while !newtag.is_null()
        && (*ctxt)
            .name
            .as_deref()
            .map_or(false, |name| html_check_auto_close(newtag, Some(name)) != 0)
    {
        if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
            let name = (*ctxt).name.as_deref().unwrap();
            end_element((*ctxt).user_data.clone(), name);
        }
        html_name_pop(ctxt);
    }
    if newtag.is_null() {
        html_auto_close_on_end(ctxt);
        return;
    }
    while newtag.is_null()
        && (*ctxt).name.as_deref().map_or(false, |name| {
            name == "head" || name == "body" || name == "html"
        })
    {
        if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
            let name = (*ctxt).name.as_deref().unwrap();
            end_element((*ctxt).user_data.clone(), name);
        }
        html_name_pop(ctxt);
    }
}

static HTML_OMITTED_DEFAULT_VALUE: AtomicI32 = AtomicI32::new(1);

/// Pushes a new element name on top of the name stack
///
/// Returns -1 in case of error, the index in the stack otherwise
#[doc(alias = "htmlnamePush")]
unsafe fn html_name_push(ctxt: HtmlParserCtxtPtr, value: *const XmlChar) -> i32 {
    if (*ctxt).html < 3 && xml_str_equal(value, c"head".as_ptr() as _) {
        (*ctxt).html = 3;
    }
    if (*ctxt).html < 10 && xml_str_equal(value, c"body".as_ptr() as _) {
        (*ctxt).html = 10;
    }
    (*ctxt).name = (!value.is_null()).then(|| {
        CStr::from_ptr(value as *const i8)
            .to_string_lossy()
            .into_owned()
    });
    (*ctxt).name_tab.push(value);
    (*ctxt).name_tab.len() as i32 - 1
}

/// The HTML DTD allows a tag to exists only implicitly
/// called when a new tag has been detected and generates the
/// appropriates implicit tags if missing
#[doc(alias = "htmlCheckImplied")]
unsafe fn html_check_implied(ctxt: HtmlParserCtxtPtr, newtag: *const XmlChar) {
    if (*ctxt).options & HtmlParserOption::HtmlParseNoimplied as i32 != 0 {
        return;
    }
    if HTML_OMITTED_DEFAULT_VALUE.load(Ordering::Relaxed) == 0 {
        return;
    }
    if xml_str_equal(newtag, c"html".as_ptr() as _) {
        return;
    }
    if (*ctxt).name_tab.is_empty() {
        html_name_push(ctxt, c"html".as_ptr() as _);
        if let Some(start_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element) {
            start_element((*ctxt).user_data.clone(), "html", &[]);
        }
    }
    if xml_str_equal(newtag, c"body".as_ptr() as _) || xml_str_equal(newtag, c"head".as_ptr() as _)
    {
        return;
    }
    if (*ctxt).name_tab.len() <= 1
        && (xml_str_equal(newtag, c"script".as_ptr() as _)
            || xml_str_equal(newtag, c"style".as_ptr() as _)
            || xml_str_equal(newtag, c"meta".as_ptr() as _)
            || xml_str_equal(newtag, c"link".as_ptr() as _)
            || xml_str_equal(newtag, c"title".as_ptr() as _)
            || xml_str_equal(newtag, c"base".as_ptr() as _))
    {
        if (*ctxt).html >= 3 {
            // we already saw or generated an <head> before
            return;
        }
        // dropped OBJECT ... i you put it first BODY will be assumed !
        html_name_push(ctxt, c"head".as_ptr() as _);
        if let Some(start_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element) {
            start_element((*ctxt).user_data.clone(), "head", &[]);
        }
    } else if !xml_str_equal(newtag, c"noframes".as_ptr() as _)
        && !xml_str_equal(newtag, c"frame".as_ptr() as _)
        && !xml_str_equal(newtag, c"frameset".as_ptr() as _)
    {
        if (*ctxt).html >= 10 {
            // we already saw or generated a <body> before
            return;
        }
        for i in 0..(*ctxt).name_tab.len() {
            if xml_str_equal((*ctxt).name_tab[i], c"body".as_ptr() as _) {
                return;
            }
            if xml_str_equal((*ctxt).name_tab[i], c"head".as_ptr() as _) {
                return;
            }
        }

        html_name_push(ctxt, c"body".as_ptr() as _);
        if let Some(start_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element) {
            start_element((*ctxt).user_data.clone(), "body", &[]);
        }
    }
}

// Macro used to grow the current buffer.
macro_rules! grow_buffer {
    ($ctxt:expr, $buffer:expr, $buffer_size:expr) => {
        $buffer_size *= 2;
        let tmp: *mut XmlChar = xml_realloc($buffer as _, $buffer_size as usize) as _;
        if tmp.is_null() {
            html_err_memory($ctxt, Some("growing buffer\n"));
            xml_free($buffer as _);
            return null_mut();
        }
        $buffer = tmp;
    };
}

/// Parse an HTML attribute value till the stop (quote),
/// if stop is 0 then it stops at the first space
///
/// Returns the attribute parsed or NULL
#[doc(alias = "htmlParseHTMLAttribute")]
unsafe fn html_parse_html_attribute(ctxt: HtmlParserCtxtPtr, stop: XmlChar) -> *mut XmlChar {
    let mut buffer: *mut XmlChar;
    let mut buffer_size: i32;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH as i32
    } else {
        XML_MAX_TEXT_LENGTH as i32
    };
    let mut out: *mut XmlChar;
    let mut name: *const XmlChar = null_mut();
    let mut cur: *const XmlChar;
    let mut ent: *const HtmlEntityDesc;

    // allocate a translation buffer.
    buffer_size = HTML_PARSER_BUFFER_SIZE as _;
    buffer = xml_malloc_atomic(buffer_size as usize) as _;
    if buffer.is_null() {
        html_err_memory(ctxt, Some("buffer allocation failed\n"));
        return null_mut();
    }
    out = buffer;

    // Ok loop until we reach one of the ending chars
    while (*ctxt).current_byte() != 0 && (*ctxt).current_byte() != stop {
        if stop == 0 && (*ctxt).current_byte() == b'>' {
            break;
        }
        if stop == 0 && xml_is_blank_char((*ctxt).current_byte() as u32) {
            break;
        }
        if (*ctxt).current_byte() == b'&' {
            if NXT!(ctxt, 1) == b'#' {
                let mut bits: i32;

                let c: u32 = html_parse_char_ref(ctxt) as _;
                if c < 0x80 {
                    *out = c as _;
                    out = out.add(1);
                    bits = -6;
                } else if c < 0x800 {
                    *out = ((c >> 6) & 0x1F) as u8 | 0xC0;
                    out = out.add(1);
                    bits = 0;
                } else if c < 0x10000 {
                    *out = ((c >> 12) & 0x0F) as u8 | 0xE0;
                    out = out.add(1);
                    bits = 6;
                } else {
                    *out = ((c >> 18) & 0x07) as u8 | 0xF0;
                    out = out.add(1);
                    bits = 12;
                }

                while bits >= 0 {
                    *out = ((c >> bits) & 0x3F) as u8 | 0x80;
                    out = out.add(1);
                    bits -= 6;
                }

                if out.offset_from(buffer) > buffer_size as isize - 100 {
                    let indx: i32 = out.offset_from(buffer) as _;

                    grow_buffer!(ctxt, buffer, buffer_size);
                    out = buffer.add(indx as usize) as _;
                }
            } else {
                ent = html_parse_entity_ref(ctxt, addr_of_mut!(name));
                if name.is_null() {
                    *out = b'&';
                    out = out.add(1);
                    if out.offset_from(buffer) > buffer_size as isize - 100 {
                        let indx: i32 = out.offset_from(buffer) as i32;

                        grow_buffer!(ctxt, buffer, buffer_size);
                        out = buffer.add(indx as usize) as _;
                    }
                } else if ent.is_null() {
                    *out = b'&';
                    out = out.add(1);
                    cur = name;
                    while *cur != 0 {
                        if out.offset_from(buffer) > buffer_size as isize - 100 {
                            let indx: i32 = out.offset_from(buffer) as i32;

                            grow_buffer!(ctxt, buffer, buffer_size);
                            out = buffer.add(indx as usize) as _;
                        }
                        *out = *cur;
                        out = out.add(1);
                        cur = cur.add(1);
                    }
                } else {
                    let mut bits: i32;

                    if out.offset_from(buffer) > buffer_size as isize - 100 {
                        let indx: i32 = out.offset_from(buffer) as i32;

                        grow_buffer!(ctxt, buffer, buffer_size);
                        out = buffer.add(indx as usize) as _;
                    }
                    let c: u32 = (*ent).value;
                    if c < 0x80 {
                        *out = c as _;
                        out = out.add(1);
                        bits = -6;
                    } else if c < 0x800 {
                        *out = ((c >> 6) & 0x1F) as u8 | 0xC0;
                        out = out.add(1);
                        bits = 0;
                    } else if c < 0x10000 {
                        *out = ((c >> 12) & 0x0F) as u8 | 0xE0;
                        out = out.add(1);
                        bits = 6;
                    } else {
                        *out = ((c >> 18) & 0x07) as u8 | 0xF0;
                        out = out.add(1);
                        bits = 12;
                    }

                    while bits >= 0 {
                        *out = ((c >> bits) & 0x3F) as u8 | 0x80;
                        out = out.add(1);
                        bits -= 6;
                    }
                }
            }
        } else {
            let mut bits: i32;
            let mut l: i32 = 0;

            if out.offset_from(buffer) > buffer_size as isize - 100 {
                let indx: i32 = out.offset_from(buffer) as i32;

                grow_buffer!(ctxt, buffer, buffer_size);
                out = buffer.add(indx as usize) as _;
            }
            let c: u32 = CUR_CHAR!(ctxt, l) as _;
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                xml_free(buffer as _);
                return null_mut();
            }
            if c < 0x80 {
                *out = c as _;
                out = out.add(1);
                bits = -6;
            } else if c < 0x800 {
                *out = ((c >> 6) & 0x1F) as u8 | 0xC0;
                out = out.add(1);
                bits = 0;
            } else if c < 0x10000 {
                *out = ((c >> 12) & 0x0F) as u8 | 0xE0;
                out = out.add(1);
                bits = 6;
            } else {
                *out = ((c >> 18) & 0x07) as u8 | 0xF0;
                out = out.add(1);
                bits = 12;
            }

            while bits >= 0 {
                *out = ((c >> bits) & 0x3F) as u8 | 0x80;
                out = out.add(1);
                bits -= 6;
            }
            NEXTL!(ctxt, l);
        }
        if out.offset_from(buffer) > max_length as isize {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrAttributeNotFinished,
                "attribute value too long\n",
                None,
                None,
            );
            xml_free(buffer as _);
            return null_mut();
        }
    }
    *out = 0;
    buffer
}

/// parse a value for an attribute
///
/// # Note
/// The parser won't do substitution of entities here, this
/// will be handled later in xmlStringGetNodeList, unless it was
/// asked for (*ctxt).replaceEntities != 0
///
/// Returns the AttValue parsed or NULL.
#[doc(alias = "htmlParseAttValue")]
unsafe fn html_parse_att_value(ctxt: HtmlParserCtxtPtr) -> *mut XmlChar {
    let ret: *mut XmlChar;

    if (*ctxt).current_byte() == b'"' {
        (*ctxt).skip_char();
        ret = html_parse_html_attribute(ctxt, b'"');
        if (*ctxt).current_byte() != b'"' {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrAttributeNotFinished,
                "AttValue: \" expected\n",
                None,
                None,
            );
        } else {
            (*ctxt).skip_char();
        }
    } else if (*ctxt).current_byte() == b'\'' {
        (*ctxt).skip_char();
        ret = html_parse_html_attribute(ctxt, b'\'');
        if (*ctxt).current_byte() != b'\'' {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrAttributeNotFinished,
                "AttValue: ' expected\n",
                None,
                None,
            );
        } else {
            (*ctxt).skip_char();
        }
    } else {
        // That's an HTMLism, the attribute value may not be quoted
        ret = html_parse_html_attribute(ctxt, 0);
        if ret.is_null() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrAttributeWithoutValue,
                "AttValue: no value found\n",
                None,
                None,
            );
        }
    }
    ret
}

/// Parse an attribute
///
/// `[41] Attribute ::= Name Eq AttValue`
///
/// `[25] Eq ::= S? '=' S?`
///
/// With namespace:
///
/// `[NS 11] Attribute ::= QName Eq AttValue`
///
/// Also the case QName == xmlns:??? is handled independently as a namespace definition.
///
/// Returns the attribute name, and the value in *value.
#[doc(alias = "htmlParseAttribute")]
unsafe fn html_parse_attribute(
    ctxt: HtmlParserCtxtPtr,
    value: *mut *mut XmlChar,
) -> *const XmlChar {
    let mut val: *mut XmlChar = null_mut();

    *value = null_mut();
    let name: *const XmlChar = html_parse_html_name(ctxt);
    if name.is_null() {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            "error parsing attribute name\n",
            None,
            None,
        );
        return null_mut();
    }

    // read the value
    SKIP_BLANKS!(ctxt);
    if (*ctxt).current_byte() == b'=' {
        (*ctxt).skip_char();
        SKIP_BLANKS!(ctxt);
        val = html_parse_att_value(ctxt);
    }

    *value = val;
    name
}

/// Checks an attribute value to detect the encoding
/// If a new encoding is detected the parser is switched to decode it and pass UTF8
#[doc(alias = "htmlCheckEncodingDirect")]
unsafe fn html_check_encoding_direct(ctxt: HtmlParserCtxtPtr, encoding: Option<&str>) {
    if ctxt.is_null()
        || encoding.is_none()
        || (*ctxt).options & HtmlParserOption::HtmlParseIgnoreEnc as i32 != 0
    {
        return;
    }

    // do not change encoding
    if (*(*ctxt).input).encoding.is_some() {
        return;
    }

    if let Some(mut encoding) = encoding {
        encoding = encoding.trim_start_matches([' ', '\t']);
        (*(*ctxt).input).encoding = Some(encoding.to_owned());

        let enc = encoding
            .parse::<XmlCharEncoding>()
            .unwrap_or(XmlCharEncoding::Error);
        // registered set of known encodings
        if !matches!(enc, XmlCharEncoding::Error) {
            if matches!(
                enc,
                XmlCharEncoding::UTF16LE
                    | XmlCharEncoding::UTF16BE
                    | XmlCharEncoding::UCS4LE
                    | XmlCharEncoding::UCS4BE
            ) && (*(*ctxt).input).buf.is_some()
                && (*(*ctxt).input)
                    .buf
                    .as_ref()
                    .unwrap()
                    .borrow()
                    .encoder
                    .is_none()
            {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidEncoding,
                    "htmlCheckEncoding: wrong encoding meta\n",
                    None,
                    None,
                );
            } else {
                (*ctxt).switch_encoding(enc);
            }
            (*ctxt).charset = XmlCharEncoding::UTF8;
        } else {
            // fallback for unknown encodings
            if let Some(handler) = find_encoding_handler(encoding) {
                (*ctxt).switch_to_encoding(handler);
                (*ctxt).charset = XmlCharEncoding::UTF8;
            } else {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrUnsupportedEncoding,
                    format!("htmlCheckEncoding: unknown encoding {encoding}\n").as_str(),
                    Some(encoding),
                    None,
                );
            }
        }

        if (*(*ctxt).input).buf.is_some()
            && (*(*ctxt).input)
                .buf
                .as_ref()
                .unwrap()
                .borrow()
                .encoder
                .is_some()
            && (*(*ctxt).input)
                .buf
                .as_ref()
                .unwrap()
                .borrow()
                .raw
                .is_some()
            && (*(*ctxt).input)
                .buf
                .as_ref()
                .unwrap()
                .borrow()
                .buffer
                .is_some()
        {
            // convert as much as possible to the parser reading buffer.
            let processed = (*(*ctxt).input).offset_from_base();
            (*(*ctxt).input)
                .buf
                .as_mut()
                .unwrap()
                .borrow_mut()
                .buffer
                .unwrap()
                .trim_head(processed);
            let res = (*(*ctxt).input)
                .buf
                .as_mut()
                .unwrap()
                .borrow_mut()
                .decode(true);
            (*(*ctxt).input).reset_base();
            if res.is_err() {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidEncoding,
                    "htmlCheckEncoding: encoder error\n",
                    None,
                    None,
                );
            }
        }
    }
}

/// Checks an http-equiv attribute from a Meta tag to detect the encoding
/// If a new encoding is detected the parser is switched to decode it and pass UTF8
#[doc(alias = "htmlCheckEncoding")]
unsafe fn html_check_encoding(ctxt: HtmlParserCtxtPtr, attvalue: &str) {
    let mut encoding = attvalue;
    let Some(pos) = attvalue
        .as_bytes()
        .windows(7)
        .position(|v| v.eq_ignore_ascii_case(b"charset"))
    else {
        return;
    };
    encoding = &encoding[pos + 7..];
    encoding = encoding.trim_start_matches(|c| xml_is_blank_char(c as u32));
    if let Some(encoding) = encoding.strip_prefix('=') {
        html_check_encoding_direct(ctxt, Some(encoding));
    }
}

/// Checks an attributes from a Meta tag
#[doc(alias = "htmlCheckMeta")]
unsafe fn html_check_meta(ctxt: HtmlParserCtxtPtr, atts: &[(String, Option<String>)]) {
    let mut http: i32 = 0;

    if ctxt.is_null() {
        return;
    }

    let mut content = None;
    for (att, value) in atts {
        if value
            .as_deref()
            .map_or(false, |v| v.eq_ignore_ascii_case("Content-Type"))
            && att.eq_ignore_ascii_case("http-equiv")
        {
            http = 1;
        } else if value.is_some() && att.eq_ignore_ascii_case("charset") {
            html_check_encoding_direct(ctxt, value.as_deref());
        } else if value.is_some() && att.eq_ignore_ascii_case("content") {
            content = value.as_deref();
        }
    }
    if let Some(content) = content.filter(|_| http != 0) {
        html_check_encoding(ctxt, content);
    }
}

/// Parse a start of tag either for rule element or EmptyElement.  
/// In both case we don't parse the tag closing chars.
///
/// `[40] STag ::= b'<' Name (S Attribute)* S? '>'`
///
/// `[44] EmptyElemTag ::= b'<' Name (S Attribute)* S? '/>'`
///
/// With namespace:
///
/// `[NS 8] STag ::= b'<' QName (S Attribute)* S? '>'`
///
/// `[NS 10] EmptyElement ::= b'<' QName (S Attribute)* S? '/>'`
///
/// Returns 0 in case of success, -1 in case of error and 1 if discarded
#[doc(alias = "htmlParseStartTag")]
unsafe fn html_parse_start_tag(ctxt: HtmlParserCtxtPtr) -> i32 {
    let mut attname: *const XmlChar;
    let mut attvalue: *mut XmlChar = null_mut();
    let mut meta: i32 = 0;
    let mut discardtag: i32 = 0;

    if ctxt.is_null() || (*ctxt).input.is_null() {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            "htmlParseStartTag: context error\n",
            None,
            None,
        );
        return -1;
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return -1;
    }
    if (*ctxt).current_byte() != b'<' {
        return -1;
    }
    (*ctxt).skip_char();

    (*ctxt).grow();
    let name: *const XmlChar = html_parse_html_name(ctxt);
    if name.is_null() {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            "htmlParseStartTag: invalid element name\n",
            None,
            None,
        );
        // Dump the bogus tag like browsers do
        #[allow(clippy::while_immutable_condition)]
        while (*ctxt).current_byte() != 0
            && (*ctxt).current_byte() != b'>'
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            (*ctxt).skip_char();
        }
        return -1;
    }
    if xml_str_equal(name, c"meta".as_ptr() as _) {
        meta = 1;
    }

    // Check for auto-closure of HTML elements.
    html_auto_close(ctxt, name);

    // Check for implied HTML elements.
    html_check_implied(ctxt, name);

    // Avoid html at any level > 0, head at any level != 1
    // or any attempt to recurse body
    if !(*ctxt).name_tab.is_empty() && xml_str_equal(name, c"html".as_ptr() as _) {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlHTMLStrucureError,
            "htmlParseStartTag: misplaced <html> tag\n",
            Some(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref()),
            None,
        );
        discardtag = 1;
        (*ctxt).depth += 1;
    }
    if (*ctxt).name_tab.len() != 1 && xml_str_equal(name, c"head".as_ptr() as _) {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlHTMLStrucureError,
            "htmlParseStartTag: misplaced <head> tag\n",
            Some(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref()),
            None,
        );
        discardtag = 1;
        (*ctxt).depth += 1;
    }
    if xml_str_equal(name, c"body".as_ptr() as _) {
        for indx in 0..(*ctxt).name_tab.len() {
            if xml_str_equal((*ctxt).name_tab[indx], c"body".as_ptr() as _) {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlHTMLStrucureError,
                    "htmlParseStartTag: misplaced <body> tag\n",
                    Some(CStr::from_ptr(name as *const i8).to_string_lossy().as_ref()),
                    None,
                );
                discardtag = 1;
                (*ctxt).depth += 1;
            }
        }
    }

    // Now parse the attributes, it ends up with the ending
    //
    // (S Attribute)* S?
    SKIP_BLANKS!(ctxt);
    'failed: while (*ctxt).current_byte() != 0
        && (*ctxt).current_byte() != b'>'
        && ((*ctxt).current_byte() != b'/' || NXT!(ctxt, 1) != b'>')
        && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        (*ctxt).grow();
        attname = html_parse_attribute(ctxt, addr_of_mut!(attvalue));
        if !attname.is_null() {
            let attname = CStr::from_ptr(attname as *const i8)
                .to_string_lossy()
                .into_owned();
            // Well formedness requires at most one declaration of an attribute
            for i in 0..(*ctxt).atts.len() {
                let (name, _) = &(*ctxt).atts[i];
                if name.as_str() == attname {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrAttributeRedefined,
                        format!("Attribute {attname} redefined\n").as_str(),
                        Some(&attname),
                        None,
                    );
                    if !attvalue.is_null() {
                        xml_free(attvalue as _);
                    }
                    // goto failed;
                    SKIP_BLANKS!(ctxt);
                    continue 'failed;
                }
            }

            // Add the pair to atts
            let value = (!attvalue.is_null()).then(|| {
                CStr::from_ptr(attvalue as *const i8)
                    .to_string_lossy()
                    .into_owned()
            });
            (*ctxt).atts.push((attname, value));
            xml_free(attvalue as _);
        } else {
            if !attvalue.is_null() {
                xml_free(attvalue as _);
            }
            // Dump the bogus attribute string up to the next blank or the end of the tag.
            while (*ctxt).current_byte() != 0
                && !xml_is_blank_char((*ctxt).current_byte() as u32)
                && (*ctxt).current_byte() != b'>'
                && ((*ctxt).current_byte() != b'/' || NXT!(ctxt, 1) != b'>')
                && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
            {
                (*ctxt).skip_char();
            }
        }
        // failed:
        SKIP_BLANKS!(ctxt);
    }

    // Handle specific association to the META tag
    if meta != 0 && !(*ctxt).atts.is_empty() {
        html_check_meta(ctxt, &(*ctxt).atts);
    }

    // SAX: Start of Element !
    if discardtag == 0 {
        html_name_push(ctxt, name);
        if let Some(start_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element) {
            let name = CStr::from_ptr(name as *const i8).to_string_lossy();
            start_element((*ctxt).user_data.clone(), &name, &(*ctxt).atts);
        }
    }

    (*ctxt).atts.clear();
    discardtag
}

/// This table is used by the htmlparser to know what to do with broken html pages.  
/// By assigning different priorities to different elements
/// the parser can decide how to handle extra endtags.  
/// Endtags are only allowed to close elements with lower or equal priority.
#[repr(C)]
pub struct ElementPriority {
    name: *const c_char,
    priority: i32,
}

const HTML_END_PRIORITY: &[ElementPriority] = &[
    ElementPriority {
        name: c"div".as_ptr(),
        priority: 150,
    },
    ElementPriority {
        name: c"td".as_ptr(),
        priority: 160,
    },
    ElementPriority {
        name: c"th".as_ptr(),
        priority: 160,
    },
    ElementPriority {
        name: c"tr".as_ptr(),
        priority: 170,
    },
    ElementPriority {
        name: c"thead".as_ptr(),
        priority: 180,
    },
    ElementPriority {
        name: c"tbody".as_ptr(),
        priority: 180,
    },
    ElementPriority {
        name: c"tfoot".as_ptr(),
        priority: 180,
    },
    ElementPriority {
        name: c"table".as_ptr(),
        priority: 190,
    },
    ElementPriority {
        name: c"head".as_ptr(),
        priority: 200,
    },
    ElementPriority {
        name: c"body".as_ptr(),
        priority: 200,
    },
    ElementPriority {
        name: c"html".as_ptr(),
        priority: 220,
    },
    ElementPriority {
        name: null(),
        priority: 100,
    }, /* Default priority */
];

/// Return value: The "endtag" priority.
#[doc(alias = "htmlGetEndPriority")]
unsafe fn html_get_end_priority(name: *const XmlChar) -> i32 {
    let mut i: usize = 0;

    while !HTML_END_PRIORITY[i].name.is_null()
        && !xml_str_equal(HTML_END_PRIORITY[i].name as _, name)
    {
        i += 1;
    }

    HTML_END_PRIORITY[i].priority
}

/// The HTML DTD allows an ending tag to implicitly close other tags.
#[doc(alias = "htmlAutoCloseOnClose")]
unsafe fn html_auto_close_on_close(ctxt: HtmlParserCtxtPtr, newtag: *const XmlChar) {
    let priority: i32 = html_get_end_priority(newtag);

    for i in (0..(*ctxt).name_tab.len()).rev() {
        if xml_str_equal(newtag, (*ctxt).name_tab[i]) {
            let newtag = CStr::from_ptr(newtag as *const i8).to_string_lossy();
            while Some(newtag.as_ref()) != (*ctxt).name.as_deref() {
                let info = (*ctxt)
                    .name
                    .as_deref()
                    .and_then(|name| html_tag_lookup(name));
                if info.filter(|info| info.end_tag == 3).is_some() {
                    let name = (*ctxt).name.as_deref().unwrap();
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrTagNameMismatch,
                        format!("Opening and ending tag mismatch: {newtag} and {name}\n").as_str(),
                        Some(&newtag),
                        Some(name),
                    );
                }
                if let Some(end_element) =
                    (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element)
                {
                    let name = (*ctxt).name.as_deref().unwrap();
                    end_element((*ctxt).user_data.clone(), name);
                }
                html_name_pop(ctxt);
            }

            return;
        }
        // A misplaced endtag can only close elements with lower
        // or equal priority, so if we find an element with higher
        // priority before we find an element with
        // matching name, we just ignore this endtag
        if html_get_end_priority((*ctxt).name_tab[i]) > priority {
            return;
        }
    }
}

/// Pops the top element name from the node info stack
///
/// Returns 0 in case of error, the pointer to NodeInfo otherwise
#[doc(alias = "htmlNodeInfoPop")]
unsafe fn html_node_info_pop(ctxt: HtmlParserCtxtPtr) -> Option<Rc<RefCell<HtmlParserNodeInfo>>> {
    (*ctxt).node_info_tab.pop()
}

/// Parse an end of tag
///
/// `[42] ETag ::= b'</' Name S? '>'`
///
/// With namespace
///
/// `[NS 9] ETag ::= b'</' QName S? '>'`
///
/// Returns 1 if the current level should be closed.
#[doc(alias = "htmlParseEndTag")]
unsafe fn html_parse_end_tag(ctxt: HtmlParserCtxtPtr) -> i32 {
    let ret: i32;

    if (*ctxt).current_byte() != b'<' || NXT!(ctxt, 1) != b'/' {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrLtSlashRequired,
            "htmlParseEndTag: '</' not found\n",
            None,
            None,
        );
        return 0;
    }
    (*ctxt).advance(2);

    let name: *const XmlChar = html_parse_html_name(ctxt);
    if name.is_null() {
        return 0;
    }
    // We should definitely be at the ending "S? '>'" part
    SKIP_BLANKS!(ctxt);
    if (*ctxt).current_byte() != b'>' {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrGtRequired,
            "End tag : expected '>'\n",
            None,
            None,
        );
        // Skip to next '>'
        #[allow(clippy::while_immutable_condition)]
        while (*ctxt).current_byte() != 0 && (*ctxt).current_byte() != b'>' {
            (*ctxt).skip_char();
        }
    }
    if (*ctxt).current_byte() == b'>' {
        (*ctxt).skip_char();
    }

    // if we ignored misplaced tags in htmlParseStartTag don't pop them out now.
    if (*ctxt).depth > 0
        && (xml_str_equal(name as _, c"html".as_ptr() as _)
            || xml_str_equal(name as _, c"body".as_ptr() as _)
            || xml_str_equal(name as _, c"head".as_ptr() as _))
    {
        (*ctxt).depth -= 1;
        return 0;
    }

    // If the name read is not one of the element in the parsing stack
    // then return, it's just an error.
    for i in (0..(*ctxt).name_tab.len()).rev() {
        if xml_str_equal(name, (*ctxt).name_tab[i]) {
            // Check for auto-closure of HTML elements.

            html_auto_close_on_close(ctxt, name);

            let name = CStr::from_ptr(name as *const i8).to_string_lossy();
            // Well formedness constraints, opening and closing must match.
            // With the exception that the autoclose may have popped stuff out of the stack.
            if let Some(ctxt_name) = (*ctxt)
                .name
                .as_deref()
                .filter(|&ctxt_name| ctxt_name != name)
            {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrTagNameMismatch,
                    format!("Opening and ending tag mismatch: {name} and {ctxt_name}\n").as_str(),
                    Some(&name),
                    Some(ctxt_name),
                );
            }

            // SAX: End of Tag
            let oldname = (*ctxt).name.as_deref();
            if oldname.map_or(false, |oldname| oldname == name) {
                if let Some(end_element) =
                    (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element)
                {
                    end_element((*ctxt).user_data.clone(), &name);
                }
                html_node_info_pop(ctxt);
                html_name_pop(ctxt);
                ret = 1;
            } else {
                ret = 0;
            }

            return ret;
        }
    }
    let name = CStr::from_ptr(name as *const i8).to_string_lossy();
    html_parse_err(
        ctxt,
        XmlParserErrors::XmlErrTagNameMismatch,
        format!("Unexpected end tag : {name}\n").as_str(),
        Some(&name),
        None,
    );
    0
}

/// Parse an HTML tag or attribute name, note that we convert it to lowercase
/// since HTML names are not case-sensitive, this doesn't consume the data
/// from the stream, it's a look-ahead
///
/// Returns the Tag Name parsed or NULL
#[doc(alias = "htmlParseHTMLName_nonInvasive")]
unsafe fn html_parse_html_name_non_invasive(ctxt: HtmlParserCtxtPtr) -> *const XmlChar {
    let mut i: usize = 0;
    let mut loc: [XmlChar; HTML_PARSER_BUFFER_SIZE] = [0; HTML_PARSER_BUFFER_SIZE];

    if !NXT!(ctxt, 1).is_ascii_alphabetic() && NXT!(ctxt, 1) != b'_' && NXT!(ctxt, 1) != b':' {
        return null_mut();
    }

    while i < HTML_PARSER_BUFFER_SIZE
        && (NXT!(ctxt, 1 + i).is_ascii_alphabetic()
            || NXT!(ctxt, 1 + i).is_ascii_digit()
            || NXT!(ctxt, 1 + i) == b':'
            || NXT!(ctxt, 1 + i) == b'-'
            || NXT!(ctxt, 1 + i) == b'_')
    {
        if NXT!(ctxt, 1 + i) >= b'A' && NXT!(ctxt, 1 + i) <= b'Z' {
            loc[i] = NXT!(ctxt, 1 + i) + 0x20;
        } else {
            loc[i] = NXT!(ctxt, 1 + i);
        }
        i += 1;
    }

    xml_dict_lookup((*ctxt).dict, loc.as_ptr(), i as _)
}

/// Parse the content of an HTML SCRIPT or STYLE element
/// http://www.w3.org/TR/html4/sgml/dtd.html#Script
/// http://www.w3.org/TR/html4/sgml/dtd.html#StyleSheet
/// http://www.w3.org/TR/html4/types.html#type-script
/// http://www.w3.org/TR/html4/types.html#h-6.15
/// http://www.w3.org/TR/html4/appendix/notes.html#h-B.3.2.1
///
/// Script data ( %Script; in the DTD) can be the content of the SCRIPT
/// element and the value of intrinsic event attributes. User agents must
/// not evaluate script data as HTML markup but instead must pass it on as
/// data to a script engine.
///
/// # Note
/// - The content is passed like CDATA
/// - the attributes for style and scripting "onXXX" are also described
///   as CDATA but SGML allows entities references in attributes so their
///   processing is identical as other attributes
#[doc(alias = "htmlParseScript")]
unsafe fn html_parse_script(ctxt: HtmlParserCtxtPtr) {
    let mut buf: [XmlChar; HTML_PARSER_BIG_BUFFER_SIZE + 5] = [0; HTML_PARSER_BIG_BUFFER_SIZE + 5];
    let mut nbchar: i32 = 0;
    let mut cur: i32;
    let mut l: i32 = 0;

    cur = CUR_CHAR!(ctxt, l);
    while cur != 0 {
        if cur == b'<' as i32 && NXT!(ctxt, 1) == b'/' {
            // One should break here, the specification is clear:
            // Authors should therefore escape "</" within the content.
            // Escape mechanisms are specific to each scripting or
            // style sheet language.
            //
            // In recovery mode, only break if end tag match the
            // current tag, effectively ignoring all tags inside the
            // script/style block and treating the entire block as
            // CDATA.
            if (*ctxt).recovery != 0 {
                let context_name = (*ctxt).name.as_deref().unwrap();
                let context_name = CString::new(context_name).unwrap();
                if xml_strncasecmp(
                    context_name.as_ptr() as *const u8,
                    (*(*ctxt).input).cur.add(2),
                    xml_strlen(context_name.as_ptr() as *const u8),
                ) == 0
                {
                    break;
                } else {
                    let name = (*ctxt).name.as_deref().unwrap();
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrTagNameMismatch,
                        format!("Element {name} embeds close tag\n").as_str(),
                        Some(name),
                        None,
                    );
                }
            } else if (NXT!(ctxt, 2) >= b'A' && NXT!(ctxt, 2) <= b'Z')
                || (NXT!(ctxt, 2) >= b'a' && NXT!(ctxt, 2) <= b'z')
            {
                break;
            }
        }
        if xml_is_char(cur as u32) {
            COPY_BUF!(ctxt, l, buf.as_mut_ptr(), nbchar, cur);
        } else {
            html_parse_err_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                "Invalid char in CDATA 0x{:X}\n",
                cur
            );
        }
        NEXTL!(ctxt, l);
        if nbchar >= HTML_PARSER_BIG_BUFFER_SIZE as i32 {
            buf[nbchar as usize] = 0;
            let s = from_utf8(&buf[..nbchar as usize]).expect("Internal Error");
            if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                if let Some(cdata_block) = sax.cdata_block {
                    // Insert as CDATA, which is the same as HTML_PRESERVE_NODE
                    cdata_block((*ctxt).user_data.clone(), s);
                } else if let Some(characters) = sax.characters {
                    characters((*ctxt).user_data.clone(), s);
                }
            }
            nbchar = 0;
            (*ctxt).shrink();
        }
        cur = CUR_CHAR!(ctxt, l);
    }

    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }

    if nbchar != 0 && (*ctxt).disable_sax == 0 {
        if let Some(sax) = (*ctxt).sax.as_deref_mut() {
            buf[nbchar as usize] = 0;
            let s = from_utf8(&buf[..nbchar as usize]).expect("Internal Error");
            if let Some(cdata_block) = sax.cdata_block {
                // Insert as CDATA, which is the same as HTML_PRESERVE_NODE
                cdata_block((*ctxt).user_data.clone(), s);
            } else if let Some(characters) = sax.characters {
                characters((*ctxt).user_data.clone(), s);
            }
        }
    }
}

/// Parse an HTML Literal
///
/// `[11] SystemLiteral ::= ('"' [^"]* '"') | ("'" [^']* "'")`
///
/// Returns the SystemLiteral parsed or NULL
#[doc(alias = "htmlParseSystemLiteral")]
unsafe fn html_parse_system_literal(ctxt: HtmlParserCtxtPtr) -> *mut XmlChar {
    let mut len: size_t = 0;
    let mut err: i32 = 0;

    let mut ret: *mut XmlChar = null_mut();

    if (*ctxt).current_byte() != b'"' && (*ctxt).current_byte() != b'\'' {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrLiteralNotStarted,
            "SystemLiteral \" or ' expected\n",
            None,
            None,
        );
        return null_mut();
    }
    let quote: i32 = (*ctxt).current_byte() as _;
    (*ctxt).skip_char();

    if (*ctxt).current_ptr() < (*ctxt).base_ptr() {
        return ret;
    }
    let start_position: size_t = (*ctxt).current_ptr().offset_from((*ctxt).base_ptr()) as _;

    #[allow(clippy::while_immutable_condition)]
    while (*ctxt).current_byte() != 0 && (*ctxt).current_byte() as i32 != quote {
        // TODO: Handle UTF-8
        if !xml_is_char((*ctxt).current_byte() as u32) {
            html_parse_err_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                "Invalid char in SystemLiteral 0x{:X}\n",
                (*ctxt).current_byte() as i32
            );
            err = 1;
        }
        (*ctxt).skip_char();
        len += 1;
    }
    if (*ctxt).current_byte() as i32 != quote {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrLiteralNotFinished,
            "Unfinished SystemLiteral\n",
            None,
            None,
        );
    } else {
        if err == 0 {
            ret = xml_strndup((*ctxt).base_ptr().add(start_position), len as _);
        }
        (*ctxt).skip_char();
    }

    ret
}

/// Parse an HTML public literal
///
/// `[12] PubidLiteral ::= '"' PubidChar* '"' | "'" (PubidChar - "'")* "'"`
///
/// Returns the PubidLiteral parsed or NULL.
#[doc(alias = "htmlParsePubidLiteral")]
unsafe fn html_parse_pubid_literal(ctxt: HtmlParserCtxtPtr) -> *mut XmlChar {
    let mut len: size_t = 0;
    let mut err: i32 = 0;

    let mut ret: *mut XmlChar = null_mut();

    if (*ctxt).current_byte() != b'"' && (*ctxt).current_byte() != b'\'' {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrLiteralNotStarted,
            "PubidLiteral \" or ' expected\n",
            None,
            None,
        );
        return null_mut();
    }
    let quote: i32 = (*ctxt).current_byte() as _;
    (*ctxt).skip_char();

    // Name ::= (Letter | '_') (NameChar)*
    if (*ctxt).current_ptr() < (*ctxt).base_ptr() {
        return ret;
    }
    let start_position: size_t = (*ctxt).current_ptr().offset_from((*ctxt).base_ptr()) as _;

    #[allow(clippy::while_immutable_condition)]
    while (*ctxt).current_byte() != 0 && (*ctxt).current_byte() as i32 != quote {
        if !xml_is_pubid_char((*ctxt).current_byte() as u32) {
            html_parse_err_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                "Invalid char in PubidLiteral 0x{:X}\n",
                (*ctxt).current_byte() as i32
            );
            err = 1;
        }
        len += 1;
        (*ctxt).skip_char();
    }

    if (*ctxt).current_byte() as i32 != quote {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrLiteralNotFinished,
            "Unfinished PubidLiteral\n",
            None,
            None,
        );
    } else {
        if err == 0 {
            ret = xml_strndup((*ctxt).base_ptr().add(start_position), len as _);
        }
        (*ctxt).skip_char();
    }

    ret
}

/// Parse an External ID or a Public ID
///
/// `[75] ExternalID ::= b'SYSTEM' S SystemLiteral | 'PUBLIC' S PubidLiteral S SystemLiteral`
///
/// `[83] PublicID ::= b'PUBLIC' S PubidLiteral`
///
/// Returns the function returns SystemLiteral and in the second
/// case publicID receives PubidLiteral, is strict is off
/// it is possible to return NULL and have publicID set.
#[doc(alias = "htmlParseExternalID")]
unsafe fn html_parse_external_id(
    ctxt: HtmlParserCtxtPtr,
    public_id: *mut *mut XmlChar,
) -> *mut XmlChar {
    let mut uri: *mut XmlChar = null_mut();

    if UPPER!(ctxt) == b'S'
        && UPP!(ctxt, 1) == b'Y'
        && UPP!(ctxt, 2) == b'S'
        && UPP!(ctxt, 3) == b'T'
        && UPP!(ctxt, 4) == b'E'
        && UPP!(ctxt, 5) == b'M'
    {
        (*ctxt).advance(6);
        if !xml_is_blank_char((*ctxt).current_byte() as u32) {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after 'SYSTEM'\n",
                None,
                None,
            );
        }
        SKIP_BLANKS!(ctxt);
        uri = html_parse_system_literal(ctxt);
        if uri.is_null() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrURIRequired,
                "htmlParseExternalID: SYSTEM, no URI\n",
                None,
                None,
            );
        }
    } else if UPPER!(ctxt) == b'P'
        && UPP!(ctxt, 1) == b'U'
        && UPP!(ctxt, 2) == b'B'
        && UPP!(ctxt, 3) == b'L'
        && UPP!(ctxt, 4) == b'I'
        && UPP!(ctxt, 5) == b'C'
    {
        (*ctxt).advance(6);
        if !xml_is_blank_char((*ctxt).current_byte() as u32) {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrSpaceRequired,
                "Space required after 'PUBLIC'\n",
                None,
                None,
            );
        }
        SKIP_BLANKS!(ctxt);
        *public_id = html_parse_pubid_literal(ctxt);
        if (*public_id).is_null() {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrPubidRequired,
                "htmlParseExternalID: PUBLIC, no Public Identifier\n",
                None,
                None,
            );
        }
        SKIP_BLANKS!(ctxt);
        if (*ctxt).current_byte() == b'"' || (*ctxt).current_byte() == b'\'' {
            uri = html_parse_system_literal(ctxt);
        }
    }
    uri
}

/// Parse a DOCTYPE declaration
///
/// `[28] doctypedecl ::= '<!DOCTYPE' S Name (S ExternalID)? S? ('[' (markupdecl | PEReference | S)* ']' S?)? '>'`
#[doc(alias = "htmlParseDocTypeDecl")]
unsafe fn html_parse_doc_type_decl(ctxt: HtmlParserCtxtPtr) {
    let mut external_id: *mut XmlChar = null_mut();

    // We know that '<!DOCTYPE' has been detected.
    (*ctxt).advance(9);

    SKIP_BLANKS!(ctxt);

    // Parse the DOCTYPE name.
    let name: *const XmlChar = html_parse_name(ctxt);
    if name.is_null() {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrNameRequired,
            "htmlParseDocTypeDecl : no DOCTYPE name !\n",
            None,
            None,
        );
    }
    // Check that upper(name) == "HTML" !!!!!!!!!!!!!

    SKIP_BLANKS!(ctxt);

    // Check for SystemID and ExternalID
    let uri: *mut XmlChar = html_parse_external_id(ctxt, addr_of_mut!(external_id));
    SKIP_BLANKS!(ctxt);

    // We should be at the end of the DOCTYPE declaration.
    if (*ctxt).current_byte() != b'>' {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrDoctypeNotFinished,
            "DOCTYPE improperly terminated\n",
            None,
            None,
        );
        // Ignore bogus content
        #[allow(clippy::while_immutable_condition)]
        while (*ctxt).current_byte() != 0
            && (*ctxt).current_byte() != b'>'
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            (*ctxt).skip_char();
        }
    }
    if (*ctxt).current_byte() == b'>' {
        (*ctxt).skip_char();
    }

    // Create or update the document accordingly to the DOCTYPE
    if (*ctxt).disable_sax == 0 {
        if let Some(internal_subset) = (*ctxt)
            .sax
            .as_deref_mut()
            .and_then(|sax| sax.internal_subset)
        {
            internal_subset(
                (*ctxt).user_data.clone(),
                (!name.is_null())
                    .then(|| CStr::from_ptr(name as *const i8).to_string_lossy())
                    .as_deref(),
                (!external_id.is_null())
                    .then(|| CStr::from_ptr(external_id as *const i8).to_string_lossy())
                    .as_deref(),
                (!uri.is_null())
                    .then(|| CStr::from_ptr(uri as *const i8).to_string_lossy())
                    .as_deref(),
            );
        }
    }

    // Cleanup, since we don't use all those identifiers
    if !uri.is_null() {
        xml_free(uri as _);
    }
    if !external_id.is_null() {
        xml_free(external_id as _);
    }
}

/// Parse an XML (SGML) comment <!-- .... -->
///
/// `[15] Comment ::= b'<!--' ((Char - '-') | ('-' (Char - '-')))* '-->'`
#[doc(alias = "htmlParseComment")]
unsafe fn html_parse_comment(ctxt: HtmlParserCtxtPtr) {
    let mut buf: *mut XmlChar;
    let mut len: i32;
    let mut size: i32 = HTML_PARSER_BUFFER_SIZE as i32;
    let mut q: i32;
    let mut ql: i32 = 0;
    let mut r: i32;
    let mut rl: i32 = 0;
    let mut cur: i32;
    let mut l: i32 = 0;
    let mut next: i32;
    let mut nl: i32 = 0;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH as i32
    } else {
        XML_MAX_TEXT_LENGTH as i32
    };

    // Check that there is a comment right here.
    if RAW!(ctxt) != b'<' || NXT!(ctxt, 1) != b'!' || NXT!(ctxt, 2) != b'-' || NXT!(ctxt, 3) != b'-'
    {
        return;
    }

    let state: XmlParserInputState = (*ctxt).instate;
    (*ctxt).instate = XmlParserInputState::XmlParserComment;
    (*ctxt).advance(4);
    buf = xml_malloc_atomic(size as usize) as _;
    if buf.is_null() {
        html_err_memory(ctxt, Some("buffer allocation failed\n"));
        (*ctxt).instate = state;
        return;
    }
    len = 0;
    *buf.add(len as usize) = 0;
    q = CUR_CHAR!(ctxt, ql);
    if q == 0 {
        // goto unfinished;
    } else {
        if q == b'>' as i32 {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrCommentAbruptlyEnded,
                "Comment abruptly ended",
                None,
                None,
            );
            cur = b'>' as i32;
            // goto finished;
        } else {
            NEXTL!(ctxt, ql);
            r = CUR_CHAR!(ctxt, rl);
            if r == 0 {
                // goto unfinished;
                let b = CStr::from_ptr(buf as *const i8).to_string_lossy();
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrCommentNotFinished,
                    format!("Comment not terminated \n<!--{b}\n").as_str(),
                    Some(&b),
                    None,
                );
                xml_free(buf as _);
                return;
            }
            if q == b'-' as i32 && r == b'>' as i32 {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrCommentAbruptlyEnded,
                    "Comment abruptly ended",
                    None,
                    None,
                );
                cur = b'>' as i32;
                // goto finished;
            } else {
                NEXTL!(ctxt, rl);
                cur = CUR_CHAR!(ctxt, l);
                while cur != 0 && (cur != b'>' as i32 || r != b'-' as i32 || q != b'-' as i32) {
                    NEXTL!(ctxt, l);
                    next = CUR_CHAR!(ctxt, nl);

                    if q == b'-' as i32
                        && r == b'-' as i32
                        && cur == b'!' as i32
                        && next == b'>' as i32
                    {
                        html_parse_err(
                            ctxt,
                            XmlParserErrors::XmlErrCommentNotFinished,
                            "Comment incorrectly closed by '--!>'",
                            None,
                            None,
                        );
                        cur = b'>' as i32;
                        break;
                    }

                    if len + 5 >= size {
                        size *= 2;
                        let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize) as _;
                        if tmp.is_null() {
                            xml_free(buf as _);
                            html_err_memory(ctxt, Some("growing buffer failed\n"));
                            (*ctxt).instate = state;
                            return;
                        }
                        buf = tmp;
                    }
                    if xml_is_char(q as u32) {
                        COPY_BUF!(ctxt, ql, buf, len, q);
                    } else {
                        html_parse_err_int!(
                            ctxt,
                            XmlParserErrors::XmlErrInvalidChar,
                            "Invalid char in comment 0x{:X}\n",
                            q
                        );
                    }
                    if len > max_length {
                        html_parse_err(
                            ctxt,
                            XmlParserErrors::XmlErrCommentNotFinished,
                            "comment too long",
                            None,
                            None,
                        );
                        xml_free(buf as _);
                        (*ctxt).instate = state;
                        return;
                    }

                    q = r;
                    ql = rl;
                    r = cur;
                    rl = l;
                    cur = next;
                    l = nl;
                }
            }
        }
        // finished:
        *buf.add(len as usize) = 0;
        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            xml_free(buf as _);
            return;
        }
        if cur == b'>' as i32 {
            (*ctxt).skip_char();
            if (*ctxt).disable_sax == 0 {
                if let Some(comment) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.comment) {
                    comment(
                        (*ctxt).user_data.clone(),
                        &CStr::from_ptr(buf as *const i8).to_string_lossy(),
                    );
                }
            }
            xml_free(buf as _);
            (*ctxt).instate = state;
            return;
        }
    }

    // unfinished:
    let b = CStr::from_ptr(buf as *const i8).to_string_lossy();
    html_parse_err(
        ctxt,
        XmlParserErrors::XmlErrCommentNotFinished,
        format!("Comment not terminated \n<!--{b}\n").as_str(),
        Some(&b),
        None,
    );
    xml_free(buf as _);
}

unsafe fn html_skip_bogus_comment(ctxt: HtmlParserCtxtPtr) {
    html_parse_err(
        ctxt,
        XmlParserErrors::XmlHTMLIncorrectlyOpenedComment,
        "Incorrectly opened comment\n",
        None,
        None,
    );

    'b: while {
        let c = (*ctxt).current_byte();
        if c == 0 {
            break 'b;
        }
        (*ctxt).skip_char();

        c != b'>'
    } {}
}

/// Parse an XML Processing Instruction.
///
/// `[16] PI ::= b'<?' PITarget (S (Char* - (Char* '?>' Char*)))? '?>'`
#[doc(alias = "xmlParsePI")]
unsafe fn html_parse_pi(ctxt: HtmlParserCtxtPtr) {
    let mut buf: *mut XmlChar;
    let mut len: i32 = 0;
    let mut size: i32 = HTML_PARSER_BUFFER_SIZE as i32;
    let mut cur: i32;
    let mut l: i32 = 0;
    let max_length: i32 = if (*ctxt).options & XmlParserOption::XmlParseHuge as i32 != 0 {
        XML_MAX_HUGE_LENGTH as i32
    } else {
        XML_MAX_TEXT_LENGTH as i32
    };
    let target: *const XmlChar;
    let state: XmlParserInputState;

    if RAW!(ctxt) == b'<' && NXT!(ctxt, 1) == b'?' {
        state = (*ctxt).instate;
        (*ctxt).instate = XmlParserInputState::XmlParserPI;
        // this is a Processing Instruction.
        (*ctxt).advance(2);

        // Parse the target name and check for special support like namespace.
        target = html_parse_name(ctxt);
        if !target.is_null() {
            let target = CStr::from_ptr(target as *const i8).to_string_lossy();
            if RAW!(ctxt) == b'>' {
                (*ctxt).advance(1);

                // SAX: PI detected.
                if (*ctxt).disable_sax == 0 {
                    if let Some(processing_instruction) = (*ctxt)
                        .sax
                        .as_deref_mut()
                        .and_then(|sax| sax.processing_instruction)
                    {
                        processing_instruction((*ctxt).user_data.clone(), &target, None);
                    }
                }
                (*ctxt).instate = state;
                return;
            }
            buf = xml_malloc_atomic(size as usize) as _;
            if buf.is_null() {
                html_err_memory(ctxt, None);
                (*ctxt).instate = state;
                return;
            }
            cur = (*ctxt).current_byte() as _;
            if !xml_is_blank_char(cur as u32) {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrSpaceRequired,
                    format!("ParsePI: PI {target} space expected\n").as_str(),
                    Some(&target),
                    None,
                );
            }
            SKIP_BLANKS!(ctxt);
            cur = CUR_CHAR!(ctxt, l);
            while cur != 0 && cur != b'>' as i32 {
                if len + 5 >= size {
                    size *= 2;
                    let tmp: *mut XmlChar = xml_realloc(buf as _, size as usize) as _;
                    if tmp.is_null() {
                        html_err_memory(ctxt, None);
                        xml_free(buf as _);
                        (*ctxt).instate = state;
                        return;
                    }
                    buf = tmp;
                }
                if xml_is_char(cur as u32) {
                    COPY_BUF!(ctxt, l, buf, len, cur);
                } else {
                    html_parse_err_int!(
                        ctxt,
                        XmlParserErrors::XmlErrInvalidChar,
                        "Invalid char in processing instruction 0x{:X}\n",
                        cur
                    );
                }
                if len > max_length {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrPINotFinished,
                        format!("PI {target} too long").as_str(),
                        Some(&target),
                        None,
                    );
                    xml_free(buf as _);
                    (*ctxt).instate = state;
                    return;
                }
                NEXTL!(ctxt, l);
                cur = CUR_CHAR!(ctxt, l);
            }
            *buf.add(len as usize) = 0;
            if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
                xml_free(buf as _);
                return;
            }
            if cur != b'>' as i32 {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrPINotFinished,
                    format!("ParsePI: PI {target} never end ...\n").as_str(),
                    Some(&target),
                    None,
                );
            } else {
                (*ctxt).advance(1);

                // SAX: PI detected.
                if (*ctxt).disable_sax == 0 {
                    if let Some(processing_instruction) = (*ctxt)
                        .sax
                        .as_deref_mut()
                        .and_then(|sax| sax.processing_instruction)
                    {
                        processing_instruction(
                            (*ctxt).user_data.clone(),
                            &target,
                            (!buf.is_null())
                                .then(|| CStr::from_ptr(buf as *const i8).to_string_lossy())
                                .as_deref(),
                        );
                    }
                }
            }
            xml_free(buf as _);
        } else {
            html_parse_err(
                ctxt,
                XmlParserErrors::XmlErrPINotStarted,
                "PI is not started correctly",
                None,
                None,
            );
        }
        (*ctxt).instate = state;
    }
}

// The list of HTML elements which are supposed not to have
// CDATA content and where a p element will be implied
//
// TODO: extend that list by reading the HTML SGML DTD on implied paragraph
const HTML_NO_CONTENT_ELEMENTS: &[&str] = &["html", "head"];

/// Check whether a p element need to be implied before inserting
/// characters in the current element.
///
/// Returns 1 if a paragraph has been inserted, 0 if not and -1 in case of error.
#[doc(alias = "htmlCheckParagraph")]
unsafe fn html_check_paragraph(ctxt: HtmlParserCtxtPtr) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    let tag = (*ctxt).name.as_deref();
    let Some(tag) = tag else {
        html_auto_close(ctxt, c"p".as_ptr() as _);
        html_check_implied(ctxt, c"p".as_ptr() as _);
        html_name_push(ctxt, c"p".as_ptr() as _);
        if let Some(start_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element) {
            start_element((*ctxt).user_data.clone(), "p", &[]);
        }
        return 1;
    };
    if HTML_OMITTED_DEFAULT_VALUE.load(Ordering::Relaxed) == 0 {
        return 0;
    }
    for &elem in HTML_NO_CONTENT_ELEMENTS {
        if tag == elem {
            html_auto_close(ctxt, c"p".as_ptr() as _);
            html_check_implied(ctxt, c"p".as_ptr() as _);
            html_name_push(ctxt, c"p".as_ptr() as _);
            if let Some(start_element) =
                (*ctxt).sax.as_deref_mut().and_then(|sax| sax.start_element)
            {
                start_element((*ctxt).user_data.clone(), "p", &[]);
            }
            return 1;
        }
    }
    0
}

/// Parse and handle entity references in content,
/// this will end-up in a call to character() since this is either a CharRef, or a predefined entity.
#[doc(alias = "htmlParseReference")]
unsafe fn html_parse_reference(ctxt: HtmlParserCtxtPtr) {
    let ent: *const HtmlEntityDesc;
    let mut out: [XmlChar; 6] = [0; 6];
    let mut name: *const XmlChar = null();
    if (*ctxt).current_byte() != b'&' {
        return;
    }

    if NXT!(ctxt, 1) == b'#' {
        let mut bits: i32;
        let mut i: i32 = 0;

        let c: u32 = html_parse_char_ref(ctxt) as _;
        if c == 0 {
            return;
        }

        if c < 0x80 {
            out[i as usize] = c as _;
            i += 1;
            bits = -6;
        } else if c < 0x800 {
            out[i as usize] = ((c >> 6) & 0x1F) as u8 | 0xC0;
            i += 1;
            bits = 0;
        } else if c < 0x10000 {
            out[i as usize] = ((c >> 12) & 0x0F) as u8 | 0xE0;
            i += 1;
            bits = 6;
        } else {
            out[i as usize] = ((c >> 18) & 0x07) as u8 | 0xF0;
            i += 1;
            bits = 12;
        }

        while bits >= 0 {
            out[i as usize] = ((c >> bits) & 0x3F) as u8 | 0x80;
            i += 1;
            bits -= 6;
        }
        out[i as usize] = 0;

        html_check_paragraph(ctxt);
        if let Some(characters) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters) {
            let s = from_utf8(&out[..i as usize]).expect("Internal Error");
            characters((*ctxt).user_data.clone(), s);
        }
    } else {
        ent = html_parse_entity_ref(ctxt, addr_of_mut!(name));
        if name.is_null() {
            html_check_paragraph(ctxt);
            if let Some(characters) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters) {
                characters((*ctxt).user_data.clone(), "&");
            }
            return;
        }
        if ent.is_null() || (*ent).value == 0 {
            html_check_paragraph(ctxt);
            if let Some(characters) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters) {
                characters((*ctxt).user_data.clone(), "&");
                characters(
                    (*ctxt).user_data.clone(),
                    &CStr::from_ptr(name as *const i8).to_string_lossy(),
                );
                // (*(*ctxt).sax).characters((*ctxt).userData,  c";".as_ptr() as _, 1);
            }
        } else {
            let mut bits: i32;
            let mut i: i32 = 0;

            let c: u32 = (*ent).value;
            if c < 0x80 {
                out[i as usize] = c as _;
                i += 1;
                bits = -6;
            } else if c < 0x800 {
                out[i as usize] = ((c >> 6) & 0x1F) as u8 | 0xC0;
                i += 1;
                bits = 0;
            } else if c < 0x10000 {
                out[i as usize] = ((c >> 12) & 0x0F) as u8 | 0xE0;
                i += 1;
                bits = 6;
            } else {
                out[i as usize] = ((c >> 18) & 0x07) as u8 | 0xF0;
                i += 1;
                bits = 12;
            }

            while bits >= 0 {
                out[i as usize] = ((c >> bits) & 0x3F) as u8 | 0x80;
                i += 1;
                bits -= 6;
            }
            out[i as usize] = 0;

            html_check_paragraph(ctxt);
            if let Some(characters) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters) {
                let s = from_utf8(&out[..i as usize]).expect("Internal Error");
                characters((*ctxt).user_data.clone(), s);
            }
        }
    }
}

// all tags allowing pc data from the html 4.01 loose dtd
//
// NOTE: it might be more appropriate to integrate this information
// into the html40ElementTable array but I don't want to risk any
// binary incompatibility
const ALLOW_PCDATA: &[&str] = &[
    "a",
    "abbr",
    "acronym",
    "address",
    "applet",
    "b",
    "bdo",
    "big",
    "blockquote",
    "body",
    "button",
    "caption",
    "center",
    "cite",
    "code",
    "dd",
    "del",
    "dfn",
    "div",
    "dt",
    "em",
    "font",
    "form",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "i",
    "iframe",
    "ins",
    "kbd",
    "label",
    "legend",
    "li",
    "noframes",
    "noscript",
    "object",
    "p",
    "pre",
    "q",
    "s",
    "samp",
    "small",
    "span",
    "strike",
    "strong",
    "td",
    "th",
    "tt",
    "u",
    "var",
];

/// Is this a sequence of blank chars that one can ignore ?
///
/// Returns 1 if ignorable 0 otherwise.
#[doc(alias = "areBlanks")]
unsafe fn are_blanks(ctxt: HtmlParserCtxtPtr, str: *const XmlChar, len: i32) -> i32 {
    let mut last_child: XmlNodePtr;
    let dtd: XmlDtdPtr;

    for j in 0..len {
        if !xml_is_blank_char(*str.add(j as usize) as u32) {
            return 0;
        }
    }

    if (*ctxt).current_byte() == 0 {
        return 1;
    }
    if (*ctxt).current_byte() != b'<' {
        return 0;
    }
    let Some(name) = (*ctxt).name.as_deref() else {
        return 1;
    };
    if name == "html" {
        return 1;
    }
    if name == "head" {
        return 1;
    }

    // Only strip CDATA children of the body tag for strict HTML DTDs
    if name == "body" && !(*ctxt).my_doc.is_null() {
        dtd = (*(*ctxt).my_doc).get_int_subset();
        if !dtd.is_null()
            && (*dtd)
                .external_id
                .as_deref()
                .filter(|e| {
                    let e = e.to_ascii_uppercase();
                    e == "-//W3C//DTD HTML 4.01//EN" || e == "-//W3C//DTD HTML 4//EN"
                })
                .is_some()
        {
            return 1;
        }
    }

    if (*ctxt).node.is_null() {
        return 0;
    }
    last_child = (*(*ctxt).node).get_last_child();
    while !last_child.is_null() && (*last_child).element_type() == XmlElementType::XmlCommentNode {
        last_child = (*last_child).prev.map_or(null_mut(), |p| p.as_ptr());
    }
    if last_child.is_null() {
        if (*(*ctxt).node).element_type() != XmlElementType::XmlElementNode
            && !(*(*ctxt).node).content.is_null()
        {
            return 0;
        }
        // keep ws in constructs like ...<b> </b>...
        // for all tags "b" allowing PCDATA
        for &pcdata in ALLOW_PCDATA {
            if name == pcdata {
                return 0;
            }
        }
    } else if (*last_child).is_text_node() {
        return 0;
    } else {
        // keep ws in constructs like <p><b>xy</b> <i>z</i><p>
        // for all tags "p" allowing PCDATA
        for &pcdata in ALLOW_PCDATA {
            if (*last_child).name().as_deref() == Some(pcdata) {
                return 0;
            }
        }
    }
    1
}

/// Parse a CharData section.
/// if we are within a CDATA section ']]>' marks an end of section.
///
/// `[14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)`
#[doc(alias = "htmlParseCharDataInternal")]
unsafe fn html_parse_char_data_internal(ctxt: HtmlParserCtxtPtr, readahead: i32) {
    let mut buf: [XmlChar; HTML_PARSER_BIG_BUFFER_SIZE + 6] = [0; HTML_PARSER_BIG_BUFFER_SIZE + 6];
    let mut nbchar: i32 = 0;
    let mut cur: i32;
    let mut l: i32 = 0;

    if readahead != 0 {
        buf[nbchar as usize] = readahead as _;
        nbchar += 1;
    }

    cur = CUR_CHAR!(ctxt, l);
    while (cur != b'<' as i32 || (*ctxt).token == b'<' as i32)
        && (cur != b'&' as i32 || (*ctxt).token == b'&' as i32)
        && cur != 0
    {
        if !xml_is_char(cur as u32) {
            html_parse_err_int!(
                ctxt,
                XmlParserErrors::XmlErrInvalidChar,
                "Invalid char in CDATA 0x{:X}\n",
                cur
            );
        } else {
            COPY_BUF!(ctxt, l, buf.as_mut_ptr(), nbchar, cur);
        }
        NEXTL!(ctxt, l);
        if nbchar >= HTML_PARSER_BIG_BUFFER_SIZE as i32 {
            buf[nbchar as usize] = 0;

            // Ok the segment is to be consumed as chars.
            if (*ctxt).disable_sax == 0 {
                if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                    let s = from_utf8(&buf[..nbchar as usize]).expect("Internal Error");
                    if are_blanks(ctxt, buf.as_ptr(), nbchar) != 0 {
                        if (*ctxt).keep_blanks != 0 {
                            if let Some(characters) = sax.characters {
                                characters((*ctxt).user_data.clone(), s);
                            }
                        } else if let Some(ignorable_whitespace) = sax.ignorable_whitespace {
                            ignorable_whitespace((*ctxt).user_data.clone(), s);
                        }
                    } else {
                        html_check_paragraph(ctxt);
                        if let Some(characters) = sax.characters {
                            characters((*ctxt).user_data.clone(), s);
                        }
                    }
                }
            }
            nbchar = 0;
            (*ctxt).shrink();
        }
        cur = CUR_CHAR!(ctxt, l);
    }
    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }
    if nbchar != 0 {
        buf[nbchar as usize] = 0;

        // Ok the segment is to be consumed as chars.
        if (*ctxt).disable_sax == 0 {
            if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                let s = from_utf8(&buf[..nbchar as usize]).expect("Internal Error");
                if are_blanks(ctxt, buf.as_ptr(), nbchar) != 0 {
                    if (*ctxt).keep_blanks != 0 {
                        if let Some(characters) = sax.characters {
                            characters((*ctxt).user_data.clone(), s);
                        }
                    } else if let Some(ignorable_whitespace) = sax.ignorable_whitespace {
                        ignorable_whitespace((*ctxt).user_data.clone(), s);
                    }
                } else {
                    html_check_paragraph(ctxt);
                    if let Some(characters) = sax.characters {
                        characters((*ctxt).user_data.clone(), s);
                    }
                }
            }
        }
    }
}

/// Parse a CharData section.
/// if we are within a CDATA section ']]>' marks an end of section.
///
/// `[14] CharData ::= [^<&]* - ([^<&]* ']]>' [^<&]*)`
#[doc(alias = "htmlParseCharData")]
unsafe fn html_parse_char_data(ctxt: HtmlParserCtxtPtr) {
    html_parse_char_data_internal(ctxt, 0);
}

/// Parse a content: comment, sub-element, reference or text.
/// Kept for compatibility with old code
#[doc(alias = "htmlParseContent")]
unsafe fn html_parse_content(ctxt: HtmlParserCtxtPtr) {
    let mut name: *const XmlChar;

    let current_node = (*ctxt).name.clone();
    let depth = (*ctxt).name_tab.len();
    loop {
        (*ctxt).grow();

        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            break;
        }

        // Our tag or one of it's parent or children is ending.
        if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'/' {
            if html_parse_end_tag(ctxt) != 0
                && (current_node.is_some() || (*ctxt).name_tab.is_empty())
            {
                return;
            }
            continue; /* while */
        } else if (*ctxt).current_byte() == b'<'
            && (NXT!(ctxt, 1).is_ascii_alphabetic()
                || NXT!(ctxt, 1) == b'_'
                || NXT!(ctxt, 1) == b':')
        {
            name = html_parse_html_name_non_invasive(ctxt);
            if name.is_null() {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    "htmlParseStartTag: invalid element name\n",
                    None,
                    None,
                );
                // Dump the bogus tag like browsers do
                #[allow(clippy::while_immutable_condition)]
                while (*ctxt).current_byte() != 0 && (*ctxt).current_byte() != b'>' {
                    (*ctxt).skip_char();
                }

                return;
            }

            if (*ctxt).name.is_some() && html_check_auto_close(name, (*ctxt).name.as_deref()) == 1 {
                html_auto_close(ctxt, name);
                continue;
            }
        }

        // Has this node been popped out during parsing of the next element
        if !(*ctxt).name_tab.is_empty()
            && depth >= (*ctxt).name_tab.len()
            && current_node != (*ctxt).name
        {
            return;
        }

        if (*ctxt).current_byte() != 0
            && (current_node.as_deref() == Some("script")
                || current_node.as_deref() == Some("style"))
        {
            // Handle SCRIPT/STYLE separately
            html_parse_script(ctxt);
        } else if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'!' {
            // Sometimes DOCTYPE arrives in the middle of the document
            if UPP!(ctxt, 2) == b'D'
                && UPP!(ctxt, 3) == b'O'
                && UPP!(ctxt, 4) == b'C'
                && UPP!(ctxt, 5) == b'T'
                && UPP!(ctxt, 6) == b'Y'
                && UPP!(ctxt, 7) == b'P'
                && UPP!(ctxt, 8) == b'E'
            {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlHTMLStrucureError,
                    "Misplaced DOCTYPE declaration\n",
                    Some("DOCTYPE"),
                    None,
                );
                html_parse_doc_type_decl(ctxt);
            } else if NXT!(ctxt, 2) == b'-' && NXT!(ctxt, 3) == b'-' {
                //  case :  a comment
                html_parse_comment(ctxt);
            } else {
                html_skip_bogus_comment(ctxt);
            }
        } else if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'?' {
            // Second case : a Processing Instruction.
            html_parse_pi(ctxt);
        } else if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1).is_ascii_alphabetic() {
            // Third case :  a sub-element.
            html_parse_element(ctxt);
        } else if (*ctxt).current_byte() == b'<' {
            if (*ctxt).disable_sax == 0 {
                if let Some(characters) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
                {
                    characters((*ctxt).user_data.clone(), "<");
                }
            }
            (*ctxt).skip_char();
        } else if (*ctxt).current_byte() == b'&' {
            // Fourth case : a reference. If if has not been resolved,
            //    parsing returns it's Name, create the node
            html_parse_reference(ctxt);
        } else if (*ctxt).current_byte() == 0 {
            // Fifth case : end of the resource
            html_auto_close_on_end(ctxt);
            break;
        } else {
            // Last case, text. Note that References are handled directly.
            html_parse_char_data(ctxt);
        }

        (*ctxt).shrink();
        (*ctxt).grow();
    }
}

/// Parse an HTML element, this is highly recursive
/// this is kept for compatibility with previous code versions
///
/// `[39] element ::= EmptyElemTag | STag content ETag`
///
/// `[41] Attribute ::= Name Eq AttValue`
#[doc(alias = "htmlParseElement")]
pub(crate) unsafe fn html_parse_element(ctxt: HtmlParserCtxtPtr) {
    let mut node_info = XmlParserNodeInfo::default();
    let mut oldptr: *const XmlChar;

    if ctxt.is_null() || (*ctxt).input.is_null() {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            "htmlParseElement: context error\n",
            None,
            None,
        );
        return;
    }

    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }

    // Capture start position
    if (*ctxt).record_info != 0 {
        node_info.begin_pos =
            (*(*ctxt).input).consumed + (*(*ctxt).input).offset_from_base() as u64;
        node_info.begin_line = (*(*ctxt).input).line as _;
    }

    let failed: i32 = html_parse_start_tag(ctxt);
    let name = (*ctxt).name.clone();
    let Some(name) = name.filter(|_| failed != -1) else {
        if (*ctxt).current_byte() == b'>' {
            (*ctxt).skip_char();
        }
        return;
    };

    // Lookup the info for that element.
    let info = html_tag_lookup(&name);
    if info.is_none() {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlHTMLUnknownTag,
            format!("Tag {name} invalid\n").as_str(),
            Some(&name),
            None,
        );
    }

    // Check for an Empty Element labeled the XML/SGML way
    if (*ctxt).current_byte() == b'/' && NXT!(ctxt, 1) == b'>' {
        (*ctxt).advance(2);
        if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
            end_element((*ctxt).user_data.clone(), &name);
        }
        html_name_pop(ctxt);
        return;
    }

    if (*ctxt).current_byte() == b'>' {
        (*ctxt).skip_char();
    } else {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrGtRequired,
            format!("Couldn't find end of Start Tag {name}\n").as_str(),
            Some(&name),
            None,
        );

        // end of parsing of this node.
        if Some(name.as_str()) == (*ctxt).name.as_deref() {
            (*ctxt).node_pop();
            html_name_pop(ctxt);
        }

        // Capture end position and add node
        if (*ctxt).record_info != 0 {
            node_info.end_pos =
                (*(*ctxt).input).consumed + (*(*ctxt).input).offset_from_base() as u64;
            node_info.end_line = (*(*ctxt).input).line as _;
            node_info.node = NonNull::new((*ctxt).node);
            xml_parser_add_node_info(ctxt, Rc::new(RefCell::new(node_info)));
        }
        return;
    }

    // Check for an Empty Element from DTD definition
    if info.map_or(false, |info| info.empty != 0) {
        if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
            end_element((*ctxt).user_data.clone(), &name);
        }
        html_name_pop(ctxt);
        return;
    }

    // Parse the content of the element:
    let current_node = (*ctxt).name.clone();
    let depth = (*ctxt).name_tab.len();
    #[allow(clippy::while_immutable_condition)]
    while (*ctxt).current_byte() != 0 {
        oldptr = (*(*ctxt).input).cur;
        html_parse_content(ctxt);
        if oldptr == (*(*ctxt).input).cur {
            break;
        }
        if (*ctxt).name_tab.len() < depth {
            break;
        }
    }

    // Capture end position and add node
    if current_node.is_some() && (*ctxt).record_info != 0 {
        node_info.end_pos = (*(*ctxt).input).consumed + (*(*ctxt).input).offset_from_base() as u64;
        node_info.end_line = (*(*ctxt).input).line as _;
        node_info.node = NonNull::new((*ctxt).node);
        xml_parser_add_node_info(ctxt, Rc::new(RefCell::new(node_info)));
    }
    if (*ctxt).current_byte() == 0 {
        html_auto_close_on_end(ctxt);
    }
}

/// Allocate and initialize a new parser context.
///
/// Returns the `htmlParserCtxtPtr` or NULL in case of allocation error
#[doc(alias = "htmlNewParserCtxt")]
pub unsafe fn html_new_parser_ctxt() -> HtmlParserCtxtPtr {
    html_new_sax_parser_ctxt(None, None)
}

/// Initialize a parser context
///
/// Returns 0 in case of success and -1 in case of error
#[doc(alias = "htmlInitParserCtxt")]
unsafe fn html_init_parser_ctxt(
    ctxt: HtmlParserCtxtPtr,
    sax: Option<Box<HtmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    memset(ctxt as _, 0, size_of::<HtmlParserCtxt>());
    std::ptr::write(&mut *ctxt, HtmlParserCtxt::default());

    (*ctxt).dict = xml_dict_create();
    if (*ctxt).dict.is_null() {
        html_err_memory(null_mut(), Some("htmlInitParserCtxt: out of memory\n"));
        return -1;
    }

    if sax.is_none() {
        let mut sax = HtmlSAXHandler::default();
        xml_sax2_init_html_default_sax_handler(&mut sax);
        (*ctxt).sax = Some(Box::new(sax));
        (*ctxt).user_data = Some(GenericErrorContext::new(ctxt));
    } else {
        (*ctxt).sax = sax;
        (*ctxt).user_data = user_data.or_else(|| Some(GenericErrorContext::new(ctxt)));
    }

    // Allocate the Input stack
    (*ctxt).input_tab.clear();
    (*ctxt).input_tab.shrink_to(5);
    (*ctxt).input = null_mut();
    (*ctxt).version = None;
    (*ctxt).encoding = None;
    (*ctxt).standalone = -1;
    (*ctxt).instate = XmlParserInputState::XmlParserStart;

    // Allocate the Node stack
    (*ctxt).input_tab.clear();
    (*ctxt).node_tab.shrink_to(10);
    (*ctxt).node = null_mut();

    // Allocate the Name stack
    (*ctxt).name_tab.clear();
    (*ctxt).name_tab.shrink_to(10);
    (*ctxt).name = None;

    (*ctxt).node_info_tab.clear();

    (*ctxt).my_doc = null_mut();
    (*ctxt).well_formed = 1;
    (*ctxt).replace_entities = 0;
    (*ctxt).linenumbers = get_line_numbers_default_value();
    (*ctxt).keep_blanks = get_keep_blanks_default_value();
    (*ctxt).html = 1;
    (*ctxt).vctxt.flags = XML_VCTXT_USE_PCTXT as _;
    (*ctxt).vctxt.user_data = Some(GenericErrorContext::new(ctxt));
    (*ctxt).vctxt.error = Some(parser_validity_error);
    (*ctxt).vctxt.warning = Some(parser_validity_warning);
    (*ctxt).record_info = 0;
    (*ctxt).validate = 0;
    (*ctxt).check_index = 0;
    #[cfg(feature = "catalog")]
    {
        (*ctxt).catalogs = None;
    }
    (*ctxt).node_seq.clear();
    0
}

/// Allocate and initialize a new SAX parser context.
/// If userData is NULL, the parser context will be passed as user data.
///
/// Returns the `htmlParserCtxtPtr` or NULL in case of allocation error
#[doc(alias = "htmlNewSAXParserCtxt")]
pub unsafe fn html_new_sax_parser_ctxt(
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
) -> HtmlParserCtxtPtr {
    let ctxt: XmlParserCtxtPtr = xml_malloc(size_of::<XmlParserCtxt>()) as XmlParserCtxtPtr;
    if ctxt.is_null() {
        html_err_memory(null_mut(), Some("NewParserCtxt: out of memory\n"));
        return null_mut();
    }
    memset(ctxt as _, 0, size_of::<XmlParserCtxt>());
    std::ptr::write(&mut *ctxt, XmlParserCtxt::default());
    if html_init_parser_ctxt(ctxt, sax, user_data) < 0 {
        html_free_parser_ctxt(ctxt);
        return null_mut();
    }
    ctxt
}

/// Create a parser context for an HTML in-memory document.
///
/// Returns the new parser context or NULL
#[doc(alias = "htmlCreateMemoryParserCtxt")]
pub unsafe fn html_create_memory_parser_ctxt(buffer: Vec<u8>) -> HtmlParserCtxtPtr {
    if buffer.is_empty() {
        return null_mut();
    }

    let ctxt: XmlParserCtxtPtr = html_new_parser_ctxt();
    if ctxt.is_null() {
        return null_mut();
    }

    let Some(buf) = XmlParserInputBuffer::from_memory(buffer, XmlCharEncoding::None) else {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    };

    let input: XmlParserInputPtr = xml_new_input_stream(ctxt);
    if input.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    (*input).filename = None;
    std::ptr::write(&raw mut (*input).buf, Some(Rc::new(RefCell::new(buf))));
    (*input).reset_base();

    (*ctxt).input_push(input);
    ctxt
}

#[doc(alias = "htmlParserFinishElementParsing")]
unsafe fn html_parser_finish_element_parsing(ctxt: HtmlParserCtxtPtr) {
    // Capture end position and add node
    if !(*ctxt).node.is_null() && (*ctxt).record_info != 0 {
        let node_info = (*ctxt).node_info_tab.last_mut().expect("Internal Error");
        node_info.borrow_mut().end_pos =
            (*(*ctxt).input).consumed + (*(*ctxt).input).offset_from_base() as u64;
        node_info.borrow_mut().end_line = (*(*ctxt).input).line as _;
        node_info.borrow_mut().node = NonNull::new((*ctxt).node);
        xml_parser_add_node_info(ctxt, node_info.clone());
        html_node_info_pop(ctxt);
    }
    if (*ctxt).current_byte() == 0 {
        html_auto_close_on_end(ctxt);
    }
}

/// Pushes a new element name on top of the node info stack
///
/// Returns 0 in case of error, the index in the stack otherwise
#[doc(alias = "htmlNodeInfoPush")]
unsafe fn html_node_info_push(
    ctxt: HtmlParserCtxtPtr,
    value: Rc<RefCell<HtmlParserNodeInfo>>,
) -> usize {
    (*ctxt).node_info_tab.push(value);
    (*ctxt).node_info_tab.len()
}

/// Parse an HTML element, new version, non recursive
///
/// `[39] element ::= EmptyElemTag | STag content ETag`
///
/// `[41] Attribute ::= Name Eq AttValue`
#[doc(alias = "htmlParseElementInternal")]
unsafe fn html_parse_element_internal(ctxt: HtmlParserCtxtPtr) {
    let mut node_info = HtmlParserNodeInfo::default();

    if ctxt.is_null() || (*ctxt).input.is_null() {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            "htmlParseElementInternal: context error\n",
            None,
            None,
        );
        return;
    }

    if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
        return;
    }

    // Capture start position
    if (*ctxt).record_info != 0 {
        node_info.begin_pos =
            (*(*ctxt).input).consumed + (*(*ctxt).input).offset_from_base() as u64;
        node_info.begin_line = (*(*ctxt).input).line as _;
    }

    let failed: i32 = html_parse_start_tag(ctxt);
    let Some(name) = (*ctxt).name.clone().filter(|_| failed != -1) else {
        if (*ctxt).current_byte() == b'>' {
            (*ctxt).skip_char();
        }
        return;
    };

    // Lookup the info for that element.
    let info = html_tag_lookup(&name);
    if info.is_none() {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlHTMLUnknownTag,
            format!("Tag {name} invalid\n").as_str(),
            Some(&name),
            None,
        );
    }

    // Check for an Empty Element labeled the XML/SGML way
    if (*ctxt).current_byte() == b'/' && NXT!(ctxt, 1) == b'>' {
        (*ctxt).advance(2);
        if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
            end_element((*ctxt).user_data.clone(), &name);
        }
        html_name_pop(ctxt);
        return;
    }

    if (*ctxt).current_byte() == b'>' {
        (*ctxt).skip_char();
    } else {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrGtRequired,
            format!("Couldn't find end of Start Tag {name}\n").as_str(),
            Some(name.as_str()),
            None,
        );

        // end of parsing of this node.
        if Some(name.as_str()) == (*ctxt).name.as_deref() {
            (*ctxt).node_pop();
            html_name_pop(ctxt);
        }

        if (*ctxt).record_info != 0 {
            html_node_info_push(ctxt, Rc::new(RefCell::new(node_info)));
        }
        html_parser_finish_element_parsing(ctxt);
        return;
    }

    // Check for an Empty Element from DTD definition
    if info.map_or(false, |info| info.empty != 0) {
        if let Some(end_element) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element) {
            end_element((*ctxt).user_data.clone(), &name);
        }
        html_name_pop(ctxt);
        return;
    }

    if (*ctxt).record_info != 0 {
        html_node_info_push(ctxt, Rc::new(RefCell::new(node_info)));
    }
}

/// Parse a content: comment, sub-element, reference or text.
/// New version for non recursive htmlParseElementInternal
#[doc(alias = "htmlParseContentInternal")]
unsafe fn html_parse_content_internal(ctxt: HtmlParserCtxtPtr) {
    let mut name: *const XmlChar;
    let mut depth = (*ctxt).name_tab.len();
    let mut current_node = if depth == 0 {
        None
    } else {
        (*ctxt).name.clone()
    };
    loop {
        (*ctxt).grow();

        if matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            break;
        }

        // Our tag or one of it's parent or children is ending.
        if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'/' {
            if html_parse_end_tag(ctxt) != 0
                && (current_node.is_some() || (*ctxt).name_tab.is_empty())
            {
                depth = (*ctxt).name_tab.len();
                if depth == 0 {
                    current_node = None;
                } else {
                    current_node = (*ctxt).name.clone();
                }
            }
            continue; /* while */
        } else if (*ctxt).current_byte() == b'<'
            && (NXT!(ctxt, 1).is_ascii_alphabetic()
                || NXT!(ctxt, 1) == b'_'
                || NXT!(ctxt, 1) == b':')
        {
            name = html_parse_html_name_non_invasive(ctxt);
            if name.is_null() {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrNameRequired,
                    "htmlParseStartTag: invalid element name\n",
                    None,
                    None,
                );
                /* Dump the bogus tag like browsers do */
                #[allow(clippy::while_immutable_condition)]
                while (*ctxt).current_byte() == 0 && (*ctxt).current_byte() != b'>' {
                    (*ctxt).skip_char();
                }

                html_parser_finish_element_parsing(ctxt);

                current_node = (*ctxt).name.clone();
                depth = (*ctxt).name_tab.len();
                continue;
            }

            if (*ctxt).name.is_some() && html_check_auto_close(name, (*ctxt).name.as_deref()) == 1 {
                html_auto_close(ctxt, name);
                continue;
            }
        }

        // Has this node been popped out during parsing of the next element
        if !(*ctxt).name_tab.is_empty()
            && depth >= (*ctxt).name_tab.len()
            && current_node != (*ctxt).name
        {
            html_parser_finish_element_parsing(ctxt);

            current_node = (*ctxt).name.clone();
            depth = (*ctxt).name_tab.len();
            continue;
        }

        if (*ctxt).current_byte() != 0
            && (current_node.as_deref() == Some("script")
                || current_node.as_deref() == Some("style"))
        {
            // Handle SCRIPT/STYLE separately
            html_parse_script(ctxt);
        } else if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'!' {
            // Sometimes DOCTYPE arrives in the middle of the document
            if UPP!(ctxt, 2) == b'D'
                && UPP!(ctxt, 3) == b'O'
                && UPP!(ctxt, 4) == b'C'
                && UPP!(ctxt, 5) == b'T'
                && UPP!(ctxt, 6) == b'Y'
                && UPP!(ctxt, 7) == b'P'
                && UPP!(ctxt, 8) == b'E'
            {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlHTMLStrucureError,
                    "Misplaced DOCTYPE declaration\n",
                    Some("DOCTYPE"),
                    None,
                );
                html_parse_doc_type_decl(ctxt);
            } else if NXT!(ctxt, 2) == b'-' && NXT!(ctxt, 3) == b'-' {
                // First case :  a comment
                html_parse_comment(ctxt);
            } else {
                html_skip_bogus_comment(ctxt);
            }
        } else if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'?' {
            // Second case : a Processing Instruction.
            html_parse_pi(ctxt);
        } else if (*ctxt).current_byte() == b'<' && NXT!(ctxt, 1).is_ascii_alphabetic() {
            // Third case :  a sub-element.
            html_parse_element_internal(ctxt);

            current_node = (*ctxt).name.clone();
            depth = (*ctxt).name_tab.len();
        } else if (*ctxt).current_byte() == b'<' {
            if (*ctxt).disable_sax == 0 {
                if let Some(characters) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
                {
                    characters((*ctxt).user_data.clone(), "<");
                }
            }
            (*ctxt).skip_char();
        } else if (*ctxt).current_byte() == b'&' {
            // Fourth case : a reference. If if has not been resolved,
            //    parsing returns it's Name, create the node
            html_parse_reference(ctxt);
        } else if (*ctxt).current_byte() == 0 {
            // Fifth case : end of the resource
            html_auto_close_on_end(ctxt);
            break;
        } else {
            // Last case, text. Note that References are handled directly.
            html_parse_char_data(ctxt);
        }

        (*ctxt).shrink();
        (*ctxt).grow();
    }
}

/// Parse an HTML document (and build a tree if using the standard SAX interface).
///
/// Returns 0, -1 in case of error. the parser context is augmented as a result of the parsing.
#[doc(alias = "htmlParseDocument")]
pub unsafe fn html_parse_document(ctxt: HtmlParserCtxtPtr) -> i32 {
    let mut start: [XmlChar; 4] = [0; 4];
    let dtd: XmlDtdPtr;

    xml_init_parser();

    if ctxt.is_null() || (*ctxt).input.is_null() {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            "htmlParseDocument: context error\n",
            None,
            None,
        );
        return XmlParserErrors::XmlErrInternalError as i32;
    }
    (*ctxt).grow();
    // SAX: beginning of the document processing.
    if let Some(set_document_locator) = (*ctxt)
        .sax
        .as_deref_mut()
        .and_then(|sax| sax.set_document_locator)
    {
        set_document_locator((*ctxt).user_data.clone(), xml_default_sax_locator());
    }

    if (*ctxt).encoding.is_none() && (*(*ctxt).input).remainder_len() >= 4 {
        // Get the 4 first bytes and decode the charset
        // if enc != xmlCharEncoding::XML_CHAR_ENCODING_NONE
        // plug some encoding conversion routines.
        start[0] = RAW!(ctxt);
        start[1] = NXT!(ctxt, 1);
        start[2] = NXT!(ctxt, 2);
        start[3] = NXT!(ctxt, 3);
        let enc = detect_encoding(&start);
        if !matches!(enc, XmlCharEncoding::None) {
            (*ctxt).switch_encoding(enc);
        }
    }

    // Wipe out everything which is before the first '<'
    SKIP_BLANKS!(ctxt);
    if (*ctxt).current_byte() == 0 {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrDocumentEmpty,
            "Document is empty\n",
            None,
            None,
        );
    }

    if (*ctxt).disable_sax == 0 {
        if let Some(start_document) = (*ctxt)
            .sax
            .as_deref_mut()
            .and_then(|sax| sax.start_document)
        {
            start_document((*ctxt).user_data.clone());
        }
    }

    // Parse possible comments and PIs before any content
    while ((*ctxt).current_byte() == b'<'
        && NXT!(ctxt, 1) == b'!'
        && NXT!(ctxt, 2) == b'-'
        && NXT!(ctxt, 3) == b'-')
        || ((*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'?')
    {
        html_parse_comment(ctxt);
        html_parse_pi(ctxt);
        SKIP_BLANKS!(ctxt);
    }

    // Then possibly doc type declaration(s) and more Misc (doctypedecl Misc*)?
    if (*ctxt).current_byte() == b'<'
        && NXT!(ctxt, 1) == b'!'
        && UPP!(ctxt, 2) == b'D'
        && UPP!(ctxt, 3) == b'O'
        && UPP!(ctxt, 4) == b'C'
        && UPP!(ctxt, 5) == b'T'
        && UPP!(ctxt, 6) == b'Y'
        && UPP!(ctxt, 7) == b'P'
        && UPP!(ctxt, 8) == b'E'
    {
        html_parse_doc_type_decl(ctxt);
    }
    SKIP_BLANKS!(ctxt);

    // Parse possible comments and PIs before any content
    while ((*ctxt).current_byte() == b'<'
        && NXT!(ctxt, 1) == b'!'
        && NXT!(ctxt, 2) == b'-'
        && NXT!(ctxt, 3) == b'-')
        || ((*ctxt).current_byte() == b'<' && NXT!(ctxt, 1) == b'?')
    {
        html_parse_comment(ctxt);
        html_parse_pi(ctxt);
        SKIP_BLANKS!(ctxt);
    }

    // Time to start parsing the tree itself
    html_parse_content_internal(ctxt);

    // autoclose
    if (*ctxt).current_byte() == 0 {
        html_auto_close_on_end(ctxt);
    }

    // SAX: end of the document processing.
    if let Some(end_document) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_document) {
        end_document((*ctxt).user_data.clone());
    }

    if (*ctxt).options & HtmlParserOption::HtmlParseNodefdtd as i32 == 0
        && !(*ctxt).my_doc.is_null()
    {
        dtd = (*(*ctxt).my_doc).get_int_subset();
        if dtd.is_null() {
            (*(*ctxt).my_doc).int_subset = xml_create_int_subset(
                (*ctxt).my_doc,
                Some("html"),
                Some("-//W3C//DTD HTML 4.0 Transitional//EN"),
                Some("http://www.w3.org/TR/REC-html40/loose.dtd"),
            );
        }
    }
    if (*ctxt).well_formed == 0 {
        return -1;
    }
    0
}

/// Create a parser context for an HTML document.
///
/// TODO: check the need to add encoding handling there
///
/// Returns the new parser context or NULL
#[doc(alias = "htmlCreateDocParserCtxt")]
unsafe fn html_create_doc_parser_ctxt(
    cur: *const XmlChar,
    encoding: Option<&str>,
) -> HtmlParserCtxtPtr {
    if cur.is_null() {
        return null_mut();
    }
    let s = CStr::from_ptr(cur as *const i8).to_bytes().to_vec();
    let ctxt: HtmlParserCtxtPtr = html_create_memory_parser_ctxt(s);
    if ctxt.is_null() {
        return null_mut();
    }

    if let Some(encoding) = encoding {
        (*(*ctxt).input).encoding = Some(encoding.to_owned());

        let enc = encoding.parse().unwrap_or(XmlCharEncoding::Error);
        // registered set of known encodings
        if !matches!(enc, XmlCharEncoding::Error) {
            (*ctxt).switch_encoding(enc);
            if (*ctxt).err_no == XmlParserErrors::XmlErrUnsupportedEncoding as i32 {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrUnsupportedEncoding,
                    format!("Unsupported encoding {encoding}\n").as_str(),
                    Some(encoding),
                    None,
                );
            }
        } else {
            // fallback for unknown encodings
            if let Some(handler) = find_encoding_handler(encoding) {
                (*ctxt).switch_to_encoding(handler);
            } else {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrUnsupportedEncoding,
                    format!("Unsupported encoding {encoding}\n").as_str(),
                    Some(encoding),
                    None,
                );
            }
        }
    }
    ctxt
}

/// Parse an HTML in-memory document.  
/// If sax is not NULL, use the SAX callbacks to handle parse events.  
/// If sax is NULL, fallback to the default DOM behavior and return a tree.
///
/// Returns the resulting document tree unless SAX is NULL or the document is not well formed.
#[doc(alias = "htmlSAXParseDoc")]
#[deprecated = "Use htmlNewSAXParserCtxt and htmlCtxtReadDoc"]
pub unsafe fn html_sax_parse_doc(
    cur: *const XmlChar,
    encoding: Option<&str>,
    sax: Option<Box<HtmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
) -> HtmlDocPtr {
    xml_init_parser();

    if cur.is_null() {
        return null_mut();
    }

    let ctxt: HtmlParserCtxtPtr = html_create_doc_parser_ctxt(cur, encoding);
    if ctxt.is_null() {
        return null_mut();
    }
    let replaced = sax.is_some();
    if let Some(sax) = sax {
        (*ctxt).sax = Some(sax);
        (*ctxt).user_data = user_data;
    }

    html_parse_document(ctxt);
    let ret: HtmlDocPtr = (*ctxt).my_doc;
    if replaced {
        (*ctxt).sax = None;
        (*ctxt).user_data = None;
    }
    html_free_parser_ctxt(ctxt);

    ret
}

/// Parse an HTML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "htmlParseDoc")]
pub unsafe fn html_parse_doc(cur: *const XmlChar, encoding: Option<&str>) -> HtmlDocPtr {
    html_sax_parse_doc(cur, encoding, None, None)
}

/// Create a parser context for a file content.
///
/// In original libxml2, automatic support for ZLIB/Compress compressed document is provided
/// by default if found at compile-time.  
/// However, this crate does not provide currently.
///
/// Returns the new parser context or NULL
#[doc(alias = "htmlCreateFileParserCtxt")]
pub unsafe fn html_create_file_parser_ctxt(
    filename: &str,
    encoding: Option<&str>,
) -> HtmlParserCtxtPtr {
    let ctxt: HtmlParserCtxtPtr = html_new_parser_ctxt();
    if ctxt.is_null() {
        return null_mut();
    }

    let canonic_filename = canonic_path(filename);
    let input_stream: HtmlParserInputPtr =
        xml_load_external_entity(Some(&canonic_filename), None, ctxt);
    if input_stream.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    (*ctxt).input_push(input_stream);

    // set encoding
    if let Some(encoding) = encoding {
        let l = encoding.len();

        if l < 1000 {
            let content = format!("charset={encoding}");
            html_check_encoding(ctxt, &content);
        }
    }

    ctxt
}

/// Parse an HTML file and build a tree. Automatic support for ZLIB/Compress
/// compressed document is provided by default if found at compile-time.
/// It use the given SAX function block to handle the parsing callback.
/// If sax is NULL, fallback to the default DOM tree building routines.
///
/// Returns the resulting document tree unless SAX is NULL or the document is not well formed.
#[doc(alias = "htmlSAXParseFile")]
#[deprecated = "Use htmlNewSAXParserCtxt and htmlCtxtReadFile"]
pub unsafe fn html_sax_parse_file(
    filename: &str,
    encoding: Option<&str>,
    sax: Option<Box<HtmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
) -> HtmlDocPtr {
    let mut oldsax = None;

    xml_init_parser();

    let ctxt: HtmlParserCtxtPtr = html_create_file_parser_ctxt(filename, encoding);
    if ctxt.is_null() {
        return null_mut();
    }
    let replaced = sax.is_some();
    if let Some(sax) = sax {
        oldsax = (*ctxt).sax.replace(sax);
        (*ctxt).user_data = user_data;
    }

    html_parse_document(ctxt);

    let ret: HtmlDocPtr = (*ctxt).my_doc;
    if replaced {
        (*ctxt).sax = oldsax;
        (*ctxt).user_data = None;
    }
    html_free_parser_ctxt(ctxt);

    ret
}

/// Parse an HTML file and build a tree.
///
/// In original libxml2, automatic support for ZLIB/Compress
/// compressed document is provided by default if found at compile-time.  
/// However, this crate does not support currently.
///
/// Returns the resulting document tree
#[doc(alias = "htmlParseFile")]
pub unsafe fn html_parse_file(filename: &str, encoding: Option<&str>) -> HtmlDocPtr {
    html_sax_parse_file(filename, encoding, None, None)
}

/// Take a block of UTF-8 chars in and try to convert it to an ASCII
/// plus HTML entities block of chars out.
///
/// Returns 0 if success, -2 if the transcoding fails, or -1 otherwise.  
/// The value of @inlen after return is the number of octets consumed
/// as the return value is positive, else unpredictable.  
/// The value of @outlen after return is the number of octets consumed.
#[doc(alias = "UTF8ToHtml")]
pub unsafe fn utf8_to_html(
    mut out: *mut u8,
    outlen: *mut i32,
    mut input: *const u8,
    inlen: *mut i32,
) -> i32 {
    let mut processed: *const u8 = input;
    let outstart: *const u8 = out;
    let instart: *const u8 = input;
    let mut c: u32;
    let mut d: u32;
    let mut trailing: i32;

    if out.is_null() || outlen.is_null() || inlen.is_null() {
        return -1;
    }
    if input.is_null() {
        // initialization nothing to do
        *outlen = 0;
        *inlen = 0;
        return 0;
    }
    let inend: *const u8 = input.add(*inlen as usize);
    let outend: *const u8 = out.add(*outlen as usize);
    while input < inend {
        d = *input as _;
        input = input.add(1);
        if d < 0x80 {
            c = d;
            trailing = 0;
        } else if d < 0xC0 {
            // trailing byte in leading position
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        } else if d < 0xE0 {
            c = d & 0x1F;
            trailing = 1;
        } else if d < 0xF0 {
            c = d & 0x0F;
            trailing = 2;
        } else if d < 0xF8 {
            c = d & 0x07;
            trailing = 3;
        } else {
            // no chance for this in Ascii
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        }

        if inend.offset_from(input) < trailing as isize {
            break;
        }

        while trailing != 0 {
            if input >= inend || {
                d = *input as _;
                input = input.add(1);
                d & 0xC0 != 0x80
            } {
                break;
            }
            c <<= 6;
            c |= d & 0x3F;
            trailing -= 1;
        }

        // assertion: c is a single UTF-4 value
        if c < 0x80 {
            if out.add(1) >= outend as _ {
                break;
            }
            *out = c as _;
            out = out.add(1);
        } else {
            let mut nbuf: [c_char; 16] = [0; 16];

            // Try to lookup a predefined HTML entity for it

            let ent: *const HtmlEntityDesc = html_entity_value_lookup(c);
            let cp = if ent.is_null() {
                snprintf(nbuf.as_mut_ptr() as _, nbuf.len(), c"#%u".as_ptr(), c);
                nbuf.as_mut_ptr()
            } else {
                (*ent).name
            };
            let len: i32 = strlen(cp) as _;
            if out.add(2 + len as usize) >= outend as _ {
                break;
            }
            *out = b'&';
            out = out.add(1);
            memcpy(out as _, cp as _, len as usize);
            out = out.add(len as usize);
            *out = b';';
            out = out.add(1);
        }
        processed = input;
    }
    *outlen = out.offset_from(outstart) as _;
    *inlen = processed.offset_from(instart) as _;
    0
}

/// Take a block of UTF-8 chars in and try to convert it to an ASCII
/// plus HTML entities block of chars out.
///
/// Returns 0 if success, -2 if the transcoding fails, or -1 otherwise.  
/// The value of @inlen after return is the number of octets consumed
/// as the return value is positive, else unpredictable.  
/// The value of @outlen after return is the number of octets consumed.  
#[doc(alias = "htmlEncodeEntities")]
pub unsafe fn html_encode_entities(
    mut out: *mut u8,
    outlen: *mut i32,
    mut input: *const u8,
    inlen: *mut i32,
    quote_char: i32,
) -> i32 {
    let mut processed: *const u8 = input;
    let outstart: *const u8 = out;
    let instart: *const u8 = input;
    let mut c: u32;
    let mut d: u32;
    let mut trailing: i32;

    if out.is_null() || outlen.is_null() || inlen.is_null() || input.is_null() {
        return -1;
    }
    let outend: *const u8 = out.add(*outlen as usize);
    let inend: *const u8 = input.add(*inlen as usize);
    while input < inend {
        d = *input as _;
        input = input.add(1);
        if d < 0x80 {
            c = d;
            trailing = 0;
        } else if d < 0xC0 {
            // trailing byte in leading position
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        } else if d < 0xE0 {
            c = d & 0x1F;
            trailing = 1;
        } else if d < 0xF0 {
            c = d & 0x0F;
            trailing = 2;
        } else if d < 0xF8 {
            c = d & 0x07;
            trailing = 3;
        } else {
            // no chance for this in Ascii
            *outlen = out.offset_from(outstart) as _;
            *inlen = processed.offset_from(instart) as _;
            return -2;
        }

        if inend.offset_from(input) < trailing as isize {
            break;
        }

        while trailing != 0 {
            trailing -= 1;
            d = *input as _;
            input = input.add(1);
            if d & 0xC0 != 0x80 {
                *outlen = out.offset_from(outstart) as _;
                *inlen = processed.offset_from(instart) as _;
                return -2;
            }
            c <<= 6;
            c |= d & 0x3F;
        }

        // assertion: c is a single UTF-4 value
        if c < 0x80
            && c != quote_char as u32
            && c != '&' as u32
            && c != '<' as u32
            && c != '>' as u32
        {
            if out >= outend as _ {
                break;
            }
            *out = c as _;
            out = out.add(1);
        } else {
            let mut nbuf: [c_char; 16] = [0; 16];

            // Try to lookup a predefined HTML entity for it
            let ent: *const HtmlEntityDesc = html_entity_value_lookup(c);
            let cp = if ent.is_null() {
                snprintf(nbuf.as_mut_ptr() as _, nbuf.len(), c"#%u".as_ptr() as _, c);
                nbuf.as_mut_ptr()
            } else {
                (*ent).name
            };
            let len: i32 = strlen(cp as _) as _;
            if outend.offset_from(out) < len as isize + 2 {
                break;
            }
            *out = b'&';
            out = out.add(1);
            memcpy(out as _, cp as _, len as usize);
            out = out.add(len as usize);
            *out = b';';
            out = out.add(1);
        }
        processed = input;
    }
    *outlen = out.offset_from(outstart) as _;
    *inlen = processed.offset_from(instart) as _;
    0
}

// The list of HTML attributes which are of content %Script;
// # Note
// when adding ones, check htmlIsScriptAttribute() since
// it assumes the name starts with 'on'
const HTML_SCRIPT_ATTRIBUTES: &[*const c_char] = &[
    c"onclick".as_ptr() as _,
    c"ondblclick".as_ptr() as _,
    c"onmousedown".as_ptr() as _,
    c"onmouseup".as_ptr() as _,
    c"onmouseover".as_ptr() as _,
    c"onmousemove".as_ptr() as _,
    c"onmouseout".as_ptr() as _,
    c"onkeypress".as_ptr() as _,
    c"onkeydown".as_ptr() as _,
    c"onkeyup".as_ptr() as _,
    c"onload".as_ptr() as _,
    c"onunload".as_ptr() as _,
    c"onfocus".as_ptr() as _,
    c"onblur".as_ptr() as _,
    c"onsubmit".as_ptr() as _,
    c"onreset".as_ptr() as _,
    c"onchange".as_ptr() as _,
    c"onselect".as_ptr() as _,
];

/// Check if an attribute is of content type Script
///
/// Returns 1 is the attribute is a script 0 otherwise
#[doc(alias = "htmlIsScriptAttribute")]
pub unsafe fn html_is_script_attribute(name: *const XmlChar) -> i32 {
    if name.is_null() {
        return 0;
    }
    // all script attributes start with 'on'
    if *name.add(0) != b'o' || *name.add(1) != b'n' {
        return 0;
    }

    for &attr in HTML_SCRIPT_ATTRIBUTES {
        if xml_str_equal(name, attr as _) {
            return 1;
        }
    }
    0
}

/// Set and return the previous value for handling HTML omitted tags.
///
/// Returns the last value for 0 for no handling, 1 for auto insertion.
#[doc(alias = "htmlHandleOmittedElem")]
pub unsafe fn html_handle_omitted_elem(val: i32) -> i32 {
    let old: i32 = HTML_OMITTED_DEFAULT_VALUE.load(Ordering::Acquire);

    HTML_OMITTED_DEFAULT_VALUE.store(val, Ordering::Release);
    old
}

/// Create a new input stream structure
/// Returns the new input stream or NULL
#[doc(alias = "htmlNewInputStream")]
#[cfg(feature = "libxml_push")]
unsafe fn html_new_input_stream(ctxt: HtmlParserCtxtPtr) -> HtmlParserInputPtr {
    let input: HtmlParserInputPtr = xml_malloc(size_of::<HtmlParserInput>()) as XmlParserInputPtr;
    if input.is_null() {
        html_err_memory(ctxt, Some("couldn't allocate a new input stream\n"));
        return null_mut();
    }
    memset(input as _, 0, size_of::<HtmlParserInput>());
    std::ptr::write(&mut *input, HtmlParserInput::default());
    (*input).filename = None;
    (*input).directory = None;
    (*input).base = null_mut();
    (*input).cur = null_mut();
    // (*input).buf = None;
    std::ptr::write(&raw mut (*input).buf, None);
    (*input).line = 1;
    (*input).col = 1;
    (*input).free = None;
    (*input).version = None;
    (*input).consumed = 0;
    (*input).length = 0;
    input
}

/// Create a parser context for using the HTML parser in push mode
/// The value of `filename` is used for fetching external entities
/// and error/warning reports.
///
/// Returns the new parser context or NULL
#[doc(alias = "htmlCreatePushParserCtxt")]
#[cfg(feature = "libxml_push")]
pub unsafe fn html_create_push_parser_ctxt(
    sax: Option<Box<XmlSAXHandler>>,
    user_data: Option<GenericErrorContext>,
    chunk: *const c_char,
    size: i32,
    filename: *const c_char,
    enc: XmlCharEncoding,
) -> HtmlParserCtxtPtr {
    use std::slice::from_raw_parts;

    use crate::io::{xml_parser_get_directory, XmlParserInputBuffer};

    xml_init_parser();

    let buf = XmlParserInputBuffer::new(enc);

    let ctxt: HtmlParserCtxtPtr = html_new_sax_parser_ctxt(sax, user_data);
    if ctxt.is_null() {
        return null_mut();
    }
    if matches!(enc, XmlCharEncoding::UTF8) || buf.encoder.is_some() {
        (*ctxt).charset = XmlCharEncoding::UTF8;
    }
    if filename.is_null() {
        (*ctxt).directory = None;
    } else if let Some(dir) =
        xml_parser_get_directory(CStr::from_ptr(filename).to_string_lossy().as_ref())
    {
        (*ctxt).directory = Some(dir.to_string_lossy().into_owned());
    }

    let input_stream: HtmlParserInputPtr = html_new_input_stream(ctxt);
    if input_stream.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }

    if filename.is_null() {
        (*input_stream).filename = None;
    } else {
        let canonic = xml_canonic_path(filename as _);
        (*input_stream).filename = Some(
            CStr::from_ptr(canonic as *const i8)
                .to_string_lossy()
                .into_owned(),
        );
        xml_free(canonic as _);
    }
    // (*input_stream).buf = Some(buf);
    std::ptr::write(
        &raw mut (*input_stream).buf,
        Some(Rc::new(RefCell::new(buf))),
    );
    (*input_stream).reset_base();

    (*ctxt).input_push(input_stream);

    if size > 0 && !chunk.is_null() && !(*ctxt).input.is_null() && (*(*ctxt).input).buf.is_some() {
        let base: size_t = (*(*ctxt).input).get_base();
        let cur = (*(*ctxt).input).offset_from_base();

        (*(*ctxt).input)
            .buf
            .as_mut()
            .unwrap()
            .borrow_mut()
            .push_bytes(from_raw_parts(chunk as *const u8, size as usize));
        (*(*ctxt).input).set_base_and_cursor(base, cur);
    }
    (*ctxt).progressive = 1;

    ctxt
}

/// Try to find if a sequence (first, next, third) or just (first next) or
/// (first) is available in the input stream.
/// This function has a side effect of (possibly) incrementing (*ctxt).checkIndex
/// to avoid rescanning sequences of bytes, it DOES change the state of the
/// parser, do not use liberally.
/// This is basically similar to xmlParseLookupSequence()
///
/// Returns the index to the current parsing point if the full sequence is available, -1 otherwise.
#[doc(alias = "htmlParseLookupSequence")]
#[cfg(feature = "libxml_push")]
unsafe fn html_parse_lookup_sequence(
    ctxt: HtmlParserCtxtPtr,
    first: XmlChar,
    next: XmlChar,
    third: XmlChar,
    ignoreattrval: i32,
) -> i32 {
    let mut quote: i32;

    let input: HtmlParserInputPtr = (*ctxt).input;
    if input.is_null() {
        return -1;
    }

    let base: size_t = (*ctxt).check_index as _;
    quote = (*ctxt).end_check_state;

    let buf: *const XmlChar = (*input).cur;
    let mut len = (*input).remainder_len();

    // take into account the sequence length
    if third != 0 {
        len -= 2;
    } else if next != 0 {
        len -= 1;
    }
    for base in base..len {
        if base >= INT_MAX as usize / 2 {
            (*ctxt).check_index = 0;
            (*ctxt).end_check_state = 0;
            return base as i32 - 2;
        }
        if ignoreattrval != 0 {
            if quote != 0 {
                if *buf.add(base) == quote as u8 {
                    quote = 0;
                }
                continue;
            }
            if *buf.add(base) == b'"' || *buf.add(base) == b'\'' {
                quote = *buf.add(base) as _;
                continue;
            }
        }
        if *buf.add(base) == first {
            if third != 0 {
                if *buf.add(base + 1) != next || *buf.add(base + 2) != third {
                    continue;
                }
            } else if next != 0 && *buf.add(base + 1) != next {
                continue;
            }
            (*ctxt).check_index = 0;
            (*ctxt).end_check_state = 0;
            return base as _;
        }
    }
    (*ctxt).check_index = base.max(len) as _;
    (*ctxt).end_check_state = quote;
    -1
}

/// Try to find a comment end tag in the input stream
/// The search includes "-->" as well as WHATWG-recommended incorrectly-closed tags.
/// (See https://html.spec.whatwg.org/multipage/parsing.html#parse-error-incorrectly-closed-comment)
/// This function has a side effect of (possibly) incrementing (*ctxt).checkIndex
/// to avoid rescanning sequences of bytes,
/// it DOES change the state of the parser, do not use liberally.  
/// This wraps to htmlParseLookupSequence()
///
/// Returns the index to the current parsing point if the full sequence is available, -1 otherwise.
#[doc(alias = "htmlParseLookupCommentEnd")]
#[cfg(feature = "libxml_push")]
unsafe fn html_parse_lookup_comment_end(ctxt: HtmlParserCtxtPtr) -> i32 {
    let mut mark: i32;

    loop {
        mark = html_parse_lookup_sequence(ctxt, b'-', b'-', 0, 0);
        if mark < 0 {
            break;
        }
        if NXT!(ctxt, mark + 2) == b'>'
            || (NXT!(ctxt, mark + 2) == b'!' && NXT!(ctxt, mark + 3) == b'>')
        {
            (*ctxt).check_index = 0;
            break;
        }
        let offset = if NXT!(ctxt, mark + 2) == b'!' { 3 } else { 2 };
        if mark + offset >= (*(*ctxt).input).remainder_len() as i32 {
            (*ctxt).check_index = mark as _;
            return -1;
        }
        (*ctxt).check_index = mark as i64 + 1;
    }
    mark
}

/// Try to progress on parsing
///
/// Returns zero if no parsing was possible
#[doc(alias = "htmlParseTryOrFinish")]
#[cfg(feature = "libxml_push")]
unsafe fn html_parse_try_or_finish(ctxt: HtmlParserCtxtPtr, terminate: i32) -> i32 {
    let ret: i32 = 0;
    let mut input: HtmlParserInputPtr;
    let mut avail = 0;
    let mut cur: XmlChar;
    let mut next: XmlChar;

    'done: loop {
        input = (*ctxt).input;
        if input.is_null() {
            break;
        }
        avail = (*input).remainder_len();
        if avail == 0 && terminate != 0 {
            html_auto_close_on_end(ctxt);
            if (*ctxt).name_tab.is_empty()
                && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
            {
                // SAX: end of the document processing.
                (*ctxt).instate = XmlParserInputState::XmlParserEOF;
                if let Some(end_document) =
                    (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_document)
                {
                    end_document((*ctxt).user_data.clone());
                }
            }
        }
        if avail < 1 {
            // goto done;
            break 'done;
        }
        // This is done to make progress and avoid an infinite loop
        // if a parsing attempt was aborted by hitting a NUL byte. After
        // changing html_current_char, this probably isn't necessary anymore.
        // We should consider removing this check.
        cur = *(*input).cur.add(0);
        if cur == 0 {
            (*ctxt).advance(1);
            continue;
        }

        match (*ctxt).instate {
            XmlParserInputState::XmlParserEOF => {
                // Document parsing is done !
                // goto done;
                break 'done;
            }
            XmlParserInputState::XmlParserStart => {
                // Very first chars read from the document flow.
                cur = *(*input).cur.add(0);
                if xml_is_blank_char(cur as u32) {
                    SKIP_BLANKS!(ctxt);
                    avail = (*input).remainder_len();
                }
                if let Some(set_document_locator) = (*ctxt)
                    .sax
                    .as_deref_mut()
                    .and_then(|sax| sax.set_document_locator)
                {
                    set_document_locator((*ctxt).user_data.clone(), xml_default_sax_locator());
                }
                if (*ctxt).disable_sax == 0 {
                    if let Some(start_document) = (*ctxt)
                        .sax
                        .as_deref_mut()
                        .and_then(|sax| sax.start_document)
                    {
                        start_document((*ctxt).user_data.clone());
                    }
                }

                cur = *(*input).cur.add(0);
                next = *(*input).cur.add(1);
                if cur == b'<'
                    && next == b'!'
                    && UPP!(ctxt, 2) == b'D'
                    && UPP!(ctxt, 3) == b'O'
                    && UPP!(ctxt, 4) == b'C'
                    && UPP!(ctxt, 5) == b'T'
                    && UPP!(ctxt, 6) == b'Y'
                    && UPP!(ctxt, 7) == b'P'
                    && UPP!(ctxt, 8) == b'E'
                {
                    if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 1) < 0 {
                        // goto done;
                        break 'done;
                    }
                    html_parse_doc_type_decl(ctxt);
                    (*ctxt).instate = XmlParserInputState::XmlParserProlog;
                } else {
                    (*ctxt).instate = XmlParserInputState::XmlParserMisc;
                }
            }
            XmlParserInputState::XmlParserMisc => {
                SKIP_BLANKS!(ctxt);
                avail = (*input).remainder_len();
                // no chars input buffer
                if avail < 1 {
                    // goto done;
                    break 'done;
                }
                // not enough chars input buffer
                if avail < 2 {
                    if terminate == 0 {
                        // goto done;
                        break 'done;
                    } else {
                        next = b' ';
                    }
                } else {
                    next = *(*input).cur.add(1);
                }
                cur = *(*input).cur.add(0);
                if cur == b'<'
                    && next == b'!'
                    && *(*input).cur.add(2) == b'-'
                    && *(*input).cur.add(3) == b'-'
                {
                    if terminate == 0 && html_parse_lookup_comment_end(ctxt) < 0 {
                        // goto done;
                        break 'done;
                    }
                    html_parse_comment(ctxt);
                    (*ctxt).instate = XmlParserInputState::XmlParserMisc;
                } else if cur == b'<' && next == b'?' {
                    if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 0) < 0 {
                        // goto done;
                        break 'done;
                    }
                    html_parse_pi(ctxt);
                    (*ctxt).instate = XmlParserInputState::XmlParserMisc;
                } else if cur == b'<'
                    && next == b'!'
                    && UPP!(ctxt, 2) == b'D'
                    && UPP!(ctxt, 3) == b'O'
                    && UPP!(ctxt, 4) == b'C'
                    && UPP!(ctxt, 5) == b'T'
                    && UPP!(ctxt, 6) == b'Y'
                    && UPP!(ctxt, 7) == b'P'
                    && UPP!(ctxt, 8) == b'E'
                {
                    if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 1) < 0 {
                        // goto done;
                        break 'done;
                    }
                    html_parse_doc_type_decl(ctxt);
                    (*ctxt).instate = XmlParserInputState::XmlParserProlog;
                } else if cur == b'<' && next == b'!' && avail < 9 {
                    // goto done;
                    break 'done;
                } else {
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                }
            }
            XmlParserInputState::XmlParserProlog => {
                SKIP_BLANKS!(ctxt);
                avail = (*input).remainder_len();
                if avail < 2 {
                    // goto done;
                    break 'done;
                }
                cur = *(*input).cur.add(0);
                next = *(*input).cur.add(1);
                if cur == b'<'
                    && next == b'!'
                    && *(*input).cur.add(2) == b'-'
                    && *(*input).cur.add(3) == b'-'
                {
                    if terminate == 0 && html_parse_lookup_comment_end(ctxt) < 0 {
                        // goto done;
                        break 'done;
                    }
                    html_parse_comment(ctxt);
                    (*ctxt).instate = XmlParserInputState::XmlParserProlog;
                } else if cur == b'<' && next == b'?' {
                    if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 0) < 0 {
                        // goto done;
                        break 'done;
                    }
                    html_parse_pi(ctxt);
                    (*ctxt).instate = XmlParserInputState::XmlParserProlog;
                } else if cur == b'<' && next == b'!' && avail < 4 {
                    // goto done;
                    break 'done;
                } else {
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                }
            }
            XmlParserInputState::XmlParserEpilog => {
                avail = (*input).remainder_len();
                if avail < 1 {
                    // goto done;
                    break 'done;
                }
                cur = *(*input).cur.add(0);
                if xml_is_blank_char(cur as u32) {
                    html_parse_char_data(ctxt);
                    // goto done;
                    break 'done;
                }
                if avail < 2 {
                    // goto done;
                    break 'done;
                }
                next = *(*input).cur.add(1);
                if cur == b'<'
                    && next == b'!'
                    && *(*input).cur.add(2) == b'-'
                    && *(*input).cur.add(3) == b'-'
                {
                    if terminate == 0 && html_parse_lookup_comment_end(ctxt) < 0 {
                        // goto done;
                        break 'done;
                    }
                    html_parse_comment(ctxt);
                    (*ctxt).instate = XmlParserInputState::XmlParserEpilog;
                } else if cur == b'<' && next == b'?' {
                    if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 0) < 0 {
                        // goto done;
                        break 'done;
                    }
                    html_parse_pi(ctxt);
                    (*ctxt).instate = XmlParserInputState::XmlParserEpilog;
                } else if cur == b'<' && next == b'!' && avail < 4 {
                    // goto done;
                    break 'done;
                } else {
                    (*ctxt).err_no = XmlParserErrors::XmlErrDocumentEnd as i32;
                    (*ctxt).well_formed = 0;
                    (*ctxt).instate = XmlParserInputState::XmlParserEOF;
                    if let Some(end_document) =
                        (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_document)
                    {
                        end_document((*ctxt).user_data.clone());
                    }
                    // goto done;
                    break 'done;
                }
            }
            XmlParserInputState::XmlParserStartTag => 'to_break: {
                // no chars in buffer
                if avail < 1 {
                    // goto done;
                    break 'done;
                }
                // not enough chars in buffer
                if avail < 2 {
                    if terminate == 0 {
                        // goto done;
                        break 'done;
                    } else {
                        next = b' ';
                    }
                } else {
                    next = *(*input).cur.add(1);
                }
                cur = *(*input).cur.add(0);
                if cur != b'<' {
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    break 'to_break;
                }
                if next == b'/' {
                    (*ctxt).instate = XmlParserInputState::XmlParserEndTag;
                    (*ctxt).check_index = 0;
                    break 'to_break;
                }
                if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 1) < 0 {
                    // goto done;
                    break 'done;
                }

                // Capture start position
                let mut node_info = HtmlParserNodeInfo::default();
                if (*ctxt).record_info != 0 {
                    node_info.begin_pos =
                        (*(*ctxt).input).consumed + (*(*ctxt).input).offset_from_base() as u64;
                    node_info.begin_line = (*(*ctxt).input).line as _;
                }

                let failed: i32 = html_parse_start_tag(ctxt);
                let Some(name) = (*ctxt).name.clone().filter(|_| failed != -1) else {
                    if (*ctxt).current_byte() == b'>' {
                        (*ctxt).skip_char();
                    }
                    break 'to_break;
                };

                // Lookup the info for that element.
                let info = html_tag_lookup(&name);
                if info.is_none() {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlHTMLUnknownTag,
                        format!("Tag {name} invalid\n").as_str(),
                        Some(&name),
                        None,
                    );
                }

                // Check for an Empty Element labeled the XML/SGML way
                if (*ctxt).current_byte() == b'/' && NXT!(ctxt, 1) == b'>' {
                    (*ctxt).advance(2);
                    if let Some(end_element) =
                        (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element)
                    {
                        end_element((*ctxt).user_data.clone(), &name);
                    }
                    html_name_pop(ctxt);
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    break 'to_break;
                }

                if (*ctxt).current_byte() == b'>' {
                    (*ctxt).skip_char();
                } else {
                    html_parse_err(
                        ctxt,
                        XmlParserErrors::XmlErrGtRequired,
                        format!("Couldn't find end of Start Tag {name}\n").as_str(),
                        Some(&name),
                        None,
                    );

                    // end of parsing of this node.
                    if Some(name.as_str()) == (*ctxt).name.as_deref() {
                        (*ctxt).node_pop();
                        html_name_pop(ctxt);
                    }

                    if (*ctxt).record_info != 0 {
                        html_node_info_push(ctxt, Rc::new(RefCell::new(node_info)));
                    }

                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    break 'to_break;
                }

                // Check for an Empty Element from DTD definition
                if info.map_or(false, |info| info.empty != 0) {
                    if let Some(end_element) =
                        (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_element)
                    {
                        end_element((*ctxt).user_data.clone(), &name);
                    }
                    html_name_pop(ctxt);
                }

                if (*ctxt).record_info != 0 {
                    html_node_info_push(ctxt, Rc::new(RefCell::new(node_info)));
                }

                (*ctxt).instate = XmlParserInputState::XmlParserContent;
            }
            XmlParserInputState::XmlParserContent => 'to_break: {
                let mut chr: [XmlChar; 2] = [0, 0];

                // Handle preparsed entities and charRef
                if (*ctxt).token != 0 {
                    chr[0] = (*ctxt).token as _;
                    html_check_paragraph(ctxt);
                    if let Some(characters) =
                        (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
                    {
                        let s = from_utf8(&chr[..1]).expect("Internal Error");
                        characters((*ctxt).user_data.clone(), s);
                    }
                    (*ctxt).token = 0;
                    (*ctxt).check_index = 0;
                }
                if avail == 1 && terminate != 0 {
                    cur = *(*input).cur.add(0);
                    if cur != b'<' && cur != b'&' {
                        if let Some(sax) = (*ctxt).sax.as_deref_mut() {
                            chr[0] = cur;
                            let s = from_utf8(&chr[..1]).expect("Internal Error");
                            if xml_is_blank_char(cur as u32) {
                                if (*ctxt).keep_blanks != 0 {
                                    if let Some(characters) = sax.characters {
                                        characters((*ctxt).user_data.clone(), s);
                                    }
                                } else if let Some(ignorable_whitespace) = sax.ignorable_whitespace
                                {
                                    ignorable_whitespace((*ctxt).user_data.clone(), s);
                                }
                            } else {
                                html_check_paragraph(ctxt);
                                if let Some(characters) = sax.characters {
                                    characters((*ctxt).user_data.clone(), s);
                                }
                            }
                        }
                        (*ctxt).token = 0;
                        (*ctxt).check_index = 0;
                        (*input).cur = (*input).cur.add(1);
                        break 'to_break;
                    }
                }
                if avail < 2 {
                    // goto done;
                    break 'done;
                }
                cur = *(*input).cur.add(0);
                next = *(*input).cur.add(1);
                if (*ctxt).name.as_deref() == Some("script")
                    || (*ctxt).name.as_deref() == Some("style")
                {
                    // Handle SCRIPT/STYLE separately
                    if terminate == 0 {
                        let idx: i32 = html_parse_lookup_sequence(ctxt, b'<', b'/', 0, 0);
                        if idx < 0 {
                            // goto done;
                            break 'done;
                        }
                        let val: XmlChar = *(*input).cur.add(idx as usize + 2);
                        if val == 0 {
                            // bad cut of input
                            // FIXME: htmlParseScript checks for additional characters after '</'.
                            (*ctxt).check_index = idx as _;
                            // goto done;
                            break 'done;
                        }
                    }
                    html_parse_script(ctxt);
                    if cur == b'<' && next == b'/' {
                        (*ctxt).instate = XmlParserInputState::XmlParserEndTag;
                        (*ctxt).check_index = 0;
                        break 'to_break;
                    }
                } else if cur == b'<' && next == b'!' {
                    if avail < 4 {
                        // goto done;
                        break 'done;
                    }
                    // Sometimes DOCTYPE arrives in the middle of the document
                    if UPP!(ctxt, 2) == b'D'
                        && UPP!(ctxt, 3) == b'O'
                        && UPP!(ctxt, 4) == b'C'
                        && UPP!(ctxt, 5) == b'T'
                        && UPP!(ctxt, 6) == b'Y'
                        && UPP!(ctxt, 7) == b'P'
                        && UPP!(ctxt, 8) == b'E'
                    {
                        if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 1) < 0 {
                            // goto done;
                            break 'done;
                        }
                        html_parse_err(
                            ctxt,
                            XmlParserErrors::XmlHTMLStrucureError,
                            "Misplaced DOCTYPE declaration\n",
                            Some("DOCTYPE"),
                            None,
                        );
                        html_parse_doc_type_decl(ctxt);
                    } else if *(*input).cur.add(2) == b'-' && *(*input).cur.add(3) == b'-' {
                        if terminate == 0 && html_parse_lookup_comment_end(ctxt) < 0 {
                            // goto done;
                            break 'done;
                        }
                        html_parse_comment(ctxt);
                        (*ctxt).instate = XmlParserInputState::XmlParserContent;
                    } else {
                        if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 0) < 0 {
                            // goto done;
                            break 'done;
                        }
                        html_skip_bogus_comment(ctxt);
                    }
                } else if cur == b'<' && next == b'?' {
                    if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 0) < 0 {
                        // goto done;
                        break 'done;
                    }
                    html_parse_pi(ctxt);
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                } else if cur == b'<' && next == b'/' {
                    (*ctxt).instate = XmlParserInputState::XmlParserEndTag;
                    (*ctxt).check_index = 0;
                    break 'to_break;
                } else if cur == b'<' && next.is_ascii_alphabetic() {
                    if terminate == 0 && (next == 0) {
                        // goto done;
                        break 'done;
                    }
                    (*ctxt).instate = XmlParserInputState::XmlParserStartTag;
                    (*ctxt).check_index = 0;
                    break 'to_break;
                } else if cur == b'<' {
                    if (*ctxt).disable_sax == 0 {
                        if let Some(characters) =
                            (*ctxt).sax.as_deref_mut().and_then(|sax| sax.characters)
                        {
                            characters((*ctxt).user_data.clone(), "<");
                        }
                    }
                    (*ctxt).skip_char();
                } else {
                    // check that the text sequence is complete
                    // before handing out the data to the parser
                    // to avoid problems with erroneous end of
                    // data detection.
                    if terminate == 0 && html_parse_lookup_sequence(ctxt, b'<', 0, 0, 0) < 0 {
                        // goto done;
                        break 'done;
                    }
                    (*ctxt).check_index = 0;
                    while !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
                        && cur != b'<'
                        && (*input).cur < (*input).end
                    {
                        if cur == b'&' {
                            html_parse_reference(ctxt);
                        } else {
                            html_parse_char_data(ctxt);
                        }
                        cur = *(*input).cur.add(0);
                    }
                }
            }
            XmlParserInputState::XmlParserEndTag => {
                if avail < 2 {
                    // goto done;
                    break 'done;
                }
                if terminate == 0 && html_parse_lookup_sequence(ctxt, b'>', 0, 0, 0) < 0 {
                    // goto done;
                    break 'done;
                }
                html_parse_end_tag(ctxt);
                if (*ctxt).name_tab.is_empty() {
                    (*ctxt).instate = XmlParserInputState::XmlParserEpilog;
                } else {
                    (*ctxt).instate = XmlParserInputState::XmlParserContent;
                }
                (*ctxt).check_index = 0;
            }
            XmlParserInputState::XmlParserCDATASection => {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "HPP: internal error, state == CDATA\n",
                    None,
                    None,
                );
                (*ctxt).instate = XmlParserInputState::XmlParserContent;
                (*ctxt).check_index = 0;
            }
            XmlParserInputState::XmlParserDTD => {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "HPP: internal error, state == DTD\n",
                    None,
                    None,
                );
                (*ctxt).instate = XmlParserInputState::XmlParserContent;
                (*ctxt).check_index = 0;
            }
            XmlParserInputState::XmlParserComment => {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "HPP: internal error, state == COMMENT\n",
                    None,
                    None,
                );
                (*ctxt).instate = XmlParserInputState::XmlParserContent;
                (*ctxt).check_index = 0;
            }
            XmlParserInputState::XmlParserPI => {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "HPP: internal error, state == PI\n",
                    None,
                    None,
                );
                (*ctxt).instate = XmlParserInputState::XmlParserContent;
                (*ctxt).check_index = 0;
            }
            XmlParserInputState::XmlParserEntityDecl => {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "HPP: internal error, state == ENTITY_DECL\n",
                    None,
                    None,
                );
                (*ctxt).instate = XmlParserInputState::XmlParserContent;
                (*ctxt).check_index = 0;
            }
            XmlParserInputState::XmlParserEntityValue => {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "HPP: internal error, state == ENTITY_VALUE\n",
                    None,
                    None,
                );
                (*ctxt).instate = XmlParserInputState::XmlParserContent;
                (*ctxt).check_index = 0;
            }
            XmlParserInputState::XmlParserAttributeValue => {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "HPP: internal error, state == ATTRIBUTE_VALUE\n",
                    None,
                    None,
                );
                (*ctxt).instate = XmlParserInputState::XmlParserStartTag;
                (*ctxt).check_index = 0;
            }
            XmlParserInputState::XmlParserSystemLiteral => {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "HPP: internal error, state == XML_PARSER_SYSTEM_LITERAL\n",
                    None,
                    None,
                );
                (*ctxt).instate = XmlParserInputState::XmlParserContent;
                (*ctxt).check_index = 0;
            }
            XmlParserInputState::XmlParserIgnore => {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "HPP: internal error, state == XML_PARSER_IGNORE\n",
                    None,
                    None,
                );
                (*ctxt).instate = XmlParserInputState::XmlParserContent;
                (*ctxt).check_index = 0;
            }
            XmlParserInputState::XmlParserPublicLiteral => {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInternalError,
                    "HPP: internal error, state == XML_PARSER_LITERAL\n",
                    None,
                    None,
                );
                (*ctxt).instate = XmlParserInputState::XmlParserContent;
                (*ctxt).check_index = 0;
            }
        }
    }
    // done:
    if avail == 0 && terminate != 0 {
        html_auto_close_on_end(ctxt);
        if (*ctxt).name_tab.is_empty()
            && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            // SAX: end of the document processing.
            (*ctxt).instate = XmlParserInputState::XmlParserEOF;
            if let Some(end_document) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_document)
            {
                end_document((*ctxt).user_data.clone());
            }
        }
    }
    if (*ctxt).options & HtmlParserOption::HtmlParseNodefdtd as i32 == 0
        && !(*ctxt).my_doc.is_null()
        && (terminate != 0
            || matches!(
                (*ctxt).instate,
                XmlParserInputState::XmlParserEOF | XmlParserInputState::XmlParserEpilog
            ))
    {
        let dtd: XmlDtdPtr = (*(*ctxt).my_doc).get_int_subset();
        if dtd.is_null() {
            (*(*ctxt).my_doc).int_subset = xml_create_int_subset(
                (*ctxt).my_doc,
                Some("html"),
                Some("-//W3C//DTD HTML 4.0 Transitional//EN"),
                Some("http://www.w3.org/TR/REC-html40/loose.dtd"),
            );
        }
    }
    ret
}

/// Parse a Chunk of memory
///
/// Returns zero if no error, the xmlParserErrors otherwise.
#[doc(alias = "htmlParseChunk")]
#[cfg(feature = "libxml_push")]
pub unsafe fn html_parse_chunk(
    ctxt: HtmlParserCtxtPtr,
    chunk: *const c_char,
    size: i32,
    terminate: i32,
) -> i32 {
    use std::slice::from_raw_parts;

    if ctxt.is_null() || (*ctxt).input.is_null() {
        html_parse_err(
            ctxt,
            XmlParserErrors::XmlErrInternalError,
            "htmlParseChunk: context error\n",
            None,
            None,
        );
        return XmlParserErrors::XmlErrInternalError as i32;
    }
    if size > 0
        && !chunk.is_null()
        && !(*ctxt).input.is_null()
        && (*(*ctxt).input).buf.is_some()
        && !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        let base: size_t = (*(*ctxt).input).get_base();
        let cur = (*(*ctxt).input).offset_from_base();

        let res: i32 = (*(*ctxt).input)
            .buf
            .as_mut()
            .unwrap()
            .borrow_mut()
            .push_bytes(from_raw_parts(chunk as *const u8, size as usize));
        (*(*ctxt).input).set_base_and_cursor(base, cur);
        if res < 0 {
            html_err_memory(ctxt, None);
            return (*ctxt).err_no;
        }
    } else if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        && (!(*ctxt).input.is_null() && (*(*ctxt).input).buf.is_some())
    {
        let input = (*(*ctxt).input).buf.as_mut().unwrap();
        if input.borrow().encoder.is_some()
            && input.borrow().buffer.is_some()
            && input.borrow().raw.is_some()
        {
            let base: size_t = (*(*ctxt).input).get_base();
            let current: size_t = (*(*ctxt).input).cur.offset_from((*(*ctxt).input).base) as _;

            let res = input.borrow_mut().decode(terminate != 0);
            (*(*ctxt).input).set_base_and_cursor(base, current);
            if res.is_err() {
                html_parse_err(
                    ctxt,
                    XmlParserErrors::XmlErrInvalidEncoding,
                    "encoder error\n",
                    None,
                    None,
                );
                return XmlParserErrors::XmlErrInvalidEncoding as i32;
            }
        }
    }
    html_parse_try_or_finish(ctxt, terminate);
    if terminate != 0 {
        if !matches!(
            (*ctxt).instate,
            XmlParserInputState::XmlParserEOF
                | XmlParserInputState::XmlParserEpilog
                | XmlParserInputState::XmlParserMisc
        ) {
            (*ctxt).err_no = XmlParserErrors::XmlErrDocumentEnd as i32;
            (*ctxt).well_formed = 0;
        }
        if !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF) {
            if let Some(end_document) = (*ctxt).sax.as_deref_mut().and_then(|sax| sax.end_document)
            {
                end_document((*ctxt).user_data.clone());
            }
        }
        (*ctxt).instate = XmlParserInputState::XmlParserEOF;
    }
    (*ctxt).err_no
}

/// Free all the memory used by a parser context.   
/// However the parsed document in (*ctxt).myDoc is not freed.
#[doc(alias = "htmlFreeParserCtxt")]
pub unsafe fn html_free_parser_ctxt(ctxt: HtmlParserCtxtPtr) {
    xml_free_parser_ctxt(ctxt);
}

/// This is the set of XML parser options that can be passed down
/// to the xmlReadDoc() and similar calls.
#[doc(alias = "xmlParserOption")]
#[repr(C)]
pub enum HtmlParserOption {
    HtmlParseRecover = 1 << 0,    /* Relaxed parsing */
    HtmlParseNodefdtd = 1 << 2,   /* do not default a doctype if not found */
    HtmlParseNoerror = 1 << 5,    /* suppress error reports */
    HtmlParseNowarning = 1 << 6,  /* suppress warning reports */
    HtmlParsePedantic = 1 << 7,   /* pedantic error reporting */
    HtmlParseNoblanks = 1 << 8,   /* remove blank nodes */
    HtmlParseNonet = 1 << 11,     /* Forbid network access */
    HtmlParseNoimplied = 1 << 13, /* Do not add implied html/body... elements */
    HtmlParseCompact = 1 << 16,   /* compact small text nodes */
    HtmlParseIgnoreEnc = 1 << 21, /* ignore internal document encoding hint */
}

/// Free a string if it is not owned by the "dict" dictionary in the current scope
macro_rules! DICT_FREE {
    ($dict:expr, $str:expr) => {
        if !$str.is_null()
            && ($dict.is_null() || crate::libxml::dict::xml_dict_owns($dict, $str) == 0)
        {
            xml_free($str as _);
        }
    };
}

/// Reset a parser context
#[doc(alias = "htmlCtxtReset")]
pub unsafe fn html_ctxt_reset(ctxt: HtmlParserCtxtPtr) {
    let mut input: XmlParserInputPtr;

    if ctxt.is_null() {
        return;
    }

    xml_init_parser();

    while {
        input = (*ctxt).input_pop();
        !input.is_null()
    } {
        // Non consuming
        xml_free_input_stream(input);
    }
    (*ctxt).input_tab.clear();
    (*ctxt).input = null_mut();

    (*ctxt).space_tab.clear();

    (*ctxt).node_tab.clear();
    (*ctxt).node = null_mut();

    (*ctxt).name_tab.clear();
    (*ctxt).name = None;

    (*ctxt).ns_tab.clear();

    (*ctxt).version = None;
    (*ctxt).encoding = None;
    (*ctxt).directory = None;
    (*ctxt).ext_sub_uri = None;
    (*ctxt).ext_sub_system = None;
    if !(*ctxt).my_doc.is_null() {
        xml_free_doc((*ctxt).my_doc);
    }
    (*ctxt).my_doc = null_mut();

    (*ctxt).standalone = -1;
    (*ctxt).has_external_subset = 0;
    (*ctxt).has_perefs = 0;
    (*ctxt).html = 1;
    (*ctxt).external = 0;
    (*ctxt).instate = XmlParserInputState::XmlParserStart;
    (*ctxt).token = 0;

    (*ctxt).well_formed = 1;
    (*ctxt).ns_well_formed = 1;
    (*ctxt).disable_sax = 0;
    (*ctxt).valid = 1;
    (*ctxt).vctxt.user_data = Some(GenericErrorContext::new(ctxt));
    (*ctxt).vctxt.flags = XML_VCTXT_USE_PCTXT as _;
    (*ctxt).vctxt.error = Some(parser_validity_error);
    (*ctxt).vctxt.warning = Some(parser_validity_warning);
    (*ctxt).record_info = 0;
    (*ctxt).check_index = 0;
    (*ctxt).end_check_state = 0;
    (*ctxt).in_subset = 0;
    (*ctxt).err_no = XmlParserErrors::XmlErrOK as i32;
    (*ctxt).depth = 0;
    (*ctxt).charset = XmlCharEncoding::None;
    #[cfg(feature = "catalog")]
    {
        (*ctxt).catalogs = None;
    }
    (*ctxt).node_seq.clear();

    if let Some(mut table) = (*ctxt).atts_default.take().map(|t| t.into_inner()) {
        table.clear_with(|data, _| xml_free(data as _));
    }

    let _ = (*ctxt).atts_special.take().map(|t| t.into_inner());

    (*ctxt).nb_errors = 0;
    (*ctxt).nb_warnings = 0;
    if (*ctxt).last_error.is_err() {
        (*ctxt).last_error.reset();
    }
}

/// Applies the options to the parser context
///
/// Returns 0 in case of success, the set of unknown or unimplemented options in case of error.
#[doc(alias = "htmlCtxtUseOptions")]
pub unsafe fn html_ctxt_use_options(ctxt: HtmlParserCtxtPtr, mut options: i32) -> i32 {
    if ctxt.is_null() {
        return -1;
    }

    if options & HtmlParserOption::HtmlParseNowarning as i32 != 0 {
        if let Some(sax) = (*ctxt).sax.as_deref_mut() {
            sax.warning = None;
        }
        (*ctxt).vctxt.warning = None;
        options -= XmlParserOption::XmlParseNowarning as i32;
        (*ctxt).options |= XmlParserOption::XmlParseNowarning as i32;
    }
    if options & HtmlParserOption::HtmlParseNoerror as i32 != 0 {
        if let Some(sax) = (*ctxt).sax.as_deref_mut() {
            sax.error = None;
            sax.fatal_error = None;
        }
        (*ctxt).vctxt.error = None;
        options -= XmlParserOption::XmlParseNoerror as i32;
        (*ctxt).options |= XmlParserOption::XmlParseNoerror as i32;
    }
    if options & HtmlParserOption::HtmlParsePedantic as i32 != 0 {
        (*ctxt).pedantic = 1;
        options -= XmlParserOption::XmlParsePedantic as i32;
        (*ctxt).options |= XmlParserOption::XmlParsePedantic as i32;
    } else {
        (*ctxt).pedantic = 0;
    }
    if options & XmlParserOption::XmlParseNoblanks as i32 != 0 {
        (*ctxt).keep_blanks = 0;
        if let Some(sax) = (*ctxt).sax.as_deref_mut() {
            sax.ignorable_whitespace = Some(xml_sax2_ignorable_whitespace);
        }
        options -= XmlParserOption::XmlParseNoblanks as i32;
        (*ctxt).options |= XmlParserOption::XmlParseNoblanks as i32;
    } else {
        (*ctxt).keep_blanks = 1;
    }
    if options & HtmlParserOption::HtmlParseRecover as i32 != 0 {
        (*ctxt).recovery = 1;
        options -= HtmlParserOption::HtmlParseRecover as i32;
    } else {
        (*ctxt).recovery = 0;
    }
    if options & HtmlParserOption::HtmlParseCompact as i32 != 0 {
        (*ctxt).options |= HtmlParserOption::HtmlParseCompact as i32;
        options -= HtmlParserOption::HtmlParseCompact as i32;
    }
    if options & XmlParserOption::XmlParseHuge as i32 != 0 {
        (*ctxt).options |= XmlParserOption::XmlParseHuge as i32;
        options -= XmlParserOption::XmlParseHuge as i32;
    }
    if options & HtmlParserOption::HtmlParseNodefdtd as i32 != 0 {
        (*ctxt).options |= HtmlParserOption::HtmlParseNodefdtd as i32;
        options -= HtmlParserOption::HtmlParseNodefdtd as i32;
    }
    if options & HtmlParserOption::HtmlParseIgnoreEnc as i32 != 0 {
        (*ctxt).options |= HtmlParserOption::HtmlParseIgnoreEnc as i32;
        options -= HtmlParserOption::HtmlParseIgnoreEnc as i32;
    }
    if options & HtmlParserOption::HtmlParseNoimplied as i32 != 0 {
        (*ctxt).options |= HtmlParserOption::HtmlParseNoimplied as i32;
        options -= HtmlParserOption::HtmlParseNoimplied as i32;
    }
    (*ctxt).dict_names = 0;
    (*ctxt).linenumbers = 1;
    options
}

/// Common front-end for the htmlRead functions
///
/// Returns the resulting document tree or NULL
#[doc(alias = "htmlDoRead")]
unsafe fn html_do_read(
    ctxt: HtmlParserCtxtPtr,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
    reuse: i32,
) -> HtmlDocPtr {
    html_ctxt_use_options(ctxt, options);
    (*ctxt).html = 1;
    if let Some(encoding) = encoding {
        if let Some(handler) = find_encoding_handler(encoding) {
            (*ctxt).switch_to_encoding(handler);
            (*(*ctxt).input).encoding = Some(encoding.to_owned());
        }
    }
    if url.is_some() && !(*ctxt).input.is_null() && (*(*ctxt).input).filename.is_none() {
        (*(*ctxt).input).filename = url.map(|u| u.to_owned());
    }
    html_parse_document(ctxt);
    let ret: HtmlDocPtr = (*ctxt).my_doc;
    (*ctxt).my_doc = null_mut();
    if reuse == 0 {
        if (*ctxt).dict_names != 0 && !ret.is_null() && (*ret).dict == (*ctxt).dict {
            (*ctxt).dict = null_mut();
        }
        xml_free_parser_ctxt(ctxt);
    }
    ret
}

/// Parse an XML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "htmlReadDoc")]
pub unsafe fn html_read_doc(
    cur: *const XmlChar,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> HtmlDocPtr {
    if cur.is_null() {
        return null_mut();
    }

    xml_init_parser();
    let ctxt: HtmlParserCtxtPtr = html_create_doc_parser_ctxt(cur, None);
    if ctxt.is_null() {
        return null_mut();
    }
    html_do_read(ctxt, url, encoding, options, 0)
}

/// Parse an XML file from the filesystem or the network.
///
/// Returns the resulting document tree
#[doc(alias = "htmlReadFile")]
pub unsafe fn html_read_file(filename: &str, encoding: Option<&str>, options: i32) -> HtmlDocPtr {
    xml_init_parser();
    let ctxt: HtmlParserCtxtPtr = html_create_file_parser_ctxt(filename, encoding);
    if ctxt.is_null() {
        return null_mut();
    }
    html_do_read(ctxt, None, None, options, 0)
}

/// Parse an XML in-memory document and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "htmlReadMemory")]
pub unsafe fn html_read_memory(
    buffer: Vec<u8>,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> HtmlDocPtr {
    xml_init_parser();
    let ctxt: HtmlParserCtxtPtr = html_create_memory_parser_ctxt(buffer);
    if ctxt.is_null() {
        return null_mut();
    }
    html_do_read(ctxt, url, encoding, options, 0)
}

/// Parse an HTML document from I/O functions and source and build a tree.
///
/// Returns the resulting document tree
#[doc(alias = "htmlReadIO")]
pub unsafe fn html_read_io(
    ioctx: impl Read + 'static,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> HtmlDocPtr {
    xml_init_parser();

    let input = XmlParserInputBuffer::from_reader(ioctx, XmlCharEncoding::None);
    let ctxt: HtmlParserCtxtPtr = html_new_parser_ctxt();
    if ctxt.is_null() {
        return null_mut();
    }
    let stream: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, Rc::new(RefCell::new(input)), XmlCharEncoding::None);
    if stream.is_null() {
        xml_free_parser_ctxt(ctxt);
        return null_mut();
    }
    (*ctxt).input_push(stream);
    html_do_read(ctxt, url, encoding, options, 0)
}

/// Parse an XML in-memory document and build a tree.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "htmlCtxtReadDoc")]
pub unsafe fn html_ctxt_read_doc(
    ctxt: XmlParserCtxtPtr,
    cur: *const XmlChar,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> HtmlDocPtr {
    if cur.is_null() {
        return null_mut();
    }
    html_ctxt_read_memory(
        ctxt,
        CStr::from_ptr(cur as *const i8).to_bytes().to_vec(),
        url,
        encoding,
        options,
    )
}

/// Parse an XML file from the filesystem or the network.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "htmlCtxtReadFile")]
pub unsafe fn html_ctxt_read_file(
    ctxt: XmlParserCtxtPtr,
    filename: &str,
    encoding: Option<&str>,
    options: i32,
) -> HtmlDocPtr {
    if ctxt.is_null() {
        return null_mut();
    }
    xml_init_parser();

    html_ctxt_reset(ctxt);

    let stream: XmlParserInputPtr = xml_load_external_entity(Some(filename), None, ctxt);
    if stream.is_null() {
        return null_mut();
    }
    (*ctxt).input_push(stream);
    html_do_read(ctxt, None, encoding, options, 1)
}

/// Parse an XML in-memory document and build a tree.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "htmlCtxtReadMemory")]
pub unsafe fn html_ctxt_read_memory(
    ctxt: XmlParserCtxtPtr,
    buffer: Vec<u8>,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> HtmlDocPtr {
    if ctxt.is_null() {
        return null_mut();
    }
    xml_init_parser();

    html_ctxt_reset(ctxt);

    let Some(input) = XmlParserInputBuffer::from_memory(buffer, XmlCharEncoding::None) else {
        return null_mut();
    };

    let stream: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, Rc::new(RefCell::new(input)), XmlCharEncoding::None);
    if stream.is_null() {
        return null_mut();
    }

    (*ctxt).input_push(stream);
    html_do_read(ctxt, url, encoding, options, 1)
}

/// Parse an HTML document from I/O functions and source and build a tree.
/// This reuses the existing @ctxt parser context
///
/// Returns the resulting document tree
#[doc(alias = "htmlCtxtReadIO")]
pub unsafe fn html_ctxt_read_io(
    ctxt: XmlParserCtxtPtr,
    ioctx: impl Read + 'static,
    url: Option<&str>,
    encoding: Option<&str>,
    options: i32,
) -> HtmlDocPtr {
    if ctxt.is_null() {
        return null_mut();
    }
    xml_init_parser();

    html_ctxt_reset(ctxt);

    let input = XmlParserInputBuffer::from_reader(ioctx, XmlCharEncoding::None);
    let stream: XmlParserInputPtr =
        xml_new_io_input_stream(ctxt, Rc::new(RefCell::new(input)), XmlCharEncoding::None);
    if stream.is_null() {
        return null_mut();
    }
    (*ctxt).input_push(stream);
    html_do_read(ctxt, url, encoding, options, 1)
}

// NRK/Jan2003: further knowledge of HTML structure
#[repr(C)]
pub enum HtmlStatus {
    HtmlNa = 0, /* something we don't check at all */
    HtmlInvalid = 0x1,
    HtmlDeprecated = 0x2,
    HtmlValid = 0x4,
    HtmlRequired = 0xc, /* VALID bit set so ( & HTML_VALID ) is TRUE */
}

/// Checks whether an attribute is valid for an element
/// Has full knowledge of Required and Deprecated attributes
///
/// Returns one of HTML_REQUIRED, HTML_VALID, HTML_DEPRECATED, HTML_INVALID
#[doc(alias = "htmlAttrAllowed")]
pub unsafe fn html_attr_allowed(
    elt: *const HtmlElemDesc,
    attr: *const XmlChar,
    legacy: i32,
) -> HtmlStatus {
    let mut p: *mut *const c_char;

    if elt.is_null() || attr.is_null() {
        return HtmlStatus::HtmlInvalid;
    }

    if !(*elt).attrs_req.is_null() {
        p = (*elt).attrs_req;
        while !(*p).is_null() {
            if xml_strcmp(*p as _, attr) == 0 {
                return HtmlStatus::HtmlRequired;
            }
            p = p.add(1);
        }
    }

    if !(*elt).attrs_opt.is_null() {
        p = (*elt).attrs_opt;
        while !(*p).is_null() {
            if xml_strcmp(*p as _, attr) == 0 {
                return HtmlStatus::HtmlValid;
            }
            p = p.add(1);
        }
    }

    if legacy != 0 && !(*elt).attrs_depr.is_null() {
        p = (*elt).attrs_depr;
        while !(*p).is_null() {
            if xml_strcmp(*p as _, attr) == 0 {
                return HtmlStatus::HtmlDeprecated;
            }
            p = p.add(1);
        }
    }

    HtmlStatus::HtmlInvalid
}

/// Checks whether an HTML element may be a direct child of a parent element.
///
/// # Note
/// - doesn't check for deprecated elements
///
/// Returns 1 if allowed; 0 otherwise.
#[doc(alias = "htmlElementAllowedHere")]
pub fn html_element_allowed_here(parent: &HtmlElemDesc, elt: &str) -> bool {
    parent.subelts.iter().any(|&sub| sub == elt)
}

/// Checks whether an HTML element may be a direct child of a parent element.
/// and if so whether it is valid or deprecated.
///
/// Returns one of htmlStatus::HTML_VALID, htmlStatus::HTML_DEPRECATED, htmlStatus::HTML_INVALID
#[doc(alias = "htmlElementStatusHere")]
pub fn html_element_status_here(parent: &HtmlElemDesc, elt: &HtmlElemDesc) -> HtmlStatus {
    if !html_element_allowed_here(parent, elt.name) {
        return HtmlStatus::HtmlInvalid;
    }

    if elt.dtd == 0 {
        HtmlStatus::HtmlValid
    } else {
        HtmlStatus::HtmlDeprecated
    }
}

/// Checks whether the tree node is valid.  
/// Experimental (the author only uses the HTML enhancements in a SAX parser)
///
/// Return: for Element nodes, a return from htmlElementAllowedHere (if legacy allowed)
/// or htmlElementStatusHere (otherwise).  
/// - For Attribute nodes, a return from htmlAttrAllowed
/// - For other nodes, htmlStatus::HTML_NA (no checks performed)
#[doc(alias = "htmlNodeStatus")]
pub unsafe fn html_node_status(node: HtmlNodePtr, legacy: i32) -> HtmlStatus {
    if node.is_null() {
        return HtmlStatus::HtmlInvalid;
    }

    match (*node).element_type() {
        XmlElementType::XmlElementNode => {
            if legacy != 0 {
                if html_tag_lookup(&(*node).parent().unwrap().name().unwrap())
                    .map_or(false, |desc| {
                        html_element_allowed_here(desc, &(*node).name().unwrap())
                    })
                {
                    HtmlStatus::HtmlValid
                } else {
                    HtmlStatus::HtmlInvalid
                }
            } else {
                html_tag_lookup(&(*node).parent().unwrap().name().unwrap())
                    .zip(html_tag_lookup(&(*node).name().unwrap()))
                    .map(|(par, chi)| html_element_status_here(par, chi))
                    .unwrap_or(HtmlStatus::HtmlInvalid)
            }
        }
        XmlElementType::XmlAttributeNode => {
            html_tag_lookup(&(*node).parent().unwrap().name().unwrap())
                .map(|desc| html_attr_allowed(desc, (*node).name, legacy))
                .unwrap_or(HtmlStatus::HtmlInvalid)
        }
        _ => HtmlStatus::HtmlNa,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        globals::reset_last_error,
        libxml::{xmlmemory::xml_mem_blocks, xmlstring::xml_strlen},
        test_util::*,
    };

    use super::*;

    #[test]
    fn test_html_auto_close_tag() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_HTML_DOC_PTR {
                for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_elem in 0..GEN_NB_HTML_NODE_PTR {
                        let mem_base = xml_mem_blocks();
                        let doc = gen_html_doc_ptr(n_doc, 0);
                        let name = gen_const_xml_char_ptr(n_name, 1);
                        let elem = gen_html_node_ptr(n_elem, 2);

                        let ret_val = html_auto_close_tag(doc, name, elem);
                        desret_int(ret_val);
                        des_html_doc_ptr(n_doc, doc, 0);
                        des_const_xml_char_ptr(n_name, name, 1);
                        des_html_node_ptr(n_elem, elem, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            eprintln!("Leak of {} blocks found in htmlAutoCloseTag {n_doc} {n_name} {n_elem}", xml_mem_blocks() - mem_base);
                            leaks += 1;
                        }
                    }
                }
            }

            assert!(leaks == 0, "{leaks} Leaks are found in htmlAutoCloseTag()");
        }
    }

    #[test]
    fn test_html_attr_allowed() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_elt in 0..GEN_NB_CONST_HTML_ELEM_DESC_PTR {
                for n_attr in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_legacy in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let elt = gen_const_html_elem_desc_ptr(n_elt, 0);
                        let attr = gen_const_xml_char_ptr(n_attr, 1);
                        let legacy = gen_int(n_legacy, 2);

                        let ret_val = html_attr_allowed(elt, attr, legacy);
                        desret_html_status(ret_val);
                        des_const_html_elem_desc_ptr(n_elt, elt, 0);
                        des_const_xml_char_ptr(n_attr, attr, 1);
                        des_int(n_legacy, legacy, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            eprintln!("Leak of {} blocks found in htmlAttrAllowed {n_elt} {n_attr} {n_legacy}", xml_mem_blocks() - mem_base);
                            leaks += 1;
                        }
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlAttrAllowed()");
        }
    }

    #[test]
    fn test_html_ctxt_reset() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_HTML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_html_parser_ctxt_ptr(n_ctxt, 0);

                html_ctxt_reset(ctxt);
                des_html_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlCtxtReset",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlCtxtReset()");
        }
    }

    #[test]
    fn test_html_ctxt_use_options() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_HTML_PARSER_CTXT_PTR {
                for n_options in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_html_parser_ctxt_ptr(n_ctxt, 0);
                    let options = gen_int(n_options, 1);

                    let ret_val = html_ctxt_use_options(ctxt, options);
                    desret_int(ret_val);
                    des_html_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_int(n_options, options, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in htmlCtxtUseOptions",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_options);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlCtxtUseOptions()"
            );
        }
    }

    #[test]
    fn test_html_encode_entities() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_out in 0..GEN_NB_UNSIGNED_CHAR_PTR {
                for n_outlen in 0..GEN_NB_INT_PTR {
                    for n_in in 0..GEN_NB_CONST_UNSIGNED_CHAR_PTR {
                        for n_inlen in 0..GEN_NB_INT_PTR {
                            for n_quote_char in 0..GEN_NB_INT {
                                let mem_base = xml_mem_blocks();
                                let out = gen_unsigned_char_ptr(n_out, 0);
                                let outlen = gen_int_ptr(n_outlen, 1);
                                let input = gen_const_unsigned_char_ptr(n_in, 2);
                                let inlen = gen_int_ptr(n_inlen, 3);
                                let quote_char = gen_int(n_quote_char, 4);

                                let ret_val = html_encode_entities(
                                    out,
                                    outlen,
                                    input as *const u8,
                                    inlen,
                                    quote_char,
                                );
                                desret_int(ret_val);
                                des_unsigned_char_ptr(n_out, out, 0);
                                des_int_ptr(n_outlen, outlen, 1);
                                des_const_unsigned_char_ptr(n_in, input as *const u8, 2);
                                des_int_ptr(n_inlen, inlen, 3);
                                des_int(n_quote_char, quote_char, 4);
                                reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in htmlEncodeEntities",
                                        xml_mem_blocks() - mem_base
                                    );
                                    eprint!(" {}", n_out);
                                    eprint!(" {}", n_outlen);
                                    eprint!(" {}", n_in);
                                    eprint!(" {}", n_inlen);
                                    eprintln!(" {}", n_quote_char);
                                }
                            }
                        }
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlEncodeEntities()"
            );
        }
    }

    #[test]
    fn test_html_entity_lookup() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let name = gen_const_xml_char_ptr(n_name, 0);

                let ret_val = html_entity_lookup(name as *const XmlChar);
                desret_const_html_entity_desc_ptr(ret_val);
                des_const_xml_char_ptr(n_name, name, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlEntityLookup",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_name);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlEntityLookup()");
        }
    }

    #[test]
    fn test_html_entity_value_lookup() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_value in 0..GEN_NB_UNSIGNED_INT {
                let mem_base = xml_mem_blocks();
                let value = gen_unsigned_int(n_value, 0);

                let ret_val = html_entity_value_lookup(value);
                desret_const_html_entity_desc_ptr(ret_val);
                des_unsigned_int(n_value, value, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlEntityValueLookup",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_value);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlEntityValueLookup()"
            );
        }
    }

    #[test]
    fn test_html_handle_omitted_elem() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_val in 0..GEN_NB_INT {
                let mem_base = xml_mem_blocks();
                let val = gen_int(n_val, 0);

                let ret_val = html_handle_omitted_elem(val);
                desret_int(ret_val);
                des_int(n_val, val, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlHandleOmittedElem",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_val);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlHandleOmittedElem()"
            );
        }
    }

    #[test]
    fn test_html_init_auto_close() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            html_init_auto_close();
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in htmlInitAutoClose",
                    xml_mem_blocks() - mem_base
                );
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlInitAutoClose()");
        }
    }

    #[test]
    fn test_html_is_auto_closed() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_doc in 0..GEN_NB_HTML_DOC_PTR {
                for n_elem in 0..GEN_NB_HTML_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let doc = gen_html_doc_ptr(n_doc, 0);
                    let elem = gen_html_node_ptr(n_elem, 1);

                    let ret_val = html_is_auto_closed(doc, elem);
                    desret_int(ret_val);
                    des_html_doc_ptr(n_doc, doc, 0);
                    des_html_node_ptr(n_elem, elem, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in htmlIsAutoClosed",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_doc);
                        eprintln!(" {}", n_elem);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlIsAutoClosed()");
        }
    }

    #[test]
    fn test_html_is_script_attribute() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_name in 0..GEN_NB_CONST_XML_CHAR_PTR {
                let mem_base = xml_mem_blocks();
                let name = gen_const_xml_char_ptr(n_name, 0);

                let ret_val = html_is_script_attribute(name as *const XmlChar);
                desret_int(ret_val);
                des_const_xml_char_ptr(n_name, name, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlIsScriptAttribute",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_name);
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlIsScriptAttribute()"
            );
        }
    }

    #[test]
    fn test_html_new_parser_ctxt() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            let mem_base = xml_mem_blocks();

            let ret_val = html_new_parser_ctxt();
            desret_html_parser_ctxt_ptr(ret_val);
            reset_last_error();
            if mem_base != xml_mem_blocks() {
                leaks += 1;
                eprintln!(
                    "Leak of {} blocks found in htmlNewParserCtxt",
                    xml_mem_blocks() - mem_base
                );
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlNewParserCtxt()");
        }
    }

    #[test]
    fn test_html_node_status() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_node in 0..GEN_NB_CONST_HTML_NODE_PTR {
                for n_legacy in 0..GEN_NB_INT {
                    let mem_base = xml_mem_blocks();
                    let node = gen_const_html_node_ptr(n_node, 0);
                    let legacy = gen_int(n_legacy, 1);

                    let ret_val = html_node_status(node as HtmlNodePtr, legacy);
                    desret_html_status(ret_val);
                    des_const_html_node_ptr(n_node, node as HtmlNodePtr, 0);
                    des_int(n_legacy, legacy, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in htmlNodeStatus",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_node);
                        eprintln!(" {}", n_legacy);
                    }
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlNodeStatus()");
        }
    }

    #[test]
    fn test_html_parse_char_ref() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_HTML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_html_parser_ctxt_ptr(n_ctxt, 0);

                let ret_val = html_parse_char_ref(ctxt);
                desret_int(ret_val);
                des_html_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlParseCharRef",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlParseCharRef()");
        }
    }

    #[test]
    fn test_html_parse_chunk() {
        #[cfg(all(feature = "html", feature = "libxml_push"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_HTML_PARSER_CTXT_PTR {
                for n_chunk in 0..GEN_NB_CONST_CHAR_PTR {
                    for n_size in 0..GEN_NB_INT {
                        for n_terminate in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_html_parser_ctxt_ptr(n_ctxt, 0);
                            let chunk = gen_const_char_ptr(n_chunk, 1);
                            let mut size = gen_int(n_size, 2);
                            let terminate = gen_int(n_terminate, 3);
                            if !chunk.is_null() && size > xml_strlen(chunk as _) {
                                size = 0;
                            }

                            let ret_val = html_parse_chunk(ctxt, chunk, size, terminate);
                            if !ctxt.is_null() {
                                xml_free_doc((*ctxt).my_doc);
                                (*ctxt).my_doc = null_mut();
                            }
                            desret_int(ret_val);
                            des_html_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_const_char_ptr(n_chunk, chunk, 1);
                            des_int(n_size, size, 2);
                            des_int(n_terminate, terminate, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in htmlParseChunk",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in htmlParseChunk()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_chunk);
                                eprint!(" {}", n_size);
                                eprintln!(" {}", n_terminate);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_html_parse_document() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_HTML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_html_parser_ctxt_ptr(n_ctxt, 0);

                let ret_val = html_parse_document(ctxt);
                if !ctxt.is_null() {
                    xml_free_doc((*ctxt).my_doc);
                    (*ctxt).my_doc = null_mut();
                }
                desret_int(ret_val);
                des_html_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlParseDocument",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlParseDocument()");
        }
    }

    #[test]
    fn test_html_parse_element() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_HTML_PARSER_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_html_parser_ctxt_ptr(n_ctxt, 0);

                html_parse_element(ctxt);
                des_html_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in htmlParseElement",
                        xml_mem_blocks() - mem_base
                    );
                    eprintln!(" {}", n_ctxt);
                }
            }
            assert!(leaks == 0, "{leaks} Leaks are found in htmlParseElement()");
        }
    }

    #[test]
    fn test_html_parse_entity_ref() {
        #[cfg(feature = "html")]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_HTML_PARSER_CTXT_PTR {
                for n_str in 0..GEN_NB_CONST_XML_CHAR_PTR_PTR {
                    let mem_base = xml_mem_blocks();
                    let ctxt = gen_html_parser_ctxt_ptr(n_ctxt, 0);
                    let str = gen_const_xml_char_ptr_ptr(n_str, 1);

                    let ret_val = html_parse_entity_ref(ctxt, str as *mut *const XmlChar);
                    desret_const_html_entity_desc_ptr(ret_val);
                    des_html_parser_ctxt_ptr(n_ctxt, ctxt, 0);
                    des_const_xml_char_ptr_ptr(n_str, str as *mut *const XmlChar, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in htmlParseEntityRef",
                            xml_mem_blocks() - mem_base
                        );
                        eprint!(" {}", n_ctxt);
                        eprintln!(" {}", n_str);
                    }
                }
            }
            assert!(
                leaks == 0,
                "{leaks} Leaks are found in htmlParseEntityRef()"
            );
        }
    }
}

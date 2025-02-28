//! Provide methods and data structures for handling regular expressions.  
//! This module is based on `libxml/regexp.h`, `regexp.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// --------
// Summary: regular expressions handling
// Description: basic API for libxml regular expressions handling used
//              for XML Schemas and validation.
//
// Copy: See Copyright for the status of this software.
//
// Author: Daniel Veillard
// --------
// regexp.c: generic and extensible Regular Expression engine
//
// Basically designed with the purpose of compiling regexps for
// the variety of validation/schemas mechanisms now available in
// XML related specifications these include:
//    - XML-1.0 DTD validation
//    - XML Schemas structure part 1
//    - XML Schemas Datatypes part 2 especially Appendix F
//    - RELAX-NG/TREX i.e. the counter proposal
//
// See Copyright for the status of this software.
//
// Daniel Veillard <veillard@redhat.com>

use std::{
    borrow::Cow,
    ffi::{c_char, CStr},
    io::Write,
    mem::{size_of, take},
    os::raw::c_void,
    ptr::{addr_of_mut, drop_in_place, null, null_mut},
};

use libc::{memcpy, memset, snprintf, strlen, INT_MAX};

use crate::{
    error::{XmlParserErrors, __xml_raise_error},
    libxml::{
        dict::{xml_dict_lookup, XmlDictPtr},
        globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
        parser_internals::xml_string_current_char,
        xmlautomata::{
            xml_free_automata, xml_new_automata, XmlAutomata, XmlAutomataPtr, XmlAutomataState,
        },
        xmlstring::{xml_strdup, xml_strndup, XmlChar},
        xmlunicode::{
            xml_ucs_is_block, xml_ucs_is_cat_c, xml_ucs_is_cat_cc, xml_ucs_is_cat_cf,
            xml_ucs_is_cat_co, xml_ucs_is_cat_l, xml_ucs_is_cat_ll, xml_ucs_is_cat_lm,
            xml_ucs_is_cat_lo, xml_ucs_is_cat_lt, xml_ucs_is_cat_lu, xml_ucs_is_cat_m,
            xml_ucs_is_cat_mc, xml_ucs_is_cat_me, xml_ucs_is_cat_mn, xml_ucs_is_cat_n,
            xml_ucs_is_cat_nd, xml_ucs_is_cat_nl, xml_ucs_is_cat_no, xml_ucs_is_cat_p,
            xml_ucs_is_cat_pc, xml_ucs_is_cat_pd, xml_ucs_is_cat_pe, xml_ucs_is_cat_pf,
            xml_ucs_is_cat_pi, xml_ucs_is_cat_po, xml_ucs_is_cat_ps, xml_ucs_is_cat_s,
            xml_ucs_is_cat_sc, xml_ucs_is_cat_sk, xml_ucs_is_cat_sm, xml_ucs_is_cat_so,
            xml_ucs_is_cat_z, xml_ucs_is_cat_zl, xml_ucs_is_cat_zp, xml_ucs_is_cat_zs,
        },
    },
};

use super::{
    chvalid::{xml_is_char, xml_is_combining, xml_is_digit, xml_is_extender},
    parser_internals::xml_is_letter,
};

const SIZE_MAX: usize = usize::MAX;
const MAX_PUSH: usize = 10000000;

// Note: the order of the enums below is significant, do not shuffle
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum XmlRegAtomType {
    #[default]
    XmlRegexpEpsilon = 1,
    XmlRegexpCharval,
    XmlRegexpRanges,
    XmlRegexpSubreg, /* used for () sub regexps */
    XmlRegexpString,
    XmlRegexpAnychar,     /* . */
    XmlRegexpAnyspace,    /* \s */
    XmlRegexpNotspace,    /* \S */
    XmlRegexpInitname,    /* \l */
    XmlRegexpNotinitname, /* \L */
    XmlRegexpNamechar,    /* \c */
    XmlRegexpNotnamechar, /* \C */
    XmlRegexpDecimal,     /* \d */
    XmlRegexpNotdecimal,  /* \D */
    XmlRegexpRealchar,    /* \w */
    XmlRegexpNotrealchar, /* \W */
    XmlRegexpLetter = 100,
    XmlRegexpLetterUppercase,
    XmlRegexpLetterLowercase,
    XmlRegexpLetterTitlecase,
    XmlRegexpLetterModifier,
    XmlRegexpLetterOthers,
    XmlRegexpMark,
    XmlRegexpMarkNonspacing,
    XmlRegexpMarkSpacecombining,
    XmlRegexpMarkEnclosing,
    XmlRegexpNumber,
    XmlRegexpNumberDecimal,
    XmlRegexpNumberLetter,
    XmlRegexpNumberOthers,
    XmlRegexpPunct,
    XmlRegexpPunctConnector,
    XmlRegexpPunctDash,
    XmlRegexpPunctOpen,
    XmlRegexpPunctClose,
    XmlRegexpPunctInitquote,
    XmlRegexpPunctFinquote,
    XmlRegexpPunctOthers,
    XmlRegexpSepar,
    XmlRegexpSeparSpace,
    XmlRegexpSeparLine,
    XmlRegexpSeparPara,
    XmlRegexpSymbol,
    XmlRegexpSymbolMath,
    XmlRegexpSymbolCurrency,
    XmlRegexpSymbolModifier,
    XmlRegexpSymbolOthers,
    XmlRegexpOther,
    XmlRegexpOtherControl,
    XmlRegexpOtherFormat,
    XmlRegexpOtherPrivate,
    XmlRegexpOtherNa,
    XmlRegexpBlockName,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlRegQuantType {
    #[default]
    XmlRegexpQuantEpsilon = 1,
    XmlRegexpQuantOnce,
    XmlRegexpQuantOpt,
    XmlRegexpQuantMult,
    XmlRegexpQuantPlus,
    XmlRegexpQuantOnceonly,
    XmlRegexpQuantAll,
    XmlRegexpQuantRange,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlRegStateType {
    #[default]
    XmlRegexpStartState = 1,
    XmlRegexpFinalState,
    XmlRegexpTransState,
    XmlRegexpSinkState,
    XmlRegexpUnreachState,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlRegMarkedType {
    #[default]
    XmlRegexpMarkNormal = 0,
    XmlRegexpMarkStart,
    XmlRegexpMarkVisited,
}

pub type XmlRegRangePtr = *mut XmlRegRange;
#[repr(C)]
#[derive(Default)]
pub struct XmlRegRange {
    neg: i32, /* 0 normal, 1 not, 2 exclude */
    typ: XmlRegAtomType,
    start: i32,
    end: i32,
    block_name: Option<String>,
}

pub type XmlRegState = XmlAutomataState;
pub type XmlRegStatePtr = *mut XmlRegState;

pub type XmlRegAtomPtr = *mut XmlRegAtom;
#[repr(C)]
pub struct XmlRegAtom {
    pub(crate) no: i32,
    pub(crate) typ: XmlRegAtomType,
    pub(crate) quant: XmlRegQuantType,
    pub(crate) min: i32,
    pub(crate) max: i32,
    pub(crate) valuep: Option<String>,
    pub(crate) valuep2: Option<String>,
    pub(crate) neg: i32,
    pub(crate) codepoint: i32,
    pub(crate) start: XmlRegStatePtr,
    pub(crate) start0: XmlRegStatePtr,
    pub(crate) stop: XmlRegStatePtr,
    pub(crate) ranges: Vec<XmlRegRangePtr>,
    pub(crate) data: *mut c_void,
}

impl Default for XmlRegAtom {
    fn default() -> Self {
        Self {
            no: 0,
            typ: XmlRegAtomType::default(),
            quant: XmlRegQuantType::default(),
            min: 0,
            max: 0,
            valuep: None,
            valuep2: None,
            neg: 0,
            codepoint: 0,
            start: null_mut(),
            start0: null_mut(),
            stop: null_mut(),
            ranges: vec![],
            data: null_mut(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct XmlRegCounter {
    pub(crate) min: i32,
    pub(crate) max: i32,
}

#[repr(C)]
pub struct XmlRegTrans {
    atom: XmlRegAtomPtr,
    to: i32,
    counter: i32,
    count: i32,
    nd: i32,
}

pub type XmlRegParserCtxt = XmlAutomata;
pub type XmlRegParserCtxtPtr = *mut XmlRegParserCtxt;

const AM_AUTOMATA_RNG: usize = 1;

/// A libxml regular expression, they can actually be far more complex
/// thank the POSIX regex expressions.
#[doc(alias = "xmlRegexpPtr")]
pub type XmlRegexpPtr = *mut XmlRegexp;
#[repr(C)]
pub struct XmlRegexp {
    string: *mut XmlChar,
    states: Vec<XmlRegStatePtr>,
    atoms: Vec<XmlRegAtomPtr>,
    counters: Vec<XmlRegCounter>,
    determinist: i32,
    flags: i32,
    // That's the compact form for determinists automatas
    nbstates: i32,
    compact: *mut i32,
    transdata: *mut *mut c_void,
    string_map: Vec<String>,
}

impl Default for XmlRegexp {
    fn default() -> Self {
        Self {
            string: null_mut(),
            states: vec![],
            atoms: vec![],
            counters: vec![],
            determinist: 0,
            flags: 0,
            nbstates: 0,
            compact: null_mut(),
            transdata: null_mut(),
            string_map: vec![],
        }
    }
}

pub type XmlRegExecRollbackPtr = *mut XmlRegExecRollback;
#[repr(C)]
pub struct XmlRegExecRollback {
    state: XmlRegStatePtr, /* the current state */
    index: i32,            /* the index in the input stack */
    nextbranch: i32,       /* the next transition to explore in that state */
    counts: Vec<i32>,      /* save the automata state if it has some */
}

pub type XmlRegInputTokenPtr = *mut XmlRegInputToken;
#[repr(C)]
pub struct XmlRegInputToken {
    value: *mut XmlChar,
    data: *mut c_void,
}

/// A libxml progressive regular expression evaluation context
#[doc(alias = "xmlRegExecCtxtPtr")]
pub type XmlRegExecCtxtPtr = *mut XmlRegExecCtxt;
#[repr(C)]
pub struct XmlRegExecCtxt {
    status: i32,        /* execution status != 0 indicate an error */
    determinist: i32,   /* did we find an indeterministic behaviour */
    comp: XmlRegexpPtr, /* the compiled regexp */
    callback: Option<XmlRegExecCallbacks>,
    data: *mut c_void,

    state: XmlRegStatePtr, /* the current state */
    transno: i32,          /* the current transition on that state */
    transcount: i32,       /* the number of chars in c_char counted transitions */
    // A stack of rollback states
    rollbacks: Vec<XmlRegExecRollback>,

    // The state of the automata if any
    counts: Vec<i32>,

    // The input stack
    input_stack_max: i32,
    input_stack_nr: i32,
    index: i32,
    char_stack: *mut i32,
    input_string: *const XmlChar,     /* when operating on characters */
    input_stack: XmlRegInputTokenPtr, /* when operating on strings */

    // error handling
    err_state_no: i32,         /* the error state number */
    err_state: XmlRegStatePtr, /* the error state */
    err_string: *mut XmlChar,  /* the string raising the error */
    err_counts: Vec<i32>,      /* counters at the error state */
    nb_push: i32,
}

impl Default for XmlRegExecCtxt {
    fn default() -> Self {
        Self {
            status: 0,
            determinist: 0,
            comp: null_mut(),
            callback: None,
            data: null_mut(),
            state: null_mut(),
            transno: 0,
            transcount: 0,
            rollbacks: vec![],
            counts: vec![],
            input_stack_max: 0,
            input_stack_nr: 0,
            index: 0,
            char_stack: null_mut(),
            input_string: null(),
            input_stack: null_mut(),
            err_state_no: 0,
            err_state: null_mut(),
            err_string: null_mut(),
            err_counts: vec![],
            nb_push: 0,
        }
    }
}

/// Allocate a new regexp parser context
///
/// Returns the new context or NULL in case of error
#[doc(alias = "xmlRegNewParserCtxt")]
pub(crate) unsafe fn xml_reg_new_parser_ctxt(string: *const XmlChar) -> XmlRegParserCtxtPtr {
    let ret: XmlRegParserCtxtPtr = xml_malloc(size_of::<XmlRegParserCtxt>()) as XmlRegParserCtxtPtr;
    if ret.is_null() {
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlRegParserCtxt::default());
    if !string.is_null() {
        (*ret).string = xml_strdup(string);
    }
    (*ret).cur = (*ret).string;
    (*ret).neg = 0;
    (*ret).negs = 0;
    (*ret).error = 0;
    (*ret).determinist = -1;
    ret
}

/// Handle an out of memory condition
#[doc(alias = "xmlRegexpErrMemory")]
unsafe fn xml_regexp_err_memory(ctxt: XmlRegParserCtxtPtr, extra: &str) {
    let mut regexp: *const c_char = null();
    if !ctxt.is_null() {
        regexp = (*ctxt).string as _;
        (*ctxt).error = XmlParserErrors::XmlErrNoMemory as _;
    }
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        None,
        XmlErrorDomain::XmlFromRegexp,
        XmlParserErrors::XmlErrNoMemory,
        XmlErrorLevel::XmlErrFatal,
        None,
        0,
        Some(extra.to_owned().into()),
        (!regexp.is_null()).then(|| CStr::from_ptr(regexp).to_string_lossy().into_owned().into()),
        None,
        0,
        0,
        "Memory allocation failed : {}\n",
        extra
    );
}

unsafe fn xml_reg_new_state(ctxt: XmlRegParserCtxtPtr) -> XmlRegStatePtr {
    let ret: XmlRegStatePtr = xml_malloc(size_of::<XmlRegState>()) as XmlRegStatePtr;
    if ret.is_null() {
        xml_regexp_err_memory(ctxt, "allocating state");
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlRegState::default());
    (*ret).typ = XmlRegStateType::XmlRegexpTransState;
    (*ret).mark = XmlRegMarkedType::XmlRegexpMarkNormal;
    ret
}

pub(crate) unsafe fn xml_reg_state_push(ctxt: XmlRegParserCtxtPtr) -> XmlRegStatePtr {
    let state: XmlRegStatePtr = xml_reg_new_state(ctxt);
    if state.is_null() {
        return null_mut();
    }

    (*state).no = (*ctxt).states.len() as i32;
    (*ctxt).states.push(state);
    state
}

/// Free a regexp state
#[doc(alias = "xmlRegFreeState")]
unsafe fn xml_reg_free_state(state: XmlRegStatePtr) {
    if state.is_null() {
        return;
    }

    drop_in_place(state);
    xml_free(state as _);
}

/// Free a regexp range
#[doc(alias = "xmlRegFreeRange")]
unsafe fn xml_reg_free_range(range: XmlRegRangePtr) {
    if range.is_null() {
        return;
    }

    drop_in_place(range);
    xml_free(range as _);
}

/// Free a regexp atom
#[doc(alias = "xmlRegFreeAtom")]
pub(crate) unsafe fn xml_reg_free_atom(atom: XmlRegAtomPtr) {
    if atom.is_null() {
        return;
    }

    for range in (*atom).ranges.drain(..) {
        xml_reg_free_range(range);
    }
    drop_in_place(atom);
    xml_free(atom as _);
}

/// Free a regexp parser context
#[doc(alias = "xmlRegFreeParserCtxt")]
pub(crate) unsafe fn xml_reg_free_parser_ctxt(ctxt: XmlRegParserCtxtPtr) {
    if ctxt.is_null() {
        return;
    }

    if !(*ctxt).string.is_null() {
        xml_free((*ctxt).string as _);
    }
    for state in (*ctxt).states.drain(..) {
        xml_reg_free_state(state);
    }
    for atom in (*ctxt).atoms.drain(..) {
        xml_reg_free_atom(atom);
    }
    drop_in_place(ctxt);
    xml_free(ctxt as _);
}

macro_rules! CUR_SCHAR {
    ( $s:expr, $l:expr ) => {
        xml_string_current_char(null_mut(), $s, addr_of_mut!($l))
    };
}

/// `[10]   Char   ::=   [^.\?*+()|#x5B#x5D]`
#[doc(alias = "xmlFAIsChar")]
unsafe fn xml_fa_is_char(ctxt: XmlRegParserCtxtPtr) -> i32 {
    let mut len: i32 = 0;

    let cur: i32 = CUR_SCHAR!((*ctxt).cur, len);
    if cur == b'.' as i32
        || cur == b'\\' as i32
        || cur == b'?' as i32
        || cur == b'*' as i32
        || cur == b'+' as i32
        || cur == b'(' as i32
        || cur == b')' as i32
        || cur == b'|' as i32
        || cur == 0x5B
        || cur == 0x5D
        || cur == 0
    {
        return -1;
    }
    cur
}

/// Allocate a new atom
///
/// Returns the new atom or NULL in case of error
#[doc(alias = "xmlRegNewAtom")]
pub(crate) unsafe fn xml_reg_new_atom(
    ctxt: XmlRegParserCtxtPtr,
    typ: XmlRegAtomType,
) -> XmlRegAtomPtr {
    let ret: XmlRegAtomPtr = xml_malloc(size_of::<XmlRegAtom>()) as XmlRegAtomPtr;
    if ret.is_null() {
        xml_regexp_err_memory(ctxt, "allocating atom");
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlRegAtom::default());
    (*ret).typ = typ;
    (*ret).quant = XmlRegQuantType::XmlRegexpQuantOnce;
    (*ret).min = 0;
    (*ret).max = 0;
    ret
}

macro_rules! CUR {
    ( $ctxt:expr ) => {
        *(*$ctxt).cur
    };
}

macro_rules! NEXTL {
    ( $ctxt:expr, $l:expr ) => {
        (*$ctxt).cur = (*$ctxt).cur.add($l as usize);
    };
}

macro_rules! NEXT {
    ( $ctxt:expr ) => {
        (*$ctxt).cur = (*$ctxt).cur.add(1);
    };
}

#[repr(C)]
enum XmlExpNodeInfo {
    XmlExpNilable = 1 << 0,
}

macro_rules! IS_NILLABLE {
    ( $node:expr ) => {
        (*$node).info & XmlExpNodeInfo::XmlExpNilable as u8
    };
}

/// Handle a compilation failure
#[doc(alias = "xmlRegexpErrCompile")]
unsafe fn xml_regexp_err_compile(ctxt: XmlRegParserCtxtPtr, extra: &str) {
    let mut regexp: *const c_char = null();
    let mut idx: i32 = 0;

    if !ctxt.is_null() {
        regexp = (*ctxt).string as _;
        idx = (*ctxt).cur.offset_from((*ctxt).string) as _;
        (*ctxt).error = XmlParserErrors::XmlRegexpCompileError as _;
    }
    __xml_raise_error!(
        None,
        None,
        None,
        null_mut(),
        None,
        XmlErrorDomain::XmlFromRegexp,
        XmlParserErrors::XmlRegexpCompileError,
        XmlErrorLevel::XmlErrFatal,
        None,
        0,
        Some(extra.to_owned().into()),
        (!regexp.is_null()).then(|| CStr::from_ptr(regexp).to_string_lossy().into_owned().into()),
        None,
        idx,
        0,
        "failed to compile: {}\n",
        extra
    );
}

macro_rules! ERROR {
    ( $ctxt:expr, $str:expr ) => {
        (*$ctxt).error = XmlParserErrors::XmlRegexpCompileError as _;
        xml_regexp_err_compile($ctxt, $str);
    };
}

unsafe fn xml_reg_state_add_trans_to(
    _ctxt: XmlRegParserCtxtPtr,
    target: XmlRegStatePtr,
    from: i32,
) {
    (*target).trans_to.push(from);
}

pub(crate) unsafe fn xml_reg_state_add_trans(
    ctxt: XmlRegParserCtxtPtr,
    state: XmlRegStatePtr,
    atom: XmlRegAtomPtr,
    target: XmlRegStatePtr,
    counter: i32,
    count: i32,
) {
    if state.is_null() {
        ERROR!(ctxt, "add state: state is NULL");
        return;
    }
    if target.is_null() {
        ERROR!(ctxt, "add state: target is NULL");
        return;
    }
    // Other routines follow the philosophy 'When in doubt, add a transition'
    // so we check here whether such a transition is already present and, if
    // so, silently ignore this request.

    for trans in (*state).trans.iter().rev() {
        if trans.atom == atom
            && trans.to == (*target).no
            && trans.counter == counter
            && trans.count == count
        {
            return;
        }
    }

    let trans = XmlRegTrans {
        atom,
        to: (*target).no,
        counter,
        count,
        nd: 0,
    };
    (*state).trans.push(trans);
    xml_reg_state_add_trans_to(ctxt, target, (*state).no);
}

#[doc(alias = "xmlFAGenerateEpsilonTransition")]
pub(crate) unsafe fn xml_fa_generate_epsilon_transition(
    ctxt: XmlRegParserCtxtPtr,
    from: XmlRegStatePtr,
    mut to: XmlRegStatePtr,
) -> i32 {
    if to.is_null() {
        to = xml_reg_state_push(ctxt);
        if to.is_null() {
            return -1;
        }
        (*ctxt).state = to;
    }
    xml_reg_state_add_trans(ctxt, from, null_mut(), to, -1, -1);
    0
}

/// Allocate a new regexp range
///
/// Returns the new range or NULL in case of error
#[doc(alias = "xmlRegNewRange")]
unsafe fn xml_reg_new_range(
    ctxt: XmlRegParserCtxtPtr,
    neg: i32,
    typ: XmlRegAtomType,
    start: i32,
    end: i32,
) -> XmlRegRangePtr {
    let ret: XmlRegRangePtr = xml_malloc(size_of::<XmlRegRange>()) as XmlRegRangePtr;
    if ret.is_null() {
        xml_regexp_err_memory(ctxt, "allocating range");
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlRegRange::default());
    (*ret).neg = neg;
    (*ret).typ = typ;
    (*ret).start = start;
    (*ret).end = end;
    ret
}

unsafe fn xml_reg_atom_add_range(
    ctxt: XmlRegParserCtxtPtr,
    atom: XmlRegAtomPtr,
    neg: i32,
    typ: XmlRegAtomType,
    start: i32,
    end: i32,
    block_name: Option<&str>,
) -> XmlRegRangePtr {
    if atom.is_null() {
        ERROR!(ctxt, "add range: atom is NULL");
        return null_mut();
    }
    if !matches!((*atom).typ, XmlRegAtomType::XmlRegexpRanges) {
        ERROR!(ctxt, "add range: atom is not ranges");
        return null_mut();
    }
    let range: XmlRegRangePtr = xml_reg_new_range(ctxt, neg, typ, start, end);
    if range.is_null() {
        return null_mut();
    }
    (*range).block_name = block_name.map(|b| b.to_owned());
    (*atom).ranges.push(range);
    range
}

/// ```text
/// [27]   charProp   ::=   IsCategory | IsBlock
/// [28]   IsCategory ::= Letters | Marks | Numbers | Punctuation |
///                       Separators | Symbols | Others
/// [29]   Letters   ::=   'L' [ultmo]?
/// [30]   Marks   ::=   'M' [nce]?
/// [31]   Numbers   ::=   'N' [dlo]?
/// [32]   Punctuation   ::=   'P' [cdseifo]?
/// [33]   Separators   ::=   'Z' [slp]?
/// [34]   Symbols   ::=   'S' [mcko]?
/// [35]   Others   ::=   'C' [cfon]?
/// [36]   IsBlock   ::=   'Is' [a-zA-Z0-9#x2D]+
/// ```
#[doc(alias = "xmlFAParseCharProp")]
unsafe fn xml_fa_parse_char_prop(ctxt: XmlRegParserCtxtPtr) {
    let mut cur: i32;
    let mut block_name: *mut XmlChar = null_mut();

    cur = CUR!(ctxt) as _;
    let typ = if cur == b'L' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b'u' as _ {
            NEXT!(ctxt);
            XmlRegAtomType::XmlRegexpLetterUppercase
        } else if cur == b'l' as _ {
            NEXT!(ctxt);
            XmlRegAtomType::XmlRegexpLetterLowercase
        } else if cur == b't' as _ {
            NEXT!(ctxt);
            XmlRegAtomType::XmlRegexpLetterTitlecase
        } else if cur == b'm' as _ {
            NEXT!(ctxt);
            XmlRegAtomType::XmlRegexpLetterModifier
        } else if cur == b'o' as _ {
            NEXT!(ctxt);
            XmlRegAtomType::XmlRegexpLetterOthers
        } else {
            XmlRegAtomType::XmlRegexpLetter
        }
    } else if cur == b'M' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b'n' as _ {
            NEXT!(ctxt);
            // nonspacing
            XmlRegAtomType::XmlRegexpMarkNonspacing
        } else if cur == b'c' as _ {
            NEXT!(ctxt);
            // spacing combining
            XmlRegAtomType::XmlRegexpMarkSpacecombining
        } else if cur == b'e' as _ {
            NEXT!(ctxt);
            // enclosing
            XmlRegAtomType::XmlRegexpMarkEnclosing
        } else {
            // all marks
            XmlRegAtomType::XmlRegexpMark
        }
    } else if cur == b'N' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b'd' as _ {
            NEXT!(ctxt);
            // digital
            XmlRegAtomType::XmlRegexpNumberDecimal
        } else if cur == b'l' as _ {
            NEXT!(ctxt);
            // letter
            XmlRegAtomType::XmlRegexpNumberLetter
        } else if cur == b'o' as _ {
            NEXT!(ctxt);
            // other
            XmlRegAtomType::XmlRegexpNumberOthers
        } else {
            // all numbers
            XmlRegAtomType::XmlRegexpNumber
        }
    } else if cur == b'P' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b'c' as _ {
            NEXT!(ctxt);
            // connector
            XmlRegAtomType::XmlRegexpPunctConnector
        } else if cur == b'd' as _ {
            NEXT!(ctxt);
            // dash
            XmlRegAtomType::XmlRegexpPunctDash
        } else if cur == b's' as _ {
            NEXT!(ctxt);
            // open
            XmlRegAtomType::XmlRegexpPunctOpen
        } else if cur == b'e' as _ {
            NEXT!(ctxt);
            // close
            XmlRegAtomType::XmlRegexpPunctClose
        } else if cur == b'i' as _ {
            NEXT!(ctxt);
            // initial quote
            XmlRegAtomType::XmlRegexpPunctInitquote
        } else if cur == b'f' as _ {
            NEXT!(ctxt);
            // final quote
            XmlRegAtomType::XmlRegexpPunctFinquote
        } else if cur == b'o' as _ {
            NEXT!(ctxt);
            // other
            XmlRegAtomType::XmlRegexpPunctOthers
        } else {
            // all punctuation
            XmlRegAtomType::XmlRegexpPunct
        }
    } else if cur == b'Z' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b's' as _ {
            NEXT!(ctxt);
            // space
            XmlRegAtomType::XmlRegexpSeparSpace
        } else if cur == b'l' as _ {
            NEXT!(ctxt);
            // line
            XmlRegAtomType::XmlRegexpSeparLine
        } else if cur == b'p' as _ {
            NEXT!(ctxt);
            // paragraph
            XmlRegAtomType::XmlRegexpSeparPara
        } else {
            // all separators
            XmlRegAtomType::XmlRegexpSepar
        }
    } else if cur == b'S' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b'm' as _ {
            // math
            NEXT!(ctxt);
            XmlRegAtomType::XmlRegexpSymbolMath
        } else if cur == b'c' as _ {
            // currency
            NEXT!(ctxt);
            XmlRegAtomType::XmlRegexpSymbolCurrency
        } else if cur == b'k' as _ {
            // modifiers
            NEXT!(ctxt);
            XmlRegAtomType::XmlRegexpSymbolModifier
        } else if cur == b'o' as _ {
            // other
            NEXT!(ctxt);
            XmlRegAtomType::XmlRegexpSymbolOthers
        } else {
            // all symbols
            XmlRegAtomType::XmlRegexpSymbol
        }
    } else if cur == b'C' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b'c' as _ {
            NEXT!(ctxt);
            // control
            XmlRegAtomType::XmlRegexpOtherControl
        } else if cur == b'f' as _ {
            NEXT!(ctxt);
            // format
            XmlRegAtomType::XmlRegexpOtherFormat
        } else if cur == b'o' as _ {
            NEXT!(ctxt);
            // private use
            XmlRegAtomType::XmlRegexpOtherPrivate
        } else if cur == b'n' as _ {
            NEXT!(ctxt);
            // not assigned
            XmlRegAtomType::XmlRegexpOtherNa
        } else {
            // all others
            XmlRegAtomType::XmlRegexpOther
        }
    } else if cur == b'I' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur != b's' as _ {
            ERROR!(ctxt, "IsXXXX expected");
            return;
        }
        NEXT!(ctxt);
        let start: *const XmlChar = (*ctxt).cur;
        cur = CUR!(ctxt) as _;
        if (cur >= b'a' as _ && cur <= b'z' as _)
            || (cur >= b'A' as _ && cur <= b'Z' as _)
            || (cur >= b'0' as _ && cur <= b'9' as _)
            || cur == 0x2D
        {
            NEXT!(ctxt);
            cur = CUR!(ctxt) as _;
            while (cur >= b'a' as _ && cur <= b'z' as _)
                || (cur >= b'A' as _ && cur <= b'Z' as _)
                || (cur >= b'0' as _ && cur <= b'9' as _)
                || cur == 0x2D
            {
                NEXT!(ctxt);
                cur = CUR!(ctxt) as _;
            }
        }
        block_name = xml_strndup(start, (*ctxt).cur.offset_from(start) as _);
        XmlRegAtomType::XmlRegexpBlockName
    } else {
        ERROR!(ctxt, "Unknown char property");
        return;
    };
    if (*ctxt).atom.is_null() {
        (*ctxt).atom = xml_reg_new_atom(ctxt, typ);
        if (*ctxt).atom.is_null() {
            xml_free(block_name as _);
            return;
        }
        (*(*ctxt).atom).valuep = (!block_name.is_null()).then(|| {
            CStr::from_ptr(block_name as *const i8)
                .to_string_lossy()
                .into_owned()
        });
    } else if matches!((*(*ctxt).atom).typ, XmlRegAtomType::XmlRegexpRanges)
        && xml_reg_atom_add_range(
            ctxt,
            (*ctxt).atom,
            (*ctxt).neg,
            typ,
            0,
            0,
            Some(
                CStr::from_ptr(block_name as *const i8)
                    .to_string_lossy()
                    .as_ref(),
            ),
        )
        .is_null()
    {
        // no op
    }
    xml_free(block_name as _);
}

// Parser for the Schemas Datatype Regular Expressions
// http://www.w3.org/TR/2001/REC-xmlschema-2-20010502/#regexs

unsafe fn parse_escaped_codeunit(ctxt: XmlRegParserCtxtPtr) -> i32 {
    let mut val: i32 = 0;
    let mut cur: i32;
    for _ in 0..4 {
        NEXT!(ctxt);
        val *= 16;
        cur = CUR!(ctxt) as _;
        if cur >= b'0' as i32 && cur <= b'9' as i32 {
            val += cur - b'0' as i32;
        } else if cur >= b'A' as i32 && cur <= b'F' as i32 {
            val += cur - b'A' as i32 + 10;
        } else if cur >= b'a' as i32 && cur <= b'f' as i32 {
            val += cur - b'a' as i32 + 10;
        } else {
            ERROR!(ctxt, "Expecting hex digit");
            return -1;
        }
    }
    val
}

unsafe fn parse_escaped_codepoint(ctxt: XmlRegParserCtxtPtr) -> i32 {
    let mut val: i32 = parse_escaped_codeunit(ctxt);
    if (0xD800..=0xDBFF).contains(&val) {
        NEXT!(ctxt);
        if CUR!(ctxt) == b'\\' {
            NEXT!(ctxt);
            if CUR!(ctxt) == b'u' {
                let low: i32 = parse_escaped_codeunit(ctxt);
                if (0xDC00..=0xDFFF).contains(&low) {
                    return (val - 0xD800) * 0x400 + (low - 0xDC00) + 0x10000;
                }
            }
        }
        ERROR!(ctxt, "Invalid low surrogate pair code unit");
        val = -1;
    }
    val
}

/// ```text
/// [23] charClassEsc ::= ( SingleCharEsc | MultiCharEsc | catEsc | complEsc )
/// [24] SingleCharEsc ::= '\' [nrt\|.?*+(){}#x2D#x5B#x5D#x5E]
/// [25] catEsc   ::=   '\p{' charProp '}'
/// [26] complEsc ::=   '\P{' charProp '}'
/// [37] MultiCharEsc ::= '.' | ('\' [sSiIcCdDwW])
/// ```
#[doc(alias = "xmlFAParseCharClassEsc")]
unsafe fn xml_fa_parse_char_class_esc(ctxt: XmlRegParserCtxtPtr) {
    let mut cur: i32;

    if CUR!(ctxt) == b'.' {
        if (*ctxt).atom.is_null() {
            (*ctxt).atom = xml_reg_new_atom(ctxt, XmlRegAtomType::XmlRegexpAnychar);
        } else if matches!((*(*ctxt).atom).typ, XmlRegAtomType::XmlRegexpRanges) {
            xml_reg_atom_add_range(
                ctxt,
                (*ctxt).atom,
                (*ctxt).neg,
                XmlRegAtomType::XmlRegexpAnychar,
                0,
                0,
                None,
            );
        }
        NEXT!(ctxt);
        return;
    }
    if CUR!(ctxt) != b'\\' {
        ERROR!(ctxt, "Escaped sequence: expecting \\");
        return;
    }
    NEXT!(ctxt);
    cur = CUR!(ctxt) as _;
    if cur == b'p' as _ {
        NEXT!(ctxt);
        if CUR!(ctxt) != b'{' {
            ERROR!(ctxt, "Expecting '{'");
            return;
        }
        NEXT!(ctxt);
        xml_fa_parse_char_prop(ctxt);
        if CUR!(ctxt) != b'}' {
            ERROR!(ctxt, "Expecting '}'");
            return;
        }
        NEXT!(ctxt);
    } else if cur == b'P' as _ {
        NEXT!(ctxt);
        if CUR!(ctxt) != b'{' {
            ERROR!(ctxt, "Expecting '{'");
            return;
        }
        NEXT!(ctxt);
        xml_fa_parse_char_prop(ctxt);
        if !(*ctxt).atom.is_null() {
            (*(*ctxt).atom).neg = 1;
        }
        if CUR!(ctxt) != b'}' {
            ERROR!(ctxt, "Expecting '}'");
            return;
        }
        NEXT!(ctxt);
    } else if cur == b'n' as _
        || cur == b'r' as _
        || cur == b't' as _
        || cur == b'\\' as _
        || cur == b'|' as _
        || cur == b'.' as _
        || cur == b'?' as _
        || cur == b'*' as _
        || cur == b'+' as _
        || cur == b'(' as _
        || cur == b')' as _
        || cur == b'{' as _
        || cur == b'}' as _
        || cur == 0x2D
        || cur == 0x5B
        || cur == 0x5D
        || cur == 0x5E
        // Non-standard escape sequences:
        //                  Java 1.8|.NET Core 3.1|MSXML 6
        || cur == b'!' as _ /*   +  |     +       |    +   */
        || cur == b'"' as _ /*   +  |     +       |    +   */
        || cur == b'#' as _ /*   +  |     +       |    +   */
        || cur == b'$' as _ /*   +  |     +       |    +   */
        || cur == b'%' as _ /*   +  |     +       |    +   */
        || cur == b',' as _ /*   +  |     +       |    +   */
        || cur == b'/' as _ /*   +  |     +       |    +   */
        || cur == b':' as _ /*   +  |     +       |    +   */
        || cur == b';' as _ /*   +  |     +       |    +   */
        || cur == b'=' as _ /*   +  |     +       |    +   */
        || cur == b'>' as _ /*      |     +       |    +   */
        || cur == b'@' as _ /*   +  |     +       |    +   */
        || cur == b'`' as _ /*   +  |     +       |    +   */
        || cur == b'~' as _ /*   +  |     +       |    +   */
        || cur == b'u' as _
    /*      |     +       |    +   */
    {
        if (*ctxt).atom.is_null() {
            (*ctxt).atom = xml_reg_new_atom(ctxt, XmlRegAtomType::XmlRegexpCharval);
            if !(*ctxt).atom.is_null() {
                match TryInto::<u8>::try_into(cur) {
                    Ok(b'n') => {
                        (*(*ctxt).atom).codepoint = b'\n' as i32;
                    }
                    Ok(b'r') => {
                        (*(*ctxt).atom).codepoint = b'\r' as i32;
                    }
                    Ok(b't') => {
                        (*(*ctxt).atom).codepoint = b'\t' as i32;
                    }
                    Ok(b'u') => {
                        cur = parse_escaped_codepoint(ctxt);
                        if cur < 0 {
                            return;
                        }
                        (*(*ctxt).atom).codepoint = cur;
                    }
                    _ => {
                        (*(*ctxt).atom).codepoint = cur;
                    }
                }
            }
        } else if matches!((*(*ctxt).atom).typ, XmlRegAtomType::XmlRegexpRanges) {
            match TryInto::<u8>::try_into(cur) {
                Ok(b'n') => {
                    cur = b'\n' as _;
                }
                Ok(b'r') => {
                    cur = b'\r' as _;
                }
                Ok(b't') => {
                    cur = b'\t' as _;
                }
                _ => {}
            }
            xml_reg_atom_add_range(
                ctxt,
                (*ctxt).atom,
                (*ctxt).neg,
                XmlRegAtomType::XmlRegexpCharval,
                cur,
                cur,
                None,
            );
        }
        NEXT!(ctxt);
    } else if cur == b's' as _
        || cur == b'S' as _
        || cur == b'i' as _
        || cur == b'I' as _
        || cur == b'c' as _
        || cur == b'C' as _
        || cur == b'd' as _
        || cur == b'D' as _
        || cur == b'w' as _
        || cur == b'W' as _
    {
        let mut typ: XmlRegAtomType = XmlRegAtomType::XmlRegexpAnyspace;

        match TryInto::<u8>::try_into(cur) {
            Ok(b's') => {
                typ = XmlRegAtomType::XmlRegexpAnyspace;
            }
            Ok(b'S') => {
                typ = XmlRegAtomType::XmlRegexpNotspace;
            }
            Ok(b'i') => {
                typ = XmlRegAtomType::XmlRegexpInitname;
            }
            Ok(b'I') => {
                typ = XmlRegAtomType::XmlRegexpNotinitname;
            }
            Ok(b'c') => {
                typ = XmlRegAtomType::XmlRegexpNamechar;
            }
            Ok(b'C') => {
                typ = XmlRegAtomType::XmlRegexpNotnamechar;
            }
            Ok(b'd') => {
                typ = XmlRegAtomType::XmlRegexpDecimal;
            }
            Ok(b'D') => {
                typ = XmlRegAtomType::XmlRegexpNotdecimal;
            }
            Ok(b'w') => {
                typ = XmlRegAtomType::XmlRegexpRealchar;
            }
            Ok(b'W') => {
                typ = XmlRegAtomType::XmlRegexpNotrealchar;
            }
            _ => {}
        }
        NEXT!(ctxt);
        if (*ctxt).atom.is_null() {
            (*ctxt).atom = xml_reg_new_atom(ctxt, typ);
        } else if matches!((*(*ctxt).atom).typ, XmlRegAtomType::XmlRegexpRanges) {
            xml_reg_atom_add_range(ctxt, (*ctxt).atom, (*ctxt).neg, typ, 0, 0, None);
        }
    } else {
        ERROR!(ctxt, "Wrong escape sequence, misuse of character '\\'");
    }
}

macro_rules! NXT {
    ( $ctxt:expr, $index:expr ) => {
        *(*$ctxt).cur.add($index as usize)
    };
}

// Need PREV to check on a '-' within a Character Group. May only be used
// when it's guaranteed that cur is not at the beginning of (*ctxt).string!
macro_rules! PREV {
    ( $ctxt:expr ) => {
        *(*$ctxt).cur.sub(1)
    };
}

/// ```text
/// [17]   charRange   ::=     seRange | XmlCharRef | XmlCharIncDash
/// [18]   seRange   ::=   charOrEsc '-' charOrEsc
/// [20]   charOrEsc   ::=   XmlChar | SingleCharEsc
/// [21]   XmlChar   ::=   [^\#x2D#x5B#x5D]
/// [22]   XmlCharIncDash   ::=   [^\#x5B#x5D]
/// ```
#[doc(alias = "xmlFAParseCharRange")]
unsafe fn xml_fa_parse_char_range(ctxt: XmlRegParserCtxtPtr) {
    let mut cur: i32;
    let mut len: i32 = 0;
    let start: i32;
    let mut end: i32;

    if CUR!(ctxt) == b'\0' {
        ERROR!(ctxt, "Expecting ']'");
        return;
    }

    cur = CUR!(ctxt) as _;
    if cur == '\\' as i32 {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        match TryInto::<u8>::try_into(cur) {
            Ok(b'n') => {
                start = 0xA;
            }
            Ok(b'r') => {
                start = 0xD;
            }
            Ok(b't') => {
                start = 0x9;
            }
            Ok(b'\\') | Ok(b'|') | Ok(b'.') | Ok(b'-') | Ok(b'^') | Ok(b'?') | Ok(b'*')
            | Ok(b'+') | Ok(b'{') | Ok(b'}') | Ok(b'(') | Ok(b')') | Ok(b'[') | Ok(b']') => {
                start = cur;
            }
            _ => {
                ERROR!(ctxt, "Invalid escape value");
                return;
            }
        }
        end = start;
        len = 1;
    } else if cur != 0x5B && cur != 0x5D {
        start = CUR_SCHAR!((*ctxt).cur, len);
        end = start;
    } else {
        ERROR!(ctxt, "Expecting a char range");
        return;
    }
    // Since we are "inside" a range, we can assume (*ctxt).cur is past
    // the start of (*ctxt).string, and PREV should be safe
    if start == '-' as i32 && NXT!(ctxt, 1) != b']' && PREV!(ctxt) != b'[' && PREV!(ctxt) != b'^' {
        NEXTL!(ctxt, len);
        return;
    }
    NEXTL!(ctxt, len);
    cur = CUR!(ctxt) as _;
    if cur != '-' as i32 || NXT!(ctxt, 1) == b'[' || NXT!(ctxt, 1) == b']' {
        xml_reg_atom_add_range(
            ctxt,
            (*ctxt).atom,
            (*ctxt).neg,
            XmlRegAtomType::XmlRegexpCharval,
            start,
            end,
            None,
        );
        return;
    }
    NEXT!(ctxt);
    cur = CUR!(ctxt) as _;
    if cur == '\\' as i32 {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        match TryInto::<u8>::try_into(cur) {
            Ok(b'n') => {
                end = 0xA;
            }
            Ok(b'r') => {
                end = 0xD;
            }
            Ok(b't') => {
                end = 0x9;
            }
            Ok(b'\\') | Ok(b'|') | Ok(b'.') | Ok(b'-') | Ok(b'^') | Ok(b'?') | Ok(b'*')
            | Ok(b'+') | Ok(b'{') | Ok(b'}') | Ok(b'(') | Ok(b')') | Ok(b'[') | Ok(b']') => {
                end = cur;
            }
            _ => {
                ERROR!(ctxt, "Invalid escape value");
                return;
            }
        }
        len = 1;
    } else if cur != '\0' as i32 && cur != 0x5B && cur != 0x5D {
        end = CUR_SCHAR!((*ctxt).cur, len);
    } else {
        ERROR!(ctxt, "Expecting the end of a char range");
        return;
    }

    // TODO check that the values are acceptable character ranges for XML
    if end < start {
        ERROR!(ctxt, "End of range is before start of range");
    } else {
        NEXTL!(ctxt, len);
        xml_reg_atom_add_range(
            ctxt,
            (*ctxt).atom,
            (*ctxt).neg,
            XmlRegAtomType::XmlRegexpCharval,
            start,
            end,
            None,
        );
    }
}

/// `[14]   posCharGroup ::= ( charRange | charClassEsc  )+`
#[doc(alias = "xmlFAParsePosCharGroup")]
unsafe fn xml_fa_parse_pos_char_group(ctxt: XmlRegParserCtxtPtr) {
    while {
        if CUR!(ctxt) == b'\\' {
            xml_fa_parse_char_class_esc(ctxt);
        } else {
            xml_fa_parse_char_range(ctxt);
        }
        CUR!(ctxt) != b']' && CUR!(ctxt) != b'-' && CUR!(ctxt) != 0 && (*ctxt).error == 0
    } {}
}

/// ```text
/// [13]   charGroup    ::= posCharGroup | negCharGroup | charClassSub
/// [15]   negCharGroup ::= '^' posCharGroup
/// [16]   charClassSub ::= ( posCharGroup | negCharGroup ) '-' charClassExpr
/// [12]   charClassExpr ::= '[' charGroup ']'
/// ```
#[doc(alias = "xmlFAParseCharGroup")]
unsafe fn xml_fa_parse_char_group(ctxt: XmlRegParserCtxtPtr) {
    let neg: i32 = (*ctxt).neg;

    if CUR!(ctxt) == b'^' as _ {
        NEXT!(ctxt);
        (*ctxt).neg = ((*ctxt).neg == 0) as i32;
        xml_fa_parse_pos_char_group(ctxt);
        (*ctxt).neg = neg;
    }
    while CUR!(ctxt) != b']' && (*ctxt).error == 0 {
        if CUR!(ctxt) == b'-' && NXT!(ctxt, 1) == b'[' {
            NEXT!(ctxt); /* eat the '-' */
            NEXT!(ctxt); /* eat the '[' */
            (*ctxt).neg = 2;
            xml_fa_parse_char_group(ctxt);
            (*ctxt).neg = neg;
            if CUR!(ctxt) == b']' {
                NEXT!(ctxt);
            } else {
                ERROR!(ctxt, "charClassExpr: ']' expected");
            }
            break;
        } else {
            xml_fa_parse_pos_char_group(ctxt);
        }
    }
}

/// ```text
/// [11]   charClass   ::=     charClassEsc | charClassExpr
/// [12]   charClassExpr   ::=   '[' charGroup ']'
/// ```
#[doc(alias = "xmlFAParseCharClass")]
unsafe fn xml_fa_parse_char_class(ctxt: XmlRegParserCtxtPtr) {
    if CUR!(ctxt) == b'[' {
        NEXT!(ctxt);
        (*ctxt).atom = xml_reg_new_atom(ctxt, XmlRegAtomType::XmlRegexpRanges);
        if (*ctxt).atom.is_null() {
            return;
        }
        xml_fa_parse_char_group(ctxt);
        if CUR!(ctxt) == b']' {
            NEXT!(ctxt);
        } else {
            ERROR!(ctxt, "xmlFAParseCharClass: ']' expected");
        }
    } else {
        xml_fa_parse_char_class_esc(ctxt);
    }
}

/// `[9]   atom   ::=   Char | charClass | ( '(' regExp ')' )`
#[doc(alias = "xmlFAParseAtom")]
unsafe fn xml_fa_parse_atom(ctxt: XmlRegParserCtxtPtr) -> i32 {
    let mut codepoint: i32;
    let mut len: i32 = 0;

    codepoint = xml_fa_is_char(ctxt);
    if codepoint > 0 {
        (*ctxt).atom = xml_reg_new_atom(ctxt, XmlRegAtomType::XmlRegexpCharval);
        if (*ctxt).atom.is_null() {
            return -1;
        }
        codepoint = CUR_SCHAR!((*ctxt).cur, len);
        (*(*ctxt).atom).codepoint = codepoint;
        NEXTL!(ctxt, len);
        return 1;
    } else if CUR!(ctxt) == b'|' || CUR!(ctxt) == 0 || CUR!(ctxt) == b')' {
        return 0;
    } else if CUR!(ctxt) == b'(' {
        NEXT!(ctxt);
        if (*ctxt).depth >= 50 {
            ERROR!(ctxt, "xmlFAParseAtom: maximum nesting depth exceeded");
            return -1;
        }
        // this extra Epsilon transition is needed if we count with 0 allowed
        // unfortunately this can't be known at that point
        xml_fa_generate_epsilon_transition(ctxt, (*ctxt).state, null_mut());
        let start0: XmlRegStatePtr = (*ctxt).state;
        xml_fa_generate_epsilon_transition(ctxt, (*ctxt).state, null_mut());
        let start: XmlRegStatePtr = (*ctxt).state;
        let oldend: XmlRegStatePtr = (*ctxt).end;
        (*ctxt).end = null_mut();
        (*ctxt).atom = null_mut();
        (*ctxt).depth += 1;
        xml_fa_parse_reg_exp(ctxt, 0);
        (*ctxt).depth -= 1;
        if CUR!(ctxt) == b')' {
            NEXT!(ctxt);
        } else {
            ERROR!(ctxt, "xmlFAParseAtom: expecting ')'");
        }
        (*ctxt).atom = xml_reg_new_atom(ctxt, XmlRegAtomType::XmlRegexpSubreg);
        if (*ctxt).atom.is_null() {
            return -1;
        }
        (*(*ctxt).atom).start = start;
        (*(*ctxt).atom).start0 = start0;
        (*(*ctxt).atom).stop = (*ctxt).state;
        (*ctxt).end = oldend;
        return 1;
    } else if CUR!(ctxt) == b'[' || CUR!(ctxt) == b'\\' || CUR!(ctxt) == b'.' {
        xml_fa_parse_char_class(ctxt);
        return 1;
    }
    0
}

/// `[8]   QuantExact   ::=   [0-9]+`
///
/// Returns 0 if success or -1 in case of error
#[doc(alias = "xmlFAParseQuantExact")]
unsafe fn xml_fa_parse_quant_exact(ctxt: XmlRegParserCtxtPtr) -> i32 {
    let mut ret: i32 = 0;
    let mut ok: i32 = 0;
    let mut overflow: i32 = 0;

    while CUR!(ctxt) >= b'0' && CUR!(ctxt) <= b'9' {
        if ret > INT_MAX / 10 {
            overflow = 1;
        } else {
            let digit: i32 = CUR!(ctxt) as i32 - b'0' as i32;

            ret *= 10;
            if ret > INT_MAX - digit {
                overflow = 1;
            } else {
                ret += digit;
            }
        }
        ok = 1;
        NEXT!(ctxt);
    }
    if ok != 1 || overflow == 1 {
        return -1;
    }
    ret
}

/// ```text
/// [4]   quantifier   ::=   [?*+] | ( '{' quantity '}' )
/// [5]   quantity   ::=   quantRange | quantMin | QuantExact
/// [6]   quantRange   ::=   QuantExact ',' QuantExact
/// [7]   quantMin   ::=   QuantExact ','
/// [8]   QuantExact   ::=   [0-9]+
/// ```
#[doc(alias = "xmlFAParseQuantifier")]
unsafe fn xml_fa_parse_quantifier(ctxt: XmlRegParserCtxtPtr) -> i32 {
    let mut cur: i32;

    cur = CUR!(ctxt) as _;
    if cur == '?' as i32 || cur == '*' as i32 || cur == '+' as i32 {
        if !(*ctxt).atom.is_null() {
            if cur == '?' as i32 {
                (*(*ctxt).atom).quant = XmlRegQuantType::XmlRegexpQuantOpt;
            } else if cur == '*' as i32 {
                (*(*ctxt).atom).quant = XmlRegQuantType::XmlRegexpQuantMult;
            } else if cur == '+' as i32 {
                (*(*ctxt).atom).quant = XmlRegQuantType::XmlRegexpQuantPlus;
            }
        }
        NEXT!(ctxt);
        return 1;
    }
    if cur == '{' as i32 {
        let mut min: i32 = 0;
        let mut max: i32 = 0;

        NEXT!(ctxt);
        cur = xml_fa_parse_quant_exact(ctxt);
        if cur >= 0 {
            min = cur;
        } else {
            ERROR!(ctxt, "Improper quantifier");
        }
        if CUR!(ctxt) == b',' {
            NEXT!(ctxt);
            if CUR!(ctxt) == b'}' {
                max = INT_MAX;
            } else {
                cur = xml_fa_parse_quant_exact(ctxt);
                if cur >= 0 {
                    max = cur;
                } else {
                    ERROR!(ctxt, "Improper quantifier");
                }
            }
        }
        if CUR!(ctxt) == b'}' {
            NEXT!(ctxt);
        } else {
            ERROR!(ctxt, "Unterminated quantifier");
        }
        if max == 0 {
            max = min;
        }
        if !(*ctxt).atom.is_null() {
            (*(*ctxt).atom).quant = XmlRegQuantType::XmlRegexpQuantRange;
            (*(*ctxt).atom).min = min;
            (*(*ctxt).atom).max = max;
        }
        return 1;
    }
    0
}

/// `[3]   piece   ::=   atom quantifier?`
#[doc(alias = "xmlFAParsePiece")]
unsafe fn xml_fa_parse_piece(ctxt: XmlRegParserCtxtPtr) -> i32 {
    (*ctxt).atom = null_mut();
    let ret: i32 = xml_fa_parse_atom(ctxt);
    if ret == 0 {
        return 0;
    }
    if (*ctxt).atom.is_null() {
        ERROR!(ctxt, "internal: no atom generated");
    }
    xml_fa_parse_quantifier(ctxt);
    1
}

/// Copy a regexp range
///
/// Returns the new copy or NULL in case of error.
#[doc(alias = "xmlRegCopyRange")]
unsafe fn xml_reg_copy_range(ctxt: XmlRegParserCtxtPtr, range: XmlRegRangePtr) -> XmlRegRangePtr {
    if range.is_null() {
        return null_mut();
    }

    let ret: XmlRegRangePtr = xml_reg_new_range(
        ctxt,
        (*range).neg,
        (*range).typ,
        (*range).start,
        (*range).end,
    );
    if ret.is_null() {
        return null_mut();
    }
    if let Some(block_name) = (*range).block_name.as_deref() {
        (*ret).block_name = Some(block_name.to_owned());
    }
    ret
}

/// Allocate a new regexp range
///
/// Returns the new atom or NULL in case of error
#[doc(alias = "xmlRegCopyAtom")]
unsafe fn xml_reg_copy_atom(ctxt: XmlRegParserCtxtPtr, atom: XmlRegAtomPtr) -> XmlRegAtomPtr {
    let ret: XmlRegAtomPtr = xml_malloc(size_of::<XmlRegAtom>()) as XmlRegAtomPtr;
    if ret.is_null() {
        xml_regexp_err_memory(ctxt, "copying atom");
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlRegAtom::default());
    (*ret).typ = (*atom).typ;
    (*ret).quant = (*atom).quant;
    (*ret).min = (*atom).min;
    (*ret).max = (*atom).max;
    if !(*atom).ranges.is_empty() {
        (*ret).ranges.reserve((*atom).ranges.len());
        for &range in &(*atom).ranges {
            (*ret).ranges.push(xml_reg_copy_range(ctxt, range));
            if (*(*ret).ranges.last().unwrap()).is_null() {
                xml_reg_free_atom(ret);
                return null_mut();
            }
        }
    }
    ret
}

pub(crate) unsafe fn xml_reg_get_counter(ctxt: XmlRegParserCtxtPtr) -> usize {
    (*ctxt).counters.push(XmlRegCounter { min: -1, max: -1 });
    (*ctxt).counters.len() - 1
}

#[doc(alias = "xmlFAGenerateCountedEpsilonTransition")]
pub(crate) unsafe fn xml_fa_generate_counted_epsilon_transition(
    ctxt: XmlRegParserCtxtPtr,
    from: XmlRegStatePtr,
    mut to: XmlRegStatePtr,
    counter: i32,
) -> i32 {
    if to.is_null() {
        to = xml_reg_state_push(ctxt);
        if to.is_null() {
            return -1;
        }
        (*ctxt).state = to;
    }
    xml_reg_state_add_trans(ctxt, from, null_mut(), to, counter, -1);
    0
}

#[doc(alias = "xmlFAGenerateCountedTransition")]
pub(crate) unsafe fn xml_fa_generate_counted_transition(
    ctxt: XmlRegParserCtxtPtr,
    from: XmlRegStatePtr,
    mut to: XmlRegStatePtr,
    counter: i32,
) -> i32 {
    if to.is_null() {
        to = xml_reg_state_push(ctxt);
        if to.is_null() {
            return -1;
        }
        (*ctxt).state = to;
    }
    xml_reg_state_add_trans(ctxt, from, null_mut(), to, -1, counter);
    0
}

pub(crate) unsafe fn xml_reg_atom_push(ctxt: XmlRegParserCtxtPtr, atom: XmlRegAtomPtr) -> i32 {
    if atom.is_null() {
        ERROR!(ctxt, "atom push: atom is NULL");
        return -1;
    }
    (*atom).no = (*ctxt).atoms.len() as i32;
    (*ctxt).atoms.push(atom);
    0
}

/// Returns 0 if success and -1 in case of error.
#[doc(alias = "xmlFAGenerateTransitions")]
pub(crate) unsafe fn xml_fa_generate_transitions(
    ctxt: XmlRegParserCtxtPtr,
    from: XmlRegStatePtr,
    mut to: XmlRegStatePtr,
    atom: XmlRegAtomPtr,
) -> i32 {
    let mut nullable: i32 = 0;

    if atom.is_null() {
        ERROR!(ctxt, "generate transition: atom == NULL");
        return -1;
    }
    if matches!((*atom).typ, XmlRegAtomType::XmlRegexpSubreg) {
        // this is a subexpression handling one should not need to
        // create a new node except for XML_REGEXP_QUANT_RANGE.
        if !to.is_null()
            && (*atom).stop != to
            && !matches!((*atom).quant, XmlRegQuantType::XmlRegexpQuantRange)
        {
            // Generate an epsilon transition to link to the target
            xml_fa_generate_epsilon_transition(ctxt, (*atom).stop, to);
        }
        match (*atom).quant {
            XmlRegQuantType::XmlRegexpQuantOpt => {
                (*atom).quant = XmlRegQuantType::XmlRegexpQuantOnce;
                // transition done to the state after end of atom.
                //      1. set transition from atom start to new state
                //      2. set transition from atom end to this state.
                if to.is_null() {
                    xml_fa_generate_epsilon_transition(ctxt, (*atom).start, null_mut());
                    xml_fa_generate_epsilon_transition(ctxt, (*atom).stop, (*ctxt).state);
                } else {
                    xml_fa_generate_epsilon_transition(ctxt, (*atom).start, to);
                }
            }
            XmlRegQuantType::XmlRegexpQuantMult => {
                (*atom).quant = XmlRegQuantType::XmlRegexpQuantOnce;
                xml_fa_generate_epsilon_transition(ctxt, (*atom).start, (*atom).stop);
                xml_fa_generate_epsilon_transition(ctxt, (*atom).stop, (*atom).start);
            }
            XmlRegQuantType::XmlRegexpQuantPlus => {
                (*atom).quant = XmlRegQuantType::XmlRegexpQuantOnce;
                xml_fa_generate_epsilon_transition(ctxt, (*atom).stop, (*atom).start);
            }
            XmlRegQuantType::XmlRegexpQuantRange => {
                let inter: XmlRegStatePtr;
                let newstate: XmlRegStatePtr;

                // create the final state now if needed
                if !to.is_null() {
                    newstate = to;
                } else {
                    newstate = xml_reg_state_push(ctxt);
                    if newstate.is_null() {
                        return -1;
                    }
                }

                // The principle here is to use counted transition
                // to avoid explosion in the number of states in the
                // graph. This is clearly more complex but should not
                // be exploitable at runtime.
                if (*atom).min == 0 && (*atom).start0.is_null() {
                    // duplicate a transition based on atom to count next
                    // occurrences after 1. We cannot loop to (*atom).start
                    // directly because we need an epsilon transition to
                    // newstate.
                    // ???? For some reason it seems we never reach that
                    //     case, I suppose this got optimized out before when
                    // building the automata
                    let copy: XmlRegAtomPtr = xml_reg_copy_atom(ctxt, atom);
                    if copy.is_null() {
                        return -1;
                    }
                    (*copy).quant = XmlRegQuantType::XmlRegexpQuantOnce;
                    (*copy).min = 0;
                    (*copy).max = 0;

                    if xml_fa_generate_transitions(ctxt, (*atom).start, null_mut(), copy) < 0 {
                        xml_reg_free_atom(copy);
                        return -1;
                    }
                    inter = (*ctxt).state;
                    let counter = xml_reg_get_counter(ctxt);
                    (*ctxt).counters[counter].min = (*atom).min - 1;
                    (*ctxt).counters[counter].max = (*atom).max - 1;
                    // count the number of times we see it again
                    xml_fa_generate_counted_epsilon_transition(
                        ctxt,
                        inter,
                        (*atom).stop,
                        counter as i32,
                    );
                    // allow a way out based on the count
                    xml_fa_generate_counted_transition(ctxt, inter, newstate, counter as i32);
                    // and also allow a direct exit for 0
                    xml_fa_generate_epsilon_transition(ctxt, (*atom).start, newstate);
                } else {
                    // either we need the atom at least once or there
                    // is an (*atom).start0 allowing to easily plug the
                    // epsilon transition.
                    let counter = xml_reg_get_counter(ctxt);
                    (*ctxt).counters[counter].min = (*atom).min - 1;
                    (*ctxt).counters[counter].max = (*atom).max - 1;
                    // allow a way out based on the count
                    xml_fa_generate_counted_transition(
                        ctxt,
                        (*atom).stop,
                        newstate,
                        counter as i32,
                    );
                    // count the number of times we see it again
                    xml_fa_generate_counted_epsilon_transition(
                        ctxt,
                        (*atom).stop,
                        (*atom).start,
                        counter as i32,
                    );
                    // and if needed allow a direct exit for 0
                    if (*atom).min == 0 {
                        xml_fa_generate_epsilon_transition(ctxt, (*atom).start0, newstate);
                    }
                }
                (*atom).min = 0;
                (*atom).max = 0;
                (*atom).quant = XmlRegQuantType::XmlRegexpQuantOnce;
                (*ctxt).state = newstate;
            }
            _ => {}
        }
        if xml_reg_atom_push(ctxt, atom) < 0 {
            return -1;
        }
        return 0;
    }
    if (*atom).min == 0
        && (*atom).max == 0
        && matches!((*atom).quant, XmlRegQuantType::XmlRegexpQuantRange)
    {
        // we can discard the atom and generate an epsilon transition instead
        if to.is_null() {
            to = xml_reg_state_push(ctxt);
            if to.is_null() {
                return -1;
            }
        }
        xml_fa_generate_epsilon_transition(ctxt, from, to);
        (*ctxt).state = to;
        xml_reg_free_atom(atom);
        return 0;
    }
    if to.is_null() {
        to = xml_reg_state_push(ctxt);
        if to.is_null() {
            return -1;
        }
    }
    let end: XmlRegStatePtr = to;
    if matches!(
        (*atom).quant,
        XmlRegQuantType::XmlRegexpQuantMult | XmlRegQuantType::XmlRegexpQuantPlus
    ) {
        // Do not pollute the target state by adding transitions from
        // it as it is likely to be the shared target of multiple branches.
        // So isolate with an epsilon transition.

        let tmp: XmlRegStatePtr = xml_reg_state_push(ctxt);
        if tmp.is_null() {
            return -1;
        }
        xml_fa_generate_epsilon_transition(ctxt, tmp, to);
        to = tmp;
    }
    if matches!((*atom).quant, XmlRegQuantType::XmlRegexpQuantRange)
        && (*atom).min == 0
        && (*atom).max > 0
    {
        nullable = 1;
        (*atom).min = 1;
        if (*atom).max == 1 {
            (*atom).quant = XmlRegQuantType::XmlRegexpQuantOpt;
        }
    }
    xml_reg_state_add_trans(ctxt, from, atom, to, -1, -1);
    (*ctxt).state = end;
    match (*atom).quant {
        XmlRegQuantType::XmlRegexpQuantOpt => {
            (*atom).quant = XmlRegQuantType::XmlRegexpQuantOnce;
            xml_fa_generate_epsilon_transition(ctxt, from, to);
        }
        XmlRegQuantType::XmlRegexpQuantMult => {
            (*atom).quant = XmlRegQuantType::XmlRegexpQuantOnce;
            xml_fa_generate_epsilon_transition(ctxt, from, to);
            xml_reg_state_add_trans(ctxt, to, atom, to, -1, -1);
        }
        XmlRegQuantType::XmlRegexpQuantPlus => {
            (*atom).quant = XmlRegQuantType::XmlRegexpQuantOnce;
            xml_reg_state_add_trans(ctxt, to, atom, to, -1, -1);
        }
        XmlRegQuantType::XmlRegexpQuantRange => {
            if nullable != 0 {
                xml_fa_generate_epsilon_transition(ctxt, from, to);
            }
        }
        _ => {}
    }
    if xml_reg_atom_push(ctxt, atom) < 0 {
        return -1;
    }
    0
}

/// @to is used to optimize by removing duplicate path in automata
/// in expressions like (a|b)(c|d)
///
/// `[2]   branch   ::=   piece*`
#[doc(alias = "xmlFAParseBranch")]
unsafe fn xml_fa_parse_branch(ctxt: XmlRegParserCtxtPtr, to: XmlRegStatePtr) -> i32 {
    let mut previous: XmlRegStatePtr;
    let mut ret: i32;

    previous = (*ctxt).state;
    ret = xml_fa_parse_piece(ctxt);
    if ret == 0 {
        // Empty branch
        xml_fa_generate_epsilon_transition(ctxt, previous, to);
    } else {
        if xml_fa_generate_transitions(
            ctxt,
            previous,
            if CUR!(ctxt) == b'|' || CUR!(ctxt) == b')' || CUR!(ctxt) == 0 {
                to
            } else {
                null_mut()
            },
            (*ctxt).atom,
        ) < 0
        {
            xml_reg_free_atom((*ctxt).atom);
            (*ctxt).atom = null_mut();
            return -1;
        }
        previous = (*ctxt).state;
        (*ctxt).atom = null_mut();
    }
    while ret != 0 && (*ctxt).error == 0 {
        ret = xml_fa_parse_piece(ctxt);
        if ret != 0 {
            if xml_fa_generate_transitions(
                ctxt,
                previous,
                if CUR!(ctxt) == b'|' || CUR!(ctxt) == b')' || CUR!(ctxt) == 0 {
                    to
                } else {
                    null_mut()
                },
                (*ctxt).atom,
            ) < 0
            {
                xml_reg_free_atom((*ctxt).atom);
                (*ctxt).atom = null_mut();
                return -1;
            }
            previous = (*ctxt).state;
            (*ctxt).atom = null_mut();
        }
    }
    0
}

/// `[1]   regExp   ::=     branch  ( '|' branch )*`
#[doc(alias = "xmlFAParseRegExp")]
unsafe fn xml_fa_parse_reg_exp(ctxt: XmlRegParserCtxtPtr, top: i32) {
    // if not top start should have been generated by an epsilon trans
    let start: XmlRegStatePtr = (*ctxt).state;
    (*ctxt).end = null_mut();
    xml_fa_parse_branch(ctxt, null_mut());
    if top != 0 {
        (*(*ctxt).state).typ = XmlRegStateType::XmlRegexpFinalState;
    }
    if CUR!(ctxt) != b'|' as _ {
        (*ctxt).end = (*ctxt).state;
        return;
    }
    let end: XmlRegStatePtr = (*ctxt).state;
    while CUR!(ctxt) == b'|' as _ && (*ctxt).error == 0 {
        NEXT!(ctxt);
        (*ctxt).state = start;
        (*ctxt).end = null_mut();
        xml_fa_parse_branch(ctxt, end);
    }
    if top == 0 {
        (*ctxt).state = end;
        (*ctxt).end = end;
    }
}

/// Eliminating general epsilon transitions can get costly in the general
/// algorithm due to the large amount of generated new transitions and
/// associated comparisons. However for simple epsilon transition used just
/// to separate building blocks when generating the automata this can be
/// reduced to state elimination:
///    - if there exists an epsilon from X to Y
///    - if there is no other transition from X
///      then X and Y are semantically equivalent and X can be eliminated
///      If X is the start state then make Y the start state, else replace the
///      target of all transitions to X by transitions to Y.
///
/// If X is a final state, skip it.
/// Otherwise it would be necessary to manipulate counters for this case when
/// eliminating state 2:
/// State 1 has a transition with an atom to state 2.
/// State 2 is final and has an epsilon transition to state 1.
#[doc(alias = "xmlFAEliminateSimpleEpsilonTransitions")]
unsafe fn xml_fa_eliminate_simple_epsilon_transitions(ctxt: XmlRegParserCtxtPtr) {
    for (statenr, &state) in (*ctxt).states.iter().enumerate() {
        if state.is_null() {
            continue;
        }
        if (*state).trans.len() != 1 {
            continue;
        }
        if matches!(
            (*state).typ,
            XmlRegStateType::XmlRegexpUnreachState | XmlRegStateType::XmlRegexpFinalState
        ) {
            continue;
        }
        // is the only transition out a basic transition
        if (*state).trans[0].atom.is_null()
            && (*state).trans[0].to >= 0
            && (*state).trans[0].to != statenr as i32
            && (*state).trans[0].counter < 0
            && (*state).trans[0].count < 0
        {
            let newto = (*state).trans[0].to;

            if !matches!((*state).typ, XmlRegStateType::XmlRegexpStartState) {
                for &index in &(*state).trans_to {
                    let tmp = (*ctxt).states[index as usize];
                    for trans in (*tmp).trans.iter_mut() {
                        if trans.to == statenr as i32 {
                            trans.to = -1;
                            xml_reg_state_add_trans(
                                ctxt,
                                tmp,
                                trans.atom,
                                (*ctxt).states[newto as usize],
                                trans.counter,
                                trans.count,
                            );
                        }
                    }
                }
                if matches!((*state).typ, XmlRegStateType::XmlRegexpFinalState) {
                    (*(*ctxt).states[newto as usize]).typ = XmlRegStateType::XmlRegexpFinalState;
                }
                // eliminate the transition completely
                (*state).trans.clear();
                (*state).typ = XmlRegStateType::XmlRegexpUnreachState;
            }
        }
    }
}

#[doc(alias = "xmlFAReduceEpsilonTransitions")]
unsafe fn xml_fa_reduce_epsilon_transitions(
    ctxt: XmlRegParserCtxtPtr,
    fromnr: i32,
    tonr: i32,
    counter: i32,
) {
    let from: XmlRegStatePtr = (*ctxt).states[fromnr as usize];
    if from.is_null() {
        return;
    }
    let to: XmlRegStatePtr = (*ctxt).states[tonr as usize];
    if to.is_null() {
        return;
    }
    if matches!(
        (*to).mark,
        XmlRegMarkedType::XmlRegexpMarkStart | XmlRegMarkedType::XmlRegexpMarkVisited
    ) {
        return;
    }

    (*to).mark = XmlRegMarkedType::XmlRegexpMarkVisited;
    if matches!((*to).typ, XmlRegStateType::XmlRegexpFinalState) {
        (*from).typ = XmlRegStateType::XmlRegexpFinalState;
    }
    for trans in &(*to).trans {
        if trans.to < 0 {
            continue;
        }
        if trans.atom.is_null() {
            // Don't remove counted transitions
            // Don't loop either
            if trans.to != fromnr {
                if trans.count >= 0 {
                    let newto: i32 = trans.to;

                    xml_reg_state_add_trans(
                        ctxt,
                        from,
                        null_mut(),
                        (*ctxt).states[newto as usize],
                        -1,
                        trans.count,
                    );
                } else if trans.counter >= 0 {
                    xml_fa_reduce_epsilon_transitions(ctxt, fromnr, trans.to, trans.counter);
                } else {
                    xml_fa_reduce_epsilon_transitions(ctxt, fromnr, trans.to, counter);
                }
            }
        } else {
            let newto: i32 = trans.to;

            if trans.counter >= 0 {
                xml_reg_state_add_trans(
                    ctxt,
                    from,
                    trans.atom,
                    (*ctxt).states[newto as usize],
                    trans.counter,
                    -1,
                );
            } else {
                xml_reg_state_add_trans(
                    ctxt,
                    from,
                    trans.atom,
                    (*ctxt).states[newto as usize],
                    counter,
                    -1,
                );
            }
        }
    }
    (*to).mark = XmlRegMarkedType::XmlRegexpMarkNormal;
}

#[doc(alias = "xmlFAEliminateEpsilonTransitions")]
pub(crate) unsafe fn xml_fa_eliminate_epsilon_transitions(ctxt: XmlRegParserCtxtPtr) {
    // Eliminate simple epsilon transition and the associated unreachable states.
    xml_fa_eliminate_simple_epsilon_transitions(ctxt);
    for state in (*ctxt).states.iter_mut() {
        if !(*state).is_null() && matches!((**state).typ, XmlRegStateType::XmlRegexpUnreachState) {
            xml_reg_free_state(*state);
            *state = null_mut();
        }
    }

    let mut has_epsilon = 0;

    // Build the completed transitions bypassing the epsilons
    // Use a marking algorithm to avoid loops
    // Mark sink states too.
    // Process from the latest states backward to the start when
    // there is long cascading epsilon chains this minimize the
    // recursions and transition compares when adding the new ones
    for (statenr, &state) in (*ctxt).states.iter().enumerate().rev() {
        if state.is_null() {
            continue;
        }
        if (*state).trans.is_empty()
            && !matches!((*state).typ, XmlRegStateType::XmlRegexpFinalState)
        {
            (*state).typ = XmlRegStateType::XmlRegexpSinkState;
        }
        for trans in (*state).trans.iter_mut() {
            if trans.atom.is_null() && trans.to >= 0 {
                if trans.to == statenr as i32 {
                    trans.to = -1;
                } else if trans.count < 0 {
                    let newto = trans.to;

                    has_epsilon = 1;
                    trans.to = -2;
                    (*state).mark = XmlRegMarkedType::XmlRegexpMarkStart;
                    xml_fa_reduce_epsilon_transitions(ctxt, statenr as i32, newto, trans.counter);
                    (*state).mark = XmlRegMarkedType::XmlRegexpMarkNormal;
                }
            }
        }
    }
    // Eliminate the epsilon transitions
    if has_epsilon != 0 {
        for &state in &(*ctxt).states {
            if state.is_null() {
                continue;
            }
            for trans in (*state).trans.iter_mut() {
                if trans.atom.is_null() && trans.count < 0 && trans.to >= 0 {
                    trans.to = -1;
                }
            }
        }
    }

    // Use this pass to detect unreachable states too
    for &state in &(*ctxt).states {
        if !state.is_null() {
            (*state).reached = XmlRegMarkedType::XmlRegexpMarkNormal;
        }
    }
    let mut state = *(*ctxt).states.first().unwrap_or(&null_mut());
    if !state.is_null() {
        (*state).reached = XmlRegMarkedType::XmlRegexpMarkStart;
    }
    while !state.is_null() {
        let mut target: XmlRegStatePtr = null_mut();
        (*state).reached = XmlRegMarkedType::XmlRegexpMarkVisited;
        // Mark all states reachable from the current reachable state
        for trans in &(*state).trans {
            if trans.to >= 0 && (!trans.atom.is_null() || trans.count >= 0) {
                let newto = trans.to;

                if (*ctxt).states[newto as usize].is_null() {
                    continue;
                }
                if matches!(
                    (*(*ctxt).states[newto as usize]).reached,
                    XmlRegMarkedType::XmlRegexpMarkNormal
                ) {
                    (*(*ctxt).states[newto as usize]).reached =
                        XmlRegMarkedType::XmlRegexpMarkStart;
                    target = (*ctxt).states[newto as usize];
                }
            }
        }

        // find the next accessible state not explored
        if target.is_null() {
            for &state in (*ctxt).states.iter().skip(1) {
                if !state.is_null()
                    && matches!((*state).reached, XmlRegMarkedType::XmlRegexpMarkStart)
                {
                    target = state;
                    break;
                }
            }
        }
        state = target;
    }
    for state in (*ctxt).states.iter_mut() {
        if !(*state).is_null() && matches!((**state).reached, XmlRegMarkedType::XmlRegexpMarkNormal)
        {
            xml_reg_free_state(*state);
            *state = null_mut();
        }
    }
}

/// Allocate a two-dimensional array and set all elements to zero.
///
/// Returns the new array or NULL in case of error.
#[doc(alias = "xmlRegCalloc2")]
unsafe fn xml_reg_calloc2(dim1: usize, dim2: usize, elem_size: usize) -> *mut c_void {
    // Check for overflow
    if dim2 == 0 || elem_size == 0 || dim1 > SIZE_MAX / dim2 / elem_size {
        return null_mut();
    }
    let total_size: usize = dim1 * dim2 * elem_size;
    let ret: *mut c_void = xml_malloc(total_size);
    if !ret.is_null() {
        memset(ret, 0, total_size);
    }
    ret
}

/// Allocate a new regexp and fill it with the result from the parser
///
/// Returns the new regexp or NULL in case of error
#[doc(alias = "xmlRegEpxFromParse")]
pub(crate) unsafe fn xml_reg_epx_from_parse(ctxt: XmlRegParserCtxtPtr) -> XmlRegexpPtr {
    let ret: XmlRegexpPtr = xml_malloc(size_of::<XmlRegexp>()) as XmlRegexpPtr;
    if ret.is_null() {
        xml_regexp_err_memory(ctxt, "compiling regexp");
        return null_mut();
    }
    std::ptr::write(&mut *ret, XmlRegexp::default());
    (*ret).string = (*ctxt).string;
    (*ret).states = take(&mut (*ctxt).states);
    (*ret).atoms = take(&mut (*ctxt).atoms);
    (*ret).counters = take(&mut (*ctxt).counters);
    (*ret).determinist = (*ctxt).determinist;
    (*ret).flags = (*ctxt).flags;
    if (*ret).determinist == -1 {
        xml_regexp_is_determinist(ret);
    }

    if (*ret).determinist != 0
        && (*ret).counters.is_empty()
        && (*ctxt).negs == 0
        && !(*ret).atoms.is_empty()
        && !(*ret).atoms[0].is_null()
        && matches!((*(*ret).atoms[0]).typ, XmlRegAtomType::XmlRegexpString)
    {
        let mut nbstates: i32 = 0;
        let mut nbatoms: i32 = 0;
        let mut transdata: *mut *mut c_void;

        // Switch to a compact representation
        // 1/ counting the effective number of states left
        // 2/ counting the unique number of atoms, and check that
        //    they are all of the string type
        // 3/ build a table state x atom for the transitions

        let state_remap: *mut i32 = xml_malloc((*ret).states.len() * size_of::<i32>()) as _;
        if state_remap.is_null() {
            xml_regexp_err_memory(ctxt, "compiling regexp");
            (*ctxt).states = take(&mut (*ret).states);
            (*ctxt).atoms = take(&mut (*ret).atoms);
            (*ctxt).counters = take(&mut (*ret).counters);
            xml_free(ret as _);
            return null_mut();
        }
        for (i, &state) in (*ret).states.iter().enumerate() {
            if !state.is_null() {
                *state_remap.add(i) = nbstates;
                nbstates += 1;
            } else {
                *state_remap.add(i) = -1;
            }
        }
        let mut string_map = Vec::with_capacity((*ret).atoms.len());
        let mut string_remap = vec![0; (*ret).atoms.len()];
        for (i, &atom) in (*ret).atoms.iter().enumerate() {
            if matches!((*atom).typ, XmlRegAtomType::XmlRegexpString)
                && matches!((*atom).quant, XmlRegQuantType::XmlRegexpQuantOnce)
            {
                let mut k = nbatoms;
                let value = (*atom).valuep.as_deref().unwrap();
                for j in 0..nbatoms {
                    if string_map[j as usize] == value {
                        string_remap[i] = j;
                        k = j;
                        break;
                    }
                }
                if k >= nbatoms {
                    string_remap[i] = nbatoms;
                    string_map.push(value.to_owned());
                    nbatoms += 1;
                }
            } else {
                (*ctxt).states = take(&mut (*ret).states);
                (*ctxt).counters = take(&mut (*ret).counters);
                (*ctxt).atoms = take(&mut (*ret).atoms);
                xml_free(state_remap as _);
                xml_free(ret as _);
                return null_mut();
            }
        }
        let transitions: *mut i32 = xml_reg_calloc2(
            (nbstates + 1) as usize,
            (nbatoms + 1) as usize,
            size_of::<i32>(),
        ) as *mut i32;
        if transitions.is_null() {
            (*ctxt).states = take(&mut (*ret).states);
            (*ctxt).counters = take(&mut (*ret).counters);
            xml_free(state_remap as _);
            xml_free(ret as _);
            return null_mut();
        }

        // Allocate the transition table. The first entry for each
        // state corresponds to the state type.
        transdata = null_mut();

        for (i, &state) in (*ret).states.iter().enumerate() {
            let mut atomno: i32;
            let mut targetno: i32;
            let mut prev: i32;

            let stateno: i32 = *state_remap.add(i);
            if stateno == -1 {
                continue;
            }

            *transitions.add((stateno * (nbatoms + 1)) as usize) = (*state).typ as _;

            for trans in &(*state).trans {
                if trans.to == -1 || trans.atom.is_null() {
                    continue;
                }
                atomno = string_remap[(*trans.atom).no as usize];
                if !(*trans.atom).data.is_null() && transdata.is_null() {
                    transdata = xml_reg_calloc2(
                        nbstates as usize,
                        nbatoms as usize,
                        size_of::<*mut c_void>(),
                    ) as *mut *mut c_void;
                    if transdata.is_null() {
                        xml_regexp_err_memory(ctxt, "compiling regexp");
                        break;
                    }
                }
                targetno = *state_remap.add(trans.to as usize);
                // if the same atom can generate transitions to 2 different
                // states then it means the automata is not deterministic and
                // the compact form can't be used !
                prev = *transitions.add((stateno * (nbatoms + 1) + atomno + 1) as usize);
                if prev != 0 {
                    if prev != targetno + 1 {
                        (*ret).determinist = 0;
                        if !transdata.is_null() {
                            xml_free(transdata as _);
                        }
                        xml_free(transitions as _);
                        xml_free(state_remap as _);
                        // goto not_determ;
                        (*ctxt).string = null_mut();
                        (*ctxt).states.clear();
                        (*ctxt).atoms.clear();
                        (*ctxt).counters.clear();
                        return ret;
                    }
                } else {
                    *transitions.add((stateno * (nbatoms + 1) + atomno + 1) as usize) =
                        targetno + 1; /* to avoid 0 */
                    if !transdata.is_null() {
                        *transdata.add((stateno * nbatoms + atomno) as usize) = (*trans.atom).data;
                    }
                }
            }
        }
        (*ret).determinist = 1;
        // Cleanup of the old data
        for state in (*ret).states.drain(..) {
            xml_reg_free_state(state);
        }
        for atom in (*ret).atoms.drain(..) {
            xml_reg_free_atom(atom);
        }
        (*ret).compact = transitions;
        (*ret).transdata = transdata;
        (*ret).string_map = string_map;
        (*ret).nbstates = nbstates;
        xml_free(state_remap as _);
    }
    // not_determ:
    (*ctxt).string = null_mut();
    (*ctxt).states.clear();
    (*ctxt).atoms.clear();
    (*ctxt).counters.clear();
    ret
}

/// Parses a regular expression conforming to XML Schemas Part 2 Datatype
/// Appendix F and builds an automata suitable for testing strings against
/// that regular expression
///
/// Returns the compiled expression or NULL in case of error
#[doc(alias = "xmlRegexpCompile")]
pub unsafe fn xml_regexp_compile(regexp: *const XmlChar) -> XmlRegexpPtr {
    let mut ret: XmlRegexpPtr = null_mut();

    let ctxt: XmlRegParserCtxtPtr = xml_reg_new_parser_ctxt(regexp);
    if ctxt.is_null() {
        return null_mut();
    }

    // initialize the parser
    (*ctxt).state = xml_reg_state_push(ctxt);
    if (*ctxt).state.is_null() {
        // goto error;
        xml_reg_free_parser_ctxt(ctxt);
        return ret;
    }
    (*ctxt).start = (*ctxt).state;
    (*ctxt).end = null_mut();

    // parse the expression building an automata
    xml_fa_parse_reg_exp(ctxt, 1);
    if CUR!(ctxt) != 0 {
        ERROR!(ctxt, "xmlFAParseRegExp: extra characters");
    }
    if (*ctxt).error != 0 {
        // goto error;
        xml_reg_free_parser_ctxt(ctxt);
        return ret;
    }
    (*ctxt).end = (*ctxt).state;
    (*(*ctxt).start).typ = XmlRegStateType::XmlRegexpStartState;
    (*(*ctxt).end).typ = XmlRegStateType::XmlRegexpFinalState;

    // remove the Epsilon except for counted transitions
    xml_fa_eliminate_epsilon_transitions(ctxt);

    if (*ctxt).error != 0 {
        // goto error;
        xml_reg_free_parser_ctxt(ctxt);
        return ret;
    }
    ret = xml_reg_epx_from_parse(ctxt);

    // error:
    xml_reg_free_parser_ctxt(ctxt);
    ret
}

/// Free a regexp
#[doc(alias = "xmlRegFreeRegexp")]
pub unsafe fn xml_reg_free_regexp(regexp: XmlRegexpPtr) {
    if regexp.is_null() {
        return;
    }

    if !(*regexp).string.is_null() {
        xml_free((*regexp).string as _);
    }
    for state in (*regexp).states.drain(..) {
        xml_reg_free_state(state);
    }
    for atom in (*regexp).atoms.drain(..) {
        xml_reg_free_atom(atom);
    }
    if !(*regexp).compact.is_null() {
        xml_free((*regexp).compact as _);
    }
    if !(*regexp).transdata.is_null() {
        xml_free((*regexp).transdata as _);
    }

    drop_in_place(regexp);
    xml_free(regexp as _);
}

unsafe fn xml_fa_reg_exec_rollback(exec: XmlRegExecCtxtPtr) {
    let Some(rollback) = (*exec).rollbacks.pop() else {
        (*exec).status = -1;
        return;
    };
    (*exec).state = rollback.state;
    (*exec).index = rollback.index;
    (*exec).transno = rollback.nextbranch;
    if !(*(*exec).comp).counters.is_empty() {
        (*exec).counts.clear();
        (*exec)
            .counts
            .extend_from_slice(&rollback.counts[..(*(*exec).comp).counters.len()]);
    }
}

unsafe fn xml_reg_check_character_range(
    typ: XmlRegAtomType,
    codepoint: i32,
    mut neg: i32,
    start: i32,
    end: i32,
    block_name: Option<&str>,
) -> i32 {
    let ret = match typ {
        XmlRegAtomType::XmlRegexpString
        | XmlRegAtomType::XmlRegexpSubreg
        | XmlRegAtomType::XmlRegexpRanges
        | XmlRegAtomType::XmlRegexpEpsilon => {
            return -1;
        }
        XmlRegAtomType::XmlRegexpAnychar => codepoint != '\n' as i32 && codepoint != '\r' as i32,
        XmlRegAtomType::XmlRegexpCharval => codepoint >= start && codepoint <= end,
        XmlRegAtomType::XmlRegexpNotspace => {
            neg = (neg == 0) as i32;
            codepoint == '\n' as i32
                || codepoint == '\r' as i32
                || codepoint == '\t' as i32
                || codepoint == ' ' as i32
        }
        XmlRegAtomType::XmlRegexpAnyspace => {
            codepoint == '\n' as i32
                || codepoint == '\r' as i32
                || codepoint == '\t' as i32
                || codepoint == ' ' as i32
        }
        XmlRegAtomType::XmlRegexpNotinitname => {
            neg = (neg == 0) as i32;
            xml_is_letter(codepoint as u32) || codepoint == '_' as i32 || codepoint == ':' as i32
        }
        XmlRegAtomType::XmlRegexpInitname => {
            xml_is_letter(codepoint as u32) || codepoint == '_' as i32 || codepoint == ':' as i32
        }
        XmlRegAtomType::XmlRegexpNotnamechar => {
            neg = (neg == 0) as i32;
            xml_is_letter(codepoint as u32)
                || xml_is_digit(codepoint as u32)
                || codepoint == '.' as i32
                || codepoint == '-' as i32
                || codepoint == '_' as i32
                || codepoint == ':' as i32
                || xml_is_combining(codepoint as u32)
                || xml_is_extender(codepoint as u32)
        }
        XmlRegAtomType::XmlRegexpNamechar => {
            xml_is_letter(codepoint as u32)
                || xml_is_digit(codepoint as u32)
                || codepoint == '.' as i32
                || codepoint == '-' as i32
                || codepoint == '_' as i32
                || codepoint == ':' as i32
                || xml_is_combining(codepoint as u32)
                || xml_is_extender(codepoint as u32)
        }
        XmlRegAtomType::XmlRegexpNotdecimal => {
            neg = (neg == 0) as i32;
            xml_ucs_is_cat_nd(codepoint)
        }
        XmlRegAtomType::XmlRegexpDecimal => xml_ucs_is_cat_nd(codepoint),
        XmlRegAtomType::XmlRegexpRealchar => {
            neg = (neg == 0) as i32;
            xml_ucs_is_cat_p(codepoint)
                || xml_ucs_is_cat_z(codepoint)
                || xml_ucs_is_cat_c(codepoint)
        }
        XmlRegAtomType::XmlRegexpNotrealchar => {
            xml_ucs_is_cat_p(codepoint)
                || xml_ucs_is_cat_z(codepoint)
                || xml_ucs_is_cat_c(codepoint)
        }
        XmlRegAtomType::XmlRegexpLetter => xml_ucs_is_cat_l(codepoint),
        XmlRegAtomType::XmlRegexpLetterUppercase => xml_ucs_is_cat_lu(codepoint),
        XmlRegAtomType::XmlRegexpLetterLowercase => xml_ucs_is_cat_ll(codepoint),
        XmlRegAtomType::XmlRegexpLetterTitlecase => xml_ucs_is_cat_lt(codepoint),
        XmlRegAtomType::XmlRegexpLetterModifier => xml_ucs_is_cat_lm(codepoint),
        XmlRegAtomType::XmlRegexpLetterOthers => xml_ucs_is_cat_lo(codepoint),
        XmlRegAtomType::XmlRegexpMark => xml_ucs_is_cat_m(codepoint),
        XmlRegAtomType::XmlRegexpMarkNonspacing => xml_ucs_is_cat_mn(codepoint),
        XmlRegAtomType::XmlRegexpMarkSpacecombining => xml_ucs_is_cat_mc(codepoint),
        XmlRegAtomType::XmlRegexpMarkEnclosing => xml_ucs_is_cat_me(codepoint),
        XmlRegAtomType::XmlRegexpNumber => xml_ucs_is_cat_n(codepoint),
        XmlRegAtomType::XmlRegexpNumberDecimal => xml_ucs_is_cat_nd(codepoint),
        XmlRegAtomType::XmlRegexpNumberLetter => xml_ucs_is_cat_nl(codepoint),
        XmlRegAtomType::XmlRegexpNumberOthers => xml_ucs_is_cat_no(codepoint),
        XmlRegAtomType::XmlRegexpPunct => xml_ucs_is_cat_p(codepoint),
        XmlRegAtomType::XmlRegexpPunctConnector => xml_ucs_is_cat_pc(codepoint),
        XmlRegAtomType::XmlRegexpPunctDash => xml_ucs_is_cat_pd(codepoint),
        XmlRegAtomType::XmlRegexpPunctOpen => xml_ucs_is_cat_ps(codepoint),
        XmlRegAtomType::XmlRegexpPunctClose => xml_ucs_is_cat_pe(codepoint),
        XmlRegAtomType::XmlRegexpPunctInitquote => xml_ucs_is_cat_pi(codepoint),
        XmlRegAtomType::XmlRegexpPunctFinquote => xml_ucs_is_cat_pf(codepoint),
        XmlRegAtomType::XmlRegexpPunctOthers => xml_ucs_is_cat_po(codepoint),
        XmlRegAtomType::XmlRegexpSepar => xml_ucs_is_cat_z(codepoint),
        XmlRegAtomType::XmlRegexpSeparSpace => xml_ucs_is_cat_zs(codepoint),
        XmlRegAtomType::XmlRegexpSeparLine => xml_ucs_is_cat_zl(codepoint),
        XmlRegAtomType::XmlRegexpSeparPara => xml_ucs_is_cat_zp(codepoint),
        XmlRegAtomType::XmlRegexpSymbol => xml_ucs_is_cat_s(codepoint),
        XmlRegAtomType::XmlRegexpSymbolMath => xml_ucs_is_cat_sm(codepoint),
        XmlRegAtomType::XmlRegexpSymbolCurrency => xml_ucs_is_cat_sc(codepoint),
        XmlRegAtomType::XmlRegexpSymbolModifier => xml_ucs_is_cat_sk(codepoint),
        XmlRegAtomType::XmlRegexpSymbolOthers => xml_ucs_is_cat_so(codepoint),
        XmlRegAtomType::XmlRegexpOther => xml_ucs_is_cat_c(codepoint),
        XmlRegAtomType::XmlRegexpOtherControl => xml_ucs_is_cat_cc(codepoint),
        XmlRegAtomType::XmlRegexpOtherFormat => xml_ucs_is_cat_cf(codepoint),
        XmlRegAtomType::XmlRegexpOtherPrivate => xml_ucs_is_cat_co(codepoint),
        XmlRegAtomType::XmlRegexpOtherNa => {
            // ret = xml_ucs_isCatCn(codepoint);
            // Seems it doesn't exist anymore in recent Unicode releases
            false
        }
        XmlRegAtomType::XmlRegexpBlockName => {
            // Fix the case of block_name is `None`...
            xml_ucs_is_block(codepoint, block_name.unwrap()) != 0
        }
    };
    if neg != 0 {
        return (!ret) as i32;
    }
    ret as i32
}

unsafe fn xml_reg_check_character(atom: XmlRegAtomPtr, codepoint: i32) -> i32 {
    let mut ret: i32;

    if atom.is_null() || !xml_is_char(codepoint as u32) {
        return -1;
    }

    match (*atom).typ {
        XmlRegAtomType::XmlRegexpSubreg | XmlRegAtomType::XmlRegexpEpsilon => {
            return -1;
        }
        XmlRegAtomType::XmlRegexpCharval => {
            return (codepoint == (*atom).codepoint) as i32;
        }
        XmlRegAtomType::XmlRegexpRanges => {
            let mut accept: i32 = 0;

            for &range in &(*atom).ranges {
                if (*range).neg == 2 {
                    ret = xml_reg_check_character_range(
                        (*range).typ,
                        codepoint,
                        0,
                        (*range).start,
                        (*range).end,
                        (*range).block_name.as_deref(),
                    );
                    if ret != 0 {
                        return 0; /* excluded char */
                    }
                } else if (*range).neg != 0 {
                    ret = xml_reg_check_character_range(
                        (*range).typ,
                        codepoint,
                        0,
                        (*range).start,
                        (*range).end,
                        (*range).block_name.as_deref(),
                    );
                    if ret == 0 {
                        accept = 1;
                    } else {
                        return 0;
                    }
                } else {
                    ret = xml_reg_check_character_range(
                        (*range).typ,
                        codepoint,
                        0,
                        (*range).start,
                        (*range).end,
                        (*range).block_name.as_deref(),
                    );
                    if ret != 0 {
                        accept = 1; /* might still be excluded */
                    }
                }
            }
            return accept;
        }
        XmlRegAtomType::XmlRegexpString => {
            println!("TODO: XML_REGEXP_STRING");
            return -1;
        }
        XmlRegAtomType::XmlRegexpAnychar
        | XmlRegAtomType::XmlRegexpAnyspace
        | XmlRegAtomType::XmlRegexpNotspace
        | XmlRegAtomType::XmlRegexpInitname
        | XmlRegAtomType::XmlRegexpNotinitname
        | XmlRegAtomType::XmlRegexpNamechar
        | XmlRegAtomType::XmlRegexpNotnamechar
        | XmlRegAtomType::XmlRegexpDecimal
        | XmlRegAtomType::XmlRegexpNotdecimal
        | XmlRegAtomType::XmlRegexpRealchar
        | XmlRegAtomType::XmlRegexpNotrealchar
        | XmlRegAtomType::XmlRegexpLetter
        | XmlRegAtomType::XmlRegexpLetterUppercase
        | XmlRegAtomType::XmlRegexpLetterLowercase
        | XmlRegAtomType::XmlRegexpLetterTitlecase
        | XmlRegAtomType::XmlRegexpLetterModifier
        | XmlRegAtomType::XmlRegexpLetterOthers
        | XmlRegAtomType::XmlRegexpMark
        | XmlRegAtomType::XmlRegexpMarkNonspacing
        | XmlRegAtomType::XmlRegexpMarkSpacecombining
        | XmlRegAtomType::XmlRegexpMarkEnclosing
        | XmlRegAtomType::XmlRegexpNumber
        | XmlRegAtomType::XmlRegexpNumberDecimal
        | XmlRegAtomType::XmlRegexpNumberLetter
        | XmlRegAtomType::XmlRegexpNumberOthers
        | XmlRegAtomType::XmlRegexpPunct
        | XmlRegAtomType::XmlRegexpPunctConnector
        | XmlRegAtomType::XmlRegexpPunctDash
        | XmlRegAtomType::XmlRegexpPunctOpen
        | XmlRegAtomType::XmlRegexpPunctClose
        | XmlRegAtomType::XmlRegexpPunctInitquote
        | XmlRegAtomType::XmlRegexpPunctFinquote
        | XmlRegAtomType::XmlRegexpPunctOthers
        | XmlRegAtomType::XmlRegexpSepar
        | XmlRegAtomType::XmlRegexpSeparSpace
        | XmlRegAtomType::XmlRegexpSeparLine
        | XmlRegAtomType::XmlRegexpSeparPara
        | XmlRegAtomType::XmlRegexpSymbol
        | XmlRegAtomType::XmlRegexpSymbolMath
        | XmlRegAtomType::XmlRegexpSymbolCurrency
        | XmlRegAtomType::XmlRegexpSymbolModifier
        | XmlRegAtomType::XmlRegexpSymbolOthers
        | XmlRegAtomType::XmlRegexpOther
        | XmlRegAtomType::XmlRegexpOtherControl
        | XmlRegAtomType::XmlRegexpOtherFormat
        | XmlRegAtomType::XmlRegexpOtherPrivate
        | XmlRegAtomType::XmlRegexpOtherNa
        | XmlRegAtomType::XmlRegexpBlockName => {
            ret = xml_reg_check_character_range(
                (*atom).typ,
                codepoint,
                0,
                0,
                0,
                (*atom).valuep.as_deref(),
            );
            if (*atom).neg != 0 {
                ret = (ret == 0) as i32;
            }
        }
    }
    ret
}

unsafe fn xml_fa_reg_exec_save(exec: XmlRegExecCtxtPtr) {
    if (*exec).nb_push as usize > MAX_PUSH {
        return;
    }
    (*exec).nb_push += 1;

    let mut rollback = XmlRegExecRollback {
        state: (*exec).state,
        index: (*exec).index,
        nextbranch: (*exec).transno + 1,
        counts: vec![],
    };
    if !(*(*exec).comp).counters.is_empty() {
        rollback
            .counts
            .extend_from_slice(&(*exec).counts[..(*(*exec).comp).counters.len()]);
    }
    (*exec).rollbacks.push(rollback);
}

pub(crate) const REGEXP_ALL_COUNTER: usize = 0x123456;

unsafe fn xml_fa_reg_exec(comp: XmlRegexpPtr, content: *const XmlChar) -> i32 {
    let mut execval = XmlRegExecCtxt::default();
    let exec: XmlRegExecCtxtPtr = addr_of_mut!(execval);
    let mut ret: i32;
    let mut codepoint: i32;
    let mut len: i32;
    let mut deter: i32;

    (*exec).input_string = content;
    (*exec).index = 0;
    (*exec).nb_push = 0;
    (*exec).determinist = 1;
    (*exec).rollbacks.clear();
    (*exec).status = 0;
    (*exec).comp = comp;
    (*exec).state = *(*comp).states.first().unwrap_or(&null_mut());
    (*exec).transno = 0;
    (*exec).transcount = 0;
    (*exec).input_stack = null_mut();
    (*exec).input_stack_max = 0;
    if !(*comp).counters.is_empty() {
        (*exec).counts.clear();
        (*exec).counts.resize((*comp).counters.len(), 0);
    } else {
        (*exec).counts.clear();
    }
    'error: {
        'b: while (*exec).status == 0
            && !(*exec).state.is_null()
            && (*(*exec).input_string.add((*exec).index as usize) != 0
                || (!(*exec).state.is_null()
                    && !matches!((*(*exec).state).typ, XmlRegStateType::XmlRegexpFinalState)))
        {
            let mut atom: XmlRegAtomPtr;

            'rollback: {
                // If end of input on non-terminal state, rollback, however we may
                // still have epsilon like transition for counted transitions
                // on counters, in that case don't break too early.  Additionally,
                // if we are working on a range like "AB{0,2}", where B is not present,
                // we don't want to break.
                len = 1;
                if *(*exec).input_string.add((*exec).index as usize) == 0
                    && (*exec).counts.is_empty()
                {
                    // if there is a transition, we must check if
                    //  atom allows minOccurs of 0
                    if (*exec).transno < (*(*exec).state).trans.len() as i32 {
                        let trans = &(*(*exec).state).trans[(*exec).transno as usize];
                        if trans.to >= 0 {
                            atom = trans.atom;
                            if !((*atom).min == 0 && (*atom).max > 0) {
                                break 'rollback;
                            }
                        }
                    } else {
                        break 'rollback;
                    }
                }

                (*exec).transcount = 0;
                (*exec).transno -= 1;
                while {
                    (*exec).transno += 1;
                    (*exec).transno < (*(*exec).state).trans.len() as i32
                } {
                    let trans = &(*(*exec).state).trans[(*exec).transno as usize];
                    if trans.to < 0 {
                        continue;
                    }
                    atom = trans.atom;
                    ret = 0;
                    deter = 1;
                    if trans.count >= 0 {
                        if (*exec).counts.is_empty() {
                            (*exec).status = -1;
                            break 'error;
                        }
                        // A counted transition.

                        let count = (*exec).counts[trans.count as usize];
                        let counter = (*(*exec).comp).counters[trans.count as usize];
                        ret = (count >= counter.min && count <= counter.max) as i32;
                        if ret != 0 && counter.min != counter.max {
                            deter = 0;
                        }
                    } else if atom.is_null() {
                        eprintln!("epsilon transition left at runtime");
                        (*exec).status = -2;
                        break;
                    } else if *(*exec).input_string.add((*exec).index as usize) != 0 {
                        codepoint =
                            CUR_SCHAR!((*exec).input_string.add((*exec).index as usize), len);
                        ret = xml_reg_check_character(atom, codepoint);
                        if ret == 1 && (*atom).min >= 0 && (*atom).max > 0 {
                            let to: XmlRegStatePtr = (*comp).states[trans.to as usize];

                            // this is a multiple input sequence
                            // If there is a counter associated increment it now.
                            // do not increment if the counter is already over the
                            // maximum limit in which case get to next transition
                            if trans.counter >= 0 {
                                if (*exec).counts.is_empty() || (*exec).comp.is_null() {
                                    (*exec).status = -1;
                                    break 'error;
                                }
                                let counter = (*(*exec).comp).counters[trans.counter as usize];
                                if (*exec).counts[trans.counter as usize] >= counter.max {
                                    // for loop on transitions
                                    continue;
                                }
                            }
                            // Save before incrementing
                            if (*(*exec).state).trans.len() as i32 > (*exec).transno + 1 {
                                xml_fa_reg_exec_save(exec);
                            }
                            if trans.counter >= 0 {
                                (*exec).counts[trans.counter as usize] += 1;
                            }
                            (*exec).transcount = 1;
                            'inner: while {
                                // Try to progress as much as possible on the input
                                if (*exec).transcount == (*atom).max {
                                    break 'inner;
                                }
                                (*exec).index += len;
                                // End of input: stop here
                                if *(*exec).input_string.add((*exec).index as usize) == 0 {
                                    (*exec).index -= len;
                                    break 'inner;
                                }
                                if (*exec).transcount >= (*atom).min {
                                    let transno: i32 = (*exec).transno;
                                    let state: XmlRegStatePtr = (*exec).state;

                                    // The transition is acceptable save it
                                    (*exec).transno = -1; /* trick */
                                    (*exec).state = to;
                                    xml_fa_reg_exec_save(exec);
                                    (*exec).transno = transno;
                                    (*exec).state = state;
                                }
                                codepoint = CUR_SCHAR!(
                                    (*exec).input_string.add((*exec).index as usize),
                                    len
                                );
                                ret = xml_reg_check_character(atom, codepoint);
                                (*exec).transcount += 1;
                                ret == 1
                            } {}
                            if (*exec).transcount < (*atom).min {
                                ret = 0;
                            }

                            // If the last check failed but one transition was found
                            // possible, rollback
                            if ret < 0 {
                                ret = 0;
                            }
                            if ret == 0 {
                                break 'rollback;
                            }
                            if trans.counter >= 0 {
                                if (*exec).counts.is_empty() {
                                    (*exec).status = -1;
                                    break 'error;
                                }
                                (*exec).counts[trans.counter as usize] -= 1;
                            }
                        } else if ret == 0 && (*atom).min == 0 && (*atom).max > 0 {
                            // we don't match on the codepoint, but minOccurs of 0
                            // says that's ok.  Setting len to 0 inhibits stepping
                            // over the codepoint.
                            (*exec).transcount = 1;
                            len = 0;
                            ret = 1;
                        }
                    } else if (*atom).min == 0 && (*atom).max > 0 {
                        // another spot to match when minOccurs is 0
                        (*exec).transcount = 1;
                        len = 0;
                        ret = 1;
                    }
                    if ret == 1 {
                        if trans.nd == 1
                            || (trans.count >= 0
                                && deter == 0
                                && (*(*exec).state).trans.len() as i32 > (*exec).transno + 1)
                        {
                            xml_fa_reg_exec_save(exec);
                        }
                        if trans.counter >= 0 {
                            // make sure we don't go over the counter maximum value
                            if (*exec).counts.is_empty() || (*exec).comp.is_null() {
                                (*exec).status = -1;
                                break 'error;
                            }
                            let counter = (*(*exec).comp).counters[trans.counter as usize];
                            if (*exec).counts[trans.counter as usize] >= counter.max {
                                // for loop on transitions
                                continue;
                            }
                            (*exec).counts[trans.counter as usize] += 1;
                        }
                        if trans.count >= 0 && (trans.count as usize) < REGEXP_ALL_COUNTER {
                            if (*exec).counts.is_empty() {
                                (*exec).status = -1;
                                break 'error;
                            }
                            (*exec).counts[trans.count as usize] = 0;
                        }
                        (*exec).state = (*comp).states[trans.to as usize];
                        (*exec).transno = 0;
                        if !trans.atom.is_null() {
                            (*exec).index += len;
                        }
                        continue 'b;
                    } else if ret < 0 {
                        (*exec).status = -4;
                        break;
                    }
                }
                if (*exec).transno != 0 || (*(*exec).state).trans.is_empty() {
                    // rollback:
                    break 'rollback;
                }
                continue 'b;
            }
            // Failed to find a way out
            (*exec).determinist = 0;
            xml_fa_reg_exec_rollback(exec);
        }
    }
    // error:
    (*exec).rollbacks.clear();
    if (*exec).state.is_null() {
        return -1;
    }
    (*exec).counts.clear();
    if (*exec).status == 0 {
        return 1;
    }
    if (*exec).status == -1 {
        if (*exec).nb_push as usize > MAX_PUSH {
            return -1;
        }
        return 0;
    }
    (*exec).status
}

/// Check if the regular expression generates the value
///
/// Returns 1 if it matches, 0 if not and a negative value in case of error
#[doc(alias = "xmlRegexpExec")]
pub unsafe fn xml_regexp_exec(comp: XmlRegexpPtr, content: *const XmlChar) -> i32 {
    if comp.is_null() || content.is_null() {
        return -1;
    }
    xml_fa_reg_exec(comp, content)
}

unsafe fn xml_reg_print_atom_type<'a>(output: &mut (impl Write + 'a), typ: XmlRegAtomType) {
    match typ {
        XmlRegAtomType::XmlRegexpEpsilon => {
            write!(output, "epsilon ");
        }
        XmlRegAtomType::XmlRegexpCharval => {
            write!(output, "charval ");
        }
        XmlRegAtomType::XmlRegexpRanges => {
            write!(output, "ranges ");
        }
        XmlRegAtomType::XmlRegexpSubreg => {
            write!(output, "subexpr ");
        }
        XmlRegAtomType::XmlRegexpString => {
            write!(output, "string ");
        }
        XmlRegAtomType::XmlRegexpAnychar => {
            write!(output, "anychar ");
        }
        XmlRegAtomType::XmlRegexpAnyspace => {
            write!(output, "anyspace ");
        }
        XmlRegAtomType::XmlRegexpNotspace => {
            write!(output, "notspace ");
        }
        XmlRegAtomType::XmlRegexpInitname => {
            write!(output, "initname ");
        }
        XmlRegAtomType::XmlRegexpNotinitname => {
            write!(output, "notinitname ");
        }
        XmlRegAtomType::XmlRegexpNamechar => {
            write!(output, "namechar ");
        }
        XmlRegAtomType::XmlRegexpNotnamechar => {
            write!(output, "notnamechar ");
        }
        XmlRegAtomType::XmlRegexpDecimal => {
            write!(output, "decimal ");
        }
        XmlRegAtomType::XmlRegexpNotdecimal => {
            write!(output, "notdecimal ");
        }
        XmlRegAtomType::XmlRegexpRealchar => {
            write!(output, "realchar ");
        }
        XmlRegAtomType::XmlRegexpNotrealchar => {
            write!(output, "notrealchar ");
        }
        XmlRegAtomType::XmlRegexpLetter => {
            write!(output, "LETTER ");
        }
        XmlRegAtomType::XmlRegexpLetterUppercase => {
            write!(output, "LETTER_UPPERCASE ");
        }
        XmlRegAtomType::XmlRegexpLetterLowercase => {
            write!(output, "LETTER_LOWERCASE ");
        }
        XmlRegAtomType::XmlRegexpLetterTitlecase => {
            write!(output, "LETTER_TITLECASE ");
        }
        XmlRegAtomType::XmlRegexpLetterModifier => {
            write!(output, "LETTER_MODIFIER ");
        }
        XmlRegAtomType::XmlRegexpLetterOthers => {
            write!(output, "LETTER_OTHERS ");
        }
        XmlRegAtomType::XmlRegexpMark => {
            write!(output, "MARK ");
        }
        XmlRegAtomType::XmlRegexpMarkNonspacing => {
            write!(output, "MARK_NONSPACING ");
        }
        XmlRegAtomType::XmlRegexpMarkSpacecombining => {
            write!(output, "MARK_SPACECOMBINING ");
        }
        XmlRegAtomType::XmlRegexpMarkEnclosing => {
            write!(output, "MARK_ENCLOSING ");
        }
        XmlRegAtomType::XmlRegexpNumber => {
            write!(output, "NUMBER ");
        }
        XmlRegAtomType::XmlRegexpNumberDecimal => {
            write!(output, "NUMBER_DECIMAL ");
        }
        XmlRegAtomType::XmlRegexpNumberLetter => {
            write!(output, "NUMBER_LETTER ");
        }
        XmlRegAtomType::XmlRegexpNumberOthers => {
            write!(output, "NUMBER_OTHERS ");
        }
        XmlRegAtomType::XmlRegexpPunct => {
            write!(output, "PUNCT ");
        }
        XmlRegAtomType::XmlRegexpPunctConnector => {
            write!(output, "PUNCT_CONNECTOR ");
        }
        XmlRegAtomType::XmlRegexpPunctDash => {
            write!(output, "PUNCT_DASH ");
        }
        XmlRegAtomType::XmlRegexpPunctOpen => {
            write!(output, "PUNCT_OPEN ");
        }
        XmlRegAtomType::XmlRegexpPunctClose => {
            write!(output, "PUNCT_CLOSE ");
        }
        XmlRegAtomType::XmlRegexpPunctInitquote => {
            write!(output, "PUNCT_INITQUOTE ");
        }
        XmlRegAtomType::XmlRegexpPunctFinquote => {
            write!(output, "PUNCT_FINQUOTE ");
        }
        XmlRegAtomType::XmlRegexpPunctOthers => {
            write!(output, "PUNCT_OTHERS ");
        }
        XmlRegAtomType::XmlRegexpSepar => {
            write!(output, "SEPAR ");
        }
        XmlRegAtomType::XmlRegexpSeparSpace => {
            write!(output, "SEPAR_SPACE ");
        }
        XmlRegAtomType::XmlRegexpSeparLine => {
            write!(output, "SEPAR_LINE ");
        }
        XmlRegAtomType::XmlRegexpSeparPara => {
            write!(output, "SEPAR_PARA ");
        }
        XmlRegAtomType::XmlRegexpSymbol => {
            write!(output, "SYMBOL ");
        }
        XmlRegAtomType::XmlRegexpSymbolMath => {
            write!(output, "SYMBOL_MATH ");
        }
        XmlRegAtomType::XmlRegexpSymbolCurrency => {
            write!(output, "SYMBOL_CURRENCY ");
        }
        XmlRegAtomType::XmlRegexpSymbolModifier => {
            write!(output, "SYMBOL_MODIFIER ");
        }
        XmlRegAtomType::XmlRegexpSymbolOthers => {
            write!(output, "SYMBOL_OTHERS ");
        }
        XmlRegAtomType::XmlRegexpOther => {
            write!(output, "OTHER ");
        }
        XmlRegAtomType::XmlRegexpOtherControl => {
            write!(output, "OTHER_CONTROL ");
        }
        XmlRegAtomType::XmlRegexpOtherFormat => {
            write!(output, "OTHER_FORMAT ");
        }
        XmlRegAtomType::XmlRegexpOtherPrivate => {
            write!(output, "OTHER_PRIVATE ");
        }
        XmlRegAtomType::XmlRegexpOtherNa => {
            write!(output, "OTHER_NA ");
        }
        XmlRegAtomType::XmlRegexpBlockName => {
            write!(output, "BLOCK ");
        }
    }
}

unsafe fn xml_reg_print_quant_type<'a>(output: &mut (impl Write + 'a), typ: XmlRegQuantType) {
    match typ {
        XmlRegQuantType::XmlRegexpQuantEpsilon => {
            write!(output, "epsilon ");
        }
        XmlRegQuantType::XmlRegexpQuantOnce => {
            write!(output, "once ");
        }
        XmlRegQuantType::XmlRegexpQuantOpt => {
            write!(output, "? ");
        }
        XmlRegQuantType::XmlRegexpQuantMult => {
            write!(output, "* ");
        }
        XmlRegQuantType::XmlRegexpQuantPlus => {
            write!(output, "+ ");
        }
        XmlRegQuantType::XmlRegexpQuantRange => {
            write!(output, "range ");
        }
        XmlRegQuantType::XmlRegexpQuantOnceonly => {
            write!(output, "onceonly ");
        }
        XmlRegQuantType::XmlRegexpQuantAll => {
            write!(output, "all ");
        }
    }
}

unsafe fn xml_reg_print_range<'a>(output: &mut (impl Write + 'a), range: XmlRegRangePtr) {
    write!(output, "  range: ");
    if (*range).neg != 0 {
        write!(output, "negative ");
    }
    xml_reg_print_atom_type(output, (*range).typ);
    writeln!(
        output,
        "{} - {}",
        char::from_u32((*range).start as u32).unwrap(),
        char::from_u32((*range).end as u32).unwrap()
    );
}

unsafe fn xml_reg_print_atom<'a>(output: &mut (impl Write + 'a), atom: XmlRegAtomPtr) {
    write!(output, " atom: ");
    if atom.is_null() {
        writeln!(output, "NULL");
        return;
    }
    if (*atom).neg != 0 {
        write!(output, "not ");
    }
    xml_reg_print_atom_type(output, (*atom).typ);
    xml_reg_print_quant_type(output, (*atom).quant);
    if matches!((*atom).quant, XmlRegQuantType::XmlRegexpQuantRange) {
        write!(output, "{}-{} ", (*atom).min, (*atom).max);
    }
    if matches!((*atom).typ, XmlRegAtomType::XmlRegexpString) {
        write!(output, "'{}' ", (*atom).valuep.as_deref().unwrap());
    }
    if matches!((*atom).typ, XmlRegAtomType::XmlRegexpCharval) {
        writeln!(
            output,
            "char {}",
            char::from_u32((*atom).codepoint as u32).unwrap()
        );
    } else if matches!((*atom).typ, XmlRegAtomType::XmlRegexpRanges) {
        writeln!(output, "{} entries", (*atom).ranges.len());
        for &range in &(*atom).ranges {
            xml_reg_print_range(output, range);
        }
    } else if matches!((*atom).typ, XmlRegAtomType::XmlRegexpSubreg) {
        writeln!(
            output,
            "start {} end {}",
            (*(*atom).start).no,
            (*(*atom).stop).no
        );
    } else {
        writeln!(output);
    }
}

unsafe fn xml_reg_print_trans<'a>(output: &mut (impl Write + 'a), trans: &XmlRegTrans) {
    write!(output, "  trans: ");
    if trans.to < 0 {
        writeln!(output, "removed");
        return;
    }
    if trans.nd != 0 {
        if trans.nd == 2 {
            write!(output, "last not determinist, ");
        } else {
            write!(output, "not determinist, ");
        }
    }
    if trans.counter >= 0 {
        write!(output, "counted {}, ", trans.counter);
    }
    if trans.count as usize == REGEXP_ALL_COUNTER {
        write!(output, "all transition, ");
    } else if trans.count >= 0 {
        write!(output, "count based {}, ", trans.count);
    }
    if trans.atom.is_null() {
        writeln!(output, "epsilon to {}", trans.to);
        return;
    }
    if matches!((*trans.atom).typ, XmlRegAtomType::XmlRegexpCharval) {
        write!(
            output,
            "char {} ",
            char::from_u32((*trans.atom).codepoint as u32).unwrap()
        );
    }
    writeln!(output, "atom {}, to {}", (*trans.atom).no, trans.to);
}

unsafe fn xml_reg_print_state<'a>(output: &mut (impl Write + 'a), state: XmlRegStatePtr) {
    write!(output, " state: ");
    if state.is_null() {
        writeln!(output, "NULL");
        return;
    }
    if matches!((*state).typ, XmlRegStateType::XmlRegexpStartState) {
        write!(output, "START ");
    }
    if matches!((*state).typ, XmlRegStateType::XmlRegexpFinalState) {
        write!(output, "FINAL ");
    }

    writeln!(
        output,
        "{}, {} transitions:",
        (*state).no,
        (*state).trans.len(),
    );
    for trans in &(*state).trans {
        xml_reg_print_trans(output, trans);
    }
}

/// Print the content of the compiled regular expression
#[doc(alias = "xmlRegexpPrint")]
pub unsafe fn xml_regexp_print<'a>(output: &mut (impl Write + 'a), regexp: XmlRegexpPtr) {
    write!(output, " regexp: ");
    if regexp.is_null() {
        writeln!(output, "NULL");
        return;
    }
    write!(
        output,
        "'{}' ",
        CStr::from_ptr((*regexp).string as *const i8).to_string_lossy()
    );
    writeln!(output);
    writeln!(output, "{} atoms:", (*regexp).atoms.len());
    for (i, &atom) in (*regexp).atoms.iter().enumerate() {
        write!(output, " {i:02} ");
        xml_reg_print_atom(output, atom);
    }
    write!(output, "{} states:", (*regexp).states.len());
    writeln!(output);
    for &state in &(*regexp).states {
        xml_reg_print_state(output, state);
    }
    writeln!(output, "{} counters:", (*regexp).counters.len());
    for (i, counter) in (*regexp).counters.iter().enumerate() {
        writeln!(output, " {i}: min {} max {}", counter.min, counter.max);
    }
}

/// Compares two atoms to check whether they are the same exactly
/// this is used to remove equivalent transitions
///
/// Returns 1 if same and 0 otherwise
#[doc(alias = "xmlFAEqualAtoms")]
unsafe fn xml_fa_equal_atoms(atom1: XmlRegAtomPtr, atom2: XmlRegAtomPtr, deep: i32) -> i32 {
    let mut ret: i32 = 0;

    if atom1 == atom2 {
        return 1;
    }
    if atom1.is_null() || atom2.is_null() {
        return 0;
    }

    if (*atom1).typ != (*atom2).typ {
        return 0;
    }
    match (*atom1).typ {
        XmlRegAtomType::XmlRegexpEpsilon => {
            ret = 0;
        }
        XmlRegAtomType::XmlRegexpString => {
            if deep == 0 {
                ret = std::ptr::eq(&(*atom1).valuep, &(*atom2).valuep) as i32;
            } else {
                ret = ((*atom1).valuep == (*atom2).valuep) as i32;
            }
        }
        XmlRegAtomType::XmlRegexpCharval => {
            ret = ((*atom1).codepoint == (*atom2).codepoint) as i32;
        }
        XmlRegAtomType::XmlRegexpRanges => {
            // too hard to do in the general case
            ret = 0;
        }
        _ => {}
    }
    ret
}

/// Compares two atoms type to check whether they intersect in some ways,
/// this is used by xmlFACompareAtoms only
///
/// Returns 1 if they may intersect and 0 otherwise
#[doc(alias = "xmlFACompareAtomTypes")]
unsafe fn xml_fa_compare_atom_types(mut type1: XmlRegAtomType, mut type2: XmlRegAtomType) -> i32 {
    if matches!(
        type1,
        XmlRegAtomType::XmlRegexpEpsilon
            | XmlRegAtomType::XmlRegexpCharval
            | XmlRegAtomType::XmlRegexpRanges
            | XmlRegAtomType::XmlRegexpSubreg
            | XmlRegAtomType::XmlRegexpString
            | XmlRegAtomType::XmlRegexpAnychar
    ) {
        return 1;
    }
    if matches!(
        type2,
        XmlRegAtomType::XmlRegexpEpsilon
            | XmlRegAtomType::XmlRegexpCharval
            | XmlRegAtomType::XmlRegexpRanges
            | XmlRegAtomType::XmlRegexpSubreg
            | XmlRegAtomType::XmlRegexpString
            | XmlRegAtomType::XmlRegexpAnychar
    ) {
        return 1;
    }

    if type1 == type2 {
        return 1;
    }

    // simplify subsequent compares by making sure type1 < type2
    if type1 > type2 {
        std::mem::swap(&mut type1, &mut type2);
    }
    match type1 {
        XmlRegAtomType::XmlRegexpAnyspace => {
            // \s
            // can't be a letter, number, mark, punctuation, symbol
            if type2 == XmlRegAtomType::XmlRegexpNotspace
                || (type2 >= XmlRegAtomType::XmlRegexpLetter
                    && type2 <= XmlRegAtomType::XmlRegexpLetterOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpNumber
                    && type2 <= XmlRegAtomType::XmlRegexpNumberOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpMark
                    && type2 <= XmlRegAtomType::XmlRegexpMarkEnclosing)
                || (type2 >= XmlRegAtomType::XmlRegexpPunct
                    && type2 <= XmlRegAtomType::XmlRegexpPunctOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpSymbol
                    && type2 <= XmlRegAtomType::XmlRegexpSymbolOthers)
            {
                return 0;
            }
        }
        XmlRegAtomType::XmlRegexpNotspace => { /* \S */ }
        XmlRegAtomType::XmlRegexpInitname => {
            // \l
            // can't be a number, mark, separator, punctuation, symbol or other
            if type2 == XmlRegAtomType::XmlRegexpNotinitname
                || (type2 >= XmlRegAtomType::XmlRegexpNumber
                    && type2 <= XmlRegAtomType::XmlRegexpNumberOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpMark
                    && type2 <= XmlRegAtomType::XmlRegexpMarkEnclosing)
                || (type2 >= XmlRegAtomType::XmlRegexpSepar
                    && type2 <= XmlRegAtomType::XmlRegexpSeparPara)
                || (type2 >= XmlRegAtomType::XmlRegexpPunct
                    && type2 <= XmlRegAtomType::XmlRegexpPunctOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpSymbol
                    && type2 <= XmlRegAtomType::XmlRegexpSymbolOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpOther
                    && type2 <= XmlRegAtomType::XmlRegexpOtherNa)
            {
                return 0;
            }
        }
        XmlRegAtomType::XmlRegexpNotinitname => { /* \L */ }
        XmlRegAtomType::XmlRegexpNamechar => {
            // \c
            // can't be a mark, separator, punctuation, symbol or other
            if type2 == XmlRegAtomType::XmlRegexpNotnamechar
                || (type2 >= XmlRegAtomType::XmlRegexpMark
                    && type2 <= XmlRegAtomType::XmlRegexpMarkEnclosing)
                || (type2 >= XmlRegAtomType::XmlRegexpPunct
                    && type2 <= XmlRegAtomType::XmlRegexpPunctOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpSepar
                    && type2 <= XmlRegAtomType::XmlRegexpSeparPara)
                || (type2 >= XmlRegAtomType::XmlRegexpSymbol
                    && type2 <= XmlRegAtomType::XmlRegexpSymbolOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpOther
                    && type2 <= XmlRegAtomType::XmlRegexpOtherNa)
            {
                return 0;
            }
        }
        XmlRegAtomType::XmlRegexpNotnamechar => { /* \C */ }
        XmlRegAtomType::XmlRegexpDecimal => {
            // \d
            // can't be a letter, mark, separator, punctuation, symbol or other
            if type2 == XmlRegAtomType::XmlRegexpNotdecimal
                || type2 == XmlRegAtomType::XmlRegexpRealchar
                || (type2 >= XmlRegAtomType::XmlRegexpLetter
                    && type2 <= XmlRegAtomType::XmlRegexpLetterOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpMark
                    && type2 <= XmlRegAtomType::XmlRegexpMarkEnclosing)
                || (type2 >= XmlRegAtomType::XmlRegexpPunct
                    && type2 <= XmlRegAtomType::XmlRegexpPunctOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpSepar
                    && type2 <= XmlRegAtomType::XmlRegexpSeparPara)
                || (type2 >= XmlRegAtomType::XmlRegexpSymbol
                    && type2 <= XmlRegAtomType::XmlRegexpSymbolOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpOther
                    && type2 <= XmlRegAtomType::XmlRegexpOtherNa)
            {
                return 0;
            }
        }
        XmlRegAtomType::XmlRegexpNotdecimal => { /* \D */ }
        XmlRegAtomType::XmlRegexpRealchar => {
            // \w
            // can't be a mark, separator, punctuation, symbol or other
            if type2 == XmlRegAtomType::XmlRegexpNotdecimal
                || (type2 >= XmlRegAtomType::XmlRegexpMark
                    && type2 <= XmlRegAtomType::XmlRegexpMarkEnclosing)
                || (type2 >= XmlRegAtomType::XmlRegexpPunct
                    && type2 <= XmlRegAtomType::XmlRegexpPunctOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpSepar
                    && type2 <= XmlRegAtomType::XmlRegexpSeparPara)
                || (type2 >= XmlRegAtomType::XmlRegexpSymbol
                    && type2 <= XmlRegAtomType::XmlRegexpSymbolOthers)
                || (type2 >= XmlRegAtomType::XmlRegexpOther
                    && type2 <= XmlRegAtomType::XmlRegexpOtherNa)
            {
                return 0;
            }
        }
        XmlRegAtomType::XmlRegexpNotrealchar => { /* \W */ }
        // at that point we know both type 1 and type2 are from
        // character categories are ordered and are different,
        // it becomes simple because this is a partition
        XmlRegAtomType::XmlRegexpLetter => {
            if type2 <= XmlRegAtomType::XmlRegexpLetterOthers {
                return 1;
            }
            return 0;
        }
        XmlRegAtomType::XmlRegexpLetterUppercase
        | XmlRegAtomType::XmlRegexpLetterLowercase
        | XmlRegAtomType::XmlRegexpLetterTitlecase
        | XmlRegAtomType::XmlRegexpLetterModifier
        | XmlRegAtomType::XmlRegexpLetterOthers => {
            return 0;
        }
        XmlRegAtomType::XmlRegexpMark => {
            if type2 <= XmlRegAtomType::XmlRegexpMarkEnclosing {
                return 1;
            }
            return 0;
        }
        XmlRegAtomType::XmlRegexpMarkNonspacing
        | XmlRegAtomType::XmlRegexpMarkSpacecombining
        | XmlRegAtomType::XmlRegexpMarkEnclosing => {
            return 0;
        }
        XmlRegAtomType::XmlRegexpNumber => {
            if type2 <= XmlRegAtomType::XmlRegexpNumberOthers {
                return 1;
            }
            return 0;
        }
        XmlRegAtomType::XmlRegexpNumberDecimal
        | XmlRegAtomType::XmlRegexpNumberLetter
        | XmlRegAtomType::XmlRegexpNumberOthers => {
            return 0;
        }
        XmlRegAtomType::XmlRegexpPunct => {
            if type2 <= XmlRegAtomType::XmlRegexpPunctOthers {
                return 1;
            }
            return 0;
        }
        XmlRegAtomType::XmlRegexpPunctConnector
        | XmlRegAtomType::XmlRegexpPunctDash
        | XmlRegAtomType::XmlRegexpPunctOpen
        | XmlRegAtomType::XmlRegexpPunctClose
        | XmlRegAtomType::XmlRegexpPunctInitquote
        | XmlRegAtomType::XmlRegexpPunctFinquote
        | XmlRegAtomType::XmlRegexpPunctOthers => {
            return 0;
        }
        XmlRegAtomType::XmlRegexpSepar => {
            if type2 <= XmlRegAtomType::XmlRegexpSeparPara {
                return 1;
            }
            return 0;
        }
        XmlRegAtomType::XmlRegexpSeparSpace
        | XmlRegAtomType::XmlRegexpSeparLine
        | XmlRegAtomType::XmlRegexpSeparPara => {
            return 0;
        }
        XmlRegAtomType::XmlRegexpSymbol => {
            if type2 <= XmlRegAtomType::XmlRegexpSymbolOthers {
                return 1;
            }
            return 0;
        }
        XmlRegAtomType::XmlRegexpSymbolMath
        | XmlRegAtomType::XmlRegexpSymbolCurrency
        | XmlRegAtomType::XmlRegexpSymbolModifier
        | XmlRegAtomType::XmlRegexpSymbolOthers => {
            return 0;
        }
        XmlRegAtomType::XmlRegexpOther => {
            if type2 <= XmlRegAtomType::XmlRegexpOtherNa {
                return 1;
            }
            return 0;
        }
        XmlRegAtomType::XmlRegexpOtherControl
        | XmlRegAtomType::XmlRegexpOtherFormat
        | XmlRegAtomType::XmlRegexpOtherPrivate
        | XmlRegAtomType::XmlRegexpOtherNa => {
            return 0;
        }
        _ => {}
    }
    1
}

/// Checks if both strings are equal or have the same content. "*"
/// can be used as a wildcard in @valStr; "|" is used as a separator of
/// substrings in both @expStr and @valStr.
///
/// Returns 1 if the comparison is satisfied and the number of substrings is equal, 0 otherwise.
#[doc(alias = "xmlRegStrEqualWildcard")]
unsafe fn xml_reg_str_equal_wildcard(exp_str: Option<&str>, val_str: Option<&str>) -> i32 {
    if exp_str == val_str {
        return 1;
    }
    let (Some(exp_str), Some(val_str)) = (exp_str, val_str) else {
        return 0;
    };
    let mut exp_str = exp_str.chars().peekable();
    let mut val_str = val_str.chars().peekable();
    while val_str.peek().is_some() {
        // Eval if we have a wildcard for the current item.
        if exp_str.peek() != val_str.peek() {
            // if one of them starts with a wildcard make valStr be it
            if val_str.peek() == Some(&'*') {
                std::mem::swap(&mut val_str, &mut exp_str);
            }
            if val_str.peek().is_some() && Some('*') == exp_str.next() {
                while val_str
                    .next_if(|&v| v != XML_REG_STRING_SEPARATOR)
                    .is_some()
                {}
                continue;
            } else {
                return 0;
            }
        }
        exp_str.next();
        val_str.next();
    }
    exp_str.next().is_none() as i32
}

unsafe fn xml_fa_compare_ranges(mut range1: XmlRegRangePtr, mut range2: XmlRegRangePtr) -> i32 {
    let mut ret: i32;

    if matches!(
        (*range1).typ,
        XmlRegAtomType::XmlRegexpRanges
            | XmlRegAtomType::XmlRegexpSubreg
            | XmlRegAtomType::XmlRegexpString
    ) || matches!(
        (*range2).typ,
        XmlRegAtomType::XmlRegexpRanges
            | XmlRegAtomType::XmlRegexpSubreg
            | XmlRegAtomType::XmlRegexpString
    ) {
        return -1;
    }

    // put them in order
    if (*range1).typ > (*range2).typ {
        std::mem::swap(&mut range1, &mut range2);
    }
    if (*range1).typ == XmlRegAtomType::XmlRegexpAnychar
        || (*range2).typ == XmlRegAtomType::XmlRegexpAnychar
    {
        ret = 1;
    } else if (*range1).typ == XmlRegAtomType::XmlRegexpEpsilon
        || (*range2).typ == XmlRegAtomType::XmlRegexpEpsilon
    {
        return 0;
    } else if (*range1).typ == (*range2).typ {
        if (*range1).typ != XmlRegAtomType::XmlRegexpCharval {
            ret = 1;
        } else if (*range1).end < (*range2).start || (*range2).end < (*range1).start {
            ret = 0;
        } else {
            ret = 1;
        }
    } else if (*range1).typ == XmlRegAtomType::XmlRegexpCharval {
        let mut neg: i32 = 0;

        // just check all codepoints in the range for acceptance,
        // this is usually way cheaper since done only once at
        // compilation than testing over and over at runtime or
        // pushing too many states when evaluating.
        if ((*range1).neg == 0 && (*range2).neg != 0) || ((*range1).neg != 0 && (*range2).neg == 0)
        {
            neg = 1;
        }

        for codepoint in (*range1).start..=(*range1).end {
            ret = xml_reg_check_character_range(
                (*range2).typ,
                codepoint,
                0,
                (*range2).start,
                (*range2).end,
                (*range2).block_name.as_deref(),
            );
            if ret < 0 {
                return -1;
            }
            if (neg == 1 && ret == 0) || (neg == 0 && ret == 1) {
                return 1;
            }
        }
        return 0;
    } else if (*range1).typ == XmlRegAtomType::XmlRegexpBlockName
        || (*range2).typ == XmlRegAtomType::XmlRegexpBlockName
    {
        if (*range1).typ == (*range2).typ {
            ret = ((*range1).block_name == (*range2).block_name) as i32;
        } else {
            // comparing a block range with anything else is way
            // too costly, and maintaining the table is like too much
            // memory too, so let's force the automata to save state here.
            return 1;
        }
    } else if (*range1).typ < XmlRegAtomType::XmlRegexpLetter
        || (*range2).typ < XmlRegAtomType::XmlRegexpLetter
    {
        if ((*range1).typ == XmlRegAtomType::XmlRegexpAnyspace
            && (*range2).typ == XmlRegAtomType::XmlRegexpNotspace)
            || ((*range1).typ == XmlRegAtomType::XmlRegexpInitname
                && (*range2).typ == XmlRegAtomType::XmlRegexpNotinitname)
            || ((*range1).typ == XmlRegAtomType::XmlRegexpNamechar
                && (*range2).typ == XmlRegAtomType::XmlRegexpNotnamechar)
            || ((*range1).typ == XmlRegAtomType::XmlRegexpDecimal
                && (*range2).typ == XmlRegAtomType::XmlRegexpNotdecimal)
            || ((*range1).typ == XmlRegAtomType::XmlRegexpRealchar
                && (*range2).typ == XmlRegAtomType::XmlRegexpNotrealchar)
        {
            ret = 0;
        } else {
            // same thing to limit complexity
            return 1;
        }
    } else {
        ret = 0;
        // (*range1).typ < (*range2).typ here
        match (*range1).typ {
            XmlRegAtomType::XmlRegexpLetter => {
                // all disjoint except in the subgroups
                if matches!(
                    (*range2).typ,
                    XmlRegAtomType::XmlRegexpLetterUppercase
                        | XmlRegAtomType::XmlRegexpLetterLowercase
                        | XmlRegAtomType::XmlRegexpLetterTitlecase
                        | XmlRegAtomType::XmlRegexpLetterModifier
                        | XmlRegAtomType::XmlRegexpLetterOthers
                ) {
                    ret = 1;
                }
            }
            XmlRegAtomType::XmlRegexpMark => {
                if matches!(
                    (*range2).typ,
                    XmlRegAtomType::XmlRegexpMarkNonspacing
                        | XmlRegAtomType::XmlRegexpMarkSpacecombining
                        | XmlRegAtomType::XmlRegexpMarkEnclosing
                ) {
                    ret = 1;
                }
            }
            XmlRegAtomType::XmlRegexpNumber => {
                if matches!(
                    (*range2).typ,
                    XmlRegAtomType::XmlRegexpNumberDecimal
                        | XmlRegAtomType::XmlRegexpNumberLetter
                        | XmlRegAtomType::XmlRegexpNumberOthers
                ) {
                    ret = 1;
                }
            }
            XmlRegAtomType::XmlRegexpPunct => {
                if matches!(
                    (*range2).typ,
                    XmlRegAtomType::XmlRegexpPunctConnector
                        | XmlRegAtomType::XmlRegexpPunctDash
                        | XmlRegAtomType::XmlRegexpPunctOpen
                        | XmlRegAtomType::XmlRegexpPunctClose
                        | XmlRegAtomType::XmlRegexpPunctInitquote
                        | XmlRegAtomType::XmlRegexpPunctFinquote
                        | XmlRegAtomType::XmlRegexpPunctOthers
                ) {
                    ret = 1;
                }
            }
            XmlRegAtomType::XmlRegexpSepar => {
                if matches!(
                    (*range2).typ,
                    XmlRegAtomType::XmlRegexpSeparSpace
                        | XmlRegAtomType::XmlRegexpSeparLine
                        | XmlRegAtomType::XmlRegexpSeparPara
                ) {
                    ret = 1;
                }
            }
            XmlRegAtomType::XmlRegexpSymbol => {
                if matches!(
                    (*range2).typ,
                    XmlRegAtomType::XmlRegexpSymbolMath
                        | XmlRegAtomType::XmlRegexpSymbolCurrency
                        | XmlRegAtomType::XmlRegexpSymbolModifier
                        | XmlRegAtomType::XmlRegexpSymbolOthers
                ) {
                    ret = 1;
                }
            }
            XmlRegAtomType::XmlRegexpOther => {
                if matches!(
                    (*range2).typ,
                    XmlRegAtomType::XmlRegexpOtherControl
                        | XmlRegAtomType::XmlRegexpOtherFormat
                        | XmlRegAtomType::XmlRegexpOtherPrivate
                ) {
                    ret = 1;
                }
            }
            _ => {
                if (*range2).typ >= XmlRegAtomType::XmlRegexpLetter
                    && (*range2).typ < XmlRegAtomType::XmlRegexpBlockName
                {
                    ret = 0;
                } else {
                    // safety net !
                    return 1;
                }
            }
        }
    }
    if ((*range1).neg == 0 && (*range2).neg != 0) || ((*range1).neg != 0 && (*range2).neg == 0) {
        ret = (ret == 0) as i32;
    }
    ret
}

/// Compares two atoms to check whether they intersect in some ways,
/// this is used by xmlFAComputesDeterminism and xmlFARecurseDeterminism only
///
/// Returns 1 if yes and 0 otherwise
#[doc(alias = "xmlFACompareAtoms")]
unsafe fn xml_fa_compare_atoms(
    mut atom1: XmlRegAtomPtr,
    mut atom2: XmlRegAtomPtr,
    deep: i32,
) -> i32 {
    let mut ret: i32 = 1;

    if atom1 == atom2 {
        return 1;
    }
    if atom1.is_null() || atom2.is_null() {
        return 0;
    }

    if matches!((*atom1).typ, XmlRegAtomType::XmlRegexpAnychar)
        || matches!((*atom2).typ, XmlRegAtomType::XmlRegexpAnychar)
    {
        return 1;
    }

    if (*atom1).typ > (*atom2).typ {
        std::mem::swap(&mut atom1, &mut atom2);
    }
    if (*atom1).typ != (*atom2).typ {
        ret = xml_fa_compare_atom_types((*atom1).typ, (*atom2).typ);
        // if they can't intersect at the type level break now
        if ret == 0 {
            return 0;
        }
    }
    'done: {
        match (*atom1).typ {
            XmlRegAtomType::XmlRegexpString => {
                if deep == 0 {
                    ret = ((*atom1).valuep != (*atom2).valuep) as i32;
                } else {
                    let val1 = (*atom1).valuep.as_deref();
                    let val2 = (*atom2).valuep.as_deref();
                    let compound1 = val1.map_or(false, |v| v.contains('|'));
                    let compound2 = val2.map_or(false, |v| v.contains('|'));

                    // Ignore negative match flag for ##other namespaces
                    if compound1 != compound2 {
                        return 0;
                    }

                    ret = xml_reg_str_equal_wildcard(val1, val2);
                }
            }
            XmlRegAtomType::XmlRegexpEpsilon => {
                // goto not_determinist;
                return 1;
            }
            XmlRegAtomType::XmlRegexpCharval => {
                if matches!((*atom2).typ, XmlRegAtomType::XmlRegexpCharval) {
                    ret = ((*atom1).codepoint == (*atom2).codepoint) as i32;
                } else {
                    ret = xml_reg_check_character(atom2, (*atom1).codepoint);
                    if ret < 0 {
                        ret = 1;
                    }
                }
            }
            XmlRegAtomType::XmlRegexpRanges => {
                if matches!((*atom2).typ, XmlRegAtomType::XmlRegexpRanges) {
                    let mut res: i32;

                    // need to check that none of the ranges eventually matches
                    for &r1 in &(*atom1).ranges {
                        for &r2 in &(*atom2).ranges {
                            res = xml_fa_compare_ranges(r1, r2);
                            if res == 1 {
                                ret = 1;
                                break 'done;
                            }
                        }
                    }
                    ret = 0;
                }
            }
            _ => {
                // goto not_determinist;
                return 1;
            }
        }
    }
    // done:
    if (*atom1).neg != (*atom2).neg {
        ret = (ret == 0) as i32;
    }
    if ret == 0 {
        return 0;
    }
    // not_determinist:
    1
}

/// Check whether the associated regexp is determinist,
/// should be called after xmlFAEliminateEpsilonTransitions()
#[doc(alias = "xmlFARecurseDeterminism")]
unsafe fn xml_fa_recurse_determinism(
    ctxt: XmlRegParserCtxtPtr,
    state: XmlRegStatePtr,
    to: i32,
    atom: XmlRegAtomPtr,
) -> i32 {
    let mut ret: i32 = 1;
    let mut res: i32;
    let mut deep: i32 = 1;

    if state.is_null() {
        return ret;
    }
    if matches!((*state).markd, XmlRegMarkedType::XmlRegexpMarkVisited) {
        return ret;
    }

    if (*ctxt).flags & AM_AUTOMATA_RNG as i32 != 0 {
        deep = 0;
    }

    // don't recurse on transitions potentially added in the course of the elimination.
    for t1 in (*state).trans.iter_mut() {
        // check transitions conflicting with the one looked at
        if t1.atom.is_null() {
            if t1.to < 0 {
                continue;
            }
            (*state).markd = XmlRegMarkedType::XmlRegexpMarkVisited;
            res = xml_fa_recurse_determinism(ctxt, (*ctxt).states[t1.to as usize], to, atom);
            if res == 0 {
                ret = 0;
                // (*t1).nd = 1;
            }
            continue;
        }
        if t1.to != to {
            continue;
        }
        if xml_fa_compare_atoms(t1.atom, atom, deep) != 0 {
            ret = 0;
            // mark the transition as non-deterministic
            t1.nd = 1;
        }
    }
    ret
}

/// Reset flags after checking determinism.
#[doc(alias = "xmlFAFinishRecurseDeterminism")]
unsafe fn xml_fa_finish_recurse_determinism(ctxt: XmlRegParserCtxtPtr, state: XmlRegStatePtr) {
    if state.is_null() {
        return;
    }
    if !matches!((*state).markd, XmlRegMarkedType::XmlRegexpMarkVisited) {
        return;
    }
    (*state).markd = XmlRegMarkedType::XmlRegexpMarkNormal;

    for t1 in &(*state).trans {
        if t1.atom.is_null() && t1.to >= 0 {
            xml_fa_finish_recurse_determinism(ctxt, (*ctxt).states[t1.to as usize]);
        }
    }
}

/// Check whether the associated regexp is determinist,
/// should be called after xmlFAEliminateEpsilonTransitions()
#[doc(alias = "xmlFAComputesDeterminism")]
pub(crate) unsafe fn xml_fa_computes_determinism(ctxt: XmlRegParserCtxtPtr) -> i32 {
    let mut ret: i32 = 1;
    let mut deep: i32 = 1;

    if (*ctxt).determinist != -1 {
        return (*ctxt).determinist;
    }

    if (*ctxt).flags & AM_AUTOMATA_RNG as i32 != 0 {
        deep = 0;
    }

    // First cleanup the automata removing cancelled transitions
    for &state in &(*ctxt).states {
        if state.is_null() {
            continue;
        }
        if (*state).trans.len() < 2 {
            continue;
        }
        for transnr in 0..(*state).trans.len() {
            let (trans, rem) = (*state).trans.split_at_mut(transnr);
            let t1 = &rem[0];
            // Determinism checks in case of counted or all transitions
            // will have to be handled separately
            if t1.atom.is_null() {
                /* (*t1).nd = 1; */
                continue;
            }
            if t1.to == -1 {
                // eliminated
                continue;
            }
            for t2 in trans {
                if t2.to == -1 {
                    // eliminated
                    continue;
                }
                if !t2.atom.is_null() && t1.to == t2.to {
                    // Here we use deep because we want to keep the
                    // transitions which indicate a conflict
                    if xml_fa_equal_atoms(t1.atom, t2.atom, deep) != 0
                        && t1.counter == t2.counter
                        && t1.count == t2.count
                    {
                        t2.to = -1; /* eliminated */
                    }
                }
            }
        }
    }

    // Check for all states that there aren't 2 transitions
    // with the same atom and a different target.
    for &state in &(*ctxt).states {
        if state.is_null() {
            continue;
        }
        if (*state).trans.len() < 2 {
            continue;
        }
        let mut last = None::<usize>;
        for transnr in 0..(*state).trans.len() {
            let (trans, rem) = (*state).trans.split_at_mut(transnr);
            let t1 = &mut rem[0];
            // Determinism checks in case of counted or all transitions
            // will have to be handled separately
            if t1.atom.is_null() {
                continue;
            }
            if t1.to == -1 {
                // eliminated
                continue;
            }
            for t2 in trans {
                if t2.to == -1 {
                    // eliminated
                    continue;
                }
                if !t2.atom.is_null() {
                    // But here we don't use deep because we want to
                    // find transitions which indicate a conflict
                    if xml_fa_compare_atoms(t1.atom, t2.atom, 1) != 0 {
                        ret = 0;
                        // mark the transitions as non-deterministic ones
                        t1.nd = 1;
                        t2.nd = 1;
                        last = Some(transnr);
                    }
                } else if t1.to != -1 {
                    // do the closure in case of remaining specific
                    // epsilon transitions like choices or all
                    ret = xml_fa_recurse_determinism(
                        ctxt,
                        (*ctxt).states[t1.to as usize],
                        t2.to,
                        t2.atom,
                    );
                    xml_fa_finish_recurse_determinism(ctxt, (*ctxt).states[t1.to as usize]);
                    // don't shortcut the computation so all non deterministic
                    // transition get marked down
                    // if (ret == 0)
                    // return(0);
                    if ret == 0 {
                        t1.nd = 1;
                        // (*t2).nd = 1;
                        last = Some(transnr);
                    }
                }
            }
            // don't shortcut the computation so all non deterministic
            // transition get marked down
            // if (ret == 0)
            // break;
        }
        // mark specifically the last non-deterministic transition
        // from a state since there is no need to set-up rollback from it
        if let Some(last) = last {
            (*state).trans[last].nd = 2;
        }

        // don't shortcut the computation so all non deterministic
        // transition get marked down
        // if (ret == 0)
        //     break;
    }

    (*ctxt).determinist = ret;
    ret
}

/// Check if the regular expression is determinist
///
/// Returns 1 if it yes, 0 if not and a negative value in case of error
#[doc(alias = "xmlRegexpIsDeterminist")]
pub unsafe fn xml_regexp_is_determinist(comp: XmlRegexpPtr) -> i32 {
    if comp.is_null() {
        return -1;
    }
    if (*comp).determinist != -1 {
        return (*comp).determinist;
    }

    let am: XmlAutomataPtr = xml_new_automata();
    if am.is_null() {
        return -1;
    }
    for state in (*am).states.drain(..) {
        xml_reg_free_state(state);
    }
    (*am).atoms = take(&mut (*comp).atoms);
    (*am).states = take(&mut (*comp).states);
    (*am).determinist = -1;
    (*am).flags = (*comp).flags;
    let ret: i32 = xml_fa_computes_determinism(am);
    (*comp).atoms = take(&mut (*am).atoms);
    (*comp).states = take(&mut (*am).states);
    xml_free_automata(am);
    (*comp).determinist = ret;
    ret
}

/// Callback function when doing a transition in the automata
#[doc(alias = "xmlRegExecCallbacks")]
pub type XmlRegExecCallbacks =
    unsafe fn(exec: XmlRegExecCtxtPtr, token: &str, transdata: *mut c_void, inputdata: *mut c_void);

/// Build a context used for progressive evaluation of a regexp.
///
/// Returns the new context
#[doc(alias = "xmlRegNewExecCtxt")]
pub unsafe fn xml_reg_new_exec_ctxt(
    comp: XmlRegexpPtr,
    callback: Option<XmlRegExecCallbacks>,
    data: *mut c_void,
) -> XmlRegExecCtxtPtr {
    if comp.is_null() {
        return null_mut();
    }
    if (*comp).compact.is_null() && (*comp).states.is_empty() {
        return null_mut();
    }
    let exec: XmlRegExecCtxtPtr = xml_malloc(size_of::<XmlRegExecCtxt>()) as XmlRegExecCtxtPtr;
    if exec.is_null() {
        xml_regexp_err_memory(null_mut(), "creating execution context");
        return null_mut();
    }
    std::ptr::write(&mut *exec, XmlRegExecCtxt::default());
    (*exec).input_string = null_mut();
    (*exec).index = 0;
    (*exec).determinist = 1;
    (*exec).rollbacks.clear();
    (*exec).status = 0;
    (*exec).comp = comp;
    if (*comp).compact.is_null() {
        (*exec).state = *(*comp).states.first().unwrap_or(&null_mut());
    }
    (*exec).transno = 0;
    (*exec).transcount = 0;
    (*exec).callback = callback;
    (*exec).data = data;
    if !(*comp).counters.is_empty() {
        (*exec).counts.clear();
        (*exec).counts.resize((*comp).counters.len(), 0);
        (*exec).err_counts.clear();
        (*exec).err_counts.resize((*comp).counters.len(), 0);
    } else {
        (*exec).counts.clear();
        (*exec).err_counts.clear();
    }
    (*exec).input_stack_max = 0;
    (*exec).input_stack_nr = 0;
    (*exec).input_stack = null_mut();
    (*exec).err_state_no = -1;
    (*exec).err_string = null_mut();
    (*exec).nb_push = 0;
    exec
}

/// Free the structures associated to a regular expression evaluation context.
#[doc(alias = "xmlRegFreeExecCtxt")]
pub unsafe fn xml_reg_free_exec_ctxt(exec: XmlRegExecCtxtPtr) {
    if exec.is_null() {
        return;
    }

    (*exec).counts.clear();
    (*exec).rollbacks.clear();
    if !(*exec).input_stack.is_null() {
        for i in 0..(*exec).input_stack_nr {
            if !(*(*exec).input_stack.add(i as usize)).value.is_null() {
                xml_free((*(*exec).input_stack.add(i as usize)).value as _);
            }
        }
        xml_free((*exec).input_stack as _);
    }
    if !(*exec).err_string.is_null() {
        xml_free((*exec).err_string as _);
    }
    drop_in_place(exec);
    xml_free(exec as _);
}

pub(crate) const REGEXP_ALL_LAX_COUNTER: usize = 0x123457;

/// Push one input token in the execution context
///
/// Returns: 1 if the regexp reached a final state, 0 if non-final,
/// and a negative value in case of error.
#[doc(alias = "xmlRegCompactPushString")]
unsafe fn xml_reg_compact_push_string(
    exec: XmlRegExecCtxtPtr,
    comp: XmlRegexpPtr,
    value: *const XmlChar,
    data: *mut c_void,
) -> i32 {
    let state: i32 = (*exec).index;
    let mut target: i32;

    if comp.is_null() || (*comp).compact.is_null() {
        return -1;
    }

    if value.is_null() {
        // are we at a final state ?
        if *(*comp)
            .compact
            .add(state as usize * ((*comp).string_map.len() + 1))
            == XmlRegStateType::XmlRegexpFinalState as i32
        {
            return 1;
        }
        return 0;
    }

    // Examine all outside transitions from current state
    for i in 0..(*comp).string_map.len() {
        target = *(*comp)
            .compact
            .add(state as usize * ((*comp).string_map.len() + 1) + i + 1);
        if target > 0 && target <= (*comp).nbstates {
            target -= 1; /* to avoid 0 */
            if xml_reg_str_equal_wildcard(
                Some(&(*comp).string_map[i]),
                Some(
                    CStr::from_ptr(value as *const i8)
                        .to_string_lossy()
                        .as_ref(),
                ),
            ) != 0
            {
                (*exec).index = target;
                if let Some(callback) = (*exec).callback {
                    if !(*comp).transdata.is_null() {
                        callback(
                            (*exec).data as _,
                            CStr::from_ptr(value as *const i8)
                                .to_string_lossy()
                                .as_ref(),
                            *(*comp)
                                .transdata
                                .add(state as usize * (*comp).string_map.len() + i),
                            data,
                        );
                    }
                }
                if *(*comp)
                    .compact
                    .add(target as usize * ((*comp).string_map.len() + 1))
                    == XmlRegStateType::XmlRegexpSinkState as i32
                {
                    // goto error;
                    break;
                }

                if *(*comp)
                    .compact
                    .add(target as usize * ((*comp).string_map.len() + 1))
                    == XmlRegStateType::XmlRegexpFinalState as i32
                {
                    return 1;
                }
                return 0;
            }
        }
    }
    // Failed to find an exit transition out from current state for the current token
    // error:
    if !(*exec).err_string.is_null() {
        xml_free((*exec).err_string as _);
    }
    (*exec).err_string = xml_strdup(value);
    (*exec).err_state_no = state;
    (*exec).status = -1;
    -1
}

unsafe fn xml_fareg_exec_save_input_string(
    exec: XmlRegExecCtxtPtr,
    value: *const XmlChar,
    data: *mut c_void,
) {
    if (*exec).input_stack_max == 0 {
        (*exec).input_stack_max = 4;
        (*exec).input_stack =
            xml_malloc((*exec).input_stack_max as usize * size_of::<XmlRegInputToken>())
                as XmlRegInputTokenPtr;
        if (*exec).input_stack.is_null() {
            xml_regexp_err_memory(null_mut(), "pushing input string");
            (*exec).input_stack_max = 0;
            return;
        }
    } else if (*exec).input_stack_nr + 1 >= (*exec).input_stack_max {
        (*exec).input_stack_max *= 2;
        let tmp: XmlRegInputTokenPtr = xml_realloc(
            (*exec).input_stack as _,
            (*exec).input_stack_max as usize * size_of::<XmlRegInputToken>(),
        ) as XmlRegInputTokenPtr;
        if tmp.is_null() {
            xml_regexp_err_memory(null_mut(), "pushing input string");
            (*exec).input_stack_max /= 2;
            return;
        }
        (*exec).input_stack = tmp;
    }
    (*(*exec).input_stack.add((*exec).input_stack_nr as usize)).value = xml_strdup(value);
    (*(*exec).input_stack.add((*exec).input_stack_nr as usize)).data = data;
    (*exec).input_stack_nr += 1;
    (*(*exec).input_stack.add((*exec).input_stack_nr as usize)).value = null_mut();
    (*(*exec).input_stack.add((*exec).input_stack_nr as usize)).data = null_mut();
}

/// Push one input token in the execution context
///
/// Returns: 1 if the regexp reached a final state, 0 if non-final,
/// and a negative value in case of error.
#[doc(alias = "xmlRegExecPushStringInternal")]
unsafe fn xml_reg_exec_push_string_internal(
    exec: XmlRegExecCtxtPtr,
    mut value: *const XmlChar,
    mut data: *mut c_void,
    compound: i32,
) -> i32 {
    let mut atom: XmlRegAtomPtr;
    let mut ret: i32;
    let mut is_final: i32 = 0;
    let mut progress: i32 = 1;

    if exec.is_null() {
        return -1;
    }
    if (*exec).comp.is_null() {
        return -1;
    }
    if (*exec).status != 0 {
        return (*exec).status;
    }

    if !(*(*exec).comp).compact.is_null() {
        return xml_reg_compact_push_string(exec, (*exec).comp, value, data);
    }

    if value.is_null() {
        if matches!((*(*exec).state).typ, XmlRegStateType::XmlRegexpFinalState) {
            return 1;
        }
        is_final = 1;
    }

    // If we have an active rollback stack push the new value there
    // and get back to where we were left
    if !value.is_null() && (*exec).input_stack_nr > 0 {
        xml_fareg_exec_save_input_string(exec, value, data);
        value = (*(*exec).input_stack.add((*exec).index as usize)).value;
        data = (*(*exec).input_stack.add((*exec).index as usize)).data;
    }

    'b: while (*exec).status == 0
        && (!value.is_null()
            || (is_final == 1
                && !matches!((*(*exec).state).typ, XmlRegStateType::XmlRegexpFinalState)))
    {
        'rollback: {
            'progress: {
                // End of input on non-terminal state, rollback, however we may
                // still have epsilon like transition for counted transitions
                // on counters, in that case don't break too early.
                if value.is_null() && (*exec).counts.is_empty() {
                    break 'rollback;
                }

                (*exec).transcount = 0;
                (*exec).transno -= 1;
                while {
                    (*exec).transno += 1;
                    (*exec).transno < (*(*exec).state).trans.len() as i32
                } {
                    let trans = &(*(*exec).state).trans[(*exec).transno as usize];
                    if trans.to < 0 {
                        continue;
                    }
                    atom = trans.atom;
                    ret = 0;
                    if trans.count as usize == REGEXP_ALL_LAX_COUNTER {
                        let mut count: i32;

                        ret = 0;

                        // Check all counted transitions from the current state
                        if value.is_null() && is_final != 0 {
                            ret = 1;
                        } else if !value.is_null() {
                            for (i, t) in (*(*exec).state).trans.iter().enumerate() {
                                if t.counter < 0 || i == (*exec).transno as usize {
                                    continue;
                                }
                                let counter = (*(*exec).comp).counters[t.counter as usize];
                                count = (*exec).counts[t.counter as usize];
                                if count < counter.max
                                    && !t.atom.is_null()
                                    && Some(
                                        CStr::from_ptr(value as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                    ) == (*t.atom).valuep.as_deref()
                                {
                                    ret = 0;
                                    break;
                                }
                                if count >= counter.min
                                    && count < counter.max
                                    && !t.atom.is_null()
                                    && Some(
                                        CStr::from_ptr(value as *const i8)
                                            .to_string_lossy()
                                            .as_ref(),
                                    ) == (*t.atom).valuep.as_deref()
                                {
                                    ret = 1;
                                    break;
                                }
                            }
                        }
                    } else if trans.count as usize == REGEXP_ALL_COUNTER {
                        let mut count: i32;

                        ret = 1;

                        // Check all counted transitions from the current state
                        for (i, t) in (*(*exec).state).trans.iter().enumerate() {
                            if t.counter < 0 || i == (*exec).transno as usize {
                                continue;
                            }
                            let counter = (*(*exec).comp).counters[t.counter as usize];
                            count = (*exec).counts[t.counter as usize];
                            if count < counter.min || count > counter.max {
                                ret = 0;
                                break;
                            }
                        }
                    } else if trans.count >= 0 {
                        // A counted transition.
                        let count = (*exec).counts[trans.count as usize];
                        let counter = (*(*exec).comp).counters[trans.count as usize];
                        ret = (count >= counter.min && count <= counter.max) as _;
                    } else if atom.is_null() {
                        eprintln!("epsilon transition left at runtime");
                        (*exec).status = -2;
                        break;
                    } else if !value.is_null() {
                        ret = xml_reg_str_equal_wildcard(
                            (*atom).valuep.as_deref(),
                            Some(
                                CStr::from_ptr(value as *const i8)
                                    .to_string_lossy()
                                    .as_ref(),
                            ),
                        );
                        if (*atom).neg != 0 {
                            ret = (ret == 0) as i32;
                            if compound == 0 {
                                ret = 0;
                            }
                        }
                        if ret == 1 && trans.counter >= 0 {
                            let count = (*exec).counts[trans.counter as usize];
                            let counter = (*(*exec).comp).counters[trans.counter as usize];
                            if count >= counter.max {
                                ret = 0;
                            }
                        }

                        if ret == 1 && (*atom).min > 0 && (*atom).max > 0 {
                            let to: XmlRegStatePtr = (*(*exec).comp).states[trans.to as usize];

                            // this is a multiple input sequence
                            if (*(*exec).state).trans.len() as i32 > (*exec).transno + 1 {
                                if (*exec).input_stack_nr <= 0 {
                                    xml_fareg_exec_save_input_string(exec, value, data);
                                }
                                xml_fa_reg_exec_save(exec);
                            }
                            (*exec).transcount = 1;
                            'inner: while {
                                // Try to progress as much as possible on the input
                                if (*exec).transcount == (*atom).max {
                                    break 'inner;
                                }
                                (*exec).index += 1;
                                value = (*(*exec).input_stack.add((*exec).index as usize)).value;
                                data = (*(*exec).input_stack.add((*exec).index as usize)).data;

                                // End of input: stop here
                                if value.is_null() {
                                    (*exec).index -= 1;
                                    break 'inner;
                                }
                                if (*exec).transcount >= (*atom).min {
                                    let transno: i32 = (*exec).transno;
                                    let state: XmlRegStatePtr = (*exec).state;

                                    // The transition is acceptable save it
                                    (*exec).transno = -1; /* trick */
                                    (*exec).state = to;
                                    if (*exec).input_stack_nr <= 0 {
                                        xml_fareg_exec_save_input_string(exec, value, data);
                                    }
                                    xml_fa_reg_exec_save(exec);
                                    (*exec).transno = transno;
                                    (*exec).state = state;
                                }
                                ret = (Some(
                                    CStr::from_ptr(value as *const i8)
                                        .to_string_lossy()
                                        .as_ref(),
                                ) == (*atom).valuep.as_deref())
                                    as i32;
                                (*exec).transcount += 1;
                                ret == 1
                            } {}
                            if (*exec).transcount < (*atom).min {
                                ret = 0;
                            }

                            // If the last check failed but one transition was found
                            // possible, rollback
                            if ret < 0 {
                                ret = 0;
                            }
                            if ret == 0 {
                                break 'rollback;
                            }
                        }
                    }
                    if ret == 1 {
                        if let Some(callback) = (*exec).callback {
                            if !atom.is_null() && !data.is_null() {
                                callback(
                                    (*exec).data as _,
                                    (*atom).valuep.as_deref().unwrap(),
                                    (*atom).data as _,
                                    data,
                                );
                            }
                        }
                        if (*(*exec).state).trans.len() as i32 > (*exec).transno + 1 {
                            if (*exec).input_stack_nr <= 0 {
                                xml_fareg_exec_save_input_string(exec, value, data);
                            }
                            xml_fa_reg_exec_save(exec);
                        }
                        if trans.counter >= 0 {
                            (*exec).counts[trans.counter as usize] += 1;
                        }
                        if trans.count >= 0 && (trans.count as usize) < REGEXP_ALL_COUNTER {
                            (*exec).counts[trans.count as usize] = 0;
                        }
                        if !(*(*exec).comp).states[trans.to as usize].is_null()
                            && matches!(
                                (*((*(*exec).comp).states[trans.to as usize])).typ,
                                XmlRegStateType::XmlRegexpSinkState
                            )
                        {
                            // entering a sink state, save the current state as error state.
                            if !(*exec).err_string.is_null() {
                                xml_free((*exec).err_string as _);
                            }
                            (*exec).err_string = xml_strdup(value);
                            (*exec).err_state = (*exec).state;
                            (*exec).err_counts.copy_from_slice(&(*exec).counts);
                        }
                        (*exec).state = (*(*exec).comp).states[trans.to as usize];
                        (*exec).transno = 0;
                        if !trans.atom.is_null() {
                            if !(*exec).input_stack.is_null() {
                                (*exec).index += 1;
                                if (*exec).index < (*exec).input_stack_nr {
                                    value =
                                        (*(*exec).input_stack.add((*exec).index as usize)).value;
                                    data = (*(*exec).input_stack.add((*exec).index as usize)).data;
                                } else {
                                    value = null_mut();
                                    data = null_mut();
                                }
                            } else {
                                value = null_mut();
                                data = null_mut();
                            }
                        }
                        break 'progress;
                    } else if ret < 0 {
                        (*exec).status = -4;
                        break;
                    }
                }
                if (*exec).transno != 0 || (*(*exec).state).trans.is_empty() {
                    break 'rollback;
                }
                continue 'b;
            }
            // progress:
            progress = 1;
            continue 'b;
        }
        // if we didn't yet rollback on the current input
        // store the current state as the error state.
        if progress != 0
            && !(*exec).state.is_null()
            && !matches!((*(*exec).state).typ, XmlRegStateType::XmlRegexpSinkState)
        {
            progress = 0;
            if !(*exec).err_string.is_null() {
                xml_free((*exec).err_string as _);
            }
            (*exec).err_string = xml_strdup(value);
            (*exec).err_state = (*exec).state;
            if !(*(*exec).comp).counters.is_empty() {
                (*exec).err_counts.copy_from_slice(&(*exec).counts);
            }
        }

        // Failed to find a way out
        (*exec).determinist = 0;
        xml_fa_reg_exec_rollback(exec);
        if !(*exec).input_stack.is_null() && (*exec).status == 0 {
            value = (*(*exec).input_stack.add((*exec).index as usize)).value;
            data = (*(*exec).input_stack.add((*exec).index as usize)).data;
        }
    }
    if (*exec).status == 0 {
        return matches!((*(*exec).state).typ, XmlRegStateType::XmlRegexpFinalState) as _;
    }
    (*exec).status
}

/// Push one input token in the execution context
///
/// Returns: 1 if the regexp reached a final state, 0 if non-final,
/// and a negative value in case of error.
#[doc(alias = "xmlRegExecPushString")]
pub unsafe fn xml_reg_exec_push_string(
    exec: XmlRegExecCtxtPtr,
    value: *const XmlChar,
    data: *mut c_void,
) -> i32 {
    xml_reg_exec_push_string_internal(exec, value, data, 0)
}

const XML_REG_STRING_SEPARATOR: char = '|';

/// Push one input token in the execution context
///
/// Returns: 1 if the regexp reached a final state, 0 if non-final,
/// and a negative value in case of error.
#[doc(alias = "xmlRegExecPushString2")]
pub unsafe fn xml_reg_exec_push_string2(
    exec: XmlRegExecCtxtPtr,
    value: *const XmlChar,
    value2: *const XmlChar,
    data: *mut c_void,
) -> i32 {
    let mut buf: [XmlChar; 150] = [0; 150];
    let str: *mut XmlChar;

    if exec.is_null() {
        return -1;
    }
    if (*exec).comp.is_null() {
        return -1;
    }
    if (*exec).status != 0 {
        return (*exec).status;
    }

    if value2.is_null() {
        return xml_reg_exec_push_string(exec, value, data);
    }

    let lenn: i32 = strlen(value2 as _) as _;
    let lenp: i32 = strlen(value as _) as _;

    if 150 < lenn + lenp + 2 {
        str = xml_malloc_atomic(lenn as usize + lenp as usize + 2) as *mut XmlChar;
        if str.is_null() {
            (*exec).status = -1;
            return -1;
        }
    } else {
        str = buf.as_mut_ptr();
    }
    memcpy(str as _, value as _, lenp as usize);
    *str.add(lenp as usize) = XML_REG_STRING_SEPARATOR as _;
    memcpy(str.add(lenp as usize + 1) as _, value2 as _, lenn as usize);
    *str.add(lenn as usize + lenp as usize + 1) = 0;

    let ret = if !(*(*exec).comp).compact.is_null() {
        xml_reg_compact_push_string(exec, (*exec).comp, str, data)
    } else {
        xml_reg_exec_push_string_internal(exec, str, data, 1)
    };

    if str != buf.as_mut_ptr() {
        xml_free(str as _);
    }
    ret
}

/// Extract information from the regexp execution, internal routine to
/// implement xmlRegExecNextValues() and xmlRegExecErrInfo()
///
/// If successfully collected, return `Some((num_val, num_neg, values_slice))`.  
/// Otherwise, return `None`.
#[doc(alias = "xmlRegExecGetValues")]
unsafe fn xml_reg_exec_get_values<'a>(
    exec: XmlRegExecCtxtPtr,
    err: i32,
    values: &'a mut [Cow<'static, str>],
    terminal: *mut i32,
) -> Option<(usize, usize, &'a [Cow<'static, str>])> {
    if exec.is_null() || values.is_empty() {
        return None;
    }

    let maxval = values.len();
    let mut nb = 0;
    let mut nbval = 0;
    let mut nbneg = 0;
    if !(*exec).comp.is_null() && !(*(*exec).comp).compact.is_null() {
        let mut target: i32;
        let comp: XmlRegexpPtr = (*exec).comp;

        let state = if err != 0 {
            if (*exec).err_state_no == -1 {
                return None;
            }
            (*exec).err_state_no
        } else {
            (*exec).index
        };
        if !terminal.is_null() {
            if *(*comp)
                .compact
                .add(state as usize * ((*comp).string_map.len() + 1))
                == XmlRegStateType::XmlRegexpFinalState as i32
            {
                *terminal = 1;
            } else {
                *terminal = 0;
            }
        }
        if nb < maxval {
            for i in 0..(*comp).string_map.len() {
                target = *(*comp)
                    .compact
                    .add(state as usize * ((*comp).string_map.len() + 1) + i + 1);
                if target > 0
                    && target <= (*comp).nbstates
                    && *(*comp)
                        .compact
                        .add((target - 1) as usize * ((*comp).string_map.len() + 1))
                        != XmlRegStateType::XmlRegexpSinkState as i32
                {
                    values[nb] = Cow::Owned((*comp).string_map[i].clone());
                    nb += 1;
                    nbval += 1;
                }
                if nb >= maxval {
                    break;
                }
            }
        }
        if nb < maxval {
            for i in 0..(*comp).string_map.len() {
                target = *(*comp)
                    .compact
                    .add(state as usize * ((*comp).string_map.len() + 1) + i + 1);
                if target > 0
                    && target <= (*comp).nbstates
                    && *(*comp)
                        .compact
                        .add((target - 1) as usize * ((*comp).string_map.len() + 1))
                        == XmlRegStateType::XmlRegexpSinkState as i32
                {
                    values[nb] = Cow::Owned((*comp).string_map[i].clone());
                    nb += 1;
                    nbneg += 1;
                }
                if nb >= maxval {
                    break;
                }
            }
        }
    } else {
        let mut atom: XmlRegAtomPtr;

        if !terminal.is_null() {
            if matches!((*(*exec).state).typ, XmlRegStateType::XmlRegexpFinalState) {
                *terminal = 1;
            } else {
                *terminal = 0;
            }
        }

        let state = if err != 0 {
            if (*exec).err_state.is_null() {
                return None;
            }
            (*exec).err_state
        } else {
            if (*exec).state.is_null() {
                return None;
            }
            (*exec).state
        };
        if nb < maxval {
            for trans in &(*state).trans {
                if trans.to < 0 {
                    continue;
                }
                atom = trans.atom;
                if atom.is_null() || (*atom).valuep.is_none() {
                    continue;
                }
                if trans.count as usize == REGEXP_ALL_LAX_COUNTER {
                    // this should not be reached but ...
                    todo!()
                } else if trans.count as usize == REGEXP_ALL_COUNTER {
                    // this should not be reached but ...
                    todo!()
                } else if trans.counter >= 0 {
                    let mut counter = None;

                    let count = if err != 0 {
                        (*exec).err_counts[trans.counter as usize]
                    } else {
                        (*exec).counts[trans.counter as usize]
                    };
                    if !(*exec).comp.is_null() {
                        counter = Some((*(*exec).comp).counters[trans.counter as usize]);
                    }
                    if counter.map_or(true, |c| count < c.max) {
                        if (*atom).neg != 0 {
                            values[nb] = Cow::Owned((*atom).valuep2.as_deref().unwrap().to_owned());
                            nb += 1;
                        } else {
                            values[nb] = Cow::Owned((*atom).valuep.as_deref().unwrap().to_owned());
                            nb += 1;
                        }
                        nbval += 1;
                    }
                } else if !(*exec).comp.is_null()
                    && !((*(*exec).comp).states[trans.to as usize]).is_null()
                    && !matches!(
                        (*((*(*exec).comp).states[trans.to as usize])).typ,
                        XmlRegStateType::XmlRegexpSinkState,
                    )
                {
                    if (*atom).neg != 0 {
                        values[nb] = Cow::Owned((*atom).valuep2.as_deref().unwrap().to_owned());
                        nb += 1;
                    } else {
                        values[nb] = Cow::Owned((*atom).valuep.as_deref().unwrap().to_owned());
                        nb += 1;
                    }
                    nbval += 1;
                }
                if nb >= maxval {
                    break;
                }
            }
        }
        if nb < maxval {
            for trans in &(*state).trans {
                if trans.to < 0 {
                    continue;
                }
                atom = trans.atom;
                if atom.is_null() || (*atom).valuep.is_none() {
                    continue;
                }
                if trans.count as usize == REGEXP_ALL_LAX_COUNTER
                    || trans.count as usize == REGEXP_ALL_COUNTER
                    || trans.counter >= 0
                {
                    continue;
                } else if !((*(*exec).comp).states[trans.to as usize]).is_null()
                    && matches!(
                        (*((*(*exec).comp).states[trans.to as usize])).typ,
                        XmlRegStateType::XmlRegexpSinkState
                    )
                {
                    if (*atom).neg != 0 {
                        values[nb] = Cow::Owned((*atom).valuep2.as_deref().unwrap().to_owned());
                        nb += 1
                    } else {
                        values[nb] = Cow::Owned((*atom).valuep.as_deref().unwrap().to_owned());
                        nb += 1;
                    }
                    nbneg += 1;
                }
                if nb >= maxval {
                    break;
                }
            }
        }
    }
    Some((nbval, nbneg, &values[..nb]))
}

/// Extract information from the regexp execution,
/// the parameter @values must point to an array of @nbval string pointers
/// on return nbval will contain the number of possible strings in that
/// state and the @values array will be updated with them. The string values
/// returned will be freed with the @exec context and don't need to be deallocated.
///
/// If successfully collected, return `Some((num_val, num_neg, values_slice))`.  
/// Otherwise, return `None`.
#[doc(alias = "xmlRegExecNextValues")]
pub unsafe fn xml_reg_exec_next_values<'a>(
    exec: XmlRegExecCtxtPtr,
    values: &'a mut [Cow<'static, str>],
    terminal: *mut i32,
) -> Option<(usize, usize, &'a [Cow<'static, str>])> {
    xml_reg_exec_get_values(exec, 0, values, terminal)
}

/// Extract error information from the regexp execution, the parameter
/// @string will be updated with the value pushed and not accepted,
/// the parameter @values must point to an array of @nbval string pointers
/// on return nbval will contain the number of possible strings in that
/// state and the @values array will be updated with them. The string values
/// returned will be freed with the @exec context and don't need to be deallocated.
///
/// If successfully collected, return `Some((num_val, num_neg, values_slice))`.  
/// Otherwise, return `None`.
#[doc(alias = "xmlRegExecErrInfo")]
pub unsafe fn xml_reg_exec_err_info<'a>(
    exec: XmlRegExecCtxtPtr,
    string: *mut *const XmlChar,
    values: &'a mut [Cow<'static, str>],
    terminal: *mut i32,
) -> Option<(usize, usize, &'a [Cow<'static, str>])> {
    if exec.is_null() {
        return None;
    }
    if !string.is_null() {
        if (*exec).status != 0 {
            *string = (*exec).err_string;
        } else {
            *string = null_mut();
        }
    }
    xml_reg_exec_get_values(exec, 1, values, terminal)
}

// expressions are used within a context
#[cfg(feature = "libxml_expr")]
pub type XmlExpCtxtPtr = *mut XmlExpCtxt;
#[cfg(feature = "libxml_expr")]
#[repr(C)]
pub struct XmlExpCtxt {
    dict: XmlDictPtr,
    table: *mut XmlExpNodePtr,
    size: i32,
    nb_elems: i32,
    nb_nodes: i32,
    max_nodes: i32,
    expr: *const c_char,
    cur: *const c_char,
    nb_cons: i32,
    tab_size: i32,
}

/// Free an expression context
#[doc(alias = "xmlExpFreeCtxt")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_free_ctxt(ctxt: XmlExpCtxtPtr) {
    use super::dict::xml_dict_free;

    if ctxt.is_null() {
        return;
    }
    xml_dict_free((*ctxt).dict);
    if !(*ctxt).table.is_null() {
        xml_free((*ctxt).table as _);
    }
    xml_free(ctxt as _);
}

/// Creates a new context for manipulating expressions
///
/// Returns the context or NULL in case of error
#[doc(alias = "xmlExpNewCtxt")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_new_ctxt(mut max_nodes: i32, dict: XmlDictPtr) -> XmlExpCtxtPtr {
    use crate::libxml::dict::{xml_dict_create, xml_dict_reference};

    let size: i32 = 256;

    if max_nodes <= 4096 {
        max_nodes = 4096;
    }

    let ret: XmlExpCtxtPtr = xml_malloc(size_of::<XmlExpCtxt>()) as XmlExpCtxtPtr;
    if ret.is_null() {
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlExpCtxt>());
    (*ret).size = size;
    (*ret).nb_elems = 0;
    (*ret).max_nodes = max_nodes;
    (*ret).table = xml_malloc(size as usize * size_of::<XmlExpNodePtr>()) as _;
    if (*ret).table.is_null() {
        xml_free(ret as _);
        return null_mut();
    }
    memset(
        (*ret).table as _,
        0,
        size as usize * size_of::<XmlExpNodePtr>(),
    );
    if dict.is_null() {
        (*ret).dict = xml_dict_create();
        if (*ret).dict.is_null() {
            xml_free((*ret).table as _);
            xml_free(ret as _);
            return null_mut();
        }
    } else {
        (*ret).dict = dict;
        xml_dict_reference((*ret).dict);
    }
    ret
}

/// Debugging facility provides the number of allocated nodes at a that point
///
/// Returns the number of nodes in use or -1 in case of error
#[doc(alias = "xmlExpCtxtNbNodes")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_ctxt_nb_nodes(ctxt: XmlExpCtxtPtr) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).nb_nodes
}

/// Debugging facility provides the number of allocated nodes over lifetime
///
/// Returns the number of nodes ever allocated or -1 in case of error
#[doc(alias = "xmlExpCtxtNbCons")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_ctxt_nb_cons(ctxt: XmlExpCtxtPtr) -> i32 {
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).nb_cons
}

// Expressions are trees but the tree is opaque
#[cfg(feature = "libxml_expr")]
pub type XmlExpNodePtr = *mut XmlExpNode;
#[cfg(feature = "libxml_expr")]
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct Count {
    f_min: i32,
    f_max: i32,
}
#[cfg(feature = "libxml_expr")]
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct Children {
    f_right: XmlExpNodePtr,
}
#[cfg(feature = "libxml_expr")]
#[repr(C)]
union Field {
    count: Count,
    children: Children,
    f_str: *const XmlChar,
}
#[cfg(feature = "libxml_expr")]
#[repr(C)]
pub struct XmlExpNode {
    typ: u8,    /* xmlExpNodeType */
    info: u8,   /* OR of XmlExpNodeInfo */
    key: u16,   /* the hash key */
    refe: u32,  /* The number of references */
    c_max: i32, /* the maximum length it can consume */
    exp_left: XmlExpNodePtr,
    next: XmlExpNodePtr, /* the next node in the hash table or free list */
    field: Field,
}

#[cfg(feature = "libxml_expr")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlExpNodeType {
    XmlExpEmpty = 0,
    XmlExpForbid = 1,
    XmlExpAtom = 2,
    XmlExpSeq = 3,
    XmlExpOr = 4,
    XmlExpCount = 5,
}

// 2 core expressions shared by all for the empty language set
// and for the set with just the empty token
#[cfg(feature = "libxml_expr")]
pub static mut FORBIDDEN_EXP: XmlExpNodePtr = null_mut();
#[cfg(feature = "libxml_expr")]
pub static mut EMPTY_EXP: XmlExpNodePtr = null_mut();

/// Dereference the expression
#[doc(alias = "xmlExpFree")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_free(ctxt: XmlExpCtxtPtr, exp: XmlExpNodePtr) {
    if exp.is_null() || exp == FORBIDDEN_EXP || exp == EMPTY_EXP {
        return;
    }
    (*exp).refe -= 1;
    if (*exp).refe == 0 {
        /* Unlink it first from the hash table */
        let key: u16 = (*exp).key % (*ctxt).size as u16;
        if *(*ctxt).table.add(key as usize) == exp {
            *(*ctxt).table.add(key as usize) = (*exp).next;
        } else {
            let mut tmp: XmlExpNodePtr;

            tmp = *(*ctxt).table.add(key as usize);
            while !tmp.is_null() {
                if (*tmp).next == exp {
                    (*tmp).next = (*exp).next;
                    break;
                }
                tmp = (*tmp).next;
            }
        }

        if (*exp).typ == XmlExpNodeType::XmlExpSeq as u8
            || (*exp).typ == XmlExpNodeType::XmlExpOr as u8
        {
            xml_exp_free(ctxt, (*exp).exp_left);
            xml_exp_free(ctxt, (*exp).field.children.f_right);
        } else if (*exp).typ == XmlExpNodeType::XmlExpCount as u8 {
            xml_exp_free(ctxt, (*exp).exp_left);
        }
        xml_free(exp as _);
        (*ctxt).nb_nodes -= 1;
    }
}

/// Increase the reference count of the expression
#[doc(alias = "xmlExpRef")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_ref(exp: XmlExpNodePtr) {
    if !exp.is_null() {
        (*exp).refe += 1;
    }
}

macro_rules! IS_BLANK {
    ( $c:expr ) => {
        $c == b' ' as _ || $c == b'\n' as _ || $c == b'\r' as _ || $c == b'\t' as _
    };
}

macro_rules! SKIP_BLANKS {
    ( $ctxt:expr ) => {
        while IS_BLANK!(*(*$ctxt).cur) {
            (*$ctxt).cur = (*$ctxt).cur.add(1);
        }
    };
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_parse_number(ctxt: XmlExpCtxtPtr) -> i32 {
    let mut ret: i32 = 0;

    SKIP_BLANKS!(ctxt);
    if CUR!(ctxt) == b'*' as _ {
        NEXT!(ctxt);
        return -1;
    }
    if CUR!(ctxt) < b'0' as _ || CUR!(ctxt) > b'9' as _ {
        return -1;
    }
    while CUR!(ctxt) >= b'0' as _ && CUR!(ctxt) <= b'9' as _ {
        ret = ret * 10 + (CUR!(ctxt) as i32 - b'0' as i32);
        NEXT!(ctxt);
    }
    ret
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_parse_or(ctxt: XmlExpCtxtPtr) -> XmlExpNodePtr {
    let mut ret: XmlExpNodePtr;
    let val: *const XmlChar;

    SKIP_BLANKS!(ctxt);
    let base: *const c_char = (*ctxt).cur;
    if *(*ctxt).cur == b'(' as i8 {
        NEXT!(ctxt);
        ret = xml_exp_parse_expr(ctxt);
        SKIP_BLANKS!(ctxt);
        if *(*ctxt).cur != b')' as i8 {
            eprintln!(
                "unbalanced '(' : {}",
                CStr::from_ptr(base as _).to_string_lossy()
            );
            xml_exp_free(ctxt, ret);
            return null_mut();
        }
        NEXT!(ctxt);
        SKIP_BLANKS!(ctxt);
    } else {
        while CUR!(ctxt) != 0
            && !IS_BLANK!(CUR!(ctxt))
            && CUR!(ctxt) != b'(' as _
            && CUR!(ctxt) != b')' as _
            && CUR!(ctxt) != b'|' as _
            && CUR!(ctxt) != b',' as _
            && CUR!(ctxt) != b'{' as _
            && CUR!(ctxt) != b'*' as _
            && CUR!(ctxt) != b'+' as _
            && CUR!(ctxt) != b'?' as _
            && CUR!(ctxt) != b'}' as _
        {
            NEXT!(ctxt);
        }
        val = xml_dict_lookup((*ctxt).dict, base as _, (*ctxt).cur.offset_from(base) as _);
        if val.is_null() {
            return null_mut();
        }
        ret = xml_exp_hash_get_entry(
            ctxt,
            XmlExpNodeType::XmlExpAtom,
            null_mut(),
            null_mut(),
            val,
            0,
            0,
        );
        if ret.is_null() {
            return null_mut();
        }
        SKIP_BLANKS!(ctxt);
    }
    if CUR!(ctxt) == b'{' as _ {
        let max: i32;

        NEXT!(ctxt);
        let min: i32 = xml_exp_parse_number(ctxt);
        if min < 0 {
            xml_exp_free(ctxt, ret);
            return null_mut();
        }
        SKIP_BLANKS!(ctxt);
        if CUR!(ctxt) == b',' as _ {
            NEXT!(ctxt);
            max = xml_exp_parse_number(ctxt);
            SKIP_BLANKS!(ctxt);
        } else {
            max = min;
        }
        if CUR!(ctxt) != b'}' as _ {
            xml_exp_free(ctxt, ret);
            return null_mut();
        }
        NEXT!(ctxt);
        ret = xml_exp_hash_get_entry(
            ctxt,
            XmlExpNodeType::XmlExpCount,
            ret,
            null_mut(),
            null(),
            min,
            max,
        );
        SKIP_BLANKS!(ctxt);
    } else if CUR!(ctxt) == b'?' as _ {
        NEXT!(ctxt);
        ret = xml_exp_hash_get_entry(
            ctxt,
            XmlExpNodeType::XmlExpCount,
            ret,
            null_mut(),
            null(),
            0,
            1,
        );
        SKIP_BLANKS!(ctxt);
    } else if CUR!(ctxt) == b'+' as _ {
        NEXT!(ctxt);
        ret = xml_exp_hash_get_entry(
            ctxt,
            XmlExpNodeType::XmlExpCount,
            ret,
            null_mut(),
            null(),
            1,
            -1,
        );
        SKIP_BLANKS!(ctxt);
    } else if CUR!(ctxt) == b'*' as _ {
        NEXT!(ctxt);
        ret = xml_exp_hash_get_entry(
            ctxt,
            XmlExpNodeType::XmlExpCount,
            ret,
            null_mut(),
            null(),
            0,
            -1,
        );
        SKIP_BLANKS!(ctxt);
    }
    ret
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_parse_seq(ctxt: XmlExpCtxtPtr) -> XmlExpNodePtr {
    let mut ret: XmlExpNodePtr;
    let mut right: XmlExpNodePtr;

    ret = xml_exp_parse_or(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b'|' as i8 {
        NEXT!(ctxt);
        right = xml_exp_parse_or(ctxt);
        if right.is_null() {
            xml_exp_free(ctxt, ret);
            return null_mut();
        }
        ret = xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpOr, ret, right, null(), 0, 0);
        if ret.is_null() {
            return null_mut();
        }
    }
    ret
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_parse_expr(ctxt: XmlExpCtxtPtr) -> XmlExpNodePtr {
    let mut ret: XmlExpNodePtr;
    let mut right: XmlExpNodePtr;

    ret = xml_exp_parse_seq(ctxt);
    SKIP_BLANKS!(ctxt);
    while CUR!(ctxt) == b',' as i8 {
        NEXT!(ctxt);
        right = xml_exp_parse_seq(ctxt);
        if right.is_null() {
            xml_exp_free(ctxt, ret);
            return null_mut();
        }
        ret = xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpSeq, ret, right, null(), 0, 0);
        if ret.is_null() {
            return null_mut();
        }
    }
    ret
}

/// Minimal parser for regexps, it understand the following constructs
///  - string terminals
///  - choice operator |
///  - sequence operator ,
///  - subexpressions (...)
///  - usual cardinality operators + * and ?
///  - finite sequences  { min, max }
///  - infinite sequences { min, * }
///    There is minimal checkings made especially no checking on strings values
///
/// Returns a new expression or NULL in case of failure
#[doc(alias = "xmlExpParse")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_parse(ctxt: XmlExpCtxtPtr, expr: *const c_char) -> XmlExpNodePtr {
    (*ctxt).expr = expr;
    (*ctxt).cur = expr;

    let ret: XmlExpNodePtr = xml_exp_parse_expr(ctxt);
    SKIP_BLANKS!(ctxt);
    if *(*ctxt).cur != 0 {
        xml_exp_free(ctxt, ret);
        return null_mut();
    }
    ret
}

/// Calculate the hash key for a token
#[doc(alias = "xmlExpHashNameComputeKey")]
#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_hash_name_compute_key(mut name: *const XmlChar) -> u16 {
    let mut value: u16 = 0;
    let mut ch: c_char;

    if !name.is_null() {
        value += 30 * (*name) as u16;
        while {
            ch = *name as _;
            name = name.add(1);
            ch != 0
        } {
            value ^= (((value as u64) << 5) + (value as u64 >> 3) + ch as u64) as u16;
        }
    }
    value
}

/// Calculate the hash key for a compound expression
#[doc(alias = "xmlExpHashComputeKey")]
#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_hash_compute_key(
    typ: XmlExpNodeType,
    left: XmlExpNodePtr,
    right: XmlExpNodePtr,
) -> u16 {
    let mut value: u64;
    let ret: u16;

    match typ {
        XmlExpNodeType::XmlExpSeq => {
            value = (*left).key as _;
            value += (*right).key as u64;
            value *= 3;
            ret = value as u16;
        }
        XmlExpNodeType::XmlExpOr => {
            value = (*left).key as _;
            value += (*right).key as u64;
            value *= 7;
            ret = value as u16;
        }
        XmlExpNodeType::XmlExpCount => {
            value = (*left).key as _;
            value += (*right).key as u64;
            ret = value as u16;
        }
        _ => {
            ret = 0;
        }
    }
    ret
}

#[cfg(feature = "libxml_expr")]
const MAX_NODES: usize = 10000;

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_new_node(ctxt: XmlExpCtxtPtr, typ: XmlExpNodeType) -> XmlExpNodePtr {
    if (*ctxt).nb_nodes as usize >= MAX_NODES {
        return null_mut();
    }
    let ret: XmlExpNodePtr = xml_malloc(size_of::<XmlExpNode>()) as XmlExpNodePtr;
    if ret.is_null() {
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlExpNode>());
    (*ret).typ = typ as u8;
    (*ret).next = null_mut();
    (*ctxt).nb_nodes += 1;
    (*ctxt).nb_cons += 1;
    ret
}

/// Get the unique entry from the hash table. The entry is created if
/// needed. @left and @right are consumed, i.e. their refe count will
/// be decremented by the operation.
///
/// Returns the pointer or NULL in case of error
#[doc(alias = "xmlExpHashGetEntry")]
#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_hash_get_entry(
    ctxt: XmlExpCtxtPtr,
    typ: XmlExpNodeType,
    mut left: XmlExpNodePtr,
    mut right: XmlExpNodePtr,
    name: *const XmlChar,
    min: i32,
    max: i32,
) -> XmlExpNodePtr {
    let mut kbase: u16;
    let mut insert: XmlExpNodePtr;

    if ctxt.is_null() {
        return null_mut();
    }

    // Check for duplicate and insertion location.
    if matches!(typ, XmlExpNodeType::XmlExpAtom) {
        kbase = xml_exp_hash_name_compute_key(name);
    } else if matches!(typ, XmlExpNodeType::XmlExpCount) {
        // COUNT reduction rule 1
        // a{1} -> a
        if min == max {
            if min == 1 {
                return left;
            }
            if min == 0 {
                xml_exp_free(ctxt, left);
                return EMPTY_EXP;
            }
        }
        if min < 0 {
            xml_exp_free(ctxt, left);
            return FORBIDDEN_EXP;
        }
        if max == -1 {
            kbase = min as u16 + 79;
        } else {
            kbase = (max - min) as u16;
        }
        kbase += (*left).key;
    } else if matches!(typ, XmlExpNodeType::XmlExpOr) {
        // Forbid reduction rules
        if (*left).typ == XmlExpNodeType::XmlExpForbid as u8 {
            xml_exp_free(ctxt, left);
            return right;
        }
        if (*right).typ == XmlExpNodeType::XmlExpForbid as u8 {
            xml_exp_free(ctxt, right);
            return left;
        }

        // OR reduction rule 1
        // a | a reduced to a
        if left == right {
            xml_exp_free(ctxt, right);
            return left;
        }
        // OR canonicalization rule 1
        // linearize (a | b) | c into a | (b | c)
        if (*left).typ == XmlExpNodeType::XmlExpOr as u8
            && (*right).typ != XmlExpNodeType::XmlExpOr as u8
        {
            std::mem::swap(&mut left, &mut right);
        }
        // OR reduction rule 2
        // a | (a | b) and b | (a | b) are reduced to a | b
        if (*right).typ == XmlExpNodeType::XmlExpOr as u8
            && (left == (*right).exp_left || left == (*right).field.children.f_right)
        {
            xml_exp_free(ctxt, left);
            return right;
        }
        // OR canonicalization rule 2
        // linearize (a | b) | c into a | (b | c)
        if (*left).typ == XmlExpNodeType::XmlExpOr as u8 {
            let mut tmp: XmlExpNodePtr;

            // OR canonicalization rule 2
            if (*(*left).field.children.f_right).typ != XmlExpNodeType::XmlExpOr as u8
                && (*(*left).field.children.f_right).key < (*(*left).exp_left).key
            {
                tmp = (*left).field.children.f_right;
                (*left).field.children.f_right = (*left).exp_left;
                (*left).exp_left = tmp;
            }
            (*(*left).field.children.f_right).refe += 1;
            tmp = xml_exp_hash_get_entry(
                ctxt,
                XmlExpNodeType::XmlExpOr,
                (*left).field.children.f_right,
                right,
                null(),
                0,
                0,
            );
            (*(*left).exp_left).refe += 1;
            tmp = xml_exp_hash_get_entry(
                ctxt,
                XmlExpNodeType::XmlExpOr,
                (*left).exp_left,
                tmp,
                null(),
                0,
                0,
            );

            xml_exp_free(ctxt, left);
            return tmp;
        }
        if (*right).typ == XmlExpNodeType::XmlExpOr as u8 {
            // Ordering in the tree
            // C | (A | B) -> A | (B | C)
            if (*left).key > (*(*right).field.children.f_right).key {
                let mut tmp: XmlExpNodePtr;
                (*(*right).field.children.f_right).refe += 1;
                tmp = xml_exp_hash_get_entry(
                    ctxt,
                    XmlExpNodeType::XmlExpOr,
                    (*right).field.children.f_right,
                    left,
                    null(),
                    0,
                    0,
                );
                (*(*right).exp_left).refe += 1;
                tmp = xml_exp_hash_get_entry(
                    ctxt,
                    XmlExpNodeType::XmlExpOr,
                    (*right).exp_left,
                    tmp,
                    null(),
                    0,
                    0,
                );
                xml_exp_free(ctxt, right);
                return tmp;
            }
            // Ordering in the tree
            // B | (A | C) -> A | (B | C)
            if (*left).key > (*(*right).exp_left).key {
                let mut tmp: XmlExpNodePtr;
                (*(*right).field.children.f_right).refe += 1;
                tmp = xml_exp_hash_get_entry(
                    ctxt,
                    XmlExpNodeType::XmlExpOr,
                    left,
                    (*right).field.children.f_right,
                    null(),
                    0,
                    0,
                );
                (*(*right).exp_left).refe += 1;
                tmp = xml_exp_hash_get_entry(
                    ctxt,
                    XmlExpNodeType::XmlExpOr,
                    (*right).exp_left,
                    tmp,
                    null(),
                    0,
                    0,
                );
                xml_exp_free(ctxt, right);
                return tmp;
            }
        }
        // we know both types are != xmlExpNodeType::XML_EXP_OR here
        else if (*left).key > (*right).key {
            std::mem::swap(&mut left, &mut right);
        }
        kbase = xml_exp_hash_compute_key(typ, left, right);
    } else if matches!(typ, XmlExpNodeType::XmlExpSeq) {
        // Forbid reduction rules
        if (*left).typ == XmlExpNodeType::XmlExpForbid as u8 {
            xml_exp_free(ctxt, right);
            return left;
        }
        if (*right).typ == XmlExpNodeType::XmlExpForbid as u8 {
            xml_exp_free(ctxt, left);
            return right;
        }
        // Empty reduction rules
        if (*right).typ == XmlExpNodeType::XmlExpEmpty as u8 {
            return left;
        }
        if (*left).typ == XmlExpNodeType::XmlExpEmpty as u8 {
            return right;
        }
        kbase = xml_exp_hash_compute_key(typ, left, right);
    } else {
        return null_mut();
    }

    let key: u16 = kbase % (*ctxt).size as u16;
    if !(*(*ctxt).table.add(key as usize)).is_null() {
        insert = *(*ctxt).table.add(key as usize);
        while !insert.is_null() {
            if (*insert).key == kbase && (*insert).typ == typ as u8 {
                if matches!(typ, XmlExpNodeType::XmlExpAtom) {
                    if name == (*insert).field.f_str {
                        (*insert).refe += 1;
                        return insert;
                    }
                } else if matches!(typ, XmlExpNodeType::XmlExpCount) {
                    if (*insert).field.count.f_min == min
                        && (*insert).field.count.f_max == max
                        && (*insert).exp_left == left
                    {
                        (*insert).refe += 1;
                        (*left).refe -= 1;
                        return insert;
                    }
                } else if (*insert).exp_left == left && (*insert).field.children.f_right == right {
                    (*insert).refe += 1;
                    (*left).refe -= 1;
                    (*right).refe -= 1;
                    return insert;
                }
            }
            insert = (*insert).next;
        }
    }

    let entry: XmlExpNodePtr = xml_exp_new_node(ctxt, typ);
    if entry.is_null() {
        return null_mut();
    }
    (*entry).key = kbase;
    if matches!(typ, XmlExpNodeType::XmlExpAtom) {
        (*entry).field.f_str = name;
        (*entry).c_max = 1;
    } else if matches!(typ, XmlExpNodeType::XmlExpCount) {
        (*entry).field.count.f_min = min;
        (*entry).field.count.f_max = max;
        (*entry).exp_left = left;
        if min == 0 || IS_NILLABLE!(left) != 0 {
            (*entry).info |= XmlExpNodeInfo::XmlExpNilable as u8;
        }
        if max < 0 {
            (*entry).c_max = -1;
        } else {
            (*entry).c_max = max * (*(*entry).exp_left).c_max;
        }
    } else {
        (*entry).exp_left = left;
        (*entry).field.children.f_right = right;
        if matches!(typ, XmlExpNodeType::XmlExpOr) {
            if IS_NILLABLE!(left) != 0 || IS_NILLABLE!(right) != 0 {
                (*entry).info |= XmlExpNodeInfo::XmlExpNilable as u8;
            }
            if (*(*entry).exp_left).c_max == -1 || (*(*entry).field.children.f_right).c_max == -1 {
                (*entry).c_max = -1;
            } else if (*(*entry).exp_left).c_max > (*(*entry).field.children.f_right).c_max {
                (*entry).c_max = (*(*entry).exp_left).c_max;
            } else {
                (*entry).c_max = (*(*entry).field.children.f_right).c_max;
            }
        } else {
            if IS_NILLABLE!(left) != 0 && IS_NILLABLE!(right) != 0 {
                (*entry).info |= XmlExpNodeInfo::XmlExpNilable as u8;
            }
            if (*(*entry).exp_left).c_max == -1 || (*(*entry).field.children.f_right).c_max == -1 {
                (*entry).c_max = -1;
            } else {
                (*entry).c_max =
                    (*(*entry).exp_left).c_max + (*(*entry).field.children.f_right).c_max;
            }
        }
    }
    (*entry).refe = 1;
    if !(*(*ctxt).table.add(key as usize)).is_null() {
        (*entry).next = *(*ctxt).table.add(key as usize);
    }

    *(*ctxt).table.add(key as usize) = entry;
    (*ctxt).nb_elems += 1;

    entry
}

/// Get the atom associated to this name from that context
///
/// Returns the node or NULL in case of error
#[doc(alias = "xmlExpNewAtom")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_new_atom(
    ctxt: XmlExpCtxtPtr,
    mut name: *const XmlChar,
    len: i32,
) -> XmlExpNodePtr {
    if ctxt.is_null() || name.is_null() {
        return null_mut();
    }
    name = xml_dict_lookup((*ctxt).dict, name, len);
    if name.is_null() {
        return null_mut();
    }
    xml_exp_hash_get_entry(
        ctxt,
        XmlExpNodeType::XmlExpAtom,
        null_mut(),
        null_mut(),
        name,
        0,
        0,
    )
}

/// Get the atom associated to the choice @left | @right
/// Note that @left and @right are consumed in the operation, to keep
/// an handle on them use xmlExpRef() and use xmlExpFree() to release them,
/// this is true even in case of failure (unless ctxt.is_null()).
///
/// Returns the node or NULL in case of error
#[doc(alias = "xmlExpNewOr")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_new_or(
    ctxt: XmlExpCtxtPtr,
    left: XmlExpNodePtr,
    right: XmlExpNodePtr,
) -> XmlExpNodePtr {
    if ctxt.is_null() {
        return null_mut();
    }
    if left.is_null() || right.is_null() {
        xml_exp_free(ctxt, left);
        xml_exp_free(ctxt, right);
        return null_mut();
    }
    xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpOr, left, right, null(), 0, 0)
}

/// Get the atom associated to the sequence @left , @right
/// Note that @left and @right are consumed in the operation, to keep
/// an handle on them use xmlExpRef() and use xmlExpFree() to release them,
/// this is true even in case of failure (unless ctxt.is_null()).
///
/// Returns the node or NULL in case of error
#[doc(alias = "xmlExpNewSeq")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_new_seq(
    ctxt: XmlExpCtxtPtr,
    left: XmlExpNodePtr,
    right: XmlExpNodePtr,
) -> XmlExpNodePtr {
    if ctxt.is_null() {
        return null_mut();
    }
    if left.is_null() || right.is_null() {
        xml_exp_free(ctxt, left);
        xml_exp_free(ctxt, right);
        return null_mut();
    }
    xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpSeq, left, right, null(), 0, 0)
}

/// Get the atom associated to the range (@subset){@min, @max}
/// Note that @subset is consumed in the operation, to keep
/// an handle on it use xmlExpRef() and use xmlExpFree() to release it,
/// this is true even in case of failure (unless ctxt.is_null()).
///
/// Returns the node or NULL in case of error
#[doc(alias = "xmlExpNewRange")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_new_range(
    ctxt: XmlExpCtxtPtr,
    subset: XmlExpNodePtr,
    min: i32,
    max: i32,
) -> XmlExpNodePtr {
    if ctxt.is_null() {
        return null_mut();
    }
    if subset.is_null() || min < 0 || max < -1 || (max >= 0 && min > max) {
        xml_exp_free(ctxt, subset);
        return null_mut();
    }
    xml_exp_hash_get_entry(
        ctxt,
        XmlExpNodeType::XmlExpCount,
        subset,
        null_mut(),
        null(),
        min,
        max,
    )
}

/// Finds if the expression is nillable, i.e. if it accepts the empty sequence
///
/// Returns 1 if nillable, 0 if not and -1 in case of error
#[doc(alias = "xmlExpIsNillable")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_is_nillable(exp: XmlExpNodePtr) -> i32 {
    if exp.is_null() {
        return -1;
    }
    (IS_NILLABLE!(exp) != 0) as i32
}

/// Indicate the maximum number of input a expression can accept
///
/// Returns the maximum length or -1 in case of error
#[doc(alias = "xmlExpMaxToken")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_max_token(expr: XmlExpNodePtr) -> i32 {
    if expr.is_null() {
        return -1;
    }
    (*expr).c_max
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_get_language_int(
    _ctxt: XmlExpCtxtPtr,
    mut exp: XmlExpNodePtr,
    list: *mut *const XmlChar,
    len: i32,
    nb: i32,
) -> i32 {
    let tmp: i32;
    let tmp2: i32;
    loop {
        if (*exp).typ == XmlExpNodeType::XmlExpEmpty as u8 {
            return 0;
        } else if (*exp).typ == XmlExpNodeType::XmlExpAtom as u8 {
            for tmp in 0..nb {
                if *list.add(tmp as usize) == (*exp).field.f_str {
                    return 0;
                }
            }
            if nb >= len {
                return -2;
            }
            *list.add(nb as usize) = (*exp).field.f_str;
            return 1;
        } else if (*exp).typ == XmlExpNodeType::XmlExpCount as u8 {
            exp = (*exp).exp_left;
            continue;
        } else if (*exp).typ == XmlExpNodeType::XmlExpSeq as u8
            || (*exp).typ == XmlExpNodeType::XmlExpOr as u8
        {
            tmp = xml_exp_get_language_int(_ctxt, (*exp).exp_left, list, len, nb);
            if tmp < 0 {
                return tmp;
            }
            tmp2 =
                xml_exp_get_language_int(_ctxt, (*exp).field.children.f_right, list, len, nb + tmp);
            if tmp2 < 0 {
                return tmp2;
            }
            return tmp + tmp2;
        }
    }
    // return (-1);
}

/// Find all the strings used in @exp and store them in @list
///
/// Returns the number of unique strings found, -1 in case of errors and
/// -2 if there is more than @len strings
#[doc(alias = "xmlExpGetLanguage")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_get_language(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    lang_list: *mut *const XmlChar,
    len: i32,
) -> i32 {
    if ctxt.is_null() || exp.is_null() || lang_list.is_null() || len <= 0 {
        return -1;
    }
    xml_exp_get_language_int(ctxt, exp, lang_list, len, 0)
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_get_start_int(
    _ctxt: XmlExpCtxtPtr,
    mut exp: XmlExpNodePtr,
    list: *mut *const XmlChar,
    len: i32,
    nb: i32,
) -> i32 {
    let mut tmp: i32;
    let tmp2: i32;
    loop {
        if (*exp).typ == XmlExpNodeType::XmlExpForbid as u8
            || (*exp).typ == XmlExpNodeType::XmlExpEmpty as u8
        {
            return 0;
        } else if (*exp).typ == XmlExpNodeType::XmlExpAtom as u8 {
            for tmp in 0..nb {
                if *list.add(tmp as usize) == (*exp).field.f_str {
                    return 0;
                }
            }
            if nb >= len {
                return -2;
            }
            *list.add(nb as usize) = (*exp).field.f_str;
            return 1;
        } else if (*exp).typ == XmlExpNodeType::XmlExpCount as u8 {
            exp = (*exp).exp_left;
            continue;
        } else if (*exp).typ == XmlExpNodeType::XmlExpSeq as u8 {
            tmp = xml_exp_get_start_int(_ctxt, (*exp).exp_left, list, len, nb);
            if tmp < 0 {
                return tmp;
            }
            if IS_NILLABLE!((*exp).exp_left) != 0 {
                tmp2 = xml_exp_get_start_int(
                    _ctxt,
                    (*exp).field.children.f_right,
                    list,
                    len,
                    nb + tmp,
                );
                if tmp2 < 0 {
                    return tmp2;
                }
                tmp += tmp2;
            }
            return tmp;
        } else if (*exp).typ == XmlExpNodeType::XmlExpOr as u8 {
            tmp = xml_exp_get_start_int(_ctxt, (*exp).exp_left, list, len, nb);
            if tmp < 0 {
                return tmp;
            }
            tmp2 = xml_exp_get_start_int(_ctxt, (*exp).field.children.f_right, list, len, nb + tmp);
            if tmp2 < 0 {
                return tmp2;
            }
            return tmp + tmp2;
        }
    }
    // return -1;
}

/// Find all the strings that appears at the start of the languages
/// accepted by @exp and store them in @list. E.g. for (a, b) | c
/// it will return the list [a, c]
///
/// Returns the number of unique strings found, -1 in case of errors and
/// -2 if there is more than @len strings
#[doc(alias = "xmlExpGetStart")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_get_start(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    tok_list: *mut *const XmlChar,
    len: i32,
) -> i32 {
    if ctxt.is_null() || exp.is_null() || tok_list.is_null() || len <= 0 {
        return -1;
    }
    xml_exp_get_start_int(ctxt, exp, tok_list, len, 0)
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_string_derive_int(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    str: *const XmlChar,
) -> XmlExpNodePtr {
    let mut ret: XmlExpNodePtr;

    if (*exp).typ == XmlExpNodeType::XmlExpEmpty as u8
        || (*exp).typ == XmlExpNodeType::XmlExpForbid as u8
    {
        return FORBIDDEN_EXP;
    } else if (*exp).typ == XmlExpNodeType::XmlExpAtom as u8 {
        if (*exp).field.f_str == str {
            ret = EMPTY_EXP;
        } else {
            /* TODO wildcards here */
            ret = FORBIDDEN_EXP;
        }
        return ret;
    } else if (*exp).typ == XmlExpNodeType::XmlExpOr as u8 {
        let tmp: XmlExpNodePtr = xml_exp_string_derive_int(ctxt, (*exp).exp_left, str);
        if tmp.is_null() {
            return null_mut();
        }
        ret = xml_exp_string_derive_int(ctxt, (*exp).field.children.f_right, str);
        if ret.is_null() {
            xml_exp_free(ctxt, tmp);
            return null_mut();
        }
        ret = xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpOr, tmp, ret, null(), 0, 0);
        return ret;
    } else if (*exp).typ == XmlExpNodeType::XmlExpSeq as u8 {
        ret = xml_exp_string_derive_int(ctxt, (*exp).exp_left, str);
        if ret.is_null() {
            return null_mut();
        } else if ret == FORBIDDEN_EXP {
            if IS_NILLABLE!((*exp).exp_left) != 0 {
                ret = xml_exp_string_derive_int(ctxt, (*exp).field.children.f_right, str);
            }
        } else {
            (*(*exp).field.children.f_right).refe += 1;
            ret = xml_exp_hash_get_entry(
                ctxt,
                XmlExpNodeType::XmlExpSeq,
                ret,
                (*exp).field.children.f_right,
                null(),
                0,
                0,
            );
        }
        return ret;
    } else if (*exp).typ == XmlExpNodeType::XmlExpCount as u8 {
        if (*exp).field.count.f_max == 0 {
            return FORBIDDEN_EXP;
        }
        ret = xml_exp_string_derive_int(ctxt, (*exp).exp_left, str);
        if ret.is_null() {
            return null_mut();
        }
        if ret == FORBIDDEN_EXP {
            return ret;
        }
        if (*exp).field.count.f_max == 1 {
            return ret;
        }
        let max = if (*exp).field.count.f_max < 0 {
            // unbounded
            -1
        } else {
            (*exp).field.count.f_max - 1
        };
        let min = if (*exp).field.count.f_min > 0 {
            (*exp).field.count.f_min - 1
        } else {
            0
        };
        (*(*exp).exp_left).refe += 1;
        let tmp: XmlExpNodePtr = xml_exp_hash_get_entry(
            ctxt,
            XmlExpNodeType::XmlExpCount,
            (*exp).exp_left,
            null_mut(),
            null(),
            min,
            max,
        );
        if ret == EMPTY_EXP {
            return tmp;
        }
        return xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpSeq, ret, tmp, null(), 0, 0);
    }
    null_mut()
}

/// Do one step of Brzozowski derivation of the expression @exp with respect to the input string
///
/// Returns the resulting expression or NULL in case of internal error
#[doc(alias = "xmlExpStringDerive")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_string_derive(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    str: *const XmlChar,
    len: i32,
) -> XmlExpNodePtr {
    use super::dict::xml_dict_exists;

    if exp.is_null() || ctxt.is_null() || str.is_null() {
        return null_mut();
    }
    // check the string is in the dictionary, if yes use an interned copy,
    // otherwise we know it's not an acceptable input
    let input: *const XmlChar = xml_dict_exists((*ctxt).dict, str, len);
    if input.is_null() {
        return FORBIDDEN_EXP;
    }
    xml_exp_string_derive_int(ctxt, exp, input)
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_check_card(exp: XmlExpNodePtr, sub: XmlExpNodePtr) -> i32 {
    let mut ret: i32 = 1;

    if (*sub).c_max == -1 {
        if (*exp).c_max != -1 {
            ret = 0;
        }
    } else if (*exp).c_max >= 0 && (*exp).c_max < (*sub).c_max {
        ret = 0;
    }
    ret
}

/// Check if exp is a multiple of sub, i.e. if there is a finite number n
/// so that sub{n} subsume exp
///
/// Returns the multiple value if successful, 0 if it is not a multiple
/// and -1 in case of internal error.
#[doc(alias = "xmlExpDivide")]
#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_divide(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    sub: XmlExpNodePtr,
    mult: *mut XmlExpNodePtr,
    remain: *mut XmlExpNodePtr,
) -> i32 {
    let mut tmp: XmlExpNodePtr;
    let mut tmp2: XmlExpNodePtr;

    if !mult.is_null() {
        *mult = null_mut();
    }
    if !remain.is_null() {
        *remain = null_mut();
    }
    if (*exp).c_max == -1 {
        return 0;
    }
    if IS_NILLABLE!(exp) != 0 && IS_NILLABLE!(sub) == 0 {
        return 0;
    }

    for i in 1..=(*exp).c_max {
        (*sub).refe += 1;
        tmp = xml_exp_hash_get_entry(
            ctxt,
            XmlExpNodeType::XmlExpCount,
            sub,
            null_mut(),
            null(),
            i,
            i,
        );
        if tmp.is_null() {
            return -1;
        }
        if xml_exp_check_card(tmp, exp) == 0 {
            xml_exp_free(ctxt, tmp);
            continue;
        }
        tmp2 = xml_exp_exp_derive_int(ctxt, tmp, exp);
        if tmp2.is_null() {
            xml_exp_free(ctxt, tmp);
            return -1;
        }
        if tmp2 != FORBIDDEN_EXP && IS_NILLABLE!(tmp2) != 0 {
            if !remain.is_null() {
                *remain = tmp2;
            } else {
                xml_exp_free(ctxt, tmp2);
            }
            if !mult.is_null() {
                *mult = tmp;
            } else {
                xml_exp_free(ctxt, tmp);
            }
            return i;
        }
        xml_exp_free(ctxt, tmp);
        xml_exp_free(ctxt, tmp2);
    }
    0
}

/// Try to do a step of Brzozowski derivation but at a higher level
/// the input being a subexpression.
///
/// Returns the resulting expression or NULL in case of internal error
#[doc(alias = "xmlExpExpDeriveInt")]
unsafe fn xml_exp_exp_derive_int(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    sub: XmlExpNodePtr,
) -> XmlExpNodePtr {
    let mut ret: XmlExpNodePtr;
    let mut tmp: XmlExpNodePtr;
    let mut tmp2: XmlExpNodePtr;
    let mut tmp3: XmlExpNodePtr;
    let mut tab: *mut *const XmlChar;
    let mut len: i32;

    // In case of equality and if the expression can only consume a finite amount,
    // then the derivation is empty
    if exp == sub && (*exp).c_max >= 0 {
        return EMPTY_EXP;
    }
    // decompose sub sequence first
    if (*sub).typ == XmlExpNodeType::XmlExpEmpty as u8 {
        (*exp).refe += 1;
        return exp;
    }
    if (*sub).typ == XmlExpNodeType::XmlExpSeq as u8 {
        tmp = xml_exp_exp_derive_int(ctxt, exp, (*sub).exp_left);
        if tmp.is_null() {
            return null_mut();
        }
        if tmp == FORBIDDEN_EXP {
            return tmp;
        }
        ret = xml_exp_exp_derive_int(ctxt, tmp, (*sub).field.children.f_right);
        xml_exp_free(ctxt, tmp);
        return ret;
    }
    if (*sub).typ == XmlExpNodeType::XmlExpOr as u8 {
        tmp = xml_exp_exp_derive_int(ctxt, exp, (*sub).exp_left);
        if tmp == FORBIDDEN_EXP {
            return tmp;
        }
        if tmp.is_null() {
            return null_mut();
        }
        ret = xml_exp_exp_derive_int(ctxt, exp, (*sub).field.children.f_right);
        if ret.is_null() || ret == FORBIDDEN_EXP {
            xml_exp_free(ctxt, tmp);
            return ret;
        }
        return xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpOr, tmp, ret, null(), 0, 0);
    }
    if xml_exp_check_card(exp, sub) == 0 {
        return FORBIDDEN_EXP;
    }
    if (*exp).typ == XmlExpNodeType::XmlExpEmpty as u8 {
        if sub == EMPTY_EXP {
            return EMPTY_EXP;
        }
        return FORBIDDEN_EXP;
    } else if (*exp).typ == XmlExpNodeType::XmlExpForbid as u8 {
        return FORBIDDEN_EXP;
    } else if (*exp).typ == XmlExpNodeType::XmlExpAtom as u8 {
        if (*sub).typ == XmlExpNodeType::XmlExpAtom as u8 {
            // TODO: handle wildcards
            if (*exp).field.f_str == (*sub).field.f_str {
                return EMPTY_EXP;
            }
            return FORBIDDEN_EXP;
        }
        if (*sub).typ == XmlExpNodeType::XmlExpCount as u8
            && (*sub).field.count.f_max == 1
            && (*(*sub).exp_left).typ == XmlExpNodeType::XmlExpAtom as u8
        {
            // TODO: handle wildcards
            if (*exp).field.f_str == (*(*sub).exp_left).field.f_str {
                return EMPTY_EXP;
            }
            return FORBIDDEN_EXP;
        }
        return FORBIDDEN_EXP;
    } else if (*exp).typ == XmlExpNodeType::XmlExpSeq as u8 {
        // try to get the sequence consumed only if possible
        if xml_exp_check_card((*exp).exp_left, sub) != 0 {
            // See if the sequence can be consumed directly
            ret = xml_exp_exp_derive_int(ctxt, (*exp).exp_left, sub);
            if ret != FORBIDDEN_EXP && !ret.is_null() {
                // TODO: assumption here that we are determinist
                //       i.e. we won't get to a nillable exp left
                //       subset which could be matched by the right
                //       part too.
                // e.g.: (a | b)+,(a | c) and 'a+,a'
                (*(*exp).field.children.f_right).refe += 1;
                return xml_exp_hash_get_entry(
                    ctxt,
                    XmlExpNodeType::XmlExpSeq,
                    ret,
                    (*exp).field.children.f_right,
                    null(),
                    0,
                    0,
                );
            }
        }
        // Try instead to decompose
        if (*sub).typ == XmlExpNodeType::XmlExpCount as u8 {
            let min: i32;
            let max: i32;

            ret = xml_exp_exp_derive_int(ctxt, (*exp).exp_left, (*sub).exp_left);
            if ret.is_null() {
                return null_mut();
            }
            if ret != FORBIDDEN_EXP {
                if (*sub).field.count.f_max < 0 {
                    max = -1;
                } else {
                    max = (*sub).field.count.f_max - 1;
                }
                if (*sub).field.count.f_min > 0 {
                    min = (*sub).field.count.f_min - 1;
                } else {
                    min = 0;
                }
                (*(*exp).field.children.f_right).refe += 1;
                tmp = xml_exp_hash_get_entry(
                    ctxt,
                    XmlExpNodeType::XmlExpSeq,
                    ret,
                    (*exp).field.children.f_right,
                    null(),
                    0,
                    0,
                );
                if tmp.is_null() {
                    return null_mut();
                }

                (*(*sub).exp_left).refe += 1;
                tmp2 = xml_exp_hash_get_entry(
                    ctxt,
                    XmlExpNodeType::XmlExpCount,
                    (*sub).exp_left,
                    null_mut(),
                    null(),
                    min,
                    max,
                );
                if tmp2.is_null() {
                    xml_exp_free(ctxt, tmp);
                    return null_mut();
                }
                ret = xml_exp_exp_derive_int(ctxt, tmp, tmp2);
                xml_exp_free(ctxt, tmp);
                xml_exp_free(ctxt, tmp2);
                return ret;
            }
        }
        // we made no progress on structured operations
    } else if (*exp).typ == XmlExpNodeType::XmlExpOr as u8 {
        ret = xml_exp_exp_derive_int(ctxt, (*exp).exp_left, sub);
        if ret.is_null() {
            return null_mut();
        }
        tmp = xml_exp_exp_derive_int(ctxt, (*exp).field.children.f_right, sub);
        if tmp.is_null() {
            xml_exp_free(ctxt, ret);
            return null_mut();
        }
        return xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpOr, ret, tmp, null(), 0, 0);
    } else if (*exp).typ == XmlExpNodeType::XmlExpCount as u8 {
        let min: i32;
        let max: i32;

        if (*sub).typ == XmlExpNodeType::XmlExpCount as u8 {
            // Try to see if the loop is completely subsumed
            tmp = xml_exp_exp_derive_int(ctxt, (*exp).exp_left, (*sub).exp_left);
            if tmp.is_null() {
                return null_mut();
            }
            if tmp == FORBIDDEN_EXP {
                let mult: i32 = xml_exp_divide(
                    ctxt,
                    (*sub).exp_left,
                    (*exp).exp_left,
                    null_mut(),
                    addr_of_mut!(tmp),
                );
                if mult <= 0 {
                    return FORBIDDEN_EXP;
                }
                if (*sub).field.count.f_max == -1 {
                    max = -1;
                    if (*exp).field.count.f_max == -1 {
                        if (*exp).field.count.f_min <= (*sub).field.count.f_min * mult {
                            min = 0;
                        } else {
                            min = (*exp).field.count.f_min - (*sub).field.count.f_min * mult;
                        }
                    } else {
                        xml_exp_free(ctxt, tmp);
                        return FORBIDDEN_EXP;
                    }
                } else if (*exp).field.count.f_max == -1 {
                    if (*exp).field.count.f_min > (*sub).field.count.f_min * mult {
                        max = -1;
                        min = (*exp).field.count.f_min - (*sub).field.count.f_min * mult;
                    } else {
                        max = -1;
                        min = 0;
                    }
                } else {
                    if (*exp).field.count.f_max < (*sub).field.count.f_max * mult {
                        xml_exp_free(ctxt, tmp);
                        return FORBIDDEN_EXP;
                    }
                    if (*sub).field.count.f_max * mult > (*exp).field.count.f_min {
                        min = 0;
                    } else {
                        min = (*exp).field.count.f_min - (*sub).field.count.f_max * mult;
                    }
                    max = (*exp).field.count.f_max - (*sub).field.count.f_max * mult;
                }
            } else if IS_NILLABLE!(tmp) == 0 {
                // TODO: loop here to try to grow if working on finite blocks.
                xml_exp_free(ctxt, tmp);
                return FORBIDDEN_EXP;
            } else if (*sub).field.count.f_max == -1 {
                if (*exp).field.count.f_max == -1 {
                    if (*exp).field.count.f_min <= (*sub).field.count.f_min {
                        max = -1;
                        min = 0;
                    } else {
                        max = -1;
                        min = (*exp).field.count.f_min - (*sub).field.count.f_min;
                    }
                } else if (*exp).field.count.f_min > (*sub).field.count.f_min {
                    xml_exp_free(ctxt, tmp);
                    return FORBIDDEN_EXP;
                } else {
                    max = -1;
                    min = 0;
                }
            } else if (*exp).field.count.f_max == -1 {
                if (*exp).field.count.f_min > (*sub).field.count.f_min {
                    max = -1;
                    min = (*exp).field.count.f_min - (*sub).field.count.f_min;
                } else {
                    max = -1;
                    min = 0;
                }
            } else {
                if (*exp).field.count.f_max < (*sub).field.count.f_max {
                    xml_exp_free(ctxt, tmp);
                    return FORBIDDEN_EXP;
                }
                if (*sub).field.count.f_max > (*exp).field.count.f_min {
                    min = 0;
                } else {
                    min = (*exp).field.count.f_min - (*sub).field.count.f_max;
                }
                max = (*exp).field.count.f_max - (*sub).field.count.f_max;
            }
            (*(*exp).exp_left).refe += 1;
            tmp2 = xml_exp_hash_get_entry(
                ctxt,
                XmlExpNodeType::XmlExpCount,
                (*exp).exp_left,
                null_mut(),
                null(),
                min,
                max,
            );
            if tmp2.is_null() {
                return null_mut();
            }
            ret = xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpSeq, tmp, tmp2, null(), 0, 0);
            return ret;
        }
        tmp = xml_exp_exp_derive_int(ctxt, (*exp).exp_left, sub);
        if tmp.is_null() {
            return null_mut();
        }
        if tmp == FORBIDDEN_EXP {
            return FORBIDDEN_EXP;
        }
        if (*exp).field.count.f_min > 0 {
            min = (*exp).field.count.f_min - 1;
        } else {
            min = 0;
        }
        if (*exp).field.count.f_max < 0 {
            max = -1;
        } else {
            max = (*exp).field.count.f_max - 1;
        }

        (*(*exp).exp_left).refe += 1;
        tmp2 = xml_exp_hash_get_entry(
            ctxt,
            XmlExpNodeType::XmlExpCount,
            (*exp).exp_left,
            null_mut(),
            null(),
            min,
            max,
        );
        if tmp2.is_null() {
            return null_mut();
        }
        ret = xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpSeq, tmp, tmp2, null(), 0, 0);
        return ret;
    }

    if IS_NILLABLE!(sub) != 0 {
        if IS_NILLABLE!(exp) == 0 {
            return FORBIDDEN_EXP;
        } else {
            ret = EMPTY_EXP;
        }
    } else {
        ret = null_mut();
    }
    // here the structured derivation made no progress so
    // we use the default token based derivation to force one more step
    if (*ctxt).tab_size == 0 {
        (*ctxt).tab_size = 40;
    }

    tab =
        xml_malloc((*ctxt).tab_size as usize * size_of::<*const XmlChar>()) as *mut *const XmlChar;
    if tab.is_null() {
        return null_mut();
    }

    // collect all the strings accepted by the subexpression on input
    len = xml_exp_get_start_int(ctxt, sub, tab, (*ctxt).tab_size, 0);
    while len < 0 {
        let temp: *mut *const XmlChar = xml_realloc(
            tab as _,
            (*ctxt).tab_size as usize * 2 * size_of::<*const XmlChar>(),
        ) as *mut *const XmlChar;
        if temp.is_null() {
            xml_free(tab as _);
            return null_mut();
        }
        tab = temp;
        (*ctxt).tab_size *= 2;
        len = xml_exp_get_start_int(ctxt, sub, tab, (*ctxt).tab_size, 0);
    }
    for i in 0..len {
        tmp = xml_exp_string_derive_int(ctxt, exp, *tab.add(i as usize));
        if tmp.is_null() || tmp == FORBIDDEN_EXP {
            xml_exp_free(ctxt, ret);
            xml_free(tab as _);
            return tmp;
        }
        tmp2 = xml_exp_string_derive_int(ctxt, sub, *tab.add(i as usize));
        if tmp2.is_null() || tmp2 == FORBIDDEN_EXP {
            xml_exp_free(ctxt, tmp);
            xml_exp_free(ctxt, ret);
            xml_free(tab as _);
            return tmp;
        }
        tmp3 = xml_exp_exp_derive_int(ctxt, tmp, tmp2);
        xml_exp_free(ctxt, tmp);
        xml_exp_free(ctxt, tmp2);

        if tmp3.is_null() || tmp3 == FORBIDDEN_EXP {
            xml_exp_free(ctxt, ret);
            xml_free(tab as _);
            return tmp3;
        }

        if ret.is_null() {
            ret = tmp3;
        } else {
            ret = xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpOr, ret, tmp3, null(), 0, 0);
            if ret.is_null() {
                xml_free(tab as _);
                return null_mut();
            }
        }
    }
    xml_free(tab as _);
    ret
}

/// Evaluates the expression resulting from @exp consuming a sub expression @sub
/// Based on algebraic derivation and sometimes direct Brzozowski derivation
/// it usually takes less than linear time and can handle expressions generating
/// infinite languages.
///
/// Returns the resulting expression or NULL in case of internal error,
/// the result must be freed
#[doc(alias = "xmlExpExpDerive")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_exp_derive(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    sub: XmlExpNodePtr,
) -> XmlExpNodePtr {
    if exp.is_null() || ctxt.is_null() || sub.is_null() {
        return null_mut();
    }

    // O(1) speedups
    if IS_NILLABLE!(sub) != 0 && IS_NILLABLE!(exp) == 0 {
        return FORBIDDEN_EXP;
    }
    if xml_exp_check_card(exp, sub) == 0 {
        return FORBIDDEN_EXP;
    }
    xml_exp_exp_derive_int(ctxt, exp, sub)
}

/// Check whether @exp accepts all the languages accepted by @sub
/// the input being a subexpression.
///
/// Returns 1 if true 0 if false and -1 in case of failure.
#[doc(alias = "xmlExpSubsume")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_subsume(ctxt: XmlExpCtxtPtr, exp: XmlExpNodePtr, sub: XmlExpNodePtr) -> i32 {
    if exp.is_null() || ctxt.is_null() || sub.is_null() {
        return -1;
    }

    // TODO: speedup by checking the language of sub is a subset of the language of exp
    // O(1) speedups
    if IS_NILLABLE!(sub) != 0 && IS_NILLABLE!(exp) == 0 {
        return 0;
    }
    if xml_exp_check_card(exp, sub) == 0 {
        return 0;
    }
    let tmp: XmlExpNodePtr = xml_exp_exp_derive_int(ctxt, exp, sub);
    if tmp.is_null() {
        return -1;
    }
    if tmp == FORBIDDEN_EXP {
        return 0;
    }
    if tmp == EMPTY_EXP {
        return 1;
    }
    if !tmp.is_null() && IS_NILLABLE!(tmp) != 0 {
        xml_exp_free(ctxt, tmp);
        return 1;
    }
    xml_exp_free(ctxt, tmp);
    0
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_dump_int<'a>(buf: &mut (impl Write + 'a), expr: XmlExpNodePtr, glob: i32) {
    let mut c: XmlExpNodePtr;

    if expr.is_null() {
        return;
    }
    if glob != 0 {
        write!(buf, "(");
    }
    if (*expr).typ == XmlExpNodeType::XmlExpEmpty as u8 {
        write!(buf, "empty");
    } else if (*expr).typ == XmlExpNodeType::XmlExpForbid as u8 {
        write!(buf, "forbidden");
    } else if (*expr).typ == XmlExpNodeType::XmlExpAtom as u8 {
        write!(
            buf,
            "{}",
            CStr::from_ptr((*expr).field.f_str as *const i8)
                .to_string_lossy()
                .as_ref()
        );
    } else if (*expr).typ == XmlExpNodeType::XmlExpSeq as u8 {
        c = (*expr).exp_left;
        if (*c).typ == XmlExpNodeType::XmlExpSeq as u8 || (*c).typ == XmlExpNodeType::XmlExpOr as u8
        {
            xml_exp_dump_int(buf, c, 1);
        } else {
            xml_exp_dump_int(buf, c, 0);
        }
        write!(buf, " , ");
        c = (*expr).field.children.f_right;
        if (*c).typ == XmlExpNodeType::XmlExpSeq as u8 || (*c).typ == XmlExpNodeType::XmlExpOr as u8
        {
            xml_exp_dump_int(buf, c, 1);
        } else {
            xml_exp_dump_int(buf, c, 0);
        }
    } else if (*expr).typ == XmlExpNodeType::XmlExpOr as u8 {
        c = (*expr).exp_left;
        if (*c).typ == XmlExpNodeType::XmlExpSeq as u8 || (*c).typ == XmlExpNodeType::XmlExpOr as u8
        {
            xml_exp_dump_int(buf, c, 1);
        } else {
            xml_exp_dump_int(buf, c, 0);
        }
        write!(buf, " | ");
        c = (*expr).field.children.f_right;
        if (*c).typ == XmlExpNodeType::XmlExpSeq as u8 || (*c).typ == XmlExpNodeType::XmlExpOr as u8
        {
            xml_exp_dump_int(buf, c, 1);
        } else {
            xml_exp_dump_int(buf, c, 0);
        }
    } else if (*expr).typ == XmlExpNodeType::XmlExpCount as u8 {
        let mut rep: [i8; 40] = [0; 40];

        c = (*expr).exp_left;
        if (*c).typ == XmlExpNodeType::XmlExpSeq as u8 || (*c).typ == XmlExpNodeType::XmlExpOr as u8
        {
            xml_exp_dump_int(buf, c, 1);
        } else {
            xml_exp_dump_int(buf, c, 0);
        }
        if (*expr).field.count.f_min == 0 && (*expr).field.count.f_max == 1 {
            rep[0] = b'?' as _;
            rep[1] = 0;
        } else if (*expr).field.count.f_min == 0 && (*expr).field.count.f_max == -1 {
            rep[0] = b'*' as _;
            rep[1] = 0;
        } else if (*expr).field.count.f_min == 1 && (*expr).field.count.f_max == -1 {
            rep[0] = b'+' as _;
            rep[1] = 0;
        } else if (*expr).field.count.f_max == (*expr).field.count.f_min {
            snprintf(
                rep.as_mut_ptr(),
                39,
                c"{%d}".as_ptr(),
                (*expr).field.count.f_min,
            );
        } else if (*expr).field.count.f_max < 0 {
            snprintf(
                rep.as_mut_ptr(),
                39,
                c"{%d,inf}".as_ptr(),
                (*expr).field.count.f_min,
            );
        } else {
            snprintf(
                rep.as_mut_ptr(),
                39,
                c"{%d,%d}".as_ptr(),
                (*expr).field.count.f_min,
                (*expr).field.count.f_max,
            );
        }
        rep[39] = 0;
        write!(buf, "{}", CStr::from_ptr(rep.as_ptr()).to_string_lossy());
    } else {
        eprintln!("Error in tree");
    }
    if glob != 0 {
        write!(buf, ")");
    }
}

/// Serialize the expression as compiled to the buffer
#[doc(alias = "xmlExpDump")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_dump<'a>(buf: &mut (impl Write + 'a), expr: XmlExpNodePtr) {
    if expr.is_null() {
        return;
    }
    xml_exp_dump_int(buf, expr, 0);
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_exp_ctxt_nb_cons() {
        #[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_EXP_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_exp_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_exp_ctxt_nb_cons(ctxt);
                desret_int(ret_val);
                des_xml_exp_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlExpCtxtNbCons",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlExpCtxtNbCons()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_exp_ctxt_nb_nodes() {
        #[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_EXP_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_exp_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_exp_ctxt_nb_nodes(ctxt);
                desret_int(ret_val);
                des_xml_exp_ctxt_ptr(n_ctxt, ctxt, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlExpCtxtNbNodes",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlExpCtxtNbNodes()");
                    eprintln!(" {}", n_ctxt);
                }
            }
        }
    }

    #[test]
    fn test_xml_exp_get_language() {
        #[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_EXP_CTXT_PTR {
                for n_exp in 0..GEN_NB_XML_EXP_NODE_PTR {
                    for n_lang_list in 0..GEN_NB_CONST_XML_CHAR_PTR_PTR {
                        for n_len in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_exp_ctxt_ptr(n_ctxt, 0);
                            let exp = gen_xml_exp_node_ptr(n_exp, 1);
                            let lang_list = gen_const_xml_char_ptr_ptr(n_lang_list, 2);
                            let len = gen_int(n_len, 3);

                            let ret_val = xml_exp_get_language(
                                ctxt,
                                exp,
                                lang_list as *mut *const XmlChar,
                                len,
                            );
                            desret_int(ret_val);
                            des_xml_exp_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_xml_exp_node_ptr(n_exp, exp, 1);
                            des_const_xml_char_ptr_ptr(
                                n_lang_list,
                                lang_list as *mut *const XmlChar,
                                2,
                            );
                            des_int(n_len, len, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlExpGetLanguage",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlExpGetLanguage()"
                                );
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_exp);
                                eprint!(" {}", n_lang_list);
                                eprintln!(" {}", n_len);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_exp_get_start() {
        #[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_EXP_CTXT_PTR {
                for n_exp in 0..GEN_NB_XML_EXP_NODE_PTR {
                    for n_tok_list in 0..GEN_NB_CONST_XML_CHAR_PTR_PTR {
                        for n_len in 0..GEN_NB_INT {
                            let mem_base = xml_mem_blocks();
                            let ctxt = gen_xml_exp_ctxt_ptr(n_ctxt, 0);
                            let exp = gen_xml_exp_node_ptr(n_exp, 1);
                            let tok_list = gen_const_xml_char_ptr_ptr(n_tok_list, 2);
                            let len = gen_int(n_len, 3);

                            let ret_val =
                                xml_exp_get_start(ctxt, exp, tok_list as *mut *const XmlChar, len);
                            desret_int(ret_val);
                            des_xml_exp_ctxt_ptr(n_ctxt, ctxt, 0);
                            des_xml_exp_node_ptr(n_exp, exp, 1);
                            des_const_xml_char_ptr_ptr(
                                n_tok_list,
                                tok_list as *mut *const XmlChar,
                                2,
                            );
                            des_int(n_len, len, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlExpGetStart",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(leaks == 0, "{leaks} Leaks are found in xmlExpGetStart()");
                                eprint!(" {}", n_ctxt);
                                eprint!(" {}", n_exp);
                                eprint!(" {}", n_tok_list);
                                eprintln!(" {}", n_len);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_exp_is_nillable() {
        #[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_exp in 0..GEN_NB_XML_EXP_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let exp = gen_xml_exp_node_ptr(n_exp, 0);

                let ret_val = xml_exp_is_nillable(exp);
                desret_int(ret_val);
                des_xml_exp_node_ptr(n_exp, exp, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlExpIsNillable",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlExpIsNillable()");
                    eprintln!(" {}", n_exp);
                }
            }
        }
    }

    #[test]
    fn test_xml_exp_max_token() {
        #[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_expr in 0..GEN_NB_XML_EXP_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let expr = gen_xml_exp_node_ptr(n_expr, 0);

                let ret_val = xml_exp_max_token(expr);
                desret_int(ret_val);
                des_xml_exp_node_ptr(n_expr, expr, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlExpMaxToken",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlExpMaxToken()");
                    eprintln!(" {}", n_expr);
                }
            }
        }
    }

    #[test]
    fn test_xml_exp_new_atom() {

        /* missing type support */
    }

    #[test]
    fn test_xml_exp_new_ctxt() {

        /* missing type support */
    }

    #[test]
    fn test_xml_exp_new_or() {

        /* missing type support */
    }

    #[test]
    fn test_xml_exp_new_range() {

        /* missing type support */
    }

    #[test]
    fn test_xml_exp_new_seq() {

        /* missing type support */
    }

    #[test]
    fn test_xml_exp_parse() {

        /* missing type support */
    }

    #[test]
    fn test_xml_exp_ref() {
        #[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_exp in 0..GEN_NB_XML_EXP_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let exp = gen_xml_exp_node_ptr(n_exp, 0);

                xml_exp_ref(exp);
                des_xml_exp_node_ptr(n_exp, exp, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlExpRef",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlExpRef()");
                    eprintln!(" {}", n_exp);
                }
            }
        }
    }

    #[test]
    fn test_xml_exp_string_derive() {

        /* missing type support */
    }

    #[test]
    fn test_xml_exp_subsume() {
        #[cfg(all(feature = "libxml_regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_EXP_CTXT_PTR {
                for n_exp in 0..GEN_NB_XML_EXP_NODE_PTR {
                    for n_sub in 0..GEN_NB_XML_EXP_NODE_PTR {
                        let mem_base = xml_mem_blocks();
                        let ctxt = gen_xml_exp_ctxt_ptr(n_ctxt, 0);
                        let exp = gen_xml_exp_node_ptr(n_exp, 1);
                        let sub = gen_xml_exp_node_ptr(n_sub, 2);

                        let ret_val = xml_exp_subsume(ctxt, exp, sub);
                        desret_int(ret_val);
                        des_xml_exp_ctxt_ptr(n_ctxt, ctxt, 0);
                        des_xml_exp_node_ptr(n_exp, exp, 1);
                        des_xml_exp_node_ptr(n_sub, sub, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlExpSubsume",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(leaks == 0, "{leaks} Leaks are found in xmlExpSubsume()");
                            eprint!(" {}", n_ctxt);
                            eprint!(" {}", n_exp);
                            eprintln!(" {}", n_sub);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_reg_exec_push_string() {
        #[cfg(feature = "libxml_regexp")]
        unsafe {
            let mut leaks = 0;

            for n_exec in 0..GEN_NB_XML_REG_EXEC_CTXT_PTR {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_data in 0..GEN_NB_USERDATA {
                        let mem_base = xml_mem_blocks();
                        let exec = gen_xml_reg_exec_ctxt_ptr(n_exec, 0);
                        let value = gen_const_xml_char_ptr(n_value, 1);
                        let data = gen_userdata(n_data, 2);

                        let ret_val = xml_reg_exec_push_string(exec, value, data);
                        desret_int(ret_val);
                        des_xml_reg_exec_ctxt_ptr(n_exec, exec, 0);
                        des_const_xml_char_ptr(n_value, value, 1);
                        des_userdata(n_data, data, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlRegExecPushString",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlRegExecPushString()"
                            );
                            eprint!(" {}", n_exec);
                            eprint!(" {}", n_value);
                            eprintln!(" {}", n_data);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_reg_exec_push_string2() {
        #[cfg(feature = "libxml_regexp")]
        unsafe {
            let mut leaks = 0;

            for n_exec in 0..GEN_NB_XML_REG_EXEC_CTXT_PTR {
                for n_value in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    for n_value2 in 0..GEN_NB_CONST_XML_CHAR_PTR {
                        for n_data in 0..GEN_NB_USERDATA {
                            let mem_base = xml_mem_blocks();
                            let exec = gen_xml_reg_exec_ctxt_ptr(n_exec, 0);
                            let value = gen_const_xml_char_ptr(n_value, 1);
                            let value2 = gen_const_xml_char_ptr(n_value2, 2);
                            let data = gen_userdata(n_data, 3);

                            let ret_val = xml_reg_exec_push_string2(exec, value, value2, data);
                            desret_int(ret_val);
                            des_xml_reg_exec_ctxt_ptr(n_exec, exec, 0);
                            des_const_xml_char_ptr(n_value, value, 1);
                            des_const_xml_char_ptr(n_value2, value2, 2);
                            des_userdata(n_data, data, 3);
                            reset_last_error();
                            if mem_base != xml_mem_blocks() {
                                leaks += 1;
                                eprint!(
                                    "Leak of {} blocks found in xmlRegExecPushString2",
                                    xml_mem_blocks() - mem_base
                                );
                                assert!(
                                    leaks == 0,
                                    "{leaks} Leaks are found in xmlRegExecPushString2()"
                                );
                                eprint!(" {}", n_exec);
                                eprint!(" {}", n_value);
                                eprint!(" {}", n_value2);
                                eprintln!(" {}", n_data);
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_reg_new_exec_ctxt() {

        /* missing type support */
    }

    #[test]
    fn test_xml_regexp_compile() {

        /* missing type support */
    }

    #[test]
    fn test_xml_regexp_exec() {
        #[cfg(feature = "libxml_regexp")]
        unsafe {
            let mut leaks = 0;

            for n_comp in 0..GEN_NB_XML_REGEXP_PTR {
                for n_content in 0..GEN_NB_CONST_XML_CHAR_PTR {
                    let mem_base = xml_mem_blocks();
                    let comp = gen_xml_regexp_ptr(n_comp, 0);
                    let content = gen_const_xml_char_ptr(n_content, 1);

                    let ret_val = xml_regexp_exec(comp, content);
                    desret_int(ret_val);
                    des_xml_regexp_ptr(n_comp, comp, 0);
                    des_const_xml_char_ptr(n_content, content, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlRegexpExec",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlRegexpExec()");
                        eprint!(" {}", n_comp);
                        eprintln!(" {}", n_content);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_regexp_is_determinist() {
        #[cfg(feature = "libxml_regexp")]
        unsafe {
            let mut leaks = 0;

            for n_comp in 0..GEN_NB_XML_REGEXP_PTR {
                let mem_base = xml_mem_blocks();
                let comp = gen_xml_regexp_ptr(n_comp, 0);

                let ret_val = xml_regexp_is_determinist(comp);
                desret_int(ret_val);
                des_xml_regexp_ptr(n_comp, comp, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlRegexpIsDeterminist",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlRegexpIsDeterminist()"
                    );
                    eprintln!(" {}", n_comp);
                }
            }
        }
    }

    #[test]
    fn test_xml_regexp_print() {
        #[cfg(feature = "libxml_regexp")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_FILE_PTR {
                for n_regexp in 0..GEN_NB_XML_REGEXP_PTR {
                    let mem_base = xml_mem_blocks();
                    let mut output = gen_file_ptr(n_output, 0).unwrap();
                    let regexp = gen_xml_regexp_ptr(n_regexp, 1);

                    xml_regexp_print(&mut output, regexp);
                    des_xml_regexp_ptr(n_regexp, regexp, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlRegexpPrint",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlRegexpPrint()");
                        eprint!(" {}", n_output);
                        eprintln!(" {}", n_regexp);
                    }
                }
            }
        }
    }
}

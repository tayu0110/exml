//! Provide methods and data structures for handling regular expressions.  
//! This module is based on `libxml/regexp.h`, `regexp.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::{
    ffi::{c_char, c_int, c_uchar, c_uint, c_ulong, c_ushort, CStr},
    mem::{size_of, zeroed},
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
};

use libc::{fprintf, memcpy, memset, printf, size_t, snprintf, strlen, FILE, INT_MAX};

use crate::{
    __xml_raise_error,
    libxml::{
        dict::{xml_dict_lookup, XmlDictPtr},
        globals::{xml_free, xml_malloc, xml_malloc_atomic, xml_realloc},
        parser_internals::xml_string_current_char,
        tree::{xml_buffer_write_char, xml_buffer_write_xml_char, XmlBufferPtr},
        xmlautomata::{
            xml_free_automata, xml_new_automata, XmlAutomata, XmlAutomataPtr, XmlAutomataState,
        },
        xmlerror::XmlParserErrors,
        xmlstring::{xml_str_equal, xml_strchr, xml_strdup, xml_strndup, XmlChar},
        xmlunicode::{
            xmlUCSIsBlock, xmlUCSIsCatC, xmlUCSIsCatCc, xmlUCSIsCatCf, xmlUCSIsCatCo, xmlUCSIsCatL,
            xmlUCSIsCatLl, xmlUCSIsCatLm, xmlUCSIsCatLo, xmlUCSIsCatLt, xmlUCSIsCatLu,
            xmlUCSIsCatM, xmlUCSIsCatMc, xmlUCSIsCatMe, xmlUCSIsCatMn, xmlUCSIsCatN, xmlUCSIsCatNd,
            xmlUCSIsCatNl, xmlUCSIsCatNo, xmlUCSIsCatP, xmlUCSIsCatPc, xmlUCSIsCatPd,
            xmlUCSIsCatPe, xmlUCSIsCatPf, xmlUCSIsCatPi, xmlUCSIsCatPo, xmlUCSIsCatPs,
            xmlUCSIsCatS, xmlUCSIsCatSc, xmlUCSIsCatSk, xmlUCSIsCatSm, xmlUCSIsCatSo, xmlUCSIsCatZ,
            xmlUCSIsCatZl, xmlUCSIsCatZp, xmlUCSIsCatZs,
        },
    },
    IS_CHAR, IS_COMBINING, IS_DIGIT, IS_EXTENDER, IS_LETTER,
};

const SIZE_MAX: size_t = size_t::MAX;
const MAX_PUSH: usize = 10000000;

/*
 * Note: the order of the enums below is significant, do not shuffle
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum XmlRegAtomType {
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlRegQuantType {
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlRegStateType {
    XmlRegexpStartState = 1,
    XmlRegexpFinalState,
    XmlRegexpTransState,
    XmlRegexpSinkState,
    XmlRegexpUnreachState,
}

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlRegMarkedType {
    XmlRegexpMarkNormal = 0,
    XmlRegexpMarkStart,
    XmlRegexpMarkVisited,
}

pub type XmlRegRangePtr = *mut XmlRegRange;
#[repr(C)]
pub struct XmlRegRange {
    neg: c_int, /* 0 normal, 1 not, 2 exclude */
    typ: XmlRegAtomType,
    start: c_int,
    end: c_int,
    block_name: *mut XmlChar,
}

pub type XmlRegState = XmlAutomataState;
pub type XmlRegStatePtr = *mut XmlRegState;

pub type XmlRegAtomPtr = *mut XmlRegAtom;
#[repr(C)]
pub struct XmlRegAtom {
    pub(crate) no: c_int,
    pub(crate) typ: XmlRegAtomType,
    pub(crate) quant: XmlRegQuantType,
    pub(crate) min: c_int,
    pub(crate) max: c_int,
    pub(crate) valuep: *mut c_void,
    pub(crate) valuep2: *mut c_void,
    pub(crate) neg: c_int,
    pub(crate) codepoint: c_int,
    pub(crate) start: XmlRegStatePtr,
    pub(crate) start0: XmlRegStatePtr,
    pub(crate) stop: XmlRegStatePtr,
    max_ranges: c_int,
    nb_ranges: c_int,
    pub(crate) ranges: *mut XmlRegRangePtr,
    pub(crate) data: *mut c_void,
}

pub type XmlRegCounterPtr = *mut XmlRegCounter;
#[repr(C)]
pub struct XmlRegCounter {
    pub(crate) min: c_int,
    pub(crate) max: c_int,
}

pub type XmlRegTransPtr = *mut XmlRegTrans;
#[repr(C)]
pub struct XmlRegTrans {
    atom: XmlRegAtomPtr,
    to: c_int,
    counter: c_int,
    count: c_int,
    nd: c_int,
}

pub type XmlRegParserCtxt = XmlAutomata;
pub type XmlRegParserCtxtPtr = *mut XmlRegParserCtxt;

const AM_AUTOMATA_RNG: usize = 1;

/**
 * xmlRegexpPtr:
 *
 * A libxml regular expression, they can actually be far more complex
 * thank the POSIX regex expressions.
 */
pub type XmlRegexpPtr = *mut XmlRegexp;
#[repr(C)]
pub struct XmlRegexp {
    string: *mut XmlChar,
    nb_states: c_int,
    states: *mut XmlRegStatePtr,
    nb_atoms: c_int,
    atoms: *mut XmlRegAtomPtr,
    nb_counters: c_int,
    counters: *mut XmlRegCounter,
    determinist: c_int,
    flags: c_int,
    /*
     * That's the compact form for determinists automatas
     */
    nbstates: c_int,
    compact: *mut c_int,
    transdata: *mut *mut c_void,
    nbstrings: c_int,
    string_map: *mut *mut XmlChar,
}

pub type XmlRegExecRollbackPtr = *mut XmlRegExecRollback;
#[repr(C)]
pub struct XmlRegExecRollback {
    state: XmlRegStatePtr, /* the current state */
    index: c_int,          /* the index in the input stack */
    nextbranch: c_int,     /* the next transition to explore in that state */
    counts: *mut c_int,    /* save the automata state if it has some */
}

pub type XmlRegInputTokenPtr = *mut XmlRegInputToken;
#[repr(C)]
pub struct XmlRegInputToken {
    value: *mut XmlChar,
    data: *mut c_void,
}

/**
 * xmlRegExecCtxtPtr:
 *
 * A libxml progressive regular expression evaluation context
 */
pub type XmlRegExecCtxtPtr = *mut XmlRegExecCtxt;
#[repr(C)]
pub struct XmlRegExecCtxt {
    status: c_int,      /* execution status != 0 indicate an error */
    determinist: c_int, /* did we find an indeterministic behaviour */
    comp: XmlRegexpPtr, /* the compiled regexp */
    callback: Option<XmlRegExecCallbacks>,
    data: *mut c_void,

    state: XmlRegStatePtr, /* the current state */
    transno: c_int,        /* the current transition on that state */
    transcount: c_int,     /* the number of chars in c_char counted transitions */
    /*
     * A stack of rollback states
     */
    max_rollbacks: c_int,
    nb_rollbacks: c_int,
    rollbacks: *mut XmlRegExecRollback,

    /*
     * The state of the automata if any
     */
    counts: *mut c_int,

    /*
     * The input stack
     */
    input_stack_max: c_int,
    input_stack_nr: c_int,
    index: c_int,
    char_stack: *mut c_int,
    input_string: *const XmlChar,     /* when operating on characters */
    input_stack: XmlRegInputTokenPtr, /* when operating on strings */

    /*
     * error handling
     */
    err_state_no: c_int,       /* the error state number */
    err_state: XmlRegStatePtr, /* the error state */
    err_string: *mut XmlChar,  /* the string raising the error */
    err_counts: *mut c_int,    /* counters at the error state */
    nb_push: c_int,
}

/**
 * xmlRegNewParserCtxt:
 * @string:  the string to parse
 *
 * Allocate a new regexp parser context
 *
 * Returns the new context or NULL in case of error
 */
pub(crate) unsafe extern "C" fn xml_reg_new_parser_ctxt(
    string: *const XmlChar,
) -> XmlRegParserCtxtPtr {
    let ret: XmlRegParserCtxtPtr = xml_malloc(size_of::<XmlRegParserCtxt>()) as XmlRegParserCtxtPtr;
    if ret.is_null() {
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRegParserCtxt>());
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

/**
 * xmlRegexpErrMemory:
 * @extra:  extra information
 *
 * Handle an out of memory condition
 */
unsafe extern "C" fn xml_regexp_err_memory(ctxt: XmlRegParserCtxtPtr, extra: *const c_char) {
    let mut regexp: *const c_char = null();
    if !ctxt.is_null() {
        regexp = (*ctxt).string as _;
        (*ctxt).error = XmlParserErrors::XmlErrNoMemory as _;
    }
    __xml_raise_error!(
        None,
        None,
        null_mut(),
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromRegexp as i32,
        XmlParserErrors::XmlErrNoMemory as i32,
        XmlErrorLevel::XmlErrFatal,
        null_mut(),
        0,
        extra,
        regexp,
        null(),
        0,
        0,
        c"Memory allocation failed : %s\n".as_ptr(),
        extra
    );
}

unsafe extern "C" fn xml_reg_new_state(ctxt: XmlRegParserCtxtPtr) -> XmlRegStatePtr {
    let ret: XmlRegStatePtr = xml_malloc(size_of::<XmlRegState>()) as XmlRegStatePtr;
    if ret.is_null() {
        xml_regexp_err_memory(ctxt, c"allocating state".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRegState>());
    (*ret).typ = XmlRegStateType::XmlRegexpTransState;
    (*ret).mark = XmlRegMarkedType::XmlRegexpMarkNormal;
    ret
}

pub(crate) unsafe extern "C" fn xml_reg_state_push(ctxt: XmlRegParserCtxtPtr) -> XmlRegStatePtr {
    if (*ctxt).nb_states >= (*ctxt).max_states {
        let new_size: size_t = if (*ctxt).max_states != 0 {
            (*ctxt).max_states as size_t * 2
        } else {
            4
        };

        let tmp: *mut XmlRegStatePtr =
            xml_realloc((*ctxt).states as _, new_size * size_of::<XmlRegStatePtr>()) as _;
        if tmp.is_null() {
            xml_regexp_err_memory(ctxt, c"adding state".as_ptr());
            return null_mut();
        }
        (*ctxt).states = tmp;
        (*ctxt).max_states = new_size as _;
    }

    let state: XmlRegStatePtr = xml_reg_new_state(ctxt);
    if state.is_null() {
        return null_mut();
    }

    (*state).no = (*ctxt).nb_states;
    *(*ctxt).states.add((*ctxt).nb_states as _) = state;
    (*ctxt).nb_states += 1;

    state
}

/**
 * xmlRegFreeState:
 * @state:  the regexp state
 *
 * Free a regexp state
 */
unsafe extern "C" fn xml_reg_free_state(state: XmlRegStatePtr) {
    if state.is_null() {
        return;
    }

    if !(*state).trans.is_null() {
        xml_free((*state).trans as _);
    }
    if !(*state).trans_to.is_null() {
        xml_free((*state).trans_to as _);
    }
    xml_free(state as _);
}

/**
 * xmlRegFreeRange:
 * @range:  the regexp range
 *
 * Free a regexp range
 */
unsafe extern "C" fn xml_reg_free_range(range: XmlRegRangePtr) {
    if range.is_null() {
        return;
    }

    if !(*range).block_name.is_null() {
        xml_free((*range).block_name as _);
    }
    xml_free(range as _);
}

/**
 * xmlRegFreeAtom:
 * @atom:  the regexp atom
 *
 * Free a regexp atom
 */
pub(crate) unsafe extern "C" fn xml_reg_free_atom(atom: XmlRegAtomPtr) {
    if atom.is_null() {
        return;
    }

    for i in 0..(*atom).nb_ranges {
        xml_reg_free_range(*(*atom).ranges.add(i as usize));
    }
    if !(*atom).ranges.is_null() {
        xml_free((*atom).ranges as _);
    }
    if matches!((*atom).typ, XmlRegAtomType::XmlRegexpString) && !(*atom).valuep.is_null() {
        xml_free((*atom).valuep as _);
    }
    if matches!((*atom).typ, XmlRegAtomType::XmlRegexpString) && !(*atom).valuep2.is_null() {
        xml_free((*atom).valuep2 as _);
    }
    if matches!((*atom).typ, XmlRegAtomType::XmlRegexpBlockName) && !(*atom).valuep.is_null() {
        xml_free((*atom).valuep as _);
    }
    xml_free(atom as _);
}

/**
 * xmlRegFreeParserCtxt:
 * @ctxt:  the regexp parser context
 *
 * Free a regexp parser context
 */
pub(crate) unsafe extern "C" fn xml_reg_free_parser_ctxt(ctxt: XmlRegParserCtxtPtr) {
    if ctxt.is_null() {
        return;
    }

    if !(*ctxt).string.is_null() {
        xml_free((*ctxt).string as _);
    }
    if !(*ctxt).states.is_null() {
        for i in 0..(*ctxt).nb_states {
            xml_reg_free_state(*(*ctxt).states.add(i as usize));
        }
        xml_free((*ctxt).states as _);
    }
    if !(*ctxt).atoms.is_null() {
        for i in 0..(*ctxt).nb_atoms {
            xml_reg_free_atom(*(*ctxt).atoms.add(i as usize));
        }
        xml_free((*ctxt).atoms as _);
    }
    if !(*ctxt).counters.is_null() {
        xml_free((*ctxt).counters as _);
    }
    xml_free(ctxt as _);
}

macro_rules! CUR_SCHAR {
    ( $s:expr, $l:expr ) => {
        xml_string_current_char(null_mut(), $s, addr_of_mut!($l))
    };
}

/**
 * xmlFAIsChar:
 * @ctxt:  a regexp parser context
 *
 * [10]   Char   ::=   [^.\?*+()|#x5B#x5D]
 */
unsafe extern "C" fn xml_fa_is_char(ctxt: XmlRegParserCtxtPtr) -> c_int {
    let mut len: c_int = 0;

    let cur: c_int = CUR_SCHAR!((*ctxt).cur, len);
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

/**
 * xmlRegNewAtom:
 * @ctxt:  the regexp parser context
 * @type:  the type of atom
 *
 * Allocate a new atom
 *
 * Returns the new atom or NULL in case of error
 */
pub(crate) unsafe extern "C" fn xml_reg_new_atom(
    ctxt: XmlRegParserCtxtPtr,
    typ: Option<XmlRegAtomType>,
) -> XmlRegAtomPtr {
    let ret: XmlRegAtomPtr = xml_malloc(size_of::<XmlRegAtom>()) as XmlRegAtomPtr;
    if ret.is_null() {
        xml_regexp_err_memory(ctxt, c"allocating atom".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRegAtom>());
    (*ret).typ = typ.expect("Invalid value: xmlRegAtomType");
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

/**
 * xmlRegexpErrCompile:
 * @extra:  extra information
 *
 * Handle a compilation failure
 */
unsafe extern "C" fn xml_regexp_err_compile(ctxt: XmlRegParserCtxtPtr, extra: *const c_char) {
    let mut regexp: *const c_char = null();
    let mut idx: c_int = 0;

    if !ctxt.is_null() {
        regexp = (*ctxt).string as _;
        idx = (*ctxt).cur.offset_from((*ctxt).string) as _;
        (*ctxt).error = XmlParserErrors::XmlRegexpCompileError as _;
    }
    __xml_raise_error!(
        None,
        None,
        null_mut(),
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromRegexp as i32,
        XmlParserErrors::XmlRegexpCompileError as i32,
        XmlErrorLevel::XmlErrFatal,
        null_mut(),
        0,
        extra,
        regexp,
        null(),
        idx,
        0,
        c"failed to compile: %s\n".as_ptr(),
        extra
    );
}

macro_rules! ERROR {
    ( $ctxt:expr, $str:expr ) => {
        (*$ctxt).error = XmlParserErrors::XmlRegexpCompileError as _;
        xml_regexp_err_compile($ctxt, $str);
    };
}

unsafe extern "C" fn xml_reg_state_add_trans_to(
    ctxt: XmlRegParserCtxtPtr,
    target: XmlRegStatePtr,
    from: c_int,
) {
    if (*target).max_trans_to == 0 {
        (*target).max_trans_to = 8;
        (*target).trans_to =
            xml_malloc((*target).max_trans_to as usize * size_of::<c_int>()) as *mut c_int;
        if (*target).trans_to.is_null() {
            xml_regexp_err_memory(ctxt, c"adding transition".as_ptr());
            (*target).max_trans_to = 0;
            return;
        }
    } else if (*target).nb_trans_to >= (*target).max_trans_to {
        (*target).max_trans_to *= 2;
        let tmp: *mut c_int = xml_realloc(
            (*target).trans_to as _,
            (*target).max_trans_to as usize * size_of::<c_int>(),
        ) as *mut c_int;
        if tmp.is_null() {
            xml_regexp_err_memory(ctxt, c"adding transition".as_ptr());
            (*target).max_trans_to /= 2;
            return;
        }
        (*target).trans_to = tmp;
    }
    *(*target).trans_to.add((*target).nb_trans_to as usize) = from;
    (*target).nb_trans_to += 1;
}

pub(crate) unsafe extern "C" fn xml_reg_state_add_trans(
    ctxt: XmlRegParserCtxtPtr,
    state: XmlRegStatePtr,
    atom: XmlRegAtomPtr,
    target: XmlRegStatePtr,
    counter: c_int,
    count: c_int,
) {
    if state.is_null() {
        ERROR!(ctxt, c"add state: state is NULL".as_ptr());
        return;
    }
    if target.is_null() {
        ERROR!(ctxt, c"add state: target is NULL".as_ptr());
        return;
    }
    /*
     * Other routines follow the philosophy 'When in doubt, add a transition'
     * so we check here whether such a transition is already present and, if
     * so, silently ignore this request.
     */

    for nrtrans in (0..(*state).nb_trans).rev() {
        let trans: XmlRegTransPtr = (*state).trans.add(nrtrans as usize);
        if (*trans).atom == atom
            && (*trans).to == (*target).no
            && (*trans).counter == counter
            && (*trans).count == count
        {
            // #ifdef DEBUG_REGEXP_GRAPH
            // 	    printf("Ignoring duplicate transition from %d to %d\n",
            // 		    (*state).no, (*target).no);
            // #endif
            return;
        }
    }

    if (*state).max_trans == 0 {
        (*state).max_trans = 8;
        (*state).trans =
            xml_malloc((*state).max_trans as usize * size_of::<XmlRegTrans>()) as *mut XmlRegTrans;
        if (*state).trans.is_null() {
            xml_regexp_err_memory(ctxt, c"adding transition".as_ptr());
            (*state).max_trans = 0;
            return;
        }
    } else if (*state).nb_trans >= (*state).max_trans {
        (*state).max_trans *= 2;
        let tmp: *mut XmlRegTrans = xml_realloc(
            (*state).trans as _,
            (*state).max_trans as usize * size_of::<XmlRegTrans>(),
        ) as *mut XmlRegTrans;
        if tmp.is_null() {
            xml_regexp_err_memory(ctxt, c"adding transition".as_ptr());
            (*state).max_trans /= 2;
            return;
        }
        (*state).trans = tmp;
    }
    // #ifdef DEBUG_REGEXP_GRAPH
    //     printf("Add trans from %d to %d ", (*state).no, (*target).no);
    //     if (count == REGEXP_ALL_COUNTER)
    // 	printf("all transition\n");
    //     else if (count >= 0)
    // 	printf("count based %d\n", count);
    //     else if (counter >= 0)
    // 	printf("counted %d\n", counter);
    //     else if (atom.is_null())
    // 	printf("epsilon transition\n");
    //     else if (atom != NULL)
    //         xmlRegPrintAtom(stdout, atom);
    // #endif

    (*(*state).trans.add((*state).nb_trans as usize)).atom = atom;
    (*(*state).trans.add((*state).nb_trans as usize)).to = (*target).no;
    (*(*state).trans.add((*state).nb_trans as usize)).counter = counter;
    (*(*state).trans.add((*state).nb_trans as usize)).count = count;
    (*(*state).trans.add((*state).nb_trans as usize)).nd = 0;
    (*state).nb_trans += 1;
    xml_reg_state_add_trans_to(ctxt, target, (*state).no);
}

/**
 * xmlFAGenerateEpsilonTransition:
 * @ctxt:  a regexp parser context
 * @from:  the from state
 * @to:  the target state or NULL for building a new one
 *
 */
pub(crate) unsafe extern "C" fn xml_fa_generate_epsilon_transition(
    ctxt: XmlRegParserCtxtPtr,
    from: XmlRegStatePtr,
    mut to: XmlRegStatePtr,
) -> c_int {
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

/**
 * xmlRegNewRange:
 * @ctxt:  the regexp parser context
 * @neg:  is that negative
 * @type:  the type of range
 * @start:  the start codepoint
 * @end:  the end codepoint
 *
 * Allocate a new regexp range
 *
 * Returns the new range or NULL in case of error
 */
unsafe extern "C" fn xml_reg_new_range(
    ctxt: XmlRegParserCtxtPtr,
    neg: c_int,
    typ: Option<XmlRegAtomType>,
    start: c_int,
    end: c_int,
) -> XmlRegRangePtr {
    let ret: XmlRegRangePtr = xml_malloc(size_of::<XmlRegRange>()) as XmlRegRangePtr;
    if ret.is_null() {
        xml_regexp_err_memory(ctxt, c"allocating range".as_ptr());
        return null_mut();
    }
    (*ret).neg = neg;
    (*ret).typ = typ.expect("Invalid value: xmlRegAtomType");
    (*ret).start = start;
    (*ret).end = end;
    ret
}

unsafe extern "C" fn xml_reg_atom_add_range(
    ctxt: XmlRegParserCtxtPtr,
    atom: XmlRegAtomPtr,
    neg: c_int,
    typ: Option<XmlRegAtomType>,
    start: c_int,
    end: c_int,
    block_name: *mut XmlChar,
) -> XmlRegRangePtr {
    if atom.is_null() {
        ERROR!(ctxt, c"add range: atom is NULL".as_ptr());
        return null_mut();
    }
    if !matches!((*atom).typ, XmlRegAtomType::XmlRegexpRanges) {
        ERROR!(ctxt, c"add range: atom is not ranges".as_ptr());
        return null_mut();
    }
    if (*atom).max_ranges == 0 {
        (*atom).max_ranges = 4;
        (*atom).ranges = xml_malloc((*atom).max_ranges as usize * size_of::<XmlRegRangePtr>())
            as *mut XmlRegRangePtr;
        if (*atom).ranges.is_null() {
            xml_regexp_err_memory(ctxt, c"adding ranges".as_ptr());
            (*atom).max_ranges = 0;
            return null_mut();
        }
    } else if (*atom).nb_ranges >= (*atom).max_ranges {
        (*atom).max_ranges *= 2;
        let tmp: *mut XmlRegRangePtr = xml_realloc(
            (*atom).ranges as _,
            (*atom).max_ranges as usize * size_of::<XmlRegRangePtr>(),
        ) as *mut XmlRegRangePtr;
        if tmp.is_null() {
            xml_regexp_err_memory(ctxt, c"adding ranges".as_ptr());
            (*atom).max_ranges /= 2;
            return null_mut();
        }
        (*atom).ranges = tmp;
    }
    let range: XmlRegRangePtr = xml_reg_new_range(ctxt, neg, typ, start, end);
    if range.is_null() {
        return null_mut();
    }
    (*range).block_name = block_name;
    *(*atom).ranges.add((*atom).nb_ranges as _) = range;
    (*atom).nb_ranges += 1;

    range
}

/**
 * xmlFAParseCharProp:
 * @ctxt:  a regexp parser context
 *
 * [27]   charProp   ::=   IsCategory | IsBlock
 * [28]   IsCategory ::= Letters | Marks | Numbers | Punctuation |
 *                       Separators | Symbols | Others
 * [29]   Letters   ::=   'L' [ultmo]?
 * [30]   Marks   ::=   'M' [nce]?
 * [31]   Numbers   ::=   'N' [dlo]?
 * [32]   Punctuation   ::=   'P' [cdseifo]?
 * [33]   Separators   ::=   'Z' [slp]?
 * [34]   Symbols   ::=   'S' [mcko]?
 * [35]   Others   ::=   'C' [cfon]?
 * [36]   IsBlock   ::=   'Is' [a-zA-Z0-9#x2D]+
 */
unsafe extern "C" fn xml_fa_parse_char_prop(ctxt: XmlRegParserCtxtPtr) {
    let mut cur: c_int;
    let typ: Option<XmlRegAtomType>;
    let mut block_name: *mut XmlChar = null_mut();

    cur = CUR!(ctxt) as _;
    if cur == b'L' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b'u' as _ {
            NEXT!(ctxt);
            typ = Some(XmlRegAtomType::XmlRegexpLetterUppercase);
        } else if cur == b'l' as _ {
            NEXT!(ctxt);
            typ = Some(XmlRegAtomType::XmlRegexpLetterLowercase);
        } else if cur == b't' as _ {
            NEXT!(ctxt);
            typ = Some(XmlRegAtomType::XmlRegexpLetterTitlecase);
        } else if cur == b'm' as _ {
            NEXT!(ctxt);
            typ = Some(XmlRegAtomType::XmlRegexpLetterModifier);
        } else if cur == b'o' as _ {
            NEXT!(ctxt);
            typ = Some(XmlRegAtomType::XmlRegexpLetterOthers);
        } else {
            typ = Some(XmlRegAtomType::XmlRegexpLetter);
        }
    } else if cur == b'M' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b'n' as _ {
            NEXT!(ctxt);
            /* nonspacing */
            typ = Some(XmlRegAtomType::XmlRegexpMarkNonspacing);
        } else if cur == b'c' as _ {
            NEXT!(ctxt);
            /* spacing combining */
            typ = Some(XmlRegAtomType::XmlRegexpMarkSpacecombining);
        } else if cur == b'e' as _ {
            NEXT!(ctxt);
            /* enclosing */
            typ = Some(XmlRegAtomType::XmlRegexpMarkEnclosing);
        } else {
            /* all marks */
            typ = Some(XmlRegAtomType::XmlRegexpMark);
        }
    } else if cur == b'N' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b'd' as _ {
            NEXT!(ctxt);
            /* digital */
            typ = Some(XmlRegAtomType::XmlRegexpNumberDecimal);
        } else if cur == b'l' as _ {
            NEXT!(ctxt);
            /* letter */
            typ = Some(XmlRegAtomType::XmlRegexpNumberLetter);
        } else if cur == b'o' as _ {
            NEXT!(ctxt);
            /* other */
            typ = Some(XmlRegAtomType::XmlRegexpNumberOthers);
        } else {
            /* all numbers */
            typ = Some(XmlRegAtomType::XmlRegexpNumber);
        }
    } else if cur == b'P' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b'c' as _ {
            NEXT!(ctxt);
            /* connector */
            typ = Some(XmlRegAtomType::XmlRegexpPunctConnector);
        } else if cur == b'd' as _ {
            NEXT!(ctxt);
            /* dash */
            typ = Some(XmlRegAtomType::XmlRegexpPunctDash);
        } else if cur == b's' as _ {
            NEXT!(ctxt);
            /* open */
            typ = Some(XmlRegAtomType::XmlRegexpPunctOpen);
        } else if cur == b'e' as _ {
            NEXT!(ctxt);
            /* close */
            typ = Some(XmlRegAtomType::XmlRegexpPunctClose);
        } else if cur == b'i' as _ {
            NEXT!(ctxt);
            /* initial quote */
            typ = Some(XmlRegAtomType::XmlRegexpPunctInitquote);
        } else if cur == b'f' as _ {
            NEXT!(ctxt);
            /* final quote */
            typ = Some(XmlRegAtomType::XmlRegexpPunctFinquote);
        } else if cur == b'o' as _ {
            NEXT!(ctxt);
            /* other */
            typ = Some(XmlRegAtomType::XmlRegexpPunctOthers);
        } else {
            /* all punctuation */
            typ = Some(XmlRegAtomType::XmlRegexpPunct);
        }
    } else if cur == b'Z' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b's' as _ {
            NEXT!(ctxt);
            /* space */
            typ = Some(XmlRegAtomType::XmlRegexpSeparSpace);
        } else if cur == b'l' as _ {
            NEXT!(ctxt);
            /* line */
            typ = Some(XmlRegAtomType::XmlRegexpSeparLine);
        } else if cur == b'p' as _ {
            NEXT!(ctxt);
            /* paragraph */
            typ = Some(XmlRegAtomType::XmlRegexpSeparPara);
        } else {
            /* all separators */
            typ = Some(XmlRegAtomType::XmlRegexpSepar);
        }
    } else if cur == b'S' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b'm' as _ {
            NEXT!(ctxt);
            typ = Some(XmlRegAtomType::XmlRegexpSymbolMath);
            /* math */
        } else if cur == b'c' as _ {
            NEXT!(ctxt);
            typ = Some(XmlRegAtomType::XmlRegexpSymbolCurrency);
            /* currency */
        } else if cur == b'k' as _ {
            NEXT!(ctxt);
            typ = Some(XmlRegAtomType::XmlRegexpSymbolModifier);
            /* modifiers */
        } else if cur == b'o' as _ {
            NEXT!(ctxt);
            typ = Some(XmlRegAtomType::XmlRegexpSymbolOthers);
            /* other */
        } else {
            /* all symbols */
            typ = Some(XmlRegAtomType::XmlRegexpSymbol);
        }
    } else if cur == b'C' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur == b'c' as _ {
            NEXT!(ctxt);
            /* control */
            typ = Some(XmlRegAtomType::XmlRegexpOtherControl);
        } else if cur == b'f' as _ {
            NEXT!(ctxt);
            /* format */
            typ = Some(XmlRegAtomType::XmlRegexpOtherFormat);
        } else if cur == b'o' as _ {
            NEXT!(ctxt);
            /* private use */
            typ = Some(XmlRegAtomType::XmlRegexpOtherPrivate);
        } else if cur == b'n' as _ {
            NEXT!(ctxt);
            /* not assigned */
            typ = Some(XmlRegAtomType::XmlRegexpOtherNa);
        } else {
            /* all others */
            typ = Some(XmlRegAtomType::XmlRegexpOther);
        }
    } else if cur == b'I' as _ {
        NEXT!(ctxt);
        cur = CUR!(ctxt) as _;
        if cur != b's' as _ {
            ERROR!(ctxt, c"IsXXXX expected".as_ptr());
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
        typ = Some(XmlRegAtomType::XmlRegexpBlockName);
        block_name = xml_strndup(start, (*ctxt).cur.offset_from(start) as _);
    } else {
        ERROR!(ctxt, c"Unknown char property".as_ptr());
        return;
    }
    if (*ctxt).atom.is_null() {
        (*ctxt).atom = xml_reg_new_atom(ctxt, typ);
        if (*ctxt).atom.is_null() {
            xml_free(block_name as _);
            return;
        }
        (*(*ctxt).atom).valuep = block_name as _;
    } else if matches!((*(*ctxt).atom).typ, XmlRegAtomType::XmlRegexpRanges)
        && xml_reg_atom_add_range(ctxt, (*ctxt).atom, (*ctxt).neg, typ, 0, 0, block_name).is_null()
    {
        xml_free(block_name as _);
    }
}

/************************************************************************
 *									*
 *	Parser for the Schemas Datatype Regular Expressions		*
 *	http://www.w3.org/TR/2001/REC-xmlschema-2-20010502/#regexs	*
 *									*
 ************************************************************************/

unsafe extern "C" fn parse_escaped_codeunit(ctxt: XmlRegParserCtxtPtr) -> c_int {
    let mut val: c_int = 0;
    let mut cur: c_int;
    for _ in 0..4 {
        NEXT!(ctxt);
        val *= 16;
        cur = CUR!(ctxt) as _;
        if cur >= b'0' as c_int && cur <= b'9' as c_int {
            val += cur - b'0' as c_int;
        } else if cur >= b'A' as c_int && cur <= b'F' as c_int {
            val += cur - b'A' as c_int + 10;
        } else if cur >= b'a' as c_int && cur <= b'f' as c_int {
            val += cur - b'a' as c_int + 10;
        } else {
            ERROR!(ctxt, c"Expecting hex digit".as_ptr());
            return -1;
        }
    }
    val
}

unsafe extern "C" fn parse_escaped_codepoint(ctxt: XmlRegParserCtxtPtr) -> c_int {
    let mut val: c_int = parse_escaped_codeunit(ctxt);
    if (0xD800..=0xDBFF).contains(&val) {
        NEXT!(ctxt);
        if CUR!(ctxt) == b'\\' {
            NEXT!(ctxt);
            if CUR!(ctxt) == b'u' {
                let low: c_int = parse_escaped_codeunit(ctxt);
                if (0xDC00..=0xDFFF).contains(&low) {
                    return (val - 0xD800) * 0x400 + (low - 0xDC00) + 0x10000;
                }
            }
        }
        ERROR!(ctxt, c"Invalid low surrogate pair code unit".as_ptr());
        val = -1;
    }
    val
}

/**
 * xmlFAParseCharClassEsc:
 * @ctxt:  a regexp parser context
 *
 * [23] charClassEsc ::= ( SingleCharEsc | MultiCharEsc | catEsc | complEsc )
 * [24] SingleCharEsc ::= '\' [nrt\|.?*+(){}#x2D#x5B#x5D#x5E]
 * [25] catEsc   ::=   '\p{' charProp '}'
 * [26] complEsc ::=   '\P{' charProp '}'
 * [37] MultiCharEsc ::= '.' | ('\' [sSiIcCdDwW])
 */
unsafe extern "C" fn xml_fa_parse_char_class_esc(ctxt: XmlRegParserCtxtPtr) {
    let mut cur: c_int;

    if CUR!(ctxt) == b'.' {
        if (*ctxt).atom.is_null() {
            (*ctxt).atom = xml_reg_new_atom(ctxt, Some(XmlRegAtomType::XmlRegexpAnychar));
        } else if matches!((*(*ctxt).atom).typ, XmlRegAtomType::XmlRegexpRanges) {
            xml_reg_atom_add_range(
                ctxt,
                (*ctxt).atom,
                (*ctxt).neg,
                Some(XmlRegAtomType::XmlRegexpAnychar),
                0,
                0,
                null_mut(),
            );
        }
        NEXT!(ctxt);
        return;
    }
    if CUR!(ctxt) != b'\\' {
        ERROR!(ctxt, c"Escaped sequence: expecting \\".as_ptr());
        return;
    }
    NEXT!(ctxt);
    cur = CUR!(ctxt) as _;
    if cur == b'p' as _ {
        NEXT!(ctxt);
        if CUR!(ctxt) != b'{' {
            ERROR!(ctxt, c"Expecting '{'".as_ptr());
            return;
        }
        NEXT!(ctxt);
        xml_fa_parse_char_prop(ctxt);
        if CUR!(ctxt) != b'}' {
            ERROR!(ctxt, c"Expecting '}'".as_ptr());
            return;
        }
        NEXT!(ctxt);
    } else if cur == b'P' as _ {
        NEXT!(ctxt);
        if CUR!(ctxt) != b'{' {
            ERROR!(ctxt, c"Expecting '{'".as_ptr());
            return;
        }
        NEXT!(ctxt);
        xml_fa_parse_char_prop(ctxt);
        if !(*ctxt).atom.is_null() {
            (*(*ctxt).atom).neg = 1;
        }
        if CUR!(ctxt) != b'}' {
            ERROR!(ctxt, c"Expecting '}'".as_ptr());
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
        /* Non-standard escape sequences:
         *                  Java 1.8|.NET Core 3.1|MSXML 6 */
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
            (*ctxt).atom = xml_reg_new_atom(ctxt, Some(XmlRegAtomType::XmlRegexpCharval));
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
                Some(XmlRegAtomType::XmlRegexpCharval),
                cur,
                cur,
                null_mut(),
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
            (*ctxt).atom = xml_reg_new_atom(ctxt, Some(typ));
        } else if matches!((*(*ctxt).atom).typ, XmlRegAtomType::XmlRegexpRanges) {
            xml_reg_atom_add_range(ctxt, (*ctxt).atom, (*ctxt).neg, Some(typ), 0, 0, null_mut());
        }
    } else {
        ERROR!(
            ctxt,
            c"Wrong escape sequence, misuse of character '\\'".as_ptr()
        );
    }
}

macro_rules! NXT {
    ( $ctxt:expr, $index:expr ) => {
        *(*$ctxt).cur.add($index as usize)
    };
}

/*
 * Need PREV to check on a '-' within a Character Group. May only be used
 * when it's guaranteed that cur is not at the beginning of (*ctxt).string!
 */
macro_rules! PREV {
    ( $ctxt:expr ) => {
        *(*$ctxt).cur.sub(1)
    };
}

/**
 * xmlFAParseCharRange:
 * @ctxt:  a regexp parser context
 *
 * [17]   charRange   ::=     seRange | XmlCharRef | XmlCharIncDash
 * [18]   seRange   ::=   charOrEsc '-' charOrEsc
 * [20]   charOrEsc   ::=   XmlChar | SingleCharEsc
 * [21]   XmlChar   ::=   [^\#x2D#x5B#x5D]
 * [22]   XmlCharIncDash   ::=   [^\#x5B#x5D]
 */
unsafe extern "C" fn xml_fa_parse_char_range(ctxt: XmlRegParserCtxtPtr) {
    let mut cur: c_int;
    let mut len: c_int = 0;
    let start: c_int;
    let mut end: c_int;

    if CUR!(ctxt) == b'\0' {
        ERROR!(ctxt, c"Expecting ']'".as_ptr());
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
                ERROR!(ctxt, c"Invalid escape value".as_ptr());
                return;
            }
        }
        end = start;
        len = 1;
    } else if cur != 0x5B && cur != 0x5D {
        start = CUR_SCHAR!((*ctxt).cur, len);
        end = start;
    } else {
        ERROR!(ctxt, c"Expecting a char range".as_ptr());
        return;
    }
    /*
     * Since we are "inside" a range, we can assume (*ctxt).cur is past
     * the start of (*ctxt).string, and PREV should be safe
     */
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
            Some(XmlRegAtomType::XmlRegexpCharval),
            start,
            end,
            null_mut(),
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
                ERROR!(ctxt, c"Invalid escape value".as_ptr());
                return;
            }
        }
        len = 1;
    } else if cur != '\0' as i32 && cur != 0x5B && cur != 0x5D {
        end = CUR_SCHAR!((*ctxt).cur, len);
    } else {
        ERROR!(ctxt, c"Expecting the end of a char range".as_ptr());
        return;
    }

    /* TODO check that the values are acceptable character ranges for XML */
    if end < start {
        ERROR!(ctxt, c"End of range is before start of range".as_ptr());
    } else {
        NEXTL!(ctxt, len);
        xml_reg_atom_add_range(
            ctxt,
            (*ctxt).atom,
            (*ctxt).neg,
            Some(XmlRegAtomType::XmlRegexpCharval),
            start,
            end,
            null_mut(),
        );
    }
}

/**
 * xmlFAParsePosCharGroup:
 * @ctxt:  a regexp parser context
 *
 * [14]   posCharGroup ::= ( charRange | charClassEsc  )+
 */
unsafe extern "C" fn xml_fa_parse_pos_char_group(ctxt: XmlRegParserCtxtPtr) {
    while {
        if CUR!(ctxt) == b'\\' {
            xml_fa_parse_char_class_esc(ctxt);
        } else {
            xml_fa_parse_char_range(ctxt);
        }
        CUR!(ctxt) != b']' && CUR!(ctxt) != b'-' && CUR!(ctxt) != 0 && (*ctxt).error == 0
    } {}
}

/**
 * xmlFAParseCharGroup:
 * @ctxt:  a regexp parser context
 *
 * [13]   charGroup    ::= posCharGroup | negCharGroup | charClassSub
 * [15]   negCharGroup ::= '^' posCharGroup
 * [16]   charClassSub ::= ( posCharGroup | negCharGroup ) '-' charClassExpr
 * [12]   charClassExpr ::= '[' charGroup ']'
 */
unsafe extern "C" fn xml_fa_parse_char_group(ctxt: XmlRegParserCtxtPtr) {
    let neg: c_int = (*ctxt).neg;

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
                ERROR!(ctxt, c"charClassExpr: ']' expected".as_ptr());
            }
            break;
        } else {
            xml_fa_parse_pos_char_group(ctxt);
        }
    }
}

/**
 * xmlFAParseCharClass:
 * @ctxt:  a regexp parser context
 *
 * [11]   charClass   ::=     charClassEsc | charClassExpr
 * [12]   charClassExpr   ::=   '[' charGroup ']'
 */
unsafe extern "C" fn xml_fa_parse_char_class(ctxt: XmlRegParserCtxtPtr) {
    if CUR!(ctxt) == b'[' {
        NEXT!(ctxt);
        (*ctxt).atom = xml_reg_new_atom(ctxt, Some(XmlRegAtomType::XmlRegexpRanges));
        if (*ctxt).atom.is_null() {
            return;
        }
        xml_fa_parse_char_group(ctxt);
        if CUR!(ctxt) == b']' {
            NEXT!(ctxt);
        } else {
            ERROR!(ctxt, c"xmlFAParseCharClass: ']' expected".as_ptr());
        }
    } else {
        xml_fa_parse_char_class_esc(ctxt);
    }
}

/**
 * xmlFAParseAtom:
 * @ctxt:  a regexp parser context
 *
 * [9]   atom   ::=   Char | charClass | ( '(' regExp ')' )
 */
unsafe extern "C" fn xml_fa_parse_atom(ctxt: XmlRegParserCtxtPtr) -> c_int {
    let mut codepoint: c_int;
    let mut len: c_int = 0;

    codepoint = xml_fa_is_char(ctxt);
    if codepoint > 0 {
        (*ctxt).atom = xml_reg_new_atom(ctxt, Some(XmlRegAtomType::XmlRegexpCharval));
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
            ERROR!(
                ctxt,
                c"xmlFAParseAtom: maximum nesting depth exceeded".as_ptr()
            );
            return -1;
        }
        /*
         * this extra Epsilon transition is needed if we count with 0 allowed
         * unfortunately this can't be known at that point
         */
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
            ERROR!(ctxt, c"xmlFAParseAtom: expecting ')'".as_ptr());
        }
        (*ctxt).atom = xml_reg_new_atom(ctxt, Some(XmlRegAtomType::XmlRegexpSubreg));
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

/**
 * xmlFAParseQuantExact:
 * @ctxt:  a regexp parser context
 *
 * [8]   QuantExact   ::=   [0-9]+
 *
 * Returns 0 if success or -1 in case of error
 */
unsafe extern "C" fn xml_fa_parse_quant_exact(ctxt: XmlRegParserCtxtPtr) -> c_int {
    let mut ret: c_int = 0;
    let mut ok: c_int = 0;
    let mut overflow: c_int = 0;

    while CUR!(ctxt) >= b'0' && CUR!(ctxt) <= b'9' {
        if ret > INT_MAX / 10 {
            overflow = 1;
        } else {
            let digit: c_int = CUR!(ctxt) as i32 - b'0' as i32;

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

/**
 * xmlFAParseQuantifier:
 * @ctxt:  a regexp parser context
 *
 * [4]   quantifier   ::=   [?*+] | ( '{' quantity '}' )
 * [5]   quantity   ::=   quantRange | quantMin | QuantExact
 * [6]   quantRange   ::=   QuantExact ',' QuantExact
 * [7]   quantMin   ::=   QuantExact ','
 * [8]   QuantExact   ::=   [0-9]+
 */
unsafe extern "C" fn xml_fa_parse_quantifier(ctxt: XmlRegParserCtxtPtr) -> c_int {
    let mut cur: c_int;

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
        let mut min: c_int = 0;
        let mut max: c_int = 0;

        NEXT!(ctxt);
        cur = xml_fa_parse_quant_exact(ctxt);
        if cur >= 0 {
            min = cur;
        } else {
            ERROR!(ctxt, c"Improper quantifier".as_ptr());
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
                    ERROR!(ctxt, c"Improper quantifier".as_ptr());
                }
            }
        }
        if CUR!(ctxt) == b'}' {
            NEXT!(ctxt);
        } else {
            ERROR!(ctxt, c"Unterminated quantifier".as_ptr());
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

/**
 * xmlFAParsePiece:
 * @ctxt:  a regexp parser context
 *
 * [3]   piece   ::=   atom quantifier?
 */
unsafe extern "C" fn xml_fa_parse_piece(ctxt: XmlRegParserCtxtPtr) -> c_int {
    (*ctxt).atom = null_mut();
    let ret: c_int = xml_fa_parse_atom(ctxt);
    if ret == 0 {
        return 0;
    }
    if (*ctxt).atom.is_null() {
        ERROR!(ctxt, c"internal: no atom generated".as_ptr());
    }
    xml_fa_parse_quantifier(ctxt);
    1
}

/**
 * xmlRegCopyRange:
 * @range:  the regexp range
 *
 * Copy a regexp range
 *
 * Returns the new copy or NULL in case of error.
 */
unsafe extern "C" fn xml_reg_copy_range(
    ctxt: XmlRegParserCtxtPtr,
    range: XmlRegRangePtr,
) -> XmlRegRangePtr {
    if range.is_null() {
        return null_mut();
    }

    let ret: XmlRegRangePtr = xml_reg_new_range(
        ctxt,
        (*range).neg,
        Some((*range).typ),
        (*range).start,
        (*range).end,
    );
    if ret.is_null() {
        return null_mut();
    }
    if !(*range).block_name.is_null() {
        (*ret).block_name = xml_strdup((*range).block_name);
        if (*ret).block_name.is_null() {
            xml_regexp_err_memory(ctxt, c"allocating range".as_ptr());
            xml_reg_free_range(ret);
            return null_mut();
        }
    }
    ret
}

/**
 * xmlRegCopyAtom:
 * @ctxt:  the regexp parser context
 * @atom:  the original atom
 *
 * Allocate a new regexp range
 *
 * Returns the new atom or NULL in case of error
 */
unsafe extern "C" fn xml_reg_copy_atom(
    ctxt: XmlRegParserCtxtPtr,
    atom: XmlRegAtomPtr,
) -> XmlRegAtomPtr {
    let ret: XmlRegAtomPtr = xml_malloc(size_of::<XmlRegAtom>()) as XmlRegAtomPtr;
    if ret.is_null() {
        xml_regexp_err_memory(ctxt, c"copying atom".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRegAtom>());
    (*ret).typ = (*atom).typ;
    (*ret).quant = (*atom).quant;
    (*ret).min = (*atom).min;
    (*ret).max = (*atom).max;
    if (*atom).nb_ranges > 0 {
        (*ret).ranges = xml_malloc(size_of::<XmlRegRangePtr>() * (*atom).nb_ranges as usize)
            as *mut XmlRegRangePtr;
        if (*ret).ranges.is_null() {
            xml_regexp_err_memory(ctxt, c"copying atom".as_ptr());
            // goto error;
            xml_reg_free_atom(ret);
            return null_mut();
        }
        for i in 0..(*atom).nb_ranges {
            *(*ret).ranges.add(i as usize) =
                xml_reg_copy_range(ctxt, *(*atom).ranges.add(i as usize));
            if (*(*ret).ranges.add(i as usize)).is_null() {
                // goto error;
                xml_reg_free_atom(ret);
                return null_mut();
            }
            (*ret).nb_ranges = i + 1;
        }
    }
    ret

    // error:
    //     xmlRegFreeAtom(ret);
    //     return(NULL);
}

pub(crate) unsafe extern "C" fn xml_reg_get_counter(ctxt: XmlRegParserCtxtPtr) -> c_int {
    if (*ctxt).max_counters == 0 {
        (*ctxt).max_counters = 4;
        (*ctxt).counters = xml_malloc((*ctxt).max_counters as usize * size_of::<XmlRegCounter>())
            as *mut XmlRegCounter;
        if (*ctxt).counters.is_null() {
            xml_regexp_err_memory(ctxt, c"allocating counter".as_ptr());
            (*ctxt).max_counters = 0;
            return -1;
        }
    } else if (*ctxt).nb_counters >= (*ctxt).max_counters {
        (*ctxt).max_counters *= 2;
        let tmp: *mut XmlRegCounter = xml_realloc(
            (*ctxt).counters as _,
            (*ctxt).max_counters as usize * size_of::<XmlRegCounter>(),
        ) as *mut XmlRegCounter;
        if tmp.is_null() {
            xml_regexp_err_memory(ctxt, c"allocating counter".as_ptr());
            (*ctxt).max_counters /= 2;
            return -1;
        }
        (*ctxt).counters = tmp;
    }
    (*(*ctxt).counters.add((*ctxt).nb_counters as usize)).min = -1;
    (*(*ctxt).counters.add((*ctxt).nb_counters as usize)).max = -1;
    let res = (*ctxt).nb_counters;
    (*ctxt).nb_counters += 1;
    res
}

/**
 * xmlFAGenerateCountedEpsilonTransition:
 * @ctxt:  a regexp parser context
 * @from:  the from state
 * @to:  the target state or NULL for building a new one
 * counter:  the counter for that transition
 *
 */
pub(crate) unsafe extern "C" fn xml_fa_generate_counted_epsilon_transition(
    ctxt: XmlRegParserCtxtPtr,
    from: XmlRegStatePtr,
    mut to: XmlRegStatePtr,
    counter: c_int,
) -> c_int {
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

/**
 * xmlFAGenerateCountedTransition:
 * @ctxt:  a regexp parser context
 * @from:  the from state
 * @to:  the target state or NULL for building a new one
 * counter:  the counter for that transition
 *
 */
pub(crate) unsafe extern "C" fn xml_fa_generate_counted_transition(
    ctxt: XmlRegParserCtxtPtr,
    from: XmlRegStatePtr,
    mut to: XmlRegStatePtr,
    counter: c_int,
) -> c_int {
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

pub(crate) unsafe extern "C" fn xml_reg_atom_push(
    ctxt: XmlRegParserCtxtPtr,
    atom: XmlRegAtomPtr,
) -> c_int {
    if atom.is_null() {
        ERROR!(ctxt, c"atom push: atom is NULL".as_ptr());
        return -1;
    }
    if (*ctxt).nb_atoms >= (*ctxt).max_atoms {
        let new_size: size_t = if (*ctxt).max_atoms != 0 {
            (*ctxt).max_atoms as usize * 2
        } else {
            4
        };

        let tmp: *mut XmlRegAtomPtr =
            xml_realloc((*ctxt).atoms as _, new_size * size_of::<XmlRegAtomPtr>()) as _;
        if tmp.is_null() {
            xml_regexp_err_memory(ctxt, c"allocating counter".as_ptr());
            return -1;
        }
        (*ctxt).atoms = tmp;
        (*ctxt).max_atoms = new_size as _;
    }
    (*atom).no = (*ctxt).nb_atoms;
    *(*ctxt).atoms.add((*ctxt).nb_atoms as usize) = atom;
    (*ctxt).nb_atoms += 1;
    0
}

/**
 * xmlFAGenerateTransitions:
 * @ctxt:  a regexp parser context
 * @from:  the from state
 * @to:  the target state or NULL for building a new one
 * @atom:  the atom generating the transition
 *
 * Returns 0 if success and -1 in case of error.
 */
pub(crate) unsafe extern "C" fn xml_fa_generate_transitions(
    ctxt: XmlRegParserCtxtPtr,
    from: XmlRegStatePtr,
    mut to: XmlRegStatePtr,
    atom: XmlRegAtomPtr,
) -> c_int {
    let mut nullable: c_int = 0;

    if atom.is_null() {
        ERROR!(ctxt, c"generate transition: atom == NULL".as_ptr());
        return -1;
    }
    if matches!((*atom).typ, XmlRegAtomType::XmlRegexpSubreg) {
        /*
         * this is a subexpression handling one should not need to
         * create a new node except for XML_REGEXP_QUANT_RANGE.
         */
        if !to.is_null()
            && (*atom).stop != to
            && !matches!((*atom).quant, XmlRegQuantType::XmlRegexpQuantRange)
        {
            /*
             * Generate an epsilon transition to link to the target
             */
            xml_fa_generate_epsilon_transition(ctxt, (*atom).stop, to);
            // #ifdef DV
            // 	} else if ((to.is_null()) && ((*atom).quant != XML_REGEXP_QUANT_RANGE) &&
            // 		   ((*atom).quant != XML_REGEXP_QUANT_ONCE)) {
            // 	    to = xmlRegStatePush(ctxt, to);
            //             if (to.is_null())
            //                 return(-1);
            // 	    (*ctxt).state = to;
            // 	    xmlFAGenerateEpsilonTransition(ctxt, (*atom).stop, to);
            // #endif
        }
        match (*atom).quant {
            XmlRegQuantType::XmlRegexpQuantOpt => {
                (*atom).quant = XmlRegQuantType::XmlRegexpQuantOnce;
                /*
                 * transition done to the state after end of atom.
                 *      1. set transition from atom start to new state
                 *      2. set transition from atom end to this state.
                 */
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
                let counter: c_int;
                let inter: XmlRegStatePtr;
                let newstate: XmlRegStatePtr;

                /*
                 * create the final state now if needed
                 */
                if !to.is_null() {
                    newstate = to;
                } else {
                    newstate = xml_reg_state_push(ctxt);
                    if newstate.is_null() {
                        return -1;
                    }
                }

                /*
                 * The principle here is to use counted transition
                 * to avoid explosion in the number of states in the
                 * graph. This is clearly more complex but should not
                 * be exploitable at runtime.
                 */
                if (*atom).min == 0 && (*atom).start0.is_null() {
                    /*
                     * duplicate a transition based on atom to count next
                     * occurrences after 1. We cannot loop to (*atom).start
                     * directly because we need an epsilon transition to
                     * newstate.
                     */
                    /* ???? For some reason it seems we never reach that
                        case, I suppose this got optimized out before when
                    building the automata */
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
                    counter = xml_reg_get_counter(ctxt);
                    if counter < 0 {
                        return -1;
                    }
                    (*(*ctxt).counters.add(counter as usize)).min = (*atom).min - 1;
                    (*(*ctxt).counters.add(counter as usize)).max = (*atom).max - 1;
                    /* count the number of times we see it again */
                    xml_fa_generate_counted_epsilon_transition(ctxt, inter, (*atom).stop, counter);
                    /* allow a way out based on the count */
                    xml_fa_generate_counted_transition(ctxt, inter, newstate, counter);
                    /* and also allow a direct exit for 0 */
                    xml_fa_generate_epsilon_transition(ctxt, (*atom).start, newstate);
                } else {
                    /*
                     * either we need the atom at least once or there
                     * is an (*atom).start0 allowing to easily plug the
                     * epsilon transition.
                     */
                    counter = xml_reg_get_counter(ctxt);
                    if counter < 0 {
                        return -1;
                    }
                    (*(*ctxt).counters.add(counter as usize)).min = (*atom).min - 1;
                    (*(*ctxt).counters.add(counter as usize)).max = (*atom).max - 1;
                    /* allow a way out based on the count */
                    xml_fa_generate_counted_transition(ctxt, (*atom).stop, newstate, counter);
                    /* count the number of times we see it again */
                    xml_fa_generate_counted_epsilon_transition(
                        ctxt,
                        (*atom).stop,
                        (*atom).start,
                        counter,
                    );
                    /* and if needed allow a direct exit for 0 */
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
        /*
         * we can discard the atom and generate an epsilon transition instead
         */
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
        /*
         * Do not pollute the target state by adding transitions from
         * it as it is likely to be the shared target of multiple branches.
         * So isolate with an epsilon transition.
         */

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

/**
 * xmlFAParseBranch:
 * @ctxt:  a regexp parser context
 * @to: optional target to the end of the branch
 *
 * @to is used to optimize by removing duplicate path in automata
 * in expressions like (a|b)(c|d)
 *
 * [2]   branch   ::=   piece*
 */
unsafe extern "C" fn xml_fa_parse_branch(ctxt: XmlRegParserCtxtPtr, to: XmlRegStatePtr) -> c_int {
    let mut previous: XmlRegStatePtr;
    let mut ret: c_int;

    previous = (*ctxt).state;
    ret = xml_fa_parse_piece(ctxt);
    if ret == 0 {
        /* Empty branch */
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

/**
 * xmlFAParseRegExp:
 * @ctxt:  a regexp parser context
 * @top:  is this the top-level expression ?
 *
 * [1]   regExp   ::=     branch  ( '|' branch )*
 */
unsafe extern "C" fn xml_fa_parse_reg_exp(ctxt: XmlRegParserCtxtPtr, top: c_int) {
    /* if not top start should have been generated by an epsilon trans */
    let start: XmlRegStatePtr = (*ctxt).state;
    (*ctxt).end = null_mut();
    xml_fa_parse_branch(ctxt, null_mut());
    if top != 0 {
        // #ifdef DEBUG_REGEXP_GRAPH
        // 	printf("State %d is final\n", (*ctxt).(*state).no);
        // #endif
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

/**
 * xmlFAEliminateSimpleEpsilonTransitions:
 * @ctxt:  a regexp parser context
 *
 * Eliminating general epsilon transitions can get costly in the general
 * algorithm due to the large amount of generated new transitions and
 * associated comparisons. However for simple epsilon transition used just
 * to separate building blocks when generating the automata this can be
 * reduced to state elimination:
 *    - if there exists an epsilon from X to Y
 *    - if there is no other transition from X
 * then X and Y are semantically equivalent and X can be eliminated
 * If X is the start state then make Y the start state, else replace the
 * target of all transitions to X by transitions to Y.
 *
 * If X is a final state, skip it.
 * Otherwise it would be necessary to manipulate counters for this case when
 * eliminating state 2:
 * State 1 has a transition with an atom to state 2.
 * State 2 is final and has an epsilon transition to state 1.
 */
unsafe extern "C" fn xml_fa_eliminate_simple_epsilon_transitions(ctxt: XmlRegParserCtxtPtr) {
    let mut newto: c_int;
    let mut state: XmlRegStatePtr;
    let mut tmp: XmlRegStatePtr;

    for statenr in 0..(*ctxt).nb_states {
        state = *(*ctxt).states.add(statenr as usize);
        if state.is_null() {
            continue;
        }
        if (*state).nb_trans != 1 {
            continue;
        }
        if matches!(
            (*state).typ,
            XmlRegStateType::XmlRegexpUnreachState | XmlRegStateType::XmlRegexpFinalState
        ) {
            continue;
        }
        /* is the only transition out a basic transition */
        if (*(*state).trans.add(0)).atom.is_null()
            && (*(*state).trans.add(0)).to >= 0
            && (*(*state).trans.add(0)).to != statenr
            && (*(*state).trans.add(0)).counter < 0
            && (*(*state).trans.add(0)).count < 0
        {
            newto = (*(*state).trans.add(0)).to;

            if matches!((*state).typ, XmlRegStateType::XmlRegexpStartState) {
                // #ifdef DEBUG_REGEXP_GRAPH
                // 		printf("Found simple epsilon trans from start %d to %d\n",
                // 		       statenr, newto);
                // #endif
            } else {
                // #ifdef DEBUG_REGEXP_GRAPH
                // 		printf("Found simple epsilon trans from %d to %d\n",
                // 		       statenr, newto);
                // #endif
                for i in 0..(*state).nb_trans_to {
                    tmp = *(*ctxt)
                        .states
                        .add(*(*state).trans_to.add(i as usize) as usize);
                    for j in 0..(*tmp).nb_trans {
                        if (*(*tmp).trans.add(j as usize)).to == statenr {
                            // #ifdef DEBUG_REGEXP_GRAPH
                            // 			    printf("Changed transition %d on %d to go to %d\n",
                            // 				   j, (*tmp).no, newto);
                            // #endif
                            (*(*tmp).trans.add(j as usize)).to = -1;
                            xml_reg_state_add_trans(
                                ctxt,
                                tmp,
                                (*(*tmp).trans.add(j as usize)).atom,
                                *(*ctxt).states.add(newto as usize),
                                (*(*tmp).trans.add(j as usize)).counter,
                                (*(*tmp).trans.add(j as usize)).count,
                            );
                        }
                    }
                }
                if matches!((*state).typ, XmlRegStateType::XmlRegexpFinalState) {
                    (*(*(*ctxt).states.add(newto as usize))).typ =
                        XmlRegStateType::XmlRegexpFinalState;
                }
                /* eliminate the transition completely */
                (*state).nb_trans = 0;

                (*state).typ = XmlRegStateType::XmlRegexpUnreachState;
            }
        }
    }
}

/**
 * xmlFAReduceEpsilonTransitions:
 * @ctxt:  a regexp parser context
 * @fromnr:  the from state
 * @tonr:  the to state
 * @counter:  should that transition be associated to a counted
 *
 */
unsafe extern "C" fn xml_fa_reduce_epsilon_transitions(
    ctxt: XmlRegParserCtxtPtr,
    fromnr: c_int,
    tonr: c_int,
    counter: c_int,
) {
    // #ifdef DEBUG_REGEXP_GRAPH
    //     printf("xmlFAReduceEpsilonTransitions(%d, %d)\n", fromnr, tonr);
    // #endif
    let from: XmlRegStatePtr = *(*ctxt).states.add(fromnr as usize);
    if from.is_null() {
        return;
    }
    let to: XmlRegStatePtr = *(*ctxt).states.add(tonr as usize);
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
        // #ifdef DEBUG_REGEXP_GRAPH
        // 	printf("State %d is final, so %d becomes final\n", tonr, fromnr);
        // #endif
        (*from).typ = XmlRegStateType::XmlRegexpFinalState;
    }
    for transnr in 0..(*to).nb_trans {
        if (*(*to).trans.add(transnr as usize)).to < 0 {
            continue;
        }
        if (*(*to).trans.add(transnr as usize)).atom.is_null() {
            /*
             * Don't remove counted transitions
             * Don't loop either
             */
            if (*(*to).trans.add(transnr as usize)).to != fromnr {
                if (*(*to).trans.add(transnr as usize)).count >= 0 {
                    let newto: c_int = (*(*to).trans.add(transnr as usize)).to;

                    xml_reg_state_add_trans(
                        ctxt,
                        from,
                        null_mut(),
                        *(*ctxt).states.add(newto as usize),
                        -1,
                        (*(*to).trans.add(transnr as usize)).count,
                    );
                } else {
                    // #ifdef DEBUG_REGEXP_GRAPH
                    // 		    printf("Found epsilon trans %d from %d to %d\n",
                    // 			   transnr, tonr, (*(*to).trans.add(transnr as usize)).to);
                    // #endif
                    if (*(*to).trans.add(transnr as usize)).counter >= 0 {
                        xml_fa_reduce_epsilon_transitions(
                            ctxt,
                            fromnr,
                            (*(*to).trans.add(transnr as usize)).to,
                            (*(*to).trans.add(transnr as usize)).counter,
                        );
                    } else {
                        xml_fa_reduce_epsilon_transitions(
                            ctxt,
                            fromnr,
                            (*(*to).trans.add(transnr as usize)).to,
                            counter,
                        );
                    }
                }
            }
        } else {
            let newto: c_int = (*(*to).trans.add(transnr as usize)).to;

            if (*(*to).trans.add(transnr as usize)).counter >= 0 {
                xml_reg_state_add_trans(
                    ctxt,
                    from,
                    (*(*to).trans.add(transnr as usize)).atom,
                    *(*ctxt).states.add(newto as usize),
                    (*(*to).trans.add(transnr as usize)).counter,
                    -1,
                );
            } else {
                xml_reg_state_add_trans(
                    ctxt,
                    from,
                    (*(*to).trans.add(transnr as usize)).atom,
                    *(*ctxt).states.add(newto as usize),
                    counter,
                    -1,
                );
            }
        }
    }
    (*to).mark = XmlRegMarkedType::XmlRegexpMarkNormal;
}

/**
 * xmlFAEliminateEpsilonTransitions:
 * @ctxt:  a regexp parser context
 *
 */
pub(crate) unsafe extern "C" fn xml_fa_eliminate_epsilon_transitions(ctxt: XmlRegParserCtxtPtr) {
    let mut state: XmlRegStatePtr;
    let mut has_epsilon: c_int;

    if (*ctxt).states.is_null() {
        return;
    }

    /*
     * Eliminate simple epsilon transition and the associated unreachable
     * states.
     */
    xml_fa_eliminate_simple_epsilon_transitions(ctxt);
    for statenr in 0..(*ctxt).nb_states {
        state = *(*ctxt).states.add(statenr as usize);
        if !state.is_null() && matches!((*state).typ, XmlRegStateType::XmlRegexpUnreachState) {
            // #ifdef DEBUG_REGEXP_GRAPH
            // 	    printf("Removed unreachable state %d\n", statenr);
            // #endif
            xml_reg_free_state(state);
            *(*ctxt).states.add(statenr as usize) = null_mut();
        }
    }

    has_epsilon = 0;

    /*
     * Build the completed transitions bypassing the epsilons
     * Use a marking algorithm to avoid loops
     * Mark sink states too.
     * Process from the latest states backward to the start when
     * there is long cascading epsilon chains this minimize the
     * recursions and transition compares when adding the new ones
     */
    for statenr in (0..(*ctxt).nb_states).rev() {
        state = *(*ctxt).states.add(statenr as usize);
        if state.is_null() {
            continue;
        }
        if (*state).nb_trans == 0 && !matches!((*state).typ, XmlRegStateType::XmlRegexpFinalState) {
            (*state).typ = XmlRegStateType::XmlRegexpSinkState;
        }
        for transnr in 0..(*state).nb_trans {
            if (*(*state).trans.add(transnr as usize)).atom.is_null()
                && (*(*state).trans.add(transnr as usize)).to >= 0
            {
                if (*(*state).trans.add(transnr as usize)).to == statenr {
                    (*(*state).trans.add(transnr as usize)).to = -1;
                // #ifdef DEBUG_REGEXP_GRAPH
                // 		    printf("Removed loopback epsilon trans %d on %d\n",
                // 			   transnr, statenr);
                // #endif
                } else if (*(*state).trans.add(transnr as usize)).count < 0 {
                    let newto: c_int = (*(*state).trans.add(transnr as usize)).to;

                    // #ifdef DEBUG_REGEXP_GRAPH
                    // 		    printf("Found epsilon trans %d from %d to %d\n",
                    // 			   transnr, statenr, newto);
                    // #endif
                    has_epsilon = 1;
                    (*(*state).trans.add(transnr as usize)).to = -2;
                    (*state).mark = XmlRegMarkedType::XmlRegexpMarkStart;
                    xml_fa_reduce_epsilon_transitions(
                        ctxt,
                        statenr,
                        newto,
                        (*(*state).trans.add(transnr as usize)).counter,
                    );
                    (*state).mark = XmlRegMarkedType::XmlRegexpMarkNormal;
                    // #ifdef DEBUG_REGEXP_GRAPH
                    // 		} else {
                    // 		    printf("Found counted transition %d on %d\n",
                    // 			   transnr, statenr);
                    // #endif
                }
            }
        }
    }
    /*
     * Eliminate the epsilon transitions
     */
    if has_epsilon != 0 {
        for statnr in 0..(*ctxt).nb_states {
            state = *(*ctxt).states.add(statnr as usize);
            if state.is_null() {
                continue;
            }
            for transnr in 0..(*state).nb_trans {
                let trans: XmlRegTransPtr = (*state).trans.add(transnr as usize);
                if (*trans).atom.is_null() && (*trans).count < 0 && (*trans).to >= 0 {
                    (*trans).to = -1;
                }
            }
        }
    }

    /*
     * Use this pass to detect unreachable states too
     */
    for statenr in 0..(*ctxt).nb_states {
        state = *(*ctxt).states.add(statenr as usize);
        if !state.is_null() {
            (*state).reached = XmlRegMarkedType::XmlRegexpMarkNormal;
        }
    }
    state = *(*ctxt).states.add(0);
    if !state.is_null() {
        (*state).reached = XmlRegMarkedType::XmlRegexpMarkStart;
    }
    while !state.is_null() {
        let mut target: XmlRegStatePtr = null_mut();
        (*state).reached = XmlRegMarkedType::XmlRegexpMarkVisited;
        /*
         * Mark all states reachable from the current reachable state
         */
        for transnr in 0..(*state).nb_trans {
            if (*(*state).trans.add(transnr as usize)).to >= 0
                && (!(*(*state).trans.add(transnr as usize)).atom.is_null()
                    || (*(*state).trans.add(transnr as usize)).count >= 0)
            {
                let newto: c_int = (*(*state).trans.add(transnr as usize)).to;

                if (*(*ctxt).states.add(newto as usize)).is_null() {
                    continue;
                }
                if matches!(
                    (*(*(*ctxt).states.add(newto as usize))).reached,
                    XmlRegMarkedType::XmlRegexpMarkNormal
                ) {
                    (*(*(*ctxt).states.add(newto as usize))).reached =
                        XmlRegMarkedType::XmlRegexpMarkStart;
                    target = *(*ctxt).states.add(newto as usize);
                }
            }
        }

        /*
         * find the next accessible state not explored
         */
        if target.is_null() {
            for statenr in 1..(*ctxt).nb_states {
                state = *(*ctxt).states.add(statenr as usize);
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
    for statenr in 0..(*ctxt).nb_states {
        state = *(*ctxt).states.add(statenr as usize);
        if !state.is_null() && matches!((*state).reached, XmlRegMarkedType::XmlRegexpMarkNormal) {
            // #ifdef DEBUG_REGEXP_GRAPH
            // 	    printf("Removed unreachable state %d\n", statenr);
            // #endif
            xml_reg_free_state(state);
            *(*ctxt).states.add(statenr as usize) = null_mut();
        }
    }
}

/**
 * xmlRegCalloc2:
 * @dim1:  size of first dimension
 * @dim2:  size of second dimension
 * @elemSize:  size of element
 *
 * Allocate a two-dimensional array and set all elements to zero.
 *
 * Returns the new array or NULL in case of error.
 */
unsafe extern "C" fn xml_reg_calloc2(dim1: size_t, dim2: size_t, elem_size: size_t) -> *mut c_void {
    /* Check for overflow */
    if dim2 == 0 || elem_size == 0 || dim1 > SIZE_MAX / dim2 / elem_size {
        return null_mut();
    }
    let total_size: size_t = dim1 * dim2 * elem_size;
    let ret: *mut c_void = xml_malloc(total_size);
    if !ret.is_null() {
        memset(ret, 0, total_size);
    }
    ret
}

/**
 * xmlRegEpxFromParse:
 * @ctxt:  the parser context used to build it
 *
 * Allocate a new regexp and fill it with the result from the parser
 *
 * Returns the new regexp or NULL in case of error
 */
pub(crate) unsafe extern "C" fn xml_reg_epx_from_parse(ctxt: XmlRegParserCtxtPtr) -> XmlRegexpPtr {
    let ret: XmlRegexpPtr = xml_malloc(size_of::<XmlRegexp>()) as XmlRegexpPtr;
    if ret.is_null() {
        xml_regexp_err_memory(ctxt, c"compiling regexp".as_ptr());
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRegexp>());
    (*ret).string = (*ctxt).string;
    (*ret).nb_states = (*ctxt).nb_states;
    (*ret).states = (*ctxt).states;
    (*ret).nb_atoms = (*ctxt).nb_atoms;
    (*ret).atoms = (*ctxt).atoms;
    (*ret).nb_counters = (*ctxt).nb_counters;
    (*ret).counters = (*ctxt).counters;
    (*ret).determinist = (*ctxt).determinist;
    (*ret).flags = (*ctxt).flags;
    if (*ret).determinist == -1 {
        xml_regexp_is_determinist(ret);
    }

    if (*ret).determinist != 0
        && (*ret).nb_counters == 0
        && (*ctxt).negs == 0
        && !(*ret).atoms.is_null()
        && !(*(*ret).atoms.add(0)).is_null()
        && matches!(
            (*(*(*ret).atoms.add(0))).typ,
            XmlRegAtomType::XmlRegexpString
        )
    {
        let mut nbstates: c_int = 0;
        let mut nbatoms: c_int = 0;
        let mut value: *mut XmlChar;
        let mut transdata: *mut *mut c_void;

        /*
         * Switch to a compact representation
         * 1/ counting the effective number of states left
         * 2/ counting the unique number of atoms, and check that
         *    they are all of the string type
         * 3/ build a table state x atom for the transitions
         */

        let state_remap: *mut c_int =
            xml_malloc((*ret).nb_states as usize * size_of::<c_int>()) as _;
        if state_remap.is_null() {
            xml_regexp_err_memory(ctxt, c"compiling regexp".as_ptr());
            xml_free(ret as _);
            return null_mut();
        }
        for i in 0..(*ret).nb_states {
            if !(*(*ret).states.add(i as usize)).is_null() {
                *state_remap.add(i as usize) = nbstates;
                nbstates += 1;
            } else {
                *state_remap.add(i as usize) = -1;
            }
        }
        // #ifdef DEBUG_COMPACTION
        // 	printf("Final: %d states\n", nbstates);
        // #endif
        let string_map: *mut *mut XmlChar =
            xml_malloc((*ret).nb_atoms as usize * size_of::<*mut c_char>()) as _;
        if string_map.is_null() {
            xml_regexp_err_memory(ctxt, c"compiling regexp".as_ptr());
            xml_free(state_remap as _);
            xml_free(ret as _);
            return null_mut();
        }
        let string_remap: *mut c_int =
            xml_malloc((*ret).nb_atoms as usize * size_of::<c_int>()) as _;
        if string_remap.is_null() {
            xml_regexp_err_memory(ctxt, c"compiling regexp".as_ptr());
            xml_free(string_map as _);
            xml_free(state_remap as _);
            xml_free(ret as _);
            return null_mut();
        }
        for i in 0..(*ret).nb_atoms {
            if matches!(
                (*(*(*ret).atoms.add(i as usize))).typ,
                XmlRegAtomType::XmlRegexpString
            ) && matches!(
                (*(*(*ret).atoms.add(i as usize))).quant,
                XmlRegQuantType::XmlRegexpQuantOnce
            ) {
                value = (*(*(*ret).atoms.add(i as usize))).valuep as _;
                let mut k = nbatoms;
                for j in 0..nbatoms {
                    if xml_str_equal(*string_map.add(j as usize), value) != 0 {
                        *string_remap.add(i as usize) = j;
                        k = j;
                        break;
                    }
                }
                if k >= nbatoms {
                    *string_remap.add(i as usize) = nbatoms;
                    *string_map.add(nbatoms as usize) = xml_strdup(value);
                    if (*string_map.add(nbatoms as usize)).is_null() {
                        for i in 0..nbatoms {
                            xml_free(*string_map.add(i as usize) as _);
                        }
                        xml_free(string_remap as _);
                        xml_free(string_map as _);
                        xml_free(state_remap as _);
                        xml_free(ret as _);
                        return null_mut();
                    }
                    nbatoms += 1;
                }
            } else {
                xml_free(state_remap as _);
                xml_free(string_remap as _);
                for i in 0..nbatoms {
                    xml_free(*string_map.add(i as usize) as _);
                }
                xml_free(string_map as _);
                xml_free(ret as _);
                return null_mut();
            }
        }
        // #ifdef DEBUG_COMPACTION
        // 	printf("Final: %d atoms\n", nbatoms);
        // #endif
        let transitions: *mut c_int = xml_reg_calloc2(
            (nbstates + 1) as usize,
            (nbatoms + 1) as usize,
            size_of::<c_int>(),
        ) as *mut c_int;
        if transitions.is_null() {
            xml_free(state_remap as _);
            xml_free(string_remap as _);
            for i in 0..nbatoms {
                xml_free(*string_map.add(i as usize) as _);
            }
            xml_free(string_map as _);
            xml_free(ret as _);
            return null_mut();
        }

        /*
         * Allocate the transition table. The first entry for each
         * state corresponds to the state type.
         */
        transdata = null_mut();

        for i in 0..(*ret).nb_states {
            let mut atomno: c_int;
            let mut targetno: c_int;
            let mut prev: c_int;
            let mut trans: XmlRegTransPtr;

            let stateno: c_int = *state_remap.add(i as usize);
            if stateno == -1 {
                continue;
            }
            let state: XmlRegStatePtr = *(*ret).states.add(i as usize);

            *transitions.add((stateno * (nbatoms + 1)) as usize) = (*state).typ as _;

            for j in 0..(*state).nb_trans {
                trans = (*state).trans.add(j as usize);
                if (*trans).to == -1 || (*trans).atom.is_null() {
                    continue;
                }
                atomno = *string_remap.add((*(*trans).atom).no as usize);
                if !(*(*trans).atom).data.is_null() && transdata.is_null() {
                    transdata = xml_reg_calloc2(
                        nbstates as usize,
                        nbatoms as usize,
                        size_of::<*mut c_void>(),
                    ) as *mut *mut c_void;
                    if transdata.is_null() {
                        xml_regexp_err_memory(ctxt, c"compiling regexp".as_ptr());
                        break;
                    }
                }
                targetno = *state_remap.add((*trans).to as usize);
                /*
                 * if the same atom can generate transitions to 2 different
                 * states then it means the automata is not deterministic and
                 * the compact form can't be used !
                 */
                prev = *transitions.add((stateno * (nbatoms + 1) + atomno + 1) as usize);
                if prev != 0 {
                    if prev != targetno + 1 {
                        (*ret).determinist = 0;
                        // #ifdef DEBUG_COMPACTION
                        // 			printf("Indet: state %d trans %d, atom %d to %d : %d to %d\n",
                        // 			       i, j, (*(*trans).atom).no, (*trans).to, atomno, targetno);
                        // 			printf("       previous to is %d\n", prev);
                        // #endif
                        if !transdata.is_null() {
                            xml_free(transdata as _);
                        }
                        xml_free(transitions as _);
                        xml_free(state_remap as _);
                        xml_free(string_remap as _);
                        for i in 0..nbatoms {
                            xml_free(*string_map.add(i as usize) as _);
                        }
                        xml_free(string_map as _);
                        // goto not_determ;
                        (*ctxt).string = null_mut();
                        (*ctxt).nb_states = 0;
                        (*ctxt).states = null_mut();
                        (*ctxt).nb_atoms = 0;
                        (*ctxt).atoms = null_mut();
                        (*ctxt).nb_counters = 0;
                        (*ctxt).counters = null_mut();
                        return ret;
                    }
                } else {
                    // #if 0
                    // 		    printf("State %d trans %d: atom %d to %d : %d to %d\n",
                    // 			   i, j, (*(*trans).atom).no, (*trans).to, atomno, targetno);
                    // #endif
                    *transitions.add((stateno * (nbatoms + 1) + atomno + 1) as usize) =
                        targetno + 1; /* to avoid 0 */
                    if !transdata.is_null() {
                        *transdata.add((stateno * nbatoms + atomno) as usize) =
                            (*(*trans).atom).data;
                    }
                }
            }
        }
        (*ret).determinist = 1;
        // #ifdef DEBUG_COMPACTION
        // 	/*
        // 	 * Debug
        // 	 */
        // 	for (i = 0;i < nbstates;i++) {
        // 	    for (j = 0;j < nbatoms + 1;j++) {
        //                 printf("%02d ", transitions[i * (nbatoms + 1) + j]);
        // 	    }
        // 	    printf("\n");
        // 	}
        // 	printf("\n");
        // #endif
        /*
         * Cleanup of the old data
         */
        if !(*ret).states.is_null() {
            for i in 0..(*ret).nb_states {
                xml_reg_free_state(*(*ret).states.add(i as usize));
            }
            xml_free((*ret).states as _);
        }
        (*ret).states = null_mut();
        (*ret).nb_states = 0;
        if !(*ret).atoms.is_null() {
            for i in 0..(*ret).nb_atoms {
                xml_reg_free_atom(*(*ret).atoms.add(i as usize));
            }
            xml_free((*ret).atoms as _);
        }
        (*ret).atoms = null_mut();
        (*ret).nb_atoms = 0;

        (*ret).compact = transitions;
        (*ret).transdata = transdata;
        (*ret).string_map = string_map;
        (*ret).nbstrings = nbatoms;
        (*ret).nbstates = nbstates;
        xml_free(state_remap as _);
        xml_free(string_remap as _);
    }
    // not_determ:
    (*ctxt).string = null_mut();
    (*ctxt).nb_states = 0;
    (*ctxt).states = null_mut();
    (*ctxt).nb_atoms = 0;
    (*ctxt).atoms = null_mut();
    (*ctxt).nb_counters = 0;
    (*ctxt).counters = null_mut();
    ret
}

/*
 * The POSIX like API
 */
/**
 * xmlRegexpCompile:
 * @regexp:  a regular expression string
 *
 * Parses a regular expression conforming to XML Schemas Part 2 Datatype
 * Appendix F and builds an automata suitable for testing strings against
 * that regular expression
 *
 * Returns the compiled expression or NULL in case of error
 */
pub unsafe extern "C" fn xml_regexp_compile(regexp: *const XmlChar) -> XmlRegexpPtr {
    let mut ret: XmlRegexpPtr = null_mut();

    let ctxt: XmlRegParserCtxtPtr = xml_reg_new_parser_ctxt(regexp);
    if ctxt.is_null() {
        return null_mut();
    }

    /* initialize the parser */
    (*ctxt).state = xml_reg_state_push(ctxt);
    if (*ctxt).state.is_null() {
        // goto error;
        xml_reg_free_parser_ctxt(ctxt);
        return ret;
    }
    (*ctxt).start = (*ctxt).state;
    (*ctxt).end = null_mut();

    /* parse the expression building an automata */
    xml_fa_parse_reg_exp(ctxt, 1);
    if CUR!(ctxt) != 0 {
        ERROR!(ctxt, c"xmlFAParseRegExp: extra characters".as_ptr());
    }
    if (*ctxt).error != 0 {
        // goto error;
        xml_reg_free_parser_ctxt(ctxt);
        return ret;
    }
    (*ctxt).end = (*ctxt).state;
    (*(*ctxt).start).typ = XmlRegStateType::XmlRegexpStartState;
    (*(*ctxt).end).typ = XmlRegStateType::XmlRegexpFinalState;

    /* remove the Epsilon except for counted transitions */
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

/**
 * xmlRegFreeRegexp:
 * @regexp:  the regexp
 *
 * Free a regexp
 */
pub unsafe extern "C" fn xml_reg_free_regexp(regexp: XmlRegexpPtr) {
    if regexp.is_null() {
        return;
    }

    if !(*regexp).string.is_null() {
        xml_free((*regexp).string as _);
    }
    if !(*regexp).states.is_null() {
        for i in 0..(*regexp).nb_states {
            xml_reg_free_state(*(*regexp).states.add(i as usize));
        }
        xml_free((*regexp).states as _);
    }
    if !(*regexp).atoms.is_null() {
        for i in 0..(*regexp).nb_atoms {
            xml_reg_free_atom(*(*regexp).atoms.add(i as usize));
        }
        xml_free((*regexp).atoms as _);
    }
    if !(*regexp).counters.is_null() {
        xml_free((*regexp).counters as _);
    }
    if !(*regexp).compact.is_null() {
        xml_free((*regexp).compact as _);
    }
    if !(*regexp).transdata.is_null() {
        xml_free((*regexp).transdata as _);
    }
    if !(*regexp).string_map.is_null() {
        for i in 0..(*regexp).nbstrings {
            xml_free(*(*regexp).string_map.add(i as usize) as _);
        }
        xml_free((*regexp).string_map as _);
    }

    xml_free(regexp as _);
}

unsafe extern "C" fn xml_fa_reg_exec_roll_back(exec: XmlRegExecCtxtPtr) {
    if (*exec).nb_rollbacks <= 0 {
        (*exec).status = -1;
        // #ifdef DEBUG_REGEXP_EXEC
        // 	printf("rollback failed on empty stack\n");
        // #endif
        return;
    }
    (*exec).nb_rollbacks -= 1;
    (*exec).state = (*(*exec).rollbacks.add((*exec).nb_rollbacks as usize)).state;
    (*exec).index = (*(*exec).rollbacks.add((*exec).nb_rollbacks as usize)).index;
    (*exec).transno = (*(*exec).rollbacks.add((*exec).nb_rollbacks as usize)).nextbranch;
    if (*(*exec).comp).nb_counters > 0 {
        if (*(*exec).rollbacks.add((*exec).nb_rollbacks as usize))
            .counts
            .is_null()
        {
            // fprintf(stderr, c"exec save: allocation failed".as_ptr());
            eprint!("exec save: allocation failed");
            (*exec).status = -6;
            return;
        }
        if !(*exec).counts.is_null() {
            memcpy(
                (*exec).counts as _,
                (*(*exec).rollbacks.add((*exec).nb_rollbacks as usize)).counts as _,
                (*(*exec).comp).nb_counters as usize * size_of::<c_int>(),
            );
        }
    }

    // #ifdef DEBUG_REGEXP_EXEC
    //     printf("restored ");
    //     xmlFARegDebugExec(exec);
    // #endif
}

unsafe extern "C" fn xmlRegCheckCharacterRange(
    typ: XmlRegAtomType,
    codepoint: c_int,
    mut neg: c_int,
    start: c_int,
    end: c_int,
    block_name: *const XmlChar,
) -> c_int {
    let mut ret: c_int;

    match typ {
        XmlRegAtomType::XmlRegexpString
        | XmlRegAtomType::XmlRegexpSubreg
        | XmlRegAtomType::XmlRegexpRanges
        | XmlRegAtomType::XmlRegexpEpsilon => {
            return -1;
        }
        XmlRegAtomType::XmlRegexpAnychar => {
            ret = (codepoint != '\n' as i32 && codepoint != '\r' as i32) as _;
        }
        XmlRegAtomType::XmlRegexpCharval => {
            ret = (codepoint >= start && codepoint <= end) as _;
        }
        XmlRegAtomType::XmlRegexpNotspace => {
            neg = (neg == 0) as i32;
            ret = (codepoint == '\n' as i32
                || codepoint == '\r' as i32
                || codepoint == '\t' as i32
                || codepoint == ' ' as i32) as _;
        }
        XmlRegAtomType::XmlRegexpAnyspace => {
            ret = (codepoint == '\n' as i32
                || codepoint == '\r' as i32
                || codepoint == '\t' as i32
                || codepoint == ' ' as i32) as _;
        }
        XmlRegAtomType::XmlRegexpNotinitname => {
            neg = (neg == 0) as i32;
            ret = (IS_LETTER!(codepoint as u32)
                || codepoint == '_' as i32
                || codepoint == ':' as i32) as i32;
        }
        XmlRegAtomType::XmlRegexpInitname => {
            ret = (IS_LETTER!(codepoint as u32)
                || codepoint == '_' as i32
                || codepoint == ':' as i32) as i32;
        }
        XmlRegAtomType::XmlRegexpNotnamechar => {
            neg = (neg == 0) as i32;
            ret = (IS_LETTER!(codepoint as u32)
                || IS_DIGIT!(codepoint as u32)
                || codepoint == '.' as i32
                || codepoint == '-' as i32
                || codepoint == '_' as i32
                || codepoint == ':' as i32
                || IS_COMBINING!(codepoint as u32)
                || IS_EXTENDER!(codepoint as u32)) as _;
        }
        XmlRegAtomType::XmlRegexpNamechar => {
            ret = (IS_LETTER!(codepoint as u32)
                || IS_DIGIT!(codepoint as u32)
                || codepoint == '.' as i32
                || codepoint == '-' as i32
                || codepoint == '_' as i32
                || codepoint == ':' as i32
                || IS_COMBINING!(codepoint as u32)
                || IS_EXTENDER!(codepoint as u32)) as _;
        }
        XmlRegAtomType::XmlRegexpNotdecimal => {
            neg = (neg == 0) as i32;
            ret = xmlUCSIsCatNd(codepoint);
        }
        XmlRegAtomType::XmlRegexpDecimal => {
            ret = xmlUCSIsCatNd(codepoint);
        }
        XmlRegAtomType::XmlRegexpRealchar => {
            neg = (neg == 0) as i32;
            ret = xmlUCSIsCatP(codepoint);
            if ret == 0 {
                ret = xmlUCSIsCatZ(codepoint);
            }
            if ret == 0 {
                ret = xmlUCSIsCatC(codepoint);
            }
        }
        XmlRegAtomType::XmlRegexpNotrealchar => {
            ret = xmlUCSIsCatP(codepoint);
            if ret == 0 {
                ret = xmlUCSIsCatZ(codepoint);
            }
            if ret == 0 {
                ret = xmlUCSIsCatC(codepoint);
            }
        }
        XmlRegAtomType::XmlRegexpLetter => {
            ret = xmlUCSIsCatL(codepoint);
        }
        XmlRegAtomType::XmlRegexpLetterUppercase => {
            ret = xmlUCSIsCatLu(codepoint);
        }
        XmlRegAtomType::XmlRegexpLetterLowercase => {
            ret = xmlUCSIsCatLl(codepoint);
        }
        XmlRegAtomType::XmlRegexpLetterTitlecase => {
            ret = xmlUCSIsCatLt(codepoint);
        }
        XmlRegAtomType::XmlRegexpLetterModifier => {
            ret = xmlUCSIsCatLm(codepoint);
        }
        XmlRegAtomType::XmlRegexpLetterOthers => {
            ret = xmlUCSIsCatLo(codepoint);
        }
        XmlRegAtomType::XmlRegexpMark => {
            ret = xmlUCSIsCatM(codepoint);
        }
        XmlRegAtomType::XmlRegexpMarkNonspacing => {
            ret = xmlUCSIsCatMn(codepoint);
        }
        XmlRegAtomType::XmlRegexpMarkSpacecombining => {
            ret = xmlUCSIsCatMc(codepoint);
        }
        XmlRegAtomType::XmlRegexpMarkEnclosing => {
            ret = xmlUCSIsCatMe(codepoint);
        }
        XmlRegAtomType::XmlRegexpNumber => {
            ret = xmlUCSIsCatN(codepoint);
        }
        XmlRegAtomType::XmlRegexpNumberDecimal => {
            ret = xmlUCSIsCatNd(codepoint);
        }
        XmlRegAtomType::XmlRegexpNumberLetter => {
            ret = xmlUCSIsCatNl(codepoint);
        }
        XmlRegAtomType::XmlRegexpNumberOthers => {
            ret = xmlUCSIsCatNo(codepoint);
        }
        XmlRegAtomType::XmlRegexpPunct => {
            ret = xmlUCSIsCatP(codepoint);
        }
        XmlRegAtomType::XmlRegexpPunctConnector => {
            ret = xmlUCSIsCatPc(codepoint);
        }
        XmlRegAtomType::XmlRegexpPunctDash => {
            ret = xmlUCSIsCatPd(codepoint);
        }
        XmlRegAtomType::XmlRegexpPunctOpen => {
            ret = xmlUCSIsCatPs(codepoint);
        }
        XmlRegAtomType::XmlRegexpPunctClose => {
            ret = xmlUCSIsCatPe(codepoint);
        }
        XmlRegAtomType::XmlRegexpPunctInitquote => {
            ret = xmlUCSIsCatPi(codepoint);
        }
        XmlRegAtomType::XmlRegexpPunctFinquote => {
            ret = xmlUCSIsCatPf(codepoint);
        }
        XmlRegAtomType::XmlRegexpPunctOthers => {
            ret = xmlUCSIsCatPo(codepoint);
        }
        XmlRegAtomType::XmlRegexpSepar => {
            ret = xmlUCSIsCatZ(codepoint);
        }
        XmlRegAtomType::XmlRegexpSeparSpace => {
            ret = xmlUCSIsCatZs(codepoint);
        }
        XmlRegAtomType::XmlRegexpSeparLine => {
            ret = xmlUCSIsCatZl(codepoint);
        }
        XmlRegAtomType::XmlRegexpSeparPara => {
            ret = xmlUCSIsCatZp(codepoint);
        }
        XmlRegAtomType::XmlRegexpSymbol => {
            ret = xmlUCSIsCatS(codepoint);
        }
        XmlRegAtomType::XmlRegexpSymbolMath => {
            ret = xmlUCSIsCatSm(codepoint);
        }
        XmlRegAtomType::XmlRegexpSymbolCurrency => {
            ret = xmlUCSIsCatSc(codepoint);
        }
        XmlRegAtomType::XmlRegexpSymbolModifier => {
            ret = xmlUCSIsCatSk(codepoint);
        }
        XmlRegAtomType::XmlRegexpSymbolOthers => {
            ret = xmlUCSIsCatSo(codepoint);
        }
        XmlRegAtomType::XmlRegexpOther => {
            ret = xmlUCSIsCatC(codepoint);
        }
        XmlRegAtomType::XmlRegexpOtherControl => {
            ret = xmlUCSIsCatCc(codepoint);
        }
        XmlRegAtomType::XmlRegexpOtherFormat => {
            ret = xmlUCSIsCatCf(codepoint);
        }
        XmlRegAtomType::XmlRegexpOtherPrivate => {
            ret = xmlUCSIsCatCo(codepoint);
        }
        XmlRegAtomType::XmlRegexpOtherNa => {
            /* ret = xmlUCSIsCatCn(codepoint); */
            /* Seems it doesn't exist anymore in recent Unicode releases */
            ret = 0;
        }
        XmlRegAtomType::XmlRegexpBlockName => {
            ret = xmlUCSIsBlock(codepoint, block_name as *const c_char);
        }
    }
    if neg != 0 {
        return (ret == 0) as i32;
    }
    ret
}

unsafe extern "C" fn xml_reg_check_character(atom: XmlRegAtomPtr, codepoint: c_int) -> c_int {
    let mut ret: c_int;
    let mut range: XmlRegRangePtr;

    if atom.is_null() || !IS_CHAR!(codepoint) {
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
            let mut accept: c_int = 0;

            for i in 0..(*atom).nb_ranges {
                range = *(*atom).ranges.add(i as usize);
                if (*range).neg == 2 {
                    ret = xmlRegCheckCharacterRange(
                        (*range).typ,
                        codepoint,
                        0,
                        (*range).start,
                        (*range).end,
                        (*range).block_name,
                    );
                    if ret != 0 {
                        return 0; /* excluded char */
                    }
                } else if (*range).neg != 0 {
                    ret = xmlRegCheckCharacterRange(
                        (*range).typ,
                        codepoint,
                        0,
                        (*range).start,
                        (*range).end,
                        (*range).block_name,
                    );
                    if ret == 0 {
                        accept = 1;
                    } else {
                        return 0;
                    }
                } else {
                    ret = xmlRegCheckCharacterRange(
                        (*range).typ,
                        codepoint,
                        0,
                        (*range).start,
                        (*range).end,
                        (*range).block_name,
                    );
                    if ret != 0 {
                        accept = 1; /* might still be excluded */
                    }
                }
            }
            return accept;
        }
        XmlRegAtomType::XmlRegexpString => {
            printf(c"TODO: XML_REGEXP_STRING\n".as_ptr());
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
            ret = xmlRegCheckCharacterRange(
                (*atom).typ,
                codepoint,
                0,
                0,
                0,
                (*atom).valuep as *const XmlChar,
            );
            if (*atom).neg != 0 {
                ret = (ret == 0) as i32;
            }
        }
    }
    ret
}

unsafe extern "C" fn xml_fa_reg_exec_save(exec: XmlRegExecCtxtPtr) {
    // #ifdef DEBUG_REGEXP_EXEC
    //     printf("saving ");
    //     (*exec).transno++;
    //     xmlFARegDebugExec(exec);
    //     (*exec).transno--;
    // #endif
    // #ifdef MAX_PUSH
    if (*exec).nb_push as usize > MAX_PUSH {
        return;
    }
    (*exec).nb_push += 1;
    // #endif

    if (*exec).max_rollbacks == 0 {
        (*exec).max_rollbacks = 4;
        (*exec).rollbacks =
            xml_malloc((*exec).max_rollbacks as usize * size_of::<XmlRegExecRollback>())
                as *mut XmlRegExecRollback;
        if (*exec).rollbacks.is_null() {
            xml_regexp_err_memory(null_mut(), c"saving regexp".as_ptr());
            (*exec).max_rollbacks = 0;
            return;
        }
        memset(
            (*exec).rollbacks as _,
            0,
            (*exec).max_rollbacks as usize * size_of::<XmlRegExecRollback>(),
        );
    } else if (*exec).nb_rollbacks >= (*exec).max_rollbacks {
        let mut tmp: *mut XmlRegExecRollback;
        let len: c_int = (*exec).max_rollbacks;

        (*exec).max_rollbacks *= 2;
        tmp = xml_realloc(
            (*exec).rollbacks as _,
            (*exec).max_rollbacks as usize * size_of::<XmlRegExecRollback>(),
        ) as *mut XmlRegExecRollback;
        if tmp.is_null() {
            xml_regexp_err_memory(null_mut(), c"saving regexp".as_ptr());
            (*exec).max_rollbacks /= 2;
            return;
        }
        (*exec).rollbacks = tmp;
        tmp = (*exec).rollbacks.add(len as usize);
        memset(
            tmp as _,
            0,
            ((*exec).max_rollbacks - len) as usize * size_of::<XmlRegExecRollback>(),
        );
    }
    (*(*exec).rollbacks.add((*exec).nb_rollbacks as usize)).state = (*exec).state;
    (*(*exec).rollbacks.add((*exec).nb_rollbacks as usize)).index = (*exec).index;
    (*(*exec).rollbacks.add((*exec).nb_rollbacks as usize)).nextbranch = (*exec).transno + 1;
    if (*(*exec).comp).nb_counters > 0 {
        if (*(*exec).rollbacks.add((*exec).nb_rollbacks as usize))
            .counts
            .is_null()
        {
            (*(*exec).rollbacks.add((*exec).nb_rollbacks as usize)).counts =
                xml_malloc((*(*exec).comp).nb_counters as usize * size_of::<c_int>()) as *mut c_int;
            if (*(*exec).rollbacks.add((*exec).nb_rollbacks as usize))
                .counts
                .is_null()
            {
                xml_regexp_err_memory(null_mut(), c"saving regexp".as_ptr());
                (*exec).status = -5;
                return;
            }
        }
        memcpy(
            (*(*exec).rollbacks.add((*exec).nb_rollbacks as usize)).counts as _,
            (*exec).counts as _,
            (*(*exec).comp).nb_counters as usize * size_of::<c_int>(),
        );
    }
    (*exec).nb_rollbacks += 1;
}

pub(crate) const REGEXP_ALL_COUNTER: usize = 0x123456;

unsafe extern "C" fn xmlFARegExec(comp: XmlRegexpPtr, content: *const XmlChar) -> c_int {
    let mut execval: XmlRegExecCtxt = unsafe { zeroed() };
    let exec: XmlRegExecCtxtPtr = addr_of_mut!(execval);
    let mut ret: c_int;
    let mut codepoint: c_int;
    let mut len: c_int;
    let mut deter: c_int;

    (*exec).input_string = content;
    (*exec).index = 0;
    (*exec).nb_push = 0;
    (*exec).determinist = 1;
    (*exec).max_rollbacks = 0;
    (*exec).nb_rollbacks = 0;
    (*exec).rollbacks = null_mut();
    (*exec).status = 0;
    (*exec).comp = comp;
    (*exec).state = *(*comp).states.add(0);
    (*exec).transno = 0;
    (*exec).transcount = 0;
    (*exec).input_stack = null_mut();
    (*exec).input_stack_max = 0;
    if (*comp).nb_counters > 0 {
        (*exec).counts =
            xml_malloc((*comp).nb_counters as usize * size_of::<c_int>()) as *mut c_int;
        if (*exec).counts.is_null() {
            xml_regexp_err_memory(null_mut(), c"running regexp".as_ptr());
            return -1;
        }
        memset(
            (*exec).counts as _,
            0,
            (*comp).nb_counters as usize * size_of::<c_int>(),
        );
    } else {
        (*exec).counts = null_mut();
    }
    'error: {
        'b: while (*exec).status == 0
            && !(*exec).state.is_null()
            && (*(*exec).input_string.add((*exec).index as usize) != 0
                || (!(*exec).state.is_null()
                    && !matches!((*(*exec).state).typ, XmlRegStateType::XmlRegexpFinalState)))
        {
            let mut trans: XmlRegTransPtr;
            let mut atom: XmlRegAtomPtr;

            'rollback: {
                /*
                 * If end of input on non-terminal state, rollback, however we may
                 * still have epsilon like transition for counted transitions
                 * on counters, in that case don't break too early.  Additionally,
                 * if we are working on a range like "AB{0,2}", where B is not present,
                 * we don't want to break.
                 */
                len = 1;
                if *(*exec).input_string.add((*exec).index as usize) == 0
                    && (*exec).counts.is_null()
                {
                    /*
                     * if there is a transition, we must check if
                     *  atom allows minOccurs of 0
                     */
                    if (*exec).transno < (*(*exec).state).nb_trans {
                        trans = (*(*exec).state).trans.add((*exec).transno as usize);
                        if (*trans).to >= 0 {
                            atom = (*trans).atom;
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
                    (*exec).transno < (*(*exec).state).nb_trans
                } {
                    trans = (*(*exec).state).trans.add((*exec).transno as usize);
                    if (*trans).to < 0 {
                        continue;
                    }
                    atom = (*trans).atom;
                    ret = 0;
                    deter = 1;
                    if (*trans).count >= 0 {
                        if (*exec).counts.is_null() {
                            (*exec).status = -1;
                            break 'error;
                        }
                        /*
                         * A counted transition.
                         */

                        let count: c_int = *(*exec).counts.add((*trans).count as usize);
                        let counter: XmlRegCounterPtr =
                            (*(*exec).comp).counters.add((*trans).count as usize);
                        // #ifdef DEBUG_REGEXP_EXEC
                        // 		printf("testing count %d: val %d, min %d, max %d\n",
                        // 		       (*trans).count, count, (*counter).min,  (*counter).max);
                        // #endif
                        ret = (count >= (*counter).min && count <= (*counter).max) as i32;
                        if ret != 0 && (*counter).min != (*counter).max {
                            deter = 0;
                        }
                    } else if atom.is_null() {
                        // fprintf(stderr, c"epsilon transition left at runtime\n".as_ptr());
                        eprintln!("epsilon transition left at runtime");
                        (*exec).status = -2;
                        break;
                    } else if *(*exec).input_string.add((*exec).index as usize) != 0 {
                        codepoint =
                            CUR_SCHAR!((*exec).input_string.add((*exec).index as usize), len);
                        ret = xml_reg_check_character(atom, codepoint);
                        if ret == 1 && (*atom).min >= 0 && (*atom).max > 0 {
                            let to: XmlRegStatePtr = *(*comp).states.add((*trans).to as usize);

                            /*
                             * this is a multiple input sequence
                             * If there is a counter associated increment it now.
                             * do not increment if the counter is already over the
                             * maximum limit in which case get to next transition
                             */
                            if (*trans).counter >= 0 {
                                if (*exec).counts.is_null()
                                    || (*exec).comp.is_null()
                                    || (*(*exec).comp).counters.is_null()
                                {
                                    (*exec).status = -1;
                                    break 'error;
                                }
                                let counter: XmlRegCounterPtr =
                                    (*(*exec).comp).counters.add((*trans).counter as usize);
                                if *(*exec).counts.add((*trans).counter as usize) >= (*counter).max
                                {
                                    continue; /* for loop on transitions */
                                }
                            }
                            /* Save before incrementing */
                            if (*(*exec).state).nb_trans > (*exec).transno + 1 {
                                xml_fa_reg_exec_save(exec);
                            }
                            if (*trans).counter >= 0 {
                                // #ifdef DEBUG_REGEXP_EXEC
                                // 			printf("Increasing count %d\n", (*trans).counter);
                                // #endif
                                *(*exec).counts.add((*trans).counter as usize) += 1;
                            }
                            (*exec).transcount = 1;
                            'inner: while {
                                /*
                                 * Try to progress as much as possible on the input
                                 */
                                if (*exec).transcount == (*atom).max {
                                    break 'inner;
                                }
                                (*exec).index += len;
                                /*
                                 * End of input: stop here
                                 */
                                if *(*exec).input_string.add((*exec).index as usize) == 0 {
                                    (*exec).index -= len;
                                    break 'inner;
                                }
                                if (*exec).transcount >= (*atom).min {
                                    let transno: c_int = (*exec).transno;
                                    let state: XmlRegStatePtr = (*exec).state;

                                    /*
                                     * The transition is acceptable save it
                                     */
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

                            /*
                             * If the last check failed but one transition was found
                             * possible, rollback
                             */
                            if ret < 0 {
                                ret = 0;
                            }
                            if ret == 0 {
                                break 'rollback;
                            }
                            if (*trans).counter >= 0 {
                                if (*exec).counts.is_null() {
                                    (*exec).status = -1;
                                    break 'error;
                                }
                                // #ifdef DEBUG_REGEXP_EXEC
                                // 			printf("Decreasing count %d\n", (*trans).counter);
                                // #endif
                                *(*exec).counts.add((*trans).counter as usize) -= 1;
                            }
                        } else if ret == 0 && (*atom).min == 0 && (*atom).max > 0 {
                            /*
                             * we don't match on the codepoint, but minOccurs of 0
                             * says that's ok.  Setting len to 0 inhibits stepping
                             * over the codepoint.
                             */
                            (*exec).transcount = 1;
                            len = 0;
                            ret = 1;
                        }
                    } else if (*atom).min == 0 && (*atom).max > 0 {
                        /* another spot to match when minOccurs is 0 */
                        (*exec).transcount = 1;
                        len = 0;
                        ret = 1;
                    }
                    if ret == 1 {
                        if (*trans).nd == 1
                            || ((*trans).count >= 0
                                && deter == 0
                                && (*(*exec).state).nb_trans > (*exec).transno + 1)
                        {
                            // #ifdef DEBUG_REGEXP_EXEC
                            // 		    if ((*trans).nd == 1)
                            // 		        printf("Saving on nd transition atom %d for %c at %d\n",
                            // 			       (*trans).(*atom).no, codepoint, (*exec).index);
                            // 		    else
                            // 		        printf("Saving on counted transition count %d for %c at %d\n",
                            // 			       (*trans).count, codepoint, (*exec).index);
                            // #endif
                            xml_fa_reg_exec_save(exec);
                        }
                        if (*trans).counter >= 0 {
                            /* make sure we don't go over the counter maximum value */
                            if (*exec).counts.is_null()
                                || (*exec).comp.is_null()
                                || (*(*exec).comp).counters.is_null()
                            {
                                (*exec).status = -1;
                                break 'error;
                            }
                            let counter: XmlRegCounterPtr =
                                (*(*exec).comp).counters.add((*trans).counter as usize);
                            if *(*exec).counts.add((*trans).counter as usize) >= (*counter).max {
                                continue; /* for loop on transitions */
                            }
                            // #ifdef DEBUG_REGEXP_EXEC
                            // 		    printf("Increasing count %d\n", (*trans).counter);
                            // #endif
                            *(*exec).counts.add((*trans).counter as usize) += 1;
                        }
                        if (*trans).count >= 0 && ((*trans).count as usize) < REGEXP_ALL_COUNTER {
                            if (*exec).counts.is_null() {
                                (*exec).status = -1;
                                break 'error;
                            }
                            // #ifdef DEBUG_REGEXP_EXEC
                            // 		    printf("resetting count %d on transition\n",
                            // 		           (*trans).count);
                            // #endif
                            *(*exec).counts.add((*trans).count as usize) = 0;
                        }
                        // #ifdef DEBUG_REGEXP_EXEC
                        // 		printf("entering state %d\n", (*trans).to);
                        // #endif
                        (*exec).state = *(*comp).states.add((*trans).to as usize);
                        (*exec).transno = 0;
                        if !(*trans).atom.is_null() {
                            (*exec).index += len;
                        }
                        continue 'b;
                    } else if ret < 0 {
                        (*exec).status = -4;
                        break;
                    }
                }
                if (*exec).transno != 0 || (*(*exec).state).nb_trans == 0 {
                    // rollback:
                    break 'rollback;
                }
                continue 'b;
            }
            /*
             * Failed to find a way out
             */
            (*exec).determinist = 0;
            // #ifdef DEBUG_REGEXP_EXEC
            // 	    printf("rollback from state %d on %d:%c\n", (*(*exec).state).no,
            // 	           codepoint,codepoint);
            // #endif
            xml_fa_reg_exec_roll_back(exec);
        }
    }
    // error:
    if !(*exec).rollbacks.is_null() {
        if !(*exec).counts.is_null() {
            for i in 0..(*exec).max_rollbacks {
                if !(*(*exec).rollbacks.add(i as usize)).counts.is_null() {
                    xml_free((*(*exec).rollbacks.add(i as usize)).counts as _);
                }
            }
        }
        xml_free((*exec).rollbacks as _);
    }
    if (*exec).state.is_null() {
        return -1;
    }
    if !(*exec).counts.is_null() {
        xml_free((*exec).counts as _);
    }
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

/**
 * xmlRegexpExec:
 * @comp:  the compiled regular expression
 * @content:  the value to check against the regular expression
 *
 * Check if the regular expression generates the value
 *
 * Returns 1 if it matches, 0 if not and a negative value in case of error
 */
pub unsafe extern "C" fn xml_regexp_exec(comp: XmlRegexpPtr, content: *const XmlChar) -> c_int {
    if comp.is_null() || content.is_null() {
        return -1;
    }
    xmlFARegExec(comp, content)
}

unsafe extern "C" fn xml_reg_print_atom_type(output: *mut FILE, typ: XmlRegAtomType) {
    match typ {
        XmlRegAtomType::XmlRegexpEpsilon => {
            fprintf(output, c"epsilon ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpCharval => {
            fprintf(output, c"charval ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpRanges => {
            fprintf(output, c"ranges ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpSubreg => {
            fprintf(output, c"subexpr ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpString => {
            fprintf(output, c"string ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpAnychar => {
            fprintf(output, c"anychar ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpAnyspace => {
            fprintf(output, c"anyspace ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpNotspace => {
            fprintf(output, c"notspace ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpInitname => {
            fprintf(output, c"initname ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpNotinitname => {
            fprintf(output, c"notinitname ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpNamechar => {
            fprintf(output, c"namechar ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpNotnamechar => {
            fprintf(output, c"notnamechar ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpDecimal => {
            fprintf(output, c"decimal ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpNotdecimal => {
            fprintf(output, c"notdecimal ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpRealchar => {
            fprintf(output, c"realchar ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpNotrealchar => {
            fprintf(output, c"notrealchar ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpLetter => {
            fprintf(output, c"LETTER ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpLetterUppercase => {
            fprintf(output, c"LETTER_UPPERCASE ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpLetterLowercase => {
            fprintf(output, c"LETTER_LOWERCASE ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpLetterTitlecase => {
            fprintf(output, c"LETTER_TITLECASE ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpLetterModifier => {
            fprintf(output, c"LETTER_MODIFIER ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpLetterOthers => {
            fprintf(output, c"LETTER_OTHERS ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpMark => {
            fprintf(output, c"MARK ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpMarkNonspacing => {
            fprintf(output, c"MARK_NONSPACING ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpMarkSpacecombining => {
            fprintf(output, c"MARK_SPACECOMBINING ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpMarkEnclosing => {
            fprintf(output, c"MARK_ENCLOSING ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpNumber => {
            fprintf(output, c"NUMBER ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpNumberDecimal => {
            fprintf(output, c"NUMBER_DECIMAL ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpNumberLetter => {
            fprintf(output, c"NUMBER_LETTER ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpNumberOthers => {
            fprintf(output, c"NUMBER_OTHERS ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpPunct => {
            fprintf(output, c"PUNCT ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpPunctConnector => {
            fprintf(output, c"PUNCT_CONNECTOR ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpPunctDash => {
            fprintf(output, c"PUNCT_DASH ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpPunctOpen => {
            fprintf(output, c"PUNCT_OPEN ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpPunctClose => {
            fprintf(output, c"PUNCT_CLOSE ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpPunctInitquote => {
            fprintf(output, c"PUNCT_INITQUOTE ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpPunctFinquote => {
            fprintf(output, c"PUNCT_FINQUOTE ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpPunctOthers => {
            fprintf(output, c"PUNCT_OTHERS ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpSepar => {
            fprintf(output, c"SEPAR ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpSeparSpace => {
            fprintf(output, c"SEPAR_SPACE ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpSeparLine => {
            fprintf(output, c"SEPAR_LINE ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpSeparPara => {
            fprintf(output, c"SEPAR_PARA ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpSymbol => {
            fprintf(output, c"SYMBOL ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpSymbolMath => {
            fprintf(output, c"SYMBOL_MATH ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpSymbolCurrency => {
            fprintf(output, c"SYMBOL_CURRENCY ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpSymbolModifier => {
            fprintf(output, c"SYMBOL_MODIFIER ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpSymbolOthers => {
            fprintf(output, c"SYMBOL_OTHERS ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpOther => {
            fprintf(output, c"OTHER ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpOtherControl => {
            fprintf(output, c"OTHER_CONTROL ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpOtherFormat => {
            fprintf(output, c"OTHER_FORMAT ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpOtherPrivate => {
            fprintf(output, c"OTHER_PRIVATE ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpOtherNa => {
            fprintf(output, c"OTHER_NA ".as_ptr());
        }
        XmlRegAtomType::XmlRegexpBlockName => {
            fprintf(output, c"BLOCK ".as_ptr());
        }
    }
}

unsafe extern "C" fn xml_reg_print_quant_type(output: *mut FILE, typ: XmlRegQuantType) {
    match typ {
        XmlRegQuantType::XmlRegexpQuantEpsilon => {
            fprintf(output, c"epsilon ".as_ptr());
        }
        XmlRegQuantType::XmlRegexpQuantOnce => {
            fprintf(output, c"once ".as_ptr());
        }
        XmlRegQuantType::XmlRegexpQuantOpt => {
            fprintf(output, c"? ".as_ptr());
        }
        XmlRegQuantType::XmlRegexpQuantMult => {
            fprintf(output, c"* ".as_ptr());
        }
        XmlRegQuantType::XmlRegexpQuantPlus => {
            fprintf(output, c"+ ".as_ptr());
        }
        XmlRegQuantType::XmlRegexpQuantRange => {
            fprintf(output, c"range ".as_ptr());
        }
        XmlRegQuantType::XmlRegexpQuantOnceonly => {
            fprintf(output, c"onceonly ".as_ptr());
        }
        XmlRegQuantType::XmlRegexpQuantAll => {
            fprintf(output, c"all ".as_ptr());
        }
    }
}

unsafe extern "C" fn xml_reg_print_range(output: *mut FILE, range: XmlRegRangePtr) {
    fprintf(output, c"  range: ".as_ptr());
    if (*range).neg != 0 {
        fprintf(output, c"negative ".as_ptr());
    }
    xml_reg_print_atom_type(output, (*range).typ);
    fprintf(output, c"%c - %c\n".as_ptr(), (*range).start, (*range).end);
}

unsafe extern "C" fn xml_reg_print_atom(output: *mut FILE, atom: XmlRegAtomPtr) {
    fprintf(output, c" atom: ".as_ptr());
    if atom.is_null() {
        fprintf(output, c"NULL\n".as_ptr());
        return;
    }
    if (*atom).neg != 0 {
        fprintf(output, c"not ".as_ptr());
    }
    xml_reg_print_atom_type(output, (*atom).typ);
    xml_reg_print_quant_type(output, (*atom).quant);
    if matches!((*atom).quant, XmlRegQuantType::XmlRegexpQuantRange) {
        fprintf(output, c"%d-%d ".as_ptr(), (*atom).min, (*atom).max);
    }
    if matches!((*atom).typ, XmlRegAtomType::XmlRegexpString) {
        fprintf(output, c"'%s' ".as_ptr(), (*atom).valuep as *mut c_char);
    }
    if matches!((*atom).typ, XmlRegAtomType::XmlRegexpCharval) {
        fprintf(output, c"char %c\n".as_ptr(), (*atom).codepoint);
    } else if matches!((*atom).typ, XmlRegAtomType::XmlRegexpRanges) {
        fprintf(output, c"%d entries\n".as_ptr(), (*atom).nb_ranges);
        for i in 0..(*atom).nb_ranges {
            xml_reg_print_range(output, *(*atom).ranges.add(i as usize));
        }
    } else if matches!((*atom).typ, XmlRegAtomType::XmlRegexpSubreg) {
        fprintf(
            output,
            c"start %d end %d\n".as_ptr(),
            (*(*atom).start).no,
            (*(*atom).stop).no,
        );
    } else {
        fprintf(output, c"\n".as_ptr());
    }
}

unsafe extern "C" fn xml_reg_print_trans(output: *mut FILE, trans: XmlRegTransPtr) {
    fprintf(output, c"  trans: ".as_ptr());
    if trans.is_null() {
        fprintf(output, c"NULL\n".as_ptr());
        return;
    }
    if (*trans).to < 0 {
        fprintf(output, c"removed\n".as_ptr());
        return;
    }
    if (*trans).nd != 0 {
        if (*trans).nd == 2 {
            fprintf(output, c"last not determinist, ".as_ptr());
        } else {
            fprintf(output, c"not determinist, ".as_ptr());
        }
    }
    if (*trans).counter >= 0 {
        fprintf(output, c"counted %d, ".as_ptr(), (*trans).counter);
    }
    if (*trans).count as usize == REGEXP_ALL_COUNTER {
        fprintf(output, c"all transition, ".as_ptr());
    } else if (*trans).count >= 0 {
        fprintf(output, c"count based %d, ".as_ptr(), (*trans).count);
    }
    if (*trans).atom.is_null() {
        fprintf(output, c"epsilon to %d\n".as_ptr(), (*trans).to);
        return;
    }
    if matches!((*(*trans).atom).typ, XmlRegAtomType::XmlRegexpCharval) {
        fprintf(output, c"char %c ".as_ptr(), (*(*trans).atom).codepoint);
    }
    fprintf(
        output,
        c"atom %d, to %d\n".as_ptr(),
        (*(*trans).atom).no,
        (*trans).to,
    );
}

unsafe extern "C" fn xml_reg_print_state(output: *mut FILE, state: XmlRegStatePtr) {
    fprintf(output, c" state: ".as_ptr());
    if state.is_null() {
        fprintf(output, c"NULL\n".as_ptr());
        return;
    }
    if matches!((*state).typ, XmlRegStateType::XmlRegexpStartState) {
        fprintf(output, c"START ".as_ptr());
    }
    if matches!((*state).typ, XmlRegStateType::XmlRegexpFinalState) {
        fprintf(output, c"FINAL ".as_ptr());
    }

    fprintf(
        output,
        c"%d, %d transitions:\n".as_ptr(),
        (*state).no,
        (*state).nb_trans,
    );
    for i in 0..(*state).nb_trans {
        xml_reg_print_trans(output, (*state).trans.add(i as usize));
    }
}

/**
 * xmlRegexpPrint:
 * @output: the file for the output debug
 * @regexp: the compiled regexp
 *
 * Print the content of the compiled regular expression
 */
pub unsafe extern "C" fn xml_regexp_print(output: *mut FILE, regexp: XmlRegexpPtr) {
    if output.is_null() {
        return;
    }
    fprintf(output, c" regexp: ".as_ptr());
    if regexp.is_null() {
        fprintf(output, c"NULL\n".as_ptr());
        return;
    }
    fprintf(output, c"'%s' ".as_ptr(), (*regexp).string);
    fprintf(output, c"\n".as_ptr());
    fprintf(output, c"%d atoms:\n".as_ptr(), (*regexp).nb_atoms);
    for i in 0..(*regexp).nb_atoms {
        fprintf(output, c" %02d ".as_ptr(), i);
        xml_reg_print_atom(output, *(*regexp).atoms.add(i as usize));
    }
    fprintf(output, c"%d states:".as_ptr(), (*regexp).nb_states);
    fprintf(output, c"\n".as_ptr());
    for i in 0..(*regexp).nb_states {
        xml_reg_print_state(output, *(*regexp).states.add(i as usize));
    }
    fprintf(output, c"%d counters:\n".as_ptr(), (*regexp).nb_counters);
    for i in 0..(*regexp).nb_counters {
        fprintf(
            output,
            c" %d: min %d max %d\n".as_ptr(),
            i,
            (*(*regexp).counters.add(i as usize)).min,
            (*(*regexp).counters.add(i as usize)).max,
        );
    }
}

/**
 * xmlFAEqualAtoms:
 * @atom1:  an atom
 * @atom2:  an atom
 * @deep: if not set only compare string pointers
 *
 * Compares two atoms to check whether they are the same exactly
 * this is used to remove equivalent transitions
 *
 * Returns 1 if same and 0 otherwise
 */
unsafe extern "C" fn xml_fa_equal_atoms(
    atom1: XmlRegAtomPtr,
    atom2: XmlRegAtomPtr,
    deep: c_int,
) -> c_int {
    let mut ret: c_int = 0;

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
                ret = ((*atom1).valuep == (*atom2).valuep) as i32;
            } else {
                ret = xml_str_equal(
                    (*atom1).valuep as *mut XmlChar,
                    (*atom2).valuep as *mut XmlChar,
                );
            }
        }
        XmlRegAtomType::XmlRegexpCharval => {
            ret = ((*atom1).codepoint == (*atom2).codepoint) as i32;
        }
        XmlRegAtomType::XmlRegexpRanges => {
            /* too hard to do in the general case */
            ret = 0;
        }
        _ => {}
    }
    ret
}

/**
 * xmlFACompareAtomTypes:
 * @type1:  an atom type
 * @type2:  an atom type
 *
 * Compares two atoms type to check whether they intersect in some ways,
 * this is used by xmlFACompareAtoms only
 *
 * Returns 1 if they may intersect and 0 otherwise
 */
unsafe extern "C" fn xmlFACompareAtomTypes(
    mut type1: XmlRegAtomType,
    mut type2: XmlRegAtomType,
) -> c_int {
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

    /* simplify subsequent compares by making sure type1 < type2 */
    if type1 > type2 {
        std::mem::swap(&mut type1, &mut type2);
    }
    match type1 {
        XmlRegAtomType::XmlRegexpAnyspace => {
            /* \s */
            /* can't be a letter, number, mark, punctuation, symbol */
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
            /* \l */
            /* can't be a number, mark, separator, punctuation, symbol or other */
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
            /* \c */
            /* can't be a mark, separator, punctuation, symbol or other */
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
            /* \d */
            /* can't be a letter, mark, separator, punctuation, symbol or other */
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
            /* \w */
            /* can't be a mark, separator, punctuation, symbol or other */
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
        /*
         * at that point we know both type 1 and type2 are from
         * character categories are ordered and are different,
         * it becomes simple because this is a partition
         */
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

/**
 * xmlRegStrEqualWildcard:
 * @expStr:  the string to be evaluated
 * @valStr:  the validation string
 *
 * Checks if both strings are equal or have the same content. "*"
 * can be used as a wildcard in @valStr; "|" is used as a separator of
 * substrings in both @expStr and @valStr.
 *
 * Returns 1 if the comparison is satisfied and the number of substrings
 * is equal, 0 otherwise.
 */

unsafe extern "C" fn xml_reg_str_equal_wildcard(
    mut exp_str: *const XmlChar,
    mut val_str: *const XmlChar,
) -> c_int {
    if exp_str == val_str {
        return 1;
    }
    if exp_str.is_null() {
        return 0;
    }
    if val_str.is_null() {
        return 0;
    }
    while {
        'to_continue: {
            /*
             * Eval if we have a wildcard for the current item.
             */
            if *exp_str != *val_str {
                /* if one of them starts with a wildcard make valStr be it */
                if *val_str == b'*' {
                    std::mem::swap(&mut val_str, &mut exp_str);
                }
                if *val_str != 0 && *exp_str != 0 && {
                    exp_str = exp_str.add(1);
                    *exp_str.sub(1) == b'*'
                } {
                    'to_break: while {
                        if *val_str == XML_REG_STRING_SEPARATOR as _ {
                            break 'to_break;
                        }
                        val_str = val_str.add(1);
                        *val_str != 0
                    } {}
                    break 'to_continue;
                } else {
                    return 0;
                }
            }
            exp_str = exp_str.add(1);
            val_str = val_str.add(1);
        }
        *val_str != 0
    } {}
    if *exp_str != 0 {
        0
    } else {
        1
    }
}

unsafe extern "C" fn xmlFACompareRanges(
    mut range1: XmlRegRangePtr,
    mut range2: XmlRegRangePtr,
) -> c_int {
    let mut ret: c_int;

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

    /* put them in order */
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
        let mut neg: c_int = 0;

        /*
         * just check all codepoints in the range for acceptance,
         * this is usually way cheaper since done only once at
         * compilation than testing over and over at runtime or
         * pushing too many states when evaluating.
         */
        if ((*range1).neg == 0 && (*range2).neg != 0) || ((*range1).neg != 0 && (*range2).neg == 0)
        {
            neg = 1;
        }

        for codepoint in (*range1).start..=(*range1).end {
            ret = xmlRegCheckCharacterRange(
                (*range2).typ,
                codepoint,
                0,
                (*range2).start,
                (*range2).end,
                (*range2).block_name,
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
            ret = xml_str_equal((*range1).block_name, (*range2).block_name);
        } else {
            /*
             * comparing a block range with anything else is way
             * too costly, and maintaining the table is like too much
             * memory too, so let's force the automata to save state
             * here.
             */
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
            /* same thing to limit complexity */
            return 1;
        }
    } else {
        ret = 0;
        /* (*range1).typ < (*range2).typ here */
        match (*range1).typ {
            XmlRegAtomType::XmlRegexpLetter => {
                /* all disjoint except in the subgroups */
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
                    /* safety net ! */
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

/**
 * xmlFACompareAtoms:
 * @atom1:  an atom
 * @atom2:  an atom
 * @deep: if not set only compare string pointers
 *
 * Compares two atoms to check whether they intersect in some ways,
 * this is used by xmlFAComputesDeterminism and xmlFARecurseDeterminism only
 *
 * Returns 1 if yes and 0 otherwise
 */
unsafe extern "C" fn xmlFACompareAtoms(
    mut atom1: XmlRegAtomPtr,
    mut atom2: XmlRegAtomPtr,
    deep: c_int,
) -> c_int {
    let mut ret: c_int = 1;

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
        ret = xmlFACompareAtomTypes((*atom1).typ, (*atom2).typ);
        /* if they can't intersect at the type level break now */
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
                    let val1: *mut XmlChar = (*atom1).valuep as *mut XmlChar;
                    let val2: *mut XmlChar = (*atom2).valuep as *mut XmlChar;
                    let compound1: c_int = !xml_strchr(val1, b'|').is_null() as i32;
                    let compound2: c_int = !xml_strchr(val2, b'|').is_null() as i32;

                    /* Ignore negative match flag for ##other namespaces */
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
                    let mut res: c_int;
                    let mut r1: XmlRegRangePtr;
                    let mut r2: XmlRegRangePtr;

                    /*
                     * need to check that none of the ranges eventually matches
                     */
                    for i in 0..(*atom1).nb_ranges {
                        for j in 0..(*atom2).nb_ranges {
                            r1 = *(*atom1).ranges.add(i as usize);
                            r2 = *(*atom2).ranges.add(j as usize);
                            res = xmlFACompareRanges(r1, r2);
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

/**
 * xmlFARecurseDeterminism:
 * @ctxt:  a regexp parser context
 *
 * Check whether the associated regexp is determinist,
 * should be called after xmlFAEliminateEpsilonTransitions()
 *
 */
unsafe extern "C" fn xml_fa_recurse_determinism(
    ctxt: XmlRegParserCtxtPtr,
    state: XmlRegStatePtr,
    to: c_int,
    atom: XmlRegAtomPtr,
) -> c_int {
    let mut ret: c_int = 1;
    let mut res: c_int;
    let mut t1: XmlRegTransPtr;
    let mut deep: c_int = 1;

    if state.is_null() {
        return ret;
    }
    if matches!((*state).markd, XmlRegMarkedType::XmlRegexpMarkVisited) {
        return ret;
    }

    if (*ctxt).flags & AM_AUTOMATA_RNG as i32 != 0 {
        deep = 0;
    }

    /*
     * don't recurse on transitions potentially added in the course of
     * the elimination.
     */
    let nb_trans: c_int = (*state).nb_trans;
    for transnr in 0..nb_trans {
        t1 = (*state).trans.add(transnr as usize);
        /*
         * check transitions conflicting with the one looked at
         */
        if (*t1).atom.is_null() {
            if (*t1).to < 0 {
                continue;
            }
            (*state).markd = XmlRegMarkedType::XmlRegexpMarkVisited;
            res =
                xml_fa_recurse_determinism(ctxt, *(*ctxt).states.add((*t1).to as usize), to, atom);
            if res == 0 {
                ret = 0;
                /* (*t1).nd = 1; */
            }
            continue;
        }
        if (*t1).to != to {
            continue;
        }
        if xmlFACompareAtoms((*t1).atom, atom, deep) != 0 {
            ret = 0;
            /* mark the transition as non-deterministic */
            (*t1).nd = 1;
        }
    }
    ret
}

/**
 * xmlFAFinishRecurseDeterminism:
 * @ctxt:  a regexp parser context
 *
 * Reset flags after checking determinism.
 */
unsafe extern "C" fn xml_fa_finish_recurse_determinism(
    ctxt: XmlRegParserCtxtPtr,
    state: XmlRegStatePtr,
) {
    if state.is_null() {
        return;
    }
    if !matches!((*state).markd, XmlRegMarkedType::XmlRegexpMarkVisited) {
        return;
    }
    (*state).markd = XmlRegMarkedType::XmlRegexpMarkNormal;

    let nb_trans: c_int = (*state).nb_trans;
    for transnr in 0..nb_trans {
        let t1: XmlRegTransPtr = (*state).trans.add(transnr as usize);
        if (*t1).atom.is_null() && (*t1).to >= 0 {
            xml_fa_finish_recurse_determinism(ctxt, *(*ctxt).states.add((*t1).to as usize));
        }
    }
}

/**
 * xmlFAComputesDeterminism:
 * @ctxt:  a regexp parser context
 *
 * Check whether the associated regexp is determinist,
 * should be called after xmlFAEliminateEpsilonTransitions()
 *
 */
pub(crate) unsafe extern "C" fn xml_fa_computes_determinism(ctxt: XmlRegParserCtxtPtr) -> c_int {
    let mut state: XmlRegStatePtr;
    let mut t1: XmlRegTransPtr;
    let mut t2: XmlRegTransPtr;
    let mut last: XmlRegTransPtr;
    let mut ret: c_int = 1;
    let mut deep: c_int = 1;

    // #ifdef DEBUG_REGEXP_GRAPH
    //     printf("xmlFAComputesDeterminism\n");
    //     xmlRegPrintCtxt(stdout, ctxt);
    // #endif
    if (*ctxt).determinist != -1 {
        return (*ctxt).determinist;
    }

    if (*ctxt).flags & AM_AUTOMATA_RNG as i32 != 0 {
        deep = 0;
    }

    /*
     * First cleanup the automata removing cancelled transitions
     */
    for statenr in 0..(*ctxt).nb_states {
        state = *(*ctxt).states.add(statenr as usize);
        if state.is_null() {
            continue;
        }
        if (*state).nb_trans < 2 {
            continue;
        }
        for transnr in 0..(*state).nb_trans {
            t1 = (*state).trans.add(transnr as usize);
            /*
             * Determinism checks in case of counted or all transitions
             * will have to be handled separately
             */
            if (*t1).atom.is_null() {
                /* (*t1).nd = 1; */
                continue;
            }
            if (*t1).to == -1 {
                /* eliminated */
                continue;
            }
            for i in 0..transnr {
                t2 = (*state).trans.add(i as usize);
                if (*t2).to == -1 {
                    /* eliminated */
                    continue;
                }
                if !(*t2).atom.is_null() && (*t1).to == (*t2).to {
                    /*
                     * Here we use deep because we want to keep the
                     * transitions which indicate a conflict
                     */
                    if xml_fa_equal_atoms((*t1).atom, (*t2).atom, deep) != 0
                        && (*t1).counter == (*t2).counter
                        && (*t1).count == (*t2).count
                    {
                        (*t2).to = -1; /* eliminated */
                    }
                }
            }
        }
    }

    /*
     * Check for all states that there aren't 2 transitions
     * with the same atom and a different target.
     */
    for statenr in 0..(*ctxt).nb_states {
        state = *(*ctxt).states.add(statenr as usize);
        if state.is_null() {
            continue;
        }
        if (*state).nb_trans < 2 {
            continue;
        }
        last = null_mut();
        for transnr in 0..(*state).nb_trans {
            t1 = (*state).trans.add(transnr as usize);
            /*
             * Determinism checks in case of counted or all transitions
             * will have to be handled separately
             */
            if (*t1).atom.is_null() {
                continue;
            }
            if (*t1).to == -1 {
                /* eliminated */
                continue;
            }
            for i in 0..transnr {
                t2 = (*state).trans.add(i as usize);
                if (*t2).to == -1 {
                    /* eliminated */
                    continue;
                }
                if !(*t2).atom.is_null() {
                    /*
                     * But here we don't use deep because we want to
                     * find transitions which indicate a conflict
                     */
                    if xmlFACompareAtoms((*t1).atom, (*t2).atom, 1) != 0 {
                        ret = 0;
                        /* mark the transitions as non-deterministic ones */
                        (*t1).nd = 1;
                        (*t2).nd = 1;
                        last = t1;
                    }
                } else if (*t1).to != -1 {
                    /*
                     * do the closure in case of remaining specific
                     * epsilon transitions like choices or all
                     */
                    ret = xml_fa_recurse_determinism(
                        ctxt,
                        *(*ctxt).states.add((*t1).to as usize),
                        (*t2).to,
                        (*t2).atom,
                    );
                    xml_fa_finish_recurse_determinism(ctxt, *(*ctxt).states.add((*t1).to as usize));
                    /* don't shortcut the computation so all non deterministic
                       transition get marked down
                    if (ret == 0)
                    return(0);
                     */
                    if ret == 0 {
                        (*t1).nd = 1;
                        /* (*t2).nd = 1; */
                        last = t1;
                    }
                }
            }
            /* don't shortcut the computation so all non deterministic
               transition get marked down
            if (ret == 0)
            break; */
        }
        /*
         * mark specifically the last non-deterministic transition
         * from a state since there is no need to set-up rollback
         * from it
         */
        if !last.is_null() {
            (*last).nd = 2;
        }

        /* don't shortcut the computation so all non deterministic
           transition get marked down
        if (ret == 0)
            break; */
    }

    (*ctxt).determinist = ret;
    ret
}

/**
 * xmlRegexpIsDeterminist:
 * @comp:  the compiled regular expression
 *
 * Check if the regular expression is determinist
 *
 * Returns 1 if it yes, 0 if not and a negative value in case of error
 */
pub unsafe extern "C" fn xml_regexp_is_determinist(comp: XmlRegexpPtr) -> c_int {
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
    if !(*am).states.is_null() {
        for i in 0..(*am).nb_states {
            xml_reg_free_state(*(*am).states.add(i as usize));
        }
        xml_free((*am).states as _);
    }
    (*am).nb_atoms = (*comp).nb_atoms;
    (*am).atoms = (*comp).atoms;
    (*am).nb_states = (*comp).nb_states;
    (*am).states = (*comp).states;
    (*am).determinist = -1;
    (*am).flags = (*comp).flags;
    let ret: c_int = xml_fa_computes_determinism(am);
    (*am).atoms = null_mut();
    (*am).states = null_mut();
    xml_free_automata(am);
    (*comp).determinist = ret;
    ret
}

/**
 * xmlRegExecCallbacks:
 * @exec: the regular expression context
 * @token: the current token string
 * @transdata: transition data
 * @inputdata: input data
 *
 * Callback function when doing a transition in the automata
 */
pub type XmlRegExecCallbacks = unsafe extern "C" fn(
    exec: XmlRegExecCtxtPtr,
    token: *const XmlChar,
    transdata: *mut c_void,
    inputdata: *mut c_void,
);

/*
 * The progressive API
 */
/**
 * xmlRegNewExecCtxt:
 * @comp: a precompiled regular expression
 * @callback: a callback function used for handling progresses in the
 *            automata matching phase
 * @data: the context data associated to the callback in this context
 *
 * Build a context used for progressive evaluation of a regexp.
 *
 * Returns the new context
 */
pub unsafe extern "C" fn xml_reg_new_exec_ctxt(
    comp: XmlRegexpPtr,
    callback: Option<XmlRegExecCallbacks>,
    data: *mut c_void,
) -> XmlRegExecCtxtPtr {
    if comp.is_null() {
        return null_mut();
    }
    if (*comp).compact.is_null() && (*comp).states.is_null() {
        return null_mut();
    }
    let exec: XmlRegExecCtxtPtr = xml_malloc(size_of::<XmlRegExecCtxt>()) as XmlRegExecCtxtPtr;
    if exec.is_null() {
        xml_regexp_err_memory(null_mut(), c"creating execution context".as_ptr());
        return null_mut();
    }
    memset(exec as _, 0, size_of::<XmlRegExecCtxt>());
    (*exec).input_string = null_mut();
    (*exec).index = 0;
    (*exec).determinist = 1;
    (*exec).max_rollbacks = 0;
    (*exec).nb_rollbacks = 0;
    (*exec).rollbacks = null_mut();
    (*exec).status = 0;
    (*exec).comp = comp;
    if (*comp).compact.is_null() {
        (*exec).state = *(*comp).states.add(0);
    }
    (*exec).transno = 0;
    (*exec).transcount = 0;
    (*exec).callback = callback;
    (*exec).data = data;
    if (*comp).nb_counters > 0 {
        /*
         * For error handling, (*exec).counts is allocated twice the size
         * the second half is used to store the data in case of rollback
         */
        (*exec).counts =
            xml_malloc((*comp).nb_counters as usize * size_of::<c_int>() * 2) as *mut c_int;
        if (*exec).counts.is_null() {
            xml_regexp_err_memory(null_mut(), c"creating execution context".as_ptr());
            xml_free(exec as _);
            return null_mut();
        }
        memset(
            (*exec).counts as _,
            0,
            (*comp).nb_counters as usize * size_of::<c_int>() * 2,
        );
        (*exec).err_counts = (*exec).counts.add((*comp).nb_counters as usize);
    } else {
        (*exec).counts = null_mut();
        (*exec).err_counts = null_mut();
    }
    (*exec).input_stack_max = 0;
    (*exec).input_stack_nr = 0;
    (*exec).input_stack = null_mut();
    (*exec).err_state_no = -1;
    (*exec).err_string = null_mut();
    (*exec).nb_push = 0;
    exec
}

/**
 * xmlRegFreeExecCtxt:
 * @exec: a regular expression evaluation context
 *
 * Free the structures associated to a regular expression evaluation context.
 */
pub unsafe extern "C" fn xml_reg_free_exec_ctxt(exec: XmlRegExecCtxtPtr) {
    if exec.is_null() {
        return;
    }

    if !(*exec).rollbacks.is_null() {
        if !(*exec).counts.is_null() {
            for i in 0..(*exec).max_rollbacks {
                if !(*(*exec).rollbacks.add(i as usize)).counts.is_null() {
                    xml_free((*(*exec).rollbacks.add(i as usize)).counts as _);
                }
            }
        }
        xml_free((*exec).rollbacks as _);
    }
    if !(*exec).counts.is_null() {
        xml_free((*exec).counts as _);
    }
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
    xml_free(exec as _);
}

pub(crate) const REGEXP_ALL_LAX_COUNTER: usize = 0x123457;

/**
 * xmlRegCompactPushString:
 * @exec: a regexp execution context
 * @comp:  the precompiled exec with a compact table
 * @value: a string token input
 * @data: data associated to the token to reuse in callbacks
 *
 * Push one input token in the execution context
 *
 * Returns: 1 if the regexp reached a final state, 0 if non-final, and
 *     a negative value in case of error.
 */
unsafe extern "C" fn xml_reg_compact_push_string(
    exec: XmlRegExecCtxtPtr,
    comp: XmlRegexpPtr,
    value: *const XmlChar,
    data: *mut c_void,
) -> c_int {
    let state: c_int = (*exec).index;
    let mut target: c_int;

    if comp.is_null() || (*comp).compact.is_null() || (*comp).string_map.is_null() {
        return -1;
    }

    if value.is_null() {
        /*
         * are we at a final state ?
         */
        if *(*comp)
            .compact
            .add(state as usize * ((*comp).nbstrings + 1) as usize)
            == XmlRegStateType::XmlRegexpFinalState as i32
        {
            return 1;
        }
        return 0;
    }

    // #ifdef DEBUG_PUSH
    //     printf("value pushed: %s\n", value);
    // #endif

    /*
     * Examine all outside transitions from current state
     */
    for i in 0..(*comp).nbstrings {
        target = *(*comp)
            .compact
            .add(state as usize * ((*comp).nbstrings + 1) as usize + i as usize + 1);
        if target > 0 && target <= (*comp).nbstates {
            target -= 1; /* to avoid 0 */
            if xml_reg_str_equal_wildcard(*(*comp).string_map.add(i as usize), value) != 0 {
                (*exec).index = target;
                if let Some(callback) = (*exec).callback {
                    if !(*comp).transdata.is_null() {
                        callback(
                            (*exec).data as _,
                            value,
                            *(*comp)
                                .transdata
                                .add(state as usize * (*comp).nbstrings as usize + i as usize),
                            data,
                        );
                    }
                }
                // #ifdef DEBUG_PUSH
                // 		printf("entering state %d\n", target);
                // #endif
                if *(*comp)
                    .compact
                    .add(target as usize * ((*comp).nbstrings + 1) as usize)
                    == XmlRegStateType::XmlRegexpSinkState as i32
                {
                    // goto error;
                    break;
                }

                if *(*comp)
                    .compact
                    .add(target as usize * ((*comp).nbstrings + 1) as usize)
                    == XmlRegStateType::XmlRegexpFinalState as i32
                {
                    return 1;
                }
                return 0;
            }
        }
    }
    /*
     * Failed to find an exit transition out from current state for the
     * current token
     */
    // #ifdef DEBUG_PUSH
    //     printf("failed to find a transition for %s on state %d\n", value, state);
    // #endif
    // error:
    if !(*exec).err_string.is_null() {
        xml_free((*exec).err_string as _);
    }
    (*exec).err_string = xml_strdup(value);
    (*exec).err_state_no = state;
    (*exec).status = -1;
    // #ifdef DEBUG_ERR
    //     testerr(exec);
    // #endif
    -1
}

unsafe extern "C" fn xml_fareg_exec_save_input_string(
    exec: XmlRegExecCtxtPtr,
    value: *const XmlChar,
    data: *mut c_void,
) {
    // #ifdef DEBUG_PUSH
    //     printf("saving value: %d:%s\n", (*exec).inputStackNr, value);
    // #endif
    if (*exec).input_stack_max == 0 {
        (*exec).input_stack_max = 4;
        (*exec).input_stack =
            xml_malloc((*exec).input_stack_max as usize * size_of::<XmlRegInputToken>())
                as XmlRegInputTokenPtr;
        if (*exec).input_stack.is_null() {
            xml_regexp_err_memory(null_mut(), c"pushing input string".as_ptr());
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
            xml_regexp_err_memory(null_mut(), c"pushing input string".as_ptr());
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

/**
 * xmlRegExecPushStringInternal:
 * @exec: a regexp execution context or NULL to indicate the end
 * @value: a string token input
 * @data: data associated to the token to reuse in callbacks
 * @compound: value was assembled from 2 strings
 *
 * Push one input token in the execution context
 *
 * Returns: 1 if the regexp reached a final state, 0 if non-final, and
 *     a negative value in case of error.
 */
unsafe extern "C" fn xmlRegExecPushStringInternal(
    exec: XmlRegExecCtxtPtr,
    mut value: *const XmlChar,
    mut data: *mut c_void,
    compound: c_int,
) -> c_int {
    let mut trans: XmlRegTransPtr;
    let mut atom: XmlRegAtomPtr;
    let mut ret: c_int;
    let mut is_final: c_int = 0;
    let mut progress: c_int = 1;

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

    // #ifdef DEBUG_PUSH
    //     printf("value pushed: %s\n", value);
    // #endif
    /*
     * If we have an active rollback stack push the new value there
     * and get back to where we were left
     */
    if !value.is_null() && (*exec).input_stack_nr > 0 {
        xml_fareg_exec_save_input_string(exec, value, data);
        value = (*(*exec).input_stack.add((*exec).index as usize)).value;
        data = (*(*exec).input_stack.add((*exec).index as usize)).data;
        // #ifdef DEBUG_PUSH
        // 	printf("value loaded: %s\n", value);
        // #endif
    }

    'b: while (*exec).status == 0
        && (!value.is_null()
            || (is_final == 1
                && !matches!((*(*exec).state).typ, XmlRegStateType::XmlRegexpFinalState)))
    {
        'rollback: {
            'progress: {
                /*
                 * End of input on non-terminal state, rollback, however we may
                 * still have epsilon like transition for counted transitions
                 * on counters, in that case don't break too early.
                 */
                if value.is_null() && (*exec).counts.is_null() {
                    break 'rollback;
                }

                (*exec).transcount = 0;
                (*exec).transno -= 1;
                while {
                    (*exec).transno += 1;
                    (*exec).transno < (*(*exec).state).nb_trans
                } {
                    trans = (*(*exec).state).trans.add((*exec).transno as usize);
                    if (*trans).to < 0 {
                        continue;
                    }
                    atom = (*trans).atom;
                    ret = 0;
                    if (*trans).count as usize == REGEXP_ALL_LAX_COUNTER {
                        let mut count: c_int;
                        let mut t: XmlRegTransPtr;
                        let mut counter: XmlRegCounterPtr;

                        ret = 0;

                        // #ifdef DEBUG_PUSH
                        // 		printf("testing all lax %d\n", (*trans).count);
                        // #endif
                        /*
                         * Check all counted transitions from the current state
                         */
                        if value.is_null() && is_final != 0 {
                            ret = 1;
                        } else if !value.is_null() {
                            for i in 0..(*(*exec).state).nb_trans {
                                t = (*(*exec).state).trans.add(i as usize);
                                if (*t).counter < 0 || t == trans {
                                    continue;
                                }
                                counter = (*(*exec).comp).counters.add((*t).counter as usize);
                                count = *(*exec).counts.add((*t).counter as usize);
                                if count < (*counter).max
                                    && !(*t).atom.is_null()
                                    && xml_str_equal(value, (*(*t).atom).valuep as _) != 0
                                {
                                    ret = 0;
                                    break;
                                }
                                if count >= (*counter).min
                                    && count < (*counter).max
                                    && !(*t).atom.is_null()
                                    && xml_str_equal(value, (*(*t).atom).valuep as _) != 0
                                {
                                    ret = 1;
                                    break;
                                }
                            }
                        }
                    } else if (*trans).count as usize == REGEXP_ALL_COUNTER {
                        let mut count: c_int;
                        let mut t: XmlRegTransPtr;
                        let mut counter: XmlRegCounterPtr;

                        ret = 1;

                        // #ifdef DEBUG_PUSH
                        // 		printf("testing all %d\n", (*trans).count);
                        // #endif
                        /*
                         * Check all counted transitions from the current state
                         */
                        for i in 0..(*(*exec).state).nb_trans {
                            t = (*(*exec).state).trans.add(i as usize);
                            if (*t).counter < 0 || t == trans {
                                continue;
                            }
                            counter = (*(*exec).comp).counters.add((*t).counter as usize);
                            count = *(*exec).counts.add((*t).counter as usize);
                            if count < (*counter).min || count > (*counter).max {
                                ret = 0;
                                break;
                            }
                        }
                    } else if (*trans).count >= 0 {
                        /*
                         * A counted transition.
                         */

                        let count: c_int = *(*exec).counts.add((*trans).count as usize);
                        let counter: XmlRegCounterPtr =
                            (*(*exec).comp).counters.add((*trans).count as usize);
                        // #ifdef DEBUG_PUSH
                        // 		printf("testing count %d: val %d, min %d, max %d\n",
                        // 		       (*trans).count, count, (*counter).min,  (*counter).max);
                        // #endif
                        ret = (count >= (*counter).min && count <= (*counter).max) as _;
                    } else if atom.is_null() {
                        // fprintf(stderr, "epsilon transition left at runtime\n");
                        eprintln!("epsilon transition left at runtime");
                        (*exec).status = -2;
                        break;
                    } else if !value.is_null() {
                        ret = xml_reg_str_equal_wildcard((*atom).valuep as _, value);
                        if (*atom).neg != 0 {
                            ret = (ret == 0) as i32;
                            if compound == 0 {
                                ret = 0;
                            }
                        }
                        if ret == 1 && (*trans).counter >= 0 {
                            let count: c_int = *(*exec).counts.add((*trans).counter as usize);
                            let counter: XmlRegCounterPtr =
                                (*(*exec).comp).counters.add((*trans).counter as usize);
                            if count >= (*counter).max {
                                ret = 0;
                            }
                        }

                        if ret == 1 && (*atom).min > 0 && (*atom).max > 0 {
                            let to: XmlRegStatePtr =
                                *(*(*exec).comp).states.add((*trans).to as usize);

                            /*
                             * this is a multiple input sequence
                             */
                            if (*(*exec).state).nb_trans > (*exec).transno + 1 {
                                if (*exec).input_stack_nr <= 0 {
                                    xml_fareg_exec_save_input_string(exec, value, data);
                                }
                                xml_fa_reg_exec_save(exec);
                            }
                            (*exec).transcount = 1;
                            'inner: while {
                                /*
                                 * Try to progress as much as possible on the input
                                 */
                                if (*exec).transcount == (*atom).max {
                                    break 'inner;
                                }
                                (*exec).index += 1;
                                value = (*(*exec).input_stack.add((*exec).index as usize)).value;
                                data = (*(*exec).input_stack.add((*exec).index as usize)).data;
                                // #ifdef DEBUG_PUSH
                                // 			printf("value loaded: %s\n", value);
                                // #endif

                                /*
                                 * End of input: stop here
                                 */
                                if value.is_null() {
                                    (*exec).index -= 1;
                                    break 'inner;
                                }
                                if (*exec).transcount >= (*atom).min {
                                    let transno: c_int = (*exec).transno;
                                    let state: XmlRegStatePtr = (*exec).state;

                                    /*
                                     * The transition is acceptable save it
                                     */
                                    (*exec).transno = -1; /* trick */
                                    (*exec).state = to;
                                    if (*exec).input_stack_nr <= 0 {
                                        xml_fareg_exec_save_input_string(exec, value, data);
                                    }
                                    xml_fa_reg_exec_save(exec);
                                    (*exec).transno = transno;
                                    (*exec).state = state;
                                }
                                ret = xml_str_equal(value, (*atom).valuep as _);
                                (*exec).transcount += 1;
                                ret == 1
                            } {}
                            if (*exec).transcount < (*atom).min {
                                ret = 0;
                            }

                            /*
                             * If the last check failed but one transition was found
                             * possible, rollback
                             */
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
                                    (*atom).valuep as _,
                                    (*atom).data as _,
                                    data,
                                );
                            }
                        }
                        if (*(*exec).state).nb_trans > (*exec).transno + 1 {
                            if (*exec).input_stack_nr <= 0 {
                                xml_fareg_exec_save_input_string(exec, value, data);
                            }
                            xml_fa_reg_exec_save(exec);
                        }
                        if (*trans).counter >= 0 {
                            // #ifdef DEBUG_PUSH
                            // 		    printf("Increasing count %d\n", (*trans).counter);
                            // #endif
                            *(*exec).counts.add((*trans).counter as usize) += 1;
                        }
                        if (*trans).count >= 0 && ((*trans).count as usize) < REGEXP_ALL_COUNTER {
                            // #ifdef DEBUG_REGEXP_EXEC
                            // 		    printf("resetting count %d on transition\n",
                            // 		           (*trans).count);
                            // #endif
                            *(*exec).counts.add((*trans).count as usize) = 0;
                        }
                        // #ifdef DEBUG_PUSH
                        // 		printf("entering state %d\n", (*trans).to);
                        // #endif
                        if !(*(*(*exec).comp).states.add((*trans).to as usize)).is_null()
                            && matches!(
                                (*(*(*(*exec).comp).states.add((*trans).to as usize))).typ,
                                XmlRegStateType::XmlRegexpSinkState
                            )
                        {
                            /*
                             * entering a sink state, save the current state as error
                             * state.
                             */
                            if !(*exec).err_string.is_null() {
                                xml_free((*exec).err_string as _);
                            }
                            (*exec).err_string = xml_strdup(value);
                            (*exec).err_state = (*exec).state;
                            memcpy(
                                (*exec).err_counts as _,
                                (*exec).counts as _,
                                (*(*exec).comp).nb_counters as usize * size_of::<c_int>(),
                            );
                        }
                        (*exec).state = *(*(*exec).comp).states.add((*trans).to as usize);
                        (*exec).transno = 0;
                        if !(*trans).atom.is_null() {
                            if !(*exec).input_stack.is_null() {
                                (*exec).index += 1;
                                if (*exec).index < (*exec).input_stack_nr {
                                    value =
                                        (*(*exec).input_stack.add((*exec).index as usize)).value;
                                    data = (*(*exec).input_stack.add((*exec).index as usize)).data;
                                // #ifdef DEBUG_PUSH
                                // 			    printf("value loaded: %s\n", value);
                                // #endif
                                } else {
                                    value = null_mut();
                                    data = null_mut();
                                    // #ifdef DEBUG_PUSH
                                    // 			    printf("end of input\n");
                                    // #endif
                                }
                            } else {
                                value = null_mut();
                                data = null_mut();
                                // #ifdef DEBUG_PUSH
                                // 			printf("end of input\n");
                                // #endif
                            }
                        }
                        break 'progress;
                    } else if ret < 0 {
                        (*exec).status = -4;
                        break;
                    }
                }
                if (*exec).transno != 0 || (*(*exec).state).nb_trans == 0 {
                    break 'rollback;
                }
                continue 'b;
            }
            // progress:
            progress = 1;
            continue 'b;
        }
        /*
         * if we didn't yet rollback on the current input
         * store the current state as the error state.
         */
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
            if (*(*exec).comp).nb_counters != 0 {
                memcpy(
                    (*exec).err_counts as _,
                    (*exec).counts as _,
                    (*(*exec).comp).nb_counters as usize * size_of::<c_int>(),
                );
            }
        }

        /*
         * Failed to find a way out
         */
        (*exec).determinist = 0;
        xml_fa_reg_exec_roll_back(exec);
        if !(*exec).input_stack.is_null() && (*exec).status == 0 {
            value = (*(*exec).input_stack.add((*exec).index as usize)).value;
            data = (*(*exec).input_stack.add((*exec).index as usize)).data;
            // #ifdef DEBUG_PUSH
            // 		printf("value loaded: %s\n", value);
            // #endif
        }
    }
    if (*exec).status == 0 {
        return matches!((*(*exec).state).typ, XmlRegStateType::XmlRegexpFinalState) as _;
    }
    // #ifdef DEBUG_ERR
    //     if ((*exec).status < 0) {
    // 	testerr(exec);
    //     }
    // #endif
    (*exec).status
}

/**
 * xmlRegExecPushString:
 * @exec: a regexp execution context or NULL to indicate the end
 * @value: a string token input
 * @data: data associated to the token to reuse in callbacks
 *
 * Push one input token in the execution context
 *
 * Returns: 1 if the regexp reached a final state, 0 if non-final, and
 *     a negative value in case of error.
 */
pub unsafe extern "C" fn xml_reg_exec_push_string(
    exec: XmlRegExecCtxtPtr,
    value: *const XmlChar,
    data: *mut c_void,
) -> c_int {
    xmlRegExecPushStringInternal(exec, value, data, 0)
}

const XML_REG_STRING_SEPARATOR: c_char = b'|' as _;

/**
 * xmlRegExecPushString2:
 * @exec: a regexp execution context or NULL to indicate the end
 * @value: the first string token input
 * @value2: the second string token input
 * @data: data associated to the token to reuse in callbacks
 *
 * Push one input token in the execution context
 *
 * Returns: 1 if the regexp reached a final state, 0 if non-final, and
 *     a negative value in case of error.
 */
pub unsafe extern "C" fn xml_reg_exec_push_string2(
    exec: XmlRegExecCtxtPtr,
    value: *const XmlChar,
    value2: *const XmlChar,
    data: *mut c_void,
) -> c_int {
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

    let lenn: c_int = strlen(value2 as _) as _;
    let lenp: c_int = strlen(value as _) as _;

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
        xmlRegExecPushStringInternal(exec, str, data, 1)
    };

    if str != buf.as_mut_ptr() {
        xml_free(str as _);
    }
    ret
}

/**
 * xmlRegExecGetValues:
 * @exec: a regexp execution context
 * @err: error extraction or normal one
 * @nbval: pointer to the number of accepted values IN/OUT
 * @nbneg: return number of negative transitions
 * @values: pointer to the array of acceptable values
 * @terminal: return value if this was a terminal state
 *
 * Extract information from the regexp execution, internal routine to
 * implement xmlRegExecNextValues() and xmlRegExecErrInfo()
 *
 * Returns: 0 in case of success or -1 in case of error.
 */
unsafe extern "C" fn xmlRegExecGetValues(
    exec: XmlRegExecCtxtPtr,
    err: c_int,
    nbval: *mut c_int,
    nbneg: *mut c_int,
    values: *mut *mut XmlChar,
    terminal: *mut c_int,
) -> c_int {
    let mut nb: c_int = 0;

    if exec.is_null() || nbval.is_null() || nbneg.is_null() || values.is_null() || *nbval <= 0 {
        return -1;
    }

    let maxval: c_int = *nbval;
    *nbval = 0;
    *nbneg = 0;
    if !(*exec).comp.is_null() && !(*(*exec).comp).compact.is_null() {
        let mut target: c_int;
        let comp: XmlRegexpPtr = (*exec).comp;

        let state = if err != 0 {
            if (*exec).err_state_no == -1 {
                return -1;
            }
            (*exec).err_state_no
        } else {
            (*exec).index
        };
        if !terminal.is_null() {
            if *(*comp)
                .compact
                .add(state as usize * ((*comp).nbstrings + 1) as usize)
                == XmlRegStateType::XmlRegexpFinalState as i32
            {
                *terminal = 1;
            } else {
                *terminal = 0;
            }
        }
        if nb < maxval {
            for i in 0..(*comp).nbstrings {
                target = *(*comp)
                    .compact
                    .add(state as usize * ((*comp).nbstrings + 1) as usize + i as usize + 1);
                if target > 0
                    && target <= (*comp).nbstates
                    && *(*comp)
                        .compact
                        .add((target - 1) as usize * ((*comp).nbstrings as usize + 1))
                        != XmlRegStateType::XmlRegexpSinkState as i32
                {
                    *values.add(nb as usize) = *(*comp).string_map.add(i as usize);
                    nb += 1;
                    *nbval += 1;
                }
                if nb >= maxval {
                    break;
                }
            }
        }
        if nb < maxval {
            for i in 0..(*comp).nbstrings {
                target = *(*comp)
                    .compact
                    .add(state as usize * ((*comp).nbstrings + 1) as usize + i as usize + 1);
                if target > 0
                    && target <= (*comp).nbstates
                    && *(*comp)
                        .compact
                        .add((target - 1) as usize * ((*comp).nbstrings as usize + 1))
                        == XmlRegStateType::XmlRegexpSinkState as i32
                {
                    *values.add(nb as usize) = *(*comp).string_map.add(i as usize);
                    nb += 1;
                    *nbneg += 1;
                }
                if nb >= maxval {
                    break;
                }
            }
        }
    } else {
        let mut trans: XmlRegTransPtr;
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
                return -1;
            }
            (*exec).err_state
        } else {
            if (*exec).state.is_null() {
                return -1;
            }
            (*exec).state
        };
        if nb < maxval {
            for transno in 0..(*state).nb_trans {
                trans = (*state).trans.add(transno as usize);
                if (*trans).to < 0 {
                    continue;
                }
                atom = (*trans).atom;
                if atom.is_null() || (*atom).valuep.is_null() {
                    continue;
                }
                if (*trans).count as usize == REGEXP_ALL_LAX_COUNTER {
                    /* this should not be reached but ... */
                    todo!()
                } else if (*trans).count as usize == REGEXP_ALL_COUNTER {
                    /* this should not be reached but ... */
                    todo!()
                } else if (*trans).counter >= 0 {
                    let mut counter: XmlRegCounterPtr = null_mut();

                    let count = if err != 0 {
                        *(*exec).err_counts.add((*trans).counter as usize)
                    } else {
                        *(*exec).counts.add((*trans).counter as usize)
                    };
                    if !(*exec).comp.is_null() {
                        counter = (*(*exec).comp).counters.add((*trans).counter as usize);
                    }
                    if counter.is_null() || count < (*counter).max {
                        if (*atom).neg != 0 {
                            *values.add(nb as usize) = (*atom).valuep2 as *mut XmlChar;
                            nb += 1;
                        } else {
                            *values.add(nb as usize) = (*atom).valuep as *mut XmlChar;
                            nb += 1;
                        }
                        (*nbval) += 1;
                    }
                } else if !(*exec).comp.is_null()
                    && !(*(*(*exec).comp).states.add((*trans).to as usize)).is_null()
                    && !matches!(
                        (*(*(*(*exec).comp).states.add((*trans).to as usize))).typ,
                        XmlRegStateType::XmlRegexpSinkState,
                    )
                {
                    if (*atom).neg != 0 {
                        *values.add(nb as usize) = (*atom).valuep2 as *mut XmlChar;
                        nb += 1;
                    } else {
                        *values.add(nb as usize) = (*atom).valuep as *mut XmlChar;
                        nb += 1;
                    }
                    *nbval += 1;
                }
                if nb >= maxval {
                    break;
                }
            }
        }
        if nb < maxval {
            for transno in 0..(*state).nb_trans {
                trans = (*state).trans.add(transno as usize);
                if (*trans).to < 0 {
                    continue;
                }
                atom = (*trans).atom;
                if atom.is_null() || (*atom).valuep.is_null() {
                    continue;
                }
                if (*trans).count as usize == REGEXP_ALL_LAX_COUNTER
                    || (*trans).count as usize == REGEXP_ALL_COUNTER
                    || (*trans).counter >= 0
                {
                    continue;
                } else if !(*(*(*exec).comp).states.add((*trans).to as usize)).is_null()
                    && matches!(
                        (*(*(*(*exec).comp).states.add((*trans).to as usize))).typ,
                        XmlRegStateType::XmlRegexpSinkState
                    )
                {
                    if (*atom).neg != 0 {
                        *values.add(nb as usize) = (*atom).valuep2 as *mut XmlChar;
                        nb += 1
                    } else {
                        *values.add(nb as usize) = (*atom).valuep as *mut XmlChar;
                        nb += 1;
                    }
                    *nbneg += 1;
                }
                if nb >= maxval {
                    break;
                }
            }
        }
    }
    0
}

/**
 * xmlRegExecNextValues:
 * @exec: a regexp execution context
 * @nbval: pointer to the number of accepted values IN/OUT
 * @nbneg: return number of negative transitions
 * @values: pointer to the array of acceptable values
 * @terminal: return value if this was a terminal state
 *
 * Extract information from the regexp execution,
 * the parameter @values must point to an array of @nbval string pointers
 * on return nbval will contain the number of possible strings in that
 * state and the @values array will be updated with them. The string values
 * returned will be freed with the @exec context and don't need to be
 * deallocated.
 *
 * Returns: 0 in case of success or -1 in case of error.
 */
pub unsafe extern "C" fn xml_reg_exec_next_values(
    exec: XmlRegExecCtxtPtr,
    nbval: *mut c_int,
    nbneg: *mut c_int,
    values: *mut *mut XmlChar,
    terminal: *mut c_int,
) -> c_int {
    xmlRegExecGetValues(exec, 0, nbval, nbneg, values, terminal)
}

/**
 * xmlRegExecErrInfo:
 * @exec: a regexp execution context generating an error
 * @string: return value for the error string
 * @nbval: pointer to the number of accepted values IN/OUT
 * @nbneg: return number of negative transitions
 * @values: pointer to the array of acceptable values
 * @terminal: return value if this was a terminal state
 *
 * Extract error information from the regexp execution, the parameter
 * @string will be updated with the value pushed and not accepted,
 * the parameter @values must point to an array of @nbval string pointers
 * on return nbval will contain the number of possible strings in that
 * state and the @values array will be updated with them. The string values
 * returned will be freed with the @exec context and don't need to be
 * deallocated.
 *
 * Returns: 0 in case of success or -1 in case of error.
 */
pub unsafe extern "C" fn xml_reg_exec_err_info(
    exec: XmlRegExecCtxtPtr,
    string: *mut *const XmlChar,
    nbval: *mut c_int,
    nbneg: *mut c_int,
    values: *mut *mut XmlChar,
    terminal: *mut c_int,
) -> c_int {
    if exec.is_null() {
        return -1;
    }
    if !string.is_null() {
        if (*exec).status != 0 {
            *string = (*exec).err_string;
        } else {
            *string = null_mut();
        }
    }
    xmlRegExecGetValues(exec, 1, nbval, nbneg, values, terminal)
}

/*
 * Formal regular expression handling
 * Its goal is to do some formal work on content models
 */

/* expressions are used within a context */
#[cfg(feature = "libxml_expr")]
pub type XmlExpCtxtPtr = *mut XmlExpCtxt;
#[cfg(feature = "libxml_expr")]
#[repr(C)]
pub struct XmlExpCtxt {
    dict: XmlDictPtr,
    table: *mut XmlExpNodePtr,
    size: c_int,
    nb_elems: c_int,
    nb_nodes: c_int,
    max_nodes: c_int,
    expr: *const c_char,
    cur: *const c_char,
    nb_cons: c_int,
    tab_size: c_int,
}

/**
 * xmlExpFreeCtxt:
 * @ctxt:  an expression context
 *
 * Free an expression context
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_free_ctxt(ctxt: XmlExpCtxtPtr) {
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

/**
 * xmlExpNewCtxt:
 * @maxNodes:  the maximum number of nodes
 * @dict:  optional dictionary to use internally
 *
 * Creates a new context for manipulating expressions
 *
 * Returns the context or NULL in case of error
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_new_ctxt(mut max_nodes: c_int, dict: XmlDictPtr) -> XmlExpCtxtPtr {
    use crate::libxml::dict::{xml_dict_create, xml_dict_reference};

    let size: c_int = 256;

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

/**
 * xmlExpCtxtNbNodes:
 * @ctxt: an expression context
 *
 * Debugging facility provides the number of allocated nodes at a that point
 *
 * Returns the number of nodes in use or -1 in case of error
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_ctxt_nb_nodes(ctxt: XmlExpCtxtPtr) -> c_int {
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).nb_nodes
}

/**
 * xmlExpCtxtNbCons:
 * @ctxt: an expression context
 *
 * Debugging facility provides the number of allocated nodes over lifetime
 *
 * Returns the number of nodes ever allocated or -1 in case of error
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_ctxt_nb_cons(ctxt: XmlExpCtxtPtr) -> c_int {
    if ctxt.is_null() {
        return -1;
    }
    (*ctxt).nb_cons
}

/* Expressions are trees but the tree is opaque */
#[cfg(feature = "libxml_expr")]
pub type XmlExpNodePtr = *mut XmlExpNode;
#[cfg(feature = "libxml_expr")]
#[repr(C)]
#[derive(Debug, Clone, Copy)]
struct Count {
    f_min: c_int,
    f_max: c_int,
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
    typ: c_uchar,  /* xmlExpNodeType */
    info: c_uchar, /* OR of XmlExpNodeInfo */
    key: c_ushort, /* the hash key */
    refe: c_uint,  /* The number of references */
    c_max: c_int,  /* the maximum length it can consume */
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

/*
 * 2 core expressions shared by all for the empty language set
 * and for the set with just the empty token
 */
#[cfg(feature = "libxml_expr")]
pub static mut FORBIDDEN_EXP: XmlExpNodePtr = null_mut();
#[cfg(feature = "libxml_expr")]
pub static mut EMPTY_EXP: XmlExpNodePtr = null_mut();

/*
 * Expressions are reference counted internally
 */
/**
 * xmlExpFree:
 * @ctxt: the expression context
 * @exp: the expression
 *
 * Dereference the expression
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_free(ctxt: XmlExpCtxtPtr, exp: XmlExpNodePtr) {
    if exp.is_null() || exp == FORBIDDEN_EXP || exp == EMPTY_EXP {
        return;
    }
    (*exp).refe -= 1;
    if (*exp).refe == 0 {
        /* Unlink it first from the hash table */
        let key: c_ushort = (*exp).key % (*ctxt).size as u16;
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

/**
 * xmlExpRef:
 * @exp: the expression
 *
 * Increase the reference count of the expression
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_ref(exp: XmlExpNodePtr) {
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
unsafe extern "C" fn xml_exp_parse_number(ctxt: XmlExpCtxtPtr) -> c_int {
    let mut ret: c_int = 0;

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
unsafe extern "C" fn xml_exp_parse_or(ctxt: XmlExpCtxtPtr) -> XmlExpNodePtr {
    let mut ret: XmlExpNodePtr;
    let val: *const XmlChar;

    SKIP_BLANKS!(ctxt);
    let base: *const c_char = (*ctxt).cur;
    if *(*ctxt).cur == b'(' as i8 {
        NEXT!(ctxt);
        ret = xml_exp_parse_expr(ctxt);
        SKIP_BLANKS!(ctxt);
        if *(*ctxt).cur != b')' as i8 {
            // fprintf(stderr, "unbalanced '(' : %s\n", base);
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
        let max: c_int;

        NEXT!(ctxt);
        let min: c_int = xml_exp_parse_number(ctxt);
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
unsafe extern "C" fn xml_exp_parse_seq(ctxt: XmlExpCtxtPtr) -> XmlExpNodePtr {
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
unsafe extern "C" fn xml_exp_parse_expr(ctxt: XmlExpCtxtPtr) -> XmlExpNodePtr {
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

/*
 * constructors can be either manual or from a string
 */
/**
 * xmlExpParse:
 * @ctxt: the expressions context
 * @expr: the 0 terminated string
 *
 * Minimal parser for regexps, it understand the following constructs
 *  - string terminals
 *  - choice operator |
 *  - sequence operator ,
 *  - subexpressions (...)
 *  - usual cardinality operators + * and ?
 *  - finite sequences  { min, max }
 *  - infinite sequences { min, * }
 * There is minimal checkings made especially no checking on strings values
 *
 * Returns a new expression or NULL in case of failure
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_parse(ctxt: XmlExpCtxtPtr, expr: *const c_char) -> XmlExpNodePtr {
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

/*
 * xmlExpHashNameComputeKey:
 * Calculate the hash key for a token
 */
#[cfg(feature = "libxml_expr")]
unsafe extern "C" fn xml_exp_hash_name_compute_key(mut name: *const XmlChar) -> c_ushort {
    let mut value: c_ushort = 0;
    let mut ch: c_char;

    if !name.is_null() {
        value += 30 * (*name) as c_ushort;
        while {
            ch = *name as _;
            name = name.add(1);
            ch != 0
        } {
            value ^= (((value as u64) << 5) + (value as u64 >> 3) + ch as c_ulong) as u16;
        }
    }
    value
}

/*
 * xmlExpHashComputeKey:
 * Calculate the hash key for a compound expression
 */
#[cfg(feature = "libxml_expr")]
unsafe extern "C" fn xml_exp_hash_compute_key(
    typ: XmlExpNodeType,
    left: XmlExpNodePtr,
    right: XmlExpNodePtr,
) -> c_ushort {
    let mut value: c_ulong;
    let ret: c_ushort;

    match typ {
        XmlExpNodeType::XmlExpSeq => {
            value = (*left).key as _;
            value += (*right).key as u64;
            value *= 3;
            ret = value as c_ushort;
        }
        XmlExpNodeType::XmlExpOr => {
            value = (*left).key as _;
            value += (*right).key as u64;
            value *= 7;
            ret = value as c_ushort;
        }
        XmlExpNodeType::XmlExpCount => {
            value = (*left).key as _;
            value += (*right).key as u64;
            ret = value as c_ushort;
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
unsafe extern "C" fn xml_exp_new_node(ctxt: XmlExpCtxtPtr, typ: XmlExpNodeType) -> XmlExpNodePtr {
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

/**
 * xmlExpHashGetEntry:
 * @table: the hash table
 *
 * Get the unique entry from the hash table. The entry is created if
 * needed. @left and @right are consumed, i.e. their refe count will
 * be decremented by the operation.
 *
 * Returns the pointer or NULL in case of error
 */
#[cfg(feature = "libxml_expr")]
unsafe extern "C" fn xml_exp_hash_get_entry(
    ctxt: XmlExpCtxtPtr,
    typ: XmlExpNodeType,
    mut left: XmlExpNodePtr,
    mut right: XmlExpNodePtr,
    name: *const XmlChar,
    min: c_int,
    max: c_int,
) -> XmlExpNodePtr {
    let mut kbase: c_ushort;
    let mut insert: XmlExpNodePtr;

    if ctxt.is_null() {
        return null_mut();
    }

    /*
     * Check for duplicate and insertion location.
     */
    if matches!(typ, XmlExpNodeType::XmlExpAtom) {
        kbase = xml_exp_hash_name_compute_key(name);
    } else if matches!(typ, XmlExpNodeType::XmlExpCount) {
        /* COUNT reduction rule 1 */
        /* a{1} -> a */
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
        /* Forbid reduction rules */
        if (*left).typ == XmlExpNodeType::XmlExpForbid as u8 {
            xml_exp_free(ctxt, left);
            return right;
        }
        if (*right).typ == XmlExpNodeType::XmlExpForbid as u8 {
            xml_exp_free(ctxt, right);
            return left;
        }

        /* OR reduction rule 1 */
        /* a | a reduced to a */
        if left == right {
            xml_exp_free(ctxt, right);
            return left;
        }
        /* OR canonicalization rule 1 */
        /* linearize (a | b) | c into a | (b | c) */
        if (*left).typ == XmlExpNodeType::XmlExpOr as u8
            && (*right).typ != XmlExpNodeType::XmlExpOr as u8
        {
            std::mem::swap(&mut left, &mut right);
        }
        /* OR reduction rule 2 */
        /* a | (a | b) and b | (a | b) are reduced to a | b */
        if (*right).typ == XmlExpNodeType::XmlExpOr as u8
            && (left == (*right).exp_left || left == (*right).field.children.f_right)
        {
            xml_exp_free(ctxt, left);
            return right;
        }
        /* OR canonicalization rule 2 */
        /* linearize (a | b) | c into a | (b | c) */
        if (*left).typ == XmlExpNodeType::XmlExpOr as u8 {
            let mut tmp: XmlExpNodePtr;

            /* OR canonicalization rule 2 */
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
            /* Ordering in the tree */
            /* C | (A | B) -> A | (B | C) */
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
            /* Ordering in the tree */
            /* B | (A | C) -> A | (B | C) */
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
        /* we know both types are != xmlExpNodeType::XML_EXP_OR here */
        else if (*left).key > (*right).key {
            std::mem::swap(&mut left, &mut right);
        }
        kbase = xml_exp_hash_compute_key(typ, left, right);
    } else if matches!(typ, XmlExpNodeType::XmlExpSeq) {
        /* Forbid reduction rules */
        if (*left).typ == XmlExpNodeType::XmlExpForbid as u8 {
            xml_exp_free(ctxt, right);
            return left;
        }
        if (*right).typ == XmlExpNodeType::XmlExpForbid as u8 {
            xml_exp_free(ctxt, left);
            return right;
        }
        /* Empty reduction rules */
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

    let key: c_ushort = kbase % (*ctxt).size as u16;
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

/**
 * xmlExpNewAtom:
 * @ctxt: the expression context
 * @name: the atom name
 * @len: the atom name length in byte (or -1);
 *
 * Get the atom associated to this name from that context
 *
 * Returns the node or NULL in case of error
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_new_atom(
    ctxt: XmlExpCtxtPtr,
    mut name: *const XmlChar,
    len: c_int,
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

/**
 * xmlExpNewOr:
 * @ctxt: the expression context
 * @left: left expression
 * @right: right expression
 *
 * Get the atom associated to the choice @left | @right
 * Note that @left and @right are consumed in the operation, to keep
 * an handle on them use xmlExpRef() and use xmlExpFree() to release them,
 * this is true even in case of failure (unless ctxt.is_null()).
 *
 * Returns the node or NULL in case of error
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_new_or(
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

/**
 * xmlExpNewSeq:
 * @ctxt: the expression context
 * @left: left expression
 * @right: right expression
 *
 * Get the atom associated to the sequence @left , @right
 * Note that @left and @right are consumed in the operation, to keep
 * an handle on them use xmlExpRef() and use xmlExpFree() to release them,
 * this is true even in case of failure (unless ctxt.is_null()).
 *
 * Returns the node or NULL in case of error
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_new_seq(
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

/**
 * xmlExpNewRange:
 * @ctxt: the expression context
 * @subset: the expression to be repeated
 * @min: the lower bound for the repetition
 * @max: the upper bound for the repetition, -1 means infinite
 *
 * Get the atom associated to the range (@subset){@min, @max}
 * Note that @subset is consumed in the operation, to keep
 * an handle on it use xmlExpRef() and use xmlExpFree() to release it,
 * this is true even in case of failure (unless ctxt.is_null()).
 *
 * Returns the node or NULL in case of error
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_new_range(
    ctxt: XmlExpCtxtPtr,
    subset: XmlExpNodePtr,
    min: c_int,
    max: c_int,
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

/*
 * The really interesting APIs
 */
/**
 * xmlExpIsNillable:
 * @exp: the expression
 *
 * Finds if the expression is nillable, i.e. if it accepts the empty sequence
 *
 * Returns 1 if nillable, 0 if not and -1 in case of error
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_is_nillable(exp: XmlExpNodePtr) -> c_int {
    if exp.is_null() {
        return -1;
    }
    (IS_NILLABLE!(exp) != 0) as i32
}

/**
 * xmlExpMaxToken:
 * @expr: a compiled expression
 *
 * Indicate the maximum number of input a expression can accept
 *
 * Returns the maximum length or -1 in case of error
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_max_token(expr: XmlExpNodePtr) -> c_int {
    if expr.is_null() {
        return -1;
    }
    (*expr).c_max
}

#[cfg(feature = "libxml_expr")]
unsafe extern "C" fn xml_exp_get_language_int(
    _ctxt: XmlExpCtxtPtr,
    mut exp: XmlExpNodePtr,
    list: *mut *const XmlChar,
    len: c_int,
    nb: c_int,
) -> c_int {
    let tmp: c_int;
    let tmp2: c_int;
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

/**
 * xmlExpGetLanguage:
 * @ctxt: the expression context
 * @exp: the expression
 * @langList: where to store the tokens
 * @len: the allocated length of @list
 *
 * Find all the strings used in @exp and store them in @list
 *
 * Returns the number of unique strings found, -1 in case of errors and
 *         -2 if there is more than @len strings
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_get_language(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    lang_list: *mut *const XmlChar,
    len: c_int,
) -> c_int {
    if ctxt.is_null() || exp.is_null() || lang_list.is_null() || len <= 0 {
        return -1;
    }
    xml_exp_get_language_int(ctxt, exp, lang_list, len, 0)
}

#[cfg(feature = "libxml_expr")]
unsafe extern "C" fn xml_exp_get_start_int(
    _ctxt: XmlExpCtxtPtr,
    mut exp: XmlExpNodePtr,
    list: *mut *const XmlChar,
    len: c_int,
    nb: c_int,
) -> c_int {
    let mut tmp: c_int;
    let tmp2: c_int;
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

/**
 * xmlExpGetStart:
 * @ctxt: the expression context
 * @exp: the expression
 * @tokList: where to store the tokens
 * @len: the allocated length of @list
 *
 * Find all the strings that appears at the start of the languages
 * accepted by @exp and store them in @list. E.g. for (a, b) | c
 * it will return the list [a, c]
 *
 * Returns the number of unique strings found, -1 in case of errors and
 *         -2 if there is more than @len strings
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_get_start(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    tok_list: *mut *const XmlChar,
    len: c_int,
) -> c_int {
    if ctxt.is_null() || exp.is_null() || tok_list.is_null() || len <= 0 {
        return -1;
    }
    xml_exp_get_start_int(ctxt, exp, tok_list, len, 0)
}

#[cfg(feature = "libxml_expr")]
unsafe extern "C" fn xml_exp_string_derive_int(
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
            // #ifdef DEBUG_DERIV
            // 		printf("deriv atom: equal => Empty\n");
            // #endif
            ret = EMPTY_EXP;
        } else {
            // #ifdef DEBUG_DERIV
            // 		printf("deriv atom: mismatch => forbid\n");
            // #endif
            /* TODO wildcards here */
            ret = FORBIDDEN_EXP;
        }
        return ret;
    } else if (*exp).typ == XmlExpNodeType::XmlExpOr as u8 {
        // #ifdef DEBUG_DERIV
        // 	    printf("deriv or: => or(derivs)\n");
        // #endif
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
        // #ifdef DEBUG_DERIV
        // 	    printf("deriv seq: starting with left\n");
        // #endif
        ret = xml_exp_string_derive_int(ctxt, (*exp).exp_left, str);
        if ret.is_null() {
            return null_mut();
        } else if ret == FORBIDDEN_EXP {
            if IS_NILLABLE!((*exp).exp_left) != 0 {
                // #ifdef DEBUG_DERIV
                // 		    printf("deriv seq: left failed but nillable\n");
                // #endif
                ret = xml_exp_string_derive_int(ctxt, (*exp).field.children.f_right, str);
            }
        } else {
            // #ifdef DEBUG_DERIV
            // 		printf("deriv seq: left match => sequence\n");
            // #endif
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
            // #ifdef DEBUG_DERIV
            // 		printf("deriv count: pattern mismatch => forbid\n");
            // #endif
            return ret;
        }
        if (*exp).field.count.f_max == 1 {
            return ret;
        }
        let max = if (*exp).field.count.f_max < 0
        /* unbounded */
        {
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
            // #ifdef DEBUG_DERIV
            // 		printf("deriv count: match to empty => new count\n");
            // #endif
            return tmp;
        }
        // #ifdef DEBUG_DERIV
        // 	    printf("deriv count: match => sequence with new count\n");
        // #endif
        return xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpSeq, ret, tmp, null(), 0, 0);
    }
    null_mut()
}

/**
 * xmlExpStringDerive:
 * @ctxt: the expression context
 * @exp: the expression
 * @str: the string
 * @len: the string len in bytes if available
 *
 * Do one step of Brzozowski derivation of the expression @exp with
 * respect to the input string
 *
 * Returns the resulting expression or NULL in case of internal error
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_string_derive(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    str: *const XmlChar,
    len: c_int,
) -> XmlExpNodePtr {
    use super::dict::xml_dict_exists;

    if exp.is_null() || ctxt.is_null() || str.is_null() {
        return null_mut();
    }
    /*
     * check the string is in the dictionary, if yes use an interned
     * copy, otherwise we know it's not an acceptable input
     */
    let input: *const XmlChar = xml_dict_exists((*ctxt).dict, str, len);
    if input.is_null() {
        return FORBIDDEN_EXP;
    }
    xml_exp_string_derive_int(ctxt, exp, input)
}

#[cfg(feature = "libxml_expr")]
unsafe extern "C" fn xml_exp_check_card(exp: XmlExpNodePtr, sub: XmlExpNodePtr) -> c_int {
    let mut ret: c_int = 1;

    if (*sub).c_max == -1 {
        if (*exp).c_max != -1 {
            ret = 0;
        }
    } else if (*exp).c_max >= 0 && (*exp).c_max < (*sub).c_max {
        ret = 0;
    }
    // #if 0
    //     if ((IS_NILLABLE(sub)) && (!IS_NILLABLE(exp)))
    //         ret = 0;
    // #endif
    ret
}

/**
 * xmlExpDivide:
 * @ctxt: the expressions context
 * @exp: the englobing expression
 * @sub: the subexpression
 * @mult: the multiple expression
 * @remain: the remain from the derivation of the multiple
 *
 * Check if exp is a multiple of sub, i.e. if there is a finite number n
 * so that sub{n} subsume exp
 *
 * Returns the multiple value if successful, 0 if it is not a multiple
 *         and -1 in case of internal error.
 */
#[cfg(feature = "libxml_expr")]
unsafe extern "C" fn xml_exp_divide(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    sub: XmlExpNodePtr,
    mult: *mut XmlExpNodePtr,
    remain: *mut XmlExpNodePtr,
) -> c_int {
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
            //  #ifdef DEBUG_DERIV
            //          printf("Divide succeeded %d\n", i);
            //  #endif
            return i;
        }
        xml_exp_free(ctxt, tmp);
        xml_exp_free(ctxt, tmp2);
    }
    //  #ifdef DEBUG_DERIV
    //      printf("Divide failed\n");
    //  #endif
    0
}

/**
 * xmlExpExpDeriveInt:
 * @ctxt: the expressions context
 * @exp: the englobing expression
 * @sub: the subexpression
 *
 * Try to do a step of Brzozowski derivation but at a higher level
 * the input being a subexpression.
 *
 * Returns the resulting expression or NULL in case of internal error
 */
unsafe extern "C" fn xml_exp_exp_derive_int(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    sub: XmlExpNodePtr,
) -> XmlExpNodePtr {
    let mut ret: XmlExpNodePtr;
    let mut tmp: XmlExpNodePtr;
    let mut tmp2: XmlExpNodePtr;
    let mut tmp3: XmlExpNodePtr;
    let mut tab: *mut *const XmlChar;
    let mut len: c_int;

    /*
     * In case of equality and if the expression can only consume a finite
     * amount, then the derivation is empty
     */
    if exp == sub && (*exp).c_max >= 0 {
        // #ifdef DEBUG_DERIV
        //         printf("Equal(exp, sub) and finite -> Empty\n");
        // #endif
        return EMPTY_EXP;
    }
    /*
     * decompose sub sequence first
     */
    if (*sub).typ == XmlExpNodeType::XmlExpEmpty as u8 {
        // #ifdef DEBUG_DERIV
        //         printf("Empty(sub) -> Empty\n");
        // #endif
        (*exp).refe += 1;
        return exp;
    }
    if (*sub).typ == XmlExpNodeType::XmlExpSeq as u8 {
        // #ifdef DEBUG_DERIV
        //         printf("Seq(sub) -> decompose\n");
        // #endif
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
        // #ifdef DEBUG_DERIV
        //         printf("Or(sub) -> decompose\n");
        // #endif
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
        // #ifdef DEBUG_DERIV
        //         printf("CheckCard(exp, sub) failed -> Forbid\n");
        // #endif
        return FORBIDDEN_EXP;
    }
    if (*exp).typ == XmlExpNodeType::XmlExpEmpty as u8 {
        if sub == EMPTY_EXP {
            return EMPTY_EXP;
        }
        // #ifdef DEBUG_DERIV
        // 	    printf("Empty(exp) -> Forbid\n");
        // #endif
        return FORBIDDEN_EXP;
    } else if (*exp).typ == XmlExpNodeType::XmlExpForbid as u8 {
        // #ifdef DEBUG_DERIV
        // 	    printf("Forbid(exp) -> Forbid\n");
        // #endif
        return FORBIDDEN_EXP;
    } else if (*exp).typ == XmlExpNodeType::XmlExpAtom as u8 {
        if (*sub).typ == XmlExpNodeType::XmlExpAtom as u8 {
            /* TODO: handle wildcards */
            if (*exp).field.f_str == (*sub).field.f_str {
                // #ifdef DEBUG_DERIV
                // 		    printf("Atom match -> Empty\n");
                // #endif
                return EMPTY_EXP;
            }
            // #ifdef DEBUG_DERIV
            // 		printf("Atom mismatch -> Forbid\n");
            // #endif
            return FORBIDDEN_EXP;
        }
        if (*sub).typ == XmlExpNodeType::XmlExpCount as u8
            && (*sub).field.count.f_max == 1
            && (*(*sub).exp_left).typ == XmlExpNodeType::XmlExpAtom as u8
        {
            /* TODO: handle wildcards */
            if (*exp).field.f_str == (*(*sub).exp_left).field.f_str {
                // #ifdef DEBUG_DERIV
                // 		    printf("Atom match -> Empty\n");
                // #endif
                return EMPTY_EXP;
            }
            // #ifdef DEBUG_DERIV
            // 		printf("Atom mismatch -> Forbid\n");
            // #endif
            return FORBIDDEN_EXP;
        }
        // #ifdef DEBUG_DERIV
        // 	    printf("Complex exp vs Atom -> Forbid\n");
        // #endif
        return FORBIDDEN_EXP;
    } else if (*exp).typ == XmlExpNodeType::XmlExpSeq as u8 {
        /* try to get the sequence consumed only if possible */
        if xml_exp_check_card((*exp).exp_left, sub) != 0 {
            /* See if the sequence can be consumed directly */
            // #ifdef DEBUG_DERIV
            // 		printf("Seq trying left only\n");
            // #endif
            ret = xml_exp_exp_derive_int(ctxt, (*exp).exp_left, sub);
            if ret != FORBIDDEN_EXP && !ret.is_null() {
                // #ifdef DEBUG_DERIV
                // 		    printf("Seq trying left only worked\n");
                // #endif
                /*
                 * TODO: assumption here that we are determinist
                 *       i.e. we won't get to a nillable exp left
                 *       subset which could be matched by the right
                 *       part too.
                 * e.g.: (a | b)+,(a | c) and 'a+,a'
                 */
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
            // #ifdef DEBUG_DERIV
            // 	    } else {
            // 		printf("Seq: left too short\n");
            // #endif
        }
        /* Try instead to decompose */
        if (*sub).typ == XmlExpNodeType::XmlExpCount as u8 {
            let min: c_int;
            let max: c_int;

            // #ifdef DEBUG_DERIV
            // 		printf("Seq: sub is a count\n");
            // #endif
            ret = xml_exp_exp_derive_int(ctxt, (*exp).exp_left, (*sub).exp_left);
            if ret.is_null() {
                return null_mut();
            }
            if ret != FORBIDDEN_EXP {
                // #ifdef DEBUG_DERIV
                // 		    printf("Seq , Count match on left\n");
                // #endif
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
        /* we made no progress on structured operations */
    } else if (*exp).typ == XmlExpNodeType::XmlExpOr as u8 {
        // #ifdef DEBUG_DERIV
        // 	    printf("Or , trying both side\n");
        // #endif
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
        let min: c_int;
        let max: c_int;

        if (*sub).typ == XmlExpNodeType::XmlExpCount as u8 {
            /*
             * Try to see if the loop is completely subsumed
             */
            tmp = xml_exp_exp_derive_int(ctxt, (*exp).exp_left, (*sub).exp_left);
            if tmp.is_null() {
                return null_mut();
            }
            if tmp == FORBIDDEN_EXP {
                // #ifdef DEBUG_DERIV
                // 		    printf("Count, Count inner don't subsume\n");
                // #endif
                let mult: c_int = xml_exp_divide(
                    ctxt,
                    (*sub).exp_left,
                    (*exp).exp_left,
                    null_mut(),
                    addr_of_mut!(tmp),
                );
                if mult <= 0 {
                    // #ifdef DEBUG_DERIV
                    // 			printf("Count, Count not multiple => forbidden\n");
                    // #endif
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
                        // #ifdef DEBUG_DERIV
                        // 			    printf("Count, Count finite can't subsume infinite\n");
                        // #endif
                        xml_exp_free(ctxt, tmp);
                        return FORBIDDEN_EXP;
                    }
                } else if (*exp).field.count.f_max == -1 {
                    // #ifdef DEBUG_DERIV
                    // 			    printf("Infinite loop consume mult finite loop\n");
                    // #endif
                    if (*exp).field.count.f_min > (*sub).field.count.f_min * mult {
                        max = -1;
                        min = (*exp).field.count.f_min - (*sub).field.count.f_min * mult;
                    } else {
                        max = -1;
                        min = 0;
                    }
                } else {
                    if (*exp).field.count.f_max < (*sub).field.count.f_max * mult {
                        // #ifdef DEBUG_DERIV
                        // 				printf("loops max mult mismatch => forbidden\n");
                        // #endif
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
                /*
                 * TODO: loop here to try to grow if working on finite
                 *       blocks.
                 */
                // #ifdef DEBUG_DERIV
                // 		    printf("Count, Count remain not nillable => forbidden\n");
                // #endif
                xml_exp_free(ctxt, tmp);
                return FORBIDDEN_EXP;
            } else if (*sub).field.count.f_max == -1 {
                if (*exp).field.count.f_max == -1 {
                    if (*exp).field.count.f_min <= (*sub).field.count.f_min {
                        // #ifdef DEBUG_DERIV
                        // 			    printf("Infinite loops Okay => COUNT(0,Inf)\n");
                        // #endif
                        max = -1;
                        min = 0;
                    } else {
                        // #ifdef DEBUG_DERIV
                        // 			    printf("Infinite loops min => Count(X,Inf)\n");
                        // #endif
                        max = -1;
                        min = (*exp).field.count.f_min - (*sub).field.count.f_min;
                    }
                } else if (*exp).field.count.f_min > (*sub).field.count.f_min {
                    // #ifdef DEBUG_DERIV
                    // 			printf("loops min mismatch 1 => forbidden ???\n");
                    // #endif
                    xml_exp_free(ctxt, tmp);
                    return FORBIDDEN_EXP;
                } else {
                    max = -1;
                    min = 0;
                }
            } else if (*exp).field.count.f_max == -1 {
                // #ifdef DEBUG_DERIV
                // 			printf("Infinite loop consume finite loop\n");
                // #endif
                if (*exp).field.count.f_min > (*sub).field.count.f_min {
                    max = -1;
                    min = (*exp).field.count.f_min - (*sub).field.count.f_min;
                } else {
                    max = -1;
                    min = 0;
                }
            } else {
                if (*exp).field.count.f_max < (*sub).field.count.f_max {
                    // #ifdef DEBUG_DERIV
                    // 			    printf("loops max mismatch => forbidden\n");
                    // #endif
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
            // #ifdef DEBUG_DERIV
            // 		printf("loops match => SEQ(COUNT())\n");
            // #endif
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
            // #ifdef DEBUG_DERIV
            // 		printf("loop mismatch => forbidden\n");
            // #endif
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

        // #ifdef DEBUG_DERIV
        // 	    printf("loop match => SEQ(COUNT())\n");
        // #endif
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

    // #ifdef DEBUG_DERIV
    //     printf("Fallback to derivative\n");
    // #endif
    if IS_NILLABLE!(sub) != 0 {
        if IS_NILLABLE!(exp) == 0 {
            return FORBIDDEN_EXP;
        } else {
            ret = EMPTY_EXP;
        }
    } else {
        ret = null_mut();
    }
    /*
     * here the structured derivation made no progress so
     * we use the default token based derivation to force one more step
     */
    if (*ctxt).tab_size == 0 {
        (*ctxt).tab_size = 40;
    }

    tab =
        xml_malloc((*ctxt).tab_size as usize * size_of::<*const XmlChar>()) as *mut *const XmlChar;
    if tab.is_null() {
        return null_mut();
    }

    /*
     * collect all the strings accepted by the subexpression on input
     */
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

/**
 * xmlExpExpDerive:
 * @ctxt: the expressions context
 * @exp: the englobing expression
 * @sub: the subexpression
 *
 * Evaluates the expression resulting from @exp consuming a sub expression @sub
 * Based on algebraic derivation and sometimes direct Brzozowski derivation
 * it usually takes less than linear time and can handle expressions generating
 * infinite languages.
 *
 * Returns the resulting expression or NULL in case of internal error, the
 *         result must be freed
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_exp_derive(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    sub: XmlExpNodePtr,
) -> XmlExpNodePtr {
    if exp.is_null() || ctxt.is_null() || sub.is_null() {
        return null_mut();
    }

    /*
     * O(1) speedups
     */
    if IS_NILLABLE!(sub) != 0 && IS_NILLABLE!(exp) == 0 {
        // #ifdef DEBUG_DERIV
        // 	printf("Sub nillable and not exp : can't subsume\n");
        // #endif
        return FORBIDDEN_EXP;
    }
    if xml_exp_check_card(exp, sub) == 0 {
        // #ifdef DEBUG_DERIV
        // 	printf("sub generate longer sequences than exp : can't subsume\n");
        // #endif
        return FORBIDDEN_EXP;
    }
    xml_exp_exp_derive_int(ctxt, exp, sub)
}

/**
 * xmlExpSubsume:
 * @ctxt: the expressions context
 * @exp: the englobing expression
 * @sub: the subexpression
 *
 * Check whether @exp accepts all the languages accepted by @sub
 * the input being a subexpression.
 *
 * Returns 1 if true 0 if false and -1 in case of failure.
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_subsume(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    sub: XmlExpNodePtr,
) -> c_int {
    if exp.is_null() || ctxt.is_null() || sub.is_null() {
        return -1;
    }

    /*
     * TODO: speedup by checking the language of sub is a subset of the
     *       language of exp
     */
    /*
     * O(1) speedups
     */
    if IS_NILLABLE!(sub) != 0 && IS_NILLABLE!(exp) == 0 {
        // #ifdef DEBUG_DERIV
        // 	printf("Sub nillable and not exp : can't subsume\n");
        // #endif
        return 0;
    }
    if xml_exp_check_card(exp, sub) == 0 {
        // #ifdef DEBUG_DERIV
        // 	printf("sub generate longer sequences than exp : can't subsume\n");
        // #endif
        return 0;
    }
    let tmp: XmlExpNodePtr = xml_exp_exp_derive_int(ctxt, exp, sub);
    // #ifdef DEBUG_DERIV
    //     printf("Result derivation :\n");
    //     PRINT_EXP(tmp);
    // #endif
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

unsafe extern "C" fn xml_exp_dump_int(buf: XmlBufferPtr, expr: XmlExpNodePtr, glob: c_int) {
    let mut c: XmlExpNodePtr;

    if expr.is_null() {
        return;
    }
    if glob != 0 {
        xml_buffer_write_char(buf, c"(".as_ptr());
    }
    if (*expr).typ == XmlExpNodeType::XmlExpEmpty as u8 {
        xml_buffer_write_char(buf, c"empty".as_ptr());
    } else if (*expr).typ == XmlExpNodeType::XmlExpForbid as u8 {
        xml_buffer_write_char(buf, c"forbidden".as_ptr());
    } else if (*expr).typ == XmlExpNodeType::XmlExpAtom as u8 {
        xml_buffer_write_xml_char(buf, (*expr).field.f_str);
    } else if (*expr).typ == XmlExpNodeType::XmlExpSeq as u8 {
        c = (*expr).exp_left;
        if (*c).typ == XmlExpNodeType::XmlExpSeq as u8 || (*c).typ == XmlExpNodeType::XmlExpOr as u8
        {
            xml_exp_dump_int(buf, c, 1);
        } else {
            xml_exp_dump_int(buf, c, 0);
        }
        xml_buffer_write_char(buf, c" , ".as_ptr());
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
        xml_buffer_write_char(buf, c" | ".as_ptr());
        c = (*expr).field.children.f_right;
        if (*c).typ == XmlExpNodeType::XmlExpSeq as u8 || (*c).typ == XmlExpNodeType::XmlExpOr as u8
        {
            xml_exp_dump_int(buf, c, 1);
        } else {
            xml_exp_dump_int(buf, c, 0);
        }
    } else if (*expr).typ == XmlExpNodeType::XmlExpCount as u8 {
        let mut rep: [c_char; 40] = [0; 40];

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
        xml_buffer_write_char(buf, rep.as_ptr());
    } else {
        eprintln!("Error in tree");
        // fprintf(stderr, "Error in tree\n");
    }
    if glob != 0 {
        xml_buffer_write_char(buf, c")".as_ptr());
    }
}

/**
 * xmlExpDump:
 * @buf:  a buffer to receive the output
 * @expr:  the compiled expression
 *
 * Serialize the expression as compiled to the buffer
 */
#[cfg(feature = "libxml_expr")]
pub unsafe extern "C" fn xml_exp_dump(buf: XmlBufferPtr, expr: XmlExpNodePtr) {
    if buf.is_null() || expr.is_null() {
        return;
    }
    xml_exp_dump_int(buf, expr, 0);
}

#[cfg(test)]
mod tests {
    use crate::{
        libxml::{xmlerror::xml_reset_last_error, xmlmemory::xml_mem_blocks},
        test_util::*,
    };

    use super::*;

    #[test]
    fn test_xml_exp_ctxt_nb_cons() {
        #[cfg(all(feature = "regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_EXP_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_exp_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_exp_ctxt_nb_cons(ctxt);
                desret_int(ret_val);
                des_xml_exp_ctxt_ptr(n_ctxt, ctxt, 0);
                xml_reset_last_error();
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
        #[cfg(all(feature = "regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_ctxt in 0..GEN_NB_XML_EXP_CTXT_PTR {
                let mem_base = xml_mem_blocks();
                let ctxt = gen_xml_exp_ctxt_ptr(n_ctxt, 0);

                let ret_val = xml_exp_ctxt_nb_nodes(ctxt);
                desret_int(ret_val);
                des_xml_exp_ctxt_ptr(n_ctxt, ctxt, 0);
                xml_reset_last_error();
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
    fn test_xml_exp_dump() {
        #[cfg(all(feature = "regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_buf in 0..GEN_NB_XML_BUFFER_PTR {
                for n_expr in 0..GEN_NB_XML_EXP_NODE_PTR {
                    let mem_base = xml_mem_blocks();
                    let buf = gen_xml_buffer_ptr(n_buf, 0);
                    let expr = gen_xml_exp_node_ptr(n_expr, 1);

                    xml_exp_dump(buf, expr);
                    des_xml_buffer_ptr(n_buf, buf, 0);
                    des_xml_exp_node_ptr(n_expr, expr, 1);
                    xml_reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlExpDump",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlExpDump()");
                        eprint!(" {}", n_buf);
                        eprintln!(" {}", n_expr);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_exp_exp_derive() {

        /* missing type support */
    }

    #[test]
    fn test_xml_exp_get_language() {
        #[cfg(all(feature = "regexp", feature = "libxml_expr"))]
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
                            xml_reset_last_error();
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
        #[cfg(all(feature = "regexp", feature = "libxml_expr"))]
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
                            xml_reset_last_error();
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
        #[cfg(all(feature = "regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_exp in 0..GEN_NB_XML_EXP_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let exp = gen_xml_exp_node_ptr(n_exp, 0);

                let ret_val = xml_exp_is_nillable(exp);
                desret_int(ret_val);
                des_xml_exp_node_ptr(n_exp, exp, 0);
                xml_reset_last_error();
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
        #[cfg(all(feature = "regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_expr in 0..GEN_NB_XML_EXP_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let expr = gen_xml_exp_node_ptr(n_expr, 0);

                let ret_val = xml_exp_max_token(expr);
                desret_int(ret_val);
                des_xml_exp_node_ptr(n_expr, expr, 0);
                xml_reset_last_error();
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
        #[cfg(all(feature = "regexp", feature = "libxml_expr"))]
        unsafe {
            let mut leaks = 0;

            for n_exp in 0..GEN_NB_XML_EXP_NODE_PTR {
                let mem_base = xml_mem_blocks();
                let exp = gen_xml_exp_node_ptr(n_exp, 0);

                xml_exp_ref(exp);
                des_xml_exp_node_ptr(n_exp, exp, 0);
                xml_reset_last_error();
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
        #[cfg(all(feature = "regexp", feature = "libxml_expr"))]
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
                        xml_reset_last_error();
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
    fn test_xml_reg_exec_err_info() {
        #[cfg(feature = "regexp")]
        unsafe {
            let mut leaks = 0;

            for n_exec in 0..GEN_NB_XML_REG_EXEC_CTXT_PTR {
                for n_string in 0..GEN_NB_CONST_XML_CHAR_PTR_PTR {
                    for n_nbval in 0..GEN_NB_INT_PTR {
                        for n_nbneg in 0..GEN_NB_INT_PTR {
                            for n_values in 0..GEN_NB_XML_CHAR_PTR_PTR {
                                for n_terminal in 0..GEN_NB_INT_PTR {
                                    let mem_base = xml_mem_blocks();
                                    let exec = gen_xml_reg_exec_ctxt_ptr(n_exec, 0);
                                    let string = gen_const_xml_char_ptr_ptr(n_string, 1);
                                    let nbval = gen_int_ptr(n_nbval, 2);
                                    let nbneg = gen_int_ptr(n_nbneg, 3);
                                    let values = gen_xml_char_ptr_ptr(n_values, 4);
                                    let terminal = gen_int_ptr(n_terminal, 5);

                                    let ret_val = xml_reg_exec_err_info(
                                        exec,
                                        string as *mut *const XmlChar,
                                        nbval,
                                        nbneg,
                                        values,
                                        terminal,
                                    );
                                    desret_int(ret_val);
                                    des_xml_reg_exec_ctxt_ptr(n_exec, exec, 0);
                                    des_const_xml_char_ptr_ptr(
                                        n_string,
                                        string as *mut *const XmlChar,
                                        1,
                                    );
                                    des_int_ptr(n_nbval, nbval, 2);
                                    des_int_ptr(n_nbneg, nbneg, 3);
                                    des_xml_char_ptr_ptr(n_values, values, 4);
                                    des_int_ptr(n_terminal, terminal, 5);
                                    xml_reset_last_error();
                                    if mem_base != xml_mem_blocks() {
                                        leaks += 1;
                                        eprint!(
                                            "Leak of {} blocks found in xmlRegExecErrInfo",
                                            xml_mem_blocks() - mem_base
                                        );
                                        assert!(
                                            leaks == 0,
                                            "{leaks} Leaks are found in xmlRegExecErrInfo()"
                                        );
                                        eprint!(" {}", n_exec);
                                        eprint!(" {}", n_string);
                                        eprint!(" {}", n_nbval);
                                        eprint!(" {}", n_nbneg);
                                        eprint!(" {}", n_values);
                                        eprintln!(" {}", n_terminal);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_reg_exec_next_values() {
        #[cfg(feature = "regexp")]
        unsafe {
            let mut leaks = 0;

            for n_exec in 0..GEN_NB_XML_REG_EXEC_CTXT_PTR {
                for n_nbval in 0..GEN_NB_INT_PTR {
                    for n_nbneg in 0..GEN_NB_INT_PTR {
                        for n_values in 0..GEN_NB_XML_CHAR_PTR_PTR {
                            for n_terminal in 0..GEN_NB_INT_PTR {
                                let mem_base = xml_mem_blocks();
                                let exec = gen_xml_reg_exec_ctxt_ptr(n_exec, 0);
                                let nbval = gen_int_ptr(n_nbval, 1);
                                let nbneg = gen_int_ptr(n_nbneg, 2);
                                let values = gen_xml_char_ptr_ptr(n_values, 3);
                                let terminal = gen_int_ptr(n_terminal, 4);

                                let ret_val =
                                    xml_reg_exec_next_values(exec, nbval, nbneg, values, terminal);
                                desret_int(ret_val);
                                des_xml_reg_exec_ctxt_ptr(n_exec, exec, 0);
                                des_int_ptr(n_nbval, nbval, 1);
                                des_int_ptr(n_nbneg, nbneg, 2);
                                des_xml_char_ptr_ptr(n_values, values, 3);
                                des_int_ptr(n_terminal, terminal, 4);
                                xml_reset_last_error();
                                if mem_base != xml_mem_blocks() {
                                    leaks += 1;
                                    eprint!(
                                        "Leak of {} blocks found in xmlRegExecNextValues",
                                        xml_mem_blocks() - mem_base
                                    );
                                    assert!(
                                        leaks == 0,
                                        "{leaks} Leaks are found in xmlRegExecNextValues()"
                                    );
                                    eprint!(" {}", n_exec);
                                    eprint!(" {}", n_nbval);
                                    eprint!(" {}", n_nbneg);
                                    eprint!(" {}", n_values);
                                    eprintln!(" {}", n_terminal);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_reg_exec_push_string() {
        #[cfg(feature = "regexp")]
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
                        xml_reset_last_error();
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
        #[cfg(feature = "regexp")]
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
                            xml_reset_last_error();
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
        #[cfg(feature = "regexp")]
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
                    xml_reset_last_error();
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
        #[cfg(feature = "regexp")]
        unsafe {
            let mut leaks = 0;

            for n_comp in 0..GEN_NB_XML_REGEXP_PTR {
                let mem_base = xml_mem_blocks();
                let comp = gen_xml_regexp_ptr(n_comp, 0);

                let ret_val = xml_regexp_is_determinist(comp);
                desret_int(ret_val);
                des_xml_regexp_ptr(n_comp, comp, 0);
                xml_reset_last_error();
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
        #[cfg(feature = "regexp")]
        unsafe {
            let mut leaks = 0;

            for n_output in 0..GEN_NB_FILE_PTR {
                for n_regexp in 0..GEN_NB_XML_REGEXP_PTR {
                    let mem_base = xml_mem_blocks();
                    let output = gen_file_ptr(n_output, 0);
                    let regexp = gen_xml_regexp_ptr(n_regexp, 1);

                    xml_regexp_print(output, regexp);
                    des_file_ptr(n_output, output, 0);
                    des_xml_regexp_ptr(n_regexp, regexp, 1);
                    xml_reset_last_error();
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

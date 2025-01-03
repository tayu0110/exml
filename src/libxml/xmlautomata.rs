//! Provide methods and data structures for building regexp automata.  
//! This module is based on `libxml/xmlautomata.h`, `xmlregexp.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

// Copyright of the original code is the following.
// Summary: API to build regexp automata
// Description: the API to build regexp automata
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
    os::raw::c_void,
    ptr::{null, null_mut},
};

use libc::{memcpy, snprintf, strlen};

use crate::libxml::{
    globals::xml_malloc_atomic,
    xmlregexp::{
        xml_fa_computes_determinism, xml_fa_eliminate_epsilon_transitions,
        xml_fa_generate_counted_epsilon_transition, xml_fa_generate_counted_transition,
        xml_fa_generate_epsilon_transition, xml_fa_generate_transitions, xml_reg_atom_push,
        xml_reg_epx_from_parse, xml_reg_free_atom, xml_reg_free_parser_ctxt, xml_reg_get_counter,
        xml_reg_new_atom, xml_reg_new_parser_ctxt, xml_reg_state_add_trans, xml_reg_state_push,
        XmlRegAtomPtr, XmlRegAtomType, XmlRegCounter, XmlRegMarkedType, XmlRegParserCtxtPtr,
        XmlRegQuantType, XmlRegStatePtr, XmlRegStateType, XmlRegTrans, XmlRegexp,
        REGEXP_ALL_COUNTER, REGEXP_ALL_LAX_COUNTER,
    },
    xmlstring::{xml_strdup, XmlChar},
};

/// A libxml automata description, It can be compiled into a regexp
#[doc(alias = "xmlAutomataPtr")]
pub type XmlAutomataPtr = *mut XmlAutomata;
#[repr(C)]
pub struct XmlAutomata {
    pub(crate) string: *mut XmlChar,
    pub(crate) cur: *mut XmlChar,
    pub(crate) error: i32,
    pub(crate) neg: i32,
    pub(crate) start: XmlRegStatePtr,
    pub(crate) end: XmlRegStatePtr,
    pub(crate) state: XmlRegStatePtr,
    pub(crate) atom: XmlRegAtomPtr,
    pub(crate) max_atoms: i32,
    pub(crate) nb_atoms: i32,
    pub(crate) atoms: *mut XmlRegAtomPtr,
    pub(crate) max_states: i32,
    pub(crate) nb_states: i32,
    pub(crate) states: *mut XmlRegStatePtr,
    pub(crate) max_counters: i32,
    pub(crate) nb_counters: i32,
    pub(crate) counters: *mut XmlRegCounter,
    pub(crate) determinist: i32,
    pub(crate) negs: i32,
    pub(crate) flags: i32,
    pub(crate) depth: i32,
}

/// A state int the automata description,
#[doc(alias = "xmlAutomataStatePtr")]
pub type XmlAutomataStatePtr = *mut XmlAutomataState;
#[repr(C)]
pub struct XmlAutomataState {
    pub(crate) typ: XmlRegStateType,
    pub(crate) mark: XmlRegMarkedType,
    pub(crate) markd: XmlRegMarkedType,
    pub(crate) reached: XmlRegMarkedType,
    pub(crate) no: i32,
    pub(crate) max_trans: i32,
    pub(crate) nb_trans: i32,
    pub(crate) trans: *mut XmlRegTrans,
    /*  knowing states pointing to us can speed things up */
    pub(crate) max_trans_to: i32,
    pub(crate) nb_trans_to: i32,
    pub(crate) trans_to: *mut i32,
}

/// Create a new automata
///
/// Returns the new object or NULL in case of failure
#[doc(alias = "xmlNewAutomata")]
pub unsafe extern "C" fn xml_new_automata() -> XmlAutomataPtr {
    let ctxt: XmlAutomataPtr = xml_reg_new_parser_ctxt(null());
    if ctxt.is_null() {
        return null_mut();
    }

    /* initialize the parser */
    (*ctxt).state = xml_reg_state_push(ctxt);
    if (*ctxt).state.is_null() {
        xml_free_automata(ctxt);
        return null_mut();
    }
    (*ctxt).start = (*ctxt).state;
    (*ctxt).end = null_mut();

    (*(*ctxt).start).typ = XmlRegStateType::XmlRegexpStartState;
    (*ctxt).flags = 0;

    ctxt
}

/// Free an automata
#[doc(alias = "xmlFreeAutomata")]
pub unsafe extern "C" fn xml_free_automata(am: XmlAutomataPtr) {
    if am.is_null() {
        return;
    }
    xml_reg_free_parser_ctxt(am);
}

/// Initial state lookup
///
/// Returns the initial state of the automata
#[doc(alias = "xmlAutomataGetInitState")]
pub unsafe extern "C" fn xml_automata_get_init_state(am: XmlAutomataPtr) -> XmlAutomataStatePtr {
    if am.is_null() {
        return null_mut();
    }
    (*am).start
}

/// Makes that state a final state
///
/// Returns 0 or -1 in case of error
#[doc(alias = "xmlAutomataSetFinalState")]
pub unsafe extern "C" fn xml_automata_set_final_state(
    am: XmlAutomataPtr,
    state: XmlAutomataStatePtr,
) -> i32 {
    if am.is_null() || state.is_null() {
        return -1;
    }
    (*state).typ = XmlRegStateType::XmlRegexpFinalState;
    0
}

/// Create a new disconnected state in the automata
///
/// Returns the new state or NULL in case of error
#[doc(alias = "xmlAutomataNewState")]
pub unsafe extern "C" fn xml_automata_new_state(am: XmlAutomataPtr) -> XmlAutomataStatePtr {
    if am.is_null() {
        return null_mut();
    }
    xml_reg_state_push(am)
}

/// If @to is NULL, this creates first a new target state in the automata
/// and then adds a transition from the @from state to the target state
/// activated by the value of @token
///
/// Returns the target state or NULL in case of error
#[doc(alias = "xmlAutomataNewTransition")]
pub unsafe extern "C" fn xml_automata_new_transition(
    am: XmlAutomataPtr,
    from: XmlAutomataStatePtr,
    to: XmlAutomataStatePtr,
    token: *const XmlChar,
    data: *mut c_void,
) -> XmlAutomataStatePtr {
    if am.is_null() || from.is_null() || token.is_null() {
        return null_mut();
    }
    let atom: XmlRegAtomPtr = xml_reg_new_atom(am, Some(XmlRegAtomType::XmlRegexpString));
    if atom.is_null() {
        return null_mut();
    }
    (*atom).data = data;
    (*atom).valuep = xml_strdup(token as _) as _;

    if xml_fa_generate_transitions(am, from, to, atom) < 0 {
        xml_reg_free_atom(atom);
        return null_mut();
    }
    if to.is_null() {
        return (*am).state;
    }
    to
}

/// If @to is NULL, this creates first a new target state in the automata
/// and then adds a transition from the @from state to the target state
/// activated by the value of @token
///
/// Returns the target state or NULL in case of error
#[doc(alias = "xmlAutomataNewTransition2")]
pub unsafe extern "C" fn xml_automata_new_transition2(
    am: XmlAutomataPtr,
    from: XmlAutomataStatePtr,
    to: XmlAutomataStatePtr,
    token: *const XmlChar,
    token2: *const XmlChar,
    data: *mut c_void,
) -> XmlAutomataStatePtr {
    if am.is_null() || from.is_null() || token.is_null() {
        return null_mut();
    }
    let atom: XmlRegAtomPtr = xml_reg_new_atom(am, Some(XmlRegAtomType::XmlRegexpString));
    if atom.is_null() {
        return null_mut();
    }
    (*atom).data = data;
    if token2.is_null() || *token2 == 0 {
        (*atom).valuep = xml_strdup(token as _) as _;
    } else {
        let lenn: i32 = strlen(token2 as _) as _;
        let lenp: i32 = strlen(token as _) as _;

        let str: *mut XmlChar = xml_malloc_atomic(lenn as usize + lenp as usize + 2) as _;
        if str.is_null() {
            xml_reg_free_atom(atom);
            return null_mut();
        }
        memcpy(str.add(0) as _, token as _, lenp as _);
        *str.add(lenp as usize) = b'|';
        memcpy(str.add(lenp as usize + 1) as _, token2 as _, lenn as usize);
        *str.add(lenn as usize + lenp as usize + 1) = 0;

        (*atom).valuep = str as _;
    }

    if xml_fa_generate_transitions(am, from, to, atom) < 0 {
        xml_reg_free_atom(atom);
        return null_mut();
    }
    if to.is_null() {
        return (*am).state;
    }
    to
}

/// If @to is NULL, this creates first a new target state in the automata
/// and then adds a transition from the @from state to the target state
/// activated by any value except (@token,@token2)
/// Note that if @token2 is not NULL, then (X, NULL) won't match to follow
/// the semantic of XSD ##other
///
/// Returns the target state or NULL in case of error
#[doc(alias = "xmlAutomataNewNegTrans")]
pub unsafe extern "C" fn xml_automata_new_neg_trans(
    am: XmlAutomataPtr,
    from: XmlAutomataStatePtr,
    to: XmlAutomataStatePtr,
    token: *const XmlChar,
    token2: *const XmlChar,
    data: *mut c_void,
) -> XmlAutomataStatePtr {
    let mut err_msg: [XmlChar; 200] = [0; 200];

    if am.is_null() || from.is_null() || token.is_null() {
        return null_mut();
    }
    let atom: XmlRegAtomPtr = xml_reg_new_atom(am, Some(XmlRegAtomType::XmlRegexpString));
    if atom.is_null() {
        return null_mut();
    }
    (*atom).data = data;
    (*atom).neg = 1;
    if token2.is_null() || *token2 == 0 {
        (*atom).valuep = xml_strdup(token as _) as _;
    } else {
        let lenn: i32 = strlen(token2 as _) as _;
        let lenp: i32 = strlen(token as _) as _;

        let str: *mut XmlChar = xml_malloc_atomic(lenn as usize + lenp as usize + 2) as _;
        if str.is_null() {
            xml_reg_free_atom(atom);
            return null_mut();
        }
        memcpy(str.add(0) as _, token as _, lenp as usize);
        *str.add(lenp as usize) = b'|';
        memcpy(str.add(lenp as usize + 1) as _, token2 as _, lenn as usize);
        *str.add(lenn as usize + lenp as usize + 1) = 0;

        (*atom).valuep = str as _;
    }
    snprintf(
        err_msg.as_mut_ptr() as _,
        199,
        c"not %s".as_ptr() as _,
        (*atom).valuep,
    );
    err_msg[199] = 0;
    (*atom).valuep2 = xml_strdup(err_msg.as_mut_ptr() as _) as _;

    if xml_fa_generate_transitions(am, from, to, atom) < 0 {
        xml_reg_free_atom(atom);
        return null_mut();
    }
    (*am).negs += 1;
    if to.is_null() {
        return (*am).state;
    }
    to
}

/// If @to is NULL, this creates first a new target state in the automata
/// and then adds a transition from the @from state to the target state
/// activated by a succession of input of value @token and whose number
/// is between @min and @max
///
/// Returns the target state or NULL in case of error
#[doc(alias = "xmlAutomataNewCountTrans")]
pub unsafe extern "C" fn xml_automata_new_count_trans(
    am: XmlAutomataPtr,
    from: XmlAutomataStatePtr,
    mut to: XmlAutomataStatePtr,
    token: *const XmlChar,
    min: i32,
    max: i32,
    data: *mut c_void,
) -> XmlAutomataStatePtr {
    if am.is_null() || from.is_null() || token.is_null() {
        return null_mut();
    }
    if min < 0 {
        return null_mut();
    }
    if max < min || max < 1 {
        return null_mut();
    }
    let atom: XmlRegAtomPtr = xml_reg_new_atom(am, Some(XmlRegAtomType::XmlRegexpString));
    if atom.is_null() {
        return null_mut();
    }
    (*atom).valuep = xml_strdup(token as _) as _;
    if (*atom).valuep.is_null() {
        // goto error;
        xml_reg_free_atom(atom);
        return null_mut();
    }
    (*atom).data = data;
    if min == 0 {
        (*atom).min = 1;
    } else {
        (*atom).min = min;
    }
    (*atom).max = max;

    /*
     * associate a counter to the transition.
     */
    let counter: i32 = xml_reg_get_counter(am);
    if counter < 0 {
        // goto error;
        xml_reg_free_atom(atom);
        return null_mut();
    }
    (*(*am).counters.add(counter as usize)).min = min;
    (*(*am).counters.add(counter as usize)).max = max;

    /* xmlFAGenerateTransitions(am, from, to, atom); */
    if to.is_null() {
        to = xml_reg_state_push(am);
        if to.is_null() {
            // goto error;
            xml_reg_free_atom(atom);
            return null_mut();
        }
    }
    xml_reg_state_add_trans(am, from, atom, to, counter, -1);
    if xml_reg_atom_push(am, atom) < 0 {
        // goto error;
        xml_reg_free_atom(atom);
        return null_mut();
    }
    (*am).state = to;

    if to.is_null() {
        to = (*am).state;
    }
    if to.is_null() {
        return null_mut();
    }
    if min == 0 {
        xml_fa_generate_epsilon_transition(am, from, to);
    }
    to

    // error:
    //     xmlRegFreeAtom(atom);
    //     return null_mut();
}

/// If @to is NULL, this creates first a new target state in the automata
/// and then adds a transition from the @from state to the target state
/// activated by a succession of input of value @token and @token2 and
/// whose number is between @min and @max
///
/// Returns the target state or NULL in case of error
#[doc(alias = "xmlAutomataNewCountTrans2")]
pub unsafe extern "C" fn xml_automata_new_count_trans2(
    am: XmlAutomataPtr,
    from: XmlAutomataStatePtr,
    mut to: XmlAutomataStatePtr,
    token: *const XmlChar,
    token2: *const XmlChar,
    min: i32,
    max: i32,
    data: *mut c_void,
) -> XmlAutomataStatePtr {
    if am.is_null() || from.is_null() || token.is_null() {
        return null_mut();
    }
    if min < 0 {
        return null_mut();
    }
    if max < min || max < 1 {
        return null_mut();
    }
    let atom: XmlRegAtomPtr = xml_reg_new_atom(am, Some(XmlRegAtomType::XmlRegexpString));
    if atom.is_null() {
        return null_mut();
    }
    if token2.is_null() || *token2 == 0 {
        (*atom).valuep = xml_strdup(token as _) as _;
        if (*atom).valuep.is_null() {
            // goto error;
            xml_reg_free_atom(atom);
            return null_mut();
        }
    } else {
        let lenn: i32 = strlen(token2 as _) as _;
        let lenp: i32 = strlen(token as _) as _;

        let str: *mut XmlChar = xml_malloc_atomic(lenn as usize + lenp as usize + 2) as _;
        if str.is_null() {
            // goto error;
            xml_reg_free_atom(atom);
            return null_mut();
        }
        memcpy(str.add(0) as _, token as _, lenp as usize);
        *str.add(lenp as usize) = b'|';
        memcpy(str.add(lenp as usize + 1) as _, token2 as _, lenn as usize);
        *str.add(lenn as usize + lenp as usize + 1) = 0;

        (*atom).valuep = str as _;
    }
    (*atom).data = data;
    if min == 0 {
        (*atom).min = 1;
    } else {
        (*atom).min = min;
    }
    (*atom).max = max;

    /*
     * associate a counter to the transition.
     */
    let counter: i32 = xml_reg_get_counter(am);
    if counter < 0 {
        // goto error;
        xml_reg_free_atom(atom);
        return null_mut();
    }
    (*(*am).counters.add(counter as usize)).min = min;
    (*(*am).counters.add(counter as usize)).max = max;

    /* xmlFAGenerateTransitions(am, from, to, atom); */
    if to.is_null() {
        to = xml_reg_state_push(am);
        if to.is_null() {
            // goto error;
            xml_reg_free_atom(atom);
            return null_mut();
        }
    }
    xml_reg_state_add_trans(am, from, atom, to, counter, -1);
    if xml_reg_atom_push(am, atom) < 0 {
        // goto error;
        xml_reg_free_atom(atom);
        return null_mut();
    }
    (*am).state = to;

    if to.is_null() {
        to = (*am).state;
    }
    if to.is_null() {
        return null_mut();
    }
    if min == 0 {
        xml_fa_generate_epsilon_transition(am, from, to);
    }
    to

    // error:
    //     xmlRegFreeAtom(atom);
    //     return null_mut();
}

/// If @to is NULL, this creates first a new target state in the automata
/// and then adds a transition from the @from state to the target state
/// activated by a succession of input of value @token and whose number
/// is between @min and @max, moreover that transition can only be crossed once.
///
/// Returns the target state or NULL in case of error
#[doc(alias = "xmlAutomataNewOnceTrans")]
pub unsafe extern "C" fn xml_automata_new_once_trans(
    am: XmlAutomataPtr,
    from: XmlAutomataStatePtr,
    mut to: XmlAutomataStatePtr,
    token: *const XmlChar,
    min: i32,
    max: i32,
    data: *mut c_void,
) -> XmlAutomataStatePtr {
    if am.is_null() || from.is_null() || token.is_null() {
        return null_mut();
    }
    if min < 1 {
        return null_mut();
    }
    if max < min {
        return null_mut();
    }
    let atom: XmlRegAtomPtr = xml_reg_new_atom(am, Some(XmlRegAtomType::XmlRegexpString));
    if atom.is_null() {
        return null_mut();
    }
    (*atom).valuep = xml_strdup(token as _) as _;
    (*atom).data = data;
    (*atom).quant = XmlRegQuantType::XmlRegexpQuantOnceonly;
    (*atom).min = min;
    (*atom).max = max;
    /*
     * associate a counter to the transition.
     */
    let counter: i32 = xml_reg_get_counter(am);
    if counter < 0 {
        // goto error;
        xml_reg_free_atom(atom);
        return null_mut();
    }
    (*(*am).counters.add(counter as usize)).min = 1;
    (*(*am).counters.add(counter as usize)).max = 1;

    /* xmlFAGenerateTransitions(am, from, to, atom); */
    if to.is_null() {
        to = xml_reg_state_push(am);
        if to.is_null() {
            // goto error;
            xml_reg_free_atom(atom);
            return null_mut();
        }
    }
    xml_reg_state_add_trans(am, from, atom, to, counter, -1);
    if xml_reg_atom_push(am, atom) < 0 {
        // goto error;
        xml_reg_free_atom(atom);
        return null_mut();
    }
    (*am).state = to;
    to

    // error:
    //     xmlRegFreeAtom(atom);
    //     return null_mut();
}

/// If @to is NULL, this creates first a new target state in the automata
/// and then adds a transition from the @from state to the target state
/// activated by a succession of input of value @token and @token2 and whose
/// number is between @min and @max, moreover that transition can only be crossed once.
///
/// Returns the target state or NULL in case of error
#[doc(alias = "xmlAutomataNewOnceTrans2")]
pub unsafe extern "C" fn xml_automata_new_once_trans2(
    am: XmlAutomataPtr,
    from: XmlAutomataStatePtr,
    mut to: XmlAutomataStatePtr,
    token: *const XmlChar,
    token2: *const XmlChar,
    min: i32,
    max: i32,
    data: *mut c_void,
) -> XmlAutomataStatePtr {
    if am.is_null() || from.is_null() || token.is_null() {
        return null_mut();
    }
    if min < 1 {
        return null_mut();
    }
    if max < min {
        return null_mut();
    }
    let atom: XmlRegAtomPtr = xml_reg_new_atom(am, Some(XmlRegAtomType::XmlRegexpString));
    if atom.is_null() {
        return null_mut();
    }
    if token2.is_null() || *token2 == 0 {
        (*atom).valuep = xml_strdup(token as _) as _;
        if (*atom).valuep.is_null() {
            // goto error;
            xml_reg_free_atom(atom);
            return null_mut();
        }
    } else {
        let lenn: i32 = strlen(token2 as _) as _;
        let lenp: i32 = strlen(token as _) as _;

        let str: *mut XmlChar = xml_malloc_atomic(lenn as usize + lenp as usize + 2) as _;
        if str.is_null() {
            // goto error;
            xml_reg_free_atom(atom);
            return null_mut();
        }
        memcpy(str.add(0) as _, token as _, lenp as _);
        *str.add(lenp as usize) = b'|';
        memcpy(str.add(lenp as usize + 1) as _, token2 as _, lenn as _);
        *str.add(lenn as usize + lenp as usize + 1) = 0;

        (*atom).valuep = str as _;
    }
    (*atom).data = data;
    (*atom).quant = XmlRegQuantType::XmlRegexpQuantOnceonly;
    (*atom).min = min;
    (*atom).max = max;
    /*
     * associate a counter to the transition.
     */
    let counter: i32 = xml_reg_get_counter(am);
    if counter < 0 {
        // goto error;
        xml_reg_free_atom(atom);
        return null_mut();
    }
    (*(*am).counters.add(counter as usize)).min = 1;
    (*(*am).counters.add(counter as usize)).max = 1;

    /* xmlFAGenerateTransitions(am, from, to, atom); */
    if to.is_null() {
        to = xml_reg_state_push(am);
        if to.is_null() {
            // goto error;
            xml_reg_free_atom(atom);
            return null_mut();
        }
    }
    xml_reg_state_add_trans(am, from, atom, to, counter, -1);
    if xml_reg_atom_push(am, atom) < 0 {
        // goto error;
        xml_reg_free_atom(atom);
        return null_mut();
    }
    (*am).state = to;
    to

    // error:
    //     xmlRegFreeAtom(atom);
    //     return null_mut();
}

#[doc(alias = "xmlFAGenerateAllTransition")]
unsafe extern "C" fn xml_fa_generate_all_transition(
    ctxt: XmlRegParserCtxtPtr,
    from: XmlRegStatePtr,
    mut to: XmlRegStatePtr,
    lax: i32,
) -> i32 {
    if to.is_null() {
        to = xml_reg_state_push(ctxt);
        if to.is_null() {
            return -1;
        }
        (*ctxt).state = to;
    }
    if lax != 0 {
        xml_reg_state_add_trans(ctxt, from, null_mut(), to, -1, REGEXP_ALL_LAX_COUNTER as _);
    } else {
        xml_reg_state_add_trans(ctxt, from, null_mut(), to, -1, REGEXP_ALL_COUNTER as _);
    }
    0
}

/// If @to is NULL, this creates first a new target state in the automata
/// and then adds a an ALL transition from the @from state to the
/// target state. That transition is an epsilon transition allowed only when
/// all transitions from the @from node have been activated.
///
/// Returns the target state or NULL in case of error
#[doc(alias = "xmlAutomataNewAllTrans")]
pub unsafe extern "C" fn xml_automata_new_all_trans(
    am: XmlAutomataPtr,
    from: XmlAutomataStatePtr,
    to: XmlAutomataStatePtr,
    lax: i32,
) -> XmlAutomataStatePtr {
    if am.is_null() || from.is_null() {
        return null_mut();
    }
    xml_fa_generate_all_transition(am, from, to, lax);
    if to.is_null() {
        return (*am).state;
    }
    to
}

/// If @to is NULL, this creates first a new target state in the automata
/// and then adds an epsilon transition from the @from state to the target state
///
/// Returns the target state or NULL in case of error
#[doc(alias = "xmlAutomataNewEpsilon")]
pub unsafe extern "C" fn xml_automata_new_epsilon(
    am: XmlAutomataPtr,
    from: XmlAutomataStatePtr,
    to: XmlAutomataStatePtr,
) -> XmlAutomataStatePtr {
    if am.is_null() || from.is_null() {
        return null_mut();
    }
    xml_fa_generate_epsilon_transition(am, from, to);
    if to.is_null() {
        return (*am).state;
    }
    to
}

/// If @to is NULL, this creates first a new target state in the automata
/// and then adds an epsilon transition from the @from state to the target state
/// which will increment the counter provided
///
/// Returns the target state or NULL in case of error
#[doc(alias = "xmlAutomataNewCountedTrans")]
pub unsafe extern "C" fn xml_automata_new_counted_trans(
    am: XmlAutomataPtr,
    from: XmlAutomataStatePtr,
    to: XmlAutomataStatePtr,
    counter: i32,
) -> XmlAutomataStatePtr {
    if am.is_null() || from.is_null() || counter < 0 {
        return null_mut();
    }
    xml_fa_generate_counted_epsilon_transition(am, from, to, counter);
    if to.is_null() {
        return (*am).state;
    }
    to
}

/// If @to is NULL, this creates first a new target state in the automata
/// and then adds an epsilon transition from the @from state to the target state
/// which will be allowed only if the counter is within the right range.
///
/// Returns the target state or NULL in case of error
#[doc(alias = "xmlAutomataNewCounterTrans")]
pub unsafe extern "C" fn xml_automata_new_counter_trans(
    am: XmlAutomataPtr,
    from: XmlAutomataStatePtr,
    to: XmlAutomataStatePtr,
    counter: i32,
) -> XmlAutomataStatePtr {
    if am.is_null() || from.is_null() || counter < 0 {
        return null_mut();
    }
    xml_fa_generate_counted_transition(am, from, to, counter);
    if to.is_null() {
        return (*am).state;
    }
    to
}

/// Create a new counter
///
/// Returns the counter number or -1 in case of error
#[doc(alias = "xmlAutomataNewCounter")]
pub unsafe extern "C" fn xml_automata_new_counter(am: XmlAutomataPtr, min: i32, max: i32) -> i32 {
    if am.is_null() {
        return -1;
    }

    let ret: i32 = xml_reg_get_counter(am);
    if ret < 0 {
        return -1;
    }
    (*(*am).counters.add(ret as usize)).min = min;
    (*(*am).counters.add(ret as usize)).max = max;
    ret
}

/// Compile the automata into a Reg Exp ready for being executed.
/// The automata should be free after this point.
///
/// Returns the compiled regexp or NULL in case of error
#[doc(alias = "xmlAutomataCompile")]
pub unsafe extern "C" fn xml_automata_compile(am: XmlAutomataPtr) -> *mut XmlRegexp {
    if am.is_null() || (*am).error != 0 {
        return null_mut();
    }
    xml_fa_eliminate_epsilon_transitions(am);
    /* xmlFAComputesDeterminism(am); */
    xml_reg_epx_from_parse(am)
}

/// Checks if an automata is determinist.
///
/// Returns 1 if true, 0 if not, and -1 in case of error
#[doc(alias = "xmlAutomataIsDeterminist")]
pub unsafe extern "C" fn xml_automata_is_determinist(am: XmlAutomataPtr) -> i32 {
    if am.is_null() {
        return -1;
    }

    let ret: i32 = xml_fa_computes_determinism(am);
    ret
}

/// Set some flags on the automata
#[doc(alias = "xmlAutomataSetFlags")]
pub(crate) unsafe extern "C" fn xml_automata_set_flags(am: XmlAutomataPtr, flags: i32) {
    if am.is_null() {
        return;
    }
    (*am).flags |= flags;
}

#[cfg(test)]
mod tests {
    use crate::{globals::reset_last_error, libxml::xmlmemory::xml_mem_blocks, test_util::*};

    use super::*;

    #[test]
    fn test_xml_automata_compile() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_get_init_state() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_is_determinist() {
        #[cfg(all(feature = "libxml_regexp", feature = "libxml_automata"))]
        unsafe {
            let mut leaks = 0;

            for n_am in 0..GEN_NB_XML_AUTOMATA_PTR {
                let mem_base = xml_mem_blocks();
                let am = gen_xml_automata_ptr(n_am, 0);

                let ret_val = xml_automata_is_determinist(am);
                desret_int(ret_val);
                des_xml_automata_ptr(n_am, am, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlAutomataIsDeterminist",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlAutomataIsDeterminist()"
                    );
                    eprintln!(" {}", n_am);
                }
            }
        }
    }

    #[test]
    fn test_xml_automata_new_all_trans() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_new_count_trans() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_new_count_trans2() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_new_counted_trans() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_new_counter() {
        #[cfg(all(feature = "libxml_regexp", feature = "libxml_automata"))]
        unsafe {
            let mut leaks = 0;

            for n_am in 0..GEN_NB_XML_AUTOMATA_PTR {
                for n_min in 0..GEN_NB_INT {
                    for n_max in 0..GEN_NB_INT {
                        let mem_base = xml_mem_blocks();
                        let am = gen_xml_automata_ptr(n_am, 0);
                        let min = gen_int(n_min, 1);
                        let max = gen_int(n_max, 2);

                        let ret_val = xml_automata_new_counter(am, min, max);
                        desret_int(ret_val);
                        des_xml_automata_ptr(n_am, am, 0);
                        des_int(n_min, min, 1);
                        des_int(n_max, max, 2);
                        reset_last_error();
                        if mem_base != xml_mem_blocks() {
                            leaks += 1;
                            eprint!(
                                "Leak of {} blocks found in xmlAutomataNewCounter",
                                xml_mem_blocks() - mem_base
                            );
                            assert!(
                                leaks == 0,
                                "{leaks} Leaks are found in xmlAutomataNewCounter()"
                            );
                            eprint!(" {}", n_am);
                            eprint!(" {}", n_min);
                            eprintln!(" {}", n_max);
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_automata_new_counter_trans() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_new_epsilon() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_new_neg_trans() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_new_once_trans() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_new_once_trans2() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_new_state() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_new_transition() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_new_transition2() {

        /* missing type support */
    }

    #[test]
    fn test_xml_automata_set_final_state() {
        #[cfg(all(feature = "libxml_regexp", feature = "libxml_automata"))]
        unsafe {
            let mut leaks = 0;

            for n_am in 0..GEN_NB_XML_AUTOMATA_PTR {
                for n_state in 0..GEN_NB_XML_AUTOMATA_STATE_PTR {
                    let mem_base = xml_mem_blocks();
                    let am = gen_xml_automata_ptr(n_am, 0);
                    let state = gen_xml_automata_state_ptr(n_state, 1);

                    let ret_val = xml_automata_set_final_state(am, state);
                    desret_int(ret_val);
                    des_xml_automata_ptr(n_am, am, 0);
                    des_xml_automata_state_ptr(n_state, state, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlAutomataSetFinalState",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(
                            leaks == 0,
                            "{leaks} Leaks are found in xmlAutomataSetFinalState()"
                        );
                        eprint!(" {}", n_am);
                        eprintln!(" {}", n_state);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_new_automata() {

        /* missing type support */
    }
}

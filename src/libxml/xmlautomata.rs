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

use crate::libxml::{
    xmlregexp::{
        XmlRegAtomPtr, XmlRegAtomType, XmlRegCounter, XmlRegMarkedType, XmlRegQuantType,
        XmlRegStatePtr, XmlRegStateType, XmlRegTrans, XmlRegexp, xml_reg_free_atom,
        xml_reg_free_parser_ctxt, xml_reg_new_parser_ctxt,
    },
    xmlstring::XmlChar,
};

/// A libxml automata description, It can be compiled into a regexp
#[doc(alias = "xmlAutomataPtr")]
pub type XmlAutomataPtr = *mut XmlAutomata;
#[doc(alias = "xmlAutomata")]
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
    pub(crate) atoms: Vec<XmlRegAtomPtr>,
    pub(crate) states: Vec<XmlRegStatePtr>,
    pub(crate) counters: Vec<XmlRegCounter>,
    pub(crate) determinist: i32,
    pub(crate) negs: i32,
    pub(crate) flags: i32,
    pub(crate) depth: i32,
}

impl XmlAutomata {
    /// Compile the automata into a Reg Exp ready for being executed.
    /// The automata should be free after this point.
    ///
    /// Returns the compiled regexp or NULL in case of error
    #[doc(alias = "xmlAutomataCompile")]
    pub unsafe fn compile(&mut self) -> *mut XmlRegexp {
        unsafe {
            if self.error != 0 {
                return null_mut();
            }
            self.fa_eliminate_epsilon_transitions();
            /* xmlFAComputesDeterminism(self); */
            self.parse()
        }
    }

    /// Checks if an automata is determinist.
    ///
    /// Returns 1 if true, 0 if not, and -1 in case of error
    #[doc(alias = "xmlAutomataIsDeterminist")]
    pub unsafe fn is_determinist(&mut self) -> i32 {
        unsafe {
            let ret: i32 = self.fa_computes_determinism();
            ret
        }
    }

    /// Initial state lookup
    ///
    /// Returns the initial state of the automata
    #[doc(alias = "xmlAutomataGetInitState")]
    pub fn get_init_state(&self) -> XmlAutomataStatePtr {
        self.start
    }

    /// Set some flags on the automata
    #[doc(alias = "xmlAutomataSetFlags")]
    pub(crate) fn set_flags(&mut self, flags: i32) {
        self.flags |= flags;
    }

    /// Create a new disconnected state in the automata
    ///
    /// Returns the new state or NULL in case of error
    #[doc(alias = "xmlAutomataNewState")]
    pub unsafe fn new_state(&mut self) -> XmlAutomataStatePtr {
        unsafe { self.reg_state_push() }
    }

    /// Create a new counter
    ///
    /// Returns the counter number or -1 in case of error
    #[doc(alias = "xmlAutomataNewCounter")]
    pub fn new_counter(&mut self, min: i32, max: i32) -> i32 {
        let ret = self.reg_get_counter();
        self.counters[ret].min = min;
        self.counters[ret].max = max;
        ret as i32
    }

    /// If @to is NULL, this creates first a new target state in the automata
    /// and then adds a transition from the @from state to the target state
    /// activated by the value of @token
    ///
    /// Returns the target state or NULL in case of error
    #[doc(alias = "xmlAutomataNewTransition")]
    pub unsafe fn new_transition(
        &mut self,
        from: XmlAutomataStatePtr,
        to: XmlAutomataStatePtr,
        token: &str,
        data: *mut c_void,
    ) -> XmlAutomataStatePtr {
        unsafe {
            if from.is_null() {
                return null_mut();
            }
            let atom: XmlRegAtomPtr = self.reg_new_atom(XmlRegAtomType::XmlRegexpString);
            if atom.is_null() {
                return null_mut();
            }
            (*atom).data = data;
            (*atom).valuep = Some(token.to_owned());

            if self.fa_generate_transitions(from, to, atom) < 0 {
                xml_reg_free_atom(atom);
                return null_mut();
            }
            if to.is_null() {
                return self.state;
            }
            to
        }
    }

    /// If @to is NULL, this creates first a new target state in the automata
    /// and then adds a transition from the @from state to the target state
    /// activated by the value of @token
    ///
    /// Returns the target state or NULL in case of error
    #[doc(alias = "xmlAutomataNewTransition2")]
    pub unsafe fn new_transition2(
        &mut self,
        from: XmlAutomataStatePtr,
        to: XmlAutomataStatePtr,
        token: &str,
        token2: Option<&str>,
        data: *mut c_void,
    ) -> XmlAutomataStatePtr {
        unsafe {
            if from.is_null() {
                return null_mut();
            }
            let atom: XmlRegAtomPtr = self.reg_new_atom(XmlRegAtomType::XmlRegexpString);
            if atom.is_null() {
                return null_mut();
            }
            (*atom).data = data;
            if let Some(token2) = token2.filter(|t| !t.is_empty()) {
                (*atom).valuep = Some(format!("{token}|{token2}"));
            } else {
                (*atom).valuep = Some(token.to_owned());
            }

            if self.fa_generate_transitions(from, to, atom) < 0 {
                xml_reg_free_atom(atom);
                return null_mut();
            }
            if to.is_null() {
                return self.state;
            }
            to
        }
    }

    /// If @to is NULL, this creates first a new target state in the automata
    /// and then adds a transition from the @from state to the target state
    /// activated by any value except (@token,@token2)
    /// Note that if @token2 is not NULL, then (X, NULL) won't match to follow
    /// the semantic of XSD ##other
    ///
    /// Returns the target state or NULL in case of error
    #[doc(alias = "xmlAutomataNewNegTrans")]
    pub unsafe fn new_neg_trans(
        &mut self,
        from: XmlAutomataStatePtr,
        to: XmlAutomataStatePtr,
        token: &str,
        token2: Option<&str>,
        data: *mut c_void,
    ) -> XmlAutomataStatePtr {
        unsafe {
            if from.is_null() {
                return null_mut();
            }
            let atom: XmlRegAtomPtr = self.reg_new_atom(XmlRegAtomType::XmlRegexpString);
            if atom.is_null() {
                return null_mut();
            }
            (*atom).data = data;
            (*atom).neg = 1;
            if let Some(token2) = token2.filter(|t| !t.is_empty()) {
                (*atom).valuep = Some(format!("{token}|{token2}"));
            } else {
                (*atom).valuep = Some(token.to_owned());
            }
            let err_msg = format!("not {}", (*atom).valuep.as_deref().unwrap());
            (*atom).valuep2 = Some(err_msg);

            if self.fa_generate_transitions(from, to, atom) < 0 {
                xml_reg_free_atom(atom);
                return null_mut();
            }
            self.negs += 1;
            if to.is_null() {
                return self.state;
            }
            to
        }
    }

    /// If @to is NULL, this creates first a new target state in the automata
    /// and then adds a transition from the @from state to the target state
    /// activated by a succession of input of value @token and whose number
    /// is between @min and @max
    ///
    /// Returns the target state or NULL in case of error
    #[doc(alias = "xmlAutomataNewCountTrans")]
    pub unsafe fn new_count_trans(
        &mut self,
        from: XmlAutomataStatePtr,
        mut to: XmlAutomataStatePtr,
        token: &str,
        min: i32,
        max: i32,
        data: *mut c_void,
    ) -> XmlAutomataStatePtr {
        unsafe {
            if from.is_null() {
                return null_mut();
            }
            if min < 0 {
                return null_mut();
            }
            if max < min || max < 1 {
                return null_mut();
            }
            let atom: XmlRegAtomPtr = self.reg_new_atom(XmlRegAtomType::XmlRegexpString);
            if atom.is_null() {
                return null_mut();
            }
            (*atom).valuep = Some(token.to_owned());
            (*atom).data = data;
            if min == 0 {
                (*atom).min = 1;
            } else {
                (*atom).min = min;
            }
            (*atom).max = max;

            // associate a counter to the transition.
            let counter = self.reg_get_counter();
            self.counters[counter].min = min;
            self.counters[counter].max = max;

            // xmlFAGenerateTransitions(am, from, to, atom);
            if to.is_null() {
                to = self.reg_state_push();
                if to.is_null() {
                    // goto error;
                    xml_reg_free_atom(atom);
                    return null_mut();
                }
            }
            self.reg_state_add_trans(from, atom, to, counter as i32, -1);
            if self.reg_atom_push(atom) < 0 {
                // goto error;
                xml_reg_free_atom(atom);
                return null_mut();
            }
            self.state = to;

            if to.is_null() {
                to = self.state;
            }
            if to.is_null() {
                return null_mut();
            }
            if min == 0 {
                self.fa_generate_epsilon_transition(from, to);
            }
            to

            // error:
            //     xmlRegFreeAtom(atom);
            //     return null_mut();
        }
    }

    /// If @to is NULL, this creates first a new target state in the automata
    /// and then adds a transition from the @from state to the target state
    /// activated by a succession of input of value @token and @token2 and
    /// whose number is between @min and @max
    ///
    /// Returns the target state or NULL in case of error
    #[doc(alias = "xmlAutomataNewCountTrans2")]
    #[allow(clippy::too_many_arguments)]
    pub unsafe fn new_count_trans2(
        &mut self,
        from: XmlAutomataStatePtr,
        mut to: XmlAutomataStatePtr,
        token: &str,
        token2: Option<&str>,
        min: i32,
        max: i32,
        data: *mut c_void,
    ) -> XmlAutomataStatePtr {
        unsafe {
            if from.is_null() {
                return null_mut();
            }
            if min < 0 {
                return null_mut();
            }
            if max < min || max < 1 {
                return null_mut();
            }
            let atom: XmlRegAtomPtr = self.reg_new_atom(XmlRegAtomType::XmlRegexpString);
            if atom.is_null() {
                return null_mut();
            }
            if let Some(token2) = token2.filter(|t| !t.is_empty()) {
                (*atom).valuep = Some(format!("{token}|{token2}"));
            } else {
                (*atom).valuep = Some(token.to_owned());
            }
            (*atom).data = data;
            if min == 0 {
                (*atom).min = 1;
            } else {
                (*atom).min = min;
            }
            (*atom).max = max;

            // associate a counter to the transition.
            let counter = self.reg_get_counter();
            self.counters[counter].min = min;
            self.counters[counter].max = max;

            // xmlFAGenerateTransitions(self, from, to, atom);
            if to.is_null() {
                to = self.reg_state_push();
                if to.is_null() {
                    // goto error;
                    xml_reg_free_atom(atom);
                    return null_mut();
                }
            }
            self.reg_state_add_trans(from, atom, to, counter as i32, -1);
            if self.reg_atom_push(atom) < 0 {
                // goto error;
                xml_reg_free_atom(atom);
                return null_mut();
            }
            self.state = to;

            if to.is_null() {
                to = self.state;
            }
            if to.is_null() {
                return null_mut();
            }
            if min == 0 {
                self.fa_generate_epsilon_transition(from, to);
            }
            to

            // error:
            //     xmlRegFreeAtom(atom);
            //     return null_mut();
        }
    }

    /// If @to is NULL, this creates first a new target state in the automata
    /// and then adds an epsilon transition from the @from state to the target state
    /// which will increment the counter provided
    ///
    /// Returns the target state or NULL in case of error
    #[doc(alias = "xmlAutomataNewCountedTrans")]
    pub unsafe fn new_counted_trans(
        &mut self,
        from: XmlAutomataStatePtr,
        to: XmlAutomataStatePtr,
        counter: i32,
    ) -> XmlAutomataStatePtr {
        unsafe {
            if from.is_null() || counter < 0 {
                return null_mut();
            }
            self.fa_generate_counted_epsilon_transition(from, to, counter);
            if to.is_null() {
                return self.state;
            }
            to
        }
    }

    /// If @to is NULL, this creates first a new target state in the automata
    /// and then adds an epsilon transition from the @from state to the target state
    /// which will be allowed only if the counter is within the right range.
    ///
    /// Returns the target state or NULL in case of error
    #[doc(alias = "xmlAutomataNewCounterTrans")]
    pub unsafe fn new_counter_trans(
        &mut self,
        from: XmlAutomataStatePtr,
        to: XmlAutomataStatePtr,
        counter: i32,
    ) -> XmlAutomataStatePtr {
        unsafe {
            if from.is_null() || counter < 0 {
                return null_mut();
            }
            self.fa_generate_counted_transition(from, to, counter);
            if to.is_null() {
                return self.state;
            }
            to
        }
    }

    /// If @to is NULL, this creates first a new target state in the automata
    /// and then adds a transition from the @from state to the target state
    /// activated by a succession of input of value @token and whose number
    /// is between @min and @max, moreover that transition can only be crossed once.
    ///
    /// Returns the target state or NULL in case of error
    #[doc(alias = "xmlAutomataNewOnceTrans")]
    pub unsafe fn new_once_trans(
        &mut self,
        from: XmlAutomataStatePtr,
        mut to: XmlAutomataStatePtr,
        token: &str,
        min: i32,
        max: i32,
        data: *mut c_void,
    ) -> XmlAutomataStatePtr {
        unsafe {
            if from.is_null() {
                return null_mut();
            }
            if min < 1 {
                return null_mut();
            }
            if max < min {
                return null_mut();
            }
            let atom: XmlRegAtomPtr = self.reg_new_atom(XmlRegAtomType::XmlRegexpString);
            if atom.is_null() {
                return null_mut();
            }
            (*atom).valuep = Some(token.to_owned());
            (*atom).data = data;
            (*atom).quant = XmlRegQuantType::XmlRegexpQuantOnceonly;
            (*atom).min = min;
            (*atom).max = max;
            // associate a counter to the transition.
            let counter = self.reg_get_counter();
            self.counters[counter].min = 1;
            self.counters[counter].max = 1;

            // xmlFAGenerateTransitions(self, from, to, atom);
            if to.is_null() {
                to = self.reg_state_push();
                if to.is_null() {
                    // goto error;
                    xml_reg_free_atom(atom);
                    return null_mut();
                }
            }
            self.reg_state_add_trans(from, atom, to, counter as i32, -1);
            if self.reg_atom_push(atom) < 0 {
                // goto error;
                xml_reg_free_atom(atom);
                return null_mut();
            }
            self.state = to;
            to

            // error:
            //     xmlRegFreeAtom(atom);
            //     return null_mut();
        }
    }

    /// If @to is NULL, this creates first a new target state in the automata
    /// and then adds a transition from the @from state to the target state
    /// activated by a succession of input of value @token and @token2 and whose
    /// number is between @min and @max, moreover that transition can only be crossed once.
    ///
    /// Returns the target state or NULL in case of error
    #[doc(alias = "xmlAutomataNewOnceTrans2")]
    #[allow(clippy::too_many_arguments)]
    pub unsafe fn new_once_trans2(
        &mut self,
        from: XmlAutomataStatePtr,
        mut to: XmlAutomataStatePtr,
        token: &str,
        token2: Option<&str>,
        min: i32,
        max: i32,
        data: *mut c_void,
    ) -> XmlAutomataStatePtr {
        unsafe {
            if from.is_null() {
                return null_mut();
            }
            if min < 1 {
                return null_mut();
            }
            if max < min {
                return null_mut();
            }
            let atom: XmlRegAtomPtr = self.reg_new_atom(XmlRegAtomType::XmlRegexpString);
            if atom.is_null() {
                return null_mut();
            }
            if let Some(token2) = token2.filter(|t| !t.is_empty()) {
                (*atom).valuep = Some(format!("{token}|{token2}"));
            } else {
                (*atom).valuep = Some(token.to_owned());
            }
            (*atom).data = data;
            (*atom).quant = XmlRegQuantType::XmlRegexpQuantOnceonly;
            (*atom).min = min;
            (*atom).max = max;
            // associate a counter to the transition.
            let counter = self.reg_get_counter();
            self.counters[counter].min = 1;
            self.counters[counter].max = 1;

            // xmlFAGenerateTransitions(self, from, to, atom);
            if to.is_null() {
                to = self.reg_state_push();
                if to.is_null() {
                    // goto error;
                    xml_reg_free_atom(atom);
                    return null_mut();
                }
            }
            self.reg_state_add_trans(from, atom, to, counter as i32, -1);
            if self.reg_atom_push(atom) < 0 {
                // goto error;
                xml_reg_free_atom(atom);
                return null_mut();
            }
            self.state = to;
            to

            // error:
            //     xmlRegFreeAtom(atom);
            //     return null_mut();
        }
    }

    /// If @to is NULL, this creates first a new target state in the automata
    /// and then adds a an ALL transition from the @from state to the
    /// target state. That transition is an epsilon transition allowed only when
    /// all transitions from the @from node have been activated.
    ///
    /// Returns the target state or NULL in case of error
    #[doc(alias = "xmlAutomataNewAllTrans")]
    pub unsafe fn new_all_trans(
        &mut self,
        from: XmlAutomataStatePtr,
        to: XmlAutomataStatePtr,
        lax: i32,
    ) -> XmlAutomataStatePtr {
        unsafe {
            if from.is_null() {
                return null_mut();
            }
            self.fa_generate_all_transition(from, to, lax);
            if to.is_null() {
                return self.state;
            }
            to
        }
    }

    /// If @to is NULL, this creates first a new target state in the automata
    /// and then adds an epsilon transition from the @from state to the target state
    ///
    /// Returns the target state or NULL in case of error
    #[doc(alias = "xmlAutomataNewEpsilon")]
    pub unsafe fn new_epsilon(
        &mut self,
        from: XmlAutomataStatePtr,
        to: XmlAutomataStatePtr,
    ) -> XmlAutomataStatePtr {
        unsafe {
            if from.is_null() {
                return null_mut();
            }
            self.fa_generate_epsilon_transition(from, to);
            if to.is_null() {
                return self.state;
            }
            to
        }
    }
}

impl Default for XmlAutomata {
    fn default() -> Self {
        Self {
            string: null_mut(),
            cur: null_mut(),
            error: 0,
            neg: 0,
            start: null_mut(),
            end: null_mut(),
            state: null_mut(),
            atom: null_mut(),
            atoms: vec![],
            states: vec![],
            counters: vec![],
            determinist: 0,
            negs: 0,
            flags: 0,
            depth: 0,
        }
    }
}

/// A state int the automata description,
#[doc(alias = "xmlAutomataStatePtr")]
pub type XmlAutomataStatePtr = *mut XmlAutomataState;
#[doc(alias = "xmlAutomataState")]
#[repr(C)]
#[derive(Default)]
pub struct XmlAutomataState {
    pub(crate) typ: XmlRegStateType,
    pub(crate) mark: XmlRegMarkedType,
    pub(crate) markd: XmlRegMarkedType,
    pub(crate) reached: XmlRegMarkedType,
    pub(crate) no: i32,
    pub(crate) trans: Vec<XmlRegTrans>,
    // knowing states pointing to us can speed things up
    pub(crate) trans_to: Vec<i32>,
}

impl XmlAutomataState {
    /// Makes that state a final state
    ///
    /// Returns 0 or -1 in case of error
    #[doc(alias = "xmlAutomataSetFinalState")]
    pub fn set_final_state(&mut self) -> i32 {
        self.typ = XmlRegStateType::XmlRegexpFinalState;
        0
    }
}

/// Create a new automata
///
/// Returns the new object or NULL in case of failure
#[doc(alias = "xmlNewAutomata")]
pub unsafe fn xml_new_automata() -> XmlAutomataPtr {
    unsafe {
        let ctxt: XmlAutomataPtr = xml_reg_new_parser_ctxt(null());
        if ctxt.is_null() {
            return null_mut();
        }

        // initialize the parser
        (*ctxt).state = (*ctxt).reg_state_push();
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
}

/// Free an automata
#[doc(alias = "xmlFreeAutomata")]
pub unsafe fn xml_free_automata(am: XmlAutomataPtr) {
    unsafe {
        if am.is_null() {
            return;
        }
        xml_reg_free_parser_ctxt(am);
    }
}

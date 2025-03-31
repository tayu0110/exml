//! Provide methods and data structures for handling regular expressions.
//!
//! This module is based on `libxml/regexp.h`, `regexp.c`, and so on in `libxml2-v2.11.8`.  
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
    ffi::{CStr, c_char},
    fmt::Debug,
    io::Write,
    mem::{size_of, take},
    os::raw::c_void,
    ptr::{addr_of_mut, null, null_mut},
    rc::Rc,
};

use libc::{memset, snprintf};

use crate::{
    error::{__xml_raise_error, XmlErrorDomain, XmlErrorLevel, XmlParserErrors},
    libxml::{
        dict::{XmlDictPtr, xml_dict_lookup},
        globals::{xml_free, xml_malloc, xml_realloc},
        xmlautomata::{XmlAutomata, XmlAutomataState},
        xmlstring::XmlChar,
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

macro_rules! CUR {
    ( $ctxt:expr ) => {
        *(*$ctxt).cur
    };
}

macro_rules! NEXT {
    ( $ctxt:expr ) => {
        (*$ctxt).cur = (*$ctxt).cur.add(1);
    };
}

macro_rules! ERROR {
    ( $ctxt:expr, $str:expr ) => {
        (*$ctxt).error = XmlParserErrors::XmlRegexpCompileError as _;
        xml_regexp_err_compile($ctxt, $str);
    };
}

const MAX_PUSH: usize = 10000000;

// Note: the order of the enums below is significant, do not shuffle
#[doc(alias = "xmlRegAtomType")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub enum XmlRegAtomType {
    #[default]
    XmlRegexpEpsilon = 1,
    XmlRegexpCharval,
    XmlRegexpRanges,
    XmlRegexpSubReg, /* used for () sub regexps */
    XmlRegexpString,
    XmlRegexpAnyChar,     /* . */
    XmlRegexpAnySpace,    /* \s */
    XmlRegexpNotSpace,    /* \S */
    XmlRegexpInitName,    /* \l */
    XmlRegexpNotInitName, /* \L */
    XmlRegexpNameChar,    /* \c */
    XmlRegexpNotNameChar, /* \C */
    XmlRegexpDecimal,     /* \d */
    XmlRegexpNotDecimal,  /* \D */
    XmlRegexpRealChar,    /* \w */
    XmlRegexpNotRealChar, /* \W */
    XmlRegexpLetter = 100,
    XmlRegexpLetterUppercase,
    XmlRegexpLetterLowercase,
    XmlRegexpLetterTitlecase,
    XmlRegexpLetterModifier,
    XmlRegexpLetterOthers,
    XmlRegexpMark,
    XmlRegexpMarkNonSpacing,
    XmlRegexpMarkSpaceCombining,
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
    XmlRegexpPunctInitQuote,
    XmlRegexpPunctFinQuote,
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

impl std::fmt::Display for XmlRegAtomType {
    #[doc(alias = "xmlRegPrintAtomType")]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            XmlRegAtomType::XmlRegexpEpsilon => write!(f, "epsilon"),
            XmlRegAtomType::XmlRegexpCharval => write!(f, "charval"),
            XmlRegAtomType::XmlRegexpRanges => write!(f, "ranges"),
            XmlRegAtomType::XmlRegexpSubReg => write!(f, "subexpr"),
            XmlRegAtomType::XmlRegexpString => write!(f, "string"),
            XmlRegAtomType::XmlRegexpAnyChar => write!(f, "anychar"),
            XmlRegAtomType::XmlRegexpAnySpace => write!(f, "anyspace"),
            XmlRegAtomType::XmlRegexpNotSpace => write!(f, "notspace"),
            XmlRegAtomType::XmlRegexpInitName => write!(f, "initname"),
            XmlRegAtomType::XmlRegexpNotInitName => write!(f, "notinitname"),
            XmlRegAtomType::XmlRegexpNameChar => write!(f, "namechar"),
            XmlRegAtomType::XmlRegexpNotNameChar => write!(f, "notnamechar"),
            XmlRegAtomType::XmlRegexpDecimal => write!(f, "decimal"),
            XmlRegAtomType::XmlRegexpNotDecimal => write!(f, "notdecimal"),
            XmlRegAtomType::XmlRegexpRealChar => write!(f, "realchar"),
            XmlRegAtomType::XmlRegexpNotRealChar => write!(f, "notrealchar"),
            XmlRegAtomType::XmlRegexpLetter => write!(f, "LETTER"),
            XmlRegAtomType::XmlRegexpLetterUppercase => write!(f, "LETTER_UPPERCASE"),
            XmlRegAtomType::XmlRegexpLetterLowercase => write!(f, "LETTER_LOWERCASE"),
            XmlRegAtomType::XmlRegexpLetterTitlecase => write!(f, "LETTER_TITLECASE"),
            XmlRegAtomType::XmlRegexpLetterModifier => write!(f, "LETTER_MODIFIER"),
            XmlRegAtomType::XmlRegexpLetterOthers => write!(f, "LETTER_OTHERS"),
            XmlRegAtomType::XmlRegexpMark => write!(f, "MARK"),
            XmlRegAtomType::XmlRegexpMarkNonSpacing => write!(f, "MARK_NONSPACING"),
            XmlRegAtomType::XmlRegexpMarkSpaceCombining => write!(f, "MARK_SPACECOMBINING"),
            XmlRegAtomType::XmlRegexpMarkEnclosing => write!(f, "MARK_ENCLOSING"),
            XmlRegAtomType::XmlRegexpNumber => write!(f, "NUMBER"),
            XmlRegAtomType::XmlRegexpNumberDecimal => write!(f, "NUMBER_DECIMAL"),
            XmlRegAtomType::XmlRegexpNumberLetter => write!(f, "NUMBER_LETTER"),
            XmlRegAtomType::XmlRegexpNumberOthers => write!(f, "NUMBER_OTHERS"),
            XmlRegAtomType::XmlRegexpPunct => write!(f, "PUNCT"),
            XmlRegAtomType::XmlRegexpPunctConnector => write!(f, "PUNCT_CONNECTOR"),
            XmlRegAtomType::XmlRegexpPunctDash => write!(f, "PUNCT_DASH"),
            XmlRegAtomType::XmlRegexpPunctOpen => write!(f, "PUNCT_OPEN"),
            XmlRegAtomType::XmlRegexpPunctClose => write!(f, "PUNCT_CLOSE"),
            XmlRegAtomType::XmlRegexpPunctInitQuote => write!(f, "PUNCT_INITQUOTE"),
            XmlRegAtomType::XmlRegexpPunctFinQuote => write!(f, "PUNCT_FINQUOTE"),
            XmlRegAtomType::XmlRegexpPunctOthers => write!(f, "PUNCT_OTHERS"),
            XmlRegAtomType::XmlRegexpSepar => write!(f, "SEPAR"),
            XmlRegAtomType::XmlRegexpSeparSpace => write!(f, "SEPAR_SPACE"),
            XmlRegAtomType::XmlRegexpSeparLine => write!(f, "SEPAR_LINE"),
            XmlRegAtomType::XmlRegexpSeparPara => write!(f, "SEPAR_PARA"),
            XmlRegAtomType::XmlRegexpSymbol => write!(f, "SYMBOL"),
            XmlRegAtomType::XmlRegexpSymbolMath => write!(f, "SYMBOL_MATH"),
            XmlRegAtomType::XmlRegexpSymbolCurrency => write!(f, "SYMBOL_CURRENCY"),
            XmlRegAtomType::XmlRegexpSymbolModifier => write!(f, "SYMBOL_MODIFIER"),
            XmlRegAtomType::XmlRegexpSymbolOthers => write!(f, "SYMBOL_OTHERS"),
            XmlRegAtomType::XmlRegexpOther => write!(f, "OTHER"),
            XmlRegAtomType::XmlRegexpOtherControl => write!(f, "OTHER_CONTROL"),
            XmlRegAtomType::XmlRegexpOtherFormat => write!(f, "OTHER_FORMAT"),
            XmlRegAtomType::XmlRegexpOtherPrivate => write!(f, "OTHER_PRIVATE"),
            XmlRegAtomType::XmlRegexpOtherNa => write!(f, "OTHER_NA"),
            XmlRegAtomType::XmlRegexpBlockName => write!(f, "BLOCK"),
        }
    }
}

#[doc(alias = "xmlRegQuantType")]
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

impl std::fmt::Display for XmlRegQuantType {
    #[doc(alias = "xmlRegPrintQuantType")]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            XmlRegQuantType::XmlRegexpQuantEpsilon => write!(f, "epsilon"),
            XmlRegQuantType::XmlRegexpQuantOnce => write!(f, "once"),
            XmlRegQuantType::XmlRegexpQuantOpt => write!(f, "?"),
            XmlRegQuantType::XmlRegexpQuantMult => write!(f, "*"),
            XmlRegQuantType::XmlRegexpQuantPlus => write!(f, "+"),
            XmlRegQuantType::XmlRegexpQuantRange => write!(f, "range"),
            XmlRegQuantType::XmlRegexpQuantOnceonly => write!(f, "onceonly"),
            XmlRegQuantType::XmlRegexpQuantAll => write!(f, "all"),
        }
    }
}

#[doc(alias = "xmlRegStateType")]
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

#[doc(alias = "xmlRegMarkedType")]
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlRegMarkedType {
    #[default]
    XmlRegexpMarkNormal = 0,
    XmlRegexpMarkStart,
    XmlRegexpMarkVisited,
}

#[doc(alias = "xmlRegRange")]
#[repr(C)]
#[derive(Default)]
pub struct XmlRegRange {
    neg: u8, /* 0 normal, 1 not, 2 exclude */
    typ: XmlRegAtomType,
    start: i32,
    end: i32,
    block_name: Option<String>,
}

impl Debug for XmlRegRange {
    #[doc(alias = "xmlRegPrintRange")]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "  range: ")?;
        if self.neg != 0 {
            write!(f, "negative ")?;
        }
        write!(f, "{} ", self.typ)?;
        writeln!(
            f,
            "{} - {}",
            char::from_u32(self.start as u32).unwrap(),
            char::from_u32(self.end as u32).unwrap()
        )?;
        Ok(())
    }
}

impl std::fmt::Display for XmlRegRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

pub type XmlRegState = XmlAutomataState;

#[doc(alias = "xmlRegAtom")]
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

    // state index
    pub(crate) start: usize,
    pub(crate) start0: usize,
    pub(crate) stop: usize,

    pub(crate) ranges: Vec<XmlRegRange>,
    pub(crate) data: *mut c_void,
}

impl XmlRegAtom {
    #[doc(alias = "xmlRegCheckCharacter")]
    fn check_character(&self, codepoint: i32) -> i32 {
        let mut ret: i32;

        if !xml_is_char(codepoint as u32) {
            return -1;
        }

        match self.typ {
            XmlRegAtomType::XmlRegexpSubReg | XmlRegAtomType::XmlRegexpEpsilon => {
                return -1;
            }
            XmlRegAtomType::XmlRegexpCharval => {
                return (codepoint == self.codepoint) as i32;
            }
            XmlRegAtomType::XmlRegexpRanges => {
                let mut accept: i32 = 0;

                for range in &self.ranges {
                    if range.neg == 2 {
                        ret = xml_reg_check_character_range(
                            range.typ,
                            codepoint,
                            0,
                            range.start,
                            range.end,
                            range.block_name.as_deref(),
                        );
                        if ret != 0 {
                            return 0; /* excluded char */
                        }
                    } else if range.neg != 0 {
                        ret = xml_reg_check_character_range(
                            range.typ,
                            codepoint,
                            0,
                            range.start,
                            range.end,
                            range.block_name.as_deref(),
                        );
                        if ret == 0 {
                            accept = 1;
                        } else {
                            return 0;
                        }
                    } else {
                        ret = xml_reg_check_character_range(
                            range.typ,
                            codepoint,
                            0,
                            range.start,
                            range.end,
                            range.block_name.as_deref(),
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
            XmlRegAtomType::XmlRegexpAnyChar
            | XmlRegAtomType::XmlRegexpAnySpace
            | XmlRegAtomType::XmlRegexpNotSpace
            | XmlRegAtomType::XmlRegexpInitName
            | XmlRegAtomType::XmlRegexpNotInitName
            | XmlRegAtomType::XmlRegexpNameChar
            | XmlRegAtomType::XmlRegexpNotNameChar
            | XmlRegAtomType::XmlRegexpDecimal
            | XmlRegAtomType::XmlRegexpNotDecimal
            | XmlRegAtomType::XmlRegexpRealChar
            | XmlRegAtomType::XmlRegexpNotRealChar
            | XmlRegAtomType::XmlRegexpLetter
            | XmlRegAtomType::XmlRegexpLetterUppercase
            | XmlRegAtomType::XmlRegexpLetterLowercase
            | XmlRegAtomType::XmlRegexpLetterTitlecase
            | XmlRegAtomType::XmlRegexpLetterModifier
            | XmlRegAtomType::XmlRegexpLetterOthers
            | XmlRegAtomType::XmlRegexpMark
            | XmlRegAtomType::XmlRegexpMarkNonSpacing
            | XmlRegAtomType::XmlRegexpMarkSpaceCombining
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
            | XmlRegAtomType::XmlRegexpPunctInitQuote
            | XmlRegAtomType::XmlRegexpPunctFinQuote
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
                    self.typ,
                    codepoint,
                    0,
                    0,
                    0,
                    self.valuep.as_deref(),
                );
                if self.neg != 0 {
                    ret = (ret == 0) as i32;
                }
            }
        }
        ret
    }
}

impl Debug for XmlRegAtom {
    #[doc(alias = "xmlRegPrintAtom")]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, " atom: ")?;
        if self.neg != 0 {
            write!(f, "not ")?;
        }
        write!(f, "{} ", self.typ)?;
        write!(f, "{} ", self.quant)?;
        if matches!(self.quant, XmlRegQuantType::XmlRegexpQuantRange) {
            write!(f, "{}-{} ", self.min, self.max)?;
        }
        if matches!(self.typ, XmlRegAtomType::XmlRegexpString) {
            write!(f, "'{}' ", self.valuep.as_deref().unwrap())?;
        }
        if matches!(self.typ, XmlRegAtomType::XmlRegexpCharval) {
            writeln!(f, "char {}", char::from_u32(self.codepoint as u32).unwrap())?;
        } else if matches!(self.typ, XmlRegAtomType::XmlRegexpRanges) {
            writeln!(f, "{} entries", self.ranges.len())?;
            for range in &self.ranges {
                write!(f, "{range:?}")?;
            }
        } else if matches!(self.typ, XmlRegAtomType::XmlRegexpSubReg) {
            writeln!(f, "start {} end {}", self.start, self.stop)?;
        } else {
            writeln!(f)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for XmlRegAtom {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
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
            start: usize::MAX,
            start0: usize::MAX,
            stop: usize::MAX,
            ranges: vec![],
            data: null_mut(),
        }
    }
}

#[doc(alias = "xmlRegCounter")]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct XmlRegCounter {
    pub(crate) min: i32,
    pub(crate) max: i32,
}

#[doc(alias = "xmlRegTrans")]
#[repr(C)]
pub struct XmlRegTrans {
    // If any index is not specified, set `usize::MAX`.
    atom_index: usize,
    to: i32,
    counter: i32,
    count: i32,
    nd: i32,
}

// Parser for the Schemas Datatype Regular Expressions
// http://www.w3.org/TR/2001/REC-xmlschema-2-20010502/#regexs
#[doc(alias = "xmlRegParserCtxt")]
pub type XmlRegParserCtxt = XmlAutomata;
pub type XmlRegParserCtxtPtr = *mut XmlRegParserCtxt;

impl XmlRegParserCtxt {
    /// Allocate a new regexp parser context
    ///
    /// Returns the new context or NULL in case of error
    #[doc(alias = "xmlRegNewParserCtxt")]
    pub(crate) fn new_parser(string: Option<&str>) -> Self {
        let mut ret = XmlRegParserCtxt::default();
        if let Some(string) = string {
            ret.string = string.to_owned().into_boxed_str();
        }
        ret.cur = 0;
        ret.neg = 0;
        ret.negs = 0;
        ret.error = 0;
        ret.determinist = -1;
        ret
    }

    /// Allocate a new regexp and fill it with the result from the parser
    ///
    /// Returns the new regexp or NULL in case of error
    #[doc(alias = "xmlRegEpxFromParse")]
    pub(crate) fn parse(&mut self) -> Option<XmlRegexp> {
        let mut ret = XmlRegexp {
            string: self.string.clone(),
            states: take(&mut self.states),
            atoms: take(&mut self.atoms),
            counters: take(&mut self.counters),
            determinist: self.determinist,
            flags: self.flags,
            ..Default::default()
        };
        if ret.determinist == -1 {
            ret.computes_determinism();
        }

        if ret.determinist != 0
            && ret.counters.is_empty()
            && self.negs == 0
            && !ret.atoms.is_empty()
            && matches!(ret.atoms[0].typ, XmlRegAtomType::XmlRegexpString)
        {
            let mut nbstates: i32 = 0;
            let mut nbatoms: i32 = 0;

            // Switch to a compact representation
            // 1/ counting the effective number of states left
            // 2/ counting the unique number of atoms, and check that
            //    they are all of the string type
            // 3/ build a table state x atom for the transitions

            let mut state_remap = vec![0; ret.states.len()];
            for (i, state) in ret.states.iter().enumerate() {
                if state.is_some() {
                    state_remap[i] = nbstates;
                    nbstates += 1;
                } else {
                    state_remap[i] = -1;
                }
            }

            let mut string_map = Vec::with_capacity(ret.atoms.len());
            let mut string_remap = vec![0; ret.atoms.len()];
            for (i, atom) in ret.atoms.iter().enumerate() {
                if matches!(atom.typ, XmlRegAtomType::XmlRegexpString)
                    && matches!(atom.quant, XmlRegQuantType::XmlRegexpQuantOnce)
                {
                    let mut k = nbatoms;
                    let value = atom.valuep.as_deref().unwrap();
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
                    self.states = take(&mut ret.states);
                    self.counters = take(&mut ret.counters);
                    self.atoms = take(&mut ret.atoms);
                    return None;
                }
            }
            let mut transitions = vec![vec![0; (nbatoms + 1) as usize]; (nbstates + 1) as usize];

            // Allocate the transition table. The first entry for each
            // state corresponds to the state type.
            let mut transdata = vec![];

            for (i, state) in ret.states.iter().enumerate() {
                let mut atomno: i32;
                let mut targetno: i32;
                let mut prev: i32;

                let stateno: i32 = state_remap[i];
                if stateno == -1 {
                    continue;
                }

                transitions[stateno as usize][0] = state.as_ref().unwrap().typ as i32;

                for trans in &state.as_ref().unwrap().trans {
                    if trans.to == -1 || trans.atom_index == usize::MAX {
                        continue;
                    }
                    atomno = string_remap[ret.atoms[trans.atom_index].no as usize];
                    if !ret.atoms[trans.atom_index].data.is_null() && transdata.is_empty() {
                        transdata = vec![vec![null_mut(); nbatoms as usize]; nbstates as usize];
                    }
                    targetno = state_remap[trans.to as usize];
                    // if the same atom can generate transitions to 2 different
                    // states then it means the automata is not deterministic and
                    // the compact form can't be used !
                    prev = transitions[stateno as usize][(atomno + 1) as usize];
                    if prev != 0 {
                        if prev != targetno + 1 {
                            ret.determinist = 0;
                            self.string = "".to_owned().into_boxed_str();
                            self.states.clear();
                            self.atoms.clear();
                            self.counters.clear();
                            return Some(ret);
                        }
                    } else {
                        transitions[stateno as usize][(atomno + 1) as usize] = targetno + 1; /* to avoid 0 */
                        if !transdata.is_empty() {
                            transdata[stateno as usize][atomno as usize] =
                                ret.atoms[trans.atom_index].data;
                        }
                    }
                }
            }
            ret.determinist = 1;
            // Cleanup of the old data
            ret.states.clear();
            ret.compact = transitions;
            ret.transdata = transdata;
            ret.string_map = string_map;
            ret.nbstates = nbstates;
        }
        // not_determ:
        self.string = "".to_owned().into_boxed_str();
        self.states.clear();
        self.atoms.clear();
        self.counters.clear();
        Some(ret)
    }

    #[doc(alias = "xmlRegGetCounter")]
    pub(crate) fn reg_get_counter(&mut self) -> usize {
        self.counters.push(XmlRegCounter { min: -1, max: -1 });
        self.counters.len() - 1
    }

    /// Allocate a new atom
    ///
    /// Returns the new atom or NULL in case of error
    #[doc(alias = "xmlRegNewAtom")]
    pub(crate) fn reg_new_atom(&mut self, typ: XmlRegAtomType) -> usize {
        let ret = XmlRegAtom {
            typ,
            quant: XmlRegQuantType::XmlRegexpQuantOnce,
            min: 0,
            max: 0,
            no: self.atoms.len() as i32,
            ..Default::default()
        };
        self.atoms.push(ret);
        self.atoms.len() - 1
    }

    /// Allocate a new regexp range
    ///
    /// Returns the new atom or NULL in case of error
    #[doc(alias = "xmlRegCopyAtom")]
    fn reg_copy_atom(&mut self, atom_index: usize) -> usize {
        let mut ret = XmlRegAtom {
            typ: self.atoms[atom_index].typ,
            quant: self.atoms[atom_index].quant,
            min: self.atoms[atom_index].min,
            max: self.atoms[atom_index].max,
            no: self.atoms.len() as i32,
            ..Default::default()
        };
        if !self.atoms[atom_index].ranges.is_empty() {
            ret.ranges.reserve(self.atoms[atom_index].ranges.len());
            for i in 0..self.atoms[atom_index].ranges.len() {
                let new = self.reg_copy_range(&self.atoms[atom_index].ranges[i]);
                ret.ranges.push(new);
            }
        }
        self.atoms.push(ret);
        self.atoms.len() - 1
    }

    /// Allocate a new regexp range
    ///
    /// Returns the new range or NULL in case of error
    #[doc(alias = "xmlRegNewRange")]
    fn reg_new_range(&self, neg: u8, typ: XmlRegAtomType, start: i32, end: i32) -> XmlRegRange {
        XmlRegRange {
            neg,
            typ,
            start,
            end,
            ..Default::default()
        }
    }

    #[doc(alias = "xmlRegAtomAddRange")]
    fn reg_atom_add_range(
        &mut self,
        atom_index: usize,
        neg: u8,
        typ: XmlRegAtomType,
        start: i32,
        end: i32,
        block_name: Option<&str>,
    ) -> Option<usize> {
        if atom_index == usize::MAX {
            ERROR!(self, "add range: atom is NULL");
            return None;
        }
        if !matches!(self.atoms[atom_index].typ, XmlRegAtomType::XmlRegexpRanges) {
            ERROR!(self, "add range: atom is not ranges");
            return None;
        }
        let mut range = self.reg_new_range(neg, typ, start, end);
        range.block_name = block_name.map(|b| b.to_owned());
        self.atoms[atom_index].ranges.push(range);
        Some(self.atoms[atom_index].ranges.len() - 1)
    }

    /// Copy a regexp range
    ///
    /// Returns the new copy or NULL in case of error.
    #[doc(alias = "xmlRegCopyRange")]
    fn reg_copy_range(&self, range: &XmlRegRange) -> XmlRegRange {
        let mut ret = self.reg_new_range(range.neg, range.typ, range.start, range.end);
        if let Some(block_name) = range.block_name.as_deref() {
            ret.block_name = Some(block_name.to_owned());
        }
        ret
    }

    #[doc(alias = "xmlRegNewState")]
    fn reg_new_state(&self) -> XmlRegState {
        XmlRegState {
            typ: XmlRegStateType::XmlRegexpTransState,
            mark: XmlRegMarkedType::XmlRegexpMarkNormal,
            invalid: false,
            ..Default::default()
        }
    }

    #[doc(alias = "xmlRegStatePush")]
    pub(crate) fn reg_state_push(&mut self) -> usize {
        let mut state = self.reg_new_state();
        state.no = self.states.len() as i32;
        self.states.push(Some(state));
        self.states.len() - 1
    }

    #[doc(alias = "xmlRegStateAddTrans")]
    pub(crate) fn reg_state_add_trans(
        &mut self,
        state: usize,
        atom_index: usize,
        target: usize,
        counter: i32,
        count: i32,
    ) {
        if self.get_state(state).is_none() {
            ERROR!(self, "add state: state is NULL");
            return;
        }
        if self.get_state(target).is_none() {
            ERROR!(self, "add state: target is NULL");
            return;
        }
        let no = self.get_state(target).unwrap().no;
        // Other routines follow the philosophy 'When in doubt, add a transition'
        // so we check here whether such a transition is already present and, if
        // so, silently ignore this request.

        for trans in self.get_state(state).unwrap().trans.iter().rev() {
            if trans.atom_index == atom_index
                && trans.to == self.get_state(target).unwrap().no
                && trans.counter == counter
                && trans.count == count
            {
                return;
            }
        }

        let trans = XmlRegTrans {
            atom_index,
            to: no,
            counter,
            count,
            nd: 0,
        };
        let no = self.get_state(state).unwrap().no;
        self.get_state_mut(state).unwrap().trans.push(trans);
        self.get_state_mut(target).unwrap().trans_to.push(no);
    }

    fn parse_escaped_codeunit(&mut self) -> i32 {
        let mut val = 0;
        for _ in 0..4 {
            self.cur += 1;
            val *= 16;
            match self.current_byte() {
                Some(cur @ b'0'..=b'9') => {
                    val += cur as i32 - b'0' as i32;
                }
                Some(cur @ b'A'..=b'F') => {
                    val += cur as i32 - b'A' as i32 + 10;
                }
                Some(cur @ b'a'..=b'f') => {
                    val += cur as i32 - b'a' as i32 + 10;
                }
                _ => {
                    ERROR!(self, "Expecting hex digit");
                    return -1;
                }
            }
        }
        val
    }

    fn parse_escaped_codepoint(&mut self) -> i32 {
        let mut val: i32 = self.parse_escaped_codeunit();
        if (0xD800..=0xDBFF).contains(&val) {
            self.cur += 1;
            if self.current_byte() == Some(b'\\') {
                self.cur += 1;
                if self.current_byte() == Some(b'u') {
                    let low: i32 = self.parse_escaped_codeunit();
                    if (0xDC00..=0xDFFF).contains(&low) {
                        return (val - 0xD800) * 0x400 + (low - 0xDC00) + 0x10000;
                    }
                }
            }
            ERROR!(self, "Invalid low surrogate pair code unit");
            val = -1;
        }
        val
    }

    /// `[10]   Char   ::=   [^.\?*+()|#x5B#x5D]`
    #[doc(alias = "xmlFAIsChar")]
    fn fa_is_char(&self) -> Option<char> {
        self.current_char().filter(|&cur| {
            cur != '.'
                && cur != '\\'
                && cur != '?'
                && cur != '*'
                && cur != '+'
                && cur != '('
                && cur != ')'
                && cur != '|'
                && cur != '\x5B'
                && cur != '\x5D'
        })
    }

    /// Returns 0 if success and -1 in case of error.
    #[doc(alias = "xmlFAGenerateTransitions")]
    pub(crate) fn fa_generate_transitions(
        &mut self,
        from: usize,
        mut to: usize,
        atom_index: usize,
    ) -> i32 {
        let mut nullable: i32 = 0;

        if atom_index == usize::MAX {
            ERROR!(self, "generate transition: atom == NULL");
            return -1;
        }
        if matches!(self.atoms[atom_index].typ, XmlRegAtomType::XmlRegexpSubReg) {
            // this is a subexpression handling one should not need to
            // create a new node except for XML_REGEXP_QUANT_RANGE.
            if self.get_state(to).is_some()
                && self.atoms[atom_index].stop != to
                && !matches!(
                    self.atoms[atom_index].quant,
                    XmlRegQuantType::XmlRegexpQuantRange
                )
            {
                // Generate an epsilon transition to link to the target
                self.fa_generate_epsilon_transition(self.atoms[atom_index].stop, to);
            }
            match self.atoms[atom_index].quant {
                XmlRegQuantType::XmlRegexpQuantOpt => {
                    self.atoms[atom_index].quant = XmlRegQuantType::XmlRegexpQuantOnce;
                    // transition done to the state after end of atom.
                    //      1. set transition from atom start to new state
                    //      2. set transition from atom end to this state.
                    if self.get_state(to).is_none() {
                        self.fa_generate_epsilon_transition(
                            self.atoms[atom_index].start,
                            usize::MAX,
                        );
                        self.fa_generate_epsilon_transition(
                            self.atoms[atom_index].stop,
                            self.state,
                        );
                    } else {
                        self.fa_generate_epsilon_transition(self.atoms[atom_index].start, to);
                    }
                }
                XmlRegQuantType::XmlRegexpQuantMult => {
                    self.atoms[atom_index].quant = XmlRegQuantType::XmlRegexpQuantOnce;
                    self.fa_generate_epsilon_transition(
                        self.atoms[atom_index].start,
                        self.atoms[atom_index].stop,
                    );
                    self.fa_generate_epsilon_transition(
                        self.atoms[atom_index].stop,
                        self.atoms[atom_index].start,
                    );
                }
                XmlRegQuantType::XmlRegexpQuantPlus => {
                    self.atoms[atom_index].quant = XmlRegQuantType::XmlRegexpQuantOnce;
                    self.fa_generate_epsilon_transition(
                        self.atoms[atom_index].stop,
                        self.atoms[atom_index].start,
                    );
                }
                XmlRegQuantType::XmlRegexpQuantRange => {
                    // create the final state now if needed
                    let newstate = if self.get_state(to).is_some() {
                        to
                    } else {
                        self.reg_state_push()
                    };

                    // The principle here is to use counted transition
                    // to avoid explosion in the number of states in the
                    // graph. This is clearly more complex but should not
                    // be exploitable at runtime.
                    if self.atoms[atom_index].min == 0
                        && self.get_state(self.atoms[atom_index].start0).is_none()
                    {
                        // duplicate a transition based on atom to count next
                        // occurrences after 1. We cannot loop to (*atom).start
                        // directly because we need an epsilon transition to
                        // newstate.
                        // ???? For some reason it seems we never reach that
                        //     case, I suppose this got optimized out before when
                        // building the automata
                        let copy = self.reg_copy_atom(atom_index);
                        if copy == usize::MAX {
                            return -1;
                        }
                        self.atoms[copy].quant = XmlRegQuantType::XmlRegexpQuantOnce;
                        self.atoms[copy].min = 0;
                        self.atoms[copy].max = 0;

                        if self.fa_generate_transitions(
                            self.atoms[atom_index].start,
                            usize::MAX,
                            copy,
                        ) < 0
                        {
                            return -1;
                        }
                        let inter = self.state;
                        let counter = self.reg_get_counter();
                        self.counters[counter].min = self.atoms[atom_index].min - 1;
                        self.counters[counter].max = self.atoms[atom_index].max - 1;
                        // count the number of times we see it again
                        self.fa_generate_counted_epsilon_transition(
                            inter,
                            self.atoms[atom_index].stop,
                            counter as i32,
                        );
                        // allow a way out based on the count
                        self.fa_generate_counted_transition(inter, newstate, counter as i32);
                        // and also allow a direct exit for 0
                        self.fa_generate_epsilon_transition(self.atoms[atom_index].start, newstate);
                    } else {
                        // either we need the atom at least once or there
                        // is an (*atom).start0 allowing to easily plug the
                        // epsilon transition.
                        let counter = self.reg_get_counter();
                        self.counters[counter].min = self.atoms[atom_index].min - 1;
                        self.counters[counter].max = self.atoms[atom_index].max - 1;
                        // allow a way out based on the count
                        self.fa_generate_counted_transition(
                            self.atoms[atom_index].stop,
                            newstate,
                            counter as i32,
                        );
                        // count the number of times we see it again
                        self.fa_generate_counted_epsilon_transition(
                            self.atoms[atom_index].stop,
                            self.atoms[atom_index].start,
                            counter as i32,
                        );
                        // and if needed allow a direct exit for 0
                        if self.atoms[atom_index].min == 0 {
                            self.fa_generate_epsilon_transition(
                                self.atoms[atom_index].start0,
                                newstate,
                            );
                        }
                    }
                    self.atoms[atom_index].min = 0;
                    self.atoms[atom_index].max = 0;
                    self.atoms[atom_index].quant = XmlRegQuantType::XmlRegexpQuantOnce;
                    self.state = newstate;
                }
                _ => {}
            }
            return 0;
        }
        if self.atoms[atom_index].min == 0
            && self.atoms[atom_index].max == 0
            && matches!(
                self.atoms[atom_index].quant,
                XmlRegQuantType::XmlRegexpQuantRange
            )
        {
            // we can discard the atom and generate an epsilon transition instead
            if self.get_state(to).is_none() {
                to = self.reg_state_push();
            }
            self.fa_generate_epsilon_transition(from, to);
            self.state = to;
            return 0;
        }
        if self.get_state(to).is_none() {
            to = self.reg_state_push();
        }
        let end = to;
        if matches!(
            self.atoms[atom_index].quant,
            XmlRegQuantType::XmlRegexpQuantMult | XmlRegQuantType::XmlRegexpQuantPlus
        ) {
            // Do not pollute the target state by adding transitions from
            // it as it is likely to be the shared target of multiple branches.
            // So isolate with an epsilon transition.

            let tmp = self.reg_state_push();
            self.fa_generate_epsilon_transition(tmp, to);
            to = tmp;
        }
        if matches!(
            self.atoms[atom_index].quant,
            XmlRegQuantType::XmlRegexpQuantRange
        ) && self.atoms[atom_index].min == 0
            && self.atoms[atom_index].max > 0
        {
            nullable = 1;
            self.atoms[atom_index].min = 1;
            if self.atoms[atom_index].max == 1 {
                self.atoms[atom_index].quant = XmlRegQuantType::XmlRegexpQuantOpt;
            }
        }
        self.reg_state_add_trans(from, atom_index, to, -1, -1);
        self.state = end;
        match self.atoms[atom_index].quant {
            XmlRegQuantType::XmlRegexpQuantOpt => {
                self.atoms[atom_index].quant = XmlRegQuantType::XmlRegexpQuantOnce;
                self.fa_generate_epsilon_transition(from, to);
            }
            XmlRegQuantType::XmlRegexpQuantMult => {
                self.atoms[atom_index].quant = XmlRegQuantType::XmlRegexpQuantOnce;
                self.fa_generate_epsilon_transition(from, to);
                self.reg_state_add_trans(to, atom_index, to, -1, -1);
            }
            XmlRegQuantType::XmlRegexpQuantPlus => {
                self.atoms[atom_index].quant = XmlRegQuantType::XmlRegexpQuantOnce;
                self.reg_state_add_trans(to, atom_index, to, -1, -1);
            }
            XmlRegQuantType::XmlRegexpQuantRange => {
                if nullable != 0 {
                    self.fa_generate_epsilon_transition(from, to);
                }
            }
            _ => {}
        }
        0
    }

    #[doc(alias = "xmlFAGenerateCountedTransition")]
    pub(crate) fn fa_generate_counted_transition(
        &mut self,
        from: usize,
        mut to: usize,
        counter: i32,
    ) -> i32 {
        if self.get_state(to).is_none() {
            to = self.reg_state_push();
            self.state = to;
        }
        self.reg_state_add_trans(from, usize::MAX, to, -1, counter);
        0
    }

    #[doc(alias = "xmlFAGenerateEpsilonTransition")]
    pub(crate) fn fa_generate_epsilon_transition(&mut self, from: usize, mut to: usize) -> i32 {
        if self.get_state(to).is_none() {
            to = self.reg_state_push();
            self.state = to;
        }
        self.reg_state_add_trans(from, usize::MAX, to, -1, -1);
        0
    }

    #[doc(alias = "xmlFAGenerateCountedEpsilonTransition")]
    pub(crate) fn fa_generate_counted_epsilon_transition(
        &mut self,
        from: usize,
        mut to: usize,
        counter: i32,
    ) -> i32 {
        if self.get_state(to).is_none() {
            to = self.reg_state_push();
            self.state = to;
        }
        self.reg_state_add_trans(from, usize::MAX, to, counter, -1);
        0
    }

    #[doc(alias = "xmlFAGenerateAllTransition")]
    pub(crate) fn fa_generate_all_transition(
        &mut self,
        from: usize,
        mut to: usize,
        lax: i32,
    ) -> i32 {
        if self.get_state(to).is_none() {
            to = self.reg_state_push();
            self.state = to;
        }
        if lax != 0 {
            self.reg_state_add_trans(from, usize::MAX, to, -1, REGEXP_ALL_LAX_COUNTER as _);
        } else {
            self.reg_state_add_trans(from, usize::MAX, to, -1, REGEXP_ALL_COUNTER as _);
        }
        0
    }

    #[doc(alias = "xmlFAEliminateEpsilonTransitions")]
    pub(crate) fn fa_eliminate_epsilon_transitions(&mut self) {
        // Eliminate simple epsilon transition and the associated unreachable states.
        self.fa_eliminate_simple_epsilon_transitions();
        for state in self.states.iter_mut() {
            state.take_if(|state| matches!(state.typ, XmlRegStateType::XmlRegexpUnreachState));
        }

        let mut has_epsilon = 0;

        // Build the completed transitions bypassing the epsilons
        // Use a marking algorithm to avoid loops
        // Mark sink states too.
        // Process from the latest states backward to the start when
        // there is long cascading epsilon chains this minimize the
        // recursions and transition compares when adding the new ones
        for statenr in (0..self.states.len()).rev() {
            let Some(state) = self.get_state_mut(statenr) else {
                continue;
            };
            if state.trans.is_empty() && !matches!(state.typ, XmlRegStateType::XmlRegexpFinalState)
            {
                state.typ = XmlRegStateType::XmlRegexpSinkState;
            }
            let len = self.get_state(statenr).unwrap().trans.len();
            for transnr in 0..len {
                let trans = &mut self.get_state_mut(statenr).unwrap().trans[transnr];
                if trans.atom_index == usize::MAX && trans.to >= 0 {
                    if trans.to == statenr as i32 {
                        trans.to = -1;
                    } else if trans.count < 0 {
                        let newto = trans.to;

                        has_epsilon = 1;
                        trans.to = -2;
                        let counter = trans.counter;
                        self.get_state_mut(statenr).unwrap().mark =
                            XmlRegMarkedType::XmlRegexpMarkStart;
                        self.fa_reduce_epsilon_transitions(statenr, newto as usize, counter);
                        self.get_state_mut(statenr).unwrap().mark =
                            XmlRegMarkedType::XmlRegexpMarkNormal;
                    }
                }
            }
        }
        // Eliminate the epsilon transitions
        if has_epsilon != 0 {
            for state in &mut self.states {
                let Some(state) = state else {
                    continue;
                };
                for trans in state.trans.iter_mut() {
                    if trans.atom_index == usize::MAX && trans.count < 0 && trans.to >= 0 {
                        trans.to = -1;
                    }
                }
            }
        }

        // Use this pass to detect unreachable states too
        for state in self.states.iter_mut().filter_map(|state| state.as_mut()) {
            state.reached = XmlRegMarkedType::XmlRegexpMarkNormal;
        }
        let mut now = 0;
        if let Some(state) = self.get_state_mut(0) {
            state.reached = XmlRegMarkedType::XmlRegexpMarkStart;
            now = 0;
        }
        while self.get_state(now).is_some() {
            let mut target = usize::MAX;
            self.get_state_mut(now).unwrap().reached = XmlRegMarkedType::XmlRegexpMarkVisited;
            // Mark all states reachable from the current reachable state
            for transnr in 0..self.get_state(now).unwrap().trans.len() {
                let trans = &self.get_state(now).unwrap().trans[transnr];
                if trans.to >= 0 && (trans.atom_index != usize::MAX || trans.count >= 0) {
                    let newto = trans.to;

                    let Some(to) = self.get_state_mut(newto as usize) else {
                        continue;
                    };
                    if matches!(to.reached, XmlRegMarkedType::XmlRegexpMarkNormal) {
                        to.reached = XmlRegMarkedType::XmlRegexpMarkStart;
                        target = newto as usize;
                    }
                }
            }

            // find the next accessible state not explored
            if self.get_state(target).is_none() {
                if let Some(pos) = self.states.iter().skip(1).position(|state| {
                    state.as_ref().is_some_and(|state| {
                        matches!(state.reached, XmlRegMarkedType::XmlRegexpMarkStart)
                    })
                }) {
                    target = pos + 1;
                }
            }
            now = target;
        }
        for state in self.states.iter_mut() {
            state.take_if(|state| matches!(state.reached, XmlRegMarkedType::XmlRegexpMarkNormal));
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
    fn fa_eliminate_simple_epsilon_transitions(&mut self) {
        for statenr in 0..self.states.len() {
            if self.get_state(statenr).is_none() {
                continue;
            }
            if self.get_state(statenr).unwrap().trans.len() != 1 {
                continue;
            }
            if matches!(
                self.get_state(statenr).unwrap().typ,
                XmlRegStateType::XmlRegexpUnreachState | XmlRegStateType::XmlRegexpFinalState
            ) {
                continue;
            }
            // is the only transition out a basic transition
            if self.get_state(statenr).unwrap().trans[0].atom_index == usize::MAX
                && self.get_state(statenr).unwrap().trans[0].to >= 0
                && self.get_state(statenr).unwrap().trans[0].to != statenr as i32
                && self.get_state(statenr).unwrap().trans[0].counter < 0
                && self.get_state(statenr).unwrap().trans[0].count < 0
            {
                let newto = self.get_state(statenr).unwrap().trans[0].to;

                if !matches!(
                    self.get_state(statenr).unwrap().typ,
                    XmlRegStateType::XmlRegexpStartState
                ) {
                    for index in 0..self.get_state(statenr).unwrap().trans_to.len() {
                        let index = self.get_state(statenr).unwrap().trans_to[index];
                        for transnr in 0..self.get_state(index as usize).unwrap().trans.len() {
                            let trans =
                                &mut self.get_state_mut(index as usize).unwrap().trans[transnr];
                            if trans.to == statenr as i32 {
                                trans.to = -1;
                                let atom_index = trans.atom_index;
                                let counter = trans.counter;
                                let count = trans.count;
                                self.reg_state_add_trans(
                                    index as usize,
                                    atom_index,
                                    newto as usize,
                                    counter,
                                    count,
                                );
                            }
                        }
                    }
                    if matches!(
                        self.get_state(statenr).unwrap().typ,
                        XmlRegStateType::XmlRegexpFinalState
                    ) {
                        self.get_state_mut(newto as usize).unwrap().typ =
                            XmlRegStateType::XmlRegexpFinalState;
                    }
                    // eliminate the transition completely
                    self.get_state_mut(statenr).unwrap().trans.clear();
                    self.get_state_mut(statenr).unwrap().typ =
                        XmlRegStateType::XmlRegexpUnreachState;
                }
            }
        }
    }

    #[doc(alias = "xmlFAReduceEpsilonTransitions")]
    fn fa_reduce_epsilon_transitions(&mut self, fromnr: usize, tonr: usize, counter: i32) {
        if self.get_state(fromnr).is_none() {
            return;
        }
        if self.get_state(tonr).is_none_or(|state| {
            matches!(
                state.mark,
                XmlRegMarkedType::XmlRegexpMarkStart | XmlRegMarkedType::XmlRegexpMarkVisited
            )
        }) {
            return;
        }

        self.get_state_mut(tonr).unwrap().mark = XmlRegMarkedType::XmlRegexpMarkVisited;
        if matches!(
            self.get_state_mut(tonr).unwrap().typ,
            XmlRegStateType::XmlRegexpFinalState
        ) {
            self.get_state_mut(fromnr).unwrap().typ = XmlRegStateType::XmlRegexpFinalState;
        }
        let len = self.get_state(tonr).unwrap().trans.len();
        for trannr in 0..len {
            let trans = &self.get_state(tonr).unwrap().trans[trannr];
            if trans.to < 0 {
                continue;
            }
            if trans.atom_index == usize::MAX {
                // Don't remove counted transitions
                // Don't loop either
                if trans.to != fromnr as i32 {
                    if trans.count >= 0 {
                        let newto: i32 = trans.to;

                        self.reg_state_add_trans(
                            fromnr,
                            usize::MAX,
                            newto as usize,
                            -1,
                            trans.count,
                        );
                    } else if trans.counter >= 0 {
                        self.fa_reduce_epsilon_transitions(
                            fromnr,
                            trans.to as usize,
                            trans.counter,
                        );
                    } else {
                        self.fa_reduce_epsilon_transitions(fromnr, trans.to as usize, counter);
                    }
                }
            } else {
                let newto: i32 = trans.to;

                if trans.counter >= 0 {
                    self.reg_state_add_trans(
                        fromnr,
                        trans.atom_index,
                        newto as usize,
                        trans.counter,
                        -1,
                    );
                } else {
                    self.reg_state_add_trans(fromnr, trans.atom_index, newto as usize, counter, -1);
                }
            }
        }
        self.get_state_mut(tonr).unwrap().mark = XmlRegMarkedType::XmlRegexpMarkNormal;
    }

    /// Check whether the associated regexp is determinist,
    /// should be called after xmlFAEliminateEpsilonTransitions()
    #[doc(alias = "xmlFARecurseDeterminism")]
    fn fa_recurse_determinism(&mut self, state: usize, to: usize, atom_index: usize) -> i32 {
        let mut ret: i32 = 1;
        let mut res: i32;
        let mut deep: i32 = 1;

        if self
            .get_state(state)
            .is_none_or(|state| matches!(state.markd, XmlRegMarkedType::XmlRegexpMarkVisited))
        {
            return ret;
        }

        if self.flags & AM_AUTOMATA_RNG as i32 != 0 {
            deep = 0;
        }

        // don't recurse on transitions potentially added in the course of the elimination.
        for t1 in 0..self.get_state(state).unwrap().trans.len() {
            // check transitions conflicting with the one looked at
            let t1 = &mut self.states[state].as_mut().unwrap().trans[t1];
            if t1.atom_index == usize::MAX {
                if t1.to < 0 {
                    continue;
                }
                let t1_to = t1.to as usize;
                self.get_state_mut(state).unwrap().markd = XmlRegMarkedType::XmlRegexpMarkVisited;
                res = self.fa_recurse_determinism(t1_to, to, atom_index);
                if res == 0 {
                    ret = 0;
                }
                continue;
            }
            if t1.to != to as i32 {
                continue;
            }
            if t1.atom_index != usize::MAX
                && atom_index != usize::MAX
                && xml_fa_compare_atoms(&self.atoms[t1.atom_index], &self.atoms[atom_index], deep)
                    != 0
            {
                ret = 0;
                // mark the transition as non-deterministic
                t1.nd = 1;
            }
        }
        ret
    }

    /// Reset flags after checking determinism.
    #[doc(alias = "xmlFAFinishRecurseDeterminism")]
    fn fa_finish_recurse_determinism(&mut self, state: usize) {
        if self.get_state(state).is_none() {
            return;
        }
        if !matches!(
            self.get_state(state).unwrap().markd,
            XmlRegMarkedType::XmlRegexpMarkVisited
        ) {
            return;
        }
        self.get_state_mut(state).unwrap().markd = XmlRegMarkedType::XmlRegexpMarkNormal;

        let len = self.get_state(state).unwrap().trans.len();
        for t1 in 0..len {
            let t1 = &self.get_state(state).unwrap().trans[t1];
            if t1.atom_index == usize::MAX && t1.to >= 0 {
                let to = t1.to as usize;
                self.fa_finish_recurse_determinism(to);
            }
        }
    }

    /// Check whether the associated regexp is determinist,
    /// should be called after xmlFAEliminateEpsilonTransitions()
    #[doc(alias = "xmlFAComputesDeterminism")]
    pub(crate) fn fa_computes_determinism(&mut self) -> i32 {
        let mut ret: i32 = 1;
        let mut deep: i32 = 1;

        if self.determinist != -1 {
            return self.determinist;
        }

        if self.flags & AM_AUTOMATA_RNG as i32 != 0 {
            deep = 0;
        }

        // First cleanup the automata removing cancelled transitions
        for state in self.states.iter_mut().filter_map(|state| state.as_mut()) {
            if state.trans.len() < 2 {
                continue;
            }
            for transnr in 0..state.trans.len() {
                let (trans, rem) = state.trans.split_at_mut(transnr);
                let t1 = &rem[0];
                // Determinism checks in case of counted or all transitions
                // will have to be handled separately
                if t1.atom_index == usize::MAX {
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
                    if t2.atom_index != usize::MAX && t1.to == t2.to {
                        // Here we use deep because we want to keep the
                        // transitions which indicate a conflict
                        if xml_fa_equal_atoms(
                            &self.atoms[t1.atom_index],
                            &self.atoms[t2.atom_index],
                            deep,
                        ) != 0
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
        for statenr in 0..self.states.len() {
            if self
                .get_state(statenr)
                .is_none_or(|state| state.trans.len() < 2)
            {
                continue;
            }
            let mut last = None::<usize>;
            for transnr in 0..self.get_state(statenr).unwrap().trans.len() {
                let t1 = &self.get_state(statenr).unwrap().trans[transnr];
                let mut t1_to = t1.to;
                let mut t1_atom_index = t1.atom_index;
                // Determinism checks in case of counted or all transitions
                // will have to be handled separately
                if t1_atom_index == usize::MAX {
                    continue;
                }
                if t1_to == -1 {
                    // eliminated
                    continue;
                }
                for transnr2 in 0..transnr {
                    let t2 = &self.get_state(statenr).unwrap().trans[transnr2];
                    let t2_to = t2.to;
                    let t2_atom_index = t2.atom_index;
                    if t2_to == -1 {
                        // eliminated
                        continue;
                    }
                    if t2_atom_index != usize::MAX {
                        // But here we don't use deep because we want to
                        // find transitions which indicate a conflict
                        if xml_fa_compare_atoms(
                            &self.atoms[t1_atom_index],
                            &self.atoms[t2.atom_index],
                            1,
                        ) != 0
                        {
                            ret = 0;
                            // mark the transitions as non-deterministic ones
                            self.get_state_mut(statenr).unwrap().trans[transnr].nd = 1;
                            self.get_state_mut(statenr).unwrap().trans[transnr2].nd = 1;
                            last = Some(transnr);
                        }
                    } else if t1_to != -1 {
                        // do the closure in case of remaining specific
                        // epsilon transitions like choices or all
                        ret = self.fa_recurse_determinism(
                            t1_to as usize,
                            t2_to as usize,
                            t2_atom_index,
                        );
                        let t1 = &self.get_state(statenr).unwrap().trans[transnr];
                        t1_to = t1.to;
                        self.fa_finish_recurse_determinism(t1_to as usize);
                        let t1 = &self.get_state(statenr).unwrap().trans[transnr];
                        t1_to = t1.to;
                        t1_atom_index = t1.atom_index;
                        // don't shortcut the computation so all non deterministic
                        // transition get marked down
                        // if (ret == 0)
                        // return(0);
                        if ret == 0 {
                            self.get_state_mut(statenr).unwrap().trans[transnr].nd = 1;
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
                self.get_state_mut(statenr).unwrap().trans[last].nd = 2;
            }

            // don't shortcut the computation so all non deterministic
            // transition get marked down
            // if (ret == 0)
            //     break;
        }

        self.determinist = ret;
        ret
    }

    /// ```text
    /// [27]   charProp    ::=   IsCategory | IsBlock
    /// [28]   IsCategory  ::= Letters | Marks | Numbers | Punctuation |
    ///                       Separators | Symbols | Others
    /// [29]   Letters     ::=   'L' [ultmo]?
    /// [30]   Marks       ::=   'M' [nce]?
    /// [31]   Numbers     ::=   'N' [dlo]?
    /// [32]   Punctuation ::=   'P' [cdseifo]?
    /// [33]   Separators  ::=   'Z' [slp]?
    /// [34]   Symbols     ::=   'S' [mcko]?
    /// [35]   Others      ::=   'C' [cfon]?
    /// [36]   IsBlock     ::=   'Is' [a-zA-Z0-9#x2D]+
    /// ```
    #[doc(alias = "xmlFAParseCharProp")]
    fn fa_parse_char_prop(&mut self) {
        let mut block_name = None;

        let cur = self.current_byte().unwrap_or(0);
        let typ = if cur == b'L' {
            self.cur += 1;
            let cur = self.current_byte().unwrap_or(0);
            if cur == b'u' {
                self.cur += 1;
                XmlRegAtomType::XmlRegexpLetterUppercase
            } else if cur == b'l' {
                self.cur += 1;
                XmlRegAtomType::XmlRegexpLetterLowercase
            } else if cur == b't' {
                self.cur += 1;
                XmlRegAtomType::XmlRegexpLetterTitlecase
            } else if cur == b'm' {
                self.cur += 1;
                XmlRegAtomType::XmlRegexpLetterModifier
            } else if cur == b'o' {
                self.cur += 1;
                XmlRegAtomType::XmlRegexpLetterOthers
            } else {
                XmlRegAtomType::XmlRegexpLetter
            }
        } else if cur == b'M' {
            self.cur += 1;
            let cur = self.current_byte().unwrap_or(0);
            if cur == b'n' {
                self.cur += 1;
                // nonspacing
                XmlRegAtomType::XmlRegexpMarkNonSpacing
            } else if cur == b'c' {
                self.cur += 1;
                // spacing combining
                XmlRegAtomType::XmlRegexpMarkSpaceCombining
            } else if cur == b'e' {
                self.cur += 1;
                // enclosing
                XmlRegAtomType::XmlRegexpMarkEnclosing
            } else {
                // all marks
                XmlRegAtomType::XmlRegexpMark
            }
        } else if cur == b'N' {
            self.cur += 1;
            let cur = self.current_byte().unwrap_or(0);
            if cur == b'd' {
                self.cur += 1;
                // digital
                XmlRegAtomType::XmlRegexpNumberDecimal
            } else if cur == b'l' {
                self.cur += 1;
                // letter
                XmlRegAtomType::XmlRegexpNumberLetter
            } else if cur == b'o' {
                self.cur += 1;
                // other
                XmlRegAtomType::XmlRegexpNumberOthers
            } else {
                // all numbers
                XmlRegAtomType::XmlRegexpNumber
            }
        } else if cur == b'P' {
            self.cur += 1;
            let cur = self.current_byte().unwrap_or(0);
            if cur == b'c' {
                self.cur += 1;
                // connector
                XmlRegAtomType::XmlRegexpPunctConnector
            } else if cur == b'd' {
                self.cur += 1;
                // dash
                XmlRegAtomType::XmlRegexpPunctDash
            } else if cur == b's' {
                self.cur += 1;
                // open
                XmlRegAtomType::XmlRegexpPunctOpen
            } else if cur == b'e' {
                self.cur += 1;
                // close
                XmlRegAtomType::XmlRegexpPunctClose
            } else if cur == b'i' {
                self.cur += 1;
                // initial quote
                XmlRegAtomType::XmlRegexpPunctInitQuote
            } else if cur == b'f' {
                self.cur += 1;
                // final quote
                XmlRegAtomType::XmlRegexpPunctFinQuote
            } else if cur == b'o' {
                self.cur += 1;
                // other
                XmlRegAtomType::XmlRegexpPunctOthers
            } else {
                // all punctuation
                XmlRegAtomType::XmlRegexpPunct
            }
        } else if cur == b'Z' {
            self.cur += 1;
            let cur = self.current_byte().unwrap_or(0);
            if cur == b's' {
                self.cur += 1;
                // space
                XmlRegAtomType::XmlRegexpSeparSpace
            } else if cur == b'l' {
                self.cur += 1;
                // line
                XmlRegAtomType::XmlRegexpSeparLine
            } else if cur == b'p' {
                self.cur += 1;
                // paragraph
                XmlRegAtomType::XmlRegexpSeparPara
            } else {
                // all separators
                XmlRegAtomType::XmlRegexpSepar
            }
        } else if cur == b'S' {
            self.cur += 1;
            let cur = self.current_byte().unwrap_or(0);
            if cur == b'm' {
                // math
                self.cur += 1;
                XmlRegAtomType::XmlRegexpSymbolMath
            } else if cur == b'c' {
                // currency
                self.cur += 1;
                XmlRegAtomType::XmlRegexpSymbolCurrency
            } else if cur == b'k' {
                // modifiers
                self.cur += 1;
                XmlRegAtomType::XmlRegexpSymbolModifier
            } else if cur == b'o' {
                // other
                self.cur += 1;
                XmlRegAtomType::XmlRegexpSymbolOthers
            } else {
                // all symbols
                XmlRegAtomType::XmlRegexpSymbol
            }
        } else if cur == b'C' {
            self.cur += 1;
            let cur = self.current_byte().unwrap_or(0);
            if cur == b'c' {
                self.cur += 1;
                // control
                XmlRegAtomType::XmlRegexpOtherControl
            } else if cur == b'f' {
                self.cur += 1;
                // format
                XmlRegAtomType::XmlRegexpOtherFormat
            } else if cur == b'o' {
                self.cur += 1;
                // private use
                XmlRegAtomType::XmlRegexpOtherPrivate
            } else if cur == b'n' {
                self.cur += 1;
                // not assigned
                XmlRegAtomType::XmlRegexpOtherNa
            } else {
                // all others
                XmlRegAtomType::XmlRegexpOther
            }
        } else if cur == b'I' {
            self.cur += 1;
            if self.current_byte() != Some(b's') {
                ERROR!(self, "IsXXXX expected");
                return;
            }
            self.cur += 1;
            let start = self.current_str();
            let trimmed =
                start.trim_start_matches(|c: char| c.is_ascii_alphanumeric() || c == '\x2D');
            let diff = start.len() - trimmed.len();
            block_name = Some(start[..diff].to_owned());
            self.cur += diff;
            XmlRegAtomType::XmlRegexpBlockName
        } else {
            ERROR!(self, "Unknown char property");
            return;
        };
        if self.atom == usize::MAX {
            self.atom = self.reg_new_atom(typ);
            if self.atom == usize::MAX {
                return;
            }
            self.atoms[self.atom].valuep = block_name;
        } else if matches!(self.atoms[self.atom].typ, XmlRegAtomType::XmlRegexpRanges)
            && self
                .reg_atom_add_range(self.atom, self.neg, typ, 0, 0, block_name.as_deref())
                .is_none()
        {
            // no op
        }
    }

    /// ```text
    /// [23] charClassEsc ::= ( SingleCharEsc | MultiCharEsc | catEsc | complEsc )
    /// [24] SingleCharEsc ::= '\' [nrt\|.?*+(){}#x2D#x5B#x5D#x5E]
    /// [25] catEsc   ::=   '\p{' charProp '}'
    /// [26] complEsc ::=   '\P{' charProp '}'
    /// [37] MultiCharEsc ::= '.' | ('\' [sSiIcCdDwW])
    /// ```
    #[doc(alias = "xmlFAParseCharClassEsc")]
    fn fa_parse_char_class_esc(&mut self) {
        if self.current_byte() == Some(b'.') {
            if self.atom == usize::MAX {
                self.atom = self.reg_new_atom(XmlRegAtomType::XmlRegexpAnyChar);
            } else if matches!(self.atoms[self.atom].typ, XmlRegAtomType::XmlRegexpRanges) {
                self.reg_atom_add_range(
                    self.atom,
                    self.neg,
                    XmlRegAtomType::XmlRegexpAnyChar,
                    0,
                    0,
                    None,
                );
            }
            self.cur += 1;
            return;
        }
        if self.current_byte() != Some(b'\\') {
            ERROR!(self, "Escaped sequence: expecting \\");
            return;
        }
        self.cur += 1;
        match self.current_byte() {
            Some(b'p') => {
                self.cur += 1;
                if self.current_byte() != Some(b'{') {
                    ERROR!(self, "Expecting '{'");
                    return;
                }
                self.cur += 1;
                self.fa_parse_char_prop();
                if self.current_byte() != Some(b'}') {
                    ERROR!(self, "Expecting '}'");
                    return;
                }
                self.cur += 1;
            }
            Some(b'P') => {
                self.cur += 1;
                if self.current_byte() != Some(b'{') {
                    ERROR!(self, "Expecting '{'");
                    return;
                }
                self.cur += 1;
                self.fa_parse_char_prop();
                if self.atom != usize::MAX {
                    self.atoms[self.atom].neg = 1;
                }
                if self.current_byte() != Some(b'}') {
                    ERROR!(self, "Expecting '}'");
                    return;
                }
                self.cur += 1;
            }
            Some(
                mut cur @ (b'n' | b'r' | b't' | b'\\' | b'|' | b'.' | b'?' | b'*' | b'+' | b'('
                | b')' | b'{' | b'}' | 0x2D | 0x5B | 0x5D | 0x5E | b'!' | b'"' | b'#'
                | b'$' | b'%' | b',' | b'/' | b':' | b';' | b'=' | b'>' | b'@' | b'`'
                | b'~' | b'u'),
            ) => {
                // Non-standard escape sequences:
                //     |Java 1.8|.NET Core 3.1|MSXML 6
                //  !  |    +   |      +      |    +
                //  "  |    +   |      +      |    +
                //  #  |    +   |      +      |    +
                //  $  |    +   |      +      |    +
                //  %  |    +   |      +      |    +
                //  ,  |    +   |      +      |    +
                //  /  |    +   |      +      |    +
                //  :  |    +   |      +      |    +
                //  ;  |    +   |      +      |    +
                //  =  |    +   |      +      |    +
                //  >  |        |      +      |    +
                //  @  |    +   |      +      |    +
                //  `  |    +   |      +      |    +
                //  ~  |    +   |      +      |    +
                //  u  |        |      +      |    +
                if self.atom == usize::MAX {
                    self.atom = self.reg_new_atom(XmlRegAtomType::XmlRegexpCharval);
                    if self.atom != usize::MAX {
                        match cur {
                            b'n' => self.atoms[self.atom].codepoint = b'\n' as i32,
                            b'r' => self.atoms[self.atom].codepoint = b'\r' as i32,
                            b't' => self.atoms[self.atom].codepoint = b'\t' as i32,
                            b'u' => {
                                let cur = self.parse_escaped_codepoint();
                                if cur < 0 {
                                    return;
                                }
                                self.atoms[self.atom].codepoint = cur;
                            }
                            _ => {
                                self.atoms[self.atom].codepoint = cur as i32;
                            }
                        }
                    }
                } else if matches!(self.atoms[self.atom].typ, XmlRegAtomType::XmlRegexpRanges) {
                    match cur {
                        b'n' => cur = b'\n',
                        b'r' => cur = b'\r',
                        b't' => cur = b'\t',
                        _ => {}
                    }
                    self.reg_atom_add_range(
                        self.atom,
                        self.neg,
                        XmlRegAtomType::XmlRegexpCharval,
                        cur as i32,
                        cur as i32,
                        None,
                    );
                }
                self.cur += 1;
            }
            Some(cur @ (b's' | b'S' | b'i' | b'I' | b'c' | b'C' | b'd' | b'D' | b'w' | b'W')) => {
                let mut typ: XmlRegAtomType = XmlRegAtomType::XmlRegexpAnySpace;

                match cur {
                    b's' => typ = XmlRegAtomType::XmlRegexpAnySpace,
                    b'S' => typ = XmlRegAtomType::XmlRegexpNotSpace,
                    b'i' => typ = XmlRegAtomType::XmlRegexpInitName,
                    b'I' => typ = XmlRegAtomType::XmlRegexpNotInitName,
                    b'c' => typ = XmlRegAtomType::XmlRegexpNameChar,
                    b'C' => typ = XmlRegAtomType::XmlRegexpNotNameChar,
                    b'd' => typ = XmlRegAtomType::XmlRegexpDecimal,
                    b'D' => typ = XmlRegAtomType::XmlRegexpNotDecimal,
                    b'w' => typ = XmlRegAtomType::XmlRegexpRealChar,
                    b'W' => typ = XmlRegAtomType::XmlRegexpNotRealChar,
                    _ => {}
                }
                self.cur += 1;
                if self.atom == usize::MAX {
                    self.atom = self.reg_new_atom(typ);
                } else if matches!(self.atoms[self.atom].typ, XmlRegAtomType::XmlRegexpRanges) {
                    self.reg_atom_add_range(self.atom, self.neg, typ, 0, 0, None);
                }
            }
            _ => {
                ERROR!(self, "Wrong escape sequence, misuse of character '\\'");
            }
        }
    }

    /// ```text
    /// [17]   charRange   ::=     seRange | XmlCharRef | XmlCharIncDash
    /// [18]   seRange   ::=   charOrEsc '-' charOrEsc
    /// [20]   charOrEsc   ::=   XmlChar | SingleCharEsc
    /// [21]   XmlChar   ::=   [^\#x2D#x5B#x5D]
    /// [22]   XmlCharIncDash   ::=   [^\#x5B#x5D]
    /// ```
    #[doc(alias = "xmlFAParseCharRange")]
    fn fa_parse_char_range(&mut self) {
        let mut len: usize;
        let start: i32;
        let mut end: i32;

        let Some(cur) = self.current_byte() else {
            ERROR!(self, "Expecting ']'");
            return;
        };

        if cur == b'\\' {
            self.cur += 1;
            match self.current_byte() {
                Some(b'n') => start = 0xA,
                Some(b'r') => start = 0xD,
                Some(b't') => start = 0x9,
                Some(
                    cur @ (b'\\' | b'|' | b'.' | b'-' | b'^' | b'?' | b'*' | b'+' | b'{' | b'}'
                    | b'(' | b')' | b'[' | b']'),
                ) => start = cur as i32,
                _ => {
                    ERROR!(self, "Invalid escape value");
                    return;
                }
            }
            end = start;
            len = 1;
        } else if cur != 0x5B && cur != 0x5D {
            if let Some(c) = self.current_char() {
                start = c as i32;
                end = start;
                len = c.len_utf8();
            } else {
                (start, end, len) = (0, 0, 0);
            }
        } else {
            ERROR!(self, "Expecting a char range");
            return;
        }
        // Since we are "inside" a range, we can assume self.cur is past
        // the start of self.string, and PREV should be safe
        if start == '-' as i32
            && self.nth_byte(1) != Some(b']')
            && self.prev_byte(1) != Some(b'[')
            && self.prev_byte(1) != Some(b'^')
        {
            self.cur += len;
            return;
        }
        self.cur += len;
        let cur = self.current_byte().unwrap_or(0);
        if cur != b'-' || self.nth_byte(1) == Some(b'[') || self.nth_byte(1) == Some(b']') {
            self.reg_atom_add_range(
                self.atom,
                self.neg,
                XmlRegAtomType::XmlRegexpCharval,
                start,
                end,
                None,
            );
            return;
        }
        self.cur += 1;
        let cur = self.current_byte().unwrap_or(0);
        if cur == b'\\' {
            self.cur += 1;
            match self.current_byte() {
                Some(b'n') => end = 0xA,
                Some(b'r') => end = 0xD,
                Some(b't') => end = 0x9,
                Some(
                    cur @ (b'\\' | b'|' | b'.' | b'-' | b'^' | b'?' | b'*' | b'+' | b'{' | b'}'
                    | b'(' | b')' | b'[' | b']'),
                ) => {
                    end = cur as i32;
                }
                _ => {
                    ERROR!(self, "Invalid escape value");
                    return;
                }
            }
            len = 1;
        } else if cur != 0 && cur != 0x5B && cur != 0x5D {
            if let Some(c) = self.current_char() {
                end = c as i32;
                len = c.len_utf8();
            } else {
                end = 0;
                len = 0;
            }
        } else {
            ERROR!(self, "Expecting the end of a char range");
            return;
        }

        // TODO check that the values are acceptable character ranges for XML
        if end < start {
            ERROR!(self, "End of range is before start of range");
        } else {
            self.cur += len;
            self.reg_atom_add_range(
                self.atom,
                self.neg,
                XmlRegAtomType::XmlRegexpCharval,
                start,
                end,
                None,
            );
        }
    }

    /// ```text
    /// [14]   posCharGroup ::= ( charRange | charClassEsc  )+
    /// ```
    #[doc(alias = "xmlFAParsePosCharGroup")]
    fn fa_parse_pos_char_group(&mut self) {
        if self.current_byte() == Some(b'\\') {
            self.fa_parse_char_class_esc();
        } else {
            self.fa_parse_char_range();
        }
        while self.error == 0 && !matches!(self.current_byte(), Some(b']' | b'-') | None) {
            if self.current_byte() == Some(b'\\') {
                self.fa_parse_char_class_esc();
            } else {
                self.fa_parse_char_range();
            }
        }
    }

    /// ```text
    /// [13]   charGroup     ::= posCharGroup | negCharGroup | charClassSub
    /// [15]   negCharGroup  ::= '^' posCharGroup
    /// [16]   charClassSub  ::= ( posCharGroup | negCharGroup ) '-' charClassExpr
    /// [12]   charClassExpr ::= '[' charGroup ']'
    /// ```
    #[doc(alias = "xmlFAParseCharGroup")]
    fn fa_parse_char_group(&mut self) {
        let neg = self.neg;

        if self.current_byte() == Some(b'^') {
            self.cur += 1;
            self.neg = (self.neg == 0) as u8;
            self.fa_parse_pos_char_group();
            self.neg = neg;
        }
        while self.current_byte() != Some(b']') && self.error == 0 {
            if self.current_byte() == Some(b'-') && self.nth_byte(1) == Some(b'[') {
                self.cur += 1; /* eat the '-' */
                self.cur += 1; /* eat the '[' */
                self.neg = 2;
                self.fa_parse_char_group();
                self.neg = neg;
                if self.current_byte() == Some(b']') {
                    self.cur += 1;
                } else {
                    ERROR!(self, "charClassExpr: ']' expected");
                }
                break;
            } else {
                self.fa_parse_pos_char_group();
            }
        }
    }

    /// ```text
    /// [11]   charClass       ::=     charClassEsc | charClassExpr
    /// [12]   charClassExpr   ::=   '[' charGroup ']'
    /// ```
    #[doc(alias = "xmlFAParseCharClass")]
    fn fa_parse_char_class(&mut self) {
        if self.current_byte() == Some(b'[') {
            self.cur += 1;
            self.atom = self.reg_new_atom(XmlRegAtomType::XmlRegexpRanges);
            if self.atom == usize::MAX {
                return;
            }
            self.fa_parse_char_group();
            if self.current_byte() == Some(b']') {
                self.cur += 1;
            } else {
                ERROR!(self, "xmlFAParseCharClass: ']' expected");
            }
        } else {
            self.fa_parse_char_class_esc();
        }
    }

    /// ```text
    /// [9]   atom   ::=   Char | charClass | ( '(' regExp ')' )
    /// ```
    #[doc(alias = "xmlFAParseAtom")]
    fn fa_parse_atom(&mut self) -> i32 {
        if let Some(codepoint) = self.fa_is_char() {
            self.atom = self.reg_new_atom(XmlRegAtomType::XmlRegexpCharval);
            if self.atom == usize::MAX {
                return -1;
            }
            self.atoms[self.atom].codepoint = codepoint as i32;
            self.cur += codepoint.len_utf8();
            return 1;
        } else if self.current_byte() == Some(b'|')
            || self.current_byte().is_none()
            || self.current_byte() == Some(b')')
        {
            return 0;
        } else if self.current_byte() == Some(b'(') {
            self.cur += 1;
            if self.depth >= 50 {
                ERROR!(self, "xmlFAParseAtom: maximum nesting depth exceeded");
                return -1;
            }
            // this extra Epsilon transition is needed if we count with 0 allowed
            // unfortunately this can't be known at that point
            self.fa_generate_epsilon_transition(self.state, usize::MAX);
            let start0 = self.state;
            self.fa_generate_epsilon_transition(self.state, usize::MAX);
            let start = self.state;
            let oldend = self.end;
            self.end = usize::MAX;
            self.atom = usize::MAX;
            self.depth += 1;
            self.fa_parse_reg_exp(0);
            self.depth -= 1;
            if self.current_byte() == Some(b')') {
                self.cur += 1;
            } else {
                ERROR!(self, "xmlFAParseAtom: expecting ')'");
            }
            self.atom = self.reg_new_atom(XmlRegAtomType::XmlRegexpSubReg);
            if self.atom == usize::MAX {
                return -1;
            }
            self.atoms[self.atom].start = start;
            self.atoms[self.atom].start0 = start0;
            self.atoms[self.atom].stop = self.state;
            self.end = oldend;
            return 1;
        } else if self.current_byte() == Some(b'[')
            || self.current_byte() == Some(b'\\')
            || self.current_byte() == Some(b'.')
        {
            self.fa_parse_char_class();
            return 1;
        }
        0
    }

    /// ```text
    /// [8]   QuantExact   ::=   [0-9]+
    /// ```
    ///
    /// Returns 0 if success or -1 in case of error
    #[doc(alias = "xmlFAParseQuantExact")]
    fn fa_parse_quant_exact(&mut self) -> Option<i32> {
        let mut ret = 0i32;
        let mut ok = false;
        let mut overflow = false;

        while let Some(d) = self.current_byte().filter(|b| b.is_ascii_digit()) {
            let (r, f) = ret.overflowing_mul(10);
            overflow |= f;
            let (r, f) = r.overflowing_add(d as i32 - b'0' as i32);
            overflow |= f;
            ret = r;
            ok = true;
            self.cur += 1;
        }
        (ok && !overflow).then_some(ret)
    }

    /// ```text
    /// [4]   quantifier ::=   [?*+] | ( '{' quantity '}' )
    /// [5]   quantity   ::=   quantRange | quantMin | QuantExact
    /// [6]   quantRange ::=   QuantExact ',' QuantExact
    /// [7]   quantMin   ::=   QuantExact ','
    /// [8]   QuantExact ::=   [0-9]+
    /// ```
    #[doc(alias = "xmlFAParseQuantifier")]
    fn fa_parse_quantifier(&mut self) -> i32 {
        let cur = self.current_byte().unwrap_or(0);
        if cur == b'?' || cur == b'*' || cur == b'+' {
            if self.atom != usize::MAX {
                if cur == b'?' {
                    self.atoms[self.atom].quant = XmlRegQuantType::XmlRegexpQuantOpt;
                } else if cur == b'*' {
                    self.atoms[self.atom].quant = XmlRegQuantType::XmlRegexpQuantMult;
                } else if cur == b'+' {
                    self.atoms[self.atom].quant = XmlRegQuantType::XmlRegexpQuantPlus;
                }
            }
            self.cur += 1;
            return 1;
        }
        if cur == b'{' {
            let mut min: i32 = 0;
            let mut max: i32 = 0;

            self.cur += 1;
            if let Some(cur) = self.fa_parse_quant_exact() {
                min = cur
            } else {
                ERROR!(self, "Improper quantifier");
            }
            if self.current_byte() == Some(b',') {
                self.cur += 1;
                if self.current_byte() == Some(b'}') {
                    max = i32::MAX;
                } else if let Some(cur) = self.fa_parse_quant_exact() {
                    max = cur;
                } else {
                    ERROR!(self, "Improper quantifier");
                }
            }
            if self.current_byte() == Some(b'}') {
                self.cur += 1;
            } else {
                ERROR!(self, "Unterminated quantifier");
            }
            if max == 0 {
                max = min;
            }
            if self.atom != usize::MAX {
                self.atoms[self.atom].quant = XmlRegQuantType::XmlRegexpQuantRange;
                self.atoms[self.atom].min = min;
                self.atoms[self.atom].max = max;
            }
            return 1;
        }
        0
    }

    /// ```text
    /// [3]   piece   ::=   atom quantifier?
    /// ```
    #[doc(alias = "xmlFAParsePiece")]
    fn fa_parse_piece(&mut self) -> i32 {
        self.atom = usize::MAX;
        let ret: i32 = self.fa_parse_atom();
        if ret == 0 {
            return 0;
        }
        if self.atom == usize::MAX {
            ERROR!(self, "internal: no atom generated");
        }
        self.fa_parse_quantifier();
        1
    }

    /// @to is used to optimize by removing duplicate path in automata
    /// in expressions like (a|b)(c|d)
    ///
    /// ```text
    /// [2]   branch   ::=   piece*
    /// ```
    #[doc(alias = "xmlFAParseBranch")]
    fn fa_parse_branch(&mut self, to: usize) -> i32 {
        let mut ret: i32;

        let mut previous = self.state;
        ret = self.fa_parse_piece();
        if ret == 0 {
            // Empty branch
            self.fa_generate_epsilon_transition(previous, to);
        } else {
            if self.fa_generate_transitions(
                previous,
                if self.current_byte() == Some(b'|')
                    || self.current_byte() == Some(b')')
                    || self.current_byte().is_none()
                {
                    to
                } else {
                    usize::MAX
                },
                self.atom,
            ) < 0
            {
                self.atom = usize::MAX;
                return -1;
            }
            previous = self.state;
            self.atom = usize::MAX;
        }
        while ret != 0 && self.error == 0 {
            ret = self.fa_parse_piece();
            if ret != 0 {
                if self.fa_generate_transitions(
                    previous,
                    if self.current_byte() == Some(b'|')
                        || self.current_byte() == Some(b')')
                        || self.current_byte().is_none()
                    {
                        to
                    } else {
                        usize::MAX
                    },
                    self.atom,
                ) < 0
                {
                    self.atom = usize::MAX;
                    return -1;
                }
                previous = self.state;
                self.atom = usize::MAX;
            }
        }
        0
    }

    /// ```text
    /// [1]   regExp   ::=     branch  ( '|' branch )*
    /// ```
    #[doc(alias = "xmlFAParseRegExp")]
    fn fa_parse_reg_exp(&mut self, top: i32) {
        // if not top start should have been generated by an epsilon trans
        let start = self.state;
        self.end = usize::MAX;
        self.fa_parse_branch(usize::MAX);
        if top != 0 {
            self.get_state_mut(self.state).unwrap().typ = XmlRegStateType::XmlRegexpFinalState;
        }
        if self.current_byte() != Some(b'|') {
            self.end = self.state;
            return;
        }
        let end = self.state;
        while self.current_byte() == Some(b'|') as _ && self.error == 0 {
            self.cur += 1;
            self.state = start;
            self.end = usize::MAX;
            self.fa_parse_branch(end);
        }
        if top == 0 {
            self.state = end;
            self.end = end;
        }
    }
}

const AM_AUTOMATA_RNG: usize = 1;

/// A libxml regular expression, they can actually be far more complex
/// thank the POSIX regex expressions.
#[doc(alias = "xmlRegexp")]
#[repr(C)]
pub struct XmlRegexp {
    string: Box<str>,
    states: Vec<Option<XmlRegState>>,
    atoms: Vec<XmlRegAtom>,
    counters: Vec<XmlRegCounter>,
    determinist: i32,
    flags: i32,
    // That's the compact form for determinists automatas
    nbstates: i32,
    compact: Vec<Vec<i32>>,
    transdata: Vec<Vec<*mut c_void>>,
    string_map: Vec<String>,
}

impl XmlRegexp {
    /// Parses a regular expression conforming to XML Schemas Part 2 Datatype
    /// Appendix F and builds an automata suitable for testing strings against
    /// that regular expression
    ///
    /// Returns the compiled expression or NULL in case of error
    #[doc(alias = "xmlRegexpCompile")]
    pub fn compile(regexp: &str) -> Option<Self> {
        let mut ctxt = XmlRegParserCtxt::new_parser(Some(regexp));

        // initialize the parser
        ctxt.state = ctxt.reg_state_push();
        if ctxt.state == usize::MAX {
            return None;
        }
        ctxt.start = ctxt.state;
        ctxt.end = usize::MAX;

        // parse the expression building an automata
        ctxt.fa_parse_reg_exp(1);
        if ctxt.current_byte().is_some() {
            ERROR!(&mut ctxt, "xmlFAParseRegExp: extra characters");
        }
        if ctxt.error != 0 {
            return None;
        }
        ctxt.end = ctxt.state;
        ctxt.get_state_mut(ctxt.start).unwrap().typ = XmlRegStateType::XmlRegexpStartState;
        ctxt.get_state_mut(ctxt.end).unwrap().typ = XmlRegStateType::XmlRegexpFinalState;

        // remove the Epsilon except for counted transitions
        ctxt.fa_eliminate_epsilon_transitions();

        if ctxt.error != 0 {
            return None;
        }
        ctxt.parse()
    }

    pub(crate) fn computes_determinism(&mut self) -> i32 {
        if self.determinist != -1 {
            return self.determinist;
        }

        let Some(mut am) = XmlAutomata::new() else {
            return -1;
        };
        am.states.clear();
        am.atoms = take(&mut self.atoms);
        am.states = take(&mut self.states);
        am.determinist = -1;
        am.flags = self.flags;
        let ret: i32 = am.fa_computes_determinism();
        self.atoms = take(&mut am.atoms);
        self.states = take(&mut am.states);
        self.determinist = ret;
        ret
    }

    /// Check if the regular expression is determinist
    ///
    /// Returns 1 if it yes, 0 if not and a negative value in case of error
    #[doc(alias = "xmlRegexpIsDeterminist")]
    pub fn is_determinist(&self) -> i32 {
        // The original `xmlRegexpIsDeterminist` performed the same process
        // as `XmlRegexp::computes_determinism`.
        // Therefore, this function did not return -1.
        assert_ne!(self.determinist, -1);
        self.determinist
    }

    #[doc(alias = "xmlRegPrintState")]
    fn print_state(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        state: Option<&XmlRegState>,
    ) -> std::fmt::Result {
        write!(f, " state: ")?;
        let Some(state) = state else {
            writeln!(f, "NULL")?;
            return Ok(());
        };
        if matches!(state.typ, XmlRegStateType::XmlRegexpStartState) {
            write!(f, "START ")?;
        }
        if matches!(state.typ, XmlRegStateType::XmlRegexpFinalState) {
            write!(f, "FINAL ")?;
        }

        writeln!(f, "{}, {} transitions:", state.no, state.trans.len(),)?;
        for trans in &state.trans {
            self.print_trans(f, trans)?;
        }
        Ok(())
    }

    #[doc(alias = "xmlRegPrintState")]
    fn print_trans(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        trans: &XmlRegTrans,
    ) -> std::fmt::Result {
        write!(f, "  trans: ")?;
        if trans.to < 0 {
            writeln!(f, "removed")?;
            return Ok(());
        }
        if trans.nd != 0 {
            if trans.nd == 2 {
                write!(f, "last not determinist, ")?;
            } else {
                write!(f, "not determinist, ")?;
            }
        }
        if trans.counter >= 0 {
            write!(f, "counted {}, ", trans.counter)?;
        }
        if trans.count as usize == REGEXP_ALL_COUNTER {
            write!(f, "all transition, ")?;
        } else if trans.count >= 0 {
            write!(f, "count based {}, ", trans.count)?;
        }
        if trans.atom_index == usize::MAX {
            writeln!(f, "epsilon to {}", trans.to)?;
            return Ok(());
        }
        if matches!(
            self.atoms[trans.atom_index].typ,
            XmlRegAtomType::XmlRegexpCharval
        ) {
            write!(
                f,
                "char {} ",
                char::from_u32(self.atoms[trans.atom_index].codepoint as u32).unwrap()
            )?;
        }
        writeln!(
            f,
            "atom {}, to {}",
            self.atoms[trans.atom_index].no, trans.to
        )?;
        Ok(())
    }
}

impl Debug for XmlRegexp {
    /// Print the content of the compiled regular expression
    #[doc(alias = "xmlRegexpPrint")]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, " regexp: ")?;
        write!(f, "'{}' ", self.string)?;
        writeln!(f)?;
        writeln!(f, "{} atoms:", self.atoms.len())?;
        for (i, atom) in self.atoms.iter().enumerate() {
            write!(f, " {i:02} ")?;
            write!(f, "{atom}")?;
        }
        write!(f, "{} states:", self.states.len())?;
        writeln!(f)?;
        for state in &self.states {
            self.print_state(f, state.as_ref())?;
        }
        writeln!(f, "{} counters:", self.counters.len())?;
        for (i, counter) in self.counters.iter().enumerate() {
            writeln!(f, " {i}: min {} max {}", counter.min, counter.max)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for XmlRegexp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Default for XmlRegexp {
    fn default() -> Self {
        Self {
            string: "".to_owned().into_boxed_str(),
            states: vec![],
            atoms: vec![],
            counters: vec![],
            determinist: 0,
            flags: 0,
            nbstates: 0,
            compact: vec![],
            transdata: vec![],
            string_map: vec![],
        }
    }
}

pub type XmlRegExecRollbackPtr = *mut XmlRegExecRollback;
#[doc(alias = "xmlRegExecRollback")]
#[repr(C)]
pub struct XmlRegExecRollback {
    state: usize,     /* the current state */
    index: usize,     /* the index in the input stack */
    nextbranch: i32,  /* the next transition to explore in that state */
    counts: Vec<i32>, /* save the automata state if it has some */
}

pub type XmlRegInputTokenPtr = *mut XmlRegInputToken;
#[doc(alias = "xmlRegInputToken")]
#[repr(C)]
pub struct XmlRegInputToken {
    value: Option<Rc<str>>,
    data: *mut c_void,
}

/// A libxml progressive regular expression evaluation context
#[doc(alias = "xmlRegExecCtxtPtr")]
pub type XmlRegExecCtxtPtr = *mut XmlRegExecCtxt;
#[doc(alias = "xmlRegExecCtxt")]
#[repr(C)]
pub struct XmlRegExecCtxt {
    status: i32,         /* execution status != 0 indicate an error */
    determinist: i32,    /* did we find an indeterministic behaviour */
    comp: Rc<XmlRegexp>, /* the compiled regexp */
    callback: Option<XmlRegExecCallbacks>,
    data: *mut c_void,

    state: usize,    /* the current state */
    transno: i32,    /* the current transition on that state */
    transcount: i32, /* the number of chars in c_char counted transitions */
    // A stack of rollback states
    rollbacks: Vec<XmlRegExecRollback>,

    // The state of the automata if any
    counts: Vec<i32>,

    // The input stack
    index: usize,
    char_stack: *mut i32,
    input_string: Box<str>,             /* when operating on characters */
    input_stack: Vec<XmlRegInputToken>, /* when operating on strings */

    // error handling
    err_state_no: i32,            /* the error state number */
    err_state: usize,             /* the error state */
    err_string: Option<Box<str>>, /* the string raising the error */
    err_counts: Vec<i32>,         /* counters at the error state */
    nb_push: i32,
}

impl XmlRegExecCtxt {
    /// Build a context used for progressive evaluation of a regexp.
    ///
    /// Returns the new context
    #[doc(alias = "xmlRegNewExecCtxt")]
    pub fn new(
        comp: Rc<XmlRegexp>,
        callback: Option<XmlRegExecCallbacks>,
        data: *mut c_void,
    ) -> Self {
        let mut exec = XmlRegExecCtxt {
            index: 0,
            determinist: 1,
            status: 0,
            comp,
            transno: 0,
            transcount: 0,
            callback,
            data,
            err_state_no: -1,
            err_string: None,
            nb_push: 0,
            ..Default::default()
        };
        if exec.comp.compact.is_empty() {
            exec.state = 0;
        }
        if !exec.comp.counters.is_empty() {
            exec.counts.clear();
            exec.counts.resize(exec.comp.counters.len(), 0);
            exec.err_counts.clear();
            exec.err_counts.resize(exec.comp.counters.len(), 0);
        } else {
            exec.counts.clear();
            exec.err_counts.clear();
        }
        exec
    }

    fn current_str(&self) -> &str {
        &self.input_string[self.index..]
    }

    fn current_char(&self) -> Option<char> {
        self.current_str().chars().next()
    }

    /// Get the current `XmlRegTrans`.
    fn trans(&self) -> &XmlRegTrans {
        self.state()
            .trans
            .get(self.transno as usize)
            .expect("Invalid State")
    }

    /// Get the number of `XmlRegTrans` kept by the current `XmlRegState`.
    fn num_transes(&self) -> usize {
        self.state().trans.len()
    }

    /// Get the current `XmlRegState`.
    fn state(&self) -> &XmlRegState {
        self.comp
            .states
            .get(self.state)
            .and_then(|state| state.as_ref())
            .expect("Invalid State")
    }

    /// Get the current `XmlRegStateType`
    fn state_type(&self) -> XmlRegStateType {
        self.state().typ
    }

    /// Get the current `XmlRegAtom`.
    fn atom(&self) -> &XmlRegAtom {
        self.comp
            .atoms
            .get(self.trans().atom_index)
            .expect("Invalid State")
    }

    /// Advance the state to the next state.
    fn next_state(&mut self) {
        self.state = self.trans().to as usize;
        self.transno = 0;
    }

    #[doc(alias = "xmlFARegExecRollBack")]
    fn rollback(&mut self) {
        let Some(rollback) = self.rollbacks.pop() else {
            self.status = -1;
            return;
        };
        self.state = rollback.state;
        self.index = rollback.index;
        self.transno = rollback.nextbranch;
        if !self.comp.counters.is_empty() {
            self.counts.clear();
            self.counts
                .extend_from_slice(&rollback.counts[..self.comp.counters.len()]);
        }
    }

    #[doc(alias = "xmlFARegExecSave")]
    fn save(&mut self) {
        if self.nb_push as usize > MAX_PUSH {
            return;
        }
        self.nb_push += 1;

        let mut rollback = XmlRegExecRollback {
            state: self.state,
            index: self.index,
            nextbranch: self.transno + 1,
            counts: vec![],
        };
        if !self.comp.counters.is_empty() {
            rollback
                .counts
                .extend_from_slice(&self.counts[..self.comp.counters.len()]);
        }
        self.rollbacks.push(rollback);
    }

    #[doc(alias = "xmlFARegExecSaveInputString")]
    fn save_input_string(&mut self, value: Option<Rc<str>>, data: *mut c_void) {
        self.input_stack.push(XmlRegInputToken { value, data });
    }

    /// Push one input token in the execution context
    ///
    /// Returns: 1 if the regexp reached a final state, 0 if non-final,
    /// and a negative value in case of error.
    #[doc(alias = "xmlRegCompactPushString")]
    unsafe fn compact_push_string(
        &mut self,
        comp: Rc<XmlRegexp>,
        value: Option<&str>,
        data: *mut c_void,
    ) -> i32 {
        unsafe {
            let state = self.index;

            if comp.compact.is_empty() {
                return -1;
            }

            let Some(value) = value else {
                // are we at a final state ?
                if comp.compact[state][0] == XmlRegStateType::XmlRegexpFinalState as i32 {
                    return 1;
                }
                return 0;
            };

            // Examine all outside transitions from current state
            for i in 0..comp.string_map.len() {
                let mut target = comp.compact[state][i + 1];
                if target > 0 && target <= comp.nbstates {
                    target -= 1; /* to avoid 0 */
                    if xml_reg_str_equal_wildcard(Some(&comp.string_map[i]), Some(value)) != 0 {
                        self.index = target as usize;
                        if let Some(callback) = self.callback {
                            if !comp.transdata.is_empty() {
                                callback(self.data as _, value, comp.transdata[state][i], data);
                            }
                        }
                        if comp.compact[target as usize][0]
                            == XmlRegStateType::XmlRegexpSinkState as i32
                        {
                            // goto error;
                            break;
                        }

                        if comp.compact[target as usize][0]
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
            self.err_string = Some(value.to_owned().into_boxed_str());
            self.err_state_no = state as i32;
            self.status = -1;
            -1
        }
    }

    /// Push one input token in the execution context
    ///
    /// Returns: 1 if the regexp reached a final state, 0 if non-final,
    /// and a negative value in case of error.
    #[doc(alias = "xmlRegExecPushStringInternal")]
    unsafe fn push_string_internal(
        &mut self,
        value: Option<&str>,
        mut data: *mut c_void,
        compound: i32,
    ) -> i32 {
        unsafe {
            let mut ret: i32;
            let mut is_final: i32 = 0;
            let mut progress: i32 = 1;

            if self.status != 0 {
                return self.status;
            }

            if !self.comp.compact.is_empty() {
                return self.compact_push_string(self.comp.clone(), value, data);
            }

            if value.is_none() {
                if matches!(
                    self.comp.states[self.state].as_ref().unwrap().typ,
                    XmlRegStateType::XmlRegexpFinalState
                ) {
                    return 1;
                }
                is_final = 1;
            }

            let mut value = value.map(Rc::from);

            // If we have an active rollback stack push the new value there
            // and get back to where we were left
            if value.is_some() && !self.input_stack.is_empty() {
                self.save_input_string(value, data);
                value = self.input_stack[self.index].value.clone();
                data = self.input_stack[self.index].data;
            }

            'b: while self.status == 0
                && (value.is_some()
                    || (is_final == 1
                        && !matches!(self.state_type(), XmlRegStateType::XmlRegexpFinalState)))
            {
                'rollback: {
                    'progress: {
                        // End of input on non-terminal state, rollback, however we may
                        // still have epsilon like transition for counted transitions
                        // on counters, in that case don't break too early.
                        if value.is_none() && self.counts.is_empty() {
                            break 'rollback;
                        }

                        self.transcount = 0;
                        self.transno -= 1;
                        while {
                            self.transno += 1;
                            self.transno < self.num_transes() as i32
                        } {
                            if self.trans().to < 0 {
                                continue;
                            }
                            ret = 0;
                            if self.trans().count as usize == REGEXP_ALL_LAX_COUNTER {
                                let mut count: i32;

                                ret = 0;

                                // Check all counted transitions from the current state
                                if value.is_none() && is_final != 0 {
                                    ret = 1;
                                } else if let Some(value) = value.as_deref() {
                                    for (i, t) in self.state().trans.iter().enumerate() {
                                        if t.counter < 0 || i == self.transno as usize {
                                            continue;
                                        }
                                        let counter = self.comp.counters[t.counter as usize];
                                        count = self.counts[t.counter as usize];
                                        if count < counter.max
                                            && t.atom_index != usize::MAX
                                            && Some(value)
                                                == self.comp.atoms[t.atom_index].valuep.as_deref()
                                        {
                                            ret = 0;
                                            break;
                                        }
                                        if count >= counter.min
                                            && count < counter.max
                                            && t.atom_index != usize::MAX
                                            && Some(value)
                                                == self.comp.atoms[t.atom_index].valuep.as_deref()
                                        {
                                            ret = 1;
                                            break;
                                        }
                                    }
                                }
                            } else if self.trans().count as usize == REGEXP_ALL_COUNTER {
                                let mut count: i32;

                                ret = 1;

                                // Check all counted transitions from the current state
                                for (i, t) in self.state().trans.iter().enumerate() {
                                    if t.counter < 0 || i == self.transno as usize {
                                        continue;
                                    }
                                    let counter = self.comp.counters[t.counter as usize];
                                    count = self.counts[t.counter as usize];
                                    if count < counter.min || count > counter.max {
                                        ret = 0;
                                        break;
                                    }
                                }
                            } else if self.trans().count >= 0 {
                                // A counted transition.
                                let count = self.counts[self.trans().count as usize];
                                let counter = self.comp.counters[self.trans().count as usize];
                                ret = (count >= counter.min && count <= counter.max) as _;
                            } else if self.trans().atom_index == usize::MAX {
                                eprintln!("epsilon transition left at runtime");
                                self.status = -2;
                                break;
                            } else if value.is_some() {
                                ret = xml_reg_str_equal_wildcard(
                                    self.atom().valuep.as_deref(),
                                    value.as_deref(),
                                );
                                if self.atom().neg != 0 {
                                    ret = (ret == 0) as i32;
                                    if compound == 0 {
                                        ret = 0;
                                    }
                                }
                                if ret == 1 && self.trans().counter >= 0 {
                                    let count = self.counts[self.trans().counter as usize];
                                    let counter = self.comp.counters[self.trans().counter as usize];
                                    if count >= counter.max {
                                        ret = 0;
                                    }
                                }

                                if ret == 1 && self.atom().min > 0 && self.atom().max > 0 {
                                    let to = self.trans().to as usize;

                                    // this is a multiple input sequence
                                    if self.num_transes() as i32 > self.transno + 1 {
                                        if self.input_stack.is_empty() {
                                            self.save_input_string(value.clone(), data);
                                        }
                                        self.save();
                                    }
                                    self.transcount = 1;
                                    'inner: while {
                                        // Try to progress as much as possible on the input
                                        if self.transcount == self.atom().max {
                                            break 'inner;
                                        }
                                        self.index += 1;
                                        // End of input: stop here
                                        if self.index == self.input_stack.len() {
                                            self.index -= 1;
                                            value = None;
                                            data = null_mut();
                                            break 'inner;
                                        }
                                        value = self.input_stack[self.index].value.clone();
                                        data = self.input_stack[self.index].data;

                                        // End of input: stop here
                                        if value.is_none() {
                                            self.index -= 1;
                                            break 'inner;
                                        }
                                        if self.transcount >= self.atom().min {
                                            let transno: i32 = self.transno;
                                            let state = self.state;

                                            // The transition is acceptable save it
                                            self.transno = -1; /* trick */
                                            self.state = to;
                                            if self.input_stack.is_empty() {
                                                self.save_input_string(value.clone(), data);
                                            }
                                            self.save();
                                            self.transno = transno;
                                            self.state = state;
                                        }
                                        ret = (value.as_deref() == self.atom().valuep.as_deref())
                                            as i32;
                                        self.transcount += 1;
                                        ret == 1
                                    } {}
                                    if self.transcount < self.atom().min {
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
                                if let Some(callback) = self.callback {
                                    if !data.is_null() && self.trans().atom_index != usize::MAX {
                                        callback(
                                            self.data as _,
                                            self.atom().valuep.as_deref().unwrap(),
                                            self.atom().data as _,
                                            data,
                                        );
                                    }
                                }
                                if self.num_transes() as i32 > self.transno + 1 {
                                    if self.input_stack.is_empty() {
                                        self.save_input_string(value.clone(), data);
                                    }
                                    self.save();
                                }
                                if self.trans().counter >= 0 {
                                    let counter = self.trans().counter as usize;
                                    self.counts[counter] += 1;
                                }
                                if self.trans().count >= 0
                                    && (self.trans().count as usize) < REGEXP_ALL_COUNTER
                                {
                                    let count = self.trans().count as usize;
                                    self.counts[count] = 0;
                                }
                                if self.trans().to != -1
                                    && self.comp.states[self.trans().to as usize].is_some()
                                    && matches!(
                                        self.state_type(),
                                        XmlRegStateType::XmlRegexpSinkState
                                    )
                                {
                                    // entering a sink state, save the current state as error state.
                                    self.err_string = value
                                        .as_deref()
                                        .map(|value| value.to_owned().into_boxed_str());
                                    self.err_state = self.state;
                                    self.err_counts.copy_from_slice(&self.counts);
                                }
                                if self.trans().atom_index != usize::MAX {
                                    if !self.input_stack.is_empty() {
                                        self.index += 1;
                                        if self.index < self.input_stack.len() {
                                            value = self.input_stack[self.index].value.clone();
                                            data = self.input_stack[self.index].data;
                                        } else {
                                            value = None;
                                            data = null_mut();
                                        }
                                    } else {
                                        value = None;
                                        data = null_mut();
                                    }
                                }
                                self.next_state();
                                break 'progress;
                            } else if ret < 0 {
                                self.status = -4;
                                break;
                            }
                        }
                        if self.transno != 0
                            || self.comp.states[self.state]
                                .as_ref()
                                .unwrap()
                                .trans
                                .is_empty()
                        {
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
                    && self.state != usize::MAX
                    && !matches!(self.state_type(), XmlRegStateType::XmlRegexpSinkState)
                {
                    progress = 0;
                    self.err_string = value
                        .as_deref()
                        .map(|value| value.to_owned().into_boxed_str());
                    self.err_state = self.state;
                    if !self.comp.counters.is_empty() {
                        self.err_counts.copy_from_slice(&self.counts);
                    }
                }

                // Failed to find a way out
                self.determinist = 0;
                self.rollback();
                if !self.input_stack.is_empty() && self.status == 0 {
                    if self.index < self.input_stack.len() {
                        value = self.input_stack[self.index].value.clone();
                        data = self.input_stack[self.index].data;
                    } else {
                        value = None;
                        data = null_mut();
                    }
                }
            }
            if self.status == 0 {
                return matches!(self.state_type(), XmlRegStateType::XmlRegexpFinalState) as _;
            }
            self.status
        }
    }

    /// Push one input token in the execution context
    ///
    /// Returns: 1 if the regexp reached a final state, 0 if non-final,
    /// and a negative value in case of error.
    #[doc(alias = "xmlRegExecPushString")]
    pub unsafe fn push_string(&mut self, value: Option<&str>, data: *mut c_void) -> i32 {
        unsafe { self.push_string_internal(value, data, 0) }
    }

    /// Push one input token in the execution context
    ///
    /// Returns: 1 if the regexp reached a final state, 0 if non-final,
    /// and a negative value in case of error.
    #[doc(alias = "xmlRegExecPushString2")]
    pub unsafe fn push_string2(
        &mut self,
        value: &str,
        value2: Option<&str>,
        data: *mut c_void,
    ) -> i32 {
        unsafe {
            if self.status != 0 {
                return self.status;
            }

            let Some(value2) = value2 else {
                return self.push_string(Some(value), data);
            };

            let s = format!("{value}{}{value2}", XML_REG_STRING_SEPARATOR);
            let ret = if !self.comp.compact.is_empty() {
                self.compact_push_string(self.comp.clone(), Some(s.as_str()), data)
            } else {
                self.push_string_internal(Some(s.as_str()), data, 1)
            };

            ret
        }
    }

    /// Extract information from the regexp execution, internal routine to
    /// implement xmlRegExecNextValues() and xmlRegExecErrInfo()
    ///
    /// If successfully collected, return `Some((num_val, num_neg, values_slice))`.  
    /// Otherwise, return `None`.
    #[doc(alias = "xmlRegExecGetValues")]
    unsafe fn get_values<'a>(
        &self,
        err: i32,
        values: &'a mut [Cow<'static, str>],
        terminal: *mut i32,
    ) -> Option<(usize, usize, &'a [Cow<'static, str>])> {
        unsafe {
            if values.is_empty() {
                return None;
            }

            let maxval = values.len();
            let mut nb = 0;
            let mut nbval = 0;
            let mut nbneg = 0;
            if !self.comp.compact.is_empty() {
                let mut target: i32;
                let comp = self.comp.clone();

                let state = if err != 0 {
                    if self.err_state_no == -1 {
                        return None;
                    }
                    self.err_state_no
                } else {
                    self.index as i32
                };
                if !terminal.is_null() {
                    if comp.compact[state as usize][0]
                        == XmlRegStateType::XmlRegexpFinalState as i32
                    {
                        *terminal = 1;
                    } else {
                        *terminal = 0;
                    }
                }
                if nb < maxval {
                    for i in 0..comp.string_map.len() {
                        target = comp.compact[state as usize][i + 1];
                        if target > 0
                            && target <= comp.nbstates
                            && comp.compact[(target - 1) as usize][0]
                                != XmlRegStateType::XmlRegexpSinkState as i32
                        {
                            values[nb] = Cow::Owned(comp.string_map[i].clone());
                            nb += 1;
                            nbval += 1;
                        }
                        if nb >= maxval {
                            break;
                        }
                    }
                }
                if nb < maxval {
                    for i in 0..comp.string_map.len() {
                        target = comp.compact[state as usize][i + 1];
                        if target > 0
                            && target <= comp.nbstates
                            && comp.compact[(target - 1) as usize][0]
                                == XmlRegStateType::XmlRegexpSinkState as i32
                        {
                            values[nb] = Cow::Owned(comp.string_map[i].clone());
                            nb += 1;
                            nbneg += 1;
                        }
                        if nb >= maxval {
                            break;
                        }
                    }
                }
            } else {
                if !terminal.is_null() {
                    if matches!(
                        self.comp.states[self.state].as_ref().unwrap().typ,
                        XmlRegStateType::XmlRegexpFinalState
                    ) {
                        *terminal = 1;
                    } else {
                        *terminal = 0;
                    }
                }

                let state = if err != 0 {
                    if self.err_state == usize::MAX || self.comp.states[self.err_state].is_none() {
                        return None;
                    }
                    self.err_state
                } else {
                    if self.state == usize::MAX || self.comp.states[self.state].is_none() {
                        return None;
                    }
                    self.state
                };
                if nb < maxval {
                    for trans in &self.comp.states[state].as_ref().unwrap().trans {
                        if trans.to < 0 {
                            continue;
                        }
                        if trans.atom_index == usize::MAX {
                            continue;
                        }
                        let atom = &self.comp.atoms[trans.atom_index];
                        if atom.valuep.is_none() {
                            continue;
                        }
                        if trans.count as usize == REGEXP_ALL_LAX_COUNTER {
                            // this should not be reached but ...
                            todo!()
                        } else if trans.count as usize == REGEXP_ALL_COUNTER {
                            // this should not be reached but ...
                            todo!()
                        } else if trans.counter >= 0 {
                            let count = if err != 0 {
                                self.err_counts[trans.counter as usize]
                            } else {
                                self.counts[trans.counter as usize]
                            };
                            let counter = self.comp.counters[trans.counter as usize];
                            if count < counter.max {
                                if atom.neg != 0 {
                                    values[nb] =
                                        Cow::Owned(atom.valuep2.as_deref().unwrap().to_owned());
                                    nb += 1;
                                } else {
                                    values[nb] =
                                        Cow::Owned(atom.valuep.as_deref().unwrap().to_owned());
                                    nb += 1;
                                }
                                nbval += 1;
                            }
                        } else if self.comp.states[trans.to as usize].is_some()
                            && !matches!(
                                self.comp.states[trans.to as usize].as_ref().unwrap().typ,
                                XmlRegStateType::XmlRegexpSinkState,
                            )
                        {
                            if atom.neg != 0 {
                                values[nb] =
                                    Cow::Owned(atom.valuep2.as_deref().unwrap().to_owned());
                                nb += 1;
                            } else {
                                values[nb] = Cow::Owned(atom.valuep.as_deref().unwrap().to_owned());
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
                    for trans in &self.comp.states[state].as_ref().unwrap().trans {
                        if trans.to < 0 {
                            continue;
                        }
                        if trans.atom_index == usize::MAX {
                            continue;
                        }
                        let atom = &self.comp.atoms[trans.atom_index];
                        if atom.valuep.is_none() {
                            continue;
                        }
                        if trans.count as usize == REGEXP_ALL_LAX_COUNTER
                            || trans.count as usize == REGEXP_ALL_COUNTER
                            || trans.counter >= 0
                        {
                            continue;
                        } else if (self.comp.states[trans.to as usize]).is_some()
                            && matches!(
                                self.comp.states[trans.to as usize].as_ref().unwrap().typ,
                                XmlRegStateType::XmlRegexpSinkState
                            )
                        {
                            if atom.neg != 0 {
                                values[nb] =
                                    Cow::Owned(atom.valuep2.as_deref().unwrap().to_owned());
                                nb += 1
                            } else {
                                values[nb] = Cow::Owned(atom.valuep.as_deref().unwrap().to_owned());
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
    pub unsafe fn next_values<'a>(
        &self,
        values: &'a mut [Cow<'static, str>],
        terminal: *mut i32,
    ) -> Option<(usize, usize, &'a [Cow<'static, str>])> {
        unsafe { self.get_values(0, values, terminal) }
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
    pub unsafe fn err_info<'a>(
        &self,
        string: Option<&mut Option<Box<str>>>,
        values: &'a mut [Cow<'static, str>],
        terminal: *mut i32,
    ) -> Option<(usize, usize, &'a [Cow<'static, str>])> {
        unsafe {
            if let Some(string) = string {
                if self.status != 0 {
                    *string = self.err_string.clone();
                } else {
                    *string = None;
                }
            }
            self.get_values(1, values, terminal)
        }
    }
}

impl Default for XmlRegExecCtxt {
    fn default() -> Self {
        Self {
            status: 0,
            determinist: 0,
            comp: Rc::new(Default::default()),
            callback: None,
            data: null_mut(),
            state: usize::MAX,
            transno: 0,
            transcount: 0,
            rollbacks: vec![],
            counts: vec![],
            index: 0,
            char_stack: null_mut(),
            input_string: "".to_owned().into_boxed_str(),
            input_stack: vec![],
            err_state_no: 0,
            err_state: usize::MAX,
            err_string: None,
            err_counts: vec![],
            nb_push: 0,
        }
    }
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
fn xml_regexp_err_compile(ctxt: &mut XmlRegParserCtxt, extra: &str) {
    let mut regexp = None;
    let mut idx = 0;

    if !ctxt.string.is_empty() {
        regexp = Some(ctxt.string.to_string().into());
        idx = ctxt.cur as i32;
        ctxt.error = XmlParserErrors::XmlRegexpCompileError as _;
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
        regexp,
        None,
        idx,
        0,
        "failed to compile: {}\n",
        extra
    );
}

fn xml_reg_check_character_range(
    typ: XmlRegAtomType,
    codepoint: i32,
    mut neg: i32,
    start: i32,
    end: i32,
    block_name: Option<&str>,
) -> i32 {
    let ret = match typ {
        XmlRegAtomType::XmlRegexpString
        | XmlRegAtomType::XmlRegexpSubReg
        | XmlRegAtomType::XmlRegexpRanges
        | XmlRegAtomType::XmlRegexpEpsilon => {
            return -1;
        }
        XmlRegAtomType::XmlRegexpAnyChar => codepoint != '\n' as i32 && codepoint != '\r' as i32,
        XmlRegAtomType::XmlRegexpCharval => codepoint >= start && codepoint <= end,
        XmlRegAtomType::XmlRegexpNotSpace => {
            neg = (neg == 0) as i32;
            codepoint == '\n' as i32
                || codepoint == '\r' as i32
                || codepoint == '\t' as i32
                || codepoint == ' ' as i32
        }
        XmlRegAtomType::XmlRegexpAnySpace => {
            codepoint == '\n' as i32
                || codepoint == '\r' as i32
                || codepoint == '\t' as i32
                || codepoint == ' ' as i32
        }
        XmlRegAtomType::XmlRegexpNotInitName => {
            neg = (neg == 0) as i32;
            xml_is_letter(codepoint as u32) || codepoint == '_' as i32 || codepoint == ':' as i32
        }
        XmlRegAtomType::XmlRegexpInitName => {
            xml_is_letter(codepoint as u32) || codepoint == '_' as i32 || codepoint == ':' as i32
        }
        XmlRegAtomType::XmlRegexpNotNameChar => {
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
        XmlRegAtomType::XmlRegexpNameChar => {
            xml_is_letter(codepoint as u32)
                || xml_is_digit(codepoint as u32)
                || codepoint == '.' as i32
                || codepoint == '-' as i32
                || codepoint == '_' as i32
                || codepoint == ':' as i32
                || xml_is_combining(codepoint as u32)
                || xml_is_extender(codepoint as u32)
        }
        XmlRegAtomType::XmlRegexpNotDecimal => {
            neg = (neg == 0) as i32;
            xml_ucs_is_cat_nd(codepoint)
        }
        XmlRegAtomType::XmlRegexpDecimal => xml_ucs_is_cat_nd(codepoint),
        XmlRegAtomType::XmlRegexpRealChar => {
            neg = (neg == 0) as i32;
            xml_ucs_is_cat_p(codepoint)
                || xml_ucs_is_cat_z(codepoint)
                || xml_ucs_is_cat_c(codepoint)
        }
        XmlRegAtomType::XmlRegexpNotRealChar => {
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
        XmlRegAtomType::XmlRegexpMarkNonSpacing => xml_ucs_is_cat_mn(codepoint),
        XmlRegAtomType::XmlRegexpMarkSpaceCombining => xml_ucs_is_cat_mc(codepoint),
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
        XmlRegAtomType::XmlRegexpPunctInitQuote => xml_ucs_is_cat_pi(codepoint),
        XmlRegAtomType::XmlRegexpPunctFinQuote => xml_ucs_is_cat_pf(codepoint),
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

pub(crate) const REGEXP_ALL_COUNTER: usize = 0x123456;

/// Check if the regular expression generates the value
///
/// Returns 1 if it matches, 0 if not and a negative value in case of error
#[doc(alias = "xmlRegexpExec", alias = "xmlFARegExec")]
pub fn xml_regexp_exec(comp: Rc<XmlRegexp>, content: &str) -> i32 {
    let mut exec = XmlRegExecCtxt::default();
    let mut ret: i32;

    exec.input_string = content.to_owned().into_boxed_str();
    exec.index = 0;
    exec.nb_push = 0;
    exec.determinist = 1;
    exec.rollbacks.clear();
    exec.status = 0;
    exec.comp = comp;
    exec.state = 0;
    exec.transno = 0;
    exec.transcount = 0;
    if !exec.comp.counters.is_empty() {
        exec.counts.clear();
        exec.counts.resize(exec.comp.counters.len(), 0);
    } else {
        exec.counts.clear();
    }
    'error: {
        'b: while !(exec.status != 0
            || exec.state == usize::MAX
            || exec.current_str().is_empty()
                && matches!(exec.state_type(), XmlRegStateType::XmlRegexpFinalState))
        {
            'rollback: {
                // If end of input on non-terminal state, rollback, however we may
                // still have epsilon like transition for counted transitions
                // on counters, in that case don't break too early.  Additionally,
                // if we are working on a range like "AB{0,2}", where B is not present,
                // we don't want to break.
                let mut len = 1;
                if exec.current_str().is_empty() && exec.counts.is_empty() {
                    // if there is a transition, we must check if
                    //  atom allows minOccurs of 0
                    if exec.transno < exec.num_transes() as i32 {
                        if exec.trans().to >= 0 && !(exec.atom().min == 0 && exec.atom().max > 0) {
                            break 'rollback;
                        }
                    } else {
                        break 'rollback;
                    }
                }

                exec.transcount = 0;
                exec.transno -= 1;
                while {
                    exec.transno += 1;
                    exec.transno < exec.num_transes() as i32
                } {
                    if exec.trans().to < 0 {
                        continue;
                    }
                    ret = 0;
                    let mut deter = 1;
                    if exec.trans().count >= 0 {
                        if exec.counts.is_empty() {
                            exec.status = -1;
                            break 'error;
                        }
                        // A counted transition.

                        let count = exec.counts[exec.trans().count as usize];
                        let counter = exec.comp.counters[exec.trans().count as usize];
                        ret = (count >= counter.min && count <= counter.max) as i32;
                        if ret != 0 && counter.min != counter.max {
                            deter = 0;
                        }
                    } else if exec.trans().atom_index == usize::MAX {
                        eprintln!("epsilon transition left at runtime");
                        exec.status = -2;
                        break;
                    } else if let Some(codepoint) = exec.current_char() {
                        ret = exec.atom().check_character(codepoint as i32);
                        if ret == 1 && exec.atom().min >= 0 && exec.atom().max > 0 {
                            let to = exec.trans().to as usize;

                            // this is a multiple input sequence
                            // If there is a counter associated increment it now.
                            // do not increment if the counter is already over the
                            // maximum limit in which case get to next transition
                            if exec.trans().counter >= 0 {
                                if exec.counts.is_empty() {
                                    exec.status = -1;
                                    break 'error;
                                }
                                let counter = exec.comp.counters[exec.trans().counter as usize];
                                if exec.counts[exec.trans().counter as usize] >= counter.max {
                                    // for loop on transitions
                                    continue;
                                }
                            }
                            // Save before incrementing
                            if exec.num_transes() as i32 > exec.transno + 1 {
                                exec.save();
                            }
                            if exec.trans().counter >= 0 {
                                let counter = exec.trans().counter as usize;
                                exec.counts[counter] += 1;
                            }
                            exec.transcount = 1;
                            'inner: while {
                                // Try to progress as much as possible on the input
                                if exec.transcount == exec.atom().max {
                                    break 'inner;
                                }
                                exec.index += codepoint.len_utf8();
                                // End of input: stop here
                                if exec.current_str().is_empty() {
                                    exec.index -= codepoint.len_utf8();
                                    break 'inner;
                                }
                                if exec.transcount >= exec.atom().min {
                                    let transno: i32 = exec.transno;
                                    let state = exec.state;

                                    // The transition is acceptable save it
                                    exec.transno = -1; /* trick */
                                    exec.state = to;
                                    exec.save();
                                    exec.transno = transno;
                                    exec.state = state;
                                }
                                let codepoint = exec.current_char().unwrap();
                                ret = exec.atom().check_character(codepoint as i32);
                                exec.transcount += 1;
                                ret == 1
                            } {}
                            if exec.transcount < exec.atom().min {
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
                            if exec.trans().counter >= 0 {
                                if exec.counts.is_empty() {
                                    exec.status = -1;
                                    break 'error;
                                }
                                let counter = exec.trans().counter as usize;
                                exec.counts[counter] -= 1;
                            }
                        } else if ret == 0 && exec.atom().min == 0 && exec.atom().max > 0 {
                            // we don't match on the codepoint, but minOccurs of 0
                            // says that's ok.  Setting len to 0 inhibits stepping
                            // over the codepoint.
                            exec.transcount = 1;
                            len = 0;
                            ret = 1;
                        }
                    } else if exec.atom().min == 0 && exec.atom().max > 0 {
                        // another spot to match when minOccurs is 0
                        exec.transcount = 1;
                        len = 0;
                        ret = 1;
                    }
                    if ret == 1 {
                        if exec.trans().nd == 1
                            || (exec.trans().count >= 0
                                && deter == 0
                                && exec.comp.states[exec.state].as_ref().unwrap().trans.len()
                                    as i32
                                    > exec.transno + 1)
                        {
                            exec.save();
                        }
                        if exec.trans().counter >= 0 {
                            // make sure we don't go over the counter maximum value
                            if exec.counts.is_empty() {
                                exec.status = -1;
                                break 'error;
                            }
                            let c = exec.trans().counter as usize;
                            let counter = exec.comp.counters[c];
                            if exec.counts[exec.trans().counter as usize] >= counter.max {
                                // for loop on transitions
                                continue;
                            }
                            exec.counts[c] += 1;
                        }
                        if exec.trans().count >= 0
                            && (exec.trans().count as usize) < REGEXP_ALL_COUNTER
                        {
                            if exec.counts.is_empty() {
                                exec.status = -1;
                                break 'error;
                            }
                            let count = exec.trans().count as usize;
                            exec.counts[count] = 0;
                        }
                        if exec.trans().atom_index != usize::MAX {
                            exec.index += len;
                        }
                        exec.next_state();
                        continue 'b;
                    } else if ret < 0 {
                        exec.status = -4;
                        break;
                    }
                }
                if exec.transno != 0
                    || exec.comp.states[exec.state]
                        .as_ref()
                        .unwrap()
                        .trans
                        .is_empty()
                {
                    // rollback:
                    break 'rollback;
                }
                continue 'b;
            }
            // Failed to find a way out
            exec.determinist = 0;
            exec.rollback();
        }
    }
    // error:
    exec.rollbacks.clear();
    if exec.state == usize::MAX {
        return -1;
    }
    exec.counts.clear();
    if exec.status == 0 {
        return 1;
    }
    if exec.status == -1 {
        if exec.nb_push as usize > MAX_PUSH {
            return -1;
        }
        return 0;
    }
    exec.status
}

/// Compares two atoms to check whether they are the same exactly
/// this is used to remove equivalent transitions
///
/// Returns 1 if same and 0 otherwise
#[doc(alias = "xmlFAEqualAtoms")]
fn xml_fa_equal_atoms(atom1: &XmlRegAtom, atom2: &XmlRegAtom, deep: i32) -> i32 {
    let mut ret: i32 = 0;

    if std::ptr::eq(atom1, atom2) {
        return 1;
    }

    if atom1.typ != atom2.typ {
        return 0;
    }
    match atom1.typ {
        XmlRegAtomType::XmlRegexpEpsilon => {
            ret = 0;
        }
        XmlRegAtomType::XmlRegexpString => {
            if deep == 0 {
                ret = std::ptr::eq(&atom1.valuep, &atom2.valuep) as i32;
            } else {
                ret = (atom1.valuep == atom2.valuep) as i32;
            }
        }
        XmlRegAtomType::XmlRegexpCharval => {
            ret = (atom1.codepoint == atom2.codepoint) as i32;
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
fn xml_fa_compare_atom_types(mut type1: XmlRegAtomType, mut type2: XmlRegAtomType) -> i32 {
    if matches!(
        type1,
        XmlRegAtomType::XmlRegexpEpsilon
            | XmlRegAtomType::XmlRegexpCharval
            | XmlRegAtomType::XmlRegexpRanges
            | XmlRegAtomType::XmlRegexpSubReg
            | XmlRegAtomType::XmlRegexpString
            | XmlRegAtomType::XmlRegexpAnyChar
    ) {
        return 1;
    }
    if matches!(
        type2,
        XmlRegAtomType::XmlRegexpEpsilon
            | XmlRegAtomType::XmlRegexpCharval
            | XmlRegAtomType::XmlRegexpRanges
            | XmlRegAtomType::XmlRegexpSubReg
            | XmlRegAtomType::XmlRegexpString
            | XmlRegAtomType::XmlRegexpAnyChar
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
        XmlRegAtomType::XmlRegexpAnySpace => {
            // \s
            // can't be a letter, number, mark, punctuation, symbol
            if type2 == XmlRegAtomType::XmlRegexpNotSpace
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
        XmlRegAtomType::XmlRegexpNotSpace => { /* \S */ }
        XmlRegAtomType::XmlRegexpInitName => {
            // \l
            // can't be a number, mark, separator, punctuation, symbol or other
            if type2 == XmlRegAtomType::XmlRegexpNotInitName
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
        XmlRegAtomType::XmlRegexpNotInitName => { /* \L */ }
        XmlRegAtomType::XmlRegexpNameChar => {
            // \c
            // can't be a mark, separator, punctuation, symbol or other
            if type2 == XmlRegAtomType::XmlRegexpNotNameChar
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
        XmlRegAtomType::XmlRegexpNotNameChar => { /* \C */ }
        XmlRegAtomType::XmlRegexpDecimal => {
            // \d
            // can't be a letter, mark, separator, punctuation, symbol or other
            if type2 == XmlRegAtomType::XmlRegexpNotDecimal
                || type2 == XmlRegAtomType::XmlRegexpRealChar
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
        XmlRegAtomType::XmlRegexpNotDecimal => { /* \D */ }
        XmlRegAtomType::XmlRegexpRealChar => {
            // \w
            // can't be a mark, separator, punctuation, symbol or other
            if type2 == XmlRegAtomType::XmlRegexpNotDecimal
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
        XmlRegAtomType::XmlRegexpNotRealChar => { /* \W */ }
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
        XmlRegAtomType::XmlRegexpMarkNonSpacing
        | XmlRegAtomType::XmlRegexpMarkSpaceCombining
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
        | XmlRegAtomType::XmlRegexpPunctInitQuote
        | XmlRegAtomType::XmlRegexpPunctFinQuote
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
fn xml_reg_str_equal_wildcard(exp_str: Option<&str>, val_str: Option<&str>) -> i32 {
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

fn xml_fa_compare_ranges(range1: &XmlRegRange, range2: &XmlRegRange) -> i32 {
    let mut ret: i32;

    if matches!(
        range1.typ,
        XmlRegAtomType::XmlRegexpRanges
            | XmlRegAtomType::XmlRegexpSubReg
            | XmlRegAtomType::XmlRegexpString
    ) || matches!(
        range2.typ,
        XmlRegAtomType::XmlRegexpRanges
            | XmlRegAtomType::XmlRegexpSubReg
            | XmlRegAtomType::XmlRegexpString
    ) {
        return -1;
    }

    // put them in order
    let (range1, range2) = if range1.typ > range2.typ {
        (range2, range1)
    } else {
        (range1, range2)
    };
    if range1.typ == XmlRegAtomType::XmlRegexpAnyChar
        || range2.typ == XmlRegAtomType::XmlRegexpAnyChar
    {
        ret = 1;
    } else if range1.typ == XmlRegAtomType::XmlRegexpEpsilon
        || range2.typ == XmlRegAtomType::XmlRegexpEpsilon
    {
        return 0;
    } else if range1.typ == range2.typ {
        if range1.typ != XmlRegAtomType::XmlRegexpCharval {
            ret = 1;
        } else if range1.end < range2.start || range2.end < range1.start {
            ret = 0;
        } else {
            ret = 1;
        }
    } else if range1.typ == XmlRegAtomType::XmlRegexpCharval {
        let mut neg: i32 = 0;

        // just check all codepoints in the range for acceptance,
        // this is usually way cheaper since done only once at
        // compilation than testing over and over at runtime or
        // pushing too many states when evaluating.
        if (range1.neg == 0 && range2.neg != 0) || (range1.neg != 0 && range2.neg == 0) {
            neg = 1;
        }

        for codepoint in range1.start..=range1.end {
            ret = xml_reg_check_character_range(
                range2.typ,
                codepoint,
                0,
                range2.start,
                range2.end,
                range2.block_name.as_deref(),
            );
            if ret < 0 {
                return -1;
            }
            if (neg == 1 && ret == 0) || (neg == 0 && ret == 1) {
                return 1;
            }
        }
        return 0;
    } else if range1.typ == XmlRegAtomType::XmlRegexpBlockName
        || range2.typ == XmlRegAtomType::XmlRegexpBlockName
    {
        if range1.typ == range2.typ {
            ret = (range1.block_name == range2.block_name) as i32;
        } else {
            // comparing a block range with anything else is way
            // too costly, and maintaining the table is like too much
            // memory too, so let's force the automata to save state here.
            return 1;
        }
    } else if range1.typ < XmlRegAtomType::XmlRegexpLetter
        || range2.typ < XmlRegAtomType::XmlRegexpLetter
    {
        if (range1.typ == XmlRegAtomType::XmlRegexpAnySpace
            && range2.typ == XmlRegAtomType::XmlRegexpNotSpace)
            || (range1.typ == XmlRegAtomType::XmlRegexpInitName
                && range2.typ == XmlRegAtomType::XmlRegexpNotInitName)
            || (range1.typ == XmlRegAtomType::XmlRegexpNameChar
                && range2.typ == XmlRegAtomType::XmlRegexpNotNameChar)
            || (range1.typ == XmlRegAtomType::XmlRegexpDecimal
                && range2.typ == XmlRegAtomType::XmlRegexpNotDecimal)
            || (range1.typ == XmlRegAtomType::XmlRegexpRealChar
                && range2.typ == XmlRegAtomType::XmlRegexpNotRealChar)
        {
            ret = 0;
        } else {
            // same thing to limit complexity
            return 1;
        }
    } else {
        ret = 0;
        // (*range1).typ < (*range2).typ here
        match range1.typ {
            XmlRegAtomType::XmlRegexpLetter => {
                // all disjoint except in the subgroups
                if matches!(
                    range2.typ,
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
                    range2.typ,
                    XmlRegAtomType::XmlRegexpMarkNonSpacing
                        | XmlRegAtomType::XmlRegexpMarkSpaceCombining
                        | XmlRegAtomType::XmlRegexpMarkEnclosing
                ) {
                    ret = 1;
                }
            }
            XmlRegAtomType::XmlRegexpNumber => {
                if matches!(
                    range2.typ,
                    XmlRegAtomType::XmlRegexpNumberDecimal
                        | XmlRegAtomType::XmlRegexpNumberLetter
                        | XmlRegAtomType::XmlRegexpNumberOthers
                ) {
                    ret = 1;
                }
            }
            XmlRegAtomType::XmlRegexpPunct => {
                if matches!(
                    range2.typ,
                    XmlRegAtomType::XmlRegexpPunctConnector
                        | XmlRegAtomType::XmlRegexpPunctDash
                        | XmlRegAtomType::XmlRegexpPunctOpen
                        | XmlRegAtomType::XmlRegexpPunctClose
                        | XmlRegAtomType::XmlRegexpPunctInitQuote
                        | XmlRegAtomType::XmlRegexpPunctFinQuote
                        | XmlRegAtomType::XmlRegexpPunctOthers
                ) {
                    ret = 1;
                }
            }
            XmlRegAtomType::XmlRegexpSepar => {
                if matches!(
                    range2.typ,
                    XmlRegAtomType::XmlRegexpSeparSpace
                        | XmlRegAtomType::XmlRegexpSeparLine
                        | XmlRegAtomType::XmlRegexpSeparPara
                ) {
                    ret = 1;
                }
            }
            XmlRegAtomType::XmlRegexpSymbol => {
                if matches!(
                    range2.typ,
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
                    range2.typ,
                    XmlRegAtomType::XmlRegexpOtherControl
                        | XmlRegAtomType::XmlRegexpOtherFormat
                        | XmlRegAtomType::XmlRegexpOtherPrivate
                ) {
                    ret = 1;
                }
            }
            _ => {
                if range2.typ >= XmlRegAtomType::XmlRegexpLetter
                    && range2.typ < XmlRegAtomType::XmlRegexpBlockName
                {
                    ret = 0;
                } else {
                    // safety net !
                    return 1;
                }
            }
        }
    }
    if (range1.neg == 0 && range2.neg != 0) || (range1.neg != 0 && range2.neg == 0) {
        ret = (ret == 0) as i32;
    }
    ret
}

/// Compares two atoms to check whether they intersect in some ways,
/// this is used by xmlFAComputesDeterminism and xmlFARecurseDeterminism only
///
/// Returns 1 if yes and 0 otherwise
#[doc(alias = "xmlFACompareAtoms")]
fn xml_fa_compare_atoms(atom1: &XmlRegAtom, atom2: &XmlRegAtom, deep: i32) -> i32 {
    let mut ret: i32 = 1;

    if std::ptr::eq(atom1, atom2) {
        return 1;
    }

    if matches!(atom1.typ, XmlRegAtomType::XmlRegexpAnyChar)
        || matches!(atom2.typ, XmlRegAtomType::XmlRegexpAnyChar)
    {
        return 1;
    }

    let (atom1, atom2) = if atom1.typ > atom2.typ {
        (atom2, atom1)
    } else {
        (atom1, atom2)
    };
    if atom1.typ != atom2.typ {
        ret = xml_fa_compare_atom_types(atom1.typ, atom2.typ);
        // if they can't intersect at the type level break now
        if ret == 0 {
            return 0;
        }
    }
    'done: {
        match atom1.typ {
            XmlRegAtomType::XmlRegexpString => {
                if deep == 0 {
                    ret = (atom1.valuep != atom2.valuep) as i32;
                } else {
                    let val1 = atom1.valuep.as_deref();
                    let val2 = atom2.valuep.as_deref();
                    let compound1 = val1.is_some_and(|v| v.contains('|'));
                    let compound2 = val2.is_some_and(|v| v.contains('|'));

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
                if matches!(atom2.typ, XmlRegAtomType::XmlRegexpCharval) {
                    ret = (atom1.codepoint == atom2.codepoint) as i32;
                } else {
                    ret = atom2.check_character(atom1.codepoint);
                    if ret < 0 {
                        ret = 1;
                    }
                }
            }
            XmlRegAtomType::XmlRegexpRanges => {
                if matches!(atom2.typ, XmlRegAtomType::XmlRegexpRanges) {
                    let mut res: i32;

                    // need to check that none of the ranges eventually matches
                    for r1 in &atom1.ranges {
                        for r2 in &atom2.ranges {
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
    if atom1.neg != atom2.neg {
        ret = (ret == 0) as i32;
    }
    if ret == 0 {
        return 0;
    }
    // not_determinist:
    1
}

/// Callback function when doing a transition in the automata
#[doc(alias = "xmlRegExecCallbacks")]
pub type XmlRegExecCallbacks =
    unsafe fn(exec: XmlRegExecCtxtPtr, token: &str, transdata: *mut c_void, inputdata: *mut c_void);

pub(crate) const REGEXP_ALL_LAX_COUNTER: usize = 0x123457;

const XML_REG_STRING_SEPARATOR: char = '|';

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
    unsafe {
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
}

/// Creates a new context for manipulating expressions
///
/// Returns the context or NULL in case of error
#[doc(alias = "xmlExpNewCtxt")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_new_ctxt(mut max_nodes: i32, dict: XmlDictPtr) -> XmlExpCtxtPtr {
    unsafe {
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
}

/// Debugging facility provides the number of allocated nodes at a that point
///
/// Returns the number of nodes in use or -1 in case of error
#[doc(alias = "xmlExpCtxtNbNodes")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_ctxt_nb_nodes(ctxt: XmlExpCtxtPtr) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return -1;
        }
        (*ctxt).nb_nodes
    }
}

/// Debugging facility provides the number of allocated nodes over lifetime
///
/// Returns the number of nodes ever allocated or -1 in case of error
#[doc(alias = "xmlExpCtxtNbCons")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_ctxt_nb_cons(ctxt: XmlExpCtxtPtr) -> i32 {
    unsafe {
        if ctxt.is_null() {
            return -1;
        }
        (*ctxt).nb_cons
    }
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
    unsafe {
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
}

/// Increase the reference count of the expression
#[doc(alias = "xmlExpRef")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_ref(exp: XmlExpNodePtr) {
    unsafe {
        if !exp.is_null() {
            (*exp).refe += 1;
        }
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
    unsafe {
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
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_parse_or(ctxt: XmlExpCtxtPtr) -> XmlExpNodePtr {
    unsafe {
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
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_parse_seq(ctxt: XmlExpCtxtPtr) -> XmlExpNodePtr {
    unsafe {
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
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_parse_expr(ctxt: XmlExpCtxtPtr) -> XmlExpNodePtr {
    unsafe {
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
    unsafe {
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
}

/// Calculate the hash key for a token
#[doc(alias = "xmlExpHashNameComputeKey")]
#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_hash_name_compute_key(mut name: *const XmlChar) -> u16 {
    unsafe {
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
}

/// Calculate the hash key for a compound expression
#[doc(alias = "xmlExpHashComputeKey")]
#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_hash_compute_key(
    typ: XmlExpNodeType,
    left: XmlExpNodePtr,
    right: XmlExpNodePtr,
) -> u16 {
    unsafe {
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
}

#[cfg(feature = "libxml_expr")]
const MAX_NODES: usize = 10000;

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_new_node(ctxt: XmlExpCtxtPtr, typ: XmlExpNodeType) -> XmlExpNodePtr {
    unsafe {
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
    unsafe {
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
                    } else if (*insert).exp_left == left
                        && (*insert).field.children.f_right == right
                    {
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
                if (*(*entry).exp_left).c_max == -1
                    || (*(*entry).field.children.f_right).c_max == -1
                {
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
                if (*(*entry).exp_left).c_max == -1
                    || (*(*entry).field.children.f_right).c_max == -1
                {
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
    unsafe {
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
    unsafe {
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
    unsafe {
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
    unsafe {
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
}

/// Finds if the expression is nillable, i.e. if it accepts the empty sequence
///
/// Returns 1 if nillable, 0 if not and -1 in case of error
#[doc(alias = "xmlExpIsNillable")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_is_nillable(exp: XmlExpNodePtr) -> i32 {
    unsafe {
        if exp.is_null() {
            return -1;
        }
        (IS_NILLABLE!(exp) != 0) as i32
    }
}

/// Indicate the maximum number of input a expression can accept
///
/// Returns the maximum length or -1 in case of error
#[doc(alias = "xmlExpMaxToken")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_max_token(expr: XmlExpNodePtr) -> i32 {
    unsafe {
        if expr.is_null() {
            return -1;
        }
        (*expr).c_max
    }
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_get_language_int(
    _ctxt: XmlExpCtxtPtr,
    mut exp: XmlExpNodePtr,
    list: *mut *const XmlChar,
    len: i32,
    nb: i32,
) -> i32 {
    unsafe {
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
                tmp2 = xml_exp_get_language_int(
                    _ctxt,
                    (*exp).field.children.f_right,
                    list,
                    len,
                    nb + tmp,
                );
                if tmp2 < 0 {
                    return tmp2;
                }
                return tmp + tmp2;
            }
        }
        // return (-1);
    }
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
    unsafe {
        if ctxt.is_null() || exp.is_null() || lang_list.is_null() || len <= 0 {
            return -1;
        }
        xml_exp_get_language_int(ctxt, exp, lang_list, len, 0)
    }
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_get_start_int(
    _ctxt: XmlExpCtxtPtr,
    mut exp: XmlExpNodePtr,
    list: *mut *const XmlChar,
    len: i32,
    nb: i32,
) -> i32 {
    unsafe {
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
                return tmp + tmp2;
            }
        }
        // return -1;
    }
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
    unsafe {
        if ctxt.is_null() || exp.is_null() || tok_list.is_null() || len <= 0 {
            return -1;
        }
        xml_exp_get_start_int(ctxt, exp, tok_list, len, 0)
    }
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_string_derive_int(
    ctxt: XmlExpCtxtPtr,
    exp: XmlExpNodePtr,
    str: *const XmlChar,
) -> XmlExpNodePtr {
    unsafe {
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
    unsafe {
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
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_check_card(exp: XmlExpNodePtr, sub: XmlExpNodePtr) -> i32 {
    unsafe {
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
    unsafe {
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
    unsafe {
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
                ret = xml_exp_hash_get_entry(
                    ctxt,
                    XmlExpNodeType::XmlExpSeq,
                    tmp,
                    tmp2,
                    null(),
                    0,
                    0,
                );
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

        tab = xml_malloc((*ctxt).tab_size as usize * size_of::<*const XmlChar>())
            as *mut *const XmlChar;
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
                ret =
                    xml_exp_hash_get_entry(ctxt, XmlExpNodeType::XmlExpOr, ret, tmp3, null(), 0, 0);
                if ret.is_null() {
                    xml_free(tab as _);
                    return null_mut();
                }
            }
        }
        xml_free(tab as _);
        ret
    }
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
    unsafe {
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
}

/// Check whether @exp accepts all the languages accepted by @sub
/// the input being a subexpression.
///
/// Returns 1 if true 0 if false and -1 in case of failure.
#[doc(alias = "xmlExpSubsume")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_subsume(ctxt: XmlExpCtxtPtr, exp: XmlExpNodePtr, sub: XmlExpNodePtr) -> i32 {
    unsafe {
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
}

#[cfg(feature = "libxml_expr")]
unsafe fn xml_exp_dump_int<'a>(buf: &mut (impl Write + 'a), expr: XmlExpNodePtr, glob: i32) {
    unsafe {
        let mut c: XmlExpNodePtr;

        if expr.is_null() {
            return;
        }
        if glob != 0 {
            write!(buf, "(").ok();
        }
        if (*expr).typ == XmlExpNodeType::XmlExpEmpty as u8 {
            write!(buf, "empty").ok();
        } else if (*expr).typ == XmlExpNodeType::XmlExpForbid as u8 {
            write!(buf, "forbidden").ok();
        } else if (*expr).typ == XmlExpNodeType::XmlExpAtom as u8 {
            write!(
                buf,
                "{}",
                CStr::from_ptr((*expr).field.f_str as *const i8)
                    .to_string_lossy()
                    .as_ref()
            )
            .ok();
        } else if (*expr).typ == XmlExpNodeType::XmlExpSeq as u8 {
            c = (*expr).exp_left;
            if (*c).typ == XmlExpNodeType::XmlExpSeq as u8
                || (*c).typ == XmlExpNodeType::XmlExpOr as u8
            {
                xml_exp_dump_int(buf, c, 1);
            } else {
                xml_exp_dump_int(buf, c, 0);
            }
            write!(buf, " , ").ok();
            c = (*expr).field.children.f_right;
            if (*c).typ == XmlExpNodeType::XmlExpSeq as u8
                || (*c).typ == XmlExpNodeType::XmlExpOr as u8
            {
                xml_exp_dump_int(buf, c, 1);
            } else {
                xml_exp_dump_int(buf, c, 0);
            }
        } else if (*expr).typ == XmlExpNodeType::XmlExpOr as u8 {
            c = (*expr).exp_left;
            if (*c).typ == XmlExpNodeType::XmlExpSeq as u8
                || (*c).typ == XmlExpNodeType::XmlExpOr as u8
            {
                xml_exp_dump_int(buf, c, 1);
            } else {
                xml_exp_dump_int(buf, c, 0);
            }
            write!(buf, " | ").ok();
            c = (*expr).field.children.f_right;
            if (*c).typ == XmlExpNodeType::XmlExpSeq as u8
                || (*c).typ == XmlExpNodeType::XmlExpOr as u8
            {
                xml_exp_dump_int(buf, c, 1);
            } else {
                xml_exp_dump_int(buf, c, 0);
            }
        } else if (*expr).typ == XmlExpNodeType::XmlExpCount as u8 {
            let mut rep: [i8; 40] = [0; 40];

            c = (*expr).exp_left;
            if (*c).typ == XmlExpNodeType::XmlExpSeq as u8
                || (*c).typ == XmlExpNodeType::XmlExpOr as u8
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
            write!(buf, "{}", CStr::from_ptr(rep.as_ptr()).to_string_lossy()).ok();
        } else {
            eprintln!("Error in tree");
        }
        if glob != 0 {
            write!(buf, ")").ok();
        }
    }
}

/// Serialize the expression as compiled to the buffer
#[doc(alias = "xmlExpDump")]
#[cfg(feature = "libxml_expr")]
pub unsafe fn xml_exp_dump<'a>(buf: &mut (impl Write + 'a), expr: XmlExpNodePtr) {
    unsafe {
        if expr.is_null() {
            return;
        }
        xml_exp_dump_int(buf, expr, 0);
    }
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
}

use std::{
    borrow::Cow,
    ptr::{drop_in_place, null_mut},
};

use crate::{
    globals::{GLOBAL_STATE, GenericError, GenericErrorContext, StructuredError},
    libxml::{
        chvalid::xml_is_blank_char,
        globals::{xml_free, xml_malloc},
        relaxng::{
            XmlRelaxNGGrammarPtr, XmlRelaxNGPtr, XmlRelaxNGValidErr, XmlRelaxNGValidError,
            xml_relaxng_add_states_uniq, xml_relaxng_validate_progressive_callback,
        },
        xmlregexp::{
            XmlRegExecCtxtPtr, xml_reg_exec_push_string, xml_reg_exec_push_string2,
            xml_reg_free_exec_ctxt, xml_reg_new_exec_ctxt,
        },
    },
    relaxng::{VALID_ERR, VALID_ERR2},
    tree::{XmlAttrPtr, XmlDocPtr, XmlElementType, XmlGenericNodePtr, XmlNodePtr},
};

use super::{XmlRelaxNGDefinePtr, xml_rng_verr_memory};

const MAX_ATTR: usize = 20;

pub type XmlRelaxNGStatesPtr = *mut XmlRelaxNGStates;
/// A RelaxNGs container for validation state
#[doc(alias = "xmlRelaxNGStates")]
#[derive(Default)]
#[repr(C)]
pub struct XmlRelaxNGStates {
    // pub(crate) nb_state: i32,  // the number of states
    // pub(crate) max_state: i32, // the size of the array
    pub(crate) tab_state: Vec<XmlRelaxNGValidStatePtr>,
}

pub type XmlRelaxNGValidStatePtr = *mut XmlRelaxNGValidState;
/// A RelaxNGs validation state
// TODO: all fieleds are used in only relaxng module.
#[doc(alias = "xmlRelaxNGValidState")]
#[repr(C)]
pub struct XmlRelaxNGValidState {
    pub(crate) node: Option<XmlGenericNodePtr>, // the current node
    pub(crate) seq: Option<XmlGenericNodePtr>,  // the sequence of children left to validate
    pub(crate) nb_attr_left: i32,               // the number of attributes left to validate
    pub(crate) value: *mut u8,                  // the value when operating on string
    pub(crate) endvalue: *mut u8,               // the end value when operating on string
    pub(crate) attrs: Vec<Option<XmlAttrPtr>>,  // the array of attributes
}

impl Default for XmlRelaxNGValidState {
    fn default() -> Self {
        Self {
            node: None,
            seq: None,
            nb_attr_left: 0,
            value: null_mut(),
            endvalue: null_mut(),
            attrs: vec![],
        }
    }
}

pub type XmlRelaxNGValidCtxtPtr = *mut XmlRelaxNGValidCtxt;
/// A RelaxNGs validation context
// TODO: all fieleds are used in only relaxng module.
#[doc(alias = "xmlRelaxNGValidCtxt")]
#[repr(C)]
pub struct XmlRelaxNGValidCtxt {
    pub(crate) user_data: Option<GenericErrorContext>, // user specific data block
    pub(crate) error: Option<GenericError>,            // the callback in case of errors
    pub(crate) warning: Option<GenericError>,          // the callback in case of warning
    pub(crate) serror: Option<StructuredError>,
    pub(crate) nb_errors: i32, // number of errors in validation

    pub(crate) schema: XmlRelaxNGPtr,  // The schema in use
    pub(crate) doc: Option<XmlDocPtr>, // the document being validated
    pub(crate) flags: i32,             // validation flags
    pub(crate) depth: i32,             // validation depth
    pub(crate) idref: i32,             // requires idref checking
    pub(crate) err_no: i32,            // the first error found

    // Errors accumulated in branches may have to be stacked to be
    // provided back when it's sure they affect validation.
    // pub(crate) err: XmlRelaxNGValidErrorPtr,     // Last error
    // pub(crate) err_nr: i32,                      // Depth of the error stack
    // pub(crate) err_max: i32,                     // Max depth of the error stack
    pub(crate) err_tab: Vec<XmlRelaxNGValidError>, // stack of errors

    pub(crate) state: XmlRelaxNGValidStatePtr, // the current validation state
    pub(crate) states: XmlRelaxNGStatesPtr,    // the accumulated state list

    pub(crate) free_state: XmlRelaxNGStatesPtr, // the pool of free valid states
    free_states: Vec<XmlRelaxNGStatesPtr>,      // the pool of free state groups

    // This is used for "progressive" validation
    pub(crate) elem: XmlRegExecCtxtPtr, // the current element regexp
    pub(crate) elem_tab: Vec<XmlRegExecCtxtPtr>, // the stack of regexp runtime
    pub(crate) pstate: i32,             // progressive state
    pub(crate) pnode: Option<XmlNodePtr>, // the current node
    pub(crate) pdef: XmlRelaxNGDefinePtr, // the non-streamable definition
    pub(crate) perr: i32,               // signal error in content model outside the regexp
}

impl XmlRelaxNGValidCtxt {
    /// Push a new regexp for the current node content model on the stack
    ///
    /// Returns 0 in case of success and -1 in case of error.
    #[doc(alias = "xmlRelaxNGElemPush")]
    pub(crate) fn elem_push(&mut self, exec: XmlRegExecCtxtPtr) -> i32 {
        self.elem_tab.push(exec);
        self.elem = exec;
        0
    }

    /// Pop the regexp of the current node content model from the stack
    ///
    /// Returns the exec or NULL if empty
    #[doc(alias = "xmlRelaxNGElemPop")]
    pub(crate) fn elem_pop(&mut self) -> XmlRegExecCtxtPtr {
        let Some(res) = self.elem_tab.pop() else {
            return null_mut();
        };
        self.elem = self.elem_tab.last().cloned().unwrap_or(null_mut());
        res
    }

    /// Push a new element start on the RelaxNG validation stack.
    ///
    /// returns 1 if no validation problem was found or 0 if validating the
    /// element requires a full node, and -1 in case of error.
    #[doc(alias = "xmlRelaxNGValidatePushElement")]
    pub unsafe fn push_element(&mut self, _doc: Option<XmlDocPtr>, elem: XmlNodePtr) -> i32 {
        unsafe {
            let mut ret: i32;

            if self.elem.is_null() {
                let schema: XmlRelaxNGPtr = self.schema;
                if schema.is_null() {
                    VALID_ERR!(self, XmlRelaxNGValidErr::XmlRelaxngErrNogrammar);
                    return -1;
                }
                let grammar: XmlRelaxNGGrammarPtr = (*schema).topgrammar;
                if grammar.is_null() || (*grammar).start.is_null() {
                    VALID_ERR!(self, XmlRelaxNGValidErr::XmlRelaxngErrNogrammar);
                    return -1;
                }
                let define: XmlRelaxNGDefinePtr = (*grammar).start;
                let Some(cont_model) = (*define).cont_model.clone() else {
                    self.pdef = define;
                    return 0;
                };
                let exec: XmlRegExecCtxtPtr = xml_reg_new_exec_ctxt(
                    cont_model,
                    Some(xml_relaxng_validate_progressive_callback),
                    self as *mut Self as _,
                );
                if exec.is_null() {
                    return -1;
                }
                self.elem_push(exec);
            }
            self.pnode = Some(elem);
            self.pstate = 0;
            if let Some(ns) = elem.ns {
                ret = xml_reg_exec_push_string2(
                    self.elem,
                    elem.name,
                    ns.href,
                    self as *mut Self as _,
                );
            } else {
                ret = xml_reg_exec_push_string(self.elem, elem.name, self as *mut Self as _);
            }
            if ret < 0 {
                VALID_ERR2!(self, XmlRelaxNGValidErr::XmlRelaxngErrElemwrong, elem.name);
            } else if self.pstate == 0 {
                ret = 0;
            } else if self.pstate < 0 {
                ret = -1;
            } else {
                ret = 1;
            }
            ret
        }
    }

    /// Pop the element end from the RelaxNG validation stack.
    ///
    /// returns 1 if no validation problem was found or 0 otherwise
    #[doc(alias = "xmlRelaxNGValidatePopElement")]
    pub unsafe fn pop_element(&mut self, _doc: Option<XmlDocPtr>, _elem: XmlNodePtr) -> i32 {
        unsafe {
            let mut ret: i32;

            if self.elem.is_null() {
                return -1;
            }
            // verify that we reached a terminal state of the content model.
            let exec: XmlRegExecCtxtPtr = self.elem_pop();
            ret = xml_reg_exec_push_string(exec, null_mut(), null_mut());
            match ret.cmp(&0) {
                std::cmp::Ordering::Equal => {
                    // TODO: get some of the names needed to exit the current state of exec
                    VALID_ERR2!(
                        self,
                        XmlRelaxNGValidErr::XmlRelaxngErrNoelem,
                        c"".as_ptr() as _
                    );
                    ret = -1;
                }
                std::cmp::Ordering::Less => {
                    ret = -1;
                }
                std::cmp::Ordering::Greater => {
                    ret = 1;
                }
            }
            xml_reg_free_exec_ctxt(exec);
            ret
        }
    }
}

impl Default for XmlRelaxNGValidCtxt {
    fn default() -> Self {
        Self {
            user_data: None,
            error: None,
            warning: None,
            serror: None,
            nb_errors: 0,
            schema: null_mut(),
            doc: None,
            flags: 0,
            depth: 0,
            idref: 0,
            err_no: 0,
            err_tab: vec![],
            state: null_mut(),
            states: null_mut(),
            free_state: null_mut(),
            free_states: vec![],
            elem: null_mut(),
            elem_tab: vec![],
            pstate: 0,
            pnode: None,
            pdef: null_mut(),
            perr: 0,
        }
    }
}

/// Create an XML RelaxNGs validation context based on the given schema
///
/// Returns the validation context or NULL in case of error
#[doc(alias = "xmlRelaxNGNewValidCtxt")]
pub unsafe fn xml_relaxng_new_valid_ctxt(schema: XmlRelaxNGPtr) -> XmlRelaxNGValidCtxtPtr {
    unsafe {
        let ret: XmlRelaxNGValidCtxtPtr = xml_malloc(size_of::<XmlRelaxNGValidCtxt>()) as _;
        if ret.is_null() {
            xml_rng_verr_memory(null_mut(), "building context\n");
            return null_mut();
        }
        std::ptr::write(&mut *ret, XmlRelaxNGValidCtxt::default());
        (*ret).schema = schema;
        GLOBAL_STATE.with_borrow(|state| {
            (*ret).error = Some(state.generic_error);
            (*ret).user_data = state.generic_error_context.clone();
        });
        if !schema.is_null() {
            (*ret).idref = (*schema).idref;
        }
        (*ret).states = null_mut();
        (*ret).free_state = null_mut();
        (*ret).err_no = XmlRelaxNGValidErr::XmlRelaxngOk as i32;
        ret
    }
}

/// Free the resources associated to the schema validation context
#[doc(alias = "xmlRelaxNGFreeValidCtxt")]
pub unsafe fn xml_relaxng_free_valid_ctxt(ctxt: XmlRelaxNGValidCtxtPtr) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        if !(*ctxt).states.is_null() {
            xml_relaxng_free_states(null_mut(), (*ctxt).states);
        }
        if !(*ctxt).free_state.is_null() {
            for state in (*(*ctxt).free_state).tab_state.drain(..) {
                xml_relaxng_free_valid_state(null_mut(), state);
            }
            xml_relaxng_free_states(null_mut(), (*ctxt).free_state);
        }
        for state in (*ctxt).free_states.drain(..) {
            xml_relaxng_free_states(null_mut(), state);
        }
        let mut exec = (*ctxt).elem_pop();
        while !exec.is_null() {
            xml_reg_free_exec_ctxt(exec);
            exec = (*ctxt).elem_pop();
        }
        drop_in_place(ctxt);
        xml_free(ctxt as _);
    }
}

/// Allocate a new RelaxNG validation state container
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlRelaxNGNewStates")]
pub(crate) unsafe fn xml_relaxng_new_states(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut size: i32,
) -> XmlRelaxNGStatesPtr {
    unsafe {
        if !ctxt.is_null() {
            if let Some(ret) = (*ctxt).free_states.pop() {
                (*ret).tab_state.clear();
                return ret;
            }
        }
        if size < 16 {
            size = 16;
        }

        let ret: XmlRelaxNGStatesPtr = xml_malloc(
            size_of::<XmlRelaxNGStates>()
                + (size as usize - 1) * size_of::<XmlRelaxNGValidStatePtr>(),
        ) as _;
        if ret.is_null() {
            xml_rng_verr_memory(ctxt, "allocating states\n");
            return null_mut();
        }
        std::ptr::write(&mut *ret, XmlRelaxNGStates::default());
        (*ret).tab_state.reserve(size as usize);
        ret
    }
}

/// Free a RelaxNG validation state container
#[doc(alias = "xmlRelaxNGFreeStates")]
pub(crate) unsafe fn xml_relaxng_free_states(
    ctxt: XmlRelaxNGValidCtxtPtr,
    states: XmlRelaxNGStatesPtr,
) {
    unsafe {
        if states.is_null() {
            return;
        }

        if ctxt.is_null() {
            drop_in_place(states);
            xml_free(states as _);
        } else {
            (*ctxt).free_states.push(states);
        }
    }
}

/// Allocate a new RelaxNG validation state
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlRelaxNGNewValidState")]
pub(crate) unsafe fn xml_relaxng_new_valid_state(
    ctxt: XmlRelaxNGValidCtxtPtr,
    node: Option<XmlGenericNodePtr>,
) -> XmlRelaxNGValidStatePtr {
    unsafe {
        let ret: XmlRelaxNGValidStatePtr;
        let mut attrs: [Option<XmlAttrPtr>; MAX_ATTR] = [None; MAX_ATTR];
        let mut nb_attrs: usize = 0;
        let mut root = None;

        if let Some(node) = node {
            if node.element_type() != XmlElementType::XmlDocumentNode {
                // In original libxml2, `node` is treats as truly `XmlNode`,
                // but it may actually be `XmlDoc`.
                // If the `node` is `XmlDoc`,
                // this may be a misbehavior because it erroneously collects an external subset.
                // Therefore, insert a check to see if the `node` is an `XmlNode`.

                let node = XmlNodePtr::try_from(node).unwrap();
                let mut attr = node.properties;
                while let Some(now) = attr {
                    if nb_attrs < MAX_ATTR {
                        attrs[nb_attrs] = Some(now);
                        nb_attrs += 1;
                    } else {
                        nb_attrs += 1;
                    }
                    attr = now.next;
                }
            }
        } else {
            root = (*ctxt).doc.and_then(|doc| doc.get_root_element());
            if root.is_none() {
                return null_mut();
            }
        }
        if !(*ctxt).free_state.is_null() && !(*(*ctxt).free_state).tab_state.is_empty() {
            ret = (*(*ctxt).free_state).tab_state.pop().unwrap();
        } else {
            ret = xml_malloc(size_of::<XmlRelaxNGValidState>()) as _;
            if ret.is_null() {
                xml_rng_verr_memory(ctxt, "allocating states\n");
                return null_mut();
            }
            std::ptr::write(&mut *ret, XmlRelaxNGValidState::default());
        }
        (*ret).value = null_mut();
        (*ret).endvalue = null_mut();
        if let Some(node) = node {
            (*ret).node = Some(node);
            (*ret).seq = node.children();
        } else {
            (*ret).node = (*ctxt).doc.map(|doc| doc.into());
            (*ret).seq = root.map(|root| root.into());
        }
        (*ret).attrs.clear();
        if nb_attrs > 0 {
            if nb_attrs < MAX_ATTR {
                (*ret).attrs.extend(attrs.iter().copied().take(nb_attrs));
            } else {
                let node = XmlNodePtr::try_from(node.unwrap()).unwrap();
                let mut attr = node.properties;
                while let Some(now) = attr {
                    (*ret).attrs.push(Some(now));
                    attr = now.next;
                }
            }
        }
        (*ret).nb_attr_left = (*ret).attrs.len() as i32;
        ret
    }
}

/// Deallocate a RelaxNG validation state structure.
#[doc(alias = "xmlRelaxNGFreeValidState")]
pub(crate) unsafe fn xml_relaxng_free_valid_state(
    ctxt: XmlRelaxNGValidCtxtPtr,
    state: XmlRelaxNGValidStatePtr,
) {
    unsafe {
        if state.is_null() {
            return;
        }

        if !ctxt.is_null() && (*ctxt).free_state.is_null() {
            (*ctxt).free_state = xml_relaxng_new_states(ctxt, 40);
        }
        if ctxt.is_null() || (*ctxt).free_state.is_null() {
            drop_in_place(state);
            xml_free(state as _);
        } else {
            xml_relaxng_add_states_uniq(ctxt, (*ctxt).free_state, state);
        }
    }
}

/// Implements the  normalizeWhiteSpace( s ) function from section 6.2.9 of the spec
///
/// If `s` is not modified or only its head or tail is trimed, return `s` wrapped `Cow::Borrowed`.  
/// Otherwise, return normalized string wrapped `Cow::Owned`.
#[doc(alias = "xmlRelaxNGNormalize")]
pub(crate) fn relaxng_normalize(mut s: &str) -> Cow<'_, str> {
    s = s.trim_start_matches(|c| xml_is_blank_char(c as u32));
    s = s.trim_end_matches(|c| xml_is_blank_char(c as u32));
    let mut chars = s.chars().peekable();
    let mut pending = 0;
    let mut buf = None::<String>;
    while let Some(c) = chars.next() {
        if xml_is_blank_char(c as u32) {
            if chars.next_if(|c| xml_is_blank_char(*c as u32)).is_some() {
                while chars.next_if(|c| xml_is_blank_char(*c as u32)).is_some() {}
                let buf = buf.get_or_insert_with(String::new);
                if pending > 0 {
                    buf.push_str(&s[..pending]);
                    pending = 0;
                }
                buf.push(' ');
            } else if let Some(buf) = buf.as_mut() {
                buf.push(c);
            } else {
                pending += c.len_utf8();
            }
        } else if let Some(buf) = buf.as_mut() {
            buf.push(c);
        } else {
            pending += c.len_utf8();
        }
    }

    buf.map(Cow::Owned).unwrap_or(Cow::Borrowed(s))
}

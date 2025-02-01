use std::{
    borrow::Cow,
    ptr::{drop_in_place, null_mut},
};

use libc::memset;

use crate::{
    globals::{GenericError, GenericErrorContext, StructuredError, GLOBAL_STATE},
    libxml::{
        chvalid::xml_is_blank_char,
        globals::{xml_free, xml_malloc, xml_realloc},
        relaxng::{
            xml_relaxng_add_states_uniq, xml_relaxng_validate_progressive_callback,
            XmlRelaxNGGrammarPtr, XmlRelaxNGPtr, XmlRelaxNGValidErr, XmlRelaxNGValidErrorPtr,
        },
        xmlregexp::{
            xml_reg_exec_push_string, xml_reg_exec_push_string2, xml_reg_free_exec_ctxt,
            xml_reg_new_exec_ctxt, XmlRegExecCtxtPtr,
        },
    },
    relaxng::{VALID_ERR, VALID_ERR2},
    tree::{NodeCommon, XmlAttrPtr, XmlDoc, XmlElementType, XmlNode},
};

use super::{xml_rng_verr_memory, XmlRelaxNGDefinePtr};

const MAX_ATTR: usize = 20;

pub type XmlRelaxNGStatesPtr = *mut XmlRelaxNGStates;
/// A RelaxNGs container for validation state
#[doc(alias = "xmlRelaxNGStates")]
#[repr(C)]
pub struct XmlRelaxNGStates {
    pub(crate) nb_state: i32,  // the number of states
    pub(crate) max_state: i32, // the size of the array
    pub(crate) tab_state: *mut XmlRelaxNGValidStatePtr,
}

pub type XmlRelaxNGValidStatePtr = *mut XmlRelaxNGValidState;
/// A RelaxNGs validation state
// TODO: all fieleds are used in only relaxng module.
#[doc(alias = "xmlRelaxNGValidState")]
#[repr(C)]
pub struct XmlRelaxNGValidState {
    pub(crate) node: *mut XmlNode, // the current node
    pub(crate) seq: *mut XmlNode,  // the sequence of children left to validate
    // pub(crate) nb_attrs: i32,            // the number of attributes
    // pub(crate) max_attrs: i32,           // the size of attrs
    pub(crate) nb_attr_left: i32, // the number of attributes left to validate
    pub(crate) value: *mut u8,    // the value when operating on string
    pub(crate) endvalue: *mut u8, // the end value when operating on string
    pub(crate) attrs: Vec<Option<XmlAttrPtr>>, // the array of attributes
}

impl Default for XmlRelaxNGValidState {
    fn default() -> Self {
        Self {
            node: null_mut(),
            seq: null_mut(),
            // nb_attrs: 0,
            // max_attrs: 0,
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

    pub(crate) schema: XmlRelaxNGPtr, // The schema in use
    pub(crate) doc: *mut XmlDoc,      // the document being validated
    pub(crate) flags: i32,            // validation flags
    pub(crate) depth: i32,            // validation depth
    pub(crate) idref: i32,            // requires idref checking
    pub(crate) err_no: i32,           // the first error found

    // Errors accumulated in branches may have to be stacked to be
    // provided back when it's sure they affect validation.
    pub(crate) err: XmlRelaxNGValidErrorPtr,     // Last error
    pub(crate) err_nr: i32,                      // Depth of the error stack
    pub(crate) err_max: i32,                     // Max depth of the error stack
    pub(crate) err_tab: XmlRelaxNGValidErrorPtr, // stack of errors

    pub(crate) state: XmlRelaxNGValidStatePtr, // the current validation state
    pub(crate) states: XmlRelaxNGStatesPtr,    // the accumulated state list

    pub(crate) free_state: XmlRelaxNGStatesPtr, // the pool of free valid states
    free_states_nr: i32,
    free_states_max: i32,
    free_states: *mut XmlRelaxNGStatesPtr, // the pool of free state groups

    // This is used for "progressive" validation
    pub(crate) elem: XmlRegExecCtxtPtr, // the current element regexp
    pub(crate) elem_nr: i32,            // the number of element validated
    pub(crate) elem_max: i32,           // the max depth of elements
    pub(crate) elem_tab: *mut XmlRegExecCtxtPtr, // the stack of regexp runtime
    pub(crate) pstate: i32,             // progressive state
    pub(crate) pnode: *mut XmlNode,     // the current node
    pub(crate) pdef: XmlRelaxNGDefinePtr, // the non-streamable definition
    pub(crate) perr: i32,               // signal error in content model outside the regexp
}

impl XmlRelaxNGValidCtxt {
    /// Push a new regexp for the current node content model on the stack
    ///
    /// Returns 0 in case of success and -1 in case of error.
    #[doc(alias = "xmlRelaxNGElemPush")]
    pub(crate) unsafe fn elem_push(&mut self, exec: XmlRegExecCtxtPtr) -> i32 {
        if self.elem_tab.is_null() {
            self.elem_max = 10;
            self.elem_tab =
                xml_malloc(self.elem_max as usize * size_of::<XmlRegExecCtxtPtr>()) as _;
            if self.elem_tab.is_null() {
                xml_rng_verr_memory(self, "validating\n");
                return -1;
            }
        }
        if self.elem_nr >= self.elem_max {
            self.elem_max *= 2;
            self.elem_tab = xml_realloc(
                self.elem_tab as _,
                self.elem_max as usize * size_of::<XmlRegExecCtxtPtr>(),
            ) as _;
            if self.elem_tab.is_null() {
                xml_rng_verr_memory(self, "validating\n");
                return -1;
            }
        }
        *self.elem_tab.add(self.elem_nr as usize) = exec;
        self.elem_nr += 1;
        self.elem = exec;
        0
    }

    /// Pop the regexp of the current node content model from the stack
    ///
    /// Returns the exec or NULL if empty
    #[doc(alias = "xmlRelaxNGElemPop")]
    pub(crate) unsafe fn elem_pop(&mut self) -> XmlRegExecCtxtPtr {
        if self.elem_nr <= 0 {
            return null_mut();
        }
        self.elem_nr -= 1;
        let ret: XmlRegExecCtxtPtr = *self.elem_tab.add(self.elem_nr as usize);
        *self.elem_tab.add(self.elem_nr as usize) = null_mut();
        if self.elem_nr > 0 {
            self.elem = *self.elem_tab.add(self.elem_nr as usize - 1);
        } else {
            self.elem = null_mut();
        }
        ret
    }

    /// Push a new element start on the RelaxNG validation stack.
    ///
    /// returns 1 if no validation problem was found or 0 if validating the
    /// element requires a full node, and -1 in case of error.
    #[doc(alias = "xmlRelaxNGValidatePushElement")]
    pub unsafe fn push_element(&mut self, _doc: *mut XmlDoc, elem: *mut XmlNode) -> i32 {
        let mut ret: i32;

        if elem.is_null() {
            return -1;
        }

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
            if (*define).cont_model.is_null() {
                self.pdef = define;
                return 0;
            }
            let exec: XmlRegExecCtxtPtr = xml_reg_new_exec_ctxt(
                (*define).cont_model,
                Some(xml_relaxng_validate_progressive_callback),
                self as *mut Self as _,
            );
            if exec.is_null() {
                return -1;
            }
            self.elem_push(exec);
        }
        self.pnode = elem;
        self.pstate = 0;
        if let Some(ns) = (*elem).ns {
            ret =
                xml_reg_exec_push_string2(self.elem, (*elem).name, ns.href, self as *mut Self as _);
        } else {
            ret = xml_reg_exec_push_string(self.elem, (*elem).name, self as *mut Self as _);
        }
        if ret < 0 {
            VALID_ERR2!(
                self,
                XmlRelaxNGValidErr::XmlRelaxngErrElemwrong,
                (*elem).name
            );
        } else if self.pstate == 0 {
            ret = 0;
        } else if self.pstate < 0 {
            ret = -1;
        } else {
            ret = 1;
        }
        ret
    }

    /// Pop the element end from the RelaxNG validation stack.
    ///
    /// returns 1 if no validation problem was found or 0 otherwise
    #[doc(alias = "xmlRelaxNGValidatePopElement")]
    pub unsafe fn pop_element(&mut self, _doc: *mut XmlDoc, elem: *mut XmlNode) -> i32 {
        let mut ret: i32;

        if self.elem.is_null() || elem.is_null() {
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

impl Default for XmlRelaxNGValidCtxt {
    fn default() -> Self {
        Self {
            user_data: None,
            error: None,
            warning: None,
            serror: None,
            nb_errors: 0,
            schema: null_mut(),
            doc: null_mut(),
            flags: 0,
            depth: 0,
            idref: 0,
            err_no: 0,
            err: null_mut(),
            err_nr: 0,
            err_max: 0,
            err_tab: null_mut(),
            state: null_mut(),
            states: null_mut(),
            free_state: null_mut(),
            free_states_nr: 0,
            free_states_max: 0,
            free_states: null_mut(),
            elem: null_mut(),
            elem_nr: 0,
            elem_max: 0,
            elem_tab: null_mut(),
            pstate: 0,
            pnode: null_mut(),
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
    let ret: XmlRelaxNGValidCtxtPtr = xml_malloc(size_of::<XmlRelaxNGValidCtxt>()) as _;
    if ret.is_null() {
        xml_rng_verr_memory(null_mut(), "building context\n");
        return null_mut();
    }
    memset(ret as _, 0, size_of::<XmlRelaxNGValidCtxt>());
    (*ret).schema = schema;
    GLOBAL_STATE.with_borrow(|state| {
        (*ret).error = Some(state.generic_error);
        (*ret).user_data = state.generic_error_context.clone();
    });
    (*ret).err_nr = 0;
    (*ret).err_max = 0;
    (*ret).err = null_mut();
    (*ret).err_tab = null_mut();
    if !schema.is_null() {
        (*ret).idref = (*schema).idref;
    }
    (*ret).states = null_mut();
    (*ret).free_state = null_mut();
    (*ret).free_states = null_mut();
    (*ret).err_no = XmlRelaxNGValidErr::XmlRelaxngOk as i32;
    ret
}

/// Free the resources associated to the schema validation context
#[doc(alias = "xmlRelaxNGFreeValidCtxt")]
pub unsafe fn xml_relaxng_free_valid_ctxt(ctxt: XmlRelaxNGValidCtxtPtr) {
    if ctxt.is_null() {
        return;
    }
    if !(*ctxt).states.is_null() {
        xml_relaxng_free_states(null_mut(), (*ctxt).states);
    }
    if !(*ctxt).free_state.is_null() {
        for k in 0..(*(*ctxt).free_state).nb_state {
            xml_relaxng_free_valid_state(
                null_mut(),
                *(*(*ctxt).free_state).tab_state.add(k as usize),
            );
        }
        xml_relaxng_free_states(null_mut(), (*ctxt).free_state);
    }
    if !(*ctxt).free_states.is_null() {
        for k in 0..(*ctxt).free_states_nr {
            xml_relaxng_free_states(null_mut(), *(*ctxt).free_states.add(k as usize));
        }
        xml_free((*ctxt).free_states as _);
    }
    if !(*ctxt).err_tab.is_null() {
        xml_free((*ctxt).err_tab as _);
    }
    if !(*ctxt).elem_tab.is_null() {
        let mut exec: XmlRegExecCtxtPtr;

        exec = (*ctxt).elem_pop();
        while !exec.is_null() {
            xml_reg_free_exec_ctxt(exec);
            exec = (*ctxt).elem_pop();
        }
        xml_free((*ctxt).elem_tab as _);
    }
    xml_free(ctxt as _);
}

/// Allocate a new RelaxNG validation state container
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlRelaxNGNewStates")]
pub(crate) unsafe fn xml_relaxng_new_states(
    ctxt: XmlRelaxNGValidCtxtPtr,
    mut size: i32,
) -> XmlRelaxNGStatesPtr {
    let ret: XmlRelaxNGStatesPtr;

    if !ctxt.is_null() && !(*ctxt).free_states.is_null() && (*ctxt).free_states_nr > 0 {
        (*ctxt).free_states_nr -= 1;
        ret = *(*ctxt).free_states.add((*ctxt).free_states_nr as usize);
        (*ret).nb_state = 0;
        return ret;
    }
    if size < 16 {
        size = 16;
    }

    ret = xml_malloc(
        size_of::<XmlRelaxNGStates>() + (size as usize - 1) * size_of::<XmlRelaxNGValidStatePtr>(),
    ) as _;
    if ret.is_null() {
        xml_rng_verr_memory(ctxt, "allocating states\n");
        return null_mut();
    }
    (*ret).nb_state = 0;
    (*ret).max_state = size;
    (*ret).tab_state = xml_malloc(size as usize * size_of::<XmlRelaxNGValidStatePtr>()) as _;
    if (*ret).tab_state.is_null() {
        xml_rng_verr_memory(ctxt, "allocating states\n");
        xml_free(ret as _);
        return null_mut();
    }
    ret
}

/// Free a RelaxNG validation state container
#[doc(alias = "xmlRelaxNGFreeStates")]
pub(crate) unsafe fn xml_relaxng_free_states(
    ctxt: XmlRelaxNGValidCtxtPtr,
    states: XmlRelaxNGStatesPtr,
) {
    if states.is_null() {
        return;
    }
    if !ctxt.is_null() && (*ctxt).free_states.is_null() {
        (*ctxt).free_states_max = 40;
        (*ctxt).free_states_nr = 0;
        (*ctxt).free_states =
            xml_malloc((*ctxt).free_states_max as usize * size_of::<XmlRelaxNGStatesPtr>()) as _;
        if (*ctxt).free_states.is_null() {
            xml_rng_verr_memory(ctxt, "storing states\n");
        }
    } else if !ctxt.is_null() && (*ctxt).free_states_nr >= (*ctxt).free_states_max {
        let tmp: *mut XmlRelaxNGStatesPtr = xml_realloc(
            (*ctxt).free_states as _,
            2 * (*ctxt).free_states_max as usize * size_of::<XmlRelaxNGStatesPtr>(),
        ) as _;
        if tmp.is_null() {
            xml_rng_verr_memory(ctxt, "storing states\n");
            xml_free((*states).tab_state as _);
            xml_free(states as _);
            return;
        }
        (*ctxt).free_states = tmp;
        (*ctxt).free_states_max *= 2;
    }
    if ctxt.is_null() || (*ctxt).free_states.is_null() {
        xml_free((*states).tab_state as _);
        xml_free(states as _);
    } else {
        *(*ctxt).free_states.add((*ctxt).free_states_nr as usize) = states;
        (*ctxt).free_states_nr += 1;
    }
}

/// Allocate a new RelaxNG validation state
///
/// Returns the newly allocated structure or NULL in case or error
#[doc(alias = "xmlRelaxNGNewValidState")]
pub(crate) unsafe fn xml_relaxng_new_valid_state(
    ctxt: XmlRelaxNGValidCtxtPtr,
    node: *mut XmlNode,
) -> XmlRelaxNGValidStatePtr {
    let ret: XmlRelaxNGValidStatePtr;
    let mut attrs: [Option<XmlAttrPtr>; MAX_ATTR] = [None; MAX_ATTR];
    let mut nb_attrs: usize = 0;
    let mut root: *mut XmlNode = null_mut();

    if node.is_null() {
        root = if (*ctxt).doc.is_null() {
            null_mut()
        } else {
            (*(*ctxt).doc).get_root_element()
        };
        if root.is_null() {
            return null_mut();
        }
    } else if (*node).element_type() != XmlElementType::XmlDocumentNode {
        // In original libxml2, `node` is treats as truly `XmlNode`,
        // but it may actually be `XmlDoc`.
        // If the `node` is `XmlDoc`,
        // this may be a misbehavior because it erroneously collects an external subset.
        // Therefore, insert a check to see if the `node` is an `XmlNode`.

        let mut attr = (*node).properties;
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
    if !(*ctxt).free_state.is_null() && (*(*ctxt).free_state).nb_state > 0 {
        (*(*ctxt).free_state).nb_state -= 1;
        ret = *(*(*ctxt).free_state)
            .tab_state
            .add((*(*ctxt).free_state).nb_state as usize);
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
    if node.is_null() {
        (*ret).node = (*ctxt).doc as _;
        (*ret).seq = root;
    } else {
        (*ret).node = node;
        (*ret).seq = (*node).children().map_or(null_mut(), |c| c.as_ptr());
    }
    (*ret).attrs.clear();
    if nb_attrs > 0 {
        if nb_attrs < MAX_ATTR {
            (*ret).attrs.extend(attrs.iter().copied().take(nb_attrs));
        } else {
            let mut attr = (*node).properties;
            while let Some(now) = attr {
                (*ret).attrs.push(Some(now));
                attr = now.next;
            }
        }
    }
    (*ret).nb_attr_left = (*ret).attrs.len() as i32;
    ret
}

/// Deallocate a RelaxNG validation state structure.
#[doc(alias = "xmlRelaxNGFreeValidState")]
pub(crate) unsafe fn xml_relaxng_free_valid_state(
    ctxt: XmlRelaxNGValidCtxtPtr,
    state: XmlRelaxNGValidStatePtr,
) {
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

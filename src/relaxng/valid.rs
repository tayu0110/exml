use std::ptr::null_mut;

use libc::{memcpy, memset};

use crate::{
    globals::{GenericError, GenericErrorContext, StructuredError, GLOBAL_STATE},
    libxml::{
        globals::{xml_free, xml_malloc, xml_realloc},
        relaxng::{
            xml_relaxng_add_states_uniq, xml_relaxng_elem_pop, XmlRelaxNGPtr, XmlRelaxNGValidErr,
            XmlRelaxNGValidErrorPtr,
        },
        xmlregexp::{xml_reg_free_exec_ctxt, XmlRegExecCtxtPtr},
    },
    tree::{NodeCommon, XmlAttrPtr, XmlDocPtr, XmlNodePtr},
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
    pub(crate) node: XmlNodePtr,       // the current node
    pub(crate) seq: XmlNodePtr,        // the sequence of children left to validate
    pub(crate) nb_attrs: i32,          // the number of attributes
    pub(crate) max_attrs: i32,         // the size of attrs
    pub(crate) nb_attr_left: i32,      // the number of attributes left to validate
    pub(crate) value: *mut u8,         // the value when operating on string
    pub(crate) endvalue: *mut u8,      // the end value when operating on string
    pub(crate) attrs: *mut XmlAttrPtr, // the array of attributes
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
    pub(crate) doc: XmlDocPtr,        // the document being validated
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
    pub(crate) pnode: XmlNodePtr,       // the current node
    pub(crate) pdef: XmlRelaxNGDefinePtr, // the non-streamable definition
    pub(crate) perr: i32,               // signal error in content model outside the regexp
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

        exec = xml_relaxng_elem_pop(ctxt);
        while !exec.is_null() {
            xml_reg_free_exec_ctxt(exec);
            exec = xml_relaxng_elem_pop(ctxt);
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
    node: XmlNodePtr,
) -> XmlRelaxNGValidStatePtr {
    let ret: XmlRelaxNGValidStatePtr;
    let mut attr: XmlAttrPtr;
    let mut attrs: [XmlAttrPtr; MAX_ATTR] = [null_mut(); MAX_ATTR];
    let mut nb_attrs: usize = 0;
    let mut root: XmlNodePtr = null_mut();

    if node.is_null() {
        root = if (*ctxt).doc.is_null() {
            null_mut()
        } else {
            (*(*ctxt).doc).get_root_element()
        };
        if root.is_null() {
            return null_mut();
        }
    } else {
        attr = (*node).properties;
        while !attr.is_null() {
            if nb_attrs < MAX_ATTR {
                attrs[nb_attrs] = attr;
                nb_attrs += 1;
            } else {
                nb_attrs += 1;
            }
            attr = (*attr).next;
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
        memset(ret as _, 0, size_of::<XmlRelaxNGValidState>());
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
    (*ret).nb_attrs = 0;
    if nb_attrs > 0 {
        if (*ret).attrs.is_null() {
            if nb_attrs < 4 {
                (*ret).max_attrs = 4;
            } else {
                (*ret).max_attrs = nb_attrs as _;
            }
            (*ret).attrs = xml_malloc((*ret).max_attrs as usize * size_of::<XmlAttrPtr>()) as _;
            if (*ret).attrs.is_null() {
                xml_rng_verr_memory(ctxt, "allocating states\n");
                return ret;
            }
        } else if (*ret).max_attrs < nb_attrs as i32 {
            let tmp: *mut XmlAttrPtr =
                xml_realloc((*ret).attrs as _, nb_attrs * size_of::<XmlAttrPtr>()) as _;
            if tmp.is_null() {
                xml_rng_verr_memory(ctxt, "allocating states\n");
                return ret;
            }
            (*ret).attrs = tmp;
            (*ret).max_attrs = nb_attrs as _;
        }
        (*ret).nb_attrs = nb_attrs as _;
        if nb_attrs < MAX_ATTR {
            memcpy(
                (*ret).attrs as _,
                attrs.as_ptr() as _,
                size_of::<XmlAttrPtr>() * nb_attrs,
            );
        } else {
            attr = (*node).properties;
            nb_attrs = 0;
            while !attr.is_null() {
                *(*ret).attrs.add(nb_attrs) = attr;
                nb_attrs += 1;
                attr = (*attr).next;
            }
        }
    }
    (*ret).nb_attr_left = (*ret).nb_attrs;
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
        if !(*state).attrs.is_null() {
            xml_free((*state).attrs as _);
        }
        xml_free(state as _);
    } else {
        xml_relaxng_add_states_uniq(ctxt, (*ctxt).free_state, state);
    }
}

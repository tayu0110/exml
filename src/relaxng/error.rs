use std::{borrow::Cow, ffi::CStr, ptr::null_mut};

use libc::snprintf;

use crate::{
    error::{XmlParserErrors, __xml_raise_error},
    globals::{GenericError, StructuredError},
    libxml::{
        globals::xml_free,
        relaxng::{
            XmlRelaxNGValidErr, XmlRelaxNGValidError, FLAGS_IGNORABLE, FLAGS_NEGATIVE,
            FLAGS_NOERROR,
        },
        xmlstring::{xml_char_strdup, xml_escape_format_string, xml_str_equal, xml_strdup},
    },
    tree::XmlNode,
};

const ERROR_IS_DUP: i32 = 1;

/// Computes a formatted error string for the given error code and args
///
/// Returns the error string, it must be deallocated by the caller
#[doc(alias = "xmlRelaxNGGetErrorString")]
unsafe fn xml_relaxng_get_error_string(
    err: XmlRelaxNGValidErr,
    mut arg1: *const u8,
    mut arg2: *const u8,
) -> *mut u8 {
    let mut msg: [i8; 1000] = [0; 1000];
    let mut result: *mut u8;

    if arg1.is_null() {
        arg1 = c"".as_ptr() as _;
    }
    if arg2.is_null() {
        arg2 = c"".as_ptr() as _;
    }

    msg[0] = 0;
    match err {
        XmlRelaxNGValidErr::XmlRelaxngOk => return null_mut(),
        XmlRelaxNGValidErr::XmlRelaxngErrMemory => {
            return xml_char_strdup(c"out of memory\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrType => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"failed to validate type %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrTypeval => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Type %s doesn't allow value '%s'\n".as_ptr() as _,
                arg1,
                arg2,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrDupid => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"ID %s redefined\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrTypecmp => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"failed to compare type %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNostate => {
            return xml_char_strdup(c"Internal error: no state\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNodefine => {
            return xml_char_strdup(c"Internal error: no define\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInternal => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Internal error: %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrListextra => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Extra data in list: %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInternodata => {
            return xml_char_strdup(c"Internal: interleave block has no data\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInterseq => {
            return xml_char_strdup(c"Invalid sequence in interleave\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInterextra => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Extra element %s in interleave\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemname => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Expecting element %s, got %s\n".as_ptr() as _,
                arg1,
                arg2,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemnons => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Expecting a namespace for element %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemwrongns => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Element %s has wrong namespace: expecting %s\n".as_ptr() as _,
                arg1,
                arg2,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemwrong => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Did not expect element %s there\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrTextwrong => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Did not expect text in element %s content\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemextrans => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Expecting no namespace for element %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemnotempty => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Expecting element %s to be empty\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNoelem => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Expecting an element %s, got nothing\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNotelem => {
            return xml_char_strdup(c"Expecting an element got text\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrAttrvalid => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Element %s failed to validate attributes\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrContentvalid => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Element %s failed to validate content\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrExtracontent => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Element %s has extra content: %s\n".as_ptr() as _,
                arg1,
                arg2,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInvalidattr => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Invalid attribute %s for element %s\n".as_ptr() as _,
                arg1,
                arg2,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrLackdata => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Datatype element %s contains no data\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrDataelem => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Datatype element %s has child elements\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrValelem => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Value element %s has child elements\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrListelem => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"List element %s has child elements\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrDatatype => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Error validating datatype %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrValue => {
            snprintf(
                msg.as_mut_ptr() as _,
                1000,
                c"Error validating value %s\n".as_ptr() as _,
                arg1,
            );
        }
        XmlRelaxNGValidErr::XmlRelaxngErrList => {
            return xml_char_strdup(c"Error validating list\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNogrammar => {
            return xml_char_strdup(c"No top grammar defined\n".as_ptr() as _)
        }
        XmlRelaxNGValidErr::XmlRelaxngErrExtradata => {
            return xml_char_strdup(c"Extra data in the document\n".as_ptr() as _)
        }
        _ => return xml_char_strdup(c"Unknown error !\n".as_ptr() as _),
    }
    if msg[0] == 0 {
        snprintf(
            msg.as_mut_ptr() as _,
            1000,
            c"Unknown error code %d\n".as_ptr() as _,
            err,
        );
    }
    msg[1000 - 1] = 0;
    result = xml_char_strdup(msg.as_ptr() as _);
    xml_escape_format_string(&raw mut result)
}

/// Handle a Relax NG Validation error
///
/// # Note
/// This method does not format the string.  
#[doc(alias = "xmlRngVErr")]
unsafe fn xml_rng_verr(
    ctxt: XmlRelaxNGValidCtxtPtr,
    node: *mut XmlNode,
    error: i32,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
) {
    let mut schannel: Option<StructuredError> = None;
    let mut channel: Option<GenericError> = None;
    let mut data = None;

    if !ctxt.is_null() {
        if (*ctxt).serror.is_some() {
            schannel = (*ctxt).serror;
        } else {
            channel = (*ctxt).error;
        }
        data = (*ctxt).user_data.clone();
        (*ctxt).nb_errors += 1;
    }
    let error = XmlParserErrors::try_from(error).unwrap();
    __xml_raise_error!(
        schannel,
        channel,
        data,
        null_mut(),
        node as _,
        XmlErrorDomain::XmlFromRelaxngv,
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
}

/// Show a validation error.
#[doc(alias = "xmlRelaxNGShowValidError")]
unsafe fn xml_relaxng_show_valid_error(
    ctxt: XmlRelaxNGValidCtxtPtr,
    err: XmlRelaxNGValidErr,
    node: *mut XmlNode,
    child: *mut XmlNode,
    arg1: *const u8,
    arg2: *const u8,
) {
    if (*ctxt).flags & FLAGS_NOERROR != 0 {
        return;
    }

    let msg: *mut u8 = xml_relaxng_get_error_string(err, arg1, arg2);
    if msg.is_null() {
        return;
    }
    let message = CStr::from_ptr(msg as *const i8)
        .to_string_lossy()
        .into_owned();
    xml_free(msg as _);

    if (*ctxt).err_no == XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
        (*ctxt).err_no = err as i32;
    }
    xml_rng_verr(
        ctxt,
        if child.is_null() { node } else { child },
        err as _,
        message.as_str(),
        (!arg1.is_null())
            .then(|| CStr::from_ptr(arg1 as *const i8).to_string_lossy())
            .as_deref(),
        (!arg2.is_null())
            .then(|| CStr::from_ptr(arg2 as *const i8).to_string_lossy())
            .as_deref(),
    );
}

const MAX_ERROR: usize = 5;

/// Show all validation error over a given index.
#[doc(alias = "xmlRelaxNGDumpValidError")]
pub(crate) unsafe fn xml_relaxng_dump_valid_error(ctxt: XmlRelaxNGValidCtxtPtr) {
    let mut k = 0;
    for (i, err) in (*ctxt).err_tab.iter_mut().enumerate() {
        if k < MAX_ERROR {
            if (*ctxt).err_tab[..i].iter().any(|dup| {
                err.err == dup.err
                    && err.node == dup.node
                    && xml_str_equal(err.arg1, dup.arg1)
                    && xml_str_equal(err.arg2, dup.arg2)
            }) {
                if err.flags & ERROR_IS_DUP != 0 {
                    if !err.arg1.is_null() {
                        xml_free(err.arg1 as _);
                    }
                    err.arg1 = null_mut();
                    if !err.arg2.is_null() {
                        xml_free(err.arg2 as _);
                    }
                    err.arg2 = null_mut();
                    err.flags = 0;
                }
                continue;
            }
            xml_relaxng_show_valid_error(ctxt, err.err, err.node, err.seq, err.arg1, err.arg2);
            k += 1;
        }
    }

    (*ctxt).err_tab.clear();
}

/// Pushes a new error on top of the error stack
///
/// Returns 0 in case of error, the index in the stack otherwise
#[doc(alias = "xmlRelaxNGValidErrorPush")]
unsafe fn xml_relaxng_valid_error_push(
    ctxt: XmlRelaxNGValidCtxtPtr,
    err: XmlRelaxNGValidErr,
    arg1: *const u8,
    arg2: *const u8,
    dup: i32,
) -> i32 {
    if let Some(last_error) = (*ctxt).err_tab.last() {
        if !(*ctxt).state.is_null()
            && last_error.node == (*(*ctxt).state).node
            && last_error.err == err
        {
            return (*ctxt).err_tab.len() as i32 - 1;
        }
    }
    let mut cur = XmlRelaxNGValidError {
        err,
        ..Default::default()
    };
    if dup != 0 {
        cur.arg1 = xml_strdup(arg1);
        cur.arg2 = xml_strdup(arg2);
        cur.flags = ERROR_IS_DUP;
    } else {
        cur.arg1 = arg1;
        cur.arg2 = arg2;
        cur.flags = 0;
    }
    if !(*ctxt).state.is_null() {
        cur.node = (*(*ctxt).state).node;
        cur.seq = (*(*ctxt).state).seq;
    } else {
        cur.node = null_mut();
        cur.seq = null_mut();
    }
    (*ctxt).err_tab.push(cur);
    (*ctxt).err_tab.len() as i32 - 1
}

/// Pops the top error from the error stack
#[doc(alias = "xmlRelaxNGValidErrorPop")]
pub(crate) unsafe fn xml_relaxng_valid_error_pop(ctxt: XmlRelaxNGValidCtxtPtr) {
    let Some(cur) = (*ctxt).err_tab.pop() else {
        return;
    };

    if cur.flags & ERROR_IS_DUP != 0 {
        if !cur.arg1.is_null() {
            xml_free(cur.arg1 as _);
        }
        if !cur.arg2.is_null() {
            xml_free(cur.arg2 as _);
        }
    }
}

/// pop and discard all errors until the given level is reached
#[doc(alias = "xmlRelaxNGPopErrors")]
pub(crate) unsafe fn xml_relaxng_pop_errors(ctxt: XmlRelaxNGValidCtxtPtr, level: i32) {
    for err in (*ctxt).err_tab[level as usize..].iter_mut() {
        if err.flags & ERROR_IS_DUP != 0 {
            if !err.arg1.is_null() {
                xml_free(err.arg1 as _);
            }
            err.arg1 = null_mut();
            if !err.arg2.is_null() {
                xml_free(err.arg2 as _);
            }
            err.arg2 = null_mut();
            err.flags = 0;
        }
    }
    (*ctxt).err_tab.truncate(level as usize);
}

/// Register a validation error, either generating it if it's sure
/// or stacking it for later handling if unsure.
#[doc(alias = "xmlRelaxNGAddValidError")]
pub(crate) unsafe fn xml_relaxng_add_valid_error(
    ctxt: XmlRelaxNGValidCtxtPtr,
    err: XmlRelaxNGValidErr,
    arg1: *const u8,
    arg2: *const u8,
    dup: i32,
) {
    if ctxt.is_null() {
        return;
    }
    if (*ctxt).flags & FLAGS_NOERROR != 0 {
        return;
    }

    // generate the error directly
    if (*ctxt).flags & FLAGS_IGNORABLE == 0 || (*ctxt).flags & FLAGS_NEGATIVE != 0 {
        let mut node: *mut XmlNode;
        let seq: *mut XmlNode;

        // Flush first any stacked error which might be the
        // real cause of the problem.
        if !(*ctxt).err_tab.is_empty() {
            xml_relaxng_dump_valid_error(ctxt);
        }
        if !(*ctxt).state.is_null() {
            node = (*(*ctxt).state).node;
            seq = (*(*ctxt).state).seq;
        } else {
            node = null_mut();
            seq = node;
        }
        if node.is_null() && seq.is_null() {
            node = (*ctxt).pnode;
        }
        xml_relaxng_show_valid_error(ctxt, err, node, seq, arg1, arg2);
    } else {
        // Stack the error for later processing if needed
        xml_relaxng_valid_error_push(ctxt, err, arg1, arg2, dup);
    }
}

/// Handle a redefinition of attribute error
#[doc(alias = "xmlRngVErrMemory")]
pub(crate) unsafe fn xml_rng_verr_memory(ctxt: XmlRelaxNGValidCtxtPtr, extra: &str) {
    let mut schannel: Option<StructuredError> = None;
    let mut channel: Option<GenericError> = None;
    let mut data = None;

    if !ctxt.is_null() {
        if let Some(serror) = (*ctxt).serror {
            schannel = Some(serror);
        } else {
            channel = (*ctxt).error;
        }
        data = (*ctxt).user_data.clone();
        (*ctxt).nb_errors += 1;
    }
    __xml_raise_error!(
        schannel,
        channel,
        data,
        null_mut(),
        null_mut(),
        XmlErrorDomain::XmlFromRelaxngv,
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
}

/// Handle a redefinition of attribute error
#[doc(alias = "xmlRngPErrMemory")]
pub(crate) unsafe fn xml_rng_perr_memory(ctxt: XmlRelaxNGParserCtxtPtr, extra: Option<&str>) {
    let mut schannel: Option<StructuredError> = None;
    let mut channel: Option<GenericError> = None;
    let mut data = None;

    if !ctxt.is_null() {
        if (*ctxt).serror.is_some() {
            schannel = (*ctxt).serror;
        } else {
            channel = (*ctxt).error;
        }
        data = (*ctxt).user_data.clone();
        (*ctxt).nb_errors += 1;
    }
    if let Some(extra) = extra {
        __xml_raise_error!(
            schannel,
            channel,
            data,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromRelaxngp,
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
            schannel,
            channel,
            data,
            null_mut(),
            null_mut(),
            XmlErrorDomain::XmlFromRelaxngp,
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

pub(crate) unsafe fn xml_rng_err_internal(
    ctxt: *mut XmlRelaxNGParserCtxt,
    node: *mut XmlNode,
    error: XmlParserErrors,
    msg: &str,
    str1: Option<Cow<'static, str>>,
    str2: Option<Cow<'static, str>>,
) {
    let mut schannel: Option<StructuredError> = None;
    let mut channel: Option<GenericError> = None;
    let mut data = None;

    if !ctxt.is_null() {
        if (*ctxt).serror.is_some() {
            schannel = (*ctxt).serror;
        } else {
            channel = (*ctxt).error;
        }
        data = (*ctxt).user_data.clone();
        (*ctxt).nb_errors += 1;
    }
    __xml_raise_error!(
        schannel,
        channel,
        data,
        null_mut(),
        node as _,
        XmlErrorDomain::XmlFromRelaxngp,
        error,
        XmlErrorLevel::XmlErrError,
        None,
        0,
        str1,
        str2,
        None,
        0,
        0,
        msg,
    );
}

/// Handle a Relax NG Parsing error
#[doc(alias = "xmlRngPErr")]
macro_rules! xml_rng_perr {
    ($ctxt:expr, $node:expr, $error:expr, $msg:literal) => {
        xml_rng_perr!(@inner, $ctxt, $node, $error, $msg, None, None);
    };
    ($ctxt:expr, $node:expr, $error:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        xml_rng_perr!(
            @inner,
            $ctxt,
            $node,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            None
        );
    };
    ($ctxt:expr, $node:expr, $error:expr, $msg:literal, $str1:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $str2);
        xml_rng_perr!(
            @inner,
            $ctxt,
            $node,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            Some($str2.to_owned().into())
        );
    };
    (@inner, $ctxt:expr, $node:expr, $error:expr, $msg:expr, $str1:expr, $str2:expr) => {
        // If this function is expanded at here,  stack overflow occurs...
        $crate::relaxng::xml_rng_err_internal($ctxt as _, $node as _, $error, $msg, $str1, $str2);
    };
}
pub(crate) use xml_rng_perr;

macro_rules! VALID_ERR {
    ($ctxt:expr, $a:expr) => {
        $crate::relaxng::xml_relaxng_add_valid_error($ctxt, $a, null_mut(), null_mut(), 0);
    };
}
macro_rules! VALID_ERR2 {
    ($ctxt:expr, $a:expr, $b:expr) => {
        $crate::relaxng::xml_relaxng_add_valid_error($ctxt, $a, $b, null_mut(), 0);
    };
}
macro_rules! VALID_ERR3 {
    ($ctxt:expr, $a:expr, $b:expr, $c:expr) => {
        $crate::relaxng::xml_relaxng_add_valid_error($ctxt, $a, $b, $c, 0);
    };
}
macro_rules! VALID_ERR2P {
    ($ctxt:expr, $a:expr, $b:expr) => {
        $crate::relaxng::xml_relaxng_add_valid_error($ctxt, $a, $b, null_mut(), 1);
    };
}
macro_rules! VALID_ERR3P {
    ($ctxt:expr, $a:expr, $b:expr, $c:expr) => {
        $crate::relaxng::xml_relaxng_add_valid_error($ctxt, $a, $b, $c, 1);
    };
}
pub(crate) use VALID_ERR;
pub(crate) use VALID_ERR2;
pub(crate) use VALID_ERR2P;
pub(crate) use VALID_ERR3;
pub(crate) use VALID_ERR3P;

use super::{XmlRelaxNGParserCtxt, XmlRelaxNGParserCtxtPtr, XmlRelaxNGValidCtxtPtr};

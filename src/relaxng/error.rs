use std::{borrow::Cow, ptr::null_mut};

use crate::{
    error::{__xml_raise_error, XmlErrorDomain, XmlErrorLevel, XmlParserErrors},
    globals::{GenericError, StructuredError},
    libxml::relaxng::{
        FLAGS_IGNORABLE, FLAGS_NEGATIVE, FLAGS_NOERROR, XmlRelaxNGValidErr, XmlRelaxNGValidError,
    },
    tree::XmlGenericNodePtr,
};

const ERROR_IS_DUP: i32 = 1;

/// Computes a formatted error string for the given error code and args
///
/// Returns the error string, it must be deallocated by the caller
#[doc(alias = "xmlRelaxNGGetErrorString")]
fn xml_relaxng_get_error_string(
    err: XmlRelaxNGValidErr,
    arg1: Option<&str>,
    arg2: Option<&str>,
) -> Option<String> {
    let arg1 = arg1.unwrap_or("");
    let arg2 = arg2.unwrap_or("");

    match err {
        XmlRelaxNGValidErr::XmlRelaxngOk => None,
        XmlRelaxNGValidErr::XmlRelaxngErrMemory => Some("out of memory\n".to_string()),
        XmlRelaxNGValidErr::XmlRelaxngErrType => Some(format!("failed to validate type {arg1}\n")),
        XmlRelaxNGValidErr::XmlRelaxngErrTypeval => {
            Some(format!("Type {arg1} doesn't allow value '{arg2}'\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrDupid => Some(format!("ID {arg1} redefined\n",)),
        XmlRelaxNGValidErr::XmlRelaxngErrTypecmp => {
            Some(format!("failed to compare type {arg1}\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNostate => Some("Internal error: no state\n".to_string()),
        XmlRelaxNGValidErr::XmlRelaxngErrNodefine => {
            Some("Internal error: no define\n".to_string())
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInternal => Some(format!("Internal error: {arg1}\n")),
        XmlRelaxNGValidErr::XmlRelaxngErrListextra => Some(format!("Extra data in list: {arg1}\n")),
        XmlRelaxNGValidErr::XmlRelaxngErrInternodata => {
            Some("Internal: interleave block has no data\n".to_string())
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInterseq => {
            Some("Invalid sequence in interleave\n".to_string())
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInterextra => {
            Some(format!("Extra element {arg1} in interleave\n",))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemname => {
            Some(format!("Expecting element {arg1}, got {arg2}\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemnons => {
            Some(format!("Expecting a namespace for element {arg1}\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemwrongns => Some(format!(
            "Element {arg1} has wrong namespace: expecting {arg2}\n"
        )),
        XmlRelaxNGValidErr::XmlRelaxngErrElemwrong => {
            Some(format!("Did not expect element {arg1} there\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrTextwrong => {
            Some(format!("Did not expect text in element {arg1} content\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemextrans => {
            Some(format!("Expecting no namespace for element {arg1}\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrElemnotempty => {
            Some(format!("Expecting element {arg1} to be empty\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNoelem => {
            Some(format!("Expecting an element {arg1}, got nothing\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrNotelem => {
            Some("Expecting an element got text\n".to_string())
        }
        XmlRelaxNGValidErr::XmlRelaxngErrAttrvalid => {
            Some(format!("Element {arg1} failed to validate attributes\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrContentvalid => {
            Some(format!("Element {arg1} failed to validate content\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrExtracontent => {
            Some(format!("Element {arg1} has extra content: {arg2}\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrInvalidattr => {
            Some(format!("Invalid attribute {arg1} for element {arg2}\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrLackdata => {
            Some(format!("Datatype element {arg1} contains no data\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrDataelem => {
            Some(format!("Datatype element {arg1} has child elements\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrValelem => {
            Some(format!("Value element {arg1} has child elements\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrListelem => {
            Some(format!("List element {arg1} has child elements\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrDatatype => {
            Some(format!("Error validating datatype {arg1}\n"))
        }
        XmlRelaxNGValidErr::XmlRelaxngErrValue => Some(format!("Error validating value {arg1}\n")),
        XmlRelaxNGValidErr::XmlRelaxngErrList => Some("Error validating list\n".to_string()),
        XmlRelaxNGValidErr::XmlRelaxngErrNogrammar => Some("No top grammar defined\n".to_string()),
        XmlRelaxNGValidErr::XmlRelaxngErrExtradata => {
            Some("Extra data in the document\n".to_string())
        }
        _ => Some("Unknown error !\n".to_string()),
    }
}

/// Handle a Relax NG Validation error
///
/// # Note
/// This method does not format the string.  
#[doc(alias = "xmlRngVErr")]
unsafe fn xml_rng_verr(
    ctxt: XmlRelaxNGValidCtxtPtr,
    node: Option<XmlGenericNodePtr>,
    error: i32,
    msg: &str,
    str1: Option<&str>,
    str2: Option<&str>,
) {
    unsafe {
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
            node,
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
            Some(msg),
        );
    }
}

/// Show a validation error.
#[doc(alias = "xmlRelaxNGShowValidError")]
unsafe fn xml_relaxng_show_valid_error(
    ctxt: XmlRelaxNGValidCtxtPtr,
    err: XmlRelaxNGValidErr,
    node: Option<XmlGenericNodePtr>,
    child: Option<XmlGenericNodePtr>,
    arg1: Option<&str>,
    arg2: Option<&str>,
) {
    unsafe {
        if (*ctxt).flags & FLAGS_NOERROR != 0 {
            return;
        }

        let Some(message) = xml_relaxng_get_error_string(err, arg1, arg2) else {
            return;
        };

        if (*ctxt).err_no == XmlRelaxNGValidErr::XmlRelaxngOk as i32 {
            (*ctxt).err_no = err as i32;
        }
        xml_rng_verr(ctxt, child.or(node), err as _, message.as_str(), arg1, arg2);
    }
}

const MAX_ERROR: usize = 5;

/// Show all validation error over a given index.
#[doc(alias = "xmlRelaxNGDumpValidError")]
pub(crate) unsafe fn xml_relaxng_dump_valid_error(ctxt: XmlRelaxNGValidCtxtPtr) {
    unsafe {
        let mut k = 0;
        for (i, err) in (*ctxt).err_tab.iter_mut().enumerate() {
            if k < MAX_ERROR {
                if (*ctxt).err_tab[..i].iter().any(|dup| {
                    err.err == dup.err
                        && err.node == dup.node
                        && err.arg1 == dup.arg1
                        && err.arg2 == dup.arg2
                }) {
                    if err.flags & ERROR_IS_DUP != 0 {
                        err.arg1 = None;
                        err.arg2 = None;
                        err.flags = 0;
                    }
                    continue;
                }
                xml_relaxng_show_valid_error(
                    ctxt,
                    err.err,
                    err.node,
                    err.seq,
                    err.arg1.as_deref(),
                    err.arg2.as_deref(),
                );
                k += 1;
            }
        }

        (*ctxt).err_tab.clear();
    }
}

/// Pushes a new error on top of the error stack
///
/// Returns 0 in case of error, the index in the stack otherwise
#[doc(alias = "xmlRelaxNGValidErrorPush")]
unsafe fn xml_relaxng_valid_error_push(
    ctxt: XmlRelaxNGValidCtxtPtr,
    err: XmlRelaxNGValidErr,
    arg1: Option<&str>,
    arg2: Option<&str>,
    _dup: i32,
) -> i32 {
    unsafe {
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
        cur.arg1 = arg1.map(|arg| arg.to_owned());
        cur.arg2 = arg2.map(|arg| arg.to_owned());
        cur.flags = 0;
        if !(*ctxt).state.is_null() {
            cur.node = (*(*ctxt).state).node;
            cur.seq = (*(*ctxt).state).seq;
        } else {
            cur.node = None;
            cur.seq = None;
        }
        (*ctxt).err_tab.push(cur);
        (*ctxt).err_tab.len() as i32 - 1
    }
}

/// Pops the top error from the error stack
#[doc(alias = "xmlRelaxNGValidErrorPop")]
pub(crate) unsafe fn xml_relaxng_valid_error_pop(ctxt: XmlRelaxNGValidCtxtPtr) {
    unsafe {
        (*ctxt).err_tab.pop();
    }
}

/// pop and discard all errors until the given level is reached
#[doc(alias = "xmlRelaxNGPopErrors")]
pub(crate) unsafe fn xml_relaxng_pop_errors(ctxt: XmlRelaxNGValidCtxtPtr, level: i32) {
    unsafe {
        for err in (*ctxt).err_tab[level as usize..].iter_mut() {
            if err.flags & ERROR_IS_DUP != 0 {
                err.arg1 = None;
                err.arg2 = None;
                err.flags = 0;
            }
        }
        (*ctxt).err_tab.truncate(level as usize);
    }
}

/// Register a validation error, either generating it if it's sure
/// or stacking it for later handling if unsure.
#[doc(alias = "xmlRelaxNGAddValidError")]
pub(crate) unsafe fn xml_relaxng_add_valid_error(
    ctxt: XmlRelaxNGValidCtxtPtr,
    err: XmlRelaxNGValidErr,
    arg1: Option<&str>,
    arg2: Option<&str>,
    dup: i32,
) {
    unsafe {
        if ctxt.is_null() {
            return;
        }
        if (*ctxt).flags & FLAGS_NOERROR != 0 {
            return;
        }

        // generate the error directly
        if (*ctxt).flags & FLAGS_IGNORABLE == 0 || (*ctxt).flags & FLAGS_NEGATIVE != 0 {
            // Flush first any stacked error which might be the
            // real cause of the problem.
            if !(*ctxt).err_tab.is_empty() {
                xml_relaxng_dump_valid_error(ctxt);
            }
            let (mut node, seq) = if !(*ctxt).state.is_null() {
                ((*(*ctxt).state).node, (*(*ctxt).state).seq)
            } else {
                (None, None)
            };
            if node.is_none() && seq.is_none() {
                node = (*ctxt).pnode.map(|node| node.into());
            }
            xml_relaxng_show_valid_error(ctxt, err, node, seq, arg1, arg2);
        } else {
            // Stack the error for later processing if needed
            xml_relaxng_valid_error_push(ctxt, err, arg1, arg2, dup);
        }
    }
}

/// Handle a redefinition of attribute error
#[doc(alias = "xmlRngVErrMemory")]
pub(crate) unsafe fn xml_rng_verr_memory(ctxt: XmlRelaxNGValidCtxtPtr, extra: &str) {
    unsafe {
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
            None,
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
}

/// Handle a redefinition of attribute error
#[doc(alias = "xmlRngPErrMemory")]
pub(crate) unsafe fn xml_rng_perr_memory(ctxt: XmlRelaxNGParserCtxtPtr, extra: Option<&str>) {
    unsafe {
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
                None,
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
                None,
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
}

pub(crate) unsafe fn xml_rng_err_internal(
    ctxt: *mut XmlRelaxNGParserCtxt,
    node: Option<XmlGenericNodePtr>,
    error: XmlParserErrors,
    msg: &str,
    str1: Option<Cow<'static, str>>,
    str2: Option<Cow<'static, str>>,
) {
    unsafe {
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
            node,
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
            Some(msg),
        );
    }
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
        $crate::relaxng::xml_relaxng_add_valid_error($ctxt, $a, None, None, 0);
    };
}
macro_rules! VALID_ERR2 {
    ($ctxt:expr, $a:expr, $b:expr) => {
        $crate::relaxng::xml_relaxng_add_valid_error($ctxt, $a, $b, None, 0);
    };
}
macro_rules! VALID_ERR3 {
    ($ctxt:expr, $a:expr, $b:expr, $c:expr) => {
        $crate::relaxng::xml_relaxng_add_valid_error($ctxt, $a, $b, $c, 0);
    };
}
macro_rules! VALID_ERR2P {
    ($ctxt:expr, $a:expr, $b:expr) => {
        $crate::relaxng::xml_relaxng_add_valid_error($ctxt, $a, $b, None, 1);
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

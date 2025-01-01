use crate::{
    error::{XmlParserErrors, __xml_raise_error},
    libxml::parser::{XmlParserCtxtPtr, XmlParserInputState},
};

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlFatalErrMsg")]
pub(crate) unsafe fn xml_fatal_err_msg(ctxt: XmlParserCtxtPtr, error: XmlParserErrors, msg: &str) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = error as i32;
    }
    __xml_raise_error!(
        None,
        None,
        None,
        ctxt as _,
        null_mut(),
        XmlErrorDomain::XmlFromParser,
        error,
        XmlErrorLevel::XmlErrFatal,
        None,
        0,
        None,
        None,
        None,
        0,
        0,
        msg,
    );
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
        if (*ctxt).recovery == 0 {
            (*ctxt).disable_sax = 1;
        }
    }
}

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlFatalErrMsgStr")]
macro_rules! xml_fatal_err_msg_str {
    ($ctxt:expr, $error:expr, $msg:literal) =>  {
        $crate::parser::xml_fatal_err_msg_str!(@inner $ctxt, $error, $msg, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $val:expr) =>  {
        let msg = format!($msg, $val);
        $crate::parser::xml_fatal_err_msg_str!(@inner $ctxt, $error, msg.as_str(), Some($val.to_owned().into()));
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $val:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                $error,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                $val,
                None,
                None,
                0,
                0,
                $msg,
            );
            if !ctxt.is_null() {
                (*ctxt).well_formed = 0;
                if (*ctxt).recovery == 0 {
                    (*ctxt).disable_sax = 1;
                }
            }
        }
    };
}
pub(crate) use xml_fatal_err_msg_str;

/// Handle a warning.
#[doc(alias = "xmlWarningMsg")]
macro_rules! xml_warning_msg {
    ($ctxt:expr, $error:expr, $msg:literal) => {
        $crate::parser::xml_warning_msg!(@inner $ctxt, $error, $msg, None, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        $crate::parser::xml_warning_msg!(@inner $ctxt, $error, &msg, Some($str1.to_owned().into()), None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $str2);
        $crate::parser::xml_warning_msg!(@inner $ctxt, $error, &msg, Some($str1.to_owned().into()), Some($str2.to_owned().into()));
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $str1:expr, $str2:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        let mut schannel: Option<$crate::globals::StructuredError> = None;

        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null()
            {
                if let Some(sax) = (*ctxt).sax.as_deref().filter(|sax| sax.initialized == $crate::libxml::parser::XML_SAX2_MAGIC as u32) {
                    schannel = sax.serror;
                }
                __xml_raise_error!(
                    schannel,
                    (*ctxt).sax.as_deref_mut().and_then(|sax| sax.warning),
                    (*ctxt).user_data.clone(),
                    ctxt as _,
                    null_mut(),
                    XmlErrorDomain::XmlFromParser,
                    $error,
                    XmlErrorLevel::XmlErrWarning,
                    None,
                    0,
                    $str1,
                    $str2,
                    None,
                    0,
                    0,
                    $msg,
                );
            } else {
                __xml_raise_error!(
                    schannel,
                    None,
                    None,
                    null_mut(),
                    null_mut(),
                    XmlErrorDomain::XmlFromParser,
                    $error,
                    XmlErrorLevel::XmlErrWarning,
                    None,
                    0,
                    $str1,
                    $str2,
                    None,
                    0,
                    0,
                    $msg,
                );
            }
        }
    };
}
pub(crate) use xml_warning_msg;

/// Handle a non fatal parser error
#[doc(alias = "xmlErrMsgStr")]
macro_rules! xml_err_msg_str {
    ($ctxt:expr, $error:expr, $msg:literal) => {
        $crate::parser::xml_err_msg_str!(@inner $ctxt, $error, $msg, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $val:expr) => {
        let msg = format!($msg, $val);
        $crate::parser::xml_err_msg_str!(@inner $ctxt, $error, &msg, Some($val.to_owned().into()));
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $val:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                $error,
                XmlErrorLevel::XmlErrError,
                None,
                0,
                $val,
                None,
                None,
                0,
                0,
                $msg,
            );
        }
    };
}
pub(crate) use xml_err_msg_str;

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlFatalErrMsgInt")]
macro_rules! xml_fatal_err_msg_int {
    ($ctxt:expr, $error:expr, $msg:expr, $val:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                $error,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                None,
                None,
                None,
                $val,
                0,
                $msg,
            );
            if !ctxt.is_null() {
                (*ctxt).well_formed = 0;
                if (*ctxt).recovery == 0 {
                    (*ctxt).disable_sax = 1;
                }
            }
        }
    };
}
pub(crate) use xml_fatal_err_msg_int;

/// Handle a validity error.
#[doc(alias = "xmlValidityError")]
macro_rules! xml_validity_error {
    ($ctxt:expr, $error:expr, $msg:literal) => {
        $crate::parser::xml_validity_error!(@inner $ctxt, $error, $msg, None, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        $crate::parser::xml_validity_error!(@inner $ctxt, $error, &msg, Some($str1.to_owned().into()), None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $str2);
        $crate::parser::xml_validity_error!(@inner $ctxt, $error, &msg, Some($str1.to_owned().into()), Some($str2.to_owned().into()));
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $str1:expr, $str2:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        let mut schannel: Option<$crate::globals::StructuredError> = None;

        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
                if let Some(sax) = (*ctxt).sax.as_deref().filter(|sax| sax.initialized == $crate::libxml::parser::XML_SAX2_MAGIC as u32) {
                    schannel = sax.serror;
                }
                __xml_raise_error!(
                    schannel,
                    (*ctxt).vctxt.error,
                    (*ctxt).vctxt.user_data.clone(),
                    ctxt as _,
                    null_mut(),
                    XmlErrorDomain::XmlFromDTD,
                    $error,
                    XmlErrorLevel::XmlErrError,
                    None,
                    0,
                    $str1,
                    $str2,
                    None,
                    0,
                    0,
                    $msg,
                );
                (*ctxt).valid = 0;
            } else {
                __xml_raise_error!(
                    schannel,
                    None,
                    None,
                    ctxt as _,
                    null_mut(),
                    XmlErrorDomain::XmlFromDTD,
                    $error,
                    XmlErrorLevel::XmlErrError,
                    None,
                    0,
                    $str1,
                    $str2,
                    None,
                    0,
                    0,
                    $msg,
                );
            }
        }
    };
}
pub(crate) use xml_validity_error;

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlFatalErrMsgStrIntStr")]
macro_rules! xml_fatal_err_msg_str_int_str {
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr, $val:expr) => {
        let msg = format!($msg, $str1, $val);
        $crate::parser::xml_fatal_err_msg_str_int_str!(
            @inner
            $ctxt,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            $val,
            None
        );
    };
    ($ctxt:expr, $error:expr, $msg:literal, $str1:expr, $val:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $val, $str2);
        $crate::parser::xml_fatal_err_msg_str_int_str!(
            @inner
            $ctxt,
            $error,
            &msg,
            Some($str1.to_owned().into()),
            $val,
            Some($str2.to_owned().into())
        );
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $str1:expr, $val:expr, $str2:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                $error,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                $str1,
                $str2,
                None,
                $val,
                0,
                $msg,
            );
            if !ctxt.is_null() {
                (*ctxt).well_formed = 0;
                if (*ctxt).recovery == 0 {
                    (*ctxt).disable_sax = 1;
                }
            }
        }
    };
}
pub(crate) use xml_fatal_err_msg_str_int_str;

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlNsErr")]
macro_rules! xml_ns_err {
    ($ctxt:expr, $error:expr, $msg:literal) => {
        $crate::parser::xml_ns_err!(@inner $ctxt, $error, $msg, None, None, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $info1:expr) => {
        let msg = format!($msg, $info1);
        $crate::parser::xml_ns_err!(@inner $ctxt, $error, &msg, Some($info1.to_owned().into()), None, None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $info1:expr, $info2:expr) => {
        let msg = format!($msg, $info1, $info2);
        $crate::parser::xml_ns_err!(@inner $ctxt, $error, &msg, Some($info1.to_owned().into()), Some($info2.to_owned().into()), None);
    };
    ($ctxt:expr, $error:expr, $msg:literal, $info1:expr, $info2:expr, $info3:expr) => {
        let msg = format!($msg, $info1, $info2, $info3);
        $crate::parser::xml_ns_err!(@inner $ctxt, $error, &msg, Some($info1.to_owned().into()), Some($info2.to_owned().into()), Some($info3.to_owned().into()));
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $info1:expr, $info2:expr, $info3:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromNamespace,
                $error,
                XmlErrorLevel::XmlErrError,
                None,
                0,
                $info1,
                $info2,
                $info3,
                0,
                0,
                $msg,
            );
            if !ctxt.is_null() {
                (*ctxt).ns_well_formed = 0;
            }
        }
    };
}
pub(crate) use xml_ns_err;

/// Handle an internal error
#[doc(alias = "xmlErrInternal")]
macro_rules! xml_err_internal {
    ($ctxt:expr, $msg:literal) => {
        $crate::parser::xml_err_internal!(@inner $ctxt, $msg, None);
    };
    ($ctxt:expr, $msg:literal, $s:expr) => {
        let msg = format!($msg, $s);
        $crate::parser::xml_err_internal!(@inner $ctxt, &msg, Some($s.into()));
    };
    (@inner $ctxt:expr, $msg:expr, $s:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = XmlParserErrors::XmlErrInternalError as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                XmlParserErrors::XmlErrInternalError,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                $s,
                None,
                None,
                0,
                0,
                $msg,
            );
            if !ctxt.is_null() {
                (*ctxt).well_formed = 0;
                if (*ctxt).recovery == 0 {
                    (*ctxt).disable_sax = 1;
                }
            }
        }
    };
}
pub(crate) use xml_err_internal;

/// Handle a redefinition of attribute error
#[doc(alias = "xmlErrMemory")]
pub(crate) unsafe fn xml_err_memory(ctxt: XmlParserCtxtPtr, extra: Option<&str>) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = XmlParserErrors::XmlErrNoMemory as i32;
        (*ctxt).instate = XmlParserInputState::XmlParserEOF;
        (*ctxt).disable_sax = 1;
    }
    if let Some(extra) = extra {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
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
            None,
            None,
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
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

/// Handle an encoding error
#[doc(alias = "__xmlErrEncoding")]
macro_rules! __xml_err_encoding {
    ($ctxt:expr, $xmlerr:expr, $msg:literal) => {
        $crate::parser::__xml_err_encoding!(@inner $ctxt, $xmlerr, $msg, None, None);
    };
    ($ctxt:expr, $xmlerr:expr, $msg:literal, $str1:expr) => {
        let msg = format!($msg, $str1);
        $crate::parser::__xml_err_encoding!(@inner $ctxt, $xmlerr, &msg, Some($str1.to_owned().into()), None);
    };
    ($ctxt:expr, $xmlerr:expr, $msg:literal, $str1:expr, $str2:expr) => {
        let msg = format!($msg, $str1, $str2);
        $crate::libxml::parser_internals::__xml_err_encoding!(@inner $ctxt, $xmlerr, &msg, Some($str1.to_owned().into()), Some($str2.to_owned().into()));
    };
    (@inner $ctxt:expr, $xmlerr:expr, $msg:expr, $str1:expr, $str2:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $xmlerr as _;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                $xmlerr,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                $str1,
                $str2,
                None,
                0,
                0,
                $msg,
            );
            if !ctxt.is_null() {
                (*ctxt).well_formed = 0;
                if (*ctxt).recovery == 0 {
                    (*ctxt).disable_sax = 1;
                }
            }
        }
    };
}
pub(crate) use __xml_err_encoding;

/// n encoding error
#[doc(alias = "xmlErrEncodingInt")]
macro_rules! xml_err_encoding_int {
    ($ctxt:expr, $error:expr, $msg:literal, $val:expr) => {
        let ctxt = $ctxt as *mut $crate::libxml::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromParser,
                $error,
                XmlErrorLevel::XmlErrFatal,
                None,
                0,
                None,
                None,
                None,
                $val,
                0,
                format!($msg, $val).as_str(),
            );
            if !ctxt.is_null() {
                (*ctxt).well_formed = 0;
                if (*ctxt).recovery == 0 {
                    (*ctxt).disable_sax = 1;
                }
            }
        }
    };
}
pub(crate) use xml_err_encoding_int;

/// Handle a namespace warning error
#[doc(alias = "xmlNsWarn")]
macro_rules! xml_ns_warn {
    ($ctxt:expr, $error:expr, $msg:expr) => {
        xml_ns_warn!(
            @inner
            $ctxt,
            $error,
            $msg,
            None,
            None,
            None
        )
    };
    ($ctxt:expr, $error:expr, $msg:expr, $info1:expr) => {
        let msg = format!($msg, $info1);
        xml_ns_warn!(
            @inner
            $ctxt,
            $error,
            &msg,
            Some($info1.to_owned().into()),
            None,
            None
        )
    };
    ($ctxt:expr, $error:expr, $msg:expr, $info1:expr, $info2:expr) => {
        let msg = format!($msg, $info1, $info2);
        xml_ns_warn!(
            @inner
            $ctxt,
            $error,
            &msg,
            Some($info1.to_owned().into()),
            Some($info2.to_owned().into()),
            None
        )
    };
    ($ctxt:expr, $error:expr, $msg:expr, $info1:expr, $info2:expr, $info3:expr) => {
        let msg = format!($msg, $info1, $info2, $info3);
        xml_ns_warn!(
            @inner
            $ctxt,
            $error,
            &msg,
            Some($info1.to_owned().into()),
            Some($info2.to_owned().into()),
            Some($info3.to_owned().into())
        )
    };
    (@inner $ctxt:expr, $error:expr, $msg:expr, $info1:expr, $info2:expr, $info3:expr) => {
        let ctxt = $ctxt as *mut XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            __xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                null_mut(),
                XmlErrorDomain::XmlFromNamespace,
                $error,
                XmlErrorLevel::XmlErrWarning,
                None,
                0,
                $info1,
                $info2,
                $info3,
                0,
                0,
                $msg,
            );
        }
    };
}
pub(crate) use xml_ns_warn;

/// Handle a redefinition of attribute error
#[doc(alias = "xmlErrAttributeDup")]
pub(crate) unsafe fn xml_err_attribute_dup(
    ctxt: XmlParserCtxtPtr,
    prefix: Option<&str>,
    localname: &str,
) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    if !ctxt.is_null() {
        (*ctxt).err_no = XmlParserErrors::XmlErrAttributeRedefined as i32;
    }

    if let Some(prefix) = prefix {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
            XmlParserErrors::XmlErrAttributeRedefined,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            Some(prefix.to_owned().into()),
            Some(localname.to_owned().into()),
            None,
            0,
            0,
            "Attribute {}:{} redefined\n",
            prefix,
            localname
        );
    } else {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            null_mut(),
            XmlErrorDomain::XmlFromParser,
            XmlParserErrors::XmlErrAttributeRedefined,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            Some(localname.to_owned().into()),
            None,
            None,
            0,
            0,
            "Attribute {} redefined\n",
            localname
        );
    }
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
        if (*ctxt).recovery == 0 {
            (*ctxt).disable_sax = 1;
        }
    }
}

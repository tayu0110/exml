use crate::{
    error::{XmlParserErrors, __xml_raise_error},
    libxml::parser::XmlParserInputState,
};

/// Handle a fatal parser error, i.e. violating Well-Formedness constraints
#[doc(alias = "xmlFatalErr")]
pub(crate) unsafe fn xml_fatal_err(
    ctxt: XmlParserCtxtPtr,
    error: XmlParserErrors,
    info: Option<&str>,
) {
    if !ctxt.is_null()
        && (*ctxt).disable_sax != 0
        && matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
    {
        return;
    }
    let errmsg = match error {
        XmlParserErrors::XmlErrInvalidHexCharRef => "CharRef: invalid hexadecimal value",
        XmlParserErrors::XmlErrInvalidDecCharRef => "CharRef: invalid decimal value",
        XmlParserErrors::XmlErrInvalidCharRef => "CharRef: invalid value",
        XmlParserErrors::XmlErrInternalError => "internal error",
        XmlParserErrors::XmlErrPERefAtEOF => "PEReference at end of document",
        XmlParserErrors::XmlErrPERefInProlog => "PEReference in prolog",
        XmlParserErrors::XmlErrPERefInEpilog => "PEReference in epilog",
        XmlParserErrors::XmlErrPERefNoName => "PEReference: no name",
        XmlParserErrors::XmlErrPERefSemicolMissing => "PEReference: expecting ';'",
        XmlParserErrors::XmlErrEntityLoop => "Detected an entity reference loop",
        XmlParserErrors::XmlErrEntityNotStarted => "EntityValue: \" or ' expected",
        XmlParserErrors::XmlErrEntityPEInternal => "PEReferences forbidden in internal subset",
        XmlParserErrors::XmlErrEntityNotFinished => "EntityValue: \" or ' expected",
        XmlParserErrors::XmlErrAttributeNotStarted => "AttValue: \" or ' expected",
        XmlParserErrors::XmlErrLtInAttribute => "Unescaped '<' not allowed in attributes values",
        XmlParserErrors::XmlErrLiteralNotStarted => "SystemLiteral \" or ' expected",
        XmlParserErrors::XmlErrLiteralNotFinished => {
            "Unfinished System or Public ID \" or ' expected"
        }
        XmlParserErrors::XmlErrMisplacedCDATAEnd => "Sequence ']]>' not allowed in content",
        XmlParserErrors::XmlErrURIRequired => "SYSTEM or PUBLIC, the URI is missing",
        XmlParserErrors::XmlErrPubidRequired => "PUBLIC, the Public Identifier is missing",
        XmlParserErrors::XmlErrHyphenInComment => "Comment must not contain '--' (double-hyphen)",
        XmlParserErrors::XmlErrPINotStarted => "xmlParsePI : no target name",
        XmlParserErrors::XmlErrReservedXmlName => "Invalid PI name",
        XmlParserErrors::XmlErrNotationNotStarted => "NOTATION: Name expected here",
        XmlParserErrors::XmlErrNotationNotFinished => "'>' required to close NOTATION declaration",
        XmlParserErrors::XmlErrValueRequired => "Entity value required",
        XmlParserErrors::XmlErrURIFragment => "Fragment not allowed",
        XmlParserErrors::XmlErrAttlistNotStarted => "'(' required to start ATTLIST enumeration",
        XmlParserErrors::XmlErrNmtokenRequired => "NmToken expected in ATTLIST enumeration",
        XmlParserErrors::XmlErrAttlistNotFinished => "')' required to finish ATTLIST enumeration",
        XmlParserErrors::XmlErrMixedNotStarted => "MixedContentDecl : '|' or ')*' expected",
        XmlParserErrors::XmlErrPCDATARequired => "MixedContentDecl : '#PCDATA' expected",
        XmlParserErrors::XmlErrElemcontentNotStarted => "ContentDecl : Name or '(' expected",
        XmlParserErrors::XmlErrElemcontentNotFinished => "ContentDecl : ',' '|' or ')' expected",
        XmlParserErrors::XmlErrPERefInIntSubset => {
            "PEReference: forbidden within markup decl in internal subset"
        }
        XmlParserErrors::XmlErrGtRequired => "expected '>'",
        XmlParserErrors::XmlErrCondsecInvalid => "XML conditional section '[' expected",
        XmlParserErrors::XmlErrExtSubsetNotFinished => "Content error in the external subset",
        XmlParserErrors::XmlErrCondsecInvalidKeyword => {
            "conditional section INCLUDE or IGNORE keyword expected"
        }
        XmlParserErrors::XmlErrCondsecNotFinished => "XML conditional section not closed",
        XmlParserErrors::XmlErrXMLDeclNotStarted => "Text declaration '<?xml' required",
        XmlParserErrors::XmlErrXMLDeclNotFinished => "parsing XML declaration: '?>' expected",
        XmlParserErrors::XmlErrExtEntityStandalone => {
            "external parsed entities cannot be standalone"
        }
        XmlParserErrors::XmlErrEntityRefSemicolMissing => "EntityRef: expecting ';'",
        XmlParserErrors::XmlErrDoctypeNotFinished => "DOCTYPE improperly terminated",
        XmlParserErrors::XmlErrLtSlashRequired => "EndTag: '</' not found",
        XmlParserErrors::XmlErrEqualRequired => "expected '='",
        XmlParserErrors::XmlErrStringNotClosed => "String not closed expecting \" or '",
        XmlParserErrors::XmlErrStringNotStarted => "String not started expecting ' or \"",
        XmlParserErrors::XmlErrEncodingName => "Invalid XML encoding name",
        XmlParserErrors::XmlErrStandaloneValue => "standalone accepts only 'yes' or 'no'",
        XmlParserErrors::XmlErrDocumentEmpty => "Document is empty",
        XmlParserErrors::XmlErrDocumentEnd => "Extra content at the end of the document",
        XmlParserErrors::XmlErrNotWellBalanced => "chunk is not well balanced",
        XmlParserErrors::XmlErrExtraContent => "extra content at the end of well balanced chunk",
        XmlParserErrors::XmlErrVersionMissing => "Malformed declaration expecting version",
        XmlParserErrors::XmlErrNameTooLong => "Name too long",
        _ => "Unregistered error message",
    };
    if !ctxt.is_null() {
        (*ctxt).err_no = error as i32;
    }
    if let Some(info) = info {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            None,
            XmlErrorDomain::XmlFromParser,
            error,
            XmlErrorLevel::XmlErrFatal,
            None,
            0,
            Some(info.to_owned().into()),
            None,
            None,
            0,
            0,
            "{}: {}\n",
            errmsg,
            info
        );
    } else {
        __xml_raise_error!(
            None,
            None,
            None,
            ctxt as _,
            None,
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
            "{}\n",
            errmsg
        );
    }
    if !ctxt.is_null() {
        (*ctxt).well_formed = 0;
        if (*ctxt).recovery == 0 {
            (*ctxt).disable_sax = 1;
        }
    }
}

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
        None,
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
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, $crate::libxml::parser::XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            $crate::error::__xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                None,
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
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        let mut schannel: Option<$crate::globals::StructuredError> = None;

        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, $crate::libxml::parser::XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null()
            {
                if let Some(sax) = (*ctxt).sax.as_deref().filter(|sax| sax.initialized == $crate::libxml::parser::XML_SAX2_MAGIC as u32) {
                    schannel = sax.serror;
                }
                $crate::error::__xml_raise_error!(
                    schannel,
                    (*ctxt).sax.as_deref_mut().and_then(|sax| sax.warning),
                    (*ctxt).user_data.clone(),
                    ctxt as _,
                    None,
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
                $crate::error::__xml_raise_error!(
                    schannel,
                    None,
                    None,
                    null_mut(),
                    None,
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
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            $crate::error::__xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                None,
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
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            $crate::error::__xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                None,
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
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
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
                $crate::error::__xml_raise_error!(
                    schannel,
                    (*ctxt).vctxt.error,
                    (*ctxt).vctxt.user_data.clone(),
                    ctxt as _,
                    None,
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
                $crate::error::__xml_raise_error!(
                    schannel,
                    None,
                    None,
                    ctxt as _,
                    None,
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
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            $crate::error::__xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                None,
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
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, $crate::libxml::parser::XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            $crate::error::__xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                None,
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
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, $crate::libxml::parser::XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $crate::error::XmlParserErrors::XmlErrInternalError as i32;
            }
            $crate::error::__xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                None,
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
            None,
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
            None,
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
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!((*ctxt).instate, $crate::libxml::parser::XmlParserInputState::XmlParserEOF)
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $xmlerr as _;
            }
            $crate::error::__xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                None,
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
        let ctxt = $ctxt as *mut $crate::parser::XmlParserCtxt;
        if ctxt.is_null()
            || (*ctxt).disable_sax == 0
            || !matches!(
                (*ctxt).instate,
                $crate::libxml::parser::XmlParserInputState::XmlParserEOF
            )
        {
            if !ctxt.is_null() {
                (*ctxt).err_no = $error as i32;
            }
            $crate::error::__xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                None,
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
            $crate::error::__xml_raise_error!(
                None,
                None,
                None,
                ctxt as _,
                None,
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

use super::XmlParserCtxtPtr;

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
            None,
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
            None,
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

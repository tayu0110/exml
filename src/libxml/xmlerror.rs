//! Provide methods and data structures for error handling.  
//! This module is based on `libxml/xmlerrors.h`, `errors.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

use std::ffi::{c_char, c_int, c_uint, c_void, CStr};
use std::io::Write;
use std::mem::{size_of, size_of_val};
use std::os::fd::FromRawFd;
use std::ptr::{addr_of_mut, null_mut};
use std::sync::atomic::{AtomicBool, Ordering};

use libc::{memcpy, memset, FILE};

use crate::error::generic_error_default;
use crate::libxml::parser::{XmlParserCtxtPtr, XmlParserInputPtr};

use super::globals::{xml_free, xml_generic_error, xml_generic_error_context, xml_last_error};
use super::tree::{XmlElementType, XmlNodePtr};
use super::xmlstring::{xml_get_utf8_char, xml_strdup, xml_strlen, XmlChar};

// #include "libxml.h"

/**
 * xmlErrorLevel:
 *
 * Indicates the level of an error
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlErrorLevel {
    XmlErrNone = 0,
    XmlErrWarning = 1, /* A simple warning */
    XmlErrError = 2,   /* A recoverable error */
    XmlErrFatal = 3,   /* A fatal error */
}

/**
 * xmlErrorDomain:
 *
 * Indicates where an error may have come from
 */
#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum XmlErrorDomain {
    XmlFromNone = 0,
    XmlFromParser,      /* The XML parser */
    XmlFromTree,        /* The tree module */
    XmlFromNamespace,   /* The XML Namespace module */
    XmlFromDtd,         /* The XML DTD validation with parser context*/
    XmlFromHtml,        /* The HTML parser */
    XmlFromMemory,      /* The memory allocator */
    XmlFromOutput,      /* The serialization code */
    XmlFromIO,          /* The Input/Output stack */
    XmlFromFtp,         /* The FTP module */
    XmlFromHttp,        /* The HTTP module */
    XmlFromXinclude,    /* The XInclude processing */
    XmlFromXpath,       /* The XPath module */
    XmlFromXpointer,    /* The XPointer module */
    XmlFromRegexp,      /* The regular expressions module */
    XmlFromDatatype,    /* The W3C XML Schemas Datatype module */
    XmlFromSchemasp,    /* The W3C XML Schemas parser module */
    XmlFromSchemasv,    /* The W3C XML Schemas validation module */
    XmlFromRelaxngp,    /* The Relax-NG parser module */
    XmlFromRelaxngv,    /* The Relax-NG validator module */
    XmlFromCatalog,     /* The Catalog module */
    XmlFromC14N,        /* The Canonicalization module */
    XmlFromXSLT,        /* The XSLT engine from libxslt */
    XmlFromValid,       /* The XML DTD validation with valid context */
    XmlFromCheck,       /* The error checking module */
    XmlFromWriter,      /* The xmlwriter module */
    XmlFromModule,      /* The dynamically loaded module module*/
    XmlFromI18N,        /* The module handling character conversion */
    XmlFromSchematronv, /* The Schematron validator module */
    XmlFromBuffer,      /* The buffers module */
    XmlFromUri,         /* The URI module */
}

impl TryFrom<i32> for XmlErrorDomain {
    type Error = anyhow::Error;
    fn try_from(value: i32) -> Result<Self, Self::Error> {
        if value == Self::XmlFromNone as i32 {
            Ok(Self::XmlFromNone)
        } else if value == Self::XmlFromParser as i32 {
            Ok(Self::XmlFromParser)
        } else if value == Self::XmlFromTree as i32 {
            Ok(Self::XmlFromTree)
        } else if value == Self::XmlFromNamespace as i32 {
            Ok(Self::XmlFromNamespace)
        } else if value == Self::XmlFromDtd as i32 {
            Ok(Self::XmlFromDtd)
        } else if value == Self::XmlFromHtml as i32 {
            Ok(Self::XmlFromHtml)
        } else if value == Self::XmlFromMemory as i32 {
            Ok(Self::XmlFromMemory)
        } else if value == Self::XmlFromOutput as i32 {
            Ok(Self::XmlFromOutput)
        } else if value == Self::XmlFromIO as i32 {
            Ok(Self::XmlFromIO)
        } else if value == Self::XmlFromFtp as i32 {
            Ok(Self::XmlFromFtp)
        } else if value == Self::XmlFromHttp as i32 {
            Ok(Self::XmlFromHttp)
        } else if value == Self::XmlFromXinclude as i32 {
            Ok(Self::XmlFromXinclude)
        } else if value == Self::XmlFromXpath as i32 {
            Ok(Self::XmlFromXpath)
        } else if value == Self::XmlFromXpointer as i32 {
            Ok(Self::XmlFromXpointer)
        } else if value == Self::XmlFromRegexp as i32 {
            Ok(Self::XmlFromRegexp)
        } else if value == Self::XmlFromDatatype as i32 {
            Ok(Self::XmlFromDatatype)
        } else if value == Self::XmlFromSchemasp as i32 {
            Ok(Self::XmlFromSchemasp)
        } else if value == Self::XmlFromSchemasv as i32 {
            Ok(Self::XmlFromSchemasv)
        } else if value == Self::XmlFromRelaxngp as i32 {
            Ok(Self::XmlFromRelaxngp)
        } else if value == Self::XmlFromRelaxngv as i32 {
            Ok(Self::XmlFromRelaxngv)
        } else if value == Self::XmlFromCatalog as i32 {
            Ok(Self::XmlFromCatalog)
        } else if value == Self::XmlFromC14N as i32 {
            Ok(Self::XmlFromC14N)
        } else if value == Self::XmlFromXSLT as i32 {
            Ok(Self::XmlFromXSLT)
        } else if value == Self::XmlFromValid as i32 {
            Ok(Self::XmlFromValid)
        } else if value == Self::XmlFromCheck as i32 {
            Ok(Self::XmlFromCheck)
        } else if value == Self::XmlFromWriter as i32 {
            Ok(Self::XmlFromWriter)
        } else if value == Self::XmlFromModule as i32 {
            Ok(Self::XmlFromModule)
        } else if value == Self::XmlFromI18N as i32 {
            Ok(Self::XmlFromI18N)
        } else if value == Self::XmlFromSchematronv as i32 {
            Ok(Self::XmlFromSchematronv)
        } else if value == Self::XmlFromBuffer as i32 {
            Ok(Self::XmlFromBuffer)
        } else if value == Self::XmlFromUri as i32 {
            Ok(Self::XmlFromUri)
        } else {
            Err(anyhow::anyhow!(
                "Invalid convert from value '{value}' to XmlErrorDomain"
            ))
        }
    }
}

/**
 * xmlError:
 *
 * An XML Error instance.
 */
pub type XmlErrorPtr = *mut XmlError;
#[repr(C)]
pub struct XmlError {
    pub domain: c_int,            /* What part of the library raised this error */
    pub code: c_int,              /* The error code, e.g. an xmlParserError */
    pub message: *mut c_char,     /* human-readable informative error message */
    pub level: XmlErrorLevel,     /* how consequent is the error */
    pub file: *mut c_char,        /* the filename */
    pub line: c_int,              /* the line number if available */
    pub str1: *mut c_char,        /* extra string information */
    pub(crate) str2: *mut c_char, /* extra string information */
    pub(crate) str3: *mut c_char, /* extra string information */
    pub int1: c_int,              /* extra number information */
    pub(crate) int2: c_int, /* error column # or 0 if N/A (todo: rename field when we would brk ABI) */
    pub ctxt: *mut c_void,  /* the parser context if available */
    pub node: *mut c_void,  /* the node in the tree */
}

/**
 * xmlParserError:
 *
 * This is an error that the XML (or HTML) parser can generate
 */
macro_rules! impl_xml_parser_errors {
    ( $( $variant:ident $( = $default:literal )? ),* ) => {
        #[repr(C)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub enum XmlParserErrors {
            $(
                $variant $( = $default )?
            ),*
        }

        impl TryFrom<i32> for XmlParserErrors {
            type Error = anyhow::Error;
            fn try_from(value: i32) -> Result<Self, Self::Error> {
                $(
                    if value == Self:: $variant as i32 {
                        return Ok(Self:: $variant);
                    }
                )*
                Err(anyhow::anyhow!("Invalid convert from value '{value}' to {}", std::any::type_name::<Self>()))
            }
        }

        impl Default for XmlParserErrors {
            fn default() -> Self {
                Self::XmlErrOK
            }
        }
    };
}
impl_xml_parser_errors!(
    XmlErrOK = 0,
    XmlErrInternalError,           /* 1 */
    XmlErrNoMemory,                /* 2 */
    XmlErrDocumentStart,           /* 3 */
    XmlErrDocumentEmpty,           /* 4 */
    XmlErrDocumentEnd,             /* 5 */
    XmlErrInvalidHexCharRef,       /* 6 */
    XmlErrInvalidDecCharRef,       /* 7 */
    XmlErrInvalidCharRef,          /* 8 */
    XmlErrInvalidChar,             /* 9 */
    XmlErrCharRefAtEOF,            /* 10 */
    XmlErrCharRefInProlog,         /* 11 */
    XmlErrCharRefInEpilog,         /* 12 */
    XmlErrCharRefInDTD,            /* 13 */
    XmlErrEntityRefAtEOF,          /* 14 */
    XmlErrEntityRefInProlog,       /* 15 */
    XmlErrEntityRefInEpilog,       /* 16 */
    XmlErrEntityRefInDTD,          /* 17 */
    XmlErrPERefAtEOF,              /* 18 */
    XmlErrPERefInProlog,           /* 19 */
    XmlErrPERefInEpilog,           /* 20 */
    XmlErrPERefInIntSubset,        /* 21 */
    XmlErrEntityRefNoName,         /* 22 */
    XmlErrEntityRefSemicolMissing, /* 23 */
    XmlErrPERefNoName,             /* 24 */
    XmlErrPERefSemicolMissing,     /* 25 */
    XmlErrUndeclaredEntity,        /* 26 */
    XmlWarUndeclaredEntity,        /* 27 */
    XmlErrUnparsedEntity,          /* 28 */
    XmlErrEntityIsExternal,        /* 29 */
    XmlErrEntityIsParameter,       /* 30 */
    XmlErrUnknownEncoding,         /* 31 */
    XmlErrUnsupportedEncoding,     /* 32 */
    XmlErrStringNotStarted,        /* 33 */
    XmlErrStringNotClosed,         /* 34 */
    XmlErrNsDeclError,             /* 35 */
    XmlErrEntityNotStarted,        /* 36 */
    XmlErrEntityNotFinished,       /* 37 */
    XmlErrLtInAttribute,           /* 38 */
    XmlErrAttributeNotStarted,     /* 39 */
    XmlErrAttributeNotFinished,    /* 40 */
    XmlErrAttributeWithoutValue,   /* 41 */
    XmlErrAttributeRedefined,      /* 42 */
    XmlErrLiteralNotStarted,       /* 43 */
    XmlErrLiteralNotFinished,      /* 44 */
    XmlErrCommentNotFinished,      /* 45 */
    XmlErrPINotStarted,            /* 46 */
    XmlErrPINotFinished,           /* 47 */
    XmlErrNotationNotStarted,      /* 48 */
    XmlErrNotationNotFinished,     /* 49 */
    XmlErrAttlistNotStarted,       /* 50 */
    XmlErrAttlistNotFinished,      /* 51 */
    XmlErrMixedNotStarted,         /* 52 */
    XmlErrMixedNotFinished,        /* 53 */
    XmlErrElemcontentNotStarted,   /* 54 */
    XmlErrElemcontentNotFinished,  /* 55 */
    XmlErrXMLDeclNotStarted,       /* 56 */
    XmlErrXMLDeclNotFinished,      /* 57 */
    XmlErrCondsecNotStarted,       /* 58 */
    XmlErrCondsecNotFinished,      /* 59 */
    XmlErrExtSubsetNotFinished,    /* 60 */
    XmlErrDoctypeNotFinished,      /* 61 */
    XmlErrMisplacedCdataEnd,       /* 62 */
    XmlErrCdataNotFinished,        /* 63 */
    XmlErrReservedXmlName,         /* 64 */
    XmlErrSpaceRequired,           /* 65 */
    XmlErrSeparatorRequired,       /* 66 */
    XmlErrNmtokenRequired,         /* 67 */
    XmlErrNameRequired,            /* 68 */
    XmlErrPcdataRequired,          /* 69 */
    XmlErrUriRequired,             /* 70 */
    XmlErrPubidRequired,           /* 71 */
    XmlErrLtRequired,              /* 72 */
    XmlErrGtRequired,              /* 73 */
    XmlErrLtslashRequired,         /* 74 */
    XmlErrEqualRequired,           /* 75 */
    XmlErrTagNameMismatch,         /* 76 */
    XmlErrTagNotFinished,          /* 77 */
    XmlErrStandaloneValue,         /* 78 */
    XmlErrEncodingName,            /* 79 */
    XmlErrHyphenInComment,         /* 80 */
    XmlErrInvalidEncoding,         /* 81 */
    XmlErrExtEntityStandalone,     /* 82 */
    XmlErrCondsecInvalid,          /* 83 */
    XmlErrValueRequired,           /* 84 */
    XmlErrNotWellBalanced,         /* 85 */
    XmlErrExtraContent,            /* 86 */
    XmlErrEntityCharError,         /* 87 */
    XmlErrEntityPeInternal,        /* 88 */
    XmlErrEntityLoop,              /* 89 */
    XmlErrEntityBoundary,          /* 90 */
    XmlErrInvalidUri,              /* 91 */
    XmlErrUriFragment,             /* 92 */
    XmlWarCatalogPI,               /* 93 */
    XmlErrNoDtd,                   /* 94 */
    XmlErrCondsecInvalidKeyword,   /* 95 */
    XmlErrVersionMissing,          /* 96 */
    XmlWarUnknownVersion,          /* 97 */
    XmlWarLangValue,               /* 98 */
    XmlWarNsUri,                   /* 99 */
    XmlWarNsUriRelative,           /* 100 */
    XmlErrMissingEncoding,         /* 101 */
    XmlWarSpaceValue,              /* 102 */
    XmlErrNotStandalone,           /* 103 */
    XmlErrEntityProcessing,        /* 104 */
    XmlErrNotationProcessing,      /* 105 */
    XmlWarNsColumn,                /* 106 */
    XmlWarEntityRedefined,         /* 107 */
    XmlErrUnknownVersion,          /* 108 */
    XmlErrVersionMismatch,         /* 109 */
    XmlErrNameTooLong,             /* 110 */
    XmlErrUserStop,                /* 111 */
    XmlErrCommentAbruptlyEnded,    /* 112 */
    XmlNsErrXmlNamespace = 200,
    XmlNsErrUndefinedNamespace, /* 201 */
    XmlNsErrQname,              /* 202 */
    XmlNsErrAttributeRedefined, /* 203 */
    XmlNsErrEmpty,              /* 204 */
    XmlNsErrColon,              /* 205 */
    XmlDtdAttributeDefault = 500,
    XmlDtdAttributeRedefined,    /* 501 */
    XmlDtdAttributeValue,        /* 502 */
    XmlDtdContentError,          /* 503 */
    XmlDtdContentModel,          /* 504 */
    XmlDtdContentNotDeterminist, /* 505 */
    XmlDtdDifferentPrefix,       /* 506 */
    XmlDtdElemDefaultNamespace,  /* 507 */
    XmlDtdElemNamespace,         /* 508 */
    XmlDtdElemRedefined,         /* 509 */
    XmlDtdEmptyNotation,         /* 510 */
    XmlDtdEntityType,            /* 511 */
    XmlDtdIdFixed,               /* 512 */
    XmlDtdIdRedefined,           /* 513 */
    XmlDtdIdSubset,              /* 514 */
    XmlDtdInvalidChild,          /* 515 */
    XmlDtdInvalidDefault,        /* 516 */
    XmlDtdLoadError,             /* 517 */
    XmlDtdMissingAttribute,      /* 518 */
    XmlDtdMixedCorrupt,          /* 519 */
    XmlDtdMultipleId,            /* 520 */
    XmlDtdNoDoc,                 /* 521 */
    XmlDtdNoDtd,                 /* 522 */
    XmlDtdNoElemName,            /* 523 */
    XmlDtdNoPrefix,              /* 524 */
    XmlDtdNoRoot,                /* 525 */
    XmlDtdNotationRedefined,     /* 526 */
    XmlDtdNotationValue,         /* 527 */
    XmlDtdNotEmpty,              /* 528 */
    XmlDtdNotPcdata,             /* 529 */
    XmlDtdNotStandalone,         /* 530 */
    XmlDtdRootName,              /* 531 */
    XmlDtdStandaloneWhiteSpace,  /* 532 */
    XmlDtdUnknownAttribute,      /* 533 */
    XmlDtdUnknownElem,           /* 534 */
    XmlDtdUnknownEntity,         /* 535 */
    XmlDtdUnknownId,             /* 536 */
    XmlDtdUnknownNotation,       /* 537 */
    XmlDtdStandaloneDefaulted,   /* 538 */
    XmlDtdXmlidValue,            /* 539 */
    XmlDtdXmlidType,             /* 540 */
    XmlDtdDupToken,              /* 541 */
    XmlHtmlStrucureError = 800,
    XmlHtmlUnknownTag,               /* 801 */
    XmlHtmlIncorrectlyOpenedComment, /* 802 */
    XmlRngpAnynameAttrAncestor = 1000,
    XmlRngpAttrConflict,             /* 1001 */
    XmlRngpAttributeChildren,        /* 1002 */
    XmlRngpAttributeContent,         /* 1003 */
    XmlRngpAttributeEmpty,           /* 1004 */
    XmlRngpAttributeNoop,            /* 1005 */
    XmlRngpChoiceContent,            /* 1006 */
    XmlRngpChoiceEmpty,              /* 1007 */
    XmlRngpCreateFailure,            /* 1008 */
    XmlRngpDataContent,              /* 1009 */
    XmlRngpDefChoiceAndInterleave,   /* 1010 */
    XmlRngpDefineCreateFailed,       /* 1011 */
    XmlRngpDefineEmpty,              /* 1012 */
    XmlRngpDefineMissing,            /* 1013 */
    XmlRngpDefineNameMissing,        /* 1014 */
    XmlRngpElemContentEmpty,         /* 1015 */
    XmlRngpElemContentError,         /* 1016 */
    XmlRngpElementEmpty,             /* 1017 */
    XmlRngpElementContent,           /* 1018 */
    XmlRngpElementName,              /* 1019 */
    XmlRngpElementNoContent,         /* 1020 */
    XmlRngpElemTextConflict,         /* 1021 */
    XmlRngpEmpty,                    /* 1022 */
    XmlRngpEmptyConstruct,           /* 1023 */
    XmlRngpEmptyContent,             /* 1024 */
    XmlRngpEmptyNotEmpty,            /* 1025 */
    XmlRngpErrorTypeLib,             /* 1026 */
    XmlRngpExceptEmpty,              /* 1027 */
    XmlRngpExceptMissing,            /* 1028 */
    XmlRngpExceptMultiple,           /* 1029 */
    XmlRngpExceptNoContent,          /* 1030 */
    XmlRngpExternalrefEmtpy,         /* 1031 */
    XmlRngpExternalRefFailure,       /* 1032 */
    XmlRngpExternalrefRecurse,       /* 1033 */
    XmlRngpForbiddenAttribute,       /* 1034 */
    XmlRngpForeignElement,           /* 1035 */
    XmlRngpGrammarContent,           /* 1036 */
    XmlRngpGrammarEmpty,             /* 1037 */
    XmlRngpGrammarMissing,           /* 1038 */
    XmlRngpGrammarNoStart,           /* 1039 */
    XmlRngpGroupAttrConflict,        /* 1040 */
    XmlRngpHrefError,                /* 1041 */
    XmlRngpIncludeEmpty,             /* 1042 */
    XmlRngpIncludeFailure,           /* 1043 */
    XmlRngpIncludeRecurse,           /* 1044 */
    XmlRngpInterleaveAdd,            /* 1045 */
    XmlRngpInterleaveCreateFailed,   /* 1046 */
    XmlRngpInterleaveEmpty,          /* 1047 */
    XmlRngpInterleaveNoContent,      /* 1048 */
    XmlRngpInvalidDefineName,        /* 1049 */
    XmlRngpInvalidUri,               /* 1050 */
    XmlRngpInvalidValue,             /* 1051 */
    XmlRngpMissingHref,              /* 1052 */
    XmlRngpNameMissing,              /* 1053 */
    XmlRngpNeedCombine,              /* 1054 */
    XmlRngpNotallowedNotEmpty,       /* 1055 */
    XmlRngpNsnameAttrAncestor,       /* 1056 */
    XmlRngpNsnameNoNs,               /* 1057 */
    XmlRngpParamForbidden,           /* 1058 */
    XmlRngpParamNameMissing,         /* 1059 */
    XmlRngpParentrefCreateFailed,    /* 1060 */
    XmlRngpParentrefNameInvalid,     /* 1061 */
    XmlRngpParentrefNoName,          /* 1062 */
    XmlRngpParentrefNoParent,        /* 1063 */
    XmlRngpParentrefNotEmpty,        /* 1064 */
    XmlRngpParseError,               /* 1065 */
    XmlRngpPatAnynameExceptAnyname,  /* 1066 */
    XmlRngpPatAttrAttr,              /* 1067 */
    XmlRngpPatAttrElem,              /* 1068 */
    XmlRngpPatDataExceptAttr,        /* 1069 */
    XmlRngpPatDataExceptElem,        /* 1070 */
    XmlRngpPatDataExceptEmpty,       /* 1071 */
    XmlRngpPatDataExceptGroup,       /* 1072 */
    XmlRngpPatDataExceptInterleave,  /* 1073 */
    XmlRngpPatDataExceptList,        /* 1074 */
    XmlRngpPatDataExceptOnemore,     /* 1075 */
    XmlRngpPatDataExceptRef,         /* 1076 */
    XmlRngpPatDataExceptText,        /* 1077 */
    XmlRngpPatListAttr,              /* 1078 */
    XmlRngpPatListElem,              /* 1079 */
    XmlRngpPatListInterleave,        /* 1080 */
    XmlRngpPatListList,              /* 1081 */
    XmlRngpPatListRef,               /* 1082 */
    XmlRngpPatListText,              /* 1083 */
    XmlRngpPatNsnameExceptAnyname,   /* 1084 */
    XmlRngpPatNsnameExceptNsname,    /* 1085 */
    XmlRngpPatOnemoreGroupAttr,      /* 1086 */
    XmlRngpPatOnemoreInterleaveAttr, /* 1087 */
    XmlRngpPatStartAttr,             /* 1088 */
    XmlRngpPatStartData,             /* 1089 */
    XmlRngpPatStartEmpty,            /* 1090 */
    XmlRngpPatStartGroup,            /* 1091 */
    XmlRngpPatStartInterleave,       /* 1092 */
    XmlRngpPatStartList,             /* 1093 */
    XmlRngpPatStartOnemore,          /* 1094 */
    XmlRngpPatStartText,             /* 1095 */
    XmlRngpPatStartValue,            /* 1096 */
    XmlRngpPrefixUndefined,          /* 1097 */
    XmlRngpRefCreateFailed,          /* 1098 */
    XmlRngpRefCycle,                 /* 1099 */
    XmlRngpRefNameInvalid,           /* 1100 */
    XmlRngpRefNoDef,                 /* 1101 */
    XmlRngpRefNoName,                /* 1102 */
    XmlRngpRefNotEmpty,              /* 1103 */
    XmlRngpStartChoiceAndInterleave, /* 1104 */
    XmlRngpStartContent,             /* 1105 */
    XmlRngpStartEmpty,               /* 1106 */
    XmlRngpStartMissing,             /* 1107 */
    XmlRngpTextExpected,             /* 1108 */
    XmlRngpTextHasChild,             /* 1109 */
    XmlRngpTypeMissing,              /* 1110 */
    XmlRngpTypeNotFound,             /* 1111 */
    XmlRngpTypeValue,                /* 1112 */
    XmlRngpUnknownAttribute,         /* 1113 */
    XmlRngpUnknownCombine,           /* 1114 */
    XmlRngpUnknownConstruct,         /* 1115 */
    XmlRngpUnknownTypeLib,           /* 1116 */
    XmlRngpUriFragment,              /* 1117 */
    XmlRngpUriNotAbsolute,           /* 1118 */
    XmlRngpValueEmpty,               /* 1119 */
    XmlRngpValueNoContent,           /* 1120 */
    XmlRngpXmlnsName,                /* 1121 */
    XmlRngpXmlNs,                    /* 1122 */
    XmlXpathExpressionOk = 1200,
    XmlXpathNumberError,            /* 1201 */
    XmlXpathUnfinishedLiteralError, /* 1202 */
    XmlXpathStartLiteralError,      /* 1203 */
    XmlXpathVariableRefError,       /* 1204 */
    XmlXpathUndefVariableError,     /* 1205 */
    XmlXpathInvalidPredicateError,  /* 1206 */
    XmlXpathExprError,              /* 1207 */
    XmlXpathUnclosedError,          /* 1208 */
    XmlXpathUnknownFuncError,       /* 1209 */
    XmlXpathInvalidOperand,         /* 1210 */
    XmlXpathInvalidType,            /* 1211 */
    XmlXpathInvalidArity,           /* 1212 */
    XmlXpathInvalidCtxtSize,        /* 1213 */
    XmlXpathInvalidCtxtPosition,    /* 1214 */
    XmlXpathMemoryError,            /* 1215 */
    XmlXptrSyntaxError,             /* 1216 */
    XmlXptrResourceError,           /* 1217 */
    XmlXptrSubResourceError,        /* 1218 */
    XmlXpathUndefPrefixError,       /* 1219 */
    XmlXpathEncodingError,          /* 1220 */
    XmlXpathInvalidCharError,       /* 1221 */
    XmlXpathInvalidCtxt,
    XmlXpathStackError,
    XmlXpathForbidVariableError,
    XmlXpathOpLimitExceeded,
    XmlXpathRecursionLimitExceeded,
    XmlXPathUnknownError, // 1227, this must put at the end of XPath errors
    XmlTreeInvalidHex = 1300,
    XmlTreeInvalidDec,         /* 1301 */
    XmlTreeUnterminatedEntity, /* 1302 */
    XmlTreeNotUtf8,            /* 1303 */
    XmlSaveNotUtf8 = 1400,
    XmlSaveCharInvalid,     /* 1401 */
    XmlSaveNoDoctype,       /* 1402 */
    XmlSaveUnknownEncoding, /* 1403 */
    XmlRegexpCompileError = 1450,
    XmlIoUnknown = 1500,
    XmlIoEacces,         /* 1501 */
    XmlIoEagain,         /* 1502 */
    XmlIoEbadf,          /* 1503 */
    XmlIoEbadmsg,        /* 1504 */
    XmlIoEbusy,          /* 1505 */
    XmlIoEcanceled,      /* 1506 */
    XmlIoEchild,         /* 1507 */
    XmlIoEdeadlk,        /* 1508 */
    XmlIoEdom,           /* 1509 */
    XmlIoEexist,         /* 1510 */
    XmlIoEfault,         /* 1511 */
    XmlIoEfbig,          /* 1512 */
    XmlIoEinprogress,    /* 1513 */
    XmlIoEintr,          /* 1514 */
    XmlIoEinval,         /* 1515 */
    XmlIoEio,            /* 1516 */
    XmlIoEisdir,         /* 1517 */
    XmlIoEmfile,         /* 1518 */
    XmlIoEmlink,         /* 1519 */
    XmlIoEmsgsize,       /* 1520 */
    XmlIoEnametoolong,   /* 1521 */
    XmlIoEnfile,         /* 1522 */
    XmlIoEnodev,         /* 1523 */
    XmlIoEnoent,         /* 1524 */
    XmlIoEnoexec,        /* 1525 */
    XmlIoEnolck,         /* 1526 */
    XmlIoEnomem,         /* 1527 */
    XmlIoEnospc,         /* 1528 */
    XmlIoEnosys,         /* 1529 */
    XmlIoEnotdir,        /* 1530 */
    XmlIoEnotempty,      /* 1531 */
    XmlIoEnotsup,        /* 1532 */
    XmlIoEnotty,         /* 1533 */
    XmlIoEnxio,          /* 1534 */
    XmlIoEperm,          /* 1535 */
    XmlIoEpipe,          /* 1536 */
    XmlIoErange,         /* 1537 */
    XmlIoErofs,          /* 1538 */
    XmlIoEspipe,         /* 1539 */
    XmlIoEsrch,          /* 1540 */
    XmlIoEtimedout,      /* 1541 */
    XmlIoExdev,          /* 1542 */
    XmlIoNetworkAttempt, /* 1543 */
    XmlIoEncoder,        /* 1544 */
    XmlIoFlush,          /* 1545 */
    XmlIoWrite,          /* 1546 */
    XmlIoNoInput,        /* 1547 */
    XmlIoBufferFull,     /* 1548 */
    XmlIoLoadError,      /* 1549 */
    XmlIoEnotsock,       /* 1550 */
    XmlIoEisconn,        /* 1551 */
    XmlIoEconnrefused,   /* 1552 */
    XmlIoEnetunreach,    /* 1553 */
    XmlIoEaddrinuse,     /* 1554 */
    XmlIoEalready,       /* 1555 */
    XmlIoEafnosupport,   /* 1556 */
    XmlXincludeRecursion = 1600,
    XmlXincludeParseValue,           /* 1601 */
    XmlXincludeEntityDefMismatch,    /* 1602 */
    XmlXincludeNoHref,               /* 1603 */
    XmlXincludeNoFallback,           /* 1604 */
    XmlXincludeHrefUri,              /* 1605 */
    XmlXincludeTextFragment,         /* 1606 */
    XmlXincludeTextDocument,         /* 1607 */
    XmlXincludeInvalidChar,          /* 1608 */
    XmlXincludeBuildFailed,          /* 1609 */
    XmlXincludeUnknownEncoding,      /* 1610 */
    XmlXincludeMultipleRoot,         /* 1611 */
    XmlXincludeXptrFailed,           /* 1612 */
    XmlXincludeXptrResult,           /* 1613 */
    XmlXincludeIncludeInInclude,     /* 1614 */
    XmlXincludeFallbacksInInclude,   /* 1615 */
    XmlXincludeFallbackNotInInclude, /* 1616 */
    XmlXincludeDeprecatedNs,         /* 1617 */
    XmlXincludeFragmentId,           /* 1618 */
    XmlCatalogMissingAttr = 1650,
    XmlCatalogEntryBroken, /* 1651 */
    XmlCatalogPreferValue, /* 1652 */
    XmlCatalogNotCatalog,  /* 1653 */
    XmlCatalogRecursion,   /* 1654 */
    XmlSchemapPrefixUndefined = 1700,
    XmlSchemapAttrformdefaultValue,         /* 1701 */
    XmlSchemapAttrgrpNonameNoref,           /* 1702 */
    XmlSchemapAttrNonameNoref,              /* 1703 */
    XmlSchemapComplextypeNonameNoref,       /* 1704 */
    XmlSchemapElemformdefaultValue,         /* 1705 */
    XmlSchemapElemNonameNoref,              /* 1706 */
    XmlSchemapExtensionNoBase,              /* 1707 */
    XmlSchemapFacetNoValue,                 /* 1708 */
    XmlSchemapFailedBuildImport,            /* 1709 */
    XmlSchemapGroupNonameNoref,             /* 1710 */
    XmlSchemapImportNamespaceNotUri,        /* 1711 */
    XmlSchemapImportRedefineNsname,         /* 1712 */
    XmlSchemapImportSchemaNotUri,           /* 1713 */
    XmlSchemapInvalidBoolean,               /* 1714 */
    XmlSchemapInvalidEnum,                  /* 1715 */
    XmlSchemapInvalidFacet,                 /* 1716 */
    XmlSchemapInvalidFacetValue,            /* 1717 */
    XmlSchemapInvalidMaxoccurs,             /* 1718 */
    XmlSchemapInvalidMinoccurs,             /* 1719 */
    XmlSchemapInvalidRefAndSubtype,         /* 1720 */
    XmlSchemapInvalidWhiteSpace,            /* 1721 */
    XmlSchemapNoattrNoref,                  /* 1722 */
    XmlSchemapNotationNoName,               /* 1723 */
    XmlSchemapNotypeNoref,                  /* 1724 */
    XmlSchemapRefAndSubtype,                /* 1725 */
    XmlSchemapRestrictionNonameNoref,       /* 1726 */
    XmlSchemapSimpletypeNoname,             /* 1727 */
    XmlSchemapTypeAndSubtype,               /* 1728 */
    XmlSchemapUnknownAllChild,              /* 1729 */
    XmlSchemapUnknownAnyattributeChild,     /* 1730 */
    XmlSchemapUnknownAttrChild,             /* 1731 */
    XmlSchemapUnknownAttrgrpChild,          /* 1732 */
    XmlSchemapUnknownAttributeGroup,        /* 1733 */
    XmlSchemapUnknownBaseType,              /* 1734 */
    XmlSchemapUnknownChoiceChild,           /* 1735 */
    XmlSchemapUnknownComplexcontentChild,   /* 1736 */
    XmlSchemapUnknownComplextypeChild,      /* 1737 */
    XmlSchemapUnknownElemChild,             /* 1738 */
    XmlSchemapUnknownExtensionChild,        /* 1739 */
    XmlSchemapUnknownFacetChild,            /* 1740 */
    XmlSchemapUnknownFacetType,             /* 1741 */
    XmlSchemapUnknownGroupChild,            /* 1742 */
    XmlSchemapUnknownImportChild,           /* 1743 */
    XmlSchemapUnknownListChild,             /* 1744 */
    XmlSchemapUnknownNotationChild,         /* 1745 */
    XmlSchemapUnknownProcesscontentChild,   /* 1746 */
    XmlSchemapUnknownRef,                   /* 1747 */
    XmlSchemapUnknownRestrictionChild,      /* 1748 */
    XmlSchemapUnknownSchemasChild,          /* 1749 */
    XmlSchemapUnknownSequenceChild,         /* 1750 */
    XmlSchemapUnknownSimplecontentChild,    /* 1751 */
    XmlSchemapUnknownSimpletypeChild,       /* 1752 */
    XmlSchemapUnknownType,                  /* 1753 */
    XmlSchemapUnknownUnionChild,            /* 1754 */
    XmlSchemapElemDefaultFixed,             /* 1755 */
    XmlSchemapRegexpInvalid,                /* 1756 */
    XmlSchemapFailedLoad,                   /* 1757 */
    XmlSchemapNothingToParse,               /* 1758 */
    XmlSchemapNoroot,                       /* 1759 */
    XmlSchemapRedefinedGroup,               /* 1760 */
    XmlSchemapRedefinedType,                /* 1761 */
    XmlSchemapRedefinedElement,             /* 1762 */
    XmlSchemapRedefinedAttrgroup,           /* 1763 */
    XmlSchemapRedefinedAttr,                /* 1764 */
    XmlSchemapRedefinedNotation,            /* 1765 */
    XmlSchemapFailedParse,                  /* 1766 */
    XmlSchemapUnknownPrefix,                /* 1767 */
    XmlSchemapDefAndPrefix,                 /* 1768 */
    XmlSchemapUnknownIncludeChild,          /* 1769 */
    XmlSchemapIncludeSchemaNotUri,          /* 1770 */
    XmlSchemapIncludeSchemaNoUri,           /* 1771 */
    XmlSchemapNotSchema,                    /* 1772 */
    XmlSchemapUnknownMemberType,            /* 1773 */
    XmlSchemapInvalidAttrUse,               /* 1774 */
    XmlSchemapRecursive,                    /* 1775 */
    XmlSchemapSupernumerousListItemType,    /* 1776 */
    XmlSchemapInvalidAttrCombination,       /* 1777 */
    XmlSchemapInvalidAttrInlineCombination, /* 1778 */
    XmlSchemapMissingSimpletypeChild,       /* 1779 */
    XmlSchemapInvalidAttrName,              /* 1780 */
    XmlSchemapRefAndContent,                /* 1781 */
    XmlSchemapCtPropsCorrect1,              /* 1782 */
    XmlSchemapCtPropsCorrect2,              /* 1783 */
    XmlSchemapCtPropsCorrect3,              /* 1784 */
    XmlSchemapCtPropsCorrect4,              /* 1785 */
    XmlSchemapCtPropsCorrect5,              /* 1786 */
    XmlSchemapDerivationOkRestriction1,     /* 1787 */
    XmlSchemapDerivationOkRestriction2_1_1, /* 1788 */
    XmlSchemapDerivationOkRestriction2_1_2, /* 1789 */
    XmlSchemapDerivationOkRestriction2_2,   /* 1790 */
    XmlSchemapDerivationOkRestriction3,     /* 1791 */
    XmlSchemapWildcardInvalidNsMember,      /* 1792 */
    XmlSchemapIntersectionNotExpressible,   /* 1793 */
    XmlSchemapUnionNotExpressible,          /* 1794 */
    XmlSchemapSrcImport3_1,                 /* 1795 */
    XmlSchemapSrcImport3_2,                 /* 1796 */
    XmlSchemapDerivationOkRestriction4_1,   /* 1797 */
    XmlSchemapDerivationOkRestriction4_2,   /* 1798 */
    XmlSchemapDerivationOkRestriction4_3,   /* 1799 */
    XmlSchemapCosCtExtends1_3,              /* 1800 */
    XmlSchemavNoroot = 1801,
    XmlSchemavUndeclaredelem,         /* 1802 */
    XmlSchemavNottoplevel,            /* 1803 */
    XmlSchemavMissing,                /* 1804 */
    XmlSchemavWrongelem,              /* 1805 */
    XmlSchemavNotype,                 /* 1806 */
    XmlSchemavNorollback,             /* 1807 */
    XmlSchemavIsabstract,             /* 1808 */
    XmlSchemavNotempty,               /* 1809 */
    XmlSchemavElemcont,               /* 1810 */
    XmlSchemavHavedefault,            /* 1811 */
    XmlSchemavNotnillable,            /* 1812 */
    XmlSchemavExtracontent,           /* 1813 */
    XmlSchemavInvalidattr,            /* 1814 */
    XmlSchemavInvalidelem,            /* 1815 */
    XmlSchemavNotdeterminist,         /* 1816 */
    XmlSchemavConstruct,              /* 1817 */
    XmlSchemavInternal,               /* 1818 */
    XmlSchemavNotsimple,              /* 1819 */
    XmlSchemavAttrunknown,            /* 1820 */
    XmlSchemavAttrinvalid,            /* 1821 */
    XmlSchemavValue,                  /* 1822 */
    XmlSchemavFacet,                  /* 1823 */
    XmlSchemavCvcDatatypeValid1_2_1,  /* 1824 */
    XmlSchemavCvcDatatypeValid1_2_2,  /* 1825 */
    XmlSchemavCvcDatatypeValid1_2_3,  /* 1826 */
    XmlSchemavCvcType3_1_1,           /* 1827 */
    XmlSchemavCvcType3_1_2,           /* 1828 */
    XmlSchemavCvcFacetValid,          /* 1829 */
    XmlSchemavCvcLengthValid,         /* 1830 */
    XmlSchemavCvcMinlengthValid,      /* 1831 */
    XmlSchemavCvcMaxlengthValid,      /* 1832 */
    XmlSchemavCvcMininclusiveValid,   /* 1833 */
    XmlSchemavCvcMaxinclusiveValid,   /* 1834 */
    XmlSchemavCvcMinexclusiveValid,   /* 1835 */
    XmlSchemavCvcMaxexclusiveValid,   /* 1836 */
    XmlSchemavCvcTotaldigitsValid,    /* 1837 */
    XmlSchemavCvcFractiondigitsValid, /* 1838 */
    XmlSchemavCvcPatternValid,        /* 1839 */
    XmlSchemavCvcEnumerationValid,    /* 1840 */
    XmlSchemavCvcComplexType2_1,      /* 1841 */
    XmlSchemavCvcComplexType2_2,      /* 1842 */
    XmlSchemavCvcComplexType2_3,      /* 1843 */
    XmlSchemavCvcComplexType2_4,      /* 1844 */
    XmlSchemavCvcElt1,                /* 1845 */
    XmlSchemavCvcElt2,                /* 1846 */
    XmlSchemavCvcElt3_1,              /* 1847 */
    XmlSchemavCvcElt3_2_1,            /* 1848 */
    XmlSchemavCvcElt3_2_2,            /* 1849 */
    XmlSchemavCvcElt4_1,              /* 1850 */
    XmlSchemavCvcElt4_2,              /* 1851 */
    XmlSchemavCvcElt4_3,              /* 1852 */
    XmlSchemavCvcElt5_1_1,            /* 1853 */
    XmlSchemavCvcElt5_1_2,            /* 1854 */
    XmlSchemavCvcElt5_2_1,            /* 1855 */
    XmlSchemavCvcElt5_2_2_1,          /* 1856 */
    XmlSchemavCvcElt5_2_2_2_1,        /* 1857 */
    XmlSchemavCvcElt5_2_2_2_2,        /* 1858 */
    XmlSchemavCvcElt6,                /* 1859 */
    XmlSchemavCvcElt7,                /* 1860 */
    XmlSchemavCvcAttribute1,          /* 1861 */
    XmlSchemavCvcAttribute2,          /* 1862 */
    XmlSchemavCvcAttribute3,          /* 1863 */
    XmlSchemavCvcAttribute4,          /* 1864 */
    XmlSchemavCvcComplexType3_1,      /* 1865 */
    XmlSchemavCvcComplexType3_2_1,    /* 1866 */
    XmlSchemavCvcComplexType3_2_2,    /* 1867 */
    XmlSchemavCvcComplexType4,        /* 1868 */
    XmlSchemavCvcComplexType5_1,      /* 1869 */
    XmlSchemavCvcComplexType5_2,      /* 1870 */
    XmlSchemavElementContent,         /* 1871 */
    XmlSchemavDocumentElementMissing, /* 1872 */
    XmlSchemavCvcComplexType1,        /* 1873 */
    XmlSchemavCvcAu,                  /* 1874 */
    XmlSchemavCvcType1,               /* 1875 */
    XmlSchemavCvcType2,               /* 1876 */
    XmlSchemavCvcIdc,                 /* 1877 */
    XmlSchemavCvcWildcard,            /* 1878 */
    XmlSchemavMisc,                   /* 1879 */
    XmlXptrUnknownScheme = 1900,
    XmlXptrChildseqStart, /* 1901 */
    XmlXptrEvalFailed,    /* 1902 */
    XmlXptrExtraObjects,  /* 1903 */
    XmlC14nCreateCtxt = 1950,
    XmlC14nRequiresUtf8,      /* 1951 */
    XmlC14nCreateStack,       /* 1952 */
    XmlC14nInvalidNode,       /* 1953 */
    XmlC14nUnknowNode,        /* 1954 */
    XmlC14nRelativeNamespace, /* 1955 */
    XmlFtpPasvAnswer = 2000,
    XmlFtpEpsvAnswer, /* 2001 */
    XmlFtpAccnt,      /* 2002 */
    XmlFtpUrlSyntax,  /* 2003 */
    XmlHttpUrlSyntax = 2020,
    XmlHttpUseIp,       /* 2021 */
    XmlHttpUnknownHost, /* 2022 */
    XmlSchemapSrcSimpleType1 = 3000,
    XmlSchemapSrcSimpleType2,                   /* 3001 */
    XmlSchemapSrcSimpleType3,                   /* 3002 */
    XmlSchemapSrcSimpleType4,                   /* 3003 */
    XmlSchemapSrcResolve,                       /* 3004 */
    XmlSchemapSrcRestrictionBaseOrSimpletype,   /* 3005 */
    XmlSchemapSrcListItemtypeOrSimpletype,      /* 3006 */
    XmlSchemapSrcUnionMembertypesOrSimpletypes, /* 3007 */
    XmlSchemapStPropsCorrect1,                  /* 3008 */
    XmlSchemapStPropsCorrect2,                  /* 3009 */
    XmlSchemapStPropsCorrect3,                  /* 3010 */
    XmlSchemapCosStRestricts1_1,                /* 3011 */
    XmlSchemapCosStRestricts1_2,                /* 3012 */
    XmlSchemapCosStRestricts1_3_1,              /* 3013 */
    XmlSchemapCosStRestricts1_3_2,              /* 3014 */
    XmlSchemapCosStRestricts2_1,                /* 3015 */
    XmlSchemapCosStRestricts2_3_1_1,            /* 3016 */
    XmlSchemapCosStRestricts2_3_1_2,            /* 3017 */
    XmlSchemapCosStRestricts2_3_2_1,            /* 3018 */
    XmlSchemapCosStRestricts2_3_2_2,            /* 3019 */
    XmlSchemapCosStRestricts2_3_2_3,            /* 3020 */
    XmlSchemapCosStRestricts2_3_2_4,            /* 3021 */
    XmlSchemapCosStRestricts2_3_2_5,            /* 3022 */
    XmlSchemapCosStRestricts3_1,                /* 3023 */
    XmlSchemapCosStRestricts3_3_1,              /* 3024 */
    XmlSchemapCosStRestricts3_3_1_2,            /* 3025 */
    XmlSchemapCosStRestricts3_3_2_2,            /* 3026 */
    XmlSchemapCosStRestricts3_3_2_1,            /* 3027 */
    XmlSchemapCosStRestricts3_3_2_3,            /* 3028 */
    XmlSchemapCosStRestricts3_3_2_4,            /* 3029 */
    XmlSchemapCosStRestricts3_3_2_5,            /* 3030 */
    XmlSchemapCosStDerivedOk2_1,                /* 3031 */
    XmlSchemapCosStDerivedOk2_2,                /* 3032 */
    XmlSchemapS4sElemNotAllowed,                /* 3033 */
    XmlSchemapS4sElemMissing,                   /* 3034 */
    XmlSchemapS4sAttrNotAllowed,                /* 3035 */
    XmlSchemapS4sAttrMissing,                   /* 3036 */
    XmlSchemapS4sAttrInvalidValue,              /* 3037 */
    XmlSchemapSrcElement1,                      /* 3038 */
    XmlSchemapSrcElement2_1,                    /* 3039 */
    XmlSchemapSrcElement2_2,                    /* 3040 */
    XmlSchemapSrcElement3,                      /* 3041 */
    XmlSchemapPPropsCorrect1,                   /* 3042 */
    XmlSchemapPPropsCorrect2_1,                 /* 3043 */
    XmlSchemapPPropsCorrect2_2,                 /* 3044 */
    XmlSchemapEPropsCorrect2,                   /* 3045 */
    XmlSchemapEPropsCorrect3,                   /* 3046 */
    XmlSchemapEPropsCorrect4,                   /* 3047 */
    XmlSchemapEPropsCorrect5,                   /* 3048 */
    XmlSchemapEPropsCorrect6,                   /* 3049 */
    XmlSchemapSrcInclude,                       /* 3050 */
    XmlSchemapSrcAttribute1,                    /* 3051 */
    XmlSchemapSrcAttribute2,                    /* 3052 */
    XmlSchemapSrcAttribute3_1,                  /* 3053 */
    XmlSchemapSrcAttribute3_2,                  /* 3054 */
    XmlSchemapSrcAttribute4,                    /* 3055 */
    XmlSchemapNoXmlns,                          /* 3056 */
    XmlSchemapNoXsi,                            /* 3057 */
    XmlSchemapCosValidDefault1,                 /* 3058 */
    XmlSchemapCosValidDefault2_1,               /* 3059 */
    XmlSchemapCosValidDefault2_2_1,             /* 3060 */
    XmlSchemapCosValidDefault2_2_2,             /* 3061 */
    XmlSchemapCvcSimpleType,                    /* 3062 */
    XmlSchemapCosCtExtends1_1,                  /* 3063 */
    XmlSchemapSrcImport1_1,                     /* 3064 */
    XmlSchemapSrcImport1_2,                     /* 3065 */
    XmlSchemapSrcImport2,                       /* 3066 */
    XmlSchemapSrcImport2_1,                     /* 3067 */
    XmlSchemapSrcImport2_2,                     /* 3068 */
    XmlSchemapInternal,                         /* 3069 non-W3C */
    XmlSchemapNotDeterministic,                 /* 3070 non-W3C */
    XmlSchemapSrcAttributeGroup1,               /* 3071 */
    XmlSchemapSrcAttributeGroup2,               /* 3072 */
    XmlSchemapSrcAttributeGroup3,               /* 3073 */
    XmlSchemapMgPropsCorrect1,                  /* 3074 */
    XmlSchemapMgPropsCorrect2,                  /* 3075 */
    XmlSchemapSrcCt1,                           /* 3076 */
    XmlSchemapDerivationOkRestriction2_1_3,     /* 3077 */
    XmlSchemapAuPropsCorrect2,                  /* 3078 */
    XmlSchemapAPropsCorrect2,                   /* 3079 */
    XmlSchemapCPropsCorrect,                    /* 3080 */
    XmlSchemapSrcRedefine,                      /* 3081 */
    XmlSchemapSrcImport,                        /* 3082 */
    XmlSchemapWarnSkipSchema,                   /* 3083 */
    XmlSchemapWarnUnlocatedSchema,              /* 3084 */
    XmlSchemapWarnAttrRedeclProh,               /* 3085 */
    XmlSchemapWarnAttrPointlessProh,            /* 3085 */
    XmlSchemapAgPropsCorrect,                   /* 3086 */
    XmlSchemapCosCtExtends1_2,                  /* 3087 */
    XmlSchemapAuPropsCorrect,                   /* 3088 */
    XmlSchemapAPropsCorrect3,                   /* 3089 */
    XmlSchemapCosAllLimited,                    /* 3090 */
    XmlSchematronvAssert = 4000,                /* 4000 */
    XmlSchematronvReport,
    XmlModuleOpen = 4900, /* 4900 */
    XmlModuleClose,       /* 4901 */
    XmlCheckFoundElement = 5000,
    XmlCheckFoundAttribute, /* 5001 */
    XmlCheckFoundText,      /* 5002 */
    XmlCheckFoundCdata,     /* 5003 */
    XmlCheckFoundEntityref, /* 5004 */
    XmlCheckFoundEntity,    /* 5005 */
    XmlCheckFoundPi,        /* 5006 */
    XmlCheckFoundComment,   /* 5007 */
    XmlCheckFoundDoctype,   /* 5008 */
    XmlCheckFoundFragment,  /* 5009 */
    XmlCheckFoundNotation,  /* 5010 */
    XmlCheckUnknownNode,    /* 5011 */
    XmlCheckEntityType,     /* 5012 */
    XmlCheckNoParent,       /* 5013 */
    XmlCheckNoDoc,          /* 5014 */
    XmlCheckNoName,         /* 5015 */
    XmlCheckNoElem,         /* 5016 */
    XmlCheckWrongDoc,       /* 5017 */
    XmlCheckNoPrev,         /* 5018 */
    XmlCheckWrongPrev,      /* 5019 */
    XmlCheckNoNext,         /* 5020 */
    XmlCheckWrongNext,      /* 5021 */
    XmlCheckNotDtd,         /* 5022 */
    XmlCheckNotAttr,        /* 5023 */
    XmlCheckNotAttrDecl,    /* 5024 */
    XmlCheckNotElemDecl,    /* 5025 */
    XmlCheckNotEntityDecl,  /* 5026 */
    XmlCheckNotNsDecl,      /* 5027 */
    XmlCheckNoHref,         /* 5028 */
    XmlCheckWrongParent,    /* 5029 */
    XmlCheckNsScope,        /* 5030 */
    XmlCheckNsAncestor,     /* 5031 */
    XmlCheckNotUtf8,        /* 5032 */
    XmlCheckNoDict,         /* 5033 */
    XmlCheckNotNcname,      /* 5034 */
    XmlCheckOutsideDict,    /* 5035 */
    XmlCheckWrongName,      /* 5036 */
    XmlCheckNameNotNull,    /* 5037 */
    XmlI18nNoName = 6000,
    XmlI18nNoHandler,     /* 6001 */
    XmlI18nExcessHandler, /* 6002 */
    XmlI18nConvFailed,    /* 6003 */
    XmlI18nNoOutput,      /* 6004 */
    XmlBufOverflow = 7000
);

impl XmlParserErrors {
    pub fn is_ok(&self) -> bool {
        *self == Self::XmlErrOK
    }
}

/**
 * xmlGenericErrorFunc:
 * @ctx:  a parsing context
 * @msg:  the message
 * @...:  the extra arguments of the varargs to format the message
 *
 * Signature of the function to use when there is an error and
 * no parsing or validity context available .
 */
pub type XmlGenericErrorFunc = unsafe extern "C" fn(ctx: *mut c_void, msg: *const c_char);
/**
 * xmlStructuredErrorFunc:
 * @userData:  user provided data for the error callback
 * @error:  the error being raised.
 *
 * Signature of the function to use when there is an error and
 * the module handles the new error reporting mechanism.
 */
pub type XmlStructuredErrorFunc = unsafe extern "C" fn(userData: *mut c_void, error: XmlErrorPtr);

/**
 * xmlGenericErrorDefaultFunc:
 * @ctx:  an error context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Default handler for out of context error messages.
 */
pub(crate) unsafe extern "C" fn xml_generic_error_default_func(
    _ctx: *mut c_void,
    msg: *const c_char,
) {
    let msg = CStr::from_ptr(msg).to_string_lossy();
    if xml_generic_error_context().is_null() {
        generic_error_default(None::<&mut dyn Write>, &msg);
    } else {
        let fp = xml_generic_error_context() as *mut FILE;
        // The buffers of C/Rust stdio are separated,
        // so unless do `fflush`, the order of outputs may be strange.
        libc::fflush(fp);
        let fd = libc::fileno(fp);
        let mut file = std::fs::File::from_raw_fd(fd);
        generic_error_default(Some(&mut file), &msg);
        // For the same reason, do `std::io::Write::flush`.
        file.flush().ok();
    }
}

// /*
//  * Use the following function to reset the two global variables
//  * xml_generic_error and xmlGenericErrorContext.
//  */
// /**
//  * xmlSetGenericErrorFunc:
//  * @ctx:  the new error handling context
//  * @handler:  the new handler function
//  *
//  * Function to reset the handler and the error context for out of
//  * context error messages.
//  * This simply means that @handler will be called for subsequent
//  * error messages while not parsing nor validating. And @ctx will
//  * be passed as first argument to @handler
//  * One can simply force messages to be emitted to another FILE * than
//  * stderr by setting @ctx to this file handle and @handler to NULL.
//  * For multi-threaded applications, this must be set separately for each thread.
//  */
// pub unsafe extern "C" fn xml_set_generic_error_func(
//     ctx: *mut c_void,
//     handler: Option<XmlGenericErrorFunc>,
// ) {
//     if xml_is_main_thread() != 0 {
//         _XML_GENERIC_ERROR_CONTEXT.store(ctx, Ordering::Relaxed);
//         _XML_GENERIC_ERROR = handler.or(Some(xml_generic_error_default_func));
//     } else {
//         (*xml_get_global_state())
//             .xml_generic_error_context
//             .store(ctx, Ordering::Relaxed);
//         (*xml_get_global_state()).xml_generic_error =
//             handler.or(Some(xml_generic_error_default_func));
//     }
//     set_generic_error(
//         Some({
//             let mut handler = handler.unwrap_or(xml_generic_error_default_func);
//             let ptr_value = addr_of_mut!(handler) as usize;
//             let ptr = ptr_value as *const fn(Option<&mut (dyn Write + 'static)>, &str);
//             *ptr
//         }),
//         (!ctx.is_null()).then(|| {
//             let file = ctx as *mut FILE;
//             fflush(file);
//             let fd = fileno(file);
//             File::from_raw_fd(fd)
//         }),
//     );
// }

// /**
//  * initGenericErrorDefaultFunc:
//  * @handler:  the handler
//  *
//  * DEPRECATED: Use xmlSetGenericErrorFunc.
//  *
//  * Set or reset (if NULL) the default handler for generic errors
//  * to the builtin error function.
//  */
// #[deprecated]
// pub unsafe extern "C" fn init_generic_error_default_func(handler: Option<XmlGenericErrorFunc>) {
//     _XML_GENERIC_ERROR = handler.or(Some(xml_generic_error_default_func));
// }

// /**
//  * xmlSetStructuredErrorFunc:
//  * @ctx:  the new error handling context
//  * @handler:  the new handler function
//  *
//  * Function to reset the handler and the error context for out of
//  * context structured error messages.
//  * This simply means that @handler will be called for subsequent
//  * error messages while not parsing nor validating. And @ctx will
//  * be passed as first argument to @handler
//  * For multi-threaded applications, this must be set separately for each thread.
//  */
// pub unsafe extern "C" fn xml_set_structured_error_func(
//     ctx: *mut c_void,
//     handler: Option<XmlStructuredErrorFunc>,
// ) {
//     if xml_is_main_thread() != 0 {
//         _XML_STRUCTURED_ERROR_CONTEXT.store(ctx, Ordering::Relaxed);
//         _XML_STRUCTURED_ERROR = handler;
//     } else {
//         (*xml_get_global_state())
//             .xml_structured_error_context
//             .store(ctx, Ordering::Relaxed);
//         (*xml_get_global_state()).xml_structured_error = handler;
//     }
// }

pub const XML_MAX_ERRORS: usize = 100;

#[macro_export]
macro_rules! XML_GET_VAR_STR {
    ( $msg:expr, $str:expr, $( $args:expr ),* ) => {
        let mut size: libc::c_int;
        let mut prev_size: libc::c_int = -1;
        let mut chars: libc::c_int;
        let mut larger: *mut libc::c_char;

        $str = $crate::libxml::globals::xml_malloc(150) as *mut libc::c_char;
        if !$str.is_null() {
            size = 150;

            while size < 64000 {
    	        chars = libc::snprintf($str, size as usize, $msg, $( $args ),*);
    	        if chars > -1 && chars < size {
    	            if prev_size == chars {
    	        	    break;
    	            } else {
    	        	    prev_size = chars;
    	            }
    	        }
    	        if chars > -1 {
    	            size += chars + 1;
                } else {
    	            size += 100;
                }
    	        if {
                    larger = $crate::libxml::globals::xml_realloc($str as _, size as usize) as *mut libc::c_char;
                    larger.is_null()
                 } {
    	            break;
    	        }
    	        $str = larger;
            }
        }
    }
}

#[macro_export]
macro_rules! xml_generic_error {
    ( $ctx:expr, $msg:expr, $( $args:expr ),* ) => {
        $crate::xml_error_with_format!($crate::libxml::globals::xml_generic_error, $ctx, $msg, $( $args ),*);
    };
    ( $ctx:expr, $msg:expr ) => {
        $crate::xml_error_with_format!($crate::libxml::globals::xml_generic_error, $ctx, $msg)
    }
}

#[macro_export]
macro_rules! xml_error_with_format {
    ( $err_func:expr, $ctx:expr, $msg:expr, $( $args:expr ),* ) => {
        (|ctx: *mut libc::c_void, msg: *const libc::c_char| {
            let mut str: *mut libc::c_char;

            $crate::XML_GET_VAR_STR!(msg, str, $( $args ),*);
            $err_func(ctx, str as _);
            if !str.is_null() {
                $crate::libxml::globals::xml_free(str as _);
            }
        })($ctx, $msg)
    };
    ( $err_func:expr, $ctx:expr, $msg:expr ) => {
        $crate::xml_error_with_format!($err_func, $ctx, $msg, )
    }
}

/*
 * Default message routines used by SAX and Valid context for error
 * and warning reporting.
 */
/**
 * xmlParserError:
 * @ctx:  an XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format an error messages, gives file, line, position and
 * extra parameters.
 */
// pub unsafe extern "C" fn xmlParserError(ctx: *mut c_void, msg: *const c_char, ...) {
// }
// #[macro_export]
// macro_rules! xmlParserError {
//     ( $ctx:expr, $msg:expr, $( $args:expr ),* ) => {
//         (|ctx: *mut libc::c_void, msg: *const libc::c_char| {
//             let mut str: *mut libc::c_char;
//             $crate::XML_GET_VAR_STR!(msg, str, $( $args ),*);
//             $crate::libxml::xmlerror::xmlParserError(ctx, str as _);
//             if !str.is_null() {
//                 $crate::libxml::globals::xml_free(str as _);
//             }
//         })($ctx, $msg)
//     };
// }
// pub(crate) unsafe extern "C" fn xml_parser_error(ctx: *mut c_void, msg: *const c_char) {
//     let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;
//     let mut input: XmlParserInputPtr = null_mut();
//     let mut cur: XmlParserInputPtr = null_mut();

//     if !ctxt.is_null() {
//         input = (*ctxt).input;
//         if !input.is_null() && (*input).filename.is_null() && (*ctxt).input_nr > 1 {
//             cur = input;
//             input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
//         }
//         xml_parser_print_file_info(input);
//     }

//     xml_generic_error(xml_generic_error_context(), c"error: ".as_ptr() as _);
//     xml_generic_error(xml_generic_error_context(), msg as _);

//     if !ctxt.is_null() {
//         xml_parser_print_file_context(input);
//         if !cur.is_null() {
//             xml_parser_print_file_info(cur);
//             xml_generic_error(xml_generic_error_context(), c"\n".as_ptr() as _);
//             xml_parser_print_file_context(cur);
//         }
//     }
// }

/**
 * xmlParserWarning:
 * @ctx:  an XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format a warning messages, gives file, line, position and
 * extra parameters.
 */
// pub unsafe extern "C" fn xmlParserWarning(ctx: *mut c_void, msg: *const c_char, ...) {
// }
#[macro_export]
macro_rules! xmlParserWarning {
    ( $ctx:expr, $msg:expr, $( $args:expr ),* ) => {
        (|ctx: *mut libc::c_void, msg: *const libc::c_char| {
            let mut str: *mut libc::c_char;
            $crate::XML_GET_VAR_STR!(msg, str, $( $args ),*);
            $crate::libxml::xmlerror::xmlParserWarning(ctx, str as _);
            if !str.is_null() {
                $crate::libxml::globals::xml_free(str as _);
            }
        })($ctx, $msg)
    };
}

pub(crate) unsafe extern "C" fn xml_parser_warning(ctx: *mut c_void, msg: *const c_char) {
    let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;
    let mut input: XmlParserInputPtr = null_mut();
    let mut cur: XmlParserInputPtr = null_mut();

    if !ctxt.is_null() {
        input = (*ctxt).input;
        if !input.is_null() && (*input).filename.is_null() && (*ctxt).input_nr > 1 {
            cur = input;
            input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
        }
        xml_parser_print_file_info(input);
    }

    xml_generic_error(xml_generic_error_context(), c"warning: ".as_ptr() as _);
    xml_generic_error(xml_generic_error_context(), msg as _);

    if !ctxt.is_null() {
        xml_parser_print_file_context(input);
        if !cur.is_null() {
            xml_parser_print_file_info(cur);
            xml_generic_error(xml_generic_error_context(), c"\n".as_ptr() as _);
            xml_parser_print_file_context(cur);
        }
    }
}

/**
 * xmlParserValidityError:
 * @ctx:  an XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format an validity error messages, gives file,
 * line, position and extra parameters.
 */
// pub unsafe extern "C" fn xmlParserValidityError(ctx: *mut c_void, msg: *const c_char, ...) {
// }
#[macro_export]
macro_rules! xmlParserValidityError {
    ( $ctx:expr, $msg:expr, $( $args:expr ),* ) => {
        (|ctx: *mut libc::c_void, msg: *const libc::c_char| {
            let mut str: *mut libc::c_char;
            $crate::XML_GET_VAR_STR!(msg, str, $( $args ),*);
            $crate::libxml::xmlerror::xmlParserValidityError(ctx, str);
            if !str.is_null() {
                $crate::libxml::globals::xml_free(str as _);
            }
        })($ctx, $msg)
    };
}

pub(crate) unsafe extern "C" fn xml_parser_validity_error(ctx: *mut c_void, msg: *const c_char) {
    let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;
    let mut input: XmlParserInputPtr = std::ptr::null_mut();
    let len: libc::c_int = xml_strlen(msg as *const XmlChar);
    static HAD_INFO: AtomicBool = AtomicBool::new(false);

    if len > 1 && *msg.add(len as usize - 2) != b':' as i8 {
        if !ctxt.is_null() {
            input = (*ctxt).input;
            if (*input).filename.is_null() && (*ctxt).input_nr > 1 {
                input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
            }

            if !HAD_INFO.load(Ordering::Acquire) {
                xml_parser_print_file_info(input);
            }
        }
        xml_generic_error(
            xml_generic_error_context(),
            c"validity error: ".as_ptr() as _,
        );
        HAD_INFO.store(false, Ordering::Release)
    } else {
        HAD_INFO.store(true, Ordering::Release);
    }

    xml_generic_error(xml_generic_error_context(), msg as _);

    if !ctxt.is_null() && !input.is_null() {
        xml_parser_print_file_context(input);
    }
}

/**
 * xmlParserValidityWarning:
 * @ctx:  an XML parser context
 * @msg:  the message to display/transmit
 * @...:  extra parameters for the message display
 *
 * Display and format a validity warning messages, gives file, line,
 * position and extra parameters.
 */
// pub unsafe extern "C" fn xmlParserValidityWarning(ctx: *mut c_void, msg: *const c_char, ...) {
// }
#[macro_export]
macro_rules! xmlParserValidityWarning {
    ( $ctx:expr, $msg:expr, $( $args:expr ),* ) => {
        (|ctx: *mut libc::c_void, msg: *const libc::c_char| {
            let mut str: *mut libc::c_char;
            $crate::XML_GET_VAR_STR!(msg, str, $( $args ),*);
            $crate::libxml::xmlerror::xmlParserValidityWarning(ctx, str);
            if !str.is_null() {
                $crate::libxml::globals::xml_free(str);
            }
        })($ctx, $msg)
    };
}

pub(crate) unsafe extern "C" fn xml_parser_validity_warning(ctx: *mut c_void, msg: *const c_char) {
    let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;
    let mut input: XmlParserInputPtr = std::ptr::null_mut();
    let len: c_int = xml_strlen(msg as *const XmlChar);

    if !ctxt.is_null() && len != 0 && *msg.add(len as usize - 1) != b':' as i8 {
        input = (*ctxt).input;
        if (*input).filename.is_null() && (*ctxt).input_nr > 1 {
            input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
        }

        xml_parser_print_file_info(input);
    }

    xml_generic_error(
        xml_generic_error_context(),
        c"validity warning: ".as_ptr() as _,
    );
    xml_generic_error(xml_generic_error_context(), msg as _);

    if !ctxt.is_null() {
        xml_parser_print_file_context(input);
    }
}

/**
 * xmlParserPrintFileContextInternal:
 * @input:  an xmlParserInputPtr input
 *
 * Displays current context within the input content for error tracking
 */
// static void
// xmlParserPrintFileContextInternal(xmlParserInputPtr input ,
// 		xmlGenericErrorFunc channel, void *data ) {
unsafe extern "C" fn xml_parser_print_file_context_internal(
    input: XmlParserInputPtr,
    channel: XmlGenericErrorFunc,
    data: *mut c_void,
) {
    let mut n: c_uint;
    let mut cur: *const XmlChar;
    let mut content: [XmlChar; 81] = [0; 81]; /* space for 80 chars + line terminator */
    let mut ctnt: *mut XmlChar;

    if input.is_null() || (*input).cur.is_null() {
        return;
    }

    cur = (*input).cur;
    let base: *const XmlChar = (*input).base;
    /* skip backwards over any end-of-lines */
    while cur > base && (*(cur) == b'\n' || *(cur) == b'\r') {
        cur = cur.offset(-1);
    }
    n = 0;
    /* search backwards for beginning-of-line (to max buff size) */
    while (n as usize) < size_of_val(&content) - 1 && cur > base && *cur != b'\n' && *cur != b'\r' {
        cur = cur.offset(-1);
        n += 1;
    }
    if n > 0 && (*cur == b'\n' || *cur == b'\r') {
        cur = cur.add(1);
    } else {
        /* skip over continuation bytes */
        while cur < (*input).cur && *cur & 0xC0 == 0x80 {
            cur = cur.add(1);
        }
    }
    /* calculate the error position in terms of the current position */
    let col: c_uint = (*input).cur.offset_from(cur) as _;
    /* search forward for end-of-line (to max buff size) */
    n = 0;
    let start: *const XmlChar = cur;
    /* copy selected text to our buffer */
    while *cur != 0 && *(cur) != b'\n' && *(cur) != b'\r' {
        let mut len: c_int = (*input).end.offset_from(cur) as _;
        let c: c_int = xml_get_utf8_char(cur, addr_of_mut!(len));

        if c < 0 || n as usize + len as usize > size_of_val(&content) - 1 {
            break;
        }
        cur = cur.add(len as usize);
        n += len as u32;
    }
    memcpy(content.as_mut_ptr() as _, start as _, n as usize);
    content[n as usize] = 0;
    /* print out the selected text */
    // channel(data, c"%s\n".as_ptr() as _, content);
    xml_error_with_format!(
        channel,
        data,
        c"%s\n".as_ptr() as _,
        content.as_ptr() as *const c_char
    );
    /* create blank line with problem pointer */
    n = 0;
    ctnt = content.as_mut_ptr();
    /* (leave buffer space for pointer + line terminator) */
    while {
        let f = n < col && (n as usize) < size_of_val(&content) - 2 && *ctnt != 0;
        n += 1;
        f
    } {
        if *(ctnt) != b'\t' {
            *(ctnt) = b' ';
        }
        ctnt = ctnt.add(1);
    }
    *ctnt = b'^';
    ctnt = ctnt.add(1);
    *ctnt = 0;
    // channel(data, c"%s\n".as_ptr() as _, content);
    xml_error_with_format!(channel, c"%s\n".as_ptr() as _, content.as_ptr() as _);
}

/**
 * xmlParserPrintFileInfo:
 * @input:  an xmlParserInputPtr input
 *
 * Displays the associated file and line information for the current input
 */
pub unsafe extern "C" fn xml_parser_print_file_info(input: XmlParserInputPtr) {
    if !input.is_null() {
        if !(*input).filename.is_null() {
            // xml_generic_error(xmlGenericErrorContext, c"%s:%d: ".as_ptr() as _, (*input).filename, (*input).line);
            xml_error_with_format!(
                xml_generic_error,
                xml_generic_error_context(),
                c"%s:%d: ".as_ptr() as _,
                (*input).filename,
                (*input).line
            );
        } else {
            // xml_generic_error(xmlGenericErrorContext, c"Entity: line %d: ".as_ptr() as _, (*input).line);
            xml_error_with_format!(
                xml_generic_error,
                xml_generic_error_context(),
                c"Entity: line %d: ".as_ptr() as _,
                (*input).line
            );
        }
    }
}

/**
 * xmlParserPrintFileContext:
 * @input:  an xmlParserInputPtr input
 *
 * Displays current context within the input content for error tracking
 */
pub unsafe extern "C" fn xml_parser_print_file_context(input: XmlParserInputPtr) {
    xml_parser_print_file_context_internal(input, xml_generic_error, xml_generic_error_context());
}

/*
 * Extended error information routines
 */
/**
 * xmlGetLastError:
 *
 * Get the last global error registered. This is per thread if compiled
 * with thread support.
 *
 * Returns NULL if no error occurred or a pointer to the error
 */
pub unsafe extern "C" fn xml_get_last_error() -> XmlErrorPtr {
    if (*xml_last_error()).code == XmlParserErrors::XmlErrOK as _ {
        return null_mut();
    }
    xml_last_error()
}

// /**
//  * xmlResetLastError:
//  *
//  * Cleanup the last global error registered. For parsing error
//  * this does not change the well-formedness result.
//  */
// pub unsafe extern "C" fn xml_reset_last_error() {
//     if (*xml_last_error()).code == XmlParserErrors::XmlErrOK as _ {
//         return;
//     }
//     xml_reset_error(xml_last_error());
// }

/**
 * xmlCtxtGetLastError:
 * @ctx:  an XML parser context
 *
 * Get the last parsing error registered.
 *
 * Returns NULL if no error occurred or a pointer to the error
 */
// pub unsafe extern "C" fn xml_ctxt_get_last_error(ctx: *mut c_void) -> XmlErrorPtr {
//     let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;

//     if ctxt.is_null() {
//         return null_mut();
//     }
//     if (*ctxt).last_error.code == XmlParserErrors::XmlErrOK as i32 {
//         return null_mut();
//     }
//     addr_of_mut!((*ctxt).last_error)
// }

/**
 * xmlCtxtResetLastError:
 * @ctx:  an XML parser context
 *
 * Cleanup the last global error registered. For parsing error
 * this does not change the well-formedness result.
 */
// pub unsafe extern "C" fn xml_ctxt_reset_last_error(ctx: *mut c_void) {
//     let ctxt: XmlParserCtxtPtr = ctx as XmlParserCtxtPtr;

//     if ctxt.is_null() {
//         return;
//     }
//     (*ctxt).err_no = XmlParserErrors::XmlErrOK as i32;
//     if (*ctxt).last_error.code == XmlParserErrors::XmlErrOK as i32 {
//         return;
//     }
//     xml_reset_error(addr_of_mut!((*ctxt).last_error));
// }

/**
 * xmlResetError:
 * @err: pointer to the error.
 *
 * Cleanup the error.
 */
pub unsafe extern "C" fn xml_reset_error(err: XmlErrorPtr) {
    if err.is_null() {
        return;
    }
    if (*err).code == XmlParserErrors::XmlErrOK as i32 {
        return;
    }
    if !(*err).message.is_null() {
        xml_free((*err).message as _);
    }
    if !(*err).file.is_null() {
        xml_free((*err).file as _);
    }
    if !(*err).str1.is_null() {
        xml_free((*err).str1 as _);
    }
    if !(*err).str2.is_null() {
        xml_free((*err).str2 as _);
    }
    if !(*err).str3.is_null() {
        xml_free((*err).str3 as _);
    }
    memset(err as _, 0, size_of::<XmlError>());
    (*err).code = XmlParserErrors::XmlErrOK as _;
}

/**
 * xmlCopyError:
 * @from:  a source error
 * @to:  a target error
 *
 * Save the original error to the new place.
 *
 * Returns 0 in case of success and -1 in case of error.
 */
pub unsafe extern "C" fn xml_copy_error(from: XmlErrorPtr, to: XmlErrorPtr) -> c_int {
    if from.is_null() || to.is_null() {
        return -1;
    }

    let message: *mut c_char = xml_strdup((*from).message as *mut XmlChar) as *mut c_char;
    let file: *mut c_char = xml_strdup((*from).file as *mut XmlChar) as *mut c_char;
    let str1: *mut c_char = xml_strdup((*from).str1 as *mut XmlChar) as *mut c_char;
    let str2: *mut c_char = xml_strdup((*from).str2 as *mut XmlChar) as *mut c_char;
    let str3: *mut c_char = xml_strdup((*from).str3 as *mut XmlChar) as *mut c_char;

    if !(*to).message.is_null() {
        xml_free((*to).message as _);
    }
    if !(*to).file.is_null() {
        xml_free((*to).file as _);
    }
    if !(*to).str1.is_null() {
        xml_free((*to).str1 as _);
    }
    if !(*to).str2.is_null() {
        xml_free((*to).str2 as _);
    }
    if !(*to).str3.is_null() {
        xml_free((*to).str3 as _);
    }
    (*to).domain = (*from).domain;
    (*to).code = (*from).code;
    (*to).level = (*from).level;
    (*to).line = (*from).line;
    (*to).node = (*from).node;
    (*to).int1 = (*from).int1;
    (*to).int2 = (*from).int2;
    (*to).node = (*from).node;
    (*to).ctxt = (*from).ctxt;
    (*to).message = message;
    (*to).file = file;
    (*to).str1 = str1;
    (*to).str2 = str2;
    (*to).str3 = str3;

    0
}

/**
 * xmlReportError:
 * @err: the error
 * @ctx: the parser context or NULL
 * @str: the formatted error message
 *
 * Report an error with its context, replace the 4 old error/warning
 * routines.
 */
pub unsafe extern "C" fn xml_report_error(
    err: XmlErrorPtr,
    ctxt: XmlParserCtxtPtr,
    str: *const c_char,
    mut channel: Option<XmlGenericErrorFunc>,
    mut data: *mut c_void,
) {
    let mut name: *const XmlChar = null_mut();

    let mut input: XmlParserInputPtr = null_mut();
    let mut cur: XmlParserInputPtr = null_mut();

    if err.is_null() {
        return;
    }

    if channel.is_none() {
        channel = Some(xml_generic_error);
        data = xml_generic_error_context();
    }
    let file: *mut c_char = (*err).file;
    let line: c_int = (*err).line;
    let code: c_int = (*err).code;
    let domain: c_int = (*err).domain;
    let level: XmlErrorLevel = (*err).level;
    let node: XmlNodePtr = (*err).node as _;

    if code == XmlParserErrors::XmlErrOK as i32 {
        return;
    }

    if !node.is_null() && matches!((*node).typ, XmlElementType::XmlElementNode) {
        name = (*node).name;
    }

    /*
     * Maintain the compatibility with the legacy error handling
     */
    let channel = channel.unwrap();
    if !ctxt.is_null() {
        input = (*ctxt).input;
        if !input.is_null() && (*input).filename.is_null() && (*ctxt).input_nr > 1 {
            cur = input;
            input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
        }
        if !input.is_null() {
            if !(*input).filename.is_null() {
                xml_error_with_format!(
                    channel,
                    data,
                    c"%s:%d: ".as_ptr() as _,
                    (*input).filename,
                    (*input).line
                );
            } else if line != 0 && domain == XmlErrorDomain::XmlFromParser as i32 {
                xml_error_with_format!(
                    channel,
                    data,
                    c"Entity: line %d: ".as_ptr() as _,
                    (*input).line
                );
            }
        }
    } else if !file.is_null() {
        xml_error_with_format!(channel, data, c"%s:%d: ".as_ptr() as _, file, line);
    } else if line != 0
        && (domain == XmlErrorDomain::XmlFromParser as i32
            || domain == XmlErrorDomain::XmlFromSchemasv as i32
            || domain == XmlErrorDomain::XmlFromSchemasp as i32
            || domain == XmlErrorDomain::XmlFromDtd as i32
            || domain == XmlErrorDomain::XmlFromRelaxngp as i32
            || domain == XmlErrorDomain::XmlFromRelaxngv as i32)
    {
        xml_error_with_format!(channel, data, c"Entity: line %d: ".as_ptr() as _, line);
    }
    if !name.is_null() {
        xml_error_with_format!(channel, data, c"element %s: ".as_ptr() as _, name);
    }
    match XmlErrorDomain::try_from(domain) {
        Ok(XmlErrorDomain::XmlFromParser) => {
            xml_error_with_format!(channel, data, c"parser ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromNamespace) => {
            xml_error_with_format!(channel, data, c"namespace ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromDtd) | Ok(XmlErrorDomain::XmlFromValid) => {
            xml_error_with_format!(channel, data, c"validity ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromHtml) => {
            xml_error_with_format!(channel, data, c"HTML parser ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromMemory) => {
            xml_error_with_format!(channel, data, c"memory ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromOutput) => {
            xml_error_with_format!(channel, data, c"output ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromIO) => {
            xml_error_with_format!(channel, data, c"I/O ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromXinclude) => {
            xml_error_with_format!(channel, data, c"XInclude ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromXpath) => {
            xml_error_with_format!(channel, data, c"XPath ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromXpointer) => {
            xml_error_with_format!(channel, data, c"parser ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromRegexp) => {
            xml_error_with_format!(channel, data, c"regexp ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromModule) => {
            xml_error_with_format!(channel, data, c"module ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromSchemasv) => {
            xml_error_with_format!(channel, data, c"Schemas validity ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromSchemasp) => {
            xml_error_with_format!(channel, data, c"Schemas parser ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromRelaxngp) => {
            xml_error_with_format!(channel, data, c"Relax-NG parser ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromRelaxngv) => {
            xml_error_with_format!(channel, data, c"Relax-NG validity ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromCatalog) => {
            xml_error_with_format!(channel, data, c"Catalog ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromC14N) => {
            xml_error_with_format!(channel, data, c"C14N ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromXSLT) => {
            xml_error_with_format!(channel, data, c"XSLT ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromI18N) => {
            xml_error_with_format!(channel, data, c"encoding ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromSchematronv) => {
            xml_error_with_format!(channel, data, c"schematron ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromBuffer) => {
            xml_error_with_format!(channel, data, c"internal buffer ".as_ptr() as _);
        }
        Ok(XmlErrorDomain::XmlFromUri) => {
            xml_error_with_format!(channel, data, c"URI ".as_ptr() as _);
        }
        _ => {}
    }
    match level {
        XmlErrorLevel::XmlErrNone => {
            xml_error_with_format!(channel, data, c": ".as_ptr() as _);
        }
        XmlErrorLevel::XmlErrWarning => {
            xml_error_with_format!(channel, data, c"warning : ".as_ptr() as _);
        }
        XmlErrorLevel::XmlErrError => {
            xml_error_with_format!(channel, data, c"error : ".as_ptr() as _);
        }
        XmlErrorLevel::XmlErrFatal => {
            xml_error_with_format!(channel, data, c"error : ".as_ptr() as _);
        }
    }
    if !str.is_null() {
        let len: c_int = xml_strlen(str as *const XmlChar);
        if len > 0 && *str.add(len as usize - 1) != b'\n' as _ {
            xml_error_with_format!(channel, data, c"%s\n".as_ptr() as _, str);
        } else {
            xml_error_with_format!(channel, data, c"%s".as_ptr() as _, str);
        }
    } else {
        xml_error_with_format!(
            channel,
            data,
            c"%s\n".as_ptr() as _,
            c"out of memory error".as_ptr()
        );
    }

    if !ctxt.is_null() {
        xml_parser_print_file_context_internal(input, channel, data);
        if !cur.is_null() {
            if !(*cur).filename.is_null() {
                xml_error_with_format!(
                    channel,
                    data,
                    c"%s:%d: \n".as_ptr() as _,
                    (*cur).filename,
                    (*cur).line
                );
            } else if line != 0 && domain == XmlErrorDomain::XmlFromParser as i32 {
                xml_error_with_format!(
                    channel,
                    data,
                    c"Entity: line %d: \n".as_ptr() as _,
                    (*cur).line
                );
            }
            xml_parser_print_file_context_internal(cur, channel, data);
        }
    }
    if domain == XmlErrorDomain::XmlFromXpath as i32
        && !(*err).str1.is_null()
        && ((*err).int1 < 100)
        && (*err).int1 < xml_strlen((*err).str1 as *const XmlChar)
    {
        let mut buf: [XmlChar; 150] = [0; 150];
        xml_error_with_format!(channel, data, c"%s\n".as_ptr() as _, (*err).str1);
        for b in buf.iter_mut().take((*err).int1 as usize) {
            *b = b' ';
        }
        buf[(*err).int1 as usize] = b'^';
        buf[(*err).int1 as usize + 1] = 0;
        xml_error_with_format!(channel, data, c"%s\n".as_ptr() as _, buf);
    }
}

#[cfg(test)]
mod tests {
    use crate::globals::reset_last_error;
    use crate::libxml::xmlmemory::xml_mem_blocks;
    use crate::test_util::*;

    use super::*;

    // #[test]
    // fn test_init_generic_error_default_func() {
    //     unsafe {
    //         let mut leaks = 0;

    //         for n_handler in 0..GEN_NB_XML_GENERIC_ERROR_FUNC_PTR {
    //             let mem_base = xml_mem_blocks();
    //             let handler = gen_xml_generic_error_func_ptr(n_handler, 0);

    //             init_generic_error_default_func(handler);
    //             des_xml_generic_error_func_ptr(n_handler, handler, 0);
    //             reset_last_error();
    //             if mem_base != xml_mem_blocks() {
    //                 leaks += 1;
    //                 eprint!(
    //                     "Leak of {} blocks found in initGenericErrorDefaultFunc",
    //                     xml_mem_blocks() - mem_base
    //                 );
    //                 assert!(
    //                     leaks == 0,
    //                     "{leaks} Leaks are found in initGenericErrorDefaultFunc()"
    //                 );
    //                 eprintln!(" {}", n_handler);
    //             }
    //         }
    //     }
    // }

    #[test]
    fn test_xml_copy_error() {
        unsafe {
            let mut leaks = 0;

            for n_from in 0..GEN_NB_XML_ERROR_PTR {
                for n_to in 0..GEN_NB_XML_ERROR_PTR {
                    let mem_base = xml_mem_blocks();
                    let from = gen_xml_error_ptr(n_from, 0);
                    let to = gen_xml_error_ptr(n_to, 1);

                    let ret_val = xml_copy_error(from, to);
                    desret_int(ret_val);
                    des_xml_error_ptr(n_from, from, 0);
                    des_xml_error_ptr(n_to, to, 1);
                    reset_last_error();
                    if mem_base != xml_mem_blocks() {
                        leaks += 1;
                        eprint!(
                            "Leak of {} blocks found in xmlCopyError",
                            xml_mem_blocks() - mem_base
                        );
                        assert!(leaks == 0, "{leaks} Leaks are found in xmlCopyError()");
                        eprint!(" {}", n_from);
                        eprintln!(" {}", n_to);
                    }
                }
            }
        }
    }

    #[test]
    fn test_xml_ctxt_get_last_error() {

        /* missing type support */
    }

    // #[test]
    // fn test_xml_ctxt_reset_last_error() {
    //     unsafe {
    //         let mut leaks = 0;

    //         for n_ctx in 0..GEN_NB_VOID_PTR {
    //             let mem_base = xml_mem_blocks();
    //             let ctx = gen_void_ptr(n_ctx, 0);

    //             xml_ctxt_reset_last_error(ctx);
    //             des_void_ptr(n_ctx, ctx, 0);
    //             reset_last_error();
    //             if mem_base != xml_mem_blocks() {
    //                 leaks += 1;
    //                 eprint!(
    //                     "Leak of {} blocks found in xmlCtxtResetLastError",
    //                     xml_mem_blocks() - mem_base
    //                 );
    //                 assert!(
    //                     leaks == 0,
    //                     "{leaks} Leaks are found in xmlCtxtResetLastError()"
    //                 );
    //                 eprintln!(" {}", n_ctx);
    //             }
    //         }
    //     }
    // }

    #[test]
    fn test_xml_get_last_error() {

        /* missing type support */
    }

    #[test]
    fn test_xml_parser_erroror() {

        /* missing type support */
    }

    #[test]
    fn test_xml_parser_print_file_context() {
        unsafe {
            let mut leaks = 0;

            for n_input in 0..GEN_NB_XML_PARSER_INPUT_PTR {
                let mem_base = xml_mem_blocks();
                let input = gen_xml_parser_input_ptr(n_input, 0);

                xml_parser_print_file_context(input);
                des_xml_parser_input_ptr(n_input, input, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlParserPrintFileContext",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlParserPrintFileContext()"
                    );
                    eprintln!(" {}", n_input);
                }
            }
        }
    }

    #[test]
    fn test_xml_parser_print_file_info() {
        unsafe {
            let mut leaks = 0;

            for n_input in 0..GEN_NB_XML_PARSER_INPUT_PTR {
                let mem_base = xml_mem_blocks();
                let input = gen_xml_parser_input_ptr(n_input, 0);

                xml_parser_print_file_info(input);
                des_xml_parser_input_ptr(n_input, input, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlParserPrintFileInfo",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(
                        leaks == 0,
                        "{leaks} Leaks are found in xmlParserPrintFileInfo()"
                    );
                    eprintln!(" {}", n_input);
                }
            }
        }
    }

    #[test]
    fn test_xml_parser_validity_error() {

        /* missing type support */
    }

    #[test]
    fn test_xml_parser_validity_warning() {

        /* missing type support */
    }

    #[test]
    fn test_xml_parser_warning() {

        /* missing type support */
    }

    #[test]
    fn test_xml_reset_error() {
        unsafe {
            let mut leaks = 0;

            for n_err in 0..GEN_NB_XML_ERROR_PTR {
                let mem_base = xml_mem_blocks();
                let err = gen_xml_error_ptr(n_err, 0);

                xml_reset_error(err);
                des_xml_error_ptr(n_err, err, 0);
                reset_last_error();
                if mem_base != xml_mem_blocks() {
                    leaks += 1;
                    eprint!(
                        "Leak of {} blocks found in xmlResetError",
                        xml_mem_blocks() - mem_base
                    );
                    assert!(leaks == 0, "{leaks} Leaks are found in xmlResetError()");
                    eprintln!(" {}", n_err);
                }
            }
        }
    }

    #[test]
    fn test_xml_reset_last_error() {
        reset_last_error();
        reset_last_error();
    }

    #[test]
    fn test_xml_set_generic_error_func() {

        /* missing type support */
    }

    #[test]
    fn test_xml_set_structured_error_func() {

        /* missing type support */
    }
}

//! Provide methods and data structures for error handling.  
//! This module is based on `libxml/xmlerrors.h`, `errors.c`, and so on in `libxml2-v2.11.8`.
//!
//! Please refer to original libxml2 documents also.

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
    XmlErrMisplacedCDATAEnd,       /* 62 */
    XmlErrCDATANotFinished,        /* 63 */
    XmlErrReservedXmlName,         /* 64 */
    XmlErrSpaceRequired,           /* 65 */
    XmlErrSeparatorRequired,       /* 66 */
    XmlErrNmtokenRequired,         /* 67 */
    XmlErrNameRequired,            /* 68 */
    XmlErrPCDATARequired,          /* 69 */
    XmlErrURIRequired,             /* 70 */
    XmlErrPubidRequired,           /* 71 */
    XmlErrLtRequired,              /* 72 */
    XmlErrGtRequired,              /* 73 */
    XmlErrLtSlashRequired,         /* 74 */
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
    XmlErrEntityPEInternal,        /* 88 */
    XmlErrEntityLoop,              /* 89 */
    XmlErrEntityBoundary,          /* 90 */
    XmlErrInvalidURI,              /* 91 */
    XmlErrURIFragment,             /* 92 */
    XmlWarCatalogPI,               /* 93 */
    XmlErrNoDTD,                   /* 94 */
    XmlErrCondsecInvalidKeyword,   /* 95 */
    XmlErrVersionMissing,          /* 96 */
    XmlWarUnknownVersion,          /* 97 */
    XmlWarLangValue,               /* 98 */
    XmlWarNsURI,                   /* 99 */
    XmlWarNsURIRelative,           /* 100 */
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
    XmlDTDAttributeDefault = 500,
    XmlDTDAttributeRedefined,    /* 501 */
    XmlDTDAttributeValue,        /* 502 */
    XmlDTDContentError,          /* 503 */
    XmlDTDContentModel,          /* 504 */
    XmlDTDContentNotDeterminist, /* 505 */
    XmlDTDDifferentPrefix,       /* 506 */
    XmlDTDElemDefaultNamespace,  /* 507 */
    XmlDTDElemNamespace,         /* 508 */
    XmlDTDElemRedefined,         /* 509 */
    XmlDTDEmptyNotation,         /* 510 */
    XmlDTDEntityType,            /* 511 */
    XmlDTDIDFixed,               /* 512 */
    XmlDTDIDRedefined,           /* 513 */
    XmlDTDIDSubset,              /* 514 */
    XmlDTDInvalidChild,          /* 515 */
    XmlDTDInvalidDefault,        /* 516 */
    XmlDTDLoadError,             /* 517 */
    XmlDTDMissingAttribute,      /* 518 */
    XmlDTDMixedCorrupt,          /* 519 */
    XmlDTDMultipleID,            /* 520 */
    XmlDTDNoDoc,                 /* 521 */
    XmlDTDNoDTD,                 /* 522 */
    XmlDTDNoElemName,            /* 523 */
    XmlDTDNoPrefix,              /* 524 */
    XmlDTDNoRoot,                /* 525 */
    XmlDTDNotationRedefined,     /* 526 */
    XmlDTDNotationValue,         /* 527 */
    XmlDTDNotEmpty,              /* 528 */
    XmlDTDNotPCDATA,             /* 529 */
    XmlDTDNotStandalone,         /* 530 */
    XmlDTDRootName,              /* 531 */
    XmlDTDStandaloneWhiteSpace,  /* 532 */
    XmlDTDUnknownAttribute,      /* 533 */
    XmlDTDUnknownElem,           /* 534 */
    XmlDTDUnknownEntity,         /* 535 */
    XmlDTDUnknownID,             /* 536 */
    XmlDTDUnknownNotation,       /* 537 */
    XmlDTDStandaloneDefaulted,   /* 538 */
    XmlDTDXmlidValue,            /* 539 */
    XmlDTDXmlidType,             /* 540 */
    XmlDTDDupToken,              /* 541 */
    XmlHTMLStrucureError = 800,
    XmlHTMLUnknownTag,               /* 801 */
    XmlHTMLIncorrectlyOpenedComment, /* 802 */
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
    XmlRngpExternalRefEmtpy,         /* 1031 */
    XmlRngpExternalRefFailure,       /* 1032 */
    XmlRngpExternalRefRecurse,       /* 1033 */
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
    XmlRngpInvalidURI,               /* 1050 */
    XmlRngpInvalidValue,             /* 1051 */
    XmlRngpMissingHref,              /* 1052 */
    XmlRngpNameMissing,              /* 1053 */
    XmlRngpNeedCombine,              /* 1054 */
    XmlRngpNotAllowedNotEmpty,       /* 1055 */
    XmlRngpNsNameAttrAncestor,       /* 1056 */
    XmlRngpNsNameNoNs,               /* 1057 */
    XmlRngpParamForbidden,           /* 1058 */
    XmlRngpParamNameMissing,         /* 1059 */
    XmlRngpParentRefCreateFailed,    /* 1060 */
    XmlRngpParentRefNameInvalid,     /* 1061 */
    XmlRngpParentRefNoName,          /* 1062 */
    XmlRngpParentRefNoParent,        /* 1063 */
    XmlRngpParentRefNotEmpty,        /* 1064 */
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
    XmlRngpPatNsNameExceptAnyName,   /* 1084 */
    XmlRngpPatNsNameExceptNsName,    /* 1085 */
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
    XmlRngpURIFragment,              /* 1117 */
    XmlRngpURINotAbsolute,           /* 1118 */
    XmlRngpValueEmpty,               /* 1119 */
    XmlRngpValueNoContent,           /* 1120 */
    XmlRngpXmlNsName,                /* 1121 */
    XmlRngpXmlNs,                    /* 1122 */
    XmlXPathExpressionOk = 1200,
    XmlXPathNumberError,            /* 1201 */
    XmlXPathUnfinishedLiteralError, /* 1202 */
    XmlXPathStartLiteralError,      /* 1203 */
    XmlXPathVariableRefError,       /* 1204 */
    XmlXPathUndefVariableError,     /* 1205 */
    XmlXPathInvalidPredicateError,  /* 1206 */
    XmlXPathExprError,              /* 1207 */
    XmlXPathUnclosedError,          /* 1208 */
    XmlXPathUnknownFuncError,       /* 1209 */
    XmlXPathInvalidOperand,         /* 1210 */
    XmlXPathInvalidType,            /* 1211 */
    XmlXPathInvalidArity,           /* 1212 */
    XmlXPathInvalidCtxtSize,        /* 1213 */
    XmlXPathInvalidCtxtPosition,    /* 1214 */
    XmlXPathMemoryError,            /* 1215 */
    XmlXPtrSyntaxError,             /* 1216 */
    XmlXPtrResourceError,           /* 1217 */
    XmlXPtrSubResourceError,        /* 1218 */
    XmlXPathUndefPrefixError,       /* 1219 */
    XmlXPathEncodingError,          /* 1220 */
    XmlXPathInvalidCharError,       /* 1221 */
    XmlXPathInvalidCtxt,
    XmlXPathStackError,
    XmlXPathForbidVariableError,
    XmlXPathOpLimitExceeded,
    XmlXPathRecursionLimitExceeded,
    XmlXPathUnknownError, // 1227, this must put at the end of XPath errors
    XmlTreeInvalidHex = 1300,
    XmlTreeInvalidDec,         /* 1301 */
    XmlTreeUnterminatedEntity, /* 1302 */
    XmlTreeNotUTF8,            /* 1303 */
    XmlSaveNotUTF8 = 1400,
    XmlSaveCharInvalid,     /* 1401 */
    XmlSaveNoDoctype,       /* 1402 */
    XmlSaveUnknownEncoding, /* 1403 */
    XmlRegexpCompileError = 1450,
    XmlIOUnknown = 1500,
    XmlIOEACCES,         /* 1501 */
    XmlIOEAGAIN,         /* 1502 */
    XmlIOEBADF,          /* 1503 */
    XmlIOEBADMSG,        /* 1504 */
    XmlIOEBUSY,          /* 1505 */
    XmlIOECANCELED,      /* 1506 */
    XmlIOECHILD,         /* 1507 */
    XmlIOEDEADLK,        /* 1508 */
    XmlIOEDOM,           /* 1509 */
    XmlIOEEXIST,         /* 1510 */
    XmlIOEFAULT,         /* 1511 */
    XmlIOEFBIG,          /* 1512 */
    XmlIOEINPROGRESS,    /* 1513 */
    XmlIOEINTR,          /* 1514 */
    XmlIOEINVAL,         /* 1515 */
    XmlIOEIO,            /* 1516 */
    XmlIOEISDIR,         /* 1517 */
    XmlIOEMFILE,         /* 1518 */
    XmlIOEMLINK,         /* 1519 */
    XmlIOEMSGSIZE,       /* 1520 */
    XmlIOENAMETOOLONG,   /* 1521 */
    XmlIOENFILE,         /* 1522 */
    XmlIOENODEV,         /* 1523 */
    XmlIOENOENT,         /* 1524 */
    XmlIOENOEXEC,        /* 1525 */
    XmlIOENOLCK,         /* 1526 */
    XmlIOENOMEM,         /* 1527 */
    XmlIOENOSPC,         /* 1528 */
    XmlIOENOSYS,         /* 1529 */
    XmlIOENOTDIR,        /* 1530 */
    XmlIOENOTEMPTY,      /* 1531 */
    XmlIOENOTSUP,        /* 1532 */
    XmlIOENOTTY,         /* 1533 */
    XmlIOENXIO,          /* 1534 */
    XmlIOEPERM,          /* 1535 */
    XmlIOEPIPE,          /* 1536 */
    XmlIOERANGE,         /* 1537 */
    XmlIOEROFS,          /* 1538 */
    XmlIOESPIPE,         /* 1539 */
    XmlIOESRCH,          /* 1540 */
    XmlIOETIMEOUT,       /* 1541 */
    XmlIOEXDEV,          /* 1542 */
    XmlIONetworkAttempt, /* 1543 */
    XmlIOEncoder,        /* 1544 */
    XmlIOFlush,          /* 1545 */
    XmlIOWrite,          /* 1546 */
    XmlIONoInput,        /* 1547 */
    XmlIOBufferFull,     /* 1548 */
    XmlIOLoadError,      /* 1549 */
    XmlIOENOTSOCK,       /* 1550 */
    XmlIOEISCONN,        /* 1551 */
    XmlIOECONNREFUSED,   /* 1552 */
    XmlIOENETUNREACH,    /* 1553 */
    XmlIOEADDRINUSE,     /* 1554 */
    XmlIOEALREADY,       /* 1555 */
    XmlIOEAFNOSUPPORT,   /* 1556 */
    XmlXIncludeRecursion = 1600,
    XmlXIncludeParseValue,           /* 1601 */
    XmlXIncludeEntityDefMismatch,    /* 1602 */
    XmlXIncludeNoHref,               /* 1603 */
    XmlXIncludeNoFallback,           /* 1604 */
    XmlXIncludeHrefURI,              /* 1605 */
    XmlXIncludeTextFragment,         /* 1606 */
    XmlXIncludeTextDocument,         /* 1607 */
    XmlXIncludeInvalidChar,          /* 1608 */
    XmlXIncludeBuildFailed,          /* 1609 */
    XmlXIncludeUnknownEncoding,      /* 1610 */
    XmlXIncludeMultipleRoot,         /* 1611 */
    XmlXIncludeXPtrFailed,           /* 1612 */
    XmlXIncludeXPtrResult,           /* 1613 */
    XmlXIncludeIncludeInInclude,     /* 1614 */
    XmlXIncludeFallbacksInInclude,   /* 1615 */
    XmlXIncludeFallbackNotInInclude, /* 1616 */
    XmlXIncludeDeprecatedNs,         /* 1617 */
    XmlXIncludeFragmentID,           /* 1618 */
    XmlCatalogMissingAttr = 1650,
    XmlCatalogEntryBroken, /* 1651 */
    XmlCatalogPreferValue, /* 1652 */
    XmlCatalogNotCatalog,  /* 1653 */
    XmlCatalogRecursion,   /* 1654 */
    XmlSchemapPrefixUndefined = 1700,
    XmlSchemapAttrFormDefaultValue,         /* 1701 */
    XmlSchemapAttrGrpNonameNoRef,           /* 1702 */
    XmlSchemapAttrNonameNoRef,              /* 1703 */
    XmlSchemapComplextypeNonameNoRef,       /* 1704 */
    XmlSchemapElemFormDefaultValue,         /* 1705 */
    XmlSchemapElemNonameNoRef,              /* 1706 */
    XmlSchemapExtensionNoBase,              /* 1707 */
    XmlSchemapFacetNoValue,                 /* 1708 */
    XmlSchemapFailedBuildImport,            /* 1709 */
    XmlSchemapGroupNonameNoRef,             /* 1710 */
    XmlSchemapImportNamespaceNotURI,        /* 1711 */
    XmlSchemapImportRedefineNsname,         /* 1712 */
    XmlSchemapImportSchemaNotURI,           /* 1713 */
    XmlSchemapInvalidBoolean,               /* 1714 */
    XmlSchemapInvalidEnum,                  /* 1715 */
    XmlSchemapInvalidFacet,                 /* 1716 */
    XmlSchemapInvalidFacetValue,            /* 1717 */
    XmlSchemapInvalidMaxoccurs,             /* 1718 */
    XmlSchemapInvalidMinoccurs,             /* 1719 */
    XmlSchemapInvalidRefAndSubtype,         /* 1720 */
    XmlSchemapInvalidWhiteSpace,            /* 1721 */
    XmlSchemapNoattrNoRef,                  /* 1722 */
    XmlSchemapNotationNoName,               /* 1723 */
    XmlSchemapNotypeNoRef,                  /* 1724 */
    XmlSchemapRefAndSubtype,                /* 1725 */
    XmlSchemapRestrictionNonameNoRef,       /* 1726 */
    XmlSchemapSimpletypeNoname,             /* 1727 */
    XmlSchemapTypeAndSubtype,               /* 1728 */
    XmlSchemapUnknownAllChild,              /* 1729 */
    XmlSchemapUnknownAnyattributeChild,     /* 1730 */
    XmlSchemapUnknownAttrChild,             /* 1731 */
    XmlSchemapUnknownAttrGrpChild,          /* 1732 */
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
    XmlSchemapIncludeSchemaNotURI,          /* 1770 */
    XmlSchemapIncludeSchemaNoURI,           /* 1771 */
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
    XmlSchemavNoRoot = 1801,
    XmlSchemavUndeclaredElem,         /* 1802 */
    XmlSchemavNotToplevel,            /* 1803 */
    XmlSchemavMissing,                /* 1804 */
    XmlSchemavWrongElem,              /* 1805 */
    XmlSchemavNoType,                 /* 1806 */
    XmlSchemavNoRollback,             /* 1807 */
    XmlSchemavIsAbstract,             /* 1808 */
    XmlSchemavNotEmpty,               /* 1809 */
    XmlSchemavElemCont,               /* 1810 */
    XmlSchemavHaveDefault,            /* 1811 */
    XmlSchemavNotNillable,            /* 1812 */
    XmlSchemavExtraContent,           /* 1813 */
    XmlSchemavInvalidAttr,            /* 1814 */
    XmlSchemavInvalidElem,            /* 1815 */
    XmlSchemavNotDeterminist,         /* 1816 */
    XmlSchemavConstruct,              /* 1817 */
    XmlSchemavInternal,               /* 1818 */
    XmlSchemavNotSimple,              /* 1819 */
    XmlSchemavAttrUnknown,            /* 1820 */
    XmlSchemavAttrInvalid,            /* 1821 */
    XmlSchemavValue,                  /* 1822 */
    XmlSchemavFacet,                  /* 1823 */
    XmlSchemavCvcDatatypeValid1_2_1,  /* 1824 */
    XmlSchemavCvcDatatypeValid1_2_2,  /* 1825 */
    XmlSchemavCvcDatatypeValid1_2_3,  /* 1826 */
    XmlSchemavCvcType3_1_1,           /* 1827 */
    XmlSchemavCvcType3_1_2,           /* 1828 */
    XmlSchemavCvcFacetValid,          /* 1829 */
    XmlSchemavCvcLengthValid,         /* 1830 */
    XmlSchemavCvcMinLengthValid,      /* 1831 */
    XmlSchemavCvcMaxLengthValid,      /* 1832 */
    XmlSchemavCvcMinInclusiveValid,   /* 1833 */
    XmlSchemavCvcMaxInclusiveValid,   /* 1834 */
    XmlSchemavCvcMinExclusiveValid,   /* 1835 */
    XmlSchemavCvcMaxExclusiveValid,   /* 1836 */
    XmlSchemavCvcTotalDigitsValid,    /* 1837 */
    XmlSchemavCvcFractionDigitsValid, /* 1838 */
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
    XmlXPtrUnknownScheme = 1900,
    XmlXPtrChildseqStart, /* 1901 */
    XmlXPtrEvalFailed,    /* 1902 */
    XmlXPtrExtraObjects,  /* 1903 */
    XmlC14NCreateCtxt = 1950,
    XmlC14NRequiresUtf8,      /* 1951 */
    XmlC14NCreateStack,       /* 1952 */
    XmlC14NInvalidNode,       /* 1953 */
    XmlC14NUnknowNode,        /* 1954 */
    XmlC14NRelativeNamespace, /* 1955 */
    XmlFTPPasvAnswer = 2000,
    XmlFTPEpsvAnswer, /* 2001 */
    XmlFTPAccnt,      /* 2002 */
    XmlFTPUrlSyntax,  /* 2003 */
    XmlHTTPUrlSyntax = 2020,
    XmlHTTPUseIp,       /* 2021 */
    XmlHTTPUnknownHost, /* 2022 */
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
    XmlCheckFoundCDATA,     /* 5003 */
    XmlCheckFoundEntityRef, /* 5004 */
    XmlCheckFoundEntity,    /* 5005 */
    XmlCheckFoundPI,        /* 5006 */
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
    XmlCheckNotDTD,         /* 5022 */
    XmlCheckNotAttr,        /* 5023 */
    XmlCheckNotAttrDecl,    /* 5024 */
    XmlCheckNotElemDecl,    /* 5025 */
    XmlCheckNotEntityDecl,  /* 5026 */
    XmlCheckNotNsDecl,      /* 5027 */
    XmlCheckNoHref,         /* 5028 */
    XmlCheckWrongParent,    /* 5029 */
    XmlCheckNsScope,        /* 5030 */
    XmlCheckNsAncestor,     /* 5031 */
    XmlCheckNotUTF8,        /* 5032 */
    XmlCheckNoDict,         /* 5033 */
    XmlCheckNotNCName,      /* 5034 */
    XmlCheckOutsideDict,    /* 5035 */
    XmlCheckWrongName,      /* 5036 */
    XmlCheckNameNotNull,    /* 5037 */
    XmlI18NNoName = 6000,
    XmlI18NNoHandler,     /* 6001 */
    XmlI18NExcessHandler, /* 6002 */
    XmlI18NConvFailed,    /* 6003 */
    XmlI18NNoOutput,      /* 6004 */
    XmlBufOverflow = 7000
);

impl XmlParserErrors {
    pub fn is_ok(&self) -> bool {
        *self == Self::XmlErrOK
    }

    pub fn is_err(&self) -> bool {
        !self.is_ok()
    }
}

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

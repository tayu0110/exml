use std::{
    borrow::Cow,
    ffi::{c_void, CStr},
    io::{stderr, Write},
    ptr::{null_mut, NonNull},
    slice::from_raw_parts,
    sync::atomic::{AtomicBool, Ordering},
};

use crate::{
    globals::{GenericError, GenericErrorContext, GLOBAL_STATE},
    libxml::{
        parser::{XmlParserCtxtPtr, XmlParserInputPtr},
        tree::{XmlElementType, XmlNode, XmlNodePtr},
        xmlerror::XmlParserErrors,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlErrorLevel {
    #[default]
    XmlErrNone = 0,
    XmlErrWarning = 1,
    XmlErrError = 2,
    XmlErrFatal = 3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum XmlErrorDomain {
    #[default]
    XmlFromNone = 0,
    XmlFromParser,
    XmlFromTree,
    XmlFromNamespace,
    XmlFromDTD,
    XmlFromHTML,
    XmlFromMemory,
    XmlFromOutput,
    XmlFromIO,
    XmlFromFTP,
    XmlFromHTTP,
    XmlFromXInclude,
    XmlFromXPath,
    XmlFromXPointer,
    XmlFromRegexp,
    XmlFromDatatype,
    XmlFromSchemasp,
    XmlFromSchemasv,
    XmlFromRelaxngp,
    XmlFromRelaxngv,
    XmlFromCatalog,
    XmlFromC14N,
    XmlFromXSLT,
    XmlFromValid,
    XmlFromCheck,
    XmlFromWriter,
    XmlFromModule,
    XmlFromI18N,
    XmlFromSchematronv,
    XmlFromBuffer,
    XmlFromURI,
}

#[derive(Debug, Clone, Default)]
pub struct XmlError {
    pub(crate) domain: XmlErrorDomain,
    pub(crate) code: XmlParserErrors,
    pub(crate) message: Option<Cow<'static, str>>,
    pub(crate) level: XmlErrorLevel,
    pub(crate) file: Option<Cow<'static, str>>, // PathBuf or Vec<u8> is better ???
    pub(crate) line: usize,
    pub(crate) str1: Option<Cow<'static, str>>,
    pub(crate) str2: Option<Cow<'static, str>>,
    pub(crate) str3: Option<Cow<'static, str>>,
    pub(crate) int1: i32,
    pub(crate) int2: i32,
    pub(crate) ctxt: Option<NonNull<c_void>>,
    pub(crate) node: Option<NonNull<XmlNode>>,
}

impl XmlError {
    pub fn reset(&mut self) {
        if !self.code.is_ok() {
            *self = Self::default();
        }
    }

    pub fn is_ok(&self) -> bool {
        self.code.is_ok()
    }

    pub fn is_err(&self) -> bool {
        !self.is_ok()
    }

    pub fn code(&self) -> XmlParserErrors {
        self.code
    }

    pub fn message(&self) -> Option<&str> {
        self.message.as_ref().map(|s| s.as_ref())
    }

    pub fn file(&self) -> Option<&str> {
        self.file.as_ref().map(|s| s.as_ref())
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn domain(&self) -> XmlErrorDomain {
        self.domain
    }

    pub fn level(&self) -> XmlErrorLevel {
        self.level
    }

    pub fn node(&self) -> Option<NonNull<XmlNode>> {
        self.node
    }

    pub fn context(&self) -> Option<NonNull<c_void>> {
        self.ctxt
    }

    pub fn str1(&self) -> Option<&str> {
        self.str1.as_ref().map(|s| s.as_ref())
    }

    pub fn int1(&self) -> i32 {
        self.int1
    }
}

/// Default generic error function.
///
/// If `out` is `None`, output `libc:perror`-like format message to stderr.
pub fn generic_error_default(_context: Option<GenericErrorContext>, msg: &str) {
    let out = GLOBAL_STATE.with_borrow_mut(|state| {
        state
            .generic_error_context
            .get_or_insert_with(|| {
                let stderr: Box<dyn Write> = Box::new(stderr());
                GenericErrorContext::new(stderr)
            })
            .clone()
    });
    let mut lock = out.context.lock().unwrap();
    let context = lock
        .downcast_mut::<Box<dyn Write>>()
        .expect("GenericErrorContext is not writable.");
    write!(context, "{msg}").ok();
}

#[macro_export]
macro_rules! generic_error {
    ( $fmt:literal, $( $args:expr ),* ) => {
        let msg = format!($fmt, $( $args ),*);
        let func = $crate::globals::GLOBAL_STATE.with_borrow_mut(|state| state.generic_error);
        func(None, msg.as_str());
    };
    ( $fmt:literal ) => {
        $crate::generic_error!($fmt, );
    }
}

#[doc(hidden)]
pub unsafe fn parser_print_file_context_internal(
    input: XmlParserInputPtr,
    channel: GenericError,
    data: Option<GenericErrorContext>,
) {
    let mut cur: *const u8;
    const SIZE: usize = 80;

    if input.is_null() || (*input).cur.is_null() {
        return;
    }

    cur = (*input).cur;
    let base: *const u8 = (*input).base;
    /* skip backwards over any end-of-lines */
    while cur > base && (*cur == b'\n' || *cur == b'\r') {
        cur = cur.sub(1);
    }
    let mut n = 0;
    /* search backwards for beginning-of-line (to max buff size) */
    while n < SIZE && cur > base && *cur != b'\n' && *cur != b'\r' {
        cur = cur.sub(1);
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
    let col = (*input).cur.offset_from(cur) as usize;
    let mut content = String::with_capacity(SIZE);

    /* search forward for end-of-line (to max buff size) */
    let mut n = 0;
    let chunk = {
        let mut i = 0;
        let mut now = *cur;
        while now != 0 && now != b'\n' && now != b'\r' && i < SIZE {
            i += 1;
            now = *cur.add(i);
        }
        from_raw_parts(cur, i)
    };
    if let Some(chunk) = chunk.utf8_chunks().next() {
        for c in chunk
            .valid()
            .chars()
            .take_while(|&c| c != '\n' && c != '\r')
        {
            n += c.len_utf8();
            if n > SIZE {
                break;
            }
            content.push(c);
        }
    }
    /* print out the selected text */
    channel(data.clone(), format!("{content}\n").as_str());
    /* create blank line with problem pointer */
    let mut ptr = content
        .bytes()
        .take(col)
        .map(|c| if c == b'\t' { '\t' } else { ' ' })
        .collect::<String>();
    if ptr.len() == SIZE {
        ptr.pop();
    }
    ptr.push_str("^\n");
    channel(data.clone(), ptr.as_str());
}

pub unsafe fn parser_print_file_context(input: XmlParserInputPtr) {
    let (channel, data) = GLOBAL_STATE
        .with_borrow(|state| (state.generic_error, state.generic_error_context.clone()));
    parser_print_file_context_internal(input, channel, data);
}

pub unsafe fn parser_print_file_info(input: XmlParserInputPtr) {
    if !input.is_null() {
        if !(*input).filename.is_null() {
            generic_error!(
                "{}:{}: ",
                CStr::from_ptr((*input).filename).to_string_lossy(),
                (*input).line
            );
        } else {
            generic_error!("Entity: line {}: ", (*input).line);
        }
    }
}

pub unsafe fn report_error(
    err: &XmlError,
    ctxt: XmlParserCtxtPtr,
    msg: Option<&str>,
    channel: Option<GenericError>,
    data: Option<GenericErrorContext>,
) {
    let mut name: *const u8 = null_mut();
    let mut input: XmlParserInputPtr = null_mut();
    let mut cur: XmlParserInputPtr = null_mut();

    let channel = GLOBAL_STATE.with_borrow_mut(|state| {
        if let Some(channel) = channel {
            channel
        } else {
            state.generic_error
        }
    });
    let file = err.file.as_ref();
    let line = err.line;
    let code = err.code;
    let domain = err.domain;
    let level = err.level;
    let node: XmlNodePtr = err.node.map_or(null_mut(), |n| n.as_ptr()) as _;

    if code.is_ok() {
        return;
    }

    if !node.is_null() && matches!((*node).typ, XmlElementType::XmlElementNode) {
        name = (*node).name;
    }

    /*
     * Maintain the compatibility with the legacy error handling
     */
    if !ctxt.is_null() {
        input = (*ctxt).input;
        if !input.is_null() && (*input).filename.is_null() && (*ctxt).input_nr > 1 {
            cur = input;
            input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
        }
        if !input.is_null() {
            if !(*input).filename.is_null() {
                channel(
                    data.clone(),
                    format!(
                        "{}:{}: ",
                        CStr::from_ptr((*input).filename).to_string_lossy(),
                        (*input).line
                    )
                    .as_str(),
                );
            } else if line != 0 && domain == XmlErrorDomain::XmlFromParser {
                channel(
                    data.clone(),
                    format!("Entity: line {}: ", (*input).line).as_str(),
                );
            }
        }
    } else if let Some(file) = file {
        channel(data.clone(), format!("{file}:{line}: ").as_str());
    } else if line != 0
        && (domain == XmlErrorDomain::XmlFromParser
            || domain == XmlErrorDomain::XmlFromSchemasv
            || domain == XmlErrorDomain::XmlFromSchemasp
            || domain == XmlErrorDomain::XmlFromDTD
            || domain == XmlErrorDomain::XmlFromRelaxngp
            || domain == XmlErrorDomain::XmlFromRelaxngv)
    {
        channel(data.clone(), format!("Entity: line {line}: ").as_str());
    }
    if !name.is_null() {
        channel(
            data.clone(),
            format!(
                "element {}: ",
                CStr::from_ptr(name as *const i8).to_string_lossy()
            )
            .as_str(),
        );
    }
    match domain {
        XmlErrorDomain::XmlFromParser => channel(data.clone(), "parser "),
        XmlErrorDomain::XmlFromNamespace => channel(data.clone(), "namespace "),
        XmlErrorDomain::XmlFromDTD | XmlErrorDomain::XmlFromValid => {
            channel(data.clone(), "validity ")
        }
        XmlErrorDomain::XmlFromHTML => channel(data.clone(), "HTML parser "),
        XmlErrorDomain::XmlFromMemory => channel(data.clone(), "memory "),
        XmlErrorDomain::XmlFromOutput => channel(data.clone(), "output "),
        XmlErrorDomain::XmlFromIO => channel(data.clone(), "I/O "),
        XmlErrorDomain::XmlFromXInclude => channel(data.clone(), "XInclude "),
        XmlErrorDomain::XmlFromXPath => channel(data.clone(), "XPath "),
        XmlErrorDomain::XmlFromXPointer => channel(data.clone(), "parser "),
        XmlErrorDomain::XmlFromRegexp => channel(data.clone(), "regexp "),
        XmlErrorDomain::XmlFromModule => channel(data.clone(), "module "),
        XmlErrorDomain::XmlFromSchemasv => channel(data.clone(), "Schemas validity "),
        XmlErrorDomain::XmlFromSchemasp => channel(data.clone(), "Schemas parser "),
        XmlErrorDomain::XmlFromRelaxngp => channel(data.clone(), "Relax-NG parser "),
        XmlErrorDomain::XmlFromRelaxngv => channel(data.clone(), "Relax-NG validity "),
        XmlErrorDomain::XmlFromCatalog => channel(data.clone(), "Catalog "),
        XmlErrorDomain::XmlFromC14N => channel(data.clone(), "C14N "),
        XmlErrorDomain::XmlFromXSLT => channel(data.clone(), "XSLT "),
        XmlErrorDomain::XmlFromI18N => channel(data.clone(), "encoding "),
        XmlErrorDomain::XmlFromSchematronv => channel(data.clone(), "schematron "),
        XmlErrorDomain::XmlFromBuffer => channel(data.clone(), "internal buffer "),
        XmlErrorDomain::XmlFromURI => channel(data.clone(), "URI "),
        _ => {}
    };
    match level {
        XmlErrorLevel::XmlErrNone => channel(data.clone(), ": "),
        XmlErrorLevel::XmlErrWarning => channel(data.clone(), "warning : "),
        XmlErrorLevel::XmlErrError => channel(data.clone(), "error : "),
        XmlErrorLevel::XmlErrFatal => channel(data.clone(), "error : "),
    };
    if let Some(msg) = msg {
        if !msg.is_empty() && !msg.ends_with('\n') {
            channel(data.clone(), format!("{msg}\n").as_str());
        } else {
            channel(data.clone(), msg);
        }
    } else {
        channel(data.clone(), "out of memory error\n");
    }

    if !ctxt.is_null() {
        parser_print_file_context_internal(input, channel, data.clone());
        if !cur.is_null() {
            if !(*cur).filename.is_null() {
                channel(
                    data.clone(),
                    format!(
                        "{}:{}: \n",
                        CStr::from_ptr((*cur).filename).to_string_lossy(),
                        (*cur).line
                    )
                    .as_str(),
                )
            } else if line != 0 && domain == XmlErrorDomain::XmlFromParser {
                channel(
                    data.clone(),
                    format!("Entity: line {}: \n", (*cur).line).as_str(),
                );
            }
            parser_print_file_context_internal(cur, channel, data.clone());
        }
    }
    if let Some(str1) = err.str1.as_ref() {
        if domain == XmlErrorDomain::XmlFromXPath && err.int1 < 100 && err.int1 < str1.len() as i32
        {
            channel(data.clone(), format!("{str1}\n").as_str());
            let mut buf = " ".repeat(err.int1 as usize);
            buf.push_str("^\n");
            channel(data.clone(), buf.as_str());
        }
    }
}

pub(crate) fn parser_error(ctx: Option<GenericErrorContext>, msg: &str) {
    let mut cur: XmlParserInputPtr = null_mut();

    if let Some(ctxt) = ctx {
        unsafe {
            let lock = ctxt.context.lock().unwrap();
            let ctxt = **lock
                .downcast_ref::<Box<XmlParserCtxtPtr>>()
                .expect("ctxt is not XmlParserCtxtPtr");
            let mut input = (*ctxt).input;
            if !input.is_null() && (*input).filename.is_null() && (*ctxt).input_nr > 1 {
                cur = input;
                input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
            }
            parser_print_file_info(input);

            generic_error!("error: {msg}");
            parser_print_file_context(input);
            if !cur.is_null() {
                parser_print_file_info(cur);
                generic_error!("\n");
                parser_print_file_context(cur);
            }
        }
    } else {
        generic_error!("error: {msg}");
    }
}

pub(crate) fn parser_warning(ctx: Option<GenericErrorContext>, msg: &str) {
    let mut cur: XmlParserInputPtr = null_mut();

    if let Some(ctx) = ctx {
        let lock = ctx.context.lock().unwrap();
        let ctxt = **lock
            .downcast_ref::<Box<XmlParserCtxtPtr>>()
            .expect("ctxt is not XmlParserCtxtPtr");
        unsafe {
            let mut input = (*ctxt).input;
            if !input.is_null() && (*input).filename.is_null() && (*ctxt).input_nr > 1 {
                cur = input;
                input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
            }
            parser_print_file_info(input);

            generic_error!("warning: {msg}");

            parser_print_file_context(input);
            if !cur.is_null() {
                parser_print_file_info(cur);
                generic_error!("\n");
                parser_print_file_context(cur);
            }
        }
    } else {
        generic_error!("warning: {msg}");
    }
}

pub(crate) fn parser_validity_error(ctx: Option<GenericErrorContext>, msg: &str) {
    let mut input: XmlParserInputPtr = std::ptr::null_mut();
    let len = msg.len();
    static HAD_INFO: AtomicBool = AtomicBool::new(false);

    unsafe {
        if let Some(ctx) = ctx {
            let lock = ctx.context.lock().unwrap();
            let ctxt = **lock
                .downcast_ref::<Box<XmlParserCtxtPtr>>()
                .expect("ctxt is not XmlParserCtxtPtr");
            if len > 1 && msg.as_bytes()[len - 2] != b':' {
                input = (*ctxt).input;
                if (*input).filename.is_null() && (*ctxt).input_nr > 1 {
                    input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
                }

                if !HAD_INFO.load(Ordering::Acquire) {
                    parser_print_file_info(input);
                }
                generic_error!("validity error: ");
                HAD_INFO.store(false, Ordering::Release)
            } else {
                HAD_INFO.store(true, Ordering::Release);
            }

            generic_error!("{msg}");

            parser_print_file_context(input);
        } else {
            if len > 1 && msg.as_bytes()[len - 2] != b':' {
                generic_error!("validity error: ");
                HAD_INFO.store(false, Ordering::Release)
            } else {
                HAD_INFO.store(true, Ordering::Release);
            }

            generic_error!("{msg}");
        }
    }
}

pub(crate) fn parser_validity_warning(ctx: Option<GenericErrorContext>, msg: &str) {
    let mut input: XmlParserInputPtr = std::ptr::null_mut();
    let len = msg.len();

    if let Some(ctx) = ctx {
        let lock = ctx.context.lock().unwrap();
        let ctxt = **lock
            .downcast_ref::<Box<XmlParserCtxtPtr>>()
            .expect("ctxt is not XmlParserCtxtPtr");
        unsafe {
            if len != 0 && msg.as_bytes()[len - 1] != b':' {
                input = (*ctxt).input;
                if (*input).filename.is_null() && (*ctxt).input_nr > 1 {
                    input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
                }

                parser_print_file_info(input);
            }
        }

        generic_error!("validity warning: {msg}");

        unsafe {
            parser_print_file_context(input);
        }
    } else {
        generic_error!("validity warning: {msg}");
    }
}

use std::{
    any::Any,
    borrow::Cow,
    ffi::{c_void, CStr, CString},
    fmt::Write as _,
    fs::File,
    io::{Error, Write},
    os::fd::AsRawFd,
    ptr::{null_mut, NonNull},
    sync::atomic::{AtomicBool, Ordering},
};

use libc::{fdopen, fflush};

use crate::{
    globals::{GenericError, GLOBAL_STATE},
    libxml::{
        globals::xml_generic_error,
        parser::{XmlParserCtxtPtr, XmlParserInputPtr},
        tree::{XmlElementType, XmlNodePtr},
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
    pub(crate) node: Option<NonNull<c_void>>,
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
}

/// Default generic error function.
///
/// If `out` is `None`, output `libc:perror`-like format message to stderr.
pub fn generic_error_default(out: Option<&mut (dyn Write + 'static)>, msg: &str) {
    if let Some(out) = out {
        write!(out, "{msg}").ok();
    } else {
        // almost quivalent to `perror` ???
        eprintln!("{msg}: {}", Error::last_os_error());
    }
}

/// # Safety
/// - `out` must be a valid `File` or `None`.
/// - If other types are set to `out`, the behaivior is undefined.
pub(crate) unsafe fn generic_error_wrapper_for_cfunction(
    out: Option<&mut (dyn Write + 'static)>,
    msg: &str,
) {
    let msg = CString::new(msg).unwrap();
    if let Some(out) = out {
        // Does it work ???
        // It seems so danger...
        let mut out = Box::from_raw(out as *mut dyn Write as *mut File);
        {
            let out = out.as_mut() as &mut dyn Any;
            if let Some(file) = out.downcast_mut::<File>() {
                file.flush().ok();
                let fd = file.as_raw_fd();
                let fp = fdopen(fd, c"a".as_ptr());
                xml_generic_error(fp as _, msg.as_ptr());
                fflush(fp);
            } else {
                unimplemented!("Unsupported output stream for generic error function.");
            }
        }
        // prevent to drop maybe-invalid pointer
        let _ = Box::into_raw(out);
    } else {
        xml_generic_error(null_mut(), msg.as_ptr());
    }
}

#[macro_export]
macro_rules! generic_error {
    ( $fmt:literal, $( $args:expr ),* ) => {
        $crate::globals::GLOBAL_STATE.with_borrow_mut(|state| {
            let msg = format!($fmt, $( $args ),*);
            let func = state.generic_error;
            let out = state.generic_error_context.as_deref_mut();
            func(out, msg.as_str());
        });
    };
    ( $fmt:literal ) => {
        $crate::generic_error!($fmt, );
    }
}

/// A dummy wrapper that allows all types to be treated as GenericError contexts.
///
/// This is a temporary workaround.  
/// The original C library treats contexts as void pointer.  
/// The closest thing to that in Rust would be std::any::Any,
/// but it cannot be treated in the same way because it is not easy to convert between trait objects.
pub(crate) struct ErrorContextWrap<T>(pub(crate) T);

impl<T> Write for ErrorContextWrap<T> {
    fn write(&mut self, _buf: &[u8]) -> std::io::Result<usize> {
        Ok(0)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

fn parser_print_file_context_internal(input: XmlParserInputPtr, buf: &mut String) {
    let mut cur: *const u8;
    const SIZE: usize = 80;
    let mut content = String::with_capacity(SIZE);

    unsafe {
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
        while n < SIZE - 1 && cur > base && *cur != b'\n' && *cur != b'\r' {
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
        /* search forward for end-of-line (to max buff size) */
        let mut n = 0;
        let s = CStr::from_ptr(cur as *const i8).to_string_lossy();
        for c in s.chars().take_while(|&c| c != '\n' && c != '\r') {
            n += c.len_utf8();
            if n > SIZE {
                break;
            }
            content.push(c);
        }
        /* print out the selected text */
        buf.push_str(format!("{content}\n").as_str());
        /* create blank line with problem pointer */
        let mut ptr = content
            .chars()
            .map(|c| if c == '\t' { c } else { ' ' })
            .collect::<String>();
        ptr.pop();
        ptr.push('^');
        buf.push_str(format!("{ptr}\n").as_str());
    }
}

pub fn parser_print_file_context(input: XmlParserInputPtr) {
    let mut buf = String::new();
    parser_print_file_context_internal(input, &mut buf);
    generic_error!("{buf}");
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
    data: Option<&mut (dyn Write + 'static)>,
) {
    let mut name: *const u8 = null_mut();
    let mut input: XmlParserInputPtr = null_mut();
    let mut cur: XmlParserInputPtr = null_mut();

    GLOBAL_STATE.with_borrow_mut(|state| {
        let channel = if let Some(channel) = channel {
            channel
        } else {
            state.generic_error
        };
        let mut stderr = std::io::stderr();
        let data = data.unwrap_or(
            state
                .generic_error_context
                .as_deref_mut()
                .unwrap_or(&mut stderr),
        );
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

        let mut output = String::new();
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
                    write!(
                        output,
                        "{}:{}: ",
                        CStr::from_ptr((*input).filename).to_string_lossy(),
                        (*input).line
                    );
                } else if line != 0 && domain == XmlErrorDomain::XmlFromParser {
                    write!(output, "Entity: line {}: ", (*input).line);
                }
            }
        } else if let Some(file) = file {
            write!(output, "{file}:{line}: ");
        } else if line != 0
            && (domain == XmlErrorDomain::XmlFromParser
                || domain == XmlErrorDomain::XmlFromSchemasv
                || domain == XmlErrorDomain::XmlFromSchemasp
                || domain == XmlErrorDomain::XmlFromDTD
                || domain == XmlErrorDomain::XmlFromRelaxngp
                || domain == XmlErrorDomain::XmlFromRelaxngv)
        {
            write!(output, "Entity: line {line}: ");
        }
        if !name.is_null() {
            write!(
                output,
                "element {}: ",
                CStr::from_ptr(name as *const i8).to_string_lossy()
            );
        }
        match domain {
            XmlErrorDomain::XmlFromParser => write!(output, "parser "),
            XmlErrorDomain::XmlFromNamespace => write!(output, "namespace "),
            XmlErrorDomain::XmlFromDTD | XmlErrorDomain::XmlFromValid => {
                write!(output, "validity ")
            }
            XmlErrorDomain::XmlFromHTML => write!(output, "HTML parser "),
            XmlErrorDomain::XmlFromMemory => write!(output, "memory "),
            XmlErrorDomain::XmlFromOutput => write!(output, "output "),
            XmlErrorDomain::XmlFromIO => write!(output, "I/O "),
            XmlErrorDomain::XmlFromXInclude => write!(output, "XInclude "),
            XmlErrorDomain::XmlFromXPath => write!(output, "XPath "),
            XmlErrorDomain::XmlFromXPointer => write!(output, "parser "),
            XmlErrorDomain::XmlFromRegexp => write!(output, "regexp "),
            XmlErrorDomain::XmlFromModule => write!(output, "module "),
            XmlErrorDomain::XmlFromSchemasv => write!(output, "Schemas validity "),
            XmlErrorDomain::XmlFromSchemasp => write!(output, "Schemas parser "),
            XmlErrorDomain::XmlFromRelaxngp => write!(output, "Relax-NG parser "),
            XmlErrorDomain::XmlFromRelaxngv => write!(output, "Relax-NG validity "),
            XmlErrorDomain::XmlFromCatalog => write!(output, "Catalog "),
            XmlErrorDomain::XmlFromC14N => write!(output, "C14N "),
            XmlErrorDomain::XmlFromXSLT => write!(output, "XSLT "),
            XmlErrorDomain::XmlFromI18N => write!(output, "encoding "),
            XmlErrorDomain::XmlFromSchematronv => write!(output, "schematron "),
            XmlErrorDomain::XmlFromBuffer => write!(output, "internal buffer "),
            XmlErrorDomain::XmlFromURI => write!(output, "URI "),
            _ => Ok(()),
        };
        match level {
            XmlErrorLevel::XmlErrNone => write!(output, ": "),
            XmlErrorLevel::XmlErrWarning => write!(output, "warning : "),
            XmlErrorLevel::XmlErrError => write!(output, "error : "),
            XmlErrorLevel::XmlErrFatal => write!(output, "error : "),
        };
        if let Some(msg) = msg {
            if !msg.is_empty() && !msg.ends_with('\n') {
                writeln!(output, "{msg}").ok();
            } else {
                write!(output, "{msg}");
            }
        } else {
            writeln!(output, "out of memory error").ok();
        }

        if !ctxt.is_null() {
            parser_print_file_context_internal(input, &mut output);
            if !cur.is_null() {
                if !(*cur).filename.is_null() {
                    writeln!(
                        output,
                        "{}:{}: ",
                        CStr::from_ptr((*cur).filename).to_string_lossy(),
                        (*cur).line
                    )
                    .ok();
                } else if line != 0 && domain == XmlErrorDomain::XmlFromParser {
                    writeln!(output, "Entity: line {}: ", (*cur).line).ok();
                }
                parser_print_file_context_internal(cur, &mut output);
            }
        }
        if let Some(str1) = err.str1.as_ref() {
            if domain == XmlErrorDomain::XmlFromXPath
                && err.int1 < 100
                && err.int1 < str1.len() as i32
            {
                writeln!(output, "{str1}").ok();
                output.push_str(" ".repeat(err.int1 as usize).as_str());
                output.push_str("^\n");
            }
        }
        channel(Some(data), &output);
    });
}

pub(crate) fn parser_error(ctx: Option<&mut (dyn Write + 'static)>, msg: &str) {
    let mut cur: XmlParserInputPtr = null_mut();

    if let Some(ctxt) = ctx {
        unsafe {
            let ctxt = (*(ctxt as *mut dyn Write as *mut ErrorContextWrap<XmlParserCtxtPtr>)).0;
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

pub(crate) fn parser_warning(ctx: Option<&mut (dyn Write + 'static)>, msg: &str) {
    let mut cur: XmlParserInputPtr = null_mut();

    if let Some(ctx) = ctx {
        let ctxt = ctx as *mut dyn Write as *mut ErrorContextWrap<XmlParserCtxtPtr>;
        unsafe {
            let ctxt = (*ctxt).0;
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

pub(crate) fn parser_validity_error(ctx: Option<&mut (dyn Write + 'static)>, msg: &str) {
    let mut input: XmlParserInputPtr = std::ptr::null_mut();
    let len = msg.len();
    static HAD_INFO: AtomicBool = AtomicBool::new(false);

    unsafe {
        if let Some(ctx) = ctx {
            let ctxt = (*(ctx as *mut dyn Write as *mut ErrorContextWrap<XmlParserCtxtPtr>)).0;
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

pub(crate) fn parser_validity_warning(ctx: Option<&mut (dyn Write + 'static)>, msg: &str) {
    let mut input: XmlParserInputPtr = std::ptr::null_mut();
    let len = msg.len();

    if let Some(ctx) = ctx {
        let ctxt = ctx as *mut dyn Write as *mut ErrorContextWrap<XmlParserCtxtPtr>;
        unsafe {
            let ctxt = (*ctxt).0;
            if len != 0 && msg.as_bytes()[len - 1] != b':' {
                input = (*ctxt).input;
                if (*input).filename.is_null() && (*ctxt).input_nr > 1 {
                    input = *(*ctxt).input_tab.add((*ctxt).input_nr as usize - 2);
                }

                parser_print_file_info(input);
            }
        }

        generic_error!("validity warning: {msg}");

        parser_print_file_context(input);
    } else {
        generic_error!("validity warning: {msg}");
    }
}

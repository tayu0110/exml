use std::{
    any::Any,
    ffi::CString,
    fs::File,
    io::{Error, Write},
    os::fd::AsRawFd,
    ptr::null_mut,
};

use libc::{fdopen, fflush};

use crate::libxml::globals::xml_generic_error;

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

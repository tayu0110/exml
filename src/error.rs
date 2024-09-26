use std::io::{Error, Write};

/// Default generic error function.
///
/// If `out` is `None`, output `libc:perror`-like format message to stderr.
pub fn generic_error_default<'a>(out: Option<&mut (dyn Write + 'a)>, msg: &str) {
    if let Some(out) = out {
        write!(out, "{msg}").ok();
    } else {
        // almost quivalent to `perror` ???
        eprintln!("{msg}: {}", Error::last_os_error());
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

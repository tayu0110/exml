use std::io::{Error, Write};

pub fn generic_error_default(out: Option<&mut impl Write>, msg: &str) {
    if let Some(out) = out {
        write!(out, "{msg}").ok();
    } else {
        // almost quivalent to `perror` ???
        eprintln!("{msg}: {}", Error::last_os_error());
    }
}

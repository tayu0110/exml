#![allow(clippy::missing_safety_doc)]

pub mod libxml;
pub mod private;
#[cfg(test)]
pub(crate) mod test_util;

pub const SYSCONFDIR: &str = if let Some(sysconfdir) = option_env!("SYSCONFDIR") {
    sysconfdir
} else {
    "/etc"
};

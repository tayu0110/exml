#![allow(clippy::missing_safety_doc)]
#![allow(clippy::blocks_in_conditions)]
#![allow(clippy::needless_range_loop)]
#![allow(unused_assignments)]
#![allow(deprecated)]
#![allow(unused)]
#![warn(unused_assignments)]
#![warn(unused_mut)]
#![warn(unused_imports)]
#![warn(unused_labels)]
#![warn(unused_parens)]
#![warn(unused_variables)]

pub mod libxml;
pub mod private;
#[cfg(test)]
pub(crate) mod test_util;

pub const SYSCONFDIR: &str = if let Some(sysconfdir) = option_env!("SYSCONFDIR") {
    sysconfdir
} else {
    "/etc"
};

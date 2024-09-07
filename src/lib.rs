pub mod libxml;
pub mod private;
#[cfg(test)]
pub(crate) mod test_util;

/*
 * libxml.h: internal header only used during the compilation of libxml
 *
 * See COPYRIGHT for the status of this software
 *
 * Author: breese@users.sourceforge.net
 */

/*
 * Due to some Autotools limitations, this variable must be passed as
 * compiler flag. Define a default value if the macro wasn't set by the
 * build system.
 */
//  #ifndef SYSCONFDIR
//    #define SYSCONFDIR "/etc"
//  #endif
pub const SYSCONFDIR: &str = if let Some(sysconfdir) = option_env!("SYSCONFDIR") {
    sysconfdir
} else {
    "/etc"
};

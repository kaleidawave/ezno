#![allow(
    clippy::new_without_default,
    // TODO: Remove when fixed
	clippy::result_unit_err,
    clippy::default_trait_access,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::implicit_hasher,
    clippy::too_many_lines,
    // More explicit sometimes to have the module name
    clippy::module_name_repetitions
)]
#![warn(clippy::cast_precision_loss, clippy::cast_possible_truncation, clippy::cast_sign_loss)]

use ezno_lib::{cli::run_cli, FSFunction};

fn main() -> std::process::ExitCode {
	let arguments = std::env::args().skip(1).collect::<Vec<_>>();
	let arguments = arguments.iter().map(String::as_str).collect::<Vec<_>>();

	run_cli(&arguments, FSFunction::new())
}

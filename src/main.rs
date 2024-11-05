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

use ezno_lib::cli::run_cli;

#[cfg(target_family = "windows")]
pub(crate) fn cli_input_resolver(prompt: &str) -> String {
	print!("{prompt}> ");
	io::Write::flush(&mut io::stdout()).unwrap();
	let mut input = String::new();
	let std_in = &mut io::stdin();
	let _n = multiline_term_input::read_string(std_in, &mut input);
	input
}

#[cfg(target_family = "unix")]
#[allow(clippy::unnecessary_wraps)]
pub(crate) fn cli_input_resolver(prompt: &str) -> String {
	print!("{prompt}> ");
	io::Write::flush(&mut io::stdout()).unwrap();
	let mut input = String::new();
	let std_in = &mut io::stdin();
	let _n = std_in.read_line(&mut input).unwrap();
	input
}

fn main() -> std::process::ExitCode {
	fn read_from_file(path: &std::path::Path) -> Option<String> {
		std::fs::read_to_string(path).ok()
	}

	fn write_to_file(path: &std::path::Path, content: String) {
		std::fs::write(path, content).unwrap();
	}

	let arguments = std::env::args().skip(1).collect::<Vec<_>>();
	let arguments = arguments.iter().map(String::as_str).collect::<Vec<_>>();

	run_cli(&arguments, &read_from_file, write_to_file, |p| Some(cli_input_resolver(p)))
}

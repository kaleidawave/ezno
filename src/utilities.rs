use std::fmt::Arguments;
use std::io;
use std::path::Path;

pub(crate) fn print_info() {
	print_to_cli(format_args!("{}@{}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION")));
	print_to_cli(format_args!("{}", env!("CARGO_PKG_DESCRIPTION")));
	print_to_cli(format_args!("For help run --help"))
}

#[cfg(target_family = "wasm")]
pub(crate) fn print_to_cli(arguments: Arguments) {
	super::wasm_bindings::log(&arguments.to_string());
}

#[cfg(not(target_family = "wasm"))]
pub(crate) fn print_to_cli(arguments: Arguments) {
	println!("{arguments}");
}

#[cfg(target_family = "windows")]
pub(crate) fn read_from_cli(std_in: &mut io::Stdin) -> (usize, String) {
	let mut input = String::new();
	let n = multiline_term_input::read_string(std_in, &mut input);
	(n, input)
}

#[cfg(target_family = "unix")]
pub(crate) fn read_from_cli(std_in: &mut io::Stdin) -> (usize, String) {
	let mut input = String::new();
	let n = std_in.read_line(&mut input).unwrap();
	(n, input)
}

#[cfg(target_family = "wasm")]
pub(crate) fn read_from_cli(_std_in: &mut io::Stdin) -> (usize, String) {
	let input = super::wasm_bindings::read_from_cli();
	(input.len(), input)
}

#[cfg(not(target_family = "wasm"))]
pub(crate) fn read_fs_path_to_string(path: impl AsRef<Path>) -> io::Result<String> {
	std::fs::read_to_string(path)
}

// TODO ...
#[cfg(target_family = "wasm")]
pub(crate) fn read_fs_path_to_string(path: impl AsRef<Path>) -> io::Result<String> {
	Ok(super::wasm_bindings::read_from_path(path.as_ref().to_str().expect("Invalid path")))
}

#[cfg(target_family = "wasm")]
pub(crate) fn get_cli_args() -> Vec<String> {
	let args = super::wasm_bindings::get_cli_args();
	args.split('\0').map(ToOwned::to_owned).collect::<Vec<_>>()
}

#[cfg(not(target_family = "wasm"))]
pub(crate) fn get_cli_args() -> Vec<String> {
	std::env::args().skip(1).collect()
}

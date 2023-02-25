use std::io;

pub(crate) fn print_info() {
	println!("{}@{}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"));
	println!("{}", env!("CARGO_PKG_DESCRIPTION"));
	println!("For help run --help")
}

#[cfg(target_os = "windows")]
pub(crate) fn read_string_from_cli(std_in: &mut io::Stdin) -> (usize, String) {
	let mut input = String::new();
	let n = multiline_term_input::read_string(std_in, &mut input);
	(n, input)
}

#[cfg(not(target_os = "windows"))]
pub(crate) fn read_string_from_cli(std_in: &mut io::Stdin) -> (usize, String) {
	let mut input = String::new();
	let n = std_in.read_line(&mut input).unwrap();
	(n, input)
}

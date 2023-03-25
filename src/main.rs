use ezno_lib::cli::run_cli;
use std::io;

fn fs_resolver(path: &std::path::Path) -> Option<(String, Vec<(usize, parser::EmptyCursorId)>)> {
	std::fs::read_to_string(path).ok().map(|content| (content, Vec::new()))
}

#[cfg(target_family = "windows")]
pub(crate) fn cli_input_resolver(prompt: &str) -> Option<String> {
	print!("{}> ", prompt);
	io::Write::flush(&mut io::stdout()).unwrap();
	let mut input = String::new();
	let std_in = &mut io::stdin();
	let _n = multiline_term_input::read_string(std_in, &mut input);
	Some(input)
}

#[cfg(target_family = "unix")]
pub(crate) fn cli_input_resolver(prompt: &str) -> Option<String> {
	print!("{}> ", prompt);
	io::Write::flush(&mut io::stdout()).unwrap();
	let mut input = String::new();
	let std_in = &mut io::stdin();
	let _n = std_in.read_line(&mut input).unwrap();
	Some(input)
}

fn main() {
	let arguments = std::env::args().skip(1).collect::<Vec<_>>();
	let arguments = arguments.iter().map(String::as_str).collect::<Vec<_>>();
	run_cli(&arguments, fs_resolver, cli_input_resolver)
}

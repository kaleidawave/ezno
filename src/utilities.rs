use std::fmt::Arguments;

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
	use std::io;

	println!("{arguments}");
	io::Write::flush(&mut io::stdout()).unwrap();
}

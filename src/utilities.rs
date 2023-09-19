use std::fmt::Arguments;

const SPONSORS_PATH: &str = "https://github.com/sponsors/kaleidawave";
const SPONSORS: Option<&'static str> = option_env!("SPONSORS");

pub(crate) fn print_info() {
	if let Some(run_id) = option_env!("GITHUB_RUN_ID") {
		print_to_cli(format_args!(
			"{}@{} ({run_id})",
			env!("CARGO_PKG_NAME"),
			env!("CARGO_PKG_VERSION")
		));
	} else {
		print_to_cli(format_args!("{}@{}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION")));
	}
	print_to_cli(format_args!("{}", env!("CARGO_PKG_DESCRIPTION")));
	print_to_cli(format_args!(
		"Repository: {}, License: {}",
		env!("CARGO_PKG_REPOSITORY"),
		env!("CARGO_PKG_LICENSE")
	));
	if let Some(sponsors) = SPONSORS {
		print_to_cli(format_args!("Supported by: {sponsors}. Join them @ {SPONSORS_PATH}"));
	} else {
		print_to_cli(format_args!("Sponsor the project @ {SPONSORS_PATH}"));
	}
	print_to_cli(format_args!("For help run --help"));
}

#[cfg(target_family = "wasm")]
pub(crate) fn print_to_cli(arguments: Arguments) {
	super::wasm_bindings::log(&arguments.to_string());
}

#[cfg(target_family = "wasm")]
pub(crate) fn print_to_cli_without_newline(arguments: Arguments) {
	// TODO :(
	super::wasm_bindings::log(&arguments.to_string());
}

#[cfg(not(target_family = "wasm"))]
pub(crate) fn print_to_cli(arguments: Arguments) {
	use std::io;

	println!("{arguments}");
	io::Write::flush(&mut io::stdout()).unwrap();
}

#[cfg(not(target_family = "wasm"))]
pub(crate) fn print_to_cli_without_newline(arguments: Arguments) {
	use std::io;

	print!("{arguments}");
	io::Write::flush(&mut io::stdout()).unwrap();
}

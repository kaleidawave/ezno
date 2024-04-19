use std::fmt::Arguments;

pub(crate) fn print_info() {
	if let Some(run_id) = option_env!("GITHUB_RUN_ID") {
		print_to_cli_with_break(format_args!(
			"{}@{} (#{run_id})",
			env!("CARGO_PKG_NAME"),
			env!("CARGO_PKG_VERSION")
		));
	} else {
		print_to_cli_with_break(format_args!(
			"{}@{}",
			env!("CARGO_PKG_NAME"),
			env!("CARGO_PKG_VERSION")
		));
	}
	print_to_cli(format_args!("{}", env!("CARGO_PKG_DESCRIPTION")));
	print_to_cli_with_break(format_args!(
		"Repository: {}, License: {}",
		env!("CARGO_PKG_REPOSITORY"),
		env!("CARGO_PKG_LICENSE")
	));
	print_to_cli(format_args!("For help run --help"));
	if let (Some(sponsors), Some(contributors)) =
		(option_env!("SPONSORS"), option_env!("CONTRIBUTORS"))
	{
		const SPONSORS_URL: &str = "https://github.com/sponsors/kaleidawave";

		print_to_cli(format_args!("\n---\n"));
		print_to_cli_with_break(format_args!("With thanks to"));
		print_to_cli(format_args!("Contributors:"));
		wrap_with_ident(contributors);
		print_to_cli(format_args!("Sponsors ({SPONSORS_URL}):"));
		wrap_with_ident(sponsors);
		print_to_cli(format_args!("and all the believers ✨"));
	}
}

fn wrap_with_ident(input: &str) {
	let mut buf = String::new();
	for part in input.split(',') {
		buf.push_str(part);
		buf.push_str(", ");
		if buf.len() > 20 {
			print_to_cli(format_args!("\t{buf}"));
			buf.clear();
		}
	}
	print_to_cli_with_break(format_args!("\t{buf}"));
}

/// Adds and extra new line afterwards
fn print_to_cli_with_break(arguments: Arguments) {
	print_to_cli(format_args!("{arguments}\n"));
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

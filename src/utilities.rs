use std::fmt::Arguments;

pub(crate) fn print_info() {
	if let (Some(run_id), Some(date)) =
		(option_env!("GITHUB_RUN_ID"), option_env!("GITHUB_RUN_DATE"))
	{
		print_to_cli(format_args!(
			"{}@{} (#{run_id} {date})\n",
			env!("CARGO_PKG_NAME"),
			env!("CARGO_PKG_VERSION")
		));
	} else {
		print_to_cli(format_args!("{}@{}\n", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION")));
	}
	print_to_cli(format_args!("{}", env!("CARGO_PKG_DESCRIPTION")));
	print_to_cli(format_args!(
		"Repository: {}, License: {}\n",
		env!("CARGO_PKG_REPOSITORY"),
		env!("CARGO_PKG_LICENSE")
	));
	print_to_cli(format_args!("For help run --help\n"));
	if let (Some(sponsors), Some(contributors)) =
		(option_env!("SPONSORS"), option_env!("CONTRIBUTORS"))
	{
		const SPONSORS_URL: &str = "https://github.com/sponsors/kaleidawave";

		print_to_cli(format_args!("With thanks to all supporters of the project including:"));
		print_to_cli(format_args!(
			"  Contributors (join them @ https://github.com/kaleidawave/ezno/issues):"
		));
		wrap_with_indent(contributors);
		print_to_cli(format_args!("  Sponsors (join them @ {SPONSORS_URL}):"));
		wrap_with_indent(sponsors);
	}
}

pub(crate) fn cli_input_resolver(prompt: &str) -> String {
	use std::io;
	print!("{prompt}> ");
	std::io::Write::flush(&mut io::stdout()).unwrap();
	let mut input = String::new();
	let std_in = &mut io::stdin();

	// multiline_term_input only works on windows for now
	#[cfg(target_family = "windows")]
	let _n = multiline_term_input::read_string(std_in, &mut input);

	#[cfg(target_family = "unix")]
	let _n = std_in.read_line(&mut input).unwrap();

	input
}

fn wrap_with_indent(input: &str) {
	// Four spaces is consitently rendered across terminals (unlike tabs)
	const INDENT: &str = "    ";
	const MAX_WIDTH: usize = 60;

	let mut buf = String::new();
	let mut iter = input.split(',').peekable();
	while let Some(part) = iter.next() {
		buf.push_str(part.trim());
		if let Some(next) = iter.peek() {
			if buf.len() + next.len() > MAX_WIDTH {
				buf.push(',');
				print_to_cli(format_args!("{INDENT}{buf}"));
				buf.clear();
			} else {
				buf.push_str(", ");
			}
		}
	}
	// TODO double check whether there needs to be another new line?
	print_to_cli(format_args!(
		"{INDENT}{buf}\n",
		INDENT = if buf.is_empty() { "" } else { INDENT }
	));
}

#[cfg(target_family = "wasm")]
pub(crate) fn print_to_cli(arguments: Arguments) {
	super::wasm_bindings::log(&arguments.to_string());
}

#[cfg(not(target_family = "wasm"))]
pub(crate) fn print_to_cli(arguments: Arguments) {
	use std::io::{self, Write};
	let stdout = io::stdout();
	let mut stdout = stdout.lock();

	// Ignore errors here for now
	let _ = writeln!(stdout, "{arguments}");
	let _ = Write::flush(&mut stdout);
}

#[derive(Debug, Copy, Clone)]
pub(crate) enum MaxDiagnostics {
	All,
	FixedTo(u16),
}

impl std::str::FromStr for MaxDiagnostics {
	type Err = std::num::ParseIntError;

	// Required method
	fn from_str(arg: &str) -> Result<Self, Self::Err> {
		if arg == "all" {
			Ok(Self::All)
		} else {
			std::str::FromStr::from_str(arg).map(Self::FixedTo)
		}
	}
}

impl Default for MaxDiagnostics {
	fn default() -> Self {
		Self::FixedTo(30)
	}
}

#[cfg(target_family = "wasm")]
pub struct FSFunction(pub js_sys::Function);

#[cfg(target_family = "wasm")]
impl checker::ReadFromFS for FSFunction {
	fn read_file(&self, path: &std::path::Path) -> Option<Vec<u8>> {
		self.0
			.call1(
				&wasm_bindgen::JsValue::null(),
				&wasm_bindgen::JsValue::from(path.display().to_string()),
			)
			.ok()
			.and_then(|s| s.as_string())
			.map(|s| s.into_bytes())
	}
}

#[cfg(not(target_family = "wasm"))]
pub struct FSFunction;

#[cfg(not(target_family = "wasm"))]
impl checker::ReadFromFS for FSFunction {
	fn read_file(&self, path: &std::path::Path) -> Option<Vec<u8>> {
		std::fs::read(path).ok()
	}
}

#[cfg(any(target_os = "windows", target_os = "linux", target_os = "macos"))]
pub(crate) fn upgrade_self() -> Result<String, Box<dyn std::error::Error>> {
	use release_downloader::{
		download_from_github, get_asset_urls_and_names_from_github_releases, replace_self,
		DownloadOptions,
	};

	let urls = get_asset_urls_and_names_from_github_releases(
		"kaleidawave",
		"ezno",
		DownloadOptions::default(),
	)?;

	let (_name, url) = urls.first().unwrap();

	let readable = download_from_github(url, None)?;

	replace_self(readable).unwrap();

	// TODO
	let msg = "done".to_owned();

	Ok(msg)
}

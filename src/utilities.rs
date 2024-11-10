use std::fmt::Arguments;

pub(crate) fn print_info() {
	if let Some(run_id) = option_env!("GITHUB_RUN_ID") {
		print_to_cli_with_break_after(format_args!(
			"{}@{} (#{run_id})",
			env!("CARGO_PKG_NAME"),
			env!("CARGO_PKG_VERSION")
		));
	} else {
		print_to_cli_with_break_after(format_args!(
			"{}@{}",
			env!("CARGO_PKG_NAME"),
			env!("CARGO_PKG_VERSION")
		));
	}
	print_to_cli(format_args!("{}", env!("CARGO_PKG_DESCRIPTION")));
	print_to_cli_with_break_after(format_args!(
		"Repository: {}, License: {}",
		env!("CARGO_PKG_REPOSITORY"),
		env!("CARGO_PKG_LICENSE")
	));
	print_to_cli_with_break_after(format_args!("For help run --help"));
	if let (Some(sponsors), Some(contributors)) =
		(option_env!("SPONSORS"), option_env!("CONTRIBUTORS"))
	{
		const SPONSORS_URL: &str = "https://github.com/sponsors/kaleidawave";

		print_to_cli_with_break_after(format_args!(
			"With thanks to all supporters of the project including:"
		));
		print_to_cli(format_args!(
			"  Contributors (join them @ https://github.com/kaleidawave/ezno/issues):"
		));
		wrap_with_ident(contributors);
		print_to_cli(format_args!("  Sponsors (join them @ {SPONSORS_URL}):"));
		wrap_with_ident(sponsors);
	}
}

#[cfg(target_family = "windows")]
pub(crate) fn cli_input_resolver(prompt: &str) -> String {
	use std::io;
	print!("{prompt}> ");
	std::io::Write::flush(&mut io::stdout()).unwrap();
	let mut input = String::new();
	let std_in = &mut io::stdin();
	let _n = multiline_term_input::read_string(std_in, &mut input);
	input
}

#[cfg(target_family = "unix")]
#[allow(clippy::unnecessary_wraps)]
pub(crate) fn cli_input_resolver(prompt: &str) -> String {
	use std::io;
	print!("{prompt}> ");
	io::Write::flush(&mut io::stdout()).unwrap();
	let mut input = String::new();
	let std_in = &mut io::stdin();
	let _n = std_in.read_line(&mut input).unwrap();
	input
}

fn wrap_with_ident(input: &str) {
	// Four spaces is stable across terminals (unlike tabs)
	const INDENT: &str = "    ";
	let mut buf = String::new();
	for part in input.split(',') {
		buf.push_str(part);
		buf.push_str(", ");
		if buf.len() > 40 {
			print_to_cli(format_args!("{INDENT}{buf}"));
			buf.clear();
		}
	}
	if !buf.is_empty() {
		print_to_cli_with_break_after(format_args!("{INDENT}{buf}"));
	} else {
		print_to_cli(format_args!("\n"))
	}
}

/// Adds and extra new line afterwards
fn print_to_cli_with_break_after(arguments: Arguments) {
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

#[derive(Debug, Copy, Clone)]
pub(crate) enum MaxDiagnostics {
	All,
	FixedTo(u16),
}

impl argh::FromArgValue for MaxDiagnostics {
	fn from_arg_value(value: &str) -> Result<Self, String> {
		if value == "all" {
			Ok(Self::All)
		} else {
			match std::str::FromStr::from_str(value) {
				Ok(value) => Ok(Self::FixedTo(value)),
				Err(reason) => Err(reason.to_string()),
			}
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

// yes i implemented it only using `native_tls`...
// TODO or(..., debug_assertions)
#[cfg(not(target_family = "wasm"))]
pub(crate) fn upgrade_self() -> Result<String, Box<dyn std::error::Error>> {
	use native_tls::{TlsConnector, TlsStream};
	use std::io::{BufRead, BufReader, BufWriter, Read, Write};
	use std::net::TcpStream;

	fn make_request(
		root: &str,
		path: &str,
	) -> Result<TlsStream<TcpStream>, Box<dyn std::error::Error>> {
		let url = format!("{root}:443");
		let tcp_stream = TcpStream::connect(url)?;
		let connector = TlsConnector::new()?;
		let mut tls_stream = connector.connect(root, tcp_stream)?;
		let request = format!(
			"GET {path} HTTP/1.1\r\n\
        Host: {root}\r\n\
        Connection: close\r\n\
        User-Agent: ezno-self-update\r\n\
        \r\n"
		);

		tls_stream.write_all(request.as_bytes())?;

		Ok(tls_stream)
	}

	let (version_name, assert_url) = {
		let mut stream = make_request("api.github.com", "/repos/kaleidawave/ezno/releases/latest")?;

		let mut response = String::new();
		stream.read_to_string(&mut response)?;

		// Skip headers
		let mut lines = response.lines();
		for line in lines.by_ref() {
			if line.is_empty() {
				break;
			}
		}

		use simple_json_parser::*;
		let body = lines.next().ok_or("No body on API request")?;

		#[cfg(target_os = "windows")]
		const EXPECTED_END: &str = "windows.exe";
		#[cfg(target_os = "linux")]
		const EXPECTED_END: &str = "linux";
		#[cfg(target_os = "macos")]
		const EXPECTED_END: &str = "macos";

		let mut required_binary = None;
		let mut version_name = None;

		// Name comes before assets so okay here on exit signal
		let result = parse_with_exit_signal(body, |keys, value| {
			if let [JSONKey::Slice("name")] = keys {
				if let RootJSONValue::String(s) = value {
					version_name = Some(s.to_owned());
				}
			} else if let [JSONKey::Slice("assets"), JSONKey::Index(_), JSONKey::Slice("browser_download_url")] =
				keys
			{
				if let RootJSONValue::String(s) = value {
					if s.ends_with(EXPECTED_END) {
						required_binary = Some(s.to_owned());
						return true;
					}
				}
			}
			false
		});

		if let Err(JSONParseError { at, reason }) = result {
			return Err(Box::from(format!("JSON parse error: {reason:?} @ {at}")));
		}

		(
			version_name.unwrap_or_default(),
			required_binary.ok_or("could not find binary for platform")?,
		)
	};

	let actual_asset_url = {
		let url = assert_url.strip_prefix("https://github.com").ok_or_else(|| {
			format!("Assert url {assert_url:?} does not start with 'https://github.com'")
		})?;
		let response = make_request("github.com", url)?;
		let mut reader = BufReader::new(response);

		// Read the status line
		let mut status_line = String::new();
		reader.read_line(&mut status_line)?;

		// Check for successful redirect
		if !status_line.contains("302 Found") {
			return Err(Box::from(format!("Expected redirect, got {status_line:?}")));
		}

		let mut location = None;
		loop {
			let mut line = String::new();
			reader.read_line(&mut line)?;
			if line == "\r\n" {
				break;
			}
			if let l @ Some(_) = line.strip_prefix("Location: ") {
				location = l.map(str::to_string);
				break;
			}
		}

		location.ok_or("no location")?
	};

	// Finally do download
	let url = actual_asset_url
		.strip_prefix("https://objects.githubusercontent.com")
		.ok_or_else(|| {
			format!("Assert url {assert_url:?} does not start with 'https://objects.githubusercontent.com'")
		})?
		.trim_end();

	let response = make_request("objects.githubusercontent.com", url)?;

	let mut reader = BufReader::new(response);

	// Read the status line
	let mut status_line = String::new();
	reader.read_line(&mut status_line)?;

	// Check for successful status code
	if !status_line.contains("200 OK") {
		return Err(Box::from(format!("Got status {status_line:?}")));
	}

	// Read and discard headers
	let mut headers = String::new();
	loop {
		let mut line = String::new();
		reader.read_line(&mut line)?;
		if line == "\r\n" {
			break;
		}
		headers.push_str(&line);
	}

	// Open the file to write the body
	let new_binary = "new-ezno.exe";
	let mut file = BufWriter::new(std::fs::File::create(new_binary)?);

	// Read the body and write it to the file
	let mut buffer = Vec::new();
	reader.read_to_end(&mut buffer)?;
	file.write_all(&buffer)?;

	self_replace::self_replace(new_binary)?;
	std::fs::remove_file(new_binary)?;

	Ok(version_name)
}

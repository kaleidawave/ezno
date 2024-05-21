use std::{
	fmt::Arguments,
	path::{Path, PathBuf},
};

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

trait FileSystem {
	fn read_directory(&self) -> Vec<String>;
}

// TODO
/// - root
/// - replace read_dir with T above
/// https://www.malikbrowne.com/blog/a-beginners-guide-glob-patterns/
pub(crate) fn parse_path_argument(p: &str) -> Vec<PathBuf> {
	fn append_bases(base: &Path, parts: &mut Vec<PathBuf>) {
		for entry in std::fs::read_dir(base).unwrap() {
			let entry = entry.unwrap();
			if entry.file_type().is_ok_and(|kind| kind.is_dir()) {
				append_bases(&entry.path(), parts)
			} else {
				// TODO * etc
				parts.push(entry.path());
			}
		}
	}

	let mut parts = Vec::<PathBuf>::new();
	for s in p.split(',') {
		let buf = PathBuf::from(s);
		if s.contains("*") {
			// TODO root
			let mut current = PathBuf::new();
			for part in buf.components() {
				if part.as_os_str() == "**" {
					let directories = std::fs::read_dir(&current).unwrap();
					// TODO recursive
					for directory in directories {
						let directory = directory.unwrap();
						append_bases(&directory.path(), &mut parts)
					}
				} else {
					// TODO
					// else if part.as_os_str().to_str().is_some_and(|s| s.contains('*')) {
					// 	let directories = std::fs::read_dir(&current).unwrap();
					// 	for directory in directories {
					// 		let directory = directory.unwrap();
					// 		if directory.file_type().is_ok_and(|r#type| r#type.is_file()) {
					// 			parts.push(current.join(directory.file_name()));
					// 		}
					// 	}
					// 	break;
					// }
					current.push(part);
				}
			}
		} else {
			parts.push(buf);
		}
	}
	parts
}

// yes i implemented it only using `native_tls`...
// TODO or(..., debug_assertions)
#[cfg(not(target_family = "wasm"))]
pub(crate) fn upgrade_self() -> Result<(), std::io::Error> {
	use native_tls::{TlsConnector, TlsStream};
	use std::io::{BufRead, BufReader, BufWriter, Read, Write};
	use std::net::TcpStream;

	fn make_request(root: &str, path: &str) -> Result<TlsStream<TcpStream>, std::io::Error> {
		let url = format!("{root}:443");
		let tcp_stream = TcpStream::connect(url)?;
		let connector = TlsConnector::new().unwrap();
		let mut tls_stream = connector.connect(root, tcp_stream).unwrap();
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

	let assert_url = {
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
		let body = lines.next().expect("No body?");

		#[cfg(target_os = "windows")]
		const EXPECTED_END: &str = "windows.exe";
		#[cfg(target_os = "linux")]
		const EXPECTED_END: &str = "linux";

		let mut required_binary = None;
		let result = parse_with_exit_signal(&body, |keys, value| {
			if let [JSONKey::Slice("assets"), JSONKey::Index(_), JSONKey::Slice("browser_download_url")] =
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
			eprintln!("{reason:?} @ {at}");
		}

		required_binary.expect("couldn't find binary")
	};

	let actual_asset_url = {
		let url = assert_url.strip_prefix("https://github.com").unwrap();
		let response = make_request("github.com", url)?;
		let mut reader = BufReader::new(response);

		// Read the status line
		let mut status_line = String::new();
		reader.read_line(&mut status_line)?;

		// Check for successful redirect
		if !status_line.contains("302 Found") {
			panic!("error {status_line}")
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

		location.expect("no location")
	};

	// Finally do download
	let url =
		actual_asset_url.strip_prefix("https://objects.githubusercontent.com").unwrap().trim_end();

	let response = make_request("objects.githubusercontent.com", dbg!(url))?;

	let mut reader = BufReader::new(response);

	// Read the status line
	let mut status_line = String::new();
	reader.read_line(&mut status_line)?;

	// Check for successful status code
	if !status_line.contains("200 OK") {
		panic!("error {status_line}")
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

	println!("Update downloaded successfully");

	self_replace::self_replace(&new_binary)?;
	std::fs::remove_file(&new_binary)?;

	Ok(())
}

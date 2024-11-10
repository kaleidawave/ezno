use std::error::Error;
use std::fs::{read_to_string, File};
use std::io::Write;
use std::mem;
use std::path::Path;

fn main() -> Result<(), Box<dyn Error>> {
	println!("cargo:rerun-if-changed=specification.md");
	println!("cargo:rerun-if-changed=staging.md");
	println!("cargo:rerun-if-changed=to_implement.md");

	let out_path = Path::new(&std::env::var("OUT_DIR")?).join("specification.rs");
	let mut out = File::create(out_path)?;

	if cfg!(not(feature = "just-staging")) {
		let specification = read_to_string("./specification.md")?;
		markdown_lines_append_test_to_rust(specification.lines().enumerate(), &mut out)?;
	}

	if cfg!(feature = "staging") {
		let staging = read_to_string("./staging.md")?;
		writeln!(&mut out, "mod staging {{ ").unwrap();
		writeln!(&mut out, "use super::{{check_expected_diagnostics, TypeCheckOptions}}; ")
			.unwrap();
		markdown_lines_append_test_to_rust(staging.lines().enumerate(), &mut out)?;
		writeln!(&mut out, "}}").unwrap();
	}

	if cfg!(feature = "all") {
		let to_implement = read_to_string("./to_implement.md")?;
		writeln!(&mut out, "mod to_implement {{ ").unwrap();
		writeln!(&mut out, "use super::{{check_expected_diagnostics, TypeCheckOptions}}; ")
			.unwrap();
		markdown_lines_append_test_to_rust(to_implement.lines().enumerate(), &mut out)?;
		writeln!(&mut out, "}}").unwrap();
	}

	Ok(())
}

const DEFAULT_FILE_PATH: &str = "main.tsx";

fn markdown_lines_append_test_to_rust(
	mut lines: std::iter::Enumerate<std::str::Lines<'_>>,
	out: &mut File,
) -> Result<(), Box<dyn Error>> {
	let mut first_section = true;

	while let Some((heading_idx, line)) = lines.next() {
		if let Some(section_heading) = line.strip_prefix("### ") {
			if !first_section {
				writeln!(out, "}}").unwrap();
			}
			first_section = false;
			let section_heading = heading_to_rust_identifier(section_heading);
			writeln!(out, "mod {section_heading} {{").unwrap();
			continue;
		}

		if !line.starts_with("#### ") {
			continue;
		}

		let heading = line.strip_prefix("####").unwrap().trim_start();
		let test_title = heading_to_rust_identifier(heading);

		pub struct File<'a> {
			path: &'a str,
			code: String,
		}

		// pub struct Block {
		// 	/// Vec for FS tests
		// 	files: Vec<File>,
		// 	expected_diagnostics: Vec<String>,
		// 	options: Vec<String>
		// }

		let files = {
			let mut files = Vec::<File>::new();
			let mut current_filename = None;
			for (_, line) in lines.by_ref() {
				// Also handles TSX
				if line.starts_with("```ts") {
					break;
				}
			}
			let mut code = String::new();

			for (_, line) in lines.by_ref() {
				if let Some(path) = line.strip_prefix("// in ") {
					if !code.trim().is_empty() {
						files.push(File {
							path: current_filename.unwrap_or(DEFAULT_FILE_PATH),
							code: mem::take(&mut code),
						});
					}
					current_filename = Some(path);
					continue;
				}
				if line == "```" {
					break;
				}
				code.push_str(line);
				code.push('\n')
			}
			files.push(File { path: current_filename.unwrap_or(DEFAULT_FILE_PATH), code });
			files
		};

		let (expected_diagnostics, options) = {
			let mut expected_diagnostics = Vec::new();
			let mut options = None::<Vec<&str>>;
			for (_, line) in lines.by_ref() {
				if let (Some(args), false) = (line.strip_prefix("With "), options.is_some()) {
					options = Some(args.split(',').collect());
				} else if line.starts_with("#") {
					panic!("block with no diagnostics or break between in {test_title}")
				} else if let Some(diagnostic) = line.strip_prefix("-") {
					let error = diagnostic.trim().replace('\\', "").replace('"', "\\\"");
					expected_diagnostics.push(format!("\"{}\"", error))
				} else if !expected_diagnostics.is_empty() {
					break;
				}
			}
			(expected_diagnostics, options)
		};

		let expected_diagnostics = expected_diagnostics.join(", ");

		let heading_idx = heading_idx + 1;
		// TODO don't allocate
		let code_as_list = files
			.into_iter()
			.map(|File { path, code }| format!("(\"{path}\",r#\"{code}\"#),"))
			.reduce(|mut acc, slice| {
				acc.push_str(&slice);
				acc
			})
			.unwrap();

		let options = if let Some(options) = options {
			let arguments = options
				.into_iter()
				.map(|value| format!("{value}: true"))
				.reduce(|mut acc, slice| {
					acc.push_str(&slice);
					acc.push_str(", ");
					acc
				})
				.unwrap();
			format!("Some(super::TypeCheckOptions {{ {arguments}, ..super::TypeCheckOptions::default() }})")
		} else {
			format!("None")
		};

		writeln!(
			out,
			"#[test] fn {test_title}() {{ 
                super::check_expected_diagnostics(
					\"{heading}\", {heading_idx}, 
					&[{code_as_list}], &[{expected_diagnostics}], 
					{options}
				)
            }}",
		)?;
	}
	if !first_section {
		writeln!(out, "}}").unwrap();
	}

	Ok(())
}

fn heading_to_rust_identifier(heading: &str) -> String {
	heading
		.replace("...", "")
		.replace([' ', '-', '/', '.', '+'], "_")
		.replace(['*', '\'', '`', '"', '&', '!', '(', ')', ',', ':'], "")
		.to_lowercase()
}

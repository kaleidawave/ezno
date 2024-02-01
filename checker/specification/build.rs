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
		writeln!(&mut out, "mod staging {{ use super::check_errors; ").unwrap();
		markdown_lines_append_test_to_rust(staging.lines().enumerate(), &mut out)?;
		writeln!(&mut out, "}}").unwrap();
	}

	if cfg!(feature = "all") {
		let to_implement = read_to_string("./to_implement.md")?;
		writeln!(&mut out, "mod to_implement {{ use super::check_errors; ").unwrap();
		markdown_lines_append_test_to_rust(to_implement.lines().enumerate(), &mut out)?;
		writeln!(&mut out, "}}").unwrap();
	}

	Ok(())
}

const DEFAULT_FILE_PATH: &str = "main.ts";

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

		let blocks = {
			let mut blocks = Vec::new();
			let mut current_filename = None;
			for (_, line) in lines.by_ref() {
				if line == "```ts" {
					break;
				}
			}
			let mut code = String::new();

			for (_, line) in lines.by_ref() {
				if let Some(path) = line.strip_prefix("// in ") {
					if !code.trim().is_empty() {
						blocks.push((
							current_filename.unwrap_or(DEFAULT_FILE_PATH),
							mem::take(&mut code),
						));
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
			blocks.push((current_filename.unwrap_or(DEFAULT_FILE_PATH), code));
			blocks
		};
		let errors = {
			let mut errors = Vec::new();
			for (_, line) in lines.by_ref() {
				if line.starts_with("#") {
					panic!("block with no diagnostics or break between in {test_title}")
				} else if line.starts_with('-') {
					let error =
						line.strip_prefix("- ").unwrap().replace('\\', "").replace('"', "\\\"");
					errors.push(format!("\"{}\"", error))
				} else if !errors.is_empty() {
					break;
				}
			}
			errors
		};

		let errors = errors.join(", ");

		let heading_idx = heading_idx + 1;
		let code = blocks
			.into_iter()
			.map(|(path, content)| format!("(\"{path}\",r#\"{content}\"#),"))
			.fold(String::new(), |mut acc, cur| {
				acc.push_str(&cur);
				acc
			});

		writeln!(
			out,
			"#[test] fn {test_title}() {{ 
                super::check_errors(\"{heading}\", {heading_idx}, &[{code}], &[{errors}])
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
		.replace([' ', '-', '/', '&'], "_")
		.replace(['*', '\'', '`', '"', '!', '(', ')', ','], "")
		.to_lowercase()
}

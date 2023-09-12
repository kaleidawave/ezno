use std::error::Error;
use std::fs::{read_to_string, File};
use std::io::Write;
use std::path::Path;

fn main() -> Result<(), Box<dyn Error>> {
	let out_path = Path::new(&std::env::var("OUT_DIR")?).join("specification.rs");
	let mut out = File::create(out_path)?;

	let specification = read_to_string("./specification.md")?;
	let mut lines = specification.lines().enumerate();

	while let Some((_, line)) = lines.next() {
		if line == "## Specification" {
			break;
		}
	}

	markdown_lines_append_test_to_rust(lines, &mut out)?;

	if cfg!(feature = "all") {
		let not_yet_implemented = read_to_string("./not_yet_implemented.md")?;
		let mut lines = not_yet_implemented.lines().enumerate();

		while let Some((_, line)) = lines.next() {
			if line == "## TODO" {
				break;
			}
		}

		writeln!(&mut out, "mod todo {{ use super::check_errors; ").unwrap();
		markdown_lines_append_test_to_rust(lines, &mut out)?;
		writeln!(&mut out, "}}").unwrap();
	}

	Ok(())
}

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
			let section_heading = heading_to_rust_name(section_heading);
			writeln!(out, "mod {section_heading} {{").unwrap();
			continue;
		}

		if !line.starts_with("#### ") {
			continue;
		}
		let heading = line.strip_prefix("####").unwrap().trim_start();
		let test_title = heading_to_rust_name(heading);

		let code = {
			while let Some((_, line)) = lines.next() {
				if line == "```ts" {
					break;
				}
			}
			let mut code = String::new();
			while let Some((_, line)) = lines.next() {
				if line == "```" {
					break;
				}
				code.push_str(line);
				code.push('\n')
			}
			// Escape "
			code.replace('"', "\\\"")
		};
		let errors = {
			let mut errors = Vec::new();
			while let Some((_, line)) = lines.next() {
				if line.is_empty() || !line.starts_with('-') {
					if !errors.is_empty() {
						break;
					}
				} else {
					let error = line.strip_prefix("- ").unwrap().replace('"', "\\\"");
					errors.push(format!("\"{}\"", error))
				}
			}
			errors
		};

		let errors = errors.join(", ");
		let heading_idx = heading_idx + 1;

		writeln!(
			out,
			"#[test] fn {test_title}() {{ 
                super::check_errors(\"{heading}\", {heading_idx}, \"{code}\", &[{errors}])
            }}",
		)?;
	}
	writeln!(out, "}}").unwrap();

	Ok(())
}

fn heading_to_rust_name(heading: &str) -> String {
	heading
		.replace([' ', '-', '&'], "_")
		.replace(['*', '\'', '!', '(', ')', ','], "")
		.to_lowercase()
}

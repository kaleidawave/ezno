use std::error::Error;
use std::fs::{read_to_string, File};
use std::io::Write;
use std::path::Path;

fn main() -> Result<(), Box<dyn Error>> {
	let out_path = Path::new(&std::env::var("OUT_DIR")?).join("specification.rs");
	let mut out = File::create(out_path)?;

	let specification = read_to_string("./specification.md")?;
	let mut lines = specification.lines();

	while let Some(line) = lines.next() {
		if line == "## Specification" {
			break;
		}
	}

	while let Some(line) = lines.next() {
		if line.is_empty() || line.starts_with("### ") {
			continue;
		}
		let heading =
			line.strip_prefix("####").unwrap().trim_start().replace(' ', "_").to_lowercase();

		let code = {
			while let Some(line) = lines.next() {
				if line == "```ts" {
					break;
				}
			}
			let mut code = String::new();
			while let Some(line) = lines.next() {
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
			while let Some(line) = lines.next() {
				if line.is_empty() || !line.starts_with('-') {
					if errors.is_empty() {
						continue;
					} else {
						break;
					}
				}
				let error = line.strip_prefix("- ").unwrap().replace('"', "\\\"");
				errors.push(format!("\"{}\"", error))
			}
			errors
		};

		let errors = errors.join(", ");

		out.write_fmt(format_args!(
			"#[test] fn {heading}() {{ 
                check_errors(\"{code}\", &[{errors}])
            }}\n",
		))?;
	}

	Ok(())
}

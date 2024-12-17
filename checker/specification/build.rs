use std::{error::Error, fs::{read_to_string, File}, io::Write, path::Path};

fn main() -> Result<(), Box<dyn Error>> {
	println!("cargo:rerun-if-changed=specification.md");
	println!("cargo:rerun-if-changed=staging.md");
	println!("cargo:rerun-if-changed=to_implement.md");

	let out_path = Path::new(&std::env::var("OUT_DIR")?).join("specification.rs");
	let mut out = File::create(out_path)?;

	if cfg!(feature = "base") {
		let specification = read_to_string("./specification.md")?;
		specification_to_tests(&specification, &mut out)?;
	}

	if cfg!(feature = "staging") {
		let staging = read_to_string("./staging.md")?;
		writeln!(&mut out, "mod staging {{ ").unwrap();
		writeln!(&mut out, "use super::{{check_expected_diagnostics, TypeCheckOptions}}; ")
			.unwrap();
		specification_to_tests(&staging, &mut out)?;
		writeln!(&mut out, "}}").unwrap();
	}

	if cfg!(feature = "to_implement") {
		let to_implement = read_to_string("./to_implement.md")?;
		writeln!(&mut out, "mod to_implement {{ ").unwrap();
		writeln!(&mut out, "use super::{{check_expected_diagnostics, TypeCheckOptions}}; ")
			.unwrap();
		specification_to_tests(&to_implement, &mut out)?;
		writeln!(&mut out, "}}").unwrap();
	}

	Ok(())
}

use simple_markdown_parser;

fn specification_to_tests(source: &str, out: &mut File) -> Result<(), Box<dyn Error>> {
	let mut current_unit = Unit::empty();
	let mut current_section = "";

	// Using the fact that it is linear, we don't need the heading chain
	let _ = simple_markdown_parser::parse(source, |item| {
		if let simple_markdown_parser::MarkdownElement::Heading { level: 3, text } = item {
			if !current_section.is_empty() {
				writeln!(out, "}}").unwrap();
			}
			current_section = text.0;
			writeln!(out, "mod {current_section} {{").unwrap();
		} else if let simple_markdown_parser::MarkdownElement::Heading { level: 4, text } = item {
			current_unit.to_rust(out);
			current_unit = Unit::empty();
			current_unit.name = text.0;
		} else if let simple_markdown_parser::MarkdownElement::CodeBlock { language: _, content } =
			item
		{
			current_unit.modules = code_to_modules(content);
		} else if let simple_markdown_parser::MarkdownElement::Paragraph(item) = item {
			if let Some(options) = item.strip_prefix("With") {
				current_unit.options = options.split(',').collect();
			}
		} else if let simple_markdown_parser::MarkdownElement::ListItem { level: _, text } = item {
			current_unit.expected_diagnostics.push(text.0);
		}
	});

	if !current_section.is_empty() {
		writeln!(out, "}}").unwrap();
	}

	Ok(())
}

struct Module<'a> {
	path: &'a str,
	code: &'a str,
}

struct Unit<'a> {
	name: &'a str,
	modules: Vec<Module<'a>>,
	expected_diagnostics: Vec<&'a str>,
	options: Vec<&'a str>,
}

impl Unit<'_> {
	pub fn empty() -> Unit<'static> {
		Unit {
			name: "",
			modules: Vec::new(),
			expected_diagnostics: Vec::new(),
			options: Vec::new(),
		}
	}

	pub fn is_empty(&self) -> bool {
		self.name.is_empty()
	}

	pub fn to_rust(self, out: &mut impl std::io::Write) {
		assert!(!self.is_empty());

		let heading_idx = 0;

		// &[{code_as_list}],
		// &[{expected_diagnostics}],
		// {options}
		writeln!(
			out,
			"#[test] fn {rust_name}() {{ 
				super::check_expected_diagnostics(
					\"{heading}\", {heading_idx},
				)
			}}",
			rust_name = heading_to_rust_identifier(self.name),
			heading = self.name,
		)
		.unwrap();

		// Code
		{
			write!(out, "&[").unwrap();
			for Module { path, code } in self.modules {
				write!(out, "(\"{path}\",r#\"{code}\"#),");
			}
			write!(out, "],").unwrap();
		}

		// Diagnostics
		{
			write!(out, "&[").unwrap();
			for diagnostic in self.expected_diagnostics {
				write!(out, "r#\"{diagnostic}\"#,");
			}
			write!(out, "],").unwrap();
		}

		// Options
		if !self.options.is_empty() {
			write!(out, "Some(super::TypeCheckOptions {{").unwrap();
			for option in self.options {
				write!(out, "{option}: true, ").unwrap();
			}
			write!(out, "..super::TypeCheckOptions::default() }})").unwrap();
		} else {
			write!(out, "None").unwrap();
		}

		writeln!(out, ")}}").unwrap();
	}
}

/// TODO move to `simple_markdown` for links
fn heading_to_rust_identifier(heading: &str) -> String {
	heading
		.replace("...", "")
		.replace([' ', '-', '/', '.', '+'], "_")
		.replace(['*', '\'', '`', '"', '&', '!', '(', ')', ',', ':'], "")
		.to_lowercase()
}

const DEFAULT_MODULE_PATH: &str = "main.tsx";

fn code_to_modules(code: &str) -> Vec<Module> {
	let mut modules = Vec::<Module>::new();
	let mut current_module_name = None;
	let mut start: usize = 0;

	for line in code.lines() {
		if let Some(path) = line.strip_prefix("// in ") {
			let offset = unsafe { line.as_ptr().offset_from(code.as_ptr()) } as usize;
			let code = code[start..offset].trim();
			if !code.is_empty() {
				let path = current_module_name.unwrap_or(DEFAULT_MODULE_PATH);
				modules.push(Module { path, code });
			}
			current_module_name = Some(path);
			start = offset;
		}
	}

	let path = current_module_name.unwrap_or(DEFAULT_MODULE_PATH);
	modules.push(Module { path, code: &code[start..] });
	modules
}

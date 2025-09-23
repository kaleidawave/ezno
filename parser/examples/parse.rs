use std::{path::Path, time::Instant};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::{
	self,
	termcolor::{ColorChoice, StandardStream},
	Config,
};
use ezno_parser::{ASTNode, Comments, Module, ParseError, ParseOptions, ToStringOptions};
use source_map::FileSystem;

type Files = source_map::MapFileStore<source_map::WithPathMap>;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let mut arguments = std::env::args();
	let _ = arguments.next();

	let first_argument = arguments.next();

	if let Some("--interactive") = first_argument.as_deref() {
		run_interactive();
		return Ok(());
	}

	let path = first_argument.ok_or("expected path argument")?;

	let mut parse_options = ParseOptions::default();

	let mut print_ast = false;
	let mut print_output = false;
	let mut print_source_maps = false;
	let mut timings = false;
	let mut parse_imports = false;
	let mut pretty = false;

	for argument in arguments {
		match argument.as_str() {
			"--no-comments" => {
				parse_options.comments = Comments::None;
			}
			"--doc-comments" => {
				parse_options.comments = Comments::JustDocumentation;
			}
			"--keywords" => {
				parse_options.record_keyword_positions = true;
			}
			"--extras" => {
				parse_options.custom_function_headers = true;
				parse_options.destructuring_type_annotation = true;
				parse_options.jsx = true;
				parse_options.is_expressions = true;
				parse_options.special_jsx_attributes = true;
				parse_options.extra_operators = true;
				parse_options.reversed_imports = true;
			}
			"--pretty" => {
				parse_options.retain_blank_lines = true;
				pretty = true;
			}
			"--partial" => {
				parse_options.partial_syntax = true;
			}
			"--no-type-annotations" => {
				parse_options.type_annotations = false;
			}
			"--type-definition-module" => {
				parse_options.type_definition_module = true;
			}
			"--top-level-html" => {
				parse_options.top_level_html = true;
			}
			"--source-map" => {
				print_source_maps = true;
			}
			"--timings" => {
				timings = true;
			}
			"--parse-imports" => {
				parse_imports = true;
			}
			"--ast" => {
				print_ast = true;
			}
			"--to-string" => {
				print_output = true;
			}
			argument => {
				eprintln!("unknown argument {argument:?}");
			}
		}
	}

	let mut fs = Files::default();

	let to_string_options = if print_output || pretty {
		Some(ToStringOptions {
			expect_markers: true,
			include_type_annotations: parse_options.type_annotations,
			pretty,
			comments: if pretty { Comments::All } else { Comments::None },
			// 60 is temp
			max_line_length: if pretty { 60 } else { u8::MAX },
			..Default::default()
		})
	} else {
		None
	};

	parse_path(
		path.as_ref(),
		timings,
		parse_imports,
		&parse_options,
		print_ast,
		print_source_maps,
		&to_string_options,
		&mut fs,
	)
}

fn parse_path(
	path: &Path,
	timings: bool,
	parse_imports: bool,
	parse_options: &ParseOptions,
	print_ast: bool,
	print_source_maps: bool,
	to_string_options: &Option<ToStringOptions>,
	fs: &mut Files,
) -> Result<(), Box<dyn std::error::Error>> {
	const EIGHT_MEGA_BYTES: usize = 8 * 1024 * 1024;

	let source = std::fs::read_to_string(path)?;
	let source_id = fs.new_source_id(path.into(), source.to_owned());

	eprintln!("parsing {:?} ({:?} bytes)", path.display(), source.len());
	let now = Instant::now();
	let extension: &str = path.extension().and_then(std::ffi::OsStr::to_str).unwrap_or_default();
	let type_annotations = extension.contains("ts");
	let jsx = extension.contains('x');

	let parse_options = ParseOptions { jsx, type_annotations, ..*parse_options };

	let on = source.clone();

	// Run in thread as stack is large and can oveflow
	let result = std::thread::Builder::new()
		.stack_size(EIGHT_MEGA_BYTES)
		.spawn(move || Module::from_string_with_options(on, parse_options, None))
		.unwrap()
		.join()
		.unwrap();

	match result {
		Ok((module, state)) => {
			if timings {
				eprintln!("Parsed in: {:?}", now.elapsed());
			} else if print_ast {
				println!("{module:#?}");
			} else {
				eprintln!("Successfully parsed source");
			}

			if let Some(to_string_options) = to_string_options {
				let now = Instant::now();

				let (output, source_map) =
					module.to_string_with_source_map(to_string_options, source_id, fs);

				if timings {
					eprintln!("ToString'ed in: {:?}", now.elapsed());
				}

				println!("{output}");
				if print_source_maps {
					let sm = source_map.unwrap().to_json(fs);
					println!("{sm}");
				}
			}

			if parse_options.record_keyword_positions {
				println!("{:?}", state.keyword_positions.as_ref());
			}

			if parse_imports {
				for import in &state.constant_imports {
					// Don't reparse files (+ catches cycles)
					let resolved_path = path.parent().unwrap().join(import);
					if fs.get_paths().contains_key(&resolved_path) {
						continue;
					}
					let () = parse_path(
						&resolved_path,
						timings,
						parse_imports,
						&parse_options,
						print_ast,
						print_source_maps,
						to_string_options,
						fs,
					)?;
				}
			}
		}
		Err(ParseError { reason, position }) => {
			let writer = StandardStream::stderr(ColorChoice::Always);
			let config = Config::default();

			let diagnostic = Diagnostic::error()
				.with_labels(vec![Label::primary(source_id, position)
					.with_message(format!("ParseError: {reason}"))]);
			term::emit(&mut writer.lock(), &config, &fs.into_code_span_store(), &diagnostic)?;
			// Err(Box::<dyn std::error::Error>::from(ParseError { reason, position }))
		}
	}
	Ok(())
}

// For spectra testing
fn run_interactive() {
	use std::io::{self, BufRead};

	let stdin = io::stdin();
	let mut buf = Vec::new();

	println!("start");

	for line in stdin.lock().lines() {
		let Ok(line) = line else { break };

		if line == "close" {
			if !buf.is_empty() {
				eprintln!("no end to message {buf:?}");
			}
			break;
		}

		if line == "end" {
			let output = String::from_utf8_lossy(&buf);

			let parse_options = ParseOptions::all_features();
			let module = Module::from_string_with_options(output.into_owned(), parse_options, None);

			match module {
				Ok((item, _)) => {
					if let [item] = &item.items.as_slice() {
						if let ezno_parser::StatementOrDeclaration::Expression(item) = item {
							// Unwrap multiple expression
							let item = item.get_inner();
							println!("{item:#?}");
						} else {
							println!("{item:#?}");
						}
					} else {
						println!("{item:#?}");
					}
				}
				Err(error) => {
					println!("error: {error:?}");
				}
			}

			println!("end");
			buf.clear();
			continue;
		}

		buf.extend_from_slice(line.as_bytes());
		buf.push(b'\n');
	}

	// println!("Finished!");
}

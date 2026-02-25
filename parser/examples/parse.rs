use std::{path::Path, time::Instant};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::{
	self, Config,
	termcolor::{ColorChoice, StandardStream},
};
use ezno_parser::{ASTNode, Module, ParseError, ParseState, SourceId, options};
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

	let mut parse_options = options::ParseOptions::default();
	let mut to_string_options = options::ToStringOptions {
		expect_markers: true,
		include_type_annotations: true,
		pretty: false,
		comments: options::CommentsOption::None,
		// 60 is temp
		max_line_length: u8::MAX,
		..Default::default()
	};

	let mut print_ast = false;
	let mut print_output = false;
	let mut print_source_maps = false;
	let mut timings = false;
	let mut parse_imports = false;
	let mut increase_stack_size = false;

	for argument in arguments {
		match argument.as_str() {
			"--no-comments" => {
				parse_options.comments = options::CommentsOption::None;
				to_string_options.comments = options::CommentsOption::None;
			}
			"--doc-comments" => {
				parse_options.comments = options::CommentsOption::JustDocumentation;
			}
			"--keywords" => {
				parse_options.features.record_keyword_positions = true;
			}
			"--extras" => {
				parse_options = options::ParseOptions::all();
			}
			"--pretty" => {
				parse_options.features.retain_blank_lines = true;
				to_string_options.pretty = true;
				to_string_options.max_line_length = 60;
				to_string_options.comments = options::CommentsOption::All;
			}
			"--partial" => {
				parse_options.features.partial_syntax = true;
			}
			"--no-type-annotations" => {
				parse_options.type_annotations = options::TypeAnnotationOption::AsErrors;
			}
			"--type-definition-module" => {
				parse_options.type_annotations = options::TypeAnnotationOption::Definitions;
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
			"--increase-stack-size" => {
				increase_stack_size = true;
			}
			argument => {
				eprintln!("unknown argument {argument:?}");
			}
		}
	}

	let mut fs = Files::default();

	let to_string_options = if print_output { Some(to_string_options) } else { None };

	parse_path(
		path.as_ref(),
		timings,
		parse_imports,
		&parse_options,
		print_ast,
		print_source_maps,
		increase_stack_size,
		&to_string_options,
		&mut fs,
	)
}

fn parse_path(
	path: &Path,
	timings: bool,
	parse_imports: bool,
	parse_options: &options::ParseOptions,
	print_ast: bool,
	print_source_maps: bool,
	increase_stack_size: bool,
	to_string_options: &Option<options::ToStringOptions>,
	fs: &mut Files,
) -> Result<(), Box<dyn std::error::Error>> {
	let source = std::fs::read_to_string(path)?;
	let source_id = fs.new_source_id(path.into(), source.to_owned());

	eprintln!("parsing {path:?} ({bytes:?} bytes)", path = path.display(), bytes = source.len());

	let path_str: &str = path.to_str().unwrap_or_default();

	// Corrections for simplicity
	let type_annotations = if path_str.ends_with(".d.ts") {
		options::TypeAnnotationOption::Definitions
	} else if path_str.ends_with(".ts") {
		options::TypeAnnotationOption::Allowed
	} else {
		parse_options.type_annotations
	};

	let jsx = if let Some(jsx_options) = parse_options.jsx {
		Some(jsx_options)
	} else if path_str.ends_with('x') {
		Some(options::JSX::default())
	} else {
		None
	};

	let parse_options = options::ParseOptions { type_annotations, jsx, ..*parse_options };

	let result = parse_source(
		&source,
		source_id,
		timings,
		increase_stack_size,
		parse_options,
		print_ast,
		print_source_maps,
		to_string_options,
		fs,
	);
	match result {
		Ok((_module, state)) => {
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
						increase_stack_size,
						to_string_options,
						fs,
					)?;
				}
			}
			Ok(())
		}
		Err(err) => Err(err),
	}
}

fn parse_source(
	source: &str,
	source_id: SourceId,
	timings: bool,
	increase_stack_size: bool,
	parse_options: options::ParseOptions,
	print_ast: bool,
	print_source_maps: bool,
	to_string_options: &Option<options::ToStringOptions>,
	fs: &Files,
) -> Result<(Module, ParseState), Box<dyn std::error::Error>> {
	let now = Instant::now();
	let input = source.to_owned();

	// Run in thread as stack is large and can overflow
	let result = if increase_stack_size {
		const EIGHT_MEGA_BYTES: usize = 8 * 1024 * 1024;

		std::thread::Builder::new()
			.stack_size(EIGHT_MEGA_BYTES)
			.spawn(move || Module::from_string_with_options(input, parse_options, 0))
			.unwrap()
			.join()
			.unwrap()
	} else {
		Module::from_string_with_options(input, parse_options, 0)
	};

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

			// if parse_options.features.record_keyword_positions {
			// 	println!("{:?}", state.keyword_positions.as_ref());
			// }

			Ok((module, state))
		}
		Err(ParseError { reason, position }) => {
			let writer = StandardStream::stderr(ColorChoice::Always);
			let config = Config::default();

			let diagnostic = Diagnostic::error().with_labels(vec![
				Label::primary(source_id, position).with_message(format!("ParseError: {reason}")),
			]);
			term::emit(&mut writer.lock(), &config, &fs.into_code_span_store(), &diagnostic)?;
			Err(Box::<dyn std::error::Error>::from(ParseError { reason, position }))
		}
	}
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
			let output = String::from_utf8(std::mem::take(&mut buf)).unwrap();
			let mut parse_options = options::ParseOptions::default();

			let output: String = if let Some(rest) = output.strip_prefix("---") {
				let (options, rest) = rest.split_once("\n---").unwrap();
				for option in options.split(',') {
					match option.trim() {
						"partial" => {
							parse_options.features.partial_syntax = true;
						}
						"extras" => {
							parse_options.extras = options::Extras::all();
							parse_options.jsx = Some(options::JSX::all());
						}
						"jsx" => {
							parse_options.jsx = Some(options::JSX::default());
						}
						option => {
							eprintln!("unexpected {option:?}");
						}
					}
				}
				rest.trim_start().to_owned()
			} else {
				output
			};

			let module = Module::from_string_with_options(output.clone(), parse_options, 0);

			// TODO could remove things here
			match module {
				Ok((item, _)) => {
					let items = item.items.as_slice();
					if let [item] = items {
						if let ezno_parser::StatementOrDeclaration::Expression(item) = item {
							// Unwrap multiple expression
							let item = item.get_inner_ref();
							println!("{item:#?}");
						} else {
							println!("{item:#?}");
						}
					} else {
						for item in items {
							println!("{item:#?}");
						}
					}
				}
				Err(error) => {
					println!("error: {error:?}");
				}
			}

			println!("end");
			continue;
		}

		buf.extend_from_slice(line.as_bytes());
		buf.push(b'\n');
	}

	// println!("Finished!");
}

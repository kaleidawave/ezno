use std::{collections::VecDeque, path::Path, time::Instant};

use ezno_parser::{ASTNode, Comments, Module, ParseOptions, ToStringOptions};
use source_map::FileSystem;

type Files = source_map::MapFileStore<source_map::WithPathMap>;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let mut args: VecDeque<_> = std::env::args().skip(1).collect();
	let path = args.pop_front().ok_or("expected argument")?;

	let comments = if args.iter().any(|item| item == "--no-comments") {
		Comments::None
	} else if args.iter().any(|item| item == "--doc-comments") {
		Comments::JustDocumentation
	} else {
		Comments::All
	};

	let display_keywords = args.iter().any(|item| item == "--keywords");
	let extras = args.iter().any(|item| item == "--extras");
	let partial_syntax = args.iter().any(|item| item == "--partial");
	let print_source_maps = args.iter().any(|item| item == "--source-map");
	let timings = args.iter().any(|item| item == "--timings");
	let type_definition_module = args.iter().any(|item| item == "--type-definition-module");
	let type_annotations = !args.iter().any(|item| item == "--no-type-annotations");
	let top_level_html = args.iter().any(|item| item == "--top-level-html");
	let parse_imports = args.iter().any(|item| item == "--parse-imports");

	let print_ast = args.iter().any(|item| item == "--ast");

	let to_string_output = args.iter().any(|item| item == "--to-string");
	let pretty = args.iter().any(|item| item == "--pretty");

	// TODO temp
	const STACK_SIZE_MB: usize = 32;
	let parse_options = ParseOptions {
		stack_size: Some(STACK_SIZE_MB * 1024 * 1024),
		comments,
		record_keyword_positions: display_keywords,
		partial_syntax,
		type_annotations,
		type_definition_module,
		retain_blank_lines: pretty,
		custom_function_headers: extras,
		destructuring_type_annotation: extras,
		jsx: extras,
		is_expressions: extras,
		special_jsx_attributes: extras,
		extra_operators: extras,
		top_level_html,
		..ParseOptions::default()
	};

	let mut fs = Files::default();

	let to_string_options = to_string_output.then(|| ToStringOptions {
		expect_markers: true,
		include_type_annotations: type_annotations,
		pretty,
		comments: if pretty { Comments::All } else { Comments::None },
		// 60 is temp
		max_line_length: if pretty { 60 } else { u8::MAX },
		..Default::default()
	});

	parse_path(
		path.as_ref(),
		timings,
		parse_imports,
		&parse_options,
		print_ast,
		print_source_maps,
		&to_string_options,
		display_keywords,
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
	display_keywords: bool,
	fs: &mut Files,
) -> Result<(), Box<dyn std::error::Error>> {
	let source = std::fs::read_to_string(path)?;
	let source_id = fs.new_source_id(path.into(), source.clone());

	eprintln!("parsing {:?} ({:?} bytes)", path.display(), source.len());
	let now = Instant::now();
	let result = Module::from_string_with_options(source.clone(), *parse_options, None);

	match result {
		Ok((module, state)) => {
			if timings {
				eprintln!("parsed in: {:?}", now.elapsed());
			}

			if print_ast {
				println!("{module:#?}");
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

			if display_keywords {
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
						parse_options,
						print_ast,
						print_source_maps,
						to_string_options,
						display_keywords,
						fs,
					)?;
				}
			}
			Ok(())
		}
		Err(parse_err) => {
			let mut line_column = parse_err
				.position
				.with_source(source_id)
				.into_line_column_span::<source_map::encodings::Utf8>(fs);
			{
				// Editor are one indexed
				line_column.line_start += 1;
				line_column.line_end += 1;
				line_column.column_start += 1;
				line_column.column_end += 1;
			}
			eprintln!("error on {line_column:?}");

			Err(Box::<dyn std::error::Error>::from(parse_err))
		}
	}
}

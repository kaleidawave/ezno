use std::{collections::VecDeque, time::Instant};

use ezno_parser::{ASTNode, Comments, Module, ParseOptions, ToStringOptions};
use source_map::FileSystem;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let mut args: VecDeque<_> = std::env::args().skip(1).collect();
	let path = args.pop_front().ok_or("expected argument")?;

	let comments = if args.iter().any(|item| item == "--no-comments") {
		Comments::None
	} else {
		Comments::All
	};

	let display_keywords = args.iter().any(|item| item == "--keywords");
	let extras = args.iter().any(|item| item == "--extras");
	let partial_syntax = args.iter().any(|item| item == "--partial");
	let source_maps = args.iter().any(|item| item == "--source-map");
	let timings = args.iter().any(|item| item == "--timings");
	let render_timings = args.iter().any(|item| item == "--render-timings");
	let type_definition_module = args.iter().any(|item| item == "--type-definition-module");
	let type_annotations = !args.iter().any(|item| item == "--no-type-annotations");

	// `parse -> print -> parse -> print` and compare difference (same as fuzzing process)
	let double = args.iter().any(|item| item == "--double");

	let print_ast = args.iter().any(|item| item == "--ast");

	// double => pretty and render thus `|| double`
	let render_output = args.iter().any(|item| item == "--render") || double;
	let pretty = args.iter().any(|item| item == "--pretty") || double;

	let now = Instant::now();

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
		..ParseOptions::default()
	};

	// let parse_options = ParseOptions {
	// 	stack_size: Some(STACK_SIZE_MB * 1024 * 1024),
	// 	jsx: false,
	// 	type_annotations: false,
	// 	retain_blank_lines: true,
	// 	..Default::default()
	// };

	let mut fs = source_map::MapFileStore::<source_map::NoPathMap>::default();

	let source = std::fs::read_to_string(path.clone())?;

	// let source = String::from_utf8([0x2f, 0x8, 0x2f, 0xa].to_vec()).unwrap();
	// let source = "44;;".to_string();

	let source_id = fs.new_source_id(path.into(), source.clone());

	eprintln!("parsing {:?} bytes", source.len());
	let result = Module::from_string_with_options(source.clone(), parse_options, None);

	match result {
		Ok((module, state)) => {
			if timings {
				eprintln!("parsed in: {:?}", now.elapsed());
			}

			if print_ast {
				println!("{module:#?}");
			}
			if source_maps || render_output || double || render_timings {
				let now = Instant::now();

				let to_string_options = ToStringOptions {
					expect_markers: true,
					include_type_annotations: type_annotations,
					pretty,
					comments: if pretty { Comments::All } else { Comments::None },
					// 60 is temp
					max_line_length: if pretty { 60 } else { u8::MAX },
					..Default::default()
				};

				// let to_string_options = ToStringOptions::default();

				let (output, source_map) =
					module.to_string_with_source_map(&to_string_options, source_id, &fs);

				if timings || render_timings {
					eprintln!("ToString'ed in: {:?}", now.elapsed());
				}
				if source_maps {
					let sm = source_map.unwrap().to_json(&fs);
					println!("{output}\n{sm}");
				}
				if render_output {
					println!("{output}");
				}

				if double {
					let result2 =
						Module::from_string_with_options(output.clone(), parse_options, None);
					return match result2 {
						Ok((module2, _state)) => {
							let output2 = module2
								.to_string_with_source_map(&to_string_options, source_id, &fs)
								.0;

							if output == output2 {
								eprintln!("{output:?} == {output2:?}");
								eprintln!("re-parse was equal ✅");
								Ok(())
							} else {
								eprintln!("{output:?} != {output2:?} (original = {source:?})");
								eprintln!("initial   {:?}", module);
								eprintln!("re-parsed {:?}", module2);
								Err(Box::<dyn std::error::Error>::from("not equal"))
							}
						}
						Err(parse_err) => {
							eprintln!("error parsing output: {output:?} from {module:?}");
							Err(Box::<dyn std::error::Error>::from(parse_err))
						}
					};
				}
			}

			if display_keywords {
				println!("{:?}", state.keyword_positions.unwrap());
			}

			Ok(())
		}
		Err(parse_err) => {
			let mut line_column = parse_err
				.position
				.with_source(source_id)
				.into_line_column_span::<source_map::encodings::Utf8>(&fs);
			{
				// Editor are one indexed
				line_column.line_start += 1;
				line_column.line_end += 1;
				line_column.column_start += 1;
				line_column.column_end += 1;
			}
			eprintln!("error on {:?}", line_column);

			Err(Box::<dyn std::error::Error>::from(parse_err))
		}
	}
}

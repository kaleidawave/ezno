use std::{collections::VecDeque, fs::read_to_string, time::Instant};

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

	// TODO temp
	const STACK_SIZE_MB: usize = 32;

	let display_keywords = args.iter().any(|item| item == "--keywords");
	let partial_syntax = args.iter().any(|item| item == "--partial");
	let source_maps = args.iter().any(|item| item == "--source-map");
	let timings = args.iter().any(|item| item == "--timings");
	let render_timings = args.iter().any(|item| item == "--render-timings");
	let no_type_annotations = args.iter().any(|item| item == "--no-type-annotations");

	let now = Instant::now();

	let options = ParseOptions {
		stack_size: Some(STACK_SIZE_MB * 1024 * 1024),
		comments,
		record_keyword_positions: display_keywords,
		partial_syntax,
		type_annotations: !no_type_annotations,
		..ParseOptions::all_features()
	};

	let mut fs = source_map::MapFileStore::<source_map::NoPathMap>::default();
	let source = read_to_string(path.clone())?;
	let source_id = fs.new_source_id(path.into(), source.clone());

	eprintln!("parsing {:?} bytes", source.len());
	let result = Module::from_string_with_options(source, options, None);

	match result {
		Ok((module, state)) => {
			if timings {
				eprintln!("parsed in: {:?}", now.elapsed());
			}

			let print_ast = args.iter().any(|item| item == "--ast");
			let render_output = args.iter().any(|item| item == "--render");
			let pretty = args.iter().any(|item| item == "--pretty");

			// `parse -> print -> parse -> print` and compare difference (same as fuzzing process)
			let double = args.iter().any(|item| item == "--double");

			if print_ast {
				println!("{module:#?}");
			}
			if source_maps || render_output || double || render_timings {
				let now = Instant::now();

				let to_string_options = ToStringOptions {
					trailing_semicolon: true,
					expect_markers: true,
					include_types: true,
					pretty,
					comments: if pretty { Comments::All } else { Comments::None },
					// 60 is temp
					max_line_length: if pretty { 60 } else { u8::MAX },
					..Default::default()
				};

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
					let result2 = Module::from_string_with_options(output.clone(), options, None);
					return match result2 {
						Ok((module2, _state)) => {
							let output2 = module2
								.to_string_with_source_map(&to_string_options, source_id, &fs)
								.0;

							eprintln!("{output:?}\n{output2:?}");
							if output != output2 {
								eprintln!("initial   {:?}", module);
								eprintln!("re-parsed {:?}", module2);
								return Err(Box::<dyn std::error::Error>::from("not equal"));
							} else {
								eprintln!("re-parse was equal ✅");
								Ok(())
							}
						}
						Err(parse_err) => {
							eprintln!("error parsing output: {output:?} from {module:?}");
							return Err(Box::<dyn std::error::Error>::from(parse_err));
						}
					};
				}
			} else if !timings {
				eprintln!("parsed in: {:?}", now.elapsed());
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

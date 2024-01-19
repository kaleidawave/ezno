use std::{fs::read_to_string, time::Instant};

use ezno_parser::{ASTNode, Comments, Module, ParseOptions, ToStringOptions};
use source_map::FileSystem;

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let mut args: Vec<_> = std::env::args().skip(1).collect();
	let path = args.drain(0..1).next().ok_or("expected argument")?;
	let now = Instant::now();

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

	let options = ParseOptions {
		stack_size: Some(STACK_SIZE_MB * 1024 * 1024),
		comments,
		record_keyword_positions: display_keywords,
		partial_syntax,
		..ParseOptions::all_features()
	};

	let mut fs = source_map::MapFileStore::<source_map::NoPathMap>::default();
	let source = read_to_string(path.clone())?;
	let source_id = fs.new_source_id(path.into(), source.clone());

	let result = Module::from_string_with_options(source, options, None);

	match result {
		Ok((module, state)) => {
			eprintln!("Parsed in: {:?}", now.elapsed());

			let print_ast = args.iter().any(|item| item == "--ast");
			let render_output = args.iter().any(|item| item == "--render");
			let pretty = args.iter().any(|item| item == "--pretty");

			if print_ast {
				println!("{module:#?}");
			}
			if source_maps || render_output {
				let (output, source_map) = module.to_string_with_source_map(
					&ToStringOptions {
						trailing_semicolon: true,
						expect_markers: true,
						include_types: true,
						pretty,
						// 60 is temp
						max_line_length: if pretty { 60 } else { u8::MAX },
						..Default::default()
					},
					source_id,
					&fs,
				);
				if source_maps {
					let sm = source_map.unwrap().to_json(&fs);
					println!("{output}\n{sm}");
				}
				if render_output {
					println!("{output}");
				}
			}

			if display_keywords {
				println!("{:?}", state.keyword_positions.unwrap());
			}
			Ok(())
		}
		Err(parse_err) => {
			eprintln!(
				"error on {:?}",
				parse_err
					.position
					.with_source(source_id)
					.into_line_column_span::<source_map::encodings::Utf8>(&fs)
			);
			Err(Box::<dyn std::error::Error>::from(parse_err))
		}
	}
}

use std::time::Instant;

use ezno_parser::{ASTNode, Comments, FromFileError, Module, ParseOptions, ToStringOptions};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let mut args: Vec<_> = std::env::args().skip(1).collect();
	let path = args.drain(0..1).next().ok_or("expected argument")?;
	let now = Instant::now();
	let mut fs = source_map::MapFileStore::<source_map::NoPathMap>::default();
	// TODO temp
	const STACK_SIZE_MB: usize = 32;
	let comments = if args.iter().any(|item| item == "--no-comments") {
		Comments::None
	} else {
		Comments::All
	};

	let options = ParseOptions {
		stack_size: Some(STACK_SIZE_MB * 1024 * 1024),
		comments,
		..ParseOptions::all_features()
	};

	let result = Module::from_file(&path, options, &mut fs);
	match result {
		Ok(module) => {
			eprintln!("Parsed in: {:?}", now.elapsed());
			if args.iter().any(|item| item == "--ast") {
				println!("{module:#?}");
			} else if args.iter().any(|item| item == "--render") {
				let output = module
					.to_string(&ToStringOptions { trailing_semicolon: true, ..Default::default() });
				println!("{output}");
			}
			Ok(())
		}
		Err(FromFileError::FileError(_file_err)) => {
			eprintln!("could not find file {path}");
			Err(Box::<dyn std::error::Error>::from("error"))
		}
		Err(FromFileError::ParseError(parse_err, source)) => {
			eprintln!("parse error {}", parse_err.reason);
			eprintln!(
				"error on {:?}",
				parse_err
					.position
					.with_source(source)
					.into_line_column_span::<source_map::encodings::Utf8>(&fs)
			);
			Err(Box::<dyn std::error::Error>::from("error"))
		}
	}
}

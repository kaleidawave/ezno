use std::time::Instant;

use ezno_parser::{ASTNode, FromFileError, Module, ParseOptions, ToStringOptions};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let path = std::env::args().skip(1).next().ok_or("expected argument")?;
	let now = Instant::now();
	let mut fs = source_map::MapFileStore::<source_map::NoPathMap>::default();
	let result = Module::from_file(&path, ParseOptions::default(), &mut fs);
	match result {
		Ok(module) => {
			eprintln!("Parsed in: {:?}", now.elapsed());
			if std::env::args().any(|item| item == "--ast") {
				println!("{module:#?}");
			} else {
				let output = module.to_string(&ToStringOptions::default());
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

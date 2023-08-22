use std::time::Instant;

use ezno_parser::{ASTNode, FromFileError, Module, ParseOptions, ToStringOptions};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let path = std::env::args().skip(1).next().ok_or("expected argument")?;
	let now = Instant::now();
	let mut fs = source_map::MapFileStore::default();
	match Module::from_file(&path, ParseOptions::default(), Vec::default(), &mut fs) {
		Ok(module) => {
			println!("{:?}", now.elapsed());
			if std::env::args().any(|item| item == "--ast") {
				println!("{module:#?}");
			} else {
				let output = module.to_string(&ToStringOptions::default());
				println!("{output}");
			}
			Ok(())
		}
		Err(FromFileError::FileError(_file_err)) => {
			println!("could not find file {path}");
			Err(Box::<dyn std::error::Error>::from("error"))
		}
		Err(FromFileError::ParseError(parse_err)) => {
			println!("parse error {}", parse_err.reason);
			println!(
				"error on {:?}",
				parse_err.position.into_line_column_span::<source_map::encodings::Utf8>(&fs)
			);
			Err(Box::<dyn std::error::Error>::from("error"))
		}
	}
}

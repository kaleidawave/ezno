use ezno_parser::{ASTNode, FromFileError, Module, ParseSettings, ToStringSettings};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let path = std::env::args().skip(1).next().ok_or("expected argument")?;
	let mut fs = source_map::MapFileStore::default();
	match Module::from_file(&path, ParseSettings::default(), Vec::default(), &mut fs) {
		Ok(module) => {
			println!("{module:#?}");

			let output = module.to_string(&ToStringSettings::default());
			println!("In string form:");
			println!("{output}");
			Ok(())
		}
		Err(FromFileError::FileError(_file_err)) => {
			println!("could not find file {path}");
			Err(Box::<dyn std::error::Error>::from("error"))
		}
		Err(FromFileError::ParseError(parse_err)) => {
			println!("parse error {}", parse_err.reason);
			println!("error on {:?}", parse_err.position.into_line_column_span(&fs));
			Err(Box::<dyn std::error::Error>::from("error"))
		}
	}
}

use ezno_parser::{FromFileError, Module, ParseSettings, ToStringSettings};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let path = std::env::args().skip(1).next().ok_or("expected argument")?;
	match Module::from_file(&path, ParseSettings::default(), Vec::default()) {
		Ok(module) => {
			let output = module.to_string(ToStringSettings::default());
			println!("{output}");
			Ok(())
		}
		Err(FromFileError::FileError(_file_err)) => {
			println!("could not find file {path}");
			Err(Box::<dyn std::error::Error>::from("error"))
		}
		Err(FromFileError::ParseError(parse_err)) => {
			println!("parse error {}", parse_err.get_reason());
			// TODO should be done in source-map
			let content = parse_err.position.source_id.get_file().unwrap().1;
			println!("error on {:?}", parse_err.position.into_line_column_span(&content));
			Err(Box::<dyn std::error::Error>::from("error"))
		}
	}
}

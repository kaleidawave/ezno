use ezno_parser::{Module, ParseSettings, ToStringSettings};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let path = std::env::args().skip(1).next().ok_or("expected argument")?;
	let module = Module::from_file(path, ParseSettings::default(), Vec::default()).unwrap();
	let output = module.to_string(ToStringSettings::default());
	println!("{output}");
	Ok(())
}

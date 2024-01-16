use ezno_parser::{ASTNode, Module, ParseOptions};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	const STACK_SIZE_MB: usize = 64;
	let options =
		ParseOptions { stack_size: Some(STACK_SIZE_MB * 1024 * 1024), ..ParseOptions::default() };

	for i in 0..200 {
		let mut s = "a ? b : c".to_owned();
		for _ in 0..(i * 5) {
			s = format!("a ? b : ({s})");
		}
		eprintln!("parsing at {}", i * 5);
		let _result = Module::from_string(s, options)?;
	}
	Ok(())
}

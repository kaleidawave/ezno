use std::error::Error;
use std::fs::File;
use std::path::Path;

fn main() -> Result<(), Box<dyn Error>> {
	let out_path = Path::new(&std::env::var("OUT_DIR")?).join("common.rs");
	let mut out = File::create(out_path)?;
	reqwest::blocking::get(
		"https://raw.githubusercontent.com/boa-dev/boa/main/fuzz/fuzz_targets/common.rs",
	)?
	.copy_to(&mut out)?;

	Ok(())
}

fn main() -> std::process::ExitCode {
	let _output = std::process::Command::new("cargo")
		.arg("build")
		.arg("--example")
		.arg("runner")
		.status()
		.unwrap();

	let output = std::process::Command::new("spectra")
		.arg("test")
		.arg("./specification/specification.md")
		.arg("./target/debug/examples/runner --rpc --interactive")
		.status()
		.unwrap();

	if output.code().is_none_or(|item| item == 0) {
		std::process::ExitCode::SUCCESS
	} else {
		std::process::ExitCode::FAILURE
	}
}

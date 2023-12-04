#[allow(unused)]
use std::{
	collections::HashSet,
	env, fs,
	path::{Path, PathBuf},
	process::Command,
	time::Instant,
};

use crate::{
	commands::{BuildOutput, FailedBuildOutput},
	error_handling::emit_ezno_diagnostic,
	utilities::print_to_cli,
};
use argh::FromArgs;

/// Ezno Compiler
#[derive(FromArgs, Debug)]
struct TopLevel {
	#[argh(subcommand)]
	nested: CompilerSubCommand,
}

#[derive(FromArgs, Debug)]
#[argh(subcommand)]
enum CompilerSubCommand {
	Info(Info),
	Build(BuildArguments),
	ASTExplorer(crate::ast_explorer::ExplorerArguments),
	Check(CheckArguments),
	// Run(RunArguments),
	#[cfg(not(target_family = "wasm"))]
	Repl(crate::repl::ReplArguments),
	// #[cfg(debug_assertions)]
	// Pack(Pack),
}

/// Display Ezno information
#[derive(FromArgs, Debug)]
#[argh(subcommand, name = "info")]
struct Info {}

// /// Generates binary form of a type definition module
// #[derive(FromArgs, Debug)]
// #[argh(subcommand, name = "pack")]
// struct Pack {
// 	/// path to module
// 	#[argh(positional)]
// 	input: PathBuf,
// 	/// output path
// 	#[argh(positional)]
// 	output: PathBuf,
// }

// TODO definition file as list
/// Build project
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "build")]
// TODO: Can be refactored with bit to reduce memory
#[allow(clippy::struct_excessive_bools)]
pub(crate) struct BuildArguments {
	/// path to input file
	#[argh(positional)]
	pub input: PathBuf,
	/// path to output
	#[argh(positional)]
	pub output: Option<PathBuf>,
	/// paths to definition files
	#[argh(option, short = 'd')]
	pub definition_file: Option<PathBuf>,

	/// whether to minify build output
	#[argh(switch, short = 'm')]
	pub minify: bool,
	/// whether to include comments in the output
	#[argh(switch)]
	pub no_comments: bool,
	/// build source maps
	#[argh(switch)]
	pub source_maps: bool,

	/// enable non standard syntax
	#[argh(switch)]
	pub non_standard_syntax: bool,
	/// enable non standard library
	#[argh(switch)]
	pub non_standard_library: bool,
	/// enable optimising transforms (warning can break code)
	#[argh(switch)]
	pub optimise: bool,

	#[cfg(not(target_family = "wasm"))]
	/// whether to display compile times
	#[argh(switch)]
	pub timings: bool,
	// /// whether to re-build on file changes
	// #[argh(switch)]
	// watch: bool,
}

/// Type check project
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "check")]
pub(crate) struct CheckArguments {
	/// path to input file
	#[argh(positional)]
	pub input: PathBuf,
	/// paths to definition files
	#[argh(option, short = 'd')]
	pub definition_file: Option<PathBuf>,
	/// whether to re-check on file changes
	#[argh(switch)]
	pub watch: bool,
}

// /// Run project using Deno
// #[derive(FromArgs, PartialEq, Debug)]
// #[argh(subcommand, name = "run")]
// struct RunArguments {
// 	/// path to input file
// 	#[argh(positional)]
// 	input: PathBuf,

// 	/// path to output
// 	#[argh(positional)]
// 	output: PathBuf,

// 	/// whether to re-run on file changes
// 	#[argh(switch)]
// 	watch: bool,
// }

#[allow(unused)]
fn file_system_resolver(path: &Path) -> Option<String> {
	// Cheaty
	if path.to_str() == Some("BLANK") {
		return Some(String::new());
	}
	match fs::read_to_string(path) {
		Ok(source) => Some(source),
		Err(_) => None,
	}
}

pub fn run_cli<T: crate::ReadFromFS, U: crate::WriteToFS, V: crate::CLIInputResolver>(
	cli_arguments: &[&str],
	read_file: &T,
	write_file: U,
	cli_input_resolver: V,
) {
	let command = match FromArgs::from_args(&["ezno-cli"], cli_arguments) {
		Ok(TopLevel { nested }) => nested,
		Err(err) => {
			print_to_cli(format_args!("{}", err.output));
			return;
		}
	};

	match command {
		CompilerSubCommand::Info(_) => {
			crate::utilities::print_info();
		}
		CompilerSubCommand::Build(build_config) => {
			let output_path = build_config.output.unwrap_or("ezno_output.js".into());
			let output = crate::commands::build(
				read_file,
				&build_config.input,
				build_config.definition_file.as_deref(),
				&output_path,
				&crate::commands::BuildConfig { strip_whitespace: build_config.minify },
				None,
			);
			match output {
				Ok(BuildOutput { diagnostics, fs, outputs }) => {
					for output in outputs {
						write_file(output.output_path.as_path(), output.content);
					}
					for diagnostic in diagnostics {
						emit_ezno_diagnostic(diagnostic, &fs).unwrap();
					}
				}
				Err(FailedBuildOutput { fs, diagnostics }) => {
					for diagnostic in diagnostics {
						emit_ezno_diagnostic(diagnostic, &fs).unwrap();
					}
				}
			}
		}
		CompilerSubCommand::ASTExplorer(mut repl) => repl.run(read_file, cli_input_resolver),
		CompilerSubCommand::Check(check_arguments) => {
			let CheckArguments { input, watch: _, definition_file } = check_arguments;
			let (diagnostics, others) =
				crate::commands::check(read_file, &input, definition_file.as_deref());

			let fs = match others {
				Ok(data) => data.module_contents,
				Err(data) => data,
			};
			for diagnostic in diagnostics {
				emit_ezno_diagnostic(diagnostic, &fs).unwrap();
			}
		}
		#[cfg(not(target_family = "wasm"))]
		CompilerSubCommand::Repl(argument) => crate::repl::run_deno_repl(cli_input_resolver, argument),
		// CompilerSubCommand::Run(run_arguments) => {
		// 	let build_arguments = BuildArguments {
		// 		input: run_arguments.input,
		// 		output: Some(run_arguments.output.clone()),
		// 		minify: true,
		// 		no_comments: true,
		// 		source_maps: false,
		// 		watch: false,
		// 		timings: false,
		// 	};
		// 	let output = build(build_arguments);

		// 	if output.is_ok() {
		// 		Command::new("deno")
		// 			.args(["run", "--allow-all", run_arguments.output.to_str().unwrap()])
		// 			.spawn()
		// 			.unwrap()
		// 			.wait()
		// 			.unwrap();
		// 	}
		// }
		// #[cfg(debug_assertions)]
		// CompilerSubCommand::Pack(Pack { input, output }) => {
		// 	let file = checker::definition_file_to_buffer(
		// 		&file_system_resolver,
		// 		&env::current_dir().unwrap(),
		// 		&input,
		// 	)
		// 	.unwrap();

		// 	std::fs::write(&output, &file).unwrap();
		// 	// println!("Wrote binary context out to {}", output.display());

		// 	let _root_ctx = checker::root_context_from_bytes(file);
		// 	println!("Registered {} types", _root_ctx.types.len());
		// }
	}
}

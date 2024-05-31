#[allow(unused)]
use std::{
	collections::HashSet,
	env, fs,
	path::{Path, PathBuf},
	process::Command,
	process::ExitCode,
	time::Instant,
};

use crate::{
	build::{build, BuildConfig, BuildOutput, EznoParsePostCheckVisitors, FailedBuildOutput},
	check::check,
	reporting::emit_diagnostics,
	utilities::{self, print_to_cli},
};
use argh::FromArgs;
use checker::CheckOutput;
use parser::ParseOptions;

/// The Ezno type-checker & compiler
#[derive(FromArgs, Debug)]
struct TopLevel {
	#[argh(subcommand)]
	nested: CompilerSubCommand,
}

#[derive(FromArgs, Debug)]
#[argh(subcommand)]
enum CompilerSubCommand {
	Info(Info),
	ASTExplorer(crate::ast_explorer::ExplorerArguments),
	Check(CheckArguments),
	Experimental(ExperimentalArguments),
	Repl(crate::repl::ReplArguments),
	// Run(RunArguments),
	// #[cfg(debug_assertions)]
	// Pack(Pack),
}

/// Display Ezno information
#[derive(FromArgs, Debug)]
#[argh(subcommand, name = "info")]
struct Info {}

/// Experimental Ezno features
#[derive(FromArgs, Debug)]
#[argh(subcommand, name = "experimental")]
pub(crate) struct ExperimentalArguments {
	#[argh(subcommand)]
	nested: ExperimentalSubcommand,
}

#[derive(FromArgs, Debug)]
#[argh(subcommand)]
pub(crate) enum ExperimentalSubcommand {
	Build(BuildArguments),
	Format(FormatArguments),
	#[cfg(not(target_family = "wasm"))]
	Upgrade(UpgradeArguments),
}

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
	/// compact diagnostics
	#[argh(switch)]
	pub compact_diagnostics: bool,
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
	/// whether to display check time
	#[argh(switch)]
	pub timings: bool,
	/// whether to print all diagnostics
	#[argh(switch)]
	pub count_diagnostics: bool,
	/// compact diagnostics
	#[argh(switch)]
	pub compact_diagnostics: bool,
}

/// Formats file in-place
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "format")]
pub(crate) struct FormatArguments {
	/// path to input file
	#[argh(positional)]
	pub path: PathBuf,
}

/// Upgrade/update the ezno binary to the latest version
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "upgrade")]
#[cfg(not(target_family = "wasm"))]
pub(crate) struct UpgradeArguments {}

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
) -> ExitCode {
	let command = match FromArgs::from_args(&["ezno-cli"], cli_arguments) {
		Ok(TopLevel { nested }) => nested,
		Err(err) => {
			print_to_cli(format_args!("{}", err.output));
			return ExitCode::FAILURE;
		}
	};

	match command {
		CompilerSubCommand::Info(_) => {
			crate::utilities::print_info();
			ExitCode::SUCCESS
		}
		CompilerSubCommand::Check(check_arguments) => {
			let CheckArguments {
				input,
				watch: _,
				definition_file,
				timings,
				count_diagnostics,
				compact_diagnostics,
			} = check_arguments;

			let entry_points = vec![input];

			#[cfg(not(target_family = "wasm"))]
			let start = timings.then(std::time::Instant::now);

			let type_check_options = Default::default();

			let CheckOutput { diagnostics, module_contents, .. } =
				check(entry_points, read_file, definition_file.as_deref(), type_check_options);

			#[cfg(not(target_family = "wasm"))]
			if let Some(start) = start {
				eprintln!("Checked in {:?}", start.elapsed());
			};

			if diagnostics.has_error() {
				if count_diagnostics {
					let count = diagnostics.into_iter().count();
					print_to_cli(format_args!("Found {count} type errors and warnings 😬"))
				} else {
					emit_diagnostics(diagnostics, &module_contents, compact_diagnostics).unwrap();
				}
				ExitCode::FAILURE
			} else {
				// May be warnings or information here
				emit_diagnostics(diagnostics, &module_contents, compact_diagnostics).unwrap();
				print_to_cli(format_args!("No type errors found 🎉"));
				ExitCode::SUCCESS
			}
		}
		CompilerSubCommand::Experimental(ExperimentalArguments {
			nested: ExperimentalSubcommand::Build(build_config),
		}) => {
			let output_path = build_config.output.unwrap_or("ezno_output.js".into());

			// TODO
			let default_builders = EznoParsePostCheckVisitors {
				expression_visitors_mut: vec![Box::new(
					crate::transformers::optimisations::ExpressionOptimiser,
				)],
				statement_visitors_mut: vec![Box::new(
					crate::transformers::optimisations::StatementOptimiser,
				)],
				variable_visitors_mut: Default::default(),
				block_visitors_mut: Default::default(),
			};

			let input_paths = vec![build_config.input];

			let output = build(
				input_paths,
				read_file,
				build_config.definition_file.as_deref(),
				&output_path,
				&BuildConfig { strip_whitespace: build_config.minify },
				Some(default_builders),
			);

			let compact_diagnostics = build_config.compact_diagnostics;

			match output {
				Ok(BuildOutput { diagnostics, fs, outputs }) => {
					for output in outputs {
						write_file(output.output_path.as_path(), output.content);
					}
					emit_diagnostics(diagnostics, &fs, compact_diagnostics).unwrap();
					print_to_cli(format_args!("Project built successfully 🎉"));
					ExitCode::SUCCESS
				}
				Err(FailedBuildOutput { fs, diagnostics }) => {
					emit_diagnostics(diagnostics, &fs, compact_diagnostics).unwrap();
					ExitCode::FAILURE
				}
			}
		}
		CompilerSubCommand::Experimental(ExperimentalArguments {
			nested: ExperimentalSubcommand::Format(FormatArguments { path }),
		}) => {
			use parser::{source_map::FileSystem, ASTNode, Module, ToStringOptions};

			let input = match fs::read_to_string(&path) {
				Ok(string) => string,
				Err(err) => {
					print_to_cli(format_args!("{err:?}"));
					return ExitCode::FAILURE;
				}
			};
			let mut files =
				parser::source_map::MapFileStore::<parser::source_map::NoPathMap>::default();
			let source_id = files.new_source_id(path.clone(), input.clone());
			let res = Module::from_string(
				input,
				ParseOptions { retain_blank_lines: true, ..Default::default() },
			);
			match res {
				Ok(module) => {
					let options =
						ToStringOptions { trailing_semicolon: true, ..Default::default() };
					let _ = fs::write(path.clone(), module.to_string(&options));
					print_to_cli(format_args!("Formatted {} 🎉", path.display()));
					ExitCode::SUCCESS
				}
				Err(err) => {
					emit_diagnostics(std::iter::once((err, source_id).into()), &files, false)
						.unwrap();
					ExitCode::FAILURE
				}
			}
		}
		#[cfg(not(target_family = "wasm"))]
		CompilerSubCommand::Experimental(ExperimentalArguments {
			nested: ExperimentalSubcommand::Upgrade(UpgradeArguments {}),
		}) => match utilities::upgrade_self() {
			Ok(name) => {
				print_to_cli(format_args!("Successfully updated to {name}"));
				std::process::ExitCode::SUCCESS
			}
			Err(err) => {
				print_to_cli(format_args!("Error: {err}\nCould not upgrade binary. Retry manually from {repository}/releases", repository=env!("CARGO_PKG_REPOSITORY")));
				std::process::ExitCode::FAILURE
			}
		},
		CompilerSubCommand::ASTExplorer(mut repl) => {
			repl.run(read_file, cli_input_resolver);
			// TODO not always true
			ExitCode::SUCCESS
		}
		CompilerSubCommand::Repl(argument) => {
			crate::repl::run_repl(cli_input_resolver, argument);
			// TODO not always true
			ExitCode::SUCCESS
		} // CompilerSubCommand::Run(run_arguments) => {
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

		  // 	let _root_ctx = checker::root_context_from_bytes(file);
		  // 	println!("Registered {} types", _root_ctx.types.len();
		  // }
	}
}

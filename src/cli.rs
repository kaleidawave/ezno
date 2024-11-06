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
	reporting::report_diagnostics_to_cli,
	utilities::{print_to_cli, MaxDiagnostics},
};
use argh::FromArgs;
use checker::{CheckOutput, TypeCheckOptions};
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
#[derive(FromArgs, Debug)]
#[argh(subcommand, name = "build")]
// TODO: Can be refactored with bit to reduce memory
#[allow(clippy::struct_excessive_bools)]
pub(crate) struct BuildArguments {
	/// path to input file (accepts glob)
	#[argh(positional)]
	pub input: String,
	/// path to output
	#[argh(positional)]
	pub output: Option<PathBuf>,
	/// paths to definition files
	#[argh(option, short = 'd')]
	pub definition_file: Option<PathBuf>,

	/// whether to minify build output
	#[argh(switch, short = 'm')]
	pub minify: bool,
	/// build source maps
	#[argh(switch)]
	pub source_maps: bool,
	/// compact diagnostics
	#[argh(switch)]
	pub compact_diagnostics: bool,
	/// enable optimising transforms (warning can currently break code)
	#[argh(switch)]
	pub optimise: bool,
	/// maximum diagnostics to print (defaults to 30, pass `all` for all and `0` to count)
	#[argh(option, default = "MaxDiagnostics::default()")]
	pub max_diagnostics: MaxDiagnostics,

	#[cfg(not(target_family = "wasm"))]
	/// whether to display compile times
	#[argh(switch)]
	pub timings: bool,
	// /// whether to re-build on file changes
	// #[argh(switch)]
	// watch: bool,
}

/// Type check project
#[derive(FromArgs, Debug)]
#[argh(subcommand, name = "check")]
pub(crate) struct CheckArguments {
	/// path to input file (accepts glob)
	#[argh(positional)]
	pub input: String,
	/// paths to definition files
	#[argh(option, short = 'd')]
	pub definition_file: Option<PathBuf>,
	/// whether to re-check on file changes TODO #164
	#[argh(switch)]
	pub _watch: bool,
	/// whether to display check time
	#[argh(switch)]
	pub timings: bool,
	/// compact diagnostics
	#[argh(switch)]
	pub compact_diagnostics: bool,
	/// maximum diagnostics to print (defaults to 30, pass `all` for all and `0` to count)
	#[argh(option, default = "MaxDiagnostics::default()")]
	pub max_diagnostics: MaxDiagnostics,
}

/// Formats file in-place
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "format")]
pub(crate) struct FormatArguments {
	/// path to input file
	#[argh(positional)]
	pub path: PathBuf,
	/// check whether file is formatted
	#[argh(switch)]
	pub check: bool,
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

pub fn run_cli<T: crate::ReadFromFS, U: crate::WriteToFS>(
	cli_arguments: &[&str],
	read_file: &T,
	write_file: U,
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
				// TODO #164
				_watch,
				definition_file,
				timings,
				compact_diagnostics,
				max_diagnostics,
			} = check_arguments;

			let type_check_options: TypeCheckOptions = if cfg!(target_family = "wasm") {
				Default::default()
			} else {
				let mut options = TypeCheckOptions::default();
				options.measure_time = timings;
				options
			};

			let entry_points = match get_entry_points(input) {
				Ok(entry_points) => entry_points,
				Err(_) => return ExitCode::FAILURE,
			};

			let result =
				check(entry_points, read_file, definition_file.as_deref(), type_check_options);
			let CheckOutput { diagnostics, module_contents, chronometer, types, .. } = result;

			let diagnostics_count = diagnostics.count();
			let current = timings.then(std::time::Instant::now);

			let result = if diagnostics.has_error() {
				if let MaxDiagnostics::FixedTo(0) = max_diagnostics {
					let count = diagnostics.into_iter().count();
					print_to_cli(format_args!(
						"Found {count} type errors and warnings {}",
						console::Emoji(" 😬", ":/")
					))
				} else {
					report_diagnostics_to_cli(
						diagnostics,
						&module_contents,
						compact_diagnostics,
						max_diagnostics,
					)
					.unwrap();
				}
				ExitCode::FAILURE
			} else {
				// May be warnings or information here
				report_diagnostics_to_cli(
					diagnostics,
					&module_contents,
					compact_diagnostics,
					max_diagnostics,
				)
				.unwrap();
				print_to_cli(format_args!("No type errors found {}", console::Emoji("🎉", ":)")));
				ExitCode::SUCCESS
			};

			#[cfg(not(target_family = "wasm"))]
			if timings {
				let reporting = current.unwrap().elapsed();

				eprintln!("---\n");
				eprintln!("Diagnostics:\t{}", diagnostics_count);
				eprintln!("Types:      \t{}", types.count_of_types());
				eprintln!("Lines:      \t{}", chronometer.lines);
				eprintln!("Cache read: \t{:?}", chronometer.cached);
				eprintln!("FS read:    \t{:?}", chronometer.fs);
				eprintln!("Parsed in:  \t{:?}", chronometer.parse);
				eprintln!("Checked in: \t{:?}", chronometer.check);
				eprintln!("Reporting:  \t{:?}", reporting);
			}

			result
		}
		CompilerSubCommand::Experimental(ExperimentalArguments {
			nested: ExperimentalSubcommand::Build(build_config),
		}) => {
			let output_path = build_config.output.unwrap_or("ezno_output.js".into());

			let mut default_builders = EznoParsePostCheckVisitors::default();

			if build_config.optimise {
				default_builders
					.expression_visitors_mut
					.push(Box::new(crate::transformers::optimisations::ExpressionOptimiser));

				default_builders
					.statement_visitors_mut
					.push(Box::new(crate::transformers::optimisations::StatementOptimiser));
			}

			let entry_points = match get_entry_points(build_config.input) {
				Ok(entry_points) => entry_points,
				Err(_) => return ExitCode::FAILURE,
			};

			#[cfg(not(target_family = "wasm"))]
			let start = build_config.timings.then(std::time::Instant::now);

			let output = build(
				entry_points,
				read_file,
				build_config.definition_file.as_deref(),
				&output_path,
				&BuildConfig {
					strip_whitespace: build_config.minify,
					source_maps: build_config.source_maps,
				},
				Some(default_builders),
			);

			#[cfg(not(target_family = "wasm"))]
			if let Some(start) = start {
				eprintln!("Checked & built in {:?}", start.elapsed());
			};

			let compact_diagnostics = build_config.compact_diagnostics;

			match output {
				Ok(BuildOutput { diagnostics, fs, outputs }) => {
					for output in outputs {
						write_file(output.output_path.as_path(), output.content);
					}
					report_diagnostics_to_cli(
						diagnostics,
						&fs,
						compact_diagnostics,
						build_config.max_diagnostics,
					)
					.unwrap();
					print_to_cli(format_args!(
						"Project built successfully {}",
						console::Emoji("🎉", ":)")
					));
					ExitCode::SUCCESS
				}
				Err(FailedBuildOutput { fs, diagnostics }) => {
					report_diagnostics_to_cli(
						diagnostics,
						&fs,
						compact_diagnostics,
						build_config.max_diagnostics,
					)
					.unwrap();
					ExitCode::FAILURE
				}
			}
		}
		CompilerSubCommand::Experimental(ExperimentalArguments {
			nested: ExperimentalSubcommand::Format(FormatArguments { path, check }),
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
				input.clone(),
				ParseOptions { retain_blank_lines: true, ..Default::default() },
			);
			match res {
				Ok(module) => {
					let options = ToStringOptions {
						trailing_semicolon: true,
						include_type_annotations: true,
						..Default::default()
					};
					let output = module.to_string(&options);
					if check {
						if input == output {
							ExitCode::SUCCESS
						} else {
							print_to_cli(format_args!(
								"{}",
								pretty_assertions::StrComparison::new(&input, &output)
							));
							ExitCode::FAILURE
						}
					} else {
						let _ = fs::write(path.clone(), output);
						print_to_cli(format_args!("Formatted {}", path.display()));
						ExitCode::SUCCESS
					}
				}
				Err(err) => {
					report_diagnostics_to_cli(
						std::iter::once((err, source_id).into()),
						&files,
						false,
						MaxDiagnostics::All,
					)
					.unwrap();
					ExitCode::FAILURE
				}
			}
		}
		#[cfg(not(target_family = "wasm"))]
		CompilerSubCommand::Experimental(ExperimentalArguments {
			nested: ExperimentalSubcommand::Upgrade(UpgradeArguments {}),
		}) => match crate::utilities::upgrade_self() {
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
			repl.run(read_file);
			// TODO not always true
			ExitCode::SUCCESS
		}
		CompilerSubCommand::Repl(argument) => {
			crate::repl::run_repl(argument);
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

// `glob` library does not work on WASM :(
#[cfg(target_family = "wasm")]
fn get_entry_points(input: String) -> Result<Vec<PathBuf>, ()> {
	Ok(vec![input.into()])
}

#[cfg(not(target_family = "wasm"))]
fn get_entry_points(input: String) -> Result<Vec<PathBuf>, ()> {
	match glob::glob(&input) {
		Ok(files) => {
			let files = files
				.into_iter()
				.collect::<Result<Vec<PathBuf>, glob::GlobError>>()
				.map_err(|err| {
					eprintln!("{err:?}");
				})?;

			if files.is_empty() {
				eprintln!("Input {input:?} matched no files");
				Err(())
			} else {
				Ok(files)
			}
		}
		Err(err) => {
			eprintln!("{err:?}");
			Err(())
		}
	}
}

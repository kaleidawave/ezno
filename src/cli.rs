use std::env;
use std::path::PathBuf;
use std::process::ExitCode;
use std::str::FromStr;
use std::time::Duration;

use crate::{
	build::{build, BuildConfig, BuildOutput, FailedBuildOutput},
	check::{check_and_report, CheckOutput, TypeCheckOptions},
	reporting::report_diagnostics_to_cli,
	utilities::{print_to_cli, MaxDiagnostics},
};

use lahl::{
	argument_result_or_out, command_result_or_out, Endpoint, NamedParameter, PositionalParameter,
	CLI,
};

/// The Ezno type-checker & compiler
static ENDPOINTS: &[Endpoint] = &[
	Endpoint::new("check", "type checks code", CHECK_BUILD_POSITIONAL_PARAMETERS, CHECK_PARAMETERS),
	Endpoint::new("repl", "run repl", &[], &[]),
	Endpoint::new("info", "show information about binary", &[], &[]),
	Endpoint::new_group("experimental", "parse", "parse code", &[], &[]),
	Endpoint::new_group("experimental", "format", "format code", &[], &[]),
	Endpoint::new_group(
		"experimental",
		"build",
		"build code",
		CHECK_BUILD_POSITIONAL_PARAMETERS,
		BUILD_PARAMETERS,
	),
	Endpoint::new_group("experimental", "upgrade", "download latest binary", &[], &[]),
];

static BUILD_PARAMETERS: &[NamedParameter] = &[
	NamedParameter::boolean("advanced-numbers", "test for number intrinsics"),
	NamedParameter::boolean("tree-shake", "enable tree shake"),
	NamedParameter::boolean("minify", "remove white-space from the output"),
	NamedParameter::boolean("source-maps", "generate source maps"),
	NamedParameter::boolean("compact-diagnostics", "emit diagnostics in compact form"),
	NamedParameter::boolean("timings", "print time taken on various steps"),
	NamedParameter::boolean("watch", "watch input files, rerun on input change"),
	NamedParameter::value(
		"max-diagnostics",
		"maximum diagnostics to print (defaults to 30, pass `all` for all and `0` to count)",
	),
	NamedParameter::value("definition-file", "definition file to include"),
];

static CHECK_BUILD_POSITIONAL_PARAMETERS: &[PositionalParameter] = &[PositionalParameter::single(
	"input",
	"a entrypoint to a project. defaults to . or index.js etc",
)];

static CHECK_PARAMETERS: &[NamedParameter] = &[
	NamedParameter::boolean("advanced-numbers", "more support for number intrinsics"),
	NamedParameter::boolean("compact-diagnostics", "emit diagnostics in compact form"),
	NamedParameter::boolean("timings", "print time taken on various steps"),
	NamedParameter::boolean("watch", "watch input files, rerun on input change"),
	NamedParameter::value(
		"max-diagnostics",
		"maximum diagnostics to print (defaults to 30, pass `all` for all and `0` to count)",
	),
	NamedParameter::value("definition-file", "definition file to include"),
];

pub fn run_cli<T: crate::ReadFromFS, U: crate::WriteToFS>(
	cli_arguments: impl Iterator<Item = String>,
	read_file: T,
	write_file: U,
) -> Result<(), ExitCode> {
	let cli = CLI::new(ENDPOINTS, "type checker", Some("info"));
	let (binary_name, result) = cli.run();
	let (selected, arguments) = command_result_or_out(result, &binary_name)?;

	match selected.name {
		"info" => {
			crate::utilities::print_info();
			Ok(())
		}
		"check" => {
			let mut input = String::default();
			let mut watch = false;
			let mut definition_file = None;
			let mut timings = false;
			let mut compact_diagnostics = false;
			let mut max_diagnostics = MaxDiagnostics::default();
			let mut advanced_numbers = false;

			for argument in arguments {
				let argument = argument_result_or_out(argument)?;
				match argument.name {
					"input" => {
						input = argument.value.unwrap();
					}
					"advanced-numbers" => {
						advanced_numbers = true;
					}
					"compact-diagnostics" => {
						compact_diagnostics = true;
					}
					"timings" => {
						timings = true;
					}
					"watch" => {
						watch = true;
					}
					"max-diagnostics" => {
						// TODO unwraps
						max_diagnostics =
							MaxDiagnostics::from_str(&argument.value.unwrap()).unwrap();
					}
					"definition-file" => {
						definition_file = Some(PathBuf::from(argument.value.unwrap()));
					}
					name => unreachable!("{name}"),
				}
			}

			let type_check_options: TypeCheckOptions = if cfg!(target_family = "wasm") {
				Default::default()
			} else {
				TypeCheckOptions {
					measure_time: timings,
					advanced_numbers,
					..TypeCheckOptions::default()
				}
			};

			let entry_points = match get_entry_points(input) {
				Ok(entry_points) => entry_points,
				Err(_) => {
					print_to_cli(format_args!("Entry point error"));
					return Err(ExitCode::FAILURE);
				}
			};

			// check_and_report is written three times because cloning
			if watch {
				#[cfg(target_family = "wasm")]
				panic!("'watch' mode not supported on WASM");

				#[cfg(not(target_family = "wasm"))]
				{
					use notify_debouncer_full::new_debouncer;

					let (tx, rx) = std::sync::mpsc::channel();
					let mut debouncer =
						new_debouncer(Duration::from_millis(200), None, tx).unwrap();

					for e in &entry_points {
						debouncer.watch(e, notify::RecursiveMode::Recursive).unwrap();
					}

					let _ = check_and_report(
						entry_points.clone(),
						&read_file,
						timings,
						definition_file.clone(),
						max_diagnostics,
						type_check_options.clone(),
						compact_diagnostics,
					);

					for res in rx {
						match res {
							Ok(_e) => {
								let _out = check_and_report(
									entry_points.clone(),
									&read_file,
									timings,
									definition_file.clone(),
									max_diagnostics,
									type_check_options.clone(),
									compact_diagnostics,
								);
							}
							Err(error) => eprintln!("Error: {error:?}"),
						}
					}

					unreachable!()
				}
			} else {
				Err(check_and_report(
					entry_points,
					&read_file,
					timings,
					definition_file,
					max_diagnostics,
					type_check_options,
					compact_diagnostics,
				))
			}
		}
		"build" => {
			let mut input = String::default();
			let mut watch = false;
			let mut definition_file = None;
			let mut timings = false;
			let mut compact_diagnostics = false;
			let mut max_diagnostics = MaxDiagnostics::default();
			let mut advanced_numbers = false;
			let mut tree_shake = false;
			let mut source_maps = false;
			let mut minify = false;
			let mut output_path = PathBuf::from("ezno.out.js");

			for argument in arguments {
				let argument = argument_result_or_out(argument)?;
				match argument.name {
					"input" => {
						input = argument.value.unwrap();
					}
					"advanced-numbers" => {
						advanced_numbers = true;
					}
					"compact-diagnostics" => {
						compact_diagnostics = true;
					}
					"timings" => {
						timings = true;
					}
					"watch" => {
						watch = true;
					}
					"max-diagnostics" => {
						// TODO unwraps
						max_diagnostics =
							MaxDiagnostics::from_str(&argument.value.unwrap()).unwrap();
					}
					"definition-file" => {
						definition_file = Some(PathBuf::from(argument.value.unwrap()));
					}
					"output-path" => {
						output_path = PathBuf::from(argument.value.unwrap());
					}
					"tree-shake" => {
						tree_shake = true;
					}
					"minify" => {
						minify = true;
					}
					"source-maps" => {
						source_maps = true;
					}
					name => unreachable!("{name}"),
				}
			}

			let entry_points = match get_entry_points(input) {
				Ok(entry_points) => entry_points,
				Err(_) => {
					print_to_cli(format_args!("Entry point error"));
					return Err(ExitCode::FAILURE);
				}
			};

			#[cfg(not(target_family = "wasm"))]
			let start = timings.then(std::time::Instant::now);

			let config = BuildConfig {
				tree_shake,
				strip_whitespace: minify,
				source_maps,
				type_definition_module: definition_file,
				// TODO not sure
				output_path,
				other_transformers: None,
				lsp_mode: false,
			};

			let output = build(entry_points, &read_file, config);

			#[cfg(not(target_family = "wasm"))]
			if let Some(start) = start {
				eprintln!("Checked & built in {:?}", start.elapsed());
			};

			match output {
				Ok(BuildOutput {
					artifacts,
					check_output: CheckOutput { module_contents, diagnostics, .. },
				}) => {
					for output in artifacts {
						write_file(output.output_path.as_path(), output.content);
					}
					report_diagnostics_to_cli(
						diagnostics,
						&module_contents,
						compact_diagnostics,
						max_diagnostics,
					)
					.unwrap();
					print_to_cli(format_args!("Project built successfully 🎉",));
					Ok(())
				}
				Err(FailedBuildOutput(CheckOutput { module_contents, diagnostics, .. })) => {
					report_diagnostics_to_cli(
						diagnostics,
						&module_contents,
						compact_diagnostics,
						max_diagnostics,
					)
					.unwrap();
					Err(ExitCode::FAILURE)
				}
			}
		}
		"format" => {
			let mut inputs = String::default();
			let mut check = false;

			for argument in arguments {
				let _argument = argument_result_or_out(argument)?;
				// match argument.name {}
			}

			todo!()
			// format()
		}
		"parse" => {
			todo!()
		}
		#[cfg(target_family = "wasm")]
		"upgrade" => {
			print_to_cli(format_args!("Cannot self upgrade self"));
			Err(std::process::ExitCode::SUCCESS)
		}
		#[cfg(not(target_family = "wasm"))]
		"upgrade" => match crate::utilities::upgrade_self() {
			Ok(name) => {
				print_to_cli(format_args!("Successfully updated to {name}"));
				Err(std::process::ExitCode::SUCCESS)
			}
			Err(err) => {
				print_to_cli(format_args!("Error: {err}\nCould not upgrade binary. Retry manually from {repository}/releases", repository=env!("CARGO_PKG_REPOSITORY")));
				Err(std::process::ExitCode::FAILURE)
			}
		},
		"repl" => {
			todo!()
		}
		name => unreachable!("{name}"),
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

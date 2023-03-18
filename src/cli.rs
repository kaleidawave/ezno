#[allow(unused)]
use std::{
	collections::HashSet,
	env, fs,
	path::{Path, PathBuf},
	process::Command,
	time::Instant,
};

use crate::{error_handling::emit_ezno_diagnostic, temp::Output, utilities};
use crate::{temp::BuildOutput, utilities::print_to_cli};
use argh::FromArgs;
// use checker::{
// 	BuildOutput, Plugin, Project, TypeCheckSettings, TypeCheckingVisitorGenerators,
// 	TypeDefinitionModulePath,
// };

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
	// Check(CheckArguments),
	// Run(RunArguments),
	// Repl(repl::ReplArguments),
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

/// Build project
#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand, name = "build")]
struct BuildArguments {
	/// path to input file
	#[argh(positional)]
	input: PathBuf,
	/// path to output
	#[argh(positional)]
	output: Option<PathBuf>,
	/// whether to minify build output
	#[argh(switch, short = 'm')]
	minify: bool,
	/// whether to include comments in the output
	#[argh(switch)]
	no_comments: bool,
	/// build source maps
	#[argh(switch)]
	source_maps: bool,
	#[cfg(not(target_family = "wasm"))]
	/// whether to display compile times
	#[argh(switch)]
	timings: bool,
	// /// whether to re-build on file changes
	// #[argh(switch)]
	// watch: bool,
}

// /// Check project
// #[derive(FromArgs, PartialEq, Debug)]
// #[argh(subcommand, name = "check")]
// struct CheckArguments {
// 	/// path to input file
// 	#[argh(positional)]
// 	input: PathBuf,

// 	/// whether to re-check on file changes
// 	#[argh(switch)]
// 	watch: bool,
// }

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
fn file_system_resolver(path: &Path) -> Option<(String, Vec<(usize, parser::EmptyCursorId)>)> {
	// Cheaty
	if path.to_str() == Some("BLANK") {
		return Some((String::new(), Vec::new()));
	}
	match fs::read_to_string(path) {
		Ok(source) => Some((source, Vec::new())),
		Err(_) => None,
	}
}

#[cfg_attr(target_family = "wasm", wasm_bindgen::prelude::wasm_bindgen)]
pub fn run_cli() {
	let get_cli_args = crate::utilities::get_cli_args();
	let arguments: Vec<&str> = get_cli_args.iter().map(AsRef::as_ref).collect();

	if arguments.is_empty() {
		utilities::print_info();
		return;
	}

	let command = match FromArgs::from_args(&["ezno-cli"], &arguments.as_slice()) {
		Ok(TopLevel { nested }) => nested,
		Err(err) => {
			print_to_cli(format_args!("{}", err.output));
			return;
		}
	};

	match command {
		CompilerSubCommand::Info(_) => {
			utilities::print_info();
		}
		CompilerSubCommand::Build(build_config) => {
			let _output = build(build_config);
		}
		CompilerSubCommand::ASTExplorer(mut repl) => repl.run(),
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
		// CompilerSubCommand::Check(check_arguments) => {
		// 	let CheckArguments { input, watch } = check_arguments;
		// 	check(input, watch)
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
		// CompilerSubCommand::Repl(argument) => repl::run_deno_repl(argument),
	}
}

/// TODO + deserialize
struct _Settings {
	current_working_directory: Option<PathBuf>,
}

// fn check(entry_point: PathBuf, watch: bool) {
// 	let cwd = env::current_dir().unwrap();

// 	let type_definition_modules_paths: HashSet<_> = if USE_PACKED {
// 		IntoIterator::into_iter([TypeDefinitionModulePath::SimpleBin]).map(Into::into).collect()
// 	} else {
// 		IntoIterator::into_iter(["checker/definitions/simple.d.ts"])
// 			.map(TypeDefinitionModulePath::from)
// 			.collect()
// 	};

// 	let visitors = checker::SecondPassVisitors {
// 		expression_visitors: vec![Box::new(checker::temp::AssertTypeExplainer)],
// 		..Default::default()
// 	};

// 	let project = Project::new(
// 		&file_system_resolver,
// 		vec![],
// 		Default::default(),
// 		type_definition_modules_paths,
// 		cwd.clone(),
// 		visitors,
// 	);

// 	// entry_point,

// 	let mut project = match project {
// 		Ok(project) => project,
// 		Err(error_handler) => {
// 			print_error_warning_info_handler(error_handler);
// 			return;
// 		}
// 	};

// 	// let check_project = || {
// 	let error_warning_info_handler = project.check(entry_point.clone());
// 	if let Err(error_warning_info_handler) = error_warning_info_handler {
// 		print_error_warning_info_handler(error_warning_info_handler);
// 	} else {
// 		print_error_warning_info_handler(project.pull_error_warning_info_handler());
// 		println!("Project checked ✅, No errors 🎉");
// 	}
// 	// };

// 	if watch {
// 		watch_command(cwd, |_path_change| {
// 			// TODO use _path_change info from change
// 			let error_warning_info_handler = project.check(entry_point.clone());
// 			if let Err(error_warning_info_handler) = error_warning_info_handler {
// 				print_error_warning_info_handler(error_warning_info_handler);
// 			} else {
// 				print_error_warning_info_handler(project.pull_error_warning_info_handler());
// 				println!("No errors 🎉");
// 			}
// 		});
// 	}
// }

// // <TData> data: TData
// fn watch_command(cwd: PathBuf, mut callback: impl FnMut(PathBuf)) {
// 	use std::time::Duration;
// 	let (tx, rx) = std::sync::mpsc::channel();

// 	let debounce_time = Duration::from_millis(500);
// 	let mut watcher = notify::watcher(tx, debounce_time).unwrap();

// 	// Add a path to be watched. All files and directories at that path and
// 	// below will be monitored for changes.
// 	notify::Watcher::watch(&mut watcher, cwd.as_path(), notify::RecursiveMode::Recursive).unwrap();

// 	println!("Watching for changes on '{}'", cwd.display());
// 	loop {
// 		match rx.recv() {
// 			Ok(event) => match event {
// 				notify::DebouncedEvent::NoticeWrite(_)
// 				| notify::DebouncedEvent::NoticeRemove(_)
// 				| notify::DebouncedEvent::Rescan
// 				| notify::DebouncedEvent::Chmod(_) => {}
// 				notify::DebouncedEvent::Create(path)
// 				| notify::DebouncedEvent::Write(path)
// 				| notify::DebouncedEvent::Remove(path)
// 				| notify::DebouncedEvent::Rename(path, _) => {
// 					println!("'{}' changed, checking project again", path.display());
// 					callback(path);
// 					println!("Watching for changes on '{}'", cwd.display());
// 				}
// 				notify::DebouncedEvent::Error(_, _) => {
// 					eprintln!("Error");
// 					break;
// 				}
// 			},
// 			Err(e) => {
// 				eprintln!("watch error: {:?}", e);
// 				break;
// 			}
// 		}
// 	}
// }

// TODO needs settings information structure
fn build(build_arguments: BuildArguments) -> Result<(), ()> {
	// let _cwd = env::current_dir().unwrap();
	let BuildArguments {
		minify,
		no_comments,
		input: entry_path,
		output: output_path,
		source_maps: _,
		#[cfg(not(target_family = "wasm"))]
		timings,
		// watch,
	} = build_arguments;

	// if watch {
	// 	unimplemented!("Watch mode for build");
	// }

	// TODO debug_types temp
	// let type_check_settings = TypeCheckSettings { ..Default::default() };
	let _output_settings = parser::ToStringSettings {
		include_comments: !no_comments,
		pretty: !minify,
		..Default::default()
	};

	// let test_plugin = Plugin {
	// 	name: "main.rs - plugin".to_owned(),
	// 	visitors: TypeCheckingVisitorGenerators {
	// 		// first_pass: vec![Box::new(|| checker::FirstPassVisitors {
	// 		// 	expression_visitors: vec![Box::new(example_visitors::StringLiteralPrinter)],
	// 		// 	..Default::default()
	// 		// })],
	// 		second_pass: vec![Box::new(|| checker::SecondPassVisitors {
	// 			expression_visitors: vec![Box::new(checker::temp::AssertTypeExplainer)],
	// 			..Default::default()
	// 		})],
	// 		..Default::default()
	// 	},
	// 	..Default::default()
	// };

	// let plugins = vec![
	// 	// ezno_web_framework::get_plugin(),
	// 	test_plugin,
	// ];

	// TODO via settings
	// let type_definition_modules_paths: HashSet<_> = HashSet::new();
	// let type_definition_modules_paths: HashSet<_> = if USE_PACKED {
	// 	IntoIterator::into_iter([TypeDefinitionModulePath::SimpleBin]).map(Into::into).collect()
	// } else {
	// 	IntoIterator::into_iter(["checker/definitions/simple.d.ts"])
	// 		.map(TypeDefinitionModulePath::from)
	// 		.collect()
	// };

	// let project = Project::new(
	// 	&file_system_resolver,
	// 	plugins,
	// 	type_check_settings,
	// 	type_definition_modules_paths,
	// 	cwd,
	// 	// TODO
	// 	Default::default(),
	// );

	// // TODO temp
	// let project = match project {
	// 	Ok(project) => project,
	// 	Err(error_handler) => {
	// 		print_error_warning_info_handler(error_handler);
	// 		return Err(());
	// 	}
	// };

	#[cfg(not(target_family = "wasm"))]
	let now = Instant::now();
	// let result = project.build(entry_point, output_settings);

	let (fs, result) = crate::temp::build(
		utilities::read_fs_path_to_string(entry_path.clone()).expect("Could not read path"),
		// TODO
		entry_path.display().to_string(),
		output_path
			.map(|path| path.into_os_string().into_string().expect("Invalid path"))
			.unwrap_or_default(),
	);

	// TODO this should be integrated with project for a breakdown of
	// each stage etc.
	#[cfg(not(target_family = "wasm"))]
	if timings {
		let elapsed = now.elapsed();
		eprintln!("Project built in {:?}", elapsed);
	}

	match result {
		Ok(BuildOutput { outputs, temp_warnings_and_infos }) => {
			for error in temp_warnings_and_infos {
				emit_ezno_diagnostic(&fs, error).unwrap();
			}
			// print_error_warning_info_handler(error_warning_info_handler);
			// if let Some(output) = output_path {
			// 	// if output.len() == 1 {
			// 	// 	fs::write(output, output.into_values().next().unwrap()).unwrap();
			// 	// } else {
			// 	// 	for (filename, content) in output {
			// 	// 		let final_path = output.join(filename);
			// 	// 		fs::write(final_path, content).unwrap();
			// 	// 	}
			// 	// }
			// } else {
			// }

			// TODO path
			for Output { content, .. } in outputs {
				print_to_cli(format_args!("{content}"));
			}
			Ok(())
		}
		Err(errors) => {
			for error in errors {
				emit_ezno_diagnostic(&fs, error).unwrap();
			}
			// print_error_warning_info_handler(error_handler);
			Err(())
		}
	}
}

#[allow(unused)]
use std::{
	collections::HashSet,
	env, fs,
	path::{Path, PathBuf},
	process::Command,
	time::Instant,
};

use argh::FromArgs;
// use checker::{
// 	BuildOutput, Plugin, Project, TypeCheckSettings, TypeCheckingVisitorGenerators,
// 	TypeDefinitionModulePath,
// };

mod ast_explorer;
// mod error_handling;
// mod example_visitors;
// mod repl;
mod utilities;

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
	// Build(BuildArguments),
	// Check(CheckArguments),
	// Run(RunArguments),
	// Repl(repl::ReplArguments),
	ASTExplorer(ast_explorer::ExplorerArguments),
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

// /// Build project
// #[derive(FromArgs, PartialEq, Debug)]
// #[argh(subcommand, name = "build")]
// struct BuildArguments {
// 	/// path to input file
// 	#[argh(positional)]
// 	input: PathBuf,
// 	/// path to output
// 	#[argh(positional)]
// 	output: Option<PathBuf>,
// 	/// whether to minify build output
// 	#[argh(switch, short = 'm')]
// 	minify: bool,
// 	/// whether to include comments in the output
// 	#[argh(switch)]
// 	no_comments: bool,
// 	/// build source maps
// 	#[argh(switch)]
// 	source_maps: bool,
// 	/// whether to re-build on file changes
// 	#[argh(switch)]
// 	watch: bool,
// 	/// whether to display compile times
// 	#[argh(switch)]
// 	timings: bool,
// }

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

fn main() {
	if env::args().len() == 1 {
		utilities::print_info();
		return;
	}

	let args: TopLevel = argh::from_env();

	// std::panic::set_hook

	match args.nested {
		CompilerSubCommand::Info(_) => {
			utilities::print_info();
		}
		// CompilerSubCommand::Build(build_config) => {
		// 	let _output = build(build_config);
		// }
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

// // TODO needs setting structure
// fn build(build_arguments: BuildArguments) -> Result<(), ()> {
// 	let cwd = env::current_dir().unwrap();
// 	let BuildArguments {
// 		minify,
// 		no_comments,
// 		input: entry_point,
// 		output: output_path,
// 		source_maps: _,
// 		watch,
// 		timings,
// 	} = build_arguments;

// 	if timings && watch {
// 		todo!("error");
// 	}

// 	if watch {
// 		unimplemented!("Watch mode for build");
// 	}

// 	// TODO debug_types temp
// 	let type_check_settings = TypeCheckSettings { ..Default::default() };
// 	let output_settings = parser::ToStringSettings {
// 		include_comments: !no_comments,
// 		pretty: !minify,
// 		..Default::default()
// 	};

// 	let test_plugin = Plugin {
// 		name: "main.rs - plugin".to_owned(),
// 		visitors: TypeCheckingVisitorGenerators {
// 			// first_pass: vec![Box::new(|| checker::FirstPassVisitors {
// 			// 	expression_visitors: vec![Box::new(example_visitors::StringLiteralPrinter)],
// 			// 	..Default::default()
// 			// })],
// 			second_pass: vec![Box::new(|| checker::SecondPassVisitors {
// 				expression_visitors: vec![Box::new(checker::temp::AssertTypeExplainer)],
// 				..Default::default()
// 			})],
// 			..Default::default()
// 		},
// 		..Default::default()
// 	};

// 	let plugins = vec![
// 		// ezno_web_framework::get_plugin(),
// 		test_plugin,
// 	];

// 	// TODO via settings
// 	// let type_definition_modules_paths: HashSet<_> = HashSet::new();
// 	let type_definition_modules_paths: HashSet<_> = if USE_PACKED {
// 		IntoIterator::into_iter([TypeDefinitionModulePath::SimpleBin]).map(Into::into).collect()
// 	} else {
// 		IntoIterator::into_iter(["checker/definitions/simple.d.ts"])
// 			.map(TypeDefinitionModulePath::from)
// 			.collect()
// 	};

// 	let project = Project::new(
// 		&file_system_resolver,
// 		plugins,
// 		type_check_settings,
// 		type_definition_modules_paths,
// 		cwd,
// 		// TODO
// 		Default::default(),
// 	);

// 	// TODO temp
// 	let project = match project {
// 		Ok(project) => project,
// 		Err(error_handler) => {
// 			print_error_warning_info_handler(error_handler);
// 			return Err(());
// 		}
// 	};

// 	let now = Instant::now();
// 	let result = project.build(entry_point, output_settings);
// 	let elapsed = now.elapsed();

// 	// TODO this should be integrated with project for a breakdown of
// 	// each stage etc.
// 	if timings {
// 		eprintln!("Project built in {:?}", elapsed);
// 	}

// 	match result {
// 		Ok(BuildOutput { output: build_outputs, error_warning_info_handler }) => {
// 			print_error_warning_info_handler(error_warning_info_handler);
// 			if let Some(output) = output_path {
// 				// TODO write errors
// 				if build_outputs.len() == 1 {
// 					fs::write(output, build_outputs.into_values().next().unwrap()).unwrap();
// 				} else {
// 					for (filename, content) in build_outputs {
// 						let final_path = output.join(filename);
// 						fs::write(final_path, content).unwrap();
// 					}
// 				}
// 			} else {
// 				for (_filename, content) in build_outputs {
// 					println!("{}", content);
// 				}
// 			}
// 			Ok(())
// 		}
// 		Err(error_handler) => {
// 			print_error_warning_info_handler(error_handler);
// 			Err(())
// 		}
// 	}
// }

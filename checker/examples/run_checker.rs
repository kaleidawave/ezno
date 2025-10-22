#[cfg(feature = "ezno-parser")]
fn main() {
	use ezno_checker::{Diagnostic, TypeCheckOptions, check_project, synthesis};
	use std::{fs, path::Path};

	fn resolver(path: &std::path::Path) -> Option<Vec<u8>> {
		fs::read(path).ok()
	}

	let default_path = Path::new("private").join("tocheck").join("aaa.tsx");
	let simple_dts_path = Path::new("checker").join("definitions").join("simple.d.ts");
	let overrides_dts_path = Path::new("checker").join("definitions").join("overrides.d.ts");

	let mut args = std::env::args().skip(1);

	let path = args.next().and_then(|arg| (!arg.starts_with("--")).then_some(arg));

	let path = path.as_deref().map_or(default_path.as_path(), Path::new);

	let mut use_simple = false;
	let mut no_cache = false;
	let mut debug_types = false;
	let mut no_lib = false;
	let mut debug_dts = false;
	let mut extras = false;
	let mut advanced_numbers = false;
	let mut measure_time = false;
	let mut simple_diagnostics = false;

	let mut print_types = false;
	let mut print_events = false;
	let mut print_called_functions = false;

	for arg in args {
		match arg.as_str() {
			"--simple-dts" => {
				use_simple = true;
			}
			"--no-cache" => {
				no_cache = true;
			}
			"--debug-types" => {
				debug_types = true;
			}
			"--no-lib" => {
				no_lib = true;
			}
			"--debug-dts" => {
				debug_dts = true;
			}
			"--extras" => {
				extras = true;
			}
			"--advanced-numbers" => {
				advanced_numbers = true;
			}
			"--timings" => {
				measure_time = true;
			}
			"--types" => {
				print_types = true;
			}
			"--events" => {
				print_events = true;
			}
			"--called-functions" => {
				print_called_functions = true;
			}
			"--simple-diagnostics" => {
				simple_diagnostics = true;
			}
			arg => {
				eprintln!("unknown argument {arg}");
			}
		}
	}

	let type_definition_files = if no_lib {
		Vec::new()
	} else {
		let definition_file = if use_simple {
			simple_dts_path.clone()
		} else if no_cache {
			overrides_dts_path.clone()
		} else {
			ezno_checker::INTERNAL_DEFINITION_FILE_PATH.into()
		};
		vec![definition_file]
	};

	let entry_points = vec![path.to_path_buf()];

	let max_inline_count = 600;

	let options = TypeCheckOptions {
		debug_types,
		record_all_assignments_and_reads: true,
		max_inline_count,
		debug_dts,
		extra_syntax: extras,
		advanced_numbers,
		measure_time,
		..Default::default()
	};

	let result = check_project::<_, synthesis::EznoParser>(
		entry_points,
		type_definition_files,
		&resolver,
		options,
		None,
	);

	if print_types {
		eprintln!("Types:");
		for (type_id, item) in result.types.user_types() {
			eprintln!("\t{type_id:?}: {item:?}");
		}
	}

	if print_events {
		eprintln!("Events on entry:");
		let (_, entry_module) = result.modules.into_iter().next().unwrap();
		for item in entry_module.info.get_events() {
			eprintln!("\t{item:?}");
		}
	}

	if print_called_functions {
		eprintln!("Called function: {:?}", result.types.called_functions);
	}

	if measure_time {
		let count = result.diagnostics.into_iter().len();
		eprintln!("Found {count} diagnostics in {chronometer:?}", chronometer = result.chronometer);
	} else if simple_diagnostics {
		for diagnostic in result.diagnostics {
			println!("{reason}", reason = diagnostic.reason());
		}
	} else {
		eprintln!("Diagnostics:");
		for diagnostic in result.diagnostics {
			let prefix: &str = match diagnostic.kind() {
				ezno_checker::DiagnosticKind::Error => "ERROR",
				ezno_checker::DiagnosticKind::Warning => "WARNING",
				ezno_checker::DiagnosticKind::Info => "INFO",
			};
			match diagnostic {
				Diagnostic::Global { reason, kind: _ } => {
					eprintln!("\t{prefix}: {reason}");
				}
				Diagnostic::Position { reason, position, kind: _ } => {
					eprintln!("\t{prefix} (@{position:?}): {reason} ");
				}
				Diagnostic::PositionWithAdditionalLabels { reason, position, labels, kind: _ } => {
					eprintln!("\t{prefix} (@{position:?}): {reason}");
					for (reason, position) in labels {
						eprintln!("\t\t{reason} {position:?}");
					}
				}
			}
		}
	}
}

#[cfg(not(feature = "ezno-parser"))]
fn main() {
	println!("Requires 'ezno-parser'")
}

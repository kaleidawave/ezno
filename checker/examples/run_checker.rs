#[cfg(feature = "ezno-parser")]
fn main() {
	use ezno_checker::{check_project, synthesis, Diagnostic, TypeCheckOptions};
	use std::{fs, path::Path, time::Instant};

	let default_path = Path::new("private").join("tocheck").join("aaa.tsx");
	let simple_dts_path = Path::new("checker").join("definitions").join("simple.d.ts");
	let overrides_dts_path = Path::new("checker").join("definitions").join("overrides.d.ts");

	let args: Vec<_> = std::env::args().skip(1).collect();

	let path = args
		.first()
		.and_then(|arg| (!arg.starts_with("--")).then_some(arg))
		.map_or(default_path.as_path(), Path::new);

	let use_simple = args.iter().any(|item| item == "--simple-dts");
	let no_cache = args.iter().any(|item| item == "--no-cache");
	let debug_types = args.iter().any(|item| item == "--debug-types");
	let no_lib = args.iter().any(|item| item == "--no-lib");
	let debug_dts = args.iter().any(|item| item == "--debug-dts");
	let extras = args.iter().any(|item| item == "--extras");
	let advanced_number_intrinsics = args.iter().any(|item| item == "--advanced-numbers");

	let now = Instant::now();

	let resolver = |path: &std::path::Path| fs::read(path).ok();

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

	let options = TypeCheckOptions {
		debug_types,
		record_all_assignments_and_reads: true,
		max_inline_count: 600,
		debug_dts,
		extra_syntax: extras,
		advanced_number_intrinsics,
		..Default::default()
	};

	let result = check_project::<_, synthesis::EznoParser>(
		entry_points,
		type_definition_files,
		&resolver,
		options,
		(),
		None,
	);

	if args.iter().any(|arg| arg == "--types") {
		eprintln!("Types:");
		for (type_id, item) in result.types.user_types() {
			eprintln!("\t{type_id:?}: {item:?}");
		}
	}

	if args.iter().any(|arg| arg == "--events") {
		eprintln!("Events on entry:");
		let (_, entry_module) = result.modules.into_iter().next().unwrap();
		for item in entry_module.info.get_events() {
			eprintln!("\t{item:?}");
		}
	}

	if args.iter().any(|arg| arg == "--called-functions") {
		eprintln!("Called function: {:?}", result.types.called_functions);
	}

	if args.iter().any(|arg| arg == "--time") {
		let end = now.elapsed();
		let count = result.diagnostics.into_iter().len();
		eprintln!("Found {count} diagnostics in {end:?}");
	} else if args.iter().any(|arg| arg == "--verbose-diagnostics") {
		eprintln!("Diagnostics:");
		for diagnostic in result.diagnostics {
			eprintln!("{diagnostic:?}");
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

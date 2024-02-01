#[cfg(feature = "ezno-parser")]
fn main() {
	use ezno_checker::{check_project, synthesis, Diagnostic};
	use std::{
		collections::HashSet,
		fs,
		path::{Path, PathBuf},
		time::Instant,
	};

	let default_path = Path::new("private").join("tocheck").join("aaa.tsx");
	let simple_dts_path = Path::new("checker").join("definitions").join("simple.d.ts");

	let mut args: Vec<_> = std::env::args().skip(1).collect();

	let first = &args.drain(0..1).next();
	let path = first.as_ref().map(Path::new).unwrap_or_else(|| default_path.as_path());
	let use_simple = args.iter().any(|item| item == "--simple-dts");

	let now = Instant::now();

	let resolver = |path: &std::path::Path| {
		if path == PathBuf::from(ezno_checker::INTERNAL_DEFINITION_FILE_PATH) {
			Some(ezno_checker::INTERNAL_DEFINITION_FILE.to_owned())
		} else {
			fs::read_to_string(path).ok().map(Into::into)
		}
	};

	let type_definition_files = HashSet::from_iter([if use_simple {
		simple_dts_path.to_path_buf()
	} else {
		ezno_checker::INTERNAL_DEFINITION_FILE_PATH.into()
	}]);

	let result = check_project::<_, synthesis::EznoParser>(
		vec![path.to_path_buf()],
		type_definition_files,
		resolver,
		None,
		(),
	);

	if args.iter().any(|arg| arg == "--types") {
		eprintln!("Types:");
		for (type_id, item) in result.types.into_vec_temp() {
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

	if args.iter().any(|arg| arg == "--time") {
		let end = now.elapsed();
		let count = result.diagnostics.into_iter().len();
		eprintln!("Found {count} diagnostics in {end:?}");
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

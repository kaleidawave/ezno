#[cfg(feature = "ezno-parser")]
fn main() {
	use ezno_checker::{check_project, synthesis, Diagnostic};
	use std::{
		env, fs,
		path::{Path, PathBuf},
		time::Instant,
	};

	let name = env::args().nth(1).unwrap_or_else(|| "examples/test.ts".to_string());
	let path = Path::new(&name);

	let now = Instant::now();

	let result = check_project::<_, synthesis::EznoParser>(
		vec![path.to_path_buf()],
		std::iter::once(ezno_checker::INTERNAL_DEFINITION_FILE_PATH.into()).collect(),
		|path: &std::path::Path| {
			if path == PathBuf::from(ezno_checker::INTERNAL_DEFINITION_FILE_PATH) {
				Some(ezno_checker::INTERNAL_DEFINITION_FILE.to_owned())
			} else {
				fs::read_to_string(path).ok()
			}
		},
		None,
		(),
	);

	let args: Vec<_> = env::args().collect();

	if !result.diagnostics.has_error() {
		if args.iter().any(|arg| arg == "--types") {
			eprintln!("Types:");
			for (type_id, item) in result.types.into_vec_temp() {
				eprintln!("\t{type_id:?}: {item:?}");
			}
		}
		if args.iter().any(|arg| arg == "--events") {
			eprintln!("Events on entry:");
			let (_, entry_module) = result.modules.into_iter().next().unwrap();
			for item in entry_module.facts.get_events() {
				eprintln!("\t{item:?}");
			}
		}
	}

	if args.iter().any(|arg| arg == "--time") {
		let end = now.elapsed();
		let count = result.diagnostics.into_iter().len();
		eprintln!("Found {count} diagnostics in {end:?}");
	} else {
		eprintln!("Diagnostics:");
		for diagnostic in result.diagnostics {
			let prefix: char = match diagnostic.kind() {
				ezno_checker::DiagnosticKind::Error => 'E',
				ezno_checker::DiagnosticKind::Warning => 'W',
				ezno_checker::DiagnosticKind::Info => 'I',
			};
			match diagnostic {
				Diagnostic::Global { reason, kind: _ } => {
					eprintln!("\t{prefix}: {reason}");
				}
				Diagnostic::Position { reason, position, kind: _ } => {
					eprintln!("\t{prefix}: {reason} {position:?}");
				}
				Diagnostic::PositionWithAdditionalLabels { reason, position, labels, kind: _ } => {
					eprintln!("\t{prefix}: {reason} {position:?}");
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

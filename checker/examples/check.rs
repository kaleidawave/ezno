#[cfg(feature = "ezno-parser")]
fn main() {
	use ezno_checker::{check_project, synthesis};
	use std::{
		env, fs,
		path::{Path, PathBuf},
	};

	let name = env::args().nth(1).unwrap_or_else(|| "examples/test.ts".to_string());
	let path = Path::new(&name);

	let (diagnostics, post_check_data) = check_project::<_, synthesis::EznoParser>(
		path.to_path_buf(),
		std::iter::once(ezno_checker::INTERNAL_DEFINITION_FILE_PATH.into()).collect(),
		|path: &std::path::Path| {
			if path == PathBuf::from(ezno_checker::INTERNAL_DEFINITION_FILE_PATH) {
				Some(ezno_checker::INTERNAL_DEFINITION_FILE.to_owned())
			} else {
				fs::read_to_string(path).ok()
			}
		},
		None,
	);

	let args: Vec<_> = env::args().collect();

	if let Ok(mut post_check_data) = post_check_data {
		if args.iter().any(|arg| arg == "--types") {
			eprintln!("Types:");
			for item in post_check_data.types.into_vec_temp() {
				eprintln!("\t{item:?}");
			}
		}
		if args.iter().any(|arg| arg == "--events") {
			eprintln!("Events on entry:");
			let entry_module =
				post_check_data.modules.remove(&post_check_data.entry_source).unwrap();
			for item in entry_module.facts.get_events() {
				eprintln!("\t{item:?}");
			}
		}
	}

	eprintln!("Diagnostics:");
	for diagnostic in diagnostics {
		eprintln!("\t{}", diagnostic.reason());
	}
}

#[cfg(not(feature = "ezno-parser"))]
fn main() {
	println!("Requires 'ezno-parser'")
}

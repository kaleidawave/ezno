#![allow(unused)]
use std::{
	collections::HashSet,
	panic,
	path::PathBuf,
	sync::{Arc, Mutex},
};

use checker::diagnostics;
use parser::ASTNode;

mod specification {
	use super::check_errors;

	// from build.rs
	include!(concat!(env!("OUT_DIR"), "/specification.rs"));
}

// fn catch_unwind_silent<F: FnOnce() -> R + panic::UnwindSafe, R>(f: F) -> std::thread::Result<R> {
// 	let prev_hook = panic::take_hook();
// 	let result = panic::catch_unwind(f);
// 	panic::set_hook(prev_hook);
// 	result
// }

/// Called by each test
fn check_errors(
	heading: &'static str,
	line: usize,
	code: &'static str,
	expected_diagnostics: &[&'static str],
) {
	// let mut fs = parser::source_map::MapFileStore::<parser::source_map::NoPathMap>::default();
	// let source =
	// 	parser::source_map::FileSystem::new_source_id(&mut fs, PathBuf::default(), code.to_owned());
	// let module =
	// 	parser::Module::from_string(code.to_owned(), parser::ParseOptions::default(), source, None)
	// 		.unwrap();

	// let global_buffer = Arc::new(Mutex::new(String::new()));
	// let old_panic_hook = panic::take_hook();

	// panic::set_hook({
	// 	let global_buffer = global_buffer.clone();
	// 	Box::new(move |info| {
	// 		let mut global_buffer = global_buffer.lock().unwrap();
	// 		if let Some(position) = info.location() {
	// 			global_buffer.push_str(&format!("panicked at {:?}", position));
	// 		} else {
	// 			global_buffer.push_str(&info.to_string());
	// 		}
	// 	})
	// });

	// let result = panic::catch_unwind(|| {
	let result = checker::check_project::<_, parser::Module>(
		PathBuf::from("TEST_CODE"),
		std::iter::once(checker::INTERNAL_DEFINITION_FILE_PATH.into()).collect(),
		|path| {
			if path == std::path::Path::new(checker::INTERNAL_DEFINITION_FILE_PATH) {
				Some(checker::INTERNAL_DEFINITION_FILE.to_owned())
			} else {
				Some(code.to_owned())
			}
		},
	);
	// });

	// panic::set_hook(old_panic_hook);

	let (diagnostics, ..) = result;
	let diagnostics: Vec<String> = diagnostics
		.into_iter()
		.map(|diag| {
			let (reason, pos) = diag.reason_and_position();
			if let Some(pos) = pos {
				// TODO position
				reason
			} else {
				reason
			}
		})
		.collect();

	if &diagnostics != expected_diagnostics {
		panic!(
			"{}",
			pretty_assertions::Comparison::new(&diagnostics, expected_diagnostics).to_string()
		)
	}

	// match result {
	// 	Ok(result) => {}
	// 	Err(error) => Err(Arc::into_inner(global_buffer).unwrap().into_inner().unwrap()),
	// }
}

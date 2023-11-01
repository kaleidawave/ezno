#![allow(unused)]
use std::{
	collections::HashSet,
	panic,
	path::PathBuf,
	sync::{Arc, Mutex},
};

use checker::{diagnostics, synthesis::EznoParser};
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
	// (Path, Content)
	code: &[(&'static str, &'static str)],
	expected_diagnostics: &[&'static str],
) {
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

	// TODO could test these
	let type_check_options = None;
	let parse_options = Default::default();

	// eprintln!("{:?}", code);

	// let result = panic::catch_unwind(|| {
	let result = checker::check_project::<_, EznoParser>(
		PathBuf::from("main.ts"),
		std::iter::once(checker::INTERNAL_DEFINITION_FILE_PATH.into()).collect(),
		|path| {
			if path == std::path::Path::new(checker::INTERNAL_DEFINITION_FILE_PATH) {
				Some(checker::INTERNAL_DEFINITION_FILE.to_owned())
			} else {
				if code.len() == 1 {
					Some(code[0].1.to_owned())
				} else {
					code.iter().find_map(|(code_path, content)| {
						(std::path::Path::new(code_path) == path)
							.then_some(content.to_owned().to_owned())
					})
				}
			}
		},
		type_check_options,
		parse_options,
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
			pretty_assertions::Comparison::new(expected_diagnostics, &diagnostics).to_string()
		)
	}

	// match result {
	// 	Ok(result) => {}
	// 	Err(error) => Err(Arc::into_inner(global_buffer).unwrap().into_inner().unwrap()),
	// }
}

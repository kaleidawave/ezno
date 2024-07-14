#![allow(unused)]
use std::{
	collections::HashSet,
	panic,
	path::{Path, PathBuf},
	process,
	sync::{Arc, Mutex},
};

use checker::{
	diagnostics,
	source_map::{Nullable, SourceId},
	synthesis::EznoParser,
};

// This is here as it is used in the included `/specification.rs`
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

// TODO under cfg
const SIMPLE_DTS: Option<&str> = Some(include_str!("../definitions/simple.d.ts"));
// const SIMPLE_DTS: Option<&str> = None;

const IN_CI: bool = option_env!("CI").is_some();

/// Called by each test
fn check_errors(
	heading: &'static str,
	_line: usize,
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
	let type_check_options = Default::default();

	// eprintln!("{:?}", code);

	// let result = panic::catch_unwind(|| {

	if IN_CI {
		eprintln!("::group::Running {heading}");
	}

	let definition_file_name: PathBuf = if SIMPLE_DTS.is_some() {
		"./checker/definitions/simple.d.ts".into()
	} else {
		checker::INTERNAL_DEFINITION_FILE_PATH.into()
	};
	let type_definition_files = std::iter::once(definition_file_name.clone()).collect();

	let result = checker::check_project::<_, EznoParser>(
		vec![PathBuf::from("main.tsx")],
		type_definition_files,
		|path: &Path| -> Option<Vec<u8>> {
			if path == definition_file_name.as_path() {
				Some(SIMPLE_DTS.unwrap().to_owned().into_bytes())
			} else if code.len() == 1 {
				Some(code[0].1.to_owned().into())
			} else {
				code.iter()
					.find_map(|(code_path, content)| {
						(std::path::Path::new(code_path) == path)
							.then_some(content.to_owned().to_owned())
					})
					.map(Into::into)
			}
		},
		type_check_options,
		(),
		None,
	);
	// });

	// panic::set_hook(old_panic_hook);

	let diagnostics: Vec<String> = result
		.diagnostics
		.into_iter()
		.map(|diag| {
			let (reason, pos) = diag.reason_and_position();
			if let Some(pos) = pos {
				assert_ne!(pos.source, SourceId::NULL);
				// TODO position
				reason
			} else {
				reason
			}
		})
		.collect();

	if IN_CI {
		eprintln!("::endgroup::");
	}

	if diagnostics != expected_diagnostics {
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

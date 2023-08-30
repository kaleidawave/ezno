#![allow(unused)]
use std::{collections::HashSet, path::PathBuf};

use checker::diagnostics;
use parser::ASTNode;

mod specification {
	use super::check_errors;

	// from build.rs
	include!(concat!(env!("OUT_DIR"), "/specification.rs"));
}

fn check_errors(
	heading: &'static str,
	line: usize,
	code: &'static str,
	expected_diagnostics: &[&'static str],
) {
	let mut fs = parser::source_map::MapFileStore::default();
	let source =
		parser::source_map::FileSystem::new_source_id(&mut fs, PathBuf::default(), code.to_owned());
	let module = parser::Module::from_string(
		code.to_owned(),
		parser::ParseOptions::default(),
		source,
		None,
		Vec::new(),
	)
	.unwrap();

	let res = checker::synthesis::module::synthesize_module_root(
		&module,
		std::iter::once(checker::INTERNAL_DEFINITION_FILE_PATH.into()).collect(),
		|_| Some(checker::INTERNAL_DEFINITION_FILE.to_owned()),
	);
	let (diagnostics, ..) = res;

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

	pretty_assertions::assert_eq!(
		diagnostics,
		expected_diagnostics,
		"issue with {}, specification:{}",
		heading,
		line
	)
}

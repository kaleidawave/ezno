#![no_main]

use ezno_checker::{check_project, synthesis, TypeCheckOptions};
use libfuzzer_sys::{fuzz_target, Corpus};
use std::str;

/// check_project_naive throws random strings into the ezno checker, validating that none of them make the checker panic.
fn do_fuzz(data: &str) -> Corpus {
	let input = data.trim_start();

	let definition_file = ezno_checker::INTERNAL_DEFINITION_FILE_PATH.into();
	let type_definition_files = vec![definition_file];

	// `lsp_mode` <=> partial syntax
	let options = TypeCheckOptions { lsp_mode: true, ..Default::default() };

	let root = "index.ts";

	let _result = check_project::<_, synthesis::EznoParser>(
		vec![root.into()],
		type_definition_files,
		&|_path: &std::path::Path| Some(input.to_owned()),
		options,
		(),
		None,
	);

	Corpus::Keep
}

fuzz_target!(|data: &str| {
	do_fuzz(data);
});

#![no_main]

mod common {
	include!(concat!(env!("OUT_DIR"), "/common.rs")); // from build.rs
}

use ezno_parser::{ASTNode, Module, ParseOptions, ToStringOptions};
use libfuzzer_sys::{fuzz_target, Corpus};
use pretty_assertions::assert_eq;

/// do_fuzz will accept a valid JS string and attempt to parse it twice and compare
/// the rendered output of both parses.
fn do_fuzz(data: common::FuzzSource) -> Corpus {
	let input = data.source;

	const STACK_SIZE_MB: usize = 32;
	let parse_options = ParseOptions {
		stack_size: Some(STACK_SIZE_MB * 1024 * 1024),
		jsx: false,
		type_annotations: false,
		// fixes some strange ; issues in asserting outputs same
		retain_blank_lines: true,
		..Default::default()
	};
	let Ok(module1) = Module::from_string(input.to_owned(), parse_options) else {
		return Corpus::Reject;
	};

	let to_string_options = ToStringOptions::default();

	let output1 = module1.to_string(&to_string_options);

	let Ok(module2) = Module::from_string(output1.to_owned(), parse_options) else {
		panic!("input: `{input}`\noutput1: `{output1}`\n\nThis parse should not error because it was just parsed above");
	};

	let output2 = module2.to_string(&to_string_options);

	// Ignore whitespace for now
	assert_eq!(output1, output2, "outputs different for {module1:?} vs {module2:?} for {input:?}");

	Corpus::Keep
}

fuzz_target!(|data: common::FuzzSource| {
	do_fuzz(data);
});

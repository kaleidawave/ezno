#![no_main]

use ezno_parser::{ASTNode, Module, ParseOptions, SourceId, ToStringOptions};
use libfuzzer_sys::{fuzz_target, Corpus};
use pretty_assertions::assert_eq;
use std::str;

/// `do_fuzz` will take an arbitrary string, parse once and see if it returned a valid AST
/// then it will print and parse that AST a second time and compare the printed outputs.
/// If the second parse has a ParseError, that's a bug!!
fn do_fuzz(data: &str) -> Corpus {
	let input = data.trim_start();

	let parse_options = ParseOptions { jsx: false, type_annotations: false, ..Default::default() };
	let Ok(module) = Module::from_string(input.to_owned(), parse_options) else {
		return Corpus::Reject;
	};

	let to_string_options = ToStringOptions { trailing_semicolon: true, ..Default::default() };

	let output1 = module.to_string(&to_string_options);

	let Ok(module) = Module::from_string(output1.to_owned(), Default::default()) else {
		panic!("input: `{input}`\noutput1: `{output1}`\n\nThis parse should not error because it was just parsed above");
	};

	let output2 = module.to_string(&to_string_options);

	assert_eq!(output1, output2, "outputs different");

	Corpus::Keep
}

fuzz_target!(|data: &str| {
	do_fuzz(data);
});

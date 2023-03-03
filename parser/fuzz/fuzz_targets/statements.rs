#![no_main]

use ezno_parser::{ASTNode, Module, ParseOutput, SourceId, ToStringSettingsAndData};
use libfuzzer_sys::{fuzz_target, Corpus};
use pretty_assertions::assert_eq;
use std::str;

/// `do_fuzz` will take an arbitrary string, parse once and see if it returned a valid AST
/// then it will print and parse that AST a second time and compare the printed outputs.
/// If the second parse has a ParseError, that's a bug!!
fn do_fuzz(data: &str) -> Corpus {
	let input = data.trim_start();

	let Ok(ParseOutput(module, state)) = Module::from_string(
		input.to_owned(),
		Default::default(),
		SourceId::NULL,
		None,
		Vec::new(),
	) else {
		return Corpus::Reject
	};

	let output1 =
		module.to_string(&ToStringSettingsAndData(Default::default(), state.function_extractor));

	let ParseOutput(module, state) = Module::from_string(
		output1.to_owned(),
		Default::default(),
		SourceId::NULL,
		None,
		Vec::new(),
	)
	.expect("This parse should not error because it was just parsed above");

	let output2 =
		module.to_string(&ToStringSettingsAndData(Default::default(), state.function_extractor));

	assert_eq!(output1, output2);

	Corpus::Keep
}

fuzz_target!(|data: &str| {
	do_fuzz(data);
});

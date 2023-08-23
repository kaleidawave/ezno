#![no_main]

mod common {
	include!(concat!(env!("OUT_DIR"), "/common.rs")); // from build.rs
}

use ezno_parser::{ASTNode, Module, SourceId, ToStringOptions};
use libfuzzer_sys::{fuzz_target, Corpus};
use pretty_assertions::assert_eq;

/// do_fuzz will accept a valid JS string and attempt to parse it twice and compare
/// the rendered output of both parses.
fn do_fuzz(data: common::FuzzSource) -> Corpus {
	let input = data.source;

	let Ok(module) = Module::from_string(
		input.to_owned(),
		Default::default(),
		SourceId::NULL,
		None,
		Vec::new(),
	) else {
		return Corpus::Reject
	};

	let output1 =
		module.to_string(&ToStringOptions::default());

	let Ok(module) = Module::from_string(
		output1.to_owned(),
		Default::default(),
		SourceId::NULL,
		None,
		Vec::new(),
	) else {
		panic!("input: `{input}`\noutput1: `{output1}`\n\nThis parse should not error because it was just parsed above");
	};

	let output2 =
		module.to_string(&ToStringOptions::default());

	assert_eq!(output1, output2);

	Corpus::Keep
}

fuzz_target!(|data: common::FuzzSource| {
	do_fuzz(data);
});

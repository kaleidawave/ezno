#![no_main]

mod common {
	include!(concat!(env!("OUT_DIR"), "/common.rs")); // from build.rs
}

use boa_interner::ToInternedString;
use ezno_parser::{ASTNode, Module, ParseOutput, SourceId, ToStringSettingsAndData};
use libfuzzer_sys::{fuzz_target, Corpus};
use pretty_assertions::assert_eq;

fn do_fuzz(data: common::FuzzData) -> Corpus {
	let input = data.ast.to_interned_string(&data.interner);

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

fuzz_target!(|data: common::FuzzData| {
	do_fuzz(data);
});

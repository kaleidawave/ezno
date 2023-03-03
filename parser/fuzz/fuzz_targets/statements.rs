#![no_main]

use ezno_parser::{ASTNode, Module, ParseOutput, SourceId, ToStringSettingsAndData};
use libfuzzer_sys::{fuzz_target, Corpus};
use pretty_assertions::assert_eq;
use std::str;

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

	let Ok(ParseOutput(module, state)) = Module::from_string(
            output1.to_owned(),
            Default::default(),
            SourceId::NULL,
            None,
            Vec::new(),
        ) else {
            return Corpus::Reject
        };

	let output2 =
		module.to_string(&ToStringSettingsAndData(Default::default(), state.function_extractor));

	assert_eq!(output1, output2);

	return Corpus::Keep;
}

fuzz_target!(|data: &str| {
	do_fuzz(data);
});

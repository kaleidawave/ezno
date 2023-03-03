#![no_main]

use ezno_parser::{ASTNode, Module, ParseOutput, SourceId, ToStringSettingsAndData};
use libfuzzer_sys::fuzz_target;
use pretty_assertions::assert_eq;
use std::str;

fuzz_target!(|data: &str| {
	let input = data.trim_start();

	let Ok(ParseOutput(module, state)) = Module::from_string(
		input.to_owned(),
		Default::default(),
		SourceId::NULL,
		None,
		Vec::new(),
	) else {
		return ()
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
            return ()
        };

	let output2 =
		module.to_string(&ToStringSettingsAndData(Default::default(), state.function_extractor));

	assert_eq!(output1, output2);
});

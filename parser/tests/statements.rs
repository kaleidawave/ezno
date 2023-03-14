use ezno_parser::{ASTNode, Module, ParseOutput, SourceId, ToStringSettingsAndData};

#[test]
fn statements() {
	let input = r#"
import x from "./h.js";
if (true) {
    console.log("hi")
}
for (const x in [4, 2]) {
    switch (x) {
        case 4:
            doThing();
        case 2:
            break;
        default:
            break;
    }
}
for (let i = 0; i < 4; i++) {
    while (true) {
        break
    }
}"#
	.trim_start();

	let ParseOutput(module, state) =
		Module::from_string(input.to_owned(), Default::default(), SourceId::NULL, None, Vec::new())
			.unwrap();

	let output =
		module.to_string(&ToStringSettingsAndData(Default::default(), state.function_extractor));

	assert_eq!(output, input);
}

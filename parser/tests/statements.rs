use ezno_parser::{ASTNode, Module, ParseOutput, SourceId, ToStringSettingsAndData};

#[test]
fn statements() {
	let file = r#"import x from "./h.js";
if (true) {
    console.log("hi")
}
for (const x in [4, 2]) {
    switch (x) {
        case 4:
            doThing()
        case 2:
            break
        default:
            break
    }
}
for (let i = 0; i < 4; i++) {
    while (true) {
        break
    }
}"#;

	let ParseOutput(module, state) =
		Module::from_string(file.to_owned(), Default::default(), SourceId::NULL, None, Vec::new())
			.unwrap();

	assert_eq!(
		module.to_string(&ToStringSettingsAndData(Default::default(), state.function_extractor)),
		file
	);
}

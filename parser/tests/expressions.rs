use ezno_parser::{ASTNode, Module, SourceId};

#[test]
fn arrow_functions() {
	let input = r#"
() => expression;
param => expression;
(param) => expression;
(param1, paramN) => expression;
() => {
    statements
};
param => {
    statements
};
(param1, paramN) => {
    statements
}
    "#
	.trim_start();

	let _module =
		Module::from_string(input.to_owned(), Default::default(), SourceId::NULL, None).unwrap();

	// let output = module.to_string(&ToStringOptions::typescript());
	// assert_eq!(output, input);
}

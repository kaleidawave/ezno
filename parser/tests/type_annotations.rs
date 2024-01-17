use ezno_parser::{ASTNode, Module, ParseOptions, ToStringOptions};

#[test]
fn random_statements() {
	let input = r#"
interface X {}
type Y = 2
function g(b: X): Y {
    return 2
}
"#
	.trim_start()
	.replace("    ", "\t");

	let module = Module::from_string(input.clone(), Default::default()).unwrap();
	let output = module.to_string(&ToStringOptions::typescript());

	assert_eq!(output, input.clone());

	assert!(Module::from_string(
		input,
		ParseOptions { type_annotations: false, ..Default::default() }
	)
	.is_err());
}

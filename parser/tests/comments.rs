use ezno_parser::{ASTNode, Module};
// use pretty_assertions::assert_eq;

#[test]
fn random_comments() {
	let input = r#"
    const [,,/* hi */] = [];
"#
	.trim_start()
	.replace("    ", "\t");

	let _module = Module::from_string(input.clone(), Default::default()).unwrap();
	// let output = module.to_string(&ToStringOptions::typescript());
	// assert_eq!(output, input);
}

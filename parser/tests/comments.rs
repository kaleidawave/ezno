use ezno_parser::{ASTNode, Module};

#[test]
fn random_comments() {
	let input = r"
    const [,,/* hi */] = [];
"
	.trim_start()
	.replace("    ", "\t");

	let _module = Module::from_string(input.clone()).unwrap();
	// let output = module.to_string(&ToStringOptions::typescript());
	// pretty_assertions::assert_eq!(output, input);
}

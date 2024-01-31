use ezno_parser::{ASTNode, Module, ParseOptions, ToStringOptions};
use pretty_assertions::assert_eq;

#[test]
fn statements() {
	let input = r#"
interface X {}
interface Y extends number, Z2<T> {}
type Z = 2
type Z2<T> = T
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

#[test]
fn variable_parameter_and_return() {
	let input = r#"
const x: number = 2;
function y(a: string): string {
	return a
}"#
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

#[test]
fn expression_level_expressions() {
	// 👎👎👎
	let input = r#"
(a satisfies number);
(b as number)"#
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

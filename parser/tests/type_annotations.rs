use ezno_parser::{ASTNode, Module, ParseOptions, ToStringOptions};
use pretty_assertions::assert_eq;

#[test]
fn statements() {
	let input = r#"
interface X {}
interface Y extends number, Z2<T> {}
type Z = 2;
type Z2<T> = T
"#
	.trim()
	.to_owned();

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

// `satisfies` is actually not under `feature="full-typescript"`
#[test]
#[cfg(feature = "full-typescript")]
fn expression_level_expressions() {
	// 👎👎👎
	let input = r#"
(a satisfies number);
(b as number);
({ 1: 2 } as const);
(a! + 2)
"#
	.trim()
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
#[cfg(feature = "full-typescript")]
fn function_and_method_overloading() {
	// use generics and redesign your bad APIs sheeple
	let input = r#"
function makeDate(timestamp: number): Date
function makeDate(m: number, d: number, y: number): Date
function makeDate(mOrTimestamp: number, d?: number, y?: number): Date {}
class X {
	constructor(a: string)
	constructor(a: number) {}
	makeDate(timestamp: number): Date
	makeDate(m: number, d: number, y: number): Date
	makeDate(mOrTimestamp: number, d?: number, y?: number): Date {}
}
"#
	.trim()
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
fn type_definition_module() {
	let input = r#"
export default function (a: string): string
"#
	.trim()
	.replace("    ", "\t");

	let parse_options = ParseOptions { type_definition_module: true, ..Default::default() };

	let module = Module::from_string(input.clone(), parse_options).unwrap();
	let output = module.to_string(&ToStringOptions::typescript());

	assert_eq!(output, input.clone());
}

#[test]
fn mapped_type() {
	let input = r#"
type Record<T extends string, V> = { [P in T]: V }
"#
	.trim()
	.replace("    ", "\t");

	let parse_options = ParseOptions { type_definition_module: true, ..Default::default() };

	let module = Module::from_string(input.clone(), parse_options).unwrap();
	let output = module.to_string(&ToStringOptions::typescript());

	assert_eq!(output, input.clone());
}

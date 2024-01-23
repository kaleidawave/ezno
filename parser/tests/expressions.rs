use ezno_parser::{ASTNode, Module};
use pretty_assertions::assert_eq;

#[test]
fn arrow_functions() {
	let input = r"
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
    "
	.trim_start();

	let _module = Module::from_string(input.to_owned(), Default::default()).unwrap();

	// let output = module.to_string(&ToStringOptions::typescript());
	// assert_eq!(output, input);
}

#[test]
fn function_calls() {
	let input = r"
x(4, 5);
y.t(2, 3);
y.t<4, 2>(3);
y.t<4, Array<5>>(3);
a(y<2>(4));
a.a?.(y<2>(4));
a.a(...expr, y)
    "
	.trim();

	let module = Module::from_string(input.to_owned(), Default::default()).unwrap();

	eprintln!("Module: {module:#?}");

	let output = module.to_string(&ezno_parser::ToStringOptions::typescript());
	assert_eq!(output, input);
}

#[test]
fn objects() {
	let input = r"
const a = { a: 5 };
const b = { ...b, a: 5, ...c, d: 4 };
const c = { async e() {
	return 2
} }
    "
	.trim();

	let module = Module::from_string(input.to_owned(), Default::default()).unwrap();

	eprintln!("Module: {module:#?}");

	let output = module.to_string(&ezno_parser::ToStringOptions::typescript());
	assert_eq!(output, input);
}

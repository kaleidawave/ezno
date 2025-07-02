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
a.a(...expr, y);
a.a(...expr, y, /* something */)
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

#[test]
fn arrays() {
	let input = r"
const a = [{ a: 5 }];
const b = [...x, 7];
const c = [/* hi */, 6]
    "
	.trim();

	let module = Module::from_string(input.to_owned(), Default::default()).unwrap();

	eprintln!("Module: {module:#?}");

	let output = module.to_string(&ezno_parser::ToStringOptions::typescript());
	assert_eq!(output, input);
}

#[test]
fn regular_expressions() {
	let input = r"
const a = /something/;
const b = /with global flag/g;
const c = /escaped \//;
const d = /in a set[=/]/
    "
	.trim();

	let module = Module::from_string(input.to_owned(), Default::default()).unwrap();

	eprintln!("Module: {module:#?}");

	let output = module.to_string(&ezno_parser::ToStringOptions::typescript());
	assert_eq!(output, input);
}

#[test]
fn import_expression() {
	let input = r#"
const a = import("file");
const b = import("some" + "expression");
const c = import.meta;
const d = import.meta.env;
const helperPath = import.meta.resolve("./lib/helper.js")
    "#
	.trim();

	let module = Module::from_string(input.to_owned(), Default::default()).unwrap();

	eprintln!("Module: {module:#?}");

	let output = module.to_string(&ezno_parser::ToStringOptions::typescript());
	assert_eq!(output, input);
}

#[cfg(feature = "extras")]
#[test]
fn jsx() {
	// note the parser supports self closing tags with `<img>` and HTML comments
	let input = r#"
function Component(item) {
	return <div>
		<h1 class="heading">{item.heading}</h1>
		<img src={item.image}>
		<!-- Some comment -->
		<p data-something>
			Something {item.content}
		</p>
		{/* hi */}
		<style>
			h1 {
				color: red;
			}
		</style>
		<button disabled>One line</button>
	</div>
}
    "#
	.trim();

	let module = Module::from_string(input.to_owned(), Default::default()).unwrap();

	eprintln!("Module: {module:?}");

	let output = module.to_string(&ezno_parser::ToStringOptions::typescript());

	eprintln!("{input}");
	eprintln!("{output}");

	assert_eq!(output, input);
}

#[test]
fn regex_and_leading_decimal() {
	let input = r"
for (const x in 0.4) {}
for await (const [a] of 0.2) {}
for (const result of /thing/) {}
"
	.trim();

	let module = Module::from_string(input.to_owned(), Default::default()).unwrap();

	eprintln!("Module: {module:#?}");

	let output = module.to_string(&ezno_parser::ToStringOptions::typescript());
	assert_eq!(output, input);
}

#[test]
fn class_and_object_divides() {
	let input = r"
const b = class Number {} / 2
"
	.trim();

	let module = Module::from_string(input.to_owned(), Default::default()).unwrap();

	eprintln!("Module: {module:#?}");

	let output = module.to_string(&ezno_parser::ToStringOptions::typescript());
	assert_eq!(output, input);
}

#[test]
fn arrow_functions_and_type_annotation() {
	let input = r#"
const a1 = () => 4;
const a2 = (x: Type<1>) => 5;
const a3 = ([a, b]: [1, 2]) => 6;
const a4 = (l: () => "hiya") => 7;
const a5 = (a: S<1, 2>, d: { [a: "a" | "b"]: number }) => 2;
const a6 = (strings: { [a: string]: number }, record: Record<string, number>, d: { [a: "a" | "b"]: number }) => 2
"#
	.trim();

	let module = Module::from_string(input.to_owned(), Default::default()).unwrap();

	eprintln!("Module: {module:#?}");

	let output = module.to_string(&ezno_parser::ToStringOptions::typescript());
	assert_eq!(output, input);
}

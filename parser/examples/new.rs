fn main() {
	use ezno_parser::ASTNode;

	// new::Lexer

	let s = r#"'Hello World' + class X { } + (a === 5) + something * 6 "#;

	let e = ezno_parser::ast::Expression::from_string(s.into(), Default::default());

	assert!(e.is_ok());
	// eprintln!("{:#?}", e);

	let s = r#"function bunc(a, param): number {
		if (a) {
			return (a: string) => {};
		}
		return 2
	}"#;

	let e = ezno_parser::ast::Expression::from_string(s.into(), Default::default());

	eprintln!("{:#?}", e);

	// let s = "";
}

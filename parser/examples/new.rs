fn main() {
	use ezno_parser::ASTNode;
	let s = r#"'Hello World' + class X { } + (a === 5) + something * 6 "#;

	let e = ezno_parser::ast::Expression::from_string(s.into(), Default::default());

	eprintln!("{:#?}", e);
}

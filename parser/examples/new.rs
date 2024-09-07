fn main() {
	use ezno_parser::ASTNode;
	let s = r#"'Hello World' + (6.2 - .4) + (a === 5) + something * 6 "#;

	let e = ezno_parser::ast::Expression::from_string(s.into(), Default::default());

	eprintln!("{:#?}", e);
}

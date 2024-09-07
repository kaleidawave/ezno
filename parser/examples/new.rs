fn main() {
	let s = r#"'Hello World' + (6.2 - .4) + (a === 5) + something * 6 "#;
	let mut lexer = ezno_parser::new::Lexer::new(s, None, Default::default());

	let e = ezno_parser::ast::Expression::from_reader2(&mut lexer);

	eprintln!("{:#?}", e);
}

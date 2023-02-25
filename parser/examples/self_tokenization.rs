#[cfg(feature = "self-rust-tokenize")]
fn main() {
	use ezno_parser::{ASTNode, Expression, ParseOutput, SourceId};
	use self_rust_tokenize::SelfRustTokenize;

	let ParseOutput(expression, _state) = Expression::from_string(
		"2 + 3".to_string(),
		Default::default(),
		SourceId::NULL,
		None,
		Default::default(),
	)
	.unwrap();

	let tokens = SelfRustTokenize::to_tokens(&expression);

	println!("{}", tokens);
}

#[cfg(not(feature = "self-rust-tokenize"))]
fn main() {
	panic!("Enable feature 'self-rust-tokenize'");
}

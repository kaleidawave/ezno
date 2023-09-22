#[cfg(feature = "self-rust-tokenize")]
fn main() {
	use ezno_parser::{ASTNode, Expression, SourceId};
	use self_rust_tokenize::SelfRustTokenize;

	let expression =
		Expression::from_string("2 + 3".to_string(), Default::default(), SourceId::NULL, None)
			.unwrap();

	let tokens = SelfRustTokenize::to_tokens(&expression);

	println!("{}", tokens);
}

#[cfg(not(feature = "self-rust-tokenize"))]
fn main() {
	panic!("Enable feature 'self-rust-tokenize'");
}

use ezno_parser::{ASTNode, Expression, SourceId, TypeReference};

fn main() {
	let reference = TypeReference::from_string(
		"Pair<Nested<Object<2>>, Array<number>>".into(),
		Default::default(),
		SourceId::NULL,
		None,
		Default::default(),
	);

	println!("{:#?}", reference);

	let expression = Expression::from_string(
		"(x << 3, x >> 2, y<Array<string>>(2), x < 7, x< 7)".into(),
		Default::default(),
		SourceId::NULL,
		None,
		Default::default(),
	);

	println!("{:#?}", expression);
}

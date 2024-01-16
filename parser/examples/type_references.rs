use ezno_parser::{ASTNode, Expression, TypeAnnotation};

fn main() {
	let reference = TypeAnnotation::from_string(
		"Pair<Nested<Object<2>>, Array<number>>".into(),
		Default::default(),
	);

	println!("{reference:#?}");

	let expression = Expression::from_string(
		"(x << 3, x >> 2, y<Array<string>>(2), x < 7, x< 7)".into(),
		Default::default(),
	);

	println!("{expression:#?}");
}

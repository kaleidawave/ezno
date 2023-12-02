use ezno_parser::{ASTNode, Expression, SourceId};

fn main() {
	let expressions = [
		"4 + 2 * 5",
		"4 * 2 + 5",
		"4 * 2 * 5",
		"console.log(4 * 2, t ? true : `Hi`) == 2 && 4 == 2",
	];
	for expression in expressions {
		let expression = Expression::from_string(
			expression.to_owned(),
			Default::default(),
			SourceId::NULL,
			None,
		);
		println!("{expression:#?}");
	}
}

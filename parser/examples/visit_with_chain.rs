use ezno_parser::{
	ASTNode, Expression,
	visiting::{Annex, Chain, VisitOptions, Visitable, Visitor, Visitors},
};

struct ShowChain;

impl Visitor<Expression, ()> for ShowChain {
	fn visit(&mut self, item: &Expression, _data: &mut (), chain: &Chain) {
		if matches!(item, Expression::VariableReference(name, _) if name == "chain") {
			eprintln!("{chain:#?}");
		}
	}
}

fn main() {
	let expr = Expression::from_string("3 && (4 && chain) && 2 == chain".to_owned()).unwrap();

	expr.visit(
		&mut Visitors { expression_visitors: vec![Box::new(ShowChain)], ..Default::default() },
		&mut (),
		&VisitOptions::default(),
		&mut Annex::new(&mut Chain::new()),
	);
}

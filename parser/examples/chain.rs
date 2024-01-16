use ezno_parser::{
	visiting::{Annex, Chain, VisitOptions, Visitable, Visitor, Visitors},
	ASTNode, Expression,
};

fn parse<T: ASTNode>(s: &str) -> T {
	T::from_string(s.to_owned(), Default::default()).unwrap()
}

struct ShowChain;

impl Visitor<Expression, ()> for ShowChain {
	fn visit(&mut self, item: &Expression, _data: &mut (), chain: &Chain) {
		if matches!(item, Expression::VariableReference(name, _) if name == "chain") {
			eprintln!("{chain:#?}");
		}
	}
}

fn main() {
	let expr = parse::<Expression>("3 && (4 && chain) && 2 == chain");

	expr.visit(
		&mut Visitors { expression_visitors: vec![Box::new(ShowChain)], ..Default::default() },
		&mut (),
		&VisitOptions::default(),
		&mut Annex::new(&mut Chain::new()),
	);
}

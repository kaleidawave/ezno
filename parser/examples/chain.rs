use ezno_parser::{
	extractor::ExtractedFunctions, ASTNode, Chain, Expression, ParseOutput, SourceId,
	VisitSettings, Visitable, Visitor, Visitors,
};
use temporary_annex::Annex;

fn parse<T: ASTNode>(s: &str) -> ParseOutput<T> {
	T::from_string(s.to_owned(), Default::default(), SourceId::NULL, None, Vec::new()).unwrap()
}

struct ShowChain;

impl Visitor<Expression, ()> for ShowChain {
	fn visit(
		&mut self,
		item: &Expression,
		_data: &mut (),
		_: &mut ExtractedFunctions,
		chain: &Chain,
	) {
		if matches!(item, Expression::VariableReference(name, _, _) if name == "chain") {
			eprintln!("{:#?}", chain);
		}
	}
}

fn main() {
	let ParseOutput(expr, mut state) = parse::<Expression>("3 && (4 && chain) && 2 == chain");

	expr.visit(
		&mut Visitors { expression_visitors: vec![Box::new(ShowChain)], ..Default::default() },
		&mut (),
		&VisitSettings::default(),
		&mut state.function_extractor,
		&mut Annex::new(&mut Chain::new()),
	);
}

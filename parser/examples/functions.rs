use ezno_parser::{declarations::StatementFunction, ASTNode, SourceId};

fn main() {
	let statement_function = "function x<T>(a: T) {}";
	let statement_function = StatementFunction::from_string(
		statement_function.to_owned(),
		Default::default(),
		SourceId::NULL,
		None,
		Vec::new(),
	);
	println!("{:#?}", statement_function);
}

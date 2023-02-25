use ezno_parser::{ASTNode, ParseOutput, SourceId, Statement, ToStringSettingsAndData};

fn main() {
	let ParseOutput(stmt, state) = Statement::from_string(
		"const x = () => 2".to_owned(),
		Default::default(),
		SourceId::NULL,
		None,
		Vec::new(),
	)
	.unwrap();

	println!("{:#?}\n{:#?}", stmt, state);

	let settings = ToStringSettingsAndData(Default::default(), state.function_extractor);
	println!("{}", stmt.to_string(&settings));
}

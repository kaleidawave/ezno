use ezno_parser::{ASTNode, SourceId, Statement};

fn main() {
	let statement =
		Statement::from_string("const x = 4".to_owned(), Default::default(), SourceId::NULL, None);
	println!("{statement:#?}");
}

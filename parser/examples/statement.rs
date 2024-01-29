use ezno_parser::{ASTNode, Statement};

fn main() {
	let statement = Statement::from_string("const x = 4".to_owned(), Default::default());

	println!("{statement:#?}");
}

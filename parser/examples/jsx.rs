use ezno_parser::{ASTNode, JSXRoot, SourceId, ToStringSettings};

fn main() {
	let source = "<MySiteLayout> <p>My page content, wrapped in a layout!</p> </MySiteLayout>";
	let result = JSXRoot::from_string(
		source.to_owned(),
		Default::default(),
		SourceId::NULL,
		None,
		Vec::new(),
	)
	.unwrap();

	println!("{}", result.to_string(&ToStringSettings::default()));
}

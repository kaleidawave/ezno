use ezno_parser::{ASTNode, JSXRoot, ToStringOptions};

fn main() {
	let source = "<MySiteLayout> <p>My page content, wrapped in a layout!</p> </MySiteLayout>";
	let result = JSXRoot::from_string(source.to_owned(), Default::default()).unwrap();

	println!("{}", result.to_string(&ToStringOptions::default()));
}

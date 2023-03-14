use ezno_ast_generator::stmt;
use parser::ASTNode;

fn main() {
	let content = "World!";
	let my_stmt = stmt!(let my_element = <h1>Hello {#content}</h1>);
	println!("{}", my_stmt.to_string(&Default::default()));
}

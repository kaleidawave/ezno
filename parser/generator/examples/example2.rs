use ezno_ast_generator::{expr, stmt};

fn main() {
	let x = expr!(x = 4);
	println!("{:#?}", x);

	let number = 4.2f64.sin();
	let y = stmt!(let y = #number;);
	println!("{:#?}", y);

	let name = "test";
	let z = stmt!(let #name = 4;);
	println!("{:#?}", z);
}

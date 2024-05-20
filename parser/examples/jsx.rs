use ezno_parser::{ASTNode, JSXRoot, ToStringOptions};

fn main() {
	let source = "<MySiteLayout> <p>My page content, wrapped in a layout!</p> </MySiteLayout>";
	let result = JSXRoot::from_string(source.to_owned(), Default::default()).unwrap();

	println!("{}", result.to_string(&ToStringOptions::default()));

	// It also handles HTML (once prefix remove)
	let source = r#"<!DOCTYPE html>
	<html lang="en">
	<head>
		<meta charset="UTF-8">
		<meta name="viewport" content="width=device-width, initial-scale=1.0">
		<title>Document</title>
	</head>
	<body>
		<h1>Hello World</h1>
	</body>
	</html>"#;

	let m = JSXRoot::from_string(
		source.strip_prefix("<!DOCTYPE html>").unwrap().to_owned(),
		Default::default(),
	)
	.unwrap();
	println!("{m:#?}");
	println!("{}", m.to_string(&ToStringOptions::default()));
}

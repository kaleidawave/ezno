use ezno_parser::extensions::jsx::JSXRoot;
use ezno_parser::{ASTNode, ToStringOptions};

fn main() {
	{
		let source = "<h1 title=\"Example text\">Hello ${name}p</h1>";
		let result = JSXRoot::from_string(source.to_owned(), Default::default()).unwrap();
		let out = result.to_string(&ToStringOptions::default());
		println!("{out}");
	}

	{
		let source = "<MySiteLayout> <p>My page content, wrapped in a layout!</p> </MySiteLayout>";
		let result = JSXRoot::from_string(source.to_owned(), Default::default()).unwrap();
		let out = result.to_string(&ToStringOptions::default());
		println!("{out}");
	}

	{
		// It also handles HTML (once prefix is removed)
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

		let source = source.strip_prefix("<!DOCTYPE html>").unwrap().to_owned();
		let result = JSXRoot::from_string(source, Default::default()).unwrap();
		println!("{result:#?}");
		let out = result.to_string(&ToStringOptions::default());
		println!("{out}");
	}
}

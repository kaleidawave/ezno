use ezno_parser::{ASTNode, Module, ParseOptions, ToStringOptions};

fn main() {
	let input = r#"const x = something ? "hello world something x" : "another thing quite long, lolmao. another thing quite long, lolmao";
const y = "hello world something x", z = "another thing quite long, lolmao. another thing quite long, lolmao";
    "#;

	let module = Module::from_string(
		input.to_owned(),
		ParseOptions { retain_blank_lines: true, ..Default::default() },
	)
	.unwrap();

	let output = module.to_string(&ToStringOptions { max_line_length: 60, ..Default::default() });

	eprintln!("Input:\n{input}\nOutput:\n{output}");
}

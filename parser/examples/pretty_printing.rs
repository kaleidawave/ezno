use ezno_parser::{ASTNode, Module, ParseOptions, ToStringOptions};

fn main() {
	let input = r#"const x = something ? "hello world something x" : "another thing quite long, lolmao. another thing quite long, lolmao";
const y = "hello world something x", z = "another thing quite long, lolmao. another thing quite long, lolmao";

function x(a: { something: string, another: number, third: "yes" }, b: Array<{ everything: any }>) {

}

something({ here: 2 }).property.get_lines_by_length(2323, 2323).then(console.log).afterwards({ do_something: ["with", 2] })

const normal = "simple".length;
    "#;

	let module = Module::from_string(
		input.to_owned(),
		ParseOptions { retain_blank_lines: true, ..Default::default() },
	)
	.unwrap();

	let output = module.to_string(&ToStringOptions {
		include_type_annotations: true,
		max_line_length: 60,
		..Default::default()
	});

	eprintln!("Input:\n{input}\nOutput:\n{output}");
}

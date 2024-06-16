use ezno_parser::{ASTNode, Module, ParseOptions, ToStringOptions};

fn main() {
	let input = r#"const x = something ? "hello world something x" : "another thing quite long, lolmao. another thing quite long, lolmao";
const y = "hello world something x", z = "another thing quite long, lolmao. another thing quite long, lolmao";

function x(a: { something: string, another: number, third: "yes" }, b: Array<{ everything: any }>) {
	something({ here: 2 }).property.get_lines_by_length(2323, 2323).then(console.log).afterwards({ do_something: ["with", 2] })
	
	const normal = "simple".length;
	
	for (let i = 0; i < something.anotherThing("large string here"); i += calculateFromFunction()) {
		console.log("here 1")
	
	}

	for (let i = 0; i < 5; i += 1) {
		console.log("here 2")
	}

	const x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 100, 5000, 1000, 122200, 100];
}
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

use ezno_parser::{
	ASTNode, Module,
	options::{Features, ParseOptions, ToStringOptions},
};

fn main() {
	let input = r#"
import { something, aaa, another_thing, bbb } from "./x.js";
	
const x = something ? "hello world something x" : "another thing quite long, lolmao. another thing quite long, lolmao";
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

	let parse_options = ParseOptions {
		features: Features { retain_blank_lines: true, ..Default::default() },
		..Default::default()
	};
	let (module, _) = Module::from_string_with_options(input.to_owned(), parse_options).unwrap();

	let output = module.to_string(&ToStringOptions {
		include_type_annotations: true,
		max_line_length: 60,
		..Default::default()
	});

	eprintln!("Input:\n{input}\nOutput:\n{output}");
}

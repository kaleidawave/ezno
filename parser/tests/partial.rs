use ezno_parser::options::{Features, ParseOptions, ToStringOptions};
use ezno_parser::{ASTNode, Module};
use pretty_assertions::assert_eq;

fn partial_options() -> ParseOptions {
	ParseOptions {
		features: Features { partial_syntax: true, ..Features::default() },
		..ParseOptions::default()
	}
}

#[test]
fn declarations() {
	let input = r"
const x = ;
const y = 
const z = 2
"
	.trim_start()
	.replace("    ", "\t");

	let (module, _) = Module::from_string_with_options(input.clone(), partial_options()).unwrap();
	let _output = module
		.to_string(&ToStringOptions { expect_markers: true, ..ToStringOptions::typescript() });

	// also assert invalid without partial
	assert!(Module::from_string_with_options(input.clone(), ParseOptions::default()).is_err());

	// TODO difference in semi colons
	// assert_eq!(output, input);
}

#[test]
fn in_statements() {
	let input = r"
if () {
	return 2
}"
	.trim_start()
	.replace("    ", "\t");

	let (module, _) = Module::from_string_with_options(input.clone(), partial_options()).unwrap();
	let output = module
		.to_string(&ToStringOptions { expect_markers: true, ..ToStringOptions::typescript() });

	// also assert invalid without partial
	assert!(Module::from_string(input.clone()).is_err());

	assert_eq!(output, input);
}

#[test]
fn type_annotations() {
	// Can't do return type annotation because conflicts with object type
	let input = r"
const x:  = 5;
function y(c: ) {
	return 2
}"
	.trim_start()
	.replace("    ", "\t");

	let (module, _) = Module::from_string_with_options(input.clone(), partial_options()).unwrap();

	let output = module
		.to_string(&ToStringOptions { expect_markers: true, ..ToStringOptions::typescript() });

	// also assert invalid without partial
	assert!(Module::from_string(input.clone()).is_err());

	assert_eq!(output, input);
}

#[test]
fn property_access() {
	let input = r"console.log(x., 2)".trim_start().replace("    ", "\t");

	let (module, _) = Module::from_string_with_options(input.clone(), partial_options()).unwrap();

	let output = module
		.to_string(&ToStringOptions { expect_markers: true, ..ToStringOptions::typescript() });

	// also assert invalid without partial
	assert!(Module::from_string(input.clone()).is_err());

	assert_eq!(output, input);
}

#[test]
fn invalid_syntax() {
	let sources = [("", true), ("][", false), ("{}}", false), ("))", false)];

	for (source, is_okay) in sources {
		let result = Module::from_string_with_options(source.to_owned(), partial_options());
		if is_okay {
			assert!(result.is_ok());
		} else {
			assert!(result.is_err());
		}
	}
}

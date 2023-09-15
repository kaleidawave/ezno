use ezno_parser::{ASTNode, Module, SourceId, ToStringOptions};
use pretty_assertions::assert_eq;

#[test]
fn statements() {
	let input = r#"
import x from "./h.js";
if (true) {
    console.log("hi")
}
for (const x in [4, 2]) {
    switch (x) {
        case 4:
            doThing();
        case 2:
            break;
        default:
            break;
    }
}
for (let i = 0; i < 4; i++) {
    while (true) {
        break
    }
}
try {
    doThing()
} catch (e) {
    console.error(e)
}
interface X {
    a: string
}"#
	.trim_start();

	let module =
		Module::from_string(input.to_owned(), Default::default(), SourceId::NULL, None).unwrap();

	let output = module.to_string(&ToStringOptions::typescript());

	assert_eq!(output, input);
}

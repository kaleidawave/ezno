use ezno_parser::{ASTNode, Module, SourceId, ToStringOptions};
use pretty_assertions::assert_eq;

#[test]
fn random_statements() {
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

#[test]
fn try_catch() {
	let input = r#"
try {
    console.log("ordinary usage")
} catch (e) {
    console.error(e)
}
try {
    console.log("no exception var")
} catch {
    console.error("error")
}
try {
    console.log("destructured catch")
} catch ({ message }) {
    console.error(message)
}
try {
    console.log("catch with type annotation")
} catch (error: unknown) {
    console.error(error)
}
try {
    console.log("finally clause")
} catch (e) {
    console.error(e)
} finally {
    console.log("done")
}
try {
    console.log("omitted catch clause")
} finally {
    console.log("done")
}
try {
    console.log("nesting");
    try {
        doThing()
    } catch (e) {
        console.error(e)
    }
} catch (e) {
    try {
        doThing()
    } catch (e) {
        console.error(e)
    }
} finally {
    try {
        doThing()
    } catch (e) {
        console.error(e)
    }
}"#
	.trim_start();

	let module =
		Module::from_string(input.to_owned(), Default::default(), SourceId::NULL, None).unwrap();
	let output = module.to_string(&Default::default());
	assert_eq!(output, input);
}

#[test]
fn imports() {
	// Taken from MDN
	let input = r#"
import defaultExport from "module-name";
import * as name from "module-name";
import { export1 } from "module-name";
import { export1 as alias1 } from "module-name";
import { default as alias } from "module-name";
import { export1, export2 } from "module-name";
import { export1, export2 as alias2, /* … */ } from "module-name";
import { "string name" as alias } from "module-name";
import defaultExport, { export1, /* … */ } from "module-name";
import defaultExport, * as name from "module-name";
import "module-name""#
		.trim_start();

	let module =
		Module::from_string(input.to_owned(), Default::default(), SourceId::NULL, None).unwrap();
	let output = module.to_string(&ToStringOptions::typescript());
	assert_eq!(output, input);

	// Additional ones
	let input = r#"
import type defaultExport from "module-name";
import { a, } from "module-name"
"#
	.trim_start();

	Module::from_string(input.to_owned(), Default::default(), SourceId::NULL, None).unwrap();
}

#[test]
fn exports() {
	// Taken from MDN
	let input = r#"
// Exporting declarations
export let name1, name2 /*, … */;
export const name1 = 1, name2 = 2 /*, … */;
export function functionName() { 
    /* … */ 
}
export class ClassName { 
    /* … */
}
export function* generatorFunctionName() { 
    /* … */ 
}
export const { name1, name2: bar } = o;
export const [ name1, name2 ] = array;

// Export list
export { name1, /* …, */ nameN };
export { variable1 as name1, variable2 as name2, /* …, */ nameN };
export { variable1 as "string name" };
export { name1 as default /*, … */ };

// Default exports
export default expression;
export default function functionName() { 
    /* … */
}
export default class ClassName { 
    /* … */ 
}
export default function* generatorFunctionName() { 
    /* … */ 
}
export default function () { 
    /* … */ 
}
export default class { 
    /* … */ 
}
export default function* () { 
    /* … */ 
}

export interface X { property: number }

// Aggregating modules
export * from "module-name";
export * as name1 from "module-name";
export { name1, /* …, */ nameN } from "module-name";
export { import1 as name1, import2 as name2, /* …, */ nameN } from "module-name";
export { default, /* …, */ } from "module-name";
export { default as name1 } from "module-name""#
		.trim_start();

	let _module =
		Module::from_string(input.to_owned(), Default::default(), SourceId::NULL, None).unwrap();

	// TODO
	// let output = module.to_string(&ToStringOptions::typescript());
	// assert_eq!(output, input);
}

#[cfg(feature = "extras")]
#[test]
fn reversed_imports() {
	let input = r#"
from "module-name" import defaultExport;
from "module-name" import * as name;
from "module-name" import { export1 };
from "module-name" import { export1, export2 };
from "module-name" import defaultExport, { export1, /* … */ };
from "module-name" import defaultExport, * as name;
    "#
	.trim();

	let module =
		Module::from_string(input.to_owned(), Default::default(), SourceId::NULL, None).unwrap();

	eprintln!("Module: {:#?}", module);

	// let output = module.to_string(&ezno_parser::ToStringOptions::typescript());
	// assert_eq!(output, input);
}

#[cfg(feature = "extras")]
#[test]
fn function_custom_headers() {
	let input = r#"
function a() {}
generator function a() {}
generator server function a() {}
generator server function a() {}
async server function a() {}
module function a() {}
    "#
	.trim();

	let module =
		Module::from_string(input.to_owned(), Default::default(), SourceId::NULL, None).unwrap();

	eprintln!("Module: {:#?}", module);

	// let output = module.to_string(&ezno_parser::ToStringOptions::typescript());
	// assert_eq!(output, input);
}

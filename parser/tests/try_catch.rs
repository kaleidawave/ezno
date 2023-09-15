use ezno_parser::{ASTNode, Module, SourceId};
use pretty_assertions::assert_eq;

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

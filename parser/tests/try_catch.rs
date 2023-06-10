use ezno_parser::{ASTNode, Module, SourceId};

#[test]
fn try_catch() {
	let input = r#"
// Ordinary/typical usage
try {
    doThing()
} catch (e) {
    console.error(e)
}

// No catch variable
try {
    doThing()
} catch {
    doOtherThing()
}


// Destructured catch
try {
    doThing()
} catch ({ message }) {
    console.error(message)
}

// Type annotation
try {
    doThing()
} catch (error: unknown) {
    console.error(error)
}

// Finally clause
try {
    doThing()
} catch (e) {
    console.error(e)
} finally {
    console.log("done")
}

// Finally without catch
try {
    doThing()
} finally {
    console.log("done")
}

// Nesting
try {
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
}
"#
	.trim_start();

	let module =
		Module::from_string(input.to_owned(), Default::default(), SourceId::NULL, None, Vec::new())
			.unwrap();

	let output = module.to_string(&Default::default());

	assert_eq!(output, input);
}

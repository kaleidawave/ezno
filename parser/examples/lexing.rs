use std::thread::spawn;

use ezno_parser::{lex_source, EmptyCursorId};
use tokenizer_lib::{ParallelTokenQueue, TokenReader};

fn main() {
	let script = "x << 2; t<Array<number>>; x >>>= 4; x++; class";

	lex_and_print_tokens(script, None);
	println!();
	lex_and_print_tokens(
		"let = Math.()",
		Some(vec![
			(3, EmptyCursorId::new(0)),
			(11, EmptyCursorId::new(1)),
			(12, EmptyCursorId::new(2)),
		]),
	);
	println!();
	lex_and_print_tokens(
		"let my_element = <h1>Hello</ h1>",
		Some(vec![(31, EmptyCursorId::new(0))]),
	);
}

fn lex_and_print_tokens(script: &'static str, cursors: Option<Vec<(usize, EmptyCursorId)>>) {
	let (mut sender, mut receiver) = ParallelTokenQueue::new();

	let thread = spawn(move || {
		lex_source(
			script,
			&mut sender,
			&Default::default(),
			None,
			None,
			cursors.unwrap_or_default(),
		)
		.unwrap();
	});

	while let Some(token) = receiver.next() {
		println!("{:?}", token);
	}

	thread.join().unwrap();
}

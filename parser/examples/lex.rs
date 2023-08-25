use std::thread::spawn;

use ezno_parser::{lex_script, EmptyCursorId};
use tokenizer_lib::{ParallelTokenQueue, TokenReader};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let path = std::env::args().skip(1).next().ok_or("expected argument")?;
	let content = std::fs::read_to_string(path)?;
	lex_and_print_tokens(content, None);
	Ok(())
}

fn lex_and_print_tokens(script: String, cursors: Option<Vec<(usize, EmptyCursorId)>>) {
	let (mut sender, mut receiver) = ParallelTokenQueue::new();

	let thread = spawn(move || {
		lex_script(
			&script,
			&mut sender,
			&Default::default(),
			None,
			None,
			cursors.unwrap_or_default(),
		)
		.unwrap();
	});
	// let mut count = 0;
	while let Some(token) = receiver.next() {
		// count += 1;
		println!("{:?}", token);
	}
	// println!("{count} tokens");

	thread.join().unwrap();
}

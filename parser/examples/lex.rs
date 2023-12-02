use std::thread::spawn;

use ezno_parser::{lex_script, EmptyCursorId};
use tokenizer_lib::{sized_tokens::SizedToken, ParallelTokenQueue, Token, TokenReader};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let path = std::env::args().nth(1).ok_or("expected argument")?;
	let content = std::fs::read_to_string(path)?;
	lex_and_print_tokens(content, None);
	Ok(())
}

fn lex_and_print_tokens(script: String, cursors: Option<Vec<(usize, EmptyCursorId)>>) {
	let (mut sender, mut receiver) = ParallelTokenQueue::new();

	let other = script.clone();
	let thread = spawn(move || {
		lex_script(&script, &mut sender, &Default::default(), None, cursors.unwrap_or_default())
	});
	// let mut count = 0;
	println!("token | start (1 based) | length");
	while let Some(Token(token, start)) = receiver.next() {
		// count += 1;
		let length = token.length();
		println!(
			"{:?} {} {} {:?}",
			token,
			start.0 + 1u32,
			length,
			other.get((start.0 as usize)..(start.0 as usize + length as usize))
		);
	}
	// println!("{count} tokens");

	match thread.join().unwrap() {
		Ok(()) => {}
		Err((lexer_err, _)) => {
			eprintln!("lexer error: {lexer_err:?}");
		}
	}
}

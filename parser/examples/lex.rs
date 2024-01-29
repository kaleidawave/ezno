use std::thread::spawn;

use ezno_parser::lex_script;
use tokenizer_lib::{sized_tokens::SizedToken, ParallelTokenQueue, Token, TokenReader};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let path = std::env::args().nth(1).ok_or("expected argument")?;
	let content = std::fs::read_to_string(path)?;
	lex_and_print_tokens(content);
	Ok(())
}

fn lex_and_print_tokens(script: String) {
	let (mut sender, mut receiver) = ParallelTokenQueue::new();

	let lexer_options: ezno_parser::LexerOptions = Default::default();
	let other = script.clone();
	let thread = spawn(move || lex_script(&script, &mut sender, &lexer_options, None));
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

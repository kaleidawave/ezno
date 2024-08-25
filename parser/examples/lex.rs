use std::thread::spawn;

use ezno_parser::{lex_script, LexerOptions};
use tokenizer_lib::{sized_tokens::SizedToken, ParallelTokenQueue, Token, TokenReader};

fn main() -> Result<(), Box<dyn std::error::Error>> {
	let mut args: std::collections::VecDeque<_> = std::env::args().skip(1).collect();
	let path = args.pop_front().ok_or("expected argument")?;
	let print_tokens = !args.iter().any(|item| item == "--quiet");
	let top_level_html = args.iter().any(|item| item == "--top-level-html");

	let script = std::fs::read_to_string(path)?;
	lex_and_print_tokens(script, print_tokens, top_level_html);
	Ok(())
}

fn lex_and_print_tokens(script: String, print_tokens: bool, top_level_html: bool) {
	let (mut sender, mut receiver) = ParallelTokenQueue::new();

	println!("{:?}", ezno_parser::source_map::LineStarts::new(&script));

	let lexer_options: LexerOptions = LexerOptions { top_level_html, ..Default::default() };
	let other = script.clone();
	let thread = spawn(move || lex_script(&script, &mut sender, &lexer_options, None));

	// let mut count = 0;
	if print_tokens {
		println!("token | start (1 based) | length");
		while let Some(Token(token, start)) = receiver.next() {
			// count += 1;
			let length = token.length();
			let represents =
				other.get((start.0 as usize)..(start.0 as usize + length as usize)).unwrap();
			println!("{:?} {} {} {:?}", token, start.0 + 1u32, length, represents);
		}
	} else {
		// Drain anyway
		while let Some(_) = receiver.next() {}
	}
	// println!("{count} tokens");

	match thread.join().unwrap() {
		Ok(()) => {
			eprintln!("lexed source 🎉");
		}
		Err((lexer_err, _)) => {
			eprintln!("lexer error: {lexer_err:?}");
		}
	}
}

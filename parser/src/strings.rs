use crate::derive_ASTNode;

/// What surrounds a string
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[apply(derive_ASTNode!)]
pub enum Quoted {
	Single,
	Double,
}

impl Quoted {
	#[must_use]
	pub fn as_char(self) -> char {
		match self {
			Quoted::Single => '\'',
			Quoted::Double => '"',
		}
	}
}

/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#escape_sequences>
/// `Ok(true) = skip_next`
pub fn escape_character(chr: char, after: &str, buf: &mut String) -> Result<usize, ()> {
	fn parse_hex(on: &str) -> u32 {
		let mut value = 0u32;
		for byte in on.bytes() {
			value <<= 4; // log2(16) = 4
			let code = match byte {
				b'0'..=b'9' => u32::from(byte - b'0'),
				b'a'..=b'f' => u32::from(byte - b'a') + 10,
				b'A'..=b'F' => u32::from(byte - b'A') + 10,
				byte => {
					panic!("bad char {byte:?}!");
				}
			};
			value |= code;
		}
		value
	}

	match chr {
		'\'' | '\"' | '`' | '\\' => {
			buf.push(chr);
			Ok(1)
		}
		't' => Ok(1),
		'n' => {
			buf.push('\n');
			Ok(1)
		}
		'r' => {
			buf.push('\r');
			Ok(1)
		}
		'0' => {
			buf.push('\0');
			Ok(1)
		}
		'v' => {
			buf.push_str("\u{000B}");
			Ok(1)
		}
		'b' => {
			buf.push_str("\u{0008}");
			Ok(1)
		}
		'f' => {
			buf.push_str("\u{000C}");
			Ok(1)
		}
		// Line endings
		'\u{000A}' | '\u{000D}' | '\u{2028}' | '\u{2029}' => {
			// custom
			Ok(0)
		}
		'x' => {
			if let Some(hex_code) = after.get(..2) {
				let code = parse_hex(hex_code);
				if let Some(chr) = char::from_u32(code) {
					buf.push(chr);
					Ok(hex_code.len() + 1)
				} else {
					Err(())
				}
			} else {
				Err(())
			}
		}
		'u' => {
			if let Some(after) = after.strip_prefix('{') {
				if let Some((inner, _)) = after.split_once('}') {
					let code = parse_hex(inner);
					if let Some(chr) = char::from_u32(code) {
						buf.push(chr);
						Ok(3 + inner.len())
					} else {
						eprintln!("bad code {inner:?}");
						Err(())
					}
				} else {
					Err(())
				}
			} else {
				Err(())
			}
		}
		chr => {
			eprintln!("unexpected item {chr:?}");
			Ok(0)
		}
	}
}

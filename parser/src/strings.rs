use std::borrow::Cow;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringError {
	InvalidStart,
	InvalidDelimeter,
	// TODO
	InvalidCharacter,
}

pub fn parse_string(current: &str) -> Result<(Cow<'_, str>, Quoted, u32), StringError> {
	let (delimeter, quoted) = if current.starts_with('"') {
		('"', Quoted::Double)
	} else if current.starts_with('\'') {
		('\'', Quoted::Single)
	} else {
		return Err(StringError::InvalidStart);
	};

	let chars: [char; _] = [delimeter, '\\', '\u{000A}', '\u{000D}', '\u{2028}', '\u{2029}'];

	let mut buf = Cow::Borrowed("");
	let current = &current[1..];
	let delimeters = current.match_indices(chars);

	let mut last = 0;
	for (idx, matched) in delimeters {
		if last > idx {
			continue;
		}
		buf += &current[last..idx];

		if let "\"" | "'" = matched {
			return Ok((buf, quoted, idx as u32 + 2));
		} else if matched == "\\" {
			let immediate = &current[idx + 1..];
			let chr = immediate.chars().next();
			if let Some(chr) = chr {
				let on = &immediate[chr.len_utf8()..];
				let result = escape_character(chr, on, buf.to_mut());
				match result {
					Ok(offset) => {
						// Skip others
						last = idx + 1 + offset;
					}
					Err(()) => {
						return Err(StringError::InvalidCharacter);
					}
				}
			} else {
				eprintln!("Expected end");
				return Err(StringError::InvalidCharacter);
			}
		} else {
			eprintln!("Expected matched but got {matched:?}");
			return Err(StringError::InvalidCharacter);
		}
	}

	Err(StringError::InvalidCharacter)
}

// What surrounds a string
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[apply(crate::derive_ASTNode!)]
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

fn parse_hex(on: &str) -> Result<u32, ()> {
	let mut value = 0u32;
	for byte in on.bytes() {
		value <<= 4; // log2(16) = 4
		let code = match byte {
			b'0'..=b'9' => u32::from(byte - b'0'),
			b'a'..=b'f' => u32::from(byte - b'a') + 10,
			b'A'..=b'F' => u32::from(byte - b'A') + 10,
			byte => {
				eprintln!("bad hex char {chr:?}!", chr = char::from(byte));
				return Err(());
			}
		};
		value |= code;
	}
	Ok(value)
}

/// <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#escape_sequences>
/// `Ok(true) = skip_next`
pub fn escape_character(chr: char, after: &str, buf: &mut String) -> Result<usize, ()> {
	match chr {
		'\'' | '\"' | '`' | '\\' => {
			buf.push(chr);
			Ok(1)
		}
		't' => {
			buf.push('\t');
			Ok(1)
		}
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
			buf.push('\u{000B}');
			Ok(1)
		}
		'b' => {
			buf.push('\u{0008}');
			Ok(1)
		}
		'f' => {
			buf.push('\u{000C}');
			Ok(1)
		}
		// Line endings
		chr @ ('\u{000A}' | '\u{000D}' | '\u{2028}' | '\u{2029}') => {
			let mut count = chr.len_utf8();
			let mut after = after.chars();
			while let Some(chr @ ('\u{000A}' | '\u{000D}' | '\u{2028}' | '\u{2029}')) = after.next()
			{
				count += chr.len_utf8();
			}
			Ok(count)
		}
		// Hexadecimal escape sequences
		'x' => {
			if let Some(hex_code) = after.get(..2) {
				let code = parse_hex(hex_code)?;
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
		// Unicode escape sequences
		'u' => {
			let (chr, width) = parse_unicode_escape_sequence(after)?;
			buf.push(chr);
			Ok(1 + width)
		}
		chr => {
			eprintln!("unexpected escape {chr:?}");
			Ok(0)
		}
	}
}

/// For string and (some reason) identifiers.
/// Parses from after `u` character
pub fn parse_unicode_escape_sequence(on: &str) -> Result<(char, usize), ()> {
	if let Some(on) = on.strip_prefix('{') {
		if let Some((inner, _)) = on.split_once('}') {
			// TODO I think this can be multiple characters
			let code = parse_hex(inner)?;
			if let Some(chr) = char::from_u32(code) {
				Ok((chr, 2 + inner.len()))
			} else {
				eprintln!("bad code {inner:?}");
				Err(())
			}
		} else {
			Err(())
		}
	} else if let Some(lead) = on.get(0..4) {
		// TODO no early return here
		let lead = parse_hex(lead)?;
		// https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Surrogates
		let surrogate = on.get(4..10).and_then(|on| on.strip_prefix("\\u"));
		let (code, count) = if let Some(trail) = surrogate
			&& !trail.starts_with('{')
		{
			// TODO no early return here
			let trail = parse_hex(trail)?;
			if (0xD800..=0xDBFF).contains(&lead) && (0xDC00..=0xDFFF).contains(&trail) {
				// https://tc39.es/ecma262/#sec-utf16decodesurrogatepair
				// "Let cp be (lead - 0xD800) × 0x400 + (trail - 0xDC00) + 0x10000"
				let code = (lead - 0xD800) * 0x400 + (trail - 0xDC00) + 0x10000;
				(code, 10)
			} else {
				// FUTURE not sure? "a" => single?
				// "A code unit that is not a leading surrogate and not a trailing surrogate is interpreted as a
				// code point with the same value"
				// and
				// "A code unit that is a leading surrogate or trailing surrogate, but is not part of a surrogate
				// pair, is interpreted as a code point with the same value."
				(lead, 4)
			}
		} else {
			(lead, 4)
		};
		if let Some(chr) = char::from_u32(code) {
			Ok((chr, count))
		} else {
			eprintln!("TODO warning here {lead:?}");
			Ok(('\u{FFFD}', count))
		}
	} else {
		Err(())
	}
}

#[cfg(test)]
mod tests {
	use super::{Quoted, parse_string};
	use std::borrow::Cow::Borrowed as b;

	#[test]
	fn quoted() {
		assert_eq!(parse_string("'Hello World'"), Ok((b("Hello World"), Quoted::Single, 13)));
		assert_eq!(
			parse_string("'Hello World'.length"),
			Ok((b("Hello World"), Quoted::Single, 13))
		);

		assert_eq!(parse_string("\"Hello World\""), Ok((b("Hello World"), Quoted::Double, 13)));
		assert_eq!(
			parse_string("\"Hello World\".length"),
			Ok((b("Hello World"), Quoted::Double, 13))
		);
	}

	#[test]
	fn escape_sequences() {
		assert_eq!(parse_string("'\\r\\n\\t'"), Ok((b("\r\n\t"), Quoted::Single, 8)));
		assert_eq!(
			parse_string("'\\0\\v\\b\\f'"),
			Ok((b("\0\u{000B}\u{0008}\u{000C}"), Quoted::Single, 10))
		);
	}

	#[test]
	fn hex_specifier() {
		assert_eq!(parse_string("'\\x41'"), Ok((b("A"), Quoted::Single, 6)));
		assert_eq!(parse_string("'\\x415'"), Ok((b("A5"), Quoted::Single, 7)));
	}

	#[test]
	fn unicode() {
		assert_eq!(parse_string("'\\u{1f600}'"), Ok((b("😀"), Quoted::Single, 11)));

		assert_eq!(parse_string("'\\u{2f804}'"), Ok((b("你"), Quoted::Single, 11)));
		assert_eq!(parse_string("'\\uD87E\\uDC04'"), Ok((b("你"), Quoted::Single, 14)));

		// TODO more
	}

	#[test]
	fn whitespace() {
		assert_eq!(parse_string("'a\\\nb'"), Ok((b("ab"), Quoted::Single, 6)));
	}
}

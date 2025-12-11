use std::borrow::Cow;

/// What surrounds string content
#[derive(PartialEq, Eq, Debug, Clone, Copy)]
#[apply(crate::derive_ASTNode!)]
pub enum Quoting {
	Single,
	Double,
}

impl Quoting {
	#[must_use]
	pub fn as_char(self) -> char {
		match self {
			Quoting::Single => '\'',
			Quoting::Double => '"',
		}
	}

	#[must_use]
	pub fn from_char(chr: char) -> Result<Self, char> {
		match chr {
			'\'' => Ok(Quoting::Single),
			'"' => Ok(Quoting::Double),
			chr => Err(chr),
		}
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringError {
	EmptyBuffer,
	/// Bad first character
	InvalidStart(char),
	/// The buffer contains no end
	NoDelimeter,
}

#[derive(PartialEq, Debug)]
pub struct ParseStringOutput<'a> {
	/// may have been transformed
	pub value: Cow<'a, str>,
	pub quoting: Quoting,
	/// used to advance read head
	pub source_length: u32,
	/// relative offsets of unknown escapes
	pub unknown_escapes: Vec<u32>,
}

pub fn parse_string<'a>(current: &'a str) -> Result<ParseStringOutput<'a>, StringError> {
	let (delimeter, quoting) = if current.starts_with('"') {
		('"', Quoting::Double)
	} else if current.starts_with('\'') {
		('\'', Quoting::Single)
	} else if let Some(first) = current.chars().next() {
		return Err(StringError::InvalidStart(first));
	} else {
		return Err(StringError::EmptyBuffer);
	};

	// All code points may appear literally in a string literal except for the closing quote code points, U+005C (REVERSE SOLIDUS), U+000D (CARRIAGE RETURN), and U+000A (LINE FEED)
	let chars: [char; _] = [delimeter, '\\', '\u{000A}', '\u{000D}'];

	let mut buf = Cow::Borrowed("");
	let current = &current[1..];
	let delimeters = current.match_indices(chars);

	let mut unknown_escapes = Vec::new();

	let mut last = 0;
	for (idx, matched) in delimeters {
		if last > idx {
			continue;
		}
		buf += &current[last..idx];

		// this is okay because delimeter is dynamic...
		if let "\"" | "'" = matched {
			let output = ParseStringOutput {
				value: buf,
				quoting,
				source_length: idx as u32 + 2,
				unknown_escapes,
			};
			return Ok(output);
		} else if matched == "\\" {
			let immediate = &current[idx + 1..];
			let chr = immediate.chars().next();
			if let Some(chr) = chr {
				let after = &immediate[chr.len_utf8()..];
				let result = escape_character(chr, after, buf.to_mut());
				match result {
					Ok(offset) => {
						// Skip others
						last = idx + 1 + offset;
					}
					Err(_) => {
						unknown_escapes.push(idx as u32);
						last = idx + 1;
					}
				}
			} else {
				return Err(StringError::NoDelimeter);
			}
		} else {
			return Err(StringError::NoDelimeter);
		}
	}

	Err(StringError::NoDelimeter)
}

fn parse_hex(on: &str) -> Result<u32, &str> {
	let mut value = 0u32;
	for byte in on.bytes() {
		value <<= 4; // log2(16) = 4
		let code = match byte {
			b'0'..=b'9' => u32::from(byte - b'0'),
			b'a'..=b'f' => u32::from(byte - b'a') + 10,
			b'A'..=b'F' => u32::from(byte - b'A') + 10,
			_byte => {
				return Err(on);
			}
		};
		value |= code;
	}
	Ok(value)
}

#[derive(Debug)]
pub enum EscapeError {
	UnknownEscape(char),
	// Either not length, or bad character
	InvalidHexadecimalSequence,
	// Missing } etc
	InvalidUnicodeSequence,
	HexadecimalNotValidCharacter { code: u32 },
}

/// Appends an [escape sequence](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#escape_sequences) to `buf` based on the character `chr` after the backslash and any characters in `after`
///
/// # Errors
///
/// See [`EscapeError`]
pub fn escape_character(chr: char, after: &str, buf: &mut String) -> Result<usize, EscapeError> {
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
				let Ok(code) = parse_hex(hex_code) else {
					return Err(EscapeError::InvalidHexadecimalSequence);
				};
				if let Some(chr) = char::from_u32(code) {
					buf.push(chr);
					Ok(hex_code.len() + 1)
				} else {
					Err(EscapeError::HexadecimalNotValidCharacter { code })
				}
			} else {
				Err(EscapeError::InvalidHexadecimalSequence)
			}
		}
		// Unicode escape sequences
		'u' => {
			let (chr, width) = parse_unicode_escape_sequence(after)?;
			buf.push(chr);
			Ok(1 + width)
		}
		chr => Err(EscapeError::UnknownEscape(chr)),
	}
}

/// For string and (some reason) identifiers.
/// Parses from after `u` character
pub fn parse_unicode_escape_sequence(on: &str) -> Result<(char, usize), EscapeError> {
	if let Some(on) = on.strip_prefix('{') {
		if let Some((inner, _)) = on.split_once('}') {
			// TODO I think this can be multiple characters
			let Ok(code) = parse_hex(inner) else {
				return Err(EscapeError::InvalidHexadecimalSequence);
			};
			if let Some(chr) = char::from_u32(code) {
				Ok((chr, 2 + inner.len()))
			} else {
				Err(EscapeError::HexadecimalNotValidCharacter { code })
			}
		} else {
			Err(EscapeError::InvalidUnicodeSequence)
		}
	} else if let Some(lead) = on.get(0..4) {
		// TODO no early return here
		let Ok(lead) = parse_hex(lead) else {
			return Err(EscapeError::InvalidHexadecimalSequence);
		};
		// https://en.wikipedia.org/wiki/Universal_Character_Set_characters#Surrogates
		let surrogate = on.get(4..10).and_then(|on| on.strip_prefix("\\u"));
		let (code, count) = if let Some(trail) = surrogate
			&& !trail.starts_with('{')
		{
			// TODO no early return here
			let Ok(trail) = parse_hex(trail) else {
				return Err(EscapeError::InvalidHexadecimalSequence);
			};
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
			Err(EscapeError::HexadecimalNotValidCharacter { code })
		}
	} else {
		Err(EscapeError::InvalidUnicodeSequence)
	}
}

#[cfg(test)]
mod tests {
	use super::{
		ParseStringOutput,
		Quoting::{self, Double, Single},
		parse_string,
	};

	fn pso<'a>(on: &'a str, quoting: Quoting, source_length: usize) -> ParseStringOutput<'a> {
		ParseStringOutput {
			value: std::borrow::Cow::Borrowed(on),
			quoting,
			source_length: source_length as u32,
			unknown_escapes: Vec::default(),
		}
	}

	#[test]
	fn Quoting() {
		assert_eq!(parse_string("'Hello World'"), Ok(pso("Hello World", Single, 13)));
		assert_eq!(parse_string("'Hello World'.length"), Ok(pso("Hello World", Single, 13)));

		assert_eq!(parse_string("\"Hello World\""), Ok(pso("Hello World", Double, 13)));
		assert_eq!(parse_string("\"Hello World\".length"), Ok(pso("Hello World", Double, 13)));
	}

	#[test]
	fn escape_sequences() {
		assert_eq!(parse_string("'\\r\\n\\t'"), Ok(pso("\r\n\t", Single, 8)));
		assert_eq!(
			parse_string("'\\0\\v\\b\\f'"),
			Ok(pso("\0\u{000B}\u{0008}\u{000C}", Single, 10))
		);
	}

	#[test]
	fn hex_specifier() {
		assert_eq!(parse_string("'\\x41'"), Ok(pso("A", Single, 6)));
		assert_eq!(parse_string("'\\x415'"), Ok(pso("A5", Single, 7)));
	}

	#[test]
	fn unicode() {
		assert_eq!(parse_string("'\\u{1f600}'"), Ok(pso("😀", Single, 11)));

		assert_eq!(parse_string("'\\u{2f804}'"), Ok(pso("你", Single, 11)));
		assert_eq!(parse_string("'\\uD87E\\uDC04'"), Ok(pso("你", Single, 14)));

		// TODO more
	}

	#[test]
	fn whitespace() {
		assert_eq!(parse_string("'a\\\nb'"), Ok(pso("ab", Single, 6)));
	}
}

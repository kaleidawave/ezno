use crate::derive_ASTNode;
use std::borrow::Cow;

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

fn count_leading_whitespace(on: &str) -> u8 {
	if on.starts_with("\r\n") {
		2
	} else if on.starts_with("\n") {
		1
	} else {
		0
	}
}

/// Modified version of <https://github.com/parcel-bundler/parcel/blob/f86f5f27c3a6553e70bd35652f19e6ab8d8e4e4a/crates/dev-dep-resolver/src/lib.rs#L368-L380>
/// https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Lexical_grammar#escape_sequences
#[must_use]
pub fn unescape_string_content(on: &str) -> Cow<'_, str> {
	let mut result = Cow::Borrowed("");
	let mut start = 0;
	for (idx, _matched) in on.match_indices('\\') {
		result += &on[start..idx];
		let after = &on[idx + 1..];
		let skipped_line_sequence_characters = count_leading_whitespace(after);
		if skipped_line_sequence_characters > 0 {
			start = idx + 1 + skipped_line_sequence_characters as usize;
		} else {
			let next = after.chars().next();
			let to_add = match next {
				Some('0') => "\0",
				Some('\'') => "'",
				Some('"') => "\"",
				Some('\\') => "\\",
				Some('n') => "\n",
				Some('r') => "\r",
				Some('v') => "\u{000B}",
				Some('t') => "\t",
				Some('b') => "\u{0008}",
				Some('f') => "\u{000C}",
				Some('x') => {
					if let Some(after) = after[1..].get(..2) {
						let code = parse_hex(after);
						if let Some(chr) = char::from_u32(code) {
							result.to_mut().push(chr);
						} else {
							panic!("bad code {after}");
						}
						start = idx + 2 + after.len();
					} else {
						panic!("hmm");
					}
					continue;
				}
				Some('u') => {
					let after = &after[1..];
					if after.starts_with('{') {
						if let Some((after, _)) = after[1..].split_once('}') {
							let code = parse_hex(after);
							if let Some(chr) = char::from_u32(code) {
								result.to_mut().push(chr);
							} else {
								panic!("bad code {after}");
							}
							start = idx + 4 + after.len();
						} else {
							panic!("hmm");
						}
					} else {
						panic!("hmm");
					}
					continue;
				}
				Some(_) => {
					// Found identity escape
					start = idx + 1;
					continue;
				}
				item => unreachable!("{item:?}"),
			};
			result += to_add;
			start = idx + 2;
		}
	}

	result += &on[start..];
	result
}

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

/// Modified version of <https://github.com/parcel-bundler/parcel/blob/f86f5f27c3a6553e70bd35652f19e6ab8d8e4e4a/crates/dev-dep-resolver/src/lib.rs#L368-L380>
#[must_use]
pub fn escape_string_content(on: &str, string_delimeter: char) -> Cow<'_, str> {
	let mut result = Cow::Borrowed("");
	let mut start = 0;
	// TODO control character
	let matcher =
		|chr: char| chr == string_delimeter || matches!(chr, '\n' | '\r' | '\\' | '\t' | '$');

	for (index, matched) in on.match_indices(matcher) {
		if let "$" = matched {
			if on[index + 1..].starts_with('{') {
				result += &on[start..index];
				result += "\\${";
				start = index + 2;
			}
		} else {
			result += &on[start..index];
			result += "\\";
			result += match matched {
				"\\" | "\"" | "'" | "`" => matched,
				"\n" => "n",
				"\r" => "r",
				"\t" => "t",
				chr => unreachable!("{chr:?}"),
			};
			start = index + 1;
		}
	}

	result += &on[start..];
	result
}

#[cfg(test)]
mod tests {
	use super::*;

	// unescaping

	#[test]
	fn unescape_quotes() {
		assert_eq!(unescape_string_content(r#"Hi \"Ben\""#), "Hi \"Ben\"");
		assert_eq!(unescape_string_content(r#"Hello\`"#), "Hello`");
	}

	#[test]
	fn unescape_unicode() {
		assert_eq!(unescape_string_content(r#"\u{0064}"#), "d");
	}

	#[test]
	fn unescape_ascii() {
		assert_eq!(unescape_string_content(r#"\x12"#), "\x12");
	}

	#[test]
	fn unescape_control_codes() {
		assert_eq!(unescape_string_content(r#"Hi \nhello"#), "Hi \nhello");
	}

	#[test]
	fn unescape_new_lines() {
		assert_eq!(unescape_string_content("Hi \nhello"), "Hi \nhello");
		assert_eq!(unescape_string_content("Hi \\nhello"), "Hi \nhello");
	}

	// escaping

	#[test]
	fn escape_quotes() {
		assert_eq!(escape_string_content("Hi \"Ben\"", '"'), "Hi \\\"Ben\\\"");
		assert_eq!(escape_string_content("Hi ${Ben} `", '`'), "Hi \\${Ben} \\`");
	}

	#[test]
	fn escape_control_codes() {
		assert_eq!(escape_string_content("Hi \t", '"'), "Hi \\t");
		assert_eq!(escape_string_content("Hi\nBen", '"'), "Hi\\nBen");
	}
}

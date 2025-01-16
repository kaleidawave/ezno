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

#[must_use]
pub fn normalise_source_text(on: &str) -> Cow<'_, str> {
	let mut buf: Option<String> = None;
	let mut last_was_backslash = false;
	for (idx, chr) in on.char_indices() {
		match (buf.as_mut(), chr) {
			(Some(buf), '\\') => {
				if last_was_backslash {
					buf.push(chr);
				}
			}
			(None, '\\') => {
				buf = Some(String::from(&on[..idx]));
			}
			(Some(buf), chr) => {
				buf.push(chr);
			}
			(None, _) => {}
		}
		last_was_backslash = '\\' == chr;
	}

	match buf {
		Some(buf) => Cow::Owned(buf),
		None => Cow::Borrowed(on),
	}
}

/// TODO non-allocating/inline modification option
#[must_use]
pub fn encode_source_text(on: &str, string_delimeter: char) -> String {
	let mut buf = String::new();
	for (_idx, chr) in on.char_indices() {
		if let ('"', '"') | ('\'', '\'') | ('`', '`' | '$') = (string_delimeter, chr) {
			buf.push('\\');
			buf.push(chr);
		} else {
			buf.push(chr);
		}
	}
	buf
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn unescape() {
		assert_eq!(normalise_source_text("Hi \\\"Ben\\\""), "Hi \"Ben\"");
	}

	#[test]
	fn escape() {
		assert_eq!(encode_source_text("Hi \"Ben\"", '"'), "Hi \\\"Ben\\\"");
		assert_eq!(encode_source_text("Hi ${Ben} `", '`'), "Hi \\${Ben} \\`");
	}
}

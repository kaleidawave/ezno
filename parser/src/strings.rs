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

// #[cfg(test)]
// mod tests {
// 	use super::*;

// 	// unescaping

// 	#[test]
// 	fn unescape_quotes() {
// 		assert_eq!(unescape_string_content(r#"Hi \"Ben\""#), "Hi \"Ben\"");
// 		assert_eq!(unescape_string_content(r#"Hello\`"#), "Hello`");
// 	}

// 	#[test]
// 	fn unescape_unicode() {
// 		assert_eq!(unescape_string_content(r#"\u{0064}"#), "d");
// 	}

// 	#[test]
// 	fn unescape_ascii() {
// 		assert_eq!(unescape_string_content(r#"\x12"#), "\x12");
// 	}

// 	#[test]
// 	fn unescape_control_codes() {
// 		assert_eq!(unescape_string_content(r#"Hi \nhello"#), "Hi \nhello");
// 	}

// 	#[test]
// 	fn unescape_new_lines() {
// 		assert_eq!(unescape_string_content("Hi \nhello"), "Hi \nhello");
// 		assert_eq!(unescape_string_content("Hi \\nhello"), "Hi \nhello");
// 	}

// 	// escaping

// 	#[test]
// 	fn escape_quotes() {
// 		assert_eq!(escape_string_content("Hi \"Ben\"", '"'), "Hi \\\"Ben\\\"");
// 		assert_eq!(escape_string_content("Hi ${Ben} `", '`'), "Hi \\${Ben} \\`");
// 	}

// 	#[test]
// 	fn escape_control_codes() {
// 		assert_eq!(escape_string_content("Hi \t", '"'), "Hi \\t");
// 		assert_eq!(escape_string_content("Hi\nBen", '"'), "Hi\\nBen");
// 	}
// }

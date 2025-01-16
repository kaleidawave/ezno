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
pub fn unescape_string_content(on: &str) -> Cow<'_, str> {
	let mut result = Cow::Borrowed("");
	let mut start = 0;
	for (index, _matched) in on.match_indices('\\') {
		result += &on[start..index];
		start = index + 1;
	}

	result += &on[start..];
	result
}

/// Also for template literals
/// Modified version of <https://github.com/parcel-bundler/parcel/blob/f86f5f27c3a6553e70bd35652f19e6ab8d8e4e4a/crates/dev-dep-resolver/src/lib.rs#L368-L380>
#[must_use]
pub fn escape_string_content(on: &str, string_delimeter: char) -> Cow<'_, str> {
	let mut result = Cow::Borrowed("");
	let mut start = 0;
	let to_escape: &[char] = match string_delimeter {
		'"' => &['\\', '"'],
		'\'' => &['\\', '\''],
		'`' => &['\\', '`', '$'],
		_ => panic!("Unknown string delimeter {string_delimeter:?}"),
	};
	for (index, matched) in on.match_indices(to_escape) {
		result += &on[start..index];
		result += "\\";
		result += matched;
		start = index + 1;
	}

	result += &on[start..];
	result
}

#[cfg(test)]
mod tests {
	use super::*;

	#[test]
	fn unescape() {
		assert_eq!(unescape_string_content("Hi \\\"Ben\\\""), "Hi \"Ben\"");
		assert_eq!(unescape_string_content("Hello\\`"), "Hello`");
	}

	#[test]
	fn escape() {
		assert_eq!(escape_string_content("Hi \"Ben\"", '"'), "Hi \\\"Ben\\\"");
		assert_eq!(escape_string_content("Hi ${Ben} `", '`'), "Hi \\${Ben} \\`");
	}
}

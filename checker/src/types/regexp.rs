use std::{collections::HashMap, fmt};

use regress::{backends, Flags, Match, Regex};

use crate::BinarySerializable;

#[derive(Debug, Clone)]
pub struct RegExp {
	source: String,
	pub groups: u32,
	pub named_group_indices: HashMap<String, u16>,
	re: Regex,
}

impl RegExp {
	pub fn new(pattern: &str, flags: Option<&str>) -> Self {
		let source = if let Some(flags) = flags {
			format!("/{pattern}/{flags}")
		} else {
			format!("/{pattern}/")
		};

		let mut f = Flags::default();

		if let Some(flags) = flags {
			for flag in flags.chars() {
				match flag {
					'd' => unimplemented!("indices for substring matches are not supported"),
					'g' => unimplemented!("stateful regex is not supported"),
					'i' => f.icase = true,
					'm' => f.multiline = true,
					's' => f.dot_all = true,
					'u' => f.unicode = true,
					'v' => f.unicode_sets = true,
					'y' => unimplemented!("sticky search is not supported"),
					_ => panic!("Unknown flag: {flag:?}"),
				}
			}
		}

		let mut ire = backends::try_parse(pattern.chars().map(u32::from), f).unwrap();
		if !f.no_opt {
			backends::optimize(&mut ire);
		}
		let compiled_regex = backends::emit(&ire);

		// dbg!(&compiled_regex);

		// let insns = compiled_regex.insns;
		// let brackets = compiled_regex.brackets;
		// let start_pred = compiled_regex.start_pred;
		// let loops = compiled_regex.loops;
		let groups = compiled_regex.groups;
		let named_group_indices = compiled_regex.named_group_indices.clone();
		// let flags = compiled_regex.flags;

		let re = Regex::from(compiled_regex);

		Self { source, groups, named_group_indices, re }
	}

	pub fn source(&self) -> &str {
		&self.source
	}

	pub fn exec(&self, string: &str) -> Option<Match> {
		self.re.find(string)
	}
}

impl fmt::Display for RegExp {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.source)
	}
}

// TODO: Optimize
impl BinarySerializable for RegExp {
	fn serialize(self, buf: &mut Vec<u8>) {
		self.source.serialize(buf);
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source_id: source_map::SourceId) -> Self {
		let source = String::deserialize(iter, source_id);

		let (pattern, flags) = source[1..].split_once('/').unwrap();
		let flags = if flags.is_empty() { None } else { Some(flags) };

		Self::new(pattern, flags)
	}
}

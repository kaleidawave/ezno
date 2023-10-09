use std::collections::BTreeMap;
use std::fmt::Debug;
use std::ops::Range;

#[derive(Debug)]
pub struct RangeMap<T> {
	entries: BTreeMap<u32, Vec<(u32, T)>>,
}

impl<T> Default for RangeMap<T> {
	fn default() -> Self {
		Self { entries: BTreeMap::default() }
	}
}

impl<T> RangeMap<T> {
	pub fn new() -> Self {
		Self { entries: Default::default() }
	}

	pub fn push(&mut self, range: impl Into<Range<u32>>, item: T) {
		let range = range.into();
		if let Some(existing) = self.entries.get_mut(&range.start) {
			existing.push((range.end, item));
		} else {
			self.entries.insert(range.start, vec![(range.end, item)]);
		}
	}

	/// Get the top level entry at some point
	pub fn get(&self, point: u32) -> Option<&T> {
		self.entries
			.range(0..(point + 1))
			// very important to reverse
			.rev()
			.find_map(|(_, v)| v.iter().find_map(|(e, v)| (*e > point).then_some(v)))
	}

	/// Get at an exact range
	pub fn get_exact(&self, range: impl Into<Range<u32>>) -> Option<&T> {
		let range = range.into();
		self.entries
			.get(&range.start)
			.and_then(|v| v.iter().find_map(|(e, v)| (*e == range.end).then_some(v)))
	}
}

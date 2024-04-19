use std::collections::BTreeMap;
use std::fmt::Debug;
use std::ops::Range;

#[derive(Debug)]
pub struct RangeMap<T> {
	/// First u32 is the start, the second second pairing is the end
	entries: BTreeMap<u32, Vec<(u32, T)>>,
}

impl<T> Default for RangeMap<T> {
	fn default() -> Self {
		Self { entries: BTreeMap::default() }
	}
}

impl<T> RangeMap<T> {
	#[must_use]
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
	#[must_use]
	pub fn get(&self, point: u32) -> Option<&T> {
		self.get_with_range(point).map(|(res, _)| res)
	}

	#[must_use]
	pub fn get_with_range(&self, point: u32) -> Option<(&T, Range<u32>)> {
		self.entries
			.range(0..=point)
			// very important to reverse
			.rev()
			.find_map(|(s, v)| v.iter().find_map(|(e, v)| (*e > point).then_some((v, (*s..*e)))))
	}

	/// TODO into custom iterator
	#[must_use]
	pub fn get_many(&self, point: u32, cb: impl for<'a> Fn(&'a T)) {
		let rev = &mut self
			.entries
			.range(0..=point)
			// very important to reverse
			.rev();

		for (start, values) in rev {
			debug_assert!(*start <= point);
			for (end, value) in values {
				if point <= *end {
					cb(value)
				}
			}
		}
	}

	/// Get at an exact range
	pub fn get_exact(&self, range: impl Into<Range<u32>>) -> Option<&T> {
		let range = range.into();
		self.entries
			.get(&range.start)
			.and_then(|v| v.iter().find_map(|(e, v)| (*e == range.end).then_some(v)))
	}
}

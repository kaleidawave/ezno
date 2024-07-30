/// Small map for 1-5 items
/// Also should be rewindable
#[derive(Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub struct Map<K, V>(pub Vec<(K, V)>);

impl<K, V> Default for Map<K, V> {
	fn default() -> Self {
		Self(Default::default())
	}
}

impl<K, V> Map<K, V>
where
	K: PartialEq,
{
	pub fn get(&self, want: &K) -> Option<&V> {
		self.0.iter().rev().find_map(|(key, value)| (want == key).then_some(value))
	}

	pub fn get_mut(&mut self, want: &K) -> Option<&mut V> {
		self.0.iter_mut().rev().find_map(|(key, value)| (want == key).then_some(value))
	}

	#[must_use]
	pub fn iter(&self) -> impl ExactSizeIterator<Item = &(K, V)> {
		self.0.iter()
	}

	pub fn iter_mut(&mut self) -> impl ExactSizeIterator<Item = &mut (K, V)> {
		self.0.iter_mut()
	}

	#[must_use]
	pub fn values(&self) -> impl ExactSizeIterator<Item = &V> {
		self.0.iter().map(|(_, v)| v)
	}

	/// *assumes `id` not already inside*
	pub fn insert(&mut self, id: K, value: V) {
		self.0.push((id, value));
	}

	#[must_use]
	pub fn is_empty(&self) -> bool {
		self.0.is_empty()
	}

	#[must_use]
	pub fn into_some(self) -> Option<Self> {
		if self.0.is_empty() {
			None
		} else {
			Some(self)
		}
	}

	#[must_use]
	pub fn len(&self) -> usize {
		self.0.len()
	}

	pub fn drop_range(&mut self, range: std::ops::RangeFrom<usize>) {
		self.0.drain(range);
	}
}

impl<K, V> std::iter::IntoIterator for Map<K, V> {
	type Item = (K, V);

	type IntoIter = <Vec<(K, V)> as std::iter::IntoIterator>::IntoIter;

	fn into_iter(self) -> Self::IntoIter {
		self.0.into_iter()
	}
}

impl<K, V> std::iter::FromIterator<(K, V)> for Map<K, V> {
	fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
		Self(Vec::from_iter(iter))
	}
}

impl<K, V> std::iter::Extend<(K, V)> for Map<K, V> {
	fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
		self.0.extend(iter);
	}
}

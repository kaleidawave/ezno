//! A low level flat representation for most Rust types. Suitable for saving and loading from files.
//!
//! Currently exists here as there may be some context related things. May become a separate crate at some point

use std::{
	collections::{HashMap, HashSet},
	convert::TryInto,
};

use source_map::{SourceId, Span, SpanWithSource};

/// TODO not sure about iterator
/// This is automated by the derive macro TODO link
pub(crate) trait BinarySerializable {
	fn serialize(self, buf: &mut Vec<u8>);

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self;
}

impl BinarySerializable for String {
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.push(u8::try_from(self.len()).unwrap());
		buf.extend_from_slice(self.as_bytes());
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		let len = iter.next().unwrap();
		String::from_iter(iter.by_ref().take(len as usize).map(|v| v as char))
	}
}

impl<T: BinarySerializable> BinarySerializable for Option<T> {
	fn serialize(self, buf: &mut Vec<u8>) {
		if let Some(item) = self {
			buf.push(1);
			item.serialize(buf);
		} else {
			buf.push(0);
		}
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		if iter.next().unwrap() == 0 {
			None
		} else {
			Some(T::deserialize(iter, source))
		}
	}
}

impl<T: BinarySerializable> BinarySerializable for Vec<T> {
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.extend_from_slice(&u16::try_from(self.len()).unwrap().to_le_bytes());
		for item in self.into_iter() {
			item.serialize(buf);
		}
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		let size = u16::from_le_bytes([iter.next().unwrap(), iter.next().unwrap()]);
		(0..size).map(|_| T::deserialize(iter, source)).collect()
	}
}

impl<T: BinarySerializable> BinarySerializable for Box<T> {
	fn serialize(self, buf: &mut Vec<u8>) {
		BinarySerializable::serialize(*self, buf)
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		Box::new(T::deserialize(iter, source))
	}
}

impl<T: BinarySerializable> BinarySerializable for Box<[T]> {
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.extend_from_slice(&u16::try_from(self.len()).unwrap().to_le_bytes());
		for item in self.into_vec() {
			item.serialize(buf);
		}
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		let size = u16::from_le_bytes([iter.next().unwrap(), iter.next().unwrap()]);
		(0..size).map(|_| T::deserialize(iter, source)).collect()
	}
}

impl<T, U> BinarySerializable for (T, U)
where
	T: BinarySerializable,
	U: BinarySerializable,
{
	fn serialize(self, buf: &mut Vec<u8>) {
		self.0.serialize(buf);
		self.1.serialize(buf);
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		(T::deserialize(iter, source), U::deserialize(iter, source))
	}
}

impl<K, V> BinarySerializable for HashMap<K, V>
where
	K: BinarySerializable + std::hash::Hash + std::cmp::Eq,
	V: BinarySerializable,
{
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.extend_from_slice(&u16::try_from(self.len()).unwrap().to_le_bytes());

		for (k, v) in self.into_iter() {
			k.serialize(buf);
			v.serialize(buf);
		}
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		let size = u16::from_le_bytes([iter.next().unwrap(), iter.next().unwrap()]);
		(0..size).map(|_| (K::deserialize(iter, source), V::deserialize(iter, source))).collect()
	}
}

impl<V> BinarySerializable for HashSet<V>
where
	V: BinarySerializable + std::hash::Hash + std::cmp::Eq,
{
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.extend_from_slice(&u16::try_from(self.len()).unwrap().to_le_bytes());

		for v in self.into_iter() {
			v.serialize(buf);
		}
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		let size = u16::from_le_bytes([iter.next().unwrap(), iter.next().unwrap()]);
		(0..size).map(|_| V::deserialize(iter, source)).collect()
	}
}

impl<K, V> BinarySerializable for map_vec::Map<K, V>
where
	K: BinarySerializable + std::hash::Hash + std::cmp::Eq,
	V: BinarySerializable,
{
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.extend_from_slice(&u16::try_from(self.len()).unwrap().to_le_bytes());
		for (k, v) in self.into_iter() {
			k.serialize(buf);
			v.serialize(buf);
		}
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		let size = u16::from_le_bytes([iter.next().unwrap(), iter.next().unwrap()]);
		(0..size).map(|_| (K::deserialize(iter, source), V::deserialize(iter, source))).collect()
	}
}

impl<V> BinarySerializable for indexmap::IndexSet<V>
where
	V: BinarySerializable + std::hash::Hash + std::cmp::Eq,
{
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.extend_from_slice(&u16::try_from(self.len()).unwrap().to_le_bytes());

		for v in self.into_iter() {
			v.serialize(buf);
		}
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		let size = u16::from_le_bytes([iter.next().unwrap(), iter.next().unwrap()]);
		(0..size).map(|_| V::deserialize(iter, source)).collect()
	}
}

impl BinarySerializable for SpanWithSource {
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.extend_from_slice(&TryInto::<u32>::try_into(self.start).unwrap().to_le_bytes());
		buf.extend_from_slice(&TryInto::<u32>::try_into(self.end).unwrap().to_le_bytes());
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		let start: u32 = u32::from_le_bytes([
			iter.next().unwrap(),
			iter.next().unwrap(),
			iter.next().unwrap(),
			iter.next().unwrap(),
		]);
		let end: u32 = u32::from_le_bytes([
			iter.next().unwrap(),
			iter.next().unwrap(),
			iter.next().unwrap(),
			iter.next().unwrap(),
		]);

		SpanWithSource { start, end, source }
	}
}

impl BinarySerializable for SourceId {
	fn serialize(self, _buf: &mut Vec<u8>) {
		todo!()
	}

	fn deserialize<I: Iterator<Item = u8>>(_iter: &mut I, _source: SourceId) -> Self {
		todo!()
	}
}

impl BinarySerializable for u32 {
	fn serialize(self, _buf: &mut Vec<u8>) {
		todo!()
	}

	fn deserialize<I: Iterator<Item = u8>>(_iter: &mut I, _source: SourceId) -> Self {
		todo!()
	}
}

impl BinarySerializable for crate::TypeId {
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.extend_from_slice(&self.0.to_le_bytes());
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		Self(u16::from_le_bytes([iter.next().unwrap(), iter.next().unwrap()]))
	}
}

impl BinarySerializable for crate::InternalFunctionId {
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.extend_from_slice(&self.get_id().to_le_bytes());
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		Self::new_from_id(u16::from_le_bytes([iter.next().unwrap(), iter.next().unwrap()]))
	}
}

impl BinarySerializable for crate::AutoConstructorId {
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.extend_from_slice(&self.get_id().to_le_bytes());
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		Self(u16::from_le_bytes([iter.next().unwrap(), iter.next().unwrap()]))
	}
}

impl BinarySerializable for ordered_float::NotNan<f64> {
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.extend_from_slice(&self.into_inner().to_le_bytes());
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		Self::new(f64::from_le_bytes([
			iter.next().unwrap(),
			iter.next().unwrap(),
			iter.next().unwrap(),
			iter.next().unwrap(),
			iter.next().unwrap(),
			iter.next().unwrap(),
			iter.next().unwrap(),
			iter.next().unwrap(),
		]))
		.unwrap()
	}
}

impl BinarySerializable for bool {
	fn serialize(self, buf: &mut Vec<u8>) {
		buf.push(if self { 1 } else { 0 })
	}

	fn deserialize<I: Iterator<Item = u8>>(iter: &mut I, source: SourceId) -> Self {
		iter.next().unwrap() == 1
	}
}

use std::borrow::Cow;

use super::TypeId;

/// Terms. `null` is a special object
/// TODO:
/// - `BigInt` (<https://github.com/rust-num/num-bigint>)
#[derive(Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub enum Constant {
	Number(f64),
	String(String),
	Boolean(bool),
	Symbol { key: String },
	Undefined,
}

impl Constant {
	// Not PartialEq because float
	pub fn equals(&self, other: &Self) -> bool {
		match (self, other) {
			(Constant::Number(a), Constant::Number(b)) if a.is_nan() && b.is_nan() => true,
			(Constant::Number(a), Constant::Number(b)) => a == b,
			(Constant::String(a), Constant::String(b)) => a == b,
			(Constant::Boolean(a), Constant::Boolean(b)) => a == b,
			(Constant::Symbol { key: a }, Constant::Symbol { key: b }) => a == b,
			(Constant::Undefined, Constant::Undefined) => true,
			_ => false,
		}
	}

	/// **AS OF THE JS IMPLEMENTATION**
	#[must_use]
	pub fn as_js_string(&self) -> Cow<str> {
		match self {
			Constant::Number(value) => Cow::Owned(value.to_string()),
			Constant::String(value) => Cow::Borrowed(value),
			Constant::Boolean(value) => Cow::Borrowed(if *value { "true" } else { "false" }),
			Constant::Symbol { key } => Cow::Owned(format!("Symbol({key})")),
			Constant::Undefined => Cow::Borrowed("undefined"),
		}
	}

	/// Like [`Constant::as_js_string`] but adds quotes to strings
	///
	/// TODO take buffer
	pub(crate) fn as_type_name(&self) -> String {
		match self {
			Constant::Number(value) => value.to_string(),
			Constant::String(value) => format!("\"{value}\""),
			Constant::Boolean(value) => if *value { "true" } else { "false" }.to_owned(),
			Constant::Symbol { key } => format!("Symbol({key})"),
			Constant::Undefined => "undefined".to_owned(),
		}
	}

	#[must_use]
	pub fn get_backing_type(&self) -> TypeId {
		match self {
			Constant::Number(_) => TypeId::NUMBER_TYPE,
			Constant::String(_) => TypeId::STRING_TYPE,
			Constant::Boolean(_) => TypeId::BOOLEAN_TYPE,
			Constant::Symbol { .. } => TypeId::SYMBOL_TYPE,
			// TODO ...
			Constant::Undefined => TypeId::NEVER_TYPE,
		}
	}
}

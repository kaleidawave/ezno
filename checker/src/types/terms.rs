use super::TypeId;

/// Terms
/// TODO:
/// - `IntoProof`
/// - `BigInt` (<https://github.com/rust-num/num-bigint>)
/// - Separate `NotNull` term, and implement js subtyping
///
/// TODO not sure about some of these
#[derive(Eq, PartialEq, Hash, Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub enum Constant {
	Number(ordered_float::NotNan<f64>),
	String(String),
	Boolean(bool),
	Symbol {
		key: String,
	},
	/// A unique function type given
	/// the `x` reference of `function x() {}`
	Undefined,
	Null,
	NaN,
}

impl Constant {
	/// **AS OF THE JS IMPLEMENTATION**
	#[must_use]
	pub fn as_js_string(&self) -> String {
		match self {
			Constant::Number(value) => value.to_string(),
			Constant::String(value) => value.clone(),
			Constant::Boolean(value) => if *value { "true" } else { "false" }.to_owned(),
			Constant::Symbol { key } => format!("Symbol({key})"),
			Constant::Undefined => "undefined".to_owned(),
			Constant::Null => "null".to_owned(),
			Constant::NaN => "NaN".to_owned(),
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
			Constant::Null => "null".to_owned(),
			Constant::NaN => "NaN".to_owned(),
		}
	}

	#[must_use]
	pub fn get_backing_type_id(&self) -> TypeId {
		match self {
			Constant::Number(_) | Constant::NaN => TypeId::NUMBER_TYPE,
			Constant::String(_) => TypeId::STRING_TYPE,
			Constant::Boolean(_) => TypeId::BOOLEAN_TYPE,
			Constant::Undefined => TypeId::UNDEFINED_TYPE,
			Constant::Null => TypeId::NULL_TYPE,
			Constant::Symbol { .. } => todo!(),
		}
	}
}

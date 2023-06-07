use crate::TypeId;

#[derive(Debug, Clone, binary_serialize_derive::BinarySerializable)]
pub struct Proxy {
	pub(crate) wraps: TypeId,
	pub(crate) trap_object: TypeId,
}

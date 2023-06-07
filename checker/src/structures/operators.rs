/// Canonical, Pure
#[derive(Debug, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum BinaryOperator {
	Add,
	Multiply,
	Modulo,
	/// TODO undecided
	Exponent,
	BitwiseOperators(BitwiseOperators),
	RelationOperator(RelationOperator),
}

#[derive(Debug, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum BitwiseOperators {
	And,
	Or,
	ShiftLeft,
	ShiftRight,
	ShiftRightUnsigned,
}

#[derive(Debug, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum RelationOperator {
	Equal,
	GreaterThan,
}

#[derive(Debug, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum LogicalOperator {
	And,
	Or,
}

// TODO split mathematics and logical
#[derive(Debug, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum UnaryOperator {
	/// `-x`
	Negation,
	/// `1/x`
	MultiplicativeInverse,
	LogicalNegation,
}

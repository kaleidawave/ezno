/// Canonical, Pure
#[derive(Debug, Clone, Copy, binary_serialize_derive::BinarySerializable)]
pub enum BinaryOperator {
	Add,
	Multiply,
	Modulo,
	Exponent,
	BitwiseOperators(BitwiseOperators),
	RelationOperator(RelationOperator),
	/// TODO undecided, how to represent of as composite of others?
	Subtract,
	Divide,
	LogicalOperator(LogicalOperator),
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
	/// TODO non standard `1/x`
	MultiplicativeInverse,
	LogicalNegation,
}

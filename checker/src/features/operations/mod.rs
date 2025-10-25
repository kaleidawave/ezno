mod logical;
mod mathematical_bitwise;
mod relation;
mod unary;

pub use logical::{LogicalOperator, evaluate_logical_operation_with_expression};
pub use mathematical_bitwise::{MathematicalOrBitwiseOperation, evaluate_mathematical_operation};
pub use relation::{
	CanonicalEqualityAndInequality, EqualityAndInequality, EqualityAndInequalityResultKind,
	evaluate_equality_inequality_operation, is_null_or_undefined,
};
pub use unary::{UnaryOperation, evaluate_unary_operator};

pub struct OperatorOptions {
	pub advanced_numbers: bool,
	pub strict_casts: bool,
}

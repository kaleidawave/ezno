mod logical;
mod mathematical_bitwise;
mod relation;
mod unary;

pub use logical::{evaluate_logical_operation_with_expression, LogicalOperator};
pub use mathematical_bitwise::{evaluate_mathematical_operation, MathematicalOrBitwiseOperation};
pub use relation::{
	evaluate_equality_inequality_operation, is_null_or_undefined, CanonicalEqualityAndInequality,
	EqualityAndInequality, EqualityAndInequalityResultKind,
};
pub use unary::{evaluate_unary_operator, UnaryOperation};

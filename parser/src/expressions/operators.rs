//! Contains the definitions for expressions

//! Operators marked in spec but implemented as a variant of [`crate::Expression`] rather than a *operator*:
//! `OptionalChain`, `OptionalCall`, `OptionalIndex`, Index, Group, Initialize, Call,

use std::convert::TryFrom;

use crate::derive_ASTNode;

/// Comma operator is on [`crate::MultipleExpression`]
/// 
/// `InstanceOf`, In are special operators
#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[apply(derive_ASTNode!)]
pub enum BinaryOperator {
	Add, Subtract, Multiply, Divide, Modulo, Exponent,

    BitwiseShiftLeft, BitwiseShiftRight, BitwiseShiftRightUnsigned,
    BitwiseAnd, BitwiseXOr, BitwiseOr,

    StrictEqual, StrictNotEqual, Equal, NotEqual,
    GreaterThan, LessThan, LessThanEqual, GreaterThanEqual,

    LogicalAnd, LogicalOr,
    NullCoalescing, 

    /// Non standard
    Pipe,
    Compose
}

impl BinaryOperator {
	#[must_use]
	pub fn is_non_standard(&self) -> bool {
		matches!(self, BinaryOperator::Pipe | BinaryOperator::Compose)
	}
}

/// Straight assignment is not here because LHS can be destructuring
#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[apply(derive_ASTNode!)]
pub enum BinaryAssignmentOperator {
    LogicalNullishAssignment,
    
    AddAssign, SubtractAssign, MultiplyAssign, DivideAssign, ModuloAssign, ExponentAssign,
    LogicalAndAssign, LogicalOrAssign,
    BitwiseShiftLeftAssign, BitwiseShiftRightAssign, BitwiseShiftRightUnsigned, 
    BitwiseAndAssign, BitwiseXOrAssign, BitwiseOrAssign,
}

#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[apply(derive_ASTNode!)]
pub enum UnaryOperator {
    Plus, Negation,
    BitwiseNot, LogicalNot,
    Await, TypeOf, Void, Delete,
	Yield, DelegatedYield,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[apply(derive_ASTNode!)]
pub enum IncrementOrDecrement {
	Increment,
	Decrement,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[apply(derive_ASTNode!)]
pub enum UnaryPrefixAssignmentOperator {
	Invert,
	IncrementOrDecrement(IncrementOrDecrement),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[apply(derive_ASTNode!)]
pub struct UnaryPostfixAssignmentOperator(pub IncrementOrDecrement);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum AssociativityDirection {
	NA,
	LeftToRight,
	RightToLeft,
}

impl AssociativityDirection {
	pub(crate) fn should_return(self, parent_precedence: u8, upcoming_precedence: u8) -> bool {
		match self {
			AssociativityDirection::RightToLeft => parent_precedence > upcoming_precedence,
			AssociativityDirection::NA | AssociativityDirection::LeftToRight => {
				parent_precedence >= upcoming_precedence
			}
		}
	}
}

pub trait Operator {
	/// String representation of operator
	fn to_str(&self) -> &'static str;

	/// Taken from: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence#table>
	fn precedence(&self) -> u8;

	/// Returns the associativity of the operator. Taken from: <https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_Precedence#table>
	fn associativity_direction(&self) -> AssociativityDirection;

	/// Is associative with self <https://en.wikipedia.org/wiki/Associative_property>
	fn is_associative(&self) -> bool;

	fn precedence_and_associativity_direction(&self) -> (u8, AssociativityDirection) {
		(self.precedence(), self.associativity_direction())
	}
}

impl Operator for BinaryOperator {
	fn to_str(&self) -> &'static str {
		match self {
			BinaryOperator::Add => "+",
			BinaryOperator::Subtract => "-",
			BinaryOperator::Multiply => "*",
			BinaryOperator::Divide => "/",
			BinaryOperator::Exponent => "**",
			BinaryOperator::LessThan => "<",
			BinaryOperator::GreaterThan => ">",
			BinaryOperator::LessThanEqual => "<=",
			BinaryOperator::GreaterThanEqual => ">=",
			BinaryOperator::Equal => "==",
			BinaryOperator::StrictEqual => "===",
			BinaryOperator::NotEqual => "!=",
			BinaryOperator::StrictNotEqual => "!==",
			BinaryOperator::Modulo => "%",
			BinaryOperator::NullCoalescing => "??",
			BinaryOperator::LogicalAnd => "&&",
			BinaryOperator::LogicalOr => "||",
			BinaryOperator::BitwiseShiftLeft => "<<",
			BinaryOperator::BitwiseShiftRight => ">>",
			BinaryOperator::BitwiseShiftRightUnsigned => ">>>",
			BinaryOperator::BitwiseAnd => "&",
			BinaryOperator::BitwiseOr => "|",
			BinaryOperator::BitwiseXOr => "^",
			BinaryOperator::Compose => "<@>", // ∘
			BinaryOperator::Pipe => "|>",
		}
	}

	fn precedence(&self) -> u8 {
		match self {
			BinaryOperator::Pipe | BinaryOperator::Compose => 15,
			BinaryOperator::Exponent => 14,
			BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulo => 13,
			BinaryOperator::Add | BinaryOperator::Subtract => 12,
			BinaryOperator::BitwiseShiftLeft
			| BinaryOperator::BitwiseShiftRightUnsigned
			| BinaryOperator::BitwiseShiftRight => 11,
			BinaryOperator::LessThan
			| BinaryOperator::LessThanEqual
			| BinaryOperator::GreaterThanEqual
			| BinaryOperator::GreaterThan => 10,
			BinaryOperator::Equal
			| BinaryOperator::NotEqual
			| BinaryOperator::StrictEqual
			| BinaryOperator::StrictNotEqual => 9,
			BinaryOperator::BitwiseAnd => 8,
			BinaryOperator::BitwiseXOr => 7,
			BinaryOperator::BitwiseOr => 6,
			BinaryOperator::LogicalAnd => 5,
			BinaryOperator::NullCoalescing | BinaryOperator::LogicalOr => 4,
		}
	}

	fn associativity_direction(&self) -> AssociativityDirection {
		if let BinaryOperator::Exponent = self {
			AssociativityDirection::RightToLeft
		} else {
			AssociativityDirection::LeftToRight
		}
	}

	fn is_associative(&self) -> bool {
		!matches!(self, Self::Subtract | Self::Exponent | Self::Divide)
	}
}

impl Operator for UnaryOperator {
	fn to_str(&self) -> &'static str {
		match self {
			UnaryOperator::Plus => "+",
			UnaryOperator::Negation => "-",
			UnaryOperator::BitwiseNot => "~",
			UnaryOperator::LogicalNot => "!",
			UnaryOperator::Delete => "delete ",
			UnaryOperator::Yield => "yield ",
			UnaryOperator::DelegatedYield => "yield* ",
			UnaryOperator::Await => "await ",
			UnaryOperator::TypeOf => "typeof ",
			UnaryOperator::Void => "void ",
		}
	}

	fn precedence(&self) -> u8 {
		match self {
			UnaryOperator::TypeOf
			| UnaryOperator::Await
			| UnaryOperator::Delete
			| UnaryOperator::Void
			| UnaryOperator::BitwiseNot
			| UnaryOperator::LogicalNot
			| UnaryOperator::Plus
			| UnaryOperator::Negation => 15,
			UnaryOperator::Yield | UnaryOperator::DelegatedYield => 2,
		}
	}

	fn associativity_direction(&self) -> AssociativityDirection {
		AssociativityDirection::RightToLeft
	}

	fn is_associative(&self) -> bool {
		true
	}
}

impl Operator for BinaryAssignmentOperator {
	fn to_str(&self) -> &'static str {
		match self {
			BinaryAssignmentOperator::LogicalNullishAssignment => "??=",
			BinaryAssignmentOperator::AddAssign => "+=",
			BinaryAssignmentOperator::SubtractAssign => "-=",
			BinaryAssignmentOperator::MultiplyAssign => "*=",
			BinaryAssignmentOperator::DivideAssign => "/=",
			BinaryAssignmentOperator::ModuloAssign => "%=",
			BinaryAssignmentOperator::ExponentAssign => "**=",
			BinaryAssignmentOperator::BitwiseShiftLeftAssign => "<<=",
			BinaryAssignmentOperator::BitwiseShiftRightAssign => ">>=",
			BinaryAssignmentOperator::BitwiseShiftRightUnsigned => ">>>=",
			BinaryAssignmentOperator::BitwiseAndAssign => "&=",
			BinaryAssignmentOperator::BitwiseXOrAssign => "^=",
			BinaryAssignmentOperator::BitwiseOrAssign => "|=",
			BinaryAssignmentOperator::LogicalAndAssign => "&&=",
			BinaryAssignmentOperator::LogicalOrAssign => "||=",
		}
	}

	fn precedence(&self) -> u8 {
		ASSIGNMENT_PRECEDENCE
	}

	fn associativity_direction(&self) -> AssociativityDirection {
		AssociativityDirection::RightToLeft
	}

	fn is_associative(&self) -> bool {
		// dbg!("TODO unsure");
		true
	}
}

impl Operator for UnaryPrefixAssignmentOperator {
	fn precedence(&self) -> u8 {
		15
	}

	fn associativity_direction(&self) -> AssociativityDirection {
		AssociativityDirection::RightToLeft
	}

	fn to_str(&self) -> &'static str {
		match self {
			UnaryPrefixAssignmentOperator::Invert => ">!", // ¡
			UnaryPrefixAssignmentOperator::IncrementOrDecrement(inc_or_dec) => inc_or_dec.to_str(),
		}
	}

	fn is_associative(&self) -> bool {
		true
	}
}

impl Operator for UnaryPostfixAssignmentOperator {
	fn precedence(&self) -> u8 {
		16
	}

	fn associativity_direction(&self) -> AssociativityDirection {
		AssociativityDirection::NA
	}

	fn to_str(&self) -> &'static str {
		self.0.to_str()
	}

	fn is_associative(&self) -> bool {
		true
	}
}

impl IncrementOrDecrement {
	fn to_str(self) -> &'static str {
		match self {
			IncrementOrDecrement::Increment => "++",
			IncrementOrDecrement::Decrement => "--",
		}
	}
}

impl From<BinaryAssignmentOperator> for BinaryOperator {
	fn from(val: BinaryAssignmentOperator) -> Self {
		match val {
			BinaryAssignmentOperator::LogicalNullishAssignment => BinaryOperator::NullCoalescing,
			BinaryAssignmentOperator::AddAssign => BinaryOperator::Add,
			BinaryAssignmentOperator::SubtractAssign => BinaryOperator::Subtract,
			BinaryAssignmentOperator::MultiplyAssign => BinaryOperator::Multiply,
			BinaryAssignmentOperator::DivideAssign => BinaryOperator::Divide,
			BinaryAssignmentOperator::ModuloAssign => BinaryOperator::Modulo,
			BinaryAssignmentOperator::ExponentAssign => BinaryOperator::Exponent,
			BinaryAssignmentOperator::LogicalAndAssign => BinaryOperator::LogicalAnd,
			BinaryAssignmentOperator::LogicalOrAssign => BinaryOperator::LogicalOr,
			BinaryAssignmentOperator::BitwiseShiftLeftAssign => BinaryOperator::BitwiseShiftLeft,
			BinaryAssignmentOperator::BitwiseShiftRightAssign => BinaryOperator::BitwiseShiftRight,
			BinaryAssignmentOperator::BitwiseShiftRightUnsigned => {
				BinaryOperator::BitwiseShiftRightUnsigned
			}
			BinaryAssignmentOperator::BitwiseAndAssign => BinaryOperator::BitwiseAnd,
			BinaryAssignmentOperator::BitwiseXOrAssign => BinaryOperator::BitwiseXOr,
			BinaryAssignmentOperator::BitwiseOrAssign => BinaryOperator::BitwiseOr,
		}
	}
}

impl TryFrom<BinaryOperator> for BinaryAssignmentOperator {
	type Error = ();
	fn try_from(val: BinaryOperator) -> Result<Self, ()> {
		match val {
			BinaryOperator::NullCoalescing => {
				Ok(BinaryAssignmentOperator::LogicalNullishAssignment)
			}
			BinaryOperator::Add => Ok(BinaryAssignmentOperator::AddAssign),
			BinaryOperator::Subtract => Ok(BinaryAssignmentOperator::SubtractAssign),
			BinaryOperator::Multiply => Ok(BinaryAssignmentOperator::MultiplyAssign),
			BinaryOperator::Divide => Ok(BinaryAssignmentOperator::DivideAssign),
			BinaryOperator::Modulo => Ok(BinaryAssignmentOperator::ModuloAssign),
			BinaryOperator::Exponent => Ok(BinaryAssignmentOperator::ExponentAssign),
			BinaryOperator::LogicalAnd => Ok(BinaryAssignmentOperator::LogicalAndAssign),
			BinaryOperator::LogicalOr => Ok(BinaryAssignmentOperator::LogicalOrAssign),
			BinaryOperator::BitwiseShiftLeft => {
				Ok(BinaryAssignmentOperator::BitwiseShiftLeftAssign)
			}
			BinaryOperator::BitwiseShiftRight => {
				Ok(BinaryAssignmentOperator::BitwiseShiftRightAssign)
			}
			BinaryOperator::BitwiseShiftRightUnsigned => {
				Ok(BinaryAssignmentOperator::BitwiseShiftRightUnsigned)
			}
			BinaryOperator::BitwiseAnd => Ok(BinaryAssignmentOperator::BitwiseAndAssign),
			BinaryOperator::BitwiseXOr => Ok(BinaryAssignmentOperator::BitwiseXOrAssign),
			BinaryOperator::BitwiseOr => Ok(BinaryAssignmentOperator::BitwiseOrAssign),
			_ => Err(()),
		}
	}
}

impl BinaryOperator {
	/// Operators which return true may or may not evaluate RHS based on their own value
	#[must_use]
	pub fn is_rhs_conditional_evaluation(&self) -> bool {
		matches!(
			self,
			BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr | BinaryOperator::NullCoalescing
		)
	}
}

// Operator precedences that aren't registered under operator trait
pub(crate) const COMMA_PRECEDENCE: u8 = 1;
pub(crate) const MEMBER_ACCESS_PRECEDENCE: u8 = 18;
pub(crate) const INDEX_PRECEDENCE: u8 = 18;
pub(crate) const CONDITIONAL_TERNARY_PRECEDENCE: u8 = 2;
pub(crate) const FUNCTION_CALL_PRECEDENCE: u8 = 18;
pub(crate) const CONSTRUCTOR_PRECEDENCE: u8 = 18;
pub(crate) const CONSTRUCTOR_WITHOUT_PARENTHESIS_PRECEDENCE: u8 = 17;
pub(crate) const RELATION_PRECEDENCE: u8 = 10;
pub(crate) const PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE: u8 = 19;
pub(crate) const ARROW_FUNCTION_PRECEDENCE: u8 = 2;
pub(crate) const ASSIGNMENT_PRECEDENCE: u8 = 2;

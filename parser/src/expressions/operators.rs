//! Contains the definitions for expressions

//! Operators marked in spec but implemented as a variant of [`crate::Expression`] rather than a *operator*:
//! `OptionalChain`, `OptionalCall`, `OptionalIndex`, Index, Group, Initialize, Call,

use std::convert::TryFrom;

use crate::derive_ASTNode;

/// `instance_of`, `in` are special operators (because of their RHS) not found here
#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[apply(derive_ASTNode!)]
pub enum BinaryOperator {
	Add, Subtract, Multiply, Divide, Remainder, Exponent,

    BitwiseShiftLeft, BitwiseShiftRight, BitwiseShiftRightUnsigned,
    BitwiseAnd, BitwiseXOr, BitwiseOr,

    StrictEqual, StrictNotEqual, Equal, NotEqual,
    GreaterThan, LessThan, LessThanEqual, GreaterThanEqual,

    LogicalAnd, LogicalOr,
    NullCoalescing,

	Comma,

	#[cfg(feature="extras")]
    Pipe,
	#[cfg(feature="extras")]
    Compose
}

impl BinaryOperator {
	/// For parsing under options
	#[must_use]
	#[cfg(feature = "extras")]
	pub fn is_non_standard(&self) -> bool {
		matches!(self, BinaryOperator::Pipe | BinaryOperator::Compose)
	}

	#[cfg(not(feature = "extras"))]
	pub fn is_non_standard(&self) -> bool {
		false
	}

	/// Operators which return true may or may not evaluate RHS based on their own value
	#[must_use]
	pub fn is_rhs_conditional_evaluation(&self) -> bool {
		matches!(
			self,
			BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr | BinaryOperator::NullCoalescing
		)
	}
}

/// Straight assignment is not here because LHS can be destructuring
#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[apply(derive_ASTNode!)]
pub enum BinaryAssignmentOperator {
    NullCoalescing,
    
    Add, Subtract, Multiply, Divide, Remainder, Exponent,
    LogicalAnd, LogicalOr,
    BitwiseShiftLeft, BitwiseShiftRight, BitwiseShiftRightUnsigned, 
    BitwiseAnd, BitwiseXOr, BitwiseOr,
}

// `yield` not here because can be used without expression
#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[apply(derive_ASTNode!)]
pub enum UnaryOperator {
    Plus, Negation,
    BitwiseNot, LogicalNot,
    Await, TypeOf, Void, Delete,
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
			BinaryOperator::Remainder => "%",
			BinaryOperator::NullCoalescing => "??",
			BinaryOperator::LogicalAnd => "&&",
			BinaryOperator::LogicalOr => "||",
			BinaryOperator::BitwiseShiftLeft => "<<",
			BinaryOperator::BitwiseShiftRight => ">>",
			BinaryOperator::BitwiseShiftRightUnsigned => ">>>",
			BinaryOperator::BitwiseAnd => "&",
			BinaryOperator::BitwiseOr => "|",
			BinaryOperator::BitwiseXOr => "^",
			BinaryOperator::Comma => ",",
			#[cfg(feature = "extras")]
			BinaryOperator::Compose => "<@>", // ∘
			#[cfg(feature = "extras")]
			BinaryOperator::Pipe => "|>",
		}
	}

	fn precedence(&self) -> u8 {
		match self {
			#[cfg(feature = "extras")]
			BinaryOperator::Pipe | BinaryOperator::Compose => 15,
			BinaryOperator::Exponent => 14,
			BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Remainder => 13,
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
			BinaryOperator::Comma => 1,
		}
	}

	fn associativity_direction(&self) -> AssociativityDirection {
		if let BinaryOperator::Exponent = self {
			AssociativityDirection::RightToLeft
		} else {
			AssociativityDirection::LeftToRight
		}
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
			| UnaryOperator::Negation => 14,
		}
	}

	fn associativity_direction(&self) -> AssociativityDirection {
		AssociativityDirection::RightToLeft
	}
}

impl Operator for BinaryAssignmentOperator {
	fn to_str(&self) -> &'static str {
		match self {
			BinaryAssignmentOperator::NullCoalescing => "??=",
			BinaryAssignmentOperator::Add => "+=",
			BinaryAssignmentOperator::Subtract => "-=",
			BinaryAssignmentOperator::Multiply => "*=",
			BinaryAssignmentOperator::Divide => "/=",
			BinaryAssignmentOperator::Remainder => "%=",
			BinaryAssignmentOperator::Exponent => "**=",
			BinaryAssignmentOperator::BitwiseShiftLeft => "<<=",
			BinaryAssignmentOperator::BitwiseShiftRight => ">>=",
			BinaryAssignmentOperator::BitwiseShiftRightUnsigned => ">>>=",
			BinaryAssignmentOperator::BitwiseAnd => "&=",
			BinaryAssignmentOperator::BitwiseXOr => "^=",
			BinaryAssignmentOperator::BitwiseOr => "|=",
			BinaryAssignmentOperator::LogicalAnd => "&&=",
			BinaryAssignmentOperator::LogicalOr => "||=",
		}
	}

	fn precedence(&self) -> u8 {
		ASSIGNMENT_PRECEDENCE
	}

	fn associativity_direction(&self) -> AssociativityDirection {
		AssociativityDirection::RightToLeft
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
			BinaryAssignmentOperator::NullCoalescing => BinaryOperator::NullCoalescing,
			BinaryAssignmentOperator::Add => BinaryOperator::Add,
			BinaryAssignmentOperator::Subtract => BinaryOperator::Subtract,
			BinaryAssignmentOperator::Multiply => BinaryOperator::Multiply,
			BinaryAssignmentOperator::Divide => BinaryOperator::Divide,
			BinaryAssignmentOperator::Remainder => BinaryOperator::Remainder,
			BinaryAssignmentOperator::Exponent => BinaryOperator::Exponent,
			BinaryAssignmentOperator::LogicalAnd => BinaryOperator::LogicalAnd,
			BinaryAssignmentOperator::LogicalOr => BinaryOperator::LogicalOr,
			BinaryAssignmentOperator::BitwiseShiftLeft => BinaryOperator::BitwiseShiftLeft,
			BinaryAssignmentOperator::BitwiseShiftRight => BinaryOperator::BitwiseShiftRight,
			BinaryAssignmentOperator::BitwiseShiftRightUnsigned => {
				BinaryOperator::BitwiseShiftRightUnsigned
			}
			BinaryAssignmentOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,
			BinaryAssignmentOperator::BitwiseXOr => BinaryOperator::BitwiseXOr,
			BinaryAssignmentOperator::BitwiseOr => BinaryOperator::BitwiseOr,
		}
	}
}

impl TryFrom<BinaryOperator> for BinaryAssignmentOperator {
	type Error = ();
	fn try_from(val: BinaryOperator) -> Result<Self, ()> {
		match val {
			BinaryOperator::NullCoalescing => Ok(BinaryAssignmentOperator::NullCoalescing),
			BinaryOperator::Add => Ok(BinaryAssignmentOperator::Add),
			BinaryOperator::Subtract => Ok(BinaryAssignmentOperator::Subtract),
			BinaryOperator::Multiply => Ok(BinaryAssignmentOperator::Multiply),
			BinaryOperator::Divide => Ok(BinaryAssignmentOperator::Divide),
			BinaryOperator::Remainder => Ok(BinaryAssignmentOperator::Remainder),
			BinaryOperator::Exponent => Ok(BinaryAssignmentOperator::Exponent),
			BinaryOperator::LogicalAnd => Ok(BinaryAssignmentOperator::LogicalAnd),
			BinaryOperator::LogicalOr => Ok(BinaryAssignmentOperator::LogicalOr),
			BinaryOperator::BitwiseShiftLeft => Ok(BinaryAssignmentOperator::BitwiseShiftLeft),
			BinaryOperator::BitwiseShiftRight => Ok(BinaryAssignmentOperator::BitwiseShiftRight),
			BinaryOperator::BitwiseShiftRightUnsigned => {
				Ok(BinaryAssignmentOperator::BitwiseShiftRightUnsigned)
			}
			BinaryOperator::BitwiseAnd => Ok(BinaryAssignmentOperator::BitwiseAnd),
			BinaryOperator::BitwiseXOr => Ok(BinaryAssignmentOperator::BitwiseXOr),
			BinaryOperator::BitwiseOr => Ok(BinaryAssignmentOperator::BitwiseOr),
			_ => Err(()),
		}
	}
}

// Operator precedences that aren't registered under operator trait
pub(crate) const COMMA_PRECEDENCE: u8 = 1;
pub(crate) const CONDITIONAL_TERNARY_PRECEDENCE: u8 = 2;
pub(crate) const ARROW_FUNCTION_PRECEDENCE: u8 = 2;
pub(crate) const ASSIGNMENT_PRECEDENCE: u8 = 2;
pub(crate) const YIELD_OPERATORS_PRECEDENCE: u8 = 2;
pub(crate) const RELATION_PRECEDENCE: u8 = 10;
pub(crate) const CONSTRUCTOR_WITHOUT_PARENTHESIS_PRECEDENCE: u8 = 17;
pub(crate) const MEMBER_ACCESS_PRECEDENCE: u8 = 18;
pub(crate) const INDEX_PRECEDENCE: u8 = 18;
pub(crate) const FUNCTION_CALL_PRECEDENCE: u8 = 18;
pub(crate) const CONSTRUCTOR_PRECEDENCE: u8 = 18;
pub(crate) const PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE: u8 = 19;

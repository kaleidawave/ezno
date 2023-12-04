//! Contains the definitions for expressions

//! Operators marked in spec but implemented as a variant of [`crate::Expression`] rather than a *operator*:
//! `OptionalChain`, `OptionalCall`, `OptionalIndex`, Index, Group, Initialize, Call,

use std::convert::TryFrom;

use crate::{TSXKeyword, TSXToken};

/// Comma operator is on [`crate::MultipleExpression`]
/// 
/// `InstanceOf`, In are special operators
#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum BinaryOperator {
	Add, Subtract, Multiply, Divide, Modulo, Exponent,

    BitwiseShiftLeft, BitwiseShiftRight, BitwiseShiftRightUnsigned,
    BitwiseAnd, BitwiseXOr, BitwiseOr,

    StrictEqual, StrictNotEqual, Equal, NotEqual,
    GreaterThan, LessThan, LessThanEqual, GreaterThanEqual,

    LogicalAnd, LogicalOr,
    NullCoalescing, 

    /// Non standard
    Divides,
    Pipe,
    Compose
}

/// Straight assignment is not here because LHS can be destructuring
#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum BinaryAssignmentOperator {
    LogicalNullishAssignment,
    
    AddAssign, SubtractAssign, MultiplyAssign, DivideAssign, ModuloAssign, ExponentAssign,
    LogicalAndAssign, LogicalOrAssign,
    BitwiseShiftLeftAssign, BitwiseShiftRightAssign, BitwiseShiftRightUnsigned, 
    BitwiseAndAssign, BitwiseXOrAssign, BitwiseOrAssign,
}

#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum UnaryOperator {
    Plus, Negation,
    BitwiseNot, LogicalNot,
    Await, TypeOf, Void, Delete,
	Yield, DelegatedYield,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum IncrementOrDecrement {
	Increment,
	Decrement,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
pub enum UnaryPrefixAssignmentOperator {
	Invert,
	IncrementOrDecrement(IncrementOrDecrement),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[cfg_attr(feature = "self-rust-tokenize", derive(self_rust_tokenize::SelfRustTokenize))]
#[cfg_attr(feature = "serde-serialize", derive(serde::Serialize))]
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

pub trait Operator: for<'a> TryFrom<&'a TSXToken> {
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
			BinaryOperator::Divides => "/%",  // ∣
			BinaryOperator::Compose => "<@>", // ∘
			BinaryOperator::Pipe => "|>",
		}
	}

	fn precedence(&self) -> u8 {
		match self {
			BinaryOperator::Pipe | BinaryOperator::Compose => 15,
			BinaryOperator::Exponent => 14,
			BinaryOperator::Multiply
			| BinaryOperator::Divide
			| BinaryOperator::Modulo
			| BinaryOperator::Divides => 13,
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
		// dbg!("TODO not sure");
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

impl TryFrom<&TSXToken> for BinaryAssignmentOperator {
	type Error = ();

	fn try_from(expression: &TSXToken) -> Result<Self, Self::Error> {
		match expression {
			TSXToken::AddAssign => Ok(BinaryAssignmentOperator::AddAssign),
			TSXToken::SubtractAssign => Ok(BinaryAssignmentOperator::SubtractAssign),
			TSXToken::MultiplyAssign => Ok(BinaryAssignmentOperator::MultiplyAssign),
			TSXToken::DivideAssign => Ok(BinaryAssignmentOperator::DivideAssign),
			TSXToken::ExponentAssign => Ok(BinaryAssignmentOperator::ExponentAssign),
			TSXToken::LogicalAndAssign => Ok(BinaryAssignmentOperator::LogicalAndAssign),
			TSXToken::LogicalOrAssign => Ok(BinaryAssignmentOperator::LogicalOrAssign),
			TSXToken::BitwiseAndAssign => Ok(BinaryAssignmentOperator::BitwiseAndAssign),
			TSXToken::BitwiseOrAssign => Ok(BinaryAssignmentOperator::BitwiseOrAssign),
			TSXToken::BitwiseXorAssign => Ok(BinaryAssignmentOperator::BitwiseXOrAssign),
			TSXToken::BitwiseShiftLeftAssign => {
				Ok(BinaryAssignmentOperator::BitwiseShiftLeftAssign)
			}
			TSXToken::BitwiseShiftRightAssign => {
				Ok(BinaryAssignmentOperator::BitwiseShiftRightAssign)
			}
			TSXToken::BitwiseShiftRightUnsignedAssign => {
				Ok(BinaryAssignmentOperator::BitwiseShiftRightUnsigned)
			}
			TSXToken::NullishCoalescingAssign => {
				Ok(BinaryAssignmentOperator::LogicalNullishAssignment)
			}
			_ => Err(()),
		}
	}
}

impl TryFrom<&TSXToken> for BinaryOperator {
	type Error = ();

	fn try_from(expression: &TSXToken) -> Result<Self, Self::Error> {
		match expression {
			TSXToken::Add => Ok(BinaryOperator::Add),
			TSXToken::Subtract => Ok(BinaryOperator::Subtract),
			TSXToken::Multiply => Ok(BinaryOperator::Multiply),
			TSXToken::Divide => Ok(BinaryOperator::Divide),
			TSXToken::Exponent => Ok(BinaryOperator::Exponent),
			TSXToken::Modulo => Ok(BinaryOperator::Modulo),
			TSXToken::Equal => Ok(BinaryOperator::Equal),
			TSXToken::NotEqual => Ok(BinaryOperator::NotEqual),
			TSXToken::StrictEqual => Ok(BinaryOperator::StrictEqual),
			TSXToken::StrictNotEqual => Ok(BinaryOperator::StrictNotEqual),
			TSXToken::LogicalOr => Ok(BinaryOperator::LogicalOr),
			TSXToken::LogicalAnd => Ok(BinaryOperator::LogicalAnd),
			TSXToken::OpenChevron => Ok(BinaryOperator::LessThan),
			TSXToken::LessThanEqual => Ok(BinaryOperator::LessThanEqual),
			TSXToken::CloseChevron => Ok(BinaryOperator::GreaterThan),
			TSXToken::GreaterThanEqual => Ok(BinaryOperator::GreaterThanEqual),
			TSXToken::BitwiseAnd => Ok(BinaryOperator::BitwiseAnd),
			TSXToken::BitwiseOr => Ok(BinaryOperator::BitwiseOr),
			TSXToken::BitwiseXOr => Ok(BinaryOperator::BitwiseXOr),
			TSXToken::BitwiseShiftLeft => Ok(BinaryOperator::BitwiseShiftLeft),
			TSXToken::BitwiseShiftRight => Ok(BinaryOperator::BitwiseShiftRight),
			TSXToken::BitwiseShiftRightUnsigned => Ok(BinaryOperator::BitwiseShiftRightUnsigned),
			TSXToken::NullishCoalescing => Ok(BinaryOperator::NullCoalescing),
			#[cfg(feature = "extras")]
			TSXToken::DividesOperator => Ok(BinaryOperator::Divides),
			#[cfg(feature = "extras")]
			TSXToken::ComposeOperator => Ok(BinaryOperator::Compose),
			#[cfg(feature = "extras")]
			TSXToken::PipeOperator => Ok(BinaryOperator::Pipe),
			_ => Err(()),
		}
	}
}

// Note `yield` and `yield *` handled differently
impl TryFrom<&TSXToken> for UnaryOperator {
	type Error = ();

	fn try_from(expression: &TSXToken) -> Result<Self, Self::Error> {
		match expression {
			TSXToken::Keyword(TSXKeyword::TypeOf) => Ok(UnaryOperator::TypeOf),
			TSXToken::Keyword(TSXKeyword::Await) => Ok(UnaryOperator::Await),
			TSXToken::Keyword(TSXKeyword::Void) => Ok(UnaryOperator::Void),
			TSXToken::Keyword(TSXKeyword::Delete) => Ok(UnaryOperator::Delete),
			TSXToken::LogicalNot => Ok(UnaryOperator::LogicalNot),
			TSXToken::BitwiseNot => Ok(UnaryOperator::BitwiseNot),
			TSXToken::Subtract => Ok(UnaryOperator::Negation),
			TSXToken::Add => Ok(UnaryOperator::Plus),
			_ => Err(()),
		}
	}
}

impl TryFrom<&TSXToken> for IncrementOrDecrement {
	type Error = ();

	fn try_from(token: &TSXToken) -> Result<Self, Self::Error> {
		match token {
			TSXToken::Increment => Ok(Self::Increment),
			TSXToken::Decrement => Ok(Self::Decrement),
			_ => Err(()),
		}
	}
}

impl TryFrom<&TSXToken> for UnaryPostfixAssignmentOperator {
	type Error = ();

	fn try_from(token: &TSXToken) -> Result<Self, Self::Error> {
		IncrementOrDecrement::try_from(token).map(Self)
	}
}

impl TryFrom<&TSXToken> for UnaryPrefixAssignmentOperator {
	type Error = ();

	fn try_from(token: &TSXToken) -> Result<Self, Self::Error> {
		#[cfg(feature = "extras")]
		if *token == TSXToken::InvertAssign {
			Ok(Self::Invert)
		} else {
			IncrementOrDecrement::try_from(token).map(Self::IncrementOrDecrement)
		}
		#[cfg(not(feature = "extras"))]
		IncrementOrDecrement::try_from(token).map(Self::IncrementOrDecrement)
	}
}

impl BinaryOperator {
	/// Operators which return true may or may not evaluate RHS based on their own value
	/// TODO might be more
	#[must_use]
	pub fn is_rhs_conditional_evaluation(&self) -> bool {
		matches!(self, BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr)
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
pub(crate) const IN_PRECEDENCE: u8 = 10;
pub(crate) const INSTANCE_OF_PRECEDENCE: u8 = 10;
pub(crate) const PARENTHESIZED_EXPRESSION_AND_LITERAL_PRECEDENCE: u8 = 19;
pub(crate) const ASSIGNMENT_PRECEDENCE: u8 = 2;
pub(crate) const AS_PRECEDENCE: u8 = 2;

use crate::{ASTNode, Expression, PropertyReference, Statement, VariableIdentifier};

use source_map::Span;

/// A trait which means that self can be pushed to a [`TokenSender`]
pub trait IntoAST<T> {
	fn into_ast(self) -> T;
}

impl<T: ASTNode> IntoAST<T> for T {
	fn into_ast(self) -> T {
		self
	}
}

pub struct Ident<'a>(&'a str);

const NULL_SPAN: Span = Span { start: 0, end: 0, source: () };

impl<'a> IntoAST<Expression> for Ident<'a> {
	fn into_ast(self) -> Expression {
		Expression::VariableReference(self.0.to_owned(), NULL_SPAN)
	}
}

impl<'a> IntoAST<Expression> for &'a str {
	fn into_ast(self) -> Expression {
		Expression::StringLiteral(self.to_owned(), crate::Quoted::Double, NULL_SPAN)
	}
}

impl<'a> IntoAST<PropertyReference> for &'a str {
	fn into_ast(self) -> PropertyReference {
		PropertyReference::Standard { property: self.to_owned(), is_private: false }
	}
}

impl<'a> IntoAST<VariableIdentifier> for &'a str {
	fn into_ast(self) -> VariableIdentifier {
		VariableIdentifier::Standard(self.to_owned(), NULL_SPAN)
	}
}

#[allow(clippy::cast_precision_loss)]
impl IntoAST<Expression> for usize {
	fn into_ast(self) -> Expression {
		Expression::NumberLiteral(crate::NumberRepresentation::from(self as f64), NULL_SPAN)
	}
}

impl IntoAST<Expression> for f64 {
	fn into_ast(self) -> Expression {
		Expression::NumberLiteral(crate::NumberRepresentation::from(self), NULL_SPAN)
	}
}

impl IntoAST<Statement> for Expression {
	fn into_ast(self) -> Statement {
		Statement::Expression(self.into())
	}
}

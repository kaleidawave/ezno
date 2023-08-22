use crate::{ASTNode, Expression, PropertyReference, Statement, VariableIdentifier};

use source_map::Span;

/// A trait which means that self can be pushed to a [TokenSender]
pub trait IntoAST<T> {
	fn into_ast(self) -> T;
}

impl<T: ASTNode> IntoAST<T> for T {
	fn into_ast(self) -> T {
		self
	}
}

pub struct Ident<'a>(&'a str);

impl<'a> IntoAST<Expression> for Ident<'a> {
	fn into_ast(self) -> Expression {
		Expression::VariableReference(self.0.to_owned(), Span::NULL_SPAN)
	}
}

impl<'a> IntoAST<Expression> for &'a str {
	fn into_ast(self) -> Expression {
		Expression::StringLiteral(self.to_owned(), crate::Quoted::Double, Span::NULL_SPAN)
	}
}

impl<'a> IntoAST<PropertyReference> for &'a str {
	fn into_ast(self) -> PropertyReference {
		PropertyReference::Standard(self.to_owned())
	}
}

impl<'a> IntoAST<VariableIdentifier> for &'a str {
	fn into_ast(self) -> VariableIdentifier {
		VariableIdentifier::Standard(self.to_owned(), Span::NULL_SPAN)
	}
}

impl IntoAST<Expression> for usize {
	fn into_ast(self) -> Expression {
		Expression::NumberLiteral(crate::NumberStructure::Number(self as f64), Span::NULL_SPAN)
	}
}

impl IntoAST<Expression> for f64 {
	fn into_ast(self) -> Expression {
		Expression::NumberLiteral(crate::NumberStructure::Number(self), Span::NULL_SPAN)
	}
}

impl IntoAST<Statement> for Expression {
	fn into_ast(self) -> Statement {
		Statement::Expression(self.into())
	}
}

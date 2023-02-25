use crate::expressions::ExpressionId;
use crate::{ASTNode, Expression, PropertyReference, Statement, VariableId, VariableIdentifier};

use source_map::SourceId;

// This it because generator AST doesn't have a position in the source
#[doc(hidden)]
const NULL_SOURCE_SPAN: source_map::Span =
	source_map::Span { start: 0, end: 0, source_id: SourceId::NULL };

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
		Expression::VariableReference(self.0.to_owned(), NULL_SOURCE_SPAN, ExpressionId::new())
	}
}

impl<'a> IntoAST<Expression> for &'a str {
	fn into_ast(self) -> Expression {
		Expression::StringLiteral(
			self.to_owned(),
			crate::Quoted::Double,
			NULL_SOURCE_SPAN,
			ExpressionId::new(),
		)
	}
}

impl<'a> IntoAST<PropertyReference> for &'a str {
	fn into_ast(self) -> PropertyReference {
		PropertyReference::Standard(self.to_owned())
	}
}

impl<'a> IntoAST<VariableIdentifier> for &'a str {
	fn into_ast(self) -> VariableIdentifier {
		VariableIdentifier::Standard(self.to_owned(), VariableId::new(), NULL_SOURCE_SPAN)
	}
}

impl IntoAST<Expression> for usize {
	fn into_ast(self) -> Expression {
		Expression::NumberLiteral(
			crate::NumberStructure::Number(self as f64),
			NULL_SOURCE_SPAN,
			ExpressionId::new(),
		)
	}
}

impl IntoAST<Expression> for f64 {
	fn into_ast(self) -> Expression {
		Expression::NumberLiteral(
			crate::NumberStructure::Number(self),
			NULL_SOURCE_SPAN,
			ExpressionId::new(),
		)
	}
}

impl IntoAST<Statement> for Expression {
	fn into_ast(self) -> Statement {
		Statement::Expression(self.into())
	}
}

use ezno_ast_generator::{expr, stmt};
use parser::{
	declarations::{VariableDeclaration, VariableDeclarationItem},
	source_map::Span,
	Declaration, Expression, StatementOrDeclaration,
};
use pretty_assertions::assert_eq;

#[test]
fn main() {
	{
		let x = expr!(x = 4);
		assert_eq!(
			x,
			Expression::Assignment {
				lhs: parser::ast::LHSOfAssignment::VariableOrPropertyAccess(
					parser::ast::VariableOrPropertyAccess::Variable(
						"x".to_owned(),
						Span::NULL_SPAN
					)
				),
				rhs: Expression::NumberLiteral(
					parser::NumberStructure::Number(4f64),
					Span::NULL_SPAN
				)
				.into(),
				position: Span::NULL_SPAN
			}
		);
	}

	{
		let number = 4.2f64.sin();
		let y = stmt!(let y = #number);
		let declaration = VariableDeclarationItem {
			name: parser::WithComment::None(parser::VariableField::Name(
				parser::VariableIdentifier::Standard("y".to_owned(), Span::NULL_SPAN),
			)),
			expression: Some(Expression::NumberLiteral(
				parser::NumberStructure::Number(-0.8715757724135882),
				Span::NULL_SPAN,
			)),
			type_annotation: None,
			position: Span::NULL_SPAN,
		};
		let expected = StatementOrDeclaration::Declaration(Declaration::Variable(
			VariableDeclaration::LetDeclaration {
				keyword: parser::Keyword::new(Span::NULL_SPAN),
				declarations: vec![declaration],
				position: Span::NULL_SPAN,
			},
		));
		assert_eq!(y, expected);
	}

	{
		let name = "test";
		let statement = stmt!(let #name = 4);
		println!("{:#?}", statement);
		let declaration = VariableDeclarationItem {
			name: parser::WithComment::None(parser::VariableField::Name(
				parser::VariableIdentifier::Standard("test".to_owned(), Span::NULL_SPAN),
			)),
			expression: Some(Expression::NumberLiteral(
				parser::NumberStructure::Number(4f64),
				Span::NULL_SPAN,
			)),
			type_annotation: None,
			position: Span::NULL_SPAN,
		};
		let expected = StatementOrDeclaration::Declaration(Declaration::Variable(
			VariableDeclaration::LetDeclaration {
				keyword: parser::Keyword::new(Span::NULL_SPAN),
				declarations: vec![declaration],
				position: Span::NULL_SPAN,
			},
		));
		assert_eq!(statement, expected);
	}
}

use ezno_ast_generator::{expr, stmt};
use ezno_parser::ASTNode;
use pretty_assertions::assert_eq;

#[test]
fn expr() {
	let x = expr!(x = 4);
	{
		use ezno_parser::{source_map, Expression};
		assert_eq!(
			x,
			Expression::Assignment {
				lhs: ezno_parser::ast::LHSOfAssignment::VariableOrPropertyAccess(
					ezno_parser::ast::VariableOrPropertyAccess::Variable(
						"x".to_owned(),
						source_map::Nullable::NULL
					)
				),
				rhs: Expression::NumberLiteral(
					ezno_parser::NumberRepresentation::from(4f64),
					source_map::Nullable::NULL
				)
				.into(),
				position: source_map::Nullable::NULL
			}
		);
	}
}

#[test]
fn stmt_with_expr_interpolation() {
	let number = 4.2f64.sin();
	let y = stmt!(let y = #number);
	{
		use ezno_parser::{
			declarations::{VariableDeclaration, VariableDeclarationItem},
			source_map, Declaration, Expression, StatementOrDeclaration,
		};
		let declaration = VariableDeclarationItem {
			name: ezno_parser::WithComment::None(ezno_parser::VariableField::Name(
				ezno_parser::VariableIdentifier::Standard(
					"y".to_owned(),
					source_map::Nullable::NULL,
				),
			)),
			expression: Some(Expression::NumberLiteral(
				ezno_parser::NumberRepresentation::from(-0.8715757724135882),
				source_map::Nullable::NULL,
			)),
			type_annotation: None,
			position: source_map::Nullable::NULL,
		};
		let expected = StatementOrDeclaration::Declaration(Declaration::Variable(
			VariableDeclaration::LetDeclaration {
				declarations: vec![declaration],
				position: source_map::Nullable::NULL,
			},
		));
		assert_eq!(y, expected);
	}
}

#[test]
fn stmt_with_var_name_interpolation() {
	let name = "test";
	let statement = stmt!(let #name = 4);
	{
		use ezno_parser::{
			declarations::{VariableDeclaration, VariableDeclarationItem},
			source_map, Declaration, Expression, StatementOrDeclaration,
		};
		eprintln!("{:#?}", statement);
		let declaration = VariableDeclarationItem {
			name: ezno_parser::WithComment::None(ezno_parser::VariableField::Name(
				ezno_parser::VariableIdentifier::Standard(
					"test".to_owned(),
					source_map::Nullable::NULL,
				),
			)),
			expression: Some(Expression::NumberLiteral(
				ezno_parser::NumberRepresentation::from(4f64),
				source_map::Nullable::NULL,
			)),
			type_annotation: None,
			position: source_map::Nullable::NULL,
		};
		let expected = StatementOrDeclaration::Declaration(Declaration::Variable(
			VariableDeclaration::LetDeclaration {
				declarations: vec![declaration],
				position: source_map::Nullable::NULL,
			},
		));
		assert_eq!(statement, expected);
	}
}

#[test]
fn interpolation_of_a_statement() {
	let statement = stmt!(let x = 4);
	let my_func = stmt!(function x() {
		console.log(3);
		#statement
	});
	let out = "function x() {
	console.log(3);
	let x = 4
}";
	assert_eq!(my_func.to_string(&Default::default()), out);
}

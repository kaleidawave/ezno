use checker::FunctionId;
use parser::{
	expressions::object_literal::ObjectLiteralMember,
	statements_and_declarations::{
		classes::{ClassMember, ClassProperty},
		ClassDeclaration, StatementOrDeclaration,
	},
	visiting::VisitorMut,
	ASTNode, Expression, ExpressionOrStatementPosition, SourceId,
};

use crate::build::CheckingOutputWithoutDiagnostics;

/// A transformer that optimises expression code
/// - Removes dead functions
///
/// TODO this can still break somethings if functions are used but not called
pub struct ExpressionOptimiser;

impl VisitorMut<Expression, CheckingOutputWithoutDiagnostics> for ExpressionOptimiser {
	fn visit_mut(
		&mut self,
		item: &mut Expression,
		data: &mut CheckingOutputWithoutDiagnostics,
		chain: &parser::visiting::Chain,
	) {
		match item {
			Expression::ObjectLiteral(literal) => {
				// TODO properties and even entire object
				for item in literal.members.iter_mut() {
					if let ObjectLiteralMember::Method(method) = item {
						let position = method.get_position();
						let function_id = FunctionId(chain.get_module(), position.start);
						if !data.is_function_called(function_id) {
							// Make it null for now to not break `Object.keys`
							let key = method.name.clone();
							*item = ObjectLiteralMember::Property {
								key,
								assignment: false,
								value: Expression::Null(position),
								position,
							};
						}
					}
				}
			}
			Expression::ArrowFunction(func) => {
				if !data
					.is_function_called(FunctionId(chain.get_module(), func.get_position().start))
				{
					*item = Expression::Null(func.get_position());
				}
			}
			Expression::ExpressionFunction(func) => {
				if !data
					.is_function_called(FunctionId(chain.get_module(), func.get_position().start))
				{
					*item = Expression::Null(func.get_position());
				}
			}
			Expression::ClassExpression(cls) => {
				shake_class(cls, data, chain.get_module());
			}
			_ => {}
		}
	}
}

/// A transformer that optimises statement code
/// - Removes dead functions
pub struct StatementOptimiser;

impl VisitorMut<StatementOrDeclaration, CheckingOutputWithoutDiagnostics> for StatementOptimiser {
	fn visit_mut(
		&mut self,
		item: &mut StatementOrDeclaration,
		data: &mut CheckingOutputWithoutDiagnostics,
		chain: &parser::visiting::Chain,
	) {
		match item {
			parser::StatementOrDeclaration::Variable(_) => {
				// TODO remove if never read
			}
			parser::StatementOrDeclaration::Function(func) => {
				let function_called = data
					.is_function_called(FunctionId(chain.get_module(), func.get_position().start));
				if !function_called {
					// Replace with property to not break Object.keys for now
					// TODO replacing this with variable isn't great but
					// is the unfortunate design of `StatementOrDeclarationMut`
					*item = parser::StatementOrDeclaration::Empty(func.get_position());
				}
			}
			parser::StatementOrDeclaration::Class(cls) => {
				shake_class(&mut cls.on, data, chain.get_module());
			}
			parser::StatementOrDeclaration::Import(_) => {
				// TODO imported items
			}
			parser::StatementOrDeclaration::Export(_) => {
				// TODO exported items
			}
			parser::StatementOrDeclaration::Enum(_)
			| parser::StatementOrDeclaration::Interface(_)
			| parser::StatementOrDeclaration::TypeAlias(_)
			| parser::StatementOrDeclaration::DeclareVariable(_)
			| parser::StatementOrDeclaration::Namespace(_) => {}
			_ => {}
		}
	}
}

/// TODO properties and even entire class
fn shake_class<T: ExpressionOrStatementPosition>(
	class: &mut ClassDeclaration<T>,
	data: &CheckingOutputWithoutDiagnostics,
	source: SourceId,
) {
	for item in class.members.iter_mut() {
		if let ClassMember::Method(is_static, func) = &item.on {
			let id = FunctionId(source, func.position.start);
			if !data.is_function_called(id) {
				// Replace with property to not break Object.keys for now
				item.on = ClassMember::Property(
					*is_static,
					ClassProperty {
						is_readonly: false,
						is_optional: false,
						key: func.name.clone(),
						type_annotation: None,
						value: Some(Box::new(Expression::Null(func.position))),
						position: func.position,
					},
				);
			}
		}
	}
}

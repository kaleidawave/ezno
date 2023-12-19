use checker::{synthesis::EznoParser, FunctionId, PostCheckData};
use parser::{
	declarations::{
		classes::{ClassMember, ClassProperty},
		ClassDeclaration,
	},
	expressions::object_literal::ObjectLiteralMember,
	visiting::{BlockItemMut, VisitorMut},
	ASTNode, Expression, ExpressionOrStatementPosition, SourceId, StatementOrDeclaration,
};

/// A transformer that optimises expression code
/// - Removes dead functions
///
/// TODO this can still break somethings if functions are used but not called
pub struct ExpressionOptimiser;

impl VisitorMut<Expression, PostCheckData<EznoParser>> for ExpressionOptimiser {
	fn visit_mut(
		&mut self,
		item: &mut Expression,
		data: &mut PostCheckData<EznoParser>,
		chain: &parser::visiting::Chain,
	) {
		match item {
			Expression::ObjectLiteral(literal) => {
				// TODO properties and even entire object
				for item in literal.members.iter_mut() {
					let item = item.get_ast_mut();
					if let ObjectLiteralMember::Method(method) = item {
						let position = *method.get_position();
						let function_id = FunctionId(chain.get_module(), position.start);
						if !data.is_function_called(function_id) {
							// Make it null for now to not break `Object.keys`
							let name = method.name.clone();
							*item = ObjectLiteralMember::Property(
								name,
								Expression::Null(position),
								position,
							);
						}
					}
				}
			}
			Expression::ArrowFunction(func) => {
				if !data
					.is_function_called(FunctionId(chain.get_module(), func.get_position().start))
				{
					*item = Expression::Null(*func.get_position());
				}
			}
			Expression::ExpressionFunction(func) => {
				if !data
					.is_function_called(FunctionId(chain.get_module(), func.get_position().start))
				{
					*item = Expression::Null(*func.get_position());
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

impl VisitorMut<BlockItemMut<'_>, PostCheckData<EznoParser>> for StatementOptimiser {
	fn visit_mut(
		&mut self,
		item: &mut BlockItemMut,
		data: &mut PostCheckData<EznoParser>,
		chain: &parser::visiting::Chain,
	) {
		if let BlockItemMut::StatementOrDeclaration(StatementOrDeclaration::Declaration(
			declaration,
		)) = item
		{
			match declaration {
				parser::Declaration::Variable(_) => {
					// TODO remove if never read
				}
				parser::Declaration::Function(func) => {
					if !data.is_function_called(FunctionId(
						chain.get_module(),
						func.get_position().start,
					)) {
						// Replace with property to not break Object.keys for now
						// TODO replacing this with variable isn't great but
						// is the unfortunate design of `StatementOrDeclarationMut`
						*declaration = parser::Declaration::Variable(
							parser::declarations::VariableDeclaration::LetDeclaration {
								keyword: parser::Keyword::new(parser::Span::NULL_SPAN),
								declarations: Vec::new(),
								position: *func.get_position(),
							},
						)
					}
				}
				parser::Declaration::Class(cls) => {
					shake_class(&mut cls.on, data, chain.get_module());
				}
				parser::Declaration::Import(_) => {
					// TODO imported items
				}
				parser::Declaration::Enum(_)
				| parser::Declaration::Interface(_)
				| parser::Declaration::TypeAlias(_)
				| parser::Declaration::DeclareVariable(_)
				| parser::Declaration::DeclareFunction(_)
				| parser::Declaration::DeclareInterface(_)
				| parser::Declaration::Export(_) => {}
			}
		}
	}
}

/// TODO properties and even entire class
fn shake_class<T: ExpressionOrStatementPosition>(
	class: &mut ClassDeclaration<T>,
	data: &PostCheckData<EznoParser>,
	source: SourceId,
) {
	for item in class.members.iter_mut() {
		if let ClassMember::Method(static_kw, func) = &item.on {
			let id = FunctionId(source, func.position.start);
			if !data.is_function_called(id) {
				// Replace with property to not break Object.keys for now
				item.on = ClassMember::Property(
					static_kw.clone(),
					ClassProperty {
						readonly_keyword: None,
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

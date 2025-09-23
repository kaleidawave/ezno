use super::{
	classes::synthesise_class_declaration, expressions::synthesise_expression,
	expressions::synthesise_multiple_expression, synthesise_block,
	variables::synthesise_variable_declaration_item, EznoParser,
};
use crate::{
	context::{Environment, Scope},
	diagnostics::TypeCheckError,
	features::{
		conditional::new_conditional_context,
		exceptions::new_try_context,
		iteration::{synthesise_iteration, IterationBehavior},
		variables::VariableMutability,
	},
	CheckingData, TypeId,
};
use parser::{
	expressions::MultipleExpression,
	statements_and_declarations::control_flow::for_statement::ForLoopCondition,
	statements_and_declarations::export::ExportDeclaration,
	statements_and_declarations::variables::{VariableDeclaration, VariableDeclarationKeyword},
	statements_and_declarations::StatementOrDeclaration,
	ASTNode, BlockOrSingleStatement,
};

use std::collections::HashMap;

pub type ExportedItems = HashMap<String, crate::features::variables::VariableOrImport>;
pub type ReturnResult = Option<TypeId>;

pub struct StatementInformation {
	label: Option<String>,
}

pub(super) fn synthesise_statement_or_declaration<T: crate::ReadFromFS>(
	statement: &StatementOrDeclaration,
	information: Option<StatementInformation>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	let position = statement.get_position().with_source(environment.get_source());
	match statement {
		StatementOrDeclaration::Variable(declaration) => {
			synthesise_variable_declaration(
				&declaration.item,
				environment,
				checking_data,
				false,
				false,
			);
		}
		StatementOrDeclaration::Class(class) => {
			use super::StatementOrExpressionVariable;
			let class = &class.on.item;

			let existing_id = checking_data
				.local_type_mappings
				.types_to_types
				.get_exact(class.name.identifier.get_position())
				.copied();

			// Adding variable is done inside
			let constructor = synthesise_class_declaration(
				class,
				existing_id,
				TypeId::ANY_TYPE,
				environment,
				checking_data,
			);

			if let Some(variable) = class.name.get_variable_id(environment.get_source()) {
				environment.info.variable_current_value.insert(variable, constructor);
			}
		}
		StatementOrDeclaration::Export(exported) => {
			match &exported.on {
				ExportDeclaration::Default { expression, position } => {
					// TODO can be inferred sometimes
					let result = synthesise_expression(
						expression,
						environment,
						checking_data,
						TypeId::ANY_TYPE,
					);

					if let Scope::Module { ref mut exported, .. } = environment.context_type.scope {
						if exported.default.is_some() {
							checking_data.diagnostics_container.add_error(
								TypeCheckError::DoubleDefaultExport(
									position.with_source(environment.get_source()),
								),
							);
						} else {
							exported.default = Some(result);
						}
					} else {
						checking_data.diagnostics_container.add_error(
							TypeCheckError::NonTopLevelExport(
								position.with_source(environment.get_source()),
							),
						);
					}
				}
				ExportDeclaration::TSDefaultFunctionDeclaration { position, .. } => {
					checking_data.diagnostics_container.add_error(
						TypeCheckError::FunctionWithoutBodyNotAllowedHere {
							position: position.with_source(environment.get_source()),
						},
					);
				}
				_ => {}
			}
		}
		StatementOrDeclaration::Enum(item) => {
			use crate::types::{
				properties::{PropertyKey, PropertyValue, Publicity},
				Constant,
			};

			let r#enum = &item.on.item;

			let mut basis = crate::features::objects::ObjectBuilder::new(
				None,
				&mut checking_data.types,
				r#enum.get_position().with_source(environment.get_source()),
				&mut environment.info,
			);

			// TODO remove enumerate, add add function and more
			for (idx, member) in r#enum.members.iter().enumerate() {
				let parser::ast::EnumMember { name, value, position } = member;
				match value {
					parser::ast::EnumMemberValue::ClassMembers(_) => {
						checking_data.raise_unimplemented_error(
							"class enums",
							position.with_source(environment.get_source()),
						);
					}
					parser::ast::EnumMemberValue::Value(_) => {
						checking_data.raise_unimplemented_error(
							"enum with value",
							position.with_source(environment.get_source()),
						);
					}
					parser::ast::EnumMemberValue::None => {}
				}

				let value =
					checking_data.types.new_constant_type(Constant::Number((idx as u8).into()));

				basis.append(
					Publicity::Public,
					PropertyKey::from(name.clone()),
					PropertyValue::Value(value),
					member.get_position().with_source(environment.get_source()),
					&mut environment.info,
				);
			}

			let variable = crate::VariableId(environment.get_source(), r#enum.get_position().start);
			environment.info.variable_current_value.insert(variable, basis.build_object());
		}
		StatementOrDeclaration::Expression(expression) => {
			synthesise_multiple_expression(
				expression,
				environment,
				checking_data,
				TypeId::ANY_TYPE,
			);
		}
		StatementOrDeclaration::Return(return_statement) => {
			environment.return_value(
				&crate::context::environment::Returnable::Statement(
					return_statement.0.as_ref().map(MultipleExpression::get_inner),
					return_statement.1,
				),
				checking_data,
			);
		}
		StatementOrDeclaration::If(if_statement) => {
			fn run_condition<T: crate::ReadFromFS>(
				current: (&MultipleExpression, &BlockOrSingleStatement),
				others: &[(&MultipleExpression, &BlockOrSingleStatement)],
				last: Option<&BlockOrSingleStatement>,
				environment: &mut Environment,
				checking_data: &mut CheckingData<T, super::EznoParser>,
			) {
				let condition_pos = current.0.get_position();
				let condition = synthesise_multiple_expression(
					current.0,
					environment,
					checking_data,
					TypeId::ANY_TYPE,
				);

				new_conditional_context(
					environment,
					(condition, condition_pos),
					|env: &mut Environment, data: &mut CheckingData<T, EznoParser>| {
						synthesise_block_or_single_statement(current.1, env, data);
					},
					if !others.is_empty() || last.is_some() {
						Some(|env: &mut Environment, data: &mut CheckingData<T, EznoParser>| {
							if let [current, others @ ..] = &others {
								run_condition(*current, others, last, env, data);
							} else {
								synthesise_block_or_single_statement(last.unwrap(), env, data);
							}
						})
					} else {
						None
					},
					checking_data,
				);
			}

			// TODO can this be an iterator?
			let others = if_statement
				.else_conditions
				.iter()
				.map(|cond| (&*cond.condition, &cond.inner))
				.collect::<Vec<_>>();

			let last = if_statement.trailing_else.as_ref().map(|b| &b.inner);

			run_condition(
				(&if_statement.condition, &if_statement.inner),
				others.as_slice(),
				last,
				environment,
				checking_data,
			);
		}
		StatementOrDeclaration::Switch(stmt) => {
			checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
				thing: "Switch statement",
				at: stmt.get_position().with_source(environment.get_source()),
			});
		}
		StatementOrDeclaration::WithStatement(stmt) => {
			checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
				thing: "With statement",
				at: stmt.get_position().with_source(environment.get_source()),
			});
		}
		StatementOrDeclaration::WhileLoop(stmt) => synthesise_iteration(
			IterationBehavior::While(stmt.condition.get_inner()),
			information.and_then(|info| info.label),
			environment,
			checking_data,
			|environment, checking_data| {
				synthesise_block_or_single_statement(&stmt.inner, environment, checking_data);
			},
			position,
		),
		StatementOrDeclaration::DoWhileLoop(stmt) => synthesise_iteration(
			IterationBehavior::DoWhile(stmt.condition.get_inner()),
			information.and_then(|info| info.label),
			environment,
			checking_data,
			|environment, checking_data| {
				synthesise_block_or_single_statement(&stmt.inner, environment, checking_data);
			},
			position,
		),
		StatementOrDeclaration::ForLoop(stmt) => match &stmt.condition {
			ForLoopCondition::ForOf { is_await: _, keyword: _, variable, of, position } => {
				synthesise_iteration(
					IterationBehavior::ForOf { lhs: variable.get_ast_ref(), rhs: &**of },
					information.and_then(|info| info.label),
					environment,
					checking_data,
					|environment, checking_data| {
						synthesise_block_or_single_statement(
							&stmt.inner,
							environment,
							checking_data,
						);
					},
					position.with_source(environment.get_source()),
				);
			}
			ForLoopCondition::ForIn { keyword: _, variable, r#in, position } => {
				synthesise_iteration(
					IterationBehavior::ForIn { lhs: variable.get_ast_ref(), rhs: r#in.get_inner() },
					information.and_then(|info| info.label),
					environment,
					checking_data,
					|environment, checking_data| {
						synthesise_block_or_single_statement(
							&stmt.inner,
							environment,
							checking_data,
						);
					},
					position.with_source(environment.get_source()),
				);
			}
			ForLoopCondition::Statements { initialiser, condition, afterthought, position } => {
				synthesise_iteration(
					IterationBehavior::For {
						initialiser: initialiser.as_ref(),
						condition: condition.as_deref().map(MultipleExpression::get_inner),
						afterthought: afterthought.as_deref().map(MultipleExpression::get_inner),
					},
					information.and_then(|info| info.label),
					environment,
					checking_data,
					|environment, checking_data| {
						crate::utilities::notify!(
							"environment.info.narrowed_values={:?}",
							environment.info.narrowed_values
						);
						synthesise_block_or_single_statement(
							&stmt.inner,
							environment,
							checking_data,
						);
					},
					position.with_source(environment.get_source()),
				);
			}
		},
		StatementOrDeclaration::Block(ref block) => {
			let (_result, _, _) = environment.new_lexical_environment_fold_into_parent(
				Scope::Block {},
				checking_data,
				|environment, checking_data| synthesise_block(&block.0, environment, checking_data),
			);
		}
		StatementOrDeclaration::Continue(label, position) => {
			if let Err(err) = environment.add_continue(label.as_deref(), *position) {
				checking_data
					.diagnostics_container
					.add_error(TypeCheckError::NotInLoopOrCouldNotFindLabel(err));
			}
		}
		StatementOrDeclaration::Break(label, position) => {
			if let Err(err) = environment.add_break(label.as_deref(), *position) {
				checking_data
					.diagnostics_container
					.add_error(TypeCheckError::NotInLoopOrCouldNotFindLabel(err));
			}
		}
		StatementOrDeclaration::Throw(stmt) => {
			let thrown_value = synthesise_multiple_expression(
				&stmt.0,
				environment,
				checking_data,
				TypeId::ANY_TYPE,
			);
			let thrown_position = stmt.1.with_source(environment.get_source());
			environment.throw_value(thrown_value, thrown_position, &mut checking_data.types);
		}
		StatementOrDeclaration::Labelled { position: _, name, statement } => {
			// Labels on invalid statements is caught at parse time
			synthesise_statement_or_declaration(
				&statement.0,
				Some(StatementInformation { label: Some(name.clone()) }),
				environment,
				checking_data,
			);
		}
		StatementOrDeclaration::VarVariable(stmt) => {
			for declaration in &stmt.item.declarations {
				synthesise_variable_declaration_item(
					declaration,
					environment,
					checking_data,
					None,
					false,
				);
			}
		}
		StatementOrDeclaration::TryCatch(stmt) => new_try_context(
			&stmt.try_inner,
			stmt.catch_inner.as_ref().map(|inner| {
				(
					inner,
					stmt.exception_var.as_ref().map(|(var, ty)| (var.get_ast_ref(), ty.as_ref())),
				)
			}),
			stmt.finally_inner.as_ref(),
			environment,
			checking_data,
		),
		// TODO do these higher up in the block. To set relevant information
		StatementOrDeclaration::Comment(s, _) if s.starts_with("@ts") => {
			crate::utilities::notify!("acknowledge '@ts-ignore' and other comments");
		}
		StatementOrDeclaration::MultiLineComment(s, _) if s.starts_with('*') => {
			crate::utilities::notify!("acknowledge '@ts-ignore' and other comments");
		}
		_ => {}
	}
}

/// Expects that this caller has already create a context for this to run in
fn synthesise_block_or_single_statement<T: crate::ReadFromFS>(
	block_or_single_statement: &BlockOrSingleStatement,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	match block_or_single_statement {
		BlockOrSingleStatement::Braced(block) => {
			synthesise_block(&block.0, environment, checking_data);
		}
		BlockOrSingleStatement::SingleStatement(statement) => {
			synthesise_statement_or_declaration(&statement.0, None, environment, checking_data);
		}
	}
}

pub(super) fn synthesise_variable_declaration<T: crate::ReadFromFS>(
	declaration: &VariableDeclaration,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
	exported: bool,
	infer_constraint: bool,
) {
	match declaration.kind {
		VariableDeclarationKeyword::Const => {
			for variable_declaration in &declaration.declarations {
				synthesise_variable_declaration_item(
					variable_declaration,
					environment,
					checking_data,
					exported.then_some(VariableMutability::Constant),
					infer_constraint,
				);
			}
		}
		VariableDeclarationKeyword::Let => {
			for variable_declaration in &declaration.declarations {
				let exported = exported.then(|| {
					let restriction = checking_data
						.local_type_mappings
						.variable_restrictions
						.get(&(environment.get_source(), variable_declaration.position.start))
						.map(|(first, _)| *first);
					VariableMutability::Mutable { reassignment_constraint: restriction }
				});

				synthesise_variable_declaration_item(
					variable_declaration,
					environment,
					checking_data,
					exported,
					infer_constraint,
				);
			}
		}
	}
}

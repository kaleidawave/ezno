use super::{
	expressions::synthesise_multiple_expression, synthesise_block,
	variables::synthesise_variable_declaration_item,
};
use crate::{
	context::Scope,
	diagnostics::TypeCheckError,
	features::{
		conditional::new_conditional_context,
		exceptions::new_try_context,
		iteration::{synthesise_iteration, IterationBehavior},
	},
	synthesis::EznoParser,
	CheckingData, Environment, TypeId,
};

use parser::{expressions::MultipleExpression, ASTNode, BlockOrSingleStatement, Statement};
use std::collections::HashMap;

pub type ExportedItems = HashMap<String, crate::features::variables::VariableOrImport>;
pub type ReturnResult = Option<TypeId>;

pub struct StatementInformation {
	label: Option<String>,
}

pub(super) fn synthesise_statement<T: crate::ReadFromFS>(
	statement: &Statement,
	information: Option<StatementInformation>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) {
	let position = statement.get_position().with_source(environment.get_source());
	match statement {
		Statement::Expression(expression) => {
			synthesise_multiple_expression(
				expression,
				environment,
				checking_data,
				TypeId::ANY_TYPE,
			);
		}
		Statement::Return(return_statement) => {
			environment.return_value(
				&crate::context::environment::Returnable::Statement(
					return_statement.0.as_ref(),
					return_statement.1,
				),
				checking_data,
			);
		}
		Statement::If(if_statement) => {
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

			let others = if_statement
				.else_conditions
				.iter()
				.map(|cond| (&cond.condition, &cond.inner))
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
		Statement::Switch(stmt) => {
			checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
				thing: "Switch statement",
				at: stmt.get_position().with_source(environment.get_source()),
			});
		}
		Statement::WhileLoop(stmt) => synthesise_iteration(
			IterationBehavior::While(&stmt.condition),
			information.and_then(|info| info.label),
			environment,
			checking_data,
			|environment, checking_data| {
				synthesise_block_or_single_statement(&stmt.inner, environment, checking_data);
			},
			position,
		),
		Statement::DoWhileLoop(stmt) => synthesise_iteration(
			IterationBehavior::DoWhile(&stmt.condition),
			information.and_then(|info| info.label),
			environment,
			checking_data,
			|environment, checking_data| {
				synthesise_block_or_single_statement(&stmt.inner, environment, checking_data);
			},
			position,
		),
		Statement::ForLoop(stmt) => match &stmt.condition {
			parser::statements::ForLoopCondition::ForOf {
				is_await: _,
				keyword: _,
				variable,
				of,
				position,
			} => {
				synthesise_iteration(
					IterationBehavior::ForOf { lhs: variable.get_ast_ref(), rhs: of },
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
			parser::statements::ForLoopCondition::ForIn {
				keyword: _,
				variable,
				r#in,
				position,
			} => {
				synthesise_iteration(
					IterationBehavior::ForIn { lhs: variable.get_ast_ref(), rhs: r#in },
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
			parser::statements::ForLoopCondition::Statements {
				initialiser,
				condition,
				afterthought,
				position,
			} => synthesise_iteration(
				IterationBehavior::For { initialiser, condition, afterthought },
				information.and_then(|info| info.label),
				environment,
				checking_data,
				|environment, checking_data| {
					synthesise_block_or_single_statement(&stmt.inner, environment, checking_data);
				},
				position.with_source(environment.get_source()),
			),
		},
		Statement::Block(ref block) => {
			let (_result, _, _) = environment.new_lexical_environment_fold_into_parent(
				Scope::Block {},
				checking_data,
				|environment, checking_data| synthesise_block(&block.0, environment, checking_data),
			);
		}
		Statement::Continue(label, position) => {
			if let Err(err) = environment.add_continue(label.as_deref(), *position) {
				checking_data
					.diagnostics_container
					.add_error(TypeCheckError::NotInLoopOrCouldNotFindLabel(err));
			}
		}
		Statement::Break(label, position) => {
			if let Err(err) = environment.add_break(label.as_deref(), *position) {
				checking_data
					.diagnostics_container
					.add_error(TypeCheckError::NotInLoopOrCouldNotFindLabel(err));
			}
		}
		Statement::Throw(stmt) => {
			let thrown_value = synthesise_multiple_expression(
				&stmt.0,
				environment,
				checking_data,
				TypeId::ANY_TYPE,
			);
			let thrown_position = stmt.1.with_source(environment.get_source());
			environment.throw_value(thrown_value, thrown_position, &mut checking_data.types);
		}
		Statement::Labelled { position: _, name, statement } => {
			// Labels on invalid statements is caught at parse time
			synthesise_statement(
				statement,
				Some(StatementInformation { label: Some(name.clone()) }),
				environment,
				checking_data,
			);
		}
		Statement::VarVariable(stmt) => {
			for declaration in &stmt.declarations {
				synthesise_variable_declaration_item(
					declaration,
					environment,
					checking_data,
					None,
					false,
				);
			}
		}
		Statement::TryCatch(stmt) => new_try_context(
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
		Statement::Comment(s, _) if s.starts_with("@ts") => {
			crate::utilities::notify!("acknowledge '@ts-ignore' and other comments");
		}
		Statement::MultiLineComment(s, _) if s.starts_with('*') => {
			crate::utilities::notify!("acknowledge '@ts-ignore' and other comments");
		}
		Statement::Comment(..)
		| Statement::MultiLineComment(..)
		| Statement::Debugger(_)
		| Statement::Empty(_)
		| Statement::AestheticSemiColon(_) => {}
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
			synthesise_statement(statement, None, environment, checking_data);
		}
	}
}

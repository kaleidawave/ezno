use super::{
	expressions::synthesize_multiple_expression, synthesize_block, variables::register_variable,
};
use crate::{
	context::{ContextId, Scope},
	diagnostics::TypeCheckError,
	events::{Event, RootReference},
	CheckingData, Environment, TypeId, Variable,
};
use parser::{ASTNode, BlockOrSingleStatement, Statement};
use std::collections::HashMap;

pub type ExportedItems = HashMap<String, Variable>;
pub type ReturnResult = Option<TypeId>;

pub(super) fn synthesize_statement<T: crate::FSResolver>(
	statement: &Statement,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
) {
	match statement {
		Statement::Expression(expression) => {
			synthesize_multiple_expression(expression, environment, checking_data);
		}
		// Statement::ExportStatement(_export_statement) => {
		// if let Some(out_exports) = out_exports {
		//     match export_statement {
		//         parser::ExportStatement::Variable { exported, is_default, .. } => {
		//             if *is_default {
		//                 todo!();
		//             }
		//             match exported {
		//                 parser::Exportable::ClassDeclaration(_)
		//                 | parser::Exportable::FunctionDeclaration(_) => panic!(),
		//                 parser::Exportable::VariableDeclaration(variable_decl_stmt) => {
		//                     let is_constant = variable_decl_stmt.is_constant();
		//                     let position = variable_decl_stmt.get_position().cloned();
		//                     for variable_declaration in
		//                         variable_decl_stmt.declarations.iter()
		//                     {
		//                         synthesize_variable_declaration(
		//                             variable_declaration,
		//                             is_constant,
		//                             position.as_ref(),
		//                             &Some(out_exports),
		//                             environment,
		//                             checking_data,
		//
		//                             module_information,
		//                         )?;
		//                     }
		//                 }
		//                 parser::Exportable::InterfaceDeclaration(_) => todo!(),
		//                 parser::Exportable::TypeAlias(_) => todo!(),
		//             }
		//         }
		//     }
		// } else {
		//     checking_data.add_error(
		//         TypeCheckError::NonTopLevelExport,
		//         export_statement.get_position().into(),
		//     )
		// }
		// }
		Statement::Return(_kw, expression) => {
			let returned = if let Some(expression) = expression {
				synthesize_multiple_expression(expression, environment, checking_data)
			} else {
				TypeId::UNDEFINED_TYPE
			};
			environment.return_value(returned);
		}
		Statement::IfStatement(if_statement) => {
			let condition =
				synthesize_multiple_expression(&if_statement.condition, environment, checking_data);

			// TODO extract for ternaries
			// TODO sometimes warning isn't useful for generated stuff

			if let crate::TruthyFalsy::Decidable(value) =
				environment.is_type_truthy_falsy(condition, &checking_data.types)
			{
				checking_data.raise_decidable_result_error(if_statement.position.clone(), value);

				if value {
					synthesize_block_or_single_statement(
						&if_statement.inner,
						environment,
						checking_data,
						Scope::Conditional {},
					);
					return;
				}
			} else {
				// synthesize_statement(&if_statement., environment, checking_data);
			}

			for else_stmt in if_statement.else_conditions.iter() {
				todo!()
			}

			if let Some(ref trailing) = if_statement.trailing_else {
				todo!()
			}

			// if let Some(ref alternative) = if_statement.else_conditions {
			// 	synthesize_statement(alternative, environment, checking_data)
			// }
		}
		Statement::SwitchStatement(_)
		| Statement::WhileStatement(_)
		| Statement::DoWhileStatement(_)
		| Statement::ForLoopStatement(_) => {
			checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
				thing: "switch, while and for statements",
				at: statement.get_position().into_owned(),
			});
			// let mut environment = environment.new_lexical_environment(ScopeType::Conditional {});
			// match &for_statement.condition {
			// 	ForLoopCondition::ForOf { keyword, variable, of } => {
			// 		todo!()
			// 		// let of_expression_instance =
			// 		//     synthesize_expression(of, &mut environment, checking_data);
			// 		// let iterator_result = get_type_iterator_with_error_handler(
			// 		//     of_expression_instance.get_type(),
			// 		//     &mut checking_data.diagnostics_container,
			// 		//     Some(&mut checking_data.type_mappings.implementors_of_generic),
			// 		//     of.get_position(),
			// 		// )
			// 		// .get_iterator_type();
			// 		// synthesize_variable_field(
			// 		//     variable.get_ast_mut(),
			// 		//     &iterator_result,
			// 		//     matches!(keyword, parser::statements::VariableKeyword::Const(_)),
			// 		//     &mut environment,
			// 		//     checking_data,
			// 		//
			// 		//
			// 		// );
			// 	}
			// 	ForLoopCondition::ForIn { keyword, variable: _, in_condition: _ } => todo!(),
			// 	ForLoopCondition::Statements { initializer, condition, final_expression } => {
			// 		match initializer {
			// 			parser::statements::ForLoopStatementInitializer::Statement(statement) => {
			// 				synthesize_variable_declaration_statement(
			// 					statement,
			// 					&mut environment,
			// 					checking_data,
			//
			// 				);
			// 			}
			// 			parser::statements::ForLoopStatementInitializer::Expression(_) => todo!(),
			// 		}
			// 		// synthesize_variable_declaration(
			// 		//     initializer,
			// 		//     *constant,
			// 		//     &mut environment,
			// 		//     checking_data,
			// 		//
			// 		//
			// 		// );
			// 		synthesize_expression(condition, &mut environment, checking_data);
			// 		synthesize_expression(final_expression, &mut environment, checking_data);
			// 	}
			// };
			// synthesize_block(&for_statement.statements, &mut environment, checking_data);
		}
		Statement::Block(ref block) => {
			let (result, _, _) = environment.new_lexical_environment_fold_into_parent(
				Scope::Block {},
				checking_data,
				|environment, checking_data| synthesize_block(&block.0, environment, checking_data),
			);
		}
		Statement::Debugger(_pos) => {
			// yay!
		}
		// TODO acknowledge '@ts-ignore' statements but error
		Statement::Comment(..) | Statement::MultiLineComment(..) => {}
		Statement::Cursor(cursor_id, _) => {
			todo!("Dump environment data somewhere")
		}
		Statement::Continue(..) | Statement::Break(..) => {
			checking_data.raise_unimplemented_error(
				"continue and break statements",
				statement.get_position().into_owned(),
			);
		}
		Statement::Throw(_, value) => {
			let thrown_value = synthesize_multiple_expression(value, environment, checking_data);
			environment.throw_value(thrown_value)
		}
		Statement::Labelled { position, name, statement } => {
			checking_data.raise_unimplemented_error(
				"labelled statements",
				statement.get_position().into_owned(),
			);
			synthesize_statement(statement, environment, checking_data);
		}
		Statement::VarVariable(_) => {
			checking_data.raise_unimplemented_error(
				"var variables statements",
				statement.get_position().into_owned(),
			);
		}
		Statement::TryCatchStatement(stmt) => {
			let throw_type: TypeId =
				environment.new_try_context(checking_data, |environment, checking_data| {
					synthesize_block(&stmt.try_inner.0, environment, checking_data);
				});

			if let Some(ref catch_block) = stmt.catch_inner {
				// TODO catch when never
				environment.new_lexical_environment_fold_into_parent(
					crate::Scope::Block {},
					checking_data,
					|environment, checking_data| {
						if let Some((clause, r#type)) = &stmt.exception_var {
							// TODO clause.type_annotation
							register_variable(
								clause.get_ast(),
								environment,
								checking_data,
								crate::context::VariableRegisterBehavior::CatchVariable {
									ty: throw_type,
								},
								// TODO
								None,
							);
						}
						synthesize_block(&catch_block.0, environment, checking_data);
					},
				);
			}
		}
		Statement::Empty(_) => {}
	}
}

fn synthesize_block_or_single_statement<T: crate::FSResolver>(
	block_or_single_statement: &BlockOrSingleStatement,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	scope: Scope,
) -> ((), Option<(Vec<Event>, HashMap<RootReference, TypeId>)>, ContextId) {
	environment.new_lexical_environment_fold_into_parent(
		scope,
		checking_data,
		|environment, checking_data| match block_or_single_statement {
			BlockOrSingleStatement::Braced(block) => {
				synthesize_block(&block.0, environment, checking_data)
			}
			BlockOrSingleStatement::SingleStatement(statement) => {
				synthesize_statement(statement, environment, checking_data)
			}
		},
	)
}

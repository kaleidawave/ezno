use super::{
	expressions::synthesize_multiple_expression, synthesize_block, variables::register_variable,
};
use crate::{
	context::{ClosedOverReferencesInScope, ContextId, Scope},
	diagnostics::TypeCheckError,
	events::Event,
	CheckingData, Environment, SynthesizableConditional, TypeId, Variable,
};
use parser::{
	statements::{ConditionalElseStatement, UnconditionalElseStatement},
	ASTNode, BlockOrSingleStatement, Statement,
};
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

			environment.new_conditional_context(
				condition,
				IfStatementBranch::Branch(&if_statement.inner),
				if !if_statement.else_conditions.is_empty() || if_statement.trailing_else.is_some()
				{
					Some(IfStatementBranch::NestedConditional {
						conditional_elses: &if_statement.else_conditions,
						unconditional_else: if_statement.trailing_else.as_ref(),
					})
				} else {
					None
				},
				checking_data,
			);

			/// Necessary because the parser treats it as a list rather than a tree
			enum IfStatementBranch<'a> {
				Branch(&'a BlockOrSingleStatement),
				NestedConditional {
					conditional_elses: &'a [ConditionalElseStatement],
					unconditional_else: Option<&'a UnconditionalElseStatement>,
				},
			}

			// TODO tidy
			impl<'a> SynthesizableConditional for IfStatementBranch<'a> {
				type ExpressionResult = ();

				fn synthesize_condition<T: crate::FSResolver>(
					self,
					environment: &mut Environment,
					checking_data: &mut CheckingData<T>,
				) -> Self::ExpressionResult {
					match self {
						IfStatementBranch::Branch(branch) => match branch {
							BlockOrSingleStatement::Braced(statements) => {
								synthesize_block(&statements.0, environment, checking_data)
							}
							BlockOrSingleStatement::SingleStatement(statement) => {
								synthesize_statement(statement, environment, checking_data)
							}
						},
						IfStatementBranch::NestedConditional {
							conditional_elses,
							unconditional_else,
						} => {
							if let [current, other @ ..] = conditional_elses {
								let condition = synthesize_multiple_expression(
									&current.condition,
									environment,
									checking_data,
								);
								environment.new_conditional_context(
									condition,
									IfStatementBranch::Branch(&current.inner),
									if !other.is_empty() || unconditional_else.is_some() {
										Some(IfStatementBranch::NestedConditional {
											conditional_elses: other,
											unconditional_else: unconditional_else.clone(),
										})
									} else {
										None
									},
									checking_data,
								);
							} else {
								match &unconditional_else.unwrap().inner {
									BlockOrSingleStatement::Braced(statements) => {
										synthesize_block(&statements.0, environment, checking_data)
									}
									BlockOrSingleStatement::SingleStatement(statement) => {
										synthesize_statement(statement, environment, checking_data)
									}
								}
							}
						}
					}
				}

				fn conditional_expression_result(
					_: TypeId,
					_: Self::ExpressionResult,
					_: Self::ExpressionResult,
					_: &mut crate::types::TypeStore,
				) -> Self::ExpressionResult {
					()
				}

				fn default_result() -> Self::ExpressionResult {
					()
				}
			}
		}
		Statement::SwitchStatement(stmt) => {
			checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
				thing: "Switch statement",
				at: stmt.get_position().into_owned(),
			});
		}
		Statement::WhileStatement(stmt) => {
			checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
				thing: "While statement",
				at: stmt.get_position().into_owned(),
			});
		}
		Statement::DoWhileStatement(stmt) => {
			checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
				thing: "Do while statement",
				at: stmt.get_position().into_owned(),
			});
		}
		Statement::ForLoopStatement(stmt) => {
			checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
				thing: "For statement",
				at: stmt.get_position().into_owned(),
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
								clause.get_ast_ref(),
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
) -> ((), Option<(Vec<Event>, ClosedOverReferencesInScope)>, ContextId) {
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

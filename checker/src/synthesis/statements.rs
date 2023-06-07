use super::{synthesize_block, synthesize_multiple_expression};
use crate::{
	context::{ContextId, Scope},
	errors::TypeCheckError,
	events::{Event, Reference},
	CheckingData, Environment, TypeId, Variable,
};
use parser::{ASTNode, BlockOrSingleStatement, Chain, ChainVariable, Statement};
use std::collections::HashMap;
use temporary_annex::{Annex, Annexable};

pub type ExportedItems = HashMap<String, Variable>;
pub type ReturnResult = Option<TypeId>;

pub(crate) fn synthesize_statement<T: crate::FSResolver>(
	statement: &mut Statement,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) {
	match statement {
		Statement::Expression(expression) => {
			synthesize_multiple_expression(expression, environment, checking_data, chain);
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
		//                         variable_decl_stmt.declarations.iter_mut()
		//                     {
		//                         synthesize_variable_declaration(
		//                             variable_declaration,
		//                             is_constant,
		//                             position.as_ref(),
		//                             &mut Some(out_exports),
		//                             environment,
		//                             checking_data,
		//                             chain,
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
				synthesize_multiple_expression(expression, environment, checking_data, chain)
			} else {
				TypeId::UNDEFINED_TYPE
			};
			environment.return_value(returned);
		}
		Statement::IfStatement(if_statement) => {
			// TODO is there invalid types for the type of the condition
			let conditional_expression = synthesize_multiple_expression(
				&mut if_statement.condition,
				environment,
				checking_data,
				chain,
			);

			// TODO extract for ternaries
			// TODO sometimes warning isn't useful for generated stuff

			todo!();

			// let (synth_main, synth_elses) = if let Some(constant) =
			// 	c.get_constant_type(conditional_expression)
			// {
			// 	let value = cast_as_boolean(constant, checking_data.settings.strict_casts).unwrap();
			// 	if value {
			// 		checking_data.diagnostics_container.add_warning(
			// 			TypeCheckWarning::UselessExpression {
			// 				expression_span: if_statement.condition.get_position().into_owned(),
			// 			},
			// 		);
			// 	} else {
			// 		checking_data.diagnostics_container.add_warning(TypeCheckWarning::DeadBranch {
			// 			expression_span: if_statement.condition.get_position().into_owned(),
			// 			expression_value: value,
			// 			block_span: if_statement.inner.get_position().into_owned(),
			// 		});
			// 	}
			// 	(value, !value)
			// } else {
			// 	(true, true)
			// };

			// if synth_main {
			// 	crate::utils::notify!("TODO Proofs from statement");
			// 	let (res, events, _) = synthesize_block_or_single_statement(
			// 		&mut if_statement.inner,
			// 		environment,
			// 		checking_data,
			// 		chain,
			// 		Scope::Conditional { },
			// 	);
			// }

			// if synth_elses {
			// 	for else_condition in if_statement.else_conditions.iter() {
			// 		todo!()
			// 	}

			// 	if let Some(trailing) = &if_statement.trailing_else {
			// 		todo!()
			// 	}

			// 	// TODO could narrow the type of a variable in the environment if the condition is instanceof or typeof
			// 	// for condition_else in if_statement.else_conditions.iter_mut() {
			// 	// 	todo!("Expression is conditional")
			// 	// 	// synthesize_expression(
			// 	// 	// 	&mut condition_else.condition,
			// 	// 	// 	environment,
			// 	// 	// 	checking_data,
			// 	// 	// 	chain,
			// 	// 	// );
			// 	// 	// crate::utils::notify!("TODO inject proofs");
			// 	// 	// let (res, events) = synthesize_block_or_single_statement(
			// 	// 	// 	&mut condition_else.statements,
			// 	// 	// 	environment,
			// 	// 	// 	checking_data,
			// 	// 	// 	chain,
			// 	// 	// 	Scope::Conditional {  }
			// 	// 	// );
			// 	// }
			// 	// if let Some(ref mut trailing_else) = if_statement.trailing_else {
			// 	// 	let (res, events) = synthesize_block_or_single_statement(
			// 	// 		&mut trailing_else.statements,
			// 	// 		environment,
			// 	// 		checking_data,
			// 	// 		chain,
			// 	// 		Scope::Conditional {  }
			// 	// 	);
			// 	// }
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
			// match &mut for_statement.condition {
			// 	ForLoopCondition::ForOf { keyword, variable, of } => {
			// 		todo!()
			// 		// let of_expression_instance =
			// 		//     synthesize_expression(of, &mut environment, checking_data, chain);
			// 		// let iterator_result = get_type_iterator_with_error_handler(
			// 		//     of_expression_instance.get_type(),
			// 		//     &mut checking_data.error_warning_info_handler,
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
			// 		//     chain,
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
			// 					chain,
			// 				);
			// 			}
			// 			parser::statements::ForLoopStatementInitializer::Expression(_) => todo!(),
			// 		}
			// 		// synthesize_variable_declaration(
			// 		//     initializer,
			// 		//     *constant,
			// 		//     &mut environment,
			// 		//     checking_data,
			// 		//     chain,
			// 		//
			// 		// );
			// 		synthesize_expression(condition, &mut environment, checking_data, chain);
			// 		synthesize_expression(final_expression, &mut environment, checking_data, chain);
			// 	}
			// };
			// synthesize_block(&mut for_statement.statements, &mut environment, checking_data, chain);
		}
		Statement::Block(ref mut block) => {
			let (result, _, _) = environment.new_lexical_environment_fold_into_parent(
				Scope::Block {},
				checking_data,
				|environment, checking_data| {
					synthesize_block(
						block,
						environment,
						checking_data,
						&mut chain.push_annex(ChainVariable::Block(block.1)),
					)
				},
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
			checking_data.diagnostics_container.add_error(TypeCheckError::Unsupported {
				thing: "continue and break statements",
				at: statement.get_position().into_owned(),
			});
		}
		Statement::Throw(_, _) => todo!(),
		Statement::Labelled { position, name, statement } => todo!(),
		Statement::VarVariable(_) => todo!(),
	}
}

fn synthesize_block_or_single_statement<T: crate::FSResolver>(
	block_or_single_statement: &mut BlockOrSingleStatement,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
	scope: Scope,
) -> ((), Option<(Vec<Event>, HashMap<Reference, TypeId>)>, ContextId) {
	environment.new_lexical_environment_fold_into_parent(
		scope,
		checking_data,
		|environment, checking_data| match block_or_single_statement {
			BlockOrSingleStatement::Braced(_) => todo!(),
			BlockOrSingleStatement::SingleStatement(statement) => {
				synthesize_statement(statement, environment, checking_data, chain)
			}
		},
	)
}

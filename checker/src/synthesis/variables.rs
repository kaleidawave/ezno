use parser::{
	declarations::VariableDeclarationItem, ASTNode, ArrayDestructuringField, Chain,
	ObjectDestructuringField, VariableField, VariableFieldInSourceCode, VariableIdentifier,
};
use temporary_annex::Annex;

use crate::{
	behavior::variables::check_variable_initialization, context::Logical, types::Constant,
	CheckingData, Environment, TypeId,
};

use super::expressions::synthesize_expression;

pub(super) fn hoist_variable_identifier(
	identifier: &VariableIdentifier,
	environment: &mut Environment,
	is_constant: bool,
) {
	if let VariableIdentifier::Standard(name, variable_id, pos) = identifier {
		todo!()
		// environment.variables.insert(
		// 	name.clone(),
		// 	crate::Variable {
		// 		is_constant,
		// 		// TODO done later
		// 		reassignment_constraint: None,
		// 		id: *variable_id,
		// 		declared_at: pos.clone(),
		// 	},
		// );
	}
}

pub(crate) fn hoist_variable_declaration(
	field: &VariableField<VariableFieldInSourceCode>,
	environment: &mut Environment,
	is_constant: bool,
) {
	match field {
		VariableField::Name(identifier) => {
			hoist_variable_identifier(&identifier, environment, is_constant);
		}
		VariableField::Array(_, _) => todo!(),
		VariableField::Object(_, _) => todo!(),
	}
}

/// Assigns values in the variable field into the environment. Will also synthesize default values
pub(crate) fn synthesize_variable_field<T: crate::FSResolver>(
	variable_field: &mut VariableField<parser::VariableFieldInSourceCode>,
	variable_type: Option<TypeId>,
	expression_type: TypeId,
	is_constant: bool,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) {
	fn declare_variable_in_environment<T: crate::FSResolver>(
		environment: &mut Environment,
		name: &VariableIdentifier,
		variable_type: Option<TypeId>,
		expression_type: TypeId,
		is_constant: bool,
		checking_data: &mut CheckingData<T>,
	) {
		// todo!();
		let VariableIdentifier::Standard(name, variable_id, position) = name else { todo!("cursor, maybe skip?") };
		let value = environment.variable_current_value.insert(*variable_id, expression_type);
		debug_assert!(value.is_none());
	}

	match variable_field {
		VariableField::Name(name) => {
			declare_variable_in_environment(
				environment,
				name,
				variable_type,
				expression_type,
				is_constant,
				checking_data,
			);
		}
		VariableField::Object(members, _) => {
			// TODO keep track of destructured properties for spread
			for member in members.iter_mut() {
				match member.get_ast_mut() {
					ObjectDestructuringField::Name(name, default_value) => {
						if let Some(default_value) = default_value {
							synthesize_expression(default_value, environment, checking_data, chain);
						}

						let VariableIdentifier::Standard(prop_name, _, _) = &name else { todo!() };

						let property = checking_data
							.types
							.new_constant_type(Constant::String(prop_name.clone()));

						let referencing_property: TypeId = environment
							.get_property(expression_type, property, checking_data, None)
							.unwrap()
							.into();

						let variable_constraint = variable_type.map(|variable_type| {
							environment
								.get_property_unbound(variable_type, property, &checking_data.types)
								.map(Logical::to_type)
								.expect("Pulling from object whose constraint doesn't match")
						});

						declare_variable_in_environment(
							environment,
							name,
							variable_constraint,
							referencing_property,
							is_constant,
							checking_data,
						);
					}
					ObjectDestructuringField::Map {
						from, variable_name, default_value, ..
					} => {
						if let Some(default_value) = default_value {
							synthesize_expression(default_value, environment, checking_data, chain);
						}
						todo!()
						// synthesize_variable_field(
						// 	variable_field.get_ast_mut(),
						// 	None,
						// 	referencing_property,
						// 	is_constant,
						// 	environment,
						// 	checking_data,
						// 	chain,
						// )
					}
					ObjectDestructuringField::Spread(..) => unimplemented!(),
				};
			}
		}
		VariableField::Array(members, _) => {
			for (index, member) in members.iter_mut().enumerate() {
				match member {
					ArrayDestructuringField::Spread(_, id) => todo!(),
					ArrayDestructuringField::Name(variable_field, _) => {
						let property = checking_data.types.new_constant_type(Constant::Number(
							(index as f64).try_into().unwrap(),
						));

						let referencing_property: TypeId = environment
							.get_property(expression_type, property, checking_data, None)
							.unwrap()
							.into();

						synthesize_variable_field(
							variable_field.get_ast_mut(),
							None,
							referencing_property,
							is_constant,
							environment,
							checking_data,
							chain,
						)
					}
					ArrayDestructuringField::None => todo!(),
				}
			}
		}
	}
}

/// Name already been hoisted,
pub(crate) fn synthesize_variable_declaration_item<
	T: crate::FSResolver,
	U: parser::ast::variable::DeclarationExpression,
>(
	variable_declaration: &mut VariableDeclarationItem<U>,
	environment: &mut Environment,
	is_constant: bool,
	checking_data: &mut CheckingData<T>,
	chain: &mut Annex<Chain>,
) {
	let variable_declared_type = if let Some(ref type_ref) = variable_declaration.type_reference {
		// TODO should assign the value here for checking later...?
		Some((
			environment.get_type_handle_errors(type_ref, checking_data),
			type_ref.get_position().clone(),
		))
	} else {
		None
	};

	// For TSC compat, let declarations without an expression shouldn't be checked with undefined. Thus the
	// following only happens in the first branch

	let expression_type = if let Some(expression) =
		variable_declaration.expression.as_option_mut_expr()
	{
		let expression_type = synthesize_expression(expression, environment, checking_data, chain);

		if let Some(variable_declared_type) = variable_declared_type.clone() {
			check_variable_initialization(
				variable_declared_type,
				(expression_type, expression.get_position()),
				environment,
				checking_data,
			);
		}

		expression_type
	} else {
		crate::utils::notify!("Expression without value, thing undefined is okay here");

		TypeId::UNDEFINED_TYPE
	};

	synthesize_variable_field(
		variable_declaration.name.get_ast_mut(),
		variable_declared_type.map(|(ty, _)| ty.clone()),
		expression_type,
		is_constant,
		environment,
		checking_data,
		chain,
	)
}

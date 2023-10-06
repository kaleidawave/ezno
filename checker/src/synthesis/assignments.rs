use ordered_float::NotNan;
use parser::{
	ast::LHSOfAssignment, expressions::assignments::VariableOrPropertyAccess, ASTNode,
	VariableIdentifier,
};

use crate::{
	behavior::assignments::{Assignable, Reference},
	context::Environment,
	synthesis::expressions::synthesise_expression,
	types::Constant,
	CheckingData, TypeId,
};

use super::{expressions::synthesise_multiple_expression, property_key_as_type};

pub(super) fn synthesise_lhs_of_assignment_to_reference<T: crate::FSResolver>(
	lhs: &LHSOfAssignment,
	environment: &mut Environment,
	checking_data: &mut CheckingData<'_, T, parser::Module>,
) -> Assignable {
	match lhs {
		LHSOfAssignment::ObjectDestructuring(items, _) => Assignable::ObjectDestructuring(
			items
				.iter()
				.map(|item| match item.get_ast_ref() {
					parser::ObjectDestructuringField::Name(name, _, _) => {
						let on = checking_data.types.new_constant_type(Constant::String(
							if let VariableIdentifier::Standard(name, _) = name {
								name.clone()
							} else {
								todo!()
							},
						));
						(
							on,
							synthesise_object_shorthand_assignable(
								name,
								checking_data,
								environment,
							),
						)
					}
					parser::ObjectDestructuringField::Spread(_, _) => todo!(),
					parser::ObjectDestructuringField::Map {
						from,
						name,
						default_value,
						position,
					} => {
						// TODO into function
						match name.get_ast_ref() {
							parser::VariableField::Name(name) => {
								let on = property_key_as_type(
									from,
									environment,
									&mut checking_data.types,
								);
								let a = synthesise_object_shorthand_assignable(
									name,
									checking_data,
									environment,
								);
								(on, a)
							}
							parser::VariableField::Array(_, _) => todo!(),
							parser::VariableField::Object(_, _) => todo!(),
						}
					}
				})
				.collect(),
		),
		LHSOfAssignment::ArrayDestructuring(items, _) => Assignable::ArrayDestructuring(
			items
				.iter()
				.map(|item| match item {
					parser::ArrayDestructuringField::Spread(_, _) => todo!(),
					parser::ArrayDestructuringField::Name(name, _) => match name.get_ast_ref() {
						parser::VariableField::Name(name) => {
							Some(synthesise_object_shorthand_assignable(
								name,
								checking_data,
								environment,
							))
						}
						parser::VariableField::Array(_, _) => todo!(),
						parser::VariableField::Object(_, _) => todo!(),
					},
					parser::ArrayDestructuringField::None => None,
				})
				.collect(),
		),
		LHSOfAssignment::VariableOrPropertyAccess(access) => Assignable::Reference(
			synthesise_access_to_reference(access, environment, checking_data),
		),
	}
}

fn synthesise_object_shorthand_assignable<T: crate::FSResolver>(
	name: &parser::VariableIdentifier,
	checking_data: &mut CheckingData<'_, T, parser::Module>,
	environment: &mut crate::context::Context<crate::context::Syntax<'_>>,
) -> Assignable {
	match name {
		parser::VariableIdentifier::Standard(name, pos) => Assignable::Reference(
			Reference::Variable(name.clone(), pos.clone().with_source(environment.get_source())),
		),
		parser::VariableIdentifier::Cursor(..) => todo!(),
	}
}

pub(crate) fn synthesise_access_to_reference<T: crate::FSResolver>(
	variable_or_property_access: &VariableOrPropertyAccess,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, parser::Module>,
) -> Reference {
	match variable_or_property_access {
		VariableOrPropertyAccess::Variable(ident, position) => Reference::Variable(
			ident.clone(),
			position.clone().with_source(environment.get_source()),
		),
		VariableOrPropertyAccess::PropertyAccess { parent, property, position } => {
			let parent_ty = synthesise_expression(&parent, environment, checking_data);
			let key_ty = match property {
				parser::PropertyReference::Standard(prop) => {
					checking_data.types.new_constant_type(Constant::String(prop.clone()))
				}
				parser::PropertyReference::Cursor(_) => todo!(),
			};
			Reference::Property {
				on: parent_ty,
				with: key_ty,
				span: position.clone().with_source(environment.get_source()),
			}
		}
		VariableOrPropertyAccess::Index { indexee, indexer, position } => {
			let parent_ty = synthesise_expression(&indexee, environment, checking_data);
			let key_ty = synthesise_multiple_expression(&indexer, environment, checking_data);
			Reference::Property {
				on: parent_ty,
				with: key_ty,
				span: position.clone().with_source(environment.get_source()),
			}
		}
	}
}

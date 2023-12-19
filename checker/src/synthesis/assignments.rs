use std::borrow::Cow;

use parser::{
	ast::LHSOfAssignment, expressions::assignments::VariableOrPropertyAccess, VariableIdentifier,
};

use crate::{
	behavior::assignments::{Assignable, Reference},
	context::{facts::Publicity, Environment},
	synthesis::expressions::synthesise_expression,
	types::properties::PropertyKey,
	CheckingData, TypeId,
};

use super::{
	expressions::synthesise_multiple_expression, parser_property_key_to_checker_property_key,
};

pub(super) fn synthesise_lhs_of_assignment_to_reference<T: crate::ReadFromFS>(
	lhs: &LHSOfAssignment,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> Assignable {
	match lhs {
		LHSOfAssignment::ObjectDestructuring(items, _) => Assignable::ObjectDestructuring(
			items
				.iter()
				.map(|item| match item.get_ast_ref() {
					parser::ObjectDestructuringField::Name(name, _, _) => {
						let on = if let VariableIdentifier::Standard(name, _) = name {
							PropertyKey::String(Cow::Owned(name.clone()))
						} else {
							todo!()
						};
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
						default_value: _,
						position: _,
					} => {
						// TODO into function
						match name.get_ast_ref() {
							parser::VariableField::Name(name) => {
								let on = parser_property_key_to_checker_property_key(
									from,
									environment,
									checking_data,
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

fn synthesise_object_shorthand_assignable<T: crate::ReadFromFS>(
	name: &parser::VariableIdentifier,
	_checking_data: &CheckingData<T, super::EznoParser>,
	environment: &crate::context::Context<crate::context::Syntax<'_>>,
) -> Assignable {
	match name {
		parser::VariableIdentifier::Standard(name, pos) => Assignable::Reference(
			Reference::Variable(name.clone(), pos.with_source(environment.get_source())),
		),
		parser::VariableIdentifier::Cursor(..) => todo!(),
	}
}

pub(crate) fn synthesise_access_to_reference<T: crate::ReadFromFS>(
	variable_or_property_access: &VariableOrPropertyAccess,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> Reference {
	match variable_or_property_access {
		VariableOrPropertyAccess::Variable(ident, position) => {
			Reference::Variable(ident.clone(), position.with_source(environment.get_source()))
		}
		VariableOrPropertyAccess::PropertyAccess { parent, property, position } => {
			let parent_ty =
				synthesise_expression(parent, environment, checking_data, TypeId::ANY_TYPE);
			match property {
				parser::PropertyReference::Standard { property, is_private } => {
					let publicity =
						if *is_private { Publicity::Private } else { Publicity::Public };
					Reference::Property {
						on: parent_ty,
						with: crate::types::properties::PropertyKey::String(Cow::Owned(
							property.clone(),
						)),
						span: position.with_source(environment.get_source()),
						publicity,
					}
				}
				parser::PropertyReference::Cursor(_) => todo!(),
			}
		}
		VariableOrPropertyAccess::Index { indexee, indexer, position } => {
			let parent_ty =
				synthesise_expression(indexee, environment, checking_data, TypeId::ANY_TYPE);
			let key_ty = synthesise_multiple_expression(
				indexer,
				environment,
				checking_data,
				TypeId::ANY_TYPE,
			);
			Reference::Property {
				on: parent_ty,
				with: crate::types::properties::PropertyKey::from_type(
					key_ty,
					&checking_data.types,
				),
				span: position.with_source(environment.get_source()),
				publicity: crate::context::facts::Publicity::Public,
			}
		}
	}
}

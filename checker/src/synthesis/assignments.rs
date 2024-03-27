use std::borrow::Cow;

use parser::{
	ast::LHSOfAssignment, expressions::assignments::VariableOrPropertyAccess, VariableField,
	VariableIdentifier,
};

use crate::{
	context::{information::Publicity, Environment},
	features::assignments::{
		Assignable, AssignableArrayDestructuringField, AssignableObjectDestructuringField,
		Reference,
	},
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
		LHSOfAssignment::ObjectDestructuring(items, _) => {
			synthesise_object_to_reference(items, environment, checking_data)
		}
		LHSOfAssignment::ArrayDestructuring(items, _) => {
			synthesise_array_to_reference(items, environment, checking_data)
		}
		LHSOfAssignment::VariableOrPropertyAccess(access) => Assignable::Reference(
			synthesise_access_to_reference(access, environment, checking_data),
		),
	}
}

fn synthesise_variable_field_to_reference<T: crate::ReadFromFS>(
	variable_field: &VariableField,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> Assignable {
	match variable_field {
		VariableField::Object(items, _) => {
			synthesise_object_to_reference(items, environment, checking_data)
		}
		VariableField::Array(items, _) => {
			synthesise_array_to_reference(items, environment, checking_data)
		}
		VariableField::Name(ident) => Assignable::Reference(match ident {
			VariableIdentifier::Standard(name, position) => {
				Reference::Variable(name.clone(), position.with_source(environment.get_source()))
			}
			VariableIdentifier::Marker(_, _) => todo!(),
		}),
	}
}

fn synthesise_object_to_reference<T: crate::ReadFromFS>(
	items: &Vec<parser::WithComment<parser::ObjectDestructuringField>>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> Assignable {
	Assignable::ObjectDestructuring(
		items
			.iter()
			.map(|item| match item.get_ast_ref() {
				parser::ObjectDestructuringField::Name(name, default_value, position) => {
					AssignableObjectDestructuringField::Mapped {
						on: synthesise_object_property_key(name, &environment),
						name: synthesise_object_shorthand_assignable(
							name,
							checking_data,
							environment,
						),
						default_value: default_value.clone(),
						position: position.with_source(environment.get_source()),
					}
				}
				parser::ObjectDestructuringField::Spread(name, position) => {
					AssignableObjectDestructuringField::Spread(
						name.clone(),
						position.with_source(environment.get_source()),
					)
				}
				parser::ObjectDestructuringField::Map { from, name, default_value, position } => {
					let on = parser_property_key_to_checker_property_key(
						from,
						environment,
						checking_data,
						true,
					);

					AssignableObjectDestructuringField::Mapped {
						on,
						name: synthesise_variable_field_to_reference(
							name.get_ast_ref(),
							environment,
							checking_data,
						),
						default_value: default_value.clone(),
						position: position.with_source(environment.get_source()),
					}
				}
			})
			.collect(),
	)
}

fn synthesise_array_to_reference<T: crate::ReadFromFS>(
	items: &Vec<parser::WithComment<parser::ArrayDestructuringField>>,
	environment: &mut Environment,
	checking_data: &mut CheckingData<T, super::EznoParser>,
) -> Assignable {
	Assignable::ArrayDestructuring(
		items
			.iter()
			.map(|item| match item.get_ast_ref() {
				parser::ArrayDestructuringField::Spread(name, position) => {
					AssignableArrayDestructuringField::Spread(
						synthesise_variable_field_to_reference(name, environment, checking_data),
						position.with_source(environment.get_source()),
					)
				}
				parser::ArrayDestructuringField::Name(name, default_value) => {
					AssignableArrayDestructuringField::Name(
						synthesise_variable_field_to_reference(name, environment, checking_data),
						default_value.clone(),
					)
				}
				parser::ArrayDestructuringField::Comment { content, is_multiline, position } => {
					AssignableArrayDestructuringField::Comment {
						content: content.clone(),
						is_multiline: *is_multiline,
						position: position.with_source(environment.get_source()),
					}
				}
				parser::ArrayDestructuringField::None => AssignableArrayDestructuringField::None,
			})
			.collect(),
	)
}

fn synthesise_object_shorthand_assignable<T: crate::ReadFromFS>(
	name: &parser::VariableIdentifier,
	_checking_data: &CheckingData<T, super::EznoParser>,
	environment: &Environment,
) -> Assignable {
	match name {
		parser::VariableIdentifier::Standard(name, pos) => Assignable::Reference(
			Reference::Variable(name.clone(), pos.with_source(environment.get_source())),
		),
		parser::VariableIdentifier::Marker(..) => todo!(),
	}
}

fn synthesise_object_property_key(
	name: &parser::VariableIdentifier,
	environment: &Environment,
) -> PropertyKey<'static> {
	match name {
		parser::VariableIdentifier::Standard(name, pos) => {
			PropertyKey::String(Cow::Owned(name.to_owned()))
		}
		parser::VariableIdentifier::Marker(..) => todo!(),
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
				parser::PropertyReference::Marker(_) => todo!(),
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
				publicity: crate::context::information::Publicity::Public,
			}
		}
	}
}

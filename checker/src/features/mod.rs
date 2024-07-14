//! Contains implementations of specific JavaScript items and how Ezno handles them.
//! Contains
//! - Helper / abstracting functions for synthesising
//!
//! Does not contain
//! - Logic stuff
//! - Context
//! - Internal structures

pub mod assignments;
pub mod conditional;
pub mod constant_functions;
pub mod exceptions;
pub mod functions;
pub mod iteration;
pub mod modules;
pub mod objects;
pub mod operations;
pub mod template_literal;
pub mod variables;

use source_map::SpanWithSource;

use crate::{
	context::{get_value_of_variable, information::InformationChain, ClosedOverReferencesInScope},
	diagnostics::TypeStringRepresentation,
	events::RootReference,
	features::functions::ClosedOverVariables,
	types::{get_constraint, properties, PartiallyAppliedGenerics, TypeStore},
	CheckingData, Environment, Logical, PropertyValue, Type, TypeId,
};

use self::objects::SpecialObject;

/// Returns result of `typeof *on*`
pub fn type_of_operator(on: TypeId, types: &mut TypeStore) -> TypeId {
	if let Some(constraint) = get_constraint(on, types) {
		let name = match constraint {
			TypeId::NUMBER_TYPE => "number",
			TypeId::STRING_TYPE => "string",
			TypeId::BOOLEAN_TYPE => "boolean",
			TypeId::SYMBOL_TYPE => "symbol",
			_constraint => {
				return types.register_type(crate::Type::Constructor(
					crate::types::Constructor::TypeOperator(crate::types::TypeOperator::TypeOf(on)),
				))
			}
		};
		// TODO could Cow or something to not allocate?
		types.new_constant_type(crate::Constant::String(name.to_owned()))
	} else if on == TypeId::UNDEFINED_TYPE {
		return types.new_constant_type(crate::Constant::String("undefined".to_owned()));
	} else if on == TypeId::UNDEFINED_TYPE {
		return types.new_constant_type(crate::Constant::String("null".to_owned()));
	} else {
		let ty = types.get_type_by_id(on);
		if let crate::Type::Constant(cst) = ty {
			// TODO backing type
			let name = match cst {
				crate::Constant::NaN | crate::Constant::Number(_) => "number",
				crate::Constant::String(_) => "string",
				crate::Constant::Boolean(_) => "boolean",
				crate::Constant::Symbol { key: _ } => "symbol",
			};
			// TODO could Cow or something to not allocate?
			types.new_constant_type(crate::Constant::String(name.to_owned()))
		} else if let crate::Type::SpecialObject(SpecialObject::Function(..)) = ty {
			types.new_constant_type(crate::Constant::String("function".to_owned()))
		} else if let crate::Type::Object(..) | crate::Type::SpecialObject(..) = ty {
			types.new_constant_type(crate::Constant::String("object".to_owned()))
		} else {
			crate::utilities::notify!("Cannot `typeof {:?}`", on);
			TypeId::ERROR_TYPE
		}
	}
}

// TODO think this is okay
fn extends_prototype(lhs: TypeId, rhs: TypeId, information: &impl InformationChain) -> bool {
	for info in information.get_chain_of_info() {
		if let Some(lhs_prototype) = info.prototypes.get(&lhs).copied() {
			let prototypes_equal = lhs_prototype == rhs;
			crate::utilities::notify!("{:?} and {:?}", lhs_prototype, rhs);
			return if prototypes_equal {
				true
			} else {
				extends_prototype(lhs_prototype, rhs, information)
			};
		}
	}
	false
}

pub fn instance_of_operator(
	lhs: TypeId,
	rhs: TypeId,
	information: &impl InformationChain,
	types: &mut TypeStore,
) -> TypeId {
	// TODO frozen prototypes
	if let Some(_constraint) = get_constraint(lhs, types) {
		todo!()
	} else {
		let rhs_prototype =
			if let Type::SpecialObject(SpecialObject::ClassConstructor { prototype, .. }) =
				types.get_type_by_id(rhs)
			{
				*prototype
			} else {
				// TODO err
				rhs
			};

		if extends_prototype(lhs, rhs_prototype, information) {
			TypeId::TRUE
		} else {
			TypeId::FALSE
		}
	}
}

/// Returns result of `*on* as *cast_to*`. Returns `Err(())` for invalid casts where invalid casts
/// occur for casting a constant
pub fn as_cast(on: TypeId, cast_to: TypeId, types: &mut TypeStore) -> Result<TypeId, ()> {
	use crate::types::{Constructor, PolyNature};

	fn can_cast_type(ty: &Type) -> bool {
		match ty {
			// TODO some of these are more correct than the others
			crate::Type::RootPolyType(_rpt) => true,
			crate::Type::Constructor(constr) => match constr {
				Constructor::CanonicalRelationOperator { .. }
				| Constructor::UnaryOperator { .. }
				| Constructor::BinaryOperator { .. } => false,
				Constructor::TypeOperator(_) => todo!(),
				Constructor::TypeRelationOperator(_) => todo!(),
				Constructor::Awaited { .. }
				| Constructor::KeyOf(..)
				| Constructor::ConditionalResult { .. }
				| Constructor::Image { .. }
				| Constructor::Property { .. } => true,
			},
			_ => false,
		}
	}

	let can_cast = on == TypeId::ERROR_TYPE || can_cast_type(types.get_type_by_id(on));

	if can_cast {
		// TSC compat around `any`
		let cast_to = if cast_to == TypeId::ANY_TYPE { TypeId::ERROR_TYPE } else { cast_to };

		Ok(types.register_type(Type::RootPolyType(PolyNature::Open(cast_to))))
	} else {
		Err(())
	}
}

/// Return `await *on*`. TODO await [`Type::Or`] etc
pub fn await_expression<T: crate::ReadFromFS, A: crate::ASTImplementation>(
	on: TypeId,
	_environment: &mut Environment,
	checking_data: &mut CheckingData<T, A>,
	position: SpanWithSource,
) -> TypeId {
	if let Some(constraint) = get_constraint(on, &checking_data.types) {
		// TODO mark type as awaited
		let inner_type = get_promise_value(constraint, &checking_data.types);
		if let Some(result) = inner_type {
			crate::utilities::notify!("Queue await effect");
			checking_data
				.types
				.register_type(Type::Constructor(crate::types::Constructor::Awaited { on, result }))
		} else {
			crate::utilities::notify!(
				"Await on {:?}, got {:?}",
				checking_data.types.get_type_by_id(on),
				checking_data.types.get_type_by_id(constraint)
			);
			checking_data.raise_unimplemented_error(
				"await has no effect (or awaited expression is more complex)",
				position,
			);
			on
		}
	} else {
		checking_data.raise_unimplemented_error("await on object", position);
		TypeId::ERROR_TYPE
	}
}

fn get_promise_value(constraint: TypeId, types: &TypeStore) -> Option<TypeId> {
	if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
		on: TypeId::PROMISE_TYPE,
		arguments,
	}) = types.get_type_by_id(constraint)
	{
		let result = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
		Some(result)
	} else {
		None
	}
}

/// For function synthesis and
pub(crate) fn create_closed_over_references(
	closed_over_references: &ClosedOverReferencesInScope,
	current_environment: &Environment,
) -> ClosedOverVariables {
	ClosedOverVariables(
		closed_over_references
			.iter()
			.map(|reference| {
				match reference {
					RootReference::Variable(on) => {
						let c = None::<
							&crate::types::generics::substitution::SubstitutionArguments<'static>,
						>;
						let get_value_of_variable =
							get_value_of_variable(current_environment, *on, c);
						let ty = if let Some(value) = get_value_of_variable {
							value
						} else {
							// TODO think we are getting rid of this
							// let name = function_environment.get_variable_name(*on);
							// checking_data.diagnostics_container.add_error(
							// 	TypeCheckError::UnreachableVariableClosedOver(
							// 		name.to_string(),
							// 		function
							// 			.get_position()
							// 			.with_source(base_environment.get_source()),
							// 	),
							// );

							// `TypeId::ERROR_TYPE` is also okay
							TypeId::NEVER_TYPE
						};
						(*on, ty)
					}
					// TODO unsure
					RootReference::This => todo!(),
				}
			})
			.collect(),
	)
}

pub struct CannotDeleteFromError {
	pub constraint: TypeStringRepresentation,
	pub position: SpanWithSource,
}

/// WIP
pub fn delete_operator(
	(publicity, under): (properties::Publicity, properties::PropertyKey<'_>),
	rhs: TypeId,
	position: SpanWithSource,
	environment: &mut Environment,
	types: &mut TypeStore,
) -> Result<TypeId, CannotDeleteFromError> {
	crate::utilities::notify!("Queue event");

	let existing = properties::has_property((publicity, &under), rhs, environment, types);

	{
		let constraint =
			environment.get_object_constraint(rhs).or_else(|| get_constraint(rhs, types));

		if let Some(constraint) = constraint {
			let constraint_type = types.get_type_by_id(constraint);
			crate::utilities::notify!("constraint={:?}", constraint_type);

			if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
				on: TypeId::READONLY_RESTRICTION,
				..
			}) = constraint_type
			{
				let constraint =
					TypeStringRepresentation::from_type_id(constraint, environment, types, false);
				return Err(CannotDeleteFromError { constraint, position });
			}

			// Array indices deletion currently broken
			let skip = constraint == TypeId::ARRAY_TYPE
				|| matches!(
					constraint_type,
					Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
						on: TypeId::ARRAY_TYPE,
						..
					})
				) || {
				let get_prototype = environment.get_prototype(constraint);
				crate::utilities::notify!("{:?}", get_prototype);
				get_prototype == TypeId::ARRAY_TYPE
			};

			if !skip {
				let property_constraint = properties::get_property_unbound(
					(constraint, None),
					(publicity, &under, None),
					environment,
					types,
				);

				if let Ok(property_constraint) = property_constraint {
					crate::utilities::notify!("property_constraint {:?}", property_constraint);
					match property_constraint {
						Logical::Pure(n) => match n {
							PropertyValue::Value(_)
							| PropertyValue::Getter(_)
							| PropertyValue::Setter(_) => {
								crate::utilities::notify!(
									"Cannot delete property because of constraint"
								);
								let constraint = TypeStringRepresentation::from_type_id(
									constraint,
									environment,
									types,
									false,
								);
								return Err(CannotDeleteFromError { constraint, position });
							}
							PropertyValue::Deleted => {
								crate::utilities::notify!("Here?");
							}
							PropertyValue::ConditionallyExists { .. } => {
								crate::utilities::notify!("OKAY!!!");
							}
							PropertyValue::Configured { on: _, descriptor } => {
								crate::utilities::notify!("descriptor={:?}", descriptor);
							}
						},
						Logical::Or { .. } => todo!(),
						Logical::Implies { .. } => todo!(),
						Logical::BasedOnKey { .. } => todo!(),
					}
				}
			}
		}
	}

	// crate::utilities::notify!("Property constraint .is_some() {:?}", property_constraint.is_some());

	// crate::utilities::notify!(
	// 	"Re-assignment constraint {}, prop={} {:?}",
	// 	print_type(constraint, types, environment, true),
	// 	print_type(under, types, environment, true),
	// 	property_constraint
	// );

	// TODO not great
	let dependency = if get_constraint(rhs, types).is_some() {
		Some(types.register_type(Type::Constructor(crate::types::Constructor::TypeOperator(
			crate::types::TypeOperator::HasProperty(rhs, under.into_owned()),
		))))
	} else {
		None
	};

	environment.info.delete_property(rhs, (publicity, under.into_owned()), position, dependency);

	Ok(dependency.unwrap_or(existing))
}

pub fn in_operator(
	(publicity, under): (properties::Publicity, &properties::PropertyKey<'_>),
	rhs: TypeId,
	environment: &mut Environment,
	types: &mut TypeStore,
) -> TypeId {
	let result = properties::has_property((publicity, under), rhs, environment, types);

	// TODO if any
	if get_constraint(rhs, types).is_some() {
		let dependency =
			types.register_type(Type::Constructor(crate::types::Constructor::TypeOperator(
				crate::types::TypeOperator::HasProperty(rhs, under.into_owned()),
			)));

		{
			let ty = crate::types::printing::print_type(result, types, environment, true);
			crate::utilities::notify!("ty={:?}, dependency={:?}", ty, dependency);
		}

		environment.info.events.push(crate::events::Event::Miscellaneous(
			crate::events::MiscellaneousEvents::Has {
				on: rhs,
				publicity,
				under: under.into_owned(),
				into: dependency,
			},
		));
		dependency
	} else {
		result
	}
}

//! Contains implementations of specific JavaScript items and how Ezno handles them.
//! Contains
//! - Helper / abstracting functions for synthesising
//! - Internal representations for specific objects
//!
//! Does not contain
//! - Type and logic stuff
//! - Contextual information

pub mod assignments;
pub mod conditional;
pub mod constant_functions;
pub mod exceptions;
pub mod functions;
pub mod iteration;
pub mod modules;
pub mod narrowing;
pub mod objects;
pub mod operations;
pub mod template_literal;
pub mod variables;

use source_map::SpanWithSource;

use crate::{
	context::{get_value_of_variable, ClosedOverReferencesInScope, InformationChain},
	diagnostics::TypeStringRepresentation,
	events::RootReference,
	types::{
		get_constraint,
		logical::{Logical, LogicalOrValid},
		properties, PartiallyAppliedGenerics, TypeStore,
	},
	CheckingData, Environment, PropertyValue, Type, TypeId,
};

use self::{functions::ClosedOverVariables, objects::SpecialObject};

pub(crate) fn type_to_js_string_name(on: TypeId) -> Option<&'static str> {
	match on {
		TypeId::NUMBER_TYPE => Some("number"),
		TypeId::STRING_TYPE => Some("string"),
		TypeId::BOOLEAN_TYPE => Some("boolean"),
		TypeId::SYMBOL_TYPE => Some("symbol"),
		TypeId::UNDEFINED_TYPE => Some("undefined"),
		TypeId::NULL_TYPE => Some("object"),
		_ => None,
	}
}

pub(crate) fn string_name_to_type(name: &str) -> Option<TypeId> {
	match name {
		"number" => Some(TypeId::NUMBER_TYPE),
		"string" => Some(TypeId::STRING_TYPE),
		"boolean" => Some(TypeId::BOOLEAN_TYPE),
		"function" => Some(TypeId::FUNCTION_TYPE),
		"undefined" => Some(TypeId::UNDEFINED_TYPE),
		"object" => Some(TypeId::OBJECT_TYPE),
		_rhs => None,
	}
}

/// Returns result of `typeof *on*`
pub fn type_of_operator(on: TypeId, types: &mut TypeStore) -> TypeId {
	if let Some(constraint) = get_constraint(on, types) {
		if let Some(str_name) = type_to_js_string_name(constraint) {
			// TODO make them interal types under `TypeId::*` as to not recreate types
			types.new_constant_type(crate::Constant::String(str_name.to_owned()))
		} else {
			// TODO if always object or function then could get more accurate tag
			types.register_type(crate::Type::Constructor(crate::types::Constructor::TypeOperator(
				crate::types::TypeOperator::TypeOf(on),
			)))
		}
	} else if on == TypeId::UNDEFINED_TYPE {
		return types.new_constant_type(crate::Constant::String("undefined".to_owned()));
	} else if on == TypeId::NULL_TYPE {
		return types.new_constant_type(crate::Constant::String("object".to_owned()));
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
pub fn extends_prototype(lhs: TypeId, rhs: TypeId, information: &impl InformationChain) -> bool {
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
	let rhs_prototype =
		if let Type::SpecialObject(SpecialObject::Function(func, _)) = types.get_type_by_id(rhs) {
			use crate::types::functions::FunctionBehavior;

			let func = types.get_function_from_id(*func);
			match &func.behavior {
				FunctionBehavior::ArrowFunction { .. } | FunctionBehavior::Method { .. } => {
					TypeId::UNDEFINED_TYPE
				}
				FunctionBehavior::Function { prototype, .. }
				| FunctionBehavior::Constructor { prototype, .. } => *prototype,
			}
		} else {
			crate::utilities::notify!("Instanceof RHS dependent or not constructor");
			rhs
		};

	instance_of_operator_rhs_prototype(lhs, rhs_prototype, information, types)
}

pub(crate) fn instance_of_operator_rhs_prototype(
	lhs: TypeId,
	rhs_prototype: TypeId,
	information: &impl InformationChain,
	types: &mut TypeStore,
) -> TypeId {
	// TODO frozen prototypes
	if let Some(constraint) = get_constraint(lhs, types) {
		if let Type::PartiallyAppliedGenerics(crate::types::PartiallyAppliedGenerics {
			on,
			arguments: _,
		}) = types.get_type_by_id(constraint)
		{
			if *on == rhs_prototype {
				return TypeId::TRUE;
			}
			crate::utilities::notify!("Here?");
		}

		types.register_type(Type::Constructor(crate::types::Constructor::TypeOperator(
			crate::types::TypeOperator::IsPrototype { lhs, rhs_prototype },
		)))
	} else if extends_prototype(lhs, rhs_prototype, information) {
		TypeId::TRUE
	} else {
		TypeId::FALSE
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

pub enum CannotDeleteFromError {
	Constraint { constraint: TypeStringRepresentation, position: SpanWithSource },
	NonConfigurable { position: SpanWithSource },
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

	let existing = has_property((publicity, &under), rhs, environment, types);

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
				return Err(CannotDeleteFromError::Constraint { constraint, position });
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
					false,
					environment,
					types,
				);

				if let Ok(property_constraint) = property_constraint {
					crate::utilities::notify!("property_constraint {:?}", property_constraint);
					match property_constraint {
						LogicalOrValid::Logical(Logical::Pure(n)) => match n {
							PropertyValue::Value(_)
							| PropertyValue::Getter(_)
							| PropertyValue::GetterAndSetter { .. }
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
								return Err(CannotDeleteFromError::Constraint {
									constraint,
									position,
								});
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
						variant => {
							crate::utilities::notify!("TODO variant={:?}", variant);
						} // Logical::Or { .. } => todo!(),
						  // Logical::Implies { .. } => todo!(),
						  // Logical::BasedOnKey { .. } => todo!(),
					}
				}
			}
		}

		// Cannot `delete` from non-configurable
		if let Ok(LogicalOrValid::Logical(Logical::Pure(value))) =
			crate::types::properties::get_property_unbound(
				(rhs, None),
				(publicity, &under, None),
				false,
				environment,
				types,
			) {
			if !value.is_configuable_simple() {
				return Err(CannotDeleteFromError::NonConfigurable { position });
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
	// TODO if any
	if get_constraint(rhs, types).is_some() {
		let dependency =
			types.register_type(Type::Constructor(crate::types::Constructor::TypeOperator(
				crate::types::TypeOperator::HasProperty(rhs, under.into_owned()),
			)));

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
		has_property((publicity, under), rhs, environment, types)
	}
}

/// WIP
pub(crate) fn has_property(
	(publicity, key): (properties::Publicity, &properties::PropertyKey<'_>),
	rhs: TypeId,
	information: &impl InformationChain,
	types: &mut TypeStore,
) -> TypeId {
	match types.get_type_by_id(rhs) {
		Type::Interface { .. }
		| Type::Class { .. }
		| Type::Constant(_)
		| Type::FunctionReference(_)
		| Type::Object(_)
		| Type::PartiallyAppliedGenerics(_)
		| Type::And(_, _)
		| Type::SpecialObject(_)
		| Type::Narrowed { .. }
		| Type::AliasTo { .. } => {
			let result = properties::get_property_unbound(
				(rhs, None),
				(publicity, key, None),
				false,
				information,
				types,
			);
			match result {
				Ok(LogicalOrValid::Logical(result)) => match result {
					Logical::Pure(_) => TypeId::TRUE,
					Logical::Or { .. } => {
						crate::utilities::notify!("or or implies `in`");
						TypeId::ERROR_TYPE
					}
					Logical::Implies { .. } => {
						crate::utilities::notify!("or or implies `in`");
						TypeId::ERROR_TYPE
					}
					Logical::BasedOnKey { .. } => {
						crate::utilities::notify!("mapped in");
						TypeId::ERROR_TYPE
					}
				},
				Ok(LogicalOrValid::NeedsCalculation(result)) => {
					crate::utilities::notify!("TODO {:?}", result);
					TypeId::ERROR_TYPE
				}
				Err(err) => {
					crate::utilities::notify!("TODO {:?}", err);
					TypeId::FALSE
				}
			}
		}
		Type::Or(_, _) => {
			crate::utilities::notify!("Condtionally");
			TypeId::ERROR_TYPE
		}
		Type::RootPolyType(_) | Type::Constructor(_) => {
			crate::utilities::notify!("Queue event / create dependent");
			let constraint = get_constraint(rhs, types).unwrap();
			has_property((publicity, key), constraint, information, types)
		}
	}
}

pub mod tsc {
	use source_map::SpanWithSource;

	use crate::{
		diagnostics,
		types::{Constructor, TypeStore},
		CheckingData, Environment, Type, TypeId,
	};

	/// Returns result of `*on* as *cast_to*`. Returns `Err(())` for invalid casts where invalid casts
	/// occur for casting a constant
	pub fn as_cast(on: TypeId, cast_to: TypeId, types: &mut TypeStore) -> Result<TypeId, ()> {
		fn can_cast_type(ty: &Type) -> bool {
			match ty {
				// TODO some of these are more correct than the others
				Type::RootPolyType(_rpt) => true,
				Type::Constructor(constr) => match constr {
					Constructor::CanonicalRelationOperator { .. }
					| Constructor::UnaryOperator { .. }
					| Constructor::BinaryOperator { .. } => false,
					Constructor::TypeOperator(_) => todo!(),
					Constructor::TypeExtends(_) => todo!(),
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

			Ok(types.register_type(Type::Narrowed { narrowed_to: cast_to, from: on }))
		} else {
			Err(())
		}
	}

	pub fn non_null_assertion(
		on: TypeId,
		environment: &Environment,
		types: &mut TypeStore,
	) -> Result<TypeId, ()> {
		let mut result = Vec::new();
		super::narrowing::build_union_from_filter(
			on,
			super::narrowing::NOT_NULL_OR_UNDEFINED,
			&mut result,
			environment,
			types,
		);
		let cast_to = types.new_or_type_from_iterator(result);
		Ok(types.register_type(Type::Narrowed { narrowed_to: cast_to, from: on }))
	}

	pub fn check_satisfies<T: crate::ReadFromFS, A: crate::ASTImplementation>(
		expr_ty: TypeId,
		to_satisfy: TypeId,
		at: SpanWithSource,
		environment: &mut Environment,
		checking_data: &mut CheckingData<T, A>,
	) {
		use crate::types::subtyping;

		let mut state = subtyping::State {
			already_checked: Default::default(),
			mode: Default::default(),
			contributions: Default::default(),
			others: subtyping::SubTypingOptions { allow_errors: false },
			object_constraints: None,
		};

		let result = subtyping::type_is_subtype(
			to_satisfy,
			expr_ty,
			&mut state,
			environment,
			&checking_data.types,
		);
		if result.is_mismatch() {
			let expected = diagnostics::TypeStringRepresentation::from_type_id(
				to_satisfy,
				environment,
				&checking_data.types,
				false,
			);
			let found = diagnostics::TypeStringRepresentation::from_type_id(
				expr_ty,
				environment,
				&checking_data.types,
				false,
			);
			checking_data
				.diagnostics_container
				.add_error(diagnostics::TypeCheckError::NotSatisfied { at, expected, found });
		}
	}
}

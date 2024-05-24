//! How type parameters are resolved

use crate::{
	features::{
		functions::{ClosureChain, ClosureId, ThisValue},
		objects::SpecialObjects,
		operations::{
			evaluate_equality_inequality_operation, evaluate_mathematical_operation,
			evaluate_pure_unary_operator,
		},
	},
	subtyping::{State, SubTypingOptions},
	types::{
		generics::contributions::Contributions, get_constraint, Constructor, ObjectNature,
		PolyNature, StructureGenerics, Type, TypeStore,
	},
	Decidable, Environment, TypeId,
};

use super::generic_type_arguments::StructureGenericArguments;

pub struct SubstitutionArguments<'a> {
	/// for extends + parent generics
	pub(crate) parent: Option<&'a SubstitutionArguments<'a>>,
	pub(crate) arguments: crate::Map<TypeId, TypeId>,
	pub(crate) closures: Vec<ClosureId>,
}

impl<'a> ClosureChain for SubstitutionArguments<'a> {
	fn get_fact_from_closure<T, R>(&self, _fact: &crate::LocalInformation, cb: T) -> Option<R>
	where
		T: Fn(ClosureId) -> Option<R>,
	{
		self.closures.iter().copied().find_map(cb)
	}
}

impl<'a> SubstitutionArguments<'a> {
	/// TODO this might need to be done per context
	pub fn set_during_application(&mut self, ty: TypeId, value: TypeId) {
		self.arguments.insert(ty, value);
	}

	pub fn get_argument(&self, id: TypeId) -> Option<TypeId> {
		self.arguments
			.get(&id)
			.cloned()
			.or_else(|| self.parent.and_then(|parent| parent.get_argument(id)))
	}
}

// TODO replace environment with information chain
pub(crate) fn substitute(
	id: TypeId,
	arguments: &SubstitutionArguments,
	environment: &Environment,
	types: &mut TypeStore,
) -> TypeId {
	// TODO parent
	if let Some(value) = arguments.get_argument(id) {
		return value;
	}

	let ty = types.get_type_by_id(id);

	match ty {
		Type::Constant(_) | Type::AliasTo { .. } | Type::Interface { .. } | Type::Class { .. } => {
			id
		}
		// This works for both objects and `AnonymousTypeAnnotation`s
		Type::Object(ObjectNature::RealDeal | ObjectNature::AnonymousTypeAnnotation) => {
			crate::utilities::notify!("Here!!!");
			todo!("arguments.curry_arguments(types, id)")
		}
		Type::SpecialObject(SpecialObjects::Function(f, t)) => {
			// Also sub the this type
			let id = if let ThisValue::Passed(p) = t {
				let function_id = *f;
				let passed = ThisValue::Passed(substitute(*p, arguments, environment, types));
				types.register_type(Type::SpecialObject(SpecialObjects::Function(
					function_id,
					passed,
				)))
			} else {
				id
			};
			todo!("arguments.curry_arguments(types, id)")
		}
		Type::SpecialObject(SpecialObjects::ClassConstructor { .. })
		| Type::FunctionReference(..) => todo!("arguments.curry_arguments(types, id)"),
		Type::SpecialObject(special_object) => match special_object {
			SpecialObjects::Promise { .. } => todo!(),
			SpecialObjects::Generator { .. } => todo!(),
			SpecialObjects::Proxy { .. } => todo!(),
			SpecialObjects::RegularExpression(_) => todo!(),
			SpecialObjects::Import(_) => todo!(),
			_ => unreachable!(),
		},
		Type::And(lhs, rhs) => {
			let rhs = *rhs;
			let lhs = substitute(*lhs, arguments, environment, types);
			let rhs = substitute(rhs, arguments, environment, types);
			types.new_and_type(lhs, rhs).unwrap_or(TypeId::NEVER_TYPE)
		}
		Type::Or(lhs, rhs) => {
			let rhs = *rhs;
			let lhs = substitute(*lhs, arguments, environment, types);
			let rhs = substitute(rhs, arguments, environment, types);
			types.new_or_type(lhs, rhs)
		}
		Type::RootPolyType(nature) => {
			if let PolyNature::Open(_) = nature {
				id
			} else if let PolyNature::FunctionGeneric { .. } | PolyNature::StructureGeneric { .. } =
				nature
			{
				crate::utilities::notify!("Could not find argument for explicit generic {:?}", id);
				id
			} else {
				// Other root poly types cases handled by the early return
				let on = crate::types::printing::print_type(id, types, environment, true);
				crate::utilities::notify!("Could not find argument for {}", on);
				TypeId::ERROR_TYPE
			}
		}

		// TODO environment should hold what dependents on what to reduce excess here
		Type::Constructor(constructor) => match constructor.clone() {
			Constructor::BinaryOperator { lhs, operator, rhs, .. } => {
				let lhs = substitute(lhs, arguments, environment, types);
				let rhs = substitute(rhs, arguments, environment, types);

				match evaluate_mathematical_operation(lhs, operator, rhs, types, false) {
					Ok(result) => result,
					Err(()) => {
						unreachable!(
							"Cannot {lhs:?} {operator:?} {rhs:?} (restriction or something failed)"
						);
					}
				}
			}
			Constructor::UnaryOperator { operand, operator, .. } => {
				match evaluate_pure_unary_operator(
					operator, operand, types,
					// Restrictions should have been made ahead of time
					false,
				) {
					Ok(result) => result,
					Err(()) => {
						unreachable!(
							"Cannot {operand:?} {operator:?} (restriction or something failed)"
						);
					}
				}
			}
			Constructor::ConditionalResult {
				condition,
				truthy_result,
				otherwise_result,
				result_union: _,
			} => {
				// TSC behavior
				if let Type::Constructor(Constructor::TypeRelationOperator(
					crate::types::TypeRelationOperator::Extends { item, extends },
				)) = types.get_type_by_id(condition)
				{
					let (item, extends) = (*item, *extends);
					let item = substitute(item, arguments, environment, types);

					// TODO pass generics down via other
					// let extends = substitute(extends, arguments, environment, types);

					return compute_extends_rule(
						extends,
						item,
						environment,
						types,
						truthy_result,
						arguments,
						otherwise_result,
					);
				}

				let condition = substitute(condition, arguments, environment, types);

				// crate::utilities::notify!(
				// 	"after on={} true={} false={}",
				// 	crate::types::printing::print_type(
				// 		condition,
				// 		types,
				// 		environment,
				// 		true
				// 	),
				// 	crate::types::printing::print_type(
				// 		truthy_result,
				// 		types,
				// 		environment,
				// 		true
				// 	),
				// 	crate::types::printing::print_type(
				// 		otherwise_result,
				// 		types,
				// 		environment,
				// 		true
				// 	)
				// );

				if let Decidable::Known(result) =
					crate::types::is_type_truthy_falsy(condition, types)
				{
					if result {
						substitute(truthy_result, arguments, environment, types)
					} else {
						substitute(otherwise_result, arguments, environment, types)
					}
				} else {
					crate::utilities::notify!("{:?} is undecidable", condition);
					let truthy_result = substitute(truthy_result, arguments, environment, types);
					let otherwise_result =
						substitute(otherwise_result, arguments, environment, types);
					// TODO result_union
					let ty = Constructor::ConditionalResult {
						condition,
						truthy_result,
						otherwise_result,
						result_union: types.new_or_type(truthy_result, otherwise_result),
					};

					types.register_type(Type::Constructor(ty))
				}
			}
			Constructor::Property { on, under, result, bind_this, .. } => {
				let id = get_constraint(on, types).unwrap_or(on);

				if let Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
					on: TypeId::ARRAY_TYPE,
					arguments,
				})) = types.get_type_by_id(id)
				{
					// Try get the constant
					if under.as_number(types).is_some() {
						crate::utilities::notify!("Temp array index property get");
						let value = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
						types.new_or_type(value, TypeId::UNDEFINED_TYPE)
					} else {
						let substitutable = arguments.into_substitutable();

						let new_result = substitute(
							result,
							// TODO
							&substitutable,
							environment,
							types,
						);
						// crate::utilities::notify!(
						// 	"Specialising the constraint {:?} to {:?} using {:?} (which is strange)",
						// 	result,
						// 	new_result,
						// 	structure_generic_arguments
						// );
						types.register_type(Type::Constructor(Constructor::Property {
							on,
							under,
							result: new_result,
							bind_this,
						}))
					}
				} else {
					todo!(
						"Constructor::Property ({:?}[{:?}]) should be covered by events",
						on,
						under
					);
				}
			}
			Constructor::Image { .. } => {
				let on = crate::types::printing::print_type(id, types, environment, true);
				todo!("Constructor::Image {on} should be covered by events");
				// id

				// let on = substitute(on, arguments, environment);

				// crate::utilities::notify!("Substituted {}", environment.debug_type(on));

				// let func_arguments = with
				// 	.into_iter()
				// 	.map(|argument| match argument {
				// 		synthesisedArgument::NonSpread { ty, pos } => {
				// 			let ty = substitute(*ty, arguments, environment);
				// 			synthesisedArgument::NonSpread { ty, pos: pos.clone() }
				// 		}
				// 	})
				// 	.collect::<Vec<_>>();

				// let FunctionCallResult { returned_type, warnings } =
				// 	call_type(on, func_arguments, None, None, environment, checking_data)
				// 		.expect("Inferred constraints and checking failed");

				// crate::utilities::notify!("TODO getting a property not substituted during calling");

				// let on = substitute(on, arguments, environment, checking_data);
				// let property = substitute(property, arguments, environment, checking_data);

				// environment
				// 	.get_property(on, property, checking_data, None)
				// 	.expect("Inferred constraints and checking failed for a property")
			}
			Constructor::StructureGenerics(StructureGenerics {
				on,
				arguments: structure_arguments,
			}) => {
				let new_structure_arguments = match structure_arguments {
					StructureGenericArguments::ExplicitRestrictions(restrictions) => {
						let restrictions = restrictions
							.into_iter()
							.map(|(lhs, (arg, pos))| {
								(lhs, (substitute(arg, arguments, environment, types), pos))
							})
							.collect();
						StructureGenericArguments::ExplicitRestrictions(restrictions)
					}
					StructureGenericArguments::Closure(_) => return id,
					StructureGenericArguments::LookUp { on } => StructureGenericArguments::LookUp {
						on: substitute(on, arguments, environment, types),
					},
				};

				types.register_type(Type::Constructor(Constructor::StructureGenerics(
					StructureGenerics { on, arguments: new_structure_arguments },
				)))
			}

			// Constructor::PrototypeOf(prototype) => {
			// 	let prototype = substitute(prototype, arguments, environment, types);
			// 	if let Type::AliasTo { to, .. } = types.get_type_by_id(prototype) {
			// 		crate::utilities::notify!(
			// 			"TODO temp might have to do more here when specialising a prototype"
			// 		);
			// 		*to
			// 	} else {
			// 		todo!()
			// 	}
			// }
			Constructor::CanonicalRelationOperator { lhs, operator, rhs } => {
				let operator = match operator {
					crate::features::operations::CanonicalEqualityAndInequality::StrictEqual => {
						crate::features::operations::EqualityAndInequality::StrictEqual
					}
					crate::features::operations::CanonicalEqualityAndInequality::LessThan => {
						crate::features::operations::EqualityAndInequality::LessThan
					}
				};
				let lhs = substitute(lhs, arguments, environment, types);
				let rhs = substitute(rhs, arguments, environment, types);

				match evaluate_equality_inequality_operation(lhs, &operator, rhs, types, false) {
					Ok(result) => result,
					Err(()) => {
						unreachable!(
							"Cannot {lhs:?} {operator:?} {rhs:?} (restriction or something failed)"
						);
					}
				}
			}
			Constructor::TypeOperator(op) => match op {
				crate::types::TypeOperator::PrototypeOf(_) => todo!(),
				crate::types::TypeOperator::TypeOf(ty) => {
					let ty = substitute(ty, arguments, environment, types);
					crate::features::type_of_operator(ty, types)
				}
			},
			Constructor::TypeRelationOperator(op) => match op {
				crate::types::TypeRelationOperator::Extends { item, extends } => {
					let item = substitute(item, arguments, environment, types);
					let extends = substitute(extends, arguments, environment, types);

					let result = crate::subtyping::type_is_subtype(
						extends,
						item,
						&mut crate::subtyping::State {
							already_checked: Default::default(),
							mode: Default::default(),
							contributions: None,
							object_constraints: None,
							others: Default::default(),
						},
						environment,
						types,
					);

					// let base = crate::types::get_larger_type(item, types);
					// let does_extend = base == extends;
					// crate::utilities::notify!(
					// 	"Extends result {:?} base={:?} extends={:?}",
					// 	does_extend,
					// 	crate::types::printing::print_type(base, types, environment, true),
					// 	crate::types::printing::print_type(extends, types, environment, true)
					// );
					if result.is_subtype() {
						TypeId::TRUE
					} else {
						TypeId::FALSE
					}

					// let does_extend = get_larger_type(ty, types) == extends;
					// crate::utilities::notify!("Extends result {:?}", does_extend);
					// if does_extend {
					// 	TypeId::TRUE
					// } else {
					// 	TypeId::FALSE
					// }

					// TODO special behavior that doesn't need to collect errors...
					// let result = type_is_subtype(extends, ty, environment, types);

					// let does_extend = matches!(result, crate::subtyping::SubTypeResult::IsSubType);

					// if does_extend {
					// 	TypeId::TRUE
					// } else {
					// 	TypeId::FALSE
					// }
				}
			},
			Constructor::Awaited { .. } => todo!("should have effect result"),
		},
	}
}

fn compute_extends_rule(
	extends: TypeId,
	item: TypeId,
	environment: &Environment,
	types: &mut TypeStore,
	truthy_result: TypeId,
	arguments: &SubstitutionArguments,
	otherwise_result: TypeId,
) -> TypeId {
	if let Type::Or(lhs, rhs) = types.get_type_by_id(item) {
		let rhs = *rhs;
		let lhs = compute_extends_rule(
			extends,
			*lhs,
			environment,
			types,
			truthy_result,
			arguments,
			otherwise_result,
		);
		let rhs = compute_extends_rule(
			extends,
			rhs,
			environment,
			types,
			truthy_result,
			arguments,
			otherwise_result,
		);
		types.new_or_type(lhs, rhs)
	} else {
		let mut state = State {
			already_checked: Default::default(),
			mode: Default::default(),
			contributions: Some(Contributions {
				parent: None,
				call_site_type_arguments: None,
				staging_contravariant: Default::default(),
				staging_covariant: Default::default(),
			}),
			object_constraints: Default::default(),
			others: SubTypingOptions::default(),
		};
		let result =
			crate::subtyping::type_is_subtype(extends, item, &mut state, &environment, types);

		if result.is_subtype() {
			// Add infer types
			let contributions = state.contributions.unwrap();
			crate::utilities::notify!("{:?}", contributions.staging_contravariant);

			if contributions.staging_contravariant.is_empty()
				&& contributions.staging_covariant.is_empty()
			{
				substitute(truthy_result, arguments, environment, types)
			} else {
				let args = contributions
					.staging_contravariant
					.into_iter()
					.map(|(key, (value, _))| (key, value))
					.chain(
						contributions
							.staging_covariant
							.into_iter()
							.map(|(key, (value, _))| (key, value)),
					)
					.collect();

				let arguments = SubstitutionArguments {
					parent: Some(arguments),
					arguments: args,
					closures: Default::default(),
				};
				substitute(truthy_result, &arguments, environment, types)
			}
		} else {
			substitute(otherwise_result, arguments, environment, types)
		}
	}
}

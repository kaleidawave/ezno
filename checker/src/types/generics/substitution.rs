//! How type parameters are resolved

use source_map::{Nullable, SpanWithSource};

use crate::{
	context::LocalInformation,
	features::{
		functions::{ClosureChain, ClosureId},
		objects::{Proxy, SpecialObject},
		operations::{evaluate_equality_inequality_operation, evaluate_mathematical_operation},
	},
	subtyping::{State, SubTypingOptions},
	types::{
		calling::ThisValue,
		generics::contributions::Contributions,
		intrinsics::{self, distribute_tsc_string_intrinsic},
		logical::{BasedOnKey, Logical, LogicalOrValid},
		properties::{get_property_unbound, Publicity},
		Constructor, ObjectNature, PartiallyAppliedGenerics, PolyNature, Type, TypeStore,
	},
	Decidable, Environment, PropertyValue, TypeId,
};

use super::generic_type_arguments::GenericArguments;

pub struct SubstitutionArguments<'a> {
	/// for extends + parent generics
	pub(crate) parent: Option<&'a SubstitutionArguments<'a>>,
	pub(crate) arguments: crate::Map<TypeId, TypeId>,
	pub(crate) closures: Vec<ClosureId>,
}

impl ClosureChain for SubstitutionArguments<'_> {
	fn get_fact_from_closure<T, R>(&self, _fact: &LocalInformation, cb: T) -> Option<R>
	where
		T: Fn(ClosureId) -> Option<R>,
	{
		self.closures.iter().copied().find_map(cb)
	}
}

impl SubstitutionArguments<'_> {
	/// TODO this might need to be done per context
	pub fn set_during_application(&mut self, ty: TypeId, value: TypeId) {
		self.arguments.insert(ty, value);
	}

	#[must_use]
	pub fn get_argument(&self, id: TypeId) -> Option<TypeId> {
		self.arguments
			.get(&id)
			.copied()
			.or_else(|| self.parent.and_then(|parent| parent.get_argument(id)))
	}

	#[must_use]
	pub(crate) fn new_arguments_for_use_in_loop() -> SubstitutionArguments<'static> {
		SubstitutionArguments {
			parent: None,
			arguments: Default::default(),
			closures: Default::default(),
		}
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
		// Closures for objects
		// Type::SpecialObject(SpecialObject::ClassConstructor { .. })
		Type::Object(ObjectNature::RealDeal) => {
			// Apply curring
			if arguments.closures.is_empty() {
				id
			} else {
				types.register_type(Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
					on: id,
					arguments: GenericArguments::Closure(arguments.closures.clone()),
				}))
			}
		}
		// Specialisation for object type annotation (todo could do per property in future)
		// Can return functions from functions somehow as well
		Type::FunctionReference(..) | Type::Object(ObjectNature::AnonymousTypeAnnotation(_)) => {
			// crate::utilities::notify!("{:?}", arguments.arguments);
			// Apply curring
			if arguments.arguments.is_empty() {
				id
			} else {
				let arguments = GenericArguments::ExplicitRestrictions(
					arguments
						.arguments
						.iter()
						.map(|(k, v)| (*k, (*v, SpanWithSource::NULL)))
						.collect(),
				);
				types.register_type(Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
					on: id,
					// TODO argument positions
					arguments,
				}))
			}
		}
		Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on,
			arguments: structure_arguments,
		}) => {
			let on = *on;
			let generic_arguments = structure_arguments.clone();

			// Fold intrinsic type
			if intrinsics::is_tsc_string_intrinsic(on) {
				let arg =
					structure_arguments.get_structure_restriction(TypeId::STRING_GENERIC).unwrap();
				let value = substitute(arg, arguments, environment, types);
				return distribute_tsc_string_intrinsic(value, on, types);
			}

			let new_structure_arguments = match generic_arguments {
				GenericArguments::ExplicitRestrictions(restrictions) => {
					let restrictions = restrictions
						.into_iter()
						.map(|(lhs, (arg, pos))| {
							(lhs, (substitute(arg, arguments, environment, types), pos))
						})
						.collect();
					GenericArguments::ExplicitRestrictions(restrictions)
				}
				GenericArguments::Closure(_) => return id,
				GenericArguments::LookUp { on } => {
					GenericArguments::LookUp { on: substitute(on, arguments, environment, types) }
				}
			};

			types.register_type(Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
				on,
				arguments: new_structure_arguments,
			}))
		}
		Type::SpecialObject(special_object) => match &**special_object {
			SpecialObject::Function(f, t) => {
				// Substitute the this type
				let id = if let ThisValue::Passed(p) = t {
					let function_id = *f;
					let passed = ThisValue::Passed(substitute(*p, arguments, environment, types));
					let new_ty =
						Type::SpecialObject(Box::new(SpecialObject::Function(function_id, passed)));
					types.register_type(new_ty)
				} else {
					id
				};
				// Apply curring
				if arguments.closures.is_empty() {
					id
				} else {
					types.register_type(Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
						on: id,
						arguments: GenericArguments::Closure(arguments.closures.clone()),
					}))
				}
			}
			SpecialObject::Promise { .. } => todo!(),
			SpecialObject::Generator { .. } => todo!(),
			SpecialObject::Proxy(Proxy { over, handler }) => {
				let (prev_over, prev_handler) = (*over, *handler);
				let over = substitute(prev_over, arguments, environment, types);
				let handler = substitute(prev_handler, arguments, environment, types);
				crate::utilities::notify!("Here {:?}", (prev_over, prev_handler, over, handler));
				let proxy = SpecialObject::Proxy(Proxy { over, handler });
				types.register_type(Type::SpecialObject(Box::new(proxy)))
			}
			SpecialObject::RegularExpression { .. } => todo!(),
			SpecialObject::Import(_) | SpecialObject::Null => id,
		},
		Type::And(lhs, rhs) => {
			let rhs = *rhs;
			let lhs = substitute(*lhs, arguments, environment, types);
			let rhs = substitute(rhs, arguments, environment, types);
			types.new_and_type(lhs, rhs) // .unwrap_or(TypeId::NEVER_TYPE)
		}
		Type::Or(lhs, rhs) => {
			let rhs = *rhs;
			let lhs = substitute(*lhs, arguments, environment, types);
			let rhs = substitute(rhs, arguments, environment, types);
			types.new_or_type(lhs, rhs)
		}
		Type::Narrowed { from, .. } => substitute(*from, arguments, environment, types),
		Type::RootPolyType(nature) => {
			if let PolyNature::Open(_) | PolyNature::Error(_) = nature {
				id
			} else if let PolyNature::InferGeneric { .. } = nature {
				// Infer generic is fine for `type Index<T> = T extends Array<infer I> ? I : never`;
				crate::utilities::notify!("No argument for infer generic (sometimes fine)");
				id
			} else if let PolyNature::FunctionGeneric { .. } | PolyNature::StructureGeneric { .. } =
				nature
			{
				crate::utilities::notify!(
					"Could not find argument for explicit generic {:?} (nature={:?})",
					id,
					nature
				);
				id
			} else if let PolyNature::FreeVariable { .. } = nature {
				crate::utilities::notify!("Could not find free variable");
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

				// TODO
				let advanced_numbers = false;

				match evaluate_mathematical_operation(
					lhs,
					operator,
					rhs,
					environment,
					types,
					false,
					advanced_numbers,
				) {
					Ok(result) => result,
					Err(()) => {
						unreachable!(
							"Cannot {:?} {operator:?} {:?} (restriction or something failed)",
							crate::types::printing::print_type(lhs, types, environment, true),
							crate::types::printing::print_type(rhs, types, environment, true)
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
				// TSC behavior, use `compute_extends_rule` which does distribution
				if let Type::Constructor(Constructor::TypeExtends(crate::types::TypeExtends {
					item,
					extends,
				})) = types.get_type_by_id(condition)
				{
					// crate::utilities::notify!("Here!");

					let (item, extends) = (*item, *extends);
					let item = substitute(item, arguments, environment, types);
					let extends = substitute(extends, arguments, environment, types);

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
			Constructor::Property { on, under, result, mode, .. } => {
				let under = under.substitute(arguments, environment, types);
				let on = substitute(on, arguments, environment, types);

				let on_type = types.get_type_by_id(on);

				if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
					on: TypeId::ARRAY_TYPE,
					arguments,
				}) = on_type
				{
					// Try get the constant
					if under.as_number(types).is_some() {
						crate::utilities::notify!("Temp array index property get");
						let value = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
						types.new_or_type(value, TypeId::UNDEFINED_TYPE)
					} else {
						let substitutable = arguments.build_substitutable();

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
							mode,
						}))
					}
				} else if let Type::Interface { .. }
				| Type::Object(ObjectNature::AnonymousTypeAnnotation(_))
				| Type::AliasTo { .. } = on_type
				{
					// TODO union ors etc
					let get_property = get_property_unbound(
						(on, None),
						(Publicity::Public, &under, None),
						false,
						environment,
						types,
					);

					match get_property {
						Ok(LogicalOrValid::Logical(value)) => {
							fn resolve_logical_during_substitution(
								value: Logical<PropertyValue>,
								environment: &Environment,
								types: &mut TypeStore,
							) -> TypeId {
								match value {
									Logical::Pure(v) => {
										// substitute(id, arguments, environment, types)
										v.as_get_type(types)
									}
									Logical::Or { .. } => todo!("{:?}", value),
									Logical::Implies { .. } => todo!("{:?}", value),
									Logical::BasedOnKey(BasedOnKey::Left { .. }) => {
										todo!("{:?}", value)
									}
									Logical::BasedOnKey(BasedOnKey::Right(property_on)) => {
										property_on
											.get_on(None, environment, types)
											.unwrap_or(TypeId::NEVER_TYPE)
									}
								}
							}

							resolve_logical_during_substitution(value, environment, types)
						}
						Ok(value) => {
							crate::utilities::notify!("TODO {:?}", value);
							TypeId::UNIMPLEMENTED_ERROR_TYPE
						}
						Err(err) => {
							crate::utilities::notify!("{:?}", err);
							TypeId::UNIMPLEMENTED_ERROR_TYPE
						}
					}
				} else {
					crate::utilities::notify!(
						"Constructor::Property ({:?}[{:?}]) should be covered by events",
						on_type,
						under
					);
					TypeId::UNIMPLEMENTED_ERROR_TYPE
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

				let result = evaluate_equality_inequality_operation(
					lhs,
					&operator,
					rhs,
					environment,
					types,
					// TODO pass down
					&crate::features::operations::OperatorOptions {
						strict_casts: false,
						advanced_numbers: false,
					},
				);

				if let Ok((left, _warning)) = result {
					left
				} else {
					crate::utilities::notify!("Error here");
					TypeId::OPEN_BOOLEAN_TYPE
				}
			}
			Constructor::TypeOperator(op) => match op {
				crate::types::TypeOperator::TypeOf(ty) => {
					let ty = substitute(ty, arguments, environment, types);
					crate::features::type_of_operator(ty, types)
				}
				crate::types::TypeOperator::HasProperty(_, _) => {
					unreachable!("'HasProperty' should be specialised by events")
				}
				crate::types::TypeOperator::HasPrototype { lhs, rhs_prototype } => {
					let lhs = substitute(lhs, arguments, environment, types);
					let rhs_prototype = substitute(rhs_prototype, arguments, environment, types);
					crate::features::instance_of_operator_rhs_prototype(
						lhs,
						rhs_prototype,
						environment,
						types,
					)
					// unreachable!("'HasProperty' should be specialised by events")
				}
			},
			Constructor::TypeExtends(op) => {
				let crate::types::TypeExtends { item, extends } = op;
				let before = item;
				let item = substitute(item, arguments, environment, types);
				let extends = substitute(extends, arguments, environment, types);

				// Just do boolean values here
				{
					use crate::types::printing::print_type;

					crate::utilities::notify!(
						"Subtyping {} (prev {}) :>= {} ({:?})",
						print_type(item, types, environment, true),
						print_type(before, types, environment, true),
						print_type(extends, types, environment, true),
						&arguments.arguments
					);
				}

				let mut state = crate::subtyping::State {
					already_checked: Default::default(),
					mode: Default::default(),
					contributions: None,
					object_constraints: None,
					others: Default::default(),
				};
				let result = crate::subtyping::type_is_subtype(
					extends,
					item,
					&mut state,
					environment,
					types,
				);

				if result.is_subtype() {
					TypeId::TRUE
				} else {
					TypeId::FALSE
				}
			}
			Constructor::Awaited { .. } => todo!("should have effect result"),
			Constructor::KeyOf(on) => {
				let on = substitute(on, arguments, environment, types);
				types.new_key_of(on)
			}
		},
	}
}

pub(crate) fn compute_extends_rule(
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
	} else if let Some(_constraint) = super::super::get_constraint(item, types) {
		let specialised_result =
			Type::Constructor(Constructor::TypeExtends(crate::types::TypeExtends {
				item,
				extends,
			}));
		types.register_type(specialised_result)
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
			crate::subtyping::type_is_subtype(extends, item, &mut state, environment, types);

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
					.map(|(key, (value, _))| (key, value.into_type(types)))
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

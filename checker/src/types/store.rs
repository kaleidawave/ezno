use std::collections::{HashMap, HashSet};

use map_vec::Map as SmallMap;
use source_map::SpanWithSource;

use crate::{
	context::{
		information::{get_property_unbound, InformationChain},
		Logical, PossibleLogical,
	},
	features::{
		functions::{ClosureId, FunctionBehavior},
		objects::SpecialObjects,
	},
	types::{FunctionType, GenericChain, PolyNature, Type},
	Environment, FunctionId, LocalInformation, TypeId,
};

use super::{
	get_constraint, poly_types::generic_type_arguments::StructureGenericArguments,
	properties::PropertyKey, Constructor, LookUpGeneric, LookUpGenericMap, StructureGenerics,
	TypeRelationOperator,
};

/// Holds all the types. Eventually may be split across modules
#[derive(Debug, binary_serialize_derive::BinarySerializable)]
pub struct TypeStore {
	/// Contains all of the types. Indexed by [TypeId]
	types: Vec<Type>,
	pub(crate) functions: HashMap<FunctionId, FunctionType>,

	// TODO
	pub(crate) _dependent_dependencies: HashMap<TypeId, HashSet<TypeId>>,
	// TODO
	pub(crate) _specialisations: HashMap<TypeId, Vec<TypeId>>,

	/// can be used for tree shaking
	pub called_functions: HashSet<FunctionId>,

	/// TODO not best place but is passed through everything so
	pub(crate) closure_counter: u32,
}

impl Default for TypeStore {
	fn default() -> Self {
		// These have to be in the order of TypeId
		let types = vec![
			Type::Interface {
				name: "error".to_owned(),
				parameters: None,
				nominal: true,
				extends: None,
			},
			Type::Interface {
				name: "never".to_owned(),
				parameters: None,
				nominal: true,
				extends: None,
			},
			Type::Interface {
				name: "any".to_owned(),
				parameters: None,
				nominal: true,
				extends: None,
			},
			Type::Interface {
				name: "boolean".to_owned(),
				parameters: None,
				nominal: true,
				extends: None,
			},
			Type::Interface {
				name: "number".to_owned(),
				parameters: None,
				nominal: true,
				extends: None,
			},
			Type::Interface {
				name: "string".to_owned(),
				parameters: None,
				nominal: true,
				extends: None,
			},
			Type::Interface {
				name: "undefined".to_owned(),
				parameters: None,
				nominal: true,
				extends: None,
			},
			Type::Interface {
				name: "null".to_owned(),
				parameters: None,
				nominal: true,
				extends: None,
			},
			Type::Interface {
				name: "Array".to_owned(),
				parameters: Some(vec![TypeId::T_TYPE]),
				nominal: true,
				extends: None,
			},
			// Array T
			Type::RootPolyType(PolyNature::Generic {
				name: "T".to_owned(),
				eager_fixed: TypeId::ANY_TYPE,
			}),
			Type::Interface {
				name: "object".to_owned(),
				parameters: None,
				nominal: false,
				extends: None,
			},
			Type::Interface {
				name: "Function".to_owned(),
				parameters: None,
				nominal: false,
				extends: None,
			},
			Type::Interface {
				name: "RegExp".to_owned(),
				parameters: None,
				nominal: true,
				extends: None,
			},
			Type::Or(TypeId::STRING_TYPE, TypeId::NUMBER_TYPE),
			// true
			Type::Constant(crate::Constant::Boolean(true)),
			// false
			Type::Constant(crate::Constant::Boolean(false)),
			// zero
			Type::Constant(crate::Constant::Number(0.into())),
			// one
			Type::Constant(crate::Constant::Number(1.into())),
			// NaN
			Type::Constant(crate::Constant::NaN),
			// inferred this free variable shortcut
			Type::RootPolyType(PolyNature::FreeVariable {
				reference: crate::events::RootReference::This,
				based_on: TypeId::ANY_TYPE,
			}),
			Type::RootPolyType(PolyNature::Generic {
				name: "new.target".to_owned(),
				// TODO
				eager_fixed: TypeId::ANY_TYPE,
			}),
			// TODO Symbols, needs Constant::Symbol
			Type::AliasTo {
				to: TypeId::ANY_TYPE,
				name: "SymbolToPrimitive".into(),
				parameters: None,
			},
			// `void` type. Does not block sometimes
			Type::AliasTo { to: TypeId::UNDEFINED_TYPE, name: "void".into(), parameters: None },
		];

		// Check that above is correct, TODO eventually a macro
		assert_eq!(types.len(), TypeId::INTERNAL_TYPE_COUNT);

		Self {
			types: types.clone(),
			functions: HashMap::new(),
			_dependent_dependencies: Default::default(),
			_specialisations: Default::default(),
			called_functions: Default::default(),
			closure_counter: 0,
		}
	}
}

impl TypeStore {
	pub fn new_constant_type(&mut self, constant: crate::Constant) -> crate::TypeId {
		// Reuse existing ids rather than creating new types sometimes
		match constant {
			crate::Constant::Number(number) if number == 1f64 => TypeId::ONE,
			crate::Constant::Number(number) if number == 0f64 => TypeId::ZERO,
			crate::Constant::Boolean(value) => {
				if value {
					TypeId::TRUE
				} else {
					TypeId::FALSE
				}
			}
			crate::Constant::Undefined => TypeId::UNDEFINED_TYPE,
			crate::Constant::Null => TypeId::NULL_TYPE,
			crate::Constant::NaN => TypeId::NAN_TYPE,
			_ => {
				let ty = Type::Constant(constant);
				// TODO maybe separate id
				self.register_type(ty)
			}
		}
	}

	pub(crate) fn register_type(&mut self, ty: Type) -> TypeId {
		let id = TypeId(self.types.len().try_into().expect("too many types!"));
		self.types.push(ty);
		id
	}

	#[must_use]
	pub fn get_type_by_id(&self, id: TypeId) -> &Type {
		&self.types[id.0 as usize]
	}

	pub fn new_or_type(&mut self, lhs: TypeId, rhs: TypeId) -> TypeId {
		if lhs == rhs {
			return lhs;
		}

		if let (TypeId::TRUE, TypeId::FALSE) | (TypeId::FALSE, TypeId::TRUE) = (lhs, rhs) {
			return TypeId::BOOLEAN_TYPE;
		}

		let ty = Type::Or(lhs, rhs);
		self.register_type(ty)
	}

	pub fn new_and_type(&mut self, lhs: TypeId, rhs: TypeId) -> Result<TypeId, ()> {
		if lhs == rhs {
			return Ok(lhs);
		}

		let left_ty = self.get_type_by_id(lhs);
		let right_ty = self.get_type_by_id(rhs);

		// TODO more cases
		if let (Type::Constant(l), Type::Constant(r)) = (left_ty, right_ty) {
			if l != r {
				return Err(());
			}
		} else if let (
			Type::Interface { nominal: true, .. },
			Type::Interface { nominal: true, .. },
		) = (left_ty, right_ty)
		{
			return Err(());
		}

		// (left and right) distributivity.
		let result = if let Type::Or(or_lhs, or_rhs) = left_ty {
			let (or_lhs, or_rhs) = (*or_lhs, *or_rhs);
			let new_lhs = self.new_and_type(or_lhs, rhs)?;
			let new_rhs = self.new_and_type(or_rhs, rhs)?;
			self.new_or_type(new_lhs, new_rhs)
		} else if let Type::Or(or_lhs, or_rhs) = right_ty {
			let (or_lhs, or_rhs) = (*or_lhs, *or_rhs);
			let new_lhs = self.new_and_type(lhs, or_lhs)?;
			let new_rhs = self.new_and_type(lhs, or_rhs)?;
			self.new_or_type(new_lhs, new_rhs)
		} else {
			let ty = Type::And(lhs, rhs);
			self.register_type(ty)
		};

		Ok(result)
	}

	/// TODO temp
	#[must_use]
	pub fn into_vec_temp(self) -> Vec<(TypeId, Type)> {
		self.types.into_iter().enumerate().map(|(idx, ty)| (TypeId(idx as u16), ty)).collect()
	}

	/// From something like: let a: number => string. Rather than a actual function
	pub fn new_function_type_annotation(
		&mut self,
		type_parameters: Option<super::poly_types::GenericTypeParameters>,
		parameters: crate::types::functions::SynthesisedParameters,
		return_type: TypeId,
		declared_at: &source_map::SpanWithSource,
		effects: Option<Vec<crate::events::Event>>,
		constant_function: Option<String>,
	) -> TypeId {
		let id = crate::FunctionId(declared_at.source, declared_at.start);
		let function_type = FunctionType {
			type_parameters,
			parameters,
			return_type,
			effects,
			// TODO
			free_variables: Default::default(),
			closed_over_variables: Default::default(),
			// TODO
			behavior: FunctionBehavior::ArrowFunction { is_async: false },
			constant_function,
			id,
		};
		self.functions.insert(id, function_type);
		self.register_type(Type::FunctionReference(id))
	}

	/// TODO this registers 3 new types, is there a smaller way
	pub fn new_conditional_extends_type(
		&mut self,
		check_type: TypeId,
		extends: TypeId,
		true_result: TypeId,
		false_result: TypeId,
	) -> TypeId {
		let on = self.register_type(Type::Constructor(super::Constructor::TypeRelationOperator(
			TypeRelationOperator::Extends { ty: check_type, extends },
		)));
		self.new_conditional_type(on, true_result, false_result)
	}

	pub fn new_conditional_type(
		&mut self,
		condition: TypeId,
		truthy_result: TypeId,
		otherwise_result: TypeId,
	) -> TypeId {
		// TODO raise warning
		if truthy_result == otherwise_result {
			return truthy_result;
		}
		// TODO on is negation then swap operands
		let ty = Type::Constructor(super::Constructor::ConditionalResult {
			condition,
			truthy_result,
			otherwise_result,
			result_union: self.new_or_type(truthy_result, otherwise_result),
		});
		self.register_type(ty)
	}

	pub fn new_anonymous_interface_ty(&mut self) -> TypeId {
		let ty = Type::Object(super::ObjectNature::AnonymousTypeAnnotation);
		self.register_type(ty)
	}

	/// Doesn't do constant compilation
	pub(crate) fn new_logical_negation_type(&mut self, operand: TypeId) -> TypeId {
		let ty = Type::Constructor(Constructor::UnaryOperator {
			operator: crate::features::operations::PureUnary::LogicalNot,
			operand,
		});
		self.register_type(ty)
	}

	/// Note it does call it over every context
	/// For:
	/// - Properties
	/// - Equality
	/// - Functions
	pub(crate) fn get_fact_about_type<C: InformationChain, TData: Copy, TResult>(
		&self,
		info_chain: &C,
		on: TypeId,
		on_type_arguments: Option<&GenericChain>,
		resolver: &impl Fn(
			&LocalInformation,
			&TypeStore,
			TypeId,
			Option<&GenericChain>,
			TData,
		) -> Option<TResult>,
		data: TData,
	) -> PossibleLogical<TResult> {
		if on == TypeId::ERROR_TYPE {
			return Err(crate::context::Missing::Error);
		}
		if on == TypeId::ANY_TYPE {
			// TODO any
			return Err(crate::context::Missing::Infer { on });
		}

		match self.get_type_by_id(on) {
			Type::SpecialObject(SpecialObjects::Function(..)) => {
				let on_function = info_chain
					.get_chain_of_info()
					.find_map(|info| resolver(info, self, on, on_type_arguments, data))
					.map(Logical::Pure);

				// TODO undecided on this
				on_function
					.or_else(|| {
						self.get_fact_about_type(
							info_chain,
							TypeId::FUNCTION_TYPE,
							on_type_arguments,
							resolver,
							data,
						)
						.ok()
					})
					.ok_or(crate::context::Missing::None)
			}
			Type::FunctionReference(_) => {
				let on_function = info_chain
					.get_chain_of_info()
					.find_map(|info| resolver(info, self, on, on_type_arguments, data))
					.map(Logical::Pure);

				// TODO undecided on this
				on_function
					.or_else(|| {
						self.get_fact_about_type(
							info_chain,
							TypeId::FUNCTION_TYPE,
							on_type_arguments,
							resolver,
							data,
						)
						.ok()
					})
					.ok_or(crate::context::Missing::None)
			}
			Type::AliasTo { to, .. } => {
				let property_on_self = info_chain
					.get_chain_of_info()
					.find_map(|info| resolver(info, self, on, on_type_arguments, data))
					.map(Logical::Pure);

				property_on_self
					.or_else(|| {
						self.get_fact_about_type(info_chain, *to, on_type_arguments, resolver, data)
							.ok()
					})
					.ok_or(crate::context::Missing::None)
			}

			Type::And(left, right) => self
				.get_fact_about_type(info_chain, *left, on_type_arguments, resolver, data)
				.ok()
				.or_else(|| {
					self.get_fact_about_type(info_chain, *right, on_type_arguments, resolver, data)
						.ok()
				})
				.ok_or(crate::context::Missing::None),

			Type::Or(left, right) => {
				let left =
					self.get_fact_about_type(info_chain, *left, on_type_arguments, resolver, data);
				let right =
					self.get_fact_about_type(info_chain, *right, on_type_arguments, resolver, data);

				// TODO throwaway if both Missing::None

				Ok(Logical::Or {
					based_on: TypeId::BOOLEAN_TYPE,
					left: Box::new(left),
					right: Box::new(right),
				})
			}
			Type::RootPolyType(_nature) => {
				// Can assign to properties on parameters etc
				let on_root_type = info_chain
					.get_chain_of_info()
					.find_map(|info| resolver(info, self, on, on_type_arguments, data))
					.map(Logical::Pure);

				on_root_type
					.or_else(|| {
						let aliases =
							get_constraint(on, self).expect("poly type with no constraint");
						self.get_fact_about_type(
							info_chain,
							aliases,
							on_type_arguments,
							resolver,
							data,
						)
						.ok()
					})
					.ok_or(crate::context::Missing::None)
			}
			Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
				on: base,
				arguments,
			})) => {
				let on_sg_type = info_chain
					.get_chain_of_info()
					.find_map(|info| resolver(info, self, on, on_type_arguments, data))
					.map(Logical::Pure);

				on_sg_type
					.or_else(|| {
						let on_type_arguments = GenericChain::append(on_type_arguments, arguments);
						let fact_opt = self
							.get_fact_about_type(
								info_chain,
								*base,
								Some(&on_type_arguments),
								resolver,
								data,
							)
							.ok();

						fact_opt.map(|fact| Logical::Implies {
							on: Box::new(fact),
							antecedent: arguments.clone(),
						})
					})
					.ok_or(crate::context::Missing::None)
			}
			Type::Constructor(Constructor::ConditionalResult {
				condition,
				truthy_result,
				otherwise_result,
				result_union: _,
			}) => {
				let left = self.get_fact_about_type(
					info_chain,
					*truthy_result,
					on_type_arguments,
					resolver,
					data,
				);
				let right = self.get_fact_about_type(
					info_chain,
					*otherwise_result,
					on_type_arguments,
					resolver,
					data,
				);

				// TODO throwaway if both Missing::None

				Ok(Logical::Or {
					based_on: *condition,
					left: Box::new(left),
					right: Box::new(right),
				})
			}
			Type::Constructor(_constructor) => {
				let on_constructor_type = info_chain
					.get_chain_of_info()
					.find_map(|info| resolver(info, self, on, on_type_arguments, data))
					.map(Logical::Pure);

				on_constructor_type
					.or_else(|| {
						// TODO implies ???
						let constraint =
							get_constraint(on, self).expect("no constraint for constructor");
						// TODO might need to send more information here, rather than forgetting via .get_type
						self.get_fact_about_type(
							info_chain,
							constraint,
							on_type_arguments,
							resolver,
							data,
						)
						.ok()
					})
					.ok_or(crate::context::Missing::None)
			}
			Type::Object(..) => {
				let object_constraint_structure_generics =
					if let Some(object_constraint) = info_chain
						.get_chain_of_info()
						.find_map(|c| c.object_constraints.get(&on).copied())
					{
						if let Type::Constructor(Constructor::StructureGenerics(
							StructureGenerics { arguments, .. },
						)) = self.get_type_by_id(object_constraint)
						{
							Some(arguments)
						} else {
							None
						}
					} else {
						None
					};

				let prototype = info_chain
					.get_chain_of_info()
					.find_map(|facts| facts.prototypes.get(&on))
					.copied();

				let generics = if let gens @ Some(_) = object_constraint_structure_generics {
					gens.cloned()
				} else if let Some(prototype) = prototype {
					self.get_look_up_generic_map_from_prototype(prototype, on).map(|properties| {
						StructureGenericArguments {
							type_restrictions: SmallMap::new(),
							properties,
							closures: Vec::new(),
						}
					})
				} else {
					None
				};

				info_chain
					.get_chain_of_info()
					.find_map(|env| resolver(env, self, on, on_type_arguments, data))
					.map(|result| {
						let pure = Logical::Pure(result);
						if let Some(ref generics) = generics {
							// TODO clone
							Logical::Implies { on: Box::new(pure), antecedent: generics.clone() }
						} else {
							pure
						}
					})
					.or_else(|| {
						if let Some(prototype) = prototype {
							self.get_fact_about_type(
								info_chain,
								prototype,
								on_type_arguments,
								resolver,
								data,
							)
							.map(|result| {
								if let Some(generics) = generics {
									Logical::Implies {
										on: Box::new(result),
										// TODO clone
										antecedent: generics.clone(),
									}
								} else {
									result
								}
							})
							.ok()
						} else {
							None
						}
					})
					.ok_or(crate::context::Missing::None)
			}
			Type::Interface { extends, .. } => info_chain
				.get_chain_of_info()
				.find_map(|env| resolver(env, self, on, on_type_arguments, data))
				.map(Logical::Pure)
				.or_else(|| {
					if let Some(extends) = extends {
						self.get_fact_about_type(
							info_chain,
							*extends,
							on_type_arguments,
							resolver,
							data,
						)
						.ok()
					} else {
						None
					}
				})
				.ok_or(crate::context::Missing::None),
			Type::Constant(cst) => info_chain
				.get_chain_of_info()
				.find_map(|info| resolver(info, self, on, on_type_arguments, data))
				.map(Logical::Pure)
				.or_else(|| {
					let backing_type = cst.get_backing_type_id();
					self.get_fact_about_type(
						info_chain,
						backing_type,
						on_type_arguments,
						resolver,
						data,
					)
					.ok()
				})
				.ok_or(crate::context::Missing::None),
			Type::SpecialObject(_) => todo!(),
		}
	}

	pub(crate) fn get_look_up_generic_map_from_prototype(
		&self,
		prototype: TypeId,
		on: TypeId,
	) -> Option<LookUpGenericMap> {
		if prototype == TypeId::ARRAY_TYPE {
			// Cannot create a union here
			Some(SmallMap::from_iter([(
				TypeId::T_TYPE,
				super::LookUpGeneric::NumberPropertyOf(on),
			)]))
		} else if let Type::Interface { parameters: Some(_parameters), .. } =
			self.get_type_by_id(prototype)
		{
			todo!("Should be stored during type definition synthesis")
		} else {
			None
		}
	}

	pub(crate) fn get_look_up_generic_from_prototype(
		&self,
		prototype: TypeId,
		on: TypeId,
	) -> Option<LookUpGeneric> {
		if prototype == TypeId::ARRAY_TYPE {
			// Cannot create a union here
			Some(super::LookUpGeneric::NumberPropertyOf(on))
		} else if let Type::Interface { parameters: Some(_parameters), .. } =
			self.get_type_by_id(prototype)
		{
			todo!("Should be stored during type definition synthesis")
		} else {
			None
		}
	}

	pub fn new_closure_id(&mut self) -> ClosureId {
		self.closure_counter += 1;
		ClosureId(self.closure_counter)
	}

	#[must_use]
	pub fn get_function_from_id(&self, id: FunctionId) -> &FunctionType {
		self.functions.get(&id).unwrap()
	}

	pub fn new_function_type(&mut self, function_type: FunctionType) -> TypeId {
		let id = function_type.id;
		self.functions.insert(id, function_type);
		self.register_type(Type::SpecialObject(SpecialObjects::Function(id, Default::default())))
	}

	/// TODO WIP
	#[allow(clippy::similar_names)]
	pub(crate) fn new_property_on_type_annotation(
		&mut self,
		indexee: TypeId,
		indexer: TypeId,
		environment: &Environment,
	) -> TypeId {
		if let Some(base) = get_constraint(indexee, self) {
			let under = PropertyKey::from_type(indexer, self);
			let ty = Type::Constructor(Constructor::Property {
				on: indexee,
				under,
				result: base,
				bind_this: true,
			});
			self.register_type(ty)
		} else if let Ok(prop) = get_property_unbound(
			indexee,
			crate::context::information::Publicity::Public,
			&PropertyKey::from_type(indexer, self),
			self,
			environment,
		) {
			match prop {
				Logical::Pure(ty) => ty.as_get_type(),
				Logical::Or { .. } => todo!(),
				Logical::Implies { .. } => todo!(),
			}
		} else {
			crate::utils::notify!("Error: no index on type annotation");
			TypeId::ERROR_TYPE
		}
	}

	/// TODO flags
	pub fn new_regex(&mut self, pattern: String) -> TypeId {
		self.register_type(Type::SpecialObject(crate::features::objects::SpecialObjects::Regexp(
			pattern,
		)))
	}

	pub fn new_function_parameter(&mut self, parameter_constraint: TypeId) -> TypeId {
		if let Type::RootPolyType(_) = self.get_type_by_id(parameter_constraint) {
			parameter_constraint
		} else {
			self.register_type(Type::RootPolyType(crate::types::PolyNature::Parameter {
				fixed_to: parameter_constraint,
			}))
		}
	}

	pub fn new_array_type(&mut self, item_type: TypeId, position: SpanWithSource) -> TypeId {
		let ty = Type::Constructor(Constructor::StructureGenerics(StructureGenerics {
			on: TypeId::ARRAY_TYPE,
			arguments: StructureGenericArguments {
				type_restrictions: FromIterator::from_iter([(
					TypeId::T_TYPE,
					(item_type, position),
				)]),
				properties: map_vec::Map::new(),
				closures: Default::default(),
			},
		}));
		self.register_type(ty)
	}

	/// TODO WIP
	pub fn new_open_type(&mut self, base: TypeId) -> TypeId {
		self.register_type(Type::RootPolyType(PolyNature::Open(base)))
	}

	/// *Dangerous* type modifying types. TODO this might be modified in the future
	pub(crate) fn modify_interface_type_parameter_constraint(
		&mut self,
		ty: TypeId,
		new_constraint: TypeId,
	) {
		if let Type::RootPolyType(PolyNature::Generic { ref mut eager_fixed, .. }) =
			self.types[ty.0 as usize]
		{
			*eager_fixed = new_constraint;
		} else {
			unreachable!()
		}
	}

	/// *Dangerous* type modifying types. TODO this might be modified in the future
	pub(crate) fn set_extends_on_interface(&mut self, interface_type: TypeId, new_extends: TypeId) {
		if let Type::Interface { ref mut extends, .. } = self.types[interface_type.0 as usize] {
			*extends = Some(new_extends);
		} else {
			unreachable!()
		}
	}
}

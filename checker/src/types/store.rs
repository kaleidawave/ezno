use std::collections::{HashMap, HashSet};

use crate::{types::intrinsics::Intrinsic, Constant, Map as SmallMap};
use source_map::{Nullable, SpanWithSource};

use crate::{
	features::{functions::ClosureId, objects::SpecialObject},
	types::{
		functions::{FunctionBehavior, FunctionType},
		logical::{Logical, LogicalOrValid},
		PolyNature, Type,
	},
	Environment, FunctionId, TypeId,
};

use super::{
	generics::generic_type_arguments::GenericArguments, get_constraint, properties::PropertyKey,
	Constructor, LookUpGeneric, LookUpGenericMap, PartiallyAppliedGenerics, TypeExtends,
};

/// Holds all the types. Eventually may be split across modules
#[derive(Debug, binary_serialize_derive::BinarySerializable)]
pub struct TypeStore {
	/// Contains all of the types. Indexed by [`TypeId`]
	types: Vec<Type>,

	/// Some types are prototypes but have generic parameters but
	pub(crate) lookup_generic_map: HashMap<TypeId, LookUpGenericMap>,

	/// Contains all the function types
	///
	/// TODO is there a faster alternative to a [`HashMap`] like how [`Type`]s are stored in a [`Vec`]
	pub(crate) functions: HashMap<FunctionId, FunctionType>,

	/// can be used for tree shaking
	pub called_functions: HashSet<FunctionId>,

	/// TODO not best place but is passed through everything so
	pub(crate) closure_counter: u32,
}

impl Default for TypeStore {
	fn default() -> Self {
		// These have to be in the order of TypeId
		let types = vec![
			// TODO will `TypeId::ANY_TYPE` cause any problems
			Type::RootPolyType(PolyNature::Error(TypeId::ANY_TYPE)),
			Type::Interface { name: "never".to_owned(), parameters: None, extends: None },
			Type::Interface { name: "any".to_owned(), parameters: None, extends: None },
			Type::Class { name: "boolean".to_owned(), type_parameters: None },
			Type::Class { name: "number".to_owned(), type_parameters: None },
			Type::Class { name: "string".to_owned(), type_parameters: None },
			// sure?
			Type::Interface { name: "undefined".to_owned(), parameters: None, extends: None },
			Type::SpecialObject(SpecialObject::Null),
			// `void` type. Has special subtyping in returns
			Type::AliasTo { to: TypeId::UNDEFINED_TYPE, name: "void".into(), parameters: None },
			Type::Class { name: "Array".to_owned(), type_parameters: Some(vec![TypeId::T_TYPE]) },
			Type::Class { name: "Promise".to_owned(), type_parameters: Some(vec![TypeId::T_TYPE]) },
			// Array and Promise type parameter. Simplifies things
			Type::RootPolyType(PolyNature::StructureGeneric {
				name: "T".into(),
				extends: TypeId::ANY_TYPE,
			}),
			Type::Interface { name: "object".to_owned(), parameters: None, extends: None },
			Type::Class { name: "Function".to_owned(), type_parameters: None },
			Type::Class { name: "RegExp".to_owned(), type_parameters: None },
			Type::Class { name: "Symbol".to_owned(), type_parameters: None },
			// true
			Type::Constant(Constant::Boolean(true)),
			// false
			Type::Constant(Constant::Boolean(false)),
			// zero
			Type::Constant(Constant::Number(0.into())),
			// one
			Type::Constant(Constant::Number(1.into())),
			// NaN
			Type::Constant(Constant::NaN),
			// ""
			Type::Constant(Constant::String(String::new())),
			// inferred this free variable shortcut
			Type::RootPolyType(PolyNature::FreeVariable {
				reference: crate::events::RootReference::This,
				based_on: TypeId::ANY_TYPE,
			}),
			Type::RootPolyType(PolyNature::FunctionGeneric {
				name: "new.target".to_owned(),
				// TODO
				extends: TypeId::ANY_TYPE,
			}),
			Type::Interface { name: "ImportMeta".to_owned(), parameters: None, extends: None },
			Type::Constant(Constant::Symbol { key: "iterator".to_owned() }),
			Type::Constant(Constant::Symbol { key: "asyncIterator".to_owned() }),
			Type::Constant(Constant::Symbol { key: "hasInstance".to_owned() }),
			Type::Constant(Constant::Symbol { key: "toPrimitive".to_owned() }),
			Type::RootPolyType(PolyNature::StructureGeneric {
				name: "S".into(),
				extends: TypeId::STRING_TYPE,
			}),
			Type::AliasTo {
				to: TypeId::STRING_TYPE,
				name: "Uppercase".into(),
				parameters: Some(vec![TypeId::STRING_GENERIC]),
			},
			Type::AliasTo {
				to: TypeId::STRING_TYPE,
				name: "Lowercase".into(),
				parameters: Some(vec![TypeId::STRING_GENERIC]),
			},
			Type::AliasTo {
				to: TypeId::STRING_TYPE,
				name: "Capitalize".into(),
				parameters: Some(vec![TypeId::STRING_GENERIC]),
			},
			Type::AliasTo {
				to: TypeId::STRING_TYPE,
				name: "Uncapitalize".into(),
				parameters: Some(vec![TypeId::STRING_GENERIC]),
			},
			// Yeah
			Type::AliasTo {
				to: TypeId::T_TYPE,
				name: "NoInfer".into(),
				parameters: Some(vec![TypeId::T_TYPE]),
			},
			Type::AliasTo {
				name: "Readonly".into(),
				to: TypeId::T_TYPE,
				parameters: Some(vec![TypeId::T_TYPE]),
			},
			Type::RootPolyType(PolyNature::MappedGeneric {
				name: "NonOptional".into(),
				extends: TypeId::BOOLEAN_TYPE,
			}),
			Type::RootPolyType(PolyNature::MappedGeneric {
				name: "Writable".into(),
				extends: TypeId::BOOLEAN_TYPE,
			}),
			Type::RootPolyType(PolyNature::StructureGeneric {
				name: "T".into(),
				extends: TypeId::NUMBER_TYPE,
			}),
			Type::AliasTo {
				to: TypeId::NUMBER_TYPE,
				name: "LessThan".into(),
				parameters: Some(vec![TypeId::NUMBER_GENERIC]),
			},
			Type::AliasTo {
				to: TypeId::NUMBER_TYPE,
				name: "GreaterThan".into(),
				parameters: Some(vec![TypeId::NUMBER_GENERIC]),
			},
			Type::AliasTo {
				to: TypeId::NUMBER_TYPE,
				name: "MultipleOf".into(),
				parameters: Some(vec![TypeId::NUMBER_GENERIC]),
			},
			// Intermediate for the below
			Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
				on: TypeId::NOT_RESTRICTION,
				arguments: GenericArguments::ExplicitRestrictions(crate::Map::from_iter([(
					TypeId::T_TYPE,
					(TypeId::NAN, SpanWithSource::NULL),
				)])),
			}),
			Type::And(TypeId::NUMBER_TYPE, TypeId::NOT_NOT_A_NUMBER),
			// TODO WIP
			Type::AliasTo {
				name: "Literal".into(),
				to: TypeId::T_TYPE,
				parameters: Some(vec![TypeId::T_TYPE]),
			},
			Type::AliasTo {
				name: "Exclusive".into(),
				to: TypeId::T_TYPE,
				parameters: Some(vec![TypeId::T_TYPE]),
			},
			Type::AliasTo {
				name: "Not".into(),
				to: TypeId::ANY_TYPE,
				parameters: Some(vec![TypeId::T_TYPE]),
			},
			Type::AliasTo {
				name: "CaseInsensitive".into(),
				to: TypeId::STRING_TYPE,
				parameters: Some(vec![TypeId::STRING_GENERIC]),
			},
			Type::RootPolyType(PolyNature::Open(TypeId::BOOLEAN_TYPE)),
			Type::RootPolyType(PolyNature::Open(TypeId::NUMBER_TYPE)),
			Type::Or(TypeId::STRING_TYPE, TypeId::NUMBER_TYPE),
		];

		// Check that above is correct, TODO eventually a macro
		assert_eq!(types.len(), TypeId::INTERNAL_TYPE_COUNT);

		let lookup_generic_map = HashMap::from_iter([(
			TypeId::ARRAY_TYPE,
			SmallMap::from_iter([(TypeId::T_TYPE, LookUpGeneric::NumberPropertyOfSelf)]),
		)]);

		Self {
			types,
			lookup_generic_map,
			functions: Default::default(),
			called_functions: Default::default(),
			closure_counter: 0,
		}
	}
}

impl TypeStore {
	#[must_use]
	pub fn count_of_types(&self) -> usize {
		self.types.len()
	}

	pub fn new_constant_type(&mut self, constant: Constant) -> crate::TypeId {
		// Reuse existing ids rather than creating new types sometimes
		match constant {
			Constant::String(s) if s.is_empty() => TypeId::EMPTY_STRING,
			Constant::Number(number) if number == 0f64 => TypeId::ZERO,
			Constant::Number(number) if number == 1f64 => TypeId::ONE,
			Constant::Boolean(value) => {
				if value {
					TypeId::TRUE
				} else {
					TypeId::FALSE
				}
			}
			Constant::NaN => TypeId::NAN,
			_ => {
				let ty = Type::Constant(constant);
				// TODO maybe separate id
				self.register_type(ty)
			}
		}
	}

	// pub(crate) fn register_type(&mut self, ty: Type) -> TypeId {
	pub fn register_type(&mut self, ty: Type) -> TypeId {
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
			return TypeId::OPEN_BOOLEAN_TYPE;
		}
		if let TypeId::NEVER_TYPE = lhs {
			return rhs;
		}
		if let TypeId::NEVER_TYPE = rhs {
			return lhs;
		}
		// Normalise to ltr
		if let Type::Or(lhs_lhs, lhs_rhs) = self.get_type_by_id(lhs) {
			let new_lhs = *lhs_lhs;
			let rhs = self.new_or_type(*lhs_rhs, rhs);
			return self.new_or_type(new_lhs, rhs);
		}

		// TODO recursive contains
		if let Type::Or(rhs_lhs, rhs_rhs) = self.get_type_by_id(rhs) {
			if lhs == *rhs_lhs {
				return self.new_or_type(lhs, *rhs_rhs);
			} else if lhs == *rhs_rhs {
				return self.new_or_type(lhs, *rhs_lhs);
			}
		}

		let ty = Type::Or(lhs, rhs);
		self.register_type(ty)
	}

	#[must_use]
	pub fn new_or_type_from_iterator(&mut self, iter: impl IntoIterator<Item = TypeId>) -> TypeId {
		iter.into_iter().reduce(|acc, n| self.new_or_type(acc, n)).unwrap_or(TypeId::NEVER_TYPE)
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
		} else if left_ty.is_nominal() && right_ty.is_nominal() {
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
		type_parameters: Option<super::generics::GenericTypeParameters>,
		parameters: crate::types::functions::SynthesisedParameters,
		return_type: TypeId,
		declared_at: &source_map::SpanWithSource,
	) -> TypeId {
		let id = crate::FunctionId(declared_at.source, declared_at.start);
		let function_type = FunctionType {
			type_parameters,
			parameters,
			return_type,
			effect: super::FunctionEffect::Unknown,
			behavior: FunctionBehavior::ArrowFunction { is_async: false },
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
		let on =
			self.register_type(Type::Constructor(super::Constructor::TypeExtends(TypeExtends {
				item: check_type,
				extends,
			})));
		self.new_conditional_type(on, true_result, false_result)
	}

	#[allow(clippy::if_same_then_else)]
	pub fn new_conditional_type(
		&mut self,
		condition: TypeId,
		truthy_result: TypeId,
		otherwise_result: TypeId,
	) -> TypeId {
		// TODO raise warning for the first 4 branches
		if truthy_result == otherwise_result {
			truthy_result
		} else if condition == TypeId::TRUE {
			truthy_result
		} else if condition == TypeId::FALSE {
			otherwise_result
		} else if truthy_result == TypeId::TRUE && otherwise_result == TypeId::FALSE {
			condition
		} else if let Type::Constructor(Constructor::UnaryOperator {
			operator: crate::features::operations::PureUnary::LogicalNot,
			operand: reversed_condition,
		}) = self.get_type_by_id(condition)
		{
			self.new_conditional_type(*reversed_condition, otherwise_result, truthy_result)
		} else {
			let ty = Type::Constructor(super::Constructor::ConditionalResult {
				condition,
				truthy_result,
				otherwise_result,
				// TODO remove
				result_union: self.new_or_type(truthy_result, otherwise_result),
			});
			self.register_type(ty)
		}
	}

	pub fn new_anonymous_interface_type(
		&mut self,
		properties: super::properties::Properties,
	) -> TypeId {
		let ty = Type::Object(super::ObjectNature::AnonymousTypeAnnotation(properties));
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

	/// Doesn't evaluate events
	pub(crate) fn new_logical_and_type(&mut self, left: TypeId, right: TypeId) -> TypeId {
		self.new_conditional_type(left, right, TypeId::FALSE)
	}

	/// Doesn't evaluate events
	pub(crate) fn new_logical_or_type(&mut self, left: TypeId, right: TypeId) -> TypeId {
		self.new_conditional_type(left, TypeId::TRUE, right)
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
		self.register_type(Type::SpecialObject(SpecialObject::Function(id, Default::default())))
	}

	pub fn new_hoisted_function_type(&mut self, function_type: FunctionType) -> TypeId {
		let id = function_type.id;
		self.functions.insert(id, function_type);
		self.register_type(Type::FunctionReference(id))
	}

	/// TODO WIP
	#[allow(clippy::similar_names)]
	pub(crate) fn new_property_on_type_annotation(
		&mut self,
		indexee: TypeId,
		indexer: TypeId,
		environment: &Environment,
	) -> TypeId {
		use super::properties::{get_property_unbound, AccessMode, Publicity};
		if get_constraint(indexee, self).is_some() {
			let under = PropertyKey::from_type(indexer, self);
			let ty = Type::Constructor(Constructor::Property {
				on: indexee,
				under,
				result: TypeId::ANY_TYPE,
				mode: AccessMode::Regular,
			});
			self.register_type(ty)
		} else {
			let result = get_property_unbound(
				(indexee, None),
				(Publicity::Public, &PropertyKey::from_type(indexer, self), None),
				false,
				environment,
				self,
			);
			if let Ok(prop) = result {
				match prop {
					LogicalOrValid::Logical(Logical::Pure(ty)) => ty.as_get_type(self),
					value => {
						crate::utilities::notify!("value={:?}", value);
						TypeId::ERROR_TYPE
					} // Logical::Or { .. } => todo!(),
					  // Logical::Implies { .. } => todo!(),
					  // Logical::BasedOnKey { .. } => todo!(),
				}
			} else {
				crate::utilities::notify!("Error: no index on type annotation");
				TypeId::ERROR_TYPE
			}
		}
	}

	pub fn new_function_parameter(&mut self, parameter_constraint: TypeId) -> TypeId {
		// TODO this has problems if there are two generic types. Aka `(a: T, b: T) -> T`. Although I have
		// no idea why this is possible so should be fine?
		if let Type::RootPolyType(PolyNature::FunctionGeneric { .. }) =
			self.get_type_by_id(parameter_constraint)
		{
			parameter_constraint
		} else {
			self.register_type(Type::RootPolyType(crate::types::PolyNature::Parameter {
				fixed_to: parameter_constraint,
			}))
		}
	}

	pub fn new_array_type(&mut self, item_type: TypeId, position: SpanWithSource) -> TypeId {
		let ty = Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::ARRAY_TYPE,
			arguments: GenericArguments::ExplicitRestrictions(FromIterator::from_iter([(
				TypeId::T_TYPE,
				(item_type, position),
			)])),
		});
		self.register_type(ty)
	}

	/// See [`PolyNature::Open`]
	pub fn new_open_type(&mut self, base: TypeId) -> TypeId {
		self.register_type(Type::RootPolyType(PolyNature::Open(base)))
	}

	pub fn new_narrowed(&mut self, from: TypeId, narrowed_to: TypeId) -> TypeId {
		self.register_type(Type::Narrowed { from, narrowed_to })
	}

	/// For any synthesis errors to keep the program going a type is needed.
	/// Most of the time use [`TypeId:ERROR_TYPE`] which is generic any like type.
	/// However sometimes we can use some type annotation instead to still leave some information.
	/// This method creates one of these
	pub(crate) fn new_error_type(&mut self, base: TypeId) -> TypeId {
		self.register_type(Type::RootPolyType(PolyNature::Error(base)))
	}

	/// *Dangerous* type modifying types. TODO this might be modified in the future
	/// TODO check disjoint
	pub(crate) fn modify_interface_type_parameter_constraint(
		&mut self,
		ty: TypeId,
		constraint: TypeId,
	) {
		let ty = &mut self.types[ty.0 as usize];
		if let Type::RootPolyType(PolyNature::StructureGeneric { extends, .. }) = ty {
			*extends = constraint;
		} else {
			todo!("{:?}", ty)
		}
	}

	/// *Dangerous* . TODO WIP
	pub(crate) fn _set_inferred_constraint(&mut self, ty: TypeId, constraint: TypeId) {
		if let Some(Type::RootPolyType(PolyNature::Parameter { fixed_to })) =
			self.types.get_mut(ty.0 as usize)
		{
			*fixed_to = constraint;
		} else {
			panic!()
		}
	}

	/// *Dangerous* type modifying types. TODO this might be modified in the future
	pub(crate) fn set_extends_on_interface(&mut self, interface_type: TypeId, extends: TypeId) {
		if let Type::Interface { extends: Some(ref mut old), .. } =
			self.types[interface_type.0 as usize]
		{
			*old = extends;
		}
	}

	/// *Dangerous* type modifying types. TODO this might be modified in the future
	pub(crate) fn update_alias(&mut self, alias_type: TypeId, to: TypeId) {
		if let Type::AliasTo { to: ref mut old, .. } = self.types[alias_type.0 as usize] {
			*old = to;
		}
	}

	/// *Dangerous* type modifying types. TODO this might be modified in the future
	pub(crate) fn update_generic_extends(&mut self, generic: TypeId, to: TypeId) {
		if let Type::RootPolyType(PolyNature::StructureGeneric { ref mut extends, .. }) =
			self.types[generic.0 as usize]
		{
			*extends = to;
		}
	}

	pub(crate) fn new_class_constructor_type(&mut self, constructor: FunctionType) -> TypeId {
		let id = constructor.id;
		self.functions.insert(id, constructor);
		self.register_type(Type::SpecialObject(SpecialObject::Function(
			id,
			crate::types::calling::ThisValue::UseParent,
		)))
	}

	pub(crate) fn create_this_object(&mut self) -> TypeId {
		self.register_type(Type::Object(super::ObjectNature::RealDeal))
	}

	pub(crate) fn new_key_of(&mut self, of: TypeId) -> TypeId {
		self.register_type(Type::Constructor(Constructor::KeyOf(of)))
	}

	pub(crate) fn new_intrinsic(&mut self, intrinsic: &Intrinsic, argument: TypeId) -> TypeId {
		let (on, to_pair) = match intrinsic {
			Intrinsic::Uppercase => (TypeId::STRING_UPPERCASE, TypeId::STRING_GENERIC),
			Intrinsic::Lowercase => (TypeId::STRING_LOWERCASE, TypeId::STRING_GENERIC),
			Intrinsic::Capitalize => (TypeId::STRING_CAPITALIZE, TypeId::STRING_GENERIC),
			Intrinsic::Uncapitalize => (TypeId::STRING_UNCAPITALIZE, TypeId::STRING_GENERIC),
			Intrinsic::NoInfer => (TypeId::NO_INFER, TypeId::T_TYPE),
			Intrinsic::Literal => (TypeId::LITERAL_RESTRICTION, TypeId::T_TYPE),
			Intrinsic::LessThan => (TypeId::LESS_THAN, TypeId::NUMBER_GENERIC),
			Intrinsic::GreaterThan => (TypeId::GREATER_THAN, TypeId::NUMBER_GENERIC),
			Intrinsic::MultipleOf => (TypeId::MULTIPLE_OF, TypeId::NUMBER_GENERIC),
			Intrinsic::Exclusive => (TypeId::EXCLUSIVE_RESTRICTION, TypeId::T_TYPE),
			Intrinsic::Not => {
				// Double negation
				if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
					on: TypeId::NOT_RESTRICTION,
					arguments: GenericArguments::ExplicitRestrictions(args),
				}) = self.get_type_by_id(argument)
				{
					return args.get(&TypeId::T_TYPE).unwrap().0;
				}

				(TypeId::NOT_RESTRICTION, TypeId::T_TYPE)
			}
			Intrinsic::CaseInsensitive => (TypeId::CASE_INSENSITIVE, TypeId::STRING_GENERIC),
		};
		let arguments = GenericArguments::ExplicitRestrictions(crate::Map::from_iter([(
			to_pair,
			(argument, <SpanWithSource as source_map::Nullable>::NULL),
		)]));

		self.register_type(Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on,
			arguments,
		}))
	}
}

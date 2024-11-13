use super::{
	get_constraint, properties, subtyping, Constant, Constructor, GenericArguments, GenericChain,
	InformationChain, MathematicalOrBitwiseOperation, PartiallyAppliedGenerics, PolyNature, Type,
	TypeId, TypeStore,
};

pub(crate) fn get_structure_arguments_based_on_object_constraint<'a, C: InformationChain>(
	object: TypeId,
	info_chain: &C,
	types: &'a TypeStore,
) -> Option<&'a GenericArguments> {
	if let Some(object_constraint) =
		info_chain.get_chain_of_info().find_map(|c| c.object_constraints.get(&object).copied())
	{
		let ty = types.get_type_by_id(object_constraint);
		if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics { arguments, .. }) = ty {
			Some(arguments)
		} else {
			crate::utilities::notify!("Generics might be missed here {:?}", ty);
			None
		}
	} else {
		None
	}
}

pub(crate) fn tuple_like(id: TypeId, types: &TypeStore, environment: &crate::Environment) -> bool {
	// TODO should be `ObjectNature::AnonymousObjectType` or something else
	let ty = types.get_type_by_id(id);
	if let Type::Object(super::ObjectNature::RealDeal) = ty {
		environment
			.get_chain_of_info()
			.any(|info| info.prototypes.get(&id).is_some_and(|p| *p == TypeId::ARRAY_TYPE))
	} else if let Type::AliasTo { to, .. } = ty {
		tuple_like(*to, types, environment)
	} else {
		false
	}
}

pub(crate) fn _unfold_tuple(_ty: TypeId) -> TypeId {
	// return Type::PropertyOf()
	todo!()
}

pub(crate) fn _assign_to_tuple(_ty: TypeId) -> TypeId {
	todo!()
	// if let PropertyKey::Type(slice) =
}

pub fn get_array_length(
	ctx: &impl InformationChain,
	on: TypeId,
	types: &TypeStore,
) -> Result<ordered_float::NotNan<f64>, Option<TypeId>> {
	let length_property = properties::PropertyKey::String(std::borrow::Cow::Borrowed("length"));
	let id = properties::get_simple_property_value(ctx, on, &length_property, types).ok_or(None)?;
	if let Type::Constant(Constant::Number(n)) = types.get_type_by_id(id) {
		Ok(*n)
	} else {
		Err(Some(id))
	}
}

/// TODO name?
#[derive(Clone, Copy, Debug)]
pub enum ArrayItem {
	Member(TypeId),
	Optional(TypeId),
	Wildcard(TypeId),
}

/// WIP
pub(crate) fn as_slice(
	ty: TypeId,
	types: &TypeStore,
	environment: &crate::Environment,
) -> Result<Vec<ArrayItem>, ()> {
	if tuple_like(ty, types, environment) {
		let ty = if let Type::AliasTo { to, .. } = types.get_type_by_id(ty) { *to } else { ty };
		let properties =
			environment.get_chain_of_info().find_map(|info| info.current_properties.get(&ty));
		if let Some(properties) = properties {
			Ok(properties
				.iter()
				.filter_map(|(_, key, value)| {
					let not_length_value = !key.is_equal_to("length");
					not_length_value.then(|| {
						crate::utilities::notify!("key (should be incremental) {:?}", key);
						if key.as_number(types).is_some() {
							if let crate::PropertyValue::ConditionallyExists { .. } = value {
								ArrayItem::Optional(value.as_get_type(types))
							} else {
								ArrayItem::Member(value.as_get_type(types))
							}
						} else {
							ArrayItem::Wildcard(value.as_get_type(types))
						}
					})
				})
				.collect())
		} else {
			crate::utilities::notify!("BAD");
			Err(())
		}
	} else {
		Err(())
	}
}

/// WIP for counting slice indexes
#[derive(derive_enum_from_into::EnumFrom, Clone, Copy, Debug)]
pub enum Counter {
	On(usize),
	AddTo(TypeId),
}

impl Counter {
	/// TODO &mut or Self -> Self?
	pub fn increment(&mut self, types: &mut TypeStore) {
		match self {
			Counter::On(value) => {
				*value += 1;
			}
			Counter::AddTo(value) => {
				*value = types.register_type(Type::Constructor(Constructor::BinaryOperator {
					lhs: *value,
					operator: MathematicalOrBitwiseOperation::Add,
					rhs: TypeId::ONE,
					// TODO could be greater than
					result: TypeId::NUMBER_TYPE,
				}));
			}
		}
	}

	pub fn add_type(&mut self, ty: TypeId, types: &mut TypeStore) {
		let current = self.into_type(types);
		let new = types.register_type(Type::Constructor(Constructor::BinaryOperator {
			lhs: ty,
			operator: MathematicalOrBitwiseOperation::Add,
			rhs: current,
			// TODO could be greater than
			result: TypeId::NUMBER_TYPE,
		}));
		*self = Counter::AddTo(new);
	}

	pub(crate) fn into_property_key(self) -> properties::PropertyKey<'static> {
		match self {
			Counter::On(value) => properties::PropertyKey::from_usize(value),
			Counter::AddTo(ty) => properties::PropertyKey::Type(ty),
		}
	}

	pub(crate) fn into_type(self, types: &mut TypeStore) -> TypeId {
		match self {
			Counter::On(value) => {
				types.new_constant_type(Constant::Number((value as f64).try_into().unwrap()))
			}
			Counter::AddTo(ty) => ty,
		}
	}
}

/// To fill in for TSC behavior for mapped types
#[must_use]
pub fn references_key_of(id: TypeId, types: &TypeStore) -> bool {
	match types.get_type_by_id(id) {
		Type::AliasTo { to, .. } => references_key_of(*to, types),
		Type::Or(lhs, rhs) | Type::And(lhs, rhs) => {
			references_key_of(*lhs, types) || references_key_of(*rhs, types)
		}
		Type::RootPolyType(c) => references_key_of(c.get_constraint(), types),
		Type::Constructor(c) => {
			if let Constructor::KeyOf(..) = c {
				true
			} else if let Constructor::BinaryOperator { lhs, rhs, operator: _, result: _ } = c {
				references_key_of(*lhs, types) || references_key_of(*rhs, types)
			} else {
				crate::utilities::notify!("TODO might have missed keyof {:?}", c);
				false
			}
		}
		Type::PartiallyAppliedGenerics(a) => {
			if let GenericArguments::ExplicitRestrictions(ref e) = a.arguments {
				e.0.iter().any(|(_, (lhs, _))| references_key_of(*lhs, types))
			} else {
				false
			}
		}
		Type::Interface { .. }
		| Type::Class { .. }
		| Type::Constant(_)
		| Type::Narrowed { .. }
		| Type::FunctionReference(_)
		| Type::Object(_)
		| Type::SpecialObject(_) => false,
	}
}

#[allow(clippy::match_like_matches_macro)]
#[must_use]
pub fn _type_is_error(ty: TypeId, types: &TypeStore) -> bool {
	if ty == TypeId::UNIMPLEMENTED_ERROR_TYPE {
		true
	} else if let Type::RootPolyType(PolyNature::Error(_)) = types.get_type_by_id(ty) {
		true
	} else {
		false
	}
}

/// TODO want to skip mapped generics because that would break subtyping
#[must_use]
pub fn get_type_as_conditional(ty: TypeId, types: &TypeStore) -> Option<(TypeId, TypeId, TypeId)> {
	match types.get_type_by_id(ty) {
		Type::Constructor(crate::types::Constructor::ConditionalResult {
			condition,
			truthy_result,
			otherwise_result,
			result_union: _,
		}) => Some((*condition, *truthy_result, *otherwise_result)),
		Type::Or(left, right) => Some((TypeId::OPEN_BOOLEAN_TYPE, *left, *right)),
		// For reasons !
		Type::RootPolyType(PolyNature::MappedGeneric { .. }) => None,
		_ => {
			if let Some(constraint) = get_constraint(ty, types) {
				get_type_as_conditional(constraint, types)
			} else {
				None
			}
		}
	}
}

/// TODO wip
#[must_use]
pub fn is_pseudo_continous((ty, generics): (TypeId, GenericChain), types: &TypeStore) -> bool {
	if let TypeId::NUMBER_TYPE | TypeId::STRING_TYPE = ty {
		true
	} else if let Some(arg) = generics.as_ref().and_then(|args| args.get_single_argument(ty)) {
		is_pseudo_continous((arg, generics), types)
	} else {
		let ty = types.get_type_by_id(ty);
		if let Type::Or(left, right) = ty {
			is_pseudo_continous((*left, generics), types)
				|| is_pseudo_continous((*right, generics), types)
		} else if let Type::And(left, right) = ty {
			is_pseudo_continous((*left, generics), types)
				&& is_pseudo_continous((*right, generics), types)
		} else if let Type::RootPolyType(PolyNature::MappedGeneric { extends, .. }) = ty {
			is_pseudo_continous((*extends, generics), types)
		} else {
			false
		}
	}
}

#[must_use]
pub fn is_inferrable_type(ty: TypeId) -> bool {
	matches!(ty, TypeId::ANY_TO_INFER_TYPE | TypeId::OBJECT_TYPE)
}

/// For quick checking. Wraps [`subtyping::type_is_subtype`]
#[must_use]
pub fn simple_subtype(
	expr_ty: TypeId,
	to_satisfy: TypeId,
	information: &impl InformationChain,
	types: &TypeStore,
) -> bool {
	let mut state = subtyping::State {
		already_checked: Default::default(),
		mode: Default::default(),
		contributions: Default::default(),
		others: subtyping::SubTypingOptions { allow_errors: true },
		object_constraints: None,
	};

	subtyping::type_is_subtype(to_satisfy, expr_ty, &mut state, information, types).is_subtype()
}

// unfolds narrowing
pub fn get_origin(ty: TypeId, types: &TypeStore) -> TypeId {
	if let Type::Narrowed { from, .. } = types.get_type_by_id(ty) {
		// Hopefully don't have a nested from
		*from
	} else {
		ty
	}
}

/// Temp fix for equality of narrowing stuff
pub fn is_not_of_constant(ty: TypeId, types: &TypeStore) -> bool {
	if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
		on: TypeId::NOT_RESTRICTION,
		arguments,
	}) = types.get_type_by_id(ty)
	{
		let inner = arguments.get_structure_restriction(TypeId::T_TYPE).unwrap();
		types.get_type_by_id(inner).is_constant()
	} else {
		false
	}
}

// TODO narrowed as well
pub fn type_equal(lhs: TypeId, rhs: TypeId, types: &TypeStore) -> bool {
	if lhs == rhs {
		true
	} else if let (Type::Constant(lhs), Type::Constant(rhs)) =
		(types.get_type_by_id(lhs), types.get_type_by_id(rhs))
	{
		lhs == rhs
	} else {
		false
	}
}

#[derive(Debug, Copy, Clone)]
pub struct AndCondition(pub TypeId);

#[derive(Debug)]
pub struct OrCase(pub Vec<AndCondition>);

pub fn into_conditions(id: TypeId, types: &TypeStore) -> Vec<AndCondition> {
	let ty = types.get_type_by_id(id);

	// Temp fix
	if let Type::Constructor(Constructor::BinaryOperator { result, .. }) = ty {
		return if matches!(*result, TypeId::NUMBER_TYPE | TypeId::STRING_TYPE) {
			vec![AndCondition(id)]
		} else {
			into_conditions(*result, types)
		};
	}

	if let Type::And(lhs, rhs) = ty {
		let mut buf = into_conditions(*lhs, types);
		buf.append(&mut into_conditions(*rhs, types));
		buf
	} else if let Some(backing) = get_constraint_or_alias(id, types) {
		into_conditions(backing, types)
	} else {
		vec![AndCondition(id)]
	}
}

pub fn into_cases(id: TypeId, types: &TypeStore) -> Vec<OrCase> {
	let ty = types.get_type_by_id(id);
	if let Type::Or(lhs, rhs) = ty {
		let mut buf = into_cases(*lhs, types);
		buf.append(&mut into_cases(*rhs, types));
		buf
	} else if let Some(backing) = get_constraint_or_alias(id, types) {
		into_cases(backing, types)
	} else {
		vec![OrCase(into_conditions(id, types))]
	}
}

/// Returns the constraint or base of a constant for a type. Otherwise just return the type
#[must_use]
pub fn get_larger_type(on: TypeId, types: &TypeStore) -> TypeId {
	if let Some(poly_base) = get_constraint(on, types) {
		poly_base
	} else if let Type::Constant(cst) = types.get_type_by_id(on) {
		cst.get_backing_type()
	} else {
		on
	}
}

#[must_use]
pub fn get_constraint_or_alias(on: TypeId, types: &TypeStore) -> Option<TypeId> {
	match types.get_type_by_id(on) {
		Type::RootPolyType(rpt) => Some(rpt.get_constraint()),
		Type::Constructor(constr) => Some(constr.get_constraint()),
		Type::AliasTo { to, parameters: None, .. } => Some(*to),
		Type::Narrowed { narrowed_to, .. } => Some(*narrowed_to),
		_ => None,
	}
}

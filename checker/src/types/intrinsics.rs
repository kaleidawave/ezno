use source_map::SpanWithSource;

use crate::{
	types::{
		generics::generic_type_arguments::GenericArguments, get_constraint,
		PartiallyAppliedGenerics, TypeStore,
	},
	TypeId,
};

use super::Type;

pub use crate::utilities::float_range::FloatRange;

/// These are special marker types (using [`Type::Alias`])
///
/// Some are from TSC, others are added by me!
pub enum Intrinsic {
	/// (TSC)
	Uppercase,
	/// (TSC)
	Lowercase,
	/// (TSC)
	Capitalize,
	/// (TSC)
	Uncapitalize,
	/// Doesn't add to `Contributions` (TSC)
	NoInfer,
	/// (EZNO)
	CaseInsensitive,
	/// Has to be a known constant
	Literal,
	/// Cannot contain excess properties
	Exclusive,
	/// This value `< arg`. Also implies [`Intrinsic::NotNotANumber`]
	LessThan,
	/// This value `> arg`. Also implies [`Intrinsic::NotNotANumber`]
	GreaterThan,
	/// This value `% arg === 0`. Also implies [`Intrinsic::NotNotANumber`]
	MultipleOf,
	/// `number \ T `
	Not,
}

pub(crate) fn distribute_tsc_string_intrinsic(
	on: TypeId,
	value: TypeId,
	types: &mut TypeStore,
) -> TypeId {
	match types.get_type_by_id(value) {
		Type::Constant(crate::Constant::String(s)) => {
			if s.is_empty() {
				return value;
			}
			let transformed = apply_string_intrinsic(on, s);
			types.new_constant_type(crate::Constant::String(transformed))
		}
		Type::AliasTo { to, .. } => distribute_tsc_string_intrinsic(on, *to, types),
		Type::Or(lhs, rhs) => {
			let (lhs, rhs) = (*lhs, *rhs);
			let lhs = distribute_tsc_string_intrinsic(on, lhs, types);
			let rhs = distribute_tsc_string_intrinsic(on, rhs, types);
			types.new_or_type(lhs, rhs)
		}
		_ => types.register_type(Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on,
			arguments: GenericArguments::ExplicitRestrictions(crate::Map::from_iter([(
				TypeId::STRING_GENERIC,
				(value, <SpanWithSource as source_map::Nullable>::NULL),
			)])),
		})),
	}
}

pub(crate) fn apply_string_intrinsic(on: TypeId, s: &str) -> String {
	match on {
		TypeId::STRING_UPPERCASE => s.to_uppercase(),
		TypeId::STRING_LOWERCASE => s.to_lowercase(),
		TypeId::STRING_CAPITALIZE => {
			let mut chars = s.chars();
			let mut s = chars.next().unwrap().to_uppercase().collect::<String>();
			s.extend(chars);
			s
		}
		TypeId::STRING_UNCAPITALIZE => {
			let mut chars = s.chars();
			let mut s = chars.next().unwrap().to_lowercase().collect::<String>();
			s.extend(chars);
			s
		}
		_ => unreachable!(),
	}
}

#[must_use]
pub fn tsc_string_intrinsic(id: TypeId) -> bool {
	matches!(
		id,
		TypeId::STRING_UPPERCASE
			| TypeId::STRING_LOWERCASE
			| TypeId::STRING_CAPITALIZE
			| TypeId::STRING_UNCAPITALIZE
	)
}

#[must_use]
pub fn is_intrinsic(id: TypeId) -> bool {
	tsc_string_intrinsic(id)
		|| ezno_number_intrinsic(id)
		|| matches!(
			id,
			TypeId::LITERAL_RESTRICTION
				| TypeId::READONLY_RESTRICTION
				| TypeId::NO_INFER
				| TypeId::EXCLUSIVE_RESTRICTION
				| TypeId::NOT_RESTRICTION
				| TypeId::CASE_INSENSITIVE
		)
}

#[must_use]
pub fn ezno_number_intrinsic(id: TypeId) -> bool {
	matches!(id, TypeId::INCLUSIVE_RANGE | TypeId::EXCLUSIVE_RANGE | TypeId::MULTIPLE_OF)
}

#[must_use]
pub fn get_greater_than(on: TypeId, types: &TypeStore) -> Option<(bool, TypeId)> {
	let on = get_constraint(on, types).unwrap_or(on);
	let ty = types.get_type_by_id(on);
	if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
		on: on @ (TypeId::INCLUSIVE_RANGE | TypeId::EXCLUSIVE_RANGE),
		arguments,
	}) = ty
	{
		let inclusive = *on == TypeId::INCLUSIVE_RANGE;
		Some((
			inclusive,
			arguments.get_structure_restriction(TypeId::NUMBER_BOTTOM_GENERIC).unwrap(),
		))
	} else if let Type::And(lhs, rhs) = ty {
		get_greater_than(*lhs, types).or_else(|| get_greater_than(*rhs, types))
	} else if let Type::Constant(crate::Constant::Number(..)) = ty {
		Some((true, on))
	} else {
		None
	}
}

#[must_use]
pub fn get_less_than(on: TypeId, types: &TypeStore) -> Option<(bool, TypeId)> {
	let on = get_constraint(on, types).unwrap_or(on);
	let ty = types.get_type_by_id(on);
	if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
		on: on @ (TypeId::INCLUSIVE_RANGE | TypeId::EXCLUSIVE_RANGE),
		arguments,
	}) = ty
	{
		let inclusive = *on == TypeId::INCLUSIVE_RANGE;
		Some((inclusive, arguments.get_structure_restriction(TypeId::NUMBER_TOP_GENERIC).unwrap()))
	} else if let Type::And(lhs, rhs) = ty {
		get_less_than(*lhs, types).or_else(|| get_less_than(*rhs, types))
	} else if let Type::Constant(crate::Constant::Number(..)) = ty {
		Some((true, on))
	} else {
		None
	}
}

#[must_use]
pub fn get_range(on: TypeId, types: &TypeStore) -> Option<FloatRange> {
	let on = get_constraint(on, types).unwrap_or(on);
	let ty = types.get_type_by_id(on);
	if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
		on: on @ (TypeId::INCLUSIVE_RANGE | TypeId::EXCLUSIVE_RANGE),
		arguments,
	}) = ty
	{
		let inclusive = *on == TypeId::INCLUSIVE_RANGE;
		crate::utilities::notify!("{:?} {:?}", on, arguments);
		let floor = arguments.get_structure_restriction(TypeId::NUMBER_BOTTOM_GENERIC).unwrap();
		let ceiling = arguments.get_structure_restriction(TypeId::NUMBER_TOP_GENERIC).unwrap();
		if let (
			Type::Constant(crate::Constant::Number(floor)),
			Type::Constant(crate::Constant::Number(ceiling)),
		) = (types.get_type_by_id(floor), types.get_type_by_id(ceiling))
		{
			let (floor, ceiling) = (*floor, *ceiling);
			Some(if inclusive {
				FloatRange::Inclusive { floor, ceiling }
			} else {
				FloatRange::Exclusive { floor, ceiling }
			})
		} else {
			crate::utilities::notify!("Not bottom top number");
			None
		}
	} else if let Type::Constant(crate::Constant::Number(number)) = ty {
		Some(FloatRange::single(*number))
	} else {
		None
	}
}

#[must_use]
pub fn get_multiple(on: TypeId, types: &TypeStore) -> Option<TypeId> {
	let on = get_constraint(on, types).unwrap_or(on);
	let ty = types.get_type_by_id(on);
	if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
		on: TypeId::MULTIPLE_OF,
		arguments,
	}) = ty
	{
		arguments.get_structure_restriction(TypeId::NUMBER_BOTTOM_GENERIC)
	} else if let Type::And(lhs, rhs) = ty {
		get_multiple(*lhs, types).or_else(|| get_multiple(*rhs, types))
	} else if let Type::Constant(crate::Constant::Number(..)) = ty {
		Some(on)
	} else {
		None
	}
}

#[allow(clippy::match_like_matches_macro)]
#[must_use]
pub fn is_not_not_a_number(on: TypeId, types: &TypeStore) -> bool {
	if on == TypeId::NOT_NOT_A_NUMBER {
		true
	} else if on == TypeId::NUMBER_TYPE || on == TypeId::ANY_TYPE {
		false
	} else {
		let ty = types.get_type_by_id(on);
		if let Type::And(lhs, rhs) = ty {
			is_not_not_a_number(*lhs, types) || is_not_not_a_number(*rhs, types)
		} else if let Type::Or(lhs, rhs) = ty {
			is_not_not_a_number(*lhs, types) && is_not_not_a_number(*rhs, types)
		} else if let Type::PartiallyAppliedGenerics(PartiallyAppliedGenerics {
			on: TypeId::MULTIPLE_OF | TypeId::INCLUSIVE_RANGE | TypeId::EXCLUSIVE_RANGE,
			arguments: _,
		}) = ty
		{
			true
		} else if let Type::AliasTo { to, .. } = ty {
			is_not_not_a_number(*to, types)
		} else if let Some(constraint) = get_constraint(on, types) {
			is_not_not_a_number(constraint, types)
		} else {
			true
		}
	}
}
